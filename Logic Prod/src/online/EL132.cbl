00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL132 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 06/29/95 09:01:19.
00007 *                            VMOD=2.016
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
00026 *    THE CLAIM LOOK-UP.
00027
00028 *    TRANS-ID    - EX21
00029
00030 *    SCREENS     - EL132A - CLAIM LOOK-UP QUALIFICATION
00031
00032 *    ENTERED BY  - EL126 - MAINTENANCE MENU
00033 *                  EL130 - NEW CLAIM SET-UP
00034 *                  EL150 - STATUS DISPLAY (ENTERS EL1323 ONLY)
00035 *                  EL1275 - CERTIFICATE COVERAGES
00036
00037 *    EXIT TO     - CALLING PROGRAM
00038 *                  EL130 - NEW CLAIM SETUP
00039
00040 *    INPUT FILES - ELCERT - CERTIFICATE INFORCE FILE
00041 *                  ELACCT - ACCOUNT MASTER FILE
00042 *                  ELMSTR - CLAIMS MASTER
00043 *                  ELRETR - RETRIEVE MASTER
00044
00045 *    OUTPUT FILE - NONE
00046
00047 *    COMMAREA    - PASSED.  IF A CERTIFICATE IS SELECTED, THE
00048 *                  CONTROL OF THAT CERTIFICATE IS PLACED IN THE
00049 *                  APPROPRIATE FIELDS OF THE COMMAAREA FOR
00050 *                  REFERENCE BY SUCCESSIVE PROGRAMS.  THE PROGRAM
00051 *                  WORK AREA OF THE COMMAREA IS USED TO PASS THE
00052 *                  RECORD KEY INFORMATION NEEDED BY EL1322 TO
00053 *                  LOCATE THE CERTIFICATE.
00054
00055 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON
00056 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE
00057 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
00058 *                  ENTRIES (XCTL FROM CICS VIA EX21) THE SCREEN
00059 *                  WILL BE READ AND ACTION WILL BE BASED ON THE
00060 *                  MAINTENANCE TYPE INDICATED.
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
00061
00062      EJECT
00063  ENVIRONMENT DIVISION.
00064
00065  DATA DIVISION.
00066
00067  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00068
00069  77  FILLER  PIC X(32)  VALUE '********************************'.
00070  77  FILLER  PIC X(32)  VALUE '*   EL132  WORKING STORAGE     *'.
00071  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.016 **********'.
00072
00073 *                                COPY ELCSCTM.
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
00074
00075 *                                COPY ELCSCRTY.
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
00076
00077  01  WS-DATE-AREA.
00078      12  SAVE-DATE               PIC X(8)    VALUE SPACES.
00079      12  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.
00080
00081  01  FILLER                          COMP-3.
00082      12  WS-RECORD-COUNT         PIC S9(3)   VALUE ZERO.
00083      12  WS-READNEXT-SW          PIC S9      VALUE ZERO.
00084      12  WS-ERROR-NUMBER         PIC S9(3)   VALUE ZERO.
00085      12  WS-ERROR-COUNT          PIC S9(3)   VALUE ZERO.
00086      12  WS-LAST-ERROR-COUNT     PIC S9(3)   VALUE ZERO.
00087      12  WS-UPDATE-SW            PIC S9      VALUE ZERO.
00088      12  WS-COMPLETED-SUCCESSFUL PIC S9      VALUE ZERO.
00089          88  TRANSACTION-SUCCESSFUL          VALUE +1.
00090
00091      12  WS-CALL-SW              PIC S9      VALUE ZERO.
00092
00093      12  TIME-IN                 PIC S9(7)   VALUE ZERO.
00094      12  TIME-OUT                    REDEFINES
00095          TIME-IN                 PIC S9(3)V9(4).
00096
00097  01  FILLER                          COMP  SYNC.
00098      12  SC-ITEM                 PIC S9(4)   VALUE +0001.
00099      12  WS-INDEX                PIC S9(4)   VALUE ZERO.
00100
00101      12  WS-JOURNAL-FILE-ID      PIC S9(4)   VALUE +1.
00102      12  WS-JOURNAL-RECORD-LENGTH  PIC S9(4)   VALUE +527.
00103
00104      12  WS-KEY-LENGTH           PIC S9(4)   VALUE ZERO.
00105
00106  01  FILLER.
00107      12  QID.
00108          16  QID-TERM            PIC X(4).
00109          16  FILLER              PIC X(4)   VALUE '132A'.
00110      12  QID-PROC-AREA           PIC XXX.
00111      12  QID-LENGTH              PIC S9(4)  VALUE +3  COMP.
00112      12  QID-ITEM                PIC S9(4)  VALUE +1  COMP.
00113
00114      12  WS-ELCNTL-KEY.
00115          16  WS-ELCNTL-ID        PIC X(3).
00116          16  WS-ELCNTL-TYPE      PIC X.
00117          16  WS-ELCNTL-USER      PIC X(4)   VALUE SPACES.
00118          16  WS-ELCNTL-SEQ       PIC S9(4)  VALUE +0       COMP.
00119
00120      12  WS-MAPSET-NAME          PIC X(8)  VALUE 'EL132S'.
00121      12  WS-MAP-NAME             PIC X(8)  VALUE 'EL132A'.
00122
00123      12  FILLER                      REDEFINES
00124          WS-MAP-NAME.
00125          16  FILLER              PIC XX.
00126          16  WS-MAP-NUMBER       PIC X(4).
00127          16  FILLER              PIC XX.
00128
00129      12  THIS-PGM                PIC X(8)  VALUE 'EL132'.
00130
00131      12  ELMSTR-FILE-ID          PIC X(8) VALUE 'ELMSTR'.
00132      12  ELMSTR2-FILE-ID         PIC X(8) VALUE 'ELMSTR2'.
00133      12  ELMSTR3-FILE-ID         PIC X(8) VALUE 'ELMSTR3'.
00134      12  ELMSTR4-FILE-ID         PIC X(8) VALUE 'ELMSTR4'.
00135      12  ELMSTR5-FILE-ID         PIC X(8) VALUE 'ELMSTR5'.
00136      12  ELMSTR6-FILE-ID         PIC X(8) VALUE 'ELMSTR6'.
00137
00138      12  ELCNTL-FILE-ID          PIC X(8) VALUE 'ELCNTL'.
00139
00140      12  ELRETR-FILE-ID          PIC X(8) VALUE 'ELRETR'.
00141      12  ELRETR2-FILE-ID         PIC X(8) VALUE 'ELRETR2'.
00142      12  ELRETR3-FILE-ID         PIC X(8) VALUE 'ELRETR3'.
00143      12  ELRETR4-FILE-ID         PIC X(8) VALUE 'ELRETR4'.
00144      12  ELRETR5-FILE-ID         PIC X(8) VALUE 'ELRETR5'.
00145      12  ELRETR6-FILE-ID         PIC X(8) VALUE 'ELRETR6'.
00146
00147      12  WS-JOURNAL-TYPE-ID      PIC XX      VALUE 'EL'.
00148
00149      12  WS-LOW-VALUES           PIC X VALUE LOW-VALUES.
00150      12  WS-SPACES               PIC X       VALUE SPACES.
00151
00152      12  WS-TRANS-ID             PIC X(4)    VALUE 'EX21'.
00153
00154      12  WS-CNTL-REC-FOUND-SW    PIC X(01)   VALUE SPACES.
00155      12  WS-NEXT-COMPANY-ID      PIC X(03)   VALUE SPACES.
00156
00157      12  WS-INPUT-FIELD          PIC X(50)   VALUE SPACES.
00158
00159      12  WS-INPUT-CHAR    REDEFINES
00160          WS-INPUT-FIELD          PIC X
00161          OCCURS 50 TIMES             INDEXED BY INPUT-INDEX.
00162
00163  01  ERROR-MESSAGES.
00164      12  ER-0000                 PIC X(4)  VALUE '0000'.
00165      12  ER-0004                 PIC X(4)  VALUE '0004'.
00166      12  ER-0008                 PIC X(4)  VALUE '0008'.
00167      12  ER-0019                 PIC X(4)  VALUE '0019'.
00168      12  ER-0022                 PIC X(4)  VALUE '0022'.
00169      12  ER-0029                 PIC X(4)  VALUE '0029'.
00170      12  ER-0070                 PIC X(4)  VALUE '0070'.
00171      12  ER-0089                 PIC X(4)  VALUE '0089'.
00172      12  ER-0194                 PIC X(4)  VALUE '0194'.
00173      12  ER-0209                 PIC X(4)  VALUE '0209'.
00174      12  ER-0210                 PIC X(4)  VALUE '0210'.
00175      12  ER-0228                 PIC X(4)  VALUE '0228'.
00176      12  ER-0284                 PIC X(4)  VALUE '0284'.
00177      12  ER-0488                 PIC X(4)  VALUE '0488'.
00178      12  ER-2370                 PIC X(4)  VALUE '2370'.
00179      12  ER-2374                 PIC X(4)  VALUE '2374'.
00180
00181      EJECT
00182 *                                COPY ELCINTF.
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
00184 *                                COPY ELC132PI.
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
00185
00186      EJECT
00187 *                                COPY ELCEMIB.
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
00188
00189      EJECT
00190 *                                COPY ELCDATE.
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
00191
00192      EJECT
00193 *                                COPY EL132S.
       01  EL132BI.
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
           05  BHEAD1L PIC S9(0004) COMP.
           05  BHEAD1F PIC  X(0001).
           05  FILLER REDEFINES BHEAD1F.
               10  BHEAD1A PIC  X(0001).
           05  BHEAD1I PIC  X(0038).
      *    -------------------------------
           05  BCOMPL PIC S9(0004) COMP.
           05  BCOMPF PIC  X(0001).
           05  FILLER REDEFINES BCOMPF.
               10  BCOMPA PIC  X(0001).
           05  BCOMPI PIC  X(0003).
      *    -------------------------------
           05  BUSERIDL PIC S9(0004) COMP.
           05  BUSERIDF PIC  X(0001).
           05  FILLER REDEFINES BUSERIDF.
               10  BUSERIDA PIC  X(0001).
           05  BUSERIDI PIC  X(0004).
      *    -------------------------------
           05  BCERTL PIC S9(0004) COMP.
           05  BCERTF PIC  X(0001).
           05  FILLER REDEFINES BCERTF.
               10  BCERTA PIC  X(0001).
           05  BCERTI PIC  X(0015).
      *    -------------------------------
           05  BNUM01L PIC S9(0004) COMP.
           05  BNUM01F PIC  X(0001).
           05  FILLER REDEFINES BNUM01F.
               10  BNUM01A PIC  X(0001).
           05  BNUM01I PIC  X(0002).
      *    -------------------------------
           05  BNAME01L PIC S9(0004) COMP.
           05  BNAME01F PIC  X(0001).
           05  FILLER REDEFINES BNAME01F.
               10  BNAME01A PIC  X(0001).
           05  BNAME01I PIC  X(0022).
      *    -------------------------------
           05  BAGE01L PIC S9(0004) COMP.
           05  BAGE01F PIC  X(0001).
           05  FILLER REDEFINES BAGE01F.
               10  BAGE01A PIC  X(0001).
           05  BAGE01I PIC  X(0002).
      *    -------------------------------
           05  BSTA01L PIC S9(0004) COMP.
           05  BSTA01F PIC  X(0001).
           05  FILLER REDEFINES BSTA01F.
               10  BSTA01A PIC  X(0001).
           05  BSTA01I PIC  X(0001).
      *    -------------------------------
           05  BIDT01L PIC S9(0004) COMP.
           05  BIDT01F PIC  X(0001).
           05  FILLER REDEFINES BIDT01F.
               10  BIDT01A PIC  X(0001).
           05  BIDT01I PIC  X(0008).
      *    -------------------------------
           05  BTYPE01L PIC S9(0004) COMP.
           05  BTYPE01F PIC  X(0001).
           05  FILLER REDEFINES BTYPE01F.
               10  BTYPE01A PIC  X(0001).
           05  BTYPE01I PIC  X(0001).
      *    -------------------------------
           05  BCARR01L PIC S9(0004) COMP.
           05  BCARR01F PIC  X(0001).
           05  FILLER REDEFINES BCARR01F.
               10  BCARR01A PIC  X(0001).
           05  BCARR01I PIC  X(0001).
      *    -------------------------------
           05  BCLAM01L PIC S9(0004) COMP.
           05  BCLAM01F PIC  X(0001).
           05  FILLER REDEFINES BCLAM01F.
               10  BCLAM01A PIC  X(0001).
           05  BCLAM01I PIC  X(0007).
      *    -------------------------------
           05  BCERT01L PIC S9(0004) COMP.
           05  BCERT01F PIC  X(0001).
           05  FILLER REDEFINES BCERT01F.
               10  BCERT01A PIC  X(0001).
           05  BCERT01I PIC  X(0016).
      *    -------------------------------
           05  BACCT01L PIC S9(0004) COMP.
           05  BACCT01F PIC  X(0001).
           05  FILLER REDEFINES BACCT01F.
               10  BACCT01A PIC  X(0001).
           05  BACCT01I PIC  X(0010).
      *    -------------------------------
           05  BNUM02L PIC S9(0004) COMP.
           05  BNUM02F PIC  X(0001).
           05  FILLER REDEFINES BNUM02F.
               10  BNUM02A PIC  X(0001).
           05  BNUM02I PIC  X(0002).
      *    -------------------------------
           05  BNAME02L PIC S9(0004) COMP.
           05  BNAME02F PIC  X(0001).
           05  FILLER REDEFINES BNAME02F.
               10  BNAME02A PIC  X(0001).
           05  BNAME02I PIC  X(0022).
      *    -------------------------------
           05  BAGE02L PIC S9(0004) COMP.
           05  BAGE02F PIC  X(0001).
           05  FILLER REDEFINES BAGE02F.
               10  BAGE02A PIC  X(0001).
           05  BAGE02I PIC  X(0002).
      *    -------------------------------
           05  BSTA02L PIC S9(0004) COMP.
           05  BSTA02F PIC  X(0001).
           05  FILLER REDEFINES BSTA02F.
               10  BSTA02A PIC  X(0001).
           05  BSTA02I PIC  X(0001).
      *    -------------------------------
           05  BIDT02L PIC S9(0004) COMP.
           05  BIDT02F PIC  X(0001).
           05  FILLER REDEFINES BIDT02F.
               10  BIDT02A PIC  X(0001).
           05  BIDT02I PIC  X(0008).
      *    -------------------------------
           05  BTYPE02L PIC S9(0004) COMP.
           05  BTYPE02F PIC  X(0001).
           05  FILLER REDEFINES BTYPE02F.
               10  BTYPE02A PIC  X(0001).
           05  BTYPE02I PIC  X(0001).
      *    -------------------------------
           05  BCARR02L PIC S9(0004) COMP.
           05  BCARR02F PIC  X(0001).
           05  FILLER REDEFINES BCARR02F.
               10  BCARR02A PIC  X(0001).
           05  BCARR02I PIC  X(0001).
      *    -------------------------------
           05  BCLAM02L PIC S9(0004) COMP.
           05  BCLAM02F PIC  X(0001).
           05  FILLER REDEFINES BCLAM02F.
               10  BCLAM02A PIC  X(0001).
           05  BCLAM02I PIC  X(0007).
      *    -------------------------------
           05  BCERT02L PIC S9(0004) COMP.
           05  BCERT02F PIC  X(0001).
           05  FILLER REDEFINES BCERT02F.
               10  BCERT02A PIC  X(0001).
           05  BCERT02I PIC  X(0016).
      *    -------------------------------
           05  BACCT02L PIC S9(0004) COMP.
           05  BACCT02F PIC  X(0001).
           05  FILLER REDEFINES BACCT02F.
               10  BACCT02A PIC  X(0001).
           05  BACCT02I PIC  X(0010).
      *    -------------------------------
           05  BNUM03L PIC S9(0004) COMP.
           05  BNUM03F PIC  X(0001).
           05  FILLER REDEFINES BNUM03F.
               10  BNUM03A PIC  X(0001).
           05  BNUM03I PIC  X(0002).
      *    -------------------------------
           05  BNAME03L PIC S9(0004) COMP.
           05  BNAME03F PIC  X(0001).
           05  FILLER REDEFINES BNAME03F.
               10  BNAME03A PIC  X(0001).
           05  BNAME03I PIC  X(0022).
      *    -------------------------------
           05  BAGE03L PIC S9(0004) COMP.
           05  BAGE03F PIC  X(0001).
           05  FILLER REDEFINES BAGE03F.
               10  BAGE03A PIC  X(0001).
           05  BAGE03I PIC  X(0002).
      *    -------------------------------
           05  BSTA03L PIC S9(0004) COMP.
           05  BSTA03F PIC  X(0001).
           05  FILLER REDEFINES BSTA03F.
               10  BSTA03A PIC  X(0001).
           05  BSTA03I PIC  X(0001).
      *    -------------------------------
           05  BIDT03L PIC S9(0004) COMP.
           05  BIDT03F PIC  X(0001).
           05  FILLER REDEFINES BIDT03F.
               10  BIDT03A PIC  X(0001).
           05  BIDT03I PIC  X(0008).
      *    -------------------------------
           05  BTYPE03L PIC S9(0004) COMP.
           05  BTYPE03F PIC  X(0001).
           05  FILLER REDEFINES BTYPE03F.
               10  BTYPE03A PIC  X(0001).
           05  BTYPE03I PIC  X(0001).
      *    -------------------------------
           05  BCARR03L PIC S9(0004) COMP.
           05  BCARR03F PIC  X(0001).
           05  FILLER REDEFINES BCARR03F.
               10  BCARR03A PIC  X(0001).
           05  BCARR03I PIC  X(0001).
      *    -------------------------------
           05  BCLAM03L PIC S9(0004) COMP.
           05  BCLAM03F PIC  X(0001).
           05  FILLER REDEFINES BCLAM03F.
               10  BCLAM03A PIC  X(0001).
           05  BCLAM03I PIC  X(0007).
      *    -------------------------------
           05  BCERT03L PIC S9(0004) COMP.
           05  BCERT03F PIC  X(0001).
           05  FILLER REDEFINES BCERT03F.
               10  BCERT03A PIC  X(0001).
           05  BCERT03I PIC  X(0016).
      *    -------------------------------
           05  BACCT03L PIC S9(0004) COMP.
           05  BACCT03F PIC  X(0001).
           05  FILLER REDEFINES BACCT03F.
               10  BACCT03A PIC  X(0001).
           05  BACCT03I PIC  X(0010).
      *    -------------------------------
           05  BNUM04L PIC S9(0004) COMP.
           05  BNUM04F PIC  X(0001).
           05  FILLER REDEFINES BNUM04F.
               10  BNUM04A PIC  X(0001).
           05  BNUM04I PIC  X(0002).
      *    -------------------------------
           05  BNAME04L PIC S9(0004) COMP.
           05  BNAME04F PIC  X(0001).
           05  FILLER REDEFINES BNAME04F.
               10  BNAME04A PIC  X(0001).
           05  BNAME04I PIC  X(0022).
      *    -------------------------------
           05  BAGE04L PIC S9(0004) COMP.
           05  BAGE04F PIC  X(0001).
           05  FILLER REDEFINES BAGE04F.
               10  BAGE04A PIC  X(0001).
           05  BAGE04I PIC  X(0002).
      *    -------------------------------
           05  BSTA04L PIC S9(0004) COMP.
           05  BSTA04F PIC  X(0001).
           05  FILLER REDEFINES BSTA04F.
               10  BSTA04A PIC  X(0001).
           05  BSTA04I PIC  X(0001).
      *    -------------------------------
           05  BIDT04L PIC S9(0004) COMP.
           05  BIDT04F PIC  X(0001).
           05  FILLER REDEFINES BIDT04F.
               10  BIDT04A PIC  X(0001).
           05  BIDT04I PIC  X(0008).
      *    -------------------------------
           05  BTYPE04L PIC S9(0004) COMP.
           05  BTYPE04F PIC  X(0001).
           05  FILLER REDEFINES BTYPE04F.
               10  BTYPE04A PIC  X(0001).
           05  BTYPE04I PIC  X(0001).
      *    -------------------------------
           05  BCARR04L PIC S9(0004) COMP.
           05  BCARR04F PIC  X(0001).
           05  FILLER REDEFINES BCARR04F.
               10  BCARR04A PIC  X(0001).
           05  BCARR04I PIC  X(0001).
      *    -------------------------------
           05  BCLAM04L PIC S9(0004) COMP.
           05  BCLAM04F PIC  X(0001).
           05  FILLER REDEFINES BCLAM04F.
               10  BCLAM04A PIC  X(0001).
           05  BCLAM04I PIC  X(0007).
      *    -------------------------------
           05  BCERT04L PIC S9(0004) COMP.
           05  BCERT04F PIC  X(0001).
           05  FILLER REDEFINES BCERT04F.
               10  BCERT04A PIC  X(0001).
           05  BCERT04I PIC  X(0016).
      *    -------------------------------
           05  BACCT04L PIC S9(0004) COMP.
           05  BACCT04F PIC  X(0001).
           05  FILLER REDEFINES BACCT04F.
               10  BACCT04A PIC  X(0001).
           05  BACCT04I PIC  X(0010).
      *    -------------------------------
           05  BNUM05L PIC S9(0004) COMP.
           05  BNUM05F PIC  X(0001).
           05  FILLER REDEFINES BNUM05F.
               10  BNUM05A PIC  X(0001).
           05  BNUM05I PIC  X(0002).
      *    -------------------------------
           05  BNAME05L PIC S9(0004) COMP.
           05  BNAME05F PIC  X(0001).
           05  FILLER REDEFINES BNAME05F.
               10  BNAME05A PIC  X(0001).
           05  BNAME05I PIC  X(0022).
      *    -------------------------------
           05  BAGE05L PIC S9(0004) COMP.
           05  BAGE05F PIC  X(0001).
           05  FILLER REDEFINES BAGE05F.
               10  BAGE05A PIC  X(0001).
           05  BAGE05I PIC  X(0002).
      *    -------------------------------
           05  BSTA05L PIC S9(0004) COMP.
           05  BSTA05F PIC  X(0001).
           05  FILLER REDEFINES BSTA05F.
               10  BSTA05A PIC  X(0001).
           05  BSTA05I PIC  X(0001).
      *    -------------------------------
           05  BIDT05L PIC S9(0004) COMP.
           05  BIDT05F PIC  X(0001).
           05  FILLER REDEFINES BIDT05F.
               10  BIDT05A PIC  X(0001).
           05  BIDT05I PIC  X(0008).
      *    -------------------------------
           05  BTYPE05L PIC S9(0004) COMP.
           05  BTYPE05F PIC  X(0001).
           05  FILLER REDEFINES BTYPE05F.
               10  BTYPE05A PIC  X(0001).
           05  BTYPE05I PIC  X(0001).
      *    -------------------------------
           05  BCARR05L PIC S9(0004) COMP.
           05  BCARR05F PIC  X(0001).
           05  FILLER REDEFINES BCARR05F.
               10  BCARR05A PIC  X(0001).
           05  BCARR05I PIC  X(0001).
      *    -------------------------------
           05  BCLAM05L PIC S9(0004) COMP.
           05  BCLAM05F PIC  X(0001).
           05  FILLER REDEFINES BCLAM05F.
               10  BCLAM05A PIC  X(0001).
           05  BCLAM05I PIC  X(0007).
      *    -------------------------------
           05  BCERT05L PIC S9(0004) COMP.
           05  BCERT05F PIC  X(0001).
           05  FILLER REDEFINES BCERT05F.
               10  BCERT05A PIC  X(0001).
           05  BCERT05I PIC  X(0016).
      *    -------------------------------
           05  BACCT05L PIC S9(0004) COMP.
           05  BACCT05F PIC  X(0001).
           05  FILLER REDEFINES BACCT05F.
               10  BACCT05A PIC  X(0001).
           05  BACCT05I PIC  X(0010).
      *    -------------------------------
           05  BNUM06L PIC S9(0004) COMP.
           05  BNUM06F PIC  X(0001).
           05  FILLER REDEFINES BNUM06F.
               10  BNUM06A PIC  X(0001).
           05  BNUM06I PIC  X(0002).
      *    -------------------------------
           05  BNAME06L PIC S9(0004) COMP.
           05  BNAME06F PIC  X(0001).
           05  FILLER REDEFINES BNAME06F.
               10  BNAME06A PIC  X(0001).
           05  BNAME06I PIC  X(0022).
      *    -------------------------------
           05  BAGE06L PIC S9(0004) COMP.
           05  BAGE06F PIC  X(0001).
           05  FILLER REDEFINES BAGE06F.
               10  BAGE06A PIC  X(0001).
           05  BAGE06I PIC  X(0002).
      *    -------------------------------
           05  BSTA06L PIC S9(0004) COMP.
           05  BSTA06F PIC  X(0001).
           05  FILLER REDEFINES BSTA06F.
               10  BSTA06A PIC  X(0001).
           05  BSTA06I PIC  X(0001).
      *    -------------------------------
           05  BIDT06L PIC S9(0004) COMP.
           05  BIDT06F PIC  X(0001).
           05  FILLER REDEFINES BIDT06F.
               10  BIDT06A PIC  X(0001).
           05  BIDT06I PIC  X(0008).
      *    -------------------------------
           05  BTYPE06L PIC S9(0004) COMP.
           05  BTYPE06F PIC  X(0001).
           05  FILLER REDEFINES BTYPE06F.
               10  BTYPE06A PIC  X(0001).
           05  BTYPE06I PIC  X(0001).
      *    -------------------------------
           05  BCARR06L PIC S9(0004) COMP.
           05  BCARR06F PIC  X(0001).
           05  FILLER REDEFINES BCARR06F.
               10  BCARR06A PIC  X(0001).
           05  BCARR06I PIC  X(0001).
      *    -------------------------------
           05  BCLAM06L PIC S9(0004) COMP.
           05  BCLAM06F PIC  X(0001).
           05  FILLER REDEFINES BCLAM06F.
               10  BCLAM06A PIC  X(0001).
           05  BCLAM06I PIC  X(0007).
      *    -------------------------------
           05  BCERT06L PIC S9(0004) COMP.
           05  BCERT06F PIC  X(0001).
           05  FILLER REDEFINES BCERT06F.
               10  BCERT06A PIC  X(0001).
           05  BCERT06I PIC  X(0016).
      *    -------------------------------
           05  BACCT06L PIC S9(0004) COMP.
           05  BACCT06F PIC  X(0001).
           05  FILLER REDEFINES BACCT06F.
               10  BACCT06A PIC  X(0001).
           05  BACCT06I PIC  X(0010).
      *    -------------------------------
           05  BNUM07L PIC S9(0004) COMP.
           05  BNUM07F PIC  X(0001).
           05  FILLER REDEFINES BNUM07F.
               10  BNUM07A PIC  X(0001).
           05  BNUM07I PIC  X(0002).
      *    -------------------------------
           05  BNAME07L PIC S9(0004) COMP.
           05  BNAME07F PIC  X(0001).
           05  FILLER REDEFINES BNAME07F.
               10  BNAME07A PIC  X(0001).
           05  BNAME07I PIC  X(0022).
      *    -------------------------------
           05  BAGE07L PIC S9(0004) COMP.
           05  BAGE07F PIC  X(0001).
           05  FILLER REDEFINES BAGE07F.
               10  BAGE07A PIC  X(0001).
           05  BAGE07I PIC  X(0002).
      *    -------------------------------
           05  BSTA07L PIC S9(0004) COMP.
           05  BSTA07F PIC  X(0001).
           05  FILLER REDEFINES BSTA07F.
               10  BSTA07A PIC  X(0001).
           05  BSTA07I PIC  X(0001).
      *    -------------------------------
           05  BIDT07L PIC S9(0004) COMP.
           05  BIDT07F PIC  X(0001).
           05  FILLER REDEFINES BIDT07F.
               10  BIDT07A PIC  X(0001).
           05  BIDT07I PIC  X(0008).
      *    -------------------------------
           05  BTYPE07L PIC S9(0004) COMP.
           05  BTYPE07F PIC  X(0001).
           05  FILLER REDEFINES BTYPE07F.
               10  BTYPE07A PIC  X(0001).
           05  BTYPE07I PIC  X(0001).
      *    -------------------------------
           05  BCARR07L PIC S9(0004) COMP.
           05  BCARR07F PIC  X(0001).
           05  FILLER REDEFINES BCARR07F.
               10  BCARR07A PIC  X(0001).
           05  BCARR07I PIC  X(0001).
      *    -------------------------------
           05  BCLAM07L PIC S9(0004) COMP.
           05  BCLAM07F PIC  X(0001).
           05  FILLER REDEFINES BCLAM07F.
               10  BCLAM07A PIC  X(0001).
           05  BCLAM07I PIC  X(0007).
      *    -------------------------------
           05  BCERT07L PIC S9(0004) COMP.
           05  BCERT07F PIC  X(0001).
           05  FILLER REDEFINES BCERT07F.
               10  BCERT07A PIC  X(0001).
           05  BCERT07I PIC  X(0016).
      *    -------------------------------
           05  BACCT07L PIC S9(0004) COMP.
           05  BACCT07F PIC  X(0001).
           05  FILLER REDEFINES BACCT07F.
               10  BACCT07A PIC  X(0001).
           05  BACCT07I PIC  X(0010).
      *    -------------------------------
           05  BNUM08L PIC S9(0004) COMP.
           05  BNUM08F PIC  X(0001).
           05  FILLER REDEFINES BNUM08F.
               10  BNUM08A PIC  X(0001).
           05  BNUM08I PIC  X(0002).
      *    -------------------------------
           05  BNAME08L PIC S9(0004) COMP.
           05  BNAME08F PIC  X(0001).
           05  FILLER REDEFINES BNAME08F.
               10  BNAME08A PIC  X(0001).
           05  BNAME08I PIC  X(0022).
      *    -------------------------------
           05  BAGE08L PIC S9(0004) COMP.
           05  BAGE08F PIC  X(0001).
           05  FILLER REDEFINES BAGE08F.
               10  BAGE08A PIC  X(0001).
           05  BAGE08I PIC  X(0002).
      *    -------------------------------
           05  BSTA08L PIC S9(0004) COMP.
           05  BSTA08F PIC  X(0001).
           05  FILLER REDEFINES BSTA08F.
               10  BSTA08A PIC  X(0001).
           05  BSTA08I PIC  X(0001).
      *    -------------------------------
           05  BIDT08L PIC S9(0004) COMP.
           05  BIDT08F PIC  X(0001).
           05  FILLER REDEFINES BIDT08F.
               10  BIDT08A PIC  X(0001).
           05  BIDT08I PIC  X(0008).
      *    -------------------------------
           05  BTYPE08L PIC S9(0004) COMP.
           05  BTYPE08F PIC  X(0001).
           05  FILLER REDEFINES BTYPE08F.
               10  BTYPE08A PIC  X(0001).
           05  BTYPE08I PIC  X(0001).
      *    -------------------------------
           05  BCARR08L PIC S9(0004) COMP.
           05  BCARR08F PIC  X(0001).
           05  FILLER REDEFINES BCARR08F.
               10  BCARR08A PIC  X(0001).
           05  BCARR08I PIC  X(0001).
      *    -------------------------------
           05  BCLAM08L PIC S9(0004) COMP.
           05  BCLAM08F PIC  X(0001).
           05  FILLER REDEFINES BCLAM08F.
               10  BCLAM08A PIC  X(0001).
           05  BCLAM08I PIC  X(0007).
      *    -------------------------------
           05  BCERT08L PIC S9(0004) COMP.
           05  BCERT08F PIC  X(0001).
           05  FILLER REDEFINES BCERT08F.
               10  BCERT08A PIC  X(0001).
           05  BCERT08I PIC  X(0016).
      *    -------------------------------
           05  BACCT08L PIC S9(0004) COMP.
           05  BACCT08F PIC  X(0001).
           05  FILLER REDEFINES BACCT08F.
               10  BACCT08A PIC  X(0001).
           05  BACCT08I PIC  X(0010).
      *    -------------------------------
           05  BNUM09L PIC S9(0004) COMP.
           05  BNUM09F PIC  X(0001).
           05  FILLER REDEFINES BNUM09F.
               10  BNUM09A PIC  X(0001).
           05  BNUM09I PIC  X(0002).
      *    -------------------------------
           05  BNAME09L PIC S9(0004) COMP.
           05  BNAME09F PIC  X(0001).
           05  FILLER REDEFINES BNAME09F.
               10  BNAME09A PIC  X(0001).
           05  BNAME09I PIC  X(0022).
      *    -------------------------------
           05  BAGE09L PIC S9(0004) COMP.
           05  BAGE09F PIC  X(0001).
           05  FILLER REDEFINES BAGE09F.
               10  BAGE09A PIC  X(0001).
           05  BAGE09I PIC  X(0002).
      *    -------------------------------
           05  BSTA09L PIC S9(0004) COMP.
           05  BSTA09F PIC  X(0001).
           05  FILLER REDEFINES BSTA09F.
               10  BSTA09A PIC  X(0001).
           05  BSTA09I PIC  X(0001).
      *    -------------------------------
           05  BIDT09L PIC S9(0004) COMP.
           05  BIDT09F PIC  X(0001).
           05  FILLER REDEFINES BIDT09F.
               10  BIDT09A PIC  X(0001).
           05  BIDT09I PIC  X(0008).
      *    -------------------------------
           05  BTYPE09L PIC S9(0004) COMP.
           05  BTYPE09F PIC  X(0001).
           05  FILLER REDEFINES BTYPE09F.
               10  BTYPE09A PIC  X(0001).
           05  BTYPE09I PIC  X(0001).
      *    -------------------------------
           05  BCARR09L PIC S9(0004) COMP.
           05  BCARR09F PIC  X(0001).
           05  FILLER REDEFINES BCARR09F.
               10  BCARR09A PIC  X(0001).
           05  BCARR09I PIC  X(0001).
      *    -------------------------------
           05  BCLAM09L PIC S9(0004) COMP.
           05  BCLAM09F PIC  X(0001).
           05  FILLER REDEFINES BCLAM09F.
               10  BCLAM09A PIC  X(0001).
           05  BCLAM09I PIC  X(0007).
      *    -------------------------------
           05  BCERT09L PIC S9(0004) COMP.
           05  BCERT09F PIC  X(0001).
           05  FILLER REDEFINES BCERT09F.
               10  BCERT09A PIC  X(0001).
           05  BCERT09I PIC  X(0016).
      *    -------------------------------
           05  BACCT09L PIC S9(0004) COMP.
           05  BACCT09F PIC  X(0001).
           05  FILLER REDEFINES BACCT09F.
               10  BACCT09A PIC  X(0001).
           05  BACCT09I PIC  X(0010).
      *    -------------------------------
           05  BNUM10L PIC S9(0004) COMP.
           05  BNUM10F PIC  X(0001).
           05  FILLER REDEFINES BNUM10F.
               10  BNUM10A PIC  X(0001).
           05  BNUM10I PIC  X(0002).
      *    -------------------------------
           05  BNAME10L PIC S9(0004) COMP.
           05  BNAME10F PIC  X(0001).
           05  FILLER REDEFINES BNAME10F.
               10  BNAME10A PIC  X(0001).
           05  BNAME10I PIC  X(0022).
      *    -------------------------------
           05  BAGE10L PIC S9(0004) COMP.
           05  BAGE10F PIC  X(0001).
           05  FILLER REDEFINES BAGE10F.
               10  BAGE10A PIC  X(0001).
           05  BAGE10I PIC  X(0002).
      *    -------------------------------
           05  BSTA10L PIC S9(0004) COMP.
           05  BSTA10F PIC  X(0001).
           05  FILLER REDEFINES BSTA10F.
               10  BSTA10A PIC  X(0001).
           05  BSTA10I PIC  X(0001).
      *    -------------------------------
           05  BIDT10L PIC S9(0004) COMP.
           05  BIDT10F PIC  X(0001).
           05  FILLER REDEFINES BIDT10F.
               10  BIDT10A PIC  X(0001).
           05  BIDT10I PIC  X(0008).
      *    -------------------------------
           05  BTYPE10L PIC S9(0004) COMP.
           05  BTYPE10F PIC  X(0001).
           05  FILLER REDEFINES BTYPE10F.
               10  BTYPE10A PIC  X(0001).
           05  BTYPE10I PIC  X(0001).
      *    -------------------------------
           05  BCARR10L PIC S9(0004) COMP.
           05  BCARR10F PIC  X(0001).
           05  FILLER REDEFINES BCARR10F.
               10  BCARR10A PIC  X(0001).
           05  BCARR10I PIC  X(0001).
      *    -------------------------------
           05  BCLAM10L PIC S9(0004) COMP.
           05  BCLAM10F PIC  X(0001).
           05  FILLER REDEFINES BCLAM10F.
               10  BCLAM10A PIC  X(0001).
           05  BCLAM10I PIC  X(0007).
      *    -------------------------------
           05  BCERT10L PIC S9(0004) COMP.
           05  BCERT10F PIC  X(0001).
           05  FILLER REDEFINES BCERT10F.
               10  BCERT10A PIC  X(0001).
           05  BCERT10I PIC  X(0016).
      *    -------------------------------
           05  BACCT10L PIC S9(0004) COMP.
           05  BACCT10F PIC  X(0001).
           05  FILLER REDEFINES BACCT10F.
               10  BACCT10A PIC  X(0001).
           05  BACCT10I PIC  X(0010).
      *    -------------------------------
           05  BNUM11L PIC S9(0004) COMP.
           05  BNUM11F PIC  X(0001).
           05  FILLER REDEFINES BNUM11F.
               10  BNUM11A PIC  X(0001).
           05  BNUM11I PIC  X(0002).
      *    -------------------------------
           05  BNAME11L PIC S9(0004) COMP.
           05  BNAME11F PIC  X(0001).
           05  FILLER REDEFINES BNAME11F.
               10  BNAME11A PIC  X(0001).
           05  BNAME11I PIC  X(0022).
      *    -------------------------------
           05  BAGE11L PIC S9(0004) COMP.
           05  BAGE11F PIC  X(0001).
           05  FILLER REDEFINES BAGE11F.
               10  BAGE11A PIC  X(0001).
           05  BAGE11I PIC  X(0002).
      *    -------------------------------
           05  BSTA11L PIC S9(0004) COMP.
           05  BSTA11F PIC  X(0001).
           05  FILLER REDEFINES BSTA11F.
               10  BSTA11A PIC  X(0001).
           05  BSTA11I PIC  X(0001).
      *    -------------------------------
           05  BIDT11L PIC S9(0004) COMP.
           05  BIDT11F PIC  X(0001).
           05  FILLER REDEFINES BIDT11F.
               10  BIDT11A PIC  X(0001).
           05  BIDT11I PIC  X(0008).
      *    -------------------------------
           05  BTYPE11L PIC S9(0004) COMP.
           05  BTYPE11F PIC  X(0001).
           05  FILLER REDEFINES BTYPE11F.
               10  BTYPE11A PIC  X(0001).
           05  BTYPE11I PIC  X(0001).
      *    -------------------------------
           05  BCARR11L PIC S9(0004) COMP.
           05  BCARR11F PIC  X(0001).
           05  FILLER REDEFINES BCARR11F.
               10  BCARR11A PIC  X(0001).
           05  BCARR11I PIC  X(0001).
      *    -------------------------------
           05  BCLAM11L PIC S9(0004) COMP.
           05  BCLAM11F PIC  X(0001).
           05  FILLER REDEFINES BCLAM11F.
               10  BCLAM11A PIC  X(0001).
           05  BCLAM11I PIC  X(0007).
      *    -------------------------------
           05  BCERT11L PIC S9(0004) COMP.
           05  BCERT11F PIC  X(0001).
           05  FILLER REDEFINES BCERT11F.
               10  BCERT11A PIC  X(0001).
           05  BCERT11I PIC  X(0016).
      *    -------------------------------
           05  BACCT11L PIC S9(0004) COMP.
           05  BACCT11F PIC  X(0001).
           05  FILLER REDEFINES BACCT11F.
               10  BACCT11A PIC  X(0001).
           05  BACCT11I PIC  X(0010).
      *    -------------------------------
           05  BNUM12L PIC S9(0004) COMP.
           05  BNUM12F PIC  X(0001).
           05  FILLER REDEFINES BNUM12F.
               10  BNUM12A PIC  X(0001).
           05  BNUM12I PIC  X(0002).
      *    -------------------------------
           05  BNAME12L PIC S9(0004) COMP.
           05  BNAME12F PIC  X(0001).
           05  FILLER REDEFINES BNAME12F.
               10  BNAME12A PIC  X(0001).
           05  BNAME12I PIC  X(0022).
      *    -------------------------------
           05  BAGE12L PIC S9(0004) COMP.
           05  BAGE12F PIC  X(0001).
           05  FILLER REDEFINES BAGE12F.
               10  BAGE12A PIC  X(0001).
           05  BAGE12I PIC  X(0002).
      *    -------------------------------
           05  BSTA12L PIC S9(0004) COMP.
           05  BSTA12F PIC  X(0001).
           05  FILLER REDEFINES BSTA12F.
               10  BSTA12A PIC  X(0001).
           05  BSTA12I PIC  X(0001).
      *    -------------------------------
           05  BIDT12L PIC S9(0004) COMP.
           05  BIDT12F PIC  X(0001).
           05  FILLER REDEFINES BIDT12F.
               10  BIDT12A PIC  X(0001).
           05  BIDT12I PIC  X(0008).
      *    -------------------------------
           05  BTYPE12L PIC S9(0004) COMP.
           05  BTYPE12F PIC  X(0001).
           05  FILLER REDEFINES BTYPE12F.
               10  BTYPE12A PIC  X(0001).
           05  BTYPE12I PIC  X(0001).
      *    -------------------------------
           05  BCARR12L PIC S9(0004) COMP.
           05  BCARR12F PIC  X(0001).
           05  FILLER REDEFINES BCARR12F.
               10  BCARR12A PIC  X(0001).
           05  BCARR12I PIC  X(0001).
      *    -------------------------------
           05  BCLAM12L PIC S9(0004) COMP.
           05  BCLAM12F PIC  X(0001).
           05  FILLER REDEFINES BCLAM12F.
               10  BCLAM12A PIC  X(0001).
           05  BCLAM12I PIC  X(0007).
      *    -------------------------------
           05  BCERT12L PIC S9(0004) COMP.
           05  BCERT12F PIC  X(0001).
           05  FILLER REDEFINES BCERT12F.
               10  BCERT12A PIC  X(0001).
           05  BCERT12I PIC  X(0016).
      *    -------------------------------
           05  BACCT12L PIC S9(0004) COMP.
           05  BACCT12F PIC  X(0001).
           05  FILLER REDEFINES BACCT12F.
               10  BACCT12A PIC  X(0001).
           05  BACCT12I PIC  X(0010).
      *    -------------------------------
           05  BNUM13L PIC S9(0004) COMP.
           05  BNUM13F PIC  X(0001).
           05  FILLER REDEFINES BNUM13F.
               10  BNUM13A PIC  X(0001).
           05  BNUM13I PIC  X(0002).
      *    -------------------------------
           05  BNAME13L PIC S9(0004) COMP.
           05  BNAME13F PIC  X(0001).
           05  FILLER REDEFINES BNAME13F.
               10  BNAME13A PIC  X(0001).
           05  BNAME13I PIC  X(0022).
      *    -------------------------------
           05  BAGE13L PIC S9(0004) COMP.
           05  BAGE13F PIC  X(0001).
           05  FILLER REDEFINES BAGE13F.
               10  BAGE13A PIC  X(0001).
           05  BAGE13I PIC  X(0002).
      *    -------------------------------
           05  BSTA13L PIC S9(0004) COMP.
           05  BSTA13F PIC  X(0001).
           05  FILLER REDEFINES BSTA13F.
               10  BSTA13A PIC  X(0001).
           05  BSTA13I PIC  X(0001).
      *    -------------------------------
           05  BIDT13L PIC S9(0004) COMP.
           05  BIDT13F PIC  X(0001).
           05  FILLER REDEFINES BIDT13F.
               10  BIDT13A PIC  X(0001).
           05  BIDT13I PIC  X(0008).
      *    -------------------------------
           05  BTYPE13L PIC S9(0004) COMP.
           05  BTYPE13F PIC  X(0001).
           05  FILLER REDEFINES BTYPE13F.
               10  BTYPE13A PIC  X(0001).
           05  BTYPE13I PIC  X(0001).
      *    -------------------------------
           05  BCARR13L PIC S9(0004) COMP.
           05  BCARR13F PIC  X(0001).
           05  FILLER REDEFINES BCARR13F.
               10  BCARR13A PIC  X(0001).
           05  BCARR13I PIC  X(0001).
      *    -------------------------------
           05  BCLAM13L PIC S9(0004) COMP.
           05  BCLAM13F PIC  X(0001).
           05  FILLER REDEFINES BCLAM13F.
               10  BCLAM13A PIC  X(0001).
           05  BCLAM13I PIC  X(0007).
      *    -------------------------------
           05  BCERT13L PIC S9(0004) COMP.
           05  BCERT13F PIC  X(0001).
           05  FILLER REDEFINES BCERT13F.
               10  BCERT13A PIC  X(0001).
           05  BCERT13I PIC  X(0016).
      *    -------------------------------
           05  BACCT13L PIC S9(0004) COMP.
           05  BACCT13F PIC  X(0001).
           05  FILLER REDEFINES BACCT13F.
               10  BACCT13A PIC  X(0001).
           05  BACCT13I PIC  X(0010).
      *    -------------------------------
           05  BNUM14L PIC S9(0004) COMP.
           05  BNUM14F PIC  X(0001).
           05  FILLER REDEFINES BNUM14F.
               10  BNUM14A PIC  X(0001).
           05  BNUM14I PIC  X(0002).
      *    -------------------------------
           05  BNAME14L PIC S9(0004) COMP.
           05  BNAME14F PIC  X(0001).
           05  FILLER REDEFINES BNAME14F.
               10  BNAME14A PIC  X(0001).
           05  BNAME14I PIC  X(0022).
      *    -------------------------------
           05  BAGE14L PIC S9(0004) COMP.
           05  BAGE14F PIC  X(0001).
           05  FILLER REDEFINES BAGE14F.
               10  BAGE14A PIC  X(0001).
           05  BAGE14I PIC  X(0002).
      *    -------------------------------
           05  BSTA14L PIC S9(0004) COMP.
           05  BSTA14F PIC  X(0001).
           05  FILLER REDEFINES BSTA14F.
               10  BSTA14A PIC  X(0001).
           05  BSTA14I PIC  X(0001).
      *    -------------------------------
           05  BIDT14L PIC S9(0004) COMP.
           05  BIDT14F PIC  X(0001).
           05  FILLER REDEFINES BIDT14F.
               10  BIDT14A PIC  X(0001).
           05  BIDT14I PIC  X(0008).
      *    -------------------------------
           05  BTYPE14L PIC S9(0004) COMP.
           05  BTYPE14F PIC  X(0001).
           05  FILLER REDEFINES BTYPE14F.
               10  BTYPE14A PIC  X(0001).
           05  BTYPE14I PIC  X(0001).
      *    -------------------------------
           05  BCARR14L PIC S9(0004) COMP.
           05  BCARR14F PIC  X(0001).
           05  FILLER REDEFINES BCARR14F.
               10  BCARR14A PIC  X(0001).
           05  BCARR14I PIC  X(0001).
      *    -------------------------------
           05  BCLAM14L PIC S9(0004) COMP.
           05  BCLAM14F PIC  X(0001).
           05  FILLER REDEFINES BCLAM14F.
               10  BCLAM14A PIC  X(0001).
           05  BCLAM14I PIC  X(0007).
      *    -------------------------------
           05  BCERT14L PIC S9(0004) COMP.
           05  BCERT14F PIC  X(0001).
           05  FILLER REDEFINES BCERT14F.
               10  BCERT14A PIC  X(0001).
           05  BCERT14I PIC  X(0016).
      *    -------------------------------
           05  BACCT14L PIC S9(0004) COMP.
           05  BACCT14F PIC  X(0001).
           05  FILLER REDEFINES BACCT14F.
               10  BACCT14A PIC  X(0001).
           05  BACCT14I PIC  X(0010).
      *    -------------------------------
           05  BNUM15L PIC S9(0004) COMP.
           05  BNUM15F PIC  X(0001).
           05  FILLER REDEFINES BNUM15F.
               10  BNUM15A PIC  X(0001).
           05  BNUM15I PIC  X(0002).
      *    -------------------------------
           05  BNAME15L PIC S9(0004) COMP.
           05  BNAME15F PIC  X(0001).
           05  FILLER REDEFINES BNAME15F.
               10  BNAME15A PIC  X(0001).
           05  BNAME15I PIC  X(0022).
      *    -------------------------------
           05  BAGE15L PIC S9(0004) COMP.
           05  BAGE15F PIC  X(0001).
           05  FILLER REDEFINES BAGE15F.
               10  BAGE15A PIC  X(0001).
           05  BAGE15I PIC  X(0002).
      *    -------------------------------
           05  BSTA15L PIC S9(0004) COMP.
           05  BSTA15F PIC  X(0001).
           05  FILLER REDEFINES BSTA15F.
               10  BSTA15A PIC  X(0001).
           05  BSTA15I PIC  X(0001).
      *    -------------------------------
           05  BIDT15L PIC S9(0004) COMP.
           05  BIDT15F PIC  X(0001).
           05  FILLER REDEFINES BIDT15F.
               10  BIDT15A PIC  X(0001).
           05  BIDT15I PIC  X(0008).
      *    -------------------------------
           05  BTYPE15L PIC S9(0004) COMP.
           05  BTYPE15F PIC  X(0001).
           05  FILLER REDEFINES BTYPE15F.
               10  BTYPE15A PIC  X(0001).
           05  BTYPE15I PIC  X(0001).
      *    -------------------------------
           05  BCARR15L PIC S9(0004) COMP.
           05  BCARR15F PIC  X(0001).
           05  FILLER REDEFINES BCARR15F.
               10  BCARR15A PIC  X(0001).
           05  BCARR15I PIC  X(0001).
      *    -------------------------------
           05  BCLAM15L PIC S9(0004) COMP.
           05  BCLAM15F PIC  X(0001).
           05  FILLER REDEFINES BCLAM15F.
               10  BCLAM15A PIC  X(0001).
           05  BCLAM15I PIC  X(0007).
      *    -------------------------------
           05  BCERT15L PIC S9(0004) COMP.
           05  BCERT15F PIC  X(0001).
           05  FILLER REDEFINES BCERT15F.
               10  BCERT15A PIC  X(0001).
           05  BCERT15I PIC  X(0016).
      *    -------------------------------
           05  BACCT15L PIC S9(0004) COMP.
           05  BACCT15F PIC  X(0001).
           05  FILLER REDEFINES BACCT15F.
               10  BACCT15A PIC  X(0001).
           05  BACCT15I PIC  X(0010).
      *    -------------------------------
           05  BNUM16L PIC S9(0004) COMP.
           05  BNUM16F PIC  X(0001).
           05  FILLER REDEFINES BNUM16F.
               10  BNUM16A PIC  X(0001).
           05  BNUM16I PIC  X(0002).
      *    -------------------------------
           05  BNAME16L PIC S9(0004) COMP.
           05  BNAME16F PIC  X(0001).
           05  FILLER REDEFINES BNAME16F.
               10  BNAME16A PIC  X(0001).
           05  BNAME16I PIC  X(0022).
      *    -------------------------------
           05  BAGE16L PIC S9(0004) COMP.
           05  BAGE16F PIC  X(0001).
           05  FILLER REDEFINES BAGE16F.
               10  BAGE16A PIC  X(0001).
           05  BAGE16I PIC  X(0002).
      *    -------------------------------
           05  BSTA16L PIC S9(0004) COMP.
           05  BSTA16F PIC  X(0001).
           05  FILLER REDEFINES BSTA16F.
               10  BSTA16A PIC  X(0001).
           05  BSTA16I PIC  X(0001).
      *    -------------------------------
           05  BIDT16L PIC S9(0004) COMP.
           05  BIDT16F PIC  X(0001).
           05  FILLER REDEFINES BIDT16F.
               10  BIDT16A PIC  X(0001).
           05  BIDT16I PIC  X(0008).
      *    -------------------------------
           05  BTYPE16L PIC S9(0004) COMP.
           05  BTYPE16F PIC  X(0001).
           05  FILLER REDEFINES BTYPE16F.
               10  BTYPE16A PIC  X(0001).
           05  BTYPE16I PIC  X(0001).
      *    -------------------------------
           05  BCARR16L PIC S9(0004) COMP.
           05  BCARR16F PIC  X(0001).
           05  FILLER REDEFINES BCARR16F.
               10  BCARR16A PIC  X(0001).
           05  BCARR16I PIC  X(0001).
      *    -------------------------------
           05  BCLAM16L PIC S9(0004) COMP.
           05  BCLAM16F PIC  X(0001).
           05  FILLER REDEFINES BCLAM16F.
               10  BCLAM16A PIC  X(0001).
           05  BCLAM16I PIC  X(0007).
      *    -------------------------------
           05  BCERT16L PIC S9(0004) COMP.
           05  BCERT16F PIC  X(0001).
           05  FILLER REDEFINES BCERT16F.
               10  BCERT16A PIC  X(0001).
           05  BCERT16I PIC  X(0016).
      *    -------------------------------
           05  BACCT16L PIC S9(0004) COMP.
           05  BACCT16F PIC  X(0001).
           05  FILLER REDEFINES BACCT16F.
               10  BACCT16A PIC  X(0001).
           05  BACCT16I PIC  X(0010).
      *    -------------------------------
           05  BSELL PIC S9(0004) COMP.
           05  BSELF PIC  X(0001).
           05  FILLER REDEFINES BSELF.
               10  BSELA PIC  X(0001).
           05  BSELI PIC  9(2).
      *    -------------------------------
           05  BEMSG1L PIC S9(0004) COMP.
           05  BEMSG1F PIC  X(0001).
           05  FILLER REDEFINES BEMSG1F.
               10  BEMSG1A PIC  X(0001).
           05  BEMSG1I PIC  X(0079).
      *    -------------------------------
           05  BPFKL PIC S9(0004) COMP.
           05  BPFKF PIC  X(0001).
           05  FILLER REDEFINES BPFKF.
               10  BPFKA PIC  X(0001).
           05  BPFKI PIC  99.
      *    -------------------------------
           05  BPFK5L PIC S9(0004) COMP.
           05  BPFK5F PIC  X(0001).
           05  FILLER REDEFINES BPFK5F.
               10  BPFK5A PIC  X(0001).
           05  BPFK5I PIC  X(0013).
      *    -------------------------------
           05  BPFK7L PIC S9(0004) COMP.
           05  BPFK7F PIC  X(0001).
           05  FILLER REDEFINES BPFK7F.
               10  BPFK7A PIC  X(0001).
           05  BPFK7I PIC  X(0011).
      *    -------------------------------
           05  BPF2L PIC S9(0004) COMP.
           05  BPF2F PIC  X(0001).
           05  FILLER REDEFINES BPF2F.
               10  BPF2A PIC  X(0001).
           05  BPF2I PIC  X(0030).
      *    -------------------------------
           05  BPFK6L PIC S9(0004) COMP.
           05  BPFK6F PIC  X(0001).
           05  FILLER REDEFINES BPFK6F.
               10  BPFK6A PIC  X(0001).
           05  BPFK6I PIC  X(0013).
       01  EL132BO REDEFINES EL132BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BHEAD1O PIC  X(0038).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCOMPO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BUSERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERTO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM01O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME01O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE01O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT01O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM01O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT01O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT01O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM02O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME02O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE02O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT02O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM02O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT02O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT02O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM03O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME03O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE03O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT03O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM03O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT03O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT03O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM04O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME04O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE04O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT04O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM04O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT04O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT04O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM05O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME05O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE05O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT05O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM05O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT05O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT05O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM06O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME06O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE06O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT06O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM06O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT06O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT06O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM07O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME07O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE07O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT07O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM07O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT07O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT07O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM08O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME08O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE08O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT08O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM08O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT08O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT08O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM09O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME09O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE09O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT09O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM09O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT09O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT09O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM10O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME10O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE10O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT10O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM10O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT10O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT10O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM11O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME11O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE11O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT11O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM11O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT11O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT11O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM12O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME12O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE12O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT12O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM12O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT12O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT12O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM13O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME13O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE13O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT13O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM13O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT13O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT13O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM14O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME14O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE14O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT14O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM14O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT14O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT14O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM15O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME15O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE15O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT15O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM15O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT15O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT15O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM16O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME16O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE16O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT16O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM16O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT16O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT16O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSELO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFKO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFK5O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFK7O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPF2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFK6O PIC  X(0013).
      *    -------------------------------
       01  EL132AI REDEFINES EL132BI.
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
           05  AHEAD1L PIC S9(0004) COMP.
           05  AHEAD1F PIC  X(0001).
           05  FILLER REDEFINES AHEAD1F.
               10  AHEAD1A PIC  X(0001).
           05  AHEAD1I PIC  X(0038).
      *    -------------------------------
           05  ACOMPL PIC S9(0004) COMP.
           05  ACOMPF PIC  X(0001).
           05  FILLER REDEFINES ACOMPF.
               10  ACOMPA PIC  X(0001).
           05  ACOMPI PIC  X(0003).
      *    -------------------------------
           05  AUSERIDL PIC S9(0004) COMP.
           05  AUSERIDF PIC  X(0001).
           05  FILLER REDEFINES AUSERIDF.
               10  AUSERIDA PIC  X(0001).
           05  AUSERIDI PIC  X(0004).
      *    -------------------------------
           05  ACLAIML PIC S9(0004) COMP.
           05  ACLAIMF PIC  X(0001).
           05  FILLER REDEFINES ACLAIMF.
               10  ACLAIMA PIC  X(0001).
           05  ACLAIMI PIC  X(0007).
      *    -------------------------------
           05  ACARIERL PIC S9(0004) COMP.
           05  ACARIERF PIC  X(0001).
           05  FILLER REDEFINES ACARIERF.
               10  ACARIERA PIC  X(0001).
           05  ACARIERI PIC  X(0001).
      *    -------------------------------
           05  ACERTNOL PIC S9(0004) COMP.
           05  ACERTNOF PIC  X(0001).
           05  FILLER REDEFINES ACERTNOF.
               10  ACERTNOA PIC  X(0001).
           05  ACERTNOI PIC  X(0010).
      *    -------------------------------
           05  ACERTSXL PIC S9(0004) COMP.
           05  ACERTSXF PIC  X(0001).
           05  FILLER REDEFINES ACERTSXF.
               10  ACERTSXA PIC  X(0001).
           05  ACERTSXI PIC  X(0001).
      *    -------------------------------
           05  ALNAMEL PIC S9(0004) COMP.
           05  ALNAMEF PIC  X(0001).
           05  FILLER REDEFINES ALNAMEF.
               10  ALNAMEA PIC  X(0001).
           05  ALNAMEI PIC  X(0015).
      *    -------------------------------
           05  AFNAMEL PIC S9(0004) COMP.
           05  AFNAMEF PIC  X(0001).
           05  FILLER REDEFINES AFNAMEF.
               10  AFNAMEA PIC  X(0001).
           05  AFNAMEI PIC  X(0012).
      *    -------------------------------
           05  AMINITL PIC S9(0004) COMP.
           05  AMINITF PIC  X(0001).
           05  FILLER REDEFINES AMINITF.
               10  AMINITA PIC  X(0001).
           05  AMINITI PIC  X(0001).
      *    -------------------------------
           05  AACCTL PIC S9(0004) COMP.
           05  AACCTF PIC  X(0001).
           05  FILLER REDEFINES AACCTF.
               10  AACCTA PIC  X(0001).
           05  AACCTI PIC  X(0010).
      *    -------------------------------
           05  AOPT3L PIC S9(0004) COMP.
           05  AOPT3F PIC  X(0001).
           05  FILLER REDEFINES AOPT3F.
               10  AOPT3A PIC  X(0001).
           05  AOPT3I PIC  X(0014).
      *    -------------------------------
           05  ASSOPTL PIC S9(0004) COMP.
           05  ASSOPTF PIC  X(0001).
           05  FILLER REDEFINES ASSOPTF.
               10  ASSOPTA PIC  X(0001).
           05  ASSOPTI PIC  X(0023).
      *    -------------------------------
           05  ASSNL PIC S9(0004) COMP.
           05  ASSNF PIC  X(0001).
           05  FILLER REDEFINES ASSNF.
               10  ASSNA PIC  X(0001).
           05  ASSNI PIC  X(0011).
      *    -------------------------------
           05  ACRTNOL PIC S9(0004) COMP.
           05  ACRTNOF PIC  X(0001).
           05  FILLER REDEFINES ACRTNOF.
               10  ACRTNOA PIC  X(0001).
           05  ACRTNOI PIC  X(0010).
      *    -------------------------------
           05  ACRTSXL PIC S9(0004) COMP.
           05  ACRTSXF PIC  X(0001).
           05  FILLER REDEFINES ACRTSXF.
               10  ACRTSXA PIC  X(0001).
           05  ACRTSXI PIC  X(0001).
      *    -------------------------------
           05  ACCNH1L PIC S9(0004) COMP.
           05  ACCNH1F PIC  X(0001).
           05  FILLER REDEFINES ACCNH1F.
               10  ACCNH1A PIC  X(0001).
           05  ACCNH1I PIC  X(0014).
      *    -------------------------------
           05  ACCNH2L PIC S9(0004) COMP.
           05  ACCNH2F PIC  X(0001).
           05  FILLER REDEFINES ACCNH2F.
               10  ACCNH2A PIC  X(0001).
           05  ACCNH2I PIC  X(0019).
      *    -------------------------------
           05  ACCNNOL PIC S9(0004) COMP.
           05  ACCNNOF PIC  X(0001).
           05  FILLER REDEFINES ACCNNOF.
               10  ACCNNOA PIC  X(0001).
           05  ACCNNOI PIC  X(0016).
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
      *    -------------------------------
           05  APFK5L PIC S9(0004) COMP.
           05  APFK5F PIC  X(0001).
           05  FILLER REDEFINES APFK5F.
               10  APFK5A PIC  X(0001).
           05  APFK5I PIC  X(0019).
      *    -------------------------------
           05  APFK6L PIC S9(0004) COMP.
           05  APFK6F PIC  X(0001).
           05  FILLER REDEFINES APFK6F.
               10  APFK6A PIC  X(0001).
           05  APFK6I PIC  X(0021).
       01  EL132AO REDEFINES EL132BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHEAD1O PIC  X(0038).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACOMPO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUSERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAIMO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERTSXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNAMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFNAMEO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMINITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AOPT3O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASSOPTO PIC  X(0023).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASSNO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACRTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACRTSXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCNH1O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCNH2O PIC  X(0019).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCNNOO PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFKO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFK5O PIC  X(0019).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFK6O PIC  X(0021).
      *    -------------------------------
       01  EL132CI REDEFINES EL132BI.
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
           05  SEQUL PIC S9(0004) COMP.
           05  SEQUF PIC  X(0001).
           05  FILLER REDEFINES SEQUF.
               10  SEQUA PIC  X(0001).
           05  SEQUI PIC  X(0010).
      *    -------------------------------
           05  CCOMPL PIC S9(0004) COMP.
           05  CCOMPF PIC  X(0001).
           05  FILLER REDEFINES CCOMPF.
               10  CCOMPA PIC  X(0001).
           05  CCOMPI PIC  X(0003).
      *    -------------------------------
           05  CUSERIDL PIC S9(0004) COMP.
           05  CUSERIDF PIC  X(0001).
           05  FILLER REDEFINES CUSERIDF.
               10  CUSERIDA PIC  X(0001).
           05  CUSERIDI PIC  X(0004).
      *    -------------------------------
           05  CCLAIML PIC S9(0004) COMP.
           05  CCLAIMF PIC  X(0001).
           05  FILLER REDEFINES CCLAIMF.
               10  CCLAIMA PIC  X(0001).
           05  CCLAIMI PIC  X(0007).
      *    -------------------------------
           05  CCARIERL PIC S9(0004) COMP.
           05  CCARIERF PIC  X(0001).
           05  FILLER REDEFINES CCARIERF.
               10  CCARIERA PIC  X(0001).
           05  CCARIERI PIC  X(0001).
      *    -------------------------------
           05  CCERTNOL PIC S9(0004) COMP.
           05  CCERTNOF PIC  X(0001).
           05  FILLER REDEFINES CCERTNOF.
               10  CCERTNOA PIC  X(0001).
           05  CCERTNOI PIC  X(0010).
      *    -------------------------------
           05  CCERTSXL PIC S9(0004) COMP.
           05  CCERTSXF PIC  X(0001).
           05  FILLER REDEFINES CCERTSXF.
               10  CCERTSXA PIC  X(0001).
           05  CCERTSXI PIC  X(0001).
      *    -------------------------------
           05  CTYPEL PIC S9(0004) COMP.
           05  CTYPEF PIC  X(0001).
           05  FILLER REDEFINES CTYPEF.
               10  CTYPEA PIC  X(0001).
           05  CTYPEI PIC  X(0001).
      *    -------------------------------
           05  PCERTNOL PIC S9(0004) COMP.
           05  PCERTNOF PIC  X(0001).
           05  FILLER REDEFINES PCERTNOF.
               10  PCERTNOA PIC  X(0001).
           05  PCERTNOI PIC  X(0010).
      *    -------------------------------
           05  PSUFXL PIC S9(0004) COMP.
           05  PSUFXF PIC  X(0001).
           05  FILLER REDEFINES PSUFXF.
               10  PSUFXA PIC  X(0001).
           05  PSUFXI PIC  X(0001).
      *    -------------------------------
           05  CSTATL PIC S9(0004) COMP.
           05  CSTATF PIC  X(0001).
           05  FILLER REDEFINES CSTATF.
               10  CSTATA PIC  X(0001).
           05  CSTATI PIC  X(0001).
      *    -------------------------------
           05  CINCREDL PIC S9(0004) COMP.
           05  CINCREDF PIC  X(0001).
           05  FILLER REDEFINES CINCREDF.
               10  CINCREDA PIC  X(0001).
           05  CINCREDI PIC  X(0008).
      *    -------------------------------
           05  CREPORTL PIC S9(0004) COMP.
           05  CREPORTF PIC  X(0001).
           05  FILLER REDEFINES CREPORTF.
               10  CREPORTA PIC  X(0001).
           05  CREPORTI PIC  X(0008).
      *    -------------------------------
           05  CCAUSCDL PIC S9(0004) COMP.
           05  CCAUSCDF PIC  X(0001).
           05  FILLER REDEFINES CCAUSCDF.
               10  CCAUSCDA PIC  X(0001).
           05  CCAUSCDI PIC  X(0006).
      *    -------------------------------
           05  CESTENDL PIC S9(0004) COMP.
           05  CESTENDF PIC  X(0001).
           05  FILLER REDEFINES CESTENDF.
               10  CESTENDA PIC  X(0001).
           05  CESTENDI PIC  X(0008).
      *    -------------------------------
           05  CCAUSEL PIC S9(0004) COMP.
           05  CCAUSEF PIC  X(0001).
           05  FILLER REDEFINES CCAUSEF.
               10  CCAUSEA PIC  X(0001).
           05  CCAUSEI PIC  X(0060).
      *    -------------------------------
           05  CBENEL PIC S9(0004) COMP.
           05  CBENEF PIC  X(0001).
           05  FILLER REDEFINES CBENEF.
               10  CBENEA PIC  X(0001).
           05  CBENEI PIC  X(0010).
      *    -------------------------------
           05  CBDATEL PIC S9(0004) COMP.
           05  CBDATEF PIC  X(0001).
           05  FILLER REDEFINES CBDATEF.
               10  CBDATEA PIC  X(0001).
           05  CBDATEI PIC  X(0008).
      *    -------------------------------
           05  CSSNL PIC S9(0004) COMP.
           05  CSSNF PIC  X(0001).
           05  FILLER REDEFINES CSSNF.
               10  CSSNA PIC  X(0001).
           05  CSSNI PIC  X(0011).
      *    -------------------------------
           05  CSEXL PIC S9(0004) COMP.
           05  CSEXF PIC  X(0001).
           05  FILLER REDEFINES CSEXF.
               10  CSEXA PIC  X(0001).
           05  CSEXI PIC  X(0001).
      *    -------------------------------
           05  CLNAMEL PIC S9(0004) COMP.
           05  CLNAMEF PIC  X(0001).
           05  FILLER REDEFINES CLNAMEF.
               10  CLNAMEA PIC  X(0001).
           05  CLNAMEI PIC  X(0015).
      *    -------------------------------
           05  CFNAMEL PIC S9(0004) COMP.
           05  CFNAMEF PIC  X(0001).
           05  FILLER REDEFINES CFNAMEF.
               10  CFNAMEA PIC  X(0001).
           05  CFNAMEI PIC  X(0012).
      *    -------------------------------
           05  CMNAMEL PIC S9(0004) COMP.
           05  CMNAMEF PIC  X(0001).
           05  FILLER REDEFINES CMNAMEF.
               10  CMNAMEA PIC  X(0001).
           05  CMNAMEI PIC  X(0001).
      *    -------------------------------
           05  LOANNOL PIC S9(0004) COMP.
           05  LOANNOF PIC  X(0001).
           05  FILLER REDEFINES LOANNOF.
               10  LOANNOA PIC  X(0001).
           05  LOANNOI PIC  X(0008).
      *    -------------------------------
           05  CLNMEL PIC S9(0004) COMP.
           05  CLNMEF PIC  X(0001).
           05  FILLER REDEFINES CLNMEF.
               10  CLNMEA PIC  X(0001).
           05  CLNMEI PIC  X(0015).
      *    -------------------------------
           05  CFNMEL PIC S9(0004) COMP.
           05  CFNMEF PIC  X(0001).
           05  FILLER REDEFINES CFNMEF.
               10  CFNMEA PIC  X(0001).
           05  CFNMEI PIC  X(0012).
      *    -------------------------------
           05  CINITL PIC S9(0004) COMP.
           05  CINITF PIC  X(0001).
           05  FILLER REDEFINES CINITF.
               10  CINITA PIC  X(0001).
           05  CINITI PIC  X(0001).
      *    -------------------------------
           05  LOANBALL PIC S9(0004) COMP.
           05  LOANBALF PIC  X(0001).
           05  FILLER REDEFINES LOANBALF.
               10  LOANBALA PIC  X(0001).
           05  LOANBALI PIC  9(7)V99.
      *    -------------------------------
           05  CPROCL PIC S9(0004) COMP.
           05  CPROCF PIC  X(0001).
           05  FILLER REDEFINES CPROCF.
               10  CPROCA PIC  X(0001).
           05  CPROCI PIC  X(0004).
      *    -------------------------------
           05  CSUPRL PIC S9(0004) COMP.
           05  CSUPRF PIC  X(0001).
           05  FILLER REDEFINES CSUPRF.
               10  CSUPRA PIC  X(0001).
           05  CSUPRI PIC  X(0001).
      *    -------------------------------
           05  CPRICDL PIC S9(0004) COMP.
           05  CPRICDF PIC  X(0001).
           05  FILLER REDEFINES CPRICDF.
               10  CPRICDA PIC  X(0001).
           05  CPRICDI PIC  X(0001).
      *    -------------------------------
           05  FILETOL PIC S9(0004) COMP.
           05  FILETOF PIC  X(0001).
           05  FILLER REDEFINES FILETOF.
               10  FILETOA PIC  X(0001).
           05  FILETOI PIC  X(0004).
      *    -------------------------------
           05  CPTHHDGL PIC S9(0004) COMP.
           05  CPTHHDGF PIC  X(0001).
           05  FILLER REDEFINES CPTHHDGF.
               10  CPTHHDGA PIC  X(0001).
           05  CPTHHDGI PIC  X(0010).
      *    -------------------------------
           05  CPDTHRUL PIC S9(0004) COMP.
           05  CPDTHRUF PIC  X(0001).
           05  FILLER REDEFINES CPDTHRUF.
               10  CPDTHRUA PIC  X(0001).
           05  CPDTHRUI PIC  X(0008).
      *    -------------------------------
           05  CTOTPDL PIC S9(0004) COMP.
           05  CTOTPDF PIC  X(0001).
           05  FILLER REDEFINES CTOTPDF.
               10  CTOTPDA PIC  X(0001).
           05  CTOTPDI PIC  9(7)V99.
      *    -------------------------------
           05  CNODAYSL PIC S9(0004) COMP.
           05  CNODAYSF PIC  X(0001).
           05  FILLER REDEFINES CNODAYSF.
               10  CNODAYSA PIC  X(0001).
           05  CNODAYSI PIC  9(5).
      *    -------------------------------
           05  CNOPMTSL PIC S9(0004) COMP.
           05  CNOPMTSF PIC  X(0001).
           05  FILLER REDEFINES CNOPMTSF.
               10  CNOPMTSA PIC  X(0001).
           05  CNOPMTSI PIC  9(4).
      *    -------------------------------
           05  CESTABL PIC S9(0004) COMP.
           05  CESTABF PIC  X(0001).
           05  FILLER REDEFINES CESTABF.
               10  CESTABA PIC  X(0001).
           05  CESTABI PIC  X(0008).
      *    -------------------------------
           05  COCCL PIC S9(0004) COMP.
           05  COCCF PIC  X(0001).
           05  FILLER REDEFINES COCCF.
               10  COCCA PIC  X(0001).
           05  COCCI PIC  X(0002).
      *    -------------------------------
           05  CMNTDTEL PIC S9(0004) COMP.
           05  CMNTDTEF PIC  X(0001).
           05  FILLER REDEFINES CMNTDTEF.
               10  CMNTDTEA PIC  X(0001).
           05  CMNTDTEI PIC  X(0008).
      *    -------------------------------
           05  CMNTYPEL PIC S9(0004) COMP.
           05  CMNTYPEF PIC  X(0001).
           05  FILLER REDEFINES CMNTYPEF.
               10  CMNTYPEA PIC  X(0001).
           05  CMNTYPEI PIC  X(0006).
      *    -------------------------------
           05  CCARRL PIC S9(0004) COMP.
           05  CCARRF PIC  X(0001).
           05  FILLER REDEFINES CCARRF.
               10  CCARRA PIC  X(0001).
           05  CCARRI PIC  X(0001).
      *    -------------------------------
           05  CGRPL PIC S9(0004) COMP.
           05  CGRPF PIC  X(0001).
           05  FILLER REDEFINES CGRPF.
               10  CGRPA PIC  X(0001).
           05  CGRPI PIC  X(0006).
      *    -------------------------------
           05  CSTATEL PIC S9(0004) COMP.
           05  CSTATEF PIC  X(0001).
           05  FILLER REDEFINES CSTATEF.
               10  CSTATEA PIC  X(0001).
           05  CSTATEI PIC  X(0002).
      *    -------------------------------
           05  CACCNTL PIC S9(0004) COMP.
           05  CACCNTF PIC  X(0001).
           05  FILLER REDEFINES CACCNTF.
               10  CACCNTA PIC  X(0001).
           05  CACCNTI PIC  X(0010).
      *    -------------------------------
           05  CEFFDTL PIC S9(0004) COMP.
           05  CEFFDTF PIC  X(0001).
           05  FILLER REDEFINES CEFFDTF.
               10  CEFFDTA PIC  X(0001).
           05  CEFFDTI PIC  X(0008).
      *    -------------------------------
           05  CISAGEL PIC S9(0004) COMP.
           05  CISAGEF PIC  X(0001).
           05  FILLER REDEFINES CISAGEF.
               10  CISAGEA PIC  X(0001).
           05  CISAGEI PIC  X(0002).
      *    -------------------------------
           05  CAPRL PIC S9(0004) COMP.
           05  CAPRF PIC  X(0001).
           05  FILLER REDEFINES CAPRF.
               10  CAPRA PIC  X(0001).
           05  CAPRI PIC  9(4)V9(4).
      *    -------------------------------
           05  CPFREQL PIC S9(0004) COMP.
           05  CPFREQF PIC  X(0001).
           05  FILLER REDEFINES CPFREQF.
               10  CPFREQA PIC  X(0001).
           05  CPFREQI PIC  99.
      *    -------------------------------
           05  CINDGRPL PIC S9(0004) COMP.
           05  CINDGRPF PIC  X(0001).
           05  FILLER REDEFINES CINDGRPF.
               10  CINDGRPA PIC  X(0001).
           05  CINDGRPI PIC  X(0001).
      *    -------------------------------
           05  CPREMTPL PIC S9(0004) COMP.
           05  CPREMTPF PIC  X(0001).
           05  FILLER REDEFINES CPREMTPF.
               10  CPREMTPA PIC  X(0001).
           05  CPREMTPI PIC  X(0001).
      *    -------------------------------
           05  CREINCDL PIC S9(0004) COMP.
           05  CREINCDF PIC  X(0001).
           05  FILLER REDEFINES CREINCDF.
               10  CREINCDA PIC  X(0001).
           05  CREINCDI PIC  X(0003).
      *    -------------------------------
           05  CJLNMEL PIC S9(0004) COMP.
           05  CJLNMEF PIC  X(0001).
           05  FILLER REDEFINES CJLNMEF.
               10  CJLNMEA PIC  X(0001).
           05  CJLNMEI PIC  X(0015).
      *    -------------------------------
           05  CJFNMEL PIC S9(0004) COMP.
           05  CJFNMEF PIC  X(0001).
           05  FILLER REDEFINES CJFNMEF.
               10  CJFNMEA PIC  X(0001).
           05  CJFNMEI PIC  X(0012).
      *    -------------------------------
           05  CJINITL PIC S9(0004) COMP.
           05  CJINITF PIC  X(0001).
           05  FILLER REDEFINES CJINITF.
               10  CJINITA PIC  X(0001).
           05  CJINITI PIC  X(0001).
      *    -------------------------------
           05  CJAGEL PIC S9(0004) COMP.
           05  CJAGEF PIC  X(0001).
           05  FILLER REDEFINES CJAGEF.
               10  CJAGEA PIC  X(0001).
           05  CJAGEI PIC  X(0002).
      *    -------------------------------
           05  LCVDSCRL PIC S9(0004) COMP.
           05  LCVDSCRF PIC  X(0001).
           05  FILLER REDEFINES LCVDSCRF.
               10  LCVDSCRA PIC  X(0001).
           05  LCVDSCRI PIC  X(0006).
      *    -------------------------------
           05  LCVKINDL PIC S9(0004) COMP.
           05  LCVKINDF PIC  X(0001).
           05  FILLER REDEFINES LCVKINDF.
               10  LCVKINDA PIC  X(0001).
           05  LCVKINDI PIC  X(0003).
      *    -------------------------------
           05  LCVCDL PIC S9(0004) COMP.
           05  LCVCDF PIC  X(0001).
           05  FILLER REDEFINES LCVCDF.
               10  LCVCDA PIC  X(0001).
           05  LCVCDI PIC  99.
      *    -------------------------------
           05  LCVOTRML PIC S9(0004) COMP.
           05  LCVOTRMF PIC  X(0001).
           05  FILLER REDEFINES LCVOTRMF.
               10  LCVOTRMA PIC  X(0001).
           05  LCVOTRMI PIC  999.
      *    -------------------------------
           05  LCVRTRML PIC S9(0004) COMP.
           05  LCVRTRMF PIC  X(0001).
           05  FILLER REDEFINES LCVRTRMF.
               10  LCVRTRMA PIC  X(0001).
           05  LCVRTRMI PIC  X(0003).
      *    -------------------------------
           05  LCVBENEL PIC S9(0004) COMP.
           05  LCVBENEF PIC  X(0001).
           05  FILLER REDEFINES LCVBENEF.
               10  LCVBENEA PIC  X(0001).
           05  LCVBENEI PIC  9(9)V99.
      *    -------------------------------
           05  LCVFORML PIC S9(0004) COMP.
           05  LCVFORMF PIC  X(0001).
           05  FILLER REDEFINES LCVFORMF.
               10  LCVFORMA PIC  X(0001).
           05  LCVFORMI PIC  X(0012).
      *    -------------------------------
           05  LCVCNDTL PIC S9(0004) COMP.
           05  LCVCNDTF PIC  X(0001).
           05  FILLER REDEFINES LCVCNDTF.
               10  LCVCNDTA PIC  X(0001).
           05  LCVCNDTI PIC  X(0008).
      *    -------------------------------
           05  LCVEXITL PIC S9(0004) COMP.
           05  LCVEXITF PIC  X(0001).
           05  FILLER REDEFINES LCVEXITF.
               10  LCVEXITA PIC  X(0001).
           05  LCVEXITI PIC  X(0008).
      *    -------------------------------
           05  LCVSTATL PIC S9(0004) COMP.
           05  LCVSTATF PIC  X(0001).
           05  FILLER REDEFINES LCVSTATF.
               10  LCVSTATA PIC  X(0001).
           05  LCVSTATI PIC  X(0006).
      *    -------------------------------
           05  ACVDSCRL PIC S9(0004) COMP.
           05  ACVDSCRF PIC  X(0001).
           05  FILLER REDEFINES ACVDSCRF.
               10  ACVDSCRA PIC  X(0001).
           05  ACVDSCRI PIC  X(0006).
      *    -------------------------------
           05  ACVKINDL PIC S9(0004) COMP.
           05  ACVKINDF PIC  X(0001).
           05  FILLER REDEFINES ACVKINDF.
               10  ACVKINDA PIC  X(0001).
           05  ACVKINDI PIC  X(0003).
      *    -------------------------------
           05  ACVCDL PIC S9(0004) COMP.
           05  ACVCDF PIC  X(0001).
           05  FILLER REDEFINES ACVCDF.
               10  ACVCDA PIC  X(0001).
           05  ACVCDI PIC  99.
      *    -------------------------------
           05  ACVOTRML PIC S9(0004) COMP.
           05  ACVOTRMF PIC  X(0001).
           05  FILLER REDEFINES ACVOTRMF.
               10  ACVOTRMA PIC  X(0001).
           05  ACVOTRMI PIC  999.
      *    -------------------------------
           05  ACVRTRML PIC S9(0004) COMP.
           05  ACVRTRMF PIC  X(0001).
           05  FILLER REDEFINES ACVRTRMF.
               10  ACVRTRMA PIC  X(0001).
           05  ACVRTRMI PIC  X(0003).
      *    -------------------------------
           05  ACVBENEL PIC S9(0004) COMP.
           05  ACVBENEF PIC  X(0001).
           05  FILLER REDEFINES ACVBENEF.
               10  ACVBENEA PIC  X(0001).
           05  ACVBENEI PIC  9(9)V99.
      *    -------------------------------
           05  ACVFORML PIC S9(0004) COMP.
           05  ACVFORMF PIC  X(0001).
           05  FILLER REDEFINES ACVFORMF.
               10  ACVFORMA PIC  X(0001).
           05  ACVFORMI PIC  X(0012).
      *    -------------------------------
           05  ACVCNDTL PIC S9(0004) COMP.
           05  ACVCNDTF PIC  X(0001).
           05  FILLER REDEFINES ACVCNDTF.
               10  ACVCNDTA PIC  X(0001).
           05  ACVCNDTI PIC  X(0008).
      *    -------------------------------
           05  ACVEXITL PIC S9(0004) COMP.
           05  ACVEXITF PIC  X(0001).
           05  FILLER REDEFINES ACVEXITF.
               10  ACVEXITA PIC  X(0001).
           05  ACVEXITI PIC  X(0008).
      *    -------------------------------
           05  ACVSTATL PIC S9(0004) COMP.
           05  ACVSTATF PIC  X(0001).
           05  FILLER REDEFINES ACVSTATF.
               10  ACVSTATA PIC  X(0001).
           05  ACVSTATI PIC  X(0006).
      *    -------------------------------
           05  CEMSG1L PIC S9(0004) COMP.
           05  CEMSG1F PIC  X(0001).
           05  FILLER REDEFINES CEMSG1F.
               10  CEMSG1A PIC  X(0001).
           05  CEMSG1I PIC  X(0072).
      *    -------------------------------
           05  CPFKEYL PIC S9(0004) COMP.
           05  CPFKEYF PIC  X(0001).
           05  FILLER REDEFINES CPFKEYF.
               10  CPFKEYA PIC  X(0001).
           05  CPFKEYI PIC  99.
      *    -------------------------------
           05  CEMSG2L PIC S9(0004) COMP.
           05  CEMSG2F PIC  X(0001).
           05  FILLER REDEFINES CEMSG2F.
               10  CEMSG2A PIC  X(0001).
           05  CEMSG2I PIC  X(0072).
       01  EL132CO REDEFINES EL132BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEQUO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCOMPO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CUSERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCLAIMO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCARIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCERTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCERTSXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCERTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PSUFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTATO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CINCREDO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREPORTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAUSCDO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CESTENDO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAUSEO PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBENEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSSNO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSEXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLNAMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CFNAMEO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMNAMEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOANNOO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLNMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CFNMEO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CINITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOANBALO PIC  ZZZZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPROCO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSUPRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPRICDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FILETOO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPTHHDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPDTHRUO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTOTPDO PIC  Z(06).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNODAYSO PIC  ZZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNOPMTSO PIC  ZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CESTABO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COCCO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMNTDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMNTYPEO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CGRPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACCNTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CEFFDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CISAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAPRO PIC  9(3).9(4).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPFREQO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CINDGRPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPREMTPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREINCDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CJLNMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CJFNMEO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CJINITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CJAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVDSCRO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVKINDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVCDO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVOTRMO PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVRTRMO PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVBENEO PIC  ZZZZZZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVFORMO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVCNDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVEXITO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVSTATO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVDSCRO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVKINDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVCDO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVOTRMO PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVRTRMO PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVBENEO PIC  ZZZZZZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVFORMO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVCNDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVEXITO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVSTATO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CEMSG1O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPFKEYO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CEMSG2O PIC  X(0072).
      *    -------------------------------
00194
00195      EJECT
00196 *                                COPY ELCLOGOF.
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
00197
00198      EJECT
00199 *                                COPY ELCATTR.
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
00200
00201      EJECT
00202 *                                COPY ELCAID.
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
00203
00204  01  FILLER      REDEFINES
00205      DFHAID.
00206
00207      12  FILLER                  PIC X(8).
00208
00209      12  PF-VALUES               PIC X
00210          OCCURS 24 TIMES.
00211      EJECT
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
00213
00214  01  DFHCOMMAREA                 PIC X(1024).
00215
00216 *01 DFHBLLDS    COMP
00217 *               SYNCHRONIZED.
00218 *    12  FILLER                  PIC S9(8).
00219 *    12  ELMSTR-POINTER          PIC S9(8).
00220 *    12  ELCNTL-POINTER          PIC S9(8).
00221
00222      EJECT
00223 *                                COPY ELCMSTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCMSTR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.012                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CLAIM MASTER FILE                         *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 350  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELMSTR                         RKP=2,LEN=20   *
00013 *       ALTERNATE PATH1 = ELMSTR2 (BY NAME)       RKP=22,LEN=29  *
00014 *       ALTERNATE PATH2 = ELMSTR3 (BY SOC SEC NO) RKP=51,LEN=12  *
00015 *       ALTERNATE PATH3 = ELMSTR5 (BY CERT NO)    RKP=63,LEN=12  *
00016 *       ALTERNATE PATH4 = ELMSTR6 (BY CREDIT CARD NO)            *
00017 *                                                 RKP=75,LEN=21  *
00018 *                                                                *
00019 *   **** NOTE ****                                               *
00020 *             ANY CHANGES TO THIS COPYBOOK MUST ALSO BE          *
00021 *             IMPLEMENTED IN COPYBOOK ELCRETR (RETRIEVE MASTER)  *
00022 *                                                                *
00023 *   LOG = YES                                                    *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
120503******************************************************************
120503*                   C H A N G E   L O G
120503*
120503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120503*-----------------------------------------------------------------
120503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120503* EFFECTIVE    NUMBER
120503*-----------------------------------------------------------------
120503* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
080307* 080307    2007032100001  PEMA  ADD TOTAL INTEREST PAID FIELD
00025 ******************************************************************
00026  01  CLAIM-MASTER.
00027      12  CL-RECORD-ID                PIC XX.
00028          88  VALID-CL-ID         VALUE 'CL'.
00029
00030      12  CL-CONTROL-PRIMARY.
00031          16  CL-COMPANY-CD           PIC X.
00032          16  CL-CARRIER              PIC X.
00033          16  CL-CLAIM-NO             PIC X(7).
00034          16  CL-CERT-NO.
00035              20  CL-CERT-PRIME       PIC X(10).
00036              20  CL-CERT-SFX         PIC X.
00037
00038      12  CL-CONTROL-BY-NAME.
00039          16  CL-COMPANY-CD-A1        PIC X.
00040          16  CL-INSURED-LAST-NAME    PIC X(15).
00041          16  CL-INSURED-NAME.
00042              20  CL-INSURED-1ST-NAME PIC X(12).
00043              20  CL-INSURED-MID-INIT PIC X.
00044
00045      12  CL-CONTROL-BY-SSN.
00046          16  CL-COMPANY-CD-A2        PIC X.
00047          16  CL-SOC-SEC-NO.
00048              20  CL-SSN-STATE        PIC XX.
00049              20  CL-SSN-ACCOUNT      PIC X(6).
00050              20  CL-SSN-LN3          PIC X(3).
00051
00052      12  CL-CONTROL-BY-CERT-NO.
00053          16  CL-COMPANY-CD-A4        PIC X.
00054          16  CL-CERT-NO-A4.
00055              20  CL-CERT-A4-PRIME    PIC X(10).
00056              20  CL-CERT-A4-SFX      PIC X.
00057
00058      12  CL-CONTROL-BY-CCN.
00059          16  CL-COMPANY-CD-A5        PIC X.
00060          16  CL-CCN-A5.
00061              20  CL-CCN.
00062                  24  CL-CCN-PREFIX-A5 PIC X(4).
00063                  24  CL-CCN-PRIME-A5 PIC X(12).
00064              20  CL-CCN-FILLER-A5    PIC X(4).
00065
00066      12  CL-INSURED-PROFILE-DATA.
00067          16  CL-INSURED-BIRTH-DT     PIC XX.
00068          16  CL-INSURED-SEX-CD       PIC X.
00069              88  INSURED-IS-MALE        VALUE 'M'.
00070              88  INSURED-IS-FEMALE      VALUE 'F'.
00071              88  INSURED-SEX-UNKNOWN    VALUE ' '.
00072          16  CL-INSURED-OCC-CD       PIC X(6).
00073          16  FILLER                  PIC X(5).
00074
00075      12  CL-PROCESSING-INFO.
00076          16  CL-PROCESSOR-ID         PIC X(4).
00077          16  CL-CLAIM-STATUS         PIC X.
00078              88  CLAIM-IS-OPEN          VALUE 'O'.
00079              88  CLAIM-IS-CLOSED        VALUE 'C'.
00080          16  CL-CLAIM-TYPE           PIC X.
00081 *            88  AH-CLAIM               VALUE 'A'.
00082 *            88  LIFE-CLAIM             VALUE 'L'.
00083 *            88  PROPERTY-CLAIM         VALUE 'P'.
00084 *            88  IUI-CLAIM              VALUE 'I'.
120503*            88  GAP-CLAIM              VALUE 'G'.
00085          16  CL-CLAIM-PREM-TYPE      PIC X.
00086              88  SINGLE-PREMIUM         VALUE '1'.
00087              88  O-B-COVERAGE           VALUE '2'.
00088              88  OPEN-END-COVERAGE      VALUE '3'.
00089          16  CL-INCURRED-DT          PIC XX.
00090          16  CL-REPORTED-DT          PIC XX.
00091          16  CL-FILE-ESTABLISH-DT    PIC XX.
00092          16  CL-EST-END-OF-DISAB-DT  PIC XX.
00093          16  CL-LAST-PMT-DT          PIC XX.
00094          16  CL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
00095          16  CL-PAID-THRU-DT         PIC XX.
00096          16  CL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
00097          16  CL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
00098          16  CL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
00099          16  CL-PMT-CALC-METHOD      PIC X.
00100              88  CL-360-DAY-YR          VALUE '1'.
00101              88  CL-365-DAY-YR          VALUE '2'.
00102              88  CL-FULL-MONTHS         VALUE '3'.
00103          16  CL-CAUSE-CD             PIC X(6).
00104
00105          16  CL-PRIME-CERT-NO.
00106              20  CL-PRIME-CERT-PRIME PIC X(10).
00107              20  CL-PRIME-CERT-SFX   PIC X.
00108
00109          16  CL-SYSTEM-IDENTIFIER    PIC XX.
00110              88  CL-CREDIT-CLAIM        VALUE 'CR'.
00111              88  CL-CONVENIENCE-CLAIM   VALUE 'CV'.
00112
00113          16  CL-MICROFILM-NO         PIC X(10).
00114          16  CL-PROG-FORM-TYPE       PIC X.
00115          16  CL-LAST-ADD-ON-DT       PIC XX.
00116
00117          16  CL-LAST-REOPEN-DT       PIC XX.
00118          16  CL-LAST-CLOSE-DT        PIC XX.
00119          16  CL-LAST-CLOSE-REASON    PIC X(01).
00120              88  FINAL-PAID             VALUE '1'.
00121              88  CLAIM-DENIED           VALUE '2'.
00122              88  AUTO-CLOSE             VALUE '3'.
00123              88  MANUAL-CLOSE           VALUE '4'.
00124              88  BENEFITS-CHANGED       VALUE 'C'.
00125              88  SETUP-ERRORS           VALUE 'E'.
00126          16  CL-ASSOC-CERT-SEQU      PIC S99.
00127          16  CL-ASSOC-CERT-TOTAL     PIC S99.
00128          16  CL-CLAIM-PAYMENT-STATUS PIC 9.
00129              88  PAYMENT-IN-PREP        VALUE 1 THRU 9.
080307         16  CL-TOTAL-INT-PAID       PIC S9(5)V99 COMP-3.
080307         16  FILLER                  PIC X.
00131
00132      12  CL-CERTIFICATE-DATA.
00133          16  CL-CERT-ORIGIN          PIC X.
00134              88  CERT-WAS-ONLINE        VALUE '1'.
00135              88  CERT-WAS-CREATED       VALUE '2'.
00136              88  COVERAGE-WAS-ADDED     VALUE '3'.
00137          16  CL-CERT-KEY-DATA.
00138              20  CL-CERT-CARRIER     PIC X.
00139              20  CL-CERT-GROUPING    PIC X(6).
00140              20  CL-CERT-STATE       PIC XX.
00141              20  CL-CERT-ACCOUNT.
00142                  24  CL-CERT-ACCOUNT-PREFIX PIC X(4).
00143                  24  CL-CERT-ACCOUNT-PRIME  PIC X(6).
00144              20  CL-CERT-EFF-DT      PIC XX.
00145
00146      12  CL-STATUS-CONTROLS.
00147          16  CL-PRIORITY-CD          PIC X.
00148              88  CONFIDENTIAL-DATA      VALUE '8'.
00149              88  HIGHEST-PRIORITY       VALUE '9'.
00150          16  CL-SUPV-ATTN-CD         PIC X.
00151              88  SUPV-NOT-REQUIRED      VALUE ' ' 'N'.
00152              88  SUPV-IS-REQUIRED       VALUE 'Y'.
00153          16  CL-PURGED-DT            PIC XX.
00154          16  CL-RESTORED-DT          PIC XX.
00155          16  CL-NEXT-AUTO-PAY-DT     PIC XX.
00156          16  CL-NEXT-RESEND-DT       PIC XX.
00157          16  CL-NEXT-FOLLOWUP-DT     PIC XX.
00158          16  FILLER                  PIC XX.
00159          16  CL-LAST-MAINT-DT        PIC XX.
00160          16  CL-LAST-MAINT-USER      PIC X(4).
00161          16  CL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
00162          16  CL-LAST-MAINT-TYPE      PIC X.
00163              88  CLAIM-SET-UP           VALUE ' '.
00164              88  PAYMENT-MADE           VALUE '1'.
00165              88  LETTER-SENT            VALUE '2'.
00166              88  MASTER-WAS-ALTERED     VALUE '3'.
00167              88  MASTER-WAS-RESTORED    VALUE '4'.
00168              88  INCURRED-DATE-CHANGED  VALUE '5'.
00169              88  FILE-CONVERTED         VALUE '6'.
00170              88  CHANGE-OF-BENEFITS     VALUE 'C'.
00171              88  ERROR-CORRECTION       VALUE 'E'.
00172          16  CL-RELATED-CLAIM-NO     PIC X(7).
00173          16  CL-HISTORY-ARCHIVE-DT   PIC XX.
00174          16  CL-BENEFICIARY          PIC X(10).
00175          16  CL-FILE-ESTABLISHED-BY  PIC X(4).
120808         16  CL-DENIAL-TYPE          PIC X.
                   88  CL-TYPE-DENIAL          VALUE '1'.
                   88  CL-TYPE-RESCISSION      VALUE '2'.
                   88  CL-TYPE-REFORMATION     VALUE '3'.
                   88  CL-TYPE-REF-TO-RES      VALUE '4'.
                   88  CL-TYPE-RECONSIDERED    VALUE '5'.
120808         16  FILLER                  PIC X(5).
00177
00178      12  CL-TRAILER-CONTROLS.
00179          16  CL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
00180              88  CL-1ST-TRL-AVAIL       VALUE +4095.
00181              88  CL-LAST-TRL-AVAIL      VALUE +100.
00182              88  CL-RESV-EXP-HIST-TRLR  VALUE +0.
00183          16  CL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
00184          16  FILLER                  PIC XX.
00185          16  CL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
00186          16  CL-ADDRESS-TRAILER-CNT.
00187              20  CL-INSURED-ADDR-CNT  PIC S9(1).
00188                  88  NO-INSURED-AVAILABLE    VALUE ZERO.
00189              20  CL-ACCOUNT-ADDR-CNT  PIC S9(1).
00190                  88  ACCOUNT-IS-ONLINE       VALUE ZERO.
00191              20  CL-BENIF-ADDR-CNT    PIC S9(1).
00192                  88  BENEFICIARY-IS-ONLINE   VALUE ZERO.
00193              20  CL-EMPLOYER-ADDR-CNT PIC S9(1).
00194                  88  NO-EMPLOY-AVAILABLE     VALUE ZERO.
00195              20  CL-DOCTOR-ADDR-CNT   PIC S9(1).
00196                  88  NO-DOCTOR-AVAILABLE     VALUE ZERO.
00197              20  CL-OTHER-1-ADDR-CNT  PIC S9(1).
00198                  88  NO-OTHER-1-ADDRESSES    VALUE ZERO.
00199              20  CL-OTHER-2-ADDR-CNT  PIC S9(1).
00200                  88  NO-OTHER-2-ADDRESSES    VALUE ZERO.
00201
00202      12  CL-CV-REFERENCE-NO.
00203          16  CL-CV-REFNO-PRIME       PIC X(18).
00204          16  CL-CV-REFNO-SFX         PIC XX.
00205
00206      12  CL-FILE-LOCATION            PIC X(4).
00207
00208      12  CL-PROCESS-ERRORS.
00209          16  CL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
00210              88  NO-FATAL-ERRORS        VALUE ZERO.
00211          16  CL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
00212              88  NO-FORCABLE-ERRORS     VALUE ZERO.
00213
00214      12  CL-PRODUCT-CD               PIC X.
00215
00216      12  CL-CURRENT-KEY-DATA.
00217          16  CL-CURRENT-CARRIER      PIC X.
00218          16  CL-CURRENT-GROUPING     PIC X(6).
00219          16  CL-CURRENT-STATE        PIC XX.
00220          16  CL-CURRENT-ACCOUNT      PIC X(10).
00221
00222      12  CL-ASSOCIATES               PIC X.
00223          88  CL-ASSOC-NO-INTERFACE      VALUE 'A'.
00224          88  CL-ASSOC-INTERFACE         VALUE 'I'.
00225          88  CL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
00226          88  CL-NON-ASSOC-INTERFACE     VALUE 'M'.
00227
00228      12  CL-ACTIVITY-CODE            PIC 99.
00229      12  CL-ACTIVITY-MAINT-DT        PIC XX.
00230      12  CL-ACTIVITY-MAINT-TYPE      PIC X(4).
00231
00232      12  CL-LAPSE-REPORT-CODE        PIC 9.
00233      12  CL-LAG-REPORT-CODE          PIC 9.
00234      12  CL-LOAN-TYPE                PIC XX.
00235      12  CL-LEGAL-STATE              PIC XX.
00236
CIDMOD*    12  FILLER                      PIC X(5).
CIDMOD     12  CL-YESNOSW                  PIC X.
CIDMOD     12  FILLER                      PIC X(4).
00224
00225      EJECT
00226 *                                COPY ELCRETR.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCRETR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CLAIM MASTER RETRIEVE FILE                *
00008 *                                                                *
00009 *   **** NOTE -- THIS FILE IS IDENTICAL TO CLAIM MASTER (ELMSTR) *
00010 *   ****      ANY CHANGES TO THIS COPYBOOK OR ELCMSTR MUST BE    *
00011 *   ****      DUPLICATED IN THE OTHER.                           *
00012 *                                                                *
00013 *   FILE TYPE = VSAM,KSDS                                        *
00014 *   RECORD SIZE = 350  RECFORM = FIXED                           *
00015 *                                                                *
00016 *   BASE CLUSTER = ELRETR                         RKP=2,LEN=20   *
00017 *       ALTERNATE PATH1 = ELRETR2 (BY NAME)       RKP=22,LEN=29  *
00018 *       ALTERNATE PATH2 = ELRETR3 (BY SOC SEC NO) RKP=51,LEN=12  *
00019 *       ALTERNATE PATH3 = ELRETR5 (BY CERT NO)    RKP=63,LEN=12  *
00020 *       ALTERNATE PATH4 = ELRETR6 (BY CREDIT CARD NO)
00021 *                                                 RKP=75,LEN=21  *
00022 *                                                                *
00023 *   LOG = YES                                                    *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00025 ******************************************************************
00026  01  RETRIEVE-MASTER.
00027      12  RL-RECORD-ID                PIC XX.
00028          88  VALID-RL-ID         VALUE 'RL'.
00029
00030      12  RL-CONTROL-PRIMARY.
00031          16  RL-COMPANY-CD           PIC X.
00032          16  RL-CARRIER              PIC X.
00033          16  RL-CLAIM-NO             PIC X(7).
00034          16  RL-CERT-NO.
00035              20  RL-CERT-PRIME       PIC X(10).
00036              20  RL-CERT-SFX         PIC X.
00037
00038      12  RL-CONTROL-BY-NAME.
00039          16  RL-COMPANY-CD-A1        PIC X.
00040          16  RL-INSURED-LAST-NAME    PIC X(15).
00041          16  RL-INSURED-NAME.
00042              20  RL-INSURED-1ST-NAME PIC X(12).
00043              20  RL-INSURED-MID-INIT PIC X.
00044
00045      12  RL-CONTROL-BY-SSN.
00046          16  RL-COMPANY-CD-A2        PIC X.
00047          16  RL-SOC-SEC-NO.
00048              20  RL-SSN-STATE        PIC XX.
00049              20  RL-SSN-ACCOUNT      PIC X(6).
00050              20  RL-SSN-LN3          PIC X(3).
00051
00052      12  RL-CONTROL-BY-CERT-NO.
00053          16  RL-COMPANY-CD-A4        PIC X.
00054          16  RL-CERT-NO-A4.
00055              20  RL-CERT-A4-PRIME    PIC X(10).
00056              20  RL-CERT-A4-SFX      PIC X.
00057
00058      12  RL-CONTROL-BY-CCN.
00059          16  RL-COMPANY-CD-A5        PIC X.
00060          16  RL-CCN-A5.
00061              20  RL-CCN-NO.
00062                  24  RL-CCN-PREFIX-A5 PIC X(4).
00063                  24  RL-CCN-PRIME-A5 PIC X(12).
00064              20  RL-CCN-FILLER-A5    PIC X(4).
00065
00066      12  RL-INSURED-PROFILE-DATA.
00067          16  RL-INSURED-BIRTH-DT     PIC XX.
00068          16  RL-INSURED-SEX-CD       PIC X.
00069              88  RL-INSURED-IS-MALE     VALUE 'M'.
00070              88  RL-INSURED-IS-FEMALE   VALUE 'F'.
00071              88  RL-INSURED-SEX-UNKNOWN VALUE ' '.
00072          16  RL-INSURED-OCC-CD       PIC X(6).
00073          16  FILLER                  PIC X(5).
00074
00075      12  RL-PROCESSING-INFO.
00076          16  RL-PROCESSOR-ID         PIC X(4).
00077          16  RL-CLAIM-STATUS         PIC X.
00078              88  RL-CLAIM-IS-OPEN       VALUE 'O'.
00079              88  RL-CLAIM-IS-CLOSED     VALUE 'C'.
00080          16  RL-CLAIM-TYPE           PIC X.
00081 *            88  RL-AH-CLAIM            VALUE 'A'.
00082 *            88  RL-LIFE-CLAIM          VALUE 'L'.
00083 *            88  RL-PROPERTY-CLAIM      VALUE 'P'.
00084 *            88  RL-UNEMPLOYMENT-CLAIM  VALUE 'U'.
00085          16  RL-CLAIM-PREM-TYPE      PIC X.
00086              88  RL-SINGLE-PREMIUM         VALUE '1'.
00087              88  RL-O-B-COVERAGE           VALUE '2'.
00088              88  RL-OPEN-END-COVERAGE      VALUE '3'.
00089          16  RL-INCURRED-DT          PIC XX.
00090          16  RL-REPORTED-DT          PIC XX.
00091          16  RL-FILE-ESTABLISH-DT    PIC XX.
00092          16  RL-EST-END-OF-DISAB-DT  PIC XX.
00093          16  RL-LAST-PMT-DT          PIC XX.
00094          16  RL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
00095          16  RL-PAID-THRU-DT         PIC XX.
00096          16  RL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
00097          16  RL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
00098          16  RL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
00099          16  RL-PMT-CALC-METHOD      PIC X.
00100              88  RL-360-DAY-YR          VALUE '1'.
00101              88  RL-365-DAY-YR          VALUE '2'.
00102              88  RL-FULL-MONTHS         VALUE '3'.
00103          16  RL-CAUSE-CD             PIC X(6).
00104
00105          16  RL-PRIME-CERT-NO.
00106              20  RL-PRIME-CERT-PRIME PIC X(10).
00107              20  RL-PRIME-CERT-SFX   PIC X.
00108
00109          16  RL-SYSTEM-IDENTIFIER    PIC XX.
00110              88  RL-CREDIT-CLAIM        VALUE 'CR'.
00111              88  RL-CONVENIENCE-CLAIM   VALUE 'CV'.
00112
00113          16  RL-MICROFILM-NO         PIC X(10).
00114          16  RL-PROG-FORM-TYPE       PIC X.
00115          16  RL-LAST-ADD-ON-DT       PIC XX.
00116
00117          16  RL-LAST-REOPEN-DT       PIC XX.
00118          16  RL-LAST-CLOSE-DT        PIC XX.
00119          16  RL-LAST-CLOSE-REASON    PIC X.
00120              88  RL-FINAL-PAID          VALUE '1'.
00121              88  RL-CLAIM-DENIED        VALUE '2'.
00122              88  RL-AUTO-CLOSE          VALUE '3'.
00123              88  RL-MANUAL-CLOSE        VALUE '4'.
00124          16  RL-ASSOC-CERT-SEQU      PIC S99.
00125          16  RL-ASSOC-CERT-TOTAL     PIC S99.
00126          16  RL-CLAIM-PAYMENT-STATUS PIC 9.
00127              88  RL-PAYMENT-IN-PREP     VALUE 1 THRU 9.
00128          16  FILLER                  PIC X(5).
00129
00130      12  RL-CERTIFICATE-DATA.
00131          16  RL-CERT-ORIGIN          PIC X.
00132              88  RL-CERT-WAS-ONLINE     VALUE '1'.
00133              88  RL-CERT-WAS-CREATED    VALUE '2'.
00134              88  RL-COVERAGE-WAS-ADDED  VALUE '3'.
00135          16  RL-CERT-KEY-DATA.
00136              20  RL-CERT-CARRIER     PIC X.
00137              20  RL-CERT-GROUPING    PIC X(6).
00138              20  RL-CERT-STATE       PIC XX.
00139              20  RL-CERT-ACCOUNT.
00140                  24  RL-CERT-ACCOUNT-PREFIX PIC X(4).
00141                  24  RL-CERT-ACCOUNT-PRIME  PIC X(6).
00142              20  RL-CERT-EFF-DT      PIC XX.
00143
00144      12  RL-STATUS-CONTROLS.
00145          16  RL-PRIORITY-CD          PIC X.
00146              88  RL-HIGHEST-PRIORITY    VALUE '9'.
00147          16  RL-SUPV-ATTN-CD         PIC X.
00148              88  RL-SUPV-NOT-REQUIRED   VALUE ' ' 'N'.
00149              88  RL-SUPV-IS-REQUIRED    VALUE 'Y'.
00150          16  RL-PURGED-DT            PIC XX.
00151          16  RL-RESTORED-DT          PIC XX.
00152          16  RL-NEXT-AUTO-PAY-DT     PIC XX.
00153          16  RL-NEXT-RESEND-DT       PIC XX.
00154          16  RL-NEXT-FOLLOWUP-DT     PIC XX.
00155          16  FILLER                  PIC XX.
00156          16  RL-LAST-MAINT-DT        PIC XX.
00157          16  RL-LAST-MAINT-USER      PIC X(4).
00158          16  RL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
00159          16  RL-LAST-MAINT-TYPE      PIC X.
00160              88  RL-CLAIM-SET-UP           VALUE ' '.
00161              88  RL-PAYMENT-MADE           VALUE '1'.
00162              88  RL-LETTER-SENT            VALUE '2'.
00163              88  RL-MASTER-WAS-ALTERED     VALUE '3'.
00164              88  RL-MASTER-WAS-RESTORED    VALUE '4'.
00165              88  RL-INCURRED-DATE-CHANGED  VALUE '5'.
00166              88  RL-FILE-CONVERTED         VALUE '6'.
00167          16  RL-RELATED-CLAIM-NO     PIC X(7).
00168          16  RL-HISTORY-ARCHIVE-DT   PIC XX.
00169          16  RL-BENEFICIARY          PIC X(10).
00170          16  RL-FILE-ESTABLISHED-BY  PIC X(4).
00171          16  FILLER                  PIC X(6).
00172
00173      12  RL-TRAILER-CONTROLS.
00174          16  RL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
00175              88  RL-1ST-TRL-AVAIL       VALUE +4095.
00176              88  RL-LAST-TRL-AVAIL      VALUE +100.
00177              88  RL-RESV-EXP-HIST-TRLR  VALUE +0.
00178          16  RL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
00179          16  FILLER                  PIC XX.
00180          16  RL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
00181          16  RL-ADDRESS-TRAILER-CNT.
00182              20  RL-INSURED-ADDR-CNT  PIC S9.
00183                  88  RL-NO-INSURED-AVAILABLE VALUE ZERO.
00184              20  RL-ACCOUNT-ADDR-CNT  PIC S9.
00185                  88  RL-ACCOUNT-IS-ONLINE    VALUE ZERO.
00186              20  RL-BENIF-ADDR-CNT    PIC S9.
00187                  88  RL-BENEFICIARY-IS-ONLINE VALUE ZERO.
00188              20  RL-EMPLOYER-ADDR-CNT PIC S9.
00189                  88  RL-NO-EMPLOY-AVAILABLE   VALUE ZERO.
00190              20  RL-DOCTOR-ADDR-CNT   PIC S9.
00191                  88  RL-NO-DOCTOR-AVAILABLE   VALUE ZERO.
00192              20  RL-OTHER-1-ADDR-CNT  PIC S9.
00193                  88  RL-NO-OTHER-1-ADDRESSES  VALUE ZERO.
00194              20  RL-OTHER-2-ADDR-CNT  PIC S9.
00195                  88  RL-NO-OTHER-2-ADDRESSES  VALUE ZERO.
00196
00197      12  RL-CV-REFERENCE-NO.
00198          16  RL-CV-REFNO-PRIME       PIC X(18).
00199          16  RL-CV-REFNO-SFX         PIC XX.
00200
00201      12  RL-FILE-LOCATION            PIC X(4).
00202
00203      12  RL-PROCESS-ERRORS.
00204          16  RL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
00205              88  RL-NO-FATAL-ERRORS     VALUE ZERO.
00206          16  RL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
00207              88  RL-NO-FORCABLE-ERRORS  VALUE ZERO.
00208
00209      12  RL-PRODUCT-CD               PIC X.
00210
00211      12  RL-CURRENT-KEY-DATA.
00212          16  RL-CURRENT-CARRIER      PIC X.
00213          16  RL-CURRENT-GROUPING     PIC X(6).
00214          16  RL-CURRENT-STATE        PIC XX.
00215          16  RL-CURRENT-ACCOUNT      PIC X(10).
00216
00217      12  RL-ASSOCIATES               PIC X.
00218          88  RL-ASSOC-NO-INTERFACE      VALUE 'A'.
00219          88  RL-ASSOC-INTERFACE         VALUE 'I'.
00220          88  RL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
00221          88  RL-NON-ASSOC-INTERFACE     VALUE 'M'.
00222
00223      12  RL-ACTIVITY-CODE            PIC 99.
00224      12  RL-ACTIVITY-MAINT-DT        PIC XX.
00225      12  RL-ACTIVITY-MAINT-TYPE      PIC X(4).
00226
00227      12  RL-LAPSE-REPORT-CODE        PIC 9.
00228      12  RL-LAG-REPORT-CODE          PIC 9.
00229      12  RL-LOAN-TYPE                PIC XX.
00230      12  RL-LEGAL-STATE              PIC XX.
00231
00232      12  FILLER                      PIC X(5).
00227
00228      EJECT
00229 *                                COPY ELCCNTL.
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
00230
00231      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CLAIM-MASTER
                                RETRIEVE-MASTER CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL132' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00233
00234      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00235      MOVE '5'                    TO DC-OPTION-CODE.
00236      PERFORM 8500-DATE-CONVERSION.
00237      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00238      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00239
00240      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00241
00242      MOVE +2                     TO  EMI-NUMBER-OF-LINES
00243                                      EMI-SWITCH2.
00244
00245 *    NOTE *******************************************************
00246 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
00247 *         *  FROM ANOTHER MODULE.                               *
00248 *         *******************************************************.
00249
00250      IF EIBCALEN NOT GREATER ZERO
00251          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00252          GO TO 8300-SEND-TEXT.
00253
00254      
      * EXEC CICS HANDLE CONDITION
00255 *        PGMIDERR (9600-PGMIDERR)
00256 *        ERROR    (9990-ERROR)
00257 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00005724' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303035373234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00258
00259      EJECT
00260  0010-MAIN-LOGIC.
00261 *    NOTE *******************************************************
00262 *         *      IF THE TRANSACTION CODE OF THE TASK THAT       *
00263 *         *  INVOKED THIS MODULE IS NOT EX21, THIS IS THE FIRST *
00264 *         *  TIME THROUGH THIS MODULE.                          *
00265 *         *******************************************************.
00266
00267      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00268          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00269              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00270              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00271              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00272              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00273              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00274              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00275              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00276              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
00277            ELSE
00278              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00279              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00280              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00281              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00282              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00283              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00284              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00285              MOVE SPACES               TO  PI-SAVED-PROGRAM-6
00286              PERFORM 7000-BUILD-SCREEN
00287              MOVE +1             TO  WS-CALL-SW
00288        ELSE
00289          GO TO 0020-MAIN-LOGIC.
00290
00291        IF PI-CALLING-PROGRAM   = 'EL132'   AND
00292           PI-RETURN-TO-PROGRAM = 'EL1275'  AND
00293           WS-CALL-SW IS EQUAL TO +1
00294            GO TO 9400-CLEAR.
00295
00296  0015-MAIN-LOGIC.
00297 *    NOTE *******************************************************
00298 *         *     INITIALIZE THE WORK FIELDS FOR THE PROGRAM      *
00299 *         *  INTERFACE BLOCK FOR THIS MODULE.                   *
00300 *         *******************************************************.
00301
00302      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.
00303
00304      MOVE ZERO                   TO  PI-1ST-TIME-SW
00305                                      PI-LINE-COUNT
00306                                      PI-AIX-RECORD-COUNT
00307                                      PI-BROWSE-SW
00308                                      PI-KEY-LENGTH
00309                                      PI-TS-ITEM
00310                                      PI-END-OF-FILE
00311                                      PI-START-SW.
00312
00313 *    NOTE *******************************************************
00314 *         *      IF CONTROL WAS PASSED FROM THE CERTIFICATE     *
00315 *         *  LOOK-UP (EL127) PASS CONTROL TO THE CLAIM LOOK-UP  *
00316 *         *  ACCESSING THE CLAIM MASTER BY CERTIFICATE NUMBER   *
00317 *         *******************************************************.
00318
00319      MOVE LOW-VALUES             TO  EL132AO.
00320
00321      IF (PI-RETURN-TO-PROGRAM = 'EL127' OR 'EL1275')
00322                   AND
00323         WS-CALL-SW = ZERO
00324          NEXT SENTENCE
00325      ELSE
00326          GO TO 0018-MAIN-LOGIC.
00327
00328      MOVE PI-COMPANY-CD      TO  PI-CK-COMPANY-CD.
00329      MOVE PI-CERT-NO         TO  PI-CK-CERT-NO-A4.
00330      MOVE PI-CLAIM-KEY       TO  PI-SELECTION-CRITERIA.
00331      MOVE +12                TO  PI-KEY-LENGTH.
00332      MOVE '4'                TO  PI-OPTION.
00333      MOVE ELMSTR5-FILE-ID    TO  PI-DSID.
00334      MOVE 'EL1322'           TO  THIS-PGM.
00335
00336      IF PI-RETURN-TO-PROGRAM = 'EL127'
00337          MOVE +12            TO  PI-KEY-LENGTH
00338      ELSE
00339          MOVE +11            TO  PI-KEY-LENGTH.
00340
00341      PERFORM 4000-READ-ELMSTR.
00342
00343      MOVE -1                 TO ACRTNOL.
00344      MOVE PI-CERT-PRIME      TO ACRTNOO.
00345      MOVE PI-CERT-SFX        TO ACRTSXO.
00346      MOVE AL-UABON           TO ACRTNOA
00347                                 ACRTSXA.
00348
00349      GO TO 8100-SEND-INITIAL-MAP.
00350
00351  0018-MAIN-LOGIC.
00352
00353      MOVE PI-COMPANY-ID          TO  WS-ELCNTL-ID.
00354      MOVE '1'                    TO  WS-ELCNTL-TYPE.
00355      MOVE SPACES                 TO  WS-ELCNTL-USER.
00356      MOVE +0                     TO  WS-ELCNTL-SEQ.
00357
00358      PERFORM 6000-READ-CONTROL THRU 6000-EXIT.
00359
00360      MOVE CF-CLAIMS-CREDIT-CARD-INDEX
00361                                  TO  PI-CREDIT-CARD-INDEX.
00362
00363      MOVE SPACES                 TO  PI-CONTROL-IN-PROGRESS.
00364
00365      GO TO 8100-SEND-INITIAL-MAP.
00366
00367      EJECT
00368  0020-MAIN-LOGIC.
00369      IF PI-1ST-TIME-SW NOT = ZERO
00370          GO TO 0015-MAIN-LOGIC.
00371
00372 *    NOTE *******************************************************
00373 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *
00374 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *
00375 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
00376 *         *******************************************************.
00377
00378      IF EIBAID = DFHCLEAR
00379          GO TO 9400-CLEAR.
00380
00381      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00382          MOVE LOW-VALUES         TO  EL132AO
00383          MOVE -1                 TO  APFKL
00384          MOVE ER-0008            TO  EMI-ERROR
00385          GO TO 8200-SEND-DATAONLY.
00386
00387      
      * EXEC CICS RECEIVE
00388 *        INTO   (EL132AO)
00389 *        MAPSET (WS-MAPSET-NAME)
00390 *        MAP    (WS-MAP-NAME)
00391 *    END-EXEC.
           MOVE LENGTH OF
            EL132AO
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00005857' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL132AO, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00392
00393      IF APFKL GREATER ZERO
00394          IF EIBAID NOT = DFHENTER
00395              MOVE ER-0004           TO  EMI-ERROR
00396              MOVE AL-UNBOF       TO  APFKA
00397              MOVE -1             TO  APFKL
00398              GO TO 8200-SEND-DATAONLY
00399            ELSE
00400              IF APFKO GREATER ZERO AND LESS '25'
00401                  MOVE PF-VALUES (APFKI)  TO  EIBAID
00402                ELSE
00403                  MOVE ER-0029           TO  EMI-ERROR
00404                  MOVE AL-UNBOF       TO  APFKA
00405                  MOVE -1             TO  APFKL
00406                  GO TO 8200-SEND-DATAONLY.
00407
00408      IF EIBAID = DFHPF12
00409          MOVE 'EL010'            TO  THIS-PGM
00410          GO TO 9300-XCTL.
00411
00412      IF EIBAID = DFHPF23
00413          GO TO 9000-RETURN-CICS.
00414
00415      IF EIBAID = DFHPF24
00416          MOVE 'EL126'            TO  THIS-PGM
00417          GO TO 9300-XCTL.
00418
00419      IF EIBAID = DFHPF1
00420          MOVE 'EL1324'           TO  THIS-PGM
00421          GO TO 9300-XCTL.
00422
00423      IF EIBAID = DFHPF2
00424          GO TO 0025-MAIN-LOGIC.
00425
00426      IF EIBAID NOT = DFHENTER AND DFHPF5 AND DFHPF6
00427          MOVE -1                 TO  APFKL
00428          MOVE ER-0008            TO  EMI-ERROR
00429          GO TO 8200-SEND-DATAONLY.
00430
00431      EJECT
00432  0025-MAIN-LOGIC.
00433      IF EIBAID = DFHPF5
00434         IF PI-ORIGINAL-COMPANY-ID NOT = SPACES
00435            PERFORM 5000-NEXT-COMPANY THRU 5000-EXIT
00436            PERFORM 5500-WRITE-SECURITY-TEMP-STORE THRU 5500-EXIT
00437         ELSE
00438            MOVE -1                 TO  APFKL
00439            MOVE ER-0008            TO  EMI-ERROR
00440            GO TO 8200-SEND-DATAONLY.
00441
00442      IF EIBAID = DFHPF6
00443         IF PI-ORIGINAL-COMPANY-ID NOT = SPACES
00444            PERFORM 5000-NEXT-COMPANY THRU 5000-EXIT
00445            PERFORM 5500-WRITE-SECURITY-TEMP-STORE THRU 5500-EXIT
00446            MOVE PI-ORIGINAL-COMPANY-CD TO PI-COMPANY-CD
00447            MOVE PI-ORIGINAL-COMPANY-ID TO PI-COMPANY-ID
00448         ELSE
00449            MOVE -1                 TO  APFKL
00450            MOVE ER-0008            TO  EMI-ERROR
00451            GO TO 8200-SEND-DATAONLY.
00452
00453      MOVE SPACES                 TO  PI-SELECTION-CRITERIA
00454                                      PI-CLAIM-KEY.
00455
00456      MOVE PI-COMPANY-CD          TO  PI-SC-COMPANY-CD
00457                                      PI-CK-COMPANY-CD.
00458
00459      MOVE 'EL1322'               TO  THIS-PGM.
00460
00461      IF PI-PROCESSOR-ID = 'LGXX'
00462          NEXT SENTENCE
00463      ELSE
00464          
      * EXEC CICS READQ TS
00465 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00466 *            INTO    (SECURITY-CONTROL)
00467 *            LENGTH  (SC-COMM-LENGTH)
00468 *            ITEM    (SC-ITEM)
00469 *        END-EXEC
      *    MOVE '*$II   L              ''   #00005934' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00470          MOVE SC-CLAIMS-DISPLAY (21)   TO  PI-DISPLAY-CAP
00471          MOVE SC-CLAIMS-UPDATE  (21)   TO  PI-MODIFY-CAP
00472          IF NOT DISPLAY-CAP
00473              MOVE 'READ'               TO  SM-READ
00474              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00475              MOVE ER-0070              TO  EMI-ERROR
00476              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00477              GO TO 8100-SEND-INITIAL-MAP.
00478
00479 ******************************************************************
00480 *           O P T I O N  1  P R O C E S S I N G                  *
00481 ******************************************************************
00482
00483      IF ACLAIML  GREATER ZERO  OR
00484         ACARIERL GREATER ZERO  OR
00485         ACERTNOL GREATER ZERO  OR
00486         ACERTSXL GREATER ZERO
00487          NEXT SENTENCE
00488        ELSE
00489          GO TO 0100-MAIN-LOGIC.
00490
00491      MOVE '1'                    TO  PI-OPTION.
00492
00493      IF ACLAIML GREATER THAN ZERO
00494          IF ACLAIMI  IS EQUAL TO  SPACES OR LOW-VALUES
00495              MOVE ER-0284            TO  EMI-ERROR
00496              MOVE -1                 TO  ALNAMEL
00497              GO TO 8200-SEND-DATAONLY.
00498
00499 ******************************************************************
00500 *              SECURITY CHECK FOR CARRIER                        *
00501 *                    04/02/84                                    *
00502 ******************************************************************
00503
00504      IF  PI-NO-CARRIER-SECURITY
00505          GO TO 0028-BUILD-CARRIER-NO.
00506
00507      IF ACARIERL GREATER ZERO
00508         IF  ACARIERI = PI-CARRIER-SECURITY
00509             GO TO 0028-BUILD-CARRIER-NO.
00510
00511      MOVE ER-2370                TO  EMI-ERROR.
00512      MOVE -1                     TO  ACARIERL.
00513      MOVE AL-UABON               TO  ACARIERA.
00514      GO TO 8200-SEND-DATAONLY.
00515
00516  0028-BUILD-CARRIER-NO.
00517      IF ACARIERL GREATER ZERO
00518          MOVE ACARIERI           TO  PI-SC-CARRIER
00519                                      PI-CK-CARRIER
00520                                      PI-CARRIER
00521          MOVE +2                 TO  PI-KEY-LENGTH
00522      ELSE
00523          MOVE ER-0194            TO  EMI-ERROR
00524          PERFORM 9900-ERROR-FORMAT
00525          MOVE -1                 TO  ACARIERL.
00526
00527      IF ACLAIML GREATER ZERO
00528          MOVE ACLAIMI           TO  PI-SC-CLAIM-NO
00529                                      PI-CK-CLAIM
00530                                      PI-CLAIM-NO
00531                                      WS-INPUT-FIELD
00532          PERFORM 0030-MAIN-LOGIC THRU 0030-MAIN-LOGIC-EXIT
00533              VARYING INPUT-INDEX FROM ACLAIML BY -1
00534                  UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE
00535          ADD +2  ACLAIML GIVING PI-KEY-LENGTH
00536        ELSE
00537          MOVE ER-0209           TO  EMI-ERROR
00538          PERFORM 9900-ERROR-FORMAT
00539          MOVE -1                TO  ACLAIML.
00540
00541      IF ACERTNOL GREATER ZERO
00542          MOVE ACERTNOI           TO  PI-SC-CERT-PRIME
00543                                      WS-INPUT-FIELD
00544          MOVE +19                TO  PI-KEY-LENGTH.
00545
00546      IF ACERTSXL GREATER ZERO
00547          MOVE +20                TO  PI-KEY-LENGTH
00548          MOVE ACERTSXI           TO  PI-SC-CERT-SFX.
00549
00550      MOVE PI-SC-CERT-NO          TO  PI-CK-CERT-NO
00551                                      PI-CERT-NO.
00552
00553      IF EMI-FATAL-CTR GREATER ZERO
00554          GO TO 8200-SEND-DATAONLY.
00555
00556      IF ACLAIML GREATER ZERO
00557          MOVE AL-UABON           TO  ACLAIMA
00558        ELSE
00559          MOVE AL-UABOF           TO  ACLAIMA.
00560
00561      IF ACARIERL GREATER ZERO
00562          MOVE AL-UABON           TO  ACARIERA
00563        ELSE
00564          MOVE AL-UABOF           TO  ACARIERA.
00565
00566      IF ACERTNOL GREATER ZERO
00567          MOVE AL-UABON           TO  ACERTNOA
00568        ELSE
00569          MOVE AL-UABOF           TO  ACERTNOA.
00570
00571      IF ACERTSXL GREATER ZERO
00572          MOVE AL-UABON           TO  ACERTSXA
00573        ELSE
00574          MOVE AL-UABOF           TO  ACERTSXA.
00575
00576      MOVE -1                     TO  ACLAIML.
00577
00578      IF EIBAID = DFHPF2
00579           MOVE ELRETR-FILE-ID    TO  PI-DSID
00580           PERFORM 3000-READ-ELRETR
00581        ELSE
00582           MOVE ELMSTR-FILE-ID    TO  PI-DSID
00583           PERFORM 4000-READ-ELMSTR.
00584
00585      MOVE +1                     TO  PI-1ST-TIME-SW.
00586
00587      IF PI-RETURN-TO-PROGRAM = 'EL126'
00588          MOVE 'EL150'            TO  THIS-PGM
00589        ELSE
00590          MOVE 'EL1323'           TO  THIS-PGM.
00591
00592      PERFORM 9300-XCTL.
00593
00594  0030-MAIN-LOGIC.
00595      SUBTRACT +1 FROM ACLAIML.
00596
00597  0030-MAIN-LOGIC-EXIT.
00598      EXIT.
00599
00600      EJECT
00601  0100-MAIN-LOGIC.
00602 ******************************************************************
00603 *           O P T I O N  2  P R O C E S S I N G                  *
00604 ******************************************************************
00605
00606      IF ALNAMEL GREATER ZERO  OR
00607         AFNAMEL GREATER ZERO  OR
00608         AMINITL GREATER ZERO  OR
00609         AACCTL  GREATER ZERO
00610          NEXT SENTENCE
00611        ELSE
00612          GO TO 0200-MAIN-LOGIC.
00613
00614      IF (AFNAMEL GREATER THAN +0 OR
00615          AMINITL GREATER THAN +0 OR
00616          AACCTL  GREATER THAN +0)
00617        AND ALNAMEL NOT GREATER ZERO
00618          MOVE ER-0488            TO  EMI-ERROR
00619          MOVE -1                 TO  ALNAMEL
00620          GO TO 8200-SEND-DATAONLY.
00621
00622      IF ALNAMEL GREATER THAN ZERO
00623          IF ALNAMEI  IS EQUAL TO  SPACES OR LOW-VALUES
00624              MOVE ER-0284            TO  EMI-ERROR
00625              MOVE -1                 TO  ALNAMEL
00626              GO TO 8200-SEND-DATAONLY.
00627
00628      MOVE '2'                    TO  PI-OPTION.
00629      MOVE PI-COMPANY-CD          TO  PI-SELECTION-CRITERIA
00630                                      PI-CK-COMPANY-CD.
00631
00632      MOVE +1                     TO  PI-KEY-LENGTH.
00633
00634      IF ALNAMEL GREATER ZERO
00635         MOVE ALNAMEI            TO  PI-CK-INSURED-LAST-NAME
00636                                     PI-SC-LAST-NAME
00637                                     WS-INPUT-FIELD
00638         IF AFNAMEL EQUAL +0
00639          AND
00640            AMINITL EQUAL +0
00641            PERFORM 0120-MAIN-LOGIC THRU 0120-MAIN-LOGIC-EXIT
00642              VARYING INPUT-INDEX FROM ALNAMEL BY -1
00643              UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE
00644            ADD ALNAMEL  TO  PI-KEY-LENGTH
00645         ELSE
00646            MOVE +16     TO  PI-KEY-LENGTH.
00647
00648      IF AFNAMEL GREATER ZERO
00649         MOVE AFNAMEI            TO  PI-SC-FIRST-NAME
00650         IF AMINITL GREATER ZERO
00651            ADD +13              TO  PI-KEY-LENGTH
00652            MOVE AMINITI         TO  PI-SC-INITIAL2
00653         ELSE
00654            MOVE AFNAMEI         TO  WS-INPUT-FIELD
00655            PERFORM 0125-MAIN-LOGIC THRU 0125-MAIN-LOGIC-EXIT
00656              VARYING INPUT-INDEX FROM AFNAMEL BY -1
00657              UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE
00658            ADD AFNAMEL           TO  PI-KEY-LENGTH.
00659
00660      IF AACCTL GREATER THAN +0
00661         MOVE AACCTI              TO  PI-ACCOUNT-NUMBER
00662      ELSE
00663         MOVE SPACES              TO  PI-ACCOUNT-NUMBER.
00664
00665      MOVE PI-SELECTION-CRITERIA  TO  PI-CLAIM-KEY.
00666
00667      IF ALNAMEL GREATER ZERO
00668          MOVE AL-UABON           TO  ALNAMEA
00669      ELSE
00670          MOVE AL-UABOF           TO  ALNAMEA.
00671
00672      IF AFNAMEL GREATER ZERO
00673          MOVE AL-UABON           TO  AFNAMEA
00674      ELSE
00675          MOVE AL-UABOF           TO  AFNAMEA.
00676
00677      IF AMINITL GREATER ZERO
00678          MOVE AL-UABON           TO  AMINITA
00679      ELSE
00680          MOVE AL-UABOF           TO  AMINITA.
00681
00682      IF AACCTL  GREATER ZERO
00683          MOVE AL-UABON           TO  AACCTA
00684      ELSE
00685          MOVE AL-UABOF           TO  AACCTA.
00686
00687      MOVE -1                     TO  ALNAMEL.
00688
00689      IF EIBAID = DFHPF2
00690           MOVE ELRETR2-FILE-ID   TO  PI-DSID
00691           PERFORM 3000-READ-ELRETR
00692        ELSE
00693           MOVE ELMSTR2-FILE-ID   TO  PI-DSID
00694           PERFORM 4000-READ-ELMSTR.
00695
00696  0120-MAIN-LOGIC.
00697      SUBTRACT +1 FROM ALNAMEL.
00698
00699  0120-MAIN-LOGIC-EXIT.
00700      EXIT.
00701
00702  0125-MAIN-LOGIC.
00703      SUBTRACT +1 FROM AFNAMEL.
00704
00705  0125-MAIN-LOGIC-EXIT.
00706      EXIT.
00707
00708      EJECT
00709  0200-MAIN-LOGIC.
00710 ******************************************************************
00711 *           O P T I O N  3  P R O C E S S I N G                  *
00712 ******************************************************************
00713
00714      IF ASSNL GREATER ZERO
00715          NEXT SENTENCE
00716        ELSE
00717          GO TO 0300-MAIN-LOGIC.
00718
00719      IF ASSNL GREATER THAN ZERO
00720          IF ASSNI    IS EQUAL TO  SPACES OR LOW-VALUES
00721              MOVE ER-0284            TO  EMI-ERROR
00722              MOVE -1                 TO  ASSNL
00723              GO TO 8200-SEND-DATAONLY.
00724
00725      MOVE '3'                    TO  PI-OPTION.
00726
00727      MOVE PI-COMPANY-CD          TO  PI-CK-COMPANY-CD
00728                                      PI-SC-COMPANY-CD.
00729      MOVE ASSNI                  TO  PI-CK-SOC-SEC-NO
00730                                      PI-SC-SOC-SEC-NO
00731                                      WS-INPUT-FIELD.
00732
00733      PERFORM 0220-MAIN-LOGIC THRU 0220-MAIN-LOGIC-EXIT
00734          VARYING INPUT-INDEX FROM ASSNL BY -1
00735              UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE
00736
00737      MOVE AL-UABON               TO  ASSNA.
00738
00739      ADD +1  ASSNL  GIVING  PI-KEY-LENGTH.
00740      MOVE -1                     TO  ASSNL.
00741
00742      IF EIBAID = DFHPF2
00743          MOVE ELRETR3-FILE-ID    TO PI-DSID
00744          PERFORM 3000-READ-ELRETR
00745      ELSE
00746          MOVE ELMSTR3-FILE-ID    TO PI-DSID
00747          PERFORM 4000-READ-ELMSTR.
00748
00749  0220-MAIN-LOGIC.
00750      SUBTRACT +1 FROM ASSNL.
00751
00752  0220-MAIN-LOGIC-EXIT.
00753      EXIT.
00754
00755      EJECT
00756  0300-MAIN-LOGIC.
00757 ******************************************************************
00758 *           O P T I O N  4  P R O C E S S I N G                  *
00759 ******************************************************************
00760
00761      IF ACRTNOL GREATER ZERO  OR
00762         ACRTSXL GREATER ZERO
00763          NEXT SENTENCE
00764        ELSE
00765          GO TO 0350-MAIN-LOGIC.
00766
00767      IF ACRTNOL GREATER THAN ZERO
00768          IF ACRTNOI  IS EQUAL TO  SPACES OR LOW-VALUES
00769              MOVE ER-0284            TO  EMI-ERROR
00770              MOVE -1                 TO  ACRTNOL
00771              GO TO 8200-SEND-DATAONLY.
00772
00773      MOVE '4'                    TO  PI-OPTION.
00774
00775      IF ACRTSXL GREATER ZERO
00776        AND ACRTNOL NOT GREATER ZERO
00777          MOVE ER-0210            TO  EMI-ERROR
00778          MOVE -1                 TO  ACLAIML
00779          GO TO 8200-SEND-DATAONLY.
00780
00781      IF ACRTNOL GREATER ZERO
00782          MOVE ACRTNOI            TO  PI-SC-CERT-PRIME-A4
00783                                      WS-INPUT-FIELD
00784          MOVE +11                TO PI-KEY-LENGTH.
00785
00786      IF ACRTSXL GREATER ZERO
00787          MOVE ACRTSXI            TO  PI-SC-CERT-SFX-A4
00788          MOVE +12                TO  PI-KEY-LENGTH.
00789
00790      MOVE PI-SC-CERT-NO-A4  TO  PI-CK-CERT-NO-A4.
00791
00792      IF ACRTNOL GREATER ZERO
00793          MOVE AL-UABON           TO  ACRTNOA
00794        ELSE
00795          MOVE AL-UABOF           TO  ACRTNOA.
00796
00797      IF ACRTSXL GREATER ZERO
00798          MOVE AL-UABON           TO  ACRTSXA
00799        ELSE
00800          MOVE AL-UABOF           TO  ACRTSXA.
00801
00802      MOVE -1                     TO  ACRTNOL.
00803
00804      IF EIBAID = DFHPF2
00805          MOVE ELRETR5-FILE-ID    TO PI-DSID
00806          PERFORM 3000-READ-ELRETR
00807      ELSE
00808          MOVE ELMSTR5-FILE-ID    TO PI-DSID
00809          PERFORM 4000-READ-ELMSTR.
00810
00811      EJECT
00812  0350-MAIN-LOGIC.
00813 ******************************************************************
00814 *           O P T I O N  5  P R O C E S S I N G                  *
00815 ******************************************************************
00816
00817      IF CREDIT-CARD-INDEX  AND
00818         ACCNNOL GREATER ZERO
00819          NEXT SENTENCE
00820        ELSE
00821          GO TO 0400-MAIN-LOGIC.
00822
00823      IF ACCNNOL GREATER ZERO
00824          IF ACCNNOI = SPACES OR LOW-VALUES
00825              MOVE ER-0284            TO  EMI-ERROR
00826              MOVE -1                 TO  ACCNNOL
00827              GO TO 8200-SEND-DATAONLY.
00828
00829      MOVE '5'                   TO  PI-OPTION.
00830
00831      MOVE ACCNNOI               TO  WS-INPUT-FIELD.
00832      MOVE ACCNNOL               TO  PI-KEY-LENGTH.
00833
00834      SET INPUT-INDEX TO +16.
00835      IF WS-INPUT-CHAR (INPUT-INDEX) = SPACE
00836          NEXT SENTENCE
00837       ELSE
00838          GO TO 0355-FULL-KEY.
00839
00840      PERFORM 0360-MAIN-LOGIC THRU 0360-MAIN-LOGIC-EXIT
00841          VARYING INPUT-INDEX FROM ACCNNOL BY -1
00842              UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE.
00843
00844      ADD +1  ACCNNOL  GIVING PI-KEY-LENGTH.
00845
00846  0355-FULL-KEY.
00847      MOVE AL-UABON               TO  ACCNNOA.
00848      MOVE -1                     TO  ACCNNOL.
00849
00850      IF WS-INPUT-CHAR (INPUT-INDEX) = '*'
00851          SUBTRACT 1 FROM PI-KEY-LENGTH
00852        ELSE
00853          MOVE +16               TO  PI-KEY-LENGTH.
00854
00855      MOVE ACCNNOI (1:PI-KEY-LENGTH) TO PI-SC-CCN-NO-A5
00856                                        PI-CK-CCN-NO-A5.
00857
00858      IF EIBAID = DFHPF2
00859          MOVE ELRETR6-FILE-ID   TO  PI-DSID
00860          PERFORM 3000-READ-ELRETR
00861        ELSE
00862          MOVE ELMSTR6-FILE-ID   TO  PI-DSID
00863          PERFORM 4000-READ-ELMSTR.
00864
00865  0360-MAIN-LOGIC.
00866      SUBTRACT +1 FROM ACCNNOL.
00867
00868  0360-MAIN-LOGIC-EXIT.
00869      EXIT.
00870
00871      EJECT
00872  0400-MAIN-LOGIC.
00873      MOVE ZERO                   TO PI-OPTION.
00874      MOVE +1                     TO  PI-KEY-LENGTH.
00875      MOVE -1                     TO  ACLAIML.
00876
00877      IF EIBAID = DFHPF2
00878          MOVE ELRETR-FILE-ID    TO  PI-DSID
00879          PERFORM 3000-READ-ELRETR
00880        ELSE
00881          MOVE ELMSTR-FILE-ID    TO  PI-DSID
00882          PERFORM 4000-READ-ELMSTR.
00883
00884      EJECT
00885  3000-READ-ELRETR SECTION.
00886
00887      
      * EXEC CICS HANDLE CONDITION
00888 *        DUPKEY (9300-XCTL)
00889 *        NOTFND (4080-NOTFND)
00890 *    END-EXEC.
      *    MOVE '"$$I                  ! # #00006357' TO DFHEIV0
           MOVE X'222424492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303036333537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00891
00892      IF (PI-DSID = ELRETR-FILE-ID AND
00893          PI-KEY-LENGTH LESS +19)
00894        OR
00895         (PI-DSID = ELRETR2-FILE-ID AND
00896          PI-KEY-LENGTH LESS +29)
00897        OR
00898         (PI-DSID = ELRETR3-FILE-ID AND
00899          PI-KEY-LENGTH LESS +12)
00900        OR
00901         (PI-DSID = ELRETR5-FILE-ID AND
00902          PI-KEY-LENGTH LESS +12)
00903        OR
00904         (PI-DSID = ELRETR6-FILE-ID AND
00905          PI-KEY-LENGTH LESS +16)
00906              MOVE +1             TO  PI-START-SW
00907              
      * EXEC CICS READ
00908 *                DATASET   (PI-DSID)
00909 *                RIDFLD    (PI-CLAIM-KEY)
00910 *                SET       (ADDRESS OF RETRIEVE-MASTER)
00911 *                GENERIC   EQUAL
00912 *                KEYLENGTH (PI-KEY-LENGTH)
00913 *            END-EXEC
      *    MOVE '&"S  KG    E          (   #00006377' TO DFHEIV0
           MOVE X'26225320204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CLAIM-KEY, 
                 PI-KEY-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF RETRIEVE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00914            ELSE
00915              MOVE ZERO           TO  PI-START-SW
00916              
      * EXEC CICS READ
00917 *                DATASET   (PI-DSID)
00918 *                RIDFLD    (PI-CLAIM-KEY)
00919 *                SET       (ADDRESS OF RETRIEVE-MASTER)
00920 *            END-EXEC.
      *    MOVE '&"S        E          (   #00006386' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF RETRIEVE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00921
00922      GO TO 9300-XCTL.
00923
00924      EJECT
00925  4000-READ-ELMSTR SECTION.
00926
00927      
      * EXEC CICS HANDLE CONDITION
00928 *        DUPKEY (9300-XCTL)
00929 *        NOTFND (4080-NOTFND)
00930 *    END-EXEC.
      *    MOVE '"$$I                  ! $ #00006397' TO DFHEIV0
           MOVE X'222424492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303036333937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00931
00932      IF (PI-DSID = ELMSTR-FILE-ID AND
00933          PI-KEY-LENGTH LESS +19)
00934        OR
00935         (PI-DSID = ELMSTR2-FILE-ID AND
00936          PI-KEY-LENGTH LESS +29)
00937        OR
00938         (PI-DSID = ELMSTR3-FILE-ID AND
00939          PI-KEY-LENGTH LESS +12)
00940        OR
00941         (PI-DSID = ELMSTR5-FILE-ID AND
00942          PI-KEY-LENGTH LESS +12)
00943        OR
00944         (PI-DSID = ELMSTR6-FILE-ID AND
00945          PI-KEY-LENGTH LESS +16)
00946              MOVE +1             TO  PI-START-SW
00947              
      * EXEC CICS READ
00948 *                DATASET   (PI-DSID)
00949 *                RIDFLD    (PI-CLAIM-KEY)
00950 *                SET       (ADDRESS OF CLAIM-MASTER)
00951 *                GENERIC   EQUAL
00952 *                KEYLENGTH (PI-KEY-LENGTH)
00953 *            END-EXEC
      *    MOVE '&"S  KG    E          (   #00006417' TO DFHEIV0
           MOVE X'26225320204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CLAIM-KEY, 
                 PI-KEY-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00954            ELSE
00955              MOVE ZERO           TO  PI-START-SW
00956              
      * EXEC CICS READ
00957 *                DATASET   (PI-DSID)
00958 *                RIDFLD    (PI-CLAIM-KEY)
00959 *                SET       (ADDRESS OF CLAIM-MASTER)
00960 *            END-EXEC.
      *    MOVE '&"S        E          (   #00006426' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00961
00962      IF PI-COMPANY-ID = 'DMD'  AND
00963         PI-START-SW = ZERO     AND
00964         CL-LAST-CLOSE-REASON = 'C' OR 'E'
00965             GO TO 4080-NOTFND.
00966
00967      GO TO 9300-XCTL.
00968
00969  4080-NOTFND.
00970      MOVE ER-0284                   TO  EMI-ERROR.
00971      GO TO 8200-SEND-DATAONLY.
00972
00973
00974      EJECT
00975  5000-NEXT-COMPANY SECTION.
00976 ******************************************************************
00977 ****      READ CURRENT COMPANY RECORD TO OBTAIN THE NEXT      ****
00978 ****      COMPANY ID.                                         ****
00979 ******************************************************************
00980
00981      MOVE PI-COMPANY-ID              TO  WS-ELCNTL-ID.
00982      MOVE '1'                        TO  WS-ELCNTL-TYPE.
00983      MOVE SPACES                     TO  WS-ELCNTL-USER.
00984      MOVE +0                         TO  WS-ELCNTL-SEQ.
00985
00986      PERFORM 6000-READ-CONTROL THRU 6000-EXIT.
00987
00988      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'
00989          MOVE ER-0022                TO  EMI-ERROR
00990          MOVE -1                     TO  APFKL
00991          GO TO 8200-SEND-DATAONLY.
00992
00993      IF EIBAID = DFHPF5
00994          MOVE CF-NEXT-COMPANY-ID     TO  WS-NEXT-COMPANY-ID.
00995
00996      IF EIBAID = DFHPF6
00997          MOVE PI-ORIGINAL-COMPANY-ID TO  WS-NEXT-COMPANY-ID.
00998
00999      IF PI-PROCESSOR-ID IS EQUAL TO 'LGXX'
01000          GO TO 5000-CONTINUE-NEXT-COMPANY.
01001
01002 ******************************************************************
01003 ****      READ THE CURRENT USER RECORD FOR UPDATE AND REMOVE  ****
01004 ****      THE TERMINAL ID FROM THE RECORD.                    ****
01005 ******************************************************************
01006
01007      MOVE PI-COMPANY-ID              TO  WS-ELCNTL-ID.
01008      MOVE '2'                        TO  WS-ELCNTL-TYPE.
01009      MOVE PI-PROCESSOR-ID            TO  WS-ELCNTL-USER.
01010      MOVE +0                         TO  WS-ELCNTL-SEQ.
01011
01012      PERFORM 6010-READ-CONTROL-UPDATE THRU 6010-EXIT.
01013
01014      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'
01015          MOVE ER-0019                TO  EMI-ERROR
01016          MOVE -1                     TO  APFKL
01017          GO TO 8200-SEND-DATAONLY.
01018
01019      MOVE SPACES                     TO  CF-CURRENT-TERM-ON.
01020
01021      PERFORM 6020-REWRITE-CONTROL THRU 6020-EXIT.
01022
01023 ******************************************************************
01024 ****      READ THE USER RECORD ON THE "NEXT" COMPANY TO       ****
01025 ****      VERIFY THAT A VALID RECORD EXISTS:                  ****
01026 ****        1.  MOVE USER CARRIER/ACCOUNT SECURITY TO PI-AREA ****
01027 ****        2.  MOVE USER SECURITY VALUES TO SECURITY CONTROL ****
01028 ****            IN WORKING STORAGE.                           ****
01029 ******************************************************************
01030
01031      MOVE WS-NEXT-COMPANY-ID         TO  WS-ELCNTL-ID.
01032      MOVE '2'                        TO  WS-ELCNTL-TYPE.
01033      MOVE PI-PROCESSOR-ID            TO  WS-ELCNTL-USER.
01034      MOVE +0                         TO  WS-ELCNTL-SEQ.
01035
01036      PERFORM 6000-READ-CONTROL THRU 6000-EXIT.
01037
01038      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'
01039          MOVE ER-0228                TO  EMI-ERROR
01040          MOVE -1                     TO  APFKL
01041          GO TO 8200-SEND-DATAONLY.
01042
01043      MOVE CF-PROCESSOR-CARRIER       TO  PI-CARRIER-SECURITY.
01044      MOVE CF-PROCESSOR-ACCOUNT       TO  PI-ACCOUNT-SECURITY.
01045      MOVE CF-INDIVIDUAL-APP (1)      TO  SC-CREDIT-CODES.
01046      MOVE CF-INDIVIDUAL-APP (2)      TO  SC-CLAIMS-CODES.
01047
01048 ******************************************************************
01049 ****      READ THE USER RECORD FOR UPDATE AND MOVE THE        ****
01050 ****      TERMINAL ID TO THE RECORD.                          ****
01051 ******************************************************************
01052
01053      MOVE WS-NEXT-COMPANY-ID         TO  WS-ELCNTL-ID.
01054      MOVE '2'                        TO  WS-ELCNTL-TYPE.
01055      MOVE PI-PROCESSOR-ID            TO  WS-ELCNTL-USER.
01056      MOVE +0                         TO  WS-ELCNTL-SEQ.
01057
01058      PERFORM 6010-READ-CONTROL-UPDATE THRU 6010-EXIT.
01059
01060      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'
01061          MOVE ER-0228                TO  EMI-ERROR
01062          MOVE -1                     TO  APFKL
01063          GO TO 8200-SEND-DATAONLY.
01064
01065      MOVE EIBTRMID                   TO  CF-CURRENT-TERM-ON.
01066
01067      PERFORM 6020-REWRITE-CONTROL THRU 6020-EXIT.
01068
01069  5000-CONTINUE-NEXT-COMPANY.
01070 ******************************************************************
01071 ****      READ THE NEW COMPANY RECORD TO VERIFY THAT IT       ****
01072 ****      EXISTS AND THEN MOVE SPECIFIC DATA TO THE PI-AREA.  ****
01073 ******************************************************************
01074
01075      MOVE WS-NEXT-COMPANY-ID         TO  WS-ELCNTL-ID.
01076      MOVE '1'                        TO  WS-ELCNTL-TYPE.
01077      MOVE SPACES                     TO  WS-ELCNTL-USER.
01078      MOVE +0                         TO  WS-ELCNTL-SEQ.
01079
01080      PERFORM 6000-READ-CONTROL THRU 6000-EXIT.
01081
01082      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'
01083          MOVE ER-0089                TO  EMI-ERROR
01084          MOVE -1                     TO  APFKL
01085          GO TO 8200-SEND-DATAONLY.
01086
01087      MOVE CF-COMPANY-CD            TO  PI-COMPANY-CD.
01088      MOVE CF-COMPANY-ID            TO  PI-COMPANY-ID.
01089      MOVE CF-COMPANY-PASSWORD      TO  PI-COMPANY-PASSWORD.
01090      MOVE CF-LGX-CREDIT-USER       TO  PI-CREDIT-USER.
01091      MOVE CF-LGX-CLAIM-USER        TO  PI-CLAIM-USER.
01092      MOVE CF-CERT-ACCESS-CONTROL   TO  PI-CERT-ACCESS-CONTROL.
01093      MOVE CF-CARRIER-CONTROL-LEVEL TO  PI-CARRIER-CONTROL-LEVEL.
01094      MOVE CF-JOURNAL-FILE-ID       TO  PI-JOURNAL-FILE-ID.
01095      MOVE CF-LOWER-CASE-LETTERS    TO  PI-LOWER-CASE-LETTERS.
01096      MOVE CF-CLAIM-PAID-THRU-TO    TO  PI-CLAIM-PAID-THRU-TO.
01097
01098      MOVE CF-LIFE-OVERRIDE-L1      TO  PI-LIFE-OVERRIDE-L1.
01099      MOVE CF-LIFE-OVERRIDE-L2      TO  PI-LIFE-OVERRIDE-L2.
01100      MOVE CF-LIFE-OVERRIDE-L6      TO  PI-LIFE-OVERRIDE-L6.
01101      MOVE CF-LIFE-OVERRIDE-L12     TO  PI-LIFE-OVERRIDE-L12.
01102
01103      MOVE CF-AH-OVERRIDE-L1        TO  PI-AH-OVERRIDE-L1.
01104      MOVE CF-AH-OVERRIDE-L2        TO  PI-AH-OVERRIDE-L2.
01105      MOVE CF-AH-OVERRIDE-L6        TO  PI-AH-OVERRIDE-L6.
01106      MOVE CF-AH-OVERRIDE-L12       TO  PI-AH-OVERRIDE-L12.
01107
01108      IF  CLAIM-SESSION
01109          MOVE CF-PRINT-ADDRESS-LABELS
01110                                    TO  PI-LABEL-CONTROL
01111      ELSE
01112          IF  CREDIT-SESSION
01113              MOVE CF-CR-PRINT-ADDRESS-LABELS
01114                                    TO  PI-LABEL-CONTROL.
01115
01116  5000-EXIT.
01117      EXIT.
01118
01119      EJECT
01120  5500-WRITE-SECURITY-TEMP-STORE  SECTION.
01121
01122      
      * EXEC CICS HANDLE CONDITION
01123 *        QIDERR   (5501-WRITE-SECURITY)
01124 *    END-EXEC.
      *    MOVE '"$N                   ! % #00006592' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303036353932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01125
01126      MOVE EIBTRMID                 TO  QID.
01127
01128      
      * EXEC CICS DELETEQ TS
01129 *        QUEUE   (QID)
01130 *    END-EXEC.
      *    MOVE '*&                    #   #00006598' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01131
01132  5501-WRITE-SECURITY.
01133
01134      
      * EXEC CICS WRITEQ TS
01135 *        QUEUE   (QID)
01136 *        FROM    (SECURITY-CONTROL)
01137 *        LENGTH  (SC-COMM-LENGTH)
01138 *        ITEM    (QID-ITEM)
01139 *    END-EXEC.
      *    MOVE '*" I                  ''   #00006604' TO DFHEIV0
           MOVE X'2A2220492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 QID-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01140
01141      MOVE QID                      TO  PI-SECURITY-TEMP-STORE-ID.
01142
01143      IF PI-PROCESSOR-ID IS EQUAL TO 'LGXX'
01144          MOVE ALL 'Y'              TO  SC-CREDIT-CODES
01145                                        SC-CLAIMS-CODES
01146                                        PI-PROCESSOR-USER-ALMIGHTY.
01147
01148  5500-EXIT.
01149      EXIT.
01150
01151      EJECT
01152  6000-READ-CONTROL SECTION.
01153      
      * EXEC CICS HANDLE CONDITION
01154 *         NOTFND (6000-NOT-FOUND)
01155 *    END-EXEC.
      *    MOVE '"$I                   ! & #00006623' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303036363233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01156
01157      
      * EXEC CICS READ
01158 *         DATASET  (ELCNTL-FILE-ID)
01159 *         RIDFLD   (WS-ELCNTL-KEY)
01160 *         SET      (ADDRESS OF CONTROL-FILE)
01161 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006627' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01162
01163      MOVE 'Y'                        TO  WS-CNTL-REC-FOUND-SW.
01164      GO TO 6000-EXIT.
01165
01166  6000-NOT-FOUND.
01167
01168      MOVE 'N'                        TO  WS-CNTL-REC-FOUND-SW.
01169
01170  6000-EXIT.
01171      EXIT.
01172
01173  6010-READ-CONTROL-UPDATE.
01174
01175      
      * EXEC CICS HANDLE CONDITION
01176 *        NOTFND   (6010-NOTFND)
01177 *    END-EXEC.
      *    MOVE '"$I                   ! '' #00006645' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303036363435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01178
01179      
      * EXEC CICS READ
01180 *        DATASET   (ELCNTL-FILE-ID)
01181 *        RIDFLD    (WS-ELCNTL-KEY)
01182 *        SET       (ADDRESS OF CONTROL-FILE)
01183 *        UPDATE
01184 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006649' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01185
01186      MOVE 'Y'                        TO  WS-CNTL-REC-FOUND-SW.
01187      GO TO 6010-EXIT.
01188
01189  6010-NOTFND.
01190
01191      MOVE 'N'                        TO  WS-CNTL-REC-FOUND-SW.
01192
01193  6010-EXIT.
01194      EXIT.
01195
01196  6020-REWRITE-CONTROL.
01197
01198      
      * EXEC CICS REWRITE
01199 *        DATASET   (ELCNTL-FILE-ID)
01200 *        FROM      (CONTROL-FILE)
01201 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006668' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01202
01203  6020-EXIT.
01204      EXIT.
01205
01206      EJECT
01207  7000-BUILD-SCREEN     SECTION.
01208 ******************************************************************
01209 *          REBUILD ORIGNAL SCREEN AND ERROR MESSAGE IF EL1322    *
01210 *          DID NOT FIND ANY CLAIM RECORDS DURING BROWSE OF FILE. *
01211 ******************************************************************
01212
01213      IF EIBTRNID = WS-TRANS-ID
01214          NEXT SENTENCE
01215        ELSE
01216          GO TO 7099-EXIT.
01217
01218      IF  PI-BROWSE-SW = +9
01219          NEXT SENTENCE
01220         ELSE
01221          GO TO 7099-EXIT.
01222
01223      MOVE LOW-VALUES             TO  EL132AO.
01224
01225      MOVE ER-2374                TO  EMI-ERROR.
01226
01227      IF OPTION-ONE-SELECTED
01228         GO TO 7099-EXIT.
01229
01230      IF OPTION-TWO-SELECTED
01231         NEXT SENTENCE
01232        ELSE
01233         GO TO 7030-OPTION-THREE.
01234
01235  7020-OPTION-TWO.
01236
01237      IF  PI-SC-LAST-NAME GREATER SPACES
01238          MOVE PI-SC-LAST-NAME TO ALNAMEO
01239          MOVE AL-UANON        TO ALNAMEA
01240          IF  PI-SC-INITIALS  GREATER SPACES
01241              MOVE PI-SC-FIRST-NAME TO AFNAMEO
01242              MOVE PI-SC-INITIAL2  TO AMINITO
01243              MOVE AL-UANON        TO AFNAMEA
01244              MOVE AL-UANON        TO AMINITA.
01245
01246      IF PI-ACCOUNT-NUMBER NOT EQUAL SPACES
01247         MOVE PI-ACCOUNT-NUMBER    TO AACCTO
01248         MOVE AL-UANON             TO AACCTA.
01249
01250      GO TO   7090-INITIALIZE-WORK-AREAS.
01251
01252  7030-OPTION-THREE.
01253      IF OPTION-THREE-SELECTED
01254         NEXT SENTENCE
01255        ELSE
01256         GO TO 7040-OPTION-FOUR.
01257
01258      MOVE -1                          TO ASSNL.
01259      IF PI-SC-SOC-SEC-NO GREATER SPACES
01260         MOVE PI-SC-SOC-SEC-NO         TO ASSNO
01261         MOVE AL-UANON                 TO ASSNA.
01262
01263      GO TO   7090-INITIALIZE-WORK-AREAS.
01264
01265  7040-OPTION-FOUR.
01266      IF OPTION-FOUR-SELECTED
01267         NEXT SENTENCE
01268        ELSE
01269         GO TO 7050-OPTION-FIVE.
01270
01271       MOVE -1                    TO ACRTNOL.
01272
01273       IF  PI-SC-CERT-PRIME-A4 GREATER SPACES
01274           MOVE PI-SC-CERT-PRIME-A4  TO  ACRTNOO
01275           MOVE AL-UANON             TO  ACRTNOA.
01276
01277       IF  PI-SC-CERT-SFX-A4   GREATER SPACES
01278           MOVE PI-SC-CERT-SFX-A4    TO  ACRTSXO
01279           MOVE AL-UANON             TO  ACRTSXA.
01280
01281      GO TO 7090-INITIALIZE-WORK-AREAS.
01282
01283  7050-OPTION-FIVE.
01284       MOVE -1                    TO ACCNNOL.
01285
01286       IF PI-SC-CCN-NO-A5 GREATER SPACES
01287           MOVE PI-SC-CCN-NO-A5   TO  ACCNNOO
01288           MOVE AL-UANON          TO  ACCNNOA.
01289
01290  7090-INITIALIZE-WORK-AREAS.
01291      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.
01292
01293      MOVE ZERO                   TO  PI-1ST-TIME-SW
01294                                      PI-LINE-COUNT
01295                                      PI-AIX-RECORD-COUNT
01296                                      PI-BROWSE-SW
01297                                      PI-KEY-LENGTH
01298                                      PI-TS-ITEM
01299                                      PI-END-OF-FILE
01300                                      PI-START-SW.
01301
01302      GO TO 8100-SEND-INITIAL-MAP.
01303
01304  7099-EXIT.
01305      EXIT.
01306
01307      EJECT
01308  8100-SEND-INITIAL-MAP SECTION.
01309
01310      IF CREDIT-CARD-INDEX
01311          MOVE AL-SANOF           TO  ACCNH1A
01312                                      ACCNH2A
01313          MOVE AL-UANOF           TO  ACCNNOA
01314       ELSE
01315          MOVE AL-SADOF           TO  ACCNH1A
01316                                      ACCNH2A
01317          MOVE AL-SADOF           TO  ACCNNOA.
01318
01319      IF SOC-SEC-NO-USED
01320         MOVE AL-SANOF TO AOPT3A ASSOPTA
01321         MOVE AL-UANOF TO ASSNA
01322      ELSE
01323         MOVE AL-SANOF TO AOPT3A ASSOPTA ASSNA.
01324
01325
01326      IF ACRTNOL NOT = -1
01327          MOVE -1                 TO  ACLAIML.
01328
01329      MOVE SAVE-DATE              TO  ADATEO.
01330      MOVE EIBTIME                TO  TIME-IN.
01331      MOVE TIME-OUT               TO  ATIMEO.
101501     MOVE PI-COMPANY-ID          TO  ACOMPO.
101501     MOVE PI-PROCESSOR-ID        TO  AUSERIDO.
01332
01333      IF PI-RETURN-TO-PROGRAM = 'EL126' OR 'EL150'
01334          MOVE '       CLAIM LOOK-UP FOR STATUS  '
01335                                  TO  AHEAD1O
01336        ELSE
01337          IF PI-RETURN-TO-PROGRAM = 'EL162'
01338              MOVE '   CLAIM LOOK-UP FOR MAIL RECORDING   '
01339                                  TO  AHEAD1O
01340            ELSE
01341              IF PI-RETURN-TO-PROGRAM = 'EL130'
01342                  MOVE '  CLAIM LOOK-UP FROM NEW CLAIM SETUP  '
01343                                  TO  AHEAD1O.
01344
01345      IF EMI-ERROR NOT = ZERO
01346          PERFORM 9900-ERROR-FORMAT.
01347
01348      MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.
01349      MOVE EMI-MESSAGE-AREA (2)    TO  AEMSG2O.
01350
101501*    IF PI-ORIGINAL-COMPANY-ID NOT = SPACES
101501*       MOVE PI-COMPANY-ID TO ACOMPO
101501*       MOVE AL-PANOF      TO APFK5A
101501*       MOVE AL-PANOF      TO APFK6A
101501*    ELSE
101501*       MOVE SPACES        TO ACOMPO
101501*       MOVE AL-PADOF      TO APFK5A
101501*       MOVE AL-PADOF      TO APFK6A.
01359
01360      
      * EXEC CICS SEND
01361 *        FROM   (EL132AO)
01362 *        MAPSET (WS-MAPSET-NAME)
01363 *        MAP    (WS-MAP-NAME)
01364 *        CURSOR ERASE
01365 *    END-EXEC.
           MOVE LENGTH OF
            EL132AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00006832' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL132AO, 
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
           
01366
01367      GO TO 9100-RETURN-TRAN.
01368
01369  8100-EXIT.
01370      EXIT.
01371
01372      EJECT
01373  8200-SEND-DATAONLY SECTION.
01374      MOVE SAVE-DATE              TO  ADATEO.
01375      MOVE EIBTIME                TO  TIME-IN.
01376      MOVE TIME-OUT               TO  ATIMEO.
101501     MOVE PI-COMPANY-ID          TO  ACOMPO.
101501     MOVE PI-PROCESSOR-ID        TO  AUSERIDO.
01377
01378      IF EMI-ERROR NOT = ZERO
01379          PERFORM 9900-ERROR-FORMAT.
01380
01381      MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.
01382      MOVE EMI-MESSAGE-AREA (2)    TO  AEMSG2O.
01383
101501*    IF PI-ORIGINAL-COMPANY-ID NOT = SPACES
101501*       MOVE PI-COMPANY-ID TO ACOMPO
101501*       MOVE AL-PANOF      TO APFK5A
101501*                             APFK6A
101501*    ELSE
101501*       MOVE SPACES        TO ACOMPO
101501*       MOVE AL-PADOF      TO APFK5A
101501*                             APFK6A.
01392
01393      
      * EXEC CICS SEND DATAONLY
01394 *        FROM   (EL132AO)
01395 *        MAPSET (WS-MAPSET-NAME)
01396 *        MAP    (WS-MAP-NAME)
01397 *        CURSOR
01398 *    END-EXEC.
           MOVE LENGTH OF
            EL132AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00006867' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL132AO, 
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
           
01399
01400      GO TO 9100-RETURN-TRAN.
01401
01402  8200-EXIT.
01403      EXIT.
01404
01405      EJECT
01406  8300-SEND-TEXT SECTION.
01407      
      * EXEC CICS SEND TEXT
01408 *        FROM   (LOGOFF-TEXT)
01409 *        LENGTH (LOGOFF-LENGTH)
01410 *        ERASE  FREEKB
01411 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00006881' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383831' TO DFHEIV0(25:11)
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
           
01412
01413      
      * EXEC CICS RETURN
01414 *    END-EXEC.
      *    MOVE '.(                    &   #00006887' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01415
01416  8300-EXIT.
01417      EXIT.
01418
01419  8500-DATE-CONVERSION SECTION.
01420      
      * EXEC CICS LINK
01421 *        PROGRAM  ('ELDATCV')
01422 *        COMMAREA (DATE-CONVERSION-DATA)
01423 *        LENGTH   (DC-COMM-LENGTH)
01424 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00006894' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01425
01426  8500-EXIT.
01427      EXIT.
01428
01429      EJECT
01430  9000-RETURN-CICS SECTION.
01431      MOVE 'EL005'                TO  THIS-PGM.
01432      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
01433      PERFORM 9300-XCTL.
01434
01435  9000-EXIT.
01436      EXIT.
01437
01438  9100-RETURN-TRAN SECTION.
01439      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
01440      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
01441
01442      
      * EXEC CICS RETURN
01443 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01444 *        LENGTH   (PI-COMM-LENGTH)
01445 *        TRANSID  (WS-TRANS-ID)
01446 *    END-EXEC.
      *    MOVE '.(CT                  &   #00006916' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01447
01448  9100-EXIT.
01449      EXIT.
01450
01451  9300-XCTL SECTION.
01452      MOVE DFHENTER               TO  EIBAID.
01453
01454      
      * EXEC CICS XCTL
01455 *        PROGRAM  (THIS-PGM)
01456 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01457 *        LENGTH   (PI-COMM-LENGTH)
01458 *    END-EXEC.
      *    MOVE '.$C                   $   #00006928' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01459
01460  9300-EXIT.
01461      EXIT.
01462
01463      EJECT
01464  9400-CLEAR SECTION.
01465      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM
01466      PERFORM 9300-XCTL.
01467
01468  9400-EXIT.
01469      EXIT.
01470
01471  9600-PGMIDERR SECTION.
01472      
      * EXEC CICS HANDLE CONDITION
01473 *        PGMIDERR (8300-SEND-TEXT)
01474 *    END-EXEC.
      *    MOVE '"$L                   ! ( #00006946' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303036393436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01475
01476      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM.
01477
01478      MOVE 'EL005'                TO  THIS-PGM
01479                                      LOGOFF-PGM.
01480      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
01481      MOVE SPACES                 TO  PI-ENTRY-CD-1.
01482      PERFORM 9300-XCTL.
01483
01484  9600-EXIT.
01485      EXIT.
01486
01487      EJECT
01488  9900-ERROR-FORMAT SECTION.
01489      
      * EXEC CICS LINK
01490 *        PROGRAM  ('EL001')
01491 *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
01492 *        LENGTH   (EMI-COMM-LENGTH)
01493 *    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   ''   #00006963' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01494
01495      MOVE ER-0000                   TO  EMI-ERROR.
01496
01497  9900-EXIT.
01498      EXIT.
01499
01500      EJECT
01501  9990-ERROR SECTION.
01502      MOVE DFHEIBLK TO EMI-LINE1.
01503      
      * EXEC CICS LINK
01504 *        PROGRAM  ('EL004')
01505 *        COMMAREA (EMI-LINE1)
01506 *        LENGTH   (72)
01507 *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00006977' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01508
01509      GO TO 8200-SEND-DATAONLY.
01510
01511  9990-EXIT.
01512      EXIT.
01513
01514  9995-SECURITY-VIOLATION.
01515 *           COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00007006' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303036' TO DFHEIV0(25:11)
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
01516
01517  9995-EXIT.
01518       EXIT.
01519


       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL132' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMIDERR,
                     9990-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 9300-XCTL,
                     4080-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 9300-XCTL,
                     4080-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 5501-WRITE-SECURITY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 6000-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 6010-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL132' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
