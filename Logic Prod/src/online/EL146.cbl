00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL146 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 09/27/95 14:15:18.
00007 *                            VMOD=2.003
00008 *
00008 *
00009 *AUTHOR.    LOGIC, INC.
00010 *           DALLAS, TEXAS.
00025 *REMARKS.
00026 *        THIS PROGRAM CONTROLS THE MAINTENANCE TO THE CHECK RECON-
00027 *    CILIATION RECORD.
00028 *
00029 *    SCREENS     - EL146S - AUTOMATIC ACTIVITY MAINTENANCE
00030 *    ENTERED BY  - EL171A - ON-LINE REPORTS MENU
00031 *
00032 *    EXIT TO     - EL171A - RESULT OF CLEAR
00033 *
00034 *    INPUT FILES - ELRCON - CHECK RECONCILIATION FILE
00035 *
00036 *    OUTPUT FILES - ELRCON
00037 *
00038 *    COMMAREA    - PASSED.
00039
00040      EJECT
00041  ENVIRONMENT DIVISION.
00042
00043  DATA DIVISION.
00044
00045  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00046
00047  77  FILLER  PIC X(32)  VALUE '********************************'.
00048  77  FILLER  PIC X(32)  VALUE '*   EL146  WORKING STORAGE     *'.
00049  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.003 *********'.
00050
00051 *                                COPY ELCSCTM.
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
00052
00053 *                                COPY ELCSCRTY.
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
00054
00055  01  WS-DATE-AREA.
00056      12  SAVE-DATE               PIC X(08)       VALUE SPACES.
00057      12  SAVE-BIN-DATE           PIC X(02)       VALUE SPACES.
00058
00059  01  FILLER                      COMP-3.
00060
00061      12  TIME-IN                 PIC S9(7)       VALUE ZERO.
00062      12  TIME-OUT                REDEFINES
00063          TIME-IN                 PIC S9(3)V9(4).
00064
00065  01  FILLER.
00066      12  GETMAIN-SPACE           PIC X(01)       VALUE SPACES.
00067      12  ELRCON-DSID             PIC X(08)       VALUE 'ELRCON'.
00068
00069      12  LINK-ELDATCV            PIC X(08)       VALUE 'ELDATCV'.
00070      12  LINK-001                PIC X(08)       VALUE 'EL001'.
00071      12  LINK-004                PIC X(08)       VALUE 'EL004'.
00072
00073      12  XCTL-005                PIC X(08)       VALUE 'EL005'.
00074      12  XCTL-010                PIC X(08)       VALUE 'EL010'.
00075      12  XCTL-126                PIC X(08)       VALUE 'EL126'.
00076
00077      12  WS-MAPSET-NAME          PIC X(08)       VALUE 'EL146S'.
00078      12  WS-MAP-NAME             PIC X(08)       VALUE 'EL146A'.
00079      12  FILLER                  REDEFINES
00080          WS-MAP-NAME.
00081          16  FILLER              PIC X(02).
00082          16  WS-MAP-NUMBER       PIC X(04).
00083          16  FILLER              PIC X(02).
00084
00085      12  WS-DATE-YY              PIC 9(04).
00086
00087      12  WS-EDIT-DATE.
00088          16  WS-EDIT-MM          PIC 99.
00089          16  FILLER              PIC X(01)  VALUE '/'.
00090          16  WS-EDIT-DD          PIC 99.
00091          16  FILLER              PIC X(01)  VALUE '/'.
00092          16  WS-EDIT-YY          PIC 99.
00093
00094      12  THIS-PGM                PIC X(08)       VALUE 'EL146'.
00095      12  WS-TRANS-ID             PIC X(04)       VALUE 'E027'.
00096      12  SC-ITEM                 PIC S9(04)      VALUE +1 COMP.
00097      12  SUB                     PIC S9(02)      VALUE +0.
00098      12  SUB1                    PIC S9(02)      VALUE +0.
00099      12  WS-EDIT-CHECK-AMT       PIC 9(07)V99.
00100      12  WS-RECORDS-READ-SW      PIC X(01)       VALUE 'N'.
00101
00102      12  DEEDIT-FIELD            PIC X(12).
00103      12  DEEDIT-FIELD-CHK REDEFINES DEEDIT-FIELD  PIC 9(10)V99.
00104
00105      12  WS-WORK-DATE.
00106          16  WS-WORK-MM          PIC 9(02)       VALUE ZEROS.
00107          16  WS-WORK-DD          PIC 9(02)       VALUE ZEROS.
00108          16  WS-WORK-YY          PIC 9(02)       VALUE ZEROS.
00109
00110      12  WS-RCON-DATE.
00111          16  WS-RCON-YEAR.
00112              20  WS-RCON-YY-1    PIC 9(02).
00113              20  WS-RCON-YY-2    PIC 9(02).
00114          16  WS-RCON-MM          PIC 9(02).
00115          16  WS-RCON-DD          PIC 9(02).
00116      EJECT
00117  01  ACCESS-KEYS.
00118
00119      12  ELRCON-KEY.
00120          16  RECON-COMPANY-CD    PIC X(01).
00121          16  RECON-CHECK-NO      PIC X(07).
00122          16  RECON-ORIGIN        PIC X(01).
00123          16  RECON-BANK-NO       PIC X(10).
00124
00125      EJECT
00126  01  ERROR-MESSAGES.
00127      12  ER-0000                 PIC X(04)       VALUE '0000'.
00128      12  ER-0004                 PIC X(04)       VALUE '0004'.
00129      12  ER-0008                 PIC X(04)       VALUE '0008'.
00130      12  ER-0023                 PIC X(04)       VALUE '0023'.
00131      12  ER-0029                 PIC X(04)       VALUE '0029'.
00132      12  ER-0070                 PIC X(04)       VALUE '0070'.
00133      12  ER-0130                 PIC X(04)       VALUE '0130'.
00134      12  ER-0131                 PIC X(04)       VALUE '0131'.
00135      12  ER-0772                 PIC X(04)       VALUE '0772'.
00136      12  ER-0773                 PIC X(04)       VALUE '0773'.
00137      12  ER-0774                 PIC X(04)       VALUE '0774'.
00138      12  ER-0775                 PIC X(04)       VALUE '0775'.
00139      12  ER-0776                 PIC X(04)       VALUE '0776'.
00140      12  ER-0789                 PIC X(04)       VALUE '0789'.
00141      12  ER-0795                 PIC X(04)       VALUE '0795'.
00142      12  ER-0796                 PIC X(04)       VALUE '0796'.
00143      12  ER-0827                 PIC X(04)       VALUE '0827'.
00144      12  ER-0831                 PIC X(04)       VALUE '0831'.
00145      12  ER-0850                 PIC X(04)       VALUE '0850'.
00146      12  ER-0898                 PIC X(04)       VALUE '0898'.
00147      12  ER-9999                 PIC X(04)       VALUE '9999'.
00148
00149      EJECT
00150 *                                COPY ELCINTF.
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
00151      12  PI-REDEF    REDEFINES PI-PROGRAM-WORK-AREA.
00152          16  PI-PREV-KEY.
00153              20  PI-PREV-COMPANY-CD   PIC X(01).
00154              20  PI-PREV-CHECK-NO     PIC X(07).
00155              20  PI-PREV-ORGIN        PIC X(01).
00156              20  PI-PREV-BANK-NO      PIC X(10).
00157          16  PI-SAVE-KEY.
00158              20  PI-SAVE-KEY-OCCURS   OCCURS  08 TIMES.
00159                  24  PI-SAVE-COMPANY-CD   PIC X(01).
00160                  24  PI-SAVE-CHECK-NO     PIC X(07).
00161                  24  PI-SAVE-ORIGIN       PIC X(01).
00162                  24  PI-SAVE-BANK-NO      PIC X(10).
00163          16  PI-FIRST-TIME-SW             PIC X(01).
00164              88  FIRST-TIME                   VALUE 'Y'.
00165          16  PI-STOP-PAY-SW               PIC X(01).
00166              88  PI-STOP-PAY                  VALUE 'Y'.
00167          16  PI-STOP-PAY-KEY.
00168              20  PI-SPAY-COMPANY-CD   PIC X(01).
00169              20  PI-SPAY-CHECK-NO     PIC X(07).
00170              20  PI-SPAY-ORGIN        PIC X(01).
00171              20  PI-SPAY-BANK-NO      PIC X(10).
00172          16  FILLER                   PIC X(448).
00173      EJECT
00174 *                                COPY EL146S.
       01  EL146AI.
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
           05  CHKNOL PIC S9(0004) COMP.
           05  CHKNOF PIC  X(0001).
           05  FILLER REDEFINES CHKNOF.
               10  CHKNOA PIC  X(0001).
           05  CHKNOI PIC  X(0007).
      *    -------------------------------
           05  BANKNOL PIC S9(0004) COMP.
           05  BANKNOF PIC  X(0001).
           05  FILLER REDEFINES BANKNOF.
               10  BANKNOA PIC  X(0001).
           05  BANKNOI PIC  X(0010).
      *    -------------------------------
           05  CSHAMTL PIC S9(0004) COMP.
           05  CSHAMTF PIC  X(0001).
           05  FILLER REDEFINES CSHAMTF.
               10  CSHAMTA PIC  X(0001).
           05  CSHAMTI PIC  X(0012).
      *    -------------------------------
           05  NOTEL PIC S9(0004) COMP.
           05  NOTEF PIC  X(0001).
           05  FILLER REDEFINES NOTEF.
               10  NOTEA PIC  X(0001).
           05  NOTEI PIC  X(67).
      *    -------------------------------
           05  CHKNO1L PIC S9(0004) COMP.
           05  CHKNO1F PIC  X(0001).
           05  FILLER REDEFINES CHKNO1F.
               10  CHKNO1A PIC  X(0001).
           05  CHKNO1I PIC  X(0007).
      *    -------------------------------
           05  CHKDT1L PIC S9(0004) COMP.
           05  CHKDT1F PIC  X(0001).
           05  FILLER REDEFINES CHKDT1F.
               10  CHKDT1A PIC  X(0001).
           05  CHKDT1I PIC  X(0008).
      *    -------------------------------
           05  STATUS1L PIC S9(0004) COMP.
           05  STATUS1F PIC  X(0001).
           05  FILLER REDEFINES STATUS1F.
               10  STATUS1A PIC  X(0001).
           05  STATUS1I PIC  X(0001).
      *    -------------------------------
           05  STATDT1L PIC S9(0004) COMP.
           05  STATDT1F PIC  X(0001).
           05  FILLER REDEFINES STATDT1F.
               10  STATDT1A PIC  X(0001).
           05  STATDT1I PIC  X(0008).
      *    -------------------------------
           05  CHKAMT1L PIC S9(0004) COMP.
           05  CHKAMT1F PIC  X(0001).
           05  FILLER REDEFINES CHKAMT1F.
               10  CHKAMT1A PIC  X(0001).
           05  CHKAMT1I PIC  X(0012).
      *    -------------------------------
           05  CSHAMT1L PIC S9(0004) COMP.
           05  CSHAMT1F PIC  X(0001).
           05  FILLER REDEFINES CSHAMT1F.
               10  CSHAMT1A PIC  X(0001).
           05  CSHAMT1I PIC  X(0012).
      *    -------------------------------
           05  CLAIM1L PIC S9(0004) COMP.
           05  CLAIM1F PIC  X(0001).
           05  FILLER REDEFINES CLAIM1F.
               10  CLAIM1A PIC  X(0001).
           05  CLAIM1I PIC  X(0007).
      *    -------------------------------
           05  CHKNO2L PIC S9(0004) COMP.
           05  CHKNO2F PIC  X(0001).
           05  FILLER REDEFINES CHKNO2F.
               10  CHKNO2A PIC  X(0001).
           05  CHKNO2I PIC  X(0007).
      *    -------------------------------
           05  CHKDT2L PIC S9(0004) COMP.
           05  CHKDT2F PIC  X(0001).
           05  FILLER REDEFINES CHKDT2F.
               10  CHKDT2A PIC  X(0001).
           05  CHKDT2I PIC  X(0008).
      *    -------------------------------
           05  STATUS2L PIC S9(0004) COMP.
           05  STATUS2F PIC  X(0001).
           05  FILLER REDEFINES STATUS2F.
               10  STATUS2A PIC  X(0001).
           05  STATUS2I PIC  X(0001).
      *    -------------------------------
           05  STATDT2L PIC S9(0004) COMP.
           05  STATDT2F PIC  X(0001).
           05  FILLER REDEFINES STATDT2F.
               10  STATDT2A PIC  X(0001).
           05  STATDT2I PIC  X(0008).
      *    -------------------------------
           05  CHKAMT2L PIC S9(0004) COMP.
           05  CHKAMT2F PIC  X(0001).
           05  FILLER REDEFINES CHKAMT2F.
               10  CHKAMT2A PIC  X(0001).
           05  CHKAMT2I PIC  X(0012).
      *    -------------------------------
           05  CSHAMT2L PIC S9(0004) COMP.
           05  CSHAMT2F PIC  X(0001).
           05  FILLER REDEFINES CSHAMT2F.
               10  CSHAMT2A PIC  X(0001).
           05  CSHAMT2I PIC  X(0012).
      *    -------------------------------
           05  CLAIM2L PIC S9(0004) COMP.
           05  CLAIM2F PIC  X(0001).
           05  FILLER REDEFINES CLAIM2F.
               10  CLAIM2A PIC  X(0001).
           05  CLAIM2I PIC  X(0007).
      *    -------------------------------
           05  CHKNO3L PIC S9(0004) COMP.
           05  CHKNO3F PIC  X(0001).
           05  FILLER REDEFINES CHKNO3F.
               10  CHKNO3A PIC  X(0001).
           05  CHKNO3I PIC  X(0007).
      *    -------------------------------
           05  CHKDT3L PIC S9(0004) COMP.
           05  CHKDT3F PIC  X(0001).
           05  FILLER REDEFINES CHKDT3F.
               10  CHKDT3A PIC  X(0001).
           05  CHKDT3I PIC  X(0008).
      *    -------------------------------
           05  STATUS3L PIC S9(0004) COMP.
           05  STATUS3F PIC  X(0001).
           05  FILLER REDEFINES STATUS3F.
               10  STATUS3A PIC  X(0001).
           05  STATUS3I PIC  X(0001).
      *    -------------------------------
           05  STATDT3L PIC S9(0004) COMP.
           05  STATDT3F PIC  X(0001).
           05  FILLER REDEFINES STATDT3F.
               10  STATDT3A PIC  X(0001).
           05  STATDT3I PIC  X(0008).
      *    -------------------------------
           05  CHKAMT3L PIC S9(0004) COMP.
           05  CHKAMT3F PIC  X(0001).
           05  FILLER REDEFINES CHKAMT3F.
               10  CHKAMT3A PIC  X(0001).
           05  CHKAMT3I PIC  X(0012).
      *    -------------------------------
           05  CSHAMT3L PIC S9(0004) COMP.
           05  CSHAMT3F PIC  X(0001).
           05  FILLER REDEFINES CSHAMT3F.
               10  CSHAMT3A PIC  X(0001).
           05  CSHAMT3I PIC  X(0012).
      *    -------------------------------
           05  CLAIM3L PIC S9(0004) COMP.
           05  CLAIM3F PIC  X(0001).
           05  FILLER REDEFINES CLAIM3F.
               10  CLAIM3A PIC  X(0001).
           05  CLAIM3I PIC  X(0007).
      *    -------------------------------
           05  CHKNO4L PIC S9(0004) COMP.
           05  CHKNO4F PIC  X(0001).
           05  FILLER REDEFINES CHKNO4F.
               10  CHKNO4A PIC  X(0001).
           05  CHKNO4I PIC  X(0007).
      *    -------------------------------
           05  CHKDT4L PIC S9(0004) COMP.
           05  CHKDT4F PIC  X(0001).
           05  FILLER REDEFINES CHKDT4F.
               10  CHKDT4A PIC  X(0001).
           05  CHKDT4I PIC  X(0008).
      *    -------------------------------
           05  STATUS4L PIC S9(0004) COMP.
           05  STATUS4F PIC  X(0001).
           05  FILLER REDEFINES STATUS4F.
               10  STATUS4A PIC  X(0001).
           05  STATUS4I PIC  X(0001).
      *    -------------------------------
           05  STATDT4L PIC S9(0004) COMP.
           05  STATDT4F PIC  X(0001).
           05  FILLER REDEFINES STATDT4F.
               10  STATDT4A PIC  X(0001).
           05  STATDT4I PIC  X(0008).
      *    -------------------------------
           05  CHKAMT4L PIC S9(0004) COMP.
           05  CHKAMT4F PIC  X(0001).
           05  FILLER REDEFINES CHKAMT4F.
               10  CHKAMT4A PIC  X(0001).
           05  CHKAMT4I PIC  X(0012).
      *    -------------------------------
           05  CSHAMT4L PIC S9(0004) COMP.
           05  CSHAMT4F PIC  X(0001).
           05  FILLER REDEFINES CSHAMT4F.
               10  CSHAMT4A PIC  X(0001).
           05  CSHAMT4I PIC  X(0012).
      *    -------------------------------
           05  CLAIM4L PIC S9(0004) COMP.
           05  CLAIM4F PIC  X(0001).
           05  FILLER REDEFINES CLAIM4F.
               10  CLAIM4A PIC  X(0001).
           05  CLAIM4I PIC  X(0007).
      *    -------------------------------
           05  CHKNO5L PIC S9(0004) COMP.
           05  CHKNO5F PIC  X(0001).
           05  FILLER REDEFINES CHKNO5F.
               10  CHKNO5A PIC  X(0001).
           05  CHKNO5I PIC  X(0007).
      *    -------------------------------
           05  CHKDT5L PIC S9(0004) COMP.
           05  CHKDT5F PIC  X(0001).
           05  FILLER REDEFINES CHKDT5F.
               10  CHKDT5A PIC  X(0001).
           05  CHKDT5I PIC  X(0008).
      *    -------------------------------
           05  STATUS5L PIC S9(0004) COMP.
           05  STATUS5F PIC  X(0001).
           05  FILLER REDEFINES STATUS5F.
               10  STATUS5A PIC  X(0001).
           05  STATUS5I PIC  X(0001).
      *    -------------------------------
           05  STATDT5L PIC S9(0004) COMP.
           05  STATDT5F PIC  X(0001).
           05  FILLER REDEFINES STATDT5F.
               10  STATDT5A PIC  X(0001).
           05  STATDT5I PIC  X(0008).
      *    -------------------------------
           05  CHKAMT5L PIC S9(0004) COMP.
           05  CHKAMT5F PIC  X(0001).
           05  FILLER REDEFINES CHKAMT5F.
               10  CHKAMT5A PIC  X(0001).
           05  CHKAMT5I PIC  X(0012).
      *    -------------------------------
           05  CSHAMT5L PIC S9(0004) COMP.
           05  CSHAMT5F PIC  X(0001).
           05  FILLER REDEFINES CSHAMT5F.
               10  CSHAMT5A PIC  X(0001).
           05  CSHAMT5I PIC  X(0012).
      *    -------------------------------
           05  CLAIM5L PIC S9(0004) COMP.
           05  CLAIM5F PIC  X(0001).
           05  FILLER REDEFINES CLAIM5F.
               10  CLAIM5A PIC  X(0001).
           05  CLAIM5I PIC  X(0007).
      *    -------------------------------
           05  CHKNO6L PIC S9(0004) COMP.
           05  CHKNO6F PIC  X(0001).
           05  FILLER REDEFINES CHKNO6F.
               10  CHKNO6A PIC  X(0001).
           05  CHKNO6I PIC  X(0007).
      *    -------------------------------
           05  CHKDT6L PIC S9(0004) COMP.
           05  CHKDT6F PIC  X(0001).
           05  FILLER REDEFINES CHKDT6F.
               10  CHKDT6A PIC  X(0001).
           05  CHKDT6I PIC  X(0008).
      *    -------------------------------
           05  STATUS6L PIC S9(0004) COMP.
           05  STATUS6F PIC  X(0001).
           05  FILLER REDEFINES STATUS6F.
               10  STATUS6A PIC  X(0001).
           05  STATUS6I PIC  X(0001).
      *    -------------------------------
           05  STATDT6L PIC S9(0004) COMP.
           05  STATDT6F PIC  X(0001).
           05  FILLER REDEFINES STATDT6F.
               10  STATDT6A PIC  X(0001).
           05  STATDT6I PIC  X(0008).
      *    -------------------------------
           05  CHKAMT6L PIC S9(0004) COMP.
           05  CHKAMT6F PIC  X(0001).
           05  FILLER REDEFINES CHKAMT6F.
               10  CHKAMT6A PIC  X(0001).
           05  CHKAMT6I PIC  X(0012).
      *    -------------------------------
           05  CSHAMT6L PIC S9(0004) COMP.
           05  CSHAMT6F PIC  X(0001).
           05  FILLER REDEFINES CSHAMT6F.
               10  CSHAMT6A PIC  X(0001).
           05  CSHAMT6I PIC  X(0012).
      *    -------------------------------
           05  CLAIM6L PIC S9(0004) COMP.
           05  CLAIM6F PIC  X(0001).
           05  FILLER REDEFINES CLAIM6F.
               10  CLAIM6A PIC  X(0001).
           05  CLAIM6I PIC  X(0007).
      *    -------------------------------
           05  CHKNO7L PIC S9(0004) COMP.
           05  CHKNO7F PIC  X(0001).
           05  FILLER REDEFINES CHKNO7F.
               10  CHKNO7A PIC  X(0001).
           05  CHKNO7I PIC  X(0007).
      *    -------------------------------
           05  CHKDT7L PIC S9(0004) COMP.
           05  CHKDT7F PIC  X(0001).
           05  FILLER REDEFINES CHKDT7F.
               10  CHKDT7A PIC  X(0001).
           05  CHKDT7I PIC  X(0008).
      *    -------------------------------
           05  STATUS7L PIC S9(0004) COMP.
           05  STATUS7F PIC  X(0001).
           05  FILLER REDEFINES STATUS7F.
               10  STATUS7A PIC  X(0001).
           05  STATUS7I PIC  X(0001).
      *    -------------------------------
           05  STATDT7L PIC S9(0004) COMP.
           05  STATDT7F PIC  X(0001).
           05  FILLER REDEFINES STATDT7F.
               10  STATDT7A PIC  X(0001).
           05  STATDT7I PIC  X(0008).
      *    -------------------------------
           05  CHKAMT7L PIC S9(0004) COMP.
           05  CHKAMT7F PIC  X(0001).
           05  FILLER REDEFINES CHKAMT7F.
               10  CHKAMT7A PIC  X(0001).
           05  CHKAMT7I PIC  X(0012).
      *    -------------------------------
           05  CSHAMT7L PIC S9(0004) COMP.
           05  CSHAMT7F PIC  X(0001).
           05  FILLER REDEFINES CSHAMT7F.
               10  CSHAMT7A PIC  X(0001).
           05  CSHAMT7I PIC  X(0012).
      *    -------------------------------
           05  CLAIM7L PIC S9(0004) COMP.
           05  CLAIM7F PIC  X(0001).
           05  FILLER REDEFINES CLAIM7F.
               10  CLAIM7A PIC  X(0001).
           05  CLAIM7I PIC  X(0007).
      *    -------------------------------
           05  CHKNO8L PIC S9(0004) COMP.
           05  CHKNO8F PIC  X(0001).
           05  FILLER REDEFINES CHKNO8F.
               10  CHKNO8A PIC  X(0001).
           05  CHKNO8I PIC  X(0007).
      *    -------------------------------
           05  CHKDT8L PIC S9(0004) COMP.
           05  CHKDT8F PIC  X(0001).
           05  FILLER REDEFINES CHKDT8F.
               10  CHKDT8A PIC  X(0001).
           05  CHKDT8I PIC  X(0008).
      *    -------------------------------
           05  STATUS8L PIC S9(0004) COMP.
           05  STATUS8F PIC  X(0001).
           05  FILLER REDEFINES STATUS8F.
               10  STATUS8A PIC  X(0001).
           05  STATUS8I PIC  X(0001).
      *    -------------------------------
           05  STATDT8L PIC S9(0004) COMP.
           05  STATDT8F PIC  X(0001).
           05  FILLER REDEFINES STATDT8F.
               10  STATDT8A PIC  X(0001).
           05  STATDT8I PIC  X(0008).
      *    -------------------------------
           05  CHKAMT8L PIC S9(0004) COMP.
           05  CHKAMT8F PIC  X(0001).
           05  FILLER REDEFINES CHKAMT8F.
               10  CHKAMT8A PIC  X(0001).
           05  CHKAMT8I PIC  X(0012).
      *    -------------------------------
           05  CSHAMT8L PIC S9(0004) COMP.
           05  CSHAMT8F PIC  X(0001).
           05  FILLER REDEFINES CSHAMT8F.
               10  CSHAMT8A PIC  X(0001).
           05  CSHAMT8I PIC  X(0012).
      *    -------------------------------
           05  CLAIM8L PIC S9(0004) COMP.
           05  CLAIM8F PIC  X(0001).
           05  FILLER REDEFINES CLAIM8F.
               10  CLAIM8A PIC  X(0001).
           05  CLAIM8I PIC  X(0007).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0072).
      *    -------------------------------
           05  ERRMSG2L PIC S9(0004) COMP.
           05  ERRMSG2F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG2F.
               10  ERRMSG2A PIC  X(0001).
           05  ERRMSG2I PIC  X(0072).
      *    -------------------------------
           05  ENTERPFL PIC S9(0004) COMP.
           05  ENTERPFF PIC  X(0001).
           05  FILLER REDEFINES ENTERPFF.
               10  ENTERPFA PIC  X(0001).
           05  ENTERPFI PIC  99.
       01  EL146AO REDEFINES EL146AI.
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
           05  CHKNOO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BANKNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSHAMTO PIC  Z,ZZZ,ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NOTEO PIC  X(67).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKNO1O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKDT1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATUS1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATDT1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKAMT1O PIC  Z,ZZZ,ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSHAMT1O PIC  Z,ZZZ,ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLAIM1O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKNO2O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKDT2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATUS2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATDT2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKAMT2O PIC  Z,ZZZ,ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSHAMT2O PIC  Z,ZZZ,ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLAIM2O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKNO3O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKDT3O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATUS3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATDT3O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKAMT3O PIC  Z,ZZZ,ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSHAMT3O PIC  Z,ZZZ,ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLAIM3O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKNO4O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKDT4O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATUS4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATDT4O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKAMT4O PIC  Z,ZZZ,ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSHAMT4O PIC  Z,ZZZ,ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLAIM4O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKNO5O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKDT5O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATUS5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATDT5O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKAMT5O PIC  Z,ZZZ,ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSHAMT5O PIC  Z,ZZZ,ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLAIM5O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKNO6O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKDT6O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATUS6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATDT6O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKAMT6O PIC  Z,ZZZ,ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSHAMT6O PIC  Z,ZZZ,ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLAIM6O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKNO7O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKDT7O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATUS7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATDT7O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKAMT7O PIC  Z,ZZZ,ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSHAMT7O PIC  Z,ZZZ,ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLAIM7O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKNO8O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKDT8O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATUS8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATDT8O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKAMT8O PIC  Z,ZZZ,ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSHAMT8O PIC  Z,ZZZ,ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLAIM8O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENTERPFO PIC  X(0002).
      *    -------------------------------
00175  01  FILLER                      REDEFINES
00176      EL146AI.
020403     12  FILLER                            PIC X(35).
00178      12  EL146A-SCREEN.
020403         16  EL146A-TARGET-CHK-NO-LENGTH   PIC S9(04)  COMP.
020403         16  EL146A-TARGET-CHK-NO-ATTRB    PIC X(01).
020403         16  EL146A-TARGET-CHK-NO          PIC X(07).
020403         16  EL146A-BANK-NO-LENGTH         PIC S9(04)  COMP.
020403         16  EL146A-BANK-NO-ATTRB          PIC X(01).
020403         16  EL146A-BANK-NO                PIC X(10).
020403         16  EL146A-TARGET-CSH-AMT-LENGTH  PIC S9(04)  COMP.
020403         16  EL146A-TARGET-CSH-AMT-ATTRB   PIC X(01).
020403         16  EL146A-TARGET-CSH-AMT         PIC Z,ZZZ,ZZ9.99.
020403         16  EL146A-CHK-NOTE-LENGTH        PIC S9(04)  COMP.
020403         16  EL146A-CHK-NOTE-ATTRB         PIC X(01).
020403         16  EL146A-CHK-NOTE               PIC X(67).
00179          16  EL146A-CHECK-DEFINED-ACTIVITY OCCURS 08 TIMES.
00180              20  EL146A-CHK-NO-LENGTH      PIC S9(04)  COMP.
00181              20  EL146A-CHK-NO-ATTRB       PIC X(01).
00182              20  EL146A-CHK-NO             PIC X(07).
00183
00184              20  EL146A-CHK-DATE-LENGTH    PIC S9(04)  COMP.
00185              20  EL146A-CHK-DATE-ATTRB     PIC X(01).
00186              20  EL146A-CHK-DATE           PIC 9(08).
00187
00188              20  EL146A-CHK-STAT-LENGTH    PIC S9(04)  COMP.
00189              20  EL146A-CHK-STAT-ATTRB     PIC X(01).
00190              20  EL146A-CHK-STAT           PIC X(01).
00191
00192              20  EL146A-CHK-STDT-LENGTH    PIC S9(04)  COMP.
00193              20  EL146A-CHK-STDT-ATTRB     PIC X(01).
00194              20  EL146A-CHK-STDT           PIC 9(08).
00195
00196              20  EL146A-CHK-AMT-LENGTH     PIC S9(04)  COMP.
00197              20  EL146A-CHK-AMT-ATTRB      PIC X(01).
00198              20  EL146A-CHK-AMT            PIC Z,ZZZ,ZZ9.99.
00199
020403             20  EL146A-CHK-CSH-AMT-LENGTH PIC S9(04)  COMP.
020403             20  EL146A-CHK-CSH-AMT-ATTRB  PIC X(01).
020403             20  EL146A-CHK-CSH-AMT        PIC Z,ZZZ,ZZ9.99.
00203
020403             20  EL146A-CHK-CLM-NO-LENGTH  PIC S9(04)  COMP.
020403             20  EL146A-CHK-CLM-NO-ATTRB   PIC X(01).
020403             20  EL146A-CHK-CLM-NO         PIC X(07).
00211
00211
00212      EJECT
00213 *                                COPY ELCEMIB.
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
00214      EJECT
00215 *                                COPY ELCDATE.
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
00216      EJECT
00217 *                                COPY ELCLOGOF.
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
00218      EJECT
00219 *                                COPY ELCATTR.
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
00220      EJECT
00221 *                                COPY ELCAID.
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
00222  01  FILLER                      REDEFINES
00223      DFHAID.
00224
00225      12  FILLER                  PIC X(08).
00226
00227      12  PF-VALUES               PIC X(01)
00228          OCCURS 24 TIMES.
00229      EJECT
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
00231
00232  01  DFHCOMMAREA                 PIC X(1024).
00233
00234      EJECT
00235 *                                COPY ELCRCON.
00001 ******************************************************************
00002 *                                                                *
      *
121703*   THIS COPYBOOK IS NOT BEING USED IN LOGIC
      *
      *
00002 *                                                                *
00003 *                            ELCRCON.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK RECONCILIATION FILE                 *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 194  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELRCON                         RKP=2,LEN=19   *
00013 *                                                                *
00014 *   LOG = YES                                                    *
00015 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00016 ******************************************************************
00017
00018  01  CHECK-RECONCILIATION.
00019      12  RC-RECORD-ID                          PIC XX.
00020          88  VALID-RC-ID                    VALUE 'RC'.
00021      12  RC-CONTROL-PRIMARY.
00022          16  RC-COMPANY-CD                     PIC X.
00023          16  RC-CHECK-NO                       PIC X(7).
00024          16  RC-CHECK-ORIGIN                   PIC X.
00025          16  RC-GL-ACCOUNT-NO                  PIC X(10).
00026
00027      12  RC-CHECK-DATA.
00028          16  RC-ISSUE-DATE.
00029              20  RC-ISSUE-YYYY.
00030                22  RC-ISSUE-CC                 PIC XX.
00031                22  RC-ISSUE-YY                 PIC XX.
00032              20  RC-ISSUE-MM                   PIC XX.
00033              20  RC-ISSUE-DD                   PIC XX.
00034          16  RC-CHECK-AMOUNT                   PIC 9(7)V99.
00035          16  RC-CARRIER                        PIC X.
00036          16  RC-CLAIM-NO                       PIC X(7).
00037          16  RC-REFERENCE-NO                   PIC X(20).
00038          16  RC-MORTGAGE-REF   REDEFINES  RC-REFERENCE-NO.
00039              20  RC-MORT-REF-1-18              PIC X(18).
00040              20  RC-MORT-REF-SUF-19-20         PIC XX.
00041          16  FILLER  REDEFINES  RC-MORTGAGE-REF.
00042              20  FILLER                        PIC X(13).
00043              20  RC-CLAIM-REF.
00044                22  RC-CLAIM-PREFIX             PIC X.
00045                22  RC-CLAIM-NO-REF             PIC X(6).
00046          16  RC-COVERAGE-TYPE                  PIC X.
00047          16  RC-BENEFIT-CODE                   PIC XX.
00048          16  RC-BENEFICIARY                    PIC X(10).
00049          16  RC-PAYMENT-TYPE                   PIC X.
00050          16  RC-STATUS                         PIC X.
020403           88  RC-STATUS-ABANDONED                VALUE 'A'.
00052            88  RC-STATUS-DESTROYED                VALUE 'D'.
00053            88  RC-STATUS-OUTSTANDING              VALUE 'O'.
00054            88  RC-STATUS-REDEEMED                 VALUE 'R'.
00055            88  RC-STATUS-STOP-PAY                 VALUE 'S'.
00056            88  RC-STATUS-UNREDEEMED               VALUE 'U'.
00057            88  RC-STATUS-VOIDED                   VALUE 'V'.
00058          16  RC-STATUS-DATE.
00059              20  RC-STATUS-YYYY.
00060                22  RC-STATUS-CC                PIC XX.
00061                22  RC-STATUS-YY                PIC XX.
00062              20  RC-STATUS-MM                  PIC XX.
00063              20  RC-STATUS-DD                  PIC XX.
00064
00065      12  RC-MAINT-AREA.
00066          16  RC-LAST-MAINT-BY                  PIC X(4).
00067          16  RC-LAST-MAINT-DT                  PIC XX.
00068          16  RC-LAST-MAINT-HHMMSS    COMP-3    PIC S9(7).
00069
00070      12  RC-CHECK-MAINT-AREA.
00071          16  RC-LAST-CHECK-BY                  PIC X(4).
00072          16  RC-LAST-CHECK-DT                  PIC XX.
00073          16  RC-LAST-CHECK-HHMMSS    COMP-3    PIC S9(7).
020403     12  RC-CASHED-AMOUNT                      PIC 9(7)V99.
00074
020403     12  RC-CHECK-NOTE                         PIC X(67).
020403     12  FILLER                                PIC X(11).
00076
00236      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA
                                CHECK-RECONCILIATION.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL146' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00238
00239      MOVE EIBDATE                TO  DC-JULIAN-YYDDD
00240      MOVE '5'                    TO  DC-OPTION-CODE
00241      PERFORM 8500-DATE-CONVERSION.
00242      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE
00243      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE
00244
00245      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK
00246
00247      MOVE +2                     TO  EMI-NUMBER-OF-LINES
00248      MOVE '2'                    TO  EMI-SWITCH2
00249
00250      IF EIBCALEN IS EQUAL TO 0
00251          GO TO 8800-UNAUTHORIZED-ACCESS
020403     END-IF
00253 *    NOTE *******************************************************
00254 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
00255 *         *  FROM ANOTHER MODULE.                               *
00256 *         *******************************************************.
00257
00258      
      * EXEC CICS HANDLE CONDITION
00259 *        ERROR    (9990-ERROR)
00260 *        NOTOPEN  (8870-NOTOPEN)
00261 *        PGMIDERR (9600-PGMIDERR)
00262 *    END-EXEC
      *    MOVE '"$.JL                 ! " #00001740' TO DFHEIV0
           MOVE X'22242E4A4C20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031373430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
020403     .
00263
00264      EJECT
00265  0010-MAIN-LOGIC.
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
00277          ELSE
00278              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00279              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00280              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00281              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00282              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00283              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00284              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00285              MOVE SPACES               TO  PI-SAVED-PROGRAM-6
020403         END-IF
00286      ELSE
00287          GO TO 0020-MAIN-LOGIC
020403     END-IF
020403     .
00288
00289  0015-MAIN-LOGIC.
00290 *    NOTE *******************************************************
00291 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      *
00292 *         *  INTERFACE BLOCK FOR THIS MODULE.                   *
00293 *         *******************************************************.
00294
00295      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA
00296
00297      IF EIBTRNID NOT EQUAL WS-TRANS-ID
00298          MOVE LOW-VALUES         TO  EL146AI
00299          MOVE LOW-VALUES         TO  PI-PREV-KEY
00300          MOVE LOW-VALUES         TO  ELRCON-KEY
00301                                      PI-SAVE-KEY
020403*                                    PI-STOP-PAY-KEY
020403*        MOVE 'N'                TO  PI-STOP-PAY-SW
00304                                      WS-RECORDS-READ-SW
00305          MOVE 'Y'                TO  PI-FIRST-TIME-SW
00306          MOVE PI-COMPANY-CD      TO  PI-PREV-COMPANY-CD
00307          GO TO 8100-SEND-INITIAL-MAP
020403     END-IF
020403     .
00308
00309      EJECT
00310  0020-MAIN-LOGIC.
00311 *    NOTE *******************************************************
00312 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *
00313 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *
00314 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
00315 *         *******************************************************.
00316
00317      IF EIBAID EQUAL DFHCLEAR
00318          GO TO 9400-CLEAR
020403     END-IF
00319
00323      
      * EXEC CICS READQ TS
00324 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00325 *            INTO    (SECURITY-CONTROL)
00326 *            LENGTH  (SC-COMM-LENGTH)
00327 *            ITEM    (SC-ITEM)
00328 *    END-EXEC
      *    MOVE '*$II   L              ''   #00001809' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00329      MOVE SC-CLAIMS-DISPLAY (23)  TO  PI-DISPLAY-CAP
00330      MOVE SC-CLAIMS-UPDATE  (23)  TO  PI-MODIFY-CAP
00331      IF NOT DISPLAY-CAP
00332          MOVE 'READ'              TO  SM-READ
00333          PERFORM 9995-SECURITY-VIOLATION
00334          MOVE ER-0070             TO  EMI-ERROR
00335          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00336          GO TO 8100-SEND-INITIAL-MAP
020403     END-IF
020403     .
00337
00338  0200-RECEIVE.
00339
00340      IF EIBAID EQUAL DFHPA1 OR DFHPA2 OR DFHPA3
00341          MOVE LOW-VALUES         TO  EL146AI
00342          MOVE ER-0008            TO  EMI-ERROR
00343          PERFORM 8200-SEND-DATAONLY
020403     END-IF
00344
00345      
      * EXEC CICS RECEIVE
00346 *        INTO   (EL146AI)
00347 *        MAPSET (WS-MAPSET-NAME)
00348 *        MAP    (WS-MAP-NAME)
00349 *    END-EXEC
           MOVE LENGTH OF
            EL146AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001834' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL146AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00350
00351      IF ENTERPFL IS EQUAL TO +0
00352          GO TO 0300-CHECK-PFKEYS
020403     END-IF
00353
00354      IF EIBAID NOT = DFHENTER
00355          MOVE ER-0004            TO  EMI-ERROR
00356          GO TO 0320-INPUT-ERROR
020403     END-IF
00357
00358      IF (ENTERPFI NUMERIC) AND (ENTERPFI > 0 AND < 25)
00359          MOVE PF-VALUES (ENTERPFI)   TO  EIBAID
00360      ELSE
00361          MOVE ER-0029                TO  EMI-ERROR
00362          GO TO 0320-INPUT-ERROR
020403     END-IF
020403     .
00363
00364  0300-CHECK-PFKEYS.
00365
020403*    IF (EIBAID = DFHPF1)  OR
020403*       (EIBAID = DFHPF2)  OR
020403*       (EIBAID = DFHPF12) OR
020403*       (EIBAID = DFHPF23) OR
020403*       (EIBAID = DFHPF24)
020403*        MOVE 'N' TO PI-STOP-PAY-SW
020403*    END-IF
00372
00373      IF EIBAID EQUAL DFHPF12
00374          MOVE XCTL-010                TO  THIS-PGM
00375          GO TO 9300-XCTL
020403     END-IF
00376
00377      IF EIBAID EQUAL DFHPF23
00378          GO TO 9000-RETURN-CICS
020403     END-IF
00379
00380      IF EIBAID EQUAL DFHPF24
00381          MOVE XCTL-126                TO  THIS-PGM
00382          GO TO 9300-XCTL
020403     END-IF
00383
00384      IF EIBAID EQUAL DFHPF1
00385          MOVE  +0                     TO  SUB
00386          GO TO 4000-START-BROWSE-FORWARD
020403     END-IF
00387
00388      IF EIBAID EQUAL DFHPF2
00389          MOVE  +9                     TO  SUB
00390          GO TO 5000-START-BROWSE-BACKWARD
020403     END-IF
00391
020403     IF EIBAID = DFHPF3
020403         IF CHKNOL > +0
020403             MOVE  +0                 TO  SUB
020403             GO TO 4000-START-BROWSE-FORWARD
020403         ELSE
020403             MOVE AL-UABON            TO  MAINTA
020403             MOVE  -1                 TO  MAINTL
020403             MOVE  ER-0773            TO  EMI-ERROR
020403             PERFORM 9900-ERROR-FORMAT THRU  9900-EXIT
00430              GO TO 8200-SEND-DATAONLY
020403         END-IF
020403     END-IF
00391
00392      IF EIBAID EQUAL DFHENTER
00393          IF MAINTL IS EQUAL TO +0
00394              MOVE +0                  TO  SUB
00395              GO TO 4000-START-BROWSE-FORWARD
00396          ELSE
00397              GO TO 0330-EDIT-DATA
020403         END-IF
020403     END-IF
00399      MOVE ER-0029                TO  EMI-ERROR
020403     .
00400
00401  0320-INPUT-ERROR.
00402
00403      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00404      MOVE AL-UNBON               TO  ENTERPFA
00405
00406      IF ENTERPFL IS EQUAL TO +0
00407          MOVE -1                 TO  MAINTL
00408      ELSE
00409          MOVE -1                 TO  ENTERPFL
020403     END-IF
00410
00411      GO TO 8200-SEND-DATAONLY
020403     .
00412
00413  0330-EDIT-DATA.
00414
00415      IF NOT  MODIFY-CAP
00416          MOVE 'UPDATE'         TO  SM-READ
00417          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00418          MOVE ER-0070          TO EMI-ERROR
00419          PERFORM  9900-ERROR-FORMAT  THRU  9900-EXIT
00420          MOVE LOW-VALUES       TO EL146AO
00421          GO TO 8100-SEND-INITIAL-MAP
020403     END-IF
00422
020403     IF (MAINTI IS EQUAL TO  'R' OR 'O' OR 'A' OR 'M')
00424          CONTINUE
00425      ELSE
00426          MOVE AL-UABON           TO  MAINTA
00427          MOVE  -1                TO  MAINTL
00428          MOVE  ER-0773           TO  EMI-ERROR
00429          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00430          GO TO 8200-SEND-DATAONLY
020403     END-IF
00431
020403     IF (CHKNOL > +0)
020403        AND (BANKNOL > +0)
020403         CONTINUE
00436      ELSE
00437          MOVE ER-0774           TO  EMI-ERROR
00438          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
020403         MOVE -1                TO  CHKNOL
020403         MOVE AL-UABON          TO  CHKNOA
00441          GO TO 8200-SEND-DATAONLY
020403     END-IF
00442
020403     IF CSHAMTL > +0
020403         MOVE CSHAMTI               TO DEEDIT-FIELD
00445          PERFORM 8000-DEEDIT        THRU 8000-EXIT
00446          IF DEEDIT-FIELD-CHK NUMERIC
020403             MOVE DEEDIT-FIELD-CHK  TO CSHAMTO
00448          ELSE
020403             MOVE AL-UNBON          TO CSHAMTA
020403             MOVE -1                TO CSHAMTL
00451              MOVE ER-0775           TO EMI-ERROR
00452              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
020403         END-IF
020403     END-IF
00462
00463       IF NOT EMI-NO-ERRORS
00464           GO TO 8200-SEND-DATAONLY
00465       ELSE
00466           GO TO 6000-REWRITE-CHECK-RECON
020403      END-IF
020403      .
00467
00468  4000-START-BROWSE-FORWARD.
00469
00470      MOVE  PI-COMPANY-CD         TO RECON-COMPANY-CD
00471
00472      IF CHKNOL > +0
00473          MOVE CHKNOI             TO RECON-CHECK-NO
020403******** Origin is always 'C' indicating the check is from the
020403******** claims system
020403         MOVE 'C'                TO RECON-ORIGIN
020403         IF EIBAID = DFHPF3
020403             MOVE SPACES         TO RECON-BANK-NO
020403         ELSE
00484              IF BANKNOL > +0
00485                  MOVE BANKNOI    TO RECON-BANK-NO
020403             ELSE
020403                 MOVE SPACES     TO RECON-BANK-NO
020403             END-IF
020403         END-IF
020403     ELSE
00491          IF PI-FIRST-TIME-SW IS EQUAL TO 'Y'
00492              MOVE 'N'                        TO PI-FIRST-TIME-SW
00493          ELSE
00494              IF PI-SAVE-CHECK-NO (8) = LOW-VALUES
00495                  MOVE -1                     TO MAINTL
00496                  MOVE ER-0130                TO EMI-ERROR
00497                  PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
00498                  GO TO 8200-SEND-DATAONLY
00499              ELSE
00500                  MOVE PI-SAVE-COMPANY-CD (8) TO RECON-COMPANY-CD
00501                  MOVE PI-SAVE-CHECK-NO (8)   TO RECON-CHECK-NO
00502                  MOVE PI-SAVE-ORIGIN (8)     TO RECON-ORIGIN
00503                  MOVE PI-SAVE-BANK-NO (8)    TO RECON-BANK-NO
00504                  MOVE LOW-VALUES             TO PI-SAVE-KEY
020403             END-IF
020403         END-IF
020403     END-IF
00474
020403     .
00505
00506  4000-START-BROWSE.
00507
00508      
      * EXEC CICS HANDLE CONDITION
00509 *        ENDFILE   (8700-END-FILE)
00510 *        NOTFND    (8300-NOT-FOUND)
00511 *    END-EXEC
      *    MOVE '"$''I                  ! # #00002022' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303032303232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00512      
      * EXEC CICS STARTBR
00513 *        DATASET   ('ELRCON')
00514 *        RIDFLD    (ELRCON-KEY)
00515 *    END-EXEC
           MOVE 'ELRCON' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00002026' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELRCON-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
020403     .
00516
00517  4000-READ-NEXT.
00518
00519      
      * EXEC CICS READNEXT
00520 *        DATASET   ('ELRCON')
00521 *        SET       (ADDRESS OF CHECK-RECONCILIATION)
00522 *        RIDFLD    (ELRCON-KEY)
00523 *    END-EXEC
           MOVE 'ELRCON' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00002034' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELRCON-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-RECONCILIATION TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00524
00525      IF PI-COMPANY-CD NOT = RC-COMPANY-CD
00526          GO TO 8700-END-FILE
020403     END-IF
00527
020403*    IF RC-CONTROL-PRIMARY = PI-PREV-KEY
020403*        GO TO 4000-READ-NEXT
020403*    END-IF
00530
00531      MOVE 'N'                      TO  PI-FIRST-TIME-SW
00532      MOVE 'Y'                      TO  WS-RECORDS-READ-SW
00533
00534      ADD +1                        TO  SUB
020403     IF EIBAID = DFHPF3 AND SUB = +1
020403        AND RC-CHECK-NO = CHKNOI
020403         MOVE RC-CHECK-NO          TO  EL146A-TARGET-CHK-NO
020403         MOVE RC-GL-ACCOUNT-NO     TO  EL146A-BANK-NO
020403         MOVE RC-CASHED-AMOUNT     TO  EL146A-TARGET-CSH-AMT
020403         MOVE RC-CHECK-NOTE        TO  EL146A-CHK-NOTE
020403     END-IF
00535      IF SUB > +8
020403        AND EIBAID NOT = DFHPF3
00536          MOVE +0                 TO  SUB
00537          MOVE ELRCON-KEY         TO  PI-PREV-KEY
020403         MOVE LOW-VALUES         TO  CHKNOO
020403         MOVE ZEROS              TO  CSHAMTO
020403         MOVE LOW-VALUES         TO  BANKNOO
020403         MOVE LOW-VALUES         TO  NOTEO
020403         MOVE LOW-VALUES         TO  MAINTO
00543          GO TO 8100-SEND-INITIAL-MAP
020403     ELSE
020403         IF SUB > +8
020403            AND EIBAID = DFHPF3
020403            GO TO 8100-SEND-INITIAL-MAP
020403         END-IF
020403     END-IF
00544
00545      MOVE ELRCON-KEY             TO  PI-PREV-KEY
00546
00547      MOVE RC-COMPANY-CD          TO  PI-SAVE-COMPANY-CD (SUB)
020403     MOVE RC-CHECK-ORIGIN        TO  PI-SAVE-ORIGIN (SUB)
00548      MOVE RC-GL-ACCOUNT-NO       TO  PI-SAVE-BANK-NO (SUB)
00550      MOVE RC-CHECK-NO            TO  EL146A-CHK-NO (SUB)
00551                                      PI-SAVE-CHECK-NO (SUB)
00553      MOVE RC-ISSUE-YYYY          TO  WS-DATE-YY
00554      MOVE WS-DATE-YY             TO  WS-EDIT-YY
00555      MOVE RC-ISSUE-MM            TO  WS-EDIT-MM
00556      MOVE RC-ISSUE-DD            TO  WS-EDIT-DD
00557      MOVE WS-EDIT-DATE           TO  EL146A-CHK-DATE (SUB)
00558
00559      MOVE RC-STATUS              TO  EL146A-CHK-STAT (SUB)
00560
00561      MOVE RC-STATUS-YYYY         TO  WS-DATE-YY
00562      MOVE WS-DATE-YY             TO  WS-EDIT-YY
00563      MOVE RC-STATUS-MM           TO  WS-EDIT-MM
00564      MOVE RC-STATUS-DD           TO  WS-EDIT-DD
00565      MOVE WS-EDIT-DATE           TO  EL146A-CHK-STDT (SUB)
00566
00567      MOVE RC-CHECK-AMOUNT        TO  EL146A-CHK-AMT (SUB)
020403     MOVE RC-CASHED-AMOUNT       TO  EL146A-CHK-CSH-AMT (SUB)
020403     MOVE RC-CLAIM-NO            TO  EL146A-CHK-CLM-NO (SUB)
00576      GO TO 4000-READ-NEXT
020403     .
00577
00578  4000-EXIT.
00579      EXIT.
00580      EJECT
00581  5000-START-BROWSE-BACKWARD.
00582
00583      
      * EXEC CICS HANDLE CONDITION
00584 *        ENDFILE   (8710-END-FILE)
00585 *        NOTFND    (8300-NOT-FOUND)
00586 *    END-EXEC
      *    MOVE '"$''I                  ! $ #00002108' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303032313038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00587
00588      MOVE  PI-COMPANY-CD                   TO RECON-COMPANY-CD
00589
00590      IF CHKNOL > +0
00591          MOVE CHKNOI                       TO RECON-CHECK-NO
00596          IF BANKNOL > +0
00597              MOVE BANKNOI                  TO RECON-BANK-NO
00598          ELSE
00599              MOVE SPACES                   TO RECON-BANK-NO
020403         END-IF
020403     ELSE
00603          IF PI-SAVE-CHECK-NO (1) = LOW-VALUES
00604              IF PI-PREV-KEY = LOW-VALUES
00605                  MOVE -1                   TO MAINTL
00606                  MOVE ER-0131              TO EMI-ERROR
00607                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00608                  GO TO 8200-SEND-DATAONLY
00609              ELSE
00610                  MOVE PI-PREV-KEY          TO ELRCON-KEY
020403             END-IF
00611          ELSE
00612              MOVE PI-SAVE-COMPANY-CD (1)   TO RECON-COMPANY-CD
00613              MOVE PI-SAVE-CHECK-NO (1)     TO RECON-CHECK-NO
00614              MOVE PI-SAVE-ORIGIN (1)       TO RECON-ORIGIN
00615              MOVE PI-SAVE-BANK-NO (1)      TO RECON-BANK-NO
00616              MOVE LOW-VALUES               TO PI-SAVE-KEY
020403         END-IF
020403     END-IF
00592
00618      
      * EXEC CICS STARTBR
00619 *        DATASET   ('ELRCON')
00620 *        RIDFLD    (ELRCON-KEY)
00621 *    END-EXEC
           MOVE 'ELRCON' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00002141' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELRCON-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
020403     .
00622
00623  5000-READ-PREV.
00624
00625      
      * EXEC CICS READPREV
00626 *        DATASET   ('ELRCON')
00627 *        SET       (ADDRESS OF CHECK-RECONCILIATION)
00628 *        RIDFLD    (ELRCON-KEY)
00629 *    END-EXEC
           MOVE 'ELRCON' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00002149' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELRCON-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-RECONCILIATION TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00630
00631      IF PI-COMPANY-CD NOT = RC-COMPANY-CD
00632          GO TO 8710-END-FILE
020403     END-IF
00633
020403     IF RC-CONTROL-PRIMARY  EQUAL  PI-PREV-KEY
020403         GO TO 5000-READ-PREV
020403     END-IF
00636
00637      MOVE 'Y'                    TO  WS-RECORDS-READ-SW
00638
00639      SUBTRACT +1                 FROM  SUB
00640      IF SUB IS LESS THAN  +1
00641          MOVE +0                 TO  SUB
00642          MOVE LOW-VALUES         TO  CHKNOO
00644          MOVE LOW-VALUES         TO  BANKNOO
020403*        MOVE ZEROS              TO  CHKAMTO
00646          MOVE ELRCON-KEY         TO  PI-PREV-KEY
00647          GO TO 8100-SEND-INITIAL-MAP
020403     END-IF
00648
00649      MOVE ELRCON-KEY             TO  PI-PREV-KEY
00651      MOVE RC-COMPANY-CD          TO  PI-SAVE-COMPANY-CD (SUB)
020403     MOVE RC-CHECK-ORIGIN        TO  PI-SAVE-ORIGIN (SUB)
00652      MOVE RC-GL-ACCOUNT-NO       TO  PI-SAVE-BANK-NO (SUB)
00653
00654      MOVE RC-CHECK-NO            TO  EL146A-CHK-NO (SUB)
00655                                      PI-SAVE-CHECK-NO (SUB)
00656
00657      MOVE RC-ISSUE-YYYY          TO  WS-DATE-YY
00658      MOVE WS-DATE-YY             TO  WS-EDIT-YY
00659      MOVE RC-ISSUE-MM            TO  WS-EDIT-MM
00660      MOVE RC-ISSUE-DD            TO  WS-EDIT-DD
00661      MOVE WS-EDIT-DATE           TO  EL146A-CHK-DATE (SUB)
00662
00663      MOVE RC-STATUS              TO  EL146A-CHK-STAT (SUB)
00664
00665      MOVE RC-STATUS-YYYY         TO  WS-DATE-YY
00666      MOVE WS-DATE-YY             TO  WS-EDIT-YY
00667      MOVE RC-STATUS-MM           TO  WS-EDIT-MM
00668      MOVE RC-STATUS-DD           TO  WS-EDIT-DD
00669      MOVE WS-EDIT-DATE           TO  EL146A-CHK-STDT (SUB)
00670
00671      MOVE RC-CHECK-AMOUNT        TO  EL146A-CHK-AMT (SUB)
020403     MOVE RC-CASHED-AMOUNT       TO  EL146A-CHK-CSH-AMT (SUB)
020403     MOVE RC-CLAIM-NO            TO  EL146A-CHK-CLM-NO (SUB)
00680      GO TO 5000-READ-PREV
020403     .
00681
00682  5000-EXIT.
00683      EXIT.
00684      EJECT
00685  6000-REWRITE-CHECK-RECON.
00686
00687      
      * EXEC CICS HANDLE CONDITION
00688 *        NOTFND    (8300-NOT-FOUND)
00689 *    END-EXEC
      *    MOVE '"$I                   ! % #00002208' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303032323038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00690
00691      MOVE  LOW-VALUES           TO  ELRCON-KEY
00692
00693      MOVE  PI-COMPANY-CD        TO  RECON-COMPANY-CD
00694      MOVE  CHKNOI               TO  RECON-CHECK-NO
020403     MOVE 'C'                   TO  RECON-ORIGIN
00699      MOVE BANKNOI               TO  RECON-BANK-NO
00700
00701      
      * EXEC CICS READ
00702 *        DATASET     ('ELRCON')
00703 *        SET         (ADDRESS OF CHECK-RECONCILIATION)
00704 *        RIDFLD      (ELRCON-KEY)
00705 *        UPDATE
00706 *    END-EXEC
           MOVE 'ELRCON' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00002219' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELRCON-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-RECONCILIATION TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00707
00708      MOVE ELRCON-KEY         TO  PI-PREV-KEY
00709
00710      MOVE  SAVE-BIN-DATE     TO  RC-LAST-MAINT-DT
00711      MOVE  EIBTIME           TO  RC-LAST-MAINT-HHMMSS
00712      MOVE  PI-PROCESSOR-ID   TO  RC-LAST-MAINT-BY
00713
020403     IF (MAINTI = 'O')
020403        AND (RC-STATUS NOT = 'R' AND 'A')
00726          MOVE ER-0795                    TO EMI-ERROR
00727          MOVE -1                         TO MAINTL
00728          PERFORM 9900-ERROR-FORMAT       THRU 9900-EXIT
00729          GO TO 8200-SEND-DATAONLY
020403     END-IF
00730
00731      IF (MAINTI = 'R')
020403        AND (RC-STATUS NOT = 'O' AND 'A' AND 'S')
00733          MOVE ER-0850                    TO EMI-ERROR
00734          MOVE -1                         TO MAINTL
00735          PERFORM 9900-ERROR-FORMAT       THRU 9900-EXIT
00736          GO TO 8200-SEND-DATAONLY
020403     END-IF
00737
020403     IF (MAINTI = 'A')
020403        AND (RC-STATUS NOT = 'O')
020403         MOVE ER-0789                    TO EMI-ERROR
020403         MOVE -1                         TO MAINTL
020403         PERFORM 9900-ERROR-FORMAT       THRU 9900-EXIT
020403         GO TO 8200-SEND-DATAONLY
020403     END-IF
00737
020403     IF MAINTI = 'M'
020403         IF CSHAMTL > +0
020403            AND RC-STATUS NOT = 'R'
020403             MOVE ER-0796                TO EMI-ERROR
020403             MOVE -1                     TO MAINTL
020403             PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
020403             GO TO 8200-SEND-DATAONLY
020403         END-IF
020403     END-IF
00737
020403*    IF MAINTI  IS EQUAL TO  'S'
020403*       IF RC-STATUS  IS EQUAL TO  'S' OR 'V'
020403*           NEXT SENTENCE
020403*       ELSE
020403*           MOVE ER-0796         TO  EMI-ERROR
020403*           MOVE -1              TO  MAINTL
020403*           PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
020403*           GO TO 8200-SEND-DATAONLY.
00746
020403*    IF MAINTI IS EQUAL TO 'S'
020403*       IF PI-STOP-PAY-KEY NOT EQUAL TO  PI-PREV-KEY
020403*           MOVE 'N'          TO  PI-STOP-PAY-SW.
00750
020403*    IF MAINTI IS EQUAL TO 'S'
020403*       IF PI-STOP-PAY-SW   IS EQUAL TO 'N'
020403*           MOVE ER-0831      TO  EMI-ERROR
020403*           MOVE -1           TO  MAINTL
020403*           MOVE AL-UABON     TO  MAINTA
020403*           MOVE SPACES       TO  MAINTO
020403*           MOVE 'Y'          TO  PI-STOP-PAY-SW
020403*           MOVE PI-PREV-KEY  TO  PI-STOP-PAY-KEY
020403*           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
020403*           GO TO 8200-SEND-DATAONLY.
00761
00762      IF MAINTI = 'R'
00770          MOVE 'R'                        TO RC-STATUS
020403         MOVE RC-CHECK-AMOUNT            TO RC-CASHED-AMOUNT
020403     END-IF
00771
020403     IF (MAINTI = 'O')
00773          MOVE  'O'                       TO RC-STATUS
020403         MOVE ZEROS                      TO RC-CASHED-AMOUNT
020403     END-IF
020403     IF (MAINTI = 'A')
020403         MOVE  'A'                       TO RC-STATUS
020403     END-IF
020403     IF MAINTI = 'M'
020403        AND CSHAMTL > +0
020403         MOVE CSHAMTO                    TO RC-CASHED-AMOUNT
020403     END-IF
00774
00775      MOVE EIBDATE                        TO DC-JULIAN-YYDDD
00776      MOVE '5'                            TO DC-OPTION-CODE
00777      PERFORM 8500-DATE-CONVERSION
00778      MOVE DC-GREG-DATE-1-MDY             TO WS-WORK-DATE
00779      IF WS-WORK-YY IS GREATER THAN 50
00780          MOVE '19'            TO  WS-RCON-YY-1
00781          MOVE WS-WORK-YY      TO  WS-RCON-YY-2
00782          MOVE WS-WORK-MM      TO  WS-RCON-MM
00783          MOVE WS-WORK-DD      TO  WS-RCON-DD
00784      ELSE
00785          MOVE '20'            TO  WS-RCON-YY-1
00786          MOVE WS-WORK-YY      TO  WS-RCON-YY-2
00787          MOVE WS-WORK-MM      TO  WS-RCON-MM
00788          MOVE WS-WORK-DD      TO  WS-RCON-DD
020403     END-IF
00789
020403     IF MAINTI NOT = 'M'
00790          MOVE WS-RCON-DATE               TO RC-STATUS-DATE
020403     END-IF
020403     IF NOTEL > +0
020403         MOVE NOTEI                      TO RC-CHECK-NOTE
020403     END-IF
00791
00792      
      * EXEC CICS REWRITE
00793 *        DATASET   (ELRCON-DSID)
00794 *        FROM      (CHECK-RECONCILIATION)
00795 *    END-EXEC
           MOVE LENGTH OF
            CHECK-RECONCILIATION
             TO DFHEIV11
      *    MOVE '&& L                  %   #00002330' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELRCON-DSID, 
                 CHECK-RECONCILIATION, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
020403     IF EIBAID = DFHPF3
020403         GO TO 6000-EXIT
           END-IF
00796
00693 *    MOVE PI-COMPANY-CD                  TO RECON-COMPANY-CD
00694 *    MOVE CHKNOI                         TO RECON-CHECK-NO
020403*    MOVE 'C'                            TO RECON-ORIGIN
00699 *    MOVE BANKNOI                        TO RECON-BANK-NO
020403     MOVE PI-SAVE-COMPANY-CD (1) TO  RECON-COMPANY-CD
020403     MOVE PI-SAVE-CHECK-NO (1)   TO  RECON-CHECK-NO
020403     MOVE PI-SAVE-ORIGIN (1)     TO  RECON-ORIGIN
020403     MOVE PI-SAVE-BANK-NO (1)    TO  RECON-BANK-NO
00801      MOVE LOW-VALUES             TO  PI-SAVE-KEY
020403                                     EL146AO
020403*    MOVE 'N'                    TO  PI-STOP-PAY-SW
020403     MOVE RC-CHECK-NO            TO  EL146A-TARGET-CHK-NO
020403     MOVE RC-GL-ACCOUNT-NO       TO  EL146A-BANK-NO
020403     MOVE RC-CASHED-AMOUNT       TO  EL146A-TARGET-CSH-AMT
020403     MOVE RC-CHECK-NOTE          TO  EL146A-CHK-NOTE
00804
00805      GO TO 4000-START-BROWSE
020403     .
00806
00807  6000-EXIT.
00808      EXIT.
00809  8000-DEEDIT.
00810      
      * EXEC CICS BIF DEEDIT
00811 *        FIELD    (DEEDIT-FIELD)
00812 *        LENGTH   (12)
00813 *    END-EXEC
           MOVE 12
             TO DFHEIV11
      *    MOVE '@"L                   #   #00002360' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
020403     .
00814
00815  8000-EXIT.
00816      EXIT.
00817      EJECT
00818  8100-SEND-INITIAL-MAP.
00819
00820      MOVE -1                     TO  MAINTL
00821      MOVE EIBTIME                TO  TIME-IN
00822      MOVE SAVE-DATE              TO  DATEO
00823      MOVE TIME-OUT               TO  TIMEO
00824      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O
00825      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O
00826
00827      
      * EXEC CICS SEND
00828 *        FROM   (EL146AI)
00829 *        MAPSET (WS-MAPSET-NAME)
00830 *        MAP    (WS-MAP-NAME)
00831 *        CURSOR
00832 *        ERASE
00833 *    END-EXEC
           MOVE LENGTH OF
            EL146AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00002378' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL146AI, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00834      GO TO 9100-RETURN-TRAN
020403     .
00835
00836  8200-SEND-DATAONLY.
00837
00838      MOVE EIBTIME                TO  TIME-IN
00839      MOVE SAVE-DATE              TO  DATEO
00840      MOVE TIME-OUT               TO  TIMEO
00841      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O
00842      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O
00843
00844      
      * EXEC CICS SEND DATAONLY
00845 *        FROM   (EL146AI)
00846 *        MAPSET (WS-MAPSET-NAME)
00847 *        MAP    (WS-MAP-NAME)
00848 *        CURSOR
00849 *    END-EXEC
           MOVE LENGTH OF
            EL146AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00002396' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL146AI, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00850
00851      GO TO 9100-RETURN-TRAN
020403     .
00852
00853      EJECT
00854  8300-NOT-FOUND.
00855
020403*    MOVE AL-UABON                TO  CHKNOA  ORIGINA.
020403     MOVE AL-UABON                TO  CHKNOA
020403*    MOVE AL-UNBON                TO  CHKAMTA
00858      MOVE -1                      TO  ENTERPFL
00859      MOVE ER-0772                 TO  EMI-ERROR
00860      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00861      GO TO 8200-SEND-DATAONLY
020403     .
00862
00863  8300-EXIT.
00864      EXIT.
00865  8500-DATE-CONVERSION.
00866
00867      
      * EXEC CICS LINK
00868 *        PROGRAM  ('ELDATCV')
00869 *        COMMAREA (DATE-CONVERSION-DATA)
00870 *        LENGTH   (DC-COMM-LENGTH)
00871 *    END-EXEC
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00002422' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
020403     .
00872
00873  8500-EXIT.
00874      EXIT.
00875
00876  8700-END-FILE.
00877      MOVE -1                     TO ENTERPFL
00878      MOVE ER-0130                TO EMI-ERROR
00879      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00880      IF WS-RECORDS-READ-SW IS EQUAL TO 'Y'
00881          GO TO 8100-SEND-INITIAL-MAP
00882      ELSE
00883          GO TO 8200-SEND-DATAONLY
020403     END-IF
020403     .
00884
00885  8700-EXIT.
00886      EXIT.
00887
00888  8710-END-FILE.
00889
00890      MOVE LOW-VALUES             TO PI-SAVE-CHECK-NO (SUB)
00891      MOVE -1                     TO ENTERPFL
00892      MOVE ER-0131                TO EMI-ERROR
00893      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00894
00895      IF WS-RECORDS-READ-SW IS EQUAL TO 'Y'
00896          GO TO 8100-SEND-INITIAL-MAP
00897      ELSE
00898          GO TO 8200-SEND-DATAONLY
020403     END-IF
020403     .
00899
00900  8710-EXIT.
00901      EXIT.
00902  8800-UNAUTHORIZED-ACCESS.
00903
00904      MOVE UNACCESS-MSG           TO  LOGOFF-MSG
00905      GO TO 8850-SEND-TEXT
           .
00906
00907  8850-SEND-TEXT.
00908
00909      
      * EXEC CICS SEND TEXT
00910 *        FROM   (LOGOFF-TEXT)
00911 *        LENGTH (LOGOFF-LENGTH)
00912 *        ERASE  FREEKB
00913 *    END-EXEC
      *    MOVE '8&      T  E F  H   F -   #00002470' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343730' TO DFHEIV0(25:11)
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00914
00915      
      * EXEC CICS RETURN
00916 *    END-EXEC
      *    MOVE '.(                    &   #00002476' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
020403     .
00917
00918  8870-NOTOPEN.
00919
00920      MOVE ER-0776                TO  EMI-ERROR
00921      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00922      MOVE -1                     TO  MAINTL
00923      GO TO 8200-SEND-DATAONLY
020403     .
00924
00925      EJECT
00926  9000-RETURN-CICS.
00927
00928      MOVE XCTL-005               TO  THIS-PGM
00929      MOVE EIBAID                 TO  PI-ENTRY-CD-1
00930      GO TO 9300-XCTL
           .
00931
00932  9100-RETURN-TRAN.
00933
00934      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO
00935      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO
00936
00937      
      * EXEC CICS RETURN
00938 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
00939 *        LENGTH   (PI-COMM-LENGTH)
00940 *        TRANSID  (WS-TRANS-ID)
00941 *    END-EXEC
      *    MOVE '.(CT                  &   #00002501' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
020403     .
00942
00943  9100-EXIT.
00944      EXIT.
00945
00946  9300-XCTL.
00947
00948      MOVE DFHENTER               TO  EIBAID
00949
00950      
      * EXEC CICS XCTL
00951 *        PROGRAM  (THIS-PGM)
00952 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
00953 *        LENGTH   (PI-COMM-LENGTH)
00954 *    END-EXEC
      *    MOVE '.$C                   $   #00002515' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
020403     .
00955
00956  9300-EXIT.
00957      EXIT.
00958
00959      EJECT
00960  9400-CLEAR.
00961
00962      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM
00963      GO TO 9300-XCTL
           .
00964
00965  9600-PGMIDERR.
00966
00967      
      * EXEC CICS HANDLE CONDITION
00968 *        PGMIDERR (8200-SEND-DATAONLY)
00969 *    END-EXEC
      *    MOVE '"$L                   ! & #00002534' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303032353334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00970
00971      MOVE  THIS-PGM              TO  PI-CALLING-PROGRAM
00972                                      LOGOFF-PGM
00973
00974      MOVE XCTL-005               TO  THIS-PGM
00975      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL
00976      MOVE SPACES                 TO  PI-ENTRY-CD-1
00977      GO TO 9300-XCTL
020403     .
00978
00979  9900-ERROR-FORMAT.
00980
00981      MOVE LINK-001               TO  THIS-PGM
00982      
      * EXEC CICS LINK
00983 *        PROGRAM  (THIS-PGM)
00984 *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
00985 *        LENGTH   (EMI-COMM-LENGTH)
00986 *    END-EXEC
      *    MOVE '."C                   ''   #00002550' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
020403     .
00987
00988  9900-EXIT.
00989      EXIT.
00990
00991  9990-ERROR.
00992
00993      MOVE DFHEIBLK               TO  EMI-LINE1
00994      MOVE LINK-004               TO  THIS-PGM
00995      
      * EXEC CICS LINK
00996 *        PROGRAM  (THIS-PGM)
00997 *        COMMAREA (EMI-LINE1)
00998 *        LENGTH   (72)
00999 *    END-EXEC
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00002564' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01000
01001      GO TO 8200-SEND-DATAONLY
020403     .
01002
01003      EJECT
01004  9995-SECURITY-VIOLATION.
01005 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00002592' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353932' TO DFHEIV0(25:11)
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
01006
01007  9995-EXIT.
01008      EXIT.
01009
01010  9999-LAST-PARAGRAPH SECTION.
01011
01012      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL146' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL146' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9990-ERROR,
                     8870-NOTOPEN,
                     9600-PGMIDERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 8700-END-FILE,
                     8300-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8710-END-FILE,
                     8300-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8300-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8200-SEND-DATAONLY
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL146' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
