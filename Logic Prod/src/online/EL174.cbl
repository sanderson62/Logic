00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL174 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/25/96 01:08:24.
00007 *                            VMOD=2.008
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
00025 *        THIS PROGRAM PRODUCES A REPORT SHOWING ALL CHECKS THAT
00026 *    ARE WAITING TO BE PRINTED.
00027
00028 *    SCREENS     - EL174A - REQUEST FOR REVIEW
00029
00030 *    ENTERED BY  - EL171  - REPORT MENU
00031
00032 *    EXIT TO     - EL171  - RESULT OF CLEAR OR END OF JOB
00033
00034
00035      EJECT
00036  ENVIRONMENT DIVISION.
00037
00038  DATA DIVISION.
00039
00040  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00041
00042  77  FILLER  PIC X(32)  VALUE '********************************'.
00043  77  FILLER  PIC X(32)  VALUE '*   EL174  WORKING STORAGE     *'.
00044  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.008 *********'.
00045
00046 *                            COPY ELCSCTM.
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
00047
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
00050  01  WS-DATE-AREA.
00051      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00052      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
00053
00054  01  FILLER                          COMP-3.
00055      05  WS-READNEXT-SW              PIC S9          VALUE ZERO.
00056
00057      05  TIME-IN                     PIC S9(7)       VALUE ZERO.
00058      05  TIME-OUT                    REDEFINES
00059          TIME-IN                     PIC S9(3)V9(4).
00060
00061  01  FILLER                          COMP SYNC.
00062      05  WS-TS-LENGTH                PIC S9(4)       VALUE +1920.
00063      05  SC-ITEM                     PIC S9(4)       VALUE +0001.
00064
00065  01  FILLER.
00066      05  WS-ACTIVITY-TRAILERS-KEY.
00067          10  WS-ATK-COMPANY-CD       PIC X.
00068          10  WS-ATK-CARRIER          PIC X.
00069          10  WS-ATK-CLAIM-NO         PIC X(7).
00070          10  WS-ATK-CERT-NO          PIC X(11).
00071          10  WS-ATK-SEQUENCE-NO      PIC S9(4)
00072                                      COMP.
00073
00074      05  CNTL-KEY.
00075          10  CNTL-CO             PIC X(3).
00076          10  CNTL-RECORD-TYPE    PIC X       VALUE '1'.
00077          10  CNTL-GENL           PIC X(4).
00078          10  CNTL-SEQ            PIC S9(4)   VALUE +0    COMP.
00079
00080      05  WS-START-CNTLNO         PIC S9(8)   VALUE +0    COMP.
00081
00082      05  WS-MAPSET-NAME              PIC X(8)      VALUE 'EL174S'.
00083      05  WS-MAP-NAME                 PIC X(8)      VALUE 'EL174A'.
00084
00085      05  FILLER                      REDEFINES
00086          WS-MAP-NAME.
00087          20  FILLER                  PIC XX.
00088          20  WS-MAP-NUMBER           PIC X(6).
00089
00090      05  THIS-PGM                    PIC X(8)      VALUE 'EL174'.
00091
00092      05  WS-CHECK-QUEUE-DSID        PIC X(8) VALUE 'ELCHKQ'.
00093      05  WS-CHECK-QUEUE-AIX-DSID    PIC X(8) VALUE 'ELCHKQ2'.
00094      05  WS-ACTIVITY-TRAILERS-DSID  PIC X(8) VALUE 'ELTRLR'.
00095      05  WS-CONTROL-DSID            PIC X(8) VALUE 'ELCNTL'.
00096
00097      05  WS-TRANS-ID                 PIC X(4)        VALUE 'EX04'.
00098      05  WS-PRINT-TRAN-ID            PIC X(4)        VALUE 'EX64'.
00099      05  WS-PRINTER-ID               PIC X(4).
00100
00101      05  WS-TEXT-MESSAGE-LENGTH      PIC S9(4)       VALUE +70
00102                                      COMP
00103                                      SYNCHRONIZED.
00104
00105      05  WS-TEXT-MESSAGE             PIC X(70)       VALUE SPACES.
00106
00107      05  WS-SUCCESSFUL-MESSAGE       PIC X(79)       VALUE
00108          '0000 TRANSACTION SUCCESSFUL'.
00109
00110      05  WS-TO-BE-PRINTED-DESC       PIC X(24)       VALUE
00111          '- CHECKS TO BE PRINTED -'.
00112
00113      05  WS-CHECKS-PRINTED-DESC      PIC X(24)       VALUE
00114          '-    PRINTED CHECKS    -'.
00115
00116      05  WS-TO-BE-PRINTED-PFDESC     PIC X(29)       VALUE
00117          'PF4=LIST PRINTED CHECKS      '.
00118
00119      05  WS-CHECKS-PRINTED-PFDESC    PIC X(29)       VALUE
00120          'PF4=LIST TO BE PRINTED CHECKS'.
00121
00122      05  WS-SAVE-CHECK-MODE          PIC X           VALUE SPACE.
00123
00124      05  WS-TEMP-STORAGE-KEY.
00125          10  WS-TS-TERM-ID           PIC X(4).
00126          10  FILLER                  PIC X(4)        VALUE '174'.
00127
00128      05  WS-TEMP-STORAGE-ITEM        PIC S9(4)       VALUE ZERO
00129                                      COMP SYNC.
00130
00131      EJECT
00132  01  ERROR-MESSAGES.
00133      12  ER-0004                 PIC X(4)  VALUE '0004'.
00134      12  ER-0008                 PIC X(4)  VALUE '0008'.
00135      12  ER-0029                 PIC X(4)  VALUE '0029'.
00136      12  ER-0070                 PIC X(4)  VALUE '0070'.
00137      12  ER-0312                 PIC X(4)  VALUE '0312'.
00138      12  ER-0375                 PIC X(4)  VALUE '0375'.
00139      12  ER-0412                 PIC X(4)  VALUE '0412'.
00140      12  ER-0413                 PIC X(4)  VALUE '0413'.
00141      12  ER-0567                 PIC X(4)  VALUE '0567'.
00142      12  ER-2295                 PIC X(4)  VALUE '2295'.
00143      EJECT
00144 *                                COPY ELCINTF.
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
00145      12  FILLER                      REDEFINES
00146          PI-PROGRAM-WORK-AREA.
00147          16  PI-CHECK-AIX-KEY.
00148              20  PI-CK-COMPANY-CODE     PIC X.
00149              20  PI-CK-CONTROL-NO       PIC S9(8)          COMP.
00150              20  PI-CK-CARRIER          PIC X(01).
00151              20  PI-CK-GROUPING         PIC X(06).
00152              20  PI-CK-STATE            PIC X(02).
00153              20  PI-CK-BENE-ACCT        PIC X(10).
00154              20  PI-CK-SEQUENCE-NO      PIC S9(4)          COMP.
00155
00156          16  PI-PREV-CHECK-AIX-KEY.
00157              20  PI-PREV-CK-COMPANY-CODE     PIC X.
00158              20  PI-PREV-CK-CONTROL-NO       PIC S9(8) COMP.
00159              20  PI-PREV-CK-CARRIER          PIC X(01).
00160              20  PI-PREV-CK-GROUPING         PIC X(06).
00161              20  PI-PREV-CK-STATE            PIC X(02).
00162              20  PI-PREV-CK-BENE-ACCT        PIC X(10).
00163              20  PI-PREV-CK-SEQUENCE-NO      PIC S9(4) COMP.
00164
00165          16  PI-TEMP-STORAGE-ITEM    PIC S9(4) COMP SYNC.
00166
00167          16  PI-END-OF-FILE          PIC S9    COMP-3.
00168
00169          16 PI-CONTROL-TOTALS.
00170             20 PI-CONTROL-TOT        PIC S9(7)V99 COMP-3.
00171             20 PI-CONTROL-GRAND-TOT  PIC S9(7)V99 COMP-3.
00172             20 PI-CONTROL-SAVE-CONTROL
00173                                      PIC S9(8) COMP.
00174          16 PI-FIRST-TIME-SWT        PIC X.
00175             88 PI-FIRST-TIME         VALUE 'Y'.
00176             88 PI-NOT-FIRST-TIME     VALUE 'N'.
00177          16 PI-EOF-SWT               PIC X.
00178             88 PI-EOF                VALUE 'Y'.
00179             88 PI-NOT-EOF            VALUE 'N'.
00180          16 PI-SEND-TOT-SWT          PIC X.
00181             88 PI-SEND-TOT           VALUE 'Y'.
00182             88 PI-NOT-SEND-TOT       VALUE 'N'.
00183          16  PI-CHECK-MODE           PIC X.
00184              88  CHECKS-PRINTED      VALUE 'Y'.
00185              88  CHECKS-TO-BE-PRINTED VALUE SPACE.
00186
00187          16  PI-START-CONTROL-NO     PIC S9(08) COMP.
00188          16  FILLER                  PIC X(563).
00189
00190      EJECT
00191 *                                    COPY EL174S.
       01  EL174AI.
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
           05  TITLEL PIC S9(0004) COMP.
           05  TITLEF PIC  X(0001).
           05  FILLER REDEFINES TITLEF.
               10  TITLEA PIC  X(0001).
           05  TITLEI PIC  X(0024).
      *    -------------------------------
           05  RPTIDL PIC S9(0004) COMP.
           05  RPTIDF PIC  X(0001).
           05  FILLER REDEFINES RPTIDF.
               10  RPTIDA PIC  X(0001).
           05  RPTIDI PIC  X(0006).
      *    -------------------------------
           05  APAGEL PIC S9(0004) COMP.
           05  APAGEF PIC  X(0001).
           05  FILLER REDEFINES APAGEF.
               10  APAGEA PIC  X(0001).
           05  APAGEI PIC  S9(4).
      *    -------------------------------
           05  ACTRL01L PIC S9(0004) COMP.
           05  ACTRL01F PIC  X(0001).
           05  FILLER REDEFINES ACTRL01F.
               10  ACTRL01A PIC  X(0001).
           05  ACTRL01I PIC  X(0007).
      *    -------------------------------
           05  ACKNO01L PIC S9(0004) COMP.
           05  ACKNO01F PIC  X(0001).
           05  FILLER REDEFINES ACKNO01F.
               10  ACKNO01A PIC  X(0001).
           05  ACKNO01I PIC  X(0007).
      *    -------------------------------
           05  APTYP01L PIC S9(0004) COMP.
           05  APTYP01F PIC  X(0001).
           05  FILLER REDEFINES APTYP01F.
               10  APTYP01A PIC  X(0001).
           05  APTYP01I PIC  X(0011).
      *    -------------------------------
           05  ACLNO01L PIC S9(0004) COMP.
           05  ACLNO01F PIC  X(0001).
           05  FILLER REDEFINES ACLNO01F.
               10  ACLNO01A PIC  X(0001).
           05  ACLNO01I PIC  X(0007).
      *    -------------------------------
           05  ACARR01L PIC S9(0004) COMP.
           05  ACARR01F PIC  X(0001).
           05  FILLER REDEFINES ACARR01F.
               10  ACARR01A PIC  X(0001).
           05  ACARR01I PIC  X(0001).
      *    -------------------------------
           05  ACERT01L PIC S9(0004) COMP.
           05  ACERT01F PIC  X(0001).
           05  FILLER REDEFINES ACERT01F.
               10  ACERT01A PIC  X(0001).
           05  ACERT01I PIC  X(0011).
      *    -------------------------------
           05  APAMT01L PIC S9(0004) COMP.
           05  APAMT01F PIC  X(0001).
           05  FILLER REDEFINES APAMT01F.
               10  APAMT01A PIC  X(0001).
           05  APAMT01I PIC  X(0013).
      *    -------------------------------
           05  APAYE01L PIC S9(0004) COMP.
           05  APAYE01F PIC  X(0001).
           05  FILLER REDEFINES APAYE01F.
               10  APAYE01A PIC  X(0001).
           05  APAYE01I PIC  X(0007).
      *    -------------------------------
           05  ABY01L PIC S9(0004) COMP.
           05  ABY01F PIC  X(0001).
           05  FILLER REDEFINES ABY01F.
               10  ABY01A PIC  X(0001).
           05  ABY01I PIC  X(0004).
      *    -------------------------------
           05  ACTRL02L PIC S9(0004) COMP.
           05  ACTRL02F PIC  X(0001).
           05  FILLER REDEFINES ACTRL02F.
               10  ACTRL02A PIC  X(0001).
           05  ACTRL02I PIC  X(0007).
      *    -------------------------------
           05  ACKNO02L PIC S9(0004) COMP.
           05  ACKNO02F PIC  X(0001).
           05  FILLER REDEFINES ACKNO02F.
               10  ACKNO02A PIC  X(0001).
           05  ACKNO02I PIC  X(0007).
      *    -------------------------------
           05  APTYP02L PIC S9(0004) COMP.
           05  APTYP02F PIC  X(0001).
           05  FILLER REDEFINES APTYP02F.
               10  APTYP02A PIC  X(0001).
           05  APTYP02I PIC  X(0011).
      *    -------------------------------
           05  ACLNO02L PIC S9(0004) COMP.
           05  ACLNO02F PIC  X(0001).
           05  FILLER REDEFINES ACLNO02F.
               10  ACLNO02A PIC  X(0001).
           05  ACLNO02I PIC  X(0007).
      *    -------------------------------
           05  ACARR02L PIC S9(0004) COMP.
           05  ACARR02F PIC  X(0001).
           05  FILLER REDEFINES ACARR02F.
               10  ACARR02A PIC  X(0001).
           05  ACARR02I PIC  X(0001).
      *    -------------------------------
           05  ACERT02L PIC S9(0004) COMP.
           05  ACERT02F PIC  X(0001).
           05  FILLER REDEFINES ACERT02F.
               10  ACERT02A PIC  X(0001).
           05  ACERT02I PIC  X(0011).
      *    -------------------------------
           05  APAMT02L PIC S9(0004) COMP.
           05  APAMT02F PIC  X(0001).
           05  FILLER REDEFINES APAMT02F.
               10  APAMT02A PIC  X(0001).
           05  APAMT02I PIC  X(0013).
      *    -------------------------------
           05  APAYE02L PIC S9(0004) COMP.
           05  APAYE02F PIC  X(0001).
           05  FILLER REDEFINES APAYE02F.
               10  APAYE02A PIC  X(0001).
           05  APAYE02I PIC  X(0007).
      *    -------------------------------
           05  ABY02L PIC S9(0004) COMP.
           05  ABY02F PIC  X(0001).
           05  FILLER REDEFINES ABY02F.
               10  ABY02A PIC  X(0001).
           05  ABY02I PIC  X(0004).
      *    -------------------------------
           05  ACTRL03L PIC S9(0004) COMP.
           05  ACTRL03F PIC  X(0001).
           05  FILLER REDEFINES ACTRL03F.
               10  ACTRL03A PIC  X(0001).
           05  ACTRL03I PIC  X(0007).
      *    -------------------------------
           05  ACKNO03L PIC S9(0004) COMP.
           05  ACKNO03F PIC  X(0001).
           05  FILLER REDEFINES ACKNO03F.
               10  ACKNO03A PIC  X(0001).
           05  ACKNO03I PIC  X(0007).
      *    -------------------------------
           05  APTYP03L PIC S9(0004) COMP.
           05  APTYP03F PIC  X(0001).
           05  FILLER REDEFINES APTYP03F.
               10  APTYP03A PIC  X(0001).
           05  APTYP03I PIC  X(0011).
      *    -------------------------------
           05  ACLNO03L PIC S9(0004) COMP.
           05  ACLNO03F PIC  X(0001).
           05  FILLER REDEFINES ACLNO03F.
               10  ACLNO03A PIC  X(0001).
           05  ACLNO03I PIC  X(0007).
      *    -------------------------------
           05  ACARR03L PIC S9(0004) COMP.
           05  ACARR03F PIC  X(0001).
           05  FILLER REDEFINES ACARR03F.
               10  ACARR03A PIC  X(0001).
           05  ACARR03I PIC  X(0001).
      *    -------------------------------
           05  ACERT03L PIC S9(0004) COMP.
           05  ACERT03F PIC  X(0001).
           05  FILLER REDEFINES ACERT03F.
               10  ACERT03A PIC  X(0001).
           05  ACERT03I PIC  X(0011).
      *    -------------------------------
           05  APAMT03L PIC S9(0004) COMP.
           05  APAMT03F PIC  X(0001).
           05  FILLER REDEFINES APAMT03F.
               10  APAMT03A PIC  X(0001).
           05  APAMT03I PIC  X(0013).
      *    -------------------------------
           05  APAYE03L PIC S9(0004) COMP.
           05  APAYE03F PIC  X(0001).
           05  FILLER REDEFINES APAYE03F.
               10  APAYE03A PIC  X(0001).
           05  APAYE03I PIC  X(0007).
      *    -------------------------------
           05  ABY03L PIC S9(0004) COMP.
           05  ABY03F PIC  X(0001).
           05  FILLER REDEFINES ABY03F.
               10  ABY03A PIC  X(0001).
           05  ABY03I PIC  X(0004).
      *    -------------------------------
           05  ACTRL04L PIC S9(0004) COMP.
           05  ACTRL04F PIC  X(0001).
           05  FILLER REDEFINES ACTRL04F.
               10  ACTRL04A PIC  X(0001).
           05  ACTRL04I PIC  X(0007).
      *    -------------------------------
           05  ACKNO04L PIC S9(0004) COMP.
           05  ACKNO04F PIC  X(0001).
           05  FILLER REDEFINES ACKNO04F.
               10  ACKNO04A PIC  X(0001).
           05  ACKNO04I PIC  X(0007).
      *    -------------------------------
           05  APTYP04L PIC S9(0004) COMP.
           05  APTYP04F PIC  X(0001).
           05  FILLER REDEFINES APTYP04F.
               10  APTYP04A PIC  X(0001).
           05  APTYP04I PIC  X(0011).
      *    -------------------------------
           05  ACLNO04L PIC S9(0004) COMP.
           05  ACLNO04F PIC  X(0001).
           05  FILLER REDEFINES ACLNO04F.
               10  ACLNO04A PIC  X(0001).
           05  ACLNO04I PIC  X(0007).
      *    -------------------------------
           05  ACARR04L PIC S9(0004) COMP.
           05  ACARR04F PIC  X(0001).
           05  FILLER REDEFINES ACARR04F.
               10  ACARR04A PIC  X(0001).
           05  ACARR04I PIC  X(0001).
      *    -------------------------------
           05  ACERT04L PIC S9(0004) COMP.
           05  ACERT04F PIC  X(0001).
           05  FILLER REDEFINES ACERT04F.
               10  ACERT04A PIC  X(0001).
           05  ACERT04I PIC  X(0011).
      *    -------------------------------
           05  APAMT04L PIC S9(0004) COMP.
           05  APAMT04F PIC  X(0001).
           05  FILLER REDEFINES APAMT04F.
               10  APAMT04A PIC  X(0001).
           05  APAMT04I PIC  X(0013).
      *    -------------------------------
           05  APAYE04L PIC S9(0004) COMP.
           05  APAYE04F PIC  X(0001).
           05  FILLER REDEFINES APAYE04F.
               10  APAYE04A PIC  X(0001).
           05  APAYE04I PIC  X(0007).
      *    -------------------------------
           05  ABY04L PIC S9(0004) COMP.
           05  ABY04F PIC  X(0001).
           05  FILLER REDEFINES ABY04F.
               10  ABY04A PIC  X(0001).
           05  ABY04I PIC  X(0004).
      *    -------------------------------
           05  ACTRL05L PIC S9(0004) COMP.
           05  ACTRL05F PIC  X(0001).
           05  FILLER REDEFINES ACTRL05F.
               10  ACTRL05A PIC  X(0001).
           05  ACTRL05I PIC  X(0007).
      *    -------------------------------
           05  ACKNO05L PIC S9(0004) COMP.
           05  ACKNO05F PIC  X(0001).
           05  FILLER REDEFINES ACKNO05F.
               10  ACKNO05A PIC  X(0001).
           05  ACKNO05I PIC  X(0007).
      *    -------------------------------
           05  APTYP05L PIC S9(0004) COMP.
           05  APTYP05F PIC  X(0001).
           05  FILLER REDEFINES APTYP05F.
               10  APTYP05A PIC  X(0001).
           05  APTYP05I PIC  X(0011).
      *    -------------------------------
           05  ACLNO05L PIC S9(0004) COMP.
           05  ACLNO05F PIC  X(0001).
           05  FILLER REDEFINES ACLNO05F.
               10  ACLNO05A PIC  X(0001).
           05  ACLNO05I PIC  X(0007).
      *    -------------------------------
           05  ACARR05L PIC S9(0004) COMP.
           05  ACARR05F PIC  X(0001).
           05  FILLER REDEFINES ACARR05F.
               10  ACARR05A PIC  X(0001).
           05  ACARR05I PIC  X(0001).
      *    -------------------------------
           05  ACERT05L PIC S9(0004) COMP.
           05  ACERT05F PIC  X(0001).
           05  FILLER REDEFINES ACERT05F.
               10  ACERT05A PIC  X(0001).
           05  ACERT05I PIC  X(0011).
      *    -------------------------------
           05  APAMT05L PIC S9(0004) COMP.
           05  APAMT05F PIC  X(0001).
           05  FILLER REDEFINES APAMT05F.
               10  APAMT05A PIC  X(0001).
           05  APAMT05I PIC  X(0013).
      *    -------------------------------
           05  APAYE05L PIC S9(0004) COMP.
           05  APAYE05F PIC  X(0001).
           05  FILLER REDEFINES APAYE05F.
               10  APAYE05A PIC  X(0001).
           05  APAYE05I PIC  X(0007).
      *    -------------------------------
           05  ABY05L PIC S9(0004) COMP.
           05  ABY05F PIC  X(0001).
           05  FILLER REDEFINES ABY05F.
               10  ABY05A PIC  X(0001).
           05  ABY05I PIC  X(0004).
      *    -------------------------------
           05  ACTRL06L PIC S9(0004) COMP.
           05  ACTRL06F PIC  X(0001).
           05  FILLER REDEFINES ACTRL06F.
               10  ACTRL06A PIC  X(0001).
           05  ACTRL06I PIC  X(0007).
      *    -------------------------------
           05  ACKNO06L PIC S9(0004) COMP.
           05  ACKNO06F PIC  X(0001).
           05  FILLER REDEFINES ACKNO06F.
               10  ACKNO06A PIC  X(0001).
           05  ACKNO06I PIC  X(0007).
      *    -------------------------------
           05  APTYP06L PIC S9(0004) COMP.
           05  APTYP06F PIC  X(0001).
           05  FILLER REDEFINES APTYP06F.
               10  APTYP06A PIC  X(0001).
           05  APTYP06I PIC  X(0011).
      *    -------------------------------
           05  ACLNO06L PIC S9(0004) COMP.
           05  ACLNO06F PIC  X(0001).
           05  FILLER REDEFINES ACLNO06F.
               10  ACLNO06A PIC  X(0001).
           05  ACLNO06I PIC  X(0007).
      *    -------------------------------
           05  ACARR06L PIC S9(0004) COMP.
           05  ACARR06F PIC  X(0001).
           05  FILLER REDEFINES ACARR06F.
               10  ACARR06A PIC  X(0001).
           05  ACARR06I PIC  X(0001).
      *    -------------------------------
           05  ACERT06L PIC S9(0004) COMP.
           05  ACERT06F PIC  X(0001).
           05  FILLER REDEFINES ACERT06F.
               10  ACERT06A PIC  X(0001).
           05  ACERT06I PIC  X(0011).
      *    -------------------------------
           05  APAMT06L PIC S9(0004) COMP.
           05  APAMT06F PIC  X(0001).
           05  FILLER REDEFINES APAMT06F.
               10  APAMT06A PIC  X(0001).
           05  APAMT06I PIC  X(0013).
      *    -------------------------------
           05  APAYE06L PIC S9(0004) COMP.
           05  APAYE06F PIC  X(0001).
           05  FILLER REDEFINES APAYE06F.
               10  APAYE06A PIC  X(0001).
           05  APAYE06I PIC  X(0007).
      *    -------------------------------
           05  ABY06L PIC S9(0004) COMP.
           05  ABY06F PIC  X(0001).
           05  FILLER REDEFINES ABY06F.
               10  ABY06A PIC  X(0001).
           05  ABY06I PIC  X(0004).
      *    -------------------------------
           05  ACTRL07L PIC S9(0004) COMP.
           05  ACTRL07F PIC  X(0001).
           05  FILLER REDEFINES ACTRL07F.
               10  ACTRL07A PIC  X(0001).
           05  ACTRL07I PIC  X(0007).
      *    -------------------------------
           05  ACKNO07L PIC S9(0004) COMP.
           05  ACKNO07F PIC  X(0001).
           05  FILLER REDEFINES ACKNO07F.
               10  ACKNO07A PIC  X(0001).
           05  ACKNO07I PIC  X(0007).
      *    -------------------------------
           05  APTYP07L PIC S9(0004) COMP.
           05  APTYP07F PIC  X(0001).
           05  FILLER REDEFINES APTYP07F.
               10  APTYP07A PIC  X(0001).
           05  APTYP07I PIC  X(0011).
      *    -------------------------------
           05  ACLNO07L PIC S9(0004) COMP.
           05  ACLNO07F PIC  X(0001).
           05  FILLER REDEFINES ACLNO07F.
               10  ACLNO07A PIC  X(0001).
           05  ACLNO07I PIC  X(0007).
      *    -------------------------------
           05  ACARR07L PIC S9(0004) COMP.
           05  ACARR07F PIC  X(0001).
           05  FILLER REDEFINES ACARR07F.
               10  ACARR07A PIC  X(0001).
           05  ACARR07I PIC  X(0001).
      *    -------------------------------
           05  ACERT07L PIC S9(0004) COMP.
           05  ACERT07F PIC  X(0001).
           05  FILLER REDEFINES ACERT07F.
               10  ACERT07A PIC  X(0001).
           05  ACERT07I PIC  X(0011).
      *    -------------------------------
           05  APAMT07L PIC S9(0004) COMP.
           05  APAMT07F PIC  X(0001).
           05  FILLER REDEFINES APAMT07F.
               10  APAMT07A PIC  X(0001).
           05  APAMT07I PIC  X(0013).
      *    -------------------------------
           05  APAYE07L PIC S9(0004) COMP.
           05  APAYE07F PIC  X(0001).
           05  FILLER REDEFINES APAYE07F.
               10  APAYE07A PIC  X(0001).
           05  APAYE07I PIC  X(0007).
      *    -------------------------------
           05  ABY07L PIC S9(0004) COMP.
           05  ABY07F PIC  X(0001).
           05  FILLER REDEFINES ABY07F.
               10  ABY07A PIC  X(0001).
           05  ABY07I PIC  X(0004).
      *    -------------------------------
           05  ACTRL08L PIC S9(0004) COMP.
           05  ACTRL08F PIC  X(0001).
           05  FILLER REDEFINES ACTRL08F.
               10  ACTRL08A PIC  X(0001).
           05  ACTRL08I PIC  X(0007).
      *    -------------------------------
           05  ACKNO08L PIC S9(0004) COMP.
           05  ACKNO08F PIC  X(0001).
           05  FILLER REDEFINES ACKNO08F.
               10  ACKNO08A PIC  X(0001).
           05  ACKNO08I PIC  X(0007).
      *    -------------------------------
           05  APTYP08L PIC S9(0004) COMP.
           05  APTYP08F PIC  X(0001).
           05  FILLER REDEFINES APTYP08F.
               10  APTYP08A PIC  X(0001).
           05  APTYP08I PIC  X(0011).
      *    -------------------------------
           05  ACLNO08L PIC S9(0004) COMP.
           05  ACLNO08F PIC  X(0001).
           05  FILLER REDEFINES ACLNO08F.
               10  ACLNO08A PIC  X(0001).
           05  ACLNO08I PIC  X(0007).
      *    -------------------------------
           05  ACARR08L PIC S9(0004) COMP.
           05  ACARR08F PIC  X(0001).
           05  FILLER REDEFINES ACARR08F.
               10  ACARR08A PIC  X(0001).
           05  ACARR08I PIC  X(0001).
      *    -------------------------------
           05  ACERT08L PIC S9(0004) COMP.
           05  ACERT08F PIC  X(0001).
           05  FILLER REDEFINES ACERT08F.
               10  ACERT08A PIC  X(0001).
           05  ACERT08I PIC  X(0011).
      *    -------------------------------
           05  APAMT08L PIC S9(0004) COMP.
           05  APAMT08F PIC  X(0001).
           05  FILLER REDEFINES APAMT08F.
               10  APAMT08A PIC  X(0001).
           05  APAMT08I PIC  X(0013).
      *    -------------------------------
           05  APAYE08L PIC S9(0004) COMP.
           05  APAYE08F PIC  X(0001).
           05  FILLER REDEFINES APAYE08F.
               10  APAYE08A PIC  X(0001).
           05  APAYE08I PIC  X(0007).
      *    -------------------------------
           05  ABY08L PIC S9(0004) COMP.
           05  ABY08F PIC  X(0001).
           05  FILLER REDEFINES ABY08F.
               10  ABY08A PIC  X(0001).
           05  ABY08I PIC  X(0004).
      *    -------------------------------
           05  ACTRL09L PIC S9(0004) COMP.
           05  ACTRL09F PIC  X(0001).
           05  FILLER REDEFINES ACTRL09F.
               10  ACTRL09A PIC  X(0001).
           05  ACTRL09I PIC  X(0007).
      *    -------------------------------
           05  ACKNO09L PIC S9(0004) COMP.
           05  ACKNO09F PIC  X(0001).
           05  FILLER REDEFINES ACKNO09F.
               10  ACKNO09A PIC  X(0001).
           05  ACKNO09I PIC  X(0007).
      *    -------------------------------
           05  APTYP09L PIC S9(0004) COMP.
           05  APTYP09F PIC  X(0001).
           05  FILLER REDEFINES APTYP09F.
               10  APTYP09A PIC  X(0001).
           05  APTYP09I PIC  X(0011).
      *    -------------------------------
           05  ACLNO09L PIC S9(0004) COMP.
           05  ACLNO09F PIC  X(0001).
           05  FILLER REDEFINES ACLNO09F.
               10  ACLNO09A PIC  X(0001).
           05  ACLNO09I PIC  X(0007).
      *    -------------------------------
           05  ACARR09L PIC S9(0004) COMP.
           05  ACARR09F PIC  X(0001).
           05  FILLER REDEFINES ACARR09F.
               10  ACARR09A PIC  X(0001).
           05  ACARR09I PIC  X(0001).
      *    -------------------------------
           05  ACERT09L PIC S9(0004) COMP.
           05  ACERT09F PIC  X(0001).
           05  FILLER REDEFINES ACERT09F.
               10  ACERT09A PIC  X(0001).
           05  ACERT09I PIC  X(0011).
      *    -------------------------------
           05  APAMT09L PIC S9(0004) COMP.
           05  APAMT09F PIC  X(0001).
           05  FILLER REDEFINES APAMT09F.
               10  APAMT09A PIC  X(0001).
           05  APAMT09I PIC  X(0013).
      *    -------------------------------
           05  APAYE09L PIC S9(0004) COMP.
           05  APAYE09F PIC  X(0001).
           05  FILLER REDEFINES APAYE09F.
               10  APAYE09A PIC  X(0001).
           05  APAYE09I PIC  X(0007).
      *    -------------------------------
           05  ABY09L PIC S9(0004) COMP.
           05  ABY09F PIC  X(0001).
           05  FILLER REDEFINES ABY09F.
               10  ABY09A PIC  X(0001).
           05  ABY09I PIC  X(0004).
      *    -------------------------------
           05  ACTRL10L PIC S9(0004) COMP.
           05  ACTRL10F PIC  X(0001).
           05  FILLER REDEFINES ACTRL10F.
               10  ACTRL10A PIC  X(0001).
           05  ACTRL10I PIC  X(0007).
      *    -------------------------------
           05  ACKNO10L PIC S9(0004) COMP.
           05  ACKNO10F PIC  X(0001).
           05  FILLER REDEFINES ACKNO10F.
               10  ACKNO10A PIC  X(0001).
           05  ACKNO10I PIC  X(0007).
      *    -------------------------------
           05  APTYP10L PIC S9(0004) COMP.
           05  APTYP10F PIC  X(0001).
           05  FILLER REDEFINES APTYP10F.
               10  APTYP10A PIC  X(0001).
           05  APTYP10I PIC  X(0011).
      *    -------------------------------
           05  ACLNO10L PIC S9(0004) COMP.
           05  ACLNO10F PIC  X(0001).
           05  FILLER REDEFINES ACLNO10F.
               10  ACLNO10A PIC  X(0001).
           05  ACLNO10I PIC  X(0007).
      *    -------------------------------
           05  ACARR10L PIC S9(0004) COMP.
           05  ACARR10F PIC  X(0001).
           05  FILLER REDEFINES ACARR10F.
               10  ACARR10A PIC  X(0001).
           05  ACARR10I PIC  X(0001).
      *    -------------------------------
           05  ACERT10L PIC S9(0004) COMP.
           05  ACERT10F PIC  X(0001).
           05  FILLER REDEFINES ACERT10F.
               10  ACERT10A PIC  X(0001).
           05  ACERT10I PIC  X(0011).
      *    -------------------------------
           05  APAMT10L PIC S9(0004) COMP.
           05  APAMT10F PIC  X(0001).
           05  FILLER REDEFINES APAMT10F.
               10  APAMT10A PIC  X(0001).
           05  APAMT10I PIC  X(0013).
      *    -------------------------------
           05  APAYE10L PIC S9(0004) COMP.
           05  APAYE10F PIC  X(0001).
           05  FILLER REDEFINES APAYE10F.
               10  APAYE10A PIC  X(0001).
           05  APAYE10I PIC  X(0007).
      *    -------------------------------
           05  ABY10L PIC S9(0004) COMP.
           05  ABY10F PIC  X(0001).
           05  FILLER REDEFINES ABY10F.
               10  ABY10A PIC  X(0001).
           05  ABY10I PIC  X(0004).
      *    -------------------------------
           05  ACTRL11L PIC S9(0004) COMP.
           05  ACTRL11F PIC  X(0001).
           05  FILLER REDEFINES ACTRL11F.
               10  ACTRL11A PIC  X(0001).
           05  ACTRL11I PIC  X(0007).
      *    -------------------------------
           05  ACKNO11L PIC S9(0004) COMP.
           05  ACKNO11F PIC  X(0001).
           05  FILLER REDEFINES ACKNO11F.
               10  ACKNO11A PIC  X(0001).
           05  ACKNO11I PIC  X(0007).
      *    -------------------------------
           05  APTYP11L PIC S9(0004) COMP.
           05  APTYP11F PIC  X(0001).
           05  FILLER REDEFINES APTYP11F.
               10  APTYP11A PIC  X(0001).
           05  APTYP11I PIC  X(0011).
      *    -------------------------------
           05  ACLNO11L PIC S9(0004) COMP.
           05  ACLNO11F PIC  X(0001).
           05  FILLER REDEFINES ACLNO11F.
               10  ACLNO11A PIC  X(0001).
           05  ACLNO11I PIC  X(0007).
      *    -------------------------------
           05  ACARR11L PIC S9(0004) COMP.
           05  ACARR11F PIC  X(0001).
           05  FILLER REDEFINES ACARR11F.
               10  ACARR11A PIC  X(0001).
           05  ACARR11I PIC  X(0001).
      *    -------------------------------
           05  ACERT11L PIC S9(0004) COMP.
           05  ACERT11F PIC  X(0001).
           05  FILLER REDEFINES ACERT11F.
               10  ACERT11A PIC  X(0001).
           05  ACERT11I PIC  X(0011).
      *    -------------------------------
           05  APAMT11L PIC S9(0004) COMP.
           05  APAMT11F PIC  X(0001).
           05  FILLER REDEFINES APAMT11F.
               10  APAMT11A PIC  X(0001).
           05  APAMT11I PIC  X(0013).
      *    -------------------------------
           05  APAYE11L PIC S9(0004) COMP.
           05  APAYE11F PIC  X(0001).
           05  FILLER REDEFINES APAYE11F.
               10  APAYE11A PIC  X(0001).
           05  APAYE11I PIC  X(0007).
      *    -------------------------------
           05  ABY11L PIC S9(0004) COMP.
           05  ABY11F PIC  X(0001).
           05  FILLER REDEFINES ABY11F.
               10  ABY11A PIC  X(0001).
           05  ABY11I PIC  X(0004).
      *    -------------------------------
           05  ACTRL12L PIC S9(0004) COMP.
           05  ACTRL12F PIC  X(0001).
           05  FILLER REDEFINES ACTRL12F.
               10  ACTRL12A PIC  X(0001).
           05  ACTRL12I PIC  X(0007).
      *    -------------------------------
           05  ACKNO12L PIC S9(0004) COMP.
           05  ACKNO12F PIC  X(0001).
           05  FILLER REDEFINES ACKNO12F.
               10  ACKNO12A PIC  X(0001).
           05  ACKNO12I PIC  X(0007).
      *    -------------------------------
           05  APTYP12L PIC S9(0004) COMP.
           05  APTYP12F PIC  X(0001).
           05  FILLER REDEFINES APTYP12F.
               10  APTYP12A PIC  X(0001).
           05  APTYP12I PIC  X(0011).
      *    -------------------------------
           05  ACLNO12L PIC S9(0004) COMP.
           05  ACLNO12F PIC  X(0001).
           05  FILLER REDEFINES ACLNO12F.
               10  ACLNO12A PIC  X(0001).
           05  ACLNO12I PIC  X(0007).
      *    -------------------------------
           05  ACARR12L PIC S9(0004) COMP.
           05  ACARR12F PIC  X(0001).
           05  FILLER REDEFINES ACARR12F.
               10  ACARR12A PIC  X(0001).
           05  ACARR12I PIC  X(0001).
      *    -------------------------------
           05  ACERT12L PIC S9(0004) COMP.
           05  ACERT12F PIC  X(0001).
           05  FILLER REDEFINES ACERT12F.
               10  ACERT12A PIC  X(0001).
           05  ACERT12I PIC  X(0011).
      *    -------------------------------
           05  APAMT12L PIC S9(0004) COMP.
           05  APAMT12F PIC  X(0001).
           05  FILLER REDEFINES APAMT12F.
               10  APAMT12A PIC  X(0001).
           05  APAMT12I PIC  X(0013).
      *    -------------------------------
           05  APAYE12L PIC S9(0004) COMP.
           05  APAYE12F PIC  X(0001).
           05  FILLER REDEFINES APAYE12F.
               10  APAYE12A PIC  X(0001).
           05  APAYE12I PIC  X(0007).
      *    -------------------------------
           05  ABY12L PIC S9(0004) COMP.
           05  ABY12F PIC  X(0001).
           05  FILLER REDEFINES ABY12F.
               10  ABY12A PIC  X(0001).
           05  ABY12I PIC  X(0004).
      *    -------------------------------
           05  ACTRL13L PIC S9(0004) COMP.
           05  ACTRL13F PIC  X(0001).
           05  FILLER REDEFINES ACTRL13F.
               10  ACTRL13A PIC  X(0001).
           05  ACTRL13I PIC  X(0007).
      *    -------------------------------
           05  ACKNO13L PIC S9(0004) COMP.
           05  ACKNO13F PIC  X(0001).
           05  FILLER REDEFINES ACKNO13F.
               10  ACKNO13A PIC  X(0001).
           05  ACKNO13I PIC  X(0007).
      *    -------------------------------
           05  APTYP13L PIC S9(0004) COMP.
           05  APTYP13F PIC  X(0001).
           05  FILLER REDEFINES APTYP13F.
               10  APTYP13A PIC  X(0001).
           05  APTYP13I PIC  X(0011).
      *    -------------------------------
           05  ACLNO13L PIC S9(0004) COMP.
           05  ACLNO13F PIC  X(0001).
           05  FILLER REDEFINES ACLNO13F.
               10  ACLNO13A PIC  X(0001).
           05  ACLNO13I PIC  X(0007).
      *    -------------------------------
           05  ACARR13L PIC S9(0004) COMP.
           05  ACARR13F PIC  X(0001).
           05  FILLER REDEFINES ACARR13F.
               10  ACARR13A PIC  X(0001).
           05  ACARR13I PIC  X(0001).
      *    -------------------------------
           05  ACERT13L PIC S9(0004) COMP.
           05  ACERT13F PIC  X(0001).
           05  FILLER REDEFINES ACERT13F.
               10  ACERT13A PIC  X(0001).
           05  ACERT13I PIC  X(0011).
      *    -------------------------------
           05  APAMT13L PIC S9(0004) COMP.
           05  APAMT13F PIC  X(0001).
           05  FILLER REDEFINES APAMT13F.
               10  APAMT13A PIC  X(0001).
           05  APAMT13I PIC  X(0013).
      *    -------------------------------
           05  APAYE13L PIC S9(0004) COMP.
           05  APAYE13F PIC  X(0001).
           05  FILLER REDEFINES APAYE13F.
               10  APAYE13A PIC  X(0001).
           05  APAYE13I PIC  X(0007).
      *    -------------------------------
           05  ABY13L PIC S9(0004) COMP.
           05  ABY13F PIC  X(0001).
           05  FILLER REDEFINES ABY13F.
               10  ABY13A PIC  X(0001).
           05  ABY13I PIC  X(0004).
      *    -------------------------------
           05  ACTRL14L PIC S9(0004) COMP.
           05  ACTRL14F PIC  X(0001).
           05  FILLER REDEFINES ACTRL14F.
               10  ACTRL14A PIC  X(0001).
           05  ACTRL14I PIC  X(0007).
      *    -------------------------------
           05  ACKNO14L PIC S9(0004) COMP.
           05  ACKNO14F PIC  X(0001).
           05  FILLER REDEFINES ACKNO14F.
               10  ACKNO14A PIC  X(0001).
           05  ACKNO14I PIC  X(0007).
      *    -------------------------------
           05  APTYP14L PIC S9(0004) COMP.
           05  APTYP14F PIC  X(0001).
           05  FILLER REDEFINES APTYP14F.
               10  APTYP14A PIC  X(0001).
           05  APTYP14I PIC  X(0011).
      *    -------------------------------
           05  ACLNO14L PIC S9(0004) COMP.
           05  ACLNO14F PIC  X(0001).
           05  FILLER REDEFINES ACLNO14F.
               10  ACLNO14A PIC  X(0001).
           05  ACLNO14I PIC  X(0007).
      *    -------------------------------
           05  ACARR14L PIC S9(0004) COMP.
           05  ACARR14F PIC  X(0001).
           05  FILLER REDEFINES ACARR14F.
               10  ACARR14A PIC  X(0001).
           05  ACARR14I PIC  X(0001).
      *    -------------------------------
           05  ACERT14L PIC S9(0004) COMP.
           05  ACERT14F PIC  X(0001).
           05  FILLER REDEFINES ACERT14F.
               10  ACERT14A PIC  X(0001).
           05  ACERT14I PIC  X(0011).
      *    -------------------------------
           05  APAMT14L PIC S9(0004) COMP.
           05  APAMT14F PIC  X(0001).
           05  FILLER REDEFINES APAMT14F.
               10  APAMT14A PIC  X(0001).
           05  APAMT14I PIC  X(0013).
      *    -------------------------------
           05  APAYE14L PIC S9(0004) COMP.
           05  APAYE14F PIC  X(0001).
           05  FILLER REDEFINES APAYE14F.
               10  APAYE14A PIC  X(0001).
           05  APAYE14I PIC  X(0007).
      *    -------------------------------
           05  ABY14L PIC S9(0004) COMP.
           05  ABY14F PIC  X(0001).
           05  FILLER REDEFINES ABY14F.
               10  ABY14A PIC  X(0001).
           05  ABY14I PIC  X(0004).
      *    -------------------------------
           05  ACTRL15L PIC S9(0004) COMP.
           05  ACTRL15F PIC  X(0001).
           05  FILLER REDEFINES ACTRL15F.
               10  ACTRL15A PIC  X(0001).
           05  ACTRL15I PIC  X(0007).
      *    -------------------------------
           05  ACKNO15L PIC S9(0004) COMP.
           05  ACKNO15F PIC  X(0001).
           05  FILLER REDEFINES ACKNO15F.
               10  ACKNO15A PIC  X(0001).
           05  ACKNO15I PIC  X(0007).
      *    -------------------------------
           05  APTYP15L PIC S9(0004) COMP.
           05  APTYP15F PIC  X(0001).
           05  FILLER REDEFINES APTYP15F.
               10  APTYP15A PIC  X(0001).
           05  APTYP15I PIC  X(0011).
      *    -------------------------------
           05  ACLNO15L PIC S9(0004) COMP.
           05  ACLNO15F PIC  X(0001).
           05  FILLER REDEFINES ACLNO15F.
               10  ACLNO15A PIC  X(0001).
           05  ACLNO15I PIC  X(0007).
      *    -------------------------------
           05  ACARR15L PIC S9(0004) COMP.
           05  ACARR15F PIC  X(0001).
           05  FILLER REDEFINES ACARR15F.
               10  ACARR15A PIC  X(0001).
           05  ACARR15I PIC  X(0001).
      *    -------------------------------
           05  ACERT15L PIC S9(0004) COMP.
           05  ACERT15F PIC  X(0001).
           05  FILLER REDEFINES ACERT15F.
               10  ACERT15A PIC  X(0001).
           05  ACERT15I PIC  X(0011).
      *    -------------------------------
           05  APAMT15L PIC S9(0004) COMP.
           05  APAMT15F PIC  X(0001).
           05  FILLER REDEFINES APAMT15F.
               10  APAMT15A PIC  X(0001).
           05  APAMT15I PIC  X(0013).
      *    -------------------------------
           05  APAYE15L PIC S9(0004) COMP.
           05  APAYE15F PIC  X(0001).
           05  FILLER REDEFINES APAYE15F.
               10  APAYE15A PIC  X(0001).
           05  APAYE15I PIC  X(0007).
      *    -------------------------------
           05  ABY15L PIC S9(0004) COMP.
           05  ABY15F PIC  X(0001).
           05  FILLER REDEFINES ABY15F.
               10  ABY15A PIC  X(0001).
           05  ABY15I PIC  X(0004).
      *    -------------------------------
           05  ACTRL16L PIC S9(0004) COMP.
           05  ACTRL16F PIC  X(0001).
           05  FILLER REDEFINES ACTRL16F.
               10  ACTRL16A PIC  X(0001).
           05  ACTRL16I PIC  X(0007).
      *    -------------------------------
           05  ACKNO16L PIC S9(0004) COMP.
           05  ACKNO16F PIC  X(0001).
           05  FILLER REDEFINES ACKNO16F.
               10  ACKNO16A PIC  X(0001).
           05  ACKNO16I PIC  X(0007).
      *    -------------------------------
           05  APTYP16L PIC S9(0004) COMP.
           05  APTYP16F PIC  X(0001).
           05  FILLER REDEFINES APTYP16F.
               10  APTYP16A PIC  X(0001).
           05  APTYP16I PIC  X(0011).
      *    -------------------------------
           05  ACLNO16L PIC S9(0004) COMP.
           05  ACLNO16F PIC  X(0001).
           05  FILLER REDEFINES ACLNO16F.
               10  ACLNO16A PIC  X(0001).
           05  ACLNO16I PIC  X(0007).
      *    -------------------------------
           05  ACARR16L PIC S9(0004) COMP.
           05  ACARR16F PIC  X(0001).
           05  FILLER REDEFINES ACARR16F.
               10  ACARR16A PIC  X(0001).
           05  ACARR16I PIC  X(0001).
      *    -------------------------------
           05  ACERT16L PIC S9(0004) COMP.
           05  ACERT16F PIC  X(0001).
           05  FILLER REDEFINES ACERT16F.
               10  ACERT16A PIC  X(0001).
           05  ACERT16I PIC  X(0011).
      *    -------------------------------
           05  APAMT16L PIC S9(0004) COMP.
           05  APAMT16F PIC  X(0001).
           05  FILLER REDEFINES APAMT16F.
               10  APAMT16A PIC  X(0001).
           05  APAMT16I PIC  X(0013).
      *    -------------------------------
           05  APAYE16L PIC S9(0004) COMP.
           05  APAYE16F PIC  X(0001).
           05  FILLER REDEFINES APAYE16F.
               10  APAYE16A PIC  X(0001).
           05  APAYE16I PIC  X(0007).
      *    -------------------------------
           05  ABY16L PIC S9(0004) COMP.
           05  ABY16F PIC  X(0001).
           05  FILLER REDEFINES ABY16F.
               10  ABY16A PIC  X(0001).
           05  ABY16I PIC  X(0004).
      *    -------------------------------
           05  ACTRL17L PIC S9(0004) COMP.
           05  ACTRL17F PIC  X(0001).
           05  FILLER REDEFINES ACTRL17F.
               10  ACTRL17A PIC  X(0001).
           05  ACTRL17I PIC  X(0007).
      *    -------------------------------
           05  ACKNO17L PIC S9(0004) COMP.
           05  ACKNO17F PIC  X(0001).
           05  FILLER REDEFINES ACKNO17F.
               10  ACKNO17A PIC  X(0001).
           05  ACKNO17I PIC  X(0007).
      *    -------------------------------
           05  APTYP17L PIC S9(0004) COMP.
           05  APTYP17F PIC  X(0001).
           05  FILLER REDEFINES APTYP17F.
               10  APTYP17A PIC  X(0001).
           05  APTYP17I PIC  X(0011).
      *    -------------------------------
           05  ACLNO17L PIC S9(0004) COMP.
           05  ACLNO17F PIC  X(0001).
           05  FILLER REDEFINES ACLNO17F.
               10  ACLNO17A PIC  X(0001).
           05  ACLNO17I PIC  X(0007).
      *    -------------------------------
           05  ACARR17L PIC S9(0004) COMP.
           05  ACARR17F PIC  X(0001).
           05  FILLER REDEFINES ACARR17F.
               10  ACARR17A PIC  X(0001).
           05  ACARR17I PIC  X(0001).
      *    -------------------------------
           05  ACERT17L PIC S9(0004) COMP.
           05  ACERT17F PIC  X(0001).
           05  FILLER REDEFINES ACERT17F.
               10  ACERT17A PIC  X(0001).
           05  ACERT17I PIC  X(0011).
      *    -------------------------------
           05  APAMT17L PIC S9(0004) COMP.
           05  APAMT17F PIC  X(0001).
           05  FILLER REDEFINES APAMT17F.
               10  APAMT17A PIC  X(0001).
           05  APAMT17I PIC  X(0013).
      *    -------------------------------
           05  APAYE17L PIC S9(0004) COMP.
           05  APAYE17F PIC  X(0001).
           05  FILLER REDEFINES APAYE17F.
               10  APAYE17A PIC  X(0001).
           05  APAYE17I PIC  X(0007).
      *    -------------------------------
           05  ABY17L PIC S9(0004) COMP.
           05  ABY17F PIC  X(0001).
           05  FILLER REDEFINES ABY17F.
               10  ABY17A PIC  X(0001).
           05  ABY17I PIC  X(0004).
      *    -------------------------------
           05  ACTRL18L PIC S9(0004) COMP.
           05  ACTRL18F PIC  X(0001).
           05  FILLER REDEFINES ACTRL18F.
               10  ACTRL18A PIC  X(0001).
           05  ACTRL18I PIC  X(0007).
      *    -------------------------------
           05  ACKNO18L PIC S9(0004) COMP.
           05  ACKNO18F PIC  X(0001).
           05  FILLER REDEFINES ACKNO18F.
               10  ACKNO18A PIC  X(0001).
           05  ACKNO18I PIC  X(0007).
      *    -------------------------------
           05  APTYP18L PIC S9(0004) COMP.
           05  APTYP18F PIC  X(0001).
           05  FILLER REDEFINES APTYP18F.
               10  APTYP18A PIC  X(0001).
           05  APTYP18I PIC  X(0011).
      *    -------------------------------
           05  ACLNO18L PIC S9(0004) COMP.
           05  ACLNO18F PIC  X(0001).
           05  FILLER REDEFINES ACLNO18F.
               10  ACLNO18A PIC  X(0001).
           05  ACLNO18I PIC  X(0007).
      *    -------------------------------
           05  ACARR18L PIC S9(0004) COMP.
           05  ACARR18F PIC  X(0001).
           05  FILLER REDEFINES ACARR18F.
               10  ACARR18A PIC  X(0001).
           05  ACARR18I PIC  X(0001).
      *    -------------------------------
           05  ACERT18L PIC S9(0004) COMP.
           05  ACERT18F PIC  X(0001).
           05  FILLER REDEFINES ACERT18F.
               10  ACERT18A PIC  X(0001).
           05  ACERT18I PIC  X(0011).
      *    -------------------------------
           05  APAMT18L PIC S9(0004) COMP.
           05  APAMT18F PIC  X(0001).
           05  FILLER REDEFINES APAMT18F.
               10  APAMT18A PIC  X(0001).
           05  APAMT18I PIC  X(0013).
      *    -------------------------------
           05  APAYE18L PIC S9(0004) COMP.
           05  APAYE18F PIC  X(0001).
           05  FILLER REDEFINES APAYE18F.
               10  APAYE18A PIC  X(0001).
           05  APAYE18I PIC  X(0007).
      *    -------------------------------
           05  ABY18L PIC S9(0004) COMP.
           05  ABY18F PIC  X(0001).
           05  FILLER REDEFINES ABY18F.
               10  ABY18A PIC  X(0001).
           05  ABY18I PIC  X(0004).
      *    -------------------------------
           05  AEMSG1L PIC S9(0004) COMP.
           05  AEMSG1F PIC  X(0001).
           05  FILLER REDEFINES AEMSG1F.
               10  AEMSG1A PIC  X(0001).
           05  AEMSG1I PIC  X(0079).
      *    -------------------------------
           05  APFKL PIC S9(0004) COMP.
           05  APFKF PIC  X(0001).
           05  FILLER REDEFINES APFKF.
               10  APFKA PIC  X(0001).
           05  APFKI PIC  9(2).
      *    -------------------------------
           05  PRINTERL PIC S9(0004) COMP.
           05  PRINTERF PIC  X(0001).
           05  FILLER REDEFINES PRINTERF.
               10  PRINTERA PIC  X(0001).
           05  PRINTERI PIC  X(0004).
      *    -------------------------------
           05  PFDESCL PIC S9(0004) COMP.
           05  PFDESCF PIC  X(0001).
           05  FILLER REDEFINES PFDESCF.
               10  PFDESCA PIC  X(0001).
           05  PFDESCI PIC  X(0029).
      *    -------------------------------
           05  CNTLNODL PIC S9(0004) COMP.
           05  CNTLNODF PIC  X(0001).
           05  FILLER REDEFINES CNTLNODF.
               10  CNTLNODA PIC  X(0001).
           05  CNTLNODI PIC  X(0011).
      *    -------------------------------
           05  CNTLNOL PIC S9(0004) COMP.
           05  CNTLNOF PIC  X(0001).
           05  FILLER REDEFINES CNTLNOF.
               10  CNTLNOA PIC  X(0001).
           05  CNTLNOI PIC  X(0007).
       01  EL174AO REDEFINES EL174AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TITLEO PIC  X(0024).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTIDO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAGEO PIC  9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL01O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO01O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APTYP01O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO01O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT01O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT01O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAYE01O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY01O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL02O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO02O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APTYP02O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO02O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT02O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT02O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAYE02O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY02O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL03O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO03O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APTYP03O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO03O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT03O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT03O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAYE03O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY03O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL04O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO04O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APTYP04O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO04O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT04O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT04O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAYE04O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY04O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL05O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO05O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APTYP05O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO05O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT05O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT05O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAYE05O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY05O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL06O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO06O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APTYP06O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO06O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT06O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT06O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAYE06O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY06O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL07O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO07O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APTYP07O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO07O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT07O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT07O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAYE07O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY07O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL08O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO08O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APTYP08O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO08O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT08O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT08O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAYE08O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY08O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL09O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO09O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APTYP09O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO09O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT09O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT09O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAYE09O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY09O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL10O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO10O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APTYP10O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO10O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT10O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT10O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAYE10O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY10O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL11O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO11O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APTYP11O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO11O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT11O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT11O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAYE11O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY11O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL12O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO12O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APTYP12O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO12O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT12O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT12O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAYE12O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY12O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL13O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO13O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APTYP13O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO13O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT13O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT13O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAYE13O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY13O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL14O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO14O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APTYP14O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO14O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT14O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT14O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAYE14O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY14O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL15O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO15O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APTYP15O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO15O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT15O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT15O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAYE15O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY15O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL16O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO16O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APTYP16O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO16O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT16O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT16O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAYE16O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY16O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL17O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO17O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APTYP17O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO17O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR17O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT17O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT17O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAYE17O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY17O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL18O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO18O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APTYP18O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO18O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR18O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT18O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT18O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAYE18O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY18O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFKO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRINTERO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFDESCO PIC  X(0029).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNTLNODO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNTLNOO PIC  X(0007).
      *    -------------------------------
00192
00193  01  FILLER                          REDEFINES
00194      EL174AI.
00195      05  FILLER                      PIC X(74).
00196
00197      05  FILLER                      OCCURS 18 TIMES
00198                                      INDEXED BY EL174A-INDEX.
00199
00200          15  EL174A-CONTROL-LENGTH   PIC S9(4)
00201                                      COMP.
00202          15  EL174A-CONTROL-ATTRB    PIC X.
00203          15  EL174A-CONTROL          PIC 9(7).
00204
00205          15  EL174A-CHECK-NO-LENGTH  PIC S9(4)
00206                                      COMP.
00207          15  EL174A-CHECK-NO-ATTRB   PIC X.
00208          15  EL174A-CHECK-NO         PIC X(7).
00209
00210          15  EL174A-PMT-TYPE-LENGTH  PIC S9(4)
00211                                      COMP.
00212          15  EL174A-PMT-TYPE-ATTRB   PIC X.
00213          15  EL174A-PMT-TYPE         PIC X(11).
00214
00215          15  EL174A-CLAIM-NO-LENGTH  PIC S9(4)
00216                                      COMP.
00217          15  EL174A-CLAIM-NO-ATTRB   PIC X.
00218          15  EL174A-CLAIM-NO         PIC X(7).
00219
00220          15  EL174A-CARRIER-LENGTH   PIC S9(4)
00221                                      COMP.
00222          15  EL174A-CARRIER-ATTRB    PIC X.
00223          15  EL174A-CARRIER          PIC X.
00224
00225          15  EL174A-CERT-NO-LENGTH   PIC S9(4)
00226                                      COMP.
00227          15  EL174A-CERT-NO-ATTRB    PIC X.
00228          15  EL174A-CERT-NO          PIC X(11).
00229
00230          15  EL174A-AMT-LENGTH       PIC S9(4)
00231                                      COMP.
00232          15  EL174A-AMT-ATTRB        PIC X.
00233          15  EL174A-AMT              PIC Z,ZZZ,ZZ9.99-.
00234
00235          15  EL174A-PAYEE-LENGTH     PIC S9(4)
00236                                      COMP.
00237          15  EL174A-PAYEE-ATTRB      PIC X.
00238          15  EL174A-PAYEE            PIC X(7).
00239
00240          15  EL174A-BY-LENGTH        PIC S9(4)
00241                                      COMP.
00242          15  EL174A-BY-ATTRB         PIC X.
00243          15  EL174A-BY               PIC X(4).
00244
00245      EJECT
00246 *                                COPY ELCEMIB.
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
00247      EJECT
00248 *                                COPY ELCDATE.
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
00249      EJECT
00250 *                                COPY ELCLOGOF.
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
00251      EJECT
00252 *                                COPY ELCATTR.
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
00253      EJECT
00254 *                                COPY ELCAID.
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
00255
00256  01  FILLER                      REDEFINES
00257      DFHAID.
00258
00259      05  FILLER                      PIC X(8).
00260
00261      05  PF-VALUES                   PIC X
00262          OCCURS 24 TIMES.
00263      EJECT
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
00265
00266  01  DFHCOMMAREA                     PIC X(1024).
00267
00268 *                                COPY ELCCHKQ.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCHKQ.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.007                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK QUE FILE                            *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 100  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCHKQ                         RKP=2,LEN=7    *
00013 *       ALTERNATE PATH1 = ELCHKQ2 (BY PAYEE)      RKP=9,LEN=26   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  CHECK-QUE.
00019      12  CQ-RECORD-ID                PIC XX.
00020          88  VALID-CQ-ID         VALUE 'CQ'.
00021
00022      12  CQ-CONTROL-PRIMARY.
00023          16  CQ-COMPANY-CD           PIC X.
00024          16  CQ-CONTROL-NUMBER       PIC S9(8)       COMP.
00025          16  CQ-SEQUENCE-NUMBER      PIC S9(4)       COMP.
00026
00027      12  CQ-CONTROL-BY-PAYEE.
DJNA           16  CQ-CONTROL-BY-NUMBER.
DJNA               20  CQ-COMPANY-CD-A1     PIC X.
DJNA               20  CQ-CONTROL-NUMBER-A1 PIC S9(8)      COMP.
00030          16  CQ-PAYEE-CARRIER        PIC X.
00031          16  CQ-PAYEE-GROUPING       PIC X(6).
00032          16  CQ-PAYEE-STATE          PIC XX.
00033          16  CQ-PAYEE-BENE-ACCT      PIC X(10).
00034          16  CQ-SEQUENCE-NUMBER-A1   PIC S9(4)       COMP.
00035
00036      12  CQ-DMD-CONTROL  REDEFINES  CQ-CONTROL-BY-PAYEE.
00037          16  CQ-DMD-COMPANY-CD-A2    PIC X.
00038          16  CQ-DMD-PAYEE-TYPE-A2    PIC X.
00039          16  CQ-DMD-BENE-CODE-A2     PIC X(10).
00040          16  CQ-DMD-CLAIM-NO-A2      PIC X(7).
00041          16  CQ-DMD-TIME-SEQ-A2      PIC S9(7)       COMP.
00042          16  FILLER                  PIC X(3).
00043
00044      12  CQ-ENTRY-TYPE               PIC X.
00045              88  CHECK-ON-QUE           VALUE 'Q'.
00046              88  ALIGNMENT-CHECK        VALUE 'A'.
00047              88  SPOILED-CHECK          VALUE 'S'.
00048              88  PAYMENT-ABORTED        VALUE 'X'.
00049
00050      12  CQ-CLAIM-MAST-CNTL.
00051          16  CQ-CARRIER              PIC X.
00052          16  CQ-CLAIM-NO             PIC X(7).
00053          16  CQ-CERT-NO.
00054              20  CQ-CERT-PRIME       PIC X(10).
00055              20  CQ-CERT-SFX         PIC X.
00056          16  CQ-CLAIM-TYPE           PIC X.
00057              88  CQ-LIFE-CLAIM          VALUE 'L'.
00058              88  CQ-AH-CLAIM            VALUE 'A'.
00059          16  CQ-CLAIM-SUB-TYPE       PIC X.
00060              88  CQ-FIXED-COVERAGE      VALUE '1'.
00061              88  CQ-O-B-COVERAGE        VALUE '2'.
00062              88  CQ-OPEN-END-COVERAGE   VALUE '3'.
00063
00064      12  CQ-PMT-TRLR-SEQUENCE        PIC S9(4)       COMP.
00065      12  CQ-CHECK-NUMBER             PIC X(7).
00066      12  CQ-CHECK-AMOUNT             PIC S9(7)V99    COMP-3.
00067      12  CQ-PAYMENT-TYPE             PIC X.
00068              88  CQ-PARTIAL-PAYMENT        VALUE '1'.
00069              88  CQ-FINAL-PAYMENT          VALUE '2'.
00070              88  CQ-LUMP-SUM-PAYMENT       VALUE '3'.
00071              88  CQ-ADDITIONAL-PAYMENT     VALUE '4'.
00072              88  CQ-CHARGEABLE-EXPENSE     VALUE '5'.
00073              88  CQ-NON-CHARGEABLE-EXPENSE VALUE '6'.
00074              88  CQ-LIFE-PREMIUM-REFUND    VALUE '7'.
00075              88  CQ-AH-PREMIUM-REFUND      VALUE '8'.
00076      12  CQ-VOID-INDICATOR           PIC X.
00077              88  CHECK-IS-STOPPED          VALUE 'S'.
00078              88  CHECK-IS-VOID             VALUE 'V'.
00079      12  CQ-TIMES-PRINTED            PIC S9(4)       COMP.
00080      12  CQ-PRINT-AT-HHMM            PIC S9(4)       COMP.
00081      12  CQ-CHECK-BY-USER            PIC X(4).
00082      12  CQ-PRE-NUMBERING-SW         PIC X.
00083        88  CHECKS-WERE-NOT-PRE-NUMBERED    VALUE SPACE.
00084        88  CHECKS-WERE-PRE-NUMBERED        VALUE '1'.
00085
00086      12  CQ-CHECK-WRITTEN-DT         PIC XX.
00087      12  CQ-LAST-UPDATED-BY          PIC S9(4)       COMP.
00088      12  CQ-LEDGER-FLAG              PIC X(01).
00089      12  CQ-VOID-AFTER-LEDGER        PIC X(01).
00090      12  CQ-LAST-UPDATED-DT          PIC XX.
00091      12  CQ-LAST-UPDATED-HHMMSS      PIC S9(6)       COMP-3.
00092      12  CQ-APPLIED-TO-RCON-DT       PIC XX.
00093
00094      12  FILLER                      PIC X(04).
00095
00096 ******************************************************************
00269      EJECT
00270 *                                COPY ELCTRLR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCTRLR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.014                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ACTIVITY TRAILER FILE                     *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 200    RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELTRLR             RKP=2,LEN=22          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
120503******************************************************************
120503*                   C H A N G E   L O G
120503*
120503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120503*-----------------------------------------------------------------
120503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120503* EFFECTIVE    NUMBER
120503*-----------------------------------------------------------------
120503* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
022106* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST
050506* 050506    2006030600001  AJRA  ADD DENIAL PROOF DATE
062806* 062806    2006030600001  AJRA  ADD PAYMENT PROOF DATE
080106* 080106    2006052500001  AJRA  ADD N AND R NOTE TYPES
041807* 041807    2006032200004  AJRA  ADD APPROVED BY TO PAYMENT
082807* 082807    2007032100001  PEMA  ADD INT RATE TO PMT TRLR
101807* 101807  IR2007100100007  PEMA  EXPAND SIZE OF CLM RESERVE FLDS
070909* 070909    2009060400001  AJRA  ADD AUTO PAY END LETTER
040110* 040110  CR2009070600002  AJRA  ADD RESEND LETTER ID TO LETTER
071910* 071910  CR2009122800001  PEMA  ADD EOB SWITCHES
102610* 102610    2009122800001  AJRA  ADD STOP DATE TO LETTER
00017 ******************************************************************
00018  01  ACTIVITY-TRAILERS.
00019      12  AT-RECORD-ID                    PIC XX.
00020          88  VALID-AT-ID                       VALUE 'AT'.
00021
00022      12  AT-CONTROL-PRIMARY.
00023          16  AT-COMPANY-CD               PIC X.
00024          16  AT-CARRIER                  PIC X.
00025          16  AT-CLAIM-NO                 PIC X(7).
00026          16  AT-CERT-NO.
00027              20  AT-CERT-PRIME           PIC X(10).
00028              20  AT-CERT-SFX             PIC X.
00029          16  AT-SEQUENCE-NO              PIC S9(4)     COMP.
00030              88  AT-1ST-TRL-AVAIL             VALUE +4095.
00031              88  AT-LAST-TRL-AVAIL            VALUE +100.
00032              88  AT-RESV-EXP-HIST-TRL         VALUE +0.
00033              88  AT-INSURED-ADDR-TRL          VALUE +1 THRU +9.
00034              88  AT-BENEFICIARY-ADDR-TRL      VALUE +11 THRU +19.
00035              88  AT-ACCOUNT-ADDR-TRL          VALUE +21 THRU +29.
00036              88  AT-PHYSICIAN-ADDR-TRL        VALUE +31 THRU +39.
00037              88  AT-EMPLOYERS-ADDR-TRL        VALUE +41 THRU +49.
00038              88  AT-OTHER-1-ADDR-TRL          VALUE +51 THRU +59.
00039              88  AT-OTHER-2-ADDR-TRL          VALUE +61 THRU +69.
00040              88  AT-DIAGNOSIS-TRL             VALUE +90.
022106             88  AT-BENEFICIARY-TRL           VALUE +91.
022106             88  AT-SPECIAL-REVIEW-TRL        VALUE +92.
00041
00042      12  AT-TRAILER-TYPE                 PIC X.
00043          88  RESERVE-EXPENSE-TR               VALUE '1'.
00044          88  PAYMENT-TR                       VALUE '2'.
00045          88  AUTO-PAY-TR                      VALUE '3'.
00046          88  CORRESPONDENCE-TR                VALUE '4'.
00047          88  ADDRESS-TR                       VALUE '5'.
00048          88  GENERAL-INFO-TR                  VALUE '6'.
00049          88  AUTO-PROMPT-TR                   VALUE '7'.
00050          88  DENIAL-TR                        VALUE '8'.
00051          88  INCURRED-CHG-TR                  VALUE '9'.
00052          88  FORM-CONTROL-TR                  VALUE 'A'.
00053
00054      12  AT-RECORDED-DT                  PIC XX.
00055      12  AT-RECORDED-BY                  PIC X(4).
00056      12  AT-LAST-MAINT-HHMMSS            PIC S9(6)     COMP-3.
00057
00058      12  AT-TRAILER-BODY                 PIC X(165).
00059
00060      12  AT-RESERVE-EXPENSE-TR  REDEFINES  AT-TRAILER-BODY.
00061          16  AT-RESERVE-CONTROLS.
00062              20  AT-MANUAL-SW            PIC X.
00063                  88  AT-MANUAL-RESERVES-USED VALUE '1'.
00064              20  AT-FUTURE-SW            PIC X.
00065                  88  AT-FUTURE-RESERVES-USED VALUE '1'.
00066              20  AT-PTC-SW               PIC X.
00067                  88  AT-PAY-TO-CURRENT-USED  VALUE '1'.
00068              20  AT-IBNR-SW              PIC X.
00069                  88  AT-IBNR-RESERVES-USED   VALUE '1'.
00070              20  AT-PTC-LF-SW            PIC X.
00071                  88  AT-LF-PTC-USED          VALUE '1'.
00072              20  AT-CDT-ACCESS-METHOD    PIC X.
00073                  88  AT-CDT-ROUND-NEAR       VALUE '1'.
00074                  88  AT-CDT-ROUND-HIGH       VALUE '2'.
00075                  88  AT-CDT-INTERPOLATED     VALUE '3'.
00076              20  AT-PERCENT-OF-CDT       PIC S9(3)V99    COMP-3.
00077          16  AT-LAST-COMPUTED-DT         PIC XX.
101807         16  AT-FUTURE-RESERVE           PIC S9(7)V99    COMP-3.
101807         16  AT-PAY-CURRENT-RESERVE      PIC S9(7)V99    COMP-3.
101807         16  AT-IBNR-RESERVE             PIC S9(7)V99    COMP-3.
101807         16  AT-INITIAL-MANUAL-RESERVE   PIC S9(7)V99    COMP-3.
101807         16  AT-CURRENT-MANUAL-RESERVE   PIC S9(7)V99    COMP-3.
101807         16  AT-ITD-ADDITIONAL-RESERVE   PIC S9(7)V99    COMP-3.
00084          16  AT-EXPENSE-CONTROLS.
00085              20  AT-EXPENSE-METHOD       PIC X.
00086                  88  NO-EXPENSE-CALCULATED    VALUE '1'.
00087                  88  FLAT-DOLLAR-PER-PMT      VALUE '2'.
00088                  88  PERCENT-OF-PMT           VALUE '3'.
00089                  88  DOLLAR-PER-OPEN-MONTH    VALUE '4'.
00090              20  AT-EXPENSE-PERCENT      PIC S9(3)V99    COMP-3.
00091              20  AT-EXPENSE-DOLLAR       PIC S9(3)V99    COMP-3.
00092          16  AT-ITD-PAID-EXPENSES        PIC S9(5)V99    COMP-3.
00093          16  AT-ITD-CHARGEABLE-EXPENSE   PIC S9(5)V99    COMP-3.
00094
00095          16  AT-ITD-LIFE-REFUNDS         PIC S9(5)V99    COMP-3.
00096          16  AT-ITD-AH-REFUNDS           PIC S9(5)V99    COMP-3.
00097
101807*        16  FILLER                      PIC X(53).
101807         16  FILLER                      PIC X(47).
00099
00100          16  AT-RESERVES-LAST-MAINT-DT   PIC XX.
00101          16  AT-RESERVES-LAST-UPDATED-BY PIC X(4).
00102
00103          16  AT-OPEN-CLOSE-HISTORY OCCURS 6 TIMES.
00104              20  AT-OPEN-CLOSE-DATE      PIC XX.
00105              20  AT-OPEN-CLOSE-TYPE      PIC X.
00106 *                    C = CLOSED
00107 *                    O = OPEN
00108              20  AT-OPEN-CLOSE-REASON    PIC X(5).
00109 *                   REASONS = ALTER, AUTO, FINAL, NEW, FORCE
00110
00111      12  AT-PAYMENT-TR  REDEFINES  AT-TRAILER-BODY.
00112          16  AT-PAYMENT-TYPE             PIC X.
00113              88  PARTIAL-PAYMENT                VALUE '1'.
00114              88  FINAL-PAYMENT                  VALUE '2'.
00115              88  LUMP-SUM-PAYMENT               VALUE '3'.
00116              88  ADDITIONAL-PAYMENT             VALUE '4'.
00117              88  CHARGEABLE-EXPENSE             VALUE '5'.
00118              88  NON-CHARGEABLE-EXPENSE         VALUE '6'.
00119              88  VOIDED-PAYMENT                 VALUE '9'.
00120              88  TRANSFER                       VALUE 'T'.
022106             88  LIFE-INTEREST                  VALUE 'I'.
00121
00122          16  AT-CLAIM-TYPE               PIC X.
00123              88  PAID-FOR-AH                    VALUE 'A'.
00124              88  PAID-FOR-LIFE                  VALUE 'L'.
00124              88  PAID-FOR-IUI                   VALUE 'I'.
120503             88  PAID-FOR-GAP                   VALUE 'G'.
00125          16  AT-CLAIM-PREM-TYPE          PIC X.
00126              88  AT-SINGLE-PREMIUM              VALUE '1'.
00127              88  AT-O-B-COVERAGE                VALUE '2'.
00128              88  AT-OPEN-END-COVERAGE           VALUE '3'.
00129          16  AT-AMOUNT-PAID              PIC S9(7)V99  COMP-3.
00130          16  AT-CHECK-NO                 PIC X(7).
00131          16  AT-PAID-FROM-DT             PIC XX.
00132          16  AT-PAID-THRU-DT             PIC XX.
00133          16  AT-DAYS-IN-PERIOD           PIC S9(4)     COMP.
00134          16  FILLER                      PIC X.
00135          16  AT-PAYEES-NAME              PIC X(30).
00136          16  AT-PAYMENT-ORIGIN           PIC X.
00137              88  ONLINE-MANUAL-PMT              VALUE '1'.
00138              88  ONLINE-AUTO-PMT                VALUE '2'.
00139              88  OFFLINE-PMT                    VALUE '3'.
00140          16  AT-CHECK-WRITTEN-DT         PIC XX.
00141          16  AT-TO-BE-WRITTEN-DT         PIC XX.
00142          16  AT-VOID-DATA.
00143              20  AT-VOID-DT              PIC XX.
041807*00144       20  AT-VOID-REASON          PIC X(30).
041807             20  AT-VOID-REASON          PIC X(26).
041807         16  AT-PMT-APPROVED-BY          PIC X(04).
00145          16  AT-ADDL-RESERVE             PIC S9(5)V99  COMP-3.
00146          16  AT-EXPENSE-PER-PMT          PIC S9(5)V99  COMP-3.
082807         16  AT-INT-RATE REDEFINES AT-EXPENSE-PER-PMT
082807                                         PIC S99V9(5)  COMP-3.
00147          16  AT-CREDIT-INTERFACE.
00148              20  AT-PMT-SELECT-DT        PIC XX.
00149                  88  PAYMENT-NOT-SELECTED  VALUE LOW-VALUE.
00150              20  AT-PMT-ACCEPT-DT        PIC XX.
00151                  88  PAYMENT-NOT-ACCEPTED  VALUE LOW-VALUE.
00152              20  AT-VOID-SELECT-DT       PIC XX.
00153                  88  VOID-NOT-SELECTED     VALUE LOW-VALUE.
00154              20  AT-VOID-ACCEPT-DT       PIC XX.
00155                  88  VOID-NOT-ACCEPTED     VALUE LOW-VALUE.
00156
00157          16  AT-CHECK-QUE-CONTROL        PIC S9(8)     COMP.
00158                  88  PAYMENT-NOT-QUEUED           VALUE ZERO.
00159                  88  CONVERSION-PAYMENT           VALUE +99999999.
00160          16  AT-CHECK-QUE-SEQUENCE       PIC S9(4)     COMP.
00161
00162          16  AT-FORCE-CONTROL            PIC X.
00163              88  PAYMENT-WAS-FORCED           VALUE '1'.
00164          16  AT-PREV-LAST-PMT-DT         PIC XX.
00165          16  AT-PREV-PAID-THRU-DT        PIC XX.
00166          16  AT-PREV-LAST-PMT-AMT        PIC S9(7)V99  COMP-3.
00167          16  AT-ELIMINATION-DAYS         PIC S999      COMP-3.
00168          16  AT-DAILY-RATE               PIC S9(3)V99  COMP-3.
00169          16  AT-BENEFIT-TYPE             PIC X.
00170
00171          16  AT-EXPENSE-TYPE             PIC X.
00172          16  AT-PAYMENT-APPROVAL-SW      PIC X.
00173
00174          16  AT-PAYEE-TYPE-CD.
00175              20  AT-PAYEE-TYPE           PIC X.
00176                  88  INSURED-PAID           VALUE 'I'.
00177                  88  BENEFICIARY-PAID       VALUE 'B'.
00178                  88  ACCOUNT-PAID           VALUE 'A'.
00179                  88  OTHER-1-PAID           VALUE 'O'.
00180                  88  OTHER-2-PAID           VALUE 'Q'.
00181                  88  DOCTOR-PAID            VALUE 'P'.
00182                  88  EMPLOYER-PAID          VALUE 'E'.
00183              20  AT-PAYEE-SEQ            PIC X.
00184
00185          16  AT-CASH-PAYMENT             PIC X.
00186          16  AT-GROUPED-PAYMENT          PIC X.
00187          16  AT-PAYMENT-NOTE-SEQ-NO      PIC S9(4)       COMP.
00188          16  AT-APPROVAL-LEVEL-REQD      PIC X.
00189          16  AT-APPROVED-LEVEL           PIC X.
00190          16  AT-VOID-TYPE                PIC X.
00191              88  AT-PAYMENT-WAS-STOPPED     VALUE 'S'.
00192              88  AT-PAYMENT-WAS-VOIDED      VALUE 'V'.
00193          16  AT-AIG-UNEMP-IND            PIC X.
00194              88  AT-AIG-UNEMPLOYMENT-PMT    VALUE 'U'.
00195          16  AT-ASSOCIATES               PIC X.
00196              88  AT-AIG-INTERFACE           VALUE 'I' 'N'.
00197              88  AT-AIG-NON-INTERFACE       VALUE 'A' 'M'.
00198
00199          16  AT-FORM-CTL-SEQ-NO          PIC S9(4)       COMP.
00200          16  AT-CV-PMT-CODE              PIC X.
00201              88  FULL-DEATH-PAYMENT         VALUE '1'.
00202              88  HALF-DEATH-PAYMENT         VALUE '2'.
00203              88  FULL-ADD-PAYMENT           VALUE '3'.
00204              88  HALF-ADD-PAYMENT           VALUE '4'.
00205              88  FULL-RIDER-PAYMENT         VALUE '5'.
00206              88  HALF-RIDER-PAYMENT         VALUE '6'.
00207              88  NON-CHG-EXP-PAYMENT        VALUE '7'.
00208              88  ADDL-PAYMENT               VALUE '8'.
00209
00210          16  AT-EOB-CODE1                PIC XXX.
00211          16  AT-EOB-CODE2                PIC XXX.
00212          16  AT-EOB-CODE3                PIC XXX.
00213          16  AT-EOB-CODE4                PIC XXX.
               16  FILLER REDEFINES AT-EOB-CODE4.
                   20  AT-INT-PMT-SELECT-DT    PIC XX.
                   20  FILLER                  PIC X.
00214          16  AT-EOB-CODE5                PIC XXX.
062806         16  FILLER REDEFINES AT-EOB-CODE5.
062806             20  AT-PMT-PROOF-DT         PIC XX.
062806             20  FILLER                  PIC X.
00215
071910         16  AT-PRINT-EOB-WITH-CHECK     PIC X.
071910             88  AT-PRINT-EOB            VALUE 'Y'.
00217
00218          16  AT-PAYMENT-LAST-MAINT-DT    PIC XX.
00219          16  AT-PAYMENT-LAST-UPDATED-BY  PIC X(4).
00220
00221      12  AT-AUTO-PAY-TR  REDEFINES  AT-TRAILER-BODY.
00222          16  AT-SCHEDULE-START-DT        PIC XX.
00223          16  AT-SCHEDULE-END-DT          PIC XX.
00224          16  AT-TERMINATED-DT            PIC XX.
00225          16  AT-LAST-PMT-TYPE            PIC X.
00226              88  LAST-PMT-IS-FINAL              VALUE 'F'.
00227              88  LAST-PMT-IS-PARTIAL            VALUE 'P'.
00228          16  AT-FIRST-PMT-DATA.
00229              20  AT-FIRST-PMT-AMT        PIC S9(7)V99  COMP-3.
00230              20  AT-DAYS-IN-1ST-PMT      PIC S9(4)     COMP.
00231              20  AT-1ST-PAY-THRU-DT      PIC XX.
00232          16  AT-REGULAR-PMT-DATA.
00233              20  AT-REGULAR-PMT-AMT      PIC S9(7)V99  COMP-3.
00234              20  AT-DAYS-IN-REG-PMT      PIC S9(4)     COMP.
00235              20  AT-INTERVAL-MONTHS      PIC S9(4)     COMP.
00236          16  AT-AUTO-PAYEE-CD.
00237              20  AT-AUTO-PAYEE-TYPE      PIC X.
00238                  88  INSURED-PAID-AUTO      VALUE 'I'.
00239                  88  BENEFICIARY-PAID-AUTO  VALUE 'B'.
00240                  88  ACCOUNT-PAID-AUTO      VALUE 'A'.
00241                  88  OTHER-1-PAID-AUTO      VALUE 'O'.
00242                  88  OTHER-2-PAID-AUTO      VALUE 'Q'.
00243              20  AT-AUTO-PAYEE-SEQ       PIC X.
00244          16  AT-AUTO-PAY-DAY             PIC 99.
00245          16  AT-AUTO-CASH                PIC X.
00246              88  AT-CASH                      VALUE 'Y'.
00247              88  AT-NON-CASH                  VALUE 'N'.
070909*        16  FILLER                      PIC X(129).
070909         16  AT-AUTO-END-LETTER          PIC X(4).
070909         16  FILLER                      PIC X(125).
00249
00250          16  AT-AUTO-PAY-LAST-MAINT-DT   PIC XX.
00251          16  AT-AUTO-PAY-LAST-UPDATED-BY PIC X(4).
00252
00253      12  AT-CORRESPONDENCE-TR  REDEFINES  AT-TRAILER-BODY.
00254          16  AT-LETTER-SENT-DT           PIC XX.
00255          16  AT-RECEIPT-FOLLOW-UP        PIC XX.
00256          16  AT-AUTO-RE-SEND-DT          PIC XX.
00257          16  AT-LETTER-ANSWERED-DT       PIC XX.
00258          16  AT-LETTER-ARCHIVE-NO        PIC S9(8)     COMP.
00259          16  AT-LETTER-ORIGIN            PIC X.
00260              88  ONLINE-CREATION              VALUE '1' '3'.
00261              88  OFFLINE-CREATION             VALUE '2' '4'.
                   88  NAPER-ONLINE-CREATION        VALUE '3'.
                   88  NAPER-OFFLINE-CREATION       VALUE '4'.
00262          16  AT-STD-LETTER-FORM          PIC X(4).
00263          16  AT-REASON-TEXT              PIC X(70).
00264          16  AT-ADDRESS-REC-SEQ-NO       PIC S9(4)     COMP.
00265          16  AT-ADDRESEE-TYPE            PIC X.
00266               88  INSURED-ADDRESEE            VALUE 'I'.
00267               88  BENEFICIARY-ADDRESEE        VALUE 'B'.
00268               88  ACCOUNT-ADDRESEE            VALUE 'A'.
00269               88  PHYSICIAN-ADDRESEE          VALUE 'P'.
00270               88  EMPLOYER-ADDRESEE           VALUE 'E'.
00271               88  OTHER-ADDRESEE-1            VALUE 'O'.
00272               88  OTHER-ADDRESEE-2            VALUE 'Q'.
00273          16  AT-ADDRESSEE-NAME           PIC X(30).
00274          16  AT-INITIAL-PRINT-DATE       PIC XX.
00275          16  AT-RESEND-PRINT-DATE        PIC XX.
00276          16  AT-CORR-SOL-UNSOL           PIC X.
00277          16  AT-LETTER-PURGED-DT         PIC XX.
CIDMOD*
CIDMOD*FOLLOWING CID CHGS REENTERED AS DMD CHGS OVERLAID THEM.
CIDMOD*
CIDMOD         16  AT-CSO-REDEFINITION.
040110             20  AT-RESEND-LETTER-FORM   PIC X(4).
040110             20  AT-AUTO-CLOSE-IND       PIC X(1).
040110             20  AT-LETTER-TO-BENE       PIC X(1).
102610             20  AT-STOP-LETTER-DT       PIC X(2).
102610             20  FILLER                  PIC X(19).
040110*             20  FILLER                  PIC X(27).
CIDMOD             20  AT-CSO-LETTER-STATUS    PIC X.
CIDMOD                 88  AT-CSO-LETTER-ONLINE    VALUE '1'.
CIDMOD                 88  AT-CSO-LETTER-PURGED    VALUE '2'.
CIDMOD                 88  AT-CSO-LETTER-RELOADED  VALUE '3'.
CIDMOD             20  AT-CSO-LETTER-PURGE-DATE   PIC XX.
CIDMOD             20  AT-CSO-LETTER-RELOAD-DATE  PIC XX.
CIDMOD*
CIDMOD*FOLLOWING DMD CHGS COMMENTED OUT AS THEY OVERLAY CID MODS NEEDED
CIDMOD*
CIDMOD*        16  FILLER                      PIC X(26).
CIDMOD*
CIDMOD*        16  AT-DMD-BSR-CODE             PIC X.
CIDMOD*            88  AT-AUTOMATED-BSR              VALUE 'A'.
CIDMOD*            88  AT-NON-AUTOMATED-BSR          VALUE 'B' ' '.
CIDMOD*
CIDMOD*        16  AT-DMD-LETTER-STATUS        PIC X.
CIDMOD*            88  AT-DMD-LETTER-ONLINE          VALUE '1'.
CIDMOD*            88  AT-DMD-LETTER-PURGED          VALUE '2'.
CIDMOD*            88  AT-DMD-LETTER-RELOADED        VALUE '3'.
CIDMOD*        16  AT-DMD-LETTER-PURGE-DT      PIC XX.
CIDMOD*        16  AT-DMD-LETTER-RELOAD-DT     PIC XX.
00290
00291          16  AT-CORR-LAST-MAINT-DT       PIC XX.
00292          16  AT-CORR-LAST-UPDATED-BY     PIC X(4).
00293
00294      12  AT-ADDRESS-TR  REDEFINES  AT-TRAILER-BODY.
00295          16  AT-ADDRESS-TYPE             PIC X.
00296              88  INSURED-ADDRESS               VALUE 'I'.
00297              88  BENEFICIARY-ADDRESS           VALUE 'B'.
00298              88  ACCOUNT-ADDRESS               VALUE 'A'.
00299              88  PHYSICIAN-ADDRESS             VALUE 'P'.
00300              88  EMPLOYER-ADDRESS              VALUE 'E'.
00301              88  OTHER-ADDRESS-1               VALUE 'O'.
00302              88  OTHER-ADDRESS-2               VALUE 'Q'.
00303          16  AT-MAIL-TO-NAME             PIC X(30).
00304          16  AT-ADDRESS-LINE-1           PIC X(30).
00305          16  AT-ADDRESS-LINE-2           PIC X(30).
00306          16  AT-CITY-STATE.
                   20  AT-CITY                 PIC X(28).
                   20  AT-STATE                PIC XX.
00307          16  AT-ZIP.
00308              20  AT-ZIP-CODE.
00309                  24  AT-ZIP-1ST          PIC X.
00310                      88  AT-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00311                  24  FILLER              PIC X(4).
00312              20  AT-ZIP-PLUS4            PIC X(4).
00313          16  AT-CANADIAN-POSTAL-CODE  REDEFINES  AT-ZIP.
00314              20  AT-CAN-POSTAL-1         PIC XXX.
00315              20  AT-CAN-POSTAL-2         PIC XXX.
00316              20  FILLER                  PIC XXX.
00317          16  AT-PHONE-NO                 PIC 9(11)     COMP-3.
00318          16  FILLER                      PIC X(23).
00319          16  AT-ADDRESS-LAST-MAINT-DT    PIC XX.
00320          16  AT-ADDRESS-LAST-UPDATED-BY  PIC X(4).
00321
00322      12  AT-GENERAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.
00323          16  AT-INFO-LINE-1              PIC X(60).
00324          16  AT-INFO-LINE-2              PIC X(60).
00325          16  AT-INFO-TRAILER-TYPE        PIC X.
00326              88  AT-PAYMENT-NOTE         VALUE 'P'.
00327              88  AT-CALL-NOTE            VALUE 'C'.
00328              88  AT-MAINT-NOTE           VALUE 'M'.
00329              88  AT-CERT-CHANGE          VALUE 'X'.
080106             88  AT-APPROVAL-NOTE        VALUE 'R'.
080106             88  AT-NOTE-FILE-NOTE       VALUE 'N'.
00330          16  AT-CALL-TYPE                PIC X.
00331              88  AT-PHONE-CALL-IN        VALUE 'I'.
00332              88  AT-PHONE-CALL-OUT       VALUE 'O'.
00333          16  AT-NOTE-CONTINUATION        PIC X.
00334              88  AT-CONTINUED-NOTE       VALUE 'X'.
071910         16  AT-EOB-CODES-EXIST          PIC X.
071910             88  AT-EOB-CODES-PRESENT    VALUE 'Y'.
00335          16  FILLER                      PIC X(35).
00336          16  AT-GEN-INFO-LAST-MAINT-DT   PIC XX.
00337          16  AT-GEN-INFO-LAST-UPDATED-BY PIC X(4).
00338
00339      12  AT-AUTO-PROMPT-TR  REDEFINES  AT-TRAILER-BODY.
00340          16  AT-PROMPT-LINE-1            PIC X(60).
00341          16  AT-PROMPT-LINE-2            PIC X(60).
00342          16  AT-PROMPT-START-DT          PIC XX.
00343          16  AT-PROMPT-END-DT            PIC XX.
00344          16  FILLER                      PIC X(35).
00345          16  AT-PROMPT-LAST-MAINT-DT     PIC XX.
00346          16  AT-PROMPT-LAST-UPDATED-BY   PIC X(4).
00347
00348      12  AT-DENIAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.
00349          16  AT-DENIAL-INFO-1            PIC X(60).
00350          16  AT-DENIAL-INFO-2            PIC X(60).
00351          16  AT-DENIAL-DT                PIC XX.
00352          16  AT-RETRACTION-DT            PIC XX.
00353          16  AT-DENIAL-REASON-CODE       PIC X(4).
050506*         16  FILLER                      PIC X(31).
050506         16  AT-DENIAL-PROOF-DT          PIC XX.
050506         16  FILLER                      PIC X(29).
00355          16  AT-DENIAL-LAST-MAINT-DT     PIC XX.
00356          16  AT-DENIAL-LAST-UPDATED-BY   PIC X(4).
00357
00358      12  AT-INCURRED-CHG-TR  REDEFINES  AT-TRAILER-BODY.
00359          16  AT-OLD-INCURRED-DT          PIC XX.
00360          16  AT-OLD-REPORTED-DT          PIC XX.
00361          16  AT-OLD-ESTABLISHED-DT       PIC XX.
00362          16  AT-OLD-TOTAL-PAID           PIC S9(7)V99     COMP-3.
00363          16  AT-OLD-DAYS-PAID            PIC S9(4)        COMP.
00364          16  AT-OLD-NO-OF-PMTS           PIC S9(3)        COMP-3.
00365          16  AT-OLD-PAID-THRU-DT         PIC XX.
00366          16  AT-LAST-PMT-MADE-DT         PIC XX.
00367          16  FILLER                      PIC X(26).
00368          16  AT-OLD-DIAG-CODE            PIC X(6).
00369          16  AT-TRAILER-CNT-AT-CHG       PIC S9(4)        COMP.
00370          16  AT-OLD-ITD-PAID-EXPENSE     PIC S9(5)V99     COMP-3.
00371          16  AT-OLD-CHARGABLE-EXPENSE    PIC S9(5)V99     COMP-3.
00372          16  AT-OLD-INIT-MAN-RESV        PIC S9(7)V99     COMP-3.
00373          16  AT-OLD-CURRENT-MAN-RESV     PIC S9(7)V99     COMP-3.
00374          16  AT-OLD-ADDL-MAN-RESV        PIC S9(7)V99     COMP-3.
00375          16  AT-OLD-DIAG-DESCRIP         PIC X(60).
00376          16  FILLER                      PIC X(25).
00377          16  AT-INCURRED-LAST-UPDATED-BY PIC X(4).
00378
00379      12  AT-FORM-CONTROL-TR  REDEFINES  AT-TRAILER-BODY.
00380          16  AT-FORM-SEND-ON-DT          PIC XX.
00381          16  AT-FORM-FOLLOW-UP-DT        PIC XX.
00382          16  AT-FORM-RE-SEND-DT          PIC XX.
00383          16  AT-FORM-ANSWERED-DT         PIC XX.
00384          16  AT-FORM-PRINTED-DT          PIC XX.
00385          16  AT-FORM-REPRINT-DT          PIC XX.
00386          16  AT-FORM-TYPE                PIC X.
00387              88  INITIAL-FORM                  VALUE '1'.
00388              88  PROGRESS-FORM                 VALUE '2'.
00389          16  AT-INSTRUCT-LN-1            PIC X(28).
00390          16  AT-INSTRUCT-LN-2            PIC X(28).
00391          16  AT-INSTRUCT-LN-3            PIC X(28).
00392          16  AT-FORM-ADDR-SEQ-NO         PIC S9(4)      COMP.
00393          16  AT-FORM-ADDRESS             PIC X.
00394              88  FORM-TO-INSURED              VALUE 'I'.
00395              88  FORM-TO-ACCOUNT              VALUE 'A'.
00396              88  FORM-TO-OTHER-1              VALUE 'O'.
00397              88  FORM-TO-OTHER-2              VALUE 'Q'.
00398          16  AT-RELATED-1.
00399              20 AT-REL-CARR-1            PIC X.
00400              20 AT-REL-CLAIM-1           PIC X(7).
00401              20 AT-REL-CERT-1            PIC X(11).
00402          16  AT-RELATED-2.
00403              20 AT-REL-CARR-2            PIC X.
00404              20 AT-REL-CLAIM-2           PIC X(7).
00405              20 AT-REL-CERT-2            PIC X(11).
00406          16  AT-EMP-FORM-SEND-ON-DT      PIC XX.
00407          16  AT-PHY-FORM-SEND-ON-DT      PIC XX.
00408          16  AT-EMP-FORM-ANSWERED-DT     PIC XX.
00409          16  AT-PHY-FORM-ANSWERED-DT     PIC XX.
00410          16  AT-FORM-REM-PRINT-DT        PIC XX.
102610         16  AT-STOP-FORM-DT             PIC X(2).
00411
102610         16  FILLER                      PIC X(09).
00413          16  AT-FORM-LAST-MAINT-DT       PIC XX.
00414          16  AT-FORM-LAST-UPDATED-BY     PIC X(4).
00415 ******************************************************************
00271      EJECT
00272 *                                COPY ELCCNTL.
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
00273      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CHECK-QUE
                                ACTIVITY-TRAILERS CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL174' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00275
00276      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00277      MOVE '5'                   TO DC-OPTION-CODE.
00278      PERFORM 8500-DATE-CONVERSION.
00279      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00280      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00281
00282      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00283
00284 *    NOTE *******************************************************
00285 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
00286 *         *  FROM ANOTHER MODULE.                               *
00287 *         *******************************************************.
00288
00289      IF EIBCALEN NOT GREATER THAN ZERO
00290          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00291          GO TO 8300-SEND-TEXT.
00292
00293      
      * EXEC CICS HANDLE CONDITION
00294 *        PGMIDERR (9600-PGMIDERR)
00295 *        ENDFILE  (4800-END-OF-FILE)
00296 *        NOTFND   (4800-END-OF-FILE)
00297 *        ERROR    (9990-ERROR) END-EXEC.
      *    MOVE '"$L''I.                ! " #00004713' TO DFHEIV0
           MOVE X'22244C27492E202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303034373133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00298
00299      EJECT
00300  0010-MAIN-LOGIC.
00301      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00302          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00303              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00304              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00305              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00306              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00307              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00308              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00309              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00310              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
00311            ELSE
00312              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00313              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00314              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00315              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00316              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00317              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00318              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00319              MOVE SPACES               TO  PI-SAVED-PROGRAM-6
00320        ELSE
00321          GO TO 0020-MAIN-LOGIC.
00322
00323  0015-MAIN-LOGIC.
00324 *    NOTE *******************************************************
00325 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      *
00326 *         *  INTERFACE BLOCK FOR THIS MODULE.                   *
00327 *         *******************************************************.
00328
00329      
      * EXEC CICS HANDLE CONDITION
00330 *        QIDERR (0015-NEXT-SENTENCE) END-EXEC.
      *    MOVE '"$N                   ! # #00004749' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034373439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00331
00332      MOVE EIBTRMID               TO  WS-TS-TERM-ID.
00333
00334      
      * EXEC CICS DELETEQ TS
00335 *        QUEUE (WS-TEMP-STORAGE-KEY) END-EXEC.
      *    MOVE '*&                    #   #00004754' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00336
00337  0015-NEXT-SENTENCE.
00338      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.
00339
00340      MOVE LOW-VALUES             TO  PI-CHECK-AIX-KEY
00341                                      PI-PREV-CHECK-AIX-KEY.
00342
00343      MOVE ZERO                   TO  PI-END-OF-FILE
00344                                      PI-TEMP-STORAGE-ITEM
00345                                      PI-CONTROL-TOT
00346                                      PI-CONTROL-SAVE-CONTROL
00347                                      PI-CONTROL-GRAND-TOT.
00348
00349      MOVE WS-SAVE-CHECK-MODE     TO  PI-CHECK-MODE.
00350      MOVE PI-COMPANY-CD          TO PI-CK-COMPANY-CODE.
00351      MOVE 'Y'                    TO PI-FIRST-TIME-SWT.
00352      MOVE 'N'                    TO PI-SEND-TOT-SWT.
00353      MOVE 'N'                    TO PI-EOF-SWT.
00354
00355      IF WS-START-CNTLNO GREATER ZERO
00356          MOVE WS-START-CNTLNO    TO  PI-CK-CONTROL-NO.
00357
00358      IF EIBAID = DFHCLEAR
00359          GO TO 9400-CLEAR.
00360
00361      IF PI-PROCESSOR-ID = 'LGXX'
00362          NEXT SENTENCE
00363      ELSE
00364          
      * EXEC CICS READQ TS
00365 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00366 *            INTO    (SECURITY-CONTROL)
00367 *            LENGTH  (SC-COMM-LENGTH)
00368 *            ITEM    (SC-ITEM)
00369 *        END-EXEC
      *    MOVE '*$II   L              ''   #00004784' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00370          MOVE SC-CLAIMS-DISPLAY (11)   TO  PI-DISPLAY-CAP
00371          MOVE SC-CLAIMS-UPDATE  (11)   TO  PI-MODIFY-CAP
00372          IF NOT DISPLAY-CAP
00373              MOVE 'READ'               TO  SM-READ
00374              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00375              MOVE ER-0070              TO  EMI-ERROR
00376              GO TO 8100-SEND-INITIAL-MAP.
00377
00378      PERFORM 4000-BROWSE-CHECK-QUEUE-FILE.
00379
00380      EJECT
00381  0020-MAIN-LOGIC.
00382 *    NOTE *******************************************************
00383 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *
00384 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *
00385 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
00386 *         *******************************************************.
00387
00388      IF EIBAID = DFHCLEAR
00389          GO TO 9400-CLEAR.
00390
00391      IF NOT MODIFY-CAP
00392          IF EIBAID = DFHPF1 OR DFHPF2
00393              NEXT SENTENCE
00394          ELSE
00395              MOVE 'UPDATE'          TO  SM-READ
00396              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00397              MOVE ER-0070           TO  EMI-ERROR
00398              GO TO 8100-SEND-INITIAL-MAP.
00399
00400      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00401          MOVE ER-0008               TO  EMI-ERROR
00402          MOVE -1                 TO  APFKL
00403          PERFORM 8200-SEND-DATAONLY.
00404
00405      
      * EXEC CICS RECEIVE
00406 *        INTO   (EL174AI)
00407 *        MAPSET (WS-MAPSET-NAME)
00408 *        MAP    (WS-MAP-NAME) END-EXEC.
           MOVE LENGTH OF
            EL174AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00004825' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL174AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00409
00410      IF APFKL IS GREATER THAN ZERO
00411          IF EIBAID NOT = DFHENTER
00412              MOVE ER-0004           TO  EMI-ERROR
00413              MOVE AL-UNBOF       TO  APFKA
00414              MOVE -1             TO  APFKL
00415              PERFORM 8200-SEND-DATAONLY
00416            ELSE
00417              IF APFKO IS NUMERIC
00418                AND APFKO IS GREATER THAN ZERO
00419                AND APFKO IS LESS THAN '25'
00420                  MOVE PF-VALUES (APFKI)  TO  EIBAID
00421                ELSE
00422                  MOVE ER-0029           TO  EMI-ERROR
00423                  MOVE AL-UNBOF       TO  APFKA
00424                  MOVE -1             TO  APFKL
00425                  PERFORM 8200-SEND-DATAONLY.
00426
00427      IF EIBAID = DFHPF12
00428          MOVE 'EL010'         TO  THIS-PGM
00429          GO TO 9300-XCTL.
00430
00431      IF EIBAID = DFHPF23
00432          GO TO 9000-RETURN-CICS.
00433
00434      IF EIBAID = DFHPF24
00435          MOVE 'EL126   '         TO  THIS-PGM
00436          GO TO 9300-XCTL.
00437
00438      IF EIBAID = DFHENTER OR DFHPF1 OR DFHPF2
00439                           OR DFHPF3 OR DFHPF4
00440          NEXT SENTENCE
00441        ELSE
00442          MOVE ER-0008            TO  EMI-ERROR
00443          MOVE -1                 TO  APFKL
00444          PERFORM 8200-SEND-DATAONLY.
00445
00446      EJECT
00447  0100-MAIN-LOGIC.
00448      IF EIBAID = DFHPF3
00449          IF CNTLNOL IS GREATER THAN 0
00450              MOVE CNTLNOI        TO  PI-START-CONTROL-NO
00451              GO TO 0130-MAIN-LOGIC
00452          ELSE
00453              GO TO 0130-MAIN-LOGIC.
00454
00455      IF EIBAID = DFHPF4
00456          IF CHECKS-TO-BE-PRINTED
00457              MOVE 'Y' TO WS-SAVE-CHECK-MODE
00458              IF CNTLNOL GREATER ZERO
00459                  IF CNTLNOI NUMERIC
00460                      MOVE CNTLNOI TO WS-START-CNTLNO
00461                      GO TO 0015-MAIN-LOGIC
00462                  ELSE
00463                      GO TO 0015-MAIN-LOGIC
00464              ELSE
00465                  GO TO 0015-MAIN-LOGIC
00466          ELSE
00467              IF CHECKS-PRINTED
00468                  MOVE SPACE       TO WS-SAVE-CHECK-MODE
00469                  IF CNTLNOL IS GREATER THAN ZERO
00470                      IF CNTLNOI IS NUMERIC
00471                          MOVE CNTLNOI    TO  WS-START-CNTLNO
00472                          GO TO 0015-MAIN-LOGIC
00473                       ELSE
00474                           GO TO 0015-MAIN-LOGIC
00475                  ELSE
00476                      GO TO 0015-MAIN-LOGIC.
00477
00478      IF EIBAID = DFHPF1 OR DFHPF2
00479          GO TO 0110-MAIN-LOGIC.
00480
00481      IF CNTLNOL GREATER ZERO
00482          IF CNTLNOI NUMERIC
00483              MOVE CNTLNOI TO WS-START-CNTLNO
00484              MOVE PI-CHECK-MODE TO WS-SAVE-CHECK-MODE
00485              GO TO 0015-MAIN-LOGIC.
00486
00487      IF PI-END-OF-FILE NOT = ZERO
00488          PERFORM 9400-CLEAR.
00489
00490      PERFORM 4000-BROWSE-CHECK-QUEUE-FILE.
00491
00492  0110-MAIN-LOGIC.
00493      IF APAGEI NUMERIC
00494         MOVE APAGEI              TO  WS-TEMP-STORAGE-ITEM
00495        ELSE
00496         MOVE 1                   TO  WS-TEMP-STORAGE-ITEM.
00497
00498      IF EIBAID = DFHPF1 AND
00499         PI-SEND-TOT     AND
00500         PI-EOF          AND
00501         WS-TEMP-STORAGE-ITEM = PI-TEMP-STORAGE-ITEM
00502          MOVE LOW-VALUE           TO EL174AI
00503          MOVE WS-TEMP-STORAGE-ITEM TO APAGEO
00504          MOVE -1                   TO APFKL
00505          MOVE ER-0375              TO EMI-ERROR
00506          SET EL174A-INDEX          TO 1
00507          MOVE 'CONTL TOTAL'     TO EL174A-PMT-TYPE (EL174A-INDEX)
00508          MOVE PI-CONTROL-TOT       TO EL174A-AMT (EL174A-INDEX)
00509          SET EL174A-INDEX UP BY +1
00510          MOVE PI-CONTROL-GRAND-TOT TO EL174A-AMT (EL174A-INDEX)
00511          MOVE 'GRAND TOTAL'     TO EL174A-PMT-TYPE (EL174A-INDEX)
00512          PERFORM 8100-SEND-INITIAL-MAP.
00513
00514      IF EIBAID = DFHPF1
00515         IF WS-TEMP-STORAGE-ITEM LESS THAN PI-TEMP-STORAGE-ITEM
00516          ADD +1  TO  WS-TEMP-STORAGE-ITEM
00517          GO TO 0120-MAIN-LOGIC
00518         ELSE
00519          IF PI-NOT-EOF
00520             GO TO 4000-BROWSE-CHECK-QUEUE-FILE
00521          ELSE
00522             NEXT SENTENCE
00523      ELSE
00524         NEXT SENTENCE.
00525
00526      IF EIBAID = DFHPF2
00527        AND WS-TEMP-STORAGE-ITEM GREATER THAN +1
00528          SUBTRACT +1 FROM WS-TEMP-STORAGE-ITEM
00529          GO TO 0120-MAIN-LOGIC.
00530
00531      MOVE ER-0312                TO  EMI-ERROR.
00532      MOVE -1                     TO  APFKL.
00533      PERFORM 8200-SEND-DATAONLY.
00534      EJECT
00535  0120-MAIN-LOGIC.
00536      MOVE EIBTRMID               TO  WS-TS-TERM-ID.
00537
00538      
      * EXEC CICS READQ TS
00539 *        QUEUE  (WS-TEMP-STORAGE-KEY)
00540 *        ITEM   (WS-TEMP-STORAGE-ITEM)
00541 *        INTO   (EL174AI)
00542 *        LENGTH (WS-TS-LENGTH) END-EXEC.
      *    MOVE '*$II   L              ''   #00004958' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 EL174AI, 
                 WS-TS-LENGTH, 
                 WS-TEMP-STORAGE-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00543
00544      MOVE WS-TEMP-STORAGE-ITEM  TO  APAGEO.
00545
00546      PERFORM 8100-SEND-INITIAL-MAP.
00547      EJECT
00548  0130-MAIN-LOGIC.
00549      
      * EXEC CICS HANDLE CONDITION
00550 *         TERMIDERR    (0130-TERMID-ERROR)
00551 *         TRANSIDERR   (0130-TRANS-ERROR)
00552 *         END-EXEC.
      *    MOVE '"$[\                  ! $ #00004969' TO DFHEIV0
           MOVE X'22245B5C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303034393639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00553
00554      MOVE SPACES                 TO PI-ALT-DMD-PRT-ID.
00555
00556      IF PRINTERL NOT = ZEROS
00557         MOVE PRINTERI            TO WS-PRINTER-ID
00558                                     PI-ALT-DMD-PRT-ID
00559         GO TO 0130-START.
00560
00561      IF PI-PROCESSOR-PRINTER IS NOT EQUAL TO SPACES
00562          MOVE PI-PROCESSOR-PRINTER   TO  WS-PRINTER-ID
00563          GO TO 0130-START.
00564
00565      MOVE PI-COMPANY-ID          TO CNTL-CO.
00566      MOVE '1'                    TO CNTL-RECORD-TYPE.
00567      MOVE SPACES                 TO CNTL-GENL.
00568      MOVE ZEROS                  TO CNTL-SEQ.
00569      
      * EXEC CICS READ
00570 *         DATASET   (WS-CONTROL-DSID)
00571 *         SET       (ADDRESS OF CONTROL-FILE)
00572 *         RIDFLD    (CNTL-KEY)
00573 *         END-EXEC.
      *    MOVE '&"S        E          (   #00004989' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00574
00575      MOVE CF-FORMS-PRINTER-ID    TO WS-PRINTER-ID.
00576
00577  0130-START.
00578
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
00580 *       MOVE EIBTRMID   TO WS-PRINTER-ID
00581          
      * EXEC CICS START
00582 *            INTERVAL(0)
00583 *            TRANSID    (WS-PRINT-TRAN-ID)
00584 *            FROM       (PROGRAM-INTERFACE-BLOCK)
00585 *            LENGTH     (PI-COMM-LENGTH)
00586 *            TERMID     (WS-PRINTER-ID)
00587 *        END-EXEC
           MOVE 0 TO DFHEIV10
      *    MOVE '0(ILF                 0   #00005001' TO DFHEIV0
           MOVE X'3028494C4620202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203020' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV10, 
                 WS-PRINT-TRAN-ID, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
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
00588        ELSE
00589          
      * EXEC CICS START
00590 *            INTERVAL(0)
00591 *            TRANSID    (WS-PRINT-TRAN-ID)
00592 *            FROM       (PROGRAM-INTERFACE-BLOCK)
00593 *            LENGTH     (PI-COMM-LENGTH)
00594 *            TERMID     (WS-PRINTER-ID)
00595 *        END-EXEC.
           MOVE 0 TO DFHEIV10
      *    MOVE '0(ILFT                0   #00005009' TO DFHEIV0
           MOVE X'3028494C4654202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203020' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV10, 
                 WS-PRINT-TRAN-ID, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 WS-PRINTER-ID, 
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
           
00596
00597      MOVE ER-0567                TO EMI-ERROR.
00598      MOVE -1                     TO APFKL.
00599      GO TO 8200-SEND-DATAONLY.
00600
00601  0130-TERMID-ERROR.
00602      MOVE ER-0412                TO EMI-ERROR.
00603      MOVE -1                     TO APFKL.
00604      GO TO 8200-SEND-DATAONLY.
00605
00606  0130-TRANS-ERROR.
00607      MOVE ER-0413                TO EMI-ERROR.
00608      MOVE -1                     TO APFKL.
00609      GO TO 8200-SEND-DATAONLY.
00610
00611      EJECT
00612  4000-BROWSE-CHECK-QUEUE-FILE SECTION.
00613      MOVE LOW-VALUES             TO  EL174AI.
00614
00615      
      * EXEC CICS STARTBR
00616 *        DATASET (WS-CHECK-QUEUE-AIX-DSID)
00617 *        RIDFLD  (PI-CHECK-AIX-KEY)
00618 *        GTEQ    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005035' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 PI-CHECK-AIX-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00619
00620      SET EL174A-INDEX TO +1.
00621
00622  4100-READNEXT.
00623      MOVE PI-CHECK-AIX-KEY           TO  PI-PREV-CHECK-AIX-KEY.
00624
00625      
      * EXEC CICS READNEXT
00626 *        DATASET (WS-CHECK-QUEUE-AIX-DSID)
00627 *        RIDFLD  (PI-CHECK-AIX-KEY)
00628 *        SET     (ADDRESS OF CHECK-QUE) END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005045' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CHECK-AIX-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00629
00630      IF CQ-COMPANY-CD NOT = PI-COMPANY-CD
00631          GO TO 4800-END-OF-FILE.
00632
00633      IF CQ-ENTRY-TYPE NOT = 'Q'
00634          GO TO 4100-READNEXT.
00635
00636      IF NOT PI-NO-CARRIER-SECURITY
00637          IF CQ-CARRIER NOT = PI-CARRIER-SECURITY
00638             GO TO 4100-READNEXT.
00639
00640      IF CQ-TIMES-PRINTED NOT = ZERO
00641          IF CHECKS-PRINTED
00642              NEXT SENTENCE
00643          ELSE
00644              GO TO 4100-READNEXT
00645      ELSE
00646          IF CHECKS-TO-BE-PRINTED
00647              NEXT SENTENCE
00648          ELSE
00649              GO TO 4100-READNEXT.
00650
00651      IF PI-FIRST-TIME
00652         IF CNTLNOL IS EQUAL TO 0
00653             MOVE PI-CK-CONTROL-NO    TO  PI-START-CONTROL-NO.
00654
00655      IF EL174A-INDEX LESS THAN +18
00656         IF PI-CONTROL-SAVE-CONTROL NOT = CQ-CONTROL-NUMBER
00657            IF PI-FIRST-TIME
00658               MOVE CQ-CONTROL-NUMBER TO PI-CONTROL-SAVE-CONTROL
00659            ELSE
00660               MOVE 'CONTL TOTAL' TO EL174A-PMT-TYPE (EL174A-INDEX)
00661               MOVE PI-CONTROL-TOT TO EL174A-AMT (EL174A-INDEX)
00662               SET EL174A-INDEX UP BY +1
00663               MOVE ZEROS TO PI-CONTROL-TOT
00664               MOVE CQ-CONTROL-NUMBER TO PI-CONTROL-SAVE-CONTROL.
00665
00666      MOVE 'N'                    TO PI-FIRST-TIME-SWT.
00667
00668      IF EL174A-INDEX GREATER THAN +17
00669         MOVE +1 TO WS-READNEXT-SW.
00670
00671      IF WS-READNEXT-SW GREATER THAN ZERO
00672          GO TO 4900-ENDBROWSE.
00673
00674      ADD CQ-CHECK-AMOUNT TO PI-CONTROL-TOT.
00675      MOVE CQ-CONTROL-NUMBER     TO  EL174A-CONTROL (EL174A-INDEX)
00676      MOVE CQ-CHECK-NUMBER       TO  EL174A-CHECK-NO (EL174A-INDEX)
00677
00678      IF CQ-PAYMENT-TYPE = '1'
00679          MOVE 'PARTIAL'          TO EL174A-PMT-TYPE (EL174A-INDEX)
00680        ELSE
00681      IF CQ-PAYMENT-TYPE = '2'
00682          MOVE 'FINAL'            TO EL174A-PMT-TYPE (EL174A-INDEX)
00683        ELSE
00684      IF CQ-PAYMENT-TYPE = '3'
00685          MOVE 'LUMP SUM'         TO EL174A-PMT-TYPE (EL174A-INDEX)
00686        ELSE
00687      IF CQ-PAYMENT-TYPE = '4'
00688          MOVE 'ADDITIONAL'       TO EL174A-PMT-TYPE (EL174A-INDEX)
00689        ELSE
00690      IF CQ-PAYMENT-TYPE = '5'
00691          MOVE 'CHG EXP'          TO EL174A-PMT-TYPE (EL174A-INDEX)
00692        ELSE
00693      IF CQ-PAYMENT-TYPE = '6'
00694          MOVE 'NON CHG EXP'      TO EL174A-PMT-TYPE (EL174A-INDEX)
00695        ELSE
00696      IF CQ-PAYMENT-TYPE = '7'
00697          MOVE 'LIFE REFUND'      TO EL174A-PMT-TYPE (EL174A-INDEX)
00698        ELSE
00699      IF CQ-PAYMENT-TYPE = '8'
00700          MOVE 'A&H REFUND'      TO EL174A-PMT-TYPE (EL174A-INDEX).
00701
00702      ADD CQ-CHECK-AMOUNT TO PI-CONTROL-GRAND-TOT.
00703      MOVE CQ-CLAIM-NO           TO EL174A-CLAIM-NO (EL174A-INDEX).
00704      MOVE CQ-CARRIER            TO EL174A-CARRIER  (EL174A-INDEX).
00705      MOVE CQ-CERT-NO            TO EL174A-CERT-NO  (EL174A-INDEX).
00706      MOVE CQ-CHECK-AMOUNT       TO EL174A-AMT      (EL174A-INDEX).
00707
00708      MOVE SPACES                 TO  WS-ACTIVITY-TRAILERS-KEY.
00709
00710      MOVE CQ-COMPANY-CD          TO  WS-ATK-COMPANY-CD.
00711      MOVE CQ-CARRIER             TO  WS-ATK-CARRIER.
00712      MOVE CQ-CLAIM-NO            TO  WS-ATK-CLAIM-NO.
00713      MOVE CQ-CERT-NO             TO  WS-ATK-CERT-NO.
00714      MOVE CQ-PMT-TRLR-SEQUENCE   TO  WS-ATK-SEQUENCE-NO.
00715
00716      
      * EXEC CICS READ
00717 *        DATASET (WS-ACTIVITY-TRAILERS-DSID)
00718 *        RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
00719 *        SET     (ADDRESS OF ACTIVITY-TRAILERS) END-EXEC.
      *    MOVE '&"S        E          (   #00005136' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00720
00721      IF AT-PAYEE-TYPE EQUAL 'I'
00722          MOVE 'INSURED'          TO  EL174A-PAYEE (EL174A-INDEX)
00723        ELSE
00724      IF AT-PAYEE-TYPE EQUAL 'B'
00725          MOVE 'BENEFICIARY'      TO  EL174A-PAYEE (EL174A-INDEX)
00726        ELSE
00727      IF AT-PAYEE-TYPE EQUAL 'A'
00728          MOVE 'ACCOUNT'          TO  EL174A-PAYEE (EL174A-INDEX)
00729        ELSE
00730      IF AT-PAYEE-TYPE EQUAL 'O'
00731          MOVE 'OTHER 1'          TO  EL174A-PAYEE (EL174A-INDEX)
00732        ELSE
00733      IF AT-PAYEE-TYPE EQUAL 'Q'
00734          MOVE 'OTHER 2'          TO  EL174A-PAYEE (EL174A-INDEX)
00735        ELSE
00736      IF AT-PAYEE-TYPE EQUAL 'P'
00737          MOVE 'DOCTOR'           TO  EL174A-PAYEE (EL174A-INDEX)
00738        ELSE
00739      IF AT-PAYEE-TYPE EQUAL 'E'
00740          MOVE 'EMPLOYER'         TO  EL174A-PAYEE (EL174A-INDEX).
00741
00742      MOVE AT-RECORDED-BY         TO EL174A-BY (EL174A-INDEX).
00743
00744      IF EL174A-INDEX LESS THAN +18
00745          SET EL174A-INDEX UP BY +1
00746          GO TO 4100-READNEXT.
00747
00748      MOVE +1                     TO  WS-READNEXT-SW.
00749      GO TO 4100-READNEXT.
00750
00751  4800-END-OF-FILE.
00752      MOVE +1                     TO  PI-END-OF-FILE.
00753
00754      MOVE 'Y'                    TO PI-EOF-SWT.
00755      IF EL174A-INDEX GREATER +16
00756         MOVE 'Y' TO PI-SEND-TOT-SWT
00757         GO TO 4900-ENDBROWSE.
00758
00759      MOVE ER-0375                   TO  EMI-ERROR.
00760      IF EL174A-INDEX LESS THAN +18
00761               MOVE 'CONTL TOTAL' TO EL174A-PMT-TYPE (EL174A-INDEX)
00762               MOVE PI-CONTROL-TOT TO EL174A-AMT (EL174A-INDEX)
00763               SET EL174A-INDEX UP BY +1
00764      ELSE
00765         NEXT SENTENCE.
00766      IF EL174A-INDEX LESS THAN +18
00767         SET EL174A-INDEX UP BY +1
00768         MOVE PI-CONTROL-GRAND-TOT TO EL174A-AMT (EL174A-INDEX)
00769         MOVE 'GRAND TOTAL' TO EL174A-PMT-TYPE (EL174A-INDEX)
00770      ELSE
00771         NEXT SENTENCE.
00772
00773  4900-ENDBROWSE.
00774      MOVE -1                     TO  APFKL.
00775
00776      MOVE EIBTRMID               TO  WS-TS-TERM-ID.
00777
00778      
      * EXEC CICS WRITEQ TS
00779 *        QUEUE  (WS-TEMP-STORAGE-KEY)
00780 *        ITEM   (PI-TEMP-STORAGE-ITEM)
00781 *        FROM   (EL174AI)
00782 *        LENGTH (WS-TS-LENGTH) END-EXEC.
      *    MOVE '*" I                  ''   #00005198' TO DFHEIV0
           MOVE X'2A2220492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 EL174AI, 
                 WS-TS-LENGTH, 
                 PI-TEMP-STORAGE-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00783
00784      MOVE PI-TEMP-STORAGE-ITEM  TO  APAGEO.
00785
00786      PERFORM 8100-SEND-INITIAL-MAP.
00787
00788  4990-EXIT.
00789      EXIT.
00790
00791      EJECT
00792  8100-SEND-INITIAL-MAP SECTION.
00793      IF CHECKS-TO-BE-PRINTED
00794          MOVE WS-TO-BE-PRINTED-DESC    TO TITLEO
00795          MOVE 'EL174A'                 TO RPTIDO
00796          MOVE WS-TO-BE-PRINTED-PFDESC  TO PFDESCO
00797      ELSE
00798          MOVE WS-CHECKS-PRINTED-PFDESC TO PFDESCO
00799          MOVE 'EL174B'                 TO RPTIDO
00800          MOVE WS-CHECKS-PRINTED-DESC   TO TITLEO.
00801
00802      IF EMI-ERROR NOT = ZERO
00803          PERFORM 9900-ERROR-FORMAT.
00804
00805      MOVE EIBTIME                TO  TIME-IN.
00806
00807      MOVE SAVE-DATE              TO  ADATEO.
00808      MOVE TIME-OUT               TO  ATIMEO.
00809      MOVE EMI-MESSAGE-AREA (1)   TO AEMSG1O.
00810
00811      
      * EXEC CICS SEND
00812 *        FROM   (EL174AI)
00813 *        MAPSET (WS-MAPSET-NAME)
00814 *        MAP    (WS-MAP-NAME)
00815 *        CURSOR ERASE END-EXEC.
           MOVE LENGTH OF
            EL174AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005231' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL174AI, 
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
           
00816
00817      PERFORM 9100-RETURN-TRAN.
00818
00819  8100-EXIT.
00820      EXIT.
00821
00822      EJECT
00823  8200-SEND-DATAONLY SECTION.
00824      IF CHECKS-TO-BE-PRINTED
00825          MOVE WS-TO-BE-PRINTED-DESC    TO TITLEO
00826          MOVE 'EL174A'                 TO RPTIDO
00827          MOVE WS-TO-BE-PRINTED-PFDESC  TO PFDESCO
00828      ELSE
00829          MOVE WS-CHECKS-PRINTED-PFDESC TO PFDESCO
00830          MOVE 'EL174B'                 TO RPTIDO
00831          MOVE WS-CHECKS-PRINTED-DESC   TO TITLEO.
00832
00833      IF EMI-ERROR NOT = ZERO
00834          PERFORM 9900-ERROR-FORMAT.
00835
00836      MOVE EIBTIME                TO  TIME-IN.
00837      MOVE SAVE-DATE              TO  ADATEO.
00838      MOVE TIME-OUT               TO  ATIMEO.
00839      MOVE EMI-MESSAGE-AREA (1)   TO AEMSG1O.
00840
00841      
      * EXEC CICS SEND DATAONLY
00842 *        FROM   (EL174AI)
00843 *        MAPSET (WS-MAPSET-NAME)
00844 *        MAP    (WS-MAP-NAME)
00845 *        CURSOR END-EXEC.
           MOVE LENGTH OF
            EL174AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00005261' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL174AI, 
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
           
00846
00847      PERFORM 9100-RETURN-TRAN.
00848
00849  8100-EXIT.
00850      EXIT.
00851
00852      EJECT
00853  8300-SEND-TEXT SECTION.
00854      
      * EXEC CICS SEND TEXT
00855 *        FROM   (LOGOFF-TEXT)
00856 *        LENGTH (LOGOFF-LENGTH)
00857 *        ERASE  FREEKB END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005274' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323734' TO DFHEIV0(25:11)
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
           
00858
00859      
      * EXEC CICS RETURN
00860 *        END-EXEC.
      *    MOVE '.(                    &   #00005279' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00861
00862  8300-EXIT.
00863      EXIT.
00864
00865      EJECT
00866  8500-DATE-CONVERSION SECTION.
00867      
      * EXEC CICS LINK
00868 *        PROGRAM  ('ELDATCV')
00869 *        COMMAREA (DATE-CONVERSION-DATA)
00870 *        LENGTH   (DC-COMM-LENGTH) END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00005287' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00871
00872  8500-EXIT.
00873      EXIT.
00874
00875      EJECT
00876  9000-RETURN-CICS SECTION.
00877      MOVE 'EL005'                TO  THIS-PGM.
00878      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
00879      PERFORM 9300-XCTL.
00880
00881  9000-EXIT.
00882      EXIT.
00883
00884  9100-RETURN-TRAN SECTION.
00885      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
00886      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
00887
00888      
      * EXEC CICS RETURN
00889 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
00890 *        LENGTH   (PI-COMM-LENGTH)
00891 *        TRANSID  (WS-TRANS-ID) END-EXEC.
      *    MOVE '.(CT                  &   #00005308' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00892
00893  9100-EXIT.
00894      EXIT.
00895
00896  9300-XCTL SECTION.
00897      
      * EXEC CICS HANDLE CONDITION
00898 *        QIDERR (9300-NEXT-SENTENCE) END-EXEC.
      *    MOVE '"$N                   ! % #00005317' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303035333137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00899
00900      MOVE EIBTRMID               TO  WS-TS-TERM-ID.
00901
00902      
      * EXEC CICS DELETEQ TS
00903 *        QUEUE (WS-TEMP-STORAGE-KEY) END-EXEC.
      *    MOVE '*&                    #   #00005322' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00904
00905  9300-NEXT-SENTENCE.
00906      MOVE DFHENTER               TO  EIBAID.
00907
00908      
      * EXEC CICS XCTL
00909 *        PROGRAM  (THIS-PGM)
00910 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
00911 *        LENGTH   (PI-COMM-LENGTH) END-EXEC.
      *    MOVE '.$C                   $   #00005328' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00912
00913  9300-EXIT.
00914      EXIT.
00915
00916      EJECT
00917  9400-CLEAR SECTION.
00918      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.
00919      PERFORM 9300-XCTL.
00920
00921  9400-EXIT.
00922      EXIT.
00923
00924  9600-PGMIDERR SECTION.
00925      
      * EXEC CICS HANDLE CONDITION
00926 *        PGMIDERR (8300-SEND-TEXT) END-EXEC.
      *    MOVE '"$L                   ! & #00005345' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303035333435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00927
00928      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM
00929                                      LOGOFF-PGM.
00930
00931      MOVE 'EL005'                TO  THIS-PGM.
00932      MOVE SPACES                 TO  PI-ENTRY-CD-1.
00933
00934      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
00935
00936      PERFORM 9300-XCTL.
00937
00938  9600-EXIT.
00939      EXIT.
00940
00941      EJECT
00942  9900-ERROR-FORMAT SECTION.
00943      
      * EXEC CICS LINK
00944 *        PROGRAM  ('EL001')
00945 *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
00946 *        LENGTH   (EMI-COMM-LENGTH) END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   ''   #00005363' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00947
00948  9900-EXIT.
00949      EXIT.
00950
00951  9990-ERROR SECTION.
00952      MOVE DFHEIBLK TO EMI-LINE1.
00953      
      * EXEC CICS LINK
00954 *        PROGRAM  ('EL004')
00955 *        COMMAREA (EMI-LINE1)
00956 *        LENGTH   (72) END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00005373' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00957
00958      PERFORM 8100-SEND-INITIAL-MAP.
00959      GO TO 9100-RETURN-TRAN.
00960
00961  9990-EXIT.
00962      EXIT.
00963
00964  9995-SECURITY-VIOLATION.
00965 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00005402' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343032' TO DFHEIV0(25:11)
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
00966
00967  9995-EXIT.
00968      EXIT.
00969

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL174' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMIDERR,
                     4800-END-OF-FILE,
                     4800-END-OF-FILE,
                     9990-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0015-NEXT-SENTENCE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0130-TERMID-ERROR,
                     0130-TRANS-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 9300-NEXT-SENTENCE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL174' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
