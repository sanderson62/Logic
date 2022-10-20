00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL1742.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/13/96 09:34:05.
00007 *                            VMOD=2.010
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
00025 *        THIS PROGRAM PRINTS A REPORT SHOWING ALL CHECKS THAT
00026 *    ARE WAITING TO BE PRINTED.
00027
00028 *    ENTERED BY  - EL174   EX64 - CHECKS TO BE PRINTED REPORT.
00029
00030      EJECT
00031  ENVIRONMENT DIVISION.
00032
00033  DATA DIVISION.
00034
00035  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00036
00037  77  THIS-PGM PIC X(6)  VALUE 'EL1742'.
00038  77  FILLER  PIC X(32)  VALUE '********************************'.
00039  77  FILLER  PIC X(32)  VALUE '*   EL1742 WORKING STORAGE     *'.
00040  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.010 *********'.
00041
00042 *    COPY ELCDMD34.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDMD34.                           *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = DMD DLO034 PARAMETER AREA                 *
00007 *                                                                *
00008 *    LENGTH = 272    RECFRM = FIXED                              *
00009 *                                                                *
00010 ******************************************************************
00011  01  DLO034-COMMUNICATION-AREA.
00012      12  DL34-PROCESS-TYPE             PIC X.
00013      12  DL34-COMPANY-ID               PIC XXX.
00014      12  DL34-PRINT-PROGRAM-ID         PIC X(8).
00015      12  DL34-USERID                   PIC X(4).
00016      12  DL34-PRINT-LINE               PIC X(250).
00017      12  DL34-OVERRIDE-PRINTER-ID      PIC X(4).
00018      12  DL34-RETURN-CODE              PIC XX.
00019  01  DLO034-REC-LENGTH                 PIC S9(4) COMP VALUE +272.
00043  01  WS-DATE-AREA.
00044      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00045      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
00046
00047  01  FILLER                          COMP-3.
00048      05  WS-READNEXT-SW              PIC S9          VALUE ZERO.
00049
00050      05  WS-TIME-WORK                PIC S9(7)       VALUE ZERO.
00051      05  WS-TIME                     REDEFINES
00052          WS-TIME-WORK                PIC S9(3)V9(4).
00053
00054  01  FILLER.
00055      05  WS-ACTIVITY-TRAILERS-KEY.
00056          10  WS-ATK-COMPANY-CD       PIC X.
00057          10  WS-ATK-CARRIER          PIC X.
00058          10  WS-ATK-CLAIM-NO         PIC X(7).
00059          10  WS-ATK-CERT-NO          PIC X(11).
00060          10  WS-ATK-SEQUENCE-NO      PIC S9(4)  COMP.
00061
00062      05  ERROR-LINE                 PIC X(80).
00063      05  WS-CHECK-AIX-DSID          PIC X(8) VALUE 'ELCHKQ2'.
00064      05  WS-ACTIVITY-TRAILERS-DSID  PIC X(8) VALUE 'ELTRLR'.
00065      05  WS-NO-CHECKS-L             PIC S9(4) VALUE +0.
00066      05  WS-NO-CHECKS-A             PIC S9(4) VALUE +0.
00067      05  WS-NO-CHECKS               PIC S9(4) VALUE +0.
00068
00069 *    COPY ELCINTF.
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
00070
00071      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00072          16  PI-CHECK-AIX-KEY.
00073              20  PI-CK-COMPANY-CODE     PIC X.
00074              20  PI-CK-CONTROL-NO       PIC S9(8)          COMP.
00075              20  PI-CK-CARRIER          PIC X.
00076              20  PI-CK-GROUPING         PIC X(6).
00077              20  PI-CK-STATE            PIC XX.
00078              20  PI-CK-BENE-ACCT        PIC X(10).
00079              20  PI-CK-SEQUENCE-NO      PIC S9(4)          COMP.
00080
00081          16  PI-PREV-CHECK-AIX-KEY.
00082              20  PI-PREV-CK-COMPANY-CODE     PIC X.
00083              20  PI-PREV-CK-CONTROL-NO       PIC S9(8) COMP.
00084              20  PI-PREV-CK-CARRIER          PIC X.
00085              20  PI-PREV-CK-GROUPING         PIC X(06).
00086              20  PI-PREV-CK-STATE            PIC XX.
00087              20  PI-PREV-CK-BENE-ACCT        PIC X(10).
00088              20  PI-PREV-CK-SEQUENCE-NO      PIC S9(4) COMP.
00089
00090          16 FILLER                       PIC XXX.
00091
00092          16 PI-CONTROL-TOTALS.
00093             20 PI-CONTROL-TOT            PIC S9(7)V99 COMP-3.
00094             20 PI-CONTROL-GRAND-TOT      PIC S9(7)V99 COMP-3.
00095             20 PI-CONTROL-SAVE-CONTROL   PIC S9(8) COMP.
00096
00097          16 PI-FIRST-TIME-SWT        PIC X.
00098             88 PI-FIRST-TIME         VALUE 'Y'.
00099             88 PI-NOT-FIRST-TIME     VALUE 'N'.
00100
00101          16 FILLER                   PIC XX.
00102
00103          16 PI-CHECK-MODE            PIC X.
00104             88 CHECKS-TO-BE-PRINTED  VALUE SPACE.
00105             88 CHECKS-RELEASED       VALUE 'R'.
00106             88 CHECKS-PRINTED        VALUE 'Y'.
00107
00108          16 PI-START-CONTROL-NO      PIC S9(8)   COMP.
00109
00110          16 PI-PAGE                  PIC S999     COMP-3.
00111          16 PI-LINE-COUNT            PIC S999     COMP-3.
00112
00113          16 PI-TOT-CHECKS            PIC S9(4).
00114
00115          16 PI-ADDTL-TOTALS.
00116             20 PI-CONTROL-TOT-L          PIC S9(7)V99 COMP-3.
00117             20 PI-CONTROL-TOT-A          PIC S9(7)V99 COMP-3.
00118             20 PI-CONTROL-GRAND-TOT-L    PIC S9(7)V99 COMP-3.
00119             20 PI-CONTROL-GRAND-TOT-A    PIC S9(7)V99 COMP-3.
00120             20 PI-TOT-CHECKS-L           PIC S9(4).
00121             20 PI-TOT-CHECKS-A           PIC S9(4).
00122
00123          16  PI-END-CONTROL-NO       PIC S9(08)    COMP.
00124          16  PI-MONTH-END-SAVE       PIC XX.
00125          16  PI-NON-CASH-REL-CNT     PIC S9(05)    COMP-3.
00126          16  PI-NON-CASH-REL-AMT     PIC S9(9)V99  COMP-3.
00127
00128          16  FILLER                  PIC X(512).
00129  EJECT
00130  01  HEADING-1.
00131      12  FILLER                  PIC XX      VALUE SPACES.
00132      12  ADATE                   PIC X(8).
00133      12  FILLER                  PIC XX      VALUE SPACES.
00134      12  ATIME                   PIC 99.99.
00135      12  FILLER                  PIC X(8)    VALUE SPACES.
00136      12  ACOMP                   PIC XXX.
00137      12  FILLER                  PIC X       VALUE SPACES.
00138      12  HDG1                    PIC X(25)   VALUE SPACES.
00139      12  FILLER                  PIC X(7)    VALUE SPACES.
00140      12  FILLER                  PIC X(5)    VALUE 'PAGE'.
00141      12  PAGE-NO                 PIC ZZ9.
00142      12  FILLER                  PIC X(3)    VALUE SPACES.
00143      12  HDG1-RPT                PIC X(6)    VALUE 'EL174A'.
00144
00145  01  FILLER.
00146      12  VAR-HEADING1A           PIC X(24)   VALUE
00147              '- CHECKS TO BE PRINTED -'.
00148      12  VAR-HEADING1B           PIC X(24)   VALUE
00149              '-    PRINTED CHECKS    -'.
00150      12  VAR-HEADING1C           PIC X(27)   VALUE
00151              '- RELEASED CONTROL GROUPS -'.
00152
00153  01  HEADING-2.
00154      12  FILLER                  PIC XX      VALUE SPACES.
00155      12  FILLER                  PIC X(54)
00156      VALUE 'CONTROL CHK NO  PMT TYPE  L/A CLAIM  CAR CERT NO'.
00157      12  FILLER                  PIC X(22)
00158      VALUE 'PMT AMOUNT  PAYEE   BY'.
00159      EJECT
00160
00161  01  DETAIL-LINE.
00162          15  FILLER                  PIC XX.
00163          15  EL174A-CONTROL          PIC 9(7).
00164          15  FILLER                  PIC X.
00165          15  EL174A-CHECK-NO         PIC X(7).
00166          15  FILLER                  PIC X.
00167          15  EL174A-PMT-TYPE         PIC X(10).
00168          15  FILLER                  PIC X.
00169          15  EL174A-COV-TYPE         PIC X.
00170          15  FILLER                  PIC X.
00171          15  EL174A-CLAIM-NO         PIC X(7).
00172          15  FILLER                  PIC XX.
00173          15  EL174A-CARRIER          PIC X.
00174          15  FILLER                  PIC X.
00175          15  EL174A-CERT-NO          PIC X(11).
00176          15  FILLER                  PIC X.
00177          15  EL174A-AMT              PIC Z,ZZZ,ZZ9.99-.
00178          15  FILLER                  PIC X.
00179          15  EL174A-PAYEE            PIC X(7).
00180          15  FILLER                  PIC X.
00181          15  EL174A-BY               PIC X(4).
00182
00183  01  TOTAL-LINE                      REDEFINES
00184      DETAIL-LINE.
00185          15  FILLER                  PIC X(13).
00186          15  EL174A-NO-CHECKS-DESC   PIC X(13).
00187          15  EL174A-NO-CHECKS        PIC ZZZ9.
00188          15  FILLER                  PIC X(10).
00189          15  EL174A-CNTL-DESC        PIC X(12).
00190          15  EL174A-CNTL-TOTAL       PIC Z,ZZZ,ZZ9.99-.
00191          15  FILLER                  PIC X(15).
00192      EJECT
00193 *    COPY ELPRTCVD.
00001 *****************************************************************
00002 *                                                               *
00003 *                            ELPRTCVD.                          *
00004 *                            VMOD=2.001                         *
00005 *****************************************************************.
00006
00007 ******************************************************************
00008 ***   WORK AREAS  FOR TERMINAL ONLINE PRINT ROUTINE
00009 ***                 -ELPRTCVD-
00010 ***   TO BE USED WITH PROCEDURE COPY MEMBER -ELPRTCVP-
00011 ******************************************************************
00012
00013  01  S-WORK-AREA                     SYNC.
00014      12  WS-LINE-LEN                 PIC S9(4)       VALUE +80
00015                                      COMP.
00016
00017      12  WS-LINE-LENGTH              PIC S9(4)       VALUE ZERO
00018                                      COMP.
00019
00020      12  WS-BUFFER-SIZE              PIC S9(4)       VALUE +1916
00021                                      COMP.
00022
00023      12  WS-BUFFER-LENGTH            PIC S9(4)       VALUE ZERO
00024                                      COMP.
00025
00026      12  WS-PROG-END                 PIC X           VALUE SPACES.
00027
00028      12  WS-PRINT-AREA.
00029          16  WS-PASSED-CNTL-CHAR     PIC X           VALUE SPACES.
00030            88  SINGLE-SPACE                          VALUE ' '.
00031            88  DOUBLE-SPACE                          VALUE '0'.
00032            88  TRIPLE-SPACE                          VALUE '-'.
00033            88  TOP-PAGE                              VALUE '1'.
00034
00035          16  WS-PASSED-DATA.
00036              20  WS-PRINT-BYTE       PIC X
00037                  OCCURS 132 TIMES    INDEXED BY PRT-INDEX.
00038
00039      12  WS-LINE-CNT                 PIC S9(3)        VALUE ZERO
00040                                      COMP-3.
00041      12  WS-WCC-CNTL                 PIC X(1)         VALUE 'H'.
00042
00043      12  WS-EM                       PIC S9(4)        VALUE +25
00044                                      COMP.
00045      12  FILLER   REDEFINES WS-EM.
00046          16  FILLER                  PIC X.
00047          16  T-EM                    PIC X.
00048
00049 *    12  WS-SS                       PIC S9(4)        VALUE +21
00049      12  WS-SS                       PIC S9(4)        VALUE +10
00050                                      COMP.
00051      12  FILLER   REDEFINES WS-SS.
00052          16  FILLER                  PIC X.
00053          16  T-SS                    PIC X.
00054
00055      12  WS-TP                       PIC S9(4)      VALUE +12
00056                                      COMP.
00057      12  FILLER   REDEFINES WS-TP.
00058          16  FILLER                  PIC X.
00059          16  T-TP                    PIC X.
00060
00061      12  WS-FIRST-TIME-SW            PIC X           VALUE '1'.
00062          88  FIRST-TIME                              VALUE '1'.
00063          88  FIRST-LINE-NEXT-BUFFER                  VALUE '2'.
00064
00065      12  WS-BUFFER-AREA.
00066          16  WS-BUFFER-BYTE          PIC X
00067              OCCURS 1920 TIMES       INDEXED BY BUFFER-INDEX
00068                                                 BUFFER-INDEX2.
00069
00070 ******************************************************************
00194      EJECT
00195 *    COPY ELCDATE.
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
00196      EJECT
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
00198
00199 *    COPY ELCCHKQ.
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
       01  DFHCOMMAREA       PIC X(01).
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
00200      EJECT
00201 *    COPY ELCTRLR.
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
00202      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CHECK-QUE
                                ACTIVITY-TRAILERS.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1742' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00204
00205      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00206      MOVE '5'                   TO DC-OPTION-CODE.
00207      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00208      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00209      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00210
00211      MOVE 80                     TO WS-LINE-LEN.
00212
00213      MOVE SPACES                 TO DL34-PROCESS-TYPE.
00214
00215  0100-RETRIEVE-LOOP.
00216      
      * EXEC CICS HANDLE CONDITION
00217 *         ENDDATA    (200-END-DATA)
00218 *         ENDFILE    (4800-END-OF-FILE)
00219 *         NOTFND     (300-NOT-FOUND)
00220 *    END-EXEC.
      *    MOVE '"$&''I                 ! " #00001447' TO DFHEIV0
           MOVE X'222426274920202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031343437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00221
00222      
      * EXEC CICS RETRIEVE
00223 *         INTO(PROGRAM-INTERFACE-BLOCK)
00224 *         LENGTH(PI-COMM-LENGTH)
00225 *    END-EXEC.
      *    MOVE '0*I L                 &   #00001453' TO DFHEIV0
           MOVE X'302A49204C20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00226
00227 * DLO034 OPEN WHEN DMD OR CID
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
00229          IF DL34-PROCESS-TYPE = SPACES
00230              MOVE 'O'                TO DL34-PROCESS-TYPE
00231              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
00232              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
00233              MOVE PI-PROCESSOR-ID    TO DL34-USERID
00234              MOVE SPACES             TO DL34-PRINT-LINE
00235              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID
00236              
      * EXEC CICS LINK
00237 *                PROGRAM    ('DLO034')
00238 *                COMMAREA   (DLO034-COMMUNICATION-AREA)
00239 *                LENGTH     (DLO034-REC-LENGTH)
00240 *            END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   ''   #00001467' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00241              IF DL34-RETURN-CODE NOT = 'OK'
00242                  MOVE  '**DLO034 OPEN ERROR - ABORT**'
00243                                      TO ERROR-LINE
00244                  PERFORM 400-SEND-TEXT
00245                  
      * EXEC CICS
00246 *                    RETURN
00247 *                END-EXEC.
      *    MOVE '.(                    &   #00001476' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00248
00249      PERFORM 1000-INITIALIZE.
00250
00251      PERFORM 4000-BROWSE-CHECK-QUEUE-FILE.
00252
00253      MOVE SPACES                 TO TOTAL-LINE.
00254      MOVE PI-LIFE-OVERRIDE-L6    TO EL174A-NO-CHECKS-DESC.
00255      MOVE WS-NO-CHECKS-L         TO EL174A-NO-CHECKS.
00256      MOVE PI-CONTROL-TOT-L       TO EL174A-CNTL-TOTAL.
00257      MOVE TOTAL-LINE             TO WS-PASSED-DATA.
00258      MOVE '0'                    TO WS-PASSED-CNTL-CHAR.
00259      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00260      PERFORM 3000-BUMP-LINE-COUNT.
00261
00262      MOVE SPACES                 TO TOTAL-LINE.
00263      MOVE PI-AH-OVERRIDE-L6      TO EL174A-NO-CHECKS-DESC.
00264      MOVE WS-NO-CHECKS-A         TO EL174A-NO-CHECKS.
00265      MOVE PI-CONTROL-TOT-A       TO EL174A-CNTL-TOTAL.
00266      MOVE TOTAL-LINE             TO WS-PASSED-DATA.
00267      MOVE ' '                    TO WS-PASSED-CNTL-CHAR.
00268      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00269      PERFORM 3000-BUMP-LINE-COUNT.
00270
00271      MOVE SPACES                 TO TOTAL-LINE.
00272      MOVE 'NO. CHECKS'           TO EL174A-NO-CHECKS-DESC.
00273      MOVE WS-NO-CHECKS           TO EL174A-NO-CHECKS.
00274      MOVE 'CONTL TOTAL'          TO EL174A-CNTL-DESC.
00275      MOVE PI-CONTROL-TOT         TO EL174A-CNTL-TOTAL.
00276      MOVE TOTAL-LINE             TO WS-PASSED-DATA.
00277      MOVE ' '                    TO WS-PASSED-CNTL-CHAR.
00278      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00279      PERFORM 3000-BUMP-LINE-COUNT.
00280
00281      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
00282        IF PI-CALLING-PROGRAM = 'EL175'
00283          MOVE SPACES              TO TOTAL-LINE
00284          MOVE 'NON CASH  '        TO EL174A-NO-CHECKS-DESC
00285          MOVE PI-NON-CASH-REL-CNT TO EL174A-NO-CHECKS
00286          MOVE PI-NON-CASH-REL-AMT TO EL174A-CNTL-TOTAL
00287          MOVE TOTAL-LINE          TO WS-PASSED-DATA
00288          MOVE '0'                 TO WS-PASSED-CNTL-CHAR
00289          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00290          PERFORM 3000-BUMP-LINE-COUNT
00291          ADD PI-NON-CASH-REL-CNT TO PI-TOT-CHECKS
00292          ADD PI-NON-CASH-REL-AMT TO PI-CONTROL-GRAND-TOT.
00293
00294      MOVE SPACES                 TO TOTAL-LINE.
00295      MOVE PI-LIFE-OVERRIDE-L6    TO EL174A-NO-CHECKS-DESC.
00296      MOVE PI-TOT-CHECKS-L        TO EL174A-NO-CHECKS.
00297      MOVE PI-CONTROL-GRAND-TOT-L TO EL174A-CNTL-TOTAL.
00298      MOVE TOTAL-LINE             TO WS-PASSED-DATA.
00299      MOVE '0'                    TO WS-PASSED-CNTL-CHAR.
00300      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00301      PERFORM 3000-BUMP-LINE-COUNT.
00302
00303      MOVE SPACES                 TO TOTAL-LINE.
00304      MOVE PI-AH-OVERRIDE-L6      TO EL174A-NO-CHECKS-DESC.
00305      MOVE PI-TOT-CHECKS-A        TO EL174A-NO-CHECKS.
00306      MOVE PI-CONTROL-GRAND-TOT-A TO EL174A-CNTL-TOTAL.
00307      MOVE TOTAL-LINE             TO WS-PASSED-DATA.
00308      MOVE ' '                    TO WS-PASSED-CNTL-CHAR.
00309      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00310      PERFORM 3000-BUMP-LINE-COUNT.
00311
00312      MOVE SPACES                 TO TOTAL-LINE.
00313      MOVE 'NO. CHECKS'           TO EL174A-NO-CHECKS-DESC.
00314      MOVE PI-TOT-CHECKS          TO EL174A-NO-CHECKS.
00315      MOVE 'GRAND TOTAL'          TO EL174A-CNTL-DESC.
00316      MOVE PI-CONTROL-GRAND-TOT   TO EL174A-CNTL-TOTAL.
00317      MOVE TOTAL-LINE             TO WS-PASSED-DATA.
00318      MOVE ' '                    TO WS-PASSED-CNTL-CHAR.
00319      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00320      MOVE 'X'                    TO WS-PROG-END.
00321      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00322      GO TO 0100-RETRIEVE-LOOP.
00323
00324  200-END-DATA.
00325
00326 * DLO034 CLOSE
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
00328          MOVE 'C'                TO DL34-PROCESS-TYPE
00329          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
00330          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
00331          MOVE PI-PROCESSOR-ID    TO DL34-USERID
00332          MOVE SPACES             TO DL34-PRINT-LINE
00333                                     DL34-OVERRIDE-PRINTER-ID
00334          
      * EXEC CICS LINK
00335 *            PROGRAM    ('DLO034')
00336 *            COMMAREA   (DLO034-COMMUNICATION-AREA)
00337 *            LENGTH     (DLO034-REC-LENGTH)
00338 *        END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   ''   #00001565' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00339          IF DL34-RETURN-CODE NOT = 'OK'
00340              MOVE  '**DLO034 CLOSE ERROR - ABORT**'
00341                                  TO ERROR-LINE
00342              PERFORM 400-SEND-TEXT.
00343
00344      
      * EXEC CICS RETURN
00345 *    END-EXEC.
      *    MOVE '.(                    &   #00001575' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00346
00347  300-NOT-FOUND.
00348      MOVE 'NO COMMUNICATION AREA FOUND' TO ERROR-LINE.
00349
00350      PERFORM 400-SEND-TEXT.
00351
00352      GO TO 200-END-DATA.
00353
00354  400-SEND-TEXT.
00355      
      * EXEC CICS SEND TEXT
00356 *         FROM  (ERROR-LINE)
00357 *         LENGTH(70)
00358 *    END-EXEC.
           MOVE 70
             TO DFHEIV11
      *    MOVE '8&      T       H   F -   #00001586' TO DFHEIV0
           MOVE X'382620202020202054202020' TO DFHEIV0(1:12)
           MOVE X'202020204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERROR-LINE, 
                 DFHEIV11, 
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
           
00359
00360      EJECT
00361  1000-INITIALIZE    SECTION.
00362
00363 *    NOTE *******************************************************
00364 *         *                                                     *
00365 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      *
00366 *         *  INTERFACE BLOCK FOR THIS MODULE.                   *
00367 *         *                                                     *
00368 *         *******************************************************.
00369
00370      IF PI-CALLING-PROGRAM = 'EL175'
00371          MOVE LOW-VALUES         TO  PI-CK-COMPANY-CODE
00372                                      PI-CK-CARRIER
00373                                      PI-CK-GROUPING
00374                                      PI-CK-STATE
00375                                      PI-CK-BENE-ACCT
00376          MOVE ZEROS              TO  PI-CK-SEQUENCE-NO
00377          MOVE 'R'                TO  PI-CHECK-MODE
00378      ELSE
00379          IF CHECKS-PRINTED
00380              MOVE LOW-VALUES             TO  PI-CHECK-AIX-KEY
00381              MOVE PI-START-CONTROL-NO    TO  PI-CK-CONTROL-NO
00382          ELSE
00383              MOVE LOW-VALUES             TO  PI-CHECK-AIX-KEY.
00384
00385      MOVE LOW-VALUES             TO  PI-PREV-CHECK-AIX-KEY.
00386
00387      MOVE ZEROS                  TO  PI-CONTROL-TOT
00388                                      PI-CONTROL-TOT-L
00389                                      PI-CONTROL-TOT-A
00390                                      PI-CONTROL-SAVE-CONTROL
00391                                      PI-CONTROL-GRAND-TOT
00392                                      PI-CONTROL-GRAND-TOT-L
00393                                      PI-CONTROL-GRAND-TOT-A
00394                                      PI-TOT-CHECKS
00395                                      PI-TOT-CHECKS-L
00396                                      PI-TOT-CHECKS-A
00397                                      WS-NO-CHECKS-L
00398                                      WS-NO-CHECKS-A
00399                                      WS-NO-CHECKS.
00400      MOVE 1                      TO  PI-PAGE
00401                                      PI-LINE-COUNT.
00402
00403      MOVE PI-COMPANY-CD          TO PI-CK-COMPANY-CODE.
00404      MOVE 'Y'                    TO PI-FIRST-TIME-SWT.
00405
00406      MOVE EIBTIME                TO  WS-TIME-WORK.
00407
00408      MOVE SAVE-DATE              TO  ADATE.
00409      MOVE WS-TIME                TO  ATIME.
00410      MOVE PI-COMPANY-ID          TO  ACOMP.
00411      PERFORM 2000-PRINT-HEADING.
00412
00413      EJECT
00414  2000-PRINT-HEADING   SECTION.
00415      MOVE PI-PAGE                TO PAGE-NO.
00416
00417      IF CHECKS-TO-BE-PRINTED
00418          MOVE VAR-HEADING1A      TO HDG1
00419      ELSE
00420          IF CHECKS-PRINTED
00421              MOVE VAR-HEADING1B  TO HDG1
00422              MOVE 'EL174B'       TO HDG1-RPT
00423          ELSE
00424              MOVE VAR-HEADING1C  TO HDG1
00425              MOVE 'EL175A'       TO HDG1-RPT.
00426
00427      MOVE '1'                    TO WS-PASSED-CNTL-CHAR.
00428
00429      IF PI-COMPANY-ID = 'AFL' OR 'AFC' OR 'RIC' OR 'SRL'
00430          IF PI-FIRST-TIME-SWT = 'Y'
00431              MOVE ' '            TO WS-PASSED-CNTL-CHAR.
00432
00433      MOVE HEADING-1              TO WS-PASSED-DATA.
00434      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00435      MOVE 1                      TO PI-LINE-COUNT.
00436
00437      MOVE HEADING-2              TO WS-PASSED-DATA.
00438      MOVE '0'                    TO WS-PASSED-CNTL-CHAR.
00439      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00440      ADD 2                       TO PI-LINE-COUNT.
00441
00442      EJECT
00443  3000-BUMP-LINE-COUNT   SECTION.
00444      IF WS-PASSED-CNTL-CHAR = SPACES
00445         ADD 1                    TO PI-LINE-COUNT
00446       ELSE
00447         ADD  2                   TO PI-LINE-COUNT.
00448
00449      IF PI-LINE-COUNT GREATER 60
00450         MOVE ZEROS TO PI-LINE-COUNT
00451         ADD 1   TO PI-PAGE
00452         PERFORM 2000-PRINT-HEADING.
00453
00454      EJECT
00455  4000-BROWSE-CHECK-QUEUE-FILE SECTION.
00456
00457      
      * EXEC CICS STARTBR
00458 *        DATASET (WS-CHECK-AIX-DSID)
00459 *        RIDFLD  (PI-CHECK-AIX-KEY)
00460 *        GTEQ
00461 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00001688' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-AIX-DSID, 
                 PI-CHECK-AIX-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00462
00463  4100-READNEXT.
00464      MOVE PI-CHECK-AIX-KEY           TO  PI-PREV-CHECK-AIX-KEY.
00465
00466      
      * EXEC CICS READNEXT
00467 *        DATASET (WS-CHECK-AIX-DSID)
00468 *        RIDFLD  (PI-CHECK-AIX-KEY)
00469 *        SET     (ADDRESS OF CHECK-QUE)
00470 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00001697' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-AIX-DSID, 
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
           
00471
00472      IF CQ-COMPANY-CD NOT = PI-COMPANY-CD
00473          GO TO 4800-END-OF-FILE.
00474
00475      IF PI-CALLING-PROGRAM = 'EL175'
00476         IF PI-COMPANY-ID = 'DMD'
00477            IF CQ-CONTROL-NUMBER NOT = PI-END-CONTROL-NO
00478               GO TO 4100-READNEXT
00479             ELSE
00480               NEXT SENTENCE
00481           ELSE
00482         IF PI-CONTROL-SAVE-CONTROL NOT = CQ-CONTROL-NUMBER
00483            IF NOT PI-FIRST-TIME
00484               IF CQ-CONTROL-NUMBER > PI-END-CONTROL-NO
00485                  GO TO 4800-END-OF-FILE.
00486
00487      IF CQ-ENTRY-TYPE NOT = 'Q'
00488          GO TO 4100-READNEXT.
00489
00490      IF CHECKS-TO-BE-PRINTED
00491          IF CQ-TIMES-PRINTED NOT = ZERO
00492              GO TO 4100-READNEXT.
00493
00494      IF CHECKS-PRINTED
00495          IF CQ-TIMES-PRINTED = ZERO
00496              GO TO 4100-READNEXT.
00497
00498      IF PI-COMPANY-ID = 'DMD'
00499      IF CHECKS-PRINTED
00500         IF PI-START-CONTROL-NO > CQ-CONTROL-NUMBER
00501            GO TO 4100-READNEXT.
00502
00503      IF PI-CONTROL-SAVE-CONTROL NOT = CQ-CONTROL-NUMBER
00504         IF PI-FIRST-TIME
00505            NEXT SENTENCE
00506         ELSE
00507            MOVE SPACES              TO  TOTAL-LINE
00508            MOVE PI-LIFE-OVERRIDE-L6 TO EL174A-NO-CHECKS-DESC
00509            MOVE WS-NO-CHECKS-L      TO EL174A-NO-CHECKS
00510            MOVE PI-CONTROL-TOT-L    TO EL174A-CNTL-TOTAL
00511            MOVE TOTAL-LINE          TO WS-PASSED-DATA
00512            MOVE '0'                 TO WS-PASSED-CNTL-CHAR
00513            PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00514            PERFORM 3000-BUMP-LINE-COUNT
00515            MOVE SPACES              TO TOTAL-LINE
00516            MOVE PI-AH-OVERRIDE-L6   TO EL174A-NO-CHECKS-DESC
00517            MOVE WS-NO-CHECKS-A      TO EL174A-NO-CHECKS
00518            MOVE PI-CONTROL-TOT-A    TO EL174A-CNTL-TOTAL
00519            MOVE TOTAL-LINE          TO WS-PASSED-DATA
00520            MOVE ' '                 TO WS-PASSED-CNTL-CHAR
00521            PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00522            PERFORM 3000-BUMP-LINE-COUNT
00523            MOVE SPACES                 TO TOTAL-LINE
00524            MOVE 'NO. CHECKS'           TO EL174A-NO-CHECKS-DESC
00525            MOVE WS-NO-CHECKS           TO EL174A-NO-CHECKS
00526            MOVE 'CONTL TOTAL'          TO EL174A-CNTL-DESC
00527            MOVE PI-CONTROL-TOT         TO EL174A-CNTL-TOTAL
00528            MOVE TOTAL-LINE             TO WS-PASSED-DATA
00529            MOVE ' '                    TO WS-PASSED-CNTL-CHAR
00530            PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00531            PERFORM 3000-BUMP-LINE-COUNT
00532            MOVE SPACES                 TO WS-PASSED-DATA
00533            MOVE '0'                    TO WS-PASSED-CNTL-CHAR
00534            PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00535            PERFORM 3000-BUMP-LINE-COUNT
00536            MOVE ZEROS            TO PI-CONTROL-TOT
00537                                     PI-CONTROL-TOT-L
00538                                     PI-CONTROL-TOT-A
00539                                     WS-NO-CHECKS-L
00540                                     WS-NO-CHECKS-A
00541                                     WS-NO-CHECKS.
00542
00543      MOVE CQ-CONTROL-NUMBER      TO PI-CONTROL-SAVE-CONTROL.
00544      MOVE 'N'                    TO PI-FIRST-TIME-SWT.
00545
00546      ADD +1                      TO WS-NO-CHECKS
00547                                     PI-TOT-CHECKS.
00548      IF CQ-LIFE-CLAIM
00549          ADD +1                  TO WS-NO-CHECKS-L
00550                                     PI-TOT-CHECKS-L
00551          ADD CQ-CHECK-AMOUNT     TO PI-CONTROL-TOT-L
00552                                     PI-CONTROL-GRAND-TOT-L
00553       ELSE
00554      IF CQ-AH-CLAIM
00555          ADD +1                  TO WS-NO-CHECKS-A
00556                                     PI-TOT-CHECKS-A
00557          ADD CQ-CHECK-AMOUNT     TO PI-CONTROL-TOT-A
00558                                     PI-CONTROL-GRAND-TOT-A.
00559
00560      MOVE SPACES                 TO DETAIL-LINE.
00561      ADD CQ-CHECK-AMOUNT         TO PI-CONTROL-TOT.
00562      MOVE CQ-CONTROL-NUMBER      TO EL174A-CONTROL.
00563      MOVE CQ-CHECK-NUMBER        TO EL174A-CHECK-NO.
00564
00565      IF CQ-PAYMENT-TYPE = '1'
00566          MOVE 'PARTIAL'          TO EL174A-PMT-TYPE
00567        ELSE
00568      IF CQ-PAYMENT-TYPE = '2'
00569          MOVE 'FINAL'            TO EL174A-PMT-TYPE
00570        ELSE
00571      IF CQ-PAYMENT-TYPE = '3'
00572          MOVE 'LUMP SUM'         TO EL174A-PMT-TYPE
00573        ELSE
00574      IF CQ-PAYMENT-TYPE = '4'
00575          MOVE 'ADDITIONAL'       TO EL174A-PMT-TYPE
00576        ELSE
00577      IF CQ-PAYMENT-TYPE = '5'
00578          MOVE 'CHG EXP'          TO EL174A-PMT-TYPE
00579        ELSE
00580      IF CQ-PAYMENT-TYPE = '6'
00581          MOVE 'NON CHG EXP'      TO EL174A-PMT-TYPE
00582        ELSE
00583      IF CQ-PAYMENT-TYPE = '7'
00584          MOVE 'LIFE REFUND'      TO EL174A-PMT-TYPE
00585        ELSE
00586      IF CQ-PAYMENT-TYPE = '8'
00587          MOVE 'A&H REFUND'       TO EL174A-PMT-TYPE.
00588
00589      IF CQ-LIFE-CLAIM
00590          MOVE 'L'                TO EL174A-COV-TYPE
00591        ELSE
00592          MOVE 'A'                TO EL174A-COV-TYPE.
00593
00594      ADD CQ-CHECK-AMOUNT        TO  PI-CONTROL-GRAND-TOT.
00595      MOVE CQ-CLAIM-NO           TO  EL174A-CLAIM-NO.
00596      MOVE CQ-CARRIER            TO  EL174A-CARRIER.
00597      MOVE CQ-CERT-NO            TO  EL174A-CERT-NO.
00598      MOVE CQ-CHECK-AMOUNT       TO  EL174A-AMT.
00599
00600      MOVE SPACES                TO  WS-ACTIVITY-TRAILERS-KEY.
00601
00602      MOVE CQ-COMPANY-CD         TO  WS-ATK-COMPANY-CD.
00603      MOVE CQ-CARRIER            TO  WS-ATK-CARRIER.
00604      MOVE CQ-CLAIM-NO           TO  WS-ATK-CLAIM-NO.
00605      MOVE CQ-CERT-NO            TO  WS-ATK-CERT-NO.
00606      MOVE CQ-PMT-TRLR-SEQUENCE  TO  WS-ATK-SEQUENCE-NO.
00607
00608      
      * EXEC CICS READ
00609 *        DATASET (WS-ACTIVITY-TRAILERS-DSID)
00610 *        RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
00611 *        SET     (ADDRESS OF ACTIVITY-TRAILERS)
00612 *    END-EXEC.
      *    MOVE '&"S        E          (   #00001839' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383339' TO DFHEIV0(25:11)
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
           
00613
00614      IF AT-PAYEE-TYPE = 'I'
00615          MOVE 'INSURED'          TO  EL174A-PAYEE
00616        ELSE
00617      IF AT-PAYEE-TYPE = 'B'
00618          MOVE 'BENEFICIARY'      TO  EL174A-PAYEE
00619        ELSE
00620      IF AT-PAYEE-TYPE = 'A'
00621          MOVE 'ACCOUNT'          TO  EL174A-PAYEE
00622        ELSE
00623      IF AT-PAYEE-TYPE = 'O'
00624          MOVE 'OTHER 1'          TO  EL174A-PAYEE
00625        ELSE
00626      IF AT-PAYEE-TYPE = 'Q'
00627          MOVE 'OTHER 2'          TO  EL174A-PAYEE
00628        ELSE
00629      IF AT-PAYEE-TYPE = 'P'
00630          MOVE 'DOCTOR'           TO  EL174A-PAYEE
00631        ELSE
00632      IF AT-PAYEE-TYPE = 'E'
00633          MOVE 'EMPLOYER'         TO  EL174A-PAYEE.
00634
00635      MOVE AT-RECORDED-BY         TO  EL174A-BY.
00636
00637      MOVE DETAIL-LINE            TO WS-PASSED-DATA.
00638      MOVE ' '                    TO WS-PASSED-CNTL-CHAR.
00639      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00640      PERFORM 3000-BUMP-LINE-COUNT.
00641
00642      GO TO 4100-READNEXT.
00643
00644  4800-END-OF-FILE.
00645      IF PI-PREV-CHECK-AIX-KEY  = LOW-VALUES
00646         GO TO 4990-EXIT.
00647
00648      
      * EXEC CICS ENDBR
00649 *        DATASET (WS-CHECK-AIX-DSID)
00650 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001879' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-AIX-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00651
00652  4990-EXIT.
00653       EXIT.
00654      EJECT
uktdel*9500-PRINT-ROUTINE SECTION.  COPY ELPRTCVP.
uktins 9500-PRINT-ROUTINE SECTION.
uktins*    COPY ELPRTCVP.
00001 ******************************************************************
00002 ***                                                              *
00003 ***                          ELPRTCVP.                           *
00004 ***                          VMOD=2.003                          *
00005 ***                                                              *
00006 ***     COPY MEMBER FOR TERMINAL ONLINE PRINT ROUTINE.           *
00007 ***     THIS ROUTINE WILL ACCOMODATE PRINTING TO A 3270          *
00008 ***     TERMINAL PRINTER. A BUFFER OF UP TO 1920 CHARACTERS      *
00009 ***     IS ACCUMULATED AND PRINTED COLLECTIVELY.                 *
00010 ***                                                              *
00011 ***     THIS ROUTINE TO BE USED ONLY WITH ACCOMPANIMENT          *
00012 ***      OF THE WORKING-STORAGE COPY MEMBER ( ELPRTCVD )         *
00013 ***     THE HOST PROGRAM MUST INITIALIZE THE FOLLOWING 3 FIELDS  *
00014 ***      FROM THE ABOVE COPY MEMBER FOR THIS PROCEDURE TO BE     *
00015 ***      SUCCESSFUL.                                             *
00016 ***      05  WS-LINE-LEN    PIC  S9(4)  COMP  VALUE +80.         *
00017 ***                         LENGTH OF THE LINE TO BE PRINTED     *
00018 ***                         DEFAULT IS 80, YOU CAN USE ANY NUMBER*
00019 ***                         UP TO 132.  THIS FIELD IS ONLY ACCEP-*
00020 ***                         TED THE FIRST TIME THRU THE ROUTINE. *
00021 ***      05  WS-PROG-END    PIC  X  VALUE SPACES.                *
00022 ***                         PROGRAM END SWITCH. INITIALIZED      *
00023 ***                         TO SPACE-     MOVE IN ANY NONBLANK   *
00024 ***                         TO IT WHEN PROGRAM IS FINISHED.      *
00025 ***      05  WS-PRINT-AREA.                                      *
00026 ***          10  WS-PASSED-CNTL-CHAR     PIC X.                  *
00027 ***          10  WS-PASSED-DATA          PIC X(132).             *
00028 ***                         USE THE DATA TO BE PRINTED IN THE    *
00029 ***                         WS-PASSED-DATA.                      *
00030 ***                         USE THE STANDARD CARRIAGE CONTROL    *
00031 ***                         CHARACTER IN THE WS-PASSED-CNTL-CHAR *
00032 ***                           SINGLE-SPACE            VALUE ' '  *
00033 ***                           DOUBLE-SPACE            VALUE '0'  *
00034 ***                           TRIPLE-SPACE            VALUE '-'  *
00035 ***                           TOP-PAGE                VALUE '1'  *
00036 ***      NOTE: A LINE COUNT IS PROVIDED IN FIELDNAME -WS-LINE-CNT*
00037 ***            THE USE OF THIS FIELD IS OPTIONAL.                *
00038 ***            THIS ROUTINE WILL ONLY ADD 1, 2, OR 3             *
00039 ***            TO THIS COUNT DEPENDING ON THE WS-PASSED-CNTL-CHAR*
00040 ***            AND RESET THE COUNT TO ZERO WHEN TOP-PAGE         *
00041 ***            CONDITION.                                        *
00042 ***                                                              *
00043 ******************************************************************
00044
00045  ELPRTCVP.
00046
pemuni*    IF PI-COMPANY-ID IS EQUAL TO 'DMD' OR 'CID'
pemuni     IF PI-COMPANY-ID IS EQUAL TO 'DMD' OR 'XXX'
00048          MOVE 'P'                TO DL34-PROCESS-TYPE
00049          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
00050          MOVE PI-PROCESSOR-ID    TO DL34-USERID
00051          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
00052          MOVE WS-PRINT-AREA      TO DL34-PRINT-LINE
00053          MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID
00054
00055          
      * EXEC CICS LINK
00056 *            PROGRAM    ('DLO034')
00057 *            COMMAREA   (DLO034-COMMUNICATION-AREA)
00058 *            LENGTH     (DLO034-REC-LENGTH)
00059 *        END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   ''   #00001944' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031393434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00060
00061             IF DL34-RETURN-CODE = 'OK'
00062                 GO TO ELPRTCVP-EXIT
00063             ELSE
00064 *               MOVE '8339'     TO EMI-ERROR ?????ERROR MESSAGE???
00065                 GO TO ELPRTCVP-EXIT.
00066
00067      IF NOT FIRST-TIME
00068          GO TO ELPRTCVP-020.
00069
00070      IF WS-LINE-LEN NOT GREATER ZERO
00071          GO TO ELPRTCVP-EXIT.
00072
00073      MOVE '2'                    TO WS-FIRST-TIME-SW.
00074      MOVE LOW-VALUES             TO WS-BUFFER-AREA.
00075
00076      SET BUFFER-INDEX TO +1
00077
00078      IF EIBTRMID IS EQUAL TO 'AFLP'
00079          NEXT SENTENCE
00080      ELSE
00081          IF NOT TOP-PAGE
00082              MOVE T-TP           TO WS-BUFFER-BYTE (BUFFER-INDEX)
00083              SET BUFFER-INDEX UP BY +1.
00084
00085  ELPRTCVP-020.
00086      IF WS-PROG-END = SPACES
00087          GO TO ELPRTCVP-030.
00088
00089      MOVE SPACES                 TO WS-PROG-END.
00090
00091      IF BUFFER-INDEX GREATER +1
00092          PERFORM ELPRTCVP-PRINT-BUFFER THRU ELPRTCVP-PRINT-EXIT.
00093
00094      MOVE '1'                    TO WS-FIRST-TIME-SW.
00095
00096      GO TO ELPRTCVP-EXIT.
00097
00098  ELPRTCVP-030.
00099      IF WS-PASSED-DATA = SPACES
00100          SET PRT-INDEX TO +1
00101          GO TO ELPRTCVP-050.
00102
00103      SET PRT-INDEX TO WS-LINE-LEN.
00104
00105  ELPRTCVP-040.
00106      IF WS-PRINT-BYTE (PRT-INDEX) NOT = SPACES
00107          GO TO ELPRTCVP-050.
00108
00109      IF PRT-INDEX GREATER +1
00110          SET PRT-INDEX DOWN BY +1
00111          GO TO ELPRTCVP-040.
00112
00113  ELPRTCVP-050.
00114      SET WS-LINE-LENGTH TO PRT-INDEX.
00115      SET BUFFER-INDEX2 TO BUFFER-INDEX.
00116      SET BUFFER-INDEX2 UP BY WS-LINE-LENGTH.
00117
00118      IF BUFFER-INDEX2 NOT LESS WS-BUFFER-SIZE
00119          PERFORM ELPRTCVP-PRINT-BUFFER THRU ELPRTCVP-PRINT-EXIT.
00120
00121      IF TRIPLE-SPACE
00122           ADD +2  TO  WS-LINE-CNT
00123           MOVE T-SS           TO WS-BUFFER-BYTE (BUFFER-INDEX)
00124                                  WS-BUFFER-BYTE (BUFFER-INDEX + 1)
00125           SET BUFFER-INDEX UP BY +2.
00126
00127      IF DOUBLE-SPACE
00128           ADD +1  TO  WS-LINE-CNT
00129           MOVE T-SS             TO WS-BUFFER-BYTE (BUFFER-INDEX)
00130           SET BUFFER-INDEX UP BY +1.
00131
00132      ADD +1 TO WS-LINE-CNT
00133 ************************************************************
00134 *     BYPASS NEW LINE SYMBOL                               *
00135 *        IF FIRST BUFFER SENT AND TOP-OF-FORM SET.         *
00136 *     OR IF FIRST LINE OF SUBSEQUENT BUFFERS.              *
00137 ************************************************************
00138
00139      IF (BUFFER-INDEX GREATER +1 AND
00140          WS-BUFFER-BYTE (BUFFER-INDEX - 1) = T-TP)  OR
00141          FIRST-LINE-NEXT-BUFFER
00142          MOVE ZERO               TO WS-FIRST-TIME-SW
00143      ELSE
00144          MOVE T-SS               TO WS-BUFFER-BYTE (BUFFER-INDEX)
00145          SET BUFFER-INDEX UP BY +1.
00146
00147 **   NOTE, SINGLE SPACE IS REQUIRED BEFORE TOP PAGE CHAR
00148
00149      IF TOP-PAGE
00150          MOVE +1                TO WS-LINE-CNT
00151          MOVE T-TP              TO WS-BUFFER-BYTE (BUFFER-INDEX)
00152          SET BUFFER-INDEX UP BY +1.
00153
00154      SET PRT-INDEX TO +1.
00155
00156  ELPRTCVP-060.
00157      MOVE WS-PRINT-BYTE (PRT-INDEX)
00158                                  TO WS-BUFFER-BYTE (BUFFER-INDEX).
00159      SET BUFFER-INDEX UP BY +1.
00160
00161      IF PRT-INDEX LESS WS-LINE-LENGTH
00162          SET PRT-INDEX UP BY +1
00163          GO TO ELPRTCVP-060.
00164
00165  ELPRTCVP-EXIT.
00166      EXIT.
00167
00168  ELPRTCVP-PRINT-BUFFER.
00169      IF WS-BUFFER-BYTE (BUFFER-INDEX - 1) = T-SS
00170         MOVE SPACE               TO WS-BUFFER-BYTE (BUFFER-INDEX)
00171         SET BUFFER-INDEX UP BY 1.
00172
00173      MOVE  T-EM                  TO  WS-BUFFER-BYTE (BUFFER-INDEX)
00174      SET WS-BUFFER-LENGTH TO BUFFER-INDEX.
00175
00176      
      * EXEC CICS SEND
00177 *        FROM    (WS-BUFFER-AREA)
00178 *        LENGTH  (WS-BUFFER-LENGTH)
00179 *        CTLCHAR (WS-WCC-CNTL)
00180 *        ERASE
00181 *    END-EXEC.
      *    MOVE '$$    C E         L F ,   #00002065' TO DFHEIV0
           MOVE X'242420202020432045202020' TO DFHEIV0(1:12)
           MOVE X'2020202020204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-BUFFER-AREA, 
                 WS-BUFFER-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 WS-WCC-CNTL, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00182
00183      SET BUFFER-INDEX TO +1.
00184      MOVE '2'                    TO WS-FIRST-TIME-SW.
00185
00186  ELPRTCVP-PRINT-EXIT.
00187      EXIT.
00188
00656
00657  9700-LINK-DATE-CONVERT  SECTION.
00658
00659      
      * EXEC CICS LINK
00660 *        PROGRAM    ('ELDATCV')
00661 *        COMMAREA   (DATE-CONVERSION-DATA)
00662 *        LENGTH     (DC-COMM-LENGTH)
00663 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00002081' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00664
00665  9700-EXIT.
00666      EXIT.
00667
00668  9900-LAST-PARAGRAPH SECTION.
00669      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1742' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1742' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 200-END-DATA,
                     4800-END-OF-FILE,
                     300-NOT-FOUND
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1742' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
