00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL6851.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 09:56:47.
00007 *                            VMOD=2.013
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
00025 *        THIS PROGRAM WILL PRINT ONE OF THREE REPORTS
00026 *          1)  CHECKS WAITING TO BE PRINTED
00027 *          2)  CHECKS PRINTED
00028 *          3)  CHECKS RELEASED REPORT
00029 *
00030 *    ENTERED BY  - EL685  - CHECKS TO BE PRINTED REPORT
00031 *                           CHECKS PRINTED REPORT
00032 *                  EL686  - A/R CHECKS RELEASED REPORT
00033
00034      EJECT
00035  ENVIRONMENT DIVISION.
00036
00037  DATA DIVISION.
00038
00039  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00040
00041  77  THIS-PGM PIC X(6)  VALUE 'EL6851'.
00042  77  FILLER  PIC X(32)  VALUE '********************************'.
00043  77  FILLER  PIC X(32)  VALUE '*   EL6851 WORKING STORAGE     *'.
00044  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.013 *********'.
00045
00046  01  WS-DATE-AREA.
00047      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00048      05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.
00049
00050  01  FILLER   COMP-3.
00051      05  WS-READNEXT-SW              PIC S9          VALUE ZERO.
00052      05  WS-TIME-WORK                PIC S9(7)       VALUE ZERO.
00053      05  WS-TIME                     REDEFINES
00054          WS-TIME-WORK                PIC S9(3)V9(4).
00055
00056  01  FILLER.
00057      05  ERROR-LINE                 PIC X(80).
00058      05  WS-CHECK-QUEUE-DSID        PIC X(8) VALUE 'ERCHKQ'.
00059      05  WS-COMCK-QUEUE-DSID        PIC X(8) VALUE 'ERCMKQ'.
00060      05  WS-PENDING-PAYMENTS-DSID   PIC X(8) VALUE 'ERPYAJ'.
00061      05  WS-NO-CHECKS               PIC S9(4) VALUE +0.
00062
00063  01  FILLER.
00064      05  WS-TO-BE-PRINTED-DESC      PIC X(28)        VALUE
00065          '-   CHECKS TO BE PRINTED   -'.
00066
00067      05  WS-CHECKS-PRINTED-DESC     PIC X(28)        VALUE
00068          '-      PRINTED CHECKS      -'.
00069
00070      05  WS-AR-TO-BE-PRINTED-DESC   PIC X(28)        VALUE
00071          '- A/R CHECKS TO BE PRINTED -'.
00072
00073      05  WS-AR-CHECKS-PRINTED-DESC  PIC X(28)        VALUE
00074          '-    A/R PRINTED CHECKS    -'.
00075
00076      05  WS-AR-CHECKS-RELEASED-DESC PIC X(28)        VALUE
00077          '-   A/R CHECKS RELEASED   - '.
00078
00079 *                            COPY ELCDMD34.
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
00080      EJECT
00081 *                            COPY ELCINTF.
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
00082      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00083          16  PI-CHECK-QUE-KEY.
00084              20  PI-CK-COMPANY-CODE     PIC X.
00085              20  PI-CK-CONTROL-NO       PIC S9(8)          COMP.
00086              20  PI-CK-SEQUENCE-NO      PIC S9(4)          COMP.
00087
00088          16  PI-PREV-CHECK-QUE-KEY.
00089              20  PI-PREV-CK-COMPANY-CODE     PIC X.
00090              20  PI-PREV-CK-CONTROL-NO       PIC S9(8) COMP.
00091              20  PI-PREV-CK-SEQUENCE-NO      PIC S9(4) COMP.
00092
00093          16  FILLER                          PIC X(03).
00094
00095          16  PI-CONTROL-TOTALS.
00096              20  PI-CONTROL-TOT              PIC S9(7)V99 COMP-3.
00097              20  PI-CONTROL-GRAND-TOT        PIC S9(7)V99 COMP-3.
00098              20  PI-CONTROL-SAVE-CONTROL     PIC S9(8) COMP.
00099
00100          16  PI-FIRST-TIME-SWT               PIC X.
00101              88  PI-FIRST-TIME                  VALUE 'Y'.
00102              88  PI-NOT-FIRST-TIME              VALUE 'N'.
00103
00104          16  FILLER                          PIC X(02).
00105          16  PI-CHECK-MODE                   PIC X.
00106              88 CHECKS-PRINTED                  VALUE 'Y'.
00107              88 CHECKS-TO-BE-PRINTED            VALUE SPACE.
00108
00109          16  FILLER                          PIC X(10).
00110
00111          16  PI-AR-MODE                      PIC X.
00112              88 PI-ORIG-CALL-AR-MENU            VALUE 'A'.
00113
00114          16  FILLER                          PIC X(01).
00115
00116          16  PI-START-CONTROL-NO             PIC S9(08)  COMP.
00117
00118          16  PI-PAGE                         PIC S999    COMP-3.
00119          16  PI-LINE-COUNT                   PIC S999    COMP-3.
00120          16  PI-TOT-CHECKS                   PIC S9(4).
00121          16  FILLER                          PIC X(581).
00122
00123      EJECT
00124  01  HEADING-1.
00125      12  FILLER                  PIC XX      VALUE SPACES.
00126      12  ADATE                   PIC X(8).
00127      12  FILLER                  PIC XX      VALUE SPACES.
00128      12  ATIME                   PIC 99.99.
00129      12  FILLER                  PIC X(8)    VALUE SPACES.
00130      12  ACOMP                   PIC XXX.
00131      12  FILLER                  PIC X       VALUE SPACES.
00132      12  ATITLEO                 PIC X(28).
00133      12  FILLER                  PIC X(14)   VALUE SPACES.
00134      12  FILLER                  PIC X(5)    VALUE 'PAGE'.
00135      12  PAGE-NO                 PIC ZZ9.
00136
00137  01  HEADING-2.
00138      12  FILLER                  PIC XX      VALUE SPACES.
00139      12  FILLER                  PIC X(42)   VALUE
00140          'CONTROL CHECK NO  PMT TYPE   CAR  GROUP   '.
00141      12  HD2-DESC-1              PIC X(08)   VALUE 'FIN.RESP'.
00142      12  FILLER                  PIC X(03)   VALUE SPACES.
00143      12  HD2-DESC-2              PIC X(08)   VALUE ' ACCOUNT'.
00144      12  FILLER                  PIC X(17)   VALUE
00145          '       PMT AMOUNT'.
00146
00147  01  HEADING-2B.
00148      12  FILLER                  PIC XX      VALUE SPACES.
00149      12  FILLER                  PIC X(36)   VALUE
00150          'CONTROL  CHK NO   CHK DT  CAR  GRP  '.
00151      12  FILLER                  PIC X(08)   VALUE '   PAYEE'.
00152      12  FILLER                  PIC X(04)   VALUE SPACES.
00153      12  FILLER                  PIC X(04)   VALUE 'SEQ '.
00154      12  FILLER                  PIC X(11)   VALUE
00155          ' PAYEE NAME'.
00156      12  FILLER                  PIC X(05)   VALUE SPACES.
00157      12  FILLER                  PIC X(10)   VALUE
00158          'PMT AMOUNT'.
00159      EJECT
00160
00161  01  DETAIL-LINE.
00162      15  FILLER                      PIC X.
00163      15  EL685A-CONTROL              PIC 9(7).
00164      15  FILLER                      PIC XX.
00165      15  EL685A-CHECK-NO             PIC X(7).
00166      15  FILLER                      PIC X.
00167      15  EL685A-PMT-TYPE             PIC X(11).
00168      15  FILLER                      PIC X(3).
00169      15  EL685A-CARRIER              PIC X.
00170      15  FILLER                      PIC XX.
00171      15  EL685A-GROUP                PIC X(6).
00172      15  FILLER                      PIC XX.
00173      15  EL685A-FIN-RESP             PIC X(10).
00174      15  FILLER                      PIC X.
00175      15  EL685A-ACCOUNT              PIC X(11).
00176      15  FILLER                      PIC X.
00177      15  EL685A-AMT                  PIC Z,ZZZ,ZZ9.99-.
00178      15  FILLER                      PIC XX.
00179
00180  01  DETAIL-LINEB                    REDEFINES
00181      DETAIL-LINE.
00182      15  FILLER                      PIC XX.
00183      15  EL685B-CONTROL              PIC 9(7).
00184      15  FILLER                      PIC XX.
00185      15  EL685B-CHECK-NO             PIC X(6).
00186      15  FILLER                      PIC XX.
00187      15  EL685B-CHK-DT               PIC X(08).
00188      15  FILLER                      PIC X(2).
00189      15  EL685B-CARRIER              PIC X.
00190      15  FILLER                      PIC X.
00191      15  EL685B-GROUP                PIC X(6).
00192      15  FILLER                      PIC XX.
00193      15  EL685B-PAYEE                PIC X(10).
00194      15  FILLER                      PIC X.
00195      15  EL685B-PAYEE-SEQ            PIC ZZZ9.
00196      15  FILLER                      PIC X.
00197      15  EL685B-PAYEE-NA             PIC X(12).
00198      15  FILLER                      PIC X.
00199      15  EL685B-AMT                  PIC Z,ZZZ,ZZ9.99-.
00200
00201  01  TOTAL-LINE                      REDEFINES
00202      DETAIL-LINE.
00203      15  FILLER                      PIC X(19).
00204      15  EL685A-NO-CHECKS-DESC       PIC X(11).
00205      15  EL685A-NO-CHECKS            PIC ZZZ9.
00206      15  FILLER                      PIC X(12).
00207      15  EL685A-CNTL-DESC            PIC X(12).
00208      15  FILLER                      PIC X(8).
00209      15  EL685A-CNTL-TOTAL           PIC Z,ZZZ,ZZ9.99-.
00210      15  FILLER                      PIC XX.
00211
00212  01  TOTAL-LINE2                     REDEFINES
00213      DETAIL-LINE.
00214      15  FILLER                      PIC X(19).
CIDMOD     15  EL685B-NO-CHECKS-DESC       PIC X(11).
CIDMOD     15  EL685B-NO-CHECKS            PIC ZZZ9.
00217      15  FILLER                      PIC X(12).
00218      15  EL685B-CNTL-DESC            PIC X(12).
00219      15  FILLER                      PIC X(8).
00220      15  EL685B-CNTL-TOTAL           PIC Z,ZZZ,ZZ9.99-.
00221      15  FILLER                      PIC XX.
00222
00223      EJECT
00224 *                                COPY ELPRTCVD.
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
00225      EJECT
00226 *                                COPY ELCDATE.
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
00227      EJECT
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
00229
00230 *01 DFHBLLDS  COMP SYNC.
00231 *    05  BLLCBAR                     PIC S9(9).
00232 *    05  CQFCBAR                     PIC S9(9).
00233 *    05  CMFCBAR                     PIC S9(9).
00234 *    05  PPFCBAR                     PIC S9(9).
00235      EJECT
00236 *                                COPY ERCCHKQ.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCHKQ                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK QUE FILE FOR THE CREDIT SYSTEM      *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 100  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ERCHKQ                         RKP=2,LEN=7    *
00013 *       ALTERNATE PATH  = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
       01  DFHCOMMAREA       PIC X(01).
00018  01  CHECK-QUE.
00019      12  CQ-RECORD-ID                PIC XX.
00020          88  VALID-CQ-ID                     VALUE 'CQ'.
00021
00022      12  CQ-CONTROL-PRIMARY.
00023          16  CQ-COMPANY-CD           PIC X.
00024          16  CQ-CONTROL-NUMBER       PIC S9(8)       COMP.
00025          16  CQ-SEQUENCE-NUMBER      PIC S9(4)       COMP.
00026
00027      12  CQ-ENTRY-TYPE               PIC X.
00028              88  CHECK-ON-QUE           VALUE 'Q'.
00029              88  ALIGNMENT-CHECK        VALUE 'A'.
00030              88  MANUAL-CHECK           VALUE 'M'.
00031              88  SPOILED-CHECK          VALUE 'S'.
00032              88  VOIDED-CHECK           VALUE 'V'.
00033              88  PAYMENT-ABORTED        VALUE 'X'.
00034
00035      12  CQ-CREDIT-MASTER-CNTL       PIC X(50).
00036
00037      12  CQ-CREDIT-PYAJ-CNTL         REDEFINES
00038          CQ-CREDIT-MASTER-CNTL.
00039          16  CQ-PYAJ-CARRIER         PIC X.
00040          16  CQ-PYAJ-GROUPING        PIC X(6).
00041          16  CQ-PYAJ-FIN-RESP        PIC X(10).
00042          16  CQ-PYAJ-ACCOUNT         PIC X(10).
00043          16  CQ-PYAJ-SEQ             PIC S9(8)  COMP.
00044          16  CQ-PYAJ-REC-TYPE        PIC X.
00045          16  FILLER                  PIC X(18).
00046
00047      12  CQ-CREDIT-CHEK-CNTL         REDEFINES
00048          CQ-CREDIT-MASTER-CNTL.
00049          16  CQ-CHEK-CARRIER         PIC X.
00050          16  CQ-CHEK-GROUPING        PIC X(6).
00051          16  CQ-CHEK-STATE           PIC XX.
00052          16  CQ-CHEK-ACCOUNT         PIC X(10).
00053          16  CQ-CHEK-CERT-EFF-DT     PIC XX.
00054          16  CQ-CHEK-CERT-NO.
00055              20  CQ-CHEK-CERT-PRIME  PIC X(10).
00056              20  CQ-CHEK-CERT-SFX    PIC X.
00057          16  CQ-CHEK-SEQ-NO          PIC S9(4)       COMP.
00058          16  CQ-CHEK-FIN-RESP        PIC X(10).
00059          16  FILLER                  PIC X(06).
00060
00061      12  CQ-CHECK-NUMBER             PIC X(7).
00062      12  CQ-CHECK-AMOUNT             PIC S9(7)V99    COMP-3.
00063      12  CQ-PAYMENT-TYPE             PIC X.
00064              88  CQ-BILLING-CREDIT         VALUE '1'.
00065              88  CQ-REFUND-PMT             VALUE '2'.
00066              88  CQ-CHECK-MAINT-PMT        VALUE '3'.
00067      12  CQ-VOID-INDICATOR           PIC X.
00068              88  CHECK-IS-VOID             VALUE 'V'.
00069      12  CQ-TIMES-PRINTED            PIC S9(4)       COMP.
00070      12  CQ-PRINT-AT-HHMM            PIC S9(4)       COMP.
00071      12  CQ-CHECK-BY-USER            PIC X(4).
00072      12  CQ-PRE-NUMBERING-SW         PIC X.
00073        88  CHECKS-WERE-NOT-PRE-NUMBERED    VALUE SPACE.
00074        88  CHECKS-WERE-PRE-NUMBERED        VALUE '1'.
00075
00076      12  CQ-CHECK-WRITTEN-DT         PIC XX.
00077      12  CQ-LAST-UPDATED-BY          PIC S9(4)       COMP.
00078      12  CQ-ACCOUNT-AGENT            PIC X(10).
00079      12  CQ-CHECK-VOIDED-DT          PIC XX.
00080
00081      12  CQ-LETTERS-IND              PIC X.
00082          88  CQ-LETTERS-REQUIRED           VALUE 'Y'.
00083
00084 ******************************************************************
00237      EJECT
00238 *                                COPY ERCCMKQ.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCMKQ                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.014                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK QUE FILE FOR THE COMMISSION         *
00008 *                      CHECK OF THE CREDIT SYSTEM                *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 1800 RECFORM = FIXED                           *
00012 *                                                                *
00013 *   BASE CLUSTER = ERCMKQ                         RKP=2,LEN=7    *
00014 *       ALTERNATE PATH  = ERCMKQ2  (BY PAYEE CONTRO AND          *
00015 *                                      CONTROL NUMBER)           *
00016 *                                                 RKP=9,LEN=30   *
00017 *                                                                *
00018 *   LOG = NO                                                     *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00020 ******************************************************************
00021  01  COMMISSION-CHECK-QUE.
00022      12  MQ-RECORD-ID                PIC XX.
00023          88  VALID-MQ-ID                     VALUE 'MQ'.
00024
00025      12  MQ-CONTROL-PRIMARY.
00026          16  MQ-COMPANY-CD           PIC X.
00027          16  MQ-CONTROL-NUMBER       PIC S9(8)       COMP.
00028          16  MQ-SEQUENCE-NUMBER      PIC S9(4)       COMP.
00029
00030      12  MQ-CONTROL-BY-PAYEE.
00031          16  MQ-COMPANY-CD-A1        PIC X.
00032          16  MQ-CSR-A1               PIC X(4).
00033          16  MQ-CARRIER-A1           PIC X.
00034          16  MQ-GROUPING-A1          PIC X(6).
00035          16  MQ-PAYEE-A1             PIC X(10).
00036          16  MQ-PAYEE-SEQ-A1         PIC S9(4)       COMP.
00037          16  MQ-CONTROL-NUMBER-A1    PIC S9(8)       COMP.
00038          16  MQ-SEQUENCE-NUMBER-A1   PIC S9(4)       COMP.
00039
00040      12  MQ-ENTRY-TYPE               PIC X.
00041              88  CHECK-ON-QUE           VALUE 'Q'.
00042              88  ALIGNMENT-CHECK        VALUE 'A'.
00043              88  SPOILED-CHECK          VALUE 'S'.
00044              88  PAYMENT-ABORTED        VALUE 'X'.
00045              88  ACH-PAYMENT            VALUE 'P'.
00046
00047      12  FILLER                      PIC X(10).
00048
00049      12  MQ-CREDIT-CHEK-CNTL.
00050          16  MQ-CHEK-CSR             PIC X(4).
00051          16  MQ-CHEK-CARRIER         PIC X.
00052          16  MQ-CHEK-GROUPING        PIC X(6).
00053          16  MQ-CHEK-PAYEE           PIC X(10).
00054          16  MQ-CHEK-PAYEE-SEQ       PIC S9(4)       COMP.
00055          16  MQ-CHEK-SEQ-NO          PIC S9(4)       COMP.
00056
00057      12  FILLER                      PIC X(10).
00058
00059      12  MQ-PAYEE-INFO.
00060          16  MQ-PAYEE-NAME           PIC X(30).
00061          16  MQ-PAYEE-ADDRESS-1      PIC X(30).
00062          16  MQ-PAYEE-ADDRESS-2      PIC X(30).
00063          16  MQ-PAYEE-CITY-ST        PIC X(30).
00064          16  MQ-PAYEE-ZIP-CODE.
00065              20  MQ-PAYEE-ZIP.
00066                  24  FILLER          PIC X(1).
00067                      88 MQ-PAYEE-CANADIAN-POST-CODE
00068                                      VALUE 'A' THRU 'Z'.
00069                  24  FILLER          PIC X(4).
00070              20  MQ-PAYEE-ZIP-EXT    PIC X(4).
00071          16  MQ-PAYEE-CANADIAN-POSTAL-CODES
00072                  REDEFINES MQ-PAYEE-ZIP-CODE.
00073              20  MQ-PAY-CAN-POSTAL-CD-1
00074                                      PIC X(3).
00075              20  MQ-PAY-CAN-POSTAL-CD-2
00076                                      PIC X(3).
00077              20  FILLER              PIC X(3).
00078
00079      12  MQ-CREDIT-PYAJ-CNTL.
00080          16  MQ-PYAJ-CARRIER         PIC X.
00081          16  MQ-PYAJ-GROUPING        PIC X(6).
00082          16  MQ-PYAJ-FIN-RESP        PIC X(10).
00083          16  FILLER                  PIC X(6).
00084
00085      12  MQ-CHECK-NUMBER             PIC X(6).
00086      12  MQ-CHECK-AMOUNT             PIC S9(7)V99    COMP-3.
00087      12  MQ-NUMBER-OF-CK-STUBS       PIC S9(3)       COMP-3.
00088      12  MQ-VOID-DT                  PIC XX.
00089      12  MQ-TIMES-PRINTED            PIC S9(4)       COMP.
00090      12  MQ-PRINT-AT-HHMM            PIC S9(4)       COMP.
00091      12  MQ-CHECK-BY-USER            PIC X(4).
00092      12  MQ-PRE-NUMBERING-SW         PIC X.
00093        88  CHECKS-WERE-NOT-PRE-NUMBERED    VALUE SPACE.
00094        88  CHECKS-WERE-PRE-NUMBERED        VALUE '1'.
00095
00096      12  MQ-CHECK-WRITTEN-DT         PIC XX.
00097      12  MQ-ACH-WRITTEN-DT REDEFINES  MQ-CHECK-WRITTEN-DT
00098                                      PIC XX.
00099      12  MQ-LAST-MAINT-BY            PIC X(4).
00100      12  MQ-LAST-MAINT-HHMMSS        PIC S9(6)       COMP-3.
00101      12  MQ-LAST-MAINT-DT            PIC XX.
00102      12  MQ-CHECK-RELEASE-DT         PIC XX.
00103      12  MQ-RECORD-TYPE              PIC X.
00104          88  MQ-DETAIL                     VALUE 'D'.
00105          88  MQ-TEXT                       VALUE 'T'.
00106
00107      12  MQ-DETAIL-INFORMATION.
00108          16  MQ-DETAIL-INFO        OCCURS 15 TIMES.
00109              20  MQ-CHECK-STUB-LINE.
00110                  24  MQ-STUB-COMMENT        PIC X(23).
00111                  24  MQ-ACCT-AGENT          PIC X(10).
00112                  24  MQ-INVOICE             PIC X(6).
00113                  24  MQ-REFERENCE           PIC X(12).
00114                  24  MQ-LEDGER-NO           PIC X(14).
00115                  24  MQ-PYAJ-AMT            PIC S9(7)V99 COMP-3.
00116                  24  MQ-PYAJ-REC-TYPE       PIC X.
00117                  24  MQ-PYAJ-SEQ            PIC S9(8)    COMP.
00118                  24  MQ-PAYMENT-TYPE        PIC X.
00119                  24  MQ-PYAJ-PMT-APPLIED    PIC X.
00120                  24  MQ-LAST-MAINT-APPLIED  PIC X.
00121                  24  MQ-NON-AR-ITEM         PIC X.
00122                  24  FILLER                 PIC X(19).
00123
00124      12  MQ-CHECK-STUB-TEXT REDEFINES MQ-DETAIL-INFORMATION.
00125          16  MQ-CHECK-TEXT-ITEMS   OCCURS 3 TIMES.
00126              20  MQ-STUB-TEXT        PIC X(70).
00127          16  MQ-STUB-FILLER          PIC X(1260).
00128
00129      12  MQ-CREDIT-SELECT-DATE       PIC XX.
00130      12  MQ-CREDIT-ACCEPT-DATE       PIC XX.
00131
00132      12  MQ-AR-STATEMENT-DT          PIC XX.
00133      12  MQ-CO-TYPE                  PIC X.
00134
00135      12  MQ-STARTING-CHECK-NUMBER    PIC X(06).
00136      12  FILLER                      PIC X(41).
00137 ******************************************************************
00239      EJECT
00240 *                                COPY ERCPYAJ.
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
00241      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CHECK-QUE
                                COMMISSION-CHECK-QUE PENDING-PAY-ADJ.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6851' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00243
00244      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00245      MOVE '5'                   TO DC-OPTION-CODE.
00246      PERFORM 9950-LINK-DATE-CONVERT THRU 9950-EXIT.
00247      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00248      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00249      MOVE SPACES                TO DL34-PROCESS-TYPE
00250
00251      MOVE 80                     TO WS-LINE-LEN.
CIDMOD*    MOVE 'N'                    TO CSO-PRINT-STARTED-SW.
00252  0100-RETRIEVE-LOOP.
00253      
      * EXEC CICS HANDLE CONDITION
00254 *         ENDDATA (200-END-DATA)
00255 *         ENDFILE (4800-END-OF-FILE)
00256 *         NOTFND  (300-NOT-FOUND)
00257 *    END-EXEC.
      *    MOVE '"$&''I                 ! " #00001264' TO DFHEIV0
           MOVE X'222426274920202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031323634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00258
00259      
      * EXEC CICS RETRIEVE
00260 *         INTO   (PROGRAM-INTERFACE-BLOCK)
00261 *         LENGTH (PI-COMM-LENGTH)
00262 *    END-EXEC.
      *    MOVE '0*I L                 &   #00001270' TO DFHEIV0
           MOVE X'302A49204C20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00263
00264 * DLO034 OPEN WHEN DMD OR CID
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
00266          IF DL34-PROCESS-TYPE IS EQUAL TO SPACES
00267              MOVE 'O'                TO DL34-PROCESS-TYPE
00268              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
00269              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
00270              MOVE PI-PROCESSOR-ID    TO DL34-USERID
00271              MOVE SPACES             TO DL34-PRINT-LINE
00272              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID
00273
00274              
      * EXEC CICS LINK
00275 *                PROGRAM    ('DLO034')
00276 *                COMMAREA   (DLO034-COMMUNICATION-AREA)
00277 *                LENGTH     (DLO034-REC-LENGTH)
00278 *            END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   ''   #00001285' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00279
00280              IF DL34-RETURN-CODE NOT = 'OK'
00281                  MOVE  '**DLO034 OPEN ERROR - ABORT**'
00282                                      TO ERROR-LINE
00283                  PERFORM 400-SEND-TEXT
00284                  
      * EXEC CICS RETURN
00285 *                END-EXEC.
      *    MOVE '.(                    &   #00001295' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00286
CIDMOD*    MOVE PI-PROCESSOR-PRINTER TO CSO-PRINT-ID.
CIDMOD*    MOVE 'F'                TO  DRS-SW.
CIDMOD*    PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
CIDMOD*    MOVE ' '                TO  DRS-SW.
CIDMOD*
00287      PERFORM 1000-INITIALIZE.
00288
00289      PERFORM 4000-BROWSE-CHECK-QUEUE-FILE.
00290
00291      MOVE SPACES                 TO TOTAL-LINE
00292                                     TOTAL-LINE2.
00293      MOVE 'CONTL TOTAL'          TO EL685A-CNTL-DESC
00294                                     EL685B-CNTL-DESC.
00295      MOVE PI-CONTROL-TOT         TO EL685A-CNTL-TOTAL
00296                                     EL685B-CNTL-TOTAL.
CIDMOD     MOVE 'NO. CHECKS '          TO EL685A-NO-CHECKS-DESC
CIDMOD                                    EL685B-NO-CHECKS-DESC.
CIDMOD     MOVE WS-NO-CHECKS           TO EL685A-NO-CHECKS
CIDMOD                                    EL685B-NO-CHECKS.
00297
00298      IF (PI-CALLING-PROGRAM = 'EL686   '  AND
00299           PI-PGM-PRINT-OPT = '5')         OR
00300          PI-ORIG-CALL-AR-MENU
00301          MOVE TOTAL-LINE2        TO WS-PASSED-DATA
00302      ELSE
00303          MOVE TOTAL-LINE         TO WS-PASSED-DATA.
00304
00305      MOVE ' '                    TO WS-PASSED-CNTL-CHAR.
00306      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00307      PERFORM 3000-BUMP-LINE-COUNT.
00308
00309      MOVE PI-CONTROL-GRAND-TOT   TO EL685A-CNTL-TOTAL
00310                                     EL685B-CNTL-TOTAL.
00311      MOVE 'GRAND TOTAL'          TO EL685A-CNTL-DESC
00312                                     EL685B-CNTL-DESC.
CIDMOD     MOVE 'NO. CHECKS '          TO EL685A-NO-CHECKS-DESC
CIDMOD                                    EL685B-NO-CHECKS-DESC.
CIDMOD     MOVE PI-TOT-CHECKS          TO EL685A-NO-CHECKS
CIDMOD                                    EL685B-NO-CHECKS.
00313
00314      IF (PI-CALLING-PROGRAM = 'EL686   '  AND
00315           PI-PGM-PRINT-OPT = '5')         OR
00316          PI-ORIG-CALL-AR-MENU
00317          MOVE TOTAL-LINE2        TO WS-PASSED-DATA
00318      ELSE
00319          MOVE TOTAL-LINE         TO WS-PASSED-DATA.
00320
00321      MOVE '0'                    TO WS-PASSED-CNTL-CHAR.
00322      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00323      MOVE 'X'                    TO WS-PROG-END.
CIDMOD*    MOVE 'L'                    TO DRS-SW.
CIDMOD*    MOVE SPACES                 TO WS-PASSED-DATA.
CIDMOD*    MOVE SPACES                 TO WS-PASSED-CNTL-CHAR.
00324      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00325      GO TO 0100-RETRIEVE-LOOP.
00326
00327  200-END-DATA.
00328
00329 * DLO034 CLOSE
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
00331         IF DL34-PROCESS-TYPE NOT EQUAL TO SPACES
00332              MOVE 'C'                TO DL34-PROCESS-TYPE
00333              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
00334              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
00335              MOVE PI-PROCESSOR-ID    TO DL34-USERID
00336              MOVE SPACES             TO DL34-PRINT-LINE
00337                                         DL34-OVERRIDE-PRINTER-ID
00338              
      * EXEC CICS LINK
00339 *                PROGRAM    ('DLO034')
00340 *                COMMAREA   (DLO034-COMMUNICATION-AREA)
00341 *                LENGTH     (DLO034-REC-LENGTH)
00342 *            END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   ''   #00001365' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00343
00344              IF DL34-RETURN-CODE NOT = 'OK'
00345                  MOVE  '**DLO034 CLOSE ERROR - ABORT**'
00346                                      TO ERROR-LINE
00347                  PERFORM 400-SEND-TEXT.
00348
CIDMOD*    MOVE 'L'  TO  DRS-SW.
CIDMOD*    PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00349      
      * EXEC CICS RETURN
00350 *    END-EXEC.
      *    MOVE '.(                    &   #00001378' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00351
00352  300-NOT-FOUND.
00353      MOVE 'NO COMMUNICATION AREA FOUND' TO ERROR-LINE.
00354      PERFORM 400-SEND-TEXT.
00355      GO TO 200-END-DATA.
00356
00357  400-SEND-TEXT.
00358      
      * EXEC CICS SEND TEXT
00359 *         FROM   (ERROR-LINE)
00360 *         LENGTH (70)
00361 *    END-EXEC.
           MOVE 70
             TO DFHEIV11
      *    MOVE '8&      T       H   F -   #00001387' TO DFHEIV0
           MOVE X'382620202020202054202020' TO DFHEIV0(1:12)
           MOVE X'202020204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333837' TO DFHEIV0(25:11)
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
           
00362      EJECT
00363  1000-INITIALIZE    SECTION.
00364
00365 *    NOTE *******************************************************
00366 *         *                                                     *
00367 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      *
00368 *         *  INTERFACE BLOCK FOR THIS MODULE.                   *
00369 *         *                                                     *
00370 *         *******************************************************.
00371
00372      IF PI-CALLING-PROGRAM = 'EL686   '
00373          MOVE LOW-VALUES               TO  PI-CK-COMPANY-CODE
00374          MOVE ZEROS                    TO  PI-CK-SEQUENCE-NO
00375      ELSE
00376          IF CHECKS-PRINTED
00377              MOVE LOW-VALUES           TO  PI-CHECK-QUE-KEY
00378              MOVE PI-START-CONTROL-NO  TO  PI-CK-CONTROL-NO
00379          ELSE
00380              MOVE LOW-VALUES           TO  PI-CHECK-QUE-KEY.
00381
00382      MOVE LOW-VALUES             TO  PI-PREV-CHECK-QUE-KEY.
00383
00384      MOVE ZEROS                  TO  PI-CONTROL-TOT
00385                                      PI-CONTROL-SAVE-CONTROL
00386                                      PI-CONTROL-GRAND-TOT
00387                                      PI-TOT-CHECKS
00388                                      WS-NO-CHECKS.
00389
00390      MOVE 1                      TO  PI-PAGE
00391                                      PI-LINE-COUNT.
00392
00393      MOVE PI-COMPANY-CD          TO  PI-CK-COMPANY-CODE.
00394      MOVE 'Y'                    TO  PI-FIRST-TIME-SWT.
00395
00396      MOVE EIBTIME                TO  WS-TIME-WORK.
00397
00398      MOVE SAVE-DATE              TO  ADATE.
00399      MOVE WS-TIME                TO  ATIME.
00400      MOVE PI-COMPANY-ID          TO  ACOMP.
00401
00402      PERFORM 2000-PRINT-HEADING.
00403
00404      EJECT
00405  2000-PRINT-HEADING   SECTION.
00406
00407      IF (PI-CALLING-PROGRAM = 'EL686   '  AND
00408           PI-PGM-PRINT-OPT = '5')
00409              MOVE WS-AR-CHECKS-RELEASED-DESC  TO ATITLEO
00410              GO TO 2000-CONTINUE.
00411
00412      IF CHECKS-TO-BE-PRINTED
00413          IF PI-ORIG-CALL-AR-MENU
00414              MOVE WS-AR-TO-BE-PRINTED-DESC    TO ATITLEO
00415          ELSE
00416              MOVE WS-TO-BE-PRINTED-DESC       TO ATITLEO
00417      ELSE
00418          IF PI-ORIG-CALL-AR-MENU
00419              MOVE WS-AR-CHECKS-PRINTED-DESC   TO ATITLEO
00420          ELSE
00421              MOVE WS-CHECKS-PRINTED-DESC      TO ATITLEO.
00422
00423  2000-CONTINUE.
00424      MOVE PI-PAGE                TO PAGE-NO.
00425      MOVE HEADING-1              TO WS-PASSED-DATA.
00426      MOVE '1'                    TO WS-PASSED-CNTL-CHAR.
00427      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00428      MOVE 1                      TO PI-LINE-COUNT.
00429
00430      IF PI-COMPANY-ID = 'LAP'  OR  'RMC'
00431          MOVE ' ACCOUNT'         TO HD2-DESC-1
00432          MOVE 'CERT NO.'         TO HD2-DESC-2.
00433
00434      IF (PI-CALLING-PROGRAM = 'EL686   '  AND
00435           PI-PGM-PRINT-OPT = '5')         OR
00436          PI-ORIG-CALL-AR-MENU
00437      MOVE HEADING-2B             TO WS-PASSED-DATA
00438      ELSE
00439      MOVE HEADING-2              TO WS-PASSED-DATA.
00440
00441      MOVE '0'                    TO WS-PASSED-CNTL-CHAR.
00442      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00443      ADD 2                       TO PI-LINE-COUNT.
00444
00445      EJECT
00446  3000-BUMP-LINE-COUNT   SECTION.
00447      IF WS-PASSED-CNTL-CHAR = SPACES
00448         ADD 1                    TO PI-LINE-COUNT
00449        ELSE
00450         ADD  2                   TO PI-LINE-COUNT.
00451
00452      IF PI-LINE-COUNT GREATER 60
00453         MOVE ZEROS TO PI-LINE-COUNT
00454         ADD 1   TO PI-PAGE
00455         PERFORM 2000-PRINT-HEADING.
00456
00457      EJECT
00458
00459  4000-BROWSE-CHECK-QUEUE-FILE SECTION.
00460
00461      IF (PI-CALLING-PROGRAM = 'EL686   '  AND
00462           PI-PGM-PRINT-OPT = '5')         OR
00463          PI-ORIG-CALL-AR-MENU
00464               PERFORM 5000-BROWSE-COMM-CHECK-QUEUE
00465               GO TO 4990-EXIT.
00466
00467      
      * EXEC CICS STARTBR
00468 *        DATASET (WS-CHECK-QUEUE-DSID)
00469 *        RIDFLD  (PI-CHECK-QUE-KEY)
00470 *        GTEQ
00471 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00001496' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 PI-CHECK-QUE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00472
00473  4100-READNEXT.
00474      MOVE PI-CHECK-QUE-KEY        TO  PI-PREV-CHECK-QUE-KEY.
00475
00476      
      * EXEC CICS READNEXT
00477 *        DATASET (WS-CHECK-QUEUE-DSID)
00478 *        RIDFLD  (PI-CHECK-QUE-KEY)
00479 *        SET     (ADDRESS OF CHECK-QUE)
00480 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00001505' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CHECK-QUE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00481
00482      IF CQ-COMPANY-CD NOT = PI-COMPANY-CD
00483          GO TO 4800-END-OF-FILE.
00484
00485      IF CQ-ENTRY-TYPE NOT = 'Q'
00486          GO TO 4100-READNEXT.
00487
00488      IF CQ-VOID-INDICATOR = 'V'
00489          GO TO 4100-READNEXT.
00490
00491      IF CQ-CHECK-AMOUNT = ZEROS
00492          GO TO 4100-READNEXT.
00493
00494      IF PI-CALLING-PROGRAM IS EQUAL TO 'EL686   '
00495          IF PI-CONTROL-SAVE-CONTROL NOT EQUAL CQ-CONTROL-NUMBER
00496              IF NOT PI-FIRST-TIME
00497                  GO TO 4800-END-OF-FILE.
00498
00499      IF CQ-TIMES-PRINTED NOT = ZERO
00500          IF CHECKS-PRINTED
00501              NEXT SENTENCE
00502          ELSE
00503              GO TO 4100-READNEXT
00504      ELSE
00505          IF CHECKS-TO-BE-PRINTED
00506              NEXT SENTENCE
00507          ELSE
00508              GO TO 4100-READNEXT.
00509
00510      IF PI-CONTROL-SAVE-CONTROL NOT = CQ-CONTROL-NUMBER
00511         IF PI-FIRST-TIME
00512            NEXT SENTENCE
00513         ELSE
00514            MOVE SPACES           TO  TOTAL-LINE
00515            MOVE 'CONTL TOTAL'    TO EL685A-CNTL-DESC
00516            MOVE PI-CONTROL-TOT   TO EL685A-CNTL-TOTAL
00517            MOVE 'NO. CHECKS '    TO EL685A-NO-CHECKS-DESC
00518            MOVE WS-NO-CHECKS     TO EL685A-NO-CHECKS
00519            MOVE TOTAL-LINE       TO WS-PASSED-DATA
00520            MOVE ' '              TO WS-PASSED-CNTL-CHAR
00521            PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00522            PERFORM 3000-BUMP-LINE-COUNT
00523            MOVE SPACES           TO WS-PASSED-DATA
00524            MOVE ' '              TO WS-PASSED-CNTL-CHAR
00525            PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00526            PERFORM 3000-BUMP-LINE-COUNT
00527            MOVE ZEROS            TO PI-CONTROL-TOT
00528                                     WS-NO-CHECKS.
00529
00530      MOVE CQ-CONTROL-NUMBER      TO PI-CONTROL-SAVE-CONTROL.
00531      MOVE 'N'                    TO PI-FIRST-TIME-SWT.
00532
00533      ADD +1                      TO WS-NO-CHECKS
00534                                     PI-TOT-CHECKS.
00535      MOVE SPACES                 TO DETAIL-LINE.
00536      ADD CQ-CHECK-AMOUNT         TO PI-CONTROL-TOT
00537                                     PI-CONTROL-GRAND-TOT.
00538      MOVE CQ-CONTROL-NUMBER      TO EL685A-CONTROL.
00539      MOVE CQ-CHECK-NUMBER        TO EL685A-CHECK-NO.
00540
00541      IF CQ-BILLING-CREDIT
00542          MOVE 'BILL CREDIT'      TO EL685A-PMT-TYPE
00543      ELSE
00544          IF CQ-REFUND-PMT
00545              MOVE 'REFUND PMT '  TO EL685A-PMT-TYPE
00546          ELSE
00547              MOVE 'CHECK MAINT'  TO EL685A-PMT-TYPE.
00548
00549      IF CQ-CHECK-MAINT-PMT OR CQ-REFUND-PMT
00550          IF PI-COMPANY-ID = 'LAP'  OR  'RMC'
00551              MOVE CQ-CHEK-CARRIER    TO  EL685A-CARRIER
00552              MOVE CQ-CHEK-GROUPING   TO  EL685A-GROUP
00553              MOVE CQ-CHEK-ACCOUNT    TO  EL685A-FIN-RESP
00554              MOVE CQ-CHEK-CERT-NO    TO  EL685A-ACCOUNT
00555          ELSE
00556              MOVE CQ-CHEK-CARRIER    TO  EL685A-CARRIER
00557              MOVE CQ-CHEK-GROUPING   TO  EL685A-GROUP
00558              MOVE CQ-CHEK-FIN-RESP   TO  EL685A-FIN-RESP
00559              MOVE CQ-CHEK-ACCOUNT    TO  EL685A-ACCOUNT
00560      ELSE
00561          MOVE CQ-PYAJ-CARRIER    TO  EL685A-CARRIER
00562          MOVE CQ-PYAJ-GROUPING   TO  EL685A-GROUP
00563          MOVE CQ-PYAJ-FIN-RESP   TO  EL685A-FIN-RESP
00564          MOVE CQ-PYAJ-ACCOUNT    TO  EL685A-ACCOUNT.
00565
00566      MOVE CQ-CHECK-AMOUNT        TO  EL685A-AMT.
00567      MOVE DETAIL-LINE            TO WS-PASSED-DATA.
00568      MOVE ' '                    TO WS-PASSED-CNTL-CHAR.
00569      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00570      PERFORM 3000-BUMP-LINE-COUNT.
00571
00572      GO TO 4100-READNEXT.
00573
00574  4800-END-OF-FILE.
00575      IF PI-PREV-CHECK-QUE-KEY  = LOW-VALUES
00576         GO TO 4990-EXIT.
00577
00578      
      * EXEC CICS ENDBR
00579 *        DATASET (WS-CHECK-QUEUE-DSID)
00580 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001607' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00581
00582  4990-EXIT.
00583      EXIT.
00584      EJECT
00585  5000-BROWSE-COMM-CHECK-QUEUE SECTION.
00586
00587      
      * EXEC CICS STARTBR
00588 *        DATASET (WS-COMCK-QUEUE-DSID)
00589 *        RIDFLD  (PI-CHECK-QUE-KEY)
00590 *        GTEQ
00591 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00001616' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-COMCK-QUEUE-DSID, 
                 PI-CHECK-QUE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00592
00593      
      * EXEC CICS HANDLE CONDITION
00594 *         ENDFILE (5800-END-OF-FILE)
00595 *    END-EXEC.
      *    MOVE '"$''                   ! # #00001622' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303031363232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00596
00597  5100-READNEXT.
00598      MOVE PI-CHECK-QUE-KEY        TO  PI-PREV-CHECK-QUE-KEY.
00599
00600      
      * EXEC CICS READNEXT
00601 *        DATASET (WS-COMCK-QUEUE-DSID)
00602 *        RIDFLD  (PI-CHECK-QUE-KEY)
00603 *        SET     (ADDRESS OF COMMISSION-CHECK-QUE)
00604 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00001629' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-COMCK-QUEUE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CHECK-QUE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMMISSION-CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00605
00606      IF MQ-COMPANY-CD NOT = PI-COMPANY-CD
00607          GO TO 5800-END-OF-FILE.
00608
00609      IF MQ-TEXT
00610          GO TO 5100-READNEXT.
00611
00612      IF MQ-ENTRY-TYPE NOT = 'Q'
00613          GO TO 5100-READNEXT.
00614
00615      IF MQ-VOID-DT NOT = LOW-VALUES
00616          GO TO 5100-READNEXT.
00617
00618      IF PI-CALLING-PROGRAM IS EQUAL TO 'EL686   '
00619          IF PI-CONTROL-SAVE-CONTROL NOT EQUAL MQ-CONTROL-NUMBER
00620              IF NOT PI-FIRST-TIME
00621                  GO TO 5800-END-OF-FILE.
00622
00623      IF PI-CALLING-PROGRAM = 'EL686   '   AND
00624           PI-PGM-PRINT-OPT = '5'          AND
00625           MQ-TIMES-PRINTED NOT = ZERO
00626             GO TO 5100-READNEXT.
00627
00628      IF MQ-TIMES-PRINTED NOT = ZERO
00629          IF CHECKS-PRINTED
00630             NEXT SENTENCE
00631          ELSE
00632             GO TO 5100-READNEXT
00633      ELSE
00634          IF CHECKS-TO-BE-PRINTED
00635             NEXT SENTENCE
00636          ELSE
00637             GO TO 5100-READNEXT.
00638
00639      IF PI-CONTROL-SAVE-CONTROL NOT = MQ-CONTROL-NUMBER
00640         IF PI-FIRST-TIME
00641            NEXT SENTENCE
00642         ELSE
00643            MOVE SPACES           TO  TOTAL-LINE2
00644            MOVE 'CONTL TOTAL'    TO EL685B-CNTL-DESC
00645            MOVE PI-CONTROL-TOT   TO EL685B-CNTL-TOTAL
CIDMOD           MOVE 'NO. CHECKS '    TO EL685B-NO-CHECKS-DESC
CIDMOD           MOVE WS-NO-CHECKS     TO EL685B-NO-CHECKS
00646            MOVE TOTAL-LINE2      TO WS-PASSED-DATA
00647            MOVE ' '              TO WS-PASSED-CNTL-CHAR
00648            PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00649            PERFORM 3000-BUMP-LINE-COUNT
00650            MOVE SPACES           TO WS-PASSED-DATA
00651            MOVE ' '              TO WS-PASSED-CNTL-CHAR
00652            PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00653            PERFORM 3000-BUMP-LINE-COUNT
00654            MOVE ZEROS            TO PI-CONTROL-TOT
00655                                     WS-NO-CHECKS.
00656
00657      MOVE MQ-CONTROL-NUMBER      TO PI-CONTROL-SAVE-CONTROL.
00658      MOVE 'N'                    TO PI-FIRST-TIME-SWT.
00659
00660      ADD +1                      TO WS-NO-CHECKS
00661                                     PI-TOT-CHECKS.
00662      MOVE SPACES                 TO DETAIL-LINE.
00663      ADD MQ-CHECK-AMOUNT         TO PI-CONTROL-TOT
00664                                     PI-CONTROL-GRAND-TOT.
00665      MOVE MQ-CONTROL-NUMBER      TO EL685B-CONTROL.
00666      MOVE MQ-CHECK-NUMBER        TO EL685B-CHECK-NO.
00667      MOVE MQ-CHECK-AMOUNT        TO  EL685B-AMT.
00668      MOVE MQ-CHEK-CARRIER        TO  EL685B-CARRIER.
00669      MOVE MQ-CHEK-GROUPING       TO  EL685B-GROUP.
00670      MOVE MQ-CHEK-PAYEE          TO  EL685B-PAYEE.
00671      MOVE MQ-PAYEE-NAME          TO  EL685B-PAYEE-NA.
00672      MOVE MQ-PAYEE-SEQ-A1        TO  EL685B-PAYEE-SEQ.
00673
00674      MOVE MQ-CHECK-WRITTEN-DT    TO  DC-BIN-DATE-1.
00675      MOVE ' '                    TO  DC-OPTION-CODE.
00676      PERFORM 9950-LINK-DATE-CONVERT THRU 9950-EXIT.
00677      MOVE DC-GREG-DATE-1-EDIT    TO  EL685B-CHK-DT.
00678
00679      MOVE DETAIL-LINEB           TO WS-PASSED-DATA.
00680      MOVE ' '                    TO WS-PASSED-CNTL-CHAR.
00681      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00682      PERFORM 3000-BUMP-LINE-COUNT
00683
00684      GO TO 5100-READNEXT.
00685
00686  5800-END-OF-FILE.
00687      IF PI-PREV-CHECK-QUE-KEY  = LOW-VALUES
00688         GO TO 5990-EXIT.
00689
00690      
      * EXEC CICS ENDBR
00691 *        DATASET (WS-COMCK-QUEUE-DSID) END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001721' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-COMCK-QUEUE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00692
00693  5990-EXIT.
00694      EXIT.
00695      EJECT
00696  9500-PRINT-ROUTINE SECTION.  
      *                             COPY ELPRTCVP.
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
      *    MOVE '."C                   ''   #00001784' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373834' TO DFHEIV0(25:11)
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
      *    MOVE '$$    C E         L F ,   #00001905' TO DFHEIV0
           MOVE X'242420202020432045202020' TO DFHEIV0(1:12)
           MOVE X'2020202020204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031393035' TO DFHEIV0(25:11)
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
00697
00698
00699  9900-LAST-PARAGRAPH SECTION.
00700      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6851' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00701
00702  9950-LINK-DATE-CONVERT.
00703
00704      
      * EXEC CICS LINK
00705 *        PROGRAM    ('ELDATCV')
00706 *        COMMAREA   (DATE-CONVERSION-DATA)
00707 *        LENGTH     (DC-COMM-LENGTH)
00708 *        END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00001925' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031393235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00709  9950-EXIT.
00710      EXIT.
00711

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6851' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 200-END-DATA,
                     4800-END-OF-FILE,
                     300-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 5800-END-OF-FILE
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6851' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
