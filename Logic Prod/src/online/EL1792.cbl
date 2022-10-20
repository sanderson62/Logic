00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL1792.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/13/96 09:39:33.
00007 *                            VMOD=2.008.
00008 *AUTHOR.        LOGIC, INC.
00009 *               DALLAS, TEXAS.
00010
00011 *DATE-COMPILED.
00012
00013 *SECURITY.   *****************************************************
00014 *            *                                                   *
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00016 *            *                                                   *
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00020 *            *                                                   *
00021 *            *****************************************************
00022
00023 *REMARKS. TRANSACTION EX59 - PRINT REPORT.
00024      EJECT
00025  ENVIRONMENT DIVISION.
00026  DATA DIVISION.
00027  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00028 *77  THIS-PGM PIC X(6)  VALUE 'EL1792'.
CIDMOD 77  THIS-PGM PIC X(5)  VALUE SPACES.
00029  77  FILLER  PIC X(32)  VALUE '********************************'.
00030  77  FILLER  PIC X(32)  VALUE '*   EL1792 WORKING STORAGE     *'.
00031  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.008 *********'.
00032
00033  01  LITERALS-NUMBERS.
00034      12  LIT-X                   PIC X       VALUE 'X'.
00035      12  LIT-1                   PIC X       VALUE '1'.
00036      SKIP1
00037  01  REPT-KEY.
00038      12  REPT-COMPANY-CODE       PIC X.
00039      12  REPT-RECORD-TYPE        PIC X.
00040      12  REPT-REPORT-ID          PIC X(5).
00041      12  REPT-LINE-NUMBER        PIC S9(8)   COMP.
00042      SKIP1
00043  01  WS-TERMINAL-ID.
00044      12  WS-TERM-PREFIX          PIC XX.
00045      12  FILLER                  PIC XX.
00046  01  REMOTE-PRINTER-CTRL.
00047      12  CTRL-BUFFER-LENGTH      PIC S9(04) COMP.
00048      12  CTRL-BUFFER-AREA        PIC X(20).
00049      SKIP1
00050  01  ERROR-NUMBERS.
00051      12  ER-0275                 PIC X(4)    VALUE '0275'.
00052      12  ER-0424                 PIC X(4)    VALUE '0424'.
00053      12  ER-8332                 PIC X(4)    VALUE '8332'.
00054      12  ER-8333                 PIC X(4)    VALUE '8333'.
00055      SKIP1
00056  01  COMP-LENGTHS.
00057      12  COMP-132                PIC S9(4)   COMP VALUE +132.
00058 *                                COPY ELCDMD34.
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
00059      EJECT
00060 *                                COPY ELCLOGOF.
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
00061      EJECT
00062 *                                COPY ELCINTF.
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
00063      12  EL1792-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.
00064          16  PI-PRINT-REPORT     PIC X(5).
00065          16  PI-START-COUNT      PIC S9(8)  COMP.
00066          16  PI-TOTAL-COUNT      PIC S9(8)  COMP.
00067          16  PI-START-PAGE-NO    PIC S9(4).
00068          16  PI-ACCUM-PAGE-NO    PIC S9(4).
00069          16  PI-END-PAGE-NO      PIC S9(4).
00070 *        16  PI-FORMS-PRINTER-ID PIC X(4).
00070          16  PI-PRINT-ID         PIC X(4).
00071          16  FILLER              PIC X(611).
00072      EJECT
00073 *                                COPY ELCEMIB.
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
00074      EJECT
00075 *                                COPY ELPRTCVD.
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
00076      EJECT
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
00078  01  DFHCOMMAREA                 PIC X(1024).
00079
00080 *                                COPY ELCREPT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCREPT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = REPORT STORAGE                            *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 146   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELREPT                 RKP=2,LEN=11      *
00013 *       ALTERNATE PATH  = NOT USED                               *
00014 *                                                                *
00015 *   LOG = NO                                                     *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  REPORT-SAVE-FILE.
00019      12  RF-RECORD-ID                PIC XX.
00020          88  VALID-RF-ID                VALUE 'RF'.
00021
00022      12  RF-CONTROL-PRIMARY.
00023          16  RF-COMPANY-CD           PIC X.
00024          16  RF-RECORD-TYPE          PIC X.
00025              88  REPORT-DETAIL-RECORD   VALUE '1'.
00026              88  REPORT-TRAILER-RECORD  VALUE '2'.
00027          16  RF-REPORT-ID.
00028              20  RF-SYSTEM-CODE      PIC XX.
00029                  88  CLAS-IC-ONLINE     VALUE 'EL'.
00030                  88  CLAS-SCS-BATCH     VALUE 'EC'.
00031              20  RF-PROGRAM-SEQUENCE PIC 999.
00032          16  RF-LINE-NUMBER          PIC S9(8)       COMP.
00033      12  RF-REPORT-LINE-133.
00034          16  RF-CTL-CHAR-133         PIC X.
00035          16  RF-DATA-133             PIC X(132).
00036          16  RF-DATA-FIRST  REDEFINES RF-DATA-133.
00037              20  RF-DATA-2-81        PIC X(80).
00038              20  FILLER              PIC X(52).
00039          16  RF-DATA-LAST   REDEFINES RF-DATA-133.
00040              20  FILLER              PIC X(53).
00041              20  RF-DATA-55-133      PIC X(79).
00042      12  RF-TRAILER-RECORD  REDEFINES RF-REPORT-LINE-133.
00043          16  FILLER                  PIC X.
00044          16  RF-CURRENT-DATE         PIC X(8).
00045          16  RF-PRINT-HH-MM-SS       PIC X(6).
00046          16  FILLER                  PIC X(115).
00047          16  RF-COMPANY-ID           PIC XXX.
00048 ******************************************************************
00081      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA REPORT-SAVE-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1792' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00083
00084      MOVE COMP-132               TO WS-LINE-LEN.
00085      MOVE SPACES                 TO DL34-PROCESS-TYPE.
CIDMOD*    MOVE 'N'                    TO CSO-PRINT-STARTED-SW.
00086
00087  0010-RETREIVE-REQUEST.
00088
00089      
      * EXEC CICS HANDLE CONDITION
00090 *        NOTOPEN  (0100-FILE-NOT-OPEN)
00091 *        ENDDATA  (9100-RETURN-CICS)
00092 *        NOTFND   (0200-COMM-AREA-NOT-FOUND)
00093 *        ERROR    (9990-ABEND)
00094 *    END-EXEC.
      *    MOVE '"$J&I.                ! " #00000691' TO DFHEIV0
           MOVE X'22244A26492E202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303030363931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00095
00096      
      * EXEC CICS RETRIEVE
00097 *        INTO    (PROGRAM-INTERFACE-BLOCK)
00098 *        LENGTH  (PI-COMM-LENGTH)
00099 *    END-EXEC.
      *    MOVE '0*I L                 &   #00000698' TO DFHEIV0
           MOVE X'302A49204C20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303030363938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00100
CIDMOD     MOVE PI-PRINT-REPORT TO THIS-PGM.
CIDMOD
00101 * DLO034 OPEN WHEN DMD OR CID
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
00103          IF DL34-PROCESS-TYPE = SPACES
00104              MOVE 'O'                TO DL34-PROCESS-TYPE
00105              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
00106              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
00107              MOVE PI-PROCESSOR-ID    TO DL34-USERID
00108              MOVE SPACES             TO DL34-PRINT-LINE
00109              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID
00110              
      * EXEC CICS LINK
00111 *                PROGRAM    ('DLO034')
00112 *                COMMAREA   (DLO034-COMMUNICATION-AREA)
00113 *                LENGTH     (DLO034-REC-LENGTH)
00114 *            END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   ''   #00000714' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303030373134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00115              IF DL34-RETURN-CODE NOT = 'OK'
00116                  MOVE '8332'         TO EMI-ERROR
00117                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00118                  
      * EXEC CICS RETURN
00119 *                END-EXEC.
      *    MOVE '.(                    &   #00000722' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303030373232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00120
CIDMOD*    MOVE 'F' TO DRS-SW.
CIDMOD*    MOVE PI-PRINT-ID TO CSO-PRINT-ID.
CIDMOD*    PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
CIDMOD*    MOVE ' ' TO DRS-SW.
CIDMOD*
00121      MOVE SPACE                  TO WS-PROG-END.
00122      MOVE PI-COMPANY-CD          TO REPT-COMPANY-CODE.
00123      MOVE LIT-1                  TO REPT-RECORD-TYPE.
00124      MOVE PI-PRINT-REPORT        TO REPT-REPORT-ID.
00125      MOVE PI-START-COUNT         TO REPT-LINE-NUMBER.
00126
00127  0020-START-BROWSE.
00128
00129      
      * EXEC CICS STARTBR
00130 *        DATASET  ('ELREPT')
00131 *        RIDFLD   (REPT-KEY)
00132 *    END-EXEC.
           MOVE 'ELREPT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00000738' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303030373338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 REPT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00133
00143      PERFORM 1000-PRINT-REPORT THRU 1000-PRINT-REPORT-EXIT.
00144
00154      
      * EXEC CICS ENDBR
00155 *        DATASET ('ELREPT')
00156 *    END-EXEC.
           MOVE 'ELREPT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00000745' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303030373435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00157
CIDMOD     GO TO 0010-RETREIVE-REQUEST.
CIDMOD*    GO TO 9100-RETURN-CICS.
00159      EJECT
00160  0100-FILE-NOT-OPEN.
00161      MOVE ER-0275                TO EMI-ERROR.
00162      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00163      MOVE EMI-MESSAGE-AREA (1)   TO WS-PRINT-AREA.
CIDMOD     PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00165      MOVE LIT-X                  TO WS-PROG-END.
CIDMOD*
CIDMOD*    MOVE 'L' TO DRS-SW.
CIDMOD*
CIDMOD     PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00167      GO TO 9100-RETURN-CICS.
00168      EJECT
00169  0200-COMM-AREA-NOT-FOUND.
00170      MOVE ER-0424                TO EMI-ERROR.
00171      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00172      MOVE EMI-MESSAGE-AREA (1)   TO WS-PRINT-AREA.
00173      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00174      MOVE LIT-X                  TO WS-PROG-END.
CIDMOD*
CIDMOD*    MOVE 'L' TO DRS-SW.
CIDMOD*
00175      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00176      GO TO 9100-RETURN-CICS.
00177      EJECT
00178  1000-PRINT-REPORT.
00179
00180      
      * EXEC CICS HANDLE CONDITION
00181 *        ENDFILE (1000-END-OF-FILE)
00182 *    END-EXEC.
      *    MOVE '"$''                   ! # #00000778' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303030373738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00183
00184      
      * EXEC CICS READNEXT
00185 *        SET      (ADDRESS OF REPORT-SAVE-FILE)
00186 *        DATASET  ('ELREPT')
00187 *        RIDFLD   (REPT-KEY)
00188 *    END-EXEC.
           MOVE 'ELREPT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00000782' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303030373832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 REPT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF REPORT-SAVE-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00189
00190      IF RF-COMPANY-CD  NOT = PI-COMPANY-CD  OR
00191         RF-RECORD-TYPE NOT = LIT-1          OR
00192         RF-REPORT-ID   NOT = PI-PRINT-REPORT
00193          GO TO 1000-END-OF-FILE.
00194
00195      IF PI-END-PAGE-NO GREATER ZERO
00196          IF RF-CTL-CHAR-133 = '1'
00197              ADD +1 TO PI-ACCUM-PAGE-NO
00198              IF PI-ACCUM-PAGE-NO GREATER PI-END-PAGE-NO
00199                  GO TO 1000-END-OF-FILE.
00200
00201      MOVE RF-REPORT-LINE-133     TO WS-PRINT-AREA.
CIDMOD     PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00203      GO TO 1000-PRINT-REPORT.
00204
00205  1000-END-OF-FILE.
00206      MOVE LIT-X                  TO WS-PROG-END.
00207
CIDMOD     IF PI-COMPANY-ID NOT = 'DMD' OR 'CID'
00209          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00210
00211  1000-PRINT-REPORT-EXIT.
00212      EXIT.
00213      EJECT
uktdel*8000-STANDARD-PRINT-ROUTINE.    COPY ELPRTCVP.
uktins 8000-STANDARD-PRINT-ROUTINE.
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
      *    MOVE '."C                   ''   #00000870' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303030383730' TO DFHEIV0(25:11)
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
      *    MOVE '$$    C E         L F ,   #00000991' TO DFHEIV0
           MOVE X'242420202020432045202020' TO DFHEIV0(1:12)
           MOVE X'2020202020204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303030393931' TO DFHEIV0(25:11)
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
00215
00216      EJECT
00217  9100-RETURN-CICS.
00218 ******   WHEN PRINTING TO A 3275 DIAL-UP TERMINAL
00219 ******   THE TRANSID AND COMMAREA MUST BE RESET
00220 ******   UPON COMPLETION OF THE TASK.
CIDMOD*
CIDMOD*    MOVE 'L' TO DRS-SW.
CIDMOD*    PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
CIDMOD*
00221      MOVE EIBTRMID               TO WS-TERMINAL-ID.
00222
00223 * DLO034 CLOSE
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
00225          MOVE 'C'                TO DL34-PROCESS-TYPE
00226          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
00227          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
00228          MOVE PI-PROCESSOR-ID    TO DL34-USERID
00229          MOVE SPACES             TO DL34-PRINT-LINE
00230                                     DL34-OVERRIDE-PRINTER-ID
00231          
      * EXEC CICS LINK
00232 *            PROGRAM    ('DLO034')
00233 *            COMMAREA   (DLO034-COMMUNICATION-AREA)
00234 *            LENGTH     (DLO034-REC-LENGTH)
00235 *        END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   ''   #00001024' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031303234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00236          IF DL34-RETURN-CODE NOT = 'OK'
00237              MOVE '8333'         TO EMI-ERROR
00238              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00239              
      * EXEC CICS RETURN
00240 *            END-EXEC.
      *    MOVE '.(                    &   #00001032' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031303332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00241
00242      IF WS-TERM-PREFIX = 'DU'
00243         
      * EXEC CICS RETURN
00244 *            TRANSID  ('EX49')
00245 *            COMMAREA (PROGRAM-INTERFACE-BLOCK)
00246 *            LENGTH   (PI-COMM-LENGTH)
00247 *       END-EXEC
           MOVE 'EX49' TO DFHEIV5
      *    MOVE '.(CT                  &   #00001036' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031303336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV5, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00248      ELSE
00249         
      * EXEC CICS  RETURN
00250 *       END-EXEC.
      *    MOVE '.(                    &   #00001042' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031303432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00251
CIDMOD     MOVE ZEROS     TO RETURN-CODE.
00252      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1792' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00253
00254  9900-ERROR-FORMAT.
00255
00256      IF EMI-ERRORS-COMPLETE
00257          GO TO 9900-EXIT.
00258
00259      
      * EXEC CICS LINK
00260 *        PROGRAM   ('EL001')
00261 *        COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
00262 *        LENGTH    (EMI-COMM-LENGTH)
00263 *    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   ''   #00001053' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031303533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00264
00265  9900-EXIT.
00266      EXIT.
00267
00268  9990-ABEND.
00269      MOVE DFHEIBLK               TO LOGOFF-FILL.
00270
00271      
      * EXEC CICS LINK
00272 *        PROGRAM   ('EL004')
00273 *        COMMAREA  (LOGOFF-FILL)
00274 *        LENGTH    (66)
00275 *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 66
             TO DFHEIV11
      *    MOVE '."C                   ''   #00001065' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031303635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 LOGOFF-FILL, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00276
00277      GO TO 9100-RETURN-CICS.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1792' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 0100-FILE-NOT-OPEN,
                     9100-RETURN-CICS,
                     0200-COMM-AREA-NOT-FOUND,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 1000-END-OF-FILE
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1792' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
