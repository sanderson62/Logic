00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL6892.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 09:59:40.
00007 *                            VMOD=2.010.
00008 *
00009 *AUTHOR.           LOGIC,INC.
00010 *                  DALLAS,TEXAS.
00011
00012 *DATE-COMPILED.
00013
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            * THIS PROGRAM IS THE PROPERTY OF LOCIC, INC. *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023
00024 *REMARKS. TRANSACTION EXH5 - HARDCOPY PRINT PROGRAM
00025 *        THIS PROGRAM IS USED TO PRINT LETTERS TO THE COMPANY
00026 *        SPECIFIED PRINTER. IT IS ACTIVATED THROUGH THE START
00027 *        COMMAND AND WILL HAVE THE INTERFACE BLOCK PASSED.
00028 *        DATA IN THE INTERFACE BLOCK WILL INDICATE THE NUMBER OF
00029 *        LINES TO PRINT AND THE NUMBER OF COPIES.
00030 *        IT WILL ALSO CONTAIN THE TEMP STORAGE ID TO USE IN ORDER
00031 *        TO RETRIEVE THE DATA TO BE PRINTED.
031011******************************************************************
031011*                   C H A N G E   L O G
031011*
031011* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031011*-----------------------------------------------------------------
031011*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031011* EFFECTIVE    NUMBER
031011*-----------------------------------------------------------------
031011* 031011  CR2007070900001  PEMA  ADD FOLLOW-UP LETTER PROCESSING
031011******************************************************************
00035  ENVIRONMENT DIVISION.
00036  DATA DIVISION.
00037  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00038  77  THIS-PGM PIC X(8) VALUE 'EL6892'.
00039  77  FILLER  PIC X(32) VALUE '********************************'.
00040  77  FILLER  PIC X(32) VALUE '*   EL6892 WORKING STORAGE     *'.
00041  77  FILLER  PIC X(32) VALUE '******* VMOD=2.010 *************'.
00042
00043  01  W-CONSTANTS.
00044      12  FILLER                  PIC  X(18)
00045                              VALUE 'PROGRAM CONSTANTS:'.
00046
00047      12  W-ENTRIES-PER-RECORD    PIC  9(03) COMP-3 VALUE 50.
00048      12  W-TS-LENGTH             PIC S9(04) COMP VALUE +3650.
00049
00050      12  W-PGM-EL689             PIC  X(08) VALUE 'EL689'.
00051      12  W-TOP-OF-FORM-MESSAGE   PIC  X(08) VALUE '*****TOP'.
00052      12  W-TOP-FORM              PIC  X(01) VALUE '1'.
00053
00054  01  W-WORK-AREAS.
00055      12  FILLER                  PIC  X(18)
00056                              VALUE 'PROGRAM WORK AREA:'.
00057
00058      12  W-ASKTIME-CTR           PIC S9(04)  COMP.
00059      12  W-NDX                   PIC  9(02).
00060      12  W-RECORDS-PRINTED       PIC S9(03) COMP-3 VALUE 0.
00061      12  W-TS-ITEM               PIC  S9(04) COMP.
00062
00063      12  W-ERROR-LINE            PIC  X(70).
00064      12  W-NEXT-TRAN             PIC  X(04).
00065      12  W-TERMINAL-ID.
00066          18  W-TERM-PREFIX       PIC  X(02).
00067          18  FILLER              PIC  X(02).
00068
00069      12  W-ADJUST-AREA.
00070          16  FILLER              PIC  X(07).
00071          16  W-AD-PRINT-AREA     PIC  X(70).
00072          16  FILLER              PIC  X(08).
00073
00074      12  W-TS-WORK-AREA          PIC X(3650).
00075      12  W-REC-ENTRIES REDEFINES W-TS-WORK-AREA.
00076          16  W-REC-ENT OCCURS 50 TIMES INDEXED BY W-TB-NDX.
00077              20  W-REC-TEXT.
00078                  24  W-REC-TEXT-TOP
00079                                  PIC  X(08).
00080                  24  FILLER      PIC  X(62).
00081              20  W-REC-PC        PIC  9(02).
00082              20  W-REC-SC        PIC  X(01).
00083                                   EJECT
00084  01  FILLER                      PIC  X(25)
00085                              VALUE 'PROGRAM INTERFACE STARTS:'.
00086 *    COPY ELCINTF.
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
00087      12  PI-WA  REDEFINES PI-PROGRAM-WORK-AREA.
00088 *    COPY ELC1042.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                           ELC1042                              *
00004 *                            VMOD=2.002                          *
00005 *                                                                *
00006 *    NOTE                                                        *
00007 *        THE WORK AREA IS USED BY EL152, EL1522, EL1042, EL153,  *
00008 *        EM152, EM1522, EL689, EL6892, EL6311, AND EL690.        *
00009 *        THIS COPYBOOK SHOULD NOT BE CHANGED WITHOUT REFERENCE   *
00010 *        TO THESE PROGRAMS.                                      *
00011 *                                                                *
00012 *    NOTE                                                        *
00013 *        THE FILLER AREA AT THE BOTTOM ARE FOR FUTURE EL1042     *
00014 *        USE ONLY!                                               *
00015 *                                                                *
00016 ******************************************************************
00017
00018          16  PI-1042-WA.
00019              20  PI-ACTION       PIC  X(01).
00020                  88 PI-SHOW-MODE           VALUE '1'.
00021                  88 PI-CLEAR-MODE          VALUE '2'.
00022                  88 PI-CREATE-MODE         VALUE '3'.
00023              20  PI-COMM-CONTROL PIC  X(12).
00024              20  PI-CURRENT-LINE PIC S9(03) COMP-3.
00025              20  PI-EOF-SW       PIC  X(01).
00026                  88  PI-FILE-EOF           VALUE 'Y'.
00027              20  PI-FILETYP      PIC  X(01).
00028              20  PI-FORM-SQUEEZE-CONTROL
00029                                  PIC  X(01).
00030                  88  PI-FORM-SQUEEZE-ON     VALUE 'Y'.
00031                  88  PI-FORM-SQUEEZE-OFF    VALUE ' '.
00032              20  PI-LAST-CONTROL PIC  X(12).
00033              20  PI-TEMP-STOR-ITEMS
00034                                  PIC S9(04) COMP.
00035              20  PI-TOTAL-LINES  PIC S9(03) COMP-3.
00036              20  PI-UPDATE-SW    PIC  9(01).
00037                  88 ANY-UPDATES            VALUE 1.
00038              20  PI-104-SCREEN-SENT-IND
00039                                  PIC  X(01).
00040                  88  PI-104-SCREEN-SENT    VALUE 'Y'.
00041                  88  PI-104-SCREEN-NOT-SENT VALUE 'N'.
00042              20  PI-1042-SCREEN-SENT-IND
00043                                  PIC  X(01).
00044                  88  PI-1042-SCREEN-SENT    VALUE 'Y'.
00045                  88  PI-1042-SCREEN-NOT-SENT VALUE 'N'.
00046              20  PI-1042-ARCHIVE-IND
00047                                  PIC  X(01).
00048                  88  PI-1042-ARCHIVE-LETTER VALUE 'Y'.
00049              20  FILLER          PIC  X(29).
00089 *    COPY ELC689PI.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELC689PI                            *
00004 *                            VMOD=2.003                          *
00005 *                                                                *
00006 *    THIS IS THE PI-PROGRAM-WORK-AREA THAT IS USED FOR THE       *
00007 *    CREDIT CORRESPONDENCE SUB-SYSTEM.  ANY CHANGES WILL         *
00008 *    WILL EFFECT THE PROGRAMS OF THAT SUB-SYSTEM.                *
00009 *                                                                *
00010 *    IF THE LENGTH OF THIS PI-AREA CHANGES THE LENGTH MUST       *
00011 *    BE CHANGED FOR THE COMM-AREA WHEN PASSING THIS PI-AREA      *
00012 *    BETWEEN PROGRAMS.                                           *
00013 *                                                                *
00014 *    THE FOLLOWING PROGRAMS USE THIS COPYBOOK:                   *
00015 *                                                                *
00016 *               EL631 - EL689  - EL6891 - EL6892                 *
00017 *                                                                *
00018 ******************************************************************
081004*                   C H A N G E   L O G
081004*
081004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
081004*-----------------------------------------------------------------
081004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
081004* EFFECTIVE    NUMBER
081004*-----------------------------------------------------------------
081004* 081004                   PEMA  CONVERT TO PSUEDO CONVERSATIONAL
100705* 100705  CR2004072800004  PEMA  ADD LETTERS TO BE RESENT
031011* 031011  CR2007070900001  PEMA  ADD FOLLOW-UP LETTER PROCESSING
081004******************************************************************
00019
00020
00021          16  PI-689-WORK-AREA.
00022              20  PI-689-ALT-PRINTER-ID
00023                                  PIC  X(04).
00024              20  PI-689-ARCHIVE-NUMBER
00025                                  PIC  9(08).
00026              20  PI-689-ARCHIVE-SW
00027                                  PIC  X(01).
00028                  88  PI-689-ARCHIVE-LETTER VALUE 'Y'.
00029              20  PI-689-DATA-SOURCE
00030                                  PIC  X(01).
00031                  88  PI-689-SRC-ACCOUNT        VALUE '1'.
00032                  88  PI-689-SRC-CERTIFICATE    VALUE '2'.
00033                  88  PI-689-SRC-COMPENSATION   VALUE '3'.
00034                  88  PI-689-SRC-PEND-BUSINESS  VALUE '4'.
00035                  88  PI-689-SRC-CHECKS         VALUE '5'.
00036              20  PI-689-ERROR-IND
00037                                  PIC  X(01).
00038                  88  PI-689-ERR-DETECTED-PREV  VALUE 'Y'.
00039              20  PI-689-ERROR    PIC  9(04).
00040                  88  PI-689-NO-ERRORS-DETECTED VALUE 0000.
00041                  88  PI-689-FATAL-ERROR
00042                      VALUES 0004 0006 0008 0013 0023 0029 0033
00043                             0042 0047 0051 0066 0067 0070
00044                             0168 0169 0174 0175 0176 0177 0179
00045                             0180 0181 0182 0184 0185 0189 0190
00046                             0191
00047                             0215 0279 0280
00048                             0412 0413 0454
00049                             0533 0537
00050                             2055 2114 2208 2209 2216 2232 2369
00051                             2398 2433 2908 2999
00052                             3000 3770 3771 3775
00053                             7250 7365 7367 7368 7369 7370 7371
00054                             7272 7373 7374 7376 7377 7378 7379
00055                             7381 7388 7390 7393 7395 7396 7398
00056                             9095 9096 9281 9298 9299 9320 9327
00057                             9426 9427.
00058                  88  PI-689-STOP-ERROR
00059                      VALUES 0004 0008 0013 0023 0029 0033
00060                             0042 0047 0066 0067 0070
00061                             0168 0169 0174 0175 0176 0177
00062                             0181 0182 0184 0185 0189 0190
00063                             0279 0280
00064                             0412 0413 0454
00065                             2055 2208 2209 2216 2232
00066                             2398 2999
00067                             3000 3770 3771 3775
00068                             7250 7365 7369 7370 7371
00069                             7272 7373 7374 7376 7377 7378 7379
00070                             7381 7388 7390 7393 7396 7398
00071                             9095 9096 9299 9320 9426.
00072              20  PI-689-FOLLOW-UP-DATE
00073                                  PIC  X(02).
00074              20  PI-689-FORM-NUMBER
00075                                  PIC  X(04).
00076              20  PI-689-LABEL-SOURCE
00077                                  PIC X(01).
00078                  88  PI-689-SOURCE-ACCOUNT  VALUE '1'.
00079                  88  PI-689-SOURCE-CARRIER  VALUE '2'.
00080                  88  PI-689-SOURCE-COMPANY  VALUE '3'.
00081                  88  PI-689-SOURCE-COMP     VALUE '4'.
00082                  88  PI-689-SOURCE-MAIL     VALUE '5'.
00083                  88  PI-689-SOURCE-CHECK    VALUE '6'.
00084                  88  PI-689-SOURCE-VARIABLE VALUE '7'.
00085              20  PI-689-NUMBER-COPIES
00086                                  PIC  9(01).
00087              20  PI-689-NUMBER-LABEL-LINES
00088                                  PIC  9(01).
00089              20  PI-689-NUMBER-TEXT-RECORDS
00090                                  PIC  9(03).
00091              20  PI-689-PRINT-ORDER-SW
00092                                  PIC  X(01).
00093                  88  PI-689-PRINT-FIRST     VALUE '1'.
00094                  88  PI-689-PRINT-SECOND    VALUE '2'.
00095                  88  PI-689-PRINT-LATER     VALUE '3'.
00096                  88  PI-689-PRINT-ONLY      VALUE '4'.
00097              20  PI-689-PRINT-RESTRICTION
00098                                  PIC  X(01).
00099                  88  PI-689-VALID-RESTRICT     VALUE 'C' 'F'.
00100                  88  PI-689-PRT-ONLY-WITH-CNTL VALUE 'C'.
00101                  88  PI-689-PRT-ONLY-WITH-FORM VALUE 'F'.
00102              20  PI-689-PRINT-SW PIC  X(01).
00103                  88  PI-689-PRINT-PERFORMED VALUE '1'.
00104              20  PI-689-RESEND-DATE-1
00105                                  PIC  X(02).
100705             20  PI-689-RESEND-LETR-1
                                       PIC X(4).
00110              20  PI-689-TEMP-STOR-ID
00111                                  PIC  X(08).
00112              20  PI-689-USE-SCREEN-IND
00113                                  PIC  X(01).
00114                  88  PI-689-CREATE-NO-SCREENS VALUE '1'.
00115              20  PI-689-ARCH-POINTER
00116                                  PIC S9(08) COMP.
00117                  88  PI-689-GET-ARCH-MAIN     VALUE +0.
00118              20  PI-689-ARCT-POINTER
00119                                  PIC S9(08) COMP.
00120                  88  PI-689-GET-ARCT-MAIN     VALUE +0.
00121              20  PI-689-VARIABLE-DATA-GRP.
00122                  24  PI-689-VARIABLE-DATA-1
00123                                  PIC  X(30).
00124                  24  PI-689-VARIABLE-DATA-2
00125                                  PIC  X(30).
00126                  24  PI-689-VARIABLE-DATA-3
00127                                  PIC  X(30).
00128                  24  PI-689-VARIABLE-DATA-4
00129                                  PIC  X(30).
00130
00131          16  PI-689-KEY-DATA-FIELDS.
00132              20  PI-689-ACCOUNT  PIC  X(10).
00133              20  PI-689-CARRIER  PIC  X(01).
00134              20  PI-689-CERT-NO.
00135                  24  PI-689-CERT-PRIME
00136                                  PIC  X(10).
00137                  24  PI-689-CERT-SFX
00138                                  PIC  X(01).
00139              20  PI-689-CHG-SEQ-NO
00140                                  PIC S9(04)    COMP.
00141              20  PI-689-CHG-SEQ-NOX REDEFINES PI-689-CHG-SEQ-NO
00142                                  PIC  X(02).
00143              20  PI-689-ENTRY-BATCH
00144                                  PIC  X(06).
00145              20  PI-689-EFF-DATE PIC  X(02).
00146              20  PI-689-EXP-DATE PIC  X(02).
00147              20  PI-689-GROUPING PIC  X(06).
00148              20  PI-689-RESP-PERSON
00149                                  PIC  X(10).
00150              20  PI-689-SEQ-NO   PIC S9(08)    COMP.
00151              20  PI-689-SEQ-NOX REDEFINES PI-689-SEQ-NO
00152                                  PIC  X(04).
00153              20  PI-689-STATE    PIC  X(02).
00154              20  PI-689-TYPE     PIC  X(01).
00155              20  PI-689-CONTROL  PIC S9(08)    COMP.
00156              20  PI-689-ALT-SEQ-NO
00157                                  PIC S9(04)    COMP.
00158          16  PI-689-DATE-EDIT    PIC  X(08).
00159          16  PI-689-FOLLOW-UP-EDIT
00160                                  PIC  X(08).
00161          16  PI-689-RESEND1-EDIT PIC  X(08).
00164          16  PI-689-SEQ-EDIT     PIC  X(08).
00165          16  PI-689-BCSEQ-EDIT   PIC  X(04).
00166          16  PI-689-LBL-OVERRIDE PIC  X(01).
00167              88  PI-689-LABELS-OVERRIDEN  VALUES 'N'.
081004         16  PI-689-FATAL-CTR    PIC 999     COMP-3.
081004         16  PI-689-FORCABLE-CTR PIC 999     COMP-3.
00090      16  FILLER                  PIC X(280).
00091                                   EJECT
00092  01  FILLER                      PIC  X(23)
00093                              VALUE 'PRINT WORK AREA STARTS:'.
00094 *    COPY ELPRTCVD.
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
00095  01  FILLER                      PIC  X(21)
00096                              VALUE 'PRINT WORK AREA ENDS:'.
00097                                   EJECT
00098 *    COPY ELCDMD34.
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
00099
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
       01  DFHCOMMAREA       PIC X(01).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6892' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00101
00102 *    MOVE +132                   TO  WS-LINE-LEN.
00103      MOVE +85                    TO  WS-LINE-LEN.
00104      MOVE SPACES                 TO DL34-PROCESS-TYPE.
00105
00106  0100-RETRIEVE-LOOP.
00107
00108      
      * EXEC CICS HANDLE CONDITION
00109 *         ENDDATA  (0200-END-DATA)
00110 *         NOTFND   (0300-NOT-FOUND)
00111 *    END-EXEC.
      *    MOVE '"$&I                  ! " #00000790' TO DFHEIV0
           MOVE X'222426492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303030373930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00112
00113      
      * EXEC CICS RETRIEVE
00114 *         INTO      (PROGRAM-INTERFACE-BLOCK)
00115 *         LENGTH    (PI-COMM-LENGTH)
00116 *    END-EXEC.
      *    MOVE '0*I L                 &   #00000795' TO DFHEIV0
           MOVE X'302A49204C20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303030373935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00117
00118 * DLO034 OPEN WHEN DMD OR CID
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
00120          IF DL34-PROCESS-TYPE IS EQUAL TO SPACES
00121              MOVE 'O'                TO DL34-PROCESS-TYPE
00122              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
00123              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
00124              MOVE PI-PROCESSOR-ID    TO DL34-USERID
00125              MOVE SPACES             TO DL34-PRINT-LINE
00126              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID
00127              
      * EXEC CICS LINK
00128 *                PROGRAM    ('DLO034')
00129 *                COMMAREA   (DLO034-COMMUNICATION-AREA)
00130 *                LENGTH     (DLO034-REC-LENGTH)
00131 *            END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   ''   #00000809' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303030383039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00132              IF DL34-RETURN-CODE NOT = 'OK'
00133                  MOVE  '**DLO034 OPEN ERROR - ABORT**'
00134                                      TO W-ERROR-LINE
00135                  PERFORM 0400-SEND-TEXT
00136                  
      * EXEC CICS RETURN
00137 *                END-EXEC.
      *    MOVE '.(                    &   #00000818' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303030383138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00138
00139      IF  PI-CALLING-PROGRAM = W-PGM-EL689
00140          GO TO 1000-PRINT-EL689.
00141
00142      GO TO 0100-RETRIEVE-LOOP.
00143                                   EJECT
00144  0200-END-DATA.
CIDMOD*    MOVE 'L'                    TO DRS-SW.
CIDMOD*    PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00146      MOVE 'MX27'                 TO W-NEXT-TRAN.
00147
00148      MOVE EIBTRMID               TO W-TERMINAL-ID
00149
00150 * DLO034 CLOSE
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
00152          MOVE 'C'                TO DL34-PROCESS-TYPE
00153          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
00154          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
00155          MOVE PI-PROCESSOR-ID    TO DL34-USERID
00156          MOVE SPACES             TO DL34-PRINT-LINE
00157                                     DL34-OVERRIDE-PRINTER-ID
00158          
      * EXEC CICS LINK
00159 *            PROGRAM    ('DLO034')
00160 *            COMMAREA   (DLO034-COMMUNICATION-AREA)
00161 *            LENGTH     (DLO034-REC-LENGTH)
00162 *        END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   ''   #00000841' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303030383431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00163          IF DL34-RETURN-CODE NOT = 'OK'
00164              MOVE  '**DLO034 CLOSE ERROR - ABORT**'
00165                                  TO W-ERROR-LINE
00166              PERFORM 0400-SEND-TEXT
00167              
      * EXEC CICS RETURN
00168 *                 TRANSID  (W-NEXT-TRAN)
00169 *                 COMMAREA (PROGRAM-INTERFACE-BLOCK)
00170 *                 LENGTH   (PI-COMM-LENGTH)
00171 *            END-EXEC.
      *    MOVE '.(CT                  &   #00000850' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303030383530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-NEXT-TRAN, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00172
00173      IF  W-TERM-PREFIX = 'DU'
00174          
      * EXEC CICS RETURN
00175 *             TRANSID  (W-NEXT-TRAN)
00176 *             COMMAREA (PROGRAM-INTERFACE-BLOCK)
00177 *             LENGTH   (PI-COMM-LENGTH)
00178 *        END-EXEC
      *    MOVE '.(CT                  &   #00000857' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303030383537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-NEXT-TRAN, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00179      ELSE
00180          
      * EXEC CICS RETURN
00181 *        END-EXEC.
      *    MOVE '.(                    &   #00000863' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303030383633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00182
00183  0300-NOT-FOUND.
00184
00185      MOVE 'NO COMMUNICATION AREA FOUND' TO W-ERROR-LINE
00186      PERFORM 0400-SEND-TEXT
00187      GO TO 0200-END-DATA.
00188
00189  0400-SEND-TEXT.
00190
00191      
      * EXEC CICS SEND TEXT
00192 *         FROM    (W-ERROR-LINE)
00193 *         LENGTH  (70)
00194 *    END-EXEC.
           MOVE 70
             TO DFHEIV11
      *    MOVE '8&      T       H   F -   #00000874' TO DFHEIV0
           MOVE X'382620202020202054202020' TO DFHEIV0(1:12)
           MOVE X'202020204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303030383734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ERROR-LINE, 
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
           
00195                                   EJECT
00196  1000-PRINT-EL689.
CIDMOD*    MOVE PI-PROCESSOR-PRINTER TO  CSO-PRINT-ID.
CIDMOD*    MOVE 'F'       TO  DRS-SW.
CIDMOD*    PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
CIDMOD*    MOVE ' '       TO  DRS-SW.
00197
00198      
      * EXEC CICS HANDLE CONDITION
00199 *         QIDERR  (1090-TS-QIDERR)
00200 *         ITEMERR (1089-TS-ITEMERR)
00201 *    END-EXEC.
      *    MOVE '"$N<                  ! # #00000885' TO DFHEIV0
           MOVE X'22244E3C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303030383835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00202
CIDMOD     IF  PI-CREATE-LABELS
CIDMOD         SUBTRACT PI-689-NUMBER-LABEL-LINES
CIDMOD             FROM PI-TOTAL-LINES.
CIDMOD*    SUBTRACT 7 FROM PI-TOTAL-LINES.
00206
00207  1005-READ-TEMP.
00208
00209      PERFORM 1010-TEMP-READ THRU 1019-EXIT
00210              VARYING
00211          W-TS-ITEM FROM 1 BY 1
00212              UNTIL
00213          W-TS-ITEM GREATER THAN PI-TEMP-STOR-ITEMS.
00214
00215      MOVE 'X'                    TO WS-PROG-END.
CIDMOD*    MOVE SPACES                 TO WS-PASSED-DATA
CIDMOD*                                   WS-PASSED-CNTL-CHAR.
00216      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00217      MOVE ZEROS                  TO W-RECORDS-PRINTED.
00218      SUBTRACT 1 FROM PI-689-NUMBER-COPIES.
00219
00220      IF  PI-689-NUMBER-COPIES = 0
00221          PERFORM 7750-DELETE-TEMP-STOR THRU 7750-EXIT
00222          GO TO 0100-RETRIEVE-LOOP.
00223
00224      GO TO 1005-READ-TEMP.
00225                                   EJECT
00226  1010-TEMP-READ.
00227
00228      
      * EXEC CICS READQ TS
00229 *         INTO    (W-TS-WORK-AREA)
00230 *         QUEUE   (PI-689-TEMP-STOR-ID)
00231 *         LENGTH  (W-TS-LENGTH)
00232 *         ITEM    (W-TS-ITEM)
00233 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00000918' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303030393138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-689-TEMP-STOR-ID, 
                 W-TS-WORK-AREA, 
                 W-TS-LENGTH, 
                 W-TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00234
00235
00236      IF  W-TS-ITEM = 1
00237              AND
00238          W-RECORDS-PRINTED EQUAL ZEROS
00239          SET W-TB-NDX            TO PI-689-NUMBER-LABEL-LINES
00240          SET W-TB-NDX UP BY +1
00241
00242          IF  W-REC-TEXT (W-TB-NDX) EQUAL SPACES
00243              MOVE W-TOP-FORM     TO WS-PASSED-CNTL-CHAR
00244              PERFORM 1020-RECORD-PRINT THRU 1029-EXIT
00245                      VARYING
00246                  W-TB-NDX FROM W-TB-NDX BY +1
00247                      UNTIL
00248                  W-TB-NDX GREATER THAN W-ENTRIES-PER-RECORD
00249
00250          ELSE
00251              IF  W-REC-TEXT-TOP (W-TB-NDX) EQUAL
00252                      W-TOP-OF-FORM-MESSAGE
00253                  SET W-TB-NDX UP BY +1
00254                  MOVE W-TOP-FORM TO WS-PASSED-CNTL-CHAR
00255
00256                  IF  W-REC-TEXT (W-TB-NDX) EQUAL SPACES
00257                      PERFORM 1020-RECORD-PRINT THRU 1029-EXIT
00258                              VARYING
00259                          W-TB-NDX FROM W-TB-NDX BY +1
00260                              UNTIL
00261                          W-TB-NDX GREATER THAN
00262                              W-ENTRIES-PER-RECORD
00263
00264                  ELSE
00265                      MOVE SPACES TO WS-PASSED-DATA
00266                      MOVE -1     TO W-RECORDS-PRINTED
00267                      PERFORM 1028-PRINT THRU 1028-EXIT
00268                      PERFORM 1020-RECORD-PRINT THRU 1029-EXIT
00269                              VARYING
00270                          W-TB-NDX FROM W-TB-NDX BY +1
00271                              UNTIL
00272                          W-TB-NDX GREATER THAN
00273                              W-ENTRIES-PER-RECORD
00274
00275              ELSE
00276                  MOVE W-TOP-FORM TO WS-PASSED-CNTL-CHAR
00277                  MOVE SPACES     TO WS-PASSED-DATA
00278                  MOVE -1         TO W-RECORDS-PRINTED
00279                  PERFORM 1028-PRINT THRU 1028-EXIT
00280                  PERFORM 1020-RECORD-PRINT THRU 1029-EXIT
00281                          VARYING
00282                      W-TB-NDX FROM W-TB-NDX BY +1
00283                          UNTIL
00284                      W-TB-NDX GREATER THAN
00285                          W-ENTRIES-PER-RECORD
00286
00287      ELSE
00288          PERFORM 1020-RECORD-PRINT THRU 1029-EXIT
00289                  VARYING
00290              W-TB-NDX FROM 1 BY 1
00291                  UNTIL
00292              W-TB-NDX GREATER THAN W-ENTRIES-PER-RECORD.
00293
00294  1019-EXIT.
00295       EXIT.
00296                                   EJECT
00297  1020-RECORD-PRINT.
00298
00299      IF  W-REC-TEXT-TOP (W-TB-NDX) = W-TOP-OF-FORM-MESSAGE
00300          MOVE W-TOP-FORM         TO WS-PASSED-CNTL-CHAR
00301          MOVE SPACES             TO WS-PASSED-DATA
00302          MOVE -1                 TO W-RECORDS-PRINTED
00303          GO TO 1028-PRINT.
00304
00305      IF  W-TB-NDX GREATER THAN 50
00306          MOVE 'INPUT RECORDS GREATER THAN 50'
00307                                  TO W-ERROR-LINE
00308          PERFORM 0400-SEND-TEXT
00309          SET W-TB-NDX DOWN BY 1
00310          MOVE W-REC-TEXT (W-TB-NDX)
00311                                  TO W-ERROR-LINE
00312          PERFORM 0400-SEND-TEXT
00313          GO TO 0200-END-DATA.
031011     IF W-REC-TEXT (W-TB-NDX) (1:6) = '&&&&&&'
031011        GO TO 1029-EXIT
031011     END-IF
00315      MOVE SPACES                 TO W-ADJUST-AREA.
00316
00317      MOVE W-REC-TEXT (W-TB-NDX)  TO W-AD-PRINT-AREA.
00318      MOVE W-ADJUST-AREA          TO WS-PASSED-DATA.
00319
00320  1028-PRINT.
00321
00322      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00323      MOVE SPACE                  TO WS-PASSED-CNTL-CHAR.
00324      ADD 1                       TO W-RECORDS-PRINTED.
00325
00326  1028-EXIT.
00327       EXIT.
00328
00329  1028-CONTINUE.
00330
00331      IF  W-RECORDS-PRINTED = PI-TOTAL-LINES
00332          SET W-TB-NDX            TO W-ENTRIES-PER-RECORD
00333          SET W-TB-NDX UP BY 1
00334          GO TO 1029-EXIT.
00335
00336  1029-EXIT.
00337       EXIT.
00338                                   EJECT
00339  1089-TS-ITEMERR.
00340
00341      MOVE 'TEMP STORAGE RECORD NOT FOUND'
00342                                   TO W-ERROR-LINE.
00343      PERFORM 0400-SEND-TEXT.
00344      GO TO 0100-RETRIEVE-LOOP.
00345
00346  1090-TS-QIDERR.
00347
00348      MOVE 'NO TEMP STORAGE RECORDS NOT FOUND'
00349                                   TO W-ERROR-LINE.
00350      PERFORM 0400-SEND-TEXT.
00351      GO TO 0100-RETRIEVE-LOOP.
00352                                   EJECT
00353  7750-DELETE-TEMP-STOR.
00354
00355      
      * EXEC CICS HANDLE CONDITION
00356 *         QIDERR (7750-EXIT)
00357 *    END-EXEC.
      *    MOVE '"$N                   ! $ #00001047' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303031303437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00358
00359      
      * EXEC CICS DELETEQ TS
00360 *         QUEUE  (PI-689-TEMP-STOR-ID)
00361 *    END-EXEC.
      *    MOVE '*&                    #   #00001051' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031303531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-689-TEMP-STOR-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00362
00363  7750-EXIT.
00364      EXIT.
00365                                   EJECT
00366 *    COPY ELPRTCVP.
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
      *    MOVE '."C                   ''   #00001114' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031313134' TO DFHEIV0(25:11)
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
      *    MOVE '$$    C E         L F ,   #00001235' TO DFHEIV0
           MOVE X'242420202020432045202020' TO DFHEIV0(1:12)
           MOVE X'2020202020204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323335' TO DFHEIV0(25:11)
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
00367
00368
00369  9999-GOBACK.
00370
00371      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6892' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00372
00373  9999-EXIT.
00374      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6892' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 0200-END-DATA,
                     0300-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 1090-TS-QIDERR,
                     1089-TS-ITEMERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 7750-EXIT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6892' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
