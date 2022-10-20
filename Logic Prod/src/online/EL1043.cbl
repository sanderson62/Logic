00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL1043.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 09:40:03.
00007 *                            VMOD=2.003
00008 *
00008 *
00009 *AUTHOR.     LOGIC,INC.
00010 *            DALLAS, TEXAS.
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
00024 *REMARKS.    TRANSACTION - EXM7 - CREDIT VARIABLE CONVERTER
00025
00026  ENVIRONMENT DIVISION.
00027
00028  DATA DIVISION.
00029  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00030  77  FILLER  PIC X(32)  VALUE '********************************'.
00031  77  FILLER  PIC X(32)  VALUE '*    EL1043 WORKING STORAGE    *'.
00032  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.003 *********'.
00033                                  EJECT
00034
00035  01  W-WORK-AREAS.
00036      12  FILLER                  PIC  X(18)
00037                                       VALUE 'PROGRAM WORK AREA:'.
00038      12  W-EXTRA-LENGTH          PIC S9(04)  COMP   VALUE +0.
00039      12  W-TOTAL-TX-LINES        PIC S9(03)  COMP-3 VALUE +0.
00040      12  W-LAST-SKIP-LINES       PIC  9(02)         VALUE  0.
00041      12  W-SKIP-LINES            PIC  9(02)         VALUE  0.
00042
00043      12  W-CALL-PGM              PIC  X(08).
00044      12  W-SAVE-BIN-DATE         PIC  X(02)  VALUE SPACES.
00045      12  W-SAVE-DATE             PIC  X(08)  VALUE SPACES.
00046      12  W-SAVE-PLAN             PIC  X(02)  VALUE SPACES.
00047      12  W-TEXT-RECORD.
00048          16  FILLER              PIC  X(02).
00049          16  W-TR-KEY.
00050              20  FILLER          PIC  X(01).
00051              20  W-TR-LETTER-NO  PIC  X(04).
00052              20  FILLER          PIC  X(08).
00053              20  W-TR-SEQ-NO     PIC S9(04) COMP.
00054          16  FILLER              PIC  X(02).
00055          16  W-TR-TEXT-LINE      PIC  X(70).
00056          16  FILLER              PIC  X(11).
00057
00058                                  EJECT
00059  01  W-SWITCH-INDICATORS-AREA.
00060      12  FILLER                  PIC  X(16)
00061                                       VALUE 'PROGRAM SWITCHS:'.
00062      12  W-TEXT-BROWSED-SW       PIC  X(01)  VALUE 'N'.
00063          88  W-TEXT-BROWSE-STARTED           VALUE 'Y'.
00064          88  W-TEXT-BROWSE-NOT-STARTED       VALUE 'N'.
00065                                  EJECT
00066  01  W-KEY-AREAS.
00067      12  FILLER                  PIC  X(13)
00068                                       VALUE 'PROGRAM KEYS:'.
00069
00070      12  W-TEXT-SAVE-KEY         PIC  X(05).
00071      12  W-TEXT-KEY.
00072          16  W-TEXT-PARTIAL-KEY.
00073              20  W-TEXT-COMPANY-CD
00074                                  PIC  X(01).
00075              20  W-TEXT-LETTER   PIC  X(04).
00076          16  W-TEXT-FILLER       PIC  X(08)   VALUE SPACES.
00077          16  W-TEXT-SEQ          PIC S9(04)   VALUE +0    COMP.
00078
00079                                  EJECT
00080  01  FILLER                      PIC  X(22)
00081                                  VALUE 'INTERFACE AREA STARTS:'.
00082 *    COPY ELCINTF.
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
00083      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.
00084 *    COPY ELC1042.
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
00085          16  FILLER              PIC X(444).
00086          16  PI-OLD-NAME         PIC  X(04).
00087          16  PI-NEW-NAME         PIC  X(04).
00088          16  PI-1043-ERROR       PIC  9(04).
00089          16  FILLER              PIC X(117).
00090
00091  01  FILLER                      PIC  X(20)
00092                                  VALUE ':INTERFACE AREA ENDS'.
00093                                  EJECT
00094  01  W-CONSTANT-AREA.
00095      12  FILLER                  PIC  X(18)
00096                                  VALUE 'PROGRAM CONSTANTS:'.
00097      12  W-ZEROS                 PIC S9(04)  VALUE ZEROS.
00098
00099      12  W-LGXX-ID               PIC  X(04)  VALUE 'LGXX'.
00100      12  W-LINK-001              PIC  X(05)  VALUE 'EL001'.
00101      12  W-LINK-004              PIC  X(05)  VALUE 'EL004'.
00102      12  W-LINK-ELDATCV          PIC  X(07)  VALUE 'ELDATCV'.
00103      12  W-PGM-EL626             PIC  X(08)  VALUE 'EL626'.
00104      12  W-TEXT-FILE-ID          PIC  X(08)  VALUE 'ELLETR'.
00105      12  W-THIS-PGM              PIC  X(08)  VALUE 'EL1043'.
00106      12  W-TOP-FORM              PIC  X(70)
00107                               VALUE '*****TOP OF FORM *****'.
00108      12  W-TRANSACTION           PIC  X(04)  VALUE 'EXM7'.
00109      12  W-XCTL-005              PIC  X(05)  VALUE 'EL005'.
00110      12  W-XCTL-010              PIC  X(05)  VALUE 'EL010'.
00111      12  W-XCTL-626              PIC  X(05)  VALUE 'EL626'.
00112                                  EJECT
00113  01  W-VARIABLE-PROCESS-CNTLS.
00114      12  FILLER                  PIC  X(26)
00115                         VALUE 'VARIABLE WORK AREA STARTS:'.
00116      12  W-VAR-HOLD.
00117          16  W-V1                PIC  X(01).
00118          16  W-V2                PIC  X(01).
00119          16  W-V3                PIC  X(01).
00120          16  W-V4                PIC  X(01).
00121
00122  01  W-SUPPORTED-VARIABLES.
00123 *****************COMPANY VARIABLES - ELCNTL ********************
00124 *****COMPANY NAME
00125      12  FILLER                  PIC  X(04) VALUE    '01.0'.
00126      12  FILLER                  PIC  X(06) VALUE  '@@001#'.
00127 *****FULL COMPANY ADDRESS
00128      12  FILLER                  PIC  X(04) VALUE    '02.1'.
00129      12  FILLER                  PIC  X(06) VALUE  '@@002#'.
00130      12  FILLER                  PIC  X(04) VALUE    '02.2'.
00131      12  FILLER                  PIC  X(06) VALUE  '@@003#'.
00132      12  FILLER                  PIC  X(04) VALUE    '02.3'.
00133      12  FILLER                  PIC  X(06) VALUE  '@@004#'.
00134      12  FILLER                  PIC  X(04) VALUE    '02.4'.
00135      12  FILLER                  PIC  X(06) VALUE  '@@005#'.
00136      12  FILLER                  PIC  X(04) VALUE    '02.5'.
00137      12  FILLER                  PIC  X(06) VALUE  '@@006#'.
00138 ************ LIFE BENEFIT VARIABLES - ELCNTL *******************
00139 *****LIFE BENEFIT DESCRIPTION
00140      12  FILLER                  PIC  X(04) VALUE    '12.0'.
00141      12  FILLER                  PIC  X(06) VALUE  '@@011#'.
00142 ************* A&H BENEFIT VARIABLES - ELCNTL *******************
00143 *****AH BENEFIT DESCRIPTION
00144      12  FILLER                  PIC  X(04) VALUE    '12.5'.
00145      12  FILLER                  PIC  X(06) VALUE  '@@015#'.
00146 *****ELIMINATION PERIOD
00147      12  FILLER                  PIC  X(04) VALUE    '27.0'.
00148      12  FILLER                  PIC  X(06) VALUE  '@@016#'.
00149 *****************CARRIER VARIABLES - ELCNTL ********************
00150 *****CARRIER NAME
00151      12  FILLER                  PIC  X(04) VALUE    '03.0'.
00152      12  FILLER                  PIC  X(06) VALUE  '@@020#'.
00153 *****FULL CARRIER ADDRESS
00154      12  FILLER                  PIC  X(04) VALUE    '04.1'.
00155      12  FILLER                  PIC  X(06) VALUE  '@@021#'.
00156      12  FILLER                  PIC  X(04) VALUE    '04.2'.
00157      12  FILLER                  PIC  X(06) VALUE  '@@022#'.
00158      12  FILLER                  PIC  X(04) VALUE    '04.3'.
00159      12  FILLER                  PIC  X(06) VALUE  '@@023#'.
00160      12  FILLER                  PIC  X(04) VALUE    '04.4'.
00161      12  FILLER                  PIC  X(06) VALUE  '@@024#'.
00162      12  FILLER                  PIC  X(04) VALUE    '04.5'.
00163      12  FILLER                  PIC  X(06) VALUE  '@@025#'.
00164 *****CARRIER PHONE NUMBER
00165      12  FILLER                  PIC  X(04) VALUE    '04.6'.
00166      12  FILLER                  PIC  X(06) VALUE  '@@026#'.
00167 ***************** MAIL VARIABLES - ELMAIL **********************
00168 *****FULL MAIL ADDRESS
00169      12  FILLER                  PIC  X(04) VALUE    '05.1'.
00170      12  FILLER                  PIC  X(06) VALUE  '@@031#'.
00171      12  FILLER                  PIC  X(04) VALUE    '05.2'.
00172      12  FILLER                  PIC  X(06) VALUE  '@@032#'.
00173      12  FILLER                  PIC  X(04) VALUE    '05.3'.
00174      12  FILLER                  PIC  X(06) VALUE  '@@033#'.
00175      12  FILLER                  PIC  X(04) VALUE    '05.4'.
00176      12  FILLER                  PIC  X(06) VALUE  '@@034#'.
00177      12  FILLER                  PIC  X(04) VALUE    '05.5'.
00178      12  FILLER                  PIC  X(06) VALUE  '@@035#'.
00179      12  FILLER                  PIC  X(04) VALUE    '05.6'.
00180      12  FILLER                  PIC  X(06) VALUE  '@@036#'.
00181 *************** ACCOUNT VARIABLES - ERACCT *********************
00182 *****ACCOUNT NAME
00183      12  FILLER                  PIC  X(04) VALUE    '06.0'.
00184      12  FILLER                  PIC  X(06) VALUE  '@@040#'.
00185 *****FULL ACCOUNT ADDRESS
00186      12  FILLER                  PIC  X(04) VALUE    '07.1'.
00187      12  FILLER                  PIC  X(06) VALUE  '@@041#'.
00188      12  FILLER                  PIC  X(04) VALUE    '07.2'.
00189      12  FILLER                  PIC  X(06) VALUE  '@@042#'.
00190      12  FILLER                  PIC  X(04) VALUE    '07.3'.
00191      12  FILLER                  PIC  X(06) VALUE  '@@043#'.
00192      12  FILLER                  PIC  X(04) VALUE    '07.4'.
00193      12  FILLER                  PIC  X(06) VALUE  '@@044#'.
00194      12  FILLER                  PIC  X(04) VALUE    '07.5'.
00195      12  FILLER                  PIC  X(06) VALUE  '@@045#'.
00196 *****ACCOUNT PHONE NUMBER
00197      12  FILLER                  PIC  X(04) VALUE    '07.6'.
00198      12  FILLER                  PIC  X(06) VALUE  '@@046#'.
00199 *************** NON FILE VARIABLES *****************************
00200 *****CURRENT DATE
00201      12  FILLER                  PIC  X(04) VALUE    '10.0'.
00202      12  FILLER                  PIC  X(06) VALUE  '@@060#'.
00203 *****FULL CURRENT DATE
00204      12  FILLER                  PIC  X(04) VALUE    '11.0'.
00205      12  FILLER                  PIC  X(06) VALUE  '@@061#'.
00206 ************** CERTIFICATE VARIABLES - ELCERT *****************
00207 *****CARRIER CODE IN CERT
00208      12  FILLER                  PIC  X(04) VALUE    '13.0'.
00209      12  FILLER                  PIC  X(06) VALUE  '@@070#'.
00210 *****GROUPING CODE IN CERT
00211      12  FILLER                  PIC  X(04) VALUE    '14.0'.
00212      12  FILLER                  PIC  X(06) VALUE  '@@071#'.
00213 *****ACCOUNT NUMBER IN CERT
00214      12  FILLER                  PIC  X(04) VALUE    '15.0'.
00215      12  FILLER                  PIC  X(06) VALUE  '@@072#'.
00216 *****CERTIFICATE NUMBER
00217      12  FILLER                  PIC  X(04) VALUE    '16.0'.
00218      12  FILLER                  PIC  X(06) VALUE  '@@073#'.
00219 *****CERT EFFECTIVE DATE
00220      12  FILLER7                 PIC  X(04) VALUE    '17.0'.
00221      12  FILLER                  PIC  X(06) VALUE  '@@074#'.
00222 *****CERT EXPIRATION DATE (LIFE)
00223      12  FILLER                  PIC  X(04) VALUE    '18.0'.
00224      12  FILLER                  PIC  X(06) VALUE  '@@075#'.
00225 *****CERT EXPIRATION DATE (AH)
00226      12  FILLER                  PIC  X(04) VALUE    '18.5'.
00227      12  FILLER                  PIC  X(06) VALUE  '@@076#'.
00228 *****LIFE TERM
00229      12  FILLER                  PIC  X(04) VALUE    '19.0'.
00230      12  FILLER                  PIC  X(06) VALUE  '@@077#'.
00231 *****AH  TERM
00232      12  FILLER                  PIC  X(04) VALUE    '19.5'.
00233      12  FILLER                  PIC  X(06) VALUE  '@@078#'.
00234 *****LIFE COVERAGE AMOUNT
00235      12  FILLER                  PIC  X(04) VALUE    '20.0'.
00236      12  FILLER                  PIC  X(06) VALUE  '@@079#'.
00237 *****AH MONTHLY BENEFIT
00238      12  FILLER                  PIC  X(04) VALUE    '20.5'.
00239      12  FILLER                  PIC  X(06) VALUE  '@@080#'.
00240 *****LIFE CANCEL DATE
00241      12  FILLER                  PIC  X(04) VALUE    '21.0'.
00242      12  FILLER                  PIC  X(06) VALUE  '@@081#'.
00243 *****AH CANCEL DATE
00244      12  FILLER                  PIC  X(04) VALUE    '21.5'.
00245      12  FILLER                  PIC  X(06) VALUE  '@@082#'.
00246 *****LIFE COVERAGE FORM NUMBER
00247      12  FILLER                  PIC  X(04) VALUE    '22.0'.
00248      12  FILLER                  PIC  X(06) VALUE  '@@083#'.
00249 *****AH COVERAGE FORM NUMBER
00250      12  FILLER                  PIC  X(04) VALUE    '22.5'.
00251      12  FILLER                  PIC  X(06) VALUE  '@@083#'.
00252 *****INSUREDS AGE AT POLICY ISSUE (NOT USED)
00253      12  FILLER                  PIC  X(04) VALUE    '23.0'.
00254      12  FILLER                  PIC  X(06) VALUE  '@@085#'.
00255 *****LOAN NUMBER
00256      12  FILLER                  PIC  X(04) VALUE    '24.0'.
00257      12  FILLER                  PIC  X(06) VALUE  '@@086#'.
00258 *****LOAN BALANCE
00259      12  FILLER                  PIC  X(04) VALUE    '25.0'.
00260      12  FILLER                  PIC  X(06) VALUE  '@@087#'.
00261 *****MEMBER NUMBER
00262      12  FILLER                  PIC  X(04) VALUE    '26.0'.
00263      12  FILLER                  PIC  X(06) VALUE  '@@088#'.
00264 *****INSURED SOC SEC NUMBER
00265      12  FILLER                  PIC  X(04) VALUE    '29.0'.
00266      12  FILLER                  PIC  X(06) VALUE  '@@089#'.
00267 *****INSURED INITIALS & LAST NAME (CERTIFICATE)
00268      12  FILLER                  PIC  X(04) VALUE    '30.0'.
00269      12  FILLER                  PIC  X(06) VALUE  '@@090#'.
00270 *****INSURED FIRST NAME (CERTIFICATE)
00271      12  FILLER                  PIC  X(04) VALUE    '30.1'.
00272      12  FILLER                  PIC  X(06) VALUE  '@@091#'.
00273 *****INSURED MIDDLE INITIAL (CERTIFICATE)
00274      12  FILLER                  PIC  X(04) VALUE    '30.2'.
00275      12  FILLER                  PIC  X(06) VALUE  '@@092#'.
00276 *****ORIG TERM * MON BEN
00277      12  FILLER                  PIC  X(04) VALUE    '50.0'.
00278      12  FILLER                  PIC  X(06) VALUE  '@@093#'.
00279 *****INSURED'S NAME (LAST, FIRST, INIT)
00280      12  FILLER                  PIC  X(04) VALUE    '51.0'.
00281      12  FILLER                  PIC  X(06) VALUE  '@@094#'.
00282 *****INSURED'S NAME (FIRST, INIT, LAST)
00283      12  FILLER                  PIC  X(04) VALUE    '52.0'.
00284      12  FILLER                  PIC  X(06) VALUE  '@@095#'.
00285 *****TITLE (MR/MS)
00286      12  FILLER                  PIC  X(04) VALUE    '53.0'.
00287      12  FILLER                  PIC  X(06) VALUE  '@@096#'.
00288 *****LIFE PREMIUM (CERTIFICATE)
00289      12  FILLER                  PIC  X(04) VALUE    '55.0'.
00290      12  FILLER                  PIC  X(06) VALUE  '@@097#'.
00291 *****A/H PREMIUM (CERTIFICATE)
00292      12  FILLER                  PIC  X(04) VALUE    '56.0'.
00293      12  FILLER                  PIC  X(06) VALUE  '@@098#'.
00294 ************** PENDING VARIABLES - ERPNDB *********************
00295 *****INSURED DATE OF BIRTH
00296      12  SS28                    PIC  X(04) VALUE    '28.0'.
00297      12  FILLER                  PIC  X(06) VALUE  '@@110#'.
00298 *****ENTERED LIFE PREMIUM (PENDING)
00299      12  FILLER                  PIC  X(04) VALUE    '31.0'.
00300      12  FILLER                  PIC  X(06) VALUE  '@@111#'.
00301 *****ENTERED A/H PREMIUM (PENDING)
00302      12  FILLER                  PIC  X(04) VALUE    '32.0'.
00303      12  FILLER                  PIC  X(06) VALUE  '@@112#'.
00304 *****CALCULATED LIFE PREMIUM (PENDING)
00305      12  FILLER                  PIC  X(04) VALUE    '33.0'.
00306      12  FILLER                  PIC  X(06) VALUE  '@@113#'.
00307 *****CALCULATED A/H PREMIUM (PENDING)
00308      12  FILLER                  PIC  X(04) VALUE    '34.0'.
00309      12  FILLER                  PIC  X(06) VALUE  '@@114#'.
00310 *****DIFFERENCE ENTER/COMPUTED LIFE PREMIUM (PENDING)
00311      12  FILLER                  PIC  X(04) VALUE    '35.0'.
00312      12  FILLER                  PIC  X(06) VALUE  '@@115#'.
00313 *****DIFFERENCE ENTER/COMPUTED A/H PREMIUM (PENDING)
00314      12  FILLER                  PIC  X(04) VALUE    '36.0'.
00315      12  FILLER                  PIC  X(06) VALUE  '@@116#'.
00316 *****PRIOR CANCEL DATE
00317      12  FILLER                  PIC  X(04) VALUE    '37.0'.
00318      12  FILLER                  PIC  X(06) VALUE  '@@117#'.
00319 *****ENTERED LIFE REFUND (PENDING)
00320      12  FILLER                  PIC  X(04) VALUE    '38.0'.
00321      12  FILLER                  PIC  X(06) VALUE  '@@118#'.
00322 *****ENTERED A/H REFUND (PENDING)
00323      12  FILLER                  PIC  X(04) VALUE    '39.0'.
00324      12  FILLER                  PIC  X(06) VALUE  '@@119#'.
00325 *****CALCULATED LIFE REFUND (PENDING)
00326      12  FILLER                  PIC  X(04) VALUE    '40.0'.
00327      12  FILLER                  PIC  X(06) VALUE  '@@120#'.
00328 *****CALCULATED A/H REFUND (PENDING)
00329      12  FILLER                  PIC  X(04) VALUE    '41.0'.
00330      12  FILLER                  PIC  X(06) VALUE  '@@121#'.
00331 *****DIFFERENCE ENTER/COMPUTED LIFE REFUND (PENDING)
00332      12  FILLER                  PIC  X(04) VALUE    '42.0'.
00333      12  FILLER                  PIC  X(06) VALUE  '@@122#'.
00334 *****DIFFERENCE ENTER/COMPUTED A/H REFUND (PENDING)
00335      12  FILLER                  PIC  X(04) VALUE    '43.0'.
00336      12  FILLER                  PIC  X(06) VALUE  '@@123#'.
00337 *****INSUREDS AGE
00338      12  FILLER                  PIC  X(04) VALUE    '44.0'.
00339      12  FILLER                  PIC  X(06) VALUE  '@@124#'.
00340 *****LIFE BENEFIT (PENDING)
00341      12  FILLER                  PIC  X(04) VALUE    '45.0'.
00342      12  FILLER                  PIC  X(06) VALUE  '@@125#'.
00343 *****A/H BENEFIT (PENDING)
00344      12  FILLER                  PIC  X(04) VALUE    '46.0'.
00345      12  FILLER                  PIC  X(06) VALUE  '@@126#'.
00346 *****LIFE RATE
00347      12  FILLER                  PIC  X(04) VALUE    '47.0'.
00348      12  FILLER                  PIC  X(06) VALUE  '@@127#'.
00349 *****A/H RATE
00350      12  FILLER                  PIC  X(04) VALUE    '48.0'.
00351      12  FILLER                  PIC  X(06) VALUE  '@@128#'.
00352 *****TERM (PENDING)
00353      12  FILLER                  PIC  X(04) VALUE    '49.0'.
00354      12  FILLER                  PIC  X(06) VALUE  '@@129#'.
00355 *****TITLE BATCH NUMBER
00356      12  FILLER                  PIC  X(04) VALUE    '54.0'.
00357      12  FILLER                  PIC  X(06) VALUE  '@@130#'.
00358 ************** COMPENSATION VARIABLES - ERCOMP ****************
00359 *****COMPENSATION ACCT NAME
00360      12  FILLER                  PIC  X(04) VALUE    '60.0'.
00361      12  FILLER                  PIC  X(06) VALUE  '@@140#'.
00362 *****FULL COMPENSATION ADDRESS
00363      12  FILLER                  PIC  X(04) VALUE    '60.1'.
00364      12  FILLER                  PIC  X(06) VALUE  '@@141#'.
00365      12  FILLER                  PIC  X(04) VALUE    '60.2'.
00366      12  FILLER                  PIC  X(06) VALUE  '@@142#'.
00367      12  FILLER                  PIC  X(04) VALUE    '60.3'.
00368      12  FILLER                  PIC  X(06) VALUE  '@@143#'.
00369      12  FILLER                  PIC  X(04) VALUE    '60.4'.
00370      12  FILLER                  PIC  X(06) VALUE  '@@144#'.
00371      12  FILLER                  PIC  X(04) VALUE    '60.5'.
00372      12  FILLER                  PIC  X(06) VALUE  '@@145#'.
00373 *****COMPENSATION PHONE NUMBER
00374      12  FILLER                  PIC  X(04) VALUE    '60.6'.
00375      12  FILLER                  PIC  X(06) VALUE  '@@147#'.
00376 ******************  PROCESSOR DATA - ELCNTL (2) ****************
00377 *****EXECUTING PROCESSOR NAME
00378      12  FILLER                  PIC  X(04) VALUE    '08.0'.
00379      12  FILLER                  PIC  X(06) VALUE  '@@151#'.
00380 *****PROCESSOR TITLE
00381      12  FILLER                  PIC  X(04) VALUE    '09.0'.
00382      12  FILLER                  PIC  X(06) VALUE  '@@152#'.
00383  01  FILLER REDEFINES W-SUPPORTED-VARIABLES.
00384      12  W-VAR-HOLD-GRP OCCURS 90 TIMES
00385                         INDEXED BY W-VG-NDX.
00386          16  W-VAR-OLD           PIC  X(04).
00387          16  W-VAR-NEW.
00388              20  W-VC-CHAR OCCURS 6 TIMES
00389                            INDEXED BY W-VC-NDX
00390                                  PIC  X(01).
00391
00392  01  W-VAR-END                   PIC  X(23)
00393                         VALUE ':VARIABLE WORK AREA END'.
00394
00395  01  W-PROGRAM-TABLE-AREA.
00396      12  FILLER                  PIC  X(15)
00397                                  VALUE 'PROGRAM TABLES:'.
00398
00399      12  W-TX-TABLE.
00400          16  W-TX-GRP OCCURS 300 TIMES
00401                        INDEXED BY W-TG-NDX.
00402              20  FILLER              PIC  X(02).
00403              20  W-TX-KEY.
00404                  24  FILLER          PIC  X(01).
00405                  24  W-TX-LETTER-NO  PIC  X(04).
00406                  24  FILLER          PIC  X(08).
00407                  24  W-TX-SEQ-NO     PIC S9(04) COMP.
00408              20  FILLER              PIC  X(02).
00409
00410              20  W-TEXT-LINE.
00411                  24  W-TX-CHAR OCCURS 70 TIMES
00412                                INDEXED BY W-TX-NDX
00413                                      PIC  X(01).
00414
00415          16  FILLER                  PIC  X(11).
00416
00417      12  W-WORK-LINE.
00418          16  W-WK-CHAR OCCURS 70 TIMES
00419                        INDEXED BY W-WK-NDX
00420                                   W-WK-NDX1
00421                                      PIC  X(01).
00422
00423      12  FILLER                  PIC  X(14)
00424                                  VALUE 'END OF TABLES:'.
00425                                  EJECT
00426  01  ERROR-MESSAGES.
00427      12  ER-0000                 PIC  X(04) VALUE '0000'.
00428      12  ER-0004                 PIC  X(04) VALUE '0004'.
00429      12  ER-0006                 PIC  X(04) VALUE '0006'.
00430      12  ER-0013                 PIC  X(04) VALUE '0013'.
00431      12  ER-0180                 PIC  X(04) VALUE '0180'.
00432      12  ER-7391                 PIC  X(04) VALUE '7391'.
00433                                  EJECT
00434 *****DFHAID                  COPY ELCAID.
00435 *    COPY ELCAID.
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
00436  01  FILLER    REDEFINES DFHAID.
00437      12  FILLER                  PIC  X(08).
00438      12  PF-VALUES               PIC  X(01) OCCURS 2.
00439                                  EJECT
00440 *    COPY ELCATTR.
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
00441                                  EJECT
00442 *    COPY ELCNWA.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELCNWA.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                         *
00006 *                                                               *
00007 *            M O V E   N A M E   W O R K   A R E A.             *
00008 *                                                               *
00009 *****************************************************************.
00010
00011  01  WS-NAME-WORK-AREA.
00012      05  WS-INSURED-LAST-NAME        PIC X(15).
00013      05  WS-INSURED-1ST-NAME         PIC X(12).
00014      05  WS-INSURED-MID-INIT         PIC X.
00015
00016      05  WS-NAME-WORK.
00017          10  WS-NW                   PIC X
00018              OCCURS 30 TIMES INDEXED BY NWA-INDEX.
00019
00020      05  WS-NAME-WORK2.
00021          10  WS-NW2                  PIC X
00022              OCCURS 20 TIMES INDEXED BY NWA-INDEX2 NWA-INDEX3
00023                                         NWA-INDEX0.
00024
00025      05  WS-NAME-SW                  PIC S9          VALUE ZERO
00026                                      COMP-3.
00027
00443                                  EJECT
00444 *    COPY ELCEMIB.
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
00445  01  EMI-SAVE-AREA               PIC  X(400).
00446                                  EJECT
00447 *    COPY ELCDATE.
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
00448                                  EJECT
00449 *    COPY ELCLOGOF.
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
00450                                  EJECT
00451 *    COPY ELCSCTM.
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
00452                                  EJECT
00453 *    COPY ELCSCRTY.
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
00454                                  EJECT
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
00456  01  DFHCOMMAREA                 PIC  X(1024).
00457
00458 *01 PARMLIST .
00459 *    02  FILLER                  PIC S9(08)  COMP.
00460
00461 *    02  L-TEXT-POINTER          PIC S9(08)  COMP.
00462
00463 *    COPY ELCTEXT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCTEXT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.008                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = TEXT FILES FOR HELP DISPLAY,              *
00008 *                                     FORM LETTERS,              *
00009 *                                     CERT FORM DISPLAY.
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 100   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ELLETR (LETTERS)   RKP=2,LEN=15          *
00015 *       ALTERNATE INDEX = NONE                                   *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ELFORM (FORMS)     RKP=2,LEN=15          *
00018 *       ALTERNATE INDEX = NONE                                   *
00019 *                                                                *
00020 *   BASE CLUSTER NAME = ELHELP (HELP)      RKP=2,LEN=15          *
00021 *       ALTERNATE INDEX = NONE                                   *
00022 *                                                                *
00023 *   LOG = NO                                                     *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00025 ******************************************************************
00026  01  TEXT-FILES.
00027      12  TEXT-FILE-ID                PIC XX.
00028          88  FORMS-FILE-TEXT            VALUE 'TF'.
00029          88  LETTER-FILE-TEXT           VALUE 'TL'.
00030          88  HELP-FILE-TEXT             VALUE 'TH'.
00031
00032      12  TX-CONTROL-PRIMARY.
00033          16  TX-COMPANY-CD           PIC X.
00034              88  TX-SYSTEM-WIDE-FILE    VALUE LOW-VALUE.
00035          16  TX-ACCESS-CD-GENL       PIC X(12).
00036
00037          16  TX-LETTER-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00038              20  TX-LETTER-NO        PIC X(4).
00039              20  FILLER              PIC X(8).
00040
00041          16  TX-FORM-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00042              20  TX-FORM-NO          PIC X(12).
00043
00044          16  TX-HELP-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00045              20  TX-HELP-TYPE        PIC X.
00046                  88  HELP-FOR-GENERAL   VALUE ' '.
00047                  88  HELP-BY-SCREEN     VALUE 'S'.
00048                  88  HELP-BY-ERROR      VALUE 'E'.
00049              20  TX-SCREEN-OR-ERROR  PIC X(4).
00050                  88  GENERAL-INFO-HELP  VALUE '0000'.
00051              20  TX-HELP-FOR-COMPANY  PIC XXX.
00052                  88  NOT-COMPANY-SPECIFIC VALUE '   '.
00053              20  FILLER              PIC X(4).
00054
00055          16  TX-LINE-SEQUENCE        PIC S9(4)     COMP.
00056
00057      12  TX-PROCESS-CONTROL          PIC XX.
00058          88  LETTER-LINE-SKIPS          VALUE '01' THRU '99'.
00059
00060      12  TX-TEXT-LINE                PIC X(70).
00061
00062      12  TX-FORM-SQUEEZE-CONTROL     PIC X.
00063          88  TX-FORM-SQUEEZE-ON         VALUE 'Y'.
00064          88  TX-FORM-SQUEEZE-OFF        VALUE SPACES.
00065          88  TX-VALID-FORM-SQUEEZE-VALUE
00066                                         VALUE 'Y' ' '.
00067
00068      12  TX-LINE-SQUEEZE-CONTROL     PIC X.
00069          88  TX-ADJUST-TO-LINE-LENGTH   VALUE 'A'.
00070          88  TX-CONTINUE-PARAGRAPH      VALUE 'C'.
00071          88  TX-DO-NOT-ADJUST           VALUE 'N'.
00072          88  TX-FORM-CONTROL-LINE       VALUE 'K'.
00073          88  TX-NEW-PARAGRAPH           VALUE 'P'.
00074          88  TX-NO-SPECIAL-INSTRUCTION  VALUE ' '.
00075          88  TX-VALID-LINE-SQ-VALUE     VALUE 'A' 'C' 'P'
00076                                               'K' 'N' ' '
00077                                               'Z'.
00078
00079      12  TX-ARCHIVE-SW               PIC X.
00080          88  TX-ARCHIVE-THIS-LETTER     VALUE 'Y'.
00081          88  TX-DO-NOT-ARCHIVE          VALUE SPACES.
00082          88  TX-VALID-ARCHIVE-VALUE     VALUE 'Y' ' '.
00083
00084      12  TX-LAST-MAINTENANCED-BY     PIC X(4).
00085      12  TX-LAST-MAINTENANCED-DT     PIC X(2).
00086
00087      12  TX-BSR-CODE                 PIC X.
00088          88  TX-BSR-LETTER              VALUE 'B'.
00089          88  TX-NON-BSR-LETTER          VALUE ' '.
00090
00091      12  FILLER                      PIC X.
00092 *****************************************************************
00464                                  EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA TEXT-FILES.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1043' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00466
00467      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00468
00469      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00470      MOVE '5'                    TO DC-OPTION-CODE.
00471      PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
00472      MOVE DC-GREG-DATE-1-EDIT    TO W-SAVE-DATE.
00473      MOVE DC-BIN-DATE-1          TO W-SAVE-BIN-DATE.
00474
00475      
      * EXEC CICS HANDLE CONDITION
00476 *        PGMIDERR (9700-PGMID-ERROR)
00477 *        ERROR    (9800-ABEND)
00478 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00001450' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031343530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00479
00480      MOVE 0000                   TO PI-1043-ERROR.
00481
00482      PERFORM 1000-CREATE-WORK-TABLE THRU 1000-EXIT.
00483
00484      IF  PI-1043-ERROR EQUAL 0000
00485          PERFORM 3000-CREATE-NEW-TEXT THRU 3000-EXIT.
00486
00487      MOVE PROGRAM-INTERFACE-BLOCK
00488                                  TO DFHCOMMAREA.
00489      
      * EXEC CICS RETURN
00490 *    END-EXEC.
      *    MOVE '.(                    &   #00001464' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00491                                  EJECT
00492  1000-CREATE-WORK-TABLE.
00493
00494      MOVE SPACES                 TO W-TX-TABLE.
00495      SET W-TG-NDX                TO +1
00496
00497      MOVE PI-COMPANY-CD          TO W-TEXT-COMPANY-CD.
00498      MOVE PI-OLD-NAME            TO W-TEXT-LETTER.
00499      MOVE W-TEXT-PARTIAL-KEY     TO W-TEXT-SAVE-KEY.
00500
00501      
      * EXEC CICS HANDLE CONDITION
00502 *         NOTFND     (1001-NOT-FOUND)
00503 *         ENDFILE    (1001-NOT-FOUND)
00504 *         NOTOPEN    (8050-TEXT-NOT-OPEN)
00505 *    END-EXEC.
      *    MOVE '"$I''J                 ! # #00001476' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303031343736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00506
00507      
      * EXEC CICS STARTBR
00508 *         DATASET    (W-TEXT-FILE-ID)
00509 *         RIDFLD     (W-TEXT-KEY)
00510 *         GTEQ
00511 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00001482' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TEXT-FILE-ID, 
                 W-TEXT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00512
00513      
      * EXEC CICS HANDLE CONDITION
00514 *         ENDFILE    (1000-ENDBR)
00515 *    END-EXEC.
      *    MOVE '"$''                   ! $ #00001488' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303031343838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00516
00517  1000-READ-NEXT.
00518
00519      
      * EXEC CICS READNEXT
00520 *         DATASET    (W-TEXT-FILE-ID)
00521 *         SET        (ADDRESS OF TEXT-FILES)
00522 *         RIDFLD     (W-TEXT-KEY)
00523 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00001494' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TEXT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-TEXT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF TEXT-FILES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00524
00525      IF  W-TEXT-PARTIAL-KEY NOT EQUAL W-TEXT-SAVE-KEY
00526          GO TO 1000-ENDBR.
00527
00528      MOVE 'Y'                    TO W-TEXT-BROWSED-SW.
00529
00530      IF  TX-FORM-CONTROL-LINE
00531              OR
00532          TX-LINE-SQUEEZE-CONTROL EQUAL 'Z'
00533          MOVE TEXT-FILES         TO W-TX-GRP (W-TG-NDX)
00534          GO TO 1000-READ-NEXT.
00535
00536      MOVE TX-PROCESS-CONTROL     TO W-SKIP-LINES.
00537
00538      IF  TX-TEXT-LINE GREATER THAN SPACES
00539          MOVE W-LAST-SKIP-LINES  TO TX-PROCESS-CONTROL
00540          MOVE W-SKIP-LINES       TO W-LAST-SKIP-LINES
00541          MOVE TEXT-FILES         TO W-TX-GRP (W-TG-NDX)
00542          MOVE TX-TEXT-LINE       TO W-WORK-LINE
00543          SET W-TX-NDX            TO W-ZEROS
00544          PERFORM 2200-CHECK-FOR-VARIABLE THRU 2200-EXIT
00545                  VARYING
00546              W-WK-NDX FROM 1 BY 1
00547                  UNTIL
00548              W-WK-NDX GREATER THAN 70
00549
00550      ELSE
00551          IF  W-SKIP-LINES EQUAL 99
00552              MOVE W-SKIP-LINES   TO W-LAST-SKIP-LINES
00553              GO TO 1000-READ-NEXT
00554
00555          ELSE
00556              IF  W-SKIP-LINES GREATER THAN ZEROS
00557                  ADD W-SKIP-LINES
00558                                  TO W-LAST-SKIP-LINES
00559                  ADD +1          TO W-LAST-SKIP-LINES
00560                  GO TO 1000-READ-NEXT
00561
00562              ELSE
00563                  ADD +1          TO W-LAST-SKIP-LINES
00564                  GO TO 1000-READ-NEXT.
00565
00566      SET W-TG-NDX UP BY 1.
00567      GO TO 1000-READ-NEXT.
00568
00569  1000-ENDBR.
00570
00571      
      * EXEC CICS ENDBR
00572 *        DATASET     (W-TEXT-FILE-ID)
00573 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001546' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TEXT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00574
00575      IF  W-TEXT-BROWSE-NOT-STARTED
00576          MOVE ER-0006            TO EMI-ERROR
00577          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00578
00579      SET W-TOTAL-TX-LINES        TO W-TG-NDX.
00580      SUBTRACT +1 FROM W-TOTAL-TX-LINES.
00581
00582  1000-EXIT.
00583      EXIT.
00584
00585  1001-NOT-FOUND.
00586
00587      MOVE ER-0006                TO EMI-ERROR.
00588      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00589                                  EJECT
00590  2200-CHECK-FOR-VARIABLE.
00591
00592      SET W-TX-NDX UP BY +1.
00593
00594      IF  W-WK-CHAR (W-WK-NDX) EQUAL '@'
00595          SET W-WK-NDX1           TO W-WK-NDX
00596          SET W-WK-NDX1 UP BY +1
00597
00598          IF  W-WK-CHAR (W-WK-NDX1) EQUAL '@'
00599              MOVE W-WK-CHAR (W-WK-NDX)
00600                  TO W-TX-CHAR (W-TG-NDX W-TX-NDX)
00601              SET W-WK-NDX UP BY +1
00602              SET W-TX-NDX UP BY +1
00603
00604          ELSE
00605              PERFORM 2220-BUILD-VARIABLE-NUMBER THRU 2220-EXIT
00606              PERFORM 2240-CONVERT-VARIABLE THRU 2240-EXIT.
00607
00608      MOVE W-WK-CHAR (W-WK-NDX)   TO W-TX-CHAR (W-TG-NDX W-TX-NDX).
00609
00610  2200-EXIT.
00611      EXIT.
00612                                  EJECT
00613  2220-BUILD-VARIABLE-NUMBER.
00614
00615      SET W-WK-NDX UP BY +1.
00616      MOVE W-WK-CHAR (W-WK-NDX)   TO W-V1.
00617      SET W-WK-NDX UP BY +1.
00618      MOVE W-WK-CHAR (W-WK-NDX)   TO W-V2.
00619      SET W-WK-NDX UP BY +1.
00620
00621      IF  W-WK-CHAR (W-WK-NDX) EQUAL '.'
00622          MOVE W-WK-CHAR (W-WK-NDX)
00623                                  TO W-V3
00624          SET W-WK-NDX UP BY +1
00625          MOVE W-WK-CHAR (W-WK-NDX)
00626                                  TO W-V4
00627          SET W-WK-NDX UP BY +1
00628          MOVE +1                 TO W-EXTRA-LENGTH
00629
00630      ELSE
00631          MOVE +3                 TO W-EXTRA-LENGTH
00632          MOVE '.'                TO W-V3
00633          MOVE ZEROS              TO W-V4.
00634
00635  2220-EXIT.
00636      EXIT.
00637                                  EJECT
00638  2240-CONVERT-VARIABLE.
00639
00640      SET W-VG-NDX                TO +1.
00641      SEARCH W-VAR-HOLD-GRP
00642          AT END
00643              MOVE ER-0180        TO EMI-ERROR
00644              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00645
00646          WHEN
00647              W-VAR-OLD (W-VG-NDX) EQUAL W-VAR-HOLD
00648              PERFORM 2260-MOVE-NEW-VAR-HOLD THRU 2260-EXIT
00649                      VARYING
00650                  W-VC-NDX FROM 1 BY 1
00651                      UNTIL
00652                  W-VC-NDX EQUAL 7.
00653
00654      PERFORM 2270-ADJUST-EXTRA-LENGTH THRU 2270-EXIT
00655              VARYING
00656          W-WK-NDX FROM W-WK-NDX BY +1
00657              UNTIL
00658          W-EXTRA-LENGTH EQUAL ZERO.
00659
00660  2240-EXIT.
00661      EXIT.
00662                                  EJECT
00663  2260-MOVE-NEW-VAR-HOLD.
00664
00665      MOVE W-VC-CHAR (W-VG-NDX W-VC-NDX)
00666                                  TO W-TX-CHAR (W-TG-NDX W-TX-NDX).
00667      SET W-TX-NDX UP BY +1.
00668
00669  2260-EXIT.
00670      EXIT.
00671                                  EJECT
00672  2270-ADJUST-EXTRA-LENGTH.
00673
00674      IF  W-WK-CHAR (W-WK-NDX) EQUAL SPACES
00675          SUBTRACT +1 FROM W-EXTRA-LENGTH
00676
00677      ELSE
00678          MOVE ZEROS              TO W-EXTRA-LENGTH
00679          SET W-WK-NDX DOWN BY +1.
00680
00681  2270-EXIT.
00682      EXIT.
00683                                  EJECT
00684  3000-CREATE-NEW-TEXT.
00685
00686      IF  PI-NEW-NAME EQUAL PI-OLD-NAME
00687          MOVE W-TX-KEY (1)       TO W-TEXT-KEY
00688          PERFORM 4000-DELETE-OLD-TEXT THRU 4000-EXIT.
00689
00690      
      * EXEC CICS HANDLE CONDITION
00691 *         NOTFND     (3200-NOT-FOUND)
00692 *         ENDFILE    (3200-NOT-FOUND)
00693 *         NOTOPEN    (8050-TEXT-NOT-OPEN)
00694 *         DUPREC     (3300-DUP-RECORD)
00695 *    END-EXEC.
      *    MOVE '"$I''J%                ! % #00001665' TO DFHEIV0
           MOVE X'222449274A25202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303031363635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00696
00697      PERFORM 3100-WRITE-NEW-TEXT THRU 3100-EXIT
00698              VARYING
00699          W-TG-NDX FROM 1 BY 1
00700              UNTIL
00701          W-TG-NDX GREATER THAN W-TOTAL-TX-LINES.
00702
00703  3000-EXIT.
00704      EXIT.
00705                                  EJECT
00706  3100-WRITE-NEW-TEXT.
00707
00708      MOVE PI-NEW-NAME            TO W-TX-LETTER-NO (W-TG-NDX).
00709      MOVE W-TX-GRP (W-TG-NDX)    TO W-TEXT-RECORD.
00710      SET W-TR-SEQ-NO             TO W-TG-NDX.
00711
00712      
      * EXEC CICS WRITE
00713 *        DATASET (W-TEXT-FILE-ID)
00714 *        FROM    (W-TEXT-RECORD)
00715 *        RIDFLD  (W-TR-KEY)
00716 *    END-EXEC.
           MOVE LENGTH OF
            W-TEXT-RECORD
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00001687' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TEXT-FILE-ID, 
                 W-TEXT-RECORD, 
                 DFHEIV11, 
                 W-TR-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00717
00718  3100-EXIT.
00719      EXIT.
00720
00721  3200-NOT-FOUND.
00722
00723      MOVE ER-0006                TO EMI-ERROR.
00724      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00725
00726  3200-EXIT.
00727      EXIT.
00728
00729  3300-DUP-RECORD.
00730
00731      MOVE ER-7391                TO EMI-ERROR.
00732      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00733
00734  3300-EXIT.
00735      EXIT.
00736                                  EJECT
00737  4000-DELETE-OLD-TEXT.
00738
00739      
      * EXEC CICS DELETE
00740 *        DATASET (W-TEXT-FILE-ID)
00741 *        RIDFLD  (W-TEXT-KEY)
00742 *        KEYLENGTH (5)
00743 *        GENERIC
00744 *    END-EXEC.
           MOVE 5
             TO DFHEIV11
      *    MOVE '&(  RKG               &   #00001714' TO DFHEIV0
           MOVE X'26282020524B472020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TEXT-FILE-ID, 
                 W-TEXT-KEY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00745
00746  4000-EXIT.
00747      EXIT.
00748
00749  8050-TEXT-NOT-OPEN.
00750
00751      MOVE ER-0013                TO EMI-ERROR.
00752      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00753                                  EJECT
00754  9000-RETURN-TRANS.
00755
00756
00757      
      * EXEC CICS RETURN
00758 *        TRANSID  (W-TRANSACTION)
00759 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
00760 *        LENGTH   (PI-COMM-LENGTH)
00761 *    END-EXEC.
      *    MOVE '.(CT                  &   #00001732' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TRANSACTION, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00762
00763  9000-EXIT.
00764      EXIT.
00765
00766  9400-XCTL.
00767
00768      
      * EXEC CICS XCTL
00769 *        PROGRAM  (W-CALL-PGM)
00770 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
00771 *        LENGTH   (PI-COMM-LENGTH)
00772 *    END-EXEC.
      *    MOVE '.$C                   $   #00001743' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CALL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00773
00774  9400-EXIT.
00775      EXIT.
00776                                  EJECT
00777  9500-LINK-DATE-CONVERT.
00778
00779      IF  DC-BIN-DATE-1 EQUAL HIGH-VALUES
00780              AND
00781          DC-OPTION-CODE EQUAL ' '
00782          MOVE '99/99/99'         TO DC-GREG-DATE-1-EDIT
00783          GO TO 9500-EXIT.
00784
00785      
      * EXEC CICS LINK
00786 *        PROGRAM    ('ELDATCV')
00787 *        COMMAREA   (DATE-CONVERSION-DATA)
00788 *        LENGTH     (DC-COMM-LENGTH)
00789 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00001760' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00790
00791  9500-EXIT.
00792      EXIT.
00793
00794                                  EJECT
00795  9700-PGMID-ERROR.
00796
00797      MOVE '9999'                 TO PI-1043-ERROR.
00798      MOVE PROGRAM-INTERFACE-BLOCK
00799                                  TO DFHCOMMAREA.
00800      
      * EXEC CICS RETURN
00801 *    END-EXEC.
      *    MOVE '.(                    &   #00001775' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00802
00803  9700-EXIT.
00804      EXIT.
00805
00806  9800-ABEND.
00807
00808      MOVE '9999'                 TO PI-1043-ERROR.
00809      MOVE PROGRAM-INTERFACE-BLOCK
00810                                  TO DFHCOMMAREA.
00811      
      * EXEC CICS RETURN
00812 *    END-EXEC.
      *    MOVE '.(                    &   #00001786' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00813
00814  9800-EXIT.
00815      EXIT.
00816                                  EJECT
00817  9900-ERROR-FORMAT.
00818
00819      MOVE EMI-ERROR              TO PI-1043-ERROR
00820
00821      MOVE PROGRAM-INTERFACE-BLOCK
00822                                  TO DFHCOMMAREA.
00823      
      * EXEC CICS RETURN
00824 *    END-EXEC.
      *    MOVE '.(                    &   #00001798' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00825
00826  9900-EXIT.
00827      EXIT.
00828
00829  9999-GOBACK.
00830
00831      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1043' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00832
00833  9999-EXIT.
00834      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1043' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9700-PGMID-ERROR,
                     9800-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 1001-NOT-FOUND,
                     1001-NOT-FOUND,
                     8050-TEXT-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1000-ENDBR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 3200-NOT-FOUND,
                     3200-NOT-FOUND,
                     8050-TEXT-NOT-OPEN,
                     3300-DUP-RECORD
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1043' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
