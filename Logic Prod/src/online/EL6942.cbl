00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL6942.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 10:03:45.
00007 *                            VMOD=2.007
00008 *
00008 *
00009 *AUTHOR.           LOGIC,INC.
00010 *                  DALLAS,TEXAS.
00011
00012 *DATE-COMPILED.
00013
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOCIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023
00024 *REMARKS. TRANSACTION EXM5 - LETTER PRINTER
00025 *        THIS PROGRAM IS USED TO PRINT THE STORED LETTERS AND
00026 *        LABELS  DEPENDING ON THE VALUE OF THE PI-ENTRY-CODES.
00027
00028 *        PRINT INITIAL LETTERS   CODE-1 = 1
00029 *                                CODE-2 = 1
00030
00031 *        PRINT FOLLOW-UP LETTERS CODE-1 = 1
00032 *                                CODE-2 = 2
00033
00034 *        RE-PRINT LETTERS        CODE-1 = 0
00035 *                                CODE-2 = 3
00036
00037 *        PRINT ADDRESS LABELS    CODE-1 = 0
00038 *                                CODE-2 = 2
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
00040  ENVIRONMENT DIVISION.
00041  DATA DIVISION.
00042  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00043  77  FILLER  PIC  X(32) VALUE '********************************'.
00044  77  FILLER  PIC  X(32) VALUE '*   EL6942 WORKING STORAGE     *'.
00045  77  FILLER  PIC  X(32) VALUE '********* V/M 2.007 ************'.
00046
00047  01  W-PROGRAM-CONSTANTS.
00048      12  FILLER                  PIC  X(18)
00049                                  VALUE 'PROGRAM CONSTANTS:'.
00050
00051      12  W-ZEROS                 PIC S9(04)  COMP VALUE +0.
00052
00053      12  W-ARCH-ID               PIC  X(08)  VALUE 'ERARCH'.
00054      12  W-ARCH3-ID              PIC  X(08)  VALUE 'ERARCH3'.
00055      12  W-ARCH4-ID              PIC  X(08)  VALUE 'ERARCH4'.
00056      12  W-ARCH5-ID              PIC  X(08)  VALUE 'ERARCH5'.
00057      12  W-ARCH6-ID              PIC  X(08)  VALUE 'ERARCH6'.
00058      12  W-ARCT-ID               PIC  X(08)  VALUE 'ERARCT'.
00059      12  W-CNTL-ID               PIC  X(08)  VALUE 'ELCNTL'.
00060
00061  01  W-PROGRAM-WORK-AREA.
00062      12  THIS-PGM                PIC  X(8)  VALUE 'EL6942'.
00063      12  FILLER                  PIC  X(18)
00064                                  VALUE 'PROGRAM WORK AREA:'.
00065
00066      12  W-ASKTIME-CTR           PIC S9(04)  COMP.
00067      12  W-ARCHIVE-SAVE          PIC S9(08)  COMP.
00068      12  W-COPIES                PIC  9.
00069      12  W-DELAY-INTERVAL        PIC S9(07)  COMP-3 VALUE +2.
00070      12  W-NDX                   PIC S9(04)  COMP   VALUE +0.
00071      12  W-NUM-OF-TEXT-RECORDS   PIC S9(04)  COMP   VALUE +0.
00072      12  W-NUMBER-OF-LINES       PIC S9(04)  COMP.
00073      12  W-RECORD-COUNT          PIC S9(04)         VALUE +0.
00074      12  W-SAVE-ARCH-NO          PIC S9(08)  COMP   VALUE +0.
00075      12  W-SKIP                  PIC  9(02).
00076      12  W-SUB                   PIC S9(04)  COMP.
00077      12  W-LETTER-TOTALS         PIC  9(07)  COMP-3 VALUE 0.
00078
00079      12  W-ASTERISK-LINE1.
00080          16  FILLER              PIC  X(78)  VALUE ALL '*'.
00081      12  W-ASTERISK-LINE.
00082          16  FILLER              PIC  X(01)  VALUE SPACES.
00083          16  FILLER              PIC  X(78)  VALUE ALL '*'.
00084      12  W-CALL-PGM              PIC  X(08).
00085      12  W-CURRENT-SAVE          PIC  X(02).
00086      12  W-ERROR-LINE            PIC  X(80).
00087      12  W-LAST-RESENT-PRINT-DATE
00088                                  PIC  X(02)  VALUE SPACES.
00089
00090      12  W-LABEL-HOLD-AREA.
00091          16  W-LABEL-LINES OCCURS 6 TIMES INDEXED BY W-L-NDX.
00092              20  W-LABEL-ZIP.
00093                  24  W-LABEL-1ST-ZIP
00094                                  PIC  X(04).
00095                  24  W-LABEL-2ND-ZIP
00096                                  PIC  X(05).
00097              20  FILLER          PIC  X(12).
00098              20  WS-LAST-ZIP.
00099                  24  WS-LAST-1ST-ZIP
00100                                  PIC  X(04).
00101                  24  WS-LAST-2ND-ZIP
00102                                  PIC  X(05).
00103
00104      12  W-SAVE-CURRENT-DATE     PIC  X(08)  VALUE SPACES.
00105      12  W-SAVE-CURRENT-BIN-DATE PIC  X(02)  VALUE SPACES.
00106      12  W-SAVE-LETTER-ARCHIVE   PIC X(250)  VALUE SPACES.
00107      12  W-TOTAL-LINE.
00108          20  FILLER              PIC  X(01)  VALUE SPACES.
00109          20  FILLER              PIC  X(20)
00110              VALUE 'PROCESS COMPLETED.  '.
00111          20  W-TOTAL-LINE-DESC   PIC  X(26)
00112              VALUE 'LETTERS PRINTED TOTAL   - '.
00113          20  W-TOTAL-LETTERS     PIC Z,ZZZ,ZZ9.
00114      12  W-WORKING-RESEND-DATE   PIC  X(02)  VALUE SPACES.
00115      12  W-LABEL-LINE-DESC   PIC  X(26)
00116              VALUE 'LABELS PRINTED TOTAL    - '.
00117
00118  01  W-PROGRAM-KEYS.
00119      12  FILLER                  PIC  X(13)
00120                                  VALUE 'PROGRAM KEYS:'.
00121      12  W-ARCH-KEY.
00122          20  W-ARCH-COMPANY-CD   PIC  X(01).
00123          20  W-ARCH-NUMBER       PIC S9(08)     COMP.
00124
00125      12  W-ARCH3-KEY.
00126          16  W-ARCH3-COMPANY-CD  PIC  X(01).
00127          16  W-ARCH3-FORM        PIC  X(04).
00128          16  W-ARCH3-CARRIER     PIC  X(01).
00129          16  W-ARCH3-GROUPING    PIC  X(06).
00130          16  W-ARCH3-STATE       PIC  X(02).
00131          16  W-ARCH3-ACCOUNT     PIC  X(10).
00132          16  W-ARCH3-ARCHIVE-NO  PIC S9(08)  COMP.
00133
00134      12  W-ARCH4-KEY.
00135          16  W-ARCH4-COMPANY-CD  PIC  X(01).
00136          16  W-ARCH4-PROCESSOR-CD
00137                                  PIC  X(04).
00138          16  W-ARCH4-CARRIER     PIC  X(01).
00139          16  W-ARCH4-GROUPING    PIC  X(06).
00140          16  W-ARCH4-STATE       PIC  X(02).
00141          16  W-ARCH4-ACCOUNT     PIC  X(10).
00142          16  W-ARCH4-ARCHIVE-NO  PIC S9(08)    COMP.
00143
00144      12  W-ARCH5-KEY.
00145          16  W-ARCH5-COMPANY-CD  PIC  X(01).
00146          16  W-ARCH5-CARRIER     PIC  X(01).
00147          16  W-ARCH5-GROUPING    PIC  X(06).
00148          16  W-ARCH5-STATE       PIC  X(02).
00149          16  W-ARCH5-ACCOUNT     PIC  X(10).
00150          16  W-ARCH5-ARCHIVE-NO  PIC S9(08)  COMP.
00151
00152      12  W-ARCH6-KEY.
00153          16  W-ARCH6-COMPANY-CD  PIC  X(01).
00154          16  W-ARCH6-ENTRY.
00155              20  W-ARCH6-FILLER  PIC  X(02).
00156              20  W-ARCH6-CONTROL PIC S9(08)  COMP.
00157          16  W-ARCH6-ARCHIVE-NO  PIC S9(08)  COMP.
00158
00159      12  W-ARCT-KEY.
00160          16  W-ARCT-PARTIAL-KEY.
00161              20  W-ARCT-COMPANY-CD
00162                                  PIC  X(01).
00163              20  W-ARCT-NUMBER   PIC S9(08)     COMP.
00164          16  W-ARCT-REC-ID       PIC  X(01).
00165          16  W-ARCT-SEQ          PIC S9(04)     COMP VALUE +0.
00166
00167      12  W-CNTL-KEY.
00168          16  W-CNTL-COMPANY-ID   PIC  X(03).
00169          16  W-CNTL-RECORD-TYPE  PIC  X(01)  VALUE '1'.
00170          16  W-CNTL-GENL.
00171              20  W-CNTL-GEN1     PIC  X(02)  VALUE SPACES.
00172              20  W-CNTL-GEN2.
00173                  24 W-CNTL-GEN3  PIC  X(01)  VALUE SPACES.
00174                  24 W-CNTL-GEN4  PIC  X(01)  VALUE SPACES.
00175          16  W-CNTL-SEQ          PIC S9(04)  VALUE +0    COMP.
00176
00177  01  W-PROGRAM-SWITCES.
00178      12  FILLER                  PIC  X(17)
00179                                  VALUE 'PROGRAM SWITCHES:'.
00180
00181      12  W-ENDBR-SW              PIC  X(01)          VALUE ' '.
00182          88  W-ENDBR                                 VALUE 'Y'.
00183      12  W-FIRST-FORM-SW         PIC  X(01)          VALUE ' '.
00184          88  W-THIS-IS-FIRST-FORM                    VALUE ' '.
00185          88  W-THIS-IS-NOT-FIRST-FORM                VALUE 'Y'.
00186      12  W-HEADER-BROWSE-STARTED PIC  X(01)          VALUE 'N'.
00187      12  W-HEADER-SW             PIC  X(01)          VALUE SPACE.
00188          88  W-HEADER-REC-FOUND                      VALUE SPACE.
00189      12  W-PRINT-SW              PIC S9(01) COMP-3   VALUE ZERO.
00190      12  W-PROCESSING-SW         PIC S9(01) COMP-3   VALUE ZERO.
00191          88  W-PROCESS-BY-KEY                        VALUE +3.
00192      12  W-TEXT-BROWSE-STARTED   PIC  X(01)          VALUE 'N'.
00193      12  W-TOP-FORM-SW           PIC  X(01)          VALUE SPACE.
00194          88  W-TOP-FORM-SET                          VALUE 'T'.
00195      12  W-OPTION-CODES          PIC  X(02).
00196          88  W-PRINT-INITIAL                         VALUE '11'.
00197          88  W-PRINT-FOLLOW-UP                       VALUE '12'.
00198          88  W-PRINT-LABELS                          VALUE ' 2'.
00199          88  W-REPRINT-LETTERS                       VALUE ' 3'.
00200                                  EJECT
00201  01  FILLER                      PIC  X(25)
00202                              VALUE 'PROGRAM INTERFACE STARTS:'.
00203 *                                COPY ELCINTF.
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
00204      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.
00205 **********************************************************
00206          16  PI-6942-ALIGNMENT-COPIES
00207                                  PIC S9(01) COMP-3.
00208          16  PI-6942-PRINT-DATE  PIC  X(08).
00209          16  PI-6942-PRINT-DATE-BIN
00210                                  PIC  X(02).
00211          16  PI-6942-PRINT-BY-KEY-IND
00212                                  PIC  X(01).
00213              88  PI-6942-PRINT-BY-KEY      VALUE 'C' 'G' 'S' 'A'.
00214              88  PI-6942-PRINT-BY-CARRIER  VALUE 'C'.
00215              88  PI-6942-PRINT-BY-GROUPING VALUE 'G'.
00216              88  PI-6942-PRINT-BY-STATE    VALUE 'S'.
00217              88  PI-6942-PRINT-BY-ACCOUNT  VALUE 'A'.
00218          16  PI-6942-PRINT-BY-PROCESSOR-IND
00219                                  PIC  X(01).
00220              88  PI-6942-PRINT-BY-PROCESSOR
00221                                            VALUE 'Y'.
00222          16  PI-6942-PRINT-ID    PIC  X(04).
00223          16  PI-6942-PRINT-KEY.
00224              20  PI-6942-PRINT-CARRIER
00225                                  PIC  X(01).
00226              20  PI-6942-PRINT-GROUPING
00227                                  PIC  X(06).
00228              20  PI-6942-PRINT-STATE
00229                                  PIC  X(02).
00230              20  PI-6942-PRINT-ACCOUNT
00231                                  PIC  X(10).
00232          16  PI-6942-PRINT-PROCESSOR
00233                                  PIC  X(04).
00234          16  PI-6942-LETTER-FORM PIC  X(04).
00235          16  PI-6942-LETTER-TYPE PIC  X(01).
00236          16  PI-6942-STARTING-ARCH-NO
00237                                  PIC S9(08) COMP.
00238          16  PI-6942-ENTRY.
00239              20  PI-6942-FILLER  PIC  X(02).
00240              20  PI-6942-QUE-CONTROL
00241                                  PIC S9(08) COMP.
00242          16  FILLER              PIC X(585).
00243                                  EJECT
00244 *    COPY ELPRTCVD.
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
00245  01  FILLER.
00246      16  FILLER                  PIC  X(200)
00247          VALUE 'THIS IS PART OF THE BUFFER ZONE'.
00248
00249                                  EJECT
00250  01  FILLER                      PIC  X(18)
00251                              VALUE 'WORK TABLE STARTS:'.
00252
00253  01  W-ADJUST-AREA.
00254      12  FILLER                  PIC  X(07).
00255      12  W-AD-PRINT-AREA         PIC  X(70).
00256      12  FILLER                  PIC  X(03).
00257
00258  01  W-WORK-TABLE.
00259      12  W-WORK-LINE OCCURS 300 TIMES
00260                           INDEXED BY W-WK-NDX.
00261          16  W-TEXT-LINE         PIC  X(70).
00262          16  W-SKIP-CONTROL      PIC  X(02).
00263              88  W-NO-LINES-SKIPPED            VALUE SPACES.
00264              88  W-SKIP-TO-NEXT-PAGE           VALUE '99'.
00265                                  EJECT
00266
00267  01  FILLER                      PIC  X(16)
00268                              VALUE 'WORK TABLE ENDS:'.
00269                                  EJECT
00270 *    COPY ELCDATE.
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
00271                                  EJECT
00272 *    COPY ERCARCH.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCARCH.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = LETTERS SENT TO ARCHIVE FILE              *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 250  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ERARCH                        RKP=2,LEN=5     *
00013 *     ALTERNATE PATH1 = ERARCH2 (CERT/RESP)      RKP=07,LEN=35   *
00014 *     ALTERNATE PATH2 = ERARCH3 (FORM NUMBER)    RKP=44,LEN=28   *
00015 *     ALTERNATE PATH3 = ERARCH4 (PROCCESSOR ID)  RKP=73,LEN=28   *
00016 *     ALTERNATE PATH4 = ERARCH5 (ACCOUNT KEY)    RKP=100,LEN=24  *
00017 *     ALTERNATE PATH5 = ERARCH6 (BTCH/CHK KEY)   RKP=124,LEN=11  *
00018 *                                                                *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00020 ******************************************************************
031011*                   C H A N G E   L O G
031011*
031011* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031011*-----------------------------------------------------------------
031011*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031011* EFFECTIVE    NUMBER
031011*-----------------------------------------------------------------
031011* 031011  CR2007070900001  PEMA  ADD FOLLOW-UP LETTER PROCESSING
031011******************************************************************
00021  01  LETTER-ARCHIVE.
00022      12  LA-RECORD-ID                PIC  X(02).
00023          88  LA-VALID-ID                VALUE 'LA'.
00024
00025      12  LA-CONTROL-PRIMARY.
00026          16  LA-COMPANY-CD           PIC  X(01).
00027          16  LA-ARCHIVE-NO           PIC S9(08)    COMP.
00028
00029      12  LA-CONTROL-BY-CERT-RESP.
00030          16  LA-COMPANY-CD-A2        PIC  X(01).
00031          16  LA-CERT-NO-A2.
00032              20  LA-CERT-PRIME-A2    PIC  X(10).
00033              20  LA-CERT-SUFFIX-A2   PIC  X(01).
00034          16  LA-RSP-PERSON-A2 REDEFINES LA-CERT-NO-A2.
00035              20  LA-RESP-PERSON-A2   PIC  X(10).
00036              20  LA-TYPE-A2          PIC  X(01).
00037          16  LA-CARRIER-A2           PIC  X(01).
00038          16  LA-GROUPING-A2          PIC  X(06).
00039          16  LA-STATE-A2             PIC  X(02).
00040          16  LA-ACCOUNT-A2           PIC  X(10).
00041          16  LA-EFFECT-DATE-A2       PIC  X(02).
00042          16  LA-ARCHIVE-NO-A2        PIC S9(08)    COMP.
00043
00044      12  LA-CONTROL-BY-FORM.
00045          16  LA-COMPANY-CD-A3        PIC  X(01).
00046          16  LA-FORM-A3              PIC  X(04).
00047          16  LA-CARRIER-A3           PIC  X(01).
00048          16  LA-GROUPING-A3          PIC  X(06).
00049          16  LA-STATE-A3             PIC  X(02).
00050          16  LA-ACCOUNT-A3           PIC  X(10).
00051          16  LA-ARCHIVE-NO-A3        PIC S9(08)    COMP.
00052
00053      12  LA-CONTROL-BY-PROCESSOR.
00054          16  LA-COMPANY-CD-A4        PIC  X(01).
00055          16  LA-PROCESSOR-CD         PIC  X(04).
00056          16  LA-CARRIER-A4           PIC  X(01).
00057          16  LA-GROUPING-A4          PIC  X(06).
00058          16  LA-STATE-A4             PIC  X(02).
00059          16  LA-ACCOUNT-A4           PIC  X(10).
00060          16  LA-ARCHIVE-NO-A4        PIC S9(08)    COMP.
00061
00062      12  LA-CONTROL-BY-KEY-FIELDS.
00063          16  LA-COMPANY-CD-A5        PIC  X(01).
00064          16  LA-CARRIER-A5           PIC  X(01).
00065          16  LA-GROUPING-A5          PIC  X(06).
00066          16  LA-STATE-A5             PIC  X(02).
00067          16  LA-ACCOUNT-A5           PIC  X(10).
00068          16  LA-ARCHIVE-NO-A5        PIC S9(08)    COMP.
00069
00070      12  LA-CONTROL-BY-GROUP-CODE.
00071          16  LA-COMPANY-CD-A6        PIC  X(01).
00072          16  LA-ENTRY-A6.
00073              20  LA-FILLER           PIC  X(02).
00074              20  LA-QUE-CONTROL-A6   PIC S9(08)    COMP.
00075          16  LA-ARCHIVE-NO-A6        PIC S9(08)    COMP.
00076
00077      12  FILLER                      PIC  X(09).
00078
00079      12  LA-HEADER-RECORD.
00080          16  LA-NUMBER-LABEL-LINES   PIC S9(04)    COMP.
00081          16  LA-CREATION-DATE        PIC  X(02).
00082          16  LA-FOLLOW-UP-DATE       PIC  X(02).
00083          16  LA-INITIAL-PRINT-DATE   PIC  X(02).
00084          16  LA-NO-OF-COPIES         PIC S9(01).
00085          16  LA-NO-OF-TEXT-RECORDS   PIC S9(04)    COMP.
00086          16  LA-REPLY-DATE           PIC  X(02).
00087          16  LA-RESEND-DATES.
00090              20  LA-RESEND-DATE      PIC  X(02).
00091              20  LA-SENT-DATE        PIC  X(02).
                   20  FILLER              PIC  X(08).
00099          16  LA-SOURCE-INFORMATION.
00100              20  LA-DATA-SOURCE      PIC  X(01).
00101              20  LA-ADDR-SOURCE      PIC  X(01).
00102          16  LA-STATUS               PIC  X(01).
00103              88  LA-STATUS-ACTIVE         VALUE 'A'.
00104              88  LA-STATUS-COMPLETED      VALUE 'C'.
00105              88  LA-STATUS-ON-HOLD        VALUE 'H'.
00106              88  LA-STATUS-TO-BE-PURGED   VALUE 'X'.
00107              88  LA-STATUS-PURGED         VALUE 'P'.
00108              88  LA-STATUS-VOIDED         VALUE 'V'.
00109          16  LA-LAST-RESENT-PRINT-DATE
00110                                      PIC  X(02).
00111          16  LA-PRINT-RESTRICTION    PIC  X(01).
00112              88  LA-PRINT-ONLY-WHEN-CNTL-GIVEN
00113                                           VALUE 'C'.
00114              88  LA-PRINT-ONLY-WHEN-FORM-GIVEN
00115                                           VALUE 'F'.
00116              88  LA-PRINT-ONLY-WHEN-PROC-GIVEN
00117                                           VALUE 'P'.
00118          16  LA-PURGED-DATE          PIC  X(02).
00119          16  LA-VOIDED-DATE          PIC  X(02).
101705         16  LA-RESEND-LETR          PIC  X(4).
               16  FILLER                  PIC  X(08).
101705*        16  LA-RESEND-LETR-2        PIC  X(4).
101705*        16  LA-RESEND-LETR-3        PIC  X(4).
101705         16  FILLER                  PIC  X(59).
00120 *        16  FILLER                  PIC  X(71).
00121
00122 ******************************************************************
00273                                  EJECT
00274 *    COPY ELCDMD34.
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
00275
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
00277 *01 PARMLIST .
00278 *    02  FILLER                  PIC S9(08) COMP.
00279 *    02  L-ARCH-POINTER          PIC S9(08) COMP.
00280 *    02  L-ARCT-POINTER          PIC S9(08) COMP.
00281 *    02  L-CNTL-POINTER          PIC S9(08) COMP.
00282                                  EJECT
       01  DFHCOMMAREA       PIC X(01).
00283  01  L-LETTER-ARCHIVE            PIC X(250).
00284                                  EJECT
00285 *    COPY ERCARCT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCARCT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = TEXT OF ARCHIVED LETTERDS                 *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 1640  RECFORM = FIXED                          *
00011 *                                                                *
00012 *   BASE CLUSTER = ERARCT                        RKP=2,LEN=8     *
00013 *                                                                *
00014 *   LOG = NO                                                     *
00015 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00016 ******************************************************************
00017  01  LETTER-ARCHIVE-TEXT.
00018      12  LT-RECORD-ID                PIC  X(02).
00019          88  LT-VALID-ID                VALUE 'LA'.
00020
00021      12  LT-CONTROL-PRIMARY.
00022          16  LT-COMPANY-CD           PIC  X(01).
00023          16  LT-ARCHIVE-NO           PIC S9(08)    COMP.
00024          16  LT-RECORD-TYPE          PIC  X(01).
00025              88  LT-ADDRESS-DATA        VALUE '1'.
00026              88  LT-TEXT-DATA           VALUE '2'.
00027          16  LT-LINE-SEQ-NO          PIC S9(04)    COMP.
00028
00029      12  FILLER                      PIC  X(28).
00030      12  LT-NUM-LINES-ON-RECORD      PIC S9(04)    COMP.
00031
00032      12  LT-TEXT-RECORD.
00033          16  LT-LETTER-TEXT OCCURS 20 TIMES
00034                             INDEXED BY LT-NDX.
00035              20  LT-TEXT-LINE        PIC  X(70).
00036              20  LT-SKIP-CONTROL     PIC  X(02).
00037                  88  LT-NO-LINES-SKIPPED             VALUE SPACES.
00038                  88  LT-SKIP-TO-NEXT-PAGE            VALUE '99'.
00039              20  FILLER              PIC  X(08).
00040
00286                                  EJECT
00287 *    COPY ELCCNTL.
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
00288                                  EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA L-LETTER-ARCHIVE
                                LETTER-ARCHIVE-TEXT CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6942' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00290
00291      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00292      MOVE '5'                    TO DC-OPTION-CODE.
00293      PERFORM 9700-DATE-LINK THRU 9700-EXIT.
00294      MOVE DC-GREG-DATE-1-EDIT    TO W-SAVE-CURRENT-DATE.
00295      MOVE DC-BIN-DATE-1          TO W-SAVE-CURRENT-BIN-DATE.
00296      MOVE SPACES                 TO DL34-PROCESS-TYPE.
00297
00298  0100-RETRIEVE-LOOP.
00299
00300      
      * EXEC CICS HANDLE CONDITION
00301 *         ENDDATA (0200-END-DATA)
00302 *         NOTFND  (0300-NOT-FOUND)
00303 *    END-EXEC.
      *    MOVE '"$&I                  ! " #00002598' TO DFHEIV0
           MOVE X'222426492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303032353938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00304
00305      
      * EXEC CICS RETRIEVE
00306 *         INTO    (PROGRAM-INTERFACE-BLOCK)
00307 *         LENGTH  (PI-COMM-LENGTH)
00308 *    END-EXEC.
      *    MOVE '0*I L                 &   #00002603' TO DFHEIV0
           MOVE X'302A49204C20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00309
00310
00311 * DLO034 OPEN WHEN DMD OR CID
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
00313          IF DL34-PROCESS-TYPE IS EQUAL TO SPACES
00314              MOVE 'O'                TO DL34-PROCESS-TYPE
00315              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
00316              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
00317              MOVE PI-PROCESSOR-ID    TO DL34-USERID
00318              MOVE SPACES             TO DL34-PRINT-LINE
00319              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID
00320              
      * EXEC CICS LINK
00321 *                PROGRAM    ('DLO034')
00322 *                COMMAREA   (DLO034-COMMUNICATION-AREA)
00323 *                LENGTH     (DLO034-REC-LENGTH)
00324 *            END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   ''   #00002618' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00325              IF DL34-RETURN-CODE NOT = 'OK'
00326                  MOVE  '**DLO034 OPEN ERROR - ABORT**'
00327                                      TO W-ERROR-LINE
00328                  PERFORM 0400-SEND-TEXT
00329                  
      * EXEC CICS RETURN
00330 *                END-EXEC.
      *    MOVE '.(                    &   #00002627' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00331
00332      PERFORM 1000-INITIALIZE THRU 1000-EXIT.
00333      PERFORM 2000-PROCESS-ARCHIVES THRU 2999-EXIT.
00334
00335      IF  W-SAVE-ARCH-NO EQUAL ZEROS
00336              OR
00337          PI-6942-STARTING-ARCH-NO EQUAL W-SAVE-ARCH-NO
00338          GO TO 0200-END-DATA.
00339
00340  0150-UPDATE-CONTROL-FILE.
00341
00342      
      * EXEC CICS HANDLE CONDITION
00343 *         NOTOPEN     (0200-END-DATA)
00344 *         NOTFND      (0200-END-DATA)
00345 *    END-EXEC.
      *    MOVE '"$JI                  ! # #00002640' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303032363430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00346
00347      MOVE SPACES                 TO W-CNTL-KEY.
00348      MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.
00349      MOVE '1'                    TO W-CNTL-RECORD-TYPE.
00350      MOVE ZEROS                  TO W-CNTL-SEQ.
00351
00352      
      * EXEC CICS READ
00353 *         UPDATE
00354 *         DATASET (W-CNTL-ID)
00355 *         SET     (ADDRESS OF CONTROL-FILE)
00356 *         RIDFLD  (W-CNTL-KEY)
00357 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00002650' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00358
00359      MOVE W-SAVE-ARCH-NO         TO CF-CREDIT-START-ARCH-NUM.
00360
00361      
      * EXEC CICS REWRITE
00362 *         DATASET (W-CNTL-ID)
00363 *         FROM    (CONTROL-FILE)
00364 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00002659' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00365
00366  0200-END-DATA.
00367
00368      MOVE '1'                    TO WS-PRINT-AREA.
00369      MOVE W-ASTERISK-LINE1       TO WS-PASSED-DATA.
00370      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00371      MOVE SPACES                 TO WS-PRINT-AREA.
00372      MOVE W-ASTERISK-LINE        TO WS-PASSED-DATA.
00373      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00374
00375      MOVE '0'                    TO WS-PRINT-AREA.
00376
00377      IF W-PRINT-LABELS
00378          MOVE W-LABEL-LINE-DESC  TO W-TOTAL-LINE-DESC.
00379
00380      MOVE W-LETTER-TOTALS        TO W-TOTAL-LETTERS.
00381      MOVE W-TOTAL-LINE           TO WS-PASSED-DATA.
00382      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00383
00384      MOVE '0'                    TO WS-PRINT-AREA.
00385      MOVE W-ASTERISK-LINE        TO WS-PASSED-DATA.
00386      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00387      MOVE SPACES                 TO WS-PRINT-AREA.
00388      MOVE W-ASTERISK-LINE        TO WS-PASSED-DATA.
00389      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00390      MOVE '1'                    TO WS-PRINT-AREA.
00391      MOVE SPACES                 TO WS-PASSED-DATA.
00392      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00393
00394      MOVE 'X'                    TO WS-PROG-END.
00395      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00396
00397 * DLO034 CLOSE
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
00399          MOVE 'C'                TO DL34-PROCESS-TYPE
00400          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
00401          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
00402          MOVE PI-PROCESSOR-ID    TO DL34-USERID
00403          MOVE SPACES             TO DL34-PRINT-LINE
00404                                     DL34-OVERRIDE-PRINTER-ID
00405          
      * EXEC CICS LINK
00406 *            PROGRAM    ('DLO034')
00407 *            COMMAREA   (DLO034-COMMUNICATION-AREA)
00408 *            LENGTH     (DLO034-REC-LENGTH)
00409 *        END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   ''   #00002703' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00410          IF DL34-RETURN-CODE NOT = 'OK'
00411              MOVE  '**DLO034 CLOSE ERROR - ABORT**'
00412                                  TO W-ERROR-LINE
00413              PERFORM 0400-SEND-TEXT.
00414
00415      
      * EXEC CICS RETURN
00416 *    END-EXEC.
      *    MOVE '.(                    &   #00002713' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00417
00418  0300-NOT-FOUND.
00419
00420      MOVE 'NO COMMUNICATION AREA FOUND'
00421                                  TO W-ERROR-LINE.
00422      PERFORM 0400-SEND-TEXT.
00423      GO TO 0200-END-DATA.
00424
00425  0400-SEND-TEXT.
00426
00427      
      * EXEC CICS SEND TEXT
00428 *        FROM   (W-ERROR-LINE)
00429 *        LENGTH (70)
00430 *    END-EXEC.
           MOVE 70
             TO DFHEIV11
      *    MOVE '8&      T       H   F -   #00002725' TO DFHEIV0
           MOVE X'382620202020202054202020' TO DFHEIV0(1:12)
           MOVE X'202020204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373235' TO DFHEIV0(25:11)
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
           
00431                                  EJECT
00432  1000-INITIALIZE.
00433
00434      MOVE SPACES                 TO W-ADJUST-AREA.
00435      MOVE W-SAVE-CURRENT-BIN-DATE
00436                                  TO W-CURRENT-SAVE.
00437
00438      MOVE PI-ENTRY-CODES         TO W-OPTION-CODES.
00439      MOVE PI-6942-STARTING-ARCH-NO
00440                                  TO W-SAVE-ARCH-NO.
00441
00442 *****************************************************************
00443 *  THE FOLLOWING 'IF' LOGIC SETS THE PRIORITIES WHEN THE USER   *
00444 *  ENTERS MORE THAN ONE PRINT RESTRICTION.  THE LOGIC USING     *
00445 *  W-PROCESSING-SW MAKES DECISIONS BASED ON THIS PRIORITY.      *
00446 *  THE PRIORITY IS 1. BRANCH ENTRY/ CHECK QUE CONTROL           *
00447 *                  2. PROCESSOR ID                              *
00448 *                  3. LETTER FORM CODE                          *
00449 *                  4. KEY FIELDS                                *
00450 *                  5. NO RESTRICTIONS                           *
00451 *****************************************************************
00452
00453      IF  PI-6942-ENTRY GREATER THAN LOW-VALUES
00454          MOVE 5                  TO W-PROCESSING-SW
00455      ELSE
00456          IF  PI-6942-PRINT-BY-PROCESSOR
00457              MOVE 1              TO W-PROCESSING-SW
00458          ELSE
00459              IF  PI-6942-LETTER-FORM GREATER THAN SPACES
00460                  MOVE 2          TO W-PROCESSING-SW
00461              ELSE
00462                  IF  PI-6942-PRINT-BY-KEY
00463                      MOVE 3      TO W-PROCESSING-SW
00464                  ELSE
00465                      MOVE 4      TO W-PROCESSING-SW.
00466
00467  1000-EXIT.
00468      EXIT.
00469                                  EJECT
00470  2000-PROCESS-ARCHIVES.
00471
00472      GO TO 2100-BY-PROCESSOR
00473            2200-BY-LETTER
00474            2300-BY-KEY
00475            2400-BY-ARCHIVE
00476            2500-BY-CONTROL-ENTRY
00477                                  DEPENDING ON W-PROCESSING-SW.
00478
00479                                  EJECT
00480  2100-BY-PROCESSOR.
00481
00482      MOVE LOW-VALUES             TO W-ARCH4-KEY.
00483      MOVE PI-COMPANY-CD          TO W-ARCH4-COMPANY-CD.
00484      MOVE PI-6942-PRINT-PROCESSOR
00485                                  TO W-ARCH4-PROCESSOR-CD.
00486      MOVE PI-6942-PRINT-CARRIER  TO W-ARCH4-CARRIER.
00487      MOVE PI-6942-PRINT-GROUPING TO W-ARCH4-GROUPING.
00488      MOVE PI-6942-PRINT-STATE    TO W-ARCH4-STATE.
00489      MOVE PI-6942-PRINT-ACCOUNT  TO W-ARCH4-ACCOUNT.
00490
00491      IF  W-PRINT-INITIAL
00492              OR
00493          W-PRINT-FOLLOW-UP
00494          COMPUTE W-ARCH4-ARCHIVE-NO
00495              = PI-6942-STARTING-ARCH-NO - 1.
00496
00497      
      * EXEC CICS HANDLE CONDITION
00498 *         NOTOPEN  (8800-ARCH4-NOT-OPEN)
00499 *         NOTFND   (2999-EXIT)
00500 *         ENDFILE  (2999-EXIT)
00501 *    END-EXEC.
      *    MOVE '"$JI''                 ! $ #00002795' TO DFHEIV0
           MOVE X'22244A492720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303032373935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00502
00503  2100-READ-NEXT.
00504
00505      ADD +1                      TO W-ARCH4-ARCHIVE-NO.
00506
00507      
      * EXEC CICS READ
00508 *         DATASET (W-ARCH4-ID)
00509 *         RIDFLD  (W-ARCH4-KEY)
00510 *         SET     (ADDRESS OF L-LETTER-ARCHIVE)
00511 *         GTEQ
00512 *    END-EXEC.
      *    MOVE '&"S        G          (   #00002805' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH4-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH4-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF L-LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00513
00514      MOVE L-LETTER-ARCHIVE       TO LETTER-ARCHIVE.
00515
00516      IF  PI-COMPANY-CD NOT EQUAL LA-COMPANY-CD
00517          GO TO 2999-EXIT.
00518
00519      IF  LA-PROCESSOR-CD NOT EQUAL PI-6942-PRINT-PROCESSOR
00520          GO TO 2999-EXIT.
00521
00522      MOVE LA-CONTROL-BY-PROCESSOR
00523                                  TO W-ARCH4-KEY.
00524
00525      IF  LA-REPLY-DATE GREATER THAN LOW-VALUES
00526              OR
00527          LA-STATUS-VOIDED
00528              OR
00529          LA-STATUS-PURGED
00530              OR
00531          LA-STATUS-TO-BE-PURGED
00532              OR
00533          LA-STATUS-ON-HOLD
00534          GO TO 2100-READ-NEXT.
00535
00536      ADD +1                      TO W-RECORD-COUNT.
00537
00538      IF  W-RECORD-COUNT IS GREATER THAN +50
00539          MOVE +0                 TO W-RECORD-COUNT
00540
00541          
      * EXEC CICS DELAY
00542 *            INTERVAL  (W-DELAY-INTERVAL)
00543 *        END-EXEC.
      *    MOVE '0$I                   &   #00002839' TO DFHEIV0
           MOVE X'302449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-DELAY-INTERVAL, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00544
00545      MOVE LA-ARCHIVE-NO          TO W-ARCHIVE-SAVE.
00546
00547      IF  W-REPRINT-LETTERS
00548          PERFORM 6000-REPRINT-CHECKS THRU 6299-EXIT
00549          GO TO 2100-READ-NEXT.
00550
00551      IF  W-PRINT-LABELS
00552          PERFORM 6300-LABEL-CHECKS THRU 6399-EXIT
00553          GO TO 2100-READ-NEXT.
00554
00555      IF  LA-STATUS-COMPLETED
00556          GO TO 2100-READ-NEXT.
00557
00558      IF  W-PRINT-INITIAL
00559          PERFORM 3000-INITIAL-CHECKS THRU 3999-EXIT
00560          GO TO 2100-READ-NEXT.
00561
00562      IF  W-PRINT-FOLLOW-UP
00563          PERFORM 4000-FOLLOW-UP-CHECKS THRU 4199-EXIT
00564          GO TO 2100-READ-NEXT.
00565
00566      GO TO 2100-READ-NEXT.
00567
00568  2100-EXIT.
00569      EXIT.
00570                                  EJECT
00571  2200-BY-LETTER.
00572
00573      MOVE LOW-VALUES             TO W-ARCH3-KEY.
00574      MOVE PI-COMPANY-CD          TO W-ARCH3-COMPANY-CD.
00575      MOVE PI-6942-LETTER-FORM    TO W-ARCH3-FORM.
00576      MOVE PI-6942-PRINT-CARRIER  TO W-ARCH3-CARRIER.
00577      MOVE PI-6942-PRINT-GROUPING TO W-ARCH3-GROUPING.
00578      MOVE PI-6942-PRINT-STATE    TO W-ARCH3-STATE.
00579      MOVE PI-6942-PRINT-ACCOUNT  TO W-ARCH3-ACCOUNT.
00580
00581      IF  W-PRINT-INITIAL
00582              OR
00583          W-PRINT-FOLLOW-UP
00584          COMPUTE W-ARCH3-ARCHIVE-NO
00585              = PI-6942-STARTING-ARCH-NO - 1.
00586
00587      
      * EXEC CICS HANDLE CONDITION
00588 *         NOTOPEN  (8810-ARCH3-NOT-OPEN)
00589 *         NOTFND   (2999-EXIT)
00590 *         ENDFILE  (2999-EXIT)
00591 *    END-EXEC.
      *    MOVE '"$JI''                 ! % #00002885' TO DFHEIV0
           MOVE X'22244A492720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303032383835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00592
00593  2200-READ-NEXT.
00594
00595      ADD +1                      TO W-ARCH3-ARCHIVE-NO.
00596
00597      
      * EXEC CICS READ
00598 *         DATASET (W-ARCH3-ID)
00599 *         RIDFLD  (W-ARCH3-KEY)
00600 *         SET     (ADDRESS OF L-LETTER-ARCHIVE)
00601 *         GTEQ
00602 *    END-EXEC.
      *    MOVE '&"S        G          (   #00002895' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH3-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH3-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF L-LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00603
00604      MOVE L-LETTER-ARCHIVE       TO LETTER-ARCHIVE.
00605
00606      IF  PI-COMPANY-CD NOT EQUAL LA-COMPANY-CD
00607          GO TO 2999-EXIT.
00608
00609      IF  LA-FORM-A3 NOT EQUAL PI-6942-LETTER-FORM
00610          GO TO 2999-EXIT.
00611
00612      MOVE LA-CONTROL-BY-FORM     TO W-ARCH3-KEY.
00613
00614      IF  LA-REPLY-DATE GREATER THAN LOW-VALUES
00615              OR
00616          LA-STATUS-VOIDED
00617              OR
00618          LA-STATUS-PURGED
00619              OR
00620          LA-STATUS-TO-BE-PURGED
00621              OR
00622          LA-STATUS-ON-HOLD
00623          GO TO 2200-READ-NEXT.
00624
00625      ADD +1                      TO W-RECORD-COUNT.
00626
00627      IF  W-RECORD-COUNT IS GREATER THAN +50
00628          MOVE +0                 TO W-RECORD-COUNT
00629
00630          
      * EXEC CICS DELAY
00631 *            INTERVAL  (W-DELAY-INTERVAL)
00632 *        END-EXEC.
      *    MOVE '0$I                   &   #00002928' TO DFHEIV0
           MOVE X'302449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-DELAY-INTERVAL, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00633
00634      MOVE LA-ARCHIVE-NO          TO W-ARCHIVE-SAVE.
00635
00636      IF  W-REPRINT-LETTERS
00637          PERFORM 6000-REPRINT-CHECKS THRU 6299-EXIT
00638          GO TO 2200-READ-NEXT.
00639
00640      IF  W-PRINT-LABELS
00641          PERFORM 6300-LABEL-CHECKS THRU 6399-EXIT
00642          GO TO 2200-READ-NEXT.
00643
00644      IF  LA-STATUS-COMPLETED
00645          GO TO 2200-READ-NEXT.
00646
00647      IF  W-PRINT-INITIAL
00648          PERFORM 3000-INITIAL-CHECKS THRU 3999-EXIT
00649          GO TO 2200-READ-NEXT.
00650
00651      IF  W-PRINT-FOLLOW-UP
00652          PERFORM 4000-FOLLOW-UP-CHECKS THRU 4199-EXIT
00653          GO TO 2200-READ-NEXT.
00654
00655      GO TO 2200-READ-NEXT.
00656                                  EJECT
00657  2300-BY-KEY.
00658
00659      MOVE LOW-VALUES             TO W-ARCH5-KEY.
00660      MOVE PI-COMPANY-CD          TO W-ARCH5-COMPANY-CD.
00661      MOVE PI-6942-PRINT-CARRIER  TO W-ARCH5-CARRIER.
00662      MOVE PI-6942-PRINT-GROUPING TO W-ARCH5-GROUPING.
00663      MOVE PI-6942-PRINT-STATE    TO W-ARCH5-STATE.
00664      MOVE PI-6942-PRINT-ACCOUNT  TO W-ARCH5-ACCOUNT.
00665
00666      IF  W-PRINT-INITIAL
00667              OR
00668          W-PRINT-FOLLOW-UP
00669          COMPUTE W-ARCH5-ARCHIVE-NO
00670              = PI-6942-STARTING-ARCH-NO - 1.
00671
00672      
      * EXEC CICS HANDLE CONDITION
00673 *         NOTOPEN  (8860-ARCH5-NOT-OPEN)
00674 *         NOTFND   (2999-EXIT)
00675 *         ENDFILE  (2999-EXIT)
00676 *    END-EXEC.
      *    MOVE '"$JI''                 ! & #00002970' TO DFHEIV0
           MOVE X'22244A492720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303032393730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00677
00678  2300-READ-NEXT.
00679
00680      ADD +1                      TO W-ARCH5-ARCHIVE-NO.
00681
00682      
      * EXEC CICS READ
00683 *         DATASET (W-ARCH5-ID)
00684 *         RIDFLD  (W-ARCH5-KEY)
00685 *         SET     (ADDRESS OF L-LETTER-ARCHIVE)
00686 *         GTEQ
00687 *    END-EXEC.
      *    MOVE '&"S        G          (   #00002980' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH5-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH5-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF L-LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00688
00689      MOVE L-LETTER-ARCHIVE       TO LETTER-ARCHIVE.
00690
00691      IF  PI-COMPANY-CD NOT EQUAL LA-COMPANY-CD
00692          GO TO 2999-EXIT.
00693
00694      MOVE LA-CONTROL-BY-KEY-FIELDS
00695                                  TO W-ARCH5-KEY.
00696
00697      IF  LA-REPLY-DATE GREATER THAN LOW-VALUES
00698              OR
00699          LA-STATUS-VOIDED
00700              OR
00701          LA-STATUS-PURGED
00702              OR
00703          LA-STATUS-TO-BE-PURGED
00704              OR
00705          LA-STATUS-ON-HOLD
00706          GO TO 2300-READ-NEXT.
00707
00708      ADD +1                      TO W-RECORD-COUNT.
00709
00710      IF  W-RECORD-COUNT IS GREATER THAN +50
00711          MOVE +0                 TO W-RECORD-COUNT
00712          
      * EXEC CICS DELAY
00713 *            INTERVAL  (W-DELAY-INTERVAL)
00714 *        END-EXEC.
      *    MOVE '0$I                   &   #00003010' TO DFHEIV0
           MOVE X'302449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-DELAY-INTERVAL, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00715
00716      MOVE LA-ARCHIVE-NO          TO W-ARCHIVE-SAVE.
00717
00718      IF  W-REPRINT-LETTERS
00719          PERFORM 6000-REPRINT-CHECKS THRU 6299-EXIT
00720          GO TO 2300-READ-NEXT.
00721
00722      IF  W-PRINT-LABELS
00723          PERFORM 6300-LABEL-CHECKS THRU 6399-EXIT
00724          GO TO 2300-READ-NEXT.
00725
00726      IF  LA-STATUS-COMPLETED
00727          GO TO 2300-READ-NEXT.
00728
00729      IF  W-PRINT-INITIAL
00730          PERFORM 3000-INITIAL-CHECKS THRU 3999-EXIT
00731          GO TO 2300-READ-NEXT.
00732
00733      IF  W-PRINT-FOLLOW-UP
00734          PERFORM 4000-FOLLOW-UP-CHECKS THRU 4199-EXIT
00735          GO TO 2300-READ-NEXT.
00736
00737      GO TO 2300-READ-NEXT.
00738                                  EJECT
00739  2400-BY-ARCHIVE.
00740
00741      MOVE LOW-VALUES             TO W-ARCH-KEY.
00742      MOVE PI-COMPANY-CD          TO W-ARCH-COMPANY-CD.
00743
00744      IF  W-PRINT-INITIAL
00745              OR
00746          W-PRINT-FOLLOW-UP
00747          COMPUTE W-ARCH-NUMBER
00748              = PI-6942-STARTING-ARCH-NO - 1.
00749
00750      
      * EXEC CICS HANDLE CONDITION
00751 *         NOTOPEN  (8870-ARCH-NOT-OPEN)
00752 *         NOTFND   (2999-EXIT)
00753 *         ENDFILE  (2999-EXIT)
00754 *    END-EXEC.
      *    MOVE '"$JI''                 ! '' #00003048' TO DFHEIV0
           MOVE X'22244A492720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303033303438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00755
00756  2400-READ-NEXT.
00757
00758      ADD +1                      TO W-ARCH-NUMBER.
00759
00760      
      * EXEC CICS READ
00761 *         DATASET (W-ARCH-ID)
00762 *         RIDFLD  (W-ARCH-KEY)
00763 *         SET     (ADDRESS OF L-LETTER-ARCHIVE)
00764 *         GTEQ
00765 *    END-EXEC.
      *    MOVE '&"S        G          (   #00003058' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF L-LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00766
00767      MOVE L-LETTER-ARCHIVE       TO LETTER-ARCHIVE.
00768
00769      IF  PI-COMPANY-CD NOT EQUAL LA-COMPANY-CD
00770          GO TO 2999-EXIT.
00771
00772      MOVE LA-CONTROL-PRIMARY     TO W-ARCH-KEY.
00773
00774      IF  LA-REPLY-DATE GREATER THAN LOW-VALUES
00775              OR
00776          LA-STATUS-VOIDED
00777              OR
00778          LA-STATUS-PURGED
00779              OR
00780          LA-STATUS-TO-BE-PURGED
00781              OR
00782          LA-STATUS-ON-HOLD
00783          GO TO 2400-READ-NEXT.
00784
00785      ADD +1                      TO W-RECORD-COUNT.
00786
00787      IF  W-RECORD-COUNT IS GREATER THAN +50
00788          MOVE +0                 TO W-RECORD-COUNT
00789          
      * EXEC CICS DELAY
00790 *            INTERVAL  (W-DELAY-INTERVAL)
00791 *        END-EXEC.
      *    MOVE '0$I                   &   #00003087' TO DFHEIV0
           MOVE X'302449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-DELAY-INTERVAL, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00792
00793      MOVE LA-ARCHIVE-NO          TO W-ARCHIVE-SAVE.
00794
00795      IF  W-REPRINT-LETTERS
00796          PERFORM 6000-REPRINT-CHECKS THRU 6299-EXIT
00797          GO TO 2400-READ-NEXT.
00798
00799      IF  W-PRINT-LABELS
00800          PERFORM 6300-LABEL-CHECKS THRU 6399-EXIT
00801          GO TO 2400-READ-NEXT.
00802
00803      IF  LA-STATUS-COMPLETED
00804          GO TO 2400-READ-NEXT.
00805
00806      IF  W-PRINT-INITIAL
00807          PERFORM 3000-INITIAL-CHECKS THRU 3999-EXIT
00808          GO TO 2400-READ-NEXT.
00809
00810      IF  W-PRINT-FOLLOW-UP
00811          PERFORM 4000-FOLLOW-UP-CHECKS THRU 4199-EXIT
00812          GO TO 2400-READ-NEXT.
00813
00814      GO TO 2400-READ-NEXT.
00815                                  EJECT
00816  2500-BY-CONTROL-ENTRY.
00817
00818      MOVE LOW-VALUES             TO W-ARCH6-KEY.
00819      MOVE PI-COMPANY-CD          TO W-ARCH6-COMPANY-CD.
00820      MOVE PI-6942-ENTRY          TO W-ARCH6-ENTRY.
00821
00822      IF  W-PRINT-INITIAL
00823              OR
00824          W-PRINT-FOLLOW-UP
00825          COMPUTE W-ARCH6-ARCHIVE-NO
00826              = PI-6942-STARTING-ARCH-NO - 1.
00827
00828      
      * EXEC CICS HANDLE CONDITION
00829 *         NOTOPEN  (8860-ARCH5-NOT-OPEN)
00830 *    END-EXEC.
      *    MOVE '"$J                   ! ( #00003126' TO DFHEIV0
           MOVE X'22244A202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303033313236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00831
00832  2500-READ-NEXT.
00833
00834      
      * EXEC CICS HANDLE CONDITION
00835 *         ENDFILE  (2999-EXIT)
00836 *         NOTFND   (2999-EXIT)
00837 *    END-EXEC.
      *    MOVE '"$''I                  ! ) #00003132' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303033313332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00838
00839      ADD +1                      TO W-ARCH6-ARCHIVE-NO.
00840
00841      
      * EXEC CICS READ
00842 *         DATASET (W-ARCH6-ID)
00843 *         RIDFLD  (W-ARCH6-KEY)
00844 *         SET     (ADDRESS OF L-LETTER-ARCHIVE)
00845 *         GTEQ
00846 *    END-EXEC.
      *    MOVE '&"S        G          (   #00003139' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH6-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH6-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF L-LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00847
00848      MOVE L-LETTER-ARCHIVE       TO LETTER-ARCHIVE.
00849
00850      IF  PI-COMPANY-CD NOT EQUAL LA-COMPANY-CD
00851          GO TO 2999-EXIT.
00852
00853      IF  PI-6942-ENTRY NOT EQUAL LA-ENTRY-A6
00854          GO TO 2999-EXIT.
00855
00856      MOVE LA-CONTROL-BY-GROUP-CODE
00857                                  TO W-ARCH6-KEY.
00858
00859      IF  LA-REPLY-DATE GREATER THAN LOW-VALUES
00860              OR
00861          LA-STATUS-VOIDED
00862              OR
00863          LA-STATUS-PURGED
00864              OR
00865          LA-STATUS-TO-BE-PURGED
00866              OR
00867          LA-STATUS-ON-HOLD
00868          GO TO 2500-READ-NEXT.
00869
00870      ADD +1                      TO W-RECORD-COUNT.
00871
00872      IF  W-RECORD-COUNT IS GREATER THAN +50
00873          MOVE +0                 TO W-RECORD-COUNT
00874          
      * EXEC CICS DELAY
00875 *            INTERVAL  (W-DELAY-INTERVAL)
00876 *        END-EXEC.
      *    MOVE '0$I                   &   #00003172' TO DFHEIV0
           MOVE X'302449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-DELAY-INTERVAL, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00877
00878      MOVE LA-ARCHIVE-NO          TO W-ARCHIVE-SAVE.
00879
00880      IF  W-REPRINT-LETTERS
00881          PERFORM 6000-REPRINT-CHECKS THRU 6299-EXIT
00882          GO TO 2500-READ-NEXT.
00883
00884      IF  W-PRINT-LABELS
00885          PERFORM 6300-LABEL-CHECKS THRU 6399-EXIT
00886          GO TO 2500-READ-NEXT.
00887
00888      IF  LA-STATUS-COMPLETED
00889          GO TO 2500-READ-NEXT.
00890
00891      IF  W-PRINT-INITIAL
00892          PERFORM 3000-INITIAL-CHECKS THRU 3999-EXIT
00893          GO TO 2500-READ-NEXT.
00894
00895      IF  W-PRINT-FOLLOW-UP
00896          PERFORM 4000-FOLLOW-UP-CHECKS THRU 4199-EXIT
00897          GO TO 2500-READ-NEXT.
00898
00899      GO TO 2500-READ-NEXT.
00900
00901  2999-EXIT.
00902      EXIT.
00903                                  EJECT
00904  3000-INITIAL-CHECKS.
00905
00906      IF  LA-INITIAL-PRINT-DATE NOT EQUAL LOW-VALUES
00907          GO TO 3999-EXIT.
00908
00909      PERFORM 3900-CHECK-FOR-ACTIVE-RESENDS THRU 3900-EXIT
031011     IF PI-6942-PRINT-DATE-BIN NOT = LOW-VALUES
031011        IF LA-CREATION-DATE NOT = PI-6942-PRINT-DATE-BIN
031011           GO TO 3999-EXIT
031011        END-IF
031011     END-IF
00923      GO TO 3100-BY-PROCESSOR
00924            3200-BY-LETTER
00925            3300-BY-KEY
00926            3400-BY-ARCHIVE
00927            3500-BY-CONTROL-ENTRY
00928                                  DEPENDING ON W-PROCESSING-SW.
00929      GO TO 3999-EXIT.
00930                                  EJECT
00931  3100-BY-PROCESSOR.
00932
00933      IF  LA-PRINT-ONLY-WHEN-CNTL-GIVEN
00934          GO TO 3999-EXIT.
00935
00936      IF  PI-6942-LETTER-FORM GREATER THAN SPACES
00937              AND
00938          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3
00939          GO TO 3999-EXIT.
00940
00941      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN
00942              AND
00943          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3
00944          GO TO 3999-EXIT.
00945
00946      IF  NOT PI-6942-PRINT-BY-KEY
00947          GO TO 3100-CONTINUE.
00948
00949      IF  LA-CARRIER-A4 GREATER THAN PI-6942-PRINT-CARRIER
00950          MOVE 'Y'                TO W-ENDBR-SW
00951          GO TO 3999-EXIT.
00952
00953      IF  PI-6942-PRINT-BY-CARRIER
00954          GO TO 3100-CONTINUE.
00955
00956      IF  LA-GROUPING-A4 GREATER THAN PI-6942-PRINT-GROUPING
00957          MOVE 'Y'                TO W-ENDBR-SW
00958          GO TO 3999-EXIT.
00959
00960      IF  PI-6942-PRINT-BY-GROUPING
00961          GO TO 3100-CONTINUE.
00962
00963      IF  LA-STATE-A4 GREATER THAN PI-6942-PRINT-STATE
00964          MOVE 'Y'                TO W-ENDBR-SW
00965          GO TO 3999-EXIT.
00966
00967      IF  PI-6942-PRINT-BY-STATE
00968          GO TO 3100-CONTINUE.
00969
00970      IF  LA-ACCOUNT-A4 GREATER THAN PI-6942-PRINT-ACCOUNT
00971          MOVE 'Y'                TO W-ENDBR-SW
00972          GO TO 3999-EXIT.
00973
00974  3100-CONTINUE.
00975
00976      MOVE LA-NO-OF-COPIES        TO W-COPIES.
00977
00978      MOVE W-CURRENT-SAVE         TO LA-INITIAL-PRINT-DATE.
00979
00980      IF  LA-RESEND-DATE = LOW-VALUES
00981          MOVE 'C'                TO LA-STATUS.
00982
00983      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.
00984
00985      IF  W-THIS-IS-FIRST-FORM
00986          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
00987              PI-6942-ALIGNMENT-COPIES TIMES
00988          MOVE 'Y'                TO W-FIRST-FORM-SW.
00989
00990      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
00991          W-COPIES TIMES.
00992
00993      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.
00994
00995      GO TO 3999-EXIT.
00996
00997  3100-EXIT.
00998       EXIT.
00999                                  EJECT
01000  3200-BY-LETTER.
01001
01002      IF  LA-PRINT-ONLY-WHEN-CNTL-GIVEN
01003              OR
01004          LA-PRINT-ONLY-WHEN-PROC-GIVEN
01005          GO TO 3999-EXIT.
01006
01007      IF  NOT PI-6942-PRINT-BY-KEY
01008          GO TO 3200-CONTINUE.
01009
01010      IF  LA-CARRIER-A3 GREATER THAN PI-6942-PRINT-CARRIER
01011          MOVE 'Y'                TO W-ENDBR-SW
01012          GO TO 3999-EXIT.
01013
01014      IF  PI-6942-PRINT-BY-CARRIER
01015          GO TO 3200-CONTINUE.
01016
01017      IF  LA-GROUPING-A3 GREATER THAN PI-6942-PRINT-GROUPING
01018          MOVE 'Y'                TO W-ENDBR-SW
01019          GO TO 3999-EXIT.
01020
01021      IF  PI-6942-PRINT-BY-GROUPING
01022          GO TO 3200-CONTINUE.
01023
01024      IF  LA-STATE-A3 GREATER THAN PI-6942-PRINT-STATE
01025          MOVE 'Y'                TO W-ENDBR-SW
01026          GO TO 3999-EXIT.
01027
01028      IF  PI-6942-PRINT-BY-STATE
01029          GO TO 3200-CONTINUE.
01030
01031      IF  LA-ACCOUNT-A3 GREATER THAN PI-6942-PRINT-ACCOUNT
01032          MOVE 'Y'                TO W-ENDBR-SW
01033          GO TO 3999-EXIT.
01034
01035  3200-CONTINUE.
01036
01037      MOVE LA-NO-OF-COPIES        TO W-COPIES.
01038
01039      MOVE W-CURRENT-SAVE         TO LA-INITIAL-PRINT-DATE.
01040
01041      IF  LA-RESEND-DATE   EQUAL LOW-VALUES
01042          MOVE 'C'                TO LA-STATUS.
01043
01044      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.
01045
01046      IF  W-THIS-IS-FIRST-FORM
01047          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01048              PI-6942-ALIGNMENT-COPIES TIMES
01049          MOVE 'Y'                TO W-FIRST-FORM-SW.
01050
01051      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01052          W-COPIES TIMES.
01053
01054      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.
01055
01056      GO TO 3999-EXIT.
01057
01058  3200-EXIT.
01059       EXIT.
01060                                  EJECT
01061  3300-BY-KEY.
01062
01063      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN
01064              OR
01065          LA-PRINT-ONLY-WHEN-CNTL-GIVEN
01066              OR
01067          LA-PRINT-ONLY-WHEN-PROC-GIVEN
01068          GO TO 3999-EXIT.
01069
01070      IF  LA-CARRIER-A5 GREATER THAN PI-6942-PRINT-CARRIER
01071          MOVE 'Y'                TO W-ENDBR-SW
01072          GO TO 3999-EXIT.
01073
01074      IF  PI-6942-PRINT-BY-CARRIER
01075          GO TO 3300-CONTINUE.
01076
01077      IF  LA-GROUPING-A5 GREATER THAN PI-6942-PRINT-GROUPING
01078          MOVE 'Y'                TO W-ENDBR-SW
01079          GO TO 3999-EXIT.
01080
01081      IF  PI-6942-PRINT-BY-GROUPING
01082          GO TO 3300-CONTINUE.
01083
01084      IF  LA-STATE-A5 GREATER THAN PI-6942-PRINT-STATE
01085          MOVE 'Y'                TO W-ENDBR-SW
01086          GO TO 3999-EXIT.
01087
01088      IF  PI-6942-PRINT-BY-STATE
01089          GO TO 3300-CONTINUE.
01090
01091      IF  LA-ACCOUNT-A5 GREATER THAN PI-6942-PRINT-ACCOUNT
01092          MOVE 'Y'                TO W-ENDBR-SW
01093          GO TO 3999-EXIT.
01094
01095  3300-CONTINUE.
01096
01097      MOVE LA-NO-OF-COPIES        TO W-COPIES.
01098
01099      MOVE W-CURRENT-SAVE         TO LA-INITIAL-PRINT-DATE.
01100      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.
01101
01102      IF  W-THIS-IS-FIRST-FORM
01103          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01104              PI-6942-ALIGNMENT-COPIES TIMES
01105          MOVE 'Y'                TO W-FIRST-FORM-SW.
01106
01107      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01108          W-COPIES TIMES.
01109
01110      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.
01111
01112      GO TO 3999-EXIT.
01113
01114  3300-EXIT.
01115       EXIT.
01116                                  EJECT
01117  3400-BY-ARCHIVE.
01118
01119      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN
01120              OR
01121          LA-PRINT-ONLY-WHEN-CNTL-GIVEN
01122              OR
01123          LA-PRINT-ONLY-WHEN-PROC-GIVEN
01124          GO TO 3999-EXIT.
01125
01126      MOVE LA-NO-OF-COPIES        TO W-COPIES.
01127
01128      MOVE W-CURRENT-SAVE         TO LA-INITIAL-PRINT-DATE.
01129
01130      IF  LA-RESEND-DATE   EQUAL LOW-VALUES
01131          MOVE 'C'                TO LA-STATUS.
01132
01133      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.
01134
01135      IF  W-THIS-IS-FIRST-FORM
01136          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01137              PI-6942-ALIGNMENT-COPIES TIMES
01138          MOVE 'Y'                TO W-FIRST-FORM-SW.
01139
01140      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01141          W-COPIES TIMES.
01142
01143      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.
01144
01145      GO TO 3999-EXIT.
01146
01147  3400-EXIT.
01148       EXIT.
01149                                  EJECT
01150  3500-BY-CONTROL-ENTRY.
01151
01152      IF  PI-6942-PRINT-PROCESSOR GREATER THAN SPACES
01153              AND
01154          PI-6942-PRINT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD
01155          GO TO 3999-EXIT.
01156
01157      IF  LA-PRINT-ONLY-WHEN-PROC-GIVEN
01158              AND
01159          PI-6942-PRINT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD
01160          GO TO 3999-EXIT.
01161
01162      IF  PI-6942-LETTER-FORM GREATER THAN SPACES
01163              AND
01164          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3
01165          GO TO 3999-EXIT.
01166
01167      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN
01168              AND
01169          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3
01170          GO TO 3999-EXIT.
01171
01172      IF  NOT W-PROCESS-BY-KEY
01173          GO TO 3500-CONTINUE.
01174
01175      IF  PI-6942-PRINT-BY-KEY
01176          IF  PI-6942-PRINT-BY-CARRIER
01177              IF  LA-CARRIER-A3 IS EQUAL PI-6942-PRINT-CARRIER
01178                  NEXT SENTENCE
01179              ELSE
01180                  GO TO 3999-EXIT
01181          ELSE
01182              IF  PI-6942-PRINT-BY-GROUPING
01183                  IF  LA-CARRIER-A3 IS EQUAL
01184                          PI-6942-PRINT-CARRIER
01185                          AND
01186                      LA-GROUPING-A3 IS EQUAL
01187                          PI-6942-PRINT-GROUPING
01188                      NEXT SENTENCE
01189                  ELSE
01190                      GO TO 3999-EXIT
01191              ELSE
01192                  IF  PI-6942-PRINT-BY-STATE
01193                      IF  LA-CARRIER-A3 IS EQUAL
01194                              PI-6942-PRINT-CARRIER
01195                              AND
01196                          LA-GROUPING-A3 IS EQUAL
01197                              PI-6942-PRINT-GROUPING
01198                              AND
01199                          LA-STATE-A3 IS EQUAL
01200                              PI-6942-PRINT-STATE
01201                          NEXT SENTENCE
01202                      ELSE
01203                          GO TO 3999-EXIT
01204                  ELSE
01205                      IF  PI-6942-PRINT-BY-ACCOUNT
01206                          IF  LA-CARRIER-A3 IS EQUAL
01207                                  PI-6942-PRINT-CARRIER
01208                                  AND
01209                              LA-GROUPING-A3 IS EQUAL
01210                                  PI-6942-PRINT-GROUPING
01211                                  AND
01212                              LA-STATE-A3 IS EQUAL
01213                                  PI-6942-PRINT-STATE
01214                                  AND
01215                              LA-ACCOUNT-A3 IS EQUAL
01216                                  PI-6942-PRINT-ACCOUNT
01217                              NEXT SENTENCE
01218                          ELSE
01219                              GO TO 3999-EXIT.
01220
01221  3500-CONTINUE.
01222
01223      MOVE LA-NO-OF-COPIES        TO W-COPIES.
01224
01225      MOVE W-CURRENT-SAVE         TO LA-INITIAL-PRINT-DATE.
01226
01227      IF  LA-RESEND-DATE = LOW-VALUES
01228          MOVE 'C'                TO LA-STATUS.
01229
01230      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.
01231
01232      IF  W-THIS-IS-FIRST-FORM
01233          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01234              PI-6942-ALIGNMENT-COPIES TIMES
01235          MOVE 'Y'                TO W-FIRST-FORM-SW.
01236
01237      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01238          W-COPIES TIMES.
01239
01240      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.
01241
01242      GO TO 3999-EXIT.
01243
01244  3500-EXIT.
01245       EXIT.
01246                                  EJECT
01247  3900-CHECK-FOR-ACTIVE-RESENDS.
01248
01249      IF LA-RESEND-DATE > LA-LAST-RESENT-PRINT-DATE
01251         MOVE LA-ARCHIVE-NO       TO W-SAVE-ARCH-NO
           END-IF
           .
01253  3900-EXIT.
01254       EXIT.
01255  3999-EXIT.
01256       EXIT.
01257                                  EJECT
01258  4000-FOLLOW-UP-CHECKS.
01259
01260      IF  LA-INITIAL-PRINT-DATE = LOW-VALUES
01261          IF  W-SAVE-ARCH-NO = ZEROS
01262                  OR
01263              LA-ARCHIVE-NO LESS THAN W-SAVE-ARCH-NO
01264              MOVE LA-ARCHIVE-NO  TO W-SAVE-ARCH-NO
01265              GO TO 4199-EXIT
01266          ELSE
01267              GO TO 4199-EXIT.
01268
01269      IF  LA-RESEND-DATE = LOW-VALUES
01270          GO TO 4199-EXIT.
01271
01272      MOVE LOW-VALUES             TO W-WORKING-RESEND-DATE
01273                                     W-LAST-RESENT-PRINT-DATE.
01274
01275      IF  PI-6942-PRINT-DATE-BIN NOT = LOW-VALUES
01276          PERFORM 4600-GET-APP-RSND-DT-DT-GIVEN THRU 4600-EXIT
01283      ELSE
01284          PERFORM 4500-GET-APP-RSND-DT-CURR-DT THRU 4500-EXIT
           END-IF
01291
01292      IF  W-WORKING-RESEND-DATE EQUAL LOW-VALUES
01293          IF  W-SAVE-ARCH-NO EQUAL ZEROS
01294              GO TO 4100-CHECK-ACTIVITY
01295          ELSE
01296              GO TO 4199-EXIT
01297      ELSE
01298          IF  W-SAVE-ARCH-NO EQUAL ZEROS
01299              MOVE LA-ARCHIVE-NO  TO W-SAVE-ARCH-NO.
01300
01301      GO TO 4010-BY-PROCESSOR
01302            4020-BY-LETTER
01303            4030-BY-KEY
01304            4040-BY-ARCHIVE
01305            4050-BY-CONTROL-ENTRY
01306                                  DEPENDING ON W-PROCESSING-SW.
01307      GO TO 4199-EXIT.
01308                                  EJECT
01309  4010-BY-PROCESSOR.
01310
01311      IF  LA-PRINT-ONLY-WHEN-CNTL-GIVEN
01312          GO TO 4199-EXIT.
01313
01314      IF  PI-6942-LETTER-FORM GREATER THAN SPACES
01315              AND
01316          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3
01317          GO TO 4199-EXIT.
01318
01319      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN
01320              AND
01321          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3
01322          GO TO 4199-EXIT.
01323
01324      IF  NOT PI-6942-PRINT-BY-KEY
01325          GO TO 4010-CONTINUE.
01326
01327      IF  LA-CARRIER-A4 LESS THAN PI-6942-PRINT-CARRIER
01328          MOVE 'Y'                TO W-ENDBR-SW
01329          GO TO 4199-EXIT.
01330
01331      IF  PI-6942-PRINT-BY-CARRIER
01332          GO TO 4010-CONTINUE.
01333
01334      IF  LA-GROUPING-A4 LESS THAN PI-6942-PRINT-GROUPING
01335          MOVE 'Y'                TO W-ENDBR-SW
01336          GO TO 4199-EXIT.
01337
01338      IF  PI-6942-PRINT-BY-GROUPING
01339          GO TO 4010-CONTINUE.
01340
01341      IF  LA-STATE-A4 LESS THAN PI-6942-PRINT-STATE
01342          MOVE 'Y'                TO W-ENDBR-SW
01343          GO TO 4199-EXIT.
01344
01345      IF  PI-6942-PRINT-BY-STATE
01346          GO TO 4010-CONTINUE.
01347
01348      IF  LA-ACCOUNT-A4 LESS THAN PI-6942-PRINT-ACCOUNT
01349          MOVE 'Y'                TO W-ENDBR-SW
01350          GO TO 4199-EXIT.
01351
01352  4010-CONTINUE.
01353
01354      MOVE LA-NO-OF-COPIES        TO W-COPIES.
01355
01356      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.
01357
01358      IF  W-THIS-IS-FIRST-FORM
01359          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01360              PI-6942-ALIGNMENT-COPIES TIMES
01361          MOVE 'Y'                TO W-FIRST-FORM-SW.
01362
01363      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01364          W-COPIES TIMES.
01365
01366      MOVE W-LAST-RESENT-PRINT-DATE
01367                                  TO LA-LAST-RESENT-PRINT-DATE
01368                                     LA-SENT-DATE
01369
01371      MOVE 'C'                TO LA-STATUS
01372
01373      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.
01374      GO TO 4199-EXIT.
01375
01376  4010-EXIT.
01377       EXIT.
01378                                  EJECT
01379  4020-BY-LETTER.
01380
01381      IF  LA-PRINT-ONLY-WHEN-CNTL-GIVEN
01382              OR
01383          LA-PRINT-ONLY-WHEN-PROC-GIVEN
01384          GO TO 4199-EXIT.
01385
01386      IF  NOT PI-6942-PRINT-BY-KEY
01387          GO TO 4020-CONTINUE.
01388
01389      IF  LA-CARRIER-A3 LESS THAN PI-6942-PRINT-CARRIER
01390          MOVE 'Y'                TO W-ENDBR-SW
01391          GO TO 4199-EXIT.
01392
01393      IF  PI-6942-PRINT-BY-CARRIER
01394          GO TO 4020-CONTINUE.
01395
01396      IF  LA-GROUPING-A3 LESS THAN PI-6942-PRINT-GROUPING
01397          MOVE 'Y'                TO W-ENDBR-SW
01398          GO TO 4199-EXIT.
01399
01400      IF  PI-6942-PRINT-BY-GROUPING
01401          GO TO 4020-CONTINUE.
01402
01403      IF  LA-STATE-A3 LESS THAN PI-6942-PRINT-STATE
01404          MOVE 'Y'                TO W-ENDBR-SW
01405          GO TO 4199-EXIT.
01406
01407      IF  PI-6942-PRINT-BY-STATE
01408          GO TO 4020-CONTINUE.
01409
01410      IF  LA-ACCOUNT-A3 LESS THAN PI-6942-PRINT-ACCOUNT
01411          MOVE 'Y'                TO W-ENDBR-SW
01412          GO TO 4199-EXIT.
01413
01414  4020-CONTINUE.
01415
01416      MOVE LA-NO-OF-COPIES        TO W-COPIES.
01417
01418      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.
01419
01420      IF  W-THIS-IS-FIRST-FORM
01421          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01422              PI-6942-ALIGNMENT-COPIES TIMES
01423          MOVE 'Y'                TO W-FIRST-FORM-SW.
01424
01425      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01426          W-COPIES TIMES.
01427
01428      MOVE W-LAST-RESENT-PRINT-DATE
01429                                  TO LA-LAST-RESENT-PRINT-DATE
01430                                     LA-SENT-DATE
01431
01433          MOVE 'C'                TO LA-STATUS.
01434
01435      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.
01436      GO TO 4199-EXIT.
01437
01438  4020-EXIT.
01439       EXIT.
01440                                  EJECT
01441  4030-BY-KEY.
01442
01443      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN
01444              OR
01445          LA-PRINT-ONLY-WHEN-CNTL-GIVEN
01446              OR
01447          LA-PRINT-ONLY-WHEN-PROC-GIVEN
01448          GO TO 4199-EXIT.
01449
01450      IF  LA-CARRIER-A5 LESS THAN PI-6942-PRINT-CARRIER
01451          MOVE 'Y'                TO W-ENDBR-SW
01452          GO TO 4199-EXIT.
01453
01454      IF  PI-6942-PRINT-BY-CARRIER
01455          GO TO 4030-CONTINUE.
01456
01457      IF  LA-GROUPING-A5 LESS THAN PI-6942-PRINT-GROUPING
01458          MOVE 'Y'                TO W-ENDBR-SW
01459          GO TO 4199-EXIT.
01460
01461      IF  PI-6942-PRINT-BY-GROUPING
01462          GO TO 4030-CONTINUE.
01463
01464      IF  LA-STATE-A5 LESS THAN PI-6942-PRINT-STATE
01465          MOVE 'Y'                TO W-ENDBR-SW
01466          GO TO 4199-EXIT.
01467
01468      IF  PI-6942-PRINT-BY-STATE
01469          GO TO 4030-CONTINUE.
01470
01471      IF  LA-ACCOUNT-A5 LESS THAN PI-6942-PRINT-ACCOUNT
01472          MOVE 'Y'                TO W-ENDBR-SW
01473          GO TO 4199-EXIT.
01474
01475  4030-CONTINUE.
01476
01477      MOVE LA-NO-OF-COPIES        TO W-COPIES.
01478
01479      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.
01480
01481      IF  W-THIS-IS-FIRST-FORM
01482          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01483              PI-6942-ALIGNMENT-COPIES TIMES
01484          MOVE 'Y'                TO W-FIRST-FORM-SW.
01485
01486      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01487          W-COPIES TIMES.
01488
01489      MOVE W-LAST-RESENT-PRINT-DATE
01490                                  TO LA-LAST-RESENT-PRINT-DATE
01491                                     LA-SENT-DATE
01492
01494          MOVE 'C'                TO LA-STATUS.
01495
01496      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.
01497      GO TO 4199-EXIT.
01498
01499  4030-EXIT.
01500       EXIT.
01501                                  EJECT
01502  4040-BY-ARCHIVE.
01503
01504      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN
01505              OR
01506          LA-PRINT-ONLY-WHEN-CNTL-GIVEN
01507              OR
01508          LA-PRINT-ONLY-WHEN-PROC-GIVEN
01509          GO TO 4199-EXIT.
01510
01511      MOVE LA-NO-OF-COPIES        TO W-COPIES.
01512
01513      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.
01514
01515      IF  W-THIS-IS-FIRST-FORM
01516          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01517              PI-6942-ALIGNMENT-COPIES TIMES
01518          MOVE 'Y'                TO W-FIRST-FORM-SW.
01519
01520      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01521          W-COPIES TIMES.
01522
01523      MOVE W-LAST-RESENT-PRINT-DATE
01524                                  TO LA-LAST-RESENT-PRINT-DATE
01525                                     LA-SENT-DATE
01526
01528          MOVE 'C'                TO LA-STATUS.
01529
01530      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.
01531      GO TO 4199-EXIT.
01532
01533  4040-EXIT.
01534       EXIT.
01535                                  EJECT
01536  4050-BY-CONTROL-ENTRY.
01537
01538      IF  PI-6942-PRINT-PROCESSOR GREATER THAN SPACES
01539              AND
01540          PI-6942-PRINT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD
01541          GO TO 4199-EXIT.
01542
01543      IF  LA-PRINT-ONLY-WHEN-PROC-GIVEN
01544              AND
01545          PI-6942-PRINT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD
01546          GO TO 4199-EXIT.
01547
01548      IF  PI-6942-LETTER-FORM GREATER THAN SPACES
01549              AND
01550          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3
01551          GO TO 4199-EXIT.
01552
01553      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN
01554              AND
01555          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3
01556          GO TO 3999-EXIT.
01557
01558      IF  W-PROCESS-BY-KEY
01559          GO TO 4050-CONTINUE.
01560
01561      IF  PI-6942-PRINT-BY-KEY
01562          IF  PI-6942-PRINT-BY-CARRIER
01563              IF  LA-CARRIER-A3 IS EQUAL PI-6942-PRINT-CARRIER
01564                  NEXT SENTENCE
01565              ELSE
01566                  GO TO 4199-EXIT
01567          ELSE
01568              IF  PI-6942-PRINT-BY-GROUPING
01569                  IF  LA-CARRIER-A3 IS EQUAL
01570                          PI-6942-PRINT-CARRIER
01571                          AND
01572                      LA-GROUPING-A3 IS EQUAL
01573                          PI-6942-PRINT-GROUPING
01574                      NEXT SENTENCE
01575                  ELSE
01576                      GO TO 4199-EXIT
01577              ELSE
01578                  IF  PI-6942-PRINT-BY-STATE
01579                      IF  LA-CARRIER-A3 IS EQUAL
01580                              PI-6942-PRINT-CARRIER
01581                              AND
01582                          LA-GROUPING-A3 IS EQUAL
01583                              PI-6942-PRINT-GROUPING
01584                              AND
01585                          LA-STATE-A3 IS EQUAL
01586                              PI-6942-PRINT-STATE
01587                          NEXT SENTENCE
01588                      ELSE
01589                          GO TO 4199-EXIT
01590                  ELSE
01591                      IF  PI-6942-PRINT-BY-ACCOUNT
01592                          IF  LA-CARRIER-A3 IS EQUAL
01593                                  PI-6942-PRINT-CARRIER
01594                                  AND
01595                              LA-GROUPING-A3 IS EQUAL
01596                                  PI-6942-PRINT-GROUPING
01597                                  AND
01598                              LA-STATE-A3 IS EQUAL
01599                                  PI-6942-PRINT-STATE
01600                                  AND
01601                              LA-ACCOUNT-A3 IS EQUAL
01602                                  PI-6942-PRINT-ACCOUNT
01603                              NEXT SENTENCE
01604                          ELSE
01605                              GO TO 4199-EXIT.
01606
01607  4050-CONTINUE.
01608
01609      MOVE LA-NO-OF-COPIES        TO W-COPIES.
01610
01611      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.
01612
01613      IF  W-THIS-IS-FIRST-FORM
01614          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01615              PI-6942-ALIGNMENT-COPIES TIMES
01616          MOVE 'Y'                TO W-FIRST-FORM-SW.
01617
01618      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01619          W-COPIES TIMES.
01620
01622      MOVE W-LAST-RESENT-PRINT-DATE
01623                                  TO LA-LAST-RESENT-PRINT-DATE
01624                                     LA-SENT-DATE
01625
01627          MOVE 'C'                TO LA-STATUS.
01628
01629      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.
01630      GO TO 4199-EXIT.
01631
01632  4050-EXIT.
01633       EXIT.
01634                                  EJECT
01635  4100-CHECK-ACTIVITY.
01636
01637      PERFORM 3900-CHECK-FOR-ACTIVE-RESENDS THRU 3900-EXIT
           .
01647  4199-EXIT.
01648      EXIT.
01649
01650  4500-GET-APP-RSND-DT-CURR-DT.
01651
01652      IF LA-RESEND-DATE = LOW-VALUE
01653          GO TO 4500-EXIT.
01654
01655      IF (LA-RESEND-DATE NOT > W-CURRENT-SAVE)
01657         AND (LA-RESEND-DATE > LA-LAST-RESENT-PRINT-DATE)
01661          MOVE W-CURRENT-SAVE     TO W-LAST-RESENT-PRINT-DATE
01662          MOVE LA-RESEND-DATE     TO W-WORKING-RESEND-DATE.
01664
01665  4500-EXIT.
01666       EXIT.
01667
01668  4600-GET-APP-RSND-DT-DT-GIVEN.
01669
01670      IF  LA-RESEND-DATE EQUAL LOW-VALUE
01671          GO TO 4600-EXIT.
01672
01673      IF  (LA-RESEND-DATE = PI-6942-PRINT-DATE-BIN)
01675          AND (LA-RESEND-DATE > LA-LAST-RESENT-PRINT-DATE)
01679          MOVE PI-6942-PRINT-DATE-BIN
01680                                  TO W-LAST-RESENT-PRINT-DATE
01681          MOVE LA-RESEND-DATE     TO W-WORKING-RESEND-DATE.
01683
01684  4600-EXIT.
01685       EXIT.
01686                                  EJECT
01687  6000-REPRINT-CHECKS.
01688
01689      IF  PI-6942-LETTER-TYPE = 'I'
01690          IF  LA-LAST-RESENT-PRINT-DATE GREATER THAN LOW-VALUES
01691                  OR
01692              LA-INITIAL-PRINT-DATE EQUAL LOW-VALUES
01693              GO TO 6299-EXIT
01694          ELSE
01695              IF  LA-INITIAL-PRINT-DATE NOT EQUAL
01696                      PI-6942-PRINT-DATE-BIN
01697                  GO TO 6299-EXIT
01698              ELSE
01699                  NEXT SENTENCE
01700      ELSE
01701          IF  PI-6942-LETTER-TYPE = 'R'
01702              IF  LA-LAST-RESENT-PRINT-DATE = LOW-VALUES
01703                  GO TO 6299-EXIT
01704              ELSE
01705                  IF  LA-LAST-RESENT-PRINT-DATE NOT EQUAL
01706                          PI-6942-PRINT-DATE-BIN
01707                      GO TO 6299-EXIT
01708                  ELSE
01709                      NEXT SENTENCE
01710          ELSE
01711              IF  LA-LAST-RESENT-PRINT-DATE = LOW-VALUES
01712                  IF  LA-INITIAL-PRINT-DATE EQUAL LOW-VALUES
01713                      GO TO 6299-EXIT
01714                  ELSE
01715                      IF  LA-INITIAL-PRINT-DATE NOT EQUAL
01716                              PI-6942-PRINT-DATE-BIN
01717                          GO TO 6299-EXIT
01718              ELSE
01719                  IF  LA-LAST-RESENT-PRINT-DATE NOT EQUAL
01720                          PI-6942-PRINT-DATE-BIN
01721                      GO TO 6299-EXIT.
01722
01723      GO TO 6010-BY-PROCESSOR
01724            6020-BY-LETTER
01725            6030-BY-KEY
01726            6040-BY-ARCHIVE
01727            6050-BY-CONTROL-ENTRY
01728                                  DEPENDING ON W-PROCESSING-SW.
01729      GO TO 6299-EXIT.
01730                                  EJECT
01731  6010-BY-PROCESSOR.
01732
01733      IF  LA-PRINT-ONLY-WHEN-CNTL-GIVEN
01734          GO TO 6299-EXIT.
01735
01736      IF  PI-6942-LETTER-FORM GREATER THAN SPACES
01737              AND
01738          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3
01739          GO TO 6299-EXIT.
01740
01741      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN
01742              AND
01743          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3
01744          GO TO 6299-EXIT.
01745
01746      IF  LA-CARRIER-A4 LESS THAN PI-6942-PRINT-CARRIER
01747          MOVE 'Y'                TO W-ENDBR-SW
01748          GO TO 6299-EXIT.
01749
01750      IF  PI-6942-PRINT-BY-CARRIER
01751          GO TO 6010-CONTINUE.
01752
01753      IF  LA-GROUPING-A4 LESS THAN PI-6942-PRINT-GROUPING
01754          MOVE 'Y'                TO W-ENDBR-SW
01755          GO TO 6299-EXIT.
01756
01757      IF  PI-6942-PRINT-BY-GROUPING
01758          GO TO 6010-CONTINUE.
01759
01760      IF  LA-STATE-A4 LESS THAN PI-6942-PRINT-STATE
01761          MOVE 'Y'                TO W-ENDBR-SW
01762          GO TO 6299-EXIT.
01763
01764      IF  PI-6942-PRINT-BY-STATE
01765          GO TO 6010-CONTINUE.
01766
01767      IF  LA-ACCOUNT-A4 LESS THAN PI-6942-PRINT-ACCOUNT
01768          MOVE 'Y'                TO W-ENDBR-SW
01769          GO TO 6299-EXIT.
01770
01771  6010-CONTINUE.
01772
01773      MOVE LA-NO-OF-COPIES        TO W-COPIES.
01774
01775      MOVE W-CURRENT-SAVE         TO LA-INITIAL-PRINT-DATE.
01776      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.
01777
01778      IF  W-THIS-IS-FIRST-FORM
01779          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01780              PI-6942-ALIGNMENT-COPIES TIMES
01781          MOVE 'Y'                TO W-FIRST-FORM-SW.
01782
01783      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01784          W-COPIES TIMES.
01785
01786      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.
01787
01788      GO TO 6299-EXIT.
01789
01790  6010-EXIT.
01791       EXIT.
01792                                  EJECT
01793  6020-BY-LETTER.
01794
01795      IF  LA-PRINT-ONLY-WHEN-CNTL-GIVEN
01796              OR
01797          LA-PRINT-ONLY-WHEN-PROC-GIVEN
01798          GO TO 6299-EXIT.
01799
01800      IF  NOT PI-6942-PRINT-BY-KEY
01801          GO TO 6020-CONTINUE.
01802
01803      IF  LA-CARRIER-A3 LESS THAN PI-6942-PRINT-CARRIER
01804          GO TO 6299-EXIT.
01805
01806      IF  LA-CARRIER-A3 LESS THAN PI-6942-PRINT-CARRIER
01807          MOVE 'Y'                TO W-ENDBR-SW
01808          GO TO 6299-EXIT.
01809
01810      IF  PI-6942-PRINT-BY-CARRIER
01811          GO TO 6020-CONTINUE.
01812
01813      IF  LA-GROUPING-A3 LESS THAN PI-6942-PRINT-GROUPING
01814          GO TO 6299-EXIT.
01815
01816      IF  LA-GROUPING-A3 LESS THAN PI-6942-PRINT-GROUPING
01817          MOVE 'Y'                TO W-ENDBR-SW
01818          GO TO 6299-EXIT.
01819
01820      IF  PI-6942-PRINT-BY-GROUPING
01821          GO TO 6020-CONTINUE.
01822
01823      IF  LA-STATE-A3 LESS THAN PI-6942-PRINT-STATE
01824          GO TO 6299-EXIT.
01825
01826      IF  LA-STATE-A3 LESS THAN PI-6942-PRINT-STATE
01827          MOVE 'Y'                TO W-ENDBR-SW
01828          GO TO 6299-EXIT.
01829
01830      IF  PI-6942-PRINT-BY-STATE
01831          GO TO 6020-CONTINUE.
01832
01833      IF  LA-ACCOUNT-A3 LESS THAN PI-6942-PRINT-ACCOUNT
01834          GO TO 6299-EXIT.
01835
01836      IF  LA-ACCOUNT-A3 LESS THAN PI-6942-PRINT-ACCOUNT
01837          MOVE 'Y'                TO W-ENDBR-SW
01838          GO TO 6299-EXIT.
01839
01840  6020-CONTINUE.
01841
01842      MOVE LA-NO-OF-COPIES        TO W-COPIES.
01843
01844      MOVE W-CURRENT-SAVE         TO LA-INITIAL-PRINT-DATE.
01845      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.
01846
01847      IF  W-THIS-IS-FIRST-FORM
01848          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01849              PI-6942-ALIGNMENT-COPIES TIMES
01850          MOVE 'Y'                TO W-FIRST-FORM-SW.
01851
01852      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01853          W-COPIES TIMES.
01854
01855      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.
01856
01857      GO TO 6299-EXIT.
01858
01859  6020-EXIT.
01860       EXIT.
01861                                  EJECT
01862  6030-BY-KEY.
01863
01864      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN
01865              OR
01866          LA-PRINT-ONLY-WHEN-CNTL-GIVEN
01867              OR
01868          LA-PRINT-ONLY-WHEN-PROC-GIVEN
01869          GO TO 6299-EXIT.
01870
01871      IF  LA-CARRIER-A5 LESS THAN PI-6942-PRINT-CARRIER
01872          MOVE 'Y'                TO W-ENDBR-SW
01873          GO TO 6299-EXIT.
01874
01875      IF  PI-6942-PRINT-BY-CARRIER
01876          GO TO 6030-CONTINUE.
01877
01878      IF  LA-GROUPING-A5 LESS THAN PI-6942-PRINT-GROUPING
01879          MOVE 'Y'                TO W-ENDBR-SW
01880          GO TO 6299-EXIT.
01881
01882      IF  PI-6942-PRINT-BY-GROUPING
01883          GO TO 6030-CONTINUE.
01884
01885      IF  LA-STATE-A5 LESS THAN PI-6942-PRINT-STATE
01886          MOVE 'Y'                TO W-ENDBR-SW
01887          GO TO 6299-EXIT.
01888
01889      IF  PI-6942-PRINT-BY-STATE
01890          GO TO 6030-CONTINUE.
01891
01892      IF  LA-ACCOUNT-A5 LESS THAN PI-6942-PRINT-ACCOUNT
01893          MOVE 'Y'                TO W-ENDBR-SW
01894          GO TO 6299-EXIT.
01895
01896  6030-CONTINUE.
01897
01898      MOVE LA-NO-OF-COPIES        TO W-COPIES.
01899
01900      MOVE W-CURRENT-SAVE         TO LA-INITIAL-PRINT-DATE.
01901      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.
01902
01903      IF  W-THIS-IS-FIRST-FORM
01904          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01905              PI-6942-ALIGNMENT-COPIES TIMES
01906          MOVE 'Y'                TO W-FIRST-FORM-SW.
01907
01908      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01909          W-COPIES TIMES.
01910
01911      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.
01912
01913      GO TO 6299-EXIT.
01914
01915  6030-EXIT.
01916       EXIT.
01917                                  EJECT
01918  6040-BY-ARCHIVE.
01919
01920      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN
01921              OR
01922          LA-PRINT-ONLY-WHEN-CNTL-GIVEN
01923              OR
01924          LA-PRINT-ONLY-WHEN-PROC-GIVEN
01925          GO TO 6299-EXIT.
01926
01927      MOVE LA-NO-OF-COPIES        TO W-COPIES.
01928
01929      MOVE W-CURRENT-SAVE         TO LA-INITIAL-PRINT-DATE.
01930      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.
01931
01932      IF  W-THIS-IS-FIRST-FORM
01933          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01934              PI-6942-ALIGNMENT-COPIES TIMES
01935          MOVE 'Y'                TO W-FIRST-FORM-SW.
01936
01937      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
01938          W-COPIES TIMES.
01939
01940      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.
01941
01942      GO TO 6299-EXIT.
01943
01944  6040-EXIT.
01945       EXIT.
01946                                  EJECT
01947  6050-BY-CONTROL-ENTRY.
01948
01949      IF  PI-6942-PRINT-PROCESSOR GREATER THAN SPACES
01950              AND
01951          PI-6942-PRINT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD
01952          GO TO 6299-EXIT.
01953
01954      IF  LA-PRINT-ONLY-WHEN-PROC-GIVEN
01955              AND
01956          PI-6942-PRINT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD
01957          GO TO 6299-EXIT.
01958
01959      IF  PI-6942-LETTER-FORM GREATER THAN SPACES
01960              AND
01961          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3
01962          GO TO 6299-EXIT.
01963
01964      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN
01965              AND
01966          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3
01967          GO TO 6299-EXIT.
01968
01969      IF  W-PROCESS-BY-KEY
01970          GO TO 6050-CONTINUE.
01971
01972      IF  PI-6942-PRINT-BY-KEY
01973          IF  PI-6942-PRINT-BY-CARRIER
01974              IF  LA-CARRIER-A3 IS EQUAL PI-6942-PRINT-CARRIER
01975                  NEXT SENTENCE
01976              ELSE
01977                  GO TO 6299-EXIT
01978          ELSE
01979              IF  PI-6942-PRINT-BY-GROUPING
01980                  IF  LA-CARRIER-A3 IS EQUAL
01981                          PI-6942-PRINT-CARRIER
01982                          AND
01983                      LA-GROUPING-A3 IS EQUAL
01984                          PI-6942-PRINT-GROUPING
01985                      NEXT SENTENCE
01986                  ELSE
01987                      GO TO 6299-EXIT
01988              ELSE
01989                  IF  PI-6942-PRINT-BY-STATE
01990
01991                      IF  LA-CARRIER-A3 IS EQUAL
01992                              PI-6942-PRINT-CARRIER
01993                              AND
01994                          LA-GROUPING-A3 IS EQUAL
01995                              PI-6942-PRINT-GROUPING
01996                              AND
01997                          LA-STATE-A3 IS EQUAL
01998                              PI-6942-PRINT-STATE
01999                          NEXT SENTENCE
02000                      ELSE
02001                          GO TO 6299-EXIT
02002                  ELSE
02003                      IF  PI-6942-PRINT-BY-ACCOUNT
02004                          IF  LA-CARRIER-A3 IS EQUAL
02005                                  PI-6942-PRINT-CARRIER
02006                                  AND
02007                              LA-GROUPING-A3 IS EQUAL
02008                                  PI-6942-PRINT-GROUPING
02009                                  AND
02010                              LA-STATE-A3 IS EQUAL
02011                                  PI-6942-PRINT-STATE
02012                                  AND
02013                              LA-ACCOUNT-A3 IS EQUAL
02014                                  PI-6942-PRINT-ACCOUNT
02015                              NEXT SENTENCE
02016                          ELSE
02017                              GO TO 6299-EXIT.
02018
02019  6050-CONTINUE.
02020
02021      MOVE LA-NO-OF-COPIES        TO W-COPIES.
02022
02023      MOVE W-CURRENT-SAVE         TO LA-INITIAL-PRINT-DATE.
02024      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.
02025
02026      IF  W-THIS-IS-FIRST-FORM
02027          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
02028              PI-6942-ALIGNMENT-COPIES TIMES
02029          MOVE 'Y'                TO W-FIRST-FORM-SW.
02030
02031      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT
02032          W-COPIES TIMES.
02033      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.
02034      GO TO 6299-EXIT.
02035
02036  6050-EXIT.
02037       EXIT.
02038
02039  6299-EXIT.
02040       EXIT.
02041                                  EJECT
02042  6300-LABEL-CHECKS.
02043
02044      IF  FIRST-TIME
02045          PERFORM 6400-ALIGNMENT-PRINT THRU 6450-EXIT.
02046
02047      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
02048
02049      IF  LA-INITIAL-PRINT-DATE NOT EQUAL PI-6942-PRINT-DATE-BIN
02050              AND
02051          LA-LAST-RESENT-PRINT-DATE NOT EQUAL
02052              PI-6942-PRINT-DATE-BIN
02053          GO TO 6399-EXIT.
02054
02055      IF  PI-6942-LETTER-TYPE = 'I'
02056          IF  LA-LAST-RESENT-PRINT-DATE GREATER THAN LOW-VALUES
02057              GO TO 6399-EXIT
02058          ELSE
02059              IF  LA-INITIAL-PRINT-DATE NOT EQUAL
02060                      PI-6942-PRINT-DATE-BIN
02061                  GO TO 6399-EXIT
02062              ELSE
02063                  NEXT SENTENCE
02064      ELSE
02065          IF  PI-6942-LETTER-TYPE = 'R'
02066              IF  LA-LAST-RESENT-PRINT-DATE = LOW-VALUES
02067                  GO TO 6399-EXIT
02068              ELSE
02069                  IF  LA-LAST-RESENT-PRINT-DATE NOT EQUAL
02070                          PI-6942-PRINT-DATE-BIN
02071                      GO TO 6399-EXIT.
02072
02073      GO TO 6310-BY-PROCESSOR
02074            6320-BY-LETTER
02075            6330-BY-KEY
02076            6340-BY-ARCHIVE
02077            6350-BY-CONTROL-ENTRY
02078                                  DEPENDING ON W-PROCESSING-SW.
02079      GO TO 6399-EXIT.
02080                                  EJECT
02081  6310-BY-PROCESSOR.
02082
02083      IF  LA-PRINT-ONLY-WHEN-CNTL-GIVEN
02084          GO TO 6399-EXIT.
02085
02086      IF  PI-6942-LETTER-FORM GREATER THAN SPACES
02087              AND
02088          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3
02089          GO TO 6399-EXIT.
02090
02091      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN
02092              AND
02093          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3
02094          GO TO 6399-EXIT.
02095
02096      IF  NOT PI-6942-PRINT-BY-KEY
02097          GO TO 6310-CONTINUE.
02098
02099      IF  LA-CARRIER-A5 LESS THAN PI-6942-PRINT-CARRIER
02100          MOVE 'Y'                TO W-ENDBR-SW
02101          GO TO 6399-EXIT.
02102
02103      IF  PI-6942-PRINT-BY-CARRIER
02104          GO TO 6310-CONTINUE.
02105
02106      IF  LA-GROUPING-A5 LESS THAN PI-6942-PRINT-GROUPING
02107          MOVE 'Y'                TO W-ENDBR-SW
02108          GO TO 6399-EXIT.
02109
02110      IF  PI-6942-PRINT-BY-GROUPING
02111          GO TO 6310-CONTINUE.
02112
02113      IF  LA-STATE-A5 LESS THAN PI-6942-PRINT-STATE
02114          MOVE 'Y'                TO W-ENDBR-SW
02115          GO TO 6399-EXIT.
02116
02117      IF  PI-6942-PRINT-BY-STATE
02118          GO TO 6310-CONTINUE.
02119
02120      IF  LA-ACCOUNT-A5 LESS THAN PI-6942-PRINT-ACCOUNT
02121          MOVE 'Y'                TO W-ENDBR-SW
02122          GO TO 6399-EXIT.
02123
02124  6310-CONTINUE.
02125
02126      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.
02127      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT.
02128      GO TO 6399-EXIT.
02129
02130  6310-EXIT.
02131       EXIT.
02132                                  EJECT
02133  6320-BY-LETTER.
02134
02135      IF  LA-PRINT-ONLY-WHEN-CNTL-GIVEN
02136              OR
02137          LA-PRINT-ONLY-WHEN-PROC-GIVEN
02138          GO TO 6399-EXIT.
02139
02140      IF  NOT PI-6942-PRINT-BY-KEY
02141          GO TO 6320-CONTINUE.
02142
02143      IF  LA-CARRIER-A3 LESS THAN PI-6942-PRINT-CARRIER
02144          MOVE 'Y'                TO W-ENDBR-SW
02145          GO TO 6399-EXIT.
02146
02147      IF  PI-6942-PRINT-BY-CARRIER
02148          GO TO 6320-CONTINUE.
02149
02150      IF  LA-GROUPING-A3 LESS THAN PI-6942-PRINT-GROUPING
02151          MOVE 'Y'                TO W-ENDBR-SW
02152          GO TO 6399-EXIT.
02153
02154      IF  PI-6942-PRINT-BY-GROUPING
02155          GO TO 6320-CONTINUE.
02156
02157      IF  LA-STATE-A3 LESS THAN PI-6942-PRINT-STATE
02158          MOVE 'Y'                TO W-ENDBR-SW
02159          GO TO 6399-EXIT.
02160
02161      IF  PI-6942-PRINT-BY-STATE
02162          GO TO 6320-CONTINUE.
02163
02164      IF  LA-ACCOUNT-A3 LESS THAN PI-6942-PRINT-ACCOUNT
02165          MOVE 'Y'                TO W-ENDBR-SW
02166          GO TO 6399-EXIT.
02167
02168  6320-CONTINUE.
02169
02170      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.
02171      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT.
02172      GO TO 6399-EXIT.
02173
02174  6320-EXIT.
02175       EXIT.
02176                                  EJECT
02177  6330-BY-KEY.
02178
02179      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN
02180              OR
02181          LA-PRINT-ONLY-WHEN-CNTL-GIVEN
02182              OR
02183          LA-PRINT-ONLY-WHEN-PROC-GIVEN
02184          GO TO 6399-EXIT.
02185
02186      IF  LA-CARRIER-A5 LESS THAN PI-6942-PRINT-CARRIER
02187          MOVE 'Y'                TO W-ENDBR-SW
02188          GO TO 6399-EXIT.
02189
02190      IF  PI-6942-PRINT-BY-CARRIER
02191          GO TO 6330-CONTINUE.
02192
02193      IF  LA-GROUPING-A5 LESS THAN PI-6942-PRINT-GROUPING
02194          MOVE 'Y'                TO W-ENDBR-SW
02195          GO TO 6399-EXIT.
02196
02197      IF  PI-6942-PRINT-BY-GROUPING
02198          GO TO 6330-CONTINUE.
02199
02200      IF  LA-STATE-A5 LESS THAN PI-6942-PRINT-STATE
02201          MOVE 'Y'                TO W-ENDBR-SW
02202          GO TO 6399-EXIT.
02203
02204      IF  PI-6942-PRINT-BY-STATE
02205          GO TO 6330-CONTINUE.
02206
02207      IF  LA-ACCOUNT-A5 LESS THAN PI-6942-PRINT-ACCOUNT
02208          MOVE 'Y'                TO W-ENDBR-SW
02209          GO TO 6399-EXIT.
02210
02211  6330-CONTINUE.
02212
02213      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.
02214      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT.
02215      GO TO 6399-EXIT.
02216
02217  6330-EXIT.
02218       EXIT.
02219                                  EJECT
02220  6340-BY-ARCHIVE.
02221
02222      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN
02223              OR
02224          LA-PRINT-ONLY-WHEN-CNTL-GIVEN
02225              OR
02226          LA-PRINT-ONLY-WHEN-PROC-GIVEN
02227          GO TO 6399-EXIT.
02228
02229      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.
02230      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT.
02231      GO TO 6399-EXIT.
02232
02233  6340-EXIT.
02234       EXIT.
02235                                  EJECT
02236  6350-BY-CONTROL-ENTRY.
02237
02238      IF  PI-6942-PRINT-PROCESSOR GREATER THAN SPACES
02239              AND
02240          PI-6942-PRINT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD
02241          GO TO 6399-EXIT.
02242
02243      IF  LA-PRINT-ONLY-WHEN-PROC-GIVEN
02244              AND
02245          PI-6942-PRINT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD
02246          GO TO 6399-EXIT.
02247
02248      IF  PI-6942-LETTER-FORM GREATER THAN SPACES
02249              AND
02250          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3
02251          GO TO 6399-EXIT.
02252
02253      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN
02254              AND
02255          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3
02256          GO TO 6399-EXIT.
02257
02258      IF  W-PROCESS-BY-KEY
02259          GO TO 6350-CONTINUE.
02260
02261      IF  PI-6942-PRINT-BY-KEY
02262          IF  PI-6942-PRINT-BY-CARRIER
02263              IF  LA-CARRIER-A3 IS EQUAL PI-6942-PRINT-CARRIER
02264                  NEXT SENTENCE
02265              ELSE
02266                  GO TO 6399-EXIT
02267          ELSE
02268              IF  PI-6942-PRINT-BY-GROUPING
02269                  IF  LA-CARRIER-A3 IS EQUAL
02270                          PI-6942-PRINT-CARRIER
02271                          AND
02272                      LA-GROUPING-A3 IS EQUAL
02273                          PI-6942-PRINT-GROUPING
02274                      NEXT SENTENCE
02275                  ELSE
02276                      GO TO 6399-EXIT
02277              ELSE
02278                  IF  PI-6942-PRINT-BY-STATE
02279                      IF  LA-CARRIER-A3 IS EQUAL
02280                              PI-6942-PRINT-CARRIER
02281                              AND
02282                          LA-GROUPING-A3 IS EQUAL
02283                              PI-6942-PRINT-GROUPING
02284                              AND
02285                          LA-STATE-A3 IS EQUAL
02286                              PI-6942-PRINT-STATE
02287                          NEXT SENTENCE
02288                      ELSE
02289                          GO TO 6399-EXIT
02290                  ELSE
02291                      IF  PI-6942-PRINT-BY-ACCOUNT
02292                          IF  LA-CARRIER-A3 IS EQUAL
02293                                  PI-6942-PRINT-CARRIER
02294                                  AND
02295                              LA-GROUPING-A3 IS EQUAL
02296                                  PI-6942-PRINT-GROUPING
02297                                  AND
02298                              LA-STATE-A3 IS EQUAL
02299                                  PI-6942-PRINT-STATE
02300                                  AND
02301                              LA-ACCOUNT-A3 IS EQUAL
02302                                  PI-6942-PRINT-ACCOUNT
02303                              NEXT SENTENCE
02304                          ELSE
02305                              GO TO 6399-EXIT.
02306
02307  6350-CONTINUE.
02308
02309      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.
02310      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT.
02311      GO TO 6399-EXIT.
02312
02313  6350-EXIT.
02314       EXIT.
02315
02316  6399-EXIT.
02317       EXIT.
02318                                  EJECT
02319  6400-ALIGNMENT-PRINT.
02320
02321      MOVE ALL '*'                TO W-LABEL-LINES (1)
02322                                     W-LABEL-LINES (2)
02323                                     W-LABEL-LINES (3).
02324
02325      MOVE SPACES                 TO W-LABEL-LINES (4)
02326                                     W-LABEL-LINES (5)
02327                                     W-LABEL-LINES (6).
02328
02329      IF  PI-6942-ALIGNMENT-COPIES GREATER THAN +0
02330          PERFORM 6480-MOVE-TO-PRINT THRU 6499-EXIT
02331              PI-6942-ALIGNMENT-COPIES TIMES
02332      ELSE
02333          PERFORM 6480-MOVE-TO-PRINT THRU 6499-EXIT 6 TIMES.
02334
02335  6450-EXIT.
02336       EXIT.
02337
02338  6480-MOVE-TO-PRINT.
02339
02340      MOVE SPACES                 TO WS-PASSED-CNTL-CHAR.
02341      MOVE W-LABEL-LINES (1)      TO WS-PASSED-DATA.
02342      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
02343      MOVE W-LABEL-LINES (2)      TO WS-PASSED-DATA.
02344      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
02345      MOVE W-LABEL-LINES (3)      TO WS-PASSED-DATA.
02346      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
02347      MOVE W-LABEL-LINES (4)      TO WS-PASSED-DATA.
02348      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
02349      MOVE W-LABEL-LINES (5)      TO WS-PASSED-DATA.
02350      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
02351      MOVE W-LABEL-LINES (6)      TO WS-PASSED-DATA.
02352      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
02353
02354  6499-EXIT.
02355       EXIT.
02356                                  EJECT
02357  7100-CREATE-PRINT-TABLES.
02358
02359      MOVE ZEROS                  TO W-NUM-OF-TEXT-RECORDS.
02360      MOVE LOW-VALUES             TO W-ARCT-KEY.
02361      MOVE PI-COMPANY-CD          TO W-ARCT-COMPANY-CD.
02362      MOVE SPACES                 TO WS-PROG-END
02363      MOVE SPACES                 TO W-WORK-TABLE.
02364      SET W-WK-NDX                TO W-ZEROS.
02365      MOVE LA-ARCHIVE-NO          TO W-ARCT-NUMBER.
02366      MOVE +1                     TO W-ARCT-SEQ.
02367
02368      IF  W-PRINT-LABELS
02369          MOVE +0                 TO W-ARCT-SEQ
02370          MOVE '1'                TO W-ARCT-REC-ID
02371          SET W-L-NDX             TO 1
02372      ELSE
02373          MOVE '2'                TO W-ARCT-REC-ID.
02374
02375      
      * EXEC CICS HANDLE CONDITION
02376 *         NOTFND  (7100-EXIT)
02377 *         ENDFILE (7100-EXIT)
02378 *         NOTOPEN (8890-ARCT-NOT-OPEN)
02379 *    END-EXEC.
      *    MOVE '"$I''J                 ! * #00004630' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303034363330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02380
02381      IF  W-PRINT-LABELS
02382           PERFORM 7110-READ-NEXT THRU 7110-EXIT
02383      ELSE
02384           ADD +1 TO W-LETTER-TOTALS
02385           PERFORM 7110-READ-NEXT THRU 7110-EXIT
02386             LA-NO-OF-TEXT-RECORDS TIMES.
02387
02388  7100-EXIT.
02389      EXIT.
02390
02391  7110-READ-NEXT.
02392
02393      
      * EXEC CICS READ
02394 *         DATASET (W-ARCT-ID)
02395 *         RIDFLD  (W-ARCT-KEY)
02396 *         SET     (ADDRESS OF LETTER-ARCHIVE-TEXT)
02397 *    END-EXEC.
      *    MOVE '&"S        E          (   #00004648' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE-TEXT TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02398
02399      IF  W-PRINT-LABELS
02400          IF (LT-LETTER-TEXT (1) EQUAL SPACES OR LOW-VALUES)  AND
02401             (LT-LETTER-TEXT (2) EQUAL SPACES OR LOW-VALUES)  AND
02402             (LT-LETTER-TEXT (3) EQUAL SPACES OR LOW-VALUES)  AND
02403             (LT-LETTER-TEXT (4) EQUAL SPACES OR LOW-VALUES)  AND
02404             (LT-LETTER-TEXT (5) EQUAL SPACES OR LOW-VALUES)  AND
02405             (LT-LETTER-TEXT (6) EQUAL SPACES OR LOW-VALUES)
02406              GO TO 7110-EXIT
02407          ELSE
02408              ADD +1 TO W-LETTER-TOTALS
02409              MOVE LT-LETTER-TEXT (1) TO W-LABEL-LINES (1)
02410              MOVE LT-LETTER-TEXT (2) TO W-LABEL-LINES (2)
02411              MOVE LT-LETTER-TEXT (3) TO W-LABEL-LINES (3)
02412              MOVE LT-LETTER-TEXT (4) TO W-LABEL-LINES (4)
02413              MOVE LT-LETTER-TEXT (5) TO W-LABEL-LINES (5)
02414              MOVE LT-LETTER-TEXT (6) TO W-LABEL-LINES (6)
02415              PERFORM 6480-MOVE-TO-PRINT THRU 6499-EXIT
02416              GO TO 7110-EXIT.
02417
02418      PERFORM 7120-PROCESS-RECORD THRU 7120-EXIT
02419              VARYING
02420          LT-NDX FROM +1 BY +1
02421              UNTIL
02422          LT-NDX GREATER THAN LT-NUM-LINES-ON-RECORD.
02423
02424      ADD +1                      TO W-ARCT-SEQ.
02425
02426  7110-EXIT.
02427      EXIT.
02428                                  EJECT
02429  7120-PROCESS-RECORD.
02430
02431      SET W-WK-NDX UP BY +1.
02432      ADD +1                      TO W-NUM-OF-TEXT-RECORDS.
02433      MOVE LT-LETTER-TEXT (LT-NDX)
02434                                  TO W-WORK-LINE (W-WK-NDX).
02435
02436  7120-EXIT.
02437      EXIT.
02438                                  EJECT
02439  7200-PRINT-ARCHIVE-RECORDS.
02440
02441      PERFORM 7220-PROCESS-TABLE THRU 7220-EXIT
02442              VARYING
02443          W-WK-NDX FROM 1 BY 1
02444              UNTIL
02445          W-WK-NDX GREATER THAN W-NUM-OF-TEXT-RECORDS.
02446
02447  7200-EXIT.
02448      EXIT.
02449                                  EJECT
02450  7220-PROCESS-TABLE.
02451
02452      IF  W-SKIP-CONTROL (W-WK-NDX) GREATER THAN '00'
02453              AND
02454          W-SKIP-CONTROL (W-WK-NDX) LESS THAN '99'
02455          MOVE SPACES             TO WS-PRINT-AREA
02456          MOVE W-SKIP-CONTROL (W-WK-NDX)
02457                                  TO W-SKIP
02458          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT W-SKIP TIMES
02459      ELSE
02460          IF  W-SKIP-TO-NEXT-PAGE (W-WK-NDX)
02461              IF  W-TEXT-LINE (W-WK-NDX) EQUAL SPACES
02462                  MOVE '1'        TO WS-PRINT-AREA
02463              ELSE
02464                  MOVE '1'        TO WS-PRINT-AREA
02465                  PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
02466                  MOVE SPACES     TO WS-PRINT-AREA
02467          ELSE
02468              MOVE SPACES         TO WS-PRINT-AREA.
031011     IF W-TEXT-LINE (W-WK-NDX) (1:6) = '&&&&&&'
031011        CONTINUE
031011     ELSE
031011        MOVE W-TEXT-LINE (W-WK-NDX) TO W-AD-PRINT-AREA
031011        MOVE W-ADJUST-AREA          TO WS-PASSED-DATA
031011        PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
031011     END-IF
           .
02475  7220-EXIT.
02476      EXIT.
02477                                 EJECT
02478  7300-UPDATE-ARCHIVE-HEADER.
02479
02480      
      * EXEC CICS HANDLE CONDITION
02481 *        DUPKEY (7399-EXIT)
02482 *    END-EXEC.
      *    MOVE '"$$                   ! + #00004737' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303034373337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02483
02484      
      * EXEC CICS READ
02485 *         DATASET (W-ARCH-ID)
02486 *         RIDFLD  (LA-CONTROL-PRIMARY)
02487 *         SET     (ADDRESS OF L-LETTER-ARCHIVE)
02488 *         UPDATE
02489 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004741' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 LA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF L-LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02490
02491      
      * EXEC CICS REWRITE
02492 *        DATASET (W-ARCH-ID)
02493 *        FROM    (LETTER-ARCHIVE)
02494 *    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004748' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02495
02496  7399-EXIT.
02497      EXIT.
02498                                  EJECT
02499  8800-ARCH4-NOT-OPEN.
02500
02501      MOVE 'LETTER ARCHIVE FILE NOT OPEN - ERARCH4'
02502                                  TO W-ERROR-LINE.
02503      PERFORM 0400-SEND-TEXT.
02504      GO TO 0200-END-DATA.
02505                                  EJECT
02506  8810-ARCH3-NOT-OPEN.
02507
02508      MOVE 'LETTER ARCHIVE FILE NOT OPEN - ERARCH3'
02509                                  TO W-ERROR-LINE.
02510      PERFORM 0400-SEND-TEXT.
02511      GO TO 0200-END-DATA.
02512                                  EJECT
02513  8860-ARCH5-NOT-OPEN.
02514
02515      MOVE 'LETTER ARCHIVE FILE NOT OPEN - ERARCH5'
02516                                  TO W-ERROR-LINE.
02517      PERFORM 0400-SEND-TEXT.
02518      GO TO 0200-END-DATA.
02519
02520  8870-ARCH-NOT-OPEN.
02521
02522      MOVE 'LETTER ARCHIVE FILE NOT OPEN - ERARCH'
02523                                  TO W-ERROR-LINE.
02524      PERFORM 0400-SEND-TEXT.
02525      GO TO 0200-END-DATA.
02526
02527  8890-ARCT-NOT-OPEN.
02528
02529      MOVE 'LETTER ARCHIVE TEXT FILE NOT OPEN -ERARCTT'
02530                                  TO W-ERROR-LINE.
02531      PERFORM 0400-SEND-TEXT.
02532      GO TO 0200-END-DATA.
02533                                  EJECT
02534  9700-DATE-LINK.
02535
02536      
      * EXEC CICS LINK
02537 *         PROGRAM  ('ELDATCV')
02538 *         COMMAREA (DATE-CONVERSION-DATA)
02539 *         LENGTH   (DC-COMM-LENGTH)
02540 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00004793' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02541
02542  9700-EXIT.
02543       EXIT.
02544                                  EJECT
uktdel*9800-PRINT-ROUTINE.             COPY ELPRTCVP.
uktins 9800-PRINT-ROUTINE.
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
      *    MOVE '."C                   ''   #00004860' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383630' TO DFHEIV0(25:11)
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
      *    MOVE '$$    C E         L F ,   #00004981' TO DFHEIV0
           MOVE X'242420202020432045202020' TO DFHEIV0(1:12)
           MOVE X'2020202020204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393831' TO DFHEIV0(25:11)
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
02546
02547  9999-GOBACK.
02548
02549      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6942' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
02550
02551  9999-EXIT.
02552      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6942' TO DFHEIV1
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
               GO TO 0200-END-DATA,
                     0200-END-DATA
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8800-ARCH4-NOT-OPEN,
                     2999-EXIT,
                     2999-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8810-ARCH3-NOT-OPEN,
                     2999-EXIT,
                     2999-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8860-ARCH5-NOT-OPEN,
                     2999-EXIT,
                     2999-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8870-ARCH-NOT-OPEN,
                     2999-EXIT,
                     2999-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8860-ARCH5-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 2999-EXIT,
                     2999-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 7100-EXIT,
                     7100-EXIT,
                     8890-ARCT-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 7399-EXIT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6942' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
