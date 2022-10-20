00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL6411.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 09:56:12.
00007 *                            VMOD=2.007.
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
00024 *REMARKS. TRANSACTION EXC9 - BILLING STATEMENT PRINT.
00025 *        THIS PROGRAM IS USED TO PRINT THE STORED STATEMENTS AND
00026 *        LABELS  DEPENDING ON THE VALUE OF THE PI-ENTRY-CODES.
00027
00028 *        PRINT INITIAL STATEMENTS CODE-1 = 1
00029 *                                 CODE-2 = 1
00030
00031 *        RE-PRINT STATEMENTS      CODE-1 = 0
00032 *                                 CODE-2 = 3
00033
00034 *        PRINT ADDRESS LABELS     CODE-1 = 0
00035 *                                 CODE-2 = 2
00036
00037 *        PRINT SINGLE STATEMENT   CODE-1 = 0
00038 *                                 CODE-2 = 4
00039      EJECT
00040  ENVIRONMENT DIVISION.
00041  DATA DIVISION.
00042  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00043  77  FILLER  PIC X(32)  VALUE '********************************'.
00044  77  FILLER  PIC X(32)  VALUE '*   EL6411 WORKING STORAGE     *'.
00045  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.007 ************'.
00046
00047 *                                COPY ELCDMD34.
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
00048 *                                COPY ELCSCTM.
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
00049 *                                COPY ELCSCRTY.
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
00050
00051     EJECT
00052  01  REMOTE-PRINTER-CTRL.
00053      12  CTRL-BUFFER-LENGTH      PIC S9(04) COMP.
00054      12  CTRL-BUFFER-AREA        PIC X(20).
00055      SKIP1
00056  01  WS-CONSTANTS.
00057      12  SC-ITEM                 PIC S9(4) COMP VALUE +1.
00058      12  TIME-IN                 PIC S9(7).
00059      12  FILLER REDEFINES TIME-IN.
00060         16  FILLER               PIC X.
00061         16  TIME-OUT             PIC 99V99.
00062         16  FILLER               PIC XX.
00063      12  THIS-PGM                PIC X(8)    VALUE 'EL6411'.
00064      12  PGM-EL6401              PIC X(8)    VALUE 'EL6401'.
00065      12  PGM-NAME                PIC X(8).
00066      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.
00067      12  BILL-ID                 PIC X(8)    VALUE 'ERBILL'.
00068      12  BILL-KEY.
00069          16  BILL-PARTIAL-KEY.
00070              20  BILL-CO         PIC X.
00071              20  BILL-CARRIER    PIC X            VALUE SPACE.
00072              20  BILL-GROUPING   PIC X(6)         VALUE SPACES.
00073              20  BILL-ACCOUNT    PIC X(10)        VALUE SPACES.
00074              20  BILL-FIN-RESP   PIC X(10)        VALUE SPACES.
00075          16  BILL-RECORD-TYPE    PIC X            VALUE SPACE.
00076          16  BILL-SEQ-NO         PIC S9(4)  COMP  VALUE +0.
00077
00078      12  BILL-SAVE-KEY           PIC X(31)        VALUE SPACES.
00079      12  BILL-SAVE-PARTIAL-KEY   PIC X(28)        VALUE SPACES.
00080
00081      12  ERROR-LINE              PIC X(80)        VALUE SPACES.
00082      12  CURRENT-SAVE            PIC XX           VALUE SPACES.
00083      12  WS-BROWSE-STARTED       PIC X VALUE 'N'.
00084      12  SUB                     PIC 9   COMP-3   VALUE ZEROS.
00085      12  WS-SKIP                 PIC 99           VALUE ZEROS.
00086      12  WS-COPIES               PIC 9            VALUE ZEROS.
00087      12  HEADER-SW               PIC X            VALUE SPACE.
00088          88  HEADER-REC-FOUND                     VALUE SPACE.
00089
00090      12  CORRESPOND-SW           PIC X            VALUE SPACE.
00091          88  CORR-REC-FOUND                       VALUE SPACE.
00092
00093      12  ADDR-SW                 PIC X            VALUE SPACE.
00094          88  ADDRESS-REC-FOUND                    VALUE SPACE.
00095
00096      12  OPTION-CODES            PIC XX           VALUE SPACES.
00097          88  PRINT-STATEMENTS        VALUE '11'.
00098          88  PRINT-LABELS            VALUE ' 2'.
00099          88  REPRINT-STATEMENTS      VALUE ' 3'.
00100          88  PRINT-SINGLE-STATEMENT  VALUE ' 4'.
00101
00102      12  COMP-132                PIC S9(4)   COMP VALUE +132.
00103      12  COMP-80                 PIC S9(4)   COMP VALUE +80.
00104      12  J-BILL-LENGTH           PIC S9(4)   COMP VALUE +210.
00105      12  JOURNAL-LENGTH          PIC S9(4)   COMP.
00106
00107      12  WS-LABEL-HOLD-AREA.
00108          16  WS-LABEL-LINES OCCURS 6 TIMES INDEXED BY L-INDX.
00109            18  WS-LABEL-ZIP.
00110              20  WS-LABEL-1ST-ZIP    PIC X(4).
00111              20  WS-LABEL-2ND-ZIP    PIC X(5).
00112            18  FILLER                PIC X(12).
00113            18  WS-LAST-ZIP.
00114              20  WS-LAST-1ST-ZIP     PIC X(4).
00115              20  WS-LAST-2ND-ZIP     PIC X(5).
00116      EJECT
00117 *                                COPY ELCINTF.
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
00118      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.
00119          16  PI-PRINT-DATE       PIC X(8).
00120          16  PI-PRINT-DATE-BIN   PIC XX.
00121          16  PI-PRINT-ID         PIC X(4).
00122          16  FILLER              PIC X(626).
00123      EJECT
00124 *                                COPY ELCJPFX.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCJPFX.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *    USER DATA FOR SYSTEM JOURNAL RECORDS  JOURNAL I.D. = "EL"   *
00008 *                                                                *
00009 *     ALL RECORDS ARE JOURNALED FOR ERROR RECOVERY               *
00010 *     FILES JOURNALED FOR AUDIT TRAIL (BEFORE CHANGE) ARE -      *
00011 *        ELCNTL - CONTROL FILE                                   *
00012 *        ELMSTR - CLAIM MASTERS                                  *
00013 *        ELTRLR - ACTIVITY TRAILERS                              *
00014 *        ELCHKQ - CHECK QUE                                      *
00015 ******************************************************************
00016  01  JOURNAL-RECORD.
00017      12  JP-USER-ID                  PIC X(4).
00018      12  JP-FILE-ID                  PIC X(8).
00019      12  JP-PROGRAM-ID               PIC X(8).
00020      12  JP-RECORD-TYPE              PIC X.
00021          88 JP-ADD              VALUE 'A'.
00022          88 JP-BEFORE-CHANGE    VALUE 'B'.
00023          88 JP-AFTER-CHANGE     VALUE 'C'.
00024          88 JP-DELETE           VALUE 'D'.
00025          88 JP-GENERIC-DELETE   VALUE 'G'.
00026          88 JP-KEY-CHG-DELETE   VALUE 'K'.
00027          88 JP-KEY-CHG-ADD      VALUE 'N'.
00028      12  JP-GENERIC-KEY-LENGTH       PIC S9(4)   COMP.
00029      12  JP-RECORD-AREA
00030
00031
00125                                  PIC X(210).
00126      EJECT
00127 *                                COPY ELPRTCVD.
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
00128      EJECT
00129 *                                COPY ELCDATE.
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
00130      EJECT
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
00132 *01 PARMLIST .
00133 *    02  FILLER            PIC S9(8)   COMP.
00134 *    02  BILL-POINTER      PIC S9(8)   COMP.
00135      EJECT
00136 *                            COPY ERCBILL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCBILL                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.007                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = BILLING STATEMENTS FOR PRINTING           *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 210  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ERBILL                        RKP=2,LEN=31    *
00013 *                                                                *
00014 *   LOG = NO                                                     *
00015 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00016 ******************************************************************
       01  DFHCOMMAREA       PIC X(01).
00017  01  BILLING-STATEMENT.
00018      02 BILLING-STATEMENT-FILE.
00019      12  BI-RECORD-ID                PIC XX.
00020          88  VALID-BI-ID                VALUE 'BI'.
00021
00022      12  BI-CONTROL-PRIMARY.
00023          16  BI-COMPANY-CD           PIC X.
00024          16  BI-CARRIER              PIC X.
00025          16  BI-GROUPING             PIC X(6).
00026          16  BI-ACCOUNT              PIC X(10).
00027          16  BI-FIN-RESP             PIC X(10).
00028          16  BI-RECORD-TYPE          PIC X.
00029              88  BI-HEADER-DATA         VALUE '1'.
00030              88  BI-ADDRESS-DATA        VALUE '2'.
00031              88  BI-TEXT-DATA           VALUE '3'.
00032          16  BI-LINE-SEQ-NO          PIC S9(4)     COMP.
00033
00034      12  BI-TEXT-RECORD.
00035          16  BI-SKIP-CONTROL         PIC X.
00036          16  BI-TEXT-LINE            PIC X(157).
00037          16  BI-TEXT-LINE-1 REDEFINES BI-TEXT-LINE.
00038              20  BI-ADDR-LIT         PIC X(14).
00039              20  BI-CO               PIC X(7).
00040              20  BI-DASH             PIC X.
00041              20  BI-ACCT             PIC X(10).
00042              20  FILLER              PIC XX.
00043              20  BI-ACCT-ADDR        PIC X(30).
00044              20  FILLER              PIC X.
00045              20  BI-REMIT-LIT        PIC X(11).
00046              20  FILLER              PIC XX.
00047              20  BI-REMIT-ADDR       PIC X(30).
00048              20  FILLER              PIC X(49).
00049          16  BI-TEXT-LINE-2 REDEFINES BI-TEXT-LINE.
00050              20  BI-INS-LAST-NAME    PIC X(15).
00051              20  FILLER              PIC X.
00052              20  BI-INS-1ST-NAME     PIC X(10).
00053              20  FILLER REDEFINES BI-INS-1ST-NAME.
00054                  24  BI-INS-INITS    PIC XX.
00055                  24  FILLER          PIC X(8).
00056              20  FILLER              PIC X.
00057              20  BI-UNDRWRTR.
00058                  24  BI-INS-INIT     PIC X.
00059                  24  FILLER          PIC XXX.
00060              20  BI-CERT             PIC X(11).
00061              20  FILLER              PIC XX.
00062              20  BI-EFF-DT           PIC X(8).
00063              20  FILLER              PIC XX.
00064              20  BI-CAN-DT           PIC X(8).
00065              20  FILLER              PIC XX.
00066              20  BI-ED-TERM          PIC ZZZ.
00067              20  FILLER              PIC XX.
00068              20  BI-TYPE             PIC XXXX.
00069              20  FILLER              PIC X.
00070              20  BI-PREM             PIC ZZ,ZZZ,ZZZ.ZZ-.
00071              20  FILLER              PIC X(3).
00072              20  BI-ED-RATE          PIC ZZZ.ZZZ.
00073              20  FILLER              PIC XX.
00074              20  BI-COMM             PIC ZZ,ZZZ,ZZZ.ZZ-.
00075              20  FILLER              PIC XX.
00076              20  BI-FACE-AMT         PIC ZZZ,ZZZ,ZZZ.ZZ-.
00077              20  FILLER              PIC X(26).
00078          16  BI-TEXT-LINE-3 REDEFINES BI-TEXT-LINE.
00079              20  FILLER              PIC X(42).
00080              20  BI-TOT-DESC         PIC X(20).
00081              20  FILLER REDEFINES BI-TOT-DESC.
00082                  24  BI-TOT-LIT      PIC X(6).
00083                  24  BI-OVERRIDE-L6  PIC X(14).
00084              20  FILLER              PIC X(11).
00085              20  BI-TOT-PREM         PIC ZZZ,ZZZ,ZZZ.99-.
00086              20  BI-TOT-DASH REDEFINES
00087                  BI-TOT-PREM         PIC X(15).
00088              20  FILLER              PIC X(11).
00089              20  BI-COM-TOT          PIC ZZZ,ZZZ,ZZZ.99-.
00090              20  FILLER              PIC XX.
00091              20  BI-FACE-TOT         PIC ZZZ,ZZZ,ZZZ.99-.
00092              20  FILLER              PIC X(26).
00093          16  BI-TEXT-LINE-4 REDEFINES BI-TEXT-LINE.
00094              20  BI-ENTRY-DESC       PIC X(30).
00095              20  FILLER              PIC X(11).
00096              20  BI-ENTRY-AMT        PIC ZZZZ,ZZZ,ZZZ.99-.
00097              20  FILLER              PIC X(31).
00098              20  BI-ACCTG-COMMENT    PIC X(30).
00099              20  FILLER              PIC X(39).
00100          16  BI-TEXT-LINE-5 REDEFINES BI-TEXT-LINE.
00101              20  FILLER              PIC X(42).
00102              20  BI-TOT-DESC5        PIC X(20).
00103              20  FILLER              PIC X(11).
00104              20  BI-TOT-PREM5        PIC ZZZ,ZZZ,ZZZ.99-.
00105              20  BI-COM-TOT5         PIC ZZZ,ZZZ,ZZZ.99-.
00106              20  BI-NON-PREM5        PIC ZZZ,ZZZ,ZZZ.99-.
00107              20  BI-NON-COMM5        PIC ZZZ,ZZZ,ZZZ.99-.
00108              20  FILLER              PIC X(24).
00109          16  BI-TEXT-FIRST REDEFINES BI-TEXT-LINE.
00110              20  BI-TEXT-2-81        PIC X(80).
00111              20  FILLER              PIC X(77).
00112          16  BI-TEXT-LAST REDEFINES BI-TEXT-LINE.
00113              20  FILLER              PIC X(53).
00114              20  BI-TEXT-55-133      PIC X(79).
00115              20  FILLER              PIC X(25).
00116          16  BI-TEXT-TYPE            PIC X.
00117              88 DETAIL-LINE              VALUE 'D'.
00118          16  BI-TERM                 PIC S999.
00119          16  BI-BENEFIT-AMT          PIC S9(9)V99 COMP-3.
00120          16  BI-PREMIUM-AMT          PIC S9(7)V99 COMP-3.
00121          16  BI-RATE                 PIC S99V9(5) COMP-3.
00122
00123      12  BI-ADDRESS-RECORD  REDEFINES  BI-TEXT-RECORD.
00124          16  FILLER                  PIC X.
00125          16  BI-ACCT-ADDRESS-LINE    PIC X(30).
00126          16  FILLER                  PIC X(10).
00127          16  BI-REMIT-ADDRESS-LINE   PIC X(30).
00128          16  FILLER                  PIC X(106).
00129
00130      12  BI-HEADER-RECORD  REDEFINES  BI-TEXT-RECORD.
00131          16  BI-PROCESSOR-CD         PIC X(4).
00132          16  BI-STATEMENT-TYPE       PIC X.
00133              88  BI-PREVIEW-ONLY         VALUE 'P'.
00134          16  BI-NO-OF-COPIES         PIC S9.
00135          16  BI-CREATION-DT          PIC XX.
00136          16  BI-INITIAL-PRINT-DATE   PIC XX.
00137          16  BI-ACCOUNT-TOTALS.
00138              20  BI-BAL-FRWD         PIC S9(9)V99     COMP-3.
00139              20  BI-PREMIUM          PIC S9(9)V99     COMP-3.
00140              20  BI-REMITTED         PIC S9(9)V99     COMP-3.
00141              20  BI-TOT-ISS-COMP     PIC S9(9)V99     COMP-3.
00142              20  BI-TOT-CAN-COMP     PIC S9(9)V99     COMP-3.
00143              20  BI-ADJUSTMNTS       PIC S9(9)V99     COMP-3.
00144              20  BI-DISBURSED        PIC S9(9)V99     COMP-3.
00145              20  BI-END-BAL          PIC S9(9)V99     COMP-3.
00146          16  BI-FIN-RESP-ACCT        PIC X(10).
00147          16  BI-FIN-RESP-NAME        PIC X(30).
00148          16  FILLER                  PIC X(79).
00149
00150
00151      02 GA-BILLING-STATEMENT REDEFINES BILLING-STATEMENT-FILE.
00152      12  FILLER                      PIC XX.
00153
00154      12  GA-CONTROL-PRIMARY.
00155          16  FILLER                  PIC X(31).
00156
00157      12  GA-TEXT-RECORD.
00158          16  GA-SKIP-CONTROL         PIC X.
00159          16  GA-TEXT-LINE            PIC X(132).
00160          16  GA-TEXT-LINE-1 REDEFINES GA-TEXT-LINE.
00161              20  FILLER              PIC X.
00162              20  GA-CARRIER          PIC X.
00163              20  GA-GROUPING         PIC X(6).
00164              20  GA-DASH             PIC X.
00165              20  GA-AGENT            PIC X(10).
00166              20  FILLER              PIC X.
00167              20  GA-AGENT-ADDR       PIC X(30).
00168              20  FILLER              PIC X(82).
00169          16  GA-TEXT-LINE-2 REDEFINES GA-TEXT-LINE.
00170              20  GA-ACCT             PIC X(10).
00171              20  FILLER              PIC X.
00172              20  GA-ACCT-NAME        PIC X(30).
00173              20  GA-BEG-BAL          PIC ZZZZ,ZZZ.99-.
00174              20  GA-NET-PREM         PIC ZZZZ,ZZZ.ZZ-.
00175              20  GA-ACCT-COMP        PIC ZZZZ,ZZZ.ZZ-.
00176              20  GA-PMTS-ADJS        PIC ZZZZ,ZZZ.ZZ-.
00177              20  GA-UNPAID-NET-PREM  PIC ZZZZ,ZZZ.ZZ-.
00178              20  GA-BEN-OVERRIDE-L6  PIC X(6).
00179              20  FILLER              PIC X.
00180              20  GA-OVERWRITE        PIC ZZZZ,ZZZ.ZZ-.
00181              20  GA-AMT-DUE          PIC ZZZ,ZZZ.99-.
00182              20  FILLER              PIC X.
00183          16  GA-TEXT-LINE-3 REDEFINES GA-TEXT-LINE.
00184              20  FILLER              PIC X(11).
00185              20  GA-ENTRY-DESC       PIC X(30).
00186              20  FILLER              PIC X(60).
00187              20  GA-ENTRY-COMMENT    PIC X(30).
00188              20  FILLER              PIC X.
00189          16  FILLER.
00190              20  GA-BENEFIT-CD       PIC XX.
00191              20  GA-BEG-BAL-AMT      PIC S9(7)V99 COMP-3.
00192              20  GA-END-BAL-AMT      PIC S9(7)V99 COMP-3.
00193              20  GA-NET-PREM-AMT     PIC S9(7)V99 COMP-3.
00194              20  GA-ACCT-COMP-AMT    PIC S9(7)V99 COMP-3.
00195              20  GA-PMTS-ADJS-AMT    PIC S9(7)V99 COMP-3.
00196              20  GA-UNPAID-NET-AMT   PIC S9(7)V99 COMP-3.
00197              20  GA-OVERWRITE-AMT    PIC S9(7)V99 COMP-3.
00198              20  GA-AMT-DUE-AMT      PIC S9(7)V99 COMP-3.
00199
00200      12  GA-ADDRESS-RECORD  REDEFINES  GA-TEXT-RECORD.
00201          16  FILLER                  PIC X.
00202          16  GA-ACCT-ADDRESS-LINE    PIC X(30).
00203          16  FILLER                  PIC X(144).
00204
00205      12  GA-HEADER-RECORD  REDEFINES  GA-TEXT-RECORD.
00206          16  GA-PROCESSOR-CD         PIC X(4).
00207          16  GA-STATEMENT-TYPE       PIC X.
00208              88  GA-PREVIEW-ONLY         VALUE 'P'.
00209          16  GA-NO-OF-COPIES         PIC S9.
00210          16  GA-CREATION-DT          PIC XX.
00211          16  GA-INITIAL-PRINT-DATE   PIC XX.
00212          16  GA-AGENT-TOTALS.
00213              20  GA-NET-UNPD         PIC S9(9)V99     COMP-3.
00214              20  GA-COMP-UNPD-PREM   PIC S9(9)V99     COMP-3.
00215              20  GA-PREMIUM          PIC S9(9)V99     COMP-3.
00216              20  GA-REMITTED         PIC S9(9)V99     COMP-3.
00217              20  GA-TOT-ISS-COMP     PIC S9(9)V99     COMP-3.
00218              20  GA-TOT-CAN-COMP     PIC S9(9)V99     COMP-3.
00219              20  GA-ADJUSTMNTS       PIC S9(9)V99     COMP-3.
00220              20  GA-DISBURSED        PIC S9(9)V99     COMP-3.
00221              20  GA-END-BALANCE      PIC S9(9)V99     COMP-3.
00222          16  GA-AGENTS-NAME          PIC X(30).
00223          16  FILLER                  PIC X(81).
00224      12  FILLER                      PIC XX.
00225
00226
00137      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA BILLING-STATEMENT.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6411' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00139
00140      MOVE SPACES                 TO DL34-PROCESS-TYPE.
00141
00142  0100-RETRIEVE-LOOP.
00143      
      * EXEC CICS HANDLE CONDITION
00144 *         ENDDATA  (0200-END-DATA)
00145 *         NOTFND   (0300-NOT-FOUND)
00146 *    END-EXEC.
      *    MOVE '"$&I                  ! " #00001099' TO DFHEIV0
           MOVE X'222426492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031303939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00147
00148      
      * EXEC CICS RETRIEVE
00149 *         INTO   (PROGRAM-INTERFACE-BLOCK)
00150 *         LENGTH (PI-COMM-LENGTH)
00151 *    END-EXEC.
      *    MOVE '0*I L                 &   #00001104' TO DFHEIV0
           MOVE X'302A49204C20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031313034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00152
00153 * DLO034 OPEN WHEN DMD OR CID
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
00155          IF DL34-PROCESS-TYPE IS EQUAL TO SPACES
00156              MOVE 'O'                TO DL34-PROCESS-TYPE
00157              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
00158              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
00159              MOVE PI-PROCESSOR-ID    TO DL34-USERID
00160              MOVE SPACES             TO DL34-PRINT-LINE
00161              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID
00162              
      * EXEC CICS LINK
00163 *                PROGRAM    ('DLO034')
00164 *                COMMAREA   (DLO034-COMMUNICATION-AREA)
00165 *                LENGTH     (DLO034-REC-LENGTH)
00166 *            END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   ''   #00001118' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031313138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00167              IF DL34-RETURN-CODE NOT = 'OK'
00168                  MOVE  '**DLO034 OPEN ERROR - ABORT**'
00169                                      TO ERROR-LINE
00170                  PERFORM 0400-SEND-TEXT
00171                  
      * EXEC CICS RETURN
00172 *                END-EXEC.
      *    MOVE '.(                    &   #00001127' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031313237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00173
00199      PERFORM 1000-INITIALIZE             THRU 1000-EXIT.
00200
00201      PERFORM 2000-BROWSE-BILLING-HEADERS THRU 2099-EXIT.
00211
00212  0200-END-DATA.
00213
00214 * DLO034 CLOSE
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
00216          MOVE 'C'                TO DL34-PROCESS-TYPE
00217          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
00218          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
00219          MOVE PI-PROCESSOR-ID    TO DL34-USERID
00220          MOVE SPACES             TO DL34-PRINT-LINE
00221                                     DL34-OVERRIDE-PRINTER-ID
00222          
      * EXEC CICS LINK
00223 *            PROGRAM    ('DLO034')
00224 *            COMMAREA   (DLO034-COMMUNICATION-AREA)
00225 *            LENGTH     (DLO034-REC-LENGTH)
00226 *        END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   ''   #00001144' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031313434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00227          IF DL34-RETURN-CODE NOT = 'OK'
00228              MOVE  '**DLO034 CLOSE ERROR - ABORT**'
00229                                  TO ERROR-LINE
00230              PERFORM 0400-SEND-TEXT.
00231
00232      
      * EXEC CICS RETURN
00233 *    END-EXEC.
      *    MOVE '.(                    &   #00001154' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031313534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00234      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6411' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00235
00236  0300-NOT-FOUND.
00237      MOVE 'NO COMMUNICATION AREA FOUND' TO ERROR-LINE
00238      PERFORM 0400-SEND-TEXT
00239      GO TO 0200-END-DATA.
00240
00241  0400-SEND-TEXT.
00242      
      * EXEC CICS SEND TEXT
00243 *         FROM   (ERROR-LINE)
00244 *         LENGTH (70)
00245 *    END-EXEC.
           MOVE 70
             TO DFHEIV11
      *    MOVE '8&      T       H   F -   #00001164' TO DFHEIV0
           MOVE X'382620202020202054202020' TO DFHEIV0(1:12)
           MOVE X'202020204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031313634' TO DFHEIV0(25:11)
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
           
00246
00247      EJECT
00248  1000-INITIALIZE.
00249      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00250      MOVE '5'                    TO DC-OPTION-CODE.
00251      PERFORM 9700-DATE-LINK THRU  9700-EXIT.
00252      MOVE DC-BIN-DATE-1          TO CURRENT-SAVE.
00253      MOVE PI-COMPANY-CD          TO BILL-CO.
00254      MOVE PI-ENTRY-CODES         TO OPTION-CODES.
00255      MOVE COMP-132               TO WS-LINE-LEN.
00256
00257  1000-EXIT.
00258      EXIT.
00259
00260      EJECT
00261  2000-BROWSE-BILLING-HEADERS.
00262      MOVE LOW-VALUES             TO BILL-KEY.
00263
00264      IF PRINT-SINGLE-STATEMENT
00265          MOVE PI-COMPANY-CD  TO  BILL-CO
00266          MOVE PI-CR-CARRIER  TO  BILL-CARRIER
00267          MOVE PI-CR-GROUPING TO  BILL-GROUPING
00268          MOVE PI-CR-ACCOUNT  TO  BILL-ACCOUNT
00269          MOVE PI-CR-FIN-RESP TO  BILL-FIN-RESP
00270          MOVE '1'            TO  BILL-RECORD-TYPE
00271      ELSE
00272          MOVE PI-COMPANY-CD  TO  BILL-CO.
00273
00274      
      * EXEC CICS HANDLE CONDITION
00275 *         NOTFND (2099-EXIT)
00276 *         NOTOPEN(8890-BILL-NOT-OPEN)
00277 *    END-EXEC.
      *    MOVE '"$IJ                  ! # #00001196' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303031313936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00278
00279  2005-START.
00280      
      * EXEC CICS STARTBR
00281 *         DATASET(BILL-ID)
00282 *         RIDFLD (BILL-KEY)
00283 *     END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00001202' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BILL-ID, 
                 BILL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00284
00285      MOVE 'Y'                    TO WS-BROWSE-STARTED.
00286
00287  2010-READ-NEXT.
00288      
      * EXEC CICS HANDLE CONDITION
00289 *         NOTFND (2050-END-BR)
00290 *         ENDFILE(2050-END-BR)
00291 *         NOTOPEN(8890-BILL-NOT-OPEN)
00292 *    END-EXEC.
      *    MOVE '"$I''J                 ! $ #00001210' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303031323130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00293
00294      
      * EXEC CICS READNEXT
00295 *         DATASET(BILL-ID)
00296 *         RIDFLD (BILL-KEY)
00297 *         SET    (ADDRESS OF BILLING-STATEMENT)
00298 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00001216' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BILL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BILL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BILLING-STATEMENT TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00299
00300      IF PI-COMPANY-CD NOT = BILL-CO
00301         GO TO 2050-END-BR.
00302
00303      IF BILL-RECORD-TYPE = '1'
00304             NEXT SENTENCE
00305      ELSE
00306         MOVE 9999                TO BILL-SEQ-NO
00307         GO TO 2010-READ-NEXT.
00308
00309      MOVE BILL-KEY              TO BILL-SAVE-KEY.
00310
00311      IF PRINT-STATEMENTS
00312         PERFORM 2900-PRINT-STATEMENTS THRU 2999-EXIT
00313         IF WS-BROWSE-STARTED = 'N'
00314            GO TO 2005-START
00315           ELSE
00316            GO TO 2010-READ-NEXT.
00317
00318      IF REPRINT-STATEMENTS
00319         PERFORM 2200-REPRINT-STATEMENTS THRU 2299-EXIT
00320         IF WS-BROWSE-STARTED = 'N'
00321            GO TO 2005-START
00322           ELSE
00323         GO TO 2010-READ-NEXT.
00324
00325      IF PRINT-SINGLE-STATEMENT
00326         PERFORM 2900-PRINT-STATEMENTS THRU 2999-EXIT
00327         GO TO 2050-END-BR.
00328
00329      PERFORM 2300-STATEMENT-LABELS THRU 2399-EXIT.
00330      GO TO 2010-READ-NEXT.
00331
00332  2050-END-BR.
00333      IF PRINT-LABELS
00334         MOVE 'X'                 TO WS-PROG-END
00335         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00336
00337      IF WS-BROWSE-STARTED = 'Y'
00338         MOVE 'N'                 TO WS-BROWSE-STARTED
00339         
      * EXEC CICS ENDBR
00340 *            DATASET(BILL-ID)
00341 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001261' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BILL-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00342
00343  2099-EXIT.
00344       EXIT.
00345
00346      EJECT
00347  2200-REPRINT-STATEMENTS.
00348      IF BI-INITIAL-PRINT-DATE   = PI-PRINT-DATE-BIN
00349         MOVE BI-NO-OF-COPIES     TO WS-COPIES
00350         PERFORM 7200-PRINT-TEXT  THRU 7299-EXIT
00351                 WS-COPIES TIMES.
00352
00353  2299-EXIT.
00354       EXIT.
00355
00356      EJECT
00357  2300-STATEMENT-LABELS.
00358      IF FIRST-TIME
00359         PERFORM 2400-ALIGNMENT-PRINT THRU 2450-EXIT.
00360
00361      IF BI-INITIAL-PRINT-DATE = PI-PRINT-DATE-BIN
00362         PERFORM 7200-PRINT-TEXT THRU 7299-EXIT.
00363
00364  2399-EXIT.
00365       EXIT.
00366
00367      EJECT
00368  2400-ALIGNMENT-PRINT.
00369      MOVE ALL '*'                TO WS-LABEL-HOLD-AREA.
00370      MOVE SPACE                  TO WS-PASSED-CNTL-CHAR.
00371      MOVE SPACES                 TO WS-LABEL-LINES (6).
00372      PERFORM 2480-MOVE-TO-PRINT THRU 2499-EXIT 6 TIMES.
00373
00374  2450-EXIT.
00375       EXIT.
00376
00377  2480-MOVE-TO-PRINT.
00378      MOVE WS-LABEL-LINES (1)     TO WS-PASSED-DATA.
00379      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00380      MOVE WS-LABEL-LINES (2)     TO WS-PASSED-DATA.
00381      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00382      MOVE WS-LABEL-LINES (3)     TO WS-PASSED-DATA.
00383      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00384      MOVE WS-LABEL-LINES (4)     TO WS-PASSED-DATA.
00385      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00386      MOVE WS-LABEL-LINES (5)     TO WS-PASSED-DATA.
00387      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00388      MOVE WS-LABEL-LINES (6)     TO WS-PASSED-DATA.
00389      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00390
00391  2499-EXIT.
00392       EXIT.
00393
00394      EJECT
00395  2900-PRINT-STATEMENTS.
00396      IF BI-INITIAL-PRINT-DATE = LOW-VALUES
00397        OR PRINT-SINGLE-STATEMENT
00398         PERFORM 7300-UPDATE-BILLING-HEADER THRU 7399-EXIT
00399         IF HEADER-REC-FOUND
00400            PERFORM 7200-PRINT-TEXT THRU 7299-EXIT
00401                    WS-COPIES TIMES.
00402
00403  2999-EXIT.
00404      EXIT.
00405
00406      EJECT
00407  7200-PRINT-TEXT.
00408      MOVE SPACES                 TO WS-PROG-END.
00409
00410      IF PRINT-LABELS
00411         MOVE '2'                 TO BILL-RECORD-TYPE
00412         SET L-INDX               TO 1
00413      ELSE
00414         MOVE '3'                 TO BILL-RECORD-TYPE.
00415
00416      MOVE ZEROS                  TO BILL-SEQ-NO.
00417
00418      
      * EXEC CICS HANDLE CONDITION
00419 *         NOTFND (7250-CHECK-FIRST-SW)
00420 *         ENDFILE(7250-CHECK-FIRST-SW)
00421 *         NOTOPEN(8890-BILL-NOT-OPEN)
00422 *    END-EXEC.
      *    MOVE '"$I''J                 ! % #00001340' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303031333430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00423
00424      IF WS-BROWSE-STARTED = 'Y'
00425          NEXT SENTENCE
00426      ELSE
00427          
      * EXEC CICS STARTBR
00428 *            DATASET(BILL-ID)
00429 *            RIDFLD (BILL-KEY)
00430 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00001349' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BILL-ID, 
                 BILL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00431          MOVE 'Y'                TO WS-BROWSE-STARTED.
00432
00433      MOVE BILL-PARTIAL-KEY       TO BILL-SAVE-PARTIAL-KEY.
00434
00435  7210-READ-NEXT.
00436      
      * EXEC CICS READNEXT
00437 *         DATASET(BILL-ID)
00438 *         RIDFLD (BILL-KEY)
00439 *         SET    (ADDRESS OF BILLING-STATEMENT)
00440 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00001358' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BILL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BILL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BILLING-STATEMENT TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00441
00442      IF BILL-PARTIAL-KEY NOT = BILL-SAVE-PARTIAL-KEY
00443         GO TO 7250-CHECK-FIRST-SW.
00444
00445      IF PRINT-LABELS
00446         MOVE BI-ACCT-ADDRESS-LINE TO WS-LABEL-LINES (L-INDX)
00447         SET L-INDX               UP BY 1
00448         GO TO 7210-READ-NEXT.
00449
00450      MOVE BI-TEXT-LINE           TO WS-PASSED-DATA.
00451
00452      MOVE BI-SKIP-CONTROL        TO WS-PASSED-CNTL-CHAR.
00453
00454      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00455
00456      GO TO 7210-READ-NEXT.
00457
00458  7250-CHECK-FIRST-SW.
00459      IF PRINT-LABELS
00460         GO TO 7260-LABEL-PRINT.
00461
00462      IF FIRST-TIME
00463         GO TO 7299-EXIT.
00464
00465      MOVE 'X'                 TO WS-PROG-END.
00466      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00467      GO TO 7290-END-BR.
00468
00469  7260-LABEL-PRINT.
00470      IF L-INDX = 1 OR
00471         WS-LABEL-HOLD-AREA = SPACES
00472         GO TO 7290-END-BR.
00473
00474      IF WS-LABEL-LINES (1) = SPACES
00475         MOVE WS-LABEL-LINES (2)  TO WS-LABEL-LINES (1)
00476         MOVE WS-LABEL-LINES (3)  TO WS-LABEL-LINES (2)
00477         MOVE WS-LABEL-LINES (4)  TO WS-LABEL-LINES (3)
00478         MOVE WS-LABEL-LINES (5)  TO WS-LABEL-LINES (4)
00479         MOVE WS-LABEL-LINES (6)  TO WS-LABEL-LINES (5)
00480         MOVE SPACES              TO WS-LABEL-LINES (6)
00481         GO TO 7260-LABEL-PRINT.
00482
00483      IF WS-LABEL-LINES (2) = SPACES
00484         MOVE WS-LABEL-LINES (3)  TO WS-LABEL-LINES (2)
00485         MOVE WS-LABEL-LINES (4)  TO WS-LABEL-LINES (3)
00486         MOVE WS-LABEL-LINES (5)  TO WS-LABEL-LINES (4)
00487         MOVE WS-LABEL-LINES (6)  TO WS-LABEL-LINES (5)
00488         MOVE SPACES              TO WS-LABEL-LINES (6)
00489         GO TO 7260-LABEL-PRINT.
00490
00491      IF WS-LABEL-LINES (3) = SPACES
00492         MOVE WS-LABEL-LINES (4)  TO WS-LABEL-LINES (3)
00493         MOVE WS-LABEL-LINES (5)  TO WS-LABEL-LINES (4)
00494         MOVE WS-LABEL-LINES (6)  TO WS-LABEL-LINES (5)
00495         MOVE SPACES              TO WS-LABEL-LINES (6)
00496         GO TO 7260-LABEL-PRINT.
00497
00498      IF WS-LABEL-LINES (4) = SPACES
00499         MOVE WS-LABEL-LINES (5)  TO WS-LABEL-LINES (4)
00500         MOVE WS-LABEL-LINES (6)  TO WS-LABEL-LINES (5)
00501         MOVE SPACES              TO WS-LABEL-LINES (6)
00502         GO TO 7260-LABEL-PRINT.
00503
00504      IF WS-LABEL-LINES (5) = SPACES
00505         MOVE WS-LABEL-LINES (6)  TO WS-LABEL-LINES (5)
00506         MOVE SPACES              TO WS-LABEL-LINES (6).
00507
00508      IF WS-LABEL-LINES (6) NOT = SPACES
00509         IF WS-LABEL-1ST-ZIP (6) NOT = ZEROS
00510            MOVE WS-LABEL-ZIP (6)     TO WS-LAST-ZIP (5)
00511            MOVE SPACES               TO WS-LABEL-LINES (6)
00512            ELSE
00513            MOVE WS-LABEL-2ND-ZIP (6) TO WS-LAST-2ND-ZIP (5)
00514            MOVE SPACES               TO WS-LABEL-LINES (6)
00515         ELSE
00516         IF WS-LABEL-LINES (5) NOT = SPACES
00517            IF WS-LABEL-1ST-ZIP (5) NOT = ZEROS
00518               MOVE WS-LABEL-ZIP (5)     TO WS-LAST-ZIP (5)
00519               MOVE SPACES               TO WS-LABEL-ZIP (5)
00520               ELSE
00521               MOVE WS-LABEL-2ND-ZIP (5) TO WS-LAST-2ND-ZIP (5)
00522               MOVE SPACES               TO WS-LABEL-ZIP (5)
00523            ELSE
00524            IF WS-LABEL-LINES (4) NOT = SPACES
00525               IF WS-LABEL-1ST-ZIP (4) NOT = ZEROS
00526                  MOVE WS-LABEL-ZIP (4)     TO WS-LAST-ZIP (4)
00527                  MOVE SPACES               TO WS-LABEL-ZIP (4)
00528                  ELSE
00529                  MOVE WS-LABEL-2ND-ZIP (4) TO WS-LAST-2ND-ZIP (4)
00530                  MOVE SPACES               TO WS-LABEL-ZIP (4).
00531
00532      PERFORM 2480-MOVE-TO-PRINT THRU 2499-EXIT.
00533
00534  7290-END-BR.
00535      IF WS-BROWSE-STARTED = 'Y'
00536         MOVE 'N'                 TO WS-BROWSE-STARTED
00537         
      * EXEC CICS ENDBR
00538 *            DATASET(BILL-ID)
00539 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001459' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BILL-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00540
00541      
      * EXEC CICS SYNCPOINT
00542 *         END-EXEC.
      *    MOVE '6"                    !   #00001463' TO DFHEIV0
           MOVE X'362220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00543
00544  7299-EXIT.
00545       EXIT.
00546
00547      EJECT
00548  7300-UPDATE-BILLING-HEADER.
00549      IF WS-BROWSE-STARTED = 'Y'
00550         MOVE 'N'                 TO WS-BROWSE-STARTED
00551         
      * EXEC CICS ENDBR
00552 *            DATASET(BILL-ID)
00553 *       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001473' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BILL-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00554
00555      PERFORM 8100-READ-HEADER THRU 8199-EXIT.
00556
00557      IF HEADER-REC-FOUND
00558          NEXT SENTENCE
00559      ELSE
00560          GO TO 7399-EXIT.
00561
00562      MOVE J-BILL-LENGTH          TO JOURNAL-LENGTH.
00563      MOVE 'B'                    TO JP-RECORD-TYPE.
00564      MOVE BILL-ID                TO JP-FILE-ID.
00565      MOVE BILLING-STATEMENT      TO JP-RECORD-AREA.
00566      PERFORM 8400-LOG-JOURNAL-RECORD.
00567      MOVE CURRENT-SAVE           TO BI-INITIAL-PRINT-DATE.
00568      MOVE BI-NO-OF-COPIES        TO WS-COPIES.
00569      MOVE J-BILL-LENGTH          TO JOURNAL-LENGTH.
00570      MOVE 'C'                    TO JP-RECORD-TYPE.
00571      MOVE BILL-ID                TO JP-FILE-ID.
00572      MOVE BILLING-STATEMENT      TO JP-RECORD-AREA.
00573
00574      
      * EXEC CICS REWRITE
00575 *         DATASET(BILL-ID)
00576 *         FROM   (BILLING-STATEMENT)
00577 *    END-EXEC.
           MOVE LENGTH OF
            BILLING-STATEMENT
             TO DFHEIV11
      *    MOVE '&& L                  %   #00001496' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BILL-ID, 
                 BILLING-STATEMENT, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00578
00579      PERFORM 8400-LOG-JOURNAL-RECORD.
00580
00581  7399-EXIT.
00582      EXIT.
00583
00584      EJECT
00585  8100-READ-HEADER.
00586      MOVE BILL-SAVE-KEY          TO BILL-KEY.
00587
00588      
      * EXEC CICS HANDLE CONDITION
00589 *         NOTOPEN(8890-BILL-NOT-OPEN)
00590 *         NOTFND (8150-NOT-FOUND)
00591 *    END-EXEC.
      *    MOVE '"$JI                  ! & #00001510' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303031353130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00592
00593      
      * EXEC CICS READ
00594 *         DATASET(BILL-ID)
00595 *         RIDFLD (BILL-KEY)
00596 *         SET    (ADDRESS OF BILLING-STATEMENT)
00597 *         UPDATE
00598 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00001515' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BILL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BILL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BILLING-STATEMENT TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00599
00600      GO TO 8199-EXIT.
00601
00602  8150-NOT-FOUND.
00603      MOVE '1'                    TO HEADER-SW.
00604
00605  8199-EXIT.
00606       EXIT.
00607
00608      EJECT
00609  8400-LOG-JOURNAL-RECORD.
00610      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.
00611      MOVE THIS-PGM               TO JP-PROGRAM-ID.
00612
00613 *    EXEC CICS JOURNAL
00614 *         JFILEID(PI-JOURNAL-FILE-ID)
00615 *         JTYPEID('EL')
00616 *         FROM   (JOURNAL-RECORD)
00617 *         LENGTH (JOURNAL-LENGTH)
00618 *    END-EXEC.
00619
00620  8890-BILL-NOT-OPEN.
00621      MOVE 'BILLING STATEMENT FILE NOT OPEN - ERBILL' TO ERROR-LINE
00622      PERFORM 0400-SEND-TEXT
00623      GO TO 0200-END-DATA.
00624
00625      EJECT
00626  9700-DATE-LINK.
00627      MOVE LINK-ELDATCV TO PGM-NAME
00628      
      * EXEC CICS LINK
00629 *        PROGRAM (PGM-NAME)
00630 *        COMMAREA(DATE-CONVERSION-DATA)
00631 *        LENGTH  (DC-COMM-LENGTH)
00632 *    END-EXEC.
      *    MOVE '."C                   ''   #00001550' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00633
00634  9700-EXIT.
00635       EXIT.
00636      EJECT
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
      *    MOVE '."C                   ''   #00001617' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363137' TO DFHEIV0(25:11)
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
      *    MOVE '$$    C E         L F ,   #00001738' TO DFHEIV0
           MOVE X'242420202020432045202020' TO DFHEIV0(1:12)
           MOVE X'2020202020204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373338' TO DFHEIV0(25:11)
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
00638
00639  9995-SECURITY-VIOLATION.
00640 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00001770' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373730' TO DFHEIV0(25:11)
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
00641
00642  9995-EXIT.
00643      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6411' TO DFHEIV1
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
               GO TO 2099-EXIT,
                     8890-BILL-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 2050-END-BR,
                     2050-END-BR,
                     8890-BILL-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 7250-CHECK-FIRST-SW,
                     7250-CHECK-FIRST-SW,
                     8890-BILL-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8890-BILL-NOT-OPEN,
                     8150-NOT-FOUND
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6411' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
