00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL602 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 16:56:43.
00007 *                            VMOD=2.015
00008 *
00009 *AUTHOR.        LOGIC,INC.
00010 *               DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *                                                                *
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00022 *            *                                                   *
00023 *            *****************************************************
00024
00025 *REMARKS.
00026 *        TRANSACTION - EXA2 - MORTALITY TABLE CONTROLS.
00027
010413******************************************************************
010413*                   C H A N G E   L O G
010413*
010413* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
010413*-----------------------------------------------------------------
010413*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
010413* EFFECTIVE    NUMBER
010413*-----------------------------------------------------------------
010413* 010413  CR2012072400002  PEMA  INCREASE MAX # TBLS ALLOWED
010413******************************************************************
00028  ENVIRONMENT DIVISION.
00029  DATA DIVISION.
00030                                  EJECT
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.
00033  77  FILLER  PIC X(032) VALUE '********************************'.
00034  77  FILLER  PIC X(032) VALUE '*    EL602 WORKING STORAGE     *'.
00035  77  FILLER  PIC X(032) VALUE '************ V/M 2.015 *********'.
00036
00037                                  EJECT
00038  01  W-PROGRAM-WORK-AREA.
00039      12  FILLER                  PIC  X(17)
00040                              VALUE 'PROGRAM WORK AREA'.
00041      12  SC-ITEM-CL-CR           PIC S9(04) COMP  VALUE +1.
00042      12  W-APPL-SCRTY-NDX        PIC S9(04) COMP  VALUE +4.
00043      12  W-ASKTIME-CTR           PIC S9(04) COMP  VALUE ZEROS.
00044      12  W-JOURNAL-LENGTH        PIC S9(04) COMP  VALUE ZEROS.
00045      12  W-LINE-NUMBER           PIC S9(04) COMP  VALUE ZEROS.
00046      12  W-NEXT-TABLE-LINE       PIC S9(04) COMP  VALUE ZEROS.
00047      12  W-REAL-TABLE-LINE       PIC S9(04) COMP  VALUE ZEROS.
00048      12  W-WORK-CTR              PIC S9(04) COMP.
00049      12  W-WORK-FIELD            PIC S9(03) COMP-3.
00050
00051      12  W-CHECK-PFKEYS          PIC  9(02).
00052      12  W-DISPLAY-LINE-NUMBER.
010413         16  W-EDITED-LINE-NUMB  PIC  Z99.
010413*        16  FILLER              PIC  X(01)       VALUE '.'.
00055      12  W-HOLD-INTEREST         PIC V9(04)       VALUE ZEROS.
00056      12  W-HOLD-JOINT-FACTOR     PIC  9(01)V9(04) VALUE ZEROS.
00057      12  W-HOLD-RESERVE-ADJ      PIC  9(01)V9(04) VALUE ZEROS.
00058      12  W-LAST-ERROR            PIC  9(04)       VALUE 9999.
00059
00060      12  W-CALL-PGM              PIC  X(08).
00061      12  W-COMP-CD-R.
00062          16  FILLER              PIC  X(01).
00063          16  W-COMP-CD-X         PIC  X(01).
00064      12  W-COMP-CD   REDEFINES
00065          W-COMP-CD-R             PIC S9(04)               COMP.
00066
00067      12  W-DEEDIT-FIELD          PIC  X(15).
00068      12  W-DEEDIT-FIELD-V0 REDEFINES
00069          W-DEEDIT-FIELD          PIC S9(15).
00070      12  W-DEEDIT-FIELD-V1 REDEFINES
00071          W-DEEDIT-FIELD          PIC S9(14)V9(01).
00072      12  W-DEEDIT-FIELD-V2 REDEFINES
00073          W-DEEDIT-FIELD          PIC S9(13)V9(02).
00074      12  W-DEEDIT-FIELD-V5 REDEFINES
00075          W-DEEDIT-FIELD          PIC S9(10)V9(05).
00076
00077      12  W-INTEREST-N            PIC V9(04).
00078      12  W-INTEREST-R   REDEFINES  W-INTEREST-N.
00079          16  FILLER              PIC  X(01).
00080          16  W-INT               PIC  X(02).
00081          16  FILLER              PIC  X(01).
00082
00083      12  W-MORT-CODE.
00084          16  W-MORT-TBL          PIC  X(01).
00085          16  W-MORT-INT          PIC  X(02).
00086          16  W-MORT-TYP          PIC  X(01).
00087      12  W-SAVE-DATE             PIC  X(08)  VALUE SPACES.
00088      12  W-SAVE-BIN-DATE         PIC  X(02)  VALUE SPACES.
00089      12  W-TEMP-LINE             PIC  X(53).
00090
00091      12  W-TIME-IN               PIC S9(07).
00092      12  W-TIME-OUT-R REDEFINES W-TIME-IN.
00093          16  FILLER              PIC  X(01).
00094          16  W-TIME-OUT          PIC  9(02)V9(02).
00095          16  FILLER              PIC  X(02).
00096
00097                                  EJECT
00098  01  W-PROGRAM-KEY-AREAS.
00099      12  FILLER                  PIC  X(10)  VALUE 'KEY AREAS:'.
00100
00101      12  W-QUID-KEY.
00102          16  W-QUID-TERMINAL     PIC  X(04).
00103          16  W-QUID-MAP-NUM      PIC  X(04)  VALUE '602A'.
00104
00105      12  W-WORKING-CNTL-KEY.
00106          16  W-CNTL-COMPANY-ID   PIC  X(03).
00107          16  W-CNTL-RECORD-TYPE  PIC  X(01)  VALUE '7'.
00108          16  FILLER              PIC  X(04)  VALUE SPACES.
00109          16  W-CNTL-SEQUENCE-NO  PIC S9(04)   COMP.
00110                                  EJECT
00111  01  W-PROGRAM-SWITCHES-AND-TESTS.
00112      12  FILLER                  PIC  X(15)
00113                                  VALUE 'SWITCHES/TESTS:'.
00114
00115      12  W-CHECK-MAINT           PIC  X(01).
00116          88  W-VALID-OPTION      VALUE 'A' 'C' 'D' 'S' 'I'.
00117      12  W-EXCHANGE-MADE-IND     PIC  X(01)  VALUE ' '.
00118          88  W-EXCHANGE-MADE                 VALUE 'Y'.
00119      12  W-FIRST-TIME-SW         PIC  X(01)  VALUE ' '.
00120          88  W-FIRST-TIME                    VALUE 'Y'.
00121      12  W-LAST-MORT-READ-IND    PIC  X(01)  VALUE ' '.
00122          88  W-LAST-MORT-RECORD-READ         VALUE 'Y'.
00123      12  W-VALID-JOINT-CODE-IND  PIC  X(01).
00124          88  W-VALID-JOINT-CODE         VALUE 'A' 'V'
00125                                         SPACES LOW-VALUES.
00126      12  W-VALID-TYPE-IND        PIC  X(05).
00127          88  W-TYPE-VALID-C             VALUE 'J' 'S'.
00128          88  W-TYPE-VALID-M             VALUE 'J' 'S' 'C'.
00129      12  W-SUPPORTED-TABLES-IND  PIC  X(05).
00130          88  W-TABLE-SUPPORTED          VALUE '41CSO' '58CET'
00131                                               '58CSO' '60CSG'
00132                                               '58FSO' '80MSO'
00125                                               '58CSO' '58FSO'
CIDMOD                                              '58UET' '58USO'
CIDMOD                                              '80CSO' '80UET'
CIDMOD                                              '80FSO' '80MET'
00134                                               '80FET' '80GBT'
101005                                              '80USO' '01CSO'
00135                                               'ZERO ' 'XXXXX'.
00136          88  W-NULL-ENTRY               VALUE 'ZERO '.
00137  01  FILLER                      PIC  X(23)
00138                              VALUE 'PROGRAM INTERFACE START'.
00139 *    COPY ELCINTF.
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
011812*                   C H A N G E   L O G
011812*
011812* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
011812*-----------------------------------------------------------------
011812*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
011812* EFFECTIVE    NUMBER
011812*-----------------------------------------------------------------
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
011812******************************************************************
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
011812
011812     12  PI-PROCESSOR-CSR-IND            PIC X.
011812         88  PI-PROCESSOR-IS-CSR             VALUE 'Y' 'S'.
011812         88  PI-PROCESSOR-IS-CSR-SUPER       VALUE 'S'.
011812
011812     12  FILLER                          PIC X(3).
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
00140      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
00141          16  PI-CURSOR           PIC S9(08)           COMP.
00142          16  PI-FIRST-CNTL-KEY   PIC  X(10).
00143          16  PI-FIRST-TIME-IND   PIC  X(01).
00144              88  PI-FIRST-TIME               VALUE LOW-VALUES.
00145              88  PI-NOT-FIRST-TIME           VALUE 'N'.
00146          16  PI-LAST-CNTL-KEY    PIC  X(10).
00147          16  PI-LAST-INSERTED-LINE
00148                                  PIC S9(04)           COMP.
00149          16  PI-LAST-MNT-DATE    PIC  X(02).
00150          16  PI-LAST-MNT-DATE-ALPHA
00151                                  PIC  X(08).
00152          16  PI-LAST-MNT-PROCESSOR
010413                                 PIC  X(04).
00154          16  PI-LAST-MNT-TIME    PIC S9(06)           COMP.
00155          16  PI-LAST-MORT-TBL    PIC S9(04)           COMP.
00156          16  PI-LAST-SEQ-NO      PIC S9(04)           COMP.
00157          16  PI-MAINT            PIC  X(01).
00158          16  PI-MODIFICATIONS-MADE-IND
00159                                  PIC  X(01).
00160              88  PI-MODIFICATIONS-MADE       VALUE 'Y'.
00161          16  PI-PASS-SW          PIC  X(01).
00162              88  PI-2ND-TIME-PAST-END        VALUE 'Y'.
00163          16  PI-TABLE-LINE       PIC S9(04)           COMP.
00164          16  PI-TOTAL-MORT-LINES PIC S9(04)           COMP.
00165          16  PI-WORK-TABLE-LINE  PIC S9(04)           COMP.
00166          16  FILLER              PIC X(583).
00167                                  EJECT
00168  01  FILLER                      PIC  X(17)
00169                              VALUE 'PROGRAM MAP START'.
00170 *    COPY EL602S.
       01  EL602AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  RUNDTEL PIC S9(0004) COMP.
           05  RUNDTEF PIC  X(0001).
           05  FILLER REDEFINES RUNDTEF.
               10  RUNDTEA PIC  X(0001).
           05  RUNDTEI PIC  X(0008).
      *    -------------------------------
           05  RUNTIMEL PIC S9(0004) COMP.
           05  RUNTIMEF PIC  X(0001).
           05  FILLER REDEFINES RUNTIMEF.
               10  RUNTIMEA PIC  X(0001).
           05  RUNTIMEI PIC  X(0005).
      *    -------------------------------
           05  COMPANYL PIC S9(0004) COMP.
           05  COMPANYF PIC  X(0001).
           05  FILLER REDEFINES COMPANYF.
               10  COMPANYA PIC  X(0001).
           05  COMPANYI PIC  X(0003).
      *    -------------------------------
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  MAINTBYL PIC S9(0004) COMP.
           05  MAINTBYF PIC  X(0001).
           05  FILLER REDEFINES MAINTBYF.
               10  MAINTBYA PIC  X(0001).
           05  MAINTBYI PIC  X(0004).
      *    -------------------------------
           05  MAINTDTL PIC S9(0004) COMP.
           05  MAINTDTF PIC  X(0001).
           05  FILLER REDEFINES MAINTDTF.
               10  MAINTDTA PIC  X(0001).
           05  MAINTDTI PIC  X(0008).
      *    -------------------------------
           05  MAINTTML PIC S9(0004) COMP.
           05  MAINTTMF PIC  X(0001).
           05  FILLER REDEFINES MAINTTMF.
               10  MAINTTMA PIC  X(0001).
           05  MAINTTMI PIC  X(0005).
      *    -------------------------------
           05  LINSEL1L PIC S9(0004) COMP.
           05  LINSEL1F PIC  X(0001).
           05  FILLER REDEFINES LINSEL1F.
               10  LINSEL1A PIC  X(0001).
           05  LINSEL1I PIC  999.
      *    -------------------------------
           05  LINSEL2L PIC S9(0004) COMP.
           05  LINSEL2F PIC  X(0001).
           05  FILLER REDEFINES LINSEL2F.
               10  LINSEL2A PIC  X(0001).
           05  LINSEL2I PIC  999.
      *    -------------------------------
           05  LINE1L PIC S9(0004) COMP.
           05  LINE1F PIC  X(0001).
           05  FILLER REDEFINES LINE1F.
               10  LINE1A PIC  X(0001).
           05  LINE1I PIC  X(0003).
      *    -------------------------------
           05  TABLE1L PIC S9(0004) COMP.
           05  TABLE1F PIC  X(0001).
           05  FILLER REDEFINES TABLE1F.
               10  TABLE1A PIC  X(0001).
           05  TABLE1I PIC  X(0005).
      *    -------------------------------
           05  TBLTP1L PIC S9(0004) COMP.
           05  TBLTP1F PIC  X(0001).
           05  FILLER REDEFINES TBLTP1F.
               10  TBLTP1A PIC  X(0001).
           05  TBLTP1I PIC  X(0001).
      *    -------------------------------
           05  INTR1L PIC S9(0004) COMP.
           05  INTR1F PIC  X(0001).
           05  FILLER REDEFINES INTR1F.
               10  INTR1A PIC  X(0001).
           05  INTR1I PIC  99999.
      *    -------------------------------
           05  ANAL1L PIC S9(0004) COMP.
           05  ANAL1F PIC  X(0001).
           05  FILLER REDEFINES ANAL1F.
               10  ANAL1A PIC  X(0001).
           05  ANAL1I PIC  X(0002).
      *    -------------------------------
           05  RVADJ1L PIC S9(0004) COMP.
           05  RVADJ1F PIC  X(0001).
           05  FILLER REDEFINES RVADJ1F.
               10  RVADJ1A PIC  X(0001).
           05  RVADJ1I PIC  999999.
      *    -------------------------------
           05  ADJDI1L PIC S9(0004) COMP.
           05  ADJDI1F PIC  X(0001).
           05  FILLER REDEFINES ADJDI1F.
               10  ADJDI1A PIC  X(0001).
           05  ADJDI1I PIC  X(0001).
      *    -------------------------------
           05  JNTFC1L PIC S9(0004) COMP.
           05  JNTFC1F PIC  X(0001).
           05  FILLER REDEFINES JNTFC1F.
               10  JNTFC1A PIC  X(0001).
           05  JNTFC1I PIC  999999.
      *    -------------------------------
           05  JNTCD1L PIC S9(0004) COMP.
           05  JNTCD1F PIC  X(0001).
           05  FILLER REDEFINES JNTCD1F.
               10  JNTCD1A PIC  X(0001).
           05  JNTCD1I PIC  X(0001).
      *    -------------------------------
           05  PCQ1L PIC S9(0004) COMP.
           05  PCQ1F PIC  X(0001).
           05  FILLER REDEFINES PCQ1F.
               10  PCQ1A PIC  X(0001).
           05  PCQ1I PIC  X(0001).
      *    -------------------------------
           05  MORTC1L PIC S9(0004) COMP.
           05  MORTC1F PIC  X(0001).
           05  FILLER REDEFINES MORTC1F.
               10  MORTC1A PIC  X(0001).
           05  MORTC1I PIC  X(0004).
      *    -------------------------------
           05  COMM1L PIC S9(0004) COMP.
           05  COMM1F PIC  X(0001).
           05  FILLER REDEFINES COMM1F.
               10  COMM1A PIC  X(0001).
           05  COMM1I PIC  X(0015).
      *    -------------------------------
           05  LINE2L PIC S9(0004) COMP.
           05  LINE2F PIC  X(0001).
           05  FILLER REDEFINES LINE2F.
               10  LINE2A PIC  X(0001).
           05  LINE2I PIC  X(0003).
      *    -------------------------------
           05  TABLE2L PIC S9(0004) COMP.
           05  TABLE2F PIC  X(0001).
           05  FILLER REDEFINES TABLE2F.
               10  TABLE2A PIC  X(0001).
           05  TABLE2I PIC  X(0005).
      *    -------------------------------
           05  TBLTP2L PIC S9(0004) COMP.
           05  TBLTP2F PIC  X(0001).
           05  FILLER REDEFINES TBLTP2F.
               10  TBLTP2A PIC  X(0001).
           05  TBLTP2I PIC  X(0001).
      *    -------------------------------
           05  INTR2L PIC S9(0004) COMP.
           05  INTR2F PIC  X(0001).
           05  FILLER REDEFINES INTR2F.
               10  INTR2A PIC  X(0001).
           05  INTR2I PIC  99999.
      *    -------------------------------
           05  ANAL2L PIC S9(0004) COMP.
           05  ANAL2F PIC  X(0001).
           05  FILLER REDEFINES ANAL2F.
               10  ANAL2A PIC  X(0001).
           05  ANAL2I PIC  X(0002).
      *    -------------------------------
           05  RVADJ2L PIC S9(0004) COMP.
           05  RVADJ2F PIC  X(0001).
           05  FILLER REDEFINES RVADJ2F.
               10  RVADJ2A PIC  X(0001).
           05  RVADJ2I PIC  999999.
      *    -------------------------------
           05  ADJDI2L PIC S9(0004) COMP.
           05  ADJDI2F PIC  X(0001).
           05  FILLER REDEFINES ADJDI2F.
               10  ADJDI2A PIC  X(0001).
           05  ADJDI2I PIC  X(0001).
      *    -------------------------------
           05  JNTFC2L PIC S9(0004) COMP.
           05  JNTFC2F PIC  X(0001).
           05  FILLER REDEFINES JNTFC2F.
               10  JNTFC2A PIC  X(0001).
           05  JNTFC2I PIC  999999.
      *    -------------------------------
           05  JNTCD2L PIC S9(0004) COMP.
           05  JNTCD2F PIC  X(0001).
           05  FILLER REDEFINES JNTCD2F.
               10  JNTCD2A PIC  X(0001).
           05  JNTCD2I PIC  X(0001).
      *    -------------------------------
           05  PCQ2L PIC S9(0004) COMP.
           05  PCQ2F PIC  X(0001).
           05  FILLER REDEFINES PCQ2F.
               10  PCQ2A PIC  X(0001).
           05  PCQ2I PIC  X(0001).
      *    -------------------------------
           05  MORTC2L PIC S9(0004) COMP.
           05  MORTC2F PIC  X(0001).
           05  FILLER REDEFINES MORTC2F.
               10  MORTC2A PIC  X(0001).
           05  MORTC2I PIC  X(0004).
      *    -------------------------------
           05  COMM2L PIC S9(0004) COMP.
           05  COMM2F PIC  X(0001).
           05  FILLER REDEFINES COMM2F.
               10  COMM2A PIC  X(0001).
           05  COMM2I PIC  X(0015).
      *    -------------------------------
           05  LINE3L PIC S9(0004) COMP.
           05  LINE3F PIC  X(0001).
           05  FILLER REDEFINES LINE3F.
               10  LINE3A PIC  X(0001).
           05  LINE3I PIC  X(0003).
      *    -------------------------------
           05  TABLE3L PIC S9(0004) COMP.
           05  TABLE3F PIC  X(0001).
           05  FILLER REDEFINES TABLE3F.
               10  TABLE3A PIC  X(0001).
           05  TABLE3I PIC  X(0005).
      *    -------------------------------
           05  TBLTP3L PIC S9(0004) COMP.
           05  TBLTP3F PIC  X(0001).
           05  FILLER REDEFINES TBLTP3F.
               10  TBLTP3A PIC  X(0001).
           05  TBLTP3I PIC  X(0001).
      *    -------------------------------
           05  INTR3L PIC S9(0004) COMP.
           05  INTR3F PIC  X(0001).
           05  FILLER REDEFINES INTR3F.
               10  INTR3A PIC  X(0001).
           05  INTR3I PIC  99999.
      *    -------------------------------
           05  ANAL3L PIC S9(0004) COMP.
           05  ANAL3F PIC  X(0001).
           05  FILLER REDEFINES ANAL3F.
               10  ANAL3A PIC  X(0001).
           05  ANAL3I PIC  X(0002).
      *    -------------------------------
           05  RVADJ3L PIC S9(0004) COMP.
           05  RVADJ3F PIC  X(0001).
           05  FILLER REDEFINES RVADJ3F.
               10  RVADJ3A PIC  X(0001).
           05  RVADJ3I PIC  999999.
      *    -------------------------------
           05  ADJDI3L PIC S9(0004) COMP.
           05  ADJDI3F PIC  X(0001).
           05  FILLER REDEFINES ADJDI3F.
               10  ADJDI3A PIC  X(0001).
           05  ADJDI3I PIC  X(0001).
      *    -------------------------------
           05  JNTFC3L PIC S9(0004) COMP.
           05  JNTFC3F PIC  X(0001).
           05  FILLER REDEFINES JNTFC3F.
               10  JNTFC3A PIC  X(0001).
           05  JNTFC3I PIC  999999.
      *    -------------------------------
           05  JNTCD3L PIC S9(0004) COMP.
           05  JNTCD3F PIC  X(0001).
           05  FILLER REDEFINES JNTCD3F.
               10  JNTCD3A PIC  X(0001).
           05  JNTCD3I PIC  X(0001).
      *    -------------------------------
           05  PCQ3L PIC S9(0004) COMP.
           05  PCQ3F PIC  X(0001).
           05  FILLER REDEFINES PCQ3F.
               10  PCQ3A PIC  X(0001).
           05  PCQ3I PIC  X(0001).
      *    -------------------------------
           05  MORTC3L PIC S9(0004) COMP.
           05  MORTC3F PIC  X(0001).
           05  FILLER REDEFINES MORTC3F.
               10  MORTC3A PIC  X(0001).
           05  MORTC3I PIC  X(0004).
      *    -------------------------------
           05  COMM3L PIC S9(0004) COMP.
           05  COMM3F PIC  X(0001).
           05  FILLER REDEFINES COMM3F.
               10  COMM3A PIC  X(0001).
           05  COMM3I PIC  X(0015).
      *    -------------------------------
           05  LINE4L PIC S9(0004) COMP.
           05  LINE4F PIC  X(0001).
           05  FILLER REDEFINES LINE4F.
               10  LINE4A PIC  X(0001).
           05  LINE4I PIC  X(0003).
      *    -------------------------------
           05  TABLE4L PIC S9(0004) COMP.
           05  TABLE4F PIC  X(0001).
           05  FILLER REDEFINES TABLE4F.
               10  TABLE4A PIC  X(0001).
           05  TABLE4I PIC  X(0005).
      *    -------------------------------
           05  TBLTP4L PIC S9(0004) COMP.
           05  TBLTP4F PIC  X(0001).
           05  FILLER REDEFINES TBLTP4F.
               10  TBLTP4A PIC  X(0001).
           05  TBLTP4I PIC  X(0001).
      *    -------------------------------
           05  INTR4L PIC S9(0004) COMP.
           05  INTR4F PIC  X(0001).
           05  FILLER REDEFINES INTR4F.
               10  INTR4A PIC  X(0001).
           05  INTR4I PIC  99999.
      *    -------------------------------
           05  ANAL4L PIC S9(0004) COMP.
           05  ANAL4F PIC  X(0001).
           05  FILLER REDEFINES ANAL4F.
               10  ANAL4A PIC  X(0001).
           05  ANAL4I PIC  X(0002).
      *    -------------------------------
           05  RVADJ4L PIC S9(0004) COMP.
           05  RVADJ4F PIC  X(0001).
           05  FILLER REDEFINES RVADJ4F.
               10  RVADJ4A PIC  X(0001).
           05  RVADJ4I PIC  999999.
      *    -------------------------------
           05  ADJDI4L PIC S9(0004) COMP.
           05  ADJDI4F PIC  X(0001).
           05  FILLER REDEFINES ADJDI4F.
               10  ADJDI4A PIC  X(0001).
           05  ADJDI4I PIC  X(0001).
      *    -------------------------------
           05  JNTFC4L PIC S9(0004) COMP.
           05  JNTFC4F PIC  X(0001).
           05  FILLER REDEFINES JNTFC4F.
               10  JNTFC4A PIC  X(0001).
           05  JNTFC4I PIC  999999.
      *    -------------------------------
           05  JNTCD4L PIC S9(0004) COMP.
           05  JNTCD4F PIC  X(0001).
           05  FILLER REDEFINES JNTCD4F.
               10  JNTCD4A PIC  X(0001).
           05  JNTCD4I PIC  X(0001).
      *    -------------------------------
           05  PCQ4L PIC S9(0004) COMP.
           05  PCQ4F PIC  X(0001).
           05  FILLER REDEFINES PCQ4F.
               10  PCQ4A PIC  X(0001).
           05  PCQ4I PIC  X(0001).
      *    -------------------------------
           05  MORTC4L PIC S9(0004) COMP.
           05  MORTC4F PIC  X(0001).
           05  FILLER REDEFINES MORTC4F.
               10  MORTC4A PIC  X(0001).
           05  MORTC4I PIC  X(0004).
      *    -------------------------------
           05  COMM4L PIC S9(0004) COMP.
           05  COMM4F PIC  X(0001).
           05  FILLER REDEFINES COMM4F.
               10  COMM4A PIC  X(0001).
           05  COMM4I PIC  X(0015).
      *    -------------------------------
           05  LINE5L PIC S9(0004) COMP.
           05  LINE5F PIC  X(0001).
           05  FILLER REDEFINES LINE5F.
               10  LINE5A PIC  X(0001).
           05  LINE5I PIC  X(0003).
      *    -------------------------------
           05  TABLE5L PIC S9(0004) COMP.
           05  TABLE5F PIC  X(0001).
           05  FILLER REDEFINES TABLE5F.
               10  TABLE5A PIC  X(0001).
           05  TABLE5I PIC  X(0005).
      *    -------------------------------
           05  TBLTP5L PIC S9(0004) COMP.
           05  TBLTP5F PIC  X(0001).
           05  FILLER REDEFINES TBLTP5F.
               10  TBLTP5A PIC  X(0001).
           05  TBLTP5I PIC  X(0001).
      *    -------------------------------
           05  INTR5L PIC S9(0004) COMP.
           05  INTR5F PIC  X(0001).
           05  FILLER REDEFINES INTR5F.
               10  INTR5A PIC  X(0001).
           05  INTR5I PIC  99999.
      *    -------------------------------
           05  ANAL5L PIC S9(0004) COMP.
           05  ANAL5F PIC  X(0001).
           05  FILLER REDEFINES ANAL5F.
               10  ANAL5A PIC  X(0001).
           05  ANAL5I PIC  X(0002).
      *    -------------------------------
           05  RVADJ5L PIC S9(0004) COMP.
           05  RVADJ5F PIC  X(0001).
           05  FILLER REDEFINES RVADJ5F.
               10  RVADJ5A PIC  X(0001).
           05  RVADJ5I PIC  999999.
      *    -------------------------------
           05  ADJDI5L PIC S9(0004) COMP.
           05  ADJDI5F PIC  X(0001).
           05  FILLER REDEFINES ADJDI5F.
               10  ADJDI5A PIC  X(0001).
           05  ADJDI5I PIC  X(0001).
      *    -------------------------------
           05  JNTFC5L PIC S9(0004) COMP.
           05  JNTFC5F PIC  X(0001).
           05  FILLER REDEFINES JNTFC5F.
               10  JNTFC5A PIC  X(0001).
           05  JNTFC5I PIC  999999.
      *    -------------------------------
           05  JNTCD5L PIC S9(0004) COMP.
           05  JNTCD5F PIC  X(0001).
           05  FILLER REDEFINES JNTCD5F.
               10  JNTCD5A PIC  X(0001).
           05  JNTCD5I PIC  X(0001).
      *    -------------------------------
           05  PCQ5L PIC S9(0004) COMP.
           05  PCQ5F PIC  X(0001).
           05  FILLER REDEFINES PCQ5F.
               10  PCQ5A PIC  X(0001).
           05  PCQ5I PIC  X(0001).
      *    -------------------------------
           05  MORTC5L PIC S9(0004) COMP.
           05  MORTC5F PIC  X(0001).
           05  FILLER REDEFINES MORTC5F.
               10  MORTC5A PIC  X(0001).
           05  MORTC5I PIC  X(0004).
      *    -------------------------------
           05  COMM5L PIC S9(0004) COMP.
           05  COMM5F PIC  X(0001).
           05  FILLER REDEFINES COMM5F.
               10  COMM5A PIC  X(0001).
           05  COMM5I PIC  X(0015).
      *    -------------------------------
           05  LINE6L PIC S9(0004) COMP.
           05  LINE6F PIC  X(0001).
           05  FILLER REDEFINES LINE6F.
               10  LINE6A PIC  X(0001).
           05  LINE6I PIC  X(0003).
      *    -------------------------------
           05  TABLE6L PIC S9(0004) COMP.
           05  TABLE6F PIC  X(0001).
           05  FILLER REDEFINES TABLE6F.
               10  TABLE6A PIC  X(0001).
           05  TABLE6I PIC  X(0005).
      *    -------------------------------
           05  TBLTP6L PIC S9(0004) COMP.
           05  TBLTP6F PIC  X(0001).
           05  FILLER REDEFINES TBLTP6F.
               10  TBLTP6A PIC  X(0001).
           05  TBLTP6I PIC  X(0001).
      *    -------------------------------
           05  INTR6L PIC S9(0004) COMP.
           05  INTR6F PIC  X(0001).
           05  FILLER REDEFINES INTR6F.
               10  INTR6A PIC  X(0001).
           05  INTR6I PIC  99999.
      *    -------------------------------
           05  ANAL6L PIC S9(0004) COMP.
           05  ANAL6F PIC  X(0001).
           05  FILLER REDEFINES ANAL6F.
               10  ANAL6A PIC  X(0001).
           05  ANAL6I PIC  X(0002).
      *    -------------------------------
           05  RVADJ6L PIC S9(0004) COMP.
           05  RVADJ6F PIC  X(0001).
           05  FILLER REDEFINES RVADJ6F.
               10  RVADJ6A PIC  X(0001).
           05  RVADJ6I PIC  999999.
      *    -------------------------------
           05  ADJDI6L PIC S9(0004) COMP.
           05  ADJDI6F PIC  X(0001).
           05  FILLER REDEFINES ADJDI6F.
               10  ADJDI6A PIC  X(0001).
           05  ADJDI6I PIC  X(0001).
      *    -------------------------------
           05  JNTFC6L PIC S9(0004) COMP.
           05  JNTFC6F PIC  X(0001).
           05  FILLER REDEFINES JNTFC6F.
               10  JNTFC6A PIC  X(0001).
           05  JNTFC6I PIC  999999.
      *    -------------------------------
           05  JNTCD6L PIC S9(0004) COMP.
           05  JNTCD6F PIC  X(0001).
           05  FILLER REDEFINES JNTCD6F.
               10  JNTCD6A PIC  X(0001).
           05  JNTCD6I PIC  X(0001).
      *    -------------------------------
           05  PCQ6L PIC S9(0004) COMP.
           05  PCQ6F PIC  X(0001).
           05  FILLER REDEFINES PCQ6F.
               10  PCQ6A PIC  X(0001).
           05  PCQ6I PIC  X(0001).
      *    -------------------------------
           05  MORTC6L PIC S9(0004) COMP.
           05  MORTC6F PIC  X(0001).
           05  FILLER REDEFINES MORTC6F.
               10  MORTC6A PIC  X(0001).
           05  MORTC6I PIC  X(0004).
      *    -------------------------------
           05  COMM6L PIC S9(0004) COMP.
           05  COMM6F PIC  X(0001).
           05  FILLER REDEFINES COMM6F.
               10  COMM6A PIC  X(0001).
           05  COMM6I PIC  X(0015).
      *    -------------------------------
           05  LINE7L PIC S9(0004) COMP.
           05  LINE7F PIC  X(0001).
           05  FILLER REDEFINES LINE7F.
               10  LINE7A PIC  X(0001).
           05  LINE7I PIC  X(0003).
      *    -------------------------------
           05  TABLE7L PIC S9(0004) COMP.
           05  TABLE7F PIC  X(0001).
           05  FILLER REDEFINES TABLE7F.
               10  TABLE7A PIC  X(0001).
           05  TABLE7I PIC  X(0005).
      *    -------------------------------
           05  TBLTP7L PIC S9(0004) COMP.
           05  TBLTP7F PIC  X(0001).
           05  FILLER REDEFINES TBLTP7F.
               10  TBLTP7A PIC  X(0001).
           05  TBLTP7I PIC  X(0001).
      *    -------------------------------
           05  INTR7L PIC S9(0004) COMP.
           05  INTR7F PIC  X(0001).
           05  FILLER REDEFINES INTR7F.
               10  INTR7A PIC  X(0001).
           05  INTR7I PIC  99999.
      *    -------------------------------
           05  ANAL7L PIC S9(0004) COMP.
           05  ANAL7F PIC  X(0001).
           05  FILLER REDEFINES ANAL7F.
               10  ANAL7A PIC  X(0001).
           05  ANAL7I PIC  X(0002).
      *    -------------------------------
           05  RVADJ7L PIC S9(0004) COMP.
           05  RVADJ7F PIC  X(0001).
           05  FILLER REDEFINES RVADJ7F.
               10  RVADJ7A PIC  X(0001).
           05  RVADJ7I PIC  999999.
      *    -------------------------------
           05  ADJDI7L PIC S9(0004) COMP.
           05  ADJDI7F PIC  X(0001).
           05  FILLER REDEFINES ADJDI7F.
               10  ADJDI7A PIC  X(0001).
           05  ADJDI7I PIC  X(0001).
      *    -------------------------------
           05  JNTFC7L PIC S9(0004) COMP.
           05  JNTFC7F PIC  X(0001).
           05  FILLER REDEFINES JNTFC7F.
               10  JNTFC7A PIC  X(0001).
           05  JNTFC7I PIC  999999.
      *    -------------------------------
           05  JNTCD7L PIC S9(0004) COMP.
           05  JNTCD7F PIC  X(0001).
           05  FILLER REDEFINES JNTCD7F.
               10  JNTCD7A PIC  X(0001).
           05  JNTCD7I PIC  X(0001).
      *    -------------------------------
           05  PCQ7L PIC S9(0004) COMP.
           05  PCQ7F PIC  X(0001).
           05  FILLER REDEFINES PCQ7F.
               10  PCQ7A PIC  X(0001).
           05  PCQ7I PIC  X(0001).
      *    -------------------------------
           05  MORTC7L PIC S9(0004) COMP.
           05  MORTC7F PIC  X(0001).
           05  FILLER REDEFINES MORTC7F.
               10  MORTC7A PIC  X(0001).
           05  MORTC7I PIC  X(0004).
      *    -------------------------------
           05  COMM7L PIC S9(0004) COMP.
           05  COMM7F PIC  X(0001).
           05  FILLER REDEFINES COMM7F.
               10  COMM7A PIC  X(0001).
           05  COMM7I PIC  X(0015).
      *    -------------------------------
           05  LINE8L PIC S9(0004) COMP.
           05  LINE8F PIC  X(0001).
           05  FILLER REDEFINES LINE8F.
               10  LINE8A PIC  X(0001).
           05  LINE8I PIC  X(0003).
      *    -------------------------------
           05  TABLE8L PIC S9(0004) COMP.
           05  TABLE8F PIC  X(0001).
           05  FILLER REDEFINES TABLE8F.
               10  TABLE8A PIC  X(0001).
           05  TABLE8I PIC  X(0005).
      *    -------------------------------
           05  TBLTP8L PIC S9(0004) COMP.
           05  TBLTP8F PIC  X(0001).
           05  FILLER REDEFINES TBLTP8F.
               10  TBLTP8A PIC  X(0001).
           05  TBLTP8I PIC  X(0001).
      *    -------------------------------
           05  INTR8L PIC S9(0004) COMP.
           05  INTR8F PIC  X(0001).
           05  FILLER REDEFINES INTR8F.
               10  INTR8A PIC  X(0001).
           05  INTR8I PIC  99999.
      *    -------------------------------
           05  ANAL8L PIC S9(0004) COMP.
           05  ANAL8F PIC  X(0001).
           05  FILLER REDEFINES ANAL8F.
               10  ANAL8A PIC  X(0001).
           05  ANAL8I PIC  X(0002).
      *    -------------------------------
           05  RVADJ8L PIC S9(0004) COMP.
           05  RVADJ8F PIC  X(0001).
           05  FILLER REDEFINES RVADJ8F.
               10  RVADJ8A PIC  X(0001).
           05  RVADJ8I PIC  999999.
      *    -------------------------------
           05  ADJDI8L PIC S9(0004) COMP.
           05  ADJDI8F PIC  X(0001).
           05  FILLER REDEFINES ADJDI8F.
               10  ADJDI8A PIC  X(0001).
           05  ADJDI8I PIC  X(0001).
      *    -------------------------------
           05  JNTFC8L PIC S9(0004) COMP.
           05  JNTFC8F PIC  X(0001).
           05  FILLER REDEFINES JNTFC8F.
               10  JNTFC8A PIC  X(0001).
           05  JNTFC8I PIC  999999.
      *    -------------------------------
           05  JNTCD8L PIC S9(0004) COMP.
           05  JNTCD8F PIC  X(0001).
           05  FILLER REDEFINES JNTCD8F.
               10  JNTCD8A PIC  X(0001).
           05  JNTCD8I PIC  X(0001).
      *    -------------------------------
           05  PCQ8L PIC S9(0004) COMP.
           05  PCQ8F PIC  X(0001).
           05  FILLER REDEFINES PCQ8F.
               10  PCQ8A PIC  X(0001).
           05  PCQ8I PIC  X(0001).
      *    -------------------------------
           05  MORTC8L PIC S9(0004) COMP.
           05  MORTC8F PIC  X(0001).
           05  FILLER REDEFINES MORTC8F.
               10  MORTC8A PIC  X(0001).
           05  MORTC8I PIC  X(0004).
      *    -------------------------------
           05  COMM8L PIC S9(0004) COMP.
           05  COMM8F PIC  X(0001).
           05  FILLER REDEFINES COMM8F.
               10  COMM8A PIC  X(0001).
           05  COMM8I PIC  X(0015).
      *    -------------------------------
           05  LINE9L PIC S9(0004) COMP.
           05  LINE9F PIC  X(0001).
           05  FILLER REDEFINES LINE9F.
               10  LINE9A PIC  X(0001).
           05  LINE9I PIC  X(0003).
      *    -------------------------------
           05  TABLE9L PIC S9(0004) COMP.
           05  TABLE9F PIC  X(0001).
           05  FILLER REDEFINES TABLE9F.
               10  TABLE9A PIC  X(0001).
           05  TABLE9I PIC  X(0005).
      *    -------------------------------
           05  TBLTP9L PIC S9(0004) COMP.
           05  TBLTP9F PIC  X(0001).
           05  FILLER REDEFINES TBLTP9F.
               10  TBLTP9A PIC  X(0001).
           05  TBLTP9I PIC  X(0001).
      *    -------------------------------
           05  INTR9L PIC S9(0004) COMP.
           05  INTR9F PIC  X(0001).
           05  FILLER REDEFINES INTR9F.
               10  INTR9A PIC  X(0001).
           05  INTR9I PIC  99999.
      *    -------------------------------
           05  ANAL9L PIC S9(0004) COMP.
           05  ANAL9F PIC  X(0001).
           05  FILLER REDEFINES ANAL9F.
               10  ANAL9A PIC  X(0001).
           05  ANAL9I PIC  X(0002).
      *    -------------------------------
           05  RVADJ9L PIC S9(0004) COMP.
           05  RVADJ9F PIC  X(0001).
           05  FILLER REDEFINES RVADJ9F.
               10  RVADJ9A PIC  X(0001).
           05  RVADJ9I PIC  999999.
      *    -------------------------------
           05  ADJDI9L PIC S9(0004) COMP.
           05  ADJDI9F PIC  X(0001).
           05  FILLER REDEFINES ADJDI9F.
               10  ADJDI9A PIC  X(0001).
           05  ADJDI9I PIC  X(0001).
      *    -------------------------------
           05  JNTFC9L PIC S9(0004) COMP.
           05  JNTFC9F PIC  X(0001).
           05  FILLER REDEFINES JNTFC9F.
               10  JNTFC9A PIC  X(0001).
           05  JNTFC9I PIC  999999.
      *    -------------------------------
           05  JNTCD9L PIC S9(0004) COMP.
           05  JNTCD9F PIC  X(0001).
           05  FILLER REDEFINES JNTCD9F.
               10  JNTCD9A PIC  X(0001).
           05  JNTCD9I PIC  X(0001).
      *    -------------------------------
           05  PCQ9L PIC S9(0004) COMP.
           05  PCQ9F PIC  X(0001).
           05  FILLER REDEFINES PCQ9F.
               10  PCQ9A PIC  X(0001).
           05  PCQ9I PIC  X(0001).
      *    -------------------------------
           05  MORTC9L PIC S9(0004) COMP.
           05  MORTC9F PIC  X(0001).
           05  FILLER REDEFINES MORTC9F.
               10  MORTC9A PIC  X(0001).
           05  MORTC9I PIC  X(0004).
      *    -------------------------------
           05  COMM9L PIC S9(0004) COMP.
           05  COMM9F PIC  X(0001).
           05  FILLER REDEFINES COMM9F.
               10  COMM9A PIC  X(0001).
           05  COMM9I PIC  X(0015).
      *    -------------------------------
           05  LINE10L PIC S9(0004) COMP.
           05  LINE10F PIC  X(0001).
           05  FILLER REDEFINES LINE10F.
               10  LINE10A PIC  X(0001).
           05  LINE10I PIC  X(0003).
      *    -------------------------------
           05  TABLE10L PIC S9(0004) COMP.
           05  TABLE10F PIC  X(0001).
           05  FILLER REDEFINES TABLE10F.
               10  TABLE10A PIC  X(0001).
           05  TABLE10I PIC  X(0005).
      *    -------------------------------
           05  TBLTP10L PIC S9(0004) COMP.
           05  TBLTP10F PIC  X(0001).
           05  FILLER REDEFINES TBLTP10F.
               10  TBLTP10A PIC  X(0001).
           05  TBLTP10I PIC  X(0001).
      *    -------------------------------
           05  INTR10L PIC S9(0004) COMP.
           05  INTR10F PIC  X(0001).
           05  FILLER REDEFINES INTR10F.
               10  INTR10A PIC  X(0001).
           05  INTR10I PIC  99999.
      *    -------------------------------
           05  ANAL10L PIC S9(0004) COMP.
           05  ANAL10F PIC  X(0001).
           05  FILLER REDEFINES ANAL10F.
               10  ANAL10A PIC  X(0001).
           05  ANAL10I PIC  X(0002).
      *    -------------------------------
           05  RVADJ10L PIC S9(0004) COMP.
           05  RVADJ10F PIC  X(0001).
           05  FILLER REDEFINES RVADJ10F.
               10  RVADJ10A PIC  X(0001).
           05  RVADJ10I PIC  999999.
      *    -------------------------------
           05  ADJDI10L PIC S9(0004) COMP.
           05  ADJDI10F PIC  X(0001).
           05  FILLER REDEFINES ADJDI10F.
               10  ADJDI10A PIC  X(0001).
           05  ADJDI10I PIC  X(0001).
      *    -------------------------------
           05  JNTFC10L PIC S9(0004) COMP.
           05  JNTFC10F PIC  X(0001).
           05  FILLER REDEFINES JNTFC10F.
               10  JNTFC10A PIC  X(0001).
           05  JNTFC10I PIC  999999.
      *    -------------------------------
           05  JNTCD10L PIC S9(0004) COMP.
           05  JNTCD10F PIC  X(0001).
           05  FILLER REDEFINES JNTCD10F.
               10  JNTCD10A PIC  X(0001).
           05  JNTCD10I PIC  X(0001).
      *    -------------------------------
           05  PCQ10L PIC S9(0004) COMP.
           05  PCQ10F PIC  X(0001).
           05  FILLER REDEFINES PCQ10F.
               10  PCQ10A PIC  X(0001).
           05  PCQ10I PIC  X(0001).
      *    -------------------------------
           05  MORTC10L PIC S9(0004) COMP.
           05  MORTC10F PIC  X(0001).
           05  FILLER REDEFINES MORTC10F.
               10  MORTC10A PIC  X(0001).
           05  MORTC10I PIC  X(0004).
      *    -------------------------------
           05  COMM10L PIC S9(0004) COMP.
           05  COMM10F PIC  X(0001).
           05  FILLER REDEFINES COMM10F.
               10  COMM10A PIC  X(0001).
           05  COMM10I PIC  X(0015).
      *    -------------------------------
           05  LINE11L PIC S9(0004) COMP.
           05  LINE11F PIC  X(0001).
           05  FILLER REDEFINES LINE11F.
               10  LINE11A PIC  X(0001).
           05  LINE11I PIC  X(0003).
      *    -------------------------------
           05  TABLE11L PIC S9(0004) COMP.
           05  TABLE11F PIC  X(0001).
           05  FILLER REDEFINES TABLE11F.
               10  TABLE11A PIC  X(0001).
           05  TABLE11I PIC  X(0005).
      *    -------------------------------
           05  TBLTP11L PIC S9(0004) COMP.
           05  TBLTP11F PIC  X(0001).
           05  FILLER REDEFINES TBLTP11F.
               10  TBLTP11A PIC  X(0001).
           05  TBLTP11I PIC  X(0001).
      *    -------------------------------
           05  INTR11L PIC S9(0004) COMP.
           05  INTR11F PIC  X(0001).
           05  FILLER REDEFINES INTR11F.
               10  INTR11A PIC  X(0001).
           05  INTR11I PIC  99999.
      *    -------------------------------
           05  ANAL11L PIC S9(0004) COMP.
           05  ANAL11F PIC  X(0001).
           05  FILLER REDEFINES ANAL11F.
               10  ANAL11A PIC  X(0001).
           05  ANAL11I PIC  X(0002).
      *    -------------------------------
           05  RVADJ11L PIC S9(0004) COMP.
           05  RVADJ11F PIC  X(0001).
           05  FILLER REDEFINES RVADJ11F.
               10  RVADJ11A PIC  X(0001).
           05  RVADJ11I PIC  999999.
      *    -------------------------------
           05  ADJDI11L PIC S9(0004) COMP.
           05  ADJDI11F PIC  X(0001).
           05  FILLER REDEFINES ADJDI11F.
               10  ADJDI11A PIC  X(0001).
           05  ADJDI11I PIC  X(0001).
      *    -------------------------------
           05  JNTFC11L PIC S9(0004) COMP.
           05  JNTFC11F PIC  X(0001).
           05  FILLER REDEFINES JNTFC11F.
               10  JNTFC11A PIC  X(0001).
           05  JNTFC11I PIC  999999.
      *    -------------------------------
           05  JNTCD11L PIC S9(0004) COMP.
           05  JNTCD11F PIC  X(0001).
           05  FILLER REDEFINES JNTCD11F.
               10  JNTCD11A PIC  X(0001).
           05  JNTCD11I PIC  X(0001).
      *    -------------------------------
           05  PCQ11L PIC S9(0004) COMP.
           05  PCQ11F PIC  X(0001).
           05  FILLER REDEFINES PCQ11F.
               10  PCQ11A PIC  X(0001).
           05  PCQ11I PIC  X(0001).
      *    -------------------------------
           05  MORTC11L PIC S9(0004) COMP.
           05  MORTC11F PIC  X(0001).
           05  FILLER REDEFINES MORTC11F.
               10  MORTC11A PIC  X(0001).
           05  MORTC11I PIC  X(0004).
      *    -------------------------------
           05  COMM11L PIC S9(0004) COMP.
           05  COMM11F PIC  X(0001).
           05  FILLER REDEFINES COMM11F.
               10  COMM11A PIC  X(0001).
           05  COMM11I PIC  X(0015).
      *    -------------------------------
           05  LINE12L PIC S9(0004) COMP.
           05  LINE12F PIC  X(0001).
           05  FILLER REDEFINES LINE12F.
               10  LINE12A PIC  X(0001).
           05  LINE12I PIC  X(0003).
      *    -------------------------------
           05  TABLE12L PIC S9(0004) COMP.
           05  TABLE12F PIC  X(0001).
           05  FILLER REDEFINES TABLE12F.
               10  TABLE12A PIC  X(0001).
           05  TABLE12I PIC  X(0005).
      *    -------------------------------
           05  TBLTP12L PIC S9(0004) COMP.
           05  TBLTP12F PIC  X(0001).
           05  FILLER REDEFINES TBLTP12F.
               10  TBLTP12A PIC  X(0001).
           05  TBLTP12I PIC  X(0001).
      *    -------------------------------
           05  INTR12L PIC S9(0004) COMP.
           05  INTR12F PIC  X(0001).
           05  FILLER REDEFINES INTR12F.
               10  INTR12A PIC  X(0001).
           05  INTR12I PIC  99999.
      *    -------------------------------
           05  ANAL12L PIC S9(0004) COMP.
           05  ANAL12F PIC  X(0001).
           05  FILLER REDEFINES ANAL12F.
               10  ANAL12A PIC  X(0001).
           05  ANAL12I PIC  X(0002).
      *    -------------------------------
           05  RVADJ12L PIC S9(0004) COMP.
           05  RVADJ12F PIC  X(0001).
           05  FILLER REDEFINES RVADJ12F.
               10  RVADJ12A PIC  X(0001).
           05  RVADJ12I PIC  999999.
      *    -------------------------------
           05  ADJDI12L PIC S9(0004) COMP.
           05  ADJDI12F PIC  X(0001).
           05  FILLER REDEFINES ADJDI12F.
               10  ADJDI12A PIC  X(0001).
           05  ADJDI12I PIC  X(0001).
      *    -------------------------------
           05  JNTFC12L PIC S9(0004) COMP.
           05  JNTFC12F PIC  X(0001).
           05  FILLER REDEFINES JNTFC12F.
               10  JNTFC12A PIC  X(0001).
           05  JNTFC12I PIC  999999.
      *    -------------------------------
           05  JNTCD12L PIC S9(0004) COMP.
           05  JNTCD12F PIC  X(0001).
           05  FILLER REDEFINES JNTCD12F.
               10  JNTCD12A PIC  X(0001).
           05  JNTCD12I PIC  X(0001).
      *    -------------------------------
           05  PCQ12L PIC S9(0004) COMP.
           05  PCQ12F PIC  X(0001).
           05  FILLER REDEFINES PCQ12F.
               10  PCQ12A PIC  X(0001).
           05  PCQ12I PIC  X(0001).
      *    -------------------------------
           05  MORTC12L PIC S9(0004) COMP.
           05  MORTC12F PIC  X(0001).
           05  FILLER REDEFINES MORTC12F.
               10  MORTC12A PIC  X(0001).
           05  MORTC12I PIC  X(0004).
      *    -------------------------------
           05  COMM12L PIC S9(0004) COMP.
           05  COMM12F PIC  X(0001).
           05  FILLER REDEFINES COMM12F.
               10  COMM12A PIC  X(0001).
           05  COMM12I PIC  X(0015).
      *    -------------------------------
           05  LINE13L PIC S9(0004) COMP.
           05  LINE13F PIC  X(0001).
           05  FILLER REDEFINES LINE13F.
               10  LINE13A PIC  X(0001).
           05  LINE13I PIC  X(0003).
      *    -------------------------------
           05  TABLE13L PIC S9(0004) COMP.
           05  TABLE13F PIC  X(0001).
           05  FILLER REDEFINES TABLE13F.
               10  TABLE13A PIC  X(0001).
           05  TABLE13I PIC  X(0005).
      *    -------------------------------
           05  TBLTP13L PIC S9(0004) COMP.
           05  TBLTP13F PIC  X(0001).
           05  FILLER REDEFINES TBLTP13F.
               10  TBLTP13A PIC  X(0001).
           05  TBLTP13I PIC  X(0001).
      *    -------------------------------
           05  INTR13L PIC S9(0004) COMP.
           05  INTR13F PIC  X(0001).
           05  FILLER REDEFINES INTR13F.
               10  INTR13A PIC  X(0001).
           05  INTR13I PIC  99999.
      *    -------------------------------
           05  ANAL13L PIC S9(0004) COMP.
           05  ANAL13F PIC  X(0001).
           05  FILLER REDEFINES ANAL13F.
               10  ANAL13A PIC  X(0001).
           05  ANAL13I PIC  X(0002).
      *    -------------------------------
           05  RVADJ13L PIC S9(0004) COMP.
           05  RVADJ13F PIC  X(0001).
           05  FILLER REDEFINES RVADJ13F.
               10  RVADJ13A PIC  X(0001).
           05  RVADJ13I PIC  999999.
      *    -------------------------------
           05  ADJDI13L PIC S9(0004) COMP.
           05  ADJDI13F PIC  X(0001).
           05  FILLER REDEFINES ADJDI13F.
               10  ADJDI13A PIC  X(0001).
           05  ADJDI13I PIC  X(0001).
      *    -------------------------------
           05  JNTFC13L PIC S9(0004) COMP.
           05  JNTFC13F PIC  X(0001).
           05  FILLER REDEFINES JNTFC13F.
               10  JNTFC13A PIC  X(0001).
           05  JNTFC13I PIC  999999.
      *    -------------------------------
           05  JNTCD13L PIC S9(0004) COMP.
           05  JNTCD13F PIC  X(0001).
           05  FILLER REDEFINES JNTCD13F.
               10  JNTCD13A PIC  X(0001).
           05  JNTCD13I PIC  X(0001).
      *    -------------------------------
           05  PCQ13L PIC S9(0004) COMP.
           05  PCQ13F PIC  X(0001).
           05  FILLER REDEFINES PCQ13F.
               10  PCQ13A PIC  X(0001).
           05  PCQ13I PIC  X(0001).
      *    -------------------------------
           05  MORTC13L PIC S9(0004) COMP.
           05  MORTC13F PIC  X(0001).
           05  FILLER REDEFINES MORTC13F.
               10  MORTC13A PIC  X(0001).
           05  MORTC13I PIC  X(0004).
      *    -------------------------------
           05  COMM13L PIC S9(0004) COMP.
           05  COMM13F PIC  X(0001).
           05  FILLER REDEFINES COMM13F.
               10  COMM13A PIC  X(0001).
           05  COMM13I PIC  X(0015).
      *    -------------------------------
           05  LINE14L PIC S9(0004) COMP.
           05  LINE14F PIC  X(0001).
           05  FILLER REDEFINES LINE14F.
               10  LINE14A PIC  X(0001).
           05  LINE14I PIC  X(0003).
      *    -------------------------------
           05  TABLE14L PIC S9(0004) COMP.
           05  TABLE14F PIC  X(0001).
           05  FILLER REDEFINES TABLE14F.
               10  TABLE14A PIC  X(0001).
           05  TABLE14I PIC  X(0005).
      *    -------------------------------
           05  TBLTP14L PIC S9(0004) COMP.
           05  TBLTP14F PIC  X(0001).
           05  FILLER REDEFINES TBLTP14F.
               10  TBLTP14A PIC  X(0001).
           05  TBLTP14I PIC  X(0001).
      *    -------------------------------
           05  INTR14L PIC S9(0004) COMP.
           05  INTR14F PIC  X(0001).
           05  FILLER REDEFINES INTR14F.
               10  INTR14A PIC  X(0001).
           05  INTR14I PIC  99999.
      *    -------------------------------
           05  ANAL14L PIC S9(0004) COMP.
           05  ANAL14F PIC  X(0001).
           05  FILLER REDEFINES ANAL14F.
               10  ANAL14A PIC  X(0001).
           05  ANAL14I PIC  X(0002).
      *    -------------------------------
           05  RVADJ14L PIC S9(0004) COMP.
           05  RVADJ14F PIC  X(0001).
           05  FILLER REDEFINES RVADJ14F.
               10  RVADJ14A PIC  X(0001).
           05  RVADJ14I PIC  999999.
      *    -------------------------------
           05  ADJDI14L PIC S9(0004) COMP.
           05  ADJDI14F PIC  X(0001).
           05  FILLER REDEFINES ADJDI14F.
               10  ADJDI14A PIC  X(0001).
           05  ADJDI14I PIC  X(0001).
      *    -------------------------------
           05  JNTFC14L PIC S9(0004) COMP.
           05  JNTFC14F PIC  X(0001).
           05  FILLER REDEFINES JNTFC14F.
               10  JNTFC14A PIC  X(0001).
           05  JNTFC14I PIC  999999.
      *    -------------------------------
           05  JNTCD14L PIC S9(0004) COMP.
           05  JNTCD14F PIC  X(0001).
           05  FILLER REDEFINES JNTCD14F.
               10  JNTCD14A PIC  X(0001).
           05  JNTCD14I PIC  X(0001).
      *    -------------------------------
           05  PCQ14L PIC S9(0004) COMP.
           05  PCQ14F PIC  X(0001).
           05  FILLER REDEFINES PCQ14F.
               10  PCQ14A PIC  X(0001).
           05  PCQ14I PIC  X(0001).
      *    -------------------------------
           05  MORTC14L PIC S9(0004) COMP.
           05  MORTC14F PIC  X(0001).
           05  FILLER REDEFINES MORTC14F.
               10  MORTC14A PIC  X(0001).
           05  MORTC14I PIC  X(0004).
      *    -------------------------------
           05  COMM14L PIC S9(0004) COMP.
           05  COMM14F PIC  X(0001).
           05  FILLER REDEFINES COMM14F.
               10  COMM14A PIC  X(0001).
           05  COMM14I PIC  X(0015).
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
       01  EL602AO REDEFINES EL602AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPANYO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTTMO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINSEL1O PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINSEL2O PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TABLE1O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TBLTP1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INTR1O PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAL1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RVADJ1O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADJDI1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTFC1O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTCD1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCQ1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MORTC1O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM1O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TABLE2O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TBLTP2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INTR2O PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAL2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RVADJ2O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADJDI2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTFC2O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTCD2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCQ2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MORTC2O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM2O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE3O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TABLE3O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TBLTP3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INTR3O PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAL3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RVADJ3O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADJDI3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTFC3O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTCD3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCQ3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MORTC3O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM3O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE4O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TABLE4O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TBLTP4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INTR4O PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAL4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RVADJ4O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADJDI4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTFC4O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTCD4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCQ4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MORTC4O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM4O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE5O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TABLE5O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TBLTP5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INTR5O PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAL5O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RVADJ5O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADJDI5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTFC5O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTCD5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCQ5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MORTC5O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM5O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE6O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TABLE6O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TBLTP6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INTR6O PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAL6O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RVADJ6O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADJDI6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTFC6O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTCD6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCQ6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MORTC6O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM6O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE7O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TABLE7O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TBLTP7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INTR7O PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAL7O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RVADJ7O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADJDI7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTFC7O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTCD7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCQ7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MORTC7O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM7O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE8O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TABLE8O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TBLTP8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INTR8O PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAL8O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RVADJ8O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADJDI8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTFC8O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTCD8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCQ8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MORTC8O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM8O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE9O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TABLE9O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TBLTP9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INTR9O PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAL9O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RVADJ9O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADJDI9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTFC9O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTCD9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCQ9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MORTC9O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM9O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE10O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TABLE10O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TBLTP10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INTR10O PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAL10O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RVADJ10O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADJDI10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTFC10O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTCD10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCQ10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MORTC10O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM10O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE11O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TABLE11O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TBLTP11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INTR11O PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAL11O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RVADJ11O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADJDI11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTFC11O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTCD11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCQ11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MORTC11O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM11O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE12O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TABLE12O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TBLTP12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INTR12O PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAL12O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RVADJ12O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADJDI12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTFC12O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTCD12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCQ12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MORTC12O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM12O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE13O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TABLE13O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TBLTP13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INTR13O PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAL13O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RVADJ13O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADJDI13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTFC13O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTCD13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCQ13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MORTC13O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM13O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE14O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TABLE14O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TBLTP14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INTR14O PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAL14O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RVADJ14O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADJDI14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTFC14O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTCD14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCQ14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MORTC14O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM14O PIC  X(0015).
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
00171                                  EJECT
00172  01  W-MAP-IN REDEFINES EL602AI.
010413     12  FILLER                  PIC  X(79).
00174      12  W-TABLE-INPUT OCCURS  14  TIMES
00175                              INDEXED BY W-TBLI-NDX
00176                                         W-TBLI-NDX2
00177                                         W-DUPTBL-NDX.
00178          16  W-LINE-L            PIC S9(04)               COMP.
00179          16  W-LINE-A            PIC  X(01).
00180          16  W-LINE-I            PIC  X(03).
00181          16  W-TABLE-INDAT.
00182              20  W-TABLE-L       PIC S9(04)               COMP.
00183              20  W-TABLE-A       PIC  X(01).
00184              20  W-TABLE-I       PIC  X(05).
00185              20  W-TBLTP-L       PIC S9(04)               COMP.
00186              20  W-TBLTP-A       PIC  X(01).
00187              20  W-TBLTP-I       PIC  X(01).
00188              20  W-INTR-L        PIC S9(04)               COMP.
00189              20  W-INTR-A        PIC  X(01).
00190              20  W-INTR-I        PIC  9(01)V9(04).
00191              20  W-ANAL-L        PIC S9(04)               COMP.
00192              20  W-ANAL-A        PIC  X(01).
00193              20  W-ANAL-I        PIC  X(02).
00194              20  W-RSADJ-L       PIC S9(04)               COMP.
00195              20  W-RSADJ-A       PIC  X(01).
00196              20  W-RSADJ-I       PIC  9(02)V9(04).
00197              20  W-ADJDI-L       PIC S9(04)               COMP.
00198              20  W-ADJDI-A       PIC  X(01).
00199              20  W-ADJDI-I       PIC  X(01).
00200              20  W-JNTFC-L       PIC S9(04)               COMP.
00201              20  W-JNTFC-A       PIC  X(01).
00202              20  W-JNTFC-I       PIC  9(02)V9(04).
00203              20  W-JNTCD-L       PIC S9(04)               COMP.
00204              20  W-JNTCD-A       PIC  X(01).
00205              20  W-JNTCD-I       PIC  X(01).
00206              20  W-PCQ-L         PIC S9(04)               COMP.
00207              20  W-PCQ-A         PIC  X(01).
00208              20  W-PCQ-I         PIC  X(01).
00209              20  W-MORTC-L       PIC S9(04)               COMP.
00210              20  W-MORTC-A       PIC  X(01).
00211              20  W-MORTC-I       PIC  X(04).
00212              20  W-COMM-L        PIC S9(04)               COMP.
00213              20  W-COMM-A        PIC  X(01).
00214              20  W-COMM-I        PIC  X(15).
00215  01  W-MAPOUT REDEFINES EL602AI.
00216      12  FILLER                  PIC  X(70).
010413     12  W-LINSEX1-O             PIC  X(03).
00218      12  FILLER                  PIC  X(03).
010413     12  W-LINSEX2-O             PIC  X(03).
00220      12  W-TABLE-OUTPUT OCCURS 14  TIMES
00221                              INDEXED BY W-TBLO-NDX.
00222          16  FILLER              PIC  X(03).
00223          16  W-LINE-O            PIC  X(03).
00224          16  FILLER              PIC  X(03).
00225          16  W-TABLE-O           PIC  X(05).
00226          16  FILLER              PIC  X(03).
00227          16  W-TBLTP-O           PIC  X(01).
00228          16  FILLER              PIC  X(03).
00229          16  W-INTR-O            PIC  .9(04).
00230          16  W-INTR-X-O REDEFINES W-INTR-O
00231                                  PIC  X(05).
00232          16  FILLER              PIC  X(03).
00233          16  W-ANAL-O            PIC  X(02).
00234          16  FILLER              PIC  X(03).
00235          16  W-RSADJ-O           PIC  9(01).9(04).
00236          16  W-RSADJ-X-O REDEFINES W-RSADJ-O
00237                                  PIC  X(06).
00238          16  FILLER              PIC  X(03).
00239          16  W-ADJDI-O           PIC  X(01).
00240          16  FILLER              PIC  X(03).
00241          16  W-JNTFC-O           PIC  9(01).9(04).
00242          16  W-JNTFC-X-O REDEFINES W-JNTFC-O
00243                                  PIC  X(06).
00244          16  FILLER              PIC  X(03).
00245          16  W-JNTCD-O           PIC  X(01).
00246          16  FILLER              PIC  X(03).
00247          16  W-PCQ-O             PIC  X(01).
00248          16  FILLER              PIC  X(03).
00249          16  W-MORTC-O           PIC  X(04).
00250          16  FILLER              PIC  X(03).
00251          16  W-COMM-O            PIC  X(15).
00252                                  EJECT
00253  01  FILLER                      PIC  X(15)
00254                              VALUE 'MORTALITY TABLE'.
00255  01  W-MORTALITY-TABLE.
010413     12  W-MORT-TBL-LINE OCCURS 153 TIMES
00257                           INDEXED BY W-MORT-NDX
00258                                      W-MORT-NDX2.
00259          16  W-TABLE             PIC  X(05).
00260          16  W-TABLE-TYPE        PIC  X(01).
00261              88  W-MT-JOINT                 VALUE 'J'.
00262              88  W-MT-SINGLE                VALUE 'S'.
00263              88  W-MT-COMBINED              VALUE 'C'.
00264              88  W-MT-TYPE-VALID-C          VALUE 'J' 'S'.
00265              88  W-MT-TYPE-VALID-M          VALUE 'J' 'S' 'C'.
00266          16  W-INTEREST          PIC SV9(04)           COMP-3.
00267          16  W-AGE-METHOD        PIC  X(02).
00268              88  W-AGE-LAST                 VALUE 'AL'.
00269              88  W-AGE-NEAR                 VALUE 'AN'.
00270          16  W-RESERVE-ADJUSTMENT
00271                                  PIC S9(01)V9(04)      COMP-3.
00272          16  W-ADJUSTMENT-DIRECTION
00273                                  PIC  X(01).
00274              88  W-MT-MINUS                 VALUE '-'.
00275              88  W-MT-PLUS                  VALUE '+'.
00276          16  W-JOINT-FACTOR      PIC S9(01)V9(04)      COMP-3.
00277          16  W-JOINT-CODE        PIC  X(01).
00278              88  W-MT-VALID-JOINT-CODE      VALUE 'A' 'V'.
00279          16  W-PC-Q              PIC  X(01).
00280              88  W-MT-VALID-PC-Q            VALUE 'Y' 'N' ' '.
00281          16  W-MORTALITY-CODE    PIC  X(04).
00282          16  W-COMMENTS          PIC  X(15).
00283          16  FILLER              PIC  X(14).
00284  01  FILLER REDEFINES W-MORTALITY-TABLE.
010413     12  W-MORT-RECORD    OCCURS 17 TIMES
00286                           INDEXED BY W-MORTR-NDX
00287                                  PIC X(477).
00288                                  EJECT
00289  01  CONSTANT-AREAS.
00290      12  FILLER                  PIC  X(17)
00291                              VALUE 'PROGRAM CONSTANTS'.
00292      12  W-CNTL-LENGTH           PIC S9(04)  VALUE +750   COMP.
00293      12  W-CNTL-JOURNAL-LENGTH   PIC S9(04)  VALUE +773   COMP.
00294      12  W-ITEM                  PIC S9(04)  VALUE +1     COMP.
010413     12  W-MORT-TBL-LENGTH       PIC S9(04)  VALUE +8109  COMP.
00296      12  W-ZERO                  PIC S9(04)  VALUE +0     COMP.
00297
00298      12  W-CNTL-FILE-ID          PIC  X(08)  VALUE 'ELCNTL'.
00299      12  W-GETMAIN-SPACE         PIC  X(01)  VALUE SPACE.
00300      12  W-MAP.
00301          16  FILLER              PIC  X(02)  VALUE 'EL'.
00302          16  W-MAP-NUM           PIC  X(03)  VALUE '602'.
00303          16  FILLER              PIC  X(03)  VALUE 'A'.
00304      12  W-MAPSET                PIC  X(08)  VALUE 'EL602S'.
00305      12  W-THIS-PGM              PIC  X(08)  VALUE 'EL602'.
00306      12  W-TRANSACTION           PIC  X(04)  VALUE 'EXA2'.
00307      12  W-XCTL-005              PIC  X(08)  VALUE 'EL005'.
00308      12  W-XCTL-010              PIC  X(08)  VALUE 'EL010'.
00309      12  W-XCTL-EL126            PIC  X(08)  VALUE 'EL126'.
00310      12  W-XCTL-EL626            PIC  X(08)  VALUE 'EL626'.
00311      12  W-XCTL-EM626            PIC  X(08)  VALUE 'EM626'.
00312      12  W-XCTL-GL800            PIC  X(08)  VALUE 'GL800'.
00313      12  W-LINK-CLDATCV          PIC  X(08)  VALUE 'ELDATCV'.
00314      12  W-LINK-001              PIC  X(08)  VALUE 'EL001'.
00315      12  W-LINK-004              PIC  X(08)  VALUE 'EL004'.
00316                                  EJECT
00317  01  ERROR-MESSAGES.
00318      12  ER-0000                 PIC  X(04)  VALUE '0000'.
00319      12  ER-0004                 PIC  X(04)  VALUE '0004'.
00320      12  ER-0007                 PIC  X(04)  VALUE '0007'.
00321      12  ER-0008                 PIC  X(04)  VALUE '0008'.
00322      12  ER-0023                 PIC  X(04)  VALUE '0023'.
00323      12  ER-0029                 PIC  X(04)  VALUE '0029'.
00324      12  ER-0042                 PIC  X(04)  VALUE '0042'.
00325      12  ER-0043                 PIC  X(04)  VALUE '0043'.
00326      12  ER-0068                 PIC  X(04)  VALUE '0068'.
00327      12  ER-0070                 PIC  X(04)  VALUE '0070'.
00328      12  ER-1698                 PIC  X(04)  VALUE '1698'.
00329      12  ER-1699                 PIC  X(04)  VALUE '1699'.
00330      12  ER-2000                 PIC  X(04)  VALUE '2000'.
00331      12  ER-2002                 PIC  X(04)  VALUE '2002'.
00332      12  ER-2349                 PIC  X(04)  VALUE '2349'.
00333      12  ER-2350                 PIC  X(04)  VALUE '2350'.
00334      12  ER-7008                 PIC  X(04)  VALUE '7008'.
00335      12  ER-7693                 PIC  X(04)  VALUE '7693'.
00336      12  ER-7694                 PIC  X(04)  VALUE '7694'.
00337      12  ER-7695                 PIC  X(04)  VALUE '7695'.
00338      12  ER-7696                 PIC  X(04)  VALUE '7696'.
00339      12  ER-7697                 PIC  X(04)  VALUE '7697'.
00340      12  ER-7698                 PIC  X(04)  VALUE '7698'.
00341      12  ER-7699                 PIC  X(04)  VALUE '7699'.
00342      12  ER-7700                 PIC  X(04)  VALUE '7700'.
00343      12  ER-7701                 PIC  X(04)  VALUE '7701'.
00344      12  ER-7702                 PIC  X(04)  VALUE '7702'.
00345      12  ER-7703                 PIC  X(04)  VALUE '7703'.
00346      12  ER-7704                 PIC  X(04)  VALUE '7704'.
00347      12  ER-7705                 PIC  X(04)  VALUE '7705'.
00348      12  ER-7706                 PIC  X(04)  VALUE '7706'.
00349      12  ER-7707                 PIC  X(04)  VALUE '7707'.
00350      12  ER-7708                 PIC  X(04)  VALUE '7708'.
00351      12  ER-7709                 PIC  X(04)  VALUE '7709'.
00352      12  ER-7710                 PIC  X(04)  VALUE '7710'.
00353      12  ER-7711                 PIC  X(04)  VALUE '7711'.
00354      12  ER-7712                 PIC  X(04)  VALUE '7712'.
00355      12  ER-7713                 PIC  X(04)  VALUE '7713'.
00356      12  ER-7714                 PIC  X(04)  VALUE '7714'.
00357      12  ER-7715                 PIC  X(04)  VALUE '7715'.
00358      12  ER-7716                 PIC  X(04)  VALUE '7716'.
00359      12  ER-7747                 PIC  X(04)  VALUE '7747'.
00360      12  ER-9096                 PIC  X(04)  VALUE '9096'.
00361      12  ER-9097                 PIC  X(04)  VALUE '9097'.
00362      12  ER-9129                 PIC  X(04)  VALUE '9129'.
00363      12  ER-9196                 PIC  X(04)  VALUE '9196'.
00364      12  ER-9299                 PIC  X(04)  VALUE '9299'.
00365                                  EJECT
00366 *    COPY ELCAID.
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
00367  01  FILLER  REDEFINES  DFHAID.
00368      12  FILLER                  PIC  X(08).
00369      12  PF-VALUES               PIC  X(01) OCCURS 2 TIMES.
00370                                  EJECT
00371 *    COPY ELCATTR.
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
00372                                  EJECT
00373 *    COPY ELCDATE.
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
00374                                  EJECT
00375 *    COPY ELCEMIB.
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
00376                                  EJECT
00377 *    COPY ELCJPFX.
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
00378                                  PIC  X(750).
00379                                  EJECT
00380 *    COPY ELCLOGOF.
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
00381                                  EJECT
00382 *    COPY ELCSCTM.
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
00383                                  EJECT
00384 *    COPY ELCSCRTY.
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
00385                                  EJECT
00386 *    COPY MPCSCRT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            MPCSCRT                             *
00004 *                            VMOD=1.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
00007 *        ACQUIRED BY SIGN-ON PROGRAM EL125.                      *
00008 *                                      (MP MORTGAGE PROTECTION)  *
00009 *                                                                *
00010 ******************************************************************
00011 *
00012  01  SECURITY-CONTROL-E.
00013      12  SC-COMM-LENGTH-E             PIC S9(04) VALUE +144 COMP.
00014      12  FILLER                       PIC  X(02) VALUE 'SC'.
00015      12  SC-QUID-KEY.
00016          16  SC-QUID-TERMINAL         PIC  X(04).
00017          16  SC-QUID-SYSTEM           PIC  X(04).
00018      12  SC-ITEM                      PIC S9(04) VALUE +1   COMP.
00019      12  SC-SECURITY-ACCESS-CODE      PIC  X(01).
00020      12  SC-PRODUCER-AUTHORIZED-SW    PIC  X(01).
00021          88 SC-PRODUCER-AUTHORIZED               VALUE ' '.
00022          88 SC-PRODUCER-NOT-AUTHORIZED           VALUE 'N'.
00023      12  SC-MP-CODES.
00024          16  SC-MP-AUTHORIZATION OCCURS 44 TIMES.
00025              20  SC-MP-DISPLAY        PIC  X(01).
00026              20  SC-MP-UPDATE         PIC  X(01).
00027      12  FILLER                       PIC  X(40).
00387                                  EJECT
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
00389
00390  01  DFHCOMMAREA                 PIC  X(01024).
00391
00392 *01  PARMLIST.
00393 *    12  FILLER                  PIC S9(08)               COMP.
00394 *    12  L-CNTL-POINTER          PIC S9(08)               COMP.
00395                                  EJECT
00396 *    COPY ELCCNTL.
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
061511* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
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
011812
011812         16  CF-CSR-IND                         PIC X.
011812         16  FILLER                             PIC X(239).
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
061511         16  CF-ST-VFY-2ND-BENE             PIC X.
061511         16  FILLER                         PIC X(186).
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
00397                                  EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL602' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00399
00400      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00401      MOVE EIBTRMID               TO W-QUID-TERMINAL.
00402
00403      MOVE LOW-VALUES             TO EL602AO.
00404
00405      
      * EXEC CICS HANDLE CONDITION
00406 *        MAPFAIL       (0030-INITIAL-PROCESS)
00407 *        NOTOPEN       (8000-NOT-OPEN)
00408 *        NOTFND        (8010-NOT-FOUND)
00409 *        PGMIDERR      (9700-PGMID-ERROR)
00410 *        ERROR         (9800-ABEND)
00411 *    END-EXEC.
      *    MOVE '"$?JIL.               ! " #00004396' TO DFHEIV0
           MOVE X'22243F4A494C2E2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303034333936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00412
00413  0005-GET-CURRENT-DATE.
00414
00415      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00416      MOVE '5'                    TO DC-OPTION-CODE.
00417      PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
00418      MOVE DC-GREG-DATE-1-EDIT    TO W-SAVE-DATE.
00419      MOVE DC-BIN-DATE-1          TO W-SAVE-BIN-DATE.
00420
00421  0010-SET-UP-ERROR-PROCESS.
00422
00423      MOVE 2                      TO EMI-NUMBER-OF-LINES.
00424      MOVE 2                      TO EMI-SWITCH2.
00425
00426  0015-START-SECURITY-PROCESS.
00427
00428      IF  EIBCALEN = ZERO
00429          MOVE UNACCESS-MSG       TO LOGOFF-MSG
00430          PERFORM 8300-SEND-TEXT THRU 8300-EXIT
00431          GO TO 9000-RETURN-TRANS.
00432                                  EJECT
00433  0025-UPDATE-CALL-HIER.
00434
00435      IF  PI-CALLING-PROGRAM NOT = W-THIS-PGM
00436
00437          IF  PI-RETURN-TO-PROGRAM NOT EQUAL W-THIS-PGM
00438              MOVE PI-SAVED-PROGRAM-5
00439                                  TO PI-SAVED-PROGRAM-6
00440              MOVE PI-SAVED-PROGRAM-4
00441                                  TO PI-SAVED-PROGRAM-5
00442              MOVE PI-SAVED-PROGRAM-3
00443                                  TO PI-SAVED-PROGRAM-4
00444              MOVE PI-SAVED-PROGRAM-2
00445                                  TO PI-SAVED-PROGRAM-3
00446              MOVE PI-SAVED-PROGRAM-1
00447                                  TO PI-SAVED-PROGRAM-2
00448              MOVE PI-RETURN-TO-PROGRAM
00449                                  TO PI-SAVED-PROGRAM-1
00450              MOVE PI-CALLING-PROGRAM
00451                                  TO PI-RETURN-TO-PROGRAM
00452              MOVE W-THIS-PGM     TO PI-CALLING-PROGRAM
00453              MOVE LOW-VALUES     TO PI-PROGRAM-WORK-AREA
00454              MOVE ZEROS          TO PI-WORK-TABLE-LINE
00455                                     PI-TABLE-LINE
00456              MOVE -14            TO PI-LAST-MORT-TBL
00457              MOVE SPACES         TO PI-PASS-SW
00458
00459          ELSE
00460              MOVE PI-RETURN-TO-PROGRAM
00461                                  TO PI-CALLING-PROGRAM
00462              MOVE PI-SAVED-PROGRAM-1
00463                                  TO PI-RETURN-TO-PROGRAM
00464              MOVE PI-SAVED-PROGRAM-2
00465                                  TO PI-SAVED-PROGRAM-1
00466              MOVE PI-SAVED-PROGRAM-3
00467                                  TO PI-SAVED-PROGRAM-2
00468              MOVE PI-SAVED-PROGRAM-4
00469                                  TO PI-SAVED-PROGRAM-3
00470              MOVE PI-SAVED-PROGRAM-5
00471                                  TO PI-SAVED-PROGRAM-4
00472              MOVE PI-SAVED-PROGRAM-6
00473                                  TO PI-SAVED-PROGRAM-5
00474              MOVE SPACES         TO PI-SAVED-PROGRAM-6.
00475
00476      IF  EIBTRNID EQUAL W-TRANSACTION
00477
00478          IF  NOT DISPLAY-CAP
00479              MOVE PI-RETURN-TO-PROGRAM
00480                                  TO W-CALL-PGM
00481              
      * EXEC CICS XCTL
00482 *                PROGRAM  (W-CALL-PGM)
00483 *                COMMAREA (PROGRAM-INTERFACE-BLOCK)
00484 *                LENGTH   (PI-COMM-LENGTH)
00485 *            END-EXEC
      *    MOVE '.$C                   %   #00004472' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CALL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00486
00487          ELSE
00488              IF  EIBAID = DFHCLEAR
00489                  MOVE PI-RETURN-TO-PROGRAM
00490                                  TO W-CALL-PGM
00491                  GO TO 9400-XCTL
00492
00493              ELSE
00494                  GO TO 0035-RECEIVE.
00495
00496  0030-INITIAL-PROCESS.
00497
00498      PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT.
00499      GO TO 0700-SET-UP-WORKING-MORT-TBL.
00500
00501  0035-RECEIVE.
00502
00503      IF  EIBAID = DFHPA1
00504             OR
00505          EIBAID = DFHPA2
00506             OR
00507          EIBAID = DFHPA3
00508          MOVE ER-0008            TO EMI-ERROR
00509          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00510          MOVE -1                 TO MAINTL
00511          GO TO 8200-SEND-DATAONLY.
00512
00513      
      * EXEC CICS RECEIVE
00514 *        MAP (W-MAP)
00515 *        MAPSET (W-MAPSET)
00516 *        INTO (EL602AI)
00517 *    END-EXEC.
           MOVE LENGTH OF
            EL602AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00004504' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAP, 
                 EL602AI, 
                 DFHEIV11, 
                 W-MAPSET, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00518
00519      PERFORM 0200-RECOVER-TEMP-STORAGE THRU 0200-EXIT.
00520
00521                                  EJECT
00522  0040-CHECK-PF-FIELD-KEY.
00523
00524      IF  ENTERPFL GREATER ZERO
00525          PERFORM 0500-TRANSFORM-PF-FIELD THRU 0500-EXIT.
00526
00527      IF  EIBAID = DFHPF23
00528          MOVE W-XCTL-005         TO W-CALL-PGM
00529          MOVE EIBAID             TO PI-ENTRY-CD-1
00530          GO TO 9400-XCTL.
00531
00532      IF  EIBAID = DFHPF24
00533          GO TO 8090-RETURN-MAIN-MENU.
00534
00535      IF  EIBAID = DFHPF12
00536          MOVE W-XCTL-010         TO W-CALL-PGM
00537          GO TO 9400-XCTL.
00538
00539      IF  EIBAID = DFHPF1
00540          GO TO 1000-GET-NEXT-14-TABLES.
00541
00542      IF  EIBAID = DFHPF2
00543          GO TO 1050-GET-LAST-14-TABLES.
00544
00545      IF  EIBAID = DFHPF3
00546          PERFORM 0800-RECREATE-MORT-RECORDS THRU 0800-EXIT
00547          MOVE ER-7707            TO EMI-ERROR
00548          MOVE -1                 TO MAINTL
00549          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00550          MOVE LOW-VALUES         TO PI-PROGRAM-WORK-AREA
00551          MOVE ZEROS              TO PI-WORK-TABLE-LINE
00552                                     PI-TABLE-LINE
00553          MOVE -14                TO PI-LAST-MORT-TBL
00554          MOVE SPACES             TO PI-PASS-SW
00555          GO TO 0700-SET-UP-WORKING-MORT-TBL.
00556
00557      IF  EIBAID NOT = DFHENTER
00558              AND
00559          EIBAID NOT = DFHPF11
00560          MOVE ER-0008            TO EMI-ERROR
00561          MOVE -1                 TO MAINTL
00562          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00563          GO TO 8200-SEND-DATAONLY.
00564
00565  0100-MAINLINE.
00566
00567      GO TO 1100-PROCESS-INPUT.
00568                                  EJECT
00569  0200-RECOVER-TEMP-STORAGE.
00570
00571      
      * EXEC CICS HANDLE CONDITION
00572 *        NOTFND  (0240-TS-PROBLEMS)
00573 *        QIDERR  (0240-TS-PROBLEMS)
00574 *        INVREQ  (0240-TS-PROBLEMS)
00575 *        ITEMERR (0240-TS-PROBLEMS)
00576 *    END-EXEC.
      *    MOVE '"$IN8<                ! # #00004562' TO DFHEIV0
           MOVE X'2224494E383C202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034353632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00577
00578      
      * EXEC CICS READQ TS
00579 *        QUEUE   (W-QUID-KEY)
00580 *        INTO    (W-MORTALITY-TABLE)
00581 *        LENGTH  (W-MORT-TBL-LENGTH)
00582 *        ITEM    (W-ITEM)
00583 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00004569' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-QUID-KEY, 
                 W-MORTALITY-TABLE, 
                 W-MORT-TBL-LENGTH, 
                 W-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00584
00585  0200-EXIT.
00586      EXIT.
00587
00588  0240-TS-PROBLEMS.
00589
00590      MOVE ER-9129                TO EMI-ERROR.
00591      MOVE -1                     TO MAINTL.
00592      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00593      GO TO 8100-SEND-INITIAL-MAP.
00594
00595  0240-EXIT.
00596      EXIT.
00597
00598  0250-CREATE-TEMP-STORAGE.
00599
00600      
      * EXEC CICS HANDLE CONDITION
00601 *        QIDERR  (0250-CONTINUE)
00602 *        INVREQ  (0240-TS-PROBLEMS)
00603 *        ITEMERR (0240-TS-PROBLEMS)
00604 *    END-EXEC.
      *    MOVE '"$N8<                 ! $ #00004591' TO DFHEIV0
           MOVE X'22244E383C20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303034353931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00605
00606      
      * EXEC CICS DELETEQ TS
00607 *        QUEUE   (W-QUID-KEY)
00608 *    END-EXEC.
      *    MOVE '*&                    #   #00004597' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-QUID-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00609
00610  0250-CONTINUE.
00611
00612      MOVE EIBCPOSN           TO PI-CURSOR
00613
00614      
      * EXEC CICS WRITEQ TS
00615 *        QUEUE   (W-QUID-KEY)
00616 *        FROM    (W-MORTALITY-TABLE)
00617 *        LENGTH  (W-MORT-TBL-LENGTH)
00618 *        ITEM    (W-ITEM)
00619 *    END-EXEC.
      *    MOVE '*" I                  ''   #00004605' TO DFHEIV0
           MOVE X'2A2220492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-QUID-KEY, 
                 W-MORTALITY-TABLE, 
                 W-MORT-TBL-LENGTH, 
                 W-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00620
00621  0250-EXIT.
00622      EXIT.
00623                                  EJECT
00624  0500-TRANSFORM-PF-FIELD.
00625
00626      IF  EIBAID NOT = DFHENTER
00627          MOVE ER-0004            TO EMI-ERROR
00628          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00629          MOVE -1                 TO ENTERPFL
00630          GO TO 8200-SEND-DATAONLY.
00631
00632      IF  ENTERPFI NOT NUMERIC
00633          MOVE ER-0029            TO EMI-ERROR
00634          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00635          MOVE -1                 TO ENTERPFL
00636          GO TO 8200-SEND-DATAONLY.
00637
00638      IF  ENTERPFI LESS 1 OR GREATER 24
00639          MOVE ER-0029            TO EMI-ERROR
00640          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00641          MOVE -1                 TO ENTERPFL
00642          GO TO 8200-SEND-DATAONLY.
00643
00644      MOVE ENTERPFI               TO W-CHECK-PFKEYS.
00645      MOVE PF-VALUES (W-CHECK-PFKEYS)
00646                                  TO EIBAID.
00647
00648  0500-EXIT.
00649      EXIT.
00650                                  EJECT
00651  0700-SET-UP-WORKING-MORT-TBL.
00652
00653      MOVE SPACES                 TO W-WORKING-CNTL-KEY.
00654      MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.
00655      MOVE '7'                    TO W-CNTL-RECORD-TYPE.
00656      MOVE +0                     TO W-CNTL-SEQUENCE-NO
00657                                     PI-TOTAL-MORT-LINES.
00658      SET W-MORT-NDX              TO W-ZERO.
00659      MOVE LOW-VALUES             TO W-MORTALITY-TABLE.
00660
00661      
      * EXEC CICS HANDLE CONDITION
00662 *        ENDFILE   (0790-LAST-RECORD-PROCESSED)
00663 *        NOTFND    (0790-LAST-RECORD-PROCESSED)
00664 *        NOTOPEN   (8000-NOT-OPEN)
00665 *    END-EXEC.
      *    MOVE '"$''IJ                 ! % #00004652' TO DFHEIV0
           MOVE X'222427494A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034363532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00666
00667      PERFORM 0720-GET-MORT-CNTL-RECORD THRU 0720-EXIT
00668              UNTIL
00669          W-LAST-MORT-RECORD-READ.
00670
00671      IF  PI-TOTAL-MORT-LINES NOT GREATER THAN +0
00672          MOVE 'A'                TO MAINTO
00673          MOVE AL-UANON           TO MAINTA
00674                                     PI-MAINT
00675          MOVE -1                 TO W-TABLE-L (1)
00676          GO TO 8100-SEND-INITIAL-MAP
00677
00678      ELSE
00679          MOVE 'S'                TO MAINTI
00680                                     PI-MAINT
00681          GO TO 1500-PROCESS-SHOWS.
00682                                  EJECT
00683  0720-GET-MORT-CNTL-RECORD.
00684
00685      
      * EXEC CICS READ
00686 *        DATASET  ('ELCNTL')
00687 *        SET      (ADDRESS OF CONTROL-FILE)
00688 *        RIDFLD   (W-WORKING-CNTL-KEY)
00689 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00004676' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-WORKING-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00690
00691      IF  CF-COMPANY-ID NOT EQUAL PI-COMPANY-ID
00692              OR
00693          NOT CF-MORTALITY-MASTER
00694          MOVE 'Y'                TO W-LAST-MORT-READ-IND
00695          GO TO 0720-EXIT.
00696
00697      IF  PI-FIRST-CNTL-KEY EQUAL LOW-VALUES
00698          MOVE W-WORKING-CNTL-KEY TO PI-FIRST-CNTL-KEY
00699
00700          MOVE CF-LAST-MAINT-DT   TO DC-BIN-DATE-1
00701          MOVE ' '                TO DC-OPTION-CODE
00702          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00703          MOVE DC-GREG-DATE-1-EDIT
00704                                  TO PI-LAST-MNT-DATE-ALPHA
00705
00706          MOVE CF-LAST-MAINT-BY   TO PI-LAST-MNT-PROCESSOR
00707          MOVE CF-LAST-MAINT-DT   TO PI-LAST-MNT-DATE
00708          MOVE CF-LAST-MAINT-HHMMSS
00709                                  TO PI-LAST-MNT-TIME.
00710
00711      MOVE W-WORKING-CNTL-KEY     TO PI-LAST-CNTL-KEY.
00712
00713      PERFORM 0780-MOVE-MORT-TBL-LINE THRU 0780-EXIT
00714              VARYING
00715          CF-MORT-NDX FROM 1 BY 1
00716              UNTIL
00717          CF-MORT-NDX GREATER THAN +9
00718              OR
00719          CF-MORT-TABLE-LINE (CF-MORT-NDX) EQUAL LOW-VALUES
00720              OR
00721          CF-MORT-TABLE (CF-MORT-NDX) NOT GREATER THAN
00722              SPACES
00723
00724      ADD +1                      TO W-CNTL-SEQUENCE-NO.
00725
00726  0720-EXIT.
00727      EXIT.
00728
00729  0780-MOVE-MORT-TBL-LINE.
00730
00731      SET W-MORT-NDX UP BY +1.
00732
010413     IF  W-MORT-NDX GREATER THAN +153
00734          MOVE ER-7693            TO EMI-ERROR
00735          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00736          MOVE -1                 TO ENTERPFL
00737          GO TO 8200-SEND-DATAONLY.
00738
00739      ADD +1                      TO PI-TOTAL-MORT-LINES.
00740
00741      MOVE CF-MORT-TABLE (CF-MORT-NDX)
00742                                  TO W-TABLE (W-MORT-NDX).
00743      MOVE CF-MORT-TABLE-TYPE (CF-MORT-NDX)
00744                                  TO W-TABLE-TYPE (W-MORT-NDX).
00745      MOVE CF-MORT-INTEREST (CF-MORT-NDX)
00746                                  TO W-INTEREST (W-MORT-NDX).
00747      MOVE CF-MORT-AGE-METHOD (CF-MORT-NDX)
00748                                  TO W-AGE-METHOD (W-MORT-NDX).
00749      MOVE CF-MORT-RESERVE-ADJUSTMENT (CF-MORT-NDX)
00750          TO W-RESERVE-ADJUSTMENT (W-MORT-NDX).
00751      MOVE CF-MORT-ADJUSTMENT-DIRECTION (CF-MORT-NDX)
00752          TO W-ADJUSTMENT-DIRECTION (W-MORT-NDX).
00753      MOVE CF-MORT-JOINT-FACTOR (CF-MORT-NDX)
00754          TO W-JOINT-FACTOR (W-MORT-NDX).
00755      MOVE CF-MORT-JOINT-CODE (CF-MORT-NDX)
00756                                  TO W-JOINT-CODE (W-MORT-NDX).
00757      MOVE CF-MORT-PC-Q (CF-MORT-NDX)
00758                                  TO W-PC-Q (W-MORT-NDX).
00759      MOVE CF-MORT-TABLE-CODE (CF-MORT-NDX)
00760          TO W-MORTALITY-CODE (W-MORT-NDX).
00761      MOVE CF-MORT-COMMENTS (CF-MORT-NDX)
00762                                  TO W-COMMENTS (W-MORT-NDX).
00763
00764  0780-EXIT.
00765      EXIT.
00766                                  EJECT
00767  0790-LAST-RECORD-PROCESSED.
00768
00769      MOVE 'Y'                    TO W-LAST-MORT-READ-IND.
00770      GO TO 0720-EXIT.
00771
00772  0790-EXIT.
00773      EXIT.
00774                                  EJECT
00775  0800-RECREATE-MORT-RECORDS.
00776
00777      IF  PI-TOTAL-MORT-LINES GREATER THAN ZERO
00778          PERFORM 0850-SORT-BY-MORT-CODE THRU 0850-EXIT.
00779
00780      IF  PI-FIRST-CNTL-KEY GREATER THAN LOW-VALUES
00781          MOVE PI-FIRST-CNTL-KEY  TO W-WORKING-CNTL-KEY
00782          PERFORM 0820-DELETE-RECORDS THRU 0820-EXIT
00783          MOVE LOW-VALUES         TO PI-FIRST-CNTL-KEY
00784                                     PI-LAST-CNTL-KEY.
00785
00786      IF  PI-TOTAL-MORT-LINES NOT GREATER THAN +0
00787          MOVE 'N'                TO PI-FIRST-TIME-IND
00788          MOVE LOW-VALUES         TO PI-MODIFICATIONS-MADE-IND
00789          GO TO 0800-EXIT.
00790
00791      MOVE SPACES                 TO W-WORKING-CNTL-KEY.
00792      MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.
00793      MOVE '7'                    TO W-CNTL-RECORD-TYPE.
00794      MOVE +0                     TO W-CNTL-SEQUENCE-NO.
00795
00796      
      * EXEC CICS HANDLE CONDITION
00797 *        DUPREC (0810-DUPLICATE-RECORD)
00798 *    END-EXEC.
      *    MOVE '"$%                   ! & #00004787' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034373837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00799
00800      
      * EXEC CICS GETMAIN
00801 *        SET    (ADDRESS OF CONTROL-FILE)
00802 *        LENGTH (W-CNTL-LENGTH)
00803 *    END-EXEC.
      *    MOVE '," L                  $   #00004791' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 W-CNTL-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00804
00805  0800-CONTINUE.
00806
00807      PERFORM 0840-CREATE-NEW-RECORDS THRU 0840-EXIT
00808              VARYING
00809          W-MORTR-NDX FROM 1 BY 1
00810              UNTIL
010413         W-MORTR-NDX GREATER THAN +17
00812              OR
00813          W-MORT-RECORD (W-MORTR-NDX) EQUAL LOW-VALUES.
00814
00815      MOVE LOW-VALUES             TO PI-PROGRAM-WORK-AREA
00816                                     W-MORTALITY-TABLE.
00817      MOVE ZEROS                  TO PI-WORK-TABLE-LINE
00818                                     PI-TABLE-LINE.
00819      MOVE -14                    TO PI-LAST-MORT-TBL.
00820      MOVE SPACES                 TO PI-PASS-SW.
00821      GO TO 0700-SET-UP-WORKING-MORT-TBL.
00822
00823  0800-EXIT.
00824      EXIT.
00825                                  EJECT
00826  0810-DUPLICATE-RECORD.
00827
00828      MOVE ER-7714                TO EMI-ERROR.
00829      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00830      MOVE -1                     TO MAINTL.
00831      GO TO 8200-SEND-DATAONLY.
00832
00833  0810-EXIT.
00834      EXIT.
00835                                  EJECT
00836  0820-DELETE-RECORDS.
00837
00838      
      * EXEC CICS DELETE
00839 *        DATASET  (W-CNTL-FILE-ID)
00840 *        RIDFLD   (W-WORKING-CNTL-KEY)
00841 *    END-EXEC.
      *    MOVE '&(  R                 &   #00004829' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 W-WORKING-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00842
00843      ADD +1                      TO W-CNTL-SEQUENCE-NO.
00844
00845      IF  W-WORKING-CNTL-KEY NOT GREATER THAN PI-LAST-CNTL-KEY
00846          GO TO 0820-DELETE-RECORDS.
00847
00848  0820-EXIT.
00849      EXIT.
00850                                  EJECT
00851  0840-CREATE-NEW-RECORDS.
00852
00853      MOVE W-MORT-RECORD (W-MORTR-NDX)
00854                                  TO CF-MORTALITY-MASTER-REC.
00855      MOVE W-WORKING-CNTL-KEY     TO CF-CONTROL-PRIMARY.
00856      MOVE 'CF'                   TO CF-RECORD-ID.
00857      MOVE CONTROL-FILE           TO JP-RECORD-AREA.
00858      MOVE PI-LAST-MNT-PROCESSOR  TO CF-LAST-MAINT-BY.
00859      MOVE PI-LAST-MNT-DATE       TO CF-LAST-MAINT-DT.
00860      MOVE PI-LAST-MNT-TIME       TO CF-LAST-MAINT-HHMMSS.
00861
00862      
      * EXEC CICS WRITE
00863 *        DATASET  (W-CNTL-FILE-ID)
00864 *        FROM     (CONTROL-FILE)
00865 *        RIDFLD   (W-WORKING-CNTL-KEY)
00866 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004853' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 W-WORKING-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00867
00868      MOVE 'A'                    TO JP-RECORD-TYPE.
00869      PERFORM 8400-LOG-JOURNAL THRU 8400-EXIT.
00870      ADD +1                      TO W-CNTL-SEQUENCE-NO.
00871
00872  0840-EXIT.
00873      EXIT.
00874                                  EJECT
00875  0850-SORT-BY-MORT-CODE.
00876
00877      COMPUTE W-NEXT-TABLE-LINE = PI-TOTAL-MORT-LINES.
00878
00879  0850-CONTINUE.
00880
00881      SET W-MORT-NDX2            TO +1.
00882      MOVE SPACES                TO W-EXCHANGE-MADE-IND.
00883
00884      PERFORM 0855-BUBBLE THRU 0855-EXIT
00885              VARYING
00886          W-MORT-NDX FROM +1 BY +1
00887              UNTIL
00888          W-MORT-NDX EQUAL W-NEXT-TABLE-LINE.
00889
00890      IF  W-EXCHANGE-MADE
00891          SUBTRACT +1 FROM W-NEXT-TABLE-LINE
00892          GO TO 0850-CONTINUE.
00893
00894  0850-EXIT.
00895      EXIT.
00896                                  EJECT
00897  0855-BUBBLE.
00898
00899      SET W-MORT-NDX2 UP BY +1.
00900
00901      IF  W-MORTALITY-CODE (W-MORT-NDX) EQUAL LOW-VALUES
00902              OR
00903          W-MORTALITY-CODE (W-MORT-NDX) GREATER THAN
00904          W-MORTALITY-CODE (W-MORT-NDX2)
00905          MOVE W-MORT-TBL-LINE (W-MORT-NDX)
00906                                  TO W-TEMP-LINE
00907          MOVE W-MORT-TBL-LINE (W-MORT-NDX2)
00908              TO W-MORT-TBL-LINE (W-MORT-NDX)
00909          MOVE W-TEMP-LINE
00910              TO W-MORT-TBL-LINE (W-MORT-NDX2)
00911          MOVE 'Y'                TO W-EXCHANGE-MADE-IND.
00912
00913  0855-EXIT.
00914      EXIT.
00915                                  EJECT
00916  1000-GET-NEXT-14-TABLES.
00917
00918      MOVE 'S'                    TO MAINTI
00919                                     PI-MAINT.
00920
00921      IF  LINSEL1L GREATER THAN ZEROS
00922          PERFORM 1020-LINSEL1-EDIT THRU 1020-EXIT
00923
00924          IF  W-TABLE (LINSEL1I) GREATER THAN LOW-VALUES
00925              COMPUTE PI-LAST-MORT-TBL = LINSEL1I - 1
00926              MOVE ZEROS          TO LINSEL1O
00927              MOVE AL-UNNOF       TO LINSEL1A
00928
00929          ELSE
00930              COMPUTE PI-LAST-MORT-TBL =
00931                  PI-TOTAL-MORT-LINES - 1
00932
00933      ELSE
00934          ADD +14                 TO PI-LAST-MORT-TBL
00935
010413         IF  PI-LAST-MORT-TBL GREATER THAN +153
00937              MOVE +0             TO PI-LAST-MORT-TBL.
00938
00939
00940  1000-START-LOOP.
00941
00942      COMPUTE PI-WORK-TABLE-LINE
00943          = PI-LAST-MORT-TBL + 1.
00944
00945      SET W-MORT-NDX              TO PI-WORK-TABLE-LINE.
00946
00947      IF  W-TABLE (W-MORT-NDX) EQUAL LOW-VALUES
00948
00949          IF  PI-TOTAL-MORT-LINES GREATER THAN +1
00950
00951              IF  PI-2ND-TIME-PAST-END
00952                  MOVE +0         TO PI-LAST-MORT-TBL
00953                  MOVE ' '        TO PI-PASS-SW
00954                  GO TO 1000-START-LOOP
00955
00956              ELSE
00957                  COMPUTE PI-LAST-MORT-TBL =
00958                      PI-TOTAL-MORT-LINES - 1
00959                  MOVE 'Y'        TO PI-PASS-SW
00960                  GO TO 1000-START-LOOP
00961
00962          ELSE
00963              MOVE ER-1699        TO EMI-ERROR
00964              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00965              MOVE -1             TO MAINTL
00966              GO TO 8200-SEND-DATAONLY
00967
00968      ELSE
00969          PERFORM 5000-MOVE-MORT-TBL-DATA THRU 5000-EXIT
00970          GO TO 8200-SEND-DATAONLY.
00971
00972  1000-EXIT.
00973      EXIT.
00974                                  EJECT
00975  1020-LINSEL1-EDIT.
00976
00977      IF  LINSEL1L GREATER THAN +0
00978
00979          IF  LINSEL1I NUMERIC
00980                 AND
00981              LINSEL1I NOT LESS THAN +01
00982                 AND
010413             LINSEL1I NOT GREATER THAN +153
00984              NEXT SENTENCE
00985
00986          ELSE
00987              MOVE ER-7706        TO EMI-ERROR
00988              MOVE -1             TO LINSEL1L
00989              MOVE AL-UNNON       TO LINSEL1A
00990              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00991              GO TO 8200-SEND-DATAONLY.
00992
00993  1020-EXIT.
00994      EXIT.
00995                                  EJECT
00996  1050-GET-LAST-14-TABLES.
00997
00998      MOVE 'S'                    TO MAINTI
00999                                     PI-MAINT.
01000
01001      SET W-MORT-NDX              TO +1.
01002
01003      IF  W-TABLE (W-MORT-NDX) EQUAL LOW-VALUES
01004          MOVE ER-1698            TO EMI-ERROR
01005          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01006          MOVE -1                 TO MAINTL
01007          GO TO 8200-SEND-DATAONLY.
01008
01009      IF  LINSEL1L GREATER THAN ZEROS
01010          PERFORM 1020-LINSEL1-EDIT THRU 1020-EXIT
01011
01012          IF  W-TABLE (LINSEL1I) GREATER THAN LOW-VALUES
01013              COMPUTE PI-LAST-MORT-TBL = LINSEL1I - 15
01014              MOVE ZEROS          TO LINSEL1O
01015              MOVE AL-UANOF       TO LINSEL1A
01016
01017          ELSE
01018              COMPUTE PI-LAST-MORT-TBL =
01019                  PI-TOTAL-MORT-LINES - 15
01020              PERFORM 1060-GET-LAST-MORT-TBL THRU 1060-EXIT
01021
01022      ELSE
01023          SUBTRACT +14 FROM PI-LAST-MORT-TBL.
01024
01025      IF  PI-LAST-MORT-TBL LESS THAN +0
01026
01027          IF  PI-LAST-MORT-TBL GREATER THAN -15
01028              MOVE +0             TO PI-LAST-MORT-TBL
01029
01030          ELSE
01031              COMPUTE PI-LAST-MORT-TBL =
01032                  PI-TOTAL-MORT-LINES - 15.
01033
01034      COMPUTE PI-WORK-TABLE-LINE
01035          = PI-LAST-MORT-TBL + 1.
01036
01037      PERFORM 5000-MOVE-MORT-TBL-DATA THRU 5000-EXIT.
01038
01039      GO TO 8200-SEND-DATAONLY.
01040
01041  1050-EXIT.
01042      EXIT.
01043                                  EJECT
01044  1060-GET-LAST-MORT-TBL.
01045
01046      SET W-MORT-NDX              TO +1
01047      SEARCH W-MORT-TBL-LINE
01048          VARYING W-MORT-NDX
01049
01050          AT END
01051              MOVE +0             TO PI-LAST-MORT-TBL
01052
01053          WHEN
01054              W-TABLE (W-MORT-NDX) EQUAL LOW-VALUES
01055              SET PI-LAST-MORT-TBL
01056                                  TO W-MORT-NDX
01057              SUBTRACT +15 FROM PI-LAST-MORT-TBL.
01058
01059  1060-EXIT.
01060      EXIT.
01061                                  EJECT
01062  1100-PROCESS-INPUT.
01063
01064      IF  MAINTL EQUAL ZEROS
01065
01066          IF  PI-MAINT GREATER THAN LOW-VALUES
01067              MOVE PI-MAINT       TO MAINTI
01068
01069          ELSE
01070              IF  PI-LAST-MORT-TBL GREATER +0
01071                  MOVE 'S'        TO PI-MAINT
01072                                     MAINTI
01073                  COMPUTE PI-WORK-TABLE-LINE
01074                      = PI-LAST-MORT-TBL + 1
01075
01076              ELSE
01077                  MOVE 'S'        TO PI-MAINT
01078                                     MAINTI
01079                  MOVE +0         TO PI-LAST-MORT-TBL
01080                  MOVE +1         TO PI-WORK-TABLE-LINE
01081
01082      ELSE
01083          MOVE MAINTI             TO W-CHECK-MAINT
01084          MOVE AL-UANON           TO MAINTA
01085
01086          IF  NOT W-VALID-OPTION
01087              MOVE -1             TO MAINTL
01088              MOVE AL-UABON       TO MAINTA
01089              MOVE ER-0023        TO EMI-ERROR
01090              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01091              GO TO 8200-SEND-DATAONLY
01092
01093          ELSE
01094              MOVE MAINTI         TO PI-MAINT.
01095
01096      IF  MAINTI EQUAL 'S'
01097          PERFORM 1500-PROCESS-SHOWS THRU 1500-EXIT
01098
01099      ELSE
01100          IF  NOT MODIFY-CAP
01101              MOVE 'UPDATE'       TO SM-READ
01102              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
01103              MOVE ER-0070        TO EMI-ERROR
01104              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01105              MOVE AL-UANON       TO MAINTA
01106              MOVE -1             TO MAINTL
01107              GO TO 8200-SEND-DATAONLY
01108
01109          ELSE
01110              IF  MAINTI EQUAL 'A'
01111                  GO TO 1200-PROCESS-ADDS
01112
01113              ELSE
01114                  IF  MAINTI EQUAL 'C'
01115                      GO TO 1300-PROCESS-CHANGES
01116
01117                  ELSE
01118                      IF  MAINTI EQUAL 'D'
01119                          GO TO 1400-PROCESS-DELETE.
01120
01121  1100-EXIT.
01122      EXIT.
01123                                  EJECT
01124  1200-PROCESS-ADDS.
01125
01126      IF  PI-TOTAL-MORT-LINES NOT EQUAL +0
01127          MOVE 'C'                TO MAINTI
01128          GO TO 1300-PROCESS-CHANGES.
01129
01130      PERFORM 2000-EDIT-MORT-TBL-DATA THRU 2000-EXIT
01131              VARYING
01132          W-TBLI-NDX FROM +1 BY +1
01133              UNTIL
01134          W-TBLI-NDX GREATER THAN +14
01135              OR
01136          W-TABLE-I (W-TBLI-NDX) EQUAL LOW-VALUES.
01137
01138      IF  NOT EMI-NO-ERRORS
01139              AND
01140          EMI-FATAL-CTR GREATER THAN ZEROS
01141          GO TO 8200-SEND-DATAONLY.
01142
01143      MOVE +0                     TO PI-LAST-MORT-TBL.
01144      SET W-MORT-NDX              TO PI-LAST-MORT-TBL.
01145
01146      PERFORM 2500-UPDATE-WORKING-MORT-TBL THRU 2500-EXIT
01147              VARYING
01148          W-TBLI-NDX FROM +1 BY +1
01149              UNTIL
01150          W-TBLI-NDX GREATER THAN +14
01151              OR
01152          W-TABLE-I (W-TBLI-NDX) EQUAL LOW-VALUES.
01153
01154      IF  NOT EMI-NO-ERRORS
01155              AND
01156          EMI-FATAL-CTR GREATER THAN ZEROS
01157          GO TO 8200-SEND-DATAONLY.
01158
01159      IF  W-MORT-NDX GREATER THAN PI-TOTAL-MORT-LINES
01160          SET PI-TOTAL-MORT-LINES TO W-MORT-NDX.
01161
01162      MOVE ER-7708                TO EMI-ERROR.
01163      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01164
01165      IF  PI-MODIFICATIONS-MADE
01166          
      * EXEC CICS ASKTIME END-EXEC
      *    MOVE '0"                    "   #00005157' TO DFHEIV0
           MOVE X'302220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01167          MOVE EIBTIME            TO PI-LAST-MNT-TIME
01168          MOVE PI-PROCESSOR-ID    TO PI-LAST-MNT-PROCESSOR
01169          MOVE W-SAVE-BIN-DATE    TO PI-LAST-MNT-DATE
01170          MOVE W-SAVE-DATE        TO PI-LAST-MNT-DATE-ALPHA.
01171
01172      GO TO 1500-PROCESS-SHOWS.
01173
01174  1200-EXIT.
01175      EXIT.
01176                                  EJECT
01177  1300-PROCESS-CHANGES.
01178
01179      PERFORM 2000-EDIT-MORT-TBL-DATA THRU 2000-EXIT
01180              VARYING
01181          W-TBLI-NDX FROM +1 BY +1
01182              UNTIL
01183          W-TBLI-NDX GREATER THAN +14.
01184
01185      IF  NOT EMI-NO-ERRORS
01186              AND
01187          EMI-FATAL-CTR GREATER THAN ZEROS
01188          GO TO 8200-SEND-DATAONLY.
01189
01190      IF  PI-LAST-MORT-TBL LESS THAN +0
01191          MOVE +0                 TO PI-LAST-MORT-TBL.
01192
01193      SET W-MORT-NDX              TO PI-LAST-MORT-TBL.
01194
01195      PERFORM 2500-UPDATE-WORKING-MORT-TBL THRU 2500-EXIT
01196              VARYING
01197          W-TBLI-NDX FROM +1 BY +1
01198              UNTIL
01199          W-TBLI-NDX GREATER THAN +14.
01200
01201      IF  NOT EMI-NO-ERRORS
01202              AND
01203          EMI-FATAL-CTR GREATER THAN ZEROS
01204          GO TO 8200-SEND-DATAONLY.
01205
01206      IF  W-MORT-NDX GREATER THAN PI-TOTAL-MORT-LINES
01207          SET PI-TOTAL-MORT-LINES TO W-MORT-NDX.
01208
01209      MOVE ER-7708                TO EMI-ERROR.
01210      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01211
01212      IF  PI-MODIFICATIONS-MADE
01213          
      * EXEC CICS ASKTIME END-EXEC
      *    MOVE '0"                    "   #00005204' TO DFHEIV0
           MOVE X'302220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01214          MOVE EIBTIME            TO PI-LAST-MNT-TIME
01215          MOVE PI-PROCESSOR-ID    TO PI-LAST-MNT-PROCESSOR
01216          MOVE W-SAVE-BIN-DATE    TO PI-LAST-MNT-DATE
01217          MOVE W-SAVE-DATE        TO PI-LAST-MNT-DATE-ALPHA.
01218
01219      GO TO 1500-PROCESS-SHOWS.
01220
01221  1300-EXIT.
01222      EXIT.
01223                                  EJECT
01224  1400-PROCESS-DELETE.
01225
01226      PERFORM 1420-CHECK-REQUEST THRU 1420-EXIT.
01227
01228      IF  NOT EMI-NO-ERRORS
01229              AND
01230          EMI-FATAL-CTR GREATER THAN ZEROS
01231          GO TO 8200-SEND-DATAONLY.
01232
01233      PERFORM 1440-DO-DELETE THRU 1440-EXIT.
01234
01235      IF  NOT EMI-NO-ERRORS
01236              AND
01237          EMI-FATAL-CTR GREATER THAN ZEROS
01238          GO TO 8200-SEND-DATAONLY.
01239
01240      MOVE ER-7708                TO EMI-ERROR.
01241      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01242
01243      IF  PI-MODIFICATIONS-MADE
01244          
      * EXEC CICS ASKTIME END-EXEC.
      *    MOVE '0"                    "   #00005235' TO DFHEIV0
           MOVE X'302220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01245          MOVE EIBTIME            TO PI-LAST-MNT-TIME
01246          MOVE PI-PROCESSOR-ID    TO PI-LAST-MNT-PROCESSOR
01247          MOVE W-SAVE-BIN-DATE    TO PI-LAST-MNT-DATE
01248          MOVE W-SAVE-DATE        TO PI-LAST-MNT-DATE-ALPHA.
01249
01250      GO TO 1500-PROCESS-SHOWS.
01251
01252  1400-EXIT.
01253      EXIT.
01254                                  EJECT
01255  1420-CHECK-REQUEST.
01256
01257      IF  LINSEL1L NOT GREATER THAN ZEROS
01258          MOVE ER-7706            TO EMI-ERROR
01259          MOVE -1                 TO LINSEL1L
01260          MOVE AL-UABON           TO LINSEL1A
01261          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01262          GO TO 1420-EXIT.
01263
01264      
      * EXEC CICS BIF DEEDIT
01265 *        FIELD   (LINSEL1I)
01266 *        LENGTH  (2)
01267 *        END-EXEC
           MOVE 2
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005255' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINSEL1I, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01268
01269      COMPUTE W-NEXT-TABLE-LINE
01270          = PI-LAST-MORT-TBL + 15
01271
01272      IF  LINSEL1I NUMERIC
01273             AND
01274          LINSEL1I NOT LESS THAN +01
01275             AND
010413         LINSEL1I NOT GREATER THAN +153
01277             AND
01278          LINSEL1I GREATER THAN PI-LAST-MORT-TBL
01279             AND
01280          LINSEL1I LESS THAN W-NEXT-TABLE-LINE
01281          SET W-TBLI-NDX      TO LINSEL1I
01282          SET W-TBLI-NDX DOWN BY PI-LAST-MORT-TBL
01283
01284          IF  W-TABLE-I (W-TBLI-NDX) EQUAL LOW-VALUES
01285              MOVE ER-9196    TO EMI-ERROR
01286              MOVE -1         TO LINSEL1L
01287              MOVE AL-UABON   TO LINSEL1A
01288              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01289
01290          ELSE
01291              NEXT SENTENCE
01292
01293      ELSE
01294          MOVE ER-7694        TO EMI-ERROR
01295          MOVE -1             TO LINSEL1L
01296          MOVE AL-UABON       TO LINSEL1A
01297          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01298
01299      IF  LINSEL2L NOT GREATER THAN ZEROS
01300          COMPUTE W-WORK-CTR = LINSEL1I + 1
01301          GO TO 1420-EXIT.
01302
01303      
      * EXEC CICS BIF DEEDIT
01304 *        FIELD   (LINSEL2I)
01305 *        LENGTH  (2)
01306 *        END-EXEC
           MOVE 2
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005294' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINSEL2I, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01307
01308      IF  LINSEL2I NUMERIC
01309             AND
01310          LINSEL2I NOT LESS THAN +01
01311             AND
010413         LINSEL2I NOT GREATER THAN +153
01313             AND
01314          LINSEL2I NOT LESS THAN LINSEL1I
01315             AND
01316          LINSEL2I NOT GREATER THAN PI-TOTAL-MORT-LINES
01317          COMPUTE W-WORK-CTR = LINSEL2I + 1
01318
01319      ELSE
01320          MOVE ER-7716        TO EMI-ERROR
01321          MOVE -1             TO LINSEL2L
01322          MOVE AL-UABON       TO LINSEL2A
01323          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01324
01325  1420-EXIT.
01326      EXIT.
01327                                  EJECT
01328  1440-DO-DELETE.
01329
01330      SET W-MORT-NDX              TO LINSEL1I.
01331      SET W-MORT-NDX2             TO W-WORK-CTR.
01332
01333      COMPUTE PI-TOTAL-MORT-LINES
01334          = PI-TOTAL-MORT-LINES - (W-WORK-CTR - LINSEL1I).
01335
01336      PERFORM 1460-BUBBLE-TABLES THRU 1460-EXIT
01337              VARYING
01338          W-MORT-NDX FROM W-MORT-NDX BY +1
01339              UNTIL
01340          W-MORT-NDX GREATER THAN PI-TOTAL-MORT-LINES.
01341
01342      PERFORM 1470-CLEAN-REST THRU 1470-EXIT
01343              VARYING
01344          W-MORT-NDX FROM W-MORT-NDX BY +1
01345              UNTIL
01346          W-MORT-NDX GREATER THAN W-WORK-CTR.
01347
01348      MOVE 'Y'                    TO PI-MODIFICATIONS-MADE-IND.
01349
01350  1440-EXIT.
01351      EXIT.
01352                                  EJECT
01353  1460-BUBBLE-TABLES.
01354
01355      MOVE W-MORT-TBL-LINE (W-MORT-NDX2)
01356          TO W-MORT-TBL-LINE (W-MORT-NDX).
01357      MOVE LOW-VALUES
01358          TO W-MORT-TBL-LINE (W-MORT-NDX2).
01359      SET W-MORT-NDX2 UP BY +1.
01360
01361  1460-EXIT.
01362      EXIT.
01363
01364  1470-CLEAN-REST.
01365
01366      MOVE LOW-VALUES
01367          TO W-MORT-TBL-LINE (W-MORT-NDX).
01368
01369  1470-EXIT.
01370      EXIT.
01371                                  EJECT
01372  1500-PROCESS-SHOWS.
01373
01374      PERFORM 5000-MOVE-MORT-TBL-DATA THRU 5000-EXIT.
01375
01376      GO TO 8100-SEND-INITIAL-MAP.
01377
01378  1500-EXIT.
01379      EXIT.
01380                                  EJECT
01381  2000-EDIT-MORT-TBL-DATA.
01382
01383      IF  W-TABLE-INDAT (W-TBLI-NDX) EQUAL LOW-VALUES
01384          GO TO 2000-EXIT.
01385
01386      MOVE +0                     TO W-HOLD-INTEREST
01387                                     W-HOLD-RESERVE-ADJ
01388                                     W-HOLD-JOINT-FACTOR.
01389
01390      PERFORM 2020-EDIT-TABLE                THRU 2020-EXIT.
01391      PERFORM 2030-EDIT-TABLE-TYPE           THRU 2030-EXIT.
01392      PERFORM 2040-EDIT-INTEREST             THRU 2040-EXIT.
01393      PERFORM 2050-EDIT-AGE-METHOD           THRU 2050-EXIT.
01394      PERFORM 2060-EDIT-RESERVE-ADJUSTMENT   THRU 2060-EXIT.
01395      PERFORM 2070-EDIT-ADJUSTMENT-DIRECTION THRU 2070-EXIT.
01396      PERFORM 2080-EDIT-JOINT-FACTOR         THRU 2080-EXIT.
01397      PERFORM 2090-EDIT-JOINT-CDE            THRU 2090-EXIT.
01398      PERFORM 2100-EDIT-PC-Q                 THRU 2100-EXIT.
01399      PERFORM 2110-EDIT-MORT-CDE             THRU 2110-EXIT.
01400      PERFORM 2120-EDIT-COMMENTS             THRU 2120-EXIT.
01401
01402  2000-EXIT.
01403      EXIT.
01404                                  EJECT
01405  2020-EDIT-TABLE.
01406
01407       IF  W-TABLE-L (W-TBLI-NDX) GREATER THAN ZEROS
01408           MOVE W-TABLE-I (W-TBLI-NDX)
01409                                  TO W-SUPPORTED-TABLES-IND
01410
01411           IF  W-TABLE-SUPPORTED
01412               MOVE AL-UANON      TO W-TABLE-A (W-TBLI-NDX)
01413           ELSE
01414               MOVE ER-7695       TO EMI-ERROR
01415               MOVE AL-UABON      TO W-TABLE-A (W-TBLI-NDX)
01416               MOVE -1            TO W-TABLE-L (W-TBLI-NDX)
01417               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01418       ELSE
01419           MOVE ER-7712           TO EMI-ERROR
01420           MOVE AL-UABON          TO W-TABLE-A (W-TBLI-NDX)
01421           MOVE -1                TO W-TABLE-L (W-TBLI-NDX)
01422           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01423
01424      GO TO 2020-EXIT.
01425
01426  2020-EXIT.
01427      EXIT.
01428                                  EJECT
01429  2030-EDIT-TABLE-TYPE.
01430
01431      IF  W-NULL-ENTRY
01432          MOVE AL-UANON          TO W-TBLTP-A (W-TBLI-NDX)
01433          MOVE SPACES            TO W-TBLTP-I (W-TBLI-NDX)
01434          GO TO 2030-EXIT.
01435
01436      IF  W-TBLTP-L (W-TBLI-NDX) GREATER THAN +0
01437          MOVE W-TBLTP-I (W-TBLI-NDX)
01438                                  TO W-VALID-TYPE-IND
01439
01440          IF  MORTGAGE-SESSION
01441                  AND
01442              W-TYPE-VALID-M
01443                  OR
01444              W-TYPE-VALID-C
01445              MOVE AL-UANON       TO W-TBLTP-A (W-TBLI-NDX)
01446              GO TO 2030-EXIT.
01447
01448      MOVE ER-7697                TO EMI-ERROR
01449      MOVE -1                     TO W-TBLTP-L (W-TBLI-NDX)
01450      MOVE AL-UABON               TO W-TBLTP-A (W-TBLI-NDX)
01451      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01452
01453  2030-EXIT.
01454      EXIT.
01455                                  EJECT
01456  2040-EDIT-INTEREST.
01457
01458      IF  W-NULL-ENTRY
01459          MOVE AL-UANON          TO W-INTR-A (W-TBLI-NDX)
01460          MOVE 1.0000            TO W-INTR-I (W-TBLI-NDX)
01461          GO TO 2040-EXIT.
01462
01463      IF  W-INTR-L (W-TBLI-NDX) GREATER THAN +0
01464          
      * EXEC CICS BIF DEEDIT
01465 *            FIELD   (W-INTR-I (W-TBLI-NDX))
01466 *            LENGTH  (5)
01467 *            END-EXEC
           MOVE 5
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005455' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-INTR-I(W-TBLI-NDX), 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01468
01469          IF  W-INTR-I (W-TBLI-NDX) NUMERIC
01470              MOVE W-INTR-I(W-TBLI-NDX)
01471                                  TO W-HOLD-INTEREST
01472              MOVE AL-UNNON       TO W-INTR-A (W-TBLI-NDX)
01473              GO TO 2040-EXIT.
01474
01475      MOVE ER-7698                TO EMI-ERROR
01476      MOVE -1                     TO W-INTR-L (W-TBLI-NDX)
01477      MOVE AL-UNBON               TO W-INTR-A (W-TBLI-NDX)
01478      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01479
01480  2040-EXIT.
01481      EXIT.
01482                                  EJECT
01483  2050-EDIT-AGE-METHOD.
01484
01485      IF  W-NULL-ENTRY
01486          MOVE AL-UANON          TO W-ANAL-A (W-TBLI-NDX)
01487          MOVE SPACES            TO W-ANAL-I (W-TBLI-NDX)
01488          GO TO 2050-EXIT.
01489
01490      IF  W-ANAL-L (W-TBLI-NDX) GREATER THAN +0
01491          IF  W-ANAL-I (W-TBLI-NDX) EQUAL 'AL' OR 'AN'
01492              MOVE AL-UANON       TO W-ANAL-A (W-TBLI-NDX)
01493              GO TO 2050-CHECK-COMPANY.
01494
01495      MOVE ER-7699                TO EMI-ERROR.
01496      MOVE -1                     TO W-ANAL-L (W-TBLI-NDX).
01497      MOVE AL-UABON               TO W-ANAL-A (W-TBLI-NDX).
01498      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01499      GO TO 2050-EXIT.
01500
01501  2050-CHECK-COMPANY.
01502
01503      
      * EXEC CICS HANDLE CONDITION
01504 *        ENDFILE   (2051-MASTER-CNTL-NOT-FOUND)
01505 *        NOTFND    (2051-MASTER-CNTL-NOT-FOUND)
01506 *    END-EXEC.
      *    MOVE '"$''I                  ! '' #00005494' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303035343934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01507
01508      MOVE SPACES                 TO W-WORKING-CNTL-KEY.
01509      MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.
01510      MOVE '1'                    TO W-CNTL-RECORD-TYPE.
01511      MOVE +0                     TO W-CNTL-SEQUENCE-NO.
01512
01513      
      * EXEC CICS READ
01514 *        DATASET(W-CNTL-FILE-ID)
01515 *        SET    (ADDRESS OF CONTROL-FILE)
01516 *        RIDFLD (W-WORKING-CNTL-KEY)
01517 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005504' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-WORKING-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01518
01519      IF  W-ANAL-I (W-TBLI-NDX) EQUAL 'AN'
01520               AND
01521          CF-USE-ALL-AGE-LAST
01522               OR
01523          W-ANAL-I (W-TBLI-NDX) EQUAL 'AL'
01524               AND
01525          CF-USE-ALL-AGE-NEAR
01526          MOVE ER-7715            TO EMI-ERROR
01527          MOVE -1                 TO W-ANAL-L (W-TBLI-NDX)
01528          MOVE AL-UABON           TO W-ANAL-A (W-TBLI-NDX)
01529          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01530          GO TO 2050-EXIT
01531
01532      ELSE
01533          MOVE AL-UANON           TO W-ANAL-A (W-TBLI-NDX).
01534
01535  2050-EXIT.
01536      EXIT.
01537                                  EJECT
01538  2051-MASTER-CNTL-NOT-FOUND.
01539
01540      MOVE ER-9299                TO EMI-ERROR.
01541      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01542      MOVE -1                     TO ENTERPFL.
01543      GO TO 8200-SEND-DATAONLY.
01544
01545  2051-EXIT.
01546      EXIT.
01547                                  EJECT
01548  2060-EDIT-RESERVE-ADJUSTMENT.
01549
01550      IF  W-NULL-ENTRY
01551          MOVE AL-UANON          TO W-RSADJ-A (W-TBLI-NDX)
01552          MOVE 1.0000            TO W-RSADJ-I (W-TBLI-NDX)
01553          GO TO 2060-EXIT.
01554
01555      IF  W-RSADJ-L (W-TBLI-NDX) GREATER THAN +0
01556          
      * EXEC CICS BIF DEEDIT
01557 *            FIELD   (W-RSADJ-I (W-TBLI-NDX))
01558 *            LENGTH  (6)
01559 *            END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005547' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-RSADJ-I(W-TBLI-NDX), 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01560
01561          IF  W-RSADJ-I (W-TBLI-NDX) NUMERIC
01562              MOVE AL-UNNON       TO W-RSADJ-A (W-TBLI-NDX)
01563
01564              IF  W-RSADJ-I (W-TBLI-NDX) EQUAL ZEROS
01565                  MOVE +1.0       TO W-RSADJ-I (W-TBLI-NDX)
01566                                     W-HOLD-RESERVE-ADJ
01567                  GO TO 2060-EXIT
01568
01569              ELSE
01570                  MOVE W-RSADJ-I (W-TBLI-NDX)
01571                                  TO W-HOLD-RESERVE-ADJ
01572                  GO TO 2060-EXIT.
01573
01574      MOVE ER-7700                TO EMI-ERROR.
01575      MOVE -1                     TO W-RSADJ-L (W-TBLI-NDX).
01576      MOVE AL-UNBON               TO W-RSADJ-A (W-TBLI-NDX).
01577      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01578
01579  2060-EXIT.
01580      EXIT.
01581                                  EJECT
01582  2070-EDIT-ADJUSTMENT-DIRECTION.
01583
01584      IF  W-NULL-ENTRY
01585          MOVE AL-UANON          TO W-ADJDI-A (W-TBLI-NDX)
01586          MOVE SPACES            TO W-ADJDI-I (W-TBLI-NDX)
01587          GO TO 2070-EXIT.
01588
01589      IF  W-ADJDI-L (W-TBLI-NDX) GREATER THAN +0
01590          IF  W-ADJDI-I (W-TBLI-NDX) EQUAL '+' OR '-' OR
01591                                           SPACES     OR
01592                                           LOW-VALUES
01593              MOVE AL-UANON   TO W-ADJDI-A (W-TBLI-NDX)
01594
01595          ELSE
01596              MOVE ER-7701    TO EMI-ERROR
01597              MOVE -1         TO W-ADJDI-L (W-TBLI-NDX)
01598              MOVE AL-UABON   TO W-ADJDI-A (W-TBLI-NDX)
01599              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01600
01601  2070-EXIT.
01602      EXIT.
01603                                  EJECT
01604  2080-EDIT-JOINT-FACTOR.
01605
01606      IF  W-NULL-ENTRY
01607          MOVE AL-UANON          TO W-JNTFC-A (W-TBLI-NDX)
01608          MOVE 1.0000            TO W-JNTFC-I (W-TBLI-NDX)
01609          GO TO 2080-EXIT.
01610
01611      IF  W-JNTFC-L (W-TBLI-NDX) GREATER THAN +0
01612          
      * EXEC CICS BIF DEEDIT
01613 *            FIELD   (W-JNTFC-I (W-TBLI-NDX))
01614 *            LENGTH  (6)
01615 *            END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005603' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-JNTFC-I(W-TBLI-NDX), 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01616
01617          IF  W-JNTFC-I (W-TBLI-NDX) NOT NUMERIC
01618              MOVE ER-2000        TO EMI-ERROR
01619              MOVE -1             TO W-JNTFC-L (W-TBLI-NDX)
01620              MOVE AL-UNBON       TO W-JNTFC-A (W-TBLI-NDX)
01621              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01622              GO TO 2080-EXIT
01623
01624          ELSE
01625              MOVE W-JNTFC-I (W-TBLI-NDX)
01626                                  TO W-HOLD-JOINT-FACTOR
01627
01628      ELSE
01629          MOVE +1.0               TO W-HOLD-JOINT-FACTOR
01630                                     W-JNTFC-I (W-TBLI-NDX).
01631
01632      IF  W-TBLTP-I (W-TBLI-NDX) EQUAL 'J'
01633              OR
01634          W-TBLTP-I (W-TBLI-NDX) EQUAL 'C'
01635
01636          IF  W-HOLD-JOINT-FACTOR NOT EQUAL +1
01637              MOVE ER-2000        TO EMI-ERROR
01638              MOVE -1             TO W-JNTFC-L (W-TBLI-NDX)
01639              MOVE AL-UNBON       TO W-JNTFC-A (W-TBLI-NDX)
01640              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01641              GO TO 2080-EXIT
01642
01643          ELSE
01644              MOVE AL-UNNON
01645                          TO W-JNTFC-A (W-TBLI-NDX)
01646              GO TO 2080-EXIT
01647
01648      ELSE
01649          IF  W-HOLD-JOINT-FACTOR LESS THAN +1
01650              MOVE ER-7710
01651                          TO EMI-ERROR
01652              MOVE -1     TO W-JNTFC-L (W-TBLI-NDX)
01653              MOVE AL-UNBON
01654                          TO W-JNTFC-A (W-TBLI-NDX)
01655              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01656              GO TO 2080-EXIT
01657
01658          ELSE
01659              MOVE AL-UNNON
01660                          TO W-JNTFC-A (W-TBLI-NDX).
01661
01662  2080-EXIT.
01663      EXIT.
01664                                  EJECT
01665  2090-EDIT-JOINT-CDE.
01666
01667      IF  W-NULL-ENTRY
01668          MOVE AL-UANON          TO W-JNTCD-A (W-TBLI-NDX)
01669          MOVE SPACES            TO W-JNTCD-I (W-TBLI-NDX)
01670          GO TO 2090-EXIT.
01671
01672      IF  W-JNTCD-L (W-TBLI-NDX) GREATER THAN +0
01673          MOVE W-JNTCD-I (W-TBLI-NDX)
01674                                  TO W-VALID-JOINT-CODE-IND
01675
01676          IF  W-VALID-JOINT-CODE
01677              MOVE AL-UANON       TO W-JNTCD-A (W-TBLI-NDX)
01678
01679          ELSE
01680              MOVE ER-7711        TO EMI-ERROR
01681              MOVE -1             TO W-JNTCD-L (W-TBLI-NDX)
01682              MOVE AL-UABON       TO W-JNTCD-A (W-TBLI-NDX)
01683              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01684
01685  2090-EXIT.
01686      EXIT.
01687                                  EJECT
01688  2100-EDIT-PC-Q.
01689
01690      IF  W-NULL-ENTRY
01691          MOVE AL-UANON          TO W-PCQ-A (W-TBLI-NDX)
01692          MOVE SPACES            TO W-PCQ-I (W-TBLI-NDX)
01693          GO TO 2100-EXIT.
01694
01695      IF  W-PCQ-L (W-TBLI-NDX) GREATER THAN +0
01696
01697          IF  W-PCQ-I (W-TBLI-NDX) EQUAL 'Y'
01698                  OR
01699              W-PCQ-I (W-TBLI-NDX) EQUAL SPACES
01700              MOVE AL-UANON       TO W-PCQ-A (W-TBLI-NDX)
01701
01702          ELSE
01703              MOVE ER-7702        TO EMI-ERROR
01704              MOVE -1             TO W-PCQ-L (W-TBLI-NDX)
01705              MOVE AL-UABON       TO W-PCQ-A (W-TBLI-NDX)
01706              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01707              GO TO 2100-EXIT.
01708
01709      IF  W-HOLD-RESERVE-ADJ NOT EQUAL +1.0
01710              AND
01711          W-PCQ-I (W-TBLI-NDX) EQUAL 'Y'
01712          MOVE ER-7705            TO EMI-ERROR
01713          MOVE -1                 TO W-RSADJ-L (W-TBLI-NDX)
01714          MOVE AL-UABON           TO W-RSADJ-A (W-TBLI-NDX)
01715          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01716
01717  2100-EXIT.
01718      EXIT.
01719                                  EJECT
01720  2110-EDIT-MORT-CDE.
01721
01722      IF  W-MORTC-I (W-TBLI-NDX) GREATER THAN SPACES
01723          MOVE W-MORTC-I (W-TBLI-NDX) TO W-MORT-CODE
01724          GO TO 2110-CHECK-AGAINST-WORK-TABLE.
01725
01726      IF  W-NULL-ENTRY
01727          MOVE AL-UANON          TO W-MORTC-A (W-TBLI-NDX)
01728          MOVE 'ZERO'            TO W-MORTC-I (W-TBLI-NDX)
01729          GO TO 2110-EXIT.
01730
01731      IF  W-INTR-L (W-TBLI-NDX) GREATER THAN +0
01732          MOVE W-INTR-I (W-TBLI-NDX)
01733                                  TO W-INTEREST-N
01734          MOVE W-INT              TO W-MORT-INT
01735          MOVE ZERO               TO W-MORT-TYP
01736          MOVE '*'                TO W-MORT-TBL
01737
01738      ELSE
01739          MOVE ER-7703            TO EMI-ERROR
01740          MOVE -1                 TO W-INTR-L (W-TBLI-NDX)
01741          MOVE AL-UABON           TO W-INTR-A (W-TBLI-NDX)
01742          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01743          GO TO 2110-EXIT.
01744
01745      IF  W-TABLE-I (W-TBLI-NDX) EQUAL '41CSO'
01746
01747          IF  W-HOLD-RESERVE-ADJ EQUAL 1.30
01748              MOVE 'B'            TO W-MORT-TBL
01749
01750          ELSE
01751              MOVE 'A'            TO W-MORT-TBL.
01752
01753      IF  W-TABLE-I (W-TBLI-NDX) EQUAL '60CSG'
01754
01755          IF  W-HOLD-RESERVE-ADJ EQUAL 1.30
01756              MOVE 'I'            TO W-MORT-TBL
01757
01758          ELSE
01759              MOVE 'G'            TO W-MORT-TBL.
01760
01761      IF  W-TABLE-I (W-TBLI-NDX) EQUAL '58CSO'
01762
01763          IF  W-ANAL-I (W-TBLI-NDX) EQUAL 'AN'
01764
01765              IF  W-HOLD-RESERVE-ADJ EQUAL 1.30
01766                  MOVE 'E'        TO W-MORT-TBL
01767
01768              ELSE
01769                  MOVE 'C'        TO W-MORT-TBL
01770
01771          ELSE
01772              IF  W-ANAL-I (W-TBLI-NDX) EQUAL 'AL'
01773
01774                  IF  W-HOLD-RESERVE-ADJ EQUAL 1.15
01775                      MOVE 'J'    TO W-MORT-TBL
01776
01777                  ELSE
01778                      IF  W-HOLD-RESERVE-ADJ EQUAL 1.30
01779                          MOVE 'F'
01780                                  TO W-MORT-TBL
01781
01782                      ELSE
01783                          IF  W-TBLTP-I (W-TBLI-NDX) EQUAL 'J'
01784                              MOVE 'P'
01785                                  TO W-MORT-TBL
01786
01787                          ELSE
01788                              MOVE 'D'
01789                                  TO W-MORT-TBL.
01790
01791
01792      IF  W-TABLE-I (W-TBLI-NDX) EQUAL '58CET'
01793
01794          IF  W-ANAL-I (W-TBLI-NDX) EQUAL 'AN'
01795              MOVE 'H'            TO W-MORT-TBL
01796
01797          ELSE
01798              MOVE 'L'            TO W-MORT-TBL.
01799
01800      IF  W-TABLE-I (W-TBLI-NDX) EQUAL '80MSO'
01801
01802          IF  W-ANAL-I (W-TBLI-NDX) EQUAL 'AL'
01803              MOVE 'S'            TO W-MORT-TBL
01804
01805          ELSE
01806              MOVE 'Q'            TO W-MORT-TBL.
01807
01808      IF  W-TABLE-I (W-TBLI-NDX) EQUAL '80FSO'
01809
01810          IF  W-ANAL-I (W-TBLI-NDX) EQUAL 'AL'
01811              MOVE 'T'            TO W-MORT-TBL
01812
01813          ELSE
01814              MOVE 'R'            TO W-MORT-TBL.
01815
01816      IF  W-TABLE-I (W-TBLI-NDX) EQUAL '80MET'
01817          MOVE 'U'                TO W-MORT-TBL.
01818
01819      IF  W-TABLE-I (W-TBLI-NDX) EQUAL '80FET'
01820          MOVE 'V'                TO W-MORT-TBL.
01821
01822      IF  W-TABLE-I (W-TBLI-NDX) EQUAL '80GBT'
01823          MOVE 'W'                TO W-MORT-TBL.
01824
01825      IF  W-TABLE-I (W-TBLI-NDX) EQUAL 'XXXXX'
01826          MOVE 'X'                TO W-MORT-TBL.
01827
01828      IF  W-MORT-TBL EQUAL '*'
01829          MOVE ER-7704            TO EMI-ERROR
01830          MOVE -1                 TO W-MORTC-L (W-TBLI-NDX)
01831          MOVE AL-UABON           TO W-MORTC-A (W-TBLI-NDX)
01832          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01833          GO TO 2110-EXIT.
01834
01835      MOVE W-MORT-CODE            TO W-MORTC-O (W-TBLI-NDX).
01836
01837  2110-CHECK-AGAINST-WORK-TABLE.
01838
01839      IF  W-MORT-CODE NOT = 'ZERO'
01840        IF  W-MORT-TYP NOT = '0'
01841            MOVE ER-7747          TO EMI-ERROR
01842            MOVE -1               TO W-MORTC-L (W-TBLI-NDX)
01843            MOVE AL-UABON         TO W-MORTC-A (W-TBLI-NDX)
01844            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01845            GO TO 2110-EXIT.
01846
01847      MOVE AL-UANON               TO W-MORTC-A (W-TBLI-NDX)
01848      MOVE +4                     TO W-MORTC-L (W-TBLI-NDX)
01849
01850      SET W-DUPTBL-NDX            TO +1.
01851      SEARCH  W-TABLE-INPUT
01852          VARYING W-DUPTBL-NDX
01853
01854          WHEN
01855              W-MORTC-L (W-DUPTBL-NDX) EQUAL ZEROS
01856              NEXT SENTENCE
01857
01858          WHEN
01859              W-MORTC-I (W-DUPTBL-NDX) EQUAL
01860                  W-MORTC-I (W-TBLI-NDX)
01861                  AND
01862              W-DUPTBL-NDX NOT EQUAL W-TBLI-NDX
01863                  AND
01864              W-TBLTP-I (W-DUPTBL-NDX) EQUAL
01865                  W-TBLTP-I (W-TBLI-NDX)
01866              MOVE ER-7696        TO EMI-ERROR
01867              MOVE AL-UABON       TO W-MORTC-A (W-DUPTBL-NDX)
01868                                     W-MORTC-A (W-TBLI-NDX)
01869              MOVE -1             TO W-MORTC-L (W-TBLI-NDX)
01870              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01871              GO TO 2110-EXIT.
01872
01873      COMPUTE W-NEXT-TABLE-LINE = PI-LAST-MORT-TBL + 15.
01874      SET W-MORT-NDX              TO +1.
01875
01876      SEARCH  W-MORT-TBL-LINE
01877          VARYING W-MORT-NDX
01878
01879          WHEN
01880              W-MORTALITY-CODE (W-MORT-NDX) EQUAL LOW-VALUES
01881              NEXT SENTENCE
01882
01883          WHEN
01884              W-MORTALITY-CODE (W-MORT-NDX)
01885                  EQUAL W-MORTC-I (W-TBLI-NDX)
01886                  AND
01887              W-TABLE-TYPE (W-MORT-NDX) EQUAL
01888                  W-TBLTP-I (W-TBLI-NDX)
01889                  AND
01890              (W-MORT-NDX NOT GREATER THAN PI-LAST-MORT-TBL
01891                  OR
01892              W-MORT-NDX NOT LESS THAN W-NEXT-TABLE-LINE)
01893              MOVE ER-7696        TO EMI-ERROR
01894              MOVE AL-UABON       TO W-MORTC-A (W-TBLI-NDX)
01895              MOVE -1             TO W-MORTC-L (W-TBLI-NDX)
01896              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01897              GO TO 2110-EXIT.
01898
01899  2110-EXIT.
01900      EXIT.
01901
01902  2120-EDIT-COMMENTS.
01903
01904      IF W-NULL-ENTRY
01905          MOVE AL-UANON          TO W-COMM-A (W-TBLI-NDX)
01906          MOVE 'NULL ENTRY'      TO W-COMM-I (W-TBLI-NDX)
01907          GO TO 2120-EXIT.
01908
01909      IF  W-COMM-L (W-TBLI-NDX) GREATER THAN +0
01910          MOVE AL-UANON           TO W-COMM-A (W-TBLI-NDX)
01911
01912      ELSE
01913          MOVE SPACES             TO W-COMM-I (W-TBLI-NDX)
01914          MOVE AL-UANON           TO W-COMM-A (W-TBLI-NDX).
01915
01916  2120-EXIT.
01917      EXIT.
01918                                  EJECT
01919  2500-UPDATE-WORKING-MORT-TBL.
01920
01921      IF  W-TABLE-INDAT (W-TBLI-NDX) EQUAL LOW-VALUES
01922          GO TO 2500-EXIT.
01923
01924      SET W-MORT-NDX UP BY +1.
01925
01926      IF  W-TABLE-I (W-TBLI-NDX) NOT EQUAL W-TABLE (W-MORT-NDX)
01927          MOVE 'Y'                TO PI-MODIFICATIONS-MADE-IND
01928          MOVE W-TABLE-I (W-TBLI-NDX)
01929              TO W-TABLE (W-MORT-NDX).
01930
01931      IF  W-TBLTP-I (W-TBLI-NDX) NOT EQUAL
01932              W-TABLE-TYPE (W-MORT-NDX)
01933          MOVE 'Y'                TO PI-MODIFICATIONS-MADE-IND
01934          MOVE W-TBLTP-I (W-TBLI-NDX)
01935              TO W-TABLE-TYPE (W-MORT-NDX).
01936
01937      IF  W-INTEREST (W-MORT-NDX) NOT NUMERIC
01938              OR
01939          W-INTR-I (W-TBLI-NDX) NOT EQUAL W-INTEREST (W-MORT-NDX)
01940          MOVE 'Y'                TO PI-MODIFICATIONS-MADE-IND
01941          MOVE W-INTR-I (W-TBLI-NDX)
01942              TO W-INTEREST (W-MORT-NDX).
01943
01944      IF  W-ANAL-I (W-TBLI-NDX) NOT EQUAL
01945              W-AGE-METHOD (W-MORT-NDX)
01946          MOVE 'Y'                TO PI-MODIFICATIONS-MADE-IND
01947          MOVE W-ANAL-I (W-TBLI-NDX)
01948              TO W-AGE-METHOD (W-MORT-NDX).
01949
01950      IF  W-RESERVE-ADJUSTMENT (W-MORT-NDX) NOT NUMERIC
01951              OR
01952          W-RSADJ-I (W-TBLI-NDX) NOT EQUAL
01953              W-RESERVE-ADJUSTMENT (W-MORT-NDX)
01954          MOVE 'Y'                TO PI-MODIFICATIONS-MADE-IND
01955          MOVE W-RSADJ-I (W-TBLI-NDX)
01956              TO W-RESERVE-ADJUSTMENT (W-MORT-NDX).
01957
01958      IF  W-ADJDI-I (W-TBLI-NDX) NOT EQUAL
01959              W-ADJUSTMENT-DIRECTION (W-MORT-NDX)
01960          MOVE 'Y'                TO PI-MODIFICATIONS-MADE-IND
01961          MOVE W-ADJDI-I (W-TBLI-NDX)
01962              TO W-ADJUSTMENT-DIRECTION (W-MORT-NDX).
01963
01964      IF  W-JOINT-FACTOR (W-MORT-NDX) NOT NUMERIC
01965              OR
01966          W-JNTFC-I (W-TBLI-NDX) NOT EQUAL
01967              W-JOINT-FACTOR (W-MORT-NDX)
01968          MOVE 'Y'                TO PI-MODIFICATIONS-MADE-IND
01969          MOVE W-JNTFC-I (W-TBLI-NDX)
01970              TO W-JOINT-FACTOR (W-MORT-NDX).
01971
01972      IF  W-JNTCD-I (W-TBLI-NDX) NOT EQUAL
01973              W-JOINT-CODE (W-MORT-NDX)
01974          MOVE 'Y'                TO PI-MODIFICATIONS-MADE-IND
01975          MOVE W-JNTCD-I (W-TBLI-NDX)
01976              TO W-JOINT-CODE (W-MORT-NDX).
01977
01978      IF  W-PCQ-I (W-TBLI-NDX) NOT EQUAL
01979              W-PC-Q (W-MORT-NDX)
01980          MOVE 'Y'                TO PI-MODIFICATIONS-MADE-IND
01981          MOVE W-PCQ-I (W-TBLI-NDX)
01982              TO W-PC-Q (W-MORT-NDX).
01983
01984      IF  W-MORTC-I (W-TBLI-NDX) NOT EQUAL
01985              W-MORTALITY-CODE (W-MORT-NDX)
01986          MOVE 'Y'                TO PI-MODIFICATIONS-MADE-IND
01987          MOVE W-MORTC-I (W-TBLI-NDX)
01988              TO W-MORTALITY-CODE (W-MORT-NDX).
01989
01990      IF  W-COMM-I (W-TBLI-NDX) NOT EQUAL
01991              W-COMMENTS (W-MORT-NDX)
01992          MOVE 'Y'                TO PI-MODIFICATIONS-MADE-IND
01993          MOVE W-COMM-I (W-TBLI-NDX)
01994              TO W-COMMENTS (W-MORT-NDX).
01995
01996  2500-EXIT.
01997      EXIT.
01998                                  EJECT
01999  5000-MOVE-MORT-TBL-DATA.
02000
02001      MOVE ZEROS                  TO LINSEL1L
02002      MOVE LOW-VALUES             TO W-LINSEX1-O
02003                                     W-LINSEX2-O.
02004      MOVE AL-UNNOF               TO LINSEL1A.
02005
02006      MOVE AL-UANON               TO MAINTA.
02007
02008      MOVE -1                     TO MAINTL.
02009      MOVE 'S'                    TO MAINTI
02010                                     PI-MAINT.
02011
02012      MOVE PI-LAST-MNT-PROCESSOR  TO MAINTBYO
02013                                     PI-UPDATE-BY.
02014      MOVE PI-LAST-MNT-DATE-ALPHA TO MAINTDTO.
02015      MOVE PI-LAST-MNT-TIME       TO W-TIME-IN
02016                                     PI-UPDATE-HHMMSS.
02017      MOVE W-TIME-OUT             TO MAINTTMO.
02018
02019      IF  PI-LAST-MORT-TBL LESS THAN +0
02020          MOVE +0                 TO PI-LAST-MORT-TBL.
02021
02022      COMPUTE W-NEXT-TABLE-LINE
02023          = PI-LAST-MORT-TBL + 14.
02024      SET W-MORT-NDX              TO PI-LAST-MORT-TBL.
02025      SET W-MORT-NDX UP BY +1.
02026
02027      PERFORM 5020-PROCESS-TABLE-DATA THRU 5020-EXIT
02028              VARYING
02029          W-MORT-NDX FROM W-MORT-NDX BY +1
02030              UNTIL
010413         W-MORT-NDX GREATER THAN +153
02032              OR
02033          W-MORT-NDX GREATER THAN W-NEXT-TABLE-LINE.
02034
02035      MOVE -1                     TO MAINTL.
02036
02037  5000-EXIT.
02038      EXIT.
02039                                  EJECT
02040  5020-PROCESS-TABLE-DATA.
02041
02042      SET W-TBLO-NDX              TO W-MORT-NDX.
02043      SET W-TBLO-NDX DOWN BY PI-LAST-MORT-TBL.
02044      SET W-LINE-NUMBER           TO W-MORT-NDX.
02045
02046      IF  W-LINE-NUMBER LESS THAN +1
02047          MOVE +1                 TO W-LINE-NUMBER
02048          SET W-MORT-NDX
02049              W-TBLO-NDX          TO +1.
02050
02051      MOVE W-LINE-NUMBER          TO W-EDITED-LINE-NUMB.
02052      MOVE W-DISPLAY-LINE-NUMBER  TO W-LINE-O (W-TBLO-NDX).
02053      MOVE AL-SABON               TO W-LINE-A (W-TBLO-NDX).
02054
02055      IF  W-TABLE (W-MORT-NDX) GREATER THAN LOW-VALUES
02056          MOVE W-TABLE (W-MORT-NDX)
02057                                  TO W-TABLE-O (W-TBLO-NDX)
02058          MOVE W-TABLE-TYPE (W-MORT-NDX)
02059                                  TO W-TBLTP-O (W-TBLO-NDX)
02060          MOVE W-INTEREST (W-MORT-NDX)
02061                                  TO W-INTR-O (W-TBLO-NDX)
02062          MOVE W-AGE-METHOD (W-MORT-NDX)
02063                                  TO W-ANAL-O (W-TBLO-NDX)
02064          MOVE W-RESERVE-ADJUSTMENT (W-MORT-NDX)
02065                                  TO W-RSADJ-O (W-TBLO-NDX)
02066          MOVE W-ADJUSTMENT-DIRECTION (W-MORT-NDX)
02067                                  TO W-ADJDI-O (W-TBLO-NDX)
02068          MOVE W-JOINT-FACTOR (W-MORT-NDX)
02069                                  TO W-JNTFC-O (W-TBLO-NDX)
02070          MOVE W-JOINT-CODE (W-MORT-NDX)
02071                                  TO W-JNTCD-O (W-TBLO-NDX)
02072          MOVE W-PC-Q (W-MORT-NDX)
02073                                  TO W-PCQ-O (W-TBLO-NDX)
02074          MOVE W-MORTALITY-CODE (W-MORT-NDX)
02075                                  TO W-MORTC-O (W-TBLO-NDX)
02076          MOVE W-COMMENTS (W-MORT-NDX)
02077                                  TO W-COMM-O (W-TBLO-NDX)
02078
02079          PERFORM 5040-SET-TABLE-LINE-ATTRB THRU 5040-EXIT
02080
02081      ELSE
02082          MOVE SPACES             TO W-TABLE-O (W-TBLO-NDX)
02083                                     W-TBLTP-O (W-TBLO-NDX)
02084                                     W-INTR-X-O (W-TBLO-NDX)
02085                                     W-ANAL-O (W-TBLO-NDX)
02086                                     W-RSADJ-X-O (W-TBLO-NDX)
02087                                     W-ADJDI-O (W-TBLO-NDX)
02088                                     W-JNTFC-X-O (W-TBLO-NDX)
02089                                     W-JNTCD-O (W-TBLO-NDX)
02090                                     W-PCQ-O (W-TBLO-NDX)
02091                                     W-MORTC-O (W-TBLO-NDX)
02092                                     W-COMM-O (W-TBLO-NDX)
02093                                     W-LINE-O (W-TBLO-NDX)
02094          MOVE AL-UANOF           TO W-TABLE-A (W-TBLO-NDX)
02095                                     W-TBLTP-A (W-TBLO-NDX)
02096                                     W-INTR-A (W-TBLO-NDX)
02097                                     W-ANAL-A (W-TBLO-NDX)
02098                                     W-RSADJ-A (W-TBLO-NDX)
02099                                     W-ADJDI-A (W-TBLO-NDX)
02100                                     W-JNTFC-A (W-TBLO-NDX)
02101                                     W-JNTCD-A (W-TBLO-NDX)
02102                                     W-PCQ-A (W-TBLO-NDX)
02103                                     W-MORTC-A (W-TBLO-NDX)
02104                                     W-COMM-A (W-TBLO-NDX)
02105                                     W-LINE-O (W-TBLO-NDX).
02106
02107  5020-EXIT.
02108      EXIT.
02109
02110  5040-SET-TABLE-LINE-ATTRB.
02111
02112      MOVE AL-UANON               TO W-TABLE-A (W-TBLO-NDX)
02113                                     W-TBLTP-A (W-TBLO-NDX)
02114                                     W-ANAL-A (W-TBLO-NDX)
02115                                     W-ADJDI-A (W-TBLO-NDX)
02116                                     W-JNTCD-A (W-TBLO-NDX)
02117                                     W-PCQ-A (W-TBLO-NDX)
02118                                     W-MORTC-A (W-TBLO-NDX)
02119                                     W-COMM-A (W-TBLO-NDX).
02120
02121      MOVE AL-UNNON               TO W-INTR-A (W-TBLO-NDX)
02122                                     W-RSADJ-A (W-TBLO-NDX)
02123                                     W-JNTFC-A (W-TBLO-NDX).
02124
02125      MOVE AL-SABON               TO W-LINE-A (W-TBLO-NDX).
02126
02127  5040-EXIT.
02128       EXIT.
02129                                  EJECT
02130 ******************************************************************
02131 *   COMMON ERROR HANDLING ROUTINES.                              *
02132 ******************************************************************
02133
02134  8000-NOT-OPEN.
02135
02136      MOVE ER-0042                TO EMI-ERROR.
02137      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02138      MOVE -1                     TO ENTERPFL.
02139
02140      IF  EIBTRNID NOT = W-TRANSACTION
02141          GO TO 8100-SEND-INITIAL-MAP.
02142
02143      GO TO 8200-SEND-DATAONLY.
02144
02145  8010-NOT-FOUND.
02146
02147      MOVE ER-0043                TO EMI-ERROR.
02148      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02149      MOVE -1                     TO ENTERPFL.
02150
02151      IF  EIBTRNID NOT = W-TRANSACTION
02152          GO TO 8100-SEND-INITIAL-MAP.
02153
02154      GO TO 8200-SEND-DATAONLY.
02155
02156  8090-RETURN-MAIN-MENU.
02157
02158      IF  CREDIT-SESSION
02159          MOVE W-XCTL-EL626      TO W-CALL-PGM
02160
02161      ELSE
02162          IF  CLAIM-SESSION
02163              MOVE W-XCTL-EL126  TO W-CALL-PGM
02164
02165          ELSE
02166              IF  MORTGAGE-SESSION
02167                  MOVE W-XCTL-EM626
02168                                 TO W-CALL-PGM
02169
02170              ELSE
02171                  IF  GENERAL-LEDGER-SESSION
02172                      MOVE W-XCTL-GL800
02173                                 TO W-CALL-PGM.
02174
02175      GO TO 9400-XCTL.
02176                                  EJECT
02177  8100-SEND-INITIAL-MAP.
02178 ******************************************************************
02179 *                                                                *
02180 *       THIS LOGIC SENDS THE INITIAL MAP.  IT WILL LOOK FOR      *
02181 *       THE MAP DATA UNDER THE NAMES LISTED BELOW AND FOUND      *
02182 *       IN THE WORK AREA SECTION OF WORKING STORAGE.             *
02183 *                                                                *
02184 ******************************************************************
02185
02186      PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.
02187
02188      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
02189      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.
02190
02191      IF  EMI-ERROR NOT EQUAL ER-9129
02192          PERFORM 0250-CREATE-TEMP-STORAGE THRU 0250-EXIT.
02193
02194      
      * EXEC CICS SEND
02195 *        MAP    (W-MAP)
02196 *        MAPSET (W-MAPSET)
02197 *        FROM   (EL602AI)
02198 *        ERASE
02199 *        FREEKB
02200 *        CURSOR
02201 *    END-EXEC.
           MOVE LENGTH OF
            EL602AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E F  H L F ,   #00006185' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2046202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAP, 
                 EL602AI, 
                 DFHEIV12, 
                 W-MAPSET, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02202
02203      GO TO 9000-RETURN-TRANS.
02204
02205  8100-EXIT.
02206      EXIT.
02207                                  EJECT
02208  8200-SEND-DATAONLY.
02209 ******************************************************************
02210 *                                                                *
02211 *       THIS LOGIC SENDS THE UPDATED VERSION OF THE MAP, USING   *
02212 *       THE FIELDS LISTED BELOW WHICH SHOULD BE FOUND IN THE     *
02213 *       WORK AREA OF WORKING STORAGE.                            *
02214 *                                                                *
02215 ******************************************************************
02216
02217      IF  EIBTRNID NOT EQUAL W-TRANSACTION
02218          GO TO 8100-SEND-INITIAL-MAP.
02219
02220      PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.
02221
02222      IF  EIBAID = DFHPF11
02223          MOVE 'Y'                TO EMI-ROLL-SWITCH
02224          PERFORM 9900-ERROR-FORMAT.
02225
02226      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
02227      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.
02228
02229      IF  EMI-ERROR NOT EQUAL ER-9129
02230          PERFORM 0250-CREATE-TEMP-STORAGE THRU 0250-EXIT.
02231
02232      
      * EXEC CICS SEND
02233 *        MAP    (W-MAP)
02234 *        MAPSET (W-MAPSET)
02235 *        FROM   (EL602AI)
02236 *        DATAONLY
02237 *        FREEKB
02238 *        CURSOR
02239 *    END-EXEC.
           MOVE LENGTH OF
            EL602AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT    F  H L F ,   #00006223' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2046202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAP, 
                 EL602AI, 
                 DFHEIV12, 
                 W-MAPSET, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02240
02241      GO TO 9000-RETURN-TRANS.
02242
02243  8200-EXIT.
02244      EXIT.
02245                                  EJECT
02246  8300-SEND-TEXT.
02247 *****************************************************************
02248 *    THIS PARAGRAPH SENDS THE COMMON LOGOFF MESSAGE.            *
02249 *****************************************************************
02250
02251      
      * EXEC CICS SEND TEXT
02252 *        FROM    (LOGOFF-TEXT)
02253 *        LENGTH  (LOGOFF-LENGTH)
02254 *        ERASE
02255 *        FREEKB
02256 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00006242' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323432' TO DFHEIV0(25:11)
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
           
02257
02258  8300-EXIT.
02259      EXIT.
02260                                  EJECT
02261  8400-LOG-JOURNAL.
02262 ******************************************************************
02263 *                                                                *
02264 *       THIS LOGIC CREATES THE REQUIRED JOURNAL ENTRIES FOR      *
02265 *       DATA BASE UPDATES.                                       *
02266 *                                                                *
02267 *       THE FOLLOWING FIELD ARE REQUIRED IN WORKING-STORAGE:     *
02268 *                                                                *
02269 *       W-CNTL-FILE-ID   PIC  X(08)                              *
02270 *       W-JOURNAL-LENGTH PIC S9(04) COMP.                        *
02271 *       W-THIS-PGM       PIC  X(08)                              *
02272 *                                                                *
02273 ******************************************************************
02274
02275      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.
02276      MOVE W-CNTL-FILE-ID         TO JP-FILE-ID.
02277      MOVE W-THIS-PGM             TO JP-PROGRAM-ID.
02278
pemuni*    IF  PI-JOURNAL-FILE-ID NOT = ZERO
pemuni*        EXEC CICS JOURNAL
pemuni*            JFILEID (PI-JOURNAL-FILE-ID)
pemuni*            JTYPEID ('MP')
pemnui*            FROM    (JOURNAL-RECORD)
pemuni*            LENGTH  (W-JOURNAL-LENGTH)
pemuni*        END-EXEC.
02286
02287  8400-EXIT.
02288      EXIT.
02289                                  EJECT
02290  9000-RETURN-TRANS.
02291 *****************************************************************
02292 *     THIS PARAGRAPH CAUSES THE PROGRAM TO EXIT TO A            *
02293 *     TRANSACTION.                                              *
02294 *     THE FOLLOWING FIELDS ARE NEEDED IN WORKING-STORAGE        *
02295 *     W-TRANSACTION          PIC  X(04)  VALUE 'XXXX'.          *
02296 *****************************************************************
02297
02298      MOVE EMI-ERROR-NUMBER(1)     TO PI-LAST-ERROR-NO.
02299
02300      
      * EXEC CICS RETURN
02301 *        TRANSID  (W-TRANSACTION)
02302 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
02303 *        LENGTH   (PI-COMM-LENGTH)
02304 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00006291' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TRANSACTION, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02305
02306  9000-EXIT.
02307      EXIT.
02308                                  EJECT
02309  9400-XCTL.
02310 *****************************************************************
02311 *    THIS PARAGRAPH TRANSFERS CONTROL TO INDICATED PROGRAM.     *
02312 *    PROGRAM MUST RESIDE IN W-CALL-PGM.                         *
02313 *****************************************************************
02314
02315      IF  PI-MODIFICATIONS-MADE
02316
02317          IF  PI-FIRST-TIME
02318
02319              IF  W-MORTALITY-TABLE EQUAL LOW-VALUES
02320                  PERFORM 0200-RECOVER-TEMP-STORAGE
02321                      THRU 0200-EXIT
02322                  MOVE 'N'            TO PI-FIRST-TIME-IND
02323                  MOVE ER-7713        TO EMI-ERROR
02324                  MOVE -1             TO ENTERPFL
02325                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02326                  PERFORM 5000-MOVE-MORT-TBL-DATA THRU 5000-EXIT
02327                  GO TO 8100-SEND-INITIAL-MAP
02328
02329              ELSE
02330                  MOVE 'N'            TO PI-FIRST-TIME-IND
02331                  MOVE ER-7713        TO EMI-ERROR
02332                  MOVE -1             TO ENTERPFL
02333                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02334                  PERFORM 5000-MOVE-MORT-TBL-DATA THRU 5000-EXIT
02335                  GO TO 8100-SEND-INITIAL-MAP.
02336
02337      
      * EXEC CICS XCTL
02338 *        PROGRAM  (W-CALL-PGM)
02339 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
02340 *        LENGTH   (PI-COMM-LENGTH)
02341 *    END-EXEC.
      *    MOVE '.$C                   %   #00006328' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CALL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02342
02343  9400-EXIT.
02344      EXIT.
02345                                  EJECT
02346  9500-LINK-DATE-CONVERT.
02347 *****************************************************************
02348 *    THIS PARAGRAPH 'CALLS' THE UTILITY DATE PROCESSOR.         *
02349 *****************************************************************
02350
02351      
      * EXEC CICS LINK
02352 *        PROGRAM    ('ELDATCV')
02353 *        COMMAREA   (DATE-CONVERSION-DATA)
02354 *        LENGTH     (DC-COMM-LENGTH)
02355 *        END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00006342' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02356
02357  9500-EXIT.
02358      EXIT.
02359                                  EJECT
02360  9600-FORMAT-DATE-TIME.
02361 *****************************************************************
02362 *     THIS LOGIC UPDATES THE DATE/TIME INFO ON GIVEN MAP        *
02363 *****************************************************************
02364
02365      MOVE PI-COMPANY-ID          TO COMPANYO.
02366      MOVE W-SAVE-DATE            TO RUNDTEO.
02367
02368      
      * EXEC CICS ASKTIME
02369 *    END-EXEC.
      *    MOVE '0"                    "   #00006359' TO DFHEIV0
           MOVE X'302220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02370
02371      MOVE EIBTIME                TO W-TIME-IN.
02372      MOVE W-TIME-OUT             TO RUNTIMEO.
02373 *    MOVE W-MAP-NUM              TO PI-CURRENT-SCREEN-NO.
02374
02375  9600-EXIT.
02376      EXIT.
02377                                  EJECT
02378  9700-PGMID-ERROR.
02379 *****************************************************************
02380 *     THIS PARAGRAPH TRANSFERS CONTROL TO EL005 LOGOFF.         *
02381 *     THE FOLLOWING FIELDS ARE NEEDED IN WORKING-STORAGE        *
02382 *     W-CALL-PGM             PIC  X(08)  VALUE 'EL000   '.      *
02383 *     W-THIS-PGM             PIC  X(08).                        *
02384 *     W-XCTL-005             PIC  X(08)  VALUE 'EL005   '.      *
02385 *****************************************************************
02386
02387      
      * EXEC CICS  HANDLE CONDITION
02388 *        PGMIDERR  (8300-SEND-TEXT)
02389 *        END-EXEC.
      *    MOVE '"$L                   ! ( #00006378' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303036333738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02390
02391      MOVE W-THIS-PGM             TO PI-CALLING-PROGRAM.
02392      MOVE ' '                    TO PI-ENTRY-CD-1.
02393      MOVE W-XCTL-005             TO W-CALL-PGM
02394                                     LOGOFF-PGM.
02395      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
02396
02397      GO TO 9400-XCTL.
02398
02399  9700-EXIT.
02400      EXIT.
02401                                  EJECT
02402  9800-ABEND.
02403 *****************************************************************
02404 *     THIS PARAGRAPH LINKS TO A COMMON ABEND ROUTINE.           *
02405 *     THE FOLLOWING FIELDS ARE NEEDED IN WORKING-STORAGE        *
02406 *     W-LINK-004             PIC  X(08)  VALUE 'EL004   '.      *
02407 *****************************************************************
02408
02409      MOVE W-LINK-004             TO W-CALL-PGM.
02410      MOVE DFHEIBLK               TO EMI-LINE1
02411
02412      
      * EXEC CICS  LINK
02413 *        PROGRAM   (W-CALL-PGM)
02414 *        COMMAREA  (EMI-LINE1)
02415 *        LENGTH    (72)
02416 *        END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00006403' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CALL-PGM, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02417
02418      GO TO 8200-SEND-DATAONLY.
02419
02420  9800-EXIT.
02421      EXIT.
02422                                  EJECT
02423  9900-ERROR-FORMAT.
02424
02425      IF  EMI-ERRORS-COMPLETE
02426          GO TO 9900-EXIT.
02427
02428      IF  EIBAID = DFHPF11
02429          NEXT SENTENCE
02430
02431      ELSE
02432          IF  EMI-ERROR EQUAL W-LAST-ERROR
02433              GO TO 9900-EXIT
02434
02435          ELSE
02436              MOVE EMI-ERROR TO W-LAST-ERROR.
02437
02438
02439      MOVE W-LINK-001          TO W-CALL-PGM
02440
02441      
      * EXEC CICS LINK
02442 *        PROGRAM    (W-CALL-PGM)
02443 *        COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
02444 *        LENGTH     (EMI-COMM-LENGTH)
02445 *    END-EXEC.
      *    MOVE '."C                   (   #00006432' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CALL-PGM, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02446
02447  9900-EXIT.
02448      EXIT.
02449
02450                                  EJECT
02451  9910-INITIALIZE-SECURITY.
02452
02453      IF  MORTGAGE-SESSION
02454          PERFORM 9915-INITIALIZE-CONV THRU 9915-EXIT
02455          GO TO 9910-EXIT.
02456
02457      IF  PI-PROCESSOR-ID NOT EQUAL 'LGXX'
02458          
      * EXEC CICS  READQ TS
02459 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
02460 *            INTO    (SECURITY-CONTROL)
02461 *            LENGTH  (SC-COMM-LENGTH)
02462 *            ITEM    (SC-ITEM-CL-CR)
02463 *            END-EXEC
      *    MOVE '*$II   L              ''   #00006449' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM-CL-CR, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02464
02465          MOVE SC-CREDIT-DISPLAY (09)
02466                              TO PI-DISPLAY-CAP
02467          MOVE SC-CREDIT-UPDATE  (09)
02468                              TO PI-MODIFY-CAP
02469
02470          IF  NOT DISPLAY-CAP
02471              MOVE 'READ'     TO SM-READ
02472              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
02473              MOVE ER-0070    TO  EMI-ERROR
02474              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02475              MOVE AL-UANON       TO MAINTA
02476              MOVE -1             TO MAINTL
02477              GO TO 8100-SEND-INITIAL-MAP.
02478
02479  9910-EXIT.
02480      EXIT.
02481                                  EJECT
02482  9915-INITIALIZE-CONV.
02483 ******************************************************************
02484 *                                                                *
02485 *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *
02486 *       USER SECURITY RECORD SET UP BY EL125.  BASED ON THE      *
02487 *       APPLICATION NUMBER FOUND IN WORKING STORAGE UNDER        *
02488 *       W-APPL-SECRTY-NDX (PIC  S9(04) COMP), THIS PROGRAM       *
02489 *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *
02490 *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *
02491 *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *
02492 *       ERROR CONDITION AND EXITS THE PROGRAM.                   *
02493 *                                                                *
02494 *       NOTE:  THE CARRIER/GRP/STATE/PRODUCER SECURITY DATA      *
02495 *       IS ALSO PROVIDED BY THIS LOGIC.                          *
02496 *                                                                *
02497 ******************************************************************
02498
02499      IF  PI-PROCESSOR-ID NOT EQUAL 'LGXX'
02500          MOVE '125E'             TO SC-QUID-SYSTEM
02501          MOVE EIBTRMID           TO SC-QUID-TERMINAL
02502
02503          
      * EXEC CICS READQ TS
02504 *            QUEUE  (SC-QUID-KEY)
02505 *            INTO   (SECURITY-CONTROL-E)
02506 *            LENGTH (SC-COMM-LENGTH-E)
02507 *            ITEM   (SC-ITEM)
02508 *        END-EXEC
      *    MOVE '*$II   L              ''   #00006494' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SC-QUID-KEY, 
                 SECURITY-CONTROL-E, 
                 SC-COMM-LENGTH-E, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02509
02510          MOVE SC-MP-DISPLAY (W-APPL-SCRTY-NDX)
02511                                  TO PI-DISPLAY-CAP
02512          MOVE SC-MP-UPDATE (W-APPL-SCRTY-NDX)
02513                                  TO PI-MODIFY-CAP
02514
02515          IF  NOT DISPLAY-CAP
02516              MOVE 'READ'         TO SM-READ
02517              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
02518              MOVE ER-9097        TO EMI-ERROR
02519              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02520              PERFORM 8100-SEND-INITIAL-MAP.
02521
02522  9915-EXIT.
02523      EXIT.
02524                                  EJECT
02525  9995-SECURITY-VIOLATION.
02526
02527      MOVE EIBDATE          TO SM-JUL-DATE.
02528      MOVE EIBTRMID         TO SM-TERMID.
02529      MOVE W-THIS-PGM       TO SM-PGM.
02530      MOVE EIBTIME          TO W-TIME-IN.
02531      MOVE W-TIME-OUT       TO SM-TIME.
02532      MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.
02533
02534      
      * EXEC CICS LINK
02535 *         PROGRAM  ('EL003')
02536 *         COMMAREA (SECURITY-MESSAGE)
02537 *         LENGTH   (80)
02538 *    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   (   #00006525' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02539
02540  9995-EXIT.
02541      EXIT.
02542                                  EJECT
02543  9999-GOBACK.
02544      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL602' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
02545
02546  9999-EXIT.
02547      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL602' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 0030-INITIAL-PROCESS,
                     8000-NOT-OPEN,
                     8010-NOT-FOUND,
                     9700-PGMID-ERROR,
                     9800-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0240-TS-PROBLEMS,
                     0240-TS-PROBLEMS,
                     0240-TS-PROBLEMS,
                     0240-TS-PROBLEMS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0250-CONTINUE,
                     0240-TS-PROBLEMS,
                     0240-TS-PROBLEMS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 0790-LAST-RECORD-PROCESSED,
                     0790-LAST-RECORD-PROCESSED,
                     8000-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 0810-DUPLICATE-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 2051-MASTER-CNTL-NOT-FOUND,
                     2051-MASTER-CNTL-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL602' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
