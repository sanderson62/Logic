       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 EL612 .
      *                            VMOD=2.001.
      *AUTHOR.    CSO.
      *           OMAHA, NEBRASKA.
      *DATE-COMPILED.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF    CSO      IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************
      *REMARKS.
      *        THIS PROGRAM PROVIDES THE MAINTENANCE FUNCTIONS NEEDED
      *    FOR THE LIFE CLAIM INTEREST SELECTION CRITERIA.
      *
      *    SCREENS     - EL612A - SELECTION CRITERIA
      *
      *    ENTERED BY  - EL605 - LIFE CLAIM INTEREST MENU
      *
      *    EXIT TO     - EL605 - LIFE CLAIM INTEREST MENU
      *
      *    COMMAREA    - PASSED
      *
      *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON
      *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE
      *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
      *                  ENTRIES (XCTL FROM CICS VIA EX  ) THE SCREEN
      *                  WILL BE READ AND ACTION WILL BE BASED ON THE
      *                  MAINTENANCE TYPE INDICATED.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 052605    2004040700004  PEMA  NEW PROGRAM
060608* 060608    2008040300002  PEMA  ADD EFF DT TO ELCLMI PASS AREA
      ******************************************************************
           EJECT
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
       77  FILLER  PIC X(32)  VALUE '********************************'.
       77  FILLER  PIC X(32)  VALUE '*   EL612  WORKING STORAGE     *'.
       77  FILLER  PIC X(32)  VALUE '***********VMOD=2.001 **********'.
       77  CI1                PIC S999  COMP-3 VALUE +0.
       77  B1                 PIC S999  COMP-3 VALUE +0.
      *    COPY ELCSCTM.
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
      *    COPY ELCSCRTY.
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
       01  WS-DATE-AREA.
           05  SAVE-DATE           PIC X(8)    VALUE SPACES.
           05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
       01  WS-INC-DATE                 PIC XX  VALUE LOW-VALUES.
       01  WS-RPT-DATE                 PIC XX  VALUE LOW-VALUES.
       01  WS-EST-DATE                 PIC XX  VALUE LOW-VALUES.
       01  WS-PRF-DATE                 PIC XX  VALUE LOW-VALUES.
       01  WS-EFF-DATE                 PIC XX  VALUE LOW-VALUES.
       01  WS-LSTPD-DATE               PIC XX  VALUE LOW-VALUES.
       01  FILLER                          COMP-3.
           05  WS-RECORD-COUNT             PIC S9(3)   VALUE ZERO.
           05  WS-READNEXT-SW              PIC S9      VALUE ZERO.
           05  WS-LAST-ERROR-COUNT         PIC S9(3)   VALUE ZERO.
           05  WS-UPDATE-SW                PIC S9      VALUE ZERO.
           05  WS-COMPLETED-SUCCESSFUL     PIC S9      VALUE ZERO.
             88  TRANSACTION-SUCCESSFUL                    VALUE +1.
             88  INITIAL-TRANSACTION                       VALUE +2.
             88  CHANGE-SUCCESSFUL                         VALUE +3.
           05  TIME-IN                     PIC S9(7)   VALUE ZERO.
           05  TIME-OUT                    REDEFINES
               TIME-IN                     PIC S9(3)V9(4).
       01  FILLER                          COMP SYNC.
           05  WS-INDEX                    PIC S9(4)   VALUE ZERO.
           05  WS-JOURNAL-FILE-ID          PIC S9(4)   VALUE +1.
           05  WS-JOURNAL-RECORD-LENGTH    PIC S9(4)   VALUE +773.
           05  ELCISC-LENGTH               PIC S9(4)   VALUE +100.
           05  GETMAIN-SPACE               PIC  X      VALUE SPACE.
       01  FILLER.
           12  DEEDIT-FIELD        PIC  X(10).
           12  DEEDIT-FIELD-V0  REDEFINES
               DEEDIT-FIELD        PIC S9(10).
           12  DEEDIT-FIELD-V2  REDEFINES
               DEEDIT-FIELD        PIC 9(8)V99.
           05  WS-BIN-DATES OCCURS 10.
               10  WS-BIN-DATE         PIC XX.
           05  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL              VALUE +00.
               88  RESP-ERROR               VALUE +01.
               88  RESP-NOTFND              VALUE +13.
               88  RESP-NOTOPEN             VALUE +19.
               88  RESP-ENDFILE             VALUE +20.
           05  WS-CONTROL-PRIMARY.
               10  WS-COMPANY-CD           PIC X      VALUE SPACES.
               10  WS-STATE                PIC XX     VALUE SPACES.
               10  WS-PROD                 PIC XX     VALUE SPACES.
               10  WS-COV-TYPE             PIC XX     VALUE SPACES.
           05  WS-CONTROL-FILE-KEY.
               10  WS-CFK-COMPANY-ID       PIC X(3)    VALUE SPACES.
               10  WS-CFK-RECORD-TYPE      PIC X       VALUE SPACES.
               10  WS-CFK-STATE            PIC XX      VALUE SPACES.
               10  WS-CFK-BENEFIT-CD       PIC XX      VALUE SPACES.
               10  WS-CFK-SEQUENCE-NO      PIC S9(4)   VALUE ZERO COMP.
           05  WS-MAPSET-NAME              PIC X(8)  VALUE 'EL612S'.
           05  WS-MAP-NAME                 PIC X(8)  VALUE 'EL612A'.
           05  FILLER                      REDEFINES
               WS-MAP-NAME.
               10  FILLER                  PIC XX.
               10  WS-MAP-NUMBER           PIC X(4).
               10  FILLER                  PIC XX.
           05  THIS-PGM                    PIC X(8)  VALUE 'EL612'.
           05  WS-JOURNAL-TYPE-ID          PIC XX      VALUE 'EL'.
           05  WS-LOW-VALUES               PIC X VALUE LOW-VALUES.
           05  WS-SPACE                    PIC X       VALUE SPACE.
           05  WS-TRANS-ID                 PIC X(4)    VALUE 'EXAG'.
           05  WS-TEMP-STORAGE-KEY.
               10  WS-TS-TERM-ID           PIC X(4)    VALUE 'XXXX'.
               10  FILLER                  PIC X(4)    VALUE '612'.
           05  WS-ERROR-MESSAGE-AREA.
               10  ER-0000                 PIC 9(4)   VALUE 0000.
               10  ER-0004                 PIC 9(4)   VALUE 0004.
               10  ER-0006                 PIC 9(4)   VALUE 0006.
               10  ER-0008                 PIC 9(4)   VALUE 0008.
               10  ER-0023                 PIC 9(4)   VALUE 0023.
               10  ER-0029                 PIC 9(4)   VALUE 0029.
               10  ER-0070                 PIC 9(4)   VALUE 0070.
               10  ER-0150                 PIC 9(4)   VALUE 0150.
               10  ER-0196                 PIC 9(4)   VALUE 0196.
               10  ER-0419                 PIC 9(4)   VALUE 0419.
               10  ER-0491                 PIC 9(4)   VALUE 0491.
               10  ER-1079                 PIC 9(4)   VALUE 1079.
               10  ER-1257                 PIC 9(4)   VALUE 1257.
               10  ER-1258                 PIC 9(4)   VALUE 1258.
               10  ER-2261                 PIC 9(4)   VALUE 2261.
               10  ER-3800                 PIC 9(4)   VALUE 3800.
               10  ER-3801                 PIC 9(4)   VALUE 3801.
               10  ER-3802                 PIC 9(4)   VALUE 3802.
               10  ER-3803                 PIC 9(4)   VALUE 3803.
               10  ER-3804                 PIC 9(4)   VALUE 3804.
               10  ER-3805                 PIC 9(4)   VALUE 3805.
               10  ER-3806                 PIC 9(4)   VALUE 3806.
               10  ER-3807                 PIC 9(4)   VALUE 3807.
               10  ER-3810                 PIC 9(4)   VALUE 3810.
               10  ER-3811                 PIC 9(4)   VALUE 3811.
               10  ER-3812                 PIC 9(4)   VALUE 3812.
               10  ER-3813                 PIC 9(4)   VALUE 3813.
               10  ER-3814                 PIC 9(4)   VALUE 3814.
               10  ER-3815                 PIC 9(4)   VALUE 3815.
               10  ER-3816                 PIC 9(4)   VALUE 3816.
               10  ER-3817                 PIC 9(4)   VALUE 3817.
               10  ER-3818                 PIC 9(4)   VALUE 3818.
               10  ER-3819                 PIC 9(4)   VALUE 3819.
               10  ER-7220                 PIC 9(4)   VALUE 7220.
               10  ER-7698                 PIC 9(4)   VALUE 7698.
               10  ER-9999                 PIC 9(4)   VALUE 9999.
      *                                COPY ELCINTF.
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
           12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
               16  PI-1ST-TIME-SW          PIC S9     COMP-3.
               16  PI-MODE                 PIC X.
                   88  ADD-FUNCTION         VALUE 'A'.
               16  PI-CI-STATE             PIC XX.
               16  PI-CI-PROD              PIC XX.
               16  PI-CI-COV-TYPE          PIC XX.
               16  PI-CI-TOP-KEY           PIC X(7).
               16  PI-CI-BOT-KEY           PIC X(7).
               16  PI-CI-KEYS OCCURS 10.
                   20  PI-CI-KEY           PIC X(7).
               16  PI-LAST-BENEFIT-NUMBER  PIC XX.
               16  PI-NEXT-BENEFIT-NUMBER  PIC XX.
               16  PI-LINE-COUNT           PIC S9(3)  COMP-3.
               16  PI-BROWSE-SW            PIC S9     COMP-3.
               16  PI-SHOW-SW              PIC S9     COMP-3.
               16  PI-CHANGE-SW            PIC S9     COMP-3.
               16  PI-UPDATE-KEY           PIC X(10).
               16  FILLER                  PIC X(553).
           EJECT
      *                                COPY EL612S.
       01  EL612AI.
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
           05  STATEL PIC S9(0004) COMP.
           05  STATEF PIC  X(0001).
           05  FILLER REDEFINES STATEF.
               10  STATEA PIC  X(0001).
           05  STATEI PIC  X(0002).
      *    -------------------------------
           05  ZINTRATL PIC S9(0004) COMP.
           05  ZINTRATF PIC  X(0001).
           05  FILLER REDEFINES ZINTRATF.
               10  ZINTRATA PIC  X(0001).
           05  ZINTRATI PIC  999V9(5).
      *    -------------------------------
           05  PRODL PIC S9(0004) COMP.
           05  PRODF PIC  X(0001).
           05  FILLER REDEFINES PRODF.
               10  PRODA PIC  X(0001).
           05  PRODI PIC  X(0002).
      *    -------------------------------
           05  COVL PIC S9(0004) COMP.
           05  COVF PIC  X(0001).
           05  FILLER REDEFINES COVF.
               10  COVA PIC  X(0001).
           05  COVI PIC  X(0002).
      *    -------------------------------
           05  INCDTL PIC S9(0004) COMP.
           05  INCDTF PIC  X(0001).
           05  FILLER REDEFINES INCDTF.
               10  INCDTA PIC  X(0001).
           05  INCDTI PIC  X(0010).
      *    -------------------------------
           05  ESTDTL PIC S9(0004) COMP.
           05  ESTDTF PIC  X(0001).
           05  FILLER REDEFINES ESTDTF.
               10  ESTDTA PIC  X(0001).
           05  ESTDTI PIC  X(0010).
      *    -------------------------------
           05  LSTPDDTL PIC S9(0004) COMP.
           05  LSTPDDTF PIC  X(0001).
           05  FILLER REDEFINES LSTPDDTF.
               10  LSTPDDTA PIC  X(0001).
           05  LSTPDDTI PIC  X(0010).
      *    -------------------------------
           05  RPTDTL PIC S9(0004) COMP.
           05  RPTDTF PIC  X(0001).
           05  FILLER REDEFINES RPTDTF.
               10  RPTDTA PIC  X(0001).
           05  RPTDTI PIC  X(0010).
      *    -------------------------------
           05  PRFDTL PIC S9(0004) COMP.
           05  PRFDTF PIC  X(0001).
           05  FILLER REDEFINES PRFDTF.
               10  PRFDTA PIC  X(0001).
           05  PRFDTI PIC  X(0010).
      *    -------------------------------
           05  EFFDTL PIC S9(0004) COMP.
           05  EFFDTF PIC  X(0001).
           05  FILLER REDEFINES EFFDTF.
               10  EFFDTA PIC  X(0001).
           05  EFFDTI PIC  X(0010).
      *    -------------------------------
           05  CLMAMTL PIC S9(0004) COMP.
           05  CLMAMTF PIC  X(0001).
           05  FILLER REDEFINES CLMAMTF.
               10  CLMAMTA PIC  X(0001).
           05  CLMAMTI PIC  9(8)V99.
      *    -------------------------------
           05  QUALL PIC S9(0004) COMP.
           05  QUALF PIC  X(0001).
           05  FILLER REDEFINES QUALF.
               10  QUALA PIC  X(0001).
           05  QUALI PIC  X(0001).
      *    -------------------------------
           05  INTAMTL PIC S9(0004) COMP.
           05  INTAMTF PIC  X(0001).
           05  FILLER REDEFINES INTAMTF.
               10  INTAMTA PIC  X(0001).
           05  INTAMTI PIC  X(0010).
      *    -------------------------------
           05  INTRATL PIC S9(0004) COMP.
           05  INTRATF PIC  X(0001).
           05  FILLER REDEFINES INTRATF.
               10  INTRATA PIC  X(0001).
           05  INTRATI PIC  X(0008).
      *    -------------------------------
           05  NODAYSL PIC S9(0004) COMP.
           05  NODAYSF PIC  X(0001).
           05  FILLER REDEFINES NODAYSF.
               10  NODAYSA PIC  X(0001).
           05  NODAYSI PIC  X(0005).
      *    -------------------------------
           05  AEMSG1L PIC S9(0004) COMP.
           05  AEMSG1F PIC  X(0001).
           05  FILLER REDEFINES AEMSG1F.
               10  AEMSG1A PIC  X(0001).
           05  AEMSG1I PIC  X(0079).
      *    -------------------------------
           05  AEMSG2L PIC S9(0004) COMP.
           05  AEMSG2F PIC  X(0001).
           05  FILLER REDEFINES AEMSG2F.
               10  AEMSG2A PIC  X(0001).
           05  AEMSG2I PIC  X(0079).
      *    -------------------------------
           05  APFKL PIC S9(0004) COMP.
           05  APFKF PIC  X(0001).
           05  FILLER REDEFINES APFKF.
               10  APFKA PIC  X(0001).
           05  APFKI PIC  S99.
       01  EL612AO REDEFINES EL612AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ZINTRATO PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRODO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COVO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INCDTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ESTDTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTPDDTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTDTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRFDTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLMAMTO PIC  ZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  QUALO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INTAMTO PIC  ZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INTRATO PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NODAYSO PIC  Z,Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFKO PIC  X(0002).
      *    -------------------------------
      *                                COPY ELCICALC.
00001 ******************************************************************
00002 *                                                                *
00003 *                           ELCCICALC.                           *
00005 *                            VMOD=2.001                          *
00006 *                                                                *
00007 *   DESCRIPTION:  DATA TO BE PASSED TO THE CLAIM INTEREST        *
00008 *                 CALCULATION MODULE                             *
00011 *                                                                *
00012 *  PASSED TO ELCLMI                                              *
00013 *  -----------------                                             *
00014 *  STATE                                                         *
00015 *  PRODUCT                                                       *
00016 *  COVERAGE                                                      *
      *  EFFECTIVE DATE                                                *
00017 *  INCURRED DATE                                                 *
00018 *  ESTABLISHED DATE                                              *
00019 *  LAST PAID DATE                                                *
00020 *  REPORTED DATE                                                 *
00021 *  CLAIM PAYMENT AMOUNT                                          *
00022 *                                                                *
00023 *  RETURNED FROM ELRTRM                                          *
00024 *  ---------------------                                         *
00025 *  RETURN CODE                                                   *
00026 *  CLAIM INTEREST DUE SWITCH Y/N                                 *
00027 *  CLAIM INTEREST AMOUNT                                         *
00029 *----------------------------------------------------------------*
00166 *----------------------------------------------------------------*
00167 *                 LENGTH = 100                                   *
00168 *                                                                *
00169 ******************************************************************
010303******************************************************************
010303*                   C H A N G E   L O G
010303*
010303* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
010303*-----------------------------------------------------------------
010303*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
010303* EFFECTIVE    NUMBER
010303*-----------------------------------------------------------------
120905* 120905    2004040700004  PEMA  NEW COPYBOOK
060608* 060608    2008040300002  PEMA  ADD EFF DT TO ELCLMI PASS AREA
010303******************************************************************
00170
00171  01  CLAIM-INT-PASS-AREA.
00172      12  CP-CLAIM-LENGTH           PIC S9(4)         VALUE +100
00173                                      COMP.
00174
00175      12  CI-RETURN-CODE            PIC X             VALUE ZERO.
00176        88  NO-CI-ERROR                             VALUE ZERO.
00177        88  CP-ERROR-OCCURED VALUE '1' '2' '3' '4' '5' '6' '7' '8'.
00179        88  CP-ERROR-IN-SEL-CRITERIA                VALUE '1'.
00180        88  CP-NOT-QUAL-LEVEL-1                     VALUE '2'.
00181        88  CP-NOT-QUAL-LEVEL-2                     VALUE '3'.
00182        88  CP-AMT-LT-5000                          VALUE '4'.
00183        88  CP-ERROR-IN-DATES                       VALUE '5'.
00184        88  CP-ERROR-IN-ST-BREAKOUT                 VALUE '6'.
00185        88  CP-ERROR-IN-SCHEDULES                   VALUE '7'.
00185        88  CP-INTEREST-RATE-IS-ZERO                VALUE '8'.
00199 ***********************  INPUT AREAS ****************************
00200
00201      12  CP-CALCULATION-AREA.
               16  CP-COMPANY-CD         PIC X.
00202          16  CP-STATE              PIC XX.
00203          16  CP-PRODUCT            PIC XX.
00204          16  CP-COVERAGE           PIC XX.
00205          16  CP-INC-DT             PIC XX.
00205          16  CP-EST-DT             PIC XX.
00205          16  CP-LSTPD-DT           PIC XX.
00205          16  CP-RPT-DT             PIC XX.
               16  CP-PRF-DT             PIC XX.
               16  CP-EFF-DT             PIC XX.
               16  CP-CLAIM-AMT          PIC S9(7)V99 COMP-3.
               16  CP-INT-RATE           PIC S99V9(5) COMP-3.
090803         16  FILLER                PIC X(37).
00363
00364 ***************    OUTPUT FROM ELCLMI   ************************
00365
00366          16  CP-CLM-INT-SW         PIC X         VALUE SPACES.
00367
00368          16  CP-CLM-INT-AMT        PIC S9(7)V99  VALUE +0 COMP-3.
               16  CP-CLM-INT-RATE       PIC S99V9(5)  VALUE +0 COMP-3.
               16  CP-CLM-INT-NODAYS     PIC S9(5)     VALUE +0 COMP-3.
010303         16  FILLER                PIC X(19).
00514 ******************************************************************
      *                                COPY ELCJPFX.
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
                                       PIC X(750).
      *                                COPY ELCEMIB.
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
      *                                COPY ELCDATE.
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
      *                                COPY ELCLOGOF.
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
      *                                COPY ELCATTR.
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
      *                                COPY ELCAID.
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
       01  FILLER    REDEFINES DFHAID.
           05  FILLER                      PIC X(8).
           05  PF-VALUES                   PIC X
               OCCURS 24 TIMES.
           EJECT
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
       01  DFHCOMMAREA                     PIC X(1024).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL612' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
           MOVE EIBDATE               TO DC-JULIAN-YYDDD.
           MOVE '5'                   TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION
                                      THRU 8500-EXIT
           MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
           MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
           MOVE DFHCOMMAREA           TO  PROGRAM-INTERFACE-BLOCK.
           MOVE +2                    TO  EMI-NUMBER-OF-LINES
                                          EMI-SWITCH2.
           IF EIBCALEN NOT > ZERO
              MOVE UNACCESS-MSG        TO LOGOFF-MSG
              GO TO 8300-SEND-TEXT
           END-IF
           
      * EXEC CICS HANDLE CONDITION
      *        PGMIDERR (9600-PGMIDERR)
      *        ERROR    (9990-ERROR)
      *    END-EXEC
      *    MOVE '"$L.                  ! " #00001314' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031333134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF PI-CALLING-PROGRAM NOT = THIS-PGM
              IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
                 MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
                 MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
                 MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
                 MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
                 MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
                 MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
                 MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
                 MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
              ELSE
                 MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
                 MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
                 MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
                 MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
                 MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
                 MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
                 MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
                 MOVE SPACES               TO  PI-SAVED-PROGRAM-6
              END-IF
           END-IF
           IF EIBTRNID NOT = WS-TRANS-ID
              GO TO 1000-INITIAL-SCREEN
           END-IF
           IF EIBAID = DFHCLEAR
              GO TO 9400-CLEAR
           END-IF
           IF NOT SYSTEM-DISPLAY-CAP
              MOVE 'READ'              TO SM-READ
              PERFORM 9995-SECURITY-VIOLATION
              MOVE ER-0070             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              GO TO 8100-SEND-INITIAL-MAP
           END-IF
           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
              MOVE LOW-VALUES          TO EL612AO
              MOVE -1                  TO APFKL
              MOVE ER-0008             TO EMI-ERROR
              GO TO 8200-SEND-DATAONLY
           END-IF
           
      * EXEC CICS RECEIVE
      *        INTO   (EL612AO)
      *        MAPSET (WS-MAPSET-NAME)
      *        MAP    (WS-MAP-NAME)
      *    END-EXEC
           MOVE LENGTH OF
            EL612AO
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001359' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL612AO, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF APFKL > ZERO
              IF EIBAID NOT = DFHENTER
                 MOVE ER-0004          TO EMI-ERROR
                 MOVE AL-UNBOF         TO APFKA
                 MOVE -1               TO APFKL
                 GO TO 8200-SEND-DATAONLY
              ELSE
                 IF APFKO NUMERIC AND
                    (APFKO > ZERO AND LESS '25')
                    MOVE PF-VALUES (APFKI)
                                       TO  EIBAID
                 ELSE
                    MOVE ER-0029       TO  EMI-ERROR
                    MOVE AL-UNBOF      TO  APFKA
                    MOVE -1            TO  APFKL
                    GO TO 8200-SEND-DATAONLY
                 END-IF
              END-IF
           END-IF
           IF EIBAID = DFHPF12
              MOVE 'EL010'             TO THIS-PGM
              GO TO 9300-XCTL
           END-IF
           IF EIBAID = DFHPF23
              GO TO 9000-RETURN-CICS
           END-IF
           IF EIBAID = DFHPF24
              IF CREDIT-SESSION
                 MOVE 'EL626'          TO THIS-PGM
                 GO TO 9300-XCTL
              ELSE
                 MOVE 'EL126'          TO THIS-PGM
                 GO TO 9300-XCTL
              END-IF
           END-IF
           IF EIBAID = DFHENTER
              CONTINUE
           ELSE
              MOVE ER-0008             TO EMI-ERROR
              MOVE -1                  TO APFKL
              GO TO 8200-SEND-DATAONLY
           END-IF
           IF SYSTEM-MODIFY-CAP
              CONTINUE
           ELSE
              MOVE 'UPDATE'            TO SM-READ
              PERFORM 9995-SECURITY-VIOLATION
              MOVE ER-0070             TO EMI-ERROR
              MOVE -1                  TO STATEL
              MOVE AL-UABON            TO STATEA
              GO TO 8200-SEND-DATAONLY
           END-IF
           IF EMI-FATAL-CTR > ZERO
              GO TO 8200-SEND-DATAONLY
           END-IF
           GO TO 3000-PROCESS-TEST
           MOVE 'LOGIC ERROR HAS OCCURRED - PROGRAM EL612'
                                       TO  LOGOFF-MSG.
           GO TO 8300-SEND-TEXT.
           .
       1000-INITIAL-SCREEN.
           MOVE SPACES                 TO PI-PROGRAM-WORK-AREA
           MOVE PI-COMPANY-CD          TO PI-CI-TOP-KEY (1:1)
                                          PI-CI-BOT-KEY (1:1)
           MOVE ZERO                   TO PI-1ST-TIME-SW
                                          PI-LINE-COUNT
                                          PI-BROWSE-SW
                                          PI-SHOW-SW
                                          PI-CHANGE-SW
           MOVE LOW-VALUES             TO EL612AO
           MOVE -1                     TO STATEL
           MOVE +2                     TO WS-COMPLETED-SUCCESSFUL
           GO TO 8100-SEND-INITIAL-MAP
           .
      *****************************************************************
       3000-PROCESS-TEST.
      *****************************************************************
           PERFORM 6010-EDIT-DATA      THRU 6010-EXIT
           IF NOT EMI-NO-ERRORS
              GO TO 8200-SEND-DATAONLY
           END-IF
           MOVE PI-COMPANY-CD          TO CP-COMPANY-CD
           MOVE STATEI                 TO CP-STATE
           MOVE PRODI                  TO CP-PRODUCT
           MOVE COVI                   TO CP-COVERAGE
           MOVE WS-INC-DATE            TO CP-INC-DT
           MOVE WS-EST-DATE            TO CP-EST-DT
           MOVE WS-LSTPD-DATE          TO CP-LSTPD-DT
           MOVE WS-RPT-DATE            TO CP-RPT-DT
           MOVE WS-PRF-DATE            TO CP-PRF-DT
060608     MOVE WS-EFF-DATE            TO CP-EFF-DT
      *    MOVE CLMAMTI                TO CP-CLAIM-AMT
           PERFORM 7000-LINK-ELCLMI    THRU 7000-EXIT
      *    MOVE +123.45                TO CP-CLM-INT-AMT
           MOVE CP-CLM-INT-SW          TO QUALO
      *    IF CP-CLM-INT-SW = 'N'
      *       MOVE 'N'                 TO QUALO
      *    ELSE
      *       MOVE 'Y'                 TO QUALO
      *    END-IF
           IF NO-CI-ERROR
              MOVE CP-CLM-INT-AMT      TO INTAMTO
              MOVE CP-CLM-INT-RATE     TO INTRATO
              MOVE CP-CLM-INT-NODAYS   TO NODAYSO
              MOVE ZEROS               TO EMI-ERROR
              GO TO 8100-SEND-INITIAL-MAP
           END-IF
           EVALUATE CI-RETURN-CODE
              WHEN '1'
                 MOVE ER-3811          TO EMI-ERROR
              WHEN '2'
                 MOVE ER-3812          TO EMI-ERROR
              WHEN '3'
                 MOVE ER-3813          TO EMI-ERROR
              WHEN '4'
                 MOVE ER-3814          TO EMI-ERROR
              WHEN '5'
                 MOVE ER-3815          TO EMI-ERROR
              WHEN '6'
                 MOVE ER-3816          TO EMI-ERROR
              WHEN '7'
                 MOVE ER-3817          TO EMI-ERROR
              WHEN '8'
                 MOVE ER-3818          TO EMI-ERROR
              WHEN OTHER
                 MOVE ER-9999          TO EMI-ERROR
           END-EVALUATE
           MOVE AL-UABON               TO STATEA
           MOVE -1                     TO STATEL
           MOVE CI-RETURN-CODE         TO STATEO
           MOVE ZEROS                  TO INTAMTO
                                          INTRATO
                                          NODAYSO
           GO TO 8200-SEND-DATAONLY
           .
       6010-EDIT-DATA.
           IF STATEL > +0
              MOVE STATEI              TO CP-STATE
              MOVE AL-UANON            TO STATEA
           ELSE
              MOVE ER-0196             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO STATEA
              MOVE -1                  TO STATEL
           END-IF
           IF PRODL > +0
              MOVE PRODI               TO CP-PRODUCT
              MOVE AL-UANON            TO PRODA
           ELSE
              MOVE ER-3819             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO PRODA
              MOVE -1                  TO PRODL
           END-IF
           IF COVL > +0
              MOVE COVI                TO CP-COVERAGE
              MOVE AL-UANON            TO COVA
           ELSE
              MOVE ER-3819             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO COVA
              MOVE -1                  TO COVL
           END-IF
           IF INCDTL > +0
              MOVE INCDTI              TO DEEDIT-FIELD
              
      * EXEC CICS BIF DEEDIT
      *           FIELD   (DEEDIT-FIELD)
      *           LENGTH  (10)
      *       END-EXEC
           MOVE 10
             TO DFHEIV11
      *    MOVE '@"L                   #   #00001529' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF DEEDIT-FIELD-V0 NUMERIC
                 MOVE DEEDIT-FIELD-V0  TO DC-GREG-DATE-MDCY
                 MOVE 'M'              TO DC-OPTION-CODE
                 PERFORM 8500-DATE-CONVERSION
                                       THRU 8500-EXIT
                 IF NO-CONVERSION-ERROR
                    MOVE AL-UANON      TO INCDTA
                    MOVE DC-BIN-DATE-1 TO WS-INC-DATE
                    MOVE DC-GREG-DATE-A-EDIT TO INCDTO
                 ELSE
                    MOVE ER-3801       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT
                    MOVE DEEDIT-FIELD  TO INCDTO
                    MOVE AL-UABON      TO INCDTA
                    MOVE -1            TO INCDTL
                 END-IF
              ELSE
                 MOVE ER-3801          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE DEEDIT-FIELD     TO INCDTO
                 MOVE AL-UABON         TO INCDTA
                 MOVE -1               TO INCDTL
              END-IF
           ELSE
              MOVE ER-3819             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO INCDTA
              MOVE -1                  TO INCDTL
           END-IF
           IF ESTDTL > +0
              MOVE ESTDTI              TO DEEDIT-FIELD
              
      * EXEC CICS BIF DEEDIT
      *           FIELD   (DEEDIT-FIELD)
      *           LENGTH  (10)
      *       END-EXEC
           MOVE 10
             TO DFHEIV11
      *    MOVE '@"L                   #   #00001564' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF DEEDIT-FIELD-V0 NUMERIC
                 MOVE DEEDIT-FIELD-V0  TO DC-GREG-DATE-MDCY
                 MOVE 'M'              TO DC-OPTION-CODE
                 PERFORM 8500-DATE-CONVERSION
                                       THRU 8500-EXIT
                 IF NO-CONVERSION-ERROR
                    MOVE AL-UANON      TO ESTDTA
                    MOVE DC-BIN-DATE-1 TO WS-EST-DATE
                    MOVE DC-GREG-DATE-A-EDIT TO ESTDTO
                 ELSE
                    MOVE ER-3801       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT
                    MOVE DEEDIT-FIELD  TO ESTDTO
                    MOVE AL-UABON      TO ESTDTA
                    MOVE -1            TO ESTDTL
                 END-IF
              ELSE
                 MOVE ER-3801          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE DEEDIT-FIELD     TO ESTDTO
                 MOVE AL-UABON         TO ESTDTA
                 MOVE -1               TO ESTDTL
              END-IF
           ELSE
              MOVE ER-3819             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO ESTDTA
              MOVE -1                  TO ESTDTL
           END-IF
           IF LSTPDDTL > +0
              MOVE LSTPDDTI            TO DEEDIT-FIELD
              
      * EXEC CICS BIF DEEDIT
      *           FIELD   (DEEDIT-FIELD)
      *           LENGTH  (10)
      *       END-EXEC
           MOVE 10
             TO DFHEIV11
      *    MOVE '@"L                   #   #00001599' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF DEEDIT-FIELD-V0 NUMERIC
                 MOVE DEEDIT-FIELD-V0  TO DC-GREG-DATE-MDCY
                 MOVE 'M'              TO DC-OPTION-CODE
                 PERFORM 8500-DATE-CONVERSION
                                       THRU 8500-EXIT
                 IF NO-CONVERSION-ERROR
                    MOVE AL-UANON      TO LSTPDDTA
                    MOVE DC-BIN-DATE-1 TO WS-LSTPD-DATE
                    MOVE DC-GREG-DATE-A-EDIT TO LSTPDDTO
                 ELSE
                    MOVE ER-3801       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT
                    MOVE DEEDIT-FIELD  TO LSTPDDTO
                    MOVE AL-UABON      TO LSTPDDTA
                    MOVE -1            TO LSTPDDTL
                 END-IF
              ELSE
                 MOVE ER-3801          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE DEEDIT-FIELD     TO LSTPDDTO
                 MOVE AL-UABON         TO LSTPDDTA
                 MOVE -1               TO LSTPDDTL
              END-IF
           ELSE
              MOVE SAVE-BIN-DATE       TO WS-LSTPD-DATE
           END-IF
           IF RPTDTL > +0
              MOVE RPTDTI              TO DEEDIT-FIELD
              
      * EXEC CICS BIF DEEDIT
      *           FIELD   (DEEDIT-FIELD)
      *           LENGTH  (10)
      *       END-EXEC
           MOVE 10
             TO DFHEIV11
      *    MOVE '@"L                   #   #00001631' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF DEEDIT-FIELD-V0 NUMERIC
                 MOVE DEEDIT-FIELD-V0  TO DC-GREG-DATE-MDCY
                 MOVE 'M'              TO DC-OPTION-CODE
                 PERFORM 8500-DATE-CONVERSION
                                       THRU 8500-EXIT
                 IF NO-CONVERSION-ERROR
                    MOVE AL-UANON      TO RPTDTA
                    MOVE DC-BIN-DATE-1 TO WS-RPT-DATE
                    MOVE DC-GREG-DATE-A-EDIT TO RPTDTO
                 ELSE
                    MOVE ER-3801       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT
                    MOVE DEEDIT-FIELD  TO RPTDTO
                    MOVE AL-UABON      TO RPTDTA
                    MOVE -1            TO RPTDTL
                 END-IF
              ELSE
                 MOVE ER-3801          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE DEEDIT-FIELD     TO RPTDTO
                 MOVE AL-UABON         TO RPTDTA
                 MOVE -1               TO RPTDTL
              END-IF
           ELSE
              MOVE ER-3819             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO RPTDTA
              MOVE -1                  TO RPTDTL
           END-IF
           IF PRFDTL > +0
              MOVE PRFDTI              TO DEEDIT-FIELD
              
      * EXEC CICS BIF DEEDIT
      *           FIELD   (DEEDIT-FIELD)
      *           LENGTH  (10)
      *       END-EXEC
           MOVE 10
             TO DFHEIV11
      *    MOVE '@"L                   #   #00001666' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF DEEDIT-FIELD-V0 NUMERIC
                 MOVE DEEDIT-FIELD-V0  TO DC-GREG-DATE-MDCY
                 MOVE 'M'              TO DC-OPTION-CODE
                 PERFORM 8500-DATE-CONVERSION
                                       THRU 8500-EXIT
                 IF NO-CONVERSION-ERROR
                    MOVE AL-UANON      TO PRFDTA
                    MOVE DC-BIN-DATE-1 TO WS-PRF-DATE
                    MOVE DC-GREG-DATE-A-EDIT TO PRFDTO
                 ELSE
                    MOVE ER-3801       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT
                    MOVE DEEDIT-FIELD  TO PRFDTO
                    MOVE AL-UABON      TO PRFDTA
                    MOVE -1            TO PRFDTL
                 END-IF
              ELSE
                 MOVE ER-3801          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE DEEDIT-FIELD     TO PRFDTO
                 MOVE AL-UABON         TO PRFDTA
                 MOVE -1               TO PRFDTL
              END-IF
           ELSE
              MOVE ER-3819             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO PRFDTA
              MOVE -1                  TO PRFDTL
           END-IF
           IF EFFDTL > +0
              MOVE EFFDTI              TO DEEDIT-FIELD
              
      * EXEC CICS BIF DEEDIT
      *           FIELD   (DEEDIT-FIELD)
      *           LENGTH  (10)
      *       END-EXEC
           MOVE 10
             TO DFHEIV11
      *    MOVE '@"L                   #   #00001701' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF DEEDIT-FIELD-V0 NUMERIC
                 MOVE DEEDIT-FIELD-V0  TO DC-GREG-DATE-MDCY
                 MOVE 'M'              TO DC-OPTION-CODE
                 PERFORM 8500-DATE-CONVERSION
                                       THRU 8500-EXIT
                 IF NO-CONVERSION-ERROR
                    MOVE AL-UANON      TO EFFDTA
                    MOVE DC-BIN-DATE-1 TO WS-EFF-DATE
                    MOVE DC-GREG-DATE-A-EDIT TO EFFDTO
                 ELSE
                    MOVE ER-3801       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT
                    MOVE DEEDIT-FIELD  TO EFFDTO
                    MOVE AL-UABON      TO EFFDTA
                    MOVE -1            TO EFFDTL
                 END-IF
              ELSE
                 MOVE ER-3801          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE DEEDIT-FIELD     TO EFFDTO
                 MOVE AL-UABON         TO EFFDTA
                 MOVE -1               TO EFFDTL
              END-IF
           ELSE
              IF CP-STATE = 'UT'
                 MOVE ER-3819             TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON            TO EFFDTA
                 MOVE -1                  TO EFFDTL
              END-IF
           END-IF
           IF CLMAMTL > +0
      *       MOVE CLMAMTI             TO DEEDIT-FIELD
              
      * EXEC CICS BIF DEEDIT
      *           FIELD   (CLMAMTI)
      *           LENGTH  (10)
      *       END-EXEC
           MOVE 10
             TO DFHEIV11
      *    MOVE '@"L                   #   #00001738' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMAMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF CLMAMTI NOT NUMERIC
                 MOVE ER-0419          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO CLMAMTA
                 MOVE -1               TO CLMAMTL
              ELSE
                 MOVE CLMAMTI          TO CP-CLAIM-AMT
                 MOVE CP-CLAIM-AMT     TO CLMAMTO
                 MOVE AL-UNNON         TO CLMAMTA
              END-IF
           ELSE
              MOVE ER-3819             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO CLMAMTA
              MOVE -1                  TO CLMAMTL
           END-IF
           IF ZINTRATL > +0
              
      * EXEC CICS BIF DEEDIT
      *           FIELD   (ZINTRATI)
      *           LENGTH  (08)
      *       END-EXEC
           MOVE 08
             TO DFHEIV11
      *    MOVE '@"L                   #   #00001759' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ZINTRATI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF ZINTRATI NOT NUMERIC
                 MOVE ER-0419          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO ZINTRATA
                 MOVE -1               TO ZINTRATL
              ELSE
                 MOVE ZINTRATI         TO CP-INT-RATE
                 MOVE CP-INT-RATE      TO ZINTRATO
                 MOVE AL-UNNON         TO ZINTRATA
              END-IF
           ELSE
              MOVE ZEROS               TO CP-INT-RATE
           END-IF
           .
       6010-EXIT.
           EXIT.
       7000-LINK-ELCLMI.
           
      * EXEC CICS LINK
      *       PROGRAM   ('ELCLMI')
      *       COMMAREA  (CLAIM-INT-PASS-AREA)
      *       LENGTH    (CP-CLAIM-LENGTH)
      *    END-EXEC
           MOVE 'ELCLMI' TO DFHEIV1
      *    MOVE '."C                   ''   #00001780' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CLAIM-INT-PASS-AREA, 
                 CP-CLAIM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       7000-EXIT.
           EXIT.
       8100-SEND-INITIAL-MAP.
      *****************************************************************
           MOVE ZERO                   TO  PI-BROWSE-SW.
           MOVE -1                     TO  STATEL
           MOVE SAVE-DATE              TO  ADATEO.
           MOVE EIBTIME                TO  TIME-IN.
           MOVE TIME-OUT               TO  ATIMEO.
           IF EMI-ERROR NOT = ZERO
               PERFORM 9900-ERROR-FORMAT
           END-IF
           MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.
           MOVE EMI-MESSAGE-AREA (2)    TO  AEMSG2O.
           
      * EXEC CICS SEND
      *        FROM   (EL612AO)
      *        MAPSET (WS-MAPSET-NAME)
      *        MAP    (WS-MAP-NAME)
      *        CURSOR ERASE
      *    END-EXEC.
           MOVE LENGTH OF
            EL612AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00001800' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL612AO, 
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
           
           GO TO 9100-RETURN-TRAN
           .
       8100-EXIT.
            EXIT.
           EJECT
      *****************************************************************
       8200-SEND-DATAONLY.
      *****************************************************************
           MOVE SAVE-DATE              TO  ADATEO.
           MOVE EIBTIME                TO  TIME-IN.
           MOVE TIME-OUT               TO  ATIMEO.
           IF EMI-ERROR NOT = ZERO
               PERFORM 9900-ERROR-FORMAT.
           MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.
           MOVE EMI-MESSAGE-AREA (2)    TO  AEMSG2O.
           
      * EXEC CICS SEND DATAONLY
      *        FROM   (EL612AO)
      *        MAPSET (WS-MAPSET-NAME)
      *        MAP    (WS-MAP-NAME)
      *        CURSOR
      *    END-EXEC.
           MOVE LENGTH OF
            EL612AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00001821' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL612AO, 
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
           
           GO TO 9100-RETURN-TRAN
           .
       8200-EXIT.
            EXIT.
           EJECT
      *****************************************************************
       8300-SEND-TEXT.
      *****************************************************************
           
      * EXEC CICS SEND TEXT
      *        FROM   (LOGOFF-TEXT)
      *        LENGTH (LOGOFF-LENGTH)
      *        ERASE  FREEKB
      *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00001835' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383335' TO DFHEIV0(25:11)
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
           
           
      * EXEC CICS RETURN
      *    END-EXEC.
      *    MOVE '.(                    &   #00001840' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       8300-EXIT.
            EXIT.
           EJECT
      *****************************************************************
       8400-LOG-JOURNAL-RECORD.
      *****************************************************************
           IF PI-JOURNAL-FILE-ID = 0
               GO TO 8400-EXIT.
           MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
           MOVE 'ELCNTL'               TO  JP-FILE-ID.
           MOVE THIS-PGM               TO  JP-PROGRAM-ID.
      *    EXEC CICS JOURNAL
      *        JFILEID (PI-JOURNAL-FILE-ID)
      *        JTYPEID (WS-JOURNAL-TYPE-ID)
      *        FROM    (JOURNAL-RECORD)
      *        LENGTH  (WS-JOURNAL-RECORD-LENGTH)
      *    END-EXEC.
       8400-EXIT.
            EXIT.
      *****************************************************************
       8500-DATE-CONVERSION.
      *****************************************************************
           
      * EXEC CICS LINK
      *        PROGRAM  ('ELDATCV')
      *        COMMAREA (DATE-CONVERSION-DATA)
      *        LENGTH   (DC-COMM-LENGTH)
      *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00001864' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       8500-EXIT.
            EXIT.
           EJECT
      *****************************************************************
       9000-RETURN-CICS.
      *****************************************************************
           MOVE 'EL005'                TO  THIS-PGM.
           MOVE EIBAID                 TO  PI-ENTRY-CD-1.
           PERFORM 9300-XCTL.
       9000-EXIT.
            EXIT.
      *****************************************************************
       9100-RETURN-TRAN.
      *****************************************************************
           MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
           MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
           
      * EXEC CICS RETURN
      *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
      *        LENGTH   (PI-COMM-LENGTH)
      *        TRANSID  (WS-TRANS-ID)
      *    END-EXEC.
      *    MOVE '.(CT                  &   #00001885' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9100-EXIT.
            EXIT.
      *****************************************************************
       9300-XCTL.
      *****************************************************************
           MOVE DFHENTER               TO  EIBAID.
           
      * EXEC CICS XCTL
      *        PROGRAM  (THIS-PGM)
      *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
      *        LENGTH   (PI-COMM-LENGTH)
      *    END-EXEC.
      *    MOVE '.$C                   $   #00001896' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9300-EXIT.
            EXIT.
           EJECT
      *****************************************************************
       9400-CLEAR.
      *****************************************************************
           MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.
           PERFORM 9300-XCTL.
       9400-EXIT.
            EXIT.
      *****************************************************************
       9600-PGMIDERR.
      *****************************************************************
           
      * EXEC CICS HANDLE CONDITION
      *        PGMIDERR (8300-SEND-TEXT)
      *    END-EXEC.
      *    MOVE '"$L                   ! # #00001914' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303031393134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE THIS-PGM               TO  PI-CALLING-PROGRAM
                                           LOGOFF-PGM.
           MOVE 'EL005'                TO  THIS-PGM.
           MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
           MOVE SPACES                 TO  PI-ENTRY-CD-1.
           PERFORM 9300-XCTL.
       9600-EXIT.
            EXIT.
      *****************************************************************
       9900-ERROR-FORMAT.
      *****************************************************************
           IF EMI-ERRORS-COMPLETE
               ADD +1             TO  EMI-FATAL-CTR
               MOVE ZERO          TO  EMI-ERROR
               GO TO 9900-EXIT.
           
      * EXEC CICS LINK
      *        PROGRAM  ('EL001')
      *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
      *        LENGTH   (EMI-COMM-LENGTH)
      *    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   ''   #00001932' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031393332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE ZERO              TO  EMI-ERROR.
       9900-EXIT.
            EXIT.
           EJECT
      *****************************************************************
       9990-ERROR.
      *****************************************************************
           MOVE DFHEIBLK               TO EMI-LINE1.
           
      * EXEC CICS LINK
      *        PROGRAM   ('EL004')
      *        COMMAREA  (EMI-LINE1)
      *        LENGTH    (72)
      *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00001945' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031393435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           PERFORM 8200-SEND-DATAONLY.
           GO TO 9100-RETURN-TRAN.
       9990-EXIT.
            EXIT.
      *****************************************************************
       9995-SECURITY-VIOLATION.
      *****************************************************************
      *           COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00001974' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031393734' TO DFHEIV0(25:11)
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
       9995-EXIT.
            EXIT.
       9999-LAST-PARAGRAPH.
           
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL612' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL612' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMIDERR,
                     9990-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL612' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
