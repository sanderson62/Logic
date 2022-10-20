       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 EL6503.
      *                            VMOD=2.001.
      *AUTHOR.    PABLO.
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
      *    FOR THE BRANCH LOCATION AND SHIPPING ADDRESSES FOR PRODUCERS
      *
      *    SCREENS     - EL650I - BRANCH LOCAL AND SHIPPING ADDR MAINT
      *
      *    ENTERED BY  - EL650 - ACCOUNT MAINTENANCE MENU
      *
      *    EXIT TO     - EL650 - ACCOUNT MAINTENANCE MENU
      *
      *    COMMAREA    - PASSED
      *
      *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL650.  ON
      *                  FIRST ENTRY, READ ERACNT TO DISPLAY INFO,  THE
      *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
      *                  ENTRIES (XCTL FROM CICS VIA EXC7) THE SCREEN
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
      * 110706  CR2006071700004  PEMA  NEW PROGRAM
111407* 111407  CR2007010300001  PEMA  USE ACCT SEC FOR THIS PROGRAM
052918* 052918  CR2018040600002  PEMA  ADD REFUND DIRECT INDICATOR
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
       77  FILLER  PIC X(32)  VALUE '********************************'.
       77  FILLER  PIC X(32)  VALUE '*   EL6503 WORKING STORAGE     *'.
       77  FILLER  PIC X(32)  VALUE '***********VMOD=2.001 **********'.
       77  CI1                PIC S999  COMP-3 VALUE +0.
       77  B1                 PIC S999  COMP-3 VALUE +0.
       77  WS-BROWSE-SW                PIC X   VALUE SPACES.
           88  ERACNT-BROWSE-STARTED         VALUE 'Y'.
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
           05  ERACNT-LENGTH               PIC S9(4)   VALUE +120.
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
           05  WS-SAVE-ERACNT-KEY      PIC X(23)   VALUE LOW-VALUES.
           05  WS-CONTROL-PRIMARY.
               10  WS-COMPANY-CD           PIC X      VALUE SPACES.
               10  WS-CARRIER              PIC X      VALUE SPACES.
               10  WS-GROUPING             PIC X(6)   VALUE SPACES.
               10  WS-STATE                PIC XX     VALUE SPACES.
               10  WS-ACCOUNT              PIC X(10)  VALUE SPACES.
               10  WS-REC-TYPE             PIC X      VALUE SPACES.
               10  WS-SEQ-NO               PIC S9(4)  VALUE +0 COMP.
           05  WS-CONTROL-FILE-KEY.
               10  WS-CFK-COMPANY-ID       PIC X(3)    VALUE SPACES.
               10  WS-CFK-RECORD-TYPE      PIC X       VALUE SPACES.
               10  WS-CFK-STATE            PIC XX      VALUE SPACES.
               10  WS-CFK-BENEFIT-CD       PIC XX      VALUE SPACES.
               10  WS-CFK-SEQUENCE-NO      PIC S9(4)   VALUE ZERO COMP.
           05  WS-MAPSET-NAME              PIC X(8)  VALUE 'EL6503S'.
           05  WS-MAP-NAME                 PIC X(8)  VALUE 'EL6503A'.
           05  FILLER                      REDEFINES
               WS-MAP-NAME.
               10  FILLER                  PIC XX.
               10  WS-MAP-NUMBER           PIC X(4).
               10  FILLER                  PIC XX.
           05  THIS-PGM                    PIC X(8)  VALUE 'EL6503'.
           05  WS-JOURNAL-TYPE-ID          PIC XX      VALUE 'EL'.
           05  WS-LOW-VALUES               PIC X VALUE LOW-VALUES.
           05  WS-SPACE                    PIC X       VALUE SPACE.
           05  WS-TRANS-ID                 PIC X(4)    VALUE 'EXC7'.
           05  WS-TEMP-STORAGE-KEY.
               10  WS-TS-TERM-ID           PIC X(4)    VALUE 'XXXX'.
               10  FILLER                  PIC X(4)    VALUE '6503'.
           05  WS-ERROR-MESSAGE-AREA.
               10  ER-0000             PIC 9(4)   VALUE 0000.
               10  ER-0004             PIC 9(4)   VALUE 0004.
               10  ER-0006             PIC 9(4)   VALUE 0006.
               10  ER-0008             PIC 9(4)   VALUE 0008.
               10  ER-0023             PIC 9(4)   VALUE 0023.
               10  ER-0029             PIC 9(4)   VALUE 0029.
               10  ER-0070             PIC 9(4)   VALUE 0070.
052918         10  ER-3271             PIC 9(4)   VALUE 3271.
               10  ER-9999             PIC 9(4)   VALUE 9999.
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
      *                                COPY ELC650PI.
00001 ******************************************************************
00002 *
00003 *                            ELC650PI.
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.007
00006 *  ***  NOTE  ***   IF ANY CHANGES ARE MADE TO THIS COPYBOOK
00007 *   YOU MUST CONSIDER ALL PROGRAMS THAT USE THIS COPYBOOK AND
00008 * PROGRAM EL6565.  ALSO, CONSIDER EL106 AND EL1061
00009 *
00010 ******************************************************************
101916******************************************************************
101916*                   C H A N G E   L O G
101916*
101916* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101916*-----------------------------------------------------------------
101916*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101916* EFFECTIVE    NUMBER
101916*-----------------------------------------------------------------
101916* 101916  IR2016101900001  PEMA  Inc tot line to 3 bytes
00011
00012      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
00013          16  PI-MAINT                   PIC X.
00014          16  PI-PREV-ACCOUNT            PIC X(20).
00015          16  PI-PREV-VG-ACCOUNT         PIC X(20).
00016          16  PI-ACCT-KEY.
00017              20  PI-ACCT-CCGSA-KEY.
00018                  24  PI-ACCT-CO             PIC X.
00019                  24  PI-ACCT-CARRIER        PIC X.
00020                  24  PI-ACCT-GROUPING       PIC X(6).
00021                  24  PI-ACCT-STATE          PIC XX.
00022                  24  PI-ACCT-ACCOUNT        PIC X(10).
00023              20  PI-ACCT-EXP-DT           PIC XX.
00024              20  PI-ACCT-REST-OF-EXP      PIC X(4).
00025          16  PI-ACCT-ID                 PIC X(8).
00026          16  PI-PLAN-KEY.
00027              20  PI-PLAN-ACCT-KEY.
00028                  24  PI-PLAN-COMPANY-CD PIC X.
00029                  24  PI-PLAN-CARRIER    PIC X.
00030                  24  PI-PLAN-GROUP      PIC X(6).
00031                  24  PI-PLAN-STATE      PIC X(2).
00032                  24  PI-PLAN-ACCOUNT    PIC X(10).
00033              20  PI-PLAN-BEN-TYPE       PIC X.
00034              20  PI-PLAN-BEN            PIC XX.
00035              20  PI-PLAN-REVISION       PIC X(3).
00036          16  PI-WS-STATE                PIC XX.
00037          16  PI-WS-CLASS                PIC XX.
00038          16  PI-WS-DEV                  PIC X(3).
00039          16  PI-WS-TYPE                 PIC X.
00040          16  PI-WS-PLAN                 PIC XX.
00041
00042          16  PI-ERPNDB-ALT-KEY.
00043              20  PI-PB-COMPANY-CD-A1    PIC X.
00044              20  PI-PB-CARRIER          PIC X.
00045              20  PI-PB-GROUPING         PIC X(6).
00046              20  PI-PB-STATE            PIC XX.
00047              20  PI-PB-ACCOUNT          PIC X(10).
00048              20  PI-PB-CERT-EFF-DT      PIC XX.
00049              20  PI-PB-CERT-NO          PIC X(10).
00050              20  PI-PB-ALT-CHG-SEQ-NO   PIC S9(4)      COMP.
00051              20  PI-PB-RECORD-TYPE      PIC X.
00052
00053          16  PI-DATE-RANGE-TABLE.
00054              20  PI-TABLE-ENT OCCURS 32 TIMES
00055                             INDEXED BY T-INDEX.
00056                  24  PI-BIN-EFF-DT          PIC XX.
00057                  24  PI-BIN-EXP-DT          PIC XX.
00058                  24  PI-BIN-MAINT-DT        PIC XX.
00059                  24  PI-BIN-LO-CERT         PIC XX.
00060                  24  PI-BIN-AR-HI-CERT      PIC XX.
00061                  24  PI-BIN-HI-CERT         PIC XX.
00062          16  PI-PAGE-NUMBER             PIC S9.
00063              88  PI-FST-PAGE               VALUE +1.
00064              88  PI-2ND-PAGE               VALUE +2.
00065              88  PI-3RD-PAGE               VALUE +3.
00066              88  PI-LST-PAGE               VALUE +4.
101916         16  PI-TOTAL-LINES             PIC S999.
00068          16  PI-LINE-SELECTED    PIC S9.
00069 ***  Y2K PROJ 7744
00070          16  EFFCHG-SAVE         PIC 9(11)   COMP-3.
00071          16  BIN-EFFCHG-SAVE     PIC XX.
00072          16  EXPCHG-SAVE         PIC 9(11)   COMP-3.
00073 ***  Y2K PROJ 7744
00074          16  BIN-EXPCHG-SAVE     PIC XX.
00075          16  PI-RECORD-ADDED-SW  PIC X.
00076              88  PI-RECORD-ADDED            VALUE '1'.
00077              88  PI-RECORD-NOT-CREATED      VALUE SPACE.
00078          16  PI-ACCNAME          PIC X(30).
00079          16  PI-COMM-POINTER     PIC S9(8)   COMP.
00080          16  PI-SV-MAINT         PIC X.
00081          16  PI-CURRENT-LINE     PIC S9(3)   COMP-3.
00082          16  PI-TEMP-STOR-ITEMS  PIC S9(4)   COMP.
00083          16  PI-UPDATE-SW        PIC X.
00084              88  PI-CHANGES-MADE             VALUE '1'.
00085          16  PI-NOTE-TYPE        PIC X.
00086              88  PI-ACCT-NOTE                VALUE '1'.
00087          16  PI-DMD-FILE-SW      PIC X.
00088              88  END-OF-FILE                 VALUE 'E'.
00089              88  INTO-NEXT-BENEFITS          VALUE 'I'.
00090              88  FIRST-OCCURS                VALUE 'F'.
00091          16  PI-DMD-OCCURS       PIC S999.
00092          16  PI-DMD-SCREEN       PIC X.
00093              88  SCREEN-1-DISPLAYED  VALUE '1'.
00094              88  SCREEN-2-DISPLAYED  VALUE '2'.
00095              88  SCREEN-3-DISPLAYED  VALUE '3'.
00096          16  PI-NAMEFLG          PIC X.
PEMTST         16  PI-EL650-DEL-SW     PIC X.
               16  PI-MAX-MFEE         PIC S9(5) COMP-3.
               16  PI-DCC-PROD-CODE    PIC XXX.
101916         16  FILLER              PIC X(34).
00098      EJECT
      *                                COPY EL6503S.
       01  EL6503AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  RUNDATEL PIC S9(0004) COMP.
           05  RUNDATEF PIC  X(0001).
           05  FILLER REDEFINES RUNDATEF.
               10  RUNDATEA PIC  X(0001).
           05  RUNDATEI PIC  X(0008).
      *    -------------------------------
           05  RUNTIMEL PIC S9(0004) COMP.
           05  RUNTIMEF PIC  X(0001).
           05  FILLER REDEFINES RUNTIMEF.
               10  RUNTIMEA PIC  X(0001).
           05  RUNTIMEI PIC  X(0005).
      *    -------------------------------
           05  CMPNYIDL PIC S9(0004) COMP.
           05  CMPNYIDF PIC  X(0001).
           05  FILLER REDEFINES CMPNYIDF.
               10  CMPNYIDA PIC  X(0001).
           05  CMPNYIDI PIC  X(0003).
      *    -------------------------------
           05  USERIDL PIC S9(0004) COMP.
           05  USERIDF PIC  X(0001).
           05  FILLER REDEFINES USERIDF.
               10  USERIDA PIC  X(0001).
           05  USERIDI PIC  X(0004).
      *    -------------------------------
           05  MAINTYPL PIC S9(0004) COMP.
           05  MAINTYPF PIC  X(0001).
           05  FILLER REDEFINES MAINTYPF.
               10  MAINTYPA PIC  X(0001).
           05  MAINTYPI PIC  X(0001).
      *    -------------------------------
           05  LSTDTEL PIC S9(0004) COMP.
           05  LSTDTEF PIC  X(0001).
           05  FILLER REDEFINES LSTDTEF.
               10  LSTDTEA PIC  X(0001).
           05  LSTDTEI PIC  X(0008).
      *    -------------------------------
           05  LSTTIMEL PIC S9(0004) COMP.
           05  LSTTIMEF PIC  X(0001).
           05  FILLER REDEFINES LSTTIMEF.
               10  LSTTIMEA PIC  X(0001).
           05  LSTTIMEI PIC  X(0005).
      *    -------------------------------
           05  LSTUSRL PIC S9(0004) COMP.
           05  LSTUSRF PIC  X(0001).
           05  FILLER REDEFINES LSTUSRF.
               10  LSTUSRA PIC  X(0001).
           05  LSTUSRI PIC  X(0004).
      *    -------------------------------
           05  CARRIERL PIC S9(0004) COMP.
           05  CARRIERF PIC  X(0001).
           05  FILLER REDEFINES CARRIERF.
               10  CARRIERA PIC  X(0001).
           05  CARRIERI PIC  X(0001).
      *    -------------------------------
           05  GROUPL PIC S9(0004) COMP.
           05  GROUPF PIC  X(0001).
           05  FILLER REDEFINES GROUPF.
               10  GROUPA PIC  X(0001).
           05  GROUPI PIC  X(0006).
      *    -------------------------------
           05  STATEL PIC S9(0004) COMP.
           05  STATEF PIC  X(0001).
           05  FILLER REDEFINES STATEF.
               10  STATEA PIC  X(0001).
           05  STATEI PIC  X(0002).
      *    -------------------------------
           05  ACCTNOL PIC S9(0004) COMP.
           05  ACCTNOF PIC  X(0001).
           05  FILLER REDEFINES ACCTNOF.
               10  ACCTNOA PIC  X(0001).
           05  ACCTNOI PIC  X(0010).
      *    -------------------------------
           05  ACTSPCL PIC S9(0004) COMP.
           05  ACTSPCF PIC  X(0001).
           05  FILLER REDEFINES ACTSPCF.
               10  ACTSPCA PIC  X(0001).
           05  ACTSPCI PIC  X(0001).
      *    -------------------------------
           05  BLINE1L PIC S9(0004) COMP.
           05  BLINE1F PIC  X(0001).
           05  FILLER REDEFINES BLINE1F.
               10  BLINE1A PIC  X(0001).
           05  BLINE1I PIC  X(0060).
      *    -------------------------------
           05  BLINE2L PIC S9(0004) COMP.
           05  BLINE2F PIC  X(0001).
           05  FILLER REDEFINES BLINE2F.
               10  BLINE2A PIC  X(0001).
           05  BLINE2I PIC  X(0060).
      *    -------------------------------
           05  SNAME1L PIC S9(0004) COMP.
           05  SNAME1F PIC  X(0001).
           05  FILLER REDEFINES SNAME1F.
               10  SNAME1A PIC  X(0001).
           05  SNAME1I PIC  X(0060).
      *    -------------------------------
           05  SNAME2L PIC S9(0004) COMP.
           05  SNAME2F PIC  X(0001).
           05  FILLER REDEFINES SNAME2F.
               10  SNAME2A PIC  X(0001).
           05  SNAME2I PIC  X(0060).
      *    -------------------------------
           05  SADDR1L PIC S9(0004) COMP.
           05  SADDR1F PIC  X(0001).
           05  FILLER REDEFINES SADDR1F.
               10  SADDR1A PIC  X(0001).
           05  SADDR1I PIC  X(0060).
      *    -------------------------------
           05  SADDR2L PIC S9(0004) COMP.
           05  SADDR2F PIC  X(0001).
           05  FILLER REDEFINES SADDR2F.
               10  SADDR2A PIC  X(0001).
           05  SADDR2I PIC  X(0060).
      *    -------------------------------
           05  SADDR3L PIC S9(0004) COMP.
           05  SADDR3F PIC  X(0001).
           05  FILLER REDEFINES SADDR3F.
               10  SADDR3A PIC  X(0001).
           05  SADDR3I PIC  X(0060).
      *    -------------------------------
           05  SCITYL PIC S9(0004) COMP.
           05  SCITYF PIC  X(0001).
           05  FILLER REDEFINES SCITYF.
               10  SCITYA PIC  X(0001).
           05  SCITYI PIC  X(0060).
      *    -------------------------------
           05  SSTATEL PIC S9(0004) COMP.
           05  SSTATEF PIC  X(0001).
           05  FILLER REDEFINES SSTATEF.
               10  SSTATEA PIC  X(0001).
           05  SSTATEI PIC  X(0002).
      *    -------------------------------
           05  SZIPL PIC S9(0004) COMP.
           05  SZIPF PIC  X(0001).
           05  FILLER REDEFINES SZIPF.
               10  SZIPA PIC  X(0001).
           05  SZIPI PIC  X(0010).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0072).
      *    -------------------------------
           05  PFKEYL PIC S9(0004) COMP.
           05  PFKEYF PIC  X(0001).
           05  FILLER REDEFINES PFKEYF.
               10  PFKEYA PIC  X(0001).
           05  PFKEYI PIC  99.
       01  EL6503AO REDEFINES EL6503AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPNYIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTUSRO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTSPCO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BLINE1O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BLINE2O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SNAME1O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SNAME2O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SADDR1O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SADDR2O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SADDR3O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SCITYO PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SSTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SZIPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYO PIC  X(0002).
      *    -------------------------------
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
           12  jp-date                     pic s9(5) comp-3.
           12  jp-time                     pic s9(7) comp-3.
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
           12  emi-claim-no                pic x(7).
           12  emi-claim-type              pic x(6).
00070      12  FILLER                      PIC X(124)  VALUE SPACES.
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
051007*00039    02  DFHPF22   PIC  X  VALUE  '�'.
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
      *                                COPY ERCACNT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCACNT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = NOTE FILE FOR RECORDING OF ACCOUNT NOTES  *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 120   RECFORM = FIXED                          *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERACNT             RKP=2,LEN=23          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *                                                                *
00016 *   LOG = YES                                                    *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 ******************************************************************
110706*                   C H A N G E   L O G
110706*
110706* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
110706*-----------------------------------------------------------------
110706*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
110706* EFFECTIVE    NUMBER
110706*-----------------------------------------------------------------
110706* 110706  CR2006071700004  PEMA  ADD BRANCH LOCATIONS
110706*           AND SHIPPING ADDRESS TO ACCOUNT NOTES FILE
110706******************************************************************
00019  01  NOTE-FILE.
00020      12  NT-FILE-ID                  PIC XX.
00021          88  VALID-NOTE-ID              VALUE 'NT'.
00022
00023      12  NT-CONTROL-PRIMARY.
00024          16  NT-COMPANY-CD           PIC X.
00027          16  NT-ACCT-NOTE-KEY.
00028              18  NT-CARRIER              PIC X.
00029              18  NT-GROUPING             PIC X(06).
00030              18  NT-STATE                PIC XX.
00031              18  NT-ACCOUNT              PIC X(10).
00025          16  NT-RECORD-TYPE          PIC X.
00026               88  ACCT-NOTE          VALUE '1'.
110706              88  ACCT-BRANCH-LOC    VALUE '2'.
110706              88  ACCT-SHIPPING-ADDR VALUE '3'.
00032          16  NT-LINE-SEQUENCE        PIC S9(4)     COMP.
00033
00034      12  NT-LAST-MAINT-DT            PIC XX.
00035      12  NT-LAST-MAINT-BY            PIC X(4).
00036      12  NT-LAST-MAINT-HHMMSS        PIC S9(7) COMP-3.
00037
110706*  ALL NOTE LINES ARE RECORD TYPE '1' WITH ALMOST UNLIMITED
110706*     SEQUENCE NUMBERS
110706     12  NT-NOTE-INFORMATION.
110706         16  NT-NOTE-LINE            PIC X(60).
00040          16  FILLER                  PIC X(25).
110706*  BOTH BRANCH LOCATION LINES ARE RECORD TYPE '2' SEQ 1 AND 2
110706     12  NT-LOCATION-INFORMATION REDEFINES
110706                         NT-NOTE-INFORMATION.
110706         16  NT-BRANCH-LOC-LINE      PIC X(60).
110706         16  FILLER                  PIC X(25).
052918* Account special indicator is record type '2', sequence 3
052918     12  filler REDEFINES NT-NOTE-INFORMATION.
052918         16  nt-account-special      PIC X.
052918         16  FILLER                  PIC X(84).
110706*  ALL SHIPPING ADDRESS LINES ARE RECORD TYPE '3'AND
      *     SEQUENCE NUMBER 1 IS NAME LINE 1
      *     SEQUENCE NUMBER 2 IS NAME LINE 2
      *     SEQUENCE NUMBER 3 IS ADDR LINE 1
      *     SEQUENCE NUMBER 4 IS ADDR LINE 2
      *     SEQUENCE NUMBER 5 IS ADDR LINE 3
      *     SEQUENCE NUMBER 6 IS CITY, ST AND ZIP
110706     12  NT-SHIPPING-INFORMATION REDEFINES
110706                         NT-NOTE-INFORMATION.
               16  NT-SHIPPING-LINE        PIC X(60).
110706         16  NT-SHIP-STATE           PIC XX.
110706         16  NT-SHIP-ZIP             PIC X(10).
110706         16  FILLER                  PIC X(13).
00041 *****************************************************************
           EJECT
      ****************************************************************
      *                                                               
      * Copyright (c) 2007-2013 Dell Inc.                             
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
         02  DFHEIV29              PIC S9(9) COMP SYNC.               
         02  DFHEIV30              PIC S9(9) COMP SYNC.               
         02  DFHEIV31              PIC S9(9) COMP SYNC.               
         02  DFHEIV32              PIC S9(4) COMP SYNC.               
         02  DFHEIV33              PIC S9(4) COMP SYNC.               
         02  DFHEIV34              PIC S9(4) COMP SYNC.               
         02  DFHEIV35              PIC S9(4) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
         02  DFHEIVL4              PIC X(255) VALUE SPACE.            
         02  DFHEIVL5              PIC X(255) VALUE SPACE.            
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
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
           02  eibresp          pic s9(8) comp.
           02  eibresp2         pic s9(8) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
       01  DFHCOMMAREA                 PIC X(1024).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6503' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8500-EXIT
           MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE
           MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE
           MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK
           MOVE +1                     TO EMI-NUMBER-OF-LINES
                                          EMI-SWITCH2
           IF EIBCALEN NOT > ZERO
              MOVE UNACCESS-MSG        TO LOGOFF-MSG
              GO TO 8300-SEND-TEXT
           END-IF
           
      * EXEC CICS HANDLE CONDITION
      *        PGMIDERR (9600-PGMIDERR)
      *        ERROR    (9990-ERROR)
      *    END-EXEC
      *    MOVE '"$L.                  ! " #00001450' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031343530' TO DFHEIV0(25:11)
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
111407     IF NOT DISPLAY-CAP
111407        MOVE 'READ'              TO SM-READ
111407        PERFORM 9995-SECURITY-VIOLATION
111407        MOVE ER-0070             TO EMI-ERROR
111407        PERFORM 9900-ERROR-FORMAT
111407                                 THRU 9900-EXIT
111407        GO TO 8100-SEND-INITIAL-MAP
111407     END-IF
           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
              MOVE LOW-VALUES          TO EL6503AO
              MOVE -1                  TO PFKEYL
              MOVE ER-0008             TO EMI-ERROR
              GO TO 8200-SEND-DATAONLY
           END-IF
           
      * EXEC CICS RECEIVE
      *        INTO   (EL6503AI)
      *        MAPSET (WS-MAPSET-NAME)
      *        MAP    (WS-MAP-NAME)
      *    END-EXEC
           MOVE LENGTH OF
            EL6503AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001495' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL6503AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF PFKEYL > ZERO
              IF EIBAID NOT = DFHENTER
                 MOVE ER-0004          TO EMI-ERROR
                 MOVE AL-UNBOF         TO PFKEYA
                 MOVE -1               TO PFKEYL
                 GO TO 8200-SEND-DATAONLY
              ELSE
                 IF PFKEYO NUMERIC AND
                    (PFKEYO > ZERO AND LESS '25')
                    MOVE PF-VALUES (PFKEYI)
                                       TO  EIBAID
                 ELSE
                    MOVE ER-0029       TO  EMI-ERROR
                    MOVE AL-UNBOF      TO  PFKEYA
                    MOVE -1            TO  PFKEYL
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
              MOVE -1                  TO PFKEYL
              GO TO 8200-SEND-DATAONLY
           END-IF
111407     IF NOT MODIFY-CAP
111407        MOVE 'UPDATE'            TO SM-READ
111407        PERFORM 9995-SECURITY-VIOLATION
111407                                 THRU 9995-EXIT
111407        MOVE ER-0070             TO EMI-ERROR
111407        PERFORM 9900-ERROR-FORMAT
111407                                 THRU 9900-EXIT
111407        MOVE AL-UANON            TO CARRIERA GROUPA
111407                                    STATEA  ACCTNOA
111407        GO TO 8100-SEND-INITIAL-MAP
111407     END-IF
           EVALUATE MAINTYPI
              WHEN 'D'
                 PERFORM 2000-DELETE-ERACNT
                                       THRU 2000-EXIT
                 MOVE ER-0000          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 GO TO 8100-SEND-INITIAL-MAP
              WHEN 'C'
                 PERFORM 3000-CHANGE-ERACNT
                                       THRU 3000-EXIT
                 MOVE ER-0000          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              WHEN 'A'
                 PERFORM 4000-ADD-ERACNT
                                       THRU 4000-EXIT
                 MOVE ER-0000          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              WHEN OTHER
                 MOVE ER-0023          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 MOVE -1               TO MAINTYPL
                 MOVE AL-UABON         TO MAINTYPA
           END-EVALUATE
           .
       1000-INITIAL-SCREEN.
      *    MOVE SPACES                 TO PI-PROGRAM-WORK-AREA
           MOVE PI-COMPANY-CD          TO WS-COMPANY-CD
           MOVE PI-ACCT-CARRIER        TO WS-CARRIER
                                          CARRIERO
           MOVE PI-ACCT-GROUPING       TO WS-GROUPING
                                          GROUPO
           MOVE PI-ACCT-STATE          TO WS-STATE
                                          STATEO
           MOVE PI-ACCT-ACCOUNT        TO WS-ACCOUNT
                                          ACCTNOO
           MOVE AL-UANON               TO CARRIERA
                                          GROUPA
                                          STATEA
                                          ACCTNOA
           MOVE ' '                    TO MAINTYPO
           MOVE -1                     TO MAINTYPL
           MOVE '2'                    TO WS-REC-TYPE
           MOVE +0                     TO WS-SEQ-NO
           MOVE WS-CONTROL-PRIMARY     TO WS-SAVE-ERACNT-KEY
           
      * EXEC CICS STARTBR
      *        DATASET   ('ERACNT')
      *        RIDFLD    (WS-CONTROL-PRIMARY)
      *        GTEQ
      *        RESP      (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ERACNT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00001601' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303031363031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF NOT RESP-NORMAL
              GO TO 8100-SEND-INITIAL-MAP
           END-IF
           SET ERACNT-BROWSE-STARTED   TO TRUE
           
      * EXEC CICS READNEXT
      *        DATASET  ('ERACNT')
      *        INTO     (NOTE-FILE)
      *        RIDFLD   (WS-CONTROL-PRIMARY)
      *        RESP     (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV12
           MOVE 'ERACNT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00001611' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031363131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV12, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF (RESP-NORMAL)
              AND (WS-CONTROL-PRIMARY (1:20) =
                 WS-SAVE-ERACNT-KEY (1:20))
              IF (WS-REC-TYPE = '2')
                 AND (WS-SEQ-NO = 1)
                 MOVE NT-BRANCH-LOC-LINE
                                       TO BLINE1O
                 PERFORM 1010-BUILD-LAST-MAINT-INFO
                                       THRU 1010-EXIT
                 
      * EXEC CICS READNEXT
      *             DATASET  ('ERACNT')
      *             INTO     (NOTE-FILE)
      *             RIDFLD   (WS-CONTROL-PRIMARY)
      *             RESP     (WS-RESPONSE)
      *          END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV12
           MOVE 'ERACNT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00001626' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031363236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV12, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              END-IF
           ELSE
              MOVE ER-0006             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF
           IF (RESP-NORMAL)
              AND (WS-CONTROL-PRIMARY (1:20) =
                 WS-SAVE-ERACNT-KEY (1:20))
              IF (WS-REC-TYPE = '2')
                 AND (WS-SEQ-NO = 2)
                 MOVE NT-BRANCH-LOC-LINE
                                       TO BLINE2O
                 PERFORM 1010-BUILD-LAST-MAINT-INFO
                                       THRU 1010-EXIT
                 
      * EXEC CICS READNEXT
      *             DATASET  ('ERACNT')
      *             INTO     (NOTE-FILE)
      *             RIDFLD   (WS-CONTROL-PRIMARY)
      *             RESP     (WS-RESPONSE)
      *          END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV12
           MOVE 'ERACNT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00001647' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031363437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV12, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              END-IF
           END-IF
052918     IF (RESP-NORMAL)
052918        AND (WS-CONTROL-PRIMARY (1:20) =
052918           WS-SAVE-ERACNT-KEY (1:20))
052918        IF (WS-REC-TYPE = '2')
052918           AND (WS-SEQ-NO = 3)
052918           MOVE NT-account-special TO actspco
052918           PERFORM 1010-BUILD-LAST-MAINT-INFO
052918                                 THRU 1010-EXIT
052918           
      * EXEC CICS READNEXT
052918*             DATASET  ('ERACNT')
052918*             INTO     (NOTE-FILE)
052918*             RIDFLD   (WS-CONTROL-PRIMARY)
052918*             RESP     (WS-RESPONSE)
052918*          END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV12
           MOVE 'ERACNT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00001663' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031363633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV12, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
052918        END-IF
052918     END-IF
           IF (RESP-NORMAL)
              AND (WS-CONTROL-PRIMARY (1:20) =
                 WS-SAVE-ERACNT-KEY (1:20))
              IF (WS-REC-TYPE = '3')
                 AND (WS-SEQ-NO = 1)
                 MOVE NT-SHIPPING-LINE TO SNAME1O
                 PERFORM 1010-BUILD-LAST-MAINT-INFO
                                       THRU 1010-EXIT
                 
      * EXEC CICS READNEXT
      *             DATASET  ('ERACNT')
      *             INTO     (NOTE-FILE)
      *             RIDFLD   (WS-CONTROL-PRIMARY)
      *             RESP     (WS-RESPONSE)
      *          END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV12
           MOVE 'ERACNT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00001679' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031363739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV12, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              END-IF
           END-IF
           IF (RESP-NORMAL)
              AND (WS-CONTROL-PRIMARY (1:20) =
                 WS-SAVE-ERACNT-KEY (1:20))
              IF (WS-REC-TYPE = '3')
                 AND (WS-SEQ-NO = 2)
                 MOVE NT-SHIPPING-LINE TO SNAME2O
                 PERFORM 1010-BUILD-LAST-MAINT-INFO
                                       THRU 1010-EXIT
                 
      * EXEC CICS READNEXT
      *             DATASET  ('ERACNT')
      *             INTO     (NOTE-FILE)
      *             RIDFLD   (WS-CONTROL-PRIMARY)
      *             RESP     (WS-RESPONSE)
      *          END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV12
           MOVE 'ERACNT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00001695' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031363935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV12, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              END-IF
           END-IF
           IF (RESP-NORMAL)
              AND (WS-CONTROL-PRIMARY (1:20) =
                 WS-SAVE-ERACNT-KEY (1:20))
              IF (WS-REC-TYPE = '3')
                 AND (WS-SEQ-NO = 3)
                 MOVE NT-SHIPPING-LINE TO SADDR1O
                 PERFORM 1010-BUILD-LAST-MAINT-INFO
                                       THRU 1010-EXIT
                 
      * EXEC CICS READNEXT
      *             DATASET  ('ERACNT')
      *             INTO     (NOTE-FILE)
      *             RIDFLD   (WS-CONTROL-PRIMARY)
      *             RESP     (WS-RESPONSE)
      *          END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV12
           MOVE 'ERACNT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00001711' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031373131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV12, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              END-IF
           END-IF
           IF (RESP-NORMAL)
              AND (WS-CONTROL-PRIMARY (1:20) =
                 WS-SAVE-ERACNT-KEY (1:20))
              IF (WS-REC-TYPE = '3')
                 AND (WS-SEQ-NO = 4)
                 MOVE NT-SHIPPING-LINE TO SADDR2O
                 PERFORM 1010-BUILD-LAST-MAINT-INFO
                                       THRU 1010-EXIT
                 
      * EXEC CICS READNEXT
      *             DATASET  ('ERACNT')
      *             INTO     (NOTE-FILE)
      *             RIDFLD   (WS-CONTROL-PRIMARY)
      *             RESP     (WS-RESPONSE)
      *          END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV12
           MOVE 'ERACNT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00001727' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031373237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV12, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              END-IF
           END-IF
           IF (RESP-NORMAL)
              AND (WS-CONTROL-PRIMARY (1:20) =
                 WS-SAVE-ERACNT-KEY (1:20))
              IF (WS-REC-TYPE = '3')
                 AND (WS-SEQ-NO = 5)
                 MOVE NT-SHIPPING-LINE TO SADDR3O
                 PERFORM 1010-BUILD-LAST-MAINT-INFO
                                       THRU 1010-EXIT
                 
      * EXEC CICS READNEXT
      *             DATASET  ('ERACNT')
      *             INTO     (NOTE-FILE)
      *             RIDFLD   (WS-CONTROL-PRIMARY)
      *             RESP     (WS-RESPONSE)
      *          END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV12
           MOVE 'ERACNT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00001743' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031373433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV12, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              END-IF
           END-IF
           IF (RESP-NORMAL)
              AND (WS-CONTROL-PRIMARY (1:20) =
                 WS-SAVE-ERACNT-KEY (1:20))
              IF (WS-REC-TYPE = '3')
                 AND (WS-SEQ-NO = 6)
                 MOVE NT-SHIPPING-LINE TO SCITYO
                 MOVE NT-SHIP-STATE    TO SSTATEO
                 MOVE NT-SHIP-ZIP      TO SZIPO
                 PERFORM 1010-BUILD-LAST-MAINT-INFO
                                       THRU 1010-EXIT
              END-IF
           END-IF
           IF ERACNT-BROWSE-STARTED
              
      * EXEC CICS ENDBR
      *          DATASET    ('ERACNT')
      *       END-EXEC
           MOVE 'ERACNT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001764' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           END-IF
           MOVE -1                     TO MAINTYPL
           GO TO 8100-SEND-INITIAL-MAP
           .
       1000-EXIT.
           EXIT.
       1010-BUILD-LAST-MAINT-INFO.
           MOVE NT-LAST-MAINT-BY       TO LSTUSRO
           MOVE NT-LAST-MAINT-HHMMSS   TO TIME-IN
           MOVE TIME-OUT               TO LSTTIMEO
           MOVE NT-LAST-MAINT-DT       TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO LSTDTEO
           END-IF
           .
       1010-EXIT.
           EXIT.
       2000-DELETE-ERACNT.
           MOVE PI-COMPANY-CD          TO WS-COMPANY-CD
           MOVE PI-ACCT-CARRIER        TO WS-CARRIER
           MOVE PI-ACCT-GROUPING       TO WS-GROUPING
           MOVE PI-ACCT-STATE          TO WS-STATE
           MOVE PI-ACCT-ACCOUNT        TO WS-ACCOUNT
           MOVE '2'                    TO WS-REC-TYPE
           MOVE +1                     TO WS-SEQ-NO
           
      * EXEC CICS READ
      *       UPDATE
      *       DATASET  ('ERACNT')
      *       INTO     (NOTE-FILE)
      *       RIDFLD   (WS-CONTROL-PRIMARY)
      *       RESP     (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00001795' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303031373935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              
      * EXEC CICS DELETE
      *          DATASET    ('ERACNT')
      *          RESP       (WS-RESPONSE)
      *       END-EXEC
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&(                    &  N#00001803' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303031383033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           END-IF
           MOVE PI-COMPANY-CD          TO WS-COMPANY-CD
           MOVE PI-ACCT-CARRIER        TO WS-CARRIER
           MOVE PI-ACCT-GROUPING       TO WS-GROUPING
           MOVE PI-ACCT-STATE          TO WS-STATE
           MOVE PI-ACCT-ACCOUNT        TO WS-ACCOUNT
           MOVE '2'                    TO WS-REC-TYPE
           MOVE +2                     TO WS-SEQ-NO
           
      * EXEC CICS READ
      *       UPDATE
      *       DATASET  ('ERACNT')
      *       INTO     (NOTE-FILE)
      *       RIDFLD   (WS-CONTROL-PRIMARY)
      *       RESP     (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00001815' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303031383135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              
      * EXEC CICS DELETE
      *          DATASET    ('ERACNT')
      *          RESP       (WS-RESPONSE)
      *       END-EXEC
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&(                    &  N#00001823' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303031383233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           END-IF
052918     MOVE PI-COMPANY-CD          TO WS-COMPANY-CD
052918     MOVE PI-ACCT-CARRIER        TO WS-CARRIER
052918     MOVE PI-ACCT-GROUPING       TO WS-GROUPING
052918     MOVE PI-ACCT-STATE          TO WS-STATE
052918     MOVE PI-ACCT-ACCOUNT        TO WS-ACCOUNT
052918     MOVE '2'                    TO WS-REC-TYPE
052918     MOVE +3                     TO WS-SEQ-NO
052918
052918     
      * EXEC CICS READ
052918*       UPDATE
052918*       DATASET  ('ERACNT')
052918*       INTO     (NOTE-FILE)
052918*       RIDFLD   (WS-CONTROL-PRIMARY)
052918*       RESP     (WS-RESPONSE)
052918*    END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00001836' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303031383336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
052918
052918     IF RESP-NORMAL
052918        
      * EXEC CICS DELETE
052918*          DATASET    ('ERACNT')
052918*          RESP       (WS-RESPONSE)
052918*       END-EXEC
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&(                    &  N#00001845' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303031383435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
052918     END-IF
           PERFORM VARYING B1 FROM +1 BY +1 UNTIL
              (B1 > +6)
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE B1                  TO WS-SEQ-NO
              
      * EXEC CICS READ
      *          UPDATE
      *          DATASET  ('ERACNT')
      *          INTO     (NOTE-FILE)
      *          RIDFLD   (WS-CONTROL-PRIMARY)
      *          RESP     (WS-RESPONSE)
      *       END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00001859' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303031383539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NORMAL
                 
      * EXEC CICS DELETE
      *             DATASET    ('ERACNT')
      *             RESP       (WS-RESPONSE)
      *          END-EXEC
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&(                    &  N#00001867' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303031383637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              END-IF
           END-PERFORM
           .
       2000-EXIT.
           EXIT.
       3000-CHANGE-ERACNT.
052918     perform 3500-edit-screen-data
052918                                 thru 3500-exit
052918
052918     if not emi-no-errors
052918        go to 8200-send-dataonly
052918     end-if
           IF BLINE1L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '2'                 TO WS-REC-TYPE
              MOVE +1                  TO WS-SEQ-NO
              
      * EXEC CICS READ
      *          UPDATE
      *          DATASET  ('ERACNT')
      *          INTO     (NOTE-FILE)
      *          RIDFLD   (WS-CONTROL-PRIMARY)
      *          RESP     (WS-RESPONSE)
      *       END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00001891' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303031383931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NORMAL
                 MOVE BLINE1I          TO NT-BRANCH-LOC-LINE
                 PERFORM 5020-REWRITE-ERACNT
                                       THRU 5020-EXIT
              ELSE
                 PERFORM 4010-ADD-BLINE1
                                       THRU 4010-EXIT
              END-IF
           END-IF
           IF BLINE2L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '2'                 TO WS-REC-TYPE
              MOVE +2                  TO WS-SEQ-NO
              
      * EXEC CICS READ
      *          UPDATE
      *          DATASET  ('ERACNT')
      *          INTO     (NOTE-FILE)
      *          RIDFLD   (WS-CONTROL-PRIMARY)
      *          RESP     (WS-RESPONSE)
      *       END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00001915' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303031393135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NORMAL
                 MOVE BLINE2I          TO NT-BRANCH-LOC-LINE
                 PERFORM 5020-REWRITE-ERACNT
                                       THRU 5020-EXIT
              ELSE
                 PERFORM 4012-ADD-BLINE2
                                       THRU 4012-EXIT
              END-IF
           END-IF
052918     IF actspcl > ZEROS
052918        MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
052918        MOVE PI-ACCT-CARRIER     TO WS-CARRIER
052918        MOVE PI-ACCT-GROUPING    TO WS-GROUPING
052918        MOVE PI-ACCT-STATE       TO WS-STATE
052918        MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
052918        MOVE '2'                 TO WS-REC-TYPE
052918        MOVE +3                  TO WS-SEQ-NO
052918
052918        
      * EXEC CICS READ
052918*          UPDATE
052918*          DATASET  ('ERACNT')
052918*          INTO     (NOTE-FILE)
052918*          RIDFLD   (WS-CONTROL-PRIMARY)
052918*          RESP     (WS-RESPONSE)
052918*       END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00001940' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303031393430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
052918
052918        IF RESP-NORMAL
052918           MOVE actspci          TO NT-account-special
052918           PERFORM 5020-REWRITE-ERACNT
052918                                 THRU 5020-EXIT
052918        ELSE
052918           PERFORM 4013-ADD-actspc
052918                                 THRU 4013-EXIT
052918        END-IF
052918     END-IF
           IF SNAME1L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +1                  TO WS-SEQ-NO
              
      * EXEC CICS READ
      *          UPDATE
      *          DATASET  ('ERACNT')
      *          INTO     (NOTE-FILE)
      *          RIDFLD   (WS-CONTROL-PRIMARY)
      *          RESP     (WS-RESPONSE)
      *       END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00001965' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303031393635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NORMAL
                 MOVE SNAME1I          TO NT-SHIPPING-LINE
                 PERFORM 5020-REWRITE-ERACNT
                                       THRU 5020-EXIT
              ELSE
                 PERFORM 4014-ADD-SNAME1
                                       THRU 4014-EXIT
              END-IF
           END-IF
           IF SNAME2L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +2                  TO WS-SEQ-NO
              
      * EXEC CICS READ
      *          UPDATE
      *          DATASET  ('ERACNT')
      *          INTO     (NOTE-FILE)
      *          RIDFLD   (WS-CONTROL-PRIMARY)
      *          RESP     (WS-RESPONSE)
      *       END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00001989' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303031393839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NORMAL
                 MOVE SNAME2I          TO NT-SHIPPING-LINE
                 PERFORM 5020-REWRITE-ERACNT
                                       THRU 5020-EXIT
              ELSE
                 PERFORM 4016-ADD-SNAME2
                                       THRU 4016-EXIT
              END-IF
           END-IF
           IF SADDR1L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +3                  TO WS-SEQ-NO
              
      * EXEC CICS READ
      *          UPDATE
      *          DATASET  ('ERACNT')
      *          INTO     (NOTE-FILE)
      *          RIDFLD   (WS-CONTROL-PRIMARY)
      *          RESP     (WS-RESPONSE)
      *       END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00002013' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032303133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NORMAL
                 MOVE SADDR1I          TO NT-SHIPPING-LINE
                 PERFORM 5020-REWRITE-ERACNT
                                       THRU 5020-EXIT
              ELSE
                 PERFORM 4018-ADD-SADDR1
                                       THRU 4018-EXIT
              END-IF
           END-IF
           IF SADDR2L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +4                  TO WS-SEQ-NO
              
      * EXEC CICS READ
      *          UPDATE
      *          DATASET  ('ERACNT')
      *          INTO     (NOTE-FILE)
      *          RIDFLD   (WS-CONTROL-PRIMARY)
      *          RESP     (WS-RESPONSE)
      *       END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00002037' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032303337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NORMAL
                 MOVE SADDR2I          TO NT-SHIPPING-LINE
                 PERFORM 5020-REWRITE-ERACNT
                                       THRU 5020-EXIT
              ELSE
                 PERFORM 4020-ADD-SADDR2
                                       THRU 4020-EXIT
              END-IF
           END-IF
           IF SADDR3L    > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +5                  TO WS-SEQ-NO
              
      * EXEC CICS READ
      *          UPDATE
      *          DATASET  ('ERACNT')
      *          INTO     (NOTE-FILE)
      *          RIDFLD   (WS-CONTROL-PRIMARY)
      *          RESP     (WS-RESPONSE)
      *       END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00002061' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032303631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NORMAL
                 MOVE SADDR3I          TO NT-SHIPPING-LINE
                 PERFORM 5020-REWRITE-ERACNT
                                       THRU 5020-EXIT
              ELSE
                 PERFORM 4022-ADD-SADDR3
                                       THRU 4022-EXIT
              END-IF
           END-IF
           IF (SCITYL     > ZEROS)
              OR (SSTATEL > ZEROS)
              OR (SZIPL   > ZEROS)
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +6                  TO WS-SEQ-NO
              
      * EXEC CICS READ
      *          UPDATE
      *          DATASET  ('ERACNT')
      *          INTO     (NOTE-FILE)
      *          RIDFLD   (WS-CONTROL-PRIMARY)
      *          RESP     (WS-RESPONSE)
      *       END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00002087' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032303837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NORMAL
                 IF SCITYL > ZEROS
                    MOVE SCITYI        TO NT-SHIPPING-LINE
                 END-IF
                 IF SSTATEL > ZEROS
                    MOVE SSTATEI       TO NT-SHIP-STATE
                 END-IF
                 IF SZIPL > ZEROS
                    MOVE SZIPI         TO NT-SHIP-ZIP
                 END-IF
                 PERFORM 5020-REWRITE-ERACNT
                                       THRU 5020-EXIT
              ELSE
                 PERFORM 4024-ADD-SCITYSTZP
                                       THRU 4024-EXIT
              END-IF
           END-IF
           .
       3000-EXIT.
           EXIT.
052918 3500-edit-screen-data.
052918
052918     if actspcl > zeros
052918        if actspci <> 'Y' and 'N' and ' '
052918           move al-uabon         to actspca
052918           move -1               to actspcl
052918           move er-3271          to emi-error
052918           perform 9900-error-format
052918                                 thru 9900-exit
052918        else
052918           move al-uanon         to actspca
052918        end-if
052918     end-if
052918
052918
052918     .
052918 3500-exit.
052918     exit.
       4000-ADD-ERACNT.
052918     perform 3500-edit-screen-data
052918                                 thru 3500-exit
052918
052918     if not emi-no-errors
052918        go to 8200-send-dataonly
052918     end-if
           PERFORM 4010-ADD-BLINE1     THRU 4010-EXIT
           PERFORM 4012-ADD-BLINE2     THRU 4012-EXIT
052918     PERFORM 4013-ADD-actspc     THRU 4013-EXIT
           PERFORM 4014-ADD-SNAME1     THRU 4014-EXIT
           PERFORM 4016-ADD-SNAME2     THRU 4016-EXIT
           PERFORM 4018-ADD-SADDR1     THRU 4018-EXIT
           PERFORM 4020-ADD-SADDR2     THRU 4020-EXIT
           PERFORM 4022-ADD-SADDR3     THRU 4022-EXIT
           PERFORM 4024-ADD-SCITYSTZP  THRU 4024-EXIT
           .
       4000-EXIT.
           EXIT.
       4010-ADD-BLINE1.
           IF BLINE1L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '2'                 TO WS-REC-TYPE
              MOVE +1                  TO WS-SEQ-NO
              
      * EXEC CICS READ
      *          DATASET  ('ERACNT')
      *          INTO     (NOTE-FILE)
      *          RIDFLD   (WS-CONTROL-PRIMARY)
      *          RESP     (WS-RESPONSE)
      *       END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00002160' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032313630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NOTFND
                 PERFORM 5010-BUILD-ERACNT
                                       THRU 5010-EXIT
                 MOVE BLINE1I          TO NT-BRANCH-LOC-LINE
                 PERFORM 5000-WRITE-ERACNT
                                       THRU 5000-EXIT
              END-IF
           END-IF
           .
       4010-EXIT.
           EXIT.
       4012-ADD-BLINE2.
           IF BLINE2L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '2'                 TO WS-REC-TYPE
              MOVE +2                  TO WS-SEQ-NO
              
      * EXEC CICS READ
      *          DATASET  ('ERACNT')
      *          INTO     (NOTE-FILE)
      *          RIDFLD   (WS-CONTROL-PRIMARY)
      *          RESP     (WS-RESPONSE)
      *       END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00002186' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032313836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NOTFND
                 PERFORM 5010-BUILD-ERACNT
                                       THRU 5010-EXIT
                 MOVE BLINE2I          TO NT-BRANCH-LOC-LINE
                 PERFORM 5000-WRITE-ERACNT
                                       THRU 5000-EXIT
              END-IF
           END-IF
           .
       4012-EXIT.
           EXIT.
052918 4013-ADD-actspc.
052918
052918     IF actspcL > ZEROS
052918        MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
052918        MOVE PI-ACCT-CARRIER     TO WS-CARRIER
052918        MOVE PI-ACCT-GROUPING    TO WS-GROUPING
052918        MOVE PI-ACCT-STATE       TO WS-STATE
052918        MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
052918        MOVE '2'                 TO WS-REC-TYPE
052918        MOVE +3                  TO WS-SEQ-NO
052918
052918        
      * EXEC CICS READ
052918*          DATASET  ('ERACNT')
052918*          INTO     (NOTE-FILE)
052918*          RIDFLD   (WS-CONTROL-PRIMARY)
052918*          RESP     (WS-RESPONSE)
052918*       END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00002214' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032323134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
052918
052918        IF RESP-NOTFND
052918           PERFORM 5010-BUILD-ERACNT
052918                                 THRU 5010-EXIT
052918           MOVE actspci          TO NT-account-special
052918           PERFORM 5000-WRITE-ERACNT
052918                                 THRU 5000-EXIT
052918        END-IF
052918     END-IF
052918
052918     .
052918 4013-EXIT.
052918     EXIT.
       4014-ADD-SNAME1.
           IF SNAME1L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +1                  TO WS-SEQ-NO
              
      * EXEC CICS READ
      *          DATASET  ('ERACNT')
      *          INTO     (NOTE-FILE)
      *          RIDFLD   (WS-CONTROL-PRIMARY)
      *          RESP     (WS-RESPONSE)
      *       END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00002242' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032323432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NOTFND
                 PERFORM 5010-BUILD-ERACNT
                                       THRU 5010-EXIT
                 MOVE SNAME1I          TO NT-SHIPPING-LINE
                 PERFORM 5000-WRITE-ERACNT
                                       THRU 5000-EXIT
              END-IF
           END-IF
           .
       4014-EXIT.
           EXIT.
       4016-ADD-SNAME2.
           IF SNAME2L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +2                  TO WS-SEQ-NO
              
      * EXEC CICS READ
      *          DATASET  ('ERACNT')
      *          INTO     (NOTE-FILE)
      *          RIDFLD   (WS-CONTROL-PRIMARY)
      *          RESP     (WS-RESPONSE)
      *       END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00002268' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032323638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NOTFND
                 PERFORM 5010-BUILD-ERACNT
                                       THRU 5010-EXIT
                 MOVE SNAME2I          TO NT-SHIPPING-LINE
                 PERFORM 5000-WRITE-ERACNT
                                       THRU 5000-EXIT
              END-IF
           END-IF
           .
       4016-EXIT.
           EXIT.
       4018-ADD-SADDR1.
           IF SADDR1L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +3                  TO WS-SEQ-NO
              
      * EXEC CICS READ
      *          DATASET  ('ERACNT')
      *          INTO     (NOTE-FILE)
      *          RIDFLD   (WS-CONTROL-PRIMARY)
      *          RESP     (WS-RESPONSE)
      *       END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00002294' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032323934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NOTFND
                 PERFORM 5010-BUILD-ERACNT
                                       THRU 5010-EXIT
                 MOVE SADDR1I          TO NT-SHIPPING-LINE
                 PERFORM 5000-WRITE-ERACNT
                                       THRU 5000-EXIT
              END-IF
           END-IF
           .
       4018-EXIT.
           EXIT.
       4020-ADD-SADDR2.
           IF SADDR2L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +4                  TO WS-SEQ-NO
              
      * EXEC CICS READ
      *          DATASET  ('ERACNT')
      *          INTO     (NOTE-FILE)
      *          RIDFLD   (WS-CONTROL-PRIMARY)
      *          RESP     (WS-RESPONSE)
      *       END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00002320' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032333230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NOTFND
                 PERFORM 5010-BUILD-ERACNT
                                       THRU 5010-EXIT
                 MOVE SADDR2I          TO NT-SHIPPING-LINE
                 PERFORM 5000-WRITE-ERACNT
                                       THRU 5000-EXIT
              END-IF
           END-IF
          .
       4020-EXIT.
           EXIT.
       4022-ADD-SADDR3.
           IF SADDR3L    > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +5                  TO WS-SEQ-NO
              
      * EXEC CICS READ
      *          DATASET  ('ERACNT')
      *          INTO     (NOTE-FILE)
      *          RIDFLD   (WS-CONTROL-PRIMARY)
      *          RESP     (WS-RESPONSE)
      *       END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00002346' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032333436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NOTFND
                 PERFORM 5010-BUILD-ERACNT
                                       THRU 5010-EXIT
                 MOVE SADDR3I          TO NT-SHIPPING-LINE
                 PERFORM 5000-WRITE-ERACNT
                                       THRU 5000-EXIT
              END-IF
           END-IF
           .
       4022-EXIT.
           EXIT.
       4024-ADD-SCITYSTZP.
           IF (SCITYL     > ZEROS)
              OR (SSTATEL > ZEROS)
              OR (SZIPL   > ZEROS)
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +6                  TO WS-SEQ-NO
              
      * EXEC CICS READ
      *          DATASET  ('ERACNT')
      *          INTO     (NOTE-FILE)
      *          RIDFLD   (WS-CONTROL-PRIMARY)
      *          RESP     (WS-RESPONSE)
      *       END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00002374' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032333734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NOTFND
                 PERFORM 5010-BUILD-ERACNT
                                       THRU 5010-EXIT
                 IF SCITYL > ZEROS
                    MOVE SCITYI        TO NT-SHIPPING-LINE
                 END-IF
                 IF SSTATEL > ZEROS
                    MOVE SSTATEI       TO NT-SHIP-STATE
                 END-IF
                 IF SZIPL > ZEROS
                    MOVE SZIPI         TO NT-SHIP-ZIP
                 END-IF
                 PERFORM 5000-WRITE-ERACNT
                                       THRU 5000-EXIT
              END-IF
           END-IF
           .
       4024-EXIT.
           EXIT.
       5000-WRITE-ERACNT.
           MOVE SAVE-BIN-DATE          TO NT-LAST-MAINT-DT
           MOVE EIBTIME                TO NT-LAST-MAINT-HHMMSS
           MOVE PI-PROCESSOR-ID        TO NT-LAST-MAINT-BY
           
      * EXEC CICS WRITE
      *         FROM    (NOTE-FILE)
      *         DATASET ('ERACNT')
      *         RIDFLD  (WS-CONTROL-PRIMARY)
      *         RESP    (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00002403' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303032343033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       5000-EXIT.
           EXIT.
       5010-BUILD-ERACNT.
           MOVE SPACES                 TO NOTE-FILE
           MOVE 'NT'                   TO NT-FILE-ID
           MOVE WS-CONTROL-PRIMARY     TO NT-CONTROL-PRIMARY
           MOVE SAVE-BIN-DATE          TO NT-LAST-MAINT-DT
           MOVE EIBTIME                TO NT-LAST-MAINT-HHMMSS
           MOVE PI-PROCESSOR-ID        TO NT-LAST-MAINT-BY
           .
       5010-EXIT.
           EXIT.
       5020-REWRITE-ERACNT.
           MOVE SAVE-BIN-DATE          TO NT-LAST-MAINT-DT
           MOVE EIBTIME                TO NT-LAST-MAINT-HHMMSS
           MOVE PI-PROCESSOR-ID        TO NT-LAST-MAINT-BY
           
      * EXEC CICS REWRITE
      *         FROM    (NOTE-FILE)
      *         DATASET ('ERACNT')
      *         RESP    (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&& L                  %  N#00002426' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303032343236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       5020-EXIT.
           EXIT.
       8100-SEND-INITIAL-MAP.
           MOVE -1                     TO  MAINTYPL
           MOVE SAVE-DATE              TO  RUNDATEO
           MOVE EIBTIME                TO  TIME-IN
           MOVE TIME-OUT               TO  RUNTIMEO
           IF EMI-ERROR NOT = ZERO
               PERFORM 9900-ERROR-FORMAT
           END-IF
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O
           
      * EXEC CICS SEND
      *        FROM   (EL6503AO)
      *        MAPSET (WS-MAPSET-NAME)
      *        MAP    (WS-MAP-NAME)
      *        CURSOR ERASE
      *    END-EXEC.
           MOVE LENGTH OF
            EL6503AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00002443' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL6503AO, 
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
       8200-SEND-DATAONLY.
           MOVE SAVE-DATE              TO  RUNDATEO
           MOVE EIBTIME                TO  TIME-IN
           MOVE TIME-OUT               TO  RUNTIMEO
           IF EMI-ERROR NOT = ZERO
              PERFORM 9900-ERROR-FORMAT
           END-IF
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O
           
      * EXEC CICS SEND DATAONLY
      *        FROM   (EL6503AO)
      *        MAPSET (WS-MAPSET-NAME)
      *        MAP    (WS-MAP-NAME)
      *        CURSOR
      *    END-EXEC.
           MOVE LENGTH OF
            EL6503AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00002461' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL6503AO, 
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
       8300-SEND-TEXT.
           
      * EXEC CICS SEND TEXT
      *        FROM   (LOGOFF-TEXT)
      *        LENGTH (LOGOFF-LENGTH)
      *        ERASE  FREEKB
      *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00002472' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343732' TO DFHEIV0(25:11)
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
      *    MOVE '.(                    ''   #00002477' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       8300-EXIT.
            EXIT.
       8400-LOG-JOURNAL-RECORD.
           IF PI-JOURNAL-FILE-ID = 0
              GO TO 8400-EXIT
           END-IF
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
       8500-DATE-CONVERSION.
           
      * EXEC CICS LINK
      *        PROGRAM  ('ELDATCV')
      *        COMMAREA (DATE-CONVERSION-DATA)
      *        LENGTH   (DC-COMM-LENGTH)
      *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00002497' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       8500-EXIT.
            EXIT.
       9000-RETURN-CICS.
           MOVE 'EL005'                TO  THIS-PGM.
           MOVE EIBAID                 TO  PI-ENTRY-CD-1.
           PERFORM 9300-XCTL.
       9000-EXIT.
            EXIT.
       9100-RETURN-TRAN.
           MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
           MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
           
      * EXEC CICS RETURN
      *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
      *        LENGTH   (PI-COMM-LENGTH)
      *        TRANSID  (WS-TRANS-ID)
      *    END-EXEC.
      *    MOVE '.(CT                  ''   #00002513' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
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
      *    MOVE '.$C                   %   #00002524' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9300-EXIT.
            EXIT.
           EJECT
      *****************************************************************
       9400-CLEAR.
      *****************************************************************
           MOVE PI-RETURN-TO-PROGRAM   TO THIS-PGM
           PERFORM 9300-XCTL
           .
       9400-EXIT.
            EXIT.
      *****************************************************************
       9600-PGMIDERR.
      *****************************************************************
           
      * EXEC CICS HANDLE CONDITION
      *        PGMIDERR (8300-SEND-TEXT)
      *    END-EXEC.
      *    MOVE '"$L                   ! # #00002543' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303032353433' TO DFHEIV0(25:11)
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
      *    MOVE '."C                   (   #00002561' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
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
      *    MOVE '."C                   (   #00002574' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
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
      *    MOVE '."C                   (   #00002603' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
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
           MOVE 'EL6503' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6503' TO DFHEIV1
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
           MOVE 'EL6503' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
