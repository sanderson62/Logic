       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 EL614 .
      *                            VMOD=2.001.
      *
      *
      *AUTHOR.    PABLO.
      *           COLLEYVILLE TX.
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
      *    FOR THE  EOB   CODES IN THE CLAIM SYSTEM.
      *
      *    SCREENS     - EL614A - EOB CODE MAINTENANCE
      *
      *    ENTERED BY  - EL101 - SYSTEM ADMINISTRATION MENU
      *                  EL156 - PAYMENT WORKSHEET
      *                  EL6317 - CERTIFICATE VERIFICATION
      *    EXIT TO     - EL101 - SYSTEM ADMINISTRATION MENU
      *                  EL156 - PAYMENT WORKSHEET
      *                  EL6317 - CERTIFICATE VERIFICATION
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
      * 083110    2009122800001  PEMA  NEW PROGRAM
011811* 011811    2009122800001  AJRA  FIX SELECTION
042211* 042211    2011040600001  PEMA  FIX ISSUE NO RECORDS FOUND
081511* 081511    2011022800001  PEMA  ADMIN SERV NAPERSOFT CHANGES
121012* 121012    2012101700002  AJRA  HIGHLIGHT REASON CODES
112113* 112113    2013090300001  AJRA  NAPERSOFT PHASE 2
120318* 120318  IR2018112800001  PEMA  Fix memory problem
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
       77  FILLER  PIC X(32)  VALUE '********************************'.
       77  FILLER  PIC X(32)  VALUE '*   EL614  WORKING STORAGE     *'.
       77  FILLER  PIC X(32)  VALUE '***********VMOD=2.001 **********'.
       77  i1                 pic s999  comp-3 value +0.
       77  S1                 PIC S999  COMP-3 VALUE +0.
121012 77  S2                 PIC S999  COMP-3 VALUE +0.
       77  E1                 PIC S999  COMP-3 VALUE +0.
       77  WS-DUP-EOB-SW      PIC X VALUE ' '.
           88  FOUND-DUP-EOB     VALUE 'Y'.
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
       01  WS-PASSED-FROM-CALL-PGM     PIC X(60)  VALUE SPACES.
       01  WS-DATE-AREA.
           05  SAVE-DATE               PIC X(8)    VALUE SPACES.
           05  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.
       01  WS-SELECTED-EOB-CODES.
           05  WS-SELECTED-CODES OCCURS 12.
               10  WS-SEL-EOB-CODE     PIC XXXX.
               10  FILLER              PIC X.
       01  FILLER                          COMP-3.
           05  TIME-IN                     PIC S9(7)   VALUE ZERO.
           05  TIME-OUT                    REDEFINES
               TIME-IN                     PIC S9(3)V9(4).
       01  FILLER                          COMP SYNC.
           05  WS-INDEX                    PIC S9(4)   VALUE ZERO.
           05  WS-JOURNAL-FILE-ID          PIC S9(4)   VALUE +1.
           05  WS-JOURNAL-RECORD-LENGTH    PIC S9(4)   VALUE +773.
112113 01  FILLER.
112113     05  QID.
112113         10  QID-TERM            PIC X(4).
112113         10  FILLER              PIC X(4)    VALUE '614A'.
112113     05  QID-ITEM                PIC S9(4)   COMP VALUE +0.
112113     05  NUM-LINES-PER-SCREEN    PIC 99      VALUE 17.
112113     05  TS-LENGTH               PIC S9(4)   VALUE +3360 COMP.
       01  ws-endbr-sw                 pic x value ' '.
           88  true-endbr               value 'Y'.
       01  FILLER.
           05  DEEDIT-FIELD        PIC  X(10).
           05  DEEDIT-FIELD-V0  REDEFINES
               DEEDIT-FIELD        PIC S9(10).
           05  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL              VALUE +00.
               88  RESP-ERROR               VALUE +01.
               88  RESP-NOTFND              VALUE +13.
               88  RESP-NOTOPEN             VALUE +19.
               88  RESP-ENDFILE             VALUE +20.
           05  WS-ELEOBC-KEY.
               10  WS-COMPANY-CD           PIC X      VALUE LOW-VALUES.
               10  WS-EOB-REC-TYPE         PIC X      VALUE LOW-VALUES.
               10  WS-EOB-CODE             PIC X(4)   VALUE LOW-VALUES.
               10  FILLER                  PIC X(9)   VALUE LOW-VALUES.
           05  WS-MAPSET-NAME              PIC X(8)  VALUE 'EL614S'.
           05  WS-MAP-NAME                 PIC X(8)  VALUE 'EL614A'.
           05  FILLER                      REDEFINES
               WS-MAP-NAME.
               10  FILLER                  PIC XX.
               10  WS-MAP-NUMBER           PIC X(4).
               10  FILLER                  PIC XX.
           05  THIS-PGM                    PIC X(8)  VALUE 'EL614'.
           05  WS-JOURNAL-TYPE-ID          PIC XX      VALUE 'EL'.
           05  WS-TRANS-ID                 PIC X(4)    VALUE 'EXAI'.
           05  WS-TEMP-STORAGE-KEY.
               10  WS-TS-TERM-ID           PIC X(4)    VALUE 'XXXX'.
               10  FILLER                  PIC X(4)    VALUE '614'.
121012     05  WS-REASON-CODE-PASSED       PIC X(26).
121012     05  WS-TABLE-CODE.
121012         10  WS-TAB-CODE-1           PIC X(1).
121012         10  WS-TAB-CODE-2-4         PIC X(3).
121012     05  WS-SCREEN-CODE.
121012         10  WS-SCR-CODE-1-3         PIC X(3).
121012         10  WS-SCR-CODE-4           PIC X(1).
           05  WS-ERROR-MESSAGE-AREA.
               10  ER-0004             PIC 9(4)   VALUE 0004.
               10  ER-0006             PIC 9(4)   VALUE 0006.
               10  ER-0008             PIC 9(4)   VALUE 0008.
               10  ER-0023             PIC 9(4)   VALUE 0023.
               10  ER-0029             PIC 9(4)   VALUE 0029.
112113         10  ER-0033             PIC 9(4)   VALUE 0033.
112113         10  ER-1698             PIC 9(4)   VALUE 1698.
112113         10  ER-1699             PIC 9(4)   VALUE 1699.
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
           12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
               16  PI-SELECTED-EOB-CODES.
                   20  PI-SELECTED-CODES OCCURS 12.
                       24  PI-EOB-CODE     PIC XXXX.
                       24  FILLER          PIC X.
               16  PI-1ST-TIME-SW          PIC S9     COMP-3.
               16  PI-MODE                 PIC X.
                   88  ADD-FUNCTION         VALUE 'A'.
               16  PI-DN-RCODE             PIC XXXX.
               16  PI-DN-TOP-KEY           PIC X(15).
               16  PI-DN-BOT-KEY           PIC X(15).
               16  PI-DN-KEYS OCCURS 17.
                   20  PI-DN-KEY           PIC X(15).
               16  PI-LINE-COUNT           PIC S9(3)  COMP-3.
               16  PI-BROWSE-SW            PIC S9     COMP-3.
               16  PI-SHOW-SW              PIC S9     COMP-3.
               16  PI-CHANGE-SW            PIC S9     COMP-3.
               16  PI-RECORD-TYPE          PIC X.
112113         16  pi-FILLER               PIC X(244).
112113         16  PI-TOTAL-LINES          PIC S9(3)   comp-3.
112113         16  PI-CURRENT-LINE         PIC S9(3)   COMP-3.
112113         16  PI-TOP-LINE-NUM         PIC S9(3)   COMP-3.
112113         16  PI-BOT-LINE-NUM         PIC S9(3)   COMP-3.
112113         16  PI-TEMP-STOR-ITEMS      PIC S9(4)   COMP.
121012         16  PI-REASON-CODE-IND.
121012             20 PI-REASON-CODE-FLAG  OCCURS 26 TIMES PIC X(1).
121012         16  PI-EXPANDED-VIEW        PIC X(1).
               16  pi-6318-pass            PIC X.
               16  filler                  pic x.
      *                                COPY EL614S.
       01  EL614AI.
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
           05  SELHL PIC S9(0004) COMP.
           05  SELHF PIC  X(0001).
           05  FILLER REDEFINES SELHF.
               10  SELHA PIC  X(0001).
           05  SELHI PIC  X(0003).
      *    -------------------------------
           05  SEL01L PIC S9(0004) COMP.
           05  SEL01F PIC  X(0001).
           05  FILLER REDEFINES SEL01F.
               10  SEL01A PIC  X(0001).
           05  SEL01I PIC  X(0001).
      *    -------------------------------
           05  EOBCD01L PIC S9(0004) COMP.
           05  EOBCD01F PIC  X(0001).
           05  FILLER REDEFINES EOBCD01F.
               10  EOBCD01A PIC  X(0001).
           05  EOBCD01I PIC  X(0004).
      *    -------------------------------
           05  DESC01L PIC S9(0004) COMP.
           05  DESC01F PIC  X(0001).
           05  FILLER REDEFINES DESC01F.
               10  DESC01A PIC  X(0001).
           05  DESC01I PIC  X(0060).
      *    -------------------------------
           05  SEL02L PIC S9(0004) COMP.
           05  SEL02F PIC  X(0001).
           05  FILLER REDEFINES SEL02F.
               10  SEL02A PIC  X(0001).
           05  SEL02I PIC  X(0001).
      *    -------------------------------
           05  EOBCD02L PIC S9(0004) COMP.
           05  EOBCD02F PIC  X(0001).
           05  FILLER REDEFINES EOBCD02F.
               10  EOBCD02A PIC  X(0001).
           05  EOBCD02I PIC  X(0004).
      *    -------------------------------
           05  DESC02L PIC S9(0004) COMP.
           05  DESC02F PIC  X(0001).
           05  FILLER REDEFINES DESC02F.
               10  DESC02A PIC  X(0001).
           05  DESC02I PIC  X(0060).
      *    -------------------------------
           05  SEL03L PIC S9(0004) COMP.
           05  SEL03F PIC  X(0001).
           05  FILLER REDEFINES SEL03F.
               10  SEL03A PIC  X(0001).
           05  SEL03I PIC  X(0001).
      *    -------------------------------
           05  EOBCD03L PIC S9(0004) COMP.
           05  EOBCD03F PIC  X(0001).
           05  FILLER REDEFINES EOBCD03F.
               10  EOBCD03A PIC  X(0001).
           05  EOBCD03I PIC  X(0004).
      *    -------------------------------
           05  DESC03L PIC S9(0004) COMP.
           05  DESC03F PIC  X(0001).
           05  FILLER REDEFINES DESC03F.
               10  DESC03A PIC  X(0001).
           05  DESC03I PIC  X(0060).
      *    -------------------------------
           05  SEL04L PIC S9(0004) COMP.
           05  SEL04F PIC  X(0001).
           05  FILLER REDEFINES SEL04F.
               10  SEL04A PIC  X(0001).
           05  SEL04I PIC  X(0001).
      *    -------------------------------
           05  EOBCD04L PIC S9(0004) COMP.
           05  EOBCD04F PIC  X(0001).
           05  FILLER REDEFINES EOBCD04F.
               10  EOBCD04A PIC  X(0001).
           05  EOBCD04I PIC  X(0004).
      *    -------------------------------
           05  DESC04L PIC S9(0004) COMP.
           05  DESC04F PIC  X(0001).
           05  FILLER REDEFINES DESC04F.
               10  DESC04A PIC  X(0001).
           05  DESC04I PIC  X(0060).
      *    -------------------------------
           05  SEL05L PIC S9(0004) COMP.
           05  SEL05F PIC  X(0001).
           05  FILLER REDEFINES SEL05F.
               10  SEL05A PIC  X(0001).
           05  SEL05I PIC  X(0001).
      *    -------------------------------
           05  EOBCD05L PIC S9(0004) COMP.
           05  EOBCD05F PIC  X(0001).
           05  FILLER REDEFINES EOBCD05F.
               10  EOBCD05A PIC  X(0001).
           05  EOBCD05I PIC  X(0004).
      *    -------------------------------
           05  DESC05L PIC S9(0004) COMP.
           05  DESC05F PIC  X(0001).
           05  FILLER REDEFINES DESC05F.
               10  DESC05A PIC  X(0001).
           05  DESC05I PIC  X(0060).
      *    -------------------------------
           05  SEL06L PIC S9(0004) COMP.
           05  SEL06F PIC  X(0001).
           05  FILLER REDEFINES SEL06F.
               10  SEL06A PIC  X(0001).
           05  SEL06I PIC  X(0001).
      *    -------------------------------
           05  EOBCD06L PIC S9(0004) COMP.
           05  EOBCD06F PIC  X(0001).
           05  FILLER REDEFINES EOBCD06F.
               10  EOBCD06A PIC  X(0001).
           05  EOBCD06I PIC  X(0004).
      *    -------------------------------
           05  DESC06L PIC S9(0004) COMP.
           05  DESC06F PIC  X(0001).
           05  FILLER REDEFINES DESC06F.
               10  DESC06A PIC  X(0001).
           05  DESC06I PIC  X(0060).
      *    -------------------------------
           05  SEL07L PIC S9(0004) COMP.
           05  SEL07F PIC  X(0001).
           05  FILLER REDEFINES SEL07F.
               10  SEL07A PIC  X(0001).
           05  SEL07I PIC  X(0001).
      *    -------------------------------
           05  EOBCD07L PIC S9(0004) COMP.
           05  EOBCD07F PIC  X(0001).
           05  FILLER REDEFINES EOBCD07F.
               10  EOBCD07A PIC  X(0001).
           05  EOBCD07I PIC  X(0004).
      *    -------------------------------
           05  DESC07L PIC S9(0004) COMP.
           05  DESC07F PIC  X(0001).
           05  FILLER REDEFINES DESC07F.
               10  DESC07A PIC  X(0001).
           05  DESC07I PIC  X(0060).
      *    -------------------------------
           05  SEL08L PIC S9(0004) COMP.
           05  SEL08F PIC  X(0001).
           05  FILLER REDEFINES SEL08F.
               10  SEL08A PIC  X(0001).
           05  SEL08I PIC  X(0001).
      *    -------------------------------
           05  EOBCD08L PIC S9(0004) COMP.
           05  EOBCD08F PIC  X(0001).
           05  FILLER REDEFINES EOBCD08F.
               10  EOBCD08A PIC  X(0001).
           05  EOBCD08I PIC  X(0004).
      *    -------------------------------
           05  DESC08L PIC S9(0004) COMP.
           05  DESC08F PIC  X(0001).
           05  FILLER REDEFINES DESC08F.
               10  DESC08A PIC  X(0001).
           05  DESC08I PIC  X(0060).
      *    -------------------------------
           05  SEL09L PIC S9(0004) COMP.
           05  SEL09F PIC  X(0001).
           05  FILLER REDEFINES SEL09F.
               10  SEL09A PIC  X(0001).
           05  SEL09I PIC  X(0001).
      *    -------------------------------
           05  EOBCD09L PIC S9(0004) COMP.
           05  EOBCD09F PIC  X(0001).
           05  FILLER REDEFINES EOBCD09F.
               10  EOBCD09A PIC  X(0001).
           05  EOBCD09I PIC  X(0004).
      *    -------------------------------
           05  DESC09L PIC S9(0004) COMP.
           05  DESC09F PIC  X(0001).
           05  FILLER REDEFINES DESC09F.
               10  DESC09A PIC  X(0001).
           05  DESC09I PIC  X(0060).
      *    -------------------------------
           05  SEL10L PIC S9(0004) COMP.
           05  SEL10F PIC  X(0001).
           05  FILLER REDEFINES SEL10F.
               10  SEL10A PIC  X(0001).
           05  SEL10I PIC  X(0001).
      *    -------------------------------
           05  EOBCD10L PIC S9(0004) COMP.
           05  EOBCD10F PIC  X(0001).
           05  FILLER REDEFINES EOBCD10F.
               10  EOBCD10A PIC  X(0001).
           05  EOBCD10I PIC  X(0004).
      *    -------------------------------
           05  DESC10L PIC S9(0004) COMP.
           05  DESC10F PIC  X(0001).
           05  FILLER REDEFINES DESC10F.
               10  DESC10A PIC  X(0001).
           05  DESC10I PIC  X(0060).
      *    -------------------------------
           05  SEL11L PIC S9(0004) COMP.
           05  SEL11F PIC  X(0001).
           05  FILLER REDEFINES SEL11F.
               10  SEL11A PIC  X(0001).
           05  SEL11I PIC  X(0001).
      *    -------------------------------
           05  EOBCD11L PIC S9(0004) COMP.
           05  EOBCD11F PIC  X(0001).
           05  FILLER REDEFINES EOBCD11F.
               10  EOBCD11A PIC  X(0001).
           05  EOBCD11I PIC  X(0004).
      *    -------------------------------
           05  DESC11L PIC S9(0004) COMP.
           05  DESC11F PIC  X(0001).
           05  FILLER REDEFINES DESC11F.
               10  DESC11A PIC  X(0001).
           05  DESC11I PIC  X(0060).
      *    -------------------------------
           05  SEL12L PIC S9(0004) COMP.
           05  SEL12F PIC  X(0001).
           05  FILLER REDEFINES SEL12F.
               10  SEL12A PIC  X(0001).
           05  SEL12I PIC  X(0001).
      *    -------------------------------
           05  EOBCD12L PIC S9(0004) COMP.
           05  EOBCD12F PIC  X(0001).
           05  FILLER REDEFINES EOBCD12F.
               10  EOBCD12A PIC  X(0001).
           05  EOBCD12I PIC  X(0004).
      *    -------------------------------
           05  DESC12L PIC S9(0004) COMP.
           05  DESC12F PIC  X(0001).
           05  FILLER REDEFINES DESC12F.
               10  DESC12A PIC  X(0001).
           05  DESC12I PIC  X(0060).
      *    -------------------------------
           05  SEL13L PIC S9(0004) COMP.
           05  SEL13F PIC  X(0001).
           05  FILLER REDEFINES SEL13F.
               10  SEL13A PIC  X(0001).
           05  SEL13I PIC  X(0001).
      *    -------------------------------
           05  EOBCD13L PIC S9(0004) COMP.
           05  EOBCD13F PIC  X(0001).
           05  FILLER REDEFINES EOBCD13F.
               10  EOBCD13A PIC  X(0001).
           05  EOBCD13I PIC  X(0004).
      *    -------------------------------
           05  DESC13L PIC S9(0004) COMP.
           05  DESC13F PIC  X(0001).
           05  FILLER REDEFINES DESC13F.
               10  DESC13A PIC  X(0001).
           05  DESC13I PIC  X(0060).
      *    -------------------------------
           05  SEL14L PIC S9(0004) COMP.
           05  SEL14F PIC  X(0001).
           05  FILLER REDEFINES SEL14F.
               10  SEL14A PIC  X(0001).
           05  SEL14I PIC  X(0001).
      *    -------------------------------
           05  EOBCD14L PIC S9(0004) COMP.
           05  EOBCD14F PIC  X(0001).
           05  FILLER REDEFINES EOBCD14F.
               10  EOBCD14A PIC  X(0001).
           05  EOBCD14I PIC  X(0004).
      *    -------------------------------
           05  DESC14L PIC S9(0004) COMP.
           05  DESC14F PIC  X(0001).
           05  FILLER REDEFINES DESC14F.
               10  DESC14A PIC  X(0001).
           05  DESC14I PIC  X(0060).
      *    -------------------------------
           05  SEL15L PIC S9(0004) COMP.
           05  SEL15F PIC  X(0001).
           05  FILLER REDEFINES SEL15F.
               10  SEL15A PIC  X(0001).
           05  SEL15I PIC  X(0001).
      *    -------------------------------
           05  EOBCD15L PIC S9(0004) COMP.
           05  EOBCD15F PIC  X(0001).
           05  FILLER REDEFINES EOBCD15F.
               10  EOBCD15A PIC  X(0001).
           05  EOBCD15I PIC  X(0004).
      *    -------------------------------
           05  DESC15L PIC S9(0004) COMP.
           05  DESC15F PIC  X(0001).
           05  FILLER REDEFINES DESC15F.
               10  DESC15A PIC  X(0001).
           05  DESC15I PIC  X(0060).
      *    -------------------------------
           05  SEL16L PIC S9(0004) COMP.
           05  SEL16F PIC  X(0001).
           05  FILLER REDEFINES SEL16F.
               10  SEL16A PIC  X(0001).
           05  SEL16I PIC  X(0001).
      *    -------------------------------
           05  EOBCD16L PIC S9(0004) COMP.
           05  EOBCD16F PIC  X(0001).
           05  FILLER REDEFINES EOBCD16F.
               10  EOBCD16A PIC  X(0001).
           05  EOBCD16I PIC  X(0004).
      *    -------------------------------
           05  DESC16L PIC S9(0004) COMP.
           05  DESC16F PIC  X(0001).
           05  FILLER REDEFINES DESC16F.
               10  DESC16A PIC  X(0001).
           05  DESC16I PIC  X(0060).
      *    -------------------------------
           05  SEL17L PIC S9(0004) COMP.
           05  SEL17F PIC  X(0001).
           05  FILLER REDEFINES SEL17F.
               10  SEL17A PIC  X(0001).
           05  SEL17I PIC  X(0001).
      *    -------------------------------
           05  EOBCD17L PIC S9(0004) COMP.
           05  EOBCD17F PIC  X(0001).
           05  FILLER REDEFINES EOBCD17F.
               10  EOBCD17A PIC  X(0001).
           05  EOBCD17I PIC  X(0004).
      *    -------------------------------
           05  DESC17L PIC S9(0004) COMP.
           05  DESC17F PIC  X(0001).
           05  FILLER REDEFINES DESC17F.
               10  DESC17A PIC  X(0001).
           05  DESC17I PIC  X(0060).
      *    -------------------------------
           05  AEMSGL PIC S9(0004) COMP.
           05  AEMSGF PIC  X(0001).
           05  FILLER REDEFINES AEMSGF.
               10  AEMSGA PIC  X(0001).
           05  AEMSGI PIC  X(0079).
      *    -------------------------------
           05  APFKL PIC S9(0004) COMP.
           05  APFKF PIC  X(0001).
           05  FILLER REDEFINES APFKF.
               10  APFKA PIC  X(0001).
           05  APFKI PIC  S99.
      *    -------------------------------
           05  F3KEYL PIC S9(0004) COMP.
           05  F3KEYF PIC  X(0001).
           05  FILLER REDEFINES F3KEYF.
               10  F3KEYA PIC  X(0001).
           05  F3KEYI PIC  X(0019).
      *    -------------------------------
           05  F5KEYL PIC S9(0004) COMP.
           05  F5KEYF PIC  X(0001).
           05  FILLER REDEFINES F5KEYF.
               10  F5KEYA PIC  X(0001).
           05  F5KEYI PIC  X(0025).
       01  EL614AO REDEFINES EL614AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SELHO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEL01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOBCD01O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC01O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEL02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOBCD02O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC02O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEL03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOBCD03O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC03O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEL04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOBCD04O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC04O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEL05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOBCD05O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC05O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEL06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOBCD06O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC06O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEL07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOBCD07O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC07O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEL08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOBCD08O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC08O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEL09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOBCD09O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC09O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEL10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOBCD10O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC10O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEL11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOBCD11O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC11O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEL12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOBCD12O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC12O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEL13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOBCD13O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC13O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEL14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOBCD14O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC14O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEL15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOBCD15O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC15O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEL16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOBCD16O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC16O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEL17O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOBCD17O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC17O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSGO PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFKO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  F3KEYO PIC  X(0019).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  F5KEYO PIC  X(0025).
      *    -------------------------------
       01  EL614AO-R REDEFINES EL614AI.
           05  FILLER                      PIC X(37).
           05  WS-MAP-LINE                 OCCURS 17
121012                                     INDEXED BY M1 M2 N1.
               10  SELL                    PIC S9(4)  COMP.
               10  SELA                    PIC X.
               10  SELI                    PIC X.
               10  RCODEL                  PIC S9(4)  COMP.
               10  RCODEA                  PIC X.
               10  RCODEO                  PIC XXXX.
               10  RCODEI                  REDEFINES
                   RCODEO                  PIC XXXX.
               10  DESCL                   PIC S9(4)   COMP.
               10  DESCA                   PIC X.
               10  DESCO                   PIC X(60).
               10  DESCI                   REDEFINES
                   DESCO                   PIC X(60).
121012     05  FILLER                      PIC X(137).
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
      *                                COPY ELCEOBC.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ELCEOBC                             *
      *                            VMOD=2.001                          *
      *                                                                *
      *   CLAIM SYSTEM EOB CODE TABLE                                  *
      *                                                                *
      *   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
      *   VSAM EOB CODE TABLE                                          *
      *                                                                *
      *   FILE DESCRIPTION = EOB CODE TABLE                            *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 350   RECFORM = FIX                            *
      *                                                                *
      *   BASE CLUSTER NAME = ELEOBC                    RKP=2,LEN=15   *
      *                                                                *
      *   LOG = NO                                                     *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      *                                                                *
      *                                                                *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 120808    2008100900001  PEMA  NEW COPYBOOK/FILE
      * 081511    2011022800001  PEMA  CHG FOR ADMIN SERV NAPERSOFT
091913* 091913    2013090300001  AJRA  ADDITIONAL RECORD TYPES
      ******************************************************************
       01  EOB-CODES.
           12  EO-RECORD-ID                      PIC XX.
               88  VALID-DN-ID                      VALUE 'EO'.
           12  EO-CONTROL-PRIMARY.
               16  EO-COMPANY-CD                 PIC X.
               16  EO-RECORD-TYPE                PIC X.
                   88  EO-EOB-RECS                  VALUE '1'.
                   88  EO-VERIF-RECS                VALUE '2'.
                   88  EO-GCE-RECS                  VALUE '3'.
091913             88  EO-CANC-RECS                 VALUE '4'.
091913             88  EO-BILL-NOTE-RECS            VALUE '5'.
               16  EO-EOB-CODE                   PIC X(4).
               16  FILLER                        PIC X(9).
           12  EO-MAINT-INFORMATION.
               16  EO-LAST-MAINT-DT              PIC XX.
               16  EO-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
               16  EO-LAST-MAINT-USER            PIC X(4).
               16  FILLER                        PIC XX.
           12  EO-DESCRIPTION                    PIC X(275).
           12  FILLER                            PIC X(46).
      ******************************************************************
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
112113 01  TS-WORK-AREA                PIC X(3348).
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
           MOVE 'EL614' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
           MOVE EIBDATE               TO DC-JULIAN-YYDDD.
           MOVE '5'                   TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION
                                      THRU 8500-EXIT
           MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
           MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
           MOVE DFHCOMMAREA           TO  PROGRAM-INTERFACE-BLOCK.
           MOVE +1                    TO  EMI-NUMBER-OF-LINES
                                          EMI-SWITCH2.
           IF EIBCALEN NOT > ZERO
              MOVE UNACCESS-MSG        TO LOGOFF-MSG
              GO TO 8300-SEND-TEXT
           END-IF
           
      * EXEC CICS HANDLE CONDITION
      *        PGMIDERR (9600-PGMIDERR)
      *        ERROR    (9990-ERROR)
      *    END-EXEC
      *    MOVE '"$L.                  ! " #00001659' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031363539' TO DFHEIV0(25:11)
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
           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
              MOVE LOW-VALUES          TO EL614AO
              MOVE -1                  TO APFKL
              MOVE ER-0008             TO EMI-ERROR
              GO TO 8200-SEND-DATAONLY
           END-IF
           
      * EXEC CICS RECEIVE
      *        INTO   (EL614AO)
      *        MAPSET (WS-MAPSET-NAME)
      *        MAP    (WS-MAP-NAME)
      *    END-EXEC
           MOVE LENGTH OF
            EL614AO
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001696' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL614AO, 
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
           IF EIBAID = DFHENTER OR DFHPF1 OR DFHPF2
121012        OR DFHPF3 OR DFHPF4 OR DFHPF5
              CONTINUE
           ELSE
              MOVE ER-0008             TO EMI-ERROR
              MOVE -1                  TO APFKL
              GO TO 8200-SEND-DATAONLY
           END-IF
           IF EIBAID = DFHPF1 OR DFHPF2 OR DFHPF3
              OR DFHPF4 OR DFHENTER OR DFHPF5
              MOVE 'S'                 TO PI-MODE
           ELSE
              MOVE AL-UABOF            TO APFKA
              MOVE -1                  TO APFKL
              MOVE ER-0023             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
           END-IF
           IF EMI-FATAL-CTR > ZERO
              GO TO 8200-SEND-DATAONLY
           END-IF
           IF PI-MODE EQUAL 'S'
              GO TO 2000-PROCESS-SHOW
           END-IF
           MOVE 'LOGIC ERROR HAS OCCURRED - PROGRAM EL614'
                                       TO LOGOFF-MSG
           GO TO 8300-SEND-TEXT
           .
       1000-INITIAL-SCREEN.
           MOVE PI-PROGRAM-WORK-AREA (1:60)
                                       TO WS-PASSED-FROM-CALL-PGM
121012     MOVE PI-REASON-CODE-IND     TO WS-REASON-CODE-PASSED
           EVALUATE TRUE
              WHEN PI-RETURN-TO-PROGRAM = 'EL156'
                 MOVE WS-PASSED-FROM-CALL-PGM
                                       TO PI-PROGRAM-WORK-AREA
                 MOVE '1'              TO PI-RECORD-TYPE
              WHEN PI-RETURN-TO-PROGRAM = 'EL6316'
                 MOVE WS-PASSED-FROM-CALL-PGM
                                       TO PI-PROGRAM-WORK-AREA
                 MOVE '4'              TO PI-RECORD-TYPE
              WHEN PI-RETURN-TO-PROGRAM = 'EL6317'
                 MOVE WS-PASSED-FROM-CALL-PGM
                                       TO PI-PROGRAM-WORK-AREA
                 MOVE '2'              TO PI-RECORD-TYPE
              WHEN PI-RETURN-TO-PROGRAM = 'EL6318'
                 MOVE WS-PASSED-FROM-CALL-PGM
                                       TO PI-PROGRAM-WORK-AREA
121012           MOVE WS-REASON-CODE-PASSED TO PI-REASON-CODE-IND
121012           MOVE 'N'              TO PI-EXPANDED-VIEW
                 MOVE '3'              TO PI-RECORD-TYPE
              WHEN OTHER
                 MOVE SPACES           TO PI-PROGRAM-WORK-AREA
                                          WS-PASSED-FROM-CALL-PGM
                                          PI-RECORD-TYPE
           END-EVALUATE
112113     MOVE PI-COMPANY-CD          TO WS-ELEOBC-KEY
           MOVE PI-RECORD-TYPE         TO WS-EOB-REC-TYPE
           MOVE ZERO                   TO PI-1ST-TIME-SW
                                          PI-LINE-COUNT
                                          PI-BROWSE-SW
                                          PI-SHOW-SW
                                          PI-CHANGE-SW
           MOVE LOW-VALUES             TO EL614AO
112113     GO TO 7000-BUILD-TABLE
           .
       2000-PROCESS-SHOW.
      **  THE FIRST THING TO DO IS CHECK
      **  FOR PF4 (CLEAR SELECTED CODES) IF PRESSED
      **  SPACE OUT THE PRIOR SELECTIONS. NEXT CHECK
      **  THE SELECTED CODES
      **  AGAINST THE TABLE AND SYNC THEM UP.
      **  IF THEY SPACE ONE OUT THEN GET RID OF IT FROM THE
      **  TABLE
           IF EIBAID = DFHPF4
              MOVE SPACES              TO PI-SELECTED-EOB-CODES
              move pi-dn-TOP-key    to WS-ELEOBC-KEY
              GO TO 7000-browse-fwd
           END-IF
121012
121012     IF EIBAID = DFHPF5
121012         IF PI-EXPANDED-VIEW = 'Y'
121012             MOVE 'N'        TO PI-EXPANDED-VIEW
121012         ELSE
121012             MOVE 'Y'        TO PI-EXPANDED-VIEW
121012         END-IF
               move pi-dn-TOP-key    to WS-ELEOBC-KEY
               GO TO 7000-browse-fwd
121012     END-IF
           IF PI-RETURN-TO-PROGRAM = 'EL156' OR 'EL6317' OR 'EL6318'
              OR 'EL6316'
              PERFORM VARYING M1 FROM +1 BY +1 UNTIL
                 M1 > +17
                 IF SELL (M1) > +0
                    IF SELI (M1) = SPACES
                       PERFORM VARYING E1 FROM +1 BY +1 UNTIL
                          E1 > +12
                          IF PI-EOB-CODE (E1) = RCODEI (M1)
                             MOVE SPACES TO PI-EOB-CODE (E1)
                          END-IF
                       END-PERFORM
                    ELSE
                       MOVE ' '       TO WS-DUP-EOB-SW
                       PERFORM VARYING E1 FROM +1 BY +1 UNTIL
                          E1 > +12
011811                    IF RCODEI (M1) = PI-EOB-CODE (E1)
                             SET FOUND-DUP-EOB TO TRUE
                          END-IF
                       END-PERFORM
                       IF NOT FOUND-DUP-EOB
                          PERFORM VARYING E1 FROM +1 BY +1 UNTIL
                             (PI-EOB-CODE (E1) = SPACES)
                             OR (E1 > +12)
                          END-PERFORM
                          IF E1 < +13
                             MOVE RCODEI (M1) TO PI-EOB-CODE (E1)
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-PERFORM
           END-IF
           EVALUATE TRUE
              WHEN EIBAID = DFHPF2
112113           MOVE PI-TOP-LINE-NUM TO PI-BOT-LINE-NUM
112113                                   PI-CURRENT-LINE
                 move pi-dn-top-key    to WS-ELEOBC-KEY
                 GO TO 7150-browse-bwd
              WHEN EIBAID = DFHPF1
112113           MOVE PI-BOT-LINE-NUM TO PI-TOP-LINE-NUM
112113                                   PI-CURRENT-LINE
                 move pi-dn-bot-key    to WS-ELEOBC-KEY
                 GO TO 7000-browse-fwd
              WHEN EIBAID = DFHPF3
                 PERFORM 6100-FORMAT-EOB-CODES
                                       THRU 6100-EXIT
                 GO TO 9400-CLEAR
              WHEN OTHER
      *          MOVE PI-COMPANY-CD    TO WS-ELEOBC-KEY
      *          GO TO 7000-BROWSE-FWD
                 MOVE -1 TO SELL (1)
                 GO TO 8200-SEND-DATAONLY
           END-EVALUATE
           .
       6100-FORMAT-EOB-CODES.
      **  THIS ROUTINE JUST ELIMINATES ANY EOB CODES THAT
      **  WERE "UN SELECTED"
           MOVE SPACES                 TO WS-SELECTED-EOB-CODES
           MOVE +1                     TO E1 S1
           PERFORM UNTIL E1 > +12
              IF PI-EOB-CODE (E1) NOT = SPACES
                 MOVE PI-EOB-CODE (E1) TO WS-SEL-EOB-CODE (S1)
                 ADD +1                TO S1
              END-IF
              ADD +1                   TO E1
           END-PERFORM
           MOVE WS-SELECTED-EOB-CODES  TO PI-SELECTED-EOB-CODES
           .
       6100-EXIT.
           EXIT.
112113
112113 7000-BUILD-TABLE.
112113     MOVE ZEROS                  TO  PI-TOTAL-LINES
112113                                     PI-CURRENT-LINE
112113                                     PI-TEMP-STOR-ITEMS
                                           i1
112113     MOVE ZERO                   TO PI-LINE-COUNT
112113     MOVE +1                     TO PI-BROWSE-SW
           move ' '                    to pi-6318-pass
112113****IF TEMP STORAGE EXISTS, DELETE IT.
112113     PERFORM 7500-READ-TS THRU 7599-EXIT
112113
112113     IF PI-TEMP-STOR-ITEMS NOT = ZERO
112113        PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT
112113     END-IF.
       7000-browse-fwd.
           set m1 to +1
           move +1 to pi-browse-sw
           move low-values to el614ao
112113
112113     
      * EXEC CICS STARTBR
112113*        DATASET   ('ELEOBC')
112113*        RIDFLD    (WS-ELEOBC-KEY)
112113*        GTEQ
112113*        RESP      (WS-RESPONSE)
112113*    END-EXEC.
           MOVE 'ELEOBC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00001915' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303031393135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-ELEOBC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
112113
112113     EVALUATE TRUE
112113        WHEN RESP-ENDFILE
112113           GO TO 7030-ENDBR
112113        WHEN RESP-NORMAL
112113           CONTINUE
112113        WHEN RESP-NOTFND
112113           GO TO 7030-ENDBR
112113     END-EVALUATE.
112113
112113***IF COMING FROM EL6318 WE WANT TO LOAD THE HIGHLIGHTED REASON CO
112113***FOLLOWED BY THE NON HIGHLIGHTED. FOR ALL OTHER SCREENS WE JUST
112113***REASON CODES IN ORDER.
112113     IF PI-RETURN-TO-PROGRAM NOT EQUAL 'EL6318'
112113         GO TO 7020-LOOP
112113     END-IF.
           perform varying s2 from +1 by +1 until
              (s2 > +26)
              or (WS-EOB-CODE(1:1) = pi-reason-code-flag (s2))
           end-perform
           if s2 <= +26
              move '1'                 to pi-6318-pass
           else
              move '2'                 to pi-6318-pass
           end-if
           if pi-6318-pass = '2'
              go to 7010-NON-HIGHLIGHT-LOOP
           end-if
           .
112113 7001-HIGHLIGHT-LOOP.
112113     
      * EXEC CICS READNEXT
112113*        INTO    (EOB-CODES)
112113*        DATASET ('ELEOBC')
112113*        RIDFLD  (WS-ELEOBC-KEY)
112113*        RESP    (WS-RESPONSE)
112113*    END-EXEC
           MOVE LENGTH OF
            EOB-CODES
             TO DFHEIV12
           MOVE 'ELEOBC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00001951' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031393531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EOB-CODES, 
                 DFHEIV12, 
                 WS-ELEOBC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
112113
112113     EVALUATE TRUE
112113        WHEN RESP-ENDFILE
112113           GO TO 7005-ENDBR
112113        WHEN (PI-COMPANY-CD NOT = WS-COMPANY-CD)
112113           OR ((PI-RECORD-TYPE NOT = EO-RECORD-TYPE)
112113               AND (PI-RECORD-TYPE NOT = SPACES))
112113           GO TO 7005-ENDBR
112113        WHEN RESP-NORMAL
112113           CONTINUE
112113        WHEN RESP-NOTFND
112113           GO TO 7005-ENDBR
112113        WHEN EO-COMPANY-CD NOT = PI-COMPANY-CD
112113           GO TO 7005-ENDBR
112113     END-EVALUATE
112113
112113     PERFORM VARYING S2 FROM +1 BY +1 UNTIL
112113        (S2 > +26)
              or (eo-eob-code (1:1) = pi-reason-code-flag (s2))
           end-perform
           if s2 > +26
              go to 7001-highlight-loop
           end-if
           MOVE eo-eob-code            TO WS-TABLE-CODE
           MOVE WS-TAB-CODE-1          TO WS-SCR-CODE-4
           MOVE WS-TAB-CODE-2-4        TO WS-SCR-CODE-1-3
           MOVE WS-SCREEN-CODE         TO RCODEO (M1)
           MOVE eo-DESCRIPTION         TO DESCO (M1)
           MOVE AL-SABON               TO RCODEA (M1)
           MOVE AL-SABOF               TO DESCA (M1)
           PERFORM VARYING E1 FROM +1 BY +1 UNTIL
              E1 > +12
              MOVE PI-EOB-CODE (E1)    TO WS-SCREEN-CODE
              MOVE WS-SCR-CODE-1-3     TO WS-TAB-CODE-2-4
              MOVE WS-SCR-CODE-4       TO WS-TAB-CODE-1
              IF eo-EOB-CODE  = WS-TABLE-CODE
                 MOVE 'S'              TO SELI (M1)
                 MOVE +1               TO SELL (M1)
              END-IF
           END-PERFORM
           if m1 = +1
              move ws-eleobc-key to pi-dn-top-key
           else
              move ws-eleobc-key to pi-dn-bot-key
           end-if
           IF PI-EXPANDED-VIEW = 'Y'
               IF eo-DESCRIPTION (61:60) > SPACES
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (61:60) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
               IF eo-DESCRIPTION (121:60) > SPACES
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (121:60) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
               IF eo-DESCRIPTION (181:60) > SPACES
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (181:60) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
               IF Eo-DESCRIPTION (241:35) > SPACES
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (241:35) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
           END-IF
           if m1 < num-lines-per-screen
              set m1 up by +1
              go to 7001-HIGHLIGHT-LOOP
           end-if
           .
112113 7005-ENDBR.
           if m1 = num-lines-per-screen   *> filled up the screen
              go to 7030-endbr
           end-if
      *** did not fill up screen, now load non highlighted stuff
112113     
      * EXEC CICS ENDBR
112113*        DATASET ('ELEOBC')
112113*    END-EXEC
           MOVE 'ELEOBC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002046' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           move '2'                    to pi-6318-pass
112113
112113     MOVE LOW-VALUES             TO WS-ELEOBC-KEY
112113     MOVE PI-COMPANY-CD          TO WS-ELEOBC-KEY
112113     MOVE PI-RECORD-TYPE         TO WS-EOB-REC-TYPE
112113     
      * EXEC CICS STARTBR
112113*        DATASET   ('ELEOBC')
112113*        RIDFLD    (WS-ELEOBC-KEY)
112113*        GTEQ
112113*        RESP      (WS-RESPONSE)
112113*    END-EXEC.
           MOVE 'ELEOBC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00002054' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303032303534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-ELEOBC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
112113
112113     EVALUATE TRUE
112113        WHEN RESP-ENDFILE
112113           GO TO 7030-ENDBR
112113        WHEN RESP-NORMAL
112113           CONTINUE
112113        WHEN RESP-NOTFND
112113           GO TO 7030-ENDBR
112113     END-EVALUATE.
112113
112113 7010-NON-HIGHLIGHT-LOOP.
112113     
      * EXEC CICS READNEXT
112113*        INTO    (EOB-CODES)
112113*        DATASET ('ELEOBC')
112113*        RIDFLD  (WS-ELEOBC-KEY)
112113*        RESP    (WS-RESPONSE)
112113*    END-EXEC
           MOVE LENGTH OF
            EOB-CODES
             TO DFHEIV12
           MOVE 'ELEOBC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00002071' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303032303731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EOB-CODES, 
                 DFHEIV12, 
                 WS-ELEOBC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
112113
112113     EVALUATE TRUE
112113        WHEN RESP-ENDFILE
112113           GO TO 7030-ENDBR
112113        WHEN (PI-COMPANY-CD NOT = WS-COMPANY-CD)
112113           OR ((PI-RECORD-TYPE NOT = EO-RECORD-TYPE)
112113               AND (PI-RECORD-TYPE NOT = SPACES))
112113           GO TO 7030-ENDBR
112113        WHEN RESP-NORMAL
112113           CONTINUE
112113        WHEN RESP-NOTFND
112113           GO TO 7030-ENDBR
112113        WHEN EO-COMPANY-CD NOT = PI-COMPANY-CD
112113           GO TO 7030-ENDBR
112113     END-EVALUATE
112113
112113     PERFORM VARYING S2 FROM +1 BY +1 UNTIL
112113        (S2 > +26)
              or EO-EOB-CODE (1:1) = PI-REASON-CODE-FLAG (S2)
112113     END-PERFORM
           if s2 <= +26  *> found one previously loaded on screen
              go to 7010-non-highlight-loop
           end-if
           MOVE eo-eob-code            TO WS-TABLE-CODE
           MOVE WS-TAB-CODE-1          TO WS-SCR-CODE-4
           MOVE WS-TAB-CODE-2-4        TO WS-SCR-CODE-1-3
           MOVE WS-SCREEN-CODE         TO RCODEO (M1)
           MOVE eo-DESCRIPTION         TO DESCO (M1)
           PERFORM VARYING E1 FROM +1 BY +1 UNTIL
              E1 > +12
              MOVE PI-EOB-CODE (E1)    TO WS-SCREEN-CODE
              MOVE WS-SCR-CODE-1-3     TO WS-TAB-CODE-2-4
              MOVE WS-SCR-CODE-4       TO WS-TAB-CODE-1
              IF eo-EOB-CODE  = WS-TABLE-CODE
                 MOVE 'S'              TO SELI (M1)
                 MOVE +1               TO SELL (M1)
              END-IF
           END-PERFORM
           if m1 = +1
              move ws-eleobc-key to pi-dn-top-key
           else
              move ws-eleobc-key to pi-dn-bot-key
           end-if
           IF PI-EXPANDED-VIEW = 'Y'
               IF eo-DESCRIPTION (61:60) > SPACES
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (61:60) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
               IF eo-DESCRIPTION (121:60) > SPACES
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (121:60) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
               IF eo-DESCRIPTION (181:60) > SPACES
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (181:60) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
               IF Eo-DESCRIPTION (241:35) > SPACES
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (241:35) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
           END-IF
           if m1 < num-lines-per-screen
              set m1 up by +1
              go to 7010-NON-HIGHLIGHT-LOOP
           end-if
      ***  filled up the screen
           go to 7030-endbr
           .
112113 7020-LOOP.
112113     
      * EXEC CICS READNEXT
112113*        INTO    (EOB-CODES)
112113*        DATASET ('ELEOBC')
112113*        RIDFLD  (WS-ELEOBC-KEY)
112113*        RESP    (WS-RESPONSE)
112113*    END-EXEC
           MOVE LENGTH OF
            EOB-CODES
             TO DFHEIV12
           MOVE 'ELEOBC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00002162' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303032313632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EOB-CODES, 
                 DFHEIV12, 
                 WS-ELEOBC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
112113
112113     EVALUATE TRUE
112113        WHEN RESP-ENDFILE
112113           GO TO 7030-ENDBR
112113        WHEN (PI-COMPANY-CD NOT = WS-COMPANY-CD)
112113           OR ((PI-RECORD-TYPE NOT = EO-RECORD-TYPE)
112113               AND (PI-RECORD-TYPE NOT = SPACES))
112113           GO TO 7030-ENDBR
112113        WHEN RESP-NORMAL
112113           CONTINUE
112113        WHEN RESP-NOTFND
112113           GO TO 7030-ENDBR
112113        WHEN EO-COMPANY-CD NOT = PI-COMPANY-CD
112113           GO TO 7030-ENDBR
112113     END-EVALUATE
           IF PI-RETURN-TO-PROGRAM = 'EL6318'
               MOVE eo-eob-code        TO WS-TABLE-CODE
               MOVE WS-TAB-CODE-1      TO WS-SCR-CODE-4
               MOVE WS-TAB-CODE-2-4    TO WS-SCR-CODE-1-3
               MOVE WS-SCREEN-CODE     TO RCODEO (M1)
           ELSE
               MOVE eo-eob-code        TO RCODEO (M1)
           END-IF
           MOVE eo-DESCRIPTION         TO DESCO (M1)
      ** Highlighting done here I think
           IF PI-RETURN-TO-PROGRAM = 'EL6318'
              PERFORM VARYING S2 FROM +1 BY +1 UNTIL
                S2 > +26
                  IF RCODEO (M1)(4:1) = PI-REASON-CODE-FLAG (S2)
                     MOVE AL-SABON TO RCODEA (M1)
                     MOVE AL-SABOF TO DESCA (M1)
                  END-IF
              END-PERFORM
           END-IF
           IF PI-RETURN-TO-PROGRAM = 'EL156' OR 'EL6317'
              OR 'EL6316'
              PERFORM VARYING E1 FROM +1 BY +1 UNTIL
                 E1 > +12
                 IF EO-EOB-CODE = PI-EOB-CODE (E1)
                    MOVE 'S'           TO SELI (M1)
                    MOVE +1            TO SELL (M1)
                 END-IF
              END-PERFORM
           END-IF
           IF PI-RETURN-TO-PROGRAM = 'EL6318'
              PERFORM VARYING E1 FROM +1 BY +1 UNTIL
                 E1 > +12
                 MOVE PI-EOB-CODE (E1) TO WS-SCREEN-CODE
                 MOVE WS-SCR-CODE-1-3  TO WS-TAB-CODE-2-4
                 MOVE WS-SCR-CODE-4    TO WS-TAB-CODE-1
                 IF eo-EOB-CODE  = WS-TABLE-CODE
                    MOVE 'S'           TO SELI (M1)
                    MOVE +1            TO SELL (M1)
                 END-IF
              END-PERFORM
           END-IF
           if m1 = +1
              move ws-eleobc-key to pi-dn-top-key
           else
              move ws-eleobc-key to pi-dn-bot-key
           end-if
           IF PI-EXPANDED-VIEW = 'Y'
               IF eo-DESCRIPTION (61:60) > SPACES
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (61:60) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
               IF eo-DESCRIPTION (121:60) > SPACES
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (121:60) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
               IF eo-DESCRIPTION (181:60) > SPACES
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (181:60) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
               IF Eo-DESCRIPTION (241:35) > SPACES
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (241:35) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
           END-IF
           if m1 < num-lines-per-screen
              set m1 up by +1
              go to 7020-loop
           end-if
           .
112113 7030-ENDBR.
           IF PI-BROWSE-SW = +1
              
      * EXEC CICS ENDBR
      *          DATASET ('ELEOBC')
      *       END-EXEC
           MOVE 'ELEOBC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002270' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              MOVE +0                  TO PI-BROWSE-SW
           END-IF
           IF m1 = 0
               MOVE ER-0006            TO EMI-ERROR
               MOVE -1                 TO APFKL
               MOVE AL-UABOF           TO APFKA
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               MOVE ZEROS              TO PI-TOTAL-LINES
               GO TO 8100-SEND-INITIAL-MAP
           END-IF.
           IF m1 < num-lines-per-screen
              MOVE ER-1699             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           END-IF.
           go to 8100-send-initial-map
           .
       7150-browse-bwd.
           set m1 to num-lines-per-screen
           move +1 to pi-browse-sw
           move low-values to el614ao
           
      * EXEC CICS STARTBR
      *        DATASET   ('ELEOBC')
      *        RIDFLD    (WS-ELEOBC-KEY)
      *        GTEQ
      *        RESP      (WS-RESPONSE)
      *    END-EXEC.
           MOVE 'ELEOBC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00002293' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303032323933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-ELEOBC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       7150-check-startbr.
           EVALUATE TRUE
              WHEN RESP-ENDFILE
                 GO TO 7180-ENDBR
              WHEN RESP-NORMAL
                 CONTINUE
              WHEN RESP-NOTFND
                 GO TO 7180-ENDBR
           END-EVALUATE.
      *** 6318-pass is just if we came from 6318
      *** pass one = highligted ones, pass two = all others
           IF PI-RETURN-TO-PROGRAM = 'EL6318'
              PERFORM VARYING S2 FROM +1 BY +1 UNTIL
                (S2 > +26)
                 or (ws-eob-code(1:1) = PI-REASON-CODE-FLAG (S2))
              END-PERFORM
              if s2 <= +26
                 move '1'              to pi-6318-pass
              end-if
           END-IF
           .
       7160-LOOP.
           
      * EXEC CICS READPREV
      *        INTO    (EOB-CODES)
      *        DATASET ('ELEOBC')
      *        RIDFLD  (WS-ELEOBC-KEY)
      *        RESP    (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            EOB-CODES
             TO DFHEIV12
           MOVE 'ELEOBC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0IL                  )  N#00002321' TO DFHEIV0
           MOVE X'2630494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303032333231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EOB-CODES, 
                 DFHEIV12, 
                 WS-ELEOBC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           EVALUATE TRUE
              WHEN RESP-ENDFILE
                 set true-endbr to true
                 GO TO 7180-ENDBR
              WHEN (PI-COMPANY-CD NOT = WS-COMPANY-CD)
                 OR ((PI-RECORD-TYPE NOT = EO-RECORD-TYPE)
                     AND (PI-RECORD-TYPE NOT = SPACES))
                 set true-endbr to true
                 GO TO 7180-ENDBR
              WHEN EO-COMPANY-CD NOT = PI-COMPANY-CD
                 set true-endbr to true
                 GO TO 7180-ENDBR
              WHEN RESP-NORMAL
                 CONTINUE
              WHEN RESP-NOTFND
                 set true-endbr to true
                 GO TO 7180-ENDBR
           END-EVALUATE
           IF PI-RETURN-TO-PROGRAM = 'EL6318'
              PERFORM VARYING S2 FROM +1 BY +1 UNTIL
                (S2 > +26)
                 or (eo-eob-code(1:1) = PI-REASON-CODE-FLAG (S2))
              END-PERFORM
              if s2 <= +26
                 if pi-6318-pass = '1'
                    continue
                 else
                    go to 7160-loop
                 end-if
              else
                 if pi-6318-pass = '2'
                    continue
                 else
                    go to 7160-loop
                 end-if
              end-if
           end-if
           if m1 = num-lines-per-screen
              move ws-eleobc-key to pi-dn-bot-key
           else
              move ws-eleobc-key to pi-dn-top-key
           end-if
           IF PI-EXPANDED-VIEW = 'Y'
              EVALUATE TRUE
                WHEN EO-DESCRIPTION (241:35) > SPACES
                  IF M1 < +5
                      GO TO 7180-ENDBR
                  ELSE
                      SET M1 DOWN BY +4
                  END-IF
                WHEN EO-DESCRIPTION (181:60) > SPACES
                  IF M1 < +4
                      GO TO 7180-ENDBR
                  ELSE
                      SET M1 DOWN BY +3
                  END-IF
                WHEN EO-DESCRIPTION (121:60) > SPACES
                  IF M1 < +3
                      GO TO 7180-ENDBR
                  ELSE
                      SET M1 DOWN BY +2
                  END-IF
                WHEN EO-DESCRIPTION (61:60) > SPACES
                  IF M1 < +2
                      GO TO 7180-ENDBR
                  ELSE
                      SET M1 DOWN BY +1
                  END-IF
              END-EVALUATE
           END-IF
           IF PI-RETURN-TO-PROGRAM = 'EL6318'
               MOVE eo-eob-code        TO WS-TABLE-CODE
               MOVE WS-TAB-CODE-1      TO WS-SCR-CODE-4
               MOVE WS-TAB-CODE-2-4    TO WS-SCR-CODE-1-3
               MOVE WS-SCREEN-CODE     TO RCODEO (M1)
           ELSE
               MOVE eo-eob-code        TO RCODEO (M1)
           END-IF
           MOVE eo-DESCRIPTION         TO DESCO (M1)
           IF PI-RETURN-TO-PROGRAM = 'EL6318'
              PERFORM VARYING S2 FROM +1 BY +1 UNTIL
                S2 > +26
                  IF RCODEO (M1)(4:1) = PI-REASON-CODE-FLAG (S2)
                     MOVE AL-SABON TO RCODEA (M1)
                     MOVE AL-SABOF TO DESCA (M1)
                  END-IF
              END-PERFORM
           END-IF
           IF PI-RETURN-TO-PROGRAM = 'EL156' OR 'EL6317'
              OR 'EL6316'
              PERFORM VARYING E1 FROM +1 BY +1 UNTIL
                 E1 > +12
                 IF eo-EOB-CODE = PI-EOB-CODE (E1)
                    MOVE 'S'           TO SELI (M1)
                    MOVE +1            TO SELL (M1)
                 END-IF
              END-PERFORM
           END-IF
           IF PI-RETURN-TO-PROGRAM = 'EL6318'
              PERFORM VARYING E1 FROM +1 BY +1 UNTIL
                 E1 > +12
                 MOVE PI-EOB-CODE (E1) TO WS-SCREEN-CODE
                 MOVE WS-SCR-CODE-1-3  TO WS-TAB-CODE-2-4
                 MOVE WS-SCR-CODE-4    TO WS-TAB-CODE-1
                 IF eo-EOB-CODE = WS-TABLE-CODE
                    MOVE 'S'           TO SELI (M1)
                    MOVE +1            TO SELL (M1)
                 END-IF
              END-PERFORM
           END-IF
           SET M2 TO M1
           IF PI-EXPANDED-VIEW = 'Y'
              IF EO-DESCRIPTION (61:60) > SPACES
                 SET N1 TO M1
                 SET M1 UP BY +1
                 MOVE EO-DESCRIPTION (61:60) TO DESCO(M1)
                 MOVE DESCA (N1)       TO DESCA (M1)
                 MOVE AL-SADOF         TO SELA (M1)
              END-IF
              IF EO-DESCRIPTION (121:60) > SPACES
                 SET N1 TO M1
                 SET M1 UP BY +1
                 MOVE EO-DESCRIPTION (121:60) TO DESCO(M1)
                 MOVE DESCA (N1)       TO DESCA (M1)
                 MOVE AL-SADOF         TO SELA (M1)
              END-IF
              IF EO-DESCRIPTION (181:60) > SPACES
                 SET N1 TO M1
                 SET M1 UP BY +1
                 MOVE EO-DESCRIPTION (181:60) TO DESCO(M1)
                 MOVE DESCA (N1)       TO DESCA (M1)
                 MOVE AL-SADOF         TO SELA (M1)
              END-IF
              IF EO-DESCRIPTION (241:35) > SPACES
                 SET N1 TO M1
                 SET M1 UP BY +1
                 MOVE EO-DESCRIPTION (241:35) TO DESCO(M1)
                 MOVE DESCA (N1)       TO DESCA (M1)
                 MOVE AL-SADOF         TO SELA (M1)
              END-IF
           END-IF
           SET M1 TO M2
           if m1 > +1
              set m1 down by +1
              go to 7160-loop
           end-if
           .
112113 7180-ENDBR.
           if pi-return-to-program = 'EL6318'
              if pi-6318-pass = '2'
                 and true-endbr
                 if m1 > +1
                    move '1' to pi-6318-pass
                    
      * EXEC CICS ENDBR
      *                DATASET ('ELEOBC')
      *             END-EXEC
           MOVE 'ELEOBC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002480' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                    MOVE +0            TO PI-BROWSE-SW
                    move '4' to ws-eob-rec-type
                    move spaces to ws-eob-code
                    
      * EXEC CICS STARTBR
      *                 DATASET   ('ELEOBC')
      *                 RIDFLD    (WS-ELEOBC-KEY)
      *                 LTEQ
      *                 RESP      (WS-RESPONSE)
      *             END-EXEC
           MOVE 'ELEOBC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         L          &  N#00002486' TO DFHEIV0
           MOVE X'262C2020202020202020204C' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303032343836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-ELEOBC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                    move ' ' to ws-endbr-sw
                    go to 7150-check-startbr
                 end-if
              end-if
           end-if
           IF PI-BROWSE-SW = +1
              
      * EXEC CICS ENDBR
      *          DATASET ('ELEOBC')
      *       END-EXEC
           MOVE 'ELEOBC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002498' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              MOVE +0                  TO PI-BROWSE-SW
           END-IF
           IF m1 = 0
               MOVE ER-0006            TO EMI-ERROR
               MOVE -1                 TO APFKL
               MOVE AL-UABOF           TO APFKA
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               MOVE ZEROS              TO PI-TOTAL-LINES
               GO TO 8100-SEND-INITIAL-MAP
           END-IF.
           if true-endbr
              and pi-6318-pass not = '2'
              MOVE ER-1698             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           end-if
           IF m1 > +1
              move pi-dn-top-key to ws-eleobc-key
              go to 7000-browse-fwd
           END-IF
           go to 8100-send-initial-map
           .
112113 7250-DELETE-TEMP-STOR.
112113     
      * EXEC CICS HANDLE CONDITION
112113*         QIDERR(7299-EXIT)
112113*    END-EXEC.
      *    MOVE '"$N                   ! # #00002523' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303032353233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
112113     
      * EXEC CICS DELETEQ TS
112113*         QUEUE(QID)
112113*    END-EXEC.
      *    MOVE '*&                    #   #00002526' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
112113 7299-EXIT.
112113     EXIT.
112113
112113 7500-READ-TS.
112113     
      * EXEC CICS HANDLE CONDITION
112113*         QIDERR(7590-TS-QIDERR)
112113*         ITEMERR(7585-QID-ITEMERR)
112113*    END-EXEC.
      *    MOVE '"$N<                  ! $ #00002533' TO DFHEIV0
           MOVE X'22244E3C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303032353333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
112113     MOVE 1                      TO QID-ITEM.
112113 7501-LOOP.
112113     
      * EXEC CICS READQ TS
112113*         INTO(TS-WORK-AREA)
112113*         QUEUE(QID)
112113*         LENGTH(TS-LENGTH)
112113*         ITEM(QID-ITEM)
112113*    END-EXEC.
      *    MOVE '*$II   L              ''   #00002539' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 TS-WORK-AREA, 
                 TS-LENGTH, 
                 QID-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
112113     ADD 1 TO QID-ITEM.
112113     GO TO 7501-LOOP.
112113
112113 7585-QID-ITEMERR.
112113     IF EIBTRNID NOT = WS-TRANS-ID
112113        SUBTRACT 1 FROM QID-ITEM
112113        MOVE QID-ITEM            TO PI-TEMP-STOR-ITEMS
112113     END-IF.
112113     GO TO 7599-EXIT.
112113
112113 7590-TS-QIDERR.
112113     IF EIBTRNID = WS-TRANS-ID
112113        AND EIBAID = DFHCLEAR
112113           GO TO 9100-RETURN-TRAN
112113     END-IF.
112113     IF EIBTRNID = WS-TRANS-ID
112113        MOVE ER-0033             TO EMI-ERROR
112113        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
112113        GO TO 8100-SEND-INITIAL-MAP
112113     END-IF.
112113
112113 7599-EXIT.
112113      EXIT.
      *****************************************************************
       8100-SEND-INITIAL-MAP.
      *****************************************************************
           MOVE -1                     TO  APFKL.
           MOVE ZERO                   TO  PI-BROWSE-SW.
           MOVE SAVE-DATE              TO  ADATEO.
           MOVE EIBTIME                TO  TIME-IN.
           MOVE TIME-OUT               TO  ATIMEO.
           IF EMI-ERROR NOT = ZERO
               PERFORM 9900-ERROR-FORMAT
           END-IF
           MOVE EMI-MESSAGE-AREA (1)    TO  AEMSGO
           IF PI-RETURN-TO-PROGRAM NOT = 'EL156' AND 'EL6317' AND
                 'EL6318' AND 'EL6316'
              MOVE AL-SADOF            TO SELHA
                                          F3KEYA
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 S1 > +17
                 MOVE AL-SADOF         TO SELA (S1)
              END-PERFORM
           END-IF
121012
121012     IF PI-EXPANDED-VIEW = 'Y'
121012         MOVE 'PF5=UNEXPAND DESCRIPTIONS' TO F5KEYO
121012     ELSE
121012         MOVE 'PF5=EXPAND DESCRIPTIONS  ' TO F5KEYO
121012     END-IF
           
      * EXEC CICS SEND
      *        FROM   (EL614AO)
      *        MAPSET (WS-MAPSET-NAME)
      *        MAP    (WS-MAP-NAME)
      *        CURSOR ERASE
      *    END-EXEC.
           MOVE LENGTH OF
            EL614AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00002595' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL614AO, 
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
      *****************************************************************
       8200-SEND-DATAONLY.
      *****************************************************************
           MOVE SAVE-DATE              TO  ADATEO.
           MOVE EIBTIME                TO  TIME-IN.
           MOVE TIME-OUT               TO  ATIMEO.
           IF EMI-ERROR NOT = ZERO
               PERFORM 9900-ERROR-FORMAT.
           MOVE EMI-MESSAGE-AREA (1)    TO  AEMSGO.
           IF PI-RETURN-TO-PROGRAM NOT = 'EL156' AND 'EL6317' AND
                 'EL6318' AND 'EL6316'
              MOVE AL-SADOF            TO SELHA
                                          F3KEYA
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 S1 > +17
                 MOVE AL-SADOF         TO SELA (S1)
              END-PERFORM
           END-IF
           
      * EXEC CICS SEND DATAONLY
      *        FROM   (EL614AO)
      *        MAPSET (WS-MAPSET-NAME)
      *        MAP    (WS-MAP-NAME)
      *        CURSOR
      *    END-EXEC.
           MOVE LENGTH OF
            EL614AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00002623' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL614AO, 
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
      *    MOVE '8&      T  E F  H   F -   #00002637' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363337' TO DFHEIV0(25:11)
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
      *    MOVE '.(                    ''   #00002642' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363432' TO DFHEIV0(25:11)
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
      *    MOVE '."C                   (   #00002666' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363636' TO DFHEIV0(25:11)
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
      *    MOVE '.(CT                  ''   #00002687' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363837' TO DFHEIV0(25:11)
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
      *    MOVE '.$C                   %   #00002698' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363938' TO DFHEIV0(25:11)
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
      *    MOVE '"$L                   ! % #00002716' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303032373136' TO DFHEIV0(25:11)
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
      *    MOVE '."C                   (   #00002734' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373334' TO DFHEIV0(25:11)
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
      *    MOVE '."C                   (   #00002747' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373437' TO DFHEIV0(25:11)
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
      *    MOVE '."C                   (   #00002776' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373736' TO DFHEIV0(25:11)
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
       9999-LAST-PARAGRAPH SECTION.
           
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL614' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL614' TO DFHEIV1
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
               GO TO 7299-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 7590-TS-QIDERR,
                     7585-QID-ITEMERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL614' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
