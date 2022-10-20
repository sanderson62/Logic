00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL144 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 04/19/95 13:02:02.
00007 *                            VMOD=2.007
00008 *
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
00025 *        THIS PROGRAM DISPLAYS ALL PENDING ACTIVITY
00026 *    FROM THE ACTIVITY-QUE FILE ALONG WITH THE TYPE OF ACTIVITY.
00027 *
00028 *    SCREENS     - EL144A - PENDING ACTIVITY REVIEW
00029 *                  EL144B - ACTIVITY MAINTENANCE
00030 *    ENTERED BY  - EL126  - PROCESSING MENU
00031 *
00032 *    EXIT TO     - EL126  - RESULT OF CLEAR
00033 *
00034 *    INPUT FILES - ELACTQ - ACTIVITY-QUE FILE
00035 *
00036 *    OUTPUT FILES - ELACTQ
00037 *
00038 *    COMMAREA    - PASSED.
00039 *
00040 *
00041 *    NARRATIVE   - ALL ACTIVITY QUE RECORDS ARE READ , THE TYPE
00042 *                  OF ACTION IS DETERMINED, THE SCREEN IS BUILT
00043 *                  THEN SENT.
00044
00045      EJECT
00046  ENVIRONMENT DIVISION.
00047
00048  DATA DIVISION.
00049
00050  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00051
00052  77  FILLER  PIC X(32)  VALUE '********************************'.
00053  77  FILLER  PIC X(32)  VALUE '*   EL144  WORKING STORAGE     *'.
00054  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.007 *********'.
00055
00056 *    COPY ELCSCTM.
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
00057
00058 *    COPY ELCSCRTY.
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
00059
00060  01  WS-DATE-AREA.
00061      12  SAVE-DATE           PIC X(8)    VALUE SPACES.
00062      12  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
00063
00064  01  FILLER                          COMP-3.
00065      12  WS-READNEXT-SW              PIC S9          VALUE ZERO.
00066
00067      12  TIME-IN                     PIC S9(7)       VALUE ZERO.
00068      12  TIME-OUT                    REDEFINES
00069          TIME-IN                     PIC S9(3)V9(4).
00070
00071  01  FILLER.
00072      12  WS-DEEDIT-FIELD             PIC X(8) VALUE SPACES.
00073      12  WS-DEEDIT-FIELD-V0 REDEFINES WS-DEEDIT-FIELD
00074                                      PIC S9(8).
00075      12  WS-DEEDIT-LENGTH            PIC S9(4) VALUE +8 COMP.
00076      12  GETMAIN-SPACE               PIC X     VALUE SPACE.
00077      12  WS-ELACTQ-LENGTH            PIC S9(04) COMP VALUE +60.
00078      12  WS-PMT-COUNT                PIC S9(01) COMP-3 VALUE +0.
00079      12  WS-UNA-PMT-COUNT            PIC S9(01) COMP-3 VALUE +0.
00080      12  WS-BROWSE-SW                PIC X(01) VALUE SPACES.
00081      12  SC-ITEM                     PIC S9(04) COMP VALUE +1.
00082      12  WS-AQ-CONTROL-PRIMARY.
00083          16  WS-AQ-COMPANY-CD        PIC X(01).
00084          16  WS-AQ-CARRIER           PIC X(01).
00085          16  WS-AQ-CLAIM-NO          PIC X(07).
00086          16  WS-AQ-CERT-PRIME        PIC X(10).
00087          16  WS-AQ-CERT-SFX          PIC X(01).
00088      12  WS-ELACTQ-DSID              PIC X(08)   VALUE 'ELACTQ'.
00089      12  WS-ELMSTR-DSID              PIC X(08)   VALUE 'ELMSTR'.
00090      12  WS-MAPSET-NAME              PIC X(8)    VALUE 'EL144S'.
00091      12  WS-MAP-NAME                 PIC X(8)    VALUE 'EL144A'.
00092
00093      12  FILLER                      REDEFINES
00094          WS-MAP-NAME.
00095          16  FILLER                  PIC X(02).
00096          16  WS-MAP-NUMBER           PIC X(04).
00097          16  FILLER                  PIC X(02).
00098
00099      12  THIS-PGM                    PIC X(8)      VALUE 'EL144'.
00100      12  XCTL-PGM                    PIC X(8).
00101
00102      12  WS-TRANS-ID                 PIC X(4)        VALUE 'EX54'.
00103
00104      12  ER-0000                     PIC 9(4)        VALUE 0000.
00105      12  ER-0004                     PIC 9(4)        VALUE 0004.
00106      12  ER-0005                     PIC 9(4)        VALUE 0005.
00107      12  ER-0008                     PIC 9(4)        VALUE 0008.
00108      12  ER-0029                     PIC 9(4)        VALUE 0029.
00109      12  ER-0048                     PIC 9(4)        VALUE 0048.
00110      12  ER-0070                     PIC 9(4)        VALUE 0070.
00111      12  ER-0284                     PIC 9(4)        VALUE 0284.
00112      12  ER-0295                     PIC 9(4)        VALUE 0295.
00113      12  ER-0296                     PIC 9(4)        VALUE 0296.
00114      12  ER-0312                     PIC 9(4)        VALUE 0312.
00115      12  ER-0313                     PIC 9(4)        VALUE 0313.
00116      12  ER-0676                     PIC 9(4)        VALUE 0676.
00117      12  ER-0677                     PIC 9(4)        VALUE 0677.
00118      12  ER-0678                     PIC 9(4)        VALUE 0678.
00119      12  ER-0679                     PIC 9(4)        VALUE 0679.
00120      12  ER-0980                     PIC 9(4)        VALUE 0980.
00121
00122      EJECT
00123 *    COPY ELCINTF.
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
00124
00125      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00126          16  PI-TOP-KEY       PIC X(20).
00127          16  PI-BOT-KEY       PIC X(20).
00128          16  PI-MAP-NAME      PIC X(06).
00129          16  FILLER           PIC X(586).
00130          16  PI-EIBAID-LAST   PIC X(8).
00131      EJECT
00132 *    COPY EL144S.
       01  EL144AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  DATEL PIC S9(0004) COMP.
           05  DATEF PIC  X(0001).
           05  FILLER REDEFINES DATEF.
               10  DATEA PIC  X(0001).
           05  DATEI PIC  X(0008).
      *    -------------------------------
           05  TIMEL PIC S9(0004) COMP.
           05  TIMEF PIC  X(0001).
           05  FILLER REDEFINES TIMEF.
               10  TIMEA PIC  X(0001).
           05  TIMEI PIC  X(0005).
      *    -------------------------------
           05  CARRL PIC S9(0004) COMP.
           05  CARRF PIC  X(0001).
           05  FILLER REDEFINES CARRF.
               10  CARRA PIC  X(0001).
           05  CARRI PIC  X(0001).
      *    -------------------------------
           05  CLAIML PIC S9(0004) COMP.
           05  CLAIMF PIC  X(0001).
           05  FILLER REDEFINES CLAIMF.
               10  CLAIMA PIC  X(0001).
           05  CLAIMI PIC  X(0007).
      *    -------------------------------
           05  CERTL PIC S9(0004) COMP.
           05  CERTF PIC  X(0001).
           05  FILLER REDEFINES CERTF.
               10  CERTA PIC  X(0001).
           05  CERTI PIC  X(0010).
      *    -------------------------------
           05  SFXL PIC S9(0004) COMP.
           05  SFXF PIC  X(0001).
           05  FILLER REDEFINES SFXF.
               10  SFXA PIC  X(0001).
           05  SFXI PIC  X(0001).
      *    -------------------------------
           05  ACLAM01L PIC S9(0004) COMP.
           05  ACLAM01F PIC  X(0001).
           05  FILLER REDEFINES ACLAM01F.
               10  ACLAM01A PIC  X(0001).
           05  ACLAM01I PIC  X(0007).
      *    -------------------------------
           05  ACARR01L PIC S9(0004) COMP.
           05  ACARR01F PIC  X(0001).
           05  FILLER REDEFINES ACARR01F.
               10  ACARR01A PIC  X(0001).
           05  ACARR01I PIC  X(0001).
      *    -------------------------------
           05  ACERT01L PIC S9(0004) COMP.
           05  ACERT01F PIC  X(0001).
           05  FILLER REDEFINES ACERT01F.
               10  ACERT01A PIC  X(0001).
           05  ACERT01I PIC  X(0011).
      *    -------------------------------
           05  APMTS01L PIC S9(0004) COMP.
           05  APMTS01F PIC  X(0001).
           05  FILLER REDEFINES APMTS01F.
               10  APMTS01A PIC  X(0001).
           05  APMTS01I PIC  X(0003).
      *    -------------------------------
           05  APCNT01L PIC S9(0004) COMP.
           05  APCNT01F PIC  X(0001).
           05  FILLER REDEFINES APCNT01F.
               10  APCNT01A PIC  X(0001).
           05  APCNT01I PIC  X(0001).
      *    -------------------------------
           05  AUCNT01L PIC S9(0004) COMP.
           05  AUCNT01F PIC  X(0001).
           05  FILLER REDEFINES AUCNT01F.
               10  AUCNT01A PIC  X(0001).
           05  AUCNT01I PIC  X(0001).
      *    -------------------------------
           05  ASTAT01L PIC S9(0004) COMP.
           05  ASTAT01F PIC  X(0001).
           05  FILLER REDEFINES ASTAT01F.
               10  ASTAT01A PIC  X(0001).
           05  ASTAT01I PIC  X(0006).
      *    -------------------------------
           05  ALETR01L PIC S9(0004) COMP.
           05  ALETR01F PIC  X(0001).
           05  FILLER REDEFINES ALETR01F.
               10  ALETR01A PIC  X(0001).
           05  ALETR01I PIC  X(0004).
      *    -------------------------------
           05  AREST01L PIC S9(0004) COMP.
           05  AREST01F PIC  X(0001).
           05  FILLER REDEFINES AREST01F.
               10  AREST01A PIC  X(0001).
           05  AREST01I PIC  X(0001).
      *    -------------------------------
           05  ACLAM02L PIC S9(0004) COMP.
           05  ACLAM02F PIC  X(0001).
           05  FILLER REDEFINES ACLAM02F.
               10  ACLAM02A PIC  X(0001).
           05  ACLAM02I PIC  X(0007).
      *    -------------------------------
           05  ACARR02L PIC S9(0004) COMP.
           05  ACARR02F PIC  X(0001).
           05  FILLER REDEFINES ACARR02F.
               10  ACARR02A PIC  X(0001).
           05  ACARR02I PIC  X(0001).
      *    -------------------------------
           05  ACERT02L PIC S9(0004) COMP.
           05  ACERT02F PIC  X(0001).
           05  FILLER REDEFINES ACERT02F.
               10  ACERT02A PIC  X(0001).
           05  ACERT02I PIC  X(0011).
      *    -------------------------------
           05  APMTS02L PIC S9(0004) COMP.
           05  APMTS02F PIC  X(0001).
           05  FILLER REDEFINES APMTS02F.
               10  APMTS02A PIC  X(0001).
           05  APMTS02I PIC  X(0003).
      *    -------------------------------
           05  APCNT02L PIC S9(0004) COMP.
           05  APCNT02F PIC  X(0001).
           05  FILLER REDEFINES APCNT02F.
               10  APCNT02A PIC  X(0001).
           05  APCNT02I PIC  X(0001).
      *    -------------------------------
           05  AUCNT02L PIC S9(0004) COMP.
           05  AUCNT02F PIC  X(0001).
           05  FILLER REDEFINES AUCNT02F.
               10  AUCNT02A PIC  X(0001).
           05  AUCNT02I PIC  X(0001).
      *    -------------------------------
           05  ASTAT02L PIC S9(0004) COMP.
           05  ASTAT02F PIC  X(0001).
           05  FILLER REDEFINES ASTAT02F.
               10  ASTAT02A PIC  X(0001).
           05  ASTAT02I PIC  X(0006).
      *    -------------------------------
           05  ALETR02L PIC S9(0004) COMP.
           05  ALETR02F PIC  X(0001).
           05  FILLER REDEFINES ALETR02F.
               10  ALETR02A PIC  X(0001).
           05  ALETR02I PIC  X(0004).
      *    -------------------------------
           05  AREST02L PIC S9(0004) COMP.
           05  AREST02F PIC  X(0001).
           05  FILLER REDEFINES AREST02F.
               10  AREST02A PIC  X(0001).
           05  AREST02I PIC  X(0001).
      *    -------------------------------
           05  ACLAM03L PIC S9(0004) COMP.
           05  ACLAM03F PIC  X(0001).
           05  FILLER REDEFINES ACLAM03F.
               10  ACLAM03A PIC  X(0001).
           05  ACLAM03I PIC  X(0007).
      *    -------------------------------
           05  ACARR03L PIC S9(0004) COMP.
           05  ACARR03F PIC  X(0001).
           05  FILLER REDEFINES ACARR03F.
               10  ACARR03A PIC  X(0001).
           05  ACARR03I PIC  X(0001).
      *    -------------------------------
           05  ACERT03L PIC S9(0004) COMP.
           05  ACERT03F PIC  X(0001).
           05  FILLER REDEFINES ACERT03F.
               10  ACERT03A PIC  X(0001).
           05  ACERT03I PIC  X(0011).
      *    -------------------------------
           05  APMTS03L PIC S9(0004) COMP.
           05  APMTS03F PIC  X(0001).
           05  FILLER REDEFINES APMTS03F.
               10  APMTS03A PIC  X(0001).
           05  APMTS03I PIC  X(0003).
      *    -------------------------------
           05  APCNT03L PIC S9(0004) COMP.
           05  APCNT03F PIC  X(0001).
           05  FILLER REDEFINES APCNT03F.
               10  APCNT03A PIC  X(0001).
           05  APCNT03I PIC  X(0001).
      *    -------------------------------
           05  AUCNT03L PIC S9(0004) COMP.
           05  AUCNT03F PIC  X(0001).
           05  FILLER REDEFINES AUCNT03F.
               10  AUCNT03A PIC  X(0001).
           05  AUCNT03I PIC  X(0001).
      *    -------------------------------
           05  ASTAT03L PIC S9(0004) COMP.
           05  ASTAT03F PIC  X(0001).
           05  FILLER REDEFINES ASTAT03F.
               10  ASTAT03A PIC  X(0001).
           05  ASTAT03I PIC  X(0006).
      *    -------------------------------
           05  ALETR03L PIC S9(0004) COMP.
           05  ALETR03F PIC  X(0001).
           05  FILLER REDEFINES ALETR03F.
               10  ALETR03A PIC  X(0001).
           05  ALETR03I PIC  X(0004).
      *    -------------------------------
           05  AREST03L PIC S9(0004) COMP.
           05  AREST03F PIC  X(0001).
           05  FILLER REDEFINES AREST03F.
               10  AREST03A PIC  X(0001).
           05  AREST03I PIC  X(0001).
      *    -------------------------------
           05  ACLAM04L PIC S9(0004) COMP.
           05  ACLAM04F PIC  X(0001).
           05  FILLER REDEFINES ACLAM04F.
               10  ACLAM04A PIC  X(0001).
           05  ACLAM04I PIC  X(0007).
      *    -------------------------------
           05  ACARR04L PIC S9(0004) COMP.
           05  ACARR04F PIC  X(0001).
           05  FILLER REDEFINES ACARR04F.
               10  ACARR04A PIC  X(0001).
           05  ACARR04I PIC  X(0001).
      *    -------------------------------
           05  ACERT04L PIC S9(0004) COMP.
           05  ACERT04F PIC  X(0001).
           05  FILLER REDEFINES ACERT04F.
               10  ACERT04A PIC  X(0001).
           05  ACERT04I PIC  X(0011).
      *    -------------------------------
           05  APMTS04L PIC S9(0004) COMP.
           05  APMTS04F PIC  X(0001).
           05  FILLER REDEFINES APMTS04F.
               10  APMTS04A PIC  X(0001).
           05  APMTS04I PIC  X(0003).
      *    -------------------------------
           05  APCNT04L PIC S9(0004) COMP.
           05  APCNT04F PIC  X(0001).
           05  FILLER REDEFINES APCNT04F.
               10  APCNT04A PIC  X(0001).
           05  APCNT04I PIC  X(0001).
      *    -------------------------------
           05  AUCNT04L PIC S9(0004) COMP.
           05  AUCNT04F PIC  X(0001).
           05  FILLER REDEFINES AUCNT04F.
               10  AUCNT04A PIC  X(0001).
           05  AUCNT04I PIC  X(0001).
      *    -------------------------------
           05  ASTAT04L PIC S9(0004) COMP.
           05  ASTAT04F PIC  X(0001).
           05  FILLER REDEFINES ASTAT04F.
               10  ASTAT04A PIC  X(0001).
           05  ASTAT04I PIC  X(0006).
      *    -------------------------------
           05  ALETR04L PIC S9(0004) COMP.
           05  ALETR04F PIC  X(0001).
           05  FILLER REDEFINES ALETR04F.
               10  ALETR04A PIC  X(0001).
           05  ALETR04I PIC  X(0004).
      *    -------------------------------
           05  AREST04L PIC S9(0004) COMP.
           05  AREST04F PIC  X(0001).
           05  FILLER REDEFINES AREST04F.
               10  AREST04A PIC  X(0001).
           05  AREST04I PIC  X(0001).
      *    -------------------------------
           05  ACLAM05L PIC S9(0004) COMP.
           05  ACLAM05F PIC  X(0001).
           05  FILLER REDEFINES ACLAM05F.
               10  ACLAM05A PIC  X(0001).
           05  ACLAM05I PIC  X(0007).
      *    -------------------------------
           05  ACARR05L PIC S9(0004) COMP.
           05  ACARR05F PIC  X(0001).
           05  FILLER REDEFINES ACARR05F.
               10  ACARR05A PIC  X(0001).
           05  ACARR05I PIC  X(0001).
      *    -------------------------------
           05  ACERT05L PIC S9(0004) COMP.
           05  ACERT05F PIC  X(0001).
           05  FILLER REDEFINES ACERT05F.
               10  ACERT05A PIC  X(0001).
           05  ACERT05I PIC  X(0011).
      *    -------------------------------
           05  APMTS05L PIC S9(0004) COMP.
           05  APMTS05F PIC  X(0001).
           05  FILLER REDEFINES APMTS05F.
               10  APMTS05A PIC  X(0001).
           05  APMTS05I PIC  X(0003).
      *    -------------------------------
           05  APCNT05L PIC S9(0004) COMP.
           05  APCNT05F PIC  X(0001).
           05  FILLER REDEFINES APCNT05F.
               10  APCNT05A PIC  X(0001).
           05  APCNT05I PIC  X(0001).
      *    -------------------------------
           05  AUCNT05L PIC S9(0004) COMP.
           05  AUCNT05F PIC  X(0001).
           05  FILLER REDEFINES AUCNT05F.
               10  AUCNT05A PIC  X(0001).
           05  AUCNT05I PIC  X(0001).
      *    -------------------------------
           05  ASTAT05L PIC S9(0004) COMP.
           05  ASTAT05F PIC  X(0001).
           05  FILLER REDEFINES ASTAT05F.
               10  ASTAT05A PIC  X(0001).
           05  ASTAT05I PIC  X(0006).
      *    -------------------------------
           05  ALETR05L PIC S9(0004) COMP.
           05  ALETR05F PIC  X(0001).
           05  FILLER REDEFINES ALETR05F.
               10  ALETR05A PIC  X(0001).
           05  ALETR05I PIC  X(0004).
      *    -------------------------------
           05  AREST05L PIC S9(0004) COMP.
           05  AREST05F PIC  X(0001).
           05  FILLER REDEFINES AREST05F.
               10  AREST05A PIC  X(0001).
           05  AREST05I PIC  X(0001).
      *    -------------------------------
           05  ACLAM06L PIC S9(0004) COMP.
           05  ACLAM06F PIC  X(0001).
           05  FILLER REDEFINES ACLAM06F.
               10  ACLAM06A PIC  X(0001).
           05  ACLAM06I PIC  X(0007).
      *    -------------------------------
           05  ACARR06L PIC S9(0004) COMP.
           05  ACARR06F PIC  X(0001).
           05  FILLER REDEFINES ACARR06F.
               10  ACARR06A PIC  X(0001).
           05  ACARR06I PIC  X(0001).
      *    -------------------------------
           05  ACERT06L PIC S9(0004) COMP.
           05  ACERT06F PIC  X(0001).
           05  FILLER REDEFINES ACERT06F.
               10  ACERT06A PIC  X(0001).
           05  ACERT06I PIC  X(0011).
      *    -------------------------------
           05  APMTS06L PIC S9(0004) COMP.
           05  APMTS06F PIC  X(0001).
           05  FILLER REDEFINES APMTS06F.
               10  APMTS06A PIC  X(0001).
           05  APMTS06I PIC  X(0003).
      *    -------------------------------
           05  APCNT06L PIC S9(0004) COMP.
           05  APCNT06F PIC  X(0001).
           05  FILLER REDEFINES APCNT06F.
               10  APCNT06A PIC  X(0001).
           05  APCNT06I PIC  X(0001).
      *    -------------------------------
           05  AUCNT06L PIC S9(0004) COMP.
           05  AUCNT06F PIC  X(0001).
           05  FILLER REDEFINES AUCNT06F.
               10  AUCNT06A PIC  X(0001).
           05  AUCNT06I PIC  X(0001).
      *    -------------------------------
           05  ASTAT06L PIC S9(0004) COMP.
           05  ASTAT06F PIC  X(0001).
           05  FILLER REDEFINES ASTAT06F.
               10  ASTAT06A PIC  X(0001).
           05  ASTAT06I PIC  X(0006).
      *    -------------------------------
           05  ALETR06L PIC S9(0004) COMP.
           05  ALETR06F PIC  X(0001).
           05  FILLER REDEFINES ALETR06F.
               10  ALETR06A PIC  X(0001).
           05  ALETR06I PIC  X(0004).
      *    -------------------------------
           05  AREST06L PIC S9(0004) COMP.
           05  AREST06F PIC  X(0001).
           05  FILLER REDEFINES AREST06F.
               10  AREST06A PIC  X(0001).
           05  AREST06I PIC  X(0001).
      *    -------------------------------
           05  ACLAM07L PIC S9(0004) COMP.
           05  ACLAM07F PIC  X(0001).
           05  FILLER REDEFINES ACLAM07F.
               10  ACLAM07A PIC  X(0001).
           05  ACLAM07I PIC  X(0007).
      *    -------------------------------
           05  ACARR07L PIC S9(0004) COMP.
           05  ACARR07F PIC  X(0001).
           05  FILLER REDEFINES ACARR07F.
               10  ACARR07A PIC  X(0001).
           05  ACARR07I PIC  X(0001).
      *    -------------------------------
           05  ACERT07L PIC S9(0004) COMP.
           05  ACERT07F PIC  X(0001).
           05  FILLER REDEFINES ACERT07F.
               10  ACERT07A PIC  X(0001).
           05  ACERT07I PIC  X(0011).
      *    -------------------------------
           05  APMTS07L PIC S9(0004) COMP.
           05  APMTS07F PIC  X(0001).
           05  FILLER REDEFINES APMTS07F.
               10  APMTS07A PIC  X(0001).
           05  APMTS07I PIC  X(0003).
      *    -------------------------------
           05  APCNT07L PIC S9(0004) COMP.
           05  APCNT07F PIC  X(0001).
           05  FILLER REDEFINES APCNT07F.
               10  APCNT07A PIC  X(0001).
           05  APCNT07I PIC  X(0001).
      *    -------------------------------
           05  AUCNT07L PIC S9(0004) COMP.
           05  AUCNT07F PIC  X(0001).
           05  FILLER REDEFINES AUCNT07F.
               10  AUCNT07A PIC  X(0001).
           05  AUCNT07I PIC  X(0001).
      *    -------------------------------
           05  ASTAT07L PIC S9(0004) COMP.
           05  ASTAT07F PIC  X(0001).
           05  FILLER REDEFINES ASTAT07F.
               10  ASTAT07A PIC  X(0001).
           05  ASTAT07I PIC  X(0006).
      *    -------------------------------
           05  ALETR07L PIC S9(0004) COMP.
           05  ALETR07F PIC  X(0001).
           05  FILLER REDEFINES ALETR07F.
               10  ALETR07A PIC  X(0001).
           05  ALETR07I PIC  X(0004).
      *    -------------------------------
           05  AREST07L PIC S9(0004) COMP.
           05  AREST07F PIC  X(0001).
           05  FILLER REDEFINES AREST07F.
               10  AREST07A PIC  X(0001).
           05  AREST07I PIC  X(0001).
      *    -------------------------------
           05  ACLAM08L PIC S9(0004) COMP.
           05  ACLAM08F PIC  X(0001).
           05  FILLER REDEFINES ACLAM08F.
               10  ACLAM08A PIC  X(0001).
           05  ACLAM08I PIC  X(0007).
      *    -------------------------------
           05  ACARR08L PIC S9(0004) COMP.
           05  ACARR08F PIC  X(0001).
           05  FILLER REDEFINES ACARR08F.
               10  ACARR08A PIC  X(0001).
           05  ACARR08I PIC  X(0001).
      *    -------------------------------
           05  ACERT08L PIC S9(0004) COMP.
           05  ACERT08F PIC  X(0001).
           05  FILLER REDEFINES ACERT08F.
               10  ACERT08A PIC  X(0001).
           05  ACERT08I PIC  X(0011).
      *    -------------------------------
           05  APMTS08L PIC S9(0004) COMP.
           05  APMTS08F PIC  X(0001).
           05  FILLER REDEFINES APMTS08F.
               10  APMTS08A PIC  X(0001).
           05  APMTS08I PIC  X(0003).
      *    -------------------------------
           05  APCNT08L PIC S9(0004) COMP.
           05  APCNT08F PIC  X(0001).
           05  FILLER REDEFINES APCNT08F.
               10  APCNT08A PIC  X(0001).
           05  APCNT08I PIC  X(0001).
      *    -------------------------------
           05  AUCNT08L PIC S9(0004) COMP.
           05  AUCNT08F PIC  X(0001).
           05  FILLER REDEFINES AUCNT08F.
               10  AUCNT08A PIC  X(0001).
           05  AUCNT08I PIC  X(0001).
      *    -------------------------------
           05  ASTAT08L PIC S9(0004) COMP.
           05  ASTAT08F PIC  X(0001).
           05  FILLER REDEFINES ASTAT08F.
               10  ASTAT08A PIC  X(0001).
           05  ASTAT08I PIC  X(0006).
      *    -------------------------------
           05  ALETR08L PIC S9(0004) COMP.
           05  ALETR08F PIC  X(0001).
           05  FILLER REDEFINES ALETR08F.
               10  ALETR08A PIC  X(0001).
           05  ALETR08I PIC  X(0004).
      *    -------------------------------
           05  AREST08L PIC S9(0004) COMP.
           05  AREST08F PIC  X(0001).
           05  FILLER REDEFINES AREST08F.
               10  AREST08A PIC  X(0001).
           05  AREST08I PIC  X(0001).
      *    -------------------------------
           05  ACLAM09L PIC S9(0004) COMP.
           05  ACLAM09F PIC  X(0001).
           05  FILLER REDEFINES ACLAM09F.
               10  ACLAM09A PIC  X(0001).
           05  ACLAM09I PIC  X(0007).
      *    -------------------------------
           05  ACARR09L PIC S9(0004) COMP.
           05  ACARR09F PIC  X(0001).
           05  FILLER REDEFINES ACARR09F.
               10  ACARR09A PIC  X(0001).
           05  ACARR09I PIC  X(0001).
      *    -------------------------------
           05  ACERT09L PIC S9(0004) COMP.
           05  ACERT09F PIC  X(0001).
           05  FILLER REDEFINES ACERT09F.
               10  ACERT09A PIC  X(0001).
           05  ACERT09I PIC  X(0011).
      *    -------------------------------
           05  APMTS09L PIC S9(0004) COMP.
           05  APMTS09F PIC  X(0001).
           05  FILLER REDEFINES APMTS09F.
               10  APMTS09A PIC  X(0001).
           05  APMTS09I PIC  X(0003).
      *    -------------------------------
           05  APCNT09L PIC S9(0004) COMP.
           05  APCNT09F PIC  X(0001).
           05  FILLER REDEFINES APCNT09F.
               10  APCNT09A PIC  X(0001).
           05  APCNT09I PIC  X(0001).
      *    -------------------------------
           05  AUCNT09L PIC S9(0004) COMP.
           05  AUCNT09F PIC  X(0001).
           05  FILLER REDEFINES AUCNT09F.
               10  AUCNT09A PIC  X(0001).
           05  AUCNT09I PIC  X(0001).
      *    -------------------------------
           05  ASTAT09L PIC S9(0004) COMP.
           05  ASTAT09F PIC  X(0001).
           05  FILLER REDEFINES ASTAT09F.
               10  ASTAT09A PIC  X(0001).
           05  ASTAT09I PIC  X(0006).
      *    -------------------------------
           05  ALETR09L PIC S9(0004) COMP.
           05  ALETR09F PIC  X(0001).
           05  FILLER REDEFINES ALETR09F.
               10  ALETR09A PIC  X(0001).
           05  ALETR09I PIC  X(0004).
      *    -------------------------------
           05  AREST09L PIC S9(0004) COMP.
           05  AREST09F PIC  X(0001).
           05  FILLER REDEFINES AREST09F.
               10  AREST09A PIC  X(0001).
           05  AREST09I PIC  X(0001).
      *    -------------------------------
           05  ACLAM10L PIC S9(0004) COMP.
           05  ACLAM10F PIC  X(0001).
           05  FILLER REDEFINES ACLAM10F.
               10  ACLAM10A PIC  X(0001).
           05  ACLAM10I PIC  X(0007).
      *    -------------------------------
           05  ACARR10L PIC S9(0004) COMP.
           05  ACARR10F PIC  X(0001).
           05  FILLER REDEFINES ACARR10F.
               10  ACARR10A PIC  X(0001).
           05  ACARR10I PIC  X(0001).
      *    -------------------------------
           05  ACERT10L PIC S9(0004) COMP.
           05  ACERT10F PIC  X(0001).
           05  FILLER REDEFINES ACERT10F.
               10  ACERT10A PIC  X(0001).
           05  ACERT10I PIC  X(0011).
      *    -------------------------------
           05  APMTS10L PIC S9(0004) COMP.
           05  APMTS10F PIC  X(0001).
           05  FILLER REDEFINES APMTS10F.
               10  APMTS10A PIC  X(0001).
           05  APMTS10I PIC  X(0003).
      *    -------------------------------
           05  APCNT10L PIC S9(0004) COMP.
           05  APCNT10F PIC  X(0001).
           05  FILLER REDEFINES APCNT10F.
               10  APCNT10A PIC  X(0001).
           05  APCNT10I PIC  X(0001).
      *    -------------------------------
           05  AUCNT10L PIC S9(0004) COMP.
           05  AUCNT10F PIC  X(0001).
           05  FILLER REDEFINES AUCNT10F.
               10  AUCNT10A PIC  X(0001).
           05  AUCNT10I PIC  X(0001).
      *    -------------------------------
           05  ASTAT10L PIC S9(0004) COMP.
           05  ASTAT10F PIC  X(0001).
           05  FILLER REDEFINES ASTAT10F.
               10  ASTAT10A PIC  X(0001).
           05  ASTAT10I PIC  X(0006).
      *    -------------------------------
           05  ALETR10L PIC S9(0004) COMP.
           05  ALETR10F PIC  X(0001).
           05  FILLER REDEFINES ALETR10F.
               10  ALETR10A PIC  X(0001).
           05  ALETR10I PIC  X(0004).
      *    -------------------------------
           05  AREST10L PIC S9(0004) COMP.
           05  AREST10F PIC  X(0001).
           05  FILLER REDEFINES AREST10F.
               10  AREST10A PIC  X(0001).
           05  AREST10I PIC  X(0001).
      *    -------------------------------
           05  ACLAM11L PIC S9(0004) COMP.
           05  ACLAM11F PIC  X(0001).
           05  FILLER REDEFINES ACLAM11F.
               10  ACLAM11A PIC  X(0001).
           05  ACLAM11I PIC  X(0007).
      *    -------------------------------
           05  ACARR11L PIC S9(0004) COMP.
           05  ACARR11F PIC  X(0001).
           05  FILLER REDEFINES ACARR11F.
               10  ACARR11A PIC  X(0001).
           05  ACARR11I PIC  X(0001).
      *    -------------------------------
           05  ACERT11L PIC S9(0004) COMP.
           05  ACERT11F PIC  X(0001).
           05  FILLER REDEFINES ACERT11F.
               10  ACERT11A PIC  X(0001).
           05  ACERT11I PIC  X(0011).
      *    -------------------------------
           05  APMTS11L PIC S9(0004) COMP.
           05  APMTS11F PIC  X(0001).
           05  FILLER REDEFINES APMTS11F.
               10  APMTS11A PIC  X(0001).
           05  APMTS11I PIC  X(0003).
      *    -------------------------------
           05  APCNT11L PIC S9(0004) COMP.
           05  APCNT11F PIC  X(0001).
           05  FILLER REDEFINES APCNT11F.
               10  APCNT11A PIC  X(0001).
           05  APCNT11I PIC  X(0001).
      *    -------------------------------
           05  AUCNT11L PIC S9(0004) COMP.
           05  AUCNT11F PIC  X(0001).
           05  FILLER REDEFINES AUCNT11F.
               10  AUCNT11A PIC  X(0001).
           05  AUCNT11I PIC  X(0001).
      *    -------------------------------
           05  ASTAT11L PIC S9(0004) COMP.
           05  ASTAT11F PIC  X(0001).
           05  FILLER REDEFINES ASTAT11F.
               10  ASTAT11A PIC  X(0001).
           05  ASTAT11I PIC  X(0006).
      *    -------------------------------
           05  ALETR11L PIC S9(0004) COMP.
           05  ALETR11F PIC  X(0001).
           05  FILLER REDEFINES ALETR11F.
               10  ALETR11A PIC  X(0001).
           05  ALETR11I PIC  X(0004).
      *    -------------------------------
           05  AREST11L PIC S9(0004) COMP.
           05  AREST11F PIC  X(0001).
           05  FILLER REDEFINES AREST11F.
               10  AREST11A PIC  X(0001).
           05  AREST11I PIC  X(0001).
      *    -------------------------------
           05  ACLAM12L PIC S9(0004) COMP.
           05  ACLAM12F PIC  X(0001).
           05  FILLER REDEFINES ACLAM12F.
               10  ACLAM12A PIC  X(0001).
           05  ACLAM12I PIC  X(0007).
      *    -------------------------------
           05  ACARR12L PIC S9(0004) COMP.
           05  ACARR12F PIC  X(0001).
           05  FILLER REDEFINES ACARR12F.
               10  ACARR12A PIC  X(0001).
           05  ACARR12I PIC  X(0001).
      *    -------------------------------
           05  ACERT12L PIC S9(0004) COMP.
           05  ACERT12F PIC  X(0001).
           05  FILLER REDEFINES ACERT12F.
               10  ACERT12A PIC  X(0001).
           05  ACERT12I PIC  X(0011).
      *    -------------------------------
           05  APMTS12L PIC S9(0004) COMP.
           05  APMTS12F PIC  X(0001).
           05  FILLER REDEFINES APMTS12F.
               10  APMTS12A PIC  X(0001).
           05  APMTS12I PIC  X(0003).
      *    -------------------------------
           05  APCNT12L PIC S9(0004) COMP.
           05  APCNT12F PIC  X(0001).
           05  FILLER REDEFINES APCNT12F.
               10  APCNT12A PIC  X(0001).
           05  APCNT12I PIC  X(0001).
      *    -------------------------------
           05  AUCNT12L PIC S9(0004) COMP.
           05  AUCNT12F PIC  X(0001).
           05  FILLER REDEFINES AUCNT12F.
               10  AUCNT12A PIC  X(0001).
           05  AUCNT12I PIC  X(0001).
      *    -------------------------------
           05  ASTAT12L PIC S9(0004) COMP.
           05  ASTAT12F PIC  X(0001).
           05  FILLER REDEFINES ASTAT12F.
               10  ASTAT12A PIC  X(0001).
           05  ASTAT12I PIC  X(0006).
      *    -------------------------------
           05  ALETR12L PIC S9(0004) COMP.
           05  ALETR12F PIC  X(0001).
           05  FILLER REDEFINES ALETR12F.
               10  ALETR12A PIC  X(0001).
           05  ALETR12I PIC  X(0004).
      *    -------------------------------
           05  AREST12L PIC S9(0004) COMP.
           05  AREST12F PIC  X(0001).
           05  FILLER REDEFINES AREST12F.
               10  AREST12A PIC  X(0001).
           05  AREST12I PIC  X(0001).
      *    -------------------------------
           05  ACLAM13L PIC S9(0004) COMP.
           05  ACLAM13F PIC  X(0001).
           05  FILLER REDEFINES ACLAM13F.
               10  ACLAM13A PIC  X(0001).
           05  ACLAM13I PIC  X(0007).
      *    -------------------------------
           05  ACARR13L PIC S9(0004) COMP.
           05  ACARR13F PIC  X(0001).
           05  FILLER REDEFINES ACARR13F.
               10  ACARR13A PIC  X(0001).
           05  ACARR13I PIC  X(0001).
      *    -------------------------------
           05  ACERT13L PIC S9(0004) COMP.
           05  ACERT13F PIC  X(0001).
           05  FILLER REDEFINES ACERT13F.
               10  ACERT13A PIC  X(0001).
           05  ACERT13I PIC  X(0011).
      *    -------------------------------
           05  APMTS13L PIC S9(0004) COMP.
           05  APMTS13F PIC  X(0001).
           05  FILLER REDEFINES APMTS13F.
               10  APMTS13A PIC  X(0001).
           05  APMTS13I PIC  X(0003).
      *    -------------------------------
           05  APCNT13L PIC S9(0004) COMP.
           05  APCNT13F PIC  X(0001).
           05  FILLER REDEFINES APCNT13F.
               10  APCNT13A PIC  X(0001).
           05  APCNT13I PIC  X(0001).
      *    -------------------------------
           05  AUCNT13L PIC S9(0004) COMP.
           05  AUCNT13F PIC  X(0001).
           05  FILLER REDEFINES AUCNT13F.
               10  AUCNT13A PIC  X(0001).
           05  AUCNT13I PIC  X(0001).
      *    -------------------------------
           05  ASTAT13L PIC S9(0004) COMP.
           05  ASTAT13F PIC  X(0001).
           05  FILLER REDEFINES ASTAT13F.
               10  ASTAT13A PIC  X(0001).
           05  ASTAT13I PIC  X(0006).
      *    -------------------------------
           05  ALETR13L PIC S9(0004) COMP.
           05  ALETR13F PIC  X(0001).
           05  FILLER REDEFINES ALETR13F.
               10  ALETR13A PIC  X(0001).
           05  ALETR13I PIC  X(0004).
      *    -------------------------------
           05  AREST13L PIC S9(0004) COMP.
           05  AREST13F PIC  X(0001).
           05  FILLER REDEFINES AREST13F.
               10  AREST13A PIC  X(0001).
           05  AREST13I PIC  X(0001).
      *    -------------------------------
           05  ACLAM14L PIC S9(0004) COMP.
           05  ACLAM14F PIC  X(0001).
           05  FILLER REDEFINES ACLAM14F.
               10  ACLAM14A PIC  X(0001).
           05  ACLAM14I PIC  X(0007).
      *    -------------------------------
           05  ACARR14L PIC S9(0004) COMP.
           05  ACARR14F PIC  X(0001).
           05  FILLER REDEFINES ACARR14F.
               10  ACARR14A PIC  X(0001).
           05  ACARR14I PIC  X(0001).
      *    -------------------------------
           05  ACERT14L PIC S9(0004) COMP.
           05  ACERT14F PIC  X(0001).
           05  FILLER REDEFINES ACERT14F.
               10  ACERT14A PIC  X(0001).
           05  ACERT14I PIC  X(0011).
      *    -------------------------------
           05  APMTS14L PIC S9(0004) COMP.
           05  APMTS14F PIC  X(0001).
           05  FILLER REDEFINES APMTS14F.
               10  APMTS14A PIC  X(0001).
           05  APMTS14I PIC  X(0003).
      *    -------------------------------
           05  APCNT14L PIC S9(0004) COMP.
           05  APCNT14F PIC  X(0001).
           05  FILLER REDEFINES APCNT14F.
               10  APCNT14A PIC  X(0001).
           05  APCNT14I PIC  X(0001).
      *    -------------------------------
           05  AUCNT14L PIC S9(0004) COMP.
           05  AUCNT14F PIC  X(0001).
           05  FILLER REDEFINES AUCNT14F.
               10  AUCNT14A PIC  X(0001).
           05  AUCNT14I PIC  X(0001).
      *    -------------------------------
           05  ASTAT14L PIC S9(0004) COMP.
           05  ASTAT14F PIC  X(0001).
           05  FILLER REDEFINES ASTAT14F.
               10  ASTAT14A PIC  X(0001).
           05  ASTAT14I PIC  X(0006).
      *    -------------------------------
           05  ALETR14L PIC S9(0004) COMP.
           05  ALETR14F PIC  X(0001).
           05  FILLER REDEFINES ALETR14F.
               10  ALETR14A PIC  X(0001).
           05  ALETR14I PIC  X(0004).
      *    -------------------------------
           05  AREST14L PIC S9(0004) COMP.
           05  AREST14F PIC  X(0001).
           05  FILLER REDEFINES AREST14F.
               10  AREST14A PIC  X(0001).
           05  AREST14I PIC  X(0001).
      *    -------------------------------
           05  ACLAM15L PIC S9(0004) COMP.
           05  ACLAM15F PIC  X(0001).
           05  FILLER REDEFINES ACLAM15F.
               10  ACLAM15A PIC  X(0001).
           05  ACLAM15I PIC  X(0007).
      *    -------------------------------
           05  ACARR15L PIC S9(0004) COMP.
           05  ACARR15F PIC  X(0001).
           05  FILLER REDEFINES ACARR15F.
               10  ACARR15A PIC  X(0001).
           05  ACARR15I PIC  X(0001).
      *    -------------------------------
           05  ACERT15L PIC S9(0004) COMP.
           05  ACERT15F PIC  X(0001).
           05  FILLER REDEFINES ACERT15F.
               10  ACERT15A PIC  X(0001).
           05  ACERT15I PIC  X(0011).
      *    -------------------------------
           05  APMTS15L PIC S9(0004) COMP.
           05  APMTS15F PIC  X(0001).
           05  FILLER REDEFINES APMTS15F.
               10  APMTS15A PIC  X(0001).
           05  APMTS15I PIC  X(0003).
      *    -------------------------------
           05  APCNT15L PIC S9(0004) COMP.
           05  APCNT15F PIC  X(0001).
           05  FILLER REDEFINES APCNT15F.
               10  APCNT15A PIC  X(0001).
           05  APCNT15I PIC  X(0001).
      *    -------------------------------
           05  AUCNT15L PIC S9(0004) COMP.
           05  AUCNT15F PIC  X(0001).
           05  FILLER REDEFINES AUCNT15F.
               10  AUCNT15A PIC  X(0001).
           05  AUCNT15I PIC  X(0001).
      *    -------------------------------
           05  ASTAT15L PIC S9(0004) COMP.
           05  ASTAT15F PIC  X(0001).
           05  FILLER REDEFINES ASTAT15F.
               10  ASTAT15A PIC  X(0001).
           05  ASTAT15I PIC  X(0006).
      *    -------------------------------
           05  ALETR15L PIC S9(0004) COMP.
           05  ALETR15F PIC  X(0001).
           05  FILLER REDEFINES ALETR15F.
               10  ALETR15A PIC  X(0001).
           05  ALETR15I PIC  X(0004).
      *    -------------------------------
           05  AREST15L PIC S9(0004) COMP.
           05  AREST15F PIC  X(0001).
           05  FILLER REDEFINES AREST15F.
               10  AREST15A PIC  X(0001).
           05  AREST15I PIC  X(0001).
      *    -------------------------------
           05  ACLAM16L PIC S9(0004) COMP.
           05  ACLAM16F PIC  X(0001).
           05  FILLER REDEFINES ACLAM16F.
               10  ACLAM16A PIC  X(0001).
           05  ACLAM16I PIC  X(0007).
      *    -------------------------------
           05  ACARR16L PIC S9(0004) COMP.
           05  ACARR16F PIC  X(0001).
           05  FILLER REDEFINES ACARR16F.
               10  ACARR16A PIC  X(0001).
           05  ACARR16I PIC  X(0001).
      *    -------------------------------
           05  ACERT16L PIC S9(0004) COMP.
           05  ACERT16F PIC  X(0001).
           05  FILLER REDEFINES ACERT16F.
               10  ACERT16A PIC  X(0001).
           05  ACERT16I PIC  X(0011).
      *    -------------------------------
           05  APMTS16L PIC S9(0004) COMP.
           05  APMTS16F PIC  X(0001).
           05  FILLER REDEFINES APMTS16F.
               10  APMTS16A PIC  X(0001).
           05  APMTS16I PIC  X(0003).
      *    -------------------------------
           05  APCNT16L PIC S9(0004) COMP.
           05  APCNT16F PIC  X(0001).
           05  FILLER REDEFINES APCNT16F.
               10  APCNT16A PIC  X(0001).
           05  APCNT16I PIC  X(0001).
      *    -------------------------------
           05  AUCNT16L PIC S9(0004) COMP.
           05  AUCNT16F PIC  X(0001).
           05  FILLER REDEFINES AUCNT16F.
               10  AUCNT16A PIC  X(0001).
           05  AUCNT16I PIC  X(0001).
      *    -------------------------------
           05  ASTAT16L PIC S9(0004) COMP.
           05  ASTAT16F PIC  X(0001).
           05  FILLER REDEFINES ASTAT16F.
               10  ASTAT16A PIC  X(0001).
           05  ASTAT16I PIC  X(0006).
      *    -------------------------------
           05  ALETR16L PIC S9(0004) COMP.
           05  ALETR16F PIC  X(0001).
           05  FILLER REDEFINES ALETR16F.
               10  ALETR16A PIC  X(0001).
           05  ALETR16I PIC  X(0004).
      *    -------------------------------
           05  AREST16L PIC S9(0004) COMP.
           05  AREST16F PIC  X(0001).
           05  FILLER REDEFINES AREST16F.
               10  AREST16A PIC  X(0001).
           05  AREST16I PIC  X(0001).
      *    -------------------------------
           05  MSG1L PIC S9(0004) COMP.
           05  MSG1F PIC  X(0001).
           05  FILLER REDEFINES MSG1F.
               10  MSG1A PIC  X(0001).
           05  MSG1I PIC  X(0079).
      *    -------------------------------
           05  PFKEYL PIC S9(0004) COMP.
           05  PFKEYF PIC  X(0001).
           05  FILLER REDEFINES PFKEYF.
               10  PFKEYA PIC  X(0001).
           05  PFKEYI PIC  9(2).
      *    -------------------------------
           05  PFKEY3L PIC S9(0004) COMP.
           05  PFKEY3F PIC  X(0001).
           05  FILLER REDEFINES PFKEY3F.
               10  PFKEY3A PIC  X(0001).
           05  PFKEY3I PIC  X(0017).
       01  EL144AO REDEFINES EL144AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLAIMO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM01O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT01O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APMTS01O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APCNT01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUCNT01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTAT01O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALETR01O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREST01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM02O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT02O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APMTS02O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APCNT02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUCNT02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTAT02O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALETR02O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREST02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM03O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT03O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APMTS03O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APCNT03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUCNT03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTAT03O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALETR03O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREST03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM04O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT04O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APMTS04O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APCNT04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUCNT04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTAT04O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALETR04O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREST04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM05O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT05O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APMTS05O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APCNT05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUCNT05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTAT05O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALETR05O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREST05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM06O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT06O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APMTS06O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APCNT06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUCNT06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTAT06O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALETR06O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREST06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM07O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT07O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APMTS07O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APCNT07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUCNT07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTAT07O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALETR07O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREST07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM08O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT08O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APMTS08O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APCNT08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUCNT08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTAT08O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALETR08O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREST08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM09O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT09O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APMTS09O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APCNT09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUCNT09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTAT09O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALETR09O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREST09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM10O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT10O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APMTS10O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APCNT10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUCNT10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTAT10O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALETR10O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREST10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM11O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT11O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APMTS11O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APCNT11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUCNT11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTAT11O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALETR11O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREST11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM12O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT12O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APMTS12O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APCNT12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUCNT12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTAT12O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALETR12O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREST12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM13O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT13O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APMTS13O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APCNT13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUCNT13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTAT13O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALETR13O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREST13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM14O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT14O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APMTS14O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APCNT14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUCNT14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTAT14O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALETR14O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREST14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM15O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT15O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APMTS15O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APCNT15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUCNT15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTAT15O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALETR15O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREST15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM16O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT16O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APMTS16O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APCNT16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUCNT16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTAT16O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALETR16O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREST16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEY3O PIC  X(0017).
      *    -------------------------------
       01  EL144BI REDEFINES EL144AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  BDATEL PIC S9(0004) COMP.
           05  BDATEF PIC  X(0001).
           05  FILLER REDEFINES BDATEF.
               10  BDATEA PIC  X(0001).
           05  BDATEI PIC  X(0008).
      *    -------------------------------
           05  BTIMEL PIC S9(0004) COMP.
           05  BTIMEF PIC  X(0001).
           05  FILLER REDEFINES BTIMEF.
               10  BTIMEA PIC  X(0001).
           05  BTIMEI PIC  X(0005).
      *    -------------------------------
           05  BCARRL PIC S9(0004) COMP.
           05  BCARRF PIC  X(0001).
           05  FILLER REDEFINES BCARRF.
               10  BCARRA PIC  X(0001).
           05  BCARRI PIC  X(0001).
      *    -------------------------------
           05  BCLAIML PIC S9(0004) COMP.
           05  BCLAIMF PIC  X(0001).
           05  FILLER REDEFINES BCLAIMF.
               10  BCLAIMA PIC  X(0001).
           05  BCLAIMI PIC  X(0007).
      *    -------------------------------
           05  BCERTL PIC S9(0004) COMP.
           05  BCERTF PIC  X(0001).
           05  FILLER REDEFINES BCERTF.
               10  BCERTA PIC  X(0001).
           05  BCERTI PIC  X(0010).
      *    -------------------------------
           05  BSFXL PIC S9(0004) COMP.
           05  BSFXF PIC  X(0001).
           05  FILLER REDEFINES BSFXF.
               10  BSFXA PIC  X(0001).
           05  BSFXI PIC  X(0001).
      *    -------------------------------
           05  BTYPEL PIC S9(0004) COMP.
           05  BTYPEF PIC  X(0001).
           05  FILLER REDEFINES BTYPEF.
               10  BTYPEA PIC  X(0001).
           05  BTYPEI PIC  X(0001).
      *    -------------------------------
           05  BPCNTL PIC S9(0004) COMP.
           05  BPCNTF PIC  X(0001).
           05  FILLER REDEFINES BPCNTF.
               10  BPCNTA PIC  X(0001).
           05  BPCNTI PIC  X(0001).
      *    -------------------------------
           05  BPUCNTL PIC S9(0004) COMP.
           05  BPUCNTF PIC  X(0001).
           05  FILLER REDEFINES BPUCNTF.
               10  BPUCNTA PIC  X(0001).
           05  BPUCNTI PIC  X(0001).
      *    -------------------------------
           05  BLETRL PIC S9(0004) COMP.
           05  BLETRF PIC  X(0001).
           05  FILLER REDEFINES BLETRF.
               10  BLETRA PIC  X(0001).
           05  BLETRI PIC  X(0004).
      *    -------------------------------
           05  BSENDL PIC S9(0004) COMP.
           05  BSENDF PIC  X(0001).
           05  FILLER REDEFINES BSENDF.
               10  BSENDA PIC  X(0001).
           05  BSENDI PIC  X(0008).
      *    -------------------------------
           05  BFOLLOWL PIC S9(0004) COMP.
           05  BFOLLOWF PIC  X(0001).
           05  FILLER REDEFINES BFOLLOWF.
               10  BFOLLOWA PIC  X(0001).
           05  BFOLLOWI PIC  X(0008).
      *    -------------------------------
           05  BMSG1L PIC S9(0004) COMP.
           05  BMSG1F PIC  X(0001).
           05  FILLER REDEFINES BMSG1F.
               10  BMSG1A PIC  X(0001).
           05  BMSG1I PIC  X(0079).
       01  EL144BO REDEFINES EL144AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAIMO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPCNTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPUCNTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BLETRO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSENDO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BFOLLOWO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BMSG1O PIC  X(0079).
      *    -------------------------------
       01  EL144CI REDEFINES EL144AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  CDATEL PIC S9(0004) COMP.
           05  CDATEF PIC  X(0001).
           05  FILLER REDEFINES CDATEF.
               10  CDATEA PIC  X(0001).
           05  CDATEI PIC  X(0008).
      *    -------------------------------
           05  CTIMEL PIC S9(0004) COMP.
           05  CTIMEF PIC  X(0001).
           05  FILLER REDEFINES CTIMEF.
               10  CTIMEA PIC  X(0001).
           05  CTIMEI PIC  X(0005).
      *    -------------------------------
           05  CCARRL PIC S9(0004) COMP.
           05  CCARRF PIC  X(0001).
           05  FILLER REDEFINES CCARRF.
               10  CCARRA PIC  X(0001).
           05  CCARRI PIC  X(0001).
      *    -------------------------------
           05  CCLAIML PIC S9(0004) COMP.
           05  CCLAIMF PIC  X(0001).
           05  FILLER REDEFINES CCLAIMF.
               10  CCLAIMA PIC  X(0001).
           05  CCLAIMI PIC  X(0007).
      *    -------------------------------
           05  CCERTL PIC S9(0004) COMP.
           05  CCERTF PIC  X(0001).
           05  FILLER REDEFINES CCERTF.
               10  CCERTA PIC  X(0001).
           05  CCERTI PIC  X(0010).
      *    -------------------------------
           05  CSFXL PIC S9(0004) COMP.
           05  CSFXF PIC  X(0001).
           05  FILLER REDEFINES CSFXF.
               10  CSFXA PIC  X(0001).
           05  CSFXI PIC  X(0001).
      *    -------------------------------
           05  CRTYPL PIC S9(0004) COMP.
           05  CRTYPF PIC  X(0001).
           05  FILLER REDEFINES CRTYPF.
               10  CRTYPA PIC  X(0001).
           05  CRTYPI PIC  X(0001).
      *    -------------------------------
           05  CMSG1L PIC S9(0004) COMP.
           05  CMSG1F PIC  X(0001).
           05  FILLER REDEFINES CMSG1F.
               10  CMSG1A PIC  X(0001).
           05  CMSG1I PIC  X(0079).
       01  EL144CO REDEFINES EL144AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCLAIMO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCERTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMSG1O PIC  X(0079).
      *    -------------------------------
00133
00134  01  FILLER                          REDEFINES
00135      EL144AI.
00136
00137      12  FILLER                      PIC X(62).
00138
00139      12  EL144A-SCREEN.
00140          16  EL144A-LINES                OCCURS 16 TIMES
00141                                          INDEXED BY EL144A-INDEX
00142                                                     EL144A-INDEXB.
00143
00144              20  EL144A-CLAIM-LENGTH     PIC S9(4)
00145                                          COMP.
00146              20  EL144A-CLAIM-ATTRB      PIC X.
00147              20  EL144A-CLAIM            PIC X(7).
00148
00149              20  EL144A-CARRIER-LENGTH   PIC S9(4)
00150                                      COMP.
00151              20  EL144A-CARRIER-ATTRB    PIC X.
00152              20  EL144A-CARRIER          PIC X.
00153
00154              20  EL144A-CERT-NO-LENGTH   PIC S9(4)
00155                                          COMP.
00156              20  EL144A-CERT-NO-ATTRB    PIC X.
00157              20  EL144A-CERT-NO          PIC X(11).
00158
00159              20  EL144A-PMT-LENGTH       PIC S9(4)
00160                                          COMP.
00161              20  EL144A-PMT-ATTRB        PIC X.
00162              20  EL144A-PMT              PIC X(3).
00163
00164              20  EL144A-PCNT-LENGTH      PIC S9(4)
00165                                          COMP.
00166              20  EL144A-PCNT-ATTRB       PIC X.
00167              20  EL144A-PCNT             PIC 9(01).
00168
00169              20  EL144A-PUCNT-LENGTH     PIC S9(4)
00170                                          COMP.
00171              20  EL144A-PUCNT-ATTRB      PIC X.
00172              20  EL144A-PUCNT            PIC 9(01).
00173
00174              20  EL144A-STATUS-LENGTH    PIC S9(4)
00175                                          COMP.
00176              20  EL144A-STATUS-ATTRB     PIC X.
00177              20  EL144A-STATUS           PIC X(06).
00178              20  EL144A-LETR-LENGTH      PIC S9(4)
00179                                          COMP.
00180              20  EL144A-LETR-ATTRB       PIC X.
00181              20  EL144A-LETR             PIC X(04).
00182              20  EL144A-REST-LENGTH      PIC S9(4)
00183                                          COMP.
00184              20  EL144A-REST-ATTRB       PIC X.
00185              20  EL144A-REST             PIC X.
00186
00187
00188      EJECT
00189 *    COPY ELCEMIB.
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
00190
00191      EJECT
00192 *    COPY ELCDATE.
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
00193
00194      EJECT
00195 *    COPY ELCLOGOF.
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
00196
00197      EJECT
00198 *    COPY ELCATTR.
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
00199
00200      EJECT
00201 *    COPY ELCAID.
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
00202
00203  01  FILLER                      REDEFINES
00204      DFHAID.
00205
00206      12  FILLER                      PIC X(8).
00207
00208      12  PF-VALUES                   PIC X
00209          OCCURS 24 TIMES.
00210      EJECT
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
00212
00213  01  DFHCOMMAREA                     PIC X(1024).
00214
00215 *01 DFHBLLDS                         COMP SYNC.
00216 *    12  BLLCBAR                     PIC S9(9).
00217 *    12  ELACTQ-BLL                  PIC S9(9).
00218 *    12  ELMSTR-BLL                  PIC S9(9).
00219      EJECT
00220 *    COPY ELCACTQ.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCACTQ.                            *
00004 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.004                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ACTIVITY QUE FILE                         *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 60     RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELACTQ             RKP=2,LEN=20          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCACTQ                          *
00017 ******************************************************************
00018
00019  01  ACTIVITY-QUE.
00020      12  AQ-RECORD-ID                PIC XX.
00021          88  VALID-AQ-ID                VALUE 'AQ'.
00022
00023      12  AQ-CONTROL-PRIMARY.
00024          16  AQ-COMPANY-CD           PIC X.
00025          16  AQ-CARRIER              PIC X.
00026          16  AQ-CLAIM-NO             PIC X(7).
00027          16  AQ-CERT-NO.
00028              20  AQ-CERT-PRIME       PIC X(10).
00029              20  AQ-CERT-SFX         PIC X.
00030
00031      12  AQ-PENDING-ACTIVITY-FLAGS.
00032          88  NO-PENDING-ACTIVITY        VALUE SPACES.
00033          16  AQ-PENDING-PAYMENT-FLAG PIC X.
00034              88  PENDING-PAYMENTS       VALUE '1'.
00035          16  AQ-PENDING-STATUS-FLAG  PIC X.
00036              88  PENDING-FULL-PRINT     VALUE '1'.
00037              88  PENDING-PART-PRINT     VALUE '2'.
00038          16  AQ-PENDING-LETTER-FLAG  PIC X.
00039              88  PENDING-LETTERS        VALUE '1'.
00040          16  AQ-PENDING-CLAIM-RESTORE PIC X.
00041              88  PENDING-RESTORE        VALUE 'C'.
00042              88  PENDING-RESTORE-LETTER VALUE 'L'.
00043
00044      12  FILLER                      PIC X(20).
00045
00046      12  AQ-RESEND-DATE              PIC XX.
00047      12  AQ-FOLLOWUP-DATE            PIC XX.
00048      12  AQ-PAYMENT-COUNTER          PIC S9        COMP-3.
00049      12  AQ-PMT-UNAPPROVED-COUNT     PIC S9        COMP-3.
00050      12  AQ-AUTO-LETTER              PIC X(4).
00051      12  FILLER                      PIC XX.
00052      12  AQ-LAST-UPDATED-BY          PIC S9(4)     COMP.
00053 *****************************************************************
00221
00222      EJECT
00223 *    COPY ELCMSTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCMSTR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.012                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CLAIM MASTER FILE                         *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 350  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELMSTR                         RKP=2,LEN=20   *
00013 *       ALTERNATE PATH1 = ELMSTR2 (BY NAME)       RKP=22,LEN=29  *
00014 *       ALTERNATE PATH2 = ELMSTR3 (BY SOC SEC NO) RKP=51,LEN=12  *
00015 *       ALTERNATE PATH3 = ELMSTR5 (BY CERT NO)    RKP=63,LEN=12  *
00016 *       ALTERNATE PATH4 = ELMSTR6 (BY CREDIT CARD NO)            *
00017 *                                                 RKP=75,LEN=21  *
00018 *                                                                *
00019 *   **** NOTE ****                                               *
00020 *             ANY CHANGES TO THIS COPYBOOK MUST ALSO BE          *
00021 *             IMPLEMENTED IN COPYBOOK ELCRETR (RETRIEVE MASTER)  *
00022 *                                                                *
00023 *   LOG = YES                                                    *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
120503******************************************************************
120503*                   C H A N G E   L O G
120503*
120503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120503*-----------------------------------------------------------------
120503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120503* EFFECTIVE    NUMBER
120503*-----------------------------------------------------------------
120503* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
080307* 080307    2007032100001  PEMA  ADD TOTAL INTEREST PAID FIELD
00025 ******************************************************************
00026  01  CLAIM-MASTER.
00027      12  CL-RECORD-ID                PIC XX.
00028          88  VALID-CL-ID         VALUE 'CL'.
00029
00030      12  CL-CONTROL-PRIMARY.
00031          16  CL-COMPANY-CD           PIC X.
00032          16  CL-CARRIER              PIC X.
00033          16  CL-CLAIM-NO             PIC X(7).
00034          16  CL-CERT-NO.
00035              20  CL-CERT-PRIME       PIC X(10).
00036              20  CL-CERT-SFX         PIC X.
00037
00038      12  CL-CONTROL-BY-NAME.
00039          16  CL-COMPANY-CD-A1        PIC X.
00040          16  CL-INSURED-LAST-NAME    PIC X(15).
00041          16  CL-INSURED-NAME.
00042              20  CL-INSURED-1ST-NAME PIC X(12).
00043              20  CL-INSURED-MID-INIT PIC X.
00044
00045      12  CL-CONTROL-BY-SSN.
00046          16  CL-COMPANY-CD-A2        PIC X.
00047          16  CL-SOC-SEC-NO.
00048              20  CL-SSN-STATE        PIC XX.
00049              20  CL-SSN-ACCOUNT      PIC X(6).
00050              20  CL-SSN-LN3          PIC X(3).
00051
00052      12  CL-CONTROL-BY-CERT-NO.
00053          16  CL-COMPANY-CD-A4        PIC X.
00054          16  CL-CERT-NO-A4.
00055              20  CL-CERT-A4-PRIME    PIC X(10).
00056              20  CL-CERT-A4-SFX      PIC X.
00057
00058      12  CL-CONTROL-BY-CCN.
00059          16  CL-COMPANY-CD-A5        PIC X.
00060          16  CL-CCN-A5.
00061              20  CL-CCN.
00062                  24  CL-CCN-PREFIX-A5 PIC X(4).
00063                  24  CL-CCN-PRIME-A5 PIC X(12).
00064              20  CL-CCN-FILLER-A5    PIC X(4).
00065
00066      12  CL-INSURED-PROFILE-DATA.
00067          16  CL-INSURED-BIRTH-DT     PIC XX.
00068          16  CL-INSURED-SEX-CD       PIC X.
00069              88  INSURED-IS-MALE        VALUE 'M'.
00070              88  INSURED-IS-FEMALE      VALUE 'F'.
00071              88  INSURED-SEX-UNKNOWN    VALUE ' '.
00072          16  CL-INSURED-OCC-CD       PIC X(6).
00073          16  FILLER                  PIC X(5).
00074
00075      12  CL-PROCESSING-INFO.
00076          16  CL-PROCESSOR-ID         PIC X(4).
00077          16  CL-CLAIM-STATUS         PIC X.
00078              88  CLAIM-IS-OPEN          VALUE 'O'.
00079              88  CLAIM-IS-CLOSED        VALUE 'C'.
00080          16  CL-CLAIM-TYPE           PIC X.
00081 *            88  AH-CLAIM               VALUE 'A'.
00082 *            88  LIFE-CLAIM             VALUE 'L'.
00083 *            88  PROPERTY-CLAIM         VALUE 'P'.
00084 *            88  IUI-CLAIM              VALUE 'I'.
120503*            88  GAP-CLAIM              VALUE 'G'.
00085          16  CL-CLAIM-PREM-TYPE      PIC X.
00086              88  SINGLE-PREMIUM         VALUE '1'.
00087              88  O-B-COVERAGE           VALUE '2'.
00088              88  OPEN-END-COVERAGE      VALUE '3'.
00089          16  CL-INCURRED-DT          PIC XX.
00090          16  CL-REPORTED-DT          PIC XX.
00091          16  CL-FILE-ESTABLISH-DT    PIC XX.
00092          16  CL-EST-END-OF-DISAB-DT  PIC XX.
00093          16  CL-LAST-PMT-DT          PIC XX.
00094          16  CL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
00095          16  CL-PAID-THRU-DT         PIC XX.
00096          16  CL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
00097          16  CL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
00098          16  CL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
00099          16  CL-PMT-CALC-METHOD      PIC X.
00100              88  CL-360-DAY-YR          VALUE '1'.
00101              88  CL-365-DAY-YR          VALUE '2'.
00102              88  CL-FULL-MONTHS         VALUE '3'.
00103          16  CL-CAUSE-CD             PIC X(6).
00104
00105          16  CL-PRIME-CERT-NO.
00106              20  CL-PRIME-CERT-PRIME PIC X(10).
00107              20  CL-PRIME-CERT-SFX   PIC X.
00108
00109          16  CL-SYSTEM-IDENTIFIER    PIC XX.
00110              88  CL-CREDIT-CLAIM        VALUE 'CR'.
00111              88  CL-CONVENIENCE-CLAIM   VALUE 'CV'.
00112
00113          16  CL-MICROFILM-NO         PIC X(10).
00114          16  CL-PROG-FORM-TYPE       PIC X.
00115          16  CL-LAST-ADD-ON-DT       PIC XX.
00116
00117          16  CL-LAST-REOPEN-DT       PIC XX.
00118          16  CL-LAST-CLOSE-DT        PIC XX.
00119          16  CL-LAST-CLOSE-REASON    PIC X(01).
00120              88  FINAL-PAID             VALUE '1'.
00121              88  CLAIM-DENIED           VALUE '2'.
00122              88  AUTO-CLOSE             VALUE '3'.
00123              88  MANUAL-CLOSE           VALUE '4'.
00124              88  BENEFITS-CHANGED       VALUE 'C'.
00125              88  SETUP-ERRORS           VALUE 'E'.
00126          16  CL-ASSOC-CERT-SEQU      PIC S99.
00127          16  CL-ASSOC-CERT-TOTAL     PIC S99.
00128          16  CL-CLAIM-PAYMENT-STATUS PIC 9.
00129              88  PAYMENT-IN-PREP        VALUE 1 THRU 9.
080307         16  CL-TOTAL-INT-PAID       PIC S9(5)V99 COMP-3.
080307         16  FILLER                  PIC X.
00131
00132      12  CL-CERTIFICATE-DATA.
00133          16  CL-CERT-ORIGIN          PIC X.
00134              88  CERT-WAS-ONLINE        VALUE '1'.
00135              88  CERT-WAS-CREATED       VALUE '2'.
00136              88  COVERAGE-WAS-ADDED     VALUE '3'.
00137          16  CL-CERT-KEY-DATA.
00138              20  CL-CERT-CARRIER     PIC X.
00139              20  CL-CERT-GROUPING    PIC X(6).
00140              20  CL-CERT-STATE       PIC XX.
00141              20  CL-CERT-ACCOUNT.
00142                  24  CL-CERT-ACCOUNT-PREFIX PIC X(4).
00143                  24  CL-CERT-ACCOUNT-PRIME  PIC X(6).
00144              20  CL-CERT-EFF-DT      PIC XX.
00145
00146      12  CL-STATUS-CONTROLS.
00147          16  CL-PRIORITY-CD          PIC X.
00148              88  CONFIDENTIAL-DATA      VALUE '8'.
00149              88  HIGHEST-PRIORITY       VALUE '9'.
00150          16  CL-SUPV-ATTN-CD         PIC X.
00151              88  SUPV-NOT-REQUIRED      VALUE ' ' 'N'.
00152              88  SUPV-IS-REQUIRED       VALUE 'Y'.
00153          16  CL-PURGED-DT            PIC XX.
00154          16  CL-RESTORED-DT          PIC XX.
00155          16  CL-NEXT-AUTO-PAY-DT     PIC XX.
00156          16  CL-NEXT-RESEND-DT       PIC XX.
00157          16  CL-NEXT-FOLLOWUP-DT     PIC XX.
00158          16  FILLER                  PIC XX.
00159          16  CL-LAST-MAINT-DT        PIC XX.
00160          16  CL-LAST-MAINT-USER      PIC X(4).
00161          16  CL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
00162          16  CL-LAST-MAINT-TYPE      PIC X.
00163              88  CLAIM-SET-UP           VALUE ' '.
00164              88  PAYMENT-MADE           VALUE '1'.
00165              88  LETTER-SENT            VALUE '2'.
00166              88  MASTER-WAS-ALTERED     VALUE '3'.
00167              88  MASTER-WAS-RESTORED    VALUE '4'.
00168              88  INCURRED-DATE-CHANGED  VALUE '5'.
00169              88  FILE-CONVERTED         VALUE '6'.
00170              88  CHANGE-OF-BENEFITS     VALUE 'C'.
00171              88  ERROR-CORRECTION       VALUE 'E'.
00172          16  CL-RELATED-CLAIM-NO     PIC X(7).
00173          16  CL-HISTORY-ARCHIVE-DT   PIC XX.
00174          16  CL-BENEFICIARY          PIC X(10).
00175          16  CL-FILE-ESTABLISHED-BY  PIC X(4).
120808         16  CL-DENIAL-TYPE          PIC X.
                   88  CL-TYPE-DENIAL          VALUE '1'.
                   88  CL-TYPE-RESCISSION      VALUE '2'.
                   88  CL-TYPE-REFORMATION     VALUE '3'.
                   88  CL-TYPE-REF-TO-RES      VALUE '4'.
                   88  CL-TYPE-RECONSIDERED    VALUE '5'.
120808         16  FILLER                  PIC X(5).
00177
00178      12  CL-TRAILER-CONTROLS.
00179          16  CL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
00180              88  CL-1ST-TRL-AVAIL       VALUE +4095.
00181              88  CL-LAST-TRL-AVAIL      VALUE +100.
00182              88  CL-RESV-EXP-HIST-TRLR  VALUE +0.
00183          16  CL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
00184          16  FILLER                  PIC XX.
00185          16  CL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
00186          16  CL-ADDRESS-TRAILER-CNT.
00187              20  CL-INSURED-ADDR-CNT  PIC S9(1).
00188                  88  NO-INSURED-AVAILABLE    VALUE ZERO.
00189              20  CL-ACCOUNT-ADDR-CNT  PIC S9(1).
00190                  88  ACCOUNT-IS-ONLINE       VALUE ZERO.
00191              20  CL-BENIF-ADDR-CNT    PIC S9(1).
00192                  88  BENEFICIARY-IS-ONLINE   VALUE ZERO.
00193              20  CL-EMPLOYER-ADDR-CNT PIC S9(1).
00194                  88  NO-EMPLOY-AVAILABLE     VALUE ZERO.
00195              20  CL-DOCTOR-ADDR-CNT   PIC S9(1).
00196                  88  NO-DOCTOR-AVAILABLE     VALUE ZERO.
00197              20  CL-OTHER-1-ADDR-CNT  PIC S9(1).
00198                  88  NO-OTHER-1-ADDRESSES    VALUE ZERO.
00199              20  CL-OTHER-2-ADDR-CNT  PIC S9(1).
00200                  88  NO-OTHER-2-ADDRESSES    VALUE ZERO.
00201
00202      12  CL-CV-REFERENCE-NO.
00203          16  CL-CV-REFNO-PRIME       PIC X(18).
00204          16  CL-CV-REFNO-SFX         PIC XX.
00205
00206      12  CL-FILE-LOCATION            PIC X(4).
00207
00208      12  CL-PROCESS-ERRORS.
00209          16  CL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
00210              88  NO-FATAL-ERRORS        VALUE ZERO.
00211          16  CL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
00212              88  NO-FORCABLE-ERRORS     VALUE ZERO.
00213
00214      12  CL-PRODUCT-CD               PIC X.
00215
00216      12  CL-CURRENT-KEY-DATA.
00217          16  CL-CURRENT-CARRIER      PIC X.
00218          16  CL-CURRENT-GROUPING     PIC X(6).
00219          16  CL-CURRENT-STATE        PIC XX.
00220          16  CL-CURRENT-ACCOUNT      PIC X(10).
00221
00222      12  CL-ASSOCIATES               PIC X.
00223          88  CL-ASSOC-NO-INTERFACE      VALUE 'A'.
00224          88  CL-ASSOC-INTERFACE         VALUE 'I'.
00225          88  CL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
00226          88  CL-NON-ASSOC-INTERFACE     VALUE 'M'.
00227
00228      12  CL-ACTIVITY-CODE            PIC 99.
00229      12  CL-ACTIVITY-MAINT-DT        PIC XX.
00230      12  CL-ACTIVITY-MAINT-TYPE      PIC X(4).
00231
00232      12  CL-LAPSE-REPORT-CODE        PIC 9.
00233      12  CL-LAG-REPORT-CODE          PIC 9.
00234      12  CL-LOAN-TYPE                PIC XX.
00235      12  CL-LEGAL-STATE              PIC XX.
00236
CIDMOD*    12  FILLER                      PIC X(5).
CIDMOD     12  CL-YESNOSW                  PIC X.
CIDMOD     12  FILLER                      PIC X(4).
00224
00225      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA ACTIVITY-QUE
                                CLAIM-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL144' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00227
00228      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00229      MOVE '5'                   TO DC-OPTION-CODE.
00230      PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00231      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00232      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00233
00234      MOVE DFHCOMMAREA           TO  PROGRAM-INTERFACE-BLOCK.
00235
00236 *    NOTE *******************************************************
00237 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
00238 *         *  FROM ANOTHER MODULE.                               *
00239 *         *******************************************************.
00240
00241      IF EIBCALEN NOT GREATER THAN ZERO
00242          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00243          GO TO 8300-SEND-TEXT.
00244
00245      
      * EXEC CICS HANDLE CONDITION
00246 *        PGMIDERR (9600-PGMIDERR)
00247 *        ERROR    (9990-ERROR)
00248 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00002943' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303032393433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00249
00250      EJECT
00251  0010-MAIN-LOGIC.
00252
00253      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00254         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00255            MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00256            MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00257            MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00258            MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00259            MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00260            MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00261            MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00262            MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
00263         ELSE
00264            MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00265            MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00266            MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00267            MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00268            MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00269            MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00270            MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00271            MOVE SPACES               TO  PI-SAVED-PROGRAM-6
00272      ELSE
00273         GO TO 0020-MAIN-LOGIC.
00274
00275  0015-MAIN-LOGIC.
00276 *    NOTE *******************************************************
00277 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      *
00278 *         *  INTERFACE BLOCK FOR THIS MODULE.                   *
00279 *         *******************************************************.
00280
00281      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA
00282
00283      MOVE LOW-VALUES             TO  PI-TOP-KEY
00284                                      PI-BOT-KEY
00285      MOVE 'EL144A'               TO  PI-MAP-NAME
00286
00287      IF EIBTRNID NOT EQUAL WS-TRANS-ID
00288         MOVE LOW-VALUES         TO  EL144AI
00289         GO TO 8100-SEND-INITIAL-MAP.
00290
00291      EJECT
00292  0020-MAIN-LOGIC.
00293 *    NOTE *******************************************************
00294 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *
00295 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *
00296 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
00297 *         *******************************************************.
00298
00299      IF EIBAID EQUAL DFHCLEAR
00300         IF PI-MAP-NAME = 'EL144B' OR 'EL144C'
00301            MOVE PI-TOP-KEY TO PI-BOT-KEY
00302                               WS-AQ-CONTROL-PRIMARY
00303            MOVE 'EL144A' TO PI-MAP-NAME
00304            GO TO 1010-BYPASS-PRIME-KEY.
00305
00306      IF EIBAID EQUAL DFHCLEAR
00307          GO TO 9400-CLEAR.
00308
00309      IF EIBAID EQUAL DFHPA1 OR DFHPA2 OR DFHPA3
00310          MOVE LOW-VALUES         TO  EL144AI
00311          MOVE ER-0008            TO  EMI-ERROR
00312          GO TO 8200-SEND-DATAONLY.
00313
00314      IF PI-MAP-NAME EQUAL 'EL144A'
00315         
      * EXEC CICS RECEIVE
00316 *            INTO   (EL144AI)
00317 *            MAPSET (WS-MAPSET-NAME)
00318 *            MAP    (WS-MAP-NAME)
00319 *       END-EXEC
           MOVE LENGTH OF
            EL144AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003013' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL144AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00320      ELSE
00321      IF PI-MAP-NAME = 'EL144B'
00322         
      * EXEC CICS RECEIVE
00323 *            INTO   (EL144AI)
00324 *            MAPSET (WS-MAPSET-NAME)
00325 *            MAP    ('EL144B')
00326 *       END-EXEC
           MOVE LENGTH OF
            EL144AI
             TO DFHEIV11
           MOVE 'EL144B' TO DFHEIV1
      *    MOVE '8"T I  L              ''   #00003020' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL144AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00327         GO TO 5000-PROCESS-EL144B
00328      ELSE
00329         
      * EXEC CICS RECEIVE
00330 *            INTO   (EL144AI)
00331 *            MAPSET (WS-MAPSET-NAME)
00332 *            MAP    ('EL144C')
00333 *       END-EXEC
           MOVE LENGTH OF
            EL144AI
             TO DFHEIV11
           MOVE 'EL144C' TO DFHEIV1
      *    MOVE '8"T I  L              ''   #00003027' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL144AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00334         GO TO 6000-PROCESS-EL144C.
00335
00336      IF PFKEYL GREATER ZERO
00337         IF EIBAID NOT = DFHENTER
00338            MOVE ER-0004        TO  EMI-ERROR
00339            MOVE AL-UNBOF       TO  PFKEYA
00340            MOVE -1             TO  PFKEYL
00341            GO TO 8200-SEND-DATAONLY
00342          ELSE
00343             IF PFKEYO IS NUMERIC
00344               AND PFKEYO IS GREATER THAN ZERO
00345               AND PFKEYO IS LESS THAN '25'
00346                 MOVE PF-VALUES (PFKEYI) TO  EIBAID
00347             ELSE
00348                MOVE ER-0029        TO  EMI-ERROR
00349                MOVE AL-UNBOF       TO  PFKEYA
00350                MOVE -1             TO  PFKEYL
00351                GO TO 8200-SEND-DATAONLY.
00352
00353      IF EIBAID EQUAL DFHPF12
00354         MOVE 'EL010'            TO  XCTL-PGM
00355         GO TO 9300-XCTL.
00356
00357      IF EIBAID EQUAL DFHPF23
00358         GO TO 9000-RETURN-CICS.
00359
00360      IF EIBAID EQUAL DFHPF24
00361         MOVE 'EL126'            TO  XCTL-PGM
00362         GO TO 9300-XCTL.
00363
00364      IF PI-PROCESSOR-ID EQUAL 'LGXX'
00365         NEXT SENTENCE
00366      ELSE
00367         
      * EXEC CICS READQ TS
00368 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00369 *            INTO    (SECURITY-CONTROL)
00370 *            LENGTH  (SC-COMM-LENGTH)
00371 *            ITEM    (SC-ITEM)
00372 *       END-EXEC
      *    MOVE '*$II   L              ''   #00003065' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00373         MOVE SC-CLAIMS-DISPLAY (22)  TO  PI-DISPLAY-CAP
00374         MOVE SC-CLAIMS-UPDATE  (22)  TO  PI-MODIFY-CAP
00375         IF NOT DISPLAY-CAP
00376            MOVE 'READ'              TO  SM-READ
00377            PERFORM 9995-SECURITY-VIOLATION
00378            MOVE ER-0070             TO  EMI-ERROR
00379            GO TO 8100-SEND-INITIAL-MAP.
00380
00381      IF EIBAID = DFHENTER OR DFHPF1 OR DFHPF2 OR DFHPF3 OR
00382                              DFHPF4
00383         NEXT SENTENCE
00384      ELSE
00385         MOVE ER-0008            TO  EMI-ERROR
00386         MOVE -1                 TO  PFKEYL
00387         GO TO 8200-SEND-DATAONLY.
00388
00389      IF EIBAID EQUAL DFHPF3
PEMMOD        IF PI-PROCESSOR-ID NOT EQUAL 'LGXX' AND 'PEMA'
00391            MOVE ER-0008         TO  EMI-ERROR
00392            MOVE -1              TO  PFKEYL
00393            GO TO 8200-SEND-DATAONLY.
00394
00395      IF EIBAID EQUAL DFHPF2
00396         GO TO 2000-START-BROWSE-BWD.
00397
00398      IF EIBAID EQUAL DFHPF3
00399         MOVE 'EL144B' TO PI-MAP-NAME
00400         GO TO 8100-SEND-INITIAL-MAP.
00401
00402      IF EIBAID = DFHPF4
00403         MOVE EIBAID   TO PI-EIBAID-LAST
00404         MOVE LOW-VALUES TO EL144CI
00405         MOVE 'EL144C' TO PI-MAP-NAME
00406         GO TO 8100-SEND-INITIAL-MAP.
00407
00408      EJECT
00409  1000-START-BROWSE-FWD.
00410
00411      MOVE LOW-VALUES       TO PI-TOP-KEY.
00412      MOVE PI-BOT-KEY       TO WS-AQ-CONTROL-PRIMARY.
00413
00414      MOVE PI-COMPANY-CD    TO WS-AQ-COMPANY-CD.
00415
00416      IF CARRL  EQUAL +0 AND
00417         CLAIML EQUAL +0 AND
00418         CERTL  EQUAL +0 AND
00419         SFXL   EQUAL +0
00420         GO TO 1010-BYPASS-PRIME-KEY.
00421
00422      MOVE LOW-VALUES       TO WS-AQ-CONTROL-PRIMARY.
00423      MOVE PI-COMPANY-CD    TO WS-AQ-COMPANY-CD.
00424
00425      IF CARRL  GREATER THAN +0
00426         MOVE CARRI         TO WS-AQ-CARRIER.
00427
00428      IF CLAIML GREATER THAN +0
00429         MOVE CLAIMI        TO WS-AQ-CLAIM-NO.
00430
00431      IF CERTL  GREATER THAN +0
00432         MOVE CERTI         TO WS-AQ-CERT-PRIME.
00433
00434      IF SFXL   GREATER THAN +0
00435         MOVE SFXI          TO WS-AQ-CERT-SFX.
00436
00437  1010-BYPASS-PRIME-KEY.
00438
00439      MOVE LOW-VALUES       TO EL144AI.
00440
00441      
      * EXEC CICS HANDLE CONDITION
00442 *        ENDFILE  (1800-END-OF-FILE)
00443 *        NOTFND   (1800-END-OF-FILE)
00444 *    END-EXEC.
      *    MOVE '"$''I                  ! # #00003139' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033313339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00445
00446      
      * EXEC CICS STARTBR
00447 *        DATASET (WS-ELACTQ-DSID)
00448 *        RIDFLD  (WS-AQ-CONTROL-PRIMARY)
00449 *        GTEQ
00450 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00003144' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELACTQ-DSID, 
                 WS-AQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00451
00452      MOVE 'Y' TO WS-BROWSE-SW.
00453      SET EL144A-INDEX TO +1.
00454
00455  1100-READNEXT.
00456
00457      
      * EXEC CICS READNEXT
00458 *        DATASET (WS-ELACTQ-DSID)
00459 *        RIDFLD  (WS-AQ-CONTROL-PRIMARY)
00460 *        SET     (ADDRESS OF ACTIVITY-QUE)
00461 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00003155' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELACTQ-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-AQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00462
00463      IF WS-AQ-COMPANY-CD NOT EQUAL PI-COMPANY-CD
00464          GO TO 1800-END-OF-FILE.
00465
00466      IF WS-AQ-CONTROL-PRIMARY EQUAL PI-BOT-KEY
00467         GO TO 1100-READNEXT.
00468
00469      IF NOT PI-NO-CARRIER-SECURITY
00470          IF WS-AQ-CARRIER NOT = PI-CARRIER-SECURITY
00471             GO TO 1100-READNEXT.
00472
00473      IF PI-TOP-KEY EQUAL LOW-VALUES
00474         MOVE WS-AQ-CONTROL-PRIMARY TO PI-TOP-KEY.
00475
00476      MOVE WS-AQ-CONTROL-PRIMARY  TO  PI-BOT-KEY.
00477
00478      MOVE AQ-CLAIM-NO    TO  EL144A-CLAIM   (EL144A-INDEX).
00479      MOVE AQ-CARRIER     TO  EL144A-CARRIER (EL144A-INDEX).
00480      MOVE AQ-CERT-NO     TO  EL144A-CERT-NO (EL144A-INDEX).
00481
00482      IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC
00483         MOVE +0 TO AQ-PMT-UNAPPROVED-COUNT.
00484
00485      IF PENDING-PAYMENTS
00486         MOVE 'YES'       TO  EL144A-PMT (EL144A-INDEX)
00487         MOVE AQ-PAYMENT-COUNTER
00488                          TO  EL144A-PCNT (EL144A-INDEX)
00489         MOVE AQ-PMT-UNAPPROVED-COUNT
00490                          TO  EL144A-PUCNT (EL144A-INDEX).
00491
00492      IF PENDING-FULL-PRINT
00493         MOVE 'YES(F)'       TO EL144A-STATUS (EL144A-INDEX)
00494      ELSE
00495      IF PENDING-PART-PRINT
00496         MOVE 'YES(P)'       TO EL144A-STATUS (EL144A-INDEX).
00497
00498      IF PENDING-LETTERS
00499         MOVE AQ-AUTO-LETTER TO EL144A-LETR (EL144A-INDEX).
00500
00501      MOVE AQ-PENDING-CLAIM-RESTORE
00502                             TO EL144A-REST (EL144A-INDEX).
00503
00504      IF EL144A-INDEX LESS THAN +16
00505         SET EL144A-INDEX UP BY +1
00506         GO TO 1100-READNEXT.
00507
00508      GO TO 1900-END-BROWSE.
00509
00510  1800-END-OF-FILE.
00511      MOVE ER-0313                TO  EMI-ERROR.
00512
00513  1900-END-BROWSE.
00514
00515      IF WS-BROWSE-SW EQUAL 'Y'
00516         
      * EXEC CICS ENDBR
00517 *            DATASET (WS-ELACTQ-DSID)
00518 *       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003214' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELACTQ-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00519
00520      MOVE -1                     TO  PFKEYL.
00521
00522      GO TO 8100-SEND-INITIAL-MAP.
00523
00524      EJECT
00525  2000-START-BROWSE-BWD.
00526
00527      MOVE LOW-VALUES       TO PI-BOT-KEY.
00528      MOVE PI-TOP-KEY       TO WS-AQ-CONTROL-PRIMARY.
00529
00530      MOVE PI-COMPANY-CD    TO WS-AQ-COMPANY-CD.
00531
00532      IF CARRL  EQUAL +0 AND
00533         CLAIML EQUAL +0 AND
00534         CERTL  EQUAL +0 AND
00535         SFXL   EQUAL +0
00536         GO TO 2010-BYPASS-PRIME-KEY.
00537
00538      MOVE LOW-VALUES       TO WS-AQ-CONTROL-PRIMARY.
00539      MOVE PI-COMPANY-CD    TO WS-AQ-COMPANY-CD.
00540
00541      IF CARRL  GREATER THAN +0
00542         MOVE CARRI         TO WS-AQ-CARRIER.
00543
00544      IF CLAIML GREATER THAN +0
00545         MOVE CLAIMI        TO WS-AQ-CLAIM-NO.
00546
00547      IF CERTL  GREATER THAN +0
00548         MOVE CERTI         TO WS-AQ-CERT-PRIME.
00549
00550      IF SFXL   GREATER THAN +0
00551         MOVE SFXI          TO WS-AQ-CERT-SFX.
00552
00553  2010-BYPASS-PRIME-KEY.
00554
00555      MOVE LOW-VALUES       TO EL144AI.
00556
00557      
      * EXEC CICS HANDLE CONDITION
00558 *        ENDFILE  (2800-END-OF-FILE)
00559 *        NOTFND   (2800-END-OF-FILE)
00560 *    END-EXEC.
      *    MOVE '"$''I                  ! $ #00003255' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303033323535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00561
00562      
      * EXEC CICS STARTBR
00563 *        DATASET (WS-ELACTQ-DSID)
00564 *        RIDFLD  (WS-AQ-CONTROL-PRIMARY)
00565 *        GTEQ
00566 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00003260' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELACTQ-DSID, 
                 WS-AQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00567
00568      MOVE 'Y' TO WS-BROWSE-SW.
00569
00570      
      * EXEC CICS READNEXT
00571 *        DATASET (WS-ELACTQ-DSID)
00572 *        RIDFLD  (WS-AQ-CONTROL-PRIMARY)
00573 *        SET     (ADDRESS OF ACTIVITY-QUE)
00574 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00003268' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELACTQ-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-AQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00575
00576      IF PI-TOP-KEY EQUAL LOW-VALUES
00577         MOVE WS-AQ-CONTROL-PRIMARY TO PI-TOP-KEY.
00578
00579      SET EL144A-INDEX TO +16.
00580
00581  2100-READPREV.
00582
00583      
      * EXEC CICS READPREV
00584 *        DATASET (WS-ELACTQ-DSID)
00585 *        RIDFLD  (WS-AQ-CONTROL-PRIMARY)
00586 *        SET     (ADDRESS OF ACTIVITY-QUE)
00587 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00003281' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELACTQ-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-AQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00588
00589      IF WS-AQ-COMPANY-CD NOT = PI-COMPANY-CD
00590          GO TO 2800-END-OF-FILE.
00591
00592      IF NOT PI-NO-CARRIER-SECURITY
00593         IF WS-AQ-CARRIER NOT = PI-CARRIER-SECURITY
00594            GO TO 2100-READPREV.
00595
00596      IF WS-AQ-CONTROL-PRIMARY EQUAL PI-TOP-KEY
00597         GO TO 2100-READPREV.
00598
00599      IF PI-BOT-KEY EQUAL LOW-VALUES
00600         MOVE WS-AQ-CONTROL-PRIMARY TO PI-BOT-KEY.
00601
00602      MOVE WS-AQ-CONTROL-PRIMARY  TO  PI-TOP-KEY.
00603
00604      MOVE AQ-CLAIM-NO    TO  EL144A-CLAIM   (EL144A-INDEX).
00605      MOVE AQ-CARRIER     TO  EL144A-CARRIER (EL144A-INDEX).
00606      MOVE AQ-CERT-NO     TO  EL144A-CERT-NO (EL144A-INDEX).
00607
00608      IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC
00609         MOVE +0 TO AQ-PMT-UNAPPROVED-COUNT.
00610
00611      IF PENDING-PAYMENTS
00612         MOVE 'YES'       TO  EL144A-PMT (EL144A-INDEX)
00613         MOVE AQ-PAYMENT-COUNTER
00614                          TO  EL144A-PCNT (EL144A-INDEX)
00615         MOVE AQ-PMT-UNAPPROVED-COUNT
00616                          TO EL144A-PUCNT (EL144A-INDEX).
00617
00618      IF PENDING-FULL-PRINT
00619         MOVE 'YES(F)'       TO EL144A-STATUS (EL144A-INDEX)
00620      ELSE
00621      IF PENDING-PART-PRINT
00622         MOVE 'YES(P)'       TO EL144A-STATUS (EL144A-INDEX).
00623
00624      IF PENDING-LETTERS
00625         MOVE AQ-AUTO-LETTER TO EL144A-LETR (EL144A-INDEX).
00626
00627      MOVE AQ-PENDING-CLAIM-RESTORE
00628                             TO EL144A-REST (EL144A-INDEX).
00629
00630      IF EL144A-INDEX GREATER THAN +1
00631         SET EL144A-INDEX DOWN BY +1
00632         GO TO 2100-READPREV.
00633
00634      GO TO 2900-END-BROWSE.
00635
00636  2800-END-OF-FILE.
00637
00638      PERFORM 3000-BUMP-SCREEN THRU 3999-EXIT.
00639
00640      MOVE ER-0313                TO  EMI-ERROR.
00641
00642  2900-END-BROWSE.
00643
00644      IF WS-BROWSE-SW EQUAL 'Y'
00645         
      * EXEC CICS ENDBR
00646 *            DATASET (WS-ELACTQ-DSID)
00647 *       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003343' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELACTQ-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00648
00649      MOVE -1                     TO  PFKEYL.
00650
00651      GO TO 8100-SEND-INITIAL-MAP.
00652
00653      EJECT
00654  3000-BUMP-SCREEN.
00655
00656      IF EL144A-INDEX EQUAL +16
00657         GO TO 3999-EXIT.
00658
00659      PERFORM 4000-REARRANGE-SCREEN THRU 4099-EXIT
00660         UNTIL EL144A-LINES (1) NOT EQUAL LOW-VALUES.
00661
00662  3999-EXIT.
00663      EXIT.
00664
00665  4000-REARRANGE-SCREEN.
00666
00667      IF EL144A-LINES (1) EQUAL LOW-VALUES
00668         MOVE EL144A-LINES (02) TO EL144A-LINES (01)
00669         MOVE EL144A-LINES (03) TO EL144A-LINES (02)
00670         MOVE EL144A-LINES (04) TO EL144A-LINES (03)
00671         MOVE EL144A-LINES (05) TO EL144A-LINES (04)
00672         MOVE EL144A-LINES (06) TO EL144A-LINES (05)
00673         MOVE EL144A-LINES (07) TO EL144A-LINES (06)
00674         MOVE EL144A-LINES (08) TO EL144A-LINES (07)
00675         MOVE EL144A-LINES (09) TO EL144A-LINES (08)
00676         MOVE EL144A-LINES (10) TO EL144A-LINES (09)
00677         MOVE EL144A-LINES (11) TO EL144A-LINES (10)
00678         MOVE EL144A-LINES (12) TO EL144A-LINES (11)
00679         MOVE EL144A-LINES (13) TO EL144A-LINES (12)
00680         MOVE EL144A-LINES (14) TO EL144A-LINES (13)
00681         MOVE EL144A-LINES (15) TO EL144A-LINES (14)
00682         MOVE EL144A-LINES (16) TO EL144A-LINES (15)
00683         MOVE LOW-VALUES        TO EL144A-LINES (16).
00684
00685  4099-EXIT.
00686      EXIT.
00687
00688      EJECT
00689  5000-PROCESS-EL144B.
00690
00691      IF EIBAID EQUAL DFHPF12
00692         MOVE 'EL010'            TO  XCTL-PGM
00693         GO TO 9300-XCTL.
00694
00695      IF EIBAID EQUAL DFHPF23
00696         GO TO 9000-RETURN-CICS.
00697
00698      IF EIBAID EQUAL DFHPF24
00699         MOVE 'EL126'            TO  XCTL-PGM
00700         GO TO 9300-XCTL.
00701
00702      IF EIBAID EQUAL DFHENTER
00703         NEXT SENTENCE
00704      ELSE
00705         MOVE ER-0008            TO  EMI-ERROR
00706         MOVE -1                 TO  CARRL
00707         GO TO 8200-SEND-DATAONLY.
00708
00709      IF BCARRL  EQUAL +0 AND
00710         BCLAIML EQUAL +0 AND
00711         BCERTL  EQUAL +0 AND
00712         BSFXL   EQUAL +0
00713           MOVE -1           TO BCARRL
00714           GO TO 8200-SEND-DATAONLY.
00715
00716      IF BCARRL  EQUAL +0 OR
00717         BCLAIML EQUAL +0 OR
00718         BCERTL  EQUAL +0
00719           MOVE ER-0005      TO EMI-ERROR
00720           MOVE -1           TO BCARRL
00721           MOVE AL-UNBON     TO BCARRA
00722           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00723
00724      IF BSFXI = LOW-VALUES
00725         MOVE SPACES TO BSFXI.
00726
00727      IF BTYPEI EQUAL 'P' OR 'L' OR 'S' OR 'F'
00728         NEXT SENTENCE
00729      ELSE
00730         MOVE ER-0676      TO EMI-ERROR
00731         MOVE -1           TO BTYPEL
00732         MOVE AL-UNBON     TO BTYPEA
00733         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00734
00735      IF BTYPEI EQUAL 'P'
00736         IF BPCNTI NOT NUMERIC OR
00737            BPCNTI EQUAL '0'
00738              MOVE ER-0677      TO EMI-ERROR
00739              MOVE -1           TO BPCNTL
00740              MOVE AL-UNBON     TO BPCNTA
00741              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00742         ELSE
00743              MOVE BPCNTI     TO WS-PMT-COUNT.
00744
00745      IF BTYPEI EQUAL 'P'
00746         IF BPUCNTI NOT NUMERIC OR
00747            BPUCNTI EQUAL '0'
00748              MOVE +0          TO WS-UNA-PMT-COUNT
00749         ELSE
00750              MOVE BPUCNTI     TO WS-UNA-PMT-COUNT.
00751
00752      IF BTYPEI EQUAL 'P'
00753         IF WS-UNA-PMT-COUNT GREATER THAN +0
00754            IF WS-UNA-PMT-COUNT NOT EQUAL WS-PMT-COUNT
00755               MOVE ER-0678      TO EMI-ERROR
00756               MOVE -1           TO BTYPEL
00757               MOVE AL-UNBON     TO BTYPEA
00758               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00759
00760      IF NOT EMI-NO-ERRORS
00761         GO TO 8200-SEND-DATAONLY.
00762
00763      MOVE PI-COMPANY-CD  TO WS-AQ-COMPANY-CD.
00764      MOVE BCARRI         TO WS-AQ-CARRIER.
00765      MOVE BCLAIMI        TO WS-AQ-CLAIM-NO.
00766      MOVE BCERTI         TO WS-AQ-CERT-PRIME.
00767      MOVE BSFXI          TO WS-AQ-CERT-SFX.
00768
00769      
      * EXEC CICS HANDLE CONDITION
00770 *         NOTFND    (5050-CLAIM-NOT-FOUND)
00771 *    END-EXEC.
      *    MOVE '"$I                   ! % #00003467' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303033343637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00772
00773      
      * EXEC CICS READ
00774 *         DATASET    (WS-ELMSTR-DSID)
00775 *         RIDFLD     (WS-AQ-CONTROL-PRIMARY)
00776 *         SET        (ADDRESS OF CLAIM-MASTER)
00777 *    END-EXEC.
      *    MOVE '&"S        E          (   #00003471' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELMSTR-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-AQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00778
00779      
      * EXEC CICS HANDLE CONDITION
00780 *         NOTFND    (5040-ADD-ELACTQ)
00781 *    END-EXEC.
      *    MOVE '"$I                   ! & #00003477' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303033343737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00782
00783      
      * EXEC CICS READ
00784 *        DATASET (WS-ELACTQ-DSID)
00785 *        RIDFLD  (WS-AQ-CONTROL-PRIMARY)
00786 *        SET     (ADDRESS OF ACTIVITY-QUE)
00787 *    END-EXEC.
      *    MOVE '&"S        E          (   #00003481' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELACTQ-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-AQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00788
00789      MOVE ER-0679      TO EMI-ERROR.
00790      MOVE -1           TO BCARRL
00791      MOVE AL-UNBON     TO BCARRA
00792                           BCLAIMA
00793                           BCERTA
00794                           BSFXA.
00795
00796      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00797      GO TO 8200-SEND-DATAONLY.
00798
00799  5040-ADD-ELACTQ.
00800
00801      
      * EXEC CICS GETMAIN
00802 *         LENGTH     (WS-ELACTQ-LENGTH)
00803 *         INITIMG    (GETMAIN-SPACE)
00804 *         SET        (ADDRESS OF ACTIVITY-QUE)
00805 *    END-EXEC.
      *    MOVE ',"IL                  $   #00003499' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 WS-ELACTQ-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00806
00807      MOVE 'AQ'            TO AQ-RECORD-ID
00808      MOVE PI-COMPANY-CD   TO AQ-COMPANY-CD
00809      MOVE BCARRI          TO AQ-CARRIER
00810      MOVE BCLAIMI         TO AQ-CLAIM-NO
00811      MOVE BCERTI          TO AQ-CERT-PRIME
00812      MOVE BSFXI           TO AQ-CERT-SFX
00813      MOVE LOW-VALUES      TO AQ-RESEND-DATE
00814                              AQ-FOLLOWUP-DATE.
00815
00816      MOVE +0              TO AQ-PAYMENT-COUNTER
00817                              AQ-PMT-UNAPPROVED-COUNT
00818                              AQ-LAST-UPDATED-BY.
00819
00820      IF BTYPEI EQUAL 'P'
00821         MOVE '1'      TO AQ-PENDING-PAYMENT-FLAG
00822      ELSE
00823      IF BTYPEI EQUAL 'S'
00824         MOVE '2'      TO AQ-PENDING-STATUS-FLAG
00825      ELSE
00826      IF BTYPEI EQUAL 'F'
00827         MOVE '1'      TO AQ-PENDING-STATUS-FLAG.
00828
00829      IF BTYPEI EQUAL 'P'
00830         MOVE WS-PMT-COUNT        TO AQ-PAYMENT-COUNTER
00831         MOVE WS-UNA-PMT-COUNT    TO AQ-PMT-UNAPPROVED-COUNT.
00832
00833      IF (BTYPEI EQUAL 'L') AND
00834         (BLETRI NOT EQUAL LOW-VALUES AND SPACES)
00835         NEXT SENTENCE
00836      ELSE
00837         GO TO 5045-CONTINUE.
00838
00839      MOVE '1'                    TO AQ-PENDING-LETTER-FLAG
00840      MOVE BLETRI                 TO AQ-AUTO-LETTER.
00841
00842      IF BSENDL NOT GREATER ZERO
00843         MOVE ER-0295             TO  EMI-ERROR
00844         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00845         MOVE -1                  TO  BSENDL
00846         MOVE AL-UNBON            TO  BSENDA
00847      ELSE
00848         MOVE BSENDI              TO  WS-DEEDIT-FIELD
00849         PERFORM 8600-DEEDIT THRU 8600-EXIT
00850         MOVE WS-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
00851         MOVE '4'                 TO  DC-OPTION-CODE
00852         PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00853         IF DC-ERROR-CODE NOT = SPACE
00854            MOVE ER-0295          TO  EMI-ERROR
00855            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00856            MOVE -1               TO  BSENDL
00857            MOVE AL-UNBON         TO  BSENDA
00858         ELSE
00859            MOVE DC-BIN-DATE-1    TO  AQ-RESEND-DATE
00860            MOVE AL-UNNON         TO  BSENDA
00861            MOVE DC-GREG-DATE-1-EDIT
00862                                  TO  BSENDO.
00863
00864      IF BFOLLOWL NOT GREATER ZERO
00865         MOVE ER-0296             TO  EMI-ERROR
00866         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00867         MOVE -1                  TO  BFOLLOWL
00868         MOVE AL-UNBON            TO  BFOLLOWA
00869      ELSE
00870         MOVE BFOLLOWI            TO  WS-DEEDIT-FIELD
00871         PERFORM 8600-DEEDIT THRU 8600-EXIT
00872         MOVE WS-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
00873         MOVE '4'                 TO  DC-OPTION-CODE
00874         PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00875         IF DC-ERROR-CODE NOT = SPACE
00876            MOVE ER-0296          TO  EMI-ERROR
00877            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00878            MOVE -1               TO  BFOLLOWL
00879            MOVE AL-UNBON         TO  BFOLLOWA
00880         ELSE
00881            MOVE DC-BIN-DATE-1    TO  AQ-FOLLOWUP-DATE
00882            MOVE AL-UNNON         TO  BFOLLOWA
00883            MOVE DC-GREG-DATE-1-EDIT
00884                                  TO  BFOLLOWO.
00885
00886  5045-CONTINUE.
00887
00888      IF (NO-PENDING-ACTIVITY)
00889               OR
00890         (AQ-PENDING-PAYMENT-FLAG EQUAL '1') AND
00891         (AQ-PAYMENT-COUNTER EQUAL +0)
00892               OR
00893         (AQ-PENDING-LETTER-FLAG EQUAL '1') AND
00894         (AQ-AUTO-LETTER EQUAL SPACES)
00895            MOVE ER-0048             TO EMI-ERROR
00896            MOVE -1                  TO BCARRL
00897            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00898
00899      IF NOT EMI-NO-ERRORS
00900         GO TO 8200-SEND-DATAONLY.
00901
00902      MOVE +144 TO AQ-LAST-UPDATED-BY.
00903
00904      
      * EXEC CICS WRITE
00905 *         DATASET     (WS-ELACTQ-DSID)
00906 *         RIDFLD      (WS-AQ-CONTROL-PRIMARY)
00907 *         FROM        (ACTIVITY-QUE)
00908 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003602' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELACTQ-DSID, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 WS-AQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00909
00910      MOVE ER-0000      TO EMI-ERROR
00911      MOVE -1           TO BCARRL
00912      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00913      GO TO 8200-SEND-DATAONLY.
00914
00915  5050-CLAIM-NOT-FOUND.
00916
00917      MOVE ER-0284      TO EMI-ERROR.
00918      MOVE -1           TO BCARRL.
00919      MOVE AL-UNBON     TO BCARRA
00920                           BCLAIMA
00921                           BCERTA
00922                           BSFXA.
00923
00924      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00925      GO TO 8200-SEND-DATAONLY.
00926
00927      EJECT
00928  6000-PROCESS-EL144C.
00929      IF EIBAID = DFHPF12
00930         MOVE 'EL010'            TO  XCTL-PGM
00931         GO TO 9300-XCTL.
00932
00933      IF EIBAID = DFHPF23
00934         GO TO 9000-RETURN-CICS.
00935
00936      IF EIBAID = DFHPF24
00937         MOVE 'EL126'            TO  XCTL-PGM
00938         GO TO 9300-XCTL.
00939
00940      IF EIBAID = DFHENTER
00941         NEXT SENTENCE
00942      ELSE
00943         MOVE ER-0008            TO  EMI-ERROR
00944         MOVE -1                 TO  CCARRL
00945         GO TO 8200-SEND-DATAONLY.
00946
00947      IF CCARRL  = +0 AND
00948         CCLAIML = +0 AND
00949         CCERTL  = +0 AND
00950         CSFXL   = +0
00951           MOVE -1           TO CCARRL
00952           GO TO 8200-SEND-DATAONLY.
00953
00954      IF CCARRL  = +0 OR
00955         CCLAIML = +0 OR
00956         CCERTL  = +0
00957           MOVE ER-0005      TO EMI-ERROR
00958           MOVE -1           TO CCARRL
00959           MOVE AL-UNBON     TO CCARRA
00960           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00961
00962      IF CSFXI = LOW-VALUES
00963         MOVE SPACES TO CSFXI.
00964
00965      IF (CRTYPL = +0)
00966              OR
00967         (CRTYPI NOT = 'C' AND 'L' AND 'B')
00968           MOVE ER-0980      TO EMI-ERROR
00969           MOVE -1           TO CRTYPL
00970           MOVE AL-UNBON     TO CRTYPA
00971           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00972
00973      IF NOT EMI-NO-ERRORS
00974         GO TO 8200-SEND-DATAONLY.
00975
00976      MOVE PI-COMPANY-CD  TO WS-AQ-COMPANY-CD.
00977      MOVE CCARRI         TO WS-AQ-CARRIER.
00978      MOVE CCLAIMI        TO WS-AQ-CLAIM-NO.
00979      MOVE CCERTI         TO WS-AQ-CERT-PRIME.
00980      MOVE CSFXI          TO WS-AQ-CERT-SFX.
00981
00982      
      * EXEC CICS HANDLE CONDITION
00983 *         NOTFND    (6040-ADD-ELACTQ)
00984 *    END-EXEC.
      *    MOVE '"$I                   ! '' #00003680' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303033363830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00985
00986      
      * EXEC CICS READ
00987 *        DATASET (WS-ELACTQ-DSID)
00988 *        RIDFLD  (WS-AQ-CONTROL-PRIMARY)
00989 *        SET     (ADDRESS OF ACTIVITY-QUE)
00990 *    END-EXEC.
      *    MOVE '&"S        E          (   #00003684' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELACTQ-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-AQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00991
00992      MOVE ER-0679      TO EMI-ERROR.
00993      MOVE -1           TO CCARRL.
00994      MOVE AL-UNBON     TO CCARRA
00995                           CCLAIMA
00996                           CCERTA
00997                           CSFXA.
00998
00999      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01000      GO TO 8200-SEND-DATAONLY.
01001
01002  6040-ADD-ELACTQ.
01003      
      * EXEC CICS GETMAIN
01004 *         LENGTH     (WS-ELACTQ-LENGTH)
01005 *         INITIMG    (GETMAIN-SPACE)
01006 *         SET        (ADDRESS OF ACTIVITY-QUE)
01007 *    END-EXEC.
      *    MOVE ',"IL                  $   #00003701' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 WS-ELACTQ-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01008
01009      MOVE 'AQ'            TO AQ-RECORD-ID.
01010      MOVE PI-COMPANY-CD   TO AQ-COMPANY-CD.
01011      MOVE CCARRI          TO AQ-CARRIER.
01012      MOVE CCLAIMI         TO AQ-CLAIM-NO.
01013      MOVE CCERTI          TO AQ-CERT-PRIME.
01014      MOVE CSFXI           TO AQ-CERT-SFX.
01015      MOVE LOW-VALUES      TO AQ-RESEND-DATE
01016                              AQ-FOLLOWUP-DATE.
01017
01018      MOVE +0              TO AQ-PAYMENT-COUNTER
01019                              AQ-PMT-UNAPPROVED-COUNT
01020                              AQ-LAST-UPDATED-BY.
01021
01022      MOVE CRTYPI          TO AQ-PENDING-CLAIM-RESTORE.
01023
01024  6045-CONTINUE.
01025      IF NOT EMI-NO-ERRORS
01026         GO TO 8200-SEND-DATAONLY.
01027
01028      MOVE +144 TO AQ-LAST-UPDATED-BY.
01029
01030      
      * EXEC CICS WRITE
01031 *         DATASET     (WS-ELACTQ-DSID)
01032 *         RIDFLD      (WS-AQ-CONTROL-PRIMARY)
01033 *         FROM        (ACTIVITY-QUE)
01034 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003728' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELACTQ-DSID, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 WS-AQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01035
01036      MOVE ER-0000      TO EMI-ERROR.
01037      MOVE -1           TO CCARRL.
01038      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01039      GO TO 8200-SEND-DATAONLY.
01040
01041  EJECT
01042  8100-SEND-INITIAL-MAP.
01043
01044      IF EMI-ERROR NOT = ZERO
01045         PERFORM 9900-ERROR-FORMAT.
01046
PEMMOD     IF PI-PROCESSOR-ID NOT EQUAL 'LGXX' AND 'PEMA'
01048         IF PI-MAP-NAME EQUAL 'EL144A'
01049            MOVE AL-SADOF       TO  PFKEY3A.
01050
01051      IF PI-MAP-NAME EQUAL 'EL144A'
01052         MOVE EIBTIME                TO TIME-IN
01053         MOVE SAVE-DATE              TO DATEO
01054         MOVE TIME-OUT               TO TIMEO
01055         MOVE -1                     TO PFKEYL
01056         MOVE EMI-MESSAGE-AREA (1)   TO MSG1O
01057         
      * EXEC CICS SEND
01058 *            FROM   (EL144AI)
01059 *            MAPSET (WS-MAPSET-NAME)
01060 *            MAP    (WS-MAP-NAME)
01061 *            CURSOR
01062 *            ERASE
01063 *       END-EXEC
           MOVE LENGTH OF
            EL144AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00003755' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL144AI, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01064      ELSE
01065      IF PI-MAP-NAME = 'EL144B'
01066         MOVE EIBTIME                TO TIME-IN
01067         MOVE SAVE-DATE              TO BDATEO
01068         MOVE TIME-OUT               TO BTIMEO
01069         MOVE -1                     TO BCARRL
01070         MOVE EMI-MESSAGE-AREA (1)   TO BMSG1O
01071         
      * EXEC CICS SEND
01072 *            FROM   (EL144AI)
01073 *            MAPSET (WS-MAPSET-NAME)
01074 *            MAP    ('EL144B')
01075 *            CURSOR
01076 *            ERASE
01077 *       END-EXEC
           MOVE LENGTH OF
            EL144AI
             TO DFHEIV12
           MOVE 'EL144B' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00003769' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL144AI, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01078      ELSE
01079         MOVE EIBTIME                TO TIME-IN
01080         MOVE SAVE-DATE              TO CDATEO
01081         MOVE TIME-OUT               TO CTIMEO
01082         MOVE -1                     TO CCARRL
01083         MOVE EMI-MESSAGE-AREA (1)   TO CMSG1O
01084         
      * EXEC CICS SEND
01085 *            FROM   (EL144AI)
01086 *            MAPSET (WS-MAPSET-NAME)
01087 *            MAP    ('EL144C')
01088 *            CURSOR
01089 *            ERASE
01090 *       END-EXEC.
           MOVE LENGTH OF
            EL144AI
             TO DFHEIV12
           MOVE 'EL144C' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00003782' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL144AI, 
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
           
01091
01092      GO TO 9100-RETURN-TRAN.
01093
01094  8200-SEND-DATAONLY.
01095
01096      IF EMI-ERROR NOT = ZERO
01097          PERFORM 9900-ERROR-FORMAT.
01098
PEMMOD     IF PI-PROCESSOR-ID NOT EQUAL 'LGXX' AND 'PEMA'
01100         IF PI-MAP-NAME EQUAL 'EL144A'
01101            MOVE AL-SADOF       TO  PFKEY3A.
01102
01103      IF PI-MAP-NAME EQUAL 'EL144A'
01104         MOVE EIBTIME                TO TIME-IN
01105         MOVE SAVE-DATE              TO DATEO
01106         MOVE TIME-OUT               TO TIMEO
01107         MOVE -1                     TO PFKEYL
01108         MOVE EMI-MESSAGE-AREA (1)   TO MSG1O
01109         
      * EXEC CICS SEND DATAONLY
01110 *           FROM   (EL144AI)
01111 *           MAPSET (WS-MAPSET-NAME)
01112 *           MAP    (WS-MAP-NAME)
01113 *           CURSOR
01114 *       END-EXEC
           MOVE LENGTH OF
            EL144AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00003807' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL144AI, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01115      ELSE
01116      IF PI-MAP-NAME = 'EL144B'
01117         MOVE EIBTIME                TO TIME-IN
01118         MOVE SAVE-DATE              TO BDATEO
01119         MOVE TIME-OUT               TO BTIMEO
01120         MOVE EMI-MESSAGE-AREA (1)   TO BMSG1O
01121         
      * EXEC CICS SEND DATAONLY
01122 *           FROM   (EL144AI)
01123 *           MAPSET (WS-MAPSET-NAME)
01124 *           MAP    ('EL144B')
01125 *           CURSOR
01126 *       END-EXEC
           MOVE LENGTH OF
            EL144AI
             TO DFHEIV12
           MOVE 'EL144B' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00003819' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL144AI, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01127      ELSE
01128         MOVE EIBTIME                TO TIME-IN
01129         MOVE SAVE-DATE              TO CDATEO
01130         MOVE TIME-OUT               TO CTIMEO
01131         MOVE EMI-MESSAGE-AREA (1)   TO CMSG1O
01132         
      * EXEC CICS SEND DATAONLY
01133 *           FROM   (EL144AI)
01134 *           MAPSET (WS-MAPSET-NAME)
01135 *           MAP    ('EL144C')
01136 *           CURSOR
01137 *       END-EXEC.
           MOVE LENGTH OF
            EL144AI
             TO DFHEIV12
           MOVE 'EL144C' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00003830' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL144AI, 
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
           
01138
01139      GO TO 9100-RETURN-TRAN.
01140
01141      EJECT
01142  8300-SEND-TEXT.
01143
01144      
      * EXEC CICS SEND TEXT
01145 *        FROM   (LOGOFF-TEXT)
01146 *        LENGTH (LOGOFF-LENGTH)
01147 *        ERASE  FREEKB
01148 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00003842' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383432' TO DFHEIV0(25:11)
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
           
01149
01150      
      * EXEC CICS RETURN
01151 *    END-EXEC.
      *    MOVE '.(                    &   #00003848' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01152
01153      EJECT
01154  8500-DATE-CONVERSION.
01155
01156      
      * EXEC CICS LINK
01157 *        PROGRAM  ('ELDATCV')
01158 *        COMMAREA (DATE-CONVERSION-DATA)
01159 *        LENGTH   (DC-COMM-LENGTH)
01160 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00003854' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01161
01162  8500-EXIT.
01163      EXIT.
01164
01165      EJECT
01166  8600-DEEDIT.
01167      
      * EXEC CICS BIF DEEDIT
01168 *        FIELD  (WS-DEEDIT-FIELD)
01169 *        LENGTH (WS-DEEDIT-LENGTH)
01170 *    END-EXEC.
      *    MOVE '@"L                   #   #00003865' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DEEDIT-FIELD, 
                 WS-DEEDIT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01171
01172  8600-EXIT.
01173      EXIT.
01174
01175      EJECT
01176  9000-RETURN-CICS.
01177
01178      MOVE 'EL005'                TO  XCTL-PGM.
01179      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
01180      GO TO 9300-XCTL.
01181
01182  9100-RETURN-TRAN.
01183
01184      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
01185      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
01186
01187      
      * EXEC CICS RETURN
01188 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01189 *        LENGTH   (PI-COMM-LENGTH)
01190 *        TRANSID  (WS-TRANS-ID)
01191 *    END-EXEC.
      *    MOVE '.(CT                  &   #00003885' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01192
01193  9100-EXIT.
01194      EXIT.
01195
01196  9300-XCTL.
01197
01198      MOVE DFHENTER               TO  EIBAID.
01199
01200      
      * EXEC CICS XCTL
01201 *        PROGRAM  (XCTL-PGM)
01202 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01203 *        LENGTH   (PI-COMM-LENGTH)
01204 *    END-EXEC.
      *    MOVE '.$C                   $   #00003898' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 XCTL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01205
01206  9300-EXIT.
01207      EXIT.
01208
01209      EJECT
01210  9400-CLEAR.
01211
01212      MOVE PI-RETURN-TO-PROGRAM  TO  XCTL-PGM.
01213      GO TO 9300-XCTL.
01214
01215  9600-PGMIDERR.
01216
01217      
      * EXEC CICS HANDLE CONDITION
01218 *        PGMIDERR (8300-SEND-TEXT)
01219 *    END-EXEC.
      *    MOVE '"$L                   ! ( #00003915' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303033393135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01220
01221      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM
01222                                      LOGOFF-PGM.
01223
01224      MOVE 'EL005'                TO  XCTL-PGM.
01225      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
01226      MOVE SPACES                 TO  PI-ENTRY-CD-1.
01227      GO TO 9300-XCTL.
01228
01229  9900-ERROR-FORMAT.
01230
01231      
      * EXEC CICS LINK
01232 *        PROGRAM  ('EL001')
01233 *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
01234 *        LENGTH   (EMI-COMM-LENGTH)
01235 *    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   ''   #00003929' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01236
01237  9900-EXIT.
01238      EXIT.
01239
01240  9990-ERROR.
01241
01242      MOVE DFHEIBLK TO EMI-LINE1.
01243      
      * EXEC CICS LINK
01244 *        PROGRAM  ('EL004')
01245 *        COMMAREA (EMI-LINE1)
01246 *        LENGTH   (72)
01247 *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00003941' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01248
01249      GO TO 8200-SEND-DATAONLY.
01250
01251      EJECT
01252  9995-SECURITY-VIOLATION.
01253 *    COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00003968' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393638' TO DFHEIV0(25:11)
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
01254
01255  9995-EXIT.
01256      EXIT.
01257
01258  9999-LAST-PARAGRAPH SECTION.
01259
01260      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL144' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01261

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL144' TO DFHEIV1
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
               GO TO 1800-END-OF-FILE,
                     1800-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 2800-END-OF-FILE,
                     2800-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 5050-CLAIM-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 5040-ADD-ELACTQ
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 6040-ADD-ELACTQ
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL144' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
