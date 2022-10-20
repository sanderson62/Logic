       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 EL128 .
      *              PROGRAM CONVERTED BY
      *              COBOL CONVERSION AID PO 5785-ABJ
      *              CONVERSION DATE 02/07/95 10:52:52.
      *                            VMOD=2.014
      *
      *
      *AUTHOR.    CENTRAL STATES HEALTH AND LIFE
      *           OMAHA, NEBRASKA
      *DATE-COMPILED.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CNETRAL STATES  *
      *            *   HEALTH AND LIFE                                 *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CENTRAL STATES. *
      *            *                                                   *
      *            *****************************************************
      *REMARKS.
      *        THIS PROGRAM PROVIDES THE QUALIFICATION NECESSARY FOR
      *    THE PURGED CERT LOOK-UP.
      *    SCREENS     - EL128A - PURGED CERT LOOK-UP QUALIFICATION
      *    ENTERED BY  - EL126 - MASTER MENU
      *    EXIT TO     - CALLING PROGRAM
      *    INPUT FILE  - ELPURG - PURGED CERT MASTER FILE
      *                  ERACCT2 - CREDIT ACCOUNT MASTER FILE
      *    OUTPUT FILE - NONE
      *    COMMAREA    - PASSED.  IF A CERTIFICATE IS SELECTED, THE
      *                  CONTROL OF THAT CERTIFICATE IS PLACED IN THE
      *                  APPROPRIATE FIELDS OF THE COMMAAREA FOR
      *                  REFERENCE BY SUCCESSIVE PROGRAMS.  THE PROGRAM
      *                  WORK AREA OF THE COMMAREA IS USED TO PASS THE
      *                  RECORD KEY INFORMATION NEEDED BY EL1282 TO
      *                  LOCATE THE CERTIFICATE.
      *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON
      *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE
      *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
      *                  ENTRIES (XCTL FROM CICS VIA EX15) THE SCREEN
      *                  WILL BE READ AND ACTION WILL BE BASED ON THE
      *                  MAINTENANCE TYPE INDICATED.
           EJECT
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
       77  FILLER  PIC X(32)  VALUE '********************************'.
       77  FILLER  PIC X(32)  VALUE '*    EL128 WORKING STORAGE     *'.
       77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.014 *********'.
      *                                    COPY ELCSCTM.
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
      *                                    COPY ELCSCRTY.
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
           05  SAVE-DATE                   PIC X(8)     VALUE SPACES.
           05  SAVE-BIN-DATE               PIC X(2)     VALUE SPACES.
       01  FILLER                          COMP-3.
           05  WS-READNEXT-SW              PIC S9       VALUE ZERO.
           05  TIME-IN                     PIC S9(7)    VALUE ZERO.
           05  TIME-OUT                    REDEFINES
               TIME-IN                     PIC S9(3)V9(4).
       01  FILLER         COMP SYNC.
           05  SC-ITEM                     PIC S9(4)    VALUE +0001.
       01  FILLER.
           05  XCTL-725                    PIC X(8)     VALUE 'EL725'.
           05  QID.
               10  QID-TERM                PIC X(4).
               10  FILLER                  PIC X(4)     VALUE '128A'.
           05  QID-ITEM                    PIC S9(4)    VALUE +1 COMP.
           05  WS-KEY-LENGTH               PIC S9(4)    VALUE +0 COMP.
           05  PART-KEY-ON-SW              PIC X(01)    VALUE 'N'.
               88  PART-KEY-ON                          VALUE 'Y'.
           05  PART-FIELD-ON-SW            PIC X(01)    VALUE ' '.
               88  PART-FIELD-ACCT                      VALUE 'A'.
               88  PART-FIELD-STATE                     VALUE 'S'.
               88  PART-FIELD-CERT                      VALUE 'C'.
           05  WS-CNTL-KEY.
               10  WS-CNTL-ID              PIC X(3).
               10  WS-CNTL-TYPE            PIC X.
               10  WS-CNTL-USER            PIC X(4)     VALUE SPACES.
               10  WS-CNTL-SEQ             PIC S9(4)    VALUE +0 COMP.
           05  WS-MAPSET-NAME              PIC X(8)     VALUE 'EL128S'.
           05  WS-MAP-NAME                 PIC X(8)     VALUE 'EL128A'.
           05  FILLER                      REDEFINES
               WS-MAP-NAME.
               10  FILLER                  PIC XX.
               10  WS-MAP-NUMBER           PIC X(4).
               10  FILLER                  PIC XX.
           05  THIS-PGM                    PIC X(8)     VALUE 'EL128'.
           05  WS-CNTL-REC-FOUND-SW        PIC X(01)    VALUE SPACE.
           05  WS-NEXT-COMPANY-ID          PIC X(03)    VALUE SPACES.
           05  WS-CONTROL-FILE-DSID        PIC X(8)     VALUE 'ELCNTL'.
           05  WS-ACCOUNT-MASTER-DSID      PIC X(8)     VALUE 'ERACCT2'.
           05  WS-CERT-MASTER-DSID         PIC X(8)     VALUE 'ELPURG'.
           05  WS-CERT-AIX01-DSID          PIC X(8)     VALUE 'ELPURG2'.
           05  WS-CERT-AIX02-DSID          PIC X(8)     VALUE 'ELPURG3'.
           05  WS-CERT-AIX03-DSID          PIC X(8)     VALUE 'ELPURG4'.
           05  WS-CERT-AIX04-DSID          PIC X(8)     VALUE 'ELPURG5'.
           05  WS-CERT-AIX05-DSID          PIC X(8)     VALUE 'ELPURG6'.
           05  WS-TRANS-ID                 PIC X(4)     VALUE 'EXXC'.
           05  WK-SC-STATE.
               12  WK-SC-STATE-1           PIC X.
               12  WK-SC-STATE-2           PIC X.
           05  WK-SC-CERT.
               12  WK-SC-CERT-1            PIC X.
               12  WK-SC-CERT-2            PIC X.
               12  WK-SC-CERT-3            PIC X.
               12  WK-SC-CERT-4            PIC X.
               12  WK-SC-CERT-5            PIC X.
               12  WK-SC-CERT-6            PIC X.
               12  WK-SC-CERT-7            PIC X.
               12  WK-SC-CERT-8            PIC X.
               12  WK-SC-CERT-9            PIC X.
               12  WK-SC-CERT-10           PIC X.
           05  WS-DEEDIT-FIELD             PIC X(15)    VALUE ZERO.
           05  WS-DEEDIT-FIELD-V0          REDEFINES
               WS-DEEDIT-FIELD             PIC S9(15).
           05  WS-INPUT-FIELD              PIC X(50)    VALUE SPACES.
           05  WS-INPUT-CHAR               REDEFINES
               WS-INPUT-FIELD              PIC X
               OCCURS 50 TIMES             INDEXED BY INPUT-INDEX.
       01  WS-FIRST-NAME.
           05  WS-FIRST-INITIAL            PIC X        VALUE SPACES.
           05  WS-FIRST-REST               PIC X(14)    VALUE SPACES.
       01  WS-INITIALS.
           05  WS-INITIAL-FIRST            PIC X        VALUE SPACES.
           05  WS-INITIAL-MIDDLE           PIC X        VALUE SPACES.
           05  PI-ACCOUNT-KEY.
               10  PI-AK-COMPANY-CD        PIC X.
               10  PI-AK-CARRIER           PIC X.
               10  PI-AK-GROUP             PIC X(06).
               10  PI-AK-STATE             PIC XX.
               10  PI-AK-ACCOUNT           PIC X(10).
               10  PI-AK-EXPIRE-DATE       PIC XX.
           EJECT
           05  ERROR-MESSAGES.
               10  ER-0004                 PIC X(4)     VALUE '0004'.
               10  ER-0008                 PIC X(4)     VALUE '0008'.
               10  ER-0019                 PIC X(4)     VALUE '0019'.
               10  ER-0022                 PIC X(4)     VALUE '0022'.
               10  ER-0029                 PIC X(4)     VALUE '0029'.
               10  ER-0070                 PIC X(4)     VALUE '0070'.
               10  ER-0089                 PIC X(4)     VALUE '0089'.
               10  ER-0194                 PIC X(4)     VALUE '0194'.
               10  ER-0195                 PIC X(4)     VALUE '0195'.
               10  ER-0196                 PIC X(4)     VALUE '0196'.
               10  ER-0197                 PIC X(4)     VALUE '0197'.
               10  ER-0198                 PIC X(4)     VALUE '0198'.
               10  ER-0201                 PIC X(4)     VALUE '0201'.
               10  ER-0210                 PIC X(4)     VALUE '0210'.
               10  ER-0215                 PIC X(4)     VALUE '0215'.
               10  ER-0216                 PIC X(4)     VALUE '0216'.
               10  ER-0228                 PIC X(4)     VALUE '0228'.
               10  ER-0488                 PIC X(4)     VALUE '0488'.
               10  ER-0671                 PIC X(4)     VALUE '0671'.
               10  ER-0764                 PIC X(4)     VALUE '0764'.
               10  ER-0765                 PIC X(4)     VALUE '0765'.
               10  ER-2370                 PIC X(4)     VALUE '2370'.
               10  ER-2371                 PIC X(4)     VALUE '2371'.
               10  ER-2373                 PIC X(4)     VALUE '2373'.
               10  ER-8100                 PIC X(4)     VALUE '8100'.
               10  ER-8101                 PIC X(4)     VALUE '8101'.
               10  ER-8102                 PIC X(4)     VALUE '8102'.
               10  ER-8103                 PIC X(4)     VALUE '8103'.
               10  ER-8104                 PIC X(4)     VALUE '8104'.
               10  ER-8105                 PIC X(4)     VALUE '8105'.
               10  ER-8106                 PIC X(4)     VALUE '8106'.
               10  ER-8107                 PIC X(4)     VALUE '8107'.
           EJECT
      *                                    COPY ELCINTF.
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
      *                                    COPY ELC127PI.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELC127PI.                           *
00004 *                            VMOD=2.005                          *
00005 *                                                                *
00006 ******************************************************************
00007
00008      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00009          16  PI-1ST-TIME-SW              PIC S9     COMP-3.
00010          16  PI-LINE-COUNT               PIC S9(3)  COMP-3.
00011          16  PI-AIX-RECORD-COUNT         PIC S9(4)  COMP SYNC.
00012          16  PI-START-SW                 PIC S9     COMP-3.
00013          16  PI-BROWSE-SW                PIC S9     COMP-3.
00014          16  PI-END-OF-FILE              PIC S9     COMP-3.
00015          16  PI-DSID                     PIC X(8).
00016          16  PI-OPTION                   PIC X.
00017            88  NO-OPTION-SELECTED                    VALUE ZERO.
00018            88  OPTION-ONE-SELECTED                   VALUE '1'.
00019            88  OPTION-TWO-SELECTED                   VALUE '2'.
00020            88  OPTION-THREE-SELECTED                 VALUE '3'.
00021            88  OPTION-FOUR-SELECTED                  VALUE '4'.
00022
00023          16  PI-SELECTION-CRITERIA.
00024              20  PI-SC-COMPANY-CD        PIC X.
00025              20  PI-SC-CARRIER           PIC X.
00026              20  PI-SC-GROUP             PIC X(6).
00027              20  PI-SC-STATE             PIC XX.
00028              20  PI-SC-ACCOUNT           PIC X(10).
00029              20  PI-SC-EFF-DATE          PIC XX.
00030              20  PI-SC-CERT-NO.
00031                  25  PI-SC-CERT-PRIME    PIC X(10).
00032                  25  PI-SC-CERT-SFX      PIC X.
00033
00034          16  FILLER REDEFINES PI-SELECTION-CRITERIA.
00035              20  FILLER                  PIC X.
00036              20  PI-SC-LAST-NAME         PIC X(15).
00037              20  PI-SC-INITIALS          PIC XX.
00038              20  PI-SC-ACCT-NO           PIC X(10).
00039              20  PI-SC-CARR              PIC X.
110106             20  PI-SC-ST                PIC XX.
110106             20  PI-SC-STATUS            PIC X.
00040              20  FILLER                  PIC X.
00041
00042          16  FILLER REDEFINES PI-SELECTION-CRITERIA.
00043              20  FILLER                  PIC X.
00044              20  PI-SC-SOC-SEC-NO        PIC X(11).
00045              20  FILLER                  PIC X(21).
00046
00047          16  FILLER REDEFINES PI-SELECTION-CRITERIA.
00048              20  FILLER                  PIC X.
00049              20  PI-SC-MEMBER-NO         PIC X(12).
00050              20  FILLER                  PIC X(20).
00051
00052          16  FILLER REDEFINES PI-SELECTION-CRITERIA.
00053              20  FILLER                  PIC X.
00054              20  PI-SC-CERT-NO-A4.
00055                  25  PI-SC-CERT-PRIME-A4 PIC X(10).
00056                  25  PI-SC-CERT-SFX-A4   PIC X.
00057              20  FILLER                  PIC X(21).
00058
00059          16  PI-CERTIFICATE-KEY.
00060              20  PI-CK-COMPANY-CD        PIC X.
00061              20  PI-CK-CARRIER           PIC X.
00062              20  PI-CK-GROUPING          PIC X(6).
00063              20  PI-CK-STATE             PIC XX.
00064              20  PI-CK-ACCOUNT           PIC X(10).
00065              20  PI-CK-CERT-EFF-DT       PIC XX.
00066              20  PI-CK-CERT-NO.
00067                  25  PI-CK-CERT-PRIME    PIC X(10).
00068                  25  PI-CK-CERT-SFX      PIC X.
00069
00070          16  FILLER REDEFINES PI-CERTIFICATE-KEY.
00071              20  FILLER                  PIC X.
00072              20  PI-CK-INSURED-LAST-NAME PIC X(15).
00073              20  PI-CK-INSURED-INITIALS  PIC XX.
00074              20  FILLER                  PIC X(15).
00075
00076          16  FILLER REDEFINES PI-CERTIFICATE-KEY.
00077              20  FILLER                  PIC X.
00078              20  PI-CK-SOC-SEC-NO        PIC X(11).
00079              20  FILLER                  PIC X(21).
00080
00081          16  FILLER REDEFINES PI-CERTIFICATE-KEY.
00082              20  FILLER                  PIC X.
00083              20  PI-CK-MEMBER-NO         PIC X(12).
00084              20  FILLER                  PIC X(20).
00085
00086          16  FILLER REDEFINES PI-CERTIFICATE-KEY.
00087              20  FILLER                  PIC X.
00088              20  PI-CK-CERT-NO-A4.
00089                  25  PI-CK-CERT-PRIME-A4 PIC X(10).
00090                  25  PI-CK-CERT-SFX-A4   PIC X.
00091              20  FILLER                  PIC X(21).
00092
00093          16  PI-PREV-INITIALS            PIC XX.
00094          16  FILLER                      PIC X(31).
00095
00096          16  PI-KEY-LENGTH               PIC S9(4)   COMP SYNC.
00097          16  PI-TS-ITEM                  PIC S9(4)   COMP SYNC.
00098          16  PI-1ST-KEY                  PIC X(33).
00099          16  PI-LAST-KEY                 PIC X(33).
00100          16  PI-PREV-AID                 PIC X.
00101
00102          16  FILLER                      PIC X(34).
00103
00104          16  PI-END-NAME                 PIC X(15).
00105          16  FILLER                      PIC X(6).
00106
00107          16  PI-END-CERTIFICATE-KEY.
00108              20  PI-END-COMPANY-ID       PIC X.
00109              20  PI-END-CARRIER          PIC X.
00110              20  PI-END-GROUPING         PIC X(6).
00111              20  PI-END-STATE            PIC XX.
00112              20  PI-END-ACCOUNT          PIC X(10).
00113              20  PI-END-CERT-EFF-DT      PIC XX.
00114              20  PI-END-CERT-NO.
00115                  24  PI-END-CERT-PRIME   PIC X(10).
00116                  24  PI-END-CERT-SFX     PIC X.
00117
00118          16  PI-LIN1-CERTIFICATE-KEY     PIC X(33).
00119          16  PI-SCREEN-COUNT             PIC S9(8) COMP.
00120          16  PI-SC-FIRST-NAME            PIC X(15).
00121          16  PI-ALT-NAME-COUNT           PIC S9(4).
00122
               16  FILLER                  PIC X(167).
               16  PI-PART-KEY-SW          PIC X(01).
               16  PI-PART-FIELD-SW        PIC X(01).
               16  FILLER                  PIC X(138).
           EJECT
      *                                    COPY ELCEMIB.
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
           EJECT
      *                                    COPY ELCDATE.
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
           EJECT
      *                                    COPY ELCLOGOF.
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
           EJECT
      *                                    COPY EL128S.
       01  EL128BI.
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
           05  BCOMPL PIC S9(0004) COMP.
           05  BCOMPF PIC  X(0001).
           05  FILLER REDEFINES BCOMPF.
               10  BCOMPA PIC  X(0001).
           05  BCOMPI PIC  X(0003).
      *    -------------------------------
           05  BAST01L PIC S9(0004) COMP.
           05  BAST01F PIC  X(0001).
           05  FILLER REDEFINES BAST01F.
               10  BAST01A PIC  X(0001).
           05  BAST01I PIC  X(0001).
      *    -------------------------------
           05  BCRTSL1L PIC S9(0004) COMP.
           05  BCRTSL1F PIC  X(0001).
           05  FILLER REDEFINES BCRTSL1F.
               10  BCRTSL1A PIC  X(0001).
           05  BCRTSL1I PIC  X(0001).
      *    -------------------------------
           05  BNAME01L PIC S9(0004) COMP.
           05  BNAME01F PIC  X(0001).
           05  FILLER REDEFINES BNAME01F.
               10  BNAME01A PIC  X(0001).
           05  BNAME01I PIC  X(0018).
      *    -------------------------------
           05  BAGE01L PIC S9(0004) COMP.
           05  BAGE01F PIC  X(0001).
           05  FILLER REDEFINES BAGE01F.
               10  BAGE01A PIC  X(0001).
           05  BAGE01I PIC  X(0002).
      *    -------------------------------
           05  BSEX01L PIC S9(0004) COMP.
           05  BSEX01F PIC  X(0001).
           05  FILLER REDEFINES BSEX01F.
               10  BSEX01A PIC  X(0001).
           05  BSEX01I PIC  X(0001).
      *    -------------------------------
           05  BCAR01L PIC S9(0004) COMP.
           05  BCAR01F PIC  X(0001).
           05  FILLER REDEFINES BCAR01F.
               10  BCAR01A PIC  X(0001).
           05  BCAR01I PIC  X(0001).
      *    -------------------------------
           05  BGRP01L PIC S9(0004) COMP.
           05  BGRP01F PIC  X(0001).
           05  FILLER REDEFINES BGRP01F.
               10  BGRP01A PIC  X(0001).
           05  BGRP01I PIC  X(0006).
      *    -------------------------------
           05  BST01L PIC S9(0004) COMP.
           05  BST01F PIC  X(0001).
           05  FILLER REDEFINES BST01F.
               10  BST01A PIC  X(0001).
           05  BST01I PIC  X(0002).
      *    -------------------------------
           05  BACCT01L PIC S9(0004) COMP.
           05  BACCT01F PIC  X(0001).
           05  FILLER REDEFINES BACCT01F.
               10  BACCT01A PIC  X(0001).
           05  BACCT01I PIC  X(0010).
      *    -------------------------------
           05  BCERT01L PIC S9(0004) COMP.
           05  BCERT01F PIC  X(0001).
           05  FILLER REDEFINES BCERT01F.
               10  BCERT01A PIC  X(0001).
           05  BCERT01I PIC  X(0011).
      *    -------------------------------
           05  BEFFD01L PIC S9(0004) COMP.
           05  BEFFD01F PIC  X(0001).
           05  FILLER REDEFINES BEFFD01F.
               10  BEFFD01A PIC  X(0001).
           05  BEFFD01I PIC  X(0008).
      *    -------------------------------
           05  BLFI01L PIC S9(0004) COMP.
           05  BLFI01F PIC  X(0001).
           05  FILLER REDEFINES BLFI01F.
               10  BLFI01A PIC  X(0001).
           05  BLFI01I PIC  X(0045).
      *    -------------------------------
           05  BAHI01L PIC S9(0004) COMP.
           05  BAHI01F PIC  X(0001).
           05  FILLER REDEFINES BAHI01F.
               10  BAHI01A PIC  X(0001).
           05  BAHI01I PIC  X(0022).
      *    -------------------------------
           05  BAST02L PIC S9(0004) COMP.
           05  BAST02F PIC  X(0001).
           05  FILLER REDEFINES BAST02F.
               10  BAST02A PIC  X(0001).
           05  BAST02I PIC  X(0001).
      *    -------------------------------
           05  BCRTSL2L PIC S9(0004) COMP.
           05  BCRTSL2F PIC  X(0001).
           05  FILLER REDEFINES BCRTSL2F.
               10  BCRTSL2A PIC  X(0001).
           05  BCRTSL2I PIC  X(0001).
      *    -------------------------------
           05  BNAME02L PIC S9(0004) COMP.
           05  BNAME02F PIC  X(0001).
           05  FILLER REDEFINES BNAME02F.
               10  BNAME02A PIC  X(0001).
           05  BNAME02I PIC  X(0018).
      *    -------------------------------
           05  BAGE02L PIC S9(0004) COMP.
           05  BAGE02F PIC  X(0001).
           05  FILLER REDEFINES BAGE02F.
               10  BAGE02A PIC  X(0001).
           05  BAGE02I PIC  X(0002).
      *    -------------------------------
           05  BSEX02L PIC S9(0004) COMP.
           05  BSEX02F PIC  X(0001).
           05  FILLER REDEFINES BSEX02F.
               10  BSEX02A PIC  X(0001).
           05  BSEX02I PIC  X(0001).
      *    -------------------------------
           05  BCAR02L PIC S9(0004) COMP.
           05  BCAR02F PIC  X(0001).
           05  FILLER REDEFINES BCAR02F.
               10  BCAR02A PIC  X(0001).
           05  BCAR02I PIC  X(0001).
      *    -------------------------------
           05  BGRP02L PIC S9(0004) COMP.
           05  BGRP02F PIC  X(0001).
           05  FILLER REDEFINES BGRP02F.
               10  BGRP02A PIC  X(0001).
           05  BGRP02I PIC  X(0006).
      *    -------------------------------
           05  BST02L PIC S9(0004) COMP.
           05  BST02F PIC  X(0001).
           05  FILLER REDEFINES BST02F.
               10  BST02A PIC  X(0001).
           05  BST02I PIC  X(0002).
      *    -------------------------------
           05  BACCT02L PIC S9(0004) COMP.
           05  BACCT02F PIC  X(0001).
           05  FILLER REDEFINES BACCT02F.
               10  BACCT02A PIC  X(0001).
           05  BACCT02I PIC  X(0010).
      *    -------------------------------
           05  BCERT02L PIC S9(0004) COMP.
           05  BCERT02F PIC  X(0001).
           05  FILLER REDEFINES BCERT02F.
               10  BCERT02A PIC  X(0001).
           05  BCERT02I PIC  X(0011).
      *    -------------------------------
           05  BEFFD02L PIC S9(0004) COMP.
           05  BEFFD02F PIC  X(0001).
           05  FILLER REDEFINES BEFFD02F.
               10  BEFFD02A PIC  X(0001).
           05  BEFFD02I PIC  X(0008).
      *    -------------------------------
           05  BLFI02L PIC S9(0004) COMP.
           05  BLFI02F PIC  X(0001).
           05  FILLER REDEFINES BLFI02F.
               10  BLFI02A PIC  X(0001).
           05  BLFI02I PIC  X(0045).
      *    -------------------------------
           05  BAHI02L PIC S9(0004) COMP.
           05  BAHI02F PIC  X(0001).
           05  FILLER REDEFINES BAHI02F.
               10  BAHI02A PIC  X(0001).
           05  BAHI02I PIC  X(0022).
      *    -------------------------------
           05  BAST03L PIC S9(0004) COMP.
           05  BAST03F PIC  X(0001).
           05  FILLER REDEFINES BAST03F.
               10  BAST03A PIC  X(0001).
           05  BAST03I PIC  X(0001).
      *    -------------------------------
           05  BCRTSL3L PIC S9(0004) COMP.
           05  BCRTSL3F PIC  X(0001).
           05  FILLER REDEFINES BCRTSL3F.
               10  BCRTSL3A PIC  X(0001).
           05  BCRTSL3I PIC  X(0001).
      *    -------------------------------
           05  BNAME03L PIC S9(0004) COMP.
           05  BNAME03F PIC  X(0001).
           05  FILLER REDEFINES BNAME03F.
               10  BNAME03A PIC  X(0001).
           05  BNAME03I PIC  X(0018).
      *    -------------------------------
           05  BAGE03L PIC S9(0004) COMP.
           05  BAGE03F PIC  X(0001).
           05  FILLER REDEFINES BAGE03F.
               10  BAGE03A PIC  X(0001).
           05  BAGE03I PIC  X(0002).
      *    -------------------------------
           05  BSEX03L PIC S9(0004) COMP.
           05  BSEX03F PIC  X(0001).
           05  FILLER REDEFINES BSEX03F.
               10  BSEX03A PIC  X(0001).
           05  BSEX03I PIC  X(0001).
      *    -------------------------------
           05  BCAR03L PIC S9(0004) COMP.
           05  BCAR03F PIC  X(0001).
           05  FILLER REDEFINES BCAR03F.
               10  BCAR03A PIC  X(0001).
           05  BCAR03I PIC  X(0001).
      *    -------------------------------
           05  BGRP03L PIC S9(0004) COMP.
           05  BGRP03F PIC  X(0001).
           05  FILLER REDEFINES BGRP03F.
               10  BGRP03A PIC  X(0001).
           05  BGRP03I PIC  X(0006).
      *    -------------------------------
           05  BST03L PIC S9(0004) COMP.
           05  BST03F PIC  X(0001).
           05  FILLER REDEFINES BST03F.
               10  BST03A PIC  X(0001).
           05  BST03I PIC  X(0002).
      *    -------------------------------
           05  BACCT03L PIC S9(0004) COMP.
           05  BACCT03F PIC  X(0001).
           05  FILLER REDEFINES BACCT03F.
               10  BACCT03A PIC  X(0001).
           05  BACCT03I PIC  X(0010).
      *    -------------------------------
           05  BCERT03L PIC S9(0004) COMP.
           05  BCERT03F PIC  X(0001).
           05  FILLER REDEFINES BCERT03F.
               10  BCERT03A PIC  X(0001).
           05  BCERT03I PIC  X(0011).
      *    -------------------------------
           05  BEFFD03L PIC S9(0004) COMP.
           05  BEFFD03F PIC  X(0001).
           05  FILLER REDEFINES BEFFD03F.
               10  BEFFD03A PIC  X(0001).
           05  BEFFD03I PIC  X(0008).
      *    -------------------------------
           05  BLFI03L PIC S9(0004) COMP.
           05  BLFI03F PIC  X(0001).
           05  FILLER REDEFINES BLFI03F.
               10  BLFI03A PIC  X(0001).
           05  BLFI03I PIC  X(0045).
      *    -------------------------------
           05  BAHI03L PIC S9(0004) COMP.
           05  BAHI03F PIC  X(0001).
           05  FILLER REDEFINES BAHI03F.
               10  BAHI03A PIC  X(0001).
           05  BAHI03I PIC  X(0022).
      *    -------------------------------
           05  BAST04L PIC S9(0004) COMP.
           05  BAST04F PIC  X(0001).
           05  FILLER REDEFINES BAST04F.
               10  BAST04A PIC  X(0001).
           05  BAST04I PIC  X(0001).
      *    -------------------------------
           05  BCRTSL4L PIC S9(0004) COMP.
           05  BCRTSL4F PIC  X(0001).
           05  FILLER REDEFINES BCRTSL4F.
               10  BCRTSL4A PIC  X(0001).
           05  BCRTSL4I PIC  X(0001).
      *    -------------------------------
           05  BNAME04L PIC S9(0004) COMP.
           05  BNAME04F PIC  X(0001).
           05  FILLER REDEFINES BNAME04F.
               10  BNAME04A PIC  X(0001).
           05  BNAME04I PIC  X(0018).
      *    -------------------------------
           05  BAGE04L PIC S9(0004) COMP.
           05  BAGE04F PIC  X(0001).
           05  FILLER REDEFINES BAGE04F.
               10  BAGE04A PIC  X(0001).
           05  BAGE04I PIC  X(0002).
      *    -------------------------------
           05  BSEX04L PIC S9(0004) COMP.
           05  BSEX04F PIC  X(0001).
           05  FILLER REDEFINES BSEX04F.
               10  BSEX04A PIC  X(0001).
           05  BSEX04I PIC  X(0001).
      *    -------------------------------
           05  BCAR04L PIC S9(0004) COMP.
           05  BCAR04F PIC  X(0001).
           05  FILLER REDEFINES BCAR04F.
               10  BCAR04A PIC  X(0001).
           05  BCAR04I PIC  X(0001).
      *    -------------------------------
           05  BGRP04L PIC S9(0004) COMP.
           05  BGRP04F PIC  X(0001).
           05  FILLER REDEFINES BGRP04F.
               10  BGRP04A PIC  X(0001).
           05  BGRP04I PIC  X(0006).
      *    -------------------------------
           05  BST04L PIC S9(0004) COMP.
           05  BST04F PIC  X(0001).
           05  FILLER REDEFINES BST04F.
               10  BST04A PIC  X(0001).
           05  BST04I PIC  X(0002).
      *    -------------------------------
           05  BACCT04L PIC S9(0004) COMP.
           05  BACCT04F PIC  X(0001).
           05  FILLER REDEFINES BACCT04F.
               10  BACCT04A PIC  X(0001).
           05  BACCT04I PIC  X(0010).
      *    -------------------------------
           05  BCERT04L PIC S9(0004) COMP.
           05  BCERT04F PIC  X(0001).
           05  FILLER REDEFINES BCERT04F.
               10  BCERT04A PIC  X(0001).
           05  BCERT04I PIC  X(0011).
      *    -------------------------------
           05  BEFFD04L PIC S9(0004) COMP.
           05  BEFFD04F PIC  X(0001).
           05  FILLER REDEFINES BEFFD04F.
               10  BEFFD04A PIC  X(0001).
           05  BEFFD04I PIC  X(0008).
      *    -------------------------------
           05  BLFI04L PIC S9(0004) COMP.
           05  BLFI04F PIC  X(0001).
           05  FILLER REDEFINES BLFI04F.
               10  BLFI04A PIC  X(0001).
           05  BLFI04I PIC  X(0045).
      *    -------------------------------
           05  BAHI04L PIC S9(0004) COMP.
           05  BAHI04F PIC  X(0001).
           05  FILLER REDEFINES BAHI04F.
               10  BAHI04A PIC  X(0001).
           05  BAHI04I PIC  X(0022).
      *    -------------------------------
           05  BAST05L PIC S9(0004) COMP.
           05  BAST05F PIC  X(0001).
           05  FILLER REDEFINES BAST05F.
               10  BAST05A PIC  X(0001).
           05  BAST05I PIC  X(0001).
      *    -------------------------------
           05  BCRTSL5L PIC S9(0004) COMP.
           05  BCRTSL5F PIC  X(0001).
           05  FILLER REDEFINES BCRTSL5F.
               10  BCRTSL5A PIC  X(0001).
           05  BCRTSL5I PIC  X(0001).
      *    -------------------------------
           05  BNAME05L PIC S9(0004) COMP.
           05  BNAME05F PIC  X(0001).
           05  FILLER REDEFINES BNAME05F.
               10  BNAME05A PIC  X(0001).
           05  BNAME05I PIC  X(0018).
      *    -------------------------------
           05  BAGE05L PIC S9(0004) COMP.
           05  BAGE05F PIC  X(0001).
           05  FILLER REDEFINES BAGE05F.
               10  BAGE05A PIC  X(0001).
           05  BAGE05I PIC  X(0002).
      *    -------------------------------
           05  BSEX05L PIC S9(0004) COMP.
           05  BSEX05F PIC  X(0001).
           05  FILLER REDEFINES BSEX05F.
               10  BSEX05A PIC  X(0001).
           05  BSEX05I PIC  X(0001).
      *    -------------------------------
           05  BCAR05L PIC S9(0004) COMP.
           05  BCAR05F PIC  X(0001).
           05  FILLER REDEFINES BCAR05F.
               10  BCAR05A PIC  X(0001).
           05  BCAR05I PIC  X(0001).
      *    -------------------------------
           05  BGRP05L PIC S9(0004) COMP.
           05  BGRP05F PIC  X(0001).
           05  FILLER REDEFINES BGRP05F.
               10  BGRP05A PIC  X(0001).
           05  BGRP05I PIC  X(0006).
      *    -------------------------------
           05  BST05L PIC S9(0004) COMP.
           05  BST05F PIC  X(0001).
           05  FILLER REDEFINES BST05F.
               10  BST05A PIC  X(0001).
           05  BST05I PIC  X(0002).
      *    -------------------------------
           05  BACCT05L PIC S9(0004) COMP.
           05  BACCT05F PIC  X(0001).
           05  FILLER REDEFINES BACCT05F.
               10  BACCT05A PIC  X(0001).
           05  BACCT05I PIC  X(0010).
      *    -------------------------------
           05  BCERT05L PIC S9(0004) COMP.
           05  BCERT05F PIC  X(0001).
           05  FILLER REDEFINES BCERT05F.
               10  BCERT05A PIC  X(0001).
           05  BCERT05I PIC  X(0011).
      *    -------------------------------
           05  BEFFD05L PIC S9(0004) COMP.
           05  BEFFD05F PIC  X(0001).
           05  FILLER REDEFINES BEFFD05F.
               10  BEFFD05A PIC  X(0001).
           05  BEFFD05I PIC  X(0008).
      *    -------------------------------
           05  BLFI05L PIC S9(0004) COMP.
           05  BLFI05F PIC  X(0001).
           05  FILLER REDEFINES BLFI05F.
               10  BLFI05A PIC  X(0001).
           05  BLFI05I PIC  X(0045).
      *    -------------------------------
           05  BAHI05L PIC S9(0004) COMP.
           05  BAHI05F PIC  X(0001).
           05  FILLER REDEFINES BAHI05F.
               10  BAHI05A PIC  X(0001).
           05  BAHI05I PIC  X(0022).
      *    -------------------------------
           05  BAST06L PIC S9(0004) COMP.
           05  BAST06F PIC  X(0001).
           05  FILLER REDEFINES BAST06F.
               10  BAST06A PIC  X(0001).
           05  BAST06I PIC  X(0001).
      *    -------------------------------
           05  BCRTSL6L PIC S9(0004) COMP.
           05  BCRTSL6F PIC  X(0001).
           05  FILLER REDEFINES BCRTSL6F.
               10  BCRTSL6A PIC  X(0001).
           05  BCRTSL6I PIC  X(0001).
      *    -------------------------------
           05  BNAME06L PIC S9(0004) COMP.
           05  BNAME06F PIC  X(0001).
           05  FILLER REDEFINES BNAME06F.
               10  BNAME06A PIC  X(0001).
           05  BNAME06I PIC  X(0018).
      *    -------------------------------
           05  BAGE06L PIC S9(0004) COMP.
           05  BAGE06F PIC  X(0001).
           05  FILLER REDEFINES BAGE06F.
               10  BAGE06A PIC  X(0001).
           05  BAGE06I PIC  X(0002).
      *    -------------------------------
           05  BSEX06L PIC S9(0004) COMP.
           05  BSEX06F PIC  X(0001).
           05  FILLER REDEFINES BSEX06F.
               10  BSEX06A PIC  X(0001).
           05  BSEX06I PIC  X(0001).
      *    -------------------------------
           05  BCAR06L PIC S9(0004) COMP.
           05  BCAR06F PIC  X(0001).
           05  FILLER REDEFINES BCAR06F.
               10  BCAR06A PIC  X(0001).
           05  BCAR06I PIC  X(0001).
      *    -------------------------------
           05  BGRP06L PIC S9(0004) COMP.
           05  BGRP06F PIC  X(0001).
           05  FILLER REDEFINES BGRP06F.
               10  BGRP06A PIC  X(0001).
           05  BGRP06I PIC  X(0006).
      *    -------------------------------
           05  BST06L PIC S9(0004) COMP.
           05  BST06F PIC  X(0001).
           05  FILLER REDEFINES BST06F.
               10  BST06A PIC  X(0001).
           05  BST06I PIC  X(0002).
      *    -------------------------------
           05  BACCT06L PIC S9(0004) COMP.
           05  BACCT06F PIC  X(0001).
           05  FILLER REDEFINES BACCT06F.
               10  BACCT06A PIC  X(0001).
           05  BACCT06I PIC  X(0010).
      *    -------------------------------
           05  BCERT06L PIC S9(0004) COMP.
           05  BCERT06F PIC  X(0001).
           05  FILLER REDEFINES BCERT06F.
               10  BCERT06A PIC  X(0001).
           05  BCERT06I PIC  X(0011).
      *    -------------------------------
           05  BEFFD06L PIC S9(0004) COMP.
           05  BEFFD06F PIC  X(0001).
           05  FILLER REDEFINES BEFFD06F.
               10  BEFFD06A PIC  X(0001).
           05  BEFFD06I PIC  X(0008).
      *    -------------------------------
           05  BLFI06L PIC S9(0004) COMP.
           05  BLFI06F PIC  X(0001).
           05  FILLER REDEFINES BLFI06F.
               10  BLFI06A PIC  X(0001).
           05  BLFI06I PIC  X(0045).
      *    -------------------------------
           05  BAHI06L PIC S9(0004) COMP.
           05  BAHI06F PIC  X(0001).
           05  FILLER REDEFINES BAHI06F.
               10  BAHI06A PIC  X(0001).
           05  BAHI06I PIC  X(0022).
      *    -------------------------------
           05  BAST07L PIC S9(0004) COMP.
           05  BAST07F PIC  X(0001).
           05  FILLER REDEFINES BAST07F.
               10  BAST07A PIC  X(0001).
           05  BAST07I PIC  X(0001).
      *    -------------------------------
           05  BCRTSL7L PIC S9(0004) COMP.
           05  BCRTSL7F PIC  X(0001).
           05  FILLER REDEFINES BCRTSL7F.
               10  BCRTSL7A PIC  X(0001).
           05  BCRTSL7I PIC  X(0001).
      *    -------------------------------
           05  BNAME07L PIC S9(0004) COMP.
           05  BNAME07F PIC  X(0001).
           05  FILLER REDEFINES BNAME07F.
               10  BNAME07A PIC  X(0001).
           05  BNAME07I PIC  X(0018).
      *    -------------------------------
           05  BAGE07L PIC S9(0004) COMP.
           05  BAGE07F PIC  X(0001).
           05  FILLER REDEFINES BAGE07F.
               10  BAGE07A PIC  X(0001).
           05  BAGE07I PIC  X(0002).
      *    -------------------------------
           05  BSEX07L PIC S9(0004) COMP.
           05  BSEX07F PIC  X(0001).
           05  FILLER REDEFINES BSEX07F.
               10  BSEX07A PIC  X(0001).
           05  BSEX07I PIC  X(0001).
      *    -------------------------------
           05  BCAR07L PIC S9(0004) COMP.
           05  BCAR07F PIC  X(0001).
           05  FILLER REDEFINES BCAR07F.
               10  BCAR07A PIC  X(0001).
           05  BCAR07I PIC  X(0001).
      *    -------------------------------
           05  BGRP07L PIC S9(0004) COMP.
           05  BGRP07F PIC  X(0001).
           05  FILLER REDEFINES BGRP07F.
               10  BGRP07A PIC  X(0001).
           05  BGRP07I PIC  X(0006).
      *    -------------------------------
           05  BST07L PIC S9(0004) COMP.
           05  BST07F PIC  X(0001).
           05  FILLER REDEFINES BST07F.
               10  BST07A PIC  X(0001).
           05  BST07I PIC  X(0002).
      *    -------------------------------
           05  BACCT07L PIC S9(0004) COMP.
           05  BACCT07F PIC  X(0001).
           05  FILLER REDEFINES BACCT07F.
               10  BACCT07A PIC  X(0001).
           05  BACCT07I PIC  X(0010).
      *    -------------------------------
           05  BCERT07L PIC S9(0004) COMP.
           05  BCERT07F PIC  X(0001).
           05  FILLER REDEFINES BCERT07F.
               10  BCERT07A PIC  X(0001).
           05  BCERT07I PIC  X(0011).
      *    -------------------------------
           05  BEFFD07L PIC S9(0004) COMP.
           05  BEFFD07F PIC  X(0001).
           05  FILLER REDEFINES BEFFD07F.
               10  BEFFD07A PIC  X(0001).
           05  BEFFD07I PIC  X(0008).
      *    -------------------------------
           05  BLFI07L PIC S9(0004) COMP.
           05  BLFI07F PIC  X(0001).
           05  FILLER REDEFINES BLFI07F.
               10  BLFI07A PIC  X(0001).
           05  BLFI07I PIC  X(0045).
      *    -------------------------------
           05  BAHI07L PIC S9(0004) COMP.
           05  BAHI07F PIC  X(0001).
           05  FILLER REDEFINES BAHI07F.
               10  BAHI07A PIC  X(0001).
           05  BAHI07I PIC  X(0022).
      *    -------------------------------
           05  BAST08L PIC S9(0004) COMP.
           05  BAST08F PIC  X(0001).
           05  FILLER REDEFINES BAST08F.
               10  BAST08A PIC  X(0001).
           05  BAST08I PIC  X(0001).
      *    -------------------------------
           05  BCRTSL8L PIC S9(0004) COMP.
           05  BCRTSL8F PIC  X(0001).
           05  FILLER REDEFINES BCRTSL8F.
               10  BCRTSL8A PIC  X(0001).
           05  BCRTSL8I PIC  X(0001).
      *    -------------------------------
           05  BNAME08L PIC S9(0004) COMP.
           05  BNAME08F PIC  X(0001).
           05  FILLER REDEFINES BNAME08F.
               10  BNAME08A PIC  X(0001).
           05  BNAME08I PIC  X(0018).
      *    -------------------------------
           05  BAGE08L PIC S9(0004) COMP.
           05  BAGE08F PIC  X(0001).
           05  FILLER REDEFINES BAGE08F.
               10  BAGE08A PIC  X(0001).
           05  BAGE08I PIC  X(0002).
      *    -------------------------------
           05  BSEX08L PIC S9(0004) COMP.
           05  BSEX08F PIC  X(0001).
           05  FILLER REDEFINES BSEX08F.
               10  BSEX08A PIC  X(0001).
           05  BSEX08I PIC  X(0001).
      *    -------------------------------
           05  BCAR08L PIC S9(0004) COMP.
           05  BCAR08F PIC  X(0001).
           05  FILLER REDEFINES BCAR08F.
               10  BCAR08A PIC  X(0001).
           05  BCAR08I PIC  X(0001).
      *    -------------------------------
           05  BGRP08L PIC S9(0004) COMP.
           05  BGRP08F PIC  X(0001).
           05  FILLER REDEFINES BGRP08F.
               10  BGRP08A PIC  X(0001).
           05  BGRP08I PIC  X(0006).
      *    -------------------------------
           05  BST08L PIC S9(0004) COMP.
           05  BST08F PIC  X(0001).
           05  FILLER REDEFINES BST08F.
               10  BST08A PIC  X(0001).
           05  BST08I PIC  X(0002).
      *    -------------------------------
           05  BACCT08L PIC S9(0004) COMP.
           05  BACCT08F PIC  X(0001).
           05  FILLER REDEFINES BACCT08F.
               10  BACCT08A PIC  X(0001).
           05  BACCT08I PIC  X(0010).
      *    -------------------------------
           05  BCERT08L PIC S9(0004) COMP.
           05  BCERT08F PIC  X(0001).
           05  FILLER REDEFINES BCERT08F.
               10  BCERT08A PIC  X(0001).
           05  BCERT08I PIC  X(0011).
      *    -------------------------------
           05  BEFFD08L PIC S9(0004) COMP.
           05  BEFFD08F PIC  X(0001).
           05  FILLER REDEFINES BEFFD08F.
               10  BEFFD08A PIC  X(0001).
           05  BEFFD08I PIC  X(0008).
      *    -------------------------------
           05  BLFI08L PIC S9(0004) COMP.
           05  BLFI08F PIC  X(0001).
           05  FILLER REDEFINES BLFI08F.
               10  BLFI08A PIC  X(0001).
           05  BLFI08I PIC  X(0045).
      *    -------------------------------
           05  BAHI08L PIC S9(0004) COMP.
           05  BAHI08F PIC  X(0001).
           05  FILLER REDEFINES BAHI08F.
               10  BAHI08A PIC  X(0001).
           05  BAHI08I PIC  X(0022).
      *    -------------------------------
           05  BEMSG1L PIC S9(0004) COMP.
           05  BEMSG1F PIC  X(0001).
           05  FILLER REDEFINES BEMSG1F.
               10  BEMSG1A PIC  X(0001).
           05  BEMSG1I PIC  X(0079).
      *    -------------------------------
           05  BEMSG2L PIC S9(0004) COMP.
           05  BEMSG2F PIC  X(0001).
           05  FILLER REDEFINES BEMSG2F.
               10  BEMSG2A PIC  X(0001).
           05  BEMSG2I PIC  X(0079).
      *    -------------------------------
           05  BSELL PIC S9(0004) COMP.
           05  BSELF PIC  X(0001).
           05  FILLER REDEFINES BSELF.
               10  BSELA PIC  X(0001).
           05  BSELI PIC  9.
      *    -------------------------------
           05  BPFKL PIC S9(0004) COMP.
           05  BPFKF PIC  X(0001).
           05  FILLER REDEFINES BPFKF.
               10  BPFKA PIC  X(0001).
           05  BPFKI PIC  99.
       01  EL128BO REDEFINES EL128BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCOMPO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAST01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCRTSL1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME01O PIC  X(0018).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE01O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEX01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCAR01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRP01O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST01O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT01O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT01O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEFFD01O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BLFI01O PIC  X(0045).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAHI01O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAST02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCRTSL2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME02O PIC  X(0018).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE02O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEX02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCAR02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRP02O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST02O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT02O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT02O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEFFD02O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BLFI02O PIC  X(0045).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAHI02O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAST03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCRTSL3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME03O PIC  X(0018).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE03O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEX03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCAR03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRP03O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST03O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT03O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT03O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEFFD03O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BLFI03O PIC  X(0045).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAHI03O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAST04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCRTSL4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME04O PIC  X(0018).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE04O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEX04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCAR04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRP04O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST04O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT04O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT04O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEFFD04O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BLFI04O PIC  X(0045).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAHI04O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAST05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCRTSL5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME05O PIC  X(0018).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE05O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEX05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCAR05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRP05O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST05O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT05O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT05O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEFFD05O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BLFI05O PIC  X(0045).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAHI05O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAST06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCRTSL6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME06O PIC  X(0018).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE06O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEX06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCAR06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRP06O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST06O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT06O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT06O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEFFD06O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BLFI06O PIC  X(0045).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAHI06O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAST07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCRTSL7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME07O PIC  X(0018).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE07O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEX07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCAR07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRP07O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST07O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT07O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT07O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEFFD07O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BLFI07O PIC  X(0045).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAHI07O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAST08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCRTSL8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME08O PIC  X(0018).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE08O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEX08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCAR08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRP08O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST08O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT08O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT08O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEFFD08O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BLFI08O PIC  X(0045).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAHI08O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSELO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFKO PIC  X(0002).
      *    -------------------------------
       01  EL128AI REDEFINES EL128BI.
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
           05  ACOMPL PIC S9(0004) COMP.
           05  ACOMPF PIC  X(0001).
           05  FILLER REDEFINES ACOMPF.
               10  ACOMPA PIC  X(0001).
           05  ACOMPI PIC  X(0003).
      *    -------------------------------
           05  ACRTNO4L PIC S9(0004) COMP.
           05  ACRTNO4F PIC  X(0001).
           05  FILLER REDEFINES ACRTNO4F.
               10  ACRTNO4A PIC  X(0001).
           05  ACRTNO4I PIC  X(0010).
      *    -------------------------------
           05  ACRTSX4L PIC S9(0004) COMP.
           05  ACRTSX4F PIC  X(0001).
           05  FILLER REDEFINES ACRTSX4F.
               10  ACRTSX4A PIC  X(0001).
           05  ACRTSX4I PIC  X(0001).
      *    -------------------------------
           05  ACARIERL PIC S9(0004) COMP.
           05  ACARIERF PIC  X(0001).
           05  FILLER REDEFINES ACARIERF.
               10  ACARIERA PIC  X(0001).
           05  ACARIERI PIC  X(0001).
      *    -------------------------------
           05  AGROUPL PIC S9(0004) COMP.
           05  AGROUPF PIC  X(0001).
           05  FILLER REDEFINES AGROUPF.
               10  AGROUPA PIC  X(0001).
           05  AGROUPI PIC  X(0006).
      *    -------------------------------
           05  ASTATEL PIC S9(0004) COMP.
           05  ASTATEF PIC  X(0001).
           05  FILLER REDEFINES ASTATEF.
               10  ASTATEA PIC  X(0001).
           05  ASTATEI PIC  X(0002).
      *    -------------------------------
           05  AACCTNOL PIC S9(0004) COMP.
           05  AACCTNOF PIC  X(0001).
           05  FILLER REDEFINES AACCTNOF.
               10  AACCTNOA PIC  X(0001).
           05  AACCTNOI PIC  X(0010).
      *    -------------------------------
           05  AEDATEL PIC S9(0004) COMP.
           05  AEDATEF PIC  X(0001).
           05  FILLER REDEFINES AEDATEF.
               10  AEDATEA PIC  X(0001).
           05  AEDATEI PIC  X(0008).
      *    -------------------------------
           05  ACERTNOL PIC S9(0004) COMP.
           05  ACERTNOF PIC  X(0001).
           05  FILLER REDEFINES ACERTNOF.
               10  ACERTNOA PIC  X(0001).
           05  ACERTNOI PIC  X(0010).
      *    -------------------------------
           05  ACERTSXL PIC S9(0004) COMP.
           05  ACERTSXF PIC  X(0001).
           05  FILLER REDEFINES ACERTSXF.
               10  ACERTSXA PIC  X(0001).
           05  ACERTSXI PIC  X(0001).
      *    -------------------------------
           05  ALNAMEL PIC S9(0004) COMP.
           05  ALNAMEF PIC  X(0001).
           05  FILLER REDEFINES ALNAMEF.
               10  ALNAMEA PIC  X(0001).
           05  ALNAMEI PIC  X(0015).
      *    -------------------------------
           05  AFNAMEL PIC S9(0004) COMP.
           05  AFNAMEF PIC  X(0001).
           05  FILLER REDEFINES AFNAMEF.
               10  AFNAMEA PIC  X(0001).
           05  AFNAMEI PIC  X(0015).
      *    -------------------------------
           05  AINITALL PIC S9(0004) COMP.
           05  AINITALF PIC  X(0001).
           05  FILLER REDEFINES AINITALF.
               10  AINITALA PIC  X(0001).
           05  AINITALI PIC  X(0001).
      *    -------------------------------
           05  AACCT2L PIC S9(0004) COMP.
           05  AACCT2F PIC  X(0001).
           05  FILLER REDEFINES AACCT2F.
               10  AACCT2A PIC  X(0001).
           05  AACCT2I PIC  X(0010).
      *    -------------------------------
           05  ACARRL PIC S9(0004) COMP.
           05  ACARRF PIC  X(0001).
           05  FILLER REDEFINES ACARRF.
               10  ACARRA PIC  X(0001).
           05  ACARRI PIC  X(0001).
      *    -------------------------------
           05  AOPT4L PIC S9(0004) COMP.
           05  AOPT4F PIC  X(0001).
           05  FILLER REDEFINES AOPT4F.
               10  AOPT4A PIC  X(0001).
           05  AOPT4I PIC  X(0014).
      *    -------------------------------
           05  ASSOPTL PIC S9(0004) COMP.
           05  ASSOPTF PIC  X(0001).
           05  FILLER REDEFINES ASSOPTF.
               10  ASSOPTA PIC  X(0001).
           05  ASSOPTI PIC  X(0023).
      *    -------------------------------
           05  ASSNL PIC S9(0004) COMP.
           05  ASSNF PIC  X(0001).
           05  FILLER REDEFINES ASSNF.
               10  ASSNA PIC  X(0001).
           05  ASSNI PIC  X(0011).
      *    -------------------------------
           05  AOPT5L PIC S9(0004) COMP.
           05  AOPT5F PIC  X(0001).
           05  FILLER REDEFINES AOPT5F.
               10  AOPT5A PIC  X(0001).
           05  AOPT5I PIC  X(0014).
      *    -------------------------------
           05  AMEOPTL PIC S9(0004) COMP.
           05  AMEOPTF PIC  X(0001).
           05  FILLER REDEFINES AMEOPTF.
               10  AMEOPTA PIC  X(0001).
           05  AMEOPTI PIC  X(0014).
      *    -------------------------------
           05  AMEMBERL PIC S9(0004) COMP.
           05  AMEMBERF PIC  X(0001).
           05  FILLER REDEFINES AMEMBERF.
               10  AMEMBERA PIC  X(0001).
           05  AMEMBERI PIC  X(0012).
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
           05  APFKI PIC  99.
       01  EL128AO REDEFINES EL128BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACOMPO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACRTNO4O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACRTSX4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEDATEO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERTSXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNAMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFNAMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AINITALO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AOPT4O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASSOPTO PIC  X(0023).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASSNO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AOPT5O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMEOPTO PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMEMBERO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFKO PIC  99.
      *    -------------------------------
           EJECT
      *                                    COPY ELCATTR.
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
           EJECT
      *                                    COPY ELCAID.
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
       01  FILLER                      REDEFINES
           DFHAID.
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
      *01 PARMLIST                         COMP
      *                                    SYNC.
      *    05  FILLER                      PIC S9(9).
      *    05  ELPURG-POINTER              PIC S9(9).
      *    05  ERACCT-POINTER              PIC S9(9).
      *    05  ELCNTL-POINTER              PIC S9(9).
           EJECT
      *                                    COPY ELCPURG.
      ******************************************************************
      *                                                                *
      *                            ELCPURG.                            *
      *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
      *                            VMOD=2.001                          *
      *                                                                *
      *   FILE DESCRIPTION = CERTIFICATE MASTER                        *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 230  RECFORM = FIXED                           *
      *                                                                *
      *   BASE CLUSTER = ELPURG                         RKP=2,LEN=33   *
      *       ALTERNATE PATH1 = ELPURG2 (BY NAME)       RKP=35,LEN=18  *
      *       ALTERNATE PATH2 = ELPURG3 (BY SOC SEC NO) RKP=53,LEN=12  *
      *       ALTERNATE PATH3 = ELPURG5 (BY CERT NO.)   RKP=65,LEN=12  *
      *       ALTERNATE PATH4 = ELPURG6 (BY MEMBER NO.) RKP=77,LEN=13  *
      *                                                                *
      *   LOG = YES                                                    *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      *
      *  NO  CID  MODS  TO  COPYBOOK  ELCPURG
      ******************************************************************
       01  PURGE-CERT-MASTER.
           12  PG-RECORD-ID                      PIC XX.
               88  VALID-PG-ID                      VALUE 'PG'.
           12  PG-CONTROL-PRIMARY.
               16  PG-COMPANY-CD                 PIC X.
               16  PG-CARRIER                    PIC X.
               16  PG-GROUPING.
                   20  PG-GROUPING-PREFIX        PIC X(3).
                   20  PG-GROUPING-PRIME         PIC X(3).
               16  PG-STATE                      PIC XX.
               16  PG-ACCOUNT.
                   20  PG-ACCOUNT-PREFIX         PIC X(4).
                   20  PG-ACCOUNT-PRIME          PIC X(6).
               16  PG-CERT-EFF-DT                PIC XX.
               16  PG-CERT-NO.
                   20  PG-CERT-PRIME             PIC X(10).
                   20  PG-CERT-SFX               PIC X.
           12  PG-CONTROL-BY-NAME.
               16  PG-COMPANY-CD-A1              PIC X.
               16  PG-INSURED-LAST-NAME          PIC X(15).
               16  PG-INSURED-INITIALS.
                   20  PG-INSURED-INITIAL1       PIC X.
                   20  PG-INSURED-INITIAL2       PIC X.
           12  PG-CONTROL-BY-SSN.
               16  PG-COMPANY-CD-A2              PIC X.
               16  PG-SOC-SEC-NO.
                   20  PG-SSN-STATE              PIC XX.
                   20  PG-SSN-ACCOUNT            PIC X(6).
                   20  PG-SSN-LN3.
                       25  PG-INSURED-INITIALS-A2.
                           30 PG-INSURED-INITIAL1-A2   PIC X.
                           30 PG-INSURED-INITIAL2-A2   PIC X.
                       25 PG-PART-LAST-NAME-A2         PIC X.
           12  PG-CONTROL-BY-CERT-NO.
               16  PG-COMPANY-CD-A4              PIC X.
               16  PG-CERT-NO-A4                 PIC X(11).
           12  PG-CONTROL-BY-MEMB.
               16  PG-COMPANY-CD-A5              PIC X.
               16  PG-MEMBER-NO.
                   20  PG-MEMB-STATE             PIC XX.
                   20  PG-MEMB-ACCOUNT           PIC X(6).
                   20  PG-MEMB-LN4.
                       25  PG-INSURED-INITIALS-A5.
                           30 PG-INSURED-INITIAL1-A5   PIC X.
                           30 PG-INSURED-INITIAL2-A5   PIC X.
                       25 PG-PART-LAST-NAME-A5         PIC XX.
           12  PG-INSURED-PROFILE-DATA.
               16  PG-INSURED-FIRST-NAME.
                   20  PG-INSURED-1ST-INIT       PIC X.
                   20  FILLER                    PIC X(9).
               16  PG-INSURED-ISSUE-AGE          PIC 99.
               16  PG-INSURED-SEX                PIC X.
                   88  PG-SEX-MALE                  VALUE 'M'.
                   88  PG-SEX-FEMAL                 VALUE 'F'.
               16  PG-INSURED-JOINT-AGE          PIC 99.
               16  PG-JOINT-INSURED-NAME.
                   20  PG-JT-LAST-NAME           PIC X(15).
                   20  PG-JT-FIRST-NAME.
                       24  PG-JT-1ST-INIT        PIC X.
                       24  FILLER                PIC X(9).
                   20  PG-JT-INITIAL             PIC X.
           12  PG-LIFE-DATA.
               16  PG-LF-BENEFIT-CD              PIC XX.
               16  PG-LF-ORIG-TERM               PIC S999      COMP-3.
               16  PG-LF-BENEFIT-AMT             PIC S9(9)V99  COMP-3.
               16  PG-LF-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
               16  PG-LF-ALT-BENEFIT-AMT         PIC S9(9)V99  COMP-3.
               16  PG-LF-ALT-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
           12  PG-AH-DATA.
               16  PG-AH-BENEFIT-CD              PIC XX.
               16  PG-AH-ORIG-TERM               PIC S999      COMP-3.
               16  PG-AH-BENEFIT-AMT             PIC S9(7)V99  COMP-3.
               16  PG-AH-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
           12  PG-LOAN-INFORMATION.
               16  PG-LOAN-APR                   PIC S999V9(4) COMP-3.
               16  PG-LOAN-TERM                  PIC S999      COMP-3.
               16  PG-PMT-EXTENSION-DAYS         PIC S999      COMP-3.
               16  PG-IND-GRP-TYPE               PIC X.
                   88  PG-INDIVIDUAL                VALUE 'I'.
                   88  PG-GROUP                     VALUE 'G'.
               16  PG-REIN-TABLE                 PIC XXX.
               16  PG-LF-LOAN-EXPIRE-DT          PIC XX.
               16  PG-AH-LOAN-EXPIRE-DT          PIC XX.
               16  PG-LOAN-1ST-PMT-DT            PIC XX.
           12  PG-STATUS-DATA.
               16  PG-ENTRY-STATUS               PIC X.
               16  PG-ENTRY-DT                   PIC XX.
               16  PG-LF-STATUS-AT-CANCEL        PIC X.
               16  PG-LF-CANCEL-DT               PIC XX.
               16  PG-LF-CANCEL-EXIT-DT          PIC XX.
               16  PG-LF-STATUS-AT-DEATH         PIC X.
               16  PG-LF-DEATH-DT                PIC XX.
               16  PG-LF-DEATH-EXIT-DT           PIC XX.
               16  PG-LF-CURRENT-STATUS          PIC X.
                   88  PG-LF-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
                                                          '4' '5' '9'.
                   88  PG-LF-NORMAL-ENTRY           VALUE '1'.
                   88  PG-LF-POLICY-PENDING         VALUE '2'.
                   88  PG-LF-POLICY-IS-RESTORE      VALUE '3'.
                   88  PG-LF-CONVERSION-ENTRY       VALUE '4'.
                   88  PG-LF-POLICY-IS-REISSUE      VALUE '5'.
                   88  PG-LF-LUMP-SUM-DISAB         VALUE '6'.
                   88  PG-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
                   88  PG-LF-CANCEL-APPLIED         VALUE '8'.
                   88  PG-LF-IS-REIN-ONLY           VALUE '9'.
                   88  PG-LF-DECLINED               VALUE 'D'.
                   88  PG-LF-VOIDED                 VALUE 'V'.
               16  PG-AH-STATUS-AT-CANCEL        PIC X.
               16  PG-AH-CANCEL-DT               PIC XX.
               16  PG-AH-CANCEL-EXIT-DT          PIC XX.
               16  PG-AH-STATUS-AT-SETTLEMENT    PIC X.
               16  PG-AH-SETTLEMENT-DT           PIC XX.
               16  PG-AH-SETTLEMENT-EXIT-DT      PIC XX.
               16  PG-AH-CURRENT-STATUS          PIC X.
                   88  PG-AH-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
                                                          '4' '5' '9'.
                   88  PG-AH-NORMAL-ENTRY           VALUE '1'.
                   88  PG-AH-POLICY-PENDING         VALUE '2'.
                   88  PG-AH-POLICY-IS-RESTORE      VALUE '3'.
                   88  PG-AH-CONVERSION-ENTRY       VALUE '4'.
                   88  PG-AH-POLICY-IS-REISSUE      VALUE '5'.
                   88  PG-AH-LUMP-SUM-DISAB         VALUE '6'.
                   88  PG-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
                   88  PG-AH-CANCEL-APPLIED         VALUE '8'.
                   88  PG-AH-IS-REIN-ONLY           VALUE '9'.
                   88  PG-AH-DECLINED               VALUE 'D'.
                   88  PG-AH-VOIDED                 VALUE 'V'.
               16  PG-ENTRY-BATCH                PIC X(6).
           12  PG-USER-FIELD                     PIC X.
           12  PG-USER-RESERVED                  PIC X(9).
      ******************************************************************
           EJECT
      *                                    COPY ERCACCT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCACCT                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.031                          *
00006 *                                                                *
00007 *   CREDIT SYSTEM ACCOUNT MASTER FILE                            *
00008 *                                                                *
00009 *   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
00010 *   VSAM ACCOUNT MASTER FILES.                                   *
00011 *                                                                *
00012 *   FILE DESCRIPTION = ACCOUNT OR PRODUCER FILES                 *
00013 *                                                                *
00014 *   FILE TYPE = VSAM,KSDS                                        *
00015 *   RECORD SIZE = 2000  RECFORM = FIX                            *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ERACCT                    RKP=2,LEN=26   *
00018 *       ALTERNATE PATH1 = ERACCT2 (ALT GROUPING) RKP=28,LEN=26   *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
102004*                   C H A N G E   L O G
102004*
102004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102004*-----------------------------------------------------------------
102004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102004* EFFECTIVE    NUMBER
102004*-----------------------------------------------------------------
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
022808* 022808    2007083100002  PEMA  ADD FREEZE STATUS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
102004******************************************************************
00025
00026  01  ACCOUNT-MASTER.
00027      12  AM-RECORD-ID                      PIC XX.
00028          88  VALID-AM-ID                      VALUE 'AM'.
00029
00030      12  AM-CONTROL-PRIMARY.
00031          16  AM-COMPANY-CD                 PIC X.
00032          16  AM-MSTR-CNTRL.
00033              20  AM-CONTROL-A.
00034                  24  AM-CARRIER            PIC X.
00035                  24  AM-GROUPING.
00036                      28 AM-GROUPING-PREFIX PIC XXX.
00037                      28 AM-GROUPING-PRIME  PIC XXX.
00038                  24  AM-STATE              PIC XX.
00039                  24  AM-ACCOUNT.
00040                      28  AM-ACCOUNT-PREFIX PIC X(4).
00041                      28  AM-ACCOUNT-PRIME  PIC X(6).
00042              20  AM-CNTRL-1   REDEFINES   AM-CONTROL-A
00043                                            PIC X(19).
00044              20  AM-CNTRL-B.
00045                  24  AM-EXPIRATION-DT      PIC XX.
00046                  24  FILLER                PIC X(4).
00047              20  AM-CNTRL-2 REDEFINES AM-CNTRL-B.
00048                  24  AM-EXPIRE-DT          PIC 9(11)  COMP-3.
00049
00050      12  AM-CONTROL-BY-VAR-GRP.
00051          16  AM-COMPANY-CD-A1              PIC X.
00052          16  AM-VG-CARRIER                 PIC X.
00053          16  AM-VG-GROUPING                PIC X(6).
00054          16  AM-VG-STATE                   PIC XX.
00055          16  AM-VG-ACCOUNT                 PIC X(10).
00056          16  AM-VG-DATE.
00057              20  AM-VG-EXPIRATION-DT       PIC XX.
00058              20  FILLER                    PIC X(4).
00059          16  AM-VG-EXP-DATE REDEFINES AM-VG-DATE
00060                                            PIC 9(11)      COMP-3.
00061      12  AM-MAINT-INFORMATION.
00062          16  AM-LAST-MAINT-DT              PIC XX.
00063          16  AM-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00064          16  AM-LAST-MAINT-USER            PIC X(4).
00065          16  FILLER                        PIC XX.
00066
00067      12  AM-EFFECTIVE-DT                   PIC XX.
00068      12  AM-EFFECT-DT                      PIC 9(11)      COMP-3.
00069
00070      12  AM-PREV-DATES  COMP-3.
00071          16  AM-PREV-EXP-DT                PIC 9(11).
00072          16  AM-PREV-EFF-DT                PIC 9(11).
00073
00074      12  AM-REPORT-CODE-1                  PIC X(10).
00075      12  AM-REPORT-CODE-2                  PIC X(10).
00076
00077      12  AM-CITY-CODE                      PIC X(4).
00078      12  AM-COUNTY-PARISH                  PIC X(6).
00079
00080      12  AM-NAME                           PIC X(30).
00081      12  AM-PERSON                         PIC X(30).
00082      12  AM-ADDRS                          PIC X(30).
00083      12  AM-CITY.
               16  AM-ADDR-CITY                  PIC X(28).
               16  AM-ADDR-STATE                 PIC XX.
00084      12  AM-ZIP.
00085          16  AM-ZIP-PRIME.
00086              20  AM-ZIP-PRI-1ST            PIC X.
00087                  88  AM-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
00088              20  FILLER                    PIC X(4).
00089          16  AM-ZIP-PLUS4                  PIC X(4).
00090      12  AM-CANADIAN-POSTAL-CODE  REDEFINES  AM-ZIP.
00091          16  AM-CAN-POSTAL-1               PIC XXX.
00092          16  AM-CAN-POSTAL-2               PIC XXX.
00093          16  FILLER                        PIC XXX.
00094      12  AM-TEL-NO.
00095          16  AM-AREA-CODE                  PIC 999.
00096          16  AM-TEL-PRE                    PIC 999.
00097          16  AM-TEL-NBR                    PIC 9(4).
00098      12  AM-TEL-LOC                        PIC X.
00099          88  AM-TEL-AT-HOME                   VALUE 'H'.
00100          88  AM-TEL-AT-BUSINESS               VALUE 'B'.
00101
00102      12  AM-COMM-STRUCTURE.
00103          16  AM-DEFN-1.
00104              20  AM-AGT-COMMS       OCCURS 10 TIMES.
00105                  24  AM-AGT.
00106                      28  AM-AGT-PREFIX     PIC X(4).
00107                      28  AM-AGT-PRIME      PIC X(6).
00108                  24  AM-COM-TYP            PIC X.
00109                  24  AM-L-COM              PIC SV9(5)     COMP-3.
00110                  24  AM-J-COM              PIC SV9(5)     COMP-3.
00111                  24  AM-A-COM              PIC SV9(5)     COMP-3.
00112                  24  AM-RECALC-LV-INDIC    PIC X.
00113                  24  AM-RETRO-LV-INDIC     PIC X.
00114                  24  AM-GL-CODES           PIC X.
00115                  24  AM-COMM-CHARGEBACK    PIC 9(02).
00116                  24  FILLER                PIC X(01).
00117          16  AM-DEFN-2   REDEFINES   AM-DEFN-1.
00118              20  AM-COM-TBLS        OCCURS 10 TIMES.
00119                  24  FILLER                PIC X(11).
00120                  24  AM-L-COMA             PIC XXX.
00121                  24  AM-J-COMA             PIC XXX.
00122                  24  AM-A-COMA             PIC XXX.
00123                  24  FILLER                PIC X(6).
00124
00125      12  AM-COMM-CHANGE-STATUS             PIC X.
00126          88  AM-COMMISSIONS-CHANGED           VALUE '*'.
00127
00128      12  AM-CSR-CODE                       PIC X(4).
00129
00130      12  AM-BILLING-STATUS                 PIC X.
00131          88  AM-ACCOUNT-BILLED                VALUE 'B'.
00132          88  AM-ACCOUNT-NOT-BILLED            VALUE ' '.
00133      12  AM-AUTO-REFUND-SW                 PIC X.
00134          88  AUTO-REFUNDS-USED                VALUE 'Y'.
00135          88  AUTO-REFUNDS-NOT-USED            VALUE 'N' ' '.
00136      12  AM-GPCD                           PIC 99.
00137      12  AM-IG                             PIC X.
00138          88  AM-HAS-INDIVIDUAL                VALUE '1'.
00139          88  AM-HAS-GROUP                     VALUE '2'.
00140      12  AM-STATUS                         PIC X.
00141          88  AM-ACCOUNT-ACTIVE                VALUE '0'.
00142          88  AM-ACCOUNT-INACTIVE              VALUE '1'.
00143          88  AM-ACCOUNT-TRANSFERRED           VALUE '2'.
102004         88  AM-ACCOUNT-CANCELLED             VALUE '3'.
022808         88  AM-ACCOUNT-FROZEN                VALUE '4'.
00144      12  AM-REMIT-TO                       PIC 99.
00145      12  AM-ID-NO                          PIC X(11).
00146
00147      12  AM-CAL-TABLE                      PIC XX.
00148      12  AM-LF-DEVIATION                   PIC XXX.
00149      12  AM-AH-DEVIATION                   PIC XXX.
00150      12  AM-LF-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00151      12  AM-AH-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00152      12  AM-LF-OB-RATE                     PIC S99V9(5)   COMP-3.
00153      12  AM-AH-OB-RATE                     PIC S99V9(5)   COMP-3.
00154      12  AM-LF-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00155      12  AM-AH-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00156
00157      12  AM-USER-FIELDS.
00158          16  AM-FLD-1                      PIC XX.
00159          16  AM-FLD-2                      PIC XX.
00160          16  AM-FLD-3                      PIC XX.
00161          16  AM-FLD-4                      PIC XX.
00162          16  AM-FLD-5                      PIC XX.
00163
00164      12  AM-1ST-PROD-DATE.
00165          16  AM-1ST-PROD-YR                PIC XX.
00166          16  AM-1ST-PROD-MO                PIC XX.
00167          16  AM-1ST-PROD-DA                PIC XX.
00168      12  AM-ANNIVERSARY-DATE               PIC 9(11)  COMP-3.
00169      12  AM-CERTS-PURGED-DATE.
00170          16  AM-PUR-YR                     PIC XX.
00171          16  AM-PUR-MO                     PIC XX.
00172          16  AM-PUR-DA                     PIC XX.
00173      12  AM-HI-CERT-DATE                   PIC 9(11)  COMP-3.
00174      12  AM-LO-CERT-DATE                   PIC 9(11)  COMP-3.
00175      12  AM-ENTRY-DATE                     PIC 9(11)  COMP-3.
00176      12  AM-INACTIVE-DATE.
00177          16  AM-INA-MO                     PIC 99.
00178          16  AM-INA-DA                     PIC 99.
00179          16  AM-INA-YR                     PIC 99.
00180      12  AM-AR-HI-CERT-DATE                PIC XX.
00181
00182      12  AM-LF-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00183      12  AM-AH-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00184
00185      12  AM-OB-PAYMENT-MODE                PIC X.
00186          88  AM-OB-PAID-MONTHLY               VALUE 'M' ' '.
00187          88  AM-OB-PAID-QUARTERLY             VALUE 'Q'.
00188          88  AM-OB-PAID-SEMI-ANNUALLY         VALUE 'S'.
00189          88  AM-OB-PAID-ANNUALLY              VALUE 'A'.
00190
00191      12  AM-AH-ONLY-INDICATOR              PIC X.
00192          88  AM-AH-ONLY-ALLOWED               VALUE 'Y' ' '.
00193          88  AM-NO-AH-ONLY                    VALUE 'N'.
00194
00195      12  AM-EDIT-LOAN-OFC                  PIC X(01).
00196
00197      12  AM-OVER-SHORT.
00198          16 AM-OVR-SHT-AMT                 PIC S999V99    COMP-3.
00199          16 AM-OVR-SHT-PCT                 PIC S9V9(4)    COMP-3.
00200
011410     12  AM-DCC-PRODUCT-CODE               PIC XXX.
041910     12  AM-DCC-CLP-STATE                  PIC XX.
00202
00203      12  AM-RECALC-COMM                    PIC X.
00204      12  AM-RECALC-REIN                    PIC X.
00205
00206      12  AM-REI-TABLE                      PIC XXX.
00207      12  AM-REI-ET-LF                      PIC X.
00208      12  AM-REI-ET-AH                      PIC X.
00209      12  AM-REI-PE-LF                      PIC X.
00210      12  AM-REI-PE-AH                      PIC X.
00211      12  AM-REI-PRT-ST                     PIC X.
00212      12  AM-REI-FEE-LF                     PIC S9V9999    COMP-3.
00213      12  AM-REI-FEE-AH                     PIC S9V9999    COMP-3.
00214      12  AM-REI-LF-TAX                     PIC S9V9999    COMP-3.
00215      12  AM-REI-GROUP-A                    PIC X(6).
00216      12  AM-REI-MORT                       PIC X(4).
00217      12  AM-REI-PRT-OW                     PIC X.
00218      12  AM-REI-PR-PCT                     PIC S9V9999    COMP-3.
00219      12  AM-REI-78-PCT                     PIC S9V9999    COMP-3.
00220      12  AM-REI-AH-TAX                     PIC S9V9999    COMP-3.
00221      12  AM-REI-GROUP-B                    PIC X(6).
00222
00223      12  AM-TRUST-TYPE                     PIC X(2).
00224
00225      12  AM-EMPLOYER-STMT-USED             PIC X.
00226      12  AM-GROUPED-CHECKS-Y-N             PIC X.
00227
00228      12  AM-STD-AH-TYPE                    PIC XX.
00229      12  AM-EARN-METHODS.
00230          16  AM-EARN-METHOD-R              PIC X.
00231              88 AM-REF-RL-R78                 VALUE 'R'.
00232              88 AM-REF-RL-PR                  VALUE 'P'.
00233              88 AM-REF-RL-MEAN                VALUE 'M'.
00234              88 AM-REF-RL-ANTICIPATION        VALUE 'A'.
00235          16  AM-EARN-METHOD-L              PIC X.
00236              88 AM-REF-LL-R78                 VALUE 'R'.
00237              88 AM-REF-LL-PR                  VALUE 'P'.
00238              88 AM-REF-LL-MEAN                VALUE 'M'.
00239              88 AM-REF-LL-ANTICIPATION        VALUE 'A'.
00240          16  AM-EARN-METHOD-A              PIC X.
00241              88 AM-REF-AH-R78                 VALUE 'R'.
00242              88 AM-REF-AH-PR                  VALUE 'P'.
00243              88 AM-REF-AH-MEAN                VALUE 'M'.
00244              88 AM-REF-AH-ANTICIPATION        VALUE 'A'.
00245              88 AM-REF-AH-CALIF-SPEC          VALUE 'C'.
00246              88 AM-REF-AH-NET                 VALUE 'N'.
00247
00248      12  AM-TOL-PREM                       PIC S999V99    COMP-3.
00249      12  AM-TOL-REF                        PIC S999V99    COMP-3.
00250      12  AM-TOL-CLM                        PIC S999V99    COMP-3.
00251
00252      12  AM-RET-Y-N                        PIC X.
00253      12  AM-RET-P-E                        PIC X.
00254      12  AM-LF-RET                         PIC S9V9999    COMP-3.
00255      12  AM-AH-RET                         PIC S9V9999    COMP-3.
00256      12  AM-RET-GRP                        PIC X(6).
00257      12  AM-RETRO-POOL  REDEFINES  AM-RET-GRP.
00258          16  AM-POOL-PRIME                 PIC XXX.
00259          16  AM-POOL-SUB                   PIC XXX.
00260      12  AM-RETRO-EARNINGS.
00261          16  AM-RET-EARN-R                 PIC X.
00262          16  AM-RET-EARN-L                 PIC X.
00263          16  AM-RET-EARN-A                 PIC X.
00264      12  AM-RET-ST-TAX-USE                 PIC X.
00265          88  CHARGE-ST-TAXES-ON-RETRO         VALUE 'Y' 'E' 'P'.
00266          88  TAXES-NOT-IN-RETRO               VALUE 'N' ' '.
00267      12  AM-RETRO-BEG-EARNINGS.
00268          16  AM-RET-BEG-EARN-R             PIC X.
00269          16  AM-RET-BEG-EARN-L             PIC X.
00270          16  AM-RET-BEG-EARN-A             PIC X.
00271      12  AM-RET-MIN-LOSS-L                 PIC SV999      COMP-3.
00272      12  AM-RET-MIN-LOSS-A                 PIC SV999      COMP-3.
00273
00274      12  AM-USER-SELECT-OPTIONS.
00275          16  AM-USER-SELECT-1              PIC X(10).
00276          16  AM-USER-SELECT-2              PIC X(10).
00277          16  AM-USER-SELECT-3              PIC X(10).
00278          16  AM-USER-SELECT-4              PIC X(10).
00279          16  AM-USER-SELECT-5              PIC X(10).
00280
00281      12  AM-LF-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00282
00283      12  AM-AH-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00284
00285      12  AM-RPT045A-SWITCH                 PIC X.
00286          88  RPT045A-OFF                   VALUE 'N'.
00287
00288      12  AM-INSURANCE-LIMITS.
00289          16  AM-MAX-MON-BEN                PIC S9(7)      COMP-3.
00290          16  AM-MAX-TOT-BEN                PIC S9(7)      COMP-3.
00291
00292      12  AM-PROFILE-CHANGE-SWITCH          PIC X.
00293          88  AM-PROFILE-DATA-CHANGED          VALUE '*'.
00294
00295      12  AM-DISMBR-COVERAGE-SW             PIC X.
00296          88  AM-DISMBR-COVERAGE               VALUE 'Y'.
00297          88  AM-NO-DISMBR-COVERAGE            VALUE 'N'.
00298
00299      12  AM-CANCEL-FEE                     PIC S9(3)V9(2) COMP-3.
00300
00301      12  AM-TOL-REF-PCT                    PIC S9V9(4)    COMP-3.
090803     12  AM-CLP-TOL-PCT                    PIC S9V9(4)    COMP-3.
092705     12  AM-SPP-LEASE-COMM                 PIC S9(5)V99   COMP-3.
           12  AM-DCC-MAX-MARKETING-FEE          PIC S9(5)      COMP-3.
090803     12  FILLER                            PIC X(5).
120406     12  AM-REPORT-CODE-3                  PIC X(10).
090803*    12  FILLER                            PIC X(22).
00303
00304      12  AM-RESERVE-DATE.
00305          16  AM-TARGET-LOSS-RATIO          PIC S9V9(4) COMP-3.
00306          16  AM-LIFE-IBNR-PCT              PIC S9V9(4) COMP-3.
00307          16  AM-CRDT-MODIFICATION-PCT      PIC S9V9(4) COMP-3.
00308
00309      12  AM-3RD-PARTY-NOTIF-LEVEL          PIC 99.
00310      12  AM-NOTIFICATION-TYPES.
00311          16  AM-NOTIF-OF-LETTERS           PIC X.
00312          16  AM-NOTIF-OF-PAYMENTS          PIC X.
00313          16  AM-NOTIF-OF-REPORTS           PIC X.
00314          16  AM-NOTIF-OF-STATUS            PIC X.
00315
00316      12  AM-BENEFIT-TABLE-USAGE            PIC X.
00317          88  AM-BENEFIT-TABLE-USED            VALUE 'Y'.
00318          88  AM-USE-DEVIATIONS-ONLY           VALUE 'D'.
00319          88  AM-EDIT-BENEFITS-ONLY            VALUE 'E'.
00320          88  AM-EDITS-NOT-USED                VALUE ' '  'N'.
00321
00322      12  AM-BENEFIT-CONTROLS.
00323          16  AM-ALLOWABLE-BENEFITS  OCCURS  20  TIMES.
00324              20  AM-BENEFIT-CODE           PIC XX.
00325              20  AM-BENEFIT-TYPE           PIC X.
00326              20  AM-BENEFIT-REVISION       PIC XXX.
00327              20  AM-BENEFIT-REM-TERM       PIC X.
00328              20  AM-BENEFIT-RETRO-Y-N      PIC X.
00329              20  FILLER                    PIC XX.
00330          16  FILLER                        PIC X(80).
00331
00332      12  AM-TRANSFER-DATA.
00333          16  AM-TRANSFERRED-FROM.
00334              20  AM-TRNFROM-CARRIER        PIC X.
00335              20  AM-TRNFROM-GROUPING.
00336                  24  AM-TRNFROM-GRP-PREFIX PIC XXX.
00337                  24  AM-TRNFROM-GRP-PRIME  PIC XXX.
00338              20  AM-TRNFROM-STATE          PIC XX.
00339              20  AM-TRNFROM-ACCOUNT.
00340                  24  AM-TRNFROM-ACCT-PREFIX PIC X(4).
00341                  24  AM-TRNFROM-ACCT-PRIME PIC X(6).
00342              20  AM-TRNFROM-DTE            PIC XX.
00343          16  AM-TRANSFERRED-TO.
00344              20  AM-TRNTO-CARRIER          PIC X.
00345              20  AM-TRNTO-GROUPING.
00346                  24  AM-TRNTO-GRP-PREFIX   PIC XXX.
00347                  24  AM-TRNTO-GRP-PRIME    PIC XXX.
00348              20  AM-TRNTO-STATE            PIC XX.
00349              20  AM-TRNTO-ACCOUNT.
00350                  24  AM-TRNTO-ACCT-PREFIX  PIC X(4).
00351                  24  AM-TRNTO-ACCT-PRIME   PIC X(6).
00352              20  AM-TRNTO-DTE              PIC XX.
00353          16  FILLER                        PIC X(10).
00354
00355      12  AM-SAVED-REMIT-TO                 PIC 99.
00356
00357      12  AM-COMM-STRUCTURE-SAVED.
00358          16  AM-DEFN-1-SAVED.
00359              20  AM-AGT-COMMS-SAVED    OCCURS 10 TIMES.
00360                  24  AM-AGT-SV             PIC X(10).
00361                  24  AM-COM-TYP-SV         PIC X.
00362                  24  AM-L-COM-SV           PIC SV9(5)     COMP-3.
00363                  24  AM-J-COM-SV           PIC SV9(5)     COMP-3.
00364                  24  AM-A-COM-SV           PIC SV9(5)     COMP-3.
00365                  24  AM-RECALC-LV-INDIC-SV PIC X.
00366                  24  FILLER                PIC X.
00367                  24  AM-GL-CODES-SV        PIC X.
00368                  24  AM-COM-CHARGEBACK-SV  PIC 99.
00369                  24  FILLER                PIC X.
00370          16  AM-DEFN-2-SAVED   REDEFINES   AM-DEFN-1-SAVED.
00371              20  AM-COM-TBLS-SAVED    OCCURS 10 TIMES.
00372                  24  FILLER                PIC X(11).
00373                  24  AM-L-COMA-SV          PIC XXX.
00374                  24  AM-J-COMA-SV          PIC XXX.
00375                  24  AM-A-COMA-SV          PIC XXX.
00376                  24  FILLER                PIC X(6).
00377
00378      12  AM-FLC-NET-PREMIUM-ALLOWANCE.
00379          16 AM-ACCOUNT-ALLOWANCE OCCURS  5 TIMES.
00380             20  AM-ALLOW-BEGIN-RANGE       PIC S9(5)      COMP-3.
00381             20  AM-ALLOW-END-RANGE         PIC S9(5)      COMP-3.
00382             20  AM-ALLOWANCE-AMT           PIC S9(5)V99   COMP-3.
00383
122806     12  AM-ORIG-DEALER-NO                 PIC X(10).
122806     12  FILLER                            PIC X(120).
00385
00386      12  AM-ACCOUNT-EXECUTIVE-DATA.
00387          16  AM-CONTROL-NAME               PIC X(30).
00388          16  AM-EXECUTIVE-ONE.
00389              20  AM-EXEC1-NAME             PIC X(15).
00390              20  AM-EXEC1-DIS-PERCENT      PIC S9(01)V9(04)
00391                                                           COMP-3.
00392              20  AM-EXEC1-LIFE-PERCENT     PIC S9(01)V9(04)
00393                                                           COMP-3.
00394          16  AM-EXECUTIVE-TWO.
00395              20  AM-EXEC2-NAME             PIC X(15).
00396              20  AM-EXEC2-DIS-PERCENT      PIC S9(01)V9(04)
00397                                                           COMP-3.
00398              20  AM-EXEC2-LIFE-PERCENT     PIC S9(01)V9(04)
00399                                                           COMP-3.
00400
00401      12  AM-RETRO-ADDITIONAL-DATA.
00402          16  AM-RETRO-QUALIFY-LIMIT        PIC S9(7)      COMP-3.
00403          16  AM-RETRO-PREM-P-E             PIC X.
00404          16  AM-RETRO-CLMS-P-I             PIC X.
00405          16  AM-RETRO-RET-BRACKET-LF.
00406              20  AM-RETRO-RET-METHOD-LF    PIC X.
00407                  88  AM-RETRO-USE-PCT-LF      VALUE 'P' ' '.
00408                  88  AM-RETRO-USE-SCALE-LF    VALUE 'S'.
00409              20  AM-RETRO-RET-BASIS-LF     PIC X.
00410                  88  AM-RETRO-EARN-BASIS-LF   VALUE 'E' ' '.
00411                  88  AM-RETRO-PAID-BASIS-LF   VALUE 'P'.
00412              20  AM-RETRO-BRACKETS-LF  OCCURS  3 TIMES.
00413                  24  AM-RETRO-RET-PCT-LF   PIC S9V9999    COMP-3.
00414                  24  AM-RETRO-RET-THRU-LF  PIC S9(7)      COMP-3.
00415          16  AM-RETRO-RET-BRACKET-AH.
00416              20  AM-RETRO-RET-METHOD-AH    PIC X.
00417                  88  AM-RETRO-USE-PCT-AH      VALUE 'P' ' '.
00418                  88  AM-RETRO-USE-SCALE-AH    VALUE 'S'.
00419                  88  AM-RETRO-USE-LIFE-METHOD VALUE 'L'.
00420              20  AM-RETRO-RET-BASIS-AH     PIC X.
00421                  88  AM-RETRO-EARN-BASIS-AH   VALUE 'E' ' '.
00422                  88  AM-RETRO-PAID-BASIS-AH   VALUE 'P'.
00423              20  AM-RETRO-BRACKETS-AH  OCCURS  3 TIMES.
00424                  24  AM-RETRO-RET-PCT-AH   PIC S9V9999    COMP-3.
00425                  24  AM-RETRO-RET-THRU-AH  PIC S9(7)      COMP-3.
00426
00427      12  AM-COMMENTS.
00428          16  AM-COMMENT-LINE           PIC X(50)   OCCURS 5 TIMES.
00429
00430      12  AM-CLIENT-OVERLAY-FLI   REDEFINES   AM-COMMENTS.
00431          16  AM-FLI-RETRO-SHARE-CODE       PIC X.
00432          16  AM-FLI-BILLING-CODE           PIC X.
00433          16  AM-FLI-ALT-STATE-CODE         PIC XX.
00434          16  AM-FLI-UNITED-IDENT           PIC X.
00435          16  AM-FLI-INTEREST-LOST-DATA.
00436              20  AM-FLI-BANK-NO            PIC X(5).
00437              20  AM-FLI-BANK-BALANCE       PIC S9(9)V99   COMP-3.
00438              20  AM-FLI-BANK-1ST-6-PREM    PIC S9(9)V99   COMP-3.
00439              20  AM-FLI-BANK-CAP-AMT       PIC S9(9)V99   COMP-3.
00440          16  AM-FLI-ALT-AGENT-CODES   OCCURS 10 TIMES.
00441              20  AM-FLI-AGT                PIC X(9).
00442              20  AM-FLI-AGT-COMM-ACC       PIC X.
00443              20  AM-FLI-AGT-SHARE-PCT      PIC S9V99      COMP-3.
00444          16  FILLER                        PIC X(102).
00445
00446      12  AM-CLIENT-OVERLAY-DMD   REDEFINES   AM-COMMENTS.
00447          16  AM-ALLOWABLE-DMD-BENEFITS  OCCURS 30 TIMES.
00448              20  AM-BENEFIT-DMD-CODE         PIC XX.
00449              20  AM-BENEFIT-DMD-TYPE         PIC X.
00450              20  AM-BENEFIT-DMD-REVISION     PIC XXX.
00451              20  AM-BENEFIT-DMD-REM-TERM     PIC X.
00452              20  AM-BENEFIT-DMD-RETRO-Y-N    PIC X.
00453          16  FILLER                          PIC X(10).
00454 ******************************************************************
           EJECT
      *                                    COPY ELCCNTL.
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
           EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA PURGE-CERT-MASTER
                                ACCOUNT-MASTER CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL128' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
           CONTINUE.
           MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
           MOVE '5'                    TO  DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION.
           MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
           MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
           MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
           MOVE +2                     TO  EMI-NUMBER-OF-LINES
                                           EMI-SWITCH2.
      *    NOTE *******************************************************
      *         *                                                     *
      *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
      *         *  FROM ANOTHER MODULE.                               *
      *         *                                                     *
      *         *******************************************************.
           IF EIBCALEN NOT GREATER THAN ZERO
               MOVE UNACCESS-MSG       TO  LOGOFF-MSG
               GO TO 8300-SEND-TEXT.
           
      * EXEC CICS HANDLE CONDITION
      *        PGMIDERR (9600-PGMIDERR)
      *        NOTFND   (0030-MAIN-LOGIC)
      *        ENDFILE  (0030-MAIN-LOGIC)
      *        ERROR    (9990-ERROR)
      *    END-EXEC.
      *    MOVE '"$LI''.                ! " #00004500' TO DFHEIV0
           MOVE X'22244C49272E202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303034353030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF PI-CALLING-PROGRAM NOT = 'EL1282'
               MOVE ZERO                TO PI-ALT-NAME-COUNT.
           EJECT
       0010-MAIN-LOGIC.
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
                   PERFORM 7000-BUILD-SCREEN
             ELSE
               GO TO 0020-CONTINUE-PROCESSING.
       0015-INITIALIZE.
      *    NOTE *******************************************************
      *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      *
      *         *  INTERFACE BLOCK FOR THIS MODULE.                   *
      *         *******************************************************.
           MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA
                                           PI-CONTROL-IN-PROGRESS.
           MOVE ZERO                   TO  PI-1ST-TIME-SW
                                           PI-LINE-COUNT
                                           PI-BROWSE-SW
                                           PI-KEY-LENGTH
                                           PI-TS-ITEM
                                           PI-END-OF-FILE
                                           PI-START-SW
                                           PI-AIX-RECORD-COUNT.
      *    NOTE *******************************************************
      *         *      SEND THE INITIAL MAP OUT TO BEGIN PROCESSING   *
      *         *  FOR EL128.                                         *
      *         *******************************************************.
           MOVE LOW-VALUES             TO  EL128AO.
           GO TO 8100-SEND-INITIAL-MAP.
           EJECT
       0020-CONTINUE-PROCESSING.
           IF PI-1ST-TIME-SW NOT = ZERO
               GO TO 0015-INITIALIZE.
      *    NOTE *******************************************************
      *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *
      *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *
      *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
      *         *******************************************************.
           IF EIBAID = DFHCLEAR
               GO TO 9400-CLEAR.
           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
               MOVE LOW-VALUES         TO  EL128AO
               MOVE -1                 TO  APFKL
               MOVE ER-0008            TO  EMI-ERROR
               GO TO 8200-SEND-DATAONLY.
           
      * EXEC CICS RECEIVE
      *        INTO   (EL128AI)
      *        MAPSET (WS-MAPSET-NAME)
      *        MAP    (WS-MAP-NAME)
      *    END-EXEC.
           MOVE LENGTH OF
            EL128AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00004569' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL128AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF APFKL GREATER ZERO
               IF EIBAID NOT = DFHENTER
                   MOVE ER-0004        TO  EMI-ERROR
                   MOVE AL-UNBOF       TO  APFKA
                   MOVE -1             TO  APFKL
                   GO TO 8200-SEND-DATAONLY
               ELSE
                   IF APFKO IS NUMERIC
                   IF APFKO GREATER 0 AND LESS 25
                       MOVE PF-VALUES (APFKI)  TO  EIBAID
                     ELSE
                       MOVE ER-0029        TO  EMI-ERROR
                       MOVE AL-UNBOF       TO  APFKA
                       MOVE -1             TO  APFKL
                       GO TO 8200-SEND-DATAONLY.
           IF EIBAID = DFHPF12
               MOVE 'EL010'         TO  THIS-PGM
               GO TO 9300-XCTL.
           IF EIBAID = DFHPF23
               GO TO 9000-RETURN-CICS.
           IF EIBAID = DFHPF24
               MOVE 'EL126'         TO  THIS-PGM
               GO TO 9300-XCTL.
           IF EIBAID NOT = DFHENTER
               MOVE ER-0008            TO  EMI-ERROR
               MOVE -1                 TO  APFKL
               GO TO 8200-SEND-DATAONLY.
           EJECT
       0025-MAIN-LOGIC.
           MOVE SPACES                 TO  PI-SELECTION-CRITERIA
                                           PI-CERTIFICATE-KEY.
           MOVE PI-COMPANY-CD          TO  PI-SC-COMPANY-CD
                                           PI-CK-COMPANY-CD.
           MOVE 'EL1282'               TO  THIS-PGM.
           IF PI-PROCESSOR-ID = 'LGXX'
               NEXT SENTENCE
           ELSE
               
      * EXEC CICS READQ TS
      *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
      *            INTO    (SECURITY-CONTROL)
      *            LENGTH  (SC-COMM-LENGTH)
      *            ITEM    (SC-ITEM)
      *        END-EXEC
      *    MOVE '*$II   L              ''   #00004611' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
               MOVE SC-CREDIT-DISPLAY (31)  TO  PI-DISPLAY-CAP
               MOVE SC-CREDIT-UPDATE  (31)  TO  PI-MODIFY-CAP
               IF NOT DISPLAY-CAP
                   MOVE 'READ'              TO  SM-READ
                   PERFORM 9995-SECURITY-VIOLATION
                   MOVE ER-0070             TO  EMI-ERROR
                   GO TO 8100-SEND-INITIAL-MAP.
           EJECT
      ******************************************************************
      *           O P T I O N  1  P R O C E S S I N G                  *
      ******************************************************************
           IF ACRTNO4L GREATER ZERO  OR
              ACRTSX4L GREATER ZERO
               NEXT SENTENCE
           ELSE
               GO TO 0027-MAIN-LOGIC.
           MOVE WS-CERT-AIX04-DSID     TO  PI-DSID.
           MOVE '1'                    TO  PI-OPTION.
           IF ACRTNO4L NOT GREATER ZERO  AND
              ACRTSX4L GREATER ZERO
               MOVE ER-0210            TO  EMI-ERROR
               MOVE -1                 TO  ACRTNO4L
               GO TO 8200-SEND-DATAONLY.
           MOVE ACRTNO4I               TO  PI-SC-CERT-PRIME-A4.
           MOVE +11                    TO  PI-KEY-LENGTH.
           IF ACRTSX4L GREATER ZERO
               MOVE ACRTSX4I           TO  PI-SC-CERT-SFX-A4
               MOVE +12                TO  PI-KEY-LENGTH.
           MOVE PI-SELECTION-CRITERIA  TO  PI-CERTIFICATE-KEY.
           MOVE -1                     TO  ACRTNO4L.
           PERFORM 4000-READ-CERT-FILE.
           EJECT
       0027-MAIN-LOGIC.
      ******************************************************************
      *           O P T I O N  2  P R O C E S S I N G                  *
      ******************************************************************
           IF ACERTNOL GREATER ZERO  OR
              ACERTSXL GREATER ZERO  OR
              AACCTNOL GREATER ZERO  OR
              ASTATEL  GREATER ZERO  OR
              ACARIERL GREATER ZERO  OR
              AGROUPL  GREATER ZERO  OR
              AEDATEL  GREATER ZERO
               NEXT SENTENCE
             ELSE
               GO TO 0100-MAIN-LOGIC.
            ADD +1                    TO  WS-KEY-LENGTH.
      ************************************************************
      *        SECURITY CHECK FOR ACCOUNT AND CARRIER NO         *
      *                      03/29/84                            *
      ************************************************************
           IF  PI-NO-ACCOUNT-SECURITY AND PI-NO-CARRIER-SECURITY
               GO TO 0028-PROCESS-OPTION-2.
           IF  PI-NO-ACCOUNT-SECURITY
               GO TO 0028-CHECK-CARRIER-SECURITY.
           IF  AACCTNOL GREATER ZERO
               NEXT SENTENCE
           ELSE
               GO TO 0028-CHECK-CARRIER-SECURITY.
           IF  AACCTNOI = PI-ACCOUNT-SECURITY
               MOVE AL-UANON           TO  AACCTNOA
           ELSE
               MOVE -1                 TO  AACCTNOL
               MOVE AL-UABON           TO  AACCTNOA
               MOVE ER-2371            TO  EMI-ERROR
               PERFORM 9900-ERROR-FORMAT.
       0028-CHECK-CARRIER-SECURITY.
           IF  PI-NO-CARRIER-SECURITY
               GO TO  0028-PROCESS-OPTION-2.
           IF ACARIERL GREATER ZERO
               NEXT SENTENCE
           ELSE
               GO TO  0028-ERROR-CHECK.
           IF  ACARIERI = PI-CARRIER-SECURITY
               MOVE AL-UANON            TO  ACARIERA
           ELSE
               MOVE -1                  TO  ACARIERL
               MOVE ER-2370             TO  EMI-ERROR
               MOVE AL-UABON            TO  ACARIERA
               PERFORM 9900-ERROR-FORMAT.
       0028-ERROR-CHECK.
           IF  EMI-FATAL-CTR GREATER ZERO
               GO TO 8200-SEND-DATAONLY.
       0028-PROCESS-OPTION-2.
           MOVE WS-CERT-MASTER-DSID    TO  PI-DSID.
           MOVE '2'                    TO  PI-OPTION.
           IF ACARIERL GREATER ZERO
               MOVE ACARIERI           TO  PI-SC-CARRIER
           ELSE
               IF PI-CERT-ACCESS-CONTROL = ('1' OR '2' OR '4')
                   MOVE ER-0194        TO  EMI-ERROR
                   PERFORM 9900-ERROR-FORMAT
                   MOVE -1             TO  ACARIERL.
           IF ACARIERL GREATER ZERO  AND
              AGROUPL  = ZERO  AND
              ASTATEL  = ZERO  AND
              AACCTNOL = ZERO  AND
              AEDATEL  = ZERO  AND
              ACERTNOL = ZERO  AND
              ACERTSXL = ZERO
              MOVE SPACES                  TO  PI-SC-GROUP
                                               PI-SC-STATE
                                               PI-SC-ACCOUNT
                                               PI-SC-EFF-DATE
                                               PI-SC-CERT-NO
              ADD +1                    TO  WS-KEY-LENGTH
              MOVE 'Y'                  TO  PART-KEY-ON-SW
              GO TO 0400-MAIN-LOGIC.
      ****    GO TO 0028-PROCESS-OPTION-2-CONT.
           IF  AGROUPL  > ZERO  AND
               ACARIERL = ZERO
               MOVE -1                 TO  AGROUPL
               MOVE AL-UABON           TO  AGROUPA
               MOVE ER-8101            TO  EMI-ERROR
               PERFORM 9900-ERROR-FORMAT.
           IF AGROUPL GREATER ZERO
               MOVE AGROUPI            TO  PI-SC-GROUP
           ELSE
               IF PI-CERT-ACCESS-CONTROL = '1'
                   MOVE ER-0195        TO  EMI-ERROR
                   PERFORM 9900-ERROR-FORMAT
                   MOVE -1             TO  AGROUPL.
           IF AGROUPL  > ZERO  AND
              ASTATEL  = ZERO  AND
              AACCTNOL = ZERO  AND
              AEDATEL  = ZERO  AND
              ACERTNOL = ZERO  AND
              ACERTSXL = ZERO
              MOVE SPACES                  TO  PI-SC-STATE
                                               PI-SC-ACCOUNT
                                               PI-SC-EFF-DATE
                                               PI-SC-CERT-NO
              ADD +7                    TO  WS-KEY-LENGTH
              MOVE 'Y'                  TO  PART-KEY-ON-SW
              GO TO 0400-MAIN-LOGIC.
      ****    GO TO 0028-PROCESS-OPTION-2-CONT.
           IF ASTATEL GREATER ZERO AND
               AGROUPL  = ZERO
               MOVE -1                 TO  ASTATEL
               MOVE AL-UABON           TO  ASTATEA
               MOVE ER-8102            TO  EMI-ERROR
               PERFORM 9900-ERROR-FORMAT.
           IF ASTATEL GREATER ZERO
               MOVE ASTATEI            TO  PI-SC-STATE
                                           WK-SC-STATE
           ELSE
               IF PI-CERT-ACCESS-CONTROL = (SPACES OR '1' OR '2')
                   MOVE ER-0196        TO  EMI-ERROR
                   PERFORM 9900-ERROR-FORMAT
                   MOVE -1             TO  ASTATEL.
           IF ASTATEL GREATER ZERO
               IF WK-SC-STATE-1 = SPACES OR LOW-VALUES
                   MOVE ER-8106        TO  EMI-ERROR
                   MOVE -1             TO  ASTATEL
                   GO TO 8200-SEND-DATAONLY
               ELSE
                   IF WK-SC-STATE-2 = SPACES OR LOW-VALUES
                      MOVE 'S'         TO  PART-FIELD-ON-SW
                      MOVE LOW-VALUES  TO  WK-SC-STATE-2
                      MOVE WK-SC-STATE TO  PI-SC-STATE.
           IF ASTATEL  > ZERO  AND
              AACCTNOL = ZERO  AND
              AEDATEL  = ZERO  AND
              ACERTNOL = ZERO  AND
              ACERTSXL = ZERO
              MOVE SPACES                  TO  PI-SC-ACCOUNT
                                               PI-SC-EFF-DATE
                                               PI-SC-CERT-NO
              MOVE 'Y'                  TO  PART-KEY-ON-SW
              IF PART-FIELD-STATE
                 ADD +8                 TO  WS-KEY-LENGTH
                 GO TO 0400-MAIN-LOGIC
              ELSE
                 ADD +9                 TO  WS-KEY-LENGTH
                 GO TO 0400-MAIN-LOGIC.
      ****    GO TO 0028-PROCESS-OPTION-2-CONT.
           IF  AACCTNOL GREATER ZERO  AND
               ASTATEL  = ZERO
               MOVE -1                 TO  AACCTNOL
               MOVE AL-UABON           TO  AACCTNOA
               MOVE ER-8100            TO  EMI-ERROR
               PERFORM 9900-ERROR-FORMAT.
           IF AACCTNOL GREATER ZERO
               MOVE AACCTNOI           TO  PI-SC-ACCOUNT
           ELSE
               MOVE ER-0197            TO  EMI-ERROR
               PERFORM 9900-ERROR-FORMAT
               MOVE -1                 TO  AACCTNOL.
           IF AACCTNOL > ZERO  AND
              AEDATEL  = ZERO  AND
              ACERTNOL = ZERO  AND
              ACERTSXL = ZERO
              MOVE SPACES                  TO  PI-SC-EFF-DATE
                                               PI-SC-CERT-NO
              ADD +19                   TO  WS-KEY-LENGTH
              MOVE 'Y'                  TO  PART-KEY-ON-SW
              GO TO 0400-MAIN-LOGIC.
      ****    GO TO 0028-PROCESS-OPTION-2-CONT.
           IF  AEDATEL GREATER ZERO   AND
               AACCTNOL = ZERO
               MOVE -1                 TO  AEDATEL
               MOVE AL-UABON           TO  AEDATEA
               MOVE ER-8103            TO  EMI-ERROR
               PERFORM 9900-ERROR-FORMAT.
           IF  AEDATEL GREATER   ZERO   AND
               AEDATEL LESS THAN +6
               MOVE -1                 TO  AEDATEL
               MOVE AL-UABON           TO  AEDATEA
               MOVE ER-8105            TO  EMI-ERROR
               PERFORM 9900-ERROR-FORMAT.
           IF AEDATEL GREATER ZERO
               MOVE AEDATEI            TO  WS-DEEDIT-FIELD
               PERFORM 8600-DEEDIT
               IF WS-DEEDIT-FIELD-V0 NUMERIC
                   MOVE WS-DEEDIT-FIELD-V0  TO  AEDATEO
                   INSPECT AEDATEI CONVERTING SPACES TO '/'
                   MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
                   MOVE '4'                TO  DC-OPTION-CODE
                   PERFORM 8500-DATE-CONVERSION
                   IF DC-ERROR-CODE NOT = SPACES
                       MOVE ER-0215        TO  EMI-ERROR
                       PERFORM 9900-ERROR-FORMAT
                       MOVE -1             TO  AEDATEL
                       MOVE AL-UABON       TO  AEDATEA
                   ELSE
                       MOVE AL-UANON       TO  AEDATEA
                       MOVE DC-BIN-DATE-1  TO  PI-SC-EFF-DATE
               ELSE
                   MOVE ER-0215        TO  EMI-ERROR
                   PERFORM 9900-ERROR-FORMAT
                   MOVE -1             TO  AEDATEL
                   MOVE AL-UABON       TO  AEDATEA
           ELSE
               MOVE ER-0216            TO  EMI-ERROR
               PERFORM 9900-ERROR-FORMAT
               MOVE -1                 TO  AEDATEL
               MOVE AL-UABOF           TO  AEDATEA.
           IF AEDATEL  > ZERO  AND
              ACERTNOL = ZERO  AND
              ACERTSXL = ZERO
              ADD +21                   TO  WS-KEY-LENGTH
              MOVE 'Y'                  TO  PART-KEY-ON-SW
              MOVE SPACES               TO  PI-SC-CERT-NO
              GO TO 0400-MAIN-LOGIC.
       0028-PROCESS-OPTION-2-CONT.
           MOVE +22                    TO  PI-KEY-LENGTH
                                           WS-KEY-LENGTH.
           IF  ACERTNOL GREATER ZERO  AND
               AEDATEL  = ZERO
               MOVE -1                 TO  ACERTNOL
               MOVE AL-UABON           TO  ACERTNOA
               MOVE ER-8104            TO  EMI-ERROR
               PERFORM 9900-ERROR-FORMAT.
           IF ACERTNOL GREATER ZERO
               MOVE ACERTNOI           TO  PI-SC-CERT-PRIME
                                           WK-SC-CERT
               MOVE +32                TO  PI-KEY-LENGTH.
           PERFORM 0029-TEST-CERT-LENGTH THRU 0029-EXIT.
           IF ACERTSXL GREATER ZERO
               MOVE ACERTSXI           TO  PI-SC-CERT-SFX
               MOVE +33                TO  PI-KEY-LENGTH.
           IF EMI-FATAL-CTR GREATER ZERO
               GO TO 0040-MAIN-LOGIC.
           IF PI-SC-CARRIER  NOT = SPACES  AND
              PI-SC-GROUP    NOT = SPACES  AND
              PI-SC-STATE    NOT = SPACES  AND
              PI-SC-ACCOUNT  NOT = SPACES  AND
              PI-SC-EFF-DATE NOT = SPACES  AND
              PI-SC-CERT-NO  NOT = SPACES
               MOVE PI-SELECTION-CRITERIA  TO  PI-CERTIFICATE-KEY
               MOVE -1                     TO  ACERTNOL
               MOVE WS-CERT-MASTER-DSID    TO  PI-DSID
               MOVE +33                    TO  PI-KEY-LENGTH
               PERFORM 4000-READ-CERT-FILE.
           IF PI-SC-CARRIER  = SPACES
              MOVE LOW-VALUES              TO  PI-SC-CARRIER.
           IF PI-SC-GROUP    = SPACES
              MOVE LOW-VALUES              TO  PI-SC-GROUP.
           IF PI-SC-STATE    = SPACES
              MOVE LOW-VALUES              TO  PI-SC-STATE.
           IF PI-SC-ACCOUNT  = SPACES
              MOVE LOW-VALUES              TO  PI-SC-ACCOUNT.
           IF PI-SC-EFF-DATE = SPACES
              MOVE LOW-VALUES              TO  PI-SC-EFF-DATE.
           IF PI-SC-CERT-NO  = SPACES
              MOVE LOW-VALUES              TO  PI-SC-CERT-NO.
      *       MOVE PI-SELECTION-CRITERIA  TO  PI-CERTIFICATE-KEY
      *       MOVE -1                     TO  ACERTNOL
      *       MOVE WS-CERT-MASTER-DSID    TO  PI-DSID
      *       PERFORM 3900-READNEXT-CERT-FILE.
           MOVE SPACES                 TO  PI-ACCOUNT-KEY.
           MOVE PI-COMPANY-CD          TO  PI-AK-COMPANY-CD.
           IF PI-CERT-ACCESS-CONTROL = '1' OR '2' OR '4'
               MOVE PI-SC-CARRIER      TO  PI-AK-CARRIER.
           IF PI-CERT-ACCESS-CONTROL = '1'
               MOVE PI-SC-GROUP        TO  PI-AK-GROUP.
           IF PI-CERT-ACCESS-CONTROL = SPACES OR '1' OR '2'
               MOVE PI-SC-STATE        TO  PI-AK-STATE.
           MOVE PI-SC-EFF-DATE         TO  PI-AK-EXPIRE-DATE.
           MOVE PI-SC-ACCOUNT          TO  PI-AK-ACCOUNT.
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND   (0030-MAIN-LOGIC)
      *    END-EXEC.
      *    MOVE '"$I                   ! # #00004917' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034393137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS STARTBR
      *        DATASET   (WS-ACCOUNT-MASTER-DSID)
      *        RIDFLD    (PI-ACCOUNT-KEY)
      *        GTEQ
      *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004920' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACCOUNT-MASTER-DSID, 
                 PI-ACCOUNT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE +1                     TO  WS-READNEXT-SW.
       0028-MAIN-LOGIC.
           
      * EXEC CICS READNEXT
      *        DATASET   (WS-ACCOUNT-MASTER-DSID)
      *        RIDFLD    (PI-ACCOUNT-KEY)
      *        SET       (ADDRESS OF ACCOUNT-MASTER)
      *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004927' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACCOUNT-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACCOUNT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           CONTINUE.
           IF PI-SC-COMPANY-CD = PI-AK-COMPANY-CD  AND
              PI-SC-CARRIER    = PI-AK-CARRIER     AND
              PI-SC-GROUP      = PI-AK-GROUP       AND
              PI-SC-STATE      = PI-AK-STATE       AND
              PI-SC-ACCOUNT    = PI-AK-ACCOUNT
               NEXT SENTENCE
             ELSE
               GO TO 0030-MAIN-LOGIC.
           IF PI-SC-EFF-DATE LESS AM-EFFECTIVE-DT
               GO TO 0030-MAIN-LOGIC.
           IF PI-SC-EFF-DATE LESS AM-EXPIRATION-DT
               GO TO 0040-MAIN-LOGIC.
           GO TO 0028-MAIN-LOGIC.
       0029-TEST-CERT-LENGTH.
           IF ACERTNOL NOT GREATER ZERO
              GO TO 0029-EXIT.
           IF WK-SC-CERT-1 = LOW-VALUES OR SPACES
              MOVE ER-8107             TO  EMI-ERROR
              MOVE -1                  TO  ACERTNOL
              GO TO 8200-SEND-DATAONLY.
           IF WK-SC-CERT-2     = SPACES OR LOW-VALUES
              ADD +1                   TO  WS-KEY-LENGTH
              MOVE LOW-VALUES          TO  WK-SC-CERT-3
                                           WK-SC-CERT-4
                                           WK-SC-CERT-5
                                           WK-SC-CERT-6
                                           WK-SC-CERT-7
                                           WK-SC-CERT-8
                                           WK-SC-CERT-9
                                           WK-SC-CERT-10
           ELSE
             IF (WK-SC-CERT-3     = SPACES OR LOW-VALUES)
                ADD +2                 TO  WS-KEY-LENGTH
                MOVE LOW-VALUES        TO  WK-SC-CERT-4
                                           WK-SC-CERT-5
                                           WK-SC-CERT-6
                                           WK-SC-CERT-7
                                           WK-SC-CERT-8
                                           WK-SC-CERT-9
                                           WK-SC-CERT-10
             ELSE
               IF (WK-SC-CERT-4     = SPACES OR LOW-VALUES)
                  ADD +3               TO  WS-KEY-LENGTH
                  MOVE LOW-VALUES      TO  WK-SC-CERT-5
                                           WK-SC-CERT-6
                                           WK-SC-CERT-7
                                           WK-SC-CERT-8
                                           WK-SC-CERT-9
                                           WK-SC-CERT-10
               ELSE
                 IF (WK-SC-CERT-5     = SPACES OR LOW-VALUES)
                    ADD +4                 TO  WS-KEY-LENGTH
                    MOVE LOW-VALUES        TO  WK-SC-CERT-6
                                               WK-SC-CERT-7
                                               WK-SC-CERT-8
                                               WK-SC-CERT-9
                                               WK-SC-CERT-10
                 ELSE
                   IF (WK-SC-CERT-6     = SPACES OR LOW-VALUES)
                      ADD +5               TO  WS-KEY-LENGTH
                      MOVE LOW-VALUES      TO  WK-SC-CERT-7
                                               WK-SC-CERT-8
                                               WK-SC-CERT-9
                                               WK-SC-CERT-10
                   ELSE
                     IF (WK-SC-CERT-7     = SPACES OR LOW-VALUES)
                        ADD +6             TO  WS-KEY-LENGTH
                        MOVE LOW-VALUES    TO  WK-SC-CERT-8
                                               WK-SC-CERT-9
                                               WK-SC-CERT-10
                     ELSE
                       IF (WK-SC-CERT-8     = SPACES OR LOW-VALUES)
                          ADD +7               TO  WS-KEY-LENGTH
                          MOVE LOW-VALUES      TO  WK-SC-CERT-9
                                                   WK-SC-CERT-10
                       ELSE
                         IF (WK-SC-CERT-9     = SPACES OR LOW-VALUES)
                            ADD +8             TO  WS-KEY-LENGTH
                            MOVE LOW-VALUES    TO  WK-SC-CERT-10
                         ELSE
                           IF (WK-SC-CERT-10    = SPACES OR LOW-VALUES)
                              ADD +9           TO  WS-KEY-LENGTH
                              MOVE LOW-VALUES  TO  WK-SC-CERT-10
                           ELSE
                              GO TO 0029-EXIT.
           MOVE WK-SC-CERT          TO  PI-SC-CERT-PRIME
           MOVE 'Y'                 TO  PART-KEY-ON-SW
           MOVE 'C'                 TO  PART-FIELD-ON-SW
           GO TO 0400-MAIN-LOGIC.
       0029-EXIT. EXIT.
       0030-MAIN-LOGIC.
      *    MOVE ER-0198                TO  EMI-ERROR.
      *    PERFORM 9900-ERROR-FORMAT.
      *    MOVE -1                     TO  AACCTNOL.
           IF PI-CERT-ACCESS-CONTROL = '1' OR '2' OR '4'
               IF ACARIERL GREATER ZERO
                   MOVE AL-UABON           TO  ACARIERA
               ELSE
                   MOVE AL-UABOF           TO  ACARIERA.
           IF PI-CERT-ACCESS-CONTROL = '1'
               IF AGROUPL GREATER ZERO
                   MOVE AL-UABON           TO  AGROUPA
               ELSE
                   MOVE AL-UABOF           TO  AGROUPA.
           IF PI-CERT-ACCESS-CONTROL = SPACES OR '1' OR '2'
               IF ASTATEL GREATER ZERO
                   MOVE AL-UABON           TO  ASTATEA
               ELSE
                   MOVE AL-UABOF           TO  ASTATEA.
           IF AACCTNOL GREATER ZERO
               MOVE AL-UABON           TO  AACCTNOA
           ELSE
               MOVE AL-UABOF           TO  AACCTNOA.
           IF AEDATEL GREATER ZERO
               MOVE AL-UNBON           TO  AEDATEA
           ELSE
               MOVE AL-UNBOF           TO  AEDATEA.
       0040-MAIN-LOGIC.
           IF EMI-FATAL-CTR GREATER ZERO
               GO TO 8200-SEND-DATAONLY.
           MOVE AM-CONTROL-PRIMARY     TO  PI-ACCOUNT-KEY.
           MOVE AM-CARRIER             TO  PI-SC-CARRIER.
           MOVE AM-GROUPING            TO  PI-SC-GROUP.
           MOVE AM-STATE               TO  PI-SC-STATE.
           MOVE PI-SELECTION-CRITERIA  TO  PI-CERTIFICATE-KEY.
           IF PI-KEY-LENGTH NOT GREATER +22
               PERFORM 4000-READ-CERT-FILE.
           IF WS-READNEXT-SW NOT = ZERO
               
      * EXEC CICS ENDBR
      *            DATASET (WS-ACCOUNT-MASTER-DSID)
      *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005061' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACCOUNT-MASTER-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND (0050-MAIN-LOGIC)
      *    END-EXEC.
      *    MOVE '"$I                   ! $ #00005064' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303035303634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS READ
      *        DATASET (WS-CERT-MASTER-DSID)
      *        RIDFLD  (PI-CERTIFICATE-KEY)
      *        SET     (ADDRESS OF PURGE-CERT-MASTER)
      *    END-EXEC.
      *    MOVE '&"S        E          (   #00005067' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CERT-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CERTIFICATE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PURGE-CERT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           CONTINUE.
           MOVE PI-CK-CARRIER          TO  PI-CARRIER.
           MOVE PI-CK-GROUPING         TO  PI-GROUPING.
           MOVE PI-CK-STATE            TO  PI-STATE.
           MOVE PI-CK-ACCOUNT          TO  PI-ACCOUNT.
           MOVE PI-CK-CERT-EFF-DT      TO  PI-CERT-EFF-DT.
           MOVE PI-CK-CERT-NO          TO  PI-CERT-NO.
           MOVE 'EL1283'               TO  THIS-PGM.
           MOVE +1                     TO  PI-1ST-TIME-SW.
           GO TO 9300-XCTL.
       0050-MAIN-LOGIC.
           MOVE ER-0201                TO  EMI-ERROR.
           MOVE -1                     TO  ACERTNOL.
           GO TO 8200-SEND-DATAONLY.
           EJECT
       0100-MAIN-LOGIC.
      ******************************************************************
      *           O P T I O N  3  P R O C E S S I N G                  *
      ******************************************************************
           IF ALNAMEL  GREATER ZERO  OR
              AFNAMEL  GREATER ZERO  OR
              AINITALL GREATER ZERO  OR
              AACCT2L  GREATER ZERO  OR
              ACARRL   GREATER ZERO
               NEXT SENTENCE
           ELSE
               GO TO 0200-MAIN-LOGIC.
           IF (AFNAMEL  GREATER ZERO OR
               AINITALL GREATER ZERO OR
               AACCT2L  GREATER ZERO OR
               ACARRL   GREATER ZERO)
                      AND
               ALNAMEL NOT GREATER ZERO
                MOVE ER-0488            TO  EMI-ERROR
                MOVE -1                 TO  ALNAMEL
                GO TO 8200-SEND-DATAONLY.
           IF AINITALL GREATER ZERO   AND
              AFNAMEL NOT GREATER ZERO
                MOVE ER-0764            TO  EMI-ERROR
                MOVE -1                 TO  AFNAMEL
                GO TO 8200-SEND-DATAONLY.
      ************************************************************
      *           SECURITY CHECK FOR ACCOUNT NUMBER              *
      *                      03/29/84                            *
      ************************************************************
           IF  PI-NO-ACCOUNT-SECURITY
               GO TO 0110-CHECK-EDITS.
           IF  AACCT2L GREATER ZERO
               IF  AACCT2I = PI-ACCOUNT-SECURITY
                  MOVE AL-UANON        TO  AACCT2A
               ELSE
                  MOVE -1              TO  AACCT2L
                  MOVE ER-2371         TO  EMI-ERROR
                  MOVE AL-UABON        TO  AACCT2A
                  GO TO 8200-SEND-DATAONLY.
       0110-CHECK-EDITS.
           MOVE WS-CERT-AIX01-DSID     TO  PI-DSID.
           MOVE '3'                    TO  PI-OPTION.
           MOVE PI-COMPANY-CD          TO  PI-SELECTION-CRITERIA.
           MOVE +1                     TO  PI-KEY-LENGTH.
           IF ALNAMEL GREATER ZERO
               MOVE ALNAMEI            TO  PI-SC-LAST-NAME
                                           WS-INPUT-FIELD
               PERFORM 0120-MAIN-LOGIC
                  THRU 0120-MAIN-LOGIC-EXIT
                    VARYING INPUT-INDEX FROM ALNAMEL BY -1
                      UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE
               ADD ALNAMEL  TO  PI-KEY-LENGTH.
           IF AFNAMEL GREATER ZERO
               MOVE +17                TO  PI-KEY-LENGTH
               MOVE AFNAMEI            TO  WS-FIRST-NAME
               MOVE WS-FIRST-INITIAL   TO  WS-INITIAL-FIRST.
           IF AFNAMEL GREATER +1
               MOVE AFNAMEI            TO PI-SC-FIRST-NAME.
           IF AINITALL GREATER ZERO
               MOVE +18                TO  PI-KEY-LENGTH
               MOVE AINITALI           TO  WS-INITIAL-MIDDLE.
           IF WS-INITIALS GREATER SPACES
               MOVE WS-INITIALS        TO  PI-SC-INITIALS.
           IF AACCT2L GREATER ZERO
               MOVE AACCT2I            TO  PI-SC-ACCT-NO.
           IF ACARRL GREATER ZERO
               MOVE ACARRI             TO  PI-SC-CARR.
           MOVE PI-SELECTION-CRITERIA  TO  PI-CERTIFICATE-KEY.
           MOVE -1                     TO  ALNAMEL.
           PERFORM 4000-READ-CERT-FILE.
       0120-MAIN-LOGIC.
           SUBTRACT +1 FROM ALNAMEL.
       0120-MAIN-LOGIC-EXIT.
           EXIT.
           EJECT
       0200-MAIN-LOGIC.
      ******************************************************************
      *           O P T I O N  4  P R O C E S S I N G                  *
      ******************************************************************
           IF ASSNL GREATER ZERO
               NEXT SENTENCE
           ELSE
               GO TO 0300-MAIN-LOGIC.
           MOVE '4'                    TO  PI-OPTION.
           MOVE WS-CERT-AIX02-DSID     TO  PI-DSID.
           MOVE PI-COMPANY-CD          TO  PI-CK-COMPANY-CD
                                           PI-CK-COMPANY-CD.
           MOVE ASSNI                  TO  PI-CK-SOC-SEC-NO
                                           PI-SC-SOC-SEC-NO
                                           WS-INPUT-FIELD.
           PERFORM 0220-MAIN-LOGIC THRU 0220-MAIN-LOGIC-EXIT
               VARYING INPUT-INDEX FROM ASSNL BY -1
                   UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE.
           ADD +1  ASSNL  GIVING  PI-KEY-LENGTH.
           MOVE -1                     TO  ASSNL.
           PERFORM 4000-READ-CERT-FILE.
       0220-MAIN-LOGIC.
           SUBTRACT +1 FROM ASSNL.
       0220-MAIN-LOGIC-EXIT.
           EXIT.
           EJECT
       0300-MAIN-LOGIC.
      ******************************************************************
      *           O P T I O N  5  P R O C E S S I N G                  *
      ******************************************************************
           IF AMEMBERL GREATER ZERO
               NEXT SENTENCE
           ELSE
               GO TO 0400-MAIN-LOGIC.
           MOVE '5'                    TO  PI-OPTION.
           MOVE WS-CERT-AIX05-DSID     TO  PI-DSID.
           MOVE PI-COMPANY-CD          TO  PI-CK-COMPANY-CD
                                           PI-SC-COMPANY-CD.
           MOVE AMEMBERI               TO  PI-CK-MEMBER-NO
                                           PI-SC-MEMBER-NO
                                           WS-INPUT-FIELD.
           PERFORM 0320-MAIN-LOGIC THRU 0320-MAIN-LOGIC-EXIT
               VARYING INPUT-INDEX FROM AMEMBERL BY -1
                   UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE.
           ADD +1  AMEMBERL  GIVING  PI-KEY-LENGTH.
           MOVE -1                     TO  AMEMBERL.
           PERFORM 4000-READ-CERT-FILE.
       0320-MAIN-LOGIC.
           SUBTRACT +1 FROM AMEMBERL.
       0320-MAIN-LOGIC-EXIT.
           EXIT.
       0400-MAIN-LOGIC.
           MOVE +1                     TO  PI-KEY-LENGTH.
           IF  PART-KEY-ON
               MOVE PI-SELECTION-CRITERIA  TO  PI-CERTIFICATE-KEY
               MOVE WS-KEY-LENGTH      TO  PI-KEY-LENGTH.
           MOVE WS-CERT-MASTER-DSID    TO  PI-DSID.
           MOVE ZERO                   TO  PI-OPTION.
           MOVE -1                     TO  ACRTNO4L.
           PERFORM 4000-READ-CERT-FILE.
           EJECT
       3900-READNEXT-CERT-FILE SECTION.
           
      * EXEC CICS HANDLE CONDITION
      *        DUPKEY (9300-XCTL)
      *        NOTFND (3980-NOTFND)
      *        DSIDERR (3970-DSIDERR)
      *    END-EXEC.
      *    MOVE '"$$I"                 ! % #00005225' TO DFHEIV0
           MOVE X'222424492220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303035323235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS ENDBR
      *         DATASET (WS-ACCOUNT-MASTER-DSID)
      *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005230' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACCOUNT-MASTER-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS STARTBR
      *        DATASET   (PI-DSID)
      *        RIDFLD    (PI-CERTIFICATE-KEY)
      *        KEYLENGTH (PI-KEY-LENGTH)
      *        GENERIC
      *        GTEQ
      *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    G          &   #00005233' TO DFHEIV0
           MOVE X'262C2020204B472020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 PI-CERTIFICATE-KEY, 
                 PI-KEY-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       3900-READNEXT-CONTINUE.
           
      * EXEC CICS READNEXT
      *        DATASET   (PI-DSID)
      *        RIDFLD    (PI-CERTIFICATE-KEY)
      *        SET       (ADDRESS OF PURGE-CERT-MASTER)
      *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005241' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CERTIFICATE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PURGE-CERT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           GO TO 3999-EXIT.
       3970-DSIDERR.
           MOVE ER-0671                TO  EMI-ERROR.
           MOVE -1                     TO  APFKL.
           GO TO 8200-SEND-DATAONLY.
       3980-NOTFND.
           GO TO 3900-READNEXT-CONTINUE.
       3999-EXIT. EXIT.
       4000-READ-CERT-FILE SECTION.
           
      * EXEC CICS HANDLE CONDITION
      *        DUPKEY (9300-XCTL)
      *        NOTFND (4080-NOTFND)
      *        DSIDERR (4070-DSIDERR)
      *    END-EXEC.
      *    MOVE '"$$I"                 ! & #00005255' TO DFHEIV0
           MOVE X'222424492220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303035323535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
      **   IF  PART-KEY-ON
      *        MOVE WS-CERT-MASTER-DSID    TO  PI-DSID
      *        PERFORM 3900-READNEXT-CERT-FILE THRU 3999-EXIT
      *        MOVE 'N'          TO  PART-KEY-ON-SW
      *        EXEC CICS ENDBR
      *           DATASET   (PI-DSID)
      *        END-EXEC
      *        GO TO 4070-CONTINUE.
           IF (PI-DSID = WS-CERT-MASTER-DSID AND
               PI-KEY-LENGTH LESS +33)
             OR
              (PI-DSID = WS-CERT-AIX01-DSID AND
               PI-KEY-LENGTH LESS +18)
             OR
              (PI-DSID = WS-CERT-AIX02-DSID AND
               PI-KEY-LENGTH LESS +12)
             OR
              (PI-DSID = WS-CERT-AIX04-DSID AND
               PI-KEY-LENGTH LESS +12)
             OR
              (PI-DSID = WS-CERT-AIX05-DSID AND
               PI-KEY-LENGTH LESS +13)
                   MOVE +1             TO  PI-START-SW
                   
      * EXEC CICS READ
      *                DATASET   (PI-DSID)
      *                RIDFLD    (PI-CERTIFICATE-KEY)
      *                SET       (ADDRESS OF PURGE-CERT-MASTER)
      *                GENERIC
      *                EQUAL
      *                KEYLENGTH (PI-KEY-LENGTH)
      *            END-EXEC
      *    MOVE '&"S  KG    E          (   #00005283' TO DFHEIV0
           MOVE X'26225320204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CERTIFICATE-KEY, 
                 PI-KEY-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PURGE-CERT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
               ELSE
                   MOVE ZERO           TO  PI-START-SW
                   
      * EXEC CICS READ
      *                DATASET   (PI-DSID)
      *                RIDFLD    (PI-CERTIFICATE-KEY)
      *                SET       (ADDRESS OF PURGE-CERT-MASTER)
      *            END-EXEC.
      *    MOVE '&"S        E          (   #00005293' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CERTIFICATE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PURGE-CERT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       4070-CONTINUE.
           GO TO 9300-XCTL.
       4070-DSIDERR.
           MOVE ER-0671                TO  EMI-ERROR.
           MOVE -1                     TO  APFKL.
           GO TO 8200-SEND-DATAONLY.
       4080-NOTFND.
      *    MOVE -1                     TO  ACRTNO4L.
           MOVE -1                     TO  APFKL.
           MOVE ER-0201                TO  EMI-ERROR.
           GO TO 8200-SEND-DATAONLY.
       4090-EXIT.
           EXIT.
           EJECT
       5500-WRITE-SECURITY-TEMP-STORE  SECTION.
           
      * EXEC CICS HANDLE CONDITION
      *        QIDERR   (5501-WRITE-SECURITY)
      *    END-EXEC.
      *    MOVE '"$N                   ! '' #00005313' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303035333133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE EIBTRMID               TO  QID.
           
      * EXEC CICS DELETEQ TS
      *        QUEUE   (QID)
      *    END-EXEC.
      *    MOVE '*&                    #   #00005317' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       5501-WRITE-SECURITY.
           
      * EXEC CICS WRITEQ TS
      *        QUEUE   (QID)
      *        FROM    (SECURITY-CONTROL)
      *        LENGTH  (SC-COMM-LENGTH)
      *        ITEM    (QID-ITEM)
      *    END-EXEC.
      *    MOVE '*" I                  ''   #00005321' TO DFHEIV0
           MOVE X'2A2220492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 QID-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE QID                    TO  PI-SECURITY-TEMP-STORE-ID.
           IF PI-PROCESSOR-ID IS EQUAL TO 'LGXX'
               MOVE ALL 'Y'            TO  SC-CREDIT-CODES
                                           SC-CLAIMS-CODES
                                           PI-PROCESSOR-USER-ALMIGHTY.
       5500-EXIT.
           EXIT.
       EJECT
       6000-READ-CONTROL SECTION.
           
      * EXEC CICS HANDLE CONDITION
      *         NOTFND (6000-NOT-FOUND)
      *    END-EXEC.
      *    MOVE '"$I                   ! ( #00005336' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303035333336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS READ
      *         DATASET  (WS-CONTROL-FILE-DSID)
      *         RIDFLD   (WS-CNTL-KEY)
      *         SET      (ADDRESS OF CONTROL-FILE)
      *    END-EXEC.
      *    MOVE '&"S        E          (   #00005339' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           CONTINUE.
           MOVE 'Y'                    TO  WS-CNTL-REC-FOUND-SW.
           GO TO 6000-EXIT.
       6000-NOT-FOUND.
           MOVE 'N'                    TO  WS-CNTL-REC-FOUND-SW.
       6000-EXIT.
           EXIT.
       6010-READ-CONTROL-UPDATE.
            
      * EXEC CICS HANDLE CONDITION
      *         NOTFND    (6010-NOTFND)
      *     END-EXEC.
      *    MOVE '"$I                   ! ) #00005352' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303035333532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
            
      * EXEC CICS READ
      *         DATASET   (WS-CONTROL-FILE-DSID)
      *         RIDFLD    (WS-CNTL-KEY)
      *         SET       (ADDRESS OF CONTROL-FILE)
      *         UPDATE
      *     END-EXEC.
      *    MOVE '&"S        EU         (   #00005355' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
            CONTINUE.
            MOVE 'Y'                       TO  WS-CNTL-REC-FOUND-SW.
            GO TO 6010-EXIT.
       6010-NOTFND.
            MOVE 'N'                       TO  WS-CNTL-REC-FOUND-SW.
       6010-EXIT.
           EXIT.
       6020-REWRITE-CONTROL.
           
      * EXEC CICS REWRITE
      *        DATASET   (WS-CONTROL-FILE-DSID)
      *        FROM      (CONTROL-FILE)
      *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005369' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       6020-EXIT.
           EXIT.
           EJECT
       7000-BUILD-SCREEN     SECTION.
      ******************************************************************
      *          REBUILD ORIGNAL SCREEN AND ERROR MESSAGE IF EL1282    *
      *          DID NOT FIND ANY CERTIFICATES DURING BROWSE OF FILE.  *
      ******************************************************************
           IF EIBTRNID = WS-TRANS-ID
              NEXT SENTENCE
             ELSE
              GO TO 7099-EXIT.
      ***
      ******** PI-BROWSE-SW = 9  MEANS NO RECORDS FOUND (SET IN EL1282)
      ***
           IF  PI-BROWSE-SW = +9
               NEXT SENTENCE
              ELSE
               GO TO 7099-EXIT.
            IF OPTION-TWO-SELECTED
               GO TO 7099-EXIT.
           MOVE LOW-VALUES             TO  EL128AO.
           MOVE ER-2373                TO  EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT.
           IF PI-ALT-NAME-COUNT GREATER 140
               MOVE ER-0765            TO  EMI-ERROR.
       7010-OPTION-ONE.
            IF OPTION-ONE-SELECTED
               NEXT SENTENCE
              ELSE
               GO TO 7030-OPTION-THREE.
            MOVE -1                       TO  ACRTNO4L.
            IF  PI-SC-CERT-PRIME-A4 GREATER SPACES
                MOVE PI-SC-CERT-PRIME-A4  TO  ACRTNO4O
                MOVE AL-UANON             TO  ACRTNO4A.
            IF  PI-SC-CERT-SFX-A4   GREATER SPACES
                MOVE PI-SC-CERT-SFX-A4    TO  ACRTSX4O
                MOVE AL-UANON             TO  ACRTSX4A.
           GO TO   7090-INITIALIZE-WORK-AREAS.
       7030-OPTION-THREE.
           IF OPTION-THREE-SELECTED
               NEXT SENTENCE
             ELSE
              GO TO 7040-OPTION-FOUR.
           MOVE -1                          TO  ALNAMEL.
           IF  PI-SC-LAST-NAME GREATER SPACES
               MOVE PI-SC-LAST-NAME         TO  ALNAMEO
               MOVE AL-UANON                TO  ALNAMEA.
           IF  PI-SC-FIRST-NAME GREATER SPACES
               MOVE PI-SC-FIRST-NAME        TO  AFNAMEO
               MOVE AL-UANON                TO  AFNAMEA.
           IF  PI-SC-INITIALS  GREATER SPACES
               MOVE PI-SC-INITIALS          TO  WS-INITIALS
               IF  WS-INITIAL-MIDDLE GREATER SPACES
                   MOVE WS-INITIAL-MIDDLE   TO  AINITALO
                   MOVE AL-UANON            TO  AINITALA.
           IF  PI-SC-ACCT-NO   GREATER SPACES
               MOVE PI-SC-ACCT-NO           TO  AACCT2O
               MOVE AL-UANON                TO  AACCT2A.
           GO TO   7090-INITIALIZE-WORK-AREAS.
       7040-OPTION-FOUR.
           IF OPTION-FOUR-SELECTED
               NEXT SENTENCE
           ELSE
               GO TO 7050-OPTION-FIVE.
           MOVE -1                          TO  ASSNL.
           IF PI-SC-SOC-SEC-NO GREATER SPACES
              MOVE PI-SC-SOC-SEC-NO         TO  ASSNO
              MOVE AL-UANON                 TO  ASSNA.
           GO TO   7090-INITIALIZE-WORK-AREAS.
       7050-OPTION-FIVE.
           MOVE -1                     TO  AMEMBERL.
           IF PI-SC-MEMBER-NO GREATER SPACES
              MOVE PI-SC-MEMBER-NO     TO  AMEMBERO
              MOVE AL-UANON            TO  AMEMBERA.
       7090-INITIALIZE-WORK-AREAS.
           MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA
                                           PI-CONTROL-IN-PROGRESS.
           MOVE ZERO                   TO  PI-1ST-TIME-SW
                                           PI-LINE-COUNT
                                           PI-BROWSE-SW
                                           PI-KEY-LENGTH
                                           PI-TS-ITEM
                                           PI-END-OF-FILE
                                           PI-START-SW
                                           PI-AIX-RECORD-COUNT.
           GO TO 8200-SEND-DATAONLY.
       7099-EXIT.
           EXIT.
           EJECT
       8100-SEND-INITIAL-MAP SECTION.
           MOVE PI-COMPANY-ID          TO  WS-CNTL-ID.
           MOVE '1'                    TO  WS-CNTL-TYPE.
           MOVE SPACES                 TO  WS-CNTL-USER.
           MOVE +0                     TO  WS-CNTL-SEQ.
           PERFORM 6000-READ-CONTROL THRU 6000-EXIT.
           IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'
               MOVE ER-0022            TO  EMI-ERROR
               MOVE -1                 TO  APFKL
               GO TO 8200-SEND-DATAONLY.
           IF SOC-SEC-NO-USED
              MOVE AL-SANOF            TO  AOPT4A ASSOPTA
              MOVE AL-UANOF            TO  ASSNA
              MOVE 'SOCIAL SECURITY NUMBER ' TO  ASSOPTO
              MOVE '** OPTION 4 **'    TO  AOPT4O
           ELSE
              MOVE AL-SANOF            TO  AOPT4A ASSOPTA ASSNA.
           IF MEMBER-NO-USED
              MOVE AL-SANOF            TO  AOPT5A AMEOPTA
              MOVE AL-UANOF            TO  AMEMBERA
              MOVE '** OPTION 5 **'    TO  AOPT5O
              IF CF-MEMBER-CAPTION = SPACES
                 MOVE 'MEMBER NUMBER ' TO  AMEOPTO
              ELSE
                 MOVE CF-MEMBER-CAPTION TO AMEOPTO
           ELSE
              MOVE AL-SANOF            TO  AOPT5A AMEOPTA AMEMBERA.
           MOVE -1                     TO  ACRTNO4L
           MOVE SAVE-DATE              TO ADATEO
           MOVE EIBTIME                TO TIME-IN
           MOVE TIME-OUT               TO ATIMEO
           IF EMI-ERROR NOT = ZERO
               PERFORM 9900-ERROR-FORMAT.
           MOVE EMI-MESSAGE-AREA (1)   TO AEMSG1O
           MOVE EMI-MESSAGE-AREA (2)   TO AEMSG2O
           MOVE PI-COMPANY-ID          TO ACOMPO
           
      * EXEC CICS SEND
      *        FROM   (EL128AO)
      *        MAPSET (WS-MAPSET-NAME)
      *        MAP    (WS-MAP-NAME)
      *        CURSOR
      *        ERASE
      *    END-EXEC.
           MOVE LENGTH OF
            EL128AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005499' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL128AO, 
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
           
           GO TO 9100-RETURN-TRAN.
       8100-EXIT.
           EXIT.
           EJECT
       8200-SEND-DATAONLY SECTION.
           MOVE SAVE-DATE              TO  ADATEO.
           MOVE EIBTIME                TO  TIME-IN.
           MOVE TIME-OUT               TO  ATIMEO.
           IF EMI-ERROR NOT = ZERO
               PERFORM 9900-ERROR-FORMAT.
           MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.
           MOVE EMI-MESSAGE-AREA (2)    TO  AEMSG2O.
           MOVE PI-COMPANY-ID          TO  ACOMPO
           
      * EXEC CICS SEND DATAONLY
      *        FROM   (EL128AO)
      *        MAPSET (WS-MAPSET-NAME)
      *        MAP    (WS-MAP-NAME)
      *        CURSOR
      *    END-EXEC.
           MOVE LENGTH OF
            EL128AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00005519' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL128AO, 
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
           
           GO TO 9100-RETURN-TRAN.
       8200-EXIT.
           EXIT.
           EJECT
       8300-SEND-TEXT SECTION.
           
      * EXEC CICS SEND TEXT
      *        FROM   (LOGOFF-TEXT)
      *        LENGTH (LOGOFF-LENGTH)
      *        ERASE
      *        FREEKB
      *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005530' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353330' TO DFHEIV0(25:11)
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
      *    MOVE '.(                    &   #00005536' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353336' TO DFHEIV0(25:11)
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
       8500-DATE-CONVERSION SECTION.
           
      * EXEC CICS LINK
      *        PROGRAM  ('ELDATCV')
      *        COMMAREA (DATE-CONVERSION-DATA)
      *        LENGTH   (DC-COMM-LENGTH)
      *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00005542' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353432' TO DFHEIV0(25:11)
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
       8600-DEEDIT SECTION.
           
      * EXEC CICS BIF DEEDIT
      *        FIELD  (WS-DEEDIT-FIELD)
      *        LENGTH (15)
      *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005550' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       8600-EXIT.
           EXIT.
           EJECT
       9000-RETURN-CICS SECTION.
           MOVE 'EL005'                TO  THIS-PGM.
           MOVE EIBAID                 TO  PI-ENTRY-CD-1.
           GO TO 9300-XCTL.
       9000-EXIT.
           EXIT.
       9100-RETURN-TRAN SECTION.
           MOVE EMI-ERROR-NUMBER (1)  TO  PI-LAST-ERROR-NO.
           MOVE WS-MAP-NUMBER         TO  PI-CURRENT-SCREEN-NO.
           
      * EXEC CICS RETURN
      *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
      *        LENGTH   (PI-COMM-LENGTH)
      *        TRANSID  (WS-TRANS-ID)
      *    END-EXEC.
      *    MOVE '.(CT                  &   #00005566' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9100-EXIT.
           EXIT.
       9300-XCTL SECTION.
           MOVE DFHENTER               TO  EIBAID.
           MOVE PART-KEY-ON-SW         TO  PI-PART-KEY-SW.
           MOVE PART-FIELD-ON-SW       TO  PI-PART-FIELD-SW.
           MOVE ' '                    TO  PART-FIELD-ON-SW.
           
      * EXEC CICS XCTL
      *        PROGRAM  (THIS-PGM)
      *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
      *        LENGTH   (PI-COMM-LENGTH)
      *    END-EXEC.
      *    MOVE '.$C                   $   #00005578' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9300-EXIT.
           EXIT.
           EJECT
       9400-CLEAR SECTION.
           MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.
           GO TO 9300-XCTL.
       9400-EXIT.
           EXIT.
       9600-PGMIDERR SECTION.
           
      * EXEC CICS HANDLE CONDITION
      *        PGMIDERR (8300-SEND-TEXT)
      *    END-EXEC.
      *    MOVE '"$L                   ! * #00005592' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303035353932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE THIS-PGM               TO  PI-CALLING-PROGRAM
                                           LOGOFF-PGM.
           MOVE 'EL005'                TO  THIS-PGM.
           MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
           MOVE SPACES                 TO  PI-ENTRY-CD-1.
           GO TO 9300-XCTL.
       9600-EXIT.
           EXIT.
           EJECT
       9900-ERROR-FORMAT SECTION.
           
      * EXEC CICS LINK
      *        PROGRAM  ('EL001')
      *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
      *        LENGTH   (EMI-COMM-LENGTH)
      *    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   ''   #00005605' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9900-EXIT.
           EXIT.
       9990-ERROR SECTION.
           MOVE DFHEIBLK               TO EMI-LINE1.
           
      * EXEC CICS LINK
      *        PROGRAM  ('EL004')
      *        COMMAREA (EMI-LINE1)
      *        LENGTH   (72)
      *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00005614' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           GO TO 8200-SEND-DATAONLY.
       9990-EXIT.
           EXIT.
       9995-SECURITY-VIOLATION.
      *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00005640' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363430' TO DFHEIV0(25:11)
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
       9999-LAST-PARAGRAPH SECTION.
           
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL128' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL128' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMIDERR,
                     0030-MAIN-LOGIC,
                     0030-MAIN-LOGIC,
                     9990-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0030-MAIN-LOGIC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0050-MAIN-LOGIC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 9300-XCTL,
                     3980-NOTFND,
                     3970-DSIDERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 9300-XCTL,
                     4080-NOTFND,
                     4070-DSIDERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 5501-WRITE-SECURITY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 6000-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 6010-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL128' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
