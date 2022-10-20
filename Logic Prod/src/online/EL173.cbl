00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL173 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/13/96 09:32:25.
00007 *                            VMOD=2.003
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
00019 *                                                                *
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00022 *            *                                                   *
00023 *            *****************************************************
00024
00025 *REMARKS.
00026 *        THIS PROGRAM PRODUCES A REPORT SHOWING ALL CLAIMS THAT
00027 *    HAVE BEEN FLAGGED AS NEEDING A SUPERVISOR'S ATTENTION.
00028
00029 *    SCREENS     - EL173A - REQUEST FOR REVIEW
00030
00031 *    ENTERED BY  - EL171  - REPORT MENU
00032
00033 *    EXIT TO     - EL171  - RESULT OF CLEAR OR END OF JOB
00034
00035 *    INPUT FILES - ELMSTR - CLAIM MASTER
00036
00037 *    OUTPUT FILES - NONE
00038
00039 *    COMMAREA    - PASSED.
00040
00041
00042 *    NARRATIVE   - ALL CLAIM RECORDS ARE READ TO DETERMINE WHETHER
00043 *                  ATTENTION HAS BEEN REQUESTED (CL-SUPV-ATTN-CD =
00044 *                  "Y").  AS PAGES ARE COMPLETED THEY ARE WRITTEN
00045 *                  TO TEMPORARY STORAGE.
00046
00047 *                  IN THE BROWSING MODE, PAGE NUMBER IS USED AS A
00048 *                  REFERENCE FOR SETTING THE ITEM NUMBER IN THE
00049 *                  T/S READ.
00050
00051      EJECT
00052  ENVIRONMENT DIVISION.
00053
00054  DATA DIVISION.
00055
00056  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00057
00058  77  FILLER  PIC X(32)  VALUE '********************************'.
00059  77  FILLER  PIC X(32)  VALUE '*   EL173  WORKING STORAGE     *'.
00060  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.003 *********'.
00061
00062      EJECT
00063 *                            COPY ELCSCTM.
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
00064
00065 *                            COPY ELCSCRTY.
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
00066
00067      EJECT
00068  01  WS-DATE-AREA.
00069      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00070      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
00071
00072  01  FILLER                          COMP-3.
00073      05  WS-READNEXT-SW              PIC S9          VALUE ZERO.
00074
00075      05  TIME-IN                     PIC S9(7)       VALUE ZERO.
00076      05  TIME-OUT                    REDEFINES
00077          TIME-IN                     PIC S9(3)V9(4).
00078
00079  01  FILLER                          COMP SYNC.
00080      05  WS-TS-LENGTH                PIC S9(4)       VALUE +1349.
00081      05  SC-ITEM                     PIC S9(4)       VALUE +0001.
00082
00083
00084  01  FILLER.
00085      05  WS-MAPSET-NAME              PIC X(8)      VALUE 'EL173S'.
00086      05  WS-MAP-NAME                 PIC X(8)      VALUE 'EL173A'.
00087
00088      05  FILLER                      REDEFINES
00089          WS-MAP-NAME.
00090          20  FILLER                  PIC XX.
00091          20  WS-MAP-NUMBER           PIC X(6).
00092
00093      05  THIS-PGM                    PIC X(8)      VALUE 'EL173'.
00094
00095      05  WS-CLAIM-MASTER-DSID        PIC X(8) VALUE 'ELMSTR'.
00096
00097      05  WS-TRANS-ID                 PIC X(4)        VALUE 'EX03'.
00098
00099      05  ER-0004                     PIC 9(4)        VALUE 0004.
00100      05  ER-0008                     PIC 9(4)        VALUE 0008.
00101      05  ER-0029                     PIC 9(4)        VALUE 0029.
00102      05  ER-0047                     PIC 9(4)        VALUE 0047.
00103      05  ER-0070                     PIC 9(4)        VALUE 0070.
00104      05  ER-0313                     PIC 9(4)        VALUE 0313.
00105
00106      05  WS-TEMP-STORAGE-KEY.
00107          10  WS-TS-TERM-ID           PIC X(4).
00108          10  FILLER                  PIC X(4)        VALUE '173'.
00109
00110      05  WS-TEMP-STORAGE-ITEM        PIC S9(4)       VALUE ZERO
00111                                      COMP SYNC.
00112
00113      EJECT
00114 *                                    COPY ELCINTF.
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
00115      12  FILLER                      REDEFINES
00116          PI-PROGRAM-WORK-AREA.
00117          16  PI-CLAIM-KEY.
00118              20  PI-CK-COMPANY-CODE     PIC X.
00119              20  PI-CK-CARRIER          PIC X.
00120              20  PI-CK-CLAIM-NO         PIC X(7).
00121              20  PI-CK-CERT-NO          PIC X(11).
00122
00123          16  PI-PREV-CLAIM-KEY.
00124              20  PI-PREV-CK-COMPANY-CODE     PIC X.
00125              20  PI-PREV-CK-CARRIER          PIC X.
00126              20  PI-PREV-CK-CLAIM-NO         PIC X(7).
00127              20  PI-PREV-CK-CERT-NO          PIC X(11).
00128
00129          16  PI-TEMP-STORAGE-ITEM    PIC S9(4)
00130                                      COMP SYNC.
00131
00132          16  PI-LAST-ITEM-NO         PIC S9(4)
00133                                      COMP SYNC.
00134
00135          16  PI-END-OF-FILE          PIC S9
00136                                      COMP-3.
00137          16  FILLER                  PIC X(595).
00138
00139      EJECT
00140 *                                    COPY EL173S SUPPRESS.
       01  EL173AI.
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
           05  APAGEL PIC S9(0004) COMP.
           05  APAGEF PIC  X(0001).
           05  FILLER REDEFINES APAGEF.
               10  APAGEA PIC  X(0001).
           05  APAGEI PIC  S9(4).
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
           05  ATYPE01L PIC S9(0004) COMP.
           05  ATYPE01F PIC  X(0001).
           05  FILLER REDEFINES ATYPE01F.
               10  ATYPE01A PIC  X(0001).
           05  ATYPE01I PIC  X(0004).
      *    -------------------------------
           05  ABY01L PIC S9(0004) COMP.
           05  ABY01F PIC  X(0001).
           05  FILLER REDEFINES ABY01F.
               10  ABY01A PIC  X(0001).
           05  ABY01I PIC  X(0004).
      *    -------------------------------
           05  AEDT01L PIC S9(0004) COMP.
           05  AEDT01F PIC  X(0001).
           05  FILLER REDEFINES AEDT01F.
               10  AEDT01A PIC  X(0001).
           05  AEDT01I PIC  X(0008).
      *    -------------------------------
           05  AIDT01L PIC S9(0004) COMP.
           05  AIDT01F PIC  X(0001).
           05  FILLER REDEFINES AIDT01F.
               10  AIDT01A PIC  X(0001).
           05  AIDT01I PIC  X(0008).
      *    -------------------------------
           05  AFILE01L PIC S9(0004) COMP.
           05  AFILE01F PIC  X(0001).
           05  FILLER REDEFINES AFILE01F.
               10  AFILE01A PIC  X(0001).
           05  AFILE01I PIC  X(0004).
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
           05  ATYPE02L PIC S9(0004) COMP.
           05  ATYPE02F PIC  X(0001).
           05  FILLER REDEFINES ATYPE02F.
               10  ATYPE02A PIC  X(0001).
           05  ATYPE02I PIC  X(0004).
      *    -------------------------------
           05  ABY02L PIC S9(0004) COMP.
           05  ABY02F PIC  X(0001).
           05  FILLER REDEFINES ABY02F.
               10  ABY02A PIC  X(0001).
           05  ABY02I PIC  X(0004).
      *    -------------------------------
           05  AEDT02L PIC S9(0004) COMP.
           05  AEDT02F PIC  X(0001).
           05  FILLER REDEFINES AEDT02F.
               10  AEDT02A PIC  X(0001).
           05  AEDT02I PIC  X(0008).
      *    -------------------------------
           05  AIDT02L PIC S9(0004) COMP.
           05  AIDT02F PIC  X(0001).
           05  FILLER REDEFINES AIDT02F.
               10  AIDT02A PIC  X(0001).
           05  AIDT02I PIC  X(0008).
      *    -------------------------------
           05  AFILE02L PIC S9(0004) COMP.
           05  AFILE02F PIC  X(0001).
           05  FILLER REDEFINES AFILE02F.
               10  AFILE02A PIC  X(0001).
           05  AFILE02I PIC  X(0004).
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
           05  ATYPE03L PIC S9(0004) COMP.
           05  ATYPE03F PIC  X(0001).
           05  FILLER REDEFINES ATYPE03F.
               10  ATYPE03A PIC  X(0001).
           05  ATYPE03I PIC  X(0004).
      *    -------------------------------
           05  ABY03L PIC S9(0004) COMP.
           05  ABY03F PIC  X(0001).
           05  FILLER REDEFINES ABY03F.
               10  ABY03A PIC  X(0001).
           05  ABY03I PIC  X(0004).
      *    -------------------------------
           05  AEDT03L PIC S9(0004) COMP.
           05  AEDT03F PIC  X(0001).
           05  FILLER REDEFINES AEDT03F.
               10  AEDT03A PIC  X(0001).
           05  AEDT03I PIC  X(0008).
      *    -------------------------------
           05  AIDT03L PIC S9(0004) COMP.
           05  AIDT03F PIC  X(0001).
           05  FILLER REDEFINES AIDT03F.
               10  AIDT03A PIC  X(0001).
           05  AIDT03I PIC  X(0008).
      *    -------------------------------
           05  AFILE03L PIC S9(0004) COMP.
           05  AFILE03F PIC  X(0001).
           05  FILLER REDEFINES AFILE03F.
               10  AFILE03A PIC  X(0001).
           05  AFILE03I PIC  X(0004).
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
           05  ATYPE04L PIC S9(0004) COMP.
           05  ATYPE04F PIC  X(0001).
           05  FILLER REDEFINES ATYPE04F.
               10  ATYPE04A PIC  X(0001).
           05  ATYPE04I PIC  X(0004).
      *    -------------------------------
           05  ABY04L PIC S9(0004) COMP.
           05  ABY04F PIC  X(0001).
           05  FILLER REDEFINES ABY04F.
               10  ABY04A PIC  X(0001).
           05  ABY04I PIC  X(0004).
      *    -------------------------------
           05  AEDT04L PIC S9(0004) COMP.
           05  AEDT04F PIC  X(0001).
           05  FILLER REDEFINES AEDT04F.
               10  AEDT04A PIC  X(0001).
           05  AEDT04I PIC  X(0008).
      *    -------------------------------
           05  AIDT04L PIC S9(0004) COMP.
           05  AIDT04F PIC  X(0001).
           05  FILLER REDEFINES AIDT04F.
               10  AIDT04A PIC  X(0001).
           05  AIDT04I PIC  X(0008).
      *    -------------------------------
           05  AFILE04L PIC S9(0004) COMP.
           05  AFILE04F PIC  X(0001).
           05  FILLER REDEFINES AFILE04F.
               10  AFILE04A PIC  X(0001).
           05  AFILE04I PIC  X(0004).
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
           05  ATYPE05L PIC S9(0004) COMP.
           05  ATYPE05F PIC  X(0001).
           05  FILLER REDEFINES ATYPE05F.
               10  ATYPE05A PIC  X(0001).
           05  ATYPE05I PIC  X(0004).
      *    -------------------------------
           05  ABY05L PIC S9(0004) COMP.
           05  ABY05F PIC  X(0001).
           05  FILLER REDEFINES ABY05F.
               10  ABY05A PIC  X(0001).
           05  ABY05I PIC  X(0004).
      *    -------------------------------
           05  AEDT05L PIC S9(0004) COMP.
           05  AEDT05F PIC  X(0001).
           05  FILLER REDEFINES AEDT05F.
               10  AEDT05A PIC  X(0001).
           05  AEDT05I PIC  X(0008).
      *    -------------------------------
           05  AIDT05L PIC S9(0004) COMP.
           05  AIDT05F PIC  X(0001).
           05  FILLER REDEFINES AIDT05F.
               10  AIDT05A PIC  X(0001).
           05  AIDT05I PIC  X(0008).
      *    -------------------------------
           05  AFILE05L PIC S9(0004) COMP.
           05  AFILE05F PIC  X(0001).
           05  FILLER REDEFINES AFILE05F.
               10  AFILE05A PIC  X(0001).
           05  AFILE05I PIC  X(0004).
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
           05  ATYPE06L PIC S9(0004) COMP.
           05  ATYPE06F PIC  X(0001).
           05  FILLER REDEFINES ATYPE06F.
               10  ATYPE06A PIC  X(0001).
           05  ATYPE06I PIC  X(0004).
      *    -------------------------------
           05  ABY06L PIC S9(0004) COMP.
           05  ABY06F PIC  X(0001).
           05  FILLER REDEFINES ABY06F.
               10  ABY06A PIC  X(0001).
           05  ABY06I PIC  X(0004).
      *    -------------------------------
           05  AEDT06L PIC S9(0004) COMP.
           05  AEDT06F PIC  X(0001).
           05  FILLER REDEFINES AEDT06F.
               10  AEDT06A PIC  X(0001).
           05  AEDT06I PIC  X(0008).
      *    -------------------------------
           05  AIDT06L PIC S9(0004) COMP.
           05  AIDT06F PIC  X(0001).
           05  FILLER REDEFINES AIDT06F.
               10  AIDT06A PIC  X(0001).
           05  AIDT06I PIC  X(0008).
      *    -------------------------------
           05  AFILE06L PIC S9(0004) COMP.
           05  AFILE06F PIC  X(0001).
           05  FILLER REDEFINES AFILE06F.
               10  AFILE06A PIC  X(0001).
           05  AFILE06I PIC  X(0004).
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
           05  ATYPE07L PIC S9(0004) COMP.
           05  ATYPE07F PIC  X(0001).
           05  FILLER REDEFINES ATYPE07F.
               10  ATYPE07A PIC  X(0001).
           05  ATYPE07I PIC  X(0004).
      *    -------------------------------
           05  ABY07L PIC S9(0004) COMP.
           05  ABY07F PIC  X(0001).
           05  FILLER REDEFINES ABY07F.
               10  ABY07A PIC  X(0001).
           05  ABY07I PIC  X(0004).
      *    -------------------------------
           05  AEDT07L PIC S9(0004) COMP.
           05  AEDT07F PIC  X(0001).
           05  FILLER REDEFINES AEDT07F.
               10  AEDT07A PIC  X(0001).
           05  AEDT07I PIC  X(0008).
      *    -------------------------------
           05  AIDT07L PIC S9(0004) COMP.
           05  AIDT07F PIC  X(0001).
           05  FILLER REDEFINES AIDT07F.
               10  AIDT07A PIC  X(0001).
           05  AIDT07I PIC  X(0008).
      *    -------------------------------
           05  AFILE07L PIC S9(0004) COMP.
           05  AFILE07F PIC  X(0001).
           05  FILLER REDEFINES AFILE07F.
               10  AFILE07A PIC  X(0001).
           05  AFILE07I PIC  X(0004).
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
           05  ATYPE08L PIC S9(0004) COMP.
           05  ATYPE08F PIC  X(0001).
           05  FILLER REDEFINES ATYPE08F.
               10  ATYPE08A PIC  X(0001).
           05  ATYPE08I PIC  X(0004).
      *    -------------------------------
           05  ABY08L PIC S9(0004) COMP.
           05  ABY08F PIC  X(0001).
           05  FILLER REDEFINES ABY08F.
               10  ABY08A PIC  X(0001).
           05  ABY08I PIC  X(0004).
      *    -------------------------------
           05  AEDT08L PIC S9(0004) COMP.
           05  AEDT08F PIC  X(0001).
           05  FILLER REDEFINES AEDT08F.
               10  AEDT08A PIC  X(0001).
           05  AEDT08I PIC  X(0008).
      *    -------------------------------
           05  AIDT08L PIC S9(0004) COMP.
           05  AIDT08F PIC  X(0001).
           05  FILLER REDEFINES AIDT08F.
               10  AIDT08A PIC  X(0001).
           05  AIDT08I PIC  X(0008).
      *    -------------------------------
           05  AFILE08L PIC S9(0004) COMP.
           05  AFILE08F PIC  X(0001).
           05  FILLER REDEFINES AFILE08F.
               10  AFILE08A PIC  X(0001).
           05  AFILE08I PIC  X(0004).
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
           05  ATYPE09L PIC S9(0004) COMP.
           05  ATYPE09F PIC  X(0001).
           05  FILLER REDEFINES ATYPE09F.
               10  ATYPE09A PIC  X(0001).
           05  ATYPE09I PIC  X(0004).
      *    -------------------------------
           05  ABY09L PIC S9(0004) COMP.
           05  ABY09F PIC  X(0001).
           05  FILLER REDEFINES ABY09F.
               10  ABY09A PIC  X(0001).
           05  ABY09I PIC  X(0004).
      *    -------------------------------
           05  AEDT09L PIC S9(0004) COMP.
           05  AEDT09F PIC  X(0001).
           05  FILLER REDEFINES AEDT09F.
               10  AEDT09A PIC  X(0001).
           05  AEDT09I PIC  X(0008).
      *    -------------------------------
           05  AIDT09L PIC S9(0004) COMP.
           05  AIDT09F PIC  X(0001).
           05  FILLER REDEFINES AIDT09F.
               10  AIDT09A PIC  X(0001).
           05  AIDT09I PIC  X(0008).
      *    -------------------------------
           05  AFILE09L PIC S9(0004) COMP.
           05  AFILE09F PIC  X(0001).
           05  FILLER REDEFINES AFILE09F.
               10  AFILE09A PIC  X(0001).
           05  AFILE09I PIC  X(0004).
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
           05  ATYPE10L PIC S9(0004) COMP.
           05  ATYPE10F PIC  X(0001).
           05  FILLER REDEFINES ATYPE10F.
               10  ATYPE10A PIC  X(0001).
           05  ATYPE10I PIC  X(0004).
      *    -------------------------------
           05  ABY10L PIC S9(0004) COMP.
           05  ABY10F PIC  X(0001).
           05  FILLER REDEFINES ABY10F.
               10  ABY10A PIC  X(0001).
           05  ABY10I PIC  X(0004).
      *    -------------------------------
           05  AEDT10L PIC S9(0004) COMP.
           05  AEDT10F PIC  X(0001).
           05  FILLER REDEFINES AEDT10F.
               10  AEDT10A PIC  X(0001).
           05  AEDT10I PIC  X(0008).
      *    -------------------------------
           05  AIDT10L PIC S9(0004) COMP.
           05  AIDT10F PIC  X(0001).
           05  FILLER REDEFINES AIDT10F.
               10  AIDT10A PIC  X(0001).
           05  AIDT10I PIC  X(0008).
      *    -------------------------------
           05  AFILE10L PIC S9(0004) COMP.
           05  AFILE10F PIC  X(0001).
           05  FILLER REDEFINES AFILE10F.
               10  AFILE10A PIC  X(0001).
           05  AFILE10I PIC  X(0004).
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
           05  ATYPE11L PIC S9(0004) COMP.
           05  ATYPE11F PIC  X(0001).
           05  FILLER REDEFINES ATYPE11F.
               10  ATYPE11A PIC  X(0001).
           05  ATYPE11I PIC  X(0004).
      *    -------------------------------
           05  ABY11L PIC S9(0004) COMP.
           05  ABY11F PIC  X(0001).
           05  FILLER REDEFINES ABY11F.
               10  ABY11A PIC  X(0001).
           05  ABY11I PIC  X(0004).
      *    -------------------------------
           05  AEDT11L PIC S9(0004) COMP.
           05  AEDT11F PIC  X(0001).
           05  FILLER REDEFINES AEDT11F.
               10  AEDT11A PIC  X(0001).
           05  AEDT11I PIC  X(0008).
      *    -------------------------------
           05  AIDT11L PIC S9(0004) COMP.
           05  AIDT11F PIC  X(0001).
           05  FILLER REDEFINES AIDT11F.
               10  AIDT11A PIC  X(0001).
           05  AIDT11I PIC  X(0008).
      *    -------------------------------
           05  AFILE11L PIC S9(0004) COMP.
           05  AFILE11F PIC  X(0001).
           05  FILLER REDEFINES AFILE11F.
               10  AFILE11A PIC  X(0001).
           05  AFILE11I PIC  X(0004).
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
           05  ATYPE12L PIC S9(0004) COMP.
           05  ATYPE12F PIC  X(0001).
           05  FILLER REDEFINES ATYPE12F.
               10  ATYPE12A PIC  X(0001).
           05  ATYPE12I PIC  X(0004).
      *    -------------------------------
           05  ABY12L PIC S9(0004) COMP.
           05  ABY12F PIC  X(0001).
           05  FILLER REDEFINES ABY12F.
               10  ABY12A PIC  X(0001).
           05  ABY12I PIC  X(0004).
      *    -------------------------------
           05  AEDT12L PIC S9(0004) COMP.
           05  AEDT12F PIC  X(0001).
           05  FILLER REDEFINES AEDT12F.
               10  AEDT12A PIC  X(0001).
           05  AEDT12I PIC  X(0008).
      *    -------------------------------
           05  AIDT12L PIC S9(0004) COMP.
           05  AIDT12F PIC  X(0001).
           05  FILLER REDEFINES AIDT12F.
               10  AIDT12A PIC  X(0001).
           05  AIDT12I PIC  X(0008).
      *    -------------------------------
           05  AFILE12L PIC S9(0004) COMP.
           05  AFILE12F PIC  X(0001).
           05  FILLER REDEFINES AFILE12F.
               10  AFILE12A PIC  X(0001).
           05  AFILE12I PIC  X(0004).
      *    -------------------------------
           05  ACALM13L PIC S9(0004) COMP.
           05  ACALM13F PIC  X(0001).
           05  FILLER REDEFINES ACALM13F.
               10  ACALM13A PIC  X(0001).
           05  ACALM13I PIC  X(0007).
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
           05  ATYPE13L PIC S9(0004) COMP.
           05  ATYPE13F PIC  X(0001).
           05  FILLER REDEFINES ATYPE13F.
               10  ATYPE13A PIC  X(0001).
           05  ATYPE13I PIC  X(0004).
      *    -------------------------------
           05  ABY13L PIC S9(0004) COMP.
           05  ABY13F PIC  X(0001).
           05  FILLER REDEFINES ABY13F.
               10  ABY13A PIC  X(0001).
           05  ABY13I PIC  X(0004).
      *    -------------------------------
           05  AEDT13L PIC S9(0004) COMP.
           05  AEDT13F PIC  X(0001).
           05  FILLER REDEFINES AEDT13F.
               10  AEDT13A PIC  X(0001).
           05  AEDT13I PIC  X(0008).
      *    -------------------------------
           05  AIDT13L PIC S9(0004) COMP.
           05  AIDT13F PIC  X(0001).
           05  FILLER REDEFINES AIDT13F.
               10  AIDT13A PIC  X(0001).
           05  AIDT13I PIC  X(0008).
      *    -------------------------------
           05  AFILE13L PIC S9(0004) COMP.
           05  AFILE13F PIC  X(0001).
           05  FILLER REDEFINES AFILE13F.
               10  AFILE13A PIC  X(0001).
           05  AFILE13I PIC  X(0004).
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
           05  ATYPE14L PIC S9(0004) COMP.
           05  ATYPE14F PIC  X(0001).
           05  FILLER REDEFINES ATYPE14F.
               10  ATYPE14A PIC  X(0001).
           05  ATYPE14I PIC  X(0004).
      *    -------------------------------
           05  ABY14L PIC S9(0004) COMP.
           05  ABY14F PIC  X(0001).
           05  FILLER REDEFINES ABY14F.
               10  ABY14A PIC  X(0001).
           05  ABY14I PIC  X(0004).
      *    -------------------------------
           05  AEDT14L PIC S9(0004) COMP.
           05  AEDT14F PIC  X(0001).
           05  FILLER REDEFINES AEDT14F.
               10  AEDT14A PIC  X(0001).
           05  AEDT14I PIC  X(0008).
      *    -------------------------------
           05  AIDT14L PIC S9(0004) COMP.
           05  AIDT14F PIC  X(0001).
           05  FILLER REDEFINES AIDT14F.
               10  AIDT14A PIC  X(0001).
           05  AIDT14I PIC  X(0008).
      *    -------------------------------
           05  AFILE14L PIC S9(0004) COMP.
           05  AFILE14F PIC  X(0001).
           05  FILLER REDEFINES AFILE14F.
               10  AFILE14A PIC  X(0001).
           05  AFILE14I PIC  X(0004).
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
           05  ATYPE15L PIC S9(0004) COMP.
           05  ATYPE15F PIC  X(0001).
           05  FILLER REDEFINES ATYPE15F.
               10  ATYPE15A PIC  X(0001).
           05  ATYPE15I PIC  X(0004).
      *    -------------------------------
           05  ABY15L PIC S9(0004) COMP.
           05  ABY15F PIC  X(0001).
           05  FILLER REDEFINES ABY15F.
               10  ABY15A PIC  X(0001).
           05  ABY15I PIC  X(0004).
      *    -------------------------------
           05  AEDT15L PIC S9(0004) COMP.
           05  AEDT15F PIC  X(0001).
           05  FILLER REDEFINES AEDT15F.
               10  AEDT15A PIC  X(0001).
           05  AEDT15I PIC  X(0008).
      *    -------------------------------
           05  AIDT15L PIC S9(0004) COMP.
           05  AIDT15F PIC  X(0001).
           05  FILLER REDEFINES AIDT15F.
               10  AIDT15A PIC  X(0001).
           05  AIDT15I PIC  X(0008).
      *    -------------------------------
           05  AFILE15L PIC S9(0004) COMP.
           05  AFILE15F PIC  X(0001).
           05  FILLER REDEFINES AFILE15F.
               10  AFILE15A PIC  X(0001).
           05  AFILE15I PIC  X(0004).
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
           05  ATYPE16L PIC S9(0004) COMP.
           05  ATYPE16F PIC  X(0001).
           05  FILLER REDEFINES ATYPE16F.
               10  ATYPE16A PIC  X(0001).
           05  ATYPE16I PIC  X(0004).
      *    -------------------------------
           05  ABY16L PIC S9(0004) COMP.
           05  ABY16F PIC  X(0001).
           05  FILLER REDEFINES ABY16F.
               10  ABY16A PIC  X(0001).
           05  ABY16I PIC  X(0004).
      *    -------------------------------
           05  AEDT16L PIC S9(0004) COMP.
           05  AEDT16F PIC  X(0001).
           05  FILLER REDEFINES AEDT16F.
               10  AEDT16A PIC  X(0001).
           05  AEDT16I PIC  X(0008).
      *    -------------------------------
           05  AIDT16L PIC S9(0004) COMP.
           05  AIDT16F PIC  X(0001).
           05  FILLER REDEFINES AIDT16F.
               10  AIDT16A PIC  X(0001).
           05  AIDT16I PIC  X(0008).
      *    -------------------------------
           05  AFILE16L PIC S9(0004) COMP.
           05  AFILE16F PIC  X(0001).
           05  FILLER REDEFINES AFILE16F.
               10  AFILE16A PIC  X(0001).
           05  AFILE16I PIC  X(0004).
      *    -------------------------------
           05  ACLAM17L PIC S9(0004) COMP.
           05  ACLAM17F PIC  X(0001).
           05  FILLER REDEFINES ACLAM17F.
               10  ACLAM17A PIC  X(0001).
           05  ACLAM17I PIC  X(0007).
      *    -------------------------------
           05  ACARR17L PIC S9(0004) COMP.
           05  ACARR17F PIC  X(0001).
           05  FILLER REDEFINES ACARR17F.
               10  ACARR17A PIC  X(0001).
           05  ACARR17I PIC  X(0001).
      *    -------------------------------
           05  ACERT17L PIC S9(0004) COMP.
           05  ACERT17F PIC  X(0001).
           05  FILLER REDEFINES ACERT17F.
               10  ACERT17A PIC  X(0001).
           05  ACERT17I PIC  X(0011).
      *    -------------------------------
           05  ATYPE17L PIC S9(0004) COMP.
           05  ATYPE17F PIC  X(0001).
           05  FILLER REDEFINES ATYPE17F.
               10  ATYPE17A PIC  X(0001).
           05  ATYPE17I PIC  X(0004).
      *    -------------------------------
           05  ABY17L PIC S9(0004) COMP.
           05  ABY17F PIC  X(0001).
           05  FILLER REDEFINES ABY17F.
               10  ABY17A PIC  X(0001).
           05  ABY17I PIC  X(0004).
      *    -------------------------------
           05  AEDT17L PIC S9(0004) COMP.
           05  AEDT17F PIC  X(0001).
           05  FILLER REDEFINES AEDT17F.
               10  AEDT17A PIC  X(0001).
           05  AEDT17I PIC  X(0008).
      *    -------------------------------
           05  AIDT17L PIC S9(0004) COMP.
           05  AIDT17F PIC  X(0001).
           05  FILLER REDEFINES AIDT17F.
               10  AIDT17A PIC  X(0001).
           05  AIDT17I PIC  X(0008).
      *    -------------------------------
           05  AFILE17L PIC S9(0004) COMP.
           05  AFILE17F PIC  X(0001).
           05  FILLER REDEFINES AFILE17F.
               10  AFILE17A PIC  X(0001).
           05  AFILE17I PIC  X(0004).
      *    -------------------------------
           05  ACLAM18L PIC S9(0004) COMP.
           05  ACLAM18F PIC  X(0001).
           05  FILLER REDEFINES ACLAM18F.
               10  ACLAM18A PIC  X(0001).
           05  ACLAM18I PIC  X(0007).
      *    -------------------------------
           05  ACARR18L PIC S9(0004) COMP.
           05  ACARR18F PIC  X(0001).
           05  FILLER REDEFINES ACARR18F.
               10  ACARR18A PIC  X(0001).
           05  ACARR18I PIC  X(0001).
      *    -------------------------------
           05  ACERT18L PIC S9(0004) COMP.
           05  ACERT18F PIC  X(0001).
           05  FILLER REDEFINES ACERT18F.
               10  ACERT18A PIC  X(0001).
           05  ACERT18I PIC  X(0011).
      *    -------------------------------
           05  ATYPE18L PIC S9(0004) COMP.
           05  ATYPE18F PIC  X(0001).
           05  FILLER REDEFINES ATYPE18F.
               10  ATYPE18A PIC  X(0001).
           05  ATYPE18I PIC  X(0004).
      *    -------------------------------
           05  ABY18L PIC S9(0004) COMP.
           05  ABY18F PIC  X(0001).
           05  FILLER REDEFINES ABY18F.
               10  ABY18A PIC  X(0001).
           05  ABY18I PIC  X(0004).
      *    -------------------------------
           05  AEDT18L PIC S9(0004) COMP.
           05  AEDT18F PIC  X(0001).
           05  FILLER REDEFINES AEDT18F.
               10  AEDT18A PIC  X(0001).
           05  AEDT18I PIC  X(0008).
      *    -------------------------------
           05  AIDT18L PIC S9(0004) COMP.
           05  AIDT18F PIC  X(0001).
           05  FILLER REDEFINES AIDT18F.
               10  AIDT18A PIC  X(0001).
           05  AIDT18I PIC  X(0008).
      *    -------------------------------
           05  AFILE18L PIC S9(0004) COMP.
           05  AFILE18F PIC  X(0001).
           05  FILLER REDEFINES AFILE18F.
               10  AFILE18A PIC  X(0001).
           05  AFILE18I PIC  X(0004).
      *    -------------------------------
           05  AEMSG1L PIC S9(0004) COMP.
           05  AEMSG1F PIC  X(0001).
           05  FILLER REDEFINES AEMSG1F.
               10  AEMSG1A PIC  X(0001).
           05  AEMSG1I PIC  X(0079).
      *    -------------------------------
           05  APFKL PIC S9(0004) COMP.
           05  APFKF PIC  X(0001).
           05  FILLER REDEFINES APFKF.
               10  APFKA PIC  X(0001).
           05  APFKI PIC  9(2).
       01  EL173AO REDEFINES EL173AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAGEO PIC  9999.
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
           05  ATYPE01O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY01O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEDT01O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT01O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFILE01O PIC  X(0004).
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
           05  ATYPE02O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY02O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEDT02O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT02O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFILE02O PIC  X(0004).
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
           05  ATYPE03O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY03O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEDT03O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT03O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFILE03O PIC  X(0004).
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
           05  ATYPE04O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY04O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEDT04O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT04O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFILE04O PIC  X(0004).
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
           05  ATYPE05O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY05O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEDT05O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT05O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFILE05O PIC  X(0004).
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
           05  ATYPE06O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY06O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEDT06O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT06O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFILE06O PIC  X(0004).
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
           05  ATYPE07O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY07O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEDT07O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT07O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFILE07O PIC  X(0004).
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
           05  ATYPE08O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY08O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEDT08O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT08O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFILE08O PIC  X(0004).
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
           05  ATYPE09O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY09O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEDT09O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT09O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFILE09O PIC  X(0004).
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
           05  ATYPE10O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY10O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEDT10O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT10O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFILE10O PIC  X(0004).
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
           05  ATYPE11O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY11O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEDT11O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT11O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFILE11O PIC  X(0004).
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
           05  ATYPE12O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY12O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEDT12O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT12O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFILE12O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACALM13O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT13O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE13O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY13O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEDT13O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT13O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFILE13O PIC  X(0004).
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
           05  ATYPE14O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY14O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEDT14O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT14O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFILE14O PIC  X(0004).
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
           05  ATYPE15O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY15O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEDT15O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT15O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFILE15O PIC  X(0004).
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
           05  ATYPE16O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY16O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEDT16O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT16O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFILE16O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM17O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR17O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT17O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE17O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY17O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEDT17O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT17O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFILE17O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM18O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR18O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT18O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE18O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABY18O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEDT18O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT18O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFILE18O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFKO PIC  99.
      *    -------------------------------
00141
00142  01  FILLER                          REDEFINES
00143      EL173AI.
00144
00145      05  FILLER                      PIC X(38).
00146
00147      05  FILLER                      OCCURS 18 TIMES
00148                                      INDEXED BY EL173A-INDEX.
00149
00150          15  EL173A-CLAIM-LENGTH     PIC S9(4)
00151                                      COMP.
00152          15  EL173A-CLAIM-ATTRB      PIC X.
00153          15  EL173A-CLAIM            PIC X(7).
00154
00155          15  EL173A-CARRIER-LENGTH   PIC S9(4)
00156                                      COMP.
00157          15  EL173A-CARRIER-ATTRB    PIC X.
00158          15  EL173A-CARRIER          PIC X.
00159
00160          15  EL173A-CERT-NO-LENGTH   PIC S9(4)
00161                                      COMP.
00162          15  EL173A-CERT-NO-ATTRB    PIC X.
00163          15  EL173A-CERT-NO          PIC X(11).
00164
00165          15  EL173A-TYPE-LENGTH      PIC S9(4)
00166                                      COMP.
00167          15  EL173A-TYPE-ATTRB       PIC X.
00168          15  EL173A-TYPE             PIC X(4).
00169
00170          15  EL173A-BY-LENGTH        PIC S9(4)
00171                                      COMP.
00172          15  EL173A-BY-ATTRB         PIC X.
00173          15  EL173A-BY               PIC X(4).
00174
00175          15  EL173A-EDATE-LENGTH     PIC S9(4)
00176                                      COMP.
00177          15  EL173A-EDATE-ATTRB      PIC X.
00178          15  EL173A-EDATE            PIC X(8).
00179
00180          15  EL173A-IDATE-LENGTH     PIC S9(4)
00181                                      COMP.
00182          15  EL173A-IDATE-ATTRB      PIC X.
00183          15  EL173A-IDATE            PIC X(8).
00184
00185          15  EL173A-FILE-LENGTH      PIC S9(4)
00186                                      COMP.
00187          15  EL173A-FILE-ATTRB       PIC X.
00188          15  EL173A-FILE             PIC X(4).
00189
00190      EJECT
00191 *                                    COPY ELCEMIB.
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
00192      EJECT
00193 *                                    COPY ELCDATE.
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
00194      EJECT
00195 *                                    COPY ELCLOGOF.
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
00196      EJECT
00197 *                                    COPY ELCATTR.
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
00198      EJECT
00199 *                                    COPY ELCAID.
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
00200
00201  01  FILLER                      REDEFINES
00202      DFHAID.
00203
00204      05  FILLER                      PIC X(8).
00205
00206      05  PF-VALUES                   PIC X
00207          OCCURS 24 TIMES.
00208      EJECT
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
00210
00211  01  DFHCOMMAREA                     PIC X(1024).
00212
00213 *                                    COPY ELCMSTR.
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
00214      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CLAIM-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL173' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00216
00217      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00218      MOVE '5'                   TO DC-OPTION-CODE.
00219      PERFORM 8500-DATE-CONVERSION.
00220      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00221      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00222
00223      MOVE DFHCOMMAREA           TO  PROGRAM-INTERFACE-BLOCK.
00224
00225 *    NOTE *******************************************************
00226 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
00227 *         *  FROM ANOTHER MODULE.                               *
00228 *         *******************************************************.
00229
00230      IF EIBCALEN NOT GREATER THAN ZERO
00231          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00232          GO TO 8300-SEND-TEXT.
00233
00234      
      * EXEC CICS HANDLE CONDITION
00235 *        PGMIDERR (9600-PGMIDERR)
00236 *        ENDFILE  (4800-END-OF-FILE)
00237 *        ERROR    (9990-ERROR) END-EXEC.
      *    MOVE '"$L''.                 ! " #00002641' TO DFHEIV0
           MOVE X'22244C272E20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303032363431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00238
00239      EJECT
00240  0010-MAIN-LOGIC.
00241      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00242          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00243              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00244              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00245              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00246              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00247              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00248              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00249              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00250              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
00251            ELSE
00252              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00253              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00254              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00255              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00256              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00257              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00258              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00259              MOVE SPACES               TO  PI-SAVED-PROGRAM-6
00260        ELSE
00261          GO TO 0020-MAIN-LOGIC.
00262
00263  0015-MAIN-LOGIC.
00264 *    NOTE *******************************************************
00265 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      *
00266 *         *  INTERFACE BLOCK FOR THIS MODULE.                   *
00267 *         *******************************************************.
00268
00269      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.
00270
00271      MOVE LOW-VALUES             TO  PI-CLAIM-KEY
00272                                      PI-PREV-CLAIM-KEY.
00273      MOVE PI-COMPANY-CD          TO  PI-CK-COMPANY-CODE.
00274
00275      MOVE ZERO                   TO  PI-END-OF-FILE
00276                                      PI-TEMP-STORAGE-ITEM
00277                                      PI-LAST-ITEM-NO.
00278
00279      IF EIBAID = DFHCLEAR
00280          GO TO 9400-CLEAR.
00281
00282      IF PI-PROCESSOR-ID = 'LGXX'
00283          NEXT SENTENCE
00284      ELSE
00285          
      * EXEC CICS READQ TS
00286 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00287 *            INTO    (SECURITY-CONTROL)
00288 *            LENGTH  (SC-COMM-LENGTH)
00289 *            ITEM    (SC-ITEM)
00290 *        END-EXEC
      *    MOVE '*$II   L              ''   #00002692' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00291          MOVE SC-CLAIMS-DISPLAY (17)  TO  PI-DISPLAY-CAP
00292          MOVE SC-CLAIMS-UPDATE  (17)  TO  PI-MODIFY-CAP
00293          IF NOT DISPLAY-CAP
00294              MOVE 'READ'              TO  SM-READ
00295              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00296              MOVE ER-0070             TO  EMI-ERROR
00297              GO TO 8100-SEND-INITIAL-MAP.
00298
00299      GO TO 4000-BROWSE-CLAIM-MASTER.
00300
00301      EJECT
00302  0020-MAIN-LOGIC.
00303 *    NOTE *******************************************************
00304 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *
00305 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *
00306 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
00307 *         *******************************************************.
00308
00309      IF EIBAID = DFHCLEAR
00310          GO TO 9400-CLEAR.
00311
00312      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00313          MOVE LOW-VALUES         TO  EL173AI
00314          MOVE ER-0008            TO  EMI-ERROR
00315          MOVE -1                 TO  APFKL
00316          GO TO 8200-SEND-DATAONLY.
00317
00318      
      * EXEC CICS RECEIVE
00319 *        INTO   (EL173AI)
00320 *        MAPSET (WS-MAPSET-NAME)
00321 *        MAP    (WS-MAP-NAME)
00322 *    END-EXEC.
           MOVE LENGTH OF
            EL173AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00002725' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL173AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00323
00324      IF APFKL GREATER ZERO
00325          IF EIBAID NOT = DFHENTER
00326              MOVE ER-0004           TO  EMI-ERROR
00327              MOVE AL-UNBOF       TO  APFKA
00328              MOVE -1             TO  APFKL
00329              GO TO 8200-SEND-DATAONLY
00330            ELSE
00331              IF APFKO IS NUMERIC
00332                AND APFKO IS GREATER THAN ZERO
00333                AND APFKO IS LESS THAN '25'
00334                  MOVE PF-VALUES (APFKI)  TO  EIBAID
00335                ELSE
00336                  MOVE ER-0029           TO  EMI-ERROR
00337                  MOVE AL-UNBOF       TO  APFKA
00338                  MOVE -1             TO  APFKL
00339                  GO TO 8200-SEND-DATAONLY.
00340
00341      IF EIBAID = DFHPF12
00342          MOVE 'EL010   '         TO  THIS-PGM
00343          GO TO 9300-XCTL.
00344
00345      IF EIBAID = DFHPF23
00346          GO TO 9000-RETURN-CICS.
00347
00348      IF EIBAID = DFHPF24
00349          MOVE 'EL126   '         TO  THIS-PGM
00350          GO TO 9300-XCTL.
00351
00352      IF EIBAID = DFHENTER OR DFHPF1 OR DFHPF2
00353          NEXT SENTENCE
00354        ELSE
00355          MOVE ER-0008               TO  EMI-ERROR
00356          MOVE -1                 TO  APFKL
00357          GO TO 8200-SEND-DATAONLY.
00358
00359      EJECT
00360
00361      IF EIBAID = DFHPF1 OR DFHPF2
00362          GO TO 0110-MAIN-LOGIC.
00363
00364      IF PI-END-OF-FILE NOT = ZERO
00365          PERFORM 9400-CLEAR.
00366
00367      GO TO 4000-BROWSE-CLAIM-MASTER.
00368
00369  0110-MAIN-LOGIC.
00370      MOVE APAGEI                 TO  WS-TEMP-STORAGE-ITEM.
00371
00372      IF EIBAID = DFHPF1
00373        IF (PI-LAST-ITEM-NO GREATER THAN ZEROS) AND
00374           (WS-TEMP-STORAGE-ITEM GREATER THAN PI-LAST-ITEM-NO)
00375          MOVE ER-0313                TO  EMI-ERROR
00376          MOVE -1                     TO  APFKL
00377          GO TO 8200-SEND-DATAONLY
00378        ELSE
00379          ADD +1  TO  WS-TEMP-STORAGE-ITEM
00380          GO TO 0120-MAIN-LOGIC.
00381
00382      IF EIBAID = DFHPF2
00383        IF WS-TEMP-STORAGE-ITEM GREATER THAN +1
00384          SUBTRACT +1 FROM WS-TEMP-STORAGE-ITEM
00385          GO TO 0120-MAIN-LOGIC
00386        ELSE
00387          MOVE ER-0047                TO  EMI-ERROR.
00388          MOVE -1                     TO  APFKL
00389          GO TO 8200-SEND-DATAONLY.
00390
00391
00392  0120-MAIN-LOGIC.
00393      MOVE EIBTRMID               TO  WS-TS-TERM-ID.
00394
00395      
      * EXEC CICS HANDLE CONDITION
00396 *        ITEMERR (0130-BROWSE-CLAIM-MASTER)
00397 *    END-EXEC.
      *    MOVE '"$<                   ! # #00002802' TO DFHEIV0
           MOVE X'22243C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303032383032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00398
00399      
      * EXEC CICS READQ TS
00400 *        QUEUE  (WS-TEMP-STORAGE-KEY)
00401 *        ITEM   (WS-TEMP-STORAGE-ITEM)
00402 *        INTO   (EL173AI)
00403 *        LENGTH (WS-TS-LENGTH) END-EXEC
      *    MOVE '*$II   L              ''   #00002806' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 EL173AI, 
                 WS-TS-LENGTH, 
                 WS-TEMP-STORAGE-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00404
00405      MOVE WS-TEMP-STORAGE-ITEM  TO  APAGEO.
00406
00407      GO TO 8100-SEND-INITIAL-MAP.
00408
00409  0130-BROWSE-CLAIM-MASTER.
00410
00411      GO TO 4000-BROWSE-CLAIM-MASTER.
00412
00413      EJECT
00414  4000-BROWSE-CLAIM-MASTER SECTION.
00415      MOVE LOW-VALUES             TO  EL173AI.
00416
00417      
      * EXEC CICS STARTBR
00418 *        DATASET (WS-CLAIM-MASTER-DSID)
00419 *        RIDFLD  (PI-CLAIM-KEY)
00420 *        GTEQ    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00002824' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 PI-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00421
00422      SET EL173A-INDEX TO +1.
00423
00424  4100-READNEXT.
00425      MOVE PI-CLAIM-KEY           TO  PI-PREV-CLAIM-KEY.
00426
00427      
      * EXEC CICS READNEXT
00428 *        DATASET (WS-CLAIM-MASTER-DSID)
00429 *        RIDFLD  (PI-CLAIM-KEY)
00430 *        SET     (ADDRESS OF CLAIM-MASTER) END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00002834' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00431
00432      IF PI-CK-COMPANY-CODE NOT = PI-COMPANY-CD
00433          GO TO 4800-END-OF-FILE.
00434
00435      IF WS-READNEXT-SW GREATER THAN ZERO
00436          GO TO 4900-ENDBROWSE.
00437
00438      IF CL-SUPV-ATTN-CD NOT = 'Y'
00439          GO TO 4100-READNEXT.
00440
00441      IF NOT PI-NO-CARRIER-SECURITY
00442          IF PI-CK-CARRIER NOT = PI-CARRIER-SECURITY
00443             GO TO 4100-READNEXT.
00444
00445      MOVE CL-CLAIM-NO    TO  EL173A-CLAIM   (EL173A-INDEX).
00446      MOVE CL-CARRIER     TO  EL173A-CARRIER (EL173A-INDEX).
00447      MOVE CL-CERT-NO     TO  EL173A-CERT-NO (EL173A-INDEX).
00448      MOVE CL-CLAIM-TYPE  TO  EL173A-TYPE    (EL173A-INDEX).
00449      MOVE CL-PROCESSOR-ID  TO  EL173A-BY    (EL173A-INDEX).
00450
00451      IF CL-FILE-ESTABLISH-DT NOT = LOW-VALUES
00452          MOVE CL-FILE-ESTABLISH-DT  TO  DC-BIN-DATE-1
00453          MOVE SPACES             TO  DC-OPTION-CODE
00454          PERFORM 8500-DATE-CONVERSION
00455          MOVE DC-GREG-DATE-1-EDIT TO EL173A-EDATE (EL173A-INDEX).
00456
00457      IF CL-INCURRED-DT NOT = LOW-VALUES
00458          MOVE CL-INCURRED-DT     TO  DC-BIN-DATE-1
00459          MOVE SPACES             TO  DC-OPTION-CODE
00460          PERFORM 8500-DATE-CONVERSION
00461          MOVE DC-GREG-DATE-1-EDIT TO EL173A-IDATE (EL173A-INDEX).
00462
00463      MOVE CL-FILE-LOCATION  TO  EL173A-FILE (EL173A-INDEX).
00464
00465      IF EL173A-INDEX LESS THAN +18
00466          SET EL173A-INDEX UP BY +1
00467          GO TO 4100-READNEXT.
00468
00469      MOVE +1                     TO  WS-READNEXT-SW.
00470      GO TO 4100-READNEXT.
00471
00472  4800-END-OF-FILE.
00473      MOVE +1                     TO  PI-END-OF-FILE.
00474      MOVE PI-TEMP-STORAGE-ITEM   TO  PI-LAST-ITEM-NO.
00475
00476      MOVE ER-0313                TO  EMI-ERROR.
00477
00478  4900-ENDBROWSE.
00479      
      * EXEC CICS ENDBR
00480 *        DATASET (WS-CLAIM-MASTER-DSID) END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002886' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00481
00482      MOVE -1                     TO  APFKL.
00483
00484      MOVE EIBTRMID               TO  WS-TS-TERM-ID.
00485
00486      
      * EXEC CICS WRITEQ TS
00487 *        QUEUE  (WS-TEMP-STORAGE-KEY)
00488 *        ITEM   (PI-TEMP-STORAGE-ITEM)
00489 *        FROM   (EL173AI)
00490 *        LENGTH (WS-TS-LENGTH) END-EXEC
      *    MOVE '*" I                  ''   #00002893' TO DFHEIV0
           MOVE X'2A2220492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 EL173AI, 
                 WS-TS-LENGTH, 
                 PI-TEMP-STORAGE-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00491
00492      MOVE PI-TEMP-STORAGE-ITEM  TO  APAGEO.
00493
00494      GO TO 8100-SEND-INITIAL-MAP.
00495
00496      EJECT
00497  8100-SEND-INITIAL-MAP SECTION.
00498      IF EMI-ERROR NOT = ZERO
00499          PERFORM 9900-ERROR-FORMAT.
00500
00501      MOVE -1                     TO  APFKL.
00502      MOVE EIBTIME                TO  TIME-IN.
00503      MOVE SAVE-DATE              TO  ADATEO.
00504      MOVE TIME-OUT               TO  ATIMEO.
00505      MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.
00506
00507      
      * EXEC CICS SEND
00508 *        FROM   (EL173AI)
00509 *        MAPSET (WS-MAPSET-NAME)
00510 *        MAP    (WS-MAP-NAME)
00511 *        CURSOR ERASE END-EXEC.
           MOVE LENGTH OF
            EL173AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00002914' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL173AI, 
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
           
00512
00513      GO TO 9100-RETURN-TRAN.
00514
00515      EJECT
00516  8200-SEND-DATAONLY SECTION.
00517      IF EMI-ERROR NOT = ZERO
00518          PERFORM 9900-ERROR-FORMAT.
00519
00520      MOVE EIBTIME                TO  TIME-IN.
00521
00522          MOVE SAVE-DATE          TO  ADATEO
00523          MOVE TIME-OUT           TO  ATIMEO
00524          MOVE EMI-MESSAGE-AREA (1) TO AEMSG1O
00525
00526      
      * EXEC CICS SEND DATAONLY
00527 *        FROM   (EL173AI)
00528 *        MAPSET (WS-MAPSET-NAME)
00529 *        MAP    (WS-MAP-NAME)
00530 *        CURSOR END-EXEC.
           MOVE LENGTH OF
            EL173AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00002933' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL173AI, 
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
           
00531
00532      GO TO 9100-RETURN-TRAN.
00533
00534      EJECT
00535  8300-SEND-TEXT SECTION.
00536      
      * EXEC CICS SEND TEXT
00537 *        FROM   (LOGOFF-TEXT)
00538 *        LENGTH (LOGOFF-LENGTH)
00539 *        ERASE  FREEKB END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00002943' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393433' TO DFHEIV0(25:11)
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
           
00540
00541      
      * EXEC CICS RETURN
00542 *        END-EXEC.
      *    MOVE '.(                    &   #00002948' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00543
00544  8300-EXIT.
00545      EXIT.
00546
00547      EJECT
00548  8500-DATE-CONVERSION SECTION.
00549      
      * EXEC CICS LINK
00550 *        PROGRAM  ('ELDATCV')
00551 *        COMMAREA (DATE-CONVERSION-DATA)
00552 *        LENGTH   (DC-COMM-LENGTH) END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00002956' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00553
00554  8500-EXIT.
00555      EXIT.
00556
00557      EJECT
00558  9000-RETURN-CICS SECTION.
00559      MOVE 'EL005'                TO  THIS-PGM.
00560      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
00561      PERFORM 9300-XCTL.
00562
00563  9000-EXIT.
00564      EXIT.
00565
00566  9100-RETURN-TRAN SECTION.
00567      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
00568      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
00569
00570      
      * EXEC CICS RETURN
00571 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
00572 *        LENGTH   (PI-COMM-LENGTH)
00573 *        TRANSID  (WS-TRANS-ID) END-EXEC.
      *    MOVE '.(CT                  &   #00002977' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00574
00575  9100-EXIT.
00576      EXIT.
00577
00578  9300-XCTL SECTION.
00579      
      * EXEC CICS HANDLE CONDITION
00580 *        QIDERR (9300-NEXT-SENTENCE) END-EXEC.
      *    MOVE '"$N                   ! $ #00002986' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303032393836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00581
00582      MOVE EIBTRMID               TO  WS-TS-TERM-ID.
00583
00584      
      * EXEC CICS DELETEQ TS
00585 *        QUEUE (WS-TEMP-STORAGE-KEY) END-EXEC.
      *    MOVE '*&                    #   #00002991' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00586
00587  9300-NEXT-SENTENCE.
00588      MOVE DFHENTER               TO  EIBAID.
00589
00590      
      * EXEC CICS XCTL
00591 *        PROGRAM  (THIS-PGM)
00592 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
00593 *        LENGTH   (PI-COMM-LENGTH) END-EXEC.
      *    MOVE '.$C                   $   #00002997' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00594
00595  9300-EXIT.
00596      EXIT.
00597
00598      EJECT
00599  9400-CLEAR SECTION.
00600      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.
00601      PERFORM 9300-XCTL.
00602
00603  9400-EXIT.
00604      EXIT.
00605
00606  9600-PGMIDERR SECTION.
00607      
      * EXEC CICS HANDLE CONDITION
00608 *        PGMIDERR (8300-SEND-TEXT) END-EXEC.
      *    MOVE '"$L                   ! % #00003014' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303033303134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00609
00610      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM
00611                                      LOGOFF-PGM.
00612
00613      MOVE 'EL005'                TO  THIS-PGM.
00614      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
00615      MOVE SPACES                 TO  PI-ENTRY-CD-1.
00616      PERFORM 9300-XCTL.
00617
00618  9600-EXIT.
00619      EXIT.
00620
00621      EJECT
00622  9900-ERROR-FORMAT SECTION.
00623      
      * EXEC CICS LINK
00624 *        PROGRAM  ('EL001')
00625 *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
00626 *        LENGTH   (EMI-COMM-LENGTH) END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   ''   #00003030' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00627
00628  9900-EXIT.
00629      EXIT.
00630
00631      EJECT
00632  9990-ERROR SECTION.
00633      MOVE DFHEIBLK TO EMI-LINE1.
00634      
      * EXEC CICS LINK
00635 *        PROGRAM  ('EL004')
00636 *        COMMAREA (EMI-LINE1)
00637 *        LENGTH   (72) END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00003041' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00638
00639      GO TO 8200-SEND-DATAONLY.
00640
00641  9990-EXIT.
00642      EXIT.
00643
00644  9995-SECURITY-VIOLATION.
00645 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00003069' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303639' TO DFHEIV0(25:11)
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
00646
00647  9995-EXIT.
00648      EXIT.
00649

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL173' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMIDERR,
                     4800-END-OF-FILE,
                     9990-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0130-BROWSE-CLAIM-MASTER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 9300-NEXT-SENTENCE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL173' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
