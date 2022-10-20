00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL6451.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 08:01:37.
00007 *                            VMOD=2.007.
00008 *
00008 *
00009 *AUTHOR.        LOGIC INC.
00010 *               DALLAS, TEXAS.
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
00025 *        THIS PROGRAM PROVIDES THE BROWSE NECESSARY FOR THE LOSS
00026 *        RATIOS LOOK-UP.
00027
00028 *    SCREENS     - EL645B - LOSS RATIOS AS OF (MM/DD/YY)
00029 *                  EL645C - LOSS RATIOS AS OF (MM/DD/YY)
00030
00031 *    ENTERED BY  - EL645 - LOSS RATIOS SELECTION MENU
00032
00033 *    EXIT TO     - EL645 - LOSS RATIOS SELECTION MENU
00034
00035 *    INPUT FILE  - ERLOSS - LOSS RATIOS
00036
00037 *    OUTPUT FILE - NONE
00038
00039 *    COMMAREA    - PASSED.  THE CONTROL INFORMATION KEYED IN THE
00040 *                  CALLING PROGRAM (EL645) IS PLACED IN THE
00041 *                  APPROPRIATE FIELDS OF THE COMMAAREA FOR
00042 *                  REFERENCE BY THIS PROGRAM.
00043
00044 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL645.  ON
00045 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE
00046 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
00047 *                  ENTRIES (XCTL FROM CICS VIA EXR2) THE SCREEN
00048 *                  WILL BE READ AND ACTION WILL BE BASED ON THE
00049 *                  MAINTENANCE TYPE INDICATED.
00050  EJECT
00051  ENVIRONMENT DIVISION.
00052  DATA DIVISION.
00053  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00054  77  FILLER  PIC  X(32) VALUE '********************************'.
00055  77  FILLER  PIC  X(32) VALUE '*   EL6451 WORKING STORAGE     *'.
00056  77  FILLER  PIC  X(32) VALUE '*********** VMOD=2.007 *********'.
00057
00058 *                            COPY ELCSCTM.
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
00059
00060 *                            COPY ELCSCRTY.
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
00061
00062  01  WS-DATE-AREA.
00063      12  SAVE-DATE               PIC  X(08)      VALUE SPACES.
00064      12  SAVE-BIN-DATE           PIC  X(02)      VALUE SPACES.
00065
00066  01  FILLER              COMP-3.
00067      12  TIME-IN                 PIC S9(07)      VALUE ZERO.
00068      12  TIME-OUT  REDEFINES
00069          TIME-IN                 PIC S9(03)V9(4).
00070
00071  01  FILLER.
00072      12  WS-MAPSET-NAME              PIC  X(08)  VALUE 'EL6451S'.
00073      12  WS-MAP-B-NAME               PIC  X(08)  VALUE 'EL645B'.
00074      12  WS-MAP-C-NAME               PIC  X(08)  VALUE 'EL645C'.
00075      12  THIS-PGM                    PIC  X(08)  VALUE 'EL6451'.
00076      12  WS-TRANS-ID                 PIC  X(04)  VALUE 'EXR2'.
00077      12  WS-LR-RUN-DATE.
00078          16  WS-LR-RUN-YR            PIC  X(02).
00079          16  WS-LR-RUN-MO            PIC  X(02).
00080          16  WS-LR-RUN-DA            PIC  X(02).
00081      12  WS-LR-RUN-DATE-EDIT.
00082          16  WS-LR-RUN-MO-EDIT       PIC  X(02).
00083          16  FILLER                  PIC  X(01)  VALUE '/'.
00084          16  WS-LR-RUN-DA-EDIT       PIC  X(02).
00085          16  FILLER                  PIC  X(01)  VALUE '/'.
00086          16  WS-LR-RUN-YR-EDIT       PIC  X(02).
00087      12  WS-CALC-RDNXT               PIC S9(08) COMP VALUE ZERO.
00088      12  WORK-COMM               PIC ZZ.ZZ.
00089      12  WORK-TABLE.
00090          16  FILLER              PIC X           VALUE SPACES.
00091          16  WK-COM-TBL          PIC XXX         VALUE SPACES.
00092          16  FILLER              PIC X           VALUE SPACES.
00093      12  ERROR-MESSAGES.
00094          16  ER-0002                 PIC  X(04)  VALUE '0002'.
00095          16  ER-0004                 PIC  X(04)  VALUE '0004'.
00096          16  ER-0008                 PIC  X(04)  VALUE '0008'.
00097          16  ER-0029                 PIC  X(04)  VALUE '0029'.
00098          16  ER-0130                 PIC  X(04)  VALUE '0130'.
00099          16  ER-0200                 PIC  X(04)  VALUE '0200'.
00100          16  ER-0673                 PIC  X(04)  VALUE '0673'.
00101          16  ER-0685                 PIC  X(04)  VALUE '0685'.
00102  EJECT
00103 *                                COPY ELCINTF.
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
00104
00105 *                                COPY ELC645PI.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELC645PI                            *
00004 *                            VMOD=2.003                          *
00005 *                                                                *
00006 ******************************************************************
00007
00008      12  PI-WORK-AREA  REDEFINES  PI-PROGRAM-WORK-AREA.
00009          16  FILLER                      PIC  X(12).
00010          16  PI-1ST-TIME-SW              PIC S9(01)  COMP-3.
00011          16  PI-LINE-COUNT               PIC S9(03)  COMP-3.
00012          16  PI-AIX-RECORD-COUNT         PIC S9(04)  COMP SYNC.
00013          16  PI-START-SW                 PIC S9(01)  COMP-3.
00014          16  PI-BROWSE-SW                PIC S9(01)  COMP-3.
00015          16  PI-END-OF-FILE              PIC S9(01)  COMP-3.
00016          16  PI-DSID                     PIC  X(08).
00017          16  PI-OPTION                   PIC  X(01).
00018              88  OPTION-ONE-SELECTED             VALUE '1'.
00019              88  OPTION-TWO-SELECTED             VALUE '2'.
00020              88  OPTION-THREE-SELECTED           VALUE '3'.
00021              88  OPTION-FOUR-SELECTED            VALUE '4'.
00022              88  OPTION-FIVE-SELECTED            VALUE '5'.
00023              88  OPTION-SIX-SELECTED             VALUE '6'.
00024          16  PI-READ-SWITCH              PIC  X(01).
00025              88  PI-READ-TOTAL                   VALUE 'B'.
00026              88  PI-READ-DETAIL                  VALUE 'C'.
00027          16  PI-RETURN-SWITCH            PIC  X(01).
00028              88  PI-REENTERED                    VALUE 'Y'.
00029          16  PI-SELECTION-CRITERIA.
00030              20  PI-SC-COMPANY-CD        PIC  X(01).
00031              20  PI-SC-RCD-TYPE          PIC  X(01).
00032              20  PI-SC-REIN-CO           PIC  X(03).
00033              20  PI-SC-RPT-CD-1          PIC  X(10).
00034              20  PI-SC-CARRIER           PIC  X(01).
00035              20  PI-SC-GROUPING          PIC  X(06).
00036              20  PI-SC-GA-RPT-CD-2       PIC  X(10).
00037              20  PI-SC-STATE             PIC  X(02).
00038              20  PI-SC-ACCOUNT           PIC  X(10).
00039              20  PI-SC-REIN-SUB          PIC  X(03).
00040          16  PI-LOSS-RATIO-KEY.
00041              20  PI-LRK-COMPANY-CD       PIC  X(01).
00042              20  PI-LRK-RCD-TYPE         PIC  X(01).
00043              20  PI-LRK-REIN-CO          PIC  X(03).
00044              20  PI-LRK-RPT-CD-1         PIC  X(10).
00045              20  PI-LRK-CARRIER          PIC  X(01).
00046              20  PI-LRK-GROUPING         PIC  X(06).
00047              20  PI-LRK-GA-RPT-CD-2      PIC  X(10).
00048              20  PI-LRK-STATE            PIC  X(02).
00049              20  PI-LRK-ACCOUNT          PIC  X(10).
00050              20  PI-LRK-REIN-SUB         PIC  X(03).
00051          16  PI-PREV-LOSS-RATIO-KEY.
00052              20  PI-PREV-LRK-COMPANY-CD  PIC  X(01).
00053              20  PI-PREV-LRK-RCD-TYPE    PIC  X(01).
00054              20  PI-PREV-LRK-REIN-CO     PIC  X(03).
00055              20  PI-PREV-LRK-RPT-CD-1    PIC  X(10).
00056              20  PI-PREV-LRK-CARRIER     PIC  X(01).
00057              20  PI-PREV-LRK-GROUPING    PIC  X(06).
00058              20  PI-PREV-LRK-GA-RPT-CD-2
00059                                          PIC  X(10).
00060              20  PI-PREV-LRK-STATE       PIC  X(02).
00061              20  PI-PREV-LRK-ACCOUNT     PIC  X(10).
00062              20  PI-PREV-LRK-REIN-SUB    PIC  X(03).
00063          16  PI-KEY-LENGTH               PIC S9(04)  COMP SYNC.
00064          16  PI-TS-ITEM                  PIC S9(04)  COMP SYNC.
00065          16  PI-1ST-KEY                  PIC  X(47).
00066          16  PI-LAST-KEY                 PIC  X(47).
00067          16  PI-PREV-AID                 PIC  X(01).
00068          16  PI-START-LOSS-RATIO-KEY.
00069              20  PI-START-LRK-COMPANY-CD
00070                                          PIC  X(01).
00071              20  PI-START-LRK-RCD-TYPE   PIC  X(01).
00072              20  PI-START-LRK-REIN-CO    PIC  X(03).
00073              20  PI-START-LRK-RPT-CD-1   PIC  X(10).
00074              20  PI-START-LRK-CARRIER    PIC  X(01).
00075              20  PI-START-LRK-GROUPING   PIC  X(06).
00076              20  PI-START-LRK-GA-RPT-CD-2
00077                                          PIC  X(10).
00078              20  PI-START-LRK-STATE      PIC  X(02).
00079              20  PI-START-LRK-ACCOUNT    PIC  X(10).
00080              20  PI-START-LRK-REIN-SUB   PIC  X(03).
00081          16  PI-END-LOSS-RATIO-KEY.
00082              20  PI-END-LRK-COMPANY-CD   PIC  X(01).
00083              20  PI-END-LRK-RCD-TYPE     PIC  X(01).
00084              20  PI-END-LRK-REIN-CO      PIC  X(03).
00085              20  PI-END-LRK-RPT-CD-1     PIC  X(10).
00086              20  PI-END-LRK-CARRIER      PIC  X(01).
00087              20  PI-END-LRK-GROUPING     PIC  X(06).
00088              20  PI-END-LRK-GA-RPT-CD-2  PIC  X(10).
00089              20  PI-END-LRK-STATE        PIC  X(02).
00090              20  PI-END-LRK-ACCOUNT      PIC  X(10).
00091              20  PI-END-LRK-REIN-SUB     PIC  X(03).
00092          16  PI-SAVE-LOSS-RATIO-KEY.
00093              20  PI-SAVE-LRK-COMPANY-CD  PIC  X(01).
00094              20  PI-SAVE-LRK-RCD-TYPE    PIC  X(01).
00095              20  PI-SAVE-LRK-REIN-CO     PIC  X(03).
00096              20  PI-SAVE-LRK-RPT-CD-1    PIC  X(10).
00097              20  PI-SAVE-LRK-CARRIER     PIC  X(01).
00098              20  PI-SAVE-LRK-GROUPING    PIC  X(06).
00099              20  PI-SAVE-LRK-GA-RPT-CD-2
00100                                          PIC  X(10).
00101              20  PI-SAVE-LRK-STATE       PIC  X(02).
00102              20  PI-SAVE-LRK-ACCOUNT     PIC  X(10).
00103              20  PI-SAVE-LRK-REIN-SUB    PIC  X(03).
00104          16  PI-LIN1-LOSS-RATIO-KEY      PIC  X(47).
00105          16  PI-SCREEN-COUNT             PIC S9(08)  COMP.
00106          16  PI-SUB                      PIC S9(02).
00107          16  PI-FIRST-TIME-SW            PIC  X(01).
00108          16  PI-MAPNAME                  PIC  X(08).
00109              88  PI-MAP-B                    VALUE 'EL654B  '.
00110              88  PI-MAP-C                    VALUE 'EL654C  '.
00111          16  PI-ACTIVE-ONLY              PIC  X(01).
00112              88  PI-ACCT-ACTIVE              VALUE 'Y'.
00113          16  FILLER                      PIC  X(163).
00114
00115 ******************************************************************
00106  EJECT
00107 *                                COPY EL6451S.
       01  EL645CI.
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
           05  CASOFDTL PIC S9(0004) COMP.
           05  CASOFDTF PIC  X(0001).
           05  FILLER REDEFINES CASOFDTF.
               10  CASOFDTA PIC  X(0001).
           05  CASOFDTI PIC  X(0008).
      *    -------------------------------
           05  CRPTNGL PIC S9(0004) COMP.
           05  CRPTNGF PIC  X(0001).
           05  FILLER REDEFINES CRPTNGF.
               10  CRPTNGA PIC  X(0001).
           05  CRPTNGI PIC  X(0008).
      *    -------------------------------
           05  CTYPEL PIC S9(0004) COMP.
           05  CTYPEF PIC  X(0001).
           05  FILLER REDEFINES CTYPEF.
               10  CTYPEA PIC  X(0001).
           05  CTYPEI PIC  X(0010).
      *    -------------------------------
           05  CNAMEL PIC S9(0004) COMP.
           05  CNAMEF PIC  X(0001).
           05  FILLER REDEFINES CNAMEF.
               10  CNAMEA PIC  X(0001).
           05  CNAMEI PIC  X(0030).
      *    -------------------------------
           05  CACTL PIC S9(0004) COMP.
           05  CACTF PIC  X(0001).
           05  FILLER REDEFINES CACTF.
               10  CACTA PIC  X(0001).
           05  CACTI PIC  X(0008).
      *    -------------------------------
           05  CRPTNG2L PIC S9(0004) COMP.
           05  CRPTNG2F PIC  X(0001).
           05  FILLER REDEFINES CRPTNG2F.
               10  CRPTNG2A PIC  X(0001).
           05  CRPTNG2I PIC  X(0008).
      *    -------------------------------
           05  CTYPE2L PIC S9(0004) COMP.
           05  CTYPE2F PIC  X(0001).
           05  FILLER REDEFINES CTYPE2F.
               10  CTYPE2A PIC  X(0001).
           05  CTYPE2I PIC  X(0010).
      *    -------------------------------
           05  CNAME2L PIC S9(0004) COMP.
           05  CNAME2F PIC  X(0001).
           05  FILLER REDEFINES CNAME2F.
               10  CNAME2A PIC  X(0001).
           05  CNAME2I PIC  X(0030).
      *    -------------------------------
           05  CYTNETL PIC S9(0004) COMP.
           05  CYTNETF PIC  X(0001).
           05  FILLER REDEFINES CYTNETF.
               10  CYTNETA PIC  X(0001).
           05  CYTNETI PIC  X(0012).
      *    -------------------------------
           05  CYTEARNL PIC S9(0004) COMP.
           05  CYTEARNF PIC  X(0001).
           05  FILLER REDEFINES CYTEARNF.
               10  CYTEARNA PIC  X(0001).
           05  CYTEARNI PIC  X(0012).
      *    -------------------------------
           05  CYTPAIDL PIC S9(0004) COMP.
           05  CYTPAIDF PIC  X(0001).
           05  FILLER REDEFINES CYTPAIDF.
               10  CYTPAIDA PIC  X(0001).
           05  CYTPAIDI PIC  X(0012).
      *    -------------------------------
           05  CYTRESL PIC S9(0004) COMP.
           05  CYTRESF PIC  X(0001).
           05  FILLER REDEFINES CYTRESF.
               10  CYTRESA PIC  X(0001).
           05  CYTRESI PIC  X(0012).
      *    -------------------------------
           05  CYTINCL PIC S9(0004) COMP.
           05  CYTINCF PIC  X(0001).
           05  FILLER REDEFINES CYTINCF.
               10  CYTINCA PIC  X(0001).
           05  CYTINCI PIC  X(0012).
      *    -------------------------------
           05  CYTRATOL PIC S9(0004) COMP.
           05  CYTRATOF PIC  X(0001).
           05  FILLER REDEFINES CYTRATOF.
               10  CYTRATOA PIC  X(0001).
           05  CYTRATOI PIC  X(0006).
      *    -------------------------------
           05  CITNETL PIC S9(0004) COMP.
           05  CITNETF PIC  X(0001).
           05  FILLER REDEFINES CITNETF.
               10  CITNETA PIC  X(0001).
           05  CITNETI PIC  X(0012).
      *    -------------------------------
           05  CITEARNL PIC S9(0004) COMP.
           05  CITEARNF PIC  X(0001).
           05  FILLER REDEFINES CITEARNF.
               10  CITEARNA PIC  X(0001).
           05  CITEARNI PIC  X(0012).
      *    -------------------------------
           05  CITPAIDL PIC S9(0004) COMP.
           05  CITPAIDF PIC  X(0001).
           05  FILLER REDEFINES CITPAIDF.
               10  CITPAIDA PIC  X(0001).
           05  CITPAIDI PIC  X(0012).
      *    -------------------------------
           05  CITRESL PIC S9(0004) COMP.
           05  CITRESF PIC  X(0001).
           05  FILLER REDEFINES CITRESF.
               10  CITRESA PIC  X(0001).
           05  CITRESI PIC  X(0012).
      *    -------------------------------
           05  CITINCL PIC S9(0004) COMP.
           05  CITINCF PIC  X(0001).
           05  FILLER REDEFINES CITINCF.
               10  CITINCA PIC  X(0001).
           05  CITINCI PIC  X(0012).
      *    -------------------------------
           05  CITRATOL PIC S9(0004) COMP.
           05  CITRATOF PIC  X(0001).
           05  FILLER REDEFINES CITRATOF.
               10  CITRATOA PIC  X(0001).
           05  CITRATOI PIC  X(0006).
      *    -------------------------------
           05  CYLNETL PIC S9(0004) COMP.
           05  CYLNETF PIC  X(0001).
           05  FILLER REDEFINES CYLNETF.
               10  CYLNETA PIC  X(0001).
           05  CYLNETI PIC  X(0012).
      *    -------------------------------
           05  CYLEARNL PIC S9(0004) COMP.
           05  CYLEARNF PIC  X(0001).
           05  FILLER REDEFINES CYLEARNF.
               10  CYLEARNA PIC  X(0001).
           05  CYLEARNI PIC  X(0012).
      *    -------------------------------
           05  CYLPAIDL PIC S9(0004) COMP.
           05  CYLPAIDF PIC  X(0001).
           05  FILLER REDEFINES CYLPAIDF.
               10  CYLPAIDA PIC  X(0001).
           05  CYLPAIDI PIC  X(0012).
      *    -------------------------------
           05  CYLRESL PIC S9(0004) COMP.
           05  CYLRESF PIC  X(0001).
           05  FILLER REDEFINES CYLRESF.
               10  CYLRESA PIC  X(0001).
           05  CYLRESI PIC  X(0012).
      *    -------------------------------
           05  CYLINCL PIC S9(0004) COMP.
           05  CYLINCF PIC  X(0001).
           05  FILLER REDEFINES CYLINCF.
               10  CYLINCA PIC  X(0001).
           05  CYLINCI PIC  X(0012).
      *    -------------------------------
           05  CYLRATOL PIC S9(0004) COMP.
           05  CYLRATOF PIC  X(0001).
           05  FILLER REDEFINES CYLRATOF.
               10  CYLRATOA PIC  X(0001).
           05  CYLRATOI PIC  X(0006).
      *    -------------------------------
           05  CILNETL PIC S9(0004) COMP.
           05  CILNETF PIC  X(0001).
           05  FILLER REDEFINES CILNETF.
               10  CILNETA PIC  X(0001).
           05  CILNETI PIC  X(0012).
      *    -------------------------------
           05  CILEARNL PIC S9(0004) COMP.
           05  CILEARNF PIC  X(0001).
           05  FILLER REDEFINES CILEARNF.
               10  CILEARNA PIC  X(0001).
           05  CILEARNI PIC  X(0012).
      *    -------------------------------
           05  CILPAIDL PIC S9(0004) COMP.
           05  CILPAIDF PIC  X(0001).
           05  FILLER REDEFINES CILPAIDF.
               10  CILPAIDA PIC  X(0001).
           05  CILPAIDI PIC  X(0012).
      *    -------------------------------
           05  CILRESL PIC S9(0004) COMP.
           05  CILRESF PIC  X(0001).
           05  FILLER REDEFINES CILRESF.
               10  CILRESA PIC  X(0001).
           05  CILRESI PIC  X(0012).
      *    -------------------------------
           05  CILINCL PIC S9(0004) COMP.
           05  CILINCF PIC  X(0001).
           05  FILLER REDEFINES CILINCF.
               10  CILINCA PIC  X(0001).
           05  CILINCI PIC  X(0012).
      *    -------------------------------
           05  CILRATOL PIC S9(0004) COMP.
           05  CILRATOF PIC  X(0001).
           05  FILLER REDEFINES CILRATOF.
               10  CILRATOA PIC  X(0001).
           05  CILRATOI PIC  X(0006).
      *    -------------------------------
           05  CYANETL PIC S9(0004) COMP.
           05  CYANETF PIC  X(0001).
           05  FILLER REDEFINES CYANETF.
               10  CYANETA PIC  X(0001).
           05  CYANETI PIC  X(0012).
      *    -------------------------------
           05  CYAEARNL PIC S9(0004) COMP.
           05  CYAEARNF PIC  X(0001).
           05  FILLER REDEFINES CYAEARNF.
               10  CYAEARNA PIC  X(0001).
           05  CYAEARNI PIC  X(0012).
      *    -------------------------------
           05  CYAPAIDL PIC S9(0004) COMP.
           05  CYAPAIDF PIC  X(0001).
           05  FILLER REDEFINES CYAPAIDF.
               10  CYAPAIDA PIC  X(0001).
           05  CYAPAIDI PIC  X(0012).
      *    -------------------------------
           05  CYARESL PIC S9(0004) COMP.
           05  CYARESF PIC  X(0001).
           05  FILLER REDEFINES CYARESF.
               10  CYARESA PIC  X(0001).
           05  CYARESI PIC  X(0012).
      *    -------------------------------
           05  CYAINCL PIC S9(0004) COMP.
           05  CYAINCF PIC  X(0001).
           05  FILLER REDEFINES CYAINCF.
               10  CYAINCA PIC  X(0001).
           05  CYAINCI PIC  X(0012).
      *    -------------------------------
           05  CYARATOL PIC S9(0004) COMP.
           05  CYARATOF PIC  X(0001).
           05  FILLER REDEFINES CYARATOF.
               10  CYARATOA PIC  X(0001).
           05  CYARATOI PIC  X(0006).
      *    -------------------------------
           05  CIANETL PIC S9(0004) COMP.
           05  CIANETF PIC  X(0001).
           05  FILLER REDEFINES CIANETF.
               10  CIANETA PIC  X(0001).
           05  CIANETI PIC  X(0012).
      *    -------------------------------
           05  CIAEARNL PIC S9(0004) COMP.
           05  CIAEARNF PIC  X(0001).
           05  FILLER REDEFINES CIAEARNF.
               10  CIAEARNA PIC  X(0001).
           05  CIAEARNI PIC  X(0012).
      *    -------------------------------
           05  CIAPAIDL PIC S9(0004) COMP.
           05  CIAPAIDF PIC  X(0001).
           05  FILLER REDEFINES CIAPAIDF.
               10  CIAPAIDA PIC  X(0001).
           05  CIAPAIDI PIC  X(0012).
      *    -------------------------------
           05  CIARESL PIC S9(0004) COMP.
           05  CIARESF PIC  X(0001).
           05  FILLER REDEFINES CIARESF.
               10  CIARESA PIC  X(0001).
           05  CIARESI PIC  X(0012).
      *    -------------------------------
           05  CIAINCL PIC S9(0004) COMP.
           05  CIAINCF PIC  X(0001).
           05  FILLER REDEFINES CIAINCF.
               10  CIAINCA PIC  X(0001).
           05  CIAINCI PIC  X(0012).
      *    -------------------------------
           05  CIARATOL PIC S9(0004) COMP.
           05  CIARATOF PIC  X(0001).
           05  FILLER REDEFINES CIARATOF.
               10  CIARATOA PIC  X(0001).
           05  CIARATOI PIC  X(0006).
      *    -------------------------------
           05  CCEXPDTL PIC S9(0004) COMP.
           05  CCEXPDTF PIC  X(0001).
           05  FILLER REDEFINES CCEXPDTF.
               10  CCEXPDTA PIC  X(0001).
           05  CCEXPDTI PIC  X(0008).
      *    -------------------------------
           05  CPEXPDTL PIC S9(0004) COMP.
           05  CPEXPDTF PIC  X(0001).
           05  FILLER REDEFINES CPEXPDTF.
               10  CPEXPDTA PIC  X(0001).
           05  CPEXPDTI PIC  X(0008).
      *    -------------------------------
           05  CCREINL PIC S9(0004) COMP.
           05  CCREINF PIC  X(0001).
           05  FILLER REDEFINES CCREINF.
               10  CCREINA PIC  X(0001).
           05  CCREINI PIC  X(0003).
      *    -------------------------------
           05  CCRETROL PIC S9(0004) COMP.
           05  CCRETROF PIC  X(0001).
           05  FILLER REDEFINES CCRETROF.
               10  CCRETROA PIC  X(0001).
           05  CCRETROI PIC  X(0001).
      *    -------------------------------
           05  CCBASISL PIC S9(0004) COMP.
           05  CCBASISF PIC  X(0001).
           05  FILLER REDEFINES CCBASISF.
               10  CCBASISA PIC  X(0001).
           05  CCBASISI PIC  X(0001).
      *    -------------------------------
           05  CPREINL PIC S9(0004) COMP.
           05  CPREINF PIC  X(0001).
           05  FILLER REDEFINES CPREINF.
               10  CPREINA PIC  X(0001).
           05  CPREINI PIC  X(0003).
      *    -------------------------------
           05  CPRETROL PIC S9(0004) COMP.
           05  CPRETROF PIC  X(0001).
           05  FILLER REDEFINES CPRETROF.
               10  CPRETROA PIC  X(0001).
           05  CPRETROI PIC  X(0001).
      *    -------------------------------
           05  CPBASISL PIC S9(0004) COMP.
           05  CPBASISF PIC  X(0001).
           05  FILLER REDEFINES CPBASISF.
               10  CPBASISA PIC  X(0001).
           05  CPBASISI PIC  X(0001).
      *    -------------------------------
           05  CCAGT1L PIC S9(0004) COMP.
           05  CCAGT1F PIC  X(0001).
           05  FILLER REDEFINES CCAGT1F.
               10  CCAGT1A PIC  X(0001).
           05  CCAGT1I PIC  X(0010).
      *    -------------------------------
           05  CCSNG1L PIC S9(0004) COMP.
           05  CCSNG1F PIC  X(0001).
           05  FILLER REDEFINES CCSNG1F.
               10  CCSNG1A PIC  X(0001).
           05  CCSNG1I PIC  X(0005).
      *    -------------------------------
           05  CCJNT1L PIC S9(0004) COMP.
           05  CCJNT1F PIC  X(0001).
           05  FILLER REDEFINES CCJNT1F.
               10  CCJNT1A PIC  X(0001).
           05  CCJNT1I PIC  X(0005).
      *    -------------------------------
           05  CCAH1L PIC S9(0004) COMP.
           05  CCAH1F PIC  X(0001).
           05  FILLER REDEFINES CCAH1F.
               10  CCAH1A PIC  X(0001).
           05  CCAH1I PIC  X(0005).
      *    -------------------------------
           05  CPAGT1L PIC S9(0004) COMP.
           05  CPAGT1F PIC  X(0001).
           05  FILLER REDEFINES CPAGT1F.
               10  CPAGT1A PIC  X(0001).
           05  CPAGT1I PIC  X(0010).
      *    -------------------------------
           05  CPSNG1L PIC S9(0004) COMP.
           05  CPSNG1F PIC  X(0001).
           05  FILLER REDEFINES CPSNG1F.
               10  CPSNG1A PIC  X(0001).
           05  CPSNG1I PIC  X(0005).
      *    -------------------------------
           05  CPJNT1L PIC S9(0004) COMP.
           05  CPJNT1F PIC  X(0001).
           05  FILLER REDEFINES CPJNT1F.
               10  CPJNT1A PIC  X(0001).
           05  CPJNT1I PIC  X(0005).
      *    -------------------------------
           05  CPAH1L PIC S9(0004) COMP.
           05  CPAH1F PIC  X(0001).
           05  FILLER REDEFINES CPAH1F.
               10  CPAH1A PIC  X(0001).
           05  CPAH1I PIC  X(0005).
      *    -------------------------------
           05  CCAGT2L PIC S9(0004) COMP.
           05  CCAGT2F PIC  X(0001).
           05  FILLER REDEFINES CCAGT2F.
               10  CCAGT2A PIC  X(0001).
           05  CCAGT2I PIC  X(0010).
      *    -------------------------------
           05  CCSNG2L PIC S9(0004) COMP.
           05  CCSNG2F PIC  X(0001).
           05  FILLER REDEFINES CCSNG2F.
               10  CCSNG2A PIC  X(0001).
           05  CCSNG2I PIC  X(0005).
      *    -------------------------------
           05  CCJNT2L PIC S9(0004) COMP.
           05  CCJNT2F PIC  X(0001).
           05  FILLER REDEFINES CCJNT2F.
               10  CCJNT2A PIC  X(0001).
           05  CCJNT2I PIC  X(0005).
      *    -------------------------------
           05  CCAH2L PIC S9(0004) COMP.
           05  CCAH2F PIC  X(0001).
           05  FILLER REDEFINES CCAH2F.
               10  CCAH2A PIC  X(0001).
           05  CCAH2I PIC  X(0005).
      *    -------------------------------
           05  CPAGT2L PIC S9(0004) COMP.
           05  CPAGT2F PIC  X(0001).
           05  FILLER REDEFINES CPAGT2F.
               10  CPAGT2A PIC  X(0001).
           05  CPAGT2I PIC  X(0010).
      *    -------------------------------
           05  CPSNG2L PIC S9(0004) COMP.
           05  CPSNG2F PIC  X(0001).
           05  FILLER REDEFINES CPSNG2F.
               10  CPSNG2A PIC  X(0001).
           05  CPSNG2I PIC  X(0005).
      *    -------------------------------
           05  CPJNT2L PIC S9(0004) COMP.
           05  CPJNT2F PIC  X(0001).
           05  FILLER REDEFINES CPJNT2F.
               10  CPJNT2A PIC  X(0001).
           05  CPJNT2I PIC  X(0005).
      *    -------------------------------
           05  CPAH2L PIC S9(0004) COMP.
           05  CPAH2F PIC  X(0001).
           05  FILLER REDEFINES CPAH2F.
               10  CPAH2A PIC  X(0001).
           05  CPAH2I PIC  X(0005).
      *    -------------------------------
           05  CCAGT3L PIC S9(0004) COMP.
           05  CCAGT3F PIC  X(0001).
           05  FILLER REDEFINES CCAGT3F.
               10  CCAGT3A PIC  X(0001).
           05  CCAGT3I PIC  X(0010).
      *    -------------------------------
           05  CCSNG3L PIC S9(0004) COMP.
           05  CCSNG3F PIC  X(0001).
           05  FILLER REDEFINES CCSNG3F.
               10  CCSNG3A PIC  X(0001).
           05  CCSNG3I PIC  X(0005).
      *    -------------------------------
           05  CCJNT3L PIC S9(0004) COMP.
           05  CCJNT3F PIC  X(0001).
           05  FILLER REDEFINES CCJNT3F.
               10  CCJNT3A PIC  X(0001).
           05  CCJNT3I PIC  X(0005).
      *    -------------------------------
           05  CCAH3L PIC S9(0004) COMP.
           05  CCAH3F PIC  X(0001).
           05  FILLER REDEFINES CCAH3F.
               10  CCAH3A PIC  X(0001).
           05  CCAH3I PIC  X(0005).
      *    -------------------------------
           05  CPAGT3L PIC S9(0004) COMP.
           05  CPAGT3F PIC  X(0001).
           05  FILLER REDEFINES CPAGT3F.
               10  CPAGT3A PIC  X(0001).
           05  CPAGT3I PIC  X(0010).
      *    -------------------------------
           05  CPSNG3L PIC S9(0004) COMP.
           05  CPSNG3F PIC  X(0001).
           05  FILLER REDEFINES CPSNG3F.
               10  CPSNG3A PIC  X(0001).
           05  CPSNG3I PIC  X(0005).
      *    -------------------------------
           05  CPJNT3L PIC S9(0004) COMP.
           05  CPJNT3F PIC  X(0001).
           05  FILLER REDEFINES CPJNT3F.
               10  CPJNT3A PIC  X(0001).
           05  CPJNT3I PIC  X(0005).
      *    -------------------------------
           05  CPAH3L PIC S9(0004) COMP.
           05  CPAH3F PIC  X(0001).
           05  FILLER REDEFINES CPAH3F.
               10  CPAH3A PIC  X(0001).
           05  CPAH3I PIC  X(0005).
      *    -------------------------------
           05  CEMSG1L PIC S9(0004) COMP.
           05  CEMSG1F PIC  X(0001).
           05  FILLER REDEFINES CEMSG1F.
               10  CEMSG1A PIC  X(0001).
           05  CEMSG1I PIC  X(0079).
      *    -------------------------------
           05  CPFKL PIC S9(0004) COMP.
           05  CPFKF PIC  X(0001).
           05  FILLER REDEFINES CPFKF.
               10  CPFKA PIC  X(0001).
           05  CPFKI PIC  99.
       01  EL645CO REDEFINES EL645CI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CASOFDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRPTNGO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTYPEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNAMEO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRPTNG2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTYPE2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNAME2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CYTNETO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CYTEARNO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CYTPAIDO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CYTRESO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CYTINCO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CYTRATOO PIC  ZZZ.Z-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CITNETO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CITEARNO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CITPAIDO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CITRESO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CITINCO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CITRATOO PIC  ZZZ.Z-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CYLNETO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CYLEARNO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CYLPAIDO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CYLRESO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CYLINCO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CYLRATOO PIC  ZZZ.Z-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CILNETO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CILEARNO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CILPAIDO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CILRESO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CILINCO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CILRATOO PIC  ZZZ.Z-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CYANETO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CYAEARNO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CYAPAIDO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CYARESO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CYAINCO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CYARATOO PIC  ZZZ.Z-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CIANETO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CIAEARNO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CIAPAIDO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CIARESO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CIAINCO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CIARATOO PIC  ZZZ.Z-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCEXPDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPEXPDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCREINO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCRETROO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCBASISO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPREINO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPRETROO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPBASISO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAGT1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCSNG1O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCJNT1O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAH1O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPAGT1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPSNG1O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPJNT1O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPAH1O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAGT2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCSNG2O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCJNT2O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAH2O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPAGT2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPSNG2O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPJNT2O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPAH2O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAGT3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCSNG3O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCJNT3O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAH3O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPAGT3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPSNG3O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPJNT3O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPAH3O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPFKO PIC  X(0002).
      *    -------------------------------
       01  EL645BI REDEFINES EL645CI.
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
           05  BASOFDTL PIC S9(0004) COMP.
           05  BASOFDTF PIC  X(0001).
           05  FILLER REDEFINES BASOFDTF.
               10  BASOFDTA PIC  X(0001).
           05  BASOFDTI PIC  X(0008).
      *    -------------------------------
           05  BRPTNGL PIC S9(0004) COMP.
           05  BRPTNGF PIC  X(0001).
           05  FILLER REDEFINES BRPTNGF.
               10  BRPTNGA PIC  X(0001).
           05  BRPTNGI PIC  X(0008).
      *    -------------------------------
           05  BTYPEL PIC S9(0004) COMP.
           05  BTYPEF PIC  X(0001).
           05  FILLER REDEFINES BTYPEF.
               10  BTYPEA PIC  X(0001).
           05  BTYPEI PIC  X(0010).
      *    -------------------------------
           05  BNAMEL PIC S9(0004) COMP.
           05  BNAMEF PIC  X(0001).
           05  FILLER REDEFINES BNAMEF.
               10  BNAMEA PIC  X(0001).
           05  BNAMEI PIC  X(0030).
      *    -------------------------------
           05  BYTNETL PIC S9(0004) COMP.
           05  BYTNETF PIC  X(0001).
           05  FILLER REDEFINES BYTNETF.
               10  BYTNETA PIC  X(0001).
           05  BYTNETI PIC  X(0012).
      *    -------------------------------
           05  BYTEARNL PIC S9(0004) COMP.
           05  BYTEARNF PIC  X(0001).
           05  FILLER REDEFINES BYTEARNF.
               10  BYTEARNA PIC  X(0001).
           05  BYTEARNI PIC  X(0012).
      *    -------------------------------
           05  BYTPAIDL PIC S9(0004) COMP.
           05  BYTPAIDF PIC  X(0001).
           05  FILLER REDEFINES BYTPAIDF.
               10  BYTPAIDA PIC  X(0001).
           05  BYTPAIDI PIC  X(0012).
      *    -------------------------------
           05  BYTRESL PIC S9(0004) COMP.
           05  BYTRESF PIC  X(0001).
           05  FILLER REDEFINES BYTRESF.
               10  BYTRESA PIC  X(0001).
           05  BYTRESI PIC  X(0012).
      *    -------------------------------
           05  BYTINCL PIC S9(0004) COMP.
           05  BYTINCF PIC  X(0001).
           05  FILLER REDEFINES BYTINCF.
               10  BYTINCA PIC  X(0001).
           05  BYTINCI PIC  X(0012).
      *    -------------------------------
           05  BYTRATOL PIC S9(0004) COMP.
           05  BYTRATOF PIC  X(0001).
           05  FILLER REDEFINES BYTRATOF.
               10  BYTRATOA PIC  X(0001).
           05  BYTRATOI PIC  X(0006).
      *    -------------------------------
           05  BITNETL PIC S9(0004) COMP.
           05  BITNETF PIC  X(0001).
           05  FILLER REDEFINES BITNETF.
               10  BITNETA PIC  X(0001).
           05  BITNETI PIC  X(0012).
      *    -------------------------------
           05  BITEARNL PIC S9(0004) COMP.
           05  BITEARNF PIC  X(0001).
           05  FILLER REDEFINES BITEARNF.
               10  BITEARNA PIC  X(0001).
           05  BITEARNI PIC  X(0012).
      *    -------------------------------
           05  BITPAIDL PIC S9(0004) COMP.
           05  BITPAIDF PIC  X(0001).
           05  FILLER REDEFINES BITPAIDF.
               10  BITPAIDA PIC  X(0001).
           05  BITPAIDI PIC  X(0012).
      *    -------------------------------
           05  BITRESL PIC S9(0004) COMP.
           05  BITRESF PIC  X(0001).
           05  FILLER REDEFINES BITRESF.
               10  BITRESA PIC  X(0001).
           05  BITRESI PIC  X(0012).
      *    -------------------------------
           05  BITINCL PIC S9(0004) COMP.
           05  BITINCF PIC  X(0001).
           05  FILLER REDEFINES BITINCF.
               10  BITINCA PIC  X(0001).
           05  BITINCI PIC  X(0012).
      *    -------------------------------
           05  BITRATOL PIC S9(0004) COMP.
           05  BITRATOF PIC  X(0001).
           05  FILLER REDEFINES BITRATOF.
               10  BITRATOA PIC  X(0001).
           05  BITRATOI PIC  X(0006).
      *    -------------------------------
           05  BYLNETL PIC S9(0004) COMP.
           05  BYLNETF PIC  X(0001).
           05  FILLER REDEFINES BYLNETF.
               10  BYLNETA PIC  X(0001).
           05  BYLNETI PIC  X(0012).
      *    -------------------------------
           05  BYLEARNL PIC S9(0004) COMP.
           05  BYLEARNF PIC  X(0001).
           05  FILLER REDEFINES BYLEARNF.
               10  BYLEARNA PIC  X(0001).
           05  BYLEARNI PIC  X(0012).
      *    -------------------------------
           05  BYLPAIDL PIC S9(0004) COMP.
           05  BYLPAIDF PIC  X(0001).
           05  FILLER REDEFINES BYLPAIDF.
               10  BYLPAIDA PIC  X(0001).
           05  BYLPAIDI PIC  X(0012).
      *    -------------------------------
           05  BYLRESL PIC S9(0004) COMP.
           05  BYLRESF PIC  X(0001).
           05  FILLER REDEFINES BYLRESF.
               10  BYLRESA PIC  X(0001).
           05  BYLRESI PIC  X(0012).
      *    -------------------------------
           05  BYLINCL PIC S9(0004) COMP.
           05  BYLINCF PIC  X(0001).
           05  FILLER REDEFINES BYLINCF.
               10  BYLINCA PIC  X(0001).
           05  BYLINCI PIC  X(0012).
      *    -------------------------------
           05  BYLRATOL PIC S9(0004) COMP.
           05  BYLRATOF PIC  X(0001).
           05  FILLER REDEFINES BYLRATOF.
               10  BYLRATOA PIC  X(0001).
           05  BYLRATOI PIC  X(0006).
      *    -------------------------------
           05  BILNETL PIC S9(0004) COMP.
           05  BILNETF PIC  X(0001).
           05  FILLER REDEFINES BILNETF.
               10  BILNETA PIC  X(0001).
           05  BILNETI PIC  X(0012).
      *    -------------------------------
           05  BILEARNL PIC S9(0004) COMP.
           05  BILEARNF PIC  X(0001).
           05  FILLER REDEFINES BILEARNF.
               10  BILEARNA PIC  X(0001).
           05  BILEARNI PIC  X(0012).
      *    -------------------------------
           05  BILPAIDL PIC S9(0004) COMP.
           05  BILPAIDF PIC  X(0001).
           05  FILLER REDEFINES BILPAIDF.
               10  BILPAIDA PIC  X(0001).
           05  BILPAIDI PIC  X(0012).
      *    -------------------------------
           05  BILRESL PIC S9(0004) COMP.
           05  BILRESF PIC  X(0001).
           05  FILLER REDEFINES BILRESF.
               10  BILRESA PIC  X(0001).
           05  BILRESI PIC  X(0012).
      *    -------------------------------
           05  BILINCL PIC S9(0004) COMP.
           05  BILINCF PIC  X(0001).
           05  FILLER REDEFINES BILINCF.
               10  BILINCA PIC  X(0001).
           05  BILINCI PIC  X(0012).
      *    -------------------------------
           05  BILRATOL PIC S9(0004) COMP.
           05  BILRATOF PIC  X(0001).
           05  FILLER REDEFINES BILRATOF.
               10  BILRATOA PIC  X(0001).
           05  BILRATOI PIC  X(0006).
      *    -------------------------------
           05  BYANETL PIC S9(0004) COMP.
           05  BYANETF PIC  X(0001).
           05  FILLER REDEFINES BYANETF.
               10  BYANETA PIC  X(0001).
           05  BYANETI PIC  X(0012).
      *    -------------------------------
           05  BYAEARNL PIC S9(0004) COMP.
           05  BYAEARNF PIC  X(0001).
           05  FILLER REDEFINES BYAEARNF.
               10  BYAEARNA PIC  X(0001).
           05  BYAEARNI PIC  X(0012).
      *    -------------------------------
           05  BYAPAIDL PIC S9(0004) COMP.
           05  BYAPAIDF PIC  X(0001).
           05  FILLER REDEFINES BYAPAIDF.
               10  BYAPAIDA PIC  X(0001).
           05  BYAPAIDI PIC  X(0012).
      *    -------------------------------
           05  BYARESL PIC S9(0004) COMP.
           05  BYARESF PIC  X(0001).
           05  FILLER REDEFINES BYARESF.
               10  BYARESA PIC  X(0001).
           05  BYARESI PIC  X(0012).
      *    -------------------------------
           05  BYAINCL PIC S9(0004) COMP.
           05  BYAINCF PIC  X(0001).
           05  FILLER REDEFINES BYAINCF.
               10  BYAINCA PIC  X(0001).
           05  BYAINCI PIC  X(0012).
      *    -------------------------------
           05  BYARATOL PIC S9(0004) COMP.
           05  BYARATOF PIC  X(0001).
           05  FILLER REDEFINES BYARATOF.
               10  BYARATOA PIC  X(0001).
           05  BYARATOI PIC  X(0006).
      *    -------------------------------
           05  BIANETL PIC S9(0004) COMP.
           05  BIANETF PIC  X(0001).
           05  FILLER REDEFINES BIANETF.
               10  BIANETA PIC  X(0001).
           05  BIANETI PIC  X(0012).
      *    -------------------------------
           05  BIAEARNL PIC S9(0004) COMP.
           05  BIAEARNF PIC  X(0001).
           05  FILLER REDEFINES BIAEARNF.
               10  BIAEARNA PIC  X(0001).
           05  BIAEARNI PIC  X(0012).
      *    -------------------------------
           05  BIAPAIDL PIC S9(0004) COMP.
           05  BIAPAIDF PIC  X(0001).
           05  FILLER REDEFINES BIAPAIDF.
               10  BIAPAIDA PIC  X(0001).
           05  BIAPAIDI PIC  X(0012).
      *    -------------------------------
           05  BIARESL PIC S9(0004) COMP.
           05  BIARESF PIC  X(0001).
           05  FILLER REDEFINES BIARESF.
               10  BIARESA PIC  X(0001).
           05  BIARESI PIC  X(0012).
      *    -------------------------------
           05  BIAINCL PIC S9(0004) COMP.
           05  BIAINCF PIC  X(0001).
           05  FILLER REDEFINES BIAINCF.
               10  BIAINCA PIC  X(0001).
           05  BIAINCI PIC  X(0012).
      *    -------------------------------
           05  BIARATOL PIC S9(0004) COMP.
           05  BIARATOF PIC  X(0001).
           05  FILLER REDEFINES BIARATOF.
               10  BIARATOA PIC  X(0001).
           05  BIARATOI PIC  X(0006).
      *    -------------------------------
           05  BEMSG1L PIC S9(0004) COMP.
           05  BEMSG1F PIC  X(0001).
           05  FILLER REDEFINES BEMSG1F.
               10  BEMSG1A PIC  X(0001).
           05  BEMSG1I PIC  X(0079).
      *    -------------------------------
           05  BPFKL PIC S9(0004) COMP.
           05  BPFKF PIC  X(0001).
           05  FILLER REDEFINES BPFKF.
               10  BPFKA PIC  X(0001).
           05  BPFKI PIC  99.
       01  EL645BO REDEFINES EL645CI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BASOFDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRPTNGO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAMEO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BYTNETO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BYTEARNO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BYTPAIDO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BYTRESO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BYTINCO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BYTRATOO PIC  ZZZ.Z-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BITNETO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BITEARNO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BITPAIDO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BITRESO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BITINCO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BITRATOO PIC  ZZZ.Z-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BYLNETO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BYLEARNO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BYLPAIDO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BYLRESO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BYLINCO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BYLRATOO PIC  ZZZ.Z-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BILNETO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BILEARNO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BILPAIDO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BILRESO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BILINCO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BILRATOO PIC  ZZZ.Z-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BYANETO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BYAEARNO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BYAPAIDO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BYARESO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BYAINCO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BYARATOO PIC  ZZZ.Z-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIANETO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIAEARNO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIAPAIDO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIARESO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIAINCO PIC  ZZZ,ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIARATOO PIC  ZZZ.Z-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFKO PIC  X(0002).
      *    -------------------------------
00108  EJECT
00109 *                                COPY ELCDATE.
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
00110  EJECT
00111 *                                COPY ELCEMIB.
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
00112  EJECT
00113 *                                COPY ELCLOGOF.
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
00114  EJECT
00115 *                                COPY ELCATTR.
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
00116  EJECT
00117 *                                COPY ELCAID.
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
00118
00119  01  FILLER  REDEFINES  DFHAID.
00120      12  FILLER                  PIC  X(08).
00121      12  PF-VALUES               PIC  X(01)
00122              OCCURS  24  TIMES.
00123
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
00125
00126  01  DFHCOMMAREA                 PIC  X(1024).
00127
00128 *01 PARMLIST             COMP.
00129 *    12  FILLER                  PIC S9(09).
00130 *    12  ERLOSS-POINTER          PIC S9(09).
00131  EJECT
00132 *                            COPY ERCLOSS.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCLOSS                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION  =  LOSS RATIOS                             *
00008 *                                                                *
00009 *   **NOTE**  THIS COPYBOOK IS USED FOR BOTH THE ON-LINE         *
00010 *             AND OFF-LINE LOSS-RATIO FILES.                     *
00011 *                                                                *
00012 *                                                                *
00013 *   FILE TYPE  =  VSAM,KSDS(ON-LINE),  SEQUENTIAL(OFFLINE)       *
00014 *   RECORD SIZE = 525           RECFORM = FIXED                  *
00015 *                                                                *
00016 *   BASE CLUSTER NAME  =  ERLOSS               RKP=2,LEN=47      *
00017 *                                                                *
00018 *   LOG = NO                                                     *
00019 *   SERVREQ = BROWSE                                             *
00020 *                                                                *
00021 ******************************************************************
00022
00023  01  LOSS-RATIO-MASTER.
00024
00025      12  LR-RECORD-ID        PIC XX.
00026          88  VALID-LR-RECORD           VALUE 'LR'.
00027
00028      12  LR-CONTROL.
00029          16  LR-COMPANY-CD   PIC X.
00030          16  LR-RCD-TYPE     PIC X.
00031              88  ACCOUNT-RECORD        VALUE 'A'.
00032              88  G-A-RECORD            VALUE 'G'.
00033              88  REIN-RECORD           VALUE 'R'.
00034              88  REPORT-CD1-RECORD     VALUE 'B'.
00035              88  REPORT-CD2-RECORD     VALUE 'C'.
00036              88  STATE-RECORD          VALUE 'S'.
00037          16  LR-REIN-CO      PIC XXX.
00038          16  LR-RPT-CD-1     PIC X(10).
00039          16  LR-CARRIER      PIC X.
00040          16  LR-GROUPING     PIC X(6).
00041          16  LR-GA-RPT-CD-2  PIC X(10).
00042          16  LR-STATE        PIC XX.
00043          16  LR-ACCOUNT      PIC X(10).
00044          16  LR-REIN-SUB     PIC XXX.
00045
00046      12  LR-DATA.
00047          16  LR-RUN-DATE     PIC X(6).
00048          16  LR-RUN-DATE-N REDEFINES
00049                 LR-RUN-DATE  PIC 9(6).
00050          16  LR-ACCT-NAME    PIC X(30).
00051          16  LR-ACCT-RANGES  OCCURS  2  TIMES.
00052              20  LR-EXP-DATE PIC X(6).
00053              20  LR-REI-TAB  PIC XXX.
00054              20  LR-RETRO    PIC X.
00055              20  LR-BASIS    PIC X.
00056              20  LR-GA-DATA  OCCURS  3  TIMES.
00057                  24  LR-AGT-NO   PIC X(10).
00058                  24  LR-SNG-PCT  PIC SV9(5)        COMP-3.
00059                  24  LR-SNG-PCT-X REDEFINES
00060                      LR-SNG-PCT  PIC XXX.
00061                  24  LR-JNT-PCT  PIC SV9(5)        COMP-3.
00062                  24  LR-JNT-PCT-X REDEFINES
00063                      LR-JNT-PCT  PIC XXX.
00064                  24  LR-A-H-PCT  PIC SV9(5)        COMP-3.
00065                  24  LR-A-H-PCT-X REDEFINES
00066                      LR-A-H-PCT  PIC XXX.
00067
00068          16  LR-REIN-NAME    PIC X(30).
00069          16  LR-G-A-NAME     PIC X(30).
00070
00071          16  LR-ACCT-STATUS  PIC X.
00072              88  ACCT-ACTIVE          VALUE 'A' '0'.
00073              88  ACCT-INACTIVE        VALUE 'I' '1'.
00074              88  ACCT-TRANSFERRED     VALUE 'T' '2'.
00075
00076          16  FILLER          PIC X(15).
00077
00078      12  LR-TOTALS  OCCURS  3  TIMES.
00079          16  LR-YTD-NET      PIC S9(11)V99     COMP-3.
00080          16  LR-YTD-EARN     PIC S9(11)V99     COMP-3.
00081          16  LR-YTD-PAID     PIC S9(11)V99     COMP-3.
00082          16  LR-YTD-RESV     PIC S9(11)V99     COMP-3.
00083          16  LR-YTD-INCUR    PIC S9(11)V99     COMP-3.
00084          16  LR-YTD-RATIO    PIC S9(4)V9       COMP-3.
00085          16  LR-ITD-NET      PIC S9(11)V99     COMP-3.
00086          16  LR-ITD-EARN     PIC S9(11)V99     COMP-3.
00087          16  LR-ITD-PAID     PIC S9(11)V99     COMP-3.
00088          16  LR-ITD-RESV     PIC S9(11)V99     COMP-3.
00089          16  LR-ITD-INCUR    PIC S9(11)V99     COMP-3.
00090          16  LR-ITD-RATIO    PIC S9(4)V9       COMP-3.
00091
00092 ******************************************************************
00133  EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA LOSS-RATIO-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6451' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00135
00136      CONTINUE.
00137
00138      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00139      MOVE '5'                    TO  DC-OPTION-CODE.
00140
00141      PERFORM 8600-DATE-CONVERSION.
00142
00143      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00144      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00145      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00146
00147 *    NOTE *******************************************************
00148 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
00149 *         *  FROM ANOTHER MODULE.                               *
00150 *         *******************************************************.
00151
00152      IF EIBCALEN  IS NOT GREATER THAN  ZERO
00153          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00154          PERFORM 8500-SEND-TEXT.
00155
00156      
      * EXEC CICS HANDLE CONDITION
00157 *        PGMIDERR  (9600-PGMIDERR)
00158 *        NOTFND    (8700-NOT-FOUND)
00159 *        ENDFILE   (1200-ENDFILE)
00160 *        DUPKEY    (1010-DUPKEY)
00161 *        ERROR     (9800-ERROR)
00162 *    END-EXEC.
      *    MOVE '"$LI''$.               ! " #00002295' TO DFHEIV0
           MOVE X'22244C4927242E2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303032323935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00163  EJECT
00164  0100-MAIN-LOGIC.
00165      IF PI-CALLING-PROGRAM  IS EQUAL TO  THIS-PGM
00166          GO TO 0110-MAIN-LOGIC.
00167
00168      IF PI-RETURN-TO-PROGRAM  IS NOT EQUAL TO  THIS-PGM
00169          MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
00170          MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
00171          MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
00172          MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
00173          MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
00174          MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
00175          MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
00176          MOVE THIS-PGM              TO  PI-CALLING-PROGRAM
00177      ELSE
00178          MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
00179          MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
00180          MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
00181          MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
00182          MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
00183          MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
00184          MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
00185          MOVE SPACES                TO  PI-SAVED-PROGRAM-6.
00186
00187      MOVE PI-LOSS-RATIO-KEY      TO  PI-1ST-KEY.
00188
00189      PERFORM 5100-READ-DIRECT  THRU  5199-EXIT.
00190
00191      CONTINUE.
00192
00193      IF PI-MAPNAME  IS EQUAL TO  'EL645B  '
00194          PERFORM 1000-BUILD-SCREEN
00195      ELSE
00196          PERFORM 1100-BUILD-SCREEN.
00197  EJECT
00198  0110-MAIN-LOGIC.
00199 *    NOTE *******************************************************
00200 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *
00201 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *
00202 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
00203 *         *******************************************************.
00204
00205      IF EIBAID  IS EQUAL TO  DFHCLEAR
00206          PERFORM 9400-CLEAR.
00207
00208      IF PI-MAPNAME  IS EQUAL TO  'EL645C  '
00209          GO TO 0200-RECEIVE-MAP-C.
00210
00211
00212  0120-RECEIVE-MAP-B.
00213      MOVE LOW-VALUES             TO  EL645BI.
00214
00215      IF EIBAID  IS EQUAL TO  DFHPA1  OR  DFHPA2  OR  DFHPA3
00216          MOVE LOW-VALUES         TO  EL645BO
00217          MOVE -1                 TO  BPFKL
00218          MOVE ER-0008            TO  EMI-ERROR
00219          PERFORM 8300-SEND-DATAONLY
00220          GO TO 9100-RETURN-TRAN.
00221
00222      
      * EXEC CICS RECEIVE
00223 *        MAPSET  (WS-MAPSET-NAME)
00224 *        MAP     (WS-MAP-B-NAME)
00225 *        INTO    (EL645BI)
00226 *    END-EXEC.
           MOVE LENGTH OF
            EL645BI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00002361' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-B-NAME, 
                 EL645BI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00227
00228      IF BPFKL  IS GREATER THAN  ZERO
00229          IF EIBAID  IS NOT EQUAL TO  DFHENTER
00230              MOVE ER-0004        TO  EMI-ERROR
00231              MOVE AL-UNBOF       TO  BPFKA
00232              MOVE -1             TO  BPFKL
00233              PERFORM 8300-SEND-DATAONLY
00234              GO TO 9100-RETURN-TRAN
00235          ELSE
00236              IF (BPFKI  IS NUMERIC)
00237                AND (BPFKI  IS GREATER THAN  ZERO
00238                    AND BPFKI  IS LESS THAN  25)
00239                      MOVE PF-VALUES (BPFKI)
00240                                  TO  EIBAID
00241                  ELSE
00242                      MOVE ER-0029
00243                                  TO  EMI-ERROR
00244                      MOVE AL-UNBOF
00245                                  TO  BPFKA
00246                      MOVE -1     TO  BPFKL
00247                      PERFORM 8300-SEND-DATAONLY
00248                      GO TO 9100-RETURN-TRAN.
00249
00250 *    NOTE *******************************************************
00251 *         *      PF KEY      USAGE                              *
00252 *         *        PF1       SEARCH FORWARD                     *
00253 *         *        PF2       SEARCH BACKWARD                    *
00254 *         *        PF3       ACCOUNT DETAIL                     *
00255 *         *        PF12      HELP                               *
00256 *         *        PF23      LOGOFF                             *
00257 *         *        PF24      RETURN TO MASTER MENU              *
00258 *         *******************************************************.
00259
00260      IF EIBAID  IS EQUAL TO  DFHPF12
00261          MOVE 'EL010'            TO  THIS-PGM
00262          GO TO 9300-XCTL.
00263
00264      IF EIBAID  IS EQUAL TO  DFHPF23
00265          PERFORM 9000-RETURN-CICS.
00266
00267      IF EIBAID  IS EQUAL TO  DFHPF24
00268          MOVE 'EL626'            TO  THIS-PGM
00269          GO TO 9300-XCTL.
00270  SKIP3
00271  0130-MAP-B-LOGIC.
00272
00273      MOVE PI-SAVED-PROGRAM-1     TO  THIS-PGM.
00274
00275      IF EIBAID  IS EQUAL TO  DFHPF1
00276          GO TO 0140-PF-KEY-ONE.
00277
00278      IF EIBAID  IS EQUAL TO  DFHPF2
00279          GO TO 0150-PF-KEY-TWO.
00280
00281      MOVE ER-0029                TO  EMI-ERROR.
00282      MOVE AL-UNBOF               TO  BPFKA.
00283      MOVE -1                     TO  BPFKL.
00284
00285      PERFORM 8300-SEND-DATAONLY.
00286
00287      GO TO 9100-RETURN-TRAN.
00288
00289  0140-PF-KEY-ONE.
00290      PERFORM 5000-START-BROWSE  THRU  5099-EXIT.
00291
00292      PERFORM 5200-READ-NEXT  THRU  5299-EXIT.
00293
00294      PERFORM 5200-READ-NEXT  THRU  5299-EXIT.
00295
00296      CONTINUE.
00297
00298      PERFORM 1000-BUILD-SCREEN.
00299
00300  0150-PF-KEY-TWO.
00301      PERFORM 5000-START-BROWSE  THRU  5099-EXIT.
00302
00303      PERFORM 5300-READ-PREVIOUS  THRU  5399-EXIT.
00304
00305      PERFORM 5300-READ-PREVIOUS  THRU  5399-EXIT.
00306
00307      CONTINUE.
00308
00309      PERFORM 1000-BUILD-SCREEN.
00310  EJECT
00311  0200-RECEIVE-MAP-C.
00312      MOVE LOW-VALUES             TO  EL645CI.
00313
00314      IF EIBAID  IS EQUAL TO  DFHPA1  OR  DFHPA2  OR  DFHPA3
00315          MOVE LOW-VALUES         TO  EL645CO
00316          MOVE -1                 TO  CPFKL
00317          MOVE ER-0008            TO  EMI-ERROR
00318          PERFORM 8400-SEND-DATAONLY
00319          GO TO 9100-RETURN-TRAN.
00320
00321      
      * EXEC CICS RECEIVE
00322 *        MAPSET  (WS-MAPSET-NAME)
00323 *        MAP     (WS-MAP-C-NAME)
00324 *        INTO    (EL645CI)
00325 *    END-EXEC.
           MOVE LENGTH OF
            EL645CI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00002460' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-C-NAME, 
                 EL645CI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00326
00327      IF CPFKL  IS GREATER THAN  ZERO
00328          IF EIBAID  IS NOT EQUAL TO  DFHENTER
00329              MOVE ER-0004        TO  EMI-ERROR
00330              MOVE AL-UNBOF       TO  CPFKA
00331              MOVE -1             TO  CPFKL
00332              PERFORM 8400-SEND-DATAONLY
00333              GO TO 9100-RETURN-TRAN
00334          ELSE
00335              IF (CPFKI  IS NUMERIC)
00336                AND (CPFKI  IS GREATER THAN  ZERO
00337                    AND CPFKI  IS LESS THAN  25)
00338                      MOVE PF-VALUES (CPFKI)
00339                                  TO  EIBAID
00340                  ELSE
00341                      MOVE ER-0029
00342                                  TO  EMI-ERROR
00343                      MOVE AL-UNBOF
00344                                  TO  CPFKA
00345                      MOVE -1     TO  CPFKL
00346                      PERFORM 8400-SEND-DATAONLY
00347                      GO TO 9100-RETURN-TRAN.
00348
00349 *    NOTE *******************************************************
00350 *         *      PF KEY      USAGE                              *
00351 *         *        PF1       SEARCH FORWARD                     *
00352 *         *        PF2       SEARCH BACKWARD                    *
00353 *         *        PF3       ACCOUNT DETAIL                     *
00354 *         *        PF12      HELP                               *
00355 *         *        PF23      LOGOFF                             *
00356 *         *        PF24      RETURN TO MASTER MENU              *
00357 *         *******************************************************.
00358
00359      IF EIBAID  IS EQUAL TO  DFHPF12
00360          MOVE 'EL010'            TO  THIS-PGM
00361          GO TO 9300-XCTL.
00362
00363      IF EIBAID  IS EQUAL TO  DFHPF23
00364          PERFORM 9000-RETURN-CICS.
00365
00366      IF EIBAID  IS EQUAL TO  DFHPF24
00367          MOVE 'EL626'            TO  THIS-PGM
00368          GO TO 9300-XCTL.
00369  SKIP3
00370  0210-MAP-C-LOGIC.
00371      MOVE PI-SAVED-PROGRAM-1     TO  THIS-PGM.
00372
00373      IF EIBAID  IS EQUAL TO  DFHPF1
00374          GO TO 0220-PF-KEY-ONE.
00375
00376      IF EIBAID  IS EQUAL TO  DFHPF2
00377          GO TO 0250-PF-KEY-TWO.
00378
00379      MOVE ER-0029                TO  EMI-ERROR.
00380      MOVE AL-UNBOF               TO  CPFKA.
00381      MOVE -1                     TO  CPFKL.
00382
00383      PERFORM 8400-SEND-DATAONLY.
00384
00385      GO TO 9100-RETURN-TRAN.
00386
00387  0220-PF-KEY-ONE.
00388      PERFORM 5000-START-BROWSE  THRU  5099-EXIT.
00389
00390      PERFORM 5200-READ-NEXT  THRU  5299-EXIT.
00391
00392  0230-READ-NEXT.
00393      PERFORM 5200-READ-NEXT  THRU  5299-EXIT.
00394
00395      CONTINUE.
00396
00397      IF OPTION-ONE-SELECTED
00398        OR OPTION-TWO-SELECTED
00399        OR OPTION-THREE-SELECTED
00400          NEXT SENTENCE
00401      ELSE
00402          PERFORM 1100-BUILD-SCREEN.
00403
00404      IF LR-ACCOUNT  IS EQUAL TO  LOW-VALUES
00405          PERFORM 1100-BUILD-SCREEN.
00406
00407      IF PI-ACCT-ACTIVE
00408          IF ACCT-ACTIVE
00409              NEXT SENTENCE
00410          ELSE
00411              GO TO 0230-READ-NEXT.
00412
00413      PERFORM 1100-BUILD-SCREEN.
00414
00415  0250-PF-KEY-TWO.
00416      PERFORM 5000-START-BROWSE  THRU  5099-EXIT.
00417
00418      PERFORM 5300-READ-PREVIOUS  THRU  5399-EXIT.
00419
00420  0260-READ-PREVIOUS.
00421      PERFORM 5300-READ-PREVIOUS  THRU  5399-EXIT.
00422
00423      CONTINUE.
00424
00425      IF OPTION-ONE-SELECTED
00426        OR OPTION-TWO-SELECTED
00427        OR OPTION-THREE-SELECTED
00428          NEXT SENTENCE
00429      ELSE
00430          PERFORM 1100-BUILD-SCREEN.
00431
00432      IF LR-ACCOUNT  IS EQUAL TO  LOW-VALUES
00433          PERFORM 1100-BUILD-SCREEN.
00434
00435      IF PI-ACCT-ACTIVE
00436          IF ACCT-ACTIVE
00437              NEXT SENTENCE
00438          ELSE
00439              GO TO 0260-READ-PREVIOUS.
00440
00441      PERFORM 1100-BUILD-SCREEN.
00442  EJECT
00443  1000-BUILD-SCREEN  SECTION.
00444      MOVE LOW-VALUES             TO  EL645BO.
00445      MOVE PI-LOSS-RATIO-KEY      TO  PI-PREV-LOSS-RATIO-KEY.
00446
00447      GO TO 1020-NEXT-SENTENCE.
00448
00449  1010-DUPKEY.
00450      MOVE ER-0685                TO  EMI-ERROR.
00451      MOVE AL-UNBOF               TO  BPFKA.
00452      MOVE -1                     TO  BPFKL.
00453
00454      PERFORM 8300-SEND-DATAONLY.
00455
00456      GO TO 9100-RETURN-TRAN.
00457
00458  1020-NEXT-SENTENCE.
00459      IF LR-COMPANY-CD  IS EQUAL TO  PI-COMPANY-CD
00460          NEXT SENTENCE
00461      ELSE
00462          GO TO 1200-ENDFILE.
00463
00464      IF OPTION-ONE-SELECTED
00465          IF LR-RCD-TYPE  IS EQUAL TO  'A'
00466              GO TO 1030-CONTINUE
00467          ELSE
00468              GO TO 1200-ENDFILE.
00469
00470      IF OPTION-TWO-SELECTED
00471          IF LR-CARRIER  IS EQUAL TO  PI-SC-CARRIER
00472            AND LR-GROUPING  IS EQUAL TO  PI-SC-GROUPING
00473            AND LR-GA-RPT-CD-2  IS EQUAL TO  PI-SC-GA-RPT-CD-2
00474            AND LR-RCD-TYPE  IS EQUAL TO  'G'
00475              GO TO 1030-CONTINUE
00476          ELSE
00477              GO TO 1200-ENDFILE.
00478
00479      IF OPTION-THREE-SELECTED
00480          IF LR-REIN-CO  IS EQUAL TO  PI-SC-REIN-CO
00481            AND LR-RCD-TYPE  IS EQUAL TO  'R'
00482              GO TO 1030-CONTINUE
00483          ELSE
00484              GO TO 1200-ENDFILE.
00485
00486      IF OPTION-FOUR-SELECTED
00487          IF LR-RPT-CD-1  IS EQUAL TO  PI-SC-RPT-CD-1
00488            AND LR-RCD-TYPE  IS EQUAL TO  'B'
00489              GO TO 1030-CONTINUE
00490          ELSE
00491              GO TO 1200-ENDFILE.
00492
00493      IF OPTION-FIVE-SELECTED
00494          IF LR-CARRIER  IS EQUAL TO  PI-SC-CARRIER
00495            AND LR-GROUPING  IS EQUAL TO  PI-SC-GROUPING
00496            AND LR-GA-RPT-CD-2  IS EQUAL TO  PI-SC-GA-RPT-CD-2
00497            AND LR-RCD-TYPE  IS EQUAL TO  'C'
00498              GO TO 1030-CONTINUE
00499          ELSE
00500              GO TO 1200-ENDFILE.
00501
00502      IF OPTION-SIX-SELECTED
00503          IF LR-STATE  IS EQUAL TO  PI-SC-STATE
00504            AND LR-RCD-TYPE  IS EQUAL TO  'S'
00505              GO TO 1030-CONTINUE
00506          ELSE
00507              GO TO 1200-ENDFILE.
00508
00509  1030-CONTINUE.
00510 ******************************************************************
00511 *        SECURITY CHECK FOR CARRIER AND ACCOUNT NUMBER           *
00512 *                        04/04/84                                *
00513 ******************************************************************
00514
00515      IF OPTION-THREE-SELECTED
00516        OR OPTION-FOUR-SELECTED
00517        OR OPTION-SIX-SELECTED
00518          GO TO 1070-MOVE-DATA.
00519
00520      IF PI-NO-CARRIER-SECURITY
00521        AND PI-NO-ACCOUNT-SECURITY
00522          GO TO 1070-MOVE-DATA.
00523
00524      IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES
00525          NEXT SENTENCE
00526      ELSE
00527          GO TO 1050-CONTINUE.
00528
00529  1040-CHECK-CARRIER.
00530      IF LR-CARRIER  IS EQUAL TO  PI-CARRIER-SECURITY
00531          GO TO 1050-CONTINUE.
00532
00533      PERFORM 5200-READ-NEXT  THRU  5299-EXIT.
00534
00535      CONTINUE.
00536
00537      GO TO 1040-CHECK-CARRIER.
00538
00539  1050-CONTINUE.
00540      IF OPTION-TWO-SELECTED
00541        OR OPTION-FIVE-SELECTED
00542          GO TO 1070-MOVE-DATA.
00543
00544      IF PI-ACCOUNT-SECURITY  IS GREATER THAN  SPACES
00545          NEXT SENTENCE
00546      ELSE
00547          GO TO 1070-MOVE-DATA.
00548
00549  1060-CHECK-ACCOUNT.
00550      IF LR-ACCOUNT  IS EQUAL TO  PI-ACCOUNT-SECURITY
00551          GO TO 1070-MOVE-DATA.
00552
00553      PERFORM 5200-READ-NEXT  THRU  5299-EXIT.
00554
00555      CONTINUE.
00556
00557      GO TO 1060-CHECK-ACCOUNT.
00558
00559  1070-MOVE-DATA.
00560      MOVE PI-LOSS-RATIO-KEY      TO  PI-LIN1-LOSS-RATIO-KEY.
00561      MOVE LR-CONTROL             TO  PI-END-LOSS-RATIO-KEY.
00562      MOVE LR-RUN-DATE            TO  WS-LR-RUN-DATE.
00563      MOVE WS-LR-RUN-YR           TO  WS-LR-RUN-YR-EDIT.
00564      MOVE WS-LR-RUN-MO           TO  WS-LR-RUN-MO-EDIT.
00565      MOVE WS-LR-RUN-DA           TO  WS-LR-RUN-DA-EDIT.
00566      MOVE WS-LR-RUN-DATE-EDIT    TO  BASOFDTO.
00567
00568      IF OPTION-TWO-SELECTED
00569          MOVE 'GEN AGT:'         TO  BRPTNGO
00570          MOVE LR-GA-RPT-CD-2     TO  BTYPEO
00571          MOVE LR-G-A-NAME        TO  BNAMEO.
00572
00573      IF OPTION-THREE-SELECTED
00574          MOVE 'REIN CO:'         TO  BRPTNGO
00575          MOVE LR-REIN-CO         TO  BTYPEO
00576          MOVE LR-REIN-NAME       TO  BNAMEO.
00577
00578      IF OPTION-FOUR-SELECTED
00579          MOVE 'RPT CD1:'         TO  BRPTNGO
00580          MOVE LR-RPT-CD-1        TO  BTYPEO
00581          MOVE SPACES             TO  BNAMEO.
00582
00583      IF OPTION-FIVE-SELECTED
00584          MOVE 'RPT CD2:'         TO  BRPTNGO
00585          MOVE LR-GA-RPT-CD-2     TO  BTYPEO
00586          MOVE SPACES             TO  BNAMEO.
00587
00588      IF OPTION-SIX-SELECTED
00589          MOVE ' STATE :'         TO  BRPTNGO
00590          MOVE LR-STATE           TO  BTYPEO
00591          MOVE LR-ACCT-NAME       TO  BNAMEO.
00592
00593      IF LR-YTD-NET (1)  IS NUMERIC
00594          MOVE LR-YTD-NET (1)     TO  BYTNETO
00595      ELSE
00596          MOVE ZEROS              TO  BYTNETO.
00597
00598      IF LR-YTD-EARN (1)  IS NUMERIC
00599          MOVE LR-YTD-EARN (1)    TO  BYTEARNO
00600      ELSE
00601          MOVE ZEROS              TO  BYTEARNO.
00602
00603      IF LR-YTD-PAID (1)  IS NUMERIC
00604          MOVE LR-YTD-PAID (1)    TO  BYTPAIDO
00605      ELSE
00606          MOVE ZEROS              TO  BYTPAIDO.
00607
00608      IF LR-YTD-RESV (1)  IS NUMERIC
00609          MOVE LR-YTD-RESV (1)    TO  BYTRESO
00610      ELSE
00611          MOVE ZEROS              TO  BYTRESO.
00612
00613      IF LR-YTD-INCUR (1)  IS NUMERIC
00614          MOVE LR-YTD-INCUR (1)   TO  BYTINCO
00615      ELSE
00616          MOVE ZEROS              TO  BYTINCO.
00617
00618      IF LR-YTD-RATIO (1)  IS NUMERIC
00619          MOVE LR-YTD-RATIO (1)   TO  BYTRATOO
00620      ELSE
00621          MOVE ZEROS              TO  BYTRATOO.
00622
00623      IF LR-ITD-NET (1)  IS NUMERIC
00624          MOVE LR-ITD-NET (1)     TO  BITNETO
00625      ELSE
00626          MOVE ZEROS              TO  BITNETO.
00627
00628      IF LR-ITD-EARN (1)  IS NUMERIC
00629          MOVE LR-ITD-EARN (1)    TO  BITEARNO
00630      ELSE
00631          MOVE ZEROS              TO  BITEARNO.
00632
00633      IF LR-ITD-PAID (1)  IS NUMERIC
00634          MOVE LR-ITD-PAID (1)    TO  BITPAIDO
00635      ELSE
00636          MOVE ZEROS              TO  BITPAIDO.
00637
00638      IF LR-ITD-RESV (1)  IS NUMERIC
00639          MOVE LR-ITD-RESV (1)    TO  BITRESO
00640      ELSE
00641          MOVE ZEROS              TO  BITRESO.
00642
00643      IF LR-ITD-INCUR (1)  IS NUMERIC
00644          MOVE LR-ITD-INCUR (1)   TO  BITINCO
00645      ELSE
00646          MOVE ZEROS              TO  BITINCO.
00647
00648      IF LR-ITD-RATIO (1)  IS NUMERIC
00649          MOVE LR-ITD-RATIO (1)   TO  BITRATOO
00650      ELSE
00651          MOVE ZEROS              TO  BITRATOO.
00652
00653      IF LR-YTD-NET (2)  IS NUMERIC
00654          MOVE LR-YTD-NET (2)     TO  BYLNETO
00655      ELSE
00656          MOVE ZEROS              TO  BYLNETO.
00657
00658      IF LR-YTD-EARN (2)  IS NUMERIC
00659          MOVE LR-YTD-EARN (2)    TO  BYLEARNO
00660      ELSE
00661          MOVE ZEROS              TO  BYLEARNO.
00662
00663      IF LR-YTD-PAID (2)  IS NUMERIC
00664          MOVE LR-YTD-PAID (2)    TO  BYLPAIDO
00665      ELSE
00666          MOVE ZEROS              TO  BYLPAIDO.
00667
00668      IF LR-YTD-RESV (2)  IS NUMERIC
00669          MOVE LR-YTD-RESV (2)    TO  BYLRESO
00670      ELSE
00671          MOVE ZEROS              TO  BYLRESO.
00672
00673      IF LR-YTD-INCUR (2)  IS NUMERIC
00674          MOVE LR-YTD-INCUR (2)   TO  BYLINCO
00675      ELSE
00676          MOVE ZEROS              TO  BYLINCO.
00677
00678      IF LR-YTD-RATIO (2)  IS NUMERIC
00679          MOVE LR-YTD-RATIO (2)   TO  BYLRATOO
00680      ELSE
00681          MOVE ZEROS              TO  BYLRATOO.
00682
00683      IF LR-ITD-NET (2)  IS NUMERIC
00684          MOVE LR-ITD-NET (2)     TO  BILNETO
00685      ELSE
00686          MOVE ZEROS              TO  BILNETO.
00687
00688      IF LR-ITD-EARN (2)  IS NUMERIC
00689          MOVE LR-ITD-EARN (2)    TO  BILEARNO
00690      ELSE
00691          MOVE ZEROS              TO  BILEARNO.
00692
00693      IF LR-ITD-PAID (2)  IS NUMERIC
00694          MOVE LR-ITD-PAID (2)    TO  BILPAIDO
00695      ELSE
00696          MOVE ZEROS              TO  BILPAIDO.
00697
00698      IF LR-ITD-RESV (2)  IS NUMERIC
00699          MOVE LR-ITD-RESV (2)    TO  BILRESO
00700      ELSE
00701          MOVE ZEROS              TO  BILRESO.
00702
00703      IF LR-ITD-INCUR (2)  IS NUMERIC
00704          MOVE LR-ITD-INCUR (2)   TO  BILINCO
00705      ELSE
00706          MOVE ZEROS              TO  BILINCO.
00707
00708      IF LR-ITD-RATIO (2)  IS NUMERIC
00709          MOVE LR-ITD-RATIO (2)   TO  BILRATOO
00710      ELSE
00711          MOVE ZEROS              TO  BILRATOO.
00712
00713      IF LR-YTD-NET (3)  IS NUMERIC
00714          MOVE LR-YTD-NET (3)     TO  BYANETO
00715      ELSE
00716          MOVE ZEROS              TO  BYANETO.
00717
00718      IF LR-YTD-EARN (3)  IS NUMERIC
00719          MOVE LR-YTD-EARN (3)    TO  BYAEARNO
00720      ELSE
00721          MOVE ZEROS              TO  BYAEARNO.
00722
00723      IF LR-YTD-PAID (3)  IS NUMERIC
00724          MOVE LR-YTD-PAID (3)    TO  BYAPAIDO
00725      ELSE
00726          MOVE ZEROS              TO  BYAPAIDO.
00727
00728      IF LR-YTD-RESV (3)  IS NUMERIC
00729          MOVE LR-YTD-RESV (3)    TO  BYARESO
00730      ELSE
00731          MOVE ZEROS              TO  BYARESO.
00732
00733      IF LR-YTD-INCUR (3)  IS NUMERIC
00734          MOVE LR-YTD-INCUR (3)   TO  BYAINCO
00735      ELSE
00736          MOVE ZEROS              TO  BYAINCO.
00737
00738      IF LR-YTD-RATIO (3)  IS NUMERIC
00739          MOVE LR-YTD-RATIO (3)   TO  BYARATOO
00740      ELSE
00741          MOVE ZEROS              TO  BYARATOO.
00742
00743      IF LR-ITD-NET (3)  IS NUMERIC
00744          MOVE LR-ITD-NET (3)     TO  BIANETO
00745      ELSE
00746          MOVE ZEROS              TO  BIANETO.
00747
00748      IF LR-ITD-EARN (3)  IS NUMERIC
00749          MOVE LR-ITD-EARN (3)    TO  BIAEARNO
00750      ELSE
00751          MOVE ZEROS              TO  BIAEARNO.
00752
00753      IF LR-ITD-PAID (3)  IS NUMERIC
00754          MOVE LR-ITD-PAID (3)    TO  BIAPAIDO
00755      ELSE
00756          MOVE ZEROS              TO  BIAPAIDO.
00757
00758      IF LR-ITD-RESV (3)  IS NUMERIC
00759          MOVE LR-ITD-RESV (3)    TO  BIARESO
00760      ELSE
00761          MOVE ZEROS              TO  BIARESO.
00762
00763      IF LR-ITD-INCUR (3)  IS NUMERIC
00764          MOVE LR-ITD-INCUR (3)   TO  BIAINCO
00765      ELSE
00766          MOVE ZEROS              TO  BIAINCO.
00767
00768      IF LR-ITD-RATIO (3)  IS NUMERIC
00769          MOVE LR-ITD-RATIO (3)   TO  BIARATOO
00770      ELSE
00771          MOVE ZEROS              TO  BIARATOO.
00772
00773      MOVE 'EL645C  '             TO  PI-MAPNAME.
00774      MOVE AL-UNNON               TO  BPFKA.
00775      MOVE -1                     TO  BPFKL.
00776
00777      IF PI-BROWSE-SW  IS EQUAL TO  +1
00778          PERFORM 5400-END-BROWSE  THRU  5499-EXIT.
00779
00780      PERFORM 8300-SEND-DATAONLY.
00781
00782      GO TO 9100-RETURN-TRAN.
00783  EJECT
00784  1100-BUILD-SCREEN.
00785      MOVE LOW-VALUES             TO  EL645CO.
00786      MOVE PI-LOSS-RATIO-KEY      TO  PI-PREV-LOSS-RATIO-KEY.
00787
00788      IF LR-COMPANY-CD  IS EQUAL TO  PI-COMPANY-CD
00789          NEXT SENTENCE
00790      ELSE
00791          GO TO 1200-ENDFILE.
00792
00793      IF OPTION-ONE-SELECTED
00794          IF LR-RCD-TYPE  IS EQUAL TO  'A'
00795              GO TO 1105-CONTINUE
00796          ELSE
00797              GO TO 1200-ENDFILE.
00798
00799      IF OPTION-TWO-SELECTED
00800          IF LR-CARRIER  IS EQUAL TO  PI-SC-CARRIER
00801            AND LR-GROUPING  IS EQUAL TO  PI-SC-GROUPING
00802            AND LR-GA-RPT-CD-2  IS EQUAL TO  PI-SC-GA-RPT-CD-2
00803            AND LR-RCD-TYPE  IS EQUAL TO  'G'
00804              GO TO 1105-CONTINUE
00805          ELSE
00806              GO TO 1200-ENDFILE.
00807
00808      IF OPTION-THREE-SELECTED
00809          IF LR-REIN-CO  IS EQUAL TO  PI-SC-REIN-CO
00810            AND LR-RCD-TYPE  IS EQUAL TO  'R'
00811              GO TO 1105-CONTINUE
00812          ELSE
00813              GO TO 1200-ENDFILE.
00814
00815      IF OPTION-FOUR-SELECTED
00816          IF LR-RPT-CD-1  IS EQUAL TO  PI-SC-RPT-CD-1
00817            AND LR-RCD-TYPE  IS EQUAL TO  'B'
00818              GO TO 1105-CONTINUE
00819          ELSE
00820              GO TO 1200-ENDFILE.
00821
00822      IF OPTION-FIVE-SELECTED
00823          IF LR-CARRIER  IS EQUAL TO  PI-SC-CARRIER
00824            AND LR-GROUPING  IS EQUAL TO  PI-SC-GROUPING
00825            AND LR-GA-RPT-CD-2  IS EQUAL TO  PI-SC-GA-RPT-CD-2
00826            AND LR-RCD-TYPE  IS EQUAL TO  'C'
00827              GO TO 1105-CONTINUE
00828          ELSE
00829              GO TO 1200-ENDFILE.
00830
00831      IF OPTION-SIX-SELECTED
00832          IF LR-STATE  IS EQUAL TO  PI-SC-STATE
00833            AND LR-RCD-TYPE  IS EQUAL TO  'S'
00834              GO TO 1105-CONTINUE
00835          ELSE
00836              GO TO 1200-ENDFILE.
00837
00838  1105-CONTINUE.
00839 ******************************************************************
00840 *        SECURITY CHECK FOR CARRIER AND ACCOUNT NUMBER           *
00841 *                        04/04/84                                *
00842 ******************************************************************
00843
00844      IF OPTION-THREE-SELECTED
00845        OR OPTION-FOUR-SELECTED
00846        OR OPTION-SIX-SELECTED
00847          GO TO 1135-MOVE-DATA.
00848
00849      IF PI-NO-CARRIER-SECURITY
00850        AND PI-NO-ACCOUNT-SECURITY
00851          GO TO 1135-MOVE-DATA.
00852
00853      IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES
00854          NEXT SENTENCE
00855      ELSE
00856          GO TO 1120-CONTINUE.
00857
00858  1110-CHECK-CARRIER.
00859      IF LR-CARRIER  IS EQUAL TO  PI-CARRIER-SECURITY
00860          GO TO 1120-CONTINUE.
00861
00862  1115-READ-NEXT.
00863      PERFORM 5200-READ-NEXT  THRU  5299-EXIT.
00864
00865      CONTINUE.
00866
00867      IF OPTION-ONE-SELECTED
00868        OR OPTION-TWO-SELECTED
00869        OR OPTION-THREE-SELECTED
00870          NEXT SENTENCE
00871      ELSE
00872          GO TO 1110-CHECK-CARRIER.
00873
00874      IF LR-ACCOUNT  IS EQUAL TO  LOW-VALUES
00875          GO TO 1110-CHECK-CARRIER.
00876
00877      IF PI-ACCT-ACTIVE
00878          IF ACCT-ACTIVE
00879              NEXT SENTENCE
00880          ELSE
00881              GO TO 1115-READ-NEXT.
00882
00883      GO TO 1110-CHECK-CARRIER.
00884
00885  1120-CONTINUE.
00886      IF OPTION-TWO-SELECTED
00887        OR OPTION-FIVE-SELECTED
00888          GO TO 1135-MOVE-DATA.
00889
00890      IF PI-ACCOUNT-SECURITY  IS GREATER THAN  SPACES
00891          NEXT SENTENCE
00892      ELSE
00893          GO TO 1135-MOVE-DATA.
00894
00895  1125-CHECK-ACCOUNT.
00896      IF LR-ACCOUNT  IS EQUAL TO  PI-ACCOUNT-SECURITY
00897          GO TO 1135-MOVE-DATA.
00898
00899  1130-READ-NEXT.
00900      PERFORM 5200-READ-NEXT  THRU  5299-EXIT.
00901
00902      CONTINUE.
00903
00904      IF OPTION-ONE-SELECTED
00905        OR OPTION-TWO-SELECTED
00906        OR OPTION-THREE-SELECTED
00907          NEXT SENTENCE
00908      ELSE
00909          GO TO 1125-CHECK-ACCOUNT.
00910
00911      IF LR-ACCOUNT  IS EQUAL TO  LOW-VALUES
00912          GO TO 1125-CHECK-ACCOUNT.
00913
00914      IF PI-ACCT-ACTIVE
00915          IF ACCT-ACTIVE
00916              NEXT SENTENCE
00917          ELSE
00918              GO TO 1130-READ-NEXT.
00919
00920      GO TO 1125-CHECK-ACCOUNT.
00921
00922  1135-MOVE-DATA.
00923      MOVE PI-LOSS-RATIO-KEY      TO  PI-LIN1-LOSS-RATIO-KEY.
00924      MOVE LR-CONTROL             TO  PI-END-LOSS-RATIO-KEY.
00925      MOVE LR-RUN-DATE            TO  WS-LR-RUN-DATE.
00926      MOVE WS-LR-RUN-YR           TO  WS-LR-RUN-YR-EDIT.
00927      MOVE WS-LR-RUN-MO           TO  WS-LR-RUN-MO-EDIT.
00928      MOVE WS-LR-RUN-DA           TO  WS-LR-RUN-DA-EDIT.
00929      MOVE WS-LR-RUN-DATE-EDIT    TO  CASOFDTO.
00930      MOVE 'ACCOUNT:'             TO  CRPTNGO.
00931      MOVE LR-ACCOUNT             TO  CTYPEO.
00932      MOVE LR-ACCT-NAME           TO  CNAMEO.
00933
00934      IF ACCT-ACTIVE
00935          MOVE 'ACTIVE'           TO  CACTO
00936      ELSE
00937          MOVE 'INACTIVE'         TO  CACTO.
00938
00939      IF OPTION-ONE-SELECTED
00940          MOVE AL-PADOF           TO  CRPTNG2A
00941                                      CTYPE2A
00942                                      CNAME2A.
00943
00944      IF OPTION-TWO-SELECTED
00945          MOVE 'GEN AGT:'         TO  CRPTNG2O
00946          MOVE LR-GA-RPT-CD-2     TO  CTYPE2O
00947          MOVE LR-G-A-NAME        TO  CNAME2O.
00948
00949      IF OPTION-THREE-SELECTED
00950          MOVE 'REIN CO:'         TO  CRPTNG2O
00951          MOVE LR-REIN-CO         TO  CTYPE2O
00952          MOVE LR-REIN-NAME       TO  CNAME2O.
00953
00954      IF OPTION-FOUR-SELECTED
00955          MOVE 'RPT CD1:'         TO  CRPTNG2O
00956          MOVE LR-RPT-CD-1        TO  CTYPE2O
00957          MOVE SPACES             TO  CNAME2O.
00958
00959      IF OPTION-FIVE-SELECTED
00960          MOVE 'RPT CD2:'         TO  CRPTNG2O
00961          MOVE LR-GA-RPT-CD-2     TO  CTYPE2O
00962          MOVE SPACES             TO  CNAME2O.
00963
00964      IF LR-YTD-NET (1)  IS NUMERIC
00965          MOVE LR-YTD-NET (1)     TO  CYTNETO
00966      ELSE
00967          MOVE ZEROS              TO  CYTNETO.
00968
00969      IF LR-YTD-EARN (1)  IS NUMERIC
00970          MOVE LR-YTD-EARN (1)    TO  CYTEARNO
00971      ELSE
00972          MOVE ZEROS              TO  CYTEARNO.
00973
00974      IF LR-YTD-PAID (1)  IS NUMERIC
00975          MOVE LR-YTD-PAID (1)    TO  CYTPAIDO
00976      ELSE
00977          MOVE ZEROS              TO  CYTPAIDO.
00978
00979      IF LR-YTD-RESV (1)  IS NUMERIC
00980          MOVE LR-YTD-RESV (1)    TO  CYTRESO
00981      ELSE
00982          MOVE ZEROS              TO  CYTRESO.
00983
00984      IF LR-YTD-INCUR (1)  IS NUMERIC
00985          MOVE LR-YTD-INCUR (1)   TO  CYTINCO
00986      ELSE
00987          MOVE ZEROS              TO  CYTINCO.
00988
00989      IF LR-YTD-RATIO (1)  IS NUMERIC
00990          MOVE LR-YTD-RATIO (1)   TO  CYTRATOO
00991      ELSE
00992          MOVE ZEROS              TO  CYTRATOO.
00993
00994      IF LR-ITD-NET (1)  IS NUMERIC
00995          MOVE LR-ITD-NET (1)     TO  CITNETO
00996      ELSE
00997          MOVE ZEROS              TO  CITNETO.
00998
00999      IF LR-ITD-EARN (1)  IS NUMERIC
01000          MOVE LR-ITD-EARN (1)    TO  CITEARNO
01001      ELSE
01002          MOVE ZEROS              TO  CITEARNO.
01003
01004      IF LR-ITD-PAID (1)  IS NUMERIC
01005          MOVE LR-ITD-PAID (1)    TO  CITPAIDO
01006      ELSE
01007          MOVE ZEROS              TO  CITPAIDO.
01008
01009      IF LR-ITD-RESV (1)  IS NUMERIC
01010          MOVE LR-ITD-RESV (1)    TO  CITRESO
01011      ELSE
01012          MOVE ZEROS              TO  CITRESO.
01013
01014      IF LR-ITD-INCUR (1)  IS NUMERIC
01015          MOVE LR-ITD-INCUR (1)   TO  CITINCO
01016      ELSE
01017          MOVE ZEROS              TO  CITINCO.
01018
01019      IF LR-ITD-RATIO (1)  IS NUMERIC
01020          MOVE LR-ITD-RATIO (1)   TO  CITRATOO
01021      ELSE
01022          MOVE ZEROS              TO  CITRATOO.
01023
01024      IF LR-YTD-NET (2)  IS NUMERIC
01025          MOVE LR-YTD-NET (2)     TO  CYLNETO
01026      ELSE
01027          MOVE ZEROS              TO  CYLNETO.
01028
01029      IF LR-YTD-EARN (2)  IS NUMERIC
01030          MOVE LR-YTD-EARN (2)    TO  CYLEARNO
01031      ELSE
01032          MOVE ZEROS              TO  CYLEARNO.
01033
01034      IF LR-YTD-PAID (2)  IS NUMERIC
01035          MOVE LR-YTD-PAID (2)    TO  CYLPAIDO
01036      ELSE
01037          MOVE ZEROS              TO  CYLPAIDO.
01038
01039      IF LR-YTD-RESV (2)  IS NUMERIC
01040          MOVE LR-YTD-RESV (2)    TO  CYLRESO
01041      ELSE
01042          MOVE ZEROS              TO  CYLRESO.
01043
01044      IF LR-YTD-INCUR (2)  IS NUMERIC
01045          MOVE LR-YTD-INCUR (2)   TO  CYLINCO
01046      ELSE
01047          MOVE ZEROS              TO  CYLINCO.
01048
01049      IF LR-YTD-RATIO (2)  IS NUMERIC
01050          MOVE LR-YTD-RATIO (2)   TO  CYLRATOO
01051      ELSE
01052          MOVE ZEROS              TO  CYLRATOO.
01053
01054      IF LR-ITD-NET (2)  IS NUMERIC
01055          MOVE LR-ITD-NET (2)     TO  CILNETO
01056      ELSE
01057          MOVE ZEROS              TO  CILNETO.
01058
01059      IF LR-ITD-EARN (2)  IS NUMERIC
01060          MOVE LR-ITD-EARN (2)    TO  CILEARNO
01061      ELSE
01062          MOVE ZEROS              TO  CILEARNO.
01063
01064      IF LR-ITD-PAID (2)  IS NUMERIC
01065          MOVE LR-ITD-PAID (2)    TO  CILPAIDO
01066      ELSE
01067          MOVE ZEROS              TO  CILPAIDO.
01068
01069      IF LR-ITD-RESV (2)  IS NUMERIC
01070          MOVE LR-ITD-RESV (2)    TO  CILRESO
01071      ELSE
01072          MOVE ZEROS              TO  CILRESO.
01073
01074      IF LR-ITD-INCUR (2)  IS NUMERIC
01075          MOVE LR-ITD-INCUR (2)   TO  CILINCO
01076      ELSE
01077          MOVE ZEROS              TO  CILINCO.
01078
01079      IF LR-ITD-RATIO (2)  IS NUMERIC
01080          MOVE LR-ITD-RATIO (2)   TO  CILRATOO
01081      ELSE
01082          MOVE ZEROS              TO  CILRATOO.
01083
01084      IF LR-YTD-NET (3)  IS NUMERIC
01085          MOVE LR-YTD-NET (3)     TO  CYANETO
01086      ELSE
01087          MOVE ZEROS              TO  CYANETO.
01088
01089      IF LR-YTD-EARN (3)  IS NUMERIC
01090          MOVE LR-YTD-EARN (3)    TO  CYAEARNO
01091      ELSE
01092          MOVE ZEROS              TO  CYAEARNO.
01093
01094      IF LR-YTD-PAID (3)  IS NUMERIC
01095          MOVE LR-YTD-PAID (3)    TO  CYAPAIDO
01096      ELSE
01097          MOVE ZEROS              TO  CYAPAIDO.
01098
01099      IF LR-YTD-RESV (3)  IS NUMERIC
01100          MOVE LR-YTD-RESV (3)    TO  CYARESO
01101      ELSE
01102          MOVE ZEROS              TO  CYARESO.
01103
01104      IF LR-YTD-INCUR (3)  IS NUMERIC
01105          MOVE LR-YTD-INCUR (3)   TO  CYAINCO
01106      ELSE
01107          MOVE ZEROS              TO  CYAINCO.
01108
01109      IF LR-YTD-RATIO (3)  IS NUMERIC
01110          MOVE LR-YTD-RATIO (3)   TO  CYARATOO
01111      ELSE
01112          MOVE ZEROS              TO  CYARATOO.
01113
01114      IF LR-ITD-NET (3)  IS NUMERIC
01115          MOVE LR-ITD-NET (3)     TO  CIANETO
01116      ELSE
01117          MOVE ZEROS              TO  CIANETO.
01118
01119      IF LR-ITD-EARN (3)  IS NUMERIC
01120          MOVE LR-ITD-EARN (3)    TO  CIAEARNO
01121      ELSE
01122          MOVE ZEROS              TO  CIAEARNO.
01123
01124      IF LR-ITD-PAID (3)  IS NUMERIC
01125          MOVE LR-ITD-PAID (3)    TO  CIAPAIDO
01126      ELSE
01127          MOVE ZEROS              TO  CIAPAIDO.
01128
01129      IF LR-ITD-RESV (3)  IS NUMERIC
01130          MOVE LR-ITD-RESV (3)    TO  CIARESO
01131      ELSE
01132          MOVE ZEROS              TO  CIARESO.
01133
01134      IF LR-ITD-INCUR (3)  IS NUMERIC
01135          MOVE LR-ITD-INCUR (3)   TO  CIAINCO
01136      ELSE
01137          MOVE ZEROS              TO  CIAINCO.
01138
01139      IF LR-ITD-RATIO (3)  IS NUMERIC
01140          MOVE LR-ITD-RATIO (3)   TO  CIARATOO
01141      ELSE
01142          MOVE ZEROS              TO  CIARATOO.
01143
01144      MOVE LR-EXP-DATE (1)        TO  WS-LR-RUN-DATE.
01145      MOVE WS-LR-RUN-YR           TO  WS-LR-RUN-YR-EDIT.
01146      MOVE WS-LR-RUN-MO           TO  WS-LR-RUN-MO-EDIT.
01147      MOVE WS-LR-RUN-DA           TO  WS-LR-RUN-DA-EDIT.
01148      MOVE WS-LR-RUN-DATE-EDIT    TO  CCEXPDTO.
01149      MOVE LR-REI-TAB (1)         TO  CCREINO.
01150      MOVE LR-RETRO (1)           TO  CCRETROO.
01151      MOVE LR-BASIS (1)           TO  CCBASISO.
01152
01153      IF LR-AGT-NO (1 1) NOT EQUAL ZEROS AND SPACES
01154          NEXT SENTENCE
01155      ELSE
01156          GO TO 1140-NEXT-AGENT.
01157
01158      MOVE LR-AGT-NO (1 1)        TO  CCAGT1O.
01159
01160      IF LR-SNG-PCT (1 1)  IS NUMERIC
01161          COMPUTE WORK-COMM = LR-SNG-PCT (1 1) * +100
01162          MOVE WORK-COMM          TO  CCSNG1O
01163      ELSE
01164          MOVE LR-SNG-PCT-X (1 1) TO  WK-COM-TBL
01165          MOVE WORK-TABLE         TO  CCSNG1O.
01166
01167      IF LR-JNT-PCT (1 1)  IS NUMERIC
01168          COMPUTE WORK-COMM = LR-JNT-PCT (1 1) * +100
01169          MOVE WORK-COMM          TO  CCJNT1O
01170      ELSE
01171          MOVE LR-JNT-PCT-X (1 1) TO  WK-COM-TBL
01172          MOVE WORK-TABLE         TO  CCJNT1O.
01173
01174      IF LR-A-H-PCT (1 1)  IS NUMERIC
01175          COMPUTE WORK-COMM = LR-A-H-PCT (1 1) * +100
01176          MOVE WORK-COMM          TO  CCAH1O
01177      ELSE
01178          MOVE LR-A-H-PCT-X (1 1) TO  WK-COM-TBL
01179          MOVE WORK-TABLE         TO  CCAH1O.
01180
01181  1140-NEXT-AGENT.
01182
01183      IF LR-AGT-NO (1 2) NOT EQUAL ZEROS AND SPACES
01184          NEXT SENTENCE
01185      ELSE
01186          GO TO 1145-NEXT-AGENT.
01187
01188      MOVE LR-AGT-NO (1 2)        TO  CCAGT2O.
01189
01190      IF LR-SNG-PCT (1 2)  IS NUMERIC
01191          COMPUTE WORK-COMM = LR-SNG-PCT (1 2) * +100
01192          MOVE WORK-COMM          TO  CCSNG2O
01193      ELSE
01194          MOVE LR-SNG-PCT-X (1 2) TO  WK-COM-TBL
01195          MOVE WORK-TABLE         TO  CCSNG2O.
01196
01197      IF LR-JNT-PCT (1 2)  IS NUMERIC
01198          COMPUTE WORK-COMM = LR-JNT-PCT (1 2) * +100
01199          MOVE WORK-COMM          TO  CCJNT2O
01200      ELSE
01201          MOVE LR-JNT-PCT-X (1 2) TO  WK-COM-TBL
01202          MOVE WORK-TABLE         TO  CCJNT2O.
01203
01204      IF LR-A-H-PCT (1 2)  IS NUMERIC
01205          COMPUTE WORK-COMM = LR-A-H-PCT (1 2) * +100
01206          MOVE WORK-COMM          TO  CCAH2O
01207      ELSE
01208          MOVE LR-A-H-PCT-X (1 2) TO  WK-COM-TBL
01209          MOVE WORK-TABLE         TO  CCAH2O.
01210
01211  1145-NEXT-AGENT.
01212
01213      IF LR-AGT-NO (1 3) NOT EQUAL ZEROS AND SPACES
01214          NEXT SENTENCE
01215      ELSE
01216          GO TO 1150-PREVIOUS.
01217
01218      MOVE LR-AGT-NO (1 3)        TO  CCAGT3O.
01219
01220      IF LR-SNG-PCT (1 3)  IS NUMERIC
01221          COMPUTE WORK-COMM = LR-SNG-PCT (1 3) * +100
01222          MOVE WORK-COMM          TO  CCSNG3O
01223      ELSE
01224          MOVE LR-SNG-PCT-X (1 3) TO  WK-COM-TBL
01225          MOVE WORK-TABLE         TO  CCSNG3O.
01226
01227      IF LR-JNT-PCT (1 3)  IS NUMERIC
01228          COMPUTE WORK-COMM = LR-JNT-PCT (1 3) * +100
01229          MOVE WORK-COMM          TO  CCJNT3O
01230      ELSE
01231          MOVE LR-JNT-PCT-X (1 3) TO  WK-COM-TBL
01232          MOVE WORK-TABLE         TO  CCJNT3O.
01233
01234      IF LR-A-H-PCT (1 3)  IS NUMERIC
01235          COMPUTE WORK-COMM = LR-A-H-PCT (1 3) * +100
01236          MOVE WORK-COMM          TO  CCAH3O
01237      ELSE
01238          MOVE LR-A-H-PCT-X (1 3) TO  WK-COM-TBL
01239          MOVE WORK-TABLE         TO  CCAH3O.
01240
01241  1150-PREVIOUS.
01242
01243      IF LR-EXP-DATE (2)  IS EQUAL TO  SPACES
01244        OR LR-EXP-DATE (2)  IS EQUAL TO  SPACES
01245          GO TO 1165-CONTINUE.
01246
01247      MOVE LR-EXP-DATE (2)        TO  WS-LR-RUN-DATE.
01248      MOVE WS-LR-RUN-YR           TO  WS-LR-RUN-YR-EDIT.
01249      MOVE WS-LR-RUN-MO           TO  WS-LR-RUN-MO-EDIT.
01250      MOVE WS-LR-RUN-DA           TO  WS-LR-RUN-DA-EDIT.
01251      MOVE WS-LR-RUN-DATE-EDIT    TO  CPEXPDTO.
01252      MOVE LR-REI-TAB (2)         TO  CPREINO.
01253      MOVE LR-RETRO (2)           TO  CPRETROO.
01254      MOVE LR-BASIS (2)           TO  CPBASISO.
01255
01256      IF LR-AGT-NO (2 1) NOT EQUAL ZEROS AND SPACES
01257          NEXT SENTENCE
01258      ELSE
01259          GO TO 1155-NEXT-AGENT.
01260
01261      MOVE LR-AGT-NO (2 1)        TO  CPAGT1O.
01262
01263      IF LR-SNG-PCT (2 1)  IS NUMERIC
01264          COMPUTE WORK-COMM = LR-SNG-PCT (2 1) * +100
01265          MOVE WORK-COMM          TO  CPSNG1O
01266      ELSE
01267          MOVE LR-SNG-PCT-X (2 1) TO  WK-COM-TBL
01268          MOVE WORK-TABLE         TO  CPSNG1O.
01269
01270      IF LR-JNT-PCT (2 1)  IS NUMERIC
01271          COMPUTE WORK-COMM = LR-JNT-PCT (2 1) * +100
01272          MOVE WORK-COMM          TO  CPJNT1O
01273      ELSE
01274          MOVE LR-JNT-PCT-X (2 1) TO  WK-COM-TBL
01275          MOVE WORK-TABLE         TO  CPJNT1O.
01276
01277      IF LR-A-H-PCT (2 1)  IS NUMERIC
01278          COMPUTE WORK-COMM = LR-A-H-PCT (2 1) * +100
01279          MOVE WORK-COMM          TO  CPAH1O
01280      ELSE
01281          MOVE LR-A-H-PCT-X (2 1) TO  WK-COM-TBL
01282          MOVE WORK-TABLE         TO  CPAH1O.
01283
01284  1155-NEXT-AGENT.
01285
01286      IF LR-AGT-NO (2 2) NOT EQUAL ZEROS AND SPACES
01287          NEXT SENTENCE
01288      ELSE
01289          GO TO 1160-NEXT-AGENT.
01290
01291      MOVE LR-AGT-NO (2 2)        TO  CPAGT2O.
01292
01293      IF LR-SNG-PCT (2 2)  IS NUMERIC
01294          COMPUTE WORK-COMM = LR-SNG-PCT (2 2) * +100
01295          MOVE WORK-COMM          TO  CPSNG2O
01296      ELSE
01297          MOVE LR-SNG-PCT-X (2 2) TO  WK-COM-TBL
01298          MOVE WORK-TABLE         TO  CPSNG2O.
01299
01300      IF LR-JNT-PCT (2 2)  IS NUMERIC
01301          COMPUTE WORK-COMM = LR-JNT-PCT (2 2) * +100
01302          MOVE WORK-COMM          TO  CPJNT2O
01303      ELSE
01304          MOVE LR-JNT-PCT-X (2 2) TO  WK-COM-TBL
01305          MOVE WORK-TABLE         TO  CPJNT2O.
01306
01307      IF LR-A-H-PCT (2 2)  IS NUMERIC
01308          COMPUTE WORK-COMM = LR-A-H-PCT (2 2) * +100
01309          MOVE WORK-COMM          TO  CPAH2O
01310      ELSE
01311          MOVE LR-A-H-PCT-X (2 2) TO  WK-COM-TBL
01312          MOVE WORK-TABLE         TO  CPAH2O.
01313
01314  1160-NEXT-AGENT.
01315
01316      IF LR-AGT-NO (2 3) NOT EQUAL ZEROS AND SPACES
01317          NEXT SENTENCE
01318      ELSE
01319          GO TO 1165-CONTINUE.
01320
01321      MOVE LR-AGT-NO (2 3)        TO  CPAGT3O.
01322
01323      IF LR-SNG-PCT (2 3)  IS NUMERIC
01324          COMPUTE WORK-COMM = LR-SNG-PCT (2 3) * +100
01325          MOVE WORK-COMM          TO  CPSNG3O
01326      ELSE
01327          MOVE LR-SNG-PCT-X (2 3) TO  WK-COM-TBL
01328          MOVE WORK-TABLE         TO  CPSNG3O.
01329
01330      IF LR-JNT-PCT (2 3)  IS NUMERIC
01331          COMPUTE WORK-COMM = LR-JNT-PCT (2 3) * +100
01332          MOVE WORK-COMM          TO  CPJNT3O
01333      ELSE
01334          MOVE LR-JNT-PCT-X (2 3) TO  WK-COM-TBL
01335          MOVE WORK-TABLE         TO  CPJNT3O.
01336
01337      IF LR-A-H-PCT (2 3)  IS NUMERIC
01338          COMPUTE WORK-COMM = LR-A-H-PCT (2 3) * +100
01339          MOVE WORK-COMM          TO  CPAH3O
01340      ELSE
01341          MOVE LR-A-H-PCT-X (2 3) TO  WK-COM-TBL
01342          MOVE WORK-TABLE         TO  CPAH3O.
01343
01344  1165-CONTINUE.
01345
01346      MOVE AL-UNNON               TO  CPFKA.
01347      MOVE -1                     TO  CPFKL.
01348
01349      IF PI-BROWSE-SW  IS EQUAL TO  +1
01350          PERFORM 5400-END-BROWSE  THRU  5499-EXIT.
01351
01352      PERFORM 8400-SEND-DATAONLY.
01353
01354      GO TO 9100-RETURN-TRAN.
01355
01356  1200-ENDFILE.
01357      MOVE ER-0130                TO  EMI-ERROR.
01358
01359      PERFORM 8300-SEND-DATAONLY.
01360
01361      GO TO 9100-RETURN-TRAN.
01362
01363  1499-EXIT.
01364      EXIT.
01365  EJECT
01366  5000-START-BROWSE.
01367      
      * EXEC CICS STARTBR
01368 *        DATASET    (PI-DSID)
01369 *        RIDFLD     (PI-LOSS-RATIO-KEY)
01370 *        GTEQ
01371 *        KEYLENGTH  (PI-KEY-LENGTH)
01372 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   K     G          &   #00003506' TO DFHEIV0
           MOVE X'262C2020204B202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 PI-LOSS-RATIO-KEY, 
                 PI-KEY-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01373
01374      MOVE +1                     TO  PI-BROWSE-SW.
01375
01376  5099-EXIT.
01377      EXIT.
01378  EJECT
01379  5100-READ-DIRECT.
01380      
      * EXEC CICS READ
01381 *        DATASET  (PI-DSID)
01382 *        RIDFLD   (PI-LOSS-RATIO-KEY)
01383 *        SET      (ADDRESS OF LOSS-RATIO-MASTER)
01384 *        GTEQ
01385 *    END-EXEC.
      *    MOVE '&"S        G          (   #00003519' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-LOSS-RATIO-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LOSS-RATIO-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01386
01387  5199-EXIT.
01388      EXIT.
01389  EJECT
01390  5200-READ-NEXT.
01391      
      * EXEC CICS READNEXT
01392 *        DATASET  (PI-DSID)
01393 *        RIDFLD   (PI-LOSS-RATIO-KEY)
01394 *        SET      (ADDRESS OF LOSS-RATIO-MASTER)
01395 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00003530' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-LOSS-RATIO-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LOSS-RATIO-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01396
01397  5299-EXIT.
01398      EXIT.
01399  EJECT
01400  5300-READ-PREVIOUS.
01401      
      * EXEC CICS READPREV
01402 *        DATASET  (PI-DSID)
01403 *        RIDFLD   (PI-LOSS-RATIO-KEY)
01404 *        SET      (ADDRESS OF LOSS-RATIO-MASTER)
01405 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00003540' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-LOSS-RATIO-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LOSS-RATIO-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01406
01407  5399-EXIT.
01408      EXIT.
01409  EJECT
01410  5400-END-BROWSE.
01411      
      * EXEC CICS ENDBR
01412 *        DATASET  (PI-DSID)
01413 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003550' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01414
01415  5499-EXIT.
01416      EXIT.
01417  EJECT
01418  8100-SEND-INITIAL-MAP  SECTION.
01419      MOVE SAVE-DATE              TO  BDATEO.
01420      MOVE EIBTIME                TO  TIME-IN.
01421      MOVE TIME-OUT               TO  BTIMEO.
01422      MOVE -1                     TO  BPFKL.
01423
01424      IF EMI-ERROR  IS NOT EQUAL TO  ZERO
01425          PERFORM 9700-ERROR-FORMAT.
01426
01427      MOVE EMI-MESSAGE-AREA (1)   TO  BEMSG1O.
01428
01429      
      * EXEC CICS SEND
01430 *        FROM    (EL645BO)
01431 *        MAPSET  (WS-MAPSET-NAME)
01432 *        MAP     (WS-MAP-B-NAME)
01433 *        CURSOR
01434 *        ERASE
01435 *    END-EXEC.
           MOVE LENGTH OF
            EL645BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00003568' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-B-NAME, 
                 EL645BO, 
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
           
01436
01437  8199-EXIT.
01438      EXIT.
01439  EJECT
01440  8200-SEND-INITIAL-MAP  SECTION.
01441      MOVE SAVE-DATE              TO  CDATEO.
01442      MOVE EIBTIME                TO  TIME-IN.
01443      MOVE TIME-OUT               TO  CTIMEO.
01444      MOVE -1                     TO  CPFKL.
01445
01446      IF EMI-ERROR  IS NOT EQUAL TO  ZERO
01447          PERFORM 9700-ERROR-FORMAT.
01448
01449      MOVE EMI-MESSAGE-AREA (1)   TO  CEMSG1O.
01450
01451      
      * EXEC CICS SEND
01452 *        FROM    (EL645CO)
01453 *        MAPSET  (WS-MAPSET-NAME)
01454 *        MAP     (WS-MAP-C-NAME)
01455 *        CURSOR
01456 *        ERASE
01457 *    END-EXEC.
           MOVE LENGTH OF
            EL645CO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00003590' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-C-NAME, 
                 EL645CO, 
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
           
01458
01459  8299-EXIT.
01460      EXIT.
01461  EJECT
01462  8300-SEND-DATAONLY  SECTION.
01463      MOVE SAVE-DATE              TO  BDATEO.
01464      MOVE EIBTIME                TO  TIME-IN.
01465      MOVE TIME-OUT               TO  BTIMEO.
01466
01467      IF EMI-ERROR  IS NOT EQUAL TO  ZERO
01468          PERFORM 9700-ERROR-FORMAT.
01469
01470      MOVE EMI-MESSAGE-AREA (1)   TO  BEMSG1O.
01471
01472      
      * EXEC CICS SEND
01473 *        FROM    (EL645BO)
01474 *        MAPSET  (WS-MAPSET-NAME)
01475 *        MAP     (WS-MAP-B-NAME)
01476 *        CURSOR
01477 *        ERASE
01478 *    END-EXEC.
           MOVE LENGTH OF
            EL645BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00003611' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-B-NAME, 
                 EL645BO, 
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
           
01479
01480  8399-EXIT.
01481      EXIT.
01482  EJECT
01483  8400-SEND-DATAONLY  SECTION.
01484      MOVE SAVE-DATE              TO  CDATEO.
01485      MOVE EIBTIME                TO  TIME-IN.
01486      MOVE TIME-OUT               TO  CTIMEO.
01487
01488      IF EMI-ERROR  IS NOT EQUAL TO  ZERO
01489          PERFORM 9700-ERROR-FORMAT.
01490
01491      MOVE EMI-MESSAGE-AREA (1)   TO  CEMSG1O.
01492
01493      
      * EXEC CICS SEND
01494 *        FROM    (EL645CO)
01495 *        MAPSET  (WS-MAPSET-NAME)
01496 *        MAP     (WS-MAP-C-NAME)
01497 *        CURSOR
01498 *        ERASE
01499 *    END-EXEC.
           MOVE LENGTH OF
            EL645CO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00003632' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-C-NAME, 
                 EL645CO, 
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
           
01500
01501  8499-EXIT.
01502      EXIT.
01503  EJECT
01504  8500-SEND-TEXT  SECTION.
01505      
      * EXEC CICS SEND TEXT
01506 *        FROM    (LOGOFF-TEXT)
01507 *        LENGTH  (LOGOFF-LENGTH)
01508 *        ERASE
01509 *        FREEKB
01510 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00003644' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363434' TO DFHEIV0(25:11)
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
           
01511
01512      
      * EXEC CICS RETURN
01513 *    END-EXEC.
      *    MOVE '.(                    &   #00003651' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01514
01515  8599-EXIT.
01516      EXIT.
01517
01518  8600-DATE-CONVERSION  SECTION.
01519      
      * EXEC CICS LINK
01520 *        PROGRAM   ('ELDATCV')
01521 *        COMMAREA  (DATE-CONVERSION-DATA)
01522 *        LENGTH    (DC-COMM-LENGTH)
01523 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00003658' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01524
01525  8699-EXIT.
01526      EXIT.
01527  EJECT
01528  8700-NOT-FOUND  SECTION.
01529      MOVE -1                     TO  BPFKL.
01530      MOVE ER-0673                TO  EMI-ERROR.
01531
01532      PERFORM 8100-SEND-INITIAL-MAP.
01533
01534      GO TO 9100-RETURN-TRAN.
01535
01536  8799-EXIT.
01537      EXIT.
01538  EJECT
01539  9000-RETURN-CICS  SECTION.
01540      MOVE 'EL005'                TO  THIS-PGM.
01541      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
01542
01543      GO TO 9300-XCTL.
01544
01545  9099-EXIT.
01546      EXIT.
01547
01548  9100-RETURN-TRAN  SECTION.
01549      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
01550      MOVE '645B'                 TO  PI-CURRENT-SCREEN-NO.
01551      MOVE EIBAID                 TO  PI-PREV-AID.
01552
01553      
      * EXEC CICS RETURN
01554 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
01555 *        LENGTH    (PI-COMM-LENGTH)
01556 *        TRANSID   (WS-TRANS-ID)
01557 *    END-EXEC.
      *    MOVE '.(CT                  &   #00003692' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01558
01559  9199-EXIT.
01560      EXIT.
01561
01562  9300-XCTL  SECTION.
01563      MOVE DFHENTER               TO  EIBAID.
01564
01565      
      * EXEC CICS XCTL
01566 *        PROGRAM   (THIS-PGM)
01567 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
01568 *        LENGTH    (PI-COMM-LENGTH)
01569 *    END-EXEC.
      *    MOVE '.$C                   $   #00003704' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01570
01571  9399-EXIT.
01572      EXIT.
01573  EJECT
01574  9400-CLEAR  SECTION.
01575      MOVE 'Y'                    TO  PI-RETURN-SWITCH.
01576      MOVE PI-RETURN-TO-PROGRAM   TO  THIS-PGM.
01577
01578      GO TO 9300-XCTL.
01579
01580  9499-EXIT.
01581      EXIT.
01582
01583  9600-PGMIDERR  SECTION.
01584      
      * EXEC CICS HANDLE CONDITION
01585 *        PGMIDERR  (8500-SEND-TEXT)
01586 *    END-EXEC.
      *    MOVE '"$L                   ! # #00003723' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033373233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01587
01588      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM
01589                                      LOGOFF-PGM.
01590      MOVE 'EL005'                TO  THIS-PGM.
01591      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
01592      MOVE SPACES                 TO  PI-ENTRY-CD-1.
01593
01594      GO TO 9300-XCTL.
01595
01596  9699-EXIT.
01597      EXIT.
01598  EJECT
01599  9700-ERROR-FORMAT  SECTION.
01600      IF EMI-ERRORS-COMPLETE
01601          GO TO 9799-EXIT.
01602
01603      
      * EXEC CICS LINK
01604 *        PROGRAM   ('EL001')
01605 *        COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
01606 *        LENGTH    (EMI-COMM-LENGTH)
01607 *    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   ''   #00003742' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01608
01609  9799-EXIT.
01610      EXIT.
01611
01612  9800-ERROR  SECTION.
01613      MOVE DFHEIBLK               TO  EMI-LINE1.
01614
01615      
      * EXEC CICS LINK
01616 *        PROGRAM  ('EL004')
01617 *        COMMAREA (EMI-LINE1)
01618 *        LENGTH   (72)
01619 *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00003754' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01620
01621      MOVE -1                     TO  BPFKL.
01622
01623      PERFORM 8100-SEND-INITIAL-MAP.
01624
01625      GO TO 9100-RETURN-TRAN.
01626
01627  9899-EXIT.
01628      EXIT.
01629
01630  9999-LAST-PARAGRAPH  SECTION.
01631      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6451' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6451' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMIDERR,
                     8700-NOT-FOUND,
                     1200-ENDFILE,
                     1010-DUPKEY,
                     9800-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 8500-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6451' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
