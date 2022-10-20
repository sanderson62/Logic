00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL688 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 03/07/95 12:54:34.
00007 *                            VMOD=2.022.
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
00024 *REMARKS.  TRANSACTION - EXG4
00025
00026 *        THIS FUNCTION READS THE CHECK QUEUE FILE AND PRINTS
00027 *    CHECKS FROM THOSE ENTRIES QUEUED.  QUEUEING IS DONE USING
00028 *    TIME (HH.MM) BY THE CHECK PRINT RELEASE PROGRAM (EL687).
00029
00030 *    SCREENS     - NONE - USERS PRINTED OUTPUT (CHECKS)
00031
00032 *    ENTERED BY  - EL687  - CHECK WRITER - VIA START
00033
00034 *    EXIT TO     - CICS
00035
00036 *    INPUT FILES - NONE
00037
00038 *    OUTPUT FILES - NONE
00039
00040 *    COMMAREA    - PASSED.
00041
00042      EJECT
00043  ENVIRONMENT DIVISION.
00044
00045  DATA DIVISION.
00046
00047  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00048
00049  77  FILLER  PIC X(32)  VALUE '********************************'.
00050  77  FILLER  PIC X(32)  VALUE '*    EL688 WORKING STORAGE     *'.
00051  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.022 *********'.
00052
00053  01  FILLER                          COMP-3.
00054      05  WS-RECORD-COUNT             PIC S9(5)       VALUE ZERO.
00055      05  WS-TIME-WORK                PIC S9(7)       VALUE ZERO.
00056      05  WS-TIME                     REDEFINES
00057          WS-TIME-WORK                PIC S9(3)V9(4).
00058      05  WS-HHMM                     REDEFINES
00059          WS-TIME-WORK                PIC S9(5)V99.
00060
00061      05  WS-DELAY-INTERVAL           PIC S9(7)       VALUE +10.
00062
00063      05  WS-DATA-SENT-SW             PIC S9          VALUE ZERO.
00064      05  WS-SW                       PIC S9          VALUE ZERO.
00065
00066      05  WS-AMT-WORK                 PIC S9(7)V99    VALUE ZERO.
00067  01  FILLER                          COMP
00068                                      SYNCHRONIZED.
00069
00070      05  WS-TS-LENGTH                PIC S9(4)       VALUE +1000.
00071      05  WS-COMM-LENGTH              PIC S9(4)       VALUE +1024.
00072      05  WS-CHECK-LINES-LENGTH       PIC S9(4)       VALUE +904.
00073
00074  01  FILLER.
00075      05  THIS-PGM                    PIC X(8)      VALUE 'EL688'.
00076
00077      05  ERMAIL-FILE-ID           PIC X(8)      VALUE 'ERMAIL'.
00078
00079      05  ERMAIL-KEY.
00080          10  ERMAIL-COMPANY-CD    PIC X         VALUE SPACES.
00081          10  ERMAIL-CARRIER       PIC X         VALUE SPACES.
00082          10  ERMAIL-GROUPING      PIC X(6)      VALUE SPACES.
00083          10  ERMAIL-STATE         PIC XX        VALUE SPACES.
00084          10  ERMAIL-ACCOUNT       PIC X(10)     VALUE SPACES.
00085          10  ERMAIL-CERT-EFF-DT   PIC XX        VALUE SPACES.
00086          10  ERMAIL-CERT-NO       PIC X(11)     VALUE SPACES.
00087
00088      05  FIRST-TIME-SW            PIC X              VALUE 'Y'.
00089          88  FIRST-ALIGNMENT                         VALUE 'Y'.
00090
00091      05  FIRST-CLIENT-SW          PIC X              VALUE 'Y'.
00092          88  FIRST-HERITAGE-CHECK                    VALUE 'Y'.
00093          88  FIRST-MADISON-CHECK                     VALUE 'Y'.
00094          88  FIRST-LAP-CHECK                         VALUE 'Y'.
00095
00096      05  WS-TEXT-MESSAGE-LENGTH      PIC S9(4)       VALUE +70
00097                                      COMP
00098                                      SYNCHRONIZED.
00099
00100      05  WS-TEXT-MESSAGE             PIC X(70)       VALUE SPACES.
00101
00102      05  WS-ZIP-CODE-LINE                            VALUE SPACES.
00103          10  WS-ZIP-CHAR             PIC X
00104              OCCURS 133 TIMES        INDEXED BY ZIP-INDEX1
00105                                                 ZIP-INDEX2
00106                                                 ZIP-INDEX3.
00107
00108      05  WS-DUMP-CODE.
00109          10  FILLER                  PIC X           VALUE 'S'.
00110          10  WS-DUMP-COUNT           PIC 999         VALUE ZERO.
00111
00112      05  WS-STUB4.
00113          10  WS-STUB4-1-20           PIC X(20)       VALUE SPACES.
00114          10  WS-STUB4-21-50          PIC X(30)       VALUE SPACES.
00115
00116      05  WS-CHECK-DATE.
00117          10  WS-CD-MO            PIC  9(02).
00118          10  WS-CD-DA            PIC  9(02).
00119          10  WS-CD-YR            PIC  9(02).
00120
00121      05  WS-ZIP-WORK.
00122          10  WS-ZIP-CODE         PIC  X(05).
00123          10  WS-ZIP-DASH         PIC  X(01).
00124          10  WS-ZIP-PLUS4        PIC  X(04).
00125
00126      05  WS-MONTH-TABLE.
00127          10  WS-MONTH-01         PIC  X(09)  VALUE '  JANUARY'.
00128          10  WS-MONTH-02         PIC  X(09)  VALUE ' FEBRUARY'.
00129          10  WS-MONTH-03         PIC  X(09)  VALUE '    MARCH'.
00130          10  WS-MONTH-04         PIC  X(09)  VALUE '    APRIL'.
00131          10  WS-MONTH-05         PIC  X(09)  VALUE '      MAY'.
00132          10  WS-MONTH-06         PIC  X(09)  VALUE '     JUNE'.
00133          10  WS-MONTH-07         PIC  X(09)  VALUE '     JULY'.
00134          10  WS-MONTH-08         PIC  X(09)  VALUE '   AUGUST'.
00135          10  WS-MONTH-09         PIC  X(09)  VALUE 'SEPTEMBER'.
00136          10  WS-MONTH-10         PIC  X(09)  VALUE '  OCTOBER'.
00137          10  WS-MONTH-11         PIC  X(09)  VALUE ' NOVEMBER'.
00138          10  WS-MONTH-12         PIC  X(09)  VALUE ' DECEMBER'.
00139      05  WS-MONT-TAB  REDEFINES  WS-MONTH-TABLE.
00140          10  WS-MONTH-ENTRY  OCCURS  12  TIMES
00141                                  PIC  X(09).
00142 *                            COPY ELCDMD34.
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
00143      EJECT
00144 *                            COPY ELCINTF.
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
00145
00146 *                            COPY ERC687PI.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ERC687PI                           *
00004 *                            VMOD=2.004                         *
00005 *                                                               *
00006 *****************************************************************.
00007
00008      12  FILLER                      REDEFINES
00009          PI-PROGRAM-WORK-AREA.
00010
00011          16  PI-TEMP-STORAGE-KEY.
00012              20  PI-TSK-TERM-ID      PIC X(4).
00013              20  PI-TSK-TIME         PIC S9(7)     COMP-3.
00014
00015          16  PI-PROCESSING-SW        PIC S9        COMP-3.
00016
00017          16  PI-NUMBER-OF-ALIGNMENT-CHECKS PIC S9  COMP-3.
00018
00019          16  PI-ALIGNMENT-CONTROL-GROUP  PIC S9(8) COMP.
00020
00021          16  PI-ALIGNMENT-SEQUENCE-NO  PIC S9(4)   COMP.
00022
00023          16  PI-NUMBER-OF-CONTROL-GROUPS  PIC S9(4) COMP.
00024
00025          16  PI-CONTROL-GROUPS       COMP
00026              OCCURS 4 TIMES          INDEXED BY PI-INDEX.
00027
00028              20  PI-CONTROL-GROUP    PIC S9(8).
00029              20  PI-HIGH-SEQUENCE    PIC S9(4).
00030
00031          16  PI-CHECK-PRINTER-ID     PIC X(4).
00032
00033          16  PI-PRINTER-STARTED-SW   PIC S9      COMP-3.
00034
00035          16  PI-ASSIGN-CHECK-NUMBERS PIC X.
00036
00037          16  PI-COMPANY-ADDRESS.
00038              20  PI-COMPANY-NAME             PIC X(30).
00039              20  PI-COMPANY-ADDRESS-LINE1    PIC X(30).
00040              20  PI-COMPANY-ADDRESS-LINE2    PIC X(30).
00041              20  PI-COMPANY-ADDRESS-LINE3    PIC X(30).
00042              20  PI-COMPANY-CITY-ST          PIC X(30).
00043              20  PI-COMPANY-ZIP-CODE.
00044                  24  PI-COMPANY-ZIP-PRIME.
00045                      26  PI-ZIP-PRI-1ST      PIC X.
00046                          88  PI-CO-CANADIAN-POST-CODE
00047                                           VALUE 'A' THRU 'Z'.
00048                      26  FILLER              PIC X(4).
00049                  24  PI-COMPANY-ZIP-PLUS4    PIC X(4).
00050              20  PI-CANADIAN-POSTAL-CODE REDEFINES
00051                  PI-COMPANY-ZIP-CODE.
00052                  24  PI-CAN-POSTAL-1         PIC XXX.
00053                  24  PI-CAN-POSTAL-2         PIC XXX.
00054                  24  FILLER                  PIC XXX.
00055              20  PI-COMPANY-PHONE-NUMBER     PIC S9(11) COMP-3.
00056          16  PI-SYSID                PIC X(4).
00057
00058          16  PI-TEMP-STORAGE-ITEM    PIC S9(4) COMP SYNC.
00059          16  PI-LETTERS-IND          PIC X.
00060
00061          16  FILLER                  PIC X(419).
00062
00147
00148      EJECT
00149 *                            COPY ELC176W2.
00001 *****************************************************************
00002 *                                                               *
00003 *                            ELC176W2.                          *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                         *
00006 *****************************************************************.
00007
00008  01  SPELL-DOLLAR-PASS-AREA.
00009
00010      05  SD-PASS-AMOUNT              PIC S9(9)V99
00011                                      COMP-3        VALUE ZEROS.
00012
00013      05  SD-PASS-SPELLED-AMOUNT      PIC X(200)    VALUE SPACES.
00014
00015      05  FILLER.
00016          10  SD-PSA-LINE1            PIC X(100)    VALUE SPACES.
00017          10  SD-PSA-LINE2            PIC X(100)    VALUE SPACES.
00018
00150
00151      EJECT
00152 *                            COPY ERC688W1.
      *****************************************************************
      *                                                               *
00003 *                            ERC688W1                           *
00004 *                            VMOD=2.002                         *
      *                                                               *
      *                                                               *
      *****************************************************************.
000100 01  FILLER.
000300   05 WS-1ST-LINE-LENGTH         PIC S9(4) COMP SYNC VALUE +48.
000700   05 WS-1ST-LINE-LENGTH-PLUS-1  PIC S9(4) COMP SYNC VALUE +49.
001100   05 WS-1ST-LINE-LENGTH-PLUS-2  PIC S9(4) COMP SYNC VALUE +50.
001500   05 WS-1ST-LINE-LENGTH-MINUS-1 PIC S9(4) COMP SYNC VALUE +47.
001900   05 WS-2ND-LINE-LENGTH         PIC S9(4) COMP SYNC VALUE +48.
002300   05 WS-AMOUNT                  PIC 9(9)V99.
002500   05 FILLER REDEFINES WS-AMOUNT.
002800         10  WS-MILLIONS         PIC 999.
002900         10  WS-THOUSANDS        PIC 999.
003000         10  WS-HUNDREDS         PIC 999.
003100
003200         10  WS-CENTS            PIC 99.
003300         10  WS-CENTS-X REDEFINES WS-CENTS
003400                                 PIC XX.
003500
003600   05 WS-AMOUNT-WORK             PIC 999.
003800   05 FILLER REDEFINES WS-AMOUNT-WORK.
004100         10  WS-HUNDRED          PIC 9.
004200         10  WS-TEEN             PIC 99.
004400         10  FILLER REDEFINES WS-TEEN.
004700             15  WS-TEN          PIC 9.
004800             15  WS-ONE          PIC 9.
004900
005000   05 WS-SPELLED-AMOUNT          PIC X(200)      VALUE SPACES.
005200   05 WS-CHAR REDEFINES WS-SPELLED-AMOUNT
005300                                 PIC X OCCURS 200
005400                                       INDEXED BY SA-INDEX
005500                                                  SA-INDEX2.
005600
005700   05 WS-SPELLED-LINE1           PIC X(100)      VALUE SPACES.
005900   05 WS-SL1 REDEFINES WS-SPELLED-LINE1
006000                                 PIC X OCCURS 100
006100                                       INDEXED BY SL1-INDEX.
006200
006300   05 WS-SPELLED-LINE2           PIC X(100)      VALUE SPACES.
006500   05 WS-SL2 REDEFINES WS-SPELLED-LINE2
006600                                 PIC X OCCURS 100
006700                                       INDEXED BY SL2-INDEX.
006800
006900   05 WS-WORD                    PIC X(21)       VALUE SPACES.
007100   05 WS-CHAR2 REDEFINES WS-WORD
007200                                 PIC X OCCURS 21
007300                                       INDEXED BY CHAR-INDEX.
007600   05 WS-SINGLE-AREA.
007700         10  FILLER    PIC X(21)      VALUE 'ONE'.
007800         10  FILLER    PIC X(21)      VALUE 'TWO'.
007900         10  FILLER    PIC X(21)      VALUE 'THREE'.
008000         10  FILLER    PIC X(21)      VALUE 'FOUR'.
008100         10  FILLER    PIC X(21)      VALUE 'FIVE'.
008200         10  FILLER    PIC X(21)      VALUE 'SIX'.
008300         10  FILLER    PIC X(21)      VALUE 'SEVEN'.
008400         10  FILLER    PIC X(21)      VALUE 'EIGHT'.
008500         10  FILLER    PIC X(21)      VALUE 'NINE'.
008600         10  FILLER    PIC X(21)      VALUE 'TEN'.
008700         10  FILLER    PIC X(21)      VALUE 'ELEVEN'.
008800         10  FILLER    PIC X(21)      VALUE 'TWELVE'.
008900         10  FILLER    PIC X(21)      VALUE 'THIRTEEN'.
009000         10  FILLER    PIC X(21)      VALUE 'FOURTEEN'.
009100         10  FILLER    PIC X(21)      VALUE 'FIFTEEN'.
009200         10  FILLER    PIC X(21)      VALUE 'SIXTEEN'.
009300         10  FILLER    PIC X(21)      VALUE 'SEVENTEEN'.
009400         10  FILLER    PIC X(21)      VALUE 'EIGHTEEN'.
009500         10  FILLER    PIC X(21)      VALUE 'NINETEEN'.
009700   05 WS-SINGLE-DESC REDEFINES WS-SINGLE-AREA
009800                                     PIC X(21) OCCURS 19
009900                                     INDEXED BY SINGLE-INDEX.
010000
010200   05 WS-UPPER-AREA.
010300         10  FILLER    PIC X(21)      VALUE SPACES.
010400         10  FILLER    PIC X(21)      VALUE 'TWENTY'.
010500         10  FILLER    PIC X(21)      VALUE 'THIRTY'.
010600         10  FILLER    PIC X(21)      VALUE 'FORTY'.
010700         10  FILLER    PIC X(21)      VALUE 'FIFTY'.
010800         10  FILLER    PIC X(21)      VALUE 'SIXTY'.
010900         10  FILLER    PIC X(21)      VALUE 'SEVENTY'.
011000         10  FILLER    PIC X(21)      VALUE 'EIGHTY'.
011100         10  FILLER    PIC X(21)      VALUE 'NINETY'.
011300   05 WS-UPPER-DESC REDEFINES WS-UPPER-AREA
011400                                     PIC X(21) OCCURS 9
011500                                     INDEXED BY UPPER-INDEX.
011600
011700   05 WS-DOLLARS-AND-CENTS           PIC X(21)  VALUE
011800                                    'DOLLARS-AND-XX-CENTS'.
012000   05 FILLER REDEFINES WS-DOLLARS-AND-CENTS.
012300         10  FILLER                  PIC X(12).
012400         10  WS-PENNEYS              PIC XX.
012300         10  FILLER                  PIC X(07).
012600
00153
00154      EJECT
00155 *                            COPY ERC688CR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERC688CR                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION:  STANDARD FORMAT CHECK PRINT LINES              *
00007 *                                                                *
00008 ******************************************************************
00009
00010  01  CHECK-PRINT-LINES.
00011      12  CHECK-PRINT-LINE-1.
00012          16  FILLER                  PIC X     VALUE '1'.
00013
00014      12  CHECK-PRINT-LINE-3.
00015          16  FILLER                  PIC X     VALUE '0'.
00016
00017      12  CHECK-PRINT-LINE-5.
00018          16  FILLER                  PIC X(10) VALUE '0'.
00019          16  CPL5-COMPANY-NAME       PIC X(30) VALUE ALL 'X'.
00020
00021      12  CHECK-PRINT-LINE-6.
00022          16  FILLER                  PIC X(10) VALUE SPACES.
00023          16  CPL6-COMPANY-ADDRESS1   PIC X(30) VALUE ALL 'X'.
00024          16  FILLER                  PIC X(25) VALUE SPACES.
00025          16  CPL6-CHECK-NUMBER       PIC X(7)  VALUE ALL 'X'.
00026
00027      12  CHECK-PRINT-LINE-7.
00028          16  FILLER                  PIC X(10) VALUE SPACES.
00029          16  CPL7-COMPANY-ADDRESS2   PIC X(30) VALUE ALL 'X'.
00030          16  FILLER                  PIC X(5)  VALUE SPACES.
00031          16  CPL7-CHECK-DATE         PIC X(18) VALUE ALL 'X'.
00032
00033      12  CHECK-PRINT-LINE-8.
00034          16  FILLER                  PIC X(10) VALUE SPACES.
00035          16  CPL8-COMPANY-ADDRESS3   PIC X(30) VALUE ALL 'X'.
00036
00037      12  CHECK-PRINT-LINE-9.
00038          16  FILLER                  PIC X(10) VALUE SPACES.
00039          16  CPL9-COMPANY-CITY-ST    PIC X(30) VALUE ALL 'X'.
00040          16  FILLER                  PIC X     VALUE SPACES.
00041          16  CPL9-COMPANY-ZIP-CODE-X PIC X(9)  VALUE SPACES.
00042          16  CPL9-COMPANY-ZIP-CODE   REDEFINES
00043              CPL9-COMPANY-ZIP-CODE-X PIC Z(9).
00044
00045      12  CHECK-PRINT-LINE-11.
00046          16  FILLER                  PIC X     VALUE '0'.
00047          16  FILLER                  PIC X(9)  VALUE
00048                                                    '    PAY'.
00049          16  CPL11-PAY-LINE-1        PIC X(48) VALUE ALL 'X'.
00050          16  FILLER                  PIC X(7)  VALUE SPACES.
00051          16  CPL11-CHECK-AMOUNT      PIC $*,***,**9.99-.
00052
00053      12  CHECK-PRINT-LINE-12.
00054          16  FILLER                  PIC X(10) VALUE SPACES.
00055          16  CPL12-PAY-LINE-2        PIC X(48) VALUE ALL 'X'.
00056
00057      12  CHECK-PRINT-LINE-14.
00058          16  FILLER                  PIC X(10) VALUE '0'.
00059          16  CPL14-PAYEE-NAME        PIC X(30) VALUE ALL 'X'.
00060
00061      12  CHECK-PRINT-LINE-15.
00062          16  FILLER                  PIC X(10) VALUE SPACES.
00063          16  CPL15-PAYEE-ADDRESS1    PIC X(30) VALUE ALL 'X'.
00064          16  FILLER                  PIC X(10) VALUE SPACES.
00065
00066      12  CHECK-PRINT-LINE-16.
00067          16  FILLER                  PIC X(10) VALUE SPACES.
00068          16  CPL16-PAYEE-ADDRESS2    PIC X(30) VALUE ALL 'X'.
00069          16  FILLER                  PIC X(10) VALUE SPACES.
00070
00071      12  CHECK-PRINT-LINE-17.
00072          16  FILLER                  PIC X(10) VALUE SPACES.
00073          16  CPL17-PAYEE-ADDRESS3    PIC X(30) VALUE ALL 'X'.
00074          16  FILLER                  PIC X(10) VALUE SPACES.
00075
00076      12  CHECK-PRINT-LINE-18.
00077          16  FILLER                  PIC X(10) VALUE SPACES.
00078          16  CPL18-PAYEE-CITY-ST     PIC X(30) VALUE ALL 'X'.
00079          16  FILLER                  PIC X     VALUE SPACES.
00080          16  CPL18-PAYEE-ZIP-CODE-X  PIC X(9)  VALUE SPACES.
00081          16  CPL18-PAYEE-ZIP-CODE    REDEFINES
00082              CPL18-PAYEE-ZIP-CODE-X  PIC Z(9).
00083
00084      12  CHECK-PRINT-LINE-21.
00085          16  FILLER                  PIC X     VALUE '-'.
00086
00087      12  CHECK-PRINT-LINE-22.
00088          16  FILLER                  PIC X     VALUE SPACE.
00089
00090      12  CHECK-PRINT-LINE-23.
00091          16  FILLER                  PIC X(7)   VALUE
00092              '-   CO-'.
00093          16  CPL23-COMPANY-ID        PIC X(3)   VALUE ALL 'X'.
00094          16  FILLER                  PIC X(8)   VALUE
00095              '  GROUP-'.
00096          16  CPL23-GROUP             PIC X(3)   VALUE ALL 'X'.
00097          16  FILLER                  PIC X(10)  VALUE
00098              '  ACCOUNT-'.
00099          16  CPL23-ACCOUNT           PIC X(6)   VALUE ALL 'X'.
00100          16  CPL23-DESC              PIC X(11)  VALUE SPACES.
00101          16  CPL23-FIN-RESP          PIC X(8)   VALUE ALL 'X'.
00102          16  FILLER                  PIC X(10)  VALUE
00103              '  DEPT NO-'.
00104          16  CPL23-DEPT-NO           PIC X(4)   VALUE ALL 'X'.
00105          16  FILLER                  PIC X(12)  VALUE SPACES.
00106
00107      12  CHECK-PRINT-LINE-25.
00108          16  CPL25-CERT-NO-DESC      PIC X(12)  VALUE
00109              '    CERT NO-'.
00110          16  CPL25-CERT-NO           PIC X(8)   VALUE ALL 'X'.
00111          16  FILLER                  PIC X(11)  VALUE
00112              '  CHECK NO-'.
00113          16  CPL25-CHECK-NO          PIC X(7)   VALUE ALL 'X'.
00114          16  FILLER                  PIC X(14)  VALUE
00115              '  AMOUNT PAID-'.
00116          16  CPL25-NET-PAYOFF        PIC Z,ZZZ,ZZ9.99-.
00117          16  CPL25-MONTHLY-BENEFIT   REDEFINES
00118              CPL25-NET-PAYOFF        PIC Z,ZZZ,ZZ9.99-.
00119          16  CPL25-MONTHLY-BENEFIT-X REDEFINES
00120              CPL25-NET-PAYOFF        PIC X(13).
00121
00122      12  CHECK-PRINT-LINE-26.
00123          16  FILLER                  PIC X(4)  VALUE '0'.
00124          16  CPL26-COMMENTS          PIC X(40) VALUE ALL 'X'.
00125          16  FILLER                  PIC X(18) VALUE
00126              '   GENERAL LEDGER-'.
00127          16  CPL26-GEN-LEDGER        PIC X(7)  VALUE ALL 'X'.
00128
00129      12  CHECK-PRINT-LINE-27.
00130          16  FILLER                  PIC X(4)  VALUE '0'.
00131          16  CPL27-STUB              PIC X(50) VALUE ALL 'X'.
00132
00133      12  CHECK-PRINT-LINE-28.
00134          16  FILLER                  PIC X(4)  VALUE SPACES.
00135          16  CPL28-STUB              PIC X(50) VALUE ALL 'X'.
00136
00137      12  CHECK-PRINT-LINE-29.
00138          16  FILLER                  PIC X(4)  VALUE SPACES.
00139          16  CPL29-STUB              PIC X(50) VALUE ALL 'X'.
00140
00141      12  CHECK-PRINT-LINE-30.
00142          16  FILLER                  PIC X(4)  VALUE SPACES.
00143          16  CPL30-STUB              PIC X(50) VALUE ALL 'X'.
00144
00145      12  CHECK-PRINT-LINE-31.
00146          16  FILLER                  PIC X(4)  VALUE SPACES.
00147          16  CPL31-STUB              PIC X(50) VALUE ALL 'X'.
00148
00156
00157      EJECT
00186
00187      EJECT
00188  01  CHECK-PRINT-LINES-SAVE-AREA     PIC X(1500) VALUE SPACES.
00189
00190      EJECT
00191 *                            COPY ELCDATE.
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
00192
00193 *                            COPY ELPRTCVD.
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
00194
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
00196
00197  01  DFHCOMMAREA                    PIC X(1024).
00198
00199      EJECT
00200 *                            COPY ERCCPA.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCPA                              *
00004 *                            VMOD=2.005                          *
00005 *                                                                *
00006 *   DESCRIPTION:  DATA TO BE PASSED TO CHECK WRITER ROUTINE.     *
00007 *   LENGTH: 700 CHARACTERS                                       *
00008 *                                                                *
00009 ******************************************************************
00010
00011  01  CHECK-PASS-AREA.
00012      12  CPA-ALIGNMENT               PIC S9(3)     COMP-3.
00013      12  CPA-CARRIER                 PIC X.
00014      12  CPA-GROUPING                PIC X(6).
00015      12  CPA-ACCOUNT                 PIC X(10).
00016      12  CPA-FIN-RESP                PIC X(10).
00017      12  CPA-STATE                   PIC XX.
00018      12  CPA-CERT-NO                 PIC X(11).
00019
00020      12  CPA-CAR-NAME                PIC X(30).
00021      12  CPA-CAR-ADDRESS-LINE1       PIC X(30).
00022      12  CPA-CAR-ADDRESS-LINE2       PIC X(30).
00023      12  CPA-CAR-ADDRESS-LINE3       PIC X(30).
00024      12  CPA-CAR-CITY-STATE          PIC X(30).
00025      12  CPA-CAR-ZIP-CODE            PIC S9(9)     COMP-3.
00026      12  CPA-PAYMENT-TYPE            PIC X.
00027      12  CPA-PAYMENT-BY              PIC X(4).
00028      12  CPA-CHECK-DATE              PIC X(2).
00029      12  CPA-CHECK-NUMBER            PIC X(7).
00030      12  CPA-AMOUNT-PAID             PIC S9(9)V99  COMP-3.
00031      12  CPA-AMOUNT-PAID-TO-DATE     PIC S9(9)V99  COMP-3.
00032      12  CPA-INCURRED-DT             PIC XX.
00033      12  CPA-REPORTED-DT             PIC XX.
00034      12  CPA-PAID-THRU-DT            PIC XX.
00035      12  CPA-PAID-FROM-DT            PIC XX.
00036      12  CPA-PAID-DT                 PIC XX.
00037
00038      12  CPA-PAYEE-NAME              PIC X(30).
00039      12  CPA-PAYEE-IN-CARE-OF        PIC X(30).
00040      12  CPA-PAYEE-ADDRESS-LINE1     PIC X(30).
00041      12  CPA-PAYEE-ADDRESS-LINE2     PIC X(30).
00042      12  CPA-PAYEE-CITY-ST           PIC X(30).
00043      12  CPA-PAYEE-ZIP-CODE.
00044          16  CPA-PAYEE-ZIP-PRIME.
00045              20  CPA-PAYEE-PRI-1ST   PIC X.
00046                  88  CPA-PAYEE-CAN-POST-CODE   VALUE 'A' THRU 'Z'.
00047              20  FILLER              PIC X(4).
00048          16  CPA-PAYEE-ZIP-PLUS4     PIC X(4).
00049      12  CPA-PAYEE-CAN-POSTAL-CODE  REDEFINES  CPA-PAYEE-ZIP-CODE.
00050          16  CPA-PAYEE-POST-1        PIC XXX.
00051          16  CPA-PAYEE-POST-2        PIC XXX.
00052          16  FILLER                  PIC XXX.
00053      12  CPA-PAYEE-PHONE-NO          PIC S9(11)   COMP-3.
00054
00055      12  CPA-SOC-SEC-NO              PIC X(11).
00056      12  CPA-MEMBER-NUMBER           PIC X(12).
00057      12  CPA-NO-OF-PMTS-MADE         PIC S9(3)    COMP-3.
00058      12  CPA-GENERAL-LEDGER          PIC X(7).
00059      12  CPA-CANC-DT                 PIC XX.
00060      12  CPA-LF-REFUND               PIC S9(7)V99  COMP-3.
00061      12  CPA-AH-REFUND               PIC S9(7)V99  COMP-3.
00062      12  CPA-INSURED-NAME            PIC X(28).
00063      12  CPA-CERT-EFF-DT             PIC XX.
00064      12  CPA-DEDUCT-WITHHELD         PIC S9(5)V99  COMP-3.
00065      12  CPA-ADDITIONAL-CHARGE       PIC S9(5)V99  COMP-3.
00066      12  FILLER                      PIC X(26).
00067      12  CPA-REST-OF-RECORD          PIC X(250).
00068
00069      12  CPA-CHECK-STUBS             REDEFINES
00070          CPA-REST-OF-RECORD.
00071          16  CPA-STUB1               PIC X(50).
00072          16  CPA-STUB2               PIC X(50).
00073          16  CPA-STUB3               PIC X(50).
00074          16  CPA-STUB4               PIC X(50).
00075          16  CPA-STUB5               PIC X(50).
00076
00201
00202      EJECT
00203 *                            COPY ERCMAIL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCMAIL                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = MAILING DATA CAPTURE RECORDS              *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
CIDMOD*   RECORD SIZE = 374   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERMAIL                 RKP=2,LEN=33      *
00013 *   ALTERNATE PATH    = NOT USED                                 *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
080406******************************************************************
080406*                   C H A N G E   L O G
080406*
080406* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
080406*-----------------------------------------------------------------
080406*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
080406* EFFECTIVE    NUMBER
080406*-----------------------------------------------------------------
080406* 080406    2006051800002  PEMA  ADD POST CARD MAIL INFO
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
111108* 111108                   PEMA  ADD CRED BENE ADDR2
00017 ******************************************************************
00018
00019  01  MAILING-DATA.
00020      12  MA-RECORD-ID                      PIC XX.
00021          88  VALID-MA-ID                       VALUE 'MA'.
00022
00023      12  MA-CONTROL-PRIMARY.
00024          16  MA-COMPANY-CD                 PIC X.
00025          16  MA-CARRIER                    PIC X.
00026          16  MA-GROUPING.
00027              20  MA-GROUPING-PREFIX        PIC XXX.
00028              20  MA-GROUPING-PRIME         PIC XXX.
00029          16  MA-STATE                      PIC XX.
00030          16  MA-ACCOUNT.
00031              20  MA-ACCOUNT-PREFIX         PIC X(4).
00032              20  MA-ACCOUNT-PRIME          PIC X(6).
00033          16  MA-CERT-EFF-DT                PIC XX.
00034          16  MA-CERT-NO.
00035              20  MA-CERT-PRIME             PIC X(10).
00036              20  MA-CERT-SFX               PIC X.
00037
00038      12  FILLER                            PIC XX.
00039
00040      12  MA-ACCESS-CONTROL.
00041          16  MA-SOURCE-SYSTEM              PIC XX.
00042              88  MA-FROM-CREDIT                VALUE 'CR'.
00043              88  MA-FROM-VSI                   VALUE 'VS'.
00044              88  MA-FROM-WARRANTY              VALUE 'WA'.
00045              88  MA-FROM-OTHER                 VALUE 'OT'.
00046          16  MA-RECORD-ADD-DT              PIC XX.
00047          16  MA-RECORD-ADDED-BY            PIC XXXX.
00048          16  MA-LAST-MAINT-DT              PIC XX.
00049          16  MA-LAST-MAINT-BY              PIC XXXX.
00050          16  MA-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
00051
00052      12  MA-PROFILE-INFO.
00053          16  MA-QUALIFY-CODE-1             PIC XX.
00054          16  MA-QUALIFY-CODE-2             PIC XX.
00055          16  MA-QUALIFY-CODE-3             PIC XX.
00056          16  MA-QUALIFY-CODE-4             PIC XX.
00057          16  MA-QUALIFY-CODE-5             PIC XX.
00058
00059          16  MA-INSURED-LAST-NAME          PIC X(15).
00060          16  MA-INSURED-FIRST-NAME         PIC X(10).
00061          16  MA-INSURED-MIDDLE-INIT        PIC X.
00062          16  MA-INSURED-ISSUE-AGE          PIC 99.
00063          16  MA-INSURED-BIRTH-DT           PIC XX.
00064          16  MA-INSURED-SEX                PIC X.
00065              88  MA-SEX-MALE                   VALUE 'M'.
00066              88  MA-SEX-FEMALE                 VALUE 'F'.
00067          16  MA-INSURED-SOC-SEC-NO         PIC X(11).
00068
080406         16  MA-ADDRESS-CORRECTED          PIC X.
081108         16  MA-JOINT-BIRTH-DT             PIC XX.
00069 *        16  FILLER                        PIC X(12).
00070
00071          16  MA-ADDRESS-LINE-1             PIC X(30).
00072          16  MA-ADDRESS-LINE-2             PIC X(30).
00073          16  MA-CITY-STATE.
                   20  MA-CITY                   PIC X(28).
                   20  MA-ADDR-STATE             PIC XX.
00074          16  MA-ZIP.
00075              20  MA-ZIP-CODE.
00076                  24  MA-ZIP-CODE-1ST       PIC X(1).
00077                      88  MA-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00078                  24  FILLER                PIC X(4).
00079              20  MA-ZIP-PLUS4              PIC X(4).
00080          16  MA-CANADIAN-POSTAL-CODE REDEFINES MA-ZIP.
00081              20  MA-CAN-POSTAL-CODE-1      PIC X(3).
00082              20  MA-CAN-POSTAL-CODE-2      PIC X(3).
00083              20  FILLER                    PIC X(3).
00084
00085          16  MA-PHONE-NO                   PIC 9(11)       COMP-3.
00086
               16  FILLER                        PIC XXX.
00087 *        16  FILLER                        PIC X(10).
00088
           12  MA-CRED-BENE-INFO.
CIDMOD         16  MA-CRED-BENE-NAME                 PIC X(25).
CIDMOD         16  MA-CRED-BENE-ADDR                 PIC X(30).
               16  MA-CRED-BENE-ADDR2                PIC X(30).
CIDMOD         16  MA-CRED-BENE-CTYST.
                   20  MA-CRED-BENE-CITY             PIC X(28).
                   20  MA-CRED-BENE-STATE            PIC XX.
CIDMOD         16  MA-CRED-BENE-ZIP.
CIDMOD             20  MA-CB-ZIP-CODE.
CIDMOD                 24  MA-CB-ZIP-CODE-1ST        PIC X(1).
CIDMOD                     88  MA-CB-CANADIAN-POST-CODE
                                                 VALUE 'A' THRU 'Z'.
CIDMOD                 24  FILLER                    PIC X(4).
CIDMOD             20  MA-CB-ZIP-PLUS4               PIC X(4).
CIDMOD         16  MA-CB-CANADIAN-POSTAL-CODE
                                  REDEFINES MA-CRED-BENE-ZIP.
CIDMOD             20  MA-CB-CAN-POSTAL-CODE-1       PIC X(3).
CIDMOD             20  MA-CB-CAN-POSTAL-CODE-2       PIC X(3).
CIDMOD             20  FILLER                        PIC X(3).
080406     12  MA-POST-CARD-MAIL-DATA.
080406         16  MA-MAIL-DATA OCCURS 7.
080406             20  MA-MAIL-TYPE              PIC X.
080406                 88  MA-12MO-MAILING           VALUE '1'.
080406                 88  MA-EXP-MAILING            VALUE '2'.
080406             20  MA-MAIL-STATUS            PIC X.
080406                 88  MA-MAIL-ST-MAILED         VALUE '1'.
080406                 88  MA-MAIL-ST-RETURNED       VALUE '2'.
080406                 88  MA-MAIL-ST-NOT-MAILED     VALUE '3'.
080406             20  MA-MAIL-DATE              PIC XX.
080406     12  FILLER                            PIC XX.
           12  FILLER                            PIC XX.
080406*    12  FILLER                            PIC X(30).
00090 ******************************************************************
00204
00205      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CHECK-PASS-AREA
                                MAILING-DATA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL688' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00207
00208      
      * EXEC CICS HANDLE CONDITION
00209 *        QIDERR  (5000-MAIN-LOGIC)
00210 *        ITEMERR (5000-MAIN-LOGIC)
00211 *        ERROR   (9990-ERROR) END-EXEC.
      *    MOVE '"$N<.                 ! " #00001368' TO DFHEIV0
           MOVE X'22244E3C2E20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031333638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00212
00213      MOVE +132                   TO  WS-LINE-LEN.
00214      MOVE SPACES                 TO DL34-PROCESS-TYPE.
00215
00216      EJECT
00217  0010-MAIN-LOGIC.
00218      
      * EXEC CICS RETRIEVE
00219 *        INTO   (PROGRAM-INTERFACE-BLOCK)
00220 *        LENGTH (PI-COMM-LENGTH) END-EXEC
      *    MOVE '0*I L                 &   #00001378' TO DFHEIV0
           MOVE X'302A49204C20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00221
00222      MOVE +1                     TO  PI-TEMP-STORAGE-ITEM.
00223
00224 * DLO034 OPEN WHEN DMD OR CID
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
00226          IF DL34-PROCESS-TYPE IS EQUAL TO SPACES
00227              MOVE 'O'                TO DL34-PROCESS-TYPE
00228              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
00229              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
00230              MOVE PI-PROCESSOR-ID    TO DL34-USERID
00231              MOVE SPACES             TO DL34-PRINT-LINE
00232              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID
00233
00234              
      * EXEC CICS LINK
00235 *                PROGRAM    ('DLO034')
00236 *                COMMAREA   (DLO034-COMMUNICATION-AREA)
00237 *                LENGTH     (DLO034-REC-LENGTH)
00238 *            END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   ''   #00001394' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00239
00240              IF DL34-RETURN-CODE NOT = 'OK'
00241                  MOVE  '**DLO034 OPEN ERROR - ABORT**'
00242                                      TO WS-TEXT-MESSAGE
00243                  PERFORM 0120-SEND-TEXT
00244                  
      * EXEC CICS RETURN
00245 *                END-EXEC.
      *    MOVE '.(                    &   #00001404' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00246
00247  0100-MAIN-LOGIC.
00248      
      * EXEC CICS READQ TS
00249 *        QUEUE  (PI-TEMP-STORAGE-KEY)
00250 *        ITEM   (PI-TEMP-STORAGE-ITEM)
00251 *        LENGTH (WS-TS-LENGTH)
00252 *        SET    (ADDRESS OF CHECK-PASS-AREA) END-EXEC
      *    MOVE '*$SI   L              ''   #00001408' TO DFHEIV0
           MOVE X'2A2453492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-TEMP-STORAGE-KEY, 
                 DFHEIV20, 
                 WS-TS-LENGTH, 
                 PI-TEMP-STORAGE-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-PASS-AREA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00253
00254      IF WS-TS-LENGTH NOT GREATER THAN +1
00255          MOVE ZERO               TO  WS-PROG-END
00256          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00257          PERFORM 0110-CLOSE-DLO034
00258          
      * EXEC CICS DELETEQ TS
00259 *            QUEUE (PI-TEMP-STORAGE-KEY) END-EXEC
      *    MOVE '*&                    #   #00001418' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-TEMP-STORAGE-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00260          
      * EXEC CICS RETURN
00261 *            END-EXEC.
      *    MOVE '.(                    &   #00001420' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00262
00284
00285      IF CPA-ALIGNMENT = ZERO
00286          GO TO 0200-MAIN-LOGIC.
00287
00288      IF FIRST-ALIGNMENT
00289          MOVE 'N'                TO  FIRST-TIME-SW
00290          MOVE +999999.99         TO  CPL11-CHECK-AMOUNT
00291                                      SD-PASS-AMOUNT
00292                                      CPL25-NET-PAYOFF
00293          PERFORM SPELL-DOLLAR
00294          MOVE SD-PSA-LINE1       TO  CPL11-PAY-LINE-1
00295          MOVE SD-PSA-LINE2       TO  CPL12-PAY-LINE-2.
00296
00297      IF PI-ASSIGN-CHECK-NUMBERS = 'N'
00298          MOVE CPA-CHECK-NUMBER   TO  CPL6-CHECK-NUMBER
00299        ELSE
00300          MOVE ALL 'X'            TO  CPL6-CHECK-NUMBER.
00301
00302
00303      GO TO 0300-MAIN-LOGIC.
00304  0110-CLOSE-DLO034.
00305
00306 * DLO034 CLOSE
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
00308          MOVE 'C'                TO DL34-PROCESS-TYPE
00309          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
00310          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
00311          MOVE PI-PROCESSOR-ID    TO DL34-USERID
00312          MOVE SPACES             TO DL34-PRINT-LINE
00313                                     DL34-OVERRIDE-PRINTER-ID
00314          
      * EXEC CICS LINK
00315 *            PROGRAM    ('DLO034')
00316 *            COMMAREA   (DLO034-COMMUNICATION-AREA)
00317 *            LENGTH     (DLO034-REC-LENGTH)
00318 *        END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   ''   #00001453' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00319
00320          IF DL34-RETURN-CODE NOT = 'OK'
00321              MOVE  '**DLO034 CLOSE ERROR - ABORT**'
00322                                  TO WS-TEXT-MESSAGE
00323              PERFORM  0120-SEND-TEXT
00324              
      * EXEC CICS RETURN
00325 *            END-EXEC.
      *    MOVE '.(                    &   #00001463' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00326
00327      EJECT
00328
00329  0120-SEND-TEXT.
00330      
      * EXEC CICS SEND TEXT
00331 *         FROM    (WS-TEXT-MESSAGE)
00332 *         LENGTH  (70)
00333 *         END-EXEC.
           MOVE 70
             TO DFHEIV11
      *    MOVE '8&      T       H   F -   #00001469' TO DFHEIV0
           MOVE X'382620202020202054202020' TO DFHEIV0(1:12)
           MOVE X'202020204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEXT-MESSAGE, 
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
           
00334
00335      EJECT
00336  0200-MAIN-LOGIC.
00337      IF CPA-ALIGNMENT NOT = ZERO
00338         MOVE SPACES              TO  CPL5-COMPANY-NAME
00339                                      CPL6-COMPANY-ADDRESS1
00340                                      CPL7-COMPANY-ADDRESS2
00341                                      CPL8-COMPANY-ADDRESS3
00342                                      CPL9-COMPANY-CITY-ST
00343                                      CPL9-COMPANY-ZIP-CODE-X
00344         MOVE CPA-CHECK-NUMBER    TO  CPL6-CHECK-NUMBER
00345         MOVE +9999999.99         TO  CPL11-CHECK-AMOUNT
00346                                      CPL25-NET-PAYOFF
00347                                      SD-PASS-AMOUNT
00348         MOVE +48                 TO  WS-1ST-LINE-LENGTH
00349                                      WS-2ND-LINE-LENGTH
00350         PERFORM SPELL-DOLLAR
00351         MOVE SD-PSA-LINE1        TO  CPL11-PAY-LINE-1
00352         MOVE SD-PSA-LINE2        TO  CPL12-PAY-LINE-2
00353         GO TO 0300-MAIN-LOGIC.
00354
00355      IF FIRST-ALIGNMENT
00356          MOVE 'N'                TO  FIRST-TIME-SW
00357      ELSE
00358          GO TO 0210-MAIN-LOGIC.
00359
00360      MOVE SPACES                 TO  CPL5-COMPANY-NAME
00361                                      CPL6-COMPANY-ADDRESS1
00362                                      CPL7-COMPANY-ADDRESS2
00363                                      CPL8-COMPANY-ADDRESS3
00364                                      CPL9-COMPANY-CITY-ST
00365                                      CPL9-COMPANY-ZIP-CODE-X.
00366
00367      IF CPL8-COMPANY-ADDRESS3 = SPACES
00368          MOVE CPL9-COMPANY-CITY-ST  TO  CPL8-COMPANY-ADDRESS3
00369          MOVE SPACES                TO  CPL9-COMPANY-CITY-ST.
00370
00371      IF CPL7-COMPANY-ADDRESS2 = SPACES
00372          MOVE CPL8-COMPANY-ADDRESS3 TO  CPL7-COMPANY-ADDRESS2
00373          MOVE CPL9-COMPANY-CITY-ST  TO  CPL8-COMPANY-ADDRESS3
00374          MOVE SPACES                TO  CPL9-COMPANY-CITY-ST.
00375
00376      IF CPL6-COMPANY-ADDRESS1 = SPACES
00377          MOVE CPL7-COMPANY-ADDRESS2 TO  CPL6-COMPANY-ADDRESS1
00378          MOVE CPL8-COMPANY-ADDRESS3 TO  CPL7-COMPANY-ADDRESS2
00379          MOVE CPL9-COMPANY-CITY-ST  TO  CPL8-COMPANY-ADDRESS3
00380          MOVE SPACES                TO  CPL9-COMPANY-CITY-ST.
00381
00382      MOVE CHECK-PRINT-LINES      TO  CHECK-PRINT-LINES-SAVE-AREA.
00383
00384  0210-MAIN-LOGIC.
00385      MOVE CHECK-PRINT-LINES-SAVE-AREA  TO  CHECK-PRINT-LINES.
00386
00387      MOVE CPA-CHECK-NUMBER        TO  CPL6-CHECK-NUMBER
00388                                       CPL25-CHECK-NO.
00389
00390      IF CPA-CHECK-DATE NOT = LOW-VALUES
00391          MOVE CPA-CHECK-DATE      TO  DC-BIN-DATE-1
00392          MOVE SPACES              TO  DC-OPTION-CODE
00393          PERFORM 8500-DATE-CONVERSION
00394          IF DC-ERROR-CODE = SPACES
00395          MOVE DC-GREG-DATE-1-ALPHA  TO  CPL7-CHECK-DATE.
00396
00397      MOVE CPA-AMOUNT-PAID         TO  SD-PASS-AMOUNT
00398                                       CPL11-CHECK-AMOUNT
00399                                       CPL25-NET-PAYOFF.
00400      PERFORM SPELL-DOLLAR.
00401      MOVE SD-PSA-LINE1            TO  CPL11-PAY-LINE-1.
00402      MOVE SD-PSA-LINE2            TO  CPL12-PAY-LINE-2.
00403
00404      MOVE CPA-PAYEE-NAME          TO  CPL14-PAYEE-NAME.
00405      MOVE CPA-PAYEE-IN-CARE-OF    TO  CPL15-PAYEE-ADDRESS1.
00406      MOVE CPA-PAYEE-ADDRESS-LINE1 TO  CPL16-PAYEE-ADDRESS2.
00407      MOVE CPA-PAYEE-ADDRESS-LINE2 TO  CPL17-PAYEE-ADDRESS3.
00408      MOVE CPA-PAYEE-CITY-ST       TO  CPL18-PAYEE-CITY-ST.
00409      MOVE CPA-PAYEE-ZIP-CODE      TO  CPL18-PAYEE-ZIP-CODE-X.
00410
00411      MOVE CHECK-PRINT-LINE-18     TO  WS-ZIP-CODE-LINE.
00412      SET ZIP-INDEX1 TO +40.
00413      SET ZIP-INDEX2 TO +42.
00414      PERFORM 5100-MOVE-ZIP-CODE.
00415      MOVE WS-ZIP-CODE-LINE        TO  CHECK-PRINT-LINE-18.
00416
00417      IF CPL17-PAYEE-ADDRESS3 = SPACES
00418          MOVE CHECK-PRINT-LINE-18  TO  CHECK-PRINT-LINE-17
00419          MOVE SPACES              TO  CHECK-PRINT-LINE-18.
00420
00421      IF CPL16-PAYEE-ADDRESS2 = SPACES
00422          MOVE CHECK-PRINT-LINE-17  TO  CHECK-PRINT-LINE-16
00423          MOVE CHECK-PRINT-LINE-18  TO  CHECK-PRINT-LINE-17
00424          MOVE SPACES             TO  CHECK-PRINT-LINE-18.
00425
00426      IF CPL15-PAYEE-ADDRESS1 = SPACES
00427          MOVE CHECK-PRINT-LINE-16  TO  CHECK-PRINT-LINE-15
00428          MOVE CHECK-PRINT-LINE-17  TO  CHECK-PRINT-LINE-16
00429          MOVE CHECK-PRINT-LINE-18  TO  CHECK-PRINT-LINE-17
00430          MOVE SPACES             TO  CHECK-PRINT-LINE-18.
00431
00432      IF CPA-CERT-NO = SPACES
00433          MOVE SPACES             TO  CPL25-CERT-NO-DESC.
00434
00435      MOVE PI-COMPANY-ID          TO  CPL23-COMPANY-ID.
00436      MOVE CPA-GROUPING           TO  CPL23-GROUP.
00437      MOVE CPA-ACCOUNT            TO  CPL23-ACCOUNT.
00438      MOVE CPA-CERT-NO            TO  CPL25-CERT-NO.
00439      MOVE CPA-GENERAL-LEDGER     TO  CPL26-GEN-LEDGER.
00440      MOVE ZEROS                  TO  CPL23-DEPT-NO.
00441      MOVE SPACES                 TO  CPL26-COMMENTS.
00442
00443      IF CPA-FIN-RESP = SPACES
00444          MOVE '    STATE -'      TO  CPL23-DESC
00445          MOVE CPA-STATE          TO  CPL23-FIN-RESP
00446      ELSE
00447          MOVE '  FIN-RESP-'      TO  CPL23-DESC
00448          MOVE CPA-FIN-RESP       TO  CPL23-FIN-RESP.
00449
00450      MOVE CPA-STUB1              TO  CPL27-STUB.
00451      MOVE CPA-STUB2              TO  CPL28-STUB.
00452      MOVE CPA-STUB3              TO  CPL29-STUB.
00453      MOVE CPA-STUB4              TO  CPL30-STUB.
00454      MOVE CPA-STUB5              TO  CPL31-STUB.
00455
00456  0300-MAIN-LOGIC.
00457      MOVE CHECK-PRINT-LINE-1     TO  WS-PRINT-AREA.
00458      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00459
00460      MOVE CHECK-PRINT-LINE-3     TO  WS-PRINT-AREA.
00461      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00462
00463      MOVE CHECK-PRINT-LINE-5     TO  WS-PRINT-AREA.
00464      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00465
00466      MOVE CHECK-PRINT-LINE-6     TO  WS-PRINT-AREA.
00467      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00468
00469      MOVE CHECK-PRINT-LINE-7     TO  WS-PRINT-AREA.
00470      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00471
00472      MOVE CHECK-PRINT-LINE-8     TO  WS-PRINT-AREA.
00473      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00474
00475      MOVE CHECK-PRINT-LINE-9     TO  WS-PRINT-AREA.
00476      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00477
00478      MOVE CHECK-PRINT-LINE-11    TO  WS-PRINT-AREA.
00479      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00480
00481      MOVE CHECK-PRINT-LINE-12    TO  WS-PRINT-AREA.
00482      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00483
00484      MOVE CHECK-PRINT-LINE-14    TO  WS-PRINT-AREA.
00485      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00486
00487      MOVE CHECK-PRINT-LINE-15    TO  WS-PRINT-AREA.
00488      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00489
00490      MOVE CHECK-PRINT-LINE-16    TO  WS-PRINT-AREA.
00491      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00492
00493      MOVE CHECK-PRINT-LINE-17    TO  WS-PRINT-AREA.
00494      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00495
00496      MOVE CHECK-PRINT-LINE-18    TO  WS-PRINT-AREA.
00497      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00498
00499      MOVE CHECK-PRINT-LINE-21    TO  WS-PRINT-AREA.
00500      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00501
00502      MOVE CHECK-PRINT-LINE-22    TO  WS-PRINT-AREA.
00503      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00504
00505      MOVE CHECK-PRINT-LINE-23    TO  WS-PRINT-AREA.
00506      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00507
00508      MOVE CHECK-PRINT-LINE-25    TO  WS-PRINT-AREA.
00509      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00510
00511      MOVE CHECK-PRINT-LINE-26    TO  WS-PRINT-AREA.
00512      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00513
00514      MOVE CHECK-PRINT-LINE-27    TO  WS-PRINT-AREA.
00515      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00516
00517      MOVE CHECK-PRINT-LINE-28    TO  WS-PRINT-AREA.
00518      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00519
00520      MOVE CHECK-PRINT-LINE-29    TO  WS-PRINT-AREA.
00521      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00522
00523      MOVE CHECK-PRINT-LINE-30    TO  WS-PRINT-AREA.
00524      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00525
00526      MOVE CHECK-PRINT-LINE-31    TO  WS-PRINT-AREA.
00527      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00528
00529      MOVE +1                     TO  WS-DATA-SENT-SW.
00530
00531      ADD +1  TO  PI-TEMP-STORAGE-ITEM.
00532
00533      GO TO 0100-MAIN-LOGIC.
00534
00535      EJECT
00536
02430
02431      EJECT
02432  5000-MAIN-LOGIC  SECTION.
02433      IF WS-DATA-SENT-SW NOT = ZERO
02434          MOVE ZERO               TO  WS-PROG-END
02435                                      WS-DATA-SENT-SW
02436          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
02437
02438      
      * EXEC CICS DELAY
02439 *        INTERVAL (WS-DELAY-INTERVAL) END-EXEC.
      *    MOVE '0$I                   &   #00001684' TO DFHEIV0
           MOVE X'302449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DELAY-INTERVAL, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02440
02441      GO TO 0100-MAIN-LOGIC.
02442
02443  5000-EXIT.
02444       EXIT.
02445      EJECT
02446  5100-MOVE-ZIP-CODE SECTION.
02447
02448      SET ZIP-INDEX3 TO ZIP-INDEX2.
02449      SET ZIP-INDEX3 UP BY +10.
02450
02451  5110-MOVE-ZIP-CODE.
02452      IF WS-ZIP-CHAR (ZIP-INDEX1) NOT = SPACES
02453          SET ZIP-INDEX1 UP BY +2
02454          GO TO 5120-MOVE-ZIP-CODE.
02455
02456      IF ZIP-INDEX1 GREATER THAN +1
02457          SET ZIP-INDEX1 DOWN BY +1
02458          GO TO 5110-MOVE-ZIP-CODE.
02459
02460      GO TO 5190-MOVE-ZIP-CODE-EXIT.
02461
02462  5120-MOVE-ZIP-CODE.
02463      MOVE WS-ZIP-CHAR (ZIP-INDEX2) TO WS-ZIP-CHAR (ZIP-INDEX1).
02464      MOVE SPACES                   TO WS-ZIP-CHAR (ZIP-INDEX2).
02465
02466      IF ZIP-INDEX2 LESS THAN ZIP-INDEX3
02467          SET ZIP-INDEX1
02468              ZIP-INDEX2 UP BY +1
02469          GO TO 5120-MOVE-ZIP-CODE.
02470
02471  5190-MOVE-ZIP-CODE-EXIT.
02472      EXIT.
02473
02474      EJECT
02475  6000-PRINT-CHECK SECTION.
02476
CIDMOD*                         COPY ELPRTCVP.
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
      *    MOVE '."C                   ''   #00001779' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373739' TO DFHEIV0(25:11)
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
      *    MOVE '$$    C E         L F ,   #00001900' TO DFHEIV0
           MOVE X'242420202020432045202020' TO DFHEIV0(1:12)
           MOVE X'2020202020204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031393030' TO DFHEIV0(25:11)
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
CIDMOD
02477 ******************************************************************
02478 ***                 -ELPRTCVP-                                   *
02479 ***     COPY MEMBER FOR TERMINAL ONLINE PRINT ROUTINE.           *
02480 ***     THIS ROUTINE WILL ACCOMODATE PRINTING TO A 3270          *
02481 ***     TERMINAL PRINTER. A BUFFER OF UP TO 1920 CHARACTERS      *
02482 ***     IS ACCUMULATED AND PRINTED COLLECTIVELY.                 *
02483 ***                                                              *
02484 ***     THIS ROUTINE TO BE USED ONLY WITH ACCOMPANIMENT          *
02485 ***      OF THE WORKING-STORAGE COPY MEMBER ( ELPRTCVD )         *
02486 ***     THE HOST PROGRAM MUST INITIALIZE THE FOLLOWING 3 FIELDS  *
02487 ***      FROM THE ABOVE COPY MEMBER FOR THIS PROCEDURE TO BE     *
02488 ***      SUCCESSFUL.                                             *
02489 ***      05  WS-LINE-LEN    PIC  S9(4)  COMP  VALUE +80.         *
02490 ***                         LENGTH OF THE LINE TO BE PRINTED     *
02491 ***                         DEFAULT IS 80, YOU CAN USE ANY NUMBER*
02492 ***                         UP TO 132.  THIS FIELD IS ONLY ACCEP-*
02493 ***                         TED THE FIRST TIME THRU THE ROUTINE. *
02494 ***      05  WS-PROG-END    PIC  X  VALUE SPACES.                *
02495 ***                         PROGRAM END SWITCH. INITIALIZED      *
02496 ***                         TO SPACE-     MOVE IN ANY NONBLANK   *
02497 ***                         TO IT WHEN PROGRAM IS FINISHED.      *
02498 ***      05  WS-PRINT-AREA.                                      *
02499 ***          10  WS-PASSED-CNTL-CHAR     PIC X.                  *
02500 ***          10  WS-PASSED-DATA          PIC X(132).             *
02501 ***                         USE THE DATA TO BE PRINTED IN THE    *
02502 ***                         WS-PASSED-DATA.                      *
02503 ***                         USE THE STANDARD CARRIAGE CONTROL    *
02504 ***                         CHARACTER IN THE WS-PASSED-CNTL-CHAR *
02505 ***                           SINGLE-SPACE            VALUE ' '  *
02506 ***                           DOUBLE-SPACE            VALUE '0'  *
02507 ***                           TRIPLE-SPACE            VALUE '-'  *
02508 ***                           TOP-PAGE                VALUE '1'  *
02509 ***      NOTE: A LINE COUNT IS PROVIDED IN FIELDNAME -WS-LINE-CNT*
02510 ***            THE USE OF THIS FIELD IS OPTIONAL.                *
02511 ***            THIS ROUTINE WILL ONLY ADD 1, 2, OR 3             *
02512 ***            TO THIS COUNT DEPENDING ON THE WS-PASSED-CNTL-CHAR*
02513 ***            AND RESET THE COUNT TO ZERO WHEN TOP-PAGE         *
02514 ***            CONDITION.                                        *
02515 ***                                                              *
02516 ******************************************************************
02517 *
02518 *ELPRTCVP.
02519 *
02520 *    IF PI-COMPANY-ID IS EQUAL TO 'DMD' OR 'CID'
02521 *        MOVE 'P'                TO DL34-PROCESS-TYPE
02522 *        MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
02523 *        MOVE PI-PROCESSOR-ID    TO DL34-USERID
02524 *        MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
02525 *        MOVE WS-PRINT-AREA      TO DL34-PRINT-LINE
02526 *        MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID
02527 *
02528 *        EXEC CICS LINK
02529 *            PROGRAM    ('DLO034')
02530 *            COMMAREA   (DLO034-COMMUNICATION-AREA)
02531 *            LENGTH     (DLO034-REC-LENGTH)
02532 *        END-EXEC
02533 *
02534 *           IF DL34-RETURN-CODE = 'OK'
02535 *               GO TO ELPRTCVP-EXIT
02536 *           ELSE
02537 *               MOVE '8339'     TO EMI-ERROR ?????ERROR MESSAGE???
02538 *               GO TO ELPRTCVP-EXIT.
02539 *
02540 *    IF NOT FIRST-TIME
02541 *        GO TO ELPRTCVP-020.
02542 *
02543 *    IF WS-LINE-LEN NOT GREATER THAN ZERO
02544 *        GO TO ELPRTCVP-EXIT.
02545 *
02546 *    MOVE '2'           TO WS-FIRST-TIME-SW.
02547 *    MOVE LOW-VALUES    TO WS-BUFFER-AREA.
02548 *
02549 *    SET BUFFER-INDEX TO +1.
02550 *
02551 *    IF PI-COMPANY-ID EQUAL 'HER' OR 'TIH' OR 'LAP' OR 'RMC' OR
02552 *                           'CVL' OR 'HAN' OR 'NSL' OR 'CNL'
02553 *        NEXT SENTENCE
02554 *    ELSE
02555 *       IF NOT TOP-PAGE
02556 *           MOVE T-TP       TO WS-BUFFER-BYTE (BUFFER-INDEX)
02557 *           SET BUFFER-INDEX UP BY +1.
02558 *
02559 *ELPRTCVP-020.
02560 *    IF WS-PROG-END = SPACES
02561 *        GO TO ELPRTCVP-030.
02562 *
02563 *    MOVE '1'               TO WS-FIRST-TIME-SW.
02564 *    MOVE SPACES            TO WS-PROG-END.
02565 *
02566 *    IF BUFFER-INDEX GREATER THAN +1
02567 *        PERFORM ELPRTCVP-PRINT-BUFFER THRU ELPRTCVP-PRINT-EXIT.
02568 *
02569 *    GO TO ELPRTCVP-EXIT.
02570 *
02571 *ELPRTCVP-030.
02572 *    IF WS-PASSED-DATA = SPACES
02573 *        SET PRT-INDEX TO +1
02574 *        GO TO ELPRTCVP-050.
02575 *
02576 *    SET PRT-INDEX TO WS-LINE-LEN.
02577 *
02578 *ELPRTCVP-040.
02579 *    IF WS-PRINT-BYTE (PRT-INDEX) NOT = SPACES
02580 *        GO TO ELPRTCVP-050.
02581 *
02582 *    IF PRT-INDEX GREATER THAN +1
02583 *        SET PRT-INDEX DOWN BY +1
02584 *        GO TO ELPRTCVP-040.
02585 *
02586 *ELPRTCVP-050.
02587 *    SET WS-LINE-LENGTH TO PRT-INDEX.
02588 *    SET BUFFER-INDEX2 TO BUFFER-INDEX.
02589 *    SET BUFFER-INDEX2 UP BY WS-LINE-LENGTH.
02590 *
02591 *    IF BUFFER-INDEX2 IS NOT LESS THAN WS-BUFFER-SIZE
02592 *        PERFORM ELPRTCVP-PRINT-BUFFER THRU ELPRTCVP-PRINT-EXIT.
02593 *
02594 *    IF TRIPLE-SPACE
02595 *         ADD +2  TO  WS-LINE-CNT
02596 *         MOVE T-SS           TO WS-BUFFER-BYTE (BUFFER-INDEX)
02597 *                                WS-BUFFER-BYTE (BUFFER-INDEX + 1)
02598 *         SET BUFFER-INDEX UP BY +2.
02599 *
02600 *    IF DOUBLE-SPACE
02601 *         ADD +1  TO  WS-LINE-CNT
02602 *         MOVE T-SS             TO WS-BUFFER-BYTE (BUFFER-INDEX)
02603 *         SET BUFFER-INDEX UP BY +1.
02604 *
02605 *    ADD +1 TO WS-LINE-CNT
02606 ************************************************************
02607 *     BYPASS NEW LINE SYMBOL                               *
02608 *        IF FIRST BUFFER SENT AND TOP-OF-FORM SET.         *
02609 *     OR IF FIRST LINE OF SUBSEQUENT BUFFERS.              *
02610 ************************************************************
02611 *
02612 *    IF (BUFFER-INDEX GREATER THAN +1 AND
02613 *        WS-BUFFER-BYTE (BUFFER-INDEX - 1) = T-TP)
02614 *        OR   FIRST-LINE-NEXT-BUFFER
02615 *        MOVE ZERO               TO WS-FIRST-TIME-SW
02616 *       ELSE
02617 *        MOVE T-SS          TO WS-BUFFER-BYTE (BUFFER-INDEX)
02618 *        SET BUFFER-INDEX UP BY +1.
02619 *
02620 *    IF TOP-PAGE
02621 **        NOTE, SINGLE SPACE IS REQUIRED BEFORE TOP PAGE CHAR
02622 *         MOVE +1                TO WS-LINE-CNT
02623 *         MOVE T-TP              TO WS-BUFFER-BYTE (BUFFER-INDEX)
02624 *         SET BUFFER-INDEX UP BY +1.
02625 *
02626 *    IF WS-PASSED-DATA = SPACES
02627 *        GO TO ELPRTCVP-EXIT.
02628 *
02629 *    SET PRT-INDEX TO +1.
02630 *
02631 *ELPRTCVP-060.
02632 *    MOVE WS-PRINT-BYTE (PRT-INDEX)
02633 *                                TO WS-BUFFER-BYTE (BUFFER-INDEX).
02634 *    SET BUFFER-INDEX UP BY +1.
02635 *
02636 *    IF PRT-INDEX LESS THAN WS-LINE-LENGTH
02637 *        SET PRT-INDEX UP BY +1
02638 *        GO TO ELPRTCVP-060.
02639 *
02640 *    SET WS-BUFFER-LENGTH TO BUFFER-INDEX.
02641 *
02642 *ELPRTCVP-EXIT.
02643 *    EXIT.
02644 *
02645 *ELPRTCVP-PRINT-BUFFER.
02646 *    IF WS-BUFFER-BYTE (BUFFER-INDEX - 1) = T-SS
02647 *       MOVE SPACE               TO WS-BUFFER-BYTE (BUFFER-INDEX)
02648 *       SET BUFFER-INDEX UP BY 1.
02649 *
02650 *    MOVE  T-EM                  TO WS-BUFFER-BYTE (BUFFER-INDEX)
02651 *    SET WS-BUFFER-LENGTH TO BUFFER-INDEX.
02652 *
02653 *    ADD +1  TO  WS-DUMP-COUNT.
02654 *
02655 *    EXEC CICS SEND
02656 *        FROM    (WS-BUFFER-AREA)
02657 *        LENGTH  (WS-BUFFER-LENGTH)
02658 *        CTLCHAR (WS-WCC-CNTL)
02659 *        ERASE   END-EXEC
02660 *
02661 *    EXEC CICS SYNCPOINT
02662 *        END-EXEC.
02663 *
02664 *    SET BUFFER-INDEX TO +1.
02665 *    MOVE '2'                    TO WS-FIRST-TIME-SW.
02666 *
02667 *ELPRTCVP-PRINT-EXIT.
02668 *    EXIT.
02669 *
02670 *    EJECT
02671  8500-DATE-CONVERSION SECTION.
02672      
      * EXEC CICS LINK
02673 *        PROGRAM  ('ELDATCV')
02674 *        COMMAREA (DATE-CONVERSION-DATA)
02675 *        LENGTH   (DC-COMM-LENGTH) END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00002109' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02676
02677  8500-EXIT.
02678      EXIT.
02679
02680      EJECT
02681  SPELL-DOLLAR SECTION. 
      *                      COPY ELC176P1.
00001 *****************************************************************
00002 *                                                               *
00003 *                            ELC176P1.                          *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                         *
00006 *                                                               *
00007 * THIS SECTION CONVERTS A DOLLAR FIGURE INTO A SPELLED OUT AMT. *
00002 *                                                               *
00008 *****************************************************************
00009
00010  SDS-010.
00011      MOVE SPACES                 TO  WS-SPELLED-AMOUNT
00012                                      SD-PASS-SPELLED-AMOUNT
00013                                      WS-SPELLED-LINE1
00014                                      WS-SPELLED-LINE2.
00015
00016      SET SA-INDEX TO +1.
00017
00018      MOVE SD-PASS-AMOUNT         TO  WS-AMOUNT.
00019
00020      IF WS-MILLIONS GREATER ZERO
00021          MOVE WS-MILLIONS        TO  WS-AMOUNT-WORK
00022          PERFORM SPELL-AMOUNT
00023          MOVE 'MILLION'          TO  WS-WORD
00024          PERFORM MOVE-WORD.
00025
00026      IF WS-THOUSANDS GREATER ZERO
00027          MOVE WS-THOUSANDS       TO  WS-AMOUNT-WORK
00028          PERFORM SPELL-AMOUNT
00029          MOVE 'THOUSAND'         TO  WS-WORD
00030          PERFORM MOVE-WORD.
00031
00032      IF WS-HUNDREDS GREATER ZERO
00033          MOVE WS-HUNDREDS        TO  WS-AMOUNT-WORK
00034          PERFORM SPELL-AMOUNT.
00035
00036      IF WS-AMOUNT LESS +1.00
00037          MOVE 'NO'               TO  WS-WORD
00038          PERFORM MOVE-WORD.
00039
00040      IF WS-CENTS NOT GREATER ZERO
00041          MOVE 'NO'               TO  WS-CENTS-X.
00042
00043      MOVE WS-CENTS-X             TO  WS-PENNEYS.
00044
00045      MOVE WS-DOLLARS-AND-CENTS   TO  WS-WORD.
00046      PERFORM MOVE-WORD.
00047
00048      INSPECT WS-SPELLED-AMOUNT REPLACING ALL '-' BY SPACES.
00049
00050      MOVE WS-SPELLED-AMOUNT      TO  SD-PASS-SPELLED-AMOUNT.
00051
00052      PERFORM MOVE-SPELLED-AMOUNT.
00053
00054  SDS-EXIT.
00055      EXIT.
00056
00057      EJECT
00058  SPELL-AMOUNT SECTION.
00059
00060 *SAS-NOTE.
00061 *
00062 *    NOTE *******************************************************
00063 *         *      THIS SECTION CONVERTS A THREE DIGIT NUMBER     *
00064 *         *  INTO A SPELLED AMOUNT.                             *
00065 *         *******************************************************
00066
00067  SAS-010.
00068      IF WS-HUNDRED GREATER ZERO
00069          SET SINGLE-INDEX        TO  WS-HUNDRED
00070          MOVE WS-SINGLE-DESC (SINGLE-INDEX)  TO  WS-WORD
00071          PERFORM MOVE-WORD
00072          MOVE 'HUNDRED'          TO  WS-WORD
00073          PERFORM MOVE-WORD.
00074
00075      IF WS-TEEN GREATER ZERO
00076          IF WS-TEEN LESS +20
00077              SET SINGLE-INDEX TO WS-TEEN
00078              MOVE WS-SINGLE-DESC (SINGLE-INDEX)  TO  WS-WORD
00079              PERFORM MOVE-WORD
00080            ELSE
00081              SET UPPER-INDEX TO WS-TEN
00082              MOVE WS-UPPER-DESC (UPPER-INDEX)  TO  WS-WORD
00083              PERFORM MOVE-WORD
00084              IF WS-ONE GREATER ZERO
00085                  SET SINGLE-INDEX TO WS-ONE
00086                  MOVE WS-SINGLE-DESC (SINGLE-INDEX)  TO  WS-WORD
00087                  PERFORM MOVE-WORD.
00088
00089  SAS-EXIT.
00090      EXIT.
00091
00092      EJECT
00093  MOVE-WORD SECTION.
00094
00095 *MWS-NOTE.
00096 *
00097 *    NOTE *******************************************************
00098 *         *      THIS SECTION MOVES ONE WORD TO THE SPELLED     *
00099 *         *  AMOUNT OUTPUT LINE.                                *
00100 *         *******************************************************.
00101
00102  MWD-010.
00103      PERFORM MOVE-CHARACTERS
00104          VARYING CHAR-INDEX FROM +1 BY +1
00105              UNTIL WS-CHAR2 (CHAR-INDEX) = SPACES.
00106
00107      SET SA-INDEX UP BY +1.
00108
00109  MWD-EXIT.
00110      EXIT.
00111
00112  MOVE-CHARACTERS SECTION.
00113
00114 *MCS-NOTE.
00115 *
00116 *    NOTE *******************************************************
00117 *         *      THIS SECTION MOVES ONE CHARACTER TO THE SPELLED*
00118 *         *  AMOUNT OUTPUT LINE.                                *
00119 *         *******************************************************.
00120
00121  MCD-010.
00122      MOVE WS-CHAR2 (CHAR-INDEX)  TO  WS-CHAR (SA-INDEX).
00123
00124      SET SA-INDEX UP BY +1.
00125
00126  MCD-EXIT.
00127      EXIT.
00128
00129      EJECT
00130  MOVE-SPELLED-AMOUNT SECTION.
00131
00132 *MSA-NOTE.
00133 *
00134 *    NOTE *******************************************************
00135 *         *      THIS SECTION MOVES THE SPELLED DOLLAR AMOUNT   *
00136 *         *  TO TWO LINES IF NECESSARY.                         *
00137 *         *******************************************************.
00138
00139  MSA-010.
00140      ADD WS-1ST-LINE-LENGTH +1 GIVING WS-1ST-LINE-LENGTH-PLUS-1.
00141      ADD WS-1ST-LINE-LENGTH +2 GIVING WS-1ST-LINE-LENGTH-PLUS-2.
00142      ADD WS-1ST-LINE-LENGTH -1 GIVING WS-1ST-LINE-LENGTH-MINUS-1.
00143
00144      MOVE WS-SPELLED-AMOUNT  TO  WS-SPELLED-LINE1.
00145
00146      IF SA-INDEX GREATER WS-1ST-LINE-LENGTH-PLUS-1
00147          SET SL2-INDEX TO +1
00148          IF WS-CHAR (WS-1ST-LINE-LENGTH-PLUS-1) = SPACES
00149              PERFORM MOVE-LINE2 VARYING SA-INDEX2
00150                FROM WS-1ST-LINE-LENGTH-PLUS-2 BY +1
00151                  UNTIL SL2-INDEX GREATER WS-2ND-LINE-LENGTH
00152            ELSE
00153              PERFORM CLEAR-LINE1 VARYING SL1-INDEX
00154                  FROM WS-1ST-LINE-LENGTH BY -1
00155                      UNTIL WS-SL1 (SL1-INDEX) = SPACES
00156              SET SL1-INDEX UP BY +1
00157              PERFORM MOVE-LINE2
00158                VARYING SA-INDEX2 FROM SL1-INDEX BY +1
00159                  UNTIL SL2-INDEX GREATER WS-2ND-LINE-LENGTH.
00160
00161      MOVE WS-SPELLED-LINE1       TO  SD-PSA-LINE1.
00162      MOVE WS-SPELLED-LINE2       TO  SD-PSA-LINE2.
00163
00164  MSA-EXIT.
00165      EXIT.
00166
00167      EJECT
00168  CLEAR-LINE1 SECTION.
00169
00170 *CLS-NOTE.
00171 *
00172 *    NOTE *******************************************************
00173 *         *      THIS SECTION CLEARS THE TRAILING WORD IN THE   *
00174 *         *  SPELLED LINE 1 IF THE AMOUNT IS GREATER THAN 78.   *
00175 *         *******************************************************.
00176
00177  CLS-010.
00178      MOVE SPACES                 TO  WS-SL1 (SL1-INDEX).
00179
00180  CLS-EXIT.
00181      EXIT.
00182
00183  MOVE-LINE2 SECTION.
00184
00185 *MLS-NOTE.
00186 *
00187 *    NOTE *******************************************************
00188 *         *      THIS SECTION MOVES ONE CHARACTER TO THE SPELLED*
00189 *         *  AMOUNT OUTPUT LINE.                                *
00190 *         *******************************************************.
00191
00192  MLS-010.
00193      MOVE WS-CHAR (SA-INDEX2)    TO  WS-SL2 (SL2-INDEX).
00194
00195      SET SL2-INDEX UP BY +1.
00196
00197      IF WS-CHAR (SA-INDEX2)     = SPACES   AND
00198         WS-CHAR (SA-INDEX2 + 1) = SPACES
00199          SET SL2-INDEX TO +99.
00200
00201  MLS-EXIT.
00202      EXIT.
00203
02682
02683      EJECT
02684  POS-SPELL-DOLLAR SECTION.
02685
02686 *SDS-NOTE.
02687 *
02688 *    NOTE *******************************************************
02689 *         *      THIS SECTION CONVERTS A DOLLAR FIGURE INTO A   *
02690 *         *  SPELLED OUT AMOUNT.                                *
02691 *         *******************************************************.
02692
02693  SDS-010.
02694      MOVE SPACES                 TO  WS-SPELLED-AMOUNT
02695                                      SD-PASS-SPELLED-AMOUNT
02696                                      WS-SPELLED-LINE1
02697                                      WS-SPELLED-LINE2.
02698      MOVE ZERO                   TO  WS-SW.
02699
02700      SET SA-INDEX TO +1.
02701
02702      MOVE SD-PASS-AMOUNT         TO  WS-AMOUNT.
02703
02704      IF WS-MILLIONS IS GREATER THAN ZERO
02705          MOVE WS-MILLIONS        TO  WS-AMOUNT-WORK
02706          PERFORM POS-SPELL-AMOUNT
02707          MOVE +1                 TO  WS-SW.
02708
02709      IF WS-THOUSANDS IS GREATER THAN ZERO
02710          MOVE WS-THOUSANDS       TO  WS-AMOUNT-WORK
02711          PERFORM POS-SPELL-AMOUNT
02712          MOVE +1                 TO  WS-SW
02713        ELSE
02714          IF WS-MILLIONS IS GREATER THAN ZERO
02715              MOVE 'ZERO'         TO  WS-WORD
02716              PERFORM MOVE-WORD 3 TIMES.
02717
02718      IF WS-HUNDREDS IS GREATER THAN ZERO
02719          MOVE WS-HUNDREDS        TO  WS-AMOUNT-WORK
02720          PERFORM POS-SPELL-AMOUNT
02721          MOVE +1                 TO  WS-SW
02722        ELSE
02723          IF WS-MILLIONS IS GREATER THAN ZERO
02724            OR WS-THOUSANDS IS GREATER THAN ZERO
02725              MOVE 'ZERO'         TO  WS-WORD
02726              PERFORM MOVE-WORD 3 TIMES.
02727
02728      IF WS-AMOUNT IS LESS THAN +1.00
02729          MOVE 'NO'               TO  WS-WORD
02730          PERFORM MOVE-WORD.
02731
02732      IF WS-CENTS IS NOT GREATER THAN ZERO
02733          MOVE 'NO'               TO  WS-CENTS-X.
02734
02735      MOVE WS-CENTS-X             TO  WS-PENNEYS.
02736
02737      MOVE WS-DOLLARS-AND-CENTS   TO  WS-WORD.
02738      PERFORM MOVE-WORD.
02739
02740      INSPECT WS-SPELLED-AMOUNT CONVERTING '-' TO ' '.
02741
02742      MOVE WS-SPELLED-AMOUNT      TO  SD-PASS-SPELLED-AMOUNT.
02743
02744      PERFORM MOVE-SPELLED-AMOUNT.
02745
02746  SDS-EXIT.
02747      EXIT.
02748
02749      EJECT
02750  POS-SPELL-AMOUNT SECTION.
02751
02752 *SAS-NOTE.
02753 *
02754 *    NOTE *******************************************************
02755 *         *      THIS SECTION CONVERTS A THREE DIGIT NUMBER     *
02756 *         *  INTO A SPELLED AMOUNT.                             *
02757 *         *******************************************************.
02758
02759  SAS-010.
02760      IF WS-HUNDRED IS GREATER THAN ZERO
02761          SET SINGLE-INDEX        TO  WS-HUNDRED
02762          MOVE WS-SINGLE-DESC (SINGLE-INDEX)  TO  WS-WORD
02763          PERFORM MOVE-WORD
02764        ELSE
02765          IF WS-SW NOT = ZERO
02766              MOVE 'ZERO'         TO  WS-WORD
02767              PERFORM MOVE-WORD.
02768
02769      IF WS-TEN IS GREATER THAN ZERO
02770          SET SINGLE-INDEX TO WS-TEN
02771          MOVE WS-SINGLE-DESC (SINGLE-INDEX)  TO  WS-WORD
02772          PERFORM MOVE-WORD
02773        ELSE
02774          IF WS-HUNDRED IS GREATER THAN ZERO
02775            OR WS-SW NOT = ZERO
02776              MOVE 'ZERO'         TO  WS-WORD
02777              PERFORM MOVE-WORD.
02778
02779      IF WS-ONE IS GREATER THAN ZERO
02780          SET SINGLE-INDEX TO WS-ONE
02781          MOVE WS-SINGLE-DESC (SINGLE-INDEX)  TO  WS-WORD
02782          PERFORM MOVE-WORD
02783        ELSE
02784          IF WS-HUNDRED IS GREATER THAN ZERO
02785            OR WS-TEN IS GREATER THAN ZERO
02786            OR WS-SW NOT = ZERO
02787              MOVE 'ZERO'         TO  WS-WORD
02788              PERFORM MOVE-WORD.
02789
02790  SAS-EXIT.
02791      EXIT.
02792
02793      EJECT
02794  9990-ERROR SECTION.
02795      
      * EXEC CICS LINK
02796 *        PROGRAM  ('EL004')
02797 *        COMMAREA (DFHEIBLK)
02798 *        LENGTH   (64) END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 64
             TO DFHEIV11
      *    MOVE '."C                   ''   #00002437' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIBLK, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02799
02800  9990-EXIT.
02801      EXIT.
02802
02803  9999-LAST-PARAGRAPH SECTION.
02804
02805      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL688' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL688' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 5000-MAIN-LOGIC,
                     5000-MAIN-LOGIC,
                     9990-ERROR
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL688' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
