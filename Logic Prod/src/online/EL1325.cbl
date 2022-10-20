00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL1325.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/13/96 09:49:45.
00007 *                            VMOD=2.003.
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
00025
00026 *        THIS PROGRAM PROVIDES ALLOWS AN OPERATOR DIRECT ACCESS
00027 *    TO A CLAIM RECORD OR PERMITS A BROWSE OF THE CLAIM FILE.
00028
00029 *    SCREENS     - EL1324 - ALPHA LOOK-UP MATCH LIST
00030
00031 *    ENTERED BY  - EL126 - MAINTENANCE MENU
00032 *                  EL130 - NEW CLAIM SET-UP
00033 *                  EL150 - STATUS DISPLAY (ENTERS EL1325 ONLY)
00034
00035 *    EXIT TO     - CALLING PROGRAM
00036 *                  EL1324 - ALPHA INDEXT LOOK-UP
00037
00038 *    INPUT FILE  - ELMSTR - CLAIM MASTER FILE
00039 *                  ELALPH - ALPHA INDEX FILE
00040
00041 *    OUTPUT FILE - NONE
00042
00043 *    COMMAREA    - PASSED.  IF A CLAIM IS SELECTED, THE
00044 *                  CONTROL OF THAT CLAIM IS PLACED IN THE
00045 *                  APPROPRIATE FIELDS OF THE COMMAAREA FOR
00046 *                  REFERENCE BY SUCCESSIVE PROGRAMS.  THE PROGRAM
00047 *                  WORK AREA OF THE COMMAREA IS USED TO PASS THE
00048 *                  RECORD KEY INFORMATION NEEDED BY EL1324 TO
00049 *                  LOCATE THE CLAIM.
00050
00051
00052 *    NARRATIVE  - USING THE CONTROL INFORMATION PASSED FROM
00053 *                 EL1324, START A BROWSE ON THE CLAIM MASTER FILE
00054 *                 IN AN ATTEMPT TO FIND THE RECORDS INDICATED.
00055 *                 IF MORE THAN 16 MATCHES ARE FOUND, THE SCREEN
00056 *                 WILL SHOW THE FIRST 16, AND WAIT FOR THE
00057 *                 OPERATOR TO CONTINUE THE BROWSE.  IF ONLY ONE
00058 *                 ENTRY IS FOUND PASS CONTROL TO THE APPROPRIATE
00059 *                 PROGRAM.
101201******************************************************************
101201*                   C H A N G E   L O G
101201*
101201* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101201*-----------------------------------------------------------------
101201*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101201* EFFECTIVE    NUMBER
101201*-----------------------------------------------------------------
101201* 101201    2001100100006  SMVA  ADD USERID TO SCREEN HEADER
101201*                              ADJUSTED REDEFINES EL1325AI FILLER
101201******************************************************************
00060
00061
00062      EJECT
00063  ENVIRONMENT DIVISION.
00064
00065  DATA DIVISION.
00066
00067  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00068  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.
00069
00070  77  FILLER  PIC X(32)  VALUE '********************************'.
00071  77  FILLER  PIC X(32)  VALUE '*   EL1325 WORKING STORAGE     *'.
00072  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.003 ************'.
00073
00074 *                                COPY ELCSCTM.
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
00075
00076 *                                COPY ELCSCRTY.
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
00077
00078  01  WS-DATE-AREA.
00079      05  SAVE-DATE               PIC X(8)    VALUE SPACES.
00080      05  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.
00081
00082  01  ERROR-MESSAGES.
00083      12  ER-0000                 PIC X(4)  VALUE '0000'.
00084      12  ER-0004                 PIC X(4)  VALUE '0004'.
00085      12  ER-0008                 PIC X(4)  VALUE '0008'.
00086      12  ER-0019                 PIC X(4)  VALUE '0019'.
00087      12  ER-0022                 PIC X(4)  VALUE '0022'.
00088      12  ER-0029                 PIC X(4)  VALUE '0029'.
00089      12  ER-0031                 PIC X(4)  VALUE '0031'.
00090      12  ER-0070                 PIC X(4)  VALUE '0070'.
00091      12  ER-0089                 PIC X(4)  VALUE '0089'.
00092      12  ER-0130                 PIC X(4)  VALUE '0130'.
00093      12  ER-0200                 PIC X(4)  VALUE '0200'.
00094      12  ER-0228                 PIC X(4)  VALUE '0228'.
00095      12  ER-0284                 PIC X(4)  VALUE '0284'.
00096
00097  01  FILLER                          COMP-3.
00098
00099      05  WS-RECORD-COUNT         PIC S9(3)   VALUE ZERO.
00100      05  WS-READNEXT-SW          PIC S9      VALUE ZERO.
00101      05  WS-NOT-FOUND            PIC S9      VALUE ZERO.
00102      05  WS-ERROR-NUMBER         PIC S9(3)   VALUE ZERO.
00103      05  WS-ERROR-COUNT          PIC S9(3)   VALUE ZERO.
00104      05  WS-LAST-ERROR-COUNT     PIC S9(3)   VALUE ZERO.
00105      05  WS-UPDATE-SW            PIC S9      VALUE ZERO.
00106      05  WS-COMPLETED-SUCCESSFUL PIC S9      VALUE ZERO.
00107          88  TRANSACTION-SUCCESSFUL          VALUE +1.
00108
00109      05  TIME-IN                 PIC S9(7)   VALUE ZERO.
00110      05  TIME-OUT         REDEFINES
00111          TIME-IN                 PIC S9(3)V9(4).
00112
00113      05  WS-MONTH-WORK           PIC S9(3)   VALUE ZERO.
00114      05  WS-YEAR-WORK            PIC S9(3)   VALUE ZERO.
00115      05  WS-AIX-RECORD-COUNT     PIC S9(5)   VALUE ZERO.
00116      05  WS-CLAIMS-SW            PIC S9      VALUE ZERO.
00117          88  WS-NO-CLAIMS-FOUND              VALUE ZERO.
00118  01  FILLER            COMP SYNCHRONIZED.
00119
00120      05  WS-INDEX                PIC S9(4)   VALUE ZERO.
00121
00122      05  WS-JOURNAL-FILE-ID      PIC S9(4)   VALUE +1.
00123      05  WS-JOURNAL-RECORD-LENGTH PIC S9(4)  VALUE +527.
00124      05  SC-ITEM                 PIC S9(4)   VALUE +0001.
00125
00126
00127  01  FILLER.
00128
00129      05  QID.
00130          10  QID-TERM            PIC X(4).
00131          10  FILLER              PIC X(4)  VALUE '132E'.
00132      05  QID-PROC-AREA           PIC XXX.
00133      05  QID-LENGTH              PIC S9(4) VALUE +3  COMP.
00134      05  QID-ITEM                PIC S9(4) VALUE +1  COMP.
00135
00136      05  WS-CLCNTL-KEY.
00137          10  WS-CLCNTL-ID        PIC X(3).
00138          10  WS-CLCNTL-TYPE      PIC X.
00139          10  WS-CLCNTL-USER      PIC X(04) VALUE SPACES.
00140          10  WS-CLCNTL-SEQ       PIC S9(4) VALUE +0      COMP.
00141
00142      05  WS-MAPSET-NAME          PIC X(8)  VALUE 'EL1325S '.
00143      05  WS-MAP-NAME             PIC X(8)  VALUE 'EL1325A '.
00144
00145      05  FILLER           REDEFINES
00146          WS-MAP-NAME.
00147          10  FILLER              PIC XX.
00148          10  WS-MAP-NUMBER       PIC X(4).
00149          10  FILLER              PIC XX.
00150
00151      05  THIS-PGM                PIC X(08)   VALUE 'EL1325'.
00152
00153      05  ALPHA-INDEX-DSID        PIC X(08)   VALUE 'ELALPH'.
00154      05  ELMSTR-DSID             PIC X(08)   VALUE 'ELMSTR'.
00155      05  ELCNTL-FILE-ID          PIC X(08)   VALUE 'ELCNTL'.
00156      05  WS-CNTL-REC-FOUND-SW    PIC X(01)   VALUE SPACES.
00157      05  WS-NEXT-COMPANY-ID      PIC X(03)   VALUE SPACES.
00158
00159      05  WS-TRANS-ID             PIC X(4)    VALUE 'E034'.
00160
00161      05  WS-TEMP-STORAGE-KEY.
00162          10  WS-TSK-TERM-ID      PIC X(4)    VALUE 'XXXX'.
00163          10  FILLER              PIC X(4)    VALUE '1325'.
00164
00165      05  WS-TS-LENGTH            PIC S9(4)   VALUE +1920   COMP
00166                                      SYNCHRONIZED.
00167
00168      05  WS-CURRENT-DATE         PIC XX VALUE LOW-VALUES.
00169      05  WS-SAVE-CLAIM-NO        PIC X(07)  VALUE SPACES.
00170      05  WS-CLAIMS-FOUND-SW      PIC X(01).
00171          88  WS-CLAIMS-MATCH         VALUE 'Y'.
00172          88  WS-CLAIMS-NO-MATCH      VALUE 'N'.
00173
00174      05  WS-DATE-WORK.
00175          10  WS-DW-MONTH         PIC 99.
00176          10  FILLER              PIC X.
00177          10  WS-DW-DAY           PIC 99.
00178          10  FILLER              PIC X.
00179          10  WS-DW-YEAR          PIC 99.
00180
00181      05  WS-CHAR                 PIC X       VALUE SPACES.
00182
00183      05  WS-INITIALS.
00184          10  WS-INIT1            PIC X.
00185          10  WS-INIT2            PIC X.
00186
00187      05  WS-CALC-RDNXT           PIC S9(8) VALUE ZERO     COMP.
00188
00189      EJECT
00190      05  WS-KEY-HOLD.
00191          10  WS-KH-CHAR          PIC X
00192              OCCURS 30 TIMES        INDEXED BY KEY-INDEX.
00193
00194      05  WS-KEY-INPUT.
00195          10  WS-KI-CHAR          PIC X
00196              OCCURS 30 TIMES        INDEXED BY KEY-INDEX2.
00197
00198 *                                COPY ELCNWA.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELCNWA.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                         *
00006 *                                                               *
00007 *            M O V E   N A M E   W O R K   A R E A.             *
00008 *                                                               *
00009 *****************************************************************.
00010
00011  01  WS-NAME-WORK-AREA.
00012      05  WS-INSURED-LAST-NAME        PIC X(15).
00013      05  WS-INSURED-1ST-NAME         PIC X(12).
00014      05  WS-INSURED-MID-INIT         PIC X.
00015
00016      05  WS-NAME-WORK.
00017          10  WS-NW                   PIC X
00018              OCCURS 30 TIMES INDEXED BY NWA-INDEX.
00019
00020      05  WS-NAME-WORK2.
00021          10  WS-NW2                  PIC X
00022              OCCURS 20 TIMES INDEXED BY NWA-INDEX2 NWA-INDEX3
00023                                         NWA-INDEX0.
00024
00025      05  WS-NAME-SW                  PIC S9          VALUE ZERO
00026                                      COMP-3.
00027
00199
00200      EJECT
00201 *                                COPY ELCINTF.
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
00202
00203 *                               COPY ELC132PI.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELC132PI.                          *
00004 *                            VMOD=2.004                         *
00005 *****************************************************************.
00006
00007      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00008          16  PI-1ST-TIME-SW              PIC S9       COMP-3.
00009          16  PI-LINE-COUNT               PIC S9(3)    COMP-3.
00010          16  PI-AIX-RECORD-COUNT         PIC S9(5)    COMP-3.
00011          16  PI-BROWSE-SW                PIC S9       COMP-3.
00012          16  PI-START-SW                 PIC S9       COMP-3.
00013          16  PI-END-OF-FILE              PIC S9       COMP-3.
00014          16  PI-DSID                     PIC X(8).
00015
00016          16  PI-KEEP-CERT-NO     PIC X(11)   OCCURS 16
00017                      INDEXED BY PI-K-INDEX.
00018
00019          16  PI-OPTION                   PIC X.
00020              88  NO-OPTION-SELECTED           VALUE ZERO.
00021              88  OPTION-ONE-SELECTED          VALUE '1'.
00022              88  OPTION-TWO-SELECTED          VALUE '2'.
00023              88  OPTION-THREE-SELECTED        VALUE '3'.
00024              88  OPTION-FOUR-SELECTED         VALUE '4'.
00025              88  OPTION-FIVE-SELECTED         VALUE '5'.
00026
00027          16  PI-SELECTION-CRITERIA.
00028              20  PI-SC-COMPANY-CD        PIC X.
00029              20  PI-SC-CARRIER           PIC X.
00030              20  PI-SC-CLAIM-NO          PIC X(7).
00031              20  PI-SC-CERT-NO.
00032                  25  PI-SC-CERT-PRIME    PIC X(10).
00033                  25  PI-SC-CERT-SFX      PIC X.
00034              20  FILLER                  PIC X(09).
00035
00036          16  FILLER REDEFINES PI-SELECTION-CRITERIA.
00037              20  FILLER                  PIC X.
00038              20  PI-SC-LAST-NAME         PIC X(15).
00039              20  PI-SC-INITIALS.
00040                  25  PI-SC-FIRST-NAME    PIC X(12).
00041                  25  PI-SC-INITIAL2      PIC X.
00042
00043          16  FILLER REDEFINES PI-SELECTION-CRITERIA.
00044              20  FILLER                  PIC X.
00045              20  PI-SC-SOC-SEC-NO        PIC X(11).
00046              20  FILLER                  PIC X(17).
00047
00048          16  FILLER REDEFINES PI-SELECTION-CRITERIA.
00049              20  FILLER                  PIC X.
00050              20  PI-SC-CERT-NO-A4.
00051                  25  PI-SC-CERT-PRIME-A4 PIC X(10).
00052                  25  PI-SC-CERT-SFX-A4   PIC X.
00053              20  FILLER                  PIC X(17).
00054
00055          16  FILLER REDEFINES PI-SELECTION-CRITERIA.
00056              20  FILLER                  PIC X.
00057              20  PI-SC-CCN-NO-A5         PIC X(20).
00058              20  FILLER                  PIC X(8).
00059
00060          16  PI-CLAIM-KEY.
00061              20  PI-CK-COMPANY-CD        PIC X.
00062              20  PI-CK-CARRIER           PIC X.
00063              20  PI-CK-CLAIM             PIC X(7).
00064              20  PI-CK-CERT-NO.
00065                  25  PI-CK-CERT-PRIME    PIC X(10).
00066                  25  PI-CK-CERT-SFX      PIC X.
00067              20  FILLER                  PIC X(09).
00068
00069          16  FILLER REDEFINES PI-CLAIM-KEY.
00070              20  FILLER                  PIC X.
00071              20  PI-CK-INSURED-LAST-NAME PIC X(15).
00072              20  PI-CK-INSURED-FRST-NAME PIC X(12).
00073              20  PI-CK-INSURED-MID-INIT  PIC X.
00074
00075          16  FILLER REDEFINES PI-CLAIM-KEY.
00076              20  FILLER                  PIC X.
00077              20  PI-CK-SOC-SEC-NO        PIC X(11).
00078              20  FILLER                  PIC X(17).
00079
00080          16  FILLER REDEFINES PI-CLAIM-KEY.
00081              20  FILLER                  PIC X.
00082              20  PI-CK-CERT-NO-A4.
00083                  25  PI-CK-CERT-PRIME-A4 PIC X(10).
00084                  25  PI-CK-CERT-SFX-A4   PIC X.
00085              20  FILLER                  PIC X(17).
00086
00087          16  FILLER REDEFINES PI-CLAIM-KEY.
00088              20  FILLER                  PIC X.
00089              20  PI-CK-CCN-NO-A5         PIC X(20).
00090              20  FILLER                  PIC X(8).
00091
00092          16  PI-LAST-EIBAID              PIC X.
00093          16  PI-SCREEN-COUNT             PIC S9(5)  COMP-3.
00094
00095          16  PI-KEY-LENGTH               PIC S9(4) COMP SYNC.
00096          16  PI-TS-ITEM                  PIC S9(4) COMP SYNC.
00097          16  PI-1ST-ALPH-KEY.
00098              20  PI-1ST-KEY            PIC X(29).
00099              20  FILLER                PIC X(15).
00100          16  PI-LAST-ALPH-KEY.
00101              20  PI-LAST-KEY           PIC X(29).
00102              20  FILLER                PIC X(15).
00103
00104          16  PI-SAVE-AREA            OCCURS 16 TIMES
00105              INDEXED BY PI-INDEX.
00106              20  PI-SA-STATE             PIC X(2).
00107              20  PI-SA-GROUP             PIC X(6).
00108              20  PI-SA-EFF-DATE          PIC X(2).
00109
00110          16  PI-LAST-NAME                PIC X(15).
00111          16  PI-ACCOUNT-NUMBER           PIC X(10).
00112          16  PI-CCN-NO                   PIC X(20).
00113          16  PI-CREDIT-CARD-INDEX        PIC X.
00114              88  CREDIT-CARD-INDEX            VALUE 'Y'.
00115
00116          16  PI-ALPH-CLAIM-KEY.
00117              20  PI-ALPH-CO-CD          PIC X(01).
00118              20  PI-ALPH-SOURCE         PIC X(01).
00119              20  PI-ALPH-NAME.
00120                  24  PI-ALPH-LAST-NAME  PIC X(15).
00121                  24  PI-ALPH-FRST-NAME.
00122                      28  PI-ALPH-F-INIT PIC X(01).
00123                      28  FILLER         PIC X(11).
00124                  24  PI-ALPH-MID-INIT   PIC X(01).
00125              20  PI-ALPH-DATE           PIC X(08).
00126              20  PI-ALPH-TIME           PIC S9(04)  COMP.
00127
00128          16  PI-ALPH-ADMIN-KEY.
00129              20  PI-ADM-COMP-CD        PIC X(01).
00130              20  PI-ADM-SOURCE         PIC X(01).
00131              20  PI-ADM-CARRIER        PIC X(01).
00132              20  PI-ADM-GROUPING.
00133                  24  PI-ADM-GRP-PREFIX PIC X(03).
00134                  24  PI-ADM-GRP-PRIME  PIC X(03).
00135              20  PI-ADM-STATE          PIC X(02).
00136              20  PI-ADM-PRODUCER.
00137                  24  PI-ADM-PROD-PRE   PIC X(04).
00138                  24  PI-ADM-PROD-PRM   PIC X(06).
00139              20  PI-ADM-CERT-EFF-DT    PIC X(02).
00140              20  PI-ADM-CERT-NUMBER.
00141                  24  PI-ADM-CERT-PRM   PIC X(10).
00142                  24  PI-ADM-CERT-SFX   PIC X(01).
00143              20  PI-ADM-DATE           PIC X(08).
00144              20  PI-ADM-TIME           PIC S9(04)  COMP.
00145
00146          16  P1-CLAIM-KEY   REDEFINES PI-ALPH-ADMIN-KEY.
00147              20  PI-CLM-COMP-CD        PIC X(01).
00148              20  PI-CLM-SOURCE         PIC X(01).
00149              20  PI-CLM-CARRIER        PIC X(01).
00150              20  PI-CLM-CLAIM-NO       PIC X(07).
00151              20  PI-CLM-CERT-NUMBER.
00152                  24  PI-CLM-CERT-PRM   PIC X(10).
00153                  24  PI-CLM-CERT-SFX   PIC X(01).
00154              20  PI-CLM-DATE           PIC X(08).
00155              20  PI-CLM-TIME           PIC S9(04)  COMP.
00156              20  FILLER                PIC X(13).
00157
00158          16  FILLER                    PIC X(02).
00204
00205      EJECT
00206 *                                COPY EL1325S.
       01  EL1325AI.
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
           05  AHEAD1L PIC S9(0004) COMP.
           05  AHEAD1F PIC  X(0001).
           05  FILLER REDEFINES AHEAD1F.
               10  AHEAD1A PIC  X(0001).
           05  AHEAD1I PIC  X(0038).
      *    -------------------------------
           05  ACOMPL PIC S9(0004) COMP.
           05  ACOMPF PIC  X(0001).
           05  FILLER REDEFINES ACOMPF.
               10  ACOMPA PIC  X(0001).
           05  ACOMPI PIC  X(0003).
      *    -------------------------------
           05  USERIDL PIC S9(0004) COMP.
           05  USERIDF PIC  X(0001).
           05  FILLER REDEFINES USERIDF.
               10  USERIDA PIC  X(0001).
           05  USERIDI PIC  X(0004).
      *    -------------------------------
           05  ANUM01L PIC S9(0004) COMP.
           05  ANUM01F PIC  X(0001).
           05  FILLER REDEFINES ANUM01F.
               10  ANUM01A PIC  X(0001).
           05  ANUM01I PIC  X(0002).
      *    -------------------------------
           05  ANAME01L PIC S9(0004) COMP.
           05  ANAME01F PIC  X(0001).
           05  FILLER REDEFINES ANAME01F.
               10  ANAME01A PIC  X(0001).
           05  ANAME01I PIC  X(0027).
      *    -------------------------------
           05  AAGE01L PIC S9(0004) COMP.
           05  AAGE01F PIC  X(0001).
           05  FILLER REDEFINES AAGE01F.
               10  AAGE01A PIC  X(0001).
           05  AAGE01I PIC  X(0002).
      *    -------------------------------
           05  ASTA01L PIC S9(0004) COMP.
           05  ASTA01F PIC  X(0001).
           05  FILLER REDEFINES ASTA01F.
               10  ASTA01A PIC  X(0001).
           05  ASTA01I PIC  X(0001).
      *    -------------------------------
           05  AIDT01L PIC S9(0004) COMP.
           05  AIDT01F PIC  X(0001).
           05  FILLER REDEFINES AIDT01F.
               10  AIDT01A PIC  X(0001).
           05  AIDT01I PIC  X(0008).
      *    -------------------------------
           05  ATYPE01L PIC S9(0004) COMP.
           05  ATYPE01F PIC  X(0001).
           05  FILLER REDEFINES ATYPE01F.
               10  ATYPE01A PIC  X(0001).
           05  ATYPE01I PIC  X(0001).
      *    -------------------------------
           05  ACARR01L PIC S9(0004) COMP.
           05  ACARR01F PIC  X(0001).
           05  FILLER REDEFINES ACARR01F.
               10  ACARR01A PIC  X(0001).
           05  ACARR01I PIC  X(0001).
      *    -------------------------------
           05  ACLAM01L PIC S9(0004) COMP.
           05  ACLAM01F PIC  X(0001).
           05  FILLER REDEFINES ACLAM01F.
               10  ACLAM01A PIC  X(0001).
           05  ACLAM01I PIC  X(0007).
      *    -------------------------------
           05  ACERT01L PIC S9(0004) COMP.
           05  ACERT01F PIC  X(0001).
           05  FILLER REDEFINES ACERT01F.
               10  ACERT01A PIC  X(0001).
           05  ACERT01I PIC  X(0011).
      *    -------------------------------
           05  AACCT01L PIC S9(0004) COMP.
           05  AACCT01F PIC  X(0001).
           05  FILLER REDEFINES AACCT01F.
               10  AACCT01A PIC  X(0001).
           05  AACCT01I PIC  X(0010).
      *    -------------------------------
           05  ANUM02L PIC S9(0004) COMP.
           05  ANUM02F PIC  X(0001).
           05  FILLER REDEFINES ANUM02F.
               10  ANUM02A PIC  X(0001).
           05  ANUM02I PIC  X(0002).
      *    -------------------------------
           05  ANAME02L PIC S9(0004) COMP.
           05  ANAME02F PIC  X(0001).
           05  FILLER REDEFINES ANAME02F.
               10  ANAME02A PIC  X(0001).
           05  ANAME02I PIC  X(0027).
      *    -------------------------------
           05  AAGE02L PIC S9(0004) COMP.
           05  AAGE02F PIC  X(0001).
           05  FILLER REDEFINES AAGE02F.
               10  AAGE02A PIC  X(0001).
           05  AAGE02I PIC  X(0002).
      *    -------------------------------
           05  ASTA02L PIC S9(0004) COMP.
           05  ASTA02F PIC  X(0001).
           05  FILLER REDEFINES ASTA02F.
               10  ASTA02A PIC  X(0001).
           05  ASTA02I PIC  X(0001).
      *    -------------------------------
           05  AIDT02L PIC S9(0004) COMP.
           05  AIDT02F PIC  X(0001).
           05  FILLER REDEFINES AIDT02F.
               10  AIDT02A PIC  X(0001).
           05  AIDT02I PIC  X(0008).
      *    -------------------------------
           05  ATYPE02L PIC S9(0004) COMP.
           05  ATYPE02F PIC  X(0001).
           05  FILLER REDEFINES ATYPE02F.
               10  ATYPE02A PIC  X(0001).
           05  ATYPE02I PIC  X(0001).
      *    -------------------------------
           05  ACARR02L PIC S9(0004) COMP.
           05  ACARR02F PIC  X(0001).
           05  FILLER REDEFINES ACARR02F.
               10  ACARR02A PIC  X(0001).
           05  ACARR02I PIC  X(0001).
      *    -------------------------------
           05  ACLAM02L PIC S9(0004) COMP.
           05  ACLAM02F PIC  X(0001).
           05  FILLER REDEFINES ACLAM02F.
               10  ACLAM02A PIC  X(0001).
           05  ACLAM02I PIC  X(0007).
      *    -------------------------------
           05  ACERT02L PIC S9(0004) COMP.
           05  ACERT02F PIC  X(0001).
           05  FILLER REDEFINES ACERT02F.
               10  ACERT02A PIC  X(0001).
           05  ACERT02I PIC  X(0011).
      *    -------------------------------
           05  AACCT02L PIC S9(0004) COMP.
           05  AACCT02F PIC  X(0001).
           05  FILLER REDEFINES AACCT02F.
               10  AACCT02A PIC  X(0001).
           05  AACCT02I PIC  X(0010).
      *    -------------------------------
           05  ANUM03L PIC S9(0004) COMP.
           05  ANUM03F PIC  X(0001).
           05  FILLER REDEFINES ANUM03F.
               10  ANUM03A PIC  X(0001).
           05  ANUM03I PIC  X(0002).
      *    -------------------------------
           05  ANAME03L PIC S9(0004) COMP.
           05  ANAME03F PIC  X(0001).
           05  FILLER REDEFINES ANAME03F.
               10  ANAME03A PIC  X(0001).
           05  ANAME03I PIC  X(0027).
      *    -------------------------------
           05  AAGE03L PIC S9(0004) COMP.
           05  AAGE03F PIC  X(0001).
           05  FILLER REDEFINES AAGE03F.
               10  AAGE03A PIC  X(0001).
           05  AAGE03I PIC  X(0002).
      *    -------------------------------
           05  ASTA03L PIC S9(0004) COMP.
           05  ASTA03F PIC  X(0001).
           05  FILLER REDEFINES ASTA03F.
               10  ASTA03A PIC  X(0001).
           05  ASTA03I PIC  X(0001).
      *    -------------------------------
           05  AIDT03L PIC S9(0004) COMP.
           05  AIDT03F PIC  X(0001).
           05  FILLER REDEFINES AIDT03F.
               10  AIDT03A PIC  X(0001).
           05  AIDT03I PIC  X(0008).
      *    -------------------------------
           05  ATYPE03L PIC S9(0004) COMP.
           05  ATYPE03F PIC  X(0001).
           05  FILLER REDEFINES ATYPE03F.
               10  ATYPE03A PIC  X(0001).
           05  ATYPE03I PIC  X(0001).
      *    -------------------------------
           05  ACARR03L PIC S9(0004) COMP.
           05  ACARR03F PIC  X(0001).
           05  FILLER REDEFINES ACARR03F.
               10  ACARR03A PIC  X(0001).
           05  ACARR03I PIC  X(0001).
      *    -------------------------------
           05  ACLAM03L PIC S9(0004) COMP.
           05  ACLAM03F PIC  X(0001).
           05  FILLER REDEFINES ACLAM03F.
               10  ACLAM03A PIC  X(0001).
           05  ACLAM03I PIC  X(0007).
      *    -------------------------------
           05  ACERT03L PIC S9(0004) COMP.
           05  ACERT03F PIC  X(0001).
           05  FILLER REDEFINES ACERT03F.
               10  ACERT03A PIC  X(0001).
           05  ACERT03I PIC  X(0011).
      *    -------------------------------
           05  AACCT03L PIC S9(0004) COMP.
           05  AACCT03F PIC  X(0001).
           05  FILLER REDEFINES AACCT03F.
               10  AACCT03A PIC  X(0001).
           05  AACCT03I PIC  X(0010).
      *    -------------------------------
           05  ANUM04L PIC S9(0004) COMP.
           05  ANUM04F PIC  X(0001).
           05  FILLER REDEFINES ANUM04F.
               10  ANUM04A PIC  X(0001).
           05  ANUM04I PIC  X(0002).
      *    -------------------------------
           05  ANAME04L PIC S9(0004) COMP.
           05  ANAME04F PIC  X(0001).
           05  FILLER REDEFINES ANAME04F.
               10  ANAME04A PIC  X(0001).
           05  ANAME04I PIC  X(0027).
      *    -------------------------------
           05  AAGE04L PIC S9(0004) COMP.
           05  AAGE04F PIC  X(0001).
           05  FILLER REDEFINES AAGE04F.
               10  AAGE04A PIC  X(0001).
           05  AAGE04I PIC  X(0002).
      *    -------------------------------
           05  ASTA04L PIC S9(0004) COMP.
           05  ASTA04F PIC  X(0001).
           05  FILLER REDEFINES ASTA04F.
               10  ASTA04A PIC  X(0001).
           05  ASTA04I PIC  X(0001).
      *    -------------------------------
           05  AIDT04L PIC S9(0004) COMP.
           05  AIDT04F PIC  X(0001).
           05  FILLER REDEFINES AIDT04F.
               10  AIDT04A PIC  X(0001).
           05  AIDT04I PIC  X(0008).
      *    -------------------------------
           05  ATYPE04L PIC S9(0004) COMP.
           05  ATYPE04F PIC  X(0001).
           05  FILLER REDEFINES ATYPE04F.
               10  ATYPE04A PIC  X(0001).
           05  ATYPE04I PIC  X(0001).
      *    -------------------------------
           05  ACARR04L PIC S9(0004) COMP.
           05  ACARR04F PIC  X(0001).
           05  FILLER REDEFINES ACARR04F.
               10  ACARR04A PIC  X(0001).
           05  ACARR04I PIC  X(0001).
      *    -------------------------------
           05  ACLAM04L PIC S9(0004) COMP.
           05  ACLAM04F PIC  X(0001).
           05  FILLER REDEFINES ACLAM04F.
               10  ACLAM04A PIC  X(0001).
           05  ACLAM04I PIC  X(0007).
      *    -------------------------------
           05  ACERT04L PIC S9(0004) COMP.
           05  ACERT04F PIC  X(0001).
           05  FILLER REDEFINES ACERT04F.
               10  ACERT04A PIC  X(0001).
           05  ACERT04I PIC  X(0011).
      *    -------------------------------
           05  AACCT04L PIC S9(0004) COMP.
           05  AACCT04F PIC  X(0001).
           05  FILLER REDEFINES AACCT04F.
               10  AACCT04A PIC  X(0001).
           05  AACCT04I PIC  X(0010).
      *    -------------------------------
           05  ANUM05L PIC S9(0004) COMP.
           05  ANUM05F PIC  X(0001).
           05  FILLER REDEFINES ANUM05F.
               10  ANUM05A PIC  X(0001).
           05  ANUM05I PIC  X(0002).
      *    -------------------------------
           05  ANAME05L PIC S9(0004) COMP.
           05  ANAME05F PIC  X(0001).
           05  FILLER REDEFINES ANAME05F.
               10  ANAME05A PIC  X(0001).
           05  ANAME05I PIC  X(0027).
      *    -------------------------------
           05  AAGE05L PIC S9(0004) COMP.
           05  AAGE05F PIC  X(0001).
           05  FILLER REDEFINES AAGE05F.
               10  AAGE05A PIC  X(0001).
           05  AAGE05I PIC  X(0002).
      *    -------------------------------
           05  ASTA05L PIC S9(0004) COMP.
           05  ASTA05F PIC  X(0001).
           05  FILLER REDEFINES ASTA05F.
               10  ASTA05A PIC  X(0001).
           05  ASTA05I PIC  X(0001).
      *    -------------------------------
           05  AIDT05L PIC S9(0004) COMP.
           05  AIDT05F PIC  X(0001).
           05  FILLER REDEFINES AIDT05F.
               10  AIDT05A PIC  X(0001).
           05  AIDT05I PIC  X(0008).
      *    -------------------------------
           05  ATYPE05L PIC S9(0004) COMP.
           05  ATYPE05F PIC  X(0001).
           05  FILLER REDEFINES ATYPE05F.
               10  ATYPE05A PIC  X(0001).
           05  ATYPE05I PIC  X(0001).
      *    -------------------------------
           05  ACARR05L PIC S9(0004) COMP.
           05  ACARR05F PIC  X(0001).
           05  FILLER REDEFINES ACARR05F.
               10  ACARR05A PIC  X(0001).
           05  ACARR05I PIC  X(0001).
      *    -------------------------------
           05  ACLAM05L PIC S9(0004) COMP.
           05  ACLAM05F PIC  X(0001).
           05  FILLER REDEFINES ACLAM05F.
               10  ACLAM05A PIC  X(0001).
           05  ACLAM05I PIC  X(0007).
      *    -------------------------------
           05  ACERT05L PIC S9(0004) COMP.
           05  ACERT05F PIC  X(0001).
           05  FILLER REDEFINES ACERT05F.
               10  ACERT05A PIC  X(0001).
           05  ACERT05I PIC  X(0011).
      *    -------------------------------
           05  AACCT05L PIC S9(0004) COMP.
           05  AACCT05F PIC  X(0001).
           05  FILLER REDEFINES AACCT05F.
               10  AACCT05A PIC  X(0001).
           05  AACCT05I PIC  X(0010).
      *    -------------------------------
           05  ANUM06L PIC S9(0004) COMP.
           05  ANUM06F PIC  X(0001).
           05  FILLER REDEFINES ANUM06F.
               10  ANUM06A PIC  X(0001).
           05  ANUM06I PIC  X(0002).
      *    -------------------------------
           05  ANAME06L PIC S9(0004) COMP.
           05  ANAME06F PIC  X(0001).
           05  FILLER REDEFINES ANAME06F.
               10  ANAME06A PIC  X(0001).
           05  ANAME06I PIC  X(0027).
      *    -------------------------------
           05  AAGE06L PIC S9(0004) COMP.
           05  AAGE06F PIC  X(0001).
           05  FILLER REDEFINES AAGE06F.
               10  AAGE06A PIC  X(0001).
           05  AAGE06I PIC  X(0002).
      *    -------------------------------
           05  ASTA06L PIC S9(0004) COMP.
           05  ASTA06F PIC  X(0001).
           05  FILLER REDEFINES ASTA06F.
               10  ASTA06A PIC  X(0001).
           05  ASTA06I PIC  X(0001).
      *    -------------------------------
           05  AIDT06L PIC S9(0004) COMP.
           05  AIDT06F PIC  X(0001).
           05  FILLER REDEFINES AIDT06F.
               10  AIDT06A PIC  X(0001).
           05  AIDT06I PIC  X(0008).
      *    -------------------------------
           05  ATYPE06L PIC S9(0004) COMP.
           05  ATYPE06F PIC  X(0001).
           05  FILLER REDEFINES ATYPE06F.
               10  ATYPE06A PIC  X(0001).
           05  ATYPE06I PIC  X(0001).
      *    -------------------------------
           05  ACARR06L PIC S9(0004) COMP.
           05  ACARR06F PIC  X(0001).
           05  FILLER REDEFINES ACARR06F.
               10  ACARR06A PIC  X(0001).
           05  ACARR06I PIC  X(0001).
      *    -------------------------------
           05  ACLAM06L PIC S9(0004) COMP.
           05  ACLAM06F PIC  X(0001).
           05  FILLER REDEFINES ACLAM06F.
               10  ACLAM06A PIC  X(0001).
           05  ACLAM06I PIC  X(0007).
      *    -------------------------------
           05  ACERT06L PIC S9(0004) COMP.
           05  ACERT06F PIC  X(0001).
           05  FILLER REDEFINES ACERT06F.
               10  ACERT06A PIC  X(0001).
           05  ACERT06I PIC  X(0011).
      *    -------------------------------
           05  AACCT06L PIC S9(0004) COMP.
           05  AACCT06F PIC  X(0001).
           05  FILLER REDEFINES AACCT06F.
               10  AACCT06A PIC  X(0001).
           05  AACCT06I PIC  X(0010).
      *    -------------------------------
           05  ANUM07L PIC S9(0004) COMP.
           05  ANUM07F PIC  X(0001).
           05  FILLER REDEFINES ANUM07F.
               10  ANUM07A PIC  X(0001).
           05  ANUM07I PIC  X(0002).
      *    -------------------------------
           05  ANAME07L PIC S9(0004) COMP.
           05  ANAME07F PIC  X(0001).
           05  FILLER REDEFINES ANAME07F.
               10  ANAME07A PIC  X(0001).
           05  ANAME07I PIC  X(0027).
      *    -------------------------------
           05  AAGE07L PIC S9(0004) COMP.
           05  AAGE07F PIC  X(0001).
           05  FILLER REDEFINES AAGE07F.
               10  AAGE07A PIC  X(0001).
           05  AAGE07I PIC  X(0002).
      *    -------------------------------
           05  ASTA07L PIC S9(0004) COMP.
           05  ASTA07F PIC  X(0001).
           05  FILLER REDEFINES ASTA07F.
               10  ASTA07A PIC  X(0001).
           05  ASTA07I PIC  X(0001).
      *    -------------------------------
           05  AIDT07L PIC S9(0004) COMP.
           05  AIDT07F PIC  X(0001).
           05  FILLER REDEFINES AIDT07F.
               10  AIDT07A PIC  X(0001).
           05  AIDT07I PIC  X(0008).
      *    -------------------------------
           05  ATYPE07L PIC S9(0004) COMP.
           05  ATYPE07F PIC  X(0001).
           05  FILLER REDEFINES ATYPE07F.
               10  ATYPE07A PIC  X(0001).
           05  ATYPE07I PIC  X(0001).
      *    -------------------------------
           05  ACARR07L PIC S9(0004) COMP.
           05  ACARR07F PIC  X(0001).
           05  FILLER REDEFINES ACARR07F.
               10  ACARR07A PIC  X(0001).
           05  ACARR07I PIC  X(0001).
      *    -------------------------------
           05  ACLAM07L PIC S9(0004) COMP.
           05  ACLAM07F PIC  X(0001).
           05  FILLER REDEFINES ACLAM07F.
               10  ACLAM07A PIC  X(0001).
           05  ACLAM07I PIC  X(0007).
      *    -------------------------------
           05  ACERT07L PIC S9(0004) COMP.
           05  ACERT07F PIC  X(0001).
           05  FILLER REDEFINES ACERT07F.
               10  ACERT07A PIC  X(0001).
           05  ACERT07I PIC  X(0011).
      *    -------------------------------
           05  AACCT07L PIC S9(0004) COMP.
           05  AACCT07F PIC  X(0001).
           05  FILLER REDEFINES AACCT07F.
               10  AACCT07A PIC  X(0001).
           05  AACCT07I PIC  X(0010).
      *    -------------------------------
           05  ANUM08L PIC S9(0004) COMP.
           05  ANUM08F PIC  X(0001).
           05  FILLER REDEFINES ANUM08F.
               10  ANUM08A PIC  X(0001).
           05  ANUM08I PIC  X(0002).
      *    -------------------------------
           05  ANAME08L PIC S9(0004) COMP.
           05  ANAME08F PIC  X(0001).
           05  FILLER REDEFINES ANAME08F.
               10  ANAME08A PIC  X(0001).
           05  ANAME08I PIC  X(0027).
      *    -------------------------------
           05  AAGE08L PIC S9(0004) COMP.
           05  AAGE08F PIC  X(0001).
           05  FILLER REDEFINES AAGE08F.
               10  AAGE08A PIC  X(0001).
           05  AAGE08I PIC  X(0002).
      *    -------------------------------
           05  ASTA08L PIC S9(0004) COMP.
           05  ASTA08F PIC  X(0001).
           05  FILLER REDEFINES ASTA08F.
               10  ASTA08A PIC  X(0001).
           05  ASTA08I PIC  X(0001).
      *    -------------------------------
           05  AIDT08L PIC S9(0004) COMP.
           05  AIDT08F PIC  X(0001).
           05  FILLER REDEFINES AIDT08F.
               10  AIDT08A PIC  X(0001).
           05  AIDT08I PIC  X(0008).
      *    -------------------------------
           05  ATYPE08L PIC S9(0004) COMP.
           05  ATYPE08F PIC  X(0001).
           05  FILLER REDEFINES ATYPE08F.
               10  ATYPE08A PIC  X(0001).
           05  ATYPE08I PIC  X(0001).
      *    -------------------------------
           05  ACARR08L PIC S9(0004) COMP.
           05  ACARR08F PIC  X(0001).
           05  FILLER REDEFINES ACARR08F.
               10  ACARR08A PIC  X(0001).
           05  ACARR08I PIC  X(0001).
      *    -------------------------------
           05  ACLAM08L PIC S9(0004) COMP.
           05  ACLAM08F PIC  X(0001).
           05  FILLER REDEFINES ACLAM08F.
               10  ACLAM08A PIC  X(0001).
           05  ACLAM08I PIC  X(0007).
      *    -------------------------------
           05  ACERT08L PIC S9(0004) COMP.
           05  ACERT08F PIC  X(0001).
           05  FILLER REDEFINES ACERT08F.
               10  ACERT08A PIC  X(0001).
           05  ACERT08I PIC  X(0011).
      *    -------------------------------
           05  AACCT08L PIC S9(0004) COMP.
           05  AACCT08F PIC  X(0001).
           05  FILLER REDEFINES AACCT08F.
               10  AACCT08A PIC  X(0001).
           05  AACCT08I PIC  X(0010).
      *    -------------------------------
           05  ANUM09L PIC S9(0004) COMP.
           05  ANUM09F PIC  X(0001).
           05  FILLER REDEFINES ANUM09F.
               10  ANUM09A PIC  X(0001).
           05  ANUM09I PIC  X(0002).
      *    -------------------------------
           05  ANAME09L PIC S9(0004) COMP.
           05  ANAME09F PIC  X(0001).
           05  FILLER REDEFINES ANAME09F.
               10  ANAME09A PIC  X(0001).
           05  ANAME09I PIC  X(0027).
      *    -------------------------------
           05  AAGE09L PIC S9(0004) COMP.
           05  AAGE09F PIC  X(0001).
           05  FILLER REDEFINES AAGE09F.
               10  AAGE09A PIC  X(0001).
           05  AAGE09I PIC  X(0002).
      *    -------------------------------
           05  ASTA09L PIC S9(0004) COMP.
           05  ASTA09F PIC  X(0001).
           05  FILLER REDEFINES ASTA09F.
               10  ASTA09A PIC  X(0001).
           05  ASTA09I PIC  X(0001).
      *    -------------------------------
           05  AIDT09L PIC S9(0004) COMP.
           05  AIDT09F PIC  X(0001).
           05  FILLER REDEFINES AIDT09F.
               10  AIDT09A PIC  X(0001).
           05  AIDT09I PIC  X(0008).
      *    -------------------------------
           05  ATYPE09L PIC S9(0004) COMP.
           05  ATYPE09F PIC  X(0001).
           05  FILLER REDEFINES ATYPE09F.
               10  ATYPE09A PIC  X(0001).
           05  ATYPE09I PIC  X(0001).
      *    -------------------------------
           05  ACARR09L PIC S9(0004) COMP.
           05  ACARR09F PIC  X(0001).
           05  FILLER REDEFINES ACARR09F.
               10  ACARR09A PIC  X(0001).
           05  ACARR09I PIC  X(0001).
      *    -------------------------------
           05  ACLAM09L PIC S9(0004) COMP.
           05  ACLAM09F PIC  X(0001).
           05  FILLER REDEFINES ACLAM09F.
               10  ACLAM09A PIC  X(0001).
           05  ACLAM09I PIC  X(0007).
      *    -------------------------------
           05  ACERT09L PIC S9(0004) COMP.
           05  ACERT09F PIC  X(0001).
           05  FILLER REDEFINES ACERT09F.
               10  ACERT09A PIC  X(0001).
           05  ACERT09I PIC  X(0011).
      *    -------------------------------
           05  AACCT09L PIC S9(0004) COMP.
           05  AACCT09F PIC  X(0001).
           05  FILLER REDEFINES AACCT09F.
               10  AACCT09A PIC  X(0001).
           05  AACCT09I PIC  X(0010).
      *    -------------------------------
           05  ANUM10L PIC S9(0004) COMP.
           05  ANUM10F PIC  X(0001).
           05  FILLER REDEFINES ANUM10F.
               10  ANUM10A PIC  X(0001).
           05  ANUM10I PIC  X(0002).
      *    -------------------------------
           05  ANAME10L PIC S9(0004) COMP.
           05  ANAME10F PIC  X(0001).
           05  FILLER REDEFINES ANAME10F.
               10  ANAME10A PIC  X(0001).
           05  ANAME10I PIC  X(0027).
      *    -------------------------------
           05  AAGE10L PIC S9(0004) COMP.
           05  AAGE10F PIC  X(0001).
           05  FILLER REDEFINES AAGE10F.
               10  AAGE10A PIC  X(0001).
           05  AAGE10I PIC  X(0002).
      *    -------------------------------
           05  ASTA10L PIC S9(0004) COMP.
           05  ASTA10F PIC  X(0001).
           05  FILLER REDEFINES ASTA10F.
               10  ASTA10A PIC  X(0001).
           05  ASTA10I PIC  X(0001).
      *    -------------------------------
           05  AIDT10L PIC S9(0004) COMP.
           05  AIDT10F PIC  X(0001).
           05  FILLER REDEFINES AIDT10F.
               10  AIDT10A PIC  X(0001).
           05  AIDT10I PIC  X(0008).
      *    -------------------------------
           05  ATYPE10L PIC S9(0004) COMP.
           05  ATYPE10F PIC  X(0001).
           05  FILLER REDEFINES ATYPE10F.
               10  ATYPE10A PIC  X(0001).
           05  ATYPE10I PIC  X(0001).
      *    -------------------------------
           05  ACARR10L PIC S9(0004) COMP.
           05  ACARR10F PIC  X(0001).
           05  FILLER REDEFINES ACARR10F.
               10  ACARR10A PIC  X(0001).
           05  ACARR10I PIC  X(0001).
      *    -------------------------------
           05  ACLAM10L PIC S9(0004) COMP.
           05  ACLAM10F PIC  X(0001).
           05  FILLER REDEFINES ACLAM10F.
               10  ACLAM10A PIC  X(0001).
           05  ACLAM10I PIC  X(0007).
      *    -------------------------------
           05  ACERT10L PIC S9(0004) COMP.
           05  ACERT10F PIC  X(0001).
           05  FILLER REDEFINES ACERT10F.
               10  ACERT10A PIC  X(0001).
           05  ACERT10I PIC  X(0011).
      *    -------------------------------
           05  AACCT10L PIC S9(0004) COMP.
           05  AACCT10F PIC  X(0001).
           05  FILLER REDEFINES AACCT10F.
               10  AACCT10A PIC  X(0001).
           05  AACCT10I PIC  X(0010).
      *    -------------------------------
           05  ANUM11L PIC S9(0004) COMP.
           05  ANUM11F PIC  X(0001).
           05  FILLER REDEFINES ANUM11F.
               10  ANUM11A PIC  X(0001).
           05  ANUM11I PIC  X(0002).
      *    -------------------------------
           05  ANAME11L PIC S9(0004) COMP.
           05  ANAME11F PIC  X(0001).
           05  FILLER REDEFINES ANAME11F.
               10  ANAME11A PIC  X(0001).
           05  ANAME11I PIC  X(0027).
      *    -------------------------------
           05  AAGE11L PIC S9(0004) COMP.
           05  AAGE11F PIC  X(0001).
           05  FILLER REDEFINES AAGE11F.
               10  AAGE11A PIC  X(0001).
           05  AAGE11I PIC  X(0002).
      *    -------------------------------
           05  ASTA11L PIC S9(0004) COMP.
           05  ASTA11F PIC  X(0001).
           05  FILLER REDEFINES ASTA11F.
               10  ASTA11A PIC  X(0001).
           05  ASTA11I PIC  X(0001).
      *    -------------------------------
           05  AIDT11L PIC S9(0004) COMP.
           05  AIDT11F PIC  X(0001).
           05  FILLER REDEFINES AIDT11F.
               10  AIDT11A PIC  X(0001).
           05  AIDT11I PIC  X(0008).
      *    -------------------------------
           05  ATYPE11L PIC S9(0004) COMP.
           05  ATYPE11F PIC  X(0001).
           05  FILLER REDEFINES ATYPE11F.
               10  ATYPE11A PIC  X(0001).
           05  ATYPE11I PIC  X(0001).
      *    -------------------------------
           05  ACARR11L PIC S9(0004) COMP.
           05  ACARR11F PIC  X(0001).
           05  FILLER REDEFINES ACARR11F.
               10  ACARR11A PIC  X(0001).
           05  ACARR11I PIC  X(0001).
      *    -------------------------------
           05  ACLAM11L PIC S9(0004) COMP.
           05  ACLAM11F PIC  X(0001).
           05  FILLER REDEFINES ACLAM11F.
               10  ACLAM11A PIC  X(0001).
           05  ACLAM11I PIC  X(0007).
      *    -------------------------------
           05  ACERT11L PIC S9(0004) COMP.
           05  ACERT11F PIC  X(0001).
           05  FILLER REDEFINES ACERT11F.
               10  ACERT11A PIC  X(0001).
           05  ACERT11I PIC  X(0011).
      *    -------------------------------
           05  AACCT11L PIC S9(0004) COMP.
           05  AACCT11F PIC  X(0001).
           05  FILLER REDEFINES AACCT11F.
               10  AACCT11A PIC  X(0001).
           05  AACCT11I PIC  X(0010).
      *    -------------------------------
           05  ANUM12L PIC S9(0004) COMP.
           05  ANUM12F PIC  X(0001).
           05  FILLER REDEFINES ANUM12F.
               10  ANUM12A PIC  X(0001).
           05  ANUM12I PIC  X(0002).
      *    -------------------------------
           05  ANAME12L PIC S9(0004) COMP.
           05  ANAME12F PIC  X(0001).
           05  FILLER REDEFINES ANAME12F.
               10  ANAME12A PIC  X(0001).
           05  ANAME12I PIC  X(0027).
      *    -------------------------------
           05  AAGE12L PIC S9(0004) COMP.
           05  AAGE12F PIC  X(0001).
           05  FILLER REDEFINES AAGE12F.
               10  AAGE12A PIC  X(0001).
           05  AAGE12I PIC  X(0002).
      *    -------------------------------
           05  ASTA12L PIC S9(0004) COMP.
           05  ASTA12F PIC  X(0001).
           05  FILLER REDEFINES ASTA12F.
               10  ASTA12A PIC  X(0001).
           05  ASTA12I PIC  X(0001).
      *    -------------------------------
           05  AIDT12L PIC S9(0004) COMP.
           05  AIDT12F PIC  X(0001).
           05  FILLER REDEFINES AIDT12F.
               10  AIDT12A PIC  X(0001).
           05  AIDT12I PIC  X(0008).
      *    -------------------------------
           05  ATYPE12L PIC S9(0004) COMP.
           05  ATYPE12F PIC  X(0001).
           05  FILLER REDEFINES ATYPE12F.
               10  ATYPE12A PIC  X(0001).
           05  ATYPE12I PIC  X(0001).
      *    -------------------------------
           05  ACARR12L PIC S9(0004) COMP.
           05  ACARR12F PIC  X(0001).
           05  FILLER REDEFINES ACARR12F.
               10  ACARR12A PIC  X(0001).
           05  ACARR12I PIC  X(0001).
      *    -------------------------------
           05  ACLAM12L PIC S9(0004) COMP.
           05  ACLAM12F PIC  X(0001).
           05  FILLER REDEFINES ACLAM12F.
               10  ACLAM12A PIC  X(0001).
           05  ACLAM12I PIC  X(0007).
      *    -------------------------------
           05  ACERT12L PIC S9(0004) COMP.
           05  ACERT12F PIC  X(0001).
           05  FILLER REDEFINES ACERT12F.
               10  ACERT12A PIC  X(0001).
           05  ACERT12I PIC  X(0011).
      *    -------------------------------
           05  AACCT12L PIC S9(0004) COMP.
           05  AACCT12F PIC  X(0001).
           05  FILLER REDEFINES AACCT12F.
               10  AACCT12A PIC  X(0001).
           05  AACCT12I PIC  X(0010).
      *    -------------------------------
           05  ANUM13L PIC S9(0004) COMP.
           05  ANUM13F PIC  X(0001).
           05  FILLER REDEFINES ANUM13F.
               10  ANUM13A PIC  X(0001).
           05  ANUM13I PIC  X(0002).
      *    -------------------------------
           05  ANAME13L PIC S9(0004) COMP.
           05  ANAME13F PIC  X(0001).
           05  FILLER REDEFINES ANAME13F.
               10  ANAME13A PIC  X(0001).
           05  ANAME13I PIC  X(0027).
      *    -------------------------------
           05  AAGE13L PIC S9(0004) COMP.
           05  AAGE13F PIC  X(0001).
           05  FILLER REDEFINES AAGE13F.
               10  AAGE13A PIC  X(0001).
           05  AAGE13I PIC  X(0002).
      *    -------------------------------
           05  ASTA13L PIC S9(0004) COMP.
           05  ASTA13F PIC  X(0001).
           05  FILLER REDEFINES ASTA13F.
               10  ASTA13A PIC  X(0001).
           05  ASTA13I PIC  X(0001).
      *    -------------------------------
           05  AIDT13L PIC S9(0004) COMP.
           05  AIDT13F PIC  X(0001).
           05  FILLER REDEFINES AIDT13F.
               10  AIDT13A PIC  X(0001).
           05  AIDT13I PIC  X(0008).
      *    -------------------------------
           05  ATYPE13L PIC S9(0004) COMP.
           05  ATYPE13F PIC  X(0001).
           05  FILLER REDEFINES ATYPE13F.
               10  ATYPE13A PIC  X(0001).
           05  ATYPE13I PIC  X(0001).
      *    -------------------------------
           05  ACARR13L PIC S9(0004) COMP.
           05  ACARR13F PIC  X(0001).
           05  FILLER REDEFINES ACARR13F.
               10  ACARR13A PIC  X(0001).
           05  ACARR13I PIC  X(0001).
      *    -------------------------------
           05  ACLAM13L PIC S9(0004) COMP.
           05  ACLAM13F PIC  X(0001).
           05  FILLER REDEFINES ACLAM13F.
               10  ACLAM13A PIC  X(0001).
           05  ACLAM13I PIC  X(0007).
      *    -------------------------------
           05  ACERT13L PIC S9(0004) COMP.
           05  ACERT13F PIC  X(0001).
           05  FILLER REDEFINES ACERT13F.
               10  ACERT13A PIC  X(0001).
           05  ACERT13I PIC  X(0011).
      *    -------------------------------
           05  AACCT13L PIC S9(0004) COMP.
           05  AACCT13F PIC  X(0001).
           05  FILLER REDEFINES AACCT13F.
               10  AACCT13A PIC  X(0001).
           05  AACCT13I PIC  X(0010).
      *    -------------------------------
           05  ANUM14L PIC S9(0004) COMP.
           05  ANUM14F PIC  X(0001).
           05  FILLER REDEFINES ANUM14F.
               10  ANUM14A PIC  X(0001).
           05  ANUM14I PIC  X(0002).
      *    -------------------------------
           05  ANAME14L PIC S9(0004) COMP.
           05  ANAME14F PIC  X(0001).
           05  FILLER REDEFINES ANAME14F.
               10  ANAME14A PIC  X(0001).
           05  ANAME14I PIC  X(0027).
      *    -------------------------------
           05  AAGE14L PIC S9(0004) COMP.
           05  AAGE14F PIC  X(0001).
           05  FILLER REDEFINES AAGE14F.
               10  AAGE14A PIC  X(0001).
           05  AAGE14I PIC  X(0002).
      *    -------------------------------
           05  ASTA14L PIC S9(0004) COMP.
           05  ASTA14F PIC  X(0001).
           05  FILLER REDEFINES ASTA14F.
               10  ASTA14A PIC  X(0001).
           05  ASTA14I PIC  X(0001).
      *    -------------------------------
           05  AIDT14L PIC S9(0004) COMP.
           05  AIDT14F PIC  X(0001).
           05  FILLER REDEFINES AIDT14F.
               10  AIDT14A PIC  X(0001).
           05  AIDT14I PIC  X(0008).
      *    -------------------------------
           05  ATYPE14L PIC S9(0004) COMP.
           05  ATYPE14F PIC  X(0001).
           05  FILLER REDEFINES ATYPE14F.
               10  ATYPE14A PIC  X(0001).
           05  ATYPE14I PIC  X(0001).
      *    -------------------------------
           05  ACARR14L PIC S9(0004) COMP.
           05  ACARR14F PIC  X(0001).
           05  FILLER REDEFINES ACARR14F.
               10  ACARR14A PIC  X(0001).
           05  ACARR14I PIC  X(0001).
      *    -------------------------------
           05  ACLAM14L PIC S9(0004) COMP.
           05  ACLAM14F PIC  X(0001).
           05  FILLER REDEFINES ACLAM14F.
               10  ACLAM14A PIC  X(0001).
           05  ACLAM14I PIC  X(0007).
      *    -------------------------------
           05  ACERT14L PIC S9(0004) COMP.
           05  ACERT14F PIC  X(0001).
           05  FILLER REDEFINES ACERT14F.
               10  ACERT14A PIC  X(0001).
           05  ACERT14I PIC  X(0011).
      *    -------------------------------
           05  AACCT14L PIC S9(0004) COMP.
           05  AACCT14F PIC  X(0001).
           05  FILLER REDEFINES AACCT14F.
               10  AACCT14A PIC  X(0001).
           05  AACCT14I PIC  X(0010).
      *    -------------------------------
           05  ANUM15L PIC S9(0004) COMP.
           05  ANUM15F PIC  X(0001).
           05  FILLER REDEFINES ANUM15F.
               10  ANUM15A PIC  X(0001).
           05  ANUM15I PIC  X(0002).
      *    -------------------------------
           05  ANAME15L PIC S9(0004) COMP.
           05  ANAME15F PIC  X(0001).
           05  FILLER REDEFINES ANAME15F.
               10  ANAME15A PIC  X(0001).
           05  ANAME15I PIC  X(0027).
      *    -------------------------------
           05  AAGE15L PIC S9(0004) COMP.
           05  AAGE15F PIC  X(0001).
           05  FILLER REDEFINES AAGE15F.
               10  AAGE15A PIC  X(0001).
           05  AAGE15I PIC  X(0002).
      *    -------------------------------
           05  ASTA15L PIC S9(0004) COMP.
           05  ASTA15F PIC  X(0001).
           05  FILLER REDEFINES ASTA15F.
               10  ASTA15A PIC  X(0001).
           05  ASTA15I PIC  X(0001).
      *    -------------------------------
           05  AIDT15L PIC S9(0004) COMP.
           05  AIDT15F PIC  X(0001).
           05  FILLER REDEFINES AIDT15F.
               10  AIDT15A PIC  X(0001).
           05  AIDT15I PIC  X(0008).
      *    -------------------------------
           05  ATYPE15L PIC S9(0004) COMP.
           05  ATYPE15F PIC  X(0001).
           05  FILLER REDEFINES ATYPE15F.
               10  ATYPE15A PIC  X(0001).
           05  ATYPE15I PIC  X(0001).
      *    -------------------------------
           05  ACARR15L PIC S9(0004) COMP.
           05  ACARR15F PIC  X(0001).
           05  FILLER REDEFINES ACARR15F.
               10  ACARR15A PIC  X(0001).
           05  ACARR15I PIC  X(0001).
      *    -------------------------------
           05  ACLAM15L PIC S9(0004) COMP.
           05  ACLAM15F PIC  X(0001).
           05  FILLER REDEFINES ACLAM15F.
               10  ACLAM15A PIC  X(0001).
           05  ACLAM15I PIC  X(0007).
      *    -------------------------------
           05  ACERT15L PIC S9(0004) COMP.
           05  ACERT15F PIC  X(0001).
           05  FILLER REDEFINES ACERT15F.
               10  ACERT15A PIC  X(0001).
           05  ACERT15I PIC  X(0011).
      *    -------------------------------
           05  AACCT15L PIC S9(0004) COMP.
           05  AACCT15F PIC  X(0001).
           05  FILLER REDEFINES AACCT15F.
               10  AACCT15A PIC  X(0001).
           05  AACCT15I PIC  X(0010).
      *    -------------------------------
           05  ANUM16L PIC S9(0004) COMP.
           05  ANUM16F PIC  X(0001).
           05  FILLER REDEFINES ANUM16F.
               10  ANUM16A PIC  X(0001).
           05  ANUM16I PIC  X(0002).
      *    -------------------------------
           05  ANAME16L PIC S9(0004) COMP.
           05  ANAME16F PIC  X(0001).
           05  FILLER REDEFINES ANAME16F.
               10  ANAME16A PIC  X(0001).
           05  ANAME16I PIC  X(0027).
      *    -------------------------------
           05  AAGE16L PIC S9(0004) COMP.
           05  AAGE16F PIC  X(0001).
           05  FILLER REDEFINES AAGE16F.
               10  AAGE16A PIC  X(0001).
           05  AAGE16I PIC  X(0002).
      *    -------------------------------
           05  ASTA16L PIC S9(0004) COMP.
           05  ASTA16F PIC  X(0001).
           05  FILLER REDEFINES ASTA16F.
               10  ASTA16A PIC  X(0001).
           05  ASTA16I PIC  X(0001).
      *    -------------------------------
           05  AIDT16L PIC S9(0004) COMP.
           05  AIDT16F PIC  X(0001).
           05  FILLER REDEFINES AIDT16F.
               10  AIDT16A PIC  X(0001).
           05  AIDT16I PIC  X(0008).
      *    -------------------------------
           05  ATYPE16L PIC S9(0004) COMP.
           05  ATYPE16F PIC  X(0001).
           05  FILLER REDEFINES ATYPE16F.
               10  ATYPE16A PIC  X(0001).
           05  ATYPE16I PIC  X(0001).
      *    -------------------------------
           05  ACARR16L PIC S9(0004) COMP.
           05  ACARR16F PIC  X(0001).
           05  FILLER REDEFINES ACARR16F.
               10  ACARR16A PIC  X(0001).
           05  ACARR16I PIC  X(0001).
      *    -------------------------------
           05  ACLAM16L PIC S9(0004) COMP.
           05  ACLAM16F PIC  X(0001).
           05  FILLER REDEFINES ACLAM16F.
               10  ACLAM16A PIC  X(0001).
           05  ACLAM16I PIC  X(0007).
      *    -------------------------------
           05  ACERT16L PIC S9(0004) COMP.
           05  ACERT16F PIC  X(0001).
           05  FILLER REDEFINES ACERT16F.
               10  ACERT16A PIC  X(0001).
           05  ACERT16I PIC  X(0011).
      *    -------------------------------
           05  AACCT16L PIC S9(0004) COMP.
           05  AACCT16F PIC  X(0001).
           05  FILLER REDEFINES AACCT16F.
               10  AACCT16A PIC  X(0001).
           05  AACCT16I PIC  X(0010).
      *    -------------------------------
           05  ASELL PIC S9(0004) COMP.
           05  ASELF PIC  X(0001).
           05  FILLER REDEFINES ASELF.
               10  ASELA PIC  X(0001).
           05  ASELI PIC  9(2).
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
           05  APFKI PIC  99.
      *    -------------------------------
           05  APFK5L PIC S9(0004) COMP.
           05  APFK5F PIC  X(0001).
           05  FILLER REDEFINES APFK5F.
               10  APFK5A PIC  X(0001).
           05  APFK5I PIC  X(0019).
      *    -------------------------------
           05  APF2L PIC S9(0004) COMP.
           05  APF2F PIC  X(0001).
           05  FILLER REDEFINES APF2F.
               10  APF2A PIC  X(0001).
           05  APF2I PIC  X(0019).
      *    -------------------------------
           05  APFK6L PIC S9(0004) COMP.
           05  APFK6F PIC  X(0001).
           05  FILLER REDEFINES APFK6F.
               10  APFK6A PIC  X(0001).
           05  APFK6I PIC  X(0021).
       01  EL1325AO REDEFINES EL1325AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHEAD1O PIC  X(0038).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACOMPO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANUM01O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAME01O PIC  X(0027).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAGE01O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTA01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT01O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM01O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT01O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT01O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANUM02O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAME02O PIC  X(0027).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAGE02O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTA02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT02O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM02O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT02O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT02O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANUM03O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAME03O PIC  X(0027).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAGE03O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTA03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT03O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM03O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT03O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT03O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANUM04O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAME04O PIC  X(0027).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAGE04O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTA04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT04O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM04O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT04O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT04O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANUM05O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAME05O PIC  X(0027).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAGE05O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTA05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT05O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM05O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT05O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT05O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANUM06O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAME06O PIC  X(0027).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAGE06O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTA06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT06O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM06O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT06O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT06O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANUM07O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAME07O PIC  X(0027).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAGE07O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTA07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT07O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM07O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT07O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT07O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANUM08O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAME08O PIC  X(0027).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAGE08O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTA08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT08O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM08O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT08O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT08O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANUM09O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAME09O PIC  X(0027).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAGE09O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTA09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT09O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM09O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT09O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT09O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANUM10O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAME10O PIC  X(0027).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAGE10O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTA10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT10O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM10O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT10O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT10O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANUM11O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAME11O PIC  X(0027).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAGE11O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTA11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT11O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM11O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT11O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT11O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANUM12O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAME12O PIC  X(0027).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAGE12O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTA12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT12O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM12O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT12O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT12O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANUM13O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAME13O PIC  X(0027).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAGE13O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTA13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT13O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM13O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT13O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT13O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANUM14O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAME14O PIC  X(0027).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAGE14O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTA14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT14O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM14O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT14O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT14O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANUM15O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAME15O PIC  X(0027).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAGE15O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTA15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT15O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM15O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT15O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT15O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANUM16O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAME16O PIC  X(0027).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAGE16O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTA16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDT16O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAM16O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERT16O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT16O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASELO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFKO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFK5O PIC  X(0019).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APF2O PIC  X(0019).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFK6O PIC  X(0021).
      *    -------------------------------
00207
00208  01  FILLER       REDEFINES      EL1325AI.
101201     05  FILLER                  PIC X(85).
00210
00211      05  EL1325-MAP-LINE         OCCURS 16 TIMES
00212          INDEXED BY EL1325-INDEX
00213                     EL1325-INDEX2.
00214
00215          10  EL1325-NUM-LENGTH   PIC S9(4)    COMP.
00216          10  EL1325-NUM-ATTRB    PIC X.
00217          10  EL1325-NUM          PIC X(2).
00218
00219          10  EL1325-NAME-LENGTH  PIC S9(4)    COMP.
00220          10  EL1325-NAME-ATTRB   PIC X.
00221          10  EL1325-NAME         PIC X(27).
00222
00223          10  EL1325-AGE-LENGTH   PIC S9(4)    COMP.
00224          10  EL1325-AGE-ATTRB    PIC X.
00225          10  EL1325-AGE          PIC 99.
00226
00227          10  EL1325-STA-LENGTH   PIC S9(4)    COMP.
00228          10  EL1325-STA-ATTRB    PIC X.
00229          10  EL1325-STA          PIC X.
00230
00231          10  EL1325-DATE-INCURRED-LENGTH PIC S9(4)     COMP.
00232          10  EL1325-DATE-INCURRED-ATTRB  PIC X.
00233          10  EL1325-DATE-INCURRED    PIC X(8).
00234
00235          10  EL1325-TYPE-LENGTH  PIC S9(4)      COMP.
00236          10  EL1325-TYPE-ATTRB   PIC X.
00237          10  EL1325-TYPE         PIC X.
00238
00239          10  EL1325-CARRIER-LENGTH   PIC S9(4)    COMP.
00240          10  EL1325-CARRIER-ATTRB    PIC X.
00241          10  EL1325-CARRIER          PIC X.
00242
00243          10  EL1325-CLAIM-LENGTH PIC S9(4)    COMP.
00244          10  EL1325-CLAIM-ATTRB  PIC X.
00245          10  EL1325-CLAIM        PIC X(7).
00246
00247          10  EL1325-CERT-NO-LENGTH   PIC S9(4)    COMP.
00248          10  EL1325-CERT-NO-ATTRB    PIC X.
00249          10  EL1325-CERT-NO      PIC X(11).
00250
00251          10  EL1325-ACCOUNT-LENGTH   PIC S9(4)    COMP.
00252          10  EL1325-ACCOUNT-ATTRB    PIC X.
00253          10  EL1325-ACCOUNT      PIC X(10).
00254      05  FILLER                  PIC X(160).
00255      EJECT
00256 *                                COPY ELCEMIB.
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
00257
00258      EJECT
00259 *                                COPY ELCDATE.
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
00260
00261      EJECT
00262 *                                COPY ELCATTR.
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
00263
00264      EJECT
00265 *                                COPY ELCLOGOF.
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
00266
00267      EJECT
00268 *                                COPY ELCAID.
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
00269
00270  01  FILLER                      REDEFINES
00271      DFHAID.
00272
00273      05  FILLER                  PIC X(8).
00274
00275      05  PF-VALUES               PIC X
00276          OCCURS 24 TIMES.
00277      EJECT
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
00279
00280  01  DFHCOMMAREA                 PIC X(1024).
00281
00282      EJECT
00283 *                                COPY ELCMSTR.
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
00284
00285      EJECT
00286 *                                COPY ELCCNTL.
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
00287
00288      EJECT
00289 *                                COPY ELCALPH.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCALPH.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ALPHA CROSS REFERENCE FILE                *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 128  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELALPH                         RKP=2,LEN=42   *
00013 *       ALTERNATE PATH1 = ELALPH2 (FULL CONTROL)  RKP=44,LEN=57  *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCALPH                          *
00017 ******************************************************************
00018
00019  01  ALPHA-INDEX.
00020      12  AI-RECORD-ID                         PIC XX.
00021          88  VALID-AI-ID                  VALUE 'AI'.
00022      12  AI-CONTROL-PRIMARY.
00023          16  AI-COMPANY-CD                     PIC X.
00024          16  AI-SOURCE                         PIC X.
00025              88  AI-CLAIM-SYSTEM          VALUE 'C'.
00026              88  AI-ADMIN-SYSTEM          VALUE 'A'.
00027          16  AI-NAME.
00028              20  AI-LAST-NAME                  PIC X(15).
00029              20  AI-FIRST-NAME.
00030                  24  AI-FIRST-INITIAL          PIC X.
00031                  24  FILLER                    PIC X(11).
00032              20  AI-MIDDLE-INIT                PIC X.
00033          16  AI-DATE                           PIC X(8).
00034          16  AI-TIME                           PIC S9(07) COMP-3.
00035
00036      12  AI-CONTROL-BY-ADMIN-KEY.
00037          16  AI-CM-COMPANY-CD                  PIC X.
00038          16  AI-CM-SOURCE                      PIC X.
00039          16  AI-CM-CARRIER                     PIC X.
00040          16  AI-CM-GROUPING.
00041              20  AI-CM-GROUPING-PREFIX         PIC X(3).
00042              20  AI-CM-GROUPING-PRIME          PIC X(3).
00043          16  AI-CM-STATE                       PIC XX.
00044          16  AI-CM-PRODUCER.
00045              20  AI-CM-PRODUCER-PREFIX         PIC X(4).
00046              20  AI-CM-PRODUCER-PRIME          PIC X(6).
00047          16  AI-CM-CERT-EFF-DT                 PIC XX.
00048          16  AI-CM-CERTIFICATE-NUMBER.
00049              20  AI-CM-CERT-PRIME              PIC X(10).
00050              20  AI-CM-CERT-SFX                PIC X.
00051          16  AI-CM-DATE                        PIC X(8).
00052          16  AI-CM-TIME                        PIC S9(7) COMP-3.
00053          16  FILLER                            PIC X(11).
00054
00055      12  AI-CONTROL-BY-CLAIM-KEY REDEFINES
00056          AI-CONTROL-BY-ADMIN-KEY.
00057          16  AI-CL-COMPANY-CD                  PIC X.
00058          16  AI-CL-SOURCE                      PIC X.
00059          16  AI-CL-CARRIER                     PIC X.
00060          16  AI-CL-CLAIM-NUMBER                PIC X(7).
00061          16  AI-CL-CERTIFICATE-NUMBER.
00062              20  AI-CL-CERT-PRIME              PIC X(10).
00063              20  AI-CL-CERT-SFX                PIC X.
00064          16  AI-CL-DATE                        PIC X(8).
00065          16  AI-CL-INCURRED-DATE               PIC XX.
00066          16  AI-CL-CLOSE-DATE                  PIC XX.
00067          16  AI-CL-TIME                        PIC S9(7)  COMP-3.
00068          16  AI-CREDIT-CARD-NUMBER.
00069              20  AI-CCN.
00070                  24  AI-CCN-PREFIX             PIC X(4).
00071                  24  AI-CCN-PRIME              PIC X(12).
00072              20  AI-CCN-FILLER                 PIC X(4).
00073
00074      12  AI-MAINT-INFO.
00075          16  AI-LAST-MAINT-BY                  PIC X(4).
00076          16  AI-LAST-MAINT-DT                  PIC XX.
00077          16  AI-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
00078
00079      12  AI-CLAIM-PAID-THRU-DT                 PIC XX.
00080
00081      12  FILLER                                PIC X(15).
00082
00290
00291      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CLAIM-MASTER
                                CONTROL-FILE ALPHA-INDEX.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1325' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00293
00294      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00295      MOVE '5'                   TO DC-OPTION-CODE.
00296      PERFORM 8500-DATE-CONVERSION.
00297      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00298      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00299
00300
00301      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00302
00303 *    NOTE *******************************************************
00304 *         *                                                     *
00305 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
00306 *         *  FROM ANOTHER MODULE.                               *
00307 *         *                                                     *
00308 *         *******************************************************.
00309
00310      IF EIBCALEN NOT GREATER THAN ZERO
00311          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00312          PERFORM 8300-SEND-TEXT.
00313
00314      
      * EXEC CICS HANDLE CONDITION
00315 *        PGMIDERR (9600-PGMIDERR)
00316 *        NOTFND   (8700-NOT-FOUND)
00317 *        ENDFILE  (3600-ENDFILE)
00318 *        DUPKEY   (3015-DUPKEY)
00319 *        ERROR    (9990-ERROR)
00320 *    END-EXEC.
      *    MOVE '"$LI''$.               ! " #00004673' TO DFHEIV0
           MOVE X'22244C4927242E2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303034363733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00321
00322      EJECT
00323  0010-MAIN-LOGIC.
00324
00325      IF PI-CALLING-PROGRAM EQUAL THIS-PGM
00326          GO TO 0100-MAIN-LOGIC.
00327
00328      IF PI-RETURN-TO-PROGRAM NOT EQUAL THIS-PGM
00329          MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00330          MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00331          MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00332          MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00333          MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00334          MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00335          MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00336          MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
00337        ELSE
00338          MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00339          MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00340          MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00341          MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00342          MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00343          MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00344          MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00345          MOVE SPACES               TO  PI-SAVED-PROGRAM-6.
00346
00347      MOVE ZERO TO PI-SCREEN-COUNT.
00348
00349      IF (PI-OPTION   =  ZERO) OR
00350         (PI-OPTION   =   '1') OR
00351         (PI-OPTION   =   '4')
00352          MOVE PI-ALPH-CLAIM-KEY    TO PI-1ST-ALPH-KEY
00353      ELSE
00354          MOVE PI-ALPH-ADMIN-KEY    TO PI-1ST-ALPH-KEY.
00355
00356      IF PI-1ST-TIME-SW EQUAL TO +2
00357          MOVE EIBTRMID           TO  WS-TSK-TERM-ID
00358          
      * EXEC CICS READQ TS
00359 *            QUEUE  (WS-TEMP-STORAGE-KEY)
00360 *            ITEM   (PI-TS-ITEM)
00361 *            INTO   (EL1325AI)
00362 *            LENGTH (WS-TS-LENGTH)
00363 *        END-EXEC
      *    MOVE '*$II   L              ''   #00004717' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 EL1325AI, 
                 WS-TS-LENGTH, 
                 PI-TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00364          
      * EXEC CICS DELETEQ TS
00365 *            QUEUE  (WS-TEMP-STORAGE-KEY)
00366 *        END-EXEC
      *    MOVE '*&                    #   #00004723' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00367          MOVE ZERO               TO  PI-1ST-TIME-SW
00368          MOVE LOW-VALUES         TO  ASELO
00369                                      APFKO
00370          PERFORM 5200-SET-ATTRB
00371              VARYING EL1325-INDEX FROM PI-LINE-COUNT BY -1
00372                  UNTIL EL1325-INDEX NOT GREATER THAN ZERO
00373          GO TO 8100-SEND-INITIAL-MAP.
00374
00375      GO TO 3000-BROWSE-ALPHA-FILE.
00376
00377      EJECT
00378  0100-MAIN-LOGIC.
00379
00380 *    NOTE *******************************************************
00381 *         *                                                     *
00382 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *
00383 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *
00384 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
00385 *         *                                                     *
00386 *         *******************************************************.
00387
00388      IF EIBAID EQUAL TO DFHCLEAR
00389          GO TO 9400-CLEAR.
00390
00391      IF EIBAID EQUAL TO (DFHPA1 OR
00392                          DFHPA2 OR
00393                          DFHPA3)
00394          MOVE LOW-VALUES            TO  EL1325AI
00395          MOVE -1                    TO  APFKL
00396          MOVE ER-0008               TO  EMI-ERROR
00397          GO TO 8200-SEND-DATAONLY.
00398
00399      
      * EXEC CICS RECEIVE
00400 *        INTO   (EL1325AI)
00401 *        MAPSET (WS-MAPSET-NAME)
00402 *        MAP    (WS-MAP-NAME)
00403 *    END-EXEC.
           MOVE LENGTH OF
            EL1325AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00004758' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL1325AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00404
00405      IF APFKL IS GREATER THAN ZERO
00406          IF EIBAID NOT EQUAL TO DFHENTER
00407              MOVE ER-0004           TO  EMI-ERROR
00408              MOVE AL-UNBOF          TO  APFKA
00409              MOVE -1                TO  APFKL
00410              GO TO 8200-SEND-DATAONLY
00411            ELSE
00412              IF APFKO IS NUMERIC
00413                AND APFKO IS GREATER THAN ZERO
00414                AND APFKO IS LESS THAN '25'
00415                  MOVE PF-VALUES (APFKI)  TO  EIBAID
00416                ELSE
00417                  MOVE ER-0029           TO  EMI-ERROR
00418                  MOVE AL-UNBOF       TO  APFKA
00419                  MOVE -1             TO  APFKL
00420                  GO TO 8200-SEND-DATAONLY.
00421
00422      IF EIBAID IS EQUAL TO DFHPF12
00423          MOVE 'EL010   '         TO  THIS-PGM
00424          GO TO 9300-XCTL.
00425
00426      IF EIBAID IS EQUAL TO DFHPF23
00427          GO TO 9000-RETURN-CICS.
00428
00429      IF EIBAID IS EQUAL TO DFHPF24
00430          MOVE 'EL126   '         TO  THIS-PGM
00431          GO TO 9300-XCTL.
00432
00433      IF EIBAID EQUAL TO (DFHENTER OR
00434                          DFHPF1 OR
00435                          DFHPF2 OR
00436                          DFHPF3 OR
00437                          DFHPF5 OR
00438                          DFHPF6)
00439          NEXT SENTENCE
00440        ELSE
00441          MOVE -1                 TO  APFKL
00442          MOVE ER-0008               TO  EMI-ERROR
00443          GO TO 8200-SEND-DATAONLY.
00444
00445      IF EIBAID EQUAL DFHPF3
00446        OR ASELL GREATER THAN ZERO
00447          NEXT SENTENCE
00448        ELSE
00449          GO TO 0120-MAIN-LOGIC.
00450
00451      IF ASELL GREATER THAN ZERO
00452         AND ASELI NUMERIC
00453         NEXT SENTENCE
00454      ELSE
00455         IF PI-SAVED-PROGRAM-1 EQUAL TO 'EL130   '
00456              NEXT SENTENCE
00457          ELSE
00458             MOVE -1                    TO  ASELL
00459             MOVE ER-0031               TO  EMI-ERROR
00460             GO TO 8200-SEND-DATAONLY.
00461
00462      IF ASELL GREATER THAN ZERO
00463        AND ASELO GREATER THAN ZERO
00464        AND ASELO LESS THAN '17'
00465        AND (ASELI NUMERIC AND ASELI NOT GREATER PI-LINE-COUNT)
00466          NEXT SENTENCE
00467        ELSE
00468          IF PI-SAVED-PROGRAM-1 EQUAL TO 'EL130   '
00469              NEXT SENTENCE
00470            ELSE
00471              MOVE -1                    TO  ASELL
00472              MOVE ER-0200               TO  EMI-ERROR
00473              GO TO 8200-SEND-DATAONLY.
00474
00475      IF ASELL IS GREATER THAN ZERO
00476          IF ASELI IS NUMERIC
00477              NEXT SENTENCE
00478          ELSE
00479              MOVE -1                 TO  ASELL
00480              MOVE ER-0031            TO  EMI-ERROR
00481              GO TO 8200-SEND-DATAONLY.
00482
00483      IF ASELL GREATER THAN ZERO
00484          SET EL1325-INDEX TO ASELI
00485          MOVE EL1325-CARRIER (EL1325-INDEX)  TO  PI-CARRIER
00486          MOVE EL1325-CLAIM   (EL1325-INDEX)  TO  PI-CLAIM-NO
00487          MOVE EL1325-CERT-NO (EL1325-INDEX)  TO  PI-CERT-NO
00488          MOVE EL1325-ACCOUNT (EL1325-INDEX)  TO  PI-ACCOUNT
00489          SET PI-INDEX TO ASELI
00490          MOVE PI-SA-GROUP    (PI-INDEX)  TO  PI-GROUPING
00491          MOVE PI-SA-STATE    (PI-INDEX)  TO  PI-STATE
00492          MOVE PI-SA-EFF-DATE (PI-INDEX)  TO  PI-CERT-EFF-DT
00493        ELSE
00494          MOVE SPACES             TO  PI-CARRIER
00495                                      PI-CLAIM-NO
00496                                      PI-CERT-NO
00497                                      PI-ACCOUNT
00498                                      PI-GROUPING
00499                                      PI-STATE
00500          MOVE LOW-VALUES         TO  PI-CERT-EFF-DT.
00501
00502      MOVE +2                     TO  PI-1ST-TIME-SW
00503
00504      IF EIBAID EQUAL TO DFHPF3
00505          MOVE 'EL1323  '         TO  THIS-PGM
00506      ELSE
00507          IF PI-SAVED-PROGRAM-1 EQUAL TO ('EL126   ' OR
00508                                          'EL127   ' OR
00509                                          'EL130   ' OR
00510                                          'EL132   ' OR
00511                                          'EL150   ' OR
00512                                          'EL1602  ' OR
00513                                          'EL162   ')
00514             MOVE PI-SAVED-PROGRAM-1  TO  THIS-PGM
00515          ELSE
00516             MOVE 'EL1323  '  TO  THIS-PGM.
00517
00518      IF THIS-PGM EQUAL TO 'EL1323  '
00519          MOVE EIBTRMID   TO  WS-TSK-TERM-ID
00520          MOVE -1         TO  ASELL
00521          
      * EXEC CICS WRITEQ TS
00522 *            FROM   (EL1325AI)
00523 *            LENGTH (WS-TS-LENGTH)
00524 *            QUEUE  (WS-TEMP-STORAGE-KEY)
00525 *            ITEM   (PI-TS-ITEM)
00526 *        END-EXEC
      *    MOVE '*" I                  ''   #00004880' TO DFHEIV0
           MOVE X'2A2220492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 EL1325AI, 
                 WS-TS-LENGTH, 
                 PI-TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00527        ELSE
00528          MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00529          MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00530          MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00531          MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00532          MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00533          MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00534          MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00535          MOVE SPACES               TO  PI-SAVED-PROGRAM-6.
00536
00537      IF THIS-PGM EQUAL TO ('EL126   ' OR 'EL127   ' OR 'EL132   ')
00538          MOVE 'EL150   '         TO  THIS-PGM.
00539
00540      GO TO 9300-XCTL.
00541
00542  0120-MAIN-LOGIC.
00543
00544      IF EIBAID EQUAL TO (DFHENTER OR DFHPF1 OR DFHPF5 OR DFHPF6)
00545        OR
00546          ((EIBAID EQUAL TO DFHPF2) AND
00547           (PI-SCREEN-COUNT GREATER THAN +1))
00548              NEXT SENTENCE
00549            ELSE
00550              MOVE -1                 TO  APFKL
00551              MOVE ER-0008            TO  EMI-ERROR
00552              GO TO 8200-SEND-DATAONLY.
00553
00554      IF EIBAID EQUAL DFHPF5
00555         IF PI-ORIGINAL-COMPANY-ID NOT EQUAL SPACES
00556            PERFORM 8600-NEXT-COMPANY THRU 8600-EXIT
00557            PERFORM 8650-WRITE-SECURITY-TEMP-STORE THRU 8650-EXIT
00558            IF NOT DISPLAY-CAP
00559                PERFORM 8800-INITIALIZE-MAP VARYING EL1325-INDEX
00560                 FROM +1 BY +1 UNTIL EL1325-INDEX GREATER THAN +16
00561                MOVE 'READ'           TO  SM-READ
00562                PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00563                MOVE ER-0070          TO  EMI-ERROR
00564                MOVE -1               TO  APFKL
00565                GO TO 8100-SEND-INITIAL-MAP
00566            ELSE
00567                NEXT SENTENCE
00568         ELSE
00569            MOVE ER-0008              TO  EMI-ERROR
00570            MOVE -1                   TO  ASELL
00571            GO TO 8200-SEND-DATAONLY.
00572
00573      IF EIBAID EQUAL DFHPF6
00574         IF PI-ORIGINAL-COMPANY-ID NOT EQUAL SPACES
00575            PERFORM 8600-NEXT-COMPANY THRU 8600-EXIT
00576            PERFORM 8650-WRITE-SECURITY-TEMP-STORE THRU 8650-EXIT
00577            MOVE PI-ORIGINAL-COMPANY-CD TO PI-COMPANY-CD
00578                                           PI-CK-COMPANY-CD
00579            IF NOT DISPLAY-CAP
00580                PERFORM 8800-INITIALIZE-MAP VARYING EL1325-INDEX
00581                 FROM +1 BY +1 UNTIL EL1325-INDEX GREATER THAN +16
00582                MOVE 'READ'           TO  SM-READ
00583                PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00584                MOVE ER-0070          TO  EMI-ERROR
00585                MOVE -1               TO  APFKL
00586                GO TO 8100-SEND-INITIAL-MAP
00587            ELSE
00588                NEXT SENTENCE
00589         ELSE
00590            MOVE ER-0008              TO  EMI-ERROR
00591            MOVE -1                   TO  ASELL
00592            GO TO 8200-SEND-DATAONLY.
00593
00594      IF PI-END-OF-FILE GREATER THAN ZERO
00595        AND ((EIBAID EQUAL TO (DFHENTER OR DFHPF1) AND
00596              PI-LAST-EIBAID EQUAL TO (DFHENTER OR DFHPF1))
00597            OR
00598             (EIBAID EQUAL TO DFHPF2 AND
00599              PI-LAST-EIBAID EQUAL TO DFHPF2))
00600                  MOVE -1             TO  ASELL
00601                  GO TO 8200-SEND-DATAONLY.
00602
00603      IF (EIBAID IS EQUAL TO DFHENTER OR DFHPF1)
00604          IF PI-ALPH-CO-CD  IS EQUAL TO PI-COMPANY-CD
00605              NEXT SENTENCE
00606          ELSE
00607              MOVE -1                 TO  ASELL
00608              GO TO 8200-SEND-DATAONLY.
00609
00610      GO TO 3000-BROWSE-ALPHA-FILE.
00611
00612      EJECT
00613  3000-BROWSE-ALPHA-FILE SECTION.
00614
00615      
      * EXEC CICS HANDLE CONDITION
00616 *        NOTFND   (8700-NOT-FOUND)
00617 *        END-EXEC.
      *    MOVE '"$I                   ! # #00004974' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034393734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00618
00619      MOVE EIBAID                 TO  PI-LAST-EIBAID.
00620
00621      MOVE LOW-VALUES             TO  EL1325AI.
00622
00623      IF PI-OPTION  =  '2'
00624          MOVE PI-CLM-CLAIM-NO    TO  WS-SAVE-CLAIM-NO.
00625
00626      IF (PI-OPTION   = ZERO)  OR
00627         (PI-OPTION   =  '1')  OR
00628         (PI-OPTION   =  '4')
00629          NEXT SENTENCE
00630      ELSE
00631          GO TO 3000-READ-ALPHA-ADMIN.
00632
00633      IF (PI-BROWSE-SW EQUAL TO ZERO)  AND
00634         (PI-START-SW EQUAL TO +1)
00635           
      * EXEC CICS STARTBR
00636 *            DATASET    (PI-DSID)
00637 *            RIDFLD     (PI-ALPH-CLAIM-KEY)
00638 *            KEYLENGTH  (PI-KEY-LENGTH)
00639 *            GENERIC
00640 *            EQUAL
00641 *         END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    E          &   #00004994' TO DFHEIV0
           MOVE X'262C2020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 PI-ALPH-CLAIM-KEY, 
                 PI-KEY-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00642           GO TO 3005-NEXT-SENTENCE.
00643
00644  3000-READ-ALPHA-ADMIN.
00645
00646      IF (PI-BROWSE-SW EQUAL TO ZERO)  AND
00647         (PI-START-SW EQUAL TO +1)
00648           
      * EXEC CICS STARTBR
00649 *            DATASET    (PI-DSID)
00650 *            RIDFLD     (PI-ALPH-ADMIN-KEY)
00651 *            KEYLENGTH  (PI-KEY-LENGTH)
00652 *            GENERIC
00653 *            EQUAL
00654 *         END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    E          &   #00005007' TO DFHEIV0
           MOVE X'262C2020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 PI-ALPH-ADMIN-KEY, 
                 PI-KEY-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00655           GO TO 3005-NEXT-SENTENCE.
00656
00657      IF EIBAID EQUAL TO DFHPF2
00658          SUBTRACT 2 FROM PI-SCREEN-COUNT
00659          PERFORM 7000-PF2-POSITION
00660          GO TO 3005-NEXT-SENTENCE.
00661
00662      IF (PI-OPTION   = ZERO) OR
00663         (PI-OPTION   =  '1') OR
00664         (PI-OPTION   =  '4')
00665          
      * EXEC CICS STARTBR
00666 *          DATASET      (PI-DSID)
00667 *          RIDFLD       (PI-ALPH-CLAIM-KEY)
00668 *          KEYLENGTH    (PI-KEY-LENGTH)
00669 *          GENERIC
00670 *          EQUAL
00671 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    E          &   #00005024' TO DFHEIV0
           MOVE X'262C2020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 PI-ALPH-CLAIM-KEY, 
                 PI-KEY-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00672      ELSE
00673          
      * EXEC CICS STARTBR
00674 *          DATASET      (PI-DSID)
00675 *          RIDFLD       (PI-ALPH-ADMIN-KEY)
00676 *          KEYLENGTH    (PI-KEY-LENGTH)
00677 *          GENERIC
00678 *          EQUAL
00679 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    E          &   #00005032' TO DFHEIV0
           MOVE X'262C2020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 PI-ALPH-ADMIN-KEY, 
                 PI-KEY-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00680
00681  3005-NEXT-SENTENCE.
00682
00683      MOVE +1                     TO  PI-BROWSE-SW.
00684      MOVE ZERO                   TO  PI-LINE-COUNT.
00685      MOVE LOW-VALUES             TO  EL1325AI.
00686
00687      IF (PI-OPTION   = ZERO) OR
00688         (PI-OPTION   =  '1') OR
00689         (PI-OPTION   =  '4')
00690          MOVE PI-ALPH-CLAIM-KEY  TO  WS-KEY-HOLD
00691      ELSE
00692          MOVE PI-ALPH-ADMIN-KEY  TO  WS-KEY-HOLD.
00693
00694      SET EL1325-INDEX            TO  +1.
00695      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00696      MOVE '5'                    TO  DC-OPTION-CODE.
00697      PERFORM 8500-DATE-CONVERSION.
00698      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-DATE.
00699
00700  3010-READNEXT.
00701
00702      IF (PI-OPTION   = ZERO) OR
00703         (PI-OPTION   =  '1') OR
00704         (PI-OPTION   =  '4')
00705          
      * EXEC CICS READNEXT
00706 *            DATASET (PI-DSID)
00707 *            RIDFLD  (PI-ALPH-CLAIM-KEY)
00708 *            SET     (ADDRESS OF ALPHA-INDEX)
00709 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005064' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ALPH-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ALPHA-INDEX TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00710      ELSE
00711          
      * EXEC CICS READNEXT
00712 *            DATASET (PI-DSID)
00713 *            RIDFLD  (PI-ALPH-ADMIN-KEY)
00714 *            SET     (ADDRESS OF ALPHA-INDEX)
00715 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005070' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ALPH-ADMIN-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ALPHA-INDEX TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00716
00717      IF PI-LINE-COUNT NOT EQUAL TO ZERO
00718          MOVE ZERO               TO  PI-AIX-RECORD-COUNT.
00719
00720  3015-DUPKEY.
00721
00722      IF LCP-ONCTR-01 =  0
00723          ADD 1 TO LCP-ONCTR-01
00724        ELSE
00725          GO TO 3016-NEXT-SENTENCE.
00726
00727      IF PI-AIX-RECORD-COUNT GREATER THAN +16
00728          SUBTRACT +1 FROM PI-AIX-RECORD-COUNT.
00729
00730  3016-NEXT-SENTENCE.
00731
00732      ADD +1  TO  WS-AIX-RECORD-COUNT
00733
00734      IF EIBAID EQUAL DFHPF1
00735         IF OPTION-ONE-SELECTED
00736            IF PI-LINE-COUNT EQUAL ZERO
00737               PERFORM 5000-MOVE-NAME
00738               IF AI-LAST-NAME  EQUAL  PI-ALPH-LAST-NAME
00739                  IF PI-LAST-ALPH-KEY EQUAL
00740                           AI-CONTROL-PRIMARY
00741                     NEXT SENTENCE
00742                  ELSE
00743                     GO TO 3010-READNEXT.
00744
00745      IF (PI-OPTION  = ZERO) OR
00746         (PI-OPTION  =  '1') OR
00747         (PI-OPTION  =  '4')
00748          MOVE PI-ALPH-CLAIM-KEY     TO  WS-KEY-INPUT
00749      ELSE
00750          MOVE PI-ALPH-ADMIN-KEY     TO  WS-KEY-INPUT.
00751
00752      SET KEY-INDEX
00753          KEY-INDEX2 TO +1.
00754
00755  3020-COMPARE-KEY.
00756
00757      IF WS-KH-CHAR (KEY-INDEX) NOT EQUAL WS-KI-CHAR (KEY-INDEX2)
00758          GO TO 3700-END-OF-BROWSE.
00759
00760      IF KEY-INDEX LESS THAN PI-KEY-LENGTH
00761          SET KEY-INDEX
00762              KEY-INDEX2 UP BY +1
00763          GO TO 3020-COMPARE-KEY.
00764
00765      IF OPTION-TWO-SELECTED
00766         IF PI-CLM-CLAIM-NO   NOT EQUAL SPACES
00767            IF PI-CLM-CLAIM-NO   EQUAL AI-CL-CLAIM-NUMBER
00768               NEXT SENTENCE
00769            ELSE
00770               GO TO 3010-READNEXT.
00771 ******************************************************************
00772 *                                                                *
00773 *        SECURITY CHECK FOR CARRIER AND ACCOUNT NUMBER           *
00774 *                      04/04/84                                  *
00775 *                                                                *
00776 ******************************************************************
00777
00778      IF  PI-NO-CARRIER-SECURITY
00779          GO TO 3095-MOVE-DATA.
00780
00781      IF  PI-CARRIER-SECURITY GREATER THAN SPACES
00782          IF  AI-CL-CARRIER = PI-CARRIER-SECURITY
00783              NEXT SENTENCE
00784          ELSE
00785              GO TO 3010-READNEXT.
00786
00787      IF  PI-CLM-CERT-NUMBER   NOT EQUAL SPACES
00788          IF PI-CLM-CERT-NUMBER  =   AI-CL-CERTIFICATE-NUMBER
00789              NEXT SENTENCE
00790          ELSE
00791              GO TO 3010-READNEXT.
00792
00793  3095-MOVE-DATA.
00794
00795      MOVE +1                     TO WS-CLAIMS-SW.
00796      MOVE 'N'                    TO WS-CLAIMS-FOUND-SW.
00797
00798      MOVE WS-KEY-INPUT TO PI-LAST-ALPH-KEY.
00799
00800      ADD +1  TO  PI-LINE-COUNT
00801                  PI-AIX-RECORD-COUNT.
00802
00803      PERFORM 5000-MOVE-NAME
00804
00805      MOVE AI-CONTROL-PRIMARY     TO PI-LAST-ALPH-KEY.
00806      MOVE AI-LAST-NAME           TO PI-LAST-NAME
00807                                     PI-CK-INSURED-LAST-NAME.
00808      MOVE WS-NAME-WORK           TO EL1325-NAME (EL1325-INDEX).
00809
00810      MOVE AI-CL-CARRIER          TO EL1325-CARRIER (EL1325-INDEX).
00811      MOVE AI-CL-CLAIM-NUMBER     TO EL1325-CLAIM   (EL1325-INDEX).
00812      MOVE AI-CL-CERTIFICATE-NUMBER
00813                                  TO EL1325-CERT-NO (EL1325-INDEX).
00814
00815      IF AI-CL-CLOSE-DATE NOT EQUAL TO LOW-VALUES
00816          MOVE SPACES             TO  DC-OPTION-CODE
00817          MOVE AI-CL-CLOSE-DATE   TO  DC-BIN-DATE-1
00818          PERFORM 8500-DATE-CONVERSION
00819          MOVE DC-GREG-DATE-1-EDIT  TO  EL1325-ACCOUNT
00820                                                    (EL1325-INDEX).
00821      PERFORM 5200-SET-ATTRB.
00822
00823      PERFORM 4000-READ-CLAIM-FILE THRU 4990-EXIT.
00824
00825      IF WS-CLAIMS-MATCH
00826         IF CL-INCURRED-DT NOT EQUAL TO LOW-VALUES
00827            MOVE SPACES             TO  DC-OPTION-CODE
00828            MOVE CL-INCURRED-DT     TO  DC-BIN-DATE-1
00829            PERFORM 8500-DATE-CONVERSION
00830            MOVE DC-GREG-DATE-1-EDIT  TO  EL1325-DATE-INCURRED
00831                                                    (EL1325-INDEX).
00832
00833      IF WS-CLAIMS-NO-MATCH
00834         IF AI-CL-INCURRED-DATE  NOT EQUAL TO LOW-VALUES
00835            MOVE SPACES              TO  DC-OPTION-CODE
00836            MOVE AI-CL-INCURRED-DATE TO  DC-BIN-DATE-1
00837            PERFORM 8500-DATE-CONVERSION
00838            MOVE DC-GREG-DATE-1-EDIT  TO  EL1325-DATE-INCURRED
00839                                                    (EL1325-INDEX).
00840
00841      IF EL1325-INDEX LESS THAN +16
00842          SET EL1325-INDEX UP BY +1
00843          GO TO 3010-READNEXT.
00844
00845      GO TO 3900-ENDBROWSE.
00846
00847  3600-ENDFILE.
00848
00849      MOVE ER-0130                   TO  EMI-ERROR.
00850      ADD +1  TO  PI-END-OF-FILE.
00851
00852  3700-END-OF-BROWSE.
00853
00854      ADD +1  TO  PI-END-OF-FILE.
00855
00856      
      * EXEC CICS ENDBR
00857 *        DATASET (PI-DSID)
00858 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005215' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00859
00860  3900-ENDBROWSE.
00861
00862 ******************************************************************
00863 *                                                                *
00864 *        IF THERE ARE NO CLAIM RECORDS FOUND DURING              *
00865 *        THE INITIAL ENTRY OF EL6322, RETURN TO THE CALLING      *
00866 *        PROGRAM.                                                *
00867 *                                                                *
00868 ******************************************************************
00869
00870      IF  WS-NO-CLAIMS-FOUND
00871          MOVE +9                 TO  PI-BROWSE-SW
00872          MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM
00873          GO TO 9400-CLEAR.
00874      ADD +1 TO PI-SCREEN-COUNT.
00875
00876      MOVE +1                     TO  PI-1ST-TIME-SW
00877
00878      MOVE -1                     TO  ASELL
00879      GO TO 8100-SEND-INITIAL-MAP.
00880
00881      EJECT
00882  4000-READ-CLAIM-FILE  SECTION.
00883
00884      
      * EXEC CICS HANDLE CONDITION
00885 *        NOTFND   (4990-EXIT)
00886 *        END-EXEC.
      *    MOVE '"$I                   ! $ #00005243' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303035323433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00887
00888      MOVE LOW-VALUES                 TO  PI-CLAIM-KEY.
00889      MOVE AI-CL-COMPANY-CD           TO  PI-CK-COMPANY-CD
00890      MOVE AI-CL-CARRIER              TO  PI-CK-CARRIER
00891      MOVE AI-CL-CLAIM-NUMBER         TO  PI-CK-CLAIM
00892      MOVE AI-CL-CERTIFICATE-NUMBER   TO  PI-CK-CERT-NO.
00893
00894      
      * EXEC CICS READ
00895 *        DATASET   ('ELMSTR')
00896 *        SET       (ADDRESS OF CLAIM-MASTER)
00897 *        RIDFLD    (PI-CLAIM-KEY)
00898 *     END-EXEC.
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&"S        E          (   #00005253' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00899
00900      IF (AI-CL-COMPANY-CD          EQUAL  PI-CK-COMPANY-CD) AND
00901         (AI-CL-CARRIER             EQUAL  PI-CK-CARRIER)    AND
00902         (AI-CL-CLAIM-NUMBER        EQUAL  PI-CK-CLAIM)      AND
00903         (AI-CL-CERTIFICATE-NUMBER  EQUAL PI-CK-CERT-NO)
00904           MOVE  'Y'                   TO  WS-CLAIMS-FOUND-SW.
00905
00906      IF CL-INSURED-BIRTH-DT NOT EQUAL TO LOW-VALUES
00907          MOVE CL-INSURED-BIRTH-DT    TO  DC-BIN-DATE-1
00908          MOVE WS-CURRENT-DATE        TO  DC-BIN-DATE-2
00909          MOVE '1'                    TO  DC-OPTION-CODE
00910          PERFORM 8500-DATE-CONVERSION
00911          DIVIDE DC-ELAPSED-MONTHS BY +12 GIVING EL1325-AGE
00912                                                    (EL1325-INDEX).
00913
00914      MOVE CL-CLAIM-STATUS      TO  EL1325-STA     (EL1325-INDEX).
00915
00916      IF CL-PURGED-DT NOT EQUAL LOW-VALUES
00917         MOVE 'P'               TO  EL1325-STA (EL1325-INDEX).
00918
00919      IF CL-INCURRED-DT NOT EQUAL TO LOW-VALUES
00920          MOVE SPACES             TO  DC-OPTION-CODE
00921          MOVE CL-INCURRED-DT     TO  DC-BIN-DATE-1
00922          PERFORM 8500-DATE-CONVERSION
00923          MOVE DC-GREG-DATE-1-EDIT  TO  EL1325-DATE-INCURRED
00924                                                    (EL1325-INDEX).
00925
00926      MOVE CL-CLAIM-TYPE        TO  EL1325-TYPE      (EL1325-INDEX)
00927      MOVE CL-CERT-ACCOUNT      TO  EL1325-ACCOUNT   (EL1325-INDEX)
00928
00929      SET PI-INDEX  TO  EL1325-INDEX
00930
00931      MOVE CL-CERT-GROUPING  TO  PI-SA-GROUP    (PI-INDEX)
00932      MOVE CL-CERT-STATE     TO  PI-SA-STATE    (PI-INDEX)
00933      MOVE CL-CERT-EFF-DT    TO  PI-SA-EFF-DATE (PI-INDEX)
00934
00935      IF CL-CERT-NO EQUAL TO CL-PRIME-CERT-NO AND
00936         CL-ASSOC-CERT-TOTAL GREATER THAN +1
00937         MOVE AL-SABON     TO EL1325-NAME-ATTRB (EL1325-INDEX)
00938         ELSE
00939         MOVE AL-SANON     TO EL1325-NAME-ATTRB (EL1325-INDEX).
00940
00941  4990-EXIT.
00942      EXIT.
00943      EJECT
00944  5000-MOVE-NAME SECTION.
00945
00946      MOVE SPACES                  TO  WS-NAME-WORK-AREA.
00947      MOVE ZEROS                   TO  WS-NAME-SW.
00948      SET NWA-INDEX                TO  +1.
00949
00950      IF (AI-FIRST-NAME  =  SPACES) AND
00951         (AI-MIDDLE-INIT =  SPACES)
00952          MOVE +1                  TO  WS-NAME-SW.
00953
00954      MOVE AI-LAST-NAME            TO  WS-NAME-WORK2.
00955      PERFORM 5100-MOVE-NAME  THRU  5190-EXIT.
00956
00957      MOVE AI-FIRST-NAME           TO  WS-NAME-WORK2.
00958      PERFORM 5100-MOVE-NAME  THRU  5190-EXIT.
00959
00960      SET NWA-INDEX  UP BY +1
00961      MOVE AI-MIDDLE-INIT          TO  WS-NAME-WORK2.
00962      PERFORM 5100-MOVE-NAME  THRU  5190-EXIT.
00963
00964  5000-EXIT.
00965      EXIT.
00966  5100-MOVE-NAME SECTION.
00967
00968      IF WS-NAME-SW  GREATER THAN  +1
00969          GO TO 5190-EXIT.
00970
00971      IF WS-NAME-WORK2  =  SPACES
00972          GO TO 5190-EXIT.
00973
00974      SET NWA-INDEX2  TO +1.
00975      SET NWA-INDEX3  TO +2.
00976
00977  5110-MOVE-NAME.
00978
00979      MOVE WS-NW2 (NWA-INDEX2)  TO  WS-NW (NWA-INDEX).
00980
00981      IF NWA-INDEX  LESS THAN  +30
00982          SET NWA-INDEX UP BY +1
00983      ELSE
00984          ADD +2          TO  WS-NAME-SW
00985          GO TO  5190-EXIT.
00986
00987      IF NWA-INDEX2  LESS THAN  +20
00988          SET NWA-INDEX3  UP BY +1
00989          SET NWA-INDEX2  UP BY +1.
00990
00991      IF WS-NW2 (NWA-INDEX2) = SPACES  AND
00992         WS-NW2 (NWA-INDEX3) = SPACES
00993          IF WS-NAME-SW  =  ZERO
00994              MOVE ' '           TO  WS-NW (NWA-INDEX)
00995              SET  NWA-INDEX UP BY +2
00996              MOVE +1            TO  WS-NAME-SW
00997              GO TO 5190-EXIT
00998          ELSE
00999              GO TO 5190-EXIT.
01000
01001      GO TO 5110-MOVE-NAME.
01002
01003  5190-EXIT.
01004      EXIT.
01005      EJECT
01006  5200-SET-ATTRB SECTION.
01007
01008      MOVE AL-SANON  TO  EL1325-NAME-ATTRB    (EL1325-INDEX)
01009                         EL1325-AGE-ATTRB     (EL1325-INDEX)
01010                         EL1325-STA-ATTRB     (EL1325-INDEX)
01011                         EL1325-DATE-INCURRED-ATTRB (EL1325-INDEX)
01012                         EL1325-TYPE-ATTRB    (EL1325-INDEX)
01013                         EL1325-CARRIER-ATTRB (EL1325-INDEX)
01014                         EL1325-CLAIM-ATTRB   (EL1325-INDEX)
01015                         EL1325-CERT-NO-ATTRB (EL1325-INDEX)
01016                         EL1325-ACCOUNT-ATTRB (EL1325-INDEX).
01017
01018  5200-EXIT.
01019      EXIT.
01020      EJECT
01021  7000-PF2-POSITION         SECTION.
01022
01023      
      * EXEC CICS IGNORE CONDITION
01024 *         DUPKEY
01025 *         END-EXEC.
      *    MOVE '"*$                   !   #00005382' TO DFHEIV0
           MOVE X'222A24202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01026
01027      
      * EXEC CICS HANDLE CONDITION
01028 *        NOTFND (8700-NOT-FOUND)
01029 *        END-EXEC.
      *    MOVE '"$I                   ! % #00005386' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303035333836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01030
01031      COMPUTE WS-CALC-RDNXT = PI-SCREEN-COUNT * 15.
01032      MOVE PI-1ST-ALPH-KEY TO PI-ALPH-CLAIM-KEY.
01033      MOVE ZERO TO PI-END-OF-FILE.
01034
01035      IF (PI-OPTION = ZERO) OR
01036         (PI-OPTION = '1') OR
01037         (PI-OPTION = '4')
01038          
      * EXEC CICS STARTBR
01039 *            DATASET   (PI-DSID)
01040 *            RIDFLD    (PI-ALPH-CLAIM-KEY)
01041 *            KEYLENGTH (PI-KEY-LENGTH)
01042 *            GENERIC
01043 *            EQUAL
01044 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    E          &   #00005397' TO DFHEIV0
           MOVE X'262C2020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 PI-ALPH-CLAIM-KEY, 
                 PI-KEY-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01045      ELSE
01046          
      * EXEC CICS STARTBR
01047 *            DATASET   (PI-DSID)
01048 *            RIDFLD    (PI-ALPH-ADMIN-KEY)
01049 *            KEYLENGTH (PI-KEY-LENGTH)
01050 *            GENERIC
01051 *            EQUAL
01052 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    E          &   #00005405' TO DFHEIV0
           MOVE X'262C2020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 PI-ALPH-ADMIN-KEY, 
                 PI-KEY-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01053
01054  7100-READNEXT-PF2.
01055
01056      IF WS-CALC-RDNXT GREATER THAN ZERO
01057        THEN
01058          NEXT SENTENCE
01059        ELSE
01060          GO TO 7999-EXIT.
01061
01062      IF (PI-OPTION = ZERO) OR
01063         (PI-OPTION = '1') OR
01064         (PI-OPTION = '4')
01065          
      * EXEC CICS READNEXT
01066 *            DATASET (PI-DSID)
01067 *            RIDFLD  (PI-ALPH-CLAIM-KEY)
01068 *            SET     (ADDRESS OF ALPHA-INDEX)
01069 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005424' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ALPH-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ALPHA-INDEX TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01070      ELSE
01071          
      * EXEC CICS READNEXT
01072 *            DATASET (PI-DSID)
01073 *            RIDFLD  (PI-ALPH-ADMIN-KEY)
01074 *            SET     (ADDRESS OF ALPHA-INDEX)
01075 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005430' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ALPH-ADMIN-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ALPHA-INDEX TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01076
01077 ******************************************************************
01078 *    IF THE SECURITY CHECKING ROUTINE OR THE INITIAL CHECKING    *
01079 *        IS CHANGED HERE, YOU                                    *
01080 *        MUST ALSO CHANGE THE CORRESPONDING ROUTINE IN           *
01081 *        PARAGRAPH 4020-COMPARE-KEY OR 4030-CHECK-OPTION.        *
01082 *                                     KER/080884.                *
01083 ******************************************************************
01084
01085      IF  PI-NO-CARRIER-SECURITY
01086          GO TO 7130-CHECK-INITAL.
01087
01088      IF  PI-CARRIER-SECURITY GREATER THAN SPACES
01089          IF  AI-CL-CARRIER = PI-CARRIER-SECURITY
01090              NEXT SENTENCE
01091          ELSE
01092              GO TO 7100-READNEXT-PF2.
01093
01094      IF  PI-CLM-CLAIM-NO  NOT EQUAL SPACES
01095          IF  PI-CLM-CLAIM-NO  EQUAL  AI-CL-CLAIM-NUMBER
01096              NEXT SENTENCE
01097          ELSE
01098              GO TO 7100-READNEXT-PF2.
01099
01100      IF  PI-CLM-CERT-NUMBER   NOT EQUAL SPACES
01101          IF PI-CLM-CERT-NUMBER  =   AI-CL-CERTIFICATE-NUMBER
01102              NEXT SENTENCE
01103          ELSE
01104              GO TO 7100-READNEXT-PF2.
01105
01106  7130-CHECK-INITAL.
01107
01108      IF  PI-OPTION  NOT EQUAL '1'
01109          GO TO 7190-COMPUTE.
01110
01111      IF PI-ALPH-FRST-NAME NOT EQUAL TO SPACES
01112         IF PI-ALPH-FRST-NAME NOT EQUAL AI-FIRST-NAME
01113            GO TO 7100-READNEXT-PF2.
01114
01115      IF PI-ALPH-MID-INIT NOT EQUAL TO SPACES
01116          IF PI-ALPH-MID-INIT NOT EQUAL TO AI-MIDDLE-INIT
01117              GO TO 7100-READNEXT-PF2.
01118
01119  7190-COMPUTE.
01120
01121      COMPUTE WS-CALC-RDNXT = WS-CALC-RDNXT - 1.
01122      GO TO 7100-READNEXT-PF2.
01123
01124  7999-EXIT.
01125      EXIT.
01126      EJECT
01127  8000-READ-CONTROL-FILE SECTION.
01128
01129      MOVE ZERO                   TO  WS-NOT-FOUND.
01130      EJECT
01131  8100-SEND-INITIAL-MAP SECTION.
01132
01133      MOVE SAVE-DATE              TO  ADATEO.
01134      MOVE EIBTIME                TO  TIME-IN.
01135      MOVE TIME-OUT               TO  ATIMEO.
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01136
01137      IF EMI-ERROR NOT EQUAL TO ZERO
01138          PERFORM 9900-ERROR-FORMAT.
01139
01140      MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.
01141
01142      IF PI-ORIGINAL-COMPANY-ID NOT EQUAL SPACES
01143         MOVE AL-PABON TO APFK5A APFK6A
01144         MOVE PI-COMPANY-ID TO ACOMPO
01145      ELSE
01146         MOVE SPACES TO ACOMPO
01147         MOVE AL-PADOF TO APFK5A APFK6A.
01148
01149      
      * EXEC CICS SEND
01150 *        FROM   (EL1325AI)
01151 *        MAPSET (WS-MAPSET-NAME)
01152 *        MAP    (WS-MAP-NAME)
01153 *        CURSOR
01154 *        ERASE
01155 *    END-EXEC.
           MOVE LENGTH OF
            EL1325AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005509' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL1325AI, 
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
           
01156
01157      GO TO 9100-RETURN-TRAN.
01158
01159  8100-EXIT.
01160      EXIT.
01161
01162      EJECT
01163  8200-SEND-DATAONLY SECTION.
01164
01165
01166      MOVE SAVE-DATE              TO  ADATEO.
01167      MOVE EIBTIME                TO  TIME-IN.
01168      MOVE TIME-OUT               TO  ATIMEO.
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01169
01170      IF EMI-ERROR NOT EQUAL TO ZERO
01171          PERFORM 9900-ERROR-FORMAT.
01172
01173      MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.
01174
01175      IF PI-ORIGINAL-COMPANY-ID NOT EQUAL SPACES
01176         MOVE AL-PABON TO APFK5A APFK6A
01177         MOVE PI-COMPANY-ID TO ACOMPO
01178      ELSE
01179         MOVE SPACES TO ACOMPO
01180         MOVE AL-PADOF TO APFK5A APFK6A.
01181
01182
01183      
      * EXEC CICS SEND DATAONLY
01184 *        FROM   (EL1325AI)
01185 *        MAPSET (WS-MAPSET-NAME)
01186 *        MAP    (WS-MAP-NAME)
01187 *        CURSOR
01188 *    END-EXEC.
           MOVE LENGTH OF
            EL1325AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00005544' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL1325AI, 
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
           
01189
01190      GO TO 9100-RETURN-TRAN.
01191
01192  8200-EXIT.
01193      EXIT.
01194
01195      EJECT
01196  8300-SEND-TEXT SECTION.
01197
01198      
      * EXEC CICS SEND TEXT
01199 *        FROM   (LOGOFF-TEXT)
01200 *        LENGTH (LOGOFF-LENGTH)
01201 *        ERASE
01202 *        FREEKB
01203 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005559' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353539' TO DFHEIV0(25:11)
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
           
01204
01205      
      * EXEC CICS RETURN
01206 *        END-EXEC.
      *    MOVE '.(                    &   #00005566' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01207
01208  8300-EXIT.
01209      EXIT.
01210
01211      EJECT
01212  8500-DATE-CONVERSION SECTION.
01213
01214      
      * EXEC CICS LINK
01215 *        PROGRAM  ('ELDATCV')
01216 *        COMMAREA (DATE-CONVERSION-DATA)
01217 *        LENGTH   (DC-COMM-LENGTH)
01218 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00005575' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01219
01220  8500-EXIT.
01221      EXIT.
01222
01223      EJECT
01224  8600-NEXT-COMPANY SECTION.
01225 ******************************************************************
01226 ****      READ THE CURRENT COMPANY RECORD TO OBTAIN THE       ****
01227 ****      NEXT COMPANY ID.                                    ****
01228 ******************************************************************
01229
01230      MOVE SPACES                     TO  WS-CLCNTL-KEY.
01231      MOVE PI-COMPANY-ID              TO  WS-CLCNTL-ID.
01232      MOVE '1'                        TO  WS-CLCNTL-TYPE.
01233      MOVE +0                         TO  WS-CLCNTL-SEQ.
01234
01235      PERFORM 8900-READ-CONTROL THRU 8900-EXIT.
01236
01237      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'
01238          PERFORM 8800-INITIALIZE-MAP VARYING EL1325-INDEX
01239            FROM +1 BY +1 UNTIL EL1325-INDEX IS GREATER THAN +16
01240          MOVE ER-0022                 TO  EMI-ERROR
01241          MOVE -1                      TO  ASELL
01242          GO TO 8100-SEND-INITIAL-MAP.
01243
01244      IF EIBAID = DFHPF5
01245          MOVE CF-NEXT-COMPANY-ID      TO  WS-NEXT-COMPANY-ID.
01246
01247      IF EIBAID = DFHPF6
01248          MOVE PI-ORIGINAL-COMPANY-ID  TO  WS-NEXT-COMPANY-ID.
01249
01250      IF PI-PROCESSOR-ID IS EQUAL TO 'LGXX'
01251          GO TO 8600-CONTINUE-NEXT-COMPANY.
01252
01253 ******************************************************************
01254 ****      READ THE CURRENT USER RECORD FOR UPDATE AND REMOVE  ****
01255 ****      THE TERMINAL ID FROM THE RECORD.                    ****
01256 ******************************************************************
01257
01258      MOVE PI-COMPANY-ID               TO  WS-CLCNTL-ID.
01259      MOVE '2'                         TO  WS-CLCNTL-TYPE.
01260      MOVE PI-PROCESSOR-ID             TO  WS-CLCNTL-USER.
01261      MOVE +0                          TO  WS-CLCNTL-SEQ.
01262
01263      PERFORM 8910-READ-CONTROL-UPDATE THRU 8910-EXIT.
01264
01265      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'
01266          MOVE ER-0019                 TO  EMI-ERROR
01267          MOVE -1                      TO  ASELL
01268          GO TO 8200-SEND-DATAONLY.
01269
01270      MOVE SPACES                      TO  CF-CURRENT-TERM-ON.
01271
01272      PERFORM 8920-REWRITE-CONTROL THRU 8920-EXIT.
01273
01274 ******************************************************************
01275 ****      READ THE USER RECORD ON THE "NEXT" COMPANY TO       ****
01276 ****      VERIFY THAT A VALID USER RECORD EXISTS:             ****
01277 ****        1.  MOVE USER CARRIER/ACCOUNT SECURITY TO PI-AREA ****
01278 ****        2.  MOVE USER SECURITY VALUES TO SECURITY CODES   ****
01279 ****            IN WORKING STORAGE.                           ****
01280 ******************************************************************
01281
01282      MOVE WS-NEXT-COMPANY-ID         TO  WS-CLCNTL-ID.
01283      MOVE '2'                        TO  WS-CLCNTL-TYPE.
01284      MOVE PI-PROCESSOR-ID            TO  WS-CLCNTL-USER.
01285      MOVE +0                         TO  WS-CLCNTL-SEQ.
01286
01287      PERFORM 8900-READ-CONTROL THRU 8900-EXIT.
01288
01289      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'
01290          PERFORM 8800-INITIALIZE-MAP VARYING EL1325-INDEX
01291             FROM +1 BY +1 UNTIL EL1325-INDEX IS GREATER THAN +16
01292          MOVE ER-0228                TO  EMI-ERROR
01293          MOVE -1                     TO  ASELL
01294          GO TO 8100-SEND-INITIAL-MAP.
01295
01296      MOVE CF-PROCESSOR-CARRIER       TO  PI-CARRIER-SECURITY.
01297      MOVE CF-PROCESSOR-ACCOUNT       TO  PI-ACCOUNT-SECURITY.
01298      MOVE CF-INDIVIDUAL-APP (1)      TO  SC-CREDIT-CODES.
01299      MOVE CF-INDIVIDUAL-APP (2)      TO  SC-CLAIMS-CODES.
01300      MOVE SC-CLAIMS-DISPLAY (21)     TO  PI-DISPLAY-CAP.
01301      MOVE SC-CLAIMS-UPDATE  (21)     TO  PI-MODIFY-CAP.
01302
01303 ******************************************************************
01304 ****      READ THE USER RECORD ON THE "NEXT" COMPANY FOR      ****
01305 ****      UPDATE AND MOVE THE TERMINAL ID INTO THE RECORD.    ****
01306 ******************************************************************
01307
01308      MOVE WS-NEXT-COMPANY-ID         TO  WS-CLCNTL-ID.
01309      MOVE '2'                        TO  WS-CLCNTL-TYPE.
01310      MOVE PI-PROCESSOR-ID            TO  WS-CLCNTL-USER.
01311      MOVE +0                         TO  WS-CLCNTL-SEQ.
01312
01313      PERFORM 8910-READ-CONTROL-UPDATE THRU 8910-EXIT.
01314
01315      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'
01316          MOVE ER-0228                TO  EMI-ERROR
01317          MOVE -1                     TO  ASELL
01318          GO TO 8200-SEND-DATAONLY.
01319
01320      MOVE EIBTRMID                   TO  CF-CURRENT-TERM-ON.
01321
01322      PERFORM 8920-REWRITE-CONTROL THRU 8920-EXIT.
01323
01324  8600-CONTINUE-NEXT-COMPANY.
01325 ******************************************************************
01326 ****      READ THE NEW COMPANY RECORD TO VERIFY THAT IT       ****
01327 ****      EXISTS AND THEN MOVE SPECIFIC DATA TO PI-AREA.      ****
01328 ******************************************************************
01329
01330      MOVE SPACES                     TO  WS-CLCNTL-KEY.
01331      MOVE WS-NEXT-COMPANY-ID         TO  WS-CLCNTL-ID.
01332      MOVE '1'                        TO  WS-CLCNTL-TYPE.
01333      MOVE +0                         TO  WS-CLCNTL-SEQ.
01334
01335      PERFORM 8900-READ-CONTROL THRU 8900-EXIT.
01336
01337      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'
01338          PERFORM 8800-INITIALIZE-MAP VARYING EL1325-INDEX
01339             FROM +1 BY +1 UNTIL EL1325-INDEX IS GREATER THAN +16
01340          MOVE ER-0089                TO  EMI-ERROR
01341          MOVE -1                     TO  ASELL
01342          GO TO 8100-SEND-INITIAL-MAP.
01343
01344      MOVE SPACES                     TO  PI-PROGRAM-WORK-AREA.
01345
01346      MOVE +1                         TO  PI-START-SW
01347                                          PI-KEY-LENGTH.
01348
01349      MOVE +0                         TO  PI-1ST-TIME-SW
01350                                          PI-LINE-COUNT
01351                                          PI-AIX-RECORD-COUNT
01352                                          PI-BROWSE-SW
01353                                          PI-END-OF-FILE
01354                                          PI-TS-ITEM
01355                                          PI-SCREEN-COUNT.
01356
01357      MOVE ALPHA-INDEX-DSID           TO  PI-DSID.
01358      MOVE CF-COMPANY-CD              TO  PI-COMPANY-CD
01359                                          PI-CK-COMPANY-CD
01360                                          PI-1ST-KEY.
01361      MOVE CF-COMPANY-ID              TO  PI-COMPANY-ID.
01362      MOVE CF-COMPANY-PASSWORD        TO  PI-COMPANY-PASSWORD.
01363      MOVE CF-LGX-CREDIT-USER         TO  PI-CREDIT-USER.
01364      MOVE CF-LGX-CLAIM-USER          TO  PI-CLAIM-USER.
01365      MOVE CF-CERT-ACCESS-CONTROL     TO  PI-CERT-ACCESS-CONTROL.
01366      MOVE CF-CARRIER-CONTROL-LEVEL   TO  PI-CARRIER-CONTROL-LEVEL.
01367      MOVE CF-JOURNAL-FILE-ID         TO  PI-JOURNAL-FILE-ID.
01368      MOVE CF-LOWER-CASE-LETTERS      TO  PI-LOWER-CASE-LETTERS.
01369      MOVE CF-CLAIM-PAID-THRU-TO      TO  PI-CLAIM-PAID-THRU-TO.
01370
01371      MOVE CF-LIFE-OVERRIDE-L1        TO  PI-LIFE-OVERRIDE-L1.
01372      MOVE CF-LIFE-OVERRIDE-L2        TO  PI-LIFE-OVERRIDE-L2.
01373      MOVE CF-LIFE-OVERRIDE-L6        TO  PI-LIFE-OVERRIDE-L6.
01374      MOVE CF-LIFE-OVERRIDE-L12       TO  PI-LIFE-OVERRIDE-L12.
01375
01376      MOVE CF-AH-OVERRIDE-L1          TO  PI-AH-OVERRIDE-L1.
01377      MOVE CF-AH-OVERRIDE-L2          TO  PI-AH-OVERRIDE-L2.
01378      MOVE CF-AH-OVERRIDE-L6          TO  PI-AH-OVERRIDE-L6.
01379      MOVE CF-AH-OVERRIDE-L12         TO  PI-AH-OVERRIDE-L12.
01380
01381      IF  CLAIM-SESSION
01382          MOVE CF-PRINT-ADDRESS-LABELS
01383                                      TO  PI-LABEL-CONTROL
01384
01385      ELSE
01386          IF  CREDIT-SESSION
01387              MOVE CF-CR-PRINT-ADDRESS-LABELS
01388                                      TO  PI-LABEL-CONTROL.
01389
01390  8600-EXIT.
01391      EXIT.
01392
01393      EJECT
01394  8650-WRITE-SECURITY-TEMP-STORE  SECTION.
01395
01396      
      * EXEC CICS HANDLE CONDITION
01397 *        QIDERR  (8651-WRITE-SECURITY)
01398 *    END-EXEC.
      *    MOVE '"$N                   ! & #00005757' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303035373537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01399
01400      MOVE EIBTRMID               TO  QID.
01401
01402      
      * EXEC CICS DELETEQ TS
01403 *        QUEUE   (QID)
01404 *    END-EXEC.
      *    MOVE '*&                    #   #00005763' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01405
01406  8651-WRITE-SECURITY.
01407
01408      
      * EXEC CICS WRITEQ TS
01409 *        QUEUE   (QID)
01410 *        FROM    (SECURITY-CONTROL)
01411 *        LENGTH  (SC-COMM-LENGTH)
01412 *        ITEM    (QID-ITEM)
01413 *    END-EXEC.
      *    MOVE '*" I                  ''   #00005769' TO DFHEIV0
           MOVE X'2A2220492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 QID-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01414
01415      MOVE QID                    TO  PI-SECURITY-TEMP-STORE-ID.
01416
01417      IF PI-PROCESSOR-ID IS EQUAL TO 'LGXX'
01418          MOVE ALL 'Y'            TO  SC-CREDIT-CODES
01419                                      SC-CLAIMS-CODES
01420                                      PI-PROCESSOR-USER-ALMIGHTY.
01421
01422  8650-EXIT.
01423      EXIT.
01424
01425      EJECT
01426  8700-NOT-FOUND SECTION.
01427      PERFORM 8800-INITIALIZE-MAP VARYING EL1325-INDEX
01428        FROM +1 BY +1 UNTIL EL1325-INDEX GREATER THAN +16.
01429      MOVE -1 TO ASELL.
01430      MOVE ER-0284                TO EMI-ERROR.
01431      GO TO 8100-SEND-INITIAL-MAP.
01432  8700-EXIT.
01433      EXIT.
01434
01435  8800-INITIALIZE-MAP SECTION.
01436      MOVE LOW-VALUES TO EL1325-MAP-LINE (EL1325-INDEX).
01437  8800-EXIT.
01438      EXIT.
01439
01440      EJECT
01441  8900-READ-CONTROL SECTION.
01442
01443      
      * EXEC CICS HANDLE CONDITION
01444 *        NOTFND   (8900-NOTFND)
01445 *    END-EXEC.
      *    MOVE '"$I                   ! '' #00005804' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303035383034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01446
01447      
      * EXEC CICS READ
01448 *        DATASET   (ELCNTL-FILE-ID)
01449 *        RIDFLD    (WS-CLCNTL-KEY)
01450 *        SET       (ADDRESS OF CONTROL-FILE)
01451 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005808' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01452
01453      MOVE 'Y'                    TO  WS-CNTL-REC-FOUND-SW.
01454      GO TO 8900-EXIT.
01455
01456  8900-NOTFND.
01457
01458      MOVE 'N'                    TO  WS-CNTL-REC-FOUND-SW.
01459
01460  8900-EXIT.
01461      EXIT.
01462
01463  8910-READ-CONTROL-UPDATE.
01464
01465      
      * EXEC CICS HANDLE CONDITION
01466 *        NOTFND   (8910-NOTFND)
01467 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00005826' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303035383236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01468
01469      
      * EXEC CICS READ
01470 *        DATASET   (ELCNTL-FILE-ID)
01471 *        RIDFLD    (WS-CLCNTL-KEY)
01472 *        SET       (ADDRESS OF CONTROL-FILE)
01473 *        UPDATE
01474 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005830' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01475
01476      MOVE 'Y'                    TO  WS-CNTL-REC-FOUND-SW.
01477      GO TO 8910-EXIT.
01478
01479  8910-NOTFND.
01480      MOVE 'N'                    TO  WS-CNTL-REC-FOUND-SW.
01481
01482  8910-EXIT.
01483      EXIT.
01484
01485  8920-REWRITE-CONTROL.
01486
01487      
      * EXEC CICS REWRITE
01488 *        DATASET   (ELCNTL-FILE-ID)
01489 *        FROM      (CONTROL-FILE)
01490 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005848' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01491
01492  8920-EXIT.
01493      EXIT.
01494
01495      EJECT
01496  9000-RETURN-CICS SECTION.
01497
01498      MOVE 'EL005   '             TO  THIS-PGM.
01499      MOVE EIBAID                 TO  PI-ENTRY-CD-1
01500      GO TO 9300-XCTL.
01501
01502  9000-EXIT.
01503      EXIT.
01504
01505  9100-RETURN-TRAN SECTION.
01506
01507      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO
01508      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO
01509      MOVE EIBAID                 TO  PI-LAST-EIBAID
01510
01511      
      * EXEC CICS RETURN
01512 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01513 *        LENGTH   (PI-COMM-LENGTH)
01514 *        TRANSID  (WS-TRANS-ID)
01515 *    END-EXEC.
      *    MOVE '.(CT                  &   #00005872' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01516
01517  9100-EXIT.
01518      EXIT.
01519
01520  9300-XCTL SECTION.
01521
01522      MOVE DFHENTER               TO  EIBAID
01523
01524      
      * EXEC CICS XCTL
01525 *        PROGRAM  (THIS-PGM)
01526 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01527 *        LENGTH   (PI-COMM-LENGTH)
01528 *    END-EXEC.
      *    MOVE '.$C                   $   #00005885' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01529
01530  9300-EXIT.
01531      EXIT.
01532
01533      EJECT
01534  9400-CLEAR SECTION.
01535
01536      IF PI-RETURN-TO-PROGRAM NOT EQUAL THIS-PGM
01537          MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
01538          MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
01539          MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
01540          MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
01541          MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
01542          MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
01543          MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
01544          MOVE SPACES               TO  PI-SAVED-PROGRAM-6
01545          MOVE PI-RETURN-TO-PROGRAM TO  THIS-PGM.
01546
01547      GO TO 9300-XCTL.
01548
01549  9400-EXIT.
01550      EXIT.
01551
01552  9600-PGMIDERR SECTION.
01553
01554      
      * EXEC CICS HANDLE CONDITION
01555 *        PGMIDERR (8300-SEND-TEXT)
01556 *    END-EXEC.
      *    MOVE '"$L                   ! ) #00005915' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303035393135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01557
01558      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM
01559
01560      MOVE 'EL005   '             TO  THIS-PGM
01561                                      LOGOFF-PGM
01562      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL
01563      MOVE SPACES                 TO  PI-ENTRY-CD-1
01564      GO TO 9300-XCTL.
01565
01566  9600-EXIT.
01567      EXIT.
01568
01569      EJECT
01570  9900-ERROR-FORMAT SECTION.
01571
01572      
      * EXEC CICS LINK
01573 *        PROGRAM  ('EL001')
01574 *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
01575 *        LENGTH   (EMI-COMM-LENGTH)
01576 *    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   ''   #00005933' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01577
01578      MOVE ER-0000                   TO  EMI-ERROR.
01579
01580  9900-EXIT.
01581      EXIT.
01582
01583  9990-ERROR SECTION.
01584
01585      MOVE DFHEIBLK TO EMI-LINE1.
01586      
      * EXEC CICS LINK
01587 *        PROGRAM  ('EL004')
01588 *        COMMAREA (EMI-LINE1)
01589 *        LENGTH   (72)
01590 *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00005947' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01591
01592      GO TO 8100-SEND-INITIAL-MAP.
01593
01594  9990-EXIT.
01595      EXIT.
01596
01597  9995-SECURITY-VIOLATION SECTION.
01598 *                                COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00005976' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393736' TO DFHEIV0(25:11)
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
01599
01600  9995-EXIT.
01601      EXIT.
01602


       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1325' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMIDERR,
                     8700-NOT-FOUND,
                     3600-ENDFILE,
                     3015-DUPKEY,
                     9990-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 8700-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 4990-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8700-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8651-WRITE-SECURITY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8900-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8910-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1325' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
