00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL686 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 09:57:46.
00007 *                            VMOD=2.018
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
00024 *REMARKS.     TRANSACTION - EXG2
00025 *
00026 *        THIS PROGRAM IS USED TO INDICATE THE CHECKS BEING
00027 *    RELEASED TO PRINT.  EACH RELEASE CONSTITUTES A CONTROL GROUP
00028 *    THAT IS REFERENCED BY THE CHECK WRITER (EL687) AND THE CHECK
00029 *    PRINT STARTER (EL686).
00030 *
00031 *    SCREENS     - EL686A - CHECK RELEASE
00032 *
00033 *    ENTERED BY  - EL671  - REPORT MENU
00034 *
00035 *    EXIT TO     - EL671  - RESULT OF CLEAR
00036 *
00037 *    INPUT FILES - ERPYAJ - PENDING PAYMENT AND ADJUSTMENTS
00038 *                  ERCHEK - CHECK MAINTENANCE FILE
00039 *                  ERCMCK - COMMISSION CHECK MAINTENANCE FILE
00040 *                  ELCNTL - CONTROL FILE
00041 *
00042 *    OUTPUT FILES - ERPYAJ - PENDING PAYMENTS AND ADJUSTMENTS
00043 *                   ERCHEK - CHECK MAINTENANCE FILE
00044 *                   ELCNTL - CONTROL FILE
00045 *                   ERCHKQ - CHECK QUEUE
00046 *                   ERCMKQ - COMMISSION CHECK QUEUE
00047 *
00048 *    COMMAREA    - PASSED.
00049 *
030612******************************************************************
030612*                   C H A N G E   L O G
030612*
030612* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
030612*-----------------------------------------------------------------
030612*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
030612* EFFECTIVE    NUMBER
030612*-----------------------------------------------------------------
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
030612******************************************************************
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
00059  77  FILLER  PIC X(32)  VALUE '*    EL686 WORKING STORAGE     *'.
00060  77  FILLER  PIC X(32)  VALUE '************ V/M 2.018 *********'.
00061
00062 *    COPY ELCSCTM.
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
00063 *    COPY ELCSCRTY.
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
00064  01  FILLER                          COMP-3.
00065      05  WS-NDX                      PIC S9(3)       VALUE ZERO.
00066      05  WS-SEQ-NO                   PIC S9(9)       VALUE ZERO.
00067
00068      05  WS-TIME-WORK                PIC S9(7)       VALUE ZERO.
00069      05  WS-TIME                     REDEFINES
00070          WS-TIME-WORK                PIC S9(3)V9(4).
00071      05  WS-HHMM                     REDEFINES
00072          WS-TIME-WORK                PIC S9(5)V99.
00073
00074      05  WS-ERPYAJ-BROWSE-SW         PIC S9          VALUE +0.
00075          88  WS-ERPYAJ-BROWSE-NOT-STARTED            VALUE +0.
00076          88  WS-ERPYAJ-BROWSE-STARTED                VALUE +1.
00077      05  WS-ERCHEK-BROWSE-SW         PIC S9          VALUE +0.
00078          88  WS-ERCHEK-BROWSE-NOT-STARTED            VALUE +0.
00079          88  WS-ERCHEK-BROWSE-STARTED                VALUE +1.
00080      05  WS-ERCMCK-BROWSE-SW         PIC S9          VALUE +0.
00081          88  WS-ERCMCK-BROWSE-NOT-STARTED            VALUE +0.
00082          88  WS-ERCMCK-BROWSE-STARTED                VALUE +1.
00083
00084      05  WS-RELEASED-COUNT           PIC S9(5)       VALUE ZERO.
00085      05  WS-RELEASED-AMOUNT          PIC S9(9)V99    VALUE ZERO.
00086
00087  01  FILLER                          COMP SYNC.
00088
00089      05  SC-ITEM                     PIC S9(4)       VALUE  +1.
00090      05  WS-INDEX                    PIC S9(4)       VALUE ZERO.
00091      05  WS-ERCHKQ-LENGTH            PIC S9(4)       VALUE +100.
00092      05  WS-ERCMKQ-LENGTH            PIC S9(4)       VALUE +1800.
00093      05  WS-TS-LENGTH                PIC S9(4)       VALUE +1920.
00094
00095      05  WS-CHECK-QUE-COUNTER        PIC S9(8)       VALUE ZERO.
00096      05  WS-CHECK-COUNTER            PIC S9(4)       VALUE +10.
00097
00098      05  WS-JOURNAL-FILE-ID          PIC S9(4)       VALUE +1.
00099      05  WS-JOURNAL-RECORD-LENGTH    PIC S9(4)       VALUE +773.
00100
00101  01  FILLER.
00102
00103      05  WS-MAPSET-NAME              PIC X(8)      VALUE 'EL686S'.
00104      05  WS-MAP-NAME                 PIC X(8)      VALUE 'EL686A'.
00105
00106      05  FILLER                      REDEFINES
00107          WS-MAP-NAME.
00108          20  FILLER                  PIC XX.
00109          20  WS-MAP-NUMBER           PIC X(6).
00110
00111      05  DEEDIT-FIELD                PIC X(15).
00112      05  DEEDIT-FIELD-V0  REDEFINES
00113          DEEDIT-FIELD                PIC S9(15).
00114
00115      05  QID.
00116          10  QID-TERM                PIC X(4)   VALUE SPACES.
00117          10  FILLER                  PIC X(4)   VALUE '125D'.
00118
00119      05  TIME-IN                     PIC S9(7).
00120      05  TIME-OUT-R   REDEFINES TIME-IN.
00121          10  FILLER                  PIC X.
00122          10  TIME-OUT                PIC 99V99.
00123          10  FILLER                  PIC X(2).
00124
00125      05  WS-CHECK-NUMBER.
00126          10  FILLER                  PIC X           VALUE ZERO.
00127          10  WS-CHECK-NO             PIC X(6)        VALUE ZERO.
00128
00129      05  WS-CONTROL-FILE-KEY.
00130          10  WS-CFK-COMPANY-ID       PIC X(3)        VALUE SPACES.
00131          10  WS-CFK-RECORD-TYPE      PIC X           VALUE SPACES.
00132          10  FILLER                  PIC XX          VALUE SPACES.
00133          10  WS-CFK-BENEFIT-NO       PIC XX          VALUE SPACES.
00134          10  WS-CFK-SEQUENCE-NO      PIC S9(4)       VALUE ZERO
00135                                      COMP.
00136
00137      05  WS-PENDING-PAYMENTS-KEY.
00138          10  WS-PPK-COMPANY-CD       PIC X.
00139          10  WS-PPK-CARRIER          PIC X.
00140          10  WS-PPK-GROUPING         PIC X(6).
00141          10  WS-PPK-FIN-RESP         PIC X(10).
00142          10  WS-PPK-ACCOUNT          PIC X(10).
00143          10  WS-PPK-FILE-SEQ-NO      PIC S9(8)
00144                                      COMP.
00145          10  WS-PPK-RECORD-TYPE      PIC X.
00146
00147      05  WS-COMCK-MAINT-KEY.
00148          10  WS-CMK-COMPANY-CD       PIC X.
00149          10  WS-CMK-CARRIER          PIC X.
00150          10  WS-CMK-GROUPING         PIC X(6).
00151          10  WS-CMK-PAYEE            PIC X(10).
00152          10  WS-CMK-PAYEE-SEQ        PIC S9(4)      COMP.
00153          10  WS-CMK-SEQUENCE-NO      PIC S9(4)      COMP.
00154
00155      05  WS-CHECK-MAINT-KEY.
00156          10  WS-CHK-COMPANY-CD       PIC X.
00157          10  WS-CHK-CARRIER          PIC X.
00158          10  WS-CHK-GROUPING         PIC X(6).
00159          10  WS-CHK-STATE            PIC XX.
00160          10  WS-CHK-ACCOUNT          PIC X(10).
00161          10  WS-CHK-CERT-EFF-DT      PIC XX.
00162          10  WS-CHK-CERT-NO.
00163              15  WS-CHK-CERT-PRIME   PIC X(10).
00164              15  WS-CHK-CERT-SFX     PIC X.
00165          10  WS-CHK-SEQ-NO           PIC S9(4)
00166                                      COMP.
00167
00168      05  THIS-PGM                    PIC X(8) VALUE 'EL686'.
00169
00170      05  EL001                       PIC X(8) VALUE 'EL001'.
00171      05  EL004                       PIC X(8) VALUE 'EL004'.
00172      05  EL005                       PIC X(8) VALUE 'EL005'.
00173      05  EL010                       PIC X(8) VALUE 'EL010'.
00174      05  EL126                       PIC X(8) VALUE 'EL126'.
00175      05  ELDATCV                     PIC X(8) VALUE 'ELDATCV'.
00176
00177      05  WS-ERCHKQ-DSID              PIC X(8) VALUE 'ERCHKQ'.
00178      05  WS-ERCHEK-DSID              PIC X(8) VALUE 'ERCHEK'.
00179      05  WS-ERCMKQ-DSID              PIC X(8) VALUE 'ERCMKQ'.
00180      05  WS-ERCMCK-DSID              PIC X(8) VALUE 'ERCMCK'.
00181      05  WS-ERPYAJ-DSID              PIC X(8) VALUE 'ERPYAJ'.
00182      05  WS-ELCNTL-DSID              PIC X(8) VALUE 'ELCNTL'.
00183
00184      05  WS-JOURNAL-TYPE-ID          PIC XX          VALUE 'EL'.
00185
00186      05  WS-SPACES                   PIC X           VALUE SPACES.
00187
00188      05  WS-TRANS-ID                 PIC X(4)        VALUE 'EXG2'.
00189      05  WS-PRINT-TRANS-ID           PIC X(4)        VALUE 'EXG9'.
00190
00191      05  WS-ERROR-FLAG-AREA.
00192          10  ER-0002                 PIC 9(4)        VALUE 0002.
00193          10  ER-0004                 PIC 9(4)        VALUE 0004.
00194          10  ER-0008                 PIC 9(4)        VALUE 0008.
00195          10  ER-0029                 PIC 9(4)        VALUE 0029.
00196          10  ER-0070                 PIC 9(4)        VALUE 0070.
00197          10  ER-0078                 PIC 9(4)        VALUE 0078.
00198          10  ER-0330                 PIC 9(4)        VALUE 0330.
00199          10  ER-0331                 PIC 9(4)        VALUE 0331.
00200          10  ER-0348                 PIC 9(4)        VALUE 0348.
00201          10  ER-0568                 PIC 9(4)        VALUE 0568.
00202          10  ER-0766                 PIC 9(4)        VALUE 0766.
00203          10  ER-3048                 PIC 9(4)        VALUE 3048.
00204
00205      05  WS-TEXT-MESSAGE-LENGTH      PIC S9(4)       VALUE +70
00206                                      COMP SYNC.
00207
00208      05  WS-TEXT-MESSAGE             PIC X(70)       VALUE SPACES.
00209
00210      05  WS-TOTAL-LINE1.
00211          10  FILLER                  PIC X(14)       VALUE
00212              'CONTROL GROUP'.
00213          10  WS-TL1-CONTROL-GROUP    PIC 9(7)-.
00214          10  WS-TL1-RELEASE          PIC X(20)       VALUE
00215              ' RELEASED'.
00216
00217      05  WS-TOTAL-LINE2.
00218          10  WS-TL1-COUNT            PIC ZZ,ZZ9.
00219          10  FILLER                  PIC X(6)        VALUE
00220              ' CHECK'.
00221          10  WS-TL1-PLURAL           PIC X           VALUE
00222              'S'.
00223          10  FILLER                  PIC X(18)       VALUE
00224              ' IN THE AMOUNT OF'.
00225          10  WS-TL1-AMOUNT           PIC Z,ZZZ,ZZ9.99.
00226
00227      05  WS-TEMP-STORAGE-KEY.
00228          10  WS-TS-TERM-ID           PIC X(4).
00229          10  FILLER                  PIC X(4)        VALUE '685'.
00230
00231      05  WS-TEMP-STORAGE-ITEM        PIC S9(4)       VALUE ZERO
00232                                      COMP SYNC.
00233
00234      EJECT
00235 *    COPY ELCINTF.
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
00236
00237      12  FILLER                      REDEFINES
00238          PI-PROGRAM-WORK-AREA.
00239              16  FILLER              PIC X.
00240              16  PI-CK-CONTROL-NO    PIC S9(8)      COMP.
00241              16  PI-CURRENT-DATE     PIC X(8).
00242              16  PI-CURRENT-DATE-BIN PIC XX.
00243              16  PI-PROC-SW          PIC S9.
00244                  88  PI-SCREEN-PROCESSED        VALUE +1.
00245              16  PI-CERT-SW          PIC S9.
00246                  88  PI-CERT-NOT-MATCHED        VALUE +0.
00247                  88  PI-CERT-MATCHED            VALUE +1.
00248              16  PI-CHECK-AMOUNT     PIC S9(7)  COMP-3.
00249              16  BIN-EFFDT           PIC XX   OCCURS 15.
00250              16  FILLER              PIC X(589).
00251
00252 *    COPY EL686S.
       01  EL686AI.
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
           05  AOPTIONL PIC S9(0004) COMP.
           05  AOPTIONF PIC  X(0001).
           05  FILLER REDEFINES AOPTIONF.
               10  AOPTIONA PIC  X(0001).
           05  AOPTIONI PIC  X(0001).
      *    -------------------------------
           05  AAMTL PIC S9(0004) COMP.
           05  AAMTF PIC  X(0001).
           05  FILLER REDEFINES AAMTF.
               10  AAMTA PIC  X(0001).
           05  AAMTI PIC  9(7).
      *    -------------------------------
           05  ABYL PIC S9(0004) COMP.
           05  ABYF PIC  X(0001).
           05  FILLER REDEFINES ABYF.
               10  ABYA PIC  X(0001).
           05  ABYI PIC  X(0004).
      *    -------------------------------
           05  ACARRL PIC S9(0004) COMP.
           05  ACARRF PIC  X(0001).
           05  FILLER REDEFINES ACARRF.
               10  ACARRA PIC  X(0001).
           05  ACARRI PIC  X(0001).
      *    -------------------------------
           05  AGROUPL PIC S9(0004) COMP.
           05  AGROUPF PIC  X(0001).
           05  FILLER REDEFINES AGROUPF.
               10  AGROUPA PIC  X(0001).
           05  AGROUPI PIC  X(0006).
      *    -------------------------------
           05  ASTATL PIC S9(0004) COMP.
           05  ASTATF PIC  X(0001).
           05  FILLER REDEFINES ASTATF.
               10  ASTATA PIC  X(0001).
           05  ASTATI PIC  X(0002).
      *    -------------------------------
           05  AACCTL PIC S9(0004) COMP.
           05  AACCTF PIC  X(0001).
           05  FILLER REDEFINES AACCTF.
               10  AACCTA PIC  X(0001).
           05  AACCTI PIC  X(0010).
      *    -------------------------------
           05  CERTO01L PIC S9(0004) COMP.
           05  CERTO01F PIC  X(0001).
           05  FILLER REDEFINES CERTO01F.
               10  CERTO01A PIC  X(0001).
           05  CERTO01I PIC  X(0011).
      *    -------------------------------
           05  EFFDT01L PIC S9(0004) COMP.
           05  EFFDT01F PIC  X(0001).
           05  FILLER REDEFINES EFFDT01F.
               10  EFFDT01A PIC  X(0001).
           05  EFFDT01I PIC  X(0008).
      *    -------------------------------
           05  CERTO02L PIC S9(0004) COMP.
           05  CERTO02F PIC  X(0001).
           05  FILLER REDEFINES CERTO02F.
               10  CERTO02A PIC  X(0001).
           05  CERTO02I PIC  X(0011).
      *    -------------------------------
           05  EFFDT02L PIC S9(0004) COMP.
           05  EFFDT02F PIC  X(0001).
           05  FILLER REDEFINES EFFDT02F.
               10  EFFDT02A PIC  X(0001).
           05  EFFDT02I PIC  X(0008).
      *    -------------------------------
           05  CERTO03L PIC S9(0004) COMP.
           05  CERTO03F PIC  X(0001).
           05  FILLER REDEFINES CERTO03F.
               10  CERTO03A PIC  X(0001).
           05  CERTO03I PIC  X(0011).
      *    -------------------------------
           05  EFFDT03L PIC S9(0004) COMP.
           05  EFFDT03F PIC  X(0001).
           05  FILLER REDEFINES EFFDT03F.
               10  EFFDT03A PIC  X(0001).
           05  EFFDT03I PIC  X(0008).
      *    -------------------------------
           05  CERTO04L PIC S9(0004) COMP.
           05  CERTO04F PIC  X(0001).
           05  FILLER REDEFINES CERTO04F.
               10  CERTO04A PIC  X(0001).
           05  CERTO04I PIC  X(0011).
      *    -------------------------------
           05  EFFDT04L PIC S9(0004) COMP.
           05  EFFDT04F PIC  X(0001).
           05  FILLER REDEFINES EFFDT04F.
               10  EFFDT04A PIC  X(0001).
           05  EFFDT04I PIC  X(0008).
      *    -------------------------------
           05  CERTO05L PIC S9(0004) COMP.
           05  CERTO05F PIC  X(0001).
           05  FILLER REDEFINES CERTO05F.
               10  CERTO05A PIC  X(0001).
           05  CERTO05I PIC  X(0011).
      *    -------------------------------
           05  EFFDT05L PIC S9(0004) COMP.
           05  EFFDT05F PIC  X(0001).
           05  FILLER REDEFINES EFFDT05F.
               10  EFFDT05A PIC  X(0001).
           05  EFFDT05I PIC  X(0008).
      *    -------------------------------
           05  CERTO06L PIC S9(0004) COMP.
           05  CERTO06F PIC  X(0001).
           05  FILLER REDEFINES CERTO06F.
               10  CERTO06A PIC  X(0001).
           05  CERTO06I PIC  X(0011).
      *    -------------------------------
           05  EFFDT06L PIC S9(0004) COMP.
           05  EFFDT06F PIC  X(0001).
           05  FILLER REDEFINES EFFDT06F.
               10  EFFDT06A PIC  X(0001).
           05  EFFDT06I PIC  X(0008).
      *    -------------------------------
           05  CERTO07L PIC S9(0004) COMP.
           05  CERTO07F PIC  X(0001).
           05  FILLER REDEFINES CERTO07F.
               10  CERTO07A PIC  X(0001).
           05  CERTO07I PIC  X(0011).
      *    -------------------------------
           05  EFFDT07L PIC S9(0004) COMP.
           05  EFFDT07F PIC  X(0001).
           05  FILLER REDEFINES EFFDT07F.
               10  EFFDT07A PIC  X(0001).
           05  EFFDT07I PIC  X(0008).
      *    -------------------------------
           05  CERTO08L PIC S9(0004) COMP.
           05  CERTO08F PIC  X(0001).
           05  FILLER REDEFINES CERTO08F.
               10  CERTO08A PIC  X(0001).
           05  CERTO08I PIC  X(0011).
      *    -------------------------------
           05  EFFDT08L PIC S9(0004) COMP.
           05  EFFDT08F PIC  X(0001).
           05  FILLER REDEFINES EFFDT08F.
               10  EFFDT08A PIC  X(0001).
           05  EFFDT08I PIC  X(0008).
      *    -------------------------------
           05  CERTO09L PIC S9(0004) COMP.
           05  CERTO09F PIC  X(0001).
           05  FILLER REDEFINES CERTO09F.
               10  CERTO09A PIC  X(0001).
           05  CERTO09I PIC  X(0011).
      *    -------------------------------
           05  EFFDT09L PIC S9(0004) COMP.
           05  EFFDT09F PIC  X(0001).
           05  FILLER REDEFINES EFFDT09F.
               10  EFFDT09A PIC  X(0001).
           05  EFFDT09I PIC  X(0008).
      *    -------------------------------
           05  CERTO10L PIC S9(0004) COMP.
           05  CERTO10F PIC  X(0001).
           05  FILLER REDEFINES CERTO10F.
               10  CERTO10A PIC  X(0001).
           05  CERTO10I PIC  X(0011).
      *    -------------------------------
           05  EFFDT10L PIC S9(0004) COMP.
           05  EFFDT10F PIC  X(0001).
           05  FILLER REDEFINES EFFDT10F.
               10  EFFDT10A PIC  X(0001).
           05  EFFDT10I PIC  X(0008).
      *    -------------------------------
           05  CERTO11L PIC S9(0004) COMP.
           05  CERTO11F PIC  X(0001).
           05  FILLER REDEFINES CERTO11F.
               10  CERTO11A PIC  X(0001).
           05  CERTO11I PIC  X(0011).
      *    -------------------------------
           05  EFFDT11L PIC S9(0004) COMP.
           05  EFFDT11F PIC  X(0001).
           05  FILLER REDEFINES EFFDT11F.
               10  EFFDT11A PIC  X(0001).
           05  EFFDT11I PIC  X(0008).
      *    -------------------------------
           05  CERTO12L PIC S9(0004) COMP.
           05  CERTO12F PIC  X(0001).
           05  FILLER REDEFINES CERTO12F.
               10  CERTO12A PIC  X(0001).
           05  CERTO12I PIC  X(0011).
      *    -------------------------------
           05  EFFDT12L PIC S9(0004) COMP.
           05  EFFDT12F PIC  X(0001).
           05  FILLER REDEFINES EFFDT12F.
               10  EFFDT12A PIC  X(0001).
           05  EFFDT12I PIC  X(0008).
      *    -------------------------------
           05  CERTO13L PIC S9(0004) COMP.
           05  CERTO13F PIC  X(0001).
           05  FILLER REDEFINES CERTO13F.
               10  CERTO13A PIC  X(0001).
           05  CERTO13I PIC  X(0011).
      *    -------------------------------
           05  EFFDT13L PIC S9(0004) COMP.
           05  EFFDT13F PIC  X(0001).
           05  FILLER REDEFINES EFFDT13F.
               10  EFFDT13A PIC  X(0001).
           05  EFFDT13I PIC  X(0008).
      *    -------------------------------
           05  CERTO14L PIC S9(0004) COMP.
           05  CERTO14F PIC  X(0001).
           05  FILLER REDEFINES CERTO14F.
               10  CERTO14A PIC  X(0001).
           05  CERTO14I PIC  X(0011).
      *    -------------------------------
           05  EFFDT14L PIC S9(0004) COMP.
           05  EFFDT14F PIC  X(0001).
           05  FILLER REDEFINES EFFDT14F.
               10  EFFDT14A PIC  X(0001).
           05  EFFDT14I PIC  X(0008).
      *    -------------------------------
           05  CERTO15L PIC S9(0004) COMP.
           05  CERTO15F PIC  X(0001).
           05  FILLER REDEFINES CERTO15F.
               10  CERTO15A PIC  X(0001).
           05  CERTO15I PIC  X(0011).
      *    -------------------------------
           05  EFFDT15L PIC S9(0004) COMP.
           05  EFFDT15F PIC  X(0001).
           05  FILLER REDEFINES EFFDT15F.
               10  EFFDT15A PIC  X(0001).
           05  EFFDT15I PIC  X(0008).
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
           05  APFKI PIC  9(2).
      *    -------------------------------
           05  ACOMPL PIC S9(0004) COMP.
           05  ACOMPF PIC  X(0001).
           05  FILLER REDEFINES ACOMPF.
               10  ACOMPA PIC  X(0001).
           05  ACOMPI PIC  X(0014).
      *    -------------------------------
           05  APF1L PIC S9(0004) COMP.
           05  APF1F PIC  X(0001).
           05  FILLER REDEFINES APF1F.
               10  APF1A PIC  X(0001).
           05  APF1I PIC  X(0028).
       01  EL686AO REDEFINES EL686AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AOPTIONO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAMTO PIC  ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTATO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO01O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT01O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO02O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT02O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO03O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT03O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO04O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT04O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO05O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT05O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO06O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT06O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO07O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT07O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO08O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT08O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO09O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT09O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO10O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT10O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO11O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT11O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO12O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT12O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO13O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT13O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO14O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT14O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO15O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT15O PIC  X(0008).
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
           05  FILLER            PIC  X(0003).
           05  ACOMPO PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APF1O PIC  X(0028).
      *    -------------------------------
00253
00254      EJECT
00255 *    COPY ELCJPFX.
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
00256                 PIC X(750).
00257
00258      EJECT
00259 *    COPY ELCEMIB.
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
00260
00261      EJECT
00262 *    COPY ELCDATE.
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
00263
00264      EJECT
00265 *    COPY ELCLOGOF.
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
00267 *    COPY ELCATTR.
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
00268
00269 *    COPY ELCAID.
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
00270
00271  01  FILLER                      REDEFINES
00272      DFHAID.
00273
00274      05  FILLER                      PIC X(8).
00275
00276      05  PF-VALUES                   PIC X
00277          OCCURS 24 TIMES.
00278
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
00280
00281  01  DFHCOMMAREA                     PIC X(1024).
00282
00283 *01 PARMLIST                         COMP SYNC.
00284 *    05  FILLER                      PIC S9(9).
00285 *    05  ERCHKQ-POINTER              PIC S9(9).
00286 *    05  ELCNTL-POINTER              PIC S9(9).
00287 *    05  ERPYAJ-POINTER              PIC S9(9).
00288 *    05  ERCHEK-POINTER              PIC S9(9).
00289 *    05  ERCMCK-POINTER              PIC S9(9).
00290 *    05  ERCMKQ-POINTER              PIC S9(9).
00291
00292      EJECT
00293 *    COPY ERCCHKQ.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCHKQ                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK QUE FILE FOR THE CREDIT SYSTEM      *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 100  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ERCHKQ                         RKP=2,LEN=7    *
00013 *       ALTERNATE PATH  = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  CHECK-QUE.
00019      12  CQ-RECORD-ID                PIC XX.
00020          88  VALID-CQ-ID                     VALUE 'CQ'.
00021
00022      12  CQ-CONTROL-PRIMARY.
00023          16  CQ-COMPANY-CD           PIC X.
00024          16  CQ-CONTROL-NUMBER       PIC S9(8)       COMP.
00025          16  CQ-SEQUENCE-NUMBER      PIC S9(4)       COMP.
00026
00027      12  CQ-ENTRY-TYPE               PIC X.
00028              88  CHECK-ON-QUE           VALUE 'Q'.
00029              88  ALIGNMENT-CHECK        VALUE 'A'.
00030              88  MANUAL-CHECK           VALUE 'M'.
00031              88  SPOILED-CHECK          VALUE 'S'.
00032              88  VOIDED-CHECK           VALUE 'V'.
00033              88  PAYMENT-ABORTED        VALUE 'X'.
00034
00035      12  CQ-CREDIT-MASTER-CNTL       PIC X(50).
00036
00037      12  CQ-CREDIT-PYAJ-CNTL         REDEFINES
00038          CQ-CREDIT-MASTER-CNTL.
00039          16  CQ-PYAJ-CARRIER         PIC X.
00040          16  CQ-PYAJ-GROUPING        PIC X(6).
00041          16  CQ-PYAJ-FIN-RESP        PIC X(10).
00042          16  CQ-PYAJ-ACCOUNT         PIC X(10).
00043          16  CQ-PYAJ-SEQ             PIC S9(8)  COMP.
00044          16  CQ-PYAJ-REC-TYPE        PIC X.
00045          16  FILLER                  PIC X(18).
00046
00047      12  CQ-CREDIT-CHEK-CNTL         REDEFINES
00048          CQ-CREDIT-MASTER-CNTL.
00049          16  CQ-CHEK-CARRIER         PIC X.
00050          16  CQ-CHEK-GROUPING        PIC X(6).
00051          16  CQ-CHEK-STATE           PIC XX.
00052          16  CQ-CHEK-ACCOUNT         PIC X(10).
00053          16  CQ-CHEK-CERT-EFF-DT     PIC XX.
00054          16  CQ-CHEK-CERT-NO.
00055              20  CQ-CHEK-CERT-PRIME  PIC X(10).
00056              20  CQ-CHEK-CERT-SFX    PIC X.
00057          16  CQ-CHEK-SEQ-NO          PIC S9(4)       COMP.
00058          16  CQ-CHEK-FIN-RESP        PIC X(10).
00059          16  FILLER                  PIC X(06).
00060
00061      12  CQ-CHECK-NUMBER             PIC X(7).
00062      12  CQ-CHECK-AMOUNT             PIC S9(7)V99    COMP-3.
00063      12  CQ-PAYMENT-TYPE             PIC X.
00064              88  CQ-BILLING-CREDIT         VALUE '1'.
00065              88  CQ-REFUND-PMT             VALUE '2'.
00066              88  CQ-CHECK-MAINT-PMT        VALUE '3'.
00067      12  CQ-VOID-INDICATOR           PIC X.
00068              88  CHECK-IS-VOID             VALUE 'V'.
00069      12  CQ-TIMES-PRINTED            PIC S9(4)       COMP.
00070      12  CQ-PRINT-AT-HHMM            PIC S9(4)       COMP.
00071      12  CQ-CHECK-BY-USER            PIC X(4).
00072      12  CQ-PRE-NUMBERING-SW         PIC X.
00073        88  CHECKS-WERE-NOT-PRE-NUMBERED    VALUE SPACE.
00074        88  CHECKS-WERE-PRE-NUMBERED        VALUE '1'.
00075
00076      12  CQ-CHECK-WRITTEN-DT         PIC XX.
00077      12  CQ-LAST-UPDATED-BY          PIC S9(4)       COMP.
00078      12  CQ-ACCOUNT-AGENT            PIC X(10).
00079      12  CQ-CHECK-VOIDED-DT          PIC XX.
00080
00081      12  CQ-LETTERS-IND              PIC X.
00082          88  CQ-LETTERS-REQUIRED           VALUE 'Y'.
00083
00084 ******************************************************************
00294
00295      EJECT
00296 *    COPY ELCCNTL.
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
00297
00298      EJECT
00299 *    COPY ERCPYAJ.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCPYAJ                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.015                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING PAYMENT AND ADJUSTMENTS           *
00008 *                                                                *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 200  RECFORM = FIXED                           *
00012 *                                                                *
00013 *   BASE CLUSTER = ERPYAJ                         RKP=2,LEN=33   *
00014 *       ALTERNATE PATHS = NONE                                   *
00015 *                                                                *
00016 *   LOG = YES                                                    *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 ******************************************************************
042303******************************************************************
042303*                   C H A N G E   L O G
042303*
042303* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
042303*-----------------------------------------------------------------
042303*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
042303* EFFECTIVE    NUMBER
042303*-----------------------------------------------------------------
042303* 042303                   PEMA ADD PROCESSING FOR DUE PREM ADJS
060205* 060205                   PEMA ADD ERCOMP TYPE TO ERPYAJ
042303******************************************************************
00019
00020  01  PENDING-PAY-ADJ.
00021      12  PY-RECORD-ID                     PIC XX.
00022          88  VALID-PY-ID                        VALUE 'PY'.
00023
00024      12  PY-CONTROL-PRIMARY.
00025          16  PY-COMPANY-CD                PIC X.
00026          16  PY-CARRIER                   PIC X.
00027          16  PY-GROUPING                  PIC X(6).
00028          16  PY-FIN-RESP                  PIC X(10).
00029          16  PY-ACCOUNT                   PIC X(10).
00030          16  PY-PRODUCER REDEFINES PY-ACCOUNT
00031                                           PIC X(10).
00032          16  PY-FILE-SEQ-NO               PIC S9(8)     COMP.
00033          16  PY-RECORD-TYPE               PIC X.
00034              88  PY-REMIT-RECEIVED            VALUE 'R'.
00035              88  PY-DEPOSIT                   VALUE 'D'.
00036              88  PY-CHARGE-TO-AGENT           VALUE 'C'.
00037              88  PY-ADJ-REM-RECEIVED          VALUE 'S'.
00038              88  PY-ADJ-DEPOSIT               VALUE 'T'.
00039              88  PY-ADJ-CHG-TO-AGT            VALUE 'U'.
00040              88  PY-ADD-TO-YTD-COMP           VALUE 'X'.
00041              88  PY-SUBTRACT-YTD-COMP         VALUE 'Y'.
00042              88  PY-ADD-TO-BALANCE            VALUE 'Z'.
00043              88  PY-FICA-ENTRY                VALUE 'F'.
00044              88  PY-REMIT-IND-GROUPING        VALUE 'G'.
00045              88  PY-POLICY-FEE                VALUE 'W'.
042303             88  PY-DUE-PREM-ADJ              VALUE 'P'.
00046
00047      12  PY-PYMT-TYPE                     PIC X.
00048              88  PY-NEW-BUS-PYMT              VALUE 'B'.
00049              88  PY-REINS-PYMT                VALUE 'R'.
00050              88  PY-EXP-PYMT                  VALUE 'E'.
00051
00052      12  PY-BIL-INV                       PIC X(6).
00053      12  PY-REF-NO                        PIC X(12).
00054
00055      12  PY-LAST-MAINT-DT                 PIC XX.
00056      12  PY-LAST-MAINT-BY                 PIC X(4).
00057      12  PY-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00058
00059      12  PY-PYADJ-RECORD.
00060          16  PY-ENTRY-AMT                 PIC S9(7)V99  COMP-3.
00061          16  PY-ENTRY-COMMENT             PIC X(30).
CIDMOD         16  PY-GL-DATA      REDEFINES PY-ENTRY-COMMENT.
CIDMOD             20  PY-GL-ACCOUNT            PIC X(10).
CIDMOD             20  PY-GL-STATE              PIC X(02).
CIDMOD             20  PY-GL-CANC-SW            PIC X(01).
CIDMOD                 88  PY-GL-CANC-SW-ON     VALUE 'Y'.
CIDMOD                 88  PY-GL-CANC-SW-OFF    VALUE 'N'.
CIDMOD             20  PY-GL-COMMENT            PIC X(10).
CIDMOD             20  FILLER      REDEFINES PY-GL-COMMENT.
CIDMOD                 24  PY-GL-CHECK-NO       PIC 9(06).
CIDMOD                 24  FILLER               PIC X(04).
CIDMOD             20  FILLER                   PIC X(07).
00074          16  PY-SAVE-ACCOUNT              PIC X(10).
00075          16  PY-SAVE-TYPE                 PIC X(01).
00076
00077          16  PY-LETTERS.
00078              20  PY-LETTER OCCURS 3 TIMES
00079                            INDEXED BY PY-LET-NDX
00080                                           PIC X(04).
00081
060205         16  PY-ERCOMP-TYPE               PIC X.
060205             88  PY-ACCOUNT-TYPE              VALUE 'A'.
060205             88  PY-GA-TYPE                   VALUE 'G'.
060205             88  PY-BANK-TYPE                 VALUE 'B'.
060205         16  FILLER                       PIC X(05).
00083
00084      12  PY-RECORD-STATUS.
00085          16  PY-CREDIT-SELECT-DT          PIC XX.
00086          16  PY-CREDIT-ACCEPT-DT          PIC XX.
00087          16  PY-BILLED-DATE               PIC XX.
00088          16  PY-REPORTED-DT               PIC XX.
00089          16  PY-PMT-APPLIED               PIC X.
00090              88  PY-ACCOUNT-PMT               VALUE 'A'.
00091              88  PY-GA-PMT                    VALUE 'G'.
00092              88  PY-OVWRITE-PMT               VALUE 'O'.
00093              88  PY-NON-AR-PMT                VALUE 'N'.
00094          16  FILLER                       PIC X(5).
00095          16  PY-INPUT-DT                  PIC XX.
00096          16  PY-CHECK-NUMBER              PIC X(6).
00097          16  PY-VOID-SW                   PIC X.
00098              88  PY-CHECK-VOIDED              VALUE 'V'.
00099          16  PY-CHECK-ORIGIN-SW           PIC X.
00100              88  PY-BILLING-CHECK             VALUE 'B'.
00101              88  PY-REFUND-CHECK              VALUE 'R'.
00102              88  PY-GA-CHECK                  VALUE 'G'.
00103              88  PY-CHECK-WRITTEN             VALUE 'W'.
00104              88  PY-CHECK-REVERSAL            VALUE 'V'.
00105          16  PY-CHECK-WRITTEN-DT          PIC XX.
00106          16  PY-CHECK-QUE-CONTROL         PIC S9(8) COMP.
00107          16  PY-CHECK-QUE-SEQUENCE        PIC S9(4) COMP.
00108          16  PY-BILL-FLAG                 PIC X.
00109              88  PY-BILLED                    VALUE 'B'.
00110          16  PY-AR-FLAG                   PIC X.
00111              88  PY-AR-CYCLE                  VALUE 'C'.
00112              88  PY-AR-MONTH-END              VALUE 'M'.
00113          16  PY-AR-DATE                   PIC XX.
00114
00115      12  PY-GL-CODES.
00116          16  PY-GL-DB                     PIC X(14).
00117          16  PY-GL-CR                     PIC X(14).
00118          16  PY-GL-FLAG                   PIC X.
00119          16  PY-GL-DATE                   PIC XX.
00120
00121      12  PY-CANCEL-FEE-FLAG               PIC X(2).
00122      12  FILLER                           PIC X(3).
00123 ******************************************************************
00300
00301      EJECT
00302 *    COPY ERCCHEK.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCHEK                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.008                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK RECORDS                             *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 600    RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERCHEK             RKP=2,LEN=35          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  CHECK-RECORDS.
00019      12  CH-RECORD-ID                      PIC XX.
00020          88  VALID-CH-ID                      VALUE 'CH'.
00021
00022      12  CH-CONTROL-PRIMARY.
00023          16  CH-COMPANY-CD                 PIC X.
00024          16  CH-CARRIER                    PIC X.
00025          16  CH-GROUPING                   PIC X(6).
00026          16  CH-STATE                      PIC XX.
00027          16  CH-ACCOUNT                    PIC X(10).
00028          16  CH-CERT-EFF-DT                PIC XX.
00029          16  CH-CERT-NO.
00030              20  CH-CERT-PRIME             PIC X(10).
00031              20  CH-CERT-SFX               PIC X.
00032          16  CH-SEQUENCE-NO                PIC S9(4)     COMP.
00033
00034      12  CH-RECORDED-DT                    PIC XX.
00035      12  CH-RECORDED-BY                    PIC X(4).
00036      12  CH-LAST-MAINT-HHMMSS              PIC S9(6)     COMP-3.
00037
00038      12  CH-AMOUNT-PAID                    PIC S9(7)V99  COMP-3.
00039      12  CH-CHECK-NO                       PIC X(7).
00040      12  CH-REASON-FOR-CHECK               PIC X(25).
00041      12  CH-CHECK-WRITTEN-DT               PIC XX.
00042      12  CH-OFFLINE-CHECK-IND              PIC X.
00043          88  MANUAL-CHECK-WRITTEN             VALUE 'Y'.
00044
00045      12  CH-PAYEE-INFO.
00046          16  CH-PAYEE-NAME-1               PIC X(30).
00047          16  CH-PAYEE-NAME-2               PIC X(30).
00048          16  CH-PAYEE-ADDRESS-1            PIC X(30).
00049          16  CH-PAYEE-ADDRESS-2            PIC X(30).
00050          16  CH-PAYEE-CITY-ST              PIC X(30).
00051          16  CH-PAYEE-ZIP-CODE.
00052              20  CH-PAYEE-ZIP.
00053                  24  CH-ZIP-PRI-1ST        PIC X.
00054                      88  CH-CANADIAN-POST-CODE
00055                                            VALUES 'A' THRU 'Z'.
00056                  24  FILLER                PIC X(4).
00057              20  CH-PAYEE-ZIP-EXT          PIC X(4).
00058          16  CH-CANADIAN-POSTAL-CODE REDEFINES CH-PAYEE-ZIP-CODE.
00059              20  CH-CAN-POSTAL-1           PIC XXX.
00060              20  CH-CAN-POSTAL-2           PIC XXX.
00061              20  FILLER                    PIC XXX.
00062
00063      12  CH-CHECK-STUB-TEXT.
00064          16  CH-STUB-LINE-1                PIC X(50).
00065          16  CH-STUB-LINE-2                PIC X(50).
00066          16  CH-STUB-LINE-3                PIC X(50).
00067          16  FILLER.
00068            18  CH-STUB-LINE-4              PIC X(20).
00069            18  CH-LIENHOLDER-NAME          PIC X(30).
00070
00071      12  CH-COMPENSATION-CONTROL.
00072          16  CH-COMP-CARRIER               PIC X.
00073          16  CH-COMP-GROUPING              PIC X(6).
00074          16  CH-COMP-FIN-RESP              PIC X(10).
00075          16  CH-COMP-ACCOUNT               PIC X(10).
00076
00077      12  CH-CREDIT-SELECT-DT               PIC XX.
00078      12  CH-CREDIT-ACCEPT-DT               PIC XX.
00079      12  CH-PAYEE-CODE                     PIC X(6).
00080
00081      12  CH-VOID-DATA.
00082          20  CH-VOID-DT                    PIC XX.
00083          20  CH-VOID-BY                    PIC X(4).
00084          20  CH-VOID-REASON                PIC X(25).
00085
00086      12  CH-CHECK-QUE-CONTROL              PIC S9(8)     COMP.
00087              88  PAYMENT-NOT-QUEUED           VALUE ZERO.
00088      12  CH-CHECK-QUE-SEQUENCE             PIC S9(4)     COMP.
00089
00090      12  CH-CHECK-REFERENCE                PIC X(12).
00091      12  CH-CHECK-ORIGIN-SW                PIC X.
00092              88  CH-REFUND-CHECK              VALUE 'R'.
00093              88  CH-MAINT-CHECK               VALUE 'M'.
00094
00095      12  CH-CANC-DT                        PIC XX.
00096      12  CH-LF-REFUND                      PIC S9(7)V99  COMP-3.
00097      12  CH-AH-REFUND                      PIC S9(7)V99  COMP-3.
00098
00099      12  CH-INSURED-NAME                   PIC X(28).
00100
00101      12  CH-DEDUCT-WITHHELD                PIC S9(5)V99  COMP-3.
00102      12  CH-ADDITIONAL-CHARGE              PIC S9(5)V99  COMP-3.
00103
00104      12  CH-LETTER-TABLE.
00105          16  CH-LETTERS OCCURS 3 TIMES
00106                         INDEXED BY CH-LT-NDX
00107                                            PIC X(04).
00108
00109      12  FILLER                            PIC X(07).
00110
00303
00304      EJECT
00305 *    COPY ERCCMCK.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCMCK                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.012                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = COMMISSION CHECK RECORDS                  *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 2000   RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERCMCK             RKP=2,LEN=26          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = NO                                                     *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  COMM-CHECK-RECORDS.
00019      12  CK-RECORD-ID                      PIC XX.
00020          88  VALID-CK-ID                      VALUE 'CK'.
00021
00022      12  CK-CONTROL-PRIMARY.
00023          16  CK-COMPANY-CD                 PIC X.
00024          16  CK-CSR                        PIC X(4).
00025          16  CK-CARRIER                    PIC X.
00026          16  CK-GROUPING                   PIC X(6).
00027          16  CK-PAYEE                      PIC X(10).
00028          16  CK-PAYEE-SEQ                  PIC S9(4)     COMP.
00029          16  CK-SEQUENCE-NO                PIC S9(4)     COMP.
00030
00031      12  CK-RECORDED-DT                    PIC XX.
00032      12  CK-RECORDED-BY                    PIC X(4).
00033      12  CK-LAST-MAINT-HHMMSS              PIC S9(6)     COMP-3.
00034
00035      12  CK-AMOUNT-PAID                    PIC S9(7)V99  COMP-3.
00036      12  CK-RECORD-TYPE                    PIC X.
00037          88  CK-DETAIL                        VALUE 'D'.
00038          88  CK-TEXT                          VALUE 'T'.
00039      12  CK-CHECK-NO                       PIC X(6).
00040      12  CK-CHECK-WRITTEN-DT               PIC XX.
00041      12  CK-PAYEE-INFO.
00042          16  CK-PAYEE-NAME                 PIC X(30).
00043          16  CK-PAYEE-ADDRESS-1            PIC X(30).
00044          16  CK-PAYEE-ADDRESS-2            PIC X(30).
00045          16  CK-PAYEE-CITY-ST              PIC X(30).
00046          16  CK-PAYEE-ZIP-CODE.
00047              20  CK-PAYEE-ZIP              PIC X(5).
00048              20  CK-PAYEE-ZIP-EXT          PIC X(4).
00049
00050      12  CK-CHECK-STUB-DATA.
00051          16  CK-CHECK-STUB-INFO  OCCURS  15  TIMES.
00052              20  CK-STUB-LINE.
00053                  24  CK-STUB-COMMENT       PIC X(23).
00054                  24  CK-ACCT-AGENT         PIC X(10).
00055                  24  CK-INVOICE            PIC X(6).
00056                  24  CK-REFERENCE          PIC X(12).
00057                  24  CK-LEDGER-NO          PIC X(14).
00058                  24  CK-DETAIL-AMT         PIC S9(7)V99 COMP-3.
00059                  24  CK-LAST-MAINT-APPLIED PIC X.
00060                  24  CK-NON-AR-ITEM        PIC X.
00061                  24  FILLER                PIC X(19).
00062
00063              20  CK-CHECK-WORK-CONTROL.
00064                   24  CK-CKWK-CSR          PIC X(4).
00065                   24  CK-CKWK-CARRIER      PIC X.
00066                   24  CK-CKWK-GROUPING     PIC X(6).
00067                   24  CK-CKWK-PAYEE        PIC X(10).
00068                   24  CK-CKWK-PAYEE-SEQ    PIC S9(4) COMP.
00069                   24  CK-CKWK-SEQ-NO       PIC S9(4) COMP.
00070              20  CK-PAYMENT-TYPE           PIC X.
00071              20  CK-PYAJ-PMT-APPLIED       PIC X.
00072
00073      12  CK-CHECK-STUB-TEXT  REDEFINES CK-CHECK-STUB-DATA.
00074          16  CK-CHECK-TEXT-ITEMS OCCURS 3 TIMES.
00075              20  CK-STUB-TEXT              PIC X(70).
00076          16  CK-STUB-FILL-AREA             PIC X(1560).
00077
00078      12  CK-CHECK-QUE-CONTROL.
00079          20  CK-QUE-CONTROL-NUMBER         PIC S9(8) COMP.
00080          20  CK-QUE-SEQ-NO                 PIC S9(4) COMP.
00081
00082      12  CK-CREDIT-SELECT-DT               PIC XX.
00083      12  CK-CREDIT-ACCEPT-DT               PIC XX.
00084
00085      12  CK-VOID-DATA.
00086          20  CK-VOID-DT                    PIC XX.
00087          20  CK-VOID-REASON                PIC X(25).
00088
00089      12  CK-AR-STATEMENT-DT                PIC XX.
00090      12  CK-PMT-APPLIED                    PIC X.
00091      12  CK-ACH-PAYMENT                    PIC X.
00092           88 PAID-BY-ACH                   VALUE 'P'.
00093           88 NOT-PAID-BY-ACH               VALUE 'N'.
00094      12  FILLER                            PIC X(08).
00095
00306
00307      EJECT
00308 *    COPY ERCCMKQ.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCMKQ                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.014                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK QUE FILE FOR THE COMMISSION         *
00008 *                      CHECK OF THE CREDIT SYSTEM                *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 1800 RECFORM = FIXED                           *
00012 *                                                                *
00013 *   BASE CLUSTER = ERCMKQ                         RKP=2,LEN=7    *
00014 *       ALTERNATE PATH  = ERCMKQ2  (BY PAYEE CONTRO AND          *
00015 *                                      CONTROL NUMBER)           *
00016 *                                                 RKP=9,LEN=30   *
00017 *                                                                *
00018 *   LOG = NO                                                     *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00020 ******************************************************************
00021  01  COMMISSION-CHECK-QUE.
00022      12  MQ-RECORD-ID                PIC XX.
00023          88  VALID-MQ-ID                     VALUE 'MQ'.
00024
00025      12  MQ-CONTROL-PRIMARY.
00026          16  MQ-COMPANY-CD           PIC X.
00027          16  MQ-CONTROL-NUMBER       PIC S9(8)       COMP.
00028          16  MQ-SEQUENCE-NUMBER      PIC S9(4)       COMP.
00029
00030      12  MQ-CONTROL-BY-PAYEE.
00031          16  MQ-COMPANY-CD-A1        PIC X.
00032          16  MQ-CSR-A1               PIC X(4).
00033          16  MQ-CARRIER-A1           PIC X.
00034          16  MQ-GROUPING-A1          PIC X(6).
00035          16  MQ-PAYEE-A1             PIC X(10).
00036          16  MQ-PAYEE-SEQ-A1         PIC S9(4)       COMP.
00037          16  MQ-CONTROL-NUMBER-A1    PIC S9(8)       COMP.
00038          16  MQ-SEQUENCE-NUMBER-A1   PIC S9(4)       COMP.
00039
00040      12  MQ-ENTRY-TYPE               PIC X.
00041              88  CHECK-ON-QUE           VALUE 'Q'.
00042              88  ALIGNMENT-CHECK        VALUE 'A'.
00043              88  SPOILED-CHECK          VALUE 'S'.
00044              88  PAYMENT-ABORTED        VALUE 'X'.
00045              88  ACH-PAYMENT            VALUE 'P'.
00046
00047      12  FILLER                      PIC X(10).
00048
00049      12  MQ-CREDIT-CHEK-CNTL.
00050          16  MQ-CHEK-CSR             PIC X(4).
00051          16  MQ-CHEK-CARRIER         PIC X.
00052          16  MQ-CHEK-GROUPING        PIC X(6).
00053          16  MQ-CHEK-PAYEE           PIC X(10).
00054          16  MQ-CHEK-PAYEE-SEQ       PIC S9(4)       COMP.
00055          16  MQ-CHEK-SEQ-NO          PIC S9(4)       COMP.
00056
00057      12  FILLER                      PIC X(10).
00058
00059      12  MQ-PAYEE-INFO.
00060          16  MQ-PAYEE-NAME           PIC X(30).
00061          16  MQ-PAYEE-ADDRESS-1      PIC X(30).
00062          16  MQ-PAYEE-ADDRESS-2      PIC X(30).
00063          16  MQ-PAYEE-CITY-ST        PIC X(30).
00064          16  MQ-PAYEE-ZIP-CODE.
00065              20  MQ-PAYEE-ZIP.
00066                  24  FILLER          PIC X(1).
00067                      88 MQ-PAYEE-CANADIAN-POST-CODE
00068                                      VALUE 'A' THRU 'Z'.
00069                  24  FILLER          PIC X(4).
00070              20  MQ-PAYEE-ZIP-EXT    PIC X(4).
00071          16  MQ-PAYEE-CANADIAN-POSTAL-CODES
00072                  REDEFINES MQ-PAYEE-ZIP-CODE.
00073              20  MQ-PAY-CAN-POSTAL-CD-1
00074                                      PIC X(3).
00075              20  MQ-PAY-CAN-POSTAL-CD-2
00076                                      PIC X(3).
00077              20  FILLER              PIC X(3).
00078
00079      12  MQ-CREDIT-PYAJ-CNTL.
00080          16  MQ-PYAJ-CARRIER         PIC X.
00081          16  MQ-PYAJ-GROUPING        PIC X(6).
00082          16  MQ-PYAJ-FIN-RESP        PIC X(10).
00083          16  FILLER                  PIC X(6).
00084
00085      12  MQ-CHECK-NUMBER             PIC X(6).
00086      12  MQ-CHECK-AMOUNT             PIC S9(7)V99    COMP-3.
00087      12  MQ-NUMBER-OF-CK-STUBS       PIC S9(3)       COMP-3.
00088      12  MQ-VOID-DT                  PIC XX.
00089      12  MQ-TIMES-PRINTED            PIC S9(4)       COMP.
00090      12  MQ-PRINT-AT-HHMM            PIC S9(4)       COMP.
00091      12  MQ-CHECK-BY-USER            PIC X(4).
00092      12  MQ-PRE-NUMBERING-SW         PIC X.
00093        88  CHECKS-WERE-NOT-PRE-NUMBERED    VALUE SPACE.
00094        88  CHECKS-WERE-PRE-NUMBERED        VALUE '1'.
00095
00096      12  MQ-CHECK-WRITTEN-DT         PIC XX.
00097      12  MQ-ACH-WRITTEN-DT REDEFINES  MQ-CHECK-WRITTEN-DT
00098                                      PIC XX.
00099      12  MQ-LAST-MAINT-BY            PIC X(4).
00100      12  MQ-LAST-MAINT-HHMMSS        PIC S9(6)       COMP-3.
00101      12  MQ-LAST-MAINT-DT            PIC XX.
00102      12  MQ-CHECK-RELEASE-DT         PIC XX.
00103      12  MQ-RECORD-TYPE              PIC X.
00104          88  MQ-DETAIL                     VALUE 'D'.
00105          88  MQ-TEXT                       VALUE 'T'.
00106
00107      12  MQ-DETAIL-INFORMATION.
00108          16  MQ-DETAIL-INFO        OCCURS 15 TIMES.
00109              20  MQ-CHECK-STUB-LINE.
00110                  24  MQ-STUB-COMMENT        PIC X(23).
00111                  24  MQ-ACCT-AGENT          PIC X(10).
00112                  24  MQ-INVOICE             PIC X(6).
00113                  24  MQ-REFERENCE           PIC X(12).
00114                  24  MQ-LEDGER-NO           PIC X(14).
00115                  24  MQ-PYAJ-AMT            PIC S9(7)V99 COMP-3.
00116                  24  MQ-PYAJ-REC-TYPE       PIC X.
00117                  24  MQ-PYAJ-SEQ            PIC S9(8)    COMP.
00118                  24  MQ-PAYMENT-TYPE        PIC X.
00119                  24  MQ-PYAJ-PMT-APPLIED    PIC X.
00120                  24  MQ-LAST-MAINT-APPLIED  PIC X.
00121                  24  MQ-NON-AR-ITEM         PIC X.
00122                  24  FILLER                 PIC X(19).
00123
00124      12  MQ-CHECK-STUB-TEXT REDEFINES MQ-DETAIL-INFORMATION.
00125          16  MQ-CHECK-TEXT-ITEMS   OCCURS 3 TIMES.
00126              20  MQ-STUB-TEXT        PIC X(70).
00127          16  MQ-STUB-FILLER          PIC X(1260).
00128
00129      12  MQ-CREDIT-SELECT-DATE       PIC XX.
00130      12  MQ-CREDIT-ACCEPT-DATE       PIC XX.
00131
00132      12  MQ-AR-STATEMENT-DT          PIC XX.
00133      12  MQ-CO-TYPE                  PIC X.
00134
00135      12  MQ-STARTING-CHECK-NUMBER    PIC X(06).
00136      12  FILLER                      PIC X(41).
00137 ******************************************************************
00309
00310      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CHECK-QUE
                                CONTROL-FILE PENDING-PAY-ADJ
                                CHECK-RECORDS COMM-CHECK-RECORDS
                                COMMISSION-CHECK-QUE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL686' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00312
00313      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00314
00315 *    NOTE *******************************************************
00316 *         *                                                     *
00317 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
00318 *         *  FROM ANOTHER MODULE.                               *
00319 *         *                                                     *
00320 *         *******************************************************.
00321
00322      IF EIBCALEN NOT GREATER THAN ZERO
00323          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00324          GO TO 8300-SEND-TEXT.
00325
00326      
      * EXEC CICS HANDLE CONDITION
00327 *        PGMIDERR   (9600-PGMIDERR)
00328 *        NOTFND     (0140-NOT-FOUND)
00329 *        ENDFILE    (0400-END-OF-SEARCH)
00330 *        ENQBUSY    (0910-ENQ-BUSY)
00331 *        TERMIDERR  (7010-TERMID-ERROR)
00332 *        TRANSIDERR (7020-TRANS-ERROR)
00333 *        ERROR      (9990-ERROR)
00334 *    END-EXEC.
      *    MOVE '"$LI'')[\.             ! " #00003616' TO DFHEIV0
           MOVE X'22244C4927295B5C2E202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033363136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00335
00336      MOVE EIBTRMID               TO  QID-TERM.
00337      MOVE +2                     TO  EMI-NUMBER-OF-LINES
00338                                      EMI-SWITCH2.
00339
00340      MOVE EIBTIME                TO  WS-TIME-WORK
00341                                      TIME-IN.
00342
00343      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00344          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00345              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00346              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00347              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00348              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00349              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00350              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00351              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00352              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
00353            ELSE
00354              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00355              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00356              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00357              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00358              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00359              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00360              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00361              MOVE SPACES               TO  PI-SAVED-PROGRAM-6
00362        ELSE
00363          GO TO 0110-PROCESS-INPUT.
00364
00365  EJECT
00366  0100-INITIALIZE.
00367
00368 *    NOTE *******************************************************
00369 *         *                                                     *
00370 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      *
00371 *         *  INTERFACE BLOCK FOR THIS MODULE.                   *
00372 *         *                                                     *
00373 *         *******************************************************.
00374
00375      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.
00376      MOVE ZERO                   TO  PI-PROC-SW.
00377
00378      MOVE LOW-VALUES             TO  EL686AI.
00379      MOVE -1                     TO  AOPTIONL.
00380
00381      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00382      MOVE '5'                    TO  DC-OPTION-CODE.
00383      PERFORM 8500-DATE-CONVERSION.
00384      MOVE DC-BIN-DATE-1          TO  PI-CURRENT-DATE-BIN.
00385      MOVE DC-GREG-DATE-1-EDIT    TO  PI-CURRENT-DATE.
00386
00387      PERFORM 8100-SEND-INITIAL-MAP.
00388
00389      EJECT
00390  0110-PROCESS-INPUT.
00391
00392 *    NOTE *******************************************************
00393 *         *                                                     *
00394 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *
00395 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *
00396 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
00397 *         *                                                     *
00398 *         *******************************************************.
00399
00400      IF EIBAID EQUAL TO DFHCLEAR
00401          GO TO 9400-CLEAR.
00402
00403      IF EIBAID EQUAL TO (DFHPA1 OR
00404                          DFHPA2 OR
00405                          DFHPA3)
00406          MOVE ER-0008            TO  EMI-ERROR
00407          MOVE -1                 TO  APFKL
00408          PERFORM 8200-SEND-DATAONLY.
00409
00410      
      * EXEC CICS RECEIVE
00411 *        INTO   (EL686AI)
00412 *        MAPSET (WS-MAPSET-NAME)
00413 *        MAP    (WS-MAP-NAME) END-EXEC.
           MOVE LENGTH OF
            EL686AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003700' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL686AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00414
00415      IF APFKL IS GREATER THAN ZERO
00416          IF EIBAID NOT = DFHENTER
00417              MOVE ER-0004        TO  EMI-ERROR
00418              MOVE AL-UNBOF       TO  APFKA
00419              MOVE -1             TO  APFKL
00420              PERFORM 8200-SEND-DATAONLY
00421            ELSE
00422              IF APFKO IS NUMERIC
00423                AND APFKO IS GREATER THAN ZERO
00424                AND APFKO IS LESS THAN '25'
00425                  MOVE PF-VALUES (APFKI)  TO  EIBAID
00426                ELSE
00427                  MOVE ER-0029        TO  EMI-ERROR
00428                  MOVE AL-UNBOF       TO  APFKA
00429                  MOVE -1             TO  APFKL
00430                  PERFORM 8200-SEND-DATAONLY.
00431
00432      IF EIBAID IS EQUAL TO DFHPF12
00433          MOVE EL010              TO  THIS-PGM
00434          GO TO 9300-XCTL.
00435
00436      IF EIBAID IS EQUAL TO DFHPF23
00437          GO TO 9000-RETURN-CICS.
00438
00439      IF EIBAID IS EQUAL TO DFHPF24
00440          MOVE EL126              TO  THIS-PGM
00441          GO TO 9300-XCTL.
00442
00443      IF EIBAID = DFHPF1 AND
00444         PI-SCREEN-PROCESSED
00445           GO TO 0200-PROCESS-CHECK-RELEASE.
00446
00447      IF EIBAID NOT = DFHENTER
00448          MOVE ER-0008            TO  EMI-ERROR
00449          MOVE -1                 TO  APFKL
00450          PERFORM 8200-SEND-DATAONLY.
00451
00452      EJECT
00453
00454 *    NOTE *******************************************************
00455 *         *                                                     *
00456 *         *      OPTION          MEANING                        *
00457 *         *                                                     *
00458 *         *        1         OPTIONS 2 THRU 4                   *
00459 *         *        2         BILLING PAYMENTS                   *
00460 *         *        3         REFUND REIMBURSMENTS               *
00461 *         *        4         MAINTENANCE PAYMENTS               *
00462 *         *                  ACCOUNT RECEIVABLE PAYMENTS        *
00463 *         *                  5 = CHECKS   6 = ACH               *
00464 *         *                                                     *
00465 *         *******************************************************.
00466
00467      IF AOPTIONL GREATER ZERO  AND
00468         AOPTIONI = '1' OR '2' OR '3' OR '4' OR '5' OR '6'
00469          MOVE AL-UNNON           TO  AOPTIONA
00470        ELSE
00471          MOVE -1                 TO  AOPTIONL
00472          MOVE AL-UNBON           TO  AOPTIONA
00473          MOVE ER-0330            TO  EMI-ERROR
00474          PERFORM 9900-ERROR-FORMAT.
00475
00476      IF PI-COMPANY-ID = 'POS'
00477         AND ACARRL NOT GREATER ZERO
00478              MOVE -1             TO  ACARRL
00479              MOVE AL-UABOF       TO  ACARRA
00480              MOVE ER-0568        TO  EMI-ERROR
00481              PERFORM 9900-ERROR-FORMAT.
00482
00483      IF AAMTL GREATER ZERO
00484         
      * EXEC CICS BIF DEEDIT
00485 *            FIELD (AAMTI)
00486 *            LENGTH (7)
00487 *       END-EXEC
           MOVE 7
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003774' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AAMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00488         IF AAMTI NOT NUMERIC
00489             MOVE -1             TO  AAMTL
00490             MOVE AL-UABON       TO  AAMTA
00491             MOVE ER-0078        TO  EMI-ERROR
00492             PERFORM 9900-ERROR-FORMAT
00493          ELSE
00494             MOVE AAMTI          TO  PI-CHECK-AMOUNT
00495
00496             MOVE AL-UANON       TO  AAMTA.
00497
00498 *    NOTE *******************************************************
00499 *         *                                                     *
00500 *         *  CHECK TO SEE THAT THE CERTIFICATE NUMBER HAS       *
00501 *         *  THE EFFECTIVE DATE ENTERED.                        *
00502 *         *                                                     *
00503 *         *******************************************************.
00504
00505      IF CERTO01I = SPACES
00506           MOVE +0               TO CERTO01L.
00507      IF EFFDT01I = SPACES
00508           MOVE +0               TO EFFDT01L.
00509      IF CERTO02I = SPACES
00510           MOVE +0               TO CERTO02L.
00511      IF EFFDT02I = SPACES
00512           MOVE +0               TO EFFDT02L.
00513      IF CERTO03I = SPACES
00514           MOVE +0               TO CERTO03L.
00515      IF EFFDT03I = SPACES
00516           MOVE +0               TO EFFDT03L.
00517      IF CERTO04I = SPACES
00518           MOVE +0               TO CERTO04L.
00519      IF EFFDT04I = SPACES
00520           MOVE +0               TO EFFDT04L.
00521      IF CERTO05I = SPACES
00522           MOVE +0               TO CERTO05L.
00523      IF EFFDT05I = SPACES
00524           MOVE +0               TO EFFDT05L.
00525      IF CERTO06I = SPACES
00526           MOVE +0               TO CERTO06L.
00527      IF EFFDT06I = SPACES
00528           MOVE +0               TO EFFDT06L.
00529      IF CERTO07I = SPACES
00530           MOVE +0               TO CERTO07L.
00531      IF EFFDT07I = SPACES
00532           MOVE +0               TO EFFDT07L.
00533      IF CERTO08I = SPACES
00534           MOVE +0               TO CERTO08L.
00535      IF EFFDT08I = SPACES
00536           MOVE +0               TO EFFDT08L.
00537      IF CERTO09I = SPACES
00538           MOVE +0               TO CERTO09L.
00539      IF EFFDT09I = SPACES
00540           MOVE +0               TO EFFDT09L.
00541      IF CERTO10I = SPACES
00542           MOVE +0               TO CERTO10L.
00543      IF EFFDT10I = SPACES
00544           MOVE +0               TO EFFDT10L.
00545      IF CERTO11I = SPACES
00546           MOVE +0               TO CERTO11L.
00547      IF EFFDT11I = SPACES
00548           MOVE +0               TO EFFDT11L.
00549      IF CERTO12I = SPACES
00550           MOVE +0               TO CERTO12L.
00551      IF EFFDT12I = SPACES
00552           MOVE +0               TO EFFDT12L.
00553      IF CERTO13I = SPACES
00554           MOVE +0               TO CERTO13L.
00555      IF EFFDT13I = SPACES
00556           MOVE +0               TO EFFDT13L.
00557      IF CERTO14I = SPACES
00558           MOVE +0               TO CERTO14L.
00559      IF EFFDT14I = SPACES
00560           MOVE +0               TO EFFDT14L.
00561      IF CERTO15I = SPACES
00562           MOVE +0               TO CERTO15L.
00563      IF EFFDT15I = SPACES
00564           MOVE +0               TO EFFDT15L.
00565
00566      IF (CERTO01L     > 0 AND EFFDT01L NOT > 0) OR
00567         (CERTO01L NOT > 0 AND EFFDT01L     > 0) OR
00568         (CERTO02L     > 0 AND EFFDT02L NOT > 0) OR
00569         (CERTO02L NOT > 0 AND EFFDT02L     > 0) OR
00570         (CERTO03L     > 0 AND EFFDT03L NOT > 0) OR
00571         (CERTO03L NOT > 0 AND EFFDT03L     > 0) OR
00572         (CERTO04L     > 0 AND EFFDT04L NOT > 0) OR
00573         (CERTO04L NOT > 0 AND EFFDT04L     > 0) OR
00574         (CERTO05L     > 0 AND EFFDT05L NOT > 0) OR
00575         (CERTO05L NOT > 0 AND EFFDT05L     > 0) OR
00576         (CERTO06L     > 0 AND EFFDT06L NOT > 0) OR
00577         (CERTO06L NOT > 0 AND EFFDT06L     > 0) OR
00578         (CERTO07L     > 0 AND EFFDT07L NOT > 0) OR
00579         (CERTO07L NOT > 0 AND EFFDT07L     > 0) OR
00580         (CERTO08L     > 0 AND EFFDT08L NOT > 0) OR
00581         (CERTO08L NOT > 0 AND EFFDT08L     > 0) OR
00582         (CERTO09L     > 0 AND EFFDT09L NOT > 0) OR
00583         (CERTO09L NOT > 0 AND EFFDT09L     > 0) OR
00584         (CERTO10L     > 0 AND EFFDT10L NOT > 0) OR
00585         (CERTO10L NOT > 0 AND EFFDT10L     > 0) OR
00586         (CERTO11L     > 0 AND EFFDT11L NOT > 0) OR
00587         (CERTO11L NOT > 0 AND EFFDT11L     > 0) OR
00588         (CERTO12L     > 0 AND EFFDT12L NOT > 0) OR
00589         (CERTO12L NOT > 0 AND EFFDT12L     > 0) OR
00590         (CERTO13L     > 0 AND EFFDT13L NOT > 0) OR
00591         (CERTO13L NOT > 0 AND EFFDT13L     > 0) OR
00592         (CERTO14L     > 0 AND EFFDT14L NOT > 0) OR
00593         (CERTO14L NOT > 0 AND EFFDT14L     > 0) OR
00594         (CERTO15L     > 0 AND EFFDT15L NOT > 0) OR
00595         (CERTO15L NOT > 0 AND EFFDT15L     > 0)
00596          MOVE ER-0766            TO EMI-ERROR
00597          MOVE -1                 TO CERTO01L
00598          PERFORM 9900-ERROR-FORMAT.
00599
00600      IF EFFDT01L GREATER ZERO
00601          MOVE EFFDT01I                 TO  DEEDIT-FIELD
00602          PERFORM 8600-DEEDIT
00603          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY
00604          MOVE '4'                      TO  DC-OPTION-CODE
00605          PERFORM 8500-DATE-CONVERSION
00606          IF DATE-CONVERSION-ERROR
00607              MOVE ER-0348              TO  EMI-ERROR
00608              MOVE -1                   TO  EFFDT01L
00609              MOVE AL-UABON             TO  EFFDT01A
00610              PERFORM 9900-ERROR-FORMAT
00611            ELSE
00612              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT01I
00613              MOVE AL-UANON             TO  EFFDT01A
00614              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (1).
00615
00616      IF EFFDT02L GREATER ZERO
00617          MOVE EFFDT02I                 TO  DEEDIT-FIELD
00618          PERFORM 8600-DEEDIT
00619          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY
00620          MOVE '4'                      TO  DC-OPTION-CODE
00621          PERFORM 8500-DATE-CONVERSION
00622          IF DATE-CONVERSION-ERROR
00623              MOVE ER-0348              TO  EMI-ERROR
00624              MOVE -1                   TO  EFFDT02L
00625              MOVE AL-UABON             TO  EFFDT02A
00626              PERFORM 9900-ERROR-FORMAT
00627            ELSE
00628              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT02I
00629              MOVE AL-UANON             TO  EFFDT02A
00630              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (2).
00631
00632      IF EFFDT03L GREATER ZERO
00633          MOVE EFFDT03I                 TO  DEEDIT-FIELD
00634          PERFORM 8600-DEEDIT
00635          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY
00636          MOVE '4'                      TO  DC-OPTION-CODE
00637          PERFORM 8500-DATE-CONVERSION
00638          IF DATE-CONVERSION-ERROR
00639              MOVE ER-0348              TO  EMI-ERROR
00640              MOVE -1                   TO  EFFDT03L
00641              MOVE AL-UABON             TO  EFFDT03A
00642              PERFORM 9900-ERROR-FORMAT
00643            ELSE
00644              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT03I
00645              MOVE AL-UANON             TO  EFFDT03A
00646              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (3).
00647
00648      IF EFFDT04L GREATER ZERO
00649          MOVE EFFDT04I                 TO  DEEDIT-FIELD
00650          PERFORM 8600-DEEDIT
00651          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY
00652          MOVE '4'                      TO  DC-OPTION-CODE
00653          PERFORM 8500-DATE-CONVERSION
00654          IF DATE-CONVERSION-ERROR
00655              MOVE ER-0348              TO  EMI-ERROR
00656              MOVE -1                   TO  EFFDT04L
00657              MOVE AL-UABON             TO  EFFDT04A
00658              PERFORM 9900-ERROR-FORMAT
00659            ELSE
00660              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT04I
00661              MOVE AL-UANON             TO  EFFDT04A
00662              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (4).
00663
00664      IF EFFDT05L GREATER ZERO
00665          MOVE EFFDT05I                 TO  DEEDIT-FIELD
00666          PERFORM 8600-DEEDIT
00667          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY
00668          MOVE '4'                      TO  DC-OPTION-CODE
00669          PERFORM 8500-DATE-CONVERSION
00670          IF DATE-CONVERSION-ERROR
00671              MOVE ER-0348              TO  EMI-ERROR
00672              MOVE -1                   TO  EFFDT05L
00673              MOVE AL-UABON             TO  EFFDT05A
00674              PERFORM 9900-ERROR-FORMAT
00675            ELSE
00676              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT05I
00677              MOVE AL-UANON             TO  EFFDT05A
00678              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (5).
00679
00680      IF EFFDT06L GREATER ZERO
00681          MOVE EFFDT06I                 TO  DEEDIT-FIELD
00682          PERFORM 8600-DEEDIT
00683          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY
00684          MOVE '4'                      TO  DC-OPTION-CODE
00685          PERFORM 8500-DATE-CONVERSION
00686          IF DATE-CONVERSION-ERROR
00687              MOVE ER-0348              TO  EMI-ERROR
00688              MOVE -1                   TO  EFFDT06L
00689              MOVE AL-UABON             TO  EFFDT06A
00690              PERFORM 9900-ERROR-FORMAT
00691            ELSE
00692              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT06I
00693              MOVE AL-UANON             TO  EFFDT06A
00694              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (6).
00695
00696      IF EFFDT07L GREATER ZERO
00697          MOVE EFFDT07I                 TO  DEEDIT-FIELD
00698          PERFORM 8600-DEEDIT
00699          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY
00700          MOVE '4'                      TO  DC-OPTION-CODE
00701          PERFORM 8500-DATE-CONVERSION
00702          IF DATE-CONVERSION-ERROR
00703              MOVE ER-0348              TO  EMI-ERROR
00704              MOVE -1                   TO  EFFDT07L
00705              MOVE AL-UABON             TO  EFFDT07A
00706              PERFORM 9900-ERROR-FORMAT
00707            ELSE
00708              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT07I
00709              MOVE AL-UANON             TO  EFFDT07A
00710              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (7).
00711
00712      IF EFFDT08L GREATER ZERO
00713          MOVE EFFDT08I                 TO  DEEDIT-FIELD
00714          PERFORM 8600-DEEDIT
00715          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY
00716          MOVE '4'                      TO  DC-OPTION-CODE
00717          PERFORM 8500-DATE-CONVERSION
00718          IF DATE-CONVERSION-ERROR
00719              MOVE ER-0348              TO  EMI-ERROR
00720              MOVE -1                   TO  EFFDT08L
00721              MOVE AL-UABON             TO  EFFDT08A
00722              PERFORM 9900-ERROR-FORMAT
00723            ELSE
00724              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT08I
00725              MOVE AL-UANON             TO  EFFDT08A
00726              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (8).
00727
00728      IF EFFDT09L GREATER ZERO
00729          MOVE EFFDT09I                 TO  DEEDIT-FIELD
00730          PERFORM 8600-DEEDIT
00731          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY
00732          MOVE '4'                      TO  DC-OPTION-CODE
00733          PERFORM 8500-DATE-CONVERSION
00734          IF DATE-CONVERSION-ERROR
00735              MOVE ER-0348              TO  EMI-ERROR
00736              MOVE -1                   TO  EFFDT09L
00737              MOVE AL-UABON             TO  EFFDT09A
00738              PERFORM 9900-ERROR-FORMAT
00739            ELSE
00740              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT09I
00741              MOVE AL-UANON             TO  EFFDT09A
00742              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (9).
00743
00744      IF EFFDT10L GREATER ZERO
00745          MOVE EFFDT10I                 TO  DEEDIT-FIELD
00746          PERFORM 8600-DEEDIT
00747          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY
00748          MOVE '4'                      TO  DC-OPTION-CODE
00749          PERFORM 8500-DATE-CONVERSION
00750          IF DATE-CONVERSION-ERROR
00751              MOVE ER-0348              TO  EMI-ERROR
00752              MOVE -1                   TO  EFFDT10L
00753              MOVE AL-UABON             TO  EFFDT10A
00754              PERFORM 9900-ERROR-FORMAT
00755            ELSE
00756              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT10I
00757              MOVE AL-UANON             TO  EFFDT10A
00758              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (10).
00759
00760      IF EFFDT11L GREATER ZERO
00761          MOVE EFFDT11I                 TO  DEEDIT-FIELD
00762          PERFORM 8600-DEEDIT
00763          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY
00764          MOVE '4'                      TO  DC-OPTION-CODE
00765          PERFORM 8500-DATE-CONVERSION
00766          IF DATE-CONVERSION-ERROR
00767              MOVE ER-0348              TO  EMI-ERROR
00768              MOVE -1                   TO  EFFDT11L
00769              MOVE AL-UABON             TO  EFFDT11A
00770              PERFORM 9900-ERROR-FORMAT
00771            ELSE
00772              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT11I
00773              MOVE AL-UANON             TO  EFFDT11A
00774              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (11).
00775
00776      IF EFFDT12L GREATER ZERO
00777          MOVE EFFDT12I                 TO  DEEDIT-FIELD
00778          PERFORM 8600-DEEDIT
00779          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY
00780          MOVE '4'                      TO  DC-OPTION-CODE
00781          PERFORM 8500-DATE-CONVERSION
00782          IF DATE-CONVERSION-ERROR
00783              MOVE ER-0348              TO  EMI-ERROR
00784              MOVE -1                   TO  EFFDT12L
00785              MOVE AL-UABON             TO  EFFDT12A
00786              PERFORM 9900-ERROR-FORMAT
00787            ELSE
00788              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT12I
00789              MOVE AL-UANON             TO  EFFDT12A
00790              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (12).
00791
00792      IF EFFDT13L GREATER ZERO
00793          MOVE EFFDT13I                 TO  DEEDIT-FIELD
00794          PERFORM 8600-DEEDIT
00795          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY
00796          MOVE '4'                      TO  DC-OPTION-CODE
00797          PERFORM 8500-DATE-CONVERSION
00798          IF DATE-CONVERSION-ERROR
00799              MOVE ER-0348              TO  EMI-ERROR
00800              MOVE -1                   TO  EFFDT13L
00801              MOVE AL-UABON             TO  EFFDT13A
00802              PERFORM 9900-ERROR-FORMAT
00803            ELSE
00804              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT13I
00805              MOVE AL-UANON             TO  EFFDT13A
00806              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (13).
00807
00808      IF EFFDT14L GREATER ZERO
00809          MOVE EFFDT14I                 TO  DEEDIT-FIELD
00810          PERFORM 8600-DEEDIT
00811          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY
00812          MOVE '4'                      TO  DC-OPTION-CODE
00813          PERFORM 8500-DATE-CONVERSION
00814          IF DATE-CONVERSION-ERROR
00815              MOVE ER-0348              TO  EMI-ERROR
00816              MOVE -1                   TO  EFFDT14L
00817              MOVE AL-UABON             TO  EFFDT14A
00818              PERFORM 9900-ERROR-FORMAT
00819            ELSE
00820              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT14I
00821              MOVE AL-UANON             TO  EFFDT14A
00822              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (14).
00823
00824      IF EFFDT15L GREATER ZERO
00825          MOVE EFFDT15I                 TO  DEEDIT-FIELD
00826          PERFORM 8600-DEEDIT
00827          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY
00828          MOVE '4'                      TO  DC-OPTION-CODE
00829          PERFORM 8500-DATE-CONVERSION
00830          IF DATE-CONVERSION-ERROR
00831              MOVE ER-0348              TO  EMI-ERROR
00832              MOVE -1                   TO  EFFDT15L
00833              MOVE AL-UABON             TO  EFFDT15A
00834              PERFORM 9900-ERROR-FORMAT
00835            ELSE
00836              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT15I
00837              MOVE AL-UANON             TO  EFFDT15A
00838              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (15).
00839
00840      IF EMI-ERROR NOT = ZERO
00841          MOVE +0                     TO PI-PROC-SW
00842          PERFORM 8200-SEND-DATAONLY
00843       ELSE
00844          GO TO 0200-PROCESS-CHECK-RELEASE.
00845
00846  0140-NOT-FOUND.
00847
00848      MOVE ER-0331                TO  EMI-ERROR.
00849      PERFORM 8200-SEND-DATAONLY.
00850
00851  0200-PROCESS-CHECK-RELEASE.
00852
00853 *    NOTE *******************************************************
00854 *         *                                                     *
00855 *         *      OBTAIN EXCLUSIVE CONTROL OF THE CHECK QUEUE    *
00856 *         *  DATASET DURING THE GENERATION OF THE CHECK QUEUE   *
00857 *         *  RECORDS.                                           *
00858 *         *                                                     *
00859 *         *******************************************************.
00860
00861 ******  ACTUAL CHECK RELEASE WILL NOT TAKE PLACE
00862 ******         UNLESS PF1 WAS PRESSED
00863
00864      IF EIBAID = DFHPF1
00865          
      * EXEC CICS ENQ
00866 *            RESOURCE (WS-ERCHKQ-DSID)
00867 *            LENGTH   (11)
00868 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '2$L                   $   #00004155' TO DFHEIV0
           MOVE X'32244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCHKQ-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00869          
      * EXEC CICS ENQ
00870 *            RESOURCE (WS-ERCMKQ-DSID)
00871 *            LENGTH   (11)
00872 *        END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '2$L                   $   #00004159' TO DFHEIV0
           MOVE X'32244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCMKQ-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00873
00874      EJECT
00875 *    NOTE *******************************************************
00876 *         *                                                     *
00877 *         *      GET THE CONTROL GROUP NUMBER FROM THE COMPANY  *
00878 *         *  CONTROL RECORD OF THE CONTROL FILE.                *
00879 *         *                                                     *
00880 *         *******************************************************.
00881
00882      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.
00883
00884      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
00885      MOVE '1'                    TO  WS-CFK-RECORD-TYPE.
00886      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
00887
00888      IF EIBAID = DFHPF1
00889          
      * EXEC CICS READ UPDATE
00890 *            DATASET (WS-ELCNTL-DSID)
00891 *            RIDFLD  (WS-CONTROL-FILE-KEY)
00892 *            SET     (ADDRESS OF CONTROL-FILE)
00893 *        END-EXEC
      *    MOVE '&"S        EU         (   #00004179' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELCNTL-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00894       ELSE
00895          
      * EXEC CICS READ
00896 *            DATASET (WS-ELCNTL-DSID)
00897 *            RIDFLD  (WS-CONTROL-FILE-KEY)
00898 *            SET     (ADDRESS OF CONTROL-FILE)
00899 *        END-EXEC.
      *    MOVE '&"S        E          (   #00004185' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELCNTL-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00900
00901      ADD +1                      TO  CF-CR-CHECK-QUE-COUNTER.
00902
00903      IF CR-QUE-COUNT-RESET
00904          MOVE +1                 TO  CF-CR-CHECK-QUE-COUNTER.
00905
00906      MOVE CF-CR-CHECK-QUE-COUNTER  TO  WS-CHECK-QUE-COUNTER
00907                                        PI-CK-CONTROL-NO.
00908
00909      MOVE WS-ELCNTL-DSID         TO  JP-FILE-ID.
00910      MOVE 'C'                    TO  JP-RECORD-TYPE.
00911      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
00912
00913      IF EIBAID = DFHPF1
00914          
      * EXEC CICS REWRITE
00915 *            DATASET (WS-ELCNTL-DSID)
00916 *            FROM    (CONTROL-FILE)
00917 *        END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004204' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELCNTL-DSID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00918          PERFORM 8400-LOG-JOURNAL-RECORD.
00919
00920      EJECT
00921 *    NOTE *******************************************************
00922 *         *                                                     *
00923 *         *      GET STORAGE FOR WRITING OF THE CHECK QUEUE     *
00924 *         *  RECORDS.                                           *
00925 *         *                                                     *
00926 *         *******************************************************.
00927
00928      IF  AOPTIONI     = '5' OR '6'
00929          
      * EXEC CICS GETMAIN
00930 *            SET (ADDRESS OF COMMISSION-CHECK-QUE)
00931 *            LENGTH (WS-ERCMKQ-LENGTH)
00932 *            INITIMG (WS-SPACES) END-EXEC
      *    MOVE ',"IL                  $   #00004219' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 WS-ERCMKQ-LENGTH, 
                 WS-SPACES
           SET ADDRESS OF COMMISSION-CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00933          COMPUTE WS-SEQ-NO = WS-TIME-WORK  *  10
00934          MOVE AOPTIONI  TO  PI-PGM-PRINT-OPT
00935          GO TO 0280-OPTION-5
00936      ELSE
00937          
      * EXEC CICS GETMAIN
00938 *            SET (ADDRESS OF CHECK-QUE)
00939 *            LENGTH (WS-ERCHKQ-LENGTH)
00940 *            INITIMG (WS-SPACES) END-EXEC.
      *    MOVE ',"IL                  $   #00004227' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 WS-ERCHKQ-LENGTH, 
                 WS-SPACES
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00941
00942      IF AOPTIONI EQUAL '1' OR '2'
00943          NEXT SENTENCE
00944      ELSE
00945          GO TO 0260-END-OF-ERPYAJ.
00946
00947 *    NOTE *******************************************************
00948 *         *                                                     *
00949 *         *      BROWSE THE PENDING PAYMENTS AND ADJUSTMENTS    *
00950 *         *  FILE FOR PAYMENTS PENDING.                         *
00951 *         *                                                     *
00952 *         *      WHEN A PAYMENT IS PENDING, GENERATE A CHECK    *
00953 *         *  QUEUE RECORD FOR RELEASE.                          *
00954 *         *                                                     *
00955 *         *******************************************************.
00956
00957      MOVE LOW-VALUES             TO  WS-PENDING-PAYMENTS-KEY.
00958      MOVE PI-COMPANY-CD          TO  WS-PPK-COMPANY-CD.
00959
00960  0225-STARTBR-ERPYAJ.
00961
00962      IF WS-ERPYAJ-BROWSE-NOT-STARTED
00963          
      * EXEC CICS STARTBR
00964 *            DATASET (WS-ERPYAJ-DSID)
00965 *            RIDFLD  (WS-PENDING-PAYMENTS-KEY)
00966 *            GTEQ
00967 *            END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004253' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERPYAJ-DSID, 
                 WS-PENDING-PAYMENTS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00968
00969      MOVE +1 TO WS-ERPYAJ-BROWSE-SW.
00970
00971      
      * EXEC CICS HANDLE CONDITION
00972 *        ENDFILE    (0260-END-OF-ERPYAJ)
00973 *        END-EXEC.
      *    MOVE '"$''                   ! # #00004261' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034323631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00974
00975  0250-READNEXT-ERPYAJ.
00976
00977      
      * EXEC CICS READNEXT
00978 *        DATASET (WS-ERPYAJ-DSID)
00979 *        RIDFLD  (WS-PENDING-PAYMENTS-KEY)
00980 *        SET     (ADDRESS OF PENDING-PAY-ADJ) END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004267' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERPYAJ-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-PENDING-PAYMENTS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00981
00982      IF WS-PPK-COMPANY-CD EQUAL PI-COMPANY-CD
00983          NEXT SENTENCE
00984      ELSE
00985          IF AOPTIONI EQUAL '1'
00986              GO TO 0260-END-OF-ERPYAJ
00987          ELSE
00988              GO TO 0400-END-OF-SEARCH.
00989
00990      IF ACARRL GREATER ZERO
00991          IF PY-CARRIER NOT = ACARRI
00992             GO TO 0250-READNEXT-ERPYAJ.
00993
00994      IF AGROUPL GREATER ZERO
00995         IF PY-GROUPING NOT = AGROUPI
00996             GO TO 0250-READNEXT-ERPYAJ.
00997
00998      IF ABYL GREATER ZERO
00999         IF PY-LAST-MAINT-BY NOT = ABYI
01000             GO TO 0250-READNEXT-ERPYAJ.
01001
01002      IF AAMTL GREATER ZERO
01003          IF PY-ENTRY-AMT LESS PI-CHECK-AMOUNT
01004              NEXT SENTENCE
01005            ELSE
01006              GO TO 0250-READNEXT-ERPYAJ.
01007
01008      IF  PY-CHARGE-TO-AGENT
01009           IF PY-BILLING-CHECK OR
01010              PY-GA-CHECK
01011              NEXT SENTENCE
01012           ELSE
01013              GO TO 0250-READNEXT-ERPYAJ
01014      ELSE
01015           GO TO 0250-READNEXT-ERPYAJ.
01016
01017      IF PY-CHECK-QUE-CONTROL  EQUAL ZEROS AND
01018         PY-CHECK-QUE-SEQUENCE EQUAL ZEROS
01019          NEXT SENTENCE
01020      ELSE
01021          GO TO 0250-READNEXT-ERPYAJ.
01022
01023 *    NOTE *******************************************************
01024 *         *                                                     *
01025 *         *      THE PAYMENT TRAILER HAS MET ALL OF THE         *
01026 *         *  QUALIFICATIONS FOR THIS CHECK RELEASE, NOW         *
01027 *         *  GENERATE A CHECK QUEUE RECORD.                     *
01028 *         *                                                     *
01029 *         *******************************************************.
01030
01031      ADD +1                      TO  WS-RELEASED-COUNT.
01032      ADD PY-ENTRY-AMT            TO  WS-RELEASED-AMOUNT.
01033
01034      IF EIBAID NOT = DFHPF1
01035          GO TO 0250-READNEXT-ERPYAJ.
01036
01037      IF WS-ERPYAJ-BROWSE-STARTED
01038          
      * EXEC CICS ENDBR
01039 *            DATASET (WS-ERPYAJ-DSID)
01040 *            END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004328' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERPYAJ-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01041          MOVE +0 TO WS-ERPYAJ-BROWSE-SW.
01042
01043      
      * EXEC CICS READ UPDATE
01044 *        DATASET (WS-ERPYAJ-DSID)
01045 *        RIDFLD  (WS-PENDING-PAYMENTS-KEY)
01046 *        SET     (ADDRESS OF PENDING-PAY-ADJ)
01047 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004333' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERPYAJ-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-PENDING-PAYMENTS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01048
01049      MOVE SPACES                 TO  CHECK-QUE.
01050      MOVE 'CQ'                   TO  CQ-RECORD-ID.
01051      MOVE PY-COMPANY-CD          TO  CQ-COMPANY-CD.
01052      MOVE WS-CHECK-QUE-COUNTER   TO  CQ-CONTROL-NUMBER
01053                                      PY-CHECK-QUE-CONTROL
01054      MOVE WS-CHECK-COUNTER       TO  CQ-SEQUENCE-NUMBER
01055                                      PY-CHECK-QUE-SEQUENCE.
01056      ADD +1                      TO  WS-CHECK-COUNTER.
01057      MOVE 'Q'                    TO  CQ-ENTRY-TYPE.
01058      MOVE PY-CARRIER             TO  CQ-PYAJ-CARRIER.
01059      MOVE PY-GROUPING            TO  CQ-PYAJ-GROUPING.
01060      MOVE PY-FIN-RESP            TO  CQ-PYAJ-FIN-RESP.
01061      MOVE PY-ACCOUNT             TO  CQ-PYAJ-ACCOUNT.
01062      MOVE PY-FILE-SEQ-NO         TO  CQ-PYAJ-SEQ.
01063      MOVE PY-RECORD-TYPE         TO  CQ-PYAJ-REC-TYPE.
01064      MOVE PY-ENTRY-AMT           TO  CQ-CHECK-AMOUNT.
01065      MOVE SPACES                 TO  WS-CHECK-NUMBER.
01066      MOVE PY-CHECK-NUMBER        TO  WS-CHECK-NO.
01067      MOVE WS-CHECK-NUMBER        TO  CQ-CHECK-NUMBER.
01068      MOVE '1'                    TO  CQ-PAYMENT-TYPE.
01069      MOVE ZERO                   TO  CQ-TIMES-PRINTED
01070                                      CQ-PRINT-AT-HHMM.
01071      MOVE PI-CURRENT-DATE-BIN    TO  CQ-CHECK-WRITTEN-DT.
01072      MOVE PY-REPORTED-DT         TO  CQ-CHECK-BY-USER.
01073      MOVE +6860                  TO  CQ-LAST-UPDATED-BY.
01074      MOVE CHECK-QUE              TO  JP-RECORD-AREA.
01075      MOVE 'A'                    TO  JP-RECORD-TYPE.
01076      MOVE WS-ERCHKQ-DSID         TO  JP-FILE-ID.
01077
01078      
      * EXEC CICS WRITE
01079 *        DATASET (WS-ERCHKQ-DSID)
01080 *        FROM    (CHECK-QUE)
01081 *        RIDFLD  (CQ-CONTROL-PRIMARY) END-EXEC.
           MOVE LENGTH OF
            CHECK-QUE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004368' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCHKQ-DSID, 
                 CHECK-QUE, 
                 DFHEIV11, 
                 CQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01082
01083      PERFORM 8400-LOG-JOURNAL-RECORD.
01084
01085      MOVE PI-PROCESSOR-ID        TO  PY-LAST-MAINT-BY.
01086
01087      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.
01088      MOVE 'C'                    TO  JP-RECORD-TYPE.
01089      MOVE WS-ERPYAJ-DSID            TO  JP-FILE-ID.
01090
01091      
      * EXEC CICS REWRITE
01092 *        DATASET (WS-ERPYAJ-DSID)
01093 *        FROM    (PENDING-PAY-ADJ) END-EXEC.
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004381' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERPYAJ-DSID, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01094
01095      PERFORM 8400-LOG-JOURNAL-RECORD.
01096
01097      GO TO 0225-STARTBR-ERPYAJ.
01098
01099      EJECT
01100
01101  0260-END-OF-ERPYAJ.
01102
01103      IF AOPTIONI EQUAL '1' OR '3' OR '4'
01104          NEXT SENTENCE
01105      ELSE
01106          GO TO 0400-END-OF-SEARCH.
01107
01108      IF WS-ERPYAJ-BROWSE-STARTED
01109          
      * EXEC CICS ENDBR
01110 *            DATASET (WS-ERPYAJ-DSID)
01111 *            END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004399' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERPYAJ-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01112          MOVE +0 TO WS-ERPYAJ-BROWSE-SW.
01113
01114 *    NOTE *******************************************************
01115 *         *                                                     *
01116 *         *      BROWSE THE CHECK-MAINTENANCE FILE FOR ANY      *
01117 *         *  PENDING PAYMENTS FOR RELEASE                       *
01118 *         *                                                     *
01119 *         *      WHEN A PAYMENT IS PENDING, GENERATE A CHECK    *
01120 *         *  QUEUE RECORD FOR RELEASE.                          *
01121 *         *                                                     *
01122 *         *******************************************************.
01123
01124      MOVE LOW-VALUES             TO  WS-CHECK-MAINT-KEY.
01125      MOVE PI-COMPANY-CD          TO  WS-CHK-COMPANY-CD.
01126
01127  0265-STARTBR-ERCHEK.
01128
01129      IF WS-ERCHEK-BROWSE-NOT-STARTED
01130          
      * EXEC CICS STARTBR
01131 *            DATASET (WS-ERCHEK-DSID)
01132 *            RIDFLD  (WS-CHECK-MAINT-KEY)
01133 *            GTEQ
01134 *            END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004420' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCHEK-DSID, 
                 WS-CHECK-MAINT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01135
01136      MOVE +1 TO WS-ERCHEK-BROWSE-SW.
01137
01138      
      * EXEC CICS HANDLE CONDITION
01139 *        ENDFILE    (0400-END-OF-SEARCH)
01140 *        END-EXEC.
      *    MOVE '"$''                   ! $ #00004428' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303034343238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01141
01142  0270-READNEXT-ERCHEK.
01143
01144      
      * EXEC CICS READNEXT
01145 *        DATASET (WS-ERCHEK-DSID)
01146 *        RIDFLD  (WS-CHECK-MAINT-KEY)
01147 *        SET     (ADDRESS OF CHECK-RECORDS) END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004434' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCHEK-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CHECK-MAINT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01148
01149      IF WS-CHK-COMPANY-CD NOT = PI-COMPANY-CD
01150          GO TO 0400-END-OF-SEARCH.
01151
01152      IF ACARRL GREATER ZERO
01153          IF CH-CARRIER NOT = ACARRI
01154              GO TO 0270-READNEXT-ERCHEK.
01155
01156      IF AGROUPL GREATER ZERO
01157         IF CH-GROUPING NOT = AGROUPI
01158             GO TO 0270-READNEXT-ERCHEK.
01159
01160      IF ASTATL GREATER ZERO
01161         IF CH-STATE NOT = ASTATI
01162             GO TO 0270-READNEXT-ERCHEK.
01163
01164      IF AACCTL GREATER ZERO
01165         IF CH-ACCOUNT NOT = AACCTI
01166             GO TO 0270-READNEXT-ERCHEK.
01167
01168      IF ABYL  GREATER ZERO
01169         IF CH-RECORDED-BY NOT = ABYI
01170             GO TO 0270-READNEXT-ERCHEK.
01171
01172      IF AAMTL GREATER ZERO
01173         IF CH-AMOUNT-PAID LESS PI-CHECK-AMOUNT
01174              NEXT SENTENCE
01175            ELSE
01176              GO TO 0270-READNEXT-ERCHEK.
01177
01178      MOVE ZERO               TO PI-CERT-SW.
01179
01180      IF ZERO = CERTO01L AND CERTO02L AND CERTO03L AND
01181                CERTO04L AND CERTO05L AND CERTO06L AND
01182                CERTO07L AND CERTO08L AND CERTO09L AND
01183                CERTO10L AND CERTO11L AND CERTO12L AND
01184                CERTO13L AND CERTO14L AND CERTO15L
01185           MOVE +1           TO PI-CERT-SW
01186        ELSE
01187      IF CERTO01I       = CH-CERT-NO     AND
01188         BIN-EFFDT (1)  = CH-CERT-EFF-DT
01189           MOVE +1           TO PI-CERT-SW
01190       ELSE
01191      IF CERTO02I       = CH-CERT-NO     AND
01192         BIN-EFFDT (2)  = CH-CERT-EFF-DT
01193           MOVE +1           TO PI-CERT-SW
01194       ELSE
01195      IF CERTO03I       = CH-CERT-NO     AND
01196         BIN-EFFDT (3)  = CH-CERT-EFF-DT
01197           MOVE +1           TO PI-CERT-SW
01198       ELSE
01199      IF CERTO04I       = CH-CERT-NO     AND
01200         BIN-EFFDT (4)  = CH-CERT-EFF-DT
01201           MOVE +1           TO PI-CERT-SW
01202       ELSE
01203      IF CERTO05I       = CH-CERT-NO     AND
01204         BIN-EFFDT (5)  = CH-CERT-EFF-DT
01205           MOVE +1           TO PI-CERT-SW
01206       ELSE
01207      IF CERTO06I       = CH-CERT-NO     AND
01208         BIN-EFFDT (6)  = CH-CERT-EFF-DT
01209           MOVE +1           TO PI-CERT-SW
01210       ELSE
01211      IF CERTO07I       = CH-CERT-NO     AND
01212         BIN-EFFDT (7)  = CH-CERT-EFF-DT
01213           MOVE +1           TO PI-CERT-SW
01214       ELSE
01215      IF CERTO08I       = CH-CERT-NO     AND
01216         BIN-EFFDT (8)  = CH-CERT-EFF-DT
01217           MOVE +1           TO PI-CERT-SW
01218       ELSE
01219      IF CERTO09I       = CH-CERT-NO     AND
01220         BIN-EFFDT (9)  = CH-CERT-EFF-DT
01221           MOVE +1           TO PI-CERT-SW
01222       ELSE
01223      IF CERTO10I       = CH-CERT-NO     AND
01224         BIN-EFFDT (10) = CH-CERT-EFF-DT
01225           MOVE +1           TO PI-CERT-SW
01226       ELSE
01227      IF CERTO11I       = CH-CERT-NO     AND
01228         BIN-EFFDT (11) = CH-CERT-EFF-DT
01229           MOVE +1           TO PI-CERT-SW
01230       ELSE
01231      IF CERTO12I       = CH-CERT-NO     AND
01232         BIN-EFFDT (12) = CH-CERT-EFF-DT
01233           MOVE +1           TO PI-CERT-SW
01234       ELSE
01235      IF CERTO13I       = CH-CERT-NO     AND
01236         BIN-EFFDT (13) = CH-CERT-EFF-DT
01237           MOVE +1           TO PI-CERT-SW
01238       ELSE
01239      IF CERTO14I       = CH-CERT-NO     AND
01240         BIN-EFFDT (14) = CH-CERT-EFF-DT
01241           MOVE +1           TO PI-CERT-SW
01242       ELSE
01243      IF CERTO15I       = CH-CERT-NO     AND
01244         BIN-EFFDT (15) = CH-CERT-EFF-DT
01245           MOVE +1           TO PI-CERT-SW.
01246
01247      IF PI-CERT-NOT-MATCHED
01248          GO TO 0270-READNEXT-ERCHEK.
01249
01250      IF CH-MAINT-CHECK
01251          IF AOPTIONI EQUAL '1' OR '4'
01252              NEXT SENTENCE
01253          ELSE
01254              GO TO 0270-READNEXT-ERCHEK.
01255
01256      IF CH-REFUND-CHECK
01257          IF AOPTIONI EQUAL '1' OR '3'
01258              NEXT SENTENCE
01259          ELSE
01260              GO TO 0270-READNEXT-ERCHEK.
01261
01262      IF CH-CHECK-QUE-CONTROL  EQUAL ZEROS AND
01263         CH-CHECK-QUE-SEQUENCE EQUAL ZEROS
01264          NEXT SENTENCE
01265      ELSE
01266          GO TO 0270-READNEXT-ERCHEK.
01267
01268 *    NOTE *******************************************************
01269 *         *                                                     *
01270 *         *      THE CHECK MAINT TRAILER HAS MET ALL OF THE     *
01271 *         *  QUALIFICATIONS FOR THIS CHECK RELEASE, NOW         *
01272 *         *  GENERATE A CHECK QUEUE RECORD.                     *
01273 *         *                                                     *
01274 *         *******************************************************.
01275
01276      IF CH-VOID-DT = LOW-VALUES  AND
01277        NOT MANUAL-CHECK-WRITTEN
01278          ADD +1                  TO  WS-RELEASED-COUNT
01279          ADD CH-AMOUNT-PAID      TO  WS-RELEASED-AMOUNT.
01280
01281      IF EIBAID NOT = DFHPF1
01282          GO TO 0270-READNEXT-ERCHEK.
01283
01284      IF WS-ERCHEK-BROWSE-STARTED
01285          
      * EXEC CICS ENDBR
01286 *            DATASET (WS-ERCHEK-DSID)
01287 *            END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004575' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCHEK-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01288          MOVE +0 TO WS-ERCHEK-BROWSE-SW.
01289
01290      
      * EXEC CICS READ UPDATE
01291 *        DATASET (WS-ERCHEK-DSID)
01292 *        RIDFLD  (WS-CHECK-MAINT-KEY)
01293 *        SET     (ADDRESS OF CHECK-RECORDS) END-EXEC.
      *    MOVE '&"S        EU         (   #00004580' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCHEK-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CHECK-MAINT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01294
01295      MOVE SPACES                 TO  CHECK-QUE.
01296      MOVE 'CQ'                   TO  CQ-RECORD-ID.
01297      MOVE CH-COMPANY-CD          TO  CQ-COMPANY-CD.
01298      MOVE WS-CHECK-QUE-COUNTER   TO  CQ-CONTROL-NUMBER
01299                                      CH-CHECK-QUE-CONTROL.
01300      MOVE WS-CHECK-COUNTER       TO  CQ-SEQUENCE-NUMBER
01301                                      CH-CHECK-QUE-SEQUENCE.
01302      ADD +1                      TO  WS-CHECK-COUNTER.
01303      MOVE 'Q'                    TO  CQ-ENTRY-TYPE.
01304      MOVE ZERO                   TO  CQ-TIMES-PRINTED
01305                                      CQ-PRINT-AT-HHMM.
01306      MOVE PI-CURRENT-DATE-BIN    TO  CQ-CHECK-WRITTEN-DT.
01307      IF MANUAL-CHECK-WRITTEN
01308          MOVE 'M'                TO  CQ-ENTRY-TYPE
01309          MOVE +1                 TO  CQ-TIMES-PRINTED
01310          MOVE CH-CHECK-NO        TO  CQ-CHECK-NUMBER
01311          MOVE CH-CHECK-WRITTEN-DT TO CQ-CHECK-WRITTEN-DT.
01312      MOVE LOW-VALUES             TO  CQ-CHECK-VOIDED-DT.
01313      IF CH-VOID-DT NOT = LOW-VALUES
01314          MOVE 'V'                TO  CQ-ENTRY-TYPE
01315                                      CQ-VOID-INDICATOR
01316          MOVE CH-VOID-DT         TO  CQ-CHECK-VOIDED-DT.
01317      MOVE CH-CARRIER             TO  CQ-CHEK-CARRIER.
01318      MOVE CH-GROUPING            TO  CQ-CHEK-GROUPING.
01319      MOVE CH-STATE               TO  CQ-CHEK-STATE.
01320      MOVE CH-ACCOUNT             TO  CQ-CHEK-ACCOUNT.
01321      MOVE CH-SEQUENCE-NO         TO  CQ-CHEK-SEQ-NO.
01322      MOVE CH-CERT-EFF-DT         TO  CQ-CHEK-CERT-EFF-DT.
01323      MOVE CH-CERT-NO             TO  CQ-CHEK-CERT-NO.
01324      MOVE CH-COMP-FIN-RESP       TO  CQ-CHEK-FIN-RESP.
01325      MOVE CH-CHECK-NO            TO  CQ-CHECK-NUMBER.
01326
01327      IF CH-REFUND-CHECK
01328          MOVE '2'                TO  CQ-PAYMENT-TYPE
01329      ELSE
01330          MOVE '3'                TO  CQ-PAYMENT-TYPE.
01331
01332      MOVE CH-AMOUNT-PAID         TO  CQ-CHECK-AMOUNT.
01333      MOVE CH-RECORDED-BY         TO  CQ-CHECK-BY-USER.
01334      MOVE +6860                  TO  CQ-LAST-UPDATED-BY.
01335      MOVE CHECK-QUE              TO  JP-RECORD-AREA.
01336      MOVE 'A'                    TO  JP-RECORD-TYPE.
01337      MOVE WS-ERCHKQ-DSID         TO  JP-FILE-ID.
01338
01339      
      * EXEC CICS WRITE
01340 *        DATASET (WS-ERCHKQ-DSID)
01341 *        FROM    (CHECK-QUE)
01342 *        RIDFLD  (CQ-CONTROL-PRIMARY) END-EXEC.
           MOVE LENGTH OF
            CHECK-QUE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004629' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCHKQ-DSID, 
                 CHECK-QUE, 
                 DFHEIV11, 
                 CQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01343
01344      PERFORM 8400-LOG-JOURNAL-RECORD.
01345
01346      MOVE CHECK-RECORDS          TO  JP-RECORD-AREA.
01347      MOVE 'C'                    TO  JP-RECORD-TYPE.
01348      MOVE WS-ERCHEK-DSID         TO  JP-FILE-ID.
01349
01350      
      * EXEC CICS REWRITE
01351 *        DATASET (WS-ERCHEK-DSID)
01352 *        FROM    (CHECK-RECORDS) END-EXEC.
           MOVE LENGTH OF
            CHECK-RECORDS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004640' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCHEK-DSID, 
                 CHECK-RECORDS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01353
01354      PERFORM 8400-LOG-JOURNAL-RECORD.
01355
01356      GO TO 0265-STARTBR-ERCHEK.
01357
01358      EJECT
01359
01360  0280-OPTION-5.
01361
01362      IF PI-PROCESSOR-ID  =  'LGXX'
01363          GO TO 0281-CONTINUE.
01364
01365      
      * EXEC CICS READQ TS
01366 *        QUEUE  (QID)
01367 *        INTO   (SECURITY-CONTROL)
01368 *        LENGTH (SC-COMM-LENGTH)
01369 *        ITEM   (SC-ITEM)
01370 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00004655' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01371
01372      MOVE SC-CREDIT-DISPLAY (7)  TO PI-DISPLAY-CAP.
01373      MOVE SC-CREDIT-UPDATE  (7)  TO PI-MODIFY-CAP.
01374
01375      IF NOT MODIFY-CAP
01376          MOVE 'READ'             TO  SM-READ
01377          PERFORM 9995-SECURITY-VIOLATION
01378          MOVE ER-0070            TO EMI-ERROR
01379          PERFORM 9900-ERROR-FORMAT
01380          GO TO 8100-SEND-INITIAL-MAP.
01381
01382  0281-CONTINUE.
01383      MOVE LOW-VALUES             TO  WS-COMCK-MAINT-KEY.
01384      MOVE PI-COMPANY-CD          TO  WS-CMK-COMPANY-CD.
01385
01386  0285-STARTBR-ERCMCK.
01387
01388      
      * EXEC CICS STARTBR
01389 *        DATASET (WS-ERCMCK-DSID)
01390 *        RIDFLD  (WS-COMCK-MAINT-KEY)
01391 *        GTEQ
01392 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004678' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCMCK-DSID, 
                 WS-COMCK-MAINT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01393
01394      MOVE +1  TO  WS-ERCMCK-BROWSE-SW.
01395
01396      
      * EXEC CICS HANDLE CONDITION
01397 *        ENDFILE    (0400-END-OF-SEARCH)
01398 *        END-EXEC.
      *    MOVE '"$''                   ! % #00004686' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034363836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01399
01400  0290-READNEXT-ERCMCK.
01401
01402      
      * EXEC CICS READNEXT
01403 *        DATASET (WS-ERCMCK-DSID)
01404 *        RIDFLD  (WS-COMCK-MAINT-KEY)
01405 *        SET     (ADDRESS OF COMM-CHECK-RECORDS) END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004692' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCMCK-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-COMCK-MAINT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMM-CHECK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01406
01407      IF WS-CMK-COMPANY-CD NOT = PI-COMPANY-CD
01408          GO TO 0400-END-OF-SEARCH.
01409
01410      IF ACARRL GREATER ZERO
01411          IF CK-CARRIER NOT = ACARRI
01412              GO TO 0290-READNEXT-ERCMCK.
01413
01414      IF AGROUPL GREATER ZERO
01415          IF CK-GROUPING NOT = AGROUPI
01416              GO TO 0290-READNEXT-ERCMCK.
01417
01418      IF ABYL GREATER ZERO
01419          IF CK-RECORDED-BY NOT = ABYI
01420              GO TO 0290-READNEXT-ERCMCK.
01421
01422      IF AAMTL GREATER ZERO
01423          IF CK-AMOUNT-PAID LESS PI-CHECK-AMOUNT
01424              NEXT SENTENCE
01425            ELSE
01426              GO TO 0290-READNEXT-ERCMCK.
01427
01428      IF CK-VOID-DT NOT = LOW-VALUES
01429          GO TO 0290-READNEXT-ERCMCK.
01430
01431      IF CK-QUE-CONTROL-NUMBER  EQUAL ZEROS AND
01432         CK-QUE-SEQ-NO          EQUAL ZEROS
01433             NEXT SENTENCE
01434      ELSE
01435          GO TO 0290-READNEXT-ERCMCK.
01436
01437 *    NOTE *******************************************************
01438 *         *                                                     *
01439 *         *      THE CHECK MAINT TRAILER HAS MET ALL OF THE     *
01440 *         *  QUALIFICATIONS FOR THIS CHECK RELEASE, NOW         *
01441 *         *  GENERATE A CHECK QUEUE RECORD.                     *
01442 *         *                                                     *
01443 *         *******************************************************.
01444
01445      IF AOPTIONI  IS EQUAL TO '6'
01446         IF CK-ACH-PAYMENT IS EQUAL TO 'P'
01447            CONTINUE
01448         ELSE
01449            GO TO 0290-READNEXT-ERCMCK.
01450
01451      IF AOPTIONI  IS EQUAL TO '5'
01452         IF CK-ACH-PAYMENT IS EQUAL TO 'P'
01453            GO TO 0290-READNEXT-ERCMCK
01454         ELSE
01455            CONTINUE.
01456
01457      IF NOT CK-TEXT
01458          ADD +1                  TO  WS-RELEASED-COUNT.
01459      ADD CK-AMOUNT-PAID          TO  WS-RELEASED-AMOUNT.
01460
01461      IF EIBAID NOT = DFHPF1
01462          GO TO 0290-READNEXT-ERCMCK.
01463
01464      
      * EXEC CICS ENDBR
01465 *        DATASET (WS-ERCMCK-DSID)
01466 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004754' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCMCK-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01467
01468      MOVE +0  TO  WS-ERCMCK-BROWSE-SW.
01469
01470      
      * EXEC CICS READ UPDATE
01471 *        DATASET (WS-ERCMCK-DSID)
01472 *        RIDFLD  (WS-COMCK-MAINT-KEY)
01473 *        SET     (ADDRESS OF COMM-CHECK-RECORDS) END-EXEC.
      *    MOVE '&"S        EU         (   #00004760' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCMCK-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-COMCK-MAINT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMM-CHECK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01474
01475      MOVE SPACES                 TO  COMMISSION-CHECK-QUE.
01476      MOVE 'MQ'                   TO  MQ-RECORD-ID.
01477      MOVE CK-COMPANY-CD          TO  MQ-COMPANY-CD
01478                                      MQ-COMPANY-CD-A1.
01479      MOVE WS-CHECK-QUE-COUNTER   TO  MQ-CONTROL-NUMBER
01480                                      CK-QUE-CONTROL-NUMBER
01481                                      MQ-CONTROL-NUMBER-A1.
01482      MOVE WS-CHECK-COUNTER       TO  MQ-SEQUENCE-NUMBER
01483                                      CK-QUE-SEQ-NO
01484                                      MQ-SEQUENCE-NUMBER-A1.
01485      ADD +1                      TO  WS-CHECK-COUNTER.
01486
01487      IF AOPTIONI  IS EQUAL TO '6'
01488         MOVE CK-ACH-PAYMENT      TO  MQ-ENTRY-TYPE
01489      ELSE
01490         MOVE 'Q'                 TO  MQ-ENTRY-TYPE.
01491
01492      MOVE CK-CSR                 TO  MQ-CHEK-CSR
01493                                      MQ-CSR-A1.
01494      MOVE CK-CARRIER             TO  MQ-CHEK-CARRIER
01495                                      MQ-CARRIER-A1.
01496      MOVE CK-GROUPING            TO  MQ-CHEK-GROUPING
01497                                      MQ-GROUPING-A1.
01498      MOVE CK-PAYEE               TO  MQ-CHEK-PAYEE
01499                                      MQ-PAYEE-A1.
01500      MOVE CK-PAYEE-SEQ           TO  MQ-CHEK-PAYEE-SEQ
01501                                      MQ-PAYEE-SEQ-A1.
01502      MOVE CK-SEQUENCE-NO         TO  MQ-CHEK-SEQ-NO.
01503      MOVE CK-CHECK-NO            TO  MQ-CHECK-NUMBER.
01504      MOVE CK-RECORD-TYPE         TO  MQ-RECORD-TYPE.
01505      MOVE CK-PAYEE-INFO          TO  MQ-PAYEE-INFO.
01506      MOVE ZERO                   TO  MQ-TIMES-PRINTED
01507                                      MQ-PRINT-AT-HHMM.
01508      MOVE CK-AMOUNT-PAID         TO  MQ-CHECK-AMOUNT.
01509      MOVE PI-CURRENT-DATE-BIN    TO  MQ-CHECK-RELEASE-DT.
01510      MOVE CK-RECORDED-BY         TO  MQ-CHECK-BY-USER.
01511      MOVE EIBTIME                TO  MQ-LAST-MAINT-HHMMSS.
01512      MOVE PI-CURRENT-DATE-BIN    TO  MQ-LAST-MAINT-DT.
01513      MOVE 'E686'                 TO  MQ-LAST-MAINT-BY.
01514      MOVE CK-AR-STATEMENT-DT     TO  MQ-AR-STATEMENT-DT.
01515      MOVE LOW-VALUES             TO  MQ-CHECK-WRITTEN-DT
01516                                      MQ-VOID-DT.
01517      MOVE COMMISSION-CHECK-QUE   TO  JP-RECORD-AREA.
01518      MOVE 'A'                    TO  JP-RECORD-TYPE.
01519      MOVE WS-ERCMKQ-DSID         TO  JP-FILE-ID.
01520      MOVE +1                     TO  WS-NDX.
01521
01522  0290-LOOP.
01523
01524      IF WS-NDX GREATER THAN 15
01525          GO TO 0300-LOOP-EXIT.
01526
01527      IF CK-TEXT
01528          MOVE CK-STUB-TEXT (WS-NDX)  TO   MQ-STUB-TEXT (WS-NDX)
01529          ADD  +1                     TO   WS-NDX
01530          GO TO 0290-LOOP.
01531
01532      IF CK-STUB-LINE (WS-NDX) = SPACES
01533          MOVE SPACES TO MQ-CHECK-STUB-LINE (WS-NDX)
01534          MOVE ZEROS TO MQ-PYAJ-AMT (WS-NDX)
01535                        MQ-PYAJ-SEQ (WS-NDX)
01536          ADD +1     TO WS-NDX
01537          GO TO 0290-LOOP.
01538
01539      MOVE CK-STUB-COMMENT (WS-NDX)    TO
01540                     MQ-STUB-COMMENT  (WS-NDX).
01541      MOVE CK-ACCT-AGENT (WS-NDX)      TO
01542                     MQ-ACCT-AGENT  (WS-NDX).
01543      MOVE CK-INVOICE (WS-NDX)    TO   MQ-INVOICE (WS-NDX).
01544      MOVE CK-REFERENCE (WS-NDX)  TO   MQ-REFERENCE (WS-NDX).
01545      MOVE CK-LEDGER-NO (WS-NDX)  TO   MQ-LEDGER-NO (WS-NDX).
01546      MOVE ZEROS                  TO   MQ-PYAJ-AMT (WS-NDX).
01547      IF CK-DETAIL-AMT (WS-NDX) NUMERIC
01548          MOVE CK-DETAIL-AMT (WS-NDX)
01549                                  TO   MQ-PYAJ-AMT (WS-NDX).
01550      MOVE CK-PAYMENT-TYPE (WS-NDX)
01551                                  TO   MQ-PAYMENT-TYPE (WS-NDX).
01552      MOVE CK-PAYMENT-TYPE (WS-NDX)
01553                                  TO   MQ-PYAJ-REC-TYPE (WS-NDX).
01554      MOVE CK-LAST-MAINT-APPLIED (WS-NDX)
01555                             TO   MQ-LAST-MAINT-APPLIED (WS-NDX).
01556
01557      MOVE CK-NON-AR-ITEM (WS-NDX) TO  MQ-NON-AR-ITEM (WS-NDX).
01558
01559      MOVE 'C'                    TO   MQ-PYAJ-REC-TYPE (WS-NDX).
01560
01561      MOVE CK-PYAJ-PMT-APPLIED (WS-NDX)
01562                                  TO   MQ-PYAJ-PMT-APPLIED (WS-NDX)
01563      MOVE WS-SEQ-NO              TO   MQ-PYAJ-SEQ (WS-NDX).
01564      ADD  +1                     TO   WS-SEQ-NO
01565                                       WS-NDX.
01566
01567      GO TO 0290-LOOP.
01568
01569  0300-LOOP-EXIT.
01570
01571      
      * EXEC CICS WRITE
01572 *        DATASET (WS-ERCMKQ-DSID)
01573 *        FROM    (COMMISSION-CHECK-QUE)
01574 *        RIDFLD  (MQ-CONTROL-PRIMARY) END-EXEC.
           MOVE LENGTH OF
            COMMISSION-CHECK-QUE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004861' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCMKQ-DSID, 
                 COMMISSION-CHECK-QUE, 
                 DFHEIV11, 
                 MQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01575
01576      PERFORM 8400-LOG-JOURNAL-RECORD.
01577
01578      MOVE COMM-CHECK-RECORDS        TO  JP-RECORD-AREA.
01579      MOVE 'C'                       TO  JP-RECORD-TYPE.
01580      MOVE WS-ERCMCK-DSID            TO  JP-FILE-ID.
01581
01582      
      * EXEC CICS REWRITE
01583 *        DATASET (WS-ERCMCK-DSID)
01584 *        FROM    (COMM-CHECK-RECORDS) END-EXEC.
           MOVE LENGTH OF
            COMM-CHECK-RECORDS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004872' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCMCK-DSID, 
                 COMM-CHECK-RECORDS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01585
01586      PERFORM 8400-LOG-JOURNAL-RECORD.
01587
01588      GO TO 0285-STARTBR-ERCMCK.
01589
01590      EJECT
01591  0400-END-OF-SEARCH.
01592
01593      IF WS-ERPYAJ-BROWSE-STARTED
01594          
      * EXEC CICS ENDBR
01595 *            DATASET (WS-ERPYAJ-DSID)
01596 *            END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004884' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERPYAJ-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01597
01598      IF WS-ERCHEK-BROWSE-STARTED
01599          
      * EXEC CICS ENDBR
01600 *            DATASET (WS-ERCHEK-DSID)
01601 *            END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004889' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCHEK-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01602
01603      IF WS-ERCMCK-BROWSE-STARTED
01604          
      * EXEC CICS ENDBR
01605 *            DATASET (WS-ERCMCK-DSID)
01606 *            END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004894' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCMCK-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01607
01608      MOVE WS-CHECK-QUE-COUNTER   TO  WS-TL1-CONTROL-GROUP.
01609      MOVE WS-RELEASED-COUNT      TO  WS-TL1-COUNT.
01610      MOVE WS-RELEASED-AMOUNT     TO  WS-TL1-AMOUNT.
01611
01612      IF WS-RELEASED-COUNT GREATER 1
01613          MOVE 'S'                TO  WS-TL1-PLURAL
01614       ELSE
01615          MOVE ' '                TO  WS-TL1-PLURAL.
01616
01617      IF EIBAID = DFHPF1
01618          MOVE LOW-VALUES         TO EL686AI
01619          MOVE 'RELEASED'         TO WS-TL1-RELEASE
01620        ELSE
01621         IF WS-RELEASED-COUNT GREATER ZERO
01622             MOVE 'TO BE RELEASED'   TO WS-TL1-RELEASE
01623           ELSE
01624             MOVE +0                 TO PI-PROC-SW
01625             MOVE -1                 TO AOPTIONL
01626             MOVE ER-3048            TO EMI-ERROR
01627             PERFORM 8200-SEND-DATAONLY.
01628
01629      MOVE WS-TOTAL-LINE1         TO  EMI-MESSAGE-AREA (1).
01630      MOVE WS-TOTAL-LINE2         TO  EMI-MESSAGE-AREA (2).
01631
01632      IF EIBAID = DFHPF1
01633          PERFORM 7000-PRINT-CHECKS-WAITING
01634          GO TO 0100-INITIALIZE
01635        ELSE
01636          MOVE +1                 TO PI-PROC-SW
01637          MOVE -1                 TO AOPTIONL
01638          MOVE AL-UNBON           TO AOPTIONA
01639                                     AAMTA
01640          MOVE AL-UABON  TO ABYA ACARRA AGROUPA ASTATA AACCTA
01641                            CERTO01A EFFDT01A CERTO02A EFFDT02A
01642                            CERTO03A EFFDT03A CERTO04A EFFDT04A
01643                            CERTO05A EFFDT05A CERTO06A EFFDT06A
01644                            CERTO07A EFFDT07A CERTO08A EFFDT08A
01645                            CERTO09A EFFDT09A CERTO10A EFFDT10A
01646                            CERTO11A EFFDT11A CERTO12A EFFDT12A
01647                            CERTO13A EFFDT13A CERTO14A EFFDT14A
01648                            CERTO15A EFFDT15A
01649          PERFORM 8200-SEND-DATAONLY.
01650
01651      EJECT
01652  0910-ENQ-BUSY.
01653
01654 *    NOTE *******************************************************
01655 *         *                                                     *
01656 *         *      IF ONE OF THE OTHER PROGRAMS (EL176 OR EL177)  *
01657 *         *  HAS EXCLUSIVE CONTROL OF THE CHECK QUEUE DSID,     *
01658 *         *  SEND A MESSAGE TO THE OPERATOR TO WAIT A FEW       *
01659 *         *  MOMENTS AND TRY AGAIN.                             *
01660 *         *                                                     *
01661 *         *******************************************************.
01662
01663      MOVE +395                   TO  EMI-ERROR.
01664      MOVE -1                     TO  AOPTIONL.
01665      PERFORM 8200-SEND-DATAONLY.
01666
01667      EJECT
01668  7000-PRINT-CHECKS-WAITING SECTION.
01669
01670      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.
01671
01672      MOVE PI-COMPANY-ID          TO WS-CFK-COMPANY-ID.
01673      MOVE '1'                    TO WS-CFK-RECORD-TYPE.
01674      MOVE ZEROS                  TO WS-CFK-SEQUENCE-NO.
01675
01676      
      * EXEC CICS READ
01677 *         DATASET   (WS-ELCNTL-DSID)
01678 *         SET       (ADDRESS OF CONTROL-FILE)
01679 *         RIDFLD    (WS-CONTROL-FILE-KEY) END-EXEC.
      *    MOVE '&"S        E          (   #00004966' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELCNTL-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01680
030612     IF PI-COMPANY-ID = 'DMD' OR 'CID' OR 'AHL'
CIDMOD         MOVE CF-FORMS-PRINTER-ID  TO  PI-PROCESSOR-PRINTER
01682 *        MOVE EIBTRMID       TO CF-FORMS-PRINTER-ID
01683          
      * EXEC CICS START
01684 *             TRANSID    (WS-PRINT-TRANS-ID)
01685 *             FROM       (PROGRAM-INTERFACE-BLOCK)
01686 *             LENGTH     (PI-COMM-LENGTH)
01687 *             TERMID     (CF-FORMS-PRINTER-ID)
01688 *        END-EXEC
      *    MOVE '0( LF                 0   #00004974' TO DFHEIV0
           MOVE X'3028204C4620202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203020' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 WS-PRINT-TRANS-ID, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01689      ELSE
01690          
      * EXEC CICS START
01691 *             TRANSID    (WS-PRINT-TRANS-ID)
01692 *             FROM       (PROGRAM-INTERFACE-BLOCK)
01693 *             LENGTH     (PI-COMM-LENGTH)
01694 *             TERMID     (CF-FORMS-PRINTER-ID)
01695 *        END-EXEC.
      *    MOVE '0( LFT                0   #00004981' TO DFHEIV0
           MOVE X'3028204C4654202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203020' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 WS-PRINT-TRANS-ID, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 CF-FORMS-PRINTER-ID, 
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
           
01696
01697      GO TO 7090-EXIT.
01698
01699  7010-TERMID-ERROR.
01700
01701      MOVE 0412                   TO EMI-ERROR.
01702      MOVE -1                     TO APFKL.
01703      PERFORM 8200-SEND-DATAONLY.
01704
01705  7020-TRANS-ERROR.
01706
01707      MOVE 0413                   TO EMI-ERROR.
01708      MOVE -1                     TO APFKL.
01709      PERFORM 8200-SEND-DATAONLY.
01710
01711  7090-EXIT.
01712      EXIT.
01713
01714      EJECT
01715  8100-SEND-INITIAL-MAP SECTION.
01716
01717      MOVE EIBTIME                TO  WS-TIME-WORK.
01718
01719      MOVE PI-CURRENT-DATE        TO  ADATEO.
01720      MOVE WS-TIME                TO  ATIMEO.
01721      MOVE EMI-MESSAGE-AREA (1)   TO AEMSG1O.
01722      MOVE EMI-MESSAGE-AREA (2)   TO AEMSG2O.
01723
01724      
      * EXEC CICS SEND
01725 *        FROM   (EL686AI)
01726 *        MAPSET (WS-MAPSET-NAME)
01727 *        MAP    (WS-MAP-NAME)
01728 *        CURSOR ERASE END-EXEC.
           MOVE LENGTH OF
            EL686AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005015' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL686AI, 
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
           
01729
01730      PERFORM 9100-RETURN-TRAN.
01731
01732  8100-EXIT.
01733      EXIT.
01734
01735      EJECT
01736  8200-SEND-DATAONLY SECTION.
01737
01738      IF EMI-ERROR = ZERO
01739          MOVE AL-PABOF           TO APF1A
01740          MOVE AL-PADOF           TO ACOMPA
01741       ELSE
01742          PERFORM 9900-ERROR-FORMAT
01743          MOVE AL-PADOF           TO APF1A
01744          MOVE AL-PABOF           TO ACOMPA.
01745
01746      MOVE EIBTIME                TO  WS-TIME-WORK.
01747
01748      MOVE PI-CURRENT-DATE        TO ADATEO.
01749      MOVE WS-TIME                TO ATIMEO.
01750      MOVE EMI-MESSAGE-AREA (1)   TO AEMSG1O.
01751      MOVE EMI-MESSAGE-AREA (2)   TO AEMSG2O.
01752
01753      
      * EXEC CICS SEND DATAONLY
01754 *        FROM   (EL686AI)
01755 *        MAPSET (WS-MAPSET-NAME)
01756 *        MAP    (WS-MAP-NAME)
01757 *        CURSOR END-EXEC.
           MOVE LENGTH OF
            EL686AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00005044' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL686AI, 
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
           
01758
01759      PERFORM 9100-RETURN-TRAN.
01760
01761  8200-EXIT.
01762      EXIT.
01763
01764      EJECT
01765  8300-SEND-TEXT SECTION.
01766
01767      
      * EXEC CICS SEND TEXT
01768 *        FROM   (LOGOFF-TEXT)
01769 *        LENGTH (LOGOFF-LENGTH)
01770 *        ERASE  FREEKB END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005058' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303538' TO DFHEIV0(25:11)
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
           
01771
01772      
      * EXEC CICS RETURN
01773 *        END-EXEC.
      *    MOVE '.(                    &   #00005063' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01774
01775  8300-EXIT.
01776      EXIT.
01777
01778      EJECT
01779  8400-LOG-JOURNAL-RECORD SECTION.
01780
01781      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
01782      MOVE THIS-PGM               TO  JP-PROGRAM-ID.
01783
pemuni*    EXEC CICS JOURNAL
pemuni*        JFILEID (PI-JOURNAL-FILE-ID)
pemuni*        JTYPEID (WS-JOURNAL-TYPE-ID)
pemuni*        FROM    (JOURNAL-RECORD)
pemuni*        LENGTH  (WS-JOURNAL-RECORD-LENGTH) END-EXEC.
01789
01790  8400-EXIT.
01791      EXIT.
01792
01793      EJECT
01794  8500-DATE-CONVERSION SECTION.
01795
01796      
      * EXEC CICS LINK
01797 *        PROGRAM  (ELDATCV)
01798 *        COMMAREA (DATE-CONVERSION-DATA)
01799 *        LENGTH   (DC-COMM-LENGTH) END-EXEC.
      *    MOVE '."C                   ''   #00005087' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01800
01801  8500-EXIT.
01802      EXIT.
01803
01804  8600-DEEDIT  SECTION.
01805      
      * EXEC CICS  BIF DEEDIT
01806 *        FIELD   (DEEDIT-FIELD)
01807 *        LENGTH  (15)
01808 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005096' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01809
01810  8600-EXIT.
01811      EXIT.
01812
01813      EJECT
01814  9000-RETURN-CICS SECTION.
01815
01816      MOVE EL005                  TO  THIS-PGM.
01817      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
01818      PERFORM 9300-XCTL.
01819
01820  9000-EXIT.
01821      EXIT.
01822
01823  9100-RETURN-TRAN SECTION.
01824
01825      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
01826      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
01827
01828      
      * EXEC CICS RETURN
01829 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01830 *        LENGTH   (PI-COMM-LENGTH)
01831 *        TRANSID  (WS-TRANS-ID) END-EXEC.
      *    MOVE '.(CT                  &   #00005119' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01832
01833  9100-EXIT.
01834      EXIT.
01835
01836  9300-XCTL SECTION.
01837
01838      MOVE DFHENTER               TO  EIBAID.
01839
01840      
      * EXEC CICS XCTL
01841 *        PROGRAM  (THIS-PGM)
01842 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01843 *        LENGTH   (PI-COMM-LENGTH) END-EXEC.
      *    MOVE '.$C                   $   #00005131' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01844
01845  9300-EXIT.
01846      EXIT.
01847
01848      EJECT
01849  9400-CLEAR SECTION.
01850
01851      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.
01852      PERFORM 9300-XCTL.
01853
01854  9400-EXIT.
01855      EXIT.
01856
01857  9600-PGMIDERR SECTION.
01858
01859      
      * EXEC CICS HANDLE CONDITION
01860 *        PGMIDERR (8300-SEND-TEXT) END-EXEC.
      *    MOVE '"$L                   ! & #00005150' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303035313530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01861
01862      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM.
01863
01864      MOVE EL005                  TO  THIS-PGM
01865                                      LOGOFF-PGM.
01866      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
01867
01868      MOVE SPACES                 TO  PI-ENTRY-CD-1.
01869      PERFORM 9300-XCTL.
01870
01871  9600-EXIT.
01872      EXIT.
01873
01874      EJECT
01875  9900-ERROR-FORMAT SECTION.
01876
01877      
      * EXEC CICS LINK
01878 *        PROGRAM  (EL001)
01879 *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
01880 *        LENGTH   (EMI-COMM-LENGTH) END-EXEC.
      *    MOVE '."C                   ''   #00005168' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL001, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01881
01882  9900-EXIT.
01883      EXIT.
01884
01885      EJECT
01886  9990-ERROR SECTION.
01887
01888      MOVE DFHEIBLK TO EMI-LINE1.
01889      
      * EXEC CICS LINK
01890 *        PROGRAM  (EL004)
01891 *        COMMAREA (EMI-LINE1)
01892 *        LENGTH   (72) END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00005180' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL004, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01893      MOVE -1 TO APFKL.
01894      PERFORM 8200-SEND-DATAONLY.
01895      GO TO 9100-RETURN-TRAN.
01896
01897  9990-EXIT.
01898      EXIT.
01899
01900  9995-SECURITY-VIOLATION.
01901 *    COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00005209' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323039' TO DFHEIV0(25:11)
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
01902
01903  9995-EXIT.
01904      EXIT.
01905
01906  9999-LAST-PARAGRAPH SECTION.
01907
01908      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL686' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01909

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL686' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMIDERR,
                     0140-NOT-FOUND,
                     0400-END-OF-SEARCH,
                     0910-ENQ-BUSY,
                     7010-TERMID-ERROR,
                     7020-TRANS-ERROR,
                     9990-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0260-END-OF-ERPYAJ
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0400-END-OF-SEARCH
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 0400-END-OF-SEARCH
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL686' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
