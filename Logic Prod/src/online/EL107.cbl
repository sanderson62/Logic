00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL107 .
00004 *              PROGRAM CONVERTED BY
$***********************
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 09/27/95 10:01:12.
00007 *                            VMOD=2.021.
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
00025 *        THIS PROGRAM PROVIDES THE MAINTENANCE FUNCTIONS NEEDED
00026 *    FOR THE BENEFIT CONTROL RECORDS.
00027 *
00028 *    SCREENS     - EL107A - BENEFIT CONTROLS
00029 *
00030 *    ENTERED BY  - EL101 - MAINTENANCE MENU
00031 *
00032 *    EXIT TO     - EL101 - MAINTENANCE MENU
00033 *
00034 *    INPUT FILE  - ELCNTL - CONTROL FILE - BENEFIT RECORDS
00035 *
00036 *    OUTPUT FILE - ELCNTL - CONTROL FILE - BENEFIT RECORDS
00037 *
00038 *    COMMAREA    - PASSED
00039 *
00040 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON
00041 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE
00042 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
00043 *                  ENTRIES (XCTL FROM CICS VIA EX11) THE SCREEN
00044 *                  WILL BE READ AND ACTION WILL BE BASED ON THE
00045 *                  MAINTENANCE TYPE INDICATED.
092602******************************************************************
092602*                   C H A N G E   L O G
092602*
092602* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
092602*-----------------------------------------------------------------
092602*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
092602* EFFECTIVE    NUMBER
092602*-----------------------------------------------------------------
092602* 092602    2002091900008  PEMA  INCREASE NUMBER OF MAXIMUM
092602*                                  BENEFIT CODES FROM 200 TO 450
082503* 082503                   PEMA  ADD BENEFIT CATEGORY
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
051414* 051414    2013100100002  PEMA  Add maximum benefits/crit period
081214* 081214    2014081200001  PEMA  correct max bens init.
092602******************************************************************
00046      EJECT
00047  ENVIRONMENT DIVISION.
00048  DATA DIVISION.
00049  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00050  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.
00051
00052  77  FILLER  PIC X(32)  VALUE '********************************'.
00053  77  FILLER  PIC X(32)  VALUE '*   EL107  WORKING STORAGE     *'.
00054  77  FILLER  PIC X(32)  VALUE '***********VMOD=2.021 **********'.
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
00057 *    COPY ELCSCRTY.
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
00058
00059  01  WS-DATE-AREA.
00060      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00061      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
00062
00063  01  FILLER                          COMP-3.
092602*    05  WS-MAX-BEN-CODES            PIC S999    VALUE +200.
092602     05  WS-MAX-BEN-CODES            PIC S999    VALUE +450.
00065      05  WS-RECORD-COUNT             PIC S9(3)   VALUE ZERO.
00066      05  WS-READNEXT-SW              PIC S9      VALUE ZERO.
00067      05  WS-LAST-ERROR-COUNT         PIC S9(3)   VALUE ZERO.
00068      05  WS-UPDATE-SW                PIC S9      VALUE ZERO.
00069      05  WS-COMPLETED-SUCCESSFUL     PIC S9      VALUE ZERO.
00070        88  TRANSACTION-SUCCESSFUL                    VALUE +1.
00071        88  INITIAL-TRANSACTION                       VALUE +2.
00072        88  CHANGE-SUCCESSFUL                         VALUE +3.
00073
00074      05  TIME-IN                     PIC S9(7)   VALUE ZERO.
00075      05  TIME-OUT                    REDEFINES
00076          TIME-IN                     PIC S9(3)V9(4).
00077
00078  01  FILLER                          COMP SYNC.
00079      05  WS-INDEX                    PIC S9(4)   VALUE ZERO.
00080      05  WS-JOURNAL-FILE-ID          PIC S9(4)   VALUE +1.
00081      05  WS-JOURNAL-RECORD-LENGTH    PIC S9(4)   VALUE +773.
00082
00083  01  FILLER.
00084      05  WS-CONTROL-FILE-KEY.
00085          10  WS-CFK-COMPANY-ID       PIC X(3)    VALUE SPACES.
00086          10  WS-CFK-RECORD-TYPE      PIC X       VALUE SPACES.
00087          10  FILLER                  PIC XX      VALUE SPACES.
00088          10  WS-CFK-BENEFIT-NO       PIC XX      VALUE SPACES.
00089          10  WS-CFK-SEQUENCE-NO      PIC S9(4)   VALUE ZERO COMP.
00090
00091      05  WS-MAPSET-NAME              PIC X(8)  VALUE 'EL107S'.
00092      05  WS-MAP-NAME                 PIC X(8)  VALUE 'EL107A'.
00093
00094      05  FILLER                      REDEFINES
00095          WS-MAP-NAME.
00096          10  FILLER                  PIC XX.
00097          10  WS-MAP-NUMBER           PIC X(4).
00098          10  FILLER                  PIC XX.
00099
00100      05  WS-EDIT-BENE-CODE           PIC XX    VALUE SPACE.
00101          88  INVALID-BENEFIT-CODE    VALUE '  ' '00'
00102                                            '90' THRU '99'.
00103
00104      05  WS-EDIT-LF-OB               PIC X     VALUE SPACE.
00105          88  VALID-LF-OB             VALUE ' ' 'A' 'C' 'D' 'E'
00106                                            'F' 'G' 'I' 'J' 'K'
00107                                            'L' 'M' 'O' 'P' 'S'
00108                                            'T' 'U' 'V' 'W' 'X'
00109                                            'Z' 'H' '2' '3' '4'
00110                                            'N'.
00111
00112      05  WS-EDIT-AH-OB               PIC X     VALUE SPACE.
00113          88  VALID-AH-OB             VALUE ' ' 'A' 'C' 'E' 'F'
00114                                            'G' 'H' 'I' 'J' 'K'
00115                                            'M' 'N' 'O' 'P' 'Q'
00116                                            'S' 'Z' '2' '3' '4'.
00117
00118      05  WS-EDIT-IG-CODE             PIC X     VALUE SPACE.
00119          88  VALID-IG-CODE           VALUE 'I' 'G' ' '.
00120
00121      05  WS-EDIT-REFUND              PIC X     VALUE SPACE.
00122          88  VALID-REFUND-METHOD     VALUE '1' '2' '3' '4'
00123                                            '5' '6' '7' '8' '9'
033104                                           ' ' 'G' 'D' 'S'.
00125
00126      05  WS-EDIT-OVRD-EARNINGS       PIC X     VALUE SPACE.
00127          88  VALID-OVRD-EARNINGS     VALUE '1' '2' '3' '4'
00128                                            '5' '6' '8' 'B' ' '.
00129
00130      05  THIS-PGM                    PIC X(8)  VALUE 'EL107'.
00131
00132      05  WS-CONTROL-FILE-DSID        PIC X(8) VALUE 'ELCNTL'.
00133
00134      05  WS-JOURNAL-TYPE-ID          PIC XX      VALUE 'EL'.
00135
00136      05  WS-LOW-VALUES               PIC X VALUE LOW-VALUES.
00137      05  WS-SPACE                    PIC X       VALUE SPACE.
00138
00139      05  WS-TRANS-ID                 PIC X(4)    VALUE 'EX11'.
00140
00141      05  WS-TEMP-STORAGE-KEY.
00142          10  WS-TS-TERM-ID           PIC X(4)    VALUE 'XXXX'.
00143          10  FILLER                  PIC X(4)    VALUE '107'.
00144
082503     05  WS-BENEFIT-CONTROLS-SAVE    PIC X(53).
00146
00147      05  WS-BENEFIT-CONTROLS-WORK.
00148          10  WS-BENEFIT-NUMBER           PIC XX.
00149          10  WS-BENEFIT-ABBREVIATION     PIC X(3).
00150          10  WS-BENEFIT-DESCRIPTION      PIC X(10).
00151          10  WS-BENEFIT-COMMENT          PIC X(10).
00152          10  WS-BENEFIT-COVERAGE-TYPE    PIC X.
00153          10  WS-BENEFIT-OUTSTANDING-BAL  PIC X.
00154          10  WS-BENEFIT-JOINT-COVERAGE   PIC X.
051414         10  WS-BENEFIT-MAX-BENS         PIC s999 comp-3.
051414         10  FILLER                      PIC X(09).
082503         10  WS-BENEFIT-CATEGORY         PIC X.
00156          10  WS-BENEFIT-LOAN-TYPE        PIC X(8).
00157          10  WS-BENEFIT-REMAIN-TERM      PIC X.
00158          10  WS-BENEFIT-EARN-METHOD      PIC X.
00159          10  WS-BENEFIT-REFUND-METHOD    PIC X.
00160          10  FILLER                      PIC X.
00161          10  WS-BENEFIT-IG-CODE          PIC X.
00162
00163      05  WS-BENEFIT-TABLE-AREA.
092602         10  WS-BENEFIT-TABLE-ENTRY  OCCURS 450 TIMES
00165              INDEXED BY BENEFIT-INDEX.
00166              15  WS-BTE-NUMBER       PIC XX.
082503             15  FILLER              PIC X(53).
00168
00169      EJECT
00170      05  WS-ERROR-MESSAGE-AREA.
00171          10  ER-0000                 PIC 9(4)   VALUE 0000.
00172          10  ER-0004                 PIC 9(4)   VALUE 0004.
00173          10  ER-0006                 PIC 9(4)   VALUE 0006.
00174          10  ER-0008                 PIC 9(4)   VALUE 0008.
00175          10  ER-0022                 PIC 9(4)   VALUE 0022.
00176          10  ER-0023                 PIC 9(4)   VALUE 0023.
00177          10  ER-0024                 PIC 9(4)   VALUE 0024.
00178          10  ER-0025                 PIC 9(4)   VALUE 0025.
00179          10  ER-0026                 PIC 9(4)   VALUE 0026.
00180          10  ER-0027                 PIC 9(4)   VALUE 0027.
00181          10  ER-0028                 PIC 9(4)   VALUE 0028.
00182          10  ER-0029                 PIC 9(4)   VALUE 0029.
00183          10  ER-0036                 PIC 9(4)   VALUE 0036.
00184          10  ER-0037                 PIC 9(4)   VALUE 0037.
00185          10  ER-0038                 PIC 9(4)   VALUE 0038.
00186          10  ER-0039                 PIC 9(4)   VALUE 0039.
00187          10  ER-0040                 PIC 9(4)   VALUE 0040.
00188          10  ER-0068                 PIC 9(4)   VALUE 0068.
00189          10  ER-0070                 PIC 9(4)   VALUE 0070.
00190          10  ER-0127                 PIC 9(4)   VALUE 0127.
00191          10  ER-0128                 PIC 9(4)   VALUE 0128.
00192          10  ER-0129                 PIC 9(4)   VALUE 0129.
00193          10  ER-0138                 PIC 9(4)   VALUE 0138.
00194          10  ER-0139                 PIC 9(4)   VALUE 0139.
00195          10  ER-0582                 PIC 9(4)   VALUE 0582.
00196          10  ER-0592                 PIC 9(4)   VALUE 0592.
00197          10  ER-0595                 PIC 9(4)   VALUE 0595.
00198          10  ER-0596                 PIC 9(4)   VALUE 0596.
00199          10  ER-0640                 PIC 9(4)   VALUE 0640.
00200          10  ER-0752                 PIC 9(4)   VALUE 0752.
00201          10  ER-0847                 PIC 9(4)   VALUE 0847.
00202          10  ER-7535                 PIC 9(4)   VALUE 7535.
051414         10  er-7537                 pic 9(4)   value 7537.
00203          10  ER-8318                 PIC 9(4)   VALUE 8318.
00204
00205      05  WS-HEADING-0.
00206          10  FILLER                  PIC X      VALUE '('.
00207          10  WS-HD0-LF-L1            PIC X.
00208          10  FILLER                  PIC X(4)   VALUE ' OR '.
00209          10  WS-HD0-AH-L1            PIC X.
00210          10  FILLER                  PIC X(19)
00211                                       VALUE ')   BENEFIT CODE  :'.
00212      EJECT
00213 *    COPY ELCINTF.
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
00214      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00215          16  PI-1ST-TIME-SW          PIC S9     COMP-3.
00216          16  PI-MODE                 PIC X.
00217          16  PI-BENEFIT-TYPE         PIC X.
00218          16  PI-BENEFIT-NUMBER       PIC XX.
00219          16  PI-LAST-BENEFIT-NUMBER  PIC XX.
00220          16  PI-NEXT-BENEFIT-NUMBER  PIC XX.
00221          16  PI-LINE-COUNT           PIC S9(3)  COMP-3.
00222          16  PI-BROWSE-SW            PIC S9     COMP-3.
00223          16  PI-LOGIC-CUSTOMER       PIC X.
00224          16  PI-SHOW-SW              PIC S9     COMP-3.
00225          16  PI-CHANGE-SW            PIC S9     COMP-3.
00226          16  PI-UPDATE-KEY           PIC X(10).
00227
00228          16  FILLER                  PIC X(615).
00229
00230      EJECT
00231
00232 *    COPY EL107S.
       01  EL107AI.
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
           05  AMAINTL PIC S9(0004) COMP.
           05  AMAINTF PIC  X(0001).
           05  FILLER REDEFINES AMAINTF.
               10  AMAINTA PIC  X(0001).
           05  AMAINTI PIC  X(0001).
      *    -------------------------------
           05  AKINDL PIC S9(0004) COMP.
           05  AKINDF PIC  X(0001).
           05  FILLER REDEFINES AKINDF.
               10  AKINDA PIC  X(0001).
           05  AKINDI PIC  X(0001).
      *    -------------------------------
           05  AHEAD0L PIC S9(0004) COMP.
           05  AHEAD0F PIC  X(0001).
           05  FILLER REDEFINES AHEAD0F.
               10  AHEAD0A PIC  X(0001).
           05  AHEAD0I PIC  X(0026).
      *    -------------------------------
           05  ABENEL PIC S9(0004) COMP.
           05  ABENEF PIC  X(0001).
           05  FILLER REDEFINES ABENEF.
               10  ABENEA PIC  X(0001).
           05  ABENEI PIC  X(0002).
      *    -------------------------------
           05  AHEAD1L PIC S9(0004) COMP.
           05  AHEAD1F PIC  X(0001).
           05  FILLER REDEFINES AHEAD1F.
               10  AHEAD1A PIC  X(0001).
           05  AHEAD1I PIC  X(0007).
      *    -------------------------------
           05  AHEAD3L PIC S9(0004) COMP.
           05  AHEAD3F PIC  X(0001).
           05  FILLER REDEFINES AHEAD3F.
               10  AHEAD3A PIC  X(0001).
           05  AHEAD3I PIC  X(0001).
      *    -------------------------------
           05  AHEAD5L PIC S9(0004) COMP.
           05  AHEAD5F PIC  X(0001).
           05  FILLER REDEFINES AHEAD5F.
               10  AHEAD5A PIC  X(0001).
           05  AHEAD5I PIC  X(0005).
      *    -------------------------------
           05  AHEAD2L PIC S9(0004) COMP.
           05  AHEAD2F PIC  X(0001).
           05  FILLER REDEFINES AHEAD2F.
               10  AHEAD2A PIC  X(0001).
           05  AHEAD2I PIC  X(0007).
      *    -------------------------------
           05  AHEAD4L PIC S9(0004) COMP.
           05  AHEAD4F PIC  X(0001).
           05  FILLER REDEFINES AHEAD4F.
               10  AHEAD4A PIC  X(0001).
           05  AHEAD4I PIC  X(0001).
      *    -------------------------------
           05  AHEAD6L PIC S9(0004) COMP.
           05  AHEAD6F PIC  X(0001).
           05  FILLER REDEFINES AHEAD6F.
               10  AHEAD6A PIC  X(0001).
           05  AHEAD6I PIC  X(0005).
      *    -------------------------------
           05  ACODE01L PIC S9(0004) COMP.
           05  ACODE01F PIC  X(0001).
           05  FILLER REDEFINES ACODE01F.
               10  ACODE01A PIC  X(0001).
           05  ACODE01I PIC  X(0002).
      *    -------------------------------
           05  AABBR01L PIC S9(0004) COMP.
           05  AABBR01F PIC  X(0001).
           05  FILLER REDEFINES AABBR01F.
               10  AABBR01A PIC  X(0001).
           05  AABBR01I PIC  X(0003).
      *    -------------------------------
           05  ADESC01L PIC S9(0004) COMP.
           05  ADESC01F PIC  X(0001).
           05  FILLER REDEFINES ADESC01F.
               10  ADESC01A PIC  X(0001).
           05  ADESC01I PIC  X(0010).
      *    -------------------------------
           05  ACOMM01L PIC S9(0004) COMP.
           05  ACOMM01F PIC  X(0001).
           05  FILLER REDEFINES ACOMM01F.
               10  ACOMM01A PIC  X(0001).
           05  ACOMM01I PIC  X(0010).
      *    -------------------------------
           05  AMB01L PIC S9(0004) COMP.
           05  AMB01F PIC  X(0001).
           05  FILLER REDEFINES AMB01F.
               10  AMB01A PIC  X(0001).
           05  AMB01I PIC  X(0002).
      *    -------------------------------
           05  ALOAN01L PIC S9(0004) COMP.
           05  ALOAN01F PIC  X(0001).
           05  FILLER REDEFINES ALOAN01F.
               10  ALOAN01A PIC  X(0001).
           05  ALOAN01I PIC  X(0008).
      *    -------------------------------
           05  AEM01L PIC S9(0004) COMP.
           05  AEM01F PIC  X(0001).
           05  FILLER REDEFINES AEM01F.
               10  AEM01A PIC  X(0001).
           05  AEM01I PIC  X(0001).
      *    -------------------------------
           05  AJCM01L PIC S9(0004) COMP.
           05  AJCM01F PIC  X(0001).
           05  FILLER REDEFINES AJCM01F.
               10  AJCM01A PIC  X(0001).
           05  AJCM01I PIC  X(0001).
      *    -------------------------------
           05  AOB01L PIC S9(0004) COMP.
           05  AOB01F PIC  X(0001).
           05  FILLER REDEFINES AOB01F.
               10  AOB01A PIC  X(0001).
           05  AOB01I PIC  X(0001).
      *    -------------------------------
           05  ALD01L PIC S9(0004) COMP.
           05  ALD01F PIC  X(0001).
           05  FILLER REDEFINES ALD01F.
               10  ALD01A PIC  X(0001).
           05  ALD01I PIC  X(0001).
      *    -------------------------------
           05  ART01L PIC S9(0004) COMP.
           05  ART01F PIC  X(0001).
           05  FILLER REDEFINES ART01F.
               10  ART01A PIC  X(0001).
           05  ART01I PIC  X(0001).
      *    -------------------------------
           05  ARM01L PIC S9(0004) COMP.
           05  ARM01F PIC  X(0001).
           05  FILLER REDEFINES ARM01F.
               10  ARM01A PIC  X(0001).
           05  ARM01I PIC  X(0001).
      *    -------------------------------
           05  AIG01L PIC S9(0004) COMP.
           05  AIG01F PIC  X(0001).
           05  FILLER REDEFINES AIG01F.
               10  AIG01A PIC  X(0001).
           05  AIG01I PIC  X(0001).
      *    -------------------------------
           05  ACA01L PIC S9(0004) COMP.
           05  ACA01F PIC  X(0001).
           05  FILLER REDEFINES ACA01F.
               10  ACA01A PIC  X(0001).
           05  ACA01I PIC  X(0001).
      *    -------------------------------
           05  ACODE02L PIC S9(0004) COMP.
           05  ACODE02F PIC  X(0001).
           05  FILLER REDEFINES ACODE02F.
               10  ACODE02A PIC  X(0001).
           05  ACODE02I PIC  X(0002).
      *    -------------------------------
           05  AABBR02L PIC S9(0004) COMP.
           05  AABBR02F PIC  X(0001).
           05  FILLER REDEFINES AABBR02F.
               10  AABBR02A PIC  X(0001).
           05  AABBR02I PIC  X(0003).
      *    -------------------------------
           05  ADESC02L PIC S9(0004) COMP.
           05  ADESC02F PIC  X(0001).
           05  FILLER REDEFINES ADESC02F.
               10  ADESC02A PIC  X(0001).
           05  ADESC02I PIC  X(0010).
      *    -------------------------------
           05  ACOMM02L PIC S9(0004) COMP.
           05  ACOMM02F PIC  X(0001).
           05  FILLER REDEFINES ACOMM02F.
               10  ACOMM02A PIC  X(0001).
           05  ACOMM02I PIC  X(0010).
      *    -------------------------------
           05  AMB02L PIC S9(0004) COMP.
           05  AMB02F PIC  X(0001).
           05  FILLER REDEFINES AMB02F.
               10  AMB02A PIC  X(0001).
           05  AMB02I PIC  X(0002).
      *    -------------------------------
           05  ALOAN02L PIC S9(0004) COMP.
           05  ALOAN02F PIC  X(0001).
           05  FILLER REDEFINES ALOAN02F.
               10  ALOAN02A PIC  X(0001).
           05  ALOAN02I PIC  X(0008).
      *    -------------------------------
           05  AEM02L PIC S9(0004) COMP.
           05  AEM02F PIC  X(0001).
           05  FILLER REDEFINES AEM02F.
               10  AEM02A PIC  X(0001).
           05  AEM02I PIC  X(0001).
      *    -------------------------------
           05  AJCM02L PIC S9(0004) COMP.
           05  AJCM02F PIC  X(0001).
           05  FILLER REDEFINES AJCM02F.
               10  AJCM02A PIC  X(0001).
           05  AJCM02I PIC  X(0001).
      *    -------------------------------
           05  AOB02L PIC S9(0004) COMP.
           05  AOB02F PIC  X(0001).
           05  FILLER REDEFINES AOB02F.
               10  AOB02A PIC  X(0001).
           05  AOB02I PIC  X(0001).
      *    -------------------------------
           05  ALD02L PIC S9(0004) COMP.
           05  ALD02F PIC  X(0001).
           05  FILLER REDEFINES ALD02F.
               10  ALD02A PIC  X(0001).
           05  ALD02I PIC  X(0001).
      *    -------------------------------
           05  ART02L PIC S9(0004) COMP.
           05  ART02F PIC  X(0001).
           05  FILLER REDEFINES ART02F.
               10  ART02A PIC  X(0001).
           05  ART02I PIC  X(0001).
      *    -------------------------------
           05  ARM02L PIC S9(0004) COMP.
           05  ARM02F PIC  X(0001).
           05  FILLER REDEFINES ARM02F.
               10  ARM02A PIC  X(0001).
           05  ARM02I PIC  X(0001).
      *    -------------------------------
           05  AIG02L PIC S9(0004) COMP.
           05  AIG02F PIC  X(0001).
           05  FILLER REDEFINES AIG02F.
               10  AIG02A PIC  X(0001).
           05  AIG02I PIC  X(0001).
      *    -------------------------------
           05  ACA02L PIC S9(0004) COMP.
           05  ACA02F PIC  X(0001).
           05  FILLER REDEFINES ACA02F.
               10  ACA02A PIC  X(0001).
           05  ACA02I PIC  X(0001).
      *    -------------------------------
           05  ACODE03L PIC S9(0004) COMP.
           05  ACODE03F PIC  X(0001).
           05  FILLER REDEFINES ACODE03F.
               10  ACODE03A PIC  X(0001).
           05  ACODE03I PIC  X(0002).
      *    -------------------------------
           05  AABBR03L PIC S9(0004) COMP.
           05  AABBR03F PIC  X(0001).
           05  FILLER REDEFINES AABBR03F.
               10  AABBR03A PIC  X(0001).
           05  AABBR03I PIC  X(0003).
      *    -------------------------------
           05  ADESC03L PIC S9(0004) COMP.
           05  ADESC03F PIC  X(0001).
           05  FILLER REDEFINES ADESC03F.
               10  ADESC03A PIC  X(0001).
           05  ADESC03I PIC  X(0010).
      *    -------------------------------
           05  ACOMM03L PIC S9(0004) COMP.
           05  ACOMM03F PIC  X(0001).
           05  FILLER REDEFINES ACOMM03F.
               10  ACOMM03A PIC  X(0001).
           05  ACOMM03I PIC  X(0010).
      *    -------------------------------
           05  AMB03L PIC S9(0004) COMP.
           05  AMB03F PIC  X(0001).
           05  FILLER REDEFINES AMB03F.
               10  AMB03A PIC  X(0001).
           05  AMB03I PIC  X(0002).
      *    -------------------------------
           05  ALOAN03L PIC S9(0004) COMP.
           05  ALOAN03F PIC  X(0001).
           05  FILLER REDEFINES ALOAN03F.
               10  ALOAN03A PIC  X(0001).
           05  ALOAN03I PIC  X(0008).
      *    -------------------------------
           05  AEM03L PIC S9(0004) COMP.
           05  AEM03F PIC  X(0001).
           05  FILLER REDEFINES AEM03F.
               10  AEM03A PIC  X(0001).
           05  AEM03I PIC  X(0001).
      *    -------------------------------
           05  AJCM03L PIC S9(0004) COMP.
           05  AJCM03F PIC  X(0001).
           05  FILLER REDEFINES AJCM03F.
               10  AJCM03A PIC  X(0001).
           05  AJCM03I PIC  X(0001).
      *    -------------------------------
           05  AOB03L PIC S9(0004) COMP.
           05  AOB03F PIC  X(0001).
           05  FILLER REDEFINES AOB03F.
               10  AOB03A PIC  X(0001).
           05  AOB03I PIC  X(0001).
      *    -------------------------------
           05  ALD03L PIC S9(0004) COMP.
           05  ALD03F PIC  X(0001).
           05  FILLER REDEFINES ALD03F.
               10  ALD03A PIC  X(0001).
           05  ALD03I PIC  X(0001).
      *    -------------------------------
           05  ART03L PIC S9(0004) COMP.
           05  ART03F PIC  X(0001).
           05  FILLER REDEFINES ART03F.
               10  ART03A PIC  X(0001).
           05  ART03I PIC  X(0001).
      *    -------------------------------
           05  ARM03L PIC S9(0004) COMP.
           05  ARM03F PIC  X(0001).
           05  FILLER REDEFINES ARM03F.
               10  ARM03A PIC  X(0001).
           05  ARM03I PIC  X(0001).
      *    -------------------------------
           05  AIG03L PIC S9(0004) COMP.
           05  AIG03F PIC  X(0001).
           05  FILLER REDEFINES AIG03F.
               10  AIG03A PIC  X(0001).
           05  AIG03I PIC  X(0001).
      *    -------------------------------
           05  ACA03L PIC S9(0004) COMP.
           05  ACA03F PIC  X(0001).
           05  FILLER REDEFINES ACA03F.
               10  ACA03A PIC  X(0001).
           05  ACA03I PIC  X(0001).
      *    -------------------------------
           05  ACODE04L PIC S9(0004) COMP.
           05  ACODE04F PIC  X(0001).
           05  FILLER REDEFINES ACODE04F.
               10  ACODE04A PIC  X(0001).
           05  ACODE04I PIC  X(0002).
      *    -------------------------------
           05  AABBR04L PIC S9(0004) COMP.
           05  AABBR04F PIC  X(0001).
           05  FILLER REDEFINES AABBR04F.
               10  AABBR04A PIC  X(0001).
           05  AABBR04I PIC  X(0003).
      *    -------------------------------
           05  ADESC04L PIC S9(0004) COMP.
           05  ADESC04F PIC  X(0001).
           05  FILLER REDEFINES ADESC04F.
               10  ADESC04A PIC  X(0001).
           05  ADESC04I PIC  X(0010).
      *    -------------------------------
           05  ACOMM04L PIC S9(0004) COMP.
           05  ACOMM04F PIC  X(0001).
           05  FILLER REDEFINES ACOMM04F.
               10  ACOMM04A PIC  X(0001).
           05  ACOMM04I PIC  X(0010).
      *    -------------------------------
           05  AMB04L PIC S9(0004) COMP.
           05  AMB04F PIC  X(0001).
           05  FILLER REDEFINES AMB04F.
               10  AMB04A PIC  X(0001).
           05  AMB04I PIC  X(0002).
      *    -------------------------------
           05  ALOAN04L PIC S9(0004) COMP.
           05  ALOAN04F PIC  X(0001).
           05  FILLER REDEFINES ALOAN04F.
               10  ALOAN04A PIC  X(0001).
           05  ALOAN04I PIC  X(0008).
      *    -------------------------------
           05  AEM04L PIC S9(0004) COMP.
           05  AEM04F PIC  X(0001).
           05  FILLER REDEFINES AEM04F.
               10  AEM04A PIC  X(0001).
           05  AEM04I PIC  X(0001).
      *    -------------------------------
           05  AJCM04L PIC S9(0004) COMP.
           05  AJCM04F PIC  X(0001).
           05  FILLER REDEFINES AJCM04F.
               10  AJCM04A PIC  X(0001).
           05  AJCM04I PIC  X(0001).
      *    -------------------------------
           05  AOB04L PIC S9(0004) COMP.
           05  AOB04F PIC  X(0001).
           05  FILLER REDEFINES AOB04F.
               10  AOB04A PIC  X(0001).
           05  AOB04I PIC  X(0001).
      *    -------------------------------
           05  ALD04L PIC S9(0004) COMP.
           05  ALD04F PIC  X(0001).
           05  FILLER REDEFINES ALD04F.
               10  ALD04A PIC  X(0001).
           05  ALD04I PIC  X(0001).
      *    -------------------------------
           05  ART04L PIC S9(0004) COMP.
           05  ART04F PIC  X(0001).
           05  FILLER REDEFINES ART04F.
               10  ART04A PIC  X(0001).
           05  ART04I PIC  X(0001).
      *    -------------------------------
           05  ARM04L PIC S9(0004) COMP.
           05  ARM04F PIC  X(0001).
           05  FILLER REDEFINES ARM04F.
               10  ARM04A PIC  X(0001).
           05  ARM04I PIC  X(0001).
      *    -------------------------------
           05  AIG04L PIC S9(0004) COMP.
           05  AIG04F PIC  X(0001).
           05  FILLER REDEFINES AIG04F.
               10  AIG04A PIC  X(0001).
           05  AIG04I PIC  X(0001).
      *    -------------------------------
           05  ACA04L PIC S9(0004) COMP.
           05  ACA04F PIC  X(0001).
           05  FILLER REDEFINES ACA04F.
               10  ACA04A PIC  X(0001).
           05  ACA04I PIC  X(0001).
      *    -------------------------------
           05  ACODE05L PIC S9(0004) COMP.
           05  ACODE05F PIC  X(0001).
           05  FILLER REDEFINES ACODE05F.
               10  ACODE05A PIC  X(0001).
           05  ACODE05I PIC  X(0002).
      *    -------------------------------
           05  AABBR05L PIC S9(0004) COMP.
           05  AABBR05F PIC  X(0001).
           05  FILLER REDEFINES AABBR05F.
               10  AABBR05A PIC  X(0001).
           05  AABBR05I PIC  X(0003).
      *    -------------------------------
           05  ADESC05L PIC S9(0004) COMP.
           05  ADESC05F PIC  X(0001).
           05  FILLER REDEFINES ADESC05F.
               10  ADESC05A PIC  X(0001).
           05  ADESC05I PIC  X(0010).
      *    -------------------------------
           05  ACOMM05L PIC S9(0004) COMP.
           05  ACOMM05F PIC  X(0001).
           05  FILLER REDEFINES ACOMM05F.
               10  ACOMM05A PIC  X(0001).
           05  ACOMM05I PIC  X(0010).
      *    -------------------------------
           05  AMB05L PIC S9(0004) COMP.
           05  AMB05F PIC  X(0001).
           05  FILLER REDEFINES AMB05F.
               10  AMB05A PIC  X(0001).
           05  AMB05I PIC  X(0002).
      *    -------------------------------
           05  ALOAN05L PIC S9(0004) COMP.
           05  ALOAN05F PIC  X(0001).
           05  FILLER REDEFINES ALOAN05F.
               10  ALOAN05A PIC  X(0001).
           05  ALOAN05I PIC  X(0008).
      *    -------------------------------
           05  AEM05L PIC S9(0004) COMP.
           05  AEM05F PIC  X(0001).
           05  FILLER REDEFINES AEM05F.
               10  AEM05A PIC  X(0001).
           05  AEM05I PIC  X(0001).
      *    -------------------------------
           05  AJCM05L PIC S9(0004) COMP.
           05  AJCM05F PIC  X(0001).
           05  FILLER REDEFINES AJCM05F.
               10  AJCM05A PIC  X(0001).
           05  AJCM05I PIC  X(0001).
      *    -------------------------------
           05  AOB05L PIC S9(0004) COMP.
           05  AOB05F PIC  X(0001).
           05  FILLER REDEFINES AOB05F.
               10  AOB05A PIC  X(0001).
           05  AOB05I PIC  X(0001).
      *    -------------------------------
           05  ALD05L PIC S9(0004) COMP.
           05  ALD05F PIC  X(0001).
           05  FILLER REDEFINES ALD05F.
               10  ALD05A PIC  X(0001).
           05  ALD05I PIC  X(0001).
      *    -------------------------------
           05  ART05L PIC S9(0004) COMP.
           05  ART05F PIC  X(0001).
           05  FILLER REDEFINES ART05F.
               10  ART05A PIC  X(0001).
           05  ART05I PIC  X(0001).
      *    -------------------------------
           05  ARM05L PIC S9(0004) COMP.
           05  ARM05F PIC  X(0001).
           05  FILLER REDEFINES ARM05F.
               10  ARM05A PIC  X(0001).
           05  ARM05I PIC  X(0001).
      *    -------------------------------
           05  AIG05L PIC S9(0004) COMP.
           05  AIG05F PIC  X(0001).
           05  FILLER REDEFINES AIG05F.
               10  AIG05A PIC  X(0001).
           05  AIG05I PIC  X(0001).
      *    -------------------------------
           05  ACA05L PIC S9(0004) COMP.
           05  ACA05F PIC  X(0001).
           05  FILLER REDEFINES ACA05F.
               10  ACA05A PIC  X(0001).
           05  ACA05I PIC  X(0001).
      *    -------------------------------
           05  ACODE06L PIC S9(0004) COMP.
           05  ACODE06F PIC  X(0001).
           05  FILLER REDEFINES ACODE06F.
               10  ACODE06A PIC  X(0001).
           05  ACODE06I PIC  X(0002).
      *    -------------------------------
           05  AABBR06L PIC S9(0004) COMP.
           05  AABBR06F PIC  X(0001).
           05  FILLER REDEFINES AABBR06F.
               10  AABBR06A PIC  X(0001).
           05  AABBR06I PIC  X(0003).
      *    -------------------------------
           05  ADESC06L PIC S9(0004) COMP.
           05  ADESC06F PIC  X(0001).
           05  FILLER REDEFINES ADESC06F.
               10  ADESC06A PIC  X(0001).
           05  ADESC06I PIC  X(0010).
      *    -------------------------------
           05  ACOMM06L PIC S9(0004) COMP.
           05  ACOMM06F PIC  X(0001).
           05  FILLER REDEFINES ACOMM06F.
               10  ACOMM06A PIC  X(0001).
           05  ACOMM06I PIC  X(0010).
      *    -------------------------------
           05  AMB06L PIC S9(0004) COMP.
           05  AMB06F PIC  X(0001).
           05  FILLER REDEFINES AMB06F.
               10  AMB06A PIC  X(0001).
           05  AMB06I PIC  X(0002).
      *    -------------------------------
           05  ALOAN06L PIC S9(0004) COMP.
           05  ALOAN06F PIC  X(0001).
           05  FILLER REDEFINES ALOAN06F.
               10  ALOAN06A PIC  X(0001).
           05  ALOAN06I PIC  X(0008).
      *    -------------------------------
           05  AEM06L PIC S9(0004) COMP.
           05  AEM06F PIC  X(0001).
           05  FILLER REDEFINES AEM06F.
               10  AEM06A PIC  X(0001).
           05  AEM06I PIC  X(0001).
      *    -------------------------------
           05  AJCM06L PIC S9(0004) COMP.
           05  AJCM06F PIC  X(0001).
           05  FILLER REDEFINES AJCM06F.
               10  AJCM06A PIC  X(0001).
           05  AJCM06I PIC  X(0001).
      *    -------------------------------
           05  AOB06L PIC S9(0004) COMP.
           05  AOB06F PIC  X(0001).
           05  FILLER REDEFINES AOB06F.
               10  AOB06A PIC  X(0001).
           05  AOB06I PIC  X(0001).
      *    -------------------------------
           05  ALD06L PIC S9(0004) COMP.
           05  ALD06F PIC  X(0001).
           05  FILLER REDEFINES ALD06F.
               10  ALD06A PIC  X(0001).
           05  ALD06I PIC  X(0001).
      *    -------------------------------
           05  ART06L PIC S9(0004) COMP.
           05  ART06F PIC  X(0001).
           05  FILLER REDEFINES ART06F.
               10  ART06A PIC  X(0001).
           05  ART06I PIC  X(0001).
      *    -------------------------------
           05  ARM06L PIC S9(0004) COMP.
           05  ARM06F PIC  X(0001).
           05  FILLER REDEFINES ARM06F.
               10  ARM06A PIC  X(0001).
           05  ARM06I PIC  X(0001).
      *    -------------------------------
           05  AIG06L PIC S9(0004) COMP.
           05  AIG06F PIC  X(0001).
           05  FILLER REDEFINES AIG06F.
               10  AIG06A PIC  X(0001).
           05  AIG06I PIC  X(0001).
      *    -------------------------------
           05  ACA06L PIC S9(0004) COMP.
           05  ACA06F PIC  X(0001).
           05  FILLER REDEFINES ACA06F.
               10  ACA06A PIC  X(0001).
           05  ACA06I PIC  X(0001).
      *    -------------------------------
           05  ACODE07L PIC S9(0004) COMP.
           05  ACODE07F PIC  X(0001).
           05  FILLER REDEFINES ACODE07F.
               10  ACODE07A PIC  X(0001).
           05  ACODE07I PIC  X(0002).
      *    -------------------------------
           05  AABBR07L PIC S9(0004) COMP.
           05  AABBR07F PIC  X(0001).
           05  FILLER REDEFINES AABBR07F.
               10  AABBR07A PIC  X(0001).
           05  AABBR07I PIC  X(0003).
      *    -------------------------------
           05  ADESC07L PIC S9(0004) COMP.
           05  ADESC07F PIC  X(0001).
           05  FILLER REDEFINES ADESC07F.
               10  ADESC07A PIC  X(0001).
           05  ADESC07I PIC  X(0010).
      *    -------------------------------
           05  ACOMM07L PIC S9(0004) COMP.
           05  ACOMM07F PIC  X(0001).
           05  FILLER REDEFINES ACOMM07F.
               10  ACOMM07A PIC  X(0001).
           05  ACOMM07I PIC  X(0010).
      *    -------------------------------
           05  AMB07L PIC S9(0004) COMP.
           05  AMB07F PIC  X(0001).
           05  FILLER REDEFINES AMB07F.
               10  AMB07A PIC  X(0001).
           05  AMB07I PIC  X(0002).
      *    -------------------------------
           05  ALOAN07L PIC S9(0004) COMP.
           05  ALOAN07F PIC  X(0001).
           05  FILLER REDEFINES ALOAN07F.
               10  ALOAN07A PIC  X(0001).
           05  ALOAN07I PIC  X(0008).
      *    -------------------------------
           05  AEM07L PIC S9(0004) COMP.
           05  AEM07F PIC  X(0001).
           05  FILLER REDEFINES AEM07F.
               10  AEM07A PIC  X(0001).
           05  AEM07I PIC  X(0001).
      *    -------------------------------
           05  AJCM07L PIC S9(0004) COMP.
           05  AJCM07F PIC  X(0001).
           05  FILLER REDEFINES AJCM07F.
               10  AJCM07A PIC  X(0001).
           05  AJCM07I PIC  X(0001).
      *    -------------------------------
           05  AOB07L PIC S9(0004) COMP.
           05  AOB07F PIC  X(0001).
           05  FILLER REDEFINES AOB07F.
               10  AOB07A PIC  X(0001).
           05  AOB07I PIC  X(0001).
      *    -------------------------------
           05  ALD07L PIC S9(0004) COMP.
           05  ALD07F PIC  X(0001).
           05  FILLER REDEFINES ALD07F.
               10  ALD07A PIC  X(0001).
           05  ALD07I PIC  X(0001).
      *    -------------------------------
           05  ART07L PIC S9(0004) COMP.
           05  ART07F PIC  X(0001).
           05  FILLER REDEFINES ART07F.
               10  ART07A PIC  X(0001).
           05  ART07I PIC  X(0001).
      *    -------------------------------
           05  ARM07L PIC S9(0004) COMP.
           05  ARM07F PIC  X(0001).
           05  FILLER REDEFINES ARM07F.
               10  ARM07A PIC  X(0001).
           05  ARM07I PIC  X(0001).
      *    -------------------------------
           05  AIG07L PIC S9(0004) COMP.
           05  AIG07F PIC  X(0001).
           05  FILLER REDEFINES AIG07F.
               10  AIG07A PIC  X(0001).
           05  AIG07I PIC  X(0001).
      *    -------------------------------
           05  ACA07L PIC S9(0004) COMP.
           05  ACA07F PIC  X(0001).
           05  FILLER REDEFINES ACA07F.
               10  ACA07A PIC  X(0001).
           05  ACA07I PIC  X(0001).
      *    -------------------------------
           05  ACODE08L PIC S9(0004) COMP.
           05  ACODE08F PIC  X(0001).
           05  FILLER REDEFINES ACODE08F.
               10  ACODE08A PIC  X(0001).
           05  ACODE08I PIC  X(0002).
      *    -------------------------------
           05  AABBR08L PIC S9(0004) COMP.
           05  AABBR08F PIC  X(0001).
           05  FILLER REDEFINES AABBR08F.
               10  AABBR08A PIC  X(0001).
           05  AABBR08I PIC  X(0003).
      *    -------------------------------
           05  ADESC08L PIC S9(0004) COMP.
           05  ADESC08F PIC  X(0001).
           05  FILLER REDEFINES ADESC08F.
               10  ADESC08A PIC  X(0001).
           05  ADESC08I PIC  X(0010).
      *    -------------------------------
           05  ACOMM08L PIC S9(0004) COMP.
           05  ACOMM08F PIC  X(0001).
           05  FILLER REDEFINES ACOMM08F.
               10  ACOMM08A PIC  X(0001).
           05  ACOMM08I PIC  X(0010).
      *    -------------------------------
           05  AMB08L PIC S9(0004) COMP.
           05  AMB08F PIC  X(0001).
           05  FILLER REDEFINES AMB08F.
               10  AMB08A PIC  X(0001).
           05  AMB08I PIC  X(0002).
      *    -------------------------------
           05  ALOAN08L PIC S9(0004) COMP.
           05  ALOAN08F PIC  X(0001).
           05  FILLER REDEFINES ALOAN08F.
               10  ALOAN08A PIC  X(0001).
           05  ALOAN08I PIC  X(0008).
      *    -------------------------------
           05  AEM08L PIC S9(0004) COMP.
           05  AEM08F PIC  X(0001).
           05  FILLER REDEFINES AEM08F.
               10  AEM08A PIC  X(0001).
           05  AEM08I PIC  X(0001).
      *    -------------------------------
           05  AJCM08L PIC S9(0004) COMP.
           05  AJCM08F PIC  X(0001).
           05  FILLER REDEFINES AJCM08F.
               10  AJCM08A PIC  X(0001).
           05  AJCM08I PIC  X(0001).
      *    -------------------------------
           05  AOB08L PIC S9(0004) COMP.
           05  AOB08F PIC  X(0001).
           05  FILLER REDEFINES AOB08F.
               10  AOB08A PIC  X(0001).
           05  AOB08I PIC  X(0001).
      *    -------------------------------
           05  ALD08L PIC S9(0004) COMP.
           05  ALD08F PIC  X(0001).
           05  FILLER REDEFINES ALD08F.
               10  ALD08A PIC  X(0001).
           05  ALD08I PIC  X(0001).
      *    -------------------------------
           05  ART08L PIC S9(0004) COMP.
           05  ART08F PIC  X(0001).
           05  FILLER REDEFINES ART08F.
               10  ART08A PIC  X(0001).
           05  ART08I PIC  X(0001).
      *    -------------------------------
           05  ARM08L PIC S9(0004) COMP.
           05  ARM08F PIC  X(0001).
           05  FILLER REDEFINES ARM08F.
               10  ARM08A PIC  X(0001).
           05  ARM08I PIC  X(0001).
      *    -------------------------------
           05  AIG08L PIC S9(0004) COMP.
           05  AIG08F PIC  X(0001).
           05  FILLER REDEFINES AIG08F.
               10  AIG08A PIC  X(0001).
           05  AIG08I PIC  X(0001).
      *    -------------------------------
           05  ACA08L PIC S9(0004) COMP.
           05  ACA08F PIC  X(0001).
           05  FILLER REDEFINES ACA08F.
               10  ACA08A PIC  X(0001).
           05  ACA08I PIC  X(0001).
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
       01  EL107AO REDEFINES EL107AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AKINDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHEAD0O PIC  X(0026).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABENEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHEAD1O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHEAD3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHEAD5O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHEAD2O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHEAD4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHEAD6O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACODE01O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AABBR01O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADESC01O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACOMM01O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMB01O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALOAN01O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEM01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AJCM01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AOB01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALD01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ART01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARM01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIG01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACA01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACODE02O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AABBR02O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADESC02O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACOMM02O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMB02O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALOAN02O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEM02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AJCM02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AOB02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALD02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ART02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARM02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIG02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACA02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACODE03O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AABBR03O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADESC03O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACOMM03O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMB03O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALOAN03O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEM03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AJCM03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AOB03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALD03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ART03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARM03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIG03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACA03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACODE04O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AABBR04O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADESC04O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACOMM04O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMB04O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALOAN04O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEM04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AJCM04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AOB04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALD04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ART04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARM04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIG04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACA04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACODE05O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AABBR05O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADESC05O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACOMM05O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMB05O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALOAN05O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEM05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AJCM05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AOB05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALD05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ART05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARM05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIG05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACA05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACODE06O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AABBR06O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADESC06O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACOMM06O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMB06O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALOAN06O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEM06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AJCM06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AOB06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALD06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ART06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARM06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIG06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACA06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACODE07O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AABBR07O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADESC07O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACOMM07O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMB07O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALOAN07O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEM07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AJCM07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AOB07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALD07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ART07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARM07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIG07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACA07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACODE08O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AABBR08O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADESC08O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACOMM08O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMB08O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALOAN08O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEM08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AJCM08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AOB08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALD08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ART08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARM08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIG08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACA08O PIC  X(0001).
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
00233
00234  01  EL107AO-R REDEFINES EL107AI.
00235      05  FILLER                      PIC X(117).
00236      05  WS-MAP-LINE                 OCCURS 8 TIMES
00237                                      INDEXED BY EL107A-INDEX.
00238          10  EL107A-CODE-LENGTH      PIC S9(4)  COMP.
00239          10  EL107A-CODE-ATTRB       PIC X.
00240          10  EL107A-CODE-O           PIC X(2).
00241          10  EL107A-CODE-I           REDEFINES
00242              EL107A-CODE-O           PIC X(2).
00243          10  EL107A-ABBR-LENGTH      PIC S9(4)   COMP.
00244          10  EL107A-ABBR-ATTRB       PIC X.
00245          10  EL107A-ABBR-O           PIC X(3).
00246          10  EL107A-ABBR-I           REDEFINES
00247              EL107A-ABBR-O           PIC X(3).
00248          10  FILLER                  REDEFINES
00249              EL107A-ABBR-O.
00250              15  EL107A-ABBR-CHAR-12 PIC XX.
00251              15  EL107A-ABBR-CHAR-3  PIC X.
00252          10  EL107A-DESC-LENGTH      PIC S9(4)   COMP.
00253          10  EL107A-DESC-ATTRB       PIC X.
00254          10  EL107A-DESC-O           PIC X(10).
00255          10  EL107A-DESC-I           REDEFINES
00256              EL107A-DESC-O           PIC X(10).
00257          10  EL107A-COMMENT-LENGTH   PIC S9(4)   COMP.
00258          10  EL107A-COMMENT-ATTRB    PIC X.
00259          10  EL107A-COMMENT-O        PIC X(10).
00260          10  EL107A-COMMENT-I        REDEFINES
00261              EL107A-COMMENT-O        PIC X(10).
051414         10  EL107A-MAX-BENS-LENGTH  PIC S9(4)   COMP.
051414         10  EL107A-MAX-BENS-ATTRB   PIC X.
051414         10  EL107A-MAX-BENS-O       PIC 99.
051414         10  EL107A-MAX-BENS-I       REDEFINES
051414             EL107A-MAX-BENS-O       PIC XX.
00262          10  EL107A-LOAN-LENGTH      PIC S9(4)   COMP.
00263          10  EL107A-LOAN-ATTRB       PIC X.
00264          10  EL107A-LOAN-O           PIC X(8).
00265          10  EL107A-LOAN-I           REDEFINES
00266              EL107A-LOAN-O           PIC X(8).
00267          10  EL107A-EM-LENGTH        PIC S9(4)   COMP.
00268          10  EL107A-EM-ATTRB         PIC X.
00269          10  EL107A-EM-O             PIC X.
00270          10  EL107A-EM-I             REDEFINES
00271              EL107A-EM-O             PIC X.
00272          10  EL107A-JOINT-LENGTH     PIC S9(4)   COMP.
00273          10  EL107A-JOINT-ATTRB      PIC X.
00274          10  EL107A-JOINT-O          PIC X.
00275          10  EL107A-JOINT-I          REDEFINES
00276              EL107A-JOINT-O          PIC X.
00277          10  EL107A-OB-LENGTH        PIC S9(4)   COMP.
00278          10  EL107A-OB-ATTRB         PIC X.
00279          10  EL107A-OB-O             PIC X.
00280          10  EL107A-OB-I             REDEFINES
00281              EL107A-OB-O             PIC X.
00282          10  EL107A-LOD-LENGTH       PIC S9(4)   COMP.
00283          10  EL107A-LOD-ATTRB        PIC X.
00284          10  EL107A-LOD-O            PIC X.
00285          10  EL107A-LOD-I            REDEFINES
00286              EL107A-LOD-O            PIC X.
00287          10  EL107A-RTM-LENGTH       PIC S9(4)   COMP.
00288          10  EL107A-RTM-ATTRB        PIC X.
00289          10  EL107A-RTM-O            PIC X.
00290          10  EL107A-RTM-I            REDEFINES
00291              EL107A-RTM-O            PIC X.
00292          10  EL107A-RFM-LENGTH       PIC S9(4)   COMP.
00293          10  EL107A-RFM-ATTRB        PIC X.
00294          10  EL107A-RFM-O            PIC X.
00295          10  EL107A-RFM-I            REDEFINES
00296              EL107A-RFM-O            PIC X.
00297          10  EL107A-IGC-LENGTH       PIC S9(4)   COMP.
00298          10  EL107A-IGC-ATTRB        PIC X.
00299          10  EL107A-IGC-O            PIC X.
00300          10  EL107A-IGC-I            REDEFINES
00301              EL107A-IGC-O            PIC X.
00297          10  EL107A-CAC-LENGTH       PIC S9(4)   COMP.
00298          10  EL107A-CAC-ATTRB        PIC X.
00299          10  EL107A-CAC-O            PIC X.
00300          10  EL107A-CAC-I            REDEFINES
00301              EL107A-CAC-O            PIC X.
00302
00303      EJECT
00304 *    COPY ELCJPFX.
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
00305                                      PIC X(750).
00306      EJECT
00307 *    COPY ELCEMIB.
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
00308
00309      EJECT
00310 *    COPY ELCDATE.
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
00311
00312      EJECT
00313 *    COPY ELCLOGOF.
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
00314
00315      EJECT
00316 *    COPY ELCATTR.
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
00317
00318      EJECT
00319 *    COPY ELCAID.
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
00320
00321  01  FILLER    REDEFINES DFHAID.
00322      05  FILLER                      PIC X(8).
00323      05  PF-VALUES                   PIC X
00324          OCCURS 24 TIMES.
00325
00326      EJECT
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
00328  01  DFHCOMMAREA                     PIC X(1024).
00329
00330 *01 DFHBLLDS                         COMP SYNC.
00331 *    05  BLLCBAR                     PIC S9(8).
00332 *    05  CFCBAR                      PIC S9(8).
00333
00334      EJECT
00335 *    COPY ELCCNTL.
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
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
012913* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
032813* 032813    2011013100001  AJRA  ADD CLAIM REAUDIT FIELDS
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
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
032813
032813         16  CF-CO-REAUDIT-INTERVAL        PIC S9(5)   COMP-3.
031808
091813         16  CF-APPROV-LEV-5.
091813             20  CF-LIFE-PAY-APP-LEVEL-5    PIC S9(7)   COMP-3.
091813             20  CF-AH-PAY-APP-LEVEL-5      PIC S9(7)   COMP-3.
091813             20  CF-AH-APP-DAY-LEVEL-5      PIC S9(5)   COMP-3.
091813
091813         16  FILLER                         PIC X(68).
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
091813             88  APPROVAL-LEVEL-5                   VALUE '5'.
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
011812
011812         16  CF-CSR-IND                         PIC X.
011812         16  FILLER                             PIC X(239).
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
012913         16  CF-ST-CAUSAL-STATE             PIC X.
012913         16  FILLER                         PIC X(185).
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
                   20  cf-maximum-benefits        pic s999 comp-3.
                   20  FILLER                     PIC X(09).
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
032813         16  CF-CARRIER-NEXT-AUDIT-CHK-NO   PIC S9(8)     COMP.
032813         16  FILLER                         PIC X(444).
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
00336
00337      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL107' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00339
00340      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00341      MOVE '5'                   TO DC-OPTION-CODE.
00342      PERFORM 8500-DATE-CONVERSION.
00343      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00344      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00345
00346      MOVE DFHCOMMAREA           TO  PROGRAM-INTERFACE-BLOCK.
00347
00348      MOVE +2                    TO  EMI-NUMBER-OF-LINES
00349                                     EMI-SWITCH2.
00350
00351      IF EIBCALEN NOT GREATER ZERO
00352          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00353          GO TO 8300-SEND-TEXT.
00354
00355      
      * EXEC CICS HANDLE CONDITION
00356 *        PGMIDERR (9600-PGMIDERR)
00357 *        ERROR    (9990-ERROR)
00358 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00003878' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033383738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00359
00360      EJECT
00361      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00362          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00363              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00364              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00365              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00366              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00367              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00368              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00369              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00370              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
00371            ELSE
00372              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00373              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00374              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00375              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00376              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00377              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00378              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00379              MOVE SPACES               TO  PI-SAVED-PROGRAM-6.
00380
00381      IF EIBTRNID NOT EQUAL WS-TRANS-ID
00382          GO TO 1000-INITIAL-SCREEN.
00383
00384      IF EIBAID = DFHCLEAR
00385          GO TO 9400-CLEAR.
00386
00387      IF NOT SYSTEM-DISPLAY-CAP
00388          MOVE 'READ'         TO SM-READ
00389          PERFORM 9995-SECURITY-VIOLATION
00390          MOVE ER-0070        TO EMI-ERROR
00391          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00392          PERFORM 8100-SEND-INITIAL-MAP
00393          GO TO 9100-RETURN-TRAN.
00394
00395      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00396          MOVE LOW-VALUES         TO  EL107AO
00397          MOVE -1                 TO  APFKL
00398          MOVE ER-0008            TO  EMI-ERROR
00399          PERFORM 8200-SEND-DATAONLY
00400          GO TO 9100-RETURN-TRAN.
00401
00402      
      * EXEC CICS RECEIVE
00403 *        INTO   (EL107AO)
00404 *        MAPSET (WS-MAPSET-NAME)
00405 *        MAP    (WS-MAP-NAME)
00406 *    END-EXEC.
           MOVE LENGTH OF
            EL107AO
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003925' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL107AO, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00407
00408      IF APFKL GREATER ZERO
00409          IF EIBAID NOT = DFHENTER
00410              MOVE ER-0004        TO  EMI-ERROR
00411              MOVE AL-UNBOF       TO  APFKA
00412              MOVE -1             TO  APFKL
00413              PERFORM 8200-SEND-DATAONLY
00414              GO TO 9100-RETURN-TRAN
00415            ELSE
00416              IF APFKO NUMERIC   AND
00417                (APFKO GREATER ZERO AND LESS '25')
00418                  MOVE PF-VALUES (APFKI)  TO  EIBAID
00419                ELSE
00420                  MOVE ER-0029        TO  EMI-ERROR
00421                  MOVE AL-UNBOF       TO  APFKA
00422                  MOVE -1             TO  APFKL
00423                  PERFORM 8200-SEND-DATAONLY
00424                  GO TO 9100-RETURN-TRAN.
00425
00426      IF EIBAID = DFHPF12
00427          MOVE 'EL010'         TO  THIS-PGM
00428          GO TO 9300-XCTL.
00429
00430      IF EIBAID = DFHPF23
00431          GO TO 9000-RETURN-CICS.
00432
00433      IF EIBAID = DFHPF24
00434         IF CREDIT-SESSION
00435              MOVE 'EL626'     TO  THIS-PGM
00436              GO TO 9300-XCTL
00437          ELSE
00438              MOVE 'EL126'     TO  THIS-PGM
00439              GO TO 9300-XCTL.
00440
00441      IF EIBAID = DFHENTER OR DFHPF1
00442          NEXT SENTENCE
00443        ELSE
00444          MOVE ER-0008            TO  EMI-ERROR
00445          MOVE -1                 TO  APFKL
00446          PERFORM 8200-SEND-DATAONLY
00447          GO TO 9100-RETURN-TRAN.
00448
00449      EJECT
00450      IF AMAINTL GREATER ZERO
00451          IF AMAINTI = 'A' OR 'C' OR 'D' OR 'S'
00452              MOVE AL-UANON       TO  AMAINTA
00453              MOVE AMAINTI        TO  PI-MODE
00454            ELSE
00455              MOVE AL-UABOF       TO  AMAINTA
00456              MOVE -1             TO  AMAINTL
00457              MOVE ER-0023        TO  EMI-ERROR
00458              PERFORM 9900-ERROR-FORMAT
00459        ELSE
00460          IF EIBAID = DFHPF1
00461              MOVE 'S'            TO  PI-MODE
00462            ELSE
00463              IF PI-1ST-TIME-SW NOT = ZERO
00464                  NEXT SENTENCE
00465                ELSE
00466                  MOVE AL-UABOF   TO  AMAINTA
00467                  MOVE -1         TO  AMAINTL
00468                  MOVE ER-0023    TO  EMI-ERROR
00469                  PERFORM 9900-ERROR-FORMAT.
00470
00471      IF SYSTEM-MODIFY-CAP
00472          NEXT SENTENCE
00473        ELSE
00474          IF AMAINTI = 'A' OR 'C' OR 'D'
00475          MOVE 'UPDATE'           TO SM-READ
00476          PERFORM 9995-SECURITY-VIOLATION
00477          MOVE ER-0070            TO EMI-ERROR
00478          MOVE -1                 TO  AMAINTL
00479          MOVE AL-UABON           TO  AMAINTA
00480          PERFORM 8200-SEND-DATAONLY
00481          GO TO 9100-RETURN-TRAN.
00482
00483      IF AKINDL GREATER ZERO
00484          IF AKINDI = PI-LIFE-OVERRIDE-L1  OR  PI-AH-OVERRIDE-L1
00485              MOVE AL-UANON       TO  AKINDA
00486              IF (AKINDI = PI-LIFE-OVERRIDE-L1  AND
00487                                     PI-BENEFIT-TYPE NOT = '4') OR
00488                 (AKINDI = PI-AH-OVERRIDE-L1  AND
00489                                     PI-BENEFIT-TYPE NOT = '5')
00490                  MOVE SPACES     TO  PI-BENEFIT-NUMBER
00491                                      PI-NEXT-BENEFIT-NUMBER
00492                  MOVE ZERO       TO  PI-BROWSE-SW
00493                  IF AKINDI = PI-LIFE-OVERRIDE-L1
00494                      MOVE '4' TO PI-BENEFIT-TYPE
00495                    ELSE
00496                      MOVE '5' TO PI-BENEFIT-TYPE
00497                ELSE
00498                  IF AKINDI = PI-LIFE-OVERRIDE-L1
00499                      MOVE '4' TO PI-BENEFIT-TYPE
00500                    ELSE
00501                      MOVE '5' TO PI-BENEFIT-TYPE
00502            ELSE
00503              MOVE AL-UABOF       TO  AKINDA
00504              MOVE -1             TO  AKINDL
00505              MOVE ER-0024        TO  EMI-ERROR
00506              PERFORM 9900-ERROR-FORMAT
00507        ELSE
00508          IF EIBAID = DFHPF1
00509              MOVE '4'            TO  PI-BENEFIT-TYPE
00510            ELSE
00511              IF PI-1ST-TIME-SW NOT = ZERO
00512                  NEXT SENTENCE
00513                ELSE
00514                  MOVE AL-UABOF   TO  AKINDA
00515                  MOVE -1         TO  AKINDL
00516                  MOVE ER-0024    TO  EMI-ERROR
00517                  PERFORM 9900-ERROR-FORMAT.
00518
00519      IF ABENEL GREATER ZERO  OR
00520         AMAINTI = 'D' OR 'C'
00521          MOVE ABENEI             TO WS-EDIT-BENE-CODE
00522          IF NOT INVALID-BENEFIT-CODE
00523              MOVE AL-UANON       TO  ABENEA
00524              MOVE ABENEI         TO  PI-BENEFIT-NUMBER
00525                                      PI-NEXT-BENEFIT-NUMBER
00526              MOVE +1             TO  PI-BROWSE-SW
00527            ELSE
00528              MOVE AL-UABOF       TO  ABENEA
00529              MOVE -1             TO  ABENEL
00530              MOVE ER-0025        TO  EMI-ERROR
00531              PERFORM 9900-ERROR-FORMAT
00532        ELSE
00533          IF PI-1ST-TIME-SW NOT = ZERO
00534              NEXT SENTENCE
00535            ELSE
00536              MOVE SPACES         TO  PI-BENEFIT-NUMBER.
00537
00538      IF EMI-FATAL-CTR GREATER ZERO
00539          PERFORM 8200-SEND-DATAONLY
00540          GO TO 9100-RETURN-TRAN.
00541
00542      MOVE +1                     TO  PI-1ST-TIME-SW.
00543
00544      IF PI-MODE NOT EQUAL 'C'
00545          MOVE +0                 TO  PI-CHANGE-SW.
00546
00547      IF PI-MODE NOT EQUAL 'S'
00548          MOVE +1                 TO  PI-BROWSE-SW.
00549
00550      IF PI-MODE EQUAL 'S'
00551          GO TO 2000-PROCESS-SHOW.
00552
00553      IF PI-MODE EQUAL 'C'
00554          GO TO 3000-PROCESS-CHANGE.
00555
00556      IF PI-MODE EQUAL 'A'
00557          GO TO 5000-PROCESS-ADD.
00558
00559      IF PI-MODE EQUAL 'D'
00560          GO TO 6500-PROCESS-DELETE.
00561
00562      MOVE 'LOGIC ERROR HAS OCCURRED - PROGRAM EL107'
00563                                  TO  LOGOFF-MSG.
00564      GO TO 8300-SEND-TEXT.
00565
00566      EJECT
00567 *****************************************************************
00568  1000-INITIAL-SCREEN.
00569 *****************************************************************
00570
00571      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.
00572
00573      MOVE ZERO                   TO  PI-1ST-TIME-SW
00574                                      PI-LINE-COUNT
00575                                      PI-BROWSE-SW
00576                                      PI-SHOW-SW
00577                                      PI-CHANGE-SW.
00578
00579      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
00580      MOVE '1'                    TO  WS-CFK-RECORD-TYPE.
00581
00582      
      * EXEC CICS READ
00583 *        DATASET   (WS-CONTROL-FILE-DSID)
00584 *        RIDFLD    (WS-CONTROL-FILE-KEY)
00585 *        SET       (ADDRESS OF CONTROL-FILE)
00586 *        GENERIC   EQUAL
00587 *        KEYLENGTH (4)
00588 *    END-EXEC.
           MOVE 4
             TO DFHEIV11
      *    MOVE '&"S  KG    E          (   #00004105' TO DFHEIV0
           MOVE X'26225320204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00589
00590      MOVE CF-LGX-INTERFACE-CNTL  TO  PI-LOGIC-CUSTOMER.
00591
00592      MOVE LOW-VALUES             TO  EL107AO.
00593
00594      MOVE -1      TO  AMAINTL.
00595      MOVE +2      TO  WS-COMPLETED-SUCCESSFUL.
00596      PERFORM 8100-SEND-INITIAL-MAP.
00597
00598      GO TO 9100-RETURN-TRAN.
00599
00600      EJECT
00601 *****************************************************************
00602  2000-PROCESS-SHOW.
00603 *****************************************************************
00604
00605      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.
00606
00607      MOVE PI-NEXT-BENEFIT-NUMBER TO  PI-BENEFIT-NUMBER.
00608
00609      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
00610      MOVE PI-BENEFIT-TYPE        TO  WS-CFK-RECORD-TYPE.
00611      MOVE PI-BENEFIT-NUMBER      TO  WS-CFK-BENEFIT-NO.
00612      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
00613
00614      MOVE +1                     TO  PI-SHOW-SW.
00615
00616      GO TO 8000-DISPLAY-RECORDS.
00617
00618      EJECT
00619 *****************************************************************
00620  3000-PROCESS-CHANGE.
00621 *****************************************************************
00622
00623      IF  PI-CHANGE-SW EQUAL +2
00624           GO TO 3000-PHASE-TWO.
00625
00626      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.
00627
00628      MOVE PI-NEXT-BENEFIT-NUMBER TO  PI-BENEFIT-NUMBER.
00629
00630      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
00631      MOVE PI-BENEFIT-TYPE        TO  WS-CFK-RECORD-TYPE.
00632      MOVE PI-BENEFIT-NUMBER      TO  WS-CFK-BENEFIT-NO.
00633      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
00634
00635      MOVE +1                     TO  PI-CHANGE-SW.
00636
00637      GO TO 8000-DISPLAY-RECORDS.
00638
00639 *****************************************************************
00640  3000-PHASE-TWO.
00641 *****************************************************************
00642
00643      
      * EXEC CICS READ
00644 *        UPDATE
00645 *        DATASET ('ELCNTL')
00646 *        SET (ADDRESS OF CONTROL-FILE)
00647 *        RIDFLD (PI-UPDATE-KEY)
00648 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00004166' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-UPDATE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00649
00650      IF CF-LAST-MAINT-BY NOT EQUAL PI-UPDATE-BY OR
00651         CF-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS
00652           
      * EXEC CICS UNLOCK
00653 *            DATASET ('ELCNTL')
00654 *         END-EXEC
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&*                    #   #00004175' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00655           MOVE ER-0068 TO EMI-ERROR
00656           PERFORM 8200-SEND-DATAONLY
00657           GO TO 9100-RETURN-TRAN.
00658
00659
00660      MOVE SPACES                 TO  WS-BENEFIT-TABLE-AREA.
00661      MOVE +1 TO WS-INDEX.
00662      SET BENEFIT-INDEX TO +1.
00663
00664  3020-MOVE-DATA.
00665      MOVE CF-BENEFIT-CONTROLS (WS-INDEX)
00666                                  TO  WS-BENEFIT-CONTROLS-WORK.
00667
00668      IF WS-BENEFIT-NUMBER NOT = ZEROS
00669          MOVE WS-BENEFIT-CONTROLS-WORK
00670                        TO  WS-BENEFIT-TABLE-ENTRY (BENEFIT-INDEX)
00671      ELSE
00672          GO TO 3030-CONTINUE.
00673
00674      SET BENEFIT-INDEX UP BY +1.
00675
00676      IF WS-INDEX LESS +8
00677          ADD +1  TO  WS-INDEX
00678          GO TO 3020-MOVE-DATA.
00679
00680  3030-CONTINUE.
00681
00682      SET BENEFIT-INDEX TO +1.
00683      SET EL107A-INDEX TO +1.
00684
00685      PERFORM 4000-EDIT-DATA THRU 4099-EXIT.
00686
00687      IF EMI-FATAL-CTR GREATER ZERO
00688          PERFORM 8200-SEND-DATAONLY
00689          GO TO 9100-RETURN-TRAN.
00690
00691      MOVE 'B'                    TO  JP-RECORD-TYPE.
00692      MOVE ZERO                   TO  JP-GENERIC-KEY-LENGTH.
00693      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
00694
00695      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
00696
00697      SET BENEFIT-INDEX TO +1.
00698      MOVE +1 TO WS-INDEX.
00699
00700  3000-UPDATE-LOOP.
00701
00702      IF WS-BTE-NUMBER (BENEFIT-INDEX) = CF-BENEFIT-CODE (WS-INDEX)
00703          MOVE WS-BENEFIT-TABLE-ENTRY (BENEFIT-INDEX)
00704                   TO  CF-BENEFIT-CONTROLS (WS-INDEX)
00705      ELSE
00706          IF WS-INDEX LESS THAN +8
00707              ADD +1  TO  WS-INDEX
00708              GO TO 3000-UPDATE-LOOP.
00709
00710      IF WS-INDEX LESS THAN +8     AND
00711              BENEFIT-INDEX LESS THAN +8
00712          ADD +1  TO  WS-INDEX
00713          SET BENEFIT-INDEX UP BY +1
00714          GO TO 3000-UPDATE-LOOP.
00715
00716      MOVE PI-PROCESSOR-ID        TO  CF-LAST-MAINT-BY
00717                                      PI-UPDATE-BY.
00718      MOVE EIBTIME                TO  CF-LAST-MAINT-HHMMSS
00719                                      PI-UPDATE-HHMMSS.
00720      MOVE SAVE-BIN-DATE          TO  CF-LAST-MAINT-DT.
00721
00722      MOVE 'C'                    TO  JP-RECORD-TYPE.
00723      MOVE ZERO                   TO  JP-GENERIC-KEY-LENGTH.
00724      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
00725
00726      
      * EXEC CICS REWRITE
00727 *        DATASET ('ELCNTL')
00728 *        FROM (CONTROL-FILE)
00729 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&& L                  %   #00004249' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00730
00731      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
00732
00733      MOVE ER-0000 TO EMI-ERROR.
00734
00735      PERFORM 7200-UPDATE-USER-TIME.
00736
00737      MOVE ZERO               TO  PI-1ST-TIME-SW
00738                                  PI-SHOW-SW
00739                                  PI-CHANGE-SW.
00740
00741      MOVE SPACES                 TO  PI-UPDATE-KEY.
00742      MOVE PI-LAST-BENEFIT-NUMBER TO  PI-NEXT-BENEFIT-NUMBER.
00743      MOVE +3      TO  WS-COMPLETED-SUCCESSFUL.
00744
00745      GO TO 2000-PROCESS-SHOW.
00746
00747      EJECT
00748 *****************************************************************
00749  4000-EDIT-DATA.
00750 *****************************************************************
00751
00752 *    NOTE *******************************************************
00753 *         *      CHECK TO SEE IF ANY DATA WAS ENTERED ON THIS   *
00754 *         *  LINE.                                              *
00755 *         *******************************************************
00756
00757      IF EL107A-ABBR-LENGTH  (EL107A-INDEX)   NOT GREATER ZERO AND
00758         EL107A-DESC-LENGTH  (EL107A-INDEX)   NOT GREATER ZERO AND
00759         EL107A-COMMENT-LENGTH (EL107A-INDEX) NOT GREATER ZERO AND
051414        EL107A-MAX-BENS-LENGTH (EL107A-INDEX) NOT GREATER ZERO AND
00760         EL107A-LOAN-LENGTH  (EL107A-INDEX)   NOT GREATER ZERO AND
00761         EL107A-EM-LENGTH    (EL107A-INDEX)   NOT GREATER ZERO AND
00762         EL107A-JOINT-LENGTH (EL107A-INDEX)   NOT GREATER ZERO AND
00763         EL107A-OB-LENGTH    (EL107A-INDEX)   NOT GREATER ZERO AND
00764         EL107A-LOD-LENGTH   (EL107A-INDEX)   NOT GREATER ZERO AND
00765         EL107A-RTM-LENGTH   (EL107A-INDEX)   NOT GREATER ZERO AND
00766         EL107A-RFM-LENGTH   (EL107A-INDEX)   NOT GREATER ZERO AND
082503        EL107A-IGC-LENGTH   (EL107A-INDEX)   NOT GREATER ZERO AND
082503        EL107A-CAC-LENGTH   (EL107A-INDEX)   NOT GREATER ZERO
00768          GO TO 4060-CONTINUE-EDIT.
00769
00770 *    NOTE *******************************************************
00771 *         *      CLEAR THE WORK AREA AND SAVE THE NUMBER OF     *
00772 *         *  ERRORS THAT OCCURRED BEFORE EDITING THIS LINE SO   *
00773 *         *  YOU CAN TEST FOR ERRORS IN THIS LINE.              *
00774 *         *******************************************************
00775
00776      MOVE EMI-FATAL-CTR          TO  WS-LAST-ERROR-COUNT.
00777
00778  4010-CONTINUE-EDIT.
00779
00780      IF EL107A-CODE-I (EL107A-INDEX) =
00781                              WS-BTE-NUMBER (BENEFIT-INDEX)
00782          MOVE WS-BENEFIT-TABLE-ENTRY (BENEFIT-INDEX)
00783                                  TO WS-BENEFIT-CONTROLS-WORK
00784      ELSE
00785          SET BENEFIT-INDEX UP BY +1
00786          GO TO 4010-CONTINUE-EDIT.
00787
00788 *    NOTE *******************************************************
00789 *         *              EDIT THE ABBREVIATION.                 *
00790 *         *******************************************************
00791
00792      IF PI-BENEFIT-TYPE = '4'
00793          GO TO 4020-CONTINUE-EDIT.
00794
00795      IF EL107A-ABBR-LENGTH (EL107A-INDEX) NOT GREATER ZERO
00796            GO TO 4030-CONTINUE-EDIT.
00797
00798      MOVE EL107A-ABBR-I (EL107A-INDEX)
00799                                  TO  WS-BENEFIT-ABBREVIATION.
00800
00801      INSPECT EL107A-ABBR-I (EL107A-INDEX)
00802                     CONVERTING LOW-VALUES TO SPACES.
00803
00804      IF EL107A-ABBR-I (EL107A-INDEX) = 'OB ' OR ' OB'
00805         GO TO 4030-CONTINUE-EDIT.
00806
00807      IF EL107A-ABBR-CHAR-12 (EL107A-INDEX) NUMERIC AND
00808         EL107A-ABBR-CHAR-12 (EL107A-INDEX) GREATER ZERO
00809          MOVE AL-UANON TO EL107A-ABBR-ATTRB (EL107A-INDEX)
00810        ELSE
00811          MOVE ER-0127            TO  EMI-ERROR
00812          PERFORM 9900-ERROR-FORMAT
00813          MOVE AL-UABON TO EL107A-ABBR-ATTRB  (EL107A-INDEX)
00814          MOVE -1       TO EL107A-ABBR-LENGTH (EL107A-INDEX).
00815
00816      IF EL107A-ABBR-CHAR-3 (EL107A-INDEX) = 'R' OR 'E'
00817          MOVE AL-UANON TO EL107A-ABBR-ATTRB (EL107A-INDEX)
00818        ELSE
00819          MOVE ER-0128            TO  EMI-ERROR
00820          PERFORM 9900-ERROR-FORMAT
00821          MOVE AL-UABON TO EL107A-ABBR-ATTRB  (EL107A-INDEX)
00822          MOVE -1       TO EL107A-ABBR-LENGTH (EL107A-INDEX).
00823
00824      GO TO 4030-CONTINUE-EDIT.
00825
00826  4020-CONTINUE-EDIT.
00827      IF EL107A-ABBR-LENGTH (EL107A-INDEX) GREATER ZERO
00828          MOVE EL107A-ABBR-I (EL107A-INDEX)
00829                                  TO  WS-BENEFIT-ABBREVIATION.
00830
00831      IF PI-COMPANY-ID = 'DMD' AND
00832         WS-BENEFIT-ABBREVIATION = SPACES
00833           MOVE ER-8318            TO  EMI-ERROR
00834           PERFORM 9900-ERROR-FORMAT
00835           MOVE AL-UABON TO EL107A-ABBR-ATTRB  (EL107A-INDEX)
00836           MOVE -1       TO EL107A-ABBR-LENGTH (EL107A-INDEX).
00837
00838  4030-CONTINUE-EDIT.
00839 *    NOTE *******************************************************
00840 *         *              EDIT THE DESCRIPTION.                  *
00841 *         *******************************************************
00842
00843      IF EL107A-DESC-LENGTH (EL107A-INDEX) GREATER ZERO
00844          MOVE EL107A-DESC-I (EL107A-INDEX)
00845                                  TO  WS-BENEFIT-DESCRIPTION.
00846
00847 *    NOTE *******************************************************
00848 *         *              EDIT THE COMMENT.                      *
00849 *         *******************************************************
00850
00851      IF EL107A-COMMENT-LENGTH (EL107A-INDEX) GREATER ZERO
00852          MOVE EL107A-COMMENT-I (EL107A-INDEX)
00853                                  TO  WS-BENEFIT-COMMENT.
00854
00855 *    NOTE *******************************************************
00856 *         *              EDIT THE ABBREVIATION.                 *
00857 *         *******************************************************
00858
00859      IF EL107A-LOAN-LENGTH (EL107A-INDEX) GREATER ZERO
00860          MOVE EL107A-LOAN-I (EL107A-INDEX)
00861                                  TO  WS-BENEFIT-LOAN-TYPE.
051414***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_-***
051414***                                                            ***
051414***   EDIT THE MAXIMUM BENEFITS/CRITICAL PERIOD                ***
051414***                                                            ***
051414***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_-***
051414
051414     IF EL107A-MAX-BENS-LENGTH (EL107A-INDEX) <> ZEROS
051414        IF EL107A-MAX-BENS-I (EL107A-INDEX) NUMERIC
051414           MOVE EL107A-MAX-BENS-O (EL107A-INDEX)
051414                                 TO WS-BENEFIT-MAX-BENS
051414        END-IF
051414     END-IF
00863 *    NOTE *******************************************************
00864 *         *              EDIT EARNINGS METHOD.                  *
00865 *         *******************************************************
00866
00867      IF EL107A-EM-LENGTH (EL107A-INDEX) GREATER ZERO
00868           IF ((EL107A-EM-I (EL107A-INDEX) NUMERIC)
00869               OR (EL107A-EM-I (EL107A-INDEX) = 'B')) AND
00870              EL107A-EM-I (EL107A-INDEX) NOT = ZERO
00871              MOVE EL107A-EM-I (EL107A-INDEX)
00872                                  TO  WS-BENEFIT-EARN-METHOD
00873              MOVE AL-UNNON  TO  EL107A-EM-ATTRB (EL107A-INDEX)
00874            ELSE
00875              MOVE AL-UNBON  TO  EL107A-EM-ATTRB (EL107A-INDEX)
00876              MOVE -1        TO  EL107A-EM-LENGTH (EL107A-INDEX)
00877              MOVE ER-0036      TO  EMI-ERROR
00878              PERFORM 9900-ERROR-FORMAT.
00879
00880 *    NOTE *******************************************************
00881 *         *          EDIT THE JOINT COVERAGE INDICATOR.         *
00882 *         *******************************************************
00883
00884      IF PI-BENEFIT-TYPE NOT = '4' AND '5'
00885          NEXT SENTENCE
00886      ELSE
00887          IF EL107A-JOINT-LENGTH (EL107A-INDEX) GREATER ZERO
00888              IF EL107A-JOINT-I (EL107A-INDEX) = SPACES OR 'J'
00889                  MOVE EL107A-JOINT-I (EL107A-INDEX)
00890                             TO  WS-BENEFIT-JOINT-COVERAGE
00891                  MOVE AL-UANON
00892                             TO  EL107A-JOINT-ATTRB (EL107A-INDEX)
00893              ELSE
00894                  MOVE AL-UABON
00895                             TO  EL107A-JOINT-ATTRB (EL107A-INDEX)
00896                  MOVE -1    TO  EL107A-JOINT-LENGTH (EL107A-INDEX)
00897                  MOVE ER-0037  TO  EMI-ERROR
00898                  PERFORM 9900-ERROR-FORMAT.
00899
00900
00901 *    NOTE *******************************************************
00902 *         *      EDIT THE OUTSTANDING BALANCE INDICATOR.        *
00903 *         *******************************************************
00904
00905      IF EL107A-OB-LENGTH (EL107A-INDEX) GREATER ZERO
00906        IF AKINDI = PI-LIFE-OVERRIDE-L1
00907          MOVE EL107A-OB-I (EL107A-INDEX) TO WS-EDIT-LF-OB
00908          IF VALID-LF-OB
00909            MOVE EL107A-OB-I (EL107A-INDEX)
00910                              TO  WS-BENEFIT-OUTSTANDING-BAL
00911            MOVE AL-UANON     TO  EL107A-OB-ATTRB (EL107A-INDEX)
00912          ELSE
00913            MOVE AL-UABON     TO  EL107A-OB-ATTRB (EL107A-INDEX)
00914            MOVE -1           TO  EL107A-OB-LENGTH (EL107A-INDEX)
00915            MOVE ER-0038      TO  EMI-ERROR
00916            PERFORM 9900-ERROR-FORMAT.
00917
00918      IF EL107A-OB-LENGTH (EL107A-INDEX) GREATER ZERO
00919        IF AKINDI = PI-AH-OVERRIDE-L1
00920          MOVE EL107A-OB-I (EL107A-INDEX) TO WS-EDIT-AH-OB
00921          IF VALID-AH-OB
00922            MOVE EL107A-OB-I (EL107A-INDEX)
00923                              TO  WS-BENEFIT-OUTSTANDING-BAL
00924            MOVE AL-UANON     TO  EL107A-OB-ATTRB (EL107A-INDEX)
00925          ELSE
00926            MOVE AL-UABON     TO  EL107A-OB-ATTRB (EL107A-INDEX)
00927            MOVE -1           TO  EL107A-OB-LENGTH (EL107A-INDEX)
00928            MOVE ER-0847      TO  EMI-ERROR
00929            PERFORM 9900-ERROR-FORMAT.
00930
051414***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_-***
051414***                                                            ***
051414***   Edit the maximum benefits against the special calc code  ***
051414***   If the maxben <> 0 then the spec calc s/b = 'C'          ***
051414***                                                            ***
051414***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_-***
051414
051414     if ws-benefit-max-bens not numeric
051414        move zeros               to ws-benefit-max-bens
051414     end-if
051414     if pi-benefit-type = '5'
051414        if ((ws-benefit-max-bens > zeros)
051414           and (ws-benefit-outstanding-bal <> 'C'))
051414                          or
051414           ((ws-benefit-max-bens = zeros)
051414           and (ws-benefit-outstanding-bal = 'C'))
051414           MOVE AL-UABON        TO EL107A-OB-ATTRB (EL107A-INDEX)
051414           MOVE -1              TO EL107A-OB-LENGTH (EL107A-INDEX)
051414           MOVE ER-7537         TO EMI-ERROR
051414           PERFORM 9900-ERROR-FORMAT
051414        end-if
051414     end-if
00931 *    NOTE *******************************************************
00932 *         IF FARM PLAN (F) SPECIAL CALC CODE IS ENTERED,
00933 *            EARNING METHOD MUST BE 4 - (TEXAS).
00934 *         *******************************************************
00935
00936      IF  WS-BENEFIT-OUTSTANDING-BAL = 'F'    AND
00937          WS-BENEFIT-EARN-METHOD NOT = '4'
00938              MOVE SPACE     TO  WS-BENEFIT-OUTSTANDING-BAL
00939              MOVE AL-UABON  TO  EL107A-OB-ATTRB (EL107A-INDEX)
00940              MOVE -1        TO  EL107A-OB-LENGTH (EL107A-INDEX)
00941              MOVE ER-0640   TO  EMI-ERROR
00942              PERFORM 9900-ERROR-FORMAT.
00943
00944 *    NOTE *******************************************************
00945 *         *    EDIT THE LEVEL OR DECREASING COVERAGE METHOD     *
00946 *         *******************************************************
00947
00948      IF PI-BENEFIT-TYPE NOT = '4'
00949          NEXT SENTENCE
00950      ELSE
00951          IF EL107A-LOD-LENGTH (EL107A-INDEX) GREATER ZERO
00952              IF EL107A-LOD-I (EL107A-INDEX) = 'L' OR 'R' OR 'P'
00953                  MOVE EL107A-LOD-I (EL107A-INDEX)
00954                                TO  WS-BENEFIT-COVERAGE-TYPE
00955                  MOVE AL-UANON TO  EL107A-LOD-ATTRB (EL107A-INDEX)
00956              ELSE
00957                  MOVE AL-UABON TO  EL107A-LOD-ATTRB (EL107A-INDEX)
00958                  MOVE -1       TO EL107A-LOD-LENGTH (EL107A-INDEX)
00959                  MOVE ER-0039  TO  EMI-ERROR
00960                  PERFORM 9900-ERROR-FORMAT.
00961
00962 *    NOTE *******************************************************
00963 *         *          EDIT THE REMAINING TERM METHOD.            *
00964 *         *******************************************************
00965
00966      IF EL107A-RTM-LENGTH (EL107A-INDEX) GREATER ZERO
00967          IF EL107A-RTM-I (EL107A-INDEX) GREATER ZERO AND LESS '8'
00968          OR EL107A-RTM-I (EL107A-INDEX) = SPACES
00969              MOVE EL107A-RTM-I (EL107A-INDEX)
00970                                  TO  WS-BENEFIT-REMAIN-TERM
00971              MOVE AL-UNNON  TO  EL107A-RTM-ATTRB (EL107A-INDEX)
00972            ELSE
00973              MOVE AL-UNBON  TO  EL107A-RTM-ATTRB (EL107A-INDEX)
00974              MOVE -1        TO  EL107A-RTM-LENGTH (EL107A-INDEX)
00975              MOVE ER-0040   TO  EMI-ERROR
00976              PERFORM 9900-ERROR-FORMAT.
00977
00978  4040-CONTINUE-EDIT.
00979 *    NOTE *******************************************************
00980 *         *          EDIT THE REFUND METHOD.                    *
00981 *         *******************************************************
00982
00983      IF EL107A-RFM-LENGTH (EL107A-INDEX) GREATER ZERO
00984          MOVE EL107A-RFM-I (EL107A-INDEX) TO WS-EDIT-REFUND
00985          IF VALID-REFUND-METHOD
00986              MOVE EL107A-RFM-I (EL107A-INDEX)
00987                                  TO  WS-BENEFIT-REFUND-METHOD
00988              MOVE AL-UANON  TO  EL107A-RFM-ATTRB (EL107A-INDEX)
00989            ELSE
00990              MOVE AL-UABON  TO  EL107A-RFM-ATTRB (EL107A-INDEX)
00991              MOVE -1        TO  EL107A-RFM-LENGTH (EL107A-INDEX)
00992              MOVE ER-0582   TO  EMI-ERROR
00993              PERFORM 9900-ERROR-FORMAT.
00994
00995 *    NOTE *******************************************************
00996 *         *          EDIT THE INDIVIDUAL/GROUP CODE             *
00997 *         *******************************************************
00998
00999      IF EL107A-IGC-LENGTH (EL107A-INDEX) GREATER ZERO
01000          MOVE EL107A-IGC-I (EL107A-INDEX) TO WS-EDIT-IG-CODE
01001          IF VALID-IG-CODE
01002              MOVE EL107A-IGC-I (EL107A-INDEX)
01003                                  TO  WS-BENEFIT-IG-CODE
01004              MOVE AL-UANON  TO  EL107A-IGC-ATTRB (EL107A-INDEX)
01005            ELSE
01006              MOVE AL-UABON  TO  EL107A-IGC-ATTRB (EL107A-INDEX)
01007              MOVE -1        TO  EL107A-IGC-LENGTH (EL107A-INDEX)
01008              MOVE ER-0596   TO  EMI-ERROR
01009              PERFORM 9900-ERROR-FORMAT.
01010
082503*    NOTE *******************************************************
082503*         *          EDIT THE BENEFIT CATEGORY                  *
082503*         *******************************************************
082503
082503     IF EL107A-CAC-LENGTH (EL107A-INDEX) > ZERO
082503        MOVE EL107A-CAC-I (EL107A-INDEX)
082503                                 TO WS-BENEFIT-CATEGORY
082503        MOVE AL-UANON
082503                        TO EL107A-CAC-ATTRB (EL107A-INDEX)
082503     END-IF
01010
01011      IF EMI-FATAL-CTR GREATER WS-LAST-ERROR-COUNT
01012          GO TO 4060-CONTINUE-EDIT.
01013
01014  4050-CONTINUE-EDIT.
01015      MOVE +1                     TO  WS-UPDATE-SW.
01016
01017      MOVE WS-BENEFIT-CONTROLS-WORK
01018                  TO  WS-BENEFIT-TABLE-ENTRY (BENEFIT-INDEX).
01019
01020  4060-CONTINUE-EDIT.
01021 *    NOTE *******************************************************
01022 *         *      AFTER ALL OF THE MAP LINES HAVE BEEN PROCESSED *
01023 *         *  CHECK TO SEE IF ANY ERRORS OCCURRED DURING EDITING. *
01024 *         *******************************************************
01025
01026      IF EL107A-INDEX LESS PI-LINE-COUNT
01027          SET EL107A-INDEX UP BY +1
01028          GO TO 4000-EDIT-DATA.
01029
01030  4099-EXIT.
01031      EXIT.
01032
01033      EJECT
01034 *****************************************************************
01035  5000-PROCESS-ADD.
01036 *****************************************************************
01037
01038      SET EL107A-INDEX TO PI-LINE-COUNT.
01039      SET EL107A-INDEX UP BY +1.
01040
01041      PERFORM 7000-LOAD-BENEFIT-RECORDS.
01042
01043      PERFORM 6000-EDIT-DATA THRU 6099-EXIT.
01044
01045      IF EMI-FATAL-CTR GREATER ZERO
01046          PERFORM 8200-SEND-DATAONLY
01047          GO TO 9100-RETURN-TRAN.
01048
01049      PERFORM 7100-UPDATE-BENEFIT-TABLE.
01050
01051      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
01052      MOVE LOW-VALUES             TO  EL107AO.
01053      PERFORM 8100-SEND-INITIAL-MAP.
01054      MOVE ZERO                   TO  PI-1ST-TIME-SW.
01055
01056      MOVE SPACES                 TO  PI-MODE
01057                                      PI-BENEFIT-TYPE
01058                                      PI-BENEFIT-NUMBER
01059                                      PI-NEXT-BENEFIT-NUMBER.
01060
01061      GO TO 9100-RETURN-TRAN.
01062
01063      EJECT
01064 *****************************************************************
01065  6000-EDIT-DATA.
01066 *****************************************************************
01067
01068 *    NOTE *******************************************************
01069 *         *      CHECK TO SEE IF ANY DATA WAS ENTERED ON THIS   *
01070 *         *  LINE.                                              *
01071 *         *******************************************************
01072
01073      IF EL107A-CODE-LENGTH    (EL107A-INDEX) GREATER ZERO    OR
01074         EL107A-ABBR-LENGTH    (EL107A-INDEX) GREATER ZERO    OR
01075         EL107A-DESC-LENGTH    (EL107A-INDEX) GREATER ZERO    OR
01076         EL107A-COMMENT-LENGTH (EL107A-INDEX) GREATER ZERO    OR
051414        EL107A-MAX-BENS-LENGTH (EL107A-INDEX) GREATER ZERO   OR
01077         EL107A-LOAN-LENGTH    (EL107A-INDEX) GREATER ZERO    OR
01078         EL107A-EM-LENGTH      (EL107A-INDEX) GREATER ZERO    OR
01079         EL107A-JOINT-LENGTH   (EL107A-INDEX) GREATER ZERO    OR
01080         EL107A-OB-LENGTH      (EL107A-INDEX) GREATER ZERO    OR
01081         EL107A-LOD-LENGTH     (EL107A-INDEX) GREATER ZERO    OR
01082         EL107A-RTM-LENGTH     (EL107A-INDEX) GREATER ZERO    OR
01083         EL107A-RFM-LENGTH     (EL107A-INDEX) GREATER ZERO    OR
082503        EL107A-IGC-LENGTH     (EL107A-INDEX) GREATER ZERO    OR
082503        EL107A-CAC-LENGTH     (EL107A-INDEX) GREATER ZERO
01085          NEXT SENTENCE
01086        ELSE
01087          GO TO 6050-CONTINUE-EDIT.
01088
01089 *    NOTE *******************************************************
01090 *         *      CLEAR THE WORK AREA AND SAVE THE NUMBER OF     *
01091 *         *  ERRORS THAT OCCURRED BEFORE EDITING THIS LINE SO   *
01092 *         *  YOU CAN TEST FOR ERRORS IN THIS LINE.              *
01093 *         *******************************************************
01094
01095      MOVE SPACES                 TO  WS-BENEFIT-CONTROLS-WORK.
01096
01097      MOVE EMI-FATAL-CTR          TO  WS-LAST-ERROR-COUNT.
01098
01099 *    NOTE *******************************************************
01100 *         *              EDIT THE BENEFIT CODE.                 *
01101 *         *******************************************************
01102
01103      IF EL107A-CODE-LENGTH (EL107A-INDEX) GREATER ZERO
01104          MOVE EL107A-CODE-I (EL107A-INDEX) TO WS-EDIT-BENE-CODE
01105          IF NOT INVALID-BENEFIT-CODE
01106              MOVE EL107A-CODE-I (EL107A-INDEX)
01107                                  TO  WS-BENEFIT-NUMBER
01108            ELSE
01109              MOVE ER-0025        TO  EMI-ERROR
01110              PERFORM 9900-ERROR-FORMAT
01111              MOVE AL-UABOF  TO  EL107A-CODE-ATTRB (EL107A-INDEX)
01112              MOVE -1  TO  EL107A-CODE-LENGTH (EL107A-INDEX)
01113        ELSE
01114          MOVE ER-0026            TO  EMI-ERROR
01115          PERFORM 9900-ERROR-FORMAT
01116          MOVE AL-UABOF  TO  EL107A-CODE-ATTRB (EL107A-INDEX)
01117          MOVE -1        TO  EL107A-CODE-LENGTH (EL107A-INDEX).
01118
01119 *    NOTE *******************************************************
01120 *         *              EDIT THE ABBREVIATION.                 *
01121 *         *******************************************************
01122
01123      IF PI-BENEFIT-TYPE = '4'
01124          GO TO 6010-CONTINUE-EDIT.
01125
01126      IF EL107A-ABBR-LENGTH (EL107A-INDEX) NOT GREATER ZERO
01127          MOVE ER-0127            TO  EMI-ERROR
01128          PERFORM 9900-ERROR-FORMAT
01129          MOVE ER-0128            TO  EMI-ERROR
01130          PERFORM 9900-ERROR-FORMAT
01131          MOVE ER-0129            TO  EMI-ERROR
01132          PERFORM 9900-ERROR-FORMAT
01133          MOVE AL-UABON TO EL107A-ABBR-ATTRB (EL107A-INDEX)
01134          MOVE -1       TO EL107A-ABBR-LENGTH (EL107A-INDEX)
01135          GO TO 6020-CONTINUE-EDIT.
01136
01137      MOVE EL107A-ABBR-I (EL107A-INDEX)
01138                                  TO  WS-BENEFIT-ABBREVIATION.
01139
01140      IF EL107A-ABBR-CHAR-12 (EL107A-INDEX) NUMERIC AND
01141         EL107A-ABBR-CHAR-12 (EL107A-INDEX) GREATER ZERO
01142          MOVE AL-UANON TO EL107A-ABBR-ATTRB (EL107A-INDEX)
01143        ELSE
01144          MOVE ER-0127            TO  EMI-ERROR
01145          PERFORM 9900-ERROR-FORMAT
01146          MOVE AL-UABON TO EL107A-ABBR-ATTRB (EL107A-INDEX)
01147          MOVE -1       TO EL107A-ABBR-LENGTH (EL107A-INDEX).
01148
01149      IF EL107A-ABBR-CHAR-3 (EL107A-INDEX) = 'R' OR 'E'
01150          MOVE AL-UANON TO EL107A-ABBR-ATTRB (EL107A-INDEX)
01151        ELSE
01152          MOVE ER-0128            TO  EMI-ERROR
01153          PERFORM 9900-ERROR-FORMAT
01154          MOVE AL-UABON TO EL107A-ABBR-ATTRB (EL107A-INDEX)
01155          MOVE -1       TO EL107A-ABBR-LENGTH (EL107A-INDEX).
01156
01157      GO TO 6020-CONTINUE-EDIT.
01158
01159  6010-CONTINUE-EDIT.
01160      IF EL107A-ABBR-LENGTH (EL107A-INDEX) GREATER ZERO
01161          MOVE EL107A-ABBR-I (EL107A-INDEX)
01162                                  TO  WS-BENEFIT-ABBREVIATION
01163        ELSE
01164          IF PI-COMPANY-ID = 'DMD'
01165            MOVE ER-8318            TO  EMI-ERROR
01166            PERFORM 9900-ERROR-FORMAT
01167            MOVE AL-UABON TO EL107A-ABBR-ATTRB  (EL107A-INDEX)
01168            MOVE -1       TO EL107A-ABBR-LENGTH (EL107A-INDEX).
01169
01170      IF PI-COMPANY-ID = 'DMD' AND
01171         WS-BENEFIT-ABBREVIATION = SPACES
01172           MOVE ER-8318            TO  EMI-ERROR
01173           PERFORM 9900-ERROR-FORMAT
01174           MOVE AL-UABON TO EL107A-ABBR-ATTRB  (EL107A-INDEX)
01175           MOVE -1       TO EL107A-ABBR-LENGTH (EL107A-INDEX).
01176
01177  6020-CONTINUE-EDIT.
01178 *    NOTE *******************************************************
01179 *         *              EDIT THE DESCRIPTION.                  *
01180 *         *******************************************************
01181
01182      IF EL107A-DESC-LENGTH (EL107A-INDEX) GREATER ZERO
01183          MOVE EL107A-DESC-I (EL107A-INDEX)
01184                                  TO  WS-BENEFIT-DESCRIPTION.
01185
01186 *    NOTE *******************************************************
01187 *         *              EDIT THE COMMENT.                      *
01188 *         *******************************************************
01189
01190      IF EL107A-COMMENT-LENGTH (EL107A-INDEX) GREATER ZERO
01191          MOVE EL107A-COMMENT-I (EL107A-INDEX)
01192                                  TO  WS-BENEFIT-COMMENT.
01193
01194 *    NOTE *******************************************************
01195 *         *              EDIT THE LOAN TYPE.                    *
01196 *         *******************************************************
01197
01198      IF EL107A-LOAN-LENGTH (EL107A-INDEX) GREATER ZERO
01199          MOVE EL107A-LOAN-I (EL107A-INDEX)
01200                                  TO  WS-BENEFIT-LOAN-TYPE.
01201
051414***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_-***
051414***                                                            ***
051414***   EDIT THE MAXIMUM BENEFITS/CRITICAL PERIOD                ***
051414***                                                            ***
051414***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_-***
051414
081214     move zeros to ws-benefit-max-bens
051414     IF PI-BENEFIT-TYPE = '5'
051414        IF EL107A-MAX-BENS-LENGTH (EL107A-INDEX) <> ZEROS
051414           IF EL107A-MAX-BENS-I (EL107A-INDEX) NUMERIC
051414              MOVE EL107A-MAX-BENS-O (EL107A-INDEX)
051414                                 TO WS-BENEFIT-MAX-BENS
051414              MOVE AL-UANON      TO EL107A-MAX-BENS-ATTRB
051414                                                (EL107A-INDEX)
051414           ELSE
051414              MOVE AL-UNBON      TO EL107A-MAX-BENS-ATTRB
051414                                                (EL107A-INDEX)
051414              MOVE -1            TO EL107A-MAX-BENS-LENGTH
051414                                                (EL107A-INDEX)
051414              MOVE 9999          TO EMI-ERROR
051414              PERFORM 9900-ERROR-FORMAT
051414           END-IF
051414        END-IF
051414     END-IF
01202 *    NOTE *******************************************************
01203 *         *              EDIT THE EARNINGS METHOD.              *
01204 *         *                   (REQUIRED FIELD)                  *
01205 *         *******************************************************
01206
01207      IF EL107A-EM-LENGTH (EL107A-INDEX) GREATER ZERO
01208         IF ((EL107A-EM-I (EL107A-INDEX) NUMERIC)
01209              OR (EL107A-EM-I (EL107A-INDEX) = 'B')) AND
01210              EL107A-EM-I (EL107A-INDEX) NOT = ZERO
01211              MOVE EL107A-EM-I (EL107A-INDEX)
01212                             TO  WS-BENEFIT-EARN-METHOD
01213              MOVE AL-UNNON  TO  EL107A-EM-ATTRB (EL107A-INDEX)
01214          ELSE
01215              MOVE AL-UNBON  TO  EL107A-EM-ATTRB (EL107A-INDEX)
01216              MOVE -1        TO  EL107A-EM-LENGTH (EL107A-INDEX)
01217              MOVE ER-0036   TO  EMI-ERROR
01218              PERFORM 9900-ERROR-FORMAT
01219        ELSE
01220            MOVE AL-UNBOF    TO  EL107A-EM-ATTRB (EL107A-INDEX)
01221            MOVE -1          TO  EL107A-EM-LENGTH (EL107A-INDEX)
01222            MOVE ER-0036     TO  EMI-ERROR
01223            PERFORM 9900-ERROR-FORMAT.
01224
01225 *    NOTE *******************************************************
01226 *         *          EDIT THE JOINT COVERAGE INDICATOR.         *
01227 *         *******************************************************
01228
01229      IF PI-BENEFIT-TYPE NOT = '4' AND '5'
01230          NEXT SENTENCE
01231      ELSE
01232          IF EL107A-JOINT-LENGTH (EL107A-INDEX) GREATER ZERO
01233              IF EL107A-JOINT-I (EL107A-INDEX) = SPACES OR 'J'
01234                  MOVE EL107A-JOINT-I (EL107A-INDEX)
01235                             TO  WS-BENEFIT-JOINT-COVERAGE
01236                  MOVE AL-UANON
01237                             TO  EL107A-JOINT-ATTRB (EL107A-INDEX)
01238              ELSE
01239                  MOVE AL-UABON
01240                             TO  EL107A-JOINT-ATTRB (EL107A-INDEX)
01241                  MOVE -1    TO  EL107A-JOINT-LENGTH (EL107A-INDEX)
01242                  MOVE ER-0037  TO  EMI-ERROR
01243                  PERFORM 9900-ERROR-FORMAT.
01244
01245 *    NOTE *******************************************************
01246 *         *        EDIT THE OUTSTANDING BALANCE INDICATOR.      *
01247 *         *******************************************************
01248
01249      IF EL107A-OB-LENGTH (EL107A-INDEX) GREATER ZERO
01250        IF AKINDI = PI-LIFE-OVERRIDE-L1
01251          MOVE EL107A-OB-I (EL107A-INDEX) TO  WS-EDIT-LF-OB
01252          IF VALID-LF-OB
01253            MOVE EL107A-OB-I (EL107A-INDEX)
01254                               TO  WS-BENEFIT-OUTSTANDING-BAL
01255            MOVE AL-UANON      TO  EL107A-OB-ATTRB (EL107A-INDEX)
01256          ELSE
01257            MOVE AL-UABON      TO  EL107A-OB-ATTRB (EL107A-INDEX)
01258            MOVE -1            TO  EL107A-OB-LENGTH (EL107A-INDEX)
01259            MOVE ER-0038       TO  EMI-ERROR
01260            PERFORM 9900-ERROR-FORMAT.
01261
01262      IF EL107A-OB-LENGTH (EL107A-INDEX) GREATER ZERO
01263        IF AKINDI = PI-AH-OVERRIDE-L1
01264          MOVE EL107A-OB-I (EL107A-INDEX) TO  WS-EDIT-AH-OB
01265          IF VALID-AH-OB
01266            MOVE EL107A-OB-I (EL107A-INDEX)
01267                               TO  WS-BENEFIT-OUTSTANDING-BAL
01268            MOVE AL-UANON      TO  EL107A-OB-ATTRB (EL107A-INDEX)
01269          ELSE
01270            MOVE AL-UABON      TO  EL107A-OB-ATTRB (EL107A-INDEX)
01271            MOVE -1            TO  EL107A-OB-LENGTH (EL107A-INDEX)
01272            MOVE ER-0847       TO  EMI-ERROR
01273            PERFORM 9900-ERROR-FORMAT.
01274
01275 *    NOTE *******************************************************
01276 *         *    EDIT THE LEVEL OR DECREASING COVERAGE METHOD     *
01277 *         *******************************************************
01278
01279      IF PI-BENEFIT-TYPE NOT = '4'
01280          NEXT SENTENCE
01281      ELSE
01282          IF EL107A-LOD-LENGTH (EL107A-INDEX) GREATER ZERO
01283              IF EL107A-LOD-I (EL107A-INDEX) = 'L' OR 'R' OR 'P'
01284                  MOVE EL107A-LOD-I (EL107A-INDEX)
01285                                TO  WS-BENEFIT-COVERAGE-TYPE
01286                  MOVE AL-UANON TO  EL107A-LOD-ATTRB (EL107A-INDEX)
01287              ELSE
01288                  MOVE AL-UABON TO  EL107A-LOD-ATTRB (EL107A-INDEX)
01289                  MOVE -1       TO EL107A-LOD-LENGTH (EL107A-INDEX)
01290                  MOVE ER-0039  TO  EMI-ERROR
01291                  PERFORM 9900-ERROR-FORMAT.
01292
01293 *    NOTE *******************************************************
01294 *         *            EDIT THE REMAINING TERM METHOD.          *
01295 *         *                  (REQUIRED FIELD)                   *
01296 *         *******************************************************
01297
01298         IF EL107A-RTM-LENGTH (EL107A-INDEX) GREATER ZERO
01299             IF EL107A-RTM-I (EL107A-INDEX) GREATER ZERO AND
01300                                            LESS '8'
01301                OR EL107A-RTM-I (EL107A-INDEX) = SPACES
01302                  MOVE EL107A-RTM-I (EL107A-INDEX)
01303                                  TO  WS-BENEFIT-REMAIN-TERM
01304                  MOVE AL-UNNON TO EL107A-RTM-ATTRB (EL107A-INDEX)
01305                ELSE
01306                  MOVE AL-UNBON TO EL107A-RTM-ATTRB (EL107A-INDEX)
01307                  MOVE -1       TO EL107A-RTM-LENGTH (EL107A-INDEX)
01308                  MOVE ER-0040  TO  EMI-ERROR
01309                  PERFORM 9900-ERROR-FORMAT.
01310
01311 *    NOTE *******************************************************
01312 *         *          EDIT THE REFUND METHOD.                    *
01313 *         *******************************************************
01314
01315      IF EL107A-RFM-LENGTH (EL107A-INDEX) GREATER ZERO
01316          MOVE EL107A-RFM-I (EL107A-INDEX) TO WS-EDIT-REFUND
01317          IF VALID-REFUND-METHOD
01318              MOVE EL107A-RFM-I (EL107A-INDEX)
01319                                  TO  WS-BENEFIT-REFUND-METHOD
01320              MOVE AL-UNNON  TO  EL107A-RFM-ATTRB (EL107A-INDEX)
01321          ELSE
01322              MOVE AL-UNBON  TO  EL107A-RFM-ATTRB (EL107A-INDEX)
01323              MOVE -1        TO  EL107A-RFM-LENGTH (EL107A-INDEX)
01324              MOVE ER-0582   TO  EMI-ERROR
01325              PERFORM 9900-ERROR-FORMAT.
01326
01327 *    NOTE *******************************************************
01328 *         *          EDIT THE INDIVIDUAL/GROUP CODE             *
01329 *         *******************************************************
01330
01331      IF EL107A-IGC-LENGTH (EL107A-INDEX) GREATER ZERO
01332          MOVE EL107A-IGC-I (EL107A-INDEX) TO WS-EDIT-IG-CODE
01333          IF VALID-IG-CODE
01334              MOVE EL107A-IGC-I (EL107A-INDEX)
01335                                  TO  WS-BENEFIT-IG-CODE
01336              MOVE AL-UANON  TO  EL107A-IGC-ATTRB (EL107A-INDEX)
01337            ELSE
01338              MOVE AL-UABON  TO  EL107A-IGC-ATTRB (EL107A-INDEX)
01339              MOVE -1        TO  EL107A-IGC-LENGTH (EL107A-INDEX)
01340              MOVE ER-0596   TO  EMI-ERROR
01341              PERFORM 9900-ERROR-FORMAT.
01342
01327 *    NOTE *******************************************************
01328 *         *          EDIT THE BENEFIT GROUP                     *
01329 *         *******************************************************
01330
082503     IF EL107A-CAC-LENGTH (EL107A-INDEX) > ZERO
082503        MOVE EL107A-CAC-I (EL107A-INDEX)
082503                                 TO  WS-BENEFIT-CATEGORY
082503        MOVE AL-UANON  TO  EL107A-CAC-ATTRB (EL107A-INDEX)
082503     END-IF
01342
01343      IF EMI-FATAL-CTR GREATER WS-LAST-ERROR-COUNT
01344          GO TO 6050-CONTINUE-EDIT.
01345
082503*    IF WS-BTE-NUMBER (200) NOT = SPACES
082503     IF WS-BTE-NUMBER (450) NOT = SPACES
01347          MOVE ER-0592         TO  EMI-ERROR
01348          PERFORM 9900-ERROR-FORMAT
01349          MOVE AL-UABOF  TO  EL107A-CODE-ATTRB (EL107A-INDEX)
01350          MOVE -1        TO  EL107A-CODE-LENGTH (EL107A-INDEX)
01351          GO TO 6050-CONTINUE-EDIT.
01352
01353      SET BENEFIT-INDEX TO +1.
01354
01355  6030-CONTINUE-EDIT.
01356
01357      IF WS-BTE-NUMBER (BENEFIT-INDEX) = SPACES
01358          GO TO 6040-CONTINUE-EDIT.
01359
01360      IF BENEFIT-INDEX > WS-MAX-BEN-CODES
01361          MOVE ER-0752         TO  EMI-ERROR
01362          PERFORM 9900-ERROR-FORMAT
01363          MOVE AL-UABOF  TO  EL107A-CODE-ATTRB (EL107A-INDEX)
01364          MOVE -1        TO  EL107A-CODE-LENGTH (EL107A-INDEX)
01365          GO TO 6099-EXIT.
01366
01367      IF WS-BENEFIT-NUMBER  GREATER  WS-BTE-NUMBER (BENEFIT-INDEX)
01368          SET BENEFIT-INDEX UP BY +1
01369          GO TO 6030-CONTINUE-EDIT.
01370
01371      IF WS-BENEFIT-NUMBER =  WS-BTE-NUMBER (BENEFIT-INDEX)
01372          MOVE ER-0027         TO  EMI-ERROR
01373          PERFORM 9900-ERROR-FORMAT
01374          MOVE AL-UABOF  TO  EL107A-CODE-ATTRB (EL107A-INDEX)
01375          MOVE -1        TO  EL107A-CODE-LENGTH (EL107A-INDEX)
01376          GO TO 6050-CONTINUE-EDIT.
01377
01378  6040-CONTINUE-EDIT.
01379
01380      IF WS-BTE-NUMBER (BENEFIT-INDEX) = SPACES
01381          MOVE WS-BENEFIT-CONTROLS-WORK
01382                      TO  WS-BENEFIT-TABLE-ENTRY (BENEFIT-INDEX)
01383          MOVE +1                 TO  WS-UPDATE-SW
01384          GO TO 6050-CONTINUE-EDIT.
01385
01386      MOVE WS-BENEFIT-TABLE-ENTRY (BENEFIT-INDEX)
01387                                  TO WS-BENEFIT-CONTROLS-SAVE.
01388      MOVE WS-BENEFIT-CONTROLS-WORK
01389                      TO  WS-BENEFIT-TABLE-ENTRY (BENEFIT-INDEX).
01390      MOVE WS-BENEFIT-CONTROLS-SAVE TO WS-BENEFIT-CONTROLS-WORK.
01391
01392      SET BENEFIT-INDEX UP BY +1.
01393
01394      GO TO 6040-CONTINUE-EDIT.
01395
01396  6050-CONTINUE-EDIT.
01397
01398 *    NOTE *******************************************************
01399 *         *      AFTER ALL OF THE MAP LINES HAVE BEEN PROCESSED *
01400 *         *  CHECK TO SEE IF ANY ERRORS OCCURRED DURING EDITING. *
01401 *         *******************************************************
01402
01403      IF EL107A-INDEX LESS +8
01404          SET EL107A-INDEX UP BY +1
01405          GO TO 6000-EDIT-DATA.
01406
01407  6099-EXIT.
01408      EXIT.
01409
01410      EJECT
01411 *****************************************************************
01412  6500-PROCESS-DELETE.
01413 *****************************************************************
01414
01415      IF PI-SHOW-SW EQUAL ZERO
01416          MOVE ER-0138            TO  EMI-ERROR
01417          MOVE -1                 TO  AMAINTL
01418          PERFORM 8200-SEND-DATAONLY
01419          GO TO 9100-RETURN-TRAN.
01420
01421      IF PI-SHOW-SW = +2
01422          MOVE ER-0139            TO  EMI-ERROR
01423          MOVE -1                 TO  AMAINTL
01424          PERFORM 8200-SEND-DATAONLY
01425          GO TO 9100-RETURN-TRAN.
01426
01427      PERFORM 7000-LOAD-BENEFIT-RECORDS.
01428
01429      SET BENEFIT-INDEX TO +1.
01430
01431  6510-MAIN-LOGIC.
01432
01433      IF PI-BENEFIT-NUMBER = WS-BTE-NUMBER (BENEFIT-INDEX)
01434          GO TO 6520-MAIN-LOGIC.
01435
01436      IF WS-BTE-NUMBER (BENEFIT-INDEX) = SPACES  OR
01437         BENEFIT-INDEX = WS-MAX-BEN-CODES
01438          MOVE ER-0028            TO  EMI-ERROR
01439          MOVE AL-UABOF           TO  ABENEA
01440          MOVE -1                 TO  ABENEL
01441          PERFORM 8200-SEND-DATAONLY
01442          GO TO 9100-RETURN-TRAN.
01443
01444      SET BENEFIT-INDEX UP BY +1.
01445
01446      GO TO 6510-MAIN-LOGIC.
01447
01448  6520-MAIN-LOGIC.
01449
01450      MOVE SPACES  TO  WS-BENEFIT-TABLE-ENTRY (BENEFIT-INDEX).
01451
01452      PERFORM 7100-UPDATE-BENEFIT-TABLE.
01453
01454      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
01455      MOVE LOW-VALUES             TO  EL107AO.
01456      PERFORM 8100-SEND-INITIAL-MAP.
01457
01458      MOVE ZERO                   TO  PI-1ST-TIME-SW
01459                                      PI-SHOW-SW.
01460      MOVE SPACES                 TO  PI-MODE
01461                                      PI-BENEFIT-TYPE
01462                                      PI-BENEFIT-NUMBER
01463                                      PI-NEXT-BENEFIT-NUMBER.
01464      GO TO 9100-RETURN-TRAN.
01465
01466      EJECT
01467 *****************************************************************
01468  7000-LOAD-BENEFIT-RECORDS SECTION.
01469 *****************************************************************
01470
01471      
      * EXEC CICS HANDLE CONDITION
01472 *        NOTFND  (7090-EXIT)
01473 *        ENDFILE (7030-ENDBROWSE)
01474 *    END-EXEC.
      *    MOVE '"$I''                  ! # #00005077' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303035303737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01475
01476      SET BENEFIT-INDEX TO +1.
01477
01478      MOVE SPACES                 TO  WS-BENEFIT-TABLE-AREA
01479                                      WS-CONTROL-FILE-KEY.
01480
01481      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
01482      MOVE PI-BENEFIT-TYPE        TO  WS-CFK-RECORD-TYPE.
01483      MOVE PI-BENEFIT-NUMBER      TO  WS-CFK-BENEFIT-NO.
01484      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
01485
01486      
      * EXEC CICS STARTBR
01487 *        DATASET   (WS-CONTROL-FILE-DSID)
01488 *        RIDFLD    (WS-CONTROL-FILE-KEY)
01489 *        GENERIC   EQUAL
01490 *        KEYLENGTH (4)
01491 *    END-EXEC.
           MOVE 4
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    E          &   #00005092' TO DFHEIV0
           MOVE X'262C2020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01492
01493  7010-READNEXT.
01494      
      * EXEC CICS READNEXT
01495 *        SET     (ADDRESS OF CONTROL-FILE)
01496 *        DATASET (WS-CONTROL-FILE-DSID)
01497 *        RIDFLD  (WS-CONTROL-FILE-KEY)
01498 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005100' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01499
01500      IF PI-BENEFIT-TYPE NOT = CF-RECORD-TYPE
01501          GO TO 7030-ENDBROWSE.
01502
01503      MOVE +1                     TO  WS-INDEX.
01504
01505  7020-MOVE-DATA.
01506      MOVE CF-BENEFIT-CONTROLS (WS-INDEX)
01507                                  TO  WS-BENEFIT-CONTROLS-WORK.
01508
01509      IF WS-BENEFIT-NUMBER NOT = ZEROS
01510          MOVE WS-BENEFIT-CONTROLS-WORK
01511                        TO  WS-BENEFIT-TABLE-ENTRY (BENEFIT-INDEX)
01512        ELSE
01513          GO TO 7030-ENDBROWSE.
01514
01515      SET BENEFIT-INDEX UP BY +1.
01516
01517      IF WS-INDEX LESS +8
01518          ADD +1  TO  WS-INDEX
01519          GO TO 7020-MOVE-DATA.
01520
01521      GO TO 7010-READNEXT.
01522
01523  7030-ENDBROWSE.
01524      
      * EXEC CICS ENDBR
01525 *        DATASET (WS-CONTROL-FILE-DSID)
01526 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005130' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01527
01528  7090-EXIT.
01529       EXIT.
01530
01531      EJECT
01532 *****************************************************************
01533  7100-UPDATE-BENEFIT-TABLE SECTION.
01534 *****************************************************************
01535
01536      MOVE SAVE-DATE              TO  DC-GREG-DATE-1-EDIT.
01537      MOVE '2'                    TO  DC-OPTION-CODE.
01538
01539      PERFORM 8500-DATE-CONVERSION.
01540
01541      MOVE EIBTIME                TO  TIME-IN.
01542
01543      
      * EXEC CICS HANDLE CONDITION
01544 *        NOTFND  (7125-WRITE-BENEFIT-RECORDS)
01545 *        ENDFILE (7120-ENDBROWSE)
01546 *    END-EXEC.
      *    MOVE '"$I''                  ! $ #00005149' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303035313439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01547
01548      SET BENEFIT-INDEX TO +1.
01549
01550      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.
01551
01552      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
01553      MOVE PI-BENEFIT-TYPE        TO  WS-CFK-RECORD-TYPE.
01554      MOVE PI-BENEFIT-NUMBER      TO  WS-CFK-BENEFIT-NO.
01555      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
01556
01557      
      * EXEC CICS STARTBR
01558 *        DATASET   (WS-CONTROL-FILE-DSID)
01559 *        RIDFLD    (WS-CONTROL-FILE-KEY)
01560 *        GENERIC   EQUAL
01561 *        KEYLENGTH (4)
01562 *    END-EXEC.
           MOVE 4
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    E          &   #00005163' TO DFHEIV0
           MOVE X'262C2020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01563
01564      MOVE 'D'                    TO  JP-RECORD-TYPE.
01565      MOVE ZERO                   TO  JP-GENERIC-KEY-LENGTH.
01566
01567  7110-READNEXT.
01568      
      * EXEC CICS READNEXT
01569 *        DATASET (WS-CONTROL-FILE-DSID)
01570 *        RIDFLD  (WS-CONTROL-FILE-KEY)
01571 *        SET     (ADDRESS OF CONTROL-FILE)
01572 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005174' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01573
01574      IF PI-BENEFIT-TYPE NOT = CF-RECORD-TYPE
01575          GO TO 7120-ENDBROWSE.
01576
01577      IF PI-UPDATE-BY     NOT = CF-LAST-MAINT-BY  OR
01578         PI-UPDATE-HHMMSS NOT = CF-LAST-MAINT-HHMMSS
01579          MOVE ER-0068            TO  EMI-ERROR
01580          MOVE LOW-VALUES         TO  EL107AO
01581          MOVE -1                 TO  AMAINTL
01582          PERFORM 8100-SEND-INITIAL-MAP
01583          GO TO 9100-RETURN-TRAN.
01584
01585      
      * EXEC CICS ENDBR
01586 *        DATASET (WS-CONTROL-FILE-DSID)
01587 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005191' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01588
01589      
      * EXEC CICS READ UPDATE
01590 *        DATASET (WS-CONTROL-FILE-DSID)
01591 *        RIDFLD  (WS-CONTROL-FILE-KEY)
01592 *        SET     (ADDRESS OF CONTROL-FILE)
01593 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005195' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01594
01595      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
01596
01597      
      * EXEC CICS DELETE
01598 *        DATASET (WS-CONTROL-FILE-DSID)
01599 *    END-EXEC.
      *    MOVE '&(                    &   #00005203' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01600
01601      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
01602
01603      
      * EXEC CICS STARTBR
01604 *        DATASET (WS-CONTROL-FILE-DSID)
01605 *        RIDFLD  (WS-CONTROL-FILE-KEY)
01606 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005209' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01607
01608      GO TO 7110-READNEXT.
01609
01610  7120-ENDBROWSE.
01611      
      * EXEC CICS ENDBR
01612 *        DATASET (WS-CONTROL-FILE-DSID)
01613 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005217' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01614
01615  7125-WRITE-BENEFIT-RECORDS.
01616      
      * EXEC CICS GETMAIN
01617 *        SET     (ADDRESS OF CONTROL-FILE)
01618 *        LENGTH  (750)
01619 *        INITIMG (WS-SPACE)
01620 *    END-EXEC.
           MOVE 750
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00005222' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 WS-SPACE
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01621
01622      MOVE 'A'                    TO  JP-RECORD-TYPE.
01623
01624      SET BENEFIT-INDEX TO +1.
01625
01626      EJECT
01627  7130-CREATE-BENEFIT-RECORD.
01628      MOVE SPACES                 TO  CONTROL-FILE.
01629
01630      MOVE 'CF'                   TO  CF-RECORD-ID.
01631      MOVE PI-COMPANY-ID          TO  CF-COMPANY-ID.
01632      MOVE PI-BENEFIT-TYPE        TO  CF-RECORD-TYPE.
01633      MOVE ZERO                   TO  CF-SEQUENCE-NO.
01634      MOVE DC-BIN-DATE-1          TO  CF-LAST-MAINT-DT.
01635      MOVE PI-PROCESSOR-ID        TO  CF-LAST-MAINT-BY.
01636      MOVE TIME-IN                TO  CF-LAST-MAINT-HHMMSS.
01637
01638      MOVE ZERO                   TO  CF-BENEFIT-CODE (1)
01639                                      CF-BENEFIT-CODE (2)
01640                                      CF-BENEFIT-CODE (3)
01641                                      CF-BENEFIT-CODE (4)
01642                                      CF-BENEFIT-CODE (5)
01643                                      CF-BENEFIT-CODE (6)
01644                                      CF-BENEFIT-CODE (7)
01645                                      CF-BENEFIT-CODE (8).
01646
01647      MOVE +1                     TO  WS-INDEX.
01648
01649  7140-MOVE-TABLE-ENTRIES.
01650      IF WS-BTE-NUMBER (BENEFIT-INDEX) = SPACES
01651          GO TO 7150-INCREMENT-BENEFIT-INDEX.
01652
01653      MOVE WS-BTE-NUMBER (BENEFIT-INDEX)  TO  CF-HI-BEN-IN-REC.
01654
01655      MOVE WS-BENEFIT-TABLE-ENTRY (BENEFIT-INDEX)
01656                  TO  CF-BENEFIT-CONTROLS (WS-INDEX).
01657
01658      IF WS-INDEX LESS +8
01659          ADD +1  TO  WS-INDEX
01660        ELSE
01661          GO TO 7160-WRITE-BENEFIT-RECORD.
01662
01663  7150-INCREMENT-BENEFIT-INDEX.
01664      IF BENEFIT-INDEX < WS-MAX-BEN-CODES
01665          SET BENEFIT-INDEX UP BY +1
01666          GO TO 7140-MOVE-TABLE-ENTRIES.
01667
01668      IF WS-INDEX NOT GREATER +1
01669          GO TO 7180-FREEMAIN.
01670
01671  7160-WRITE-BENEFIT-RECORD.
01672      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
01673
01674      
      * EXEC CICS WRITE
01675 *        DATASET (WS-CONTROL-FILE-DSID)
01676 *        RIDFLD  (CF-CONTROL-PRIMARY)
01677 *        FROM    (CONTROL-FILE)
01678 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00005280' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 CF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01679
01680      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
01681
01682      IF BENEFIT-INDEX < WS-MAX-BEN-CODES
01683          SET BENEFIT-INDEX UP BY +1
01684          GO TO 7130-CREATE-BENEFIT-RECORD.
01685
01686  7180-FREEMAIN.
01687      
      * EXEC CICS FREEMAIN
01688 *        DATA (CONTROL-FILE)
01689 *    END-EXEC.
      *    MOVE ',$D                   "   #00005293' TO DFHEIV0
           MOVE X'2C2444202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CONTROL-FILE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01690
01691  7190-EXIT.
01692       EXIT.
01693
01694      EJECT
01695 *****************************************************************
01696  7200-UPDATE-USER-TIME SECTION.
01697 *****************************************************************
01698
01699      
      * EXEC CICS HANDLE CONDITION
01700 *        NOTFND  (7220-ENDBROWSE)
01701 *        ENDFILE (7220-ENDBROWSE)
01702 *    END-EXEC.
      *    MOVE '"$I''                  ! % #00005305' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303035333035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01703
01704      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.
01705
01706      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
01707      MOVE PI-BENEFIT-TYPE        TO  WS-CFK-RECORD-TYPE.
01708      MOVE LOW-VALUES             TO  WS-CFK-BENEFIT-NO.
01709      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
01710
01711      
      * EXEC CICS STARTBR
01712 *        DATASET   (WS-CONTROL-FILE-DSID)
01713 *        RIDFLD    (WS-CONTROL-FILE-KEY)
01714 *        GENERIC   EQUAL
01715 *        KEYLENGTH (4)
01716 *    END-EXEC.
           MOVE 4
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    E          &   #00005317' TO DFHEIV0
           MOVE X'262C2020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01717
01718  7210-READNEXT.
01719      
      * EXEC CICS READNEXT
01720 *        DATASET (WS-CONTROL-FILE-DSID)
01721 *        RIDFLD  (WS-CONTROL-FILE-KEY)
01722 *        SET     (ADDRESS OF CONTROL-FILE)
01723 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005325' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01724
01725      IF PI-BENEFIT-TYPE NOT = CF-RECORD-TYPE
01726          GO TO 7220-ENDBROWSE.
01727
01728      IF PI-UPDATE-BY = CF-LAST-MAINT-BY  AND
01729         PI-UPDATE-HHMMSS = CF-LAST-MAINT-HHMMSS
01730          GO TO 7210-READNEXT.
01731
01732      
      * EXEC CICS ENDBR
01733 *        DATASET (WS-CONTROL-FILE-DSID)
01734 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005338' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01735
01736      
      * EXEC CICS READ UPDATE
01737 *        DATASET (WS-CONTROL-FILE-DSID)
01738 *        RIDFLD  (WS-CONTROL-FILE-KEY)
01739 *        SET     (ADDRESS OF CONTROL-FILE)
01740 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005342' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01741
01742      MOVE PI-UPDATE-BY           TO  CF-LAST-MAINT-BY.
01743      MOVE PI-UPDATE-HHMMSS       TO  CF-LAST-MAINT-HHMMSS.
01744      MOVE SAVE-BIN-DATE          TO  CF-LAST-MAINT-DT.
01745
01746      
      * EXEC CICS REWRITE
01747 *        DATASET ('ELCNTL')
01748 *        FROM (CONTROL-FILE)
01749 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&& L                  %   #00005352' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01750
01751      
      * EXEC CICS STARTBR
01752 *        DATASET (WS-CONTROL-FILE-DSID)
01753 *        RIDFLD  (WS-CONTROL-FILE-KEY)
01754 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005357' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01755
01756      GO TO 7210-READNEXT.
01757
01758  7220-ENDBROWSE.
01759      
      * EXEC CICS ENDBR
01760 *        DATASET (WS-CONTROL-FILE-DSID)
01761 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005365' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01762
01763      EJECT
01764 *****************************************************************
01765  8000-DISPLAY-RECORDS SECTION.
01766 *****************************************************************
01767
01768      
      * EXEC CICS HANDLE CONDITION
01769 *        NOTFND  (8060-DISPLAY-RECORDS)
01770 *        ENDFILE (8040-DISPLAY-RECORDS)
01771 *    END-EXEC.
      *    MOVE '"$I''                  ! & #00005374' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303035333734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01772
01773      SET EL107A-INDEX TO +1.
01774      MOVE PI-BENEFIT-NUMBER      TO  PI-LAST-BENEFIT-NUMBER.
01775
01776      
      * EXEC CICS STARTBR
01777 *        DATASET   (WS-CONTROL-FILE-DSID)
01778 *        RIDFLD    (WS-CONTROL-FILE-KEY)
01779 *        GENERIC   GTEQ
01780 *        KEYLENGTH (8)
01781 *    END-EXEC.
           MOVE 8
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    G          &   #00005382' TO DFHEIV0
           MOVE X'262C2020204B472020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01782
01783      MOVE LOW-VALUES             TO  EL107AO.
01784      MOVE ZERO                   TO  PI-LINE-COUNT.
01785
01786      EJECT
01787  8010-DISPLAY-RECORDS.
01788      
      * EXEC CICS READNEXT
01789 *        SET     (ADDRESS OF CONTROL-FILE)
01790 *        DATASET (WS-CONTROL-FILE-DSID)
01791 *        RIDFLD  (WS-CONTROL-FILE-KEY)
01792 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005394' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01793
01794      ADD +1  TO  WS-RECORD-COUNT.
01795
01796      IF PI-CHANGE-SW EQUAL +1
01797          IF WS-RECORD-COUNT EQUAL +1
01798              MOVE CF-CONTROL-PRIMARY TO PI-UPDATE-KEY.
01799
01800      MOVE +1                     TO  WS-INDEX.
01801
01802 *    NOTE *******************************************************
01803 *         *      CHECK IF ALL OF THE RECORDS OF THIS BENEFIT    *
01804 *         *  TYPE HAVE BEEN PROCESSED.                          *
01805 *         *******************************************************
01806
01807      IF WS-CFK-COMPANY-ID NOT = PI-COMPANY-ID  OR
01808         CF-RECORD-TYPE    NOT = PI-BENEFIT-TYPE
01809          GO TO 8015-DISPLAY-RECORDS.
01810
01811 *    NOTE *******************************************************
01812 *         *      IF WS-READNEXT SW IS ON (+1), THIS READ IS     *
01813 *         *  JUST TO FIND OUT THE KEY OF THE NEXT RECORD.       *
01814 *         *******************************************************
01815
01816      IF WS-READNEXT-SW = +1
01817          MOVE CF-BENEFIT-CONTROLS (1) TO WS-BENEFIT-CONTROLS-WORK
01818          MOVE WS-BENEFIT-NUMBER       TO PI-NEXT-BENEFIT-NUMBER
01819          GO TO 8050-DISPLAY-RECORDS.
01820
01821      GO TO 8020-DISPLAY-RECORDS.
01822
01823  8015-DISPLAY-RECORDS.
01824      IF WS-READNEXT-SW = +1
01825          MOVE SPACES             TO  PI-NEXT-BENEFIT-NUMBER
01826          MOVE ZERO               TO  PI-BROWSE-SW
01827          GO TO 8050-DISPLAY-RECORDS.
01828
01829      IF PI-MODE = 'A'
01830          GO TO 8050-DISPLAY-RECORDS.
01831
01832      MOVE SPACES                 TO  PI-NEXT-BENEFIT-NUMBER
01833      MOVE ZERO                   TO  PI-BROWSE-SW.
01834
01835      IF PI-LINE-COUNT NOT GREATER ZERO
01836          MOVE ER-0028            TO   EMI-ERROR
01837          IF PI-MODE = 'C' OR 'D'
01838              MOVE -1             TO  ABENEL
01839              MOVE AL-UABON       TO  ABENEA
01840              MOVE ZERO           TO  PI-1ST-TIME-SW.
01841
01842      GO TO 8040-DISPLAY-RECORDS.
01843
01844      EJECT
01845  8020-DISPLAY-RECORDS.
01846      IF LCP-ONCTR-01 =  0
01847          ADD 1 TO LCP-ONCTR-01
01848          MOVE CF-LAST-MAINT-BY      TO  PI-UPDATE-BY
01849          MOVE CF-LAST-MAINT-HHMMSS  TO  PI-UPDATE-HHMMSS
01850        ELSE
01851          GO TO 8030-DISPLAY-RECORDS.
01852
01853 *    NOTE *******************************************************
01854 *         *      THE FOLLOWING LOGIC IS TO BYPASS ALL TABLE     *
01855 *         *  ENTRIES UNTIL THE ENTRY INQUIRED IS THE 1ST ONE    *
01856 *         *  TO BE PROCESSED.                                   *
01857 *         *******************************************************
01858
01859      IF PI-BROWSE-SW = ZERO
01860          GO TO 8030-DISPLAY-RECORDS.
01861
01862      MOVE ER-0028                TO  EMI-ERROR.
01863
01864  8025-DISPLAY-RECORDS.
01865      MOVE CF-BENEFIT-CONTROLS (WS-INDEX)
01866                                  TO  WS-BENEFIT-CONTROLS-WORK.
01867
01868      IF WS-BENEFIT-NUMBER NOT LESS PI-BENEFIT-NUMBER
01869          GO TO 8030-DISPLAY-RECORDS.
01870
01871      IF WS-INDEX LESS +8
01872          ADD +1  TO  WS-INDEX
01873          GO TO 8025-DISPLAY-RECORDS.
01874
01875      GO TO 8010-DISPLAY-RECORDS.
01876
01877      EJECT
01878  8030-DISPLAY-RECORDS.
01879 *    NOTE *******************************************************
01880 *         *          MOVE THE TABLE ENTRY TO THE MAP.           *
01881 *         *******************************************************
01882
01883      MOVE CF-BENEFIT-CONTROLS (WS-INDEX)
01884                                  TO  WS-BENEFIT-CONTROLS-WORK.
01885
01886      IF WS-BENEFIT-NUMBER = ZERO
01887          GO TO 8015-DISPLAY-RECORDS.
01888
01889      IF PI-BENEFIT-NUMBER = WS-BENEFIT-NUMBER
01890          MOVE ZERO   TO  EMI-ERROR.
01891
01892      ADD +1  TO  PI-LINE-COUNT.
01893
01894      MOVE WS-BENEFIT-NUMBER      TO EL107A-CODE-O (EL107A-INDEX).
01895      MOVE WS-BENEFIT-ABBREVIATION TO EL107A-ABBR-O (EL107A-INDEX).
01896      MOVE WS-BENEFIT-DESCRIPTION TO EL107A-DESC-O (EL107A-INDEX).
01897      MOVE WS-BENEFIT-COMMENT   TO EL107A-COMMENT-O (EL107A-INDEX).
051414     if pi-benefit-type not = '4'
051414        if ws-benefit-max-bens not = zeros
051414           MOVE WS-BENEFIT-MAX-BENS
051414                         TO EL107A-MAX-BENS-O (EL107A-INDEX)
051414        end-if
051414     end-if
01898      MOVE WS-BENEFIT-LOAN-TYPE   TO EL107A-LOAN-O (EL107A-INDEX).
01899      MOVE WS-BENEFIT-EARN-METHOD TO EL107A-EM-O   (EL107A-INDEX).
01900      MOVE WS-BENEFIT-JOINT-COVERAGE
01901                                  TO EL107A-JOINT-O (EL107A-INDEX).
01902      MOVE WS-BENEFIT-OUTSTANDING-BAL
01903                                    TO EL107A-OB-O (EL107A-INDEX).
01904      MOVE WS-BENEFIT-COVERAGE-TYPE TO EL107A-LOD-O (EL107A-INDEX).
01905      MOVE WS-BENEFIT-REMAIN-TERM   TO EL107A-RTM-O (EL107A-INDEX).
01906      MOVE WS-BENEFIT-REFUND-METHOD TO EL107A-RFM-O (EL107A-INDEX).
01907      MOVE WS-BENEFIT-IG-CODE       TO EL107A-IGC-O (EL107A-INDEX).
082503     MOVE WS-BENEFIT-CATEGORY      TO EL107A-CAC-O (EL107A-INDEX)
01908
01909      MOVE AL-SANON  TO  EL107A-CODE-ATTRB (EL107A-INDEX).
01910
01911      IF PI-BENEFIT-TYPE NOT = '4'
01912          MOVE AL-SADOF TO EL107A-LOD-ATTRB (EL107A-INDEX)
01913          MOVE 'E J S  '          TO  AHEAD1O
01914          MOVE 'M T P  '          TO  AHEAD2O.
01915
01916  8033-DISPLAY-RECORDS.
01917      IF EL107A-INDEX LESS +8
01918          SET EL107A-INDEX UP BY +1
01919          GO TO 8035-DISPLAY-RECORDS.
01920
01921      IF WS-INDEX NOT LESS +8
01922          MOVE +1                 TO  WS-READNEXT-SW
01923          GO TO 8010-DISPLAY-RECORDS
01924        ELSE
01925          GO TO 8050-DISPLAY-RECORDS.
01926
01927  8035-DISPLAY-RECORDS.
01928      IF WS-INDEX LESS +8
01929          ADD +1  TO  WS-INDEX
01930          MOVE CF-BENEFIT-CONTROLS (WS-INDEX)
01931                                  TO  WS-BENEFIT-CONTROLS-WORK
01932          MOVE WS-BENEFIT-NUMBER  TO  PI-NEXT-BENEFIT-NUMBER
01933          GO TO 8030-DISPLAY-RECORDS.
01934
01935      IF PI-MODE = 'C' OR 'D'
01936          GO TO 8040-DISPLAY-RECORDS.
01937
01938      IF PI-MODE = 'A'
01939          GO TO 8050-DISPLAY-RECORDS.
01940
01941      GO TO 8010-DISPLAY-RECORDS.
01942
01943      EJECT
01944  8040-DISPLAY-RECORDS.
01945      IF EL107A-INDEX GREATER +8
01946          GO TO 8050-DISPLAY-RECORDS.
01947
01948      IF PI-BENEFIT-TYPE NOT = '4'
01949          MOVE AL-SADOF TO EL107A-LOD-ATTRB   (EL107A-INDEX)
01950          MOVE 'E J S  '          TO  AHEAD1O
01951          MOVE 'M T P  '          TO  AHEAD2O.
01952
01953      IF PI-MODE = 'C'
01954             AND PI-CHANGE-SW = +1
01955          MOVE AL-SADOF  TO  EL107A-CODE-ATTRB    (EL107A-INDEX)
01956                             EL107A-ABBR-ATTRB    (EL107A-INDEX)
01957                             EL107A-DESC-ATTRB    (EL107A-INDEX)
01958                             EL107A-COMMENT-ATTRB (EL107A-INDEX)
051414                            EL107A-MAX-BENS-ATTRB (EL107A-INDEX)
01959                             EL107A-LOAN-ATTRB    (EL107A-INDEX)
01960                             EL107A-EM-ATTRB      (EL107A-INDEX)
01961                             EL107A-JOINT-ATTRB   (EL107A-INDEX)
01962                             EL107A-OB-ATTRB      (EL107A-INDEX)
01963                             EL107A-LOD-ATTRB     (EL107A-INDEX)
01964                             EL107A-RTM-ATTRB     (EL107A-INDEX)
01965                             EL107A-RFM-ATTRB     (EL107A-INDEX)
01966                             EL107A-IGC-ATTRB     (EL107A-INDEX)
082503                            EL107A-CAC-ATTRB     (EL107A-INDEX).
01967
01968      SET EL107A-INDEX UP BY +1.
01969      GO TO 8040-DISPLAY-RECORDS.
01970
01971  8050-DISPLAY-RECORDS.
01972      
      * EXEC CICS ENDBR
01973 *        DATASET (WS-CONTROL-FILE-DSID)
01974 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005587' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01975
01976      IF PI-MODE = 'C'
01977          GO TO 8070-DISPLAY-RECORDS.
01978
01979      IF PI-MODE = 'A'
01980          MOVE -1  TO  EL107A-CODE-LENGTH (EL107A-INDEX)
01981          GO TO 8070-DISPLAY-RECORDS.
01982
01983      MOVE -1                     TO  AMAINTL.
01984      GO TO 8070-DISPLAY-RECORDS.
01985
01986  8060-DISPLAY-RECORDS.
01987      MOVE ER-0006                TO  EMI-ERROR.
01988      MOVE -1                     TO  ABENEL.
01989      MOVE AL-UABOF               TO  ABENEA.
01990
01991  8070-DISPLAY-RECORDS.
01992      IF PI-MODE = 'A'
01993          NEXT SENTENCE
01994        ELSE
01995          GO TO 8090-DISPLAY-RECORDS.
01996
01997  8080-DISPLAY-RECORDS.
01998      IF PI-BENEFIT-TYPE NOT = '4'
01999          MOVE AL-SADOF  TO  EL107A-LOD-ATTRB (EL107A-INDEX).
02000
02001      IF PI-MODE = 'C'
02002             AND PI-CHANGE-SW = +1
02003          MOVE AL-SADOF  TO  EL107A-CODE-ATTRB    (EL107A-INDEX)
02004                             EL107A-ABBR-ATTRB    (EL107A-INDEX)
02005                             EL107A-DESC-ATTRB    (EL107A-INDEX)
02006                             EL107A-COMMENT-ATTRB (EL107A-INDEX)
051414                            EL107A-MAX-BENS-ATTRB (EL107A-INDEX)
02007                             EL107A-LOAN-ATTRB    (EL107A-INDEX)
02008                             EL107A-EM-ATTRB      (EL107A-INDEX)
02009                             EL107A-JOINT-ATTRB   (EL107A-INDEX)
02010                             EL107A-OB-ATTRB      (EL107A-INDEX)
02011                             EL107A-LOD-ATTRB     (EL107A-INDEX)
02012                             EL107A-RTM-ATTRB     (EL107A-INDEX)
02013                             EL107A-RFM-ATTRB     (EL107A-INDEX)
02014                             EL107A-IGC-ATTRB     (EL107A-INDEX)
082603                            EL107A-CAC-ATTRB     (EL107A-INDEX).
02015
02016      IF EL107A-INDEX LESS +8
02017          SET EL107A-INDEX UP BY +1
02018          GO TO 8080-DISPLAY-RECORDS.
02019
02020  8090-DISPLAY-RECORDS.
02021      IF PI-BENEFIT-TYPE = '4'
02022          MOVE PI-LIFE-OVERRIDE-L1 TO  AKINDO
02023      ELSE
02024          MOVE PI-AH-OVERRIDE-L1   TO  AKINDO.
02025
02026      MOVE AL-UANON                TO  AKINDA.
02027
02028      IF PI-MODE = 'C'
02029          IF EMI-ERROR = ER-0028
02030              MOVE -1                  TO  ABENEL
02031          ELSE
02032          IF EMI-ERROR = ZERO AND PI-CHANGE-SW = +1
02033              MOVE AL-PANON            TO  AKINDA
02034                                           ABENEA
02035              MOVE -1                  TO  EL107A-ABBR-LENGTH (1)
02036              MOVE +2 TO  PI-CHANGE-SW
02037              MOVE ER-7535 TO  EMI-ERROR.
02038
02039      MOVE PI-MODE                TO  AMAINTO.
02040      MOVE AL-UANON               TO  AMAINTA.
02041
02042      IF EMI-ERROR NOT = ZERO AND ER-7535
02043          MOVE +2                 TO  PI-SHOW-SW.
02044
02045      PERFORM 8100-SEND-INITIAL-MAP.
02046
02047      IF PI-MODE = 'S'   AND
02048         PI-NEXT-BENEFIT-NUMBER NOT = SPACES
02049          MOVE +1                 TO  PI-BROWSE-SW.
02050
02051      GO TO 9100-RETURN-TRAN.
02052
02053      EJECT
02054 *****************************************************************
02055  8100-SEND-INITIAL-MAP SECTION.
02056 *****************************************************************
02057
02058      IF TRANSACTION-SUCCESSFUL OR
02059         INITIAL-TRANSACTION OR
02060         CHANGE-SUCCESSFUL
02061          NEXT SENTENCE
02062      ELSE
02063          GO TO 8100-SEND-MAP.
02064
02065      MOVE -1                     TO  AMAINTL.
02066      MOVE ZERO                   TO  PI-BROWSE-SW.
02067
02068      IF CHANGE-SUCCESSFUL
02069          GO TO 8100-SEND-MAP.
02070
02071      SET EL107A-INDEX TO +1.
02072
02073  8100-INITALIZE-MAP-LINE.
02074      MOVE AL-SADOF  TO  EL107A-CODE-ATTRB    (EL107A-INDEX)
02075                         EL107A-ABBR-ATTRB    (EL107A-INDEX)
02076                         EL107A-DESC-ATTRB    (EL107A-INDEX)
02077                         EL107A-COMMENT-ATTRB (EL107A-INDEX)
051414                        EL107A-MAX-BENS-ATTRB (EL107A-INDEX)
02078                         EL107A-LOAN-ATTRB    (EL107A-INDEX)
02079                         EL107A-EM-ATTRB      (EL107A-INDEX)
02080                         EL107A-JOINT-ATTRB   (EL107A-INDEX)
02081                         EL107A-OB-ATTRB      (EL107A-INDEX)
02082                         EL107A-LOD-ATTRB     (EL107A-INDEX)
02083                         EL107A-RTM-ATTRB     (EL107A-INDEX)
02084                         EL107A-RFM-ATTRB     (EL107A-INDEX)
02085                         EL107A-IGC-ATTRB     (EL107A-INDEX)
082603                        EL107A-CAC-ATTRB     (EL107A-INDEX)
02086
02087      IF EL107A-INDEX LESS +8
02088          SET EL107A-INDEX UP BY +1
02089          GO TO 8100-INITALIZE-MAP-LINE.
02090
02091  8100-SEND-MAP.
02092      MOVE SAVE-DATE              TO  ADATEO.
02093      MOVE EIBTIME                TO  TIME-IN.
02094      MOVE TIME-OUT               TO  ATIMEO.
02095
02096      MOVE PI-LIFE-OVERRIDE-L1    TO  WS-HD0-LF-L1.
02097      MOVE PI-AH-OVERRIDE-L1      TO  WS-HD0-AH-L1.
02098      MOVE WS-HEADING-0           TO  AHEAD0O.
02099
02100      IF EMI-ERROR NOT = ZERO
02101          PERFORM 9900-ERROR-FORMAT
02102        ELSE
02103          IF TRANSACTION-SUCCESSFUL
02104              PERFORM 9900-ERROR-FORMAT
02105          ELSE
02106            IF CHANGE-SUCCESSFUL
02107                PERFORM 9900-ERROR-FORMAT.
02108
02109      MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.
02110      MOVE EMI-MESSAGE-AREA (2)    TO  AEMSG2O.
02111
02112      
      * EXEC CICS SEND
02113 *        FROM   (EL107AO)
02114 *        MAPSET (WS-MAPSET-NAME)
02115 *        MAP    (WS-MAP-NAME)
02116 *        CURSOR ERASE
02117 *    END-EXEC.
           MOVE LENGTH OF
            EL107AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005731' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL107AO, 
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
           
02118
02119  8100-EXIT.
02120       EXIT.
02121
02122      EJECT
02123 *****************************************************************
02124  8200-SEND-DATAONLY SECTION.
02125 *****************************************************************
02126
02127      MOVE SAVE-DATE              TO  ADATEO.
02128      MOVE EIBTIME                TO  TIME-IN.
02129      MOVE TIME-OUT               TO  ATIMEO.
02130
02131      MOVE PI-LIFE-OVERRIDE-L1    TO  WS-HD0-LF-L1.
02132      MOVE PI-AH-OVERRIDE-L1      TO  WS-HD0-AH-L1.
02133      MOVE WS-HEADING-0           TO  AHEAD0O.
02134
02135      IF EMI-ERROR NOT = ZERO
02136          PERFORM 9900-ERROR-FORMAT.
02137
02138      MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.
02139      MOVE EMI-MESSAGE-AREA (2)    TO  AEMSG2O.
02140
02141      
      * EXEC CICS SEND DATAONLY
02142 *        FROM   (EL107AO)
02143 *        MAPSET (WS-MAPSET-NAME)
02144 *        MAP    (WS-MAP-NAME)
02145 *        CURSOR
02146 *    END-EXEC.
           MOVE LENGTH OF
            EL107AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00005760' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL107AO, 
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
           
02147
02148  8200-EXIT.
02149       EXIT.
02150
02151      EJECT
02152 *****************************************************************
02153  8300-SEND-TEXT SECTION.
02154 *****************************************************************
02155
02156      
      * EXEC CICS SEND TEXT
02157 *        FROM   (LOGOFF-TEXT)
02158 *        LENGTH (LOGOFF-LENGTH)
02159 *        ERASE  FREEKB
02160 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005775' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373735' TO DFHEIV0(25:11)
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
           
02161
02162      
      * EXEC CICS RETURN
02163 *    END-EXEC.
      *    MOVE '.(                    ''   #00005781' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02164
02165  8300-EXIT.
02166       EXIT.
02167
02168      EJECT
02169 *****************************************************************
02170  8400-LOG-JOURNAL-RECORD SECTION.
02171 *****************************************************************
02172
02173      IF PI-JOURNAL-FILE-ID = 0
02174          GO TO 8400-EXIT.
02175
02176      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
02177      MOVE WS-CONTROL-FILE-DSID   TO  JP-FILE-ID.
02178      MOVE THIS-PGM               TO  JP-PROGRAM-ID.
02179
pemuni*    EXEC CICS JOURNAL
pemuni*        JFILEID (PI-JOURNAL-FILE-ID)
pemuni*        JTYPEID (WS-JOURNAL-TYPE-ID)
pemuni*        FROM    (JOURNAL-RECORD)
pemuni*        LENGTH  (WS-JOURNAL-RECORD-LENGTH)
pemuni*    END-EXEC.
02186
02187  8400-EXIT.
02188       EXIT.
02189
02190 *****************************************************************
02191  8500-DATE-CONVERSION SECTION.
02192 *****************************************************************
02193
02194      
      * EXEC CICS LINK
02195 *        PROGRAM  ('ELDATCV')
02196 *        COMMAREA (DATE-CONVERSION-DATA)
02197 *        LENGTH   (DC-COMM-LENGTH)
02198 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00005813' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02199
02200  8500-EXIT.
02201       EXIT.
02202
02203      EJECT
02204 *****************************************************************
02205  9000-RETURN-CICS SECTION.
02206 *****************************************************************
02207
02208      MOVE 'EL005'                TO  THIS-PGM.
02209      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
02210      PERFORM 9300-XCTL.
02211
02212  9000-EXIT.
02213       EXIT.
02214
02215 *****************************************************************
02216  9100-RETURN-TRAN SECTION.
02217 *****************************************************************
02218
02219      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
02220      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
02221
02222      
      * EXEC CICS RETURN
02223 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
02224 *        LENGTH   (PI-COMM-LENGTH)
02225 *        TRANSID  (WS-TRANS-ID)
02226 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00005841' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02227
02228  9100-EXIT.
02229       EXIT.
02230
02231 *****************************************************************
02232  9300-XCTL SECTION.
02233 *****************************************************************
02234
02235      MOVE DFHENTER               TO  EIBAID.
02236
02237      
      * EXEC CICS XCTL
02238 *        PROGRAM  (THIS-PGM)
02239 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
02240 *        LENGTH   (PI-COMM-LENGTH)
02241 *    END-EXEC.
      *    MOVE '.$C                   %   #00005856' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02242
02243  9300-EXIT.
02244       EXIT.
02245
02246      EJECT
02247 *****************************************************************
02248  9400-CLEAR SECTION.
02249 *****************************************************************
02250
02251      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.
02252      PERFORM 9300-XCTL.
02253
02254  9400-EXIT.
02255       EXIT.
02256
02257 *****************************************************************
02258  9600-PGMIDERR SECTION.
02259 *****************************************************************
02260
02261      
      * EXEC CICS HANDLE CONDITION
02262 *        PGMIDERR (8300-SEND-TEXT)
02263 *    END-EXEC.
      *    MOVE '"$L                   ! '' #00005880' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303035383830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02264
02265      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM
02266                                      LOGOFF-PGM.
02267
02268      MOVE 'EL005'                TO  THIS-PGM.
02269      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
02270      MOVE SPACES                 TO  PI-ENTRY-CD-1.
02271      PERFORM 9300-XCTL.
02272
02273  9600-EXIT.
02274       EXIT.
02275
02276 *****************************************************************
02277  9900-ERROR-FORMAT SECTION.
02278 *****************************************************************
02279
02280      IF EMI-ERRORS-COMPLETE
02281          ADD +1             TO  EMI-FATAL-CTR
02282          MOVE ZERO          TO  EMI-ERROR
02283          GO TO 9900-EXIT.
02284
02285      
      * EXEC CICS LINK
02286 *        PROGRAM  ('EL001')
02287 *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
02288 *        LENGTH   (EMI-COMM-LENGTH)
02289 *    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   (   #00005904' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02290
02291      MOVE ZERO              TO  EMI-ERROR.
02292
02293  9900-EXIT.
02294       EXIT.
02295
02296      EJECT
02297 *****************************************************************
02298  9990-ERROR SECTION.
02299 *****************************************************************
02300
02301      MOVE DFHEIBLK               TO EMI-LINE1.
02302      
      * EXEC CICS LINK
02303 *        PROGRAM   ('EL004')
02304 *        COMMAREA  (EMI-LINE1)
02305 *        LENGTH    (72)
02306 *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00005921' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02307
02308      PERFORM 8200-SEND-DATAONLY.
02309      GO TO 9100-RETURN-TRAN.
02310
02311  9990-EXIT.
02312       EXIT.
02313
02314 *****************************************************************
02315  9995-SECURITY-VIOLATION.
02316 *****************************************************************
02317
02318 *           COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00005954' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393534' TO DFHEIV0(25:11)
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
02319
02320  9995-EXIT.
02321       EXIT.
02322
02323  9999-LAST-PARAGRAPH SECTION.
02324      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL107' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL107' TO DFHEIV1
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
               GO TO 7090-EXIT,
                     7030-ENDBROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 7125-WRITE-BENEFIT-RECORDS,
                     7120-ENDBROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 7220-ENDBROWSE,
                     7220-ENDBROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8060-DISPLAY-RECORDS,
                     8040-DISPLAY-RECORDS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL107' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
