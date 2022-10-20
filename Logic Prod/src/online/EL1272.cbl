00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL1272.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/07/95 10:55:03.
00007 *                            VMOD=2.036.
00008 *
00008 *
00009 *AUTHOR.     LOGIC INC.
00010 *            DALLAS, TEXAS.
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
00025 *        THIS PROGRAM PROVIDES THE BROWSE NECESSARY FOR
00026 *    THE CERTIFICATE LOOK-UP.
00027
00028 *    SCREENS     - EL127B - CERTIFICATE LOOK-UP MATCH LIST
00029
00030 *    ENTERED BY  - EL127 - CERTIFICATE QUALIFICATION
00031
00032 *    EXIT TO     - EL127
00033 *                  EL130 - NEW CLAIM SETUP
00034
00035 *    INPUT FILE  - ELCERT - CERTIFICATE INFORCE FILE
00036
00037 *    OUTPUT FILE - NONE
00038
00039 *    COMMAREA    - PASSED.  IF A CERTIFICATE IS SELECTED, THE
00040 *                  CONTROL OF THAT CERTIFICATE IS PLACED IN THE
00041 *                  APPROPRIATE FIELDS OF THE COMMAAREA FOR
00042 *                  REFERENCE BY SUCCESSIVE PROGRAMS.  THE PROGRAM
00043 *                  WORK AREA OF THE COMMAREA IS USED TO PASS THE
00044 *                  RECORD KEY INFORMATION NEEDED BY EL1272 TO
00045 *                  LOCATE THE CERTIFICATE.
00046
00047 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON
00048 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE
00049 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
00050 *                  ENTRIES (XCTL FROM CICS VIA EXX2) THE SCREEN
00051 *                  WILL BE READ AND ACTION WILL BE BASED ON THE
00052 *                  MAINTENANCE TYPE INDICATED.
101501******************************************************************
101501*                   C H A N G E   L O G
101501*
101501* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101501*-----------------------------------------------------------------
101501*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101501* EFFECTIVE    NUMBER
101501*-----------------------------------------------------------------
101501* 101501    2001100100006  SMVA  ADD USERID & COMPANY ID TO SCREEN
101501*                              ADJUSTED REDEFINES EL127BI FILLER
110106* 110106  CR2005050500006  PEMA  ADD STATE TO OPTION 3
101501******************************************************************
00053      EJECT
00054  ENVIRONMENT DIVISION.
00055
00056  DATA DIVISION.
00057
00058  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00059  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.
00060  77  LCP-ONCTR-02                  PIC S9(8) COMP-3 VALUE ZERO.
00061
00062  77  FILLER  PIC X(32)  VALUE '********************************'.
00063  77  FILLER  PIC X(32)  VALUE '*   EL1272 WORKING STORAGE     *'.
00064  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.036 *********'.
00065
       01  filler.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL              VALUE +00.
               88  RESP-ERROR               VALUE +01.
               88  RESP-TERMIDERR           VALUE +11.
               88  RESP-NOTFND              VALUE +13.
               88  RESP-DUPREC              VALUE +14.
               88  RESP-DUPKEY              VALUE +15.
               88  RESP-INVREQ              VALUE +16.
               88  RESP-NOTOPEN             VALUE +19.
               88  RESP-ENDFILE             VALUE +20.
               88  RESP-ILLOGIC             VALUE +21.
               88  RESP-LENGERR             VALUE +22.
00066  01  FLA-WS.
00067      05  SAVE-NAME                   PIC X(15).
00068      05  FILLER REDEFINES SAVE-NAME.
00069          10 SV-BYTE                  PIC X OCCURS 15.
00070      05  A-SUB                       PIC 99.
00071      05  B-SUB                       PIC 99.
00072
00073 *                                    COPY ELCSCTM.
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
00074
00075 *                                    COPY ELCSCRTY.
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
00076
00077  01  WS-DATE-AREA.
00078      05  SAVE-DATE                   PIC X(8)     VALUE SPACES.
00079      05  SAVE-BIN-DATE               PIC X(2)     VALUE SPACES.
00080
00081  01  FILLER                          COMP-3.
00082      05  WS-NOT-FOUND                PIC S9       VALUE ZERO.
00083      05  WS-ST-REC-NOT-FOUND         PIC S9       VALUE ZERO.
00084      05  TIME-IN                     PIC S9(7)    VALUE ZERO.
00085      05  TIME-OUT                    REDEFINES
00086          TIME-IN                     PIC S9(3)V9(4).
00087
00088      05  WS-MONTH-WORK               PIC S9(3)    VALUE ZERO.
00089      05  WS-YEAR-WORK                PIC S9(3)    VALUE ZERO.
00090      05  WS-CERT-SW                  PIC S9       VALUE ZERO.
00091         88  WS-NO-CERT-FOUND                      VALUE ZERO.
00092      05  SUB                         PIC S99      VALUE ZERO.
00093
00094  01  FILLER                          COMP
00095                                      SYNC.
00096      05  WS-INDEX                    PIC S9(4)    VALUE ZERO.
00097      05  WS-TS-LENGTH                PIC S9(4)    VALUE +1920.
00098      05  WS-WORK-LENGTH              PIC S9(4)    VALUE +1024.
00099      05  WS-AIX-RECORD-COUNT         PIC S9(4)    VALUE ZERO.
00100
00101  01  FILLER.
00102
00103      05  WS-COMPARE-INDICATOR        PIC  X.
00104          88 NAME-FOUND                         VALUE SPACE.
00105          88 NAME-NOT-FOUND                     VALUE 'X'.
00106      05  WS-NAME-INDEX               PIC S9(4)  COMP.
00107      05  WS-CM-NAME                  PIC X(15).
00108      05  WS-CM-NAME-CHAR REDEFINES
00109          WS-CM-NAME                  PIC X
00110                                      OCCURS 15.
00111      05  WS-PI-NAME                  PIC  X(15).
00112      05  WS-PI-NAME-CHAR  REDEFINES
00113          WS-PI-NAME                  PIC  X
00114                                      OCCURS 15.
00115
00116      05  QID.
00117          10  QID-TERM                PIC X(4).
00118          10  FILLER                  PIC X(4)     VALUE '127A'.
00119      05  QID-ITEM                    PIC S9(4)    VALUE +1  COMP.
00120
00121      05  WS-CONTROL-FILE-KEY.
00122          10  WS-CFK-COMPANY-ID           PIC X(3).
00123          10  WS-CFK-RECORD-TYPE          PIC X.
00124          10  WS-CFK-ACCESS-TYPE.
00125              15  WS-CFK-BENE-ACCESS.
00126                  20  FILLER              PIC XX.
00127                  20  WS-CFK-BENEFIT-NO   PIC XX.
00128              15  WS-CFK-PROC-ACCESS REDEFINES WS-CFK-BENE-ACCESS.
00129                  20  WS-CFK-PROD-ID      PIC X(04).
00130          10  WS-CFK-SEQUENCE-NO          PIC S9(4)  COMP.
00131
00132      05  WS-MAPSET-NAME              PIC X(8)     VALUE 'EL127S'.
00133      05  WS-MAP-NAME                 PIC X(8)     VALUE 'EL127B'.
00134
00135      05  FILLER                      REDEFINES
00136          WS-MAP-NAME.
00137          10  FILLER                  PIC XX.
00138          10  WS-MAP-NUMBER           PIC X(4).
00139          10  FILLER                  PIC XX.
00140
00141      05  THIS-PGM                    PIC X(8)     VALUE 'EL1272'.
00142      05  ELRTRM                      PIC X(8)     VALUE 'ELRTRM'.
00143
00144      05  WS-CONTROL-FILE-DSID        PIC X(8)     VALUE 'ELCNTL'.
00145
00146      05  WS-TRANS-ID                 PIC X(4)     VALUE 'EXX2'.
00147
00148      05  WS-TEMP-STORAGE-KEY.
00149          10  WS-TSK-TERM-ID          PIC X(4)     VALUE 'XXXX'.
00150          10  FILLER                  PIC X(4)     VALUE '1272'.
00151
00152      05  WS-EL1273-TS.
00153          10  WS-TS1-TERM-ID          PIC X(4)     VALUE 'XXXX'.
00154          10  FILLER                  PIC X(4)     VALUE '1273'.
00155
00156      05  WS-INITIALS.
00157          10  WS-INIT1                PIC X.
00158          10  WS-INIT2                PIC X.
00159
00160      05  WS-CURRENT-DATE             PIC XX.
00161
00162      05  WS-KEY-HOLD.
00163          10  WS-KH-CHAR              PIC X
00164              OCCURS 33 TIMES         INDEXED BY KEY-INDEX.
00165
00166      05  WS-KEY-INPUT.
00167          10  WS-KI-CHAR              PIC X
00168              OCCURS 33 TIMES         INDEXED BY KEY-INDEX2.
00169
00170      05  WS-CALC-RDNXT               PIC S9(8)    VALUE ZERO COMP.
00171      05  WS-CERTS-SELECTED           PIC X        VALUE 'N'.
00172          88  NO-CERTS-SELECTED                    VALUE 'N'.
00173      05  WS-SELECTED-SW              PIC X        VALUE 'N'.
00174          88  PREVIOUSLY-SELECTED                  VALUE 'Y'.
00175
00176      05  WS-CNTL-REC-FOUND-SW        PIC X        VALUE SPACE.
00177      05  WS-NEXT-COMPANY-ID          PIC XXX      VALUE SPACES.
00178
00179      05  WS-CERT-CONTROL.
00180          10  WS-CARRIER              PIC X.
00181          10  WS-GROUPING             PIC X(6).
00182          10  WS-STATE                PIC XX.
00183          10  WS-ACCOUNT              PIC X(10).
00184          10  WS-CERT-NO              PIC X(11).
00185          10  WS-EFF-DT               PIC XX.
00186
00187      EJECT
00188      05  ERROR-MESSAGES.
00189          10  ER-0004                 PIC X(4)     VALUE '0004'.
00190          10  ER-0008                 PIC X(4)     VALUE '0008'.
00191          10  ER-0019                 PIC X(4)     VALUE '0019'.
00192          10  ER-0022                 PIC X(4)     VALUE '0022'.
00193          10  ER-0029                 PIC X(4)     VALUE '0029'.
00194          10  ER-0070                 PIC X(4)     VALUE '0070'.
00195          10  ER-0089                 PIC X(4)     VALUE '0089'.
00196          10  ER-0130                 PIC X(4)     VALUE '0130'.
00197          10  ER-0131                 PIC X(4)     VALUE '0131'.
00198          10  ER-0200                 PIC X(4)     VALUE '0200'.
00199          10  ER-0201                 PIC X(4)     VALUE '0201'.
00200          10  ER-0228                 PIC X(4)     VALUE '0228'.
00201          10  ER-0363                 PIC X(4)     VALUE '0363'.
00202          10  ER-0659                 PIC X(4)     VALUE '0659'.
00203          10  ER-0765                 PIC X(4)     VALUE '0765'.
00204          10  ER-2848                 PIC X(4)     VALUE '2848'.
00205
00206      EJECT
00207 *                                    COPY ELCNWA.
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
00208
00209      EJECT
00210 *                                    COPY ELCINTF.
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
00211
00212      EJECT
00213 *                                    COPY ELC127PI.
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
00214          16  PI-SUB                       PIC S99.
00215          16  PI-FIRST-TIME-SW             PIC X.
00216          16  PI-EL127-TO-EL130-CNTRL.
00217              20  PI-CERT-SELECT-CNT       PIC S9(4)   COMP.
00218              20  PI-CERT-PROCESSED-CNT    PIC S9(4)   COMP.
00219              20  PI-CERT-CONTROLS-EL127 OCCURS 5 TIMES.
00220                  24  PI-EL127-CARRIER     PIC X.
00221                  24  PI-EL127-GROUPING    PIC X(6).
00222                  24  PI-EL127-STATE       PIC XX.
00223                  24  PI-EL127-ACCOUNT     PIC X(10).
00224                  24  PI-EL127-CERT-NO     PIC X(11).
00225                  24  PI-EL127-EFF-DT      PIC XX.
00226          16  PI-PART-KEY-SW               PIC X.
00227          16  PI-PART-FIELD-SW             PIC X.
00228          16  PI-SAVE-KEY-LENGTH           PIC S9(4)    COMP.
00229          16  FILLER                       PIC X(136).
       01  filler pic x(500) value spaces.
00230
00231      EJECT
00232 *                                    COPY EL127S.
       01  EL127BI.
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
           05  BUSERIDL PIC S9(0004) COMP.
           05  BUSERIDF PIC  X(0001).
           05  FILLER REDEFINES BUSERIDF.
               10  BUSERIDA PIC  X(0001).
           05  BUSERIDI PIC  X(0004).
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
      *    -------------------------------
           05  BPFK3L PIC S9(0004) COMP.
           05  BPFK3F PIC  X(0001).
           05  FILLER REDEFINES BPFK3F.
               10  BPFK3A PIC  X(0001).
           05  BPFK3I PIC  X(0016).
      *    -------------------------------
           05  BPFK5L PIC S9(0004) COMP.
           05  BPFK5F PIC  X(0001).
           05  FILLER REDEFINES BPFK5F.
               10  BPFK5A PIC  X(0001).
           05  BPFK5I PIC  X(0015).
      *    -------------------------------
           05  BPFK7L PIC S9(0004) COMP.
           05  BPFK7F PIC  X(0001).
           05  FILLER REDEFINES BPFK7F.
               10  BPFK7A PIC  X(0001).
           05  BPFK7I PIC  X(0013).
      *    -------------------------------
           05  BPFK4L PIC S9(0004) COMP.
           05  BPFK4F PIC  X(0001).
           05  FILLER REDEFINES BPFK4F.
               10  BPFK4A PIC  X(0001).
           05  BPFK4I PIC  X(0015).
      *    -------------------------------
           05  BPFK6L PIC S9(0004) COMP.
           05  BPFK6F PIC  X(0001).
           05  FILLER REDEFINES BPFK6F.
               10  BPFK6A PIC  X(0001).
           05  BPFK6I PIC  X(0013).
       01  EL127BO REDEFINES EL127BI.
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
           05  BUSERIDO PIC  X(0004).
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
           05  FILLER            PIC  X(0003).
           05  BPFK3O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFK5O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFK7O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFK4O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFK6O PIC  X(0013).
      *    -------------------------------
       01  EL127AI REDEFINES EL127BI.
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
           05  AUSERIDL PIC S9(0004) COMP.
           05  AUSERIDF PIC  X(0001).
           05  FILLER REDEFINES AUSERIDF.
               10  AUSERIDA PIC  X(0001).
           05  AUSERIDI PIC  X(0004).
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
           05  ASTL PIC S9(0004) COMP.
           05  ASTF PIC  X(0001).
           05  FILLER REDEFINES ASTF.
               10  ASTA PIC  X(0001).
           05  ASTI PIC  X(0002).
      *    -------------------------------
           05  ASTATUSL PIC S9(0004) COMP.
           05  ASTATUSF PIC  X(0001).
           05  FILLER REDEFINES ASTATUSF.
               10  ASTATUSA PIC  X(0001).
           05  ASTATUSI PIC  X(0001).
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
      *    -------------------------------
           05  APFK6L PIC S9(0004) COMP.
           05  APFK6F PIC  X(0001).
           05  FILLER REDEFINES APFK6F.
               10  APFK6A PIC  X(0001).
           05  APFK6I PIC  X(0016).
      *    -------------------------------
           05  APFK7L PIC S9(0004) COMP.
           05  APFK7F PIC  X(0001).
           05  FILLER REDEFINES APFK7F.
               10  APFK7A PIC  X(0001).
           05  APFK7I PIC  X(0020).
      *    -------------------------------
           05  APFK9L PIC S9(0004) COMP.
           05  APFK9F PIC  X(0001).
           05  FILLER REDEFINES APFK9F.
               10  APFK9A PIC  X(0001).
           05  APFK9I PIC  X(0028).
       01  EL127AO REDEFINES EL127BI.
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
           05  AUSERIDO PIC  X(0004).
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
           05  ASTO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTATUSO PIC  X(0001).
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
           05  FILLER            PIC  X(0003).
           05  APFK6O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFK7O PIC  X(0020).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFK9O PIC  X(0028).
      *    -------------------------------
00233
00234  01  FILLER REDEFINES EL127BI.
101501     05  FILLER                      PIC X(44).
00236
00237      05  EL127B-MAP-LINE             OCCURS 8 TIMES
00238          INDEXED BY EL127B-INDEX
00239                     EL127B-INDEX2.
00240
00241          10  EL127B-AST-LENGTH       PIC S9(4)    COMP.
00242          10  EL127B-AST-ATTRB        PIC X.
00243          10  EL127B-AST              PIC X.
00244
00245          10  EL127B-CERT-SEL-LENGTH  PIC S9(4)    COMP.
00246          10  EL127B-CERT-SEL-ATTRB   PIC X.
00247          10  EL127B-CERT-SEL         PIC X.
00248
00249          10  EL127B-NAME-LENGTH      PIC S9(4)    COMP.
00250          10  EL127B-NAME-ATTRB       PIC X.
00251          10  EL127B-NAME-O           PIC X(18).
00252
00253          10  EL127B-AGE-LENGTH       PIC S9(4)    COMP.
00254          10  EL127B-AGE-ATTRB        PIC X.
00255          10  EL127B-AGE              PIC 99.
00256
00257          10  EL127B-SEX-LENGTH       PIC S9(4)    COMP.
00258          10  EL127B-SEX-ATTRB        PIC X.
00259          10  EL127B-SEX              PIC X.
00260
00261          10  EL127B-CARRIER-LENGTH   PIC S9(4)    COMP.
00262          10  EL127B-CARRIER-ATTRB    PIC X.
00263          10  EL127B-CARRIER          PIC X.
00264
00265          10  EL127B-GROUP-LENGTH     PIC S9(4)    COMP.
00266          10  EL127B-GROUP-ATTRB      PIC X.
00267          10  EL127B-GROUP            PIC X(6).
00268
00269          10  EL127B-STATE-LENGTH     PIC S9(4)    COMP.
00270          10  EL127B-STATE-ATTRB      PIC X.
00271          10  EL127B-STATE            PIC XX.
00272
00273          10  EL127B-ACCOUNT-LENGTH   PIC S9(4)    COMP.
00274          10  EL127B-ACCOUNT-ATTRB    PIC X.
00275          10  EL127B-ACCOUNT          PIC X(10).
00276
00277          10  EL127B-CERT-NO-LENGTH   PIC S9(4)    COMP.
00278          10  EL127B-CERT-NO-ATTRB    PIC X.
00279          10  EL127B-CERT-NO          PIC X(11).
00280
00281          10  EL127B-EFF-DATE-LENGTH  PIC S9(4)    COMP.
00282          10  EL127B-EFF-DATE-ATTRB   PIC X.
00283          10  EL127B-EFF-DATE         PIC X(8).
00284
00285          10 EL127B-LIFE-INFO-LENGTH PIC S9(4) COMP.
00286          10 EL127B-LIFE-INFO-ATTRB PIC X.
00287          10 EL127B-LIFE-INFO.
00288              15 EL127B-MEMB-LOAN     PIC X(21).
00289              15 EL127B-LI-ABVR       PIC X(4).
00290              15 FILLER               PIC X.
00291              15 EL127B-LI-DESC2      PIC X(3).
00292              15 FILLER               PIC X.
00293              15 EL127B-LI-AMT        PIC ZZZ,ZZZ,ZZ9.99-.
00294              15 EL127B-LI-DATE       REDEFINES
00295                  EL127B-LI-AMT       PIC X(15).
00296
00297          10 EL127B-AH-INFO-LENGTH PIC S9(4)       COMP.
00298          10 EL127B-AH-INFO-ATTRB PIC X.
00299          10 EL127B-AH-INFO.
00300              15 EL127B-AH-ABVR       PIC X(4).
00301              15 FILLER               PIC X.
00302              15 EL127B-AH-DESC2      PIC X(3).
00303              15 FILLER               PIC X.
00304              15 EL127B-AH-AMT        PIC ZZZZ,ZZ9.99-.
00305              15 EL127B-AH-DATE       REDEFINES
00306                  EL127B-AH-AMT       PIC X(12).
00307              15 FILLER               PIC X.
00308
00309
00310      EJECT
00311 *                                    COPY ELCCALC.
00001 ******************************************************************
00002 *                                                                *
00003 *                           ELCCALC.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.025                          *
00006 *                                                                *
00007 *   DESCRIPTION:  DATA TO BE PASSED TO REMAINING TERM ROUTINE    *
00008 *                 REMAINING AMOUNT ROUTINE, LOSS RESERVE ROUTINE *
00009 *                 REFUND CALCULATIONS ROUTINE, EARNINGS CALCU -  *
00010 *                 LATIONS ROUTINE, AND THE RATING ROUTINE.       *
00011 *                                                                *
00012 *  PASSED TO ELRTRM                                              *
00013 *  -----------------                                             *
00014 *  METHOD CODE (I.E. FULL MONTH, HALF ADJ, ETC)                  *
00015 *  ORIGINAL TERM                                                 *
00016 *  BEGINNING DATE                                                *
00017 *  ENDING DATE                                                   *
00018 *  COMPANY I.D.                                                  *
00019 *  ACCOUNT MASTER USER FIELD                                     *
00020 *  PROCESS SWITCH (CANCEL, CLAIM)                                *
00021 *  FREE LOOK DAYS                                                *
00022 *                                                                *
00023 *  RETURNED FROM ELRTRM                                          *
00024 *  ---------------------                                         *
00025 *  REMAINING TERM 1 - USED FOR EARNINGS                          *
00026 *  REMAINING TERM 2 - USED FOR BENEFIT CALCULATIONS              *
00027 *  REMAINING TERM 3 - USED FOR CLAIM BENEFITS                    *
00028 *  ODD DAYS - REMAINING DAYS PAST FULL MONTHS                    *
00029 *----------------------------------------------------------------*
00030 *  PASSED TO ELRAMT                                              *
00031 *  ----------------                                              *
00032 *  REMAINING TERM 1 OR 2 OR 3 (FROM ELRTRM)                      *
00033 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00034 *  ORIGINAL AMOUNT                                               *
00035 *  ALTERNATE BENEFIT (BALLON)                                    *
00036 *  A.P.R. - NET PAY ONLY                                         *
00037 *  METHOD
00038 *  PAYMENT FREQUENCY - FOR FARM PLAN                             *
00039 *  COMPANY I.D.                                                  *
00040 *  BENEFIT TYPE                                                  *
00041 *                                                                *
00042 *  RETURNED FROM ELRAMT                                          *
00043 *  --------------------                                          *
00044 *  REMAINING AMOUNT 1 - CURRENT                                  *
00045 *  REMAINING AMOUNT 2 - PREVIOUS MONTH                           *
00046 *  REMAINING AMOUNT FACTOR
00047 *----------------------------------------------------------------*
00048 *  PASSED TO ELRESV                                              *
00049 *  -----------------                                             *
00050 *  CERTIFICATE EFFECTIVE DATE                                    *
00051 *  VALUATION DATE                                                *
00052 *  PAID THRU DATE                                                *
00053 *  BENEFIT                                                       *
00054 *  INCURRED DATE                                                 *
00055 *  REPORTED DATE                                                 *
00056 *  ISSUE AGE                                                     *
00057 *  TERM                                                          *
00058 *  CDT PERCENT                                                   *
00059 *  CDT METHOD (I.E. INTERPOLATED, AVERAGE, ETC)                  *
00060 * *CLAIM TYPE (LIFE, A/H)                                        *
00061 * *REMAINING BENEFIT (FROM ELRAMT)                               *
00062 * *ONLY FIELDS REQUIRED FOR LIFE CLAIMS                          *
00063 *                                                                *
00064 *  RETURNED FROM ELRESV                                          *
00065 *  --------------------                                          *
00066 *  CDT TABLE USED                                                *
00067 *  CDT FACTOR USED                                               *
00068 *  PAY TO CURRENT RESERVE                                        *
00069 *  I.B.N.R. - A/H ONLY                                           *
00070 *  FUTURE (ACCRUED) AH ONLY                                      *
00071 *----------------------------------------------------------------*
00072 *  PASSED TO ELRATE                                              *
00073 *  ----------------                                              *
00074 *  CERT ISSUE DATE                                               *
00075 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00076 *  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
00077 *  CAPPED TERM   (ONLY FOR TRUNCATED LIFE)                       *
00078 *  STATE CODE (CLIENT DEFINED)                                   *
00079 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00080 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00081 *  DEVIATION CODE                                                *
00082 *  ISSUE AGE                                                     *
00083 *  ORIGINAL BENEFIT AMOUNT                                       *
00084 *  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
00085 *  PROCESS TYPE (ISSUE OR CANCEL)                                *
00086 *  BENEFIT KIND (LIFE OR A/H)                                    *
00087 *  A.P.R.                                                        *
00088 *  METHOD
00089 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00090 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00091 *  COMPANY I.D. (3 CHARACTER)                                    *
00092 *  BENEFIT CODE                                                  *
00093 *  BENEFIT OVERRIDE CODE                                         *
00094 *  MAXIMUM MONTHLY BENEFIT (FROM ACCT MASTER - CSL ONLY)         *
00095 *  MAXIMUM TOTAL BENEFIT (FROM ACCT MASTER - CSL ONLY)           *
00096 *  JOINT INDICATOR (CSL ONLY)                                    *
00097 *  FIRST PAYMENT DATE (CSL ONLY)                                 *
00098 *  PERIODIC PAYMENT AMOUNT (IN CP-REMAINING-TERM - CSL ONLY)     *
00099 *                                                                *
00100 *  RETURNED FROM ELRATE                                          *
00101 *  --------------------                                          *
00102 *  CALCULATED PREMIUM                                            *
00103 *  PREMIUM RATE                                                  *
00104 *  MORTALITY CODE                                                *
00105 *  MAX ATTAINED AGE                                              *
00106 *  MAX AGE                                                       *
00107 *  MAX TERM                                                      *
00108 *  MAX MONTHLY BENEFIT                                           *
00109 *  MAX TOTAL BENIFIT                                             *
00110 *  COMPOSITE RATE (OPEN-END ONLY)                                *
00111 *----------------------------------------------------------------*
00112 *  PASSED TO ELRFND                                              *
00113 *  ----------------                                              *
00114 *  CERT ISSUE DATE                                               *
00115 *  REFUND DATE                                                   *
00116 *  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
00117 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00118 *  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
00119 *  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
00120 *  STATE CODE (CLIENT DEFINED)                                   *
00121 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00122 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00123 *  DEVIATION CODE                                                *
00124 *  ISSUE AGE                                                     *
00125 *  ORIGINAL BENEFIT AMOUNT                                       *
00126 *  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
00127 *  PROCESS TYPE (CANCEL)                                         *
00128 *  BENEFIT KIND (LIFE OR A/H)                                    *
00129 *  A.P.R.                                                        *
00130 *  EARNING METHOD - (CODE FROM BENEFIT, STATE OR ACCOUNT RECORD) *
00131 *  RATING METHOD -  (CODE FROM BENEFIT)                          *
00132 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00133 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00134 *  COMPANY I.D. (3 CHARACTER)                                    *
00135 *  BENEFIT CODE                                                  *
00136 *  BENEFIT OVERRIDE CODE                                         *
00137 *                                                                *
00138 *  RETURNED FROM ELRFND                                          *
00139 *  --------------------                                          *
00140 *  CALCULATED REFUND                                             *
00141 *----------------------------------------------------------------*
00142 *  PASSED TO ELEARN                                              *
00143 *  ----------------                                              *
00144 *  CERT ISSUE DATE                                               *
00145 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00146 *  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
00147 *  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
00148 *  STATE CODE (CLIENT DEFINED)                                   *
00149 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00150 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00151 *  DEVIATION CODE                                                *
00152 *  ISSUE AGE                                                     *
00153 *  ORIGINAL BENEFIT AMOUNT                                       *
00154 *  BENEFIT KIND (LIFE OR A/H)                                    *
00155 *  A.P.R.                                                        *
00156 *  METHOD - (EARNING CODE FROM BENEFIT RECORD)                   *
00157 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00158 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00159 *  COMPANY I.D. (3 CHARACTER)                                    *
00160 *  BENEFIT CODE                                                  *
00161 *  BENEFIT OVERRIDE CODE                                         *
00162 *                                                                *
00163 *  RETURNED FROM ELEARN                                          *
00164 *  --------------------                                          *
00165 *  INDICATED  EARNINGS                                           *
00166 *----------------------------------------------------------------*
00167 *                 LENGTH = 450                                   *
00168 *                                                                *
00169 ******************************************************************
010303******************************************************************
010303*                   C H A N G E   L O G
010303*
010303* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
010303*-----------------------------------------------------------------
010303*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
010303* EFFECTIVE    NUMBER
010303*-----------------------------------------------------------------
010303* 010303    2001061800003  PEMA  ADD DCC/MONTHLY PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
101807* 101807    2007100100007  PEMA  EXPAND CLM RESERVE FIELDS
010410* 010410    2008021200005  PEMA  ADD FIELDS FOR MN NET PAY BALLOON
010410* 010410    2009050700003  PEMA  ADD FIELDS FOR SPP-DD
041310* 041310  CR2008021200005  PEMA  ADD CODE FOR MN LEVEL
041710* 041710    2007111300001  AJRA  ADD CLAIM CALC SW FOR SC NP+6
101110* 101110  CR2010012700001  PEMA ADD DDF REFUND/UEP PROCESSING
071211* 071211  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
010303******************************************************************
00170
00171  01  CALCULATION-PASS-AREA.
00172      12  CP-COMM-LENGTH            PIC S9(4)         VALUE +450
00173                                      COMP.
00174
00175      12  CP-RETURN-CODE            PIC X             VALUE ZERO.
00176        88  NO-CP-ERROR                             VALUE ZERO.
00177        88  CP-ERROR-OCCURED VALUE '1' '2' '3' '4' '5' '6' '7' '8'
00178                                   '9' 'A' 'B' 'C' 'D' 'E' 'H'.
00179        88  CP-ERROR-IN-AMOUNTS                     VALUE '1'.
00180        88  CP-ERROR-IN-DATES                       VALUE '2'.
00181        88  CP-ERROR-IN-OPTIONS                     VALUE '3'.
00182        88  CP-ERROR-IN-TERMS                       VALUE '4'.
00183        88  CP-ERROR-IN-FREQUENCY                   VALUE '5'.
00184        88  CP-ERROR-RATE-NOT-FOUND                 VALUE '6'.
00185        88  CP-ERROR-RATE-IS-ZERO                   VALUE '7'.
00186        88  CP-ERROR-AMT-OUTSIDE-LIMIT              VALUE '8'.
00187        88  CP-ERROR-TERM-OUTSIDE-LIMIT             VALUE '9'.
00188        88  CP-ERROR-AGE-OUTSIDE-LIMIT              VALUE 'A'.
00189        88  CP-ERROR-ATT-OUTSIDE-LIMIT              VALUE 'B'.
00190        88  CP-ERROR-TOT-OUTSIDE-LIMIT              VALUE 'C'.
00191        88  CP-ERROR-RATE-FILE-NOTOPEN              VALUE 'D'.
00192        88  CP-ERROR-ISSUE-AGE-ZERO                 VALUE 'E'.
00193        88  CP-ERROR-NO-LIMITS-CRI                  VALUE 'F'.
00194        88  CP-ERROR-DIV-BY-ZERO                    VALUE 'G'.
00195        88  CP-ERROR-LOAN-TERM                      VALUE 'H'.
00196
00197      12  CP-RETURN-CODE-2          PIC X             VALUE ZERO.
00198        88  NO-CP-ERROR-2                           VALUE ZERO.
00199 ***********************  INPUT AREAS ****************************
00200
00201      12  CP-CALCULATION-AREA.
00202          16  CP-ACCOUNT-NUMBER     PIC X(10)       VALUE SPACES.
00203          16  CP-CERT-EFF-DT        PIC XX.
00204          16  CP-VALUATION-DT       PIC XX.
00205          16  CP-PAID-THRU-DT       PIC XX.
00206          16  CP-BENEFIT-TYPE       PIC X.
00207            88  CP-AH                               VALUE 'A' 'D'
00208                                                    'I' 'U'.
00209            88  CP-REDUCING-LIFE                    VALUE 'R'.
00210            88  CP-LEVEL-LIFE                       VALUE 'L' 'P'.
00211          16  CP-INCURRED-DT        PIC XX.
00212          16  CP-REPORTED-DT        PIC XX.
00213          16  CP-ACCT-FLD-5         PIC XX            VALUE SPACE.
00214          16  CP-COMPANY-ID         PIC XXX           VALUE SPACE.
00215          16  CP-ISSUE-AGE          PIC S9(3)         VALUE ZERO
00216                                      COMP-3.
00217          16  CP-CDT-PERCENT        PIC S9(3)V99      VALUE ZERO
00218                                      COMP-3.
00219          16  CP-CDT-METHOD         PIC X.
00220            88  CP-CDT-ROUND-NEAR                   VALUE '1'.
00221            88  CP-CDT-ROUND-HIGH                   VALUE '2'.
00222            88  CP-CDT-INTERPOLATED                 VALUE '3'.
00223          16  CP-CLAIM-TYPE         PIC X.
00224            88  CP-AH-CLAIM                         VALUE 'A'.
00225            88  CP-LIFE-CLAIM                       VALUE 'L'.
00226          16  CP-ORIGINAL-TERM      PIC S9(3)         VALUE ZERO
00227                                      COMP-3.
00228          16  CP-ORIGINAL-BENEFIT   PIC S9(9)V99      VALUE ZERO
00229                                      COMP-3.
00230          16  CP-ORIGINAL-PREMIUM   PIC S9(7)V99      VALUE ZERO
00231                                      COMP-3.
00232          16  CP-REMAINING-TERM     PIC S9(3)V99      VALUE ZERO
00233                                      COMP-3.
00234          16  CP-REMAINING-BENEFIT  PIC S9(9)V99      VALUE ZERO
00235                                      COMP-3.
00236          16  CP-LOAN-APR           PIC S9(3)V9(4)    VALUE ZERO
00237                                      COMP-3.
00238          16  CP-PAY-FREQUENCY      PIC S9(3)         VALUE ZERO
00239                                      COMP-3.
00240          16  CP-REM-TERM-METHOD    PIC X.
00241            88  CP-EARN-AFTER-15TH                  VALUE '1'.
00242            88  CP-EARN-ON-HALF-MONTH               VALUE '2'.
00243            88  CP-EARN-ON-1ST-DAY                  VALUE '3'.
00244            88  CP-EARN-ON-FULL-MONTH               VALUE '4'.
00245            88  CP-EARN-WITH-NO-DAYS                VALUE '5'.
00246            88  CP-EARN-AFTER-14TH                  VALUE '6'.
00247            88  CP-EARN-AFTER-16TH                  VALUE '7'.
00248          16  CP-EARNING-METHOD     PIC X.
00249            88  CP-EARN-BY-R78                      VALUE '1' 'R'.
00250            88  CP-EARN-BY-PRORATA                  VALUE '2' 'P'.
00251            88  CP-EARN-AS-CALIF                    VALUE '3' 'C'.
00252            88  CP-EARN-AS-TEXAS                    VALUE '4' 'T'.
00253            88  CP-EARN-AS-FARM-PLAN                VALUE '4' 'T'.
00254            88  CP-EARN-AS-NET-PAY                  VALUE '5' 'N'.
00255            88  CP-EARN-ANTICIPATION                VALUE '6' 'A'.
00256            88  CP-EARN-AS-MEAN                     VALUE '8' 'M'.
00257            88  CP-EARN-AS-SUM-OF-DIGITS            VALUE '9'.
00258            88  CP-EARN-AS-REG-BALLOON              VALUE 'B'.
033104           88  CP-GAP-NON-REFUNDABLE               VALUE 'G'.
033104           88  CP-GAP-ACTUARIAL                    VALUE 'S'.
092310           88  CP-DCC-SPP-DDF                      VALUE 'D' 'I'.
                 88  CP-DCC-SPP-DDF-IU                   VALUE 'I'.
00259          16  CP-PROCESS-TYPE       PIC X.
00260            88  CP-CLAIM                            VALUE '1'.
00261            88  CP-CANCEL                           VALUE '2'.
00262            88  CP-ISSUE                            VALUE '3'.
00263          16  CP-SPECIAL-CALC-CD    PIC X.
00264            88  CP-OUTSTANDING-BAL              VALUE 'O'.
00265            88  CP-1-MTH-INTEREST               VALUE ' '.
00266            88  CP-0-MTH-INTEREST               VALUE 'A'.
00267            88  CP-OB-OFFLINE-RESERVED          VALUE 'B'.
00268            88  CP-CRITICAL-PERIOD              VALUE 'C'.
00269            88  CP-TERM-IS-DAYS                 VALUE 'D'.
00270            88  CP-USE-PREM-AS-ENTERED          VALUE 'E'.
00271            88  CP-FARM-PLAN                    VALUE 'F'.
00272            88  CP-RATE-AS-STANDARD             VALUE 'G'.
00273            88  CP-2-MTH-INTEREST               VALUE 'I'.
00274            88  CP-3-MTH-INTEREST               VALUE 'J'.
00275            88  CP-4-MTH-INTEREST               VALUE 'K'.
00276            88  CP-BALLOON-LAST-PMT             VALUE 'L'.
00277            88  CP-MORTGAGE-REC                 VALUE 'M'.
00278            88  CP-OUTSTANDING-BALANCE          VALUE 'O'.
00279            88  CP-NET-PAY-PRUDENTIAL           VALUE 'P'.
00280            88  CP-NET-PAY-SIMPLE               VALUE 'S'.
00281            88  CP-TRUNCATED-LIFE               VALUE 'T' 'U' 'V'
00282                                                      'W' 'X'.
00283            88  CP-TRUNCATE-0-MTH               VALUE 'T'.
00284            88  CP-TRUNCATE-1-MTH               VALUE 'U'.
00285            88  CP-TRUNCATE-2-MTH               VALUE 'V'.
00286            88  CP-TRUNCATE-3-MTH               VALUE 'W'.
00287            88  CP-TRUNCATE-4-MTH               VALUE 'X'.
00288            88  CP-SUMMARY-REC                  VALUE 'Z'.
00289            88  CP-PROPERTY-BENEFIT             VALUE '2'.
00290            88  CP-UNEMPLOYMENT-BENEFIT         VALUE '3'.
00291            88  CP-AD-D-BENEFIT                 VALUE '4'.
00292            88  CP-CSL-METH-1                   VALUE '5'.
00293            88  CP-CSL-METH-2                   VALUE '6'.
00294            88  CP-CSL-METH-3                   VALUE '7'.
00295            88  CP-CSL-METH-4                   VALUE '8'.
00296
00297          16  CP-LOAN-TERM          PIC S9(3)       VALUE ZERO
00298                                      COMP-3.
00299          16  CP-CLASS-CODE         PIC XX          VALUE ZERO.
00300          16  CP-DEVIATION-CODE     PIC XXX         VALUE ZERO.
00301          16  CP-STATE              PIC XX          VALUE SPACE.
00302          16  CP-STATE-STD-ABBRV    PIC XX          VALUE SPACE.
00303          16  CP-BENEFIT-CD         PIC XX          VALUE ZERO.
00304            88  CP-CSL-VALID-NP-BENEFIT-CD VALUES '12' '13'
00305                '34' '35' '36' '37' '44' '45' '46' '47' '72' '73'.
00306          16  CP-R78-OPTION         PIC X.
00307            88  CP-TERM-TIMES-TERM-PLUS-1           VALUE ' '.
00308            88  CP-TERM-TIMES-TERM                  VALUE '1'.
00309
00310          16  CP-COMPANY-CD         PIC X             VALUE SPACE.
00311          16  CP-IBNR-RESERVE-SW    PIC X.
00312          16  CP-CLAIM-STATUS       PIC X.
00313          16  CP-RATE-FILE          PIC X.
00314          16  CP-TERM-OR-EXT-DAYS   PIC S9(05)        VALUE ZERO
00315                                      COMP-3.
00316
00317          16  CP-LIFE-OVERRIDE-CODE PIC X.
00318          16  CP-AH-OVERRIDE-CODE   PIC X.
00319
00320          16  CP-RATE-DEV-PCT       PIC S9V9(6)       VALUE ZERO
00321                                      COMP-3.
               16  CP-CLP-RATE-UP        REDEFINES CP-RATE-DEV-PCT
                                         PIC S9(5)V99 COMP-3.
00322          16  CP-CRITICAL-MONTHS    PIC S9(3)         VALUE ZERO
00323                                      COMP-3.
00324          16  CP-ALTERNATE-BENEFIT  PIC S9(9)V99      VALUE ZERO
00325                                      COMP-3.
00326          16  CP-ALTERNATE-PREMIUM  PIC S9(7)V99      VALUE ZERO
00327                                      COMP-3.
               16  CP-DDF-CSO-ADMIN-FEE REDEFINES CP-ALTERNATE-PREMIUM
                                        PIC S9(7)V99 COMP-3.
00328
00329          16  CP-PAID-FROM-DATE     PIC X(02).
00330          16  CP-CLAIM-CALC-METHOD  PIC X(01).
00331          16  CP-EXT-DAYS-CALC      PIC X.
00332            88  CP-EXT-NO-CHG                   VALUE ' '.
00333            88  CP-EXT-CHG-LF                   VALUE '1'.
00334            88  CP-EXT-CHG-AH                   VALUE '2'.
00335            88  CP-EXT-CHG-LF-AH                VALUE '3'.
00336          16  CP-DOMICILE-STATE     PIC XX.
00337          16  CP-CARRIER            PIC X.
00338          16  CP-REIN-FLAG          PIC X.
00339          16  CP-REM-TRM-CALC-OPTION PIC X.
00340            88  VALID-REM-TRM-CALC-OPTION    VALUE '1'
00341                       '2' '3' '4' '5'.
00342            88  CP-CALC-OPTION-DEFAULT       VALUE '4'.
00343            88  CP-CONSIDER-EXTENSION        VALUE '3' '4' '5'.
00344            88  CP-30-DAY-MONTH              VALUE '1' '3' '5'.
00345            88  CP-NO-EXT-30-DAY-MONTH       VALUE '1'.
00346            88  CP-NO-EXT-ACTUAL-DAYS        VALUE '2'.
00347            88  CP-EXT-30-DAY-MONTH          VALUE '3'.
00348            88  CP-EXT-ACTUAL-DAYS           VALUE '4'.
                 88  CP-USE-EXP-AND-1ST-PMT       VALUE '5'.
00349          16  CP-SIG-SWITCH         PIC X.
00350          16  CP-RATING-METHOD      PIC X.
00351            88  CP-RATE-AS-R78                      VALUE '1' 'R'.
00352            88  CP-RATE-AS-PRORATA                  VALUE '2' 'P'.
00353            88  CP-RATE-AS-CALIF                    VALUE '3' 'C'.
00354            88  CP-RATE-AS-TEXAS                    VALUE '4' 'T'.
00355            88  CP-RATE-AS-FARM-PLAN                VALUE '4' 'T'.
00356            88  CP-RATE-AS-NET-PAY                  VALUE '5' 'N'.
00357            88  CP-RATE-AS-ANTICIPATION             VALUE '6' 'A'.
00358            88  CP-RATE-AS-MEAN                     VALUE '8' 'M'.
00359            88  CP-RATE-AS-REG-BALLOON              VALUE 'B'.
00360          16  CP-SALES-TAX          PIC S9V9999     VALUE  ZEROS
00361                                      COMP-3.
090803         16  CP-BEN-CATEGORY       PIC X.
011904         16  CP-DCC-LF-RATE        PIC S99V9(5) COMP-3 VALUE +0.
               16  CP-DCC-ACT-COMM REDEFINES CP-DCC-LF-RATE
                                         PIC S99V9(5) COMP-3.
011904         16  CP-DCC-AH-RATE        PIC S99V9(5) COMP-3 VALUE +0.
               16  CP-DCC-PMF-COMM REDEFINES CP-DCC-AH-RATE
                                         PIC S99V9(5) COMP-3.
080305         16  CP-DAYS-TO-1ST-PMT    PIC S999     COMP-3 VALUE +0.
               16  CP-AH-BALLOON-SW      PIC X  VALUE ' '.
041310         16  CP-EXPIRE-DT          PIC XX.
041710         16  CP-LF-CLAIM-CALC-SW   PIC X  VALUE ' '.
               16  CP-DDF-HI-FACT        PIC S9V999   COMP-3 VALUE +0.
               16  CP-DDF-LO-FACT        PIC S9V999   COMP-3 VALUE +0.
               16  CP-DDF-CLP            PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-DDF-SPEC-CALC      PIC X.
                   88  CP-CALC-GROSS-FEE        VALUE 'G'.
                   88  CP-CALC-CLP              VALUE 'C'.
               16  CP-IU-RATE-UP         PIC S9(5)V99   COMP-3 VALUE +0.
               16  CP-CANCEL-REASON      PIC X.
               16  CP-DDF-ADMIN-FEES     PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-PMT-MODE           PIC X.
               16  CP-NO-OF-PMTS         PIC S999 COMP-3 VALUE +0.
071211         16  CP-1ST-YR-ALLOW       PIC S999V99 COMP-3 VALUE +0.
               16  CP-DDF-COMM-AND-MFEE  PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-DDF-YR1AF          PIC S9(5)V99 COMP-3 VALUE +0.
071211         16  FILLER                PIC X.
00363
00364 ***************    OUTPUT FROM ELRESV   ************************
00365
00366          16  CP-CDT-TABLE          PIC 9             VALUE ZERO.
00367
00368          16  CP-CDT-FACTOR         PIC S9(5)V9(6)    VALUE ZERO
00369                                      COMP-3.
101807         16  CP-PTC-RESERVE        PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  CP-IBNR-RESERVE       PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  CP-FUTURE-RESERVE     PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  FILLER                PIC X(09).
00377 ***************    OUTPUT FROM ELRTRM   *************************
00378
00379          16  CP-REMAINING-TERM-1   PIC S9(4)V9    VALUE ZERO
00380                                      COMP-3.
00381          16  CP-REMAINING-TERM-2   PIC S9(4)V9    VALUE ZERO
00382                                      COMP-3.
00383          16  CP-REMAINING-TERM-3   PIC S9(4)V9    VALUE ZERO
00384                                      COMP-3.
00385          16  CP-ODD-DAYS           PIC S9(3)      VALUE ZERO
00386                                      COMP-3.
00387          16  FILLER                PIC X(12).
00388
00389 ***************    OUTPUT FROM ELRAMT   *************************
00390
00391          16  CP-REMAINING-AMT      PIC S9(9)V99   VALUE ZERO
00392                                      COMP-3.
00393          16  CP-REMAINING-AMT-PRV  PIC S9(9)V99   VALUE ZERO
00394                                      COMP-3.
00395          16  FILLER                PIC X(12).
00396
00397 ***************    OUTPUT FROM ELRATE   *************************
00398
00399          16  CP-CALC-PREMIUM       PIC S9(7)V99   VALUE ZERO
00400                                      COMP-3.
00401          16  CP-PREMIUM-RATE       PIC S9(2)V9(5) VALUE ZERO
00402                                      COMP-3.
00403          16  CP-MORTALITY-CODE     PIC X(4).
00404          16  CP-RATE-EDIT-FLAG     PIC X.
00405              88  CP-RATES-NEED-APR                  VALUE '1'.
00406          16  CP-COMPOSITE-RATE     PIC S99V999    VALUE ZERO
00407                                      COMP-3.
00408          16  CP-POLICY-FEE         PIC S9(3)V99 VALUE +0 COMP-3.
032905         16  CP-LF-PREM            PIC S9(7)V99 VALUE +0 COMP-3.
               16  CP-LF-BALLOON-PREM REDEFINES CP-LF-PREM
                                         PIC S9(7)V99 COMP-3.
00409          16  FILLER                PIC X(07).
00410
00411 ***************    OUTPUT FROM ELRFND   *************************
00412
00413          16  CP-CALC-REFUND        PIC S9(7)V99   VALUE ZERO
00414                                      COMP-3.
00415          16  CP-REFUND-TYPE-USED   PIC X.
00416            88  CP-R-AS-R78                         VALUE '1'.
00417            88  CP-R-AS-PRORATA                     VALUE '2'.
00418            88  CP-R-AS-CALIF                       VALUE '3'.
00419            88  CP-R-AS-TEXAS                       VALUE '4'.
00420            88  CP-R-AS-FARM-PLAN                   VALUE '4'.
00421            88  CP-R-AS-NET-PAY                     VALUE '5'.
00422            88  CP-R-AS-ANTICIPATION                VALUE '6'.
00423            88  CP-R-AS-MEAN                        VALUE '8'.
00424            88  CP-R-AS-SUM-OF-DIGITS               VALUE '9'.
033104           88  CP-R-AS-GAP-NON-REFUND              VALUE 'G'.
033104           88  CP-R-AS-GAP-ACTUARIAL               VALUE 'S'.
092310           88  CP-R-AS-SPP-DDF                     VALUE 'D'.
092310           88  CP-R-AS-SPP-DDF-IU                  VALUE 'I'.
                 88  CP-R-AS-REPOSSESSION                VALUE 'R'.
00425          16  FILLER                PIC X(12).
00426
00427 ***************    OUTPUT FROM ELEARN   *************************
00428
00429          16  CP-R78-U-PRM          PIC S9(7)V99   VALUE ZERO
00430                                      COMP-3.
00431          16  CP-R78-U-PRM-ALT      PIC S9(7)V99   VALUE ZERO
00432                                      COMP-3.
00433          16  CP-PRORATA-U-PRM      PIC S9(7)V99   VALUE ZERO
00434                                      COMP-3.
00435          16  CP-PRORATA-U-PRM-ALT  PIC S9(7)V99   VALUE ZERO
00436                                      COMP-3.
00437          16  CP-STATE-U-PRM        PIC S9(7)V99   VALUE ZERO
00438                                      COMP-3.
00439          16  CP-DOMICILE-U-PRM     PIC S9(7)V99   VALUE ZERO
00440                                      COMP-3.
00441          16  CP-EARNING-TYPE-USED  PIC X.
00442            88  CP-E-AS-SPECIAL                     VALUE 'S'.
00443            88  CP-E-AS-R78                         VALUE '1'.
00444            88  CP-E-AS-PRORATA                     VALUE '2'.
00445            88  CP-E-AS-TEXAS                       VALUE '4'.
00446            88  CP-E-AS-FARM-PLAN                   VALUE '4'.
00447            88  CP-E-AS-NET-PAY                     VALUE '5'.
00448            88  CP-E-AS-ANTICIPATION                VALUE '6'.
00449            88  CP-E-AS-MEAN                        VALUE '8'.
00450            88  CP-E-AS-SUM-OF-DIGITS               VALUE '9'.
00451          16  FILLER                PIC X(12).
00452
00453 ***************    OUTPUT FROM ELPMNT   *************************
00454
00455          16  CP-ACTUAL-DAYS        PIC S9(05)     VALUE ZERO
00456                                      COMP-3.
00457          16  CP-CLAIM-PAYMENT      PIC S9(7)V99   VALUE ZERO
00458                                      COMP-3.
00459          16  FILLER                PIC X(12).
00460
00461 ***************   MISC WORK AREAS    *****************************
00462          16  CP-TOTAL-PAID         PIC S9(7)V99   VALUE ZERO
00463                                      COMP-3.
00464          16  CP-R-MAX-ATT-AGE      PIC S9(3)      VALUE ZERO
00465                                      COMP-3.
00466          16  CP-R-MAX-AGE          PIC S9(3)      VALUE ZERO
00467                                      COMP-3.
00468          16  CP-R-MAX-TERM         PIC S9(5)      VALUE ZERO
00469                                      COMP-3.
00470          16  CP-R-MAX-TOT-BEN      PIC S9(7)V99   VALUE ZERO
00471                                      COMP-3.
00472          16  CP-R-MAX-MON-BEN      PIC S9(7)V99   VALUE ZERO
00473                                      COMP-3.
00474          16  CP-IO-FUNCTION        PIC X          VALUE SPACE.
00475              88  OPEN-RATE-FILE                   VALUE 'O'.
00476              88  CLOSE-RATE-FILE                  VALUE 'C'.
00477              88  IO-ERROR                         VALUE 'E'.
00478
00479          16  CP-FIRST-PAY-DATE     PIC XX.
00480
00481          16  CP-JOINT-INDICATOR    PIC X.
00482
00483          16  CP-RESERVE-REMAINING-TERM
00484                                    PIC S9(4)V9    VALUE ZERO
00485                                      COMP-3.
00486
00487          16  CP-INSURED-BIRTH-DT   PIC XX.
00488
00489          16  CP-INCURRED-AGE       PIC S9(3)      VALUE ZERO
00490                                      COMP-3.
00491
00492          16  CP-MONTHLY-PAYMENT    PIC S9(5)V99   VALUE ZERO
00493                                      COMP-3.
00494
00495          16  CP-RATING-BENEFIT-AMT PIC S9(9)V99   VALUE ZERO
00496                                      COMP-3.
00497
00498          16  CP-ODD-DAYS-TO-PMT    PIC S9(3)      VALUE ZERO
00499                                      COMP-3.
00500
00501          16  CP-MNTHS-TO-FIRST-PMT PIC S9(3)      VALUE ZERO
00502                                      COMP-3.
00503
00504          16  CP-REMAMT-FACTOR      PIC S9(4)V9(9) VALUE ZEROS
00505                                      COMP-3.
00506
00507          16  CP-FREE-LOOK          PIC S9(3)      VALUE ZERO
00508                                      COMP-3.
00509
00510          16  CP-ROA-REFUND         PIC X          VALUE 'N'.
00511              88  CP-ROA-PREM-AT-REFUND            VALUE 'Y'.
00512
010303         16  CP-NET-BENEFIT-AMT    PIC S9(9)V99   VALUE ZERO
010303                                     COMP-3.
041710         16  CP-SCNP-6MO-AMT       PIC S9(9)V99   VALUE ZERO
041710                                     COMP-3.
               16  CP-MONTH              PIC S999     COMP-3 VALUE +0.
041710         16  FILLER                PIC X(15).
00514 ******************************************************************
00312
00313      EJECT
00314 *                                    COPY ELCDATE.
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
00315
00316      EJECT
00317 *                                    COPY ELCEMIB.
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
00318
00319      EJECT
00320 *                                    COPY ELCLOGOF.
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
00321
00322      EJECT
00323 *                                    COPY ELCATTR.
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
00324
00325      EJECT
00326 *                                    COPY ELCAID.
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
00327
00328  01  FILLER REDEFINES DFHAID.
00329      05  FILLER                      PIC X(8).
00330
00331      05  PF-VALUES                   PIC X
00332          OCCURS 24 TIMES.
00333
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
00335
00336  01  DFHCOMMAREA                     PIC X(1024).
00337
00338 *01 PARMLIST   COMP SYNC.
00339 *    05  FILLER                      PIC S9(9).
00340 *    05  ELCERT-POINTER              PIC S9(9).
00341 *    05  ELCNTL-POINTER              PIC S9(9).
00342
00343      EJECT
00344 *                                    COPY ELCCERT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCCERT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.013                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CERTIFICATE MASTER                        *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 450  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCERT                         RKP=2,LEN=33   *
00013 *       ALTERNATE PATH1 = ELCERT2 (BY NAME)       RKP=35,LEN=18  *
00014 *       ALTERNATE PATH2 = ELCERT3 (BY SOC SEC NO) RKP=53,LEN=12  *
00015 *       ALTERNATE PATH3 = ELCERT5 (BY CERT NO.)   RKP=65,LEN=12  *
00016 *       ALTERNATE PATH4 = ELCERT6 (BY MEMBER NO.) RKP=77,LEN=13  *
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
040504* 040504  CR2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
061405* 061405  CR2005060300001  PEMA  ADD CLP STATE PROCESS FOR DCC
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
102109* 102109  CR2008100900003  AJRA  ADD CLAIM CERT NOTE IND
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
032612* 032612  CR2011110200001  PEMA  AHL CHANGES
122002******************************************************************
00021
00022  01  CERTIFICATE-MASTER.
00023      12  CM-RECORD-ID                      PIC XX.
00024          88  VALID-CM-ID                      VALUE 'CM'.
00025
00026      12  CM-CONTROL-PRIMARY.
00027          16  CM-COMPANY-CD                 PIC X.
00028          16  CM-CARRIER                    PIC X.
00029          16  CM-GROUPING.
00030              20  CM-GROUPING-PREFIX        PIC X(3).
00031              20  CM-GROUPING-PRIME         PIC X(3).
00032          16  CM-STATE                      PIC XX.
00033          16  CM-ACCOUNT.
00034              20  CM-ACCOUNT-PREFIX         PIC X(4).
00035              20  CM-ACCOUNT-PRIME          PIC X(6).
00036          16  CM-CERT-EFF-DT                PIC XX.
00037          16  CM-CERT-NO.
00038              20  CM-CERT-PRIME             PIC X(10).
00039              20  CM-CERT-SFX               PIC X.
00040
00041      12  CM-CONTROL-BY-NAME.
00042          16  CM-COMPANY-CD-A1              PIC X.
00043          16  CM-INSURED-LAST-NAME          PIC X(15).
00044          16  CM-INSURED-INITIALS.
00045              20  CM-INSURED-INITIAL1       PIC X.
00046              20  CM-INSURED-INITIAL2       PIC X.
00047
00048      12  CM-CONTROL-BY-SSN.
00049          16  CM-COMPANY-CD-A2              PIC X.
00050          16  CM-SOC-SEC-NO.
00051              20  CM-SSN-STATE              PIC XX.
00052              20  CM-SSN-ACCOUNT            PIC X(6).
00053              20  CM-SSN-LN3.
00054                  25  CM-INSURED-INITIALS-A2.
00055                      30 CM-INSURED-INITIAL1-A2   PIC X.
00056                      30 CM-INSURED-INITIAL2-A2   PIC X.
00057                  25 CM-PART-LAST-NAME-A2         PIC X.
00058
00059      12  CM-CONTROL-BY-CERT-NO.
00060          16  CM-COMPANY-CD-A4              PIC X.
00061          16  CM-CERT-NO-A4                 PIC X(11).
00062
00063      12  CM-CONTROL-BY-MEMB.
00064          16  CM-COMPANY-CD-A5              PIC X.
00065          16  CM-MEMBER-NO.
00066              20  CM-MEMB-STATE             PIC XX.
00067              20  CM-MEMB-ACCOUNT           PIC X(6).
00068              20  CM-MEMB-LN4.
00069                  25  CM-INSURED-INITIALS-A5.
00070                      30 CM-INSURED-INITIAL1-A5   PIC X.
00071                      30 CM-INSURED-INITIAL2-A5   PIC X.
00072                  25 CM-PART-LAST-NAME-A5         PIC XX.
00073
00074      12  CM-INSURED-PROFILE-DATA.
00075          16  CM-INSURED-FIRST-NAME.
00076              20  CM-INSURED-1ST-INIT       PIC X.
00077              20  FILLER                    PIC X(9).
00078          16  CM-INSURED-ISSUE-AGE          PIC 99.
00079          16  CM-INSURED-SEX                PIC X.
00080              88  CM-SEX-MALE                  VALUE 'M'.
00081              88  CM-SEX-FEMAL                 VALUE 'F'.
00082          16  CM-INSURED-JOINT-AGE          PIC 99.
00083          16  CM-JOINT-INSURED-NAME.
00084              20  CM-JT-LAST-NAME           PIC X(15).
00085              20  CM-JT-FIRST-NAME.
00086                  24  CM-JT-1ST-INIT        PIC X.
00087                  24  FILLER                PIC X(9).
00088              20  CM-JT-INITIAL             PIC X.
00089
00090      12  CM-LIFE-DATA.
00091          16  CM-LF-BENEFIT-CD              PIC XX.
00092          16  CM-LF-ORIG-TERM               PIC S999      COMP-3.
00093          16  CM-LF-CRITICAL-PERIOD         PIC S999      COMP-3.
00094          16  CM-LF-TERM-IN-DAYS            PIC S9(5)     COMP-3.
00095          16  CM-LF-DEV-CODE                PIC XXX.
00096          16  CM-LF-DEV-PCT                 PIC S9V9(6)   COMP-3.
00097          16  CM-LF-BENEFIT-AMT             PIC S9(9)V99  COMP-3.
00098          16  CM-LF-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00099          16  CM-LF-ALT-BENEFIT-AMT         PIC S9(9)V99  COMP-3.
00100          16  CM-LF-ALT-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00101          16  CM-LF-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00102          16  CM-LF-REMAINING-AMT           PIC S9(9)V99  COMP-3.
00103          16  CM-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00104          16  CM-LF-ITD-DEATH-AMT           PIC S9(9)V99  COMP-3.
00105          16  CM-LF-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
00106          16  CM-LF-POLICY-FEE              PIC S9(3)V99  COMP-3.
00107          16  CM-LF-ALT-PREMIUM-RATE        PIC S99V9(5)  COMP-3.
00108          16  FILLER                        PIC XX.
00109
00110      12  CM-AH-DATA.
00111          16  CM-AH-BENEFIT-CD              PIC XX.
00112          16  CM-AH-ORIG-TERM               PIC S999      COMP-3.
00113          16  CM-AH-CRITICAL-PERIOD         PIC S999      COMP-3.
00114          16  CM-AH-DEV-CODE                PIC XXX.
00115          16  CM-AH-DEV-PCT                 PIC S9V9(6)   COMP-3.
00116          16  CM-AH-BENEFIT-AMT             PIC S9(7)V99  COMP-3.
00117          16  CM-AH-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00118          16  CM-AH-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00119          16  CM-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00120          16  CM-AH-ITD-LUMP-PMT            PIC S9(7)V99  COMP-3.
00121          16  CM-AH-ITD-AH-PMT              PIC S9(9)V99  COMP-3.
00122          16  CM-AH-PAID-THRU-DT            PIC XX.
00123              88  NO-AH-CLAIMS-PAID            VALUE LOW-VALUE.
00124          16  CM-AH-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
00125          16  CM-AH-POLICY-FEE              PIC S9(3)V99  COMP-3.
00126          16  CM-AH-CEDED-BENEFIT           PIC S9(7)V99  COMP-3.
00127          16  FILLER                        PIC X.
00128
00129      12  CM-LOAN-INFORMATION.
00130          16  CM-LIVES                      PIC S9(7)     COMP-3.
011410         16  CM-DDF-IU-RATE-UP REDEFINES CM-LIVES
011410                                           PIC S9(5)V99  COMP-3.
00131          16  CM-BILLED                     PIC S9(7)     COMP-3.
00132          16  CM-LOAN-APR                   PIC S999V9(4) COMP-3.
00133          16  CM-PAY-FREQUENCY              PIC S99.
00134          16  CM-LOAN-TERM                  PIC S999      COMP-3.
00135          16  CM-RATE-CLASS                 PIC XX.
00136          16  CM-BENEFICIARY                PIC X(25).
00137          16  CM-POLICY-FORM-NO             PIC X(12).
00138          16  CM-PMT-EXTENSION-DAYS         PIC S999      COMP-3.
00139          16  CM-LAST-ADD-ON-DT             PIC XX.
00140          16  CM-DEDUCTIBLE-AMOUNTS.
00141              20  CM-CLAIM-DEDUCT-WITHHELD  PIC S9(5)V99  COMP-3.
00142              20  CM-CANCEL-DEDUCT-WITHHELD PIC S9(5)V99  COMP-3.
00143          16  CM-RESIDENT-RATE REDEFINES CM-DEDUCTIBLE-AMOUNTS.
00144              20  CM-RESIDENT-STATE         PIC XX.
00145              20  CM-RATE-CODE              PIC X(4).
00146              20  FILLER                    PIC XX.
110105         16  FILLER REDEFINES CM-DEDUCTIBLE-AMOUNTS.
110105             20  CM-LOAN-OFFICER           PIC X(5).
110105             20  FILLER                    PIC XXX.
00147          16  CM-CSR-CODE                   PIC XXX.
00148          16  CM-UNDERWRITING-CODE          PIC X.
00149              88  CM-POLICY-UNDERWRITTEN       VALUE 'Y'.
081606         16  CM-POST-CARD-IND              PIC X.
072308         16  CM-NH-INTERFACE-SW            PIC X.
00151          16  CM-PREMIUM-TYPE               PIC X.
00152              88  CM-SING-PRM                  VALUE '1'.
00153              88  CM-O-B-COVERAGE              VALUE '2'.
00154              88  CM-OPEN-END                  VALUE '3'.
00155          16  CM-IND-GRP-TYPE               PIC X.
00156              88  CM-INDIVIDUAL                VALUE 'I'.
00157              88  CM-GROUP                     VALUE 'G'.
00158          16  CM-SKIP-CODE                  PIC X.
00159              88  NO-MONTHS-SKIPPED            VALUE SPACE.
00160              88  SKIP-JULY                    VALUE '1'.
00161              88  SKIP-AUGUST                  VALUE '2'.
00162              88  SKIP-SEPTEMBER               VALUE '3'.
00163              88  SKIP-JULY-AUG                VALUE '4'.
00164              88  SKIP-AUG-SEPT                VALUE '5'.
00165              88  SKIP-JULY-AUG-SEPT           VALUE '6'.
00166              88  SKIP-JUNE-JULY-AUG           VALUE '7'.
00167              88  SKIP-JUNE                    VALUE '8'.
00168              88  SKIP-JUNE-JULY               VALUE '9'.
00169              88  SKIP-AUG-SEPT-OCT            VALUE 'A'.
00170              88  SKIP-BI-WEEKLY-3RD-PMT       VALUE 'X'.
00171          16  CM-PAYMENT-MODE               PIC X.
00172              88  PAY-MONTHLY                  VALUE SPACE.
00173              88  PAY-WEEKLY                   VALUE '1'.
00174              88  PAY-SEMI-MONTHLY             VALUE '2'.
00175              88  PAY-BI-WEEKLY                VALUE '3'.
00176              88  PAY-SEMI-ANUALLY             VALUE '4'.
00177          16  CM-LOAN-NUMBER                PIC X(8).
00178          16  CM-LOAN-BALANCE               PIC S9(7)V99  COMP-3.
110105         16  CM-OLD-LOF                    PIC XXX.
00179 *        16  CM-LOAN-OFFICER               PIC XXX.
00180          16  CM-REIN-TABLE                 PIC XXX.
00181          16  CM-SPECIAL-REIN-CODE          PIC X.
00182          16  CM-LF-LOAN-EXPIRE-DT          PIC XX.
00183          16  CM-AH-LOAN-EXPIRE-DT          PIC XX.
00184          16  CM-LOAN-1ST-PMT-DT            PIC XX.
00185
00186      12  CM-STATUS-DATA.
00187          16  CM-ENTRY-STATUS               PIC X.
00188          16  CM-ENTRY-DT                   PIC XX.
00189
00190          16  CM-LF-STATUS-AT-CANCEL        PIC X.
00191          16  CM-LF-CANCEL-DT               PIC XX.
00192          16  CM-LF-CANCEL-EXIT-DT          PIC XX.
00193
00194          16  CM-LF-STATUS-AT-DEATH         PIC X.
00195          16  CM-LF-DEATH-DT                PIC XX.
00196          16  CM-LF-DEATH-EXIT-DT           PIC XX.
00197
00198          16  CM-LF-CURRENT-STATUS          PIC X.
00199              88  CM-LF-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00200                                                 'M' '4' '5' '9'.
00201              88  CM-LF-NORMAL-ENTRY           VALUE '1'.
00202              88  CM-LF-POLICY-PENDING         VALUE '2'.
00203              88  CM-LF-POLICY-IS-RESTORE      VALUE '3'.
00204              88  CM-LF-CONVERSION-ENTRY       VALUE '4'.
00205              88  CM-LF-POLICY-IS-REISSUE      VALUE '5'.
122002             88  CM-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00206              88  CM-LF-LUMP-SUM-DISAB         VALUE '6'.
00207              88  CM-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00208              88  CM-LF-CANCEL-APPLIED         VALUE '8'.
00209              88  CM-LF-IS-REIN-ONLY           VALUE '9'.
00210              88  CM-LF-DECLINED               VALUE 'D'.
00211              88  CM-LF-VOIDED                 VALUE 'V'.
00212
00213          16  CM-AH-STATUS-AT-CANCEL        PIC X.
00214          16  CM-AH-CANCEL-DT               PIC XX.
00215          16  CM-AH-CANCEL-EXIT-DT          PIC XX.
00216
00217          16  CM-AH-STATUS-AT-SETTLEMENT    PIC X.
00218          16  CM-AH-SETTLEMENT-DT           PIC XX.
00219          16  CM-AH-SETTLEMENT-EXIT-DT      PIC XX.
00220
00221          16  CM-AH-CURRENT-STATUS          PIC X.
00222              88  CM-AH-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00223                                                 'M' '4' '5' '9'.
00224              88  CM-AH-NORMAL-ENTRY           VALUE '1'.
00225              88  CM-AH-POLICY-PENDING         VALUE '2'.
00226              88  CM-AH-POLICY-IS-RESTORE      VALUE '3'.
00227              88  CM-AH-CONVERSION-ENTRY       VALUE '4'.
00228              88  CM-AH-POLICY-IS-REISSUE      VALUE '5'.
122002             88  CM-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00229              88  CM-AH-LUMP-SUM-DISAB         VALUE '6'.
00230              88  CM-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00231              88  CM-AH-CANCEL-APPLIED         VALUE '8'.
00232              88  CM-AH-IS-REIN-ONLY           VALUE '9'.
00233              88  CM-AH-DECLINED               VALUE 'D'.
00234              88  CM-AH-VOIDED                 VALUE 'V'.
00235
00236          16  CM-CLAIM-INTERFACE-SW         PIC X.
00237              88  NO-CLAIM-ATTACHED            VALUE SPACE.
00238              88  CERT-AND-CLAIM-ONLINE        VALUE '1'.
00239              88  CERT-WAS-CREATED-FOR-CLAIM   VALUE '2'.
00240          16  CM-CLAIM-ATTACHED-COUNT       PIC S9(4)     COMP.
00241
00242          16  CM-ENTRY-BATCH                PIC X(6).
00243          16  CM-LF-EXIT-BATCH              PIC X(6).
00244          16  CM-AH-EXIT-BATCH              PIC X(6).
00245          16  CM-LAST-MONTH-END             PIC XX.
00246
00247      12  CM-NOTE-SW                        PIC X.
00248          88  CERT-NOTES-ARE-NOT-PRESENT       VALUE ' '.
00249          88  CERT-NOTES-PRESENT               VALUE '1'.
00250          88  BILLING-NOTES-PRESENT            VALUE '2'.
00251          88  CERT-BILLING-NOTES-PRESENT       VALUE '3'.
102109         88  CLAIM-NOTES-PRESENT              VALUE '4'.
102109         88  CLAIM-CERT-NOTES-PRESENT         VALUE '5'.
102109         88  CLAIM-BILLING-NOTES-PRESENT      VALUE '6'.
102109         88  CLAIM-CERT-BILL-NOTES-PRESENT    VALUE '7'.
00252      12  CM-COMP-EXCP-SW                   PIC X.
00253          88  COMPENSATION-SAME-AS-ACCT        VALUE ' '.
00254          88  THIS-CERT-HAS-ERCOMM-ENTRY       VALUE '1'.
00255      12  CM-INSURED-ADDRESS-SW             PIC X.
00256          88  INSURED-ADDR-NOT-PRESENT         VALUE ' '.
00257          88  INSURED-ADDR-PRESENT             VALUE '1'.
00258
011410*    12  CM-LF-CEDED-BENEFIT               PIC S9(7)V99   COMP-3.
011410     12  CM-LF-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
00260
011410*    12  CM-ISS-MICROFILM-NO               PIC S9(9)      COMP-3.
011410     12  CM-AH-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
072308*    12  CM-CAN-MICROFILM-NO               PIC S9(9)      COMP-3.
072308     12  CM-NH-INT-ON-REFS                 PIC S9(7)V99   COMP-3.
00263
00264      12  CM-CREDIT-INTERFACE-SW-1          PIC X.
00265          88  CERT-ADDED-BATCH                 VALUE ' '.
00266          88  CERT-ADDED-ONLINE                VALUE '1'.
00267          88  CERT-PEND-ISSUE-ERROR            VALUE '2'.
00268          88  CERT-PURGED-OFFLINE              VALUE '3'.
00269          88  CERT-PEND-ISSUE-RETURNED         VALUE '4'.
00270      12  CM-CREDIT-INTERFACE-SW-2          PIC X.
00271          88  CERT-AS-LOADED                   VALUE ' '.
00272          88  CERT-CANCELLED-ONLINE            VALUE '1'.
00273          88  CERT-CLAIM-ONLINE                VALUE '2'.
00274          88  CERT-CLAIM-CANCEL-ONLINE         VALUE '3'.
00275          88  CERT-PEND-CANCEL-ERROR           VALUE '4'.
00276          88  CERT-PEND-CANCEL-VOID            VALUE '5'.
00277          88  CERT-PEND-CAN-VOID-ERROR         VALUE '6'.
00278          88  CERT-PEND-CANCEL-RETURNED        VALUE '7'.
00279
00280      12  CM-ACCOUNT-COMM-PCTS.
00281          16  CM-LIFE-COMM-PCT              PIC SV9(5)    COMP-3.
00282          16  CM-AH-COMM-PCT                PIC SV9(5)    COMP-3.
00283
00284      12  CM-USER-FIELD                     PIC X.
040504     12  CM-ADDL-CLP                       PIC S9(5)V99  COMP-3.
061405     12  CM-CLP-STATE                      PIC XX.
032612     12  CM-LF-CLASS-CD REDEFINES CM-CLP-STATE PIC XX.
061405     12  CM-USER-RESERVED                  PIC XXX.
032612     12  FILLER REDEFINES CM-USER-RESERVED.
032612         16  CM-AH-CLASS-CD                PIC XX.
032612         16  F                             PIC X.
00286 ******************************************************************
00345      EJECT
00346 *                                    COPY ELCCNTL.
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
00347      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA
                                CERTIFICATE-MASTER CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1272' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00349
00350      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00351      MOVE '5'                    TO DC-OPTION-CODE.
00352      PERFORM 8500-DATE-CONVERSION.
00353      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00354      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00355
00356      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00357
00358      MOVE +2                     TO  EMI-NUMBER-OF-LINES
00359                                      EMI-SWITCH2.
00360
00361 *    NOTE *******************************************************
00362 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
00363 *         *  FROM ANOTHER MODULE.                               *
00364 *         *******************************************************.
00365
00366      IF EIBCALEN NOT > ZERO
00367          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00368          GO TO 8300-SEND-TEXT.
00369
00370      
      * EXEC CICS HANDLE CONDITION
00371 *        PGMIDERR (9600-PGMIDERR)
00372 *        NOTFND   (8700-NOT-FOUND)
00373 *        ENDFILE  (4700-END-OF-BROWSE)
00374 *        DUPKEY   (4015-DUPKEY)
00375 *        ITEMERR  (9400-CLEAR)
00376 *        ERROR    (9990-ERROR)
00377 *    END-EXEC.
      *    MOVE '"$LI''$<.              ! " #00005117' TO DFHEIV0
           MOVE X'22244C4927243C2E20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303035313137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00378
00379      EJECT
           if pi-calling-program = 'EL1503'
              move +1                  to pi-ts-item
              MOVE +2                  TO PI-1ST-TIME-SW
              move dfhpf1              to eibaid
           END-IF
00380      IF PI-CALLING-PROGRAM = 'EL1273'
00381          MOVE EIBTRMID           TO  WS-TS1-TERM-ID
00382          
      * EXEC CICS READQ TS
00383 *            QUEUE  (WS-EL1273-TS)
00384 *            INTO   (PI-PROGRAM-WORK-AREA)
00385 *            LENGTH (WS-WORK-LENGTH)
00386 *        END-EXEC
      *    MOVE '*$I    L              ''   #00005134' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-EL1273-TS, 
                 PI-PROGRAM-WORK-AREA, 
                 WS-WORK-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00387          
      * EXEC CICS DELETEQ TS
00388 *            QUEUE  (WS-EL1273-TS)
00389 *        END-EXEC
      *    MOVE '*&                    #   #00005139' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-EL1273-TS, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00390          MOVE +2                 TO  PI-1ST-TIME-SW.
00391
00392      IF PI-CALLING-PROGRAM = 'EL127'
00393          MOVE ZERO               TO PI-SCREEN-COUNT
00394          MOVE PI-CERTIFICATE-KEY TO PI-1ST-KEY
00395          MOVE PI-KEY-LENGTH      TO PI-SAVE-KEY-LENGTH.
00396
00397      IF PI-CALLING-PROGRAM = 'EL1272'
00398          GO TO 0100-CONTINUE-PROCESSING
00399      ELSE
00400          MOVE ZERO               TO PI-ALT-NAME-COUNT.
00401
00402      IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00403          MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00404          MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00405          MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00406          MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00407          MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00408          MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00409          MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00410          MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
00411        ELSE
00412          MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00413          MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00414          MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00415          MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00416          MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00417          MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00418          MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00419          MOVE SPACES               TO  PI-SAVED-PROGRAM-6.
00420
00421      MOVE +0                     TO PI-SUB.
00422      MOVE LOW-VALUES             TO PI-CERT-CONTROLS-EL127 (1)
00423                                     PI-CERT-CONTROLS-EL127 (2)
00424                                     PI-CERT-CONTROLS-EL127 (3)
00425                                     PI-CERT-CONTROLS-EL127 (4)
00426                                     PI-CERT-CONTROLS-EL127 (5).
00427
00428      IF PI-1ST-TIME-SW = +2
00429          MOVE EIBTRMID           TO  WS-TSK-TERM-ID
00430          
      * EXEC CICS READQ TS
00431 *            QUEUE  (WS-TEMP-STORAGE-KEY)
00432 *            ITEM   (PI-TS-ITEM)
00433 *            INTO   (EL127BI)
00434 *            LENGTH (WS-TS-LENGTH)
00435 *        END-EXEC
      *    MOVE '*$II   L              ''   #00005182' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 EL127BI, 
                 WS-TS-LENGTH, 
                 PI-TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
               add +1 to pi-ts-item
00430          
      * EXEC CICS READQ TS
00431 *            QUEUE  (WS-TEMP-STORAGE-KEY)
00432 *            ITEM   (PI-TS-ITEM)
00433 *            INTO   (PI-PROGRAM-WORK-AREA)
00434 *            LENGTH (Ws-work-length)
      *            resp   (ws-response)
00435 *        END-EXEC
      *    MOVE '*$II   L              ''  N#00005189' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303035313839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 PI-PROGRAM-WORK-AREA, 
                 Ws-work-length, 
                 PI-TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00436          
      * EXEC CICS DELETEQ TS
00437 *            QUEUE  (WS-TEMP-STORAGE-KEY)
00438 *        END-EXEC
      *    MOVE '*&                    #   #00005196' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00439          MOVE ZERO               TO  PI-1ST-TIME-SW
00440          MOVE LOW-VALUES         TO  BSELO
00441                                      BPFKO
00442          PERFORM 6000-SET-ATTRB
00443              VARYING EL127B-INDEX FROM PI-LINE-COUNT BY -1
00444                UNTIL EL127B-INDEX NOT > ZERO
00445          GO TO 8100-SEND-INITIAL-MAP.
00446
00447      PERFORM 4000-BROWSE-CERT-FILE.
00448
00449      EJECT
00450  0100-CONTINUE-PROCESSING.
00451 *    NOTE *******************************************************
00452 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *
00453 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *
00454 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
00455 *         *******************************************************.
00456
00457      IF EIBAID = DFHCLEAR
00458          GO TO 9400-CLEAR.
00459
00460      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00461          MOVE LOW-VALUES         TO  EL127BO
00462          MOVE -1                 TO  BPFKL
00463          MOVE ER-0008            TO  EMI-ERROR
00464          GO TO 8200-SEND-DATAONLY.
00465
00466      
      * EXEC CICS RECEIVE
00467 *        INTO   (EL127BI)
00468 *        MAPSET (WS-MAPSET-NAME)
00469 *        MAP    (WS-MAP-NAME)
00470 *    END-EXEC.
           MOVE LENGTH OF
            EL127BI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00005226' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL127BI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00471
00472      IF BPFKL > ZERO
00473          IF EIBAID NOT = DFHENTER
00474              MOVE ER-0004        TO  EMI-ERROR
00475              MOVE AL-UNBOF       TO  BPFKA
00476              MOVE -1             TO  BPFKL
00477              GO TO 8200-SEND-DATAONLY
00478          ELSE
00479              IF (BPFKI NUMERIC) AND
00480                  (BPFKI > ZERO AND < 25)
00481                      MOVE PF-VALUES (BPFKI)  TO  EIBAID
00482                  ELSE
00483                      MOVE ER-0029        TO  EMI-ERROR
00484                      MOVE AL-UNBOF       TO  BPFKA
00485                      MOVE -1             TO  BPFKL
00486                      GO TO 8200-SEND-DATAONLY.
00487
00488      EJECT
00489 *    NOTE *******************************************************
00490 *         *      PF KEY USAGE:                                  *
00491 *         *        PF1       SEARCH FORWARD                     *
00492 *         *        PF2       SEARCH BACKWARD                    *
00493 *         *        PF3       CANCELLATION QUOTE                 *
00494 *         *        PF4       NEW CLAIM SETUP                    *
00495 *         *        PF5       CLAIM LOOK-UP                      *
00496 *         *        PF6       NEXT COMPANY                       *
00497 *         *        PF7       ORIGINAL COMPANY                   *
      *         *        PF8       CERT CLAIM HISTORY                 *
00498 *      (removed)   PF9       REVOLVING CREDIT INTERFACE (CRI)   *
00499 *         *        PF12      HELP                               *
00500 *         *        PF23      LOGOFF                             *
00501 *         *        PF24      RETURN TO MASTER MENU              *
00502 *         *******************************************************.
00503
00504      IF EIBAID = DFHPF12
00505          MOVE 'EL010'         TO  THIS-PGM
00506          GO TO 9300-XCTL.
00507
00508      IF EIBAID = DFHPF23
00509          GO TO 9000-RETURN-CICS.
00510
00511      IF EIBAID = DFHPF24
00512          MOVE 'EL126'            TO  THIS-PGM
00513          GO TO 9300-XCTL.
00514
00515      IF BSELL > ZERO
00516          IF BSELI NOT NUMERIC
00517              MOVE -1             TO  BSELL
00518              MOVE ER-0200        TO  EMI-ERROR
00519              GO TO 8200-SEND-DATAONLY.
00520
00521      EJECT
00522      IF CLAIM-SESSION
00523        IF EIBAID = DFHPF4
00524          IF BSELL > ZERO  AND
00525             BSELI NOT > PI-LINE-COUNT
00526              NEXT SENTENCE
00527            ELSE
00528          IF BCRTSL1L > 0 OR
00529             BCRTSL2L > 0 OR
00530             BCRTSL3L > 0 OR
00531             BCRTSL4L > 0 OR
00532             BCRTSL5L > 0 OR
00533             BCRTSL6L > 0 OR
00534             BCRTSL7L > 0 OR
00535             BCRTSL8L > 0
00536              NEXT SENTENCE
00537            ELSE
00538              MOVE -1             TO  BSELL
00539              MOVE ER-0200        TO  EMI-ERROR
00540              GO TO 8200-SEND-DATAONLY.
00541
00542      IF CLAIM-SESSION
00543        IF EIBAID = DFHPF4 OR
00544          PI-SAVED-PROGRAM-1 = 'EL130' OR
00545            BCRTSL1L > 0 OR
00546            BCRTSL2L > 0 OR
00547            BCRTSL3L > 0 OR
00548            BCRTSL4L > 0 OR
00549            BCRTSL5L > 0 OR
00550            BCRTSL6L > 0 OR
00551            BCRTSL7L > 0 OR
00552            BCRTSL8L > 0
00553               GO TO 0115-BUILD-CERT-KEYS.
00554
00555      IF EIBAID = DFHPF3 OR DFHPF4 OR DFHPF5
00556          OR BSELL > ZERO
00557          NEXT SENTENCE
00558      ELSE
00559          GO TO 0120-MAIN-LOGIC.
00560
00561      IF (CREDIT-SESSION  AND
00562          EIBAID = (DFHPF4 OR DFHPF5))
00563        OR
00564          (PI-MODIFY-CAP = 'C' AND
00565           EIBAID = (DFHPF4 OR DFHPF5))
00566              MOVE ER-0008        TO  EMI-ERROR
00567              MOVE -1             TO  BPFKL
00568              MOVE AL-UNBOF       TO  BPFKA
00569              GO TO 8200-SEND-DATAONLY.
00570
00571      IF BSELL > ZERO        AND
00572         BSELO > ZERO        AND
00573         BSELO < '9'         AND
00574         BSELI NOT > PI-LINE-COUNT
00575          NEXT SENTENCE
00576        ELSE
00577          MOVE -1                 TO  BSELL
00578          MOVE ER-0200            TO  EMI-ERROR
00579          GO TO 8200-SEND-DATAONLY.
00580
00581      IF EIBAID = DFHPF3
00582        OR (BSELL > ZERO AND
00583            PI-SAVED-PROGRAM-1 NOT = 'EL130')
00584             MOVE EIBTRMID       TO  WS-TSK-TERM-ID
00585             PERFORM 8620-WRITE-TEMP-STORAGE THRU 8630-EXIT.
00586
00587      IF CREDIT-SESSION
00588          NEXT SENTENCE
00589      ELSE
00590          IF (PI-SAVED-PROGRAM-1 NOT = 'EL130') AND
00591              (EIBAID = DFHENTER OR DFHPF3 OR DFHPF5
00592                                           OR DFHPF8)
00593                  NEXT SENTENCE
00594              ELSE
00595                  GO TO 8200-SEND-DATAONLY.
00596
00597  EJECT
00598      SET EL127B-INDEX TO BSELI.
00599
00600      IF EIBAID = DFHPF5
00601         IF EL127B-AST (EL127B-INDEX) NOT = '*'
00602          MOVE -1                 TO  BSELL
00603          MOVE ER-0363            TO  EMI-ERROR
00604          GO TO 8200-SEND-DATAONLY.
00605
00606      MOVE EL127B-CARRIER (EL127B-INDEX)  TO  PI-CARRIER.
00607      MOVE EL127B-GROUP   (EL127B-INDEX)  TO  PI-GROUPING.
00608      MOVE EL127B-STATE   (EL127B-INDEX)  TO  PI-STATE.
00609      MOVE EL127B-ACCOUNT (EL127B-INDEX)  TO  PI-ACCOUNT.
00610      MOVE EL127B-CERT-NO (EL127B-INDEX)  TO  PI-CERT-NO.
00611
00612      MOVE EL127B-EFF-DATE (EL127B-INDEX)  TO  DC-GREG-DATE-1-EDIT.
00613      MOVE '2'                    TO  DC-OPTION-CODE.
00614      PERFORM 8500-DATE-CONVERSION.
00615      MOVE DC-BIN-DATE-1          TO  PI-CERT-EFF-DT.
00616
00617      MOVE +2                     TO  PI-1ST-TIME-SW.
00618
00619      MOVE PI-SAVED-PROGRAM-1     TO  THIS-PGM.
00620
00621      IF THIS-PGM  NOT = 'EL130'
00622          MOVE 'EL1273'           TO  THIS-PGM.
00623
00624      IF EIBAID = DFHPF3
00625          IF PI-COMPANY-ID = 'AIG' OR 'AUK'
00626              MOVE ER-0029        TO  EMI-ERROR
00627              MOVE -1             TO  BPFKL
00628              GO TO 8200-SEND-DATAONLY
00629          ELSE
00630              MOVE 'EL1278'       TO  THIS-PGM
00631              GO TO 9300-XCTL.
00632
00633      IF EIBAID = DFHPF5
00634          MOVE 'EL132'            TO  THIS-PGM.
           IF EIBAID = DFHPF8
              MOVE 'EL1503'            TO THIS-PGM
              MOVE EIBTRMID            TO WS-TSK-TERM-ID
      *       PERFORM 8620-WRITE-TEMP-STORAGE
      *                                THRU 8630-EXIT
              GO TO 9300-XCTL
           END-IF
00646      IF THIS-PGM NOT = 'EL1273'
00647          MOVE PI-RETURN-TO-PROGRAM   TO  PI-CALLING-PROGRAM
00648          MOVE PI-SAVED-PROGRAM-1     TO  PI-RETURN-TO-PROGRAM
00649          MOVE PI-SAVED-PROGRAM-2     TO  PI-SAVED-PROGRAM-1
00650          MOVE PI-SAVED-PROGRAM-3     TO  PI-SAVED-PROGRAM-2
00651          MOVE PI-SAVED-PROGRAM-4     TO  PI-SAVED-PROGRAM-3
00652          MOVE PI-SAVED-PROGRAM-5     TO  PI-SAVED-PROGRAM-4
00653          MOVE PI-SAVED-PROGRAM-6     TO  PI-SAVED-PROGRAM-5
00654          MOVE SPACES                 TO  PI-SAVED-PROGRAM-6.
00655
00656      GO TO 9300-XCTL.
00657
00658  EJECT
00659  0115-BUILD-CERT-KEYS.
00660
00661      IF (BCRTSL1L > 0) OR
00662         (EIBAID = DFHPF4 AND BSELL > 0
00663         AND BSELI = 1)
00664            MOVE +1                 TO  SUB
00665            PERFORM 0116-BUILD-EL127-CERT-KEYS THRU 0116-BUILD-EXIT
00666            MOVE +0                 TO  SUB
00667            PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT
00668            IF PREVIOUSLY-SELECTED
00669                NEXT SENTENCE
00670            ELSE
00671                ADD +1              TO  PI-SUB
00672                IF PI-SUB > +5
00673                    MOVE ER-0659    TO  EMI-ERROR
00674                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00675                ELSE
00676                    MOVE WS-CERT-CONTROL    TO
00677                                   PI-CERT-CONTROLS-EL127 (PI-SUB).
00678
00679      IF (BCRTSL2L > 0) OR
00680         (EIBAID = DFHPF4 AND BSELL > 0
00681         AND BSELI = 2)
00682            MOVE +2                 TO  SUB
00683            PERFORM 0116-BUILD-EL127-CERT-KEYS THRU 0116-BUILD-EXIT
00684            MOVE +0                 TO  SUB
00685            PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT
00686            ADD +1                  TO  PI-SUB
00687            IF PREVIOUSLY-SELECTED
00688                NEXT SENTENCE
00689            ELSE
00690                IF PI-SUB > +5
00691                    MOVE ER-0659    TO  EMI-ERROR
00692                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00693                ELSE
00694                    MOVE WS-CERT-CONTROL    TO
00695                                   PI-CERT-CONTROLS-EL127 (PI-SUB).
00696
00697      IF (BCRTSL3L > 0) OR
00698         (EIBAID = DFHPF4 AND BSELL > 0
00699         AND BSELI = 3)
00700            MOVE +3                 TO  SUB
00701            PERFORM 0116-BUILD-EL127-CERT-KEYS THRU 0116-BUILD-EXIT
00702            MOVE +0                 TO  SUB
00703            PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT
00704            ADD +1                  TO  PI-SUB
00705            IF PREVIOUSLY-SELECTED
00706                NEXT SENTENCE
00707            ELSE
00708                IF PI-SUB > +5
00709                    MOVE ER-0659    TO  EMI-ERROR
00710                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00711                ELSE
00712                    MOVE WS-CERT-CONTROL    TO
00713                                   PI-CERT-CONTROLS-EL127 (PI-SUB).
00714
00715      IF (BCRTSL4L > 0) OR
00716         (EIBAID = DFHPF4 AND BSELL > 0
00717         AND BSELI = 4)
00718            MOVE +4                 TO  SUB
00719            PERFORM 0116-BUILD-EL127-CERT-KEYS THRU 0116-BUILD-EXIT
00720            MOVE +0                 TO  SUB
00721            PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT
00722            ADD +1                  TO  PI-SUB
00723            IF PREVIOUSLY-SELECTED
00724                NEXT SENTENCE
00725            ELSE
00726                IF PI-SUB > +5
00727                    MOVE ER-0659    TO  EMI-ERROR
00728                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00729                ELSE
00730                    MOVE WS-CERT-CONTROL    TO
00731                                   PI-CERT-CONTROLS-EL127 (PI-SUB).
00732
00733      IF (BCRTSL5L > 0) OR
00734         (EIBAID = DFHPF4 AND BSELL > 0
00735         AND BSELI = 5)
00736            MOVE +5                 TO  SUB
00737            PERFORM 0116-BUILD-EL127-CERT-KEYS THRU 0116-BUILD-EXIT
00738            MOVE +0                 TO  SUB
00739            PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT
00740            ADD +1                  TO  PI-SUB
00741            IF PREVIOUSLY-SELECTED
00742                NEXT SENTENCE
00743            ELSE
00744                IF PI-SUB > +5
00745                    MOVE ER-0659    TO  EMI-ERROR
00746                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00747                ELSE
00748                    MOVE WS-CERT-CONTROL    TO
00749                                   PI-CERT-CONTROLS-EL127 (PI-SUB).
00750
00751      IF (BCRTSL6L > 0) OR
00752         (EIBAID = DFHPF4 AND BSELL > 0
00753         AND BSELI = 6)
00754            MOVE +6                 TO  SUB
00755            PERFORM 0116-BUILD-EL127-CERT-KEYS THRU 0116-BUILD-EXIT
00756            MOVE +0                 TO  SUB
00757            PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT
00758            ADD +1                  TO  PI-SUB
00759            IF PREVIOUSLY-SELECTED
00760                NEXT SENTENCE
00761            ELSE
00762                IF PI-SUB > +5
00763                    MOVE ER-0659    TO  EMI-ERROR
00764                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00765                ELSE
00766                    MOVE WS-CERT-CONTROL    TO
00767                                   PI-CERT-CONTROLS-EL127 (PI-SUB).
00768
00769      IF (BCRTSL7L > 0) OR
00770         (EIBAID = DFHPF4 AND BSELL > 0
00771         AND BSELI = 7)
00772            MOVE +7                 TO  SUB
00773            PERFORM 0116-BUILD-EL127-CERT-KEYS THRU 0116-BUILD-EXIT
00774            MOVE +0                 TO  SUB
00775            PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT
00776            ADD +1                  TO  PI-SUB
00777            IF PREVIOUSLY-SELECTED
00778                NEXT SENTENCE
00779            ELSE
00780                IF PI-SUB > +5
00781                    MOVE ER-0659    TO  EMI-ERROR
00782                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00783                ELSE
00784                    MOVE WS-CERT-CONTROL    TO
00785                                   PI-CERT-CONTROLS-EL127 (PI-SUB).
00786
00787      IF (BCRTSL8L > 0) OR
00788         (EIBAID = DFHPF4 AND BSELL > 0
00789         AND BSELI = 8)
00790            MOVE +8                 TO  SUB
00791            PERFORM 0116-BUILD-EL127-CERT-KEYS THRU 0116-BUILD-EXIT
00792            MOVE +0                 TO  SUB
00793            PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT
00794            ADD +1                  TO  PI-SUB
00795            IF PREVIOUSLY-SELECTED
00796                NEXT SENTENCE
00797            ELSE
00798                IF PI-SUB > +5
00799                    MOVE ER-0659    TO  EMI-ERROR
00800                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00801                ELSE
00802                    MOVE WS-CERT-CONTROL    TO
00803                                   PI-CERT-CONTROLS-EL127 (PI-SUB).
00804
00805      IF EIBAID = DFHPF4
00806          MOVE PI-SUB                 TO  PI-CERT-SELECT-CNT
00807          MOVE +1                     TO  PI-CERT-PROCESSED-CNT
00808          MOVE PI-EL127-CARRIER (1)   TO  PI-CARRIER
00809          MOVE PI-EL127-GROUPING (1)  TO  PI-GROUPING
00810          MOVE PI-EL127-STATE (1)     TO  PI-STATE
00811          MOVE PI-EL127-ACCOUNT (1)   TO  PI-ACCOUNT
00812          MOVE PI-EL127-CERT-NO (1)   TO  PI-CERT-NO
00813          MOVE PI-EL127-EFF-DT (1)    TO  PI-CERT-EFF-DT
00814          MOVE 'EL130'                TO  THIS-PGM
00815          MOVE PI-RETURN-TO-PROGRAM   TO  PI-CALLING-PROGRAM
00816          MOVE PI-SAVED-PROGRAM-1     TO  PI-RETURN-TO-PROGRAM
00817          MOVE PI-SAVED-PROGRAM-2     TO  PI-SAVED-PROGRAM-1
00818          MOVE PI-SAVED-PROGRAM-3     TO  PI-SAVED-PROGRAM-2
00819          MOVE PI-SAVED-PROGRAM-4     TO  PI-SAVED-PROGRAM-3
00820          MOVE PI-SAVED-PROGRAM-5     TO  PI-SAVED-PROGRAM-4
00821          MOVE PI-SAVED-PROGRAM-6     TO  PI-SAVED-PROGRAM-5
00822          MOVE SPACES                 TO  PI-SAVED-PROGRAM-6
00823          GO TO 9300-XCTL.
00824
00825      GO TO 0120-MAIN-LOGIC.
00826
00827  0115-CK-FOR-DUP-SELECTION.
00828
00829      ADD +1                      TO  SUB.
00830      IF SUB > 5
00831          MOVE 'N'                TO  WS-SELECTED-SW
00832          GO TO 0115-DUP-EXIT.
00833
00834      IF WS-CERT-CONTROL = PI-CERT-CONTROLS-EL127 (SUB)
00835          MOVE 'Y'                TO  WS-SELECTED-SW
00836          GO TO 0115-DUP-EXIT.
00837
00838      GO TO 0115-CK-FOR-DUP-SELECTION.
00839
00840  0115-DUP-EXIT.
00841      EXIT.
00842
00843  0116-BUILD-EL127-CERT-KEYS.
00844
00845      MOVE EL127B-CARRIER (SUB)   TO  WS-CARRIER.
00846      MOVE EL127B-GROUP (SUB)     TO  WS-GROUPING.
00847      MOVE EL127B-STATE (SUB)     TO  WS-STATE.
00848      MOVE EL127B-ACCOUNT (SUB)   TO  WS-ACCOUNT.
00849      MOVE EL127B-CERT-NO (SUB)   TO  WS-CERT-NO.
00850      MOVE EL127B-EFF-DATE (SUB)  TO  DC-GREG-DATE-1-EDIT.
00851      MOVE '2'                    TO  DC-OPTION-CODE.
00852      PERFORM 8500-DATE-CONVERSION.
00853      IF NO-CONVERSION-ERROR
00854          MOVE DC-BIN-DATE-1      TO  WS-EFF-DT
00855      ELSE
00856          MOVE LOW-VALUES         TO  WS-EFF-DT.
00857
00858  0116-BUILD-EXIT.
00859      EXIT.
00860
00861  0120-MAIN-LOGIC.
00862      IF EIBAID = (DFHENTER OR DFHPF1 OR DFHPF6 OR DFHPF7)
00863        OR
00864          ((EIBAID = DFHPF2) AND
00865           (PI-SCREEN-COUNT > +1))
00866              NEXT SENTENCE
00867            ELSE
00868              MOVE ER-0131            TO  EMI-ERROR
00869              MOVE -1                 TO  BPFKL
00870              GO TO 8200-SEND-DATAONLY.
00871
00872      IF EIBAID = DFHPF6
00873         IF PI-ORIGINAL-COMPANY-ID NOT = SPACES
00874            PERFORM 8600-NEXT-COMPANY THRU 8600-EXIT
00875            PERFORM 8650-WRITE-SECURITY-TEMP-STORE THRU 8650-EXIT
00876         ELSE
00877            MOVE ER-0008              TO  EMI-ERROR
00878            MOVE -1                   TO  BSELL
00879            GO TO 8200-SEND-DATAONLY.
00880
00881      IF EIBAID = DFHPF7
00882         IF PI-ORIGINAL-COMPANY-ID NOT = SPACES
00883            PERFORM 8600-NEXT-COMPANY THRU 8600-EXIT
00884            PERFORM 8650-WRITE-SECURITY-TEMP-STORE THRU 8650-EXIT
00885            MOVE PI-ORIGINAL-COMPANY-CD   TO PI-COMPANY-CD
00886                                             PI-CK-COMPANY-CD
00887         ELSE
00888            MOVE ER-0008              TO  EMI-ERROR
00889            MOVE -1                   TO  BSELL
00890            GO TO 8200-SEND-DATAONLY.
00891
00892      IF PI-END-OF-FILE NOT > ZERO
00893          PERFORM 4000-BROWSE-CERT-FILE.
00894
00895      IF PI-END-OF-FILE > ZERO
00896          IF EIBAID = DFHPF2
00897              NEXT SENTENCE
00898          ELSE
00899              MOVE ER-0130        TO  EMI-ERROR
00900              MOVE -1             TO  BSELL
00901              GO TO 8200-SEND-DATAONLY.
00902
00903      IF EIBAID  = DFHPF2
00904          NEXT SENTENCE
00905        ELSE
00906          IF PI-DSID NOT = 'ELCERT'
00907              GO TO 9400-CLEAR.
00908
00909      IF (PI-PREV-AID = (DFHPF1 OR DFHENTER) AND
00910          EIBAID = DFHPF2)
00911        OR
00912          (PI-PREV-AID = DFHPF2 AND
00913           EIBAID = (DFHPF1 OR DFHENTER))
00914              PERFORM 4000-BROWSE-CERT-FILE
00915            ELSE
00916              GO TO 9400-CLEAR.
00917
00918      EJECT
00919  4000-BROWSE-CERT-FILE SECTION.
00920      MOVE PI-CERTIFICATE-KEY   TO PI-LIN1-CERTIFICATE-KEY.
00921
00922      
      * EXEC CICS HANDLE CONDITION
00923 *         NOTFND (8700-NOT-FOUND)
00924 *         END-EXEC.
      *    MOVE '"$I                   ! # #00005679' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303035363739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00925
00926      MOVE LOW-VALUES             TO  EL127BO.
00927
00928      IF PI-BROWSE-SW = ZERO
00929        AND PI-START-SW = +1
00930          
      * EXEC CICS STARTBR
00931 *            DATASET   (PI-DSID)
00932 *            RIDFLD    (PI-CERTIFICATE-KEY)
00933 *            GENERIC
00934 *            GTEQ
00935 *            KEYLENGTH (PI-KEY-LENGTH)
00936 *            END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    G          &   #00005687' TO DFHEIV0
           MOVE X'262C2020204B472020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 PI-CERTIFICATE-KEY, 
                 PI-KEY-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00937          GO TO 4005-NEXT-SENTENCE.
00938
00939      IF EIBAID = DFHPF1
00940        AND OPTION-FOUR-SELECTED
00941        AND PI-1ST-KEY = PI-LAST-KEY
00942          PERFORM 7000-PF2-POSITION
00943          GO TO 4005-NEXT-SENTENCE.
00944
00945      IF EIBAID = DFHPF2
00946          SUBTRACT 2 FROM PI-SCREEN-COUNT
00947          PERFORM 7000-PF2-POSITION
00948          GO TO 4005-NEXT-SENTENCE.
00949
00950      
      * EXEC CICS STARTBR
00951 *        DATASET (PI-DSID)
00952 *        RIDFLD  (PI-CERTIFICATE-KEY)
00953 *        EQUAL
00954 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         E          &   #00005707' TO DFHEIV0
           MOVE X'262C20202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 PI-CERTIFICATE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00955
00956  4005-NEXT-SENTENCE.
00957      MOVE +1                     TO  PI-BROWSE-SW.
00958      MOVE ZERO                   TO  PI-LINE-COUNT.
00959      MOVE LOW-VALUES             TO  EL127BO.
00960      MOVE PI-CERTIFICATE-KEY     TO  WS-KEY-HOLD.
00961      SET EL127B-INDEX TO +1.
00962
00963  4010-READNEXT.
00964      
      * EXEC CICS READNEXT
00965 *        DATASET (PI-DSID)
00966 *        RIDFLD  (PI-CERTIFICATE-KEY)
00967 *        SET     (ADDRESS OF CERTIFICATE-MASTER)
00968 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005721' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CERTIFICATE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00969
00970      IF PI-LINE-COUNT NOT = ZERO
00971          MOVE ZERO               TO  PI-AIX-RECORD-COUNT.
00972
00973  4015-DUPKEY.
00974      IF LCP-ONCTR-01 =  0
00975          ADD 1 TO LCP-ONCTR-01
00976      ELSE
00977          GO TO 4016-NEXT-SENTENCE.
00978
00979      IF PI-AIX-RECORD-COUNT > +8
00980          SUBTRACT +1 FROM PI-AIX-RECORD-COUNT.
00981
00982  4016-NEXT-SENTENCE.
00983
00984      ADD +1  TO  WS-AIX-RECORD-COUNT.
00985
00986      IF EIBAID = DFHPF1 OR DFHENTER
00987          IF PI-LINE-COUNT = ZERO
00988              IF OPTION-THREE-SELECTED
00989                  PERFORM 5000-MOVE-NAME
00990                  IF CM-INSURED-LAST-NAME = PI-END-NAME
00991                      IF PI-END-CERTIFICATE-KEY =
00992                              CM-CONTROL-PRIMARY
00993                          NEXT SENTENCE
00994                        ELSE
00995                          GO TO 4010-READNEXT
00996                    ELSE
00997                      NEXT SENTENCE
00998                ELSE
00999                  IF OPTION-ONE-SELECTED
01000                      IF ((CM-COMPANY-CD = PI-END-COMPANY-ID)
01001                          AND (CM-CERT-NO = PI-END-CERT-NO))
01002                          IF PI-END-CERTIFICATE-KEY =
01003                                  CM-CONTROL-PRIMARY
01004                              NEXT SENTENCE
01005                            ELSE
01006                              GO TO 4010-READNEXT.
01007
01008      MOVE PI-CERTIFICATE-KEY     TO  WS-KEY-INPUT.
01009
01010      SET KEY-INDEX
01011          KEY-INDEX2 TO +1.
01012
01013  4020-COMPARE-KEY.
01014      IF PI-PART-FIELD-SW  NOT = ' '
01015         MOVE +1  TO PI-KEY-LENGTH.
01016
01017      IF WS-KH-CHAR (KEY-INDEX) NOT = WS-KI-CHAR (KEY-INDEX2)
01018          GO TO 4700-END-OF-BROWSE.
01019
01020      IF KEY-INDEX < PI-KEY-LENGTH
01021          SET KEY-INDEX
01022              KEY-INDEX2 UP BY +1
01023          GO TO 4020-COMPARE-KEY.
01024
01025 ******************************************************************
01026 *        SECURITY CHECK FOR CARRIER AND ACCOUNT NUMBER           *
01027 *                        04/04/84                                *
01028 ******************************************************************
01029
01030 ******************************************************************
01031 *    IF THE SECURITY CHECK ROUTINE IS CHANGED HERE, YOU MUST     *
01032 *        ALSO CHANGE THE SECURITY CHECK ROUTINE IN               *
01033 *        7100-READNEXT-PF2.           KER/080884                 *
01034 ******************************************************************
01035
01036      IF PI-NO-CARRIER-SECURITY AND
01037         PI-NO-ACCOUNT-SECURITY
01038          GO TO 4030-CHECK-OPTION.
01039
01040      IF PI-CARRIER-SECURITY > SPACES
01041          IF CM-CARRIER = PI-CARRIER-SECURITY
01042              NEXT SENTENCE
01043             ELSE
01044              GO TO 4010-READNEXT.
01045
01046      IF PI-ACCOUNT-SECURITY > SPACES
01047          IF CM-ACCOUNT = PI-ACCOUNT-SECURITY
01048              NEXT SENTENCE
01049             ELSE
01050              GO TO 4010-READNEXT.
01051
01052  4030-CHECK-OPTION.
01053      IF NOT OPTION-THREE-SELECTED
01054          GO TO 4090-MOVE-DATA.
01055
01056      IF (CM-INSURED-LAST-NAME NOT = PI-SC-LAST-NAME) OR
01057         (CM-INSURED-INITIALS  NOT = PI-PREV-INITIALS)
01058          MOVE +1         TO PI-ALT-NAME-COUNT.
01059
01060      MOVE CM-INSURED-INITIALS    TO PI-PREV-INITIALS.
01061
01062 ******************************************************************
01063 *   IF READING ELCERT (ALT BY NAME), ADD TO PI-ALT-NAME-COUNT.   *
01064 *   ON EITHER OF TWO CONDITIONS: 1. END OF SEARCH.....(EL1272)   *
01065 *                                2. NOT FOUND.........(EL127)    *
01066 *  ......IF COUNT WAS OVER 140, DISPLAY MESSAGE ER-0765,         *
01067 *           DIRECTING USER TO ECS052 FOR FULL LIST.              *
01068 ******************************************************************
01069
01070      IF EIBAID = DFHPF2
01071           SUBTRACT +1 FROM PI-ALT-NAME-COUNT
01072         ELSE
01073           ADD +1        TO PI-ALT-NAME-COUNT.
01074
01075 ******************************************************************
01076 *        IF THE INITIAL CHECKING ROUTINE OR THE ACCOUNT CHECKING *
01077 *    ROUTINE IS CHANGED, YOU MUST ALSO CHANGE THE CORRESPONDING  *
01078 *    ROUTINE IN 7130-CHECK-INITIAL.        KER/080884            *
01079 ******************************************************************
01080
01081      IF PI-SC-INITIALS NOT = SPACES
01082         MOVE PI-SC-INITIALS      TO WS-INITIALS
01083         IF WS-INIT2 NOT = SPACE
01084            IF PI-SC-INITIALS NOT = CM-INSURED-INITIALS
01085               GO TO 4010-READNEXT
01086              ELSE
01087               NEXT SENTENCE
01088            ELSE
01089            IF WS-INIT1 NOT = CM-INSURED-INITIAL1
01090               GO TO 4010-READNEXT.
01091
01092      IF PI-SC-FIRST-NAME = SPACES
01093          GO TO 4040-CONTINUE.
01094
01095      MOVE PI-SC-FIRST-NAME       TO WS-PI-NAME.
01096      MOVE CM-INSURED-FIRST-NAME  TO WS-CM-NAME.
01097
01098      MOVE SPACE                  TO WS-COMPARE-INDICATOR.
01099      PERFORM 4035-CHECK-NAME THRU 4035-EXIT
01100          VARYING WS-NAME-INDEX FROM 15 BY -1
01101            UNTIL WS-NAME-INDEX = ZERO.
01102
01103      IF NAME-NOT-FOUND
01104          GO TO 4010-READNEXT
01105        ELSE
01106          GO TO 4040-CONTINUE.
01107
01108  4035-CHECK-NAME.
01109      IF WS-PI-NAME-CHAR (WS-NAME-INDEX) NOT = ' ' AND
01110         WS-CM-NAME-CHAR (WS-NAME-INDEX)
01111           MOVE 'X'               TO WS-COMPARE-INDICATOR.
01112
01113  4035-EXIT.
01114       EXIT.
01115
01116  4040-CONTINUE.
01117      IF PI-SC-ACCT-NO = SPACES
01118          GO TO 4050-CK-CARRIER.
01119
01120      IF PI-SC-ACCT-NO = CM-ACCOUNT
01121          NEXT SENTENCE
01122        ELSE
01123          GO TO 4010-READNEXT.
01124
01125  4050-CK-CARRIER.
01126      IF PI-SC-CARR = SPACES
110106        GO TO 4060-CK-ST
           END-IF
01129      IF PI-SC-CARR = CM-CARRIER
01130          NEXT SENTENCE
01131        ELSE
01132          GO TO 4010-READNEXT.
01133
110106 4060-CK-ST.
110106     IF PI-SC-ST = SPACES
110106        GO TO 4070-CK-STATUS
110106     END-IF
110106     IF PI-SC-ST = CM-STATE
110106        CONTINUE
110106     ELSE
110106        GO TO 4010-READNEXT
110106     END-IF
110106     .
       4070-CK-STATUS.
           IF PI-SC-STATUS NOT = 'Y'
              GO TO 4090-MOVE-DATA
           END-IF
           IF (CM-LF-CURRENT-STATUS = '1' OR '2' OR '4')
              OR (CM-AH-CURRENT-STATUS = '1' OR '2' OR '4')
              CONTINUE
           ELSE
              GO TO 4010-READNEXT
           END-IF
           IF ((CM-LF-LOAN-EXPIRE-DT NOT = LOW-VALUES)
              AND (CM-LF-LOAN-EXPIRE-DT > SAVE-BIN-DATE))
                             OR
              ((CM-AH-LOAN-EXPIRE-DT NOT = LOW-VALUES)
              AND (CM-AH-LOAN-EXPIRE-DT > SAVE-BIN-DATE))
              CONTINUE
           ELSE
              GO TO 4010-READNEXT
           END-IF
           .
01134  4090-MOVE-DATA.
01135      IF LCP-ONCTR-02 =  0
01136          ADD 1 TO LCP-ONCTR-02
01137        ELSE
01138          GO TO 4095-MOVE-DATA.
01139
01140      MOVE +1                     TO WS-CERT-SW.
01141
01142  4095-MOVE-DATA.
01143      MOVE WS-KEY-INPUT           TO  PI-LAST-KEY.
01144
01145      ADD +1                      TO  PI-LINE-COUNT
01146                                      PI-AIX-RECORD-COUNT.
01147
01148      IF CM-CLAIM-ATTACHED-COUNT > ZERO
01149          MOVE '*'                TO  EL127B-AST (EL127B-INDEX).
01150
01151      IF PI-COMPANY-ID = 'FLA'
01152          PERFORM 9050-FLA-NAME THRU 9050-EXIT-FIX.
01153
01154      PERFORM 5000-MOVE-NAME.
01155
01156      MOVE CM-CONTROL-PRIMARY   TO  PI-END-CERTIFICATE-KEY.
01157      MOVE CM-INSURED-LAST-NAME TO  PI-END-NAME.
01158
01159      MOVE WS-NAME-WORK         TO  EL127B-NAME-O   (EL127B-INDEX).
01160      MOVE CM-INSURED-ISSUE-AGE TO  EL127B-AGE      (EL127B-INDEX).
01161      MOVE CM-INSURED-SEX       TO  EL127B-SEX      (EL127B-INDEX).
01162      MOVE CM-CARRIER           TO  EL127B-CARRIER  (EL127B-INDEX).
01163      MOVE CM-GROUPING          TO  EL127B-GROUP    (EL127B-INDEX).
01164      MOVE CM-STATE             TO  EL127B-STATE    (EL127B-INDEX).
01165      MOVE CM-ACCOUNT           TO  EL127B-ACCOUNT  (EL127B-INDEX).
01166      MOVE CM-CERT-NO           TO  EL127B-CERT-NO  (EL127B-INDEX).
01167
01168      IF CM-CERT-EFF-DT NOT = LOW-VALUES
01169          MOVE CM-CERT-EFF-DT       TO  DC-BIN-DATE-1
01170          MOVE SPACES               TO  DC-OPTION-CODE
01171          PERFORM 8500-DATE-CONVERSION
01172          MOVE DC-GREG-DATE-1-EDIT  TO  EL127B-EFF-DATE
01173                                               (EL127B-INDEX).
01174
01175      PERFORM 6000-SET-ATTRB.
01176
01177      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
01178      MOVE '5'                    TO  DC-OPTION-CODE.
01179      PERFORM 8500-DATE-CONVERSION.
01180      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-DATE.
01181
01182      IF CM-LF-BENEFIT-CD = '00'
01183          GO TO 4100-A-AND-H-INFO.
01184
01185      MOVE SPACES     TO    EL127B-LIFE-INFO (EL127B-INDEX).
01186
01187      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.
01188      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
01189      MOVE '4'                    TO  WS-CFK-RECORD-TYPE.
01190      MOVE CM-LF-BENEFIT-CD       TO  WS-CFK-BENEFIT-NO.
01191
01192      PERFORM 8000-READ-CONTROL-FILE.
01193
01194      IF WS-NOT-FOUND = ZERO
01195          MOVE CF-BENEFIT-ALPHA (WS-INDEX)
01196                                 TO  EL127B-LI-ABVR (EL127B-INDEX).
01197
01198      IF CM-LF-CURRENT-STATUS = '8'
01199         IF CM-LF-CANCEL-DT NOT = LOW-VALUES
01200             MOVE CM-LF-CANCEL-DT TO DC-BIN-DATE-1
01201             MOVE SPACES          TO DC-OPTION-CODE
01202             PERFORM 8500-DATE-CONVERSION
01203             IF NOT DATE-CONVERSION-ERROR
01204                 MOVE DC-GREG-DATE-1-EDIT TO EL127B-LI-DATE
01205                                                    (EL127B-INDEX).
01206
01207      IF CM-LF-CURRENT-STATUS = '7'
01208         IF CM-LF-DEATH-DT NOT = LOW-VALUES
01209             MOVE CM-LF-DEATH-DT  TO DC-BIN-DATE-1
01210             MOVE SPACES          TO DC-OPTION-CODE
01211             PERFORM 8500-DATE-CONVERSION
01212             IF NOT DATE-CONVERSION-ERROR
01213                 MOVE DC-GREG-DATE-1-EDIT TO EL127B-LI-DATE
01214                                                   (EL127B-INDEX).
01215
01216 * READ STATE MASTER RECORD FOR FREE LOOK PERIOD *
01217
01218      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.
01219      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
01220      MOVE '3'                    TO  WS-CFK-RECORD-TYPE.
01221      MOVE CM-STATE               TO  WS-CFK-ACCESS-TYPE.
01222
01223      PERFORM 8000-READ-CONTROL-FILE.
01224
01225      IF WS-ST-REC-NOT-FOUND = ZERO
01226         MOVE CF-ST-FREE-LOOK-PERIOD
01227                                  TO CP-FREE-LOOK
01228      ELSE
01229         MOVE ZERO                TO CP-FREE-LOOK.
01230
01231      MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.
01232      MOVE CM-CERT-EFF-DT         TO  CP-CERT-EFF-DT.
01233      MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
01234      MOVE WS-CURRENT-DATE        TO  CP-VALUATION-DT.
01235      MOVE CM-LF-ORIG-TERM        TO  CP-ORIGINAL-TERM.
01236      MOVE PI-REM-TRM-CALC-OPTION TO  CP-REM-TRM-CALC-OPTION.
01237      MOVE '4'                    TO  CP-REM-TERM-METHOD.
01238      MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.
01239
01240      
      * EXEC CICS LINK
01241 *         PROGRAM  (ELRTRM)
01242 *         COMMAREA (CALCULATION-PASS-AREA)
01243 *         LENGTH   (CP-COMM-LENGTH)
01244 *    END-EXEC.
      *    MOVE '."C                   (   #00006027' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELRTRM, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01245
01246      IF CM-LF-CURRENT-STATUS = '1' OR '4'
01247         IF CP-REMAINING-TERM-3 = ZEROS
01248            MOVE 'EXPIRED'        TO EL127B-LI-DESC2 (EL127B-INDEX)
01249            MOVE CM-LF-BENEFIT-AMT TO EL127B-LI-AMT  (EL127B-INDEX)
01250         ELSE
01251            MOVE 'ACTIVE'        TO EL127B-LI-DESC2 (EL127B-INDEX).
01252
01253      IF CM-LF-CURRENT-STATUS = '2'
01254         MOVE 'PEND  '           TO EL127B-LI-DESC2 (EL127B-INDEX).
01255
01256      IF CM-LF-CURRENT-STATUS = '3'
01257         MOVE 'RESTORE'          TO EL127B-LI-DESC2 (EL127B-INDEX).
01258
01259      IF CM-LF-CURRENT-STATUS = '5'
01260         MOVE 'REISSUE'          TO EL127B-LI-DESC2 (EL127B-INDEX).
01261
01262      IF CM-LF-CURRENT-STATUS = '6'
01263         MOVE 'LMP DIS'          TO EL127B-LI-DESC2 (EL127B-INDEX).
01264
01265      IF CM-LF-CURRENT-STATUS = '7'
01266         MOVE 'DEATH  '          TO EL127B-LI-DESC2 (EL127B-INDEX).
01267
01268      IF CM-LF-CURRENT-STATUS = '8'
01269         MOVE 'CANCEL '          TO EL127B-LI-DESC2 (EL127B-INDEX).
01270
01271      IF CM-LF-CURRENT-STATUS = '9'
01272         MOVE 'RE-ONLY'          TO EL127B-LI-DESC2 (EL127B-INDEX).
01273
01274      IF CM-LF-CURRENT-STATUS = 'D'
01275         MOVE 'DECLINE'          TO EL127B-LI-DESC2 (EL127B-INDEX).
01276
01277      IF CM-LF-CURRENT-STATUS = 'V'
01278         MOVE 'VOID'             TO EL127B-LI-DESC2 (EL127B-INDEX).
01279
01280      IF CM-LF-CURRENT-STATUS = '6' OR '7' OR '8'
01281         NEXT SENTENCE
01282      ELSE
01283         MOVE CM-LF-BENEFIT-AMT   TO EL127B-LI-AMT  (EL127B-INDEX).
01284
01285      IF PI-COMPANY-ID = 'CRI'  OR  'PEM'
01286         MOVE CM-MEMBER-NO     TO  EL127B-MEMB-LOAN (EL127B-INDEX)
01287      ELSE
01288         MOVE CM-LOAN-NUMBER   TO  EL127B-MEMB-LOAN (EL127B-INDEX).
01289
01290  4100-A-AND-H-INFO.
01291      IF CM-AH-BENEFIT-CD = '00'
01292          GO TO 4200-CONTINUE.
01293
01294      MOVE SPACES               TO  EL127B-AH-INFO (EL127B-INDEX).
01295
01296      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.
01297      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
01298      MOVE '5'                    TO  WS-CFK-RECORD-TYPE.
01299      MOVE CM-AH-BENEFIT-CD       TO  WS-CFK-BENEFIT-NO.
01300
01301      PERFORM 8000-READ-CONTROL-FILE.
01302
01303      IF WS-NOT-FOUND = ZERO
01304          MOVE CF-BENEFIT-ALPHA (WS-INDEX)
01305                                 TO  EL127B-AH-ABVR (EL127B-INDEX).
01306
01307 * READ STATE MASTER RECORD FOR FREE LOOK PERIOD *
01308
01309      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.
01310      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
01311      MOVE '3'                    TO  WS-CFK-RECORD-TYPE.
01312      MOVE CM-STATE               TO  WS-CFK-ACCESS-TYPE.
01313
01314      PERFORM 8000-READ-CONTROL-FILE.
01315
01316      IF WS-ST-REC-NOT-FOUND = ZERO
01317         MOVE CF-ST-FREE-LOOK-PERIOD
01318                                  TO CP-FREE-LOOK
01319      ELSE
01320         MOVE ZERO                TO CP-FREE-LOOK.
01321
01322      MOVE CM-CERT-EFF-DT         TO  CP-CERT-EFF-DT.
01323      MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
01324      MOVE WS-CURRENT-DATE        TO  CP-VALUATION-DT.
01325      MOVE CM-AH-ORIG-TERM        TO  CP-ORIGINAL-TERM.
01326      MOVE PI-REM-TRM-CALC-OPTION TO  CP-REM-TRM-CALC-OPTION.
01327      MOVE '4'                    TO  CP-REM-TERM-METHOD.
01328      MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.
01329      
      * EXEC CICS LINK
01330 *         PROGRAM  (ELRTRM)
01331 *         COMMAREA (CALCULATION-PASS-AREA)
01332 *         LENGTH   (CP-COMM-LENGTH)
01333 *    END-EXEC.
      *    MOVE '."C                   (   #00006116' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELRTRM, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01334
01335      IF CM-AH-CURRENT-STATUS = '8'
01336         IF CM-AH-CANCEL-DT NOT = LOW-VALUES
01337             MOVE CM-AH-CANCEL-DT TO DC-BIN-DATE-1
01338             MOVE SPACES          TO DC-OPTION-CODE
01339             PERFORM 8500-DATE-CONVERSION
01340             IF NOT DATE-CONVERSION-ERROR
01341                 MOVE DC-GREG-DATE-1-EDIT TO EL127B-AH-DATE
01342                                                    (EL127B-INDEX).
01343
01344      IF CM-AH-CURRENT-STATUS = '6' OR '7'
01345         IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES
01346             MOVE CM-AH-SETTLEMENT-DT TO DC-BIN-DATE-1
01347             MOVE SPACES              TO DC-OPTION-CODE
01348             PERFORM 8500-DATE-CONVERSION
01349             IF NOT DATE-CONVERSION-ERROR
01350                 MOVE DC-GREG-DATE-1-EDIT TO EL127B-AH-DATE
01351                                                    (EL127B-INDEX).
01352
01353      IF CM-AH-CURRENT-STATUS = '1' OR '4'
01354        IF CP-REMAINING-TERM-3 = ZEROS
01355           MOVE 'EXPIRED'         TO EL127B-AH-DESC2 (EL127B-INDEX)
01356           MOVE CM-AH-BENEFIT-AMT TO EL127B-AH-AMT   (EL127B-INDEX)
01357        ELSE
01358           MOVE 'ACTIVE'         TO EL127B-AH-DESC2 (EL127B-INDEX).
01359
01360      IF CM-AH-CURRENT-STATUS = '2'
01361         MOVE 'PEND   '          TO EL127B-AH-DESC2 (EL127B-INDEX).
01362
01363      IF CM-AH-CURRENT-STATUS = '3'
01364         MOVE 'RESTORE'          TO EL127B-AH-DESC2 (EL127B-INDEX).
01365
01366      IF CM-AH-CURRENT-STATUS = '5'
01367         MOVE 'REISSUE'          TO EL127B-AH-DESC2 (EL127B-INDEX).
01368
01369      IF CM-AH-CURRENT-STATUS = '6'
01370         MOVE 'LMP DIS'          TO EL127B-AH-DESC2 (EL127B-INDEX).
01371
01372      IF CM-AH-CURRENT-STATUS = '7'
01373         MOVE 'DEATH  '          TO EL127B-AH-DESC2 (EL127B-INDEX).
01374
01375      IF CM-AH-CURRENT-STATUS = '8'
01376         MOVE 'CANCEL '          TO EL127B-AH-DESC2 (EL127B-INDEX).
01377
01378      IF CM-AH-CURRENT-STATUS = '9'
01379         MOVE 'RE-ONLY'          TO EL127B-AH-DESC2 (EL127B-INDEX).
01380
01381      IF CM-AH-CURRENT-STATUS = 'D'
01382         MOVE 'DECLINE'          TO EL127B-AH-DESC2 (EL127B-INDEX).
01383
01384      IF CM-AH-CURRENT-STATUS = 'V'
01385         MOVE 'VOID'             TO EL127B-AH-DESC2 (EL127B-INDEX).
01386
01387      IF CM-AH-CURRENT-STATUS = '6' OR '7' OR '8'
01388         NEXT SENTENCE
01389      ELSE
01390         MOVE CM-AH-BENEFIT-AMT   TO EL127B-AH-AMT  (EL127B-INDEX).
01391
01392  4200-CONTINUE.
01393      IF EL127B-INDEX < +8
01394          SET EL127B-INDEX UP BY +1
01395          GO TO 4010-READNEXT.
01396
01397      GO TO 4900-ENDBROWSE.
01398
01399  4700-END-OF-BROWSE.
01400      MOVE ER-0130                 TO  EMI-ERROR.
01401      ADD +1  TO  PI-END-OF-FILE.
01402
01403  4900-ENDBROWSE.
01404
01405      ADD 1 TO PI-SCREEN-COUNT.
01406
01407      
      * EXEC CICS ENDBR
01408 *        DATASET (PI-DSID)
01409 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006194' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01410
01411      IF CREDIT-SESSION
01412          IF (PI-LINE-COUNT  =  +1)  AND
01413             (PI-1ST-TIME-SW =  ZEROS)
01414              NEXT SENTENCE
01415          ELSE
01416              GO TO 4910-SEND-MAP
01417      ELSE
01418         GO TO 4910-SEND-MAP.
01419
01420      MOVE EL127B-CARRIER (1)        TO  PI-CARRIER.
01421      MOVE EL127B-GROUP   (1)        TO  PI-GROUPING.
01422      MOVE EL127B-STATE   (1)        TO  PI-STATE.
01423      MOVE EL127B-ACCOUNT (1)        TO  PI-ACCOUNT.
01424      MOVE EL127B-CERT-NO (1)        TO  PI-CERT-NO.
01425      MOVE EL127B-EFF-DATE(1)        TO  DC-GREG-DATE-1-EDIT.
01426      MOVE '2'                       TO  DC-OPTION-CODE.
01427      PERFORM 8500-DATE-CONVERSION.
01428      MOVE DC-BIN-DATE-1             TO  PI-CERT-EFF-DT.
01429
01430      MOVE PI-RETURN-TO-PROGRAM    TO  PI-CALLING-PROGRAM.
01431      MOVE PI-SAVED-PROGRAM-1      TO  PI-RETURN-TO-PROGRAM
01432                                       THIS-PGM.
01433      MOVE PI-SAVED-PROGRAM-2      TO  PI-SAVED-PROGRAM-1.
01434      MOVE PI-SAVED-PROGRAM-3      TO  PI-SAVED-PROGRAM-2.
01435      MOVE PI-SAVED-PROGRAM-4      TO  PI-SAVED-PROGRAM-3.
01436      MOVE PI-SAVED-PROGRAM-5      TO  PI-SAVED-PROGRAM-4.
01437      MOVE PI-SAVED-PROGRAM-6      TO  PI-SAVED-PROGRAM-5.
01438      MOVE SPACES                  TO  PI-SAVED-PROGRAM-6.
01439
01440      IF THIS-PGM = 'EL6311' OR 'EL626' OR 'EL126' OR
01441                    'EL1272' OR 'EL127'
01442          MOVE 'EL1273'            TO  THIS-PGM
01443          GO TO 9300-XCTL.
01444
01445  4910-SEND-MAP.
01446      IF WS-NO-CERT-FOUND
01447          MOVE +9                 TO  PI-BROWSE-SW
01448          GO TO 9400-CLEAR.
01449
01450      MOVE +1                     TO  PI-1ST-TIME-SW.
01451      MOVE -1                     TO  BSELL.
01452      GO TO 8100-SEND-INITIAL-MAP.
01453
01454      EJECT
uktdel*5000-MOVE-NAME SECTION. COPY ELCMNS REPLACING
uktins 5000-MOVE-NAME SECTION.
uktins*    COPY ELCMNS REPLACING
01456 *    CL-INSURED-LAST-NAME        BY  CM-INSURED-LAST-NAME
01457 *    CL-INSURED-1ST-NAME         BY  CM-INSURED-FIRST-NAME
01458 *    CL-INSURED-MID-INIT         BY  CM-INSURED-INITIAL2.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELCMNS.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                         *
00006 *                                                               *
00007 *                     M O V E   N A M E   R O U T I N E         *
00008 *                                                               *
00009 *                THE FOLLOWING ROUTINE MOVES THE INSURRED'S     *
00010 *            NAME FROM THE CLAIM MASTER TO A WORK AREA WITH     *
00011 *            NO EMBEDDED BLANKS.                                *
00012 *                                                               *
00013 *                  FIELD               VALUE                    *
00014 *                                                               *
00015 *                LAST NAME (CL15)      SMITH                    *
00016 *                1ST NAME  (CL12)      JOHN                     *
00017 *                MID NAME  (CL12)      ALLEN                    *
00018 *                                                               *
00019 *                AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30)  *
00020 *                                                               *
00021 *                        SMITH, JOHN ALLEN                      *
00022 *                                                               *
00023 *                TO USE THIS ROUTINE YOU ALSO NEED A WORKING    *
00024 *            STORAGE COPYBOOK:                                  *
00025 *                                                               *
00026 *                01  WS-NAME-WORK-AREA COPY ELCNWA.             *
00027 *                                                               *
00028 *****************************************************************.
00029
00030      MOVE SPACES                 TO  WS-NAME-WORK-AREA.
00031      MOVE ZERO                   TO  WS-NAME-SW.
00032      SET NWA-INDEX TO +1.
00033
00034      IF CM-INSURED-FIRST-NAME = SPACES  AND
00035         CM-INSURED-INITIAL2 = SPACES
00036          MOVE +1                 TO  WS-NAME-SW.
00037
00038      MOVE CM-INSURED-LAST-NAME  TO  WS-NAME-WORK2.
00039      PERFORM 5100-MOVE-NAME THRU 5190-EXIT.
00040
00041      MOVE CM-INSURED-FIRST-NAME   TO  WS-NAME-WORK2.
00042      PERFORM 5100-MOVE-NAME THRU 5190-EXIT.
00043
00044      SET NWA-INDEX UP BY +1.
00045      MOVE CM-INSURED-INITIAL2   TO  WS-NAME-WORK2.
00046      PERFORM 5100-MOVE-NAME THRU 5190-EXIT.
00047
00048  5000-EXIT.
00049      EXIT.
00050
00051      EJECT
00052  5100-MOVE-NAME SECTION.
00053      IF WS-NAME-SW GREATER THAN +1
00054          GO TO 5190-EXIT.
00055
00056      IF WS-NAME-WORK2 = SPACES
00057          GO TO 5190-EXIT.
00058
00059      SET NWA-INDEX2 TO +1.
00060      SET NWA-INDEX3 TO +2.
00061
00062  5110-MOVE-NAME.
00063      MOVE WS-NW2 (NWA-INDEX2)  TO  WS-NW (NWA-INDEX).
00064
00065      IF NWA-INDEX LESS THAN +30
00066          SET NWA-INDEX UP BY +1
00067        ELSE
00068          ADD +2  TO  WS-NAME-SW
00069          GO TO 5190-EXIT.
00070
00071      IF NWA-INDEX2 LESS THAN +20
00072          SET NWA-INDEX3 UP BY +1
00073          SET NWA-INDEX2 UP BY +1.
00074
00075      IF WS-NW2 (NWA-INDEX2) = SPACES AND
00076         WS-NW2 (NWA-INDEX3) = SPACES
00077          IF WS-NAME-SW = ZERO
00078              MOVE ','            TO  WS-NW (NWA-INDEX)
00079              SET NWA-INDEX UP BY +2
00080              MOVE +1             TO  WS-NAME-SW
00081              GO TO 5190-EXIT
00082            ELSE
00083              GO TO 5190-EXIT.
00084
00085      GO TO 5110-MOVE-NAME.
00086
00087  5190-EXIT.
00088      EXIT.
00089
01459
01460      EJECT
01461  6000-SET-ATTRB SECTION.
01462      MOVE AL-SANON      TO  EL127B-AST-ATTRB       (EL127B-INDEX)
01463                             EL127B-CARRIER-ATTRB   (EL127B-INDEX)
01464                             EL127B-GROUP-ATTRB     (EL127B-INDEX)
01465                             EL127B-STATE-ATTRB     (EL127B-INDEX)
01466                             EL127B-ACCOUNT-ATTRB   (EL127B-INDEX)
01467                             EL127B-CERT-NO-ATTRB   (EL127B-INDEX)
01468                             EL127B-NAME-ATTRB      (EL127B-INDEX)
01469                             EL127B-AGE-ATTRB       (EL127B-INDEX)
01470                             EL127B-SEX-ATTRB       (EL127B-INDEX)
01471                             EL127B-EFF-DATE-ATTRB  (EL127B-INDEX)
01472                             EL127B-LIFE-INFO-ATTRB (EL127B-INDEX)
01473                             EL127B-AH-INFO-ATTRB   (EL127B-INDEX).
01474
01475      IF CREDIT-SESSION
01476          MOVE AL-SANON  TO  EL127B-CERT-SEL-ATTRB  (EL127B-INDEX)
01477          MOVE AL-UNNON  TO  BSELA
01478      ELSE
01479          MOVE AL-UANON  TO  EL127B-CERT-SEL-ATTRB  (EL127B-INDEX).
01480
01481      IF PI-SAVED-PROGRAM-1 = 'EL130'
01482          MOVE AL-SANOF  TO  BPFK3A   BPFK5A
01483          MOVE SPACES    TO  BPFK3O   BPFK5O
01484      ELSE
01485          MOVE AL-SANON  TO  BPFK3A   BPFK5A.
01486
01487  6000-EXIT.
01488      EXIT.
01489
01490      EJECT
01491  7000-PF2-POSITION         SECTION.
01492
01493      
      * EXEC CICS IGNORE CONDITION
01494 *         DUPKEY
01495 *    END-EXEC.
      *    MOVE '"*$                   !   #00006372' TO DFHEIV0
           MOVE X'222A24202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01496
01497      
      * EXEC CICS HANDLE CONDITION
01498 *        NOTFND (8700-NOT-FOUND)
01499 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00006376' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303036333736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01500
01501      IF PI-PART-FIELD-SW  = 'C' OR 'S'
01502         MOVE PI-SAVE-KEY-LENGTH TO PI-KEY-LENGTH.
01503
01504      COMPUTE WS-CALC-RDNXT = PI-SCREEN-COUNT * 7.
01505      MOVE PI-1ST-KEY             TO PI-CERTIFICATE-KEY.
01506      MOVE ZERO                   TO PI-END-OF-FILE.
01507
01508      IF (OPTION-FOUR-SELECTED   AND PI-KEY-LENGTH = 12)
01509                       OR
01510         (OPTION-THREE-SELECTED  AND PI-KEY-LENGTH = 18)
01511        
      * EXEC CICS STARTBR
01512 *         DATASET   (PI-DSID)
01513 *         RIDFLD    (PI-CERTIFICATE-KEY)
01514 *         EQUAL
01515 *         KEYLENGTH (PI-KEY-LENGTH)
01516 *      END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   K     E          &   #00006390' TO DFHEIV0
           MOVE X'262C2020204B202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 PI-CERTIFICATE-KEY, 
                 PI-KEY-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01517       ELSE
01518        
      * EXEC CICS STARTBR
01519 *         DATASET   (PI-DSID)
01520 *         RIDFLD    (PI-CERTIFICATE-KEY)
01521 *         GENERIC
01522 *         EQUAL
01523 *         KEYLENGTH (PI-KEY-LENGTH)
01524 *      END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    E          &   #00006397' TO DFHEIV0
           MOVE X'262C2020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 PI-CERTIFICATE-KEY, 
                 PI-KEY-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01525
01526  7100-READNEXT-PF2.
01527      IF WS-CALC-RDNXT > ZERO
01528          NEXT SENTENCE
01529        ELSE
01530          GO TO 7999-EXIT.
01531
01532      
      * EXEC CICS READNEXT
01533 *        DATASET (PI-DSID)
01534 *        RIDFLD  (PI-CERTIFICATE-KEY)
01535 *        SET     (ADDRESS OF CERTIFICATE-MASTER)
01536 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006411' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CERTIFICATE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01537
01538 ******************************************************************
01539 *    IF THE SECURITY CHECKING ROUTINE, INITIAL COMPARE ROUTINE   *
01540 *        OR THE ACCOUNT COMPARE ROUTINE IS CHANGED HERE, YOU     *
01541 *        MUST ALSO CHANGE THE CORRESPONDING ROUTINE IN           *
01542 *        PARAGRAPH 4020-COMPARE-KEY OR 4030-CHECK-OPTION.        *
01543 *                                     KER/080884.                *
01544 ******************************************************************
01545
01546      IF PI-NO-CARRIER-SECURITY AND
01547         PI-NO-ACCOUNT-SECURITY
01548          GO TO 7130-CHECK-INITIAL.
01549
01550      IF PI-CARRIER-SECURITY > SPACES
01551          IF CM-CARRIER NOT = PI-CARRIER-SECURITY
01552              GO TO 7100-READNEXT-PF2.
01553
01554      IF PI-ACCOUNT-SECURITY > SPACES
01555          IF CM-ACCOUNT NOT = PI-ACCOUNT-SECURITY
01556              GO TO 7100-READNEXT-PF2.
01557
01558  7130-CHECK-INITIAL.
01559      IF NOT OPTION-THREE-SELECTED
01560          GO TO 7190-COMPUTE.
01561
01562      IF PI-SC-INITIALS NOT = SPACES
01563         MOVE PI-SC-INITIALS      TO WS-INITIALS
01564         IF WS-INIT2 NOT = SPACE
01565            IF PI-SC-INITIALS NOT = CM-INSURED-INITIALS
01566               GO TO 7100-READNEXT-PF2
01567              ELSE
01568               NEXT SENTENCE
01569            ELSE
01570            IF WS-INIT1 NOT = CM-INSURED-INITIAL1
01571               GO TO 7100-READNEXT-PF2.
01572
01573      IF PI-SC-FIRST-NAME = SPACE
01574          GO TO 7150-CONTINUE.
01575
01576      MOVE PI-SC-FIRST-NAME       TO WS-PI-NAME.
01577      MOVE CM-INSURED-FIRST-NAME  TO WS-CM-NAME.
01578
01579      MOVE SPACE                  TO WS-COMPARE-INDICATOR.
01580      PERFORM 4035-CHECK-NAME THRU 4035-EXIT
01581          VARYING WS-NAME-INDEX FROM 15 BY -1
01582              UNTIL WS-NAME-INDEX = ZERO.
01583
01584      IF NAME-NOT-FOUND
01585          GO TO 7100-READNEXT-PF2.
01586
01587  7150-CONTINUE.
01588      IF PI-SC-ACCT-NO = SPACES
01589          GO TO 7190-COMPUTE.
01590
01591      IF PI-SC-ACCT-NO NOT = CM-ACCOUNT
01592          GO TO 7100-READNEXT-PF2.
01593
01594  7190-COMPUTE.
01595      COMPUTE WS-CALC-RDNXT = WS-CALC-RDNXT - 1.
01596      GO TO 7100-READNEXT-PF2.
01597
01598  7999-EXIT.
01599      EXIT.
01600
01601      EJECT
01602  8000-READ-CONTROL-FILE SECTION.
01603      MOVE ZERO                   TO  WS-NOT-FOUND
01604                                      WS-ST-REC-NOT-FOUND.
01605
01606      
      * EXEC CICS HANDLE CONDITION
01607 *        NOTFND (8020-NOTFND)
01608 *    END-EXEC.
      *    MOVE '"$I                   ! % #00006485' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303036343835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01609
01610      
      * EXEC CICS READ
01611 *        DATASET   (WS-CONTROL-FILE-DSID)
01612 *        RIDFLD    (WS-CONTROL-FILE-KEY)
01613 *        SET       (ADDRESS OF CONTROL-FILE)
01614 *        GENERIC
01615 *        GTEQ
01616 *        KEYLENGTH (8)
01617 *    END-EXEC.
           MOVE 8
             TO DFHEIV11
      *    MOVE '&"S  KG    G          (   #00006489' TO DFHEIV0
           MOVE X'26225320204B472020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343839' TO DFHEIV0(25:11)
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
           
01618
01619      IF CF-RECORD-TYPE NOT = WS-CFK-RECORD-TYPE
01620          GO TO 8020-NOTFND.
01621
01622      IF WS-CFK-RECORD-TYPE = '3'
01623          GO TO 8090-EXIT.
01624
01625      MOVE +1                     TO WS-INDEX.
01626
01627  8010-FIND-BENEFIT.
01628      IF WS-CFK-BENEFIT-NO = CF-BENEFIT-CODE (WS-INDEX)
01629          GO TO 8090-EXIT.
01630
01631      IF CF-BENEFIT-CODE (WS-INDEX) NOT < CF-HI-BEN-IN-REC
01632          GO TO 8020-NOTFND.
01633
01634      IF WS-INDEX < +8
01635          ADD +1  TO  WS-INDEX
01636          GO TO 8010-FIND-BENEFIT.
01637
01638  8020-NOTFND.
01639      IF WS-CFK-RECORD-TYPE = '3'
01640         MOVE +1                  TO  WS-ST-REC-NOT-FOUND
01641         GO TO 8090-EXIT.
01642
01643      MOVE +1                     TO  WS-NOT-FOUND.
01644
01645  8090-EXIT.
01646      EXIT.
01647
01648      EJECT
01649  8100-SEND-INITIAL-MAP SECTION.
01650      MOVE SAVE-DATE              TO  ADATEO.
01651      MOVE EIBTIME                TO  TIME-IN.
01652      MOVE TIME-OUT               TO  ATIMEO.
101501     MOVE PI-COMPANY-ID          TO  ACOMPO.
101501     MOVE PI-PROCESSOR-ID        TO  AUSERIDO.
01653
01654      IF CREDIT-SESSION
01655         MOVE SPACES              TO BPFK4O
01656                                     BPFK5O.
01662
01663      IF EMI-ERROR NOT = ZERO
01664          PERFORM 9900-ERROR-FORMAT.
01665
01666      IF PI-ALT-NAME-COUNT > 140
01667          MOVE ER-0765             TO  EMI-ERROR
01668          PERFORM 9900-ERROR-FORMAT.
01669
01670      MOVE EMI-MESSAGE-AREA (1)    TO  BEMSG1O.
01671      MOVE EMI-MESSAGE-AREA (2)    TO  BEMSG2O.
101501*    IF PI-ORIGINAL-COMPANY-ID NOT = SPACES
101501*       MOVE AL-PABON            TO BPFK6A BPFK7A
101501*       MOVE PI-COMPANY-ID       TO BCOMPO
101501*    ELSE
101501*       MOVE SPACES              TO BCOMPO
101501*       MOVE AL-PADOF            TO BPFK6A BPFK7A.
01679
01680      
      * EXEC CICS SEND
01681 *        FROM   (EL127BO)
01682 *        MAPSET (WS-MAPSET-NAME)
01683 *        MAP    (WS-MAP-NAME)
01684 *        CURSOR
01685 *        ERASE
01686 *    END-EXEC.
           MOVE LENGTH OF
            EL127BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00006555' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL127BO, 
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
           
01687
01688      GO TO 9100-RETURN-TRAN.
01689
01690  8100-EXIT.
01691      EXIT.
01692
01693      EJECT
01694  8200-SEND-DATAONLY SECTION.
01695      MOVE SAVE-DATE              TO  ADATEO.
01696      MOVE EIBTIME                TO  TIME-IN.
01697      MOVE TIME-OUT               TO  ATIMEO.
101501     MOVE PI-COMPANY-ID          TO  ACOMPO.
101501     MOVE PI-PROCESSOR-ID        TO  AUSERIDO.
01698
01699      IF CREDIT-SESSION
01700         MOVE SPACES              TO BPFK4O
01701                                     BPFK5O.
01702
01703      IF EMI-ERROR NOT = ZERO
01704          PERFORM 9900-ERROR-FORMAT.
01705
01706      IF PI-ALT-NAME-COUNT > 140
01707          MOVE ER-0765             TO  EMI-ERROR
01708          PERFORM 9900-ERROR-FORMAT.
01709
01710      MOVE EMI-MESSAGE-AREA (1)   TO  BEMSG1O.
01711      MOVE EMI-MESSAGE-AREA (2)   TO  BEMSG2O.
01712
101501*    IF PI-ORIGINAL-COMPANY-ID NOT = SPACES
101501*       MOVE AL-PABON            TO BPFK6A BPFK7A
101501*       MOVE PI-COMPANY-ID       TO BCOMPO
101501*    ELSE
101501*       MOVE SPACES              TO BCOMPO
101501*       MOVE AL-PADOF            TO BPFK6A BPFK7A.
01719
01720      
      * EXEC CICS SEND DATAONLY
01721 *        FROM   (EL127BO)
01722 *        MAPSET (WS-MAPSET-NAME)
01723 *        MAP    (WS-MAP-NAME)
01724 *        CURSOR
01725 *    END-EXEC.
           MOVE LENGTH OF
            EL127BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00006597' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL127BO, 
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
           
01726
01727      GO TO 9100-RETURN-TRAN.
01728
01729  8200-EXIT.
01730      EXIT.
01731
01732      EJECT
01733  8300-SEND-TEXT SECTION.
01734      
      * EXEC CICS SEND TEXT
01735 *        FROM   (LOGOFF-TEXT)
01736 *        LENGTH (LOGOFF-LENGTH)
01737 *        ERASE
01738 *        FREEKB
01739 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00006611' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363131' TO DFHEIV0(25:11)
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
           
01740
01741      
      * EXEC CICS RETURN
01742 *    END-EXEC.
      *    MOVE '.(                    ''   #00006618' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01743
01744  8300-EXIT.
01745      EXIT.
01746
01747  8500-DATE-CONVERSION SECTION.
01748      
      * EXEC CICS LINK
01749 *        PROGRAM  ('ELDATCV')
01750 *        COMMAREA (DATE-CONVERSION-DATA)
01751 *        LENGTH   (DC-COMM-LENGTH)
01752 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00006625' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01753
01754  8500-EXIT.
01755      EXIT.
01756
01757      EJECT
01758  8600-NEXT-COMPANY SECTION.
01759 ******************************************************************
01760 ****      READ THE CURRENT COMPANY RECORD TO OBTAIN THE       ****
01761 ****      NEXT COMPANY ID.                                    ****
01762 ******************************************************************
01763
01764      MOVE SPACES                     TO  WS-CONTROL-FILE-KEY.
01765      MOVE PI-COMPANY-ID              TO  WS-CFK-COMPANY-ID.
01766      MOVE '1'                        TO  WS-CFK-RECORD-TYPE.
01767      MOVE +0                         TO  WS-CFK-SEQUENCE-NO.
01768
01769      PERFORM 8900-READ-CONTROL THRU 8900-EXIT.
01770
01771      IF WS-CNTL-REC-FOUND-SW = 'N'
01772          PERFORM 8800-INITIALIZE-MAP VARYING EL127B-INDEX
01773            FROM +1 BY +1 UNTIL EL127B-INDEX > +8
01774          MOVE ER-0022                TO  EMI-ERROR
01775          MOVE -1                     TO  BSELL
01776          GO TO 8100-SEND-INITIAL-MAP.
01777
01778      IF EIBAID = DFHPF6
01779          MOVE CF-NEXT-COMPANY-ID     TO  WS-NEXT-COMPANY-ID.
01780
01781      IF EIBAID = DFHPF7
01782          MOVE PI-ORIGINAL-COMPANY-ID TO  WS-NEXT-COMPANY-ID.
01783
01784      IF PI-PROCESSOR-ID = 'LGXX'
01785          GO TO 8600-CONTINUE-NEXT-COMPANY.
01786
01787 ******************************************************************
01788 ****      READ THE CURRENT USER RECORD FOR UPDATE AND REMOVE  ****
01789 ****      THE TERMINAL ID FROM THE RECORD.                    ****
01790 ******************************************************************
01791
01792      MOVE PI-COMPANY-ID              TO  WS-CFK-COMPANY-ID.
01793      MOVE '2'                        TO  WS-CFK-RECORD-TYPE.
01794      MOVE PI-PROCESSOR-ID            TO  WS-CFK-ACCESS-TYPE.
01795      MOVE +0                         TO  WS-CFK-SEQUENCE-NO.
01796
01797      PERFORM 8910-READ-CONTROL-UPDATE THRU 8910-EXIT.
01798
01799      IF WS-CNTL-REC-FOUND-SW = 'N'
01800          MOVE ER-0019                TO  EMI-ERROR
01801          MOVE -1                     TO  BSELL
01802          GO TO 8200-SEND-DATAONLY.
01803
01804      MOVE SPACES                     TO  CF-CURRENT-TERM-ON.
01805
01806      PERFORM 8920-REWRITE-CONTROL THRU 8920-EXIT.
01807
01808 ******************************************************************
01809 ****      READ THE USER RECORD ON THE "NEXT" COMPANY TO       ****
01810 ****      VERIFY THAT A VALID USER RECORD EXISTS:             ****
01811 ****        1.  MOVE USER CARRIER/ACCOUNT SECURITY TO PI-AREA ****
01812 ****        2.  MOVE USER SECURITY VALUES TO SECURITY CODES   ****
01813 ****            IN WORKING STORAGE
01814 ******************************************************************
01815
01816      MOVE WS-NEXT-COMPANY-ID         TO  WS-CFK-COMPANY-ID.
01817      MOVE '2'                        TO  WS-CFK-RECORD-TYPE.
01818      MOVE PI-PROCESSOR-ID            TO  WS-CFK-ACCESS-TYPE.
01819      MOVE +0                         TO  WS-CFK-SEQUENCE-NO.
01820
01821      PERFORM 8900-READ-CONTROL THRU 8900-EXIT.
01822
01823      IF WS-CNTL-REC-FOUND-SW = 'N'
01824          PERFORM 8800-INITIALIZE-MAP VARYING EL127B-INDEX
01825            FROM +1 BY +1 UNTIL EL127B-INDEX > +8
01826          MOVE ER-0228                TO  EMI-ERROR
01827          MOVE -1                     TO  BSELL
01828          GO TO 8100-SEND-INITIAL-MAP.
01829
01830      MOVE CF-PROCESSOR-CARRIER       TO  PI-CARRIER-SECURITY.
01831      MOVE CF-PROCESSOR-ACCOUNT       TO  PI-ACCOUNT-SECURITY.
01832      MOVE CF-INDIVIDUAL-APP(1)       TO  SC-CREDIT-CODES.
01833      MOVE CF-INDIVIDUAL-APP(2)       TO  SC-CLAIMS-CODES.
01834
01835 ******************************************************************
01836 ****      READ THE USER RECORD ON THE "NEXT" COMPANY FOR      ****
01837 ****      UPDATE AND MOVE THE TERMINAL ID INTO THE RECORD.    ****
01838 ******************************************************************
01839
01840      MOVE WS-NEXT-COMPANY-ID         TO  WS-CFK-COMPANY-ID.
01841      MOVE '2'                        TO  WS-CFK-RECORD-TYPE.
01842      MOVE PI-PROCESSOR-ID            TO  WS-CFK-ACCESS-TYPE.
01843      MOVE +0                         TO  WS-CFK-SEQUENCE-NO.
01844
01845      PERFORM 8910-READ-CONTROL-UPDATE THRU 8910-EXIT.
01846
01847      IF WS-CNTL-REC-FOUND-SW = 'N'
01848          MOVE ER-0228                TO  EMI-ERROR
01849          MOVE -1                     TO  BSELL
01850          GO TO 8200-SEND-DATAONLY.
01851
01852      MOVE EIBTRMID                   TO  CF-CURRENT-TERM-ON.
01853
01854      PERFORM 8920-REWRITE-CONTROL THRU 8920-EXIT.
01855
01856  8600-CONTINUE-NEXT-COMPANY.
01857 ******************************************************************
01858 ****      READ THE NEW COMPANY RECORD TO VERIFY THAT IT       ****
01859 ****      EXISTS AND THEN MOVE SPECIFIC DATA TO PI-AREA.      ****
01860 ******************************************************************
01861
01862      MOVE SPACES                     TO  WS-CONTROL-FILE-KEY.
01863      MOVE WS-NEXT-COMPANY-ID         TO  WS-CFK-COMPANY-ID.
01864      MOVE '1'                        TO  WS-CFK-RECORD-TYPE.
01865      MOVE +0                         TO  WS-CFK-SEQUENCE-NO.
01866
01867      PERFORM 8900-READ-CONTROL THRU 8900-EXIT.
01868
01869      IF WS-CNTL-REC-FOUND-SW = 'N'
01870          PERFORM 8800-INITIALIZE-MAP VARYING EL127B-INDEX
01871             FROM +1 BY +1 UNTIL EL127B-INDEX > +8
01872          MOVE ER-0089                TO  EMI-ERROR
01873          MOVE -1                     TO  BSELL
01874          GO TO 8100-SEND-INITIAL-MAP.
01875
01876      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.
01877
01878      MOVE +1                     TO  PI-START-SW
01879                                      PI-KEY-LENGTH.
01880
01881      MOVE +0                     TO  PI-1ST-TIME-SW
01882                                      PI-LINE-COUNT
01883                                      PI-AIX-RECORD-COUNT
01884                                      PI-BROWSE-SW
01885                                      PI-END-OF-FILE
01886                                      PI-TS-ITEM
01887                                      PI-SCREEN-COUNT
01888                                      PI-SUB
01889                                      PI-ALT-NAME-COUNT.
01890
01891      MOVE LOW-VALUES             TO  PI-CERT-CONTROLS-EL127 (1)
01892                                      PI-CERT-CONTROLS-EL127 (2)
01893                                      PI-CERT-CONTROLS-EL127 (3)
01894                                      PI-CERT-CONTROLS-EL127 (4)
01895                                      PI-CERT-CONTROLS-EL127 (5).
01896
01897      MOVE 'ELCERT'                   TO  PI-DSID.
01898      MOVE CF-COMPANY-CD              TO  PI-COMPANY-CD
01899                                          PI-CK-COMPANY-CD.
01900      MOVE CF-COMPANY-ID              TO  PI-COMPANY-ID.
01901      MOVE CF-COMPANY-PASSWORD        TO  PI-COMPANY-PASSWORD.
01902      MOVE CF-LGX-CREDIT-USER         TO  PI-CREDIT-USER.
01903      MOVE CF-LGX-CLAIM-USER          TO  PI-CLAIM-USER.
01904      MOVE CF-CERT-ACCESS-CONTROL     TO  PI-CERT-ACCESS-CONTROL.
01905      MOVE CF-CARRIER-CONTROL-LEVEL   TO  PI-CARRIER-CONTROL-LEVEL.
01906      MOVE CF-JOURNAL-FILE-ID         TO  PI-JOURNAL-FILE-ID.
01907      MOVE CF-LOWER-CASE-LETTERS      TO  PI-LOWER-CASE-LETTERS.
01908      MOVE CF-CLAIM-PAID-THRU-TO      TO  PI-CLAIM-PAID-THRU-TO.
01909
01910      MOVE CF-LIFE-OVERRIDE-L1    TO  PI-LIFE-OVERRIDE-L1.
01911      MOVE CF-LIFE-OVERRIDE-L2    TO  PI-LIFE-OVERRIDE-L2.
01912      MOVE CF-LIFE-OVERRIDE-L6    TO  PI-LIFE-OVERRIDE-L6.
01913      MOVE CF-LIFE-OVERRIDE-L12   TO  PI-LIFE-OVERRIDE-L12.
01914
01915      MOVE CF-AH-OVERRIDE-L1      TO  PI-AH-OVERRIDE-L1.
01916      MOVE CF-AH-OVERRIDE-L2      TO  PI-AH-OVERRIDE-L2.
01917      MOVE CF-AH-OVERRIDE-L6      TO  PI-AH-OVERRIDE-L6.
01918      MOVE CF-AH-OVERRIDE-L12     TO  PI-AH-OVERRIDE-L12.
01919
01920      IF CREDIT-SESSION
01921          MOVE CF-CURRENT-MONTH-END
01922                                  TO  PI-CR-MONTH-END-DT
01923          MOVE CF-CAR-GROUP-ACCESS-CNTL
01924                                  TO  PI-CAR-GROUP-ACCESS-CNTL
01925          MOVE CF-CR-PRINT-ADDRESS-LABELS
01926                                  TO  PI-LABEL-CONTROL
01927      ELSE
01928          MOVE CF-PRINT-ADDRESS-LABELS
01929                                  TO  PI-LABEL-CONTROL.
01930
01931  8600-EXIT.
01932      EXIT.
01933
01934  8620-WRITE-TEMP-STORAGE   SECTION.
01935
01936      
      * EXEC CICS HANDLE CONDITION
01937 *        QIDERR   (8625-WRITE-TEMP-STORAGE)
01938 *    END-EXEC.
      *    MOVE '"$N                   ! & #00006813' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303036383133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01939
01940      
      * EXEC CICS DELETEQ TS
01941 *        QUEUE  (WS-TEMP-STORAGE-KEY)
01942 *    END-EXEC.
      *    MOVE '*&                    #   #00006817' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01943
01944  8625-WRITE-TEMP-STORAGE.
01945
           move +1 to pi-ts-item
01946      MOVE -1             TO  BSELL.
01947
01948      
      * EXEC CICS WRITEQ TS
01949 *         FROM   (EL127BO)
01950 *         LENGTH (WS-TS-LENGTH)
      *         QUEUE  (WS-TEMP-STORAGE-KEY)
      *         ITEM   (PI-TS-ITEM)
      *    END-EXEC
      *    MOVE '*" I                  ''   #00006826' TO DFHEIV0
           MOVE X'2A2220492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 EL127BO, 
                 WS-TS-LENGTH, 
                 PI-TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if eibaid = DFHPF8
              add +1 to pi-ts-item
              
      * EXEC CICS WRITEQ TS
      *          FROM   (PI-PROGRAM-WORK-AREA)
      *          LENGTH (WS-WORK-LENGTH)
      *          QUEUE  (WS-TEMP-STORAGE-KEY)
      *          ITEM   (PI-TS-ITEM)
      *       END-EXEC
      *    MOVE '*" I                  ''   #00006834' TO DFHEIV0
           MOVE X'2A2220492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 PI-PROGRAM-WORK-AREA, 
                 WS-WORK-LENGTH, 
                 PI-TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           end-if
           .
01955  8630-EXIT.
01956      EXIT.
01957
01958  8650-WRITE-SECURITY-TEMP-STORE   SECTION.
01959
01960      
      * EXEC CICS HANDLE CONDITION
01961 *        QIDERR   (8651-WRITE-SECURITY)
01962 *    END-EXEC.
      *    MOVE '"$N                   ! '' #00006847' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303036383437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01963
01964      MOVE EIBTRMID               TO QID.
01965
01966  8651-WRITE-SECURITY.
01967
01968      
      * EXEC CICS WRITEQ TS
01969 *        QUEUE   (QID)
01970 *        FROM    (SECURITY-CONTROL)
01971 *        LENGTH  (SC-COMM-LENGTH)
01972 *        ITEM    (QID-ITEM)
01973 *    END-EXEC.
      *    MOVE '*" I                  ''   #00006855' TO DFHEIV0
           MOVE X'2A2220492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 QID-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01974
01975      MOVE QID                    TO PI-SECURITY-TEMP-STORE-ID.
01976
01977      IF PI-PROCESSOR-ID = 'LGXX'
01978          MOVE ALL 'Y'            TO SC-CREDIT-CODES
01979                                     SC-CLAIMS-CODES
01980                                     PI-PROCESSOR-USER-ALMIGHTY.
01981
01982  8650-EXIT.
01983      EXIT.
01984
01985      EJECT
01986  8700-NOT-FOUND SECTION.
01987      PERFORM 8800-INITIALIZE-MAP VARYING EL127B-INDEX
01988        FROM +1 BY +1 UNTIL EL127B-INDEX > +8.
01989      MOVE -1                     TO BSELL.
01990      MOVE ER-0201                TO EMI-ERROR.
01991      GO TO 8100-SEND-INITIAL-MAP.
01992
01993  8700-EXIT.
01994      EXIT.
01995
01996  8800-INITIALIZE-MAP SECTION.
01997      MOVE LOW-VALUES            TO EL127B-MAP-LINE (EL127B-INDEX).
01998  8800-EXIT.
01999      EXIT.
02000
02001      EJECT
02002  8900-READ-CONTROL SECTION.
02003
02004      
      * EXEC CICS HANDLE CONDITION
02005 *        NOTFND   (8900-NOTFND)
02006 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00006891' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303036383931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02007
02008      
      * EXEC CICS READ
02009 *        DATASET   (WS-CONTROL-FILE-DSID)
02010 *        RIDFLD    (WS-CONTROL-FILE-KEY)
02011 *        SET       (ADDRESS OF CONTROL-FILE)
02012 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006895' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383935' TO DFHEIV0(25:11)
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
           
02013
02014      MOVE 'Y'                    TO  WS-CNTL-REC-FOUND-SW.
02015      GO TO 8900-EXIT.
02016
02017  8900-NOTFND.
02018      MOVE 'N'                    TO  WS-CNTL-REC-FOUND-SW.
02019
02020  8900-EXIT.
02021      EXIT.
02022
02023  8910-READ-CONTROL-UPDATE.
02024
02025      
      * EXEC CICS HANDLE CONDITION
02026 *        NOTFND   (8910-NOTFND)
02027 *    END-EXEC.
      *    MOVE '"$I                   ! ) #00006912' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303036393132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02028
02029      
      * EXEC CICS READ
02030 *        DATASET   (WS-CONTROL-FILE-DSID)
02031 *        RIDFLD    (WS-CONTROL-FILE-KEY)
02032 *        SET       (ADDRESS OF CONTROL-FILE)
02033 *        UPDATE
02034 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006916' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393136' TO DFHEIV0(25:11)
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
           
02035
02036      MOVE 'Y'                    TO  WS-CNTL-REC-FOUND-SW.
02037      GO TO 8910-EXIT.
02038
02039  8910-NOTFND.
02040      MOVE 'N'                    TO  WS-CNTL-REC-FOUND-SW.
02041
02042  8910-EXIT.
02043      EXIT.
02044
02045  8920-REWRITE-CONTROL.
02046
02047      
      * EXEC CICS REWRITE
02048 *        DATASET   (WS-CONTROL-FILE-DSID)
02049 *        FROM      (CONTROL-FILE)
02050 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006934' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02051
02052  8920-EXIT.
02053      EXIT.
02054
02055      EJECT
02056  9000-RETURN-CICS SECTION.
02057      MOVE 'EL005'                TO  THIS-PGM.
02058      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
02059      GO TO 9300-XCTL.
02060
02061  9000-EXIT.
02062      EXIT.
02063
02064  9050-FLA-NAME.
02065      MOVE CM-INSURED-LAST-NAME   TO  SAVE-NAME.
02066      MOVE ZERO                   TO  A-SUB.
02067
02068  9050-ADD.
02069      ADD 1 TO A-SUB.
02070      IF A-SUB > 15
02071          GO TO 9050-EXIT-FIX.
02072
02073      IF SV-BYTE (A-SUB) = ' '
02074          MOVE A-SUB              TO B-SUB
02075          GO TO 9050-PASS.
02076
02077      GO TO 9050-ADD.
02078
02079  9050-PASS.
02080      ADD 1 TO A-SUB.
02081      IF A-SUB > 15
02082          GO TO 9050-EXIT-FIX.
02083
02084      IF SV-BYTE (A-SUB) = '*'
02085          MOVE '*'                TO SV-BYTE (B-SUB)
02086          MOVE ' '                TO SV-BYTE (A-SUB)
02087          GO TO 9050-EXIT-FIX.
02088
02089      GO TO 9050-PASS.
02090
02091  9050-EXIT-FIX.
02092      MOVE SAVE-NAME              TO CM-INSURED-LAST-NAME.
02093
02094  9100-RETURN-TRAN SECTION.
02095      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
02096      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
02097      MOVE EIBAID                 TO  PI-PREV-AID.
02098
02099      
      * EXEC CICS RETURN
02100 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
02101 *        LENGTH   (PI-COMM-LENGTH)
02102 *        TRANSID  (WS-TRANS-ID)
02103 *        END-EXEC.
      *    MOVE '.(CT                  ''   #00006986' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02104
02105  9100-EXIT.
02106      EXIT.
02107
02108  9300-XCTL SECTION.
02109      MOVE DFHENTER               TO  EIBAID.
02110
02111      IF THIS-PGM = 'EL1273'
02112          MOVE EIBTRMID       TO  WS-TS1-TERM-ID
02113          
      * EXEC CICS WRITEQ TS
02114 *            FROM   (PI-PROGRAM-WORK-AREA)
02115 *            LENGTH (WS-WORK-LENGTH)
02116 *            QUEUE  (WS-EL1273-TS)
02117 *        END-EXEC.
      *    MOVE '*"                    ''   #00007000' TO DFHEIV0
           MOVE X'2A2220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-EL1273-TS, 
                 PI-PROGRAM-WORK-AREA, 
                 WS-WORK-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02118
02119      
      * EXEC CICS XCTL
02120 *        PROGRAM  (THIS-PGM)
02121 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
02122 *        LENGTH   (PI-COMM-LENGTH)
02123 *    END-EXEC.
      *    MOVE '.$C                   %   #00007006' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02124
02125  9300-EXIT.
02126      EXIT.
02127
02128      EJECT
02129  9400-CLEAR SECTION.
02130      MOVE PI-RETURN-TO-PROGRAM   TO  THIS-PGM.
02131      GO TO 9300-XCTL.
02132
02133  9400-EXIT.
02134      EXIT.
02135
02136  9600-PGMIDERR SECTION.
02137      
      * EXEC CICS HANDLE CONDITION
02138 *        PGMIDERR (8300-SEND-TEXT)
02139 *    END-EXEC.
      *    MOVE '"$L                   ! * #00007024' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303037303234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02140
02141      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM
02142                                      LOGOFF-PGM.
02143      MOVE 'EL005'                TO  THIS-PGM.
02144      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
02145      MOVE SPACES                 TO  PI-ENTRY-CD-1.
02146      GO TO 9300-XCTL.
02147
02148  9600-EXIT.
02149      EXIT.
02150
02151      EJECT
02152  9900-ERROR-FORMAT SECTION.
02153
02154      
      * EXEC CICS LINK
02155 *        PROGRAM  ('EL001')
02156 *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
02157 *        LENGTH   (EMI-COMM-LENGTH)
02158 *    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   (   #00007041' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02159
02160  9900-EXIT.
02161      EXIT.
02162
02163  9990-ERROR SECTION.
02164      MOVE DFHEIBLK               TO EMI-LINE1.
02165
02166      
      * EXEC CICS LINK
02167 *        PROGRAM  ('EL004')
02168 *        COMMAREA (EMI-LINE1)
02169 *        LENGTH   (72)
02170 *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00007053' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02171
02172      MOVE -1                     TO BSELL.
02173      GO TO 8100-SEND-INITIAL-MAP.
02174

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1272' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMIDERR,
                     8700-NOT-FOUND,
                     4700-END-OF-BROWSE,
                     4015-DUPKEY,
                     9400-CLEAR,
                     9990-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 8700-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8700-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8020-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8625-WRITE-TEMP-STORAGE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8651-WRITE-SECURITY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8900-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 8910-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1272' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
