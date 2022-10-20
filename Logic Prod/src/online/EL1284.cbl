00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL1284.
00008 *
00009 *AUTHOR.           LOGIC,INC.
00010 *                  DALLAS,TEXAS.
00011
00012 *DATE-COMPILED.
00013 *SECURITY.   *****************************************************
00014 *            *                                                   *
00015 *            * THIS PROGRAM IS THE PROPERTY OF LOCIC, INC. *
00016 *            *                                                   *
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00020 *            *                                                   *
00021 *            *****************************************************
00022 *
00023 *REMARKS.     TRANSACTION - EXXG - CLAIM MEMO MAINTENANCE.
00024 *
00025 ******************************************************************
00026 *                   C H A N G E   L O G
00027 *
00028 * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
00029 *-----------------------------------------------------------------
00030 *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
00031 * EFFECTIVE    NUMBER
00032 *-----------------------------------------------------------------
010416* 010416   2015072900002   TANA  NEW CLAIM MEMO SCREEN
040416* 040416   2016021500002   TANA  CHANGE REWRITE FIELD VALID VALUES
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
00034 ******************************************************************
00035 *
00036  ENVIRONMENT DIVISION.
00037
00038      EJECT
00039  DATA DIVISION.
00040  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00041  77  FILLER  PIC X(32)  VALUE '********************************'.
00042  77  FILLER  PIC X(32)  VALUE '*     EL1284 WORKING STORAGE   *'.
00043  77  FILLER  PIC X(32)  VALUE '********* VMOD=2.005 ***********'.
00044
00045 *    COPY ELCSCTM.
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
00046 *    COPY ELCSCRTY.
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
00047
00048      EJECT
00049  01  STANDARD-AREAS.
00050      12  GETMAIN-SPACE           PIC X       VALUE SPACE.
00051      12  MAP-NAME                PIC X(8)    VALUE 'EL1284A'.
00052      12  MAPSET-NAME             PIC X(8)    VALUE 'EL1284S '.
00053      12  SCRN-NUMBER             PIC X(4)    VALUE '128D'.
00054      12  TRANS-ID                PIC X(4)    VALUE 'EXXG'.
00055      12  THIS-PGM                PIC X(8)    VALUE 'EL1284'.
00056      12  PGM-NAME                PIC X(8).
00061      12  PGM-EL1276              PIC X(8)    VALUE 'EL1276'.
00062
00063      12  ELMEMO-FILE-ID          PIC X(8)    VALUE 'ELMEMO'.
00064      12  ELCERT-FILE-ID          PIC X(8)    VALUE 'ELCERT'.
00065      12  QID.
00066          16  QID-TERM            PIC X(4).
00067          16  FILLER              PIC X(4)    VALUE '128D'.
00068      12  QID-ITEM                PIC S9(4)   COMP VALUE +0.
00069      12  SC-ITEM                 PIC S9(4)   COMP VALUE +1.
00070      12  WS-LINE                 PIC 9(3)    VALUE 0.
00074      12  WS-BLANK-LINES          PIC S9(4)   COMP VALUE +0.
00075
00076  01  WORK-AREA.
00077      12  SAVE-DATE           PIC X(8)    VALUE SPACES.
00078      12  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
00079
00080      12  ELMEMO-LENGTH           PIC S9(4)   COMP VALUE +132.
00081      12  ELMEMO-KEY-LENGTH       PIC S9(4)   COMP VALUE +23.
00082      12  ELMEMO-START-LENGTH     PIC S9(4)   COMP VALUE +21.
00083      12  ELMEMO-KEY.
00084          16  ELMEMO-PARTIAL-KEY.
00085              20 ELMEMO-COMPANY-CD    PIC X.
00024              20 ELMEMO-RECORD-TYPE   PIC X.
00086              20 ELMEMO-CARRIER       PIC X.
00087              20 ELMEMO-CLAIM-NO      PIC  X(7).
00091              20 ELMEMO-CERT-NO.
00092                 25 ELMEMO-CERT-PRIME PIC X(10).
00093                 25 ELMEMO-CERT-SFX   PIC X.
00095          16 ELMEMO-SEQ           PIC S9(4) COMP.
00096      12  SV-PRIOR-KEY.
00097          20 SV-COMPANY-CD            PIC X.
00024          20 SV-RECORD-TYPE           PIC X.
00098          20 SV-CARRIER               PIC X.
00099          20 SV-CLAIM-NO              PIC  X(7).
00103          20 SV-CERT-NO.
00104             25 SV-CERT-PRIME         PIC X(10).
00105             25 SV-CERT-SFX           PIC X(1).
00107      12  ELCERT-KEY.
00108          16  ELCERT-COMPANY-CD        PIC X.
00109          16  ELCERT-CARRIER           PIC X.
00110          16  ELCERT-GROUPING          PIC X(6).
00111          16  ELCERT-STATE             PIC XX.
00112          16  ELCERT-ACCOUNT           PIC X(10).
00113          16  ELCERT-EFF-DT            PIC XX.
00114          16  ELCERT-CERT-NO.
00115              20  ELCERT-CERT-PRIME    PIC X(10).
00116              20  ELCERT-CERT-SFX      PIC X.
00117
00118      12  TIME-IN                 PIC S9(7).
00119      12  TIME-SPLIT REDEFINES TIME-IN.
00120          16  FILLER              PIC X.
00121          16  TIME-OUT            PIC 99V99.
00122          16  FILLER              PIC X(2).
00123      12  XCTL-005                PIC X(8)    VALUE 'EL005'.
00124      12  XCTL-126                PIC X(8)    VALUE 'EL126'.
00125      12  LINK-001                PIC X(8)    VALUE 'EL001'.
00126      12  LINK-004                PIC X(8)    VALUE 'EL004'.
00127      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.
00128      12  MAX-LINES               PIC 999     VALUE 300.
00129      12  NUM-LINES-PER-SCREEN    PIC 99      VALUE 10.
00130      12  TS-NUM-REC-IN-GROUP     PIC 99      VALUE 50.
00131      12  TS-GROUP-WORK           PIC 9(5)    VALUE 0  COMP-3.
00132      12  TS-LENGTH               PIC S9(4)   VALUE +3650 COMP.
00133      12  ROLL-COUNTER            PIC S999    VALUE +0 COMP-3.
00134      12  TEMP-CURR-LINE          PIC S9(3)   COMP-3.
00135      12  WS-SUB                  PIC S9(3)   COMP-3.
00136      12  WS-SUB1                 PIC S9(3)   COMP-3.
00239      12  WS-ERASE-EOF            PIC X       VALUE X'80'.
00240      12  DEEDIT-DATE-INPUT.
00241          16  FILLER              PIC XX.
00242          16  DEEDIT-DATE         PIC X(6).
00137      12  WS-CURSOR-SET           PIC X       VALUE 'N'.
00138          88 CURSOR-SET                       VALUE 'Y'.
00137      12  WS-SCREEN-LINE          PIC X       VALUE 'N'.
00138          88 SCREEN-LINE-FOUND                VALUE 'Y'.
00139
00208      12  ELMSTR-KEY.
00209          16  MSTR-COMP-CD    PIC X.
00210          16  MSTR-CARRIER    PIC X.
00211          16  MSTR-CLAIM-NO   PIC X(7).
00212          16  MSTR-CERT-NO.
00213              20  MSTR-CERT-NO-PRIME  PIC X(10).
00214              20  MSTR-CERT-NO-SUFX   PIC X.
00552  01  COMP-LENGTHS.
00553      12  CNTL-GENERIC-LENGTH     PIC S9(4)   COMP VALUE +8.
00554      12  JOURNAL-LENGTH          PIC S9(4)   COMP VALUE +0.
00555      12  DATE-LENGTH             PIC S9(4)   COMP VALUE +8.
00556      12  MO-YR-LENGTH            PIC S9(4)   COMP VALUE +5.
00557      12  TERM-LENGTH             PIC S9(4)   COMP VALUE +3.
00558      12  BEN-LENGTH              PIC S9(4)   COMP VALUE +11.
00559      12  FREQ-LENGTH             PIC S9(4)   COMP VALUE +2.
00560      12  MSTR-LENGTH             PIC S9(4)   COMP VALUE +350.
00561      12  CERT-LENGTH             PIC S9(4)   COMP VALUE +450.
00562      12  TRLR-LENGTH             PIC S9(4)   COMP VALUE +200.
00563      12  ACTQ-LENGTH             PIC S9(4)   COMP VALUE +60.
00564      12  CHKQ-LENGTH             PIC S9(4)   COMP VALUE +100.
00565      12  ARCH-LENGTH             PIC S9(4)   COMP VALUE +90.
00566      12  ALPH-LENGTH             PIC S9(4)   COMP VALUE +128.
00159      EJECT
00160  01  ERROR-MESSAGES.
00161      12  ER-0000             PIC X(04)       VALUE '0000'.
00162      12  ER-0004             PIC X(04)       VALUE '0004'.
00163      12  ER-0006             PIC X(04)       VALUE '0006'.
00164      12  ER-0008             PIC X(04)       VALUE '0008'.
00165      12  ER-0023             PIC X(04)       VALUE '0023'.
00166      12  ER-0029             PIC X(04)       VALUE '0029'.
00167      12  ER-0030             PIC X(04)       VALUE '0030'.
00168      12  ER-0031             PIC X(04)       VALUE '0031'.
00169      12  ER-0032             PIC X(04)       VALUE '0032'.
00170      12  ER-0033             PIC X(04)       VALUE '0033'.
00171      12  ER-0041             PIC X(04)       VALUE '0041'.
00172      12  ER-0044             PIC X(04)       VALUE '0044'.
00173      12  ER-0045             PIC X(04)       VALUE '0045'.
00173      12  ER-0046             PIC X(04)       VALUE '0046'.
00174      12  ER-0047             PIC X(04)       VALUE '0047'.
00175      12  ER-0048             PIC X(04)       VALUE '0048'.
00176      12  ER-0049             PIC X(04)       VALUE '0049'.
00177      12  ER-0050             PIC X(04)       VALUE '0050'.
00178      12  ER-0051             PIC X(04)       VALUE '0051'.
00179      12  ER-0066             PIC X(04)       VALUE '0066'.
00180      12  ER-0067             PIC X(04)       VALUE '0067'.
00181      12  ER-0069             PIC X(04)       VALUE '0069'.
00182      12  ER-0070             PIC X(04)       VALUE '0070'.
00183      12  ER-0140             PIC X(04)       VALUE '0140'.
           12  ER-0314             PIC X(04)       VALUE '0314'.
00184      12  ER-2954             PIC X(04)       VALUE '2954'.
040416     12  ER-3846             PIC X(04)       VALUE '3846'.
00185      EJECT
00186 *                       COPY ELCLOGOF.
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
00187      EJECT
00188 *                       COPY ELCAID.
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
00189  01  FILLER  REDEFINES DFHAID.
00190      12  FILLER                  PIC X(8).
00191      12  PF-VALUES OCCURS 24 TIMES       PIC X.
00192      EJECT
00193 *                       COPY ELCEMIB.
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
00194      EJECT
00195 *                       COPY ELCINTF.
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
00196      EJECT
00197      12  PI-EL1284-AREA    REDEFINES PI-PROGRAM-WORK-AREA.
00198          16  FILLER              PIC X(318).
00199          16  PI-TOTAL-LINES      PIC S9(3).
00200          16  PI-CURRENT-LINE     PIC S9(3)   COMP-3.
00201          16  PI-TEMP-STOR-ITEMS  PIC S9(4)   COMP.
00202          16  PI-UPDATE-SW        PIC X.
00203              88  PI-CHANGES-MADE             VALUE '1'.
               16  PI-LONG-HLTH-APP    PIC  X(01).
               16  PI-REWRITE          PIC  X(01).
               16  PI-CHKCOVG          PIC  X(01).
               16  PI-MR-RELEASE-DATE  PIC  X(08).
00214          16  PI-BILLING-NOTES-EXIST PIC X.
00215          16  PI-CERT-NOTES-EXIST    PIC X.
00216          16  PI-CLAIM-NOTES-EXIST   PIC X.
00217          16  PI-SET-NOTE-CHANGE  PIC X.
00218              88 PI-CHANGE-IN-NOTE-TYPE       VALUE 'Y'.
00219      EJECT
00220 *                          COPY ELCATTR.
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
00221      EJECT
00222 *                          COPY ELCDATE.
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
00223      EJECT
00224 *                          COPY EL1284S.
       01  EL1284AI.
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
           05  CMPNYIDL PIC S9(0004) COMP.
           05  CMPNYIDF PIC  X(0001).
           05  FILLER REDEFINES CMPNYIDF.
               10  CMPNYIDA PIC  X(0001).
           05  CMPNYIDI PIC  X(0003).
      *    -------------------------------
           05  USERIDL PIC S9(0004) COMP.
           05  USERIDF PIC  X(0001).
           05  FILLER REDEFINES USERIDF.
               10  USERIDA PIC  X(0001).
           05  USERIDI PIC  X(0004).
      *    -------------------------------
           05  CLAIMNOL PIC S9(0004) COMP.
           05  CLAIMNOF PIC  X(0001).
           05  FILLER REDEFINES CLAIMNOF.
               10  CLAIMNOA PIC  X(0001).
           05  CLAIMNOI PIC  X(0007).
      *    -------------------------------
           05  TYPEL PIC S9(0004) COMP.
           05  TYPEF PIC  X(0001).
           05  FILLER REDEFINES TYPEF.
               10  TYPEA PIC  X(0001).
           05  TYPEI PIC  X(0006).
      *    -------------------------------
           05  CARRL PIC S9(0004) COMP.
           05  CARRF PIC  X(0001).
           05  FILLER REDEFINES CARRF.
               10  CARRA PIC  X(0001).
           05  CARRI PIC  X(0001).
      *    -------------------------------
           05  CERTNOL PIC S9(0004) COMP.
           05  CERTNOF PIC  X(0001).
           05  FILLER REDEFINES CERTNOF.
               10  CERTNOA PIC  X(0001).
           05  CERTNOI PIC  X(0011).
      *    -------------------------------
           05  FSTNMEL PIC S9(0004) COMP.
           05  FSTNMEF PIC  X(0001).
           05  FILLER REDEFINES FSTNMEF.
               10  FSTNMEA PIC  X(0001).
           05  FSTNMEI PIC  X(0012).
      *    -------------------------------
           05  LASTNMEL PIC S9(0004) COMP.
           05  LASTNMEF PIC  X(0001).
           05  FILLER REDEFINES LASTNMEF.
               10  LASTNMEA PIC  X(0001).
           05  LASTNMEI PIC  X(0015).
      *    -------------------------------
           05  TOTL PIC S9(0004) COMP.
           05  TOTF PIC  X(0001).
           05  FILLER REDEFINES TOTF.
               10  TOTA PIC  X(0001).
           05  TOTI PIC  X(0003).
      *    -------------------------------
           05  LNGHLAPL PIC S9(0004) COMP.
           05  LNGHLAPF PIC  X(0001).
           05  FILLER REDEFINES LNGHLAPF.
               10  LNGHLAPA PIC  X(0001).
           05  LNGHLAPI PIC  X(0001).
      *    -------------------------------
           05  REWRITEL PIC S9(0004) COMP.
           05  REWRITEF PIC  X(0001).
           05  FILLER REDEFINES REWRITEF.
               10  REWRITEA PIC  X(0001).
           05  REWRITEI PIC  X(0001).
      *    -------------------------------
           05  CHKCOVGL PIC S9(0004) COMP.
           05  CHKCOVGF PIC  X(0001).
           05  FILLER REDEFINES CHKCOVGF.
               10  CHKCOVGA PIC  X(0001).
           05  CHKCOVGI PIC  X(0001).
      *    -------------------------------
           05  MRRELDTL PIC S9(0004) COMP.
           05  MRRELDTF PIC  X(0001).
           05  FILLER REDEFINES MRRELDTF.
               10  MRRELDTA PIC  X(0001).
           05  MRRELDTI PIC  X(0008).
      *    -------------------------------
           05  LN1L PIC S9(0004) COMP.
           05  LN1F PIC  X(0001).
           05  FILLER REDEFINES LN1F.
               10  LN1A PIC  X(0001).
           05  LN1I PIC  X(0003).
      *    -------------------------------
           05  FNLINE1L PIC S9(0004) COMP.
           05  FNLINE1F PIC  X(0001).
           05  FILLER REDEFINES FNLINE1F.
               10  FNLINE1A PIC  X(0001).
           05  FNLINE1I PIC  X(0063).
      *    -------------------------------
           05  MTBY1L PIC S9(0004) COMP.
           05  MTBY1F PIC  X(0001).
           05  FILLER REDEFINES MTBY1F.
               10  MTBY1A PIC  X(0001).
           05  MTBY1I PIC  X(0004).
      *    -------------------------------
           05  MTDT1L PIC S9(0004) COMP.
           05  MTDT1F PIC  X(0001).
           05  FILLER REDEFINES MTDT1F.
               10  MTDT1A PIC  X(0001).
           05  MTDT1I PIC  X(0006).
      *    -------------------------------
           05  LN2L PIC S9(0004) COMP.
           05  LN2F PIC  X(0001).
           05  FILLER REDEFINES LN2F.
               10  LN2A PIC  X(0001).
           05  LN2I PIC  X(0003).
      *    -------------------------------
           05  FNLINE2L PIC S9(0004) COMP.
           05  FNLINE2F PIC  X(0001).
           05  FILLER REDEFINES FNLINE2F.
               10  FNLINE2A PIC  X(0001).
           05  FNLINE2I PIC  X(0063).
      *    -------------------------------
           05  MTBY2L PIC S9(0004) COMP.
           05  MTBY2F PIC  X(0001).
           05  FILLER REDEFINES MTBY2F.
               10  MTBY2A PIC  X(0001).
           05  MTBY2I PIC  X(0004).
      *    -------------------------------
           05  MTDT2L PIC S9(0004) COMP.
           05  MTDT2F PIC  X(0001).
           05  FILLER REDEFINES MTDT2F.
               10  MTDT2A PIC  X(0001).
           05  MTDT2I PIC  X(0006).
      *    -------------------------------
           05  LN3L PIC S9(0004) COMP.
           05  LN3F PIC  X(0001).
           05  FILLER REDEFINES LN3F.
               10  LN3A PIC  X(0001).
           05  LN3I PIC  X(0003).
      *    -------------------------------
           05  FNLINE3L PIC S9(0004) COMP.
           05  FNLINE3F PIC  X(0001).
           05  FILLER REDEFINES FNLINE3F.
               10  FNLINE3A PIC  X(0001).
           05  FNLINE3I PIC  X(0063).
      *    -------------------------------
           05  MTBY3L PIC S9(0004) COMP.
           05  MTBY3F PIC  X(0001).
           05  FILLER REDEFINES MTBY3F.
               10  MTBY3A PIC  X(0001).
           05  MTBY3I PIC  X(0004).
      *    -------------------------------
           05  MTDT3L PIC S9(0004) COMP.
           05  MTDT3F PIC  X(0001).
           05  FILLER REDEFINES MTDT3F.
               10  MTDT3A PIC  X(0001).
           05  MTDT3I PIC  X(0006).
      *    -------------------------------
           05  LN4L PIC S9(0004) COMP.
           05  LN4F PIC  X(0001).
           05  FILLER REDEFINES LN4F.
               10  LN4A PIC  X(0001).
           05  LN4I PIC  X(0003).
      *    -------------------------------
           05  FNLINE4L PIC S9(0004) COMP.
           05  FNLINE4F PIC  X(0001).
           05  FILLER REDEFINES FNLINE4F.
               10  FNLINE4A PIC  X(0001).
           05  FNLINE4I PIC  X(0063).
      *    -------------------------------
           05  MTBY4L PIC S9(0004) COMP.
           05  MTBY4F PIC  X(0001).
           05  FILLER REDEFINES MTBY4F.
               10  MTBY4A PIC  X(0001).
           05  MTBY4I PIC  X(0004).
      *    -------------------------------
           05  MTDT4L PIC S9(0004) COMP.
           05  MTDT4F PIC  X(0001).
           05  FILLER REDEFINES MTDT4F.
               10  MTDT4A PIC  X(0001).
           05  MTDT4I PIC  X(0006).
      *    -------------------------------
           05  LN5L PIC S9(0004) COMP.
           05  LN5F PIC  X(0001).
           05  FILLER REDEFINES LN5F.
               10  LN5A PIC  X(0001).
           05  LN5I PIC  X(0003).
      *    -------------------------------
           05  FNLINE5L PIC S9(0004) COMP.
           05  FNLINE5F PIC  X(0001).
           05  FILLER REDEFINES FNLINE5F.
               10  FNLINE5A PIC  X(0001).
           05  FNLINE5I PIC  X(0063).
      *    -------------------------------
           05  MTBY5L PIC S9(0004) COMP.
           05  MTBY5F PIC  X(0001).
           05  FILLER REDEFINES MTBY5F.
               10  MTBY5A PIC  X(0001).
           05  MTBY5I PIC  X(0004).
      *    -------------------------------
           05  MTDT5L PIC S9(0004) COMP.
           05  MTDT5F PIC  X(0001).
           05  FILLER REDEFINES MTDT5F.
               10  MTDT5A PIC  X(0001).
           05  MTDT5I PIC  X(0006).
      *    -------------------------------
           05  LN6L PIC S9(0004) COMP.
           05  LN6F PIC  X(0001).
           05  FILLER REDEFINES LN6F.
               10  LN6A PIC  X(0001).
           05  LN6I PIC  X(0003).
      *    -------------------------------
           05  FNLINE6L PIC S9(0004) COMP.
           05  FNLINE6F PIC  X(0001).
           05  FILLER REDEFINES FNLINE6F.
               10  FNLINE6A PIC  X(0001).
           05  FNLINE6I PIC  X(0063).
      *    -------------------------------
           05  MTBY6L PIC S9(0004) COMP.
           05  MTBY6F PIC  X(0001).
           05  FILLER REDEFINES MTBY6F.
               10  MTBY6A PIC  X(0001).
           05  MTBY6I PIC  X(0004).
      *    -------------------------------
           05  MTDT6L PIC S9(0004) COMP.
           05  MTDT6F PIC  X(0001).
           05  FILLER REDEFINES MTDT6F.
               10  MTDT6A PIC  X(0001).
           05  MTDT6I PIC  X(0006).
      *    -------------------------------
           05  LN7L PIC S9(0004) COMP.
           05  LN7F PIC  X(0001).
           05  FILLER REDEFINES LN7F.
               10  LN7A PIC  X(0001).
           05  LN7I PIC  X(0003).
      *    -------------------------------
           05  FNLINE7L PIC S9(0004) COMP.
           05  FNLINE7F PIC  X(0001).
           05  FILLER REDEFINES FNLINE7F.
               10  FNLINE7A PIC  X(0001).
           05  FNLINE7I PIC  X(0063).
      *    -------------------------------
           05  MTBY7L PIC S9(0004) COMP.
           05  MTBY7F PIC  X(0001).
           05  FILLER REDEFINES MTBY7F.
               10  MTBY7A PIC  X(0001).
           05  MTBY7I PIC  X(0004).
      *    -------------------------------
           05  MTDT7L PIC S9(0004) COMP.
           05  MTDT7F PIC  X(0001).
           05  FILLER REDEFINES MTDT7F.
               10  MTDT7A PIC  X(0001).
           05  MTDT7I PIC  X(0006).
      *    -------------------------------
           05  LN8L PIC S9(0004) COMP.
           05  LN8F PIC  X(0001).
           05  FILLER REDEFINES LN8F.
               10  LN8A PIC  X(0001).
           05  LN8I PIC  X(0003).
      *    -------------------------------
           05  FNLINE8L PIC S9(0004) COMP.
           05  FNLINE8F PIC  X(0001).
           05  FILLER REDEFINES FNLINE8F.
               10  FNLINE8A PIC  X(0001).
           05  FNLINE8I PIC  X(0063).
      *    -------------------------------
           05  MTBY8L PIC S9(0004) COMP.
           05  MTBY8F PIC  X(0001).
           05  FILLER REDEFINES MTBY8F.
               10  MTBY8A PIC  X(0001).
           05  MTBY8I PIC  X(0004).
      *    -------------------------------
           05  MTDT8L PIC S9(0004) COMP.
           05  MTDT8F PIC  X(0001).
           05  FILLER REDEFINES MTDT8F.
               10  MTDT8A PIC  X(0001).
           05  MTDT8I PIC  X(0006).
      *    -------------------------------
           05  LN9L PIC S9(0004) COMP.
           05  LN9F PIC  X(0001).
           05  FILLER REDEFINES LN9F.
               10  LN9A PIC  X(0001).
           05  LN9I PIC  X(0003).
      *    -------------------------------
           05  FNLINE9L PIC S9(0004) COMP.
           05  FNLINE9F PIC  X(0001).
           05  FILLER REDEFINES FNLINE9F.
               10  FNLINE9A PIC  X(0001).
           05  FNLINE9I PIC  X(0063).
      *    -------------------------------
           05  MTBY9L PIC S9(0004) COMP.
           05  MTBY9F PIC  X(0001).
           05  FILLER REDEFINES MTBY9F.
               10  MTBY9A PIC  X(0001).
           05  MTBY9I PIC  X(0004).
      *    -------------------------------
           05  MTDT9L PIC S9(0004) COMP.
           05  MTDT9F PIC  X(0001).
           05  FILLER REDEFINES MTDT9F.
               10  MTDT9A PIC  X(0001).
           05  MTDT9I PIC  X(0006).
      *    -------------------------------
           05  LN10L PIC S9(0004) COMP.
           05  LN10F PIC  X(0001).
           05  FILLER REDEFINES LN10F.
               10  LN10A PIC  X(0001).
           05  LN10I PIC  X(0003).
      *    -------------------------------
           05  FNLIN10L PIC S9(0004) COMP.
           05  FNLIN10F PIC  X(0001).
           05  FILLER REDEFINES FNLIN10F.
               10  FNLIN10A PIC  X(0001).
           05  FNLIN10I PIC  X(0063).
      *    -------------------------------
           05  MTBY10L PIC S9(0004) COMP.
           05  MTBY10F PIC  X(0001).
           05  FILLER REDEFINES MTBY10F.
               10  MTBY10A PIC  X(0001).
           05  MTBY10I PIC  X(0004).
      *    -------------------------------
           05  MTDT10L PIC S9(0004) COMP.
           05  MTDT10F PIC  X(0001).
           05  FILLER REDEFINES MTDT10F.
               10  MTDT10A PIC  X(0001).
           05  MTDT10I PIC  X(0006).
      *    -------------------------------
           05  ERRMSGBL PIC S9(0004) COMP.
           05  ERRMSGBF PIC  X(0001).
           05  FILLER REDEFINES ERRMSGBF.
               10  ERRMSGBA PIC  X(0001).
           05  ERRMSGBI PIC  X(0072).
      *    -------------------------------
           05  FUNCTL PIC S9(0004) COMP.
           05  FUNCTF PIC  X(0001).
           05  FILLER REDEFINES FUNCTF.
               10  FUNCTA PIC  X(0001).
           05  FUNCTI PIC  X(0001).
      *    -------------------------------
           05  LINE1L PIC S9(0004) COMP.
           05  LINE1F PIC  X(0001).
           05  FILLER REDEFINES LINE1F.
               10  LINE1A PIC  X(0001).
           05  LINE1I PIC  999.
      *    -------------------------------
           05  LINE2L PIC S9(0004) COMP.
           05  LINE2F PIC  X(0001).
           05  FILLER REDEFINES LINE2F.
               10  LINE2A PIC  X(0001).
           05  LINE2I PIC  999.
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  99.
      *    -------------------------------
           05  PF6NOTEL PIC S9(0004) COMP.
           05  PF6NOTEF PIC  X(0001).
           05  FILLER REDEFINES PF6NOTEF.
               10  PF6NOTEA PIC  X(0001).
           05  PF6NOTEI PIC  X(0017).
       01  EL1284AO REDEFINES EL1284AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPNYIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLAIMNOO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPEO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTNOO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FSTNMEO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LASTNMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOTO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LNGHLAPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REWRITEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKCOVGO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MRRELDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN1O PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FNLINE1O PIC  X(0063).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTBY1O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTDT1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN2O PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FNLINE2O PIC  X(0063).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTBY2O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTDT2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN3O PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FNLINE3O PIC  X(0063).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTBY3O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTDT3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN4O PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FNLINE4O PIC  X(0063).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTBY4O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTDT4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN5O PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FNLINE5O PIC  X(0063).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTBY5O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTDT5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN6O PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FNLINE6O PIC  X(0063).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTBY6O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTDT6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN7O PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FNLINE7O PIC  X(0063).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTBY7O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTDT7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN8O PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FNLINE8O PIC  X(0063).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTBY8O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTDT8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN9O PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FNLINE9O PIC  X(0063).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTBY9O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTDT9O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN10O PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FNLIN10O PIC  X(0063).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTBY10O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTDT10O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSGBO PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FUNCTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PF6NOTEO PIC  X(0017).
      *    -------------------------------
00225      EJECT
00226 *01  EL1284R REDEFINES EL1284AI.
       66 GROUP2  RENAMES LN1L THRU MTDT10I.
       01  EL1284R.
00227 *    12  FILLER                  PIC X(202).
00228      12  SC-ALL-LINES.
00229       14 SC-LINES OCCURS 10 TIMES INDEXED BY SC-INDX.
00230          16  SC-LINL             PIC S9(4)   COMP.
00231          16  SC-LINA             PIC X.
00232          16  SC-LIN              PIC ZZ9.
00233          16  SC-TEXTL            PIC S9(4)   COMP.
00234          16  SC-TEXTA            PIC X.
00235          16  SC-TEXT             PIC X(63).
00233          16  SC-MTBYL            PIC S9(4)   COMP.
00234          16  SC-MTBYA            PIC X.
00235          16  SC-MTBY             PIC X(4).
00233          16  SC-MTDTL            PIC S9(4)   COMP.
00234          16  SC-MTDTA            PIC X.
00235          16  SC-MTDT             PIC X(6).
00236 *    12  FILLER                  PIC X(116).
00237      EJECT
00238  01  RECORD-TABLE                PIC X(21900) VALUE SPACES.
00239  01  REC-TABLE  REDEFINES RECORD-TABLE.
00240      12  TS-GROUP OCCURS 6 TIMES INDEXED BY TS-INDX PIC X(3650).
00241  01  REC-ENTRIES REDEFINES RECORD-TABLE.
00242      12  REC-ENT OCCURS 300 TIMES INDEXED BY TB-INDX TB-INDX1.
00243          16  REC-TEXT                    PIC X(63).
00244          16  REC-LAST-MAINT-BY           PIC XXXX.
00245          16  REC-LAST-MAINT-DT           PIC XX.
00246          16  REC-LAST-MAINT-HHMMSS       PIC S9(7) COMP-3.
00247
00248  01  TS-WORK-AREA                        PIC X(3650).
00249      EJECT
00257 *        COPY ELCMSTR.
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
031213* 031213    2012113000002  PEMA  ADD ACCIDENT INDICATOR
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
081817* 081817    2016100700001  TANA  ADD NBR OF EXTENSIONS
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
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
052614*            88  FAMILY-LEAVE-CLAIM     VALUE 'F'.
100518*            88  OTHER-CLAIM            VALUE 'O'.
022122*            88  hospital-claim         value 'H'.
022122*            88  bereavement-claim      value 'B'.
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
051414         16  FILLER REDEFINES CL-MICROFILM-NO.
051414             20  CL-BENEFIT-PERIOD   PIC 99.
051414             20  FILLER              PIC X(8).
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
031213         16  CL-CRITICAL-PERIOD      PIC 99.
031213*        16  FILLER                  PIC XX.
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
081817         16  CL-NO-OF-EXTENSIONS     PIC 99.
081817         16  filler                  pic x(3).
      *        16  CL-CRIT-PER-RECURRENT   PIC X.
      *        16  CL-CRIT-PER-RTW-MOS     PIC 99.
      *        16  CL-RTW-DT               PIC XX.
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
CIDMOD     12  CL-YESNOSW                  PIC X.
031213     12  CL-ACCIDENT-CLAIM-SW        PIC X.
031213         88  CL-ACCIDENT-NOT-SET           VALUE ' '.
031213         88  CL-CLAIM-DUE-TO-ACCIDENT      VALUE 'Y'.
031213         88  CL-CLAIM-NOT-DUE-TO-ACCIDENT  VALUE 'N'.
051414     12  cl-insured-type             pic x.
051414         88  cl-claim-on-primary         value 'P'.
051414         88  cl-claim-on-co-borrower     value 'C'.
031213     12  cl-benefit-expiration-dt    PIC XX.
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
00252  01  DFHCOMMAREA                 PIC X(1500).
00253
00250 *        COPY ELCMEMO.
00001 ******************************************************************
00002 *                                                                *
00003 *                                                                *
00004 *                           ELCMEMO.                             *
00005 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE           *
00006 *                                                                *
00007 *                                                                *
00008 *   FILE DESCRIPTION = CLAIM MEMO NOTES                          *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 132    RECFORM = FIXED                         *
00012 *                                                                *
00013 *   BASE CLUSTER NAME = ELMEMO             RKP=2,LEN=23          *
00014 *       ALTERNATE INDEX = NONE                                   *
00015 *                                                                *
00016 *   LOG = YES                                                    *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 ******************************************************************
00019  01  CLAIM-MEMO-FILE.
00020      12  MM-RECORD-ID                  PIC  XX.
00021          88  VALID-MM-ID                    VALUE 'MM'.
00022      12  MM-CONTROL-PRIMARY.
00023        16  MM-COMPANY-CD               PIC  X.
00024        16  MM-RECORD-TYPE              PIC  X.
00025            88  MM-HDR-REC                   VALUE '1'.
00026            88  MM-DETAIL-REC                VALUE '2'.
00027        16  MM-CARRIER                  PIC  X.
00028        16  MM-CLAIM-NO                 PIC  X(7).
00029        16  MM-CERT-NO.
00030            20  MM-CERT-PRIME           PIC  X(10).
00031            20  MM-CERT-SFX             PIC  X.
00032        16  MM-PAYMENT-SEQ-NO           PIC  S9(4)  COMP.
00033
00034    12  MM-CLAIM-MEMO                   PIC  X(63).
00035    12  MM-HEADER-INFO REDEFINES MM-CLAIM-MEMO.
00036        16  MM-LONG-HEALTH-APP          PIC  X(01).
00037        16  MM-REWRITE-IND              PIC  X(01).
00037        16  MM-CHECKED-OTHER-COVG       PIC  X(01).
00038        16  MM-MR-RELEASED-FROM-DATE    PIC  X(02).
00039    12  FILLER                          PIC  X(34).
00040
00041    12  MM-LAST-MAINT-DT                PIC  XX.
00042    12  MM-LAST-MAINT-BY                PIC  X(4).
00043    12  MM-LAST-MAINT-HHMMSS            PIC  S9(6) COMP-3.
00255
00256 *        COPY ELCCERT.
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
090314* 090314  CR2014081300001  PEMA  LOAD CERTS INVOLVED IN THAO
010716* 010716  CR2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
062017* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
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
090314         16  cm-temp-epiq                  pic xx.
090314             88  EPIQ-CLASS                  value 'EQ'.
090314*        16  FILLER                        PIC XX.
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
010716         16  CM-CANCEL-FEE                 PIC S9(3)V99  COMP-3.
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
062017         16  CM-REF-INTERFACE-SW           PIC X.
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
                   88  CM-LF-POLICY-IS-CASH         VALUE 'C'.
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
                   88  CM-AH-POLICY-IS-CASH         VALUE 'C'.
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
062017     12  CM-INT-ON-REFS                    PIC S9(7)V99   COMP-3.
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
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CLAIM-MEMO-FILE
                                CERTIFICATE-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1284' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00259
00260      MOVE EIBTRMID               TO QID-TERM.
00261      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00262      MOVE '5'                    TO DC-OPTION-CODE.
00263      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00264      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00265      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00266
00267      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK
00268      IF EIBCALEN = ZEROS
00269          GO TO 8800-UNAUTHORIZED-ACCESS
00270      END-IF.
00271
00272 *    IF PI-CALLING-PROGRAM NOT = THIS-PGM AND PGM-EL1276
00274 *        PERFORM 4900-SET-NOTES-FLAG THRU 4900-EXIT
00275 *        IF CLAIM-SESSION
00276 *            SET PI-CLAIM-NOTE TO TRUE
00277 *        ELSE
00278 *            SET PI-CERT-NOTE TO TRUE
00279 *        END-IF
00280 *    END-IF.
00281
00282 *    IF PI-CALLING-PROGRAM = PGM-EL1276
00283 *        MOVE THIS-PGM           TO PI-CALLING-PROGRAM
00284 *        MOVE 'N'                TO PI-PF5-PRESSED
00285 *        MOVE 'N'                TO PI-PF6-PRESSED
00286 *    END-IF.
00287
00288      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00289         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00290            MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00291            MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00292            MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00293            MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00294            MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00295            MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00296            MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00297            MOVE THIS-PGM TO PI-CALLING-PROGRAM
00298         ELSE
00299            MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00300            MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00301            MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00302            MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00303            MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00304            MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00305            MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00306            MOVE SPACES               TO PI-SAVED-PROGRAM-6
00307         END-IF
00308      END-IF.
00309
00310  1000-START.
00137      MOVE 'N' TO WS-CURSOR-SET
00311
00312      MOVE LOW-VALUES TO EL1284AI
                              EL1284R
00313      MOVE SPACES                 TO  ELMEMO-KEY
00314                                      ELCERT-KEY.
00315
00316      MOVE PI-COMPANY-CD          TO  ELMEMO-COMPANY-CD
00317                                      ELCERT-COMPANY-CD.
00318      MOVE PI-CARRIER             TO  ELMEMO-CARRIER
00319                                      ELCERT-CARRIER.
           MOVE PI-CLAIM-NO            TO  ELMEMO-CLAIM-NO
00328      MOVE PI-CERT-PRIME          TO  ELMEMO-CERT-PRIME
00329                                      ELCERT-CERT-PRIME.
00330      MOVE PI-CERT-SFX            TO  ELMEMO-CERT-SFX
00331                                      ELCERT-CERT-SFX.
00332      MOVE ZEROS                  TO  ELMEMO-RECORD-TYPE
00333      MOVE ZEROS                  TO  ELMEMO-SEQ.
00334      MOVE ELMEMO-PARTIAL-KEY     TO  SV-PRIOR-KEY.
00335
00336      IF EIBTRNID NOT = TRANS-ID
              INITIALIZE PI-EL1284-AREA
00337         MOVE '0'                 TO  PI-UPDATE-SW
00338         IF PI-PROCESSOR-ID NOT = 'LGXX'
00339             
      * EXEC CICS READQ TS
00340 *                    QUEUE (PI-SECURITY-TEMP-STORE-ID)
00341 *                    INTO (SECURITY-CONTROL)
00342 *                    LENGTH (SC-COMM-LENGTH)
00343 *                    ITEM  (SC-ITEM)
00344 *           END-EXEC
      *    MOVE '*$II   L              ''   #00002374' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00345           MOVE SC-CREDIT-DISPLAY (32) TO PI-DISPLAY-CAP
00346           MOVE SC-CREDIT-UPDATE (32)  TO PI-MODIFY-CAP
00347         END-IF
00348      END-IF.
00349
00350      IF NOT DISPLAY-CAP
00351          MOVE 'READ'             TO  SM-READ
00352          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00353          MOVE ER-0070            TO  EMI-ERROR
00354          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00355          GO TO 8100-SEND-INITIAL-MAP
00356      END-IF.
00357
00358      
      * EXEC CICS HANDLE AID
00359 *         CLEAR(9400-CLEAR)
00360 *    END-EXEC.
      *    MOVE '"&=                  V! " #00002393' TO DFHEIV0
           MOVE X'22263D202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020562120' TO DFHEIV0(13:12)
           MOVE X'2220233030303032333933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00361
00362      
      * EXEC CICS HANDLE CONDITION
00363 *         ERROR(9990-ABEND)
00364 *         PGMIDERR(9600-PGMID-ERROR)
00365 *    END-EXEC.
      *    MOVE '"$.L                  ! # #00002397' TO DFHEIV0
           MOVE X'22242E4C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303032333937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00366
00367      IF EIBTRNID NOT = TRANS-ID
00368          GO TO 7000-BUILD-TABLE
00369      END-IF.
00370
00371      IF PI-CHANGE-IN-NOTE-TYPE
00372          GO TO 7000-BUILD-TABLE
00373      END-IF.
00374
00375      EJECT
00376  2000-RECEIVE.
00377      IF EIBAID = DFHPA1 OR
00378         EIBAID = DFHPA2 OR
00379         EIBAID = DFHPA3
00380            MOVE ER-0008 TO EMI-ERROR
00381            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00382            GO TO 8200-SEND-DATAONLY
00383      END-IF.
00384
00385      
      * EXEC CICS RECEIVE
00386 *         MAP(MAP-NAME)
00387 *         MAPSET(MAPSET-NAME)
00388 *         INTO(EL1284AI)
00389 *    END-EXEC.
           MOVE LENGTH OF
            EL1284AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00002420' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL1284AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE GROUP2 TO EL1284R.
00390
00391      IF PFENTERL = ZEROS
00392         GO TO 2001-CHECK-PFKEYS
00393      END-IF.
00394
00395      IF EIBAID NOT = DFHENTER
00396         MOVE ER-0004             TO EMI-ERROR
00397         GO TO 2002-INPUT-ERROR
00398      END-IF.
00399
00400      IF PFENTERI NUMERIC AND
00401         (PFENTERI > 00 AND  < 25)
00402         MOVE PF-VALUES (PFENTERI) TO EIBAID
00403      ELSE
00404         MOVE ER-0029 TO EMI-ERROR
00405         GO TO 2002-INPUT-ERROR
00406      END-IF.
00407
00408  2001-CHECK-PFKEYS.
00409      IF EIBAID = DFHPF23
00410         GO TO 9000-RETURN-CICS
00411      END-IF.
00412
00413      IF EIBAID = DFHPF24
00414         GO TO 9200-RETURN-MAIN-MENU
00415      END-IF.
00416
00417      IF EIBAID = DFHPF12
00418         GO TO 9500-PF12
00419      END-IF.
00420
00456      IF FUNCTL NOT = ZEROS AND EIBAID NOT = DFHENTER
00457         IF FUNCTI = 'A' OR = SPACES
00458            NEXT SENTENCE
00459         ELSE
00460            MOVE ER-0050          TO EMI-ERROR
00461            MOVE -1 TO FUNCTL
00462            MOVE AL-UABON TO FUNCTA PFENTERA
00463            GO TO 2002-INPUT-ERROR
00464         END-IF
00465      END-IF.
00466
00467      IF EIBAID = DFHPF1
00468         MOVE NUM-LINES-PER-SCREEN TO ROLL-COUNTER
00469         GO TO 7400-PAGE-ROUTINE
00470      END-IF.
00471
00472      IF EIBAID = DFHPF2
00473         SUBTRACT NUM-LINES-PER-SCREEN FROM ROLL-COUNTER
00474         GO TO 7400-PAGE-ROUTINE
00475      END-IF.
00476
00477      IF EIBAID = DFHPF3
00478         MOVE 5                   TO ROLL-COUNTER
00479         GO TO 7400-PAGE-ROUTINE
00480      END-IF.
00481
00482      IF EIBAID = DFHPF4
00483         MOVE -5                  TO ROLL-COUNTER
00484         GO TO 7400-PAGE-ROUTINE
00485      END-IF.
00486
00487      IF EIBAID = DFHENTER
00488         GO TO 2003-EDIT-DATA
00489      END-IF.
00490
00491      MOVE ER-0029                TO EMI-ERROR.
00492
00493  2002-INPUT-ERROR.
00494      MOVE -1                     TO PFENTERL
00495      MOVE AL-UNBON               TO PFENTERA
00496      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00497      GO TO 8200-SEND-DATAONLY.
00498
00499  2003-EDIT-DATA.
00500
00501      IF FUNCTI = 'L'
00502          NEXT SENTENCE
00503      ELSE
00504          IF NOT MODIFY-CAP
00505              MOVE 'UPDATE'       TO  SM-READ
00506              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00507              MOVE ER-0070        TO  EMI-ERROR
00508              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00509              GO TO 8100-SEND-INITIAL-MAP
00510          END-IF
00511      END-IF.
040416     IF FUNCTI NOT = 'Q'
              PERFORM 2010-EDIT-CHECK
040416     END-IF.
00512
00513      IF FUNCTL = ZEROS OR FUNCTI = SPACES
00514         GO TO 4000-CHANGE-ROUTINE
00515      END-IF.
00516
00517      IF (FUNCTI = 'S' OR = 'D' OR = 'Q' OR
00518                 = 'I' OR = 'A' OR = 'L')
00519          NEXT SENTENCE
00520      ELSE
00521          MOVE ER-0023            TO EMI-ERROR
00522          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00523          MOVE AL-UABON           TO FUNCTA
00524          MOVE -1                 TO FUNCTL
00525          GO TO 8200-SEND-DATAONLY
00526      END-IF.
00527
00528      IF FUNCTI = 'D'  OR = 'I' OR = 'L'
00529         PERFORM 2500-LINE-CHECK THRU 2599-EXIT
00530      ELSE
00531         IF LINE1L NOT = ZEROS OR
00532            LINE2L NOT = ZEROS
00533            MOVE ER-0030          TO EMI-ERROR
00534            MOVE -1               TO LINE1L
00535            MOVE AL-UNBON         TO LINE1A LINE2A
00536            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00537            GO TO 8200-SEND-DATAONLY
00538         END-IF
00539      END-IF.
00540
00541      IF FUNCTI = 'A'
00542         GO TO 5000-ADD-NEW-LINES
00543      END-IF.
00544      IF FUNCTI = 'Q'
00545         GO TO 9410-RETURN
00546      END-IF.
00547      IF FUNCTI = 'S'
00548         GO TO 4500-SAVE-DATA
00549      END-IF.
00550      IF PI-TOTAL-LINES = 0
00551         MOVE ER-0048             TO EMI-ERROR
00552         MOVE -1                  TO FUNCTL
00553         MOVE AL-UNBON            TO FUNCTA
00554         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00555         GO TO 8200-SEND-DATAONLY
00556      END-IF.
00557      IF FUNCTI = 'L'
00558         GO TO 5500-LOOKUP
00559      END-IF.
00560      IF FUNCTI = 'D'
00561         GO TO 3000-DELETE-LINES
00562      END-IF.
00563
00564      GO TO 3500-INSERT-LINES.
00565      EJECT
00566  2010-EDIT-CHECK.
      *
           IF LNGHLAPL > 0
              MOVE LNGHLAPI TO  PI-LONG-HLTH-APP
              MOVE '1'      TO PI-UPDATE-SW
           ELSE
              IF LNGHLAPA = WS-ERASE-EOF
                 MOVE SPACES TO PI-LONG-HLTH-APP
                 MOVE '1'    TO PI-UPDATE-SW
              END-IF
              MOVE PI-LONG-HLTH-APP TO LNGHLAPI
           END-IF.
      *
           IF LNGHLAPI > SPACE
              IF LNGHLAPI = 'N' OR 'Y'
                 CONTINUE
              ELSE
                 MOVE -1                     TO LNGHLAPL
                 MOVE AL-UABON               TO LNGHLAPA
                 MOVE ER-0046                TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF.
      *
           IF REWRITEL > 0
              MOVE REWRITEI TO PI-REWRITE
              MOVE '1'      TO PI-UPDATE-SW
           ELSE
              IF REWRITEA = WS-ERASE-EOF
                 MOVE SPACES TO PI-REWRITE
                 MOVE '1'    TO PI-UPDATE-SW
              END-IF
              MOVE PI-REWRITE TO REWRITEI
           END-IF
      *
           IF REWRITEI > SPACE
040416        IF REWRITEI = 'N' OR 'Y' OR 'D'
                 CONTINUE
              ELSE
                 MOVE -1                     TO REWRITEL
                 MOVE AL-UABON               TO REWRITEA
040416           MOVE ER-3846                TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF.
      *
           IF CHKCOVGL > 0
              MOVE CHKCOVGI TO PI-CHKCOVG
              MOVE '1'      TO PI-UPDATE-SW
           ELSE
              IF CHKCOVGA = WS-ERASE-EOF
                 MOVE SPACES TO PI-CHKCOVG
                 MOVE '1'    TO PI-UPDATE-SW
              END-IF
              MOVE PI-CHKCOVG TO CHKCOVGI
           END-IF
      *
           IF CHKCOVGI > SPACE
              IF CHKCOVGI = 'N' OR 'Y'
                 CONTINUE
              ELSE
                 MOVE -1                     TO CHKCOVGL
                 MOVE AL-UABON               TO CHKCOVGA
                 MOVE ER-0046                TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF.
      *
           IF MRRELDTL > 0
              MOVE MRRELDTI TO PI-MR-RELEASE-DATE
              MOVE '1'      TO PI-UPDATE-SW
           ELSE
              IF MRRELDTA = WS-ERASE-EOF
                 MOVE SPACES TO PI-MR-RELEASE-DATE
                 MOVE '1'    TO PI-UPDATE-SW
              END-IF
              MOVE PI-MR-RELEASE-DATE TO MRRELDTI
           END-IF.
      *
00566  2500-LINE-CHECK.
00567      IF LINE1L = ZEROS AND
00568         LINE2L = ZEROS
00569         MOVE ER-0069             TO EMI-ERROR
00570         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00571         MOVE -1                  TO LINE1L
00572         GO TO 8200-SEND-DATAONLY
00573      END-IF.
00574
00575      IF LINE1L NOT = ZEROS
00576         IF LINE1I = ZERO AND FUNCTI EQUAL 'L'
00577             MOVE 1               TO LINE1I
00578         END-IF
00579         IF LINE1I = ZERO AND FUNCTI EQUAL 'D'
00580            MOVE ER-0049          TO EMI-ERROR
00581            MOVE AL-UNBON         TO LINE1A
00582            MOVE -1               TO LINE1L
00583            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00584            GO TO 8200-SEND-DATAONLY
00585         END-IF
00586         IF LINE1I NOT NUMERIC OR
00587            LINE1I > PI-TOTAL-LINES
00588            MOVE ER-0031          TO EMI-ERROR
00589            MOVE AL-UNBON         TO LINE1A
00590            MOVE -1               TO LINE1L
00591            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00592            GO TO 8200-SEND-DATAONLY
00593         ELSE
00594            IF LINE2L = ZEROS
00595               MOVE 1             TO LINE2I
00596               IF FUNCTI = 'I'
00597                   GO TO 2510-MAX-CHECK
00598               ELSE
00599                   NEXT SENTENCE
00600               END-IF
00601            ELSE
00602               IF FUNCTI = 'I'
00603                  GO TO 2510-MAX-CHECK
00604               ELSE
00605                  IF LINE2I NOT NUMERIC
00606                     MOVE AL-UNBON TO LINE2A
00607                     MOVE ER-0032  TO EMI-ERROR
00608                     MOVE -1       TO LINE2L
00609                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00610                     GO TO 8200-SEND-DATAONLY
00611                  ELSE
00612                     NEXT SENTENCE
00613                  END-IF
00614               END-IF
00615            END-IF
00616         END-IF
00617      ELSE
00618         IF LINE2L = ZEROS
00619            NEXT SENTENCE
00620         ELSE
00621            MOVE -1               TO LINE2L
00622            MOVE ER-0041          TO EMI-ERROR
00623            MOVE AL-UNBON         TO LINE2A
00624            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00625            GO TO 8200-SEND-DATAONLY
00626         END-IF
00627      END-IF.
00628      GO TO 2599-EXIT.
00629  2510-MAX-CHECK.
00630      IF LINE2I NOT NUMERIC
00631         MOVE -1                  TO LINE2L
00632         MOVE ER-0032             TO EMI-ERROR
00633         MOVE AL-UNBON            TO LINE2A
00634         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00635         GO TO 8200-SEND-DATAONLY
00636      ELSE
00637         COMPUTE ROLL-COUNTER = LINE2I + PI-TOTAL-LINES
00638         IF ROLL-COUNTER GREATER THAN MAX-LINES
00639            MOVE -1               TO LINE2L
00640            MOVE ER-0044          TO EMI-ERROR
00641            MOVE AL-UNBON         TO LINE2A
00642            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00643            GO TO 8200-SEND-DATAONLY
00644         END-IF
00645      END-IF.
00646  2599-EXIT.
00647       EXIT.
00648      EJECT
00649  3000-DELETE-LINES.
00650      IF LINE2L = ZEROS AND LINE2I = 1
00651         MOVE LINE1I              TO LINE2I
00652      END-IF.
00653
00654      IF LINE2I > PI-TOTAL-LINES OR < LINE1I
00655         MOVE ER-0049             TO EMI-ERROR
00656         MOVE AL-UNBON            TO LINE2A
00657         MOVE -1                  TO LINE2L
00658         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00659         GO TO 8200-SEND-DATAONLY
00660      END-IF.
00661
00662      PERFORM 7450-SET-INDX THRU 7450-EXIT.
00663      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
00664              VARYING SC-INDX FROM 1 BY 1 UNTIL
00665              SC-INDX > NUM-LINES-PER-SCREEN
00666      SET TB-INDX TO LINE1I.
00667
00668      IF NOT EMI-NO-ERRORS
00669         GO TO 8200-SEND-DATAONLY
00670      END-IF.
00671
00672      SET TB-INDX TO LINE1I
00673      COMPUTE ROLL-COUNTER = LINE2I - LINE1I + 1.
00674
00675      IF LINE2I NOT = PI-TOTAL-LINES
00676         SET TB-INDX1 TO LINE2I
00677         SET TB-INDX1 UP BY 1
00678         PERFORM 3100-DELETE-TABLE-ENTRIES
00679                 UNTIL TB-INDX1 > PI-TOTAL-LINES
00680      END-IF.
00681
00682      PERFORM 3150-BLANK-TABLE-ENTRIES
00683              ROLL-COUNTER TIMES.
00684      SUBTRACT ROLL-COUNTER FROM PI-TOTAL-LINES.
00685
00686      IF PI-CURRENT-LINE > PI-TOTAL-LINES
00687         MOVE PI-TOTAL-LINES      TO PI-CURRENT-LINE
00688         SUBTRACT 1 FROM PI-CURRENT-LINE
00689      END-IF.
00690
00691      SET TB-INDX  TO PI-CURRENT-LINE
00692      MOVE LOW-VALUES             TO EL1284AI
                                          EL1284R
00693
           PERFORM 7800-MOVE-SAVE-TO-MAP.
00694      IF PI-CURRENT-LINE > ZERO
00695          PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT
00696              VARYING SC-INDX FROM 1 BY 1 UNTIL
00697              SC-INDX > NUM-LINES-PER-SCREEN
00698          PERFORM 7200-PUT-TEMP-STOR  THRU 7249-EXIT
00699      END-IF.
00700
00701      MOVE '1'                    TO PI-UPDATE-SW.
00702      IF PI-TOTAL-LINES = ZEROS
00703         MOVE ZEROS               TO PI-CURRENT-LINE
00704      END-IF.
040416     MOVE -1 TO FUNCTL
040416     SET CURSOR-SET TO TRUE
00706      GO TO 8100-SEND-INITIAL-MAP.
00707      EJECT
00708
00709  3100-DELETE-TABLE-ENTRIES.
00710      MOVE REC-ENT (TB-INDX1)     TO REC-ENT (TB-INDX)
00711      SET TB-INDX TB-INDX1 UP BY 1.
00712
00713  3150-BLANK-TABLE-ENTRIES.
00714      MOVE SPACES               TO REC-ENT (TB-INDX).
00715      MOVE SAVE-BIN-DATE        TO REC-LAST-MAINT-DT (TB-INDX).
00716      MOVE EIBTIME              TO REC-LAST-MAINT-HHMMSS (TB-INDX).
00717      MOVE PI-PROCESSOR-ID      TO REC-LAST-MAINT-BY (TB-INDX).
00718      SET TB-INDX UP BY 1.
00719      EJECT
00720  3500-INSERT-LINES.
00721
00722      PERFORM 7450-SET-INDX THRU 7450-EXIT.
00723      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
00724              VARYING SC-INDX FROM 1 BY 1 UNTIL
00725              SC-INDX > NUM-LINES-PER-SCREEN.
00726
00727      IF NOT EMI-NO-ERRORS
00728         GO TO 8200-SEND-DATAONLY
00729      END-IF.
00730
00731      SET TB-INDX TO PI-TOTAL-LINES.
00732      ADD LINE2I TO PI-TOTAL-LINES.
00733      SET TB-INDX1 TO PI-TOTAL-LINES.
00734      PERFORM 3600-INSERT-TABLE-ENTRIES
00735              UNTIL TB-INDX = LINE1I.
00736      SET TB-INDX UP BY 1.
00737
00738      IF LINE1I EQUAL ZERO
00739          SET PI-CURRENT-LINE TO 1
00740      ELSE
00741          SET PI-CURRENT-LINE TO LINE1I
00742      END-IF.
00743
00744      COMPUTE ROLL-COUNTER = PI-CURRENT-LINE +
00745                             NUM-LINES-PER-SCREEN.
00746      IF TB-INDX NOT LESS THAN ROLL-COUNTER OR
00747                     LESS THAN PI-CURRENT-LINE
00748         SET SC-INDX TO 1
00749         SET SC-INDX DOWN BY 1
00750      ELSE
00751         SET ROLL-COUNTER TO TB-INDX
00752         COMPUTE ROLL-COUNTER = ROLL-COUNTER - PI-CURRENT-LINE
00753                   + 1
00754         SET SC-INDX TO ROLL-COUNTER
00755      END-IF.
00756
00757      PERFORM 3150-BLANK-TABLE-ENTRIES LINE2I TIMES.
00758      SET TB-INDX TO PI-CURRENT-LINE.
00759      MOVE LOW-VALUES             TO EL1284AI
                                          EL1284R
00760
00761      IF SC-INDX NOT = ZERO
00762         MOVE -1 TO SC-TEXTL (SC-INDX)
              SET CURSOR-SET TO TRUE
00763      END-IF.
00764
           PERFORM 7800-MOVE-SAVE-TO-MAP.
00765      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT
00766             VARYING SC-INDX FROM 1 BY 1 UNTIL
00767             SC-INDX > NUM-LINES-PER-SCREEN.
00768      PERFORM 7200-PUT-TEMP-STOR  THRU 7249-EXIT.
00769      MOVE '1'                    TO PI-UPDATE-SW.
040416     MOVE -1 TO FUNCTL
040416     SET CURSOR-SET TO TRUE
00770      GO TO 8100-SEND-INITIAL-MAP.
00771
00772  3600-INSERT-TABLE-ENTRIES.
00773      MOVE REC-ENT (TB-INDX)      TO REC-ENT (TB-INDX1).
00774      SET TB-INDX TB-INDX1 DOWN BY 1.
00775      EJECT
00776
00777  4000-CHANGE-ROUTINE.
00778      PERFORM 7450-SET-INDX THRU 7450-EXIT.
00779      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
00780              VARYING SC-INDX FROM 1 BY 1 UNTIL
00781              SC-INDX > NUM-LINES-PER-SCREEN.
00782
00783      IF NOT EMI-NO-ERRORS
00784         GO TO 8200-SEND-DATAONLY
00785      END-IF.
00786
00787      PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.
00788      MOVE SPACES                 TO ERRMSGBO.
00789      GO TO 8200-SEND-DATAONLY.
00790
00791      EJECT
00792  4500-SAVE-DATA.
00793      PERFORM 7450-SET-INDX THRU 7450-EXIT.
00794      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
00795              VARYING SC-INDX FROM 1 BY 1 UNTIL
00796              SC-INDX > NUM-LINES-PER-SCREEN.
00797      IF NOT EMI-NO-ERRORS
00798         GO TO 8200-SEND-DATAONLY
00799      END-IF.
00800
00801      
      * EXEC CICS HANDLE CONDITION
00802 *         NOTFND(4610-ENDBR)
00803 *         NOTOPEN(6000-NOT-OPEN)
00804 *         ENDFILE(4610-ENDBR)
00805 *    END-EXEC.
      *    MOVE '"$IJ''                 ! $ #00002895' TO DFHEIV0
           MOVE X'2224494A2720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303032383935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00806
00807  4610-LOOP.
00808      
      * EXEC CICS READ
00809 *        DATASET (ELMEMO-FILE-ID)
00810 *        RIDFLD  (ELMEMO-KEY)
00811 *        SET     (ADDRESS OF CLAIM-MEMO-FILE)
00812 *        GTEQ
00813 *    END-EXEC.
      *    MOVE '&"S        G          (   #00002902' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMEMO-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMEMO-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MEMO-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00814
00815      MOVE MM-CONTROL-PRIMARY     TO ELMEMO-KEY.
00816
00817      IF ELMEMO-PARTIAL-KEY NOT = SV-PRIOR-KEY
00818          MOVE SV-PRIOR-KEY       TO ELMEMO-PARTIAL-KEY
00819          GO TO 4610-ENDBR
00820      END-IF.
00821
00822      
      * EXEC CICS DELETE
00823 *        DATASET (ELMEMO-FILE-ID)
00824 *        RIDFLD  (ELMEMO-KEY)
00825 *    END-EXEC.
      *    MOVE '&(  R                 &   #00002916' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMEMO-FILE-ID, 
                 ELMEMO-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00826
00827      GO TO 4610-LOOP.
00828  4610-ENDBR.
00829      
      * EXEC CICS GETMAIN
00830 *         LENGTH(ELMEMO-LENGTH)
00831 *         SET(ADDRESS OF CLAIM-MEMO-FILE)
00832 *         INITIMG(GETMAIN-SPACE)
00833 *    END-EXEC.
      *    MOVE ',"IL                  $   #00002923' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELMEMO-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF CLAIM-MEMO-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           PERFORM 4650-WRITE-HEADER THRU 4650-EXIT.
00834
00835      MOVE 1                      TO  ELMEMO-SEQ.
00836      MOVE ZERO                   TO  WS-BLANK-LINES.
00837
00838      PERFORM 4700-WRITE-FILE THRU 4799-EXIT
00839              VARYING TB-INDX FROM 1 BY 1 UNTIL
00840              TB-INDX > PI-TOTAL-LINES.
00841
00842      SUBTRACT WS-BLANK-LINES FROM PI-TOTAL-LINES.
00843
00846      GO TO 9410-RETURN.
00848  4650-WRITE-HEADER.
00853
00854      MOVE SPACES                 TO  CLAIM-MEMO-FILE.
00855      MOVE ZERO                   TO  ELMEMO-SEQ.
00856      MOVE ELMEMO-KEY             TO  MM-CONTROL-PRIMARY.
00857      MOVE  'MM'                  TO  MM-RECORD-ID.
00858      MOVE REC-TEXT (TB-INDX)     TO  MM-CLAIM-MEMO
00036      MOVE LNGHLAPI               TO MM-LONG-HEALTH-APP
00037      MOVE REWRITEI               TO MM-REWRITE-IND
00037      MOVE CHKCOVGI               TO MM-CHECKED-OTHER-COVG
01335      IF MRRELDTI > SPACES
01335         MOVE MRRELDTI               TO DEEDIT-DATE-INPUT
01336         
      * EXEC CICS BIF DEEDIT
01337 *           FIELD    (DEEDIT-DATE-INPUT)
01338 *           LENGTH   (DATE-LENGTH)
01339 *       END-EXEC
      *    MOVE '@"L                   #   #00002952' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-DATE-INPUT, 
                 DATE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01340         MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY
01341         MOVE '4'            TO DC-OPTION-CODE
01342         PERFORM PERFORM 9700-LINK-DATE-CONVERT
01343         IF NOT DATE-CONVERSION-ERROR
01344             MOVE DC-BIN-DATE-1      TO MM-MR-RELEASED-FROM-DATE
01345 *           MOVE 'X'        TO UPDATE-SWITCH MSTR-SWITCH
01346 *           MOVE AL-UANON           TO INCA
01347 *           MOVE DC-GREG-DATE-1-EDIT TO INCO
01348         ELSE
                  MOVE -1         TO MRRELDTL
01349             MOVE ER-0314    TO EMI-ERROR
01350             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00635             GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF.
01351 *        MOVE AL-UABON   TO INCA
01352 *        MOVE -1         TO INCL
01353 *        MOVE 'X'        TO ERROR-SWITCH.
00866      
      * EXEC CICS WRITE
00867 *         DATASET(ELMEMO-FILE-ID)
00868 *         FROM(CLAIM-MEMO-FILE)
00869 *         RIDFLD(ELMEMO-KEY)
00870 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MEMO-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00002974' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMEMO-FILE-ID, 
                 CLAIM-MEMO-FILE, 
                 DFHEIV11, 
                 ELMEMO-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00871  4650-EXIT.
00872       EXIT.
00847
00848  4700-WRITE-FILE.
040416*    IF REC-TEXT (TB-INDX)  EQUAL SPACES
040416*        ADD +1                  TO  WS-BLANK-LINES
040416*        GO TO 4799-EXIT
040416*    END-IF.
00853
00854      MOVE SPACES                 TO  CLAIM-MEMO-FILE.
00855      ADD 1                       TO  ELMEMO-SEQ.
00856      MOVE ELMEMO-KEY             TO  MM-CONTROL-PRIMARY.
00857      MOVE  'MM'                  TO  MM-RECORD-ID.
00858      MOVE REC-TEXT (TB-INDX)     TO  MM-CLAIM-MEMO
00859      MOVE REC-LAST-MAINT-BY (TB-INDX)
00860                                  TO  MM-LAST-MAINT-BY
00861      MOVE REC-LAST-MAINT-HHMMSS (TB-INDX)
00862                                  TO  MM-LAST-MAINT-HHMMSS.
00863      MOVE REC-LAST-MAINT-DT (TB-INDX)
00864                                  TO  MM-LAST-MAINT-DT.
00865
00866      
      * EXEC CICS WRITE
00867 *         DATASET(ELMEMO-FILE-ID)
00868 *         FROM(CLAIM-MEMO-FILE)
00869 *         RIDFLD(ELMEMO-KEY)
00870 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MEMO-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003000' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMEMO-FILE-ID, 
                 CLAIM-MEMO-FILE, 
                 DFHEIV11, 
                 ELMEMO-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00871  4799-EXIT.
00872       EXIT.
00873
00874      EJECT
00971
00972  4900-SET-NOTES-FLAG.
00973
00974      
      * EXEC CICS HANDLE CONDITION
00975 *        NOTFND   (4900-EXIT)
00976 *    END-EXEC.
      *    MOVE '"$I                   ! % #00003012' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303033303132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00977
00978      MOVE SPACES                 TO  ELCERT-KEY.
00979      MOVE PI-COMPANY-CD          TO  ELCERT-COMPANY-CD.
00980      MOVE PI-CARRIER             TO  ELCERT-CARRIER.
00981      MOVE PI-GROUPING            TO  ELCERT-GROUPING.
00982      MOVE PI-STATE               TO  ELCERT-STATE.
00983      MOVE PI-ACCOUNT             TO  ELCERT-ACCOUNT.
00984      MOVE PI-CERT-EFF-DT         TO  ELCERT-EFF-DT.
00985      MOVE PI-CERT-PRIME          TO  ELCERT-CERT-PRIME.
00986      MOVE PI-CERT-SFX            TO  ELCERT-CERT-SFX.
00987
00988      
      * EXEC CICS READ
00989 *    EQUAL
00990 *    DATASET   (ELCERT-FILE-ID)
00991 *    SET       (ADDRESS OF CERTIFICATE-MASTER)
00992 *    RIDFLD    (ELCERT-KEY)
00993 *    END-EXEC.
      *    MOVE '&"S        E          (   #00003026' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00994
00995 *    MOVE 'N'                    TO PI-BILLING-NOTES-EXIST
00996 *                                   PI-CERT-NOTES-EXIST
00997 *                                   PI-CLAIM-NOTES-EXIST.
00998 *    IF CM-NOTE-SW EQUAL '2' OR '3' OR '6' OR '7'
00999 *         MOVE 'Y'               TO PI-BILLING-NOTES-EXIST
01000 *    END-IF.
01001 *    IF CM-NOTE-SW EQUAL '1' OR '3' OR '5' OR '7'
01002 *         MOVE 'Y'               TO PI-CERT-NOTES-EXIST
01003 *    END-IF.
01004 *    IF CM-NOTE-SW EQUAL '4' OR '5' OR '6' OR '7'
01005 *         MOVE 'Y'               TO PI-CLAIM-NOTES-EXIST
01006 *    END-IF.
01007
01008
01009  4900-EXIT.
01010       EXIT.
01011
01012      EJECT
01013  5000-ADD-NEW-LINES.
01014      PERFORM 7450-SET-INDX THRU 7450-EXIT.
01015      MOVE 'N'                    TO WS-SCREEN-LINE.
01016      MOVE 1                      TO WS-SUB.
01017      MOVE ZERO                   TO WS-SUB1.
01018      PERFORM VARYING SC-INDX FROM NUM-LINES-PER-SCREEN BY -1
01019              UNTIL SCREEN-LINE-FOUND OR SC-INDX < 1
01020          IF SC-TEXT (SC-INDX) >  SPACES
01021              SET WS-SUB          TO SC-INDX
01022              MOVE 'Y'            TO WS-SCREEN-LINE
01023          END-IF
01024      END-PERFORM.
01025      IF PI-TOTAL-LINES = 0
01026          MOVE 1                  TO WS-SUB1
01027      ELSE
01028          MOVE 2                  TO WS-SUB1
01029      END-IF.
01030      PERFORM VARYING SC-INDX FROM WS-SUB1 BY 1
01031              UNTIL SC-INDX GREATER THAN WS-SUB
01032          IF SC-TEXTL (SC-INDX) EQUAL ZEROS
01033              MOVE 1      TO SC-TEXTL (SC-INDX)
01034              MOVE SPACES TO SC-TEXT (SC-INDX)
                   MOVE LOW-VALUES TO SC-MTBY (SC-INDX)
                                      SC-MTDT (SC-INDX)
01035          END-IF
01036      END-PERFORM.
01037      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
01038              VARYING SC-INDX FROM 1 BY 1 UNTIL
01039              SC-INDX > NUM-LINES-PER-SCREEN.
01040
01041      IF NOT EMI-NO-ERRORS
01042         GO TO 8200-SEND-DATAONLY
01043      END-IF.
01044
01045      MOVE PI-TOTAL-LINES         TO  PI-CURRENT-LINE.
01046      PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.
01047      MOVE LOW-VALUES             TO  EL1284AI
                                           EL1284R
           PERFORM 7800-MOVE-SAVE-TO-MAP.
01048      SET TB-INDX TO PI-CURRENT-LINE.
01049      MOVE 'A'                    TO FUNCTI.
01050      MOVE -1                     TO  SC-TEXTL (2).
           SET CURSOR-SET TO TRUE
01051      MOVE AL-UANON               TO  FUNCTA.
01052      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT
01053              VARYING SC-INDX FROM 1 BY 1 UNTIL
01054              SC-INDX > NUM-LINES-PER-SCREEN.
01055      MOVE '1'                    TO PI-UPDATE-SW.
01056      GO TO 8100-SEND-INITIAL-MAP.
01057      EJECT
01058  5500-LOOKUP.
01059      PERFORM 7500-READ-TS THRU 7599-EXIT.
01060      SET TB-INDX TO PI-CURRENT-LINE.
01061      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
01062              VARYING SC-INDX FROM 1 BY 1 UNTIL
01063              SC-INDX > NUM-LINES-PER-SCREEN.
01064
01065      IF NOT EMI-NO-ERRORS
01066         GO TO 8200-SEND-DATAONLY
01067      END-IF.
01068
01069      MOVE LINE1I                 TO  PI-CURRENT-LINE.
01070      SET TB-INDX                 TO PI-CURRENT-LINE.
01071      MOVE LOW-VALUES             TO  EL1284AI
                                           EL1284R
           PERFORM 7800-MOVE-SAVE-TO-MAP.
01072      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT
01073              VARYING SC-INDX FROM 1 BY 1
01074              UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
01075      PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.
040416     MOVE -1 TO FUNCTL
040416     SET CURSOR-SET TO TRUE
01076      GO TO 8100-SEND-INITIAL-MAP.
01077      EJECT
01078  6000-NOT-OPEN.
01079      MOVE ER-2954                TO  EMI-ERROR.
01080      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01081
01082      IF EIBAID = DFHCLEAR
01083          GO TO 9410-RETURN
01084      ELSE
01085          GO TO 8100-SEND-INITIAL-MAP
01086      END-IF.
01087      EJECT
01088  7000-BUILD-TABLE.
01089
01090      SET TB-INDX TO 1.
01091      MOVE ZEROS                  TO  PI-TOTAL-LINES
01092                                      PI-CURRENT-LINE
01093                                      PI-TEMP-STOR-ITEMS
01094                                      PI-UPDATE-SW.
01095      MOVE LOW-VALUES             TO  EL1284AI
                                           EL1284R
01099
01100 ****IF TEMP STORAGE EXISTS, DELETE IT.
01101      IF PI-CHANGE-IN-NOTE-TYPE
01102          MOVE 'N' TO PI-SET-NOTE-CHANGE
01103      ELSE
01104          PERFORM 7500-READ-TS THRU 7599-EXIT
01105      END-IF.
01106
01107      IF PI-TEMP-STOR-ITEMS NOT = ZERO
01108         PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT
01109      END-IF.
01110
01111      
      * EXEC CICS HANDLE CONDITION
01112 *         NOTFND(7010-ENDBR)
01113 *         NOTOPEN(6000-NOT-OPEN)
01114 *         ENDFILE(7010-ENDBR)
01115 *    END-EXEC.
      *    MOVE '"$IJ''                 ! & #00003156' TO DFHEIV0
           MOVE X'2224494A2720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303033313536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01116
01117      
      * EXEC CICS STARTBR
01118 *         DATASET(ELMEMO-FILE-ID)
01119 *         RIDFLD(ELMEMO-KEY)
01120 *         KEYLENGTH(ELMEMO-START-LENGTH)
01121 *         GENERIC
01122 *         GTEQ
01123 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    G          &   #00003162' TO DFHEIV0
           MOVE X'262C2020204B472020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMEMO-FILE-ID, 
                 ELMEMO-KEY, 
                 ELMEMO-START-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01124
01125  7001-LOOP.
01126      
      * EXEC CICS READNEXT
01127 *         SET(ADDRESS OF CLAIM-MEMO-FILE)
01128 *         DATASET(ELMEMO-FILE-ID)
01129 *         RIDFLD(ELMEMO-KEY)
01130 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00003171' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMEMO-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMEMO-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MEMO-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01131
01132      IF MM-COMPANY-CD NOT = SV-COMPANY-CD
01133          GO TO 7010-ENDBR
01134      END-IF.
01135
01136      IF (MM-CARRIER = SV-CARRIER)
01141         AND (MM-CERT-NO = SV-CERT-NO)
01141         AND (MM-CLAIM-NO = SV-CLAIM-NO)
01142         AND (MM-RECORD-TYPE = SV-RECORD-TYPE)
              IF MM-PAYMENT-SEQ-NO = ZERO
                 MOVE MM-LONG-HEALTH-APP TO LNGHLAPO
                                            PI-LONG-HLTH-APP
                 MOVE MM-REWRITE-IND TO REWRITEO
                                        PI-REWRITE
                 MOVE MM-CHECKED-OTHER-COVG TO CHKCOVGO
                                               PI-CHKCOVG
                 MOVE MM-MR-RELEASED-FROM-DATE TO DC-BIN-DATE-1
                 MOVE ' '                    TO DC-OPTION-CODE
01433            PERFORM 9700-LINK-DATE-CONVERT
01434            IF DC-EDIT1-MONTH > ZERO
                    MOVE DC-GREG-DATE-1-EDIT    TO MRRELDTO
                                                   PI-MR-RELEASE-DATE
                 END-IF
              ELSE
01143            MOVE MM-CLAIM-MEMO TO REC-TEXT (TB-INDX)
01144            MOVE MM-LAST-MAINT-BY TO REC-LAST-MAINT-BY (TB-INDX)
01145            MOVE MM-LAST-MAINT-DT TO REC-LAST-MAINT-DT (TB-INDX)
01146            MOVE MM-LAST-MAINT-HHMMSS TO
01147                           REC-LAST-MAINT-HHMMSS (TB-INDX)
01155            SET TB-INDX UP BY 1
              END-IF
01156         GO TO 7001-LOOP
01157      END-IF.
01158  7010-ENDBR.
01159      IF TB-INDX = 1
01160          MOVE ER-0006            TO EMI-ERROR
01161          MOVE 'A'                TO FUNCTI
01162          MOVE -1                 TO SC-TEXTL (1)
01163          MOVE AL-UANON           TO FUNCTA
01164          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01165          MOVE ZEROS              TO PI-TOTAL-LINES
01166          GO TO 8100-SEND-INITIAL-MAP
01167      END-IF.
01168
01169      
      * EXEC CICS ENDBR
01170 *         DATASET(ELMEMO-FILE-ID)
01171 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003220' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMEMO-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01172
01173      SET TB-INDX DOWN BY 1.
01174      SET PI-TOTAL-LINES TO TB-INDX.
01175      MOVE 1                      TO PI-CURRENT-LINE.
01176
01177  7050-FORMAT-LINES.
01178      SET TB-INDX TO PI-CURRENT-LINE.
01179      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT
01180              VARYING SC-INDX FROM 1
01181              BY 1 UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
01182      PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.
01183      GO TO 8100-SEND-INITIAL-MAP.
01184      EJECT
01185  7100-FORMAT-SCREEN.
01186      IF TB-INDX > PI-TOTAL-LINES
01187         IF FUNCTI NOT = 'A'
01188            MOVE AL-PANON         TO SC-TEXTA (SC-INDX)
01189         END-IF
01190      END-IF.
01191
01192      IF TB-INDX > PI-TOTAL-LINES
01193          GO TO 7100-EXIT
01194      END-IF.
01195
01196      SET WS-LINE TO TB-INDX.
01201      MOVE WS-LINE TO SC-LIN (SC-INDX).
01202      MOVE REC-TEXT (TB-INDX)     TO SC-TEXT (SC-INDX).
           MOVE REC-LAST-MAINT-BY (TB-INDX) TO SC-MTBY (SC-INDX).
           MOVE REC-LAST-MAINT-DT (TB-INDX) TO DC-BIN-DATE-1.
           MOVE ' '                    TO DC-OPTION-CODE.
01433      PERFORM 9700-LINK-DATE-CONVERT.
01434      MOVE DC-GREG-DATE-1-MDY     TO SC-MTDT (SC-INDX).
01203      SET ROLL-COUNTER TO TB-INDX.
01204
01205      IF NOT MODIFY-CAP
01206          MOVE AL-PANOF           TO SC-TEXTA (SC-INDX)
01207          SET TB-INDX UP BY 1
01208          GO TO 7100-EXIT
01209      END-IF.
01210
01211      SET TB-INDX UP BY 1.
01212
01213  7100-EXIT.
01214       EXIT.
01215
01235  7200-PUT-TEMP-STOR.
01236      PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT.
01237      SET TS-INDX TO 1.
01238      MOVE 0                      TO PI-TEMP-STOR-ITEMS.
01239      PERFORM 7300-WRITE-TS THRU 7399-EXIT
01240              VARYING TS-GROUP-WORK FROM 0 BY TS-NUM-REC-IN-GROUP
01241              UNTIL TS-GROUP-WORK NOT LESS THAN PI-TOTAL-LINES.
01242  7249-EXIT.
01243       EXIT.
01244  7250-DELETE-TEMP-STOR.
01245      
      * EXEC CICS HANDLE CONDITION
01246 *         QIDERR(7299-EXIT)
01247 *    END-EXEC.
      *    MOVE '"$N                   ! '' #00003278' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303033323738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01248      
      * EXEC CICS DELETEQ TS
01249 *         QUEUE(QID)
01250 *    END-EXEC.
      *    MOVE '*&                    #   #00003281' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01251  7299-EXIT.
01252      EXIT.
01253      EJECT
01254  7300-WRITE-TS.
01255      MOVE TS-GROUP (TS-INDX)     TO TS-WORK-AREA.
01256      SET TS-INDX UP BY 1.
01257      ADD 1 TO PI-TEMP-STOR-ITEMS.
01258      
      * EXEC CICS WRITEQ TS
01259 *         FROM(TS-WORK-AREA)
01260 *         QUEUE(QID)
01261 *         LENGTH(TS-LENGTH)
01262 *         ITEM(PI-TEMP-STOR-ITEMS)
01263 *    END-EXEC.
      *    MOVE '*" I   L              ''   #00003291' TO DFHEIV0
           MOVE X'2A2220492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 TS-WORK-AREA, 
                 TS-LENGTH, 
                 PI-TEMP-STOR-ITEMS, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01264  7399-EXIT.
01265      EXIT.
01266      EJECT
01267  7400-PAGE-ROUTINE.
01268
01269      IF PFENTERL NOT = ZEROS
01270         MOVE -1                  TO PFENTERL
01271         ELSE
01272         MOVE -1                  TO FUNCTL
01273      END-IF.
01274
01275      IF PI-TOTAL-LINES = 0
01276         MOVE ER-0047             TO EMI-ERROR
01277         MOVE -1                  TO FUNCTL
01278         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01279         GO TO 8200-SEND-DATAONLY
01280      END-IF.
01281
01282      COMPUTE TEMP-CURR-LINE = PI-CURRENT-LINE + ROLL-COUNTER.
01283
01284      IF TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS
01285         MOVE ER-0067             TO EMI-ERROR
01286         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01287         MOVE 1 TO TEMP-CURR-LINE
01288      END-IF.
01289
01290      IF TEMP-CURR-LINE GREATER THAN PI-TOTAL-LINES
01291         MOVE ER-0066             TO EMI-ERROR
01292         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01293         COMPUTE TEMP-CURR-LINE = PI-TOTAL-LINES + 1
01294                                - NUM-LINES-PER-SCREEN
01295         IF TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS
01296            MOVE 1                TO TEMP-CURR-LINE
01297         END-IF
01298      END-IF.
01299
01300      PERFORM 7450-SET-INDX THRU 7450-EXIT.
01301      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
01302              VARYING SC-INDX FROM 1 BY 1 UNTIL
01303              SC-INDX > NUM-LINES-PER-SCREEN.
01304
01305      IF EMI-ERROR = ER-0066 OR = ER-0067 OR = ZEROS
01306         NEXT SENTENCE
01307      ELSE
01308         GO TO 8200-SEND-DATAONLY
01309      END-IF.
01310
01311      MOVE TEMP-CURR-LINE         TO PI-CURRENT-LINE.
01312      SET TB-INDX TO PI-CURRENT-LINE.
01313      MOVE LOW-VALUES             TO EL1284AI
                                          EL1284R
           PERFORM 7800-MOVE-SAVE-TO-MAP.
01314      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT
01315              VARYING SC-INDX FROM 1 BY 1
01316              UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
01317      PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.
01318      GO TO 8100-SEND-INITIAL-MAP.
01319      EJECT
01320
01321  7450-SET-INDX.
01322      IF PI-CURRENT-LINE = 0 AND PI-TOTAL-LINES = 0
01323         SET TB-INDX TO 1
01324      ELSE
01325         PERFORM 7500-READ-TS THRU 7599-EXIT
01326         IF PI-CURRENT-LINE = 0
01327            SET TB-INDX TO 1
01328         ELSE
01329            SET TB-INDX TO PI-CURRENT-LINE
01330         END-IF
01331      END-IF.
01332  7450-EXIT.
01333       EXIT.
01334      EJECT
01335  7500-READ-TS.
01336      
      * EXEC CICS HANDLE CONDITION
01337 *         QIDERR(7590-TS-QIDERR)
01338 *         ITEMERR(7585-QID-ITEMERR)
01339 *    END-EXEC.
      *    MOVE '"$N<                  ! ( #00003371' TO DFHEIV0
           MOVE X'22244E3C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303033333731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01340      SET TS-INDX TO 1.
01341      MOVE 1                      TO QID-ITEM.
01342  7501-LOOP.
01343      
      * EXEC CICS READQ TS
01344 *         INTO(TS-WORK-AREA)
01345 *         QUEUE(QID)
01346 *         LENGTH(TS-LENGTH)
01347 *         ITEM(QID-ITEM)
01348 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00003378' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 TS-WORK-AREA, 
                 TS-LENGTH, 
                 QID-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01349      MOVE TS-WORK-AREA           TO TS-GROUP (TS-INDX).
01350      SET TS-INDX UP BY 1.
01351      ADD 1 TO QID-ITEM.
01352      GO TO 7501-LOOP.
01353
01354  7585-QID-ITEMERR.
01355      IF EIBTRNID NOT = TRANS-ID
01356         SUBTRACT 1 FROM QID-ITEM
01357         MOVE QID-ITEM            TO PI-TEMP-STOR-ITEMS
01358      END-IF.
01359      GO TO 7599-EXIT.
01360
01361  7590-TS-QIDERR.
01362      IF EIBTRNID = TRANS-ID
01363         AND EIBAID = DFHCLEAR
01364            GO TO 9410-RETURN
01365      END-IF.
01366      IF EIBTRNID = TRANS-ID
01367         MOVE ER-0033             TO EMI-ERROR
01368         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01369         GO TO 8100-SEND-INITIAL-MAP
01370      END-IF.
01371
01372  7599-EXIT.
01373       EXIT.
01374
01375      EJECT
01376  7600-UPDATE-TABLE-FROM-SCREEN.
01377
01378      IF SC-TEXTL (SC-INDX) NOT = ZEROS
01379          IF TB-INDX NOT > PI-TOTAL-LINES
01380              PERFORM 7700-MOVE-DATA THRU 7700-EXIT
01381              SET TB-INDX UP BY 1
01382          ELSE
01383              IF PI-TOTAL-LINES = MAX-LINES
01384                  MOVE ER-0051    TO EMI-ERROR
01385                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01386                  GO TO 8200-SEND-DATAONLY
01387              ELSE
01388                  PERFORM 7700-MOVE-DATA THRU 7700-EXIT
01389                  SET TB-INDX UP BY 1
01390                  ADD 1 TO PI-TOTAL-LINES
01391              END-IF
01392          END-IF
01393      ELSE
01394         IF TB-INDX NOT > PI-TOTAL-LINES
01395            SET TB-INDX UP BY 1
01396         END-IF
01397      END-IF.
01398
01399  7699-EXIT.
01400       EXIT.
01401
01402  7700-MOVE-DATA.
01403      MOVE '1'                    TO PI-UPDATE-SW.
01404
01405      IF SC-TEXTL (SC-INDX) NOT = ZEROS
01406         MOVE SC-TEXT (SC-INDX)  TO REC-TEXT (TB-INDX)
01407         MOVE PI-PROCESSOR-ID    TO REC-LAST-MAINT-BY (TB-INDX)
01409         MOVE EIBTIME            TO REC-LAST-MAINT-HHMMSS (TB-INDX)
01411         MOVE SAVE-BIN-DATE      TO REC-LAST-MAINT-DT (TB-INDX)
01413      END-IF.
01414
01415  7700-EXIT.
01416       EXIT.
       7800-MOVE-SAVE-TO-MAP.
           MOVE PI-LONG-HLTH-APP TO LNGHLAPI
           MOVE PI-REWRITE TO REWRITEI
           MOVE PI-CHKCOVG TO CHKCOVGI
           MOVE PI-MR-RELEASE-DATE TO MRRELDTI.
01417      EJECT
01418  8100-SEND-INITIAL-MAP.
01419      MOVE SAVE-DATE              TO DATEO.
01420      MOVE EIBTIME                TO TIME-IN.
01421      MOVE TIME-OUT               TO TIMEO.
01422      MOVE PI-COMPANY-ID          TO CMPNYIDO.
01423      MOVE PI-PROCESSOR-ID        TO USERIDO.
01424      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGBO.
01425      MOVE PI-CARRIER             TO CARRO
01426 *    MOVE PI-GROUPING            TO FGROUPO.
01427 *    MOVE PI-STATE               TO FSTO.
01428 *    MOVE PI-ACCOUNT             TO FACOUNTO.
01429      MOVE PI-CERT-NO             TO CERTNOO
01429      MOVE PI-CLAIM-NO            TO CLAIMNOO
01430 *    MOVE PI-CERT-SFX            TO FCRTSFXO.
      *
           PERFORM 8150-READ-CLAIM.
      *
01431      MOVE  ' '                   TO DC-OPTION-CODE.
01432      MOVE PI-CERT-EFF-DT         TO DC-BIN-DATE-1.
01433      PERFORM 9700-LINK-DATE-CONVERT.
01434 *    MOVE DC-GREG-DATE-1-EDIT    TO FEFFDTO.
01435      MOVE PI-TOTAL-LINES         TO TOTO.
01443 *    IF PI-CLAIM-NOTE
01444 *        MOVE SCR-CLAIM-NOTE-TYPE TO FTYPEO
01445 *        MOVE SCR-PF6-CERT        TO PF6NOTEO
01446 *        MOVE SCR-CERT-YN         TO FTYPEYNO
01447 *        IF PI-CERT-NOTES-EXIST = 'Y'
01448 *            MOVE 'YES'           TO FCERTYNO
01449 *        ELSE
01450 *            MOVE 'NO '           TO FCERTYNO
01451 *        END-IF
01452 *    ELSE
01453 *        MOVE SCR-CERT-NOTE-TYPE TO FTYPEO
01454 *        MOVE SCR-PF6-CLAIM      TO PF6NOTEO
01455 *        MOVE SCR-CLAIM-YN       TO FTYPEYNO
01456 *        IF PI-CLAIM-NOTES-EXIST = 'Y'
01457 *            MOVE 'YES'           TO FCERTYNO
01458 *        ELSE
01459 *            MOVE 'NO '           TO FCERTYNO
01460 *        END-IF
01461 *    END-IF.
01462 *    IF PI-BILLING-NOTES-EXIST = 'Y'
01463 *        MOVE 'YES'           TO FBILLYNO
01464 *    ELSE
01465 *        MOVE 'NO '           TO FBILLYNO
01466 *    END-IF
01467
           MOVE EL1284R TO GROUP2.
           IF NOT CURSOR-SET
01468         MOVE -1                     TO LNGHLAPL
           END-IF
01469
01470      
      * EXEC CICS SEND
01471 *         MAP(MAP-NAME)
01472 *         MAPSET(MAPSET-NAME)
01473 *         FROM(EL1284AO)
01474 *         ERASE
01475 *         CURSOR
01476 *    END-EXEC.
           MOVE LENGTH OF
            EL1284AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00003507' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL1284AO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01477
01478      GO TO 9100-RETURN-TRAN.
01479
01480  8150-READ-CLAIM.
01012      
      * EXEC CICS HANDLE CONDITION
01013 *        NOTFND   (8150-NOT-FOUND)
01014 *    END-EXEC.
      *    MOVE '"$I                   ! ) #00003518' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303033353138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01016      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
01017      MOVE PI-CARRIER             TO MSTR-CARRIER.
01018      MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO.
01019      MOVE PI-CERT-NO             TO MSTR-CERT-NO.
01021
01022      
      * EXEC CICS READ
01023 *        UPDATE
01024 *        DATASET   ('ELMSTR')
01025 *        RIDFLD    (ELMSTR-KEY)
01026 *        INTO      (CLAIM-MASTER)
01027 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&"IL       EU         (   #00003526' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
121802     EVALUATE TRUE
121802        WHEN CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1
121802           MOVE PI-AH-OVERRIDE-L6
                                       TO TYPEO
121802        WHEN CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
121802           MOVE PI-LIFE-OVERRIDE-L6
                                       TO TYPEO
121802        WHEN CL-CLAIM-TYPE = 'I'
121802           MOVE '  IU  '         TO TYPEO
121802        WHEN CL-CLAIM-TYPE = 'G'
121802           MOVE ' GAP  '         TO TYPEO
052614        WHEN CL-CLAIM-TYPE = 'F'
052614           MOVE ' FAM  '         TO TYPEO
080322        WHEN CL-CLAIM-TYPE = 'B'
080322           MOVE ' BRV  '         TO TYPEO
080322        WHEN CL-CLAIM-TYPE = 'H'
080322           MOVE ' HSP '          TO TYPEO
100518        WHEN CL-CLAIM-TYPE = 'O'
100518           MOVE ' OTH  '         TO TYPEO
121802     END-EVALUATE.
           MOVE CL-INSURED-1ST-NAME    TO FSTNMEO.
           MOVE CL-INSURED-LAST-NAME   TO LASTNMEO.
       8150-NOT-FOUND.
           MOVE SPACES    TO TYPEO.
      *
01480  8200-SEND-DATAONLY.
01481      MOVE EIBTIME                TO TIME-IN.
01482      MOVE TIME-OUT               TO TIMEO.
01483      MOVE PI-COMPANY-ID          TO CMPNYIDO.
01484      MOVE PI-PROCESSOR-ID        TO USERIDO.
01485      MOVE PI-TOTAL-LINES         TO TOTO.
           MOVE EL1284R TO GROUP2.
01486
01487      IF NOT EMI-NO-ERRORS
01488         MOVE EMI-MESSAGE-AREA (1) TO ERRMSGBO
01489      ELSE
01490         MOVE -1                  TO FUNCTL
01491      END-IF.
01492
01493      
      * EXEC CICS SEND
01494 *         MAP(MAP-NAME)
01495 *         MAPSET(MAPSET-NAME)
01496 *         FROM(EL1284AO)
01497 *         DATAONLY
01498 *         CURSOR
01499 *    END-EXEC.
           MOVE LENGTH OF
            EL1284AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00003571' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL1284AO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01500
01501      GO TO 9100-RETURN-TRAN.
01502
01503  8300-SEND-TEXT.
01504      
      * EXEC CICS SEND TEXT
01505 *         FROM(LOGOFF-TEXT)
01506 *         ERASE
01507 *         FREEKB
01508 *         LENGTH(LOGOFF-LENGTH)
01509 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00003582' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353832' TO DFHEIV0(25:11)
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
           
01510
01511      PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT.
01512
01513      
      * EXEC CICS RETURN
01514 *    END-EXEC.
      *    MOVE '.(                    ''   #00003591' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01515
01516  8800-UNAUTHORIZED-ACCESS.
01517      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
01518      GO TO 8300-SEND-TEXT.
01519
01520  9000-RETURN-CICS.
01521      IF PI-CHANGES-MADE
01522         MOVE ER-0045             TO EMI-ERROR
01523         MOVE -1                  TO FUNCTL
01524         MOVE SPACES              TO PFENTERO
01525         MOVE AL-UNNOF            TO PFENTERA
01526         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01527         GO TO 8200-SEND-DATAONLY
01528      END-IF.
01529
01530      MOVE EIBAID                 TO PI-ENTRY-CD-1.
01531      MOVE XCTL-005               TO PGM-NAME.
01532      GO TO 9300-XCTL.
01533
01534  9100-RETURN-TRAN.
01535      MOVE SCRN-NUMBER            TO PI-CURRENT-SCREEN-NO.
01536      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
01537      
      * EXEC CICS RETURN
01538 *         TRANSID(TRANS-ID)
01539 *         COMMAREA(PROGRAM-INTERFACE-BLOCK)
01540 *         LENGTH(PI-COMM-LENGTH)
01541 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00003615' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01542
01543
01544  9200-RETURN-MAIN-MENU.
01545      IF PI-CHANGES-MADE
01546         MOVE -1                  TO FUNCTL
01547         MOVE SPACES              TO PFENTERO
01548         MOVE AL-UNNOF            TO PFENTERA
01549         MOVE ER-0045             TO EMI-ERROR
01550         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01551         GO TO 8200-SEND-DATAONLY
01552      END-IF.
01553
01554      MOVE XCTL-126               TO PGM-NAME.
01555
01556  9300-XCTL.
01557      PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT.
01558      
      * EXEC CICS XCTL
01559 *         PROGRAM  (PGM-NAME)
01560 *         COMMAREA (PROGRAM-INTERFACE-BLOCK)
01561 *         LENGTH   (PI-COMM-LENGTH)
01562 *    END-EXEC.
      *    MOVE '.$C                   %   #00003636' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01563
01564  9400-CLEAR.
01565
01566      IF PI-CHANGES-MADE
01567          MOVE ER-0045            TO EMI-ERROR
               MOVE -1                 TO FUNCTL
               SET CURSOR-SET TO TRUE
01568          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
               PERFORM 7800-MOVE-SAVE-TO-MAP
01569          IF PI-CURRENT-LINE GREATER THAN ZERO
01570              PERFORM 7500-READ-TS THRU 7599-EXIT
01571              SET TB-INDX TO PI-CURRENT-LINE
01572              PERFORM 7100-FORMAT-SCREEN  THRU  7100-EXIT
01573                  VARYING SC-INDX FROM 1 BY 1 UNTIL
01574                  SC-INDX GREATER NUM-LINES-PER-SCREEN
01575          END-IF
01576          GO TO 8100-SEND-INITIAL-MAP
01577      END-IF.
01578
01579  9410-RETURN.
01580 *    IF PF5-PRESSED
01581 *        MOVE PGM-EL1276           TO PGM-NAME
01582 *    ELSE
01583 *        IF PF6-PRESSED
01584 *            IF PI-CLAIM-NOTE
01585 *                SET PI-CERT-NOTE  TO TRUE
01586 *            ELSE
01587 *                SET PI-CLAIM-NOTE TO TRUE
01588 *            END-IF
01589 *            SET PI-CHANGE-IN-NOTE-TYPE TO TRUE
01590 *            MOVE 'N'              TO PI-PF6-PRESSED
01591 *            GO TO 1000-START
01592 *        ELSE
01593              MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME
01594 *        END-IF
01595 *    END-IF.
01596      GO TO 9300-XCTL.
01597
01598  9500-PF12.
01599      IF PI-CHANGES-MADE
01600         MOVE -1                  TO FUNCTL
01601         MOVE SPACES              TO PFENTERO
01602         MOVE AL-UNNOF            TO PFENTERA
01603         MOVE ER-0045             TO EMI-ERROR
01604         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01605         GO TO 8200-SEND-DATAONLY
01606      END-IF.
01607
01608      MOVE 'EL010'                TO PGM-NAME.
01609      GO TO 9300-XCTL.
01610
01611      EJECT
01612
01613      EJECT
01614  9600-PGMID-ERROR.
01615      
      * EXEC CICS HANDLE CONDITION
01616 *         PGMIDERR(8300-SEND-TEXT)
01617 *    END-EXEC.
      *    MOVE '"$L                   ! * #00003696' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303033363936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01618
01619      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
01620      MOVE SPACES                 TO  PI-ENTRY-CD-1.
01621      MOVE XCTL-005               TO  PGM-NAME.
01622      MOVE PGM-NAME               TO  LOGOFF-PGM.
01623      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
01624
01625      GO TO 9300-XCTL.
01626
01627  9700-LINK-DATE-CONVERT.
01628
01629      MOVE LINK-ELDATCV           TO  PGM-NAME.
01630      
      * EXEC CICS LINK
01631 *        PROGRAM    (PGM-NAME)
01632 *        COMMAREA   (DATE-CONVERSION-DATA)
01633 *        LENGTH     (DC-COMM-LENGTH)
01634 *    END-EXEC.
      *    MOVE '."C                   (   #00003711' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01635
01636  9700-EXIT.
01637      EXIT.
01638
01639  9900-ERROR-FORMAT.
01640      IF NOT EMI-ERRORS-COMPLETE
01641         MOVE LINK-001            TO  PGM-NAME
01642         
      * EXEC CICS LINK
01643 *            PROGRAM(PGM-NAME)
01644 *            COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)
01645 *            LENGTH(EMI-COMM-LENGTH)
01646 *       END-EXEC
      *    MOVE '."C                   (   #00003723' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01647      END-IF.
01648  9900-EXIT.
01649      EXIT.
01650
01651  9990-ABEND.
01652      MOVE LINK-004               TO  PGM-NAME.
01653      MOVE DFHEIBLK               TO  EMI-LINE1.
01654
01655      
      * EXEC CICS LINK
01656 *        PROGRAM   (PGM-NAME)
01657 *        COMMAREA  (EMI-LINE1)
01658 *        LENGTH    (72)
01659 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00003736' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01660
01661      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSGBO.
01662      MOVE -1                     TO  FUNCTL.
01663
01664      GO TO 8100-SEND-INITIAL-MAP.
01665
01666      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1284' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01667
01668  9995-SECURITY-VIOLATION.
01669 *                                COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00003767' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373637' TO DFHEIV0(25:11)
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
01670
01671  9995-EXIT.
01672      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1284' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9400-CLEAR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 9990-ABEND,
                     9600-PGMID-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 4610-ENDBR,
                     6000-NOT-OPEN,
                     4610-ENDBR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 4900-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 7010-ENDBR,
                     6000-NOT-OPEN,
                     7010-ENDBR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 7299-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 7590-TS-QIDERR,
                     7585-QID-ITEMERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 8150-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1284' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
