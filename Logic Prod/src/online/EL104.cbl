00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL104 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 10:18:15.
00007 *                            VMOD=2.010
00008 *
00008 *
00009 *AUTHOR.        LOGIC,INC.
00010 *               DALLAS,TEXAS.
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
00025 *        TRANSACTION EX06 - TEXT FILE MAINTENANCE
00026 *        THIS PROGRAM IS USED TO PERFORM MAINTENANCE TO THE HELP,
00027 *        LETTER, AND FORM FILES.
00028 *
00029 *        IT WILL RENUMBER, COPY OR DELETE RECORDS FROM THE FILES
00030 *        OR IT WILL BROWSE THE FILE AND BUILD TEMP STORAGE ITEMS
00031 *        FOR EACH RECORD THAT MATCHES THE KEY AND PASS CONTROL TO
00032 *        PROGRAM EL1042 FOR MAINTENANCE.
020410******************************************************************
020410*                   C H A N G E   L O G
020410*
020410* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
020410*-----------------------------------------------------------------
020410*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
020410* EFFECTIVE    NUMBER
020410*-----------------------------------------------------------------
020410* 020410  CR2009061500002  AJRA  FIX HELP FILE SELECTION
020410******************************************************************
00033
00034  ENVIRONMENT DIVISION.
00035  DATA DIVISION.
00036  EJECT
00037  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00038  77  FILLER  PIC X(32)  VALUE '********************************'.
00039  77  FILLER  PIC X(32)  VALUE '*    EL104 WORKING STORAGE     *'.
00040  77  FILLER  PIC X(32)  VALUE '******** V/M 2.010 *************'.
00041
00042 *    COPY ELCSCTM.
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
00043 *    COPY ELCSCRTY.
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
00044 *    COPY MPCSCRT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            MPCSCRT                             *
00004 *                            VMOD=1.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
00007 *        ACQUIRED BY SIGN-ON PROGRAM EL125.                      *
00008 *                                      (MP MORTGAGE PROTECTION)  *
00009 *                                                                *
00010 ******************************************************************
00011 *
00012  01  SECURITY-CONTROL-E.
00013      12  SC-COMM-LENGTH-E             PIC S9(04) VALUE +144 COMP.
00014      12  FILLER                       PIC  X(02) VALUE 'SC'.
00015      12  SC-QUID-KEY.
00016          16  SC-QUID-TERMINAL         PIC  X(04).
00017          16  SC-QUID-SYSTEM           PIC  X(04).
00018      12  SC-ITEM                      PIC S9(04) VALUE +1   COMP.
00019      12  SC-SECURITY-ACCESS-CODE      PIC  X(01).
00020      12  SC-PRODUCER-AUTHORIZED-SW    PIC  X(01).
00021          88 SC-PRODUCER-AUTHORIZED               VALUE ' '.
00022          88 SC-PRODUCER-NOT-AUTHORIZED           VALUE 'N'.
00023      12  SC-MP-CODES.
00024          16  SC-MP-AUTHORIZATION OCCURS 44 TIMES.
00025              20  SC-MP-DISPLAY        PIC  X(01).
00026              20  SC-MP-UPDATE         PIC  X(01).
00027      12  FILLER                       PIC  X(40).
00045
00046     EJECT
00047
00048  01  WS-DATE-AREA.
00049      12  SAVE-DATE               PIC X(8)    VALUE SPACES.
00050      12  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.
00051
00052  01  WS-CONSTANTS.
00053      12  W-APPL-SCRTY-NDX        PIC S9(4) COMP VALUE +7.
00054      12  W-SC-ITEM               PIC S9(4) COMP VALUE +1.
00055      12  MAP-NAME.
00056          16  MAP-NAME-PRE        PIC X(2)    VALUE 'EL'.
00057          16  MAP-NUMBER          PIC X(4)    VALUE '104A'.
00058          16  MAP-NAME-FILL       PIC X(2)    VALUE SPACES.
00059      12  MAPSET-NAME             PIC X(8)    VALUE 'EL104S'.
00060      12  TRANS-ID                PIC X(4)    VALUE 'EX06'.
00061      12  PGM-NAME                PIC X(8).
00062      12  PGM-EL1042              PIC X(8)    VALUE 'EL1042'.
00063      12  THIS-PGM                PIC X(8)    VALUE 'EL104'.
00064      12  W-MAINT                 PIC X(1).
00065          88  W-ADD                           VALUE 'A'.
00066          88  W-CHANGE                        VALUE 'C'.
00067          88  W-COPY                          VALUE 'K'.
00068          88  W-DELETE                        VALUE 'D' 'E'.
00069          88  W-RENAME                        VALUE 'R'.
00070          88  W-SHOW                          VALUE 'S' 'M'.
00071          88  W-TRANSFORM                     VALUE 'T'.
00072          88  W-VALID-MAINT-CR                VALUE 'A' 'C' 'D' 'K'
00073                                                    'R' 'S' 'M' 'E'
00074                                                    'T'.
00075          88  W-VALID-MAINT-REST              VALUE 'A' 'C' 'D' 'K'
00076                                                    'R' 'S' 'M'
00077                                                    'E'.
00078      12  W-FILE-TYPE             PIC X(1).
00079          88  W-FORM                          VALUE 'F'.
00080          88  W-HELP                          VALUE 'H' 'A'.
00081          88  W-LETTER                        VALUE 'L'.
00082      12  XCTL-CLAIM              PIC X(8)    VALUE 'EL126'.
00083      12  XCTL-CREDIT             PIC X(8)    VALUE 'EL626'.
00084      12  XCTL-GEN-LEDGER         PIC X(8)    VALUE 'GL800'.
00085      12  XCTL-LIFE               PIC X(8)    VALUE 'LF400'.
00086      12  XCTL-MORTGAGE           PIC X(8)    VALUE 'EM626'.
00087      12  XCTL-WARRANTY           PIC X(8)    VALUE 'WA126'.
00088      12  FILE-ID                 PIC X(8).
00089      12  FILE-KEY.
00090          16  FILE-PARTIAL-KEY.
00091            18  CO-CD             PIC X.
00092            18  CNTL-AREA         PIC X(12).
00093          16  SEQ                 PIC S9(4)   COMP.
00094      12  BROWSE-STARTED-SW       PIC X       VALUE SPACE.
00095          88  BROWSE-STARTED      VALUE 'Y'.
00096      12  LETTER-ID               PIC X(8)    VALUE 'ELLETR'.
00097      12  FORM-ID                 PIC X(8)    VALUE 'ELFORM'.
00098      12  HELP-ID                 PIC X(8)    VALUE 'ELHELP'.
00099      12  REQUEST-TYPE            PIC X       VALUE SPACES.
00100          88  RENUMBER-REQUEST    VALUE '1'.
00101          88  COPY-REQUEST        VALUE '2'.
00102          88  DELETE-REQUEST      VALUE '3'.
00103      12  CNTL-KEY                PIC X(12).
00104      12  CNTL-K1  REDEFINES CNTL-KEY.
00105          16  K1                  PIC X.
00106          16  K2                  PIC X.
00107          16  K3                  PIC X.
00108          16  K4                  PIC X.
00109          16  K5                  PIC X.
00110          16  FILLER              PIC X.
00111          16  K7                  PIC X.
00112          16  K8                  PIC X.
00113          16  K9                  PIC X.
00114          16  FILLER              PIC X(2).
00115          16  K12                 PIC X.
00116      12  TS-NAME.
00117          16  FILLER              PIC X(4)    VALUE '104A'.
00118          16  TS-TERM             PIC X(4).
00119      12  TS-ITEM                 PIC S9(4)   COMP VALUE +0.
00120
00121      12  FILE-LENGTH             PIC S9(4)   COMP VALUE +100.
00122      12  UPDATE-SW               PIC 9       VALUE 0.
00123          88  FILE-UPDATED        VALUE 1.
00124
00125      12  OLD-KEY.
00126          16  OLD-CO-CD           PIC X.
00127          16  OLD-COMM-AREA       PIC X(12).
00128          16  OLD-SEQ             PIC S9(4)   COMP.
00129      12  OLD-KEY-SAVE            PIC X(15).
00130
00131      12  TIME-IN                 PIC S9(7).
00132      12  FILLER REDEFINES TIME-IN.
00133          16  FILLER              PIC X.
00134          16  TIME-OUT            PIC 99V99.
00135          16  FILLER              PIC 9(2).
00136      12  WS-HEX-01               PIC S9(4) COMP  VALUE +1.
00137      12  FILLER REDEFINES WS-HEX-01.
00138          16  FILLER              PIC X.
00139          16  WS-LGX-COMPANY-CD   PIC X.
00140  EJECT
00141 *    COPY ELCLOGOF.
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
00142  EJECT
00143 *    COPY ELCAID.
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
00144  01  FILLER  REDEFINES DFHAID.
00145      12  FILLER                  PIC X(8).
00146      12  PF-VALUES OCCURS 24 TIMES       PIC X.
00147  EJECT
00148 *    COPY ELCEMIB.
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
00149      05  WS-ERROR-MESSAGE-AREA.
00150          10  ER-0000                 PIC 9(4)   VALUE 0000.
00151          10  ER-0004                 PIC 9(4)   VALUE 0004.
00152          10  ER-0005                 PIC 9(4)   VALUE 0005.
00153          10  ER-0006                 PIC 9(4)   VALUE 0006.
00154          10  ER-0008                 PIC 9(4)   VALUE 0008.
00155          10  ER-0009                 PIC 9(4)   VALUE 0009.
00156          10  ER-0012                 PIC 9(4)   VALUE 0012.
00157          10  ER-0013                 PIC 9(4)   VALUE 0013.
00158          10  ER-0014                 PIC 9(4)   VALUE 0014.
00159          10  ER-0015                 PIC 9(4)   VALUE 0015.
00160          10  ER-0023                 PIC 9(4)   VALUE 0023.
00161          10  ER-0029                 PIC 9(4)   VALUE 0029.
00162          10  ER-0070                 PIC 9(4)   VALUE 0070.
00163          10  ER-0076                 PIC 9(4)   VALUE 0076.
00164          10  ER-1162                 PIC 9(4)   VALUE 1162.
00165          10  ER-2237                 PIC 9(4)   VALUE 2237.
00166          10  ER-2238                 PIC 9(4)   VALUE 2238.
00167          10  ER-7270                 PIC 9(4)   VALUE 7270.
00168          10  ER-7271                 PIC 9(4)   VALUE 7271.
00169          10  ER-7392                 PIC 9(4)   VALUE 7392.
00170          10  ER-9097                 PIC 9(4)   VALUE 9097.
00171  EJECT
00172 *    COPY ELCINTF.
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
00173      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.
00174 *    COPY ELC1042.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                           ELC1042                              *
00004 *                            VMOD=2.002                          *
00005 *                                                                *
00006 *    NOTE                                                        *
00007 *        THE WORK AREA IS USED BY EL152, EL1522, EL1042, EL153,  *
00008 *        EM152, EM1522, EL689, EL6892, EL6311, AND EL690.        *
00009 *        THIS COPYBOOK SHOULD NOT BE CHANGED WITHOUT REFERENCE   *
00010 *        TO THESE PROGRAMS.                                      *
00011 *                                                                *
00012 *    NOTE                                                        *
00013 *        THE FILLER AREA AT THE BOTTOM ARE FOR FUTURE EL1042     *
00014 *        USE ONLY!                                               *
00015 *                                                                *
00016 ******************************************************************
00017
00018          16  PI-1042-WA.
00019              20  PI-ACTION       PIC  X(01).
00020                  88 PI-SHOW-MODE           VALUE '1'.
00021                  88 PI-CLEAR-MODE          VALUE '2'.
00022                  88 PI-CREATE-MODE         VALUE '3'.
00023              20  PI-COMM-CONTROL PIC  X(12).
00024              20  PI-CURRENT-LINE PIC S9(03) COMP-3.
00025              20  PI-EOF-SW       PIC  X(01).
00026                  88  PI-FILE-EOF           VALUE 'Y'.
00027              20  PI-FILETYP      PIC  X(01).
00028              20  PI-FORM-SQUEEZE-CONTROL
00029                                  PIC  X(01).
00030                  88  PI-FORM-SQUEEZE-ON     VALUE 'Y'.
00031                  88  PI-FORM-SQUEEZE-OFF    VALUE ' '.
00032              20  PI-LAST-CONTROL PIC  X(12).
00033              20  PI-TEMP-STOR-ITEMS
00034                                  PIC S9(04) COMP.
00035              20  PI-TOTAL-LINES  PIC S9(03) COMP-3.
00036              20  PI-UPDATE-SW    PIC  9(01).
00037                  88 ANY-UPDATES            VALUE 1.
00038              20  PI-104-SCREEN-SENT-IND
00039                                  PIC  X(01).
00040                  88  PI-104-SCREEN-SENT    VALUE 'Y'.
00041                  88  PI-104-SCREEN-NOT-SENT VALUE 'N'.
00042              20  PI-1042-SCREEN-SENT-IND
00043                                  PIC  X(01).
00044                  88  PI-1042-SCREEN-SENT    VALUE 'Y'.
00045                  88  PI-1042-SCREEN-NOT-SENT VALUE 'N'.
00046              20  PI-1042-ARCHIVE-IND
00047                                  PIC  X(01).
00048                  88  PI-1042-ARCHIVE-LETTER VALUE 'Y'.
00049              20  FILLER          PIC  X(29).
00175          16  FILLER              PIC X(431).
00176          16  PI-104-SCREEN-BYPASS.
00177              20  PI-104-TYPE     PIC  X(01).
00178              20  PI-104-NAME     PIC  X(12).
00179          16  PI-OLD-NAME         PIC  X(04).
00180          16  PI-NEW-NAME         PIC  X(04).
00181          16  PI-1043-ERROR       PIC  9(04).
00182          16  FILLER              PIC X(117).
00183  EJECT
00184 *    COPY ELCATTR.
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
00185  EJECT
00186 *    COPY ELCDATE.
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
00187  EJECT
00188 *    COPY EL104S.
       01  EL104BFI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  DATEBFL PIC S9(0004) COMP.
           05  DATEBFF PIC  X(0001).
           05  FILLER REDEFINES DATEBFF.
               10  DATEBFA PIC  X(0001).
           05  DATEBFI PIC  X(0008).
      *    -------------------------------
           05  TIMEBFL PIC S9(0004) COMP.
           05  TIMEBFF PIC  X(0001).
           05  FILLER REDEFINES TIMEBFF.
               10  TIMEBFA PIC  X(0001).
           05  TIMEBFI PIC  X(0005).
      *    -------------------------------
           05  COMPFL PIC S9(0004) COMP.
           05  COMPFF PIC  X(0001).
           05  FILLER REDEFINES COMPFF.
               10  COMPFA PIC  X(0001).
           05  COMPFI PIC  X(0003).
      *    -------------------------------
           05  TXTTPBFL PIC S9(0004) COMP.
           05  TXTTPBFF PIC  X(0001).
           05  FILLER REDEFINES TXTTPBFF.
               10  TXTTPBFA PIC  X(0001).
           05  TXTTPBFI PIC  X(0008).
      *    -------------------------------
           05  CNTRLBFL PIC S9(0004) COMP.
           05  CNTRLBFF PIC  X(0001).
           05  FILLER REDEFINES CNTRLBFF.
               10  CNTRLBFA PIC  X(0001).
           05  CNTRLBFI PIC  X(0012).
      *    -------------------------------
           05  ARCHTBFL PIC S9(0004) COMP.
           05  ARCHTBFF PIC  X(0001).
           05  FILLER REDEFINES ARCHTBFF.
               10  ARCHTBFA PIC  X(0001).
           05  ARCHTBFI PIC  X(0009).
      *    -------------------------------
           05  ARCHBFL PIC S9(0004) COMP.
           05  ARCHBFF PIC  X(0001).
           05  FILLER REDEFINES ARCHBFF.
               10  ARCHBFA PIC  X(0001).
           05  ARCHBFI PIC  X(0001).
      *    -------------------------------
           05  FRMSTBFL PIC S9(0004) COMP.
           05  FRMSTBFF PIC  X(0001).
           05  FILLER REDEFINES FRMSTBFF.
               10  FRMSTBFA PIC  X(0001).
           05  FRMSTBFI PIC  X(0012).
      *    -------------------------------
           05  FRMSQBFL PIC S9(0004) COMP.
           05  FRMSQBFF PIC  X(0001).
           05  FILLER REDEFINES FRMSQBFF.
               10  FRMSQBFA PIC  X(0001).
           05  FRMSQBFI PIC  X(0001).
      *    -------------------------------
           05  TOTFL PIC S9(0004) COMP.
           05  TOTFF PIC  X(0001).
           05  FILLER REDEFINES TOTFF.
               10  TOTFA PIC  X(0001).
           05  TOTFI PIC  999.
      *    -------------------------------
           05  LNESQTFL PIC S9(0004) COMP.
           05  LNESQTFF PIC  X(0001).
           05  FILLER REDEFINES LNESQTFF.
               10  LNESQTFA PIC  X(0001).
           05  LNESQTFI PIC  X(0001).
      *    -------------------------------
           05  L1FL PIC S9(0004) COMP.
           05  L1FF PIC  X(0001).
           05  FILLER REDEFINES L1FF.
               10  L1FA PIC  X(0001).
           05  L1FI PIC  X(0003).
      *    -------------------------------
           05  TEXT1FL PIC S9(0004) COMP.
           05  TEXT1FF PIC  X(0001).
           05  FILLER REDEFINES TEXT1FF.
               10  TEXT1FA PIC  X(0001).
           05  TEXT1FI PIC  X(0070).
      *    -------------------------------
           05  CF1FL PIC S9(0004) COMP.
           05  CF1FF PIC  X(0001).
           05  FILLER REDEFINES CF1FF.
               10  CF1FA PIC  X(0001).
           05  CF1FI PIC  X(0002).
      *    -------------------------------
           05  SQ1FL PIC S9(0004) COMP.
           05  SQ1FF PIC  X(0001).
           05  FILLER REDEFINES SQ1FF.
               10  SQ1FA PIC  X(0001).
           05  SQ1FI PIC  X(0001).
      *    -------------------------------
           05  L2FL PIC S9(0004) COMP.
           05  L2FF PIC  X(0001).
           05  FILLER REDEFINES L2FF.
               10  L2FA PIC  X(0001).
           05  L2FI PIC  X(0003).
      *    -------------------------------
           05  TEXT2FL PIC S9(0004) COMP.
           05  TEXT2FF PIC  X(0001).
           05  FILLER REDEFINES TEXT2FF.
               10  TEXT2FA PIC  X(0001).
           05  TEXT2FI PIC  X(0070).
      *    -------------------------------
           05  CF2FL PIC S9(0004) COMP.
           05  CF2FF PIC  X(0001).
           05  FILLER REDEFINES CF2FF.
               10  CF2FA PIC  X(0001).
           05  CF2FI PIC  X(0002).
      *    -------------------------------
           05  SQ2FL PIC S9(0004) COMP.
           05  SQ2FF PIC  X(0001).
           05  FILLER REDEFINES SQ2FF.
               10  SQ2FA PIC  X(0001).
           05  SQ2FI PIC  X(0001).
      *    -------------------------------
           05  L3FL PIC S9(0004) COMP.
           05  L3FF PIC  X(0001).
           05  FILLER REDEFINES L3FF.
               10  L3FA PIC  X(0001).
           05  L3FI PIC  X(0003).
      *    -------------------------------
           05  TEXT3FL PIC S9(0004) COMP.
           05  TEXT3FF PIC  X(0001).
           05  FILLER REDEFINES TEXT3FF.
               10  TEXT3FA PIC  X(0001).
           05  TEXT3FI PIC  X(0070).
      *    -------------------------------
           05  CF3FL PIC S9(0004) COMP.
           05  CF3FF PIC  X(0001).
           05  FILLER REDEFINES CF3FF.
               10  CF3FA PIC  X(0001).
           05  CF3FI PIC  X(0002).
      *    -------------------------------
           05  SQ3FL PIC S9(0004) COMP.
           05  SQ3FF PIC  X(0001).
           05  FILLER REDEFINES SQ3FF.
               10  SQ3FA PIC  X(0001).
           05  SQ3FI PIC  X(0001).
      *    -------------------------------
           05  L4FL PIC S9(0004) COMP.
           05  L4FF PIC  X(0001).
           05  FILLER REDEFINES L4FF.
               10  L4FA PIC  X(0001).
           05  L4FI PIC  X(0003).
      *    -------------------------------
           05  TEXT4FL PIC S9(0004) COMP.
           05  TEXT4FF PIC  X(0001).
           05  FILLER REDEFINES TEXT4FF.
               10  TEXT4FA PIC  X(0001).
           05  TEXT4FI PIC  X(0070).
      *    -------------------------------
           05  CF4FL PIC S9(0004) COMP.
           05  CF4FF PIC  X(0001).
           05  FILLER REDEFINES CF4FF.
               10  CF4FA PIC  X(0001).
           05  CF4FI PIC  X(0002).
      *    -------------------------------
           05  SQ4FL PIC S9(0004) COMP.
           05  SQ4FF PIC  X(0001).
           05  FILLER REDEFINES SQ4FF.
               10  SQ4FA PIC  X(0001).
           05  SQ4FI PIC  X(0001).
      *    -------------------------------
           05  L5FL PIC S9(0004) COMP.
           05  L5FF PIC  X(0001).
           05  FILLER REDEFINES L5FF.
               10  L5FA PIC  X(0001).
           05  L5FI PIC  X(0003).
      *    -------------------------------
           05  TEXT5FL PIC S9(0004) COMP.
           05  TEXT5FF PIC  X(0001).
           05  FILLER REDEFINES TEXT5FF.
               10  TEXT5FA PIC  X(0001).
           05  TEXT5FI PIC  X(0070).
      *    -------------------------------
           05  CF5FL PIC S9(0004) COMP.
           05  CF5FF PIC  X(0001).
           05  FILLER REDEFINES CF5FF.
               10  CF5FA PIC  X(0001).
           05  CF5FI PIC  X(0002).
      *    -------------------------------
           05  SQ5FL PIC S9(0004) COMP.
           05  SQ5FF PIC  X(0001).
           05  FILLER REDEFINES SQ5FF.
               10  SQ5FA PIC  X(0001).
           05  SQ5FI PIC  X(0001).
      *    -------------------------------
           05  L6FL PIC S9(0004) COMP.
           05  L6FF PIC  X(0001).
           05  FILLER REDEFINES L6FF.
               10  L6FA PIC  X(0001).
           05  L6FI PIC  X(0003).
      *    -------------------------------
           05  TEXT6FL PIC S9(0004) COMP.
           05  TEXT6FF PIC  X(0001).
           05  FILLER REDEFINES TEXT6FF.
               10  TEXT6FA PIC  X(0001).
           05  TEXT6FI PIC  X(0070).
      *    -------------------------------
           05  CF6FL PIC S9(0004) COMP.
           05  CF6FF PIC  X(0001).
           05  FILLER REDEFINES CF6FF.
               10  CF6FA PIC  X(0001).
           05  CF6FI PIC  X(0002).
      *    -------------------------------
           05  SQ6FL PIC S9(0004) COMP.
           05  SQ6FF PIC  X(0001).
           05  FILLER REDEFINES SQ6FF.
               10  SQ6FA PIC  X(0001).
           05  SQ6FI PIC  X(0001).
      *    -------------------------------
           05  L7FL PIC S9(0004) COMP.
           05  L7FF PIC  X(0001).
           05  FILLER REDEFINES L7FF.
               10  L7FA PIC  X(0001).
           05  L7FI PIC  X(0003).
      *    -------------------------------
           05  TEXT7FL PIC S9(0004) COMP.
           05  TEXT7FF PIC  X(0001).
           05  FILLER REDEFINES TEXT7FF.
               10  TEXT7FA PIC  X(0001).
           05  TEXT7FI PIC  X(0070).
      *    -------------------------------
           05  CF7FL PIC S9(0004) COMP.
           05  CF7FF PIC  X(0001).
           05  FILLER REDEFINES CF7FF.
               10  CF7FA PIC  X(0001).
           05  CF7FI PIC  X(0002).
      *    -------------------------------
           05  SQ7FL PIC S9(0004) COMP.
           05  SQ7FF PIC  X(0001).
           05  FILLER REDEFINES SQ7FF.
               10  SQ7FA PIC  X(0001).
           05  SQ7FI PIC  X(0001).
      *    -------------------------------
           05  L8FL PIC S9(0004) COMP.
           05  L8FF PIC  X(0001).
           05  FILLER REDEFINES L8FF.
               10  L8FA PIC  X(0001).
           05  L8FI PIC  X(0003).
      *    -------------------------------
           05  TEXT8FL PIC S9(0004) COMP.
           05  TEXT8FF PIC  X(0001).
           05  FILLER REDEFINES TEXT8FF.
               10  TEXT8FA PIC  X(0001).
           05  TEXT8FI PIC  X(0070).
      *    -------------------------------
           05  CF8FL PIC S9(0004) COMP.
           05  CF8FF PIC  X(0001).
           05  FILLER REDEFINES CF8FF.
               10  CF8FA PIC  X(0001).
           05  CF8FI PIC  X(0002).
      *    -------------------------------
           05  SQ8FL PIC S9(0004) COMP.
           05  SQ8FF PIC  X(0001).
           05  FILLER REDEFINES SQ8FF.
               10  SQ8FA PIC  X(0001).
           05  SQ8FI PIC  X(0001).
      *    -------------------------------
           05  L9FL PIC S9(0004) COMP.
           05  L9FF PIC  X(0001).
           05  FILLER REDEFINES L9FF.
               10  L9FA PIC  X(0001).
           05  L9FI PIC  X(0003).
      *    -------------------------------
           05  TEXT9FL PIC S9(0004) COMP.
           05  TEXT9FF PIC  X(0001).
           05  FILLER REDEFINES TEXT9FF.
               10  TEXT9FA PIC  X(0001).
           05  TEXT9FI PIC  X(0070).
      *    -------------------------------
           05  CF9FL PIC S9(0004) COMP.
           05  CF9FF PIC  X(0001).
           05  FILLER REDEFINES CF9FF.
               10  CF9FA PIC  X(0001).
           05  CF9FI PIC  X(0002).
      *    -------------------------------
           05  SQ9FL PIC S9(0004) COMP.
           05  SQ9FF PIC  X(0001).
           05  FILLER REDEFINES SQ9FF.
               10  SQ9FA PIC  X(0001).
           05  SQ9FI PIC  X(0001).
      *    -------------------------------
           05  L10FL PIC S9(0004) COMP.
           05  L10FF PIC  X(0001).
           05  FILLER REDEFINES L10FF.
               10  L10FA PIC  X(0001).
           05  L10FI PIC  X(0003).
      *    -------------------------------
           05  TEXT10FL PIC S9(0004) COMP.
           05  TEXT10FF PIC  X(0001).
           05  FILLER REDEFINES TEXT10FF.
               10  TEXT10FA PIC  X(0001).
           05  TEXT10FI PIC  X(0070).
      *    -------------------------------
           05  CF10FL PIC S9(0004) COMP.
           05  CF10FF PIC  X(0001).
           05  FILLER REDEFINES CF10FF.
               10  CF10FA PIC  X(0001).
           05  CF10FI PIC  X(0002).
      *    -------------------------------
           05  SQ10FL PIC S9(0004) COMP.
           05  SQ10FF PIC  X(0001).
           05  FILLER REDEFINES SQ10FF.
               10  SQ10FA PIC  X(0001).
           05  SQ10FI PIC  X(0001).
      *    -------------------------------
           05  L11FL PIC S9(0004) COMP.
           05  L11FF PIC  X(0001).
           05  FILLER REDEFINES L11FF.
               10  L11FA PIC  X(0001).
           05  L11FI PIC  X(0003).
      *    -------------------------------
           05  TEXT11FL PIC S9(0004) COMP.
           05  TEXT11FF PIC  X(0001).
           05  FILLER REDEFINES TEXT11FF.
               10  TEXT11FA PIC  X(0001).
           05  TEXT11FI PIC  X(0070).
      *    -------------------------------
           05  CF11FL PIC S9(0004) COMP.
           05  CF11FF PIC  X(0001).
           05  FILLER REDEFINES CF11FF.
               10  CF11FA PIC  X(0001).
           05  CF11FI PIC  X(0002).
      *    -------------------------------
           05  SQ11FL PIC S9(0004) COMP.
           05  SQ11FF PIC  X(0001).
           05  FILLER REDEFINES SQ11FF.
               10  SQ11FA PIC  X(0001).
           05  SQ11FI PIC  X(0001).
      *    -------------------------------
           05  L12FL PIC S9(0004) COMP.
           05  L12FF PIC  X(0001).
           05  FILLER REDEFINES L12FF.
               10  L12FA PIC  X(0001).
           05  L12FI PIC  X(0003).
      *    -------------------------------
           05  TEXT12FL PIC S9(0004) COMP.
           05  TEXT12FF PIC  X(0001).
           05  FILLER REDEFINES TEXT12FF.
               10  TEXT12FA PIC  X(0001).
           05  TEXT12FI PIC  X(0070).
      *    -------------------------------
           05  CF12FL PIC S9(0004) COMP.
           05  CF12FF PIC  X(0001).
           05  FILLER REDEFINES CF12FF.
               10  CF12FA PIC  X(0001).
           05  CF12FI PIC  X(0002).
      *    -------------------------------
           05  SQ12FL PIC S9(0004) COMP.
           05  SQ12FF PIC  X(0001).
           05  FILLER REDEFINES SQ12FF.
               10  SQ12FA PIC  X(0001).
           05  SQ12FI PIC  X(0001).
      *    -------------------------------
           05  L13FL PIC S9(0004) COMP.
           05  L13FF PIC  X(0001).
           05  FILLER REDEFINES L13FF.
               10  L13FA PIC  X(0001).
           05  L13FI PIC  X(0003).
      *    -------------------------------
           05  TEXT13FL PIC S9(0004) COMP.
           05  TEXT13FF PIC  X(0001).
           05  FILLER REDEFINES TEXT13FF.
               10  TEXT13FA PIC  X(0001).
           05  TEXT13FI PIC  X(0070).
      *    -------------------------------
           05  CF13FL PIC S9(0004) COMP.
           05  CF13FF PIC  X(0001).
           05  FILLER REDEFINES CF13FF.
               10  CF13FA PIC  X(0001).
           05  CF13FI PIC  X(0002).
      *    -------------------------------
           05  SQ13FL PIC S9(0004) COMP.
           05  SQ13FF PIC  X(0001).
           05  FILLER REDEFINES SQ13FF.
               10  SQ13FA PIC  X(0001).
           05  SQ13FI PIC  X(0001).
      *    -------------------------------
           05  L14FL PIC S9(0004) COMP.
           05  L14FF PIC  X(0001).
           05  FILLER REDEFINES L14FF.
               10  L14FA PIC  X(0001).
           05  L14FI PIC  X(0003).
      *    -------------------------------
           05  TEXT14FL PIC S9(0004) COMP.
           05  TEXT14FF PIC  X(0001).
           05  FILLER REDEFINES TEXT14FF.
               10  TEXT14FA PIC  X(0001).
           05  TEXT14FI PIC  X(0070).
      *    -------------------------------
           05  CF14FL PIC S9(0004) COMP.
           05  CF14FF PIC  X(0001).
           05  FILLER REDEFINES CF14FF.
               10  CF14FA PIC  X(0001).
           05  CF14FI PIC  X(0002).
      *    -------------------------------
           05  SQ14FL PIC S9(0004) COMP.
           05  SQ14FF PIC  X(0001).
           05  FILLER REDEFINES SQ14FF.
               10  SQ14FA PIC  X(0001).
           05  SQ14FI PIC  X(0001).
      *    -------------------------------
           05  L15FL PIC S9(0004) COMP.
           05  L15FF PIC  X(0001).
           05  FILLER REDEFINES L15FF.
               10  L15FA PIC  X(0001).
           05  L15FI PIC  X(0003).
      *    -------------------------------
           05  TEXT15FL PIC S9(0004) COMP.
           05  TEXT15FF PIC  X(0001).
           05  FILLER REDEFINES TEXT15FF.
               10  TEXT15FA PIC  X(0001).
           05  TEXT15FI PIC  X(0070).
      *    -------------------------------
           05  CF15FL PIC S9(0004) COMP.
           05  CF15FF PIC  X(0001).
           05  FILLER REDEFINES CF15FF.
               10  CF15FA PIC  X(0001).
           05  CF15FI PIC  X(0002).
      *    -------------------------------
           05  SQ15FL PIC S9(0004) COMP.
           05  SQ15FF PIC  X(0001).
           05  FILLER REDEFINES SQ15FF.
               10  SQ15FA PIC  X(0001).
           05  SQ15FI PIC  X(0001).
      *    -------------------------------
           05  ERMSGBFL PIC S9(0004) COMP.
           05  ERMSGBFF PIC  X(0001).
           05  FILLER REDEFINES ERMSGBFF.
               10  ERMSGBFA PIC  X(0001).
           05  ERMSGBFI PIC  X(0072).
      *    -------------------------------
           05  FUNCTFL PIC S9(0004) COMP.
           05  FUNCTFF PIC  X(0001).
           05  FILLER REDEFINES FUNCTFF.
               10  FUNCTFA PIC  X(0001).
           05  FUNCTFI PIC  X(0001).
      *    -------------------------------
           05  LINE1FL PIC S9(0004) COMP.
           05  LINE1FF PIC  X(0001).
           05  FILLER REDEFINES LINE1FF.
               10  LINE1FA PIC  X(0001).
           05  LINE1FI PIC  999.
      *    -------------------------------
           05  LINE2FL PIC S9(0004) COMP.
           05  LINE2FF PIC  X(0001).
           05  FILLER REDEFINES LINE2FF.
               10  LINE2FA PIC  X(0001).
           05  LINE2FI PIC  999.
      *    -------------------------------
           05  PFBFL PIC S9(0004) COMP.
           05  PFBFF PIC  X(0001).
           05  FILLER REDEFINES PFBFF.
               10  PFBFA PIC  X(0001).
           05  PFBFI PIC  99.
      *    -------------------------------
           05  MNTBYFL PIC S9(0004) COMP.
           05  MNTBYFF PIC  X(0001).
           05  FILLER REDEFINES MNTBYFF.
               10  MNTBYFA PIC  X(0001).
           05  MNTBYFI PIC  X(0005).
      *    -------------------------------
           05  MNTONFL PIC S9(0004) COMP.
           05  MNTONFF PIC  X(0001).
           05  FILLER REDEFINES MNTONFF.
               10  MNTONFA PIC  X(0001).
           05  MNTONFI PIC  X(0008).
       01  EL104BFO REDEFINES EL104BFI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEBFO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEBFO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPFO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TXTTPBFO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNTRLBFO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCHTBFO PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCHBFO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRMSTBFO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRMSQBFO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOTFO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LNESQTFO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L1FO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT1FO PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF1FO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ1FO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L2FO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT2FO PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF2FO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ2FO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L3FO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT3FO PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF3FO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ3FO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L4FO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT4FO PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF4FO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ4FO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L5FO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT5FO PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF5FO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ5FO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L6FO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT6FO PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF6FO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ6FO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L7FO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT7FO PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF7FO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ7FO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L8FO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT8FO PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF8FO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ8FO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L9FO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT9FO PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF9FO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ9FO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L10FO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT10FO PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF10FO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ10FO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L11FO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT11FO PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF11FO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ11FO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L12FO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT12FO PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF12FO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ12FO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L13FO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT13FO PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF13FO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ13FO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L14FO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT14FO PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF14FO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ14FO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L15FO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT15FO PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF15FO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ15FO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERMSGBFO PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FUNCTFO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE1FO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE2FO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFBFO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MNTBYFO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MNTONFO PIC  X(0008).
      *    -------------------------------
       01  EL104BI REDEFINES EL104BFI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  DATEBL PIC S9(0004) COMP.
           05  DATEBF PIC  X(0001).
           05  FILLER REDEFINES DATEBF.
               10  DATEBA PIC  X(0001).
           05  DATEBI PIC  X(0008).
      *    -------------------------------
           05  TIMEBL PIC S9(0004) COMP.
           05  TIMEBF PIC  X(0001).
           05  FILLER REDEFINES TIMEBF.
               10  TIMEBA PIC  X(0001).
           05  TIMEBI PIC  X(0005).
      *    -------------------------------
           05  COMPANYL PIC S9(0004) COMP.
           05  COMPANYF PIC  X(0001).
           05  FILLER REDEFINES COMPANYF.
               10  COMPANYA PIC  X(0001).
           05  COMPANYI PIC  X(0003).
      *    -------------------------------
           05  TEXTTPBL PIC S9(0004) COMP.
           05  TEXTTPBF PIC  X(0001).
           05  FILLER REDEFINES TEXTTPBF.
               10  TEXTTPBA PIC  X(0001).
           05  TEXTTPBI PIC  X(0008).
      *    -------------------------------
           05  CONTRLBL PIC S9(0004) COMP.
           05  CONTRLBF PIC  X(0001).
           05  FILLER REDEFINES CONTRLBF.
               10  CONTRLBA PIC  X(0001).
           05  CONTRLBI PIC  X(0012).
      *    -------------------------------
           05  ARCHTBL PIC S9(0004) COMP.
           05  ARCHTBF PIC  X(0001).
           05  FILLER REDEFINES ARCHTBF.
               10  ARCHTBA PIC  X(0001).
           05  ARCHTBI PIC  X(0009).
      *    -------------------------------
           05  ARCHBL PIC S9(0004) COMP.
           05  ARCHBF PIC  X(0001).
           05  FILLER REDEFINES ARCHBF.
               10  ARCHBA PIC  X(0001).
           05  ARCHBI PIC  X(0001).
      *    -------------------------------
           05  FORMSTBL PIC S9(0004) COMP.
           05  FORMSTBF PIC  X(0001).
           05  FILLER REDEFINES FORMSTBF.
               10  FORMSTBA PIC  X(0001).
           05  FORMSTBI PIC  X(0012).
      *    -------------------------------
           05  FORMSQBL PIC S9(0004) COMP.
           05  FORMSQBF PIC  X(0001).
           05  FILLER REDEFINES FORMSQBF.
               10  FORMSQBA PIC  X(0001).
           05  FORMSQBI PIC  X(0001).
      *    -------------------------------
           05  TOTL PIC S9(0004) COMP.
           05  TOTF PIC  X(0001).
           05  FILLER REDEFINES TOTF.
               10  TOTA PIC  X(0001).
           05  TOTI PIC  999.
      *    -------------------------------
           05  LINESQTL PIC S9(0004) COMP.
           05  LINESQTF PIC  X(0001).
           05  FILLER REDEFINES LINESQTF.
               10  LINESQTA PIC  X(0001).
           05  LINESQTI PIC  X(0001).
      *    -------------------------------
           05  L1L PIC S9(0004) COMP.
           05  L1F PIC  X(0001).
           05  FILLER REDEFINES L1F.
               10  L1A PIC  X(0001).
           05  L1I PIC  X(0003).
      *    -------------------------------
           05  TEXT1L PIC S9(0004) COMP.
           05  TEXT1F PIC  X(0001).
           05  FILLER REDEFINES TEXT1F.
               10  TEXT1A PIC  X(0001).
           05  TEXT1I PIC  X(0070).
      *    -------------------------------
           05  CF1L PIC S9(0004) COMP.
           05  CF1F PIC  X(0001).
           05  FILLER REDEFINES CF1F.
               10  CF1A PIC  X(0001).
           05  CF1I PIC  X(0002).
      *    -------------------------------
           05  SQ1L PIC S9(0004) COMP.
           05  SQ1F PIC  X(0001).
           05  FILLER REDEFINES SQ1F.
               10  SQ1A PIC  X(0001).
           05  SQ1I PIC  X(0001).
      *    -------------------------------
           05  L2L PIC S9(0004) COMP.
           05  L2F PIC  X(0001).
           05  FILLER REDEFINES L2F.
               10  L2A PIC  X(0001).
           05  L2I PIC  X(0003).
      *    -------------------------------
           05  TEXT2L PIC S9(0004) COMP.
           05  TEXT2F PIC  X(0001).
           05  FILLER REDEFINES TEXT2F.
               10  TEXT2A PIC  X(0001).
           05  TEXT2I PIC  X(0070).
      *    -------------------------------
           05  CF2L PIC S9(0004) COMP.
           05  CF2F PIC  X(0001).
           05  FILLER REDEFINES CF2F.
               10  CF2A PIC  X(0001).
           05  CF2I PIC  X(0002).
      *    -------------------------------
           05  SQ2L PIC S9(0004) COMP.
           05  SQ2F PIC  X(0001).
           05  FILLER REDEFINES SQ2F.
               10  SQ2A PIC  X(0001).
           05  SQ2I PIC  X(0001).
      *    -------------------------------
           05  L3L PIC S9(0004) COMP.
           05  L3F PIC  X(0001).
           05  FILLER REDEFINES L3F.
               10  L3A PIC  X(0001).
           05  L3I PIC  X(0003).
      *    -------------------------------
           05  TEXT3L PIC S9(0004) COMP.
           05  TEXT3F PIC  X(0001).
           05  FILLER REDEFINES TEXT3F.
               10  TEXT3A PIC  X(0001).
           05  TEXT3I PIC  X(0070).
      *    -------------------------------
           05  CF3L PIC S9(0004) COMP.
           05  CF3F PIC  X(0001).
           05  FILLER REDEFINES CF3F.
               10  CF3A PIC  X(0001).
           05  CF3I PIC  X(0002).
      *    -------------------------------
           05  SQ3L PIC S9(0004) COMP.
           05  SQ3F PIC  X(0001).
           05  FILLER REDEFINES SQ3F.
               10  SQ3A PIC  X(0001).
           05  SQ3I PIC  X(0001).
      *    -------------------------------
           05  L4L PIC S9(0004) COMP.
           05  L4F PIC  X(0001).
           05  FILLER REDEFINES L4F.
               10  L4A PIC  X(0001).
           05  L4I PIC  X(0003).
      *    -------------------------------
           05  TEXT4L PIC S9(0004) COMP.
           05  TEXT4F PIC  X(0001).
           05  FILLER REDEFINES TEXT4F.
               10  TEXT4A PIC  X(0001).
           05  TEXT4I PIC  X(0070).
      *    -------------------------------
           05  CF4L PIC S9(0004) COMP.
           05  CF4F PIC  X(0001).
           05  FILLER REDEFINES CF4F.
               10  CF4A PIC  X(0001).
           05  CF4I PIC  X(0002).
      *    -------------------------------
           05  SQ4L PIC S9(0004) COMP.
           05  SQ4F PIC  X(0001).
           05  FILLER REDEFINES SQ4F.
               10  SQ4A PIC  X(0001).
           05  SQ4I PIC  X(0001).
      *    -------------------------------
           05  L5L PIC S9(0004) COMP.
           05  L5F PIC  X(0001).
           05  FILLER REDEFINES L5F.
               10  L5A PIC  X(0001).
           05  L5I PIC  X(0003).
      *    -------------------------------
           05  TEXT5L PIC S9(0004) COMP.
           05  TEXT5F PIC  X(0001).
           05  FILLER REDEFINES TEXT5F.
               10  TEXT5A PIC  X(0001).
           05  TEXT5I PIC  X(0070).
      *    -------------------------------
           05  CF5L PIC S9(0004) COMP.
           05  CF5F PIC  X(0001).
           05  FILLER REDEFINES CF5F.
               10  CF5A PIC  X(0001).
           05  CF5I PIC  X(0002).
      *    -------------------------------
           05  SQ5L PIC S9(0004) COMP.
           05  SQ5F PIC  X(0001).
           05  FILLER REDEFINES SQ5F.
               10  SQ5A PIC  X(0001).
           05  SQ5I PIC  X(0001).
      *    -------------------------------
           05  L6L PIC S9(0004) COMP.
           05  L6F PIC  X(0001).
           05  FILLER REDEFINES L6F.
               10  L6A PIC  X(0001).
           05  L6I PIC  X(0003).
      *    -------------------------------
           05  TEXT6L PIC S9(0004) COMP.
           05  TEXT6F PIC  X(0001).
           05  FILLER REDEFINES TEXT6F.
               10  TEXT6A PIC  X(0001).
           05  TEXT6I PIC  X(0070).
      *    -------------------------------
           05  CF6L PIC S9(0004) COMP.
           05  CF6F PIC  X(0001).
           05  FILLER REDEFINES CF6F.
               10  CF6A PIC  X(0001).
           05  CF6I PIC  X(0002).
      *    -------------------------------
           05  SQ6L PIC S9(0004) COMP.
           05  SQ6F PIC  X(0001).
           05  FILLER REDEFINES SQ6F.
               10  SQ6A PIC  X(0001).
           05  SQ6I PIC  X(0001).
      *    -------------------------------
           05  L7L PIC S9(0004) COMP.
           05  L7F PIC  X(0001).
           05  FILLER REDEFINES L7F.
               10  L7A PIC  X(0001).
           05  L7I PIC  X(0003).
      *    -------------------------------
           05  TEXT7L PIC S9(0004) COMP.
           05  TEXT7F PIC  X(0001).
           05  FILLER REDEFINES TEXT7F.
               10  TEXT7A PIC  X(0001).
           05  TEXT7I PIC  X(0070).
      *    -------------------------------
           05  CF7L PIC S9(0004) COMP.
           05  CF7F PIC  X(0001).
           05  FILLER REDEFINES CF7F.
               10  CF7A PIC  X(0001).
           05  CF7I PIC  X(0002).
      *    -------------------------------
           05  SQ7L PIC S9(0004) COMP.
           05  SQ7F PIC  X(0001).
           05  FILLER REDEFINES SQ7F.
               10  SQ7A PIC  X(0001).
           05  SQ7I PIC  X(0001).
      *    -------------------------------
           05  L8L PIC S9(0004) COMP.
           05  L8F PIC  X(0001).
           05  FILLER REDEFINES L8F.
               10  L8A PIC  X(0001).
           05  L8I PIC  X(0003).
      *    -------------------------------
           05  TEXT8L PIC S9(0004) COMP.
           05  TEXT8F PIC  X(0001).
           05  FILLER REDEFINES TEXT8F.
               10  TEXT8A PIC  X(0001).
           05  TEXT8I PIC  X(0070).
      *    -------------------------------
           05  CF8L PIC S9(0004) COMP.
           05  CF8F PIC  X(0001).
           05  FILLER REDEFINES CF8F.
               10  CF8A PIC  X(0001).
           05  CF8I PIC  X(0002).
      *    -------------------------------
           05  SQ8L PIC S9(0004) COMP.
           05  SQ8F PIC  X(0001).
           05  FILLER REDEFINES SQ8F.
               10  SQ8A PIC  X(0001).
           05  SQ8I PIC  X(0001).
      *    -------------------------------
           05  L9L PIC S9(0004) COMP.
           05  L9F PIC  X(0001).
           05  FILLER REDEFINES L9F.
               10  L9A PIC  X(0001).
           05  L9I PIC  X(0003).
      *    -------------------------------
           05  TEXT9L PIC S9(0004) COMP.
           05  TEXT9F PIC  X(0001).
           05  FILLER REDEFINES TEXT9F.
               10  TEXT9A PIC  X(0001).
           05  TEXT9I PIC  X(0070).
      *    -------------------------------
           05  CF9L PIC S9(0004) COMP.
           05  CF9F PIC  X(0001).
           05  FILLER REDEFINES CF9F.
               10  CF9A PIC  X(0001).
           05  CF9I PIC  X(0002).
      *    -------------------------------
           05  SQ9L PIC S9(0004) COMP.
           05  SQ9F PIC  X(0001).
           05  FILLER REDEFINES SQ9F.
               10  SQ9A PIC  X(0001).
           05  SQ9I PIC  X(0001).
      *    -------------------------------
           05  L10L PIC S9(0004) COMP.
           05  L10F PIC  X(0001).
           05  FILLER REDEFINES L10F.
               10  L10A PIC  X(0001).
           05  L10I PIC  X(0003).
      *    -------------------------------
           05  TEXT10L PIC S9(0004) COMP.
           05  TEXT10F PIC  X(0001).
           05  FILLER REDEFINES TEXT10F.
               10  TEXT10A PIC  X(0001).
           05  TEXT10I PIC  X(0070).
      *    -------------------------------
           05  CF10L PIC S9(0004) COMP.
           05  CF10F PIC  X(0001).
           05  FILLER REDEFINES CF10F.
               10  CF10A PIC  X(0001).
           05  CF10I PIC  X(0002).
      *    -------------------------------
           05  SQ10L PIC S9(0004) COMP.
           05  SQ10F PIC  X(0001).
           05  FILLER REDEFINES SQ10F.
               10  SQ10A PIC  X(0001).
           05  SQ10I PIC  X(0001).
      *    -------------------------------
           05  L11L PIC S9(0004) COMP.
           05  L11F PIC  X(0001).
           05  FILLER REDEFINES L11F.
               10  L11A PIC  X(0001).
           05  L11I PIC  X(0003).
      *    -------------------------------
           05  TEXT11L PIC S9(0004) COMP.
           05  TEXT11F PIC  X(0001).
           05  FILLER REDEFINES TEXT11F.
               10  TEXT11A PIC  X(0001).
           05  TEXT11I PIC  X(0070).
      *    -------------------------------
           05  CF11L PIC S9(0004) COMP.
           05  CF11F PIC  X(0001).
           05  FILLER REDEFINES CF11F.
               10  CF11A PIC  X(0001).
           05  CF11I PIC  X(0002).
      *    -------------------------------
           05  SQ11L PIC S9(0004) COMP.
           05  SQ11F PIC  X(0001).
           05  FILLER REDEFINES SQ11F.
               10  SQ11A PIC  X(0001).
           05  SQ11I PIC  X(0001).
      *    -------------------------------
           05  L12L PIC S9(0004) COMP.
           05  L12F PIC  X(0001).
           05  FILLER REDEFINES L12F.
               10  L12A PIC  X(0001).
           05  L12I PIC  X(0003).
      *    -------------------------------
           05  TEXT12L PIC S9(0004) COMP.
           05  TEXT12F PIC  X(0001).
           05  FILLER REDEFINES TEXT12F.
               10  TEXT12A PIC  X(0001).
           05  TEXT12I PIC  X(0070).
      *    -------------------------------
           05  CF12L PIC S9(0004) COMP.
           05  CF12F PIC  X(0001).
           05  FILLER REDEFINES CF12F.
               10  CF12A PIC  X(0001).
           05  CF12I PIC  X(0002).
      *    -------------------------------
           05  SQ12L PIC S9(0004) COMP.
           05  SQ12F PIC  X(0001).
           05  FILLER REDEFINES SQ12F.
               10  SQ12A PIC  X(0001).
           05  SQ12I PIC  X(0001).
      *    -------------------------------
           05  L13L PIC S9(0004) COMP.
           05  L13F PIC  X(0001).
           05  FILLER REDEFINES L13F.
               10  L13A PIC  X(0001).
           05  L13I PIC  X(0003).
      *    -------------------------------
           05  TEXT13L PIC S9(0004) COMP.
           05  TEXT13F PIC  X(0001).
           05  FILLER REDEFINES TEXT13F.
               10  TEXT13A PIC  X(0001).
           05  TEXT13I PIC  X(0070).
      *    -------------------------------
           05  CF13L PIC S9(0004) COMP.
           05  CF13F PIC  X(0001).
           05  FILLER REDEFINES CF13F.
               10  CF13A PIC  X(0001).
           05  CF13I PIC  X(0002).
      *    -------------------------------
           05  SQ13L PIC S9(0004) COMP.
           05  SQ13F PIC  X(0001).
           05  FILLER REDEFINES SQ13F.
               10  SQ13A PIC  X(0001).
           05  SQ13I PIC  X(0001).
      *    -------------------------------
           05  L14L PIC S9(0004) COMP.
           05  L14F PIC  X(0001).
           05  FILLER REDEFINES L14F.
               10  L14A PIC  X(0001).
           05  L14I PIC  X(0003).
      *    -------------------------------
           05  TEXT14L PIC S9(0004) COMP.
           05  TEXT14F PIC  X(0001).
           05  FILLER REDEFINES TEXT14F.
               10  TEXT14A PIC  X(0001).
           05  TEXT14I PIC  X(0070).
      *    -------------------------------
           05  CF14L PIC S9(0004) COMP.
           05  CF14F PIC  X(0001).
           05  FILLER REDEFINES CF14F.
               10  CF14A PIC  X(0001).
           05  CF14I PIC  X(0002).
      *    -------------------------------
           05  SQ14L PIC S9(0004) COMP.
           05  SQ14F PIC  X(0001).
           05  FILLER REDEFINES SQ14F.
               10  SQ14A PIC  X(0001).
           05  SQ14I PIC  X(0001).
      *    -------------------------------
           05  L15L PIC S9(0004) COMP.
           05  L15F PIC  X(0001).
           05  FILLER REDEFINES L15F.
               10  L15A PIC  X(0001).
           05  L15I PIC  X(0003).
      *    -------------------------------
           05  TEXT15L PIC S9(0004) COMP.
           05  TEXT15F PIC  X(0001).
           05  FILLER REDEFINES TEXT15F.
               10  TEXT15A PIC  X(0001).
           05  TEXT15I PIC  X(0070).
      *    -------------------------------
           05  CF15L PIC S9(0004) COMP.
           05  CF15F PIC  X(0001).
           05  FILLER REDEFINES CF15F.
               10  CF15A PIC  X(0001).
           05  CF15I PIC  X(0002).
      *    -------------------------------
           05  SQ15L PIC S9(0004) COMP.
           05  SQ15F PIC  X(0001).
           05  FILLER REDEFINES SQ15F.
               10  SQ15A PIC  X(0001).
           05  SQ15I PIC  X(0001).
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
           05  PFENTRBL PIC S9(0004) COMP.
           05  PFENTRBF PIC  X(0001).
           05  FILLER REDEFINES PFENTRBF.
               10  PFENTRBA PIC  X(0001).
           05  PFENTRBI PIC  99.
      *    -------------------------------
           05  MNTBYL PIC S9(0004) COMP.
           05  MNTBYF PIC  X(0001).
           05  FILLER REDEFINES MNTBYF.
               10  MNTBYA PIC  X(0001).
           05  MNTBYI PIC  X(0004).
      *    -------------------------------
           05  MNTONL PIC S9(0004) COMP.
           05  MNTONF PIC  X(0001).
           05  FILLER REDEFINES MNTONF.
               10  MNTONA PIC  X(0001).
           05  MNTONI PIC  X(0008).
       01  EL104BO REDEFINES EL104BFI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEBO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEBO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPANYO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXTTPBO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CONTRLBO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCHTBO PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCHBO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORMSTBO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORMSQBO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOTO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINESQTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT1O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT2O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L3O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT3O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L4O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT4O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L5O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT5O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF5O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L6O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT6O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF6O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L7O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT7O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF7O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L8O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT8O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF8O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L9O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT9O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF9O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L10O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT10O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF10O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L11O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT11O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF11O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L12O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT12O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF12O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L13O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT13O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF13O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L14O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT14O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF14O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L15O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT15O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CF15O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SQ15O PIC  X(0001).
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
           05  PFENTRBO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MNTBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MNTONO PIC  X(0008).
      *    -------------------------------
       01  EL104AI REDEFINES EL104BFI.
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
           05  COMPAL PIC S9(0004) COMP.
           05  COMPAF PIC  X(0001).
           05  FILLER REDEFINES COMPAF.
               10  COMPAA PIC  X(0001).
           05  COMPAI PIC  X(0003).
      *    -------------------------------
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  TRANSFML PIC S9(0004) COMP.
           05  TRANSFMF PIC  X(0001).
           05  FILLER REDEFINES TRANSFMF.
               10  TRANSFMA PIC  X(0001).
           05  TRANSFMI PIC  X(0014).
      *    -------------------------------
           05  FILETYPL PIC S9(0004) COMP.
           05  FILETYPF PIC  X(0001).
           05  FILLER REDEFINES FILETYPF.
               10  FILETYPA PIC  X(0001).
           05  FILETYPI PIC  X(0001).
      *    -------------------------------
           05  CONTROLL PIC S9(0004) COMP.
           05  CONTROLF PIC  X(0001).
           05  FILLER REDEFINES CONTROLF.
               10  CONTROLA PIC  X(0001).
           05  CONTROLI PIC  X(0012).
      *    -------------------------------
           05  NEWCONTL PIC S9(0004) COMP.
           05  NEWCONTF PIC  X(0001).
           05  FILLER REDEFINES NEWCONTF.
               10  NEWCONTA PIC  X(0001).
           05  NEWCONTI PIC  X(0012).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0079).
      *    -------------------------------
           05  ERRMSG2L PIC S9(0004) COMP.
           05  ERRMSG2F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG2F.
               10  ERRMSG2A PIC  X(0001).
           05  ERRMSG2I PIC  X(0079).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  99.
      *    -------------------------------
           05  HDNPFAL PIC S9(0004) COMP.
           05  HDNPFAF PIC  X(0001).
           05  FILLER REDEFINES HDNPFAF.
               10  HDNPFAA PIC  X(0001).
           05  HDNPFAI PIC  X(0019).
       01  EL104AO REDEFINES EL104BFI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPAO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRANSFMO PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FILETYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CONTROLO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NEWCONTO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HDNPFAO PIC  X(0019).
      *    -------------------------------
       01  EL104AFI REDEFINES EL104BFI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  DATEAFL PIC S9(0004) COMP.
           05  DATEAFF PIC  X(0001).
           05  FILLER REDEFINES DATEAFF.
               10  DATEAFA PIC  X(0001).
           05  DATEAFI PIC  X(0008).
      *    -------------------------------
           05  TIMEAFL PIC S9(0004) COMP.
           05  TIMEAFF PIC  X(0001).
           05  FILLER REDEFINES TIMEAFF.
               10  TIMEAFA PIC  X(0001).
           05  TIMEAFI PIC  X(0005).
      *    -------------------------------
           05  COMPAFL PIC S9(0004) COMP.
           05  COMPAFF PIC  X(0001).
           05  FILLER REDEFINES COMPAFF.
               10  COMPAFA PIC  X(0001).
           05  COMPAFI PIC  X(0003).
      *    -------------------------------
           05  MAINTFL PIC S9(0004) COMP.
           05  MAINTFF PIC  X(0001).
           05  FILLER REDEFINES MAINTFF.
               10  MAINTFA PIC  X(0001).
           05  MAINTFI PIC  X(0001).
      *    -------------------------------
           05  TRNSFMFL PIC S9(0004) COMP.
           05  TRNSFMFF PIC  X(0001).
           05  FILLER REDEFINES TRNSFMFF.
               10  TRNSFMFA PIC  X(0001).
           05  TRNSFMFI PIC  X(0014).
      *    -------------------------------
           05  FILETPFL PIC S9(0004) COMP.
           05  FILETPFF PIC  X(0001).
           05  FILLER REDEFINES FILETPFF.
               10  FILETPFA PIC  X(0001).
           05  FILETPFI PIC  X(0001).
      *    -------------------------------
           05  CONTRLFL PIC S9(0004) COMP.
           05  CONTRLFF PIC  X(0001).
           05  FILLER REDEFINES CONTRLFF.
               10  CONTRLFA PIC  X(0001).
           05  CONTRLFI PIC  X(0012).
      *    -------------------------------
           05  NEWCNTFL PIC S9(0004) COMP.
           05  NEWCNTFF PIC  X(0001).
           05  FILLER REDEFINES NEWCNTFF.
               10  NEWCNTFA PIC  X(0001).
           05  NEWCNTFI PIC  X(0012).
      *    -------------------------------
           05  ERMSG1FL PIC S9(0004) COMP.
           05  ERMSG1FF PIC  X(0001).
           05  FILLER REDEFINES ERMSG1FF.
               10  ERMSG1FA PIC  X(0001).
           05  ERMSG1FI PIC  X(0079).
      *    -------------------------------
           05  ERMSG2FL PIC S9(0004) COMP.
           05  ERMSG2FF PIC  X(0001).
           05  FILLER REDEFINES ERMSG2FF.
               10  ERMSG2FA PIC  X(0001).
           05  ERMSG2FI PIC  X(0079).
      *    -------------------------------
           05  PFENTFL PIC S9(0004) COMP.
           05  PFENTFF PIC  X(0001).
           05  FILLER REDEFINES PFENTFF.
               10  PFENTFA PIC  X(0001).
           05  PFENTFI PIC  99.
      *    -------------------------------
           05  HDNPFAFL PIC S9(0004) COMP.
           05  HDNPFAFF PIC  X(0001).
           05  FILLER REDEFINES HDNPFAFF.
               10  HDNPFAFA PIC  X(0001).
           05  HDNPFAFI PIC  X(0022).
       01  EL104AFO REDEFINES EL104BFI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEAFO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEAFO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPAFO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTFO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRNSFMFO PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FILETPFO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CONTRLFO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NEWCNTFO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERMSG1FO PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERMSG2FO PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTFO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HDNPFAFO PIC  X(0022).
      *    -------------------------------
00189  EJECT
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
00191  01  DFHCOMMAREA                 PIC X(1024).
00192 *01 PARMLIST .
00193 *    12  FILLER                  PIC S9(8)  COMP.
00194 *    12  TXT-ADDR                PIC S9(8)  COMP.
00195  EJECT
00196 *    COPY ELCTEXT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCTEXT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.008                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = TEXT FILES FOR HELP DISPLAY,              *
00008 *                                     FORM LETTERS,              *
00009 *                                     CERT FORM DISPLAY.
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 100   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ELLETR (LETTERS)   RKP=2,LEN=15          *
00015 *       ALTERNATE INDEX = NONE                                   *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ELFORM (FORMS)     RKP=2,LEN=15          *
00018 *       ALTERNATE INDEX = NONE                                   *
00019 *                                                                *
00020 *   BASE CLUSTER NAME = ELHELP (HELP)      RKP=2,LEN=15          *
00021 *       ALTERNATE INDEX = NONE                                   *
00022 *                                                                *
00023 *   LOG = NO                                                     *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00025 ******************************************************************
00026  01  TEXT-FILES.
00027      12  TEXT-FILE-ID                PIC XX.
00028          88  FORMS-FILE-TEXT            VALUE 'TF'.
00029          88  LETTER-FILE-TEXT           VALUE 'TL'.
00030          88  HELP-FILE-TEXT             VALUE 'TH'.
00031
00032      12  TX-CONTROL-PRIMARY.
00033          16  TX-COMPANY-CD           PIC X.
00034              88  TX-SYSTEM-WIDE-FILE    VALUE LOW-VALUE.
00035          16  TX-ACCESS-CD-GENL       PIC X(12).
00036
00037          16  TX-LETTER-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00038              20  TX-LETTER-NO        PIC X(4).
00039              20  FILLER              PIC X(8).
00040
00041          16  TX-FORM-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00042              20  TX-FORM-NO          PIC X(12).
00043
00044          16  TX-HELP-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00045              20  TX-HELP-TYPE        PIC X.
00046                  88  HELP-FOR-GENERAL   VALUE ' '.
00047                  88  HELP-BY-SCREEN     VALUE 'S'.
00048                  88  HELP-BY-ERROR      VALUE 'E'.
00049              20  TX-SCREEN-OR-ERROR  PIC X(4).
00050                  88  GENERAL-INFO-HELP  VALUE '0000'.
00051              20  TX-HELP-FOR-COMPANY  PIC XXX.
00052                  88  NOT-COMPANY-SPECIFIC VALUE '   '.
00053              20  FILLER              PIC X(4).
00054
00055          16  TX-LINE-SEQUENCE        PIC S9(4)     COMP.
00056
00057      12  TX-PROCESS-CONTROL          PIC XX.
00058          88  LETTER-LINE-SKIPS          VALUE '01' THRU '99'.
00059
00060      12  TX-TEXT-LINE                PIC X(70).
00061
00062      12  TX-FORM-SQUEEZE-CONTROL     PIC X.
00063          88  TX-FORM-SQUEEZE-ON         VALUE 'Y'.
00064          88  TX-FORM-SQUEEZE-OFF        VALUE SPACES.
00065          88  TX-VALID-FORM-SQUEEZE-VALUE
00066                                         VALUE 'Y' ' '.
00067
00068      12  TX-LINE-SQUEEZE-CONTROL     PIC X.
00069          88  TX-ADJUST-TO-LINE-LENGTH   VALUE 'A'.
00070          88  TX-CONTINUE-PARAGRAPH      VALUE 'C'.
00071          88  TX-DO-NOT-ADJUST           VALUE 'N'.
00072          88  TX-FORM-CONTROL-LINE       VALUE 'K'.
00073          88  TX-NEW-PARAGRAPH           VALUE 'P'.
00074          88  TX-NO-SPECIAL-INSTRUCTION  VALUE ' '.
00075          88  TX-VALID-LINE-SQ-VALUE     VALUE 'A' 'C' 'P'
00076                                               'K' 'N' ' '
00077                                               'Z'.
00078
00079      12  TX-ARCHIVE-SW               PIC X.
00080          88  TX-ARCHIVE-THIS-LETTER     VALUE 'Y'.
00081          88  TX-DO-NOT-ARCHIVE          VALUE SPACES.
00082          88  TX-VALID-ARCHIVE-VALUE     VALUE 'Y' ' '.
00083
00084      12  TX-LAST-MAINTENANCED-BY     PIC X(4).
00085      12  TX-LAST-MAINTENANCED-DT     PIC X(2).
00086
00087      12  TX-BSR-CODE                 PIC X.
00088          88  TX-BSR-LETTER              VALUE 'B'.
00089          88  TX-NON-BSR-LETTER          VALUE ' '.
00090
00091      12  FILLER                      PIC X.
00092 *****************************************************************
00197  EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA TEXT-FILES.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL104' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00199
00200      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.
00201
00202      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00203      MOVE '5'                   TO DC-OPTION-CODE.
00204      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00205      MOVE DC-GREG-DATE-1-EDIT   TO SAVE-DATE.
00206      MOVE DC-BIN-DATE-1         TO SAVE-BIN-DATE.
00207
00208      MOVE 2 TO EMI-NUMBER-OF-LINES.
00209
00210      IF  PI-LANGUAGE-IS-FR
00211          MOVE 'EL104AF'         TO MAP-NAME.
00212
00213      IF EIBCALEN = ZEROS
00214          GO TO 8800-UNAUTHORIZED-ACCESS.
00215
00216      IF  PI-CALLING-PROGRAM NOT = THIS-PGM
00217          IF  PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00218              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00219              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00220              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00221              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00222              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00223              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00224              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00225              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00226              MOVE LOW-VALUES           TO EL104AI
00227              MOVE 'N'                  TO PI-104-SCREEN-SENT-IND
00228
00229              IF  PI-104-SCREEN-BYPASS
00230                      NOT EQUAL LOW-VALUES
00231                      AND
00232                  PI-104-SCREEN-BYPASS
00233                      NOT EQUAL SPACES
00234                  MOVE PI-104-TYPE      TO FILETYPI
00235                  MOVE +1               TO FILETYPL
00236                  MOVE PI-104-NAME      TO CONTROLI
00237                  MOVE +12              TO CONTROLL
00238                  MOVE +1               TO MAINTL
00239                  MOVE QUOTE            TO EIBAID
00240
00241                  IF  PI-LANGUAGE-IS-FR
00242                      MOVE 'M'          TO MAINTI
00243                      GO TO 2001-CHECK-PFKEYS
00244
00245                  ELSE
00246                      MOVE 'S'          TO MAINTI
00247                      GO TO 2001-CHECK-PFKEYS
00248
00249              ELSE
00250                  GO TO 8100-SEND-INITIAL-MAP
00251
00252        ELSE
00253            MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00254            MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00255            MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00256            MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00257            MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00258            MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00259            MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00260            MOVE SPACES               TO PI-SAVED-PROGRAM-6
00261            MOVE LOW-VALUES TO EL104AI
00262            MOVE PI-LAST-CONTROL TO CONTROLO
00263            MOVE PI-FILETYP      TO FILETYPO
00264            MOVE +1              TO MAINTL
00265                                    FILETYPL
00266            MOVE +12             TO CONTROLL
00267            MOVE AL-UANON        TO CONTROLA
00268                                    MAINTA
00269                                    FILETYPA
00270            IF  PI-LANGUAGE-IS-FR
00271                MOVE 'M'         TO MAINTO
00272                GO TO 8100-SEND-INITIAL-MAP
00273
00274            ELSE
00275                MOVE 'S'         TO MAINTO
00276                GO TO 8100-SEND-INITIAL-MAP.
00277
00278      
      * EXEC CICS HANDLE CONDITION
00279 *         PGMIDERR(9600-PGMID-ERROR)
00280 *         ERROR   (9990-ABEND)
00281 *     END-EXEC.
      *    MOVE '"$L.                  ! " #00002902' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303032393032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00282
00283      IF EIBAID = DFHCLEAR
00284         GO TO 9400-CLEAR.
00285
00286      IF PI-PROCESSOR-ID = 'LGXX'
00287          GO TO 0200-RECEIVE.
00288
00289      IF  MORTGAGE-SESSION
00290          PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT
00291          GO TO 0200-RECEIVE.
00292
00293      
      * EXEC CICS READQ TS
00294 *        QUEUE  (PI-SECURITY-TEMP-STORE-ID)
00295 *        INTO   (SECURITY-CONTROL)
00296 *        LENGTH (SC-COMM-LENGTH)
00297 *        ITEM   (W-SC-ITEM)
00298 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00002917' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 W-SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00299
00300      MOVE SC-CREDIT-DISPLAY (03)  TO PI-DISPLAY-CAP.
00301      MOVE SC-CREDIT-UPDATE  (03)  TO PI-MODIFY-CAP.
00302
00303      IF NOT DISPLAY-CAP
00304          MOVE 'READ'          TO SM-READ
00305          PERFORM 9995-SECURITY-VIOLATION
00306          MOVE ER-0070         TO  EMI-ERROR
00307          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00308          GO TO 8100-SEND-INITIAL-MAP.
00309
00310  EJECT
00311  0200-RECEIVE.
00312      MOVE LOW-VALUES TO EL104AI.
00313
00314      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00315            MOVE -1 TO MAINTL
00316            MOVE ER-0008 TO EMI-ERROR
00317            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00318            GO TO 8200-SEND-DATAONLY.
00319
00320      
      * EXEC CICS RECEIVE
00321 *         MAP   (MAP-NAME)
00322 *         MAPSET(MAPSET-NAME)
00323 *         INTO  (EL104AI)
00324 *     END-EXEC.
           MOVE LENGTH OF
            EL104AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00002944' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL104AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00325
00326      IF PFENTERL = ZEROS
00327         GO TO 2001-CHECK-PFKEYS.
00328
00329      IF EIBAID NOT = DFHENTER
00330         MOVE ER-0004 TO EMI-ERROR
00331         GO TO 2002-INPUT-ERROR.
00332
00333      IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)
00334          MOVE PF-VALUES (PFENTERI) TO EIBAID
00335      ELSE
00336          MOVE ER-0029 TO EMI-ERROR
00337          GO TO 2002-INPUT-ERROR.
00338
00339  2001-CHECK-PFKEYS.
00340      IF EIBAID = DFHPF23
00341          GO TO 9000-RETURN-CICS.
00342
00343      IF EIBAID = DFHPF24
00344          GO TO 9200-RETURN-MAIN-MENU.
00345
00346      IF EIBAID = DFHPF12
00347          GO TO 9500-PF12.
00348
00349      IF  FILETYPL = ZERO
00350          MOVE ER-7270            TO EMI-ERROR
00351          MOVE -1                 TO FILETYPL
00352          MOVE AL-UABON           TO FILETYPA
00353          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00354          GO TO 8200-SEND-DATAONLY
00355
00356      ELSE
00357          MOVE FILETYPI           TO W-FILE-TYPE
00358
00359          IF  W-LETTER
00360              MOVE '1'            TO PI-ENTRY-CD-1
00361
00362          ELSE
00363              IF  W-FORM
00364                  MOVE '2'        TO PI-ENTRY-CD-1
00365
00366              ELSE
00367                  IF  W-HELP
00368                      MOVE '3'    TO PI-ENTRY-CD-1
00369
00370                  ELSE
00371                      MOVE '0'    TO PI-ENTRY-CD-1
00372                      MOVE ER-7270
00373                                  TO EMI-ERROR
00374                      MOVE -1     TO FILETYPL
00375                      MOVE AL-UABON
00376                                  TO FILETYPA
00377                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00378                      GO TO 8200-SEND-DATAONLY.
00379
00380      MOVE FILETYPI               TO PI-FILETYP.
00381
00382      IF EIBAID = DFHPF1 OR DFHPF2
00383         GO TO 4000-BROWSE-FILE.
00384
00385      IF EIBAID = DFHENTER
00386         GO TO 2003-EDIT-DATA.
00387
00388      IF EIBAID = DFHPF5  AND
00389         PI-PROCESSOR-ID = 'LGXX'
00390          GO TO 2300-COPY-SAMP.
00391
00392      MOVE ER-0029 TO EMI-ERROR.
00393
00394  2002-INPUT-ERROR.
00395      MOVE -1       TO PFENTERL.
00396      MOVE AL-UNBON TO PFENTERA.
00397      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00398      GO TO 8200-SEND-DATAONLY.
00399
00400  2003-EDIT-DATA.
00401
00402      IF MAINTL = ZERO
00403          MOVE ER-0023            TO EMI-ERROR
00404          MOVE -1                 TO MAINTL
00405          MOVE AL-UABON           TO MAINTA
00406          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00407          GO TO 8200-SEND-DATAONLY.
00408
00409      MOVE MAINTI                 TO W-MAINT.
00410
00411      IF  CREDIT-SESSION
00412
00413          IF  W-VALID-MAINT-CR
00414              MOVE MAINTI         TO PI-ACTION
00415
00416          ELSE
00417              MOVE ER-0023        TO EMI-ERROR
00418              MOVE -1             TO MAINTL
00419              MOVE AL-UABON       TO MAINTA
00420              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00421              GO TO 8200-SEND-DATAONLY
00422
00423      ELSE
00424          IF  W-VALID-MAINT-REST
00425              MOVE MAINTI         TO PI-ACTION
00426
00427          ELSE
00428              MOVE ER-0023        TO EMI-ERROR
00429              MOVE -1             TO MAINTL
00430              MOVE AL-UABON       TO MAINTA
00431              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00432              GO TO 8200-SEND-DATAONLY.
00433
00434      IF  W-SHOW
00435          NEXT SENTENCE
00436
00437      ELSE
00438          IF NOT MODIFY-CAP
00439              MOVE 'UPDATE'       TO SM-READ
00440              PERFORM 9995-SECURITY-VIOLATION
00441              MOVE ER-0070        TO EMI-ERROR
00442              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00443              GO TO 8200-SEND-DATAONLY.
00444
00445      MOVE CONTROLI TO OLD-COMM-AREA  CNTL-KEY PI-COMM-CONTROL.
00446      MOVE PI-COMPANY-CD TO OLD-CO-CD.
00447      MOVE ZEROS         TO OLD-SEQ.
00448
00449      IF PI-ENTRY-CD-1 = '1'
00450          MOVE LETTER-ID TO FILE-ID
00451      ELSE
00452          IF PI-ENTRY-CD-1 = '2'
00453              MOVE FORM-ID TO FILE-ID
00454          ELSE
00455              MOVE HELP-ID TO FILE-ID.
00456
020410     IF FILE-ID = HELP-ID
020410         MOVE LOW-VALUES TO OLD-CO-CD
020410     END-IF.
020410
00457      MOVE OLD-KEY TO OLD-KEY-SAVE.
00458
00459      IF CONTROLL = ZEROS
00460          MOVE -1                  TO CONTROLL
00461          MOVE ER-0005             TO EMI-ERROR
00462          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00463      ELSE
00464          MOVE AL-UANON            TO CONTROLA.
00465
00466      IF  W-TRANSFORM
00467
00468          IF  NEWCONTL = ZEROS
00469              MOVE CONTROLI       TO NEWCONTO
00470              MOVE AL-UANON       TO NEWCONTA
00471
00472          ELSE
00473              MOVE NEWCONTI       TO NEWCONTO
00474              MOVE AL-UANON       TO NEWCONTA
00475
00476      ELSE
00477          IF  W-RENAME
00478                  OR
00479              W-COPY
00480
00481              IF  NEWCONTL = ZEROS
00482                  MOVE -1         TO NEWCONTL
00483                  MOVE ER-7271    TO EMI-ERROR
00484                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00485
00486              ELSE
00487                  MOVE NEWCONTI   TO NEWCONTO
00488                  MOVE AL-UANON   TO NEWCONTA.
00489
00490      IF NOT EMI-NO-ERRORS
00491         GO TO 8200-SEND-DATAONLY.
00492
00493      IF  W-ADD
00494              OR
00495          W-CHANGE
00496              OR
00497          W-SHOW
00498          GO TO 2100-BUILD-TS-RECORDS.
00499
00500      IF  W-TRANSFORM
00501          GO TO 2700-CONVERT-LETTERS.
00502
00503      IF  W-RENAME
00504          MOVE NEWCONTI           TO CNTL-KEY
00505          MOVE '1'                TO REQUEST-TYPE
00506          MOVE AL-UANON           TO NEWCONTA.
00507
00508      IF  W-COPY
00509          MOVE NEWCONTI           TO CNTL-KEY
00510          MOVE '2'                TO REQUEST-TYPE
00511          MOVE AL-UANON           TO NEWCONTA.
00512
00513      IF  W-DELETE
00514          MOVE CONTROLI           TO CNTL-KEY
00515          MOVE '3'                TO REQUEST-TYPE
00516          MOVE AL-UANON           TO CONTROLA
00517          GO TO 5000-DELETE-FILE.
00518
00519      PERFORM 2500-EDIT-CONTROL THRU 2599-EXIT.
00520      GO TO 3000-RENUMBER-FILE.
00521
00522  2100-BUILD-TS-RECORDS.
00523      MOVE CONTROLI TO CNTL-KEY
00524                       PI-LAST-CONTROL.
00525      PERFORM 2500-EDIT-CONTROL THRU 2599-EXIT.
00526      MOVE PGM-EL1042 TO PGM-NAME.
00527      GO TO 9300-XCTL.
00528  EJECT
00529  2300-COPY-SAMP.
00530     
      * EXEC CICS GETMAIN
00531 *       SET     (ADDRESS OF TEXT-FILES)
00532 *       LENGTH  (100)
00533 *   END-EXEC.
           MOVE 100
             TO DFHEIV11
      *    MOVE '," L                  $   #00003158' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 DFHEIV99
           SET ADDRESS OF TEXT-FILES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00534
00535      MOVE CONTROLI   TO  CNTL-KEY  OLD-COMM-AREA.
00536      MOVE ZEROS      TO  OLD-SEQ.
00537      MOVE LETTER-ID  TO  FILE-ID.
00538
00539      
      * EXEC CICS HANDLE CONDITION
00540 *        NOTFND   (2330-NOT-FOUND)
00541 *        NOTOPEN  (6000-NOT-OPEN)
00542 *        DUPREC   (2350-DUP-REC)
00543 *        ENDFILE  (2340-END-FILE)
00544 *    END-EXEC.
      *    MOVE '"$IJ%''                ! # #00003167' TO DFHEIV0
           MOVE X'2224494A2527202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033313637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00545
00546  2310-LOOP.
00547      MOVE WS-LGX-COMPANY-CD  TO  OLD-CO-CD.
00548
020410     IF FILE-ID = HELP-ID
020410         MOVE LOW-VALUES TO OLD-CO-CD
020410     END-IF.
020410
00549      
      * EXEC CICS STARTBR
00550 *        DATASET    (FILE-ID)
00551 *        RIDFLD     (OLD-KEY)
00552 *        KEYLENGTH  (15)
00553 *        GTEQ
00554 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   K     G          &   #00003181' TO DFHEIV0
           MOVE X'262C2020204B202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID, 
                 OLD-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00555
00556  2320-READNEXT.
00557      
      * EXEC CICS READNEXT
00558 *        INTO     (TEXT-FILES)
00559 *        DATASET  (FILE-ID)
00560 *        RIDFLD   (OLD-KEY)
00561 *    END-EXEC.
           MOVE LENGTH OF
            TEXT-FILES
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00003189' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID, 
                 TEXT-FILES, 
                 DFHEIV12, 
                 OLD-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00562
00563      
      * EXEC CICS ENDBR
00564 *        DATASET  (FILE-ID)
00565 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003195' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00566
00567      IF TX-ACCESS-CD-GENL NOT = CNTL-KEY
00568          GO TO 2340-END-FILE.
00569
020410     IF FILE-ID = HELP-ID
020410         MOVE LOW-VALUES TO  TX-COMPANY-CD
020410     ELSE
00570      MOVE PI-COMPANY-CD  TO  TX-COMPANY-CD.
00571
00572      
      * EXEC CICS WRITE
00573 *        FROM     (TEXT-FILES)
00574 *        DATASET  (FILE-ID)
00575 *        RIDFLD   (TX-CONTROL-PRIMARY)
00576 *    END-EXEC.
           MOVE LENGTH OF
            TEXT-FILES
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003207' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID, 
                 TEXT-FILES, 
                 DFHEIV11, 
                 TX-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00577
00578      ADD +1  TO  TX-LINE-SEQUENCE.
00579
00580      MOVE TX-LINE-SEQUENCE  TO  OLD-SEQ.
00581      MOVE 1                 TO  UPDATE-SW.
00582
00583      GO TO 2310-LOOP.
00584
00585  2330-NOT-FOUND.
00586      MOVE -1    TO  CONTROLL.
00587      MOVE ER-0006  TO  EMI-ERROR.
00588
00589      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00590
00591      GO TO 8200-SEND-DATAONLY.
00592
00593  2340-END-FILE.
00594      IF NOT FILE-UPDATED
00595          GO TO 2330-NOT-FOUND.
00596
00597      MOVE LOW-VALUES  TO  EL104AO.
00598      MOVE -1          TO  MAINTL.
00599
00600      GO TO 8100-SEND-INITIAL-MAP.
00601
00602  2350-DUP-REC.
00603      MOVE -1    TO  NEWCONTL.
00604      MOVE ER-0076  TO  EMI-ERROR.
00605
00606      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00607
00608      GO TO 8200-SEND-DATAONLY.
00609  EJECT
00610  2500-EDIT-CONTROL.
00611      IF PI-ENTRY-CD-1 = '1'
00612         IF K5 NOT = SPACES
00613            MOVE ER-0009 TO EMI-ERROR
00614            GO TO 2600-SET-ERROR-FLAG.
00615
00616      IF PI-ENTRY-CD-1 = '3'
00617          IF (K1 = ' ' OR 'S' OR 'E')  AND
00618             K9 = SPACE
00619              NEXT SENTENCE
00620          ELSE
00621               MOVE ER-0012 TO EMI-ERROR
00622               GO TO 2600-SET-ERROR-FLAG.
00623
00624      IF PI-ENTRY-CD-1 = '3'
00625          IF K1 = 'E'
00626             IF K2 NOT NUMERIC  OR
00627                K3 NOT NUMERIC  OR
00628                K4 NOT NUMERIC  OR
00629                K5 NOT NUMERIC
00630                 MOVE ER-0012 TO EMI-ERROR
00631                 GO TO 2600-SET-ERROR-FLAG.
00632
00633      IF PI-ENTRY-CD-1 = '3'
00634          IF K1 = 'S'
00635             IF K2 NOT NUMERIC  OR
00636                K3 NOT NUMERIC  OR
00637                K4 NOT NUMERIC
00638                 MOVE ER-0012 TO EMI-ERROR
00639                 GO TO 2600-SET-ERROR-FLAG.
00640
00641  2599-EXIT.
00642       EXIT.
00643
00644  2600-SET-ERROR-FLAG.
00645      IF RENUMBER-REQUEST OR COPY-REQUEST
00646          MOVE -1       TO NEWCONTL
00647          MOVE AL-UABON TO NEWCONTA
00648      ELSE
00649          MOVE -1       TO CONTROLL
00650          MOVE AL-UABON TO CONTROLA.
00651
00652      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00653      GO TO 8200-SEND-DATAONLY.
00654  EJECT
00655  2700-CONVERT-LETTERS.
00656
00657      MOVE NEWCONTI               TO PI-NEW-NAME.
00658      MOVE CONTROLI               TO PI-OLD-NAME.
00659      MOVE 0000                   TO PI-1043-ERROR.
00660
00661      
      * EXEC CICS LINK
00662 *         PROGRAM   ('EL1043')
00663 *         COMMAREA  (PROGRAM-INTERFACE-BLOCK)
00664 *         LENGTH    (PI-COMM-LENGTH)
00665 *    END-EXEC.
           MOVE 'EL1043' TO DFHEIV1
      *    MOVE '."C                   ''   #00003296' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00666
00667      IF  PI-1043-ERROR NOT EQUAL 0000
00668          MOVE PI-1043-ERROR      TO EMI-ERROR
00669          MOVE -1                 TO CONTROLL
00670
00671      ELSE
00672          MOVE NEWCONTI           TO CONTROLO
00673          MOVE SPACES             TO NEWCONTO
00674          MOVE -1                 TO MAINTL
00675          MOVE ER-7392            TO EMI-ERROR.
00676
00677          IF  PI-LANGUAGE-IS-FR
00678              MOVE 'M'            TO MAINTO
00679
00680          ELSE
00681              MOVE 'S'            TO MAINTO.
00682
00683      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00684      GO TO 8100-SEND-INITIAL-MAP.
00685
00686  2700-EXIT.
00687      EXIT.
00688  EJECT
00689  3000-RENUMBER-FILE.
00690      MOVE 0                      TO UPDATE-SW.
00691
00692      
      * EXEC CICS GETMAIN
00693 *         SET    (ADDRESS OF TEXT-FILES)
00694 *         LENGTH (100)
00695 *     END-EXEC.
           MOVE 100
             TO DFHEIV11
      *    MOVE '," L                  $   #00003327' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 DFHEIV99
           SET ADDRESS OF TEXT-FILES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00696
00697      MOVE CONTROLI TO CNTL-KEY.
00698
00699      
      * EXEC CICS HANDLE CONDITION
00700 *         NOTFND (3900-NOT-FOUND)
00701 *         NOTOPEN(6000-NOT-OPEN)
00702 *         DUPREC (3920-DUP-REC)
00703 *         ENDFILE(3910-END-FILE)
00704 *     END-EXEC.
      *    MOVE '"$IJ%''                ! $ #00003334' TO DFHEIV0
           MOVE X'2224494A2527202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303033333334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00705
00706  3001-LOOP.
00707      
      * EXEC CICS STARTBR
00708 *         DATASET  (FILE-ID)
00709 *         RIDFLD   (OLD-KEY)
00710 *         KEYLENGTH(15)
00711 *         GTEQ
00712 *     END-EXEC.
           MOVE 15
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   K     G          &   #00003342' TO DFHEIV0
           MOVE X'262C2020204B202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID, 
                 OLD-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00713
00714  3100-READNEXT.
00715      
      * EXEC CICS READNEXT
00716 *         INTO   (TEXT-FILES)
00717 *         DATASET(FILE-ID)
00718 *         RIDFLD (OLD-KEY)
00719 *     END-EXEC.
           MOVE LENGTH OF
            TEXT-FILES
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00003350' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID, 
                 TEXT-FILES, 
                 DFHEIV12, 
                 OLD-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00720
00721      
      * EXEC CICS ENDBR
00722 *         DATASET (FILE-ID)
00723 *     END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003356' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00724
00725      IF TX-ACCESS-CD-GENL NOT = CNTL-KEY
00726         GO TO 3910-END-FILE.
00727
00728      MOVE NEWCONTI TO TX-ACCESS-CD-GENL.
00729
00730      
      * EXEC CICS WRITE
00731 *         FROM   (TEXT-FILES)
00732 *         DATASET(FILE-ID)
00733 *         RIDFLD (TX-CONTROL-PRIMARY)
00734 *     END-EXEC.
           MOVE LENGTH OF
            TEXT-FILES
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003365' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID, 
                 TEXT-FILES, 
                 DFHEIV11, 
                 TX-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00735
00736      ADD +1 TO TX-LINE-SEQUENCE.
00737      MOVE TX-LINE-SEQUENCE TO OLD-SEQ.
00738      MOVE 1 TO UPDATE-SW
00739      GO TO 3001-LOOP.
00740
00741  EJECT
00742  3900-NOT-FOUND.
00743      MOVE -1      TO CONTROLL.
00744      MOVE ER-0006 TO EMI-ERROR.
00745      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00746      GO TO 8200-SEND-DATAONLY.
00747
00748  3910-END-FILE.
00749      IF NOT FILE-UPDATED
00750         GO TO 3900-NOT-FOUND.
00751
00752      IF RENUMBER-REQUEST
00753         
      * EXEC CICS DELETE
00754 *            DATASET  (FILE-ID)
00755 *            RIDFLD   (OLD-KEY-SAVE)
00756 *            KEYLENGTH(13)
00757 *            GENERIC
00758 *        END-EXEC.
           MOVE 13
             TO DFHEIV11
      *    MOVE '&(  RKG               &   #00003388' TO DFHEIV0
           MOVE X'26282020524B472020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID, 
                 OLD-KEY-SAVE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00759
00760      MOVE LOW-VALUES TO EL104AO.
00761      MOVE -1         TO CONTROLL.
00762      GO TO 8100-SEND-INITIAL-MAP.
00763
00764  3920-DUP-REC.
00765      MOVE -1      TO NEWCONTL.
00766      MOVE ER-0076 TO EMI-ERROR.
00767      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00768      GO TO 8200-SEND-DATAONLY.
00769
00770  EJECT
00771  4000-BROWSE-FILE.
00772      IF PI-ENTRY-CD-1 = '1'
00773          MOVE LETTER-ID TO FILE-ID
00774      ELSE
00775          IF PI-ENTRY-CD-1 = '2'
00776              MOVE FORM-ID TO FILE-ID
00777          ELSE
00778              MOVE HELP-ID TO FILE-ID.
00779
00780      MOVE SPACE TO BROWSE-STARTED-SW.
00781
00782      
      * EXEC CICS HANDLE CONDITION
00783 *         NOTOPEN (6000-NOT-OPEN)
00784 *         NOTFND  (4800-NO-RECORD)
00785 *         ENDFILE (4900-END-FILE)
00786 *     END-EXEC.
      *    MOVE '"$JI''                 ! % #00003417' TO DFHEIV0
           MOVE X'22244A492720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303033343137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00787
00788      IF  CONTROLI = SPACES OR LOW-VALUES
00789
00790          IF  PI-LAST-CONTROL GREATER THAN LOW-VALUES
00791              MOVE PI-LAST-CONTROL
00792                                  TO CONTROLI
00793
00794          ELSE
00795              MOVE LOW-VALUES     TO CONTROLI.
00796
00797      MOVE PI-COMPANY-CD TO CO-CD.
020410     IF FILE-ID = HELP-ID
020410         MOVE LOW-VALUES TO CO-CD
020410     END-IF.
020410
00798      MOVE ZEROS         TO SEQ.
00799      MOVE CONTROLI      TO CNTL-AREA  CNTL-KEY  PI-COMM-CONTROL.
00800
00801      
      * EXEC CICS STARTBR
00802 *         DATASET(FILE-ID)
00803 *         RIDFLD (FILE-KEY)
00804 *     END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00003440' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID, 
                 FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00805
00806      MOVE 'Y' TO BROWSE-STARTED-SW.
00807
00808      IF EIBAID = DFHPF2
00809         MOVE 1 TO SEQ
00810         GO TO 4600-BROWSE-BKWD
00811        ELSE
00812         MOVE 9999 TO SEQ.
00813
00814  4500-READNEXT.
00815      
      * EXEC CICS READNEXT
00816 *         SET    (ADDRESS OF TEXT-FILES)
00817 *         DATASET(FILE-ID)
00818 *         RIDFLD (FILE-KEY)
00819 *     END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00003454' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF TEXT-FILES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00820
020410     IF FILE-ID = HELP-ID
020410         NEXT SENTENCE
020410     ELSE
00821      IF TX-COMPANY-CD NOT = PI-COMPANY-CD
00822         GO TO 4900-END-FILE.
00823
00824      IF EIBAID = DFHPF1
00825         IF CONTROLO = TX-ACCESS-CD-GENL
00826           GO TO 4500-READNEXT.
00827
00828  4550-DISPLAY-SCREEN.
00829      MOVE TX-ACCESS-CD-GENL TO CONTROLO
00830                                CNTL-KEY   PI-COMM-CONTROL.
00831
00832      MOVE TX-CONTROL-PRIMARY    TO OLD-KEY.
00833      MOVE -1       TO MAINTL.
00834      MOVE AL-UANON TO CONTROLA.
00835      GO TO 8100-SEND-INITIAL-MAP.
00836
00837  4600-BROWSE-BKWD.
00838      
      * EXEC CICS HANDLE CONDITION
00839 *         NOTFND(4900-END-FILE)
00840 *     END-EXEC.
      *    MOVE '"$I                   ! & #00003480' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303033343830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00841
00842      
      * EXEC CICS READPREV
00843 *       DATASET (FILE-ID)
00844 *       SET     (ADDRESS OF TEXT-FILES)
00845 *       RIDFLD  (FILE-KEY)
00846 *   END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00003484' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF TEXT-FILES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00847
00848  4650-BROWSE-LOOP.
00849      IF PI-FILE-EOF
00850         MOVE SPACE TO PI-EOF-SW
00851       ELSE
00852         
      * EXEC CICS READPREV
00853 *       DATASET (FILE-ID)
00854 *       SET     (ADDRESS OF TEXT-FILES)
00855 *       RIDFLD  (FILE-KEY)
00856 *   END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00003494' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF TEXT-FILES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00857
020410     IF FILE-ID = HELP-ID
020410         NEXT SENTENCE
020410     ELSE
00858      IF TX-COMPANY-CD NOT = PI-COMPANY-CD
00859         GO TO 4900-END-FILE.
00860
00861      GO TO 4550-DISPLAY-SCREEN.
00862
00863  4800-NO-RECORD.
00864      MOVE -1                     TO CONTROLL.
00865      MOVE AL-UANON               TO CONTROLA.
00866      MOVE ER-1162                TO EMI-ERROR.
00867      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00868
00869      IF BROWSE-STARTED
00870        
      * EXEC CICS ENDBR
00871 *           DATASET  (FILE-ID)
00872 *       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003515' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00873
00874      GO TO 8200-SEND-DATAONLY.
00875
00876  4900-END-FILE.
00877      IF EIBAID = DFHPF1
00878          MOVE 'Y'                TO PI-EOF-SW
00879          MOVE ER-2237            TO EMI-ERROR
00880      ELSE
00881          MOVE ER-2238            TO EMI-ERROR.
00882
00883      MOVE -1                     TO MAINTL.
00884
00885      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00886
00887      IF BROWSE-STARTED
00888        
      * EXEC CICS ENDBR
00889 *           DATASET  (FILE-ID)
00890 *       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003533' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00891
00892      MOVE SPACE TO BROWSE-STARTED-SW.
00893      GO TO 8200-SEND-DATAONLY.
00894  EJECT
00895  5000-DELETE-FILE.
00896      
      * EXEC CICS HANDLE CONDITION
00897 *         NOTFND (3900-NOT-FOUND)
00898 *         NOTOPEN(6000-NOT-OPEN)
00899 *     END-EXEC.
      *    MOVE '"$IJ                  ! '' #00003541' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303033353431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00900
00901      
      * EXEC CICS DELETE
00902 *         DATASET  (FILE-ID)
00903 *         RIDFLD   (OLD-KEY-SAVE)
00904 *         KEYLENGTH(13)
00905 *         GENERIC
00906 *     END-EXEC.
           MOVE 13
             TO DFHEIV11
      *    MOVE '&(  RKG               &   #00003546' TO DFHEIV0
           MOVE X'26282020524B472020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID, 
                 OLD-KEY-SAVE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00907
00908      MOVE 1          TO UPDATE-SW.
00909      MOVE LOW-VALUES TO EL104AI.
00910      GO TO 8100-SEND-INITIAL-MAP.
00911
00912  6000-NOT-OPEN.
00913      MOVE -1 TO MAINTL.
00914
00915      IF PI-ENTRY-CD-1 = '1'
00916          MOVE ER-0013 TO EMI-ERROR
00917      ELSE
00918          IF PI-ENTRY-CD-1 = '2'
00919              MOVE ER-0014 TO EMI-ERROR
00920          ELSE
00921              MOVE ER-0015 TO EMI-ERROR.
00922
00923      MOVE -1           TO MAINTL.
00924      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00925      GO TO 8200-SEND-DATAONLY.
00926  EJECT
00927  8100-SEND-INITIAL-MAP.
00928
00929      MOVE 'Y'          TO PI-104-SCREEN-SENT-IND.
00930      MOVE SAVE-DATE    TO DATEO.
00931      MOVE EIBTIME      TO TIME-IN.
00932      MOVE TIME-OUT     TO TIMEO.
00933      MOVE PI-COMPANY-ID          TO COMPAO.
00934
00935      IF  NOT CREDIT-SESSION
00936          MOVE SPACES             TO TRANSFMO
00937          MOVE AL-PANOF           TO TRANSFMA.
00938
00939      IF PI-PROCESSOR-ID = 'LGXX'
00940          MOVE AL-PANOF  TO  HDNPFAA.
00941
00942      IF FILE-UPDATED
00943          MOVE ER-0000   TO EMI-ERROR
00944          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00945
00946      MOVE PI-FILETYP    TO FILETYPO.
00947      MOVE AL-UANON      TO FILETYPA.
00948
00949      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.
00950      MOVE AL-SABON             TO ERRMSG1A.
00951      MOVE EMI-MESSAGE-AREA (2) TO ERRMSG2O.
00952      MOVE AL-SABON             TO ERRMSG2A.
00953
00954      
      * EXEC CICS SEND
00955 *         MAPSET(MAPSET-NAME)
00956 *         MAP   (MAP-NAME)
00957 *         FROM  (EL104AO)
00958 *         ERASE
00959 *     END-EXEC.
           MOVE LENGTH OF
            EL104AO
             TO DFHEIV11
      *    MOVE '8$      T  E    H L F ,   #00003599' TO DFHEIV0
           MOVE X'382420202020202054202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL104AO, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00960
00961      GO TO 9100-RETURN-TRAN.
00962
00963  8200-SEND-DATAONLY.
00964
00965      IF  PI-104-SCREEN-NOT-SENT
00966          GO TO 8100-SEND-INITIAL-MAP.
00967
00968      MOVE EIBTIME  TO TIME-IN.
00969      MOVE TIME-OUT TO TIMEO.
00970      MOVE PI-COMPANY-ID          TO COMPAO.
00971
00972      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.
00973      MOVE AL-SABON             TO ERRMSG1A.
00974      MOVE EMI-MESSAGE-AREA (2) TO ERRMSG2O.
00975      MOVE AL-SABON             TO ERRMSG2A.
00976
00977      IF  NOT CREDIT-SESSION
00978          MOVE SPACES             TO TRANSFMO
00979          MOVE AL-PANOF           TO TRANSFMA.
00980
00981      IF PI-ENTRY-CD-1 NOT = '0'
00982          MOVE PI-FILETYP    TO FILETYPO
00983          MOVE AL-UANON      TO FILETYPA.
00984
00985      
      * EXEC CICS SEND
00986 *         MAPSET(MAPSET-NAME)
00987 *         MAP   (MAP-NAME)
00988 *         DATAONLY
00989 *         CURSOR
00990 *         FROM  (EL104AO)
00991 *     END-EXEC.
           MOVE LENGTH OF
            EL104AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00003630' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL104AO, 
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
           
00992
00993      GO TO 9100-RETURN-TRAN.
00994
00995  8300-SEND-TEXT.
00996      
      * EXEC CICS SEND TEXT
00997 *         FROM  (LOGOFF-TEXT)
00998 *         ERASE
00999 *         FREEKB
01000 *         LENGTH(LOGOFF-LENGTH)
01001 *     END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00003641' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363431' TO DFHEIV0(25:11)
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
           
01002
01003      
      * EXEC CICS RETURN
01004 *     END-EXEC.
      *    MOVE '.(                    &   #00003648' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01005
01006  8800-UNAUTHORIZED-ACCESS.
01007      MOVE UNACCESS-MSG TO LOGOFF-MSG.
01008      GO TO 8300-SEND-TEXT.
01009
01010  9000-RETURN-CICS.
01011      MOVE EIBAID  TO PI-ENTRY-CD-1.
01012      MOVE 'EL005' TO PGM-NAME.
01013      GO TO 9300-XCTL.
01014
01015  9100-RETURN-TRAN.
01016      MOVE MAP-NUMBER           TO PI-CURRENT-SCREEN-NO.
01017      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.
01018
01019      MOVE PI-COMM-CONTROL      TO PI-LAST-CONTROL.
01020
01021      
      * EXEC CICS RETURN
01022 *         TRANSID (TRANS-ID)
01023 *         COMMAREA(PROGRAM-INTERFACE-BLOCK)
01024 *         LENGTH  (PI-COMM-LENGTH)
01025 *     END-EXEC.
      *    MOVE '.(CT                  &   #00003666' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01026
01027  9200-RETURN-MAIN-MENU.
01028      IF CLAIM-SESSION
01029          MOVE XCTL-CLAIM         TO PGM-NAME
01030      ELSE
01031          IF CREDIT-SESSION
01032              MOVE XCTL-CREDIT    TO PGM-NAME
01033          ELSE
01034              IF WARRANTY-SESSION
01035                  MOVE XCTL-WARRANTY  TO PGM-NAME
01036              ELSE
01037                  IF MORTGAGE-SESSION
01038                      MOVE XCTL-MORTGAGE      TO PGM-NAME
01039                  ELSE
01040                      MOVE XCTL-GEN-LEDGER    TO PGM-NAME.
01041
01042  9300-XCTL.
01043      
      * EXEC CICS XCTL
01044 *         PROGRAM (PGM-NAME)
01045 *         COMMAREA(PROGRAM-INTERFACE-BLOCK)
01046 *         LENGTH  (PI-COMM-LENGTH)
01047 *     END-EXEC.
      *    MOVE '.$C                   $   #00003688' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01048
01049  9400-CLEAR.
01050      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.
01051      GO TO 9300-XCTL.
01052
01053  9500-PF12.
01054      MOVE 'EL010' TO PGM-NAME.
01055      GO TO 9300-XCTL.
01056
01057  9600-PGMID-ERROR.
01058      
      * EXEC CICS HANDLE CONDITION
01059 *         PGMIDERR(8300-SEND-TEXT)
01060 *     END-EXEC.
      *    MOVE '"$L                   ! ( #00003703' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303033373033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01061
01062      MOVE PGM-NAME     TO PI-CALLING-PROGRAM.
01063      MOVE SPACES       TO PI-ENTRY-CD-1.
01064      MOVE 'EL005'      TO PGM-NAME.
01065      MOVE PGM-NAME     TO LOGOFF-PGM.
01066      MOVE PGMIDERR-MSG TO LOGOFF-FILL.
01067      GO TO 9300-XCTL.
01068
01069
01070  9700-LINK-DATE-CONVERT.
01071      
      * EXEC CICS LINK
01072 *        PROGRAM    ('ELDATCV')
01073 *        COMMAREA   (DATE-CONVERSION-DATA)
01074 *        LENGTH     (DC-COMM-LENGTH)
01075 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00003716' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01076
01077  9700-EXIT.
01078      EXIT.
01079
01080  9900-ERROR-FORMAT.
01081
01082      MOVE PI-LANGUAGE-TYPE       TO EMI-LANGUAGE-IND.
01083
01084      IF NOT EMI-ERRORS-COMPLETE
01085         
      * EXEC CICS LINK
01086 *            PROGRAM ('EL001')
01087 *            COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)
01088 *            LENGTH  (EMI-COMM-LENGTH)
01089 *        END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   ''   #00003730' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01090
01091  9900-EXIT.
01092       EXIT.
01093
01094  9910-INITIALIZE-SECURITY.
01095 ******************************************************************
01096 *                                                                *
01097 *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *
01098 *       USER SECURITY RECORD SET UP BY EL125.  BASED ON THE      *
01099 *       APPLICATION NUMBER FOUND IN WORKING STORAGE UNDER        *
01100 *       W-APPL-SECRTY-NDX (PIC  S9(04) COMP), THIS PROGRAM       *
01101 *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *
01102 *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *
01103 *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *
01104 *       ERROR CONDITION AND EXITS THE PROGRAM.                   *
01105 *                                                                *
01106 *       NOTE:  THE CARRIER/GRP/STATE/PRODUCER SECURITY DATA      *
01107 *       IS ALSO PROVIDED BY THIS LOGIC.                          *
01108 *                                                                *
01109 ******************************************************************
01110
01111      IF  PI-PROCESSOR-ID NOT EQUAL 'LGXX'
01112          MOVE '125E'             TO SC-QUID-SYSTEM
01113          MOVE EIBTRMID           TO SC-QUID-TERMINAL
01114
01115          
      * EXEC CICS READQ TS
01116 *            QUEUE  (SC-QUID-KEY)
01117 *            INTO   (SECURITY-CONTROL-E)
01118 *            LENGTH (SC-COMM-LENGTH-E)
01119 *            ITEM   (SC-ITEM)
01120 *        END-EXEC
      *    MOVE '*$II   L              ''   #00003760' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SC-QUID-KEY, 
                 SECURITY-CONTROL-E, 
                 SC-COMM-LENGTH-E, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01121
01122          MOVE SC-MP-DISPLAY (W-APPL-SCRTY-NDX)
01123                                  TO PI-DISPLAY-CAP
01124          MOVE SC-MP-UPDATE (W-APPL-SCRTY-NDX)
01125                                  TO PI-MODIFY-CAP
01126
01127          IF  NOT DISPLAY-CAP
01128              MOVE 'READ'         TO SM-READ
01129              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
01130              MOVE ER-9097        TO EMI-ERROR
01131              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01132              PERFORM 8100-SEND-INITIAL-MAP.
01133
01134  9910-EXIT.
01135      EXIT.
01136
01137  9990-ABEND.
01138      MOVE 'EL004'                TO PGM-NAME.
01139      MOVE DFHEIBLK               TO EMI-LINE1.
01140
01141      
      * EXEC CICS LINK
01142 *        PROGRAM   (PGM-NAME)
01143 *        COMMAREA  (EMI-LINE1)
01144 *        LENGTH    (72)
01145 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00003786' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01146
01147      GO TO 8200-SEND-DATAONLY.
01148
01149      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL104' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01150
01151  9995-SECURITY-VIOLATION.
01152 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00003814' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383134' TO DFHEIV0(25:11)
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
01153
01154  9995-EXIT.
01155      EXIT.
01156

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL104' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 2330-NOT-FOUND,
                     6000-NOT-OPEN,
                     2350-DUP-REC,
                     2340-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 3900-NOT-FOUND,
                     6000-NOT-OPEN,
                     3920-DUP-REC,
                     3910-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 6000-NOT-OPEN,
                     4800-NO-RECORD,
                     4900-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 4900-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 3900-NOT-FOUND,
                     6000-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL104' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
