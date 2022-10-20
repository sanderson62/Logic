00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL1042.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 03/21/95 13:28:51.
00007 *                            VMOD=2.021
00008 *
00008 *
00009 *AUTHOR.           LOGIC,INC.
00010 *                  DALLAS,TEXAS.
00011
00012 *DATE-COMPILED.
00013 *            *****************************************************
00014 *            *                                                   *
00015 *            * THIS PROGRAM IS THE PROPERTY OF LOCIC, INC. *
00016 *            *                                                   *
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00020 *            *                                                   *
00021 *            *****************************************************
00022
00023 *REMARKS. TRANSACTION EX14 - TEXT FILE MAINTENANCE
00024 *        THIS PROGRAM IS USED TO PERFORM MAINTENANCE TO THE HELP,
00025 *        LETTER, AND FORM FILES, DEPENDING ON THE OPTION
00026 *        SPECIFIED FROM THE TEXT MAINT MENU (EL104).
020410******************************************************************
020410*                   C H A N G E   L O G
020410*
020410* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
020410*-----------------------------------------------------------------
020410*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
020410* EFFECTIVE    NUMBER
020410*-----------------------------------------------------------------
020410* 020410  CR2009061500002  AJRA  FIX HELP FILE SELECTION
082211* 082211  CR2011022800001  AJRA  NAPERSOFT ACCT SERVICES
020410******************************************************************
00027
00028      EJECT
00029  ENVIRONMENT DIVISION.
00030  DATA DIVISION.
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032  77  FILLER  PIC X(32)  VALUE '********************************'.
00033  77  FILLER  PIC X(32)  VALUE '*   EL1042 WORKING STORAGE     *'.
00034  77  FILLER  PIC X(32)  VALUE '******* VMOD=2.021 *************'.
00035
00036 *                            COPY ELCSCTM.
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
00037 *                            COPY ELCSCRTY.
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
00038 *                            COPY MPCSCRT.
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
00039
00040     EJECT
00041
00042  01  WS-DATE-AREA.
00043      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00044      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
00045
00046  01  W-LAST-SAVE-AREA.
00047      05  W-SAVE-LAST-MAINT-BY    PIC X(4)    VALUE SPACES.
00048      05  W-SAVE-LAST-MAINT-DT    PIC X(8)    VALUE SPACES.
00049
00050  01  W-HOLD-LINE                 PIC  X(73).
00051
00052  01  WS-CONSTANTS.
00053      12  LOWER-CASE PIC X(26) VALUE 'abcdefghijklmnopqrstuvwxyz'.
00054      12  UPPER-CASE PIC X(26) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
00055      12  W-APPL-SCRTY-NDX        PIC S9(04)  COMP VALUE +7.
00056      12  MAP-NAME.
00057          16  MAP-NAME-PRE        PIC X(2)    VALUE 'EL'.
00058          16  MAP-NUMBER          PIC X(4)    VALUE '104B'.
00059          16  MAP-NAME-FILL       PIC X(2)    VALUE SPACES.
00060
00061      12  W-SC-ITEM               PIC S9(4) COMP VALUE +1.
00062      12  MAPSET-NAME             PIC X(8)    VALUE 'EL104S'.
00063      12  TRANS-ID                PIC X(4)    VALUE 'EX14'.
00064      12  PGM-NAME                PIC X(8).
00065      12  PGM-EL152               PIC X(8)    VALUE 'EL152'.
00066      12  PGM-EM152               PIC X(8)    VALUE 'EM152'.
00067      12  PGM-EL689               PIC X(8)    VALUE 'EL689'.
00068      12  THIS-PGM                PIC X(8)    VALUE 'EL1042'.
00069      12  XCTL-CLAIM              PIC X(8)    VALUE 'EL126'.
00070      12  XCTL-CREDIT             PIC X(8)    VALUE 'EL626'.
00071      12  XCTL-WARRANTY           PIC X(8)    VALUE 'WA126'.
00072      12  XCTL-MORTGAGE           PIC X(8)    VALUE 'EM626'.
00073      12  XCTL-GEN-LEDGER         PIC X(8)    VALUE 'GL800'.
00074      12  FILE-ID                 PIC X(8).
00075      12  LETTER-ID               PIC X(8)    VALUE 'ELLETR'.
00076      12  WS-8126-ERROR-SW        PIC X       VALUE 'N'.
00077      12  W-LINE-SQUEEZE-IND      PIC X(01).
00078          88  W-LINE-SQUEEZE-VALID-VALUE      VALUE ' ' 'A'
00079              'C' 'D' 'E' 'F' 'G' 'H' 'K' 'N' 'P' 'Q' 'R' 'S'
00080              'T' 'U' 'Z' '1' '2' '3' '4' '5'.
00081      12  FORM-ID                 PIC X(8)    VALUE 'ELFORM'.
00082      12  HELP-ID                 PIC X(8)    VALUE 'ELHELP'.
00083      12  TS-NAME.
00084          16  FILLER              PIC X(4)    VALUE '104A'.
00085          16  TS-TERM             PIC X(4).
00086      12  TS-ITEM                 PIC S9(4)   COMP VALUE +0.
00087      12  FILE-LENGTH             PIC S9(4)   COMP VALUE +100.
00088      12  FILE-KEY.
00089          16  FILE-PARTIAL-KEY.
00090            18  CO-CD             PIC X.
00091            18  CNTL-AREA         PIC X(12).
00092          16  SEQ                 PIC S9(4)   COMP.
00093      12  OLD-KEY-SAVE            PIC X(13).
00094
00095      12  TIME-IN                 PIC S9(7).
00096      12  FILLER REDEFINES TIME-IN.
00097          16  FILLER              PIC X.
00098          16  TIME-OUT            PIC 99V99.
00099          16  FILLER              PIC 9(2).
00100      12  MAX-LINES               PIC 999     VALUE 300.
00101      12  NUM-LINES-PER-SCREEN    PIC 99      VALUE 15.
00102      12  TS-NUM-REC-IN-GROUP     PIC 99      VALUE 50.
00103      12  TS-GROUP-WORK           PIC 9(5)    VALUE 0  COMP-3.
00104      12  TS-LENGTH               PIC S9(4)   VALUE +3650 COMP.
00105      12  ROLL-COUNTER            PIC S999    VALUE +0 COMP-3.
00106      12  TEMP-CURR-LINE          PIC S9(3)   COMP-3.
00107      EJECT
00108 *                                COPY ELCLOGOF.
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
00109      EJECT
00110 *                                COPY ELCAID.
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
00111  01  FILLER  REDEFINES DFHAID.
00112      12  FILLER                  PIC X(8).
00113      12  PF-VALUES OCCURS 24 TIMES       PIC X.
00114      EJECT
00115 *                                    COPY ELCEMIB.
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
00116
00117  01  WS-ERROR-MESSAGE-AREA.
00118      12  ER-0000                     PIC 9(4)   VALUE 0000.
00119      12  ER-0004                     PIC 9(4)   VALUE 0004.
00120      12  ER-0006                     PIC 9(4)   VALUE 0006.
00121      12  ER-0008                     PIC 9(4)   VALUE 0008.
00122      12  ER-0013                     PIC 9(4)   VALUE 0013.
00123      12  ER-0014                     PIC 9(4)   VALUE 0014.
00124      12  ER-0015                     PIC 9(4)   VALUE 0015.
00125      12  ER-0023                     PIC 9(4)   VALUE 0023.
00126      12  ER-0029                     PIC 9(4)   VALUE 0029.
00127      12  ER-0030                     PIC 9(4)   VALUE 0030.
00128      12  ER-0031                     PIC 9(4)   VALUE 0031.
00129      12  ER-0032                     PIC 9(4)   VALUE 0032.
00130      12  ER-0033                     PIC 9(4)   VALUE 0033.
00131      12  ER-0041                     PIC 9(4)   VALUE 0041.
00132      12  ER-0044                     PIC 9(4)   VALUE 0044.
00133      12  ER-0045                     PIC 9(4)   VALUE 0045.
00134      12  ER-0047                     PIC 9(4)   VALUE 0047.
00135      12  ER-0048                     PIC 9(4)   VALUE 0048.
00136      12  ER-0049                     PIC 9(4)   VALUE 0049.
00137      12  ER-0050                     PIC 9(4)   VALUE 0050.
00138      12  ER-0051                     PIC 9(4)   VALUE 0051.
00139      12  ER-0066                     PIC 9(4)   VALUE 0066.
00140      12  ER-0067                     PIC 9(4)   VALUE 0067.
00141      12  ER-0069                     PIC 9(4)   VALUE 0069.
00142      12  ER-0070                     PIC 9(4)   VALUE 0070.
00143      12  ER-0140                     PIC 9(4)   VALUE 0140.
00144      12  ER-0141                     PIC 9(4)   VALUE 0141.
00145      12  ER-0212                     PIC 9(4)   VALUE 0212.
00146      12  ER-7375                     PIC 9(4)   VALUE 7375.
00147      12  ER-8126                     PIC 9(4)   VALUE 8126.
00148      12  ER-9097                     PIC 9(4)   VALUE 9097.
00149      12  ER-9348                     PIC 9(4)   VALUE 9348.
00150      12  ER-9349                     PIC 9(4)   VALUE 9349.
00151  01  EMI-SAVE-AREA                   PIC X(400).
00152      EJECT
00153  01  FILLER                          PIC X(22)
00154                               VALUE 'INTERFACE AREA STARTS:'.
00155 *                                    COPY ELCINTF.
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
00156      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.
00157 *        COPY ELC1042.
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
00158          16  FILLER                  PIC X(573).
00159
00160  01  FILLER                          PIC X(20)
00161                               VALUE ':INTERFACE AREA ENDS'.
00162      EJECT
00163 *                                    COPY ELCATTR.
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
00164      EJECT
00165 *                                    COPY ELCDATE.
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
00166      EJECT
00167  01  FILLER                          PIC X(16)
00168                               VALUE 'MAP AREA STARTS:'.
00169 *                                    COPY EL104S.
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
00170      EJECT
00171  01  EL104BR REDEFINES EL104BI.
00172      12  FILLER                  PIC X(108).
00173      12  SC-ALL-LINES.
00174       14 SC-LINES OCCURS 15 TIMES INDEXED BY SC-INDX.
00175          16  SC-LINL             PIC S9(4)   COMP.
00176          16  SC-LINA             PIC X.
00177          16  SC-LIN              PIC Z99.
00178          16  SC-TEXTL            PIC S9(4)   COMP.
00179          16  SC-TEXTA            PIC X.
00180          16  SC-TEXT             PIC X(70).
00181          16  SC-PCL              PIC S9(4)   COMP.
00182          16  SC-PCA              PIC X.
00183          16  SC-PC               PIC XX.
00184          16  SC-SQL              PIC S9(4)   COMP.
00185          16  SC-SQA              PIC X.
00186          16  SC-SQ               PIC X.
00187
00188  01  FILLER                          PIC X(14)
00189                               VALUE ':MAP AREA ENDS'.
00190  01  RECORD-TABLE                PIC X(21900) VALUE SPACES.
00191  01  REC-TABLE  REDEFINES RECORD-TABLE.
00192      12  TS-GROUP OCCURS 6 TIMES INDEXED BY TS-INDX PIC X(3650).
00193  01  REC-ENTRIES REDEFINES RECORD-TABLE.
00194      12  REC-ENT OCCURS 300 TIMES INDEXED BY TB-INDX TB-INDX1.
00195          16  REC-TEXT            PIC X(70).
00196          16  REC-PC              PIC XX.
00197          16  REC-SQ              PIC X.
00198  01  TS-WORK-AREA                PIC X(3650).
00199      EJECT
00200
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
00202  01  DFHCOMMAREA                 PIC X(1024).
00203 *01 PARMLIST .
00204 *    02  FILLER                  PIC S9(8)  COMP.
00205 *    02  TXT-ADDR                PIC S9(8)  COMP.
00206      EJECT
00207 *                                COPY ELCTEXT.
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
00208      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA TEXT-FILES.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1042' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00210
00211      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.
00212
00213      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00214      MOVE '5'                   TO DC-OPTION-CODE.
00215      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00216      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00217      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00218
00219      IF  EIBCALEN = ZEROS
00220          GO TO 8800-UNAUTHORIZED-ACCESS.
00221
00222      MOVE EIBTRMID        TO TS-TERM.
00223      MOVE PI-COMM-CONTROL TO CNTL-AREA.
00224      MOVE PI-COMPANY-CD   TO CO-CD.
00225      MOVE ZEROS           TO SEQ.
00226      MOVE LOW-VALUES      TO EL104BI.
00227      MOVE ERROR-MESSAGE-INTERFACE-BLOCK   TO EMI-SAVE-AREA.
00228      
      * EXEC CICS HANDLE CONDITION
00229 *         ERROR(9990-ABEND)
00230 *         PGMIDERR(9600-PGMID-ERROR)
00231 *    END-EXEC.
      *    MOVE '"$.L                  ! " #00002867' TO DFHEIV0
           MOVE X'22242E4C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303032383637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00232
00233      IF  PI-LANGUAGE-IS-FR
00234          MOVE 'EL104BF'          TO MAP-NAME.
00235
00236      IF  PI-ENTRY-CD-1 = '1'
00237          MOVE LETTER-ID TO FILE-ID
00238
00239      ELSE
00240          IF  PI-ENTRY-CD-1 = '2'
00241              MOVE FORM-ID TO FILE-ID
00242
00243          ELSE
00244              MOVE HELP-ID TO FILE-ID.
020410
020410     IF FILE-ID = HELP-ID
020410         MOVE LOW-VALUES TO CO-CD
020410     END-IF.
00245
00246      MOVE FILE-PARTIAL-KEY TO OLD-KEY-SAVE.
00247
00248      IF  PI-CALLING-PROGRAM NOT = THIS-PGM
00249         IF  PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00250            MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00251            MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00252            MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00253            MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00254            MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00255            MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00256            MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00257            MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00258            MOVE 'N'                  TO PI-1042-SCREEN-SENT-IND
00259            PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT
00260
00261         ELSE
00262            MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00263            MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00264            MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00265            MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00266            MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00267            MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00268            MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00269            MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00270
00271      IF  EIBTRNID = 'MX27'
00272              OR
00273          EIBTRNID = 'EX27'
00274              OR
00275          EIBTRNID = 'EXH3'
00276          PERFORM 7500-READ-TS  THRU  7599-EXIT
00277          GO TO 7050-FORMAT-LINES.
00278
00279      GO TO 7000-BUILD-TABLE.
00280
00281
00282  0100-PA.
00283      MOVE ER-0008 TO EMI-ERROR
00284      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00285      GO TO 8200-SEND-DATAONLY.
00286      EJECT
00287  0200-RECEIVE.
00288      
      * EXEC CICS HANDLE AID
00289 *         CLEAR   (9400-CLEAR)
00290 *         PA1     (0100-PA)
00291 *         PA2     (0100-PA)
00292 *         PA3     (0100-PA)
00293 *         END-EXEC.
      *    MOVE '"&=!"#               V! # #00002931' TO DFHEIV0
           MOVE X'22263D212223202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020562120' TO DFHEIV0(13:12)
           MOVE X'2320233030303032393331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00294
00295      MOVE EMI-SAVE-AREA   TO ERROR-MESSAGE-INTERFACE-BLOCK
00296      MOVE PI-COMM-CONTROL TO CNTL-AREA.
00297      MOVE PI-COMPANY-CD   TO CO-CD.
020410     IF FILE-ID = HELP-ID
020410         MOVE LOW-VALUES TO CO-CD
020410     END-IF.
00298      MOVE ZEROS           TO SEQ.
00299      MOVE ZEROS           TO ROLL-COUNTER.
00300      MOVE LOW-VALUES      TO EL104BI.
00301
00302      
      * EXEC CICS SYNCPOINT
00303 *         END-EXEC.
      *    MOVE '6"                    !   #00002948' TO DFHEIV0
           MOVE X'362220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00304
00305      IF LOWER-CASE-LETTERS-USED
00306         
      * EXEC CICS RECEIVE
00307 *            MAP   (MAP-NAME)
00308 *            MAPSET(MAPSET-NAME)
00309 *            INTO  (EL104BI)
00310 *            ASIS
00311 *            END-EXEC
           MOVE LENGTH OF
            EL104BI
             TO DFHEIV11
      *    MOVE '8"TAI  L              ''   #00002952' TO DFHEIV0
           MOVE X'382254414920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL104BI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00312          ELSE
00313         
      * EXEC CICS RECEIVE
00314 *            MAP   (MAP-NAME)
00315 *            MAPSET(MAPSET-NAME)
00316 *            INTO  (EL104BI)
00317 *            END-EXEC.
           MOVE LENGTH OF
            EL104BI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00002959' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL104BI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00318
00319      IF  NOT DISPLAY-CAP
00320          MOVE 'READ'             TO SM-READ
00321          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00322          MOVE ER-9097            TO EMI-ERROR
00323          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00324          GO TO 8100-SEND-INITIAL-MAP.
00325
00326      IF  PFENTRBL = ZEROS
00327         GO TO 2001-CHECK-PFKEYS.
00328
00329      IF  EIBAID NOT = DFHENTER
00330         MOVE ER-0004 TO EMI-ERROR
00331         GO TO 2002-INPUT-ERROR.
00332
00333      IF  (PFENTRBI NUMERIC) AND (PFENTRBI GREATER 0 AND LESS 25)
00334         MOVE PF-VALUES (PFENTRBI) TO EIBAID
00335        ELSE
00336         MOVE ER-0029 TO EMI-ERROR
00337         GO TO 2002-INPUT-ERROR.
00338
00339  2001-CHECK-PFKEYS.
00340      IF  EIBAID = DFHPF23
00341         GO TO 9000-RETURN-CICS.
00342
00343      IF  EIBAID = DFHPF24
00344         GO TO 9200-RETURN-MAIN-MENU.
00345
00346      IF  EIBAID = DFHPF12
00347         GO TO 9500-PF12.
00349      IF  FUNCTL NOT = ZEROS AND EIBAID NOT = DFHENTER
00350
00351         IF  FUNCTI = 'A' OR 'a' OR SPACES
00352            NEXT SENTENCE
00353           ELSE
00354            MOVE ER-0050 TO EMI-ERROR
00355            MOVE -1 TO FUNCTL
00356            MOVE AL-UABON TO FUNCTA PFENTRBA
00357            GO TO 2002-INPUT-ERROR.
00358
00359      IF  EIBAID = DFHPF1
00360         MOVE NUM-LINES-PER-SCREEN TO ROLL-COUNTER
00361         GO TO 7400-PAGE-ROUTINE.
00362
00363      IF  EIBAID = DFHPF2
00364         SUBTRACT NUM-LINES-PER-SCREEN FROM ROLL-COUNTER
00365         GO TO 7400-PAGE-ROUTINE.
00366
00367      IF  EIBAID = DFHPF3
00368         MOVE 5 TO ROLL-COUNTER
00369         GO TO 7400-PAGE-ROUTINE.
00370
00371      IF  EIBAID = DFHPF4
00372         MOVE -5 TO ROLL-COUNTER
00373         GO TO 7400-PAGE-ROUTINE.
           IF EIBAID = DFHPF5
              GO TO 9450-PF5
           END-IF
00375      IF  EIBAID = DFHENTER
00376         GO TO 2003-EDIT-DATA.
00377
00378      MOVE ER-0029 TO EMI-ERROR.
00379
00380  2002-INPUT-ERROR.
00381      MOVE -1       TO PFENTRBL.
00382      MOVE AL-UNBON TO PFENTRBA.
00383      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00384      GO TO 8200-SEND-DATAONLY.
00385
00386  2003-EDIT-DATA.
00387
00388      IF  (PI-LANGUAGE-IS-FR
00389                   AND
00390              (FUNCTI = 'Q' OR 'q' OR 'V' OR 'v'))
00391              OR
00392          FUNCTI = 'Q' OR 'q' OR 'L' OR 'l'
00393          NEXT SENTENCE
00394
00395      ELSE
00396          IF  NOT MODIFY-CAP
00397              MOVE 'UPDATE'       TO SM-READ
00398              PERFORM 9995-SECURITY-VIOLATION
00399              MOVE ER-0070        TO EMI-ERROR
00400              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00401              GO TO 8200-SEND-DATAONLY.
00402
00403      IF  FUNCTL = ZEROS OR FUNCTI = SPACES
00404          GO TO 4000-CHANGE-ROUTINE
00405
00406      ELSE
00407          IF  (PI-LANGUAGE-IS-FR
00408                       AND
00409                  (FUNCTI = 'G' OR 'V' OR 'Q' OR
00410                            'I' OR 'A' OR 'E'  OR
00411                            'C' OR
00412                            'g' OR 'v' OR 'q' OR
00413                            'i' OR 'a' OR 'e'  OR
00414                            'c'))
00415                  OR
00416              (FUNCTI = 'S' OR 'D' OR 'Q' OR
00417                       'I' OR 'A' OR 'L'  OR
00418                        'c' OR 'C' OR
00419                        's' OR 'd' OR 'q' OR
00420                       'i' OR 'a' OR 'l')
00421              NEXT SENTENCE
00422          ELSE
00423              MOVE ER-0023 TO EMI-ERROR
00424              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00425              MOVE AL-UABON TO FUNCTA
00426              MOVE -1 TO FUNCTL
00427              GO TO 8200-SEND-DATAONLY.
00428
00429      IF  FUNCTI = 'c' OR 'C'
00430          PERFORM 2600-C-LINE-CHECK THRU 2600-EXIT
00431
00432      ELSE
00433          IF  (PI-LANGUAGE-IS-FR
00434                       AND
00435                  (FUNCTI = 'E' OR 'I' OR 'V' OR
00436                          'e' OR 'i' OR 'v'))
00437
00438                  OR
00439              FUNCTI = 'D' OR 'I' OR 'L' OR
00440                      'd' OR 'i' OR 'l'
00441              PERFORM 2500-LINE-CHECK THRU 2599-EXIT
00442
00443          ELSE
00444              IF  LINE1L NOT = ZEROS
00445                      OR
00446                  LINE2L NOT = ZEROS
00447                  MOVE ER-0030 TO EMI-ERROR
00448                  MOVE -1 TO LINE1L
00449                  MOVE AL-UNBON TO LINE1A LINE2A
00450                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00451                  GO TO 8200-SEND-DATAONLY.
00452
00453      IF  FUNCTI = 'A' OR 'a'
00454          GO TO 5000-ADD-NEW-LINES.
00455
00456      IF  FUNCTI = 'Q' OR 'q'
00457          GO TO 9410-RETURN.
00458
00459      IF  (PI-LANGUAGE-IS-FR
00460                   AND
00461              (FUNCTI = 'g' OR 'G'))
00462              OR
00463          FUNCTI = 's' OR 'S'
00464          GO TO 4500-SAVE-DATA.
00465
00466      IF  PI-TOTAL-LINES = 0
00467         MOVE ER-0048 TO EMI-ERROR
00468         MOVE -1 TO FUNCTL
00469         MOVE AL-UNBON TO FUNCTA
00470         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00471         GO TO 8200-SEND-DATAONLY.
00472
00473      IF  (PI-LANGUAGE-IS-FR
00474                   AND
00475              (FUNCTI = 'V' OR 'v'))
00476              OR
00477          FUNCTI = 'L' OR 'l'
00478          GO TO 5500-LOOKUP.
00479
00480      IF  FUNCTI = 'D' OR 'd'
00481          GO TO 3000-DELETE-LINES.
00482
00483      IF  FUNCTI = 'c' OR 'C'
00484          GO TO 3700-COPY-LINES.
00485
00486      GO TO 3500-INSERT-LINES.
00487      EJECT
00488  2500-LINE-CHECK.
00489      IF  LINE1L = ZEROS AND
00490         LINE2L = ZEROS
00491         MOVE ER-0069 TO EMI-ERROR
00492         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00493         MOVE -1 TO LINE1L
00494         GO TO 8200-SEND-DATAONLY.
00495
00496      IF  LINE1L NOT = ZEROS
00497         IF  LINE1I NOT NUMERIC OR
00498            LINE1I GREATER PI-TOTAL-LINES
00499            MOVE ER-0031 TO EMI-ERROR
00500            MOVE AL-UNBON TO LINE1A
00501            MOVE -1 TO LINE1L
00502            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00503            GO TO 8200-SEND-DATAONLY
00504           ELSE
00505            IF  LINE2L = ZEROS
00506               MOVE 1 TO LINE2I
00507              ELSE
00508               IF  FUNCTI = 'I' OR 'i'
00509                  GO TO 2510-MAX-CHECK
00510                 ELSE
00511                  IF  LINE2I NOT NUMERIC
00512                     MOVE AL-UNBON TO LINE2A
00513                     MOVE ER-0032 TO EMI-ERROR
00514                     MOVE -1      TO LINE2L
00515                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00516                     GO TO 8200-SEND-DATAONLY
00517                     ELSE
00518                     NEXT SENTENCE
00519        ELSE
00520         IF  LINE2L = ZEROS
00521            NEXT SENTENCE
00522           ELSE
00523            MOVE -1 TO LINE2L
00524            MOVE ER-0041 TO EMI-ERROR
00525            MOVE AL-UNBON TO LINE2A
00526            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00527            GO TO 8200-SEND-DATAONLY.
00528
00529      GO TO 2599-EXIT.
00530
00531  2510-MAX-CHECK.
00532      IF  LINE2I NOT NUMERIC
00533         MOVE -1 TO LINE2L
00534         MOVE ER-0032 TO EMI-ERROR
00535         MOVE AL-UNBON TO LINE2A
00536         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00537         GO TO 8200-SEND-DATAONLY
00538        ELSE
00539         COMPUTE ROLL-COUNTER = LINE2I + PI-TOTAL-LINES
00540         IF  ROLL-COUNTER GREATER THAN MAX-LINES
00541            MOVE -1 TO LINE2L
00542            MOVE ER-0044 TO EMI-ERROR
00543            MOVE AL-UNBON TO LINE2A
00544            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00545            GO TO 8200-SEND-DATAONLY.
00546
00547  2599-EXIT.
00548       EXIT.
00549      EJECT
00550
00551  2600-C-LINE-CHECK.
00552
00553      IF  LINE1L = ZEROS
00554          MOVE ER-0069 TO EMI-ERROR
00555          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00556          MOVE -1 TO LINE1L
00557          GO TO 8200-SEND-DATAONLY.
00558
00559      IF  LINE1I NOT NUMERIC
00560              OR
00561          LINE1I GREATER PI-TOTAL-LINES
00562          MOVE ER-0031 TO EMI-ERROR
00563          MOVE AL-UNBON TO LINE1A
00564          MOVE -1 TO LINE1L
00565          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00566          GO TO 8200-SEND-DATAONLY.
00567
00568      IF  LINE2L GREATER THAN ZEROS
00569          IF  LINE2I NOT NUMERIC
00570                  OR
00571              LINE2I GREATER PI-TOTAL-LINES
00572              MOVE ER-0032 TO EMI-ERROR
00573              MOVE AL-UNBON TO LINE2A
00574              MOVE -1 TO LINE2L
00575              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00576              GO TO 8200-SEND-DATAONLY.
00577
00578  2600-EXIT.
00579       EXIT.
00580      EJECT
00581  3000-DELETE-LINES.
00582      IF  LINE2L = ZEROS AND LINE2I = 1
00583         MOVE LINE1I TO LINE2I.
00584
00585      IF  PI-RETURN-TO-PROGRAM = PGM-EM152
00586              OR
00587          PI-RETURN-TO-PROGRAM = PGM-EL152
00588              OR
00589          PI-RETURN-TO-PROGRAM = PGM-EL689
00590
00591          IF  LINE1I LESS 8
00592                   AND
00593              PI-CREATE-LABELS
00594              MOVE ER-0212 TO EMI-ERROR
00595              MOVE AL-UNBON TO LINE1A
00596              MOVE -1 TO LINE1L
00597              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00598              GO TO 8200-SEND-DATAONLY.
00599
00600      IF  LINE2I GREATER PI-TOTAL-LINES OR LESS LINE1I
00601         MOVE ER-0049 TO EMI-ERROR
00602         MOVE AL-UNBON TO LINE2A
00603         MOVE -1 TO LINE2L
00604         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00605         GO TO 8200-SEND-DATAONLY.
00606
00607      PERFORM 7450-SET-INDX THRU 7450-EXIT.
00608
00609
00610      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
00611              VARYING SC-INDX FROM 1 BY 1 UNTIL
00612              SC-INDX GREATER NUM-LINES-PER-SCREEN.
00613
00614      IF  NOT EMI-NO-ERRORS
00615         GO TO 8200-SEND-DATAONLY.
00616
00617      SET TB-INDX TO LINE1I.
00618      COMPUTE ROLL-COUNTER = LINE2I - LINE1I + 1.
00619
00620      IF  LINE2I NOT = PI-TOTAL-LINES
00621         SET TB-INDX1 TO LINE2I
00622         SET TB-INDX1 UP BY 1
00623         PERFORM 3100-DELETE-TABLE-ENTRIES
00624                 UNTIL TB-INDX1 GREATER PI-TOTAL-LINES.
00625
00626      PERFORM 3150-BLANK-TABLE-ENTRIES
00627              ROLL-COUNTER TIMES.
00628
00629      SUBTRACT ROLL-COUNTER FROM PI-TOTAL-LINES.
00630
00631      IF  PI-CURRENT-LINE GREATER PI-TOTAL-LINES
00632         MOVE PI-TOTAL-LINES       TO PI-CURRENT-LINE
00633         SUBTRACT 1 FROM PI-CURRENT-LINE.
00634
00635      SET TB-INDX  TO PI-CURRENT-LINE.
00636      MOVE LOW-VALUES TO EL104BI.
00637
00638      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT
00639             VARYING SC-INDX FROM 1 BY 1 UNTIL
00640             SC-INDX GREATER NUM-LINES-PER-SCREEN.
00641
00642      MOVE 1 TO PI-UPDATE-SW.
00643
00644      IF  PI-TOTAL-LINES = ZEROS
00645         MOVE ZEROS TO PI-CURRENT-LINE.
00646
00647      GO TO 8100-SEND-INITIAL-MAP.
00648      EJECT
00649  3100-DELETE-TABLE-ENTRIES.
00650      MOVE REC-ENT (TB-INDX1) TO REC-ENT (TB-INDX).
00651      SET TB-INDX TB-INDX1 UP BY 1.
00652
00653  3150-BLANK-TABLE-ENTRIES.
00654      MOVE SPACES TO REC-ENT (TB-INDX).
00655      SET TB-INDX UP BY 1.
00656      EJECT
00657  3500-INSERT-LINES.
00658
00659      IF  PI-RETURN-TO-PROGRAM = PGM-EM152
00660              OR
00661          PI-RETURN-TO-PROGRAM = PGM-EL152
00662              OR
00663          PI-RETURN-TO-PROGRAM = PGM-EL689
00664
00665          IF  LINE1I LESS 7
00666                  AND
00667              PI-CREATE-LABELS
00668              MOVE ER-0212  TO EMI-ERROR
00669              MOVE AL-UNBON TO LINE1A
00670              MOVE -1       TO LINE1L
00671              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00672              GO TO 8200-SEND-DATAONLY.
00673
00674      PERFORM 7450-SET-INDX THRU 7450-EXIT.
00675
00676      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
00677              VARYING SC-INDX FROM 1 BY 1 UNTIL
00678              SC-INDX GREATER NUM-LINES-PER-SCREEN.
00679
00680      IF  NOT EMI-NO-ERRORS
00681         GO TO 8200-SEND-DATAONLY.
00682
00683      SET TB-INDX  TO PI-TOTAL-LINES.
00684      ADD LINE2I   TO PI-TOTAL-LINES.
00685      SET TB-INDX1 TO PI-TOTAL-LINES.
00686
00687      PERFORM 3600-INSERT-TABLE-ENTRIES
00688              UNTIL TB-INDX = LINE1I.
00689
00690      SET TB-INDX UP BY 1.
00691
00692      COMPUTE ROLL-COUNTER =
00693                       PI-CURRENT-LINE + NUM-LINES-PER-SCREEN.
00694
00695      IF  TB-INDX NOT LESS ROLL-COUNTER OR
00696                     LESS PI-CURRENT-LINE
00697         SET SC-INDX TO 1
00698         SET SC-INDX DOWN BY 1
00699        ELSE
00700         SET ROLL-COUNTER TO TB-INDX
00701         COMPUTE ROLL-COUNTER = ROLL-COUNTER - PI-CURRENT-LINE + 1
00702         SET SC-INDX TO ROLL-COUNTER.
00703
00704      PERFORM 3150-BLANK-TABLE-ENTRIES LINE2I TIMES.
00705      SET TB-INDX     TO PI-CURRENT-LINE.
00706      MOVE LOW-VALUES TO EL104BI.
00707
00708      IF  SC-INDX NOT = ZERO
00709         MOVE -1 TO SC-TEXTL (SC-INDX).
00710
00711      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT
00712             VARYING SC-INDX FROM 1 BY 1 UNTIL
00713             SC-INDX GREATER NUM-LINES-PER-SCREEN.
00714
00715      MOVE 1 TO PI-UPDATE-SW.
00716      GO TO 8100-SEND-INITIAL-MAP.
00717
00718  3600-INSERT-TABLE-ENTRIES.
00719      MOVE REC-ENT (TB-INDX)       TO REC-ENT (TB-INDX1).
00720      SET TB-INDX TB-INDX1 DOWN BY 1.
00721      EJECT
00722  3700-COPY-LINES.
00723
00724      IF  PI-RETURN-TO-PROGRAM = PGM-EM152
00725              OR
00726          PI-RETURN-TO-PROGRAM = PGM-EL152
00727              OR
00728          PI-RETURN-TO-PROGRAM = PGM-EL689
00729
00730          IF  LINE1I LESS 7
00731                  AND
00732              PI-CREATE-LABELS
00733              MOVE ER-0212        TO EMI-ERROR
00734              MOVE AL-UNBON       TO LINE1A
00735              MOVE -1             TO LINE1L
00736              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00737              GO TO 8200-SEND-DATAONLY.
00738
00739      PERFORM 7450-SET-INDX THRU 7450-EXIT.
00740
00741      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
00742              VARYING
00743          SC-INDX FROM 1 BY 1
00744              UNTIL
00745          SC-INDX GREATER NUM-LINES-PER-SCREEN.
00746
00747      IF  NOT EMI-NO-ERRORS
00748         GO TO 8200-SEND-DATAONLY.
00749
00750      SET TB-INDX                  TO LINE1I.
00751      MOVE REC-ENT (TB-INDX)       TO W-HOLD-LINE.
00752      SET TB-INDX                  TO PI-TOTAL-LINES.
00753      ADD +1                       TO PI-TOTAL-LINES.
00754      SET TB-INDX1                 TO PI-TOTAL-LINES.
00755
00756      IF  LINE2L NOT GREATER THAN ZEROS
00757          MOVE LINE1I              TO LINE2I.
00758
00759      PERFORM 3720-INSERT-TABLE-ENTRIES
00760              UNTIL
00761          TB-INDX = LINE2I.
00762
00763      SET TB-INDX UP BY 1.
00764
00765      COMPUTE ROLL-COUNTER
00766          = PI-CURRENT-LINE + NUM-LINES-PER-SCREEN.
00767
00768      IF  TB-INDX NOT LESS ROLL-COUNTER
00769              OR
00770          TB-INDX LESS PI-CURRENT-LINE
00771          SET SC-INDX              TO 1
00772          SET SC-INDX DOWN BY 1
00773
00774      ELSE
00775          SET ROLL-COUNTER TO TB-INDX
00776          COMPUTE ROLL-COUNTER = ROLL-COUNTER - PI-CURRENT-LINE + 1
00777          SET SC-INDX             TO ROLL-COUNTER.
00778
00779      MOVE W-HOLD-LINE            TO REC-ENT (TB-INDX).
00780      SET TB-INDX                 TO PI-CURRENT-LINE.
00781      MOVE LOW-VALUES             TO EL104BI.
00782
00783      IF  SC-INDX NOT = ZERO
00784         MOVE -1                  TO SC-TEXTL (SC-INDX).
00785
00786      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT
00787              VARYING
00788          SC-INDX FROM 1 BY 1
00789              UNTIL
00790          SC-INDX GREATER NUM-LINES-PER-SCREEN.
00791
00792      MOVE 1                      TO PI-UPDATE-SW.
00793      GO TO 8100-SEND-INITIAL-MAP.
00794
00795  3720-INSERT-TABLE-ENTRIES.
00796
00797      MOVE REC-ENT (TB-INDX)      TO REC-ENT (TB-INDX1).
00798      SET TB-INDX TB-INDX1 DOWN BY 1.
00799
00800      EJECT
00801  4000-CHANGE-ROUTINE.
00802
00803      IF  ARCHBL GREATER THAN ZEROS
00804              AND
00805          (CREDIT-SESSION
00806                   OR
00807              MORTGAGE-SESSION)
00808
00809          IF  ARCHBI EQUAL 'Y'
00810                  OR
00811              ARCHBI EQUAL SPACES
00812              MOVE ARCHBI         TO PI-1042-ARCHIVE-IND
00813
00814          ELSE
00815              MOVE ER-7375        TO EMI-ERROR
00816              MOVE -1             TO ARCHBL
00817              MOVE AL-UABON       TO ARCHBA
00818              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00819
00820      IF  ARCHBL GREATER THAN ZEROS
00821              AND
00822          CLAIM-SESSION
00823              AND
00824          PI-COMPANY-ID = 'DMD'
00825
00826          IF ARCHBI = 'B' OR ' ' OR 'b'
00827              INSPECT ARCHBI CONVERTING LOWER-CASE TO
00828                                        UPPER-CASE
00829              MOVE ARCHBI         TO PI-1042-ARCHIVE-IND
00830              MOVE 1              TO PI-UPDATE-SW
00831              MOVE AL-UANON       TO ARCHBA
00832          ELSE
00833              MOVE ER-8126        TO EMI-ERROR
00834              MOVE -1             TO ARCHBL
00835              MOVE AL-UABON       TO ARCHBA
00836              MOVE 'Y'            TO WS-8126-ERROR-SW
00837              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00838              GO TO 8200-SEND-DATAONLY.
00839
00840      IF  FORMSQBL GREATER THAN ZEROS
00841              AND
00842          (MORTGAGE-SESSION
00843                  OR
00844              CREDIT-SESSION)
00845
00846          IF  FORMSQBI EQUAL 'Y'
00847                  OR
00848              FORMSQBI EQUAL SPACES
00849              MOVE FORMSQBI       TO PI-FORM-SQUEEZE-CONTROL
00850
00851          ELSE
00852              MOVE ER-9349        TO EMI-ERROR
00853              MOVE -1             TO FORMSQBL
00854              MOVE AL-UABON       TO FORMSQBA
00855              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00856
00857      PERFORM 7450-SET-INDX THRU 7450-EXIT.
00858      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
00859              VARYING SC-INDX FROM 1 BY 1 UNTIL
00860              SC-INDX GREATER NUM-LINES-PER-SCREEN.
00861
00862      IF  NOT EMI-NO-ERRORS
00863         GO TO 8200-SEND-DATAONLY.
00864
00865      MOVE SPACES TO ERRMSGBO
00866      GO TO 8200-SEND-DATAONLY.
00867
00868      EJECT
00869  4500-SAVE-DATA.
00870
00871      IF  ARCHBL GREATER THAN ZEROS
00872              AND
00873          CREDIT-SESSION
00874
00875          IF  ARCHBI EQUAL 'Y'
00876                  OR
00877              ARCHBI EQUAL SPACES
00878              MOVE ARCHBI       TO PI-1042-ARCHIVE-IND
00879
00880          ELSE
00881              MOVE ER-7375      TO EMI-ERROR
00882              MOVE -1           TO ARCHBL
00883              MOVE AL-UABON     TO ARCHBA
00884              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00885
00886      IF  ARCHBL GREATER THAN ZEROS
00887              AND
00888          CLAIM-SESSION
00889              AND
00890          PI-COMPANY-ID = 'DMD'
00891
00892          IF ARCHBI = 'B' OR ' ' OR 'b'
00893              INSPECT ARCHBI CONVERTING LOWER-CASE TO
00894                                        UPPER-CASE
00895              MOVE ARCHBI         TO PI-1042-ARCHIVE-IND
00896              MOVE 1              TO PI-UPDATE-SW
00897              MOVE AL-UANON      TO ARCHBA
00898          ELSE
00899              MOVE ER-8126        TO EMI-ERROR
00900              MOVE -1             TO ARCHBL
00901              MOVE AL-UABON       TO ARCHBA
00902              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00903              GO TO 8200-SEND-DATAONLY.
00904
00905      IF  FORMSQBL GREATER THAN ZEROS
00906              AND
00907          (MORTGAGE-SESSION
00908                  OR
00909              CREDIT-SESSION)
00910
00911          IF  FORMSQBI EQUAL 'Y'
00912                  OR
00913              FORMSQBI EQUAL SPACES
00914              MOVE FORMSQBI     TO PI-FORM-SQUEEZE-CONTROL
00915
00916          ELSE
00917              MOVE ER-9349      TO EMI-ERROR
00918              MOVE -1           TO FORMSQBL
00919              MOVE AL-UABON     TO FORMSQBA
00920              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00921
00922      PERFORM 7450-SET-INDX THRU 7450-EXIT.
00923
00924      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
00925              VARYING SC-INDX FROM 1 BY 1 UNTIL
00926              SC-INDX GREATER NUM-LINES-PER-SCREEN.
00927
00928      IF  NOT EMI-NO-ERRORS
00929         GO TO 8200-SEND-DATAONLY.
00930
00931      IF  PI-RETURN-TO-PROGRAM = PGM-EM152
00932              OR
00933          PI-RETURN-TO-PROGRAM = PGM-EL152
00934              OR
00935          PI-RETURN-TO-PROGRAM = PGM-EL689
00936
00937          PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT
00938          GO TO 9410-RETURN.
00939
00940      
      * EXEC CICS HANDLE CONDITION
00941 *         NOTFND  (4610-ENDBR)
00942 *         NOTOPEN (6000-NOT-OPEN)
00943 *         ENDFILE (4610-ENDBR)
00944 *     END-EXEC.
      *    MOVE '"$IJ''                 ! $ #00003587' TO DFHEIV0
           MOVE X'2224494A2720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303033353837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00945
00946      
      * EXEC CICS DELETE
00947 *         DATASET   (FILE-ID)
00948 *         RIDFLD    (FILE-KEY)
00949 *         KEYLENGTH (13)
00950 *         GENERIC
00951 *     END-EXEC.
           MOVE 13
             TO DFHEIV11
      *    MOVE '&(  RKG               &   #00003593' TO DFHEIV0
           MOVE X'26282020524B472020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID, 
                 FILE-KEY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00952
00953  4610-ENDBR.
00954      
      * EXEC CICS GETMAIN
00955 *         SET(ADDRESS OF TEXT-FILES)
00956 *         LENGTH(FILE-LENGTH)
00957 *     END-EXEC.
      *    MOVE '," L                  $   #00003601' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 FILE-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF TEXT-FILES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00958
00959      PERFORM 4700-WRITE-FILE THRU 4799-EXIT
00960              VARYING TB-INDX FROM 1 BY 1 UNTIL
00961              TB-INDX GREATER PI-TOTAL-LINES.
00962
00963      GO TO 9410-RETURN.
00964
00965  4700-WRITE-FILE.
00966      ADD 1 TO SEQ.
00967      MOVE FILE-KEY TO TX-CONTROL-PRIMARY.
00968
00969      IF  PI-ENTRY-CD-1 = '1'
00970          MOVE  'TL' TO TEXT-FILE-ID
00971      ELSE
00972          IF  PI-ENTRY-CD-1 = '2'
00973              MOVE 'TF' TO TEXT-FILE-ID
00974          ELSE
00975              MOVE 'TH' TO TEXT-FILE-ID.
00976
00977      MOVE REC-PC (TB-INDX)   TO TX-PROCESS-CONTROL.
00978      MOVE REC-SQ (TB-INDX)   TO TX-LINE-SQUEEZE-CONTROL.
00979      MOVE REC-TEXT (TB-INDX) TO TX-TEXT-LINE.
00980      MOVE PI-PROCESSOR-ID    TO TX-LAST-MAINTENANCED-BY
00981                                 MNTBYO.
00982      MOVE SAVE-BIN-DATE      TO TX-LAST-MAINTENANCED-DT.
00983      MOVE SAVE-DATE          TO MNTONO.
00984
00985      IF  MORTGAGE-SESSION
00986              OR
00987          CREDIT-SESSION
00988          MOVE PI-FORM-SQUEEZE-CONTROL
00989                              TO TX-FORM-SQUEEZE-CONTROL
00990          MOVE PI-1042-ARCHIVE-IND
00991                              TO TX-ARCHIVE-SW.
00992
00993      IF  PI-COMPANY-ID = 'DMD'
00994              AND
00995          CLAIM-SESSION
00996          MOVE PI-1042-ARCHIVE-IND
00997                              TO TX-BSR-CODE.
00998
00999      IF  TX-PROCESS-CONTROL = SPACES
01000         MOVE ZEROS TO TX-PROCESS-CONTROL.
01001
01002      
      * EXEC CICS WRITE
01003 *         DATASET (FILE-ID)
01004 *         FROM    (TEXT-FILES)
01005 *         RIDFLD  (FILE-KEY)
01006 *     END-EXEC.
           MOVE LENGTH OF
            TEXT-FILES
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003649' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID, 
                 TEXT-FILES, 
                 DFHEIV11, 
                 FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01007  4799-EXIT.
01008       EXIT.
01009
01010      EJECT
01011  5000-ADD-NEW-LINES.
01012
01013      IF  ARCHBL GREATER THAN ZEROS
01014              AND
01015          (CREDIT-SESSION
01016                  OR
01017              MORTGAGE-SESSION)
01018
01019          IF  ARCHBI EQUAL 'Y'
01020                  OR
01021              ARCHBI EQUAL SPACES
01022              MOVE ARCHBI       TO PI-1042-ARCHIVE-IND
01023
01024          ELSE
01025              MOVE ER-7375      TO EMI-ERROR
01026              MOVE -1           TO ARCHBL
01027              MOVE AL-UABON     TO ARCHBA
01028              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01029 *
01030 *    ELSE
01031 *        MOVE SPACES           TO PI-1042-ARCHIVE-IND.
01032
01033      IF  ARCHBL GREATER THAN ZEROS
01034              AND
01035          CLAIM-SESSION
01036              AND
01037          PI-COMPANY-ID = 'DMD'
01038
01039          IF ARCHBI = 'B' OR ' ' OR 'b'
01040              INSPECT ARCHBI CONVERTING LOWER-CASE TO
01041                                        UPPER-CASE
01042              MOVE ARCHBI         TO PI-1042-ARCHIVE-IND
01043              MOVE 1              TO PI-UPDATE-SW
01044              MOVE AL-UANON      TO ARCHBA
01045          ELSE
01046              MOVE ER-8126        TO EMI-ERROR
01047              MOVE -1             TO ARCHBL
01048              MOVE AL-UABON       TO ARCHBA
01049              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01050              GO TO 8200-SEND-DATAONLY.
01051
01052
01053      IF  FORMSQBL GREATER THAN ZEROS
01054              AND
01055          (MORTGAGE-SESSION
01056                  OR
01057              CREDIT-SESSION)
01058
01059          IF  FORMSQBI EQUAL 'Y'
01060                  OR
01061              FORMSQBI EQUAL SPACES
01062              MOVE FORMSQBI       TO PI-FORM-SQUEEZE-CONTROL
01063
01064          ELSE
01065              MOVE ER-9349        TO EMI-ERROR
01066              MOVE -1             TO FORMSQBL
01067              MOVE AL-UABON       TO FORMSQBA
01068              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01069 *
01070 *    ELSE
01071 *        MOVE SPACES             TO PI-FORM-SQUEEZE-CONTROL.
01072
01073      PERFORM 7450-SET-INDX THRU 7450-EXIT.
01074
01075      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
01076              VARYING SC-INDX FROM 1 BY 1 UNTIL
01077              SC-INDX GREATER NUM-LINES-PER-SCREEN
01078
01079      IF  NOT EMI-NO-ERRORS
01080         GO TO 8200-SEND-DATAONLY.
01081
01082      IF  PI-TOTAL-LINES EQUAL ZEROS
01083          MOVE LOW-VALUES TO EL104BI
01084          MOVE AL-UANON   TO FUNCTA
01085          MOVE 'A'        TO FUNCTI
01086          GO TO 8100-SEND-INITIAL-MAP.
01087
01088      MOVE PI-TOTAL-LINES
01089                      TO PI-CURRENT-LINE.
01090
01091      MOVE LOW-VALUES TO EL104BI.
01092      SET TB-INDX     TO PI-CURRENT-LINE.
01093      MOVE 'A'        TO FUNCTI.
01094      MOVE -1         TO SC-TEXTL (2).
01095      MOVE AL-UANON   TO FUNCTA.
01096
01097      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT
01098              VARYING SC-INDX FROM 1 BY 1 UNTIL
01099              SC-INDX GREATER NUM-LINES-PER-SCREEN.
01100
01101      GO TO 8100-SEND-INITIAL-MAP.
01102      EJECT
01103  5500-LOOKUP.
01104      SET TB-INDX TO PI-CURRENT-LINE.
01105
01106      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
01107              VARYING SC-INDX FROM 1 BY 1 UNTIL
01108              SC-INDX GREATER NUM-LINES-PER-SCREEN.
01109
01110      IF  NOT EMI-NO-ERRORS
01111         GO TO 8200-SEND-DATAONLY.
01112
01113      MOVE LINE1I TO PI-CURRENT-LINE.
01114      SET TB-INDX TO PI-CURRENT-LINE.
01115      MOVE LOW-VALUES TO EL104BI.
01116
01117      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT
01118              VARYING SC-INDX FROM 1 BY 1
01119              UNTIL SC-INDX GREATER NUM-LINES-PER-SCREEN.
01120
01121      GO TO 8100-SEND-INITIAL-MAP.
01122      EJECT
01123  6000-NOT-OPEN.
01124      IF  PI-ENTRY-CD-1 = '1'
01125          MOVE ER-0013 TO EMI-ERROR
01126      ELSE
01127          IF  PI-ENTRY-CD-1 = '2'
01128              MOVE ER-0014 TO EMI-ERROR
01129          ELSE
01130              MOVE ER-0015 TO EMI-ERROR.
01131
01132      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01133      GO TO 8200-SEND-DATAONLY.
01134      EJECT
01135  7000-BUILD-TABLE.
01136      SET TB-INDX TO 1.
01137      MOVE ZEROS TO PI-TOTAL-LINES
01138                    PI-CURRENT-LINE
01139                    PI-TEMP-STOR-ITEMS
01140                    PI-UPDATE-SW.
01141
01142      MOVE LOW-VALUES TO EL104BI.
01143      PERFORM 7500-READ-TS THRU 7599-EXIT.
01144
01145      IF  PI-TEMP-STOR-ITEMS NOT = ZERO
01146         MOVE ER-0140 TO EMI-ERROR
01147         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01148         MULTIPLY PI-TEMP-STOR-ITEMS BY TS-NUM-REC-IN-GROUP GIVING
01149                  PI-TOTAL-LINES
01150         MOVE 1 TO PI-CURRENT-LINE
01151         SET TB-INDX TO 1
01152         PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT
01153                 VARYING SC-INDX FROM 1
01154                 BY 1 UNTIL SC-INDX GREATER NUM-LINES-PER-SCREEN
01155         GO TO 8100-SEND-INITIAL-MAP.
01156
01157      
      * EXEC CICS HANDLE CONDITION
01158 *         NOTFND (7010-ENDBR)
01159 *         NOTOPEN(6000-NOT-OPEN)
01160 *         ENDFILE(7010-ENDBR)
01161 *     END-EXEC.
      *    MOVE '"$IJ''                 ! % #00003804' TO DFHEIV0
           MOVE X'2224494A2720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303033383034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01162
01163      
      * EXEC CICS STARTBR
01164 *         DATASET(FILE-ID)
01165 *         RIDFLD (FILE-KEY)
01166 *         KEYLENGTH(13)
01167 *         GENERIC
01168 *         GTEQ
01169 *     END-EXEC.
           MOVE 13
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    G          &   #00003810' TO DFHEIV0
           MOVE X'262C2020204B472020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID, 
                 FILE-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01170
01171  7001-LOOP.
01172      
      * EXEC CICS READNEXT
01173 *         SET    (ADDRESS OF TEXT-FILES)
01174 *         DATASET(FILE-ID)
01175 *         RIDFLD (FILE-KEY)
01176 *     END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00003819' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383139' TO DFHEIV0(25:11)
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
           
01177
01178      IF  FILE-PARTIAL-KEY NOT EQUAL OLD-KEY-SAVE
01179          GO TO 7010-ENDBR.
01180
01181      MOVE TX-PROCESS-CONTROL     TO REC-PC (TB-INDX).
01182      PERFORM 7150-TEST-SQUEEZE-LINE THRU 7150-EXIT.
01183      MOVE TX-TEXT-LINE           TO REC-TEXT (TB-INDX).
01184      MOVE TX-FORM-SQUEEZE-CONTROL
01185                                  TO PI-FORM-SQUEEZE-CONTROL.
01186
01187      IF  PI-COMPANY-ID = 'DMD'
01188              AND
01189          CLAIM-SESSION
01190          MOVE TX-BSR-CODE        TO PI-1042-ARCHIVE-IND
01191
01192      ELSE
01193          MOVE TX-ARCHIVE-SW      TO PI-1042-ARCHIVE-IND.
01194
01195      IF  TX-LAST-MAINTENANCED-BY GREATER THAN SPACES
01196          MOVE TX-LAST-MAINTENANCED-BY
01197                                  TO W-SAVE-LAST-MAINT-BY
01198          MOVE TX-LAST-MAINTENANCED-DT
01199                                  TO DC-BIN-DATE-1
01200          MOVE ' '                TO DC-OPTION-CODE
01201          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01202
01203          IF  NO-CONVERSION-ERROR
01204              MOVE DC-GREG-DATE-1-EDIT
01205                                  TO W-SAVE-LAST-MAINT-DT
01206
01207          ELSE
01208              MOVE SPACES         TO W-SAVE-LAST-MAINT-DT
01209                                     W-SAVE-LAST-MAINT-BY.
01210
01211      SET TB-INDX UP BY 1
01212      GO TO 7001-LOOP.
01213
01214  7010-ENDBR.
01215      IF  TB-INDX = 1
01216         MOVE ER-0006  TO EMI-ERROR
01217         MOVE 'A'      TO FUNCTI
01218         MOVE -1       TO SC-TEXTL (1)
01219         MOVE AL-UANON TO FUNCTA
01220         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01221         GO TO 8100-SEND-INITIAL-MAP.
01222
01223      
      * EXEC CICS ENDBR
01224 *         DATASET(FILE-ID)
01225 *     END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003870' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01226
01227      SET TB-INDX DOWN BY 1.
01228      SET PI-TOTAL-LINES TO TB-INDX.
01229      MOVE 1 TO PI-CURRENT-LINE.
01230
01231  7050-FORMAT-LINES.
01232
01233      SET TB-INDX TO PI-CURRENT-LINE.
01234
01235      IF  MORTGAGE-SESSION
01236              OR
01237          CREDIT-SESSION
01238          IF  PI-1042-ARCHIVE-IND EQUAL 'Y'
01239              MOVE PI-1042-ARCHIVE-IND
01240                                 TO ARCHBO.
01241
01242      IF  CLAIM-SESSION
01243              AND
01244          PI-COMPANY-ID = 'DMD'
01245          IF PI-1042-ARCHIVE-IND = 'B' OR ' '
01246              MOVE AL-UANON      TO ARCHBA
01247              MOVE PI-1042-ARCHIVE-IND
01248                                 TO ARCHBO
090505         END-IF
090505     END-IF
01249
090505*        ELSE
090505*            MOVE SPACES        TO PI-1042-ARCHIVE-IND
090505*                                  ARCHBO.
01253
01254      IF  MORTGAGE-SESSION
01255              OR
01256          CREDIT-SESSION
01257          IF  PI-FORM-SQUEEZE-CONTROL EQUAL 'Y'
01258              MOVE PI-FORM-SQUEEZE-CONTROL
01259                                 TO FORMSQBO.
01260
01261      MOVE W-SAVE-LAST-MAINT-DT  TO MNTONO.
01262      MOVE W-SAVE-LAST-MAINT-BY  TO MNTBYO.
01263
01264      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT
01265              VARYING SC-INDX FROM 1
01266              BY 1 UNTIL SC-INDX GREATER NUM-LINES-PER-SCREEN.
01267
01268      GO TO 8100-SEND-INITIAL-MAP.
01269      EJECT
01270  7100-FORMAT-SCREEN.
           IF REC-SQ (TB-INDX) = 'Z'
              MOVE AL-PANOF            TO SC-TEXTA (SC-INDX)
           END-IF
01272      IF  TB-INDX NOT GREATER PI-TOTAL-LINES
01273          MOVE REC-TEXT (TB-INDX) TO SC-TEXT (SC-INDX)
01274          MOVE REC-PC (TB-INDX)   TO SC-PC (SC-INDX)
01275          PERFORM 7151-TEST-SQUEEZE-LINE THRU 7151-EXIT
01276          SET ROLL-COUNTER        TO TB-INDX
01277          MOVE ROLL-COUNTER       TO SC-LIN (SC-INDX)
01278          SET TB-INDX UP BY 1
01279
01280      ELSE
01281          IF  FUNCTI = 'A' OR 'a'
01282
01283              IF  CLAIM-SESSION
01284                      AND
01285                  TB-INDX GREATER THAN +1
01286                  MOVE AL-PANOF   TO SC-SQA (SC-INDX)
01287
01288              ELSE
01289                  NEXT SENTENCE
01290
01291          ELSE
01292              MOVE AL-PANOF       TO SC-TEXTA (SC-INDX)
01293                                     SC-PCA (SC-INDX)
01294                                     SC-SQA (SC-INDX).
01295
01296      IF  PI-RETURN-TO-PROGRAM = PGM-EM152
01297              OR
01298          PI-RETURN-TO-PROGRAM = PGM-EL152
01299              OR
01300          PI-RETURN-TO-PROGRAM = PGM-EL689
01301
01302          MOVE SPACES   TO SC-PC (SC-INDX)
01303          MOVE AL-PANOF TO SC-PCA (SC-INDX)
01304          MOVE SPACES   TO SC-SQ (SC-INDX)
01305          MOVE AL-PANOF TO SC-SQA (SC-INDX).
01306
01307  7100-EXIT.
01308       EXIT.
01309
01310  7150-TEST-SQUEEZE-LINE.
01311
01312      MOVE TX-LINE-SQUEEZE-CONTROL
01313                                  TO W-LINE-SQUEEZE-IND.
01314
01315      IF  W-LINE-SQUEEZE-VALID-VALUE
01316          MOVE TX-LINE-SQUEEZE-CONTROL
01317                              TO REC-SQ (TB-INDX)
01318
01319      ELSE
01320          MOVE SPACES         TO REC-SQ (TB-INDX).
01321
01322
01323  7150-EXIT.
01324       EXIT.
01325
01326  7151-TEST-SQUEEZE-LINE.
01327
01328      MOVE REC-SQ (TB-INDX)   TO W-LINE-SQUEEZE-IND.
01329
01330      IF  W-LINE-SQUEEZE-VALID-VALUE
01331          NEXT SENTENCE
01332
01333      ELSE
01334          MOVE SPACES         TO REC-SQ (TB-INDX).
01335
01336      IF  MORTGAGE-SESSION
01337              OR
01338          CREDIT-SESSION
01339          MOVE REC-SQ (TB-INDX)
01340                              TO SC-SQ (SC-INDX)
01341
01342      ELSE
01343          IF  CLAIM-SESSION
01344                  AND
01345              TB-INDX EQUAL +1
01346              MOVE REC-SQ (TB-INDX)
01347                              TO SC-SQ (SC-INDX)
01348
01349          ELSE
01350              MOVE ZEROS      TO SC-SQL (SC-INDX)
01351              MOVE AL-SADOF   TO SC-SQA (SC-INDX)
01352              MOVE LOW-VALUES TO SC-SQ  (SC-INDX).
01353
01354  7151-EXIT.
01355       EXIT.
01356
01357  7200-PUT-TEMP-STOR.
01358      PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT.
01359      SET TS-INDX TO 1.
01360      MOVE 0 TO PI-TEMP-STOR-ITEMS.
01361
01362      PERFORM 7300-WRITE-TS THRU 7399-EXIT
01363              VARYING TS-GROUP-WORK FROM 0 BY TS-NUM-REC-IN-GROUP
01364              UNTIL TS-GROUP-WORK NOT LESS PI-TOTAL-LINES.
01365  7249-EXIT.
01366       EXIT.
01367
01368  7250-DELETE-TEMP-STOR.
01369      
      * EXEC CICS HANDLE CONDITION
01370 *         QIDERR(7299-EXIT)
01371 *     END-EXEC.
      *    MOVE '"$N                   ! & #00004020' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034303230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01372
01373      
      * EXEC CICS DELETEQ TS
01374 *         QUEUE(TS-NAME)
01375 *     END-EXEC.
      *    MOVE '*&                    #   #00004024' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-NAME, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01376
01377  7299-EXIT.
01378      EXIT.
01379      EJECT
01380  7300-WRITE-TS.
01381      MOVE TS-GROUP (TS-INDX) TO TS-WORK-AREA.
01382      SET TS-INDX UP BY 1.
01383      ADD 1 TO PI-TEMP-STOR-ITEMS.
01384
01385      
      * EXEC CICS WRITEQ TS
01386 *         FROM  (TS-WORK-AREA)
01387 *         QUEUE (TS-NAME)
01388 *         LENGTH(TS-LENGTH)
01389 *         ITEM  (PI-TEMP-STOR-ITEMS)
01390 *     END-EXEC.
      *    MOVE '*" I                  ''   #00004036' TO DFHEIV0
           MOVE X'2A2220492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-NAME, 
                 TS-WORK-AREA, 
                 TS-LENGTH, 
                 PI-TEMP-STOR-ITEMS, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01391
01392  7399-EXIT.
01393      EXIT.
01394      EJECT
01395  7400-PAGE-ROUTINE.
01396      IF  PFENTRBL NOT = ZEROS
01397         MOVE -1 TO PFENTRBL
01398        ELSE
01399         MOVE -1 TO FUNCTL.
01400
01401      IF  PI-TOTAL-LINES = 0
01402         MOVE ER-0047 TO EMI-ERROR
01403         MOVE -1 TO FUNCTL
01404         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01405         GO TO 8200-SEND-DATAONLY.
01406
01407      COMPUTE TEMP-CURR-LINE = PI-CURRENT-LINE + ROLL-COUNTER.
01408
01409      IF  TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS
01410         MOVE ER-0067 TO EMI-ERROR
01411         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01412         MOVE 1 TO TEMP-CURR-LINE.
01413
01414      IF  TEMP-CURR-LINE GREATER PI-TOTAL-LINES
01415         MOVE ER-0066 TO EMI-ERROR
01416         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01417         COMPUTE TEMP-CURR-LINE = PI-TOTAL-LINES + 1
01418                                - NUM-LINES-PER-SCREEN
01419         IF  TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS
01420            MOVE 1 TO TEMP-CURR-LINE.
01421
01422      PERFORM 7450-SET-INDX THRU 7450-EXIT.
01423
01424      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
01425              VARYING SC-INDX FROM 1 BY 1 UNTIL
01426              SC-INDX GREATER NUM-LINES-PER-SCREEN.
01427
01428      IF  EMI-ERROR = ER-0066 OR ER-0067 OR ZEROS
01429         NEXT SENTENCE
01430        ELSE
01431         GO TO 8200-SEND-DATAONLY.
01432
01433      MOVE TEMP-CURR-LINE TO PI-CURRENT-LINE.
01434      SET TB-INDX         TO PI-CURRENT-LINE.
01435      MOVE LOW-VALUES     TO EL104BI.
01436
01437      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT
01438              VARYING SC-INDX FROM 1 BY 1
01439              UNTIL SC-INDX GREATER NUM-LINES-PER-SCREEN
01440
01441      IF  CREDIT-SESSION
01442          IF  PI-1042-ARCHIVE-IND EQUAL 'Y'
01443              MOVE PI-1042-ARCHIVE-IND
01444                                 TO ARCHBO.
01445
01446      IF  CLAIM-SESSION
01447              AND
01448          PI-COMPANY-ID = 'DMD'
01449          IF PI-1042-ARCHIVE-IND = 'B' OR ' '
01450              MOVE AL-UANON      TO ARCHBA
01451              MOVE PI-1042-ARCHIVE-IND
01452                                 TO ARCHBO.
01453
01454      IF  MORTGAGE-SESSION
01455              OR
01456          CREDIT-SESSION
01457          IF  PI-FORM-SQUEEZE-CONTROL EQUAL 'Y'
01458              MOVE PI-FORM-SQUEEZE-CONTROL
01459                                  TO FORMSQBO.
01460
01461      GO TO 8100-SEND-INITIAL-MAP.
01462      EJECT
01463  7450-SET-INDX.
01464      IF  PI-CURRENT-LINE = 0
01465         SET TB-INDX TO 1
01466        ELSE
01467         SET TB-INDX TO PI-CURRENT-LINE.
01468
01469  7450-EXIT.
01470       EXIT.
01471      EJECT
01472  7500-READ-TS.
01473      
      * EXEC CICS HANDLE CONDITION
01474 *         QIDERR (7590-TS-QIDERR)
01475 *         ITEMERR(7585-TS-ITEMERR)
01476 *     END-EXEC.
      *    MOVE '"$N<                  ! '' #00004124' TO DFHEIV0
           MOVE X'22244E3C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303034313234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01477
01478      IF  CREDIT-SESSION
01479          IF  PI-1042-ARCHIVE-IND EQUAL 'Y'
01480              MOVE PI-1042-ARCHIVE-IND
01481                                 TO ARCHBO.
01482
01483      IF  CLAIM-SESSION
01484              AND
01485          PI-COMPANY-ID = 'DMD'
01486          IF PI-1042-ARCHIVE-IND = 'B' OR ' '
01487              MOVE AL-UANON      TO ARCHBA
01488              MOVE PI-1042-ARCHIVE-IND
01489                                 TO ARCHBO.
01490
01491      IF  MORTGAGE-SESSION
01492              OR
01493          CREDIT-SESSION
01494          IF  PI-FORM-SQUEEZE-CONTROL EQUAL 'Y'
01495              MOVE PI-FORM-SQUEEZE-CONTROL
01496                                  TO FORMSQBO.
01497
01498      SET TS-INDX TO 1.
01499      MOVE 1 TO TS-ITEM.
01500
01501  7501-LOOP.
01502      
      * EXEC CICS READQ TS
01503 *         INTO  (TS-WORK-AREA)
01504 *         QUEUE (TS-NAME)
01505 *         LENGTH(TS-LENGTH)
01506 *         ITEM  (TS-ITEM)
01507 *     END-EXEC.
      *    MOVE '*$II   L              ''   #00004153' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-NAME, 
                 TS-WORK-AREA, 
                 TS-LENGTH, 
                 TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01508
01509      MOVE TS-WORK-AREA TO TS-GROUP (TS-INDX).
01510      SET TS-INDX UP BY 1.
01511      ADD 1 TO TS-ITEM.
01512      GO TO 7501-LOOP.
01513
01514  7585-TS-ITEMERR.
01515      IF  EIBTRNID NOT = TRANS-ID
01516         SUBTRACT 1 FROM TS-ITEM
01517         MOVE TS-ITEM TO PI-TEMP-STOR-ITEMS.
01518
01519      GO TO 7599-EXIT.
01520
01521  7590-TS-QIDERR.
01522
01523      MOVE ZEROS TO PI-TEMP-STOR-ITEMS.
01524
01525  7599-EXIT.
01526       EXIT.
01527      EJECT
01528  7600-UPDATE-TABLE-FROM-SCREEN.
01529
01530      IF  SC-TEXTL (SC-INDX) NOT = ZEROS
01531              OR
01532          SC-PCL   (SC-INDX) NOT = ZEROS
01533              OR
01534          SC-SQL   (SC-INDX) NOT = ZEROS
01535
01536          IF  TB-INDX NOT GREATER PI-TOTAL-LINES
01537              PERFORM 7700-MOVE-DATA THRU 7700-EXIT
01538              SET TB-INDX UP BY 1
01539
01540          ELSE
01541              IF  PI-TOTAL-LINES = MAX-LINES
01542                  MOVE ER-0051 TO EMI-ERROR
01543                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01544                  GO TO 8200-SEND-DATAONLY
01545
01546              ELSE
01547                  PERFORM 7700-MOVE-DATA THRU 7700-EXIT
01548                  SET TB-INDX UP BY 1
01549                  ADD 1 TO PI-TOTAL-LINES
01550
01551      ELSE
01552          IF  TB-INDX NOT GREATER PI-TOTAL-LINES
01553              SET TB-INDX UP BY 1.
01554
01555  7699-EXIT.
01556       EXIT.
01557
01558  7700-MOVE-DATA.
01559
01560      IF  SC-TEXTL (SC-INDX) NOT = ZEROS
01561          MOVE 1                 TO PI-UPDATE-SW
01562          MOVE SC-TEXT (SC-INDX) TO REC-TEXT (TB-INDX).
01563
01564      IF  PI-RETURN-TO-PROGRAM = PGM-EM152
01565              OR
01566          PI-RETURN-TO-PROGRAM = PGM-EL152
01567              OR
01568          PI-RETURN-TO-PROGRAM = PGM-EL689
01569          GO TO 7700-EXIT.
01570
01571      IF  SC-PCL (SC-INDX) NOT = ZEROS
01572          IF  SC-PC (SC-INDX) = SPACES OR
01573              SC-PC (SC-INDX) NUMERIC
01574              MOVE SC-PC (SC-INDX)
01575                                  TO REC-PC (TB-INDX)
01576              MOVE 1              TO PI-UPDATE-SW
01577
01578          ELSE
01579              MOVE -1             TO SC-PCL (SC-INDX)
01580              MOVE ER-0141        TO EMI-ERROR
01581              MOVE AL-UABON       TO SC-PCA (SC-INDX)
01582              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01583
01584      IF  SC-SQL (SC-INDX) NOT EQUAL ZEROS
01585              AND
01586          SC-SQ (SC-INDX) NOT EQUAL LOW-VALUES
01587          MOVE SC-SQ (SC-INDX)    TO W-LINE-SQUEEZE-IND
01588
01589          IF  W-LINE-SQUEEZE-VALID-VALUE
01590              MOVE 1              TO PI-UPDATE-SW
01591              MOVE SC-SQ (SC-INDX)
01592                                  TO REC-SQ (TB-INDX)
01593
01594          ELSE
01595              MOVE -1             TO SC-SQL (SC-INDX)
01596              MOVE ER-9348        TO EMI-ERROR
01597              MOVE AL-UABON       TO SC-SQA (SC-INDX)
01598              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01599
01600  7700-EXIT.
01601       EXIT.
01602      EJECT
01603  8100-SEND-INITIAL-MAP.
01604
01605      MOVE 'Y'                  TO PI-1042-SCREEN-SENT-IND.
01606      MOVE PI-COMPANY-ID        TO COMPANYO.
01607      MOVE SAVE-DATE            TO DATEBO.
01608      MOVE EIBTIME              TO TIME-IN.
01609      MOVE TIME-OUT             TO TIMEBO.
01610      MOVE EMI-MESSAGE-AREA (1) TO ERRMSGBO.
01611
01612      IF  PI-ENTRY-CD-1 = '1'
01613          MOVE 'LETTERS' TO TEXTTPBO
01614      ELSE
01615          IF  PI-ENTRY-CD-1 = '2'
01616              MOVE 'FORMS' TO TEXTTPBO
01617          ELSE
01618              MOVE 'HELP' TO TEXTTPBO.
01619
01620      MOVE ' ARCHIVE:'                 TO ARCHTBO.
01621
01622      IF  CLAIM-SESSION   AND
01623          PI-COMPANY-ID = 'DMD'
01624              MOVE '     BSR:'         TO ARCHTBO
01625              MOVE +13                 TO FORMSTBL
01626              MOVE +1                  TO LINESQTL
01627              MOVE AL-SADOF            TO FORMSTBA
01628                                          SC-SQA (02)
01629                                          SC-SQA (03)
01630                                          SC-SQA (04)
01631                                          SC-SQA (05)
01632                                          SC-SQA (06)
01633                                          SC-SQA (07)
01634                                          SC-SQA (08)
01635                                          SC-SQA (09)
01636                                          SC-SQA (10)
01637                                          SC-SQA (11)
01638                                          SC-SQA (12)
01639                                          SC-SQA (13)
01640                                          SC-SQA (14)
01641                                          SC-SQA (15)
01642      ELSE
01643
01644      IF  MORTGAGE-SESSION
01645          MOVE +13                     TO FORMSTBL
01646          MOVE AL-SANOF                TO FORMSTBA
01647          MOVE +1                      TO LINESQTL
01648          MOVE AL-SANOF                TO LINESQTA
01649          MOVE AL-UANON                TO FORMSQBA
01650          MOVE AL-UANON                TO ARCHBA
01651                                          ARCHTBA
01652
01653      ELSE
01654          IF  CLAIM-SESSION
01655              MOVE +13                 TO FORMSTBL
01656              MOVE +1                  TO LINESQTL
01657              MOVE AL-SADOF            TO FORMSTBA
01658                                          ARCHBA
01659                                          ARCHTBA
01660                                          SC-SQA (02)
01661                                          SC-SQA (03)
01662                                          SC-SQA (04)
01663                                          SC-SQA (05)
01664                                          SC-SQA (06)
01665                                          SC-SQA (07)
01666                                          SC-SQA (08)
01667                                          SC-SQA (09)
01668                                          SC-SQA (10)
01669                                          SC-SQA (11)
01670                                          SC-SQA (12)
01671                                          SC-SQA (13)
01672                                          SC-SQA (14)
01673                                          SC-SQA (15)
01674
01675          ELSE
01676              MOVE +13                 TO FORMSTBL
01677              MOVE AL-SANOF            TO FORMSTBA
01678              MOVE +1                  TO LINESQTL
01679              MOVE AL-SANOF            TO LINESQTA
01680              MOVE AL-UANON            TO FORMSQBA
01681                                          ARCHBA
01682              MOVE +9                  TO ARCHTBL
01683              MOVE AL-SANOF            TO ARCHTBA.
01684
01685      IF  CREDIT-SESSION
01686              OR
01687          MORTGAGE-SESSION
01688
01689          IF  PI-1042-ARCHIVE-IND EQUAL 'Y'
01690              MOVE +1            TO ARCHBL
01691              MOVE AL-UANON      TO ARCHBA
01692              MOVE PI-1042-ARCHIVE-IND
01693                                 TO ARCHBO.
01694
01695      IF  CLAIM-SESSION
01696              AND
01697          PI-COMPANY-ID = 'DMD'
01698          IF PI-1042-ARCHIVE-IND = 'B' OR ' '
01699              MOVE +1            TO ARCHBL
01700              MOVE AL-UANON      TO ARCHBA
01701              MOVE PI-1042-ARCHIVE-IND
01702                                 TO ARCHBO.
01703
01704      IF  MORTGAGE-SESSION
01705              OR
01706          CREDIT-SESSION
01707
01708          IF  PI-FORM-SQUEEZE-CONTROL EQUAL 'Y'
01709              MOVE PI-FORM-SQUEEZE-CONTROL
01710                                  TO FORMSQBO
01711              MOVE +1             TO FORMSQBL
01712              MOVE AL-UANON       TO FORMSQBA.
01713
01714      MOVE PI-COMM-CONTROL TO CONTRLBI.
01715      MOVE PI-TOTAL-LINES  TO TOTI.
01716      MOVE -1              TO FUNCTL.
01717
01718      
      * EXEC CICS SEND
01719 *         MAPSET(MAPSET-NAME)
01720 *         MAP   (MAP-NAME)
01721 *         FROM  (EL104BO)
01722 *         ERASE
01723 *         CURSOR
01724 *     END-EXEC.
           MOVE LENGTH OF
            EL104BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00004369' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL104BO, 
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
           
01725
01726      GO TO 0200-RECEIVE.
01727
01728  8200-SEND-DATAONLY.
01729
01730      IF  PI-1042-SCREEN-NOT-SENT
01731          GO TO 8100-SEND-INITIAL-MAP.
01732
01733      MOVE PI-COMPANY-ID        TO COMPANYO.
01734      MOVE EIBTIME        TO TIME-IN.
01735      MOVE TIME-OUT       TO TIMEBO.
01736      MOVE PI-TOTAL-LINES TO TOTI.
01737
01738      MOVE ' ARCHIVE:'                 TO ARCHTBO.
01739
01740      IF  CLAIM-SESSION   AND
01741          PI-COMPANY-ID = 'DMD'
01742              MOVE '     BSR:'         TO ARCHTBO
01743              MOVE +13                 TO FORMSTBL
01744              MOVE +1                  TO LINESQTL
01745              MOVE AL-SADOF            TO FORMSTBA
01746                                          SC-SQA (02)
01747                                          SC-SQA (03)
01748                                          SC-SQA (04)
01749                                          SC-SQA (05)
01750                                          SC-SQA (06)
01751                                          SC-SQA (07)
01752                                          SC-SQA (08)
01753                                          SC-SQA (09)
01754                                          SC-SQA (10)
01755                                          SC-SQA (11)
01756                                          SC-SQA (12)
01757                                          SC-SQA (13)
01758                                          SC-SQA (14)
01759                                          SC-SQA (15)
01760      ELSE
01761
01762      IF  MORTGAGE-SESSION
01763          MOVE +13                     TO FORMSTBL
01764          MOVE AL-SANOF                TO FORMSTBA
01765          MOVE +1                      TO LINESQTL
01766          MOVE AL-SANOF                TO LINESQTA
01767          MOVE AL-UANON                TO FORMSQBA
01768          MOVE AL-UANON                TO ARCHBA
01769                                          ARCHTBA
01770
01771      ELSE
01772          IF  CLAIM-SESSION
01773              MOVE +13                 TO FORMSTBL
01774              MOVE +1                  TO LINESQTL
01775              MOVE AL-SADOF            TO FORMSTBA
01776                                          ARCHBA
01777                                          ARCHTBA
01778                                          SC-SQA (02)
01779                                          SC-SQA (03)
01780                                          SC-SQA (04)
01781                                          SC-SQA (05)
01782                                          SC-SQA (06)
01783                                          SC-SQA (07)
01784                                          SC-SQA (08)
01785                                          SC-SQA (09)
01786                                          SC-SQA (10)
01787                                          SC-SQA (11)
01788                                          SC-SQA (12)
01789                                          SC-SQA (13)
01790                                          SC-SQA (14)
01791                                          SC-SQA (15)
01792
01793          ELSE
01794              MOVE +13                 TO FORMSTBL
01795              MOVE AL-SANOF            TO FORMSTBA
01796              MOVE +1                  TO LINESQTL
01797              MOVE AL-SANOF            TO LINESQTA
01798              MOVE AL-UANON            TO FORMSQBA
01799                                          ARCHBA
01800              MOVE +9                  TO ARCHTBL
01801              MOVE AL-SANOF            TO ARCHTBA.
01802
01803      IF  CREDIT-SESSION
01804              OR
01805          MORTGAGE-SESSION
01806
01807          IF  PI-1042-ARCHIVE-IND EQUAL 'Y'
01808              MOVE +1            TO ARCHBL
01809              MOVE AL-UANON      TO ARCHBA
01810              MOVE PI-1042-ARCHIVE-IND
01811                                 TO ARCHBO.
01812
01813      IF  CLAIM-SESSION
01814              AND
01815          PI-COMPANY-ID = 'DMD'
01816              AND
01817          WS-8126-ERROR-SW = 'N'
01818          IF PI-1042-ARCHIVE-IND = 'B' OR ' '
01819              MOVE +1            TO ARCHBL
01820              MOVE AL-UANON      TO ARCHBA
01821              MOVE PI-1042-ARCHIVE-IND
01822                                 TO ARCHBO.
01823
01824      IF  MORTGAGE-SESSION
01825              OR
01826          CREDIT-SESSION
01827
01828          IF  PI-FORM-SQUEEZE-CONTROL EQUAL 'Y'
01829              MOVE PI-FORM-SQUEEZE-CONTROL
01830                                  TO FORMSQBO
01831              MOVE +1             TO FORMSQBL
01832              MOVE AL-UANON       TO FORMSQBA.
01833
01834      IF  NOT EMI-NO-ERRORS
01835          MOVE EMI-MESSAGE-AREA (1) TO ERRMSGBO
01836      ELSE
01837          MOVE -1 TO FUNCTL.
01838
01839      
      * EXEC CICS SEND
01840 *         MAPSET(MAPSET-NAME)
01841 *         MAP   (MAP-NAME)
01842 *         DATAONLY
01843 *         CURSOR
01844 *         FROM  (EL104BO)
01845 *    END-EXEC.
           MOVE LENGTH OF
            EL104BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00004490' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL104BO, 
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
           
01846
01847      GO TO 0200-RECEIVE.
01848
01849  8300-SEND-TEXT.
01850
01851      
      * EXEC CICS SEND TEXT
01852 *         FROM  (LOGOFF-TEXT)
01853 *         ERASE
01854 *         FREEKB
01855 *         LENGTH(LOGOFF-LENGTH)
01856 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00004502' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353032' TO DFHEIV0(25:11)
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
           
01857
01858      PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT.
01859
01860      
      * EXEC CICS RETURN
01861 *    END-EXEC.
      *    MOVE '.(                    &   #00004511' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01862
01863  8800-UNAUTHORIZED-ACCESS.
01864
01865      MOVE UNACCESS-MSG TO LOGOFF-MSG.
01866      GO TO 8300-SEND-TEXT.
01867
01868  9000-RETURN-CICS.
01869
01870      IF  ANY-UPDATES
01871          MOVE ER-0045  TO EMI-ERROR
01872          MOVE -1       TO FUNCTL
01873          MOVE SPACES   TO PFENTRBO
01874          MOVE AL-UNNOF TO PFENTRBA
01875          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01876          GO TO 8200-SEND-DATAONLY.
01877
01878      PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.
01879
01880
01881      MOVE EIBAID  TO PI-ENTRY-CD-1.
01882      MOVE 'EL005' TO PGM-NAME.
01883      GO TO 9300-XCTL.
01884
01885  9100-RETURN-TRAN.
01886
01887      
      * EXEC CICS RETURN
01888 *         TRANSID (TRANS-ID)
01889 *         COMMAREA(PROGRAM-INTERFACE-BLOCK)
01890 *         LENGTH  (PI-COMM-LENGTH)
01891 *    END-EXEC.
      *    MOVE '.(CT                  &   #00004538' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01892
01893  9200-RETURN-MAIN-MENU.
01894
01895      IF  ANY-UPDATES
01896          MOVE -1       TO FUNCTL
01897          MOVE SPACES   TO PFENTRBO
01898          MOVE AL-UNNOF TO PFENTRBA
01899          MOVE ER-0045  TO EMI-ERROR
01900          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01901          GO TO 8200-SEND-DATAONLY.
01902
01903      IF  CLAIM-SESSION
01904          MOVE XCTL-CLAIM      TO PGM-NAME
01905
01906      ELSE
01907          IF  CREDIT-SESSION
01908              MOVE XCTL-CREDIT TO PGM-NAME
01909
01910          ELSE
01911              IF  WARRANTY-SESSION
01912                  MOVE XCTL-WARRANTY TO PGM-NAME
01913
01914              ELSE
01915                  IF  MORTGAGE-SESSION
01916                      MOVE XCTL-MORTGAGE   TO PGM-NAME
01917
01918                  ELSE
01919                      MOVE XCTL-GEN-LEDGER TO PGM-NAME.
01920
01921  9300-XCTL.
01922
01923      MOVE MAP-NUMBER           TO PI-CURRENT-SCREEN-NO.
01924      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.
01925
01926      MOVE TRANS-ID             TO EIBTRNID
01927
01928      IF  PI-RETURN-TO-PROGRAM NOT EQUAL PGM-EM152
01929              AND
01930          PI-RETURN-TO-PROGRAM NOT EQUAL PGM-EL152
01931              AND
01932          PI-RETURN-TO-PROGRAM NOT EQUAL PGM-EL689
01933
01934          PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT.
01935
01936      
      * EXEC CICS XCTL
01937 *         PROGRAM (PGM-NAME)
01938 *         COMMAREA(PROGRAM-INTERFACE-BLOCK)
01939 *         LENGTH  (PI-COMM-LENGTH)
01940 *     END-EXEC.
      *    MOVE '.$C                   $   #00004587' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01941
01942  9400-CLEAR.
01943      IF  ANY-UPDATES
01944         MOVE ER-0045  TO EMI-ERROR
01945         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01946         SET TB-INDX TO PI-CURRENT-LINE
01947         PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT
01948                 VARYING SC-INDX FROM 1 BY 1 UNTIL
01949                 SC-INDX GREATER NUM-LINES-PER-SCREEN
01950         GO TO 8100-SEND-INITIAL-MAP.
01951
01952  9410-RETURN.
01953         MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.
01954         GO TO 9300-XCTL.
01955
       9450-PF5.
           IF ANY-UPDATES
              MOVE -1                  TO FUNCTL
              MOVE SPACES              TO PFENTRBO
              MOVE AL-UNNOF            TO PFENTRBA
              MOVE ER-0045             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF
082211     IF CREDIT-SESSION
082211         MOVE 'EL1044'           TO PGM-NAME
082211     ELSE
082211         MOVE 'EL1041'           TO PGM-NAME
082211     END-IF
           GO TO 9300-XCTL
           .
01956  9500-PF12.
01957      IF  ANY-UPDATES
01958         MOVE -1       TO FUNCTL
01959         MOVE SPACES   TO PFENTRBO
01960         MOVE AL-UNNOF TO PFENTRBA
01961         MOVE ER-0045  TO EMI-ERROR
01962         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01963         GO TO 8200-SEND-DATAONLY.
01964
01965
01966      MOVE 'EL010' TO PGM-NAME.
01967      GO TO 9300-XCTL.
01968      EJECT
01969  9600-PGMID-ERROR.
01970      
      * EXEC CICS HANDLE CONDITION
01971 *         PGMIDERR(8300-SEND-TEXT)
01972 *     END-EXEC.
      *    MOVE '"$L                   ! ( #00004637' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303034363337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01973
01974      MOVE PGM-NAME     TO PI-CALLING-PROGRAM.
01975      MOVE SPACES       TO PI-ENTRY-CD-1.
01976      MOVE 'EL005'      TO PGM-NAME.
01977      MOVE PGM-NAME     TO LOGOFF-PGM.
01978      MOVE PGMIDERR-MSG TO LOGOFF-FILL.
01979      GO TO 9300-XCTL.
01980
01981  9700-LINK-DATE-CONVERT.
01982      
      * EXEC CICS LINK
01983 *        PROGRAM    ('ELDATCV')
01984 *        COMMAREA   (DATE-CONVERSION-DATA)
01985 *        LENGTH     (DC-COMM-LENGTH)
01986 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00004649' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01987
01988  9700-EXIT.
01989      EXIT.
01990
01991  9900-ERROR-FORMAT.
01992
01993      MOVE PI-LANGUAGE-TYPE       TO EMI-LANGUAGE-IND.
01994
01995      IF  NOT EMI-ERRORS-COMPLETE
01996         
      * EXEC CICS LINK
01997 *            PROGRAM('EL001')
01998 *            COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)
01999 *            LENGTH(EMI-COMM-LENGTH)
02000 *        END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   ''   #00004663' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02001
02002  9900-EXIT.
02003       EXIT.
02004                                   EJECT
02005  9910-INITIALIZE-SECURITY.
02006 ******************************************************************
02007 *    THIS IS A COMBINED SECURITY PROCESSOR.                      *
02008 *                                                                *
02009 ******************************************************************
02010
02011      IF  PI-PROCESSOR-ID EQUAL 'LGXX'
02012          GO TO 9910-EXIT.
02013
02014      IF  MORTGAGE-SESSION
02015          MOVE '125E'                 TO SC-QUID-SYSTEM
02016          MOVE EIBTRMID               TO SC-QUID-TERMINAL
02017
02018          
      * EXEC CICS READQ TS
02019 *            QUEUE  (SC-QUID-KEY)
02020 *            INTO   (SECURITY-CONTROL-E)
02021 *            LENGTH (SC-COMM-LENGTH-E)
02022 *            ITEM   (SC-ITEM)
02023 *        END-EXEC
      *    MOVE '*$II   L              ''   #00004685' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SC-QUID-KEY, 
                 SECURITY-CONTROL-E, 
                 SC-COMM-LENGTH-E, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02024
02025          MOVE SC-MP-DISPLAY (W-APPL-SCRTY-NDX)
02026                                      TO PI-DISPLAY-CAP
02027          MOVE SC-MP-UPDATE (W-APPL-SCRTY-NDX)
02028                                      TO PI-MODIFY-CAP
02029
02030          GO TO 9910-EXIT
02031
02032      ELSE
02033          
      * EXEC CICS READQ TS
02034 *            QUEUE  (PI-SECURITY-TEMP-STORE-ID)
02035 *            INTO   (SECURITY-CONTROL)
02036 *            LENGTH (SC-COMM-LENGTH)
02037 *            ITEM   (W-SC-ITEM)
02038 *        END-EXEC
      *    MOVE '*$II   L              ''   #00004700' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 W-SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02039
02040          MOVE SC-CREDIT-DISPLAY (03)  TO PI-DISPLAY-CAP
02041          MOVE SC-CREDIT-UPDATE  (03)  TO PI-MODIFY-CAP.
02042
02043  9910-EXIT.
02044      EXIT.
02045                                   EJECT
02046  9990-ABEND.
02047      MOVE DFHEIBLK               TO EMI-LINE1.
02048      
      * EXEC CICS LINK
02049 *        PROGRAM   ('EL004')
02050 *        COMMAREA  (EMI-LINE1)
02051 *        LENGTH    (72)
02052 *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00004715' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02053
02054      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGBO.
02055      GO TO 8200-SEND-DATAONLY.
02056
02057      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1042' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
02058
02059  9995-SECURITY-VIOLATION.
02060 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00004744' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373434' TO DFHEIV0(25:11)
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
02061
02062  9995-EXIT.
02063      EXIT.
02064

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1042' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9990-ABEND,
                     9600-PGMID-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 9400-CLEAR,
                     0100-PA,
                     0100-PA,
                     0100-PA
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 4610-ENDBR,
                     6000-NOT-OPEN,
                     4610-ENDBR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 7010-ENDBR,
                     6000-NOT-OPEN,
                     7010-ENDBR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 7299-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 7590-TS-QIDERR,
                     7585-TS-ITEMERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1042' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
