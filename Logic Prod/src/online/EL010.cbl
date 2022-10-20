00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL010 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 09:26:36.
00007 *                            VMOD=2.005.
00008 *AUTHOR.        LOGIC, INC.
00009 *               DALLAS, TEXAS.
00009 *
00010 *DATE-COMPILED.
00011 *SECURITY.   *****************************************************
00012 *            *                                                   *
00013 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00014 *            *                                                   *
00015 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00016 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00017 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00018 *            *                                                   *
00019 *            *****************************************************
00020 *REMARKS. EX99 - OPERATOR HELP FUNCTIONS.
00021      EJECT
00022  ENVIRONMENT DIVISION.
00023  DATA DIVISION.
00024  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00025  01  LCP-TIME-OF-DAY-XX.
00026      05  LCP-TIME-OF-DAY-68        PIC 9(6).
00027      05  FILLER                    PIC 99.
00028  01  LCP-CICS-TIME                 PIC 9(15).
00029  01  LCP-CURRENT-DATE-68.
00030      05  LCP-MONTH                 PIC X(2).
00031      05  FILLER                    PIC X VALUE '/'.
00032      05  LCP-DAY1                  PIC X(2).
00033      05  FILLER                    PIC X VALUE '/'.
00034      05  LCP-YEAR                  PIC X(2).
00035  01  LCP-CICS-DATE                 PIC 9(15).
00036  77  FILLER  PIC X(32)  VALUE '********************************'.
00037  77  FILLER  PIC X(32)  VALUE '*   EL010  WORKING STORAGE     *'.
00038  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.005 *********'.
00039
00040  77  WS-COMM-LENGTH              PIC S9(4)   COMP VALUE +1500.
00041  77  CHANGE-KEY-SW               PIC X.
00042      88  TIME-TO-CHANGE-KEY            VALUE 'X'.
00043
00044  01  LITERALS-NUMBERS.
00045      12  LIT-COMM                PIC X(4)    VALUE '010A'.
00046      12  LIT-BUFF                PIC X(4)    VALUE '010B'.
00047      12  XCTL-EL005              PIC X(8)    VALUE 'EL005'.
00048      12  THIS-PGM                PIC X(8)    VALUE 'EL010'.
00049      12  XCTL-EL126              PIC X(8)    VALUE 'EL126'.
00050      12  THIS-TRANS              PIC X(4)    VALUE 'EX99'.
00051
00052      12  ER-7008                 PIC X(4)    VALUE '7008'.
00053      12  ER-0015                 PIC X(4)    VALUE '0015'.
00054      12  ER-0064                 PIC X(4)    VALUE '0064'.
00055      12  ER-0130                 PIC X(4)    VALUE '0130'.
00056      12  ER-0131                 PIC X(4)    VALUE '0131'.
00057      12  ER-0268                 PIC X(4)    VALUE '0268'.
00058
00059      12  QUEUE-KEY.
00060          16  TERM-ID             PIC X(4).
00061          16  SUB-KEY             PIC X(4).
00062      12  COMM-KEY                PIC X(8).
00063      12  BUFFER-KEY              PIC X(8).
00064      12  TRANS-ID                PIC X(4).
00065      12  WCC-CTL                 PIC X       VALUE ' '.
00066      12  COUNT-1                 PIC 99.
00067      12  CHECK-PFKEYS            PIC 99.
00068      12  CALL-PGM                PIC X(8).
00069      EJECT
00070  01  FORMATTED-LINE.
00071      12  FILLER                  PIC XX      VALUE SPACES.
00072      12  HOLD-LINE               PIC X(70).
00073      12  FILLER                  PIC X(7)    VALUE SPACES.
00074
00075  01  TIME-UNFORMATTED.
00076      12  UN-HOURS                PIC XX.
00077      12  UN-MINUTES              PIC XX.
00078      12  FILLER                  PIC X(4).
00079
00080  01  TIME-FORMATTED.
00081      12  FOR-HOURS               PIC XX.
00082      12  FILLER                  PIC X       VALUE '.'.
00083      12  FOR-MINUTES             PIC XX.
00084
00085  01  ERROR-SWITCHES.
00086      12  ERROR-SWITCH            PIC X.
00087          88  END-OF-FILE                     VALUE 'E'.
00088          88  SCREEN-FULL                     VALUE 'F'.
00089          88  SCREEN-ERROR                    VALUE 'X'.
00090
00091  01  FILE-READ-KEY.
00092      12  COMPANY-CODE            PIC X.
00093      12  HELP-TYPE               PIC X.
00094      12  ERROR-SCREEN            PIC X(4).
00095      12  HELP-COMPANY            PIC X(3).
00096      12  FILLER                  PIC X(4)    VALUE SPACES.
00097      12  LINE-SEQUENCE           PIC S9(4)   COMP.
00098
00099  01  COMP-LENGTHS.
00100      12  BUFFER-LENGTH           PIC S9(4)   COMP.
00101      12  CURSOR-POS              PIC S9(4)   COMP.
00102      EJECT
00103 *    COPY ELCLOGOF.
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
00104      EJECT
00105 *    COPY EL010S.
       01  EL010AI.
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
           05  SCODEL PIC S9(0004) COMP.
           05  SCODEF PIC  X(0001).
           05  FILLER REDEFINES SCODEF.
               10  SCODEA PIC  X(0001).
           05  SCODEI PIC  X(0004).
      *    -------------------------------
           05  ECODEL PIC S9(0004) COMP.
           05  ECODEF PIC  X(0001).
           05  FILLER REDEFINES ECODEF.
               10  ECODEA PIC  X(0001).
           05  ECODEI PIC  X(0004).
           05  INFOD OCCURS 15  TIMES.
      *    -------------------------------
               10  INFOL PIC S9(0004) COMP.
               10  INFOF PIC  X(0001).
               10  FILLER REDEFINES INFOF.
                   15  INFOA PIC  X(0001).
               10  INFOI PIC  X(0079).
      *    -------------------------------
           05  MSGL PIC S9(0004) COMP.
           05  MSGF PIC  X(0001).
           05  FILLER REDEFINES MSGF.
               10  MSGA PIC  X(0001).
           05  MSGI PIC  X(0075).
      *    -------------------------------
           05  PFKEYL PIC S9(0004) COMP.
           05  PFKEYF PIC  X(0001).
           05  FILLER REDEFINES PFKEYF.
               10  PFKEYA PIC  X(0001).
           05  PFKEYI PIC  X(0002).
       01  EL010AO REDEFINES EL010AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SCODEO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ECODEO PIC  X(0004).
      *    -------------------------------
           05  INFOD OCCURS 15  TIMES.
               10  FILLER        PIC  X(0003).
               10  INFOO PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSGO PIC  X(0075).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYO PIC  X(0002).
      *    -------------------------------
00106      EJECT
00107 *    COPY ELCAID.
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
00108  01  FILLER REDEFINES DFHAID.
00109      12  FILLER                  PIC X(8).
00110      12  AID-KEYS OCCURS 24 TIMES.
00111          16  FILLER              PIC X.
00112      EJECT
00113 *    COPY ELCINTF.
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
00114      12  EL010-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.
00115          16  PI-CALLING-TRANS    PIC X(4).
00116          16  SAVE-CURSOR         PIC S9(4)   COMP.
00117          16  SAVE-KEY.
00118              20  SAVE-COMPANY-CD PIC X.
00119              20  SAVE-HELP-TYPE  PIC X.
00120              20  SAVE-CONTROL    PIC X(4).
00121              20  FILLER          PIC X(7).
00122              20  SAVE-LINE       PIC S9(4)   COMP.
00123          16  EOF-SWITCH          PIC X.
00124              88  LAST-REC-NOT-FOUND          VALUE 'W'.
00125              88  TOP-OF-FILE                 VALUE 'X'.
00126              88  BOTTOM-OF-FILE              VALUE 'Y'.
00127              88  BAD-BROWSE                  VALUE 'Z'.
00128          16  FILLER              PIC X(618).
00129      EJECT
00130 *    COPY ELCEMIB.
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
00131      EJECT
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
00133  01  DFHCOMMAREA                 PIC X(1500).
00134
00135 *01 PARM-LIST .
00136 *    12  FILLER                  PIC S9(8)   COMP.
00137 *    12  HELP-PNT                PIC S9(8)   COMP.
00138 *    12  BUFF-PNT                PIC S9(8)   COMP.
00139      EJECT
00140 *    COPY ELCTEXT.
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
00141
00142  01  BUFFER-AREA                 PIC X.
00143      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA TEXT-FILES
                                BUFFER-AREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL010' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00145
00146      IF EIBCALEN = ZERO
00147          GO TO 8800-UNAUTHORIZED-ACCESS.
00148
00149      MOVE THIS-TRANS             TO TRANS-ID.
00150      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00151
00152      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00153          MOVE LOW-VALUES         TO EL010AO
00154          MOVE ER-7008            TO EMI-ERROR
00155          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00156          GO TO 8110-SEND-DATA.
00157
00158      MOVE SPACES                 TO CHANGE-KEY-SW.
00159
00160      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00161          GO TO 0100-SAVE-BUFFER.
00162
00163      IF EIBAID = DFHCLEAR
00164          GO TO 0200-RESTORE-BUFFER.
00165
00166      
      * EXEC CICS HANDLE CONDITION
00167 *        PGMIDERR  (8820-XCTL-ERROR)
00168 *        MAPFAIL   (0050-MAPFAIL-HANDLE)
00169 *        ERROR     (9990-ABEND)
00170 *    END-EXEC.
      *    MOVE '"$L?.                 ! " #00000838' TO DFHEIV0
           MOVE X'22244C3F2E20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303030383338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00171
00172      
      * EXEC CICS RECEIVE
00173 *        MAP       ('EL010A')
00174 *        MAPSET    ('EL010S')
00175 *    END-EXEC.
           MOVE 'EL010A' TO DFHEIV1
           MOVE 'EL010S' TO DFHEIV2
      *    MOVE '8"T                   ''   #00000844' TO DFHEIV0
           MOVE X'382254202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303030383434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL010AI, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00176
00177      MOVE SPACES                 TO ERROR-SWITCHES.
00178
00179      IF PFKEYL GREATER THAN ZERO
00180          PERFORM 0300-TRANS-PF THRU 0310-TRANS-PF-EXIT.
00181
00182      IF SCREEN-ERROR
00183          MOVE ER-7008            TO EMI-ERROR
00184          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00185          GO TO 8110-SEND-DATA.
00186
00187      IF EIBAID = DFHPF1 OR DFHPF3
00188          GO TO 2000-BROWSE-FORWARD.
00189
00190      IF EIBAID = DFHPF2 OR DFHPF4
00191          GO TO 2100-BROWSE-BACKWARD.
00192
00193      IF EIBAID = DFHPF7
00194         MOVE SPACES              TO FILE-READ-KEY
00195         MOVE PI-COMPANY-CD       TO COMPANY-CODE
00196          GO TO 2600-USER-NOTES.
00197
00198      IF EIBAID = DFHPF5
00199         MOVE SPACES              TO FILE-READ-KEY
00200         MOVE LOW-VALUES          TO COMPANY-CODE
00201          GO TO 2400-SKIP-TO-SCREEN.
00202
00203      IF EIBAID = DFHPF6
00204         MOVE SPACES              TO FILE-READ-KEY
00205         MOVE LOW-VALUES          TO COMPANY-CODE
00206          GO TO 2500-SKIP-TO-ERROR.
00207
00208      MOVE ER-7008                TO EMI-ERROR.
00209      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00210      GO TO 8110-SEND-DATA.
00211
00212
00213  0050-MAPFAIL-HANDLE.
00214      MOVE LOW-VALUES             TO EL010AO.
00215      MOVE 'Y'                    TO EOF-SWITCH.
00216      GO TO 2000-BROWSE-FORWARD.
00217      EJECT
00218
00219  0100-SAVE-BUFFER.
00220      MOVE EIBTRMID               TO TERM-ID.
00221      MOVE LIT-COMM               TO SUB-KEY.
00222      MOVE QUEUE-KEY              TO COMM-KEY.
00223      MOVE LIT-BUFF               TO SUB-KEY.
00224      MOVE QUEUE-KEY              TO BUFFER-KEY.
00225
00226      PERFORM 0400-DELETE-TS THRU 0420-DELETE-TS-EXIT.
00227
00228      
      * EXEC CICS WRITEQ TS
00229 *        QUEUE   (COMM-KEY)
00230 *        FROM    (PROGRAM-INTERFACE-BLOCK)
00231 *        LENGTH  (WS-COMM-LENGTH)
00232 *    END-EXEC.
      *    MOVE '*"                    ''   #00000900' TO DFHEIV0
           MOVE X'2A2220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303030393030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMM-KEY, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00233
00234      MOVE EIBTRNID               TO PI-CALLING-TRANS.
00235      MOVE EIBCPOSN               TO SAVE-CURSOR.
00236
00237      
      * EXEC CICS RECEIVE
00238 *        SET     (ADDRESS OF BUFFER-AREA)
00239 *        LENGTH  (BUFFER-LENGTH)
00240 *        BUFFER
00241 *    END-EXEC.
      *    MOVE '$"S B  L              ''   #00000909' TO DFHEIV0
           MOVE X'242253204220204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303030393039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 BUFFER-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BUFFER-AREA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00242
00243      
      * EXEC CICS WRITEQ TS
00244 *        QUEUE   (BUFFER-KEY)
00245 *        FROM    (BUFFER-AREA)
00246 *        LENGTH  (BUFFER-LENGTH)
00247 *    END-EXEC.
      *    MOVE '*"                    ''   #00000915' TO DFHEIV0
           MOVE X'2A2220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303030393135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BUFFER-KEY, 
                 BUFFER-AREA, 
                 BUFFER-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00248
00249      MOVE THIS-PGM               TO PI-CALLING-PROGRAM.
00250      MOVE LOW-VALUES             TO EL010AO.
00251      INSPECT PI-CURRENT-SCREEN-NO CONVERTING 'S' TO 'A'.
00252      MOVE PI-CURRENT-SCREEN-NO   TO SCODEO.
00253      MOVE PI-LAST-ERROR-NO       TO ECODEO.
00254
00255      MOVE SPACES                 TO FILE-READ-KEY
00256      MOVE LOW-VALUES             TO COMPANY-CODE
00257      GO TO  2400-SKIP-TO-SCREEN.
00258
00259 *    IF PI-LAST-ERROR-NO = SPACES OR = ZEROS
00260 *       MOVE SPACES              TO FILE-READ-KEY
00261 *       MOVE LOW-VALUES          TO COMPANY-CODE
00262 *        GO TO  2400-SKIP-TO-SCREEN.
00263 *
00264 *    MOVE SPACES                 TO FILE-READ-KEY
00265 *    MOVE LOW-VALUES             TO COMPANY-CODE
00266 *    GO TO  2500-SKIP-TO-ERROR.
00267      EJECT
00268  0200-RESTORE-BUFFER.
00269      MOVE PI-CALLING-TRANS       TO TRANS-ID.
00270      MOVE SAVE-CURSOR            TO CURSOR-POS.
00271      MOVE EIBTRMID               TO TERM-ID.
00272      MOVE LIT-COMM               TO SUB-KEY.
00273      MOVE QUEUE-KEY              TO COMM-KEY.
00274      MOVE LIT-BUFF               TO SUB-KEY.
00275      MOVE QUEUE-KEY              TO BUFFER-KEY.
00276
00277      
      * EXEC CICS READQ TS
00278 *        QUEUE   (COMM-KEY)
00279 *        INTO    (PROGRAM-INTERFACE-BLOCK)
00280 *        LENGTH  (WS-COMM-LENGTH)
00281 *    END-EXEC.
      *    MOVE '*$I    L              ''   #00000949' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303030393439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMM-KEY, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00282
00283      
      * EXEC CICS READQ TS
00284 *        QUEUE   (BUFFER-KEY)
00285 *        SET     (ADDRESS OF BUFFER-AREA)
00286 *        LENGTH  (BUFFER-LENGTH)
00287 *    END-EXEC.
      *    MOVE '*$S    L              ''   #00000955' TO DFHEIV0
           MOVE X'2A2453202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303030393535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BUFFER-KEY, 
                 DFHEIV20, 
                 BUFFER-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BUFFER-AREA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00288
00289      
      * EXEC CICS SEND
00290 *        FROM     (BUFFER-AREA)
00291 *        LENGTH   (BUFFER-LENGTH)
00292 *        CTLCHAR  (WCC-CTL)
00293 *    END-EXEC.
      *    MOVE '$$    C           L F ,   #00000961' TO DFHEIV0
           MOVE X'242420202020432020202020' TO DFHEIV0(1:12)
           MOVE X'2020202020204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303030393631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BUFFER-AREA, 
                 BUFFER-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 WCC-CTL, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00294
00295      MOVE LOW-VALUES             TO EL010AO.
00296
00297      
      * EXEC CICS SEND
00298 *        MAP      ('EL010A')
00299 *        MAPSET   ('EL010S')
00300 *        CURSOR   (CURSOR-POS)
00301 *        DATAONLY
00302 *        FREEKB
00303 *    END-EXEC.
           MOVE 'EL010A' TO DFHEIV1
           MOVE 'EL010S' TO DFHEIV2
      *    MOVE '8$D    CT    F  H     ,   #00000969' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303030393639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL010AO, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 CURSOR-POS, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00304
00305      PERFORM 0400-DELETE-TS THRU 0420-DELETE-TS-EXIT.
00306
00307      GO TO 9000-RETURN-TRANS.
00308
00309      EJECT
00310  0300-TRANS-PF.
00311      IF PFKEYI LESS 1 OR GREATER 24
00312          MOVE 'X'                TO ERROR-SWITCH
00313          GO TO 0310-TRANS-PF-EXIT.
00314
00315      MOVE PFKEYI                 TO CHECK-PFKEYS.
00316      MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.
00317
00318  0310-TRANS-PF-EXIT.
00319      EXIT.
00320
00321  0400-DELETE-TS.
00322      
      * EXEC CICS HANDLE CONDITION
00323 *        QIDERR (0410-BUFF-KEY-NOT-FOUND)
00324 *    END-EXEC.
      *    MOVE '"$N                   ! # #00000994' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303030393934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00325
00326      
      * EXEC CICS DELETEQ TS
00327 *        QUEUE (BUFFER-KEY)
00328 *    END-EXEC.
      *    MOVE '*&                    #   #00000998' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303030393938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BUFFER-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00329
00330  0410-BUFF-KEY-NOT-FOUND.
00331      
      * EXEC CICS HANDLE CONDITION
00332 *        QIDERR (0420-DELETE-TS-EXIT)
00333 *    END-EXEC.
      *    MOVE '"$N                   ! $ #00001003' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303031303033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00334
00335      
      * EXEC CICS DELETEQ TS
00336 *        QUEUE (COMM-KEY)
00337 *    END-EXEC.
      *    MOVE '*&                    #   #00001007' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031303037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMM-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00338
00339  0420-DELETE-TS-EXIT.
00340      EXIT.
00341
00342      EJECT
00343  2000-BROWSE-FORWARD.
00344      IF BOTTOM-OF-FILE
00345          MOVE ER-0130            TO EMI-ERROR
00346          PERFORM 9900-ERROR-FORMAT
00347              THRU 9900-EXIT
00348         GO TO 8110-SEND-DATA.
00349
00350      MOVE SPACE                  TO EOF-SWITCH.
00351
00352      IF EIBAID = DFHPF1
00353         ADD 15                   TO SAVE-LINE
00354      ELSE
00355         ADD 5                    TO SAVE-LINE.
00356
00357      MOVE SAVE-KEY               TO FILE-READ-KEY.
00358
00359  2010-SKIP-ADD.
00360      MOVE 1                      TO COUNT-1
00361      MOVE SPACES                 TO ERROR-SWITCH FORMATTED-LINE.
00362
00363      PERFORM 4000-START-BROWSE THRU 4010-START-BROWSE-EXIT.
00364
00365      IF BAD-BROWSE
00366         MOVE ER-0064             TO EMI-ERROR
00367         PERFORM 9900-ERROR-FORMAT
00368            THRU 9900-EXIT
00369         MOVE 'W'                 TO EOF-SWITCH
00370         PERFORM 3100-FILL-SCREEN THRU 3110-FILL-SCREEN-EXIT
00371              VARYING COUNT-1 FROM 1 BY 1 UNTIL COUNT-1 GREATER 15
00372         GO TO 8110-SEND-DATA.
00373
00374      PERFORM 4020-READ-FILE THRU 4040-READ-FILE-EXIT.
00375
00376      IF (EIBTRNID NOT = THIS-TRANS OR EIBAID = DFHPF5
00377         OR EIBAID = DFHPF6 OR EIBAID = DFHPF7)
00378           AND ERROR-SCREEN NOT = SAVE-CONTROL
00379         MOVE ER-0064             TO EMI-ERROR
00380         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00381         MOVE 'W'                 TO EOF-SWITCH
00382         PERFORM 3100-FILL-SCREEN THRU 3110-FILL-SCREEN-EXIT
00383               VARYING COUNT-1 FROM 1 BY 1 UNTIL COUNT-1 GREATER 15
00384         GO TO 8110-SEND-DATA.
00385
00386      IF END-OF-FILE
00387         GO TO 2020-END-FILE.
00388
00389      MOVE FILE-READ-KEY          TO SAVE-KEY.
00390
00391      IF HELP-BY-SCREEN
00392         MOVE ERROR-SCREEN        TO SCODEO
00393      ELSE
00394         IF HELP-BY-ERROR
00395            MOVE ERROR-SCREEN     TO ECODEO
00396         ELSE
00397            MOVE SPACES           TO ECODEO  SCODEO.
00398
00399      PERFORM 3000-BUILD-SCREEN THRU 3010-BUILD-SCREEN-EXIT
00400          VARYING COUNT-1 FROM 1 BY 1 UNTIL COUNT-1 GREATER 15
00401                  OR END-OF-FILE.
00402
00403  2020-END-FILE.
00404      IF END-OF-FILE
00405          PERFORM 3100-FILL-SCREEN THRU 3110-FILL-SCREEN-EXIT
00406        VARYING COUNT-1 FROM COUNT-1 BY 1 UNTIL COUNT-1 GREATER 15.
00407
00408      PERFORM 4050-END-BROWSE THRU 4060-END-BROWSE-EXIT.
00409      GO TO 8110-SEND-DATA.
00410
00411  2100-BROWSE-BACKWARD.
00412      IF TOP-OF-FILE
00413          MOVE ER-0131            TO EMI-ERROR
00414          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00415          GO TO 8110-SEND-DATA.
00416
00417      IF LAST-REC-NOT-FOUND
00418          MOVE ER-0268            TO EMI-ERROR
00419          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00420          GO TO 8110-SEND-DATA.
00421
00422      MOVE SPACE TO EOF-SWITCH.
00423
00424      IF SAVE-LINE = 1 OR ZEROS
00425          GO TO 2200-NEW-SCREEN.
00426
00427      IF EIBAID = DFHPF2
00428         SUBTRACT 15                   FROM SAVE-LINE
00429      ELSE
00430         SUBTRACT 5                    FROM SAVE-LINE.
00431
00432      IF SAVE-LINE LESS ZERO
00433          MOVE ZEROES             TO SAVE-LINE.
00434
00435      MOVE SAVE-KEY               TO FILE-READ-KEY.
00436      GO TO 2010-SKIP-ADD.
00437      EJECT
00438  2200-NEW-SCREEN.
00439      MOVE SAVE-KEY               TO FILE-READ-KEY.
00440      PERFORM 4000-START-BROWSE THRU 4010-START-BROWSE-EXIT.
00441      PERFORM 4070-READ-PREV THRU 4090-READ-PREV-EXIT
00442      PERFORM 4070-READ-PREV THRU 4090-READ-PREV-EXIT.
00443
00444      IF SAVE-COMPANY-CD NOT = COMPANY-CODE
00445          MOVE ER-0131            TO EMI-ERROR
00446          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00447          GO TO 8110-SEND-DATA.
00448
00449      IF END-OF-FILE
00450         MOVE LOW-VALUES          TO ERROR-SCREEN.
00451
00452      MOVE ZEROS                  TO LINE-SEQUENCE.
00453      MOVE FILE-READ-KEY          TO SAVE-KEY.
00454      PERFORM 4050-END-BROWSE  THRU 4060-END-BROWSE-EXIT.
00455      GO TO 2010-SKIP-ADD.
00456
00457  2400-SKIP-TO-SCREEN.
00458      MOVE SPACES                 TO EOF-SWITCH.
00459      MOVE ZEROES                 TO LINE-SEQUENCE.
00460      MOVE 'S'                    TO HELP-TYPE.
00461
00462      IF SCODEI = LOW-VALUES OR SPACES
00463          MOVE SPACES             TO ERROR-SCREEN
00464      ELSE
00465          MOVE SCODEI             TO ERROR-SCREEN.
00466
00467      IF SCODEI = ZEROES
00468          MOVE SPACE              TO HELP-TYPE.
00469
00470      MOVE FILE-READ-KEY          TO SAVE-KEY
00471      GO TO 2010-SKIP-ADD.
00472
00473  2500-SKIP-TO-ERROR.
00474      MOVE SPACES                 TO EOF-SWITCH.
00475      MOVE ZEROS                  TO LINE-SEQUENCE.
00476      MOVE 'E'                    TO HELP-TYPE.
00477
00478      IF ECODEI = LOW-VALUES OR ZEROES
00479          MOVE SPACES             TO ERROR-SCREEN
00480      ELSE
00481          MOVE ECODEI             TO ERROR-SCREEN.
00482
00483      IF ECODEI = ZEROES
00484          MOVE SPACE              TO HELP-TYPE.
00485
00486      MOVE FILE-READ-KEY          TO SAVE-KEY
00487      GO TO 2010-SKIP-ADD.
00488      EJECT
00489  2600-USER-NOTES.
00490      MOVE SPACES                 TO  EOF-SWITCH.
00491      MOVE ZEROS                  TO  LINE-SEQUENCE.
00492      MOVE SAVE-HELP-TYPE         TO  HELP-TYPE.
00493      IF HELP-TYPE IS EQUAL TO 'S'
00494          IF SCODEI IS EQUAL TO LOW-VALUES OR SPACES
00495              MOVE SPACES         TO  ERROR-SCREEN
00496          ELSE
00497              MOVE SCODEI         TO  ERROR-SCREEN.
00498      IF HELP-TYPE IS EQUAL TO 'E'
00499          IF ECODEI IS EQUAL TO LOW-VALUES OR SPACES
00500              MOVE SPACES         TO  ERROR-SCREEN
00501          ELSE
00502              MOVE ECODEI         TO  ERROR-SCREEN.
00503      IF HELP-TYPE IS EQUAL TO 'S'
00504          IF SCODEI IS EQUAL TO ZEROS
00505              MOVE SPACE          TO  HELP-TYPE.
00506      IF HELP-TYPE IS EQUAL TO 'E'
00507          IF ECODEI IS EQUAL TO ZEROS
00508              MOVE SPACE          TO  HELP-TYPE.
00509      MOVE FILE-READ-KEY          TO  SAVE-KEY.
00510      GO TO 2010-SKIP-ADD.
00511      EJECT
00512  3000-BUILD-SCREEN.
00513      MOVE TX-TEXT-LINE           TO HOLD-LINE.
00514      MOVE FORMATTED-LINE         TO INFOO (COUNT-1).
00515      PERFORM 4020-READ-FILE THRU 4040-READ-FILE-EXIT.
00516
00517      IF TX-SCREEN-OR-ERROR NOT = SAVE-CONTROL
00518          MOVE 'E'                TO ERROR-SWITCH.
00519
00520  3010-BUILD-SCREEN-EXIT.
00521      EXIT.
00522
00523
00524  3100-FILL-SCREEN.
00525      MOVE SPACES                 TO FORMATTED-LINE.
00526      MOVE FORMATTED-LINE         TO INFOO (COUNT-1).
00527
00528  3110-FILL-SCREEN-EXIT.
00529      EXIT.
00530      EJECT
00531  4000-START-BROWSE.
00532      
      * EXEC CICS HANDLE CONDITION
00533 *        NOTOPEN (5000-HELP-NOT-OPEN)
00534 *        NOTFND  (4005-KEY-NOT-FOUND)
00535 *    END-EXEC.
      *    MOVE '"$JI                  ! % #00001204' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303031323034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00536
00537      
      * EXEC CICS STARTBR
00538 *        DATASET ('ELHELP')
00539 *        RIDFLD  (FILE-READ-KEY)
00540 *    END-EXEC.
           MOVE 'ELHELP' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00001209' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 FILE-READ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00541
00542      GO TO 4010-START-BROWSE-EXIT.
00543
00544  4005-KEY-NOT-FOUND.
00545         MOVE 'Z'                    TO EOF-SWITCH.
00546
00547  4010-START-BROWSE-EXIT.
00548      EXIT.
00549      EJECT
00550  4020-READ-FILE.
00551      
      * EXEC CICS HANDLE CONDITION
00552 *        NOTFND  (4030-END-OF-FILE)
00553 *        ENDFILE (4030-END-OF-FILE)
00554 *    END-EXEC.
      *    MOVE '"$I''                  ! & #00001223' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303031323233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00555
00556      
      * EXEC CICS READNEXT
00557 *        SET      (ADDRESS OF TEXT-FILES)
00558 *        DATASET  ('ELHELP')
00559 *        RIDFLD   (FILE-READ-KEY)
00560 *    END-EXEC.
           MOVE 'ELHELP' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00001228' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 FILE-READ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF TEXT-FILES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00561
00562      IF TX-COMPANY-CD NOT = SAVE-COMPANY-CD
00563            GO TO 4030-END-OF-FILE.
00564
00565      GO TO 4040-READ-FILE-EXIT.
00566
00567  4030-END-OF-FILE.
00568      MOVE 'E'                    TO ERROR-SWITCH.
00569      MOVE 'Y'                    TO EOF-SWITCH.
00570      MOVE ER-0130                TO EMI-ERROR.
00571      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00572
00573  4040-READ-FILE-EXIT.
00574      EXIT.
00575
00576  4050-END-BROWSE.
00577      
      * EXEC CICS ENDBR
00578 *        DATASET ('ELHELP')
00579 *    END-EXEC.
           MOVE 'ELHELP' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001249' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00580
00581  4060-END-BROWSE-EXIT.
00582      EXIT.
00583      EJECT
00584  4070-READ-PREV.
00585      
      * EXEC CICS HANDLE CONDITION
00586 *        ENDFILE (4080-END-OF-FILE)
00587 *        NOTFND  (4080-END-OF-FILE)
00588 *    END-EXEC.
      *    MOVE '"$''I                  ! '' #00001257' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303031323537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00589
00590      
      * EXEC CICS READPREV
00591 *        SET      (ADDRESS OF TEXT-FILES)
00592 *        DATASET  ('ELHELP')
00593 *        RIDFLD   (FILE-READ-KEY)
00594 *    END-EXEC.
           MOVE 'ELHELP' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00001262' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 FILE-READ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF TEXT-FILES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00595
00596      GO TO 4090-READ-PREV-EXIT.
00597
00598  4080-END-OF-FILE.
00599      MOVE 'E'                    TO ERROR-SWITCH.
00600      MOVE 'X'                    TO EOF-SWITCH.
00601      MOVE ER-0131                TO EMI-ERROR.
00602      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00603
00604  4090-READ-PREV-EXIT.
00605      EXIT.
00606
00607  5000-HELP-NOT-OPEN.
00608      MOVE ER-0015                TO EMI-ERROR.
00609      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00610      GO TO 8110-SEND-DATA.
00611      EJECT
00612  8100-SEND-INITIAL-MAP.
00613      PERFORM 8130-FORMAT-TIME-DATE
00614              THRU 8140-FORMAT-TIME-DATE-EXIT.
00615
00616      
      * EXEC CICS SEND
00617 *        MAP     ('EL010A')
00618 *        MAPSET  ('EL010S')
00619 *        ERASE
00620 *        FREEKB
00621 *    END-EXEC.
           MOVE 'EL010A' TO DFHEIV1
           MOVE 'EL010S' TO DFHEIV2
      *    MOVE '8$      T  E F  H     ,   #00001288' TO DFHEIV0
           MOVE X'382420202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL010AO, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00622
00623      GO TO 9000-RETURN-TRANS.
00624
00625  8110-SEND-DATA.
00626      IF EIBTRNID NOT = THIS-TRANS
00627         GO TO 8100-SEND-INITIAL-MAP.
00628
00629      PERFORM 8130-FORMAT-TIME-DATE
00630              THRU 8140-FORMAT-TIME-DATE-EXIT.
00631
00632      
      * EXEC CICS SEND
00633 *        MAP     ('EL010A')
00634 *        MAPSET  ('EL010S')
00635 *        DATAONLY
00636 *        FREEKB
00637 *    END-EXEC.
           MOVE 'EL010A' TO DFHEIV1
           MOVE 'EL010S' TO DFHEIV2
      *    MOVE '8$D     T    F  H     ,   #00001304' TO DFHEIV0
           MOVE X'382444202020202054202020' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL010AO, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00638
00639      GO TO 9000-RETURN-TRANS.
00640
00641  8130-FORMAT-TIME-DATE.
00642      
      * EXEC CICS ASKTIME ABSTIME(LCP-CICS-DATE)
00643 *    END-EXEC
      *    MOVE '0"A                   "   #00001314' TO DFHEIV0
           MOVE X'302241202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-DATE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00644      
      * EXEC CICS FORMATTIME
00645 *              ABSTIME(LCP-CICS-DATE)
00646 *              YYMMDD(LCP-CURRENT-DATE-68)
00647 *              DATESEP('/')
00648 *    END-EXEC
           MOVE '/' TO DFHEIV9
      *    MOVE 'j$((   "              $   #00001316' TO DFHEIV0
           MOVE X'6A2428282020202220202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-DATE, 
                 LCP-CURRENT-DATE-68, 
                 DFHEIV9
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00649      MOVE LCP-CURRENT-DATE-68 TO DATEO.
00650      
      * EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)
00651 *    END-EXEC
      *    MOVE '0"A                   "   #00001322' TO DFHEIV0
           MOVE X'302241202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00652      
      * EXEC CICS FORMATTIME
00653 *              ABSTIME(LCP-CICS-TIME)
00654 *              TIME(LCP-TIME-OF-DAY-XX)
00655 *    END-EXEC
      *    MOVE 'j$(     (             #   #00001324' TO DFHEIV0
           MOVE X'6A2428202020202028202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME, 
                 LCP-TIME-OF-DAY-XX
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00656      MOVE  LCP-TIME-OF-DAY-68 TO TIME-UNFORMATTED.
00657      MOVE UN-HOURS               TO FOR-HOURS.
00658      MOVE UN-MINUTES             TO FOR-MINUTES.
00659      MOVE TIME-FORMATTED         TO TIMEO.
00660      MOVE EMI-MESSAGE-AREA (1)   TO MSGO.
00661
00662  8140-FORMAT-TIME-DATE-EXIT.
00663      EXIT.
00664
00665  8800-UNAUTHORIZED-ACCESS.
00666      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
00667      GO TO 8990-SEND-TEXT.
00668
00669  8820-XCTL-ERROR.
00670      
      * EXEC CICS HANDLE CONDITION
00671 *        PGMIDERR (8880-SIGN-OFF-ERROR)
00672 *    END-EXEC.
      *    MOVE '"$L                   ! ( #00001342' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303031333432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00673
00674      MOVE SPACE                  TO PI-ENTRY-CD-1.
00675      MOVE CALL-PGM               TO PI-CALLING-PROGRAM.
00676      MOVE XCTL-EL005             TO CALL-PGM.
00677      GO TO 9200-XCTL.
00678
00679  8880-SIGN-OFF-ERROR.
00680      MOVE CALL-PGM               TO LOGOFF-PGM.
00681      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
00682
00683  8990-SEND-TEXT.
00684      
      * EXEC CICS SEND TEXT
00685 *        FROM    (LOGOFF-TEXT)
00686 *        LENGTH  (LOGOFF-LENGTH)
00687 *        ERASE
00688 *        FREEKB
00689 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00001356' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333536' TO DFHEIV0(25:11)
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
           
00690
00691      GO TO 9100-RETURN-CICS.
00692      EJECT
00693  9000-RETURN-TRANS.
00694      
      * EXEC CICS RETURN
00695 *        TRANSID   (TRANS-ID)
00696 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
00697 *        LENGTH    (WS-COMM-LENGTH)
00698 *    END-EXEC.
      *    MOVE '.(CT                  &   #00001366' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00699
00700      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL010' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00701
00702  9100-RETURN-CICS.
00703      
      * EXEC CICS RETURN
00704 *    END-EXEC.
      *    MOVE '.(                    &   #00001375' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00705
00706      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL010' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00707
00708  9200-XCTL.
00709      
      * EXEC CICS XCTL
00710 *        PROGRAM   (CALL-PGM)
00711 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
00712 *        LENGTH    (WS-COMM-LENGTH)
00713 *    END-EXEC.
      *    MOVE '.$C                   $   #00001381' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CALL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00714
00715  9900-ERROR-FORMAT.
00716      IF NOT EMI-ERRORS-COMPLETE
00717
00718          
      * EXEC CICS LINK
00719 *            PROGRAM   ('EL001')
00720 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
00721 *            LENGTH    (EMI-COMM-LENGTH)
00722 *        END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   ''   #00001390' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00723
00724  9900-EXIT.
00725      EXIT.
00726
00727  9990-ABEND.
00728      
      * EXEC CICS LINK
00729 *        PROGRAM   ('EL004')
00730 *        COMMAREA  (DFHEIBLK)
00731 *        LENGTH    (64)
00732 *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 64
             TO DFHEIV11
      *    MOVE '."C                   ''   #00001400' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIBLK, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00733
00734      GO TO 9100-RETURN-CICS.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL010' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8820-XCTL-ERROR,
                     0050-MAPFAIL-HANDLE,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0410-BUFF-KEY-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0420-DELETE-TS-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 5000-HELP-NOT-OPEN,
                     4005-KEY-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 4030-END-OF-FILE,
                     4030-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 4080-END-OF-FILE,
                     4080-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8880-SIGN-OFF-ERROR
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL010' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
