00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL9301.
00004 *                            VMOD=2.006
00005
00006  AUTHOR.     LOGIC,INC.
00007              DALLAS, TEXAS.
00008
00009  DATE-COMPILED.
00010  SECURITY.   *****************************************************
00011              *                                                   *
00012              *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00013              *                                                   *
00014              *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00015              *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00016              *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *
00017              *                                                   *
00018              *****************************************************
00019
00020  REMARKS. TRANSACTION - EXI2 - NEW BUSINESS - DATA ENTRY (ISSUES).
00021
00022  ENVIRONMENT DIVISION.
00023
00024      EJECT
00025  DATA DIVISION.
00026  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00027  77  FILLER  PIC X(32)  VALUE '*** PAN EL9301    PANLVL 012 ***'.
00028  77  FILLER  PIC X(32)  VALUE '********************************'.
00029  77  FILLER  PIC X(32)  VALUE '*    EL9301 WORKING STORAGE    *'.
00030  77  FILLER  PIC X(32)  VALUE '******VMOD=2.006****************'.
00031
00032 *                            COPY ELCSCTM.
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
00033
00034 *                            COPY ELCSCRTY.
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
00035
00036  01  STANDARD-AREAS.
00037      12  GETMAIN-SPACE       PIC X       VALUE SPACE.
00038      12  EL930B              PIC X(8)    VALUE 'EL930B  '.
00039      12  EL930C              PIC X(8)    VALUE 'EL930C  '.
00040      12  MAPSET-EL9301S      PIC X(8)    VALUE 'EL9301S '.
00041      12  SCREEN-NUMBER       PIC X(6)    VALUE 'EL930B'.
00042      12  TRANS-EXI2          PIC X(4)    VALUE 'EXI2'.
00043      12  THIS-PGM            PIC X(8)    VALUE 'EL9301  '.
00044      12  PGM-NAME            PIC X(8).
00045      12  TIME-IN             PIC S9(7).
00046      12  TIME-OUT-R  REDEFINES TIME-IN.
00047          16  FILLER          PIC X.
00048          16  TIME-OUT        PIC 99V99.
00049          16  FILLER          PIC X(2).
00050      12  LINK-EL001          PIC X(8)    VALUE 'EL001   '.
00051      12  LINK-EL004          PIC X(8)    VALUE 'EL004   '.
00052      12  XCTL-EL005          PIC X(8)    VALUE 'EL005   '.
00053      12  XCTL-EL010          PIC X(8)    VALUE 'EL010   '.
00054      12  XCTL-EL626          PIC X(8)    VALUE 'EL626   '.
00055      12  XCTL-EL930          PIC X(8)    VALUE 'EL930   '.
00056      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV '.
00057      12  FILE-ID-ERPNDT      PIC X(8)    VALUE 'ERPNDT  '.
00058      12  FILE-ID-ERPNDM      PIC X(8)    VALUE 'ERPNDM  '.
00059      12  FILE-ID-ELCERT      PIC X(8)    VALUE 'ELCERT  '.
00060      12  FILE-ID-ELCNTL      PIC X(8)    VALUE 'ELCNTL  '.
00061      12  FILE-ID-ERACCT2     PIC X(8)    VALUE 'ERACCT2 '.
00062      12  WS-CURRENT-DT       PIC X(8)    VALUE SPACES.
00063      12  WS-CURRENT-BIN-DT   PIC XX      VALUE SPACES.
00064      12  WS-TERM-IN-DAYS-SW  PIC X.
00065          88  WS-TERM-IN-DAYS-FOUND       VALUE 'Y'.
00066      12  W-TEST-ZIP          PIC X.
00067          88  W-CANADIAN-POST-CODE   VALUE 'A' THRU 'Z'.
00068
00069      EJECT
00070
00071  01  ERROR-MESSAGES.
00072      12  ER-0000                 PIC X(4)  VALUE '0000'.
00073      12  ER-0004                 PIC X(4)  VALUE '0004'.
00074      12  ER-0008                 PIC X(4)  VALUE '0008'.
00075      12  ER-0029                 PIC X(4)  VALUE '0029'.
00076      12  ER-0070                 PIC X(4)  VALUE '0070'.
00077      12  ER-0194                 PIC X(4)  VALUE '0194'.
00078      12  ER-0195                 PIC X(4)  VALUE '0195'.
00079      12  ER-0196                 PIC X(4)  VALUE '0196'.
00080      12  ER-0197                 PIC X(4)  VALUE '0197'.
00081      12  ER-2208                 PIC X(4)  VALUE '2208'.
00082      12  ER-2209                 PIC X(4)  VALUE '2209'.
00083      12  ER-2210                 PIC X(4)  VALUE '2210'.
00084      12  ER-2212                 PIC X(4)  VALUE '2212'.
00085      12  ER-2217                 PIC X(4)  VALUE '2217'.
00086      12  ER-2218                 PIC X(4)  VALUE '2218'.
00087      12  ER-2200                 PIC X(4)  VALUE '2200'.
00088      12  ER-2220                 PIC X(4)  VALUE '2220'.
00089      12  ER-2222                 PIC X(4)  VALUE '2222'.
00090      12  ER-2223                 PIC X(4)  VALUE '2223'.
00091      12  ER-2226                 PIC X(4)  VALUE '2226'.
00092      12  ER-2227                 PIC X(4)  VALUE '2227'.
00093      12  ER-2228                 PIC X(4)  VALUE '2228'.
00094      12  ER-2240                 PIC X(4)  VALUE '2240'.
00095      12  ER-2241                 PIC X(4)  VALUE '2241'.
00096      12  ER-2247                 PIC X(4)  VALUE '2247'.
00097      12  ER-2423                 PIC X(4)  VALUE '2423'.
00098      12  ER-2424                 PIC X(4)  VALUE '2424'.
00099      12  ER-2425                 PIC X(4)  VALUE '2425'.
00100      12  ER-2426                 PIC X(4)  VALUE '2426'.
00101      12  ER-2427                 PIC X(4)  VALUE '2427'.
00102      12  ER-2428                 PIC X(4)  VALUE '2428'.
00103      12  ER-2431                 PIC X(4)  VALUE '2431'.
00104      12  ER-2433                 PIC X(4)  VALUE '2433'.
00105      12  ER-2437                 PIC X(4)  VALUE '2437'.
00106      12  ER-2429                 PIC X(4)  VALUE '2429'.
00107      12  ER-2442                 PIC X(4)  VALUE '2442'.
00108      12  ER-2471                 PIC X(4)  VALUE '2471'.
00109      12  ER-2526                 PIC X(4)  VALUE '2526'.
00110      12  ER-2529                 PIC X(4)  VALUE '2529'.
00111      12  ER-2531                 PIC X(4)  VALUE '2531'.
00112      12  ER-2532                 PIC X(4)  VALUE '2532'.
00113      12  ER-2541                 PIC X(4)  VALUE '2541'.
00114      12  ER-2542                 PIC X(4)  VALUE '2542'.
00115      12  ER-2589                 PIC X(4)  VALUE '2589'.
00116      12  ER-2591                 PIC X(4)  VALUE '2591'.
00117      12  ER-2592                 PIC X(4)  VALUE '2592'.
00118      12  ER-2593                 PIC X(4)  VALUE '2593'.
00119      12  ER-2594                 PIC X(4)  VALUE '2594'.
00120      12  ER-2629                 PIC X(4)  VALUE '2629'.
00121      12  ER-2630                 PIC X(4)  VALUE '2630'.
00122      12  ER-2635                 PIC X(4)  VALUE '2635'.
00123      12  ER-2636                 PIC X(4)  VALUE '2636'.
00124      12  ER-2651                 PIC X(4)  VALUE '2651'.
00125      12  ER-2901                 PIC X(4)  VALUE '2901'.
00126      12  ER-7400                 PIC X(4)  VALUE '7400'.
00127      12  ER-7403                 PIC X(4)  VALUE '7403'.
00128      12  ER-7404                 PIC X(4)  VALUE '7404'.
00129      12  ER-7405                 PIC X(4)  VALUE '7405'.
00130      12  ER-7423                 PIC X(4)  VALUE '7423'.
00131      12  ER-7424                 PIC X(4)  VALUE '7424'.
00132      12  ER-7530                 PIC X(4)  VALUE '7530'.
00133      12  ER-7632                 PIC X(4)  VALUE '7632'.
00134      12  ER-7573                 PIC X(4)  VALUE '7573'.
00135      12  ER-7630                 PIC X(4)  VALUE '7630'.
00136      12  ER-7631                 PIC X(4)  VALUE '7631'.
00137      12  ER-7633                 PIC X(4)  VALUE '7633'.
00138      12  ER-7997                 PIC X(4)  VALUE '7997'.
00139      12  ER-7998                 PIC X(4)  VALUE '7998'.
00140      12  ER-9999                 PIC X(4)  VALUE '9999'.
00141
00142      EJECT
00143
00144
00145  01  ACCESS-KEYS.
00146
00147      12  ERPNDT-KEY.
00148          16  ERPNDT-COMP-CD          PIC X     VALUE SPACE.
00149          16  ERPNDT-ENTRY-BATCH      PIC X(6)  VALUE SPACES.
00150          16  ERPNDT-BATCH-SEQ        PIC S9(4) VALUE +1 COMP.
00151          16  ERPNDT-BATCH-CHG-SEQ    PIC S9(4) VALUE +0 COMP.
00152
00153      12  ERPNDT-RECORD-LENGTH        PIC S9(4) COMP VALUE +585.
00154      12  ERPNDT-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +608.
00155
00156      12  ELCNTL-KEY.
00157          16  ELCNTL-COMPANY-ID       PIC X(3)  VALUE SPACES.
00158          16  ELCNTL-REC-TYPE         PIC X     VALUE SPACES.
00159          16  ELCNTL-ACCESS.
00160              20  FILLER              PIC XX.
00161              20  ELCNTL-HI-BEN       PIC XX.
00162          16  ELCNTL-ST-ACCESS REDEFINES ELCNTL-ACCESS.
00163              20  ELCNTL-STATE        PIC XX.
00164              20  FILLER              PIC X.
00165              20  ELCNTL-CARRIER        PIC X.
00166          16  ELCNTL-SEQ              PIC S9(4) VALUE +0 COMP.
00167
00168      12  ELCNTL-RECORD-LENGTH        PIC S9(4) COMP VALUE +504.
00169      12  ELCNTL-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +527.
00170
00171      12  ELCERT-KEY.
00172          16  ELCERT-COMPANY-CD       PIC X.
00173          16  ELCERT-CARRIER          PIC X.
00174          16  ELCERT-GROUPING         PIC X(6).
00175          16  ELCERT-STATE            PIC XX.
00176          16  ELCERT-ACCOUNT          PIC X(10).
00177          16  ELCERT-CERT-EFF-DT      PIC XX.
00178          16  ELCERT-CERT-NO.
00179              20  ELCERT-CERT-PRIME   PIC X(10).
00180              20  ELCERT-CERT-SFX     PIC X.
00181
00182      12  ELCERT-RECORD-LENGTH        PIC S9(4) COMP VALUE +450.
00183      12  ELCERT-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +473.
00184
00185      12  ERPNDM-KEY.
00186          16  ERPNDM-COMP-CD          PIC X     VALUE SPACE.
00187          16  ERPNDM-ENTRY-BATCH      PIC X(6)  VALUE SPACES.
00188          16  ERPNDM-BATCH-SEQ        PIC S9(4) VALUE +1 COMP.
00189          16  ERPNDM-BATCH-CHG-SEQ    PIC S9(4) VALUE +0 COMP.
00190
00191      12  ERPNDM-RECORD-LENGTH        PIC S9(4) COMP VALUE +250.
00192      12  ERPNDM-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +273.
00193
00194      12  ERACCT-KEY.
00195          16  ERACCT-COMP-KEY.
00196              20  ERACCT-CO       PIC X     VALUE SPACES.
00197              20  ERACCT-CARRIER  PIC X     VALUE SPACES.
00198              20  ERACCT-GROUPING PIC X(6)  VALUE SPACES.
00199              20  ERACCT-STATE    PIC XX    VALUE SPACES.
00200              20  ERACCT-ACCOUNT  PIC X(10) VALUE SPACES.
00201          16  ERACCT-EXP-DATE     PIC XX    VALUE SPACES.
00202          16  FILLER              PIC X(4)  VALUE LOW-VALUES.
00203      12  ERACCT-SAVE-KEY         PIC X(20) VALUE SPACES.
00204
00205      EJECT
00206
00207  01  WORK-AREA.
00208
00209      12  DEEDIT-FIELD            PIC X(15).
00210      12  FILLER REDEFINES DEEDIT-FIELD.
00211          16  FILLER              PIC X(4).
00212          16  DEEDIT-FIELD-X11    PIC X(11).
00213      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD PIC S9(15).
00214      12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD PIC S9(13)V99.
00215      12  DEEDIT-FIELD-V3 REDEFINES DEEDIT-FIELD PIC S9(12)V9(3).
00216      12  DEEDIT-FIELD-V4 REDEFINES DEEDIT-FIELD PIC S9(11)V9(4).
00217      12  DEEDIT-FIELD-V5 REDEFINES DEEDIT-FIELD PIC S9(10)V9(5).
00218
00219      12  WS-SUB                  PIC S9(4) VALUE +0  COMP.
00220      12  WS-SUB1                 PIC S9(4) VALUE +0  COMP.
00221      12  WS-SUB2                 PIC S9(4) VALUE +0  COMP.
00222      12  WS-SUB3                 PIC S9(4) VALUE +0  COMP.
00223      12  WS-COV-SUB              PIC S9(4) VALUE +0  COMP.
00224      12  WS-EDIT-SUB             PIC S9(4) VALUE +0  COMP.
00225
00226      12  WS-CALC-TERM            PIC S999V9(5) VALUE ZEROS.
00227      12  WS-CALC-TERM-R REDEFINES WS-CALC-TERM.
00228          16  WS-CALC-TERM-WHOLE  PIC S999.
00229          16  WS-CALC-TERM-REMAIN PIC SV9(5).
00230
00231      12  ERROR-SW                PIC X     VALUE SPACE.
00232          88  NO-ERROR                VALUE SPACE.
00233          88  ERRORS                  VALUE 'Y'.
00234
00235      12  WS-BROWSE-SW            PIC X     VALUE SPACE.
00236          88  BROWSE-NOT-STARTED      VALUE SPACE.
00237          88  BROWSE-STARTED          VALUE 'Y'.
00238      12  WS-FIRST-TIME-SW        PIC X     VALUE SPACE.
00239          88 FIRST-TIME               VALUE '0'.
00240          88 NOT-FIRST-TIME           VALUE '1'.
00241      12  WS-DATA-KEYED-SW        PIC X     VALUE SPACE.
00242          88  WS-DATA-NOT-KEYED       VALUE SPACE.
00243          88  WS-DATA-KEYED           VALUE 'Y'.
00244
00245      12  WS-EDITED-LF-CODE       PIC XX   VALUE ZEROS.
00246      12  WS-LF-ABBR-DESC         PIC XXX  VALUE SPACES.
00247
00248      12  WS-EDITED-AH-CODE       PIC XX   VALUE ZEROS.
00249      12  WS-AH-ABBR-DESC         PIC XXX  VALUE SPACES.
00250
00251      12  WS-BEN-CD               PIC XX   VALUE SPACES.
00252
00253      12  WS-ENTRY-CODE           PIC X     VALUE SPACE.
00254          88  WS-ENTRY-CODE-VALID   VALUE ' ' 'E' 'R' 'P'.
00255
00256      12  WS-ALL-NINES            PIC S9(7)V99 VALUE +9999999.99.
00257
00258      12  WS-MODE-CODE            PIC X     VALUE SPACE.
00259          88 WS-MODE-CODE-VALID     VALUE ' ' 'M' 'W' 'S' 'B'.
00260
00261      12  WS-KIND                 PIC XX    VALUE SPACE.
00262          88 WS-KIND-LF             VALUE 'LF'.
00263          88 WS-KIND-AH             VALUE 'AH'.
00264          88 WS-KIND-PR             VALUE 'PR'.
00265          88 WS-KIND-UE             VALUE 'UE'.
00266          88 WS-KIND-DI             VALUE 'DI'.
00267          88 WS-KIND-MONTHLY        VALUE 'AH' 'UE'.
00268
00269      12  WS-JOURNAL-RECORD-LENGTH   PIC S9(4) COMP VALUE +0000.
00270
00271      12  WS-EDIT-CODE            PIC X(4)  VALUE SPACES.
00272
00273      12  WS-SAVE-INPUT-FIELDS.
00274
00275          16  WS-BAGE                 PIC 99       VALUE ZERO.
00276          16  WS-BJNT-AGE             PIC 99       VALUE ZERO.
00277          16  WS-BDAYS                PIC 999      VALUE ZERO.
00278          16  WS-BLN-TERM             PIC 999      VALUE ZERO.
00279          16  WS-BFREQ                PIC 99       VALUE ZERO.
00280          16  WS-BPHONE               PIC 9(12)    VALUE 0 COMP-3.
00281          16  WS-BAPR                 PIC S99V9(4) VALUE +0 COMP-3.
00282          16  WS-BPMT                 PIC S9(6)V99 VALUE +0 COMP-3.
00283          16  WS-BPMTS                PIC S999     VALUE +0 COMP-3.
00284          16  WS-BLIVES               PIC 9(3)      COMP-3.
00285
00286          16  WS-B-COVERAGE OCCURS 2 TIMES.
00287              20  WS-BTERM            PIC 999       COMP-3.
00288              20  WS-BCRIT-PERD       PIC 99        COMP-3.
00289              20  WS-BBEN             PIC S9(10)V99 COMP-3.
00290              20  WS-BALT-BEN     PIC S9(10)V99 COMP-3.
00291              20  WS-BPREM        PIC S9(10)V99 COMP-3.
00292              20  WS-BALT-PREM    PIC S9(07)V99 COMP-3.
00293
00294          16  WS-C-FIELDS   OCCURS 4 TIMES.
00295              20  WS-CLIVES      PIC 9(3)          COMP-3.
00296              20  WS-CREFUND1    PIC S9(7)V99      COMP-3.
00297              20  WS-CREFUND2    PIC S9(7)V99      COMP-3.
00298
00299      12  WS-CONVERTED-BIRTH  PIC XX    VALUE SPACE.
00300      12  WS-CONVERTED-EFFDT      PIC XX    VALUE SPACES.
00301      12  WS-CONVERTED-1ST-PMT-DT PIC XX    VALUE SPACES.
00302      12  WS-CONVERTED-EXPIRDT      OCCURS 2 TIMES PIC XX.
00303      12  WS-CONVERTED-CANCEL-DATES OCCURS 4 TIMES.
00304          16  WS-CONVERTED-CANDT1 PIC XX.
00305          16  WS-CONVERTED-CANDT2 PIC XX.
00306
00307      12  WS-FIRST-NAME.
00308          16  WS-1ST-INIT         PIC X.
00309          16  FILLER              PIC X(9).
00310
00311      12  WS-INITIALS.
00312          16  WS-INITIAL-1        PIC X.
00313          16  WS-INITIAL-2        PIC X.
00314
00315      EJECT
00316
00317  01  CLASIC-WARNING.
00318      12  WARNING-LENGTH              PIC S9(4)  VALUE +124 COMP.
00319      12  WARNING-TEXT.
00320          16  FILLER                  PIC X(80)  VALUE
00321              'THIS DATA MAY HAVE PREVIOUSLY BEEN PROCESSED'.
00322          16  FILLER                  PIC X(44)  VALUE
00323              'CONTACT LOGIC INC. FOR FURTHER INFORMATION'.
00324
00325      EJECT
00326
00327 *                            COPY ELCDATE.
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
00328
00329      EJECT
00330 *                            COPY ELCLOGOF.
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
00331
00332      EJECT
00333 *                            COPY ELCATTR.
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
00334
00335      EJECT
00336 *                                  COPY ELCEMIB.
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
00337
00338      EJECT
00339 *                            COPY ELCINTF.
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
00340 *    COPY ELC930PI.
00001
00002 ******************************************************************
00003 *                            ELC930PI                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *    THIS IS THE PI-PROGRAM-WORK-AREA THAT IS USED FOR THE DATA  *
00007 *    ENTRY SUB-SYSTEM.                                           *
00008 *                                                                *
00009 *    THE FOLLOWING PROGRAMS USE THIS COPYBOOK.                   *
00010 *                                                                *
00011 *               EL930 - EL9301 - EL9302                          *
00012 ******************************************************************
00013
00014      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
00015          16  PI-AM-NAME                  PIC X(30).
00016          16  PI-MAP-NAME                 PIC X(8).
00017          16  PI-BATCH-AMOUNTS    COMP-3.
00018              20  PI-LF-ISS-REMITTED      PIC S9(8)V99.
00019              20  PI-LF-ISS-ENTERED       PIC S9(8)V99.
00020              20  PI-LF-CAN-REMITTED      PIC S9(8)V99.
00021              20  PI-LF-CAN-ENTERED       PIC S9(8)V99.
00022              20  PI-AH-ISS-REMITTED      PIC S9(8)V99.
00023              20  PI-AH-ISS-ENTERED       PIC S9(8)V99.
00024              20  PI-AH-CAN-REMITTED      PIC S9(8)V99.
00025              20  PI-AH-CAN-ENTERED       PIC S9(8)V99.
00026              20  PI-ISS-CNT-REMITTED     PIC S9(5).
00027              20  PI-ISS-CNT-ENTERED      PIC S9(5).
00028              20  PI-CAN-CNT-REMITTED     PIC S9(5).
00029              20  PI-CAN-CNT-ENTERED      PIC S9(5).
00030          16  PI-MAINT-FUNC               PIC X.
00031          16  PI-ERROR-SW                 PIC X.
00032              88  PI-DATA-ERRORS              VALUE 'Y'.
00033          16  PI-UPDATE-SW                PIC X.
00034              88  PI-DATA-UPDATED             VALUE 'Y'.
00035          16  PI-DISPLAY-SW               PIC X.
00036              88  PI-LAST-FUNC-DISPLAY        VALUE 'Y'.
00037          16  PI-SAVE-CALLING-PGM         PIC X(8).
00038          16  PI-LAST-SEQ-NO-ADDED        PIC S9(4) COMP.
00039          16  PI-NEXT-DISPLAY-SEQ-NO      PIC S9(4) COMP.
00040          16  PI-SAV-CARRIER              PIC X.
00041          16  PI-SAV-GROUPING             PIC X(6).
00042          16  PI-SAV-STATE                PIC XX.
00043          16  PI-SAV-ACCOUNT              PIC X(10).
00044          16  PI-SAV-CERT-EFF-DT          PIC XX.
00045          16  PI-SAV-CERT-NO.
00046              20  PI-SAV-CERT-PRIME       PIC X(14).
00047              20  PI-SAV-CERT-SFX         PIC X.
00048          16  PI-SAV-ENDING-ERPNDT-KEY.
00049              20  PI-SAV-COMP-CD          PIC X.
00050              20  PI-SAV-ENTRY-BATCH      PIC X(6).
00051              20  PI-SAV-BATCH-SEQ        PIC S9(4) COMP.
00052              20  PI-SAV-BATCH-CHG-SEQ    PIC S9(4) COMP.
00053          16  PI-VERIFY-DELETE-SW         PIC X.
00054              88  PI-DELETE-IS-OK             VALUE 'Y'.
00055          16  PI-EL930-FIRST-TIME-SW      PIC X.
00056              88  PI-EL930-FIRST-TIME         VALUE SPACE.
00057          16  PI-CREDIT-EDIT-CONTROLS.
00058              20  PI-MIN-PREMIUM          PIC S9(3)V99  COMP-3.
00059              20  PI-MIN-AGE              PIC 99.
00060              20  PI-DEFAULT-AGE          PIC 99.
00061              20  PI-MIN-TERM             PIC S9(3)     COMP-3.
00062              20  PI-MAX-TERM             PIC S9(3)     COMP-3.
00063              20  PI-DEFAULT-SEX          PIC X.
00064              20  PI-JOINT-AGE-INPUT      PIC X.
00065                  88 PI-JOINT-AGE-IS-INPUT       VALUE '1'.
00066              20  PI-BIRTH-DATE-INPUT     PIC X.
00067                  88 PI-BIRTH-DATE-IS-INPUT      VALUE '1'.
00068          16  PI-KEYED-SWITCHES.
00069              20  PI-ISS-SUFFIX-KEYED-SW  PIC X.
00070                  88  PI-ISS-SUFFIX-KEYED     VALUE 'Y'.
00071              20  PI-CAN-SUFFIX-KEYED-SW  PIC X.
00072                  88  PI-CAN-SUFFIX-KEYED     VALUE 'Y'.
00073              20  PI-IG-KEYED-SW          PIC X.
00074                  88  PI-IG-KEYED             VALUE 'Y'.
00075              20  PI-APR-KEYED-SW         PIC X.
00076                  88  PI-APR-KEYED            VALUE 'Y'.
00077              20  PI-FREQ-KEYED-SW        PIC X.
00078                  88  PI-FREQ-KEYED           VALUE 'Y'.
00079              20  PI-SIG-KEYED-SW         PIC X.
00080                  88  PI-SIG-KEYED            VALUE 'Y'.
00081              20  PI-LFRT-KEYED-SW        PIC X.
00082                  88  PI-LFRT-KEYED           VALUE 'Y'.
00083              20  PI-AHRT-KEYED-SW        PIC X.
00084                  88  PI-AHRT-KEYED           VALUE 'Y'.
00085              20  PI-SSNUM-KEYED-SW       PIC X.
00086                  88  PI-SSNUM-KEYED          VALUE 'Y'.
00087              20  PI-JNT-SSNUM-KEYED-SW   PIC X.
00088                  88  PI-JNT-SSNUM-KEYED      VALUE 'Y'.
00089              20  PI-MEMBER-KEYED-SW      PIC X.
00090                  88  PI-MEMBER-KEYED         VALUE 'Y'.
00091              20  PI-MODE-KEYED-SW        PIC X.
00092                  88  PI-MODE-KEYED           VALUE 'Y'.
00093              20  PI-PMTS-KEYED-SW        PIC X.
00094                  88  PI-PMTS-KEYED           VALUE 'Y'.
00095              20  PI-LN-OFFICER-KEYED-SW  PIC X.
00096                  88  PI-LN-OFFICER-KEYED     VALUE 'Y'.
00097              20  PI-ENTRY-KEYED-SW       PIC X.
00098                  88  PI-ENTRY-KEYED          VALUE 'Y'.
00099              20  PI-RINCD-KEYED-SW       PIC X.
00100                  88  PI-RINCD-KEYED          VALUE 'Y'.
00101              20  PI-RTCLS-KEYED-SW       PIC X.
00102                  88  PI-RTCLS-KEYED          VALUE 'Y'.
00103              20  PI-LNTRM-KEYED-SW       PIC X.
00104                  88  PI-LNTRM-KEYED          VALUE 'Y'.
00105              20  PI-EXPIR-KEYED-SW       PIC X.
00106                  88  PI-EXPIR-KEYED          VALUE 'Y'.
00107              20  PI-PMT-KEYED-SW         PIC X.
00108                  88  PI-PMT-KEYED            VALUE 'Y'.
00109              20  PI-1ST-PMT-KEYED-SW     PIC X.
00110                  88  PI-1ST-PMT-KEYED        VALUE 'Y'.
00111              20  PI-DAYS-KEYED-SW        PIC X.
00112                  88  PI-DAYS-KEYED           VALUE 'Y'.
00113              20  PI-SKPCD-KEYED-SW       PIC X.
00114                  88  PI-SKPCD-KEYED          VALUE 'Y'.
00115              20  PI-JNT-AGE-KEYED-SW     PIC X.
00116                  88  PI-JNT-AGE-KEYED        VALUE 'Y'.
00117              20  PI-JNT-NAME-KEYED-SW    PIC X.
00118                  88  PI-JNT-NAME-KEYED       VALUE 'Y'.
00119              20  PI-ISS-LIVES-KEYED-SW   PIC X.
00120                  88  PI-ISS-LIVES-KEYED      VALUE 'Y'.
00121              20  PI-CAN-LIVES-KEYED-SW   PIC X.
00122                  88  PI-CAN-LIVES-KEYED      VALUE 'Y'.
00123              20  PI-PAYEE-KEYED-SW       PIC X.
00124                  88  PI-PAYEE-KEYED          VALUE 'Y'.
00125              20  PI-CHK-REQ-KEYED-SW     PIC X.
00126                  88  PI-CHK-REQ-KEYED        VALUE 'Y'.
00127              20  PI-ZIP4-KEYED-SW        PIC X.
00128                  88  PI-ZIP4-KEYED           VALUE 'Y'.
00129              20  PI-POLICY-KEYED-SW      PIC X.
00130                  88  PI-POLICY-KEYED         VALUE 'Y'.
00131              20  PI-EXPIRE-KEYED-SW      PIC X.
00132                  88  PI-EXPIRE-KEYED         VALUE 'Y'.
00133              20  PI-CRIT-PERD-KEYED-SW    PIC X.
00134                  88  PI-CRIT-PERD-KEYED      VALUE 'Y'.
00135              20  PI-BENEFICIARY-KEYED-SW PIC X.
00136                  88  PI-BENEFICIARY-KEYED    VALUE 'Y'.
00137              20  PI-PHONE-KEYED-SW       PIC X.
00138                  88  PI-PHONE-KEYED          VALUE 'Y'.
00139              20  PI-ALT-BEN-KEYED-SW     PIC X.
00140                  88  PI-ALT-BEN-KEYED        VALUE 'Y'.
00141              20  PI-ALT-PREM-KEYED-SW    PIC X.
00142                  88  PI-ALT-PREM-KEYED       VALUE 'Y'.
00143          16  PI-ACCT-LOW-EFF-DT          PIC XX.
00144          16  PI-ACCT-HIGH-EXP-DT         PIC XX.
00145          16  PI-BATCH-EOF-SW             PIC X.
00146              88  PI-BATCH-EOF                VALUE 'Y'.
00147          16  PI-NB-MONTH-END-DT          PIC XX.
00148          16  PI-ISSUE-ADDED-SW           PIC X.
00149              88  PI-ISSUE-ADDED              VALUE 'Y'.
00150
00151      EJECT
00341      EJECT
00342 *                            COPY ELCJPFX.
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
00343                              PIC X(608).
00344
00345      EJECT
00346 *                            COPY ELCAID.
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
00347  01  FILLER    REDEFINES DFHAID.
00348      12  FILLER              PIC X(8).
00349      12  PF-VALUES           PIC X       OCCURS 2.
00350
00351      EJECT
00352  01  DATA-ENTRY-MAP              PIC X(1116).
00353 *                                      COPY EL9301S.
       01  EL930BI.
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
           05  BBATCHL PIC S9(0004) COMP.
           05  BBATCHF PIC  X(0001).
           05  FILLER REDEFINES BBATCHF.
               10  BBATCHA PIC  X(0001).
           05  BBATCHI PIC  X(0008).
      *    -------------------------------
           05  BSEQL PIC S9(0004) COMP.
           05  BSEQF PIC  X(0001).
           05  FILLER REDEFINES BSEQF.
               10  BSEQA PIC  X(0001).
           05  BSEQI PIC  X(0004).
      *    -------------------------------
           05  BCARHDL PIC S9(0004) COMP.
           05  BCARHDF PIC  X(0001).
           05  FILLER REDEFINES BCARHDF.
               10  BCARHDA PIC  X(0001).
           05  BCARHDI PIC  X(0005).
      *    -------------------------------
           05  BCARL PIC S9(0004) COMP.
           05  BCARF PIC  X(0001).
           05  FILLER REDEFINES BCARF.
               10  BCARA PIC  X(0001).
           05  BCARI PIC  X(0001).
      *    -------------------------------
           05  BGRPHDL PIC S9(0004) COMP.
           05  BGRPHDF PIC  X(0001).
           05  FILLER REDEFINES BGRPHDF.
               10  BGRPHDA PIC  X(0001).
           05  BGRPHDI PIC  X(0005).
      *    -------------------------------
           05  BGRPL PIC S9(0004) COMP.
           05  BGRPF PIC  X(0001).
           05  FILLER REDEFINES BGRPF.
               10  BGRPA PIC  X(0001).
           05  BGRPI PIC  X(0006).
      *    -------------------------------
           05  BSTHDL PIC S9(0004) COMP.
           05  BSTHDF PIC  X(0001).
           05  FILLER REDEFINES BSTHDF.
               10  BSTHDA PIC  X(0001).
           05  BSTHDI PIC  X(0004).
      *    -------------------------------
           05  BSTL PIC S9(0004) COMP.
           05  BSTF PIC  X(0001).
           05  FILLER REDEFINES BSTF.
               10  BSTA PIC  X(0001).
           05  BSTI PIC  X(0002).
      *    -------------------------------
           05  BACCHDL PIC S9(0004) COMP.
           05  BACCHDF PIC  X(0001).
           05  FILLER REDEFINES BACCHDF.
               10  BACCHDA PIC  X(0001).
           05  BACCHDI PIC  X(0006).
      *    -------------------------------
           05  BACCTL PIC S9(0004) COMP.
           05  BACCTF PIC  X(0001).
           05  FILLER REDEFINES BACCTF.
               10  BACCTA PIC  X(0001).
           05  BACCTI PIC  X(0010).
      *    -------------------------------
           05  BCERTL PIC S9(0004) COMP.
           05  BCERTF PIC  X(0001).
           05  FILLER REDEFINES BCERTF.
               10  BCERTA PIC  X(0001).
           05  BCERTI PIC  X(0010).
      *    -------------------------------
           05  BSFXL PIC S9(0004) COMP.
           05  BSFXF PIC  X(0001).
           05  FILLER REDEFINES BSFXF.
               10  BSFXA PIC  X(0001).
           05  BSFXI PIC  X(0001).
      *    -------------------------------
           05  BEFFDTL PIC S9(0004) COMP.
           05  BEFFDTF PIC  X(0001).
           05  FILLER REDEFINES BEFFDTF.
               10  BEFFDTA PIC  X(0001).
           05  BEFFDTI PIC  X(0006).
      *    -------------------------------
           05  BLASTNML PIC S9(0004) COMP.
           05  BLASTNMF PIC  X(0001).
           05  FILLER REDEFINES BLASTNMF.
               10  BLASTNMA PIC  X(0001).
           05  BLASTNMI PIC  X(0015).
      *    -------------------------------
           05  B1STNML PIC S9(0004) COMP.
           05  B1STNMF PIC  X(0001).
           05  FILLER REDEFINES B1STNMF.
               10  B1STNMA PIC  X(0001).
           05  B1STNMI PIC  X(0010).
      *    -------------------------------
           05  BINTL PIC S9(0004) COMP.
           05  BINTF PIC  X(0001).
           05  FILLER REDEFINES BINTF.
               10  BINTA PIC  X(0001).
           05  BINTI PIC  X(0001).
      *    -------------------------------
           05  BSEXL PIC S9(0004) COMP.
           05  BSEXF PIC  X(0001).
           05  FILLER REDEFINES BSEXF.
               10  BSEXA PIC  X(0001).
           05  BSEXI PIC  X(0001).
      *    -------------------------------
           05  BAGEL PIC S9(0004) COMP.
           05  BAGEF PIC  X(0001).
           05  FILLER REDEFINES BAGEF.
               10  BAGEA PIC  X(0001).
           05  BAGEI PIC  X(0002).
      *    -------------------------------
           05  BSSNL PIC S9(0004) COMP.
           05  BSSNF PIC  X(0001).
           05  FILLER REDEFINES BSSNF.
               10  BSSNA PIC  X(0001).
           05  BSSNI PIC  X(0011).
      *    -------------------------------
           05  BKIND1L PIC S9(0004) COMP.
           05  BKIND1F PIC  X(0001).
           05  FILLER REDEFINES BKIND1F.
               10  BKIND1A PIC  X(0001).
           05  BKIND1I PIC  X(0002).
      *    -------------------------------
           05  BTYPE1L PIC S9(0004) COMP.
           05  BTYPE1F PIC  X(0001).
           05  FILLER REDEFINES BTYPE1F.
               10  BTYPE1A PIC  X(0001).
           05  BTYPE1I PIC  X(0003).
      *    -------------------------------
           05  BTRM1L PIC S9(0004) COMP.
           05  BTRM1F PIC  X(0001).
           05  FILLER REDEFINES BTRM1F.
               10  BTRM1A PIC  X(0001).
           05  BTRM1I PIC  X(0003).
      *    -------------------------------
           05  BBEN1L PIC S9(0004) COMP.
           05  BBEN1F PIC  X(0001).
           05  FILLER REDEFINES BBEN1F.
               10  BBEN1A PIC  X(0001).
           05  BBEN1I PIC  X(0012).
      *    -------------------------------
           05  BPRM1L PIC S9(0004) COMP.
           05  BPRM1F PIC  X(0001).
           05  FILLER REDEFINES BPRM1F.
               10  BPRM1A PIC  X(0001).
           05  BPRM1I PIC  X(0011).
      *    -------------------------------
           05  BEXPIR1L PIC S9(0004) COMP.
           05  BEXPIR1F PIC  X(0001).
           05  FILLER REDEFINES BEXPIR1F.
               10  BEXPIR1A PIC  X(0001).
           05  BEXPIR1I PIC  X(0006).
      *    -------------------------------
           05  BCP1L PIC S9(0004) COMP.
           05  BCP1F PIC  X(0001).
           05  FILLER REDEFINES BCP1F.
               10  BCP1A PIC  X(0001).
           05  BCP1I PIC  X(0002).
      *    -------------------------------
           05  BALTBN1L PIC S9(0004) COMP.
           05  BALTBN1F PIC  X(0001).
           05  FILLER REDEFINES BALTBN1F.
               10  BALTBN1A PIC  X(0001).
           05  BALTBN1I PIC  X(0012).
      *    -------------------------------
           05  BALTPM1L PIC S9(0004) COMP.
           05  BALTPM1F PIC  X(0001).
           05  FILLER REDEFINES BALTPM1F.
               10  BALTPM1A PIC  X(0001).
           05  BALTPM1I PIC  X(0009).
      *    -------------------------------
           05  BKIND2L PIC S9(0004) COMP.
           05  BKIND2F PIC  X(0001).
           05  FILLER REDEFINES BKIND2F.
               10  BKIND2A PIC  X(0001).
           05  BKIND2I PIC  X(0002).
      *    -------------------------------
           05  BTYPE2L PIC S9(0004) COMP.
           05  BTYPE2F PIC  X(0001).
           05  FILLER REDEFINES BTYPE2F.
               10  BTYPE2A PIC  X(0001).
           05  BTYPE2I PIC  X(0003).
      *    -------------------------------
           05  BTRM2L PIC S9(0004) COMP.
           05  BTRM2F PIC  X(0001).
           05  FILLER REDEFINES BTRM2F.
               10  BTRM2A PIC  X(0001).
           05  BTRM2I PIC  X(0003).
      *    -------------------------------
           05  BBEN2L PIC S9(0004) COMP.
           05  BBEN2F PIC  X(0001).
           05  FILLER REDEFINES BBEN2F.
               10  BBEN2A PIC  X(0001).
           05  BBEN2I PIC  X(0012).
      *    -------------------------------
           05  BPRM2L PIC S9(0004) COMP.
           05  BPRM2F PIC  X(0001).
           05  FILLER REDEFINES BPRM2F.
               10  BPRM2A PIC  X(0001).
           05  BPRM2I PIC  X(0011).
      *    -------------------------------
           05  BEXPIR2L PIC S9(0004) COMP.
           05  BEXPIR2F PIC  X(0001).
           05  FILLER REDEFINES BEXPIR2F.
               10  BEXPIR2A PIC  X(0001).
           05  BEXPIR2I PIC  X(0006).
      *    -------------------------------
           05  BCP2L PIC S9(0004) COMP.
           05  BCP2F PIC  X(0001).
           05  FILLER REDEFINES BCP2F.
               10  BCP2A PIC  X(0001).
           05  BCP2I PIC  X(0002).
      *    -------------------------------
           05  BALTBN2L PIC S9(0004) COMP.
           05  BALTBN2F PIC  X(0001).
           05  FILLER REDEFINES BALTBN2F.
               10  BALTBN2A PIC  X(0001).
           05  BALTBN2I PIC  X(0012).
      *    -------------------------------
           05  BALTPM2L PIC S9(0004) COMP.
           05  BALTPM2F PIC  X(0001).
           05  FILLER REDEFINES BALTPM2F.
               10  BALTPM2A PIC  X(0001).
           05  BALTPM2I PIC  X(0009).
      *    -------------------------------
           05  BLIVSL PIC S9(0004) COMP.
           05  BLIVSF PIC  X(0001).
           05  FILLER REDEFINES BLIVSF.
               10  BLIVSA PIC  X(0001).
           05  BLIVSI PIC  X(0003).
      *    -------------------------------
           05  BJNT1STL PIC S9(0004) COMP.
           05  BJNT1STF PIC  X(0001).
           05  FILLER REDEFINES BJNT1STF.
               10  BJNT1STA PIC  X(0001).
           05  BJNT1STI PIC  X(0010).
      *    -------------------------------
           05  BJNTINTL PIC S9(0004) COMP.
           05  BJNTINTF PIC  X(0001).
           05  FILLER REDEFINES BJNTINTF.
               10  BJNTINTA PIC  X(0001).
           05  BJNTINTI PIC  X(0001).
      *    -------------------------------
           05  BJNTNAML PIC S9(0004) COMP.
           05  BJNTNAMF PIC  X(0001).
           05  FILLER REDEFINES BJNTNAMF.
               10  BJNTNAMA PIC  X(0001).
           05  BJNTNAMI PIC  X(0015).
      *    -------------------------------
           05  BJNTAGEL PIC S9(0004) COMP.
           05  BJNTAGEF PIC  X(0001).
           05  FILLER REDEFINES BJNTAGEF.
               10  BJNTAGEA PIC  X(0001).
           05  BJNTAGEI PIC  X(0002).
      *    -------------------------------
           05  BNFICRYL PIC S9(0004) COMP.
           05  BNFICRYF PIC  X(0001).
           05  FILLER REDEFINES BNFICRYF.
               10  BNFICRYA PIC  X(0001).
           05  BNFICRYI PIC  X(0025).
      *    -------------------------------
           05  B1STPMTL PIC S9(0004) COMP.
           05  B1STPMTF PIC  X(0001).
           05  FILLER REDEFINES B1STPMTF.
               10  B1STPMTA PIC  X(0001).
           05  B1STPMTI PIC  X(0006).
      *    -------------------------------
           05  BDAYL PIC S9(0004) COMP.
           05  BDAYF PIC  X(0001).
           05  FILLER REDEFINES BDAYF.
               10  BDAYA PIC  X(0001).
           05  BDAYI PIC  X(0003).
      *    -------------------------------
           05  BLNTRML PIC S9(0004) COMP.
           05  BLNTRMF PIC  X(0001).
           05  FILLER REDEFINES BLNTRMF.
               10  BLNTRMA PIC  X(0001).
           05  BLNTRMI PIC  X(0003).
      *    -------------------------------
           05  BLONOFCL PIC S9(0004) COMP.
           05  BLONOFCF PIC  X(0001).
           05  FILLER REDEFINES BLONOFCF.
               10  BLONOFCA PIC  X(0001).
           05  BLONOFCI PIC  X(0003).
      *    -------------------------------
           05  BMODEL PIC S9(0004) COMP.
           05  BMODEF PIC  X(0001).
           05  FILLER REDEFINES BMODEF.
               10  BMODEA PIC  X(0001).
           05  BMODEI PIC  X(0001).
      *    -------------------------------
           05  BFRQL PIC S9(0004) COMP.
           05  BFRQF PIC  X(0001).
           05  FILLER REDEFINES BFRQF.
               10  BFRQA PIC  X(0001).
           05  BFRQI PIC  X(0002).
      *    -------------------------------
           05  BPMTSL PIC S9(0004) COMP.
           05  BPMTSF PIC  X(0001).
           05  FILLER REDEFINES BPMTSF.
               10  BPMTSA PIC  X(0001).
           05  BPMTSI PIC  X(0003).
      *    -------------------------------
           05  BPMTAMTL PIC S9(0004) COMP.
           05  BPMTAMTF PIC  X(0001).
           05  FILLER REDEFINES BPMTAMTF.
               10  BPMTAMTA PIC  X(0001).
           05  BPMTAMTI PIC  X(0009).
      *    -------------------------------
           05  BCAPTNL PIC S9(0004) COMP.
           05  BCAPTNF PIC  X(0001).
           05  FILLER REDEFINES BCAPTNF.
               10  BCAPTNA PIC  X(0001).
           05  BCAPTNI PIC  X(0010).
      *    -------------------------------
           05  BRCL PIC S9(0004) COMP.
           05  BRCF PIC  X(0001).
           05  FILLER REDEFINES BRCF.
               10  BRCA PIC  X(0001).
           05  BRCI PIC  X(0001).
      *    -------------------------------
           05  BENTRYL PIC S9(0004) COMP.
           05  BENTRYF PIC  X(0001).
           05  FILLER REDEFINES BENTRYF.
               10  BENTRYA PIC  X(0001).
           05  BENTRYI PIC  X(0001).
      *    -------------------------------
           05  BSKIPL PIC S9(0004) COMP.
           05  BSKIPF PIC  X(0001).
           05  FILLER REDEFINES BSKIPF.
               10  BSKIPA PIC  X(0001).
           05  BSKIPI PIC  X(0001).
      *    -------------------------------
           05  BINDGRPL PIC S9(0004) COMP.
           05  BINDGRPF PIC  X(0001).
           05  FILLER REDEFINES BINDGRPF.
               10  BINDGRPA PIC  X(0001).
           05  BINDGRPI PIC  X(0001).
      *    -------------------------------
           05  BSIGL PIC S9(0004) COMP.
           05  BSIGF PIC  X(0001).
           05  FILLER REDEFINES BSIGF.
               10  BSIGA PIC  X(0001).
           05  BSIGI PIC  X(0001).
      *    -------------------------------
           05  BPOLICYL PIC S9(0004) COMP.
           05  BPOLICYF PIC  X(0001).
           05  FILLER REDEFINES BPOLICYF.
               10  BPOLICYA PIC  X(0001).
           05  BPOLICYI PIC  X(0012).
      *    -------------------------------
           05  BRTCLSL PIC S9(0004) COMP.
           05  BRTCLSF PIC  X(0001).
           05  FILLER REDEFINES BRTCLSF.
               10  BRTCLSA PIC  X(0001).
           05  BRTCLSI PIC  X(0002).
      *    -------------------------------
           05  BAPRL PIC S9(0004) COMP.
           05  BAPRF PIC  X(0001).
           05  FILLER REDEFINES BAPRF.
               10  BAPRA PIC  X(0001).
           05  BAPRI PIC  9(3)V9999.
      *    -------------------------------
           05  BBIRTHL PIC S9(0004) COMP.
           05  BBIRTHF PIC  X(0001).
           05  FILLER REDEFINES BBIRTHF.
               10  BBIRTHA PIC  X(0001).
           05  BBIRTHI PIC  X(0006).
      *    -------------------------------
           05  BMEML PIC S9(0004) COMP.
           05  BMEMF PIC  X(0001).
           05  FILLER REDEFINES BMEMF.
               10  BMEMA PIC  X(0001).
           05  BMEMI PIC  X(0012).
      *    -------------------------------
           05  BADDRS1L PIC S9(0004) COMP.
           05  BADDRS1F PIC  X(0001).
           05  FILLER REDEFINES BADDRS1F.
               10  BADDRS1A PIC  X(0001).
           05  BADDRS1I PIC  X(0030).
      *    -------------------------------
           05  BADDRS2L PIC S9(0004) COMP.
           05  BADDRS2F PIC  X(0001).
           05  FILLER REDEFINES BADDRS2F.
               10  BADDRS2A PIC  X(0001).
           05  BADDRS2I PIC  X(0030).
      *    -------------------------------
           05  BCITYSTL PIC S9(0004) COMP.
           05  BCITYSTF PIC  X(0001).
           05  FILLER REDEFINES BCITYSTF.
               10  BCITYSTA PIC  X(0001).
           05  BCITYSTI PIC  X(0030).
      *    -------------------------------
           05  BZIP5L PIC S9(0004) COMP.
           05  BZIP5F PIC  X(0001).
           05  FILLER REDEFINES BZIP5F.
               10  BZIP5A PIC  X(0001).
           05  BZIP5I PIC  X(0005).
      *    -------------------------------
           05  BDASHL PIC S9(0004) COMP.
           05  BDASHF PIC  X(0001).
           05  FILLER REDEFINES BDASHF.
               10  BDASHA PIC  X(0001).
           05  BDASHI PIC  X(0001).
      *    -------------------------------
           05  BZIP4L PIC S9(0004) COMP.
           05  BZIP4F PIC  X(0001).
           05  FILLER REDEFINES BZIP4F.
               10  BZIP4A PIC  X(0001).
           05  BZIP4I PIC  X(0004).
      *    -------------------------------
           05  BPHONEL PIC S9(0004) COMP.
           05  BPHONEF PIC  X(0001).
           05  FILLER REDEFINES BPHONEF.
               10  BPHONEA PIC  X(0001).
           05  BPHONEI PIC  X(0012).
      *    -------------------------------
           05  BERMSG1L PIC S9(0004) COMP.
           05  BERMSG1F PIC  X(0001).
           05  FILLER REDEFINES BERMSG1F.
               10  BERMSG1A PIC  X(0001).
           05  BERMSG1I PIC  X(0079).
      *    -------------------------------
           05  BERMSG2L PIC S9(0004) COMP.
           05  BERMSG2F PIC  X(0001).
           05  FILLER REDEFINES BERMSG2F.
               10  BERMSG2A PIC  X(0001).
           05  BERMSG2I PIC  X(0079).
      *    -------------------------------
           05  BPFENTRL PIC S9(0004) COMP.
           05  BPFENTRF PIC  X(0001).
           05  FILLER REDEFINES BPFENTRF.
               10  BPFENTRA PIC  X(0001).
           05  BPFENTRI PIC  9(2).
      *    -------------------------------
           05  BDELHDGL PIC S9(0004) COMP.
           05  BDELHDGF PIC  X(0001).
           05  FILLER REDEFINES BDELHDGF.
               10  BDELHDGA PIC  X(0001).
           05  BDELHDGI PIC  X(0017).
       01  EL930BO REDEFINES EL930BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BBATCHO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARHDO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRPHDO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTHDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCHDO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEFFDTO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BLASTNMO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  B1STNMO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSSNO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BKIND1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTRM1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BBEN1O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPRM1O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEXPIR1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCP1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BALTBN1O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BALTPM1O PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BKIND2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTRM2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BBEN2O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPRM2O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEXPIR2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCP2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BALTBN2O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BALTPM2O PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BLIVSO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BJNT1STO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BJNTINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BJNTNAMO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BJNTAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNFICRYO PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  B1STPMTO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDAYO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BLNTRMO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BLONOFCO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BMODEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BFRQO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPMTSO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPMTAMTO PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCAPTNO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRCO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENTRYO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSKIPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BINDGRPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSIGO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPOLICYO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRTCLSO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAPRO PIC  99.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BBIRTHO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BMEMO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BADDRS1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BADDRS2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCITYSTO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BZIP5O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDASHO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BZIP4O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPHONEO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BERMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BERMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFENTRO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDELHDGO PIC  X(0017).
      *    -------------------------------
       01  EL930CI REDEFINES EL930BI.
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
           05  CBATCHL PIC S9(0004) COMP.
           05  CBATCHF PIC  X(0001).
           05  FILLER REDEFINES CBATCHF.
               10  CBATCHA PIC  X(0001).
           05  CBATCHI PIC  X(0008).
      *    -------------------------------
           05  CSEQ1L PIC S9(0004) COMP.
           05  CSEQ1F PIC  X(0001).
           05  FILLER REDEFINES CSEQ1F.
               10  CSEQ1A PIC  X(0001).
           05  CSEQ1I PIC  X(0004).
      *    -------------------------------
           05  CCARHD1L PIC S9(0004) COMP.
           05  CCARHD1F PIC  X(0001).
           05  FILLER REDEFINES CCARHD1F.
               10  CCARHD1A PIC  X(0001).
           05  CCARHD1I PIC  X(0005).
      *    -------------------------------
           05  CCAR1L PIC S9(0004) COMP.
           05  CCAR1F PIC  X(0001).
           05  FILLER REDEFINES CCAR1F.
               10  CCAR1A PIC  X(0001).
           05  CCAR1I PIC  X(0001).
      *    -------------------------------
           05  CGRPHD1L PIC S9(0004) COMP.
           05  CGRPHD1F PIC  X(0001).
           05  FILLER REDEFINES CGRPHD1F.
               10  CGRPHD1A PIC  X(0001).
           05  CGRPHD1I PIC  X(0005).
      *    -------------------------------
           05  CGRP1L PIC S9(0004) COMP.
           05  CGRP1F PIC  X(0001).
           05  FILLER REDEFINES CGRP1F.
               10  CGRP1A PIC  X(0001).
           05  CGRP1I PIC  X(0006).
      *    -------------------------------
           05  CSTHD1L PIC S9(0004) COMP.
           05  CSTHD1F PIC  X(0001).
           05  FILLER REDEFINES CSTHD1F.
               10  CSTHD1A PIC  X(0001).
           05  CSTHD1I PIC  X(0004).
      *    -------------------------------
           05  CST1L PIC S9(0004) COMP.
           05  CST1F PIC  X(0001).
           05  FILLER REDEFINES CST1F.
               10  CST1A PIC  X(0001).
           05  CST1I PIC  X(0002).
      *    -------------------------------
           05  CACCHD1L PIC S9(0004) COMP.
           05  CACCHD1F PIC  X(0001).
           05  FILLER REDEFINES CACCHD1F.
               10  CACCHD1A PIC  X(0001).
           05  CACCHD1I PIC  X(0006).
      *    -------------------------------
           05  CACCT1L PIC S9(0004) COMP.
           05  CACCT1F PIC  X(0001).
           05  FILLER REDEFINES CACCT1F.
               10  CACCT1A PIC  X(0001).
           05  CACCT1I PIC  X(0010).
      *    -------------------------------
           05  CCERT1L PIC S9(0004) COMP.
           05  CCERT1F PIC  X(0001).
           05  FILLER REDEFINES CCERT1F.
               10  CCERT1A PIC  X(0001).
           05  CCERT1I PIC  X(0010).
      *    -------------------------------
           05  CSFX1L PIC S9(0004) COMP.
           05  CSFX1F PIC  X(0001).
           05  FILLER REDEFINES CSFX1F.
               10  CSFX1A PIC  X(0001).
           05  CSFX1I PIC  X(0001).
      *    -------------------------------
           05  CEFFDT1L PIC S9(0004) COMP.
           05  CEFFDT1F PIC  X(0001).
           05  FILLER REDEFINES CEFFDT1F.
               10  CEFFDT1A PIC  X(0001).
           05  CEFFDT1I PIC  9(6).
      *    -------------------------------
           05  CLSTNM1L PIC S9(0004) COMP.
           05  CLSTNM1F PIC  X(0001).
           05  FILLER REDEFINES CLSTNM1F.
               10  CLSTNM1A PIC  X(0001).
           05  CLSTNM1I PIC  X(0015).
      *    -------------------------------
           05  CKIND1L PIC S9(0004) COMP.
           05  CKIND1F PIC  X(0001).
           05  FILLER REDEFINES CKIND1F.
               10  CKIND1A PIC  X(0001).
           05  CKIND1I PIC  X(0002).
      *    -------------------------------
           05  CCANDT1L PIC S9(0004) COMP.
           05  CCANDT1F PIC  X(0001).
           05  FILLER REDEFINES CCANDT1F.
               10  CCANDT1A PIC  X(0001).
           05  CCANDT1I PIC  9(6).
      *    -------------------------------
           05  CRFUND1L PIC S9(0004) COMP.
           05  CRFUND1F PIC  X(0001).
           05  FILLER REDEFINES CRFUND1F.
               10  CRFUND1A PIC  X(0001).
           05  CRFUND1I PIC  S9(9)V9(2).
      *    -------------------------------
           05  CKIND2L PIC S9(0004) COMP.
           05  CKIND2F PIC  X(0001).
           05  FILLER REDEFINES CKIND2F.
               10  CKIND2A PIC  X(0001).
           05  CKIND2I PIC  X(0002).
      *    -------------------------------
           05  CCANDT2L PIC S9(0004) COMP.
           05  CCANDT2F PIC  X(0001).
           05  FILLER REDEFINES CCANDT2F.
               10  CCANDT2A PIC  X(0001).
           05  CCANDT2I PIC  9(6).
      *    -------------------------------
           05  CRFUND2L PIC S9(0004) COMP.
           05  CRFUND2F PIC  X(0001).
           05  FILLER REDEFINES CRFUND2F.
               10  CRFUND2A PIC  X(0001).
           05  CRFUND2I PIC  S9(9)V9(2).
      *    -------------------------------
           05  CCHK1L PIC S9(0004) COMP.
           05  CCHK1F PIC  X(0001).
           05  FILLER REDEFINES CCHK1F.
               10  CCHK1A PIC  X(0001).
           05  CCHK1I PIC  X(0001).
      *    -------------------------------
           05  CPAYEE1L PIC S9(0004) COMP.
           05  CPAYEE1F PIC  X(0001).
           05  FILLER REDEFINES CPAYEE1F.
               10  CPAYEE1A PIC  X(0001).
           05  CPAYEE1I PIC  X(0006).
      *    -------------------------------
           05  CLIVES1L PIC S9(0004) COMP.
           05  CLIVES1F PIC  X(0001).
           05  FILLER REDEFINES CLIVES1F.
               10  CLIVES1A PIC  X(0001).
           05  CLIVES1I PIC  9(3).
      *    -------------------------------
           05  CSEQ2L PIC S9(0004) COMP.
           05  CSEQ2F PIC  X(0001).
           05  FILLER REDEFINES CSEQ2F.
               10  CSEQ2A PIC  X(0001).
           05  CSEQ2I PIC  X(0004).
      *    -------------------------------
           05  CCARHD2L PIC S9(0004) COMP.
           05  CCARHD2F PIC  X(0001).
           05  FILLER REDEFINES CCARHD2F.
               10  CCARHD2A PIC  X(0001).
           05  CCARHD2I PIC  X(0005).
      *    -------------------------------
           05  CCAR2L PIC S9(0004) COMP.
           05  CCAR2F PIC  X(0001).
           05  FILLER REDEFINES CCAR2F.
               10  CCAR2A PIC  X(0001).
           05  CCAR2I PIC  X(0001).
      *    -------------------------------
           05  CGRPHD2L PIC S9(0004) COMP.
           05  CGRPHD2F PIC  X(0001).
           05  FILLER REDEFINES CGRPHD2F.
               10  CGRPHD2A PIC  X(0001).
           05  CGRPHD2I PIC  X(0005).
      *    -------------------------------
           05  CGRP2L PIC S9(0004) COMP.
           05  CGRP2F PIC  X(0001).
           05  FILLER REDEFINES CGRP2F.
               10  CGRP2A PIC  X(0001).
           05  CGRP2I PIC  X(0006).
      *    -------------------------------
           05  CSTHD2L PIC S9(0004) COMP.
           05  CSTHD2F PIC  X(0001).
           05  FILLER REDEFINES CSTHD2F.
               10  CSTHD2A PIC  X(0001).
           05  CSTHD2I PIC  X(0004).
      *    -------------------------------
           05  CST2L PIC S9(0004) COMP.
           05  CST2F PIC  X(0001).
           05  FILLER REDEFINES CST2F.
               10  CST2A PIC  X(0001).
           05  CST2I PIC  X(0002).
      *    -------------------------------
           05  CACCHD2L PIC S9(0004) COMP.
           05  CACCHD2F PIC  X(0001).
           05  FILLER REDEFINES CACCHD2F.
               10  CACCHD2A PIC  X(0001).
           05  CACCHD2I PIC  X(0006).
      *    -------------------------------
           05  CACCT2L PIC S9(0004) COMP.
           05  CACCT2F PIC  X(0001).
           05  FILLER REDEFINES CACCT2F.
               10  CACCT2A PIC  X(0001).
           05  CACCT2I PIC  X(0010).
      *    -------------------------------
           05  CCERT2L PIC S9(0004) COMP.
           05  CCERT2F PIC  X(0001).
           05  FILLER REDEFINES CCERT2F.
               10  CCERT2A PIC  X(0001).
           05  CCERT2I PIC  X(0010).
      *    -------------------------------
           05  CSFX2L PIC S9(0004) COMP.
           05  CSFX2F PIC  X(0001).
           05  FILLER REDEFINES CSFX2F.
               10  CSFX2A PIC  X(0001).
           05  CSFX2I PIC  X(0001).
      *    -------------------------------
           05  CEFFDT2L PIC S9(0004) COMP.
           05  CEFFDT2F PIC  X(0001).
           05  FILLER REDEFINES CEFFDT2F.
               10  CEFFDT2A PIC  X(0001).
           05  CEFFDT2I PIC  9(6).
      *    -------------------------------
           05  CLSTNM2L PIC S9(0004) COMP.
           05  CLSTNM2F PIC  X(0001).
           05  FILLER REDEFINES CLSTNM2F.
               10  CLSTNM2A PIC  X(0001).
           05  CLSTNM2I PIC  X(0015).
      *    -------------------------------
           05  CKIND3L PIC S9(0004) COMP.
           05  CKIND3F PIC  X(0001).
           05  FILLER REDEFINES CKIND3F.
               10  CKIND3A PIC  X(0001).
           05  CKIND3I PIC  X(0002).
      *    -------------------------------
           05  CCANDT3L PIC S9(0004) COMP.
           05  CCANDT3F PIC  X(0001).
           05  FILLER REDEFINES CCANDT3F.
               10  CCANDT3A PIC  X(0001).
           05  CCANDT3I PIC  9(6).
      *    -------------------------------
           05  CRFUND3L PIC S9(0004) COMP.
           05  CRFUND3F PIC  X(0001).
           05  FILLER REDEFINES CRFUND3F.
               10  CRFUND3A PIC  X(0001).
           05  CRFUND3I PIC  S9(9)V9(2).
      *    -------------------------------
           05  CKIND4L PIC S9(0004) COMP.
           05  CKIND4F PIC  X(0001).
           05  FILLER REDEFINES CKIND4F.
               10  CKIND4A PIC  X(0001).
           05  CKIND4I PIC  X(0002).
      *    -------------------------------
           05  CCANDT4L PIC S9(0004) COMP.
           05  CCANDT4F PIC  X(0001).
           05  FILLER REDEFINES CCANDT4F.
               10  CCANDT4A PIC  X(0001).
           05  CCANDT4I PIC  9(6).
      *    -------------------------------
           05  CRFUND4L PIC S9(0004) COMP.
           05  CRFUND4F PIC  X(0001).
           05  FILLER REDEFINES CRFUND4F.
               10  CRFUND4A PIC  X(0001).
           05  CRFUND4I PIC  S9(9)V9(2).
      *    -------------------------------
           05  CCHK2L PIC S9(0004) COMP.
           05  CCHK2F PIC  X(0001).
           05  FILLER REDEFINES CCHK2F.
               10  CCHK2A PIC  X(0001).
           05  CCHK2I PIC  X(0001).
      *    -------------------------------
           05  CPAYEE2L PIC S9(0004) COMP.
           05  CPAYEE2F PIC  X(0001).
           05  FILLER REDEFINES CPAYEE2F.
               10  CPAYEE2A PIC  X(0001).
           05  CPAYEE2I PIC  X(0006).
      *    -------------------------------
           05  CLIVES2L PIC S9(0004) COMP.
           05  CLIVES2F PIC  X(0001).
           05  FILLER REDEFINES CLIVES2F.
               10  CLIVES2A PIC  X(0001).
           05  CLIVES2I PIC  9(3).
      *    -------------------------------
           05  CERMSG1L PIC S9(0004) COMP.
           05  CERMSG1F PIC  X(0001).
           05  FILLER REDEFINES CERMSG1F.
               10  CERMSG1A PIC  X(0001).
           05  CERMSG1I PIC  X(0079).
      *    -------------------------------
           05  CERMSG2L PIC S9(0004) COMP.
           05  CERMSG2F PIC  X(0001).
           05  FILLER REDEFINES CERMSG2F.
               10  CERMSG2A PIC  X(0001).
           05  CERMSG2I PIC  X(0079).
      *    -------------------------------
           05  CPFENTRL PIC S9(0004) COMP.
           05  CPFENTRF PIC  X(0001).
           05  FILLER REDEFINES CPFENTRF.
               10  CPFENTRA PIC  X(0001).
           05  CPFENTRI PIC  9(2).
      *    -------------------------------
           05  CDELHDGL PIC S9(0004) COMP.
           05  CDELHDGF PIC  X(0001).
           05  FILLER REDEFINES CDELHDGF.
               10  CDELHDGA PIC  X(0001).
           05  CDELHDGI PIC  X(0017).
       01  EL930CO REDEFINES EL930BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBATCHO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSEQ1O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCARHD1O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAR1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CGRPHD1O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CGRP1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTHD1O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CST1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACCHD1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACCT1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCERT1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSFX1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CEFFDT1O PIC  999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLSTNM1O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CKIND1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCANDT1O PIC  999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRFUND1O PIC  9999999.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CKIND2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCANDT2O PIC  999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRFUND2O PIC  9999999.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCHK1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPAYEE1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLIVES1O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSEQ2O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCARHD2O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAR2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CGRPHD2O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CGRP2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTHD2O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CST2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACCHD2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACCT2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCERT2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSFX2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CEFFDT2O PIC  999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLSTNM2O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CKIND3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCANDT3O PIC  999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRFUND3O PIC  9999999.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CKIND4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCANDT4O PIC  999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRFUND4O PIC  9999999.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCHK2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPAYEE2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLIVES2O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPFENTRO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CDELHDGO PIC  X(0017).
      *    -------------------------------
00354
00355      EJECT
00356
00357  01  MAP-B REDEFINES EL930BI.
00358      12  FILLER                      PIC X(42).
00359      12  DATA-AREA-B.
00360          16  BSEQ-LEN                PIC S9(4)  COMP.
00361          16  BSEQ-ATTRB              PIC X.
00362          16  BSEQ                    PIC 9(4).
00363          16  FILLER                  PIC X(8).
00364          16  BCAR-LEN                PIC S9(4)  COMP.
00365          16  BCAR-ATTRB              PIC X.
00366          16  BCAR                    PIC X.
00367          16  FILLER                  PIC X(8).
00368          16  BGRP-LEN                PIC S9(4)  COMP.
00369          16  BGRP-ATTRB              PIC X.
00370          16  BGRP                    PIC X(6).
00371          16  FILLER                  PIC X(7).
00372          16  BST-LEN                 PIC S9(4)  COMP.
00373          16  BST-ATTRB               PIC X.
00374          16  BST                     PIC XX.
00375          16  FILLER                  PIC X(9).
00376          16  BACCT-LEN               PIC S9(4)  COMP.
00377          16  BACCT-ATTRB             PIC X.
00378          16  BACCT                   PIC X(10).
00379          16  BCERT-LEN               PIC S9(4)  COMP.
00380          16  BCERT-ATTRB             PIC X.
00381          16  BCERT                   PIC X(10).
00382          16  BSFX-LEN                PIC S9(4)  COMP.
00383          16  BSFX-ATTRB              PIC X.
00384          16  BSFX                    PIC X.
00385          16  BEFFDT-LEN              PIC S9(4)  COMP.
00386          16  BEFFDT-ATTRB            PIC X.
00387          16  BEFFDT                  PIC 9(6).
00388          16  BLAST-NAME-LEN          PIC S9(4)  COMP.
00389          16  BLAST-NAME-ATTRB        PIC X.
00390          16  BLAST-NAME              PIC X(15).
00391          16  B1ST-NAME-LEN           PIC S9(4)  COMP.
00392          16  B1ST-NAME-ATTRB         PIC X.
00393          16  B1ST-NAME               PIC X(10).
00394          16  BINIT-LEN               PIC S9(4)  COMP.
00395          16  BINIT-ATTRB             PIC X.
00396          16  BINIT                   PIC X.
00397          16  BSEX-LEN                PIC S9(4)  COMP.
00398          16  BSEX-ATTRB              PIC X.
00399          16  BSEX                    PIC X.
00400          16  BAGE-LEN                PIC S9(4)  COMP.
00401          16  BAGE-ATTRB              PIC X.
00402          16  BAGE                    PIC 99.
00403          16  BSSNUM-LEN              PIC S9(4)  COMP.
00404          16  BSSNUM-ATTRB            PIC X.
00405          16  BSSNUM                  PIC X(11).
00406
00407          16  MAP-B-COVERAGE OCCURS 2 TIMES.
00408
00409              20  BKIND-LEN           PIC S9(4)  COMP.
00410              20  BKIND-ATTRB         PIC X.
00411              20  BKIND               PIC XX.
00412              20  BTYPE-LEN           PIC S9(4)  COMP.
00413              20  BTYPE-ATTRB         PIC X.
00414              20  BTYPE               PIC X(3).
00415              20  BTERM-LEN           PIC S9(4)  COMP.
00416              20  BTERM-ATTRB         PIC X.
00417              20  BTERMI              PIC 999.
00418              20  BTERMO REDEFINES
00419                             BTERMI   PIC ZZZ.
00420              20  BBEN-LEN            PIC S9(4)  COMP.
00421              20  BBEN-ATTRB          PIC X.
00422              20  BBENI               PIC 9(12).
00423              20  BBENO REDEFINES
00424                                BBENI PIC Z(9).99.
00425              20  BPREM-LEN           PIC S9(4)  COMP.
00426              20  BPREM-ATTRB         PIC X.
00427              20  BPREMI              PIC 9(11).
00428              20  BPREMO REDEFINES
00429                               BPREMI PIC Z(7).99-.
00430              20  BEXPIRE-LEN         PIC S9(4)  COMP.
00431              20  BEXPIRE-ATTRB       PIC X.
00432              20  BEXPIRE             PIC 9(6).
00433              20  BCRIT-PERD-LEN      PIC S9(4)  COMP.
00434              20  BCRIT-PERD-ATTRB    PIC X.
00435              20  BCRIT-PERDI         PIC 99.
00436              20  BCRIT-PERDO REDEFINES
00437                          BCRIT-PERDI PIC ZZ.
00438              20  BALT-BEN-LEN        PIC S9(4)  COMP.
00439              20  BALT-BEN-ATTRB      PIC X.
00440              20  BALT-BENI           PIC 9(12).
00441              20  BALT-BENO REDEFINES
00442                                BALT-BENI PIC Z(9).ZZ.
00443              20  BALT-PREM-LEN       PIC S9(4)  COMP.
00444              20  BALT-PREM-ATTRB     PIC X.
00445              20  BALT-PREMI          PIC 9(9).
00446              20  BALT-PREMO REDEFINES
00447                                BALT-PREMI PIC Z(6).ZZ.
00448
00449          16  BLIVES-LEN              PIC S9(4)  COMP.
00450          16  BLIVES-ATTRB            PIC X.
00451          16  BLIVESI                 PIC 9(3).
00452          16  BLIVESO    REDEFINES
00453                           BLIVESI    PIC Z(3).
00454          16  BJNT-1ST-NAME-LEN       PIC S9(4)   COMP.
00455          16  BJNT-1ST-NAME-ATTRB     PIC X.
00456          16  BJNT-1ST-NAME           PIC X(10).
00457          16  BJNT-INIT-LEN           PIC S9(4)   COMP.
00458          16  BJNT-INIT-ATTRB         PIC X.
00459          16  BJNT-INIT               PIC X.
00460          16  BJNT-LST-NAME-LEN       PIC S9(4)   COMP.
00461          16  BJNT-LST-NAME-ATTRB     PIC X.
00462          16  BJNT-LST-NAME           PIC X(15).
00463          16  BJNT-AGE-LEN            PIC S9(4)   COMP.
00464          16  BJNT-AGE-ATTRB          PIC X.
00465          16  BJNT-AGE                PIC 99.
00466          16  BBENEFICIARY-LEN        PIC S9(4)   COMP.
00467          16  BBENEFICIARY-ATTRB      PIC X.
00468          16  BBENEFICIARY            PIC X(25).
00469          16  B1ST-PMT-LEN            PIC S9(4)  COMP.
00470          16  B1ST-PMT-ATTRB          PIC X.
00471          16  B1ST-PMT                PIC 9(6).
00472          16  BDAYS-LEN               PIC S9(4)  COMP.
00473          16  BDAYS-ATTRB             PIC X.
00474          16  BDAYSI                  PIC 9(3).
00475          16  BDAYSO REDEFINES
00476                           BDAYSI     PIC ZZZ.
00477          16  BLN-TERM-LEN            PIC S9(4)  COMP.
00478          16  BLN-TERM-ATTRB          PIC X.
00479          16  BLN-TERMI               PIC 9(3).
00480          16  BLN-TERMO REDEFINES
00481                           BLN-TERMI  PIC ZZZ.
00482          16  BLN-OFFICER-LEN         PIC S9(4)  COMP.
00483          16  BLN-OFFICER-ATTRB       PIC X.
00484          16  BLN-OFFICER             PIC XXX.
00485          16  BMODE-LEN               PIC S9(4)  COMP.
00486          16  BMODE-ATTRB             PIC X.
00487          16  BMODE                   PIC X.
00488          16  BFREQ-LEN               PIC S9(4)  COMP.
00489          16  BFREQ-ATTRB             PIC X.
00490          16  BFREQI                  PIC 99.
00491          16  BFREQO REDEFINES BFREQI PIC ZZ.
00492          16  BPMTS-LEN               PIC S9(4)  COMP.
00493          16  BPMTS-ATTRB             PIC X.
00494          16  BPMTS-IN                PIC 999.
00495          16  BPMTS-OUT REDEFINES BPMTS-IN
00496                                      PIC ZZZ.
00497          16  BPMT-LEN                PIC S9(4)  COMP.
00498          16  BPMT-ATTRB              PIC X.
00499          16  BPMTI                   PIC 9(9).
00500          16  BPMTO REDEFINES BPMTI   PIC Z(6).ZZ.
00501          16  BCAPTN-LEN              PIC S9(4)  COMP.
00502          16  BCAPTN-ATTRB            PIC X.
00503          16  BCAPTN                  PIC X(10).
00504          16  BRINCD-LEN              PIC S9(4)  COMP.
00505          16  BRINCD-ATTRB            PIC X.
00506          16  BRINCD                  PIC X.
00507          16  BENTRY-LEN              PIC S9(4)  COMP.
00508          16  BENTRY-ATTRB            PIC X.
00509          16  BENTRY                  PIC X.
00510          16  BSKPCD-LEN              PIC S9(4)  COMP.
00511          16  BSKPCD-ATTRB            PIC X.
00512          16  BSKPCD                  PIC X.
00513          16  BIND-GRP-LEN            PIC S9(4)  COMP.
00514          16  BIND-GRP-ATTRB          PIC X.
00515          16  BIND-GRP                PIC X.
00516          16  BSIG-LEN                PIC S9(4)  COMP.
00517          16  BSIG-ATTRB              PIC X.
00518          16  BSIG                    PIC X.
00519          16  BPOLICY-LEN             PIC S9(4)  COMP.
00520          16  BPOLICY-ATTRB           PIC X.
00521          16  BPOLICY                 PIC X(12).
00522          16  BRTCLS-LEN              PIC S9(4)  COMP.
00523          16  BRTCLS-ATTRB            PIC X.
00524          16  BRTCLS                  PIC XX.
00525          16  BAPR-LEN                PIC S9(4)  COMP.
00526          16  BAPR-ATTRB              PIC X.
00527          16  BAPR-IN                 PIC 9(7).
00528          16  BAPR-OUT REDEFINES BAPR-IN
00529                                      PIC 99.9999.
00530          16  BBIRTH-LEN              PIC S9(4)  COMP.
00531          16  BBIRTH-ATTRB            PIC X.
00532          16  BBIRTH-DT               PIC 9(6).
00533          16  BMEM-NO-LEN             PIC S9(4)   COMP.
00534          16  BMEM-NO-ATTRB           PIC X.
00535          16  BMEM-NO                 PIC X(12).
00536          16  BADDRS1-LEN             PIC S9(4)  COMP.
00537          16  BADDRS1-ATTRB           PIC X.
00538          16  BADDRS1                 PIC X(30).
00539          16  BADDRS2-LEN             PIC S9(4)  COMP.
00540          16  BADDRS2-ATTRB           PIC X.
00541          16  BADDRS2                 PIC X(30).
00542          16  BCITYST-LEN             PIC S9(4)  COMP.
00543          16  BCITYST-ATTRB           PIC X.
00544          16  BCITYST                 PIC X(30).
00545          16  BZIP5-LEN               PIC S9(4)  COMP.
00546          16  BZIP5-ATTRB             PIC X.
00547          16  BZIP5                   PIC X(5).
00548          16  BDASH-LEN               PIC S9(4)  COMP.
00549          16  BDASH-ATTRB             PIC X.
00550          16  BDASH                   PIC X.
00551          16  BZIP4-LEN               PIC S9(4)  COMP.
00552          16  BZIP4-ATTRB             PIC X.
00553          16  BZIP4                   PIC X(4).
00554          16  BPHONE-LEN              PIC S9(4)  COMP.
00555          16  BPHONE-ATTRB            PIC X.
00556          16  BPHONE                  PIC 9(12).
00557          16  BPHONE-NO REDEFINES
00558                                BPHONE PIC 999B999B9999.
00559
00560
00561  01  MAP-C REDEFINES EL930BI.
00562      12  FILLER                  PIC X(42).
00563      12  DATA-AREA-C             OCCURS 2 TIMES.
00564          16  CSEQ-LEN                PIC S9(4)  COMP.
00565          16  CSEQ-ATTRB              PIC X.
00566          16  CSEQ                    PIC 9(4).
00567          16  FILLER                  PIC X(8).
00568          16  CCAR-LEN                PIC S9(4)  COMP.
00569          16  CCAR-ATTRB              PIC X.
00570          16  CCAR                    PIC X.
00571          16  FILLER                  PIC X(8).
00572          16  CGRP-LEN                PIC S9(4)  COMP.
00573          16  CGRP-ATTRB              PIC X.
00574          16  CGRP                    PIC X(6).
00575          16  FILLER                  PIC X(7).
00576          16  CST-LEN                 PIC S9(4)  COMP.
00577          16  CST-ATTRB               PIC X.
00578          16  CST                     PIC XX.
00579          16  FILLER                  PIC X(9).
00580          16  CACCT-LEN               PIC S9(4)  COMP.
00581          16  CACCT-ATTRB             PIC X.
00582          16  CACCT                   PIC X(10).
00583          16  CCERT-LEN               PIC S9(4)  COMP.
00584          16  CCERT-ATTRB             PIC X.
00585          16  CCERT                   PIC X(10).
00586          16  CSFX-LEN                PIC S9(4)  COMP.
00587          16  CSFX-ATTRB              PIC X.
00588          16  CSFX                    PIC X.
00589          16  CEFFDT-LEN              PIC S9(4)  COMP.
00590          16  CEFFDT-ATTRB            PIC X.
00591          16  CEFFDT                  PIC 9(6).
00592          16  CLAST-NAME-LEN          PIC S9(4)  COMP.
00593          16  CLAST-NAME-ATTRB        PIC X.
00594          16  CLAST-NAME              PIC X(15).
00595          16  CANCEL-INFO.
00596              20  CKIND1-LEN          PIC S9(4)  COMP.
00597              20  CKIND1-ATTRB        PIC X.
00598              20  CKIND1              PIC XX.
00599              20  CCANDT1-LEN         PIC S9(4)  COMP.
00600              20  CCANDT1-ATTRB       PIC X.
00601              20  CCANDT1             PIC 9(6).
00602              20  CREFUND1-LEN        PIC S9(4)  COMP.
00603              20  CREFUND1-ATTRB      PIC X.
00604              20  CREFUND1I           PIC X(11).
00605              20  CREFUND1O REDEFINES
00606                            CREFUND1I PIC Z(7).99-.
00607              20  CKIND2-LEN          PIC S9(4)  COMP.
00608              20  CKIND2-ATTRB        PIC X.
00609              20  CKIND2              PIC XX.
00610              20  CCANDT2-LEN         PIC S9(4)  COMP.
00611              20  CCANDT2-ATTRB       PIC X.
00612              20  CCANDT2             PIC 9(6).
00613              20  CREFUND2-LEN        PIC S9(4)  COMP.
00614              20  CREFUND2-ATTRB      PIC X.
00615              20  CREFUND2I           PIC X(11).
00616              20  CREFUND2O REDEFINES
00617                            CREFUND2I PIC Z(7).99-.
00618              20  CCHK-LEN            PIC S9(4)  COMP.
00619              20  CCHK-ATTRB          PIC X.
00620              20  CCHK                PIC X.
00621              20  CPAYEE-LEN          PIC S9(4)  COMP.
00622              20  CPAYEE-ATTRB        PIC X.
00623              20  CPAYEE              PIC X(6).
00624              20  CLIVES-LEN          PIC S9(4)  COMP.
00625              20  CLIVES-ATTRB        PIC X.
00626              20  CLIVESI             PIC 999.
00627              20  CLIVESO REDEFINES
00628                           CLIVESI    PIC ZZZ.
00629
00630      EJECT
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
00632  01  DFHCOMMAREA             PIC X(1024).
00633
00634      EJECT
00635  01  PARMLIST.
00636      02  FILLER              PIC S9(8)   COMP.
00637      02  ERPNDT-POINTER      PIC S9(8)   COMP.
00638      02  ELCNTL-POINTER      PIC S9(8)   COMP.
00639      02  ELCERT-POINTER      PIC S9(8)   COMP.
00640      02  ERPNDM-POINTER      PIC S9(8)   COMP.
00641      02  ERACCT-POINTER      PIC S9(8)   COMP.
00642
00643      EJECT
00644
00645 *                                COPY ERCPNDB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCPNDB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.025                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING NEW BUSINESS (ISSUES AND CANCELS) *
00008 *                                                                *
00009 ******************************************************************
00010 *   NOTE: IF THIS FORMAT IS CHANGED, THE SORT RECORD IN THE      *
00011 *         EL861 A/R SYSTEM MAY NEED TO BE CHANGED.               *
00012 ******************************************************************
00013 *                                                                *
00014 *                                                                *
00015 *   FILE TYPE = VSAM,KSDS                                        *
00016 *   RECORD SIZE = 585  RECFORM = FIXED                           *
00017 *                                                                *
00018 *   BASE CLUSTER = ERPNDB                         RKP=2,LEN=11   *
00019 *       ALTERNATE PATH1 = ERPNDB2  (BY CAR GRP STATE ACCOUNT     *
00020 *                                  EFF-DT CERT CHG-SEQ REC-TYPE) *
00021 *                                                 RKP=13,LEN=36  *
00022 *       ALTERNATE PATH2 = ERPNDB3  (BY CO, ORIGINAL BATCH, SEQ.  *
00023 *                                      AND CHG-SEQ.)             *
00024 *                                                RKP=49,LEN=11   *
00025 *       ALTERNATE PATH3 = ERPNDB4  (BY CO, CSR-ID, BATCH, SEQ.   *
00026 *                                      AND CHG-SEQ.)             *
00027 *                                                RKP=60,LEN=15   *
00028 *                                                                *
00029 *   LOG = NO                                                     *
00030 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00031 ******************************************************************
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
011904* 011904                   PEMA  ADD TOTAL FEE PROCESSING
040504* 040504    2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
020305* 020305    2005020000000  PEMA  ADD CLP STATE TO PNDB RECORD
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
032306* 032306                   PEMA  ADD BOW LOAN NUMBER
081606* 081606    2006080800002  PEMA  ADD VIN, ISSUES ONLY
073107* 073107  CR2006051600002  PEMA  ADD OVERCHARGE PROCESSING
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
032109* 032109    2009021700002  PEMA  ADD CRED BENE TO PNDB REC
072209* 072209  CR2008101500003  AJRA  ADD BORROWER FIRST NAME TO ENDORS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
122002******************************************************************
00032
00033  01  PENDING-BUSINESS.
00034      12  PB-RECORD-ID                     PIC XX.
00035          88  VALID-PB-ID                        VALUE 'PB'.
00036
00037      12  PB-CONTROL-PRIMARY.
00038          16  PB-COMPANY-CD                PIC X.
00039          16  PB-ENTRY-BATCH               PIC X(6).
00040          16  PB-BATCH-SEQ-NO              PIC S9(4)     COMP.
00041          16  PB-BATCH-CHG-SEQ-NO          PIC S9(4)     COMP.
00042
00043      12  PB-CONTROL-BY-ACCOUNT.
00044          16  PB-COMPANY-CD-A1             PIC X.
00045          16  PB-CARRIER                   PIC X.
00046          16  PB-GROUPING.
00047              20  PB-GROUPING-PREFIX       PIC XXX.
00048              20  PB-GROUPING-PRIME        PIC XXX.
00049          16  PB-STATE                     PIC XX.
00050          16  PB-ACCOUNT.
00051              20  PB-ACCOUNT-PREFIX        PIC X(4).
00052              20  PB-ACCOUNT-PRIME         PIC X(6).
00053          16  PB-CERT-EFF-DT               PIC XX.
00054          16  PB-CERT-NO.
00055              20  PB-CERT-PRIME            PIC X(10).
00056              20  PB-CERT-SFX              PIC X.
00057          16  PB-ALT-CHG-SEQ-NO            PIC S9(4)     COMP.
00058
00059          16  PB-RECORD-TYPE               PIC X.
00060              88  PB-MAILING-DATA                VALUE '0'.
00061              88  PB-ISSUE                       VALUE '1'.
00062              88  PB-CANCELLATION                VALUE '2'.
00063              88  PB-BATCH-TRAILER               VALUE '9'.
00064
00065      12  PB-CONTROL-BY-ORIG-BATCH.
00066          16  PB-ORIGINAL-COMPANY-CD       PIC X.
00067          16  PB-ORIGINAL-ENTRY-BATCH      PIC X(6).
00068          16  PB-ORIGINAL-SEQ-NO           PIC S9(4)     COMP.
00069          16  PB-ORIGINAL-CHG-SEQ-NO       PIC S9(4)     COMP.
00070
00071      12  PB-CONTROL-BY-CSR.
00072          16  PB-CSR-COMPANY-CD            PIC X.
00073          16  PB-CSR-ID                    PIC X(4).
00074          16  PB-CSR-ENTRY-BATCH           PIC X(6).
00075          16  PB-CSR-BATCH-SEQ-NO          PIC S9(4)     COMP.
00076          16  PB-CSR-BATCH-CHG-SEQ-NO      PIC S9(4)     COMP.
00077 ******************************************************************
00078 *    MAILING DATA IS PROCESSED IN ONLY PRGS. EL512 & EL513       *
00079 ******************************************************************
00080
00081      12  PB-LAST-MAINT-DT                 PIC XX.
00082      12  PB-LAST-MAINT-BY                 PIC X(4).
00083      12  PB-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00084
00085      12  PB-RECORD-BODY                   PIC X(375).
00086
00087      12  PB-ISSUE-RECORD   REDEFINES PB-RECORD-BODY.
00088          16  PB-CERT-ORIGIN               PIC X.
00089              88  CLASIC-CREATED-CERT         VALUE '1'.
00090          16  PB-I-NAME.
00091              20  PB-I-INSURED-LAST-NAME   PIC X(15).
00092              20  PB-I-INSURED-FIRST-NAME.
00093                  24  PB-I-INSURED-1ST-INIT PIC X.
00094                  24  FILLER                PIC X(9).
00095              20  PB-I-INSURED-MIDDLE-INIT PIC X.
00096          16  PB-I-AGE                     PIC S99   COMP-3.
00097          16  PB-I-JOINT-AGE               PIC S99   COMP-3.
00098          16  PB-I-BIRTHDAY                PIC XX.
00099          16  PB-I-INSURED-SEX             PIC X.
00100              88  PB-SEX-MALE     VALUE 'M'.
00101              88  PB-SEX-FEMALE   VALUE 'F'.
00102
00103          16  PB-I-LF-TERM                 PIC S999   COMP-3.
00104          16  PB-I-AH-TERM                 PIC S999   COMP-3.
00105          16  PB-I-LOAN-TERM               PIC S999   COMP-3.
00106          16  PB-I-PAY-FREQUENCY           PIC S99    COMP-3.
00107          16  PB-I-SKIP-CODE               PIC X.
00108              88  PB-NO-MONTHS-SKIPPED      VALUE ' ' '0'.
00109              88  PB-SKIP-JULY              VALUE '1'.
00110              88  PB-SKIP-AUGUST            VALUE '2'.
00111              88  PB-SKIP-SEPTEMBER         VALUE '3'.
00112              88  PB-SKIP-JULY-AUG          VALUE '4'.
00113              88  PB-SKIP-AUG-SEPT          VALUE '5'.
00114              88  PB-SKIP-JULY-AUG-SEPT     VALUE '6'.
00115              88  PB-SKIP-JUNE-JULY-AUG     VALUE '7'.
00116              88  PB-SKIP-JUNE              VALUE '8'.
00117              88  PB-SKIP-JUNE-JULY         VALUE '9'.
00118              88  PB-SKIP-AUG-SEPT-OCT      VALUE 'A'.
00119              88  PB-SKIP-BI-WKLY-3RD-PMT   VALUE 'X'.
00120          16  PB-I-TERM-TYPE               PIC X.
00121              88  PB-PAID-MONTHLY           VALUE ' ' 'M'.
00122              88  PB-PAID-WEEKLY            VALUE 'W'.
00123              88  PB-PAID-SEMI-MONTHLY      VALUE 'S'.
00124              88  PB-PAID-BI-WEEKLY         VALUE 'B'.
00125              88  PB-PAID-13-YEARLY         VALUE 'T'.
00126          16  PB-I-NO-OF-PAYMENTS          PIC S999   COMP-3.
00127          16  PB-I-POLICY-FORM-NO          PIC X(12).
00128          16  PB-I-DATA-ENTRY-SW           PIC X.
00129              88  PB-EFF-DT-PROCESSING      VALUE '1' ' '.
00130              88  PB-EXT-DAYS-PROCESSING    VALUE '2'.
00131              88  PB-EXPIRE-DT-PROCESSING   VALUE '3'.
00132              88  PB-1ST-PMT-DT-PROCESSING  VALUE '4'.
00133          16  PB-I-PAYMENT-AMOUNT          PIC S9(7)V99  COMP-3.
073107         16  PB-I-DCC-OVER-CHG-AMT        PIC S9(5)V99  COMP-3.
011410*        16  PB-I-MICROFILM-NO            PIC S9(9)      COMP-3.
011410         16  PB-I-AH-CLP                  PIC S9(5)V99 COMP-3.
011410         16  FILLER                       PIC X.
00136
00137          16  PB-I-LIFE-BENEFIT-CD         PIC XX.
00138              88  PB-VALID-LIFE               VALUE '01' THRU '89'.
00139              88  PB-INVALID-LIFE             VALUE '  ' '00'
00140                                                    '90' THRU '99'.
00141          16  PB-I-LF-BENEFIT-CD   REDEFINES PB-I-LIFE-BENEFIT-CD
00142                                           PIC XX.
00143          16  PB-I-LF-BENEFIT-AMT          PIC S9(9)V99   COMP-3.
100703         16  PB-I-AMOUNT-FINANCED REDEFINES
100703                  PB-I-LF-BENEFIT-AMT     PIC S9(9)V99   COMP-3.
00144          16  PB-I-LF-ALT-BENEFIT-AMT      PIC S9(9)V99   COMP-3.
100703         16  PB-I-UNPAID-CASH-PRICE REDEFINES
100703                  PB-I-LF-ALT-BENEFIT-AMT PIC S9(9)V99   COMP-3.
00145          16  PB-I-LF-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
00146          16  PB-I-LF-ALT-PREMIUM-AMT      PIC S9(7)V99   COMP-3.
100703         16  PB-I-CLP-AMOUNT REDEFINES
100703                  PB-I-LF-ALT-PREMIUM-AMT PIC S9(7)V99   COMP-3.
00147          16  PB-I-LF-CALC-FLAG            PIC X.
00148              88 PB-COMP-LF-PREM               VALUE '?'.
00149          16  PB-I-LF-PREM-CALC            PIC S9(7)V99   COMP-3.
00150          16  PB-I-LF-ALT-PREM-CALC        PIC S9(7)V99   COMP-3.
00151          16  PB-I-LF-RATE                 PIC S99V9(5)   COMP-3.
00152          16  PB-I-LF-ALT-RATE             PIC S99V9(5)   COMP-3.
00153          16  PB-I-LF-POLICY-FEE           PIC S9(3)V99   COMP-3.
00154          16  PB-I-LF-REI-RATE             PIC S99V9(5)   COMP-3.
00155          16  PB-I-LF-ALT-REI-RATE         PIC S99V9(5)   COMP-3.
00156          16  PB-I-LF-ABBR                 PIC XXX.
00157          16  PB-I-LF-INPUT-CD             PIC XX.
00158
00159          16  PB-I-AH-BENEFIT-CD           PIC XX.
00160              88  PB-VALID-AH                 VALUE '01' THRU '89'.
00161              88  PB-INVALID-AH               VALUE '  ' '00'
00162                                                    '90' THRU '99'.
00163          16  PB-I-AH-BENEFIT-AMT          PIC S9(7)V99   COMP-3.
00164          16  PB-I-AH-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
00165          16  PB-I-AH-CALC-FLAG            PIC X.
00166              88 PB-COMP-AH-PREM                  VALUE '?'.
00167          16  PB-I-AH-PREM-CALC            PIC S9(7)V99   COMP-3.
00168          16  PB-I-AH-RATE                 PIC S99V9(5)   COMP-3.
00169          16  PB-I-AH-POLICY-FEE           PIC S9(3)V99   COMP-3.
00170          16  PB-I-AH-REI-RATE             PIC S99V9(5)   COMP-3.
00171          16  PB-I-AH-RATE-TRM             PIC S999       COMP-3.
00172          16  PB-I-AH-ABBR                 PIC XXX.
00173          16  PB-I-AH-INPUT-CD             PIC XXX.
00174
00175          16  PB-I-SPECIAL-REIN-CODE       PIC X.
00176          16  PB-I-REIN-TABLE              PIC XXX.
00177          16  PB-I-BUSINESS-TYPE           PIC 99.
00178          16  PB-I-INDV-GRP-CD             PIC X.
00179          16  PB-I-MORT-CODE.
00180              20  PB-I-TABLE               PIC X.
00181              20  PB-I-INTEREST            PIC XX.
00182              20  PB-I-MORT-TYP            PIC X.
00183          16  PB-I-LF-CRIT-PER             PIC S9(3)      COMP-3.
00184          16  PB-I-AH-CRIT-PER             PIC S9(3)      COMP-3.
011410         16  PB-I-LF-CLP                  PIC S9(5)V99   COMP-3.
00186          16  PB-I-INDV-GRP-OVRD           PIC X.
00187          16  PB-I-RATE-CLASS-OVRD         PIC XX.
00188          16  PB-I-SIG-SW                  PIC X.
00189              88  PB-POLICY-SIGNED             VALUE 'Y'.
00190          16  PB-I-RATE-CLASS              PIC XX.
00191          16  PB-I-RATE-DEVIATION-LF       PIC XXX.
00192          16  PB-I-RATE-DEVIATION-AH       PIC XXX.
00193          16  PB-I-RATE-DEV-PCT-LF         PIC S9V9(6)    COMP-3.
00194          16  PB-I-RATE-DEV-PCT-AH         PIC S9V9(6)    COMP-3.
00195          16  PB-I-LIFE-COMMISSION         PIC SV9(5)     COMP-3.
00196          16  PB-I-JOINT-COMMISSION        PIC SV9(5)     COMP-3.
00197          16  PB-I-AH-COMMISSION           PIC SV9(5)     COMP-3.
00198          16  PB-I-BENEFIT-TYPE            PIC XXX.
00199          16  PB-I-OB-FLAG                 PIC X.
00200              88  PB-I-OB                      VALUE 'B'.
00201              88  PB-I-SUMMARY                 VALUE 'Z'.
00202          16  PB-I-ENTRY-STATUS            PIC X.
00203              88  PB-I-POLICY-IS-ACTIVE        VALUE '1' '3' '4'
122002                                              'M' '5' '9' '2'.
00205              88  PB-I-NORMAL-ENTRY            VALUE '1'.
00206              88  PB-I-POLICY-PENDING          VALUE '2'.
00207              88  PB-I-CONVERSION-ENTRY        VALUE '4'.
00208              88  PB-I-POLICY-IS-REISSUE       VALUE '5'.
122002             88  PB-I-POLICY-IS-MONTHLY       VALUE 'M'.
00209              88  PB-I-REIN-ONLY               VALUE '9'.
00210              88  PB-I-POLICY-IS-DECLINED      VALUE 'D'.
00211              88  PB-I-POLICY-IS-VOIDED        VALUE 'V'.
00212              88  PB-I-PREM-ACCTNG-ONLY        VALUE 'P'.
00213              88  PB-I-UNDERWRITE-POLICY       VALUE 'U'.
00214          16  PB-I-INT-CODE                PIC X.
00215              88  PB-ADD-ON-INTEREST           VALUE 'A'.
00216              88  PB-SIMPLE-INTEREST           VALUE 'S'.
00217          16  PB-I-LOAN-APR                PIC 9(3)V9(4)   COMP-3.
00218          16  PB-I-SOC-SEC-NO              PIC X(11).
00219          16  PB-I-MEMBER-NO               PIC X(12).
00220          16  PB-I-CURR-SEQ                PIC S9(4)       COMP.
110105*        16  PB-I-LOAN-OFFICER            PIC XXX.
110105         16  PB-I-OLD-LOF                 PIC XXX.
00222          16  PB-I-LF-EXPIRE-DT            PIC XX.
00223          16  PB-I-AH-EXPIRE-DT            PIC XX.
00224          16  PB-I-EXTENTION-DAYS          PIC S999        COMP-3.
00225          16  PB-I-TERM-IN-DAYS            PIC S9(5)       COMP-3.
00226          16  PB-I-LIFE-INDICATOR          PIC X.
00227              88  PB-I-JOINT-COVERAGE         VALUE 'J'.
00228          16  PB-I-LIVES                   PIC S9(7)       COMP-3.
00229          16  PB-I-MAIL-ADDRS-SW           PIC X.
00230              88 PB-I-MAIL-ADDRS-NOT-PRESENT  VALUE ' '.
00231              88 PB-I-MAIL-ADDRS-PRESENT      VALUE '1'.
00232          16  PB-I-1ST-PMT-DT              PIC XX.
00233          16  PB-I-JOINT-INSURED.
00234              20 PB-I-JOINT-LAST-NAME      PIC X(15).
00235              20 PB-I-JOINT-FIRST-NAME.
00236                 24  PB-I-JOINT-FIRST-INIT PIC X.
00237                 24  FILLER                PIC X(9).
00238              20 PB-I-JOINT-MIDDLE-INIT    PIC X.
100703*        16  PB-I-BENEFICIARY-NAME        PIC X(25).
100703         16  PB-I-BENEFICIARY-NAME.
100703             20  PB-I-BANK-NUMBER         PIC X(10).
100703             20  FILLER                   PIC X(15).
00240          16  PB-I-LAST-ADD-ON-DT          PIC XX.
011904         16  PB-I-REFERENCE               PIC X(12).
011904         16  FILLER REDEFINES PB-I-REFERENCE.
011904             20  PB-I-TOT-FEES            PIC S9(7)V99 COMP-3.
011904             20  PB-I-TOT-FEES-CALC       PIC S9(7)V99 COMP-3.
020305             20  PB-I-CLP-STATE           PIC XX.
00242          16  PB-I-UNDERWRITING-STATUS     PIC X.
00243              88  PB-I-POLICY-ACCEPTED         VALUE 'A' 'N'.
00244              88  PB-I-POLICY-DECLINED         VALUE 'D'.
00245              88  PB-I-NEEDS-UNDERWRITING      VALUE 'U'.
00246          16  PB-I-STATE-TAX               PIC S9(7)V99 COMP-3.
00247          16  PB-I-MUNI-TAX                PIC S9(7)V99 COMP-3.
00248          16  PB-I-RESIDENT-STATE          PIC XX.
00249          16  PB-I-RATE-CODE               PIC X(4).
00250          16  PB-I-NUM-BILLED              PIC S9(7)    COMP-3.
PEMMOD         16  PB-I-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
PEMMOD         16  PB-I-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
100703         16  PB-I-BANK-FEE                PIC S999V99  COMP-3.
100703         16  PB-I-BANK-NOCHRGB            PIC 99.
040504         16  PB-I-ADDL-CLP                PIC S9(5)V99 COMP-3.
081108         16  PB-I-JOINT-BIRTHDAY          PIC XX.
00252
00253      12  PB-CANCEL-RECORD   REDEFINES PB-RECORD-BODY.
00254          16  PB-C-LF-CANCEL-VOID-SW       PIC X.
00255              88  PB-C-LF-CANCEL-VOIDED        VALUE '1'.
00256          16  PB-C-CANCEL-ORIGIN           PIC X.
00257              88  PB-C-CLAIM-CREATED-CANCEL   VALUE '1'.
00258          16  PB-C-LF-CANCEL-DT            PIC XX.
00259          16  PB-C-LF-CANCEL-AMT           PIC S9(7)V99    COMP-3.
00260          16  PB-C-LF-CALC-REQ             PIC X.
00261              88 PB-COMP-LF-CANCEL            VALUE '?'.
00262          16  PB-C-LF-REF-CALC             PIC S9(7)V99    COMP-3.
00263          16  PB-C-LF-REM-TERM             PIC S9(3)       COMP-3.
00264          16  PB-C-AH-CANCEL-VOID-SW       PIC X.
00265              88  PB-C-AH-CANCEL-VOIDED        VALUE '1'.
00266          16  PB-C-AH-CANCEL-DT            PIC XX.
00267          16  PB-C-AH-CANCEL-AMT           PIC S9(7)V99    COMP-3.
00268          16  PB-C-AH-CALC-REQ             PIC X.
00269              88 PB-COMP-AH-CANCEL            VALUE '?'.
00270          16  PB-C-AH-REF-CALC             PIC S9(7)V99    COMP-3.
00271          16  PB-C-AH-REM-TERM             PIC S9(3)       COMP-3.
00272          16  PB-C-LAST-NAME               PIC X(15).
00273          16  PB-C-REFUND-SW               PIC X.
00274              88  PB-C-REFUND-CREATED          VALUE 'Y'.
00275              88  PB-C-REFUND-REQUESTED        VALUE 'R'.
00276          16  PB-C-LIVES                   PIC S9(3)       COMP-3.
00277          16  PB-C-PAYEE-CODE              PIC X(6).
00278          16  PB-C-LF-REFUND-OVERRIDE      PIC X.
00279          16  PB-C-AH-REFUND-OVERRIDE      PIC X.
00280          16  PB-C-LF-COMM-CHARGEBACK      PIC X.
00281          16  PB-C-AH-COMM-CHARGEBACK      PIC X.
00282          16  PB-C-REFERENCE               PIC X(12).
PEMMOD         16  PB-C-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
PEMMOD         16  PB-C-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
081606         16  PB-C-POST-CARD-IND           PIC X.
081606         16  PB-C-CANCEL-REASON           PIC X.
072308         16  PB-C-REF-INTERFACE-SW        PIC X.
00283          16  FILLER                       PIC X(09).
PEMMOD*        16  FILLER                       PIC X(18).
00284          16  PB-C-POLICY-FORM-NO          PIC X(12).
072308*        16  PB-C-MICROFILM-NO            PIC S9(9)      COMP-3.
072308         16  PB-C-NH-INT-ON-REFS          PIC S9(7)V99   COMP-3.
00286          16  PB-CANCELED-CERT-DATA.
00287              20  PB-CI-INSURED-NAME.
00288                  24  PB-CI-LAST-NAME      PIC X(15).
00289                  24  PB-CI-INITIALS       PIC XX.
00290              20  PB-CI-INSURED-AGE        PIC S99         COMP-3.
00291              20  PB-CI-INSURED-SEX        PIC X.
00292              20  PB-CI-LF-TERM            PIC S999        COMP-3.
00293              20  PB-CI-LF-BENEFIT-CD      PIC XX.
00294              20  PB-CI-LF-BENEFIT-AMT     PIC S9(9)V99    COMP-3.
00295              20  PB-CI-LF-ALT-BENEFIT-AMT PIC S9(9)V99    COMP-3.
00296              20  PB-CI-LF-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00297              20  PB-CI-LF-ALT-PREMIUM-AMT PIC S9(7)V99    COMP-3.
00298              20  PB-CI-AH-TERM            PIC S999        COMP-3.
00299              20  PB-CI-AH-BENEFIT-CD      PIC XX.
00300              20  PB-CI-AH-BENEFIT-AMT     PIC S9(7)V99    COMP-3.
00301              20  PB-CI-AH-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00302              20  PB-CI-RATE-CLASS         PIC XX.
00303              20  PB-CI-RATE-DEV-LF        PIC XXX.
00304              20  PB-CI-RATE-DEV-AH        PIC XXX.
00305              20  PB-CI-RATE-DEV-PCT-LF    PIC S9V9(6)     COMP-3.
00306              20  PB-CI-RATE-DEV-PCT-AH    PIC S9V9(6)     COMP-3.
00307              20  PB-CI-LIFE-COMMISSION    PIC SV9(5)      COMP-3.
00308              20  PB-CI-AH-COMMISSION      PIC SV9(5)      COMP-3.
00309              20  PB-CI-LF-ABBR            PIC X(3).
00310              20  PB-CI-AH-ABBR            PIC X(3).
00311              20  PB-CI-OB-FLAG            PIC X.
00312                  88  PB-CI-OB                VALUE 'B'.
00313              20  PB-CI-LF-POLICY-STATUS   PIC X.
00314                  88  PB-CI-LF-POLICY-IS-ACTIVE       VALUE '1' '3'
122002                                           'M' '4' '5' '9' '2'.
00316                  88  PB-CI-LF-NORMAL-ENTRY           VALUE '1'.
00317                  88  PB-CI-LF-POLICY-PENDING         VALUE '2'.
00318                  88  PB-CI-LF-POLICY-IS-RESTORE      VALUE '3'.
00319                  88  PB-CI-LF-CONVERSION-ENTRY       VALUE '4'.
00320                  88  PB-CI-LF-POLICY-IS-REISSUE      VALUE '5'.
122002                 88  PB-CI-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00321                  88  PB-CI-LF-LUMP-SUM-DISAB         VALUE '6'.
00322                  88  PB-CI-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00323                  88  PB-CI-LF-CANCEL-APPLIED         VALUE '8'.
00324                  88  PB-CI-LF-REIN-ONLY              VALUE '9'.
00325                  88  PB-CI-LF-POLICY-IS-DECLINED     VALUE 'D'.
00326                  88  PB-CI-LF-POLICY-IS-VOID         VALUE 'V'.
00327              20  PB-CI-AH-POLICY-STATUS   PIC X.
00328                  88  PB-CI-AH-POLICY-IS-ACTIVE       VALUE '1' '3'
122002                                           'M' '4' '5' '9' '2'.
00330                  88  PB-CI-AH-NORMAL-ENTRY           VALUE '1'.
00331                  88  PB-CI-AH-POLICY-PENDING         VALUE '2'.
00332                  88  PB-CI-AH-POLICY-IS-RESTORE      VALUE '3'.
00333                  88  PB-CI-AH-CONVERSION-ENTRY       VALUE '4'.
00334                  88  PB-CI-AH-POLICY-IS-REISSUE      VALUE '5'.
122002                 88  PB-CI-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00335                  88  PB-CI-AH-LUMP-SUM-DISAB         VALUE '6'.
00336                  88  PB-CI-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00337                  88  PB-CI-AH-CANCEL-APPLIED         VALUE '8'.
00338                  88  PB-CI-AH-REIN-ONLY              VALUE '9'.
00339                  88  PB-CI-AH-POLICY-IS-DECLINED     VALUE 'D'.
00340                  88  PB-CI-AH-POLICY-IS-VOID         VALUE 'V'.
00341              20  PB-CI-PAY-FREQUENCY      PIC 99.
00342              20  PB-CI-LOAN-APR           PIC 9(3)V9(4)   COMP-3.
00343              20  PB-CI-SOC-SEC-NO         PIC X(11).
00344              20  PB-CI-MEMBER-NO          PIC X(12).
00345              20  PB-CI-INT-CODE           PIC X.
00346                  88  PB-CI-ADD-ON                  VALUE 'A'.
00347                  88  PB-CI-SIMPLE                  VALUE 'S'.
00348              20  PB-CI-LOAN-TERM          PIC S999        COMP-3.
00349              20  PB-CI-LOAN-1ST-PMT-DT    PIC X(2).
00350              20  PB-CI-COMP-EXCP-SW       PIC X.
00351                  88  PB-CI-NO-COMP-EXCP            VALUE ' '.
00352                  88  PB-CI-CERT-HAS-ERCOMM-ENTRY   VALUE '1'.
00353              20  PB-CI-ENTRY-STATUS       PIC X.
00354              20  PB-CI-CURR-SEQ           PIC S9(4)       COMP.
00355              20  PB-CI-AH-PAID-THRU-DT    PIC XX.
00356              20  PB-CI-AH-SETTLEMENT-DT   PIC XX.
00357              20  PB-CI-DEATH-DT           PIC XX.
00358              20  PB-CI-LF-PRIOR-CANCEL-DT PIC XX.
00359              20  PB-CI-AH-PRIOR-CANCEL-DT PIC XX.
00360              20  PB-CI-AH-CANCEL-AMT      PIC S9(7)V99    COMP-3.
00361              20  PB-CI-LF-CANCEL-AMT      PIC S9(7)V99    COMP-3.
00362              20  PB-CI-CREDIT-INTERFACE-SW-1 PIC X.
00363              20  PB-CI-CREDIT-INTERFACE-SW-2 PIC X.
00364              20  PB-CI-ENTRY-DT              PIC XX.
00365              20  PB-CI-ENTRY-BATCH           PIC X(6).
00366              20  PB-CI-LF-EXPIRE-DT          PIC XX.
00367              20  PB-CI-AH-EXPIRE-DT          PIC XX.
00368              20  PB-CI-EXTENTION-DAYS        PIC S999     COMP-3.
00369              20  PB-CI-TERM-IN-DAYS          PIC S9(5)    COMP-3.
110105             20  PB-CI-OLD-LOF               PIC XXX.
110105*            20  PB-CI-LOAN-OFFICER          PIC XXX.
00371              20  PB-CI-LIVES                 PIC S9(3)    COMP-3.
00372              20  PB-CI-LF-CRIT-PER           PIC S9(3)    COMP-3.
00373              20  PB-CI-AH-CRIT-PER           PIC S9(3)    COMP-3.
00374              20  PB-CI-INDV-GRP-CD           PIC X.
100703             20  PB-CI-BENEFICIARY-NAME.
100703                 24  PB-CI-BANK-NUMBER       PIC X(10).
100703                 24  FILLER                  PIC X(15).
00376              20  PB-CI-NOTE-SW               PIC X.
00377              20  PB-CI-CANCEL-FEE            PIC S9(3)V99 COMP-3.
00378              20  PB-CI-MUNI-TAX              PIC S9(7)V99 COMP-3.
00379              20  PB-CI-STATE-TAX             PIC S9(7)V99 COMP-3.
040504             20  PB-CI-ADDL-CLP              PIC S9(5)V99 COMP-3.
110105             20  PB-CI-LOAN-OFFICER          PIC X(5).
032306             20  PB-CI-BOW-LOAN-NUMBER       PIC X(14).
072209             20  PB-CI-FIRST-NAME            PIC X(10).
00380
072209         16  FILLER                       PIC X(17).
072209*032306  16  FILLER                       PIC X(27).
040504*        16  FILLER                       PIC X(46).
00382
00383      12  PB-MAIL-RECORD    REDEFINES PB-RECORD-BODY.
00384          16  FILLER                       PIC X(10).
00385          16  PB-M-INSURED-LAST-NAME       PIC X(15).
00386          16  PB-M-INSURED-FIRST-NAME      PIC X(10).
00387          16  PB-M-INSURED-MID-INIT        PIC X.
00388          16  PB-M-INSURED-AGE             PIC 99.
00389          16  PB-M-INSURED-BIRTHDAY        PIC XX.
00390          16  PB-M-INSURED-SEX             PIC X.
00391          16  PB-M-INSURED-SOC-SEC-NO      PIC X(11).
00392          16  PB-M-INSURED-ADDRESS-1       PIC X(30).
00393          16  PB-M-INSURED-ADDRESS-2       PIC X(30).
00394          16  PB-M-INSURED-CITY-STATE.
051810             20  PB-M-INSURED-CITY        PIC X(28).
051810             20  PB-M-INSURED-STATE       PIC XX.
00395          16  PB-M-INSURED-ZIP-CODE.
00396              20  PB-M-INSURED-ZIP-PRIME.
00397                  24  PB-M-INSURED-ZIP-1   PIC X.
00398                      88  PB-M-CANADIAN-POST-CODE
00399                                              VALUE 'A' THRU 'Z'.
00400                  24  FILLER               PIC X(4).
00401              20  PB-M-INSURED-ZIP-PLUS4   PIC X(4).
00402          16  PB-M-INSURED-CANADIAN-ZIP  REDEFINES
00403                                         PB-M-INSURED-ZIP-CODE.
00404              20  PM-M-INS-CAN-POST1       PIC XXX.
00405              20  PM-M-INS-CAN-POST2       PIC XXX.
00406              20  FILLER                   PIC XXX.
00407          16  PB-M-INSURED-PHONE-NO        PIC 9(10).
081108         16  PB-M-JOINT-BIRTHDAY          PIC XX.
               16  PB-M-CRED-BENE-NAME          PIC X(30).
               16  PB-M-CRED-BENE-ADDR1         PIC X(30).
               16  PB-M-CRED-BENE-ADDR2         PIC X(30).
               16  PB-M-CRED-BENE-CITYST.
                   20  PB-M-CRED-BENE-CITY      PIC X(28).
                   20  PB-M-CRED-BENE-STATE     PIC XX.
081108         16  FILLER                       PIC X(92).
00409
00410      12  PB-BATCH-RECORD   REDEFINES PB-RECORD-BODY.
00411          16  FILLER                       PIC X(10).
00412          16  PB-B-LF-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00413          16  PB-B-LF-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00414          16  PB-B-LF-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00415          16  PB-B-LF-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00416          16  PB-B-LF-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00417          16  PB-B-LF-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00418          16  PB-B-AH-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00419          16  PB-B-AH-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00420          16  PB-B-AH-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00421          16  PB-B-AH-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00422          16  PB-B-AH-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00423          16  PB-B-AH-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00424          16  PB-B-ISSUE-CNT-REMITTED      PIC S9(5)    COMP-3.
00425          16  PB-B-ISSUE-CNT-ENTERED       PIC S9(5)    COMP-3.
00426          16  PB-B-CANCEL-CNT-REMITTED     PIC S9(5)    COMP-3.
00427          16  PB-B-CANCEL-CNT-ENTERED      PIC S9(5)    COMP-3.
00428          16  PB-B-HIGHEST-SEQ-NO          PIC S9(4)    COMP.
00429          16  PB-ACCOUNT-NAME              PIC X(30).
00430          16  PB-PREM-REF-RPT-FLAG         PIC X.
00431          16  PB-REFERENCE                 PIC X(12).
00432          16  PB-B-RECEIVED-DT             PIC XX.
00433          16  FILLER                       PIC X(234).
00434
00435      12  PB-RECORD-STATUS.
00436          16  PB-CREDIT-SELECT-DT          PIC XX.
00437          16  PB-CREDIT-ACCEPT-DT          PIC XX.
00438          16  PB-BILLED-DT                 PIC XX.
00439          16  PB-BILLING-STATUS            PIC X.
00440              88  PB-ENTRY-REVERSED            VALUE 'R'.
00441              88  PB-EL860-INTERNAL-ERROR      VALUE 'E'.
00442              88  PB-EL860-INTERNAL-PROCESS    VALUE 'P'.
00443          16  PB-RECORD-BILL               PIC X.
00444              88  PB-RECORD-ON-HOLD            VALUE 'H'.
00445              88  PB-RECORD-RETURNED           VALUE 'R'.
00446              88  PB-RECORD-ENDORSED           VALUE 'E'.
00447              88  PB-OVERRIDE-LIFE             VALUE 'L'.
00448              88  PB-OVERRIDE-AH               VALUE 'A'.
00449              88  PB-OVERRIDE-BOTH             VALUE 'B'.
00450          16  PB-BATCH-ENTRY               PIC X.
00451              88  PB-POLICY-IS-DECLINED        VALUE 'D'.
00452              88  PB-REIN-ONLY-CERT            VALUE 'R'.
00453              88  PB-REISSUED-CERT             VALUE 'E'.
122002             88  PB-MONTHLY-CERT              VALUE 'M'.
00454              88  PB-PREM-ACCTNG-ONLY          VALUE 'P'.
00455              88  PB-NEEDS-UNDERWRITING        VALUE 'U'.
00456              88  PB-POLICY-IS-VOIDED          VALUE 'V'.
00457          16  PB-FORCE-CODE                PIC X.
00458              88  PB-FORCE-OFF                 VALUE ' ' '0'.
00459              88  PB-ISSUE-FORCE               VALUE 'A' 'O'.
00460              88  PB-CANCEL-FORCE              VALUE '8'.
00461              88  PB-ALL-ISSUE-FORCED          VALUE 'A' 'O'.
00462              88  PB-ALL-CANCEL-FORCED         VALUE '8'.
00463              88  PB-ALL-CANCEL-FORCED-NO-FEE  VALUE '9'.
00464              88  PB-CANCEL-DATE-FORCED        VALUE 'D'.
00465              88  PB-CANCEL-DATE-FORCED-NO-FEE VALUE 'E'.
00466              88  PB-ISSUE-DATE-FORCED         VALUE 'D'.
073107             88  PB-OVERCHARGE-FORCE          VALUE 'O'.
00467          16  PB-FATAL-FLAG                PIC X.
00468              88  PB-FATAL-ERRORS              VALUE 'X'.
00469          16  PB-FORCE-ER-CD               PIC X.
00470              88  PB-FORCE-ERRORS              VALUE 'F'.
00471              88  PB-UNFORCED-ERRORS           VALUE 'X'.
00472          16  PB-WARN-ER-CD                PIC X.
00473              88  PB-WARNING-ERRORS            VALUE 'W'.
00474          16  FILLER                       PIC X.
00475          16  PB-OUT-BAL-CD                PIC X.
00476              88  PB-OUT-OF-BAL                VALUE 'O'.
00477          16  PB-LIFE-OVERRIDE-L1          PIC X.
00478          16  PB-AH-OVERRIDE-L1            PIC X.
00479          16  PB-INPUT-DT                  PIC XX.
00480          16  PB-INPUT-BY                  PIC X(4).
00481          16  PB-CHG-COUNT                 PIC 9(3)        COMP-3.
00482          16  PB-CALC-TOLERANCE            PIC 9(3)V99     COMP-3.
00483          16  PB-TOLERANCE-REJECT-SW       PIC X.
00484          16  PB-LF-EARNING-METHOD         PIC X.
00485          16  PB-AH-EARNING-METHOD         PIC X.
00486          16  PB-LF-TERM-CALC-METHOD       PIC X.
00487          16  PB-AH-TERM-CALC-METHOD       PIC X.
00488          16  PB-REIN-CD                   PIC XXX.
00489          16  PB-LF-REFUND-TYPE            PIC X.
00490          16  PB-AH-REFUND-TYPE            PIC X.
00491          16  PB-ACCT-EFF-DT               PIC XX.
00492          16  PB-ACCT-EXP-DT               PIC XX.
00493          16  PB-COMPANY-ID                PIC X(3).
00494          16  PB-LF-BILLED-AMTS            PIC S9(7)V99  COMP-3.
00495          16  PB-AH-BILLED-AMTS            PIC S9(7)V99  COMP-3.
00496          16  PB-SV-CARRIER                PIC X.
00497          16  PB-SV-GROUPING               PIC X(6).
00498          16  PB-SV-STATE                  PIC XX.
00499          16  PB-CONFIRMATION-REPT-DT      PIC XX.
00500          16  PB-GA-BILLING-INFO.
00501              20  PB-GA-BILL-DT OCCURS 5 TIMES
00502                                           PIC XX.
00503          16  PB-SV-REMIT-TO  REDEFINES
00504              PB-GA-BILLING-INFO           PIC X(10).
00505          16  PB-NO-OF-ERRORS              PIC S9(3) COMP-3.
110105         16  PB-I-LOAN-OFFICER            PIC X(5).
081606         16  PB-I-VIN                     PIC X(17).
00506
110105         16  FILLER                       PIC X(04).
110105         16  IMNET-BYPASS-SW              PIC X.
00508
00509 ******************************************************************
00510 *                COMMON EDIT ERRORS                              *
00511 ******************************************************************
00512
00513      12  PB-COMMON-ERRORS.
00514          16  PB-COMMON-ERROR    OCCURS 10 TIMES
00515                                            PIC S9(4)     COMP.
00516
00517 ******************************************************************
00646      EJECT
00647
00648 *                                COPY ELCCNTL.
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
00649      EJECT
00650
00651 *                                COPY ELCCERT.
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
061405     12  CM-USER-RESERVED                  PIC XXX.
00286 ******************************************************************
00652      EJECT
00653
00654 *                                COPY ERCPNDM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCPNDM                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING MAILING DATA                      *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
CIDMOD*   RECORD SIZE = 374   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERPNDM                 RKP=2,LEN=11      *
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
071108* 071108  CR2008040800002  PEMA  ADD CRED BENE INFORMATION
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
00017 ******************************************************************
00018
00019  01  PENDING-MAILING-DATA.
00020      12  PM-RECORD-ID                      PIC XX.
00021          88  VALID-MA-ID                       VALUE 'PM'.
00022
00023      12  PM-CONTROL-PRIMARY.
00024          16  PM-COMPANY-CD                 PIC X.
00025          16  PM-ENTRY-BATCH                PIC X(6).
00026          16  PM-BATCH-SEQ-NO               PIC S9(4)     COMP.
00027          16  PM-BATCH-CHG-SEQ-NO           PIC S9(4)     COMP.
00028
00029      12  FILLER                            PIC X(14).
00030
00031      12  PM-ACCESS-CONTROL.
00032          16  PM-SOURCE-SYSTEM              PIC XX.
00033              88  PM-FROM-CREDIT                VALUE 'CR'.
00034              88  PM-FROM-VSI                   VALUE 'VS'.
00035              88  PM-FROM-WARRANTY              VALUE 'WA'.
00036              88  PM-FROM-OTHER                 VALUE 'OT'.
00037          16  PM-RECORD-ADD-DT              PIC XX.
00038          16  PM-RECORD-ADDED-BY            PIC XXXX.
00039          16  PM-LAST-MAINT-DT              PIC XX.
00040          16  PM-LAST-MAINT-BY              PIC XXXX.
00041          16  PM-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
00042
00043      12  PM-PROFILE-INFO.
00044          16  PM-QUALIFY-CODE-1             PIC XX.
00045          16  PM-QUALIFY-CODE-2             PIC XX.
00046          16  PM-QUALIFY-CODE-3             PIC XX.
00047          16  PM-QUALIFY-CODE-4             PIC XX.
00048          16  PM-QUALIFY-CODE-5             PIC XX.
00049
00050          16  PM-INSURED-LAST-NAME          PIC X(15).
00051          16  PM-INSURED-FIRST-NAME         PIC X(10).
00052          16  PM-INSURED-MIDDLE-INIT        PIC X.
00053          16  PM-INSURED-ISSUE-AGE          PIC 99.
00054          16  PM-INSURED-BIRTH-DT           PIC XX.
00055          16  PM-INSURED-SEX                PIC X.
00056              88  PM-SEX-MALE                   VALUE 'M'.
00057              88  PM-SEX-FEMALE                 VALUE 'F'.
00058          16  PM-INSURED-SOC-SEC-NO         PIC X(11).
00059
080406         16  PM-ADDRESS-CORRECTED          PIC X.
081108         16  PM-JOINT-BIRTH-DT             PIC XX.
00060 *        16  FILLER                        PIC X(12).
00061
00062          16  PM-ADDRESS-LINE-1             PIC X(30).
00063          16  PM-ADDRESS-LINE-2             PIC X(30).
00064          16  PM-CITY-STATE.
                   20  PM-CITY                   PIC X(28).
                   20  PM-STATE                  PIC XX.
00065          16  PM-ZIP.
00066              20  PM-ZIP-CODE.
00067                  24  PM-ZIP-1              PIC X.
00068                      88  PM-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00069                  24  FILLER                PIC X(4).
00070              20  PM-ZIP-PLUS4              PIC X(4).
00071          16  PM-CANADIAN-ZIP  REDEFINES  PM-ZIP.
00072              20  PM-CAN-POST1              PIC XXX.
00073              20  PM-CAN-POST2              PIC XXX.
00074              20  FILLER                    PIC XXX.
00075
00076          16  PM-PHONE-NO                   PIC 9(11)       COMP-3.
00077
00078          16  FILLER                        PIC X(03).
00079
           12  PM-CRED-BENE-INFO.
CIDMOD         16  PM-CRED-BENE-NAME             PIC X(25).
CIDMOD         16  PM-CRED-BENE-ADDR             PIC X(30).
071108         16  PM-CRED-BENE-ADDR2            PIC X(30).
CIDMOD         16  PM-CRED-BENE-CTYST.
                   20  PM-CRED-BENE-CITY         PIC X(28).
                   20  PM-CRED-BENE-STATE        PIC XX.
CIDMOD         16  PM-CRED-BENE-ZIP.
CIDMOD             20  PM-CB-ZIP-CODE.
CIDMOD                 24  PM-CB-ZIP-1           PIC X.
CIDMOD                     88  PM-CB-CANADIAN-POST-CODE
                                        VALUE 'A' THRU 'Z'.
CIDMOD                 24  FILLER                PIC X(4).
CIDMOD             20  PM-CB-ZIP-PLUS4           PIC X(4).
CIDMOD         16  PM-CB-CANADIAN-ZIP  REDEFINES  PM-CRED-BENE-ZIP.
CIDMOD             20  PM-CB-CAN-POST1           PIC XXX.
CIDMOD             20  PM-CB-CAN-POST2           PIC XXX.
CIDMOD             20  FILLER                    PIC XXX.
080406     12  PM-POST-CARD-MAIL-DATA.
080406         16  PM-MAIL-DATA OCCURS 7.
080406             20  PM-MAIL-TYPE              PIC X.
080406                 88  PM-12MO-MAILING           VALUE '1'.
080406                 88  PM-EXP-MAILING            VALUE '2'.
080406             20  PM-MAIL-STATUS            PIC X.
080406                 88  PM-MAIL-ST-MAILED         VALUE '1'.
080406                 88  PM-MAIL-ST-RETURNED       VALUE '2'.
080406                 88  PM-MAIL-ST-NOT-MAILED     VALUE '3'.
080406             20  PM-MAIL-DATE              PIC XX.
080406     12  FILLER                            PIC XX.
           12  FILLER                            PIC X(12).
080406*    12  FILLER                            PIC X(30).
00075
00081 ******************************************************************
00655      EJECT
00656
00657 *                                COPY ERCACCT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCACCT                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.031                          *
00006 *                                                                *
00007 *   CREDIT SYSTEM ACCOUNT MASTER FILE                            *
00008 *                                                                *
00009 *   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
00010 *   VSAM ACCOUNT MASTER FILES.                                   *
00011 *                                                                *
00012 *   FILE DESCRIPTION = ACCOUNT OR PRODUCER FILES                 *
00013 *                                                                *
00014 *   FILE TYPE = VSAM,KSDS                                        *
00015 *   RECORD SIZE = 2000  RECFORM = FIX                            *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ERACCT                    RKP=2,LEN=26   *
00018 *       ALTERNATE PATH1 = ERACCT2 (ALT GROUPING) RKP=28,LEN=26   *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
102004*                   C H A N G E   L O G
102004*
102004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102004*-----------------------------------------------------------------
102004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102004* EFFECTIVE    NUMBER
102004*-----------------------------------------------------------------
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
022808* 022808    2007083100002  PEMA  ADD FREEZE STATUS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
102004******************************************************************
00025
00026  01  ACCOUNT-MASTER.
00027      12  AM-RECORD-ID                      PIC XX.
00028          88  VALID-AM-ID                      VALUE 'AM'.
00029
00030      12  AM-CONTROL-PRIMARY.
00031          16  AM-COMPANY-CD                 PIC X.
00032          16  AM-MSTR-CNTRL.
00033              20  AM-CONTROL-A.
00034                  24  AM-CARRIER            PIC X.
00035                  24  AM-GROUPING.
00036                      28 AM-GROUPING-PREFIX PIC XXX.
00037                      28 AM-GROUPING-PRIME  PIC XXX.
00038                  24  AM-STATE              PIC XX.
00039                  24  AM-ACCOUNT.
00040                      28  AM-ACCOUNT-PREFIX PIC X(4).
00041                      28  AM-ACCOUNT-PRIME  PIC X(6).
00042              20  AM-CNTRL-1   REDEFINES   AM-CONTROL-A
00043                                            PIC X(19).
00044              20  AM-CNTRL-B.
00045                  24  AM-EXPIRATION-DT      PIC XX.
00046                  24  FILLER                PIC X(4).
00047              20  AM-CNTRL-2 REDEFINES AM-CNTRL-B.
00048                  24  AM-EXPIRE-DT          PIC 9(11)  COMP-3.
00049
00050      12  AM-CONTROL-BY-VAR-GRP.
00051          16  AM-COMPANY-CD-A1              PIC X.
00052          16  AM-VG-CARRIER                 PIC X.
00053          16  AM-VG-GROUPING                PIC X(6).
00054          16  AM-VG-STATE                   PIC XX.
00055          16  AM-VG-ACCOUNT                 PIC X(10).
00056          16  AM-VG-DATE.
00057              20  AM-VG-EXPIRATION-DT       PIC XX.
00058              20  FILLER                    PIC X(4).
00059          16  AM-VG-EXP-DATE REDEFINES AM-VG-DATE
00060                                            PIC 9(11)      COMP-3.
00061      12  AM-MAINT-INFORMATION.
00062          16  AM-LAST-MAINT-DT              PIC XX.
00063          16  AM-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00064          16  AM-LAST-MAINT-USER            PIC X(4).
00065          16  FILLER                        PIC XX.
00066
00067      12  AM-EFFECTIVE-DT                   PIC XX.
00068      12  AM-EFFECT-DT                      PIC 9(11)      COMP-3.
00069
00070      12  AM-PREV-DATES  COMP-3.
00071          16  AM-PREV-EXP-DT                PIC 9(11).
00072          16  AM-PREV-EFF-DT                PIC 9(11).
00073
00074      12  AM-REPORT-CODE-1                  PIC X(10).
00075      12  AM-REPORT-CODE-2                  PIC X(10).
00076
00077      12  AM-CITY-CODE                      PIC X(4).
00078      12  AM-COUNTY-PARISH                  PIC X(6).
00079
00080      12  AM-NAME                           PIC X(30).
00081      12  AM-PERSON                         PIC X(30).
00082      12  AM-ADDRS                          PIC X(30).
00083      12  AM-CITY.
               16  AM-ADDR-CITY                  PIC X(28).
               16  AM-ADDR-STATE                 PIC XX.
00084      12  AM-ZIP.
00085          16  AM-ZIP-PRIME.
00086              20  AM-ZIP-PRI-1ST            PIC X.
00087                  88  AM-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
00088              20  FILLER                    PIC X(4).
00089          16  AM-ZIP-PLUS4                  PIC X(4).
00090      12  AM-CANADIAN-POSTAL-CODE  REDEFINES  AM-ZIP.
00091          16  AM-CAN-POSTAL-1               PIC XXX.
00092          16  AM-CAN-POSTAL-2               PIC XXX.
00093          16  FILLER                        PIC XXX.
00094      12  AM-TEL-NO.
00095          16  AM-AREA-CODE                  PIC 999.
00096          16  AM-TEL-PRE                    PIC 999.
00097          16  AM-TEL-NBR                    PIC 9(4).
00098      12  AM-TEL-LOC                        PIC X.
00099          88  AM-TEL-AT-HOME                   VALUE 'H'.
00100          88  AM-TEL-AT-BUSINESS               VALUE 'B'.
00101
00102      12  AM-COMM-STRUCTURE.
00103          16  AM-DEFN-1.
00104              20  AM-AGT-COMMS       OCCURS 10 TIMES.
00105                  24  AM-AGT.
00106                      28  AM-AGT-PREFIX     PIC X(4).
00107                      28  AM-AGT-PRIME      PIC X(6).
00108                  24  AM-COM-TYP            PIC X.
00109                  24  AM-L-COM              PIC SV9(5)     COMP-3.
00110                  24  AM-J-COM              PIC SV9(5)     COMP-3.
00111                  24  AM-A-COM              PIC SV9(5)     COMP-3.
00112                  24  AM-RECALC-LV-INDIC    PIC X.
00113                  24  AM-RETRO-LV-INDIC     PIC X.
00114                  24  AM-GL-CODES           PIC X.
00115                  24  AM-COMM-CHARGEBACK    PIC 9(02).
00116                  24  FILLER                PIC X(01).
00117          16  AM-DEFN-2   REDEFINES   AM-DEFN-1.
00118              20  AM-COM-TBLS        OCCURS 10 TIMES.
00119                  24  FILLER                PIC X(11).
00120                  24  AM-L-COMA             PIC XXX.
00121                  24  AM-J-COMA             PIC XXX.
00122                  24  AM-A-COMA             PIC XXX.
00123                  24  FILLER                PIC X(6).
00124
00125      12  AM-COMM-CHANGE-STATUS             PIC X.
00126          88  AM-COMMISSIONS-CHANGED           VALUE '*'.
00127
00128      12  AM-CSR-CODE                       PIC X(4).
00129
00130      12  AM-BILLING-STATUS                 PIC X.
00131          88  AM-ACCOUNT-BILLED                VALUE 'B'.
00132          88  AM-ACCOUNT-NOT-BILLED            VALUE ' '.
00133      12  AM-AUTO-REFUND-SW                 PIC X.
00134          88  AUTO-REFUNDS-USED                VALUE 'Y'.
00135          88  AUTO-REFUNDS-NOT-USED            VALUE 'N' ' '.
00136      12  AM-GPCD                           PIC 99.
00137      12  AM-IG                             PIC X.
00138          88  AM-HAS-INDIVIDUAL                VALUE '1'.
00139          88  AM-HAS-GROUP                     VALUE '2'.
00140      12  AM-STATUS                         PIC X.
00141          88  AM-ACCOUNT-ACTIVE                VALUE '0'.
00142          88  AM-ACCOUNT-INACTIVE              VALUE '1'.
00143          88  AM-ACCOUNT-TRANSFERRED           VALUE '2'.
102004         88  AM-ACCOUNT-CANCELLED             VALUE '3'.
022808         88  AM-ACCOUNT-FROZEN                VALUE '4'.
00144      12  AM-REMIT-TO                       PIC 99.
00145      12  AM-ID-NO                          PIC X(11).
00146
00147      12  AM-CAL-TABLE                      PIC XX.
00148      12  AM-LF-DEVIATION                   PIC XXX.
00149      12  AM-AH-DEVIATION                   PIC XXX.
00150      12  AM-LF-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00151      12  AM-AH-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00152      12  AM-LF-OB-RATE                     PIC S99V9(5)   COMP-3.
00153      12  AM-AH-OB-RATE                     PIC S99V9(5)   COMP-3.
00154      12  AM-LF-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00155      12  AM-AH-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00156
00157      12  AM-USER-FIELDS.
00158          16  AM-FLD-1                      PIC XX.
00159          16  AM-FLD-2                      PIC XX.
00160          16  AM-FLD-3                      PIC XX.
00161          16  AM-FLD-4                      PIC XX.
00162          16  AM-FLD-5                      PIC XX.
00163
00164      12  AM-1ST-PROD-DATE.
00165          16  AM-1ST-PROD-YR                PIC XX.
00166          16  AM-1ST-PROD-MO                PIC XX.
00167          16  AM-1ST-PROD-DA                PIC XX.
00168      12  AM-ANNIVERSARY-DATE               PIC 9(11)  COMP-3.
00169      12  AM-CERTS-PURGED-DATE.
00170          16  AM-PUR-YR                     PIC XX.
00171          16  AM-PUR-MO                     PIC XX.
00172          16  AM-PUR-DA                     PIC XX.
00173      12  AM-HI-CERT-DATE                   PIC 9(11)  COMP-3.
00174      12  AM-LO-CERT-DATE                   PIC 9(11)  COMP-3.
00175      12  AM-ENTRY-DATE                     PIC 9(11)  COMP-3.
00176      12  AM-INACTIVE-DATE.
00177          16  AM-INA-MO                     PIC 99.
00178          16  AM-INA-DA                     PIC 99.
00179          16  AM-INA-YR                     PIC 99.
00180      12  AM-AR-HI-CERT-DATE                PIC XX.
00181
00182      12  AM-LF-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00183      12  AM-AH-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00184
00185      12  AM-OB-PAYMENT-MODE                PIC X.
00186          88  AM-OB-PAID-MONTHLY               VALUE 'M' ' '.
00187          88  AM-OB-PAID-QUARTERLY             VALUE 'Q'.
00188          88  AM-OB-PAID-SEMI-ANNUALLY         VALUE 'S'.
00189          88  AM-OB-PAID-ANNUALLY              VALUE 'A'.
00190
00191      12  AM-AH-ONLY-INDICATOR              PIC X.
00192          88  AM-AH-ONLY-ALLOWED               VALUE 'Y' ' '.
00193          88  AM-NO-AH-ONLY                    VALUE 'N'.
00194
00195      12  AM-EDIT-LOAN-OFC                  PIC X(01).
00196
00197      12  AM-OVER-SHORT.
00198          16 AM-OVR-SHT-AMT                 PIC S999V99    COMP-3.
00199          16 AM-OVR-SHT-PCT                 PIC S9V9(4)    COMP-3.
00200
011410     12  AM-DCC-PRODUCT-CODE               PIC XXX.
041910     12  AM-DCC-CLP-STATE                  PIC XX.
00202
00203      12  AM-RECALC-COMM                    PIC X.
00204      12  AM-RECALC-REIN                    PIC X.
00205
00206      12  AM-REI-TABLE                      PIC XXX.
00207      12  AM-REI-ET-LF                      PIC X.
00208      12  AM-REI-ET-AH                      PIC X.
00209      12  AM-REI-PE-LF                      PIC X.
00210      12  AM-REI-PE-AH                      PIC X.
00211      12  AM-REI-PRT-ST                     PIC X.
00212      12  AM-REI-FEE-LF                     PIC S9V9999    COMP-3.
00213      12  AM-REI-FEE-AH                     PIC S9V9999    COMP-3.
00214      12  AM-REI-LF-TAX                     PIC S9V9999    COMP-3.
00215      12  AM-REI-GROUP-A                    PIC X(6).
00216      12  AM-REI-MORT                       PIC X(4).
00217      12  AM-REI-PRT-OW                     PIC X.
00218      12  AM-REI-PR-PCT                     PIC S9V9999    COMP-3.
00219      12  AM-REI-78-PCT                     PIC S9V9999    COMP-3.
00220      12  AM-REI-AH-TAX                     PIC S9V9999    COMP-3.
00221      12  AM-REI-GROUP-B                    PIC X(6).
00222
00223      12  AM-TRUST-TYPE                     PIC X(2).
00224
00225      12  AM-EMPLOYER-STMT-USED             PIC X.
00226      12  AM-GROUPED-CHECKS-Y-N             PIC X.
00227
00228      12  AM-STD-AH-TYPE                    PIC XX.
00229      12  AM-EARN-METHODS.
00230          16  AM-EARN-METHOD-R              PIC X.
00231              88 AM-REF-RL-R78                 VALUE 'R'.
00232              88 AM-REF-RL-PR                  VALUE 'P'.
00233              88 AM-REF-RL-MEAN                VALUE 'M'.
00234              88 AM-REF-RL-ANTICIPATION        VALUE 'A'.
00235          16  AM-EARN-METHOD-L              PIC X.
00236              88 AM-REF-LL-R78                 VALUE 'R'.
00237              88 AM-REF-LL-PR                  VALUE 'P'.
00238              88 AM-REF-LL-MEAN                VALUE 'M'.
00239              88 AM-REF-LL-ANTICIPATION        VALUE 'A'.
00240          16  AM-EARN-METHOD-A              PIC X.
00241              88 AM-REF-AH-R78                 VALUE 'R'.
00242              88 AM-REF-AH-PR                  VALUE 'P'.
00243              88 AM-REF-AH-MEAN                VALUE 'M'.
00244              88 AM-REF-AH-ANTICIPATION        VALUE 'A'.
00245              88 AM-REF-AH-CALIF-SPEC          VALUE 'C'.
00246              88 AM-REF-AH-NET                 VALUE 'N'.
00247
00248      12  AM-TOL-PREM                       PIC S999V99    COMP-3.
00249      12  AM-TOL-REF                        PIC S999V99    COMP-3.
00250      12  AM-TOL-CLM                        PIC S999V99    COMP-3.
00251
00252      12  AM-RET-Y-N                        PIC X.
00253      12  AM-RET-P-E                        PIC X.
00254      12  AM-LF-RET                         PIC S9V9999    COMP-3.
00255      12  AM-AH-RET                         PIC S9V9999    COMP-3.
00256      12  AM-RET-GRP                        PIC X(6).
00257      12  AM-RETRO-POOL  REDEFINES  AM-RET-GRP.
00258          16  AM-POOL-PRIME                 PIC XXX.
00259          16  AM-POOL-SUB                   PIC XXX.
00260      12  AM-RETRO-EARNINGS.
00261          16  AM-RET-EARN-R                 PIC X.
00262          16  AM-RET-EARN-L                 PIC X.
00263          16  AM-RET-EARN-A                 PIC X.
00264      12  AM-RET-ST-TAX-USE                 PIC X.
00265          88  CHARGE-ST-TAXES-ON-RETRO         VALUE 'Y' 'E' 'P'.
00266          88  TAXES-NOT-IN-RETRO               VALUE 'N' ' '.
00267      12  AM-RETRO-BEG-EARNINGS.
00268          16  AM-RET-BEG-EARN-R             PIC X.
00269          16  AM-RET-BEG-EARN-L             PIC X.
00270          16  AM-RET-BEG-EARN-A             PIC X.
00271      12  AM-RET-MIN-LOSS-L                 PIC SV999      COMP-3.
00272      12  AM-RET-MIN-LOSS-A                 PIC SV999      COMP-3.
00273
00274      12  AM-USER-SELECT-OPTIONS.
00275          16  AM-USER-SELECT-1              PIC X(10).
00276          16  AM-USER-SELECT-2              PIC X(10).
00277          16  AM-USER-SELECT-3              PIC X(10).
00278          16  AM-USER-SELECT-4              PIC X(10).
00279          16  AM-USER-SELECT-5              PIC X(10).
00280
00281      12  AM-LF-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00282
00283      12  AM-AH-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00284
00285      12  AM-RPT045A-SWITCH                 PIC X.
00286          88  RPT045A-OFF                   VALUE 'N'.
00287
00288      12  AM-INSURANCE-LIMITS.
00289          16  AM-MAX-MON-BEN                PIC S9(7)      COMP-3.
00290          16  AM-MAX-TOT-BEN                PIC S9(7)      COMP-3.
00291
00292      12  AM-PROFILE-CHANGE-SWITCH          PIC X.
00293          88  AM-PROFILE-DATA-CHANGED          VALUE '*'.
00294
00295      12  AM-DISMBR-COVERAGE-SW             PIC X.
00296          88  AM-DISMBR-COVERAGE               VALUE 'Y'.
00297          88  AM-NO-DISMBR-COVERAGE            VALUE 'N'.
00298
00299      12  AM-CANCEL-FEE                     PIC S9(3)V9(2) COMP-3.
00300
00301      12  AM-TOL-REF-PCT                    PIC S9V9(4)    COMP-3.
090803     12  AM-CLP-TOL-PCT                    PIC S9V9(4)    COMP-3.
092705     12  AM-SPP-LEASE-COMM                 PIC S9(5)V99   COMP-3.
           12  AM-DCC-MAX-MARKETING-FEE          PIC S9(5)      COMP-3.
090803     12  FILLER                            PIC X(5).
120406     12  AM-REPORT-CODE-3                  PIC X(10).
090803*    12  FILLER                            PIC X(22).
00303
00304      12  AM-RESERVE-DATE.
00305          16  AM-TARGET-LOSS-RATIO          PIC S9V9(4) COMP-3.
00306          16  AM-LIFE-IBNR-PCT              PIC S9V9(4) COMP-3.
00307          16  AM-CRDT-MODIFICATION-PCT      PIC S9V9(4) COMP-3.
00308
00309      12  AM-3RD-PARTY-NOTIF-LEVEL          PIC 99.
00310      12  AM-NOTIFICATION-TYPES.
00311          16  AM-NOTIF-OF-LETTERS           PIC X.
00312          16  AM-NOTIF-OF-PAYMENTS          PIC X.
00313          16  AM-NOTIF-OF-REPORTS           PIC X.
00314          16  AM-NOTIF-OF-STATUS            PIC X.
00315
00316      12  AM-BENEFIT-TABLE-USAGE            PIC X.
00317          88  AM-BENEFIT-TABLE-USED            VALUE 'Y'.
00318          88  AM-USE-DEVIATIONS-ONLY           VALUE 'D'.
00319          88  AM-EDIT-BENEFITS-ONLY            VALUE 'E'.
00320          88  AM-EDITS-NOT-USED                VALUE ' '  'N'.
00321
00322      12  AM-BENEFIT-CONTROLS.
00323          16  AM-ALLOWABLE-BENEFITS  OCCURS  20  TIMES.
00324              20  AM-BENEFIT-CODE           PIC XX.
00325              20  AM-BENEFIT-TYPE           PIC X.
00326              20  AM-BENEFIT-REVISION       PIC XXX.
00327              20  AM-BENEFIT-REM-TERM       PIC X.
00328              20  AM-BENEFIT-RETRO-Y-N      PIC X.
00329              20  FILLER                    PIC XX.
00330          16  FILLER                        PIC X(80).
00331
00332      12  AM-TRANSFER-DATA.
00333          16  AM-TRANSFERRED-FROM.
00334              20  AM-TRNFROM-CARRIER        PIC X.
00335              20  AM-TRNFROM-GROUPING.
00336                  24  AM-TRNFROM-GRP-PREFIX PIC XXX.
00337                  24  AM-TRNFROM-GRP-PRIME  PIC XXX.
00338              20  AM-TRNFROM-STATE          PIC XX.
00339              20  AM-TRNFROM-ACCOUNT.
00340                  24  AM-TRNFROM-ACCT-PREFIX PIC X(4).
00341                  24  AM-TRNFROM-ACCT-PRIME PIC X(6).
00342              20  AM-TRNFROM-DTE            PIC XX.
00343          16  AM-TRANSFERRED-TO.
00344              20  AM-TRNTO-CARRIER          PIC X.
00345              20  AM-TRNTO-GROUPING.
00346                  24  AM-TRNTO-GRP-PREFIX   PIC XXX.
00347                  24  AM-TRNTO-GRP-PRIME    PIC XXX.
00348              20  AM-TRNTO-STATE            PIC XX.
00349              20  AM-TRNTO-ACCOUNT.
00350                  24  AM-TRNTO-ACCT-PREFIX  PIC X(4).
00351                  24  AM-TRNTO-ACCT-PRIME   PIC X(6).
00352              20  AM-TRNTO-DTE              PIC XX.
00353          16  FILLER                        PIC X(10).
00354
00355      12  AM-SAVED-REMIT-TO                 PIC 99.
00356
00357      12  AM-COMM-STRUCTURE-SAVED.
00358          16  AM-DEFN-1-SAVED.
00359              20  AM-AGT-COMMS-SAVED    OCCURS 10 TIMES.
00360                  24  AM-AGT-SV             PIC X(10).
00361                  24  AM-COM-TYP-SV         PIC X.
00362                  24  AM-L-COM-SV           PIC SV9(5)     COMP-3.
00363                  24  AM-J-COM-SV           PIC SV9(5)     COMP-3.
00364                  24  AM-A-COM-SV           PIC SV9(5)     COMP-3.
00365                  24  AM-RECALC-LV-INDIC-SV PIC X.
00366                  24  FILLER                PIC X.
00367                  24  AM-GL-CODES-SV        PIC X.
00368                  24  AM-COM-CHARGEBACK-SV  PIC 99.
00369                  24  FILLER                PIC X.
00370          16  AM-DEFN-2-SAVED   REDEFINES   AM-DEFN-1-SAVED.
00371              20  AM-COM-TBLS-SAVED    OCCURS 10 TIMES.
00372                  24  FILLER                PIC X(11).
00373                  24  AM-L-COMA-SV          PIC XXX.
00374                  24  AM-J-COMA-SV          PIC XXX.
00375                  24  AM-A-COMA-SV          PIC XXX.
00376                  24  FILLER                PIC X(6).
00377
00378      12  AM-FLC-NET-PREMIUM-ALLOWANCE.
00379          16 AM-ACCOUNT-ALLOWANCE OCCURS  5 TIMES.
00380             20  AM-ALLOW-BEGIN-RANGE       PIC S9(5)      COMP-3.
00381             20  AM-ALLOW-END-RANGE         PIC S9(5)      COMP-3.
00382             20  AM-ALLOWANCE-AMT           PIC S9(5)V99   COMP-3.
00383
122806     12  AM-ORIG-DEALER-NO                 PIC X(10).
122806     12  FILLER                            PIC X(120).
00385
00386      12  AM-ACCOUNT-EXECUTIVE-DATA.
00387          16  AM-CONTROL-NAME               PIC X(30).
00388          16  AM-EXECUTIVE-ONE.
00389              20  AM-EXEC1-NAME             PIC X(15).
00390              20  AM-EXEC1-DIS-PERCENT      PIC S9(01)V9(04)
00391                                                           COMP-3.
00392              20  AM-EXEC1-LIFE-PERCENT     PIC S9(01)V9(04)
00393                                                           COMP-3.
00394          16  AM-EXECUTIVE-TWO.
00395              20  AM-EXEC2-NAME             PIC X(15).
00396              20  AM-EXEC2-DIS-PERCENT      PIC S9(01)V9(04)
00397                                                           COMP-3.
00398              20  AM-EXEC2-LIFE-PERCENT     PIC S9(01)V9(04)
00399                                                           COMP-3.
00400
00401      12  AM-RETRO-ADDITIONAL-DATA.
00402          16  AM-RETRO-QUALIFY-LIMIT        PIC S9(7)      COMP-3.
00403          16  AM-RETRO-PREM-P-E             PIC X.
00404          16  AM-RETRO-CLMS-P-I             PIC X.
00405          16  AM-RETRO-RET-BRACKET-LF.
00406              20  AM-RETRO-RET-METHOD-LF    PIC X.
00407                  88  AM-RETRO-USE-PCT-LF      VALUE 'P' ' '.
00408                  88  AM-RETRO-USE-SCALE-LF    VALUE 'S'.
00409              20  AM-RETRO-RET-BASIS-LF     PIC X.
00410                  88  AM-RETRO-EARN-BASIS-LF   VALUE 'E' ' '.
00411                  88  AM-RETRO-PAID-BASIS-LF   VALUE 'P'.
00412              20  AM-RETRO-BRACKETS-LF  OCCURS  3 TIMES.
00413                  24  AM-RETRO-RET-PCT-LF   PIC S9V9999    COMP-3.
00414                  24  AM-RETRO-RET-THRU-LF  PIC S9(7)      COMP-3.
00415          16  AM-RETRO-RET-BRACKET-AH.
00416              20  AM-RETRO-RET-METHOD-AH    PIC X.
00417                  88  AM-RETRO-USE-PCT-AH      VALUE 'P' ' '.
00418                  88  AM-RETRO-USE-SCALE-AH    VALUE 'S'.
00419                  88  AM-RETRO-USE-LIFE-METHOD VALUE 'L'.
00420              20  AM-RETRO-RET-BASIS-AH     PIC X.
00421                  88  AM-RETRO-EARN-BASIS-AH   VALUE 'E' ' '.
00422                  88  AM-RETRO-PAID-BASIS-AH   VALUE 'P'.
00423              20  AM-RETRO-BRACKETS-AH  OCCURS  3 TIMES.
00424                  24  AM-RETRO-RET-PCT-AH   PIC S9V9999    COMP-3.
00425                  24  AM-RETRO-RET-THRU-AH  PIC S9(7)      COMP-3.
00426
00427      12  AM-COMMENTS.
00428          16  AM-COMMENT-LINE           PIC X(50)   OCCURS 5 TIMES.
00429
00430      12  AM-CLIENT-OVERLAY-FLI   REDEFINES   AM-COMMENTS.
00431          16  AM-FLI-RETRO-SHARE-CODE       PIC X.
00432          16  AM-FLI-BILLING-CODE           PIC X.
00433          16  AM-FLI-ALT-STATE-CODE         PIC XX.
00434          16  AM-FLI-UNITED-IDENT           PIC X.
00435          16  AM-FLI-INTEREST-LOST-DATA.
00436              20  AM-FLI-BANK-NO            PIC X(5).
00437              20  AM-FLI-BANK-BALANCE       PIC S9(9)V99   COMP-3.
00438              20  AM-FLI-BANK-1ST-6-PREM    PIC S9(9)V99   COMP-3.
00439              20  AM-FLI-BANK-CAP-AMT       PIC S9(9)V99   COMP-3.
00440          16  AM-FLI-ALT-AGENT-CODES   OCCURS 10 TIMES.
00441              20  AM-FLI-AGT                PIC X(9).
00442              20  AM-FLI-AGT-COMM-ACC       PIC X.
00443              20  AM-FLI-AGT-SHARE-PCT      PIC S9V99      COMP-3.
00444          16  FILLER                        PIC X(102).
00445
00446      12  AM-CLIENT-OVERLAY-DMD   REDEFINES   AM-COMMENTS.
00447          16  AM-ALLOWABLE-DMD-BENEFITS  OCCURS 30 TIMES.
00448              20  AM-BENEFIT-DMD-CODE         PIC XX.
00449              20  AM-BENEFIT-DMD-TYPE         PIC X.
00450              20  AM-BENEFIT-DMD-REVISION     PIC XXX.
00451              20  AM-BENEFIT-DMD-REM-TERM     PIC X.
00452              20  AM-BENEFIT-DMD-RETRO-Y-N    PIC X.
00453          16  FILLER                          PIC X(10).
00454 ******************************************************************
00658      EJECT
00659
00660
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA PARMLIST
                                PENDING-BUSINESS CONTROL-FILE
                                CERTIFICATE-MASTER
                                PENDING-MAILING-DATA ACCOUNT-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL9301' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00662
00663      SERVICE RELOAD PARMLIST.
00664
00665      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00666      MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.
00667
00668      MOVE +2                     TO EMI-NUMBER-OF-LINES.
00669
00670      IF EIBCALEN = 0
00671          GO TO 8800-UNAUTHORIZED-ACCESS.
00672
00673      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00674      MOVE '5'                    TO DC-OPTION-CODE.
00675      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
00676      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.
00677      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.
00678
00679      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00680          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00681              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00682              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00683              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00684              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00685              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00686              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00687              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00688              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00689          ELSE
00690              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00691              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00692              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00693              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00694              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00695              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00696              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00697              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00698
00699      MOVE LOW-VALUES             TO EL930BI.
00700
00701      IF EIBTRNID NOT = TRANS-EXI2
00702          MOVE ZEROS              TO PI-LF-ISS-ENTERED
00703                                     PI-LF-CAN-ENTERED
00704                                     PI-AH-ISS-ENTERED
00705                                     PI-AH-CAN-ENTERED
00706                                     PI-ISS-CNT-ENTERED
00707                                     PI-CAN-CNT-ENTERED
00708          IF PI-MAINT-FUNC = 'N'
00709             MOVE +0              TO PI-LAST-SEQ-NO-ADDED
00710             MOVE +1              TO PI-NEXT-DISPLAY-SEQ-NO
00711             IF PI-MAP-NAME = EL930B
00712                PERFORM 8550-SET-MAP-SEQ-NOS
00713                GO TO 8100-SEND-INITIAL-MAP
00714             ELSE
00715                PERFORM 8550-SET-MAP-SEQ-NOS
00716                        VARYING WS-SUB2 FROM 1 BY 1
00717                        UNTIL WS-SUB2   GREATER THAN +2
00718                  GO TO 8100-SEND-INITIAL-MAP
00719          ELSE
00720              GO TO 3000-CONTINUE-ENTRY.
00721
00722      
      * EXEC CICS HANDLE CONDITION
00723 *        PGMIDERR  (9600-PGMID-ERROR)
00724 *        ERROR     (9990-ABEND)
00725 *        END-EXEC.
      *    MOVE '"$L.                  ! " #00005846' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303035383436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00726
00727      IF EIBAID = DFHCLEAR
00728          GO TO 9400-CLEAR.
00729
00730      EJECT
00731
00732 ******************************************************************
00733 *                                                                *
00734 *                R E C E I V E   M A P S                         *
00735 *                                                                *
00736 ******************************************************************
00737
00738  0200-RECEIVE.
00739
00740      IF EIBAID = DFHPA1 OR
00741                  DFHPA2 OR
00742                  DFHPA3
00743          MOVE ER-0008            TO EMI-ERROR
00744          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00745          IF PI-MAP-NAME = EL930B
00746              MOVE -1             TO BPFENTRL
00747              GO TO 8200-SEND-DATAONLY
00748          ELSE
00749              MOVE -1             TO CPFENTRL
00750              GO TO 8200-SEND-DATAONLY.
00751
00752      
      * EXEC CICS RECEIVE
00753 *        MAP      (PI-MAP-NAME)
00754 *        MAPSET   (MAPSET-EL9301S)
00755 *        INTO     (DATA-ENTRY-MAP)
00756 *        END-EXEC.
           MOVE LENGTH OF
            DATA-ENTRY-MAP
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00005876' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 DATA-ENTRY-MAP, 
                 DFHEIV11, 
                 MAPSET-EL9301S, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00757
uktdel*    TRANSFORM DATA-ENTRY-MAP FROM '_' TO ' '.
uktins     INSPECT DATA-ENTRY-MAP REPLACING ALL '_' BY ' '.
00759
00760      IF PI-MAP-NAME = EL930B
00761          IF BPFENTRL GREATER THAN ZERO
00762              IF EIBAID NOT = DFHENTER
00763                  MOVE ER-0004    TO EMI-ERROR
00764                  MOVE AL-UNBOF   TO BPFENTRA
00765                  MOVE -1         TO BPFENTRL
00766                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00767                  GO TO 8200-SEND-DATAONLY
00768              ELSE
00769                  IF BPFENTRI NUMERIC
00770                    AND BPFENTRI GREATER THAN 0
00771                    AND BPFENTRI LESS THAN 23
00772                      MOVE PF-VALUES (BPFENTRI) TO EIBAID
00773                  ELSE
00774                      MOVE ER-0029  TO EMI-ERROR
00775                      MOVE AL-UNBOF TO BPFENTRA
00776                      MOVE -1       TO BPFENTRL
00777                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00778                      GO TO 8200-SEND-DATAONLY
00779          ELSE
00780              NEXT SENTENCE
00781
00782      ELSE
00783
00784      IF PI-MAP-NAME = EL930C
00785          IF CPFENTRL GREATER THAN ZERO
00786              IF EIBAID NOT = DFHENTER
00787                  MOVE ER-0004    TO EMI-ERROR
00788                  MOVE AL-UNBOF   TO CPFENTRA
00789                  MOVE -1         TO CPFENTRL
00790                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00791                  GO TO 8200-SEND-DATAONLY
00792              ELSE
00793                  IF CPFENTRI NUMERIC
00794                    AND CPFENTRI GREATER THAN 0
00795                    AND CPFENTRI LESS THAN 23
00796                      MOVE PF-VALUES (CPFENTRI) TO EIBAID
00797                  ELSE
00798                      MOVE ER-0029  TO EMI-ERROR
00799                      MOVE AL-UNBOF TO BPFENTRA
00800                      MOVE -1       TO BPFENTRL
00801                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00802                      GO TO 8200-SEND-DATAONLY.
00803
00804
00805      EJECT
00806 ******************************************************************
00807 *                                                                *
00808 *                  C H E C K   PF   K E Y S                      *
00809 *                                                                *
00810 ******************************************************************
00811
00812
00813 ******************************************************************
00814 *                                                                *
00815 *   PF KEY FUNCTIONS:                                            *
00816 *                                                                *
00817 *   PF1 = BROWSE FOWARD                                          *
00818 *   PF2 = BROWSE BACKWARD                                        *
00819 *   PF3 = ADD ISSUE RECORD                                       *
00820 *   PF4 = ADD CANCEL RECORD                                      *
00821 *   PF5 = RESET TABS (OPEN PRTECTED FIELDS)                      *
00822 *   PF6 = DELETE ENTRY                                           *
00823 *                                                                *
00824 ******************************************************************
00825
00826  0300-CHECK-PFKEYS.
00827
00828      IF EIBAID = DFHPF12
00829          GO TO 9500-PF12.
00830
00831      IF EIBAID = DFHENTER
00832          GO TO 1000-EDIT-MAPB.
00833
00834      IF EIBAID = DFHPF1
00835          GO TO 2000-BROWSE-FWD.
00836
00837      IF EIBAID = DFHPF2
00838          GO TO 2100-BROWSE-BKWD.
00839
00840      IF EIBAID = DFHPF3
00841          MOVE SPACE              TO PI-DISPLAY-SW
00842          MOVE LOW-VALUES         TO DATA-ENTRY-MAP
00843          ADD +1, PI-LAST-SEQ-NO-ADDED
00844                GIVING PI-NEXT-DISPLAY-SEQ-NO
00845          MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ
00846          MOVE EL930B             TO PI-MAP-NAME
00847          PERFORM 8550-SET-MAP-SEQ-NOS
00848          GO TO 8100-SEND-INITIAL-MAP.
00849
00850      IF EIBAID = DFHPF4
00851          MOVE SPACE              TO PI-DISPLAY-SW
00852          MOVE LOW-VALUES         TO MAP-C
00853          ADD +1, PI-LAST-SEQ-NO-ADDED
00854                GIVING PI-NEXT-DISPLAY-SEQ-NO
00855          MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ
00856          MOVE EL930C             TO PI-MAP-NAME
00857          PERFORM 8550-SET-MAP-SEQ-NOS
00858                 VARYING WS-SUB2 FROM 1 BY 1
00859                 UNTIL WS-SUB2 GREATER THAN +2
00860          GO TO 8100-SEND-INITIAL-MAP.
00861
00862      IF EIBAID = DFHPF5
00863         IF PI-MAP-NAME = EL930B
00864            PERFORM 0610-UNPROTECT-FIELDS THRU 0610-EXIT
00865            ADD +1, PI-LAST-SEQ-NO-ADDED
00866                   GIVING PI-NEXT-DISPLAY-SEQ-NO
00867            PERFORM 8550-SET-MAP-SEQ-NOS
00868            MOVE -1             TO BCERTL
00869            GO TO 8200-SEND-DATAONLY
00870         ELSE
00871            PERFORM 0710-UNPROTECT-FIELDS THRU 0710-EXIT
00872            ADD +1, PI-LAST-SEQ-NO-ADDED
00873                   GIVING PI-NEXT-DISPLAY-SEQ-NO
00874            PERFORM 8550-SET-MAP-SEQ-NOS
00875                    VARYING WS-SUB2 FROM +1 BY +1
00876                    UNTIL WS-SUB2 GREATER THAN +2
00877           MOVE -1             TO CCERT-LEN  (1)
00878           GO TO 8200-SEND-DATAONLY.
00879
00880      IF EIBAID = DFHPF6
00881          IF PI-LAST-FUNC-DISPLAY
00882              GO TO 6000-DELETE-PEND-BUS-RECORD
00883          ELSE
00884              MOVE ER-2594        TO EMI-ERROR
00885              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00886              IF PI-MAP-NAME = EL930B
00887                  MOVE -1         TO BPFENTRL
00888                  GO TO 8200-SEND-DATAONLY
00889              ELSE
00890                  MOVE -1         TO CPFENTRL
00891                  GO TO 8200-SEND-DATAONLY.
00892
00893      MOVE ER-0008 TO EMI-ERROR.
00894      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00895
00896      IF PI-MAP-NAME = EL930B
00897          MOVE -1                 TO BPFENTRL
00898      ELSE
00899          MOVE -1                 TO CPFENTRL.
00900
00901      GO TO 8200-SEND-DATAONLY.
00902
00903      EJECT
00904
00905
00906 ******************************************************************
00907 *                                                                *
00908 *         P R O T E C T   I S S U E   F I E L D S                *
00909 *                                                                *
00910 ******************************************************************
00911
00912  0600-PROTECT-FIELDS.
00913
00914      IF NOT PI-ISS-SUFFIX-KEYED
00915          MOVE AL-SANOF           TO BSFX-ATTRB.
00916
00917      IF NOT PI-IG-KEYED
00918          MOVE AL-SANOF           TO BIND-GRP-ATTRB.
00919
00920      IF NOT PI-1ST-PMT-KEYED
00921          MOVE AL-SANOF           TO B1ST-PMT-ATTRB.
00922
00923      IF NOT PI-DAYS-KEYED
00924          MOVE AL-SANOF           TO BDAYS-ATTRB.
00925
00926      IF NOT PI-APR-KEYED
00927          MOVE AL-SANOF           TO BAPR-ATTRB.
00928
00929      IF NOT PI-FREQ-KEYED
00930          MOVE AL-SANOF           TO BFREQ-ATTRB.
00931
00932      IF NOT PI-SIG-KEYED
00933          MOVE AL-SANOF           TO BSIG-ATTRB.
00934
00935      IF NOT PI-ISS-LIVES-KEYED
00936          MOVE AL-SANOF           TO BLIVES-ATTRB.
00937
00938      IF NOT PI-SSNUM-KEYED
00939          MOVE AL-SANOF           TO BSSNUM-ATTRB.
00940
00941      IF NOT PI-MEMBER-KEYED
00942          MOVE AL-SANOF           TO BMEM-NO-ATTRB.
00943
00944      IF NOT PI-MODE-KEYED
00945          MOVE AL-SANOF           TO BMODE-ATTRB.
00946
00947      IF NOT PI-PMTS-KEYED
00948          MOVE AL-SANOF           TO BPMTS-ATTRB.
00949
00950      IF NOT PI-LN-OFFICER-KEYED
00951          MOVE AL-SANOF           TO BLN-OFFICER-ATTRB.
00952
00953      IF NOT PI-LNTRM-KEYED
00954          MOVE AL-SANOF           TO BLN-TERM-ATTRB.
00955
00956      IF NOT PI-ENTRY-KEYED
00957          MOVE AL-SANOF           TO BENTRY-ATTRB.
00958
00959 *    IF NOT PI-ZIP4-KEYED
00960 *        MOVE AL-SANOF           TO BZIP4-ATTRB.
00961
00962      IF NOT PI-POLICY-KEYED
00963          MOVE AL-SANOF           TO BPOLICY-ATTRB.
00964
00965      IF NOT PI-RINCD-KEYED
00966          MOVE AL-SANOF           TO BRINCD-ATTRB.
00967
00968      IF NOT PI-RTCLS-KEYED
00969          MOVE AL-SANOF           TO BRTCLS-ATTRB.
00970
00971
00972      IF NOT PI-EXPIRE-KEYED
00973          MOVE AL-SANOF           TO BEXPIRE-ATTRB (1)
00974                                     BEXPIRE-ATTRB (2).
00975
00976      IF NOT PI-CRIT-PERD-KEYED
00977          MOVE AL-SANOF           TO BCRIT-PERD-ATTRB (1)
00978                                     BCRIT-PERD-ATTRB (2).
00979
00980      IF NOT PI-PMT-KEYED
00981          MOVE AL-SANOF           TO BPMT-ATTRB.
00982
00983      IF NOT PI-SKPCD-KEYED
00984          MOVE AL-SANOF           TO BSKPCD-ATTRB.
00985
00986      IF PI-BIRTH-DATE-IS-INPUT
00987          MOVE AL-SANOF           TO BAGE-ATTRB
00988      ELSE
00989          MOVE AL-SANOF           TO BBIRTH-ATTRB.
00990
00991      IF NOT PI-JNT-AGE-KEYED
00992          MOVE AL-SANOF           TO BJNT-AGE-ATTRB.
00993
00994      IF NOT PI-JNT-NAME-KEYED
00995          MOVE AL-SANOF           TO BJNT-INIT-ATTRB
00996                                     BJNT-LST-NAME-ATTRB
00997                                     BJNT-1ST-NAME-ATTRB.
00998
00999      IF NOT PI-BENEFICIARY-KEYED
01000          MOVE AL-SANOF           TO BBENEFICIARY-ATTRB.
01001
01002      IF NOT PI-PHONE-KEYED
01003          MOVE AL-SANOF           TO BPHONE-ATTRB.
01004
01005      IF NOT PI-ALT-BEN-KEYED
01006          MOVE AL-SANOF           TO BALT-BEN-ATTRB (1).
01007
01008      IF NOT PI-ALT-PREM-KEYED
01009          MOVE AL-SANOF           TO BALT-PREM-ATTRB (1).
01010
01011  0600-EXIT.
01012      EXIT.
01013
01014      EJECT
01015
01016 ******************************************************************
01017 *                                                                *
01018 *         U N P R O T E C T   I S S U E   F I E L D S            *
01019 *                                                                *
01020 ******************************************************************
01021
01022  0610-UNPROTECT-FIELDS.
01023
01024      IF NOT PI-ISS-SUFFIX-KEYED
01025          MOVE AL-UANOF           TO BSFX-ATTRB.
01026
01027      IF NOT PI-IG-KEYED
01028          MOVE AL-UANOF           TO BIND-GRP-ATTRB.
01029
01030      IF NOT PI-1ST-PMT-KEYED
01031          MOVE AL-UANOF           TO B1ST-PMT-ATTRB.
01032
01033      IF NOT PI-DAYS-KEYED
01034          MOVE AL-UANOF           TO BDAYS-ATTRB.
01035
01036      IF NOT PI-APR-KEYED
01037          MOVE AL-UANOF           TO BAPR-ATTRB.
01038
01039      IF NOT PI-FREQ-KEYED
01040          MOVE AL-UANOF           TO BFREQ-ATTRB.
01041
01042      IF NOT PI-SIG-KEYED
01043          MOVE AL-UANOF           TO BSIG-ATTRB.
01044
01045      IF NOT PI-ISS-LIVES-KEYED
01046          MOVE AL-UANOF           TO BLIVES-ATTRB.
01047
01048      IF NOT PI-SSNUM-KEYED
01049          MOVE AL-UANOF           TO BSSNUM-ATTRB.
01050
01051      IF NOT PI-MEMBER-KEYED
01052          MOVE AL-UANOF           TO BMEM-NO-ATTRB.
01053
01054      IF NOT PI-MODE-KEYED
01055          MOVE AL-UANOF           TO BMODE-ATTRB.
01056
01057      IF NOT PI-PMTS-KEYED
01058          MOVE AL-UANOF           TO BPMTS-ATTRB.
01059
01060      IF NOT PI-LN-OFFICER-KEYED
01061          MOVE AL-UANOF           TO BLN-OFFICER-ATTRB.
01062
01063      IF NOT PI-ENTRY-KEYED
01064          MOVE AL-UANOF           TO BENTRY-ATTRB.
01065
01066 *    IF NOT PI-ZIP4-KEYED
01067 *        MOVE AL-UANOF           TO BZIP4-ATTRB.
01068
01069      IF NOT PI-POLICY-KEYED
01070          MOVE AL-UANOF           TO BPOLICY-ATTRB.
01071
01072      IF NOT PI-RINCD-KEYED
01073          MOVE AL-UANOF           TO BRINCD-ATTRB.
01074
01075      IF NOT PI-RTCLS-KEYED
01076          MOVE AL-UANOF           TO BRTCLS-ATTRB.
01077
01078
01079      IF NOT PI-LNTRM-KEYED
01080          MOVE AL-UANOF           TO BLN-TERM-ATTRB.
01081
01082
01083      IF NOT PI-EXPIRE-KEYED
01084          MOVE AL-UANOF           TO BEXPIRE-ATTRB (1)
01085                                     BEXPIRE-ATTRB (2).
01086
01087      IF NOT PI-CRIT-PERD-KEYED
01088          MOVE AL-UANOF           TO BCRIT-PERD-ATTRB (1)
01089          MOVE AL-UANOF           TO BCRIT-PERD-ATTRB (2).
01090
01091      IF NOT PI-PMT-KEYED
01092          MOVE AL-UANOF           TO BPMT-ATTRB.
01093
01094      IF NOT PI-SKPCD-KEYED
01095          MOVE AL-UANOF           TO BSKPCD-ATTRB.
01096
01097      IF PI-BIRTH-DATE-IS-INPUT
01098          MOVE AL-UANOF           TO BAGE-ATTRB
01099      ELSE
01100          MOVE AL-UANOF           TO BBIRTH-ATTRB.
01101
01102      IF NOT PI-JNT-AGE-KEYED
01103          MOVE AL-UANOF           TO BJNT-AGE-ATTRB.
01104
01105      IF NOT PI-JNT-NAME-KEYED
01106          MOVE AL-UANOF           TO BJNT-INIT-ATTRB
01107                                     BJNT-LST-NAME-ATTRB
01108                                     BJNT-1ST-NAME-ATTRB.
01109
01110      IF NOT PI-BENEFICIARY-KEYED
01111          MOVE AL-UANOF           TO BBENEFICIARY-ATTRB.
01112
01113      IF NOT PI-PHONE-KEYED
01114          MOVE AL-UANOF           TO BPHONE-ATTRB.
01115
01116      IF NOT PI-ALT-BEN-KEYED
01117          MOVE AL-UANOF           TO BALT-BEN-ATTRB (1).
01118
01119      IF NOT PI-ALT-PREM-KEYED
01120          MOVE AL-UANOF           TO BALT-PREM-ATTRB (1).
01121
01122  0610-EXIT.
01123
01124      EXIT.
01125
01126      EJECT
01127
01128 ******************************************************************
01129 *                                                                *
01130 *         P R O T E C T   C A N C E L   F I E L D S              *
01131 *                                                                *
01132 ******************************************************************
01133
01134  0700-PROTECT-FIELDS.
01135
01136      IF NOT PI-CAN-SUFFIX-KEYED
01137          MOVE AL-SANOF           TO CSFX-ATTRB (1)
01138                                     CSFX-ATTRB (2)
01139                                     CSFX-ATTRB (3)
01140                                     CSFX-ATTRB (4).
01141
01142      IF NOT PI-CAN-LIVES-KEYED
01143          MOVE AL-SANOF           TO CLIVES-ATTRB (1)
01144                                     CLIVES-ATTRB (2)
01145                                     CLIVES-ATTRB (3)
01146                                     CLIVES-ATTRB (4).
01147      IF NOT PI-PAYEE-KEYED
01148          MOVE AL-SANOF           TO CPAYEE-ATTRB (1)
01149                                     CPAYEE-ATTRB (2)
01150                                     CPAYEE-ATTRB (3)
01151                                     CPAYEE-ATTRB (4).
01152      IF NOT PI-CHK-REQ-KEYED
01153          MOVE AL-SANOF           TO CCHK-ATTRB   (1)
01154                                     CCHK-ATTRB   (2)
01155                                     CCHK-ATTRB   (3)
01156                                     CCHK-ATTRB   (4).
01157  0700-EXIT.
01158      EXIT.
01159
01160      EJECT
01161
01162 ******************************************************************
01163 *                                                                *
01164 *         U N P R O T E C T   C A N C E L   F I E L D S          *
01165 *                                                                *
01166 ******************************************************************
01167
01168  0710-UNPROTECT-FIELDS.
01169
01170      IF NOT PI-CAN-SUFFIX-KEYED
01171          MOVE AL-UANOF           TO CSFX-ATTRB (1)
01172                                     CSFX-ATTRB (2)
01173                                     CSFX-ATTRB (3)
01174                                     CSFX-ATTRB (4).
01175
01176      IF NOT PI-CAN-LIVES-KEYED
01177          MOVE AL-UANOF           TO CLIVES-ATTRB (1)
01178                                     CLIVES-ATTRB (2)
01179                                     CLIVES-ATTRB (3)
01180                                     CLIVES-ATTRB (4).
01181      IF NOT PI-PAYEE-KEYED
01182          MOVE AL-UANOF           TO CPAYEE-ATTRB (1)
01183                                     CPAYEE-ATTRB (2)
01184                                     CPAYEE-ATTRB (3)
01185                                     CPAYEE-ATTRB (4).
01186      IF NOT PI-CHK-REQ-KEYED
01187          MOVE AL-UANOF           TO CCHK-ATTRB   (1)
01188                                     CCHK-ATTRB   (2)
01189                                     CCHK-ATTRB   (3)
01190                                     CCHK-ATTRB   (4).
01191  0710-EXIT.
01192      EXIT.
01193
01194      EJECT
01195
01196 ******************************************************************
01197 *                                                                *
01198 *     E D I T   D A T A   E N T R Y   I S S U E    S C R E EN    *
01199 *                                                                *
01200 ******************************************************************
01201
01202  1000-EDIT-MAPB.
01203
01204      IF PI-MAP-NAME NOT = EL930B
01205          GO TO 1100-EDIT-MAPC.
01206
01207      IF PI-LAST-FUNC-DISPLAY
01208        AND BSFX-LEN           = ZEROS
01209        AND B1ST-NAME-LEN      = ZEROS
01210        AND BLAST-NAME-LEN     = ZEROS
01211        AND BINIT-LEN          = ZEROS
01212        AND BJNT-1ST-NAME-LEN  = ZEROS
01213        AND BJNT-INIT-LEN      = ZEROS
01214        AND BJNT-LST-NAME-LEN  = ZEROS
01215        AND BSEX-LEN           = ZEROS
01216        AND BAGE-LEN           = ZEROS
01217        AND BSSNUM-LEN         = ZEROS
01218        AND BIND-GRP-LEN       = ZEROS
01219        AND BAPR-LEN           = ZEROS
01220        AND BFREQ-LEN          = ZEROS
01221        AND BSIG-LEN           = ZEROS
01222        AND BTERM-LEN     (1)  = ZEROS
01223        AND BTERM-LEN     (2)  = ZEROS
01224        AND BTYPE-LEN     (1)  = ZEROS
01225        AND BTYPE-LEN     (2)  = ZEROS
01226        AND BBEN-LEN      (1)  = ZEROS
01227        AND BBEN-LEN      (2)  = ZEROS
01228        AND BALT-BEN-LEN  (1)  = ZEROS
01229        AND BPREM-LEN     (1)  = ZEROS
01230        AND BPREM-LEN     (2)  = ZEROS
01231        AND BALT-PREM-LEN (1)  = ZEROS
01232        AND BLIVES-LEN         = ZEROS
01233        AND BPOLICY-LEN        = ZEROS
01234        AND BENTRY-LEN         = ZEROS
01235        AND BRINCD-LEN         = ZEROS
01236        AND BSSNUM-LEN         = ZEROS
01237        AND BMEM-NO-LEN        = ZEROS
01238        AND BJNT-AGE-LEN       = ZEROS
01239        AND BBENEFICIARY-LEN   = ZEROS
01240        AND BBIRTH-LEN         = ZEROS
01241        AND BMODE-LEN          = ZEROS
01242        AND BPMTS-LEN          = ZEROS
01243        AND BLN-OFFICER-LEN    = ZEROS
01244        AND BDAYS-LEN          = ZEROS
01245        AND BLN-TERM-LEN       = ZEROS
01246        AND BEXPIRE-LEN (1)    = ZEROS
01247        AND BEXPIRE-LEN (2)    = ZEROS
01248        AND BPMT-LEN           = ZEROS
01249        AND B1ST-PMT-LEN       = ZEROS
01250        AND BSKPCD-LEN         = ZEROS
01251        AND BADDRS1-LEN        = ZEROS
01252        AND BADDRS2-LEN        = ZEROS
01253        AND BCITYST-LEN        = ZEROS
01254        AND BZIP5-LEN          = ZEROS
01255        AND BAGE-LEN           = ZEROS
01256        AND BZIP4-LEN          = ZEROS
01257        AND BPHONE-LEN         = ZEROS
01258          MOVE SPACE              TO PI-DISPLAY-SW
01259          GO TO 1030-NOTHING-TO-EDIT.
01260
01261  1010-EDIT-MAPB.
01262
01263      IF BCERT-LEN             = ZEROS
01264        AND BLAST-NAME-LEN     = ZEROS
01265        AND BEFFDT-LEN         = ZEROS
01266        AND NOT PI-LAST-FUNC-DISPLAY
01267          GO TO 1030-NOTHING-TO-EDIT.
01268
01269      MOVE AL-SABON               TO BSEQ-ATTRB.
01270
01271
01272      IF NOT PI-LAST-FUNC-DISPLAY
01273         IF BCAR-LEN GREATER THAN ZEROS
01274            MOVE AL-UANON           TO BCAR-ATTRB
01275            PERFORM 1500-VERIFY-CARRIER-ID THRU 1590-EXIT
01276         ELSE
01277            IF NOT ST-ACCNT-CNTL AND NOT ACCNT-CNTL
01278                MOVE -1             TO BCAR-LEN
01279                MOVE AL-UABON       TO BCAR-ATTRB
01280                MOVE ER-0194        TO EMI-ERROR
01281                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01282
01283      IF NOT PI-LAST-FUNC-DISPLAY
01284         IF BGRP-LEN GREATER THAN ZEROS
01285            MOVE AL-UANON           TO BGRP-ATTRB
01286         ELSE
01287            IF CARR-GROUP-ST-ACCNT-CNTL
01288                MOVE -1 TO          BGRP-LEN
01289                MOVE AL-UABON       TO BGRP-ATTRB
01290                MOVE ER-0195        TO EMI-ERROR
01291                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01292
01293      IF NOT PI-LAST-FUNC-DISPLAY
01294        IF BST-LEN GREATER THAN ZEROS
01295            MOVE AL-UANON           TO BST-ATTRB
01296            PERFORM 1600-VERIFY-STATE-ID THRU 1690-EXIT
01297        ELSE
01298            IF NOT ACCNT-CNTL AND NOT CARR-ACCNT-CNTL
01299                MOVE -1             TO BST-LEN
01300                MOVE AL-UABON       TO BST-ATTRB
01301                MOVE ER-0196        TO EMI-ERROR
01302                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01303
01304      IF NOT PI-LAST-FUNC-DISPLAY
01305         IF BACCT-LEN GREATER THAN ZEROS
01306            MOVE AL-UANON           TO BACCT-ATTRB
01307            PERFORM 1700-VERIFY-ACCOUNT THRU 1790-EXIT
01308         ELSE
01309            MOVE -1 TO BACCT-LEN
01310            MOVE AL-UABON           TO BACCT-ATTRB
01311            MOVE ER-0197            TO EMI-ERROR
01312            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01313
01314      IF BCERT-LEN  GREATER THAN ZEROS
01315        AND PI-LAST-FUNC-DISPLAY
01316          NEXT SENTENCE
01317      ELSE
01318          IF BCERT-LEN  GREATER THAN ZEROS
01319              MOVE AL-UANON       TO BCERT-ATTRB
01320          ELSE
01321              MOVE -1             TO BCERT-LEN
01322              MOVE ER-2218        TO EMI-ERROR
01323              MOVE AL-UABON       TO BCERT-ATTRB
01324              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01325
01326      IF BSFX-LEN  NOT = ZEROS
01327          MOVE 'Y'                TO PI-ISS-SUFFIX-KEYED-SW
01328          MOVE AL-UANON           TO BSFX-ATTRB.
01329
01330      IF BEFFDT-LEN  = ZEROS
01331        AND PI-LAST-FUNC-DISPLAY
01332          NEXT SENTENCE
01333      ELSE
01334          IF BEFFDT-LEN   GREATER THAN ZEROS
01335              MOVE AL-UNNON           TO BEFFDT-ATTRB
01336              IF BEFFDT   NUMERIC
01337                  MOVE 4              TO DC-OPTION-CODE
01338                  MOVE BEFFDT    TO DC-GREG-DATE-1-MDY
01339                  PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
01340                  MOVE DC-BIN-DATE-1  TO WS-CONVERTED-EFFDT
01341                  IF NO-CONVERSION-ERROR
01342                      IF WS-CONVERTED-EFFDT NOT LESS THAN
01343                        PI-ACCT-LOW-EFF-DT  AND LESS THAN
01344                        PI-ACCT-HIGH-EXP-DT
01345                          NEXT SENTENCE
01346                      ELSE
01347                          MOVE -1       TO BEFFDT-LEN
01348                          MOVE ER-2589  TO EMI-ERROR
01349                          MOVE AL-UNBON TO BEFFDT-ATTRB
01350                          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01351                  ELSE
01352                      MOVE -1         TO BEFFDT-LEN
01353                      MOVE ER-2226    TO EMI-ERROR
01354                      MOVE AL-UNBON   TO BEFFDT-ATTRB
01355                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01356              ELSE
01357                  MOVE -1             TO BEFFDT-LEN
01358                  MOVE ER-2223        TO EMI-ERROR
01359                  MOVE AL-UNBON       TO BEFFDT-ATTRB
01360                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01361          ELSE
01362              MOVE -1                 TO BEFFDT-LEN
01363              MOVE ER-2220            TO EMI-ERROR
01364              MOVE AL-UNBON           TO BEFFDT-ATTRB
01365              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01366
01367      IF BLAST-NAME-LEN   GREATER THAN ZEROS
01368          MOVE AL-UANON           TO BLAST-NAME-ATTRB.
01369
01370      IF B1ST-NAME-LEN    GREATER THAN ZEROS
01371          MOVE AL-UANON           TO B1ST-NAME-ATTRB.
01372
01373      IF BINIT-LEN        GREATER THAN ZEROS
01374          MOVE AL-UANON           TO BINIT-ATTRB.
01375
01376      IF BSEX-LEN         GREATER THAN ZEROS
01377          IF BSEX   = 'M' OR 'F'
01378              MOVE AL-UANON       TO BSEX-ATTRB
01379          ELSE
01380              MOVE -1             TO BSEX-LEN
01381              MOVE ER-2629        TO EMI-ERROR
01382              MOVE AL-UABON       TO BSEX-ATTRB
01383              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01384
01385      IF BAGE-LEN        GREATER THAN ZEROS
01386         IF BAGE NUMERIC
01387            MOVE BAGE             TO WS-BAGE
01388            MOVE AL-UNNON         TO BAGE-ATTRB
01389         ELSE
01390            MOVE -1             TO BAGE-LEN
01391            MOVE ER-2223        TO EMI-ERROR
01392            MOVE AL-UNBON       TO BAGE-ATTRB
01393            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01394
01395
01396      IF BSSNUM-LEN          GREATER THAN ZEROS
01397          MOVE 'Y'                TO PI-SSNUM-KEYED-SW
01398          MOVE AL-UANON           TO BSSNUM-ATTRB.
01399
01400      MOVE +0                     TO WS-SUB1.
01401
01402      EJECT
01403
01404 ******************************************************************
01405 *                                                                *
01406 *           E D I T   I S S U E   C O V E R A G E S              *
01407 *                                                                *
01408 ******************************************************************
01409
01410  1020-EDIT-COVERAGES.
01411
01412      IF NOT MODIFY-CAP
01413            MOVE 'UPDATE'       TO SM-READ
01414            PERFORM 9995-SECURITY-VIOLATION
01415            MOVE ER-0070        TO EMI-ERROR
01416            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01417            GO TO 8100-SEND-INITIAL-MAP.
01418
01419      ADD +1                      TO WS-SUB1.
01420
01421      IF WS-SUB1 GREATER THAN +2
01422         GO TO 1025-CONT-EDIT.
01423
01424      IF BTYPE-LEN          (WS-SUB1)  GREATER THAN ZEROS
01425         OR  BTERM-LEN      (WS-SUB1)  GREATER THAN ZEROS
01426         OR  BBEN-LEN       (WS-SUB1)  GREATER THAN ZEROS
01427         OR  BPREM-LEN      (WS-SUB1)  GREATER THAN ZEROS
01428         OR  BCRIT-PERD-LEN (WS-SUB1)  GREATER THAN ZEROS
01429         OR  BEXPIRE-LEN    (WS-SUB1)  GREATER THAN ZEROS
01430         OR  BALT-PREM-LEN  (WS-SUB1)  GREATER THAN ZEROS
01431         OR  BALT-BEN-LEN   (WS-SUB1)  GREATER THAN ZEROS
01432             MOVE 'Y'             TO WS-DATA-KEYED-SW
01433      ELSE
01434             GO TO 1020-EDIT-COVERAGES.
01435
01436      IF NOT PI-LAST-FUNC-DISPLAY
01437         IF BTYPE-LEN  (WS-SUB1) GREATER THAN ZEROS
01438              MOVE AL-UANON         TO BTYPE-ATTRB       (WS-SUB1)
01439              PERFORM 1040-EDIT-INPUT-CODE THRU 1059-EXIT
01440         ELSE
01441            NEXT SENTENCE
01442      ELSE
01443         IF BTYPE-LEN  (WS-SUB1) GREATER THAN ZEROS
01444            IF BTYPE   (WS-SUB1) = SPACES OR ZEROS
01445               MOVE AL-UANON      TO BTYPE-ATTRB       (WS-SUB1)
01446            ELSE
01447               MOVE AL-UANON      TO BTYPE-ATTRB       (WS-SUB1)
01448               PERFORM 1040-EDIT-INPUT-CODE THRU 1059-EXIT.
01449
01450      IF BPMTS-LEN           GREATER THAN ZEROS
01451          MOVE 'Y'                TO PI-PMTS-KEYED-SW
01452          MOVE BPMTS-IN           TO DEEDIT-FIELD
01453          PERFORM 8600-DEEDIT
01454          IF DEEDIT-FIELD-V0 NUMERIC
01455             MOVE DEEDIT-FIELD-V0 TO WS-BPMTS
01456             MOVE AL-UNNON        TO BPMTS-ATTRB.
01457
01458      IF BPMT-LEN              GREATER THAN ZEROS
01459          MOVE 'Y'                TO PI-PMT-KEYED-SW
01460          MOVE BPMTI              TO DEEDIT-FIELD
01461          PERFORM 8600-DEEDIT
01462          IF DEEDIT-FIELD-V2     NUMERIC
01463            MOVE DEEDIT-FIELD-V2 TO WS-BPMT
01464            MOVE AL-UNNON           TO BPMT-ATTRB.
01465
01466      IF BTERM-LEN (WS-SUB1) GREATER THAN ZEROS
01467         NEXT SENTENCE
01468      ELSE
01469         IF BLN-TERM-LEN GREATER THAN ZERO
01470            AND WS-BLN-TERM NUMERIC
01471            MOVE WS-BLN-TERM      TO BTERMI      (WS-SUB1)
01472            MOVE +3               TO BTERM-LEN   (WS-SUB1).
01473
01474      IF  WS-TERM-IN-DAYS-FOUND
01475          AND BMODE-LEN   GREATER THAN ZERO
01476          PERFORM 1090-CALCULATE-MONTHLY-TERM THRU 1094-EXIT
01477      ELSE
01478          IF BMODE-LEN   GREATER THAN ZEROS
01479             AND BPMTS-LEN   GREATER THAN ZEROS
01480             PERFORM 1080-TERM-CONVERSION THRU 1089-EXIT.
01481
01482      IF PI-LAST-FUNC-DISPLAY
01483         IF BTERM-LEN (WS-SUB1)  = ZEROS
01484            NEXT SENTENCE
01485         ELSE
01486            MOVE BTERMI    (WS-SUB1)   TO DEEDIT-FIELD
01487            PERFORM 8600-DEEDIT
01488            IF DEEDIT-FIELD-V0 NUMERIC
01489               MOVE DEEDIT-FIELD-V0    TO WS-BTERM    (WS-SUB1)
01490               IF WS-BTERM (WS-SUB1)  GREATER THAN ZERO
01491                     MOVE AL-UNNON     TO BTERM-ATTRB (WS-SUB1)
01492               ELSE
01493                     MOVE ER-2241      TO EMI-ERROR
01494                     MOVE -1           TO BTERM-LEN   (WS-SUB1)
01495                     MOVE AL-UNBOF     TO BTERM-ATTRB (WS-SUB1)
01496                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01497            ELSE
01498               MOVE ER-2223          TO EMI-ERROR
01499               MOVE -1               TO BTERM-LEN   (WS-SUB1)
01500               MOVE AL-UNBON         TO BTERM-ATTRB (WS-SUB1)
01501               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01502      ELSE
01503         IF  BTERM-LEN      (WS-SUB1)   GREATER THAN ZEROS
01504             MOVE BTERMI    (WS-SUB1)   TO DEEDIT-FIELD
01505             PERFORM 8600-DEEDIT
01506             IF DEEDIT-FIELD-V0      NUMERIC
01507                IF DEEDIT-FIELD-V0     GREATER THAN ZERO
01508                   MOVE DEEDIT-FIELD-V0 TO WS-BTERM    (WS-SUB1)
01509                   MOVE AL-UNNON        TO BTERM-ATTRB (WS-SUB1)
01510                ELSE
01511                   MOVE ER-2241         TO EMI-ERROR
01512                   MOVE -1              TO BTERM-LEN   (WS-SUB1)
01513                   MOVE AL-UNBOF        TO BTERM-ATTRB (WS-SUB1)
01514                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01515             ELSE
01516               MOVE ER-2223             TO EMI-ERROR
01517               MOVE -1                  TO BTERM-LEN   (WS-SUB1)
01518               MOVE AL-UNBON            TO BTERM-ATTRB (WS-SUB1)
01519               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01520         ELSE
01521             MOVE ER-2240               TO EMI-ERROR
01522             MOVE -1                    TO BTERM-LEN   (WS-SUB1)
01523             MOVE AL-UNBOF              TO BTERM-ATTRB (WS-SUB1)
01524             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01525
01526      IF BBEN-LEN (WS-SUB1) = ZEROS
01527         AND PI-LAST-FUNC-DISPLAY
01528            NEXT SENTENCE
01529      ELSE
01530         IF BBEN-LEN (WS-SUB1) GREATER THAN ZEROS
01531            MOVE AL-UNNON           TO BBEN-ATTRB (WS-SUB1)
01532            MOVE BBENI (WS-SUB1)    TO DEEDIT-FIELD
01533            PERFORM 8600-DEEDIT
01534            IF DEEDIT-FIELD-V2  NUMERIC
01535               IF DEEDIT-FIELD-V2 GREATER THAN ZEROS
01536                  MOVE DEEDIT-FIELD-V2 TO WS-BBEN    (WS-SUB1)
01537               ELSE
01538                  MOVE ER-7632    TO EMI-ERROR
01539                  MOVE -1         TO BBEN-LEN   (WS-SUB1)
01540                  MOVE AL-UNBOF   TO BBEN-ATTRB (WS-SUB1)
01541                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01542            ELSE
01543               MOVE ER-2223         TO EMI-ERROR
01544               MOVE AL-UNBON        TO BBEN-ATTRB (WS-SUB1)
01545               MOVE -1              TO BBEN-LEN   (WS-SUB1)
01546         ELSE
01547            MOVE ER-7632    TO EMI-ERROR
01548            MOVE -1         TO BBEN-LEN   (WS-SUB1)
01549            MOVE AL-UNBOF   TO BBEN-ATTRB (WS-SUB1)
01550            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01551
01552      IF BPREM-LEN    (WS-SUB1) = ZEROS
01553         AND PI-LAST-FUNC-DISPLAY
01554            NEXT SENTENCE
01555      ELSE
01556         IF BPREM-LEN (WS-SUB1) GREATER THAN ZEROS
01557            MOVE AL-UNNON           TO BPREM-ATTRB (WS-SUB1)
01558            MOVE BPREMI (WS-SUB1)   TO DEEDIT-FIELD
01559            PERFORM 8600-DEEDIT
01560            IF DEEDIT-FIELD-V2  NUMERIC
01561               IF DEEDIT-FIELD-V2 GREATER THAN ZEROS
01562                  MOVE DEEDIT-FIELD-V2 TO WS-BPREM   (WS-SUB1)
01563               ELSE
01564                  MOVE ER-7633    TO EMI-ERROR
01565                  MOVE -1         TO BPREM-LEN   (WS-SUB1)
01566                  MOVE AL-UNBOF   TO BPREM-ATTRB (WS-SUB1)
01567                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01568            ELSE
01569               MOVE AL-UNBON   TO BPREM-ATTRB (WS-SUB1)
01570               MOVE ER-2223         TO EMI-ERROR
01571               MOVE -1              TO BPREM-LEN  (WS-SUB1)
01572         ELSE
01573            MOVE ER-7633    TO EMI-ERROR
01574            MOVE -1         TO BPREM-LEN   (WS-SUB1)
01575            MOVE AL-UNBOF   TO BPREM-ATTRB (WS-SUB1)
01576            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01577
01578      IF BEXPIRE-LEN (WS-SUB1)   GREATER THAN ZEROS
01579         MOVE 'Y'                 TO PI-EXPIRE-KEYED-SW
01580          IF BEXPIRE (WS-SUB1)    NUMERIC
01581              MOVE AL-UNNON       TO BEXPIRE-ATTRB (WS-SUB1)
01582              MOVE 4              TO DC-OPTION-CODE
01583              MOVE BEXPIRE (WS-SUB1)   TO DC-GREG-DATE-1-MDY
01584              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
01585              MOVE DC-BIN-DATE-1  TO WS-CONVERTED-EXPIRDT (WS-SUB1)
01586              IF NO-CONVERSION-ERROR
01587                  NEXT SENTENCE
01588              ELSE
01589                  MOVE -1         TO BEXPIRE-LEN   (WS-SUB1)
01590                  MOVE ER-2531    TO EMI-ERROR
01591                  MOVE AL-UNBON   TO BEXPIRE-ATTRB (WS-SUB1)
01592                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01593          ELSE
01594              MOVE -1             TO BEXPIRE-LEN   (WS-SUB1)
01595              MOVE ER-2532        TO EMI-ERROR
01596              MOVE AL-UNBON       TO BEXPIRE-ATTRB (WS-SUB1)
01597              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01598
01599      IF BCRIT-PERD-LEN  (WS-SUB1)    GREATER THAN ZEROS
01600         MOVE 'Y'                     TO PI-CRIT-PERD-KEYED-SW
01601         MOVE BCRIT-PERDI (WS-SUB1)   TO DEEDIT-FIELD
01602         PERFORM 8600-DEEDIT
01603         IF DEEDIT-FIELD-V0 NUMERIC
01604            MOVE DEEDIT-FIELD-V0      TO WS-BCRIT-PERD    (WS-SUB1)
01605            MOVE AL-UNNON             TO BCRIT-PERD-ATTRB (WS-SUB1)
01606          ELSE
01607            MOVE -1                   TO BCRIT-PERD-LEN   (WS-SUB1)
01608            MOVE AL-UNBON             TO BCRIT-PERD-ATTRB (WS-SUB1)
01609            MOVE ER-2223              TO EMI-ERROR
01610            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01611
01612
01613      IF BALT-BEN-LEN (WS-SUB1) = ZEROS
01614         AND PI-LAST-FUNC-DISPLAY
01615            NEXT SENTENCE
01616      ELSE
01617         IF BALT-BEN-LEN (WS-SUB1) GREATER THAN ZEROS
01618            MOVE 'Y'              TO PI-ALT-BEN-KEYED-SW
01619            MOVE AL-UNNON         TO BALT-BEN-ATTRB (WS-SUB1)
01620            MOVE BALT-BENI (WS-SUB1)    TO DEEDIT-FIELD
01621            PERFORM 8600-DEEDIT
01622            IF DEEDIT-FIELD-V2  NUMERIC
01623               MOVE DEEDIT-FIELD-V2 TO WS-BALT-BEN (WS-SUB1)
01624            ELSE
01625               MOVE ER-2223       TO EMI-ERROR
01626               MOVE AL-UNBON      TO BALT-BEN-ATTRB (WS-SUB1)
01627               MOVE -1            TO BALT-BEN-LEN   (WS-SUB1).
01628
01629      IF BALT-PREM-LEN    (WS-SUB1) = ZEROS
01630         AND PI-LAST-FUNC-DISPLAY
01631            NEXT SENTENCE
01632      ELSE
01633         IF BALT-PREM-LEN   (WS-SUB1) GREATER THAN ZEROS
01634            MOVE 'Y'              TO PI-ALT-PREM-KEYED-SW
01635            MOVE AL-UNNON              TO BALT-PREM-ATTRB (WS-SUB1)
01636            MOVE BALT-PREMI (WS-SUB1)  TO DEEDIT-FIELD
01637            PERFORM 8600-DEEDIT
01638            IF DEEDIT-FIELD-V2  NUMERIC
01639               MOVE DEEDIT-FIELD-V2 TO WS-BALT-PREM   (WS-SUB1)
01640            ELSE
01641               MOVE AL-UNBON      TO BALT-PREM-ATTRB (WS-SUB1)
01642               MOVE ER-2223       TO EMI-ERROR
01643               MOVE -1            TO BALT-PREM-LEN  (WS-SUB1).
01644
01645      GO TO 1020-EDIT-COVERAGES.
01646
01647  1025-CONT-EDIT.
01648
01649      IF BLIVES-LEN               GREATER THAN ZEROS
01650         MOVE 'Y'                   TO PI-ISS-LIVES-KEYED-SW
01651         MOVE BLIVESI               TO DEEDIT-FIELD
01652         PERFORM 8600-DEEDIT
01653         IF DEEDIT-FIELD-V0 NUMERIC
01654            MOVE DEEDIT-FIELD-V0    TO WS-BLIVES
01655            MOVE AL-UNNON           TO BLIVES-ATTRB
01656         ELSE
01657            MOVE -1                 TO BLIVES-LEN
01658            MOVE AL-UNBON           TO BLIVES-ATTRB
01659            MOVE ER-2223            TO EMI-ERROR
01660            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01661
01662      IF BJNT-1ST-NAME-LEN     GREATER THAN ZEROS OR
01663         BJNT-INIT-LEN         GREATER THAN ZEROS OR
01664         BJNT-LST-NAME-LEN     GREATER THAN ZEROS
01665          MOVE 'Y'                TO PI-JNT-NAME-KEYED-SW
01666          MOVE AL-UANON           TO BJNT-1ST-NAME-ATTRB
01667                                     BJNT-INIT-ATTRB
01668                                     BJNT-LST-NAME-ATTRB.
01669
01670      IF BJNT-AGE-LEN        GREATER THAN ZEROS
01671         MOVE 'Y'                 TO PI-JNT-AGE-KEYED-SW
01672         IF BJNT-AGE NUMERIC
01673            MOVE BJNT-AGE         TO WS-BJNT-AGE
01674            MOVE AL-UNNON         TO BJNT-AGE-ATTRB
01675         ELSE
01676            MOVE -1             TO BJNT-AGE-LEN
01677            MOVE ER-2223        TO EMI-ERROR
01678            MOVE AL-UNBON       TO BJNT-AGE-ATTRB
01679            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01680
01681      IF BBENEFICIARY-LEN    GREATER THAN ZEROS
01682          MOVE 'Y'                TO PI-BENEFICIARY-KEYED-SW
01683          MOVE AL-UANON           TO BBENEFICIARY-ATTRB.
01684
01685      IF B1ST-PMT-LEN GREATER THAN ZEROS
01686         MOVE 'Y'                     TO PI-1ST-PMT-KEYED-SW
01687         IF B1ST-PMT = SPACES
01688            MOVE LOW-VALUES           TO WS-CONVERTED-1ST-PMT-DT
01689         ELSE
01690            MOVE B1ST-PMT             TO DEEDIT-FIELD
01691            PERFORM 8600-DEEDIT
01692            MOVE DEEDIT-FIELD-V0      TO DC-GREG-DATE-1-MDY
01693            MOVE AL-UNNON             TO B1ST-PMT-ATTRB
01694            MOVE 4                    TO DC-OPTION-CODE
01695            PERFORM 8500-DATE-CONVERT
01696            IF NO-CONVERSION-ERROR
01697               MOVE DC-BIN-DATE-1     TO WS-CONVERTED-1ST-PMT-DT
01698               MOVE AL-UANON          TO B1ST-PMT-ATTRB
01699            ELSE
01700               MOVE -1                TO B1ST-PMT-LEN
01701               MOVE ER-2200           TO EMI-ERROR
01702               MOVE AL-UNBON          TO B1ST-PMT-ATTRB
01703               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01704
01705      IF BDAYS-LEN       GREATER THAN ZEROS
01706         MOVE 'Y'                TO PI-DAYS-KEYED-SW
01707         MOVE BDAYSI             TO DEEDIT-FIELD
01708         PERFORM 8600-DEEDIT
01709         IF DEEDIT-FIELD-V0 NUMERIC
01710            MOVE DEEDIT-FIELD-V0  TO WS-BDAYS
01711            MOVE AL-UNNON        TO BDAYS-ATTRB
01712         ELSE
01713            MOVE -1              TO BDAYS-LEN
01714            MOVE ER-7530         TO EMI-ERROR
01715            MOVE AL-UNBON        TO BDAYS-ATTRB
01716            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01717
01718      IF BLN-TERM-LEN   = ZEROS
01719          NEXT SENTENCE
01720      ELSE
01721         MOVE 'Y'                 TO PI-LNTRM-KEYED-SW
01722         MOVE BLN-TERMI           TO DEEDIT-FIELD
01723         PERFORM 8600-DEEDIT
01724         IF DEEDIT-FIELD-V0 NUMERIC
01725            MOVE DEEDIT-FIELD-V0  TO WS-BLN-TERM
01726            IF WS-BLN-TERM  GREATER THAN ZERO
01727               MOVE AL-UNNON TO BLN-TERM-ATTRB
01728                  ELSE
01729                      MOVE ER-2241  TO EMI-ERROR
01730                      MOVE -1       TO BLN-TERM-LEN
01731                      MOVE AL-UNBOF TO BLN-TERM-ATTRB
01732                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01733              ELSE
01734                  MOVE ER-2223      TO EMI-ERROR
01735                  MOVE -1           TO BLN-TERM-LEN
01736                  MOVE AL-UNBON     TO BLN-TERM-ATTRB
01737                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01738
01739      IF BLN-OFFICER-LEN      GREATER THAN ZEROS
01740          MOVE 'Y'                TO PI-LN-OFFICER-KEYED-SW
01741          MOVE AL-UANON           TO BLN-OFFICER-ATTRB.
01742
01743      IF BMODE-LEN            GREATER THAN ZEROS
01744          MOVE BMODE              TO WS-MODE-CODE
01745          MOVE 'Y'                TO PI-MODE-KEYED-SW
01746          IF WS-MODE-CODE-VALID
01747              MOVE AL-UANON       TO BMODE-ATTRB
01748          ELSE
01749              MOVE -1             TO BMODE-LEN
01750              MOVE ER-2591        TO EMI-ERROR
01751              MOVE AL-UABON       TO BMODE-ATTRB
01752              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01753
01754      IF BFREQ-LEN             GREATER THAN ZEROS
01755         MOVE 'Y'                 TO PI-FREQ-KEYED-SW
01756         MOVE BFREQI              TO DEEDIT-FIELD
01757         PERFORM 8600-DEEDIT
01758         IF DEEDIT-FIELD-V0 NUMERIC
01759            MOVE DEEDIT-FIELD-V0  TO WS-BFREQ
01760            MOVE AL-UNNON         TO BFREQ-ATTRB
01761         ELSE
01762            MOVE -1             TO BFREQ-LEN
01763            MOVE ER-2223        TO EMI-ERROR
01764            MOVE AL-UNBON       TO BFREQ-ATTRB
01765            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01766
01767      IF BPMTS-LEN           GREATER THAN ZEROS
01768          MOVE 'Y'                TO PI-PMTS-KEYED-SW
01769          MOVE BPMTS-IN           TO DEEDIT-FIELD
01770          PERFORM 8600-DEEDIT
01771          IF DEEDIT-FIELD-V0 NUMERIC
01772             MOVE DEEDIT-FIELD-V0 TO WS-BPMTS
01773             MOVE AL-UNNON        TO BPMTS-ATTRB
01774          ELSE
01775              MOVE -1             TO BPMTS-LEN
01776              MOVE ER-2592        TO EMI-ERROR
01777              MOVE AL-UNBON       TO BPMTS-ATTRB
01778              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01779
01780      IF BPMT-LEN              GREATER THAN ZEROS
01781          MOVE 'Y'                TO PI-PMT-KEYED-SW
01782          MOVE BPMTI              TO DEEDIT-FIELD
01783          PERFORM 8600-DEEDIT
01784          IF DEEDIT-FIELD-V2     NUMERIC
01785            MOVE DEEDIT-FIELD-V2 TO WS-BPMT
01786            MOVE AL-UNNON           TO BPMT-ATTRB
01787         ELSE
01788            MOVE -1                 TO BPMT-LEN
01789            MOVE ER-2529            TO EMI-ERROR
01790            MOVE AL-UNBON           TO BPMT-ATTRB
01791            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01792
01793      IF BRINCD-LEN              GREATER THAN ZEROS
01794          MOVE 'Y'                TO PI-RINCD-KEYED-SW
01795          MOVE AL-UANON           TO BRINCD-ATTRB.
01796
01797      IF BENTRY-LEN           GREATER THAN ZEROS
01798          MOVE 'Y'                TO PI-ENTRY-KEYED-SW
01799          MOVE BENTRY             TO WS-ENTRY-CODE
01800          IF WS-ENTRY-CODE-VALID
01801             MOVE AL-UANON       TO BENTRY-ATTRB
01802          ELSE
01803             MOVE ER-7573        TO EMI-ERROR
01804             MOVE -1             TO BENTRY-LEN
01805             MOVE AL-UABON       TO BENTRY-ATTRB
01806             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01807
01808      IF  BSKPCD-LEN          GREATER THAN ZEROS
01809          MOVE 'Y'                TO PI-SKPCD-KEYED-SW
01810          MOVE AL-UANON           TO BSKPCD-ATTRB.
01811
01812      IF BIND-GRP-LEN        GREATER THAN ZEROS
01813          MOVE 'Y'                TO PI-IG-KEYED-SW
01814          MOVE AL-UANON           TO BIND-GRP-ATTRB.
01815
01816      IF BSIG-LEN            GREATER THAN ZEROS
01817          MOVE 'Y'                TO PI-SIG-KEYED-SW
01818          IF BSIG   = 'Y'
01819              MOVE AL-UANON       TO BSIG-ATTRB
01820          ELSE
01821              MOVE -1             TO BSIG-LEN
01822              MOVE ER-2651        TO EMI-ERROR
01823              MOVE AL-UABON       TO BSIG-ATTRB
01824              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01825
01826      IF BPOLICY-LEN          GREATER THAN ZEROS
01827          MOVE 'Y'                TO PI-POLICY-KEYED-SW
01828          MOVE AL-UANON           TO BPOLICY-ATTRB.
01829
01830      IF BRTCLS-LEN           GREATER THAN ZEROS
01831          MOVE 'Y'                TO PI-RTCLS-KEYED-SW
01832          MOVE AL-UANON           TO BRTCLS-ATTRB.
01833
01834      IF BAPR-LEN            GREATER THAN ZEROS
01835          MOVE 'Y'                TO PI-APR-KEYED-SW
01836          MOVE BAPR-IN            TO DEEDIT-FIELD
01837          PERFORM 8600-DEEDIT
01838          IF DEEDIT-FIELD-V4  NUMERIC
01839              MOVE AL-UNNON       TO BAPR-ATTRB
01840              MOVE DEEDIT-FIELD-V4 TO WS-BAPR
01841          ELSE
01842              MOVE ER-2471        TO EMI-ERROR
01843              MOVE -1             TO BAPR-LEN
01844              MOVE AL-UNBON       TO BAPR-ATTRB
01845              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01846
01847      IF BBIRTH-LEN   GREATER THAN ZEROS
01848          IF BBIRTH-DT  NUMERIC
01849              MOVE AL-UNNON       TO BBIRTH-ATTRB
01850              MOVE 4              TO DC-OPTION-CODE
01851              MOVE BBIRTH-DT      TO DC-GREG-DATE-1-MDY
01852              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
01853              MOVE DC-BIN-DATE-1  TO WS-CONVERTED-BIRTH
01854              IF NO-CONVERSION-ERROR
01855                  IF BAGE-LEN   = ZERO
01856                      PERFORM 1095-CALC-AGE THRU 1099-EXIT
01857                  ELSE
01858                      NEXT SENTENCE
01859              ELSE
01860                  MOVE -1         TO BBIRTH-LEN
01861                  MOVE ER-2228    TO EMI-ERROR
01862                  MOVE AL-UNBON   TO BBIRTH-ATTRB
01863                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01864          ELSE
01865              MOVE -1             TO BBIRTH-LEN
01866              MOVE ER-2223        TO EMI-ERROR
01867              MOVE AL-UNBON       TO BBIRTH-ATTRB
01868              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01869      ELSE
01870          IF PI-LAST-FUNC-DISPLAY
01871              NEXT SENTENCE
01872          ELSE
01873              IF PI-BIRTH-DATE-IS-INPUT
01874                  MOVE ER-2442    TO EMI-ERROR
01875                  MOVE -1         TO BBIRTH-LEN
01876                  MOVE AL-UNBON   TO BBIRTH-ATTRB
01877                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01878              ELSE
01879                  MOVE AL-SANOF   TO BBIRTH-ATTRB.
01880
01881      IF BMEM-NO-LEN      GREATER THAN ZEROS
01882          MOVE 'Y'                TO PI-MEMBER-KEYED-SW
01883          MOVE AL-UANON           TO BMEM-NO-ATTRB.
01884
01885      IF  BADDRS1-LEN        GREATER THAN ZEROS
01886          MOVE AL-UANON           TO BADDRS1-ATTRB.
01887
01888      IF  BADDRS2-LEN        GREATER THAN ZEROS
01889          MOVE AL-UANON           TO BADDRS2-ATTRB.
01890
01891      IF  BCITYST-LEN        GREATER THAN ZEROS
01892          MOVE AL-UANON           TO BCITYST-ATTRB.
01893
01894      IF  BZIP5-LEN          GREATER THAN ZEROS
01895          MOVE AL-UANON           TO BZIP5-ATTRB.
01896
01897      IF  BZIP4-LEN          GREATER THAN ZEROS
01898 *        MOVE 'Y'                TO PI-ZIP4-KEYED-SW
01899          MOVE AL-UANON           TO BZIP4-ATTRB.
01900
01901      IF  BPHONE-LEN         GREATER THAN ZERO
01902          MOVE 'Y'                TO PI-PHONE-KEYED-SW
01903          MOVE BPHONE             TO DEEDIT-FIELD
01904          PERFORM 8600-DEEDIT
01905          MOVE DEEDIT-FIELD-V0 TO WS-BPHONE
01906          MOVE AL-UANON       TO BPHONE-ATTRB.
01907
01908      IF NOT PI-LAST-FUNC-DISPLAY
01909         AND WS-DATA-NOT-KEYED
01910         MOVE ER-7400             TO EMI-ERROR
01911         MOVE -1                  TO BTYPE-LEN (1)
01912         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01913         MOVE 'Y'                 TO PI-ERROR-SW.
01914
01915      IF EMI-ERROR = ZEROS
01916          MOVE 'Y'                TO PI-UPDATE-SW
01917          MOVE SPACE              TO PI-DISPLAY-SW
01918          PERFORM 4000-BUILD-ISSUE-RECORD THRU 4900-EXIT
01919      ELSE
01920          MOVE ZEROS              TO EMI-ERROR
01921          MOVE 'Y'                TO PI-ERROR-SW.
01922
01923  1030-NOTHING-TO-EDIT.
01924
01925      IF PI-DATA-ERRORS
01926          MOVE AL-SABON           TO BSEQ-ATTRB
01927          GO TO 8200-SEND-DATAONLY.
01928
01929
01930      IF PI-SAV-BATCH-SEQ LESS THAN PI-LAST-SEQ-NO-ADDED
01931          SUBTRACT 1 FROM PI-SAV-BATCH-SEQ
01932          MOVE ER-0000        TO EMI-ERROR
01933          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01934          GO TO 2000-BROWSE-FWD.
01935
01936      MOVE LOW-VALUES     TO DATA-ENTRY-MAP.
01937      ADD +1, PI-LAST-SEQ-NO-ADDED
01938                          GIVING PI-NEXT-DISPLAY-SEQ-NO.
01939      MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ.
01940
01941      PERFORM 8550-SET-MAP-SEQ-NOS.
01942
01943      MOVE ER-0000        TO EMI-ERROR.
01944      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01945      GO TO 8100-SEND-INITIAL-MAP.
01946
01947      EJECT
01948
01949 ******************************************************************
01950 *                                                                *
01951 *            E D I T   I N P U T   C O D E S                     *
01952 *                                                                *
01953 ******************************************************************
01954
01955  1040-EDIT-INPUT-CODE.
01956
01957      IF WS-SUB1 = +2
01958         GO TO 1050-EDIT-INPUT-AH-CODE.
01959
01960      MOVE SPACES                 TO ELCNTL-ACCESS.
01961      MOVE 'L'                    TO ELCNTL-REC-TYPE.
01962      PERFORM 1070-ELCNTL-READ THRU 1079-EXIT.
01963
01964      IF EMI-ERROR = 9999
01965          GO TO 1048-NO-RECORD.
01966
01967      MOVE +1 TO WS-EDIT-SUB.
01968
01969  1041-SEARCH-LOOP.
01970
01971      IF CF-LIFE-CODE-OUT (WS-EDIT-SUB) = ZEROS
01972          GO TO 1047-NO-MATCH-FOUND.
01973
01974      IF BTYPE (1) = CF-LIFE-CODE-IN (WS-EDIT-SUB)
01975          MOVE CF-LIFE-CODE-OUT (WS-EDIT-SUB) TO WS-EDITED-LF-CODE
01976          PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT
01977          GO TO 1059-EXIT.
01978
01979      ADD 1   TO WS-EDIT-SUB.
01980
01981      IF WS-EDIT-SUB GREATER THAN 120
01982          GO TO 1047-NO-MATCH-FOUND.
01983      GO TO 1041-SEARCH-LOOP.
01984
01985  1047-NO-MATCH-FOUND.
01986
01987      MOVE ER-2424                TO EMI-ERROR.
01988      MOVE AL-UABON               TO BTYPE-ATTRB (1).
01989      MOVE -1                     TO BTYPE-LEN   (1).
01990      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01991      MOVE 'Y'                    TO ERROR-SW.
01992      GO TO 1059-EXIT.
01993
01994  1048-NO-RECORD.
01995
01996      MOVE ER-2423                TO EMI-ERROR.
01997      MOVE AL-UABON               TO BTYPE-ATTRB (1).
01998      MOVE -1                     TO BTYPE-LEN   (1).
01999      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02000      MOVE 'Y'                    TO ERROR-SW.
02001      GO TO 1059-EXIT.
02002
02003
02004  1050-EDIT-INPUT-AH-CODE.
02005
02006      MOVE SPACES                 TO ELCNTL-ACCESS.
02007      MOVE 'A'                    TO ELCNTL-REC-TYPE.
02008
02009      PERFORM 1070-ELCNTL-READ THRU 1079-EXIT.
02010
02011      IF EMI-ERROR = 9999
02012          GO TO 1058-NO-RECORD.
02013
02014      MOVE +1 TO WS-EDIT-SUB.
02015
02016  1051-SEARCH-LOOP.
02017
02018      IF CF-AH-CODE-OUT (WS-EDIT-SUB) = ZEROS
02019          GO TO 1057-NO-MATCH-FOUND.
02020
02021      IF BTYPE (2) = CF-AH-CODE-IN (WS-EDIT-SUB)
02022          MOVE CF-AH-CODE-OUT (WS-EDIT-SUB) TO WS-EDITED-AH-CODE
02023          PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT
02024          GO TO 1059-EXIT.
02025
02026      ADD +1  TO WS-EDIT-SUB.
02027
02028      IF WS-EDIT-SUB GREATER THAN +96
02029          GO TO 1057-NO-MATCH-FOUND.
02030
02031      GO TO 1051-SEARCH-LOOP.
02032
02033  1057-NO-MATCH-FOUND.
02034
02035      MOVE ER-2428                TO EMI-ERROR.
02036      MOVE AL-UABON               TO BTYPE-ATTRB (2).
02037      MOVE -1                     TO BTYPE-LEN   (2).
02038      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02039      MOVE 'Y'                    TO ERROR-SW.
02040      GO TO 1059-EXIT.
02041
02042  1058-NO-RECORD.
02043
02044      MOVE ER-2427                TO EMI-ERROR.
02045      MOVE AL-UABON               TO BTYPE-ATTRB (2).
02046      MOVE -1                     TO BTYPE-LEN   (2).
02047      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02048      MOVE 'Y'                    TO ERROR-SW.
02049
02050  1059-EXIT.
02051      EXIT.
02052
02053      EJECT
02054
02055 ******************************************************************
02056 *                                                                *
02057 *         B E N E F I T   M A S T E R   R E A D                  *
02058 *                                                                *
02059 ******************************************************************
02060
02061  1060-BENEFIT-MASTER-READ.
02062
02063      MOVE SPACES                 TO ELCNTL-ACCESS.
02064
02065      IF ELCNTL-REC-TYPE = 'L'
02066          MOVE WS-EDITED-LF-CODE  TO WS-BEN-CD
02067                                     ELCNTL-HI-BEN
02068          MOVE '4'                TO ELCNTL-REC-TYPE
02069      ELSE
02070          MOVE WS-EDITED-AH-CODE  TO WS-BEN-CD
02071                                     ELCNTL-HI-BEN
02072          MOVE '5'                TO ELCNTL-REC-TYPE.
02073
02074      PERFORM 1070-ELCNTL-READ THRU 1079-EXIT.
02075
02076      IF EMI-ERROR = 9999
02077          GO TO 1062-NO-RECORD.
02078
02079      IF (ELCNTL-COMPANY-ID NOT  = CF-COMPANY-ID) OR
02080         (ELCNTL-REC-TYPE NOT = CF-RECORD-TYPE)
02081            GO TO 1062-NO-RECORD.
02082
02083      PERFORM 1061-BENEFIT-DUMMY THRU 1061-DUMMY-EXIT
02084          VARYING WS-SUB FROM +1 BY +1 UNTIL
02085             ((WS-SUB GREATER +8) OR
02086             (CF-BENEFIT-CODE (WS-SUB) = WS-BEN-CD)).
02087
02088      IF WS-SUB NOT = +9
02089          IF ELCNTL-REC-TYPE = '4'
02090              MOVE CF-BENEFIT-ALPHA (WS-SUB) TO WS-LF-ABBR-DESC
02091          ELSE
02092              MOVE CF-BENEFIT-ALPHA (WS-SUB) TO WS-AH-ABBR-DESC
02093      ELSE
02094          GO TO 1063-NO-MATCH-FOUND.
02095
02096      IF  CF-TERM-IN-DAYS (WS-SUB)
02097          MOVE 'Y'                TO WS-TERM-IN-DAYS-SW.
02098
02099      GO TO 1069-EXIT.
02100
02101  1061-BENEFIT-DUMMY.
02102
02103  1061-DUMMY-EXIT.
02104      EXIT.
02105
02106  1062-NO-RECORD.
02107
02108      MOVE ER-2426                TO EMI-ERROR.
02109
02110      IF ELCNTL-REC-TYPE = '4'
02111          MOVE AL-UABON           TO BTYPE-ATTRB (1)
02112          MOVE -1                 TO BTYPE-LEN   (1)
02113      ELSE
02114          MOVE AL-UABON           TO BTYPE-ATTRB (2)
02115          MOVE -1                 TO BTYPE-LEN   (2).
02116
02117      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02118      MOVE 'Y'                    TO ERROR-SW.
02119
02120      GO TO 1069-EXIT.
02121
02122  1063-NO-MATCH-FOUND.
02123
02124      IF ELCNTL-REC-TYPE = '4'
02125          MOVE ER-2425            TO EMI-ERROR
02126          MOVE AL-UABON           TO BTYPE-ATTRB (1)
02127          MOVE -1                 TO BTYPE-LEN   (1)
02128      ELSE
02129          MOVE ER-2429            TO EMI-ERROR
02130          MOVE AL-UABON           TO BTYPE-ATTRB (2)
02131          MOVE -1                 TO BTYPE-LEN   (2).
02132
02133      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02134      MOVE 'Y'                    TO ERROR-SW.
02135      GO TO 1069-EXIT.
02136
02137  1069-EXIT.
02138      EXIT.
02139
02140      EJECT
02141
02142
02143 ******************************************************************
02144 *                                                                *
02145 *             C O N T R O L   F I L E   R E A D                  *
02146 *                                                                *
02147 ******************************************************************
02148
02149  1070-ELCNTL-READ.
02150
02151      
      * EXEC CICS HANDLE CONDITION
02152 *        NOTFND  (1078-NO-RECORD)
02153 *        ENDFILE (1078-NO-RECORD)
02154 *        END-EXEC.
      *    MOVE '"$I''                  ! # #00007276' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303037323736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02155
02156      IF ELCNTL-REC-TYPE = '1' OR '4' OR '5'
02157          
      * EXEC CICS READ
02158 *            DATASET (FILE-ID-ELCNTL)
02159 *            SET     (ELCNTL-POINTER)
02160 *            RIDFLD  (ELCNTL-KEY)
02161 *            GTEQ
02162 *            END-EXEC
      *    MOVE '&"S        G          (   #00007282' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
                 ELCNTL-POINTER, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02163      ELSE
02164          
      * EXEC CICS READ
02165 *            DATASET (FILE-ID-ELCNTL)
02166 *            SET     (ELCNTL-POINTER)
02167 *            RIDFLD  (ELCNTL-KEY)
02168 *            END-EXEC.
      *    MOVE '&"S        E          (   #00007289' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
                 ELCNTL-POINTER, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02169
02170      SERVICE RELOAD CONTROL-FILE.
02171
02172      GO TO 1079-EXIT.
02173
02174  1078-NO-RECORD.
02175
02176      MOVE ER-9999                TO EMI-ERROR.
02177
02178  1079-EXIT.
02179      EXIT.
02180
02181      EJECT
02182
02183
02184 ******************************************************************
02185 *                                                                *
02186 *   T E R M   C O N V E R S I O N   FOR   T E R M  IN  D A Y S   *
02187 *                                                                *
02188 ******************************************************************
02189
02190  1080-TERM-CONVERSION.
02191
02192      IF BMODE   = ' ' OR 'M'
02193          MOVE WS-BPMTS      TO WS-CALC-TERM-WHOLE
02194          GO TO 1085-ROUND-TERM.
02195
02196      IF BMODE   = 'S'
02197          COMPUTE WS-CALC-TERM = WS-BPMTS   / 2
02198          GO TO 1085-ROUND-TERM.
02199
02200      IF BMODE   = 'W'
02201          COMPUTE WS-CALC-TERM = WS-BPMTS   / 4.33333
02202          GO TO 1085-ROUND-TERM.
02203
02204      IF BMODE   = 'B'
02205          COMPUTE WS-CALC-TERM = WS-BPMTS   / 2.16667.
02206
02207  1085-ROUND-TERM.
02208
02209      IF WS-CALC-TERM-REMAIN GREATER THAN .00000
02210          ADD +1                  TO WS-CALC-TERM-WHOLE.
02211      MOVE ZEROS                  TO WS-CALC-TERM-REMAIN.
02212
02213      IF  BTYPE-LEN       (WS-SUB1)  GREATER THAN ZEROS
02214          IF  WS-KIND-MONTHLY
02215              IF BTERM-LEN    (WS-SUB1)  EQUAL ZEROS
02216                 IF BPREM-LEN (WS-SUB1)  GREATER THAN ZEROS
02217                    IF  BPMT-LEN   GREATER THAN ZEROS
02218                        NEXT SENTENCE
02219                    ELSE
02220                        GO TO 1087-EDIT-TERM.
02221
02222      IF  BMODE   = 'M'
02223          MOVE WS-BPMT            TO  BBENO      (WS-SUB1)
02224                                      WS-BBEN    (WS-SUB1)
02225          MOVE AL-UNNON           TO  BBEN-ATTRB (WS-SUB1)
02226          MOVE +12                TO  BBEN-LEN   (WS-SUB1)
02227          GO TO 1087-EDIT-TERM.
02228
02229      IF  BMODE   = 'W'
02230          COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 4.33333
02231          MOVE    WS-BBEN (WS-SUB1) TO BBENO       (WS-SUB1)
02232          MOVE AL-UNNON             TO  BBEN-ATTRB (WS-SUB1)
02233          MOVE +12                  TO  BBEN-LEN   (WS-SUB1)
02234          GO TO 1087-EDIT-TERM.
02235
02236      IF  BMODE   = 'S'
02237          COMPUTE WS-BBEN (WS-SUB1)  ROUNDED = WS-BPMT * 2
02238          MOVE    WS-BBEN (WS-SUB1)  TO BBENO      (WS-SUB1)
02239          MOVE AL-UNNON              TO BBEN-ATTRB (WS-SUB1)
02240          MOVE +12                   TO BBEN-LEN   (WS-SUB1)
02241          GO TO 1087-EDIT-TERM.
02242
02243      IF  BMODE   = 'B'
02244          COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 2.16667
02245          MOVE    WS-BBEN (WS-SUB1) TO BBENO       (WS-SUB1)
02246          MOVE +12                  TO BBEN-LEN    (WS-SUB1)
02247          MOVE AL-UNNON             TO BBEN-ATTRB  (WS-SUB1).
02248
02249  1087-EDIT-TERM.
02250
02251      IF BTERM-LEN (WS-SUB1)  = ZEROS
02252          MOVE WS-CALC-TERM-WHOLE   TO BTERMI      (WS-SUB1)
02253                                       WS-BTERM    (WS-SUB1)
02254          MOVE +3                   TO BTERM-LEN   (WS-SUB1)
02255          GO TO 1089-EXIT.
02256
02257      MOVE BTERMI    (WS-SUB1)   TO DEEDIT-FIELD.
02258      PERFORM 8600-DEEDIT.
02259      IF DEEDIT-FIELD-V0 NUMERIC
02260         MOVE DEEDIT-FIELD-V0    TO WS-BTERM    (WS-SUB1)
02261         IF WS-BTERM (WS-SUB1)  GREATER THAN ZERO
02262            MOVE AL-UNNON        TO BTERM-ATTRB (WS-SUB1)
02263         ELSE
02264            MOVE ER-2241         TO EMI-ERROR
02265            MOVE -1              TO BTERM-LEN   (WS-SUB1)
02266            MOVE AL-UNBOF        TO BTERM-ATTRB (WS-SUB1)
02267            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02268            GO TO 1089-EXIT.
02269
02270      IF WS-BTERM (WS-SUB1)   NOT = WS-CALC-TERM-WHOLE
02271          MOVE -1                   TO BTERM-LEN   (WS-SUB1)
02272          MOVE ER-2593              TO EMI-ERROR
02273          MOVE AL-UNBON             TO BTERM-ATTRB (WS-SUB1)
02274          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02275
02276  1089-EXIT.
02277      EXIT.
02278
02279      EJECT
02280
02281 ******************************************************************
02282 *                                                                *
02283 *         CALCULATE MONTHLY TERM AND BENEFIT                     *
02284 *                                                                *
02285 ******************************************************************
02286
02287  1090-CALCULATE-MONTHLY-TERM.
02288
02289      COMPUTE  WS-CALC-TERM = WS-BDAYS / 31.
02290
02291      IF WS-CALC-TERM-REMAIN GREATER THAN .00000
02292         ADD +1 TO WS-CALC-TERM.
02293
02294      MOVE ZEROS                  TO  WS-CALC-TERM-REMAIN.
02295      MOVE WS-CALC-TERM           TO  BTERMO    (2).
02296      MOVE +3                     TO  BTERM-LEN (2).
02297
02298      IF  BTYPE-LEN  (2)  GREATER THAN ZEROS
02299          IF BTERM-LEN (2)  EQUAL ZEROS
02300             IF BPREM-LEN (2) GREATER THAN ZEROS
02301                IF  BPMT-LEN   GREATER THAN ZEROS
02302                    NEXT SENTENCE
02303                ELSE
02304                    GO TO 1094-EXIT.
02305
02306      IF  BMODE   = 'M'
02307          MOVE WS-BPMT            TO  WS-BBEN     (2)
02308                                      BBENO       (2)
02309          MOVE AL-UNNON           TO  BBEN-ATTRB  (2)
02310          MOVE +12                TO  BBEN-LEN    (2)
02311          GO TO 1094-EXIT.
02312
02313      IF  BMODE   = 'W'
02314          COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 4.33333
02315          MOVE WS-BBEN    (WS-SUB1)   TO BBENO       (WS-SUB1)
02316          MOVE  AL-UNNON              TO  BBEN-ATTRB (WS-SUB1)
02317          MOVE  +12                   TO  BBEN-LEN   (WS-SUB1)
02318          GO TO 1094-EXIT.
02319
02320      IF  BMODE   = 'S'
02321          COMPUTE WS-BBEN (2)        ROUNDED = WS-BPMT * 2
02322          MOVE WS-BBEN    (2)         TO  BBENO      (2)
02323          MOVE  AL-UNNON              TO  BBEN-ATTRB (2)
02324          MOVE  +12                   TO  BBEN-LEN   (2)
02325          GO TO 1094-EXIT.
02326
02327      IF  BMODE   = 'B'
02328          COMPUTE WS-BBEN (2)       ROUNDED = WS-BPMT * 2.16667
02329          MOVE WS-BBEN    (2)         TO  BBENO      (2)
02330          MOVE +12                    TO  BBEN-LEN   (2)
02331          MOVE AL-UNNON               TO  BBEN-ATTRB (2).
02332
02333
02334  1094-EXIT.
02335      EXIT.
02336
02337      EJECT
02338
02339
02340 ******************************************************************
02341 *                                                                *
02342 *               CALCULATE INSURED'S AGE                          *
02343 *                                                                *
02344 ******************************************************************
02345
02346  1095-CALC-AGE.
02347
02348      MOVE WS-CONVERTED-BIRTH TO DC-BIN-DATE-1.
02349      MOVE WS-CURRENT-BIN-DT TO  DC-BIN-DATE-2.
02350      MOVE 1 TO DC-OPTION-CODE.
02351      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
02352      IF NO-CONVERSION-ERROR
02353          COMPUTE WS-BAGE = DC-ELAPSED-MONTHS / 12
02354          MOVE WS-BAGE            TO BAGE
02355          MOVE +2                 TO BAGE-LEN
02356          MOVE AL-UNNON TO BAGE-ATTRB .
02357
02358  1099-EXIT.
02359      EXIT.
02360
02361      EJECT
02362
02363 ******************************************************************
02364 *                                                                *
02365 *    E D I T   D A T A   E N T R Y   C A N C E L   S C R E E N   *
02366 *                                                                *
02367 ******************************************************************
02368
02369
02370  1100-EDIT-MAPC.
02371
02372      MOVE +1                     TO WS-SUB2.
02373
02374      IF PI-LAST-FUNC-DISPLAY
02375         AND CLAST-NAME-LEN (1)   = ZEROS
02376         AND CLAST-NAME-LEN (2)   = ZEROS
02377         AND CLAST-NAME-LEN (3)   = ZEROS
02378         AND CLAST-NAME-LEN (4)   = ZEROS
02379         AND CCANDT1-LEN     (1) = ZEROS
02380         AND CCANDT2-LEN     (1) = ZEROS
02381         AND CCANDT1-LEN     (2) = ZEROS
02382         AND CCANDT2-LEN     (2) = ZEROS
02383         AND CCANDT1-LEN     (3) = ZEROS
02384         AND CCANDT2-LEN     (3) = ZEROS
02385         AND CCANDT1-LEN     (4) = ZEROS
02386         AND CCANDT2-LEN     (4) = ZEROS
02387         AND CREFUND1-LEN    (1) = ZEROS
02388         AND CREFUND2-LEN    (1) = ZEROS
02389         AND CREFUND1-LEN    (2) = ZEROS
02390         AND CREFUND2-LEN    (2) = ZEROS
02391         AND CREFUND1-LEN    (3) = ZEROS
02392         AND CREFUND2-LEN    (3) = ZEROS
02393         AND CREFUND1-LEN    (4) = ZEROS
02394         AND CREFUND2-LEN    (4) = ZEROS
02395         AND CLIVES-LEN     (1) = ZEROS
02396         AND CLIVES-LEN     (2) = ZEROS
02397         AND CLIVES-LEN     (3) = ZEROS
02398         AND CLIVES-LEN     (4) = ZEROS
02399         AND CPAYEE-LEN     (1) = ZEROS
02400         AND CPAYEE-LEN     (2) = ZEROS
02401         AND CPAYEE-LEN     (3) = ZEROS
02402         AND CPAYEE-LEN     (4) = ZEROS
02403         AND CCHK-LEN       (1) = ZEROS
02404         AND CCHK-LEN       (2) = ZEROS
02405         AND CCHK-LEN       (3) = ZEROS
02406         AND CCHK-LEN       (4) = ZEROS
02407          GO TO 1130-NOTHING-TO-EDIT.
02408
02409  1110-EDIT-MAPC-LOOP.
02410
02411      IF CCERT-LEN       (WS-SUB2) = ZEROS
02412        AND CEFFDT-LEN   (WS-SUB2) = ZEROS
02413        AND CCANDT1-LEN  (WS-SUB2) = ZEROS
02414        AND CCANDT2-LEN  (WS-SUB2) = ZEROS
02415        AND CREFUND1-LEN (WS-SUB2) = ZEROS
02416        AND CREFUND2-LEN (WS-SUB2) = ZEROS
02417        AND NOT PI-LAST-FUNC-DISPLAY
02418          GO TO 1120-INCREMENT-OCCURANCE.
02419
02420      MOVE 'Y'                    TO WS-DATA-KEYED-SW.
02421
02422      MOVE AL-SABON               TO CSEQ-ATTRB (WS-SUB2).
02423
02424      IF NOT PI-LAST-FUNC-DISPLAY
02425         IF CCAR-LEN (WS-SUB2) GREATER THAN ZEROS
02426            MOVE AL-UANON           TO CCAR-ATTRB (WS-SUB2)
02427            PERFORM 1500-VERIFY-CARRIER-ID THRU 1590-EXIT
02428         ELSE
02429            IF NOT ST-ACCNT-CNTL AND NOT ACCNT-CNTL
02430               MOVE -1             TO CCAR-LEN   (WS-SUB2)
02431               MOVE AL-UABON       TO CCAR-ATTRB (WS-SUB2)
02432               MOVE ER-0194        TO EMI-ERROR
02433               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02434
02435      IF NOT PI-LAST-FUNC-DISPLAY
02436         IF CGRP-LEN (WS-SUB2) GREATER THAN ZEROS
02437            MOVE AL-UANON           TO CGRP-ATTRB (WS-SUB2)
02438         ELSE
02439            IF CARR-GROUP-ST-ACCNT-CNTL
02440               MOVE -1             TO CGRP-LEN      (WS-SUB2)
02441               MOVE AL-UABON       TO CGRP-ATTRB    (WS-SUB2)
02442               MOVE ER-0195        TO EMI-ERROR
02443               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02444
02445      IF NOT PI-LAST-FUNC-DISPLAY
02446         IF CST-LEN (WS-SUB2) GREATER THAN ZEROS
02447            MOVE AL-UANON           TO CST-ATTRB  (WS-SUB2)
02448            PERFORM 1600-VERIFY-STATE-ID THRU 1690-EXIT
02449         ELSE
02450            IF NOT ACCNT-CNTL AND NOT CARR-ACCNT-CNTL
02451               MOVE -1             TO CST-LEN    (WS-SUB2)
02452               MOVE AL-UABON       TO CST-ATTRB  (WS-SUB2)
02453               MOVE ER-0196        TO EMI-ERROR
02454               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02455
02456      IF NOT PI-LAST-FUNC-DISPLAY
02457         IF CACCT-LEN (WS-SUB2) GREATER THAN ZEROS
02458            MOVE AL-UANON           TO CACCT-ATTRB (WS-SUB2)
02459            PERFORM 1700-VERIFY-ACCOUNT THRU 1790-EXIT
02460         ELSE
02461            MOVE -1 TO CACCT-LEN      (WS-SUB2)
02462            MOVE AL-UABON           TO CACCT-ATTRB (WS-SUB2)
02463            MOVE ER-0197            TO EMI-ERROR
02464            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02465
02466
02467      IF CCERT-LEN (WS-SUB2) = ZEROS
02468        AND PI-LAST-FUNC-DISPLAY
02469          NEXT SENTENCE
02470      ELSE
02471          IF CCERT-LEN (WS-SUB2)  NOT = ZEROS
02472              MOVE AL-UANON       TO CCERT-ATTRB (WS-SUB2)
02473          ELSE
02474              MOVE -1             TO CCERT-LEN   (WS-SUB2)
02475              MOVE ER-2218        TO EMI-ERROR
02476              MOVE AL-UABON       TO CCERT-ATTRB (WS-SUB2)
02477              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02478
02479      IF CSFX-LEN (WS-SUB1)      GREATER THAN ZEROS
02480          MOVE 'Y'                TO PI-CAN-SUFFIX-KEYED-SW
02481          MOVE AL-UANON           TO CSFX-ATTRB (WS-SUB2).
02482
02483      IF CEFFDT-LEN (WS-SUB2) = ZEROS
02484        AND PI-LAST-FUNC-DISPLAY
02485          NEXT SENTENCE
02486      ELSE
02487          IF CEFFDT-LEN (WS-SUB2) GREATER THAN ZEROS
02488              MOVE AL-UNNON           TO CEFFDT-ATTRB (WS-SUB2)
02489              IF CEFFDT (WS-SUB2) NUMERIC
02490                  MOVE 4              TO DC-OPTION-CODE
02491                  MOVE CEFFDT (WS-SUB2)  TO DC-GREG-DATE-1-MDY
02492                  PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
02493                  MOVE DC-BIN-DATE-1  TO WS-CONVERTED-EFFDT
02494                  IF NO-CONVERSION-ERROR
02495                      IF WS-CONVERTED-EFFDT NOT LESS THAN
02496                        PI-ACCT-LOW-EFF-DT AND LESS THAN
02497                        PI-ACCT-HIGH-EXP-DT
02498                          NEXT SENTENCE
02499                      ELSE
02500                          MOVE -1       TO CEFFDT-LEN (WS-SUB2)
02501                          MOVE ER-2589  TO EMI-ERROR
02502                          MOVE AL-UNBON TO CEFFDT-ATTRB (WS-SUB2)
02503                          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02504                  ELSE
02505                      MOVE -1         TO CEFFDT-LEN (WS-SUB2)
02506                      MOVE ER-2226    TO EMI-ERROR
02507                      MOVE AL-UNBON   TO CEFFDT-ATTRB (WS-SUB2)
02508                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02509              ELSE
02510                  MOVE -1             TO CEFFDT-LEN (WS-SUB2)
02511                  MOVE ER-2223        TO EMI-ERROR
02512                  MOVE AL-UNBON       TO CEFFDT-ATTRB (WS-SUB2)
02513                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02514          ELSE
02515              MOVE -1                 TO CEFFDT-LEN (WS-SUB2)
02516              MOVE ER-2220            TO EMI-ERROR
02517              MOVE AL-UNBON           TO CEFFDT-ATTRB (WS-SUB2)
02518              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02519
02520      IF CLAST-NAME-LEN (WS-SUB2) GREATER THAN ZEROS
02521          MOVE AL-UANON           TO CLAST-NAME-ATTRB (WS-SUB2).
02522
02523
02524
02525      EJECT
02526
02527 ******************************************************************
02528 *                                                                *
02529 *           E D I T   C A N C E L   C O V E R A G E S            *
02530 *                                                                *
02531 ******************************************************************
02532
02533  1115-EDIT-COVERAGES.
02534
02535      IF CCANDT1-LEN (WS-SUB2) = ZEROS
02536        AND PI-LAST-FUNC-DISPLAY
02537          NEXT SENTENCE
02538      ELSE
02539          IF CCANDT1-LEN (WS-SUB2) GREATER THAN ZEROS
02540              MOVE AL-UNNON       TO CCANDT1-ATTRB (WS-SUB2)
02541              IF PI-LAST-FUNC-DISPLAY AND
02542                 CCANDT1 (WS-SUB2) = SPACES
02543                  MOVE LOW-VALUES TO WS-CONVERTED-CANDT1 (WS-SUB2)
02544              ELSE
02545                 IF CCANDT1 (WS-SUB2) NUMERIC
02546                    MOVE 4              TO DC-OPTION-CODE
02547                    MOVE CCANDT1 (WS-SUB2) TO
02548                                       DC-GREG-DATE-1-MDY
02549                    PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
02550                    MOVE DC-BIN-DATE-1  TO WS-CONVERTED-CANDT1
02551                                                          (WS-SUB2)
02552                    IF NO-CONVERSION-ERROR
02553                       NEXT SENTENCE
02554                    ELSE
02555                       MOVE -1       TO CCANDT1-LEN   (WS-SUB2)
02556                       MOVE ER-2227  TO EMI-ERROR
02557                       MOVE AL-UNBON TO CCANDT1-ATTRB (WS-SUB2)
02558                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02559                 ELSE
02560                    MOVE -1         TO CCANDT1-LEN   (WS-SUB2)
02561                    MOVE ER-2223    TO EMI-ERROR
02562                    MOVE AL-UNBON   TO CCANDT1-ATTRB (WS-SUB2)
02563                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02564          ELSE
02565              IF CREFUND1-LEN (WS-SUB2) GREATER THAN ZEROS
02566                 MOVE -1             TO CCANDT1-LEN   (WS-SUB2)
02567                 MOVE ER-2222        TO EMI-ERROR
02568                 MOVE AL-UNBOF       TO CCANDT1-ATTRB (WS-SUB2)
02569                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02570
02571
02572      IF CREFUND1-LEN (WS-SUB2) = ZEROS
02573        AND PI-LAST-FUNC-DISPLAY
02574          NEXT SENTENCE
02575      ELSE
02576          IF CREFUND1-LEN (WS-SUB2) NOT = ZEROS
02577             MOVE AL-UNNON       TO CREFUND1-ATTRB (WS-SUB2)
02578             MOVE CREFUND1I (WS-SUB2) TO DEEDIT-FIELD-X11
02579             PERFORM 8600-DEEDIT
02580             MOVE DEEDIT-FIELD-V2  TO WS-CREFUND1 (WS-SUB2).
02581
02582      IF CCANDT2-LEN (WS-SUB2) = ZEROS
02583        AND PI-LAST-FUNC-DISPLAY
02584          NEXT SENTENCE
02585      ELSE
02586          IF CCANDT2-LEN (WS-SUB2) GREATER THAN ZEROS
02587              MOVE AL-UNNON       TO CCANDT2-ATTRB (WS-SUB2)
02588              IF PI-LAST-FUNC-DISPLAY AND
02589                 CCANDT2 (WS-SUB2) = SPACES
02590                  MOVE LOW-VALUES TO WS-CONVERTED-CANDT2 (WS-SUB2)
02591              ELSE
02592                 IF CCANDT2 (WS-SUB2) NUMERIC
02593                    MOVE 4              TO DC-OPTION-CODE
02594                    MOVE CCANDT2 (WS-SUB2) TO
02595                                       DC-GREG-DATE-1-MDY
02596                    PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
02597                    MOVE DC-BIN-DATE-1  TO WS-CONVERTED-CANDT2
02598                                                          (WS-SUB2)
02599                    IF NO-CONVERSION-ERROR
02600                       NEXT SENTENCE
02601                    ELSE
02602                       MOVE -1       TO CCANDT2-LEN   (WS-SUB2)
02603                       MOVE ER-2227  TO EMI-ERROR
02604                       MOVE AL-UNBON TO CCANDT2-ATTRB (WS-SUB2)
02605                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02606                 ELSE
02607                    MOVE -1         TO CCANDT2-LEN   (WS-SUB2)
02608                    MOVE ER-2223    TO EMI-ERROR
02609                    MOVE AL-UNBON   TO CCANDT2-ATTRB (WS-SUB2)
02610                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02611          ELSE
02612              IF CREFUND2-LEN (WS-SUB2) GREATER THAN ZEROS
02613                 MOVE -1             TO CCANDT2-LEN   (WS-SUB2)
02614                 MOVE ER-2222        TO EMI-ERROR
02615                 MOVE AL-UNBOF       TO CCANDT2-ATTRB (WS-SUB2)
02616                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02617
02618
02619      IF CREFUND2-LEN (WS-SUB2) = ZEROS
02620        AND PI-LAST-FUNC-DISPLAY
02621          NEXT SENTENCE
02622      ELSE
02623          IF CREFUND2-LEN (WS-SUB2) NOT = ZEROS
02624             MOVE AL-UNNON       TO CREFUND2-ATTRB (WS-SUB2)
02625             MOVE CREFUND2I (WS-SUB2) TO DEEDIT-FIELD-X11
02626             PERFORM 8600-DEEDIT
02627             MOVE DEEDIT-FIELD-V2  TO WS-CREFUND2 (WS-SUB2).
02628
02629      IF CLIVES-LEN  (WS-SUB2)  GREATER THAN ZEROS
02630          MOVE 'Y'                TO PI-CAN-LIVES-KEYED-SW
02631          MOVE CLIVESI (WS-SUB2 ) TO DEEDIT-FIELD
02632          PERFORM 8600-DEEDIT
02633          IF DEEDIT-FIELD-V0 NUMERIC
02634              MOVE DEEDIT-FIELD-V0 TO WS-CLIVES   (WS-SUB2)
02635              MOVE AL-UNNON       TO CLIVES-ATTRB (WS-SUB2)
02636          ELSE
02637              MOVE -1             TO CLIVES-LEN   (WS-SUB2)
02638              MOVE AL-UNBON       TO CLIVES-ATTRB (WS-SUB2)
02639              MOVE ER-2223        TO EMI-ERROR
02640              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02641
02642      IF CPAYEE-LEN  (WS-SUB2)  GREATER THAN ZEROS
02643         MOVE 'Y'                 TO PI-PAYEE-KEYED-SW.
02644
02645      IF CCHK-LEN    (WS-SUB2)  GREATER THAN ZEROS
02646         MOVE 'Y'                 TO PI-CHK-REQ-KEYED-SW
02647         IF  CCHK    (WS-SUB2)  = 'R' OR ' '
02648             NEXT SENTENCE
02649         ELSE
02650             MOVE ER-7405         TO EMI-ERROR
02651             MOVE -1              TO CCHK-LEN      (WS-SUB2)
02652             MOVE AL-UABON        TO CCHK-ATTRB    (WS-SUB2)
02653             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02654
02655      IF PI-LAST-FUNC-DISPLAY
02656         NEXT SENTENCE
02657      ELSE
02658         IF CCANDT1-LEN (WS-SUB2) GREATER THAN ZEROS OR
02659            CCANDT2-LEN (WS-SUB2) GREATER THAN ZEROS
02660            NEXT SENTENCE
02661         ELSE
02662            MOVE ER-2222          TO EMI-ERROR
02663            MOVE -1               TO CCANDT1-LEN   (WS-SUB2)
02664            MOVE AL-UNBOF         TO CCANDT1-ATTRB (WS-SUB2)
02665            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02666
02667
02668      IF  EMI-ERROR = ZEROS
02669          MOVE 'Y'                TO PI-UPDATE-SW
02670          MOVE SPACE              TO PI-DISPLAY-SW
02671          PERFORM 5000-BUILD-CANCEL-RECORD THRU 5900-EXIT
02672      ELSE
02673          MOVE ZEROS              TO EMI-ERROR
02674          MOVE 'Y'                TO PI-ERROR-SW.
02675
02676  1120-INCREMENT-OCCURANCE.
02677
02678      ADD +1                      TO WS-SUB2.
02679
02680      IF WS-SUB2 GREATER THAN +2 OR PI-LAST-FUNC-DISPLAY
02681          NEXT SENTENCE
02682      ELSE
02683          GO TO 1110-EDIT-MAPC-LOOP.
02684
02685
02686  1130-NOTHING-TO-EDIT.
02687
02688
02689      IF PI-DATA-ERRORS
02690          MOVE AL-SABON           TO CSEQ-ATTRB (1) CSEQ-ATTRB (2)
02691                                     CSEQ-ATTRB (3) CSEQ-ATTRB (4)
02692         GO TO 8100-SEND-INITIAL-MAP.
02693
02694      IF NOT PI-LAST-FUNC-DISPLAY
02695         AND WS-DATA-NOT-KEYED
02696         MOVE ER-7400             TO EMI-ERROR
02697         MOVE -1                  TO CCANDT1-LEN (1)
02698         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02699         MOVE 'Y'                 TO PI-ERROR-SW
02700         GO TO 8200-SEND-DATAONLY.
02701
02702      IF PI-SAV-BATCH-SEQ LESS THAN PI-LAST-SEQ-NO-ADDED
02703         SUBTRACT 1 FROM PI-SAV-BATCH-SEQ
02704         MOVE ER-0000            TO EMI-ERROR
02705         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02706         GO TO 2000-BROWSE-FWD.
02707
02708      MOVE LOW-VALUES     TO DATA-ENTRY-MAP.
02709
02710      ADD +1, PI-LAST-SEQ-NO-ADDED
02711                             GIVING PI-NEXT-DISPLAY-SEQ-NO.
02712
02713      MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ.
02714
02715      PERFORM 8550-SET-MAP-SEQ-NOS
02716                   VARYING WS-SUB2 FROM +1 BY +1
02717                   UNTIL WS-SUB2 GREATER THAN +2.
02718
02719      MOVE ER-0000            TO EMI-ERROR.
02720      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02721      GO TO 8100-SEND-INITIAL-MAP.
02722
02723      EJECT
02724
02725 ******************************************************************
02726 *                                                                *
02727 *           V E R I F Y   C A R R I E R   I D                    *
02728 *                                                                *
02729 ******************************************************************
02730
02731  1500-VERIFY-CARRIER-ID.
02732
02733      MOVE SPACES                 TO ELCNTL-KEY.
02734      MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.
02735      MOVE '6'                    TO ELCNTL-REC-TYPE.
02736
02737      IF PI-MAP-NAME = EL930B
02738         MOVE BCAR                TO ELCNTL-CARRIER
02739      ELSE
02740         MOVE CCAR (WS-SUB2)      TO ELCNTL-CARRIER.
02741
02742      MOVE +0                     TO ELCNTL-SEQ.
02743
02744      
      * EXEC CICS HANDLE CONDITION
02745 *        NOTFND   (1580-NO-CARRIER)
02746 *        END-EXEC.
      *    MOVE '"$I                   ! $ #00007869' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303037383639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02747
02748      
      * EXEC CICS READ
02749 *        DATASET   (FILE-ID-ELCNTL)
02750 *        SET       (ELCNTL-POINTER)
02751 *        RIDFLD    (ELCNTL-KEY)
02752 *        END-EXEC.
      *    MOVE '&"S        E          (   #00007873' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
                 ELCNTL-POINTER, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02753      SERVICE RELOAD CONTROL-FILE.
02754      GO TO 1590-EXIT.
02755
02756  1580-NO-CARRIER.
02757
02758      IF PI-MAP-NAME = EL930B
02759         MOVE -1                  TO BCARL
02760         MOVE AL-UABON            TO BCARA
02761      ELSE
02762         MOVE AL-UABON            TO CCAR-ATTRB (WS-SUB2)
02763         MOVE -1                  TO CCAR-LEN   (WS-SUB2).
02764
02765      MOVE ER-2208                TO EMI-ERROR.
02766      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02767
02768  1590-EXIT.
02769      EXIT.
02770
02771      EJECT
02772
02773 ******************************************************************
02774 *                                                                *
02775 *               V E R I F Y   S T A T E  I D                     *
02776 *                                                                *
02777 ******************************************************************
02778
02779  1600-VERIFY-STATE-ID.
02780
02781      MOVE SPACES                 TO ELCNTL-KEY.
02782      MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.
02783      MOVE '3'                    TO ELCNTL-REC-TYPE.
02784
02785      IF PI-MAP-NAME = EL930B
02786         MOVE BST                 TO ELCNTL-STATE
02787      ELSE
02788         MOVE CST  (WS-SUB2)      TO ELCNTL-STATE.
02789
02790      MOVE +0                     TO ELCNTL-SEQ.
02791
02792      
      * EXEC CICS HANDLE CONDITION
02793 *        NOTFND   (1680-NO-STATE)
02794 *        END-EXEC.
      *    MOVE '"$I                   ! % #00007917' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303037393137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02795
02796      
      * EXEC CICS READ
02797 *        DATASET   (FILE-ID-ELCNTL)
02798 *        SET       (ELCNTL-POINTER)
02799 *        RIDFLD    (ELCNTL-KEY)
02800 *        END-EXEC.
      *    MOVE '&"S        E          (   #00007921' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
                 ELCNTL-POINTER, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02801
02802      SERVICE RELOAD CONTROL-FILE.
02803
02804      GO TO 1690-EXIT.
02805
02806  1680-NO-STATE.
02807
02808      IF PI-MAP-NAME = EL930B
02809         MOVE -1                  TO BST-LEN
02810         MOVE AL-UABON            TO BST-ATTRB
02811      ELSE
02812         MOVE AL-UABON            TO CST-LEN (WS-SUB2)
02813         MOVE -1                  TO CST-LEN (WS-SUB2).
02814
02815      MOVE ER-2209                TO EMI-ERROR.
02816      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02817
02818  1690-EXIT.
02819
02820      EXIT.
02821
02822      EJECT
02823
02824 ******************************************************************
02825 *                                                                *
02826 *               V E R I F Y   A C C O U N T                      *
02827 *                                                                *
02828 ******************************************************************
02829
02830  1700-VERIFY-ACCOUNT.
02831
02832      IF PI-MAP-NAME NOT = EL930B
02833         GO TO 1710-VERIFY-CANCEL-ACCOUNT.
02834
02835      IF BCAR-LEN GREATER THAN ZEROS
02836         MOVE BCAR                TO ERACCT-CARRIER
02837      ELSE
02838          MOVE SPACES             TO ERACCT-CARRIER.
02839
02840      IF BGRP-LEN GREATER THAN ZEROS
02841          MOVE BGRP               TO ERACCT-GROUPING
02842      ELSE
02843          MOVE SPACES             TO ERACCT-GROUPING.
02844
02845      IF BST-LEN GREATER THAN ZEROS
02846          MOVE BST                TO ERACCT-STATE
02847      ELSE
02848          MOVE SPACES             TO ERACCT-STATE.
02849
02850      MOVE BACCT                  TO ERACCT-ACCOUNT.
02851
02852      GO TO 1720-READ-ACCOUNT.
02853
02854  1710-VERIFY-CANCEL-ACCOUNT.
02855
02856      IF CCAR-LEN  (WS-SUB2) GREATER THAN ZEROS
02857         MOVE CCAR (WS-SUB2)      TO ERACCT-CARRIER
02858      ELSE
02859          MOVE SPACES             TO ERACCT-CARRIER.
02860
02861      IF CGRP-LEN   (WS-SUB2) GREATER THAN ZEROS
02862          MOVE CGRP (WS-SUB2)     TO ERACCT-GROUPING
02863      ELSE
02864          MOVE SPACES             TO ERACCT-GROUPING.
02865
02866      IF CST-LEN    (WS-SUB2) GREATER THAN ZEROS
02867          MOVE CST  (WS-SUB2)     TO ERACCT-STATE
02868      ELSE
02869          MOVE SPACES             TO ERACCT-STATE.
02870
02871      MOVE CACCT (WS-SUB2)        TO ERACCT-ACCOUNT.
02872
02873
02874  1720-READ-ACCOUNT.
02875
02876      MOVE PI-COMPANY-CD          TO ERACCT-CO.
02877      MOVE '0'                    TO WS-FIRST-TIME-SW.
02878      MOVE LOW-VALUES             TO ERACCT-EXP-DATE
02879                                     PI-ACCT-LOW-EFF-DT
02880                                     PI-ACCT-HIGH-EXP-DT.
02881
02882      
      * EXEC CICS HANDLE CONDITION
02883 *        NOTFND   (1780-ACCOUNT-INVALID)
02884 *        ENDFILE  (1780-ACCOUNT-INVALID)
02885 *        END-EXEC.
      *    MOVE '"$I''                  ! & #00008007' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303038303037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02886
02887      
      * EXEC CICS STARTBR
02888 *        DATASET (FILE-ID-ERACCT2)
02889 *        RIDFLD  (ERACCT-KEY)
02890 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00008012' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERACCT2, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02891
02892      MOVE 'Y'                    TO WS-BROWSE-SW.
02893
02894      MOVE ERACCT-KEY             TO ERACCT-SAVE-KEY.
02895
02896  1750-READ-LOOP.
02897
02898      
      * EXEC CICS READNEXT
02899 *        DATASET   (FILE-ID-ERACCT2)
02900 *        SET       (ERACCT-POINTER)
02901 *        RIDFLD    (ERACCT-KEY)
02902 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00008023' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERACCT2, 
                 ERACCT-POINTER, 
                 DFHEIV99, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02903
02904      SERVICE RELOAD ACCOUNT-MASTER.
02905
02906      IF ERACCT-COMP-KEY NOT = ERACCT-SAVE-KEY
02907          GO TO 1780-ACCOUNT-INVALID.
02908
02909      IF FIRST-TIME
02910          MOVE '1'                TO WS-FIRST-TIME-SW
02911          MOVE AM-VG-CARRIER      TO PI-SAV-CARRIER
02912          MOVE AM-VG-GROUPING     TO PI-SAV-GROUPING
02913          MOVE AM-VG-STATE        TO PI-SAV-STATE
02914          MOVE AM-VG-ACCOUNT      TO PI-SAV-ACCOUNT
02915          MOVE AM-EFFECTIVE-DT    TO PI-ACCT-LOW-EFF-DT.
02916
02917      MOVE AM-EXPIRATION-DT       TO PI-ACCT-HIGH-EXP-DT.
02918
02919      GO TO 1750-READ-LOOP.
02920
02921  1780-ACCOUNT-INVALID.
02922
02923      IF BROWSE-STARTED
02924         MOVE SPACE  TO WS-BROWSE-SW
02925         
      * EXEC CICS ENDBR
02926 *            DATASET (FILE-ID-ERACCT2)
02927 *            END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008050' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERACCT2, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02928
02929      IF PI-ACCT-LOW-EFF-DT NOT = LOW-VALUES
02930         GO TO 1790-EXIT.
02931
02932      IF PI-MAP-NAME NOT = EL930B
02933         GO TO 1785-SET-CANCEL-CONTROL.
02934
02935      IF CARR-GROUP-ST-ACCNT-CNTL
02936          MOVE -1                     TO BCAR-LEN
02937          MOVE AL-UABON               TO BCAR-ATTRB
02938                                         BGRP-ATTRB
02939                                         BST-ATTRB
02940                                         BACCT-ATTRB
02941      ELSE
02942          IF ST-ACCNT-CNTL
02943              MOVE -1                 TO BST-LEN
02944              MOVE AL-UABON           TO BST-ATTRB
02945                                         BACCT-ATTRB
02946          ELSE
02947              IF CARR-ST-ACCNT-CNTL
02948                  MOVE -1             TO BCAR-LEN
02949                  MOVE AL-UABON       TO BCAR-ATTRB
02950                                         BST-ATTRB
02951                                         BACCT-ATTRB
02952              ELSE
02953                  IF ACCNT-CNTL
02954                      MOVE -1         TO BACCT-LEN
02955                      MOVE AL-UABON   TO BACCT-ATTRB
02956                  ELSE
02957                      MOVE -1         TO BCAR-LEN
02958                      MOVE AL-UABON   TO BCAR-ATTRB
02959                                         BACCT-ATTRB.
02960
02961      MOVE ER-2210                TO EMI-ERROR.
02962
02963      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02964
02965      GO TO 8200-SEND-DATAONLY.
02966
02967  1785-SET-CANCEL-CONTROL.
02968
02969      IF CARR-GROUP-ST-ACCNT-CNTL
02970          MOVE -1                     TO CCAR-LEN       (WS-SUB2)
02971          MOVE AL-UABON               TO CCAR-ATTRB     (WS-SUB2)
02972                                         CGRP-ATTRB     (WS-SUB2)
02973                                         CST-ATTRB      (WS-SUB2)
02974                                         CACCT-ATTRB (WS-SUB2)
02975      ELSE
02976          IF ST-ACCNT-CNTL
02977              MOVE -1                 TO CST-LEN        (WS-SUB2)
02978              MOVE AL-UABON           TO CST-ATTRB      (WS-SUB2)
02979                                         CACCT-ATTRB (WS-SUB2)
02980          ELSE
02981              IF CARR-ST-ACCNT-CNTL
02982                  MOVE -1             TO CCAR-LEN       (WS-SUB2)
02983                  MOVE AL-UABON       TO CCAR-ATTRB     (WS-SUB2)
02984                                         CST-ATTRB      (WS-SUB2)
02985                                         CACCT-ATTRB (WS-SUB2)
02986              ELSE
02987                  IF ACCNT-CNTL
02988                      MOVE -1         TO CACCT-LEN      (WS-SUB2)
02989                      MOVE AL-UABON   TO CACCT-ATTRB (WS-SUB2)
02990                  ELSE
02991                      MOVE -1         TO CCAR-LEN     (WS-SUB2)
02992                      MOVE AL-UABON   TO CCAR-ATTRB     (WS-SUB2)
02993                                         CACCT-ATTRB (WS-SUB2).
02994
02995      MOVE ER-2210                TO EMI-ERROR.
02996
02997      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02998
02999      GO TO 8200-SEND-DATAONLY.
03000
03001  1790-EXIT.
03002
03003      EXIT.
03004
03005      EJECT
03006
03007 ******************************************************************
03008 *                                                                *
03009 *                   B R O W S E   F O R W A R D                  *
03010 *                                                                *
03011 ******************************************************************
03012
03013  2000-BROWSE-FWD.
03014
03015      MOVE LOW-VALUES             TO DATA-ENTRY-MAP.
03016
03017      ADD +1                      TO PI-SAV-BATCH-SEQ.
03018
03019      
      * EXEC CICS HANDLE CONDITION
03020 *        NOTFND (2020-END-FILE)
03021 *        END-EXEC.
      *    MOVE '"$I                   ! '' #00008144' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303038313434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03022
03023      
      * EXEC CICS READ
03024 *        SET     (ERPNDT-POINTER)
03025 *        DATASET (FILE-ID-ERPNDT)
03026 *        RIDFLD  (PI-SAV-ENDING-ERPNDT-KEY)
03027 *        GTEQ
03028 *        END-EXEC.
      *    MOVE '&"S        G          (   #00008148' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDT, 
                 ERPNDT-POINTER, 
                 DFHEIV99, 
                 PI-SAV-ENDING-ERPNDT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03029
03030      SERVICE RELOAD PENDING-BUSINESS.
03031
03032      IF PB-COMPANY-CD = PI-SAV-COMP-CD
03033        AND PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH
03034          NEXT SENTENCE
03035      ELSE
03036          GO TO 2020-END-FILE.
03037
03038      IF PB-BATCH-TRAILER
03039          GO TO 2020-END-FILE.
03040
03041      MOVE PB-BATCH-SEQ-NO        TO PI-SAV-BATCH-SEQ.
03042
03043      IF PB-ISSUE
03044          MOVE EL930B             TO PI-MAP-NAME
03045          MOVE AL-SANOF           TO BDELHDGA
03046          PERFORM 7000-FORMAT-ISSUE-SCREEN THRU 7090-EXIT
03047      ELSE
03048          MOVE EL930C             TO PI-MAP-NAME
03049          MOVE AL-SANOF           TO CDELHDGA
03050          PERFORM 7100-FORMAT-CANCEL-SCREEN THRU 7190-EXIT.
03051
03052  2010-SEND-MAP.
03053
03054      GO TO 8100-SEND-INITIAL-MAP.
03055
03056  2020-END-FILE.
03057
03058      MOVE SPACE                  TO PI-DISPLAY-SW.
03059
03060      IF PI-MAP-NAME = EL930B
03061          MOVE LOW-VALUES         TO DATA-ENTRY-MAP
03062          MOVE -1                 TO BPFENTRL
03063          ADD +1, PI-LAST-SEQ-NO-ADDED
03064                GIVING PI-NEXT-DISPLAY-SEQ-NO
03065          PERFORM 8550-SET-MAP-SEQ-NOS
03066      ELSE
03067          MOVE LOW-VALUES         TO DATA-ENTRY-MAP
03068          MOVE -1                 TO CPFENTRL
03069          ADD +1, PI-LAST-SEQ-NO-ADDED
03070                GIVING PI-NEXT-DISPLAY-SEQ-NO
03071          PERFORM 8550-SET-MAP-SEQ-NOS
03072                  VARYING WS-SUB2 FROM +1 BY +1
03073                  UNTIL WS-SUB2 GREATER THAN +2.
03074
03075      MOVE ER-2217                TO EMI-ERROR.
03076      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03077      GO TO 2010-SEND-MAP.
03078
03079      EJECT
03080
03081 ******************************************************************
03082 *                                                                *
03083 *                B R O W S E   B A C K W A R D                   *
03084 *                                                                *
03085 ******************************************************************
03086
03087  2100-BROWSE-BKWD.
03088
03089      MOVE LOW-VALUES             TO DATA-ENTRY-MAP.
03090
03091      SUBTRACT +1             FROM PI-SAV-BATCH-SEQ.
03092
03093      IF PI-SAV-BATCH-SEQ NOT GREATER THAN +0
03094          GO TO 2120-END-FILE.
03095
03096      
      * EXEC CICS HANDLE CONDITION
03097 *        NOTFND (2100-BROWSE-BKWD)
03098 *        END-EXEC.
      *    MOVE '"$I                   ! ( #00008221' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303038323231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03099
03100      
      * EXEC CICS READ
03101 *        SET     (ERPNDT-POINTER)
03102 *        DATASET (FILE-ID-ERPNDT)
03103 *        RIDFLD  (PI-SAV-ENDING-ERPNDT-KEY)
03104 *        END-EXEC.
      *    MOVE '&"S        E          (   #00008225' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDT, 
                 ERPNDT-POINTER, 
                 DFHEIV99, 
                 PI-SAV-ENDING-ERPNDT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03105
03106      SERVICE RELOAD PENDING-BUSINESS.
03107
03108      IF PB-COMPANY-CD     = PI-SAV-COMP-CD
03109        AND PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH
03110          NEXT SENTENCE
03111      ELSE
03112          GO TO 2120-END-FILE.
03113
03114      IF PB-BATCH-TRAILER
03115          GO TO 2120-END-FILE.
03116
03117      IF PB-ISSUE
03118          MOVE EL930B             TO PI-MAP-NAME
03119          MOVE AL-SANOF           TO BDELHDGA
03120          PERFORM 7000-FORMAT-ISSUE-SCREEN THRU 7090-EXIT
03121      ELSE
03122          MOVE EL930C             TO PI-MAP-NAME
03123          MOVE AL-SANOF           TO CDELHDGA
03124          PERFORM 7100-FORMAT-CANCEL-SCREEN THRU 7190-EXIT.
03125
03126  2110-SEND-MAP.
03127      GO TO 8100-SEND-INITIAL-MAP.
03128
03129  2120-END-FILE.
03130
03131      MOVE ER-2431                TO EMI-ERROR.
03132      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03133      MOVE ER-0000                TO EMI-ERROR.
03134      MOVE ZEROS                  TO PI-SAV-BATCH-SEQ.
03135      GO TO 2000-BROWSE-FWD.
03136
03137      EJECT
03138
03139 ******************************************************************
03140 *                                                                *
03141 *            C O N T I U E   B A T C H   E N T R Y               *
03142 *                                                                *
03143 ******************************************************************
03144
03145  3000-CONTINUE-ENTRY.
03146
03147      MOVE PI-SAV-ENDING-ERPNDT-KEY TO ERPNDT-KEY.
03148
03149      
      * EXEC CICS HANDLE CONDITION
03150 *        NOTFND (3300-REC-NOT-FND)
03151 *        END-EXEC.
      *    MOVE '"$I                   ! ) #00008274' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303038323734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03152
03153      
      * EXEC CICS STARTBR
03154 *        DATASET (FILE-ID-ERPNDT)
03155 *        RIDFLD  (ERPNDT-KEY)
03156 *        GTEQ
03157 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00008278' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDT, 
                 ERPNDT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03158
03159  3100-READ-LOOP.
03160
03161      
      * EXEC CICS HANDLE CONDITION
03162 *        ENDFILE (3200-END-BROWSE)
03163 *        END-EXEC.
      *    MOVE '"$''                   ! * #00008286' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303038323836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03164
03165      
      * EXEC CICS READNEXT
03166 *        SET     (ERPNDT-POINTER)
03167 *        DATASET (FILE-ID-ERPNDT)
03168 *        RIDFLD  (ERPNDT-KEY)
03169 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00008290' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDT, 
                 ERPNDT-POINTER, 
                 DFHEIV99, 
                 ERPNDT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03170
03171      SERVICE RELOAD PENDING-BUSINESS.
03172
03173      IF PB-COMPANY-CD     = PI-SAV-COMP-CD
03174        AND PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH
03175          NEXT SENTENCE
03176      ELSE
03177          GO TO 3200-END-BROWSE.
03178
03179      IF NOT PB-BATCH-TRAILER
03180          GO TO 3110-NOT-BATCH-TRAILER.
03181
03182  3105-PRIME-PI-COUNTS.
03183
03184      IF PI-LF-ISS-REMITTED = ZEROS
03185          MOVE PB-B-LF-ISS-PRM-REMITTED  TO PI-LF-ISS-REMITTED.
03186
03187      IF PI-AH-ISS-REMITTED = ZEROS
03188          MOVE PB-B-AH-ISS-PRM-REMITTED  TO PI-AH-ISS-REMITTED.
03189
03190      IF PI-ISS-CNT-REMITTED = ZEROS
03191          MOVE PB-B-ISSUE-CNT-REMITTED   TO PI-ISS-CNT-REMITTED.
03192
03193      IF PI-CAN-CNT-REMITTED = ZEROS
03194          MOVE PB-B-CANCEL-CNT-REMITTED  TO PI-CAN-CNT-REMITTED.
03195
03196      IF PI-LF-CAN-REMITTED = ZEROS
03197          MOVE PB-B-LF-CAN-PRM-REMITTED  TO PI-LF-CAN-REMITTED.
03198
03199      IF PI-AH-CAN-REMITTED = ZEROS
03200          MOVE PB-B-AH-CAN-PRM-REMITTED  TO PI-AH-CAN-REMITTED.
03201
03202      GO TO 3200-END-BROWSE.
03203
03204  3110-NOT-BATCH-TRAILER.
03205
03206      IF PB-ISSUE
03207          ADD PB-I-LF-PREMIUM-AMT     TO PI-LF-ISS-ENTERED
03208          ADD PB-I-LF-ALT-PREMIUM-AMT TO PI-LF-ISS-ENTERED
03209          ADD PB-I-AH-PREMIUM-AMT     TO PI-AH-ISS-ENTERED
03210          ADD +1                      TO PI-ISS-CNT-ENTERED
03211      ELSE
03212          ADD PB-C-LF-CANCEL-AMT      TO PI-LF-CAN-ENTERED
03213          ADD PB-C-AH-CANCEL-AMT      TO PI-AH-CAN-ENTERED
03214          ADD +1                      TO PI-CAN-CNT-ENTERED.
03215      MOVE PB-BATCH-SEQ-NO            TO PI-LAST-SEQ-NO-ADDED
03216                                         PI-SAV-BATCH-SEQ.
03217      GO TO 3100-READ-LOOP.
03218
03219  3200-END-BROWSE.
03220
03221      
      * EXEC CICS ENDBR
03222 *        DATASET (FILE-ID-ERPNDT)
03223 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008346' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDT, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03224
03225      ADD +1, PI-LAST-SEQ-NO-ADDED
03226                             GIVING PI-NEXT-DISPLAY-SEQ-NO.
03227
03228      IF PI-MAINT-FUNC = 'B'
03229          MOVE ZEROS              TO PI-SAV-BATCH-SEQ
03230          GO TO 2000-BROWSE-FWD
03231      ELSE
03232          ADD +1                  TO PI-SAV-BATCH-SEQ.
03233
03234      IF PI-LF-ISS-REMITTED = ZEROS
03235        AND PI-AH-ISS-REMITTED = ZEROS
03236        AND PI-LF-CAN-REMITTED = ZEROS
03237        AND PI-AH-CAN-REMITTED = ZEROS
03238        AND PI-ISS-CNT-REMITTED = ZEROS
03239        AND PI-CAN-CNT-REMITTED = ZEROS
03240          MOVE  EL930B            TO PI-MAP-NAME
03241      ELSE
03242          IF PI-LF-ISS-REMITTED = ZEROS
03243            AND PI-AH-ISS-REMITTED = ZEROS
03244            AND PI-ISS-CNT-REMITTED = ZEROS
03245              MOVE  EL930C        TO PI-MAP-NAME
03246          ELSE
03247              MOVE  EL930B        TO PI-MAP-NAME.
03248
03249      IF PI-MAP-NAME = EL930B
03250          PERFORM 8550-SET-MAP-SEQ-NOS
03251      ELSE
03252          PERFORM 8550-SET-MAP-SEQ-NOS
03253                  VARYING WS-SUB2 FROM +1 BY +1
03254                  UNTIL WS-SUB2 GREATER THAN +2.
03255
03256      GO TO 8100-SEND-INITIAL-MAP.
03257
03258  3300-REC-NOT-FND.
03259
03260      MOVE ER-2212                TO EMI-ERROR.
03261      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03262      GO TO 8100-SEND-INITIAL-MAP.
03263
03264      EJECT
03265
03266 ******************************************************************
03267 *                                                                *
03268 *            B U I L D   I S S U E   R E C O R D                 *
03269 *                                                                *
03270 ******************************************************************
03271
03272  4000-BUILD-ISSUE-RECORD.
03273
03274      IF BSEQ   GREATER THAN PI-LAST-SEQ-NO-ADDED
03275          GO TO 4100-ADD-ISSUE-RECORD.
03276
03277 ******************************************************************
03278 *                                                                *
03279 *   THE DATA ENTRY SYSTEM ALLOWS BROWSING OF THE CURRENT BUS.    *
03280 *   FILE. THE DATA ENTRY SYS. DOES NOT HAVE A MAINT. FUNCTION.   *
03281 *   THE PROGRAM ASSUMES THAT IF A MATCH ON THE READ FOR UPDATE   *
03282 *   IS SUCCESSFUL, THE RECORD HAS PREVIOUSLY BEEN DISPLAYED      *
03283 *   THRUOUGH A BROWSE.  CHANGES ARE APPLIED AND THE PB-RECORD IS *
03284 *   REWRITTEN, ELSE A NEW PENDING BUS. RECORD IS ADDED.          *
03285 *                                                                *
03286 ******************************************************************
03287
03288      MOVE PI-COMPANY-CD          TO ERPNDT-COMP-CD.
03289      MOVE PI-SAV-ENTRY-BATCH     TO ERPNDT-ENTRY-BATCH.
03290      MOVE BSEQ                   TO ERPNDT-BATCH-SEQ.
03291
03292      
      * EXEC CICS HANDLE CONDITION
03293 *        NOTFND (4100-ADD-ISSUE-RECORD)
03294 *        END-EXEC.
      *    MOVE '"$I                   ! + #00008417' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303038343137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03295
03296      
      * EXEC CICS READ
03297 *        SET     (ERPNDT-POINTER)
03298 *        DATASET (FILE-ID-ERPNDT)
03299 *        RIDFLD  (ERPNDT-KEY)
03300 *        UPDATE
03301 *        END-EXEC.
      *    MOVE '&"S        EU         (   #00008421' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDT, 
                 ERPNDT-POINTER, 
                 DFHEIV99, 
                 ERPNDT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03302
03303      SERVICE RELOAD PENDING-BUSINESS.
03304
03305      MOVE PB-CONTROL-PRIMARY     TO ERPNDM-KEY.
03306
03307      MOVE 'B'                    TO JP-RECORD-TYPE
03308      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
03309      MOVE ERPNDT-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
03310      MOVE FILE-ID-ERPNDT         TO JP-FILE-ID.
03311      PERFORM 8400-LOG-JOURNAL-RECORD.
03312
03313      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.
03314      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.
03315      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT.
03316
03317      IF BLAST-NAME-LEN      GREATER THAN ZEROS
03318          MOVE BLAST-NAME         TO PB-I-INSURED-LAST-NAME.
03319
03320      IF B1ST-NAME-LEN       GREATER THAN ZEROS
03321          MOVE B1ST-NAME          TO PB-I-INSURED-FIRST-NAME.
03322
03323      IF BINIT-LEN           GREATER THAN ZEROS
03324          MOVE BINIT              TO PB-I-INSURED-MIDDLE-INIT.
03325
03326      IF BAGE-LEN            GREATER THAN ZEROS
03327          MOVE WS-BAGE            TO PB-I-AGE.
03328
03329      IF BJNT-AGE-LEN        GREATER THAN ZEROS
03330          MOVE WS-BJNT-AGE        TO PB-I-JOINT-AGE.
03331
03332      IF BBIRTH-LEN          GREATER THAN ZEROS
03333          MOVE WS-CONVERTED-BIRTH TO PB-I-BIRTHDAY.
03334
03335      IF BSEX-LEN            GREATER THAN ZEROS
03336          MOVE BSEX               TO PB-I-INSURED-SEX.
03337
03338      IF BTERM-LEN  (1)      GREATER THAN ZEROS
03339         MOVE WS-BTERM (1)        TO PB-I-LF-TERM.
03340
03341      IF BTERM-LEN  (2)      GREATER THAN ZEROS
03342         MOVE WS-BTERM (2)         TO PB-I-AH-TERM.
03343
03344      IF BLN-TERM-LEN        GREATER THAN ZEROS
03345          MOVE WS-BLN-TERM        TO PB-I-LOAN-TERM.
03346
03347      IF BFREQ-LEN           GREATER THAN ZEROS
03348          MOVE WS-BFREQ           TO PB-I-PAY-FREQUENCY.
03349
03350      IF BSKPCD-LEN          GREATER THAN ZEROS
03351          MOVE BSKPCD             TO PB-I-SKIP-CODE.
03352
03353      IF BMODE-LEN           GREATER THAN ZEROS
03354          MOVE BMODE              TO PB-I-TERM-TYPE.
03355
03356      IF BPMTS-LEN           GREATER THAN ZEROS
03357          MOVE WS-BPMTS           TO PB-I-NO-OF-PAYMENTS.
03358
03359      IF BPOLICY-LEN         GREATER THAN ZEROS
03360          MOVE BPOLICY            TO PB-I-POLICY-FORM-NO.
03361
03362 ******************************************************************
03363 *          IF BTYPE = ZEROS DELETE LIFE COVERAGE.                *
03364 *                                                                *
03365 *          IF BTYPE = SPACES ZERO OUT BENEFIT CODE.              *
03366 ******************************************************************
03367
03368      IF BTYPE-LEN     (1)   GREATER THAN ZEROS
03369         IF BTYPE      (1)   NOT = SPACES AND ZEROS
03370            MOVE BTYPE (1)         TO PB-I-LF-INPUT-CD
03371            MOVE WS-EDITED-LF-CODE TO PB-I-LIFE-BENEFIT-CD
03372            MOVE WS-LF-ABBR-DESC   TO PB-I-LF-ABBR
03373         ELSE
03374            IF BTYPE   (1) = SPACES
03375               MOVE SPACES         TO PB-I-LF-INPUT-CD
03376                                      PB-I-LF-ABBR
03377               MOVE ZEROS          TO PB-I-LIFE-BENEFIT-CD
03378            ELSE
03379               SUBTRACT PB-I-LF-PREMIUM-AMT FROM PI-LF-ISS-ENTERED
03380               SUBTRACT PB-I-LF-ALT-PREMIUM-AMT FROM
03381                        PI-LF-ISS-ENTERED
03382               MOVE SPACES         TO PB-I-LF-INPUT-CD
03383                                      PB-I-LF-ABBR
03384               MOVE ZEROS          TO PB-I-LF-TERM
03385                                      PB-I-LF-BENEFIT-AMT
03386                                      PB-I-LF-PREMIUM-AMT
03387                                      PB-I-LF-BENEFIT-CD
03388                                      PB-I-LF-PREM-CALC
03389                                      PB-I-LF-ALT-BENEFIT-AMT
03390                                      PB-I-LF-ALT-PREMIUM-AMT
03391                                      PB-I-LF-CRIT-PER
03392               MOVE LOW-VALUES     TO PB-I-LF-EXPIRE-DT.
03393
03394      IF  BBEN-LEN      (1)  GREATER THAN ZEROS
03395          MOVE WS-BBEN  (1)       TO PB-I-LF-BENEFIT-AMT.
03396
03397      IF  BALT-BEN-LEN     (1)  GREATER THAN ZEROS
03398          MOVE WS-BALT-BEN (1)       TO PB-I-LF-ALT-BENEFIT-AMT.
03399
03400      IF  BPREM-LEN     (1)  GREATER THAN ZEROS
03401          IF WS-BPREM   (1)  = WS-ALL-NINES OR
03402             WS-BPREM   (1)  GREATER THAN WS-ALL-NINES
03403             SUBTRACT PB-I-LF-PREMIUM-AMT FROM PI-LF-ISS-ENTERED
03404             MOVE ZEROS           TO PB-I-LF-PREMIUM-AMT
03405             MOVE '?'             TO PB-I-LF-CALC-FLAG
03406          ELSE
03407             SUBTRACT PB-I-LF-PREMIUM-AMT FROM PI-LF-ISS-ENTERED
03408             MOVE WS-BPREM (1)    TO PB-I-LF-PREMIUM-AMT
03409             ADD  WS-BPREM (1)    TO PI-LF-ISS-ENTERED
03410             MOVE SPACE           TO PB-I-LF-CALC-FLAG.
03411
03412      IF  BALT-PREM-LEN     (1) GREATER THAN ZEROS
03413          SUBTRACT PB-I-LF-ALT-PREMIUM-AMT FROM PI-LF-ISS-ENTERED
03414          MOVE WS-BALT-PREM (1)   TO PB-I-LF-ALT-PREMIUM-AMT
03415          ADD  WS-BALT-PREM (1)   TO PI-LF-ISS-ENTERED.
03416
03417 ******************************************************************
03418 *          IF BTYPE = ZEROS DELETE A&H COVERAGE.                 *
03419 *                                                                *
03420 *          IF BTYPE = SPACES ZERO OUT BENEFIT CODE.              *
03421 ******************************************************************
03422
03423      IF BTYPE-LEN     (2)   GREATER THAN ZEROS
03424         IF BTYPE      (2)   NOT = SPACES AND ZEROS
03425            MOVE BTYPE (2)         TO PB-I-AH-INPUT-CD
03426            MOVE WS-EDITED-AH-CODE TO PB-I-AH-BENEFIT-CD
03427            MOVE WS-AH-ABBR-DESC   TO PB-I-AH-ABBR
03428         ELSE
03429            IF BTYPE   (2) = SPACES
03430               MOVE SPACES         TO PB-I-AH-INPUT-CD
03431                                      PB-I-AH-ABBR
03432               MOVE ZEROS          TO PB-I-AH-BENEFIT-CD
03433            ELSE
03434               SUBTRACT PB-I-AH-PREMIUM-AMT FROM PI-AH-ISS-ENTERED
03435               MOVE SPACES         TO PB-I-AH-INPUT-CD
03436                                      PB-I-AH-ABBR
03437               MOVE ZEROS          TO PB-I-AH-TERM
03438                                      PB-I-AH-BENEFIT-AMT
03439                                      PB-I-AH-PREMIUM-AMT
03440                                      PB-I-AH-BENEFIT-CD
03441                                      PB-I-AH-PREM-CALC
03442                                      PB-I-AH-CRIT-PER
03443               MOVE LOW-VALUES     TO PB-I-AH-EXPIRE-DT.
03444
03445      IF  BBEN-LEN      (2)  GREATER THAN ZEROS
03446          MOVE WS-BBEN  (2)       TO PB-I-AH-BENEFIT-AMT.
03447
03448
03449      IF  BPREM-LEN     (2)  GREATER THAN ZEROS
03450          IF WS-BPREM   (2)  = WS-ALL-NINES OR
03451             WS-BPREM   (2)  GREATER THAN WS-ALL-NINES
03452             SUBTRACT PB-I-AH-PREMIUM-AMT FROM PI-AH-ISS-ENTERED
03453             MOVE ZEROS              TO PB-I-AH-PREMIUM-AMT
03454             MOVE '?'                TO PB-I-AH-CALC-FLAG
03455          ELSE
03456             SUBTRACT PB-I-AH-PREMIUM-AMT FROM PI-AH-ISS-ENTERED
03457             MOVE WS-BPREM (2)    TO PB-I-AH-PREMIUM-AMT
03458             ADD  WS-BPREM (2)    TO PI-AH-ISS-ENTERED
03459             MOVE SPACE           TO PB-I-AH-CALC-FLAG.
03460
03461      IF BCRIT-PERD-LEN      (1)   GREATER THAN ZEROS
03462         MOVE WS-BCRIT-PERD  (1)   TO PB-I-LF-CRIT-PER.
03463
03464      IF BCRIT-PERD-LEN      (2)   GREATER THAN ZEROS
03465         MOVE WS-BCRIT-PERD  (2)   TO PB-I-AH-CRIT-PER.
03466
03467      IF BIND-GRP-LEN        GREATER THAN ZEROS
03468          MOVE BIND-GRP           TO PB-I-INDV-GRP-OVRD.
03469
03470      IF BRTCLS-LEN          GREATER THAN ZEROS
03471          MOVE BRTCLS             TO PB-I-RATE-CLASS-OVRD.
03472
03473      IF BSIG-LEN            GREATER THAN ZEROS
03474          MOVE BSIG               TO PB-I-SIG-SW.
03475
03476      IF BAPR-LEN            GREATER THAN ZEROS
03477          MOVE WS-BAPR            TO PB-I-LOAN-APR.
03478
03479      IF BSSNUM-LEN          GREATER THAN ZEROS
03480          MOVE BSSNUM             TO PB-I-SOC-SEC-NO.
03481
03482      IF BMEM-NO-LEN         GREATER THAN ZEROS
03483          MOVE BMEM-NO        TO PB-I-MEMBER-NO.
03484
03485      IF BLN-OFFICER-LEN     GREATER THAN ZEROS
03486          MOVE BLN-OFFICER        TO PB-I-LOAN-OFFICER.
03487
03488      IF BEXPIRE-LEN    (1)  GREATER THAN ZEROS
03489         MOVE WS-CONVERTED-EXPIRDT (1) TO PB-I-LF-EXPIRE-DT.
03490
03491      IF BEXPIRE-LEN    (2)  GREATER THAN ZEROS
03492         MOVE WS-CONVERTED-EXPIRDT (2) TO PB-I-AH-EXPIRE-DT.
03493
03494      IF B1ST-PMT-LEN        GREATER THAN ZEROS
03495         MOVE WS-CONVERTED-1ST-PMT-DT TO  PB-I-1ST-PMT-DT.
03496
03497      IF BDAYS-LEN           GREATER THAN ZEROS
03498         MOVE WS-BDAYS            TO PB-I-TERM-IN-DAYS
03499                                     PB-I-EXTENTION-DAYS.
03500
03501      IF BRINCD-LEN               GREATER THAN ZEROS
03502         MOVE BRINCD              TO PB-I-SPECIAL-REIN-CODE.
03503
03504      IF BENTRY-LEN   NOT = ZEROS
03505         IF BENTRY = 'E' OR 'R' OR 'P'
03506            MOVE BENTRY           TO PB-BATCH-ENTRY
03507      ELSE
03508         IF BENTRY = 'H'
03509            MOVE BENTRY           TO PB-RECORD-BILL
03510         ELSE
03511            MOVE BENTRY           TO PB-FORCE-CODE.
03512
03513      IF BLIVES-LEN          GREATER THAN ZEROS
03514         MOVE WS-BLIVES           TO PB-I-LIVES.
03515
03516      IF BJNT-1ST-NAME-LEN   GREATER THAN ZEROS
03517          MOVE BJNT-1ST-NAME      TO PB-I-JOINT-FIRST-NAME.
03518
03519      IF BJNT-INIT-LEN       GREATER THAN ZEROS
03520          MOVE BJNT-INIT          TO PB-I-JOINT-MIDDLE-INIT.
03521
03522      IF BJNT-LST-NAME-LEN   GREATER THAN ZEROS
03523          MOVE BJNT-LST-NAME      TO PB-I-JOINT-LAST-NAME.
03524
03525      IF BBENEFICIARY-LEN    GREATER THAN ZEROS
03526          MOVE BBENEFICIARY       TO PB-I-BENEFICIARY-NAME.
03527
03528      MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.
03529      MOVE PI-AH-OVERRIDE-L1      TO PB-LIFE-OVERRIDE-L1.
03530
03531      IF PI-MAIL-YES
03532         MOVE '1'                 TO PB-I-MAIL-ADDRS-SW.
03533
03534      MOVE 'C'                    TO JP-RECORD-TYPE.
03535      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
03536      MOVE ERPNDT-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
03537
03538      
      * EXEC CICS REWRITE
03539 *        DATASET (FILE-ID-ERPNDT)
03540 *        FROM    (PENDING-BUSINESS)
03541 *        END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008663' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDT, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03542
03543      MOVE ERPNDT-KEY             TO PI-SAV-ENDING-ERPNDT-KEY.
03544
03545      PERFORM 8400-LOG-JOURNAL-RECORD.
03546
03547      IF EIBAID = DFHENTER
03548          MOVE PI-NEXT-DISPLAY-SEQ-NO TO BSEQ
03549          ADD +1                      TO PI-NEXT-DISPLAY-SEQ-NO
03550          MOVE AL-SABON               TO BSEQ-ATTRB.
03551
03552      IF PI-MAIL-YES
03553         NEXT SENTENCE
03554      ELSE
03555         GO TO 4900-EXIT.
03556
03557      IF BLAST-NAME-LEN = ZEROS AND
03558         B1ST-NAME-LEN  = ZEROS AND
03559         BINIT-LEN      = ZEROS AND
03560         BADDRS1-LEN    = ZEROS AND
03561         BADDRS2-LEN    = ZEROS AND
03562         BCITYST-LEN    = ZEROS AND
03563         BZIP5-LEN      = ZEROS AND
03564         BZIP4-LEN      = ZEROS AND
03565         BPHONE-LEN     = ZEROS AND
03566         BSSNUM-LEN     = ZEROS
03567         GO TO 4900-EXIT.
03568
03569      
      * EXEC CICS HANDLE CONDITION
03570 *        NOTFND (4185-ADD-MAILING-RECORD)
03571 *        END-EXEC.
      *    MOVE '"$I                   ! , #00008694' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303038363934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03572
03573      
      * EXEC CICS READ
03574 *        SET     (ERPNDM-POINTER)
03575 *        DATASET (FILE-ID-ERPNDM)
03576 *        RIDFLD  (ERPNDM-KEY)
03577 *        UPDATE
03578 *        END-EXEC.
      *    MOVE '&"S        EU         (   #00008698' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDM, 
                 ERPNDM-POINTER, 
                 DFHEIV99, 
                 ERPNDM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03579
03580      SERVICE RELOAD PENDING-MAILING-DATA.
03581
03582      MOVE 'B'                    TO JP-RECORD-TYPE.
03583      MOVE PENDING-MAILING-DATA   TO JP-RECORD-AREA.
03584      MOVE ERPNDM-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
03585      MOVE FILE-ID-ERPNDM         TO JP-FILE-ID.
03586      PERFORM 8400-LOG-JOURNAL-RECORD.
03587
03588      MOVE PI-PROCESSOR-ID        TO PM-LAST-MAINT-BY.
03589      MOVE EIBTIME                TO PM-LAST-MAINT-HHMMSS.
03590      MOVE WS-CURRENT-BIN-DT      TO PM-LAST-MAINT-DT.
03591
03592      IF BLAST-NAME-LEN      GREATER THAN ZEROS
03593          MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.
03594
03595      IF B1ST-NAME-LEN       GREATER THAN ZEROS
03596          MOVE B1ST-NAME          TO PM-INSURED-FIRST-NAME.
03597
03598      IF BINIT-LEN           GREATER THAN ZEROS
03599          MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.
03600
03601      IF BAGE-LEN            GREATER THAN ZEROS
03602          MOVE WS-BAGE            TO PM-INSURED-ISSUE-AGE.
03603
03604      IF BBIRTH-LEN          GREATER THAN ZEROS
03605         MOVE  WS-CONVERTED-BIRTH TO PM-INSURED-BIRTH-DT.
03606
03607      IF BLAST-NAME-LEN      GREATER THAN ZEROS
03608          MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.
03609
03610      IF BINIT-LEN           GREATER THAN ZEROS
03611          MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.
03612
03613      IF BADDRS1-LEN         GREATER THAN ZERO
03614          MOVE BADDRS1            TO PM-ADDRESS-LINE-1.
03615
03616      IF BADDRS2-LEN         GREATER THAN ZERO
03617          MOVE BADDRS2            TO PM-ADDRESS-LINE-2.
03618
03619      IF BCITYST-LEN         GREATER THAN ZERO
03620          MOVE BCITYST            TO PM-CITY-STATE.
03621
03622      IF BZIP5-LEN           GREATER THAN ZERO
03623          MOVE BZIP5              TO W-TEST-ZIP
03624
03625          IF  W-CANADIAN-POST-CODE
03626              MOVE BZIP5          TO PM-CAN-POST1
03627
03628          ELSE
03629              MOVE BZIP5          TO PM-ZIP-CODE.
03630
03631
03632      IF BZIP4-LEN           GREATER THAN ZERO
03633
03634          IF  PM-CANADIAN-POST-CODE
03635              MOVE BZIP4          TO PM-CAN-POST2
03636
03637          ELSE
03638              MOVE BZIP4          TO PM-ZIP-PLUS4.
03639
03640      IF BPHONE-LEN          GREATER THAN ZERO
03641          MOVE WS-BPHONE          TO PM-PHONE-NO.
03642
03643      IF BSSNUM-LEN          GREATER THAN ZEROS
03644          MOVE BSSNUM             TO PM-INSURED-SOC-SEC-NO.
03645
03646      MOVE 'C'                    TO JP-RECORD-TYPE.
03647      MOVE PENDING-MAILING-DATA   TO JP-RECORD-AREA.
03648      MOVE ERPNDM-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
03649      MOVE FILE-ID-ERPNDM         TO JP-FILE-ID.
03650
03651      
      * EXEC CICS REWRITE
03652 *        DATASET (FILE-ID-ERPNDM)
03653 *        FROM    (PENDING-MAILING-DATA)
03654 *        END-EXEC.
           MOVE LENGTH OF
            PENDING-MAILING-DATA
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008776' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDM, 
                 PENDING-MAILING-DATA, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03655
03656      PERFORM 8400-LOG-JOURNAL-RECORD.
03657
03658      GO TO 4900-EXIT.
03659
03660      EJECT
03661
03662 ******************************************************************
03663 *                                                                *
03664 *               A D D  I S S U E   R E C O R D                   *
03665 *                                                                *
03666 ******************************************************************
03667
03668  4100-ADD-ISSUE-RECORD.
03669
03670      
      * EXEC CICS GETMAIN
03671 *        SET     (ERPNDT-POINTER)
03672 *        LENGTH  (ERPNDT-RECORD-LENGTH)
03673 *        INITIMG (GETMAIN-SPACE)
03674 *        END-EXEC.
      *    MOVE ',"IL                  $   #00008795' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-POINTER, 
                 ERPNDT-RECORD-LENGTH, 
                 GETMAIN-SPACE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03675
03676      SERVICE RELOAD PENDING-BUSINESS.
03677
03678      MOVE 'PB'                   TO PB-RECORD-ID.
03679      MOVE PI-COMPANY-CD          TO PB-COMPANY-CD
03680                                     PB-COMPANY-CD-A1.
03681      MOVE PI-COMPANY-ID          TO PB-COMPANY-ID.
03682      MOVE PI-SAV-ENTRY-BATCH     TO PB-ENTRY-BATCH.
03683      MOVE BSEQ                   TO PB-BATCH-SEQ-NO.
03684
03685      IF BSEQ   GREATER THAN PI-LAST-SEQ-NO-ADDED
03686          MOVE BSEQ          TO PI-LAST-SEQ-NO-ADDED.
03687
03688      IF BCAR-LEN GREATER THAN ZEROS
03689         MOVE BCAR                TO PB-CARRIER.
03690
03691      IF BGRP-LEN GREATER THAN ZEROS
03692         MOVE BGRP                TO PB-GROUPING.
03693
03694      IF BST-LEN  GREATER THAN ZEROS
03695         MOVE BST                 TO PB-STATE.
03696
03697      IF BACCT-LEN GREATER THAN ZEROS
03698         MOVE BACCT               TO PB-ACCOUNT.
03699
03700      MOVE '1'                    TO PB-RECORD-TYPE.
03701      MOVE BCERT                  TO PB-CERT-PRIME.
03702      MOVE WS-CONVERTED-EFFDT     TO PB-CERT-EFF-DT.
03703      MOVE ZEROS                  TO PB-BATCH-CHG-SEQ-NO
03704                                     PB-ALT-CHG-SEQ-NO.
03705
03706      MOVE ZEROS                  TO PB-I-LOAN-TERM
03707                                     PB-I-LF-PREM-CALC
03708                                     PB-I-LF-ALT-PREM-CALC
03709                                     PB-I-LF-RATE
03710                                     PB-I-LF-ALT-RATE
03711                                     PB-I-LF-REI-RATE
03712                                     PB-I-LF-ALT-REI-RATE
03713                                     PB-I-RATE-DEV-PCT-LF
03714                                     PB-I-AH-PREM-CALC
03715                                     PB-I-AH-RATE
03716                                     PB-I-AH-REI-RATE
03717                                     PB-I-AH-RATE-TRM
03718                                     PB-I-RATE-DEV-PCT-AH
03719                                     PB-I-BUSINESS-TYPE
03720                                     PB-I-LIFE-COMMISSION
03721                                     PB-I-JOINT-COMMISSION
03722                                     PB-I-AH-COMMISSION
03723                                     PB-I-CURR-SEQ
03724                                     PB-CHG-COUNT
03725                                     PB-LF-BILLED-AMTS
03726                                     PB-AH-BILLED-AMTS
03727                                     PB-CALC-TOLERANCE
03728                                     PB-I-EXTENTION-DAYS
03729                                     PB-I-TERM-IN-DAYS.
03730
03731      MOVE LOW-VALUES             TO PB-CREDIT-ACCEPT-DT
03732                                     PB-I-LF-EXPIRE-DT
03733                                     PB-I-AH-EXPIRE-DT
03734                                     PB-I-1ST-PMT-DT
03735                                     PB-BILLED-DT
03736                                     PB-ACCT-EFF-DT
03737                                     PB-ACCT-EXP-DT.
03738
03739
03740      MOVE 'X'                    TO PB-FATAL-FLAG.
03741
03742      IF PI-MAIL-YES
03743         MOVE '1'                 TO PB-I-MAIL-ADDRS-SW.
03744
03745      IF PI-NB-MONTH-END-DT NOT = SPACES
03746         MOVE PI-NB-MONTH-END-DT  TO PB-CREDIT-SELECT-DT
03747        ELSE
03748         MOVE PI-CR-MONTH-END-DT     TO PB-CREDIT-SELECT-DT.
03749
03750      IF BSFX-LEN            GREATER THAN ZEROS
03751          MOVE BSFX               TO PB-CERT-SFX.
03752
03753      IF BLAST-NAME-LEN      GREATER THAN ZEROS
03754          MOVE BLAST-NAME         TO PB-I-INSURED-LAST-NAME.
03755
03756      IF B1ST-NAME-LEN       GREATER THAN ZEROS
03757          MOVE B1ST-NAME          TO PB-I-INSURED-FIRST-NAME.
03758
03759      IF BINIT-LEN           GREATER THAN ZEROS
03760          MOVE BINIT              TO PB-I-INSURED-MIDDLE-INIT.
03761
03762      IF BAGE-LEN            GREATER THAN ZEROS
03763          MOVE WS-BAGE            TO PB-I-AGE
03764      ELSE
03765          MOVE ZEROS              TO PB-I-AGE.
03766
03767
03768      IF BJNT-AGE-LEN        GREATER THAN ZEROS
03769          MOVE WS-BJNT-AGE        TO PB-I-JOINT-AGE
03770      ELSE
03771          MOVE ZEROS              TO PB-I-JOINT-AGE.
03772
03773      IF BBIRTH-LEN          GREATER THAN ZEROS
03774          MOVE WS-CONVERTED-BIRTH TO PB-I-BIRTHDAY
03775      ELSE
03776          MOVE LOW-VALUES          TO PB-I-BIRTHDAY.
03777
03778      IF BSEX-LEN            GREATER THAN ZEROS
03779          MOVE BSEX               TO PB-I-INSURED-SEX.
03780
03781      IF BTERM-LEN  (1)      GREATER THAN ZEROS
03782         MOVE WS-BTERM (1)           TO PB-I-LF-TERM
03783      ELSE
03784         MOVE ZEROS               TO PB-I-LF-TERM.
03785
03786      IF BTERM-LEN  (2)      GREATER THAN ZEROS
03787         MOVE WS-BTERM (2)           TO PB-I-AH-TERM
03788      ELSE
03789         MOVE ZEROS              TO PB-I-AH-TERM.
03790
03791      IF BLN-TERM-LEN        GREATER THAN ZEROS
03792          MOVE WS-BLN-TERM        TO PB-I-LOAN-TERM
03793      ELSE
03794          MOVE ZEROS              TO PB-I-LOAN-TERM.
03795
03796      IF BFREQ-LEN           GREATER THAN ZEROS
03797          MOVE WS-BFREQ           TO PB-I-PAY-FREQUENCY
03798      ELSE
03799          MOVE ZEROS              TO PB-I-PAY-FREQUENCY.
03800
03801      IF BSKPCD-LEN          GREATER THAN ZEROS
03802          MOVE BSKPCD             TO PB-I-SKIP-CODE.
03803
03804      IF BMODE-LEN           GREATER THAN ZEROS
03805          MOVE BMODE              TO PB-I-TERM-TYPE.
03806
03807      IF BPMTS-LEN           GREATER THAN ZEROS
03808          MOVE WS-BPMTS           TO PB-I-NO-OF-PAYMENTS
03809      ELSE
03810          MOVE ZEROS              TO PB-I-NO-OF-PAYMENTS.
03811
03812      IF BPOLICY-LEN         GREATER THAN ZEROS
03813          MOVE BPOLICY            TO PB-I-POLICY-FORM-NO.
03814
03815      IF BTYPE-LEN     (1)   GREATER THAN ZEROS
03816         IF BTYPE      (1)   NOT = ZEROS  AND  SPACES
03817            MOVE BTYPE (1)         TO PB-I-LF-INPUT-CD
03818            MOVE WS-EDITED-LF-CODE TO PB-I-LIFE-BENEFIT-CD
03819            MOVE WS-LF-ABBR-DESC  TO PB-I-LF-ABBR
03820         ELSE
03821            MOVE ZEROS             TO PB-I-LIFE-BENEFIT-CD
03822      ELSE
03823            MOVE ZEROS             TO PB-I-LIFE-BENEFIT-CD.
03824
03825      IF  BBEN-LEN      (1)  GREATER THAN ZEROS
03826          MOVE WS-BBEN  (1)       TO PB-I-LF-BENEFIT-AMT
03827      ELSE
03828          MOVE ZEROS              TO PB-I-LF-BENEFIT-AMT.
03829
03830      IF  BALT-BEN-LEN     (1)  GREATER THAN ZEROS
03831          MOVE WS-BALT-BEN (1)    TO PB-I-LF-ALT-BENEFIT-AMT
03832      ELSE
03833          MOVE ZEROS              TO PB-I-LF-ALT-BENEFIT-AMT.
03834
03835      IF  BPREM-LEN     (1)  GREATER THAN ZEROS
03836          IF WS-BPREM   (1) = WS-ALL-NINES OR
03837             WS-BPREM   (1) GREATER THAN WS-ALL-NINES
03838             MOVE ZEROS           TO PB-I-LF-PREMIUM-AMT
03839             MOVE '?'             TO PB-I-LF-CALC-FLAG
03840          ELSE
03841             ADD  WS-BPREM (1)    TO PI-LF-ISS-ENTERED
03842             MOVE WS-BPREM (1)    TO PB-I-LF-PREMIUM-AMT
03843      ELSE
03844          MOVE ZEROS              TO PB-I-LF-PREMIUM-AMT.
03845
03846      IF  BALT-PREM-LEN     (1) GREATER THAN ZEROS
03847          MOVE WS-BALT-PREM (1)   TO PB-I-LF-ALT-PREMIUM-AMT
03848          ADD  WS-BALT-PREM (1)   TO PI-LF-ISS-ENTERED
03849      ELSE
03850          MOVE ZEROS              TO PB-I-LF-ALT-PREMIUM-AMT.
03851
03852      IF BTYPE-LEN     (2)   GREATER THAN ZEROS
03853         IF BTYPE      (2)   NOT = ZEROS  AND  SPACES
03854            MOVE BTYPE (2)         TO PB-I-AH-INPUT-CD
03855            MOVE WS-EDITED-AH-CODE TO PB-I-AH-BENEFIT-CD
03856            MOVE WS-AH-ABBR-DESC   TO PB-I-AH-ABBR
03857         ELSE
03858            MOVE ZEROS             TO PB-I-AH-BENEFIT-CD
03859      ELSE
03860            MOVE ZEROS             TO PB-I-AH-BENEFIT-CD.
03861
03862      IF  BBEN-LEN         (2)  GREATER THAN ZEROS
03863          MOVE WS-BBEN     (2)    TO PB-I-AH-BENEFIT-AMT
03864      ELSE
03865          MOVE ZEROS              TO PB-I-AH-BENEFIT-AMT.
03866
03867
03868      IF  BPREM-LEN     (2)  GREATER THAN ZEROS
03869          IF WS-BPREM   (2) = WS-ALL-NINES OR
03870             WS-BPREM   (2) GREATER THAN WS-ALL-NINES
03871             MOVE ZEROS            TO PB-I-AH-PREMIUM-AMT
03872             MOVE '?'              TO PB-I-AH-CALC-FLAG
03873          ELSE
03874             ADD  WS-BPREM (2)     TO PI-AH-ISS-ENTERED
03875             MOVE WS-BPREM (2)     TO PB-I-AH-PREMIUM-AMT
03876      ELSE
03877          MOVE ZEROS               TO PB-I-AH-PREMIUM-AMT.
03878
03879      IF BCRIT-PERD-LEN      (1)   GREATER THAN ZEROS
03880         MOVE WS-BCRIT-PERD  (1)   TO PB-I-LF-CRIT-PER
03881      ELSE
03882         MOVE ZEROS                TO PB-I-LF-CRIT-PER.
03883
03884      IF BCRIT-PERD-LEN      (2)   GREATER THAN ZEROS
03885         MOVE WS-BCRIT-PERD  (2)   TO PB-I-AH-CRIT-PER
03886      ELSE
03887         MOVE ZEROS                TO PB-I-AH-CRIT-PER.
03888      IF BIND-GRP-LEN        GREATER THAN ZEROS
03889          MOVE BIND-GRP           TO PB-I-INDV-GRP-OVRD.
03890
03891      IF BRTCLS-LEN          GREATER THAN ZEROS
03892          MOVE BRTCLS             TO PB-I-RATE-CLASS-OVRD.
03893
03894      IF BSIG-LEN            GREATER THAN ZEROS
03895          MOVE BSIG               TO PB-I-SIG-SW.
03896
03897      IF BAPR-LEN            GREATER THAN ZEROS
03898          MOVE WS-BAPR            TO PB-I-LOAN-APR
03899      ELSE
03900          MOVE ZEROS              TO PB-I-LOAN-APR.
03901
03902      IF BSSNUM-LEN          GREATER THAN ZEROS
03903          MOVE BSSNUM             TO PB-I-SOC-SEC-NO.
03904
03905      IF BMEM-NO-LEN         GREATER THAN ZEROS
03906          MOVE BMEM-NO        TO PB-I-MEMBER-NO.
03907
03908      IF BLN-OFFICER-LEN     GREATER THAN ZEROS
03909          MOVE BLN-OFFICER        TO PB-I-LOAN-OFFICER.
03910
03911      IF BEXPIRE-LEN    (1)  GREATER THAN ZEROS
03912         MOVE WS-CONVERTED-EXPIRDT (1) TO PB-I-LF-EXPIRE-DT
03913      ELSE
03914         MOVE LOW-VALUES               TO PB-I-LF-EXPIRE-DT.
03915
03916      IF BEXPIRE-LEN    (2)  GREATER THAN ZEROS
03917         MOVE WS-CONVERTED-EXPIRDT (2) TO PB-I-AH-EXPIRE-DT
03918      ELSE
03919         MOVE LOW-VALUES               TO PB-I-AH-EXPIRE-DT.
03920
03921      IF B1ST-PMT-LEN        GREATER THAN ZEROS
03922         MOVE WS-CONVERTED-1ST-PMT-DT TO  PB-I-1ST-PMT-DT.
03923
03924      IF BDAYS-LEN           GREATER THAN ZEROS
03925         MOVE WS-BDAYS            TO PB-I-TERM-IN-DAYS
03926                                     PB-I-EXTENTION-DAYS
03927      ELSE
03928         MOVE ZEROS               TO PB-I-TERM-IN-DAYS
03929                                     PB-I-EXTENTION-DAYS.
03930
03931      IF BRINCD-LEN               GREATER THAN ZEROS
03932         MOVE BRINCD              TO PB-I-SPECIAL-REIN-CODE.
03933
03934      IF BENTRY-LEN   NOT = ZEROS
03935         IF BENTRY = 'E' OR 'R' OR 'P'
03936            MOVE BENTRY           TO PB-BATCH-ENTRY
03937      ELSE
03938         IF BENTRY = 'H'
03939            MOVE BENTRY           TO PB-RECORD-BILL
03940         ELSE
03941            MOVE BENTRY           TO PB-FORCE-CODE.
03942
03943      IF BLIVES-LEN          GREATER THAN ZEROS
03944         MOVE WS-BLIVES           TO PB-I-LIVES
03945      ELSE
03946         MOVE ZEROS               TO PB-I-LIVES.
03947
03948      IF BJNT-1ST-NAME-LEN   GREATER THAN ZEROS
03949          MOVE BJNT-1ST-NAME      TO PB-I-JOINT-FIRST-NAME.
03950
03951      IF BJNT-INIT-LEN       GREATER THAN ZEROS
03952          MOVE BJNT-INIT          TO PB-I-JOINT-MIDDLE-INIT.
03953
03954      IF BJNT-LST-NAME-LEN   GREATER THAN ZEROS
03955          MOVE BJNT-LST-NAME      TO PB-I-JOINT-LAST-NAME.
03956
03957      IF BBENEFICIARY-LEN    GREATER THAN ZEROS
03958          MOVE BBENEFICIARY       TO PB-I-BENEFICIARY-NAME.
03959
03960      MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.
03961      MOVE PI-AH-OVERRIDE-L1      TO PB-LIFE-OVERRIDE-L1.
03962
03963
03964  4175-WRITE-PB-RECORD.
03965
03966      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY
03967                                     PB-INPUT-BY.
03968      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.
03969      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT
03970                                     PB-INPUT-DT.
03971      MOVE 'A'                    TO JP-RECORD-TYPE.
03972      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
03973      MOVE PB-CONTROL-PRIMARY     TO PI-SAV-ENDING-ERPNDT-KEY
03974                                     ERPNDM-KEY.
03975
03976      ADD +1                      TO PI-SAV-BATCH-SEQ.
03977
03978      
      * EXEC CICS HANDLE CONDITION
03979 *        DUPREC (4200-DUPLICATE-ALT-INDEX)
03980 *        END-EXEC.
      *    MOVE '"$%                   ! - #00009103' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303039313033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03981
03982      
      * EXEC CICS WRITE
03983 *        DATASET (FILE-ID-ERPNDT)
03984 *        FROM    (PENDING-BUSINESS)
03985 *        RIDFLD  (PB-CONTROL-PRIMARY)
03986 *        END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00009107' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDT, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 PB-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03987
03988      ADD +1                      TO PI-ISS-CNT-ENTERED.
03989
03990      MOVE ERPNDT-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
03991      MOVE FILE-ID-ERPNDT         TO JP-FILE-ID.
03992      PERFORM 8400-LOG-JOURNAL-RECORD.
03993
03994      MOVE PI-NEXT-DISPLAY-SEQ-NO TO BSEQ.
03995      MOVE AL-SABON               TO BSEQ-ATTRB.
03996
03997      ADD +1                      TO PI-NEXT-DISPLAY-SEQ-NO.
03998
03999      EJECT
04000
04001 ******************************************************************
04002 *                                                                *
04003 *    CHECK THE FIRST ISSUE RECORD IN EVERY NEW BATCH.  VERIFY    *
04004 *    THAT THE CERTIFICATE DOES NOT EXIST ON THE CERT. MASTER     *
04005 *    FILE.  IF IT DOES DISPLAY WARNING MESSAGE ON BLANK SCREEN.  *
04006 *                                                                *
04007 ******************************************************************
04008
04009
04010      IF  PI-MAINT-FUNC = 'N' NEXT SENTENCE
04011         ELSE
04012          GO TO 4185-ADD-MAILING-RECORD.
04013
04014      IF  PI-ISSUE-ADDED
04015          GO TO 4185-ADD-MAILING-RECORD.
04016
04017      MOVE 'Y'                    TO  PI-ISSUE-ADDED-SW.
04018
04019      
      * EXEC CICS HANDLE CONDITION
04020 *        NOTFND (4185-ADD-MAILING-RECORD)
04021 *        END-EXEC.
      *    MOVE '"$I                   ! . #00009144' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303039313434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04022
04023      MOVE PB-CONTROL-BY-ACCOUNT  TO  ELCERT-KEY.
04024
04025      
      * EXEC CICS READ
04026 *        SET     (ELCERT-POINTER)
04027 *        DATASET (FILE-ID-ELCERT)
04028 *        RIDFLD  (ELCERT-KEY)
04029 *        LENGTH  (ELCERT-RECORD-LENGTH)
04030 *        UPDATE
04031 *        END-EXEC.
      *    MOVE '&"SL       EU         (   #00009150' TO DFHEIV0
           MOVE X'2622534C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCERT, 
                 ELCERT-POINTER, 
                 ELCERT-RECORD-LENGTH, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04032
04033      SERVICE RELOAD CERTIFICATE-MASTER.
04034
04035      IF  CERT-WAS-CREATED-FOR-CLAIM
04036          GO TO 4185-ADD-MAILING-RECORD.
04037
04038      GO TO 8350-SEND-WARNING.
04039
04040      EJECT
04041
04042 ******************************************************************
04043 *                                                                *
04044 *          A D D   M A I L I N G   R E C O R D                   *
04045 *                                                                *
04046 ******************************************************************
04047
04048  4185-ADD-MAILING-RECORD.
04049
04050      IF PI-MAIL-YES
04051         NEXT SENTENCE
04052      ELSE
04053         GO TO 4900-EXIT.
04054
04055      
      * EXEC CICS GETMAIN
04056 *        SET     (ERPNDM-POINTER)
04057 *        LENGTH  (ERPNDM-RECORD-LENGTH)
04058 *        INITIMG (GETMAIN-SPACE)
04059 *        END-EXEC.
      *    MOVE ',"IL                  $   #00009180' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDM-POINTER, 
                 ERPNDM-RECORD-LENGTH, 
                 GETMAIN-SPACE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04060
04061      SERVICE RELOAD PENDING-MAILING-DATA.
04062
04063      MOVE 'PM'                   TO PM-RECORD-ID.
04064      MOVE 'ER'                   TO PM-SOURCE-SYSTEM.
04065
04066      MOVE PI-PROCESSOR-ID        TO PM-LAST-MAINT-BY
04067                                     PM-RECORD-ADDED-BY.
04068      MOVE EIBTIME                TO PM-LAST-MAINT-HHMMSS.
04069      MOVE WS-CURRENT-BIN-DT      TO PM-LAST-MAINT-DT
04070                                     PM-RECORD-ADD-DT.
04071
04072      MOVE ERPNDM-KEY             TO PM-CONTROL-PRIMARY.
04073
04074      IF BLAST-NAME-LEN      GREATER THAN ZEROS
04075          MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.
04076
04077      IF B1ST-NAME-LEN       GREATER THAN ZEROS
04078          MOVE B1ST-NAME          TO PM-INSURED-FIRST-NAME.
04079
04080      IF BINIT-LEN           GREATER THAN ZEROS
04081          MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.
04082
04083
04084      IF BAGE-LEN            GREATER THAN ZEROS
04085          MOVE WS-BAGE            TO PM-INSURED-ISSUE-AGE
04086      ELSE
04087          MOVE ZEROS              TO PM-INSURED-ISSUE-AGE.
04088
04089      IF BBIRTH-LEN          GREATER THAN ZEROS
04090         MOVE  WS-CONVERTED-BIRTH TO PM-INSURED-BIRTH-DT
04091      ELSE
04092         MOVE LOW-VALUES          TO PM-INSURED-BIRTH-DT.
04093
04094      IF BLAST-NAME-LEN      GREATER THAN ZEROS
04095          MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.
04096
04097      IF BINIT-LEN           GREATER THAN ZEROS
04098          MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.
04099
04100      IF BADDRS1-LEN         GREATER THAN ZERO
04101          MOVE BADDRS1            TO PM-ADDRESS-LINE-1.
04102
04103      IF BADDRS2-LEN         GREATER THAN ZERO
04104          MOVE BADDRS2            TO PM-ADDRESS-LINE-2.
04105
04106      IF BCITYST-LEN         GREATER THAN ZERO
04107          MOVE BCITYST            TO PM-CITY-STATE.
04108
04109      IF BZIP5-LEN           GREATER THAN ZERO
04110          MOVE BZIP5              TO W-TEST-ZIP
04111
04112          IF  W-CANADIAN-POST-CODE
04113              MOVE BZIP5          TO PM-CAN-POST1
04114
04115          ELSE
04116              MOVE BZIP5          TO PM-ZIP-CODE.
04117
04118      IF BZIP4-LEN           GREATER THAN ZERO
04119
04120          IF  PM-CANADIAN-POST-CODE
04121              MOVE BZIP4          TO PM-CAN-POST2
04122
04123          ELSE
04124              MOVE BZIP4          TO PM-ZIP-PLUS4.
04125
04126      IF BPHONE-LEN          GREATER THAN ZERO
04127          MOVE WS-BPHONE          TO PM-PHONE-NO
04128      ELSE
04129          MOVE ZEROS              TO PM-PHONE-NO.
04130
04131      IF BSSNUM-LEN          GREATER THAN ZEROS
04132          MOVE BSSNUM             TO PM-INSURED-SOC-SEC-NO.
04133
04134      MOVE 'A'                    TO JP-RECORD-TYPE.
04135      MOVE PENDING-MAILING-DATA   TO JP-RECORD-AREA.
04136      MOVE ERPNDM-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
04137      MOVE FILE-ID-ERPNDM         TO JP-FILE-ID.
04138
04139      
      * EXEC CICS WRITE
04140 *        DATASET (FILE-ID-ERPNDM)
04141 *        FROM    (PENDING-MAILING-DATA)
04142 *        RIDFLD  (PM-CONTROL-PRIMARY)
04143 *        END-EXEC.
           MOVE LENGTH OF
            PENDING-MAILING-DATA
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00009264' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039323634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDM, 
                 PENDING-MAILING-DATA, 
                 DFHEIV11, 
                 PM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04144
04145      PERFORM 8400-LOG-JOURNAL-RECORD.
04146
04147      MOVE LOW-VALUES             TO MAP-B.
04148
04149      GO TO 4900-EXIT.
04150
04151  4200-DUPLICATE-ALT-INDEX.
04152
04153      MOVE ER-2247                TO EMI-ERROR.
04154      MOVE -1                     TO BCERT-LEN.
04155      MOVE AL-UABON               TO BCERT-ATTRB.
04156      MOVE AL-UNBON               TO BEFFDT-ATTRB.
04157      MOVE 'Y'                    TO PI-ERROR-SW.
04158
04159      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04160
04161      IF BPREM-LEN (1) GREATER THAN ZEROS
04162          SUBTRACT WS-BPREM (1)
04163                                  FROM PI-LF-ISS-ENTERED.
04164
04165      IF BALT-PREM-LEN (1) GREATER THAN ZEROS
04166          SUBTRACT WS-BALT-PREM (1)
04167                                  FROM PI-LF-ISS-ENTERED.
04168
04169      IF BPREM-LEN (2) GREATER THAN ZEROS
04170          SUBTRACT WS-BPREM (2)
04171                                  FROM PI-AH-ISS-ENTERED.
04172
04173      SUBTRACT +1                 FROM PI-LAST-SEQ-NO-ADDED.
04174      SUBTRACT +1                 FROM PI-SAV-BATCH-SEQ.
04175
04176  4900-EXIT.
04177      EXIT.
04178
04179      EJECT
04180
04181 ******************************************************************
04182 *                                                                *
04183 *            B U I L D   C A N C E L   R E C O R D               *
04184 *                                                                *
04185 ******************************************************************
04186
04187  5000-BUILD-CANCEL-RECORD.
04188
04189      IF CSEQ (WS-SUB2) GREATER THAN PI-LAST-SEQ-NO-ADDED
04190          GO TO 5100-ADD-CANCEL-RECORD.
04191
04192 ******************************************************************
04193 *                                                                *
04194 *   THE DATA ENTRY SYSTEM ALLOWS BROWSING OF THE CURRENT BUS.    *
04195 *   FILE. THE DATA ENTRY SYS. DOES NOT HAVE A MAINT. FUNCTION.   *
04196 *   THE PROGRAM ASSUMES THAT IF A MATCH ON THE READ FOR UPDATE   *
04197 *   IS SUCCESSFUL, THE RECORD HAS PREVIOUSLY BEEN DISPLAYED      *
04198 *   THRUOUGH A BROWSE.  CHANGES ARE APPLIED AND THE PB-RECORD IS *
04199 *   REWRITEN, ELSE A NEW PB-RECORD IS ADDED.                     *
04200 *                                                                *
04201 ******************************************************************
04202
04203      MOVE PI-COMPANY-CD          TO ERPNDT-COMP-CD.
04204      MOVE PI-SAV-ENTRY-BATCH     TO ERPNDT-ENTRY-BATCH.
04205      MOVE CSEQ (WS-SUB2)         TO ERPNDT-BATCH-SEQ.
04206
04207      
      * EXEC CICS HANDLE CONDITION
04208 *        NOTFND (5100-ADD-CANCEL-RECORD)
04209 *        END-EXEC.
      *    MOVE '"$I                   ! / #00009332' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303039333332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04210
04211      
      * EXEC CICS READ
04212 *        SET     (ERPNDT-POINTER)
04213 *        DATASET (FILE-ID-ERPNDT)
04214 *        RIDFLD  (ERPNDT-KEY)
04215 *        UPDATE
04216 *        END-EXEC.
      *    MOVE '&"S        EU         (   #00009336' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039333336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDT, 
                 ERPNDT-POINTER, 
                 DFHEIV99, 
                 ERPNDT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04217
04218      SERVICE RELOAD PENDING-BUSINESS.
04219
04220      MOVE 'B'                    TO JP-RECORD-TYPE
04221      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
04222      MOVE ERPNDT-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
04223      MOVE FILE-ID-ERPNDT         TO JP-FILE-ID.
04224
04225      PERFORM 8400-LOG-JOURNAL-RECORD.
04226
04227      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.
04228      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.
04229      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT.
04230
04231      IF CSFX-LEN  (WS-SUB2) GREATER THAN ZEROS
04232         MOVE CSFX (WS-SUB2)      TO PB-CERT-SFX.
04233
04234      IF CLAST-NAME-LEN (WS-SUB2) GREATER THAN ZEROS
04235          MOVE CLAST-NAME (WS-SUB2)  TO PB-C-LAST-NAME.
04236
04237      IF CREFUND1-LEN   (WS-SUB2) GREATER THAN ZEROS
04238          IF WS-CREFUND1 (WS-SUB2) = WS-ALL-NINES OR
04239             WS-CREFUND1 (WS-SUB2) GREATER THAN WS-ALL-NINES
04240             SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED
04241             MOVE ZEROS            TO PB-C-LF-CANCEL-AMT
04242             MOVE '?'              TO PB-C-LF-CALC-REQ
04243          ELSE
04244             SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED
04245             ADD WS-CREFUND1  (WS-SUB2) TO PI-LF-CAN-ENTERED
04246             MOVE WS-CREFUND1 (WS-SUB2) TO PB-C-LF-CANCEL-AMT
04247             MOVE SPACE                 TO PB-C-LF-CALC-REQ.
04248
04249      IF CREFUND2-LEN   (WS-SUB2) GREATER THAN ZEROS
04250          IF WS-CREFUND2 (WS-SUB2) = WS-ALL-NINES OR
04251             WS-CREFUND2 (WS-SUB2) GREATER THAN WS-ALL-NINES
04252             SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED
04253             MOVE ZEROS            TO PB-C-AH-CANCEL-AMT
04254             MOVE '?'              TO PB-C-AH-CALC-REQ
04255          ELSE
04256             SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED
04257             ADD WS-CREFUND2  (WS-SUB2) TO PI-AH-CAN-ENTERED
04258             MOVE WS-CREFUND2 (WS-SUB2) TO PB-C-AH-CANCEL-AMT
04259             MOVE SPACE                 TO PB-C-AH-CALC-REQ.
04260
04261 ******************************************************************
04262 *      IF CANCEL DATE = SPACES (LOW-VALUES) DELETE COVERAGE.     *
04263 ******************************************************************
04264
04265      IF CCANDT1-LEN (WS-SUB2) GREATER THAN ZEROS
04266         MOVE WS-CONVERTED-CANDT1 (WS-SUB2) TO PB-C-LF-CANCEL-DT
04267         IF   WS-CONVERTED-CANDT1 (WS-SUB2) = LOW-VALUES
04268              SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED
04269              MOVE ZEROS          TO PB-C-LF-REF-CALC
04270                                     PB-C-LF-CANCEL-AMT.
04271
04272      IF CCANDT2-LEN (WS-SUB2) GREATER THAN ZEROS
04273         MOVE WS-CONVERTED-CANDT2 (WS-SUB2) TO PB-C-AH-CANCEL-DT
04274         IF   WS-CONVERTED-CANDT2 (WS-SUB2) = LOW-VALUES
04275              SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED
04276              MOVE ZEROS          TO PB-C-AH-REF-CALC
04277                                     PB-C-AH-CANCEL-AMT.
04278
04279      IF CLIVES-LEN  (WS-SUB2) GREATER THAN ZEROS
04280         MOVE WS-CLIVES (WS-SUB2) TO PB-C-LIVES.
04281
04282      IF CPAYEE-LEN  (WS-SUB2) GREATER THAN ZEROS
04283         MOVE CPAYEE (WS-SUB2)    TO PB-C-PAYEE-CODE.
04284
04285      IF CCHK-LEN    (WS-SUB2) GREATER THAN ZEROS
04286         MOVE CCHK   (WS-SUB2)    TO PB-C-REFUND-SW.
04287      MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.
04288      MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.
04289
04290      MOVE 'C'                    TO JP-RECORD-TYPE.
04291      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
04292      MOVE ERPNDT-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
04293      MOVE FILE-ID-ERPNDT         TO JP-FILE-ID.
04294
04295      
      * EXEC CICS REWRITE
04296 *        DATASET (FILE-ID-ERPNDT)
04297 *        FROM    (PENDING-BUSINESS)
04298 *        END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00009420' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDT, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04299
04300      MOVE ERPNDT-KEY             TO PI-SAV-ENDING-ERPNDT-KEY.
04301
04302      PERFORM 8400-LOG-JOURNAL-RECORD.
04303
04304      MOVE LOW-VALUES             TO DATA-AREA-C (WS-SUB2).
04305
04306      IF EIBAID = DFHENTER
04307          MOVE PI-NEXT-DISPLAY-SEQ-NO TO CSEQ (WS-SUB2)
04308          ADD +1 TO               PI-NEXT-DISPLAY-SEQ-NO
04309          MOVE AL-SABON               TO CSEQ-ATTRB (WS-SUB2).
04310
04311      GO TO 5900-EXIT.
04312
04313      EJECT
04314
04315 ******************************************************************
04316 *                                                                *
04317 *            B U I L D   C A N C E L   R E C O R D               *
04318 *                                                                *
04319 ******************************************************************
04320
04321  5100-ADD-CANCEL-RECORD.
04322
04323      
      * EXEC CICS GETMAIN
04324 *        SET     (ERPNDT-POINTER)
04325 *        LENGTH  (ERPNDT-RECORD-LENGTH)
04326 *        INITIMG (GETMAIN-SPACE)
04327 *        END-EXEC.
      *    MOVE ',"IL                  $   #00009448' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-POINTER, 
                 ERPNDT-RECORD-LENGTH, 
                 GETMAIN-SPACE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04328
04329      SERVICE RELOAD PENDING-BUSINESS.
04330
04331      MOVE 'PB'                   TO PB-RECORD-ID.
04332      MOVE PI-COMPANY-CD          TO PB-COMPANY-CD
04333                                     PB-COMPANY-CD-A1.
04334      MOVE PI-COMPANY-ID          TO PB-COMPANY-ID.
04335      MOVE PI-SAV-ENTRY-BATCH     TO PB-ENTRY-BATCH.
04336      MOVE CSEQ (WS-SUB2)            TO PB-BATCH-SEQ-NO.
04337
04338      IF CSEQ (WS-SUB2) GREATER THAN PI-LAST-SEQ-NO-ADDED
04339         MOVE CSEQ (WS-SUB2)      TO PI-LAST-SEQ-NO-ADDED.
04340
04341      IF CCAR-LEN  (WS-SUB2) GREATER THAN ZEROS
04342         MOVE CCAR (WS-SUB2)      TO PB-CARRIER.
04343
04344      IF CGRP-LEN  (WS-SUB2) GREATER THAN ZEROS
04345         MOVE CGRP (WS-SUB2)      TO PB-GROUPING.
04346
04347      IF CST-LEN  (WS-SUB2) GREATER THAN ZEROS
04348         MOVE CST (WS-SUB2)       TO PB-STATE.
04349
04350      IF CACCT-LEN     (WS-SUB2) GREATER THAN ZEROS
04351         MOVE CACCT    (WS-SUB2)  TO PB-ACCOUNT.
04352
04353      MOVE '2'                    TO PB-RECORD-TYPE.
04354      MOVE CCERT (WS-SUB2)        TO PB-CERT-PRIME.
04355      MOVE WS-CONVERTED-EFFDT     TO PB-CERT-EFF-DT.
04356
04357      MOVE ZEROS                  TO PB-BATCH-CHG-SEQ-NO
04358                                     PB-ALT-CHG-SEQ-NO.
04359
04360
04361      MOVE ZEROS                  TO PB-C-LF-REF-CALC
04362                                     PB-C-AH-REF-CALC
04363                                     PB-CI-INSURED-AGE
04364                                     PB-CI-LF-TERM
04365                                     PB-CI-AH-TERM
04366                                     PB-CI-LF-BENEFIT-CD
04367                                     PB-CI-LF-BENEFIT-AMT
04368                                     PB-CI-LF-ALT-BENEFIT-AMT
04369                                     PB-CI-LF-PREMIUM-AMT
04370                                     PB-CI-LF-ALT-PREMIUM-AMT
04371                                     PB-CI-AH-BENEFIT-CD
04372                                     PB-CI-AH-BENEFIT-AMT
04373                                     PB-CI-AH-PREMIUM-AMT
04374                                     PB-CI-PAY-FREQUENCY
04375                                     PB-CI-LOAN-APR
04376                                     PB-CI-LOAN-TERM
04377                                     PB-CI-LIFE-COMMISSION
04378                                     PB-CI-AH-COMMISSION
04379                                     PB-CI-CURR-SEQ
04380                                     PB-CI-AH-CANCEL-AMT
04381                                     PB-CI-LF-CANCEL-AMT
04382                                     PB-CI-RATE-DEV-PCT-LF
04383                                     PB-CI-RATE-DEV-PCT-AH
04384                                     PB-CI-EXTENTION-DAYS
04385                                     PB-CI-TERM-IN-DAYS
04386                                     PB-CI-LIVES
04387                                     PB-CI-LF-CRIT-PER
04388                                     PB-CI-AH-CRIT-PER
04389                                     PB-C-LF-REM-TERM
04390                                     PB-C-AH-REM-TERM
04391                                     PB-CHG-COUNT
04392                                     PB-LF-BILLED-AMTS
04393                                     PB-AH-BILLED-AMTS
04394                                     PB-CALC-TOLERANCE.
04395
04396      MOVE LOW-VALUES             TO PB-CI-AH-PAID-THRU-DT
04397                                     PB-CI-AH-SETTLEMENT-DT
04398                                     PB-CI-DEATH-DT
04399                                     PB-CI-LF-PRIOR-CANCEL-DT
04400                                     PB-CI-AH-PRIOR-CANCEL-DT
04401                                     PB-CI-ENTRY-DT
04402                                     PB-CI-LF-EXPIRE-DT
04403                                     PB-CI-AH-EXPIRE-DT
04404                                     PB-CI-LOAN-1ST-PMT-DT
04405                                     PB-C-LF-CANCEL-DT
04406                                     PB-C-AH-CANCEL-DT
04407                                     PB-CREDIT-ACCEPT-DT
04408                                     PB-BILLED-DT
04409                                     PB-ACCT-EFF-DT
04410                                     PB-ACCT-EXP-DT.
04411
04412      IF PI-NB-MONTH-END-DT NOT = SPACES
04413         MOVE PI-NB-MONTH-END-DT  TO PB-CREDIT-SELECT-DT
04414        ELSE
04415         MOVE PI-CR-MONTH-END-DT  TO PB-CREDIT-SELECT-DT.
04416
04417      MOVE 'X'                    TO PB-FATAL-FLAG.
04418
04419
04420      IF CSFX-LEN  (WS-SUB2) GREATER THAN ZEROS
04421         MOVE CSFX (WS-SUB2)      TO PB-CERT-SFX.
04422
04423      IF CLAST-NAME-LEN (WS-SUB2) GREATER THAN ZEROS
04424          MOVE CLAST-NAME (WS-SUB2)  TO PB-C-LAST-NAME.
04425
04426      IF CCANDT1-LEN (WS-SUB2) GREATER THAN ZEROS
04427         MOVE WS-CONVERTED-CANDT1 (WS-SUB2) TO PB-C-LF-CANCEL-DT.
04428
04429      IF CCANDT2-LEN (WS-SUB2) GREATER THAN ZEROS
04430         MOVE WS-CONVERTED-CANDT2 (WS-SUB2) TO PB-C-AH-CANCEL-DT.
04431
04432      IF CREFUND1-LEN   (WS-SUB2) GREATER THAN ZEROS
04433          IF WS-CREFUND1 (WS-SUB2) = WS-ALL-NINES OR
04434             WS-CREFUND1 (WS-SUB2) GREATER THAN WS-ALL-NINES
04435             MOVE ZEROS            TO PB-C-LF-CANCEL-AMT
04436             MOVE '?'              TO PB-C-LF-CALC-REQ
04437          ELSE
04438             ADD  WS-CREFUND1  (WS-SUB2) TO PI-LF-CAN-ENTERED
04439             MOVE WS-CREFUND1  (WS-SUB2) TO PB-C-LF-CANCEL-AMT
04440      ELSE
04441          MOVE ZEROS            TO PB-C-LF-CANCEL-AMT.
04442
04443      IF CREFUND2-LEN   (WS-SUB2) GREATER THAN ZEROS
04444          IF WS-CREFUND2 (WS-SUB2) = WS-ALL-NINES OR
04445             WS-CREFUND2 (WS-SUB2) GREATER THAN WS-ALL-NINES
04446             MOVE ZEROS            TO PB-C-AH-CANCEL-AMT
04447             MOVE '?'              TO PB-C-AH-CALC-REQ
04448          ELSE
04449             ADD  WS-CREFUND2  (WS-SUB2) TO PI-AH-CAN-ENTERED
04450             MOVE WS-CREFUND2  (WS-SUB2) TO PB-C-AH-CANCEL-AMT
04451      ELSE
04452          MOVE ZEROS              TO PB-C-AH-CANCEL-AMT.
04453
04454
04455      IF CLIVES-LEN  (WS-SUB2) GREATER THAN ZEROS
04456         MOVE WS-CLIVES (WS-SUB2) TO PB-C-LIVES
04457      ELSE
04458         MOVE ZEROS               TO PB-C-LIVES.
04459
04460      IF CPAYEE-LEN  (WS-SUB2) GREATER THAN ZEROS
04461         MOVE CPAYEE (WS-SUB2)    TO PB-C-PAYEE-CODE.
04462
04463      IF CCHK-LEN    (WS-SUB2) GREATER THAN ZEROS
04464         MOVE CCHK   (WS-SUB2)    TO PB-C-REFUND-SW.
04465
04466      MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.
04467      MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.
04468
04469      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY
04470                                     PB-INPUT-BY.
04471      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.
04472      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT
04473                                     PB-INPUT-DT.
04474      MOVE 'A'                    TO JP-RECORD-TYPE.
04475      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
04476      MOVE ERPNDT-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
04477      MOVE FILE-ID-ERPNDT         TO JP-FILE-ID.
04478      MOVE PB-CONTROL-PRIMARY     TO PI-SAV-ENDING-ERPNDT-KEY.
04479      ADD +1                      TO PI-SAV-BATCH-SEQ.
04480
04481      
      * EXEC CICS HANDLE CONDITION
04482 *        DUPREC (5200-DUPLICATE-ALT-INDEX)
04483 *        END-EXEC.
      *    MOVE '"$%                   ! 0 #00009606' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303039363036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04484
04485      
      * EXEC CICS WRITE
04486 *        DATASET (FILE-ID-ERPNDT)
04487 *        FROM    (PENDING-BUSINESS)
04488 *        RIDFLD  (PB-CONTROL-PRIMARY)
04489 *        END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00009610' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039363130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDT, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 PB-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04490
04491      ADD +1                      TO PI-CAN-CNT-ENTERED.
04492
04493      PERFORM 8400-LOG-JOURNAL-RECORD.
04494
04495      MOVE LOW-VALUES             TO DATA-AREA-C (WS-SUB2).
04496      MOVE PI-NEXT-DISPLAY-SEQ-NO TO CSEQ        (WS-SUB2).
04497      MOVE AL-SABON               TO CSEQ-ATTRB  (WS-SUB2).
04498
04499      ADD +1 TO                   PI-NEXT-DISPLAY-SEQ-NO.
04500
04501      GO TO 5900-EXIT.
04502
04503  5200-DUPLICATE-ALT-INDEX.
04504
04505      MOVE ER-2247                TO EMI-ERROR.
04506      MOVE -1                     TO CCERT-LEN    (WS-SUB2).
04507      MOVE AL-UABON               TO CCERT-ATTRB  (WS-SUB2).
04508      MOVE AL-UNBON               TO CEFFDT-ATTRB (WS-SUB2).
04509      MOVE 'Y'                    TO PI-ERROR-SW.
04510
04511      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04512
04513      IF CREFUND1-LEN (WS-SUB2) GREATER THAN ZEROS
04514          SUBTRACT WS-CREFUND1 (WS-SUB2) FROM PI-LF-CAN-ENTERED.
04515
04516      IF CREFUND2-LEN (WS-SUB2) GREATER THAN ZEROS
04517          SUBTRACT WS-CREFUND2 (WS-SUB2) FROM PI-AH-CAN-ENTERED.
04518
04519  5900-EXIT.
04520
04521      EXIT.
04522
04523      EJECT
04524
04525 ******************************************************************
04526 *                                                                *
04527 *   D E L E T E  P E N D I N G   B U S I N E S S    R E C O R D  *
04528 *                                                                *
04529 ******************************************************************
04530
04531  6000-DELETE-PEND-BUS-RECORD.
04532
04533      MOVE PI-COMPANY-CD          TO ERPNDT-COMP-CD.
04534      MOVE PI-SAV-ENTRY-BATCH     TO ERPNDT-ENTRY-BATCH.
04535
04536      IF PI-MAP-NAME = EL930B
04537          MOVE BSEQ               TO ERPNDT-BATCH-SEQ
04538      ELSE
04539          MOVE CSEQ (1)           TO ERPNDT-BATCH-SEQ.
04540
04541      
      * EXEC CICS HANDLE CONDITION
04542 *        NOTFND (6990-REC-NOTFND)
04543 *        END-EXEC.
      *    MOVE '"$I                   ! 1 #00009666' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303039363636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04544
04545      
      * EXEC CICS READ
04546 *        SET     (ERPNDT-POINTER)
04547 *        DATASET (FILE-ID-ERPNDT)
04548 *        RIDFLD  (ERPNDT-KEY)
04549 *        UPDATE
04550 *        END-EXEC.
      *    MOVE '&"S        EU         (   #00009670' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039363730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDT, 
                 ERPNDT-POINTER, 
                 DFHEIV99, 
                 ERPNDT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04551
04552      SERVICE RELOAD PENDING-BUSINESS.
04553
04554 ******************************************************************
04555 *                                                                *
04556 *    PENDING BUSINESS RECORD CAN NOT BE DELETED THROUGH DATA     *
04557 *    ENTRY IF THE RECORD HAS BEEN EDITED.  IF THE RECORD HAS     *
04558 *    BEEN EDITED, THE CURRENT BUSINESS RECORD CAN ONLY BE DELETED*
04559 *    THRUOUGH REVIEW AND CORRECTION.                             *
04560 *                                                                *
04561 ******************************************************************
04562
04563      IF  PB-ACCT-EFF-DT = LOW-VALUES NEXT SENTENCE
04564         ELSE
04565          GO TO 6880-DELETE-ERROR.
04566
04567      IF PB-ISSUE
04568          SUBTRACT PB-I-LF-PREMIUM-AMT     FROM PI-LF-ISS-ENTERED
04569          SUBTRACT PB-I-LF-ALT-PREMIUM-AMT FROM PI-LF-ISS-ENTERED
04570          SUBTRACT PB-I-AH-PREMIUM-AMT     FROM PI-AH-ISS-ENTERED
04571          SUBTRACT +1 FROM PI-ISS-CNT-ENTERED
04572      ELSE
04573          SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED
04574          SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED
04575          SUBTRACT +1 FROM PI-CAN-CNT-ENTERED.
04576
04577
04578  6300-DELETE-PB-RECORD.
04579
04580      MOVE 'D'                    TO JP-RECORD-TYPE.
04581      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
04582      MOVE ERPNDT-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
04583      MOVE FILE-ID-ERPNDT         TO JP-FILE-ID.
04584
04585      
      * EXEC CICS DELETE
04586 *        DATASET (FILE-ID-ERPNDT)
04587 *        END-EXEC.
      *    MOVE '&(                    &   #00009710' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303039373130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDT, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04588
04589      PERFORM 8400-LOG-JOURNAL-RECORD.
04590
04591      MOVE 'Y'                    TO PI-UPDATE-SW.
04592      MOVE ER-0000                TO EMI-ERROR
04593
04594      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04595
04596      ADD +1, PI-LAST-SEQ-NO-ADDED
04597                      GIVING PI-NEXT-DISPLAY-SEQ-NO.
04598
04599      IF PI-MAP-NAME = EL930B
04600          MOVE LOW-VALUES         TO MAP-B
04601          PERFORM 8550-SET-MAP-SEQ-NOS
04602      ELSE
04603          MOVE LOW-VALUES         TO MAP-C
04604          PERFORM 8550-SET-MAP-SEQ-NOS
04605                  VARYING WS-SUB2 FROM +1 BY +1
04606                  UNTIL WS-SUB2 GREATER THAN +2.
04607
04608      GO TO 8100-SEND-INITIAL-MAP.
04609
04610  6880-DELETE-ERROR.
04611
04612      
      * EXEC CICS UNLOCK
04613 *         DATASET (FILE-ID-ERPNDT)
04614 *         END-EXEC.
      *    MOVE '&*                    #   #00009737' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303039373337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDT, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04615
04616      MOVE ER-2901        TO EMI-ERROR.
04617      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04618      IF PI-MAP-NAME = EL930B
04619          MOVE -1                 TO BPFENTRL
04620      ELSE
04621          MOVE -1                 TO CPFENTRL.
04622
04623      GO TO 8200-SEND-DATAONLY.
04624
04625  6990-REC-NOTFND.
04626
04627      MOVE ER-2433                TO EMI-ERROR
04628
04629      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04630
04631      IF PI-MAP-NAME = EL930B
04632          MOVE -1                 TO BPFENTRL
04633      ELSE
04634          MOVE -1                 TO CPFENTRL.
04635
04636      GO TO 8200-SEND-DATAONLY.
04637
04638      EJECT
04639
04640 ******************************************************************
04641 *                                                                *
04642 *           F O R M A T   I S S U E   S C R E E N                *
04643 *                                                                *
04644 ******************************************************************
04645
04646  7000-FORMAT-ISSUE-SCREEN.
04647
04648      MOVE 'Y'                        TO PI-DISPLAY-SW.
04649      MOVE LOW-VALUES                 TO DATA-AREA-B.
04650      MOVE -1                         TO BPFENTRL.
04651      MOVE PB-BATCH-SEQ-NO            TO BSEQ.
04652      MOVE AL-SABON                   TO BSEQ-ATTRB.
04653
04654      MOVE PB-CARRIER                 TO BCAR.
04655      MOVE AL-SANOF                   TO BCAR-ATTRB.
04656      MOVE PB-GROUPING                TO BGRP.
04657      MOVE AL-SANOF                   TO BGRP-ATTRB.
04658      MOVE PB-STATE                   TO BST.
04659      MOVE AL-SANOF                   TO BST-ATTRB.
04660      MOVE PB-ACCOUNT                 TO BACCT.
04661      MOVE AL-SANOF                   TO BACCT-ATTRB.
04662
04663      MOVE PB-CERT-PRIME              TO BCERT.
04664      MOVE AL-SANON                   TO BCERT-ATTRB.
04665      MOVE PB-CERT-SFX                TO BSFX.
04666      MOVE AL-SANOF                   TO BSFX-ATTRB.
04667      MOVE PB-CERT-EFF-DT             TO DC-BIN-DATE-1.
04668      MOVE SPACE                      TO DC-OPTION-CODE.
04669      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
04670      MOVE DC-GREG-DATE-1-MDY         TO BEFFDT.
04671      MOVE AL-SANOF                   TO BEFFDT-ATTRB.
04672
04673      MOVE PI-LIFE-OVERRIDE-L2        TO BKIND (1).
04674      MOVE PI-AH-OVERRIDE-L2          TO BKIND(2).
04675
04676      IF PB-I-INSURED-LAST-NAME GREATER THAN SPACES
04677         MOVE PB-I-INSURED-LAST-NAME  TO BLAST-NAME.
04678
04679      IF PB-I-INSURED-FIRST-NAME GREATER THAN SPACES
04680         MOVE PB-I-INSURED-FIRST-NAME TO B1ST-NAME.
04681
04682      IF PB-I-INSURED-MIDDLE-INIT GREATER THAN SPACES
04683         MOVE PB-I-INSURED-MIDDLE-INIT TO BINIT.
04684
04685      IF PB-I-AGE GREATER THAN ZEROS
04686         MOVE PB-I-AGE                TO BAGE.
04687
04688      IF PB-I-JOINT-AGE GREATER THAN ZEROS
04689         MOVE PB-I-JOINT-AGE          TO BJNT-AGE.
04690
04691      IF PB-I-BIRTHDAY NOT = LOW-VALUES
04692         MOVE PB-I-BIRTHDAY           TO DC-BIN-DATE-1
04693         MOVE SPACE                   TO DC-OPTION-CODE
04694         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
04695         MOVE DC-GREG-DATE-1-MDY      TO BBIRTH-DT.
04696
04697      IF PB-I-INSURED-SEX GREATER THAN SPACES
04698         MOVE PB-I-INSURED-SEX        TO BSEX.
04699
04700      IF PB-I-LF-TERM GREATER THAN ZEROS
04701         MOVE PB-I-LF-TERM            TO BTERMO (1).
04702
04703      IF PB-I-AH-TERM GREATER THAN ZEROS
04704         MOVE PB-I-AH-TERM            TO BTERMO (2).
04705
04706      IF PB-I-LOAN-TERM GREATER THAN ZEROS
04707         MOVE PB-I-LOAN-TERM          TO BLN-TERMO.
04708
04709      IF PB-I-PAY-FREQUENCY GREATER THAN ZEROS
04710         MOVE PB-I-PAY-FREQUENCY      TO BFREQO.
04711
04712      IF PB-I-SKIP-CODE GREATER THAN SPACES
04713         MOVE PB-I-SKIP-CODE          TO BSKPCD.
04714
04715      IF PB-I-TERM-TYPE GREATER THAN SPACES
04716         MOVE PB-I-TERM-TYPE          TO BMODE.
04717
04718      IF PB-I-NO-OF-PAYMENTS GREATER THAN ZEROS
04719         MOVE PB-I-NO-OF-PAYMENTS     TO BPMTS-OUT.
04720
04721      IF PB-I-POLICY-FORM-NO GREATER THAN SPACES
04722         MOVE PB-I-POLICY-FORM-NO     TO BPOLICY.
04723
04724      IF PB-I-LF-INPUT-CD GREATER THAN SPACES
04725         MOVE PB-I-LF-INPUT-CD        TO BTYPE (1).
04726
04727      IF PB-I-LF-BENEFIT-AMT GREATER THAN ZEROS
04728         MOVE PB-I-LF-BENEFIT-AMT     TO BBENO (1).
04729
04730      IF PB-I-LF-ALT-BENEFIT-AMT GREATER THAN ZEROS
04731         MOVE PB-I-LF-ALT-BENEFIT-AMT TO BALT-BENO (1).
04732
04733      IF PB-I-LF-PREMIUM-AMT GREATER THAN ZEROS
04734         MOVE PB-I-LF-PREMIUM-AMT     TO BPREMO (1).
04735
04736      IF PB-I-LF-ALT-PREMIUM-AMT GREATER THAN ZEROS
04737         MOVE PB-I-LF-ALT-PREMIUM-AMT TO BALT-PREMO (1).
04738
04739      IF PB-I-AH-INPUT-CD GREATER THAN SPACES
04740         MOVE PB-I-AH-INPUT-CD        TO BTYPE (2).
04741
04742      IF PB-I-AH-BENEFIT-AMT GREATER THAN ZEROS
04743         MOVE PB-I-AH-BENEFIT-AMT     TO BBENO (2).
04744
04745      IF PB-I-AH-PREMIUM-AMT GREATER THAN ZEROS
04746         MOVE PB-I-AH-PREMIUM-AMT     TO BPREMO (2).
04747
04748      IF PB-I-LF-CRIT-PER GREATER THAN ZEROS
04749         MOVE PB-I-LF-CRIT-PER        TO BCRIT-PERDO (1).
04750
04751      IF PB-I-AH-CRIT-PER GREATER THAN ZEROS
04752         MOVE PB-I-AH-CRIT-PER           TO BCRIT-PERDO (2).
04753
04754      IF PB-BATCH-ENTRY GREATER THAN SPACES
04755         MOVE PB-BATCH-ENTRY          TO BENTRY.
04756
04757      IF PB-I-SPECIAL-REIN-CODE GREATER THAN SPACE
04758        MOVE PB-I-SPECIAL-REIN-CODE   TO BRINCD.
04759
04760      IF PB-I-INDV-GRP-OVRD GREATER THAN SPACES
04761         MOVE PB-I-INDV-GRP-OVRD      TO BIND-GRP.
04762
04763      IF PB-I-RATE-CLASS-OVRD GREATER THAN SPACES
04764         MOVE PB-I-RATE-CLASS-OVRD    TO BRTCLS.
04765
04766      IF PB-I-SIG-SW GREATER THAN SPACES
04767          MOVE PB-I-SIG-SW            TO BSIG.
04768
04769      IF PB-I-LOAN-APR GREATER THAN ZEROS
04770         MOVE PB-I-LOAN-APR           TO BAPR-OUT.
04771
04772      IF PB-I-SOC-SEC-NO GREATER THAN SPACES
04773         MOVE PB-I-SOC-SEC-NO         TO BSSNUM.
04774
04775      IF PB-I-MEMBER-NO GREATER THAN SPACES
04776         MOVE PB-I-MEMBER-NO          TO BMEM-NO.
04777
04778      IF PB-I-LOAN-OFFICER GREATER THAN SPACES
04779         MOVE PB-I-LOAN-OFFICER       TO BLN-OFFICER.
04780
04781      IF PB-I-LF-EXPIRE-DT NOT = LOW-VALUES
04782         MOVE PB-I-LF-EXPIRE-DT   TO DC-BIN-DATE-1
04783         MOVE SPACE               TO DC-OPTION-CODE
04784         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
04785         MOVE DC-GREG-DATE-1-MDY  TO BEXPIRE (1).
04786
04787      IF PB-I-AH-EXPIRE-DT NOT = LOW-VALUES
04788         MOVE PB-I-AH-EXPIRE-DT   TO DC-BIN-DATE-1
04789         MOVE SPACE               TO DC-OPTION-CODE
04790         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
04791         MOVE DC-GREG-DATE-1-MDY  TO BEXPIRE (2).
04792
04793      IF PB-I-1ST-PMT-DT GREATER THAN LOW-VALUES
04794         MOVE PB-I-1ST-PMT-DT     TO DC-BIN-DATE-1
04795         MOVE SPACE               TO DC-OPTION-CODE
04796         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
04797         MOVE DC-GREG-DATE-1-MDY  TO B1ST-PMT.
04798
04799      IF PB-I-EXTENTION-DAYS NUMERIC
04800         IF PB-I-EXTENTION-DAYS NOT = ZEROS
04801            MOVE PB-I-EXTENTION-DAYS  TO BDAYSO
04802         ELSE
04803            IF PB-I-TERM-IN-DAYS NUMERIC
04804               IF PB-I-TERM-IN-DAYS NOT = ZEROS
04805                  MOVE PB-I-TERM-IN-DAYS TO BDAYSO.
04806
04807      IF PB-I-LIVES GREATER THAN ZEROS
04808         MOVE PB-I-LIVES              TO BLIVESO.
04809
04810      IF PB-I-JOINT-FIRST-NAME GREATER THAN SPACES
04811         MOVE PB-I-JOINT-FIRST-NAME   TO BJNT-1ST-NAME.
04812
04813      IF PB-I-JOINT-MIDDLE-INIT GREATER THAN SPACES
04814         MOVE PB-I-JOINT-MIDDLE-INIT  TO BJNT-INIT.
04815
04816      IF PB-I-JOINT-LAST-NAME GREATER THAN SPACES
04817         MOVE PB-I-JOINT-LAST-NAME    TO BJNT-LST-NAME.
04818
04819      IF PB-I-BENEFICIARY-NAME GREATER THAN SPACES
04820         MOVE PB-I-BENEFICIARY-NAME   TO BBENEFICIARY.
04821
04822      MOVE PB-CONTROL-PRIMARY         TO ERPNDM-KEY.
04823
04824      
      * EXEC CICS HANDLE CONDITION
04825 *        NOTFND (7090-EXIT)
04826 *        END-EXEC.
      *    MOVE '"$I                   ! 2 #00009949' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3220233030303039393439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04827
04828      
      * EXEC CICS READ
04829 *        SET     (ERPNDM-POINTER)
04830 *        DATASET (FILE-ID-ERPNDM)
04831 *        RIDFLD  (ERPNDM-KEY)
04832 *        UPDATE
04833 *        END-EXEC.
      *    MOVE '&"S        EU         (   #00009953' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039393533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDM, 
                 ERPNDM-POINTER, 
                 DFHEIV99, 
                 ERPNDM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04834
04835      SERVICE RELOAD PENDING-MAILING-DATA.
04836
04837      IF PM-ADDRESS-LINE-1 GREATER THAN SPACES
04838         MOVE PM-ADDRESS-LINE-1       TO BADDRS1.
04839
04840      IF PM-ADDRESS-LINE-2 GREATER THAN SPACES
04841         MOVE PM-ADDRESS-LINE-2       TO BADDRS2.
04842
04843      IF PM-CITY-STATE GREATER THAN SPACES
04844         MOVE PM-CITY-STATE           TO BCITYST.
04845
04846      IF PM-CANADIAN-POST-CODE
04847         MOVE PM-CAN-POST1            TO BZIP5
04848         MOVE PM-CAN-POST2            TO BZIP4
04849         MOVE SPACES                  TO BDASH
04850
04851      ELSE
04852          IF PM-ZIP-CODE GREATER THAN SPACES
04853             MOVE PM-ZIP-CODE         TO BZIP5
04854
04855             IF  PM-ZIP-PLUS4 GREATER THAN SPACES
04856                 MOVE '-'             TO BDASH
04857                 MOVE PM-ZIP-PLUS4    TO BZIP4
04858
04859             ELSE
04860                 MOVE SPACES          TO BDASH
04861                                         BZIP4
04862
04863         ELSE
04864             MOVE SPACES              TO BDASH
04865                                         BZIP4
04866                                         BZIP5.
04867
04868      IF PM-PHONE-NO NUMERIC
04869         IF PM-PHONE-NO GREATER THAN ZEROS
04870            MOVE PM-PHONE-NO          TO  BPHONE-NO
uktdel*          TRANSFORM BPHONE-NO FROM ' ' TO '-'.
uktins           INSPECT BPHONE-NO REPLACING ALL ' ' BY '-'.
04872
04873  7090-EXIT.
04874
04875      EXIT.
04876
04877      EJECT
04878
04879 ******************************************************************
04880 *                                                                *
04881 *           F O R M A T   C A N C E L   S C R E E N              *
04882 *                                                                *
04883 ******************************************************************
04884
04885  7100-FORMAT-CANCEL-SCREEN.
04886
04887      MOVE 'Y'                    TO PI-DISPLAY-SW.
04888
04889      MOVE LOW-VALUES             TO DATA-AREA-C (2)
uktdel*                                TO DATA-AREA-C (3).
uktins                                    DATA-AREA-C (3).
04891
04892      MOVE -1                     TO CPFENTRL.
04893
04894      MOVE PB-BATCH-SEQ-NO        TO CSEQ        (1).
04895      MOVE AL-SABON               TO CSEQ-ATTRB  (1).
04896
04897      MOVE PB-CARRIER             TO CCAR        (1).
04898      MOVE AL-SANOF               TO CCAR-ATTRB  (1).
04899      MOVE PB-GROUPING            TO CGRP        (1).
04900      MOVE AL-SANOF               TO CGRP-ATTRB  (1).
04901      MOVE PB-STATE               TO CST         (1).
04902      MOVE AL-SANOF               TO CST-ATTRB   (1).
04903      MOVE PB-ACCOUNT             TO CACCT       (1).
04904      MOVE AL-SANOF               TO CACCT-ATTRB (1).
04905
04906      MOVE PB-CERT-PRIME          TO CCERT       (1).
04907      MOVE AL-SANON               TO CCERT-ATTRB (1).
04908      MOVE PB-CERT-SFX            TO CSFX        (1).
04909      MOVE AL-SANON               TO CSFX-ATTRB  (1).
04910
04911      MOVE PB-CERT-EFF-DT         TO DC-BIN-DATE-1.
04912      MOVE SPACE                  TO DC-OPTION-CODE.
04913      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
04914      MOVE AL-SANON               TO CEFFDT-ATTRB (1).
04915      MOVE DC-GREG-DATE-1-MDY     TO CEFFDT       (1).
04916      MOVE PB-C-LAST-NAME         TO CLAST-NAME   (1).
04917
04918
04919      IF PB-C-LF-CANCEL-DT NOT = LOW-VALUES
04920         MOVE PB-C-LF-CANCEL-DT   TO DC-BIN-DATE-1
04921         MOVE SPACE               TO DC-OPTION-CODE
04922         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
04923         MOVE DC-GREG-DATE-1-MDY  TO CCANDT1 (1)
04924         MOVE AL-UANON            TO CCANDT1-ATTRB (1).
04925
04926      IF PB-C-AH-CANCEL-DT NOT = LOW-VALUES
04927         MOVE PB-C-AH-CANCEL-DT   TO DC-BIN-DATE-1
04928         MOVE SPACE               TO DC-OPTION-CODE
04929         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
04930         MOVE DC-GREG-DATE-1-MDY  TO CCANDT2 (1)
04931         MOVE AL-UANON            TO CCANDT2-ATTRB (1).
04932
04933      IF PB-C-LF-CANCEL-AMT NOT =  ZEROS
04934         MOVE PB-C-LF-CANCEL-AMT  TO CREFUND1O (1)
04935         MOVE AL-UNNON            TO CREFUND1-ATTRB (1).
04936
04937      IF PB-C-AH-CANCEL-AMT NOT =  ZEROS
04938         MOVE PB-C-AH-CANCEL-AMT  TO CREFUND2O      (1)
04939         MOVE AL-UNNON            TO CREFUND2-ATTRB (1).
04940
04941      IF PB-C-LIVES GREATER THAN ZEROS
04942         MOVE PB-C-LIVES          TO CLIVESO      (1)
04943         MOVE AL-UNNON            TO CLIVES-ATTRB (1).
04944
04945      IF PB-C-PAYEE-CODE GREATER THAN SPACES
04946         MOVE PB-C-PAYEE-CODE     TO CPAYEE       (1)
04947         MOVE AL-UANON            TO CPAYEE-ATTRB (1).
04948
04949      IF PB-C-REFUND-SW  GREATER THAN SPACES
04950         MOVE PB-C-REFUND-SW      TO CCHK         (1)
04951         MOVE AL-UANON            TO CCHK-ATTRB   (1).
04952
04953      IF PB-C-LAST-NAME GREATER THAN SPACES
04954         MOVE PB-C-LAST-NAME      TO CLAST-NAME       (1)
04955         MOVE AL-UANON            TO CLAST-NAME-ATTRB (1).
04956
04957      PERFORM 7180-PROTECT-FIELDS VARYING WS-SUB2 FROM +2 BY +1
04958                                  UNTIL WS-SUB2 GREATER THAN +2.
04959
04960      GO TO 7190-EXIT.
04961
04962
04963  7180-PROTECT-FIELDS.
04964
04965      MOVE AL-SANOF               TO CCERT-ATTRB      (WS-SUB2)
04966                                     CSFX-ATTRB       (WS-SUB2)
04967                                     CEFFDT-ATTRB     (WS-SUB2)
04968                                     CLAST-NAME-ATTRB (WS-SUB2)
04969                                     CCANDT1-ATTRB    (WS-SUB2)
04970                                     CCANDT2-ATTRB    (WS-SUB2)
04971                                     CREFUND1-ATTRB   (WS-SUB2)
04972                                     CREFUND2-ATTRB   (WS-SUB2)
04973                                     CLIVES-ATTRB     (WS-SUB2).
04974
04975  7190-EXIT.
04976
04977      EJECT
04978
04979 ******************************************************************
04980 *                                                                *
04981 *    S  E N D    I N I T I A L   M A P   FOR    I S S U E S      *
04982 *                                                                *
04983 ******************************************************************
04984
04985  8100-SEND-INITIAL-MAP.
04986      IF PI-MAP-NAME = EL930B
04987          NEXT SENTENCE
04988      ELSE
04989          GO TO 8110-SEND-INITIAL-CANCEL-MAP.
04990
04991      MOVE PI-MEMBER-CAPTION        TO BCAPTNO.
04992
04993      IF EIBAID NOT = DFHPF5
04994        AND EIBAID NOT = DFHPF1
04995        AND EIBAID NOT = DFHPF2
04996        AND PI-MAINT-FUNC NOT = 'B'
04997          PERFORM 0600-PROTECT-FIELDS THRU 0600-EXIT.
04998
04999      MOVE PI-SAV-ENTRY-BATCH     TO BBATCHO.
05000      MOVE PI-CR-MONTH-END-DT     TO DC-BIN-DATE-1.
05001      MOVE SPACE                  TO DC-OPTION-CODE.
05002      PERFORM 8500-DATE-CONVERT.
05003      MOVE WS-CURRENT-DT          TO BDATEO.
05004      MOVE EIBTIME                TO TIME-IN.
05005      MOVE TIME-OUT               TO BTIMEO.
05006
05007      MOVE PI-LIFE-OVERRIDE-L2    TO BKIND (1).
05008      MOVE AL-SABOF               TO BKIND-ATTRB (1).
05009      MOVE PI-AH-OVERRIDE-L2      TO BKIND (2).
05010      MOVE AL-SABOF               TO BKIND-ATTRB (2).
05011
05012      IF PI-DATA-ERRORS
05013          MOVE SPACE              TO PI-ERROR-SW
05014      ELSE
05015          IF EIBAID = DFHPF1 OR DFHPF2
05016              NEXT SENTENCE
05017          ELSE
05018              IF CARR-GROUP-ST-ACCNT-CNTL
05019                  MOVE -1                     TO BCAR-LEN
05020              ELSE
05021                  IF ST-ACCNT-CNTL
05022                     MOVE -1                  TO BST-LEN
05023                     MOVE AL-SADOF            TO BCARHDA
05024                                                 BGRPHDA
05025                     MOVE AL-SANOF            TO BCARA
05026                                                 BGRPA
05027                  ELSE
05028                     IF CARR-ST-ACCNT-CNTL
05029                        MOVE -1               TO BCAR-LEN
05030                        MOVE AL-SADOF         TO BGRPHDA
05031                        MOVE AL-SANOF         TO BGRPA
05032                     ELSE
05033                        IF ACCNT-CNTL
05034                           MOVE -1            TO BACCT-LEN
05035                           MOVE AL-SADOF      TO BCARHDA
05036                                                 BGRPHDA
05037                                                 BSTHDA
05038                           MOVE AL-SANOF      TO BCARA
05039                                                 BGRPA
05040                                                 BSTA
05041                        ELSE
05042                           IF CARR-ACCNT-CNTL
05043                              MOVE AL-SADOF   TO BGRPHDA
05044                                                 BSTHDA
05045                              MOVE AL-SANOF   TO BGRPA
05046                                                 BSTA
05047                              MOVE -1         TO BCAR-LEN.
05048
05049
05050      MOVE EMI-MESSAGE-AREA (1)   TO BERMSG1O.
05051      MOVE EMI-MESSAGE-AREA (2)   TO BERMSG2O.
05052
05053      
      * EXEC CICS SEND
05054 *        MAP      (PI-MAP-NAME)
05055 *        MAPSET   (MAPSET-EL9301S)
05056 *        FROM     (DATA-ENTRY-MAP)
05057 *        ERASE
05058 *        CURSOR
05059 *        END-EXEC.
           MOVE LENGTH OF
            DATA-ENTRY-MAP
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00010180' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303130313830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 DATA-ENTRY-MAP, 
                 DFHEIV12, 
                 MAPSET-EL9301S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05060
05061      GO TO 9100-RETURN-TRAN.
05062
05063      EJECT
05064
05065 ******************************************************************
05066 *                                                                *
05067 *    S  E N D    I N I T I A L   M A P   FOR    C A N C E L S    *
05068 *                                                                *
05069 ******************************************************************
05070
05071  8110-SEND-INITIAL-CANCEL-MAP.
05072
05073      IF EIBAID NOT = DFHPF5
05074        AND EIBAID NOT = DFHPF1
05075        AND EIBAID NOT = DFHPF2
05076        AND PI-MAINT-FUNC NOT = 'B'
05077          PERFORM 0700-PROTECT-FIELDS THRU 0700-EXIT.
05078
05079      MOVE PI-SAV-ENTRY-BATCH     TO CBATCHO.
05080      MOVE PI-CR-MONTH-END-DT     TO DC-BIN-DATE-1.
05081      MOVE SPACE                  TO DC-OPTION-CODE.
05082      PERFORM 8500-DATE-CONVERT.
05083      MOVE WS-CURRENT-DT          TO CDATEO.
05084      MOVE EIBTIME                TO TIME-IN.
05085      MOVE TIME-OUT               TO CTIMEO.
05086
05087      MOVE PI-LIFE-OVERRIDE-L2    TO CKIND1 (1).
05088      MOVE AL-SABOF               TO CKIND1-ATTRB (1).
05089      MOVE PI-AH-OVERRIDE-L2      TO CKIND2 (1).
05090      MOVE AL-SABOF               TO CKIND2-ATTRB (1).
05091      MOVE PI-LIFE-OVERRIDE-L2    TO CKIND1 (2).
05092      MOVE AL-SABOF               TO CKIND1-ATTRB (2).
05093      MOVE PI-AH-OVERRIDE-L2      TO CKIND2 (2).
05094      MOVE AL-SABOF               TO CKIND2-ATTRB (2).
05095
05096      IF PI-DATA-ERRORS
05097          MOVE SPACE              TO PI-ERROR-SW
05098      ELSE
05099          IF EIBAID = DFHPF1 OR DFHPF2
05100              NEXT SENTENCE
05101          ELSE
05102              IF CARR-GROUP-ST-ACCNT-CNTL
05103                  MOVE -1                     TO CCAR1L
05104              ELSE
05105                  IF ST-ACCNT-CNTL
05106                     MOVE -1                  TO CST1L
05107                     MOVE AL-SADOF            TO CCARHD1A
05108                                                 CGRPHD1A
05109                     MOVE AL-SANOF            TO CCAR1A
05110                                                 CGRP1A
05111                     MOVE AL-SADOF            TO CCARHD2A
05112                                                 CGRPHD2A
05113                     MOVE AL-SANOF            TO CCAR2A
05114                                                 CGRP2A
05115                  ELSE
05116                     IF CARR-ST-ACCNT-CNTL
05117                        MOVE -1               TO CCAR1L
05118                        MOVE AL-SADOF         TO CGRPHD1A
05119                        MOVE AL-SANOF         TO CGRP1A
05120                        MOVE AL-SADOF         TO CGRPHD2A
05121                        MOVE AL-SANOF         TO CGRP2A
05122                     ELSE
05123                        IF ACCNT-CNTL
05124                           MOVE -1            TO CACCT1L
05125                           MOVE AL-SADOF      TO CCARHD1A
05126                                                 CGRPHD1A
05127                                                 CSTHD1A
05128                           MOVE AL-SANOF      TO CCAR1A
05129                           MOVE AL-SADOF      TO CCARHD2A
05130                                                 CGRPHD2A
05131                                                 CSTHD2A
05132                           MOVE AL-SANOF      TO CCAR2A
05133                                                 CGRP2A
05134                                                 CST2A
05135                        ELSE
05136                           IF CARR-ACCNT-CNTL
05137                              MOVE AL-SADOF   TO CGRPHD1A
05138                                                 CSTHD1A
05139                              MOVE AL-SANOF   TO CGRP1A
05140                                                 CST1A
05141                              MOVE AL-SADOF   TO CGRPHD2A
05142                                                 CSTHD2A
05143                              MOVE AL-SANOF   TO CGRP2A
05144                                                 CST2A
05145                              MOVE -1         TO CCAR1L.
05146
05147
05148
05149      MOVE EMI-MESSAGE-AREA (1)   TO CERMSG1O.
05150      MOVE EMI-MESSAGE-AREA (2)   TO CERMSG2O.
05151
05152      
      * EXEC CICS SEND
05153 *        MAP      (PI-MAP-NAME)
05154 *        MAPSET   (MAPSET-EL9301S)
05155 *        FROM     (DATA-ENTRY-MAP)
05156 *        ERASE
05157 *        CURSOR
05158 *        END-EXEC.
           MOVE LENGTH OF
            DATA-ENTRY-MAP
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00010279' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303130323739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 DATA-ENTRY-MAP, 
                 DFHEIV12, 
                 MAPSET-EL9301S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05159
05160      GO TO 9100-RETURN-TRAN.
05161
05162      EJECT
05163
05164 ******************************************************************
05165 *                                                                *
05166 *              S E N D    D A T A O N L Y                        *
05167 *                                                                *
05168 ******************************************************************
05169
05170  8200-SEND-DATAONLY.
05171
05172      MOVE SPACE              TO PI-ERROR-SW.
05173
05174      IF PI-MAP-NAME = EL930B
05175          MOVE PI-MEMBER-CAPTION      TO BCAPTNO
05176          MOVE WS-CURRENT-DT          TO BDATEO
05177          MOVE EIBTIME                TO TIME-IN
05178          MOVE TIME-OUT               TO BTIMEO
05179          MOVE EMI-MESSAGE-AREA (1)   TO BERMSG1O
05180          MOVE EMI-MESSAGE-AREA (2)   TO BERMSG2O
05181          
      * EXEC CICS SEND
05182 *            MAP      (PI-MAP-NAME)
05183 *            MAPSET   (MAPSET-EL9301S)
05184 *            FROM     (DATA-ENTRY-MAP)
05185 *            DATAONLY
05186 *            CURSOR
05187 *            END-EXEC
           MOVE LENGTH OF
            DATA-ENTRY-MAP
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00010308' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303130333038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 DATA-ENTRY-MAP, 
                 DFHEIV12, 
                 MAPSET-EL9301S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
05188      ELSE
05189          MOVE WS-CURRENT-DT          TO CDATEO
05190          MOVE EIBTIME                TO TIME-IN
05191          MOVE TIME-OUT               TO CTIMEO
05192          MOVE EMI-MESSAGE-AREA (1)   TO CERMSG1O
05193          MOVE EMI-MESSAGE-AREA (2)   TO CERMSG2O
05194          
      * EXEC CICS SEND
05195 *            MAP      (PI-MAP-NAME)
05196 *            MAPSET   (MAPSET-EL9301S)
05197 *            FROM     (DATA-ENTRY-MAP)
05198 *            DATAONLY
05199 *            CURSOR
05200 *            END-EXEC.
           MOVE LENGTH OF
            DATA-ENTRY-MAP
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00010321' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303130333231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 DATA-ENTRY-MAP, 
                 DFHEIV12, 
                 MAPSET-EL9301S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05201
05202      GO TO 9100-RETURN-TRAN.
05203
05204      EJECT
05205
05206 ******************************************************************
05207 *                                                                *
05208 *                U T I L I T Y   R O U T I N E S                 *
05209 *                                                                *
05210 ******************************************************************
05211
05212  8300-SEND-TEXT.
05213      
      * EXEC CICS SEND TEXT
05214 *        FROM     (LOGOFF-TEXT)
05215 *        LENGTH   (LOGOFF-LENGTH)
05216 *        ERASE
05217 *        FREEKB
05218 *        END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00010340' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303130333430' TO DFHEIV0(25:11)
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
           
05219      
      * EXEC CICS RETURN
05220 *        END-EXEC.
      *    MOVE '.(                    &   #00010346' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303130333436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05221
05222  8350-SEND-WARNING.
05223      
      * EXEC CICS SEND TEXT
05224 *        FROM     (WARNING-TEXT)
05225 *        LENGTH   (WARNING-LENGTH)
05226 *        ERASE
05227 *        FREEKB
05228 *        END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00010350' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303130333530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WARNING-TEXT, 
                 WARNING-LENGTH, 
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
           
05229
05230      GO TO 9100-RETURN-TRAN.
05231
05232
05233  8400-LOG-JOURNAL-RECORD.
05234      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.
05235      MOVE THIS-PGM                TO JP-PROGRAM-ID.
05236
05237 *    EXEC CICS JOURNAL
05238 *        JFILEID     (PI-JOURNAL-FILE-ID)
05239 *        JTYPEID     ('EL')
05240 *        FROM        (JOURNAL-RECORD)
05241 *        LENGTH      (WS-JOURNAL-RECORD-LENGTH)
05242 *        END-EXEC.
05243
05244  8500-DATE-CONVERT.
05245
05246      
      * EXEC CICS LINK
05247 *        PROGRAM  (LINK-ELDATCV)
05248 *        COMMAREA (DATE-CONVERSION-DATA)
05249 *        LENGTH   (DC-COMM-LENGTH) END-EXEC.
      *    MOVE '."C                   ''   #00010373' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303130333733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-ELDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05250
05251  8500-EXIT.
05252      EXIT.
05253
05254      EJECT
05255
05256  8550-SET-MAP-SEQ-NOS.
05257
05258      IF PI-MAP-NAME = EL930B
05259          MOVE PI-NEXT-DISPLAY-SEQ-NO TO BSEQ
05260          MOVE AL-SABON               TO BSEQ-ATTRB
05261      ELSE
05262          MOVE PI-NEXT-DISPLAY-SEQ-NO TO CSEQ (WS-SUB2)
05263          MOVE AL-SABON               TO CSEQ-ATTRB (WS-SUB2).
05264
05265      ADD +1  TO PI-NEXT-DISPLAY-SEQ-NO.
05266
05267  8555-EXIT.
05268      EXIT.
05269
05270  8600-DEEDIT.
05271
05272      
      * EXEC CICS BIF DEEDIT
05273 *        FIELD   (DEEDIT-FIELD)
05274 *        LENGTH  (15)
05275 *        END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00010399' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303130333939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05276
05277  8600-EXIT.
05278      EXIT.
05279
05280  8800-UNAUTHORIZED-ACCESS.
05281      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
05282      GO TO 8300-SEND-TEXT.
05283
05284  9000-RETURN-CICS.
05285      
      * EXEC CICS RETURN
05286 *        END-EXEC.
      *    MOVE '.(                    &   #00010412' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303130343132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05287
05288  9100-RETURN-TRAN.
05289      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
05290      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.
05291      
      * EXEC CICS RETURN
05292 *        TRANSID    (TRANS-EXI2)
05293 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
05294 *        LENGTH     (PI-COMM-LENGTH)
05295 *        END-EXEC.
      *    MOVE '.(CT                  &   #00010418' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303130343138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-EXI2, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05296
05297  9300-XCTL.
05298
05299      
      * EXEC CICS XCTL
05300 *        PROGRAM    (PGM-NAME)
05301 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
05302 *        LENGTH     (PI-COMM-LENGTH)
05303 *        END-EXEC.
      *    MOVE '.$C                   $   #00010426' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303130343236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05304
05305  9400-CLEAR.
05306
05307      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME
05308      GO TO 9300-XCTL.
05309
05310  9500-PF12.
05311
05312      MOVE XCTL-EL010             TO PGM-NAME.
05313      GO TO 9300-XCTL.
05314
05315  9600-PGMID-ERROR.
05316
05317      
      * EXEC CICS HANDLE CONDITION
05318 *        PGMIDERR    (8300-SEND-TEXT)
05319 *        END-EXEC.
      *    MOVE '"$L                   ! 3 #00010444' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3320233030303130343434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05320      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
05321      MOVE ' '                    TO PI-ENTRY-CD-1.
05322      MOVE XCTL-EL005            TO PGM-NAME.
05323      MOVE PGM-NAME               TO LOGOFF-PGM.
05324      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
05325      GO TO 9300-XCTL.
05326
05327  9900-ERROR-FORMAT.
05328
05329      IF PI-MAP-NAME = EL930B
05330         MOVE 2                   TO EMI-NUMBER-OF-LINES
05331        ELSE
05332         MOVE 1                   TO EMI-NUMBER-OF-LINES.
05333
05334      IF NOT EMI-ERRORS-COMPLETE
05335          MOVE LINK-EL001         TO PGM-NAME
05336          
      * EXEC CICS LINK
05337 *            PROGRAM    (PGM-NAME)
05338 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
05339 *            LENGTH     (EMI-COMM-LENGTH)
05340 *            END-EXEC.
      *    MOVE '."C                   ''   #00010463' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303130343633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05341  9900-EXIT.
05342      EXIT.
05343
05344  9990-ABEND.
05345
05346      MOVE LINK-EL004             TO PGM-NAME.
05347      MOVE DFHEIBLK               TO EMI-LINE1
05348      
      * EXEC CICS LINK
05349 *        PROGRAM   (PGM-NAME)
05350 *        COMMAREA  (EMI-LINE1)
05351 *        LENGTH    (72)
05352 *        END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00010475' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303130343735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05353      IF PI-MAP-NAME = EL930B
05354          MOVE -1 TO BPFENTRL
05355      ELSE
05356          MOVE -1 TO CPFENTRL.
05357      GO TO 8200-SEND-DATAONLY.
05358      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL9301' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
05359
05360  9995-SECURITY-VIOLATION.
05361 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00010505' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303130353035' TO DFHEIV0(25:11)
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
05362
05363  9995-EXIT.
05364      EXIT.
05365

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL9301' TO DFHEIV1
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
               GO TO 1078-NO-RECORD,
                     1078-NO-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1580-NO-CARRIER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 1680-NO-STATE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 1780-ACCOUNT-INVALID,
                     1780-ACCOUNT-INVALID
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 2020-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 2100-BROWSE-BKWD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 3300-REC-NOT-FND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 3200-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 4100-ADD-ISSUE-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 4185-ADD-MAILING-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 4200-DUPLICATE-ALT-INDEX
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 4185-ADD-MAILING-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 5100-ADD-CANCEL-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 5200-DUPLICATE-ALT-INDEX
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 6990-REC-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 7090-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL9301' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
