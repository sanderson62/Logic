00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL651.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 05/16/94 10:57:36.
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE
00008 *                            VMOD=2.019
00009
00010 *AUTHOR.     LOGIC,INC.
00011 *            DALLAS, TEXAS.
00012
00013 *DATE-COMPILED.
00014
00015 *SECURITY.   *****************************************************
00016 *            *                                                   *
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00018 *            *                                                   *
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00022 *            *                                                   *
00023 *            *****************************************************
00024
00025 *REMARKS.    TRANSACTION - EXD1 - REINSURANCE MASTER MAINT.
00026
00027      EJECT
00028  ENVIRONMENT DIVISION.
00029  DATA DIVISION.
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL651 WORKING STORAGE     *'.
00033  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.019 *********'.
00034
00035 *                          COPY ELCSCTM.
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
00036 *                          COPY ELCSCRTY.
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
00037      EJECT
00038  01  WS-DATE-AREA.
00039      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.
00040      05  SAVE-BIN-DATE               PIC X(2)    VALUE SPACES.
00041
00042  01  STANDARD-AREAS.
00043      05  GETMAIN-SPACE               PIC X       VALUE SPACE.
00044      05  MAP-NAME                    PIC X(8)    VALUE 'EL651A'.
00045      05  MAPSET-NAME                 PIC X(8)    VALUE 'EL651S'.
00046      05  TRANS-ID                    PIC X(4)    VALUE 'EXD1'.
00047      05  EL6592-TRANS-ID             PIC X(4)    VALUE 'EX66'.
00048      05  THIS-PGM                    PIC X(8)    VALUE 'EL651'.
00049      05  PGM-NAME                    PIC X(8).
00050      05  SUB1                        PIC S99     VALUE +0.
00051      05  SC-ITEM                     PIC S9(4) COMP VALUE +1.
00052      05  WS-SAVE-PI-SUB              PIC S99     VALUE +0.
00053
00054      05  NINES                       PIC S9(9)V99 VALUE
00055                                                  +999999999.99.
00056      05  TIME-IN                     PIC S9(7).
00057      05  TIME-OUT-R  REDEFINES TIME-IN.
00058          10  FILLER                  PIC X.
00059          10  TIME-OUT                PIC 99V99.
00060          10  FILLER                  PIC X(2).
00061      05  TIME-MT                     PIC S9(7).
00062      05  TIME-MT-R  REDEFINES TIME-MT.
00063          10  FILLER                  PIC X.
00064          10  TIME-LMT                PIC 99V99.
00065          10  FILLER                  PIC X(2).
00066      05  XCTL-005                    PIC X(8)    VALUE 'EL005'.
00067      05  XCTL-010                    PIC X(8)    VALUE 'EL010'.
00068      05  XCTL-626                    PIC X(8)    VALUE 'EL126'.
00069      05  XCTL-6511                   PIC X(8)    VALUE 'EL6511'.
00070      05  XCTL-657                    PIC X(8)    VALUE 'EL657'.
00071      05  LINK-001                    PIC X(8)    VALUE 'EL001'.
00072      05  LINK-004                    PIC X(8)    VALUE 'EL004'.
00073      05  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
00074      05  FILE-ID                     PIC X(8)    VALUE SPACES.
00075      05  REIN-FILE-ID                PIC X(8)    VALUE 'ERREIN'.
00076      05  CNTL-FILE-ID                PIC X(8)    VALUE 'ELCNTL'.
00077      05  BIN-CURRENT-SAVE            PIC XX      VALUE SPACES.
00078      05  WS-SAVE-REC                 PIC X(131)  VALUE SPACES.
00079
00080  01  MISC-WORK-AREAS.
00081      05  WS-LOW-DATE                 PIC X(8)    VALUE '00/00/00'.
00082      05  WS-HIGH-DATE                PIC X(8)    VALUE '99/99/99'.
00083      05  WS-HOLD-PIC98               PIC 9(8)    VALUE ZEROS.
00084      05  WS-HOLD-PIC98-2             PIC 9(8)    VALUE ZEROS.
00085      05  WS-PHONE-IN                 PIC 9(10).
00086      05  WS-PHONE-IN-R  REDEFINES WS-PHONE-IN.
00087          10  WSPI-AREA               PIC X(3).
00088          10  WSPI-PFX                PIC X(3).
00089          10  WSPI-SFX                PIC X(4).
00090      05  WS-PHONE-OUT.
00091          10  WSPO-AREA               PIC X(3).
00092          10  FILLER                  PIC X       VALUE '-'.
00093          10  WSPO-PFX                PIC X(3).
00094          10  FILLER                  PIC X       VALUE '-'.
00095          10  WSPO-SFX                PIC X(4).
00096      05  WS-SAVE-KEY                 PIC X(8).
00097      05  WS-GETMAIN-SW               PIC X       VALUE ' '.
00098
00099      05  DEEDIT-FIELD                PIC X(15).
00100      05  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).
00101      05  DEEDIT-FIELD-V1  REDEFINES DEEDIT-FIELD PIC S9(13)V99.
00102
00103      05  ERREIN-LENGTH               PIC S9(4)   VALUE +4023 COMP.
00104      05  ELCNTL-LENGTH               PIC S9(4)   VALUE +773  COMP.
00105      05  FILE-LENGTH                 PIC S9(4)   VALUE +0    COMP.
00106      05  DATE-TEST-AREA              PIC 9(8).
00107      05  DATE-TEST-AREA-R  REDEFINES DATE-TEST-AREA.
00108          10  DATE-TEST-CC            PIC 99.
00109          10  DATE-TEST-YY            PIC 99.
00110          10  DATE-TEST-MM            PIC 99.
00111          10  DATE-TEST-DD            PIC 99.
00112      05  DIVIDE-RESULT               PIC 99.
00113      05  DIVIDE-REMAINDER            PIC 9.
00114
00115      05  WS-PREV-AGE                 PIC 99       VALUE ZERO.
00116      05  WS-PREV-TRM                 PIC 999      VALUE ZERO.
00117      05  WS-PREV-AMT                 PIC S9(9)V99 VALUE ZERO.
00118
00119      05  WS-CHANGE-SW                PIC XX      VALUE SPACES.
00120          88  CHANGES-NOT-MADE                    VALUE SPACES.
00121
00122      05  WS-PAGE-LEVEL-SW            PIC X       VALUE SPACE.
00123          88  PAGE-LEVEL-FORWARD                  VALUE 'F'.
00124          88  PAGE-LEVEL-BACKWARD                 VALUE 'B'.
00125
00126      05  WS-CHECK-LEVEL              PIC S9(4)  VALUE +0  COMP.
00127          88  VALID-LEVEL                        VALUE +1 THRU +30.
00128
00129      05  WS-CHECK-PERCENT            PIC X       VALUE SPACE.
00130          88  EXCESS-LEVEL                        VALUE 'X'.
00131          88  NO-LEVEL                            VALUE ' '.
00132          88  VALID-PERCENT                       VALUE 'X' ' '.
00133
00134      05  WS-CHECK-QTED               PIC X       VALUE SPACE.
00135          88  VALID-QTED-CALC                     VALUE '1' '2'
00136                                                        '3' '4'
00137                                                        ' '.
00138
00139      05  WS-CHECK-BEN-CODE           PIC X      VALUE SPACE.
00140          88  VALID-BEN-CODE                     VALUE 'A' THRU 'N'
00141                                                   'P' 'R' 'S' ' '
00142                                                       '2' '3' '4'
00143                                                       '7' '8'.
00144
00145      05  WS-CHECK-INTR               PIC X       VALUE SPACE.
00146          88  VALID-INTR-CODE                     VALUE 'X' 'Y' 'Z'
00147                                                        'E' 'W'
00148                                                        'N' ' '.
00149
00150      05  WS-CHECK-REM                PIC X       VALUE SPACE.
00151          88  VALID-REM-SW                        VALUE 'X' 'Y' 'N'
00152                                                        'R' 'L' 'Z'
00153                                                        ' '.
00154
00155      05  WS-STATE-FOUND-SW           PIC X       VALUE SPACE.
00156          88  STATE-FOUND                         VALUE 'Y'.
00157
00158      05  WS-CARRIER-FOUND-SW         PIC X       VALUE SPACE.
00159          88  CARRIER-FOUND                       VALUE 'Y'.
00160
00161      05  WS-ACCESS.
00162          10  WS-STATE                PIC XX      VALUE SPACES.
00163          10  FILLER                  PIC XX      VALUE SPACES.
00164      05  CARRIER-ACCESS      REDEFINES
00165          WS-ACCESS.
00166          10  FILLER                  PIC X(3).
00167          10  WS-CARRIER              PIC X.
00168
00169      05  ELCNTL-KEY.
00170          10  CNTL-COMP-ID            PIC X(3)    VALUE SPACES.
00171          10  CNTL-REC-TYPE           PIC X       VALUE SPACES.
00172          10  CNTL-ACCESS             PIC X(4)    VALUE SPACES.
00173          10  CNTL-SEQ-NO             PIC S9(4)   VALUE +0  COMP.
00174
00175      05  MISC-SAVE-AREAS.
00176          10  WS-LO-HI-DATES.
00177              15  WS-LO-DATE          PIC 9(8).
00178              15  WS-HI-DATE          PIC 9(8).
00179          10  WS-LIFE-AGE-TRMS.
00180              15  WS-LFAGE-LO         PIC 99.
00181              15  WS-LFAGE-HI         PIC 99.
00182              15  WS-LFTRM-LO         PIC S999.
00183              15  WS-LFTRM-HI         PIC S999.
00184          10  WS-AH-AGE-TRMS.
00185              15  WS-AHAGE-LO         PIC 99.
00186              15  WS-AHAGE-HI         PIC 99.
00187              15  WS-AHTRM-LO         PIC S999.
00188              15  WS-AHTRM-HI         PIC S999.
00189          10  WS-LIFE-BEN-AMTS.
00190              15  WS-LF-LIM-LO        PIC S9(9)V99.
00191              15  WS-LF-LIM-HI        PIC S9(9)V99.
00192              15  WS-LF-LO            PIC S9(9)V99.
00193              15  WS-LF-HI            PIC S9(9)V99.
00194          10  WS-AH-BEN-AMTS.
00195              15  WS-AHBEN-LIM-LO     PIC S9(7)V99.
00196              15  WS-AHBEN-LIM-HI     PIC S9(7)V99.
00197              15  WS-AHMOA-LIM-LO     PIC S9(7)V99.
00198              15  WS-AHMOA-LIM-HI     PIC S9(7)V99.
00199              15  WS-AHBEN-LO         PIC S9(7)V99.
00200              15  WS-AHBEN-HI         PIC S9(7)V99.
00201              15  WS-AHMOA-LO         PIC S9(7)V99.
00202              15  WS-AHMOA-HI         PIC S9(7)V99.
00203      EJECT
00204      05  ERROR-MESSAGES.
00205          10  ER-0000                 PIC X(4)    VALUE '0000'.
00206          10  ER-0004                 PIC X(4)    VALUE '0004'.
00207          10  ER-0008                 PIC X(4)    VALUE '0008'.
00208          10  ER-0023                 PIC X(4)    VALUE '0023'.
00209          10  ER-0029                 PIC X(4)    VALUE '0029'.
00210          10  ER-0050                 PIC X(4)    VALUE '0050'.
00211          10  ER-0068                 PIC X(4)    VALUE '0068'.
00212          10  ER-0070                 PIC X(4)    VALUE '0070'.
00213          10  ER-0142                 PIC X(4)    VALUE '0142'.
00214          10  ER-0585                 PIC X(4)    VALUE '0585'.
00215          10  ER-0589                 PIC X(4)    VALUE '0589'.
00216          10  ER-0592                 PIC X(4)    VALUE '0592'.
00217          10  ER-0593                 PIC X(4)    VALUE '0593'.
00218          10  ER-2055                 PIC X(4)    VALUE '2055'.
00219          10  ER-2056                 PIC X(4)    VALUE '2056'.
00220          10  ER-2067                 PIC X(4)    VALUE '2067'.
00221          10  ER-2112                 PIC X(4)    VALUE '2112'.
00222          10  ER-2139                 PIC X(4)    VALUE '2139'.
00223          10  ER-2140                 PIC X(4)    VALUE '2140'.
00224          10  ER-2141                 PIC X(4)    VALUE '2141'.
00225          10  ER-2208                 PIC X(4)    VALUE '2208'.
00226          10  ER-2310                 PIC X(4)    VALUE '2310'.
00227          10  ER-2311                 PIC X(4)    VALUE '2311'.
00228          10  ER-2312                 PIC X(4)    VALUE '2312'.
00229          10  ER-2313                 PIC X(4)    VALUE '2313'.
00230          10  ER-2316                 PIC X(4)    VALUE '2316'.
00231          10  ER-2317                 PIC X(4)    VALUE '2317'.
00232          10  ER-2318                 PIC X(4)    VALUE '2318'.
00233          10  ER-2319                 PIC X(4)    VALUE '2319'.
00234          10  ER-2320                 PIC X(4)    VALUE '2320'.
00235          10  ER-2321                 PIC X(4)    VALUE '2321'.
00236          10  ER-2322                 PIC X(4)    VALUE '2322'.
00237          10  ER-2323                 PIC X(4)    VALUE '2323'.
00238          10  ER-2324                 PIC X(4)    VALUE '2324'.
00239          10  ER-2325                 PIC X(4)    VALUE '2325'.
00240          10  ER-2326                 PIC X(4)    VALUE '2326'.
00241          10  ER-2327                 PIC X(4)    VALUE '2327'.
00242          10  ER-2328                 PIC X(4)    VALUE '2328'.
00243          10  ER-2329                 PIC X(4)    VALUE '2329'.
00244          10  ER-2330                 PIC X(4)    VALUE '2330'.
00245          10  ER-2331                 PIC X(4)    VALUE '2331'.
00246          10  ER-2333                 PIC X(4)    VALUE '2333'.
00247          10  ER-2334                 PIC X(4)    VALUE '2334'.
00248          10  ER-2335                 PIC X(4)    VALUE '2335'.
00249          10  ER-2338                 PIC X(4)    VALUE '2338'.
00250          10  ER-2339                 PIC X(4)    VALUE '2339'.
00251          10  ER-2340                 PIC X(4)    VALUE '2340'.
00252          10  ER-2341                 PIC X(4)    VALUE '2341'.
00253          10  ER-2342                 PIC X(4)    VALUE '2342'.
00254          10  ER-2343                 PIC X(4)    VALUE '2343'.
00255          10  ER-2344                 PIC X(4)    VALUE '2344'.
00256          10  ER-2345                 PIC X(4)    VALUE '2345'.
00257          10  ER-2346                 PIC X(4)    VALUE '2346'.
00258          10  ER-2347                 PIC X(4)    VALUE '2347'.
00259          10  ER-2348                 PIC X(4)    VALUE '2348'.
00260          10  ER-2356                 PIC X(4)    VALUE '2356'.
00261          10  ER-2370                 PIC X(4)    VALUE '2370'.
00262          10  ER-4008                 PIC X(4)    VALUE '4008'.
00263          10  ER-7805                 PIC X(4)    VALUE '7805'.
00264
00265      EJECT
00266 *                                    COPY ELCDATE.
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
00267      EJECT
00268 *                                    COPY ELCLOGOF.
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
00269      EJECT
00270 *                                    COPY ELCATTR.
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
00271      EJECT
00272 *                                    COPY ELCEMIB.
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
00273      EJECT
00274 *                                    COPY ELCINTF.
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
00275      12  PI-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.
00276          16  PI-CHECK-MAINT-TYPE     PIC X.
00277              88  VALID-MAINT-TYPE                VALUE 'S' 'A' 'C'
00278                                                        'D' 'K'.
00279              88  ADD-FUNCTION                    VALUE 'A'.
00280              88  SHOW-FUNCTION                   VALUE 'S'.
00281              88  DELETE-FUNCTION                 VALUE 'D'.
00282              88  CHANGE-FUNCTION                 VALUE 'C'.
00283              88  COPY-FUNCTION                   VALUE 'K'.
00284
00285          16  PI-PREV-MAINTYP         PIC X.
00286
00287          16  PI-ERREIN-KEY.
00288              20  PI-ERR-COMPANY-CD   PIC X.
00289              20  PI-ERR-CODE         PIC X.
00290              20  PI-ERR-TABLE        PIC X(3).
00291              20  PI-ERR-TABLE-SUB    PIC X(3).
00292
00293          16  PI-START-LEVEL          PIC 9(2).
00294
00295          16  PI-SAVE-ERREIN-KEY      PIC X(8).
00296
00297          16  PI-FIRST-TIME-SW        PIC X.
00298              88  FIRST-TIME                      VALUE 'Y'.
00299          16  PI-ENTRY-SW             PIC X.
00300              88  NOT-FIRST-ENTRY                 VALUE 'Y'.
00301          16  PI-BROWSE-SW            PIC X.
00302              88  BROWSE-STARTED                  VALUE 'Y'.
00303          16  PI-ERREIN-EOF-SW        PIC X.
00304              88  ERREIN-EOF                      VALUE 'Y'.
00305          16  PI-EXCESS-SW            PIC X.
00306              88  EXCESS-LEVEL-EXISTS             VALUE 'X'.
00307          16  PI-COMPANY-ADD-SW       PIC X.
00308              88  COMPANY-RECORD-ADDED            VALUE 'Y'.
00309
00310          16  PI-SUB                  PIC S99.
00311          16  PI-LAST-LEVEL           PIC S99.
00312
00313          16  PI-SAVE-TABLE           PIC X(3).
00314          16  PI-SAVE-COMPANY         PIC X(3).
00315          16  PI-SAVE-COMP-SUB        PIC X(3).
00316          16  PI-MAPNAME              PIC X(8).
00317          16  FILLER                  PIC X(593).
00318
00319      EJECT
00320 *                                    COPY ELCJPFX.
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
00321                                      PIC X(4000).
00322
00323      EJECT
00324 *                                    COPY ELCAID.
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
00325  01  FILLER    REDEFINES DFHAID.
00326      05  FILLER                      PIC X(8).
00327      05  PF-VALUES                   PIC X       OCCURS 2.
00328
00329      EJECT
00330 *                                    COPY EL651S.
       01  EL651AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  RUNDATEL PIC S9(0004) COMP.
           05  RUNDATEF PIC  X(0001).
           05  FILLER REDEFINES RUNDATEF.
               10  RUNDATEA PIC  X(0001).
           05  RUNDATEI PIC  X(0008).
      *    -------------------------------
           05  RUNTIMEL PIC S9(0004) COMP.
           05  RUNTIMEF PIC  X(0001).
           05  FILLER REDEFINES RUNTIMEF.
               10  RUNTIMEA PIC  X(0001).
           05  RUNTIMEI PIC  X(0005).
      *    -------------------------------
           05  MAINTYPL PIC S9(0004) COMP.
           05  MAINTYPF PIC  X(0001).
           05  FILLER REDEFINES MAINTYPF.
               10  MAINTYPA PIC  X(0001).
           05  MAINTYPI PIC  X(0001).
      *    -------------------------------
           05  LMBYL PIC S9(0004) COMP.
           05  LMBYF PIC  X(0001).
           05  FILLER REDEFINES LMBYF.
               10  LMBYA PIC  X(0001).
           05  LMBYI PIC  X(0004).
      *    -------------------------------
           05  LMDATEL PIC S9(0004) COMP.
           05  LMDATEF PIC  X(0001).
           05  FILLER REDEFINES LMDATEF.
               10  LMDATEA PIC  X(0001).
           05  LMDATEI PIC  X(0008).
      *    -------------------------------
           05  LMTIMEL PIC S9(0004) COMP.
           05  LMTIMEF PIC  X(0001).
           05  FILLER REDEFINES LMTIMEF.
               10  LMTIMEA PIC  X(0001).
           05  LMTIMEI PIC  X(0005).
      *    -------------------------------
           05  TABLEL PIC S9(0004) COMP.
           05  TABLEF PIC  X(0001).
           05  FILLER REDEFINES TABLEF.
               10  TABLEA PIC  X(0001).
           05  TABLEI PIC  X(0003).
      *    -------------------------------
           05  COMPANYL PIC S9(0004) COMP.
           05  COMPANYF PIC  X(0001).
           05  FILLER REDEFINES COMPANYF.
               10  COMPANYA PIC  X(0001).
           05  COMPANYI PIC  X(0003).
      *    -------------------------------
           05  COMPSUBL PIC S9(0004) COMP.
           05  COMPSUBF PIC  X(0001).
           05  FILLER REDEFINES COMPSUBF.
               10  COMPSUBA PIC  X(0001).
           05  COMPSUBI PIC  X(0003).
      *    -------------------------------
           05  LEVELL PIC S9(0004) COMP.
           05  LEVELF PIC  X(0001).
           05  FILLER REDEFINES LEVELF.
               10  LEVELA PIC  X(0001).
           05  LEVELI PIC  X(0002).
      *    -------------------------------
           05  OFLEVELL PIC S9(0004) COMP.
           05  OFLEVELF PIC  X(0001).
           05  FILLER REDEFINES OFLEVELF.
               10  OFLEVELA PIC  X(0001).
           05  OFLEVELI PIC  X(0002).
      *    -------------------------------
           05  TOTABLEL PIC S9(0004) COMP.
           05  TOTABLEF PIC  X(0001).
           05  FILLER REDEFINES TOTABLEF.
               10  TOTABLEA PIC  X(0001).
           05  TOTABLEI PIC  X(0003).
      *    -------------------------------
           05  CARDESCL PIC S9(0004) COMP.
           05  CARDESCF PIC  X(0001).
           05  FILLER REDEFINES CARDESCF.
               10  CARDESCA PIC  X(0001).
           05  CARDESCI PIC  X(0011).
      *    -------------------------------
           05  CARRIERL PIC S9(0004) COMP.
           05  CARRIERF PIC  X(0001).
           05  FILLER REDEFINES CARRIERF.
               10  CARRIERA PIC  X(0001).
           05  CARRIERI PIC  X(0001).
      *    -------------------------------
           05  PERCNTL PIC S9(0004) COMP.
           05  PERCNTF PIC  X(0001).
           05  FILLER REDEFINES PERCNTF.
               10  PERCNTA PIC  X(0001).
           05  PERCNTI PIC  X(0001).
      *    -------------------------------
           05  LIFEHDL PIC S9(0004) COMP.
           05  LIFEHDF PIC  X(0001).
           05  FILLER REDEFINES LIFEHDF.
               10  LIFEHDA PIC  X(0001).
           05  LIFEHDI PIC  X(0004).
      *    -------------------------------
           05  FDATEL PIC S9(0004) COMP.
           05  FDATEF PIC  X(0001).
           05  FILLER REDEFINES FDATEF.
               10  FDATEA PIC  X(0001).
           05  FDATEI PIC  X(0008).
      *    -------------------------------
           05  TDATEL PIC S9(0004) COMP.
           05  TDATEF PIC  X(0001).
           05  FILLER REDEFINES TDATEF.
               10  TDATEA PIC  X(0001).
           05  TDATEI PIC  X(0008).
      *    -------------------------------
           05  LFLOAGEL PIC S9(0004) COMP.
           05  LFLOAGEF PIC  X(0001).
           05  FILLER REDEFINES LFLOAGEF.
               10  LFLOAGEA PIC  X(0001).
           05  LFLOAGEI PIC  X(0002).
      *    -------------------------------
           05  LFHIAGEL PIC S9(0004) COMP.
           05  LFHIAGEF PIC  X(0001).
           05  FILLER REDEFINES LFHIAGEF.
               10  LFHIAGEA PIC  X(0001).
           05  LFHIAGEI PIC  X(0002).
      *    -------------------------------
           05  LFLOTRML PIC S9(0004) COMP.
           05  LFLOTRMF PIC  X(0001).
           05  FILLER REDEFINES LFLOTRMF.
               10  LFLOTRMA PIC  X(0001).
           05  LFLOTRMI PIC  X(0003).
      *    -------------------------------
           05  LFHITRML PIC S9(0004) COMP.
           05  LFHITRMF PIC  X(0001).
           05  FILLER REDEFINES LFHITRMF.
               10  LFHITRMA PIC  X(0001).
           05  LFHITRMI PIC  X(0003).
      *    -------------------------------
           05  LFLOBENL PIC S9(0004) COMP.
           05  LFLOBENF PIC  X(0001).
           05  FILLER REDEFINES LFLOBENF.
               10  LFLOBENA PIC  X(0001).
           05  LFLOBENI PIC  9(14).
      *    -------------------------------
           05  LFHIBENL PIC S9(0004) COMP.
           05  LFHIBENF PIC  X(0001).
           05  FILLER REDEFINES LFHIBENF.
               10  LFHIBENA PIC  X(0001).
           05  LFHIBENI PIC  9(14).
      *    -------------------------------
           05  AHHD1L PIC S9(0004) COMP.
           05  AHHD1F PIC  X(0001).
           05  FILLER REDEFINES AHHD1F.
               10  AHHD1A PIC  X(0001).
           05  AHHD1I PIC  X(0003).
      *    -------------------------------
           05  AHLOAGEL PIC S9(0004) COMP.
           05  AHLOAGEF PIC  X(0001).
           05  FILLER REDEFINES AHLOAGEF.
               10  AHLOAGEA PIC  X(0001).
           05  AHLOAGEI PIC  X(0002).
      *    -------------------------------
           05  AHHIAGEL PIC S9(0004) COMP.
           05  AHHIAGEF PIC  X(0001).
           05  FILLER REDEFINES AHHIAGEF.
               10  AHHIAGEA PIC  X(0001).
           05  AHHIAGEI PIC  X(0002).
      *    -------------------------------
           05  AHLOTRML PIC S9(0004) COMP.
           05  AHLOTRMF PIC  X(0001).
           05  FILLER REDEFINES AHLOTRMF.
               10  AHLOTRMA PIC  X(0001).
           05  AHLOTRMI PIC  X(0003).
      *    -------------------------------
           05  AHHITRML PIC S9(0004) COMP.
           05  AHHITRMF PIC  X(0001).
           05  FILLER REDEFINES AHHITRMF.
               10  AHHITRMA PIC  X(0001).
           05  AHHITRMI PIC  X(0003).
      *    -------------------------------
           05  AHLOBENL PIC S9(0004) COMP.
           05  AHLOBENF PIC  X(0001).
           05  FILLER REDEFINES AHLOBENF.
               10  AHLOBENA PIC  X(0001).
           05  AHLOBENI PIC  9(12).
      *    -------------------------------
           05  AHHIBENL PIC S9(0004) COMP.
           05  AHHIBENF PIC  X(0001).
           05  FILLER REDEFINES AHHIBENF.
               10  AHHIBENA PIC  X(0001).
           05  AHHIBENI PIC  9(12).
      *    -------------------------------
           05  AHHD2L PIC S9(0004) COMP.
           05  AHHD2F PIC  X(0001).
           05  FILLER REDEFINES AHHD2F.
               10  AHHD2A PIC  X(0001).
           05  AHHD2I PIC  X(0003).
      *    -------------------------------
           05  AMLOBENL PIC S9(0004) COMP.
           05  AMLOBENF PIC  X(0001).
           05  FILLER REDEFINES AMLOBENF.
               10  AMLOBENA PIC  X(0001).
           05  AMLOBENI PIC  9(12).
      *    -------------------------------
           05  AMHIBENL PIC S9(0004) COMP.
           05  AMHIBENF PIC  X(0001).
           05  FILLER REDEFINES AMHIBENF.
               10  AMHIBENA PIC  X(0001).
           05  AMHIBENI PIC  9(12).
      *    -------------------------------
           05  LFQTEDL PIC S9(0004) COMP.
           05  LFQTEDF PIC  X(0001).
           05  FILLER REDEFINES LFQTEDF.
               10  LFQTEDA PIC  X(0001).
           05  LFQTEDI PIC  X(0001).
      *    -------------------------------
           05  LIFEBENL PIC S9(0004) COMP.
           05  LIFEBENF PIC  X(0001).
           05  FILLER REDEFINES LIFEBENF.
               10  LIFEBENA PIC  X(0001).
           05  LIFEBENI PIC  X(0001).
      *    -------------------------------
           05  LFINTRL PIC S9(0004) COMP.
           05  LFINTRF PIC  X(0001).
           05  FILLER REDEFINES LFINTRF.
               10  LFINTRA PIC  X(0001).
           05  LFINTRI PIC  X(0001).
      *    -------------------------------
           05  LFREML PIC S9(0004) COMP.
           05  LFREMF PIC  X(0001).
           05  FILLER REDEFINES LFREMF.
               10  LFREMA PIC  X(0001).
           05  LFREMI PIC  X(0001).
      *    -------------------------------
           05  LFSTATEL PIC S9(0004) COMP.
           05  LFSTATEF PIC  X(0001).
           05  FILLER REDEFINES LFSTATEF.
               10  LFSTATEA PIC  X(0001).
           05  LFSTATEI PIC  X(0002).
      *    -------------------------------
           05  LFBENL PIC S9(0004) COMP.
           05  LFBENF PIC  X(0001).
           05  FILLER REDEFINES LFBENF.
               10  LFBENA PIC  X(0001).
           05  LFBENI PIC  9V9999.
      *    -------------------------------
           05  LFLOAMTL PIC S9(0004) COMP.
           05  LFLOAMTF PIC  X(0001).
           05  FILLER REDEFINES LFLOAMTF.
               10  LFLOAMTA PIC  X(0001).
           05  LFLOAMTI PIC  9(14).
      *    -------------------------------
           05  LFHIAMTL PIC S9(0004) COMP.
           05  LFHIAMTF PIC  X(0001).
           05  FILLER REDEFINES LFHIAMTF.
               10  LFHIAMTA PIC  X(0001).
           05  LFHIAMTI PIC  9(14).
      *    -------------------------------
           05  AHQTEDL PIC S9(0004) COMP.
           05  AHQTEDF PIC  X(0001).
           05  FILLER REDEFINES AHQTEDF.
               10  AHQTEDA PIC  X(0001).
           05  AHQTEDI PIC  X(0001).
      *    -------------------------------
           05  AHTBENL PIC S9(0004) COMP.
           05  AHTBENF PIC  X(0001).
           05  FILLER REDEFINES AHTBENF.
               10  AHTBENA PIC  X(0001).
           05  AHTBENI PIC  X(0001).
      *    -------------------------------
           05  AHSTATEL PIC S9(0004) COMP.
           05  AHSTATEF PIC  X(0001).
           05  FILLER REDEFINES AHSTATEF.
               10  AHSTATEA PIC  X(0001).
           05  AHSTATEI PIC  X(0002).
      *    -------------------------------
           05  AHBENL PIC S9(0004) COMP.
           05  AHBENF PIC  X(0001).
           05  FILLER REDEFINES AHBENF.
               10  AHBENA PIC  X(0001).
           05  AHBENI PIC  9V9999.
      *    -------------------------------
           05  AHLOAMTL PIC S9(0004) COMP.
           05  AHLOAMTF PIC  X(0001).
           05  FILLER REDEFINES AHLOAMTF.
               10  AHLOAMTA PIC  X(0001).
           05  AHLOAMTI PIC  9(12).
      *    -------------------------------
           05  AHHIAMTL PIC S9(0004) COMP.
           05  AHHIAMTF PIC  X(0001).
           05  FILLER REDEFINES AHHIAMTF.
               10  AHHIAMTA PIC  X(0001).
           05  AHHIAMTI PIC  9(12).
      *    -------------------------------
           05  AMLOAMTL PIC S9(0004) COMP.
           05  AMLOAMTF PIC  X(0001).
           05  FILLER REDEFINES AMLOAMTF.
               10  AMLOAMTA PIC  X(0001).
           05  AMLOAMTI PIC  9(12).
      *    -------------------------------
           05  AMHIAMTL PIC S9(0004) COMP.
           05  AMHIAMTF PIC  X(0001).
           05  FILLER REDEFINES AMHIAMTF.
               10  AMHIAMTA PIC  X(0001).
           05  AMHIAMTI PIC  9(12).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0072).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  99.
       01  EL651AO REDEFINES EL651AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LMBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LMDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LMTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TABLEO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPANYO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPSUBO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LEVELO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OFLEVELO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOTABLEO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARDESCO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PERCNTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LIFEHDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFLOAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFHIAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFLOTRMO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFHITRMO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFLOBENO PIC  ZZZ,ZZZ,ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFHIBENO PIC  ZZZ,ZZZ,ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHHD1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHLOAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHHIAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHLOTRMO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHHITRMO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHLOBENO PIC  Z,ZZZ,ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHHIBENO PIC  Z,ZZZ,ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHHD2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMLOBENO PIC  Z,ZZZ,ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMHIBENO PIC  Z,ZZZ,ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFQTEDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LIFEBENO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFINTRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFREMO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFSTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFBENO PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFLOAMTO PIC  ZZZ,ZZZ,ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFHIAMTO PIC  ZZZ,ZZZ,ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHQTEDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHTBENO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHSTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHBENO PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHLOAMTO PIC  Z,ZZZ,ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHHIAMTO PIC  Z,ZZZ,ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMLOAMTO PIC  Z,ZZZ,ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMHIAMTO PIC  Z,ZZZ,ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  99.
      *    -------------------------------
00331
00332      EJECT
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
00338 *01 PARMLIST .
00339 *    02  FILLER                      PIC S9(8)   COMP.
00340 *    02  ERREIN-POINTER              PIC S9(8)   COMP.
00341 *    02  ELCNTL-POINTER              PIC S9(8)   COMP.
00342      EJECT
00343 *                                    COPY ERCREIN.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCREIN                             *
00003 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00004 *                            VMOD=2.010                          *
00005 *                                                                *
00006 *   ONLINE CREDIT SYSTEM                                         *
00007 *                                                                *
00008 *   FILE DESCRIPTION = REINSURANCE MASTER FILE                   *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 4000  RECFORM = FIXED                          *
00012 *                                                                *
00013 *   BASE CLUSTER NAME = ERREIN                   RKP=2,LEN=8     *
00014 *       ALTERNATE PATH = NONE                                    *
00015 *                                                                *
00016 *   LOG = NO                                                     *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 *                                                                *
00019 ******************************************************************
103101*                   C H A N G E   L O G
103101*
103101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
103101*-----------------------------------------------------------------
103101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
103101* EFFECTIVE    NUMBER
103101*-----------------------------------------------------------------
103101* 103101    2001100100006  SMVA  ADD STATE EXHIBIT REPORT OPTION F
032707* 032707    2007032100006  PEMA  ADD EXCISE TAX CAPABILITY
103101******************************************************************
00021  01  REINSURANCE-RECORD.
00022      12  RE-RECORD-ID                      PIC XX.
00023          88  VALID-RE-ID                      VALUE 'RE'.
00024
00025      12  RE-CONTROL-PRIMARY.
00026          16  RE-COMPANY-CD                 PIC X.
00027          16  RE-KEY.
00028              20  RE-CODE                   PIC X.
00029                  88  RE-TABLE-RECORD          VALUE 'A'.
00030                  88  RE-COMPANY-RECORD        VALUE 'B'.
00031              20  RE-TABLE                  PIC XXX.
00032              20  FILLER                    PIC XXX.
00033          16  RE-COMPANY-KEY REDEFINES RE-KEY.
00034              20  FILLER                    PIC X.
00035              20  RE-COMPANY.
00036                  24  RE-COMP-PRIME         PIC XXX.
00037                  24  RE-COMP-SUB           PIC XXX.
00038
00039      12  RE-MAINT-INFORMATION.
00040          16  RE-LAST-MAINT-DT              PIC XX.
00041          16  RE-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00042          16  RE-LAST-MAINT-USER            PIC X(4).
00043          16  FILLER                        PIC X(10).
00044
00045      12  RE-TABLE-DATA.
00046          16  RE-100-COMP                   PIC 99.
00047
00048          16  RE-COMP-INFO    OCCURS 30 TIMES.
00049              20  RE-REI-COMP-NO.
00050                  24  RE-REI-COMP           PIC XXX.
00051                  24  RE-REI-COMP-SUB       PIC XXX.
00052              20  RE-LF-QC                  PIC X.
00053              20  RE-AH-QC                  PIC X.
00054              20  RE-LO-DATE                PIC 9(11)     COMP-3.
00055              20  RE-HI-DATE                PIC 9(11)     COMP-3.
00056              20  RE-LFAGE-LO               PIC 99.
00057              20  RE-LFAGE-HI               PIC 99.
00058              20  RE-AHAGE-LO               PIC 99.
00059              20  RE-AHAGE-HI               PIC 99.
00060              20  RE-LFTRM-LO               PIC S999       COMP-3.
00061              20  RE-LFTRM-HI               PIC S999       COMP-3.
00062              20  RE-AHTRM-LO               PIC S999       COMP-3.
00063              20  RE-AHTRM-HI               PIC S999       COMP-3.
00064              20  RE-LF-PCT                 PIC S9V9999    COMP-3.
00065              20  RE-AH-PCT                 PIC S9V9999    COMP-3.
00066              20  RE-LF-LIM-LO              PIC S9(9)V99   COMP-3.
00067              20  RE-LF-LIM-HI              PIC S9(9)V99   COMP-3.
00068              20  RE-LF-LO                  PIC S9(9)V99   COMP-3.
00069              20  RE-LF-HI                  PIC S9(9)V99   COMP-3.
00070              20  RE-AHBEN-LIM-LO           PIC S9(7)V99   COMP-3.
00071              20  RE-AHBEN-LIM-HI           PIC S9(7)V99   COMP-3.
00072              20  RE-AHBEN-LO               PIC S9(7)V99   COMP-3.
00073              20  RE-AHBEN-HI               PIC S9(7)V99   COMP-3.
00074              20  RE-AHMOA-LIM-LO           PIC S9(7)V99   COMP-3.
00075              20  RE-AHMOA-LIM-HI           PIC S9(7)V99   COMP-3.
00076              20  RE-AHMOA-LO               PIC S9(7)V99   COMP-3.
00077              20  RE-AHMOA-HI               PIC S9(7)V99   COMP-3.
00078              20  RE-LF-BEN-CODE            PIC X.
00079              20  RE-AH-BEN-CODE            PIC X.
00080              20  RE-INTERACTIVE            PIC X.
00081              20  RE-REMAINING              PIC X.
CIDMOD             20  RE-LF-RUNOFF-SW           PIC X.
CIDMOD             20  RE-AH-RUNOFF-SW           PIC X.
CIDMOD             20  FILLER                    PIC X(19).
00083
00084          16  RE-COMP-INFO-END              PIC X(6).
00085          16  RE-NSP-ST-CD-LF               PIC XX.
00086          16  RE-NSP-ST-CD-AH               PIC XX.
00087          16  RE-TABLE-CARRIER-SECURITY     PIC X.
00088              88  NO-TABLE-CARRIER-SECURITY    VALUE SPACE.
00089
00090          16  FILLER                        PIC X(27).
00091
00092      12  RE-COMPANY-DATA   REDEFINES   RE-TABLE-DATA.
00093          16  RE-NAME                       PIC X(30).
00094          16  RE-LF-PE                      PIC X.
00095          16  RE-AH-PE                      PIC X.
00096          16  RE-LF-FEE                     PIC S9V9999    COMP-3.
00097          16  RE-AH-FEE                     PIC S9V9999    COMP-3.
00098          16  RE-AH-PR-PCT                  PIC S9V9999    COMP-3.
00099          16  RE-AH-78-PCT                  PIC S9V9999    COMP-3.
00100          16  RE-PRT-ST                     PIC X.
00101          16  RE-PRT-OW                     PIC X.
00102          16  RE-MORT-CODE                  PIC X(4).
00103          16  RE-CLAIM-CODE                 PIC X.
00104          16  RE-ZERO-LF-FEE                PIC X.
00105          16  RE-ZERO-AH-FEE                PIC X.
00106          16  RE-CEDE-NAME                  PIC X(30).
00107          16  RE-LF-COMM                    PIC X.
00108          16  RE-AH-COMM                    PIC X.
00109          16  RE-LF-TAX                     PIC X.
00110          16  RE-AH-TAX                     PIC X.
00111          16  RE-CLM-INCURRED-LIM           PIC 9(11)  COMP-3.
00116          16  RE-LF-IBNR-PCT                PIC SV999      COMP-3.
00117          16  RE-AH-IBNR-PCT                PIC SV999      COMP-3.
00118
00119          16  RE-COMP-CARRIER-SECURITY      PIC X.
00120              88  NO-COMP-CARRIER-SECURITY     VALUE SPACE.
00121
00122          16  RE-LF-CEDING-FEE-BRACKETS.
00123              20  RE-LF-FEE-METHOD          PIC X.
00124                  88  RE-LF-FEE-BRACKETED         VALUE '1' '2'.
00125                  88  RE-LF-FEE-METHOD-1          VALUE '1'.
00126                  88  RE-LF-FEE-METHOD-2          VALUE '2'.
00127                  88  RE-LF-FEE-PERCENT           VALUE ' ' 'P'.
00128              20  RE-LF-FEE-BASIS           PIC X.
00129                  88  RE-LF-GROSS-CEDED             VALUE '1'.
00130                  88  RE-LF-NET-CEDED               VALUE '2'.
00131                  88  RE-LF-GROSS-WRITTEN           VALUE '3'.
00132                  88  RE-LF-NET-WRITTEN             VALUE '4'.
00133                  88  RE-LF-COMBINE-GROSS-CEDED     VALUE '5'.
00134                  88  RE-LF-COMBINE-NET-CEDED       VALUE '6'.
00135                  88  RE-LF-COMBINE-GROSS-WRITTEN   VALUE '7'.
00136                  88  RE-LF-COMBINE-NET-WRITTEN     VALUE '8'.
00137              20  FILLER                    PIC XXX.
00138              20  RE-LF-FEE-RANGES  OCCURS 6 TIMES.
00139                  24  RE-LF-FEE-RANGE-PCT   PIC S9V9999    COMP-3.
00140                  24  RE-LF-FEE-THRU-AMT    PIC S9(7)V99   COMP-3.
00141
00142          16  RE-AH-CEDING-FEE-BRACKETS.
00143              20  RE-AH-FEE-METHOD          PIC X.
00144                  88  RE-AH-FEE-BRACKETED         VALUE '1' '2'.
00145                  88  RE-AH-FEE-METHOD-1          VALUE '1'.
00146                  88  RE-AH-FEE-METHOD-2          VALUE '2'.
00147                  88  RE-AH-FEE-PERCENT           VALUE ' ' 'P'.
00148              20  RE-AH-FEE-BASIS           PIC X.
00149                  88  RE-AH-GROSS-CEDED             VALUE '1'.
00150                  88  RE-AH-NET-CEDED               VALUE '2'.
00151                  88  RE-AH-GROSS-WRITTEN           VALUE '3'.
00152                  88  RE-AH-NET-WRITTEN             VALUE '4'.
00153                  88  RE-AH-COMBINE-GROSS-CEDED     VALUE '5'.
00154                  88  RE-AH-COMBINE-NET-CEDED       VALUE '6'.
00155                  88  RE-AH-COMBINE-GROSS-WRITTEN   VALUE '7'.
00156                  88  RE-AH-COMBINE-NET-WRITTEN     VALUE '8'.
00157              20  FILLER                    PIC XXX.
00158              20  RE-AH-FEE-RANGES  OCCURS 6 TIMES.
00159                  24  RE-AH-FEE-RANGE-PCT   PIC S9V9999    COMP-3.
00160                  24  RE-AH-FEE-THRU-AMT    PIC S9(7)V99   COMP-3.
00161
00162          16  RE-EARNING-START-DT           PIC 9(11)  COMP-3.
00166
00167          16  RE-OLD-CEDING-STMT            PIC X.
00168
00169          16  RE-LF-CLM-PCT                 PIC S9V9999    COMP-3.
00170          16  RE-AH-CLM-PCT                 PIC S9V9999    COMP-3.
00171          16  RE-LF-CLM-MAX                 PIC S9(7)V99   COMP-3.
00172          16  RE-AH-CLM-MAX                 PIC S9(7)V99   COMP-3.
00173          16  RE-LF-PR-PCT                  PIC S9V9999    COMP-3.
00174          16  RE-LF-78-PCT                  PIC S9V9999    COMP-3.
00175          16  RE-REINS-GROUPING-CODE        PIC X(6).
00176          16  RE-MORT-SW                    PIC X.
00177          16  RE-CEDING-TYPE-FLAG           PIC X.
00178              88  RE-NO-CESSION-TYPE                VALUE ' '.
00179              88  RE-CEDED                          VALUE 'C'.
00180              88  RE-ASSUMED                        VALUE 'A'.
00181              88  RE-PHANTOM                        VALUE 'P'.
00182
00183          16  RE-CEDING-STMT-OPT-A          PIC X.
00184              88  REPORT-A-WANTED    VALUE ' ' 'Y'.
00185          16  RE-CEDING-STMT-OPT-B          PIC X.
00186              88  REPORT-B-WANTED    VALUE ' ' 'Y'.
00187          16  RE-CEDING-STMT-OPT-C          PIC X.
00188              88  REPORT-C-WANTED    VALUE ' ' 'Y'.
00189          16  RE-CEDING-STMT-OPT-D          PIC X.
00190              88  REPORT-D-WANTED    VALUE ' ' 'Y'.
00191          16  RE-CEDING-STMT-OPT-E          PIC X.
00192              88  REPORT-E-WANTED    VALUE ' ' 'Y'.
00193
00194          16  RE-PRT-CRSV                   PIC X.
00195
00196          16  RE-GL-CENTER                  PIC X(4).
00197
00198          16  RE-CUSTODIAL-BAL              PIC S9(7)V99   COMP-3.
00199
00200          16  RE-EARNING-STOP-DT            PIC 9(11)  COMP-3.
00204
00205          16  RE-EARN-STOP-CODE             PIC X.
00206              88  STOP-LIFE-EARNING  VALUE 'L' 'B'.
00207              88  STOP-AH-EARNING    VALUE 'A' 'B'.
00208
103101         16  RE-STATE-EXHIBIT-OPT-F        PIC X.
103101             88  RPTF-ECS152-WANTED VALUE ' ' 'Y'.
103101
032707         16  RE-EXCISE-TAX                 PIC S9V9999 COMP-3.
032707         16  FILLER                        PIC X(2281).
00210
00211          16  RE-DESC OCCURS 18 TIMES       PIC X(79).
00212
00213 ******************************************************************
00344      EJECT
00345 *                                    COPY ELCCNTL.
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
00346      EJECT
00347
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA
                                REINSURANCE-RECORD CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL651' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00349      CONTINUE.
00350
00351      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00352      MOVE '5'                    TO  DC-OPTION-CODE.
00353      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00354      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00355      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00356
00357      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00358
00359  1000-START.
00360      IF EIBCALEN = 0
00361          GO TO 8800-UNAUTHORIZED-ACCESS.
00362
00363      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00364          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00365              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00366              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00367              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00368              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00369              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00370              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00371              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00372              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
00373          ELSE
00374              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00375              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00376              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00377              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00378              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00379              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00380              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00381              MOVE SPACES               TO  PI-SAVED-PROGRAM-6.
00382
00383      
      * EXEC CICS HANDLE CONDITION
00384 *        NOTOPEN  (9990-ABEND)
00385 *        NOTFND   (8880-NOT-FOUND)
00386 *        PGMIDERR (9600-PGMID-ERROR)
00387 *        ERROR    (9990-ABEND)
00388 *    END-EXEC.
      *    MOVE '"$JIL.                ! " #00003382' TO DFHEIV0
           MOVE X'22244A494C2E202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033333832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00389
00390      IF EIBTRNID = EL6592-TRANS-ID
00391          MOVE 'S'                TO  MAINTYPI
00392                                      PI-CHECK-MAINT-TYPE
00393          MOVE PI-ERR-TABLE       TO  TABLEI
00394          MOVE PI-START-LEVEL     TO  LEVELI
00395          MOVE +1                 TO  MAINTYPL
00396          MOVE +3                 TO  TABLEL
00397          MOVE +2                 TO  LEVELL
00398          GO TO 4000-EDIT-MAINT.
00399
00400      IF EIBTRNID NOT = TRANS-ID
00401          IF NOT-FIRST-ENTRY
00402              MOVE +1             TO  PI-SUB
00403              MOVE ZEROS          TO  PI-LAST-LEVEL
00404                                      MISC-SAVE-AREAS
00405              GO TO 5000-BUILD-INITIAL-SCREEN
00406          ELSE
00407              MOVE LOW-VALUES     TO  EL651AI
00408                                      PI-PROGRAM-WORK-AREA
00409              MOVE 'Y'            TO  PI-ENTRY-SW
00410                                      PI-FIRST-TIME-SW
00411              MOVE ZEROS          TO  PI-LAST-LEVEL
00412              MOVE +1             TO  PI-SUB
00413              GO TO 8100-SEND-INITIAL-MAP.
00414
00415
00416      IF EIBAID = DFHCLEAR
00417          GO TO 9400-CLEAR.
00418
00419      IF PI-PROCESSOR-ID = 'LGXX'
00420          GO TO 2000-RECEIVE.
00421
00422      
      * EXEC CICS READQ TS
00423 *        QUEUE  (PI-SECURITY-TEMP-STORE-ID)
00424 *        INTO   (SECURITY-CONTROL)
00425 *        LENGTH (SC-COMM-LENGTH)
00426 *        ITEM   (SC-ITEM)
00427 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00003421' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00428
00429      MOVE SC-CREDIT-DISPLAY (07)  TO PI-DISPLAY-CAP.
00430      MOVE SC-CREDIT-UPDATE  (07)  TO PI-MODIFY-CAP.
00431
00432      IF NOT DISPLAY-CAP
00433          MOVE 'READ'          TO SM-READ
00434          PERFORM 9995-SECURITY-VIOLATION
00435          MOVE ER-0070        TO  EMI-ERROR
00436          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00437          GO TO 8100-SEND-INITIAL-MAP.
00438
00439      EJECT
00440  2000-RECEIVE.
00441      MOVE LOW-VALUES             TO  EL651AI.
00442
00443      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00444          MOVE ER-0008            TO  EMI-ERROR
00445          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00446          MOVE -1                 TO  MAINTYPL
00447          GO TO 8200-SEND-DATAONLY.
00448
00449      
      * EXEC CICS RECEIVE
00450 *        MAP    (MAP-NAME)
00451 *        MAPSET (MAPSET-NAME)
00452 *        INTO   (EL651AI)
00453 *    END-EXEC.
           MOVE LENGTH OF
            EL651AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003448' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL651AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00454
00455      IF PFENTERL = 0
00456          GO TO 3000-CHECK-PFENTERS.
00457
00458      IF EIBAID NOT = DFHENTER
00459          MOVE ER-0004            TO  EMI-ERROR
00460          GO TO 3100-INPUT-ERROR.
00461
00462      IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)
00463          MOVE PF-VALUES (PFENTERI)   TO EIBAID
00464      ELSE
00465          MOVE ER-0029                TO EMI-ERROR
00466          GO TO 3100-INPUT-ERROR.
00467
00468      EJECT
00469  3000-CHECK-PFENTERS.
00470      IF EIBAID = DFHPF23
00471          GO TO 8810-PF23.
00472
00473      IF EIBAID = DFHPF24
00474          GO TO 9200-RETURN-MAIN-MENU.
00475
00476      IF EIBAID = DFHPF12
00477          GO TO 9500-PF12.
00478
00479      IF MAINTYPL GREATER ZERO
00480          IF MAINTYPI NOT = SPACE
00481              IF EIBAID = DFHENTER OR DFHPF10
00482                 NEXT SENTENCE
00483                ELSE
00484                 MOVE ER-0050    TO  EMI-ERROR
00485                 GO TO 3100-INPUT-ERROR.
00486
00487      IF EIBAID = DFHPF1
00488          GO TO 7100-PAGE-TABLE-FORWARD.
00489
00490      IF EIBAID = DFHPF2
00491          GO TO 7200-PAGE-TABLE-BACKWARD.
00492
00493      IF EIBAID = DFHPF3
00494          MOVE 'F'                TO  WS-PAGE-LEVEL-SW
00495          GO TO 7300-PAGE-LEVEL.
00496
00497      IF EIBAID = DFHPF4
00498          MOVE 'B'                TO  WS-PAGE-LEVEL-SW
00499          GO TO 7300-PAGE-LEVEL.
00500
00501      IF EIBAID = DFHPF7
00502          MOVE PI-ERR-TABLE       TO  PI-SAVE-TABLE
00503          MOVE PI-SAVE-COMPANY    TO  PI-ERR-TABLE
00504          MOVE PI-SAVE-COMP-SUB   TO  PI-ERR-TABLE-SUB
00505          MOVE 'EL6511B'          TO  PI-MAPNAME
00506          MOVE XCTL-6511          TO  PGM-NAME
00507          GO TO 9300-XCTL.
00508
00509      IF EIBAID = DFHPF8
00510          MOVE XCTL-657           TO  PGM-NAME
00511          GO TO 9300-XCTL.
00512
00513      IF EIBAID = DFHPF9
00514          MOVE PI-ERR-TABLE       TO  PI-SAVE-TABLE
00515          MOVE PI-SAVE-COMPANY    TO  PI-ERR-TABLE
00516          MOVE PI-SAVE-COMP-SUB   TO  PI-ERR-TABLE-SUB
00517          MOVE XCTL-6511          TO  PGM-NAME
00518          MOVE 'EL6511C'          TO  PI-MAPNAME
00519          GO TO 9300-XCTL.
00520
00521      IF EIBAID = DFHENTER OR EIBAID = DFHPF10
00522          GO TO 4000-EDIT-MAINT.
00523
00524      MOVE ER-0029                TO  EMI-ERROR.
00525
00526  3100-INPUT-ERROR.
00527      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00528      MOVE AL-UNBON               TO  PFENTERA.
00529      MOVE -1                     TO  PFENTERL.
00530      GO TO 8200-SEND-DATAONLY.
00531
00532      EJECT
00533  4000-EDIT-MAINT.
00534      IF MAINTYPL GREATER ZERO
00535          MOVE MAINTYPI           TO  PI-CHECK-MAINT-TYPE
00536          IF VALID-MAINT-TYPE
00537              MOVE AL-UANON       TO  MAINTYPA
00538          ELSE
00539              MOVE -1             TO  MAINTYPL
00540              MOVE AL-UABON       TO  MAINTYPA
00541              MOVE ER-0023        TO  EMI-ERROR
00542              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00543      ELSE
00544          MOVE -1                 TO  MAINTYPL
00545          MOVE AL-UABON           TO  MAINTYPA
00546          MOVE ER-0023            TO  EMI-ERROR
00547          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00548
00549      IF NOT MODIFY-CAP
00550          IF NOT SHOW-FUNCTION
00551              MOVE 'UPDATE'       TO SM-READ
00552              PERFORM 9995-SECURITY-VIOLATION
00553              MOVE ER-0070        TO  EMI-ERROR
00554              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00555
00556      IF TABLEL GREATER ZERO
00557          MOVE ZERO TO TALLY
00558          INSPECT TABLEI TALLYING TALLY FOR ALL SPACES
00559          IF TALLY GREATER ZERO
00560              MOVE -1             TO  TABLEL
00561              MOVE AL-UABON       TO  TABLEA
00562              MOVE ER-2341        TO  EMI-ERROR
00563              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00564          ELSE
00565              MOVE AL-UANON       TO  TABLEA
00566              MOVE TABLEI         TO  PI-ERR-TABLE
00567              MOVE LOW-VALUES     TO  PI-ERR-TABLE-SUB
00568      ELSE
00569          MOVE -1                 TO  TABLEL
00570          MOVE AL-UABON           TO  TABLEA
00571          MOVE ER-2140            TO  EMI-ERROR
00572          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00573
00574      IF LEVELL GREATER ZERO
00575          IF LEVELI NOT NUMERIC
00576              MOVE -1                     TO  LEVELL
00577              MOVE AL-UNBON               TO  LEVELA
00578              MOVE ER-2310                TO  EMI-ERROR
00579              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00580          ELSE
00581              MOVE LEVELI                 TO  WS-CHECK-LEVEL
00582              IF VALID-LEVEL
00583                  IF CHANGE-FUNCTION
00584                      IF LEVELI = PI-SUB
00585                          MOVE AL-UNNON   TO  LEVELA
00586                      ELSE
00587                          MOVE -1         TO  MAINTYPL
00588                          MOVE ER-2056    TO  EMI-ERROR
00589                          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00590                          GO TO 8200-SEND-DATAONLY
00591                  ELSE
00592                      MOVE WS-CHECK-LEVEL TO  PI-SUB
00593                      MOVE AL-UNNON       TO  LEVELA
00594              ELSE
00595                  MOVE -1                 TO  LEVELL
00596                  MOVE AL-UNBON           TO  LEVELA
00597                  MOVE ER-2310            TO  EMI-ERROR
00598                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00599
00600      IF TOTABLEL GREATER ZERO
00601          MOVE ZERO TO TALLY
00602          INSPECT TOTABLEI TALLYING TALLY FOR ALL SPACES
00603          IF TALLY GREATER ZERO
00604              MOVE -1             TO  TOTABLEL
00605              MOVE AL-UABON       TO  TOTABLEA
00606              MOVE ER-2342        TO  EMI-ERROR
00607              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00608          ELSE
00609              MOVE AL-UANON       TO  TOTABLEA
00610      ELSE
00611          IF COPY-FUNCTION
00612              MOVE -1             TO  TOTABLEL
00613              MOVE AL-UABON       TO  TOTABLEA
00614              MOVE ER-2343        TO  EMI-ERROR
00615              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00616
00617      IF ADD-FUNCTION OR COPY-FUNCTION OR CHANGE-FUNCTION
00618          IF (COMPANYI EQUAL ZEROS) AND
00619                      (COMPSUBI EQUAL ZEROS)
00620              MOVE -1             TO  COMPANYL
00621              MOVE AL-UABON       TO  COMPANYA
00622                                      COMPSUBA
00623              MOVE ER-7805        TO  EMI-ERROR
00624              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00625
00626      IF EMI-NO-ERRORS
00627          NEXT SENTENCE
00628      ELSE
00629          GO TO 8200-SEND-DATAONLY.
00630
00631      IF ADD-FUNCTION
00632          GO TO 4200-ADD.
00633
00634      IF CHANGE-FUNCTION
00635          GO TO 4400-CHANGE.
00636
00637      IF COPY-FUNCTION
00638          GO TO 4500-COPY.
00639
00640      IF DELETE-FUNCTION OR EIBAID = DFHPF10
00641          GO TO 4600-DELETE-LEVEL.
00642
00643      IF SHOW-FUNCTION
00644          MOVE 'Y'                TO  WS-CHANGE-SW
00645          GO TO 5000-BUILD-INITIAL-SCREEN.
00646
00647      MOVE -1                     TO  MAINTYPL.
00648      MOVE ER-2056                TO  EMI-ERROR.
00649      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00650      GO TO 8200-SEND-DATAONLY.
00651
00652  4000-EXIT.
00653      EXIT.
00654      EJECT
00655
00656  4200-ADD.
00657      PERFORM 7000-EDIT THRU 7000-EXIT.
00658
00659      IF EMI-NO-ERRORS
00660          NEXT SENTENCE
00661      ELSE
00662          GO TO 8200-SEND-DATAONLY.
00663
00664      
      * EXEC CICS HANDLE CONDITION
00665 *        NOTOPEN  (9990-ABEND)
00666 *        NOTFND   (4250-ADD-REC)
00667 *        END-EXEC.
      *    MOVE '"$JI                  ! # #00003663' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033363633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00668
00669      MOVE 'A'                    TO  PI-ERR-CODE.
00670
00671      PERFORM 7650-READ-ERREIN THRU 7650-EXIT.
00672
00673 ******************************************************************
00674 *  CODE ADDED TO INSURE IF USER HAS CARRIER SECURITY THAT THEY   *
00675 *  ARE ONLY ALLOWED TO APPLY MAINTENANCE TO THOSE REIN. RECORDS  *
00676 *  WITH A MATCHING CARRIER IN THE RE-TABLE-CARRIER-SECURITY FIELD*
00677 ******************************************************************
00678
00679      IF PI-CARRIER-SECURITY GREATER SPACE
00680          IF PI-CARRIER-SECURITY = RE-TABLE-CARRIER-SECURITY
00681              NEXT SENTENCE
00682          ELSE
00683              MOVE -1             TO  MAINTYPL
00684              MOVE ER-2370        TO  EMI-ERROR
00685              MOVE AL-UANON       TO  MAINTYPA
00686              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00687              GO TO 8200-SEND-DATAONLY.
00688
00689      GO TO 4300-ADD-LEVEL.
00690
00691  4250-ADD-REC.
00692      IF WS-GETMAIN-SW = '1'
00693          MOVE ' '                TO  WS-GETMAIN-SW
00694          MOVE SPACES             TO  REINSURANCE-RECORD
00695      ELSE
00696          PERFORM 7700-ERREIN-GETMAIN THRU 7700-EXIT.
00697
00698      MOVE ZEROS                  TO  RE-100-COMP
00699                                      RE-LAST-MAINT-HHMMSS.
00700      MOVE LOW-VALUES             TO  RE-KEY.
00701      MOVE PI-ERR-CODE            TO  RE-CODE.
00702      MOVE PI-ERR-TABLE           TO  RE-TABLE.
00703
00704      MOVE +1                     TO  PI-SUB.
00705
00706  4255-ZERO-RECORD.
00707      MOVE ZEROS                  TO  RE-LFAGE-LO (PI-SUB)
00708                                      RE-AHAGE-LO (PI-SUB)
00709                                      RE-LFTRM-LO (PI-SUB)
00710                                      RE-AHTRM-LO (PI-SUB)
00711                                      RE-LF-PCT (PI-SUB)
00712                                      RE-AH-PCT (PI-SUB)
00713                                      RE-LF-LIM-LO (PI-SUB)
00714                                      RE-LF-LO (PI-SUB)
00715                                      RE-AHBEN-LIM-LO (PI-SUB)
00716                                      RE-AHBEN-LO (PI-SUB)
00717                                      RE-AHMOA-LIM-LO (PI-SUB)
00718                                      RE-AHMOA-LO (PI-SUB)
00719                                      RE-LO-DATE (PI-SUB).
00720
00721       MOVE ALL '9'               TO  RE-LFAGE-HI (PI-SUB)
00722                                      RE-AHAGE-HI (PI-SUB)
00723                                      RE-LFTRM-HI (PI-SUB)
00724                                      RE-AHTRM-HI (PI-SUB)
00725                                      RE-HI-DATE (PI-SUB).
00726
00727      MOVE NINES                  TO  RE-LF-LIM-HI (PI-SUB)
00728                                      RE-LF-HI (PI-SUB)
00729                                      RE-AHBEN-LIM-HI (PI-SUB)
00730                                      RE-AHBEN-HI (PI-SUB)
00731                                      RE-AHMOA-LIM-HI (PI-SUB)
00732                                      RE-AHMOA-HI (PI-SUB).
00733      ADD +1                      TO  PI-SUB.
00734
00735      IF PI-SUB GREATER +30
00736          MOVE +1                 TO  PI-SUB
00737      ELSE
00738          GO TO 4255-ZERO-RECORD.
00739
00740      PERFORM 6000-CHECK-FOR-UPDATE THRU 6099-EXIT.
00741
00742      MOVE PI-PROCESSOR-ID        TO  RE-LAST-MAINT-USER.
00743      MOVE EIBTIME                TO  RE-LAST-MAINT-HHMMSS.
00744      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00745
00746      MOVE '5'                    TO  DC-OPTION-CODE.
00747      MOVE LINK-ELDATCV           TO  PGM-NAME.
00748
00749      
      * EXEC CICS LINK
00750 *        PROGRAM   (PGM-NAME)
00751 *        COMMAREA  (DATE-CONVERSION-DATA)
00752 *        LENGTH    (DC-COMM-LENGTH)
00753 *    END-EXEC.
      *    MOVE '."C                   ''   #00003748' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00754
00755      MOVE DC-BIN-DATE-1          TO  RE-LAST-MAINT-DT
00756                                      BIN-CURRENT-SAVE.
00757      MOVE PI-COMPANY-CD          TO  RE-COMPANY-CD.
00758      MOVE 'RE'                   TO  RE-RECORD-ID.
00759 *    MOVE REIN-FILE-ID           TO  FILE-ID.
00760 *    MOVE 'A'                    TO  JP-RECORD-TYPE
00761 *    MOVE ERREIN-LENGTH          TO  FILE-LENGTH
00762 *    MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA.
00763
00764      
      * EXEC CICS WRITE
00765 *        DATASET (REIN-FILE-ID)
00766 *        FROM    (REINSURANCE-RECORD)
00767 *        RIDFLD  (RE-CONTROL-PRIMARY)
00768 *    END-EXEC.
           MOVE LENGTH OF
            REINSURANCE-RECORD
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003763' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 REINSURANCE-RECORD, 
                 DFHEIV11, 
                 RE-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00769
00770 *    PERFORM 8400-LOG-JOURNAL-RECORD.
00771      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.
00772
00773      IF COMPANY-RECORD-ADDED
00774          MOVE SPACE              TO  PI-COMPANY-ADD-SW
00775          MOVE ER-2345            TO  EMI-ERROR
00776      ELSE
00777          MOVE ER-0000            TO  EMI-ERROR.
00778
00779      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00780
00781      GO TO 5000-BUILD-INITIAL-SCREEN.
00782
00783  4200-EXIT.
00784      EXIT.
00785      EJECT
00786
00787  4300-ADD-LEVEL.
00788      IF PI-LAST-LEVEL = +30
00789          MOVE -1                 TO  MAINTYPL
00790          MOVE ER-2344            TO  EMI-ERROR
00791          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00792          GO TO 8200-SEND-DATAONLY.
00793
00794      PERFORM 7750-READ-ERREIN-UPDATE THRU 7750-EXIT.
00795
00796 *    MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA.
00797
00798      IF EXCESS-LEVEL-EXISTS
00799          MOVE RE-COMP-INFO (PI-LAST-LEVEL) TO WS-SAVE-REC
00800          MOVE PI-SUB             TO  WS-SAVE-PI-SUB
00801          MOVE PI-LAST-LEVEL      TO  PI-SUB
00802          PERFORM 6000-CHECK-FOR-UPDATE THRU 6099-EXIT
00803          MOVE WS-SAVE-PI-SUB     TO  PI-SUB
00804          ADD +1                  TO  PI-LAST-LEVEL
00805                                      RE-100-COMP
00806          MOVE WS-SAVE-REC        TO  RE-COMP-INFO (PI-LAST-LEVEL)
00807      ELSE
00808          ADD +1                  TO  PI-LAST-LEVEL
00809          MOVE PI-SUB             TO  WS-SAVE-PI-SUB
00810          MOVE PI-LAST-LEVEL      TO  PI-SUB
00811          PERFORM 6000-CHECK-FOR-UPDATE THRU 6099-EXIT
00812          MOVE WS-SAVE-PI-SUB     TO  PI-SUB.
00813
00814      IF RE-LAST-MAINT-USER = PI-UPDATE-BY OR
00815         RE-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS
00816          NEXT SENTENCE
00817      ELSE
00818          
      * EXEC CICS UNLOCK
00819 *             DATASET  (REIN-FILE-ID)
00820 *        END-EXEC
      *    MOVE '&*                    #   #00003817' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00821          MOVE ER-0068            TO  EMI-ERROR
00822          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00823          GO TO 5000-BUILD-INITIAL-SCREEN.
00824
00825      MOVE PI-PROCESSOR-ID        TO  RE-LAST-MAINT-USER.
00826      MOVE EIBTIME                TO  RE-LAST-MAINT-HHMMSS.
00827      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00828      MOVE '5'                    TO  DC-OPTION-CODE.
00829      MOVE LINK-ELDATCV           TO  PGM-NAME.
00830
00831      
      * EXEC CICS LINK
00832 *        PROGRAM   (PGM-NAME)
00833 *        COMMAREA  (DATE-CONVERSION-DATA)
00834 *        LENGTH    (DC-COMM-LENGTH)
00835 *    END-EXEC.
      *    MOVE '."C                   ''   #00003830' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00836
00837      MOVE DC-BIN-DATE-1          TO  RE-LAST-MAINT-DT
00838                                      BIN-CURRENT-SAVE.
00839 *    MOVE 'B'                    TO  JP-RECORD-TYPE
00840 *    MOVE REIN-FILE-ID           TO  FILE-ID.
00841 *    MOVE ERREIN-LENGTH          TO  FILE-LENGTH
00842 *    PERFORM 8400-LOG-JOURNAL-RECORD
00843 *    MOVE REINSURANCE-RECORD     TO JP-RECORD-AREA.
00844
00845      
      * EXEC CICS REWRITE
00846 *        DATASET  (REIN-FILE-ID)
00847 *        FROM     (REINSURANCE-RECORD)
00848 *    END-EXEC.
           MOVE LENGTH OF
            REINSURANCE-RECORD
             TO DFHEIV11
      *    MOVE '&& L                  %   #00003844' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 REINSURANCE-RECORD, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00849
00850 *    MOVE 'C'                    TO  JP-RECORD-TYPE
00851 *    MOVE ERREIN-LENGTH          TO  FILE-LENGTH
00852 *    PERFORM 8400-LOG-JOURNAL-RECORD.
00853      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.
00854      MOVE ER-0000                TO  EMI-ERROR.
00855      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00856
00857      MOVE 'Y'                    TO  WS-CHANGE-SW.
00858
00859      GO TO 5000-BUILD-INITIAL-SCREEN.
00860
00861  4300-EXIT.
00862      EXIT.
00863      EJECT
00864  4400-CHANGE.
00865      IF PI-ERREIN-KEY = PI-SAVE-ERREIN-KEY
00866          NEXT SENTENCE
00867      ELSE
00868          MOVE ER-2056            TO  EMI-ERROR
00869          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00870          MOVE -1                 TO  MAINTYPL
00871          GO TO 8200-SEND-DATAONLY.
00872
00873      PERFORM 7000-EDIT               THRU 7000-EXIT.
00874
00875      IF EMI-NO-ERRORS
00876          NEXT SENTENCE
00877      ELSE
00878          GO TO 8200-SEND-DATAONLY.
00879
00880      PERFORM 7750-READ-ERREIN-UPDATE THRU 7750-EXIT.
00881
00882 *    MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA.
00883
00884      PERFORM 6000-CHECK-FOR-UPDATE   THRU 6099-EXIT.
00885
00886      IF RE-LAST-MAINT-USER = PI-UPDATE-BY OR
00887         RE-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS
00888          NEXT SENTENCE
00889      ELSE
00890          
      * EXEC CICS UNLOCK
00891 *             DATASET  (REIN-FILE-ID)
00892 *        END-EXEC
      *    MOVE '&*                    #   #00003889' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00893          MOVE ER-0068            TO  EMI-ERROR
00894          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00895          GO TO 8200-SEND-DATAONLY.
00896
00897      MOVE PI-PROCESSOR-ID        TO  RE-LAST-MAINT-USER.
00898      MOVE EIBTIME                TO  RE-LAST-MAINT-HHMMSS.
00899      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00900      MOVE '5'                    TO  DC-OPTION-CODE.
00901      MOVE LINK-ELDATCV           TO  PGM-NAME.
00902
00903      
      * EXEC CICS LINK
00904 *        PROGRAM   (PGM-NAME)
00905 *        COMMAREA  (DATE-CONVERSION-DATA)
00906 *        LENGTH    (DC-COMM-LENGTH)
00907 *    END-EXEC.
      *    MOVE '."C                   ''   #00003902' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00908
00909      MOVE DC-BIN-DATE-1          TO  RE-LAST-MAINT-DT
00910                                      BIN-CURRENT-SAVE.
00911 *    MOVE 'B'                    TO  JP-RECORD-TYPE.
00912 *    MOVE REIN-FILE-ID           TO  FILE-ID.
00913 *    MOVE ERREIN-LENGTH          TO  FILE-LENGTH.
00914 *    PERFORM 8400-LOG-JOURNAL-RECORD.
00915 *    MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA.
00916
00917      
      * EXEC CICS REWRITE
00918 *        DATASET  (REIN-FILE-ID)
00919 *        FROM     (REINSURANCE-RECORD)
00920 *    END-EXEC.
           MOVE LENGTH OF
            REINSURANCE-RECORD
             TO DFHEIV11
      *    MOVE '&& L                  %   #00003916' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 REINSURANCE-RECORD, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00921
00922 *    MOVE 'C'                    TO  JP-RECORD-TYPE
00923 *    MOVE ERREIN-LENGTH          TO  FILE-LENGTH
00924 *    PERFORM 8400-LOG-JOURNAL-RECORD.
00925      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.
00926      MOVE ER-0000                TO  EMI-ERROR.
00927      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00928
00929      MOVE 'Y'                    TO  WS-CHANGE-SW.
00930
00931      GO TO 5000-BUILD-INITIAL-SCREEN.
00932
00933  4400-EXIT.
00934      EXIT.
00935      EJECT
00936
00937  4500-COPY.
00938      MOVE LOW-VALUES             TO  PI-ERREIN-KEY.
00939      MOVE 'A'                    TO  PI-ERR-CODE.
00940      MOVE TOTABLEI               TO  PI-ERR-TABLE.
00941
00942      
      * EXEC CICS HANDLE CONDITION
00943 *        NOTOPEN  (9990-ABEND)
00944 *        NOTFND   (4550-COPY-REC)
00945 *    END-EXEC.
      *    MOVE '"$JI                  ! $ #00003941' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303033393431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00946
00947      PERFORM 7650-READ-ERREIN THRU 7650-EXIT.
00948
00949      MOVE -1                     TO  TOTABLEL.
00950      MOVE AL-UABON               TO  TOTABLEA.
00951      MOVE ER-2139                TO  EMI-ERROR.
00952      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00953
00954      GO TO 8200-SEND-DATAONLY.
00955
00956  4550-COPY-REC.
00957      
      * EXEC CICS HANDLE CONDITION
00958 *        NOTFND (8880-NOT-FOUND)
00959 *    END-EXEC.
      *    MOVE '"$I                   ! % #00003956' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303033393536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00960
00961      MOVE TABLEI                 TO  PI-ERR-TABLE.
00962
00963      PERFORM 7650-READ-ERREIN THRU 7650-EXIT.
00964
00965      MOVE TOTABLEI               TO  PI-ERR-TABLE
00966                                      RE-TABLE.
00967
00968      MOVE PI-PROCESSOR-ID        TO  RE-LAST-MAINT-USER.
00969      MOVE EIBTIME                TO  RE-LAST-MAINT-HHMMSS.
00970      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00971
00972      MOVE '5'                    TO  DC-OPTION-CODE.
00973      MOVE LINK-ELDATCV           TO  PGM-NAME.
00974
00975      
      * EXEC CICS LINK
00976 *        PROGRAM   (PGM-NAME)
00977 *        COMMAREA  (DATE-CONVERSION-DATA)
00978 *        LENGTH    (DC-COMM-LENGTH)
00979 *    END-EXEC.
      *    MOVE '."C                   ''   #00003974' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00980
00981      MOVE DC-BIN-DATE-1           TO  RE-LAST-MAINT-DT
00982                                       BIN-CURRENT-SAVE.
00983      MOVE PI-COMPANY-CD           TO  RE-COMPANY-CD.
00984 *    MOVE 'RE'                    TO  RE-RECORD-ID.
00985 *    MOVE REIN-FILE-ID            TO  FILE-ID.
00986 *    MOVE 'A'                     TO  JP-RECORD-TYPE.
00987 *    MOVE ERREIN-LENGTH           TO  FILE-LENGTH.
00988 *    MOVE REINSURANCE-RECORD      TO  JP-RECORD-AREA.
00989
00990      
      * EXEC CICS WRITE
00991 *        DATASET (REIN-FILE-ID)
00992 *        FROM    (REINSURANCE-RECORD)
00993 *        RIDFLD  (RE-CONTROL-PRIMARY)
00994 *    END-EXEC.
           MOVE LENGTH OF
            REINSURANCE-RECORD
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003989' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 REINSURANCE-RECORD, 
                 DFHEIV11, 
                 RE-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00995
00996 *    PERFORM 8400-LOG-JOURNAL-RECORD.
00997      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.
00998      MOVE ER-0000                TO  EMI-ERROR.
00999      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01000
01001      MOVE LOW-VALUES             TO  EL651AO.
01002
01003      MOVE PI-ERR-TABLE           TO  TABLEO.
01004      MOVE AL-UANON               TO  TABLEA.
01005
01006      GO TO 8100-SEND-INITIAL-MAP.
01007
01008  4500-EXIT.
01009      EXIT.
01010      EJECT
01011
01012  4600-DELETE-LEVEL.
01013      IF PI-ERREIN-KEY = PI-SAVE-ERREIN-KEY
01014          NEXT SENTENCE
01015      ELSE
01016          MOVE ER-2056            TO  EMI-ERROR
01017          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01018          MOVE -1                 TO  MAINTYPL
01019          GO TO 8200-SEND-DATAONLY.
01020
01021      IF (PI-SUB  =  +1 AND PI-LAST-LEVEL = +1) OR
01022          EIBAID = DFHPF10
01023              GO TO 4800-DELETE.
01024
01025      PERFORM 7750-READ-ERREIN-UPDATE THRU 7750-EXIT.
01026
01027      MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA.
01028
01029  4625-ZERO-RECORD.
01030      IF RE-REI-COMP (PI-SUB) NOT = SPACES
01031          IF PI-SUB NOT = +30
01032              MOVE PI-SUB         TO  SUB1
01033              ADD +1              TO  SUB1
01034              MOVE RE-COMP-INFO (SUB1)
01035                                  TO  RE-COMP-INFO (PI-SUB)
01036              ADD +1              TO  PI-SUB
01037              GO TO 4625-ZERO-RECORD.
01038
01039      IF RE-REI-COMP (PI-SUB) = SPACES
01040          GO TO 4650-CONT.
01041
01042      MOVE SPACES                 TO  RE-COMP-INFO (30).
01043      MOVE ZEROS                  TO  RE-LFAGE-LO (30)
01044                                      RE-AHAGE-LO (30)
01045                                      RE-LFTRM-LO (30)
01046                                      RE-AHTRM-LO (30)
01047                                      RE-LF-PCT (30)
01048                                      RE-AH-PCT (30)
01049                                      RE-LF-LIM-LO (30)
01050                                      RE-LF-LO (30)
01051                                      RE-AHBEN-LIM-LO (30)
01052                                      RE-AHBEN-LO (30)
01053                                      RE-AHMOA-LIM-LO (30)
01054                                      RE-AHMOA-LO (30)
01055                                      RE-LO-DATE (30).
01056
01057       MOVE ALL '9'               TO  RE-LFAGE-HI (30)
01058                                      RE-AHAGE-HI (30)
01059                                      RE-LFTRM-HI (30)
01060                                      RE-AHTRM-HI (30)
01061                                      RE-HI-DATE (30).
01062
01063      MOVE NINES                  TO  RE-LF-LIM-HI (30)
01064                                      RE-LF-HI (30)
01065                                      RE-AHBEN-LIM-HI (30)
01066                                      RE-AHBEN-HI (30)
01067                                      RE-AHMOA-LIM-HI (30)
01068                                      RE-AHMOA-HI (30).
01069
01070  4650-CONT.
01071      MOVE PI-PROCESSOR-ID        TO  RE-LAST-MAINT-USER.
01072      MOVE EIBTIME                TO  RE-LAST-MAINT-HHMMSS.
01073      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
01074      MOVE '5'                    TO  DC-OPTION-CODE.
01075      MOVE LINK-ELDATCV           TO  PGM-NAME.
01076
01077      
      * EXEC CICS LINK
01078 *        PROGRAM   (PGM-NAME)
01079 *        COMMAREA  (DATE-CONVERSION-DATA)
01080 *        LENGTH    (DC-COMM-LENGTH)
01081 *    END-EXEC.
      *    MOVE '."C                   ''   #00004076' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01082
01083      MOVE DC-BIN-DATE-1          TO  RE-LAST-MAINT-DT
01084                                      BIN-CURRENT-SAVE.
01085 *    MOVE REIN-FILE-ID           TO  FILE-ID.
01086 *    MOVE 'B'                    TO  JP-RECORD-TYPE.
01087 *    MOVE ERREIN-LENGTH          TO  FILE-LENGTH.
01088 *    PERFORM 8400-LOG-JOURNAL-RECORD.
01089 *    MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA.
01090
01091      
      * EXEC CICS REWRITE
01092 *        DATASET  (REIN-FILE-ID)
01093 *        FROM     (REINSURANCE-RECORD)
01094 *    END-EXEC.
           MOVE LENGTH OF
            REINSURANCE-RECORD
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004090' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 REINSURANCE-RECORD, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01095
01096 *    MOVE 'C'                    TO  JP-RECORD-TYPE
01097 *    MOVE ERREIN-LENGTH          TO  FILE-LENGTH
01098 *    PERFORM 8400-LOG-JOURNAL-RECORD.
01099      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.
01100      MOVE ER-0000                TO  EMI-ERROR.
01101      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01102
01103      MOVE +1                     TO  PI-SUB.
01104
01105      GO TO 5000-BUILD-INITIAL-SCREEN.
01106
01107  4600-EXIT.
01108      EXIT.
01109      EJECT
01110  4800-DELETE.
01111      IF PI-ERREIN-KEY = PI-SAVE-ERREIN-KEY
01112          NEXT SENTENCE
01113      ELSE
01114          MOVE ER-2056            TO  EMI-ERROR
01115          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01116          MOVE -1                 TO  MAINTYPL
01117          GO TO 8200-SEND-DATAONLY.
01118
01119      PERFORM 7750-READ-ERREIN-UPDATE THRU 7750-EXIT.
01120
01121 *    MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA.
01122
01123      IF RE-LAST-MAINT-USER = PI-UPDATE-BY OR
01124         RE-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS
01125          NEXT SENTENCE
01126      ELSE
01127          
      * EXEC CICS UNLOCK
01128 *             DATASET  (REIN-FILE-ID)
01129 *        END-EXEC
      *    MOVE '&*                    #   #00004126' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01130          MOVE ER-0068            TO  EMI-ERROR
01131          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01132          GO TO 8200-SEND-DATAONLY.
01133
01134      
      * EXEC CICS DELETE
01135 *        DATASET  (REIN-FILE-ID)
01136 *    END-EXEC.
      *    MOVE '&(                    &   #00004133' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01137
01138 *    MOVE REIN-FILE-ID           TO  FILE-ID.
01139 *    MOVE ERREIN-LENGTH          TO  FILE-LENGTH.
01140 *    PERFORM 8400-LOG-JOURNAL-RECORD.
01141
01142      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
01143      MOVE '5'                    TO  DC-OPTION-CODE.
01144      MOVE LINK-ELDATCV           TO  PGM-NAME.
01145
01146      
      * EXEC CICS LINK
01147 *        PROGRAM   (PGM-NAME)
01148 *        COMMAREA  (DATE-CONVERSION-DATA)
01149 *        LENGTH    (DC-COMM-LENGTH)
01150 *    END-EXEC.
      *    MOVE '."C                   ''   #00004145' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01151
01152      MOVE DC-BIN-DATE-1          TO  BIN-CURRENT-SAVE.
01153      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.
01154
01155      MOVE ER-0000                TO  EMI-ERROR.
01156      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01157
01158      MOVE LOW-VALUES             TO  EL651AO.
01159
01160      MOVE PI-ERR-TABLE           TO  TABLEO.
01161      MOVE AL-UANON               TO  TABLEA.
01162
01163      GO TO 8100-SEND-INITIAL-MAP.
01164
01165  4800-EXIT.
01166      EXIT.
01167      EJECT
01168
01169  5000-BUILD-INITIAL-SCREEN.
01170      MOVE LOW-VALUES             TO  EL651AO.
01171      MOVE ZEROS                  TO  MISC-SAVE-AREAS.
01172      MOVE PI-COMPANY-CD          TO  PI-ERR-COMPANY-CD.
01173      MOVE 'A'                    TO  PI-ERR-CODE.
01174
01175      PERFORM 7650-READ-ERREIN THRU 7650-EXIT.
01176
01177 ******************************************************************
01178 *    ADD CODE TO SHOW RECORDS WHICH HAVE A MATCHING CARRIER IF   *
01179 *    USER HAS CARRIER SECURITY.                                  *
01180 ******************************************************************
01181
01182      IF PI-CARRIER-SECURITY GREATER SPACE
01183          IF PI-CARRIER-SECURITY = RE-TABLE-CARRIER-SECURITY
01184              NEXT SENTENCE
01185          ELSE
01186              GO TO 8880-NOT-FOUND.
01187
01188      MOVE RE-LAST-MAINT-USER     TO  PI-UPDATE-BY.
01189      MOVE RE-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.
01190      MOVE PI-ERREIN-KEY          TO  PI-SAVE-ERREIN-KEY.
01191      MOVE ZEROS                  TO  PI-LAST-LEVEL.
01192
01193      IF CHANGES-NOT-MADE
01194          MOVE +1                 TO  PI-SUB.
01195
01196      IF RE-100-COMP = ZERO
01197          MOVE SPACE              TO  PI-EXCESS-SW
01198      ELSE
01199          MOVE 'X'                TO  PI-EXCESS-SW.
01200
01201  5050-SET-UP-SCREEN.
01202      IF PI-SUB NOT NUMERIC
01203          MOVE +1                 TO  PI-SUB.
01204
01205      MOVE PI-SUB                 TO  WS-CHECK-LEVEL
01206
01207      IF VALID-LEVEL
01208         IF RE-REI-COMP (PI-SUB) = SPACES
01209            MOVE +1               TO  PI-SUB
01210            MOVE ER-2338          TO  EMI-ERROR
01211            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01212         ELSE
01213            NEXT SENTENCE
01214      ELSE
01215         MOVE +1                  TO  PI-SUB
01216         MOVE ER-2338             TO  EMI-ERROR
01217         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01218
01219      IF RE-AH-PCT (PI-SUB) NOT NUMERIC
01220         GO TO 5070-SEARCH-LAST-LEVEL.
01221
01222      IF RE-AH-PCT (PI-SUB) = ZEROS
01223          NEXT SENTENCE
01224      ELSE
01225          MOVE RE-AH-PCT (PI-SUB) TO  AHBENO
01226          MOVE AL-UNNON           TO  AHBENA.
01227
01228      IF RE-LF-PCT (PI-SUB) = ZEROS
01229          NEXT SENTENCE
01230      ELSE
01231          MOVE RE-LF-PCT (PI-SUB)     TO  LFBENO
01232          MOVE AL-UNNON               TO  LFBENA.
01233
01234      IF RE-LO-DATE (PI-SUB) = ZEROS
01235          MOVE WS-LOW-DATE            TO  FDATEO
01236          MOVE AL-UNNON               TO  FDATEA
01237      ELSE
01238          MOVE RE-LO-DATE (PI-SUB)    TO  DC-GREG-DATE-1-YMD
01239                                          WS-LO-DATE
01240          MOVE '3'                    TO  DC-OPTION-CODE
01241          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01242          MOVE DC-GREG-DATE-1-EDIT    TO  FDATEO
01243          MOVE AL-UNNON               TO  FDATEA.
01244
01245 ***  Y2K PROJ 7744
01246      MOVE RE-HI-DATE(PI-SUB)         TO WS-HOLD-PIC98.
01247
01248      IF WS-HOLD-PIC98 = 99999999
01249          MOVE WS-HIGH-DATE           TO  TDATEO
01250          MOVE AL-UNNON               TO  TDATEA
01251      ELSE
01252          MOVE RE-HI-DATE (PI-SUB)    TO  DC-GREG-DATE-1-YMD
01253                                          WS-HI-DATE
01254          MOVE '3'                    TO  DC-OPTION-CODE
01255          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01256          MOVE DC-GREG-DATE-1-EDIT    TO  TDATEO
01257          MOVE AL-UNNON               TO  TDATEA
01258          MOVE DC-ALPHA-CENTURY       TO  WS-HI-DATE(1:2).
01259
01260      MOVE RE-LF-LIM-LO (PI-SUB)         TO  LFLOBENO.
01261      MOVE RE-LF-LIM-HI (PI-SUB)         TO  LFHIBENO.
01262      MOVE RE-AHBEN-LIM-LO (PI-SUB)      TO  AHLOBENO.
01263      MOVE RE-AHBEN-LIM-HI (PI-SUB)      TO  AHHIBENO.
01264      MOVE RE-AHMOA-LIM-LO (PI-SUB)      TO  AMLOBENO.
01265      MOVE RE-AHMOA-LIM-HI (PI-SUB)      TO  AMHIBENO.
01266      MOVE RE-LF-LO (PI-SUB)             TO  LFLOAMTO.
01267      MOVE RE-LF-HI (PI-SUB)             TO  LFHIAMTO.
01268      MOVE RE-AHBEN-LO (PI-SUB)          TO  AHLOAMTO.
01269      MOVE RE-AHBEN-HI (PI-SUB)          TO  AHHIAMTO.
01270      MOVE RE-AHMOA-LO (PI-SUB)          TO  AMLOAMTO.
01271      MOVE RE-AHMOA-HI (PI-SUB)          TO  AMHIAMTO.
01272      MOVE RE-REI-COMP (PI-SUB)          TO  COMPANYO
01273                                             PI-SAVE-COMPANY.
01274      MOVE RE-REI-COMP-SUB (PI-SUB)      TO  COMPSUBO
01275                                             PI-SAVE-COMP-SUB.
01276      MOVE RE-LFAGE-LO (PI-SUB)          TO  LFLOAGEO.
01277      MOVE RE-LFAGE-HI (PI-SUB)          TO  LFHIAGEO.
01278      MOVE RE-LFTRM-LO (PI-SUB)          TO  LFLOTRMO.
01279      MOVE RE-LFTRM-HI (PI-SUB)          TO  LFHITRMO.
01280      MOVE RE-LF-BEN-CODE (PI-SUB)       TO  LIFEBENO.
01281      MOVE RE-AHAGE-LO (PI-SUB)          TO  AHLOAGEO.
01282      MOVE RE-AHAGE-HI (PI-SUB)          TO  AHHIAGEO.
01283      MOVE RE-AHTRM-LO (PI-SUB)          TO  AHLOTRMO.
01284      MOVE RE-AHTRM-HI (PI-SUB)          TO  AHHITRMO.
01285      MOVE RE-AH-BEN-CODE (PI-SUB)       TO  AHTBENO.
01286      MOVE RE-LF-QC (PI-SUB)             TO  LFQTEDO.
01287      MOVE RE-AH-QC (PI-SUB)             TO  AHQTEDO.
01288      MOVE RE-REMAINING (PI-SUB)         TO  LFREMO.
01289
01290      MOVE RE-INTERACTIVE (PI-SUB)       TO  LFINTRO.
01291      MOVE RE-NSP-ST-CD-LF               TO  LFSTATEO.
01292      MOVE RE-NSP-ST-CD-AH               TO  AHSTATEO.
01293      MOVE RE-TABLE-CARRIER-SECURITY     TO  CARRIERO.
01294
01295      IF PI-NO-CARRIER-SECURITY
01296          MOVE AL-UANOF                  TO  CARRIERA
01297      ELSE
01298          MOVE AL-SANOF                  TO  CARRIERA.
01299
01300      MOVE AL-UANON               TO  AHTBENA     TABLEA
01301                                      LFREMA      COMPANYA
01302                                      COMPSUBA    AHQTEDA
01303                                      LIFEBENA    LFQTEDA
01304                                      LFINTRA.
01305
01306      MOVE AL-UNNON               TO  LFHIBENA    AHLOAMTA
01307                                      LFHIAMTA    AMLOAMTA
01308                                      AHHIAMTA    AMLOBENA
01309                                      AHHIBENA    LFLOAGEA
01310                                      AMHIBENA    LFHIAGEA
01311                                      AMHIAMTA    AHLOAGEA
01312                                      AHLOTRMA    AHHIAGEA
01313                                      AHHITRMA    LFLOTRMA
01314                                      LFLOBENA    LFHITRMA
01315                                      AHLOBENA    LFLOAMTA.
01316
01317      MOVE PI-SUB                 TO   PI-LAST-LEVEL
01318                                       SUB1.
01319
01320      MOVE RE-LAST-MAINT-DT       TO  DC-BIN-DATE-1.
01321      MOVE SPACE                  TO  DC-OPTION-CODE.
01322      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
01323      MOVE DC-GREG-DATE-1-EDIT    TO  LMDATEO.
01324
01325      MOVE RE-LAST-MAINT-USER     TO  LMBYO.
01326      MOVE RE-LAST-MAINT-HHMMSS   TO  TIME-MT.
01327      MOVE TIME-LMT               TO  LMTIMEO.
01328
01329  5070-SEARCH-LAST-LEVEL.
01330      ADD +1                      TO  SUB1.
01331
01332      IF SUB1 GREATER +30
01333          NEXT SENTENCE
01334      ELSE
01335          IF RE-REI-COMP (SUB1) = SPACES
01336              NEXT SENTENCE
01337          ELSE
01338              MOVE SUB1           TO  PI-LAST-LEVEL
01339              GO TO 5070-SEARCH-LAST-LEVEL.
01340
01341      IF EXCESS-LEVEL-EXISTS AND
01342         PI-SUB = PI-LAST-LEVEL
01343            MOVE 'X'              TO  PERCNTO
01344            MOVE AL-UANON         TO  PERCNTA.
01345
01346      MOVE PI-SUB                 TO  LEVELO.
01347      MOVE PI-LAST-LEVEL          TO  OFLEVELO.
01348      MOVE PI-ERR-TABLE           TO  TABLEO.
01349      MOVE AL-SANON               TO  OFLEVELA.
01350      MOVE AL-UNNOF               TO  LEVELA.
01351
01352      MOVE SPACE                  TO  PI-FIRST-TIME-SW.
01353      GO TO 8100-SEND-INITIAL-MAP.
01354
01355      EJECT
01356  6000-CHECK-FOR-UPDATE.
01357       IF CHANGE-FUNCTION
01358           GO TO 6010-CONT.
01359
01360       IF TABLEL GREATER ZERO
01361           MOVE TABLEI            TO  RE-TABLE.
01362
01363  6010-CONT.
01364       IF COMPANYL GREATER ZERO
01365           MOVE COMPANYI          TO  RE-REI-COMP (PI-SUB).
01366
01367       IF COMPSUBL GREATER ZERO
01368           MOVE COMPSUBI          TO  RE-REI-COMP-SUB (PI-SUB).
01369
01370       IF FDATEL GREATER ZERO
01371           MOVE WS-LO-DATE        TO  RE-LO-DATE (PI-SUB).
01372
01373       IF TDATEL GREATER ZERO
01374           MOVE WS-HI-DATE        TO  RE-HI-DATE (PI-SUB).
01375
01376       IF PERCNTL GREATER ZERO
01377           IF PERCNTI = SPACE
01378               MOVE ZEROS         TO  RE-100-COMP
01379           ELSE
01380               MOVE PI-SUB        TO  RE-100-COMP.
01381
01382       IF LFLOAGEL GREATER ZERO
01383           MOVE WS-LFAGE-LO       TO  RE-LFAGE-LO (PI-SUB).
01384
01385       IF LFHIAGEL GREATER ZERO
01386           MOVE WS-LFAGE-HI       TO  RE-LFAGE-HI (PI-SUB).
01387
01388       IF AHLOAGEL GREATER ZERO
01389           MOVE WS-AHAGE-LO       TO  RE-AHAGE-LO (PI-SUB).
01390
01391       IF AHHIAGEL GREATER ZERO
01392           MOVE WS-AHAGE-HI       TO  RE-AHAGE-HI (PI-SUB).
01393
01394       IF LFLOTRML GREATER ZERO
01395           MOVE WS-LFTRM-LO       TO  RE-LFTRM-LO (PI-SUB).
01396
01397       IF LFHITRML GREATER ZERO
01398           MOVE WS-LFTRM-HI       TO  RE-LFTRM-HI (PI-SUB).
01399
01400       IF AHLOTRML GREATER ZERO
01401           MOVE WS-AHTRM-LO       TO  RE-AHTRM-LO (PI-SUB).
01402
01403       IF AHHITRML GREATER ZERO
01404           MOVE WS-AHTRM-HI       TO  RE-AHTRM-HI (PI-SUB).
01405
01406       IF LFLOBENL GREATER ZERO
01407           MOVE WS-LF-LIM-LO      TO  RE-LF-LIM-LO (PI-SUB).
01408
01409       IF LFHIBENL GREATER ZERO
01410           MOVE WS-LF-LIM-HI      TO  RE-LF-LIM-HI (PI-SUB).
01411
01412       IF AHLOBENL GREATER ZERO
01413           MOVE WS-AHBEN-LIM-LO   TO  RE-AHBEN-LIM-LO (PI-SUB).
01414
01415       IF AHHIBENL GREATER ZERO
01416           MOVE WS-AHBEN-LIM-HI   TO  RE-AHBEN-LIM-HI (PI-SUB).
01417
01418       IF AMLOBENL GREATER ZERO
01419           MOVE WS-AHMOA-LIM-LO   TO  RE-AHMOA-LIM-LO (PI-SUB).
01420
01421       IF AMHIBENL GREATER ZERO
01422           MOVE WS-AHMOA-LIM-HI   TO  RE-AHMOA-LIM-HI (PI-SUB).
01423
01424       IF LFLOAMTL GREATER ZERO
01425           MOVE WS-LF-LO          TO  RE-LF-LO (PI-SUB).
01426
01427       IF LFHIAMTL GREATER ZERO
01428           MOVE WS-LF-HI          TO  RE-LF-HI (PI-SUB).
01429
01430       IF AHLOAMTL GREATER ZERO
01431           MOVE WS-AHBEN-LO       TO  RE-AHBEN-LO (PI-SUB).
01432
01433       IF AHHIAMTL GREATER ZERO
01434           MOVE WS-AHBEN-HI       TO  RE-AHBEN-HI (PI-SUB).
01435
01436       IF AMLOAMTL GREATER ZERO
01437           MOVE WS-AHMOA-LO       TO  RE-AHMOA-LO (PI-SUB).
01438
01439       IF AMHIAMTL GREATER ZERO
01440           MOVE WS-AHMOA-HI       TO  RE-AHMOA-HI (PI-SUB).
01441
01442       IF LIFEBENL GREATER ZERO
01443           MOVE LIFEBENI          TO  RE-LF-BEN-CODE (PI-SUB).
01444
01445       IF LFSTATEL GREATER ZERO
01446           MOVE LFSTATEI          TO  RE-NSP-ST-CD-LF
01447           MOVE ER-0585           TO  EMI-ERROR
01448           PERFORM 9900-ERROR-FORMAT
01449              THRU 9900-EXIT.
01450
01451       IF AHSTATEL GREATER ZERO
01452           MOVE AHSTATEI          TO  RE-NSP-ST-CD-AH
01453           MOVE ER-0585           TO  EMI-ERROR
01454           PERFORM 9900-ERROR-FORMAT
01455              THRU 9900-EXIT.
01456
01457       IF AHTBENL GREATER ZERO
01458           MOVE AHTBENI           TO  RE-AH-BEN-CODE (PI-SUB).
01459
01460       IF LFBENL GREATER ZERO
01461           MOVE LFBENI            TO  RE-LF-PCT (PI-SUB)
01462       ELSE
01463           MOVE ZEROS             TO  RE-LF-PCT (PI-SUB).
01464
01465       IF AHBENL GREATER ZERO
01466           MOVE AHBENI            TO  RE-AH-PCT (PI-SUB)
01467       ELSE
01468           MOVE ZEROS             TO  RE-AH-PCT (PI-SUB).
01469
01470       IF LFBENL GREATER ZERO AND
01471          AHBENL GREATER ZERO AND
01472          AHBENI = ZEROS      AND
01473          LFBENI = ZEROS
01474           MOVE ER-0593           TO  EMI-ERROR
01475           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01476
01477       IF LFINTRL GREATER ZERO
01478           MOVE LFINTRI           TO  RE-INTERACTIVE (PI-SUB).
01479
01480       IF LFQTEDL GREATER ZERO
01481           MOVE LFQTEDI           TO  RE-LF-QC (PI-SUB).
01482
01483       IF AHQTEDL GREATER ZERO
01484           MOVE AHQTEDI           TO  RE-AH-QC (PI-SUB).
01485
01486       IF LFREML GREATER ZERO
01487           MOVE LFREMI            TO  RE-REMAINING (PI-SUB).
01488
01489       IF CARRIERL GREATER ZERO
01490           MOVE CARRIERI          TO  RE-TABLE-CARRIER-SECURITY.
01491
01492       MOVE 'A'                   TO  RE-CODE.
01493
01494  6099-EXIT.
01495      EXIT.
01496      EJECT
01497
01498  7000-EDIT.
01499      IF COMPSUBL = ZERO
01500          MOVE AL-UANON           TO  COMPSUBA
01501          MOVE SPACES             TO  COMPSUBI.
01502
01503      IF COMPANYL GREATER ZERO
01504          MOVE ZERO TO TALLY
01505          INSPECT COMPANYI TALLYING TALLY FOR ALL SPACES
01506          IF TALLY GREATER ZERO
01507              MOVE -1             TO  COMPANYL
01508              MOVE AL-UABON       TO  COMPANYA
01509              MOVE ER-2340        TO  EMI-ERROR
01510              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01511          ELSE
01512              PERFORM 7400-SEARCH-FOR-COMPANY THRU 7499-EXIT
01513              MOVE AL-UANON       TO  COMPANYA
01514      ELSE
01515          IF ADD-FUNCTION
01516              MOVE -1             TO  COMPANYL
01517              MOVE AL-UABON       TO  COMPANYA
01518              MOVE ER-2311        TO  EMI-ERROR
01519              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01520
01521      IF PERCNTL GREATER ZERO
01522          MOVE PERCNTI            TO  WS-CHECK-PERCENT
01523          IF EXCESS-LEVEL
01524              IF EXCESS-LEVEL-EXISTS
01525                  IF PI-LAST-LEVEL = PI-SUB
01526                      MOVE AL-UANON   TO  PERCNTA
01527                  ELSE
01528                      MOVE -1         TO  PERCNTL
01529                      MOVE AL-UABON   TO  PERCNTA
01530                      MOVE ER-2347    TO  EMI-ERROR
01531                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01532              ELSE
01533                  IF PI-LAST-LEVEL = PI-SUB
01534                      MOVE AL-UANON   TO  PERCNTA
01535                  ELSE
01536                      MOVE -1         TO  PERCNTL
01537                      MOVE AL-UABON   TO  PERCNTA
01538                      MOVE ER-2348    TO  EMI-ERROR
01539                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01540          ELSE
01541              IF NO-LEVEL
01542                  MOVE AL-UANON       TO  PERCNTA
01543              ELSE
01544                  MOVE -1             TO  PERCNTL
01545                  MOVE AL-UABON       TO  PERCNTA
01546                  MOVE ER-2316        TO  EMI-ERROR
01547                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01548      ELSE
01549          MOVE AL-UANON           TO  PERCNTA
01550          MOVE SPACE              TO  PERCNTO.
01551
01552      IF PI-CARRIER-SECURITY GREATER SPACE
01553          MOVE PI-CARRIER-SECURITY    TO  CARRIERI
01554          MOVE AL-SANON               TO  CARRIERA.
01555
01556      IF CARRIERL GREATER ZERO
01557          MOVE CARRIERI           TO  WS-CARRIER
01558          PERFORM 7550-CHECK-CARRIER THRU 7550-EXIT
01559          IF CARRIER-FOUND
01560              MOVE AL-UANON       TO  CARRIERA
01561          ELSE
01562              IF CARRIERI = SPACES
01563                  MOVE AL-UANON   TO  CARRIERA
01564              ELSE
01565                  MOVE -1         TO  CARRIERL
01566                  MOVE AL-UABON   TO  CARRIERA
01567                  MOVE ER-2208    TO  EMI-ERROR
01568                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01569
01570      MOVE ZEROS                  TO  DATE-TEST-AREA.
01571
01572 ***  Y2K PROJ 7744
01573      IF FDATEL GREATER ZERO
01574          MOVE FDATEI             TO  DEEDIT-FIELD
01575          PERFORM 7500-DEEDIT THRU 7500-EXIT
01576          IF DEEDIT-FIELD-V0 GREATER ZERO
01577              MOVE DEEDIT-FIELD-V0    TO  DC-GREG-DATE-1-MDY
01578              MOVE '4'                TO  DC-OPTION-CODE
01579              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01580              IF NO-CONVERSION-ERROR
01581                  MOVE AL-UANON       TO  FDATEA
01582                  MOVE DC-GREG-DATE-1-YMD   TO  DATE-TEST-AREA
01583                                                WS-LO-DATE
01584                  MOVE DC-ALPHA-CENTURY     TO  DATE-TEST-AREA(1:2)
01585                                                WS-LO-DATE(1:2)
01586                  MOVE DC-GREG-DATE-1-EDIT  TO FDATEO
01587              ELSE
01588                  MOVE -1         TO  FDATEL
01589                  MOVE AL-UABON   TO  FDATEA
01590                  MOVE ER-2312    TO  EMI-ERROR
01591                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01592              END-IF
01593          ELSE
01594              MOVE WS-LOW-DATE    TO  FDATEO
01595              MOVE ZEROS          TO  WS-LO-DATE
01596              MOVE AL-UANON       TO  FDATEA
01597          END-IF
01598      ELSE
01599          IF ADD-FUNCTION
01600              MOVE WS-LOW-DATE    TO  FDATEO
01601              MOVE ZEROS          TO  WS-LO-DATE
01602              MOVE AL-UANON       TO  FDATEA
01603          END-IF
01604      END-IF.
01605
01606      IF TDATEL GREATER ZERO
01607          MOVE TDATEI             TO  DEEDIT-FIELD
01608          PERFORM 7500-DEEDIT THRU 7500-EXIT
01609          IF DEEDIT-FIELD-V0 = 999999
01610              MOVE WS-HIGH-DATE   TO  TDATEO
01611              MOVE 99999999       TO  WS-HI-DATE
01612              MOVE AL-UANON       TO  TDATEA
01613          ELSE
01614              MOVE DEEDIT-FIELD-V0    TO  DC-GREG-DATE-1-MDY
01615              MOVE '4'                TO  DC-OPTION-CODE
01616              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01617              IF NO-CONVERSION-ERROR
01618                  MOVE DC-GREG-DATE-1-YMD TO WS-HOLD-PIC98-2
01619                  MOVE DC-ALPHA-CENTURY   TO WS-HOLD-PIC98-2(1:2)
01620                  IF WS-HOLD-PIC98-2 > DATE-TEST-AREA
01621                      MOVE AL-UANON            TO  TDATEA
01622                      MOVE DC-GREG-DATE-1-EDIT TO  TDATEO
01623                      MOVE DC-GREG-DATE-1-YMD  TO  WS-HI-DATE
01624                      MOVE DC-ALPHA-CENTURY    TO  WS-HI-DATE(1:2)
01625                  ELSE
01626                      MOVE -1         TO  TDATEL
01627                      MOVE AL-UABON   TO  TDATEA
01628                      MOVE ER-2339    TO  EMI-ERROR
01629                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01630                  END-IF
01631              ELSE
01632                  MOVE -1         TO  TDATEL
01633                  MOVE AL-UABON   TO  TDATEA
01634                  MOVE ER-2313    TO  EMI-ERROR
01635                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01636              END-IF
01637          END-IF
01638      ELSE
01639          IF ADD-FUNCTION
01640              MOVE WS-HIGH-DATE   TO  TDATEO
01641              MOVE 99999999       TO  WS-HI-DATE
01642              MOVE AL-UANON       TO  TDATEA
01643          END-IF
01644      END-IF.
01645 ***  Y2K PROJ 7744
01646
01647      MOVE ZEROS                  TO  WS-PREV-AGE
01648                                      WS-PREV-TRM
01649                                      WS-PREV-AMT.
01650
01651      IF LFLOAGEL GREATER ZERO
01652          IF LFLOAGEI NUMERIC
01653              MOVE AL-UANON       TO  LFLOAGEA
01654              MOVE LFLOAGEI       TO  WS-PREV-AGE
01655                                      WS-LFAGE-LO
01656          ELSE
01657              MOVE -1             TO  LFLOAGEL
01658              MOVE AL-UNBON       TO  LFLOAGEA
01659              MOVE ER-2317        TO  EMI-ERROR
01660              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01661      ELSE
01662          IF ADD-FUNCTION
01663              MOVE ZEROS          TO  LFLOAGEO
01664                                      WS-LFAGE-LO
01665              MOVE AL-UNNON       TO  LFLOAGEA.
01666
01667      IF LFHIAGEL GREATER ZERO
01668          IF LFHIAGEI NUMERIC
01669              IF LFHIAGEI LESS WS-PREV-AGE
01670                  MOVE -1         TO  LFHIAGEL
01671                  MOVE AL-UNBON   TO  LFHIAGEA
01672                  MOVE ER-2318    TO  EMI-ERROR
01673                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01674              ELSE
01675                  MOVE AL-UNNON   TO  LFHIAGEA
01676                  MOVE LFHIAGEI   TO  WS-LFAGE-HI
01677          ELSE
01678              MOVE -1             TO  LFHIAGEL
01679              MOVE AL-UNBON       TO  LFHIAGEA
01680              MOVE ER-2319        TO  EMI-ERROR
01681              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01682      ELSE
01683          IF ADD-FUNCTION
01684              MOVE 99             TO  LFHIAGEO
01685                                      WS-LFAGE-HI
01686              MOVE AL-UNNON       TO  LFHIAGEA.
01687
01688      IF LFLOTRML GREATER ZERO
01689          IF LFLOTRMI NUMERIC
01690              MOVE AL-UNNON       TO  LFLOTRMA
01691              MOVE LFLOTRMI       TO  WS-PREV-TRM
01692                                      WS-LFTRM-LO
01693          ELSE
01694              MOVE -1             TO  LFLOTRML
01695              MOVE AL-UNBON       TO  LFLOTRMA
01696              MOVE ER-2320        TO  EMI-ERROR
01697              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01698      ELSE
01699          IF ADD-FUNCTION
01700              MOVE ZEROS          TO  LFLOTRMO
01701                                      WS-LFTRM-LO
01702              MOVE AL-UNNON       TO  LFLOTRMA.
01703
01704      IF LFHITRML GREATER ZERO
01705          IF LFHITRMI NUMERIC
01706              IF LFHITRMI LESS WS-PREV-TRM
01707                  MOVE -1         TO  LFHITRML
01708                  MOVE AL-UABON   TO  LFHITRMA
01709                  MOVE ER-2321    TO  EMI-ERROR
01710                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01711              ELSE
01712                  MOVE AL-UNNON   TO  LFHITRMA
01713                  MOVE LFHITRMI   TO  WS-LFTRM-HI
01714          ELSE
01715              MOVE -1             TO  LFHITRML
01716              MOVE AL-UNBON       TO  LFHITRMA
01717              MOVE ER-2322        TO  EMI-ERROR
01718              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01719      ELSE
01720          IF ADD-FUNCTION
01721              MOVE 999            TO  LFHITRMO
01722                                      WS-LFTRM-HI
01723              MOVE AL-UNNON       TO  LFHITRMA.
01724
01725      IF LFLOBENL GREATER ZERO
01726          MOVE LFLOBENI           TO  DEEDIT-FIELD
01727          PERFORM 7500-DEEDIT THRU 7500-EXIT
01728          IF DEEDIT-FIELD-V0 NUMERIC
01729              MOVE DEEDIT-FIELD-V1    TO  WS-PREV-AMT
01730                                          LFLOBENO
01731                                          WS-LF-LIM-LO
01732              MOVE AL-UNNON           TO  LFLOBENA
01733          ELSE
01734              MOVE -1             TO  LFLOBENL
01735              MOVE AL-UNBON       TO  LFLOBENA
01736              MOVE ER-2323        TO  EMI-ERROR
01737              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01738      ELSE
01739          IF ADD-FUNCTION
01740              MOVE ZEROS          TO  LFLOBENO
01741                                      WS-LF-LIM-LO
01742              MOVE AL-UNNON       TO  LFLOBENA.
01743
01744      IF LFHIBENL GREATER ZERO
01745          MOVE LFHIBENI           TO  DEEDIT-FIELD
01746          PERFORM 7500-DEEDIT THRU 7500-EXIT
01747          IF DEEDIT-FIELD-V0 NUMERIC
01748              IF DEEDIT-FIELD-V1 LESS WS-PREV-AMT
01749                  MOVE -1         TO  LFHIBENL
01750                  MOVE AL-UNBON   TO  LFHIBENA
01751                  MOVE ER-2324    TO  EMI-ERROR
01752                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01753              ELSE
01754                  MOVE AL-UNNON   TO  LFHIBENA
01755                  MOVE DEEDIT-FIELD-V1    TO  LFHIBENO
01756                                              WS-LF-LIM-HI
01757          ELSE
01758              MOVE -1             TO  LFHIBENL
01759              MOVE AL-UNBON       TO  LFHIBENA
01760              MOVE ER-2325        TO  EMI-ERROR
01761              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01762      ELSE
01763          IF ADD-FUNCTION
01764              MOVE NINES          TO  LFHIBENO
01765                                      WS-LF-LIM-HI
01766              MOVE AL-UNNON       TO  LFHIBENA.
01767
01768      MOVE ZEROS                  TO  WS-PREV-AGE
01769                                      WS-PREV-TRM
01770                                      WS-PREV-AMT.
01771
01772      IF AHLOAGEL GREATER ZERO
01773          IF AHLOAGEI NUMERIC
01774              MOVE AL-UNNON       TO  AHLOAGEA
01775              MOVE AHLOAGEI       TO  WS-PREV-AGE
01776                                      WS-AHAGE-LO
01777          ELSE
01778              MOVE -1             TO  AHLOAGEL
01779              MOVE AL-UNBON       TO  AHLOAGEA
01780              MOVE ER-2317        TO  EMI-ERROR
01781              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01782      ELSE
01783          IF ADD-FUNCTION
01784              MOVE ZEROS          TO  AHLOAGEO
01785                                      WS-AHAGE-LO
01786              MOVE AL-UNNON       TO  AHLOAGEA.
01787
01788      IF AHHIAGEL GREATER ZERO
01789          IF AHHIAGEI NUMERIC
01790              IF AHHIAGEI LESS WS-PREV-AGE
01791                  MOVE -1         TO  AHHIAGEL
01792                  MOVE AL-UNBON   TO  AHHIAGEA
01793                  MOVE ER-2318    TO  EMI-ERROR
01794                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01795              ELSE
01796                  MOVE AL-UNNON   TO  AHHIAGEA
01797                  MOVE AHHIAGEI   TO  WS-AHAGE-HI
01798          ELSE
01799              MOVE -1             TO  AHHIAGEL
01800              MOVE AL-UNBON       TO  AHHIAGEA
01801              MOVE ER-2319        TO  EMI-ERROR
01802              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01803      ELSE
01804          IF ADD-FUNCTION
01805              MOVE 99             TO  AHHIAGEO
01806                                      WS-AHAGE-HI
01807              MOVE AL-UNNON       TO  AHHIAGEA.
01808
01809      IF AHLOTRML GREATER ZERO
01810          IF AHLOTRMI NUMERIC
01811              MOVE AL-UNNON       TO  AHLOTRMA
01812              MOVE AHLOTRMI       TO  WS-PREV-TRM
01813                                      WS-AHTRM-LO
01814          ELSE
01815              MOVE -1             TO  AHLOTRML
01816              MOVE AL-UNBON       TO  AHLOTRMA
01817              MOVE ER-2320        TO  EMI-ERROR
01818              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01819      ELSE
01820          IF ADD-FUNCTION
01821              MOVE ZEROS          TO  AHLOTRMO
01822                                      WS-AHTRM-LO
01823              MOVE AL-UNNON       TO  AHLOTRMA.
01824
01825      IF AHHITRML GREATER ZERO
01826          IF AHHITRMI NUMERIC
01827              IF AHHITRMI LESS WS-PREV-TRM
01828                  MOVE -1         TO  AHHITRML
01829                  MOVE AL-UNBON   TO  AHHITRMA
01830                  MOVE ER-2321    TO  EMI-ERROR
01831                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01832              ELSE
01833                  MOVE AL-UNNON   TO  AHHITRMA
01834                  MOVE AHHITRMI   TO  WS-AHTRM-HI
01835          ELSE
01836              MOVE -1             TO  AHHITRML
01837              MOVE AL-UNBON       TO  AHHITRMA
01838              MOVE ER-2322        TO  EMI-ERROR
01839              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01840      ELSE
01841          IF ADD-FUNCTION
01842              MOVE 999            TO  AHHITRMO
01843                                      WS-AHTRM-HI
01844              MOVE AL-UNNON       TO  AHHITRMA.
01845
01846      IF LFBENL GREATER ZERO AND
01847         LFBENI NUMERIC AND
01848         LFBENI GREATER +1
01849         MOVE -1                  TO  LFBENL
01850         MOVE AL-UNBON            TO  LFBENA
01851         MOVE ER-2356             TO  EMI-ERROR
01852         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01853
01854      IF AHBENL GREATER ZERO AND
01855         AHBENI NUMERIC AND
01856         AHBENI GREATER +1
01857         MOVE -1                  TO  AHBENL
01858         MOVE AL-UNBON            TO  AHBENA
01859         MOVE ER-2356             TO  EMI-ERROR
01860         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01861
01862      IF AHLOBENL GREATER ZERO
01863          MOVE AHLOBENI           TO  DEEDIT-FIELD
01864          PERFORM 7500-DEEDIT THRU 7500-EXIT
01865          IF DEEDIT-FIELD-V0 NUMERIC
01866              MOVE AL-UNNON       TO  AHLOBENA
01867              MOVE DEEDIT-FIELD-V1    TO  WS-PREV-AMT
01868                                          AHLOBENO
01869                                          WS-AHBEN-LIM-LO
01870          ELSE
01871              MOVE -1             TO  AHLOBENL
01872              MOVE AL-UNBON       TO  AHLOBENA
01873              MOVE ER-2323        TO  EMI-ERROR
01874              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01875      ELSE
01876          IF ADD-FUNCTION
01877              MOVE ZEROS          TO  AHLOBENO
01878                                      WS-AHBEN-LIM-LO
01879              MOVE AL-UNNON       TO  AHLOBENA.
01880
01881      IF AHHIBENL GREATER ZERO
01882          MOVE AHHIBENI           TO  DEEDIT-FIELD
01883          PERFORM 7500-DEEDIT THRU 7500-EXIT
01884          IF DEEDIT-FIELD-V0 NUMERIC
01885              IF DEEDIT-FIELD-V1 LESS WS-PREV-AMT
01886                  MOVE -1         TO  AHHIBENL
01887                  MOVE AL-UNBON   TO  AHHIBENA
01888                  MOVE ER-2324    TO  EMI-ERROR
01889                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01890              ELSE
01891                  MOVE AL-UNNON   TO  AHHIBENA
01892                  MOVE DEEDIT-FIELD-V1    TO  AHHIBENO
01893                                              WS-AHBEN-LIM-HI
01894          ELSE
01895              MOVE -1             TO  AHHIBENL
01896              MOVE AL-UNBON       TO  AHHIBENA
01897              MOVE ER-2325        TO  EMI-ERROR
01898              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01899      ELSE
01900          IF ADD-FUNCTION
01901              MOVE NINES          TO  AHHIBENO
01902                                      WS-AHBEN-LIM-HI
01903              MOVE AL-UNNON       TO  AHHIBENA.
01904
01905      MOVE ZEROS                  TO  WS-PREV-AGE
01906                                      WS-PREV-TRM
01907                                      WS-PREV-AMT.
01908
01909      IF AMLOBENL GREATER ZERO
01910          MOVE AMLOBENI           TO  DEEDIT-FIELD
01911          PERFORM 7500-DEEDIT THRU 7500-EXIT
01912          IF DEEDIT-FIELD-V0 NUMERIC
01913              MOVE AL-UNNON       TO  AMLOBENA
01914              MOVE DEEDIT-FIELD-V1    TO  WS-PREV-AMT
01915                                          AMLOBENO
01916                                          WS-AHMOA-LIM-LO
01917          ELSE
01918              MOVE -1             TO  AMLOBENL
01919              MOVE AL-UNBON       TO  AMLOBENA
01920              MOVE ER-2323        TO  EMI-ERROR
01921              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01922      ELSE
01923          IF ADD-FUNCTION
01924              MOVE ZEROS          TO  AMLOBENO
01925                                      WS-AHMOA-LIM-LO
01926              MOVE AL-UNNON       TO  AMLOBENA.
01927
01928      IF AMHIBENL GREATER ZERO
01929          MOVE AMHIBENI           TO  DEEDIT-FIELD
01930          PERFORM 7500-DEEDIT THRU 7500-EXIT
01931          IF DEEDIT-FIELD-V0 NUMERIC
01932              IF DEEDIT-FIELD-V1 LESS WS-PREV-AMT
01933                  MOVE -1         TO  AMHIBENL
01934                  MOVE AL-UNBON   TO  AMHIBENA
01935                  MOVE ER-2324    TO  EMI-ERROR
01936                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01937              ELSE
01938                  MOVE AL-UNNON   TO  AMHIBENA
01939                  MOVE DEEDIT-FIELD-V1 TO AMHIBENO
01940                                          WS-AHMOA-LIM-HI
01941          ELSE
01942              MOVE -1             TO  AMHIBENL
01943              MOVE AL-UNBON       TO  AMHIBENA
01944              MOVE ER-2325        TO  EMI-ERROR
01945              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01946      ELSE
01947          IF ADD-FUNCTION
01948              MOVE NINES          TO  AMHIBENO
01949                                      WS-AHMOA-LIM-HI
01950              MOVE AL-UNNON       TO  AMHIBENA.
01951
01952      IF LFQTEDL GREATER ZERO
01953          MOVE LFQTEDI            TO  WS-CHECK-QTED
01954          IF VALID-QTED-CALC
01955              MOVE AL-UANON       TO  LFQTEDA
01956          ELSE
01957              MOVE -1             TO  LFQTEDL
01958              MOVE AL-UABON       TO  LFQTEDA
01959              MOVE ER-2333        TO  EMI-ERROR
01960              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01961      ELSE
01962          IF ADD-FUNCTION
01963              MOVE SPACE          TO  LFQTEDO
01964              MOVE AL-UANON       TO  LFQTEDA.
01965
01966      IF LIFEBENL GREATER ZERO
01967          MOVE LIFEBENI           TO  WS-CHECK-BEN-CODE
01968          IF VALID-BEN-CODE
01969              MOVE AL-UANON       TO  LIFEBENA
01970          ELSE
01971              MOVE -1             TO  LIFEBENL
01972              MOVE AL-UABON       TO  LIFEBENA
01973              MOVE ER-2326        TO  EMI-ERROR
01974              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01975      ELSE
01976          IF ADD-FUNCTION
01977              MOVE SPACE          TO  LIFEBENO
01978              MOVE AL-UABON       TO  LIFEBENA.
01979
01980      IF LFINTRL GREATER ZERO
01981          MOVE LFINTRI            TO  WS-CHECK-INTR
01982          IF VALID-INTR-CODE
01983              MOVE AL-UANON       TO  LFINTRA
01984          ELSE
01985              MOVE -1             TO  LFINTRL
01986              MOVE AL-UABON       TO  LFINTRA
01987              MOVE ER-2327        TO  EMI-ERROR
01988              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01989      ELSE
01990          IF ADD-FUNCTION
01991              MOVE SPACE          TO  LFINTRO
01992              MOVE AL-UABON       TO  LFINTRA.
01993
01994      IF LFREML GREATER ZERO
01995          MOVE LFREMI             TO  WS-CHECK-REM
01996          IF VALID-REM-SW
01997              MOVE AL-UANON       TO  LFREMA
01998          ELSE
01999          IF PI-COMPANY-ID EQUAL 'NSL' AND
02000             WS-CHECK-REM EQUAL 'I'
02001              MOVE AL-UANON       TO  LFREMA
02002          ELSE
02003              MOVE -1             TO  LFREML
02004              MOVE AL-UABON       TO  LFREMA
02005              MOVE ER-2334        TO  EMI-ERROR
02006              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02007      ELSE
02008          IF ADD-FUNCTION
02009              MOVE SPACE          TO  LFREMO
02010              MOVE AL-UANON       TO  LFREMA.
02011
02012      IF LFSTATEL GREATER ZERO
02013          MOVE SPACES             TO  WS-ACCESS
02014          MOVE LFSTATEI           TO  WS-STATE
02015          PERFORM 7050-CHECK-STATE THRU 7050-EXIT
02016          IF STATE-FOUND
02017              MOVE AL-UANON       TO  LFSTATEA
02018          ELSE
02019              IF LFSTATEI = SPACES
02020                  MOVE AL-UANON   TO  LFSTATEA
02021              ELSE
02022                  MOVE -1         TO  LFSTATEL
02023                  MOVE AL-UABON   TO  LFSTATEA
02024                  MOVE ER-2335    TO  EMI-ERROR
02025                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02026
02027      IF LFBENL GREATER ZERO
02028          IF LFBENI NUMERIC
02029              MOVE AL-UNNON       TO  LFBENA
02030          ELSE
02031              MOVE -1             TO  LFBENL
02032              MOVE AL-UNBON       TO  LFBENA
02033              MOVE ER-2331        TO  EMI-ERROR
02034              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02035      ELSE
02036          IF ADD-FUNCTION
02037              MOVE ZEROS          TO  LFBENO
02038              MOVE AL-UNNON       TO  LFBENA.
02039
02040      MOVE ZEROS                  TO  WS-PREV-AMT.
02041
02042      IF LFLOAMTL GREATER ZERO
02043          MOVE LFLOAMTI           TO  DEEDIT-FIELD
02044          PERFORM 7500-DEEDIT THRU 7500-EXIT
02045          IF DEEDIT-FIELD-V0 NUMERIC
02046              MOVE AL-UNNON       TO  LFLOAMTA
02047              MOVE DEEDIT-FIELD-V1    TO  WS-PREV-AMT
02048                                          LFLOAMTO
02049                                          WS-LF-LO
02050          ELSE
02051              MOVE -1             TO  LFLOAMTL
02052              MOVE AL-UNBON       TO  LFLOAMTA
02053              MOVE ER-2328        TO  EMI-ERROR
02054              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02055      ELSE
02056          IF ADD-FUNCTION
02057              MOVE ZEROS          TO  LFLOAMTO
02058                                      WS-LF-LO
02059              MOVE AL-UNNON       TO  LFLOAMTA.
02060
02061      IF LFHIAMTL GREATER ZERO
02062          MOVE LFHIAMTI           TO  DEEDIT-FIELD
02063          PERFORM 7500-DEEDIT THRU 7500-EXIT
02064          IF DEEDIT-FIELD-V0 NUMERIC
02065              IF DEEDIT-FIELD-V1 LESS WS-PREV-AMT
02066                  MOVE -1         TO  LFHIAMTL
02067                  MOVE AL-UNBON   TO  LFHIAMTA
02068                  MOVE ER-2329    TO  EMI-ERROR
02069                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02070              ELSE
02071                  MOVE AL-UNNON   TO  LFHIAMTA
02072                  MOVE DEEDIT-FIELD-V1 TO LFHIAMTO
02073                                          WS-LF-HI
02074          ELSE
02075              MOVE -1             TO  LFHIAMTL
02076              MOVE AL-UNBON       TO  LFHIAMTA
02077              MOVE ER-2330        TO  EMI-ERROR
02078              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02079      ELSE
02080          IF ADD-FUNCTION
02081              MOVE NINES          TO  LFHIAMTO
02082                                      WS-LF-HI
02083              MOVE AL-UNNON       TO  LFHIAMTA.
02084
02085      IF AHQTEDL GREATER ZERO
02086          MOVE AHQTEDI            TO  WS-CHECK-QTED
02087          IF VALID-QTED-CALC
02088              MOVE AL-UANON       TO  AHQTEDA
02089          ELSE
02090              MOVE -1             TO  AHQTEDL
02091              MOVE AL-UABON       TO  AHQTEDA
02092              MOVE ER-2333        TO  EMI-ERROR
02093              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02094      ELSE
02095          IF ADD-FUNCTION
02096              MOVE SPACE          TO  AHQTEDO
02097              MOVE AL-UANON       TO  AHQTEDA.
02098
02099      IF AHTBENL GREATER ZERO
02100          MOVE AHTBENI            TO  WS-CHECK-BEN-CODE
02101          IF VALID-BEN-CODE
02102              MOVE AL-UANON       TO  AHTBENA
02103          ELSE
02104              MOVE -1             TO  AHTBENL
02105              MOVE AL-UABON       TO  AHTBENA
02106              MOVE ER-2326        TO  EMI-ERROR
02107              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02108      ELSE
02109          IF ADD-FUNCTION
02110              MOVE SPACE          TO  AHTBENO
02111              MOVE AL-UANON       TO  AHTBENA.
02112
02113      IF AHSTATEL GREATER ZERO
02114          MOVE SPACES             TO  WS-ACCESS
02115          MOVE AHSTATEI           TO  WS-STATE
02116          PERFORM 7050-CHECK-STATE THRU 7050-EXIT
02117          IF STATE-FOUND
02118              MOVE AL-UANON       TO  AHSTATEA
02119          ELSE
02120              IF AHSTATEI = SPACES
02121                  MOVE AL-UANON   TO  AHSTATEA
02122              ELSE
02123                  MOVE -1         TO  AHSTATEL
02124                  MOVE AL-UABON   TO  AHSTATEA
02125                  MOVE ER-2335    TO  EMI-ERROR
02126                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02127
02128      IF AHBENL GREATER ZERO
02129          IF AHBENI NUMERIC
02130              MOVE AL-UNNON       TO  AHBENA
02131          ELSE
02132              MOVE -1             TO  AHBENL
02133              MOVE AL-UNBON       TO  AHBENA
02134              MOVE ER-2331        TO  EMI-ERROR
02135              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02136      ELSE
02137          IF ADD-FUNCTION
02138              MOVE ZEROS          TO  AHBENO
02139              MOVE AL-UNNON       TO  AHBENA.
02140
02141      MOVE ZEROS                  TO  WS-PREV-AMT.
02142
02143      IF AHLOAMTL GREATER ZERO
02144          MOVE AHLOAMTI           TO  DEEDIT-FIELD
02145          PERFORM 7500-DEEDIT THRU 7500-EXIT
02146          IF DEEDIT-FIELD-V0 NUMERIC
02147              MOVE AL-UNNON       TO  AHLOAMTA
02148              MOVE DEEDIT-FIELD-V1 TO WS-PREV-AMT
02149                                      AHLOAMTO
02150                                      WS-AHBEN-LO
02151          ELSE
02152              MOVE -1             TO  AHLOAMTL
02153              MOVE AL-UNBON       TO  AHLOAMTA
02154              MOVE ER-2328        TO  EMI-ERROR
02155              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02156      ELSE
02157          IF ADD-FUNCTION
02158              MOVE ZEROS          TO  AHLOAMTO
02159                                      WS-AHBEN-LO
02160              MOVE AL-UNNON       TO  AHLOAMTA.
02161
02162      IF AHHIAMTL GREATER ZERO
02163          MOVE AHHIAMTI           TO  DEEDIT-FIELD
02164          PERFORM 7500-DEEDIT THRU 7500-EXIT
02165          IF DEEDIT-FIELD-V0 NUMERIC
02166              IF DEEDIT-FIELD-V1 LESS WS-PREV-AMT
02167                  MOVE -1         TO  AHHIAMTL
02168                  MOVE AL-UNBON   TO  AHHIAMTA
02169                  MOVE ER-2329    TO  EMI-ERROR
02170                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02171              ELSE
02172                  MOVE AL-UNNON   TO  AHHIAMTA
02173                  MOVE DEEDIT-FIELD-V1    TO  AHHIAMTO
02174                                              WS-AHBEN-HI
02175          ELSE
02176              MOVE -1             TO  AHHIAMTL
02177              MOVE AL-UNBON       TO  AHHIAMTA
02178              MOVE ER-2330        TO  EMI-ERROR
02179              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02180      ELSE
02181          IF ADD-FUNCTION
02182              MOVE NINES          TO  AHHIAMTO
02183                                      WS-AHBEN-HI
02184              MOVE AL-UNNON       TO  AHHIAMTA.
02185
02186
02187      MOVE ZEROS                  TO  WS-PREV-AMT.
02188
02189      IF AMLOAMTL GREATER ZERO
02190          MOVE AMLOAMTI           TO  DEEDIT-FIELD
02191          PERFORM 7500-DEEDIT THRU 7500-EXIT
02192          IF DEEDIT-FIELD-V0 NUMERIC
02193              MOVE AL-UNNON       TO  AMLOAMTA
02194              MOVE DEEDIT-FIELD-V1    TO  WS-PREV-AMT
02195                                          AMLOAMTO
02196                                          WS-AHMOA-LO
02197          ELSE
02198              MOVE -1             TO  AMLOAMTL
02199              MOVE AL-UNBON       TO  AMLOAMTA
02200              MOVE ER-2328        TO  EMI-ERROR
02201              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02202      ELSE
02203          IF ADD-FUNCTION
02204              MOVE ZEROS          TO  AMLOAMTO
02205                                      WS-AHMOA-LO
02206              MOVE AL-UNNON       TO  AMLOAMTA.
02207
02208      IF AMHIAMTL GREATER ZERO
02209          MOVE AMHIAMTI           TO  DEEDIT-FIELD
02210          PERFORM 7500-DEEDIT THRU 7500-EXIT
02211          IF DEEDIT-FIELD-V0 NUMERIC
02212              IF DEEDIT-FIELD-V1 LESS WS-PREV-AMT
02213                  MOVE -1         TO  AMHIAMTL
02214                  MOVE AL-UNBON   TO  AMHIAMTA
02215                  MOVE ER-2329    TO  EMI-ERROR
02216                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02217              ELSE
02218                  MOVE AL-UNNON   TO  AMHIAMTA
02219                  MOVE DEEDIT-FIELD-V1    TO  AMHIAMTO
02220                                              WS-AHMOA-HI
02221          ELSE
02222              MOVE -1             TO  AMHIAMTL
02223              MOVE AL-UNBON       TO  AMHIAMTA
02224              MOVE ER-2330        TO  EMI-ERROR
02225              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02226      ELSE
02227          IF ADD-FUNCTION
02228              MOVE NINES          TO  AMHIAMTO
02229                                      WS-AHMOA-HI
02230              MOVE AL-UNNON       TO  AMHIAMTA.
02231
02232      IF WS-LIFE-AGE-TRMS = ZEROS
02233          IF WS-LIFE-BEN-AMTS NOT = ZEROS
02234              MOVE -1             TO  LFLOAGEL
02235              MOVE ER-2112        TO  EMI-ERROR
02236              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02237              MOVE AL-UNBON       TO  LFLOAGEA  LFHIAGEA
02238                                      LFLOTRMA  LFHITRMA
02239                                      LFLOBENA  LFHIBENA
02240                                      LFLOAMTA  LFHIAMTA.
02241
02242      IF WS-AH-AGE-TRMS = ZEROS
02243          IF WS-AH-BEN-AMTS NOT = ZEROS
02244              MOVE -1             TO  AHLOAGEL
02245              MOVE ER-2112        TO  EMI-ERROR
02246              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02247              MOVE AL-UNBON       TO  AHLOAGEA  AHHIAGEA
02248                                      AHLOTRMA  AHHITRMA
02249                                      AHLOBENA  AHHIBENA
02250                                      AHLOAMTA  AHHIAMTA
02251                                      AMLOBENA  AMHIBENA
02252                                      AMLOAMTA  AMHIAMTA.
02253
02254  7000-EXIT.
02255      EXIT.
02256      EJECT
02257  7050-CHECK-STATE.
02258      MOVE SPACES                 TO  WS-STATE-FOUND-SW
02259                                      ELCNTL-KEY.
02260      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
02261      MOVE '3'                    TO  CNTL-REC-TYPE.
02262      MOVE WS-ACCESS              TO  CNTL-ACCESS.
02263      MOVE +0                     TO  CNTL-SEQ-NO.
02264
02265      
      * EXEC CICS HANDLE CONDITION
02266 *        NOTFND   (7050-EXIT)
02267 *    END-EXEC.
      *    MOVE '"$I                   ! & #00005264' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303035323634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02268
02269      
      * EXEC CICS READ
02270 *        DATASET   (CNTL-FILE-ID)
02271 *        SET       (ADDRESS OF CONTROL-FILE)
02272 *        RIDFLD    (ELCNTL-KEY)
02273 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005268' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02274
02275      MOVE 'Y'                    TO  WS-STATE-FOUND-SW.
02276
02277  7050-EXIT.
02278      EXIT.
02279      EJECT
02280  7100-PAGE-TABLE-FORWARD.
02281      MOVE SPACES                 TO  PI-ERREIN-EOF-SW.
02282
02283      IF TABLEL GREATER ZERO
02284          MOVE ZERO TO TALLY
02285          INSPECT TABLEI TALLYING TALLY FOR ALL SPACES
02286          IF TALLY GREATER ZERO
02287              MOVE -1             TO  TABLEL
02288              MOVE AL-UABON       TO  TABLEA
02289              MOVE ER-2341        TO  EMI-ERROR
02290              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02291              GO TO 8200-SEND-DATAONLY
02292          ELSE
02293              MOVE AL-UANON       TO  TABLEA
02294              MOVE TABLEI         TO  PI-ERR-TABLE
02295      ELSE
02296          MOVE LOW-VALUES         TO  PI-ERREIN-KEY
02297          MOVE  'Y'               TO  PI-FIRST-TIME-SW.
02298
02299      MOVE PI-COMPANY-CD          TO  PI-ERR-COMPANY-CD.
02300      MOVE 'A'                    TO  PI-ERR-CODE.
02301      MOVE PI-ERREIN-KEY          TO  WS-SAVE-KEY.
02302
02303      PERFORM 7800-START-BROWSE THRU 7800-EXIT.
02304
02305      
      * EXEC CICS HANDLE CONDITION
02306 *        ENDFILE  (7150-ENDFILE)
02307 *        NOTFND   (7175-NOTFOUND)
02308 *        END-EXEC.
      *    MOVE '"$''I                  ! '' #00005304' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303035333034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02309
02310  7110-READ-NEXT.
02311      PERFORM 7850-READNEXT THRU 7850-EXIT.
02312
02313      IF ERREIN-EOF
02314          GO TO 7150-ENDFILE.
02315
02316      IF PI-ERREIN-KEY = WS-SAVE-KEY
02317          GO TO 7110-READ-NEXT.
02318
02319      IF PI-CARRIER-SECURITY GREATER SPACE
02320          IF PI-CARRIER-SECURITY = RE-TABLE-CARRIER-SECURITY
02321              NEXT SENTENCE
02322          ELSE
02323              GO TO 7110-READ-NEXT.
02324
02325      MOVE RE-LAST-MAINT-USER     TO  PI-UPDATE-BY.
02326      MOVE RE-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.
02327
02328      MOVE PI-ERREIN-KEY          TO  PI-SAVE-ERREIN-KEY.
02329
02330      MOVE LOW-VALUES             TO  EL651AO.
02331
02332      MOVE +1                     TO  PI-SUB.
02333
02334      GO TO 5050-SET-UP-SCREEN.
02335
02336  7150-ENDFILE.
02337      IF BROWSE-STARTED
02338          PERFORM 7950-END-BROWSE THRU 7950-EXIT.
02339
02340      IF FIRST-TIME
02341          MOVE LOW-VALUES         TO  EL651AO
02342          MOVE ER-2346            TO  EMI-ERROR
02343          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02344          MOVE -1                 TO  MAINTYPL
02345          GO TO 8100-SEND-INITIAL-MAP
02346      ELSE
02347          MOVE ER-2067            TO  EMI-ERROR
02348          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02349          MOVE LOW-VALUES         TO  EL651AO
02350          GO TO 7100-PAGE-TABLE-FORWARD.
02351
02352  7175-NOTFOUND.
02353      IF BROWSE-STARTED
02354          PERFORM 7950-END-BROWSE THRU 7950-EXIT.
02355
02356      GO TO 8880-NOT-FOUND.
02357
02358  7199-EXIT.
02359      EXIT.
02360      EJECT
02361  7200-PAGE-TABLE-BACKWARD.
02362      MOVE SPACES                 TO  PI-ERREIN-EOF-SW.
02363
02364      IF TABLEL GREATER ZERO
02365          MOVE ZERO TO TALLY
02366          INSPECT TABLEI TALLYING TALLY FOR ALL SPACES
02367          IF TALLY GREATER ZERO
02368              MOVE -1             TO  TABLEL
02369              MOVE AL-UABON       TO  TABLEA
02370              MOVE ER-2341        TO  EMI-ERROR
02371              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02372              GO TO 8200-SEND-DATAONLY
02373          ELSE
02374              MOVE AL-UANON       TO  TABLEA
02375              MOVE TABLEI         TO  PI-ERR-TABLE
02376              MOVE LOW-VALUES     TO  PI-ERR-TABLE-SUB
02377      ELSE
02378          GO TO 7275-NOTFOUND.
02379
02380      MOVE PI-COMPANY-CD          TO  PI-ERR-COMPANY-CD.
02381      MOVE 'A'                    TO  PI-ERR-CODE.
02382      MOVE PI-ERREIN-KEY          TO  WS-SAVE-KEY.
02383
02384      PERFORM 7800-START-BROWSE THRU 7800-EXIT.
02385
02386      
      * EXEC CICS HANDLE CONDITION
02387 *        ENDFILE  (7250-ENDFILE)
02388 *        NOTFND   (7275-NOTFOUND)
02389 *    END-EXEC.
      *    MOVE '"$''I                  ! ( #00005385' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303035333835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02390
02391      PERFORM 7850-READNEXT THRU 7850-EXIT.
02392
02393      IF ERREIN-EOF
02394          GO TO 7250-ENDFILE.
02395
02396  7210-READ-PREV.
02397      PERFORM 7900-READPREV THRU 7900-EXIT.
02398
02399      IF ERREIN-EOF
02400          GO TO 7250-ENDFILE.
02401
02402      IF PI-ERREIN-KEY = WS-SAVE-KEY
02403          GO TO 7210-READ-PREV.
02404
02405      IF PI-CARRIER-SECURITY GREATER SPACE
02406         IF PI-CARRIER-SECURITY NOT = RE-TABLE-CARRIER-SECURITY
02407              GO TO 7210-READ-PREV.
02408
02409      MOVE RE-LAST-MAINT-USER     TO  PI-UPDATE-BY.
02410      MOVE RE-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.
02411
02412      MOVE PI-ERREIN-KEY          TO  PI-SAVE-ERREIN-KEY.
02413
02414      MOVE LOW-VALUES             TO  EL651AO.
02415
02416      MOVE +1                     TO  PI-SUB.
02417
02418      GO TO 5050-SET-UP-SCREEN.
02419
02420  7250-ENDFILE.
02421      IF BROWSE-STARTED
02422          PERFORM 7950-END-BROWSE THRU 7950-EXIT.
02423
02424      MOVE ER-2067                TO  EMI-ERROR
02425      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02426      MOVE LOW-VALUES             TO  EL651AO
02427      GO TO 7100-PAGE-TABLE-FORWARD.
02428
02429  7275-NOTFOUND.
02430      IF BROWSE-STARTED
02431          PERFORM 7950-END-BROWSE THRU 7950-EXIT.
02432
02433      GO TO 8880-NOT-FOUND.
02434
02435  7299-EXIT.
02436      EXIT.
02437      EJECT
02438  7300-PAGE-LEVEL.
02439      MOVE SPACES                 TO  PI-ERREIN-EOF-SW.
02440
02441      IF TABLEL GREATER ZERO
02442          MOVE ZERO TO TALLY
02443          INSPECT TABLEI TALLYING TALLY FOR ALL SPACES
02444          IF TALLY GREATER ZERO
02445              MOVE -1             TO  TABLEL
02446              MOVE AL-UABON       TO  TABLEA
02447              MOVE ER-2341        TO  EMI-ERROR
02448              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02449              GO TO 8200-SEND-DATAONLY
02450          ELSE
02451              MOVE AL-UANON       TO  TABLEA
02452              MOVE TABLEI         TO  PI-ERR-TABLE
02453              MOVE LOW-VALUES     TO  PI-ERR-TABLE-SUB
02454      ELSE
02455          MOVE -1                 TO  TABLEL
02456          MOVE AL-UABON           TO  TABLEA
02457          MOVE ER-2140            TO  EMI-ERROR
02458          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02459          GO TO 8200-SEND-DATAONLY.
02460
02461      MOVE PI-COMPANY-CD          TO  PI-ERR-COMPANY-CD.
02462      MOVE 'A'                    TO  PI-ERR-CODE.
02463
02464      PERFORM 7800-START-BROWSE THRU 7800-EXIT.
02465
02466      
      * EXEC CICS HANDLE CONDITION
02467 *        ENDFILE  (7350-ENDFILE)
02468 *        NOTFND   (7375-NOTFOUND)
02469 *    END-EXEC.
      *    MOVE '"$''I                  ! ) #00005465' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303035343635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02470
02471      PERFORM 7850-READNEXT THRU 7850-EXIT.
02472
02473      IF ERREIN-EOF
02474          IF BROWSE-STARTED
02475              PERFORM 7950-END-BROWSE THRU 7950-EXIT
02476              GO TO 7100-PAGE-TABLE-FORWARD.
02477
02478      MOVE RE-LAST-MAINT-USER     TO  PI-UPDATE-BY.
02479      MOVE RE-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.
02480
02481      MOVE PI-ERREIN-KEY          TO  PI-SAVE-ERREIN-KEY.
02482
02483      IF LEVELL GREATER ZERO
02484          MOVE LEVELI             TO  WS-CHECK-LEVEL
02485          IF VALID-LEVEL
02486              MOVE LEVELI         TO  PI-SUB
02487              IF RE-REI-COMP (PI-SUB) = SPACES
02488                  MOVE +1         TO  PI-SUB
02489                  MOVE ER-2338    TO  EMI-ERROR
02490                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02491                  GO TO 5000-BUILD-INITIAL-SCREEN
02492              ELSE
02493                  MOVE LOW-VALUES TO  EL651AO
02494                  GO TO 5050-SET-UP-SCREEN
02495          ELSE
02496              MOVE +1             TO  PI-SUB
02497              MOVE ER-2338        TO  EMI-ERROR
02498              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02499              GO TO 5000-BUILD-INITIAL-SCREEN.
02500
02501      MOVE LOW-VALUES             TO  EL651AO.
02502
02503      IF PAGE-LEVEL-FORWARD
02504          ADD +1                  TO  PI-SUB
02505      ELSE
02506          SUBTRACT +1 FROM PI-SUB.
02507
02508      IF PI-SUB GREATER +30 OR
02509         RE-REI-COMP (PI-SUB) = SPACES
02510             MOVE +1              TO  PI-SUB
02511             MOVE ER-2338         TO  EMI-ERROR
02512             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02513             GO TO 5000-BUILD-INITIAL-SCREEN
02514      ELSE
02515          IF PI-SUB LESS +1
02516              MOVE PI-LAST-LEVEL  TO  PI-SUB
02517              MOVE ER-0592        TO  EMI-ERROR
02518              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02519
02520      GO TO 5050-SET-UP-SCREEN.
02521
02522  7350-ENDFILE.
02523      IF BROWSE-STARTED
02524          PERFORM 7950-END-BROWSE THRU 7950-EXIT.
02525
02526      MOVE ER-2067                TO  EMI-ERROR.
02527      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02528      MOVE LOW-VALUES             TO  EL651AO.
02529      GO TO 7100-PAGE-TABLE-FORWARD.
02530
02531  7375-NOTFOUND.
02532      IF BROWSE-STARTED
02533          PERFORM 7950-END-BROWSE THRU 7950-EXIT.
02534
02535      GO TO 8880-NOT-FOUND.
02536
02537  7399-EXIT.
02538      EXIT.
02539      EJECT
02540
02541  7400-SEARCH-FOR-COMPANY.
02542      MOVE PI-ERREIN-KEY          TO  PI-SAVE-ERREIN-KEY.
02543      MOVE COMPANYI               TO  PI-ERR-TABLE.
02544      MOVE COMPSUBI               TO  PI-ERR-TABLE-SUB.
02545      MOVE 'B'                    TO  PI-ERR-CODE.
02546
02547      
      * EXEC CICS HANDLE CONDITION
02548 *        NOTOPEN  (9990-ABEND)
02549 *        NOTFND   (7425-NOT-FOUND)
02550 *    END-EXEC.
      *    MOVE '"$JI                  ! * #00005546' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303035353436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02551
02552      PERFORM 7650-READ-ERREIN THRU 7650-EXIT.
02553
02554      MOVE PI-SAVE-ERREIN-KEY     TO  PI-ERREIN-KEY.
02555
02556      GO TO 7499-EXIT.
02557
02558  7425-NOT-FOUND.
02559      PERFORM 7700-ERREIN-GETMAIN THRU 7700-EXIT.
02560      MOVE '1'                    TO  WS-GETMAIN-SW.
02561
02562      MOVE ZEROS                  TO  RE-LF-FEE
02563                                      RE-AH-FEE
02564                                      RE-AH-PR-PCT
02565                                      RE-AH-78-PCT
02566                                      RE-LF-PR-PCT
02567                                      RE-LF-78-PCT
02568 *                                    RE-PR-PCT
02569 *                                    RE-78-PCT
02570                                      RE-LF-IBNR-PCT
02571                                      RE-AH-IBNR-PCT
02572                                      RE-CLM-INCURRED-LIM
02573                                      RE-LF-CLM-PCT
02574                                      RE-AH-CLM-PCT
02575                                      RE-LF-CLM-MAX
02576                                      RE-AH-CLM-MAX
02577                                      RE-LF-CEDING-FEE-BRACKETS
02578                                      RE-AH-CEDING-FEE-BRACKETS
CIDMOD                                     RE-EARNING-START-DT
CIDMOD                                     RE-EARNING-STOP-DT
CIDMOD                                     RE-CUSTODIAL-BAL
02579                                      RE-LAST-MAINT-HHMMSS.
02580
02581      MOVE SPACES                 TO  RE-LF-FEE-METHOD
02582                                      RE-AH-FEE-METHOD
02583                                      RE-LF-FEE-BASIS
02584                                      RE-AH-FEE-BASIS.
02585
02586      MOVE COMPANYI               TO  RE-COMP-PRIME
02587                                      PI-ERR-TABLE.
02588      MOVE COMPSUBI               TO  RE-COMP-SUB
02589                                      PI-ERR-TABLE-SUB.
02590      MOVE 'B'                    TO  RE-CODE
02591                                      PI-ERR-CODE.
02592      MOVE PI-COMPANY-CD          TO  PI-ERR-COMPANY-CD
02593                                      RE-COMPANY-CD.
02594
02595      MOVE PI-PROCESSOR-ID        TO  RE-LAST-MAINT-USER.
02596      MOVE EIBTIME                TO  RE-LAST-MAINT-HHMMSS.
02597      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
02598
02599      MOVE '5'                    TO  DC-OPTION-CODE.
02600      MOVE LINK-ELDATCV           TO  PGM-NAME.
02601
02602      
      * EXEC CICS LINK
02603 *        PROGRAM   (PGM-NAME)
02604 *        COMMAREA  (DATE-CONVERSION-DATA)
02605 *        LENGTH    (DC-COMM-LENGTH)
02606 *    END-EXEC.
      *    MOVE '."C                   ''   #00005604' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02607
02608      MOVE DC-BIN-DATE-1          TO  RE-LAST-MAINT-DT.
02609      MOVE PI-COMPANY-CD          TO  RE-COMPANY-CD.
02610      MOVE 'RE'                   TO  RE-RECORD-ID.
02611 *    MOVE REIN-FILE-ID           TO  FILE-ID.
02612 *    MOVE 'A'                    TO  JP-RECORD-AREA
02613
02614      
      * EXEC CICS WRITE
02615 *        DATASET (REIN-FILE-ID)
02616 *        FROM    (REINSURANCE-RECORD)
02617 *        RIDFLD  (RE-CONTROL-PRIMARY)
02618 *    END-EXEC.
           MOVE LENGTH OF
            REINSURANCE-RECORD
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00005616' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 REINSURANCE-RECORD, 
                 DFHEIV11, 
                 RE-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02619
02620 *    MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA.
02621
02622 *    PERFORM 8400-LOG-JOURNAL-RECORD.
02623
02624      MOVE PI-SAVE-ERREIN-KEY     TO  PI-ERREIN-KEY.
02625      MOVE 'Y'                    TO  PI-COMPANY-ADD-SW.
02626
02627  7499-EXIT.
02628      EXIT.
02629      EJECT
02630  7500-DEEDIT.
02631      
      * EXEC CICS BIF
02632 *         DEEDIT
02633 *         FIELD  (DEEDIT-FIELD)
02634 *         LENGTH (15)
02635 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005633' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02636
02637  7500-EXIT.
02638      EXIT.
02639      EJECT
02640  7550-CHECK-CARRIER.
02641      MOVE SPACES                 TO  WS-CARRIER-FOUND-SW
02642                                      ELCNTL-KEY.
02643      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
02644      MOVE '6'                    TO  CNTL-REC-TYPE.
02645      MOVE CARRIER-ACCESS         TO  CNTL-ACCESS.
02646      MOVE +0                     TO  CNTL-SEQ-NO.
02647
02648      
      * EXEC CICS HANDLE CONDITION
02649 *        NOTFND   (7550-EXIT)
02650 *    END-EXEC.
      *    MOVE '"$I                   ! + #00005650' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303035363530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02651
02652      
      * EXEC CICS READ
02653 *        DATASET   (CNTL-FILE-ID)
02654 *        SET       (ADDRESS OF CONTROL-FILE)
02655 *        RIDFLD    (ELCNTL-KEY)
02656 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005654' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02657
02658      MOVE 'Y'                    TO  WS-CARRIER-FOUND-SW.
02659
02660  7550-EXIT.
02661      EXIT.
02662      EJECT
02663  7650-READ-ERREIN.
02664      MOVE PI-COMPANY-CD          TO  PI-ERR-COMPANY-CD.
02665
02666      
      * EXEC CICS READ
02667 *         DATASET  (REIN-FILE-ID)
02668 *         SET      (ADDRESS OF REINSURANCE-RECORD)
02669 *         RIDFLD   (PI-ERREIN-KEY)
02670 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005668' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERREIN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF REINSURANCE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02671
02672      CONTINUE.
02673
02674  7650-EXIT.
02675      EXIT.
02676      EJECT
02677  7700-ERREIN-GETMAIN.
02678      
      * EXEC CICS GETMAIN
02679 *         SET     (ADDRESS OF REINSURANCE-RECORD)
02680 *         LENGTH  (ERREIN-LENGTH)
02681 *         INITIMG (GETMAIN-SPACE)
02682 *    END-EXEC.
      *    MOVE ',"IL                  $   #00005680' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERREIN-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF REINSURANCE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02683
02684      CONTINUE.
02685
02686  7700-EXIT.
02687      EXIT.
02688      EJECT
02689  7750-READ-ERREIN-UPDATE.
02690      MOVE PI-COMPANY-CD          TO  PI-ERR-COMPANY-CD.
02691      MOVE 'A'                    TO  PI-ERR-CODE.
02692
02693      
      * EXEC CICS READ
02694 *         DATASET  (REIN-FILE-ID)
02695 *         SET      (ADDRESS OF REINSURANCE-RECORD)
02696 *         RIDFLD   (PI-ERREIN-KEY)
02697 *         UPDATE
02698 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005695' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERREIN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF REINSURANCE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02699
02700      CONTINUE.
02701
02702
02703  7750-EXIT.
02704      EXIT.
02705      EJECT
02706  7800-START-BROWSE.
02707      
      * EXEC CICS STARTBR
02708 *         DATASET  (REIN-FILE-ID)
02709 *         RIDFLD   (PI-ERREIN-KEY)
02710 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005709' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 PI-ERREIN-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02711
02712      MOVE 'Y'                    TO  PI-BROWSE-SW.
02713
02714  7800-EXIT.
02715      EXIT.
02716      EJECT
02717  7850-READNEXT.
02718      
      * EXEC CICS READNEXT
02719 *         DATASET  (REIN-FILE-ID)
02720 *         SET      (ADDRESS OF REINSURANCE-RECORD)
02721 *         RIDFLD   (PI-ERREIN-KEY)
02722 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005720' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERREIN-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF REINSURANCE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02723
02724      CONTINUE.
02725
02726      IF PI-COMPANY-CD NOT = RE-COMPANY-CD OR
02727         PI-ERR-CODE NOT = 'A'
02728          MOVE LOW-VALUES         TO  EL651AO
02729          MOVE 'Y'                TO  PI-ERREIN-EOF-SW
02730          IF FIRST-TIME
02731              MOVE ER-2346        TO  EMI-ERROR
02732          ELSE
02733              MOVE ER-2067        TO  EMI-ERROR
02734      ELSE
02735          GO TO 7850-EXIT.
02736
02737      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02738
02739  7850-EXIT.
02740      EXIT.
02741      EJECT
02742  7900-READPREV.
02743      
      * EXEC CICS READPREV
02744 *         DATASET  (REIN-FILE-ID)
02745 *         SET      (ADDRESS OF REINSURANCE-RECORD)
02746 *         RIDFLD   (PI-ERREIN-KEY)
02747 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00005745' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERREIN-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF REINSURANCE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02748
02749      CONTINUE.
02750
02751      IF PI-COMPANY-CD NOT = RE-COMPANY-CD OR
02752         PI-ERR-CODE NOT = 'A'
02753          MOVE LOW-VALUES         TO  EL651AO
02754          MOVE 'Y'                TO  PI-ERREIN-EOF-SW
02755          IF FIRST-TIME
02756              MOVE ER-2346        TO  EMI-ERROR
02757              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02758          ELSE
02759              MOVE ER-2067        TO  EMI-ERROR
02760              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02761
02762  7900-EXIT.
02763      EXIT.
02764      EJECT
02765  7950-END-BROWSE.
02766      
      * EXEC CICS ENDBR
02767 *         DATASET  (REIN-FILE-ID)
02768 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005768' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02769
02770      MOVE SPACE                  TO  PI-BROWSE-SW.
02771
02772  7950-EXIT.
02773      EXIT.
02774      EJECT
02775  8000-UPDATE-MAINT-DATE.
02776      MOVE SPACES                 TO  ELCNTL-KEY.
02777
02778      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
02779      MOVE '1'                    TO  CNTL-REC-TYPE.
02780      MOVE +0                     TO  CNTL-SEQ-NO.
02781
02782      
      * EXEC CICS HANDLE CONDITION
02783 *        NOTFND   (8000-EXIT)
02784 *    END-EXEC.
      *    MOVE '"$I                   ! , #00005784' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303035373834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02785
02786      
      * EXEC CICS READ
02787 *        UPDATE
02788 *        DATASET   (CNTL-FILE-ID)
02789 *        SET       (ADDRESS OF CONTROL-FILE)
02790 *        RIDFLD    (ELCNTL-KEY)
02791 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005788' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02792
02793      CONTINUE.
02794
02795      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
02796      MOVE 'B'                    TO  JP-RECORD-TYPE.
02797      MOVE CNTL-FILE-ID           TO  FILE-ID.
02798      MOVE ELCNTL-LENGTH          TO  FILE-LENGTH.
02799      PERFORM 8400-LOG-JOURNAL-RECORD.
02800
02801      MOVE BIN-CURRENT-SAVE       TO  CF-REINSURANCE-TAB-MAINT-DT.
02802
02803      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
02804      MOVE 'C'                    TO  JP-RECORD-TYPE.
02805      MOVE CNTL-FILE-ID           TO  FILE-ID.
02806      MOVE ELCNTL-LENGTH          TO  FILE-LENGTH.
02807
02808      
      * EXEC CICS REWRITE
02809 *        DATASET   (CNTL-FILE-ID)
02810 *        FROM      (CONTROL-FILE)
02811 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005810' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02812
02813      PERFORM 8400-LOG-JOURNAL-RECORD.
02814
02815  8000-EXIT.
02816       EXIT.
02817      EJECT
02818
02819  8100-SEND-INITIAL-MAP.
02820      MOVE SAVE-DATE              TO  RUNDATEO.
02821      MOVE EIBTIME                TO  TIME-IN.
02822      MOVE TIME-OUT               TO  RUNTIMEO.
02823      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
02824      MOVE -1                     TO  MAINTYPL.
02825      MOVE PI-LIFE-OVERRIDE-L2    TO  LIFEHDO.
02826      MOVE PI-AH-OVERRIDE-L2      TO  AHHD1O  AHHD2O.
02827      MOVE AL-SANON               TO  LIFEHDA  AHHD1A  AHHD2A.
02828
02829      
      * EXEC CICS SEND
02830 *        MAP     (MAP-NAME)
02831 *        MAPSET  (MAPSET-NAME)
02832 *        FROM    (EL651AO)
02833 *        ERASE
02834 *        CURSOR
02835 *    END-EXEC.
           MOVE LENGTH OF
            EL651AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005831' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL651AO, 
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
           
02836
02837      GO TO 9100-RETURN-TRAN.
02838
02839  8200-SEND-DATAONLY.
02840      MOVE SAVE-DATE              TO  RUNDATEO.
02841      MOVE EIBTIME                TO  TIME-IN.
02842      MOVE TIME-OUT               TO  RUNTIMEO.
02843      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
02844      MOVE PI-LIFE-OVERRIDE-L2    TO  LIFEHDO.
02845      MOVE PI-AH-OVERRIDE-L2      TO  AHHD1O  AHHD2O.
02846      MOVE AL-SANON               TO  LIFEHDA  AHHD1A  AHHD2A.
02847
02848      
      * EXEC CICS SEND
02849 *        MAP     (MAP-NAME)
02850 *        MAPSET  (MAPSET-NAME)
02851 *        FROM    (EL651AO)
02852 *        DATAONLY
02853 *        CURSOR
02854 *    END-EXEC.
           MOVE LENGTH OF
            EL651AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00005850' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL651AO, 
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
           
02855
02856      GO TO 9100-RETURN-TRAN.
02857
02858  8300-SEND-TEXT.
02859      
      * EXEC CICS SEND TEXT
02860 *        FROM    (LOGOFF-TEXT)
02861 *        LENGTH  (LOGOFF-LENGTH)
02862 *        ERASE
02863 *        FREEKB
02864 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005861' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383631' TO DFHEIV0(25:11)
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
           
02865
02866      
      * EXEC CICS RETURN
02867 *    END-EXEC.
      *    MOVE '.(                    &   #00005868' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02868
02869  8400-LOG-JOURNAL-RECORD.
02870      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
02871      MOVE FILE-ID                TO  JP-FILE-ID.
02872      MOVE THIS-PGM               TO  JP-PROGRAM-ID.
02873 *    IF PI-JOURNAL-FILE-ID NOT = ZERO
02874 *        EXEC CICS JOURNAL
02875 *            JFILEID  (PI-JOURNAL-FILE-ID)
02876 *            JTYPEID  ('EL')
02877 *            FROM     (JOURNAL-RECORD)
02878 *            LENGTH   (FILE-LENGTH)
02879 *        END-EXEC.
02880
02881  8800-UNAUTHORIZED-ACCESS.
02882      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
02883      GO TO 8300-SEND-TEXT.
02884
02885  8810-PF23.
02886      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
02887      MOVE XCTL-005               TO  PGM-NAME.
02888      GO TO 9300-XCTL.
02889
02890  8870-NOTOPEN.
02891      MOVE ER-2055                TO  EMI-ERROR.
02892      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02893      MOVE -1                     TO  PFENTERL.
02894      IF EIBTRNID NOT = TRANS-ID
02895          GO TO 8100-SEND-INITIAL-MAP.
02896
02897      GO TO 8200-SEND-DATAONLY.
02898
02899  8880-NOT-FOUND.
02900      MOVE ER-0142                TO  EMI-ERROR.
02901      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02902      MOVE -1                     TO  MAINTYPL.
02903      IF EIBTRNID NOT = TRANS-ID
02904          GO TO 8100-SEND-INITIAL-MAP.
02905
02906      GO TO 8200-SEND-DATAONLY.
02907
02908  9000-RETURN-CICS.
02909      
      * EXEC CICS RETURN
02910 *    END-EXEC.
      *    MOVE '.(                    &   #00005911' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02911
02912  9100-RETURN-TRAN.
02913      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
02914      MOVE '651S'                 TO  PI-CURRENT-SCREEN-NO.
02915      
      * EXEC CICS RETURN
02916 *        TRANSID   (TRANS-ID)
02917 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
02918 *        LENGTH    (PI-COMM-LENGTH)
02919 *    END-EXEC.
      *    MOVE '.(CT                  &   #00005917' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02920
02921  9200-RETURN-MAIN-MENU.
02922      MOVE XCTL-626               TO  PGM-NAME.
02923      GO TO 9300-XCTL.
02924
02925  9300-XCTL.
02926      
      * EXEC CICS XCTL
02927 *        PROGRAM   (PGM-NAME)
02928 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
02929 *        LENGTH    (PI-COMM-LENGTH)
02930 *    END-EXEC.
      *    MOVE '.$C                   $   #00005928' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02931
02932  9400-CLEAR.
02933      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
02934      GO TO 9300-XCTL.
02935
02936  9500-PF12.
02937      MOVE XCTL-010               TO  PGM-NAME.
02938      GO TO 9300-XCTL.
02939
02940  9600-PGMID-ERROR.
02941      
      * EXEC CICS HANDLE CONDITION
02942 *        PGMIDERR  (8300-SEND-TEXT)
02943 *    END-EXEC.
      *    MOVE '"$L                   ! - #00005943' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303035393433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02944
02945      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
02946      MOVE ' '                    TO  PI-ENTRY-CD-1.
02947      MOVE XCTL-005               TO  PGM-NAME.
02948      MOVE PGM-NAME               TO  LOGOFF-PGM.
02949      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
02950      GO TO 9300-XCTL.
02951
02952  9700-LINK-DATE-CONVERT.
02953      MOVE LINK-ELDATCV           TO  PGM-NAME.
02954
02955      
      * EXEC CICS LINK
02956 *        PROGRAM   (PGM-NAME)
02957 *        COMMAREA  (DATE-CONVERSION-DATA)
02958 *        LENGTH    (DC-COMM-LENGTH)
02959 *    END-EXEC.
      *    MOVE '."C                   ''   #00005957' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02960
02961  9700-EXIT.
02962      EXIT.
02963
02964  9900-ERROR-FORMAT.
02965      IF NOT EMI-ERRORS-COMPLETE
02966          MOVE LINK-001           TO  PGM-NAME
02967          
      * EXEC CICS LINK
02968 *            PROGRAM   (PGM-NAME)
02969 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
02970 *            LENGTH    (EMI-COMM-LENGTH)
02971 *        END-EXEC.
      *    MOVE '."C                   ''   #00005969' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02972
02973  9900-EXIT.
02974      EXIT.
02975
02976  9990-ABEND.
02977      MOVE LINK-004               TO  PGM-NAME.
02978      MOVE DFHEIBLK               TO  EMI-LINE1
02979      
      * EXEC CICS LINK
02980 *        PROGRAM   (PGM-NAME)
02981 *        COMMAREA  (EMI-LINE1)
02982 *        LENGTH    (72)
02983 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00005981' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02984
02985      GO TO 8200-SEND-DATAONLY.
02986
02987      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL651' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
02988
02989  9995-SECURITY-VIOLATION.
02990 *           COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00006009' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303039' TO DFHEIV0(25:11)
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
02991
02992  9995-EXIT.
02993       EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL651' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9990-ABEND,
                     8880-NOT-FOUND,
                     9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 9990-ABEND,
                     4250-ADD-REC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 9990-ABEND,
                     4550-COPY-REC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8880-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 7050-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 7150-ENDFILE,
                     7175-NOTFOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 7250-ENDFILE,
                     7275-NOTFOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 7350-ENDFILE,
                     7375-NOTFOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 9990-ABEND,
                     7425-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 7550-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 8000-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL651' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
