00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL538
00003  PROGRAM-ID.                 EL538 .                                 LV006
00004 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL538
00005 *                            VMOD=2.013.                          EL538
00006 *                                                                 EL538
00007 *AUTHOR.     LOGIC, INC.                                          EL538
00008 *            DALLAS, TEXAS.                                       EL538
00009                                                                   EL538
00010 *DATE-COMPILED.                                                   EL538
00011                                                                   EL538
00012 *SECURITY.   *****************************************************EL538
00013 *            *                                                   *EL538
00014 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL538
00015 *            *                                                   *EL538
00016 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL538
00017 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL538
00018 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *EL538
00019 *            *                                                   *EL538
00020 *            *****************************************************EL538
00021 *REMARKS.                                                         EL538
00022 *             PRINT LIMITS EXCEEDED REPORT.                       EL538
00023                                                                   EL538
00024 ******************************************************************EL538
00025 *         LIMITS EXCEEDED CONTROL CARD FORMAT:                   *EL538
00026 *                                                                *EL538
00027 *  CC. 1-3  CLIENT ID.                                           *EL538
00028 *                                                                *EL538
00029 *      5-6  REPORT OPTION -                                      *EL538
00030 *                  AN - ACCOUNT, LAST NAME , AND INITIALS        *EL538
00031 *                  SN - CARRIER, STATE, LAST NAME, AND 1ST INIT  *EL538
00032 *                  SF - STATE, ACCOUNT, LAST NAME, FIRST INIT    *EL538
00033 *                  CN - GROUPING, AND LAST NAME                  *EL538
00034 *                  NI - LAST NAME, AND INITIALS                  *EL538
00035 *                  SS - SOCIAL SECURITY NUMBER ONLY              *EL538
00036 *                  AS - ACCOUNT, AND SOCIAL SECURITY NUMBER
031811*                  AL - ACCOUNT AND LAST NAME ONLY
00037 *                                                                *EL538
00038 *      8-9  STATE CODE - (DEFAULT - ZZ IF ALPHA, 99 IF NUMERIC)  *EL538
00039 *                                                                *EL538
00040 *    11-12  PENDING OPTION - '  ' - USE PENDING FILE.            *EL538
00041 *                            'NO' - DO NOT USE PENDING FILE.     *EL538
00042 *                            'PO' - SHOW 'ONLY' LIMITS EXCEEDED  *EL538
00043 *                                   CAUSED BY PENDING CERT.      *EL538
00044 *                                                                *EL538
00045 *    14-31  LIMIT ONE - 14-15  AGE                               *EL538
00046 *                       16-21  LIFE BENEFIT LIMIT         9(6).  *EL538
00047 *                       22-25  A&H MONTHLY BENEFIT LIMIT  9(4).  *EL538
00048 *                       26-31  A&H TOTAL BENEFIT LIMIT    9(6).  *EL538
00049 *    32-49  LIMIT TWO                                            *EL538
00050 *                                                                *EL538
00051 *    50-67  LIMIT THREE                                          *EL538
00052 *                                                                *EL538
00053 *    68-80  UNUSED                                               *EL538
00054 *                                                                *EL538
00055 ******************************************************************EL538
091704*                   C H A N G E   L O G
091704*
091704* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
091704*-----------------------------------------------------------------
091704*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
091704* EFFECTIVE    NUMBER
091704*-----------------------------------------------------------------
091704* 091704   2004083100004   PEMA  REMOVE CASH CERTS FROM REPORT
110604* 110604                   PEMA  ADD SPP PROCESSING
022307* 022307   2007012900001   PEMA  ADD SIG SW PROCESSING
060209* 060209   2009022300003   AJRA  FILTER CASH CERTS FROM REPORT
031811* 031811   2011020900002   PEMA  ADD ACCT,LNAM,1ST INIT OPTION &
031811* 031811   2011031100002   PEMA  ADD SSN ON EACH PRINT LINE
021913* 021913   2013021900001   PEMA  REMOVE COVERAGE IF CANCELLED
010716* 010716  CR2015082500001  PEMA  VPP CHANGES
091704******************************************************************
00056    EJECT                                                          EL538
00057  ENVIRONMENT DIVISION.                                            EL538
00058  INPUT-OUTPUT SECTION.                                            EL538
00059  FILE-CONTROL.                                                    EL538
00060      SELECT LIMIT-CARDS      ASSIGN TO SYS006-UR-2540R-S-SYS006.  EL538
00061      SELECT ALPHA-EXTRACT    ASSIGN TO SYS010-UT-2400-S-SYS010.   EL538
00062      SELECT SORTED-TRANS     ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.  EL538
00063      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   EL538
00064      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   EL538
00065      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   EL538
00066                                                                   EL538
00067      SELECT ERPNDB       ASSIGN TO SYS021-FBA1-ERPNDB             EL538
00068                          ORGANIZATION IS INDEXED                  EL538
00069                          ACCESS IS DYNAMIC                        EL538
00070                          RECORD KEY IS PB-CONTROL-PRIMARY         EL538
00071                          FILE STATUS IS ERPNDB-FILE-STATUS.       EL538
DAN
DAN        SELECT ELMSTR5      ASSIGN TO SYS022-FBA1-ELMSTR5
DAN                            ORGANIZATION IS INDEXED
DAN                            ACCESS IS DYNAMIC
DAN                            RECORD KEY IS CL-CONTROL-BY-CERT-NO
DAN                            FILE STATUS IS ELMSTR5-FILE-STATUS.      EL538
00072                                                                   EL538
00073      SELECT ELREPT       ASSIGN TO SYS018-FBA1-ELREPT             EL538
00074                          ORGANIZATION IS INDEXED                  EL538
00075                          ACCESS IS DYNAMIC                        EL538
00076                          RECORD KEY IS RF-CONTROL-PRIMARY         EL538
00077                          FILE STATUS IS DTE-VSAM-FLAGS.           EL538
00078                                                                   EL538
00079  DATA DIVISION.                                                   EL538
00080  FILE SECTION.                                                    EL538
00081  FD  LIMIT-CARDS                                                  EL538
00082      BLOCK CONTAINS 0 RECORDS
00083      RECORDING MODE IS F.                                         EL538
00084  01  LIMIT-CARD.                                                  EL538
00085      12  LC-CLIENT          PIC XXX.                              EL538
00086      12  FILLER             PIC X.                                EL538
00087      12  LC-OPTION          PIC XX.                               EL538
00088      12  FILLER             PIC X.                                EL538
00089      12  LC-STATE           PIC XX.                               EL538
00090      12  FILLER             PIC X.                                EL538
00091      12  LC-PEND-OPTION     PIC XX.                               EL538
00092      12  FILLER             PIC X.                                EL538
00093      12  LC-LIMITS.                                               EL538
00094          16  AGE-1          PIC 99.                               EL538
00095          16  LIFE-1         PIC S9(6).                            EL538
00096          16  AH-1           PIC S9(4).                            EL538
00097          16  AHMX-1         PIC S9(6).                            EL538
00098                                                                   EL538
00099          16  AGE-2          PIC 99.                               EL538
00100          16  LIFE-2         PIC S9(6).                            EL538
00101          16  AH-2           PIC S9(4).                            EL538
00102          16  AHMX-2         PIC S9(6).                            EL538
00103                                                                   EL538
00104          16  AGE-3          PIC 99.                               EL538
00105          16  LIFE-3         PIC S9(6).                            EL538
00106          16  AH-3           PIC S9(4).                            EL538
00107          16  AHMX-3         PIC S9(6).                            EL538
00108                                                                   EL538
00109      12  FILLER             PIC X(13).                            EL538
00110                                                                   EL538
00111  FD  ALPHA-EXTRACT                                                EL538
00112                              COPY ECSAEXFD.                       EL538
00113                                                                   EL538
00114  SD  SORTED-TRANS.                                                EL538
00115                                                                   EL538
00116  01  SORT-RECORD.                                                 EL538
00117      12  SORT-ALPHA-EXTRACT      PIC X(300).                      EL538
00118      12  SORT-KEY.                                                EL538
00119          16  S-CURRENT-CONTROL   PIC X(29).                       EL538
00120          16  FILLER              PIC X(8).                        EL538
00121                                                                   EL538
00122  FD  DISK-DATE                                                    EL538
00123                              COPY ELCDTEFD.                       EL538
00124                                                                   EL538
00125  FD  PRNTR                                                        EL538
031811     RECORDING MODE F
031811     LABEL RECORDS OMITTED
031811     BLOCK CONTAINS 0 RECORDS
031811     RECORD CONTAINS 150 CHARACTERS.
031811 01  PRT.                           
031811     12  P-CTL               PIC  X.
031811     12  P-DATA              PIC  X(149).
00127                                                                   EL538
00128  FD  FICH                                                         EL538
031811     BLOCK CONTAINS 0 RECORDS
031811     RECORDING MODE F.                                
031811                                                      
031811 01  FICH-REC                            PIC   X(150).
031811
00131  FD  ERPNDB.                                                      EL538
00132                              COPY ERCPNDB.                        EL538
DAN        EJECT                                                        EL538
DAN    FD  ELMSTR5.                                                     EL538
DAN                                COPY ELCMSTR.                        EL538
00133      EJECT                                                        EL538
00134  FD  ELREPT.                                                      EL538
00135                              COPY ELCREPT.                        EL538
00136      EJECT                                                        EL538
00137  WORKING-STORAGE SECTION.                                         EL538
00138  77  FILLER  PIC X(32) VALUE '********************************'.  EL538
00139  77  FILLER  PIC X(32) VALUE '     EL538 WORKING-STORAGE     '.   EL538
00140  77  FILLER  PIC X(32) VALUE '******** VMOD=2.013 ***********'.   EL538
00141                                                                   EL538
00142  77  PGM-SUB                     PIC S999  COMP   VALUE +538.     EL538
00143  77  OLC-REPORT-NAME             PIC X(6)         VALUE 'EL538'.  EL538
00144                                                                   EL538
00145  77  X                           PIC X            VALUE SPACE.    EL538
00146  77  WS-ABEND-MESSAGE            PIC X(80)        VALUE SPACES.   EL538
00147  77  WS-ABEND-FILE-STATUS        PIC XX           VALUE ZERO.     EL538
00148  77  WS-RETURN-CODE              PIC S9(3)        VALUE ZERO.     EL538
00149  77  WS-ZERO                     PIC S9           VALUE ZERO.     EL538
00150                                                                   EL538
00151  77  ERPNDB-FILE-STATUS          PIC XX           VALUE ZERO.     EL538
DAN    77  ELMSTR5-FILE-STATUS         PIC XX           VALUE ZERO.     EL538
00152                                                                   EL538
00153  77  FIRST-SWITCH                PIC X            VALUE SPACE.    EL538
00154      88  FIRST-RECORD               VALUE ' '.                    EL538
00155  77  LIMIT-SWITCH                PIC X            VALUE SPACE.    EL538
00156      88  LIMITS-EXCEEDED            VALUE 'X'.                    EL538
00157  77  PENDING-USE-SWITCH          PIC XX           VALUE SPACE.    EL538
00158      88  PENDING-NOT-USED           VALUE 'NO'.                   EL538
00159      88  PENDING-ONLY-OPTION        VALUE 'PO'.                   EL538
00160      88  PENDING-WITH-DATE          VALUE 'PD'.                   EL538
00161  77  PENDING-CERT-SWITCH         PIC X            VALUE SPACES.   EL538
00162      88  NO-PENDING-CERTS-USED      VALUE ' '.                    EL538
00163      88  CERT-ON-PENDING            VALUE 'X'.                    EL538
00164  77  LF-JOINT-SW                 PIC X            VALUE SPACES.   EL538
00165      88  JOINT-LF-COVERAGE          VALUE 'J'.                    EL538
00166  77  AH-JOINT-SW                 PIC X            VALUE SPACES.   EL538
00167      88  JOINT-AH-COVERAGE          VALUE 'J'.                    EL538
DAN    77  CLAIM-OPEN-SW               PIC XXX          VALUE SPACES.   EL538
DAN        88  OPEN-CLAIM-FOUND           VALUE 'YES'.                  EL538
DAN        88  NO-OPEN-CLAIMS             VALUE 'NO '.                  EL538
       77  WS-DISPLAY-AMT              PIC ZZZ,ZZZ,ZZZ.99.
00168                                                                   EL538
00169  01  TABLE-OF-LIMITS.                                             EL538
00170      12  LIMIT-OPTION            PIC XX.                          EL538
00171      12  LIMITS-BY-STATE     OCCURS 50 TIMES.                     EL538
00172          16  LIMIT-STATE         PIC XX.                          EL538
00173          16  LIMIT-LIMITS.                                        EL538
00174              20  LIMIT-BRACKETS  OCCURS 3 TIMES.                  EL538
00175                  24  LC-AGE      PIC 99.                          EL538
00176                  24  LC-LIFE     PIC S9(6).                       EL538
00177                  24  LC-AH       PIC S9(4).                       EL538
00178                  24  LC-AHMX     PIC S9(6).                       EL538
00179                                                                   EL538
00180  01  STATES.                                                      EL538
00181      03  DEFAULT-ST              PIC XX   VALUE '**'.             EL538
00182          88  NO-DEFAULT-STATE       VALUE '**'.                   EL538
00183      03  LAST-STATE              PIC XX   VALUE '**'.             EL538
00184                                                                   EL538
00185  01  WS-CI-INITIALS.                                              EL538
00186      03  WS-CI-1ST-INIT-FNAME    PIC X.                           EL538
00187      03  WS-CI-INIT              PIC X.                           EL538
00188                                                                   EL538
DAN    01  WS-SAVE-CERT-KEY            PIC X(12).                       EL538
DAN                                                                     EL538
00189  01  WS-ACCT.                                                     EL538
00190      03  FILLER                  PIC X(4).                        EL538
00191      03  WS-ACCT-6               PIC X(6).                        EL538
00192                                                                   EL538
00193  01  WS-SSN.                                                      EL538
00194      03  WS-SSN-ST               PIC XX.                          EL538
00195      03  WS-SSN-ACCT             PIC X(6).                        EL538
00196      03  FILLER                  PIC X(3).                        EL538
00197                                                                   EL538
00198  01  WORK-SOC-SEC-NO.                                             EL538
00199      12  WK1-SOC-SEC-NO.                                          EL538
00200          16  WK1-SSN-FIRST2      PIC XX.                          EL538
00201          16  FILLER              PIC X.                           EL538
00202          16  WK1-SSN-DASH-1      PIC X.                           EL538
00203          16  FILLER              PIC XX.                          EL538
00204          16  WK1-SSN-DASH-2      PIC X.                           EL538
00205          16  FILLER              PIC X(4).                        EL538
00206      12  WK2-SOC-SEC-NO  REDEFINES  WK1-SOC-SEC-NO.               EL538
00207          16  WK2-SSN-1           PIC XXX.                         EL538
00208          16  WK2-SSN-2           PIC XX.                          EL538
00209          16  WK2-SSN-3           PIC X(4).                        EL538
00210          16  FILLER              PIC XX.                          EL538
00211      12  WK3-SOC-SEC-NO  REDEFINES  WK1-SOC-SEC-NO.               EL538
00212          16  FILLER              PIC XX.                          EL538
00213          16  WK3-SSN-1           PIC XXX.                         EL538
00214          16  WK3-SSN-2           PIC XX.                          EL538
00215          16  WK3-SSN-3           PIC X(4).                        EL538
00216                                                                   EL538
00217  01  SOC-SEC-NUMBER.                                              EL538
00218      12  SOC-SEC-NO-1            PIC XXX.                         EL538
00219      12  FILLER                  PIC X          VALUE '-'.        EL538
00220      12  SOC-SEC-NO-2            PIC XX.                          EL538
00221      12  FILLER                  PIC X          VALUE '-'.        EL538
00222      12  SOC-SEC-NO-3            PIC X(4).                        EL538
00223                                                                   EL538
00224  01  COMP-3-WORK     COMP-3.                                      EL538
00225      12  X1                      PIC S999       VALUE +0.         EL538
00226      12  X2                      PIC S999       VALUE +0.         EL538
00227      12  X3                      PIC S999       VALUE +0.         EL538
00228      12  SUBA                    PIC S999       VALUE +0.         EL538
00229      12  CNDX                    PIC S999       VALUE +0.         EL538
00230      12  SNDX                    PIC S999       VALUE +0.         EL538
00231      12  PG-NO                   PIC S9(5)      VALUE +0.         EL538
00232      12  LN-CT                   PIC S999       VALUE +80.        EL538
00233      12  CNT                     PIC S9(7)      VALUE +0.         EL538
00234      12  MAX-LN                  PIC S9(5)      VALUE +56.        EL538
00235      12  COMBINE-LF-AMT          PIC S9(9)V99   VALUE +0.         EL538
00236      12  MATCH-COUNT             PIC S9(3)      VALUE +0.         EL538
00237      12  DROP-COUNT              PIC S9(7)      VALUE +0.         EL538
00238      12  CANCEL-COUNT            PIC S9(7)      VALUE +0.         EL538
00239                                                                   EL538
00240  01  STORE-AX-RECORDS.                                            EL538
00241      12  SV-RECS                 PIC X(300)  OCCURS  250 TIMES.   EL538
00242  01  STORE-AX-RECORDS-2.                                          EL538
00243      12  SV-RECS-2               PIC X(300)  OCCURS  250 TIMES.   EL538
00244                                                                   EL538
00245  01  SV-AREAS-MISC.                                               EL538
00246      12  SV-HI-AGE               PIC 99                  VALUE  0.EL538
00247      12  SV-REM-AMTL             PIC S9(9)V99  COMP-3    VALUE +0.EL538
00248      12  SV-AH-BEN               PIC S9(7)V99  COMP-3    VALUE +0.EL538
00249      12  SV-REM-AMTA             PIC S9(9)V99  COMP-3    VALUE +0.EL538
00250      12  SV-RUN-DATE-YMD         PIC XX        VALUE SPACES.      EL538
00251                                                                   EL538
00252      12  SV-PC-CONTROL           PIC X(36)  OCCURS 500 TIMES.     EL538
00253                                                                   EL538
00254      12  SV-JT-CONTROL           PIC X(36)         VALUE SPACES.  EL538
00255      12  SV-JT-LNAME             PIC X(15)         VALUE SPACES.  EL538
00256      12  SV-JT-FINIT             PIC X             VALUE SPACES.  EL538
00257                                                                   EL538
00258      12  PRIOR-CONTROL.                                           EL538
00259          16  PRIOR-STATE         PIC XX.                          EL538
00260          16  FILLER              PIC X(27).                       EL538
00261                                                                   EL538
00262      12  SORT-CONTROL-KEY.                                        EL538
00263          16  SV-ST               PIC XX.                          EL538
00264          16  FILLER              PIC X(27).                       EL538
00265          16  SV-EFF-DT           PIC 9(11)  COMP-3.                  CL**4
00266          16  SV-REC-ID           PIC X(2).                        EL538
00267      12  SORT-CNTRLA   REDEFINES   SORT-CONTROL-KEY.              EL538
00268          16  FILLER              PIC XX.                          EL538
00269          16  SVA-ACCT            PIC X(10).                       EL538
00270          16  SVA-NAME            PIC X(15).                       EL538
00271          16  SVA-INIT.                                            EL538
00272              20  SVA-1ST-INIT    PIC X.                           EL538
00273              20  SVA-MID-INIT    PIC X.                           EL538
00274          16  FILLER              PIC X(8).                        EL538
00275      12  SORT-CNTLB   REDEFINES   SORT-CONTROL-KEY.               EL538
00276          16  FILLER              PIC XX.                          EL538
00277          16  SVB-CARR            PIC X.                           EL538
00278          16  SVB-NAME            PIC X(15).                       EL538
00279          16  SVB-INIT            PIC X.                           EL538
00280          16  FILLER              PIC X(18).                       EL538
00281      12  SORT-CNTLC   REDEFINES   SORT-CONTROL-KEY.               EL538
00282          16  FILLER              PIC XX.                          EL538
00283          16  SVC-ACCT            PIC X(10).                       EL538
00284          16  SVC-NAME            PIC X(15).                       EL538
00285          16  SVC-INIT.                                            EL538
00286              20  SVC-1ST-INIT    PIC X.                           EL538
00287              20  SVC-MID-INIT    PIC X.                           EL538
00288          16  FILLER              PIC X(8).                        EL538
00289                                                                   EL538
00290  01  MESSAGE-WORK-AREAS.                                          EL538
00291      12  MESS1.                                                   EL538
00292          16  MESS1-LF  PIC X(6)  VALUE ' LIFE '.                  EL538
00293          16  FILLER    PIC X(9)  VALUE 'EXCEEDED '.               EL538
00294      12  MESS2.                                                   EL538
00295          16  FILLER    PIC X(16) VALUE ' MO.BEN EXCEEDED'.        EL538
00296      12  MESS3.                                                   EL538
00297          16  MESS3-AH  PIC X(6)  VALUE ' A & H'.                  EL538
00298          16  FILLER    PIC X(10) VALUE ' EXCEEDED '.              EL538
00299                                                                   EL538
00300  01  DTL.                                                         EL538
00301**    12  FILLER             PIC X.                                EL538
DAN        12  P-OPEN-FLAG        PIC X.                                EL538
00302      12  P-CARR             PIC X.                                EL538
00303      12  P-GROUPING         PIC X(6).                             EL538
00304      12  FILLER             PIC X.                                EL538
00305      12  P-ST               PIC XX.                               EL538
00306      12  FILLER             PIC X.                                EL538
00307      12  P-ACCT             PIC X(10).                            EL538
00308      12  FILLER             PIC X.                                EL538
00309      12  P-CERT             PIC X(11).                            EL538
00310      12  FILLER             PIC X.                                EL538
00311      12  P-EMO              PIC 99.                               EL538
00312      12  P-EDA              PIC 99.                               EL538
00313      12  P-EYR              PIC 99.                               EL538
00314      12  P-JOINT-IND        PIC X.                                EL538
00315      12  P-NAME             PIC X(15).                            EL538
00316      12  FILLER             PIC X.                                EL538
00317      12  P-1ST-NAME         PIC X(10).                            EL538
00318      12  P-MID-INIT         PIC X.                                EL538
00319      12  FILLER             PIC X.
031811     12  P-SSN              PIC X(11).
031811     12  F                  PIC X.
00320      12  P-AGE              PIC 99.                               EL538
00321      12  FILLER             PIC X.                                EL538
00322      12  P-TTL.                                                   EL538
00323          16  P-L-TYPE       PIC XXX.                              EL538
00324          16  FILLER         PIC X.                                EL538
00325          16  P-L-TERM       PIC 999         BLANK WHEN ZERO.      EL538
00326      12  P-L-FACE           PIC ZZZZZZZZZ-  BLANK WHEN ZERO.      EL538
00327      12  P-L-REMTERM        PIC 999         BLANK WHEN ZERO.      EL538
00328      12  FILLER             PIC X.                                EL538
00329      12  P-L-REMFACE        PIC ZZZZZZZZZ-  BLANK WHEN ZERO.      EL538
00330      12  FILLER             PIC X.                                EL538
00331      12  P-A-TYPE           PIC XXX.                              EL538
00332      12  FILLER             PIC X.                                EL538
00333      12  P-A-TERM           PIC 999         BLANK WHEN ZERO.      EL538
022307     12  P-A-BEN            PIC ZZZZZ-      BLANK WHEN ZERO.      EL538
00335      12  P-A-REMTERM        PIC 999         BLANK WHEN ZERO.      EL538
00336      12  FILLER             PIC X .                               EL538
022307     12  P-A-REMBEN         PIC ZZZZZZ-    BLANK WHEN ZERO.
022307     12  P-ID               PIC XXX.
00339                                                                   EL538
00340  01  HD1.                                                         EL538
00341      12  HD-TYPE       PIC X(40)   VALUE SPACES.                  EL538
00342      12  FILLER        PIC X(11)   VALUE SPACES.                  EL538
031811     12  F             PIC X(6)    VALUE SPACES.
00343      12  FILLER        PIC X(23)   VALUE 'LIMITS EXCEEDED REPORT'.EL538
031811     12  F             PIC X(25)   VALUE SPACES.
031811     12  F             PIC X(8)    VALUE 'STATE - '.
031811     12  HD1-STATE     PIC XX      VALUE SPACES.
031811     12  F             PIC X(6)    VALUE SPACES.
00344      12  FILLER        PIC X(10)   VALUE SPACES.                  EL538
00345      12  FILLER        PIC X(13)   VALUE 'EL538'.                 EL538
00346                                                                   EL538
00347  01  HD2.                                                         EL538
00348      12  HD2T          PIC X(44)   VALUE SPACES.                  EL538
00349      12  FILLER        PIC X(3)    VALUE SPACES.                  EL538
031811     12  F             PIC X(6)    VALUE SPACES.
00350      12  HD-CO         PIC X(30).                                 EL538
031811     12  F             PIC X(6)    VALUE SPACES.
00351      12  FILLER        PIC X(42)   VALUE SPACES.                  EL538
00352      12  HD-RD         PIC X(8).                                  EL538
00353      12  FILLER        PIC X(5)    VALUE SPACES.                  EL538
00354                                                                   EL538
00355  01  HD3.                                                         EL538
00356      12  HD3T          PIC X(44)   VALUE SPACES.                  EL538
00357      12  FILLER        PIC X(9)    VALUE SPACES.                  EL538
031811     12  F             PIC X(6)    VALUE SPACES.
00358      12  HD-DT         PIC X(18).                                 EL538
031811     12  F             PIC X(6)    VALUE SPACES.
00359      12  FILLER        PIC X(48)   VALUE SPACES.                  EL538
00360      12  FILLER        PIC X(5)    VALUE 'PAGE'.                  EL538
00361      12  HD-PG         PIC ZZ,ZZ9.                                EL538
00362      12  FILLER        PIC XX      VALUE SPACES.                  EL538
00363                                                                   EL538
00364  01  HD3-D.                                                       EL538
00365      12  HD3T-D        PIC X(44)   VALUE SPACES.                  EL538
00366      12  FILLER        PIC X(2)    VALUE SPACES.                  EL538
031811     12  F             PIC X(6)    VALUE SPACES.
00367      12  FILLER        PIC X(23)   VALUE                          EL538
00368                'PENDING BUSINESS AFTER '.                         EL538
00369      12  HD-DT-D       PIC X(18).                                 EL538
031811     12  F             PIC X(6)    VALUE SPACES.
00370      12  FILLER        PIC X(25)   VALUE SPACES.                  EL538
00371      12  FILLER        PIC X(5)    VALUE 'PAGE'.                  EL538
00372      12  HD-PG-D       PIC ZZ,ZZ9.                                EL538
00373      12  FILLER        PIC X(9)    VALUE SPACES.                  EL538
00374                                                                   EL538
00375  01  HD3A.                                                        EL538
00376      12  HD3AT         PIC X(44)   VALUE SPACES.                  EL538
00377      12  FILLER        PIC X(89)   VALUE SPACES.                  EL538
00378                                                                   EL538
00379  01  HD4-HDR.                                                     EL538
00380      12  FILLER  PIC X(72) VALUE SPACES.                          EL538
031811     12  F       PIC X(12) VALUE SPACES.
00381      12  FILLER  PIC X(13) VALUE ' *-----------'.                 EL538
00382      12  HD-4-LF PIC X(6)  VALUE ' LIFE '.                        EL538
00383      12  FILLER  PIC X(23) VALUE '-----------* *-------- '.       EL538
00384      12  HD-4-AH PIC X(6)  VALUE ' A & H'.                        EL538
00385      12  FILLER  PIC X(12) VALUE '--------*   '.                  EL538
00386                                                                   EL538
00387  01  HD4.                                                         EL538
00388      12  FILLER  PIC X(30) VALUE ' CAR GRP ST   ACCOUNT     CERT'.EL538
00389      12  FILLER  PIC X(30) VALUE '      ISSUE                   '.EL538
031811     12  F       PIC X(12) VALUE SPACES.
00390      12  FILLER  PIC X(30) VALUE '         AGE TYP ORG ORIGINAL '.EL538
00391      12  FILLER  PIC X(30) VALUE 'REM  REMAIN    TYP ORG ORIG RE'.EL538
00392      12  FILLER  PIC X(12) VALUE 'M REMAIN  SR'.                  EL538
00393                                                                   EL538
00394  01  HD5.                                                         EL538
00395      12  FILLER  PIC X(30) VALUE '               NUMBER    NUMBE'.EL538
00396      12  FILLER  PIC X(30) VALUE 'R      DATE      INSUREDS NAME'.EL538
031811     12  F       PIC X(12) VALUE SPACES.
00397      12  FILLER  PIC X(30) VALUE ' SSN             TRM BENEFIT  '.EL538
00398      12  FILLER  PIC X(30) VALUE 'TRM  BENEFIT       TRM BENE TR'.EL538
00399      12  FILLER  PIC X(12) VALUE 'M BENEFIT   '.                  EL538
00400                                                                   EL538
00401  01  HD6.                                                         EL538
00402      12  HD-6A   PIC X(27)  VALUE SPACES.                         EL538
00403      12  HD-SOC  PIC X(11)  VALUE SPACES.                         EL538
00404      12  FILLER  PIC X(47)  VALUE SPACES.                         EL538
00405      12  HD6-SUB PIC X(47)  VALUE SPACES.                         EL538
00406                                                                   EL538
00407  01  HD6-SOC PIC X(27) VALUE '  SOCIAL SECURITY NUMBER - '.       EL538
00408                                                                   EL538
00409  01  HD7.                                                         EL538
00410      12  L-DESC  PIC X(15) VALUE SPACES.                          EL538
00411      12  A-DESA  PIC X(16) VALUE SPACES.                          EL538
00412      12  A-DESB  PIC X(16) VALUE SPACES.                          EL538
00413                                                                   EL538
00414  01  HD8.                                                         EL538
00415      12  FILLER         PIC X(23)   VALUE SPACE.                  EL538
00416      12  FILLER         PIC X(15)   VALUE 'L I M I T  -  1'.      EL538
00417      12  FILLER         PIC X(21)   VALUE SPACE.                  EL538
00418      12  FILLER         PIC X(15)   VALUE 'L I M I T  -  2'.      EL538
00419      12  FILLER         PIC X(21)   VALUE SPACE.                  EL538
00420      12  FILLER         PIC X(15)   VALUE 'L I M I T  -  3'.      EL538
00421      12  FILLER         PIC X(22)   VALUE SPACE.                  EL538
00422                                                                   EL538
00423  01  HD8A.                                                        EL538
00424      12  FILLER  PIC X(15) VALUE SPACES.                          EL538
00425      12  FILLER  PIC X(30) VALUE '*----------------------------*'.EL538
00426      12  FILLER  PIC X(6)  VALUE SPACES.                          EL538
00427      12  FILLER  PIC X(30) VALUE '*----------------------------*'.EL538
00428      12  FILLER  PIC X(6)  VALUE SPACES.                          EL538
00429      12  FILLER  PIC X(30) VALUE '*----------------------------*'.EL538
00430      12  FILLER  PIC X(15) VALUE SPACES.                          EL538
00431                                                                   EL538
00432  01  HD9.                                                         EL538
00433      12  FILLER         PIC X(5)    VALUE SPACE.                  EL538
00434      12  FILLER         PIC X(16)   VALUE 'STATE     AGE   '.     EL538
00435      12  HD-9-LF-1      PIC XX      VALUE 'LF'.                   EL538
00436      12  FILLER         PIC X(7)    VALUE ' BEN   '.              EL538
00437      12  HD-9-AH1-1     PIC XX      VALUE 'AH'.                   EL538
00438      12  FILLER         PIC X(11)   VALUE ' BEN   TOT '.          EL538
00439      12  HD-9-AH2-1     PIC XX      VALUE 'AH'.                   EL538
00440      12  FILLER         PIC X(12)   VALUE '      AGE   '.         EL538
00441      12  HD-9-LF-2      PIC XX      VALUE 'LF'.                   EL538
00442      12  FILLER         PIC X(7)    VALUE ' BEN   '.              EL538
00443      12  HD-9-AH1-2     PIC XX      VALUE 'AH'.                   EL538
00444      12  FILLER         PIC X(11)   VALUE ' BEN   TOT '.          EL538
00445      12  HD-9-AH2-2     PIC XX      VALUE 'AH'.                   EL538
00446      12  FILLER         PIC X(12)   VALUE '      AGE   '.         EL538
00447      12  HD-9-LF-3      PIC XX      VALUE 'LF'.                   EL538
00448      12  FILLER         PIC X(7)    VALUE ' BEN   '.              EL538
00449      12  HD-9-AH1-3     PIC XX      VALUE 'AH'.                   EL538
00450      12  FILLER         PIC X(11)   VALUE ' BEN   TOT '.          EL538
00451      12  HD-9-AH2-3     PIC XX      VALUE 'AH'.                   EL538
00452      12  FILLER         PIC X(15)   VALUE SPACE.                  EL538
00453                                                                   EL538
00454  01  HD10.                                                        EL538
00455      12  FILLER         PIC X(7)    VALUE SPACE.                  EL538
00456      12  HD-10-STATE    PIC XX      VALUE SPACE.                  EL538
00457      12  FILLER         PIC X(7)    VALUE SPACE.                  EL538
00458      12  HD-10-AGE-1    PIC ZZ      VALUE ZEROS.                  EL538
00459      12  FILLER         PIC XXX     VALUE SPACE.                  EL538
00460      12  HD-10-LIFE-1   PIC Z(6)    VALUE ZEROS.                  EL538
00461      12  FILLER         PIC X(4)    VALUE SPACE.                  EL538
00462      12  HD-10-AH-1     PIC Z(4)    VALUE ZEROS.                  EL538
00463      12  FILLER         PIC X(4)    VALUE SPACE.                  EL538
00464      12  HD-10-AHMX-1   PIC Z(6)    VALUE ZEROS.                  EL538
00465      12  FILLER         PIC X(7)    VALUE SPACE.                  EL538
00466      12  HD-10-AGE-2    PIC ZZ      VALUE ZEROS.                  EL538
00467      12  FILLER         PIC XXX     VALUE SPACE.                  EL538
00468      12  HD-10-LIFE-2   PIC Z(6)    VALUE ZEROS.                  EL538
00469      12  FILLER         PIC X(4)    VALUE SPACE.                  EL538
00470      12  HD-10-AH-2     PIC Z(4)    VALUE ZEROS.                  EL538
00471      12  FILLER         PIC X(4)    VALUE SPACE.                  EL538
00472      12  HD-10-AHMX-2   PIC Z(6)    VALUE ZEROS.                  EL538
00473      12  FILLER         PIC X(7)    VALUE SPACE.                  EL538
00474      12  HD-10-AGE-3    PIC ZZ      VALUE ZEROS.                  EL538
00475      12  FILLER         PIC XXX     VALUE SPACE.                  EL538
00476      12  HD-10-LIFE-3   PIC Z(6)    VALUE ZEROS.                  EL538
00477      12  FILLER         PIC X(4)    VALUE SPACE.                  EL538
00478      12  HD-10-AH-3     PIC Z(4)    VALUE ZEROS.                  EL538
00479      12  FILLER         PIC X(4)    VALUE SPACE.                  EL538
00480      12  HD-10-AHMX-3   PIC Z(6)    VALUE ZEROS.                  EL538
00481      12  FILLER         PIC X(15)   VALUE SPACE.                  EL538
00482                                                                   EL538
00483  01  TABLE-HD.                                                    EL538
00484      12  FILLER        PIC X(46)   VALUE SPACE.                   EL538
031811     12  F             PIC X(6)    VALUE SPACES.
00485      12  FILLER        PIC X(31)   VALUE                          EL538
00486                   '***  LIMITS EXCEEDED TABLE  ***'.              EL538
031811     12  F             PIC X(6)    VALUE SPACES.
00487      12  FILLER        PIC X(55)   VALUE SPACE.                   EL538
00488                                                                   EL538
00489  01  LIMITS-HD.                                                   EL538
00490      12  FILLER  PIC X(5)  VALUE '(AGE'.                          EL538
00491      12  HD-AGE  PIC 99.                                          EL538
00492      12  FILLER  PIC X     VALUE SPACE.                           EL538
00493      12  HD-LF   PIC XX    VALUE 'LF'.                            EL538
00494      12  FILLER  PIC X     VALUE SPACE.                           EL538
00495      12  HD-TTL  PIC $$$$,$$$.                                    EL538
00496      12  FILLER  PIC X(4)  VALUE ' MB '.                          EL538
00497      12  HD-MO   PIC $$,$$$.                                      EL538
00498      12  FILLER  PIC X     VALUE SPACE.                           EL538
00499      12  HD-AH   PIC XX    VALUE 'AH'.                            EL538
00500      12  FILLER  PIC X     VALUE SPACE.                           EL538
00501      12  HD-TTA  PIC $$$$,$$$.                                    EL538
00502      12  FILLER  PIC X     VALUE ')'.                             EL538
00503      EJECT                                                        EL538
00504                              COPY ECSAEX01.                       EL538
00505      EJECT                                                        EL538
00506                              COPY ELCAEXVR.                       EL538
00507      EJECT                                                        EL538
00508                              COPY ELCDATE.                           CL**6
00509      EJECT                                                        EL538
00510                              COPY ELCDTECX.                       EL538
00511                                                                   EL538
00512                              COPY ELCDTEVR.                       EL538
00513                                                                   EL538
00514                                                                   EL538
00525                                                                   EL538
00526                                                                   EL538
00527      EJECT                                                        EL538
00528  PROCEDURE DIVISION.                                              EL538
00529  0000-DATE-CARD-ROUTINE SECTION.                                  EL538
00530                              COPY ELCDTERX SUPPRESS.              EL538
00531                                                                   EL538
00532  1000-SET-VARIABLE-HEADINGS.                                      EL538
00533      MOVE COMPANY-NAME           TO  HD-CO.                       EL538
00534      MOVE WS-CURRENT-DATE        TO  HD-RD.                       EL538
00535      MOVE ALPH-DATE              TO  HD-DT, HD-DT-D.              EL538
00536                                                                   EL538
00537      MOVE BIN-RUN-DATE           TO SV-RUN-DATE-YMD.              EL538
00538                                                                   EL538
00539      MOVE LIFE-OVERRIDE-L2       TO  HD-LF                        EL538
00540                                      HD-9-LF-1                    EL538
00541                                      HD-9-LF-2                    EL538
00542                                      HD-9-LF-3.                   EL538
00543      MOVE AH-OVERRIDE-L2         TO  HD-AH                        EL538
00544                                      HD-9-AH1-1  HD-9-AH2-1       EL538
00545                                      HD-9-AH1-2  HD-9-AH2-2       EL538
00546                                      HD-9-AH1-3  HD-9-AH2-3.      EL538
00547      MOVE LIFE-OVERRIDE-L6       TO  HD-4-LF                      EL538
00548                                      MESS1-LF.                    EL538
00549      MOVE AH-OVERRIDE-L6         TO  HD-4-AH                      EL538
00550                                      MESS3-AH.                    EL538
00551  EJECT                                                            EL538
00552  EJECT                                                            EL538
00553  1100-CONTROL-CARD-ROUTINE.                                       EL538
00554      OPEN INPUT LIMIT-CARDS                                       EL538
00555           OUTPUT PRNTR.                                           EL538
00556                                                                   EL538
00557      MOVE SPACES                 TO  TABLE-OF-LIMITS              EL538
00558                                      STORE-AX-RECORDS.            EL538
00559      MOVE +1                     TO  X3.                          EL538
00560                                                                   EL538
00561  1200-READ-CARDS.                                                 EL538
00562      READ LIMIT-CARDS  AT END  CLOSE LIMIT-CARDS                  EL538
00563                                GO TO 1300-CHECK-FOR-LIMITS.       EL538
00564                                                                   EL538
00565      IF LC-CLIENT NOT = DTE-CLIENT                                EL538
00566          GO TO 1200-READ-CARDS.                                   EL538
00567                                                                   EL538
00568      MOVE LC-OPTION              TO LIMIT-OPTION.                 EL538
00569      MOVE LC-PEND-OPTION         TO PENDING-USE-SWITCH.           EL538
00570                                                                   EL538
00571      INSPECT LIMIT-CARD  CONVERTING ' '  TO '0'.                  EL538
00572                                                                   EL538
00573      MOVE LC-STATE               TO LIMIT-STATE (X3).             EL538
00574      MOVE LC-LIMITS              TO LIMIT-LIMITS (X3).            EL538
00575                                                                   EL538
00576      IF LC-STATE = 'ZZ'  OR  '99'                                 EL538
00577          MOVE LC-STATE           TO DEFAULT-ST.                   EL538
00578                                                                   EL538
00579      ADD +1                      TO X3.                           EL538
00580                                                                   EL538
00581      IF X3 GREATER THAN +50                                       EL538
00582          MOVE '***  LIMIT TABLE SIZE EXCEEDED  ***'               EL538
00583                                  TO WS-ABEND-MESSAGE              EL538
00584          GO TO ABEND-PGM.                                         EL538
00585                                                                   EL538
00586      GO TO 1200-READ-CARDS.                                       EL538
00587                                                                   EL538
00588  1300-CHECK-FOR-LIMITS.                                           EL538
00589      MOVE '**'                   TO LIMIT-STATE (X3).             EL538
00590      MOVE ZEROS                  TO LIMIT-LIMITS (X3).            EL538
00591                                                                   EL538
00592      IF X3 = +1                                                   EL538
00593          DISPLAY '**********************************************' EL538
00594          DISPLAY '***  EL538  **  NOT SCHEDULED TO RUN       ***' EL538
00595          DISPLAY '**********************************************' EL538
00596          GO TO 9999-END-OF-JOB.                                   EL538
00597                                                                   EL538
00598      IF NO-DEFAULT-STATE                                          EL538
00599          MOVE '***  NO DEFAULT LIMITS CARD FOUND  ***'            EL538
00600                                  TO WS-ABEND-MESSAGE              EL538
00601          GO TO ABEND-PGM.                                         EL538
00602                                                                   EL538
00603      PERFORM 8000-PRINT-LIMITS-TABLE THRU 8999-PRINT-LIMITS-EXIT. EL538
00604                                                                   EL538
00605  1400-SORTING-PROCEDURES.                                         EL538
00606      SORT SORTED-TRANS                                            EL538
00607              ON ASCENDING KEY SORT-KEY                            EL538
00608          INPUT PROCEDURE IS SORT-ALPHA-EXTRACTS                   EL538
00609          OUTPUT PROCEDURE IS LIMITS-EXCEEDED-REPORT.              EL538
00610                                                                   EL538
00611                                                                   EL538
00612      IF SORT-RETURN GREATER THAN +0004                            EL538
00613          MOVE '**  SORT UNSUCCESSFULL  **'                        EL538
00614                                  TO WS-ABEND-MESSAGE              EL538
00615          GO TO ABEND-PGM.                                         EL538
00616                                                                   EL538
00617                                                                   EL538
00618      GO TO 9999-END-OF-JOB.                                       EL538
00619  EJECT                                                            EL538
00620  SORT-ALPHA-EXTRACTS SECTION.                                     EL538
00621                                                                   EL538
00622  2000-OPEN-ALPHA-EXTRACTS.                                        EL538
00623      OPEN INPUT ALPHA-EXTRACT.                                    EL538
00624                                                                   EL538
00625  2100-READ-FOR-SORT.                                              EL538
00626      READ ALPHA-EXTRACT  INTO  ALPHA-RECORD                       EL538
00627                    AT END GO TO 2600-CLOSE-ALPHA.                 EL538
00628                                                                   EL538
00629      COPY ELCAEXM1.                                               EL538
00630                                                                   EL538
00631      IF AX-ENTRY-STATUS = '5'  OR  '9' OR 'D' OR 'V' OR 'U'       EL538
00632          GO TO 2100-READ-FOR-SORT.                                EL538
00633                                                                   EL538
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
060209        IF (AX-FNAME (1:4) = 'CASH' OR AX-LNAME (1:4) = 'CASH')
091704                 AND
091704           ((AX-LF-TERM = 1)
091704           OR (AX-AH-TERM = 1))
091704           GO TO 2100-READ-FOR-SORT
091704        END-IF
091704     END-IF
091704
00634      IF AX-LF-STATUS = 'E' OR '6' OR '7' OR '8' OR ' '
00635         MOVE ZEROS               TO AX-LF-REMAMT
021913                                    AX-LF-TYP
021913     END-IF

00637      IF AX-AH-STATUS = 'E' OR '6' OR '7' OR '8' OR ' '
00638         MOVE ZEROS               TO AX-AH-REMAMT
021913                                    AX-AH-TYP
021913     END-IF
00639                                                                   EL538
00640      IF AX-LF-REMTERM EQUAL ZEROS                                 EL538
00641            MOVE ZEROS TO AX-LF-REMAMT.                            EL538
00642                                                                   EL538
00643      IF AX-AH-REMTERM EQUAL ZEROS                                 EL538
00644            MOVE ZEROS TO AX-AH-REMAMT.                            EL538
00645                                                                   EL538
00646      IF (AX-LF-REMAMT = ZERO)  AND                                EL538
00647          (AX-AH-REMAMT = ZERO)                                    EL538
00648              GO TO 2100-READ-FOR-SORT.                            EL538
00649                                                                   EL538
00650  2200-BUILD-SORT-KEY.                                             EL538
00651      PERFORM 4000-LIFE-CLAS-LOOK-RTN THRU                         EL538
00652              4999-LIFE-CLAS-LOOK-RTN-X.                           EL538
00653                                                                   EL538
00654      IF CLAS-I-BAL (CLAS-INDEXL) = 'B' OR 'O' OR 'Z'              EL538
00655          GO TO 2299-RELEASE-EXIT.                                 EL538
00656                                                                   EL538
00657      PERFORM 5000-AH-CLAS-LOOK-RTN THRU                           EL538
00658              5999-AH-CLAS-LOOK-RTN-X.                             EL538
00659                                                                   EL538
00660      IF CLAS-I-BAL (CLAS-INDEXA) = 'B' OR 'O' OR 'Z'              EL538
00661          GO TO 2299-RELEASE-EXIT.                                 EL538
00662                                                                   EL538
00663      MOVE SPACES                 TO SORT-CONTROL-KEY.             EL538
00664                                                                   EL538
031811     IF LIMIT-OPTION = 'AN' OR 'CN' OR 'SN' OR 'SF' OR 'AL'
00666          IF AX-NAME = SPACES                                      EL538
00667              GO TO 2299-RELEASE-EXIT.                             EL538
00668                                                                   EL538
00669 ***                                                               EL538
00670 ***  IF SOCIAL SECURITY WAS PRIMED WITH STATE AND ACCOUNT,        EL538
00671 ***  CONSIDER IT AS NO SSN.                                       EL538
00672 ***                                                               EL538
00673      MOVE AX-SOC-NO              TO WS-SSN.                       EL538
00674      IF AX-STATE      = WS-SSN-ST  OR                             EL538
00675         AX-ACCT-PRIME = WS-SSN-ACCT                               EL538
00676           MOVE SPACES TO AX-SOC-NO.                               EL538
00677                                                                   EL538
00678      IF LIMIT-OPTION = 'SS' OR 'AS'                               EL538
00679          IF AX-SOC-NO = SPACES  OR  ZEROS  OR                     EL538
00680                         '000-00-0000'  OR                         EL538
00681                         '00000000000'  OR                         EL538
00682                         '000000000  '  OR                         EL538
00683                         '   -  -    '  OR                         EL538
00684                         '999-99-9999'  OR                         EL538
00685                         '99999999999'                             EL538
00686              GO TO 2299-RELEASE-EXIT                              EL538
00687          ELSE                                                     EL538
00688              PERFORM 2500-FIX-SOC-SEC-NUMBER THRU                 EL538
00689                      2599-FIX-SSN-EXIT.                           EL538
031811     IF LIMIT-OPTION = 'AL'
031811        PERFORM 2500-FIX-SOC-SEC-NUMBER
031811                                 THRU 2599-FIX-SSN-EXIT
031811     END-IF
00690                                                                   EL538
00691      MOVE AX-STATE               TO SV-ST.                        EL538
00692      MOVE AX-RECORD-ID           TO SV-REC-ID.                    EL538
00693      MOVE AX-DT                  TO SV-EFF-DT.                    EL538
00694                                                                   EL538
00695      IF LIMIT-OPTION = 'AN'                                       EL538
00696          MOVE AX-ACCOUNT         TO SVA-ACCT                      EL538
00697          MOVE AX-LNAME           TO SVA-NAME                      EL538
00698          MOVE AX-1ST-INIT-FNAME  TO SVA-1ST-INIT                  EL538
00699          MOVE AX-INIT            TO SVA-MID-INIT.                 EL538
00700                                                                   EL538
031811     IF LIMIT-OPTION = 'AL'
031811         MOVE AX-ACCOUNT         TO SVA-ACCT
031811         MOVE AX-LNAME           TO SVA-NAME
031811         MOVE AX-1ST-INIT-FNAME  TO SVA-1ST-INIT
031811     END-IF
00700                                                                   EL538
00701      IF LIMIT-OPTION = 'SS'                                       EL538
00702          MOVE AX-SOC-NO          TO SVB-NAME.                     EL538
00703                                                                   EL538
00704      IF LIMIT-OPTION = 'SN'                                       EL538
00705          MOVE AX-CARRIER         TO SVB-CARR                      EL538
00706          MOVE AX-LNAME           TO SVB-NAME                      EL538
00707          MOVE AX-1ST-INIT-FNAME  TO SVB-INIT.                     EL538
00708                                                                   EL538
00709      IF LIMIT-OPTION = 'SF'                                       EL538
00710          MOVE AX-ACCOUNT         TO SVC-ACCT                      EL538
00711          MOVE AX-LNAME           TO SVC-NAME                      EL538
00712          MOVE AX-1ST-INIT-FNAME  TO SVC-1ST-INIT                  EL538
00713          IF AX-JOINT-ALPHA                                        EL538
00714              MOVE SPACES         TO SVC-MID-INIT                  EL538
00715          ELSE                                                     EL538
00716              MOVE LOW-VALUES     TO SVC-MID-INIT.                 EL538
00717                                                                   EL538
00718      IF LIMIT-OPTION = 'AS'                                       EL538
00719          MOVE AX-ACCOUNT         TO SVA-ACCT                      EL538
00720          MOVE AX-SOC-NO          TO SVA-NAME.                     EL538
00721                                                                   EL538
00722      IF LIMIT-OPTION = 'CN'                                       EL538
00723          MOVE AX-GROUPING        TO SVA-ACCT                      EL538
00724          MOVE AX-LNAME           TO SVA-NAME                      EL538
00725          MOVE AX-1ST-INIT-FNAME  TO SVA-1ST-INIT                  EL538
00726          MOVE AX-INIT            TO SVA-MID-INIT.                 EL538
00727                                                                   EL538
00728      IF LIMIT-OPTION = 'NI'                                       EL538
00729          MOVE AX-LNAME           TO SVA-NAME                      EL538
00730          MOVE AX-1ST-INIT-FNAME  TO SVA-1ST-INIT                  EL538
00731          MOVE AX-INIT            TO SVA-MID-INIT.                 EL538
00732                                                                   EL538
00733  2290-RELEASE-TO-SORT.                                            EL538
00734      COPY ELCAEXM2.                                               EL538
00735      MOVE ALPHA-RECORD           TO SORT-ALPHA-EXTRACT.           EL538
00736      MOVE SORT-CONTROL-KEY       TO SORT-KEY.                     EL538
00737                                                                   EL538
00738      RELEASE SORT-RECORD.                                         EL538
00739                                                                   EL538
00740      ADD +1 TO CNT.                                               EL538
00741                                                                   EL538
00742  2299-RELEASE-EXIT.                                               EL538
00743      EXIT.                                                        EL538
00744                                                                   EL538
00745  2300-AX-READ-RETURN.                                             EL538
00746                                                                   EL538
00747      GO TO 2100-READ-FOR-SORT.                                    EL538
00748                                                                   EL538
00749  2500-FIX-SOC-SEC-NUMBER.                                         EL538
00750                                                                   EL538
00751      MOVE AX-SOC-NO              TO WORK-SOC-SEC-NO.              EL538
00752                                                                   EL538
00753      IF WK1-SSN-DASH-1 = '-'                                      EL538
00754          GO TO 2599-FIX-SSN-EXIT.                                 EL538
00755                                                                   EL538
00756      IF (WK1-SSN-DASH-1 = ' '                                     EL538
00757        AND WK1-SSN-FIRST2 NOT EQUAL SPACES)                       EL538
00758          MOVE '-'                TO WK1-SSN-DASH-1                EL538
00759          MOVE '-'                TO WK1-SSN-DASH-2                EL538
00760          MOVE WORK-SOC-SEC-NO    TO AX-SOC-NO                     EL538
00761          GO TO 2599-FIX-SSN-EXIT.                                 EL538
00762                                                                   EL538
00763      IF WK1-SSN-FIRST2 NOT = SPACES                               EL538
00764          MOVE WK2-SSN-1          TO SOC-SEC-NO-1                  EL538
00765          MOVE WK2-SSN-2          TO SOC-SEC-NO-2                  EL538
00766          MOVE WK2-SSN-3          TO SOC-SEC-NO-3                  EL538
00767      ELSE                                                         EL538
00768          MOVE WK3-SSN-1          TO SOC-SEC-NO-1                  EL538
00769          MOVE WK3-SSN-2          TO SOC-SEC-NO-2                  EL538
00770          MOVE WK3-SSN-3          TO SOC-SEC-NO-3.                 EL538
00771                                                                   EL538
00772      MOVE SOC-SEC-NUMBER         TO AX-SOC-NO.                    EL538
00773                                                                   EL538
00774  2599-FIX-SSN-EXIT.                                               EL538
00775      EXIT.                                                        EL538
00776                                                                   EL538
00777                                                                   EL538
00778  2600-CLOSE-ALPHA.                                                EL538
00779      CLOSE ALPHA-EXTRACT.                                         EL538
00780                                                                   EL538
00781      IF PENDING-NOT-USED                                          EL538
00782          GO TO SORT-ALPHA-EXTRACTS-EXIT.                          EL538
00783                                                                   EL538
00784      OPEN INPUT ERPNDB.                                           EL538
00785                                                                   EL538
00786      IF ERPNDB-FILE-STATUS  = '00' OR '97'                        EL538
00787          NEXT SENTENCE                                            EL538
00788        ELSE                                                       EL538
00789          MOVE 'ERROR OCCURED OPEN - ERPNDB'                       EL538
00790                                  TO  WS-ABEND-MESSAGE             EL538
00791          MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL538
00792          GO TO ABEND-PGM.                                         EL538
00793                                                                   EL538
00794  2700-READ-PENDING.                                               EL538
00795      MOVE LOW-VALUES             TO  PB-CONTROL-PRIMARY.          EL538
00796      MOVE DTE-CLASIC-COMPANY-CD  TO  PB-COMPANY-CD.               EL538
00797                                                                   EL538
00798      START ERPNDB                                                 EL538
00799          KEY IS GREATER THAN PB-CONTROL-PRIMARY.                  EL538
00800                                                                   EL538
00801  2800-READ-PENDING.                                               EL538
00802      READ ERPNDB NEXT RECORD.                                     EL538
00803                                                                   EL538
00804      IF ERPNDB-FILE-STATUS = '10'  OR  '23'  OR  '94'             EL538
00805          GO TO 2900-CLOSE-PENDING.                                EL538
00806                                                                   EL538
00807      IF ERPNDB-FILE-STATUS NOT = ZERO                             EL538
00808          MOVE 'ERROR OCCURED READNEXT - ERPNDB'                   EL538
00809                                  TO  WS-ABEND-MESSAGE             EL538
00810          MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL538
00811          GO TO ABEND-PGM.                                         EL538
00812                                                                   EL538
00813      IF PB-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL538
00814          GO TO 2900-CLOSE-PENDING.                                EL538
00815                                                                   EL538
00816      IF PB-ISSUE OR PB-CANCELLATION                               EL538
00817          NEXT SENTENCE                                            EL538
00818      ELSE                                                         EL538
00819          GO TO 2800-READ-PENDING.                                 EL538
00820                                                                   EL538
00821      IF PB-CANCELLATION                                           EL538
00822          GO TO 2850-FORMAT-CANCELLATION.                          EL538
00823                                                                   EL538
00824      IF PB-I-ENTRY-STATUS = '5'  OR  '9' OR 'D' OR 'V' OR 'U'     EL538
00825          GO TO 2800-READ-PENDING.                                 EL538
00826                                                                   EL538
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
060209        IF (PB-I-INSURED-FIRST-NAME (1:4) = 'CASH' OR
060209            PB-I-INSURED-LAST-NAME (1:4) = 'CASH')
091704                 AND
091704           ((PB-I-LF-TERM = 1)
091704           OR (PB-I-AH-TERM = 1))
091704           GO TO 2800-READ-PENDING
091704        END-IF
091704     END-IF
091704
00827      IF DTE-CLIENT EQUAL 'LBL'                                    EL538
00828          NEXT SENTENCE                                            EL538
00829      ELSE                                                         EL538
00830         IF PB-FATAL-ERRORS OR                                     EL538
00831            PB-UNFORCED-ERRORS                                     EL538
00832            GO TO 2800-READ-PENDING.                               EL538
00833                                                                   EL538
00834      IF PENDING-WITH-DATE                                         EL538
00835          NEXT SENTENCE                                            EL538
00836      ELSE                                                         EL538
00837          GO TO 2810-CONTINUE-PENDING.                             EL538
00838                                                                   EL538
00839      IF PB-INPUT-DT GREATER THAN SV-RUN-DATE-YMD                  EL538
00840         NEXT SENTENCE                                             EL538
00841      ELSE                                                         EL538
00842         GO TO 2800-READ-PENDING.                                  EL538
00843                                                                   EL538
00844  2810-CONTINUE-PENDING.                                           EL538
00845                                                                   EL538
00846      MOVE SPACES                 TO ALPHA-RECORD.                 EL538
00847      MOVE 'PB'                   TO AX-RECORD-ID.                 EL538
00848      MOVE 'I'                    TO AX-ALPHA-TYPE-CODE.           EL538
00849      MOVE PB-SV-CARRIER          TO AX-CARRIER.                   EL538
00850      MOVE PB-SV-GROUPING         TO AX-GROUPING.                  EL538
00851      MOVE PB-SV-STATE            TO AX-STATE.                     EL538
00852      MOVE PB-ACCOUNT             TO AX-ACCOUNT.                   EL538
00853      MOVE PB-CERT-NO             TO AX-CERT-NO.                   EL538
00854                                                                   EL538
00855      MOVE PB-CERT-EFF-DT         TO DC-BIN-DATE-1.                EL538
00856      MOVE ' '                    TO DC-OPTION-CODE.               EL538
00857                                                                   EL538
00858      PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT.                 EL538
00859                                                                   EL538
00860      IF NO-CONVERSION-ERROR                                       EL538
00861 **       MOVE DC-GREG-DATE-1-YMD TO AX-DT                         EL538
00862 **       MOVE DC-ALPHA-CEN-N     TO AX-CC                         EL538
LGC185         MOVE DC-GREG-DATE-CYMD  TO AX-DT                         EL538
LGC185                                    WS-AX-DT-N                    EL538
00863      ELSE                                                         EL538
00864          MOVE ZEROS              TO AX-DT.                        EL538
00865                                                                   EL538
00866      MOVE PB-I-NAME              TO AX-NAME.                      EL538
00867      MOVE PB-I-AGE               TO AX-AGE.                       EL538
00868      MOVE PB-I-INSURED-SEX       TO AX-SEX.                       EL538
00869      MOVE PB-I-SOC-SEC-NO        TO AX-SOC-NO
022307     MOVE PB-I-SIG-SW            TO AX-IND-GRP
00870                                                                   EL538
00871      MOVE PB-I-LF-BENEFIT-CD     TO AX-LF-TYP.                    EL538
00872      MOVE PB-I-LF-TERM           TO AX-LF-TERM  AX-LF-REMTERM.    EL538
00873      MOVE PB-I-LF-BENEFIT-AMT    TO AX-LF-AMT  AX-LF-REMAMT.      EL538
00874      MOVE PB-I-LF-ALT-BENEFIT-AMT                                 EL538
00875                                  TO AX-LF-AMT-ALT.                EL538
00876      ADD PB-I-LF-ALT-BENEFIT-AMT TO AX-LF-REMAMT.                 EL538
00877      MOVE PB-I-LF-PREMIUM-AMT    TO AX-LF-PRM.                    EL538
00878                                                                   EL538
00879      MOVE PB-I-AH-BENEFIT-CD     TO AX-AH-TYP.                    EL538
00880      MOVE PB-I-AH-TERM           TO AX-AH-TERM  AX-AH-REMTERM.    EL538
00881      MOVE PB-I-AH-BENEFIT-AMT    TO AX-AH-AMT.                    EL538
00882      COMPUTE AX-AH-REMAMT = AX-AH-TERM * AX-AH-AMT.               EL538
00883      MOVE PB-I-AH-PREMIUM-AMT    TO AX-AH-PRM.                    EL538
00884                                                                   EL538
00885      PERFORM 2200-BUILD-SORT-KEY THRU 2299-RELEASE-EXIT.          EL538
00886                                                                   EL538
00887      IF PB-I-JOINT-INSURED EQUAL LOW-VALUES OR SPACES             EL538
00888          GO TO 2800-READ-PENDING.                                 EL538
00889                                                                   EL538
00890      MOVE CLAS-STARTL TO CLAS-INDEXL.                             EL538
00891      MOVE CLAS-STARTA TO CLAS-INDEXA.                             EL538
00892                                                                   EL538
00893      MOVE SPACES TO LF-JOINT-SW, AH-JOINT-SW.                     EL538
00894                                                                   EL538
00895      IF AX-LF-TYP EQUAL ZEROS OR SPACES OR LOW-VALUES             EL538
00896          GO TO 2830-FIND-AH-LOOP.                                 EL538
00897                                                                   EL538
00898   2820-FIND-LIFE-LOOP.                                            EL538
00899                                                                   EL538
00900      IF CLAS-INDEXL GREATER THAN CLAS-MAXL                        EL538
00901          MOVE ' INVALID LIFE TYPE ' TO WS-ABEND-MESSAGE           EL538
00902          MOVE AX-LF-TYP TO WS-ABEND-FILE-STATUS                   EL538
00903          GO TO ABEND-PGM.                                         EL538
00904                                                                   EL538
00905      IF AX-LF-TYP NOT EQUAL CLAS-I-BEN (CLAS-INDEXL)              EL538
00906          ADD +1 TO CLAS-INDEXL                                    EL538
00907          GO TO 2820-FIND-LIFE-LOOP.                               EL538
00908                                                                   EL538
00909      MOVE CLAS-I-JOINT (CLAS-INDEXL) TO LF-JOINT-SW.              EL538
00910                                                                   EL538
00911   2830-FIND-AH-LOOP.                                              EL538
00912                                                                   EL538
00913      IF AX-AH-TYP EQUAL ZEROS OR SPACES OR LOW-VALUES             EL538
00914          GO TO 2840-BUILD-JOINT-ALPHA.                            EL538
00915                                                                   EL538
00916      IF CLAS-INDEXA GREATER THAN CLAS-MAXA                        EL538
00917          MOVE ' INVALID AH TYPE ' TO WS-ABEND-MESSAGE             EL538
00918          MOVE AX-AH-TYP TO WS-ABEND-FILE-STATUS                   EL538
00919          GO TO ABEND-PGM.                                         EL538
00920                                                                   EL538
00921      IF AX-AH-TYP NOT EQUAL CLAS-I-BEN (CLAS-INDEXA)              EL538
00922          ADD +1 TO CLAS-INDEXA                                    EL538
00923          GO TO 2830-FIND-AH-LOOP.                                 EL538
00924                                                                   EL538
00925      MOVE CLAS-I-JOINT (CLAS-INDEXA) TO AH-JOINT-SW.              EL538
00926                                                                   EL538
00927   2840-BUILD-JOINT-ALPHA.                                         EL538
00928                                                                   EL538
00929      IF JOINT-LF-COVERAGE OR JOINT-AH-COVERAGE                    EL538
00930          NEXT SENTENCE                                            EL538
00931      ELSE                                                         EL538
00932          GO TO 2800-READ-PENDING.                                 EL538
00933                                                                   EL538
00934      MOVE 'J'                    TO AX-ALPHA-TYPE-CODE.           EL538
00935      MOVE PB-I-JOINT-INSURED     TO AX-NAME.                      EL538
00936      MOVE PB-I-JOINT-AGE         TO AX-AGE.                       EL538
00937      MOVE SPACES                 TO AX-SEX                        EL538
00938                                     AX-SOC-NO.                    EL538
00939                                                                   EL538
00940      IF NOT JOINT-AH-COVERAGE                                     EL538
00941          MOVE SPACES                 TO AX-AH-TYP                 EL538
00942          MOVE ZEROS                  TO AX-AH-TERM                EL538
00943                                         AX-AH-REMTERM             EL538
00944                                         AX-AH-AMT                 EL538
00945                                         AX-AH-REMAMT              EL538
00946                                         AX-AH-PRM                 EL538
00947                                         AX-AH-REFUND              EL538
00948                                         AX-AH-CLAIM-PMTS.         EL538
00949                                                                   EL538
00950      IF NOT JOINT-LF-COVERAGE                                     EL538
00951          MOVE SPACES                 TO AX-LF-TYP                 EL538
00952          MOVE ZEROS                  TO AX-LF-TERM                EL538
00953                                         AX-LF-REMTERM             EL538
00954                                         AX-LF-AMT                 EL538
00955                                         AX-LF-REMAMT              EL538
00956                                         AX-LF-AMT-ALT             EL538
00957                                         AX-LF-REMAMT-ALT          EL538
00958                                         AX-LF-PRM                 EL538
00959                                         AX-LF-PRM-ALT             EL538
00960                                         AX-LF-REFUND              EL538
00961                                         AX-LF-CLAIM-PMTS.         EL538
00962                                                                   EL538
00963      PERFORM 2200-BUILD-SORT-KEY THRU 2299-RELEASE-EXIT.          EL538
00964                                                                   EL538
00965      GO TO 2800-READ-PENDING.                                     EL538
00966                                                                   EL538
00967  2850-FORMAT-CANCELLATION.                                        EL538
00968                                                                   EL538
00969      IF PB-CI-ENTRY-STATUS = '5'  OR  '9' OR 'D' OR 'V' OR 'U'    EL538
00970          GO TO 2800-READ-PENDING.                                 EL538
00971                                                                   EL538
00972      MOVE SPACES                 TO ALPHA-RECORD.                 EL538
00973      MOVE 'AC'                   TO AX-RECORD-ID.                 EL538
00974      MOVE 'I'                    TO AX-ALPHA-TYPE-CODE.           EL538
00975      MOVE PB-SV-CARRIER          TO AX-CARRIER.                   EL538
00976      MOVE PB-SV-GROUPING         TO AX-GROUPING.                  EL538
00977      MOVE PB-SV-STATE            TO AX-STATE.                     EL538
00978      MOVE PB-ACCOUNT             TO AX-ACCOUNT.                   EL538
00979      MOVE PB-CERT-NO             TO AX-CERT-NO.                   EL538
00980                                                                   EL538
00981      MOVE PB-CERT-EFF-DT         TO DC-BIN-DATE-1.                EL538
00982      MOVE ' '                    TO DC-OPTION-CODE.               EL538
00983                                                                   EL538
00984      PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT.                    CL**5
00985                                                                   EL538
00986      IF NO-CONVERSION-ERROR                                       EL538
00987 **       MOVE DC-GREG-DATE-1-YMD TO AX-DT                         EL538
00988 **       MOVE DC-ALPHA-CEN-N     TO AX-CC                         EL538
LGC185         MOVE DC-GREG-DATE-CYMD  TO AX-DT                         EL538
LGC185                                    WS-AX-DT-N                    EL538
00989      ELSE                                                         EL538
00990          MOVE ZEROS              TO AX-DT.                        EL538
00991                                                                   EL538
00992      MOVE PB-CI-LAST-NAME        TO AX-LNAME.                     EL538
00993      MOVE SPACES                 TO AX-INIT                       EL538
00994                                     AX-FNAME.                     EL538
00995                                                                   EL538
00996      MOVE PB-CI-INSURED-AGE      TO AX-AGE.                       EL538
00997      MOVE PB-CI-INSURED-SEX      TO AX-SEX.                       EL538
00998      MOVE PB-CI-SOC-SEC-NO       TO AX-SOC-NO.                    EL538
00999                                                                   EL538
01000      IF PB-CI-LF-BENEFIT-CD EQUAL ZEROS OR SPACES OR LOW-VALUES   EL538
01001          MOVE SPACES             TO AX-LF-TYP                     EL538
01002          MOVE ZEROS              TO AX-LF-TERM                    EL538
01003                                     AX-LF-REMTERM                 EL538
01004                                     AX-LF-AMT                     EL538
01005                                     AX-LF-REMAMT                  EL538
01006                                     AX-LF-AMT-ALT                 EL538
01007                                     AX-LF-REMAMT-ALT              EL538
01008                                     AX-LF-PRM                     EL538
01009                                     AX-LF-PRM-ALT                 EL538
01010                                     AX-LF-REFUND                  EL538
01011                                     AX-LF-CLAIM-PMTS              EL538
01012          GO TO 2860-FORMAT-AH.                                    EL538
01013                                                                   EL538
01014      MOVE PB-CI-LF-BENEFIT-CD    TO AX-LF-TYP.                    EL538
01015      MOVE PB-CI-LF-TERM          TO AX-LF-TERM.                   EL538
01016                                                                   EL538
01017      MOVE PB-C-LF-REM-TERM       TO AX-LF-REMTERM.                EL538
01018                                                                   EL538
01019      COMPUTE  AX-LF-AMT = PB-CI-LF-BENEFIT-AMT * -1.              EL538
01020                                                                   EL538
01021      COMPUTE  AX-LF-REMAMT =                                      EL538
01022          (PB-CI-LF-BENEFIT-AMT + PB-CI-LF-ALT-BENEFIT-AMT) * -1.  EL538
01023                                                                   EL538
01024      COMPUTE AX-LF-AMT-ALT = PB-CI-LF-ALT-BENEFIT-AMT * -1.       EL538
01025                                                                   EL538
01026      COMPUTE AX-LF-PRM = (PB-CI-LF-PREMIUM-AMT  +                 EL538
01027                           PB-CI-LF-ALT-PREMIUM-AMT) * -1.         EL538
01028                                                                   EL538
01029  2860-FORMAT-AH.                                                  EL538
01030                                                                   EL538
01031      IF PB-CI-AH-BENEFIT-CD EQUAL ZEROS OR SPACES OR LOW-VALUES   EL538
01032          MOVE SPACES             TO AX-AH-TYP                     EL538
01033          MOVE ZEROS              TO AX-AH-TERM                    EL538
01034                                     AX-AH-REMTERM                 EL538
01035                                     AX-AH-AMT                     EL538
01036                                     AX-AH-REMAMT                  EL538
01037                                     AX-AH-PRM                     EL538
01038                                     AX-AH-REFUND                  EL538
01039                                     AX-AH-CLAIM-PMTS              EL538
01040          GO TO 2870-CONTINUE.                                     EL538
01041                                                                   EL538
01042      MOVE PB-CI-AH-BENEFIT-CD    TO AX-AH-TYP.                    EL538
01043      MOVE PB-CI-AH-TERM          TO AX-AH-TERM.                   EL538
01044      MOVE PB-C-AH-REM-TERM       TO AX-AH-REMTERM.                EL538
01045      COMPUTE AX-AH-AMT = PB-CI-AH-BENEFIT-AMT * -1.               EL538
01046      COMPUTE AX-AH-REMAMT = (AX-AH-REMTERM * AX-AH-AMT) * -1.     EL538
01047      COMPUTE AX-AH-PRM = PB-CI-AH-PREMIUM-AMT * -1.               EL538
01048                                                                   EL538
01049  2870-CONTINUE.                                                   EL538
01050                                                                   EL538
01051      PERFORM 2200-BUILD-SORT-KEY THRU 2299-RELEASE-EXIT.          EL538
01052                                                                   EL538
01053      GO TO 2800-READ-PENDING.                                     EL538
01054                                                                   EL538
01055  2900-CLOSE-PENDING.                                              EL538
01056      CLOSE ERPNDB.                                                EL538
01057                                                                   EL538
01058      IF ERPNDB-FILE-STATUS NOT = ZERO                             EL538
01059          MOVE 'ERROR OCCURED CLOSE - ERPNDB'                      EL538
01060                                  TO  WS-ABEND-MESSAGE             EL538
01061          MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL538
01062          GO TO ABEND-PGM.                                         EL538
01063                                                                   EL538
01064  SORT-ALPHA-EXTRACTS-EXIT.                                        EL538
01065      EXIT.                                                        EL538
01066  EJECT                                                            EL538
01067  LIMITS-EXCEEDED-REPORT SECTION.                                  EL538
DAN                                                                     EL538
DAN    3000-OPEN-ELMSTR5.                                               EL538
DAN        OPEN INPUT ELMSTR5.                                          EL538
DAN                                                                     EL538
DAN        IF ELMSTR5-FILE-STATUS  = '00' OR '97'                       EL538
DAN            NEXT SENTENCE                                            EL538
DAN          ELSE                                                       EL538
DAN            MOVE 'ERROR OCCURED OPEN - ELMSTR5'                      EL538
DAN                                    TO  WS-ABEND-MESSAGE             EL538
DAN            MOVE ELMSTR5-FILE-STATUS TO  WS-ABEND-FILE-STATUS        EL538
DAN            GO TO ABEND-PGM.                                         EL538
01068                                                                   EL538
01069  3000-RETURN-SORTED-ALPHA.                                        EL538
01070      RETURN SORTED-TRANS INTO ALPHA-RECORD                        EL538
01071                   AT END PERFORM 3100-CONTROL-BREAK THRU          EL538
01072                                  3999-CONTROL-BREAK-EXIT          EL538
01073                          GO TO LIMITS-EXCEEDED-REPORT-EXIT.       EL538
01074                                                                   EL538
01075      COPY ELCAEXM1.                                               EL538
01076                                                                   EL538
01077      IF FIRST-RECORD                                              EL538
01078          MOVE 'X'                TO FIRST-SWITCH                  EL538
01079          MOVE +0                 TO MATCH-COUNT                   EL538
01080          MOVE S-CURRENT-CONTROL  TO PRIOR-CONTROL.                EL538
01081                                                                   EL538
01082      IF AX-RECORD-ID NOT EQUAL 'AC'                               EL538
01083          MOVE +1 TO SNDX                                          EL538
01084          GO TO 3010-CONTINUE.                                     EL538
01085                                                                   EL538
01086      ADD +1 TO CNDX.                                              EL538
01087      IF CNDX GREATER THAN +500                                    EL538
01088          MOVE +1 TO CNDX.                                         EL538
01089                                                                   EL538
01090      MOVE AX-CONTROL  TO SV-PC-CONTROL (CNDX).                    EL538
01091      ADD 1 TO DROP-COUNT.                                         EL538
01092      GO TO 3000-RETURN-SORTED-ALPHA.                              EL538
01093                                                                   EL538
01094  3010-CONTINUE.                                                   EL538
01095                                                                   EL538
01096      IF AX-CONTROL = SV-PC-CONTROL (SNDX)                         EL538
01097          ADD 1 TO CANCEL-COUNT                                    EL538
01098          GO TO 3000-RETURN-SORTED-ALPHA.                          EL538
01099                                                                   EL538
01100      ADD +1 TO SNDX.                                              EL538
01101      IF SNDX NOT GREATER THAN +500                                EL538
01102          GO TO 3010-CONTINUE.                                     EL538
01103                                                                   EL538
01104      IF AX-JOINT-ALPHA                                            EL538
01105         IF (AX-CONTROL EQUAL SV-JT-CONTROL) AND                   EL538
01106            (AX-LNAME EQUAL SV-JT-LNAME)  AND                      EL538
01107            (AX-1ST-INIT-FNAME EQUAL SV-JT-FINIT)                  EL538
01108             GO TO 3000-RETURN-SORTED-ALPHA.                       EL538
01109                                                                   EL538
01110      IF (AX-LNAME NOT EQUAL SV-JT-LNAME) OR                       EL538
01111         (AX-1ST-INIT-FNAME NOT EQUAL SV-JT-FINIT) OR              EL538
01112         (AX-CONTROL NOT EQUAL SV-JT-CONTROL)                      EL538
01113           MOVE AX-LNAME TO SV-JT-LNAME                            EL538
01114           MOVE AX-1ST-INIT-FNAME TO SV-JT-FINIT                   EL538
01115           MOVE AX-CONTROL TO SV-JT-CONTROL.                       EL538
01116                                                                   EL538
01117      ADD +1                      TO X1.                           EL538
01118                                                                   EL538
01119      IF (S-CURRENT-CONTROL NOT = PRIOR-CONTROL)
              OR (X1 GREATER THAN +400)
01121          PERFORM 3100-CONTROL-BREAK
                                       THRU 3999-CONTROL-BREAK-EXIT
01122          MOVE +0                 TO MATCH-COUNT
           END-IF
01123                                                                   EL538
01124      ADD +1                     TO MATCH-COUNT.                   EL538
01125                                                                   EL538
01126      COPY ELCAEXM2.                                               EL538
01127                                                                   EL538
01128      MOVE ALPHA-RECORD          TO SV-RECS (X1).                  EL538
01129                                                                   EL538
01130      IF AX-RECORD-ID = 'PB' OR 'AC'                               EL538
01131          MOVE 'X' TO PENDING-CERT-SWITCH.                         EL538
01132                                                                   EL538
01133      IF AX-AGE GREATER SV-HI-AGE                                  EL538
01134          MOVE AX-AGE TO SV-HI-AGE.                                EL538
01135                                                                   EL538
110604     IF AX-LF-TYP NOT = '  ' AND '00'
01136         ADD AX-LF-REMAMT         TO SV-REM-AMTL
110604     END-IF

110604     IF AX-AH-TYP NOT = '  ' AND '00'
01137         ADD AX-AH-AMT            TO SV-AH-BEN
01138         ADD AX-AH-REMAMT         TO SV-REM-AMTA
110604     END-IF
01139                                                                   EL538
01140      GO TO 3000-RETURN-SORTED-ALPHA.                              EL538
01141                                                                   EL538
01142  3100-CONTROL-BREAK.                                              EL538
01143                                                                   EL538
01144      IF DTE-CLIENT EQUAL 'LBL'                                    EL538
01145          IF MATCH-COUNT NOT GREATER THAN +1                       EL538
01146              GO TO 3900-INITIALIZE-NEW-CONTROL.                   EL538
01147                                                                   EL538
01148      IF PRIOR-STATE = LAST-STATE                                  EL538
01149          GO TO 3400-CHECK-THE-LIMITS.                             EL538
01150                                                                   EL538
01151      MOVE PRIOR-STATE            TO LAST-STATE.                   EL538
01152      MOVE +80                    TO LN-CT.                        EL538
01153      MOVE +1                     TO X3.                           EL538
01154      MOVE +0                     TO SUBA.                         EL538
01155                                                                   EL538
01156  3200-FIND-STATE-LIMITS.                                          EL538
01157      IF PRIOR-STATE = LIMIT-STATE (X3)                            EL538
01158          GO TO 3300-SET-LIMIT-HEADINGS.                           EL538
01159                                                                   EL538
01160      IF LIMIT-STATE (X3) = '**'                                   EL538
01161         IF PRIOR-STATE NOT = DEFAULT-ST                           EL538
01162             MOVE DEFAULT-ST      TO PRIOR-STATE                   EL538
01163             MOVE +0              TO X3                            EL538
01164         ELSE                                                      EL538
01165             GO TO 3400-CHECK-THE-LIMITS.                          EL538
01166                                                                   EL538
01167      ADD +1                      TO X3.                           EL538
01168                                                                   EL538
01169      GO TO 3200-FIND-STATE-LIMITS.                                EL538
01170                                                                   EL538
01171  3300-SET-LIMIT-HEADINGS.                                         EL538
01172      ADD +1                      TO SUBA.                         EL538
01173                                                                   EL538
01174      IF LC-AGE  (X3 SUBA) = ZEROS AND                             EL538
01175         LC-LIFE (X3 SUBA) = ZEROS AND                             EL538
01176         LC-AH   (X3 SUBA) = ZEROS AND                             EL538
01177         LC-AHMX (X3 SUBA) = ZEROS                                 EL538
01178          GO TO 3400-CHECK-THE-LIMITS.                             EL538
01179                                                                   EL538
01180      MOVE LC-AGE (X3 SUBA)       TO HD-AGE.                       EL538
01181      MOVE LC-LIFE (X3 SUBA)      TO HD-TTL.                       EL538
01182      MOVE LC-AH (X3 SUBA)        TO HD-MO.                        EL538
01183                                                                   EL538
01184      IF LC-AHMX (X3 SUBA) GREATER THAN +1000                      EL538
01185          MOVE LC-AHMX (X3 SUBA)  TO HD-TTA                        EL538
01186      ELSE                                                         EL538
01187          MOVE LC-LIFE (X3 SUBA)  TO HD-TTA                        EL538
01188          MOVE LC-LIFE (X3 SUBA)  TO LC-AHMX (X3 SUBA).            EL538
01189                                                                   EL538
01190      IF DTE-CLIENT = 'FLI'  OR  'FLU'                             EL538
01191          IF SUBA = +3                                             EL538
01192              MOVE ZEROS TO LC-AHMX (X3 SUBA)  HD-TTA.             EL538
01193                                                                   EL538
01194      IF SUBA = +1                                                 EL538
01195          MOVE LIMITS-HD          TO HD2T.                         EL538
01196                                                                   EL538
01197      IF SUBA = +2                                                 EL538
01198          MOVE LIMITS-HD          TO HD3T, HD3T-D.                 EL538
01199                                                                   EL538
01200      IF SUBA = +3                                                 EL538
01201          MOVE LIMITS-HD          TO HD3AT                         EL538
01202      ELSE                                                         EL538
01203          GO TO 3300-SET-LIMIT-HEADINGS.                           EL538
01204                                                                   EL538
01205  3400-CHECK-THE-LIMITS.                                           EL538
01206      MOVE +1                     TO SUBA.                         EL538
01207      IF LC-AGE (X3 SUBA) = ZERO                                   EL538
01208          GO TO 3600-CHECK-AMOUNT-LIMIT.                           EL538
01209                                                                   EL538
01210  3500-CHECK-AGE-LIMIT.                                            EL538

01211      IF SV-HI-AGE NOT GREATER THAN LC-AGE (X3 SUBA)               EL538
01212          GO TO 3600-CHECK-AMOUNT-LIMIT.                           EL538
01213                                                                   EL538
01214      ADD +1                      TO SUBA.                         EL538
01215                                                                   EL538
01216      IF SUBA GREATER THAN +3                                      EL538
01217          SUBTRACT +1 FROM SUBA                                    EL538
01218          GO TO 3600-CHECK-AMOUNT-LIMIT                            EL538
01219      ELSE                                                         EL538
01220          IF LC-AGE (X3 SUBA) = ZERO                               EL538
01221              SUBTRACT +1 FROM SUBA                                EL538
01222              GO TO 3600-CHECK-AMOUNT-LIMIT.                       EL538
01223                                                                   EL538
01224      GO TO 3500-CHECK-AGE-LIMIT.                                  EL538
01225                                                                   EL538
01226  3600-CHECK-AMOUNT-LIMIT.                                         EL538

01227      MOVE SPACES                 TO LIMIT-SWITCH                  EL538
01228                                     HD7.                          EL538
01229                                                                   EL538
01230      IF SV-REM-AMTL GREATER THAN LC-LIFE (X3 SUBA)                EL538
01231          MOVE 'X'                TO LIMIT-SWITCH                  EL538
01232          MOVE MESS1              TO L-DESC
           END-IF
01233      IF SV-AH-BEN GREATER THAN LC-AH (X3 SUBA)                    EL538
01234          MOVE 'X'                TO LIMIT-SWITCH                  EL538
01235          MOVE MESS2              TO A-DESA
           END-IF
01236      IF SV-REM-AMTA GREATER THAN LC-AHMX (X3 SUBA)                EL538
01237          MOVE 'X'                TO LIMIT-SWITCH                  EL538
01238          MOVE MESS3              TO A-DESB
           END-IF
01239                                                                   EL538
01240      IF LIMITS-EXCEEDED                                           EL538
01241          IF (PENDING-ONLY-OPTION  AND                             EL538
01242              NO-PENDING-CERTS-USED)                               EL538
01243                 GO TO 3900-INITIALIZE-NEW-CONTROL                 EL538
01244          ELSE                                                     EL538
01245             IF (PENDING-WITH-DATE    AND                          EL538
01246                 NO-PENDING-CERTS-USED)                            EL538
01247                 GO TO 3900-INITIALIZE-NEW-CONTROL                 EL538
01248              ELSE                                                 EL538
01249                 NEXT SENTENCE                                     EL538
01250      ELSE                                                         EL538
01251          GO TO 3900-INITIALIZE-NEW-CONTROL.                       EL538
01252                                                                   EL538
01253      COMPUTE X2 = MAX-LN - 2 - X1.                                EL538
01254      IF LN-CT GREATER X2                                          EL538
01255          PERFORM 6000-PT-HDNG THRU 6999-PT-HDNG-EXIT.             EL538
01256                                                                   EL538
01257      MOVE HIGH-VALUES            TO SV-RECS (X1).                 EL538
01258      MOVE HD7                    TO HD6-SUB.                      EL538
01259      MOVE SV-RECS (1)            TO ALPHA-RECORD.                 EL538
01260      COPY ELCAEXM1.                                               EL538
01261      IF LIMIT-OPTION = 'SS' OR 'AS'
01262          MOVE AX-SOC-NO          TO HD-SOC.                       EL538
01263      MOVE '0'                    TO P-CTL.                           CL**2
01264      MOVE HD6                    TO P-DATA.                       EL538
01265      PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT.                    EL538
01266      ADD +1                      TO LN-CT.                        EL538
01267                                                                   EL538
01268      MOVE +1                     TO X1.                           EL538
01269                                                                   EL538
01270  3700-PRINT-DETAIL-LINE.                                          EL538

01271      IF SV-RECS (X1) = HIGH-VALUES                                EL538
01272          GO TO 3800-PRINT-TOTALS.                                 EL538
01273                                                                   EL538
01274      IF LN-CT GREATER MAX-LN                                      EL538
01275          PERFORM 6000-PT-HDNG THRU 6999-PT-HDNG-EXIT.             EL538
01276                                                                   EL538
01277      MOVE SV-RECS (X1)           TO ALPHA-RECORD.                 EL538
01278                                                                   EL538
01279      COPY ELCAEXM1.                                               EL538
01280                                                                   EL538
01281      MOVE SPACES TO DTL.                                          EL538
01282                                                                   EL538
01283      IF X1 = +1                                                   EL538
01284          OR (AX-CARRIER NOT = P-CARR)                             EL538
01285          OR (AX-GROUPING NOT = P-GROUPING)                        EL538
01286          OR (AX-STATE NOT = P-ST)                                 EL538
01287          OR (AX-ACCOUNT NOT = P-ACCT)                             EL538
01288          OR (AX-LNAME NOT = P-NAME)                               EL538
01289              MOVE AX-CARRIER     TO P-CARR                        EL538
01290              MOVE AX-GROUPING    TO P-GROUPING                    EL538
01291              MOVE AX-STATE       TO P-ST                          EL538
01292              MOVE AX-ACCOUNT     TO P-ACCT                        EL538
01293              MOVE AX-LNAME       TO P-NAME                        EL538
01294              MOVE AX-FNAME       TO P-1ST-NAME                    EL538
01295              MOVE AX-INIT        TO P-MID-INIT.                   EL538
01296                                                                   EL538
01297      IF AX-JOINT-ALPHA                                            EL538
01298          MOVE '*'                TO P-JOINT-IND.                  EL538
01299      MOVE AX-CERT-NO             TO P-CERT.                       EL538
01300      MOVE AX-MO                  TO P-EMO.                        EL538
01301      MOVE AX-DA                  TO P-EDA.                        EL538
01302      MOVE AX-YR                  TO P-EYR.                        EL538
01303      MOVE AX-AGE                 TO P-AGE
031811     MOVE AX-SOC-NO              TO P-SSN
031811     IF P-SSN = '   -  -    '
031811        MOVE SPACES              TO P-SSN
031811     END-IF
01304                                                                   EL538
01305      IF AX-LF-TYP NOT = ZEROS                                     EL538
01306          MOVE AX-LF-TERM         TO P-L-TERM                      EL538
01307          COMPUTE COMBINE-LF-AMT = AX-LF-AMT + AX-LF-AMT-ALT       EL538
01308          MOVE COMBINE-LF-AMT     TO P-L-FACE                      EL538
01309          MOVE AX-LF-REMTERM      TO P-L-REMTERM                   EL538
01310          MOVE AX-LF-REMAMT       TO P-L-REMFACE                   EL538
01311          PERFORM 4000-LIFE-CLAS-LOOK-RTN THRU                     EL538
01312                  4999-LIFE-CLAS-LOOK-RTN-X                        EL538
01313          IF CLAS-INDEXL GREATER THAN CLAS-MAXL                    EL538
01314              MOVE AX-LF-TYP      TO P-L-TYPE                      EL538
01315          ELSE                                                     EL538
01316              MOVE CLAS-I-AB3 (CLAS-INDEXL) TO P-L-TYPE.           EL538
01317                                                                   EL538
01318      IF AX-AH-TYP NOT = ZEROS                                     EL538
01319          MOVE AX-AH-TERM         TO P-A-TERM                      EL538
01320          MOVE AX-AH-AMT          TO P-A-BEN                       EL538
01321          MOVE AX-AH-REMTERM      TO P-A-REMTERM                   EL538
01322          MOVE AX-AH-REMAMT       TO P-A-REMBEN                    EL538
01323          PERFORM 5000-AH-CLAS-LOOK-RTN THRU                       EL538
01324                  5999-AH-CLAS-LOOK-RTN-X                          EL538
01325          IF CLAS-INDEXA GREATER THAN CLAS-MAXA                    EL538
01326              MOVE AX-AH-TYP      TO P-A-TYPE                      EL538
01327          ELSE                                                     EL538
01328              MOVE CLAS-I-AB3 (CLAS-INDEXA) TO P-A-TYPE.           EL538
01329                                                                   EL538
01330      IF AX-RECORD-ID = 'PB' OR 'AC'                               EL538
01331         MOVE 'PB '               TO P-ID
           END-IF

01332      IF AX-RECORD-ID = 'OE'
01333         MOVE 'OE '               TO P-ID
           END-IF

022307*   FYI - THE SIGNATURE SWITCH WAS PASSED IN THE IND-GRP FIELD
022307     IF AX-RECORD-ID = 'PB'
022307        IF AX-IND-GRP = 'Y'
022307           MOVE 'Y'              TO P-ID (3:1)
022307        END-IF
022307     END-IF

DAN        PERFORM 9000-FIND-OPEN-CLAIM THRU 9000-EXIT                  EL538
DAN        IF OPEN-CLAIM-FOUND                                          EL538
DAN           MOVE '*' TO P-OPEN-FLAG                                   EL538
DAN        ELSE                                                         EL538
DAN           MOVE ' ' TO P-OPEN-FLAG.                                  EL538
DAN                                                                     EL538
01335      MOVE DTL                    TO P-DATA.                       EL538
01336      MOVE ' '                    TO P-CTL.                           CL**2
01337      PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT.                    EL538
01338                                                                   EL538
01339      ADD +1                      TO X1.                           EL538
01340                                                                   EL538
01341      GO TO 3700-PRINT-DETAIL-LINE.                                EL538
01342                                                                   EL538
01343  3800-PRINT-TOTALS.                                               EL538
01344      MOVE SPACES                 TO DTL.                          EL538
01345      MOVE 'TOTAL '               TO P-TTL.                        EL538
01346      MOVE SV-REM-AMTL            TO P-L-REMFACE.                  EL538
01347      MOVE SV-AH-BEN              TO P-A-BEN.                      EL538
01348      MOVE SV-REM-AMTA            TO P-A-REMBEN.                   EL538
01349      MOVE DTL                    TO P-DATA.                       EL538
01350      MOVE ' '                    TO P-CTL.                           CL**2
01351      IF X1 NOT = +2                                               EL538
01352          PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT.                EL538
01353                                                                   EL538
01354  3900-INITIALIZE-NEW-CONTROL.                                     EL538
01355      MOVE S-CURRENT-CONTROL      TO PRIOR-CONTROL.                EL538
01356      MOVE SORT-ALPHA-EXTRACT     TO ALPHA-RECORD.                 EL538
01357      COPY ELCAEXM1.                                               EL538
01358      MOVE +1                     TO X1.                           EL538
01359      MOVE SPACES                 TO STORE-AX-RECORDS              EL538
01360                                     PENDING-CERT-SWITCH.          EL538
01361      MOVE ZEROS                  TO SV-HI-AGE                     EL538
01362                                     SV-REM-AMTL                   EL538
01363                                     SV-AH-BEN                     EL538
01364                                     SV-REM-AMTA.                  EL538
01365                                                                   EL538
01366  3999-CONTROL-BREAK-EXIT.                                         EL538
01367      EXIT.                                                        EL538
01368                                                                   EL538
01369                                                                   EL538
01370  LIMITS-EXCEEDED-REPORT-EXIT.                                     EL538
01371      EXIT.                                                        EL538
01372  EJECT                                                            EL538
01373  PERFORMED-ROUTINES SECTION.                                      EL538
01374                                                                   EL538
01375  4000-LIFE-CLAS-LOOK-RTN.                                         EL538
01376      MOVE CLAS-STARTL            TO CLAS-INDEXL.                  EL538
01377                                                                   EL538
01378  4500-LIFE-CLAS-LOOP.                                             EL538
01379      IF CLAS-INDEXL GREATER CLAS-MAXL  OR                         EL538
01380         CLAS-STARTL = ZEROS                                       EL538
01381          COMPUTE CLAS-INDEXL = CLAS-MAXL + 1                      EL538
01382          GO TO 4999-LIFE-CLAS-LOOK-RTN-X.                         EL538
01383                                                                   EL538
01384      IF AX-LF-TYP = CLAS-I-BEN (CLAS-INDEXL)                      EL538
01385          GO TO 4999-LIFE-CLAS-LOOK-RTN-X.                         EL538
01386                                                                   EL538
01387      ADD +1 TO CLAS-INDEXL.                                       EL538
01388                                                                   EL538
01389      GO TO 4500-LIFE-CLAS-LOOP.                                   EL538
01390                                                                   EL538
01391  4999-LIFE-CLAS-LOOK-RTN-X.                                       EL538
01392      EXIT.                                                        EL538
01393                                                                   EL538
01394  5000-AH-CLAS-LOOK-RTN.                                           EL538
01395      MOVE CLAS-STARTA            TO CLAS-INDEXA.                  EL538
01396                                                                   EL538
01397  5500-AH-CLAS-LOOP.                                               EL538
01398      IF CLAS-INDEXA GREATER CLAS-MAXA  OR                         EL538
01399         CLAS-STARTA = ZEROS                                       EL538
01400          COMPUTE CLAS-INDEXA = CLAS-MAXA + 1                      EL538
01401          GO TO 5999-AH-CLAS-LOOK-RTN-X.                           EL538
01402                                                                   EL538
01403      IF AX-AH-TYP = CLAS-I-BEN (CLAS-INDEXA)                      EL538
01404          GO TO 5999-AH-CLAS-LOOK-RTN-X.                           EL538
01405                                                                   EL538
01406      ADD +1 TO CLAS-INDEXA.                                       EL538
01407                                                                   EL538
01408      GO TO 5500-AH-CLAS-LOOP.                                     EL538
01409                                                                   EL538
01410  5999-AH-CLAS-LOOK-RTN-X.                                         EL538
01411      EXIT.                                                        EL538
01412                                                                   EL538
01413  6000-PT-HDNG.                                                    EL538
01414      ADD +1                      TO PG-NO.                        EL538
01415      MOVE PG-NO                  TO HD-PG, HD-PG-D.               EL538
031811     MOVE S-CURRENT-CONTROL (1:2) TO HD1-STATE
031811     IF HD1-STATE = '  '
031811        MOVE 'ZZ' TO HD1-STATE
031811     END-IF
01416      MOVE HD1                    TO P-DATA.                       EL538
01417      MOVE '1'                    TO P-CTL.                           CL**2
01418      PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT.                    EL538
01419      MOVE HD2                    TO P-DATA.                       EL538
01420      MOVE ' '                    TO P-CTL.                           CL**2
01421      PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT.                    EL538
01422                                                                   EL538
01423      IF PENDING-WITH-DATE                                         EL538
01424          MOVE HD3-D              TO P-DATA                        EL538
01425      ELSE                                                         EL538
01426          MOVE HD3                TO P-DATA.                       EL538
01427                                                                   EL538
01428      MOVE ' '                    TO P-CTL.                           CL**2
01429      PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT.                    EL538
01430      MOVE HD3A                   TO P-DATA.                       EL538
01431      MOVE ' '                    TO P-CTL.                           CL**2
01432      PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT.                    EL538
01433      MOVE HD4-HDR                TO P-DATA.                       EL538
01434      MOVE ' '                    TO P-CTL.                           CL**2
01435      PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT.                    EL538
01436      MOVE HD4                    TO P-DATA.                       EL538
01437      MOVE ' '                    TO P-CTL.                           CL**2
01438      PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT.                    EL538
01439      MOVE HD5                    TO P-DATA.                       EL538
01440      MOVE ' '                    TO P-CTL.                           CL**2
01441      PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT.                    EL538
01442      MOVE SPACES                 TO P-DATA.                       EL538
01443      MOVE ' '                    TO P-CTL.                           CL**2
01444      PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT.                    EL538
01445      MOVE +8                     TO LN-CT.                        EL538
01446      MOVE SPACES                 TO PRT.                          EL538
01447                                                                   EL538
01448  6999-PT-HDNG-EXIT.                                               EL538
01449      EXIT.                                                        EL538
01450                                                                   EL538
01451  7000-PRT-RTN.                                                    EL538
01452      MOVE P-CTL TO X.                                             EL538
01453                                                                   EL538
01454  7500-PRT-RTN-GO.                                                 EL538
01455      ADD +1                      TO LN-CT.                        EL538
01456                                                                   EL538
01457      MOVE SPACE TO DTE-FICH.                                      EL538
01458                              COPY ELCPRT2X.                       EL538
01459                                                                   EL538
01460  8000-PRINT-LIMITS-TABLE.                                         EL538
01461      IF LIMIT-OPTION = 'AN'                                       EL538
01462          MOVE '(BY ACCOUNT, LAST NAME & INITIALS)' TO HD-TYPE.    EL538
01463                                                                   EL538
031811     IF LIMIT-OPTION = 'AL'
031811         MOVE '(BY ACCOUNT, LAST NAME & 1ST INIT)' TO HD-TYPE
031811     END-IF
01463                                                                   EL538
01464      IF LIMIT-OPTION = 'AS'                                       EL538
01465          MOVE HD6-SOC TO HD-6A                                    EL538
01466          MOVE '(BY ACCOUNT,SOC.SEC.NUMBER)' TO HD-TYPE.           EL538
01467                                                                   EL538
01468      IF LIMIT-OPTION = 'CN'                                       EL538
01469          MOVE '(BY COMPANY, LAST NAME)' TO HD-TYPE.               EL538
01470                                                                   EL538
01471      IF LIMIT-OPTION = 'NI'                                       EL538
01472          MOVE '(BY LAST NAME AND INITIALS)' TO HD-TYPE.           EL538
01473                                                                   EL538
01474      IF LIMIT-OPTION = 'SN'                                       EL538
01475          MOVE '(BY CARRIER,STATE,LAST NAME,1ST INIT)' TO HD-TYPE. EL538
01476                                                                   EL538
01477      IF LIMIT-OPTION = 'SF'                                       EL538
01478          MOVE '(BY STATE, ACCOUNT, LAST NAME, 1ST INIT)'          EL538
01479                                                       TO HD-TYPE. EL538
01480                                                                   EL538
01481      IF LIMIT-OPTION = 'SS'                                       EL538
01482          MOVE HD6-SOC TO HD-6A                                    EL538
01483          MOVE '(BY SOCIAL SECURITY NUMBER)' TO HD-TYPE.           EL538
01484                                                                   EL538
01485      MOVE +1                     TO X3.                           EL538
01486                                                                   EL538
01487  8500-DATE-CONVERSION.                                            EL538
01488      COPY ELCDCS.                                                 EL538
01489                                                                   EL538
01490                                                                   EL538
01491  8500-PRINT-LIMITS-DETAIL.                                        EL538
01492      IF LIMIT-STATE (X3) = '**'                                   EL538
01493          GO TO 8999-PRINT-LIMITS-EXIT.                            EL538
01494                                                                   EL538
01495      MOVE LIMIT-STATE (X3)       TO HD-10-STATE.                  EL538
01496                                                                   EL538
01497      MOVE LC-AGE (X3 1)          TO HD-10-AGE-1.                  EL538
01498      MOVE LC-LIFE (X3 1)         TO HD-10-LIFE-1.                 EL538
01499      MOVE LC-AH (X3 1)           TO HD-10-AH-1.                   EL538
01500      MOVE LC-AHMX (X3 1)         TO HD-10-AHMX-1.                 EL538
01501                                                                   EL538
01502      MOVE LC-AGE (X3 2)          TO HD-10-AGE-2.                  EL538
01503      MOVE LC-LIFE (X3 2)         TO HD-10-LIFE-2.                 EL538
01504      MOVE LC-AH (X3 2)           TO HD-10-AH-2.                   EL538
01505      MOVE LC-AHMX (X3 2)         TO HD-10-AHMX-2.                 EL538
01506                                                                   EL538
01507      MOVE LC-AGE (X3 3)          TO HD-10-AGE-3.                  EL538
01508      MOVE LC-LIFE (X3 3)         TO HD-10-LIFE-3.                 EL538
01509      MOVE LC-AH (X3 3)           TO HD-10-AH-3.                   EL538
01510      MOVE LC-AHMX (X3 3)         TO HD-10-AHMX-3.                 EL538
01511                                                                   EL538
01512      IF LN-CT GREATER THAN +56                                    EL538
031811         MOVE 'ZZ'               TO HD1-STATE
01513          MOVE HD1                TO P-DATA                        EL538
01514          MOVE '1'                TO P-CTL                            CL**2
01515          PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT                 EL538
01516          MOVE HD2                TO P-DATA                        EL538
01517          MOVE ' '                TO P-CTL                            CL**2
01518          PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT                 EL538
01519          MOVE TABLE-HD           TO P-DATA                        EL538
01520          MOVE ' '                TO P-CTL                            CL**2
01521          PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT                 EL538
01522          IF PENDING-WITH-DATE                                     EL538
01523              MOVE 1                  TO HD-PG-D                   EL538
01524              MOVE HD3-D              TO P-DATA                    EL538
01525              MOVE ' '                TO P-CTL                        CL**2
01526              PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT             EL538
01527              MOVE HD8                TO P-DATA                    EL538
01528              MOVE '0'                TO P-CTL                        CL**2
01529              PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT             EL538
01530              MOVE HD8A               TO P-DATA                    EL538
01531              MOVE ' '                TO P-CTL                        CL**2
01532              PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT             EL538
01533              MOVE HD9                TO P-DATA                    EL538
01534              MOVE ' '                TO P-CTL                        CL**2
01535              PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT             EL538
01536              MOVE +8                 TO LN-CT                     EL538
01537          ELSE                                                     EL538
01538              MOVE 1                  TO HD-PG                     EL538
01539              MOVE HD3                TO P-DATA                    EL538
01540              MOVE ' '                TO P-CTL                        CL**2
01541              PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT             EL538
01542              MOVE HD8                TO P-DATA                    EL538
01543              MOVE '0'                TO P-CTL                        CL**2
01544              PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT             EL538
01545              MOVE HD8A               TO P-DATA                    EL538
01546              MOVE ' '                TO P-CTL                        CL**2
01547              PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT             EL538
01548              MOVE HD9                TO P-DATA                    EL538
01549              MOVE ' '                TO P-CTL                        CL**2
01550              PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT             EL538
01551              MOVE +8                 TO LN-CT.                    EL538
01552                                                                   EL538
01553      MOVE HD10                   TO P-DATA.                       EL538
01554      MOVE '0'                    TO P-CTL.                           CL**2
01555      PERFORM 7000-PRT-RTN THRU DTE-PRINT-EXIT.                    EL538
01556      ADD +1                      TO LN-CT.                        EL538
01557                                                                   EL538
01558      ADD +1                      TO X3.                           EL538
01559                                                                   EL538
01560      GO TO 8500-PRINT-LIMITS-DETAIL.                              EL538
01561                                                                   EL538
01562  8999-PRINT-LIMITS-EXIT.                                          EL538
01563      EXIT.                                                        EL538
DAN        EJECT                                                        EL331
DAN   *****************************************************             EL331
DAN   *  9000-FIND-OPEN-CLAIM                             *             EL331
DAN   *****************************************************             EL331
DAN   *  THIS ROUTINE WILL BROWSE THE CLAIM MASTER FILE   *             EL331
DAN   *  THRU THE CERTIFICATE ALTERNATE INDEX.  IF ANY    *             EL331
DAN   *  OPEN CLAIM IS FOUND, THE OPEN-CLAIM-SW IS SET TO *             EL331
DAN   *  'YES' AND THE ROUTINE IS EXITED.                 *             EL331
DAN   *****************************************************             EL331
DAN                                                                     EL331
DAN    9000-FIND-OPEN-CLAIM.                                            EL331
DAN                                                                     EL331
DAN        MOVE AX-COMPANY-CD   TO CL-COMPANY-CD-A4                     EL331
DAN        MOVE AX-CERT         TO CL-CERT-A4-PRIME                     EL331
DAN        MOVE AX-CERT-SUFFIX  TO CL-CERT-A4-SFX                       EL331
DAN                                                                     EL331
DAN        MOVE 'NO' TO CLAIM-OPEN-SW.                                  EL331
DAN        MOVE CL-CONTROL-BY-CERT-NO TO WS-SAVE-CERT-KEY               EL331
DAN                                                                     EL331
DAN        START ELMSTR5                                                EL331
DAN                   KEY EQUAL CL-CONTROL-BY-CERT-NO.                  EL331
DAN   ***                                                               EL331
DAN        IF ELMSTR5-FILE-STATUS  EQUAL '23'                           EL538
DAN            GO TO 9000-EXIT.                                         EL331
DAN                                                                     EL331
DAN        IF ELMSTR5-FILE-STATUS  EQUAL ZERO                           EL538
DAN            PERFORM 9100-READNEXT-CLAIM  THRU 9100-EXIT              EL331
DAN              UNTIL OPEN-CLAIM-FOUND                                 EL331
DAN                 OR CL-CONTROL-BY-CERT-NO NOT = WS-SAVE-CERT-KEY     EL331
DAN                 OR ELMSTR5-FILE-STATUS = '10'                       EL331
DAN        ELSE                                                         EL331
DAN            MOVE 'ERROR OCCURED START - ELMSTR5'                     EL331
DAN                                      TO  WS-ABEND-MESSAGE           EL331
DAN            MOVE ELMSTR5-FILE-STATUS TO  WS-ABEND-FILE-STATUS        EL331
DAN            GO TO ABEND-PGM.                                         EL331
DAN                                                                     EL331
DAN    9000-EXIT.                                                       EL331
DAN         EXIT.                                                       EL331
DAN                                                                     EL331
DAN        EJECT                                                        EL331
DAN   *****************************************************             EL331
DAN   *  9100-READNEXT-CLAIM                              *             EL331
DAN   *****************************************************             EL331
DAN   *  READ CLAIM MASTER FILE AND CHECK KEYS FOR MATCH  *             EL331
DAN   *****************************************************             EL331
DAN                                                                     EL331
DAN    9100-READNEXT-CLAIM.                                             EL331
DAN                                                                     EL331
DAN        READ ELMSTR5  NEXT  RECORD.                                  EL331
DAN                                                                     EL331
DAN        IF ELMSTR5-FILE-STATUS  EQUAL '10'                           EL538
DAN           GO TO 9100-EXIT.                                          EL331
DAN                                                                     EL331
DAN        IF ELMSTR5-FILE-STATUS  > '02'                               EL538
DAN            MOVE 'ERROR OCCURED READNEXT - ELMSTR5'                  EL331
DAN                                     TO  WS-ABEND-MESSAGE            EL331
DAN            MOVE ELMSTR5-FILE-STATUS TO  WS-ABEND-FILE-STATUS        EL331
DAN            GO TO ABEND-PGM.                                         EL331
DAN                                                                     EL331
DAN        IF CL-CONTROL-BY-CERT-NO  NOT EQUAL WS-SAVE-CERT-KEY         EL331
DAN           GO TO 9100-EXIT.                                          EL331
DAN                                                                     EL331
DAN        IF CL-CLAIM-STATUS = 'O'                                     EL331
DAN           MOVE 'YES' TO CLAIM-OPEN-SW.                              EL331
DAN                                                                     EL331
DAN    9100-EXIT.                                                       EL331
DAN         EXIT.                                                       EL331
01564                                                                   EL538
01565                                                                   EL538
01566  ABEND-PGM.                                                       EL538
01567                              COPY ELCABEND  SUPPRESS.             EL538
01568                                                                   EL538
01569  9999-END-OF-JOB.                                                 EL538
01570                              COPY ELCPRTCX.                       EL538
01571      CLOSE PRNTR.                                                 EL538
01572                                                                   EL538
01573                                                                   EL538
01582                                                                   EL538
01583                                                                   EL538
01584      GOBACK.                                                      EL538
01585                                                                   EL538
