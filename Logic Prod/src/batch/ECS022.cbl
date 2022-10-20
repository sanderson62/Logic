00001  IDENTIFICATION DIVISION.                                         11/30/98
00002                                                                   ECS022
00003  PROGRAM-ID.                 ECS022.                                 LV016
00004 *              PROGRAM CONVERTED BY                               ECS022
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS022
00006 *              CONVERSION DATE 12/07/94 18:17:21.                 ECS022
00007 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             ECS022
00008 *                            VMOD=2.017.                             CL**9
00009                                                                   ECS022
00010 *AUTHOR.     LOGIC, INC.                                          ECS022
00011 *            DALLAS, TEXAS.                                       ECS022
00012                                                                   ECS022
00013 *DATE-COMPILED.                                                   ECS022
00024                                                                   ECS022
00025 *REMARKS.                                                         ECS022
00026 *                RISK DISTRIBUTION REPORT.                        ECS022
040304******************************************************************
040304*                   C H A N G E   L O G
040304*
040304* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
040304*-----------------------------------------------------------------
040304*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
040304* EFFECTIVE    NUMBER
040304*-----------------------------------------------------------------
040304* 040304                   SMVA  FIX LOGIC FLOW FROM 0550- PUT 
040304*                                GOBACK IN 9999-END-JOB PARAGRAPH
040304******************************************************************
00027      EJECT                                                        ECS022
00028  ENVIRONMENT DIVISION.                                            ECS022
00029  INPUT-OUTPUT SECTION.                                            ECS022
00030  FILE-CONTROL.                                                    ECS022
00031      SELECT  CERT-MASTER ASSIGN TO SYS012-UT-2400-S-SYS012.       ECS022
00032      SELECT  TEMP-EXTRACT ASSIGN TO SYS013-UT-2400-S-SYS013.      ECS022
00033      SELECT  EXTRACT     ASSIGN TO SYS010-UT-2400-S-SYS010.       ECS022
00034      SELECT  ACCOUNT     ASSIGN TO ERACCTT                        ECS022
00035                          ACCESS         SEQUENTIAL                ECS022
00036                          ORGANIZATION   INDEXED                   ECS022
00037                          FILE STATUS    AM-FILE-STATUS            ECS022
00038                          RECORD KEY     AM-KEY.                   ECS022
00039      SELECT  DISK-DATE   ASSIGN TO SYS019-UT-3380-S-SYS019.       ECS022
00040      SELECT  PRINTER     ASSIGN TO SYS008.
00041      SELECT  SUMMARY     ASSIGN TO SYS011-UT-2400-S-SYS011A.      ECS022
00042      SELECT  FICH        ASSIGN TO SYS020-UT-2400-S-SYS020.       ECS022
00043      SELECT  SORT-FILE   ASSIGN TO SYS001-UT-3380-S-SORTWK1.      ECS022
00044  EJECT                                                            ECS022
00045  DATA DIVISION.                                                   ECS022
00046  FILE SECTION.                                                    ECS022
00047                                                                   ECS022
00048  FD  CERT-MASTER                                                  ECS022
00049      BLOCK CONTAINS 0 RECORDS
00050      RECORDING MODE F.                                            ECS022
00051                                                                   ECS022
00052      COPY ECSCRT01.                                               ECS022
00053                                                                   ECS022
00054  FD  EXTRACT                                                      ECS022
00055      BLOCK CONTAINS 0 RECORDS
00056      RECORDING MODE F.                                            ECS022
00057                                                                   ECS022
00058  01  D-EXT                          PIC X(510).                   ECS022
00059                                                                   ECS022
00060  FD  TEMP-EXTRACT                                                 ECS022
00061      BLOCK CONTAINS 0 RECORDS
00062      RECORDING MODE F.                                            ECS022
00063                                                                   ECS022
00064  01  TEMP-EXTRACT-RECORD            PIC X(510).                   ECS022
00065                                                                   ECS022
00066  FD  ACCOUNT.                                                     ECS022
00067                                                                   ECS022
00068  01  AM-REC.                                                      ECS022
00069      05  FILLER                     PIC XX.                       ECS022
00070      05  AM-KEY                     PIC X(26).                    ECS022
00071      05  FILLER                     PIC X(1972).                  ECS022
00072                                                                   ECS022
00073  FD  DISK-DATE                                                    ECS022
00074      COPY ELCDTEFD.                                               ECS022
00075  EJECT                                                            ECS022
00076  FD  PRINTER                                                      ECS022
00077      COPY ELCPRTFD.                                               ECS022
00078  EJECT                                                            ECS022
00079  FD  SUMMARY                                                      ECS022
00080      BLOCK CONTAINS 0 RECORDS
00081      RECORDING MODE F.                                            ECS022
00082                                                                   ECS022
CIDMOD*01  SUM-REC1        PIC X(1366).                                 ECS022
PEMMOD 01  SUM-REC1        PIC X(1446).                                 ECS022
00084                                                                   ECS022
00085  FD  FICH                                                         ECS022
00086      COPY ELCFCHFD.                                               ECS022
00087  EJECT                                                            ECS022
00088  SD  SORT-FILE.                                                   ECS022
00089                                                                   ECS022
00090  01  SORTWORK.                                                    ECS022
00091      05  SR-REPORT-CD    PIC X.                                   ECS022
00092      05  SR-REPORT-CNTL  PIC X(10).                               ECS022
00093      05  SR-CARR-GROUP   PIC X(7).                                ECS022
00094      05  SR-REPT-CD2     PIC X(10).                               ECS022
00095      05  SR-STATE        PIC XX.                                  ECS022
00096      05  SR-ACCOUNT      PIC X(10).                               ECS022
00097      05  SR-REPT-CD1     PIC X(10).                               ECS022
CIDMOD*    05  FILLER          PIC X(1316).                             ECS022
PEMMOD     05  FILLER          PIC X(1396).                             ECS022
00099  EJECT                                                            ECS022
00100  WORKING-STORAGE SECTION.                                         ECS022
00101  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS022
00102  77  FILLER  PIC X(32) VALUE '********************************'.  ECS022
00103  77  FILLER  PIC X(32) VALUE '     ECS022 WORKING STORAGE     '.  ECS022
00104  77  FILLER  PIC X(32) VALUE '*******VMOD=2.017***************'.     CL**9
00105                                                                   ECS022
00106      COPY ERCACCT.                                                ECS022
00107  EJECT                                                            ECS022
00108      COPY ECSEXT01.                                               ECS022
00109  EJECT                                                            ECS022
00110      COPY ELCDTECX.                                               ECS022
00111  EJECT                                                            ECS022
00112      COPY ELCDTEVR.                                               ECS022
00113  EJECT                                                            ECS022
00114  01  WS-AM-CNTL.                                                  ECS022
00115      12  WS-AM-CARR  PIC X       VALUE SPACES.                    ECS022
00116      12  WS-AM-GRP   PIC X(6)    VALUE SPACES.                    ECS022
00117                                                                   ECS022
00118  01  WS-DE-CNTL.                                                  ECS022
00119      12  WS-DE-CARR  PIC X       VALUE SPACES.                    ECS022
00120      12  WS-DE-GRP   PIC X(6)    VALUE SPACES.                    ECS022
00121                                                                   ECS022
00122  01  HOLD-TOT-OPT    PIC 9.                                       ECS022
00123                                                                   ECS022
00124  01  HEADINGS.                                                    ECS022
00125      12  HEAD-1.                                                  ECS022
00126          16  FILLER  PIC X(53)   VALUE SPACES.                    ECS022
00127          16  FILLER  PIC X(20)   VALUE 'RISK DISTRIBUTION   '.    ECS022
00128          16  HEAD-1-DESC PIC X(13) VALUE SPACES.                  ECS022
00129          16  FILLER  PIC X(33)   VALUE SPACES.                    ECS022
00130          16  FILLER  PIC X(6)    VALUE 'ECS022'.                  ECS022
00131      12  HEAD-2.                                                  ECS022
00132          16  FILLER  PIC X(47)   VALUE SPACES.                    ECS022
00133          16  H1-CMP  PIC X(30).                                   ECS022
00134          16  FILLER  PIC X(42)   VALUE SPACES.                    ECS022
00135          16  H2-IPL  PIC X(8).                                    ECS022
00136      12  HEAD-3.                                                  ECS022
00137          16  FILLER  PIC X(53)   VALUE SPACES.                    ECS022
00138          16  H3-DATE PIC X(18).                                   ECS022
00139          16  FILLER  PIC X(48)   VALUE SPACES.                    ECS022
00140          16  FILLER  PIC X(5)    VALUE 'PAGE '.                   ECS022
00141          16  H1-PAGE PIC ZZ,ZZZ.                                  ECS022
00142      12  HEAD-3A.                                                 ECS022
00143          16  H3-DES  PIC X(58)   VALUE SPACES.                    ECS022
00144          16  FILLER  PIC X(75)   VALUE SPACES.                    ECS022
00145      12  HEAD-4.                                                  ECS022
00146          16  H4-DES  PIC X(58)   VALUE SPACES.                    ECS022
00147          16  FILLER  PIC X(75)   VALUE SPACES.                    ECS022
00148      12  HEAD-5.                                                  ECS022
00149          16  FILLER  PIC X(19) VALUE 'T E R M      AGE   '.       ECS022
00150          16  FILLER  PIC X(19) VALUE '         AGE       '.       ECS022
00151          16  FILLER  PIC X(19) VALUE '     AGE           '.       ECS022
00152          16  FILLER  PIC X(19) VALUE ' AGE            AGE'.       ECS022
00153          16  FILLER  PIC X(19) VALUE '            AGE    '.       ECS022
00154          16  FILLER  PIC X(19) VALUE '        AGE        '.       ECS022
00155          16  FILLER  PIC X(18) VALUE '   TOTAL      PCT '.        ECS022
00156      12  HEAD-6.                                                  ECS022
00157          16  FILLER  PIC X(19) VALUE '          UNDER  36'.       ECS022
00158          16  FILLER  PIC X(19) VALUE '       36 - 40     '.       ECS022
00159          16  FILLER  PIC X(19) VALUE '   41 - 45        4'.       ECS022
00160          16  FILLER  PIC X(19) VALUE '6 - 50        51 - '.       ECS022
00161          16  FILLER  PIC X(19) VALUE '55        56 - 64  '.       ECS022
00162          16  FILLER  PIC X(19) VALUE '      OVER 64      '.       ECS022
00163          16  FILLER  PIC X(18) VALUE ' ALL  AGES        '.        ECS022
00164      12  MIL-HEAD-6.                                              ECS022
00165          16  FILLER  PIC X(19) VALUE '          UNDER  41'.       ECS022
00166          16  FILLER  PIC X(19) VALUE '       41 - 45     '.       ECS022
00167          16  FILLER  PIC X(19) VALUE '   46 - 50        5'.       ECS022
00168          16  FILLER  PIC X(19) VALUE '1 - 55        56 - '.       ECS022
00169          16  FILLER  PIC X(19) VALUE '59        60 - 65  '.       ECS022
00170          16  FILLER  PIC X(19) VALUE '      OVER 65      '.       ECS022
00171          16  FILLER  PIC X(18) VALUE ' ALL  AGES        '.        ECS022
00172      12  HEAD-7.                                                  ECS022
00173          16  H7-DES  PIC X(41) VALUE SPACES.                      ECS022
00174          16  FILLER  PIC X(91) VALUE SPACES.                      ECS022
00175      12  ACCT-TOT.                                                ECS022
00176          16  FILLER  PIC X(18) VALUE '* * * ACCOUNT NO. '.        ECS022
00177          16  AT-NO   PIC X(10).                                   ECS022
00178          16  FILLER  PIC X(10) VALUE ' TOTALS'.                   ECS022
00179          16  AT-NA   PIC X(30) VALUE SPACES.                      ECS022
00180      12  STATE-TOT.                                               ECS022
00181          16  FILLER  PIC X(15) VALUE '* * * STATE OF '.           ECS022
00182          16  ST-DES  PIC X(19).                                   ECS022
00183          16  FILLER  PIC X(7)  VALUE ' TOTALS'.                   ECS022
00184      12  COMP-TOT.                                                ECS022
00185          16  FILLER  PIC X(19) VALUE '* * CARRIER/GROUP  '.       ECS022
00186          16  CP-CARR PIC X(1).                                    ECS022
00187          16  FILLER  PIC X(1)  VALUE SPACES.                      ECS022
00188          16  CP-GRP  PIC X(6).                                    ECS022
00189          16  FILLER  PIC X(19) VALUE ' TOTALS'.                   ECS022
00190      12  PA-HEAD.                                                 ECS022
00191          16  FILLER  PIC X(41) VALUE '      PREMIUM ANALYSIS'.    ECS022
00192      12  AE-HEAD.                                                 ECS022
00193          16  FILLER  PIC X(41) VALUE                              ECS022
00194                      '      AVERAGE EXPOSURE ANALYSIS'.           ECS022
00195  EJECT                                                            ECS022
00196  01  DTL-LINE.                                                    ECS022
00197      12  D-DATA.                                                  ECS022
00198          16  D-DES2       PIC X(6).                               ECS022
00199          16  D-AMT        PIC ZZZ,ZZZ,ZZZ.99-                     ECS022
00200                             BLANK WHEN ZERO OCCURS 8.             ECS022
00201      12  D-DATA1     REDEFINES   D-DATA.                          ECS022
00202          16  FILLER       PIC X(6).                               ECS022
00203          16  D-PCTS  OCCURS 8.                                    ECS022
00204              20  FILLER   PIC X(10).                              ECS022
00205              20  D-PCT    PIC ZZZ-.                               ECS022
00206              20  D-PCT1   PIC X.                                  ECS022
00207      12  D-DATA2     REDEFINES   D-DATA1.                         ECS022
00208          16  D-DES        PIC X(7).                               ECS022
00209          16  FILLER       PIC X(119).                             ECS022
00210      12  FILLER           PIC X   VALUE SPACE.                    ECS022
00211      12  D-EPCT           PIC ZZZ-.                               ECS022
00212      12  D-EPCT1          PIC X   VALUE SPACE.                    ECS022
00213                                                                   ECS022
00214  01  COUNT-LINE  SYNC.                                            ECS022
00215      12  FILLER      PIC X(24) VALUE ' NUMBER OF CERTIFICATES '.  ECS022
00216      12  D-CNT       PIC Z,ZZZ,ZZ9.                               ECS022
00217      12  FILLER      PIC X(99) VALUE SPACES.                      ECS022
00218                                                                   ECS022
00219  01  AGE-LINE    SYNC.                                            ECS022
00220      12  FILLER      PIC X(22) VALUE ' AVERAGE AGE'.              ECS022
00221      12  D-AGE       PIC ZZZZ,ZZZ.99.                             ECS022
00222      12  FILLER      PIC X(99) VALUE SPACES.                      ECS022
00223                                                                   ECS022
00224  01  WEIGHTED-LINE.                                               ECS022
00225      12  FILLER      PIC X(24) VALUE ' AVERAGE WEIGHTED AGE'.     ECS022
00226      12  D-WEIGHTED  PIC ZZ,ZZZ.99.                               ECS022
00227      12  FILLER      PIC X(99) VALUE SPACES.                      ECS022
00228                                                                   ECS022
00229  01  TERM-LINE   SYNC.                                            ECS022
00230      12  FILLER      PIC X(22) VALUE ' AVERAGE TERM'.             ECS022
00231      12  D-TERM      PIC ZZZZ,ZZZ.99.                             ECS022
00232      12  FILLER      PIC X(99) VALUE SPACES.                      ECS022
00233  01  FILLER.                                                      ECS022
00234      12  WS-BEGIN-DATE         PIC 9(8) VALUE ZEROS.                 CL*15
00235      12  FILLER REDEFINES WS-BEGIN-DATE.                             CL*15
00236          16  WS-BEGIN-CCYY     PIC 9(4).                             CL*16
00237          16  WS-BEGIN-CCYR REDEFINES  WS-BEGIN-CCYY.              ECS022
00238              20  WS-BEGIN-CC   PIC 99.                            ECS022
00239              20  WS-BEGIN-YR   PIC 99.                            ECS022
00240          16  WS-BEGIN-MO       PIC 99.                            ECS022
00241          16  WS-BEGIN-DA       PIC 99.                            ECS022
00242                                                                      CL*15
00243      12  WS-END-DATE           PIC 9(8) VALUE ZEROS.                 CL*15
00244      12  FILLER REDEFINES WS-END-DATE.                               CL*15
00245          16  WS-END-CCYY       PIC 9(4).                             CL*16
00246          16  WS-END-CCYR REDEFINES  WS-END-CCYY.                  ECS022
00247              20  WS-END-CC     PIC 99.                            ECS022
00248              20  WS-END-YR     PIC 99.                            ECS022
00249          16  WS-END-MO         PIC 99.                            ECS022
00250          16  WS-END-DA         PIC 99.                            ECS022
00251                                                                      CL*15
00252      12  WS-BIN-LF-EXP         PIC XX VALUE LOW-VALUES.           ECS022
00253      12  WS-BIN-AH-EXP         PIC XX VALUE LOW-VALUES.           ECS022
00254      12  WS-BIN-RUN-DATE       PIC XX VALUE LOW-VALUES.           ECS022
00255      12  WS-BIN-RUN-DATE-3     PIC XX VALUE LOW-VALUES.           ECS022
00256      12  WS-BIN-1ST-DATE       PIC XX VALUE LOW-VALUES.           ECS022
00257      12  WS-BIN-EFF-DT         PIC XX VALUE LOW-VALUES.           ECS022
00258      12  WS-BIN-ENT-DT         PIC XX VALUE LOW-VALUES.           ECS022
00259      12  RUN-DT                PIC S9(5) VALUE +0 COMP-3.         ECS022
00260      12  WRK-DT-LF             PIC S9(5) VALUE +0 COMP-3.         ECS022
00261      12  WRK-DT-AH             PIC S9(5) VALUE +0 COMP-3.         ECS022
00262      12  WRK-DT                PIC S9(5) VALUE +0 COMP-3.         ECS022
00263      12  OPT-DT                PIC S9(5) VALUE +0 COMP-3.         ECS022
00264      12  LF-REM-TRM            PIC S999V99 VALUE +0 COMP-3.       ECS022
00265      12  AH-REM-TRM            PIC S999V99 VALUE +0 COMP-3.       ECS022
00266      12  VSAM-ERROR.                                              ECS022
00267          16  VSAM-AM-CONSTANT  PIC XX VALUE '06'.                 ECS022
00268          16  VSAM-ERR-CODE     PIC XX VALUE '00'.                 ECS022
00269      12  AM-FILE-STATUS        PIC XX VALUE '00'.                 ECS022
00270      12  EXTRACT-COUNT         PIC S9(7)     COMP-3 VALUE +0.     ECS022
00271      12  TEMP-EXTRACT-COUNT    PIC S9(7)     COMP-3 VALUE +0.     ECS022
00272      12  PREMIUM-TOTAL         PIC S9(11)V99 COMP-3 VALUE +0.     ECS022
00273      12  SORT-CNT              PIC S9(7)     COMP-3 VALUE +0.     ECS022
00274      12  ACCT-CNT              PIC S9(7)     COMP-3 VALUE +0.     ECS022
00275      12  RETR-CNT              PIC S9(7)     COMP-3 VALUE +0.     ECS022
00276      12  PAGE-CT               PIC S9(5)     COMP-3 VALUE +0.     ECS022
00277      12  X                     PIC X       VALUE SPACE.           ECS022
00278      12  WS-SUB                PIC S9(5)   COMP.                  ECS022
00279      12  WS-SUB2               PIC S9(5)   COMP.                  ECS022
00280      12  WS-SUB3               PIC S9(5)   COMP.                  ECS022
00281      12  START1                PIC S9(5)   COMP.                  ECS022
00282      12  START2                PIC S9(5)   COMP.                  ECS022
00283      12  END1                  PIC S9(5)   COMP.                  ECS022
00284      12  END2                  PIC S9(5)   COMP.                  ECS022
00285      12  FORTY                 PIC S9(5)   COMP VALUE +40.        ECS022
00286      12  C1-SUB                PIC S9(5)   COMP.                  ECS022
00287      12  RC-SUB                PIC S9(5)   COMP.                  ECS022
00288      12  COL-TOT-SUB           PIC S9(5)   COMP.                  ECS022
00289      12  ROW-TOT-SUB           PIC S9(5)   COMP.                  ECS022
00290      12  X1                    PIC S9(5)   COMP.                  ECS022
00291      12  X2                    PIC S9(5)   COMP.                  ECS022
00292      12  X3                    PIC S9(5)   COMP.                  ECS022
00293      12  X4                    PIC S9(5)   COMP.                  ECS022
00294      12  X5                    PIC S9(5)   COMP.                  ECS022
00295      12  WS-TYPE-SW            PIC X.                             ECS022
00296          88  SUMMARY-REC             VALUE 'Z'.                   ECS022
00297          88  OB-REC                  VALUE 'B'.                   ECS022
00298      12  VALID-EXT             PIC 9 VALUE 0.                     ECS022
00299      12  BENEFIT-FOUND         PIC 9 VALUE 0.                     ECS022
00300      12  STATE-FOUND           PIC 9 VALUE 0.                     ECS022
00301      12  EXT-EOF               PIC 9 VALUE 0.                     ECS022
00302      12  SRT-EOF               PIC 9 VALUE 0.                     ECS022
00303      12  AM-EOF                PIC 9 VALUE 0.                     ECS022
00304      12  WS-RETURN-CODE        PIC X(4)  VALUE SPACES.            ECS022
00305      12  ABEND-OPTION          PIC X     VALUE 'Y'.               ECS022
00306      12  WS-ABEND-MESSAGE      PIC X(80) VALUE SPACES.            ECS022
00307      12  WS-ABEND-FILE-STATUS  PIC XX    VALUE SPACES.            ECS022
00308      12  WS-ZERO               PIC S9    VALUE ZERO.              ECS022
00309      12  PGM-SUB               PIC S999  COMP-3 VALUE +022.       ECS022
00310  01  WK-ACCT.                                                     ECS022
00311      12  WK-GA                 PIC X(10) VALUE SPACES.            ECS022
00312  01  H4-CTL.                                                      ECS022
00313      12  FILLER                PIC X(4).                          ECS022
00314      12  H4-CARR               PIC X.                             ECS022
00315      12  FILLER                PIC X.                             ECS022
00316      12  H4-GRP                PIC X(6).                          ECS022
00317      12  FILLER                PIC X(5).                          ECS022
00318      12  H4-ST                 PIC XX.                            ECS022
00319  01  H-PCT.                                                       ECS022
00320      12  HOLD-PCT              PIC S9(3) OCCURS 13  COMP-3.       ECS022
00321  01  TOT-PREM                  PIC S9(9)V99  COMP-3.              ECS022
00322  01  PRM1                      PIC S9(9)V99  COMP-3.              ECS022
00323  01  WK-TERM                   PIC 999.                           ECS022
00324                                                                   ECS022
00325  EJECT                                                            ECS022
00326  01  ACCUMS.                                                      ECS022
00327      12  ACC1    OCCURS  280 TIMES   COMP-3.                      ECS022
00328          16  L-PREM      PIC S9(9)V99.                            ECS022
00329          16  A-PREM      PIC S9(9)V99.                            ECS022
PEMMOD         16  L-BEN       PIC S9(12)V99.                           ECS022
00331          16  A-BEN       PIC S9(9)V99.                            ECS022
00332          16  L-CT        PIC S9(7).                               ECS022
00333          16  A-CT        PIC S9(7).                               ECS022
00334                                                                   ECS022
00335      12  ACC2    OCCURS   7  TIMES   COMP-3.                      ECS022
00336          16  KOUNT       PIC S9(7).                               ECS022
00337          16  AGE         PIC S9(10).                              ECS022
00338          16  TERM        PIC S9(12).                              ECS022
00339                                                                   ECS022
00340      12  WEIGHT-CNTRS    OCCURS 7 TIMES    COMP-3.                ECS022
00341          16  WEIGHT-CNT   PIC S9(7).                              ECS022
00342          16  WEIGHT-FACE  PIC S9(12)V99.                          ECS022
00343          16  WEIGHT-WORK  PIC S9(12)V99.                          ECS022
00344                                                                   ECS022
00345  01  SUM-REC.                                                     ECS022
00346      12  SUM-CTL.                                                 ECS022
00347          16  S-CODE      PIC X.                                   ECS022
00348          16  S-RPT-CNTL  PIC X(10).                               ECS022
00349          16  S-CG.                                                ECS022
00350              20  S-CARR  PIC X.                                   ECS022
00351              20  S-GRP   PIC X(6).                                ECS022
00352          16  S-RPT-CD2   PIC X(10).                               ECS022
00353          16  S-ST        PIC XX.                                  ECS022
00354          16  S-ACCT      PIC X(10).                               ECS022
00355          16  S-RPT-CD1   PIC X(10).                               ECS022
00356      12  SUM-DATA    OCCURS  40 TIMES COMP-3.                     ECS022
00357          16  SL-P    PIC S9(9)V99.                                ECS022
00358          16  SA-P    PIC S9(9)V99.                                ECS022
PEMMOD         16  SL-B    PIC S9(12)V99.                               ECS022
00360          16  SA-B    PIC S9(9)V99.                                ECS022
00361          16  SL-C    PIC S9(7).                                   ECS022
00362          16  SA-C    PIC S9(7).                                   ECS022
00363      12  S-CT        PIC S9(7)   COMP-3 VALUE +0.                 ECS022
CIDMOD     12  S-AGE       PIC S9(11)   COMP-3 VALUE +0.                ECS022
CIDMOD     12  S-TERM      PIC S9(11)   COMP-3 VALUE +0.                ECS022
00366      12  S-CNT       PIC S9(7)   COMP-3 VALUE +0.                 ECS022
00367      12  S-FACE      PIC S9(12)V99  COMP-3 VALUE +0.              ECS022
00368      12  S-WORK      PIC S9(12)V99  COMP-3 VALUE +0.              ECS022
00369  EJECT                                                            ECS022
00370 *                                                                 ECS022
00371 *                                                                 ECS022
00372 * LEAVE THE ABOVE 2 LINES IN TO PREVENT LOSING THE LINE ABOVE     ECS022
00384  EJECT                                                            ECS022
00385      COPY ELCCALC.                                                ECS022
00386  EJECT                                                            ECS022
00387      COPY ELCDATE.                                                   CL**7
00388  EJECT                                                            ECS022
00389      COPY ELCCRTVR.                                               ECS022
00390  EJECT                                                            ECS022
00391      COPY ELCEXTVR.                                               ECS022
00392  EJECT                                                            ECS022

040304******************************************************************
00393  PROCEDURE DIVISION.                                              ECS022

00394  0100-DATE-READ.                                                  ECS022
00395      COPY ELCDTERX.                                               ECS022
00396                                                                   ECS022
00397      OPEN OUTPUT PRINTER.                                         ECS022
00398                                                                   ECS022
00399      MOVE RUN-MO        TO WS-END-MO.                                CL*11
00400      MOVE RUN-DA        TO WS-END-DA.                                CL*11
00401      MOVE RUN-YR        TO WS-END-YR.                                CL*11
00402                                                                   ECS022
00403      COMPUTE WS-BEGIN-CCYY = WS-END-CCYY - +1.                       CL*11
00404      MOVE 12            TO WS-BEGIN-MO.                           ECS022
00405      MOVE 31            TO WS-BEGIN-DA.                           ECS022
00406                                                                   ECS022
00407      DISPLAY '   BEGIN DATE    ' WS-BEGIN-DATE.                      CL*11
00408      DISPLAY '   END   DATE    ' WS-END-DATE.                        CL*11
00409                                                                   ECS022
00410      IF DTE-TOT-OPT = 4                                              CL*11
00411         MOVE 'Y-T-D ISSUES' TO HEAD-1-DESC                        ECS022
00412      ELSE                                                         ECS022
00413      IF DTE-TOT-OPT = 3                                              CL*11
00414         MOVE 'Q-T-D INFORCE' TO HEAD-1-DESC                       ECS022
00415      ELSE                                                         ECS022
00416      IF DTE-TOT-OPT = 2                                              CL*11
00417         MOVE 'ALL INFORCE' TO HEAD-1-DESC                         ECS022
00418      ELSE                                                         ECS022
00419         MOVE 'M-T-D ISSUES' TO HEAD-1-DESC.                       ECS022
00420                                                                   ECS022
00421      IF DTE-TOT-OPT = 2 OR 3 OR 4                                 ECS022
00422         PERFORM 0010-BUILD-INFORCE THRU 0099-EXIT.                ECS022
00423                                                                   ECS022
00424      GO TO 0110-START-1.                                          ECS022
00425                                                                   ECS022
00426  0010-BUILD-INFORCE.                                              ECS022
00427      OPEN INPUT CERT-MASTER                                       ECS022
00428          OUTPUT TEMP-EXTRACT.                                     ECS022
00429                                                                   ECS022
00430      MOVE BIN-RUN-DATE           TO WS-BIN-RUN-DATE.              ECS022
00431                                                                   ECS022
00432      MOVE WS-BIN-RUN-DATE        TO DC-BIN-DATE-1.                ECS022
00433      MOVE -3                     TO DC-ELAPSED-MONTHS.            ECS022
00434      MOVE +0                     TO DC-ELAPSED-DAYS.              ECS022
00435      MOVE '6'                    TO DC-OPTION-CODE.               ECS022
00436      PERFORM 1090-DATE-CONVERSION THRU 1090-EXIT.                 ECS022
00437      MOVE DC-BIN-DATE-2          TO WS-BIN-RUN-DATE-3.            ECS022
00438                                                                   ECS022
00439  0020-READ-CERT-MASTER.                                           ECS022
00440                                                                   ECS022
00441      READ CERT-MASTER AT END                                      ECS022
00442           GO TO 0060-FINISH-INFORCE-EXTRACT.                      ECS022
00443                                                                   ECS022
00444      IF CR-POLICY-IS-DECLINED OR                                  ECS022
00445         CR-POLICY-IS-VOID                                         ECS022
00446         GO TO 0020-READ-CERT-MASTER.                              ECS022
00447                                                                   ECS022
00448      COPY ELCCRTM1.                                               ECS022
00449                                                                   ECS022
00450      IF DTE-TOT-OPT = 4                                              CL*11
00451         IF (WS-CR-ENTRY-DATE GREATER THAN WS-BEGIN-DATE) AND      ECS022
00452            (WS-CR-ENTRY-DATE NOT GREATER THAN WS-END-DATE) AND    ECS022
00453            (CR-ENTRY-STATUS NOT = '5' AND '9')                       CL*11
00454            GO TO 0030-BUILD-EXTRACT                               ECS022
00455         ELSE                                                      ECS022
00456            GO TO 0020-READ-CERT-MASTER.                           ECS022
00457                                                                   ECS022
00458      IF (CR-LF-CURRENT-STATUS = '1' OR '4')                       ECS022
00459        OR                                                         ECS022
00460         (CR-AH-CURRENT-STATUS = '1' OR '4')                       ECS022
00461         NEXT SENTENCE                                             ECS022
00462      ELSE                                                         ECS022
00463         GO TO 0020-READ-CERT-MASTER.                              ECS022
00464                                                                   ECS022
00465      MOVE CR-ENTRY-CC            TO DC-ALPHA-CEN-N.               ECS022
00466      MOVE CR-ENTRY-YR            TO DC-YMD-YEAR.                  ECS022
00467      MOVE CR-ENTRY-MO            TO DC-YMD-MONTH.                 ECS022
00468      MOVE CR-ENTRY-DA            TO DC-YMD-DAY.                   ECS022
00469      MOVE '3'                    TO DC-OPTION-CODE.               ECS022
00470      PERFORM 1090-DATE-CONVERSION THRU 1090-EXIT.                 ECS022
00471      MOVE DC-BIN-DATE-1          TO WS-BIN-ENT-DT.                ECS022
00472                                                                   ECS022
00473      MOVE CR-CC                  TO DC-ALPHA-CEN-N.               ECS022
00474      MOVE CR-YR                  TO DC-YMD-YEAR.                  ECS022
00475      MOVE CR-MO                  TO DC-YMD-MONTH.                 ECS022
00476      MOVE CR-DA                  TO DC-YMD-DAY.                   ECS022
00477      MOVE '3'                    TO DC-OPTION-CODE.               ECS022
00478      PERFORM 1090-DATE-CONVERSION THRU 1090-EXIT.                 ECS022
00479      MOVE DC-BIN-DATE-1          TO WS-BIN-EFF-DT.                ECS022
00480                                                                   ECS022
00481      IF CR-LOAN-1ST-PMT-DT NOT NUMERIC                            ECS022
00482         MOVE ZEROS               TO CR-LOAN-1ST-PMT-DT.           ECS022
00483                                                                   ECS022
00484      MOVE LOW-VALUES             TO WS-BIN-1ST-DATE.              ECS022
00485                                                                   ECS022
00486      IF CR-LOAN-1ST-PMT-DT NOT = ZEROS                            ECS022
00487         MOVE CR-LOAN-1ST-PMT-DT  TO DC-GREG-DATE-1-YMD            ECS022
00488         MOVE +0                  TO DC-ELAPSED-MONTHS             ECS022
00489                                     DC-ELAPSED-DAYS               ECS022
00490         MOVE '3'                 TO DC-OPTION-CODE                ECS022
00491         PERFORM 1090-DATE-CONVERSION THRU 1090-EXIT               ECS022
00492         IF NO-CONVERSION-ERROR                                    ECS022
00493             MOVE DC-BIN-DATE-1   TO WS-BIN-1ST-DATE               ECS022
00494         ELSE                                                      ECS022
00495             MOVE LOW-VALUES      TO WS-BIN-1ST-DATE.              ECS022
00496                                                                   ECS022
00497      IF WS-BIN-1ST-DATE  LESS THAN  WS-BIN-EFF-DT                 ECS022
00498         MOVE LOW-VALUES          TO WS-BIN-1ST-DATE.              ECS022
00499                                                                   ECS022
00500      IF WS-BIN-1ST-DATE = LOW-VALUES                                 CL*11
00501         MOVE WS-BIN-EFF-DT       TO DC-BIN-DATE-1                 ECS022
00502         MOVE +1                  TO DC-ELAPSED-MONTHS             ECS022
00503         MOVE +0                  TO DC-ELAPSED-DAYS               ECS022
00504         MOVE '6'                 TO DC-OPTION-CODE                ECS022
00505         PERFORM 1090-DATE-CONVERSION THRU 1090-EXIT               ECS022
00506         MOVE DC-BIN-DATE-2       TO WS-BIN-1ST-DATE.              ECS022
00507                                                                   ECS022
00508      MOVE WS-BIN-1ST-DATE        TO DC-BIN-DATE-1.                ECS022
00509      COMPUTE DC-ELAPSED-MONTHS = CR-LF-TERM - +1.                    CL*11
00510      MOVE +0                     TO DC-ELAPSED-DAYS.              ECS022
00511      MOVE '6'                    TO DC-OPTION-CODE.               ECS022
00512      PERFORM 1090-DATE-CONVERSION THRU 1090-EXIT.                 ECS022
00513      IF NO-CONVERSION-ERROR                                       ECS022
00514         MOVE DC-BIN-DATE-2       TO WS-BIN-LF-EXP                 ECS022
00515      ELSE                                                         ECS022
00516         MOVE LOW-VALUES          TO WS-BIN-LF-EXP.                ECS022
00517                                                                   ECS022
00518      MOVE WS-BIN-1ST-DATE        TO DC-BIN-DATE-1.                ECS022
00519      COMPUTE DC-ELAPSED-MONTHS = CR-AH-TERM - +1.                    CL*11
00520      MOVE +0                     TO DC-ELAPSED-DAYS.              ECS022
00521      MOVE '6'                    TO DC-OPTION-CODE.               ECS022
00522      PERFORM 1090-DATE-CONVERSION THRU 1090-EXIT.                 ECS022
00523      IF NO-CONVERSION-ERROR                                       ECS022
00524         MOVE DC-BIN-DATE-2       TO WS-BIN-AH-EXP                 ECS022
00525      ELSE                                                         ECS022
00526         MOVE LOW-VALUES          TO WS-BIN-AH-EXP.                ECS022
00527                                                                   ECS022
00528      IF (WS-BIN-LF-EXP LESS THAN WS-BIN-RUN-DATE)                 ECS022
00529        AND                                                        ECS022
00530         (WS-BIN-AH-EXP LESS THAN WS-BIN-RUN-DATE)                 ECS022
00531         GO TO 0020-READ-CERT-MASTER.                              ECS022
00532                                                                   ECS022
00533      IF WS-BIN-ENT-DT GREATER THAN WS-BIN-RUN-DATE                ECS022
00534         GO TO 0020-READ-CERT-MASTER.                              ECS022
00535                                                                   ECS022
00536      IF DTE-TOT-OPT = 3                                           ECS022
00537         IF (WS-BIN-ENT-DT GREATER THAN WS-BIN-RUN-DATE-3) AND     ECS022
00538            (WS-BIN-ENT-DT NOT GREATER THAN WS-BIN-RUN-DATE)       ECS022
00539            NEXT SENTENCE                                          ECS022
00540         ELSE                                                      ECS022
00541            GO TO 0020-READ-CERT-MASTER.                           ECS022
00542                                                                   ECS022
00543 ***************************************************************   ECS022
00544                                                                   ECS022
00545  0160-CALC-REM-TERM.                                              ECS022
00546      MOVE WS-BIN-EFF-DT          TO CP-CERT-EFF-DT.               ECS022
00547      MOVE WS-BIN-1ST-DATE        TO CP-FIRST-PAY-DATE.            ECS022
00548      MOVE WS-BIN-RUN-DATE        TO CP-VALUATION-DT.              ECS022
00549      MOVE STATE-SUB (CLAS-INDEXS)                                 ECS022
00550                                  TO CP-STATE.                     ECS022
00551      MOVE STATE-ABBR (CLAS-INDEXS)                                ECS022
00552                                  TO CP-STATE-STD-ABBRV.           ECS022
00553      MOVE '3'                    TO CP-PROCESS-TYPE.              ECS022
00554      MOVE DTE-CLASIC-COMPANY-CD  TO CP-COMPANY-CD.                ECS022
00555      MOVE DTE-CLIENT             TO CP-COMPANY-ID.                ECS022
00556      MOVE SPACES                 TO CP-ACCT-FLD-5.                ECS022
00557      MOVE DTE-REM-TRM            TO CP-REM-TERM-METHOD.           ECS022
00558      MOVE DTE-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.      ECS022
00559                                                                   ECS022
00560      IF DTE-CLIENT = 'FIM'                                        ECS022
00561          MOVE '5'                 TO CP-REM-TERM-METHOD.          ECS022
00562                                                                   ECS022
00563      IF DTE-CLIENT = 'POS'                                        ECS022
00564          IF CR-CARRIER = '1'                                      ECS022
00565              MOVE '1'             TO CP-REM-TERM-METHOD           ECS022
00566          ELSE                                                     ECS022
00567              MOVE '2'             TO CP-REM-TERM-METHOD.          ECS022
00568                                                                   ECS022
00569      IF CR-LFTYP = ZEROS                                          ECS022
00570          MOVE ZEROS               TO LF-REM-TRM                   ECS022
00571          GO TO 0170-CALC-REM-TERM-AH.                             ECS022
00572                                                                   ECS022
00573      MOVE CLAS-I-RL-AH (CLAS-INDEXL)                              ECS022
00574                                   TO CP-BENEFIT-TYPE.             ECS022
00575      MOVE CLAS-I-BAL (CLAS-INDEXL)                                ECS022
00576                                   TO CP-SPECIAL-CALC-CD.          ECS022
00577      MOVE CR-LF-TERM              TO CP-ORIGINAL-TERM             ECS022
00578                                      CP-LOAN-TERM.                ECS022
00579      IF CP-TERM-IS-DAYS                                           ECS022
00580          MOVE CR-LF-TERM-IN-DAYS  TO CP-TERM-OR-EXT-DAYS          ECS022
00581      ELSE                                                         ECS022
00582          MOVE ZEROS               TO CP-TERM-OR-EXT-DAYS.         ECS022
00583                                                                   ECS022
00584      PERFORM 1080-GET-REMAINING-TERM THRU 1080-EXIT.              ECS022
00585                                                                   ECS022
00586      MOVE CP-REMAINING-TERM-2     TO LF-REM-TRM.                  ECS022
00587                                                                   ECS022
00588  0170-CALC-REM-TERM-AH.                                           ECS022
00589      IF CR-AHTYP = ZEROS                                          ECS022
00590          MOVE ZEROS                  TO AH-REM-TRM                ECS022
00591          GO TO 0170-CHECK-REM-TERM.                               ECS022
00592                                                                   ECS022
00593      IF CR-LFTYP NOT = ZEROS                                      ECS022
00594          IF CR-AH-TERM = CR-LF-TERM                               ECS022
00595              MOVE CP-REMAINING-TERM-2    TO AH-REM-TRM            ECS022
00596              GO TO 0170-CHECK-REM-TERM.                           ECS022
00597                                                                   ECS022
00598      MOVE DTE-REM-TRM-CALC-OPTION    TO CP-REM-TRM-CALC-OPTION.      CL*11
00599      MOVE CLAS-I-RL-AH (CLAS-INDEXA) TO CP-BENEFIT-TYPE.          ECS022
00600      MOVE CLAS-I-BAL (CLAS-INDEXA)   TO CP-SPECIAL-CALC-CD.       ECS022
00601      MOVE CR-AH-TERM                 TO CP-ORIGINAL-TERM          ECS022
00602                                         CP-LOAN-TERM.             ECS022
00603      MOVE ZEROS                      TO CP-TERM-OR-EXT-DAYS.      ECS022
00604                                                                   ECS022
00605      PERFORM 1080-GET-REMAINING-TERM THRU 1080-EXIT.              ECS022
00606                                                                   ECS022
00607      MOVE CP-REMAINING-TERM-2        TO AH-REM-TRM.               ECS022
00608                                                                   ECS022
00609  0170-CHECK-REM-TERM.                                             ECS022
00610      IF  (LF-REM-TRM  NOT GREATER THAN ZEROS)  AND                ECS022
00611          (AH-REM-TRM  NOT GREATER THAN ZEROS)                     ECS022
00612              GO TO 0020-READ-CERT-MASTER.                         ECS022
00613                                                                   ECS022
00614 ***************************************************************   ECS022
00615                                                                   ECS022
00616  0030-BUILD-EXTRACT.                                              ECS022
00617      MOVE SPACES                 TO DETAIL-EXTRACT.               ECS022
00618      MOVE ZEROS                  TO DE-LF-TYPE                    ECS022
00619                                     DE-AH-TYPE                    ECS022
00620                                     DE-LF-TERM                    ECS022
00621                                     DE-LF-BEN                     ECS022
00622                                     DE-LF-BEN-ALT                 ECS022
00623                                     DE-LF-PRM                     ECS022
00624                                     DE-LF-PRM-ALT                 ECS022
00625                                     DE-AH-TERM                    ECS022
00626                                     DE-AH-BEN                     ECS022
00627                                     DE-AH-PRM.                    ECS022
00628                                                                   ECS022
00629      MOVE 'DE'                   TO DE-RECORD-ID.                 ECS022
00630      MOVE CR-COMPANY-CD          TO DE-COMPANY-CD.                ECS022
00631      MOVE CR-FULL-CONTROL        TO DE-CONTROL.                   ECS022
00632      MOVE 'I'                    TO DE-TRANS.                     ECS022
00633      MOVE CR-AGE                 TO DE-AGE.                       ECS022
00634      MOVE CR-SEX                 TO DE-SEX.                       ECS022
00635      MOVE CR-COMPENSATION-LEVELS TO DE-COMM-LEVELS.               ECS022
00636                                                                   ECS022
00637      IF DTE-TOT-OPT = 4                                              CL*11
00638         NEXT SENTENCE                                             ECS022
00639      ELSE                                                         ECS022
00640         IF CR-LF-CURRENT-STATUS = '1' OR '4'                      ECS022
00641            NEXT SENTENCE                                          ECS022
00642         ELSE                                                      ECS022
00643            GO TO 0040-BUILD-AH-DATA.                              ECS022
00644                                                                   ECS022
00645      IF DTE-TOT-OPT = 4                                              CL*11
00646         NEXT SENTENCE                                             ECS022
00647      ELSE                                                         ECS022
00648         IF WS-BIN-LF-EXP LESS THAN WS-BIN-RUN-DATE                ECS022
00649            GO TO 0040-BUILD-AH-DATA.                              ECS022
00650                                                                   ECS022
00651      MOVE CR-LFTYP               TO DE-LF-TYPE.                   ECS022
00652      MOVE CR-LF-TERM             TO DE-LF-TERM.                   ECS022
00653      MOVE CR-LFAMT               TO DE-LF-BEN.                    ECS022
00654      MOVE CR-LFPRM               TO DE-LF-PRM.                       CL*11
00655      MOVE CR-LFAMT-ALT           TO DE-LF-BEN-ALT.                   CL*13
00656      MOVE CR-LFPRM-ALT           TO DE-LF-PRM-ALT.                   CL*13
00657      COMPUTE PREMIUM-TOTAL = PREMIUM-TOTAL +                      ECS022
00658              CR-LFPRM + CR-LFPRM-ALT.                             ECS022
00659                                                                   ECS022
00660  0040-BUILD-AH-DATA.                                              ECS022
00661                                                                   ECS022
00662      IF DTE-TOT-OPT = 4                                              CL*11
00663         NEXT SENTENCE                                             ECS022
00664      ELSE                                                         ECS022
00665         IF CR-AH-CURRENT-STATUS = '1' OR '4'                      ECS022
00666            NEXT SENTENCE                                          ECS022
00667         ELSE                                                      ECS022
00668            GO TO 0050-WRITE-EXTRACT.                              ECS022
00669                                                                   ECS022
00670      IF DTE-TOT-OPT = 4                                              CL*11
00671         NEXT SENTENCE                                             ECS022
00672      ELSE                                                         ECS022
00673         IF WS-BIN-AH-EXP LESS THAN WS-BIN-RUN-DATE                ECS022
00674            GO TO 0050-WRITE-EXTRACT.                              ECS022
00675                                                                   ECS022
00676      MOVE CR-AHTYP               TO DE-AH-TYPE.                   ECS022
00677      MOVE CR-AH-TERM             TO DE-AH-TERM.                   ECS022
00678      MOVE CR-AHAMT               TO DE-AH-BEN.                    ECS022
00679      MOVE CR-AHPRM               TO DE-AH-PRM.                    ECS022
00680      COMPUTE PREMIUM-TOTAL = PREMIUM-TOTAL + CR-AHPRM.            ECS022
00681                                                                   ECS022
00682  0050-WRITE-EXTRACT.                                              ECS022
00683      WRITE TEMP-EXTRACT-RECORD FROM DETAIL-EXTRACT.               ECS022
00684      ADD +1 TO TEMP-EXTRACT-COUNT.                                ECS022
00685      GO TO 0020-READ-CERT-MASTER.                                 ECS022
00686                                                                   ECS022
00687  0060-FINISH-INFORCE-EXTRACT.                                     ECS022
00688      CLOSE CERT-MASTER                                            ECS022
00689            TEMP-EXTRACT.                                          ECS022
00690                                                                   ECS022
00691  0099-EXIT.                                                       ECS022
00692      EXIT.                                                        ECS022
00693                                                                   ECS022
00694      EJECT                                                        ECS022
00695  0110-START-1.                                                    ECS022
00696      IF TEMP-EXTRACT-COUNT GREATER THAN +0                        ECS022
00697         OPEN INPUT TEMP-EXTRACT                                   ECS022
00698      ELSE                                                         ECS022
00699         OPEN INPUT EXTRACT.                                       ECS022
00700                                                                   ECS022
00701      OPEN  INPUT   ACCOUNT.                                       ECS022
00702                                                                   ECS022
00703      IF AM-FILE-STATUS  = '00' OR '97'                            ECS022
00704          NEXT SENTENCE                                            ECS022
00705        ELSE                                                       ECS022
00706          MOVE AM-FILE-STATUS         TO WS-ABEND-FILE-STATUS      ECS022
00707          MOVE ' ACCOUNT OPEN ERROR ' TO WS-ABEND-MESSAGE          ECS022
00708          GO TO ABEND-PGM.                                         ECS022
00709                                                                   ECS022
00710      IF (DTE-TOT-OPT = 2 OR 3 OR 4)                               ECS022
00711        AND                                                        ECS022
00712         (TEMP-EXTRACT-COUNT = +0)                                 ECS022
00713         GO TO 0550-OVER-BK.                                       ECS022
00714                                                                   ECS022
00715  0120-FIRST-SET.                                                  ECS022
00716      MOVE  WS-CURRENT-DATE       TO  H2-IPL.                      ECS022
00717      MOVE  ALPH-DATE             TO  H3-DATE.                     ECS022
00718      MOVE  COMPANY-NAME          TO  H1-CMP.                      ECS022
00719      MOVE  +0                    TO  X1.                          ECS022
00720                                                                   ECS022
00721  0130-CLEAR-40.                                                   ECS022
00722      MOVE  +0                    TO  X3.                          ECS022
00723                                                                   ECS022
00724  0140-CLEAR-LOOP.                                                 ECS022
00725      ADD  +1  TO  X3.                                             ECS022
00726      IF X3  GREATER  +40                                             CL*11
00727          GO TO 0150-EXIT.                                         ECS022
00728                                                                   ECS022
00729      ADD  +1  TO  X1.                                             ECS022
00730      MOVE  +0  TO  L-PREM (X1)  A-PREM (X1)  L-BEN (X1) A-BEN (X1)ECS022
00731                    L-CT   (X1)  A-CT   (X1).                      ECS022
00732      GO TO 0140-CLEAR-LOOP.                                       ECS022
00733                                                                   ECS022
00734  0150-EXIT.                                                       ECS022
00735      EXIT.                                                        ECS022
00736                                                                   ECS022
00737  0160-SET-UP.                                                     ECS022
00738      PERFORM  0130-CLEAR-40 THRU 0150-EXIT  6 TIMES.              ECS022
00739                                                                   ECS022
00740      MOVE  +0  TO  KOUNT  (1)  AGE (1)  TERM (1).                 ECS022
00741                                                                   ECS022
00742      MOVE  ACC2 (1)  TO  ACC2 (2) ACC2 (3) ACC2 (4) ACC2 (5).     ECS022
00743                                                                   ECS022
00744      MOVE ZERO TO WEIGHT-CNT (1) WEIGHT-FACE (1) WEIGHT-WORK (1). ECS022
00745                                                                   ECS022
00746      MOVE WEIGHT-CNTRS (1) TO WEIGHT-CNTRS (2) WEIGHT-CNTRS (3)   ECS022
00747                               WEIGHT-CNTRS (4) WEIGHT-CNTRS (5).  ECS022
00748                                                                   ECS022
00749  0170-SORT-ROUTINE.                                               ECS022
00750      SORT SORT-FILE ASCENDING  SR-REPORT-CD                       ECS022
00751                                SR-REPORT-CNTL                     ECS022
00752                                SR-REPT-CD1                        ECS022
00753                                SR-CARR-GROUP                      ECS022
00754                                SR-REPT-CD2                        ECS022
00755                                SR-STATE                           ECS022
00756                                SR-ACCOUNT                         ECS022
00757          INPUT PROCEDURE 0190-READ-ACCT THRU 0580-EXIT            ECS022
00758          GIVING SUMMARY.                                          ECS022
00759                                                                   ECS022
00760      IF SORT-RETURN NOT = ZEROS                                   ECS022
00761          MOVE '0101'             TO WS-RETURN-CODE                ECS022
00762          GO TO ABEND-PGM.                                         ECS022
00763                                                                   ECS022
00764      MOVE ZEROS  TO RETURN-CODE.

040304     PERFORM 9999-END-JOB.
00765                                                                   ECS022
00766  EJECT                                                            ECS022
00767  0190-READ-ACCT  SECTION.                                         ECS022
00768      READ  ACCOUNT INTO ACCOUNT-MASTER.                           ECS022
00769                                                                   ECS022
00770      IF AM-FILE-STATUS = '00' OR '10'                             ECS022
00771          MOVE AM-CARRIER         TO WS-AM-CARR                    ECS022
00772          MOVE AM-GROUPING        TO WS-AM-GRP                     ECS022
00773      ELSE                                                         ECS022
00774          MOVE AM-FILE-STATUS TO WS-ABEND-FILE-STATUS              ECS022
00775                                 VSAM-ERR-CODE                     ECS022
00776          MOVE VSAM-ERROR              TO WS-RETURN-CODE           ECS022
00777          MOVE 'ACCOUNT MASTER ERROR ' TO WS-ABEND-MESSAGE         ECS022
00778          GO TO ABEND-PGM.                                         ECS022
00779                                                                   ECS022
00780      IF AM-FILE-STATUS = '00'                                     ECS022
00781          ADD +1 TO ACCT-CNT.                                      ECS022
00782                                                                   ECS022
00783  0200-EXIT.                                                       ECS022
00784      EXIT.                                                        ECS022
00785                                                                   ECS022
00786  EJECT                                                            ECS022
00787  0210-READ-EXTRACT.                                               ECS022
00788      IF DTE-TOT-OPT = 2 OR 3 OR 4                                 ECS022
00789         READ TEMP-EXTRACT INTO DETAIL-EXTRACT AT END              ECS022
00790              GO TO 0550-OVER-BK                                   ECS022
00791      ELSE                                                         ECS022
00792         READ EXTRACT INTO DETAIL-EXTRACT AT END                   ECS022
00793              GO TO 0550-OVER-BK.                                  ECS022
00794                                                                   ECS022
00795      IF DE-RECORD-ID NOT = 'DE'                                   ECS022
00796           GO TO 0210-READ-EXTRACT.                                ECS022
00797                                                                   ECS022
00798      IF DE-REIN = 'R'                                             ECS022
00799          GO TO 0210-READ-EXTRACT.                                 ECS022
00800                                                                   ECS022
00801      IF DE-TRANS NOT = 'I'                                        ECS022
00802          GO TO 0210-READ-EXTRACT.                                 ECS022
00803                                                                   ECS022
00804      IF DE-ENTRY-STATUS = '5' OR '9' OR 'D' OR 'V'                ECS022
00805          GO TO 0210-READ-EXTRACT.                                 ECS022
00806                                                                   ECS022
00807      COPY ELCEXTM1.                                               ECS022
00808                                                                   ECS022
00809      MOVE DE-CARRIER             TO WS-DE-CARR.                   ECS022
00810      MOVE DE-GROUPING            TO WS-DE-GRP.                    ECS022
00811                                                                   ECS022
00812      MOVE SPACE                  TO WS-TYPE-SW.                   ECS022
00813                                                                   ECS022
00814  0220-TYPE-LOOKUP.                                                ECS022
00815      MOVE CLAS-STARTL            TO CLAS-INDEXL.                  ECS022
00816      MOVE CLAS-STARTA            TO CLAS-INDEXA.                  ECS022
00817                                                                   ECS022
00818      IF DE-LF-TYPE = ZERO                                         ECS022
00819          MOVE ZERO               TO CLAS-INDEXL                   ECS022
00820          GO TO 0220-FIND-AH.                                      ECS022
00821                                                                   ECS022
00822  0220-FIND-LIFE-LOOP.                                             ECS022
00823      IF CLAS-INDEXL GREATER CLAS-MAXL OR CLAS-STARTL = ZERO       ECS022
00824          DISPLAY 'LIFE BENEFIT ' DE-LF-TYPE ' NOT IN TABLE'       ECS022
00825          MOVE '0401'             TO WS-RETURN-CODE                ECS022
00826          GO TO ABEND-PGM.                                         ECS022
00827                                                                   ECS022
00828      IF DE-LF-TYPE NOT = CLAS-I-BEN (CLAS-INDEXL)                 ECS022
00829          ADD +1 TO CLAS-INDEXL                                    ECS022
00830          GO TO 0220-FIND-LIFE-LOOP.                               ECS022
00831                                                                   ECS022
00832       MOVE CLAS-I-CALC-TYPE (CLAS-INDEXL) TO WS-TYPE-SW.          ECS022
00833                                                                   ECS022
00834  0220-FIND-AH.                                                    ECS022
00835      IF DE-AH-TYPE = ZERO                                         ECS022
00836          MOVE ZERO               TO CLAS-INDEXA                   ECS022
00837          GO TO 0220-END-LOOKUP.                                   ECS022
00838                                                                   ECS022
00839  0220-FIND-AH-LOOP.                                               ECS022
00840      IF CLAS-INDEXA GREATER CLAS-MAXA OR CLAS-STARTA = ZEROS      ECS022
00841          DISPLAY 'A&H BENEFIT ' DE-AH-TYPE ' NOT IN TABLE'        ECS022
00842          MOVE '0402'             TO WS-RETURN-CODE                ECS022
00843          GO TO ABEND-PGM.                                         ECS022
00844                                                                   ECS022
00845      IF DE-AH-TYPE NOT = CLAS-I-BEN (CLAS-INDEXA)                 ECS022
00846          ADD +1 TO CLAS-INDEXA                                    ECS022
00847          GO TO 0220-FIND-AH-LOOP.                                 ECS022
00848                                                                   ECS022
00849       IF WS-TYPE-SW = SPACE                                       ECS022
00850           MOVE CLAS-I-CALC-TYPE (CLAS-INDEXA) TO WS-TYPE-SW.      ECS022
00851                                                                   ECS022
00852  0220-END-LOOKUP.                                                 ECS022
00853      IF OB-REC OR SUMMARY-REC                                     ECS022
00854          GO TO 0210-READ-EXTRACT.                                 ECS022
00855                                                                   ECS022
00856      ADD +1 TO EXTRACT-COUNT.                                     ECS022
00857                                                                   ECS022
00858  0220-EXIT.                                                       ECS022
00859       EXIT.                                                       ECS022
00860                                                                   ECS022
00861  EJECT                                                            ECS022
00862  0240-FIND-ACCT.                                                  ECS022
00863      IF WS-DE-CNTL = WS-AM-CNTL                                   ECS022
00864          GO TO 0250-CK-ST.                                        ECS022
00865                                                                   ECS022
00866      IF WS-DE-CNTL LESS WS-AM-CNTL                                ECS022
00867          DISPLAY 'INVALID COMPANY CODE  ' WS-DE-CNTL              ECS022
00868          MOVE '0301'         TO WS-RETURN-CODE                    ECS022
00869          GO TO ABEND-PGM.                                         ECS022
00870                                                                   ECS022
00871      PERFORM  0190-READ-ACCT  THRU  0200-EXIT.                    ECS022
00872                                                                   ECS022
00873      GO TO 0240-FIND-ACCT.                                        ECS022
00874                                                                   ECS022
00875  0250-CK-ST.                                                      ECS022
00876      IF DE-STATE = AM-STATE                                       ECS022
00877          GO TO 0260-CK-ACCT.                                      ECS022
00878                                                                   ECS022
00879      IF DE-STATE LESS AM-STATE                                    ECS022
00880          DISPLAY 'INVALID  STATE CODE  '  DE-STATE                ECS022
00881          MOVE '0301'         TO WS-RETURN-CODE                    ECS022
00882          GO TO ABEND-PGM.                                         ECS022
00883                                                                   ECS022
00884      PERFORM  0190-READ-ACCT  THRU  0200-EXIT.                    ECS022
00885                                                                   ECS022
00886      GO TO 0240-FIND-ACCT.                                        ECS022
00887                                                                   ECS022
00888  0260-CK-ACCT.                                                    ECS022
00889      IF DE-ACCOUNT = AM-ACCOUNT                                   ECS022
00890          GO TO 0270-CK-DATE.                                      ECS022
00891                                                                   ECS022
00892      IF DE-ACCOUNT LESS  AM-ACCOUNT                               ECS022
00893          DISPLAY 'INVALID ACCOUNT NUMBER  '  DE-ACCOUNT           ECS022
00894          MOVE '0301'         TO WS-RETURN-CODE                    ECS022
00895          GO TO ABEND-PGM.                                         ECS022
00896                                                                   ECS022
00897      PERFORM  0190-READ-ACCT  THRU  0200-EXIT.                    ECS022
00898                                                                   ECS022
00899      GO TO 0240-FIND-ACCT.                                        ECS022
00900                                                                   ECS022
00901  0270-CK-DATE.                                                    ECS022
00902      IF DE-EFF  LESS AM-EXPIRE-DT                                 ECS022
00903          GO TO 0270-FIND-X.                                       ECS022
00904                                                                   ECS022
00905      PERFORM  0190-READ-ACCT  THRU  0200-EXIT.                    ECS022
00906                                                                   ECS022
00907      GO TO 0240-FIND-ACCT.                                        ECS022
00908                                                                   ECS022
00909  0270-FIND-X.                                                     ECS022
00910      EXIT.                                                        ECS022
00911                                                                   ECS022
00912  EJECT                                                            ECS022
00913  0290-ACCUMULATE.                                                 ECS022
00914      IF DE-LF-TERM NOT = +0                                       ECS022
00915          MOVE DE-LF-TERM         TO WK-TERM                       ECS022
00916      ELSE                                                         ECS022
00917          MOVE DE-AH-TERM         TO WK-TERM.                      ECS022
00918                                                                   ECS022
00919      ADD +1 TO WEIGHT-CNT (1).                                    ECS022
00920                                                                   ECS022
00921      COMPUTE WEIGHT-FACE (1) =                                    ECS022
00922          (DE-LF-BEN + DE-LF-BEN-ALT) +                            ECS022
00923          (DE-AH-BEN * WK-TERM) + WEIGHT-FACE (1).                 ECS022
00924                                                                   ECS022
00925      COMPUTE WEIGHT-WORK (1) =                                    ECS022
00926          WEIGHT-WORK (1) + ((DE-AH-BEN * WK-TERM) * DE-AGE) +     ECS022
00927                      ((DE-LF-BEN + DE-LF-BEN-ALT) * DE-AGE).      ECS022
00928                                                                   ECS022
00929      ADD   +1      TO KOUNT (1).                                  ECS022
00930      ADD  DE-AGE   TO AGE   (1).                                  ECS022
00931      ADD  WK-TERM  TO TERM  (1).                                  ECS022
00932                                                                   ECS022
00933      IF DTE-CLIENT = 'MIL'                                        ECS022
00934          GO TO 0295-SET-MIL-AGES.                                 ECS022
00935                                                                   ECS022
00936      IF DE-AGE LESS +36                                           ECS022
00937          MOVE +1             TO X1                                ECS022
00938          GO TO 0300-GET-TRM.                                      ECS022
00939                                                                   ECS022
00940      IF DE-AGE LESS +41                                           ECS022
00941          MOVE +2             TO X1                                ECS022
00942          GO TO 0300-GET-TRM.                                      ECS022
00943                                                                   ECS022
00944      IF DE-AGE LESS +46                                           ECS022
00945          MOVE +3             TO X1                                ECS022
00946          GO TO 0300-GET-TRM.                                      ECS022
00947                                                                   ECS022
00948      IF DE-AGE LESS +51                                           ECS022
00949          MOVE +4             TO X1                                ECS022
00950          GO TO 0300-GET-TRM.                                      ECS022
00951                                                                   ECS022
00952      IF DE-AGE LESS +56                                           ECS022
00953          MOVE +5             TO X1                                ECS022
00954          GO TO 0300-GET-TRM.                                      ECS022
00955                                                                   ECS022
00956      IF DE-AGE LESS +65                                           ECS022
00957          MOVE +6             TO X1                                ECS022
00958          GO TO 0300-GET-TRM.                                      ECS022
00959                                                                   ECS022
00960      MOVE  +7                    TO X1.                           ECS022
00961                                                                   ECS022
00962      GO TO 0300-GET-TRM.                                          ECS022
00963                                                                   ECS022
00964  0295-SET-MIL-AGES.                                               ECS022
00965      IF DE-AGE LESS +41                                           ECS022
00966          MOVE +1             TO X1                                ECS022
00967          GO TO 0300-GET-TRM.                                      ECS022
00968                                                                   ECS022
00969      IF DE-AGE LESS +46                                           ECS022
00970          MOVE +2             TO X1                                ECS022
00971          GO TO 0300-GET-TRM.                                      ECS022
00972                                                                   ECS022
00973      IF DE-AGE LESS +51                                           ECS022
00974          MOVE +3             TO X1                                ECS022
00975          GO TO 0300-GET-TRM.                                      ECS022
00976                                                                   ECS022
00977      IF DE-AGE LESS +56                                           ECS022
00978          MOVE +4             TO X1                                ECS022
00979          GO TO 0300-GET-TRM.                                      ECS022
00980                                                                   ECS022
00981      IF DE-AGE LESS +60                                           ECS022
00982          MOVE +5             TO X1                                ECS022
00983          GO TO 0300-GET-TRM.                                      ECS022
00984                                                                   ECS022
00985      IF DE-AGE LESS +66                                           ECS022
00986          MOVE +6             TO X1                                ECS022
00987          GO TO 0300-GET-TRM.                                      ECS022
00988                                                                   ECS022
00989      MOVE  +7  TO X1.                                             ECS022
00990                                                                   ECS022
00991  0300-GET-TRM.                                                       CL**8
00992                                                                      CL**8
00996      IF WK-TERM LESS +19                                             CL**8
00997          GO TO 0310-ADD-1.                                           CL**8
00998                                                                      CL**8
00999      ADD +8  TO X1.                                                  CL**8
01000                                                                      CL**8
01001      IF WK-TERM LESS +37                                             CL**8
01002          GO TO 0310-ADD-1.                                           CL**8
01003                                                                      CL**8
01004      ADD +8  TO X1.                                                  CL**8
01005                                                                      CL**8
01006      IF WK-TERM LESS +61                                             CL**8
01007          GO TO 0310-ADD-1.                                           CL**8
01008                                                                      CL**8
01009      ADD +8  TO X1.                                                  CL**8
01010                                                                      CL**8
01030  0310-ADD-1.                                                      ECS022
01031      IF DE-LF-BEN NOT = ZERO                                      ECS022
01032          ADD  +1              TO  L-CT    (X1)                    ECS022
01033          ADD  DE-LF-PRM       TO  L-PREM  (X1)                    ECS022
01034          ADD  DE-LF-PRM-ALT   TO  L-PREM  (X1)                    ECS022
01035          ADD  DE-LF-BEN       TO  L-BEN   (X1)                    ECS022
01036          ADD  DE-LF-BEN-ALT   TO  L-BEN   (X1).                   ECS022
01037                                                                   ECS022
01038      IF DE-AH-TYPE NOT = ZERO                                     ECS022
01039          ADD  +1          TO  A-CT    (X1)                        ECS022
01040          ADD  DE-AH-PRM   TO  A-PREM  (X1)                        ECS022
01041          ADD  DE-AH-BEN   TO  A-BEN   (X1).                       ECS022
01042                                                                   ECS022
01043      PERFORM  0210-READ-EXTRACT  THRU  0220-EXIT.                 ECS022
01044                                                                   ECS022
01045      IF WS-DE-CNTL NOT = WS-AM-CNTL                               ECS022
01046          GO TO 0520-COMPANY-BK.                                   ECS022
01047                                                                   ECS022
01048      IF DE-STATE NOT = AM-STATE                                   ECS022
01049          GO TO 0490-STATE-BK.                                     ECS022
01050                                                                   ECS022
01051      IF DE-ACCOUNT NOT = AM-ACCOUNT                               ECS022
01052          GO TO 0450-ACCT-BK.                                      ECS022
01053                                                                   ECS022
01054      IF DE-EFF  NOT LESS  AM-EXPIRE-DT                               CL**5
01055          GO TO 0320-DATE-BK.                                      ECS022
01056                                                                   ECS022
01057      GO TO 0290-ACCUMULATE.                                       ECS022
01058                                                                   ECS022
01059      EJECT                                                        ECS022
01060  0320-DATE-BK.                                                    ECS022
01061      MOVE  +0                    TO  X1.                          ECS022
01062      MOVE  +33                   TO  X3.                          ECS022
01063      MOVE  +1                    TO  X2  X4.                      ECS022
01064                                                                   ECS022
01065  0330-DO-COL.                                                     ECS022
01066      ADD  +1   TO  X1.                                            ECS022
01067                                                                   ECS022
01068      IF X1  GREATER  +4                                           ECS022
01069          GO TO 0340-NEXT-COL.                                     ECS022
01070                                                                   ECS022
01071      ADD  L-PREM (X2)  TO  L-PREM (X3).                           ECS022
01072      ADD  A-PREM (X2)  TO  A-PREM (X3).                           ECS022
01073      ADD  L-BEN  (X2)  TO  L-BEN  (X3).                           ECS022
01074      ADD  A-BEN  (X2)  TO  A-BEN  (X3).                           ECS022
01075      ADD  L-CT   (X2)  TO  L-CT   (X3).                           ECS022
01076      ADD  A-CT   (X2)  TO  A-CT   (X3).                           ECS022
01077      ADD  +8   TO  X2.                                            ECS022
01078                                                                   ECS022
01079      GO TO 0330-DO-COL.                                           ECS022
01080                                                                   ECS022
01081  0340-NEXT-COL.                                                   ECS022
01082      ADD  +1  TO  X3.                                             ECS022
01083                                                                   ECS022
01084      IF X3 GREATER +39                                            ECS022
01085          GO TO  0350-DO-ACROSS.                                   ECS022
01086                                                                   ECS022
01087      MOVE  +0                    TO X1.                           ECS022
01088      ADD  +1  TO  X4.                                             ECS022
01089      MOVE  X4                    TO  X2.                          ECS022
01090                                                                   ECS022
01091      GO TO 0330-DO-COL.                                           ECS022
01092                                                                   ECS022
01093  0350-DO-ACROSS.                                                  ECS022
01094      MOVE  +0                    TO  X1.                          ECS022
01095      MOVE  +8                    TO  X2.                          ECS022
01096                                                                   ECS022
01097  0360-DO-ACC.                                                     ECS022
01098      ADD  +1  TO  X1.                                             ECS022
01099                                                                   ECS022
01100      IF X1  GREATER  +40                                          ECS022
01101          GO TO 0370-DO-SUMMARY.                                   ECS022
01102                                                                   ECS022
01103      IF X1 = X2                                                   ECS022
01104          ADD +8  TO X2                                            ECS022
01105          GO TO 0360-DO-ACC.                                       ECS022
01106                                                                   ECS022
01107      ADD  L-PREM (X1)  TO  L-PREM (X2).                           ECS022
01108      ADD  A-PREM (X1)  TO  A-PREM (X2).                           ECS022
01109      ADD  L-BEN  (X1)  TO  L-BEN  (X2).                           ECS022
01110      ADD  A-BEN  (X1)  TO  A-BEN  (X2).                           ECS022
01111      ADD  L-CT   (X1)  TO  L-CT   (X2).                           ECS022
01112      ADD  A-CT   (X1)  TO  A-CT   (X2).                           ECS022
01113                                                                   ECS022
01114      GO TO 0360-DO-ACC.                                           ECS022
01115                                                                   ECS022
01116  0370-DO-SUMMARY.                                                 ECS022
01117      MOVE  '1'                   TO S-CODE.                       ECS022
01118      MOVE  AM-ACCOUNT            TO S-RPT-CNTL                    ECS022
01119                                     S-ACCT.                       ECS022
PEMMOD     MOVE AM-CARRIER             TO S-RPT-CNTL (1:1)
PEMMOD                                    S-ACCT (1:1)
01120      MOVE  WS-AM-CNTL            TO S-CG.                         ECS022
01121      MOVE SPACES                 TO S-RPT-CD1                     ECS022
01122                                     S-RPT-CD2.                    ECS022
01123      MOVE  AM-STATE              TO S-ST.                         ECS022
01124      MOVE  +0                    TO X1.                           ECS022
01125      PERFORM   0680-LOAD-SUMMARY  THRU  0700-EXIT.                ECS022
01126                                                                   ECS022
01127      MOVE  KOUNT (1)             TO S-CT.                         ECS022
01128      MOVE  AGE   (1)             TO S-AGE.                        ECS022
01129      MOVE  TERM  (1)             TO S-TERM.                       ECS022
01130      MOVE  WEIGHT-CNT (1)        TO S-CNT.                        ECS022
01131      MOVE  WEIGHT-FACE (1)       TO S-FACE.                       ECS022
01132      MOVE  WEIGHT-WORK (1)       TO S-WORK.                       ECS022
01133      PERFORM  0710-WRITE-SUM  THRU  0720-EXIT.                    ECS022
01134                                                                   ECS022
01135      IF AM-AGT (1)   =  ZERO                                      ECS022
01136          GO TO  0380-GA-EXT.                                      ECS022
01137                                                                   ECS022
01138      MOVE  '2'                   TO  S-CODE.                      ECS022
01139      MOVE  AM-AGT (1)            TO  S-RPT-CNTL.                  ECS022
01140      PERFORM  0710-WRITE-SUM  THRU  0720-EXIT.                    ECS022
01141                                                                   ECS022
01142  0380-GA-EXT.                                                     ECS022
01143      IF AM-REPORT-CODE-2 NOT = SPACES AND ZEROS                   ECS022
01144          MOVE AM-REPORT-CODE-2   TO S-RPT-CD2                     ECS022
01145          MOVE SPACES             TO S-RPT-CNTL                    ECS022
01146                                     S-RPT-CD1                     ECS022
01147          MOVE  WS-AM-CNTL        TO S-CG                          ECS022
01148          MOVE  AM-STATE          TO S-ST                          ECS022
01149          MOVE  AM-ACCOUNT        TO S-ACCT                        ECS022
01150          MOVE '3'                TO S-CODE                        ECS022
01151          PERFORM 0710-WRITE-SUM THRU 0720-EXIT.                   ECS022
01152                                                                   ECS022
01153      MOVE  +1                    TO  X1.                          ECS022
01154  0390-GA-LOOP.                                                    ECS022
01155      ADD  +1  TO  X1.                                             ECS022
01156                                                                   ECS022
01157      IF X1 GREATER  +10                                           ECS022
01158          GO TO 0400-GA-EXIT.                                      ECS022
01159                                                                   ECS022
01160      IF AM-AGT (X1) = ZERO                                        ECS022
01161          GO TO 0390-GA-LOOP.                                      ECS022
01162                                                                   ECS022
01163      MOVE  '4'                   TO  S-CODE.                      ECS022
01164      MOVE AM-AGT (X1)            TO  S-RPT-CNTL.                  ECS022
01165      PERFORM  0710-WRITE-SUM  THRU  0720-EXIT.                    ECS022
01166      GO TO 0390-GA-LOOP.                                          ECS022
01167                                                                   ECS022
01168  0400-GA-EXIT.                                                    ECS022
01169      MOVE  '5'                   TO  S-CODE.                      ECS022
01170      MOVE  AM-GPCD               TO  S-RPT-CNTL.                  ECS022
01171      PERFORM  0710-WRITE-SUM  THRU  0720-EXIT.                    ECS022
01172                                                                   ECS022
01173      IF AM-REPORT-CODE-1 NOT = SPACES AND ZEROS                   ECS022
01174          MOVE AM-REPORT-CODE-1   TO S-RPT-CD1                     ECS022
01175                                     S-RPT-CNTL                    ECS022
01176          MOVE SPACES             TO S-RPT-CD2                     ECS022
01177          MOVE '7'                TO S-CODE                        ECS022
01178          PERFORM 0710-WRITE-SUM THRU 0720-EXIT.                   ECS022
01179                                                                   ECS022
01180      IF AM-REPORT-CODE-2 NOT = SPACES AND ZEROS                   ECS022
01181          MOVE AM-REPORT-CODE-2   TO S-RPT-CD2                     ECS022
01182          MOVE SPACES             TO S-RPT-CNTL                    ECS022
01183                                     S-RPT-CD1                     ECS022
01184          MOVE '8'                TO S-CODE                        ECS022
01185          PERFORM 0710-WRITE-SUM THRU 0720-EXIT.                   ECS022
01186                                                                   ECS022
01187  0410-DATE-BUMP.                                                  ECS022
01188      MOVE  +0                    TO  X1.                          ECS022
01189      MOVE  +40                   TO  X2.                          ECS022
01190      PERFORM  0650-BUMP-UP  THRU  0670-EXIT.                      ECS022
01191      ADD  KOUNT (1)  TO KOUNT (2).                                ECS022
01192      ADD  AGE   (1)  TO AGE   (2).                                ECS022
01193      ADD  TERM  (1)  TO TERM  (2).                                ECS022
01194      MOVE ZERO                   TO  KOUNT (1) AGE (1) TERM (1).  ECS022
01195      ADD WEIGHT-CNT  (1) TO WEIGHT-CNT  (2).                      ECS022
01196      ADD WEIGHT-FACE (1) TO WEIGHT-FACE (2).                      ECS022
01197      ADD WEIGHT-WORK (1) TO WEIGHT-WORK (2).                      ECS022
01198      MOVE ZERO TO WEIGHT-CNT (1) WEIGHT-FACE (1) WEIGHT-WORK (1). ECS022
01199                                                                   ECS022
01200  0420-EXIT.                                                       ECS022
01201      MOVE  +0                    TO  X1.                          ECS022
01202      PERFORM  0130-CLEAR-40  THRU 0150-EXIT.                      ECS022
01203                                                                   ECS022
01204  0430-DATE-EXIT.                                                  ECS022
01205      GO TO 0240-FIND-ACCT.                                        ECS022
01206  EJECT                                                            ECS022
01207                                                                   ECS022
01208  0450-ACCT-BK.                                                    ECS022
01209      PERFORM  0320-DATE-BK  THRU  0420-EXIT.                      ECS022
01210                                                                   ECS022
01211      IF DTE-PGM-OPT GREATER '1'                                   ECS022
01212          GO TO 0460-ACCT-BUMP.                                    ECS022
01213                                                                   ECS022
01214      MOVE 'CARRIER/GROUP-STATE'  TO H3-DES.                       ECS022
01215      MOVE  SPACES                TO H4-CTL.                       ECS022
01216      MOVE  WS-AM-CARR            TO H4-CARR.                      ECS022
01217      MOVE  WS-AM-GRP             TO H4-GRP.                       ECS022
01218      MOVE  AM-STATE              TO H4-ST.                        ECS022
01219      MOVE  AM-ACCOUNT            TO AT-NO.                        ECS022
01220      MOVE  H4-CTL                TO H4-DES.                       ECS022
01221      PERFORM  0620-HEADS  THRU  0630-EXIT.                        ECS022
01222      MOVE  AM-NAME               TO AT-NA.                        ECS022
01223      MOVE  +40                   TO X4.                           ECS022
01224                                                                   ECS022
01225      IF KOUNT (2) = ZERO                                          ECS022
01226          MOVE +1                 TO KOUNT (2).                    ECS022
01227                                                                   ECS022
01228      MOVE  KOUNT (2)             TO  D-CNT.                       ECS022
01229      COMPUTE  D-AGE   =   AGE (2)  /  KOUNT (2).                  ECS022
01230      COMPUTE  D-TERM  =   TERM (2) /  KOUNT (2).                  ECS022
01231                                                                   ECS022
01232      IF WEIGHT-FACE (2) NOT = ZERO                                ECS022
01233          COMPUTE D-WEIGHTED ROUNDED =                             ECS022
01234              (WEIGHT-WORK (2) / WEIGHT-FACE (2)).                 ECS022
01235                                                                   ECS022
01236      MOVE  ACCT-TOT              TO  HEAD-7.                      ECS022
01237      PERFORM   0740-DTL-PRT.                                      ECS022
01238                                                                   ECS022
01239  0460-ACCT-BUMP.                                                  ECS022
01240      MOVE  +40                   TO  X1.                          ECS022
01241      MOVE  +80                   TO  X2.                          ECS022
01242      PERFORM  0650-BUMP-UP  THRU  0670-EXIT.                      ECS022
01243      MOVE  +40                   TO  X1.                          ECS022
01244      PERFORM   0130-CLEAR-40  THRU  0150-EXIT.                    ECS022
01245      ADD  KOUNT (2)   TO  KOUNT  (3).                             ECS022
01246      ADD  AGE   (2)   TO  AGE    (3).                             ECS022
01247      ADD  TERM  (2)   TO  TERM   (3).                             ECS022
01248      MOVE  ZERO                  TO KOUNT (2)  AGE (2)  TERM (2). ECS022
01249      ADD WEIGHT-CNT  (2) TO WEIGHT-CNT  (3).                      ECS022
01250      ADD WEIGHT-FACE (2) TO WEIGHT-FACE (3).                      ECS022
01251      ADD WEIGHT-WORK (2) TO WEIGHT-WORK (3).                      ECS022
01252      MOVE ZERO TO WEIGHT-CNT (2) WEIGHT-FACE (2) WEIGHT-WORK (2). ECS022
01253                                                                   ECS022
01254  0470-ACCT-EXIT.                                                  ECS022
01255      GO TO 0240-FIND-ACCT.                                        ECS022
01256  EJECT                                                            ECS022
01257                                                                   ECS022
01258  0490-STATE-BK.                                                   ECS022
01259      PERFORM  0450-ACCT-BK  THRU  0460-ACCT-BUMP.                 ECS022
01260      MOVE  'CARRIER/GROUP'       TO H3-DES.                       ECS022
01261      MOVE  SPACES                TO H4-CTL.                       ECS022
01262      MOVE  WS-AM-CARR            TO H4-CARR.                      ECS022
01263      MOVE  WS-AM-GRP             TO H4-GRP.                       ECS022
01264      MOVE  H4-CTL                TO H4-DES.                       ECS022
01265      PERFORM  0620-HEADS  THRU 0630-EXIT.                         ECS022
01266      MOVE CLAS-STARTS            TO CLAS-INDEXS.                  ECS022
01267      PERFORM 0590-STATE-LOOKUP THRU 0600-EXIT.                    ECS022
01268      MOVE  STATE-TOT             TO H7-DES.                       ECS022
01269      MOVE  KOUNT (3)             TO D-CNT.                        ECS022
01270      COMPUTE  D-AGE   =   AGE (3)  /  KOUNT (3).                  ECS022
01271      COMPUTE  D-TERM  =   TERM (3) /  KOUNT (3).                  ECS022
01272                                                                   ECS022
01273      IF WEIGHT-FACE (3) NOT = ZERO                                ECS022
01274          COMPUTE D-WEIGHTED ROUNDED =                             ECS022
01275              (WEIGHT-WORK (3) / WEIGHT-FACE (3)).                 ECS022
01276                                                                   ECS022
01277      MOVE  +80                   TO X4.                           ECS022
01278      PERFORM    0740-DTL-PRT.                                     ECS022
01279      MOVE  +80                   TO X1.                           ECS022
01280      MOVE  +120                  TO X2.                           ECS022
01281      PERFORM   0650-BUMP-UP  THRU  0670-EXIT.                     ECS022
01282      MOVE  +80                   TO X1.                           ECS022
01283      PERFORM   0680-LOAD-SUMMARY  THRU  0700-EXIT.                ECS022
01284      MOVE  '6'                   TO S-CODE.                       ECS022
01285      MOVE  KOUNT  (3)            TO S-CT.                         ECS022
01286      MOVE  AGE    (3)            TO S-AGE.                        ECS022
01287      MOVE  TERM   (3)            TO S-TERM.                       ECS022
01288      MOVE WEIGHT-CNT (3)         TO S-CNT.                        ECS022
01289      MOVE WEIGHT-FACE (3)        TO S-FACE.                       ECS022
01290      MOVE WEIGHT-WORK (3)        TO S-WORK.                       ECS022
01291      MOVE  WS-AM-CNTL            TO S-CG.                         ECS022
01292      MOVE  AM-STATE              TO S-ST.                         ECS022
01293      MOVE  SPACES                TO S-RPT-CNTL                    ECS022
01294                                     S-RPT-CD2                     ECS022
01295                                     S-ACCT                        ECS022
01296                                     S-RPT-CD1.                    ECS022
01297      PERFORM  0710-WRITE-SUM  THRU  0720-EXIT.                    ECS022
01298      MOVE  +80                   TO X1.                           ECS022
01299      PERFORM   0130-CLEAR-40  THRU  0150-EXIT.                    ECS022
01300      ADD   KOUNT (3)   TO  KOUNT (4).                             ECS022
01301      ADD   AGE   (3)   TO  AGE   (4).                             ECS022
01302      ADD  TERM   (3)   TO  TERM  (4).                             ECS022
01303      MOVE  ZERO                  TO KOUNT (3)  AGE (3)  TERM (3). ECS022
01304      ADD WEIGHT-CNT  (3) TO WEIGHT-CNT  (4).                      ECS022
01305      ADD WEIGHT-FACE (3) TO WEIGHT-FACE (4).                      ECS022
01306      ADD WEIGHT-WORK (3) TO WEIGHT-WORK (4).                      ECS022
01307      MOVE ZERO TO WEIGHT-CNT (3) WEIGHT-FACE (3) WEIGHT-WORK (3). ECS022
01308                                                                   ECS022
01309  0500-STATE-EXIT.                                                 ECS022
01310      GO TO 0240-FIND-ACCT.                                        ECS022
01311  EJECT                                                            ECS022
01312                                                                   ECS022
01313  0520-COMPANY-BK.                                                 ECS022
01314      PERFORM   0490-STATE-BK.                                     ECS022
01315      MOVE  SPACES                TO  H3-DES                       ECS022
01316                                      H4-DES.                      ECS022
01317      PERFORM  0620-HEADS  THRU  0630-EXIT.                        ECS022
01318      MOVE  WS-AM-CARR            TO  CP-CARR.                     ECS022
01319      MOVE  WS-AM-GRP             TO  CP-GRP.                      ECS022
01320      MOVE  COMP-TOT              TO  H7-DES.                      ECS022
01321      MOVE  KOUNT (4)             TO   D-CNT.                      ECS022
01322      COMPUTE   D-AGE  =    AGE (4)  /  KOUNT (4).                 ECS022
01323      COMPUTE   D-TERM =    TERM (4) /  KOUNT (4).                 ECS022
01324                                                                   ECS022
01325      IF WEIGHT-FACE (4) NOT = ZERO                                ECS022
01326          COMPUTE D-WEIGHTED ROUNDED =                             ECS022
01327              (WEIGHT-WORK (4) / WEIGHT-FACE (4)).                 ECS022
01328                                                                   ECS022
01329      MOVE  +120                  TO X4.                           ECS022
01330      PERFORM  0740-DTL-PRT.                                       ECS022
01331      MOVE  +120                  TO X1.                           ECS022
01332      MOVE  +160                  TO X2.                           ECS022
01333      PERFORM   0650-BUMP-UP  THRU   0670-EXIT.                    ECS022
01334      MOVE  +120                  TO X1.                           ECS022
01335      PERFORM  0680-LOAD-SUMMARY  THRU   0700-EXIT.                ECS022
01336      MOVE  '9'                   TO S-CODE.                       ECS022
01337      MOVE  KOUNT  (4)            TO S-CT.                         ECS022
01338      MOVE  AGE    (4)            TO S-AGE.                        ECS022
01339      MOVE  TERM   (4)            TO S-TERM.                       ECS022
01340      MOVE WEIGHT-CNT (4)         TO S-CNT.                        ECS022
01341      MOVE WEIGHT-FACE (4)        TO S-FACE.                       ECS022
01342      MOVE WEIGHT-WORK (4)        TO S-WORK.                       ECS022
01343      MOVE  WS-AM-CNTL            TO S-CG.                         ECS022
01344      MOVE  SPACES                TO S-RPT-CNTL                    ECS022
01345                                     S-RPT-CD2                     ECS022
01346                                     S-ST                          ECS022
01347                                     S-ACCT                        ECS022
01348                                     S-RPT-CD1.                    ECS022
01349      PERFORM  0710-WRITE-SUM  THRU  0720-EXIT.                    ECS022
01350      MOVE  +120                  TO  X1.                          ECS022
01351      PERFORM   0130-CLEAR-40  THRU  0150-EXIT.                    ECS022
01352      ADD  KOUNT (4)  TO  KOUNT (5).                               ECS022
01353      ADD  AGE   (4)  TO  AGE   (5).                               ECS022
01354      ADD  TERM  (4)  TO  TERM  (5).                               ECS022
01355      MOVE  ZERO                  TO KOUNT (4) AGE (4)  TERM (4).  ECS022
01356      ADD WEIGHT-CNT  (4) TO WEIGHT-CNT  (5).                      ECS022
01357      ADD WEIGHT-FACE (4) TO WEIGHT-FACE (5).                      ECS022
01358      ADD WEIGHT-WORK (4) TO WEIGHT-WORK (5).                      ECS022
01359      MOVE ZERO TO WEIGHT-CNT (4) WEIGHT-FACE (4) WEIGHT-WORK (4). ECS022
01360                                                                   ECS022
01361  0530-COMPANY-EXIT.                                               ECS022
01362      GO TO 0240-FIND-ACCT.                                        ECS022
01363  EJECT                                                            ECS022
01364                                                                   ECS022
01365  0550-OVER-BK.                                                    ECS022
01366      IF EXTRACT-COUNT = ZERO                                      ECS022
01367         PERFORM 0620-HEADS THRU 0630-EXIT                         ECS022
01368         MOVE '-'                 TO P-CTL                            CL**2
01369         MOVE 'NO DATA ECS022'    TO H7-DES                        ECS022
01370         MOVE HEAD-7              TO P-DATA                        ECS022
01371         PERFORM 1040-PRT-LINE THRU 1060-EXIT                      ECS022
040304        PERFORM 0560-CLOSE-FILES THRU 0580-EXIT
040304        PERFORM 9999-END-JOB
040304     END-IF.
01373                                                                   ECS022
01374      PERFORM   0520-COMPANY-BK.                                   ECS022
01375      MOVE 'REPORT TOTALS'        TO H3-DES  H7-DES.               ECS022
01376      PERFORM   0620-HEADS   THRU  0630-EXIT.                      ECS022
01377      MOVE   KOUNT  (5)           TO   D-CNT.                      ECS022
01378      COMPUTE  D-AGE  =   AGE (5)   /  KOUNT (5).                  ECS022
01379      COMPUTE  D-TERM =   TERM (5)  /  KOUNT (5).                  ECS022
01380                                                                   ECS022
01381      IF WEIGHT-FACE (5) NOT = ZERO                                ECS022
01382          COMPUTE D-WEIGHTED ROUNDED =                             ECS022
01383              (WEIGHT-WORK (5) / WEIGHT-FACE (5)).                 ECS022
01384                                                                   ECS022
01385      MOVE  +160                  TO  X4.                          ECS022
01386      PERFORM  0740-DTL-PRT.                                       ECS022
01387      MOVE  +160                  TO  X1.                          ECS022
01388      PERFORM   0130-CLEAR-40  THRU  0150-EXIT.                    ECS022
01389      MOVE  ZERO                  TO KOUNT (5) AGE (5) TERM (5).   ECS022
01390                                                                   ECS022
01391  0560-CLOSE-FILES.                                                ECS022
01392      IF DTE-TOT-OPT = 2 OR 3 OR 4                                 ECS022
040304        IF TEMP-EXTRACT-COUNT GREATER THAN +0 
01393             CLOSE TEMP-EXTRACT  
040304        END-IF
01394      ELSE                                                         ECS022
01395         CLOSE EXTRACT.                                            ECS022
01396                                                                   ECS022
01397      CLOSE ACCOUNT PRINTER.                                       ECS022
01398                                                                   ECS022
01399  0570-CLOSE-FICH.                                                 ECS022
01400                              COPY ELCPRTC.                        ECS022
01401 *                                                                 ECS022
01402 *                                                                 ECS022
01403 * LEAVE THE ABOVE 2 LINES IN TO PREVENT LOSING THE LINE ABOVE     ECS022
01412                                                                   ECS022
01413  0580-EXIT.                                                       ECS022
01414       EXIT.                                                       ECS022
01415  EJECT                                                            ECS022
01416                                                                   ECS022
01417  0590-STATE-LOOKUP.                                               ECS022
01418      IF CLAS-MAXS NOT GREATER ZEROS                               ECS022
01419          MOVE 'INVALID STATE'    TO ST-DES                        ECS022
01420          GO TO 0600-EXIT.                                         ECS022
01421                                                                   ECS022
01422      IF CLAS-INDEXS GREATER CLAS-MAXS                             ECS022
01423          MOVE 'INVALID STATE'    TO ST-DES                        ECS022
01424          GO TO 0600-EXIT.                                         ECS022
01425                                                                   ECS022
01426      IF AM-STATE NOT = STATE-SUB (CLAS-INDEXS)                    ECS022
01427          ADD +1 TO CLAS-INDEXS                                    ECS022
01428          GO TO 0590-STATE-LOOKUP.                                 ECS022
01429                                                                   ECS022
01430      MOVE STATE-PIC (CLAS-INDEXS) TO ST-DES.                      ECS022
01431                                                                   ECS022
01432  0600-EXIT.                                                       ECS022
01433      EXIT.                                                        ECS022
01434                                                                   ECS022
01435  EJECT                                                            ECS022
01436  0620-HEADS.                                                      ECS022
01437      ADD  +1   TO  PAGE-CT.                                       ECS022
01438      MOVE  PAGE-CT               TO   H1-PAGE.                    ECS022
01439      MOVE  HEAD-1                TO  P-DATA.                      ECS022
01440      MOVE  '1'                   TO  P-CTL.                          CL**3
01441      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01442                                                                      CL*11
01443      MOVE  HEAD-2                TO  P-DATA.                      ECS022
01444      MOVE  ' '                   TO  P-CTL.                          CL**3
01445      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01446                                                                      CL*11
01447      MOVE  HEAD-3                TO  P-DATA.                      ECS022
01448      MOVE  ' '                   TO  P-CTL.                          CL**3
01449      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01450                                                                      CL*11
01451      MOVE HEAD-3A                TO P-DATA.                       ECS022
01452      MOVE ' '                    TO P-CTL.                           CL**3
01453      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01454                                                                      CL*11
01455      MOVE  HEAD-4                TO  P-DATA.                      ECS022
01456      MOVE  ' '                   TO  P-CTL.                          CL**3
01457      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01458                                                                      CL*11
01459      MOVE  ALL '*'               TO P-DATA.                       ECS022
01460      MOVE  ' '                   TO  P-CTL.                          CL**4
01461      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01462                                                                      CL*11
01463      MOVE  HEAD-5                TO  P-DATA.                      ECS022
01464      MOVE  ' '                   TO  P-CTL.                          CL**3
01465      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01466                                                                   ECS022
01470      MOVE  HEAD-6                TO  P-DATA.                      ECS022
01471                                                                   ECS022
01472      MOVE  ' '                   TO  P-CTL.                          CL**4
01473      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01474                                                                      CL*11
01475      MOVE  ALL '*'               TO  P-DATA.                      ECS022
01476      MOVE  ' '                   TO  P-CTL.                          CL**4
01477      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01478                                                                   ECS022
01479  0630-EXIT.                                                       ECS022
01480      EXIT.                                                        ECS022
01481  EJECT                                                            ECS022
01482                                                                   ECS022
01483  0650-BUMP-UP.                                                    ECS022
01484      MOVE +0                     TO  X3.                          ECS022
01485                                                                   ECS022
01486  0660-BUMP-LOOP.                                                  ECS022
01487      ADD  +1  TO  X3.                                             ECS022
01488                                                                   ECS022
01489      IF X3  GREATER  +40                                          ECS022
01490          GO TO  0670-EXIT.                                        ECS022
01491                                                                   ECS022
01492      ADD  +1  TO  X1 X2.                                          ECS022
01493                                                                   ECS022
01494      ADD  L-PREM (X1)  TO  L-PREM (X2).                           ECS022
01495      ADD  A-PREM (X1)  TO  A-PREM (X2).                           ECS022
01496      ADD  L-BEN  (X1)  TO  L-BEN  (X2).                           ECS022
01497      ADD  A-BEN  (X1)  TO  A-BEN  (X2).                           ECS022
01498      ADD  L-CT   (X1)  TO  L-CT   (X2).                           ECS022
01499      ADD  A-CT   (X1)  TO  A-CT   (X2).                           ECS022
01500                                                                   ECS022
01501      GO TO 0660-BUMP-LOOP.                                        ECS022
01502                                                                   ECS022
01503  0670-EXIT.                                                       ECS022
01504      EXIT.                                                        ECS022
01505      EJECT                                                        ECS022
01506                                                                   ECS022
01507  0680-LOAD-SUMMARY.                                               ECS022
01508      MOVE  +0                   TO  X2.                           ECS022
01509                                                                   ECS022
01510  0690-LOAD-SUM.                                                   ECS022
01511      ADD  +1   TO  X1  X2.                                        ECS022
01512                                                                   ECS022
01513      IF X2 GREATER +40                                            ECS022
01514          GO TO 0700-EXIT.                                         ECS022
01515                                                                   ECS022
01516      MOVE  L-PREM (X1)           TO  SL-P  (X2).                  ECS022
01517      MOVE  A-PREM (X1)           TO  SA-P  (X2).                  ECS022
01518      MOVE  L-BEN  (X1)           TO  SL-B  (X2).                  ECS022
01519      MOVE  A-BEN  (X1)           TO  SA-B  (X2).                  ECS022
01520      MOVE  L-CT   (X1)           TO  SL-C  (X2).                  ECS022
01521      MOVE  A-CT   (X1)           TO  SA-C  (X2).                  ECS022
01522                                                                   ECS022
01523      GO TO 0690-LOAD-SUM.                                         ECS022
01524                                                                   ECS022
01525  0700-EXIT.                                                       ECS022
01526      EXIT.                                                        ECS022
01527      EJECT                                                        ECS022
01528                                                                   ECS022
01529  0710-WRITE-SUM.                                                  ECS022
01530      RELEASE SORTWORK FROM SUM-REC.                               ECS022
01531                                                                   ECS022
01532  0720-EXIT.                                                       ECS022
01533      EXIT.                                                        ECS022
01534      EJECT                                                        ECS022
01535                                                                   ECS022
01536  0740-DTL-PRT.                                                    ECS022
01537      MOVE  HEAD-7                TO  P-DATA.                      ECS022
01538      MOVE  '0'                   TO  P-CTL.                          CL**2
01539      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01540                                                                   ECS022
01541      MOVE  SPACES                TO  HEAD-7.                      ECS022
01542      MOVE  ALL '*'               TO  H7-DES.                      ECS022
01543      MOVE  HEAD-7                TO  P-DATA.                      ECS022
01544      MOVE  ' '                   TO   P-CTL.                         CL**2
01545      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01546                                                                   ECS022
01547      MOVE  PA-HEAD               TO  P-DATA.                      ECS022
01548      MOVE  ' '                   TO  P-CTL.                          CL**2
01549      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01550                                                                   ECS022
01551      MOVE  HEAD-7                TO  P-DATA.                      ECS022
01552      MOVE  ' '                   TO  P-CTL.                          CL**2
01553      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01554                                                                   ECS022
01555      MOVE  LIFE-OVERRIDE-L6      TO  P-DATA.                      ECS022
01556      MOVE ' '                    TO  P-CTL.                          CL**2
01557      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01558                                                                   ECS022
01559      MOVE  SPACES                TO  DTL-LINE.                    ECS022
01560      PERFORM  0750-LIFE-PCTS  THRU  0790-DO-HOLD-13.              ECS022
01561                                                                   ECS022
01562      MOVE  +0                    TO  X2.                          ECS022
01563      MOVE  X4                    TO  X1.                          ECS022
01564      PERFORM   0860-TOTAL-SET  THRU  0890-EXIT.                   ECS022
01565                                                                   ECS022
01566      MOVE AH-OVERRIDE-L6         TO  P-DATA.                      ECS022
01567      MOVE  '-'                   TO  P-CTL.                          CL**4
01568      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01569                                                                   ECS022
01570      PERFORM  0800-A-H-PCTS   THRU  0840-DO-HOLD-A13.             ECS022
01571      MOVE  +1                    TO  X2.                          ECS022
01572      MOVE  X4                    TO  X1.                          ECS022
01573      PERFORM   0860-TOTAL-SET  THRU  0890-EXIT.                   ECS022
01574                                                                   ECS022
01575      MOVE  ALL '*'               TO  P-DATA.                      ECS022
01576      MOVE  '0'                   TO  P-CTL.                          CL**2
01577      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01578                                                                   ECS022
01579      MOVE  AE-HEAD               TO  P-DATA.                      ECS022
01580      MOVE  ' '                   TO  P-CTL.                          CL**2
01581      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01582                                                                   ECS022
01583      MOVE  HEAD-7                TO   P-DATA.                     ECS022
01584      MOVE  ' '                   TO  P-CTL.                          CL**2
01585      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01586                                                                   ECS022
01587      MOVE  LIFE-OVERRIDE-L6      TO  P-DATA.                      ECS022
01588      MOVE  '0'                   TO  P-CTL.                          CL**2
01589      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01590                                                                   ECS022
01591      MOVE  +2                    TO  X2.                          ECS022
01592      MOVE  X4                    TO  X1.                          ECS022
01593      MOVE  SPACES                TO  DTL-LINE.                    ECS022
01594                                                                   ECS022
01595      PERFORM 0850-ZERO-HOLD-PCT.                                  ECS022
01596      PERFORM 0860-TOTAL-SET.                                      ECS022
01597                                                                   ECS022
01598      MOVE  AH-OVERRIDE-L6        TO  P-DATA.                      ECS022
01599      MOVE  '0'                   TO  P-CTL.                          CL**2
01600      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01601                                                                   ECS022
01602      MOVE  +3                    TO  X2.                          ECS022
01603      MOVE  X4                    TO  X1.                          ECS022
01604      PERFORM  0860-TOTAL-SET.                                     ECS022
01605      MOVE  COUNT-LINE            TO P-DATA.                       ECS022
01606      MOVE  '0'                   TO P-CTL.                           CL**2
01607      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01608                                                                   ECS022
01609      MOVE  AGE-LINE              TO  P-DATA.                      ECS022
01610      MOVE  '0'                   TO P-CTL.                           CL**2
01611      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01612                                                                   ECS022
01613      MOVE  WEIGHTED-LINE         TO P-DATA.                       ECS022
01614      MOVE  '0'                   TO P-CTL.                           CL**2
01615      PERFORM   1040-PRT-LINE THRU  1060-EXIT.                     ECS022
01616                                                                   ECS022
01617      MOVE  TERM-LINE             TO  P-DATA.                      ECS022
01618      MOVE  '0'                   TO P-CTL.                           CL**2
01619      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01620                                                                   ECS022
01621  0750-LIFE-PCTS.                                                  ECS022
01622      COMPUTE  X2  =  X4  +  +40.                                  ECS022
01623                                                                   ECS022
01624      IF L-PREM (X2) = ZERO                                        ECS022
01625          PERFORM 0850-ZERO-HOLD-PCT                               ECS022
01626          GO TO 0790-DO-HOLD-13.                                   ECS022
01627                                                                   ECS022
01628      COMPUTE  X1  =  X4  +  +8.                                   ECS022
01629      MOVE  ZERO                  TO  TOT-PREM.                    ECS022
01630      MOVE  +1                    TO  X5.                          ECS022
01631      MOVE  +0                    TO  X3.                          ECS022
01632                                                                   ECS022
01633  0760-L-PCT-LOOP-1.                                               ECS022
01634      ADD  +1  TO  X3.                                             ECS022
01635                                                                   ECS022
01636      IF X3 GREATER +4                                             ECS022
01637          GO TO 0770-DO-HOLD-5.                                    ECS022
01638                                                                   ECS022
01639      COMPUTE  TOT-PREM  =  TOT-PREM  +  L-PREM  (X1).             ECS022
01640      COMPUTE HOLD-PCT (X5)  =  ((TOT-PREM  *  +100)  /            ECS022
01641              L-PREM (X2))  +  +.51.                               ECS022
01642      COMPUTE PRM1 = (L-PREM (X2) * HOLD-PCT (X5)) / +100.         ECS022
01643                                                                   ECS022
01644      IF PRM1 GREATER  TOT-PREM                                    ECS022
01645          SUBTRACT +1  FROM  HOLD-PCT (X5).                        ECS022
01646                                                                   ECS022
01647      COMPUTE TOT-PREM =  TOT-PREM - ((L-PREM (X2)  *              ECS022
01648              HOLD-PCT (X5))  /  +100).                            ECS022
01649                                                                   ECS022
01650      ADD  +8  TO  X1.                                             ECS022
01651      ADD  +1  TO  X5.                                             ECS022
01652                                                                   ECS022
01653      GO TO 0760-L-PCT-LOOP-1.                                     ECS022
01654                                                                   ECS022
01655  0770-DO-HOLD-5.                                                  ECS022
01656      COMPUTE HOLD-PCT (5) =  HOLD-PCT (1) + HOLD-PCT (2)  +       ECS022
01657                              HOLD-PCT (3) + HOLD-PCT (4).         ECS022
01658      COMPUTE X1  =  X4 +  +33.                                    ECS022
01659      MOVE  +6                    TO  X5.                          ECS022
01660      MOVE  +0                    TO  X3.                          ECS022
01661      MOVE  ZERO                  TO  TOT-PREM.                    ECS022
01662                                                                   ECS022
01663  0780-L-PCT-LOOP.                                                 ECS022
01664      ADD  +1   TO  X3.                                            ECS022
01665                                                                   ECS022
01666      IF X3 GREATER  +7                                            ECS022
01667          GO TO 0790-DO-HOLD-13.                                   ECS022
01668                                                                   ECS022
01669      COMPUTE  TOT-PREM =  TOT-PREM  +  L-PREM (X1).               ECS022
01670      COMPUTE HOLD-PCT (X5)  =  ((TOT-PREM  *  +100)  /            ECS022
01671          L-PREM (X2))  +  +.51.                                   ECS022
01672      COMPUTE PRM1 = (L-PREM (X2) * HOLD-PCT (X5)) / +100.         ECS022
01673                                                                   ECS022
01674      IF PRM1 GREATER TOT-PREM                                     ECS022
01675          SUBTRACT +1  FROM  HOLD-PCT (X5).                        ECS022
01676                                                                   ECS022
01677      COMPUTE TOT-PREM =  TOT-PREM - ((L-PREM (X2)  *              ECS022
01678                          HOLD-PCT (X5)) / +100).                  ECS022
01679                                                                   ECS022
01680      ADD  +1  TO X1                                               ECS022
01681                  X5.                                              ECS022
01682                                                                   ECS022
01683      GO TO 0780-L-PCT-LOOP.                                       ECS022
01684                                                                   ECS022
01685  0790-DO-HOLD-13.                                                 ECS022
01686      COMPUTE  HOLD-PCT (13)  =  HOLD-PCT (6) + HOLD-PCT (7) +     ECS022
01687               HOLD-PCT (8)   +  HOLD-PCT (9) + HOLD-PCT (10) +    ECS022
01688               HOLD-PCT (11)  +  HOLD-PCT (12).                    ECS022
01689                                                                   ECS022
01690  0800-A-H-PCTS.                                                   ECS022
01691      COMPUTE X2 =  X4  +  +40.                                    ECS022
01692                                                                   ECS022
01693      IF A-PREM (X2) = ZERO                                        ECS022
01694          PERFORM 0850-ZERO-HOLD-PCT                               ECS022
01695          GO TO 0840-DO-HOLD-A13.                                  ECS022
01696                                                                   ECS022
01697      COMPUTE X1 =  X4  +  +8.                                     ECS022
01698      MOVE  ZERO                  TO  TOT-PREM.                    ECS022
01699      MOVE  +1                    TO  X5.                          ECS022
01700      MOVE  +0                    TO  X3.                          ECS022
01701                                                                   ECS022
01702  0810-A-PCT-L-1.                                                  ECS022
01703      ADD   +1  TO  X3.                                            ECS022
01704                                                                   ECS022
01705      IF X3 GREATER  +4                                            ECS022
01706          GO TO 0820-DO-HOLD-A5.                                   ECS022
01707                                                                   ECS022
01708      COMPUTE TOT-PREM  =  TOT-PREM  +  A-PREM (X1).               ECS022
01709      COMPUTE HOLD-PCT (X5)  =  ((TOT-PREM  *  +100)  /            ECS022
01710          A-PREM (X2))  +  +.51.                                   ECS022
01711      COMPUTE PRM1 = (A-PREM (X2) * HOLD-PCT (X5)) / +100.         ECS022
01712                                                                   ECS022
01713      IF PRM1 GREATER TOT-PREM                                     ECS022
01714          SUBTRACT +1  FROM  HOLD-PCT (X5).                        ECS022
01715                                                                   ECS022
01716      COMPUTE TOT-PREM  =  TOT-PREM -  ((A-PREM (X2) *             ECS022
01717                           HOLD-PCT (X5))  / +100).                ECS022
01718                                                                   ECS022
01719      ADD +8  TO X1.                                               ECS022
01720      ADD +1  TO X5.                                               ECS022
01721                                                                   ECS022
01722      GO TO 0810-A-PCT-L-1.                                        ECS022
01723                                                                   ECS022
01724  0820-DO-HOLD-A5.                                                 ECS022
01725      COMPUTE HOLD-PCT (5) =  HOLD-PCT (1)  +  HOLD-PCT (2)  +     ECS022
01726                              HOLD-PCT (3)  +  HOLD-PCT (4).       ECS022
01727                                                                   ECS022
01728      COMPUTE  X1  =  X4  +  +33.                                  ECS022
01729      MOVE  +6                    TO  X5.                          ECS022
01730      MOVE  +0                    TO  X3.                          ECS022
01731      MOVE  ZERO                  TO  TOT-PREM.                    ECS022
01732                                                                   ECS022
01733  0830-A-PCT-L.                                                    ECS022
01734      ADD  +1  TO  X3.                                             ECS022
01735                                                                   ECS022
01736      IF X3 GREATER  +7                                            ECS022
01737          GO TO 0840-DO-HOLD-A13.                                  ECS022
01738                                                                   ECS022
01739      COMPUTE  TOT-PREM =  TOT-PREM  +  A-PREM (X1).               ECS022
01740      COMPUTE HOLD-PCT (X5)  =  ((TOT-PREM  *  +100)  /            ECS022
01741          A-PREM (X2))  +  +.51.                                   ECS022
01742      COMPUTE PRM1 = (A-PREM (X2) * HOLD-PCT (X5)) / +100.         ECS022
01743                                                                   ECS022
01744      IF PRM1 GREATER TOT-PREM                                     ECS022
01745          SUBTRACT +1  FROM  HOLD-PCT (X5).                        ECS022
01746                                                                   ECS022
01747      COMPUTE  TOT-PREM =  TOT-PREM -  ((A-PREM (X2) *             ECS022
01748                  HOLD-PCT (X5))  /  +100).                        ECS022
01749                                                                   ECS022
01750      ADD  +1  TO  X1                                              ECS022
01751                   X5.                                             ECS022
01752                                                                   ECS022
01753      GO TO 0830-A-PCT-L.                                          ECS022
01754                                                                   ECS022
01755  0840-DO-HOLD-A13.                                                ECS022
01756      COMPUTE  HOLD-PCT (13) =  HOLD-PCT (7)  +  HOLD-PCT (8)  +   ECS022
01757               HOLD-PCT (9)  +  HOLD-PCT (10) +  HOLD-PCT (11) +   ECS022
01758               HOLD-PCT (12) +  HOLD-PCT (6).                      ECS022
01759                                                                   ECS022
01760  0850-ZERO-HOLD-PCT.                                              ECS022
01761      MOVE ZERO TO HOLD-PCT (1)  HOLD-PCT (2)  HOLD-PCT (3)        ECS022
01762                   HOLD-PCT (4)  HOLD-PCT (5)  HOLD-PCT (6)        ECS022
01763                   HOLD-PCT (7)  HOLD-PCT (8)  HOLD-PCT (9)        ECS022
01764                   HOLD-PCT (10) HOLD-PCT (11) HOLD-PCT (12)       ECS022
01765                   HOLD-PCT (13).                                  ECS022
01766                                                                   ECS022
01767  0860-TOTAL-SET.                                                  ECS022
01768      PERFORM  0900-LOAD-8 THRU 0910-EXIT.                         ECS022
01769                                                                   ECS022
01770      MOVE  HOLD-PCT (1)          TO  D-EPCT.                      ECS022
01771                                                                   ECS022
01772      IF HOLD-PCT (1) NOT = ZERO                                   ECS022
01773          MOVE '%'                TO  D-EPCT1                      ECS022
01774        ELSE                                                       ECS022
01775          MOVE  ' '               TO  D-EPCT1.                     ECS022
01776                                                                   ECS022
01777      IF DTE-CLIENT = 'NCB'                                           CL**8
01778          MOVE  'T  0-48'         TO  D-DES                           CL**8
01779      ELSE                                                            CL**8
01780          MOVE  'T  0-18'         TO  D-DES.                          CL**8
01781      MOVE  DTL-LINE              TO  P-DATA.                      ECS022
01782      MOVE ' '                    TO  P-CTL.                          CL**4
01783                                                                   ECS022
01784      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01785      PERFORM  0900-LOAD-8  THRU 0910-EXIT.                        ECS022
01786                                                                   ECS022
01787      MOVE  HOLD-PCT (2)          TO  D-EPCT.                      ECS022
01788                                                                   ECS022
01789      IF HOLD-PCT (2) NOT = ZERO                                   ECS022
01790          MOVE '%'                TO  D-EPCT1                      ECS022
01791        ELSE                                                       ECS022
01792          MOVE  ' '               TO  D-EPCT1.                     ECS022
01793                                                                   ECS022
01797      MOVE  'E 19-36'             TO  D-DES.       
01798      MOVE  DTL-LINE              TO  P-DATA.                      ECS022
01799      MOVE ' '                    TO  P-CTL.                          CL**4
01800                                                                   ECS022
01801      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01802      PERFORM 0900-LOAD-8   THRU 0910-EXIT.                        ECS022
01803                                                                   ECS022
01804      MOVE  HOLD-PCT (3)          TO  D-EPCT.                      ECS022
01805                                                                   ECS022
01806      IF HOLD-PCT (3) NOT = ZERO                                   ECS022
01807          MOVE '%'                TO  D-EPCT1                      ECS022
01808        ELSE                                                       ECS022
01809          MOVE  ' '               TO  D-EPCT1.                     ECS022
01810                                                                   ECS022
01814      MOVE  'R 37-60'             TO  D-DES.    
01815      MOVE  DTL-LINE              TO  P-DATA.                      ECS022
01816      MOVE  ' '                   TO  P-CTL.                          CL**4
01817                                                                   ECS022
01818      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01819      PERFORM 0900-LOAD-8   THRU 0910-EXIT.                        ECS022
01820                                                                   ECS022
01821      MOVE  HOLD-PCT (4)          TO  D-EPCT.                      ECS022
01822                                                                   ECS022
01823      IF HOLD-PCT (4) NOT = ZERO                                   ECS022
01824          MOVE '%'                TO  D-EPCT1                      ECS022
01825        ELSE                                                       ECS022
01826          MOVE  ' '               TO  D-EPCT1.                     ECS022
01827                                                                   ECS022
01831      MOVE  'M 61---'             TO  D-DES.   
01832      MOVE  DTL-LINE              TO  P-DATA.                      ECS022
01833      MOVE  ' '                   TO  P-CTL.                          CL**5
01834                                                                   ECS022
01835      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01836      PERFORM 0900-LOAD-8   THRU 0910-EXIT.                        ECS022
01837                                                                   ECS022
01838      MOVE  HOLD-PCT (5)          TO  D-EPCT.                      ECS022
01839                                                                   ECS022
01840      IF HOLD-PCT (5) NOT = ZERO                                   ECS022
01841          MOVE '%'                TO D-EPCT1                       ECS022
01842        ELSE                                                       ECS022
01843          MOVE  ' '               TO D-EPCT1.                      ECS022
01844                                                                   ECS022
01845      MOVE  ' TOTAL'              TO D-DES2.                       ECS022
01846      MOVE  DTL-LINE              TO P-DATA.                       ECS022
01847      MOVE  '0'                   TO P-CTL.                           CL**2
01848      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01849      MOVE  SPACES                TO  DTL-LINE.                    ECS022
01850      MOVE  +6                    TO  X1.                          ECS022
01851      MOVE  +0                    TO  X2.                          ECS022
01852                                                                   ECS022
01853  0870-PCT-LINE.                                                   ECS022
01854      ADD  +1  TO   X2.                                            ECS022
01855                                                                   ECS022
01856      IF X2 GREATER  +8                                            ECS022
01857          GO TO 0880-PCT-LOADED.                                   ECS022
01858                                                                   ECS022
01859      MOVE  HOLD-PCT (X1)         TO  D-PCT (X2).                  ECS022
01860                                                                   ECS022
01861      IF HOLD-PCT (X1) NOT = ZERO                                  ECS022
01862          MOVE '%'                TO  D-PCT1 (X2)                  ECS022
01863        ELSE                                                       ECS022
01864          MOVE ' '                TO  D-PCT1 (X2).                 ECS022
01865                                                                   ECS022
01866      ADD  +1  TO  X1.                                             ECS022
01867      GO TO 0870-PCT-LINE.                                         ECS022
01868                                                                   ECS022
01869  0880-PCT-LOADED.                                                 ECS022
01870      MOVE  'PERCENT'             TO  D-DES.                       ECS022
01871      MOVE  ' '                   TO  P-CTL.                          CL**2
01872      MOVE  DTL-LINE              TO  P-DATA.                      ECS022
01873      PERFORM 1040-PRT-LINE THRU 1060-EXIT.                        ECS022
01874                                                                   ECS022
01875  0890-EXIT.                                                       ECS022
01876      EXIT.                                                        ECS022
01877                                                                   ECS022
01878  0900-LOAD-8.                                                     ECS022
01879      IF X2 = +0                                                   ECS022
01880          PERFORM 0920-LIFE-PREM-LOAD THRU 0940-EXIT.              ECS022
01881      IF X2 = +1                                                   ECS022
01882          PERFORM 0950-A-H-PREM-LOAD  THRU 0970-EXIT.              ECS022
01883      IF X2 = +2                                                   ECS022
01884          PERFORM 0980-LIFE-BEN-LOAD  THRU 1000-EXIT.              ECS022
01885      IF X2 = +3                                                   ECS022
01886          PERFORM 1010-A-H-BEN-LOAD   THRU 1030-EXIT.              ECS022
01887                                                                   ECS022
01888  0910-EXIT.                                                       ECS022
01889      EXIT.                                                        ECS022
01890                                                                   ECS022
01891  0920-LIFE-PREM-LOAD.                                             ECS022
01892      MOVE   +0                   TO  X3.                          ECS022
01893                                                                   ECS022
01894  0930-L-P-L.                                                      ECS022
01895      ADD  +1   TO  X3.                                            ECS022
01896                                                                   ECS022
01897      IF X3 GREATER +8                                             ECS022
01898          GO TO  0940-EXIT.                                        ECS022
01899                                                                   ECS022
01900      ADD  +1   TO  X1.                                            ECS022
01901      MOVE  L-PREM (X1)           TO   D-AMT (X3).                 ECS022
01902                                                                   ECS022
01903      GO TO 0930-L-P-L.                                            ECS022
01904                                                                   ECS022
01905  0940-EXIT.                                                       ECS022
01906      EXIT.                                                        ECS022
01907                                                                   ECS022
01908  0950-A-H-PREM-LOAD.                                              ECS022
01909      MOVE  +0                    TO  X3.                          ECS022
01910                                                                   ECS022
01911  0960-A-P-L.                                                      ECS022
01912      ADD  +1  TO  X3.                                             ECS022
01913      IF X3 GREATER  +8                                            ECS022
01914          GO TO 0970-EXIT.                                         ECS022
01915      ADD  +1  TO  X1.                                             ECS022
01916      MOVE  A-PREM (X1)           TO  D-AMT (X3).                  ECS022
01917                                                                   ECS022
01918      GO TO 0960-A-P-L.                                            ECS022
01919                                                                   ECS022
01920  0970-EXIT.                                                       ECS022
01921      EXIT.                                                        ECS022
01922                                                                   ECS022
01923  0980-LIFE-BEN-LOAD.                                              ECS022
01924      MOVE +0                     TO  X3.                          ECS022
01925                                                                   ECS022
01926  0990-L-B-L.                                                      ECS022
01927      ADD  +1  TO  X3.                                             ECS022
01928                                                                   ECS022
01929      IF X3 GREATER +8                                             ECS022
01930          GO TO 1000-EXIT.                                         ECS022
01931                                                                   ECS022
01932      ADD  +1  TO  X1.                                             ECS022
01933                                                                   ECS022
01934      IF L-CT (X1) =  ZERO                                         ECS022
01935          MOVE  L-BEN (X1) TO D-AMT (X3)                           ECS022
01936          GO TO 0990-L-B-L.                                        ECS022
01937                                                                   ECS022
01938      COMPUTE D-AMT (X3) ROUNDED = L-BEN (X1)  /  L-CT (X1).       ECS022
01939                                                                   ECS022
01940      GO TO 0990-L-B-L.                                            ECS022
01941                                                                   ECS022
01942  1000-EXIT.                                                       ECS022
01943      EXIT.                                                        ECS022
01944                                                                   ECS022
01945  1010-A-H-BEN-LOAD.                                               ECS022
01946      MOVE  +0                    TO X3.                           ECS022
01947                                                                   ECS022
01948  1020-A-B-L.                                                      ECS022
01949      ADD  +1  TO  X3.                                             ECS022
01950      IF X3 GREATER  +8                                            ECS022
01951          GO TO 1030-EXIT.                                         ECS022
01952                                                                   ECS022
01953      ADD  +1  TO  X1.                                             ECS022
01954      IF A-CT (X1) = ZERO                                          ECS022
01955          MOVE  A-BEN (X1)        TO  D-AMT (X3)                   ECS022
01956          GO TO  1020-A-B-L.                                       ECS022
01957                                                                   ECS022
01958      COMPUTE D-AMT (X3) ROUNDED = A-BEN (X1)  /  A-CT (X1).       ECS022
01959                                                                   ECS022
01960      GO TO 1020-A-B-L.                                            ECS022
01961                                                                   ECS022
01962  1030-EXIT.                                                       ECS022
01963      EXIT.                                                        ECS022
01964                                                                   ECS022
01965  1040-PRT-LINE.                                                   ECS022
01966      MOVE P-CTL                  TO X.                            ECS022
01967                                                                   ECS022
01968  1050-PRT-LINE-GO.                                                ECS022
01969                              COPY ELCPRT2.                        ECS022
01970                                                                   ECS022
01971  1060-EXIT.                                                       ECS022
01972      EXIT.                                                        ECS022
01973                                                                   ECS022
01974  1080-GET-REMAINING-TERM.                                         ECS022
01975                                                                   ECS022
01976      CALL  'ELRTRMX'    USING  CALCULATION-PASS-AREA.             ECS022
01977                                                                   ECS022
01978  1080-EXIT.                                                       ECS022
01979      EXIT.                                                        ECS022
01980                                                                   ECS022
01981  1090-DATE-CONVERSION.                                            ECS022
01982                                                                   ECS022
01983      CALL  'ELDATCX'    USING  DATE-CONVERSION-DATA.              ECS022
01984                                                                   ECS022
01985  1090-EXIT.                                                       ECS022
01986      EXIT.                                                        ECS022
01987                                                                   ECS022
01988  ABEND-PGM SECTION.                                               ECS022
01989                         COPY ELCABEND.                            ECS022
040304 9999-END-JOB.
040304
040304     GOBACK.
040304
040304 9999-EXIT.
040304     EXIT.
