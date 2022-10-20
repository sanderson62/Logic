00001  IDENTIFICATION DIVISION.                                         06/10/98
00002                                                                   ECS023
00003  PROGRAM-ID.                 ECS023.                                 LV003
00004 *              PROGRAM CONVERTED BY                               ECS023
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS023
00006 *              CONVERSION DATE 11/28/95 11:04:47.                 ECS023
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS023
00008 *                          VMOD=2.011                             ECS023
00009                                                                   ECS023
00010 *AUTHOR.        LOGIC, INC.                                       ECS023
00011 *               DALLAS, TEXAS.                                    ECS023
00012                                                                   ECS023
00013 *DATE-COMPILED.                                                   ECS023
00014                                                                   ECS023
00015 *SECURITY.   *****************************************************ECS023
00016 *            *                                                   *ECS023
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS023
00018 *            *                                                   *ECS023
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS023
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS023
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS023
00022 *            *                                                   *ECS023
00023 *            *****************************************************ECS023
00024                                                                   ECS023
00025 *REMARKS.                                                         ECS023
00026 *          ****  RISK DISTRIBUTION REPORT ****                    ECS023
00027 *                                                                 ECS023
00028 * PGM-OPT S-CODE COMING FROM ECS022:                              ECS023
00029 *    1......1 = RISK SUMMARY BY ACCOUNT..........(ECS-023A)       ECS023
00030 *    2......2 = RISK SUMMARY BY LEV 1 AGENT......(ECS-023B)       ECS023
00031 *    3......3 = RISK SUMMARY BY RPT CD 2 W/CO....(ECS-023C)       ECS023
00032 *    4......4 = RISK SUMMARY BY LEV 2 & 3 AGENT..(ECS-023D)       ECS023
00033 *    5......5 = RISK SUMMARY BY TYPE OF BUSINESS.(ECS-023E)       ECS023
00034 *    6......6 = RISK SUMMARY BY STATE............(ECS-023F)       ECS023
00035 *    7......9 = RISK SUMMARY BY COMPANY..........(ECS-023G)       ECS023
00036 *                                                                 ECS023
00037 *           7 = REPORT CODE 1 TOTALS                              ECS023
00038 *           8 = REPORT CODE 2 TOTALS                              ECS023
00039                                                                   ECS023
00040 * TOT-OPT                                                         ECS023
00041 *    1......REPORT CODES NOT USED                                 ECS023
00042 *    2......USE REPORT CODE 1 ONLY                                ECS023
00043 *    3......USE REPORT CODE 2 ONLY                                ECS023
00044 *    4......USE BOTH REPORT CODES                                 ECS023
00045  EJECT                                                            ECS023
00046  ENVIRONMENT DIVISION.                                            ECS023
00047  CONFIGURATION SECTION.                                           ECS023
00048  SPECIAL-NAMES.                                                   ECS023
00049      C02 IS LCP-CH2                                               ECS023
00050      C03 IS LCP-CH3                                               ECS023
00051      C04 IS LCP-CH4                                               ECS023
00052      C05 IS LCP-CH5                                               ECS023
00053      C06 IS LCP-CH6                                               ECS023
00054      C07 IS LCP-CH7                                               ECS023
00055      C08 IS LCP-CH8                                               ECS023
00056      C09 IS LCP-CH9                                               ECS023
00057      C10 IS LCP-CH10                                              ECS023
00058      C11 IS LCP-CH11                                              ECS023
00059      C12 IS LCP-CH12                                              ECS023
00060      S01 IS LCP-P01                                               ECS023
00061      S02 IS LCP-P02.                                              ECS023
00062  INPUT-OUTPUT SECTION.                                            ECS023
00063  FILE-CONTROL.                                                    ECS023
00064                                                                   ECS023
00065      SELECT  SORT-FILE   ASSIGN TO SYS001-UT-3380-S-SORTWK1.      ECS023
00066      SELECT  PRINTER     ASSIGN TO SYS008-UR-1403-S-SYS008.       ECS023
00067      SELECT  SUMMARY     ASSIGN TO SYS011-UT-2400-S-SYS011.       ECS023
00068      SELECT  ACCOUNT     ASSIGN TO ERACCTT                        ECS023
00069                          ACCESS         SEQUENTIAL                ECS023
00070                          ORGANIZATION   INDEXED                   ECS023
00071                          FILE STATUS    AM-FILE-STATUS            ECS023
00072                          RECORD KEY     AM-KEY.                   ECS023
00073      SELECT  DISK-DATE   ASSIGN TO SYS019-UT-3380-S-SYS019.       ECS023
00074      SELECT  FICH        ASSIGN TO SYS020-UT-2400-S-SYS020.       ECS023
00075  EJECT                                                            ECS023
00076  DATA DIVISION.                                                   ECS023
00077  FILE SECTION.                                                    ECS023
00078                                                                   ECS023
00079  SD  SORT-FILE.                                                   ECS023
00080                                                                   ECS023
00081  01  SORT-REC.                                                    ECS023
00082      05  FILLER      PIC X(3).                                    ECS023
00083      05  SORT-KEY3   PIC X(7).                                    ECS023
00084      05  SORT-KEY2   PIC XX.                                      ECS023
00085      05  SORT-KEY1   PIC X(10).                                   ECS023
CIDMOD     05  SORT-KEY4   PIC X(6).
00086      05  FILLER      PIC X(1972).                                 ECS023
00087                                                                   ECS023
00088  FD  PRINTER                                                      ECS023
00089      COPY ELCPRTFD.                                               ECS023
00090  EJECT                                                            ECS023
00091  FD  SUMMARY                                                      ECS023
00092      BLOCK CONTAINS 0 RECORDS
00093      RECORDING MODE F.                                            ECS023
00094                                                                   ECS023
00095  01  SUM-REC1.                                                    ECS023
00096      12  SREC-CODE           PIC X.                               ECS023
020405     12  FILLER              PIC X(1475).
00098                                                                   ECS023
00099  FD  ACCOUNT.                                                     ECS023
00100                                                                   ECS023
00101  01  AM-MSTR.                                                     ECS023
00102      05  FILLER              PIC XX.                              ECS023
00103      05  AM-KEY              PIC X(26).                           ECS023
00104      05  FILLER              PIC X(1972).                         ECS023
00105  EJECT                                                            ECS023
00106  FD  DISK-DATE                                                    ECS023
00107      COPY ELCDTEFD.                                               ECS023
00108  EJECT                                                            ECS023
00109  FD  FICH                                                         ECS023
00110      COPY ELCFCHFD.                                               ECS023
00111  EJECT                                                            ECS023
00112  WORKING-STORAGE SECTION.                                         ECS023
00113  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS023
00114  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.   ECS023
00115  77  LCP-ASA                       PIC X.                         ECS023
00116  77  FILLER  PIC X(32) VALUE '********************************'.  ECS023
00117  77  FILLER  PIC X(32) VALUE '     ECS023 WORKING STORAGE     '.  ECS023
00118  77  FILLER  PIC X(32) VALUE '*****VMOD=2.011*****************'.  ECS023
00119                                                                   ECS023
00120  77  SUMMARY-COUNT           PIC  9(9)  COMP     VALUE ZERO.      ECS023
00121  77  FIRST-SW                PIC  X              VALUE 'X'.       ECS023
00122                                                                   ECS023
00123  EJECT                                                            ECS023
00124      COPY ERCACCT.                                                ECS023
00125  EJECT                                                            ECS023
00126  01  HEADINGS.                                                    ECS023
00127      12  HEAD-1.                                                  ECS023
00128          16  FILLER  PIC X(53)   VALUE SPACES.                    ECS023
00129          16  FILLER  PIC X(17)   VALUE 'RISK DISTRIBUTION'.       ECS023
00130          16  FILLER  PIC X(49)   VALUE SPACES.                    ECS023
00131          16  FILLER  PIC X(6)    VALUE 'ECS023'.                  ECS023
00132          16  H1-RPT  PIC X       VALUE ' '.                       ECS023
00133                                                                   ECS023
00134      12  HEAD-2.                                                  ECS023
00135          16  FILLER  PIC X(47)   VALUE SPACES.                    ECS023
00136          16  H2-CMP  PIC X(30).                                   ECS023
00137          16  FILLER  PIC X(42)   VALUE SPACES.                    ECS023
00138          16  H2-IPL  PIC X(8).                                    ECS023
00139                                                                   ECS023
00140      12  HEAD-3.                                                  ECS023
00141          16  FILLER  PIC X(53)   VALUE SPACES.                    ECS023
00142          16  H3-DATE PIC X(18).                                   ECS023
00143          16  FILLER  PIC X(45)   VALUE SPACES.                    ECS023
00144          16  FILLER  PIC X(5)    VALUE 'PAGE '.                   ECS023
00145          16  H3-PAGE PIC ZZ,ZZ9.                                  ECS023
00146                                                                   ECS023
00147      12  HEAD-3A.                                                 ECS023
020405         16  H3A-DES             PIC X(30) VALUE SPACES.
020405         16  H3A-DES-RC1         PIC X(15) VALUE SPACES.
020405         16  H3A-RPT-CODE1       PIC X(10) VALUE SPACES.
020405         16  FILLER              PIC X(50) VALUE SPACES.
00149 *        16  FILLER              PIC X(75) VALUE SPACES.
00150                                                                   ECS023
00151      12  HEAD-3B.                                                 ECS023
00152          16  H3B-DES PIC X(58)   VALUE SPACES.                    ECS023
00153          16  FILLER  PIC X(75)   VALUE SPACES.                    ECS023
00154                                                                   ECS023
00155      12  HEAD-4.                                                  ECS023
00156          16  H4-DES  PIC X(58) VALUE SPACES.                      ECS023
00157          16  FILLER  PIC X(75) VALUE SPACES.                      ECS023
00158                                                                   ECS023
00159      12  HEAD-5.                                                  ECS023
00160          16  FILLER  PIC X(19) VALUE 'T E R M      AGE   '.       ECS023
00161          16  FILLER  PIC X(19) VALUE '         AGE       '.       ECS023
00162          16  FILLER  PIC X(19) VALUE '     AGE           '.       ECS023
00163          16  FILLER  PIC X(19) VALUE ' AGE            AGE'.       ECS023
00164          16  FILLER  PIC X(19) VALUE '            AGE    '.       ECS023
00165          16  FILLER  PIC X(19) VALUE '        AGE        '.       ECS023
00166          16  FILLER  PIC X(18) VALUE '   TOTAL      PCT '.        ECS023
00167                                                                   ECS023
00168      12  HEAD-6.                                                  ECS023
00169          16  FILLER  PIC X(19) VALUE '          UNDER  36'.       ECS023
00170          16  FILLER  PIC X(19) VALUE '       36 - 40     '.       ECS023
00171          16  FILLER  PIC X(19) VALUE '   41 - 45        4'.       ECS023
00172          16  FILLER  PIC X(19) VALUE '6 - 50        51 - '.       ECS023
00173          16  FILLER  PIC X(19) VALUE '55        56 - 64  '.       ECS023
00174          16  FILLER  PIC X(19) VALUE '      OVER 64      '.       ECS023
00175          16  FILLER  PIC X(18) VALUE ' ALL  AGES        '.        ECS023
00176                                                                   ECS023
00177      12  MIL-HEAD-6.                                              ECS023
00178          16  FILLER  PIC X(19) VALUE '          UNDER  41'.       ECS023
00179          16  FILLER  PIC X(19) VALUE '       41 - 45     '.       ECS023
00180          16  FILLER  PIC X(19) VALUE '   46 - 50        5'.       ECS023
00181          16  FILLER  PIC X(19) VALUE '1 - 55        56 - '.       ECS023
00182          16  FILLER  PIC X(19) VALUE '59        60 - 65  '.       ECS023
00183          16  FILLER  PIC X(19) VALUE '      OVER 65      '.       ECS023
00184          16  FILLER  PIC X(18) VALUE ' ALL  AGES        '.        ECS023
00185                                                                   ECS023
00186      12  HEAD-7.                                                  ECS023
00187          16  H7-DES  PIC X(41) VALUE SPACES.                      ECS023
00188          16  FILLER  PIC X(91) VALUE SPACES.                      ECS023
00189                                                                   ECS023
00190      12  ACCT-TOT.                                                ECS023
00191          16  FILLER  PIC X(18) VALUE '* * * ACCOUNT NO. '.        ECS023
00192          16  AT-NO   PIC X(10).                                   ECS023
00193          16  FILLER  PIC X(10) VALUE ' TOTALS'.                   ECS023
00194          16  AT-NA   PIC X(30) VALUE SPACES.                      ECS023
00195                                                                   ECS023
00196      12  STATE-TOT.                                               ECS023
00197          16  FILLER  PIC X(16) VALUE '* * * STATE NO. '.          ECS023
00198          16  ST-NO   PIC XX.                                      ECS023
00199          16  FILLER  PIC X(8)  VALUE ' TOTALS '.                  ECS023
00200          16  ST-DES  PIC X(19).                                   ECS023
00201                                                                   ECS023
00202      12  COMP-TOT.                                                ECS023
00203          16  FILLER  PIC X(18) VALUE '* *CARRIER/GROUP  '.        ECS023
00204          16  CP-NO   PIC X(7).                                    ECS023
00205          16  FILLER  PIC X(19) VALUE ' TOTALS'.                   ECS023
00206                                                                   ECS023
00207      12  AGENT-TOT.                                               ECS023
00208          16  FILLER  PIC X(16) VALUE '* * * AGENT NO. '.          ECS023
00209          16  AG-NO   PIC X(10).                                   ECS023
00210          16  FILLER  PIC X(19) VALUE ' TOTALS'.                   ECS023
00211                                                                   ECS023
00212      12  GA-TOT.                                                  ECS023
00213          16  FILLER  PIC X(15) VALUE '* * * G.A. NO. '.           ECS023
00214          16  GA-NO   PIC X(10).                                   ECS023
00215          16  FILLER  PIC X(20) VALUE ' TOTALS'.                   ECS023
00216                                                                   ECS023
00217      12  RPT1-TOT.                                                ECS023
00218          16  FILLER   PIC X(6)  VALUE '* * * '.                   ECS023
00219          16  RPT1-CAP PIC X(12) VALUE SPACES.                     ECS023
00220          16  RPT1-NO  PIC X(10).                                  ECS023
00221          16  FILLER   PIC X(18) VALUE ' TOTALS'.                  ECS023
00222                                                                   ECS023
00223      12  RPT2-TOT.                                                ECS023
00224          16  FILLER   PIC X(6)  VALUE '* * * '.                   ECS023
00225          16  RPT2-CAP PIC X(12) VALUE SPACES.                     ECS023
00226          16  RPT2-NO  PIC X(10).                                  ECS023
00227          16  FILLER   PIC X(18) VALUE ' TOTALS'.                  ECS023
00228                                                                   ECS023
00229      12  CLASS-TOT.                                               ECS023
00230          16  FILLER  PIC X(16) VALUE '* * * BUS. TYPE '.          ECS023
00231          16  CL-NO   PIC XX.                                      ECS023
00232          16  FILLER  PIC X(8)   VALUE ' TOTALS '.                 ECS023
00233          16  CL-DES  PIC X(24).                                   ECS023
00234                                                                   ECS023
00235      12  B1-HD.                                                   ECS023
00236          16  FILLER  PIC X(16)  VALUE '* CARRIER/GROUP '.         ECS023
00237          16  B1-C    PIC X(7).                                    ECS023
00238          16  FILLER  PIC X(7)   VALUE ' STATE '.                  ECS023
00239          16  B1-S    PIC XX.                                      ECS023
00240          16  FILLER  PIC X(22)  VALUE ' TOTALS FOR BUS. TYPE '.   ECS023
00241          16  B1-T    PIC XX.                                      ECS023
00242          16  FILLER  PIC XX     VALUE ' '.                        ECS023
00243          16  B1-D    PIC X(24).                                   ECS023
00244                                                                   ECS023
00245      12  B2-HD.                                                   ECS023
00246          16  FILLER  PIC X(14)  VALUE '* * STATE NO. '.           ECS023
00247          16  B2-S    PIC XX.                                      ECS023
00248          16  FILLER  PIC X(22)  VALUE ' TOTALS FOR BUS. TYPE '.   ECS023
00249          16  B2-T    PIC XX.                                      ECS023
00250          16  FILLER  PIC XX     VALUE ' '.                        ECS023
00251          16  B2-D    PIC X(24).                                   ECS023
00252                                                                   ECS023
00253      12  S-HD.                                                    ECS023
00254          16  FILLER  PIC X(16)  VALUE '* CARRIER GROUP '.         ECS023
00255          16  S-C     PIC X(7).                                    ECS023
00256          16  FILLER  PIC X(18)  VALUE ' TOTALS FOR STATE '.       ECS023
00257          16  S-S     PIC XX.                                      ECS023
00258          16  FILLER  PIC XX     VALUE '  '.                       ECS023
00259          16  S-D     PIC X(20).                                   ECS023
00260                                                                   ECS023
00261      12  AA-HD1.                                                  ECS023
00262          16  FILLER  PIC X(16)  VALUE '* CARRIER/GROUP '.         ECS023
00263          16  AA1-C   PIC X(7).                                    ECS023
00264          16  FILLER  PIC X(7)   VALUE ' STATE '.                  ECS023
00265          16  AA1-S   PIC XX.                                      ECS023
00266          16  FILLER  PIC X(19)  VALUE ' TOTALS FOR AGENT '.       ECS023
00267          16  AA1-A   PIC X(10).                                   ECS023
00268                                                                   ECS023
00269      12  AR-HD1.                                                  ECS023
00270          16  FILLER  PIC X(16)  VALUE '* CARRIER/GROUP '.         ECS023
00271          16  AR1-C   PIC X(7).                                    ECS023
00272                                                                   ECS023
00273      12  AA-HD2.                                                  ECS023
00274          16  FILLER  PIC X(14)  VALUE '* * STATE NO. '.           ECS023
00275          16  AA2-S   PIC XX.                                      ECS023
00276          16  FILLER  PIC X(19)  VALUE ' TOTALS FOR AGENT '.       ECS023
00277          16  AA2-A   PIC X(6).                                    ECS023
00278                                                                   ECS023
00279      12  AR-HD2.                                                  ECS023
00280          16  FILLER  PIC X(14)  VALUE '* * STATE NO. '.           ECS023
00281          16  AR2-S   PIC XX.                                      ECS023
00282          16  FILLER  PIC X(19)  VALUE ' TOTALS FOR RPTCD2'.       ECS023
00283          16  AR2-A   PIC X(6).                                    ECS023
00284                                                                   ECS023
00285      12  AG-HD1.                                                  ECS023
00286          16  FILLER  PIC X(16)  VALUE '* CARRIER/GROUP '.         ECS023
00287          16  AG1-C   PIC X(7).                                    ECS023
00288          16  FILLER  PIC X(7)   VALUE ' STATE '.                  ECS023
00289          16  AG1-S   PIC XX.                                      ECS023
00290          16  FILLER  PIC X(23)  VALUE ' TOTALS FOR GEN. AGENT '.  ECS023
00291          16  AG1-G   PIC X(10).                                   ECS023
00292                                                                   ECS023
00293      12  AG-HD2.                                                  ECS023
00294          16  FILLER  PIC X(14)  VALUE '* * STATE NO. '.           ECS023
00295          16  AG2-S   PIC XX.                                      ECS023
00296          16  FILLER  PIC X(23)  VALUE ' TOTALS FOR GEN. AGENT '.  ECS023
00297          16  AG2-G   PIC X(10).                                   ECS023
00298                                                                   ECS023
00299      12  A1-HD.                                                   ECS023
00300          16  FILLER  PIC X(16)  VALUE '* CARRIER/GROUP '.         ECS023
00301          16  A1-C    PIC X(7).                                    ECS023
00302          16  FILLER  PIC X(7)   VALUE ' STATE '.                  ECS023
00303          16  A1-S    PIC XX.                                      ECS023
00304          16  FILLER  PIC X(20)  VALUE ' TOTALS FOR ACCOUNT '.     ECS023
00305          16  A1-A    PIC X(10).                                   ECS023
00306          16  FILLER  PIC XX     VALUE '  '.                       ECS023
00307          16  A1-D    PIC X(20).                                   ECS023
00308                                                                   ECS023
00309      12  A2-HD.                                                   ECS023
00310          16  FILLER  PIC X(14)  VALUE '* * STATE NO. '.           ECS023
00311          16  A2-S    PIC XX.                                      ECS023
00312          16  FILLER  PIC X(20)  VALUE ' TOTALS FOR ACCOUNT '.     ECS023
00313          16  A2-A    PIC X(10).                                   ECS023
00314          16  FILLER  PIC XX     VALUE '  '.                       ECS023
00315          16  A2-D    PIC X(30).                                   ECS023
00316                                                                   ECS023
00317      12  PA-HEAD.                                                 ECS023
00318          16  FILLER  PIC X(41) VALUE '      PREMIUM ANALYSIS'.    ECS023
00319                                                                   ECS023
00320      12  AE-HEAD.                                                 ECS023
00321          16  FILLER  PIC X(41) VALUE                              ECS023
00322                      '      AVERAGE EXPOSURE ANALYSIS'.           ECS023
00323      12  REPT-HEAD.                                               ECS023
00324          16  REPT-NAME   PIC X(10) VALUE SPACES.                  ECS023
00325          16  FILLER      PIC X(8)  VALUE ' SUMMARY'.              ECS023
00326  EJECT                                                            ECS023
00327  01  DTL-LINE.                                                    ECS023
00328      12  D-DATA.                                                  ECS023
00329          16  D-DES2       PIC X(6).                               ECS023
00330          16  D-AMT        PIC ZZZ,ZZZ,ZZZ.99-                     ECS023
00331                               BLANK WHEN ZERO OCCURS 8.           ECS023
00332      12  D-DATA1 REDEFINES D-DATA.                                ECS023
00333          16  FILLER       PIC X(6).                               ECS023
00334          16  D-PCTS  OCCURS 8.                                    ECS023
00335              20  FILLER   PIC X(10).                              ECS023
00336              20  D-PCT    PIC ZZZ-.                               ECS023
00337              20  D-PCT1   PIC X.                                  ECS023
00338      12  D-DATA2 REDEFINES D-DATA1.                               ECS023
00339          16  D-DES        PIC X(7).                               ECS023
00340          16  FILLER       PIC X(119).                             ECS023
00341      12  FILLER           PIC X   VALUE SPACE.                    ECS023
00342      12  D-EPCT           PIC ZZZ-.                               ECS023
00343      12  D-EPCT1          PIC X   VALUE SPACE.                    ECS023
00344                                                                   ECS023
00345  01  COUNT-LINE  SYNC.                                            ECS023
00346      12  FILLER      PIC X(24) VALUE ' NUMBER OF POLICIES'.       ECS023
00347      12  D-CNT       PIC Z,ZZZ,ZZ9.                               ECS023
00348      12  FILLER      PIC X(99) VALUE SPACES.                      ECS023
00349                                                                   ECS023
00350  01  AGE-LINE    SYNC.                                            ECS023
00351      12  FILLER      PIC X(22) VALUE ' AVERAGE AGE'.              ECS023
00352      12  D-AGE       PIC ZZZZ,ZZZ.99.                             ECS023
00353      12  FILLER      PIC X(99) VALUE SPACES.                      ECS023
00354                                                                   ECS023
00355  01  WEIGHTED-LINE.                                               ECS023
00356      12  FILLER      PIC X(24) VALUE ' AVERAGE WEIGHTED AGE'.     ECS023
00357      12  D-WEIGHTED  PIC ZZ,ZZZ.99.                               ECS023
00358      12  FILLER      PIC X(99) VALUE SPACES.                      ECS023
00359                                                                   ECS023
00360  01  TERM-LINE   SYNC.                                            ECS023
00361      12  FILLER      PIC X(22) VALUE ' AVERAGE TERM'.             ECS023
00362      12  D-TERM      PIC ZZZZ,ZZZ.99.                             ECS023
00363      12  FILLER      PIC X(99) VALUE SPACES.                      ECS023
00364                                                                   ECS023
00365  01  WEIGHT-CNTRSX.                                               ECS023
00366      12  WEIGHT-CNTRS    OCCURS 7 TIMES    COMP-3.                ECS023
00367          16  WEIGHT-CNT    PIC S9(7).                             ECS023
00368          16  WEIGHT-FACE   PIC S9(12)V99.                         ECS023
00369          16  WEIGHT-WORK   PIC S9(12)V99.                         ECS023
00370  EJECT                                                            ECS023
00371  01  MISC-WK.                                                     ECS023
00372      12  AM-FILE-STATUS  PIC XX    VALUE '00'.                    ECS023
00373      12  HEAD-SPACE      PIC XXX   VALUE SPACES.                  ECS023
00374      12  AM-CO           PIC X(7)  VALUE SPACES.                  ECS023
00375      12  L-NAME          PIC X(30) VALUE SPACES.                  ECS023
00376      12  X               PIC X     VALUE SPACE.                   ECS023
00377      12  PAGE-CT         PIC S9(5) VALUE +0  COMP-3.              ECS023
00378      12  X1              PIC S9(5) COMP.                          ECS023
00379      12  X2              PIC S9(5) COMP.                          ECS023
00380      12  X3              PIC S9(5) COMP.                          ECS023
00381      12  X4              PIC S9(5) COMP.                          ECS023
00382      12  X5              PIC S9(5) COMP.                          ECS023
00383      12  S-CNT           PIC S9(5)  VALUE +0  COMP-3.             ECS023
00384      12  C-CNT           PIC S9(5)  VALUE +0  COMP-3.             ECS023
00385      12  DUMMY-COMP-BK-SW PIC X     VALUE '0'.                    ECS023
00386      12  LAST-CTL.                                                ECS023
00387          16  L-CODE      PIC X.                                   ECS023
00388          16  L-CNTL      PIC X(10).                               ECS023
00389          16  L-CO.                                                ECS023
00390              20  L-CARR  PIC X.                                   ECS023
00391              20  L-GRP   PIC X(6).                                ECS023
00392          16  L-RPT2      PIC X(10).                               ECS023
00393          16  L-ST        PIC XX.                                  ECS023
00394          16  L-ACCT      PIC X(10).                               ECS023
00395      12  H4-CTL.                                                  ECS023
00396          16  FILLER      PIC XX.                                  ECS023
00397          16  H4-CMP.                                              ECS023
00398              18  H4-CARR PIC X.                                   ECS023
00399              18  H4-GRP  PIC X(6).                                ECS023
00400          16  FILLER      PIC X(4).                                ECS023
00401          16  H4-STE      PIC XX.                                  ECS023
00402      12  HOLD-PCT        PIC S9(3) OCCURS 13  COMP-3.             ECS023
00403      12  TOT-PREM        PIC S9(9)V99  COMP-3.                    ECS023
00404      12  PRM1            PIC S9(9)V99  COMP-3.                    ECS023
00405      12  TYP-HLD         PIC XX.                                  ECS023
00406      12  TYP-DES         PIC X(24).                               ECS023
00407      12  ST-SUB1.                                                 ECS023
00408          16  ST-SB       PIC 99.                                  ECS023
00409      12  WS-RETURN-CODE         PIC X(4)   VALUE SPACES.          ECS023
00410      12  WS-ABEND-OPTION        PIC X      VALUE 'Y'.             ECS023
00411      12  WS-ABEND-FILE-STATUS   PIC XX     VALUE SPACES.          ECS023
00412      12  WS-ABEND-OPTION        PIC X      VALUE 'Y'.             ECS023
00413      12  WS-ZERO                PIC S9     VALUE ZERO.            ECS023
00414      12  WS-ABEND-MESSAGE       PIC X(80)  VALUE SPACES.          ECS023
00415      12  PGM-SUB                PIC S999 COMP VALUE +023.         ECS023
00416  EJECT                                                            ECS023
00417  01  ACCUMS.                                                      ECS023
00418      12  ACC1    OCCURS  280 TIMES   COMP-3.                      ECS023
00419          16  L-PREM      PIC S9(9)V99.                            ECS023
00420          16  A-PREM      PIC S9(9)V99.                            ECS023
PEMMOD         16  L-BEN       PIC S9(12)V99.                           ECS023
00422          16  A-BEN       PIC S9(9)V99.                            ECS023
00423          16  L-CT        PIC S9(7).                               ECS023
00424          16  A-CT        PIC S9(7).                               ECS023
00425      12  ACC2    OCCURS   7  TIMES   COMP-3.                      ECS023
00426          16  KOUNT       PIC S9(7).                               ECS023
CIDMOD         16  AGE         PIC S9(11).                              ECS023
CIDMOD         16  TERM        PIC S9(11).                              ECS023
00429  EJECT                                                            ECS023
00430  01  SUM-REC.                                                     ECS023
00431      12  SUM-CTL.                                                 ECS023
00432          16  S-CODE      PIC X.                                   ECS023
00433          16  S-CNTL      PIC X(10).                               ECS023
00434          16  S-CO        PIC X(7).                                ECS023
00435          16  S-RPT-CD2   PIC X(10).                               ECS023
00436          16  S-ST        PIC XX.                                  ECS023
00437          16  S-ACCT      PIC X(10).                               ECS023
00438      12  S-RPT-CD1       PIC X(10).                               ECS023
020405     12  S-AM-NAME       PIC X(30).
00439      12  SUM-DATA    OCCURS  40 TIMES  COMP-3.                    ECS023
00440          16  SL-P        PIC S9(9)V99.                            ECS023
00441          16  SA-P        PIC S9(9)V99.                            ECS023
PEMMOD         16  SL-B        PIC S9(12)V99.                           ECS023
00443          16  SA-B        PIC S9(9)V99.                            ECS023
00444          16  SL-C        PIC S9(7).                               ECS023
00445          16  SA-C        PIC S9(7).                               ECS023
00446      12  S-CT            PIC S9(7)     COMP-3.                    ECS023
CIDMOD     12  S-AGE           PIC S9(11)    COMP-3.                    ECS023
CIDMOD     12  S-TERM          PIC S9(11)    COMP-3.                    ECS023
00449      12  S-COUNT         PIC S9(7)     COMP-3.                    ECS023
00450      12  S-FACE          PIC S9(12)V99 COMP-3.                    ECS023
00451      12  S-WORK          PIC S9(12)V99 COMP-3.                    ECS023
00452                                                                   ECS023
00453      COPY ELCDTECX.                                               ECS023
00454                                                                   ECS023
00455      COPY ELCDTEVR.                                               ECS023
00456  EJECT                                                            ECS023
00457  PROCEDURE DIVISION.                                              ECS023
00458  0000-DATE-READ.                                                  ECS023
00459      COPY ELCDTERX.                                               ECS023
00460                                                                   ECS023
00461  0100-START-1.                                                    ECS023
00462      OPEN INPUT   SUMMARY  ACCOUNT                                ECS023
00463           OUTPUT  PRINTER.                                        ECS023
00464                                                                   ECS023
00465  0120-FIRST-SET.                                                  ECS023
00466      MOVE WS-CURRENT-DATE        TO H2-IPL.                       ECS023
00467      MOVE ALPH-DATE              TO H3-DATE.                      ECS023
00468      MOVE COMPANY-NAME           TO H2-CMP.                       ECS023
00469      MOVE +0                     TO X1.                           ECS023
00470                                                                   ECS023
00471  0130-CLEAR-40.                                                   ECS023
00472      MOVE +0                     TO X3.                           ECS023
00473                                                                   ECS023
00474  0140-CLEAR-LOOP.                                                 ECS023
00475      ADD +1 TO X3.                                                ECS023
00476      IF X3 GREATER THAN +40                                       ECS023
00477          GO TO 0150-CLEAR-X.                                      ECS023
00478                                                                   ECS023
00479      ADD +1 TO X1.                                                ECS023
00480      MOVE +0                     TO L-PREM (X1)  A-PREM (X1)      ECS023
00481                                     L-BEN (X1)   A-BEN (X1)       ECS023
00482                                     L-CT (X1)    A-CT (X1).       ECS023
00483      GO TO 0140-CLEAR-LOOP.                                       ECS023
00484                                                                   ECS023
00485  0150-CLEAR-X.                                                    ECS023
00486      EXIT.                                                        ECS023
00487                                                                   ECS023
00488  0160-SET-UP.                                                     ECS023
00489      PERFORM 0130-CLEAR-40 THRU 0150-CLEAR-X 6 TIMES.             ECS023
00490      MOVE +0 TO KOUNT (1)  AGE (1)  TERM (1).                     ECS023
00491      MOVE ACC2 (1)               TO ACC2 (2)  ACC2 (3)            ECS023
00492                                     ACC2 (4)  ACC2 (5).           ECS023
00493      MOVE ZERO                   TO WEIGHT-CNT (1)                ECS023
00494                                     WEIGHT-FACE (1)               ECS023
00495                                     WEIGHT-WORK (1).              ECS023
00496      MOVE WEIGHT-CNTRS (1)       TO WEIGHT-CNTRS (2)              ECS023
00497                                     WEIGHT-CNTRS (3)              ECS023
00498                                     WEIGHT-CNTRS (4)              ECS023
00499                                     WEIGHT-CNTRS (5).             ECS023
00500  EJECT                                                            ECS023
00501  0180-SORT-PROCEDURE.                                             ECS023
00502      SORT SORT-FILE ON ASCENDING SORT-KEY1                        ECS023
00503                                  SORT-KEY3                        ECS023
00504                                  SORT-KEY2                        ECS023
CIDMOD                     DESCENDING  SORT-KEY4
00505          INPUT PROCEDURE  0190-GET-ACCT  THRU 0200-ACCT-END       ECS023
00506          OUTPUT PROCEDURE 0220-READ-ACCT THRU 0480-CLOSE-EXIT.    ECS023
00507                                                                   ECS023
00508      IF SORT-RETURN NOT = ZEROS                                   ECS023
00509          MOVE '0101'              TO WS-RETURN-CODE               ECS023
00510          GO TO ABEND-PGM.                                         ECS023
00511                                                                   ECS023
00512      GOBACK.                                                      ECS023
00513                                                                   ECS023
00514  0190-GET-ACCT SECTION.                                           ECS023
00515      READ ACCOUNT AT END                                          ECS023
00516          CLOSE ACCOUNT                                            ECS023
00517          GO TO 0200-ACCT-END.                                     ECS023
00518                                                                   ECS023
PEMMOD     MOVE AM-KEY (2:1)           TO AM-KEY (11:1)
00519      RELEASE SORT-REC FROM AM-MSTR.                               ECS023
00520      GO TO 0190-GET-ACCT.                                         ECS023
00521                                                                   ECS023
00522  0200-ACCT-END.                                                   ECS023
00523      EXIT.                                                        ECS023
00524                                                                   ECS023
00525  EJECT                                                            ECS023
00526                                                                   ECS023
00527  0220-READ-ACCT SECTION.                                          ECS023
00528      RETURN SORT-FILE INTO ACCOUNT-MASTER AT END                  ECS023
00529          MOVE HIGH-VALUES TO ACCOUNT-MASTER.                      ECS023
00530                                                                   ECS023
00531  0230-EXIT.                                                       ECS023
00532      EXIT.                                                        ECS023
00533                                                                   ECS023
00534  0240-READ-SUMMARY.                                               ECS023
00535      READ SUMMARY INTO SUM-REC AT END                             ECS023
00536          GO TO 0450-OVER-BK.                                      ECS023
00537                                                                   ECS023
00538      ADD +1 TO SUMMARY-COUNT.                                     ECS023
00539                                                                   ECS023
00540 ****************************************                          ECS023
00541 ***** IF REPORT CODE 2 PGM-OPT (3) IS USED,                       ECS023
00542 *****     SKIP ANY REPORT CODE TOTALS                             ECS023
00543      IF DTE-PGM-OPT = '3'                                         ECS023
00544          IF S-CODE = '7' OR '8'                                   ECS023
00545              GO TO 0240-READ-SUMMARY.                             ECS023
00546 ****************************************                          ECS023
00547                                                                   ECS023
00548 ***** REPORT CODE 1                                               ECS023
00549      IF S-CODE = '7'                                              ECS023
00550          IF DTE-TOT-OPT NOT = '2' AND '4'                         ECS023
00551              GO TO 0240-READ-SUMMARY.                             ECS023
00552                                                                   ECS023
00553 ***** REPORT CODE 2                                               ECS023
00554      IF S-CODE = '8'                                              ECS023
00555          IF DTE-TOT-OPT NOT = '3' AND '4'                         ECS023
00556              GO TO 0240-READ-SUMMARY                              ECS023
00557          ELSE                                                     ECS023
00558              MOVE S-RPT-CD2      TO S-CNTL                        ECS023
00559              MOVE SPACES         TO S-RPT-CD2.                    ECS023
00560                                                                   ECS023
00561 ***** SUMMARY BY ACCOUNT                                          ECS023
00562      IF S-CODE  = '1' AND                                         ECS023
00563         DTE-PGM-OPT NOT = '1' AND '8'                             ECS023
00564          GO TO 0240-READ-SUMMARY.                                 ECS023
00565                                                                   ECS023
00566 ***** SUMMARY BY LEVEL 1 AGENT                                    ECS023
00567      IF S-CODE  = '2' AND                                         ECS023
00568         DTE-PGM-OPT NOT = '2' AND '8'                             ECS023
00569          GO TO 0240-READ-SUMMARY.                                 ECS023
00570                                                                   ECS023
00571 ***** SUMMARY BY COMPANY,REPORT CODE 2                            ECS023
00572      IF S-CODE  = '3' AND                                         ECS023
00573         DTE-PGM-OPT NOT = '3' AND '8'                             ECS023
00574          GO TO 0240-READ-SUMMARY.                                 ECS023
00575                                                                   ECS023
00576 ***** SUMMARY BY LEVEL 2 & 3 AGENT                                ECS023
00577      IF S-CODE  = '4' AND                                         ECS023
00578         DTE-PGM-OPT NOT = '4' AND '8'                             ECS023
00579          GO TO 0240-READ-SUMMARY.                                 ECS023
00580                                                                   ECS023
00581 ***** SUMMARY BY BUSINESS TYPE                                    ECS023
00582      IF S-CODE  = '5' AND                                         ECS023
00583         DTE-PGM-OPT NOT = '5' AND '8'                             ECS023
00584          GO TO 0240-READ-SUMMARY.                                 ECS023
00585                                                                   ECS023
00586 ***** SUMMARY BY STATE                                            ECS023
00587      IF S-CODE  = '6' AND                                         ECS023
00588         DTE-PGM-OPT NOT = '6' AND '8'                             ECS023
00589          GO TO 0240-READ-SUMMARY.                                 ECS023
00590                                                                   ECS023
00591 ***** SUMMARY BY COMPANY                                          ECS023
00592      IF S-CODE  = '9' AND                                         ECS023
00593         DTE-PGM-OPT NOT = '7' AND '8'                             ECS023
00594          GO TO 0240-READ-SUMMARY.                                 ECS023
00595                                                                   ECS023
00596      IF LCP-ONCTR-01 =  0                                         ECS023
00597          ADD 1 TO LCP-ONCTR-01                                    ECS023
00598          MOVE SUM-CTL            TO LAST-CTL
020405         MOVE S-AM-NAME          TO L-NAME
           END-IF
00599                                                                   ECS023
00600      MOVE SPACES                 TO H4-CTL  H4-DES  H3B-DES.      ECS023
00601                                                                   ECS023
00602  0250-R-S-X.                                                      ECS023
00603      GO TO 0300-ACCUMULATE.                                       ECS023
00604  EJECT                                                            ECS023
00605  0260-FIND-ACCT.                                                  ECS023
00606      IF L-ACCT  = AM-ACCOUNT   AND                                ECS023
00607         L-CARR  = AM-CARRIER   AND                                ECS023
00608         L-GRP   = AM-GROUPING  AND                                ECS023
00609         L-ST    = AM-STATE                                        ECS023
020405*        MOVE AM-NAME            TO L-NAME                        ECS023
00611          GO TO 0270-CK-ST.                                        ECS023
00612                                                                   ECS023
00613      PERFORM 0220-READ-ACCT THRU 0230-EXIT.                       ECS023
00614      GO TO 0260-FIND-ACCT.                                        ECS023
00615                                                                   ECS023
00616  0270-CK-ST.                                                      ECS023
00617      IF L-ST = AM-STATE                                           ECS023
00618          GO TO 0280-CK-COMP.                                      ECS023
00619                                                                   ECS023
00620      IF L-ST LESS THAN AM-STATE                                   ECS023
00621          DISPLAY 'INVALID STATE  ' L-ST                           ECS023
00622          MOVE '0301'             TO WS-RETURN-CODE                ECS023
00623          GO TO ABEND-PGM.                                         ECS023
00624                                                                   ECS023
00625      PERFORM 0220-READ-ACCT THRU 0230-EXIT.                       ECS023
00626      GO TO 0260-FIND-ACCT.                                        ECS023
00627                                                                   ECS023
00628  0280-CK-COMP.                                                    ECS023
00629       STRING AM-CARRIER         DELIMITED BY SIZE                 ECS023
00630              AM-GROUPING        DELIMITED BY SIZE                 ECS023
00631       INTO AM-CO.                                                 ECS023
00632                                                                   ECS023
00633      IF L-CO = AM-CO                                              ECS023
00634          GO TO 0290-EXIT.                                         ECS023
00635                                                                   ECS023
00636      IF L-CO LESS THAN AM-CO                                      ECS023
00637          DISPLAY ' INVALID COMPANY  ' L-CO                        ECS023
00638          MOVE '0301'             TO WS-RETURN-CODE                ECS023
00639          GO TO ABEND-PGM.                                         ECS023
00640                                                                   ECS023
00641      PERFORM 0220-READ-ACCT THRU 0230-EXIT.                       ECS023
00642      GO TO 0260-FIND-ACCT.                                        ECS023
00643                                                                   ECS023
00644  0290-EXIT.                                                       ECS023
00645      EXIT.                                                        ECS023
00646  EJECT                                                            ECS023
00647  0300-ACCUMULATE.                                                 ECS023
00648      MOVE +0                     TO X1.                           ECS023
00649      MOVE +40                    TO X2.                           ECS023
00650                                                                   ECS023
00651  0310-A-LOOP.                                                     ECS023
00652      ADD +1 TO X1.                                                ECS023
00653      IF X1 GREATER THAN +40                                       ECS023
00654          GO TO 0320-A-CAT.                                        ECS023
00655                                                                   ECS023
00656      ADD +1 TO X2.                                                ECS023
00657      ADD SL-P (X1) TO L-PREM (X2).                                ECS023
00658      ADD SA-P (X1) TO A-PREM (X2).                                ECS023
00659      ADD SL-B (X1) TO L-BEN (X2).                                 ECS023
00660      ADD SA-B (X1) TO A-BEN (X2).                                 ECS023
00661      ADD SL-C (X1) TO L-CT (X2).                                  ECS023
00662      ADD SA-C (X1) TO A-CT (X2).                                  ECS023
00663                                                                   ECS023
00664      GO TO 0310-A-LOOP.                                           ECS023
00665                                                                   ECS023
00666  0320-A-CAT.                                                      ECS023
00667      ADD S-CT    TO KOUNT (2).                                    ECS023
00668      ADD S-AGE   TO AGE (2).                                      ECS023
00669      ADD S-TERM  TO TERM (2).                                     ECS023
00670      ADD S-COUNT TO WEIGHT-CNT (2).                               ECS023
00671      ADD S-FACE  TO WEIGHT-FACE (2).                              ECS023
00672      ADD S-WORK  TO WEIGHT-WORK (2).                              ECS023
00673                                                                   ECS023
00674  0330-A-X.                                                        ECS023
00675      PERFORM 0240-READ-SUMMARY.                                   ECS023
00676                                                                   ECS023
00677      IF S-CODE NOT = L-CODE                                       ECS023
00678          GO TO 0440-CODE-BK.                                      ECS023
00679                                                                   ECS023

      ***  TRUE ACCOUNT BREAK
020405     IF L-CODE = '1'
020405        IF S-ACCT NOT = L-ACCT
020405           GO TO 0400-ACCT-BK
020405        END-IF
020405     END-IF

020405     IF L-CODE NOT = '1'
00680         IF S-CNTL NOT = L-CNTL
00681            GO TO 0400-ACCT-BK
              END-IF
           END-IF
00682                                                                   ECS023
00683      IF L-CODE = '7' OR '8'                                       ECS023
00684          GO TO 0300-ACCUMULATE.                                   ECS023
00685                                                                   ECS023
00686      IF S-ST NOT = L-ST                                           ECS023
00687          GO TO 0370-STATE-BK.                                     ECS023
00688                                                                   ECS023
00689      IF S-CO NOT = L-CO                                           ECS023
00690          GO TO 0340-COMPANY-BK.                                   ECS023
00691                                                                   ECS023
00692      GO TO 0300-ACCUMULATE.                                       ECS023
00693                                                                   ECS023
00694  EJECT                                                            ECS023
00695  0340-COMPANY-BK.                                                 ECS023
00696      ADD +1 TO C-CNT.                                             ECS023
00697      MOVE SPACES                 TO HEAD-7.                       ECS023
00698                                                                   ECS023
00699      IF L-CODE = '1'                                              ECS023
00700          MOVE L-CO               TO A1-C                          ECS023
00701          MOVE L-ST               TO A1-S                          ECS023
00702          MOVE L-CNTL             TO A1-A                          ECS023
020405*        PERFORM 0260-FIND-ACCT THRU 0290-EXIT                    ECS023
00704          MOVE L-NAME             TO A1-D                          ECS023
00705          MOVE A1-HD              TO HEAD-7.                       ECS023
00706                                                                   ECS023
00707      IF L-CODE = '2'                                              ECS023
00708          MOVE L-CO               TO AA1-C                         ECS023
00709          MOVE L-ST               TO AA1-S                         ECS023
00710          MOVE L-CNTL             TO AA1-A                         ECS023
00711          MOVE AA-HD1             TO HEAD-7.                       ECS023
00712                                                                   ECS023
00713      IF L-CODE = '3'                                              ECS023
00714          MOVE L-CO               TO AR1-C                         ECS023
00715          MOVE AR-HD1             TO HEAD-7.                       ECS023
00716                                                                   ECS023
00717      IF L-CODE = '4'                                              ECS023
00718          MOVE L-CO               TO AG1-C                         ECS023
00719          MOVE L-ST               TO AG1-S                         ECS023
00720          MOVE L-CNTL             TO AG1-G                         ECS023
00721          MOVE AG-HD1             TO HEAD-7.                       ECS023
00722                                                                   ECS023
00723      IF L-CODE = '5'                                              ECS023
00724          MOVE L-CO               TO B1-C                          ECS023
00725          MOVE L-ST               TO B1-S                          ECS023
00726          MOVE L-CNTL             TO B1-T  TYP-HLD                 ECS023
00727          PERFORM 0520-TYPE-FIND THRU 0550-T-F-X                   ECS023
00728          MOVE TYP-DES            TO B1-D                          ECS023
00729          MOVE B1-HD              TO HEAD-7.                       ECS023
00730                                                                   ECS023
00731      IF L-CODE = '6'                                              ECS023
00732          MOVE L-CO               TO S-C                           ECS023
00733          MOVE L-ST               TO S-S  ST-SUB1                  ECS023
00734          MOVE CLAS-STARTS        TO CLAS-INDEXS                   ECS023
00735          PERFORM 0500-STATE-LOOKUP THRU 0510-STATE-LOOK-X         ECS023
00736          MOVE STATE-PIC (CLAS-INDEXS) TO S-D                      ECS023
00737          MOVE S-HD               TO HEAD-7.                       ECS023
00738                                                                   ECS023
00739      IF L-CODE = '9'                                              ECS023
00740          MOVE L-CO               TO CP-NO                         ECS023
00741          MOVE COMP-TOT           TO HEAD-7.                       ECS023
00742                                                                   ECS023
00743      PERFORM 0560-HEADS THRU 0570-HEADS-EXIT.                     ECS023
00744                                                                   ECS023
00745      MOVE +40                    TO X4.                           ECS023
00746      MOVE KOUNT (2)              TO D-CNT.                        ECS023
00747                                                                   ECS023
00748      IF KOUNT (2) = ZERO                                          ECS023
00749          MOVE +1                 TO KOUNT (2).                    ECS023
00750                                                                   ECS023
00751      COMPUTE D-AGE  = AGE  (2) / KOUNT (2).                       ECS023
00752      COMPUTE D-TERM = TERM (2) / KOUNT (2).                       ECS023
00753      IF WEIGHT-FACE (2) NOT = ZERO                                ECS023
00754          COMPUTE D-WEIGHTED ROUNDED =                             ECS023
00755              (WEIGHT-WORK (2) / WEIGHT-FACE (2)).                 ECS023
00756                                                                   ECS023
00757      PERFORM 0610-DTL-PRT.                                        ECS023
00758                                                                   ECS023
00759  0350-COMP-BUMP.                                                  ECS023
00760      MOVE +40                    TO X1.                           ECS023
00761      MOVE +80                    TO X2.                           ECS023
00762      PERFORM 0580-BUMP-UP THRU 0600-B-U-X.                        ECS023
00763      MOVE +40                    TO X1.                           ECS023
00764      PERFORM 0130-CLEAR-40 THRU 0150-CLEAR-X.                     ECS023
00765      ADD KOUNT (2) TO KOUNT (3).                                  ECS023
00766      ADD AGE   (2) TO AGE   (3).                                  ECS023
00767      ADD TERM  (2) TO TERM  (3).                                  ECS023
00768      MOVE ZERO                   TO KOUNT (2)  AGE (2)  TERM (2). ECS023
00769      ADD WEIGHT-CNT  (2) TO WEIGHT-CNT  (3).                      ECS023
00770      ADD WEIGHT-FACE (2) TO WEIGHT-FACE (3).                      ECS023
00771      ADD WEIGHT-WORK (2) TO WEIGHT-WORK (3).                      ECS023
00772      MOVE ZERO                   TO WEIGHT-CNT  (2)               ECS023
00773                                     WEIGHT-FACE (2)               ECS023
00774                                     WEIGHT-WORK (2).              ECS023
00775                                                                   ECS023
00776  0355-ADVANCE-CNTL.                                               ECS023
00777      MOVE SUM-CTL                TO LAST-CTL.                     ECS023
020405     MOVE S-AM-NAME              TO L-NAME.
00778                                                                   ECS023
00779  0360-COMPANY-EXIT.                                               ECS023
00780      GO TO 0300-ACCUMULATE.                                       ECS023
00781  EJECT                                                            ECS023
00782  0370-STATE-BK.                                                   ECS023
00783      ADD +1 TO S-CNT.                                             ECS023
00784                                                                   ECS023
00785      IF C-CNT = +0                                                ECS023
00786          PERFORM 0350-COMP-BUMP                                   ECS023
00787          MOVE 'CARRIER GROUP'    TO H3B-DES                       ECS023
00788          MOVE SPACES             TO H4-CTL                        ECS023
00789          MOVE L-CO               TO H4-CMP                        ECS023
00790          STRING HEAD-SPACE DELIMITED BY SIZE                      ECS023
00791                 H4-CARR    DELIMITED BY SIZE                      ECS023
00792                 HEAD-SPACE DELIMITED BY SIZE                      ECS023
00793                 H4-GRP     DELIMITED BY SIZE                      ECS023
00794                 HEAD-SPACE DELIMITED BY SIZE                      ECS023
00795                 H4-STE     DELIMITED BY SIZE                      ECS023
00796          INTO   H4-DES                                            ECS023
00797      ELSE                                                         ECS023
00798          PERFORM 0340-COMPANY-BK THRU 0350-COMP-BUMP.             ECS023
00799                                                                   ECS023
00800      IF S-CNTL = L-CNTL                                           ECS023
00801          IF S-CO  NOT = L-CO OR                                   ECS023
00802             C-CNT NOT = +0                                        ECS023
00803              MOVE '1'            TO DUMMY-COMP-BK-SW.             ECS023
00804                                                                   ECS023
00805      MOVE ZERO                   TO C-CNT.                        ECS023
00806      MOVE SPACES                 TO HEAD-7.                       ECS023
00807                                                                   ECS023
00808      IF L-CODE = '9'                                              ECS023
00809          GO TO 0380-STATE-BUMP.                                   ECS023
00810                                                                   ECS023
00811      IF L-CODE = '1'                                              ECS023
00812          MOVE L-ST               TO A2-S                          ECS023
00813          MOVE L-CNTL             TO A2-A                          ECS023
020405*        PERFORM 0260-FIND-ACCT THRU 0290-EXIT                    ECS023
00815          MOVE L-NAME             TO A2-D                          ECS023
00816          MOVE A2-HD              TO HEAD-7.                       ECS023
00817                                                                   ECS023
00818      IF L-CODE = '2'                                              ECS023
00819          MOVE L-ST               TO AA2-S                         ECS023
00820          MOVE L-CNTL             TO AA2-A                         ECS023
00821          MOVE AA-HD2             TO HEAD-7.                       ECS023
00822                                                                   ECS023
00823      IF L-CODE = '3'                                              ECS023
00824          MOVE L-ST               TO AR2-S                         ECS023
00825          MOVE L-RPT2             TO AR2-A                         ECS023
00826          MOVE AR-HD2             TO HEAD-7.                       ECS023
00827                                                                   ECS023
00828      IF L-CODE = '4'                                              ECS023
00829          MOVE L-ST               TO AG2-S                         ECS023
00830          MOVE L-CNTL             TO AG2-G                         ECS023
00831          MOVE AG-HD2             TO HEAD-7.                       ECS023
00832                                                                   ECS023
00833      IF L-CODE = '5'                                              ECS023
00834          MOVE L-ST               TO B2-S                          ECS023
00835          MOVE L-CNTL             TO B2-T  TYP-HLD                 ECS023
00836          PERFORM 0520-TYPE-FIND THRU 0550-T-F-X                   ECS023
00837          MOVE TYP-DES            TO B2-D                          ECS023
00838          MOVE B2-HD              TO HEAD-7.                       ECS023
00839                                                                   ECS023
00840      IF L-CODE = '6'                                              ECS023
00841          MOVE L-ST               TO ST-NO                         ECS023
00842          MOVE CLAS-STARTS        TO CLAS-INDEXS                   ECS023
00843          PERFORM 0500-STATE-LOOKUP THRU 0510-STATE-LOOK-X         ECS023
00844          MOVE STATE-TOT          TO HEAD-7.                       ECS023
00845                                                                   ECS023
00846      PERFORM 0560-HEADS THRU 0570-HEADS-EXIT.                     ECS023
00847                                                                   ECS023
00848      MOVE KOUNT (3)              TO D-CNT.                        ECS023
00849      IF KOUNT (3) = ZERO                                          ECS023
00850          MOVE +1                 TO KOUNT (3).                    ECS023
00851                                                                   ECS023
00852      COMPUTE D-AGE  = AGE  (3) / KOUNT (3).                       ECS023
00853      COMPUTE D-TERM = TERM (3) / KOUNT (3).                       ECS023
00854      MOVE +80                    TO X4.                           ECS023
00855      IF WEIGHT-FACE (3) NOT = ZERO                                ECS023
00856          COMPUTE D-WEIGHTED ROUNDED =                             ECS023
00857              (WEIGHT-WORK (3) / WEIGHT-FACE (3)).                 ECS023
00858                                                                   ECS023
00859      PERFORM 0610-DTL-PRT.                                        ECS023
00860                                                                   ECS023
00861      IF S-CODE = '1' OR '2'                                       ECS023
00862          GO TO 0380-STATE-BUMP.                                   ECS023
00863                                                                   ECS023
00864      IF S-CNTL NOT = L-CNTL                                       ECS023
00865          GO TO 0380-STATE-BUMP                                    ECS023
00866      ELSE                                                         ECS023
00867          MOVE SUM-CTL                TO LAST-CTL
020405         MOVE S-AM-NAME              TO L-NAME
020405     END-IF
00868      .
00869  0380-STATE-BUMP.                                                 ECS023
00870      MOVE +80                    TO X1.                           ECS023
00871      MOVE +120                   TO X2.                           ECS023
00872      PERFORM 0580-BUMP-UP THRU 0600-B-U-X.                        ECS023
00873                                                                   ECS023
00874      MOVE +80                    TO X1.                           ECS023
00875      PERFORM 0130-CLEAR-40 THRU 0150-CLEAR-X.                     ECS023
00876                                                                   ECS023
00877      ADD KOUNT (3) TO KOUNT (4).                                  ECS023
00878      ADD AGE   (3) TO AGE   (4).                                  ECS023
00879      ADD TERM  (3) TO TERM  (4).                                  ECS023
00880      MOVE ZERO                   TO KOUNT (3)  AGE (3)  TERM (3). ECS023
00881                                                                   ECS023
00882      ADD WEIGHT-CNT  (3) TO WEIGHT-CNT  (4).                      ECS023
00883      ADD WEIGHT-FACE (3) TO WEIGHT-FACE (4).                      ECS023
00884      ADD WEIGHT-WORK (3) TO WEIGHT-WORK (4).                      ECS023
00885      MOVE ZERO                   TO WEIGHT-CNT  (3)               ECS023
00886                                     WEIGHT-FACE (3)               ECS023
00887                                     WEIGHT-WORK (3).              ECS023
00888                                                                   ECS023
00889  0390-STATE-EXIT.                                                 ECS023
00890      GO TO 0300-ACCUMULATE.                                       ECS023
00891  EJECT                                                            ECS023
00892  0400-ACCT-BK.                                                    ECS023
00893      IF L-CODE = '8'                                              ECS023
00894          PERFORM 0350-COMP-BUMP                                   ECS023
00895          PERFORM 0380-STATE-BUMP                                  ECS023
00896          MOVE 'CARRIER GROUP'     TO H3B-DES                      ECS023
00897          MOVE SPACES              TO H4-CTL                       ECS023
00898          MOVE L-CO                TO H4-CMP                       ECS023
00899          STRING HEAD-SPACE DELIMITED BY SIZE                      ECS023
00900                 H4-CARR    DELIMITED BY SIZE                      ECS023
00901                 HEAD-SPACE DELIMITED BY SIZE                      ECS023
00902                 H4-GRP     DELIMITED BY SIZE                      ECS023
00903          INTO   H4-DES                                            ECS023
00904          GO TO 0410-DO-ACCT.                                      ECS023
00905                                                                   ECS023
00906      IF L-CODE = '7'                                              ECS023
00907          PERFORM 0350-COMP-BUMP                                   ECS023
00908          PERFORM 0380-STATE-BUMP                                  ECS023
00909          MOVE SPACES              TO H3B-DES                      ECS023
00910                                      HEAD-4                       ECS023
00911          GO TO 0410-DO-ACCT.                                      ECS023
00912                                                                   ECS023
00913      IF S-CNT = +0 AND                                            ECS023
00914         C-CNT = +0                                                ECS023
00915          PERFORM 0350-COMP-BUMP                                   ECS023
00916          PERFORM 0380-STATE-BUMP                                  ECS023
00917          MOVE 'CARRIER GROUP STATE' TO H3B-DES                    ECS023
00918          MOVE SPACES                TO H4-CTL                     ECS023
00919          MOVE L-CO                  TO H4-CMP                     ECS023
00920          MOVE L-ST                  TO H4-STE                     ECS023
00921          STRING HEAD-SPACE DELIMITED BY SIZE                      ECS023
00922                 H4-CARR    DELIMITED BY SIZE                      ECS023
00923                 HEAD-SPACE DELIMITED BY SIZE                      ECS023
00924                 H4-GRP     DELIMITED BY SIZE                      ECS023
00925                 HEAD-SPACE DELIMITED BY SIZE                      ECS023
00926                 H4-STE     DELIMITED BY SIZE                      ECS023
00927          INTO   H4-DES                                            ECS023
00928          GO TO 0410-DO-ACCT.                                      ECS023
00929                                                                   ECS023
00930      IF C-CNT NOT = +0 AND                                        ECS023
00931         S-CNT = +0                                                ECS023
00932          PERFORM 0340-COMPANY-BK THRU 0350-COMP-BUMP              ECS023
00933          PERFORM 0380-STATE-BUMP                                  ECS023
00934          STRING HEAD-SPACE DELIMITED BY SIZE                      ECS023
00935                 H4-CARR    DELIMITED BY SIZE                      ECS023
00936                 HEAD-SPACE DELIMITED BY SIZE                      ECS023
00937                 H4-GRP     DELIMITED BY SIZE                      ECS023
00938                 HEAD-SPACE DELIMITED BY SIZE                      ECS023
00939                 H4-STE     DELIMITED BY SIZE                      ECS023
00940          INTO   H4-DES                                            ECS023
00941          GO TO 0410-DO-ACCT.                                      ECS023
00942                                                                   ECS023
00943      IF S-CNT NOT = +0 AND                                        ECS023
00944         C-CNT = +0                                                ECS023
00945          PERFORM 0370-STATE-BK THRU 0380-STATE-BUMP               ECS023
00946          IF DUMMY-COMP-BK-SW = '0'                                ECS023
00947              MOVE 'CARRIER GROUP' TO H3B-DES                      ECS023
00948              MOVE SPACES          TO H4-CTL                       ECS023
00949              MOVE L-CO            TO H4-CMP                       ECS023
00950              STRING HEAD-SPACE DELIMITED BY SIZE                  ECS023
00951                     H4-CARR    DELIMITED BY SIZE                  ECS023
00952                     HEAD-SPACE DELIMITED BY SIZE                  ECS023
00953                     H4-GRP     DELIMITED BY SIZE                  ECS023
00954                     HEAD-SPACE DELIMITED BY SIZE                  ECS023
00955                     H4-STE     DELIMITED BY SIZE                  ECS023
00956              INTO   H4-DES                                        ECS023
00957              GO TO 0410-DO-ACCT                                   ECS023
00958          ELSE                                                     ECS023
00959              MOVE '0'            TO DUMMY-COMP-BK-SW              ECS023
00960              GO TO 0410-DO-ACCT.                                  ECS023
00961                                                                   ECS023
00962      PERFORM 0370-STATE-BK THRU 0380-STATE-BUMP.                  ECS023
00963                                                                   ECS023
00964  0410-DO-ACCT.                                                    ECS023
00965      MOVE +0                     TO S-CNT  C-CNT.                 ECS023
00966                                                                   ECS023
00967      IF L-CODE = '6' OR '9'                                       ECS023
00968          GO TO 0420-ACCT-BUMP.                                    ECS023
00969                                                                   ECS023
00970      MOVE SPACES                 TO HEAD-7.                       ECS023
00971                                                                   ECS023
00972      IF L-CODE = '1'                                              ECS023
020405*        PERFORM 0260-FIND-ACCT THRU 0290-EXIT                    ECS023
020405         MOVE L-ACCT             TO AT-NO
00974 *        MOVE L-CNTL             TO AT-NO                         ECS023
00975          MOVE L-NAME             TO AT-NA                         ECS023
00976          MOVE ACCT-TOT           TO HEAD-7.                       ECS023
00977                                                                   ECS023
00978      IF L-CODE = '2'                                              ECS023
00979          MOVE L-CNTL             TO AG-NO                         ECS023
00980          MOVE AGENT-TOT          TO HEAD-7.                       ECS023
00981                                                                   ECS023
00982      IF L-CODE = '3'                                              ECS023
00983          MOVE L-RPT2             TO RPT2-NO                       ECS023
00984          MOVE CLAS-REPORT-CD2-CAPTION                             ECS023
00985                                  TO RPT2-CAP                      ECS023
00986          MOVE RPT2-TOT           TO HEAD-7.                       ECS023
00987                                                                   ECS023
00988      IF L-CODE = '4'                                              ECS023
00989          MOVE L-CNTL             TO GA-NO                         ECS023
00990          MOVE GA-TOT             TO HEAD-7.                       ECS023
00991                                                                   ECS023
00992      IF L-CODE = '5'                                              ECS023
00993          MOVE L-CNTL             TO CL-NO  TYP-HLD                ECS023
00994          PERFORM 0520-TYPE-FIND THRU 0550-T-F-X                   ECS023
00995          MOVE TYP-DES            TO CL-DES                        ECS023
00996          MOVE CLASS-TOT          TO HEAD-7.                       ECS023
00997                                                                   ECS023
00998      IF L-CODE = '7'                                              ECS023
00999          MOVE SPACES             TO H3B-DES                       ECS023
01000          MOVE L-CNTL             TO RPT1-NO                       ECS023
01001          MOVE CLAS-REPORT-CD1-CAPTION                             ECS023
01002                                  TO RPT1-CAP                      ECS023
01003          MOVE RPT1-TOT           TO HEAD-7.                       ECS023
01004                                                                   ECS023
01005      IF L-CODE = '8'                                              ECS023
01006          MOVE L-CNTL             TO RPT2-NO                       ECS023
01007          MOVE CLAS-REPORT-CD2-CAPTION                             ECS023
01008                                  TO RPT2-CAP                      ECS023
01009          MOVE RPT2-TOT           TO HEAD-7.                       ECS023
01010                                                                   ECS023
01011      PERFORM 0560-HEADS THRU 0570-HEADS-EXIT.                     ECS023
01012      MOVE KOUNT (4)              TO D-CNT.                        ECS023
01013                                                                   ECS023
01014      IF KOUNT (4) = ZERO                                          ECS023
01015          MOVE +1                 TO KOUNT (4).                    ECS023
01016                                                                   ECS023
01017      COMPUTE D-AGE  = AGE  (4) / KOUNT (4).                       ECS023
01018      COMPUTE D-TERM = TERM (4) / KOUNT (4).                       ECS023
01019      MOVE +120                   TO X4.                           ECS023
01020                                                                   ECS023
01021      IF WEIGHT-FACE (4) NOT = ZERO                                ECS023
01022          COMPUTE D-WEIGHTED ROUNDED =                             ECS023
01023              (WEIGHT-WORK (4) / WEIGHT-FACE (4)).                 ECS023
01024                                                                   ECS023
01025      PERFORM 0610-DTL-PRT.                                        ECS023
01026                                                                   ECS023
01027  0420-ACCT-BUMP.                                                  ECS023
01028      MOVE SUM-CTL                TO LAST-CTL
020405     MOVE S-AM-NAME              TO L-NAME
01029                                                                   ECS023
01030      MOVE +120                   TO X1.                           ECS023
01031      MOVE +160                   TO X2.                           ECS023
01032      PERFORM 0580-BUMP-UP THRU 0600-B-U-X.                        ECS023
01033                                                                   ECS023
01034      MOVE +120                   TO X1.                           ECS023
01035      PERFORM 0130-CLEAR-40 THRU 0150-CLEAR-X.                     ECS023
01036                                                                   ECS023
01037      ADD KOUNT (4) TO KOUNT (5).                                  ECS023
01038      ADD AGE   (4) TO AGE   (5).                                  ECS023
01039      ADD TERM  (4) TO TERM  (5).                                  ECS023
01040      MOVE ZERO                   TO KOUNT (4)  AGE (4)  TERM (4). ECS023
01041                                                                   ECS023
01042      ADD WEIGHT-CNT  (4) TO WEIGHT-CNT  (5).                      ECS023
01043      ADD WEIGHT-FACE (4) TO WEIGHT-FACE (5).                      ECS023
01044      ADD WEIGHT-WORK (4) TO WEIGHT-WORK (5).                      ECS023
01045      MOVE ZERO                   TO WEIGHT-CNT  (4)               ECS023
01046                                     WEIGHT-FACE (4)               ECS023
01047                                     WEIGHT-WORK (4).              ECS023
01048                                                                   ECS023
01049  0430-ACCT-EXIT.                                                  ECS023
01050      GO TO 0300-ACCUMULATE.                                       ECS023
01051  EJECT                                                            ECS023
01052  0440-CODE-BK.                                                    ECS023
01053      PERFORM 0400-ACCT-BK THRU 0420-ACCT-BUMP.                    ECS023
01054      MOVE +160                   TO X1.                           ECS023
01055      PERFORM 0130-CLEAR-40 THRU 0150-CLEAR-X.                     ECS023
01056      MOVE ZERO                   TO KOUNT (5)  AGE (5)  TERM (5). ECS023
01057      GO TO 0300-ACCUMULATE.                                       ECS023
01058                                                                   ECS023
01059  0450-OVER-BK.                                                    ECS023
01060      IF SUMMARY-COUNT = ZERO                                      ECS023
01061          PERFORM 0560-HEADS THRU 0570-HEADS-EXIT                  ECS023
01062          MOVE '-'                TO P-CTL                            CL**2
01063          MOVE 'NO DATA ECS023'   TO H7-DES                        ECS023
01064          MOVE HEAD-7             TO P-DATA                        ECS023
01065          PERFORM 0910-PRT-LINE THRU 0930-EXIT                     ECS023
01066          GO TO 0460-CLOSE-FILES.                                  ECS023
01067                                                                   ECS023
01068      PERFORM 0400-ACCT-BK THRU 0420-ACCT-BUMP.                    ECS023
01069      MOVE ' REPORT TOTALS'       TO H3A-DES H7-DES.               ECS023
01070      MOVE SPACES                 TO H3B-DES                       ECS023
01071                                     H4-DES.                       ECS023
01072      MOVE 'X'                    TO L-CODE.                       ECS023
01073      PERFORM 0560-HEADS THRU 0570-HEADS-EXIT.                     ECS023
01074      MOVE KOUNT (5)              TO D-CNT.                        ECS023
01075                                                                   ECS023
01076      IF KOUNT (5) = ZERO                                          ECS023
01077          MOVE +1                 TO KOUNT (5).                    ECS023
01078                                                                   ECS023
01079      COMPUTE D-AGE  = AGE  (5) / KOUNT (5).                       ECS023
01080      COMPUTE D-TERM = TERM (5) / KOUNT (5).                       ECS023
01081      MOVE +160                   TO X4.                           ECS023
01082                                                                   ECS023
01083      IF WEIGHT-FACE (5) NOT = ZERO                                ECS023
01084          COMPUTE D-WEIGHTED ROUNDED =                             ECS023
01085              (WEIGHT-WORK (5) / WEIGHT-FACE (5)).                 ECS023
01086                                                                   ECS023
01087      PERFORM 0610-DTL-PRT.                                        ECS023
01088      MOVE +160                   TO X1.                           ECS023
01089      PERFORM 0130-CLEAR-40 THRU 0150-CLEAR-X.                     ECS023
01090      MOVE ZERO                   TO KOUNT (5)  AGE (5)  TERM (5). ECS023
01091  EJECT                                                            ECS023
01092  0460-CLOSE-FILES.                                                ECS023
01093      COPY ELCPRTC.                                                ECS023
01094      CLOSE SUMMARY  PRINTER.                                      ECS023
01095                                                                   ECS023
01096  0470-ACCT-LOOP.                                                  ECS023
01097      IF AM-MSTR-CNTRL = HIGH-VALUES                               ECS023
01098          GO TO 0480-CLOSE-EXIT.                                   ECS023
01099                                                                   ECS023
01100      PERFORM 0220-READ-ACCT THRU 0230-EXIT.                       ECS023
01101                                                                   ECS023
01102      GO TO 0470-ACCT-LOOP.                                        ECS023
01103                                                                   ECS023
01104  0480-CLOSE-EXIT.                                                 ECS023
01105      EXIT.                                                        ECS023
01106  EJECT                                                            ECS023
01107                                                                   ECS023
01108  0500-STATE-LOOKUP.                                               ECS023
01109      IF (CLAS-MAXS NOT GREATER ZERO)   OR                         ECS023
01110         (CLAS-INDEXS GREATER CLAS-MAXS)                           ECS023
01111           MOVE 'INVALID STATE'    TO ST-DES                       ECS023
01112           GO TO 0510-STATE-LOOK-X.                                ECS023
01113                                                                   ECS023
01114      IF L-ST NOT = STATE-SUB (CLAS-INDEXS)                        ECS023
01115          ADD +1 TO CLAS-INDEXS                                    ECS023
01116          GO TO 0500-STATE-LOOKUP.                                 ECS023
01117                                                                   ECS023
01118      MOVE STATE-PIC (CLAS-INDEXS) TO ST-DES.                      ECS023
01119                                                                   ECS023
01120  0510-STATE-LOOK-X.                                               ECS023
01121      EXIT.                                                        ECS023
01122                                                                   ECS023
01123  0520-TYPE-FIND.                                                  ECS023
01124      IF CLAS-MAXB NOT GREATER THAN ZEROS                          ECS023
01125          MOVE 'UNKNOWN'          TO TYP-DES                       ECS023
01126          GO TO 0550-T-F-X.                                        ECS023
01127                                                                   ECS023
01128      IF FIRST-SW = 'X'                                            ECS023
01129          MOVE ' '                TO FIRST-SW                      ECS023
01130          MOVE CLAS-STARTB        TO CLAS-INDEXB                   ECS023
01131          GO TO 0530-TYP-LOOP.                                     ECS023
01132                                                                   ECS023
01133      IF TYP-HLD = CLAS-BUSC-CODE (CLAS-INDEXB)                    ECS023
01134          GO TO 0540-TYP-X.                                        ECS023
01135                                                                   ECS023
01136      MOVE CLAS-STARTB            TO CLAS-INDEXB.                  ECS023
01137                                                                   ECS023
01138  0530-TYP-LOOP.                                                   ECS023
01139      IF CLAS-INDEXB GREATER THAN CLAS-MAXB                        ECS023
01140          MOVE 'UNKNOWN'          TO TYP-DES                       ECS023
01141          GO TO 0550-T-F-X.                                        ECS023
01142                                                                   ECS023
01143      IF TYP-HLD NOT = CLAS-BUSC-CODE (CLAS-INDEXB)                ECS023
01144          ADD +1 TO CLAS-INDEXB                                    ECS023
01145          GO TO 0530-TYP-LOOP.                                     ECS023
01146                                                                   ECS023
01147  0540-TYP-X.                                                      ECS023
01148      MOVE CLAS-BUSC-DESC (CLAS-INDEXB) TO TYP-DES.                ECS023
01149                                                                   ECS023
01150  0550-T-F-X.                                                      ECS023
01151      EXIT.                                                        ECS023
01152  EJECT                                                            ECS023
01153  0560-HEADS.                                                      ECS023
01154      ADD +1 TO PAGE-CT.                                           ECS023
01155      MOVE PAGE-CT                TO H3-PAGE.                      ECS023
01156                                                                   ECS023
01157      IF L-CODE = '1'                                              ECS023
01158          MOVE 'A'                TO H1-RPT.                       ECS023
01159      IF L-CODE = '2'                                              ECS023
01160          MOVE 'B'                TO H1-RPT.                       ECS023
01161      IF L-CODE = '3'                                              ECS023
01162          MOVE 'C'                TO H1-RPT.                       ECS023
01163      IF L-CODE = '4'                                              ECS023
01164          MOVE 'D'                TO H1-RPT.                       ECS023
01165      IF L-CODE = '5'                                              ECS023
01166          MOVE 'E'                TO H1-RPT.                       ECS023
01167      IF L-CODE = '6'                                              ECS023
01168          MOVE 'F'                TO H1-RPT.                       ECS023
01169      IF L-CODE = '9'                                              ECS023
01170          MOVE 'G'                TO H1-RPT.                       ECS023
01171                                                                   ECS023
01172      MOVE HEAD-1                 TO P-DATA.                       ECS023
01173      MOVE '1'                    TO P-CTL.                           CL**2
01174      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01175                                                                   ECS023
01176      IF L-CODE = '1'
01177         MOVE '    ACCOUNT SUMMARY'
01178                                  TO H3A-DES
020405        MOVE 'REPORT CODE 1:'    TO H3A-DES-RC1
020405        MOVE L-CNTL              TO H3A-RPT-CODE1
020405     END-IF

01179                                                                   ECS023
01180      IF L-CODE = '2'                                              ECS023
01181          MOVE '    AGENT SUMMARY'                                 ECS023
01182                                  TO H3A-DES.                      ECS023
01183                                                                   ECS023
01184      IF L-CODE = '3'                                              ECS023
01185          MOVE ' RPT CD 2 SUMMARY'                                 ECS023
01186                                  TO H3A-DES.                      ECS023
01187                                                                   ECS023
01188      IF L-CODE = '4'                                              ECS023
01189          MOVE '    GEN. AGENT SUMMARY'                            ECS023
01190                                  TO H3A-DES.                      ECS023
01191                                                                   ECS023
01192      IF L-CODE = '5'                                              ECS023
01193          MOVE '    BUS. TYPE SUMMARY'                             ECS023
01194                                   TO H3A-DES.                     ECS023
01195                                                                   ECS023
01196      IF L-CODE = '6'                                              ECS023
01197          MOVE '    STATE SUMMARY'                                 ECS023
01198                                   TO H3A-DES.                     ECS023
01199                                                                   ECS023
01200      IF L-CODE = '7'                                              ECS023
01201          MOVE SPACES              TO H3A-DES                      ECS023
01202          MOVE CLAS-REPORT-CD1-CAPTION                             ECS023
01203                                   TO REPT-NAME                    ECS023
01204          MOVE REPT-HEAD           TO H3A-DES.                     ECS023
01205                                                                   ECS023
01206      IF L-CODE = '8' OR '3'                                       ECS023
01207          MOVE SPACES              TO H3A-DES                      ECS023
01208          MOVE CLAS-REPORT-CD2-CAPTION                             ECS023
01209                                   TO REPT-NAME                    ECS023
01210          MOVE REPT-HEAD           TO H3A-DES.                     ECS023
01211                                                                   ECS023
01212      IF L-CODE = '9'                                              ECS023
01213          MOVE 'CARRIER/GROUP SUMMARY'                             ECS023
01214                                   TO H3A-DES.                     ECS023
01215                                                                   ECS023
01216      MOVE HEAD-2                 TO P-DATA.                       ECS023
01217      MOVE ' '                    TO P-CTL.                           CL**2
01218      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01219                                                                   ECS023
01220      MOVE HEAD-3                 TO P-DATA.                       ECS023
01221      MOVE ' '                    TO P-CTL.                           CL**2
01222      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01223                                                                   ECS023
01224      MOVE HEAD-3A                TO P-DATA.                       ECS023
01225      MOVE ' '                    TO P-CTL.                           CL**2
01226      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01227                                                                   ECS023
01228      MOVE HEAD-3B                TO P-DATA.                       ECS023
01229      MOVE ' '                    TO P-CTL.                           CL**2
01230      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01231                                                                   ECS023
01232      MOVE HEAD-4                 TO P-DATA.                       ECS023
01233      MOVE ' '                    TO P-CTL.                           CL**2
01234      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01235                                                                   ECS023
01236      MOVE ALL '*'                TO P-DATA.                       ECS023
01237      MOVE ' '                    TO P-CTL.                           CL**2
01238      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01239                                                                   ECS023
01240      MOVE HEAD-5                 TO P-DATA.                       ECS023
01241      MOVE ' '                    TO P-CTL.                           CL**2
01242      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01243                                                                   ECS023
01244      IF DTE-CLIENT = 'MIL'                                        ECS023
01245          MOVE MIL-HEAD-6         TO P-DATA                        ECS023
01246      ELSE                                                         ECS023
01247          MOVE HEAD-6             TO P-DATA.                       ECS023
01248                                                                   ECS023
01249      MOVE ' '                    TO P-CTL.                           CL**2
01250      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01251                                                                   ECS023
01252      MOVE ALL '*'                TO P-DATA.                       ECS023
01253      MOVE ' '                    TO P-CTL.                           CL**2
01254      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01255                                                                   ECS023
020405     MOVE SPACES                 TO HEAD-3A
           .
01256  0570-HEADS-EXIT.                                                 ECS023
01257      EXIT.                                                        ECS023
01258  EJECT                                                            ECS023
01259  0580-BUMP-UP.                                                    ECS023
01260      MOVE +0                     TO X3.                           ECS023
01261                                                                   ECS023
01262  0590-BUMP-LOOP.                                                  ECS023
01263      ADD +1 TO X3.                                                ECS023
01264      IF X3 GREATER THAN +40                                       ECS023
01265          GO TO 0600-B-U-X.                                        ECS023
01266                                                                   ECS023
01267      ADD +1 TO X1                                                 ECS023
01268                X2.                                                ECS023
01269      ADD L-PREM (X1) TO L-PREM (X2).                              ECS023
01270      ADD A-PREM (X1) TO A-PREM (X2).                              ECS023
01271      ADD L-BEN  (X1) TO L-BEN  (X2).                              ECS023
01272      ADD A-BEN  (X1) TO A-BEN  (X2).                              ECS023
01273      ADD L-CT   (X1) TO L-CT   (X2).                              ECS023
01274      ADD A-CT   (X1) TO A-CT   (X2).                              ECS023
01275      GO TO 0590-BUMP-LOOP.                                        ECS023
01276                                                                   ECS023
01277  0600-B-U-X.                                                      ECS023
01278      EXIT.                                                        ECS023
01279  EJECT                                                            ECS023
01280  0610-DTL-PRT.                                                    ECS023
01281      MOVE HEAD-7                 TO P-DATA.                       ECS023
01282      MOVE '0'                    TO P-CTL.                           CL**2
01283      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01284                                                                   ECS023
01285      MOVE SPACES                 TO HEAD-7.                       ECS023
01286      MOVE ALL '*'                TO H7-DES.                       ECS023
01287      MOVE HEAD-7                 TO P-DATA.                       ECS023
01288      MOVE ' '                    TO P-CTL.                           CL**2
01289      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01290                                                                   ECS023
01291      MOVE PA-HEAD                TO P-DATA.                       ECS023
01292      MOVE ' '                    TO P-CTL.                           CL**2
01293      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01294                                                                   ECS023
01295      MOVE HEAD-7                 TO P-DATA.                       ECS023
01296      MOVE ' '                    TO P-CTL.                           CL**2
01297      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01298                                                                   ECS023
01299      MOVE LIFE-OVERRIDE-L6       TO P-DATA.                       ECS023
01300      MOVE ' '                    TO P-CTL.                           CL**2
01301      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01302                                                                   ECS023
01303      MOVE SPACES                 TO DTL-LINE.                     ECS023
01304      PERFORM 0620-LIFE-PCTS THRU 0660-DO-HOLD-13.                 ECS023
01305      MOVE +0                     TO X2.                           ECS023
01306      MOVE X4                     TO X1.                           ECS023
01307      PERFORM 0730-TOTAL-SET THRU 0760-T-S-X.                      ECS023
01308      MOVE AH-OVERRIDE-L6         TO P-DATA.                       ECS023
01309      MOVE '-'                    TO P-CTL.                           CL**2
01310      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01311                                                                   ECS023
01312      PERFORM 0670-A-H-PCTS THRU 0710-DO-HOLD-A13.                 ECS023
01313      MOVE +1                     TO X2.                           ECS023
01314      MOVE X4                     TO X1.                           ECS023
01315      PERFORM 0730-TOTAL-SET THRU 0760-T-S-X.                      ECS023
01316      MOVE ALL '*'                TO P-DATA.                       ECS023
01317      MOVE '0'                    TO P-CTL.                           CL**2
01318      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01319                                                                   ECS023
01320      MOVE AE-HEAD                TO P-DATA.                       ECS023
01321      MOVE ' '                    TO P-CTL.                           CL**2
01322      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01323                                                                   ECS023
01324      MOVE HEAD-7                 TO P-DATA.                       ECS023
01325      MOVE ' '                    TO P-CTL.                           CL**2
01326      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01327                                                                   ECS023
01328      MOVE LIFE-OVERRIDE-L6       TO P-DATA.                       ECS023
01329      MOVE '0'                    TO P-CTL.                           CL**2
01330      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01331                                                                   ECS023
01332      MOVE +2                     TO X2.                           ECS023
01333      MOVE X4                     TO X1.                           ECS023
01334      MOVE SPACES                 TO DTL-LINE.                     ECS023
01335      PERFORM 0720-ZERO-HOLD-PCT.                                  ECS023
01336      PERFORM 0730-TOTAL-SET.                                      ECS023
01337      MOVE AH-OVERRIDE-L6         TO P-DATA.                       ECS023
01338      MOVE '0'                    TO P-CTL.                           CL**2
01339      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01340                                                                   ECS023
01341      MOVE +3                     TO X2.                           ECS023
01342      MOVE X4                     TO X1.                           ECS023
01343      PERFORM 0730-TOTAL-SET.                                      ECS023
01344      MOVE COUNT-LINE             TO P-DATA.                       ECS023
01345      MOVE '0'                    TO P-CTL.                           CL**2
01346      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01347                                                                   ECS023
01348      MOVE AGE-LINE               TO P-DATA.                       ECS023
01349      MOVE ' '                    TO P-CTL.                           CL**2
01350      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01351                                                                   ECS023
01352      MOVE WEIGHTED-LINE          TO P-DATA.                       ECS023
01353      MOVE ' '                    TO P-CTL.                           CL**2
01354      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01355                                                                   ECS023
01356      MOVE TERM-LINE              TO P-DATA.                       ECS023
01357      MOVE ' '                    TO P-CTL.                           CL**2
01358      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01359                                                                   ECS023
01360  0620-LIFE-PCTS.                                                  ECS023
01361      COMPUTE X2 = X4 + +40.                                       ECS023
01362      IF L-PREM (X2) = ZERO                                        ECS023
01363          PERFORM 0720-ZERO-HOLD-PCT                               ECS023
01364          GO TO 0660-DO-HOLD-13.                                   ECS023
01365                                                                   ECS023
01366      COMPUTE X1 = X4 + +8.                                        ECS023
01367      MOVE ZERO                   TO TOT-PREM.                     ECS023
01368      MOVE +1                     TO X5.                           ECS023
01369      MOVE +0                     TO X3.                           ECS023
01370                                                                   ECS023
01371  0630-L-PCT-LOOP-1.                                               ECS023
01372      ADD +1 TO X3.                                                ECS023
01373      IF X3 GREATER THAN +4                                        ECS023
01374          GO TO 0640-DO-HOLD-5.                                    ECS023
01375                                                                   ECS023
01376      COMPUTE TOT-PREM = TOT-PREM + L-PREM (X1).                   ECS023
01377      COMPUTE HOLD-PCT (X5) = ((TOT-PREM * +100) /                 ECS023
01378                              L-PREM (X2)) + +.51.                 ECS023
01379                                                                   ECS023
01380      COMPUTE PRM1 = (L-PREM (X2) * HOLD-PCT (X5)) / +100.         ECS023
01381      IF PRM1 GREATER THAN TOT-PREM                                ECS023
01382          SUBTRACT +1 FROM HOLD-PCT (X5).                          ECS023
01383                                                                   ECS023
01384      COMPUTE TOT-PREM = TOT-PREM - ((L-PREM (X2) *                ECS023
01385                         HOLD-PCT (X5)) / +100).                   ECS023
01386      ADD +8 TO X1.                                                ECS023
01387      ADD +1 TO X5.                                                ECS023
01388                                                                   ECS023
01389      GO TO 0630-L-PCT-LOOP-1.                                     ECS023
01390                                                                   ECS023
01391  0640-DO-HOLD-5.                                                  ECS023
01392      COMPUTE HOLD-PCT (5) = HOLD-PCT (1) + HOLD-PCT (2)           ECS023
01393                           + HOLD-PCT (3) + HOLD-PCT (4).          ECS023
01394      COMPUTE X1 = X4 + +33.                                       ECS023
01395      MOVE +6                     TO X5.                           ECS023
01396      MOVE +0                     TO X3.                           ECS023
01397      MOVE ZERO                   TO TOT-PREM.                     ECS023
01398                                                                   ECS023
01399  0650-L-PCT-LOOP.                                                 ECS023
01400      ADD +1 TO X3.                                                ECS023
01401      IF X3 GREATER THAN +7                                        ECS023
01402          GO TO 0660-DO-HOLD-13.                                   ECS023
01403                                                                   ECS023
01404      COMPUTE TOT-PREM = TOT-PREM + L-PREM (X1).                   ECS023
01405      COMPUTE HOLD-PCT (X5) = ((TOT-PREM * +100) /                 ECS023
01406                              L-PREM (X2)) + +.51.                 ECS023
01407                                                                   ECS023
01408      COMPUTE PRM1 = (L-PREM (X2) * HOLD-PCT (X5)) / +100.         ECS023
01409      IF PRM1 GREATER THAN TOT-PREM                                ECS023
01410          SUBTRACT +1 FROM HOLD-PCT (X5).                          ECS023
01411                                                                   ECS023
01412      COMPUTE TOT-PREM = TOT-PREM - ((L-PREM (X2) *                ECS023
01413                         HOLD-PCT (X5)) / +100).                   ECS023
01414      ADD +1 TO X1                                                 ECS023
01415                X5.                                                ECS023
01416                                                                   ECS023
01417      GO TO 0650-L-PCT-LOOP.                                       ECS023
01418  EJECT                                                            ECS023
01419  0660-DO-HOLD-13.                                                 ECS023
01420      COMPUTE HOLD-PCT (13) = HOLD-PCT (6) + HOLD-PCT (7) +        ECS023
01421              HOLD-PCT (8) + HOLD-PCT (9) + HOLD-PCT (10) +        ECS023
01422              HOLD-PCT (11) + HOLD-PCT (12).                       ECS023
01423                                                                   ECS023
01424  0670-A-H-PCTS.                                                   ECS023
01425      COMPUTE X2 = X4 + +40.                                       ECS023
01426      IF A-PREM (X2) = ZERO                                        ECS023
01427          PERFORM 0720-ZERO-HOLD-PCT                               ECS023
01428          GO TO 0710-DO-HOLD-A13.                                  ECS023
01429                                                                   ECS023
01430      COMPUTE X1 = X4 + +8.                                        ECS023
01431      MOVE ZERO                   TO TOT-PREM.                     ECS023
01432      MOVE +1                     TO X5.                           ECS023
01433      MOVE +0                     TO X3.                           ECS023
01434                                                                   ECS023
01435  0680-A-PCT-L-1.                                                  ECS023
01436      ADD +1 TO X3.                                                ECS023
01437      IF X3 GREATER THAN +4                                        ECS023
01438          GO TO 0690-DO-HOLD-A5.                                   ECS023
01439                                                                   ECS023
01440      COMPUTE TOT-PREM = TOT-PREM + A-PREM (X1).                   ECS023
01441      COMPUTE HOLD-PCT (X5) = ((TOT-PREM * +100) /                 ECS023
01442                              A-PREM (X2)) + +.51.                 ECS023
01443                                                                   ECS023
01444      COMPUTE PRM1 = (A-PREM (X2) * HOLD-PCT (X5)) / +100.         ECS023
01445      IF PRM1 GREATER THAN TOT-PREM                                ECS023
01446          SUBTRACT +1 FROM HOLD-PCT (X5).                          ECS023
01447                                                                   ECS023
01448      COMPUTE TOT-PREM = TOT-PREM - ((A-PREM (X2) *                ECS023
01449                         HOLD-PCT (X5)) / +100).                   ECS023
01450      ADD +8 TO X1.                                                ECS023
01451      ADD +1 TO X5.                                                ECS023
01452                                                                   ECS023
01453      GO TO 0680-A-PCT-L-1.                                        ECS023
01454                                                                   ECS023
01455  0690-DO-HOLD-A5.                                                 ECS023
01456      COMPUTE HOLD-PCT (5) = HOLD-PCT (1) + HOLD-PCT (2)           ECS023
01457                           + HOLD-PCT (3) + HOLD-PCT (4).          ECS023
01458      COMPUTE X1 = X4 + +33.                                       ECS023
01459      MOVE +6                     TO X5.                           ECS023
01460      MOVE +0                     TO X3.                           ECS023
01461      MOVE ZERO                   TO TOT-PREM.                     ECS023
01462                                                                   ECS023
01463  0700-A-PCT-L.                                                    ECS023
01464      ADD +1 TO X3.                                                ECS023
01465      IF X3 GREATER THAN +7                                        ECS023
01466          GO TO 0710-DO-HOLD-A13.                                  ECS023
01467                                                                   ECS023
01468      COMPUTE TOT-PREM = TOT-PREM + A-PREM (X1).                   ECS023
01469      COMPUTE HOLD-PCT (X5) = ((TOT-PREM * +100) /                 ECS023
01470                              A-PREM (X2)) + +.51.                 ECS023
01471                                                                   ECS023
01472      COMPUTE PRM1 = (A-PREM (X2) * HOLD-PCT (X5)) / +100.         ECS023
01473      IF PRM1 GREATER THAN TOT-PREM                                ECS023
01474          SUBTRACT +1 FROM HOLD-PCT (X5).                          ECS023
01475                                                                   ECS023
01476      COMPUTE TOT-PREM = TOT-PREM - ((A-PREM (X2) *                ECS023
01477                         HOLD-PCT (X5)) / +100).                   ECS023
01478      ADD +1 TO X1                                                 ECS023
01479                X5.                                                ECS023
01480                                                                   ECS023
01481      GO TO 0700-A-PCT-L.                                          ECS023
01482                                                                   ECS023
01483  0710-DO-HOLD-A13.                                                ECS023
01484      COMPUTE HOLD-PCT (13) = HOLD-PCT (7) + HOLD-PCT (8) +        ECS023
01485              HOLD-PCT (9) + HOLD-PCT (10) + HOLD-PCT (11) +       ECS023
01486              HOLD-PCT (12) + HOLD-PCT (6).                        ECS023
01487                                                                   ECS023
01488  0720-ZERO-HOLD-PCT.                                              ECS023
01489      MOVE ZERO TO HOLD-PCT (1)   HOLD-PCT (2)   HOLD-PCT (3)      ECS023
01490                   HOLD-PCT (4)   HOLD-PCT (5)   HOLD-PCT (6)      ECS023
01491                   HOLD-PCT (7)   HOLD-PCT (8)   HOLD-PCT (9)      ECS023
01492                   HOLD-PCT (10)  HOLD-PCT (11)  HOLD-PCT (12)     ECS023
01493                   HOLD-PCT (13).                                  ECS023
01494  EJECT                                                            ECS023
01495  0730-TOTAL-SET.                                                  ECS023
01496      PERFORM 0770-LOAD-8 THRU 0780-EXIT.                          ECS023
01497      MOVE HOLD-PCT (1)           TO D-EPCT.                       ECS023
01498                                                                   ECS023
01499      IF HOLD-PCT (1) NOT = ZERO                                   ECS023
01500          MOVE '%'                TO D-EPCT1                       ECS023
01501      ELSE                                                         ECS023
01502          MOVE ' '                TO D-EPCT1.                      ECS023
01503                                                                   ECS023
01504      IF DTE-CLIENT = 'NCB'                                           CL**3
01505          MOVE 'T  0-48'          TO D-DES                            CL**3
01506      ELSE                                                            CL**3
01507          MOVE 'T  0-18'          TO D-DES.                           CL**3
01508      MOVE DTL-LINE               TO P-DATA.                       ECS023
01509      MOVE ' '                    TO P-CTL.                           CL**2
01510                                                                   ECS023
01511      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01512      PERFORM 0770-LOAD-8   THRU 0780-EXIT.                        ECS023
01513                                                                   ECS023
01514      MOVE HOLD-PCT (2)           TO D-EPCT.                       ECS023
01515                                                                   ECS023
01516      IF HOLD-PCT (2) NOT = ZERO                                   ECS023
01517          MOVE '%'                TO D-EPCT1                       ECS023
01518      ELSE                                                         ECS023
01519          MOVE ' '                TO D-EPCT1.                      ECS023
01520                                                                   ECS023
01521      IF DTE-CLIENT = 'NCB'                                           CL**3
01522          MOVE 'E 49-60'          TO D-DES                            CL**3
01523      ELSE                                                            CL**3
01524          MOVE 'E 19-36'          TO D-DES.                           CL**3
01525      MOVE DTL-LINE               TO P-DATA.                       ECS023
01526      MOVE ' '                    TO P-CTL.                           CL**2
01527                                                                   ECS023
01528      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01529      PERFORM 0770-LOAD-8   THRU 0780-EXIT.                        ECS023
01530                                                                   ECS023
01531      MOVE HOLD-PCT (3)           TO D-EPCT.                       ECS023
01532                                                                   ECS023
01533      IF HOLD-PCT (3) NOT = ZERO                                   ECS023
01534          MOVE '%'                TO D-EPCT1                       ECS023
01535      ELSE                                                         ECS023
01536          MOVE ' '                TO D-EPCT1.                      ECS023
01537                                                                   ECS023
01538      IF DTE-CLIENT = 'NCB'                                           CL**3
01539          MOVE 'R OVR60'          TO D-DES                            CL**3
01540      ELSE                                                            CL**3
01541          MOVE 'R 37-60'          TO D-DES.                           CL**3
01542      MOVE DTL-LINE               TO P-DATA.                       ECS023
01543      MOVE ' '                    TO P-CTL.                           CL**2
01544                                                                   ECS023
01545      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01546      PERFORM 0770-LOAD-8   THRU 0780-EXIT.                        ECS023
01547                                                                   ECS023
01548      MOVE HOLD-PCT (4)           TO D-EPCT.                       ECS023
01549      IF HOLD-PCT (4) NOT = ZERO                                   ECS023
01550          MOVE '%'                TO D-EPCT1                       ECS023
01551      ELSE                                                         ECS023
01552          MOVE ' '                TO D-EPCT1.                      ECS023
01553                                                                   ECS023
01554      IF DTE-CLIENT = 'NCB'                                           CL**3
01555          MOVE 'M OV120'          TO D-DES                            CL**3
01556      ELSE                                                            CL**3
01557          MOVE 'M 61---'          TO D-DES.                           CL**3
01558      MOVE DTL-LINE               TO P-DATA.                       ECS023
01559      MOVE ' '                    TO P-CTL.                           CL**2
01560                                                                   ECS023
01561      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01562      PERFORM 0770-LOAD-8 THRU 0780-EXIT.                          ECS023
01563                                                                   ECS023
01564      MOVE HOLD-PCT (5)           TO D-EPCT.                       ECS023
01565      IF HOLD-PCT (5) NOT = ZERO                                   ECS023
01566          MOVE '%'                TO D-EPCT1                       ECS023
01567      ELSE                                                         ECS023
01568          MOVE ' '                TO D-EPCT1.                      ECS023
01569                                                                   ECS023
01570      MOVE ' TOTAL'               TO D-DES2.                       ECS023
01571      MOVE DTL-LINE               TO P-DATA.                       ECS023
01572      MOVE '0'                    TO P-CTL.                           CL**2
01573      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01574                                                                   ECS023
01575      MOVE SPACES                 TO DTL-LINE.                     ECS023
01576      MOVE +6                     TO X1.                           ECS023
01577      MOVE +0                     TO X2.                           ECS023
01578                                                                   ECS023
01579  0740-PCT-LINE.                                                   ECS023
01580      ADD +1 TO X2.                                                ECS023
01581      IF X2 GREATER THAN +8                                        ECS023
01582          GO TO 0750-PCT-LOADED.                                   ECS023
01583                                                                   ECS023
01584      MOVE HOLD-PCT (X1)          TO D-PCT (X2).                   ECS023
01585      IF HOLD-PCT (X1) NOT = ZERO                                  ECS023
01586          MOVE '%'                TO D-PCT1 (X2)                   ECS023
01587      ELSE                                                         ECS023
01588          MOVE ' '                TO D-PCT1 (X2).                  ECS023
01589                                                                   ECS023
01590      ADD +1 TO X1.                                                ECS023
01591                                                                   ECS023
01592      GO TO 0740-PCT-LINE.                                         ECS023
01593                                                                   ECS023
01594  0750-PCT-LOADED.                                                 ECS023
01595      MOVE 'PERCENT'              TO D-DES.                        ECS023
01596      MOVE ' '                    TO P-CTL.                           CL**2
01597      MOVE DTL-LINE               TO P-DATA.                       ECS023
01598      PERFORM 0910-PRT-LINE THRU 0930-EXIT.                        ECS023
01599                                                                   ECS023
01600  0760-T-S-X.                                                      ECS023
01601      EXIT.                                                        ECS023
01602                                                                   ECS023
01603  0770-LOAD-8.                                                     ECS023
01604      IF X2 = +0                                                   ECS023
01605          PERFORM 0790-LIFE-PREM-LOAD THRU 0810-EXIT.              ECS023
01606      IF X2 = +1                                                   ECS023
01607          PERFORM 0820-A-H-PREM-LOAD  THRU 0840-EXIT.              ECS023
01608      IF X2 = +2                                                   ECS023
01609          PERFORM 0850-LIFE-BEN-LOAD  THRU 0870-EXIT.              ECS023
01610      IF X2 = +3                                                   ECS023
01611          PERFORM 0880-A-H-BEN-LOAD   THRU 0900-EXIT.              ECS023
01612                                                                   ECS023
01613  0780-EXIT.                                                       ECS023
01614      EXIT.                                                        ECS023
01615                                                                   ECS023
01616  0790-LIFE-PREM-LOAD.                                             ECS023
01617      MOVE +0                     TO X3.                           ECS023
01618                                                                   ECS023
01619  0800-L-P-L.                                                      ECS023
01620      ADD +1 TO X3.                                                ECS023
01621      IF X3 GREATER THAN +8                                        ECS023
01622          GO TO 0810-EXIT.                                         ECS023
01623                                                                   ECS023
01624      ADD +1 TO X1.                                                ECS023
01625      MOVE L-PREM (X1)            TO D-AMT (X3).                   ECS023
01626                                                                   ECS023
01627      GO TO 0800-L-P-L.                                            ECS023
01628                                                                   ECS023
01629  0810-EXIT.                                                       ECS023
01630      EXIT.                                                        ECS023
01631                                                                   ECS023
01632  0820-A-H-PREM-LOAD.                                              ECS023
01633      MOVE +0                     TO X3.                           ECS023
01634                                                                   ECS023
01635  0830-A-P-L.                                                      ECS023
01636      ADD +1 TO X3.                                                ECS023
01637      IF X3 GREATER THAN +8                                        ECS023
01638          GO TO 0840-EXIT.                                         ECS023
01639                                                                   ECS023
01640      ADD +1 TO X1.                                                ECS023
01641      MOVE A-PREM (X1)            TO D-AMT (X3).                   ECS023
01642                                                                   ECS023
01643      GO TO 0830-A-P-L.                                            ECS023
01644                                                                   ECS023
01645  0840-EXIT.                                                       ECS023
01646      EXIT.                                                        ECS023
01647  EJECT                                                            ECS023
01648  0850-LIFE-BEN-LOAD.                                              ECS023
01649      MOVE +0                     TO X3.                           ECS023
01650                                                                   ECS023
01651  0860-L-B-L.                                                      ECS023
01652      ADD +1 TO X3.                                                ECS023
01653      IF X3 GREATER THAN +8                                        ECS023
01654          GO TO 0870-EXIT.                                         ECS023
01655                                                                   ECS023
01656      ADD +1 TO X1.                                                ECS023
01657      IF L-CT (X1) = ZERO                                          ECS023
01658          MOVE L-BEN (X1)         TO D-AMT (X3)                    ECS023
01659          GO TO 0860-L-B-L.                                        ECS023
01660                                                                   ECS023
01661      COMPUTE D-AMT (X3) ROUNDED = L-BEN (X1) / L-CT (X1).         ECS023
01662                                                                   ECS023
01663      GO TO 0860-L-B-L.                                            ECS023
01664                                                                   ECS023
01665  0870-EXIT.                                                       ECS023
01666      EXIT.                                                        ECS023
01667                                                                   ECS023
01668  0880-A-H-BEN-LOAD.                                               ECS023
01669      MOVE +0                     TO X3.                           ECS023
01670                                                                   ECS023
01671  0890-A-B-L.                                                      ECS023
01672      ADD +1 TO X3.                                                ECS023
01673      IF X3 GREATER THAN +8                                        ECS023
01674          GO TO 0900-EXIT.                                         ECS023
01675                                                                   ECS023
01676      ADD +1 TO X1.                                                ECS023
01677      IF A-CT (X1) = ZERO                                          ECS023
01678          MOVE A-BEN (X1) TO D-AMT (X3)                            ECS023
01679          GO TO 0890-A-B-L.                                        ECS023
01680                                                                   ECS023
01681      COMPUTE D-AMT (X3) ROUNDED = A-BEN (X1) / A-CT (X1).         ECS023
01682                                                                   ECS023
01683      GO TO 0890-A-B-L.                                            ECS023
01684                                                                   ECS023
01685  0900-EXIT.                                                       ECS023
01686      EXIT.                                                        ECS023
01687  EJECT                                                            ECS023
01688  0910-PRT-LINE.                                                   ECS023
01689      MOVE P-CTL                  TO X.                            ECS023
01690                                                                   ECS023
01691  0920-PRT-LINE-GO.                                                ECS023
CIDMOD     COPY PRTN023.                                                ECS023
CIDMOD*    COPY ELCPRT2.                                                ECS023
01693                                                                   ECS023
01694  0930-EXIT.                                                       ECS023
01695      EXIT.                                                        ECS023
01696                                                                   ECS023
01697  ABEND-PGM SECTION.                                               ECS023
01698      COPY ELCABEND.                                               ECS023
01699 /                                                                 ECS023
01700  LCP-WRITE-POS-PRT SECTION.                                       ECS023
01701      IF LCP-ASA = '+'                                             ECS023
01702          WRITE PRT AFTER 0 LINE                                   ECS023
01703      ELSE                                                         ECS023
01704      IF LCP-ASA = ' '                                             ECS023
01705          WRITE PRT AFTER ADVANCING 1 LINE                         ECS023
01706      ELSE                                                         ECS023
01707      IF LCP-ASA = '0'                                             ECS023
01708          WRITE PRT AFTER ADVANCING 2 LINE                         ECS023
01709      ELSE                                                         ECS023
01710      IF LCP-ASA = '-'                                             ECS023
01711          WRITE PRT AFTER ADVANCING 3 LINE                         ECS023
01712      ELSE                                                         ECS023
01713      IF LCP-ASA = '1'                                             ECS023
01714          WRITE PRT AFTER ADVANCING PAGE                           ECS023
01715      ELSE                                                         ECS023
01716      IF LCP-ASA = '2'                                             ECS023
01717          WRITE PRT AFTER ADVANCING LCP-CH2                        ECS023
01718      ELSE                                                         ECS023
01719      IF LCP-ASA = '3'                                             ECS023
01720          WRITE PRT AFTER ADVANCING LCP-CH3                        ECS023
01721      ELSE                                                         ECS023
01722      IF LCP-ASA = '4'                                             ECS023
01723          WRITE PRT AFTER ADVANCING LCP-CH4                        ECS023
01724      ELSE                                                         ECS023
01725      IF LCP-ASA = '5'                                             ECS023
01726          WRITE PRT AFTER ADVANCING LCP-CH5                        ECS023
01727      ELSE                                                         ECS023
01728      IF LCP-ASA = '6'                                             ECS023
01729          WRITE PRT AFTER ADVANCING LCP-CH6                        ECS023
01730      ELSE                                                         ECS023
01731      IF LCP-ASA = '7'                                             ECS023
01732          WRITE PRT AFTER ADVANCING LCP-CH7                        ECS023
01733      ELSE                                                         ECS023
01734      IF LCP-ASA = '8'                                             ECS023
01735          WRITE PRT AFTER ADVANCING LCP-CH8                        ECS023
01736      ELSE                                                         ECS023
01737      IF LCP-ASA = '9'                                             ECS023
01738          WRITE PRT AFTER ADVANCING LCP-CH9                        ECS023
01739      ELSE                                                         ECS023
01740      IF LCP-ASA = 'A'                                             ECS023
01741          WRITE PRT AFTER ADVANCING LCP-CH10                       ECS023
01742      ELSE                                                         ECS023
01743      IF LCP-ASA = 'B'                                             ECS023
01744          WRITE PRT AFTER ADVANCING LCP-CH11                       ECS023
01745      ELSE                                                         ECS023
01746      IF LCP-ASA = 'C'                                             ECS023
01747          WRITE PRT AFTER ADVANCING LCP-CH12                       ECS023
01748      ELSE                                                         ECS023
01749      IF LCP-ASA = 'V'                                             ECS023
01750          WRITE PRT AFTER ADVANCING LCP-P01                        ECS023
01751      ELSE                                                         ECS023
01752      IF LCP-ASA = 'W'                                             ECS023
01753          WRITE PRT AFTER ADVANCING LCP-P02                        ECS023
01754      ELSE                                                         ECS023
01755      DISPLAY 'ASA CODE ERROR'.                                    ECS023
01756  LCP-WRITE-END-PRT.                                               ECS023
01757      EXIT.                                                        ECS023
