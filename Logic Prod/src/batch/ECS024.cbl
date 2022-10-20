00001  IDENTIFICATION DIVISION.                                         04/02/98
00002                                                                   ECS024
00003  PROGRAM-ID.                ECS024.                                  LV009
00004 *              PROGRAM CONVERTED BY                               ECS024
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS024
00006 *              CONVERSION DATE 11/26/94 02:57:07.                 ECS024
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            ECS024
00008 *                           VMOD=2.009.                           ECS024
00009                                                                   ECS024
00010 *AUTHOR.     LOGIC, INC.                                          ECS024
00011 *            DALLAS, TEXAS.                                       ECS024
00012                                                                   ECS024
00013 *DATE-COMPILED.                                                   ECS024
00014                                                                   ECS024
00015 *SECURITY.   *****************************************************ECS024
00016 *            *                                                   *ECS024
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS024
00018 *            *                                                   *ECS024
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS024
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS024
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS024
00022 *            *                                                   *ECS024
00023 *            *****************************************************ECS024
00024                                                                   ECS024
00025 *REMARKS.                                                         ECS024
00026                                                                   ECS024
00027 *    ACCOUNT CURRENT SUMMARY AND ABBREVIATED RISK DISTRIBUTION.   ECS024
00028                                                                   ECS024
052814******************************************************************
052814*                   C H A N G E   L O G
052814*
052814* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
052814*-----------------------------------------------------------------
052814*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
052814* EFFECTIVE    NUMBER
052814*-----------------------------------------------------------------
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
052814******************************************************************
00029  ENVIRONMENT DIVISION.                                            ECS024
00030  INPUT-OUTPUT SECTION.                                            ECS024
00031  FILE-CONTROL.                                                    ECS024
00032      SELECT RE-RECAP         ASSIGN TO SYS018-UT-2400-S-SYS018.   ECS024
00033      SELECT AM-MSTR          ASSIGN TO SYS015                     ECS024
00034                              ACCESS IS SEQUENTIAL                 ECS024
00035                              ORGANIZATION IS INDEXED              ECS024
00036                              FILE STATUS IS AM-FILE-STATUS        ECS024
00037                              RECORD KEY IS AM-CONTROL-PRIMARY.    ECS024
00038      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS024
00039      SELECT DISK-DATE        ASSIGN TO SYS019-UT-3380-S-SYS019.   ECS024
00040      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS024
00041                                                                   ECS024
00042  EJECT                                                            ECS024
00043  DATA DIVISION.                                                   ECS024
00044  FILE SECTION.                                                    ECS024
00045                                                                   ECS024
00046  FD  RE-RECAP                                                     ECS024
00047                              COPY ECSEXTFD.                       ECS024
00048                                                                   ECS024
00049  EJECT                                                            ECS024
00050                              COPY ECSEXT01.                       ECS024
00051                                                                   ECS024
00052  EJECT                                                            ECS024
00053  FD  AM-MSTR.                                                     ECS024
00054                             COPY ERCACCT.                         ECS024
00055                                                                   ECS024
00056  EJECT                                                            ECS024
00057  FD  DISK-DATE                                                    ECS024
00058                              COPY ELCDTEFD.                       ECS024
00059                                                                   ECS024
00060  EJECT                                                            ECS024
00061  FD  PRNTR                                                        ECS024
00062                              COPY ELCPRTFD.                       ECS024
00063                                                                   ECS024
00064  01  DTL.                                                         ECS024
00065      12  FILLER      PIC X(133).                                  ECS024
00066                                                                   ECS024
00067  01  RSK-DTL.                                                     ECS024
00068      12  FILLER      PIC X(27).                                   ECS024
00069      12  RD-AGE      PIC X(12).                                   ECS024
00070      12  FILLER      PIC X(7).                                    ECS024
00071      12  RD-AA       PIC Z99.                                     ECS024
00072      12  FILLER      PIC X(12).                                   ECS024
00073      12  RD-LAT      PIC 999.                                     ECS024
00074      12  FILLER      PIC XXXX.                                    ECS024
00075      12  RD-LAA      PIC ZZ,ZZ9.                                  ECS024
00076      12  FILLER      PIC X(11).                                   ECS024
00077      12  RD-AAT      PIC 999.                                     ECS024
00078      12  FILLER      PIC XXXX.                                    ECS024
00079      12  RD-AAA      PIC ZZ,ZZ9.                                  ECS024
00080      12  FILLER      PIC X(35).                                   ECS024
00081                                                                   ECS024
00082                                                                   ECS024
00083  FD  FICH                                                         ECS024
00084                              COPY ELCFCHFD.                       ECS024
00085                                                                   ECS024
00086  EJECT                                                            ECS024
00087  WORKING-STORAGE SECTION.                                         ECS024
00088  77  FILLER  PIC X(32) VALUE '********************************'.  ECS024
00089  77  FILLER  PIC X(32) VALUE '     ECS024 WORKING STORAGE     '.  ECS024
00090  77  FILLER  PIC X(32) VALUE '*****VMOD=2.009*****************'.  ECS024
00091                                                                   ECS024
00092  01  VSAM-ERROR.                                                  ECS024
00093      05  VSAM-CONSTANT        PIC XX VALUE '06'.                  ECS024
00094      05  VSAM-ERR-CODE        PIC XX VALUE '00'.                  ECS024
00095  01  AM-FILE-STATUS           PIC XX VALUE '00'.                  ECS024
00096  01  WS.                                                          ECS024
00097      12  WS-ABEND-MESSAGE    PIC X(80) VALUE ZEROS.               ECS024
00098      12  WS-RETURN-CODE      PIC X(4)  VALUE ZEROS.               ECS024
00099      12  WS-ZERO             PIC S9    VALUE ZEROS.               ECS024
00100      12  WS-ABEND-FILE-STATUS PIC XX   VALUE SPACES.              ECS024
00101      12  ABEND-OPTION        PIC X     VALUE 'Y'.                 ECS024
00102      12  PGM-SUB             PIC S999  VALUE +024  COMP.          ECS024
00103      12  X                   PIC X     VALUE SPACE.               ECS024
00104  01  WS-AM-CO                PIC X(7) VALUE SPACES.               ECS024
00105  EJECT                                                            ECS024
00106                              COPY ELCDTECX.                       ECS024
00107                                                                   ECS024
00108                              COPY ELCDTEVR.                       ECS024
00109                                                                   ECS024
00110  EJECT                                                            ECS024
00111  01  BINARY-WORK-AREA    COMP.                                    ECS024
00112      12  X1          PIC S999        VALUE +0.                    ECS024
00113      12  X2          PIC S999        VALUE +0.                    ECS024
00114      12  X3          PIC S999        VALUE +0.                    ECS024
00115      12  X4          PIC S999        VALUE +0.                    ECS024
00116      12  NDX         PIC S999        VALUE +0.                    ECS024
00117      12  B0          PIC S999        VALUE +0.                    ECS024
00118      12  B1          PIC S999        VALUE +1.                    ECS024
00119      12  B75         PIC S999        VALUE +75.                   ECS024
00120      12  B2          PIC S999        VALUE +2.                    ECS024
00121      12  B3          PIC S999        VALUE +3.                    ECS024
00122      12  B4          PIC S999        VALUE +4.                    ECS024
00123                                                                   ECS024
00124  01  COMP-3-WORK-AREA    COMP-3.                                  ECS024
00125      12  LN-CT       PIC S9(5)       VALUE +99.                   ECS024
00126      12  PG-NO       PIC S9(5)       VALUE +0.                    ECS024
00127      12  K1          PIC S9(5)       VALUE +1.                    ECS024
00128      12  LN-MX       PIC S9(5)       VALUE +56.                   ECS024
00129      12  K-1         PIC S9(5)       VALUE -1.                    ECS024
00130      12  T-LP        PIC S9(7)V99    VALUE +0.                    ECS024
00131      12  T-LC        PIC S9(7)V99    VALUE +0.                    ECS024
00132      12  T-AP        PIC S9(7)V99    VALUE +0.                    ECS024
00133      12  T-AC        PIC S9(7)V99    VALUE +0.                    ECS024
00134      12  T-ALLOW     PIC S9(7)V99    VALUE +0.                    ECS024
00135      12  LCOM        PIC S9(5)V99    VALUE +0.                    ECS024
00136      12  ACOM        PIC S9(5)V99    VALUE +0.                    ECS024
00137      12  L-PREM      PIC S9(7)V99    VALUE +0.                    ECS024
00138      12  A-PREM      PIC S9(7)V99    VALUE +0.                    ECS024
00139      12  ZEV5        PIC SV9(5)      VALUE +.0.                   ECS024
00140      12  NTDUE       PIC S9(7)V99    VALUE +0.                    ECS024
00141      12  TOTPRM      PIC S9(7)V99    VALUE +0.                    ECS024
00142      12  S-LP        PIC S9(7)V99    VALUE +0.                    ECS024
00143      12  S-LC        PIC S9(7)V99    VALUE +0.                    ECS024
00144      12  S-AP        PIC S9(7)V99    VALUE +0.                    ECS024
00145      12  S-AC        PIC S9(7)V99    VALUE +0.                    ECS024
00146      12  S-ND        PIC S9(7)V99    VALUE +0.                    ECS024
00147      12  S-ALLOW     PIC S9(7)V99    VALUE +0.                    ECS024
00148      12  Z9          PIC S9(7)V99    VALUE +0.                    ECS024
00149      12  Z7          PIC S9(7)       VALUE +0.                    ECS024
00150      12  DIST-TOTALS  OCCURS 3.                                   ECS024
00151          16  T-CNT       PIC S9(7).                               ECS024
00152          16  T-AGE       PIC S9(9).                               ECS024
00153          16  T-LT        PIC S9(9).                               ECS024
00154          16  T-LA        PIC S9(7)V99.                            ECS024
00155          16  T-LCNT      PIC S9(7).                               ECS024
00156          16  T-AT        PIC S9(9).                               ECS024
00157          16  T-AA        PIC S9(7)V99.                            ECS024
00158          16  T-ACNT      PIC S9(7).                               ECS024
00159                                                                   ECS024
00160  EJECT                                                            ECS024
00161  01  HD1A.                                                        ECS024
00162      12  FILLER      PIC X(49)   VALUE SPACES.                    ECS024
00163      12  FILLER      PIC X(26)   VALUE                            ECS024
00164                          'ACCOUNT CURRENT FOR MONTH '.            ECS024
00165      12  FILLER      PIC X(49)   VALUE SPACES.                    ECS024
00166      12  FILLER      PIC X(8)    VALUE 'ECS-024 '.                ECS024
00167                                                                   ECS024
00168  01  HD1B.                                                        ECS024
00169      12  FILLER      PIC X(50)   VALUE SPACES.                    ECS024
00170      12  FILLER      PIC X(23)   VALUE                            ECS024
00171                          'ACCOUNT CURRENT SUMMARY'.               ECS024
00172      12  FILLER      PIC X(51)   VALUE SPACES.                    ECS024
00173      12  FILLER      PIC X(8)    VALUE 'ECS-024 '.                ECS024
00174                                                                   ECS024
00175  01  HD2.                                                         ECS024
00176      12  FILLER      PIC X(47)   VALUE SPACES.                    ECS024
00177      12  HD-CMP      PIC X(30).                                   ECS024
00178      12  FILLER      PIC X(47)   VALUE SPACES.                    ECS024
00179      12  HD-IPL      PIC X(8).                                    ECS024
00180                                                                   ECS024
00181  01  HD3.                                                         ECS024
00182      12  FILLER      PIC X(53)  VALUE SPACES.                     ECS024
00183      12  HD-DATE     PIC X(18)  VALUE SPACES.                     ECS024
00184      12  FILLER      PIC X(41)  VALUE SPACES.                     ECS024
00185      12  FILLER      PIC X(5)   VALUE 'PAGE '.                    ECS024
00186      12  HD-PG       PIC ZZ,ZZ9.                                  ECS024
00187                                                                   ECS024
00188  01  HD4.                                                         ECS024
00189      12  FILLER      PIC X(11)  VALUE ' CARRIER - '.              ECS024
00190      12  HDCO        PIC X.                                       ECS024
00191      12  FILLER      PIC X(9)   VALUE ' GROUP - '.                ECS024
00192      12  HDGRP       PIC X(6).                                    ECS024
00193      12  FILLER      PIC X(5)   VALUE SPACES.                     ECS024
00194      12  HDST1       PIC X(8)   VALUE 'STATE - '.                 ECS024
00195      12  HDST2       PIC XX.                                      ECS024
00196      12  FILLER      PIC XX     VALUE SPACES.                     ECS024
00197      12  HDST3       PIC X(20)  VALUE SPACES.                     ECS024
00198                                                                   ECS024
00199  01  HD5-FLC.                                                     ECS024
00200      12  HD5A-FLC    PIC X(39)  VALUE ' ACCOUNT  ACCOUNT NAME'.   ECS024
00201      12  FILLER      PIC X(09)  VALUE SPACE.                      ECS024
00202      12  H5LIFE1-FLC PIC X(6).                                    ECS024
00203      12  FILLER      PIC X(8)   VALUE SPACE.                      ECS024
00204      12  H5LIFE2-FLC PIC X(6).                                    ECS024
00205      12  FILLER      PIC X(11)  VALUE SPACE.                      ECS024
00206      12  H5AH1-FLC   PIC X(6).                                    ECS024
00207      12  FILLER      PIC X(8)   VALUE SPACE.                      ECS024
00208      12  H5AH2-FLC   PIC X(6).                                    ECS024
00209      12  FILLER      PIC X(09)  VALUE SPACE.                      ECS024
00210      12  FILLER      PIC X(10)  VALUE 'NET DUE'.                  ECS024
00211      12  FILLER      PIC X(18)  VALUE '    ACCOUNT'.              ECS024
00212                                                                   ECS024
00213  01  HD6-FLC.                                                     ECS024
00214      12  FILLER      PIC X(48)  VALUE ' NUMBER'.                  ECS024
00215      12  FILLER      PIC X(12)  VALUE 'PREMIUM'.                  ECS024
00216      12  FILLER      PIC X(18)  VALUE ' ALLOWANCE'.               ECS024
00217      12  FILLER      PIC X(12)  VALUE 'PREMIUM'.                  ECS024
00218      12  FILLER      PIC X(18)  VALUE ' ALLOWANCE'.               ECS024
00219      12  FILLER      PIC X(10)  VALUE 'COMPANY'.                  ECS024
00220      12  FILLER      PIC X(18)  VALUE '   ALLOWANCE'.             ECS024
00221                                                                   ECS024
00222  01  HD5.                                                         ECS024
00223      12  HD5A        PIC X(39)  VALUE ' ACCOUNT  ACCOUNT NAME'.   ECS024
00224      12  FILLER      PIC X(11)  VALUE SPACE.                      ECS024
00225      12  H5LIFE1     PIC X(6).                                    ECS024
00226      12  FILLER      PIC X(6)   VALUE SPACE.                      ECS024
00227      12  FILLER      PIC X      VALUE SPACE.                      ECS024
00228      12  H5LIFE2     PIC X(6).                                    ECS024
00229      12  FILLER      PIC X(10)  VALUE SPACE.                      ECS024
00230      12  FILLER      PIC X      VALUE SPACE.                      ECS024
00231      12  H5AH1       PIC X(6).                                    ECS024
00232      12  FILLER      PIC X(6)   VALUE SPACE.                      ECS024
00233      12  FILLER      PIC XX     VALUE SPACE.                      ECS024
00234      12  H5AH2       PIC X(6).                                    ECS024
00235      12  FILLER      PIC X(10)  VALUE SPACE.                      ECS024
00236      12  FILLER      PIC X(10)  VALUE 'NET DUE'.                  ECS024
00237                                                                   ECS024
00238  01  HD6.                                                         ECS024
00239      12  FILLER      PIC X(48)  VALUE ' NUMBER'.                  ECS024
00240      12  FILLER      PIC X(12)  VALUE 'PREMIUM'.                  ECS024
00241      12  FILLER      PIC X(18)  VALUE 'COMMISSION'.               ECS024
00242      12  FILLER      PIC X(12)  VALUE 'PREMIUM'.                  ECS024
00243      12  FILLER      PIC X(20)  VALUE 'COMMISSION'.               ECS024
00244      12  HD-SPECIAL  PIC X(10)  VALUE 'COMPANY'.                  ECS024
00245                                                                   ECS024
00246  01  DTL-OTHER.                                                   ECS024
00247      12  FILLER      PIC X.                                       ECS024
00248      12  P-ACCT      PIC X(10).                                   ECS024
00249      12  FILLER      PIC X.                                       ECS024
00250      12  P-NAME      PIC X(30).                                   ECS024
00251      12  FILLER      PIC XXX.                                     ECS024
00252      12  P-LPRM      PIC Z,ZZZ,ZZZ.99-.                           ECS024
00253      12  FILLER      PIC XX.                                      ECS024
00254      12  P-LCOM      PIC Z,ZZZ,ZZZ.99-.                           ECS024
00255      12  FILLER      PIC XX.                                      ECS024
00256      12  P-APRM      PIC Z,ZZZ,ZZZ.99-.                           ECS024
00257      12  FILLER      PIC XX.                                      ECS024
00258      12  P-ACOM      PIC Z,ZZZ,ZZZ.99-.                           ECS024
00259      12  FILLER      PIC XXXX.                                    ECS024
00260      12  P-NET       PIC Z,ZZZ,ZZZ.99-.                           ECS024
00261      12  FILLER      PIC X(13).                                   ECS024
00262                                                                   ECS024
00263  01  DTL-FLC.                                                     ECS024
00264      12  FILLER      PIC X.                                       ECS024
00265      12  P-ACCT-FLC.                                              ECS024
00266          16  FILLER   PIC X(3).                                   ECS024
00267          16  P-ST-FLC PIC X(2).                                   ECS024
00268          16  FILLER   PIC X(5).                                   ECS024
00269      12  FILLER      PIC X.                                       ECS024
00270      12  P-NAME-FLC  PIC X(30).                                   ECS024
00271      12  FILLER      PIC XX.                                      ECS024
00272      12  P-LPRM-FLC  PIC Z,ZZZ,ZZZ.99-.                           ECS024
00273      12  FILLER      PIC XX.                                      ECS024
00274      12  P-LCOM-FLC  PIC Z,ZZZ,ZZZ.99-.                           ECS024
00275      12  FILLER      PIC XX.                                      ECS024
00276      12  P-APRM-FLC  PIC Z,ZZZ,ZZZ.99-.                           ECS024
00277      12  FILLER      PIC XX.                                      ECS024
00278      12  P-ACOM-FLC  PIC Z,ZZZ,ZZZ.99-.                           ECS024
00279      12  FILLER      PIC XX.                                      ECS024
00280      12  P-NET-FLC   PIC Z,ZZZ,ZZZ.99-.                           ECS024
00281      12  FILLER      PIC XX.                                      ECS024
00282      12  P-ALLOW-FLC PIC Z,ZZZ,ZZZ.99-.                           ECS024
00283                                                                   ECS024
00284  EJECT                                                            ECS024
00285  01  MISC-WORK-AREA.                                              ECS024
00286      12  THIS-CTL.                                                ECS024
00287          16  THS-CO.                                                 CL**8
00288              20 THS-CARR PIC X.                                      CL**8
00289              20 THS-COMP PIC X(6).                                   CL**8
00290          16  THS-ST      PIC XX.                                     CL**8
00291          16  THS-ACCT    PIC X(10).                                  CL**8
00292          16  THS-EFF     PIC 9(11)  COMP-3.                       ECS024
00293      12  LST-CTL     PIC X(18)  VALUE SPACES.                     ECS024
00294      12  LST-NM      PIC X(30).                                   ECS024
00295      12  LST-ACC     PIC X(10).                                   ECS024
00296      12  LST-CO      PIC X(6).                                    ECS024
00297      12  WK-ST.                                                   ECS024
00298          16  W-ST    PIC 99.                                      ECS024
00299      12  WK-AGT.                                                  ECS024
00300          16  FILLER  PIC XX.                                      ECS024
00301          16  WK-Z    PIC XXXX.                                    ECS024
00302      12  SV-TYP      PIC XXX.                                     ECS024
00303      12  THS-CTL     PIC X(6).                                    ECS024
00304      12  CK-CTL      PIC X(6)  VALUE SPACES.                      ECS024
00305                                                                   ECS024
00306  01  WK-DISPLAY-DATE        PIC 9(11).                               CL**3
00307  01  WK-DISPLAY-DATE-RDEF   REDEFINES  WK-DISPLAY-DATE.              CL**3
00308      05  FILLER             PIC 999.                                 CL**3
00309      05  WK-DISPLAY-CCYY    PIC 9999.                                CL**3
00310      05  WK-DISPLAY-MO      PIC 99.                                  CL**3
00311      05  WK-DISPLAY-DA      PIC 99.                                  CL**4
00312                                                                      CL**3
00313  01  STATE-TOTAL-AREA.                                               CL**3
00314      12  ST-TOTALS OCCURS 75.                                     ECS024
00315          16  ST-LP       PIC S9(7)V99    COMP-3.                  ECS024
00316          16  ST-LC       PIC S9(7)V99    COMP-3.                  ECS024
00317          16  ST-AP       PIC S9(7)V99    COMP-3.                  ECS024
00318          16  ST-AC       PIC S9(7)V99    COMP-3.                  ECS024
00319          16  ST-ND       PIC S9(7)V99    COMP-3.                  ECS024
00320          16  ST-ALLOW    PIC S9(7)V99    COMP-3.                  ECS024
00321          16  ST-ID       PIC X.                                   ECS024
00322                                                                   ECS024
00323  01  CARRIER-TOTAL-AREA.                                          ECS024
00324      03 CA-LPRM     PIC S9(7)V99 COMP-3.                          ECS024
00325      03 CA-LCOM     PIC S9(7)V99 COMP-3.                          ECS024
00326      03 CA-APRM     PIC S9(7)V99 COMP-3.                          ECS024
00327      03 CA-ACOM     PIC S9(7)V99 COMP-3.                          ECS024
00328      03 CA-NET      PIC S9(7)V99 COMP-3.                          ECS024
00329      03 CA-ALLOW    PIC S9(7)V99 COMP-3.                          ECS024
00330                                                                   ECS024
00331  01  SV-RDX      PIC X(168).                                      ECS024
00332                                                                   ECS024
00333  01  HD-RD.                                                       ECS024
00334      12  FILLER  PIC X(20)   VALUE SPACES.                        ECS024
00335      12  HD-TYP  PIC X(8)    VALUE SPACES.                        ECS024
00336      12  HD-CTL  PIC X(8)    VALUE SPACES.                        ECS024
00337      12  HD-NME  PIC X(30)   VALUE SPACES.                        ECS024
00338                                                                   ECS024
00339  01  RD1.                                                         ECS024
00340      12  FILLER  PIC X(46)   VALUE SPACES.                        ECS024
00341      12  FILLER  PIC X(31)   VALUE 'AVE.    LIFE  AVE.     AVE.'. ECS024
00342      12  FILLER  PIC X(30)   VALUE 'A&H   AVE.     AVE'.          ECS024
00343                                                                   ECS024
00344  01  RD2.                                                         ECS024
00345      12  FILLER  PIC X(46)   VALUE SPACES.                        ECS024
00346      12  FILLER  PIC X(31)   VALUE 'AGE           TERM   AMOUNT'. ECS024
00347      12  FILLER  PIC X(30)   VALUE '      TERM   BENEFIT'.        ECS024
00348                                                                   ECS024
00349  01  RDX-CALC-AREA.                                               ECS024
00350      12  XDX-ID      PIC XXX.                                     ECS024
00351      12  XDX-CTL     PIC X(6).                                    ECS024
00352      12  FILLER      PIC X(18).                                   ECS024
00353      12  XDX-AMTS   COMP-3       OCCURS 3.                        ECS024
00354          16  XDX-CNT     PIC S9(7).                               ECS024
00355          16  XDX-AGE     PIC S9(9).                               ECS024
00356          16  XDX-LT      PIC S9(9).                               ECS024
00357          16  XDX-LA      PIC S9(7)V99.                            ECS024
00358          16  XDX-LCNT    PIC S9(7).                               ECS024
00359          16  XDX-AT      PIC S9(9).                               ECS024
00360          16  XDX-AA      PIC S9(7)V99.                            ECS024
00361          16  XDX-ACNT    PIC S9(7).                               ECS024
00362                                                                   ECS024
00363  EJECT                                                            ECS024
00364  PROCEDURE DIVISION.                                              ECS024
00365  0100-SET-START.                                                  ECS024
00366                              COPY ELCDTERX.                       ECS024
00367                                                                   ECS024
00368      MOVE COMPANY-NAME           TO HD-CMP.                       ECS024
00369      MOVE ALPH-DATE              TO HD-DATE.                      ECS024
00370      MOVE WS-CURRENT-DATE        TO HD-IPL.                       ECS024
00371                                                                   ECS024
00372      OPEN INPUT RE-RECAP                                          ECS024
00373                 AM-MSTR                                           ECS024
00374           OUTPUT PRNTR.                                           ECS024
00375                                                                   ECS024
00376      IF AM-FILE-STATUS  = '00' OR '97'                            ECS024
00377          NEXT SENTENCE                                            ECS024
00378        ELSE                                                       ECS024
00379          MOVE AM-FILE-STATUS     TO WS-ABEND-FILE-STATUS          ECS024
00380          MOVE AM-FILE-STATUS     TO VSAM-ERR-CODE                 ECS024
00381          MOVE VSAM-ERROR         TO WS-RETURN-CODE                ECS024
00382          MOVE 'ERACCT OPEN ERROR ' TO WS-ABEND-MESSAGE            ECS024
00383          GO TO ABEND-PGM.                                         ECS024
00384                                                                   ECS024
00385      MOVE SPACES                 TO STATE-TOTAL-AREA.             ECS024
00386                                                                   ECS024
00387      IF DTE-CLIENT = 'LAP'                                        ECS024
00388         MOVE 'CARRIER'           TO HD-SPECIAL.                   ECS024
00389                                                                   ECS024
00390  0110-ZERO-CARR.                                                  ECS024
00391      MOVE ZEROS                  TO CA-LPRM                       ECS024
00392                                     CA-LCOM                       ECS024
00393                                     CA-APRM                       ECS024
00394                                     CA-ACOM                       ECS024
00395                                     CA-ALLOW                      ECS024
00396                                     CA-NET.                       ECS024
00397                                                                   ECS024
00398  0120-ZERO-DIST.                                                  ECS024
00399      MOVE ZEROS                  TO T-CNT (1)                     ECS024
00400                                     T-AGE (1)                     ECS024
00401                                     T-LT  (1)                     ECS024
00402                                     T-LA  (1)                     ECS024
00403                                     T-LCNT (1)                    ECS024
00404                                     T-AT  (1)                     ECS024
00405                                     T-AA  (1)                     ECS024
00406                                     T-ACNT (1).                   ECS024
00407                                                                   ECS024
00408      MOVE DIST-TOTALS (1)        TO DIST-TOTALS (2)               ECS024
00409                                     DIST-TOTALS (3).              ECS024
00410                                                                   ECS024
00411  EJECT                                                            ECS024
00412  0140-GET-ACCT.                                                   ECS024
00413      READ AM-MSTR.                                                ECS024
00414                                                                   ECS024
00415      IF AM-FILE-STATUS = '00' OR '10'                             ECS024
00416          NEXT SENTENCE                                            ECS024
00417      ELSE                                                         ECS024
00418          MOVE AM-FILE-STATUS     TO WS-ABEND-FILE-STATUS          ECS024
00419          MOVE AM-FILE-STATUS     TO VSAM-ERR-CODE                 ECS024
00420          MOVE VSAM-ERROR         TO WS-RETURN-CODE                ECS024
00421          MOVE 'VSAM SEQUENCE ERROR ' TO WS-ABEND-MESSAGE          ECS024
00422          GO TO ABEND-PGM.                                         ECS024
00423                                                                   ECS024
00424      IF AM-FILE-STATUS = '10'                                     ECS024
00425          MOVE HIGH-VALUES        TO ACCOUNT-MASTER.               ECS024
00426                                                                   ECS024
00427  0150-EXIT.                                                       ECS024
00428       EXIT.                                                       ECS024
00429                                                                   ECS024
00430  EJECT                                                            ECS024
00431  0160-GET-DTL.                                                    ECS024
00432      READ RE-RECAP AT END                                         ECS024
00433          GO TO 0730-END-RTN.                                      ECS024
00434                                                                   ECS024
00435      IF DE-ENTRY-STATUS = 'D' OR 'V'                              ECS024
00436          GO TO 0160-GET-DTL.                                      ECS024
00437                                                                   ECS024
00438      IF DE-RECORD-ID NOT = 'DE'                                   ECS024
00439          GO TO 0160-GET-DTL.                                      ECS024
00440                                                                   ECS024
00441      IF DE-TRANS NOT = 'I' AND                                    ECS024
00442         DE-TRANS NOT = 'C'                                        ECS024
00443           GO TO 0160-GET-DTL.                                     ECS024
00444                                                                   ECS024
00445      IF DE-ISSUE                                                  ECS024
00446         IF DE-LF-STAT-CDE = '5' OR '9'                            ECS024
00447            GO TO 0160-GET-DTL.                                    ECS024
00448                                                                   ECS024
00449      IF DE-ISSUE                                                  ECS024
00450         IF DE-AH-STAT-CDE = '5' OR '9'                            ECS024
00451            GO TO 0160-GET-DTL.                                    ECS024
00452                                                                   ECS024
00453      IF DE-REIN NOT = SPACES                                      ECS024
00454         GO TO 0160-GET-DTL.                                       ECS024
00455                                                                   ECS024
00456      MOVE DE-CARRIER             TO THS-CARR.                     ECS024
00457      MOVE DE-GROUPING            TO THS-COMP.                     ECS024
00458      MOVE DE-STATE               TO THS-ST.                       ECS024
00459      MOVE DE-ACCOUNT             TO THS-ACCT.                     ECS024
00460      MOVE DE-EFF                 TO THS-EFF.                      ECS024
00461                                                                   ECS024
00462  EJECT                                                            ECS024
00463  0170-MAIN-CHECK.                                                 ECS024
00464      IF LST-CTL = SPACES                                          ECS024
00465          MOVE THIS-CTL           TO LST-CTL.                      ECS024
00466                                                                   ECS024
00467      STRING AM-CARRIER DELIMITED BY SIZE                          ECS024
00468             AM-GROUPING DELIMITED BY SIZE                         ECS024
00469      INTO WS-AM-CO.                                               ECS024
00470                                                                   ECS024
00471      IF THS-CO   = WS-AM-CO AND                                   ECS024
00472         THS-ST   = AM-STATE AND                                   ECS024
00473         THS-ACCT = AM-ACCOUNT AND                                 ECS024
00474         THS-EFF LESS AM-EXPIRE-DT                                 ECS024
00475           GO TO 0190-ML1.                                         ECS024
00476                                                                   ECS024
00477      IF (THIS-CTL GREATER AM-MSTR-CNTRL) OR                          CL**9
00478         (THIS-CTL = AM-MSTR-CNTRL)                                   CL**9
00479          PERFORM 0140-GET-ACCT THRU 0150-EXIT                     ECS024
00480          GO TO 0170-MAIN-CHECK.                                   ECS024
00481                                                                   ECS024
00482      MOVE THS-EFF                TO  WK-DISPLAY-DATE.                CL**5
00483      DISPLAY 'ACCOUNT MISSING- ' 'CARR ' THS-CARR ' GROUP '          CL**3
00484              THS-COMP ' ST ' THS-ST ' ACCT ' THS-ACCT ' EFF DATE '   CL**3
00485              WK-DISPLAY-CCYY WK-DISPLAY-MO WK-DISPLAY-DA.            CL**3
00486                                                                      CL**3
00487      MOVE '0302'                 TO WS-RETURN-CODE.               ECS024
00488      GO TO ABEND-PGM.                                             ECS024
00489                                                                   ECS024
00490  EJECT                                                            ECS024
00491  0190-ML1.                                                        ECS024
00492      MOVE AM-NAME                TO LST-NM.                       ECS024
00493      MOVE AM-ACCOUNT             TO LST-ACC.                      ECS024
00494      MOVE DE-LF-PRM              TO L-PREM.                       ECS024
00495      ADD DE-LF-PRM-ALT           TO L-PREM.                       ECS024
00496      MOVE DE-AH-PRM              TO A-PREM.                       ECS024
00497                                                                   ECS024
00498      IF DE-CANCEL  COMPUTE L-PREM = DE-LF-RFND * K-1              ECS024
00499                    COMPUTE A-PREM = DE-AH-RFND * K-1.             ECS024
00500                                                                   ECS024
00501      ADD L-PREM                  TO T-LP.                         ECS024
00502      ADD A-PREM                  TO T-AP.                         ECS024
00503                                                                   ECS024
00504 *    COMPUTE LCOM ROUNDED = L-PREM * DE-L-PC (1).                 ECS024
00505 *    COMPUTE ACOM ROUNDED = A-PREM * DE-A-PC (1).                 ECS024
00506 *    ADD LCOM                    TO T-LC.                         ECS024
00507 *    ADD ACOM                    TO T-AC.                         ECS024
00508      MOVE  +0                    TO  X1.                          ECS024
00509                                                                   ECS024
00510  EJECT                                                            ECS024
00511  0200-AGENT-LOOP.                                                 ECS024
00512      ADD  +1                     TO  X1.                          ECS024
00513                                                                   ECS024
00514      IF X1 GREATER +10                                            ECS024
00515          GO  TO  0210-AGENT-X.                                    ECS024
00516                                                                   ECS024
00517      IF DE-AGT-TYPE (X1) = 'W' OR 'R' OR 'T'                      ECS024
00518         GO TO 0200-AGENT-LOOP.                                    ECS024
00519                                                                   ECS024
00520      IF DTE-PGM-OPT EQUAL '2'                                     ECS024
052814        IF DE-AGT-TYPE (X1) EQUAL 'O' OR 'P' or 'S'
00522            GO TO 0200-AGENT-LOOP.                                 ECS024
00523                                                                   ECS024
00524      IF DE-AGT (X1) NOT = ZEROS  AND NOT = SPACES                 ECS024
00525          COMPUTE LCOM ROUNDED = L-PREM  *  DE-L-PC (X1)           ECS024
00526          COMPUTE ACOM ROUNDED = A-PREM  *  DE-A-PC (X1)           ECS024
00527          ADD LCOM                TO T-LC                          ECS024
00528          ADD ACOM                TO T-AC.                         ECS024
00529                                                                   ECS024
00530      GO TO  0200-AGENT-LOOP.                                      ECS024
00531                                                                   ECS024
00532  0210-AGENT-X.                                                    ECS024
00533      PERFORM 0160-GET-DTL.                                        ECS024
00534                                                                   ECS024
00535      IF THIS-CTL LESS AM-CNTRL-1                                  ECS024
00536          GO TO 0190-ML1.                                          ECS024
00537                                                                   ECS024
00538      IF THS-CO   = WS-AM-CO AND                                   ECS024
00539         THS-ST   = AM-STATE AND                                   ECS024
00540         THS-ACCT = AM-ACCOUNT                                     ECS024
00541            GO TO 0170-MAIN-CHECK                                  ECS024
00542          ELSE                                                     ECS024
00543            GO TO 0230-MAIN-BREAK.                                 ECS024
00544                                                                   ECS024
00545  EJECT                                                            ECS024
00546  0230-MAIN-BREAK.                                                 ECS024
00547      IF LN-CT LESS LN-MX                                          ECS024
00548          GO TO 0250-PT-ACCT.                                      ECS024
00549                                                                   ECS024
00550  0240-PT-HDNG.                                                    ECS024
00551      MOVE HD1A                   TO P-DATA.                       ECS024
00552      MOVE '1'                    TO X.                            ECS024
00553      PERFORM 0460-PRT-LINE THRU 0470-EXIT.                        ECS024
00554      MOVE HD2                    TO P-DATA.                       ECS024
00555      MOVE ' '                    TO X.                            ECS024
00556      PERFORM 0460-PRT-LINE THRU 0470-EXIT.                        ECS024
00557      ADD K1                      TO PG-NO.                        ECS024
00558      MOVE PG-NO                  TO HD-PG.                        ECS024
00559      MOVE HD3                    TO P-DATA.                       ECS024
00560      MOVE ' '                    TO X.                            ECS024
00561      PERFORM 0460-PRT-LINE THRU 0470-EXIT.                        ECS024
00562      MOVE AM-CARRIER             TO HDCO.                         ECS024
00563      MOVE AM-GROUPING            TO HDGRP.                        ECS024
00564      MOVE AM-STATE               TO HDST2                         ECS024
00565                                     WK-ST.                        ECS024
00566      PERFORM 0690-LOCATE-STATE THRU 0710-EXIT.                    ECS024
00567      MOVE STATE-PIC (W-ST)       TO HDST3.                        ECS024
00568      MOVE HD4                    TO P-DATA.                       ECS024
00569      MOVE '0'                    TO X.                            ECS024
00570      PERFORM 0460-PRT-LINE THRU 0470-EXIT.                        ECS024
00571      MOVE AH-OVERRIDE-L6         TO H5AH1                         ECS024
00572                                     H5AH2                         ECS024
00573                                     H5AH1-FLC                     ECS024
00574                                     H5AH2-FLC.                    ECS024
00575      MOVE LIFE-OVERRIDE-L6       TO H5LIFE1                       ECS024
00576                                     H5LIFE2                       ECS024
00577                                     H5LIFE1-FLC                   ECS024
00578                                     H5LIFE2-FLC.                  ECS024
00579                                                                   ECS024
00580      IF DTE-CLIENT = 'FLC'                                        ECS024
00581          MOVE HD5-FLC            TO P-DATA                        ECS024
00582          MOVE '0'                TO X                             ECS024
00583          PERFORM 0460-PRT-LINE   THRU 0470-EXIT                   ECS024
00584          MOVE HD6-FLC            TO P-DATA                        ECS024
00585          MOVE ' '                TO X                             ECS024
00586          PERFORM 0460-PRT-LINE   THRU 0470-EXIT                   ECS024
00587      ELSE                                                         ECS024
00588          MOVE HD5                TO P-DATA                        ECS024
00589          MOVE '0'                TO X                             ECS024
00590          PERFORM 0460-PRT-LINE   THRU 0470-EXIT                   ECS024
00591          MOVE HD6                TO P-DATA                        ECS024
00592          MOVE ' '                TO X                             ECS024
00593          PERFORM 0460-PRT-LINE   THRU 0470-EXIT.                  ECS024
00594                                                                   ECS024
00595      MOVE SPACES                 TO P-DATA.                       ECS024
00596      MOVE ' '                    TO X.                            ECS024
00597      PERFORM 0460-PRT-LINE THRU 0470-EXIT.                        ECS024
00598      MOVE 9                      TO LN-CT.                        ECS024
00599                                                                   ECS024
00600  EJECT                                                            ECS024
00601  0250-PT-ACCT.                                                    ECS024
00602                                                                   ECS024
00603      MOVE SPACES                 TO DTL                           ECS024
00604                                     DTL-FLC                       ECS024
00605                                     DTL-OTHER.                    ECS024
00606      IF DTE-CLIENT EQUAL 'FLC'                                    ECS024
00607          MOVE LST-ACC            TO P-ACCT-FLC                    ECS024
00608          MOVE LST-NM             TO P-NAME-FLC                    ECS024
00609          MOVE T-LP               TO P-LPRM-FLC                    ECS024
00610          MOVE T-LC               TO P-LCOM-FLC                    ECS024
00611          MOVE T-AP               TO P-APRM-FLC                    ECS024
00612          MOVE T-AC               TO P-ACOM-FLC                    ECS024
00613          COMPUTE TOTPRM = T-LP + T-AP                             ECS024
00614          COMPUTE NTDUE = TOTPRM - T-AC - T-LC                     ECS024
00615          MOVE NTDUE              TO P-NET-FLC                     ECS024
00616          MOVE +1                 TO NDX                           ECS024
00617          PERFORM 0500-FLC-ACCOUNT-SEARCH                          ECS024
00618                                  THRU 0509-EXIT                   ECS024
00619          MOVE T-ALLOW            TO P-ALLOW-FLC                   ECS024
00620          MOVE DTL-FLC            TO DTL                           ECS024
00621          MOVE ' '                TO X                             ECS024
00622          PERFORM 0460-PRT-LINE   THRU 0470-EXIT                   ECS024
00623      ELSE                                                         ECS024
00624          MOVE LST-ACC            TO P-ACCT                        ECS024
00625          MOVE LST-NM             TO P-NAME                        ECS024
00626          MOVE T-LP               TO P-LPRM                        ECS024
00627          MOVE T-LC               TO P-LCOM                        ECS024
00628          MOVE T-AP               TO P-APRM                        ECS024
00629          MOVE T-AC               TO P-ACOM                        ECS024
00630          COMPUTE NTDUE = T-LP + T-AP - T-AC - T-LC                ECS024
00631          MOVE NTDUE              TO P-NET                         ECS024
00632          MOVE DTL-OTHER          TO DTL                           ECS024
00633          MOVE ' '                TO X                             ECS024
00634          PERFORM 0460-PRT-LINE   THRU 0470-EXIT.                  ECS024
00635                                                                   ECS024
00636      ADD K1                      TO LN-CT.                        ECS024
00637                                                                   ECS024
00638  0260-M-R-3.                                                      ECS024
00639                                                                   ECS024
00640      PERFORM 0120-ZERO-DIST.                                      ECS024
00641      ADD T-LP                    TO S-LP.                         ECS024
00642      ADD T-LC                    TO S-LC.                         ECS024
00643      ADD T-AP                    TO S-AP.                         ECS024
00644      ADD T-AC                    TO S-AC.                         ECS024
00645      ADD NTDUE                   TO S-ND.                         ECS024
00646      IF DTE-CLIENT EQUAL 'FLC'                                    ECS024
00647          ADD T-ALLOW             TO S-ALLOW                       ECS024
00648      ELSE                                                         ECS024
00649          MOVE ZEROS              TO S-ALLOW.                      ECS024
00650                                                                   ECS024
00651      MOVE Z9                     TO T-LP                          ECS024
00652                                     T-LC                          ECS024
00653                                     T-AP                          ECS024
00654                                     T-AC                          ECS024
00655                                     T-ALLOW                       ECS024
00656                                     NTDUE.                        ECS024
00657                                                                   ECS024
00658  0270-EXIT.                                                       ECS024
00659      EXIT.                                                        ECS024
00660                                                                   ECS024
00661  0280-M-B-XX.                                                     ECS024
00662      IF THS-CO = WS-AM-CO AND                                     ECS024
00663         THS-ST = AM-STATE                                         ECS024
00664            GO TO 0170-MAIN-CHECK                                  ECS024
00665          ELSE                                                     ECS024
00666            GO TO 0300-STATE-BREAK.                                ECS024
00667                                                                   ECS024
00668  EJECT                                                            ECS024
00669  0300-STATE-BREAK.                                                ECS024
00670      MOVE SPACES                 TO DTL                           ECS024
00671                                     DTL-OTHER                     ECS024
00672                                     DTL-FLC.                      ECS024
00673      IF DTE-CLIENT EQUAL 'FLC'                                    ECS024
00674          MOVE 'STATE TOTAL'      TO P-NAME-FLC                    ECS024
00675          MOVE S-LP               TO P-LPRM-FLC                    ECS024
00676          MOVE S-LC               TO P-LCOM-FLC                    ECS024
00677          MOVE S-AP               TO P-APRM-FLC                    ECS024
00678          MOVE S-AC               TO P-ACOM-FLC                    ECS024
00679          MOVE S-ND               TO P-NET-FLC                     ECS024
00680          MOVE S-ALLOW            TO P-ALLOW-FLC                   ECS024
00681          MOVE DTL-FLC            TO DTL                           ECS024
00682          MOVE '0'                TO X                             ECS024
00683          PERFORM 0460-PRT-LINE THRU 0470-EXIT                     ECS024
00684      ELSE                                                         ECS024
00685          MOVE 'STATE TOTAL'      TO P-NAME                        ECS024
00686          MOVE S-LP               TO P-LPRM                        ECS024
00687          MOVE S-LC               TO P-LCOM                        ECS024
00688          MOVE S-AP               TO P-APRM                        ECS024
00689          MOVE S-AC               TO P-ACOM                        ECS024
00690          MOVE S-ND               TO P-NET                         ECS024
00691          MOVE DTL-OTHER          TO DTL                           ECS024
00692          MOVE '0'                TO X                             ECS024
00693          PERFORM 0460-PRT-LINE THRU 0470-EXIT.                    ECS024
00694      MOVE +99                    TO LN-CT.                        ECS024
00695      MOVE SPACES                 TO LST-CTL.                      ECS024
00696      MOVE S-LP                   TO ST-LP (75).                   ECS024
00697      MOVE S-AP                   TO ST-AP (75).                   ECS024
00698      MOVE S-LC                   TO ST-LC (75).                   ECS024
00699      MOVE S-AC                   TO ST-AC (75).                   ECS024
00700      MOVE S-ND                   TO ST-ND (75).                   ECS024
00701      IF DTE-CLIENT EQUAL 'FLC'                                    ECS024
00702         MOVE S-ALLOW             TO ST-ALLOW (75)                 ECS024
00703      ELSE                                                         ECS024
00704         MOVE ZEROS               TO ST-ALLOW (75).                ECS024
00705                                                                   ECS024
00706      MOVE ST-TOTALS (75)         TO ST-TOTALS (W-ST).             ECS024
00707      MOVE '*'                    TO ST-ID (W-ST).                 ECS024
00708      MOVE Z9                     TO S-LP                          ECS024
00709                                     S-LC                          ECS024
00710                                     S-AP                          ECS024
00711                                     S-AC                          ECS024
00712                                     S-ND                          ECS024
00713                                     S-ALLOW.                      ECS024
00714                                                                   ECS024
00715  0310-EXIT.                                                       ECS024
00716      EXIT.                                                        ECS024
00717                                                                   ECS024
00718  0320-S-B-XX.                                                     ECS024
00719      IF THS-CO = WS-AM-CO                                         ECS024
00720          GO TO 0170-MAIN-CHECK                                    ECS024
00721         ELSE                                                      ECS024
00722          GO TO 0340-COMPANY-BREAK.                                ECS024
00723                                                                   ECS024
00724  EJECT                                                            ECS024
00725  0340-COMPANY-BREAK.                                              ECS024
00726      MOVE HD1B                   TO P-DATA.                       ECS024
00727      MOVE '1'                    TO X.                            ECS024
00728      PERFORM 0460-PRT-LINE THRU 0470-EXIT.                        ECS024
00729      MOVE HD2                    TO P-DATA.                       ECS024
00730      MOVE ' '                    TO X.                            ECS024
00731      PERFORM 0460-PRT-LINE THRU 0470-EXIT.                        ECS024
00732      ADD K1                      TO PG-NO.                        ECS024
00733      MOVE PG-NO                  TO HD-PG.                        ECS024
00734      MOVE HD3                    TO P-DATA.                       ECS024
00735      MOVE ' '                    TO X.                            ECS024
00736      PERFORM 0460-PRT-LINE THRU 0470-EXIT.                        ECS024
00737      MOVE SPACES                 TO HDST1                         ECS024
00738                                     HDST2                         ECS024
00739                                     HDST3.                        ECS024
00740      MOVE HD4                    TO P-DATA.                       ECS024
00741      MOVE '0'                    TO X.                            ECS024
00742      PERFORM 0460-PRT-LINE THRU 0470-EXIT.                        ECS024
00743      MOVE 'STATE - '             TO HDST1.                        ECS024
00744      MOVE '  STATE   STATE NAME' TO HD5A-FLC                      ECS024
00745                                     HD5A.                         ECS024
00746      MOVE AH-OVERRIDE-L6         TO H5AH1                         ECS024
00747                                     H5AH2                         ECS024
00748                                     H5AH1-FLC                     ECS024
00749                                     H5AH2-FLC.                    ECS024
00750      MOVE LIFE-OVERRIDE-L6       TO H5LIFE1                       ECS024
00751                                     H5LIFE2                       ECS024
00752                                     H5LIFE1-FLC                   ECS024
00753                                     H5LIFE2-FLC.                  ECS024
00754                                                                   ECS024
00755      IF DTE-CLIENT = 'FLC'                                        ECS024
00756          MOVE HD5-FLC            TO P-DATA                        ECS024
00757          MOVE '0'                TO X                             ECS024
00758          PERFORM 0460-PRT-LINE   THRU 0470-EXIT                   ECS024
00759          MOVE HD6-FLC            TO P-DATA                        ECS024
00760          MOVE ' '                TO X                             ECS024
00761          PERFORM 0460-PRT-LINE   THRU 0470-EXIT                   ECS024
00762      ELSE                                                         ECS024
00763          MOVE HD5                TO P-DATA                        ECS024
00764          MOVE '0'                TO X                             ECS024
00765          PERFORM 0460-PRT-LINE   THRU 0470-EXIT                   ECS024
00766          MOVE HD6                TO P-DATA                        ECS024
00767          MOVE ' '                TO X                             ECS024
00768          PERFORM 0460-PRT-LINE   THRU 0470-EXIT.                  ECS024
00769                                                                   ECS024
00770      MOVE ' ACCOUNT  ACCOUNT NAME' TO HD5A-FLC                    ECS024
00771                                       HD5A.                       ECS024
00772      MOVE SPACES                 TO P-DATA.                       ECS024
00773      MOVE ' '                    TO X.                            ECS024
00774      PERFORM 0460-PRT-LINE THRU 0470-EXIT.                        ECS024
00775      MOVE B1                     TO X1.                           ECS024
00776                                                                   ECS024
00777  0350-STATE-LOOP.                                                 ECS024
00778      IF ST-ID (X1) = '*'                                          ECS024
00779          GO TO 0380-ST-PRNT.                                      ECS024
00780                                                                   ECS024
00781  0360-ST-XIT.                                                     ECS024
00782      ADD B1                      TO X1.                           ECS024
00783      IF X1 NOT = B75                                              ECS024
00784          GO TO 0350-STATE-LOOP.                                   ECS024
00785      GO TO 0390-STATE-TOTALS.                                     ECS024
00786                                                                   ECS024
00787  EJECT                                                            ECS024
00788  0380-ST-PRNT.                                                    ECS024
00789      MOVE SPACES                TO DTL                            ECS024
00790                                    DTL-OTHER                      ECS024
00791                                    DTL-FLC.                       ECS024
00792                                                                   ECS024
00793      IF DTE-CLIENT EQUAL 'FLC'                                    ECS024
00794          MOVE ST-TOTALS (X1)    TO ST-TOTALS (75)                 ECS024
00795          MOVE X1                TO W-ST                           ECS024
00796          MOVE STATE-SUB (X1)    TO P-ST-FLC                       ECS024
00797          MOVE STATE-PIC (X1)    TO P-NAME-FLC                     ECS024
00798          MOVE ST-LP (75)        TO P-LPRM-FLC                     ECS024
00799          MOVE ST-LC (75)        TO P-LCOM-FLC                     ECS024
00800          MOVE ST-AP (75)        TO P-APRM-FLC                     ECS024
00801          MOVE ST-AC (75)        TO P-ACOM-FLC                     ECS024
00802          MOVE ST-ND (75)        TO P-NET-FLC                      ECS024
00803          MOVE ST-ALLOW (75)     TO P-ALLOW-FLC                    ECS024
00804          MOVE DTL-FLC           TO DTL                            ECS024
00805          MOVE ' '               TO X                              ECS024
00806          PERFORM 0460-PRT-LINE  THRU 0470-EXIT                    ECS024
00807      ELSE                                                         ECS024
00808          MOVE ST-TOTALS (X1)    TO ST-TOTALS (75)                 ECS024
00809          MOVE X1                TO W-ST                           ECS024
00810          MOVE STATE-SUB (X1)    TO P-ACCT                         ECS024
00811          MOVE STATE-PIC (X1)    TO P-NAME                         ECS024
00812          MOVE ST-LP (75)        TO P-LPRM                         ECS024
00813          MOVE ST-LC (75)        TO P-LCOM                         ECS024
00814          MOVE ST-AP (75)        TO P-APRM                         ECS024
00815          MOVE ST-AC (75)        TO P-ACOM                         ECS024
00816          MOVE ST-ND (75)        TO P-NET                          ECS024
00817          MOVE DTL-OTHER         TO DTL                            ECS024
00818          MOVE ' '               TO X                              ECS024
00819          PERFORM 0460-PRT-LINE  THRU 0470-EXIT.                   ECS024
00820      ADD ST-LP (75)             TO S-LP.                          ECS024
00821      ADD ST-LC (75)             TO S-LC.                          ECS024
00822      ADD ST-AP (75)             TO S-AP.                          ECS024
00823      ADD ST-AC (75)             TO S-AC.                          ECS024
00824      ADD ST-ND (75)             TO S-ND.                          ECS024
00825      IF DTE-CLIENT EQUAL 'FLC'                                    ECS024
00826          ADD ST-ALLOW (75)      TO S-ALLOW                        ECS024
00827      ELSE                                                         ECS024
00828          MOVE ZEROS             TO S-ALLOW.                       ECS024
00829                                                                   ECS024
00830      GO TO 0360-ST-XIT.                                           ECS024
00831                                                                   ECS024
00832  EJECT                                                            ECS024
00833  0390-STATE-TOTALS.                                               ECS024
00834      MOVE SPACES                 TO DTL                           ECS024
00835                                    DTL-FLC                        ECS024
00836                                    DTL-OTHER.                     ECS024
00837      IF DTE-CLIENT EQUAL 'FLC'                                    ECS024
00838         MOVE 'GROUP TOTALS'     TO P-NAME-FLC                     ECS024
00839         MOVE S-LP               TO P-LPRM-FLC                     ECS024
00840         MOVE S-LC               TO P-LCOM-FLC                     ECS024
00841         MOVE S-AP               TO P-APRM-FLC                     ECS024
00842         MOVE S-AC               TO P-ACOM-FLC                     ECS024
00843         MOVE S-ND               TO P-NET-FLC                      ECS024
00844         MOVE S-ALLOW            TO P-ALLOW-FLC                    ECS024
00845         MOVE DTL-FLC            TO DTL                            ECS024
00846         MOVE '0'                TO X                              ECS024
00847         PERFORM 0460-PRT-LINE   THRU 0470-EXIT                    ECS024
00848      ELSE                                                         ECS024
00849         MOVE 'GROUP TOTALS'     TO P-NAME                         ECS024
00850         MOVE S-LP               TO P-LPRM                         ECS024
00851         MOVE S-LC               TO P-LCOM                         ECS024
00852         MOVE S-AP               TO P-APRM                         ECS024
00853         MOVE S-AC               TO P-ACOM                         ECS024
00854         MOVE S-ND               TO P-NET                          ECS024
00855         MOVE DTL-OTHER          TO DTL                            ECS024
00856         MOVE '0'                TO X                              ECS024
00857         PERFORM 0460-PRT-LINE   THRU 0470-EXIT.                   ECS024
00858      ADD S-LP                   TO CA-LPRM.                       ECS024
00859      ADD S-LC                   TO CA-LCOM.                       ECS024
00860      ADD S-AP                   TO CA-APRM.                       ECS024
00861      ADD S-AC                   TO CA-ACOM.                       ECS024
00862      ADD S-ND                   TO CA-NET.                        ECS024
00863      IF DTE-CLIENT EQUAL 'FLC'                                    ECS024
00864         ADD  S-ALLOW            TO CA-ALLOW                       ECS024
00865      ELSE                                                         ECS024
00866         MOVE ZEROS              TO CA-ALLOW.                      ECS024
00867                                                                   ECS024
00868      MOVE SPACES                TO STATE-TOTAL-AREA.              ECS024
00869      MOVE Z9                    TO S-LP                           ECS024
00870                                    S-LC                           ECS024
00871                                    S-AP                           ECS024
00872                                    S-AC                           ECS024
00873                                    S-ND                           ECS024
00874                                    S-ALLOW.                       ECS024
00875                                                                   ECS024
00876  0400-EXIT.                                                       ECS024
00877      EXIT.                                                        ECS024
00878                                                                   ECS024
00879  0410-CARR-XX.                                                    ECS024
00880      IF THS-CARR = AM-CARRIER                                     ECS024
00881          GO TO 0170-MAIN-CHECK                                    ECS024
00882         ELSE                                                      ECS024
00883          GO TO 0430-CARRIER-TOTALS.                               ECS024
00884                                                                   ECS024
00885  EJECT                                                            ECS024
00886  0430-CARRIER-TOTALS.                                             ECS024
00887      MOVE SPACES                  TO DTL                          ECS024
00888                                      DTL-OTHER                    ECS024
00889                                      DTL-FLC.                     ECS024
00890      IF DTE-CLIENT EQUAL 'FLC'                                    ECS024
00891          MOVE 'CARRIER TOTALS'    TO P-NAME-FLC                   ECS024
00892          MOVE CA-LPRM             TO P-LPRM-FLC                   ECS024
00893          MOVE CA-LCOM             TO P-LCOM-FLC                   ECS024
00894          MOVE CA-APRM             TO P-APRM-FLC                   ECS024
00895          MOVE CA-ACOM             TO P-ACOM-FLC                   ECS024
00896          MOVE CA-NET              TO P-NET-FLC                    ECS024
00897          MOVE CA-ALLOW            TO P-ALLOW-FLC                  ECS024
00898          MOVE DTL-FLC             TO DTL                          ECS024
00899          MOVE '0'                 TO X                            ECS024
00900          PERFORM 0460-PRT-LINE THRU 0470-EXIT                     ECS024
00901      ELSE                                                         ECS024
00902          MOVE 'CARRIER TOTALS'    TO P-NAME                       ECS024
00903          MOVE CA-LPRM             TO P-LPRM                       ECS024
00904          MOVE CA-LCOM             TO P-LCOM                       ECS024
00905          MOVE CA-APRM             TO P-APRM                       ECS024
00906          MOVE CA-ACOM             TO P-ACOM                       ECS024
00907          MOVE CA-NET              TO P-NET                        ECS024
00908          MOVE DTL-OTHER           TO DTL                          ECS024
00909          MOVE '0'                 TO X                            ECS024
00910          PERFORM 0460-PRT-LINE THRU 0470-EXIT.                    ECS024
00911                                                                   ECS024
00912      MOVE SPACES                  TO DTL                          ECS024
00913                                      PRT.                         ECS024
00914      MOVE ZEROS                   TO CA-LPRM                      ECS024
00915                                      CA-LCOM                      ECS024
00916                                      CA-APRM                      ECS024
00917                                      CA-ACOM                      ECS024
00918                                      CA-NET                       ECS024
00919                                      CA-ALLOW.                    ECS024
00920                                                                   ECS024
00921  0440-EXIT.                                                       ECS024
00922      EXIT.                                                        ECS024
00923                                                                   ECS024
00924  0450-C-B-XX.                                                     ECS024
00925      GO TO 0170-MAIN-CHECK.                                       ECS024
00926                                                                   ECS024
00927  0460-PRT-LINE.                                                   ECS024
00928                              COPY ELCPRT2.                        ECS024
00929  0470-EXIT.                                                       ECS024
00930      EXIT.                                                        ECS024
00931                                                                   ECS024
00932  EJECT                                                            ECS024
00933  0500-FLC-ACCOUNT-SEARCH.                                         ECS024
00934      MOVE ZEROS                        TO T-ALLOW.                ECS024
00935                                                                   ECS024
00936  0500-FLC-SEARCH-LOOP.                                            ECS024
00937                                                                   ECS024
00938      IF NDX GREATER THAN +5                                       ECS024
00939          MOVE ZEROS            TO T-ALLOW                         ECS024
00940          GO TO 0509-EXIT.                                         ECS024
00941                                                                   ECS024
00942      IF AM-ALLOW-BEGIN-RANGE (NDX) NUMERIC                        ECS024
00943          NEXT SENTENCE                                            ECS024
00944      ELSE                                                         ECS024
00945          MOVE ZEROS            TO AM-ALLOW-BEGIN-RANGE (NDX).     ECS024
00946                                                                   ECS024
00947      IF AM-ALLOW-END-RANGE (NDX) NUMERIC                          ECS024
00948          NEXT SENTENCE                                            ECS024
00949      ELSE                                                         ECS024
00950          MOVE ZEROS            TO AM-ALLOW-END-RANGE (NDX).       ECS024
00951                                                                   ECS024
00952      IF AM-ALLOWANCE-AMT (NDX) NUMERIC                            ECS024
00953          NEXT SENTENCE                                            ECS024
00954      ELSE                                                         ECS024
00955          MOVE ZEROS            TO AM-ALLOWANCE-AMT (NDX).         ECS024
00956                                                                   ECS024
00957      IF (TOTPRM NOT LESS THAN AM-ALLOW-BEGIN-RANGE (NDX)          ECS024
00958         AND NOT GREATER THAN AM-ALLOW-END-RANGE (NDX))            ECS024
00959           MOVE AM-ALLOWANCE-AMT (NDX)  TO T-ALLOW                 ECS024
00960           GO TO 0509-EXIT                                         ECS024
00961      ELSE                                                         ECS024
00962         ADD +1                TO NDX                              ECS024
00963         GO TO 0500-FLC-SEARCH-LOOP.                               ECS024
00964                                                                   ECS024
00965  0509-EXIT.                                                       ECS024
00966      EXIT.                                                        ECS024
00967                                                                   ECS024
00968  EJECT                                                            ECS024
00969  0690-LOCATE-STATE.                                               ECS024
00970      MOVE ZEROS                   TO CLAS-INDEXS.                 ECS024
00971                                                                   ECS024
00972  0700-LOCATE-LOOP.                                                ECS024
00973      ADD +1                       TO CLAS-INDEXS.                 ECS024
00974      IF WK-ST NOT = STATE-SUB (CLAS-INDEXS)                       ECS024
00975         GO TO 0700-LOCATE-LOOP.                                   ECS024
00976                                                                   ECS024
00977      MOVE CLAS-INDEXS             TO W-ST.                        ECS024
00978                                                                   ECS024
00979  0710-EXIT.                                                       ECS024
00980       EXIT.                                                       ECS024
00981                                                                   ECS024
00982  0730-END-RTN.                                                    ECS024
00983      MOVE HIGH-VALUES             TO THIS-CTL.                    ECS024
00984      PERFORM 0230-MAIN-BREAK      THRU 0270-EXIT.                 ECS024
00985      PERFORM 0300-STATE-BREAK     THRU 0310-EXIT.                 ECS024
00986      PERFORM 0340-COMPANY-BREAK   THRU 0400-EXIT.                 ECS024
00987      PERFORM 0430-CARRIER-TOTALS  THRU 0440-EXIT.                 ECS024
00988                                                                   ECS024
00989      CLOSE RE-RECAP                                               ECS024
00990            AM-MSTR                                                ECS024
00991            PRNTR.                                                 ECS024
00992                                                                   ECS024
00993  0730-CLOSE-FICH.                                                 ECS024
00994                              COPY ELCPRTC.                        ECS024
00995                                                                   ECS024
00996      GOBACK.                                                      ECS024
00997                                                                   ECS024
00998  ABEND-PGM.                                                       ECS024
00999                              COPY ELCABEND.                       ECS024
01000                                                                   ECS024
