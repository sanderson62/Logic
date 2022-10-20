00001  IDENTIFICATION DIVISION.                                         04/04/98
00002                                                                   ECS025
00003  PROGRAM-ID.                ECS025.                                  LV013
00004 *              PROGRAM CONVERTED BY                               ECS025
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS025
00006 *              CONVERSION DATE 10/03/95 15:28:18.                 ECS025
00007 *             PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE           ECS025
00008 *                           VMOD=2.007.                           ECS025
00009                                                                   ECS025
00010 *AUTHOR.     LOGIC, INC.                                          ECS025
00011 *            DALLAS, TEXAS.                                       ECS025
00012                                                                   ECS025
00013 *DATE-COMPILED.                                                   ECS025
00014                                                                   ECS025
00015 *SECURITY.   *****************************************************ECS025
00016 *            *                                                   *ECS025
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS025
00018 *            *                                                   *ECS025
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS025
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS025
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS025
00022 *            *                                                   *ECS025
00023 *            *****************************************************ECS025
00024                                                                   ECS025
00025 *REMARKS.                                                         ECS025
00026                                                                   ECS025
00027 *        ACCOUNT CURRENT DETAIL TOTALS BY ACCOUNT,STATE           ECS025
00028 *            AND  COMPANY.                                        ECS025
00029                                                                   ECS025
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
00030  ENVIRONMENT DIVISION.                                            ECS025
00031  CONFIGURATION SECTION.                                           ECS025
00032  SPECIAL-NAMES.                                                   ECS025
00033      C02 IS LCP-CH2                                               ECS025
00034      C03 IS LCP-CH3                                               ECS025
00035      C04 IS LCP-CH4                                               ECS025
00036      C05 IS LCP-CH5                                               ECS025
00037      C06 IS LCP-CH6                                               ECS025
00038      C07 IS LCP-CH7                                               ECS025
00039      C08 IS LCP-CH8                                               ECS025
00040      C09 IS LCP-CH9                                               ECS025
00041      C10 IS LCP-CH10                                              ECS025
00042      C11 IS LCP-CH11                                              ECS025
00043      C12 IS LCP-CH12                                              ECS025
00044      S01 IS LCP-P01                                               ECS025
00045      S02 IS LCP-P02.                                              ECS025
00046  INPUT-OUTPUT SECTION.                                            ECS025
00047  FILE-CONTROL.                                                    ECS025
00048      SELECT RE-RECAP         ASSIGN TO SYS018-UT-2400-S-SYS018.   ECS025
00049      SELECT AM-MSTR          ASSIGN TO SYS015                     ECS025
00050                              ACCESS IS SEQUENTIAL                 ECS025
00051                              ORGANIZATION IS INDEXED              ECS025
00052                              FILE STATUS IS AM-FILE-STATUS        ECS025
00053                              RECORD KEY IS AM-CONTROL-PRIMARY.    ECS025
00054      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS025
00055      SELECT DISK-DATE        ASSIGN TO SYS019-UT-3380-S-SYS019.   ECS025
00056      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS025
00057                                                                   ECS025
00058  EJECT                                                            ECS025
00059  DATA DIVISION.                                                   ECS025
00060  FILE SECTION.                                                    ECS025
00061  FD  RE-RECAP                                                     ECS025
00062                              COPY ECSEXTFD.                       ECS025
00063                              COPY ECSEXT01.                       ECS025
00064                                                                   ECS025
00065  EJECT                                                            ECS025
00066  FD  AM-MSTR.                                                     ECS025
00067                                                                   ECS025
00068                             COPY ERCACCT.                         ECS025
00069                                                                   ECS025
00070  EJECT                                                            ECS025
00071  FD  DISK-DATE                                                    ECS025
00072                              COPY ELCDTEFD.                       ECS025
00073                                                                   ECS025
00074  EJECT                                                            ECS025
00075  FD  PRNTR                                                        ECS025
00076                              COPY ELCPRTFD.                       ECS025
00077                                                                   ECS025
00078  FD  FICH                                                         ECS025
00079                              COPY ELCFCHFD.                       ECS025
00080                                                                   ECS025
00081                                                                   ECS025
00082  EJECT                                                            ECS025
00083  WORKING-STORAGE SECTION.                                         ECS025
00084  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS025
00085  77  LCP-ASA                       PIC X.                         ECS025
00086  77  FILLER  PIC X(32) VALUE '********************************'.  ECS025
00087  77  FILLER  PIC X(32) VALUE '     ECS025 WORKING STORAGE     '.  ECS025
00088  77  FILLER  PIC X(32) VALUE '*****VMOD=2.007 ****************'.  ECS025
00089                                                                   ECS025
00090  01  FIRST-PASS               PIC 9 VALUE 0.                      ECS025
00091  01  VSAM-ERROR.                                                  ECS025
00092      05  VSAM-CONSTANT        PIC XX VALUE '06'.                  ECS025
00093      05  VSAM-ERR-CODE        PIC XX VALUE '00'.                  ECS025
00094  01  AM-FILE-STATUS           PIC XX VALUE '00'.                  ECS025
00095  01  WS.                                                          ECS025
00096      12  PGM-SUB         PIC S999    COMP      VALUE +025.        ECS025
00097      12  WS-ABEND-MESSAGE PIC X(80)    VALUE ZEROS.               ECS025
00098      12  WS-RETURN-CODE  PIC X(4)    VALUE ZEROS.                 ECS025
00099      12  WS-ABEND-FILE-STATUS PIC XX VALUE SPACES.                ECS025
00100      12  WS-ZERO         PIC S9    VALUE ZEROS.                   ECS025
00101      12  ABEND-OPTION    PIC X     VALUE 'Y'.                     ECS025
00102      12  X               PIC X     VALUE ' '.                     ECS025
00103  01  WS-AM-CO            PIC X(7) VALUE SPACE.                    ECS025
00104  EJECT                                                            ECS025
00105                              COPY ELCDTECX.                       ECS025
00106                                                                   ECS025
00107                              COPY ELCDTEVR.                       ECS025
00108                                                                   ECS025
00109  EJECT                                                            ECS025
00110  01  BINARY-WORK-AREA    COMP.                                    ECS025
00111      12  X1          PIC S999        VALUE +0.                    ECS025
00112      12  X2          PIC S999        VALUE +0.                    ECS025
00113      12  X3          PIC S999        VALUE +0.                    ECS025
00114      12  X4          PIC S999        VALUE +0.                    ECS025
00115      12  B0          PIC S999        VALUE +0.                    ECS025
00116      12  B1          PIC S999        VALUE +1.                    ECS025
00117      12  B55         PIC S999        VALUE +55.                   ECS025
00118      12  B2          PIC S999        VALUE +2.                    ECS025
00119      12  B3          PIC S999        VALUE +3.                    ECS025
00120      12  B4          PIC S999        VALUE +4.                    ECS025
00121                                                                   ECS025
00122  01  COMP-3-WORK-AREA    COMP-3.                                  ECS025
00123      12  LN-CT       PIC S9(5)       VALUE +99.                   ECS025
00124      12  PG-NO       PIC S9(5)       VALUE +0.                    ECS025
00125      12  K1          PIC S9(5)       VALUE +1.                    ECS025
00126      12  LN-MX       PIC S9(5)   VALUE +56.                       ECS025
00127      12  DET-NET-DUE     PIC S9(5)V99    VALUE +0.                ECS025
00128      12  K-1         PIC S9(5)       VALUE -1.                    ECS025
00129      12  T-LP        PIC S9(7)V99    VALUE +0.                    ECS025
00130      12  T-LC        PIC S9(7)V99    VALUE +0.                    ECS025
00131      12  T-AP        PIC S9(7)V99    VALUE +0.                    ECS025
00132      12  T-AC        PIC S9(7)V99    VALUE +0.                    ECS025
00133      12  LCOM        PIC S9(5)V99    VALUE +0.                    ECS025
00134      12  ACOM        PIC S9(5)V99    VALUE +0.                    ECS025
00135      12  L-PREM      PIC S9(7)V99    VALUE +0.                    ECS025
00136      12  A-PREM      PIC S9(7)V99    VALUE +0.                    ECS025
00137      12  ZEV5        PIC SV9(5)      VALUE +.0.                   ECS025
00138      12  NTDUE       PIC S9(7)V99    VALUE +0.                    ECS025
00139      12  S-LP        PIC S9(7)V99    VALUE +0.                    ECS025
00140      12  S-LC        PIC S9(7)V99    VALUE +0.                    ECS025
00141      12  S-AP        PIC S9(7)V99    VALUE +0.                    ECS025
00142      12  S-AC        PIC S9(7)V99    VALUE +0.                    ECS025
00143      12  S-ND        PIC S9(7)V99    VALUE +0.                    ECS025
00144      12  D-LC        PIC S9(5)V99    VALUE +0.                    ECS025
00145      12  D-AC        PIC S9(5)V99    VALUE +0.                    ECS025
00146      12  DIST-TOTALS  OCCURS 3.                                   ECS025
00147          16  T-CNT       PIC S9(7).                               ECS025
00148          16  T-AGE       PIC S9(9).                               ECS025
00149          16  T-LT        PIC S9(9).                               ECS025
00150          16  T-LA        PIC S9(7)V99.                            ECS025
00151          16  T-LCNT      PIC S9(7).                               ECS025
00152          16  T-AT        PIC S9(9).                               ECS025
00153          16  T-AA        PIC S9(7)V99.                            ECS025
00154          16  T-ACNT      PIC S9(7).                               ECS025
00155                                                                   ECS025
00156  EJECT                                                            ECS025
00157  01  HD1.                                                         ECS025
00158      12  FILLER      PIC X(51)   VALUE SPACES.                    ECS025
00159      12  FILLER      PIC X(22)   VALUE 'ACCOUNT CURRENT REPORT'.  ECS025
00160      12  HD1-DESC    PIC X(47)   VALUE SPACES.                    ECS025
00161      12  FILLER      PIC X(6)    VALUE 'ECS025'.                  ECS025
00162                                                                   ECS025
00163  01  HD2.                                                         ECS025
00164      12  FILLER      PIC X(47)   VALUE SPACES.                    ECS025
00165      12  HD-CMP      PIC X(30).                                   ECS025
00166      12  FILLER      PIC X(43)   VALUE SPACES.                    ECS025
00167      12  HD-IPL      PIC X(8).                                    ECS025
00168                                                                   ECS025
00169  01  HD3.                                                         ECS025
00170      12  FILLER      PIC X(53)   VALUE SPACES.                    ECS025
00171      12  HD-DT1      PIC X(18).                                   ECS025
00172      12  FILLER      PIC X(45)   VALUE SPACES.                    ECS025
00173      12  FILLER      PIC X(5)    VALUE 'PAGE '.                   ECS025
00174      12  D-HD-PG     PIC ZZ,ZZ9.                                  ECS025
00175                                                                   ECS025
00176  01  HD4.                                                         ECS025
00177      12  FILLER      PIC X(11)  VALUE ' CARRIER - '.              ECS025
00178      12  HDCO        PIC X.                                       ECS025
00179      12  FILLER      PIC X(9)  VALUE ' GROUP - '.                 ECS025
00180      12  HDGRP       PIC X(6).                                    ECS025
00181      12  FILLER      PIC X(5)   VALUE SPACES.                     ECS025
00182      12  HDST1       PIC X(8)   VALUE 'STATE - '.                 ECS025
00183      12  HDST2       PIC XX.                                      ECS025
00184      12  FILLER      PIC XX     VALUE SPACES.                     ECS025
00185      12  HDST3       PIC X(20)  VALUE SPACES.                     ECS025
00186                                                                   ECS025
00187  01  HD5.                                                         ECS025
00188      12  FILLER      PIC X(8)   VALUE ' ACCOUNT'.                 ECS025
00189      12  FILLER      PIC X(20)  VALUE '   EFFECTIVE   CERT '.     ECS025
00190      12  FILLER      PIC X(12)  VALUE '     TYPE   '.             ECS025
00191      12  HD5LIFE1    PIC X(12)  VALUE '------------'.             ECS025
00192      12  HD5LIFE2    PIC X(12).                                   ECS025
00193      12  HD5LIFE3    PIC X(12)  VALUE '------------'.             ECS025
00194      12  FILLER      PIC XXX    VALUE SPACES.                     ECS025
00195      12  HD5AH1      PIC X(12)  VALUE '------------'.             ECS025
00196      12  HD5AH2      PIC X(12).                                   ECS025
00197      12  HD5AH3      PIC X(12)  VALUE '------------'.             ECS025
00198      12  FILLER      PIC X(16)  VALUE '        NET DUE '.         ECS025
00199                                                                   ECS025
00200  01  HD6.                                                         ECS025
00201      12  FILLER      PIC X(8)  VALUE ' NUMBER '.                  ECS025
00202      12  FILLER      PIC X(20) VALUE '      DATE    NUMBER'.      ECS025
00203      12  FILLER      PIC X(24) VALUE '            TRM TYPE    '.  ECS025
00204      12  FILLER      PIC X(24) VALUE ' PREMIUM     COMMISSION '.  ECS025
00205      12  FILLER      PIC X(24) VALUE '   TRM TYPE     PREMIUM '.  ECS025
00206      12  FILLER      PIC X(24) VALUE '    COMMISSION          '.  ECS025
00207      12  FILLER      PIC X(08) VALUE SPACES.                      ECS025
00208                                                                   ECS025
00209  01  HD7.                                                         ECS025
00210      12  FILLER      PIC X(18)  VALUE '            STATE '.       ECS025
00211      12  FILLER      PIC X(28)  VALUE '  STATE NAME            '. ECS025
00212      12  HD7LIFE1    PIC X(09)  VALUE '---------'.                ECS025
00213      12  HD7LIFE2    PIC X(12).                                   ECS025
00214      12  HD7LIFE3    PIC X(09)  VALUE '---------'.                ECS025
00215      12  FILLER      PIC X(09)  VALUE SPACES.                     ECS025
00216      12  HD7AH1      PIC X(09)  VALUE '---------'.                ECS025
00217      12  HD7AH2      PIC X(12).                                   ECS025
00218      12  HD7AH3      PIC X(09)  VALUE '---------'.                ECS025
00219      12  FILLER      PIC X(16)  VALUE '        NET DUE '.         ECS025
00220                                                                   ECS025
00221  01  HD8.                                                         ECS025
00222      12  FILLER      PIC X(8)  VALUE SPACES.                      ECS025
00223      12  FILLER      PIC X(20) VALUE SPACES.                      ECS025
00224      12  FILLER      PIC X(22) VALUE SPACES.                      ECS025
00225      12  FILLER      PIC X(25) VALUE 'PREMIUM        COMMISSION'. ECS025
00226      12  FILLER      PIC X(25) VALUE '               PREMIUM   '. ECS025
00227      12  FILLER      PIC X(24) VALUE '    COMMISSION          '.  ECS025
00228      12  FILLER      PIC X(08) VALUE SPACES.                      ECS025
00229                                                                   ECS025
00230  01  PRT-DETAIL.                                                  ECS025
00231      05  D-AM-ACCOUNT    PIC X(10).                               ECS025
00232      05  FILLER          PIC X(2)    VALUE SPACES.                ECS025
00233      05  D-EFF-MO    PIC 99.                                      ECS025
00234      05  D-MIN-1     PIC X.                                       ECS025
00235      05  D-EFF-DA    PIC 99.                                      ECS025
00236      05  D-MIN-2     PIC X.                                       ECS025
00237      05  D-EFF-YR    PIC 99.                                      ECS025
00238      05  FILLER      PIC X       VALUE SPACES.                    ECS025
00239      05  D-CERT-NO   PIC X(11).                                   ECS025
00240      05  FILLER      PIC X       VALUE SPACES.                    ECS025
00241      05  D-TYPE      PIC X(6).                                    ECS025
00242      05  FILLER      PIC X       VALUE SPACES.                    ECS025
00243      05  D-LF-TERM   PIC Z99     BLANK WHEN ZERO.                 ECS025
00244      05  FILLER      PIC X(2)    VALUE SPACES.                    ECS025
00245      05  D-L-TYPE    PIC XX.                                      ECS025
00246      05  FILLER      PIC X       VALUE SPACES.                    ECS025
00247      05  D-LIFE-PR   PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.          ECS025
00248      05  FILLER      PIC XXXX    VALUE SPACES.                    ECS025
00249      05  D-LIFE-CM   PIC ZZZ,ZZZ.99-    BLANK WHEN ZERO.          ECS025
00250      05  FILLER      PIC XXX     VALUE SPACES.                    ECS025
00251      05  D-AH-TERM   PIC Z99     BLANK WHEN ZERO.                 ECS025
00252      05  FILLER      PIC X       VALUE SPACES.                    ECS025
00253      05  D-AH-TYPE   PIC XXX.                                     ECS025
00254      05  FILLER      PIC X       VALUE SPACES.                    ECS025
00255      05  D-AH-PREM   PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.          ECS025
00256      05  FILLER      PIC XXXX    VALUE SPACES.                    ECS025
00257      05  D-AH-CM     PIC ZZZ,ZZZ.99-    BLANK WHEN ZERO.          ECS025
00258      05  FILLER      PIC X       VALUE SPACES.                    ECS025
00259      05  D-NET-DUE   PIC ZZZ,ZZZ,ZZZ.99- BLANK WHEN ZERO.         ECS025
00260      05  FILLER      PIC X(2)    VALUE SPACES.                    ECS025
00261                                                                   ECS025
00262  01  DTL REDEFINES PRT-DETAIL.                                    ECS025
00263      12  FILLER      PIC X.                                       ECS025
00264      12  P-ACCT      PIC X(10).                                   ECS025
00265      12  FILLER      PIC XXXX.                                    ECS025
00266      12  P-NAME.                                                  ECS025
00267          16  P-SUB   PIC XX.                                      ECS025
00268          16  FILLER  PIC XXXX.                                    ECS025
00269          16  P-PIC   PIC X(24).                                   ECS025
00270      12  FILLER      PIC XX.                                      ECS025
00271      12  P-LPRM      PIC ZZZ,ZZZ,ZZZ.99- BLANK WHEN ZERO.         ECS025
00272      12  FILLER      PIC X.                                       ECS025
00273      12  P-LCOM      PIC ZZ,ZZZ,ZZZ.99-  BLANK WHEN ZERO.         ECS025
00274      12  FILLER      PIC X(9).                                    ECS025
00275      12  P-APRM      PIC ZZZ,ZZZ,ZZZ.99- BLANK WHEN ZERO.         ECS025
00276      12  FILLER      PIC X.                                       ECS025
00277      12  P-ACOM      PIC ZZ,ZZZ,ZZZ.99-  BLANK WHEN ZERO.         ECS025
00278      12  FILLER      PIC X.                                       ECS025
00279      12  P-NET       PIC ZZZ,ZZZ,ZZZ.99-.                         ECS025
00280                                                                   ECS025
00281  01  RSK-DTL REDEFINES PRT-DETAIL.                                ECS025
00282      12  FILLER      PIC X(27).                                   ECS025
00283      12  RD-AGE      PIC X(12).                                   ECS025
00284      12  FILLER      PIC X(7).                                    ECS025
00285      12  RD-AA       PIC Z99.                                     ECS025
00286      12  FILLER      PIC X(12).                                   ECS025
00287      12  RD-LAT      PIC 999.                                     ECS025
00288      12  FILLER      PIC XXXX.                                    ECS025
00289      12  RD-LAA      PIC ZZ,ZZ9.                                  ECS025
00290      12  FILLER      PIC X(11).                                   ECS025
00291      12  RD-AAT      PIC 999.                                     ECS025
00292      12  FILLER      PIC XXXX.                                    ECS025
00293      12  RD-AAA      PIC ZZ,ZZ9.                                  ECS025
00294      12  FILLER      PIC X(35).                                   ECS025
00295                                                                   ECS025
00296                                                                   ECS025
00297  EJECT                                                            ECS025
00298  01  MISC-WORK-AREA.                                              ECS025
00299      12  THIS-CTL.                                                ECS025
00300          16  THS-CO.                                              ECS025
00301             18 THS-CARR  PIC X.                                   ECS025
00302             18 THS-COMP  PIC X(6).                                ECS025
00303          16  THS-ST      PIC XX.                                  ECS025
00304          16  THS-ACCT    PIC X(10).                               ECS025
00305          16  THS-EFF     PIC 9(11)  COMP-3.                       ECS025
00306      12  LST-CTL     PIC X(18)  VALUE SPACES.                     ECS025
00307      12  LST-NM      PIC X(30).                                   ECS025
00308      12  LST-ACC     PIC X(10).                                   ECS025
00309      12  LST-CO      PIC X(7).                                    ECS025
00310      12  WK-ST.                                                   ECS025
00311          16  W-ST    PIC 99.                                      ECS025
00312      12  WK-AGT.                                                  ECS025
00313          16  FILLER  PIC XX.                                      ECS025
00314          16  WK-Z    PIC XXXX.                                    ECS025
00315      12  SV-TYP      PIC XXX.                                     ECS025
00316      12  THS-CTL     PIC X(6).                                    ECS025
00317      12  CK-CTL      PIC X(6)  VALUE SPACES.                      ECS025
00318                                                                      CL**7
00319  01  WK-DATE-EFF            PIC 9(11).                               CL*13
00320  01  WK-DATE-EFF-RDEF  REDEFINES  WK-DATE-EFF.                       CL*13
00321      05  FILLER             PIC 999.                                 CL**8
00322      05  WK-CCYY.                                                    CL*11
00323          10  WK-CC          PIC 99.                                  CL*11
00324          10  WK-YY          PIC 99.                                  CL*11
00325      05  WK-MM              PIC 99.                                  CL*11
00326      05  WK-DD              PIC 99.                                  CL*11
00327                                                                      CL**8
00328  01  SUB-TYPE        PIC 9   VALUE ZERO.                          ECS025
00329                                                                   ECS025
00330  01  STATE-TOTAL-AREA.                                            ECS025
00331      12  ST-TOTALS OCCURS 100.                                    ECS025
00332          16  ST-LP       PIC S9(7)V99   COMP-3.                   ECS025
00333          16  ST-LC       PIC S9(7)V99   COMP-3.                   ECS025
00334          16  ST-AP       PIC S9(7)V99   COMP-3.                   ECS025
00335          16  ST-AC       PIC S9(7)V99   COMP-3.                   ECS025
00336          16  ST-ND       PIC S9(7)V99   COMP-3.                   ECS025
00337          16  ST-ID       PIC X.                                   ECS025
00338                                                                   ECS025
00339  01  CARRIER-TOTAL-AREA.                                          ECS025
00340      03 CA-LPRM     PIC S9(7)V99 COMP-3.                          ECS025
00341      03 CA-LCOM     PIC S9(7)V99 COMP-3.                          ECS025
00342      03 CA-APRM     PIC S9(7)V99 COMP-3.                          ECS025
00343      03 CA-ACOM     PIC S9(7)V99 COMP-3.                          ECS025
00344      03 CA-NET      PIC S9(7)V99 COMP-3.                          ECS025
00345                                                                   ECS025
00346  01  SV-RDX      PIC X(168).                                      ECS025
00347                                                                   ECS025
00348  01  HD-RD.                                                       ECS025
00349      12  FILLER  PIC X(20)   VALUE SPACES.                        ECS025
00350      12  HD-TYP  PIC X(8)    VALUE SPACES.                        ECS025
00351      12  HD-CTL  PIC X(8)    VALUE SPACES.                        ECS025
00352      12  HD-NME  PIC X(20)   VALUE SPACES.                        ECS025
00353                                                                   ECS025
00354  01  RD1.                                                         ECS025
00355      12  FILLER  PIC X(46)   VALUE SPACES.                        ECS025
00356      12  FILLER  PIC X(31)   VALUE 'AVE.    LIFE  AVE.     AVE.'. ECS025
00357      12  FILLER  PIC X(30)   VALUE 'A&H   AVE.     AVE'.          ECS025
00358                                                                   ECS025
00359  01  RD2.                                                         ECS025
00360      12  FILLER  PIC X(46)   VALUE SPACES.                        ECS025
00361      12  FILLER  PIC X(31)   VALUE 'AGE           TERM   AMOUNT'. ECS025
00362      12  FILLER  PIC X(30)   VALUE '      TERM   BENEFIT'.        ECS025
00363                                                                   ECS025
00364  01  RDX-CALC-AREA.                                               ECS025
00365      12  XDX-ID      PIC XXX.                                     ECS025
00366      12  XDX-CTL     PIC X(6).                                    ECS025
00367      12  FILLER      PIC X(18).                                   ECS025
00368      12  XDX-AMTS   COMP-3,      OCCURS 3.                        ECS025
00369          16  XDX-CNT     PIC S9(7).                               ECS025
00370          16  XDX-AGE     PIC S9(9).                               ECS025
00371          16  XDX-LT      PIC S9(9).                               ECS025
00372          16  XDX-LA      PIC S9(7)V99.                            ECS025
00373          16  XDX-LCNT    PIC S9(7).                               ECS025
00374          16  XDX-AT      PIC S9(9).                               ECS025
00375          16  XDX-AA      PIC S9(7)V99.                            ECS025
00376          16  XDX-ACNT    PIC S9(7).                               ECS025
00377                                                                   ECS025
00378  EJECT                                                            ECS025
00379  PROCEDURE DIVISION.                                              ECS025
00380  0100-SET-START.                                                  ECS025
00381                              COPY ELCDTERX SUPPRESS.              ECS025
00382                                                                   ECS025
00383      IF DTE-PGM-OPT = '2'                                         ECS025
00384          MOVE ' - (REPORTS DIRECT COMMISSIONS ONLY)' TO HD1-DESC. ECS025
00385      IF DTE-PGM-OPT = '3'                                         ECS025
00386          MOVE ' - (REPORTS OVERWRITE COMMISSIONS ONLY)'           ECS025
00387                                                      TO HD1-DESC. ECS025
00388                                                                   ECS025
00389      MOVE COMPANY-NAME TO HD-CMP.                                 ECS025
00390      MOVE ALPH-DATE TO HD-DT1.                                    ECS025
00391      MOVE WS-CURRENT-DATE TO HD-IPL.                              ECS025
00392                                                                   ECS025
00393      OPEN INPUT RE-RECAP  AM-MSTR                                 ECS025
00394           OUTPUT PRNTR.                                           ECS025
00395                                                                   ECS025
00396      IF AM-FILE-STATUS  = '00' OR '97'                            ECS025
00397          NEXT SENTENCE                                            ECS025
00398        ELSE                                                       ECS025
00399          MOVE AM-FILE-STATUS TO WS-ABEND-FILE-STATUS              ECS025
00400          MOVE AM-FILE-STATUS TO VSAM-ERR-CODE                     ECS025
00401          MOVE VSAM-ERROR     TO WS-RETURN-CODE                    ECS025
00402          MOVE 'ERACCT OPEN ERROR ' TO WS-ABEND-MESSAGE            ECS025
00403          PERFORM ABEND-PGM.                                       ECS025
00404                                                                   ECS025
00405      MOVE SPACES TO STATE-TOTAL-AREA.                             ECS025
00406                                                                   ECS025
00407  0110-ZERO-CARR.                                                  ECS025
00408      MOVE ZEROS TO CA-LPRM CA-LCOM CA-APRM CA-ACOM CA-NET.        ECS025
00409                                                                   ECS025
00410  0120-ZERO-DIST.                                                  ECS025
00411      MOVE ZEROS TO T-CNT (1)                                      ECS025
00412                    T-AGE (1)                                      ECS025
00413                    T-LT  (1)                                      ECS025
00414                    T-LA  (1)                                      ECS025
00415                    T-LCNT (1)                                     ECS025
00416                    T-AT  (1)                                      ECS025
00417                    T-AA  (1)                                      ECS025
00418                    T-ACNT (1).                                    ECS025
00419                                                                   ECS025
00420      MOVE DIST-TOTALS (1) TO DIST-TOTALS (2),                     ECS025
00421                              DIST-TOTALS (3).                     ECS025
00422                                                                   ECS025
00423  EJECT                                                            ECS025
00424  0140-GET-ACCT.                                                   ECS025
00425      READ AM-MSTR.                                                ECS025
00426                                                                   ECS025
00427      IF AM-FILE-STATUS = '00' OR '10'                             ECS025
00428          NEXT SENTENCE                                            ECS025
00429      ELSE                                                         ECS025
00430          MOVE AM-FILE-STATUS TO WS-ABEND-FILE-STATUS              ECS025
00431          MOVE AM-FILE-STATUS TO VSAM-ERR-CODE                     ECS025
00432          MOVE VSAM-ERROR     TO WS-RETURN-CODE                    ECS025
00433          MOVE 'VSAM SEQUENCE ERROR ' TO WS-ABEND-MESSAGE          ECS025
00434          PERFORM ABEND-PGM.                                       ECS025
00435                                                                   ECS025
00436      IF AM-FILE-STATUS = '10'                                     ECS025
00437          MOVE HIGH-VALUES TO AM-CNTRL-1.                          ECS025
00438                                                                   ECS025
00439  0150-GET-ACCT-X.    EXIT.                                        ECS025
00440                                                                   ECS025
00441  0160-GET-DTL.                                                    ECS025
00442      READ RE-RECAP                                                ECS025
00443           AT END GO TO 0520-END-RTN.                              ECS025
00444                                                                   ECS025
00445      IF DE-RECORD-ID NOT = 'DE'                                   ECS025
00446          GO TO 0160-GET-DTL.                                      ECS025
00447                                                                   ECS025
00448      IF DE-TRANS NOT = 'I' AND 'C'                                ECS025
00449          GO TO 0160-GET-DTL.                                      ECS025
00450                                                                   ECS025
00451      IF DE-ISSUE                                                  ECS025
00452         IF DE-LF-STAT-CDE = ('5' OR '9')                          ECS025
00453            GO TO 0160-GET-DTL.                                    ECS025
00454                                                                   ECS025
00455      IF DE-ISSUE                                                  ECS025
00456         IF DE-AH-STAT-CDE = ('5' OR '9')                          ECS025
00457            GO TO 0160-GET-DTL.                                    ECS025
00458                                                                   ECS025
00459      IF DE-REIN NOT = SPACES                                      ECS025
00460         GO TO 0160-GET-DTL.                                       ECS025
00461                                                                   ECS025
00462      MOVE DE-CARRIER  TO THS-CARR.                                ECS025
00463      MOVE DE-GROUPING TO THS-COMP.                                ECS025
00464      MOVE DE-STATE    TO THS-ST.                                  ECS025
00465      MOVE DE-ACCOUNT  TO THS-ACCT.                                ECS025
00466      MOVE DE-EFF      TO THS-EFF.                                 ECS025
00467                                                                   ECS025
00468  0170-MAIN-CHECK.                                                 ECS025
00469      IF LST-CTL = SPACES                                          ECS025
00470          MOVE THIS-CTL TO LST-CTL.                                ECS025
00471                                                                   ECS025
00472      STRING AM-CARRIER DELIMITED BY SIZE                          ECS025
00473             AM-GROUPING DELIMITED BY SIZE                         ECS025
00474      INTO WS-AM-CO.                                               ECS025
00475                                                                   ECS025
00476      IF THS-CO = WS-AM-CO AND                                     ECS025
00477         THS-ST = AM-STATE AND                                     ECS025
00478         THS-ACCT = AM-ACCOUNT AND                                 ECS025
00479         THS-EFF LESS AM-EXPIRE-DT GO TO 0190-ML1.                 ECS025
00480                                                                   ECS025
00481      IF (THIS-CTL GREATER AM-MSTR-CNTRL) OR                          CL**5
00482         (THIS-CTL = AM-MSTR-CNTRL)                                   CL**6
00483          PERFORM 0140-GET-ACCT THRU 0150-GET-ACCT-X               ECS025
00484          GO TO 0170-MAIN-CHECK.                                   ECS025
00485                                                                   ECS025
00486      MOVE THS-EFF                TO  WK-DATE-EFF.                    CL**9
00487      DISPLAY 'ACCOUNT MISSING-' ' CARR ' THS-CARR ' GROUP '          CL**5
00488              THS-COMP ' ST ' THS-ST ' ACCT ' THS-ACCT                CL**5
00489              ' EFF' WK-CCYY WK-MM WK-DD.                             CL**9
00490                                                                      CL**5
00491      MOVE '0302' TO WS-RETURN-CODE.                               ECS025
00492      PERFORM ABEND-PGM.                                           ECS025
00493                                                                   ECS025
00494  EJECT                                                            ECS025
00495  0190-ML1.                                                        ECS025
00496                                                                   ECS025
00497      MOVE AM-NAME     TO LST-NM.                                  ECS025
00498      MOVE AM-ACCOUNT  TO LST-ACC.                                 ECS025
00499      MOVE  DE-LF-PRM  TO  L-PREM.                                 ECS025
00500      ADD   DE-LF-PRM-ALT  TO  L-PREM.                             ECS025
00501      MOVE  DE-AH-PRM  TO  A-PREM.                                 ECS025
00502                                                                   ECS025
00503      IF DE-CANCEL  COMPUTE L-PREM = DE-LF-RFND * K-1              ECS025
00504                    COMPUTE A-PREM = DE-AH-RFND * K-1.             ECS025
00505                                                                   ECS025
00506      ADD L-PREM TO T-LP.                                          ECS025
00507      ADD A-PREM TO T-AP.                                          ECS025
00508                                                                   ECS025
00509      MOVE  +0   TO  X1.                                           ECS025
00510                                                                   ECS025
00511  0200-CALC-COMM-LOOP.                                             ECS025
00512                                                                   ECS025
00513      ADD  +1  TO  X1.                                             ECS025
00514                                                                   ECS025
00515      IF X1 GREATER THAN +10                                       ECS025
00516          GO TO 0210-CALC-COMM-X.                                  ECS025
00517                                                                   ECS025
00518      IF DE-AGT-TYPE (X1) = ' ' OR 'W' OR 'R' OR 'T'               ECS025
00519          GO TO 0200-CALC-COMM-LOOP.                               ECS025
00520                                                                   ECS025
00521      IF DTE-PGM-OPT  =  '2'                                       ECS025
00522         IF DE-AGT-TYPE (X1) NOT EQUAL 'C' AND 'D'                 ECS025
00523             GO TO 0200-CALC-COMM-LOOP.                            ECS025
00524                                                                   ECS025
00525      IF DTE-PGM-OPT  =  '3'                                       ECS025
052814        IF DE-AGT-TYPE (X1) NOT EQUAL 'O' AND 'P' and 'S'
00527             GO TO 0200-CALC-COMM-LOOP.                            ECS025
00528                                                                   ECS025
00529      IF DE-AGT (X1) NOT = ZERO AND SPACES                         ECS025
00530          COMPUTE LCOM ROUNDED = L-PREM  *  DE-L-PC (X1)           ECS025
00531          COMPUTE ACOM ROUNDED = A-PREM  *  DE-A-PC (X1)           ECS025
00532          ADD  LCOM  TO  T-LC                                      ECS025
00533                         D-LC                                      ECS025
00534          ADD  ACOM  TO  T-AC                                      ECS025
00535                         D-AC.                                     ECS025
00536                                                                   ECS025
00537      GO TO 0200-CALC-COMM-LOOP.                                   ECS025
00538                                                                   ECS025
00539  0210-CALC-COMM-X.                                                ECS025
00540                                                                   ECS025
00541      PERFORM 0560-PRINT-DETAIL-LINE THRU 0570-LINE-EXIT.          ECS025
00542                                                                   ECS025
00543      PERFORM 0160-GET-DTL.                                        ECS025
00544                                                                   ECS025
00545      IF THIS-CTL LESS AM-CNTRL-1                                  ECS025
00546          GO TO 0190-ML1.                                          ECS025
00547                                                                   ECS025
00548      IF THS-CO   = WS-AM-CO AND                                   ECS025
00549         THS-ST   = AM-STATE AND                                   ECS025
00550         THS-ACCT = AM-ACCOUNT                                     ECS025
00551          GO TO 0170-MAIN-CHECK                                    ECS025
00552         ELSE                                                      ECS025
00553          GO TO 0230-MAIN-BREAK.                                   ECS025
00554                                                                   ECS025
00555  EJECT                                                            ECS025
00556  0230-MAIN-BREAK.                                                 ECS025
00557      IF LN-CT LESS LN-MX                                          ECS025
00558          GO TO 0250-PT-ACCT.                                      ECS025
00559                                                                   ECS025
00560  0240-PT-HDNG.                                                    ECS025
00561      MOVE HD1              TO P-DATA.                             ECS025
00562      MOVE '1'              TO X.                                  ECS025
00563      PERFORM 0460-PRT-LINE THRU 0470-PRT-EXIT.                    ECS025
00564      ADD K1 TO PG-NO.                                             ECS025
00565      MOVE PG-NO            TO D-HD-PG.                            ECS025
00566      MOVE HD2              TO P-DATA.                             ECS025
00567      MOVE ' '              TO X.                                  ECS025
00568      PERFORM 0460-PRT-LINE THRU 0470-PRT-EXIT.                    ECS025
00569      MOVE HD3              TO P-DATA.                             ECS025
00570      MOVE ' '              TO X.                                  ECS025
00571      PERFORM 0460-PRT-LINE THRU 0470-PRT-EXIT.                    ECS025
00572                                                                   ECS025
00573      MOVE AM-CARRIER       TO HDCO.                               ECS025
00574      MOVE AM-GROUPING      TO HDGRP.                              ECS025
00575      MOVE AM-STATE         TO HDST2, WK-ST,                       ECS025
00576      PERFORM 0490-LOCATE-STATE THRU 0510-LOCATE-XIT.              ECS025
00577      MOVE STATE-PIC (W-ST) TO HDST3.                              ECS025
00578      MOVE HD4              TO P-DATA.                             ECS025
00579      MOVE '0'              TO X.                                  ECS025
00580      PERFORM 0460-PRT-LINE THRU 0470-PRT-EXIT.                    ECS025
00581                                                                   ECS025
00582      IF LIFE-OVERRIDE-L12 = SPACES                                ECS025
00583          MOVE SPACES            TO HD5LIFE1                       ECS025
00584                                    HD5LIFE2                       ECS025
00585                                    HD5LIFE3                       ECS025
00586      ELSE                                                         ECS025
00587          MOVE LIFE-OVERRIDE-L12 TO HD5LIFE2.                      ECS025
00588                                                                   ECS025
00589      IF AH-OVERRIDE-L12 = SPACES                                  ECS025
00590          MOVE SPACES            TO HD5AH1                         ECS025
00591                                    HD5AH2                         ECS025
00592                                    HD5AH3                         ECS025
00593      ELSE                                                         ECS025
00594          MOVE AH-OVERRIDE-L12   TO HD5AH2.                        ECS025
00595                                                                   ECS025
00596      MOVE HD5 TO P-DATA.                                          ECS025
00597      MOVE '0' TO X.                                               ECS025
00598      PERFORM 0460-PRT-LINE THRU 0470-PRT-EXIT.                    ECS025
00599                                                                   ECS025
00600      MOVE HD6 TO P-DATA.                                          ECS025
00601      MOVE ' ' TO X.                                               ECS025
00602      PERFORM 0460-PRT-LINE THRU 0470-PRT-EXIT.                    ECS025
00603                                                                   ECS025
00604      MOVE SPACES TO P-DATA.                                       ECS025
00605      MOVE ' ' TO X.                                               ECS025
00606      PERFORM 0460-PRT-LINE THRU 0470-PRT-EXIT.                    ECS025
00607                                                                   ECS025
00608      MOVE 9 TO LN-CT.                                             ECS025
00609                                                                   ECS025
00610  0250-PT-ACCT.                                                    ECS025
00611      MOVE SPACES            TO DTL.                               ECS025
00612      MOVE ' *TOTAL*'        TO P-ACCT.                            ECS025
00613      MOVE LST-NM            TO P-NAME.                            ECS025
00614      MOVE T-LP              TO P-LPRM.                            ECS025
00615      MOVE T-LC              TO P-LCOM.                            ECS025
00616      MOVE T-AP              TO P-APRM.                            ECS025
00617      MOVE T-AC              TO P-ACOM.                            ECS025
00618      COMPUTE NTDUE = T-LP + T-AP - T-AC - T-LC.                   ECS025
00619      MOVE NTDUE             TO P-NET.                             ECS025
00620      MOVE ' '               TO X.                                 ECS025
00621      MOVE DTL               TO PRT.                               ECS025
00622      PERFORM 0460-PRT-LINE THRU 0470-PRT-EXIT.                    ECS025
00623      MOVE SPACES            TO DTL PRT.                           ECS025
00624      MOVE '0'               TO X.                                 ECS025
00625      PERFORM 0460-PRT-LINE THRU 0470-PRT-EXIT.                    ECS025
00626      ADD 3 TO LN-CT.                                              ECS025
00627                                                                   ECS025
00628  0260-M-R-3.                                                      ECS025
00629      PERFORM 0120-ZERO-DIST.                                      ECS025
00630      ADD T-LP  TO S-LP.                                           ECS025
00631      ADD T-LC  TO S-LC.                                           ECS025
00632      ADD T-AP  TO S-AP.                                           ECS025
00633      ADD T-AC  TO S-AC.                                           ECS025
00634      ADD NTDUE TO S-ND.                                           ECS025
00635      MOVE ZEROS TO T-LP  T-LC  T-AP  T-AC  NTDUE.                    CL**3
00636                                                                   ECS025
00637  0270-M-B-X.                                                      ECS025
00638      EXIT.                                                        ECS025
00639                                                                   ECS025
00640  0280-M-B-XX.                                                     ECS025
00641      IF THS-CO = WS-AM-CO AND                                     ECS025
00642         THS-ST = AM-STATE                                         ECS025
00643          GO TO 0170-MAIN-CHECK                                    ECS025
00644         ELSE                                                      ECS025
00645          GO TO 0300-STATE-BREAK.                                  ECS025
00646                                                                   ECS025
00647  EJECT                                                            ECS025
00648  0300-STATE-BREAK.                                                ECS025
00649      MOVE SPACES            TO DTL.                               ECS025
00650      MOVE 'STATE TOTAL'     TO P-NAME.                            ECS025
00651      MOVE S-LP              TO P-LPRM.                            ECS025
00652      MOVE S-LC              TO P-LCOM.                            ECS025
00653      MOVE S-AP              TO P-APRM.                            ECS025
00654      MOVE S-AC              TO P-ACOM.                            ECS025
00655      MOVE S-ND              TO P-NET.                             ECS025
00656      MOVE '0'               TO X.                                 ECS025
00657      MOVE DTL               TO PRT.                               ECS025
00658      PERFORM 0460-PRT-LINE THRU 0470-PRT-EXIT.                    ECS025
00659      MOVE SPACES            TO DTL PRT.                           ECS025
00660      MOVE +99               TO LN-CT.                             ECS025
00661      MOVE SPACES            TO LST-CTL.                           ECS025
00662      MOVE S-LP              TO ST-LP (100).                       ECS025
00663      MOVE S-AP              TO ST-AP (100).                       ECS025
00664      MOVE S-LC              TO ST-LC (100).                       ECS025
00665      MOVE S-AC              TO ST-AC (100).                       ECS025
00666      MOVE S-ND              TO ST-ND (100).                       ECS025
00667      MOVE ST-TOTALS (100)   TO ST-TOTALS (W-ST).                  ECS025
00668      MOVE '*'               TO ST-ID (W-ST).                      ECS025
00669      MOVE ZEROS             TO S-LP  S-LC  S-AP  S-AC  S-ND.         CL**3
00670                                                                   ECS025
00671  0310-S-B-X.                                                      ECS025
00672      EXIT.                                                        ECS025
00673                                                                   ECS025
00674  0320-S-B-XX.                                                     ECS025
00675      IF THS-CO = WS-AM-CO                                         ECS025
00676          GO TO 0170-MAIN-CHECK                                    ECS025
00677          ELSE                                                     ECS025
00678          GO TO 0340-COMPANY-BREAK.                                ECS025
00679                                                                   ECS025
00680  EJECT                                                            ECS025
00681  0340-COMPANY-BREAK.                                              ECS025
00682      MOVE HD1               TO P-DATA.                            ECS025
00683      MOVE '1'               TO X.                                 ECS025
00684      PERFORM 0460-PRT-LINE THRU 0470-PRT-EXIT.                    ECS025
00685      ADD K1                 TO PG-NO.                             ECS025
00686      MOVE PG-NO             TO D-HD-PG.                           ECS025
00687      MOVE HD2               TO P-DATA.                            ECS025
00688      MOVE ' '               TO X.                                 ECS025
00689      PERFORM 0460-PRT-LINE THRU 0470-PRT-EXIT.                    ECS025
00690      MOVE HD3               TO P-DATA.                            ECS025
00691      MOVE ' '               TO X.                                 ECS025
00692      PERFORM 0460-PRT-LINE THRU 0470-PRT-EXIT.                    ECS025
00693      MOVE SPACES            TO HDST1  HDST2  HDST3.               ECS025
00694      MOVE HD4               TO P-DATA.                            ECS025
00695      MOVE '0'               TO X.                                 ECS025
00696      PERFORM 0460-PRT-LINE THRU 0470-PRT-EXIT.                    ECS025
00697      MOVE 'STATE - '        TO HDST1.                             ECS025
00698                                                                   ECS025
00699      IF LIFE-OVERRIDE-L12 = SPACES                                ECS025
00700          MOVE SPACES            TO HD7LIFE1                       ECS025
00701                                    HD7LIFE2                       ECS025
00702                                    HD7LIFE3                       ECS025
00703      ELSE                                                         ECS025
00704          MOVE LIFE-OVERRIDE-L12 TO HD7LIFE2.                      ECS025
00705                                                                   ECS025
00706      IF AH-OVERRIDE-L12 = SPACES                                  ECS025
00707          MOVE SPACES            TO HD7AH1                         ECS025
00708                                    HD7AH2                         ECS025
00709                                    HD7AH3                         ECS025
00710      ELSE                                                         ECS025
00711          MOVE AH-OVERRIDE-L12   TO HD7AH2.                        ECS025
00712                                                                   ECS025
00713      MOVE HD7              TO P-DATA.                             ECS025
00714      MOVE '0'              TO X.                                  ECS025
00715      PERFORM 0460-PRT-LINE THRU 0470-PRT-EXIT.                    ECS025
00716      MOVE HD8              TO P-DATA.                             ECS025
00717      MOVE ' '              TO X.                                  ECS025
00718      PERFORM 0460-PRT-LINE THRU 0470-PRT-EXIT.                    ECS025
00719      MOVE ' '              TO X.                                  ECS025
00720      MOVE SPACES           TO P-DATA.                             ECS025
00721      PERFORM 0460-PRT-LINE THRU 0470-PRT-EXIT.                    ECS025
00722      MOVE B1               TO X1.                                 ECS025
00723                                                                   ECS025
00724  0350-STATE-LOOP.                                                 ECS025
00725      IF ST-ID (X1) = '*'                                          ECS025
00726          GO TO 0380-ST-PRNT.                                      ECS025
00727                                                                   ECS025
00728  0360-ST-XIT.                                                     ECS025
00729      ADD B1 TO X1.                                                ECS025
00730      IF X1 NOT = B55                                              ECS025
00731          GO TO 0350-STATE-LOOP.                                   ECS025
00732                                                                   ECS025
00733      GO TO 0390-STATE-TOTALS.                                     ECS025
00734                                                                   ECS025
00735  EJECT                                                            ECS025
00736  0380-ST-PRNT.                                                    ECS025
00737      MOVE SPACES           TO DTL.                                ECS025
00738      MOVE ST-TOTALS (X1)   TO ST-TOTALS (100).                    ECS025
00739      MOVE X1               TO W-ST.                               ECS025
00740      MOVE 'TOTALS FOR  '   TO P-ACCT.                             ECS025
00741      MOVE STATE-SUB (X1)   TO P-SUB.                              ECS025
00742      MOVE STATE-PIC (X1)   TO P-PIC.                              ECS025
00743      MOVE ST-LP (100)      TO P-LPRM.                             ECS025
00744      MOVE ST-LC (100)      TO P-LCOM.                             ECS025
00745      MOVE ST-AP (100)      TO P-APRM.                             ECS025
00746      MOVE ST-AC (100)      TO P-ACOM.                             ECS025
00747      MOVE ST-ND (100)      TO P-NET.                              ECS025
00748      MOVE ' '              TO X.                                  ECS025
00749      MOVE DTL              TO PRT.                                ECS025
00750      PERFORM 0460-PRT-LINE THRU 0470-PRT-EXIT.                    ECS025
00751      MOVE SPACES           TO DTL PRT.                            ECS025
00752      ADD ST-LP (100)       TO S-LP.                               ECS025
00753      ADD ST-LC (100)       TO S-LC.                               ECS025
00754      ADD ST-AP (100)       TO S-AP.                               ECS025
00755      ADD ST-AC (100)       TO S-AC.                               ECS025
00756      ADD ST-ND (100)       TO S-ND.                               ECS025
00757      GO TO 0360-ST-XIT.                                           ECS025
00758                                                                   ECS025
00759  0390-STATE-TOTALS.                                               ECS025
00760      MOVE SPACES           TO DTL.                                ECS025
00761      MOVE 'GROUP  TOTALS'  TO P-NAME.                             ECS025
00762      MOVE S-LP             TO P-LPRM.                             ECS025
00763      MOVE S-LC             TO P-LCOM.                             ECS025
00764      MOVE S-AP             TO P-APRM.                             ECS025
00765      MOVE S-AC             TO P-ACOM.                             ECS025
00766      MOVE S-ND             TO P-NET.                              ECS025
00767      MOVE '0'              TO X.                                  ECS025
00768      MOVE DTL              TO PRT.                                ECS025
00769      PERFORM 0460-PRT-LINE THRU 0470-PRT-EXIT.                    ECS025
00770      MOVE SPACES           TO DTL PRT.                            ECS025
00771      ADD S-LP              TO CA-LPRM.                            ECS025
00772      ADD S-LC              TO CA-LCOM.                            ECS025
00773      ADD S-AP              TO CA-APRM.                            ECS025
00774      ADD S-AC              TO CA-ACOM.                            ECS025
00775      ADD S-ND              TO CA-NET.                             ECS025
00776      MOVE SPACES           TO STATE-TOTAL-AREA.                   ECS025
00777      MOVE ZEROS            TO S-LP  S-LC  S-AP  S-AC  S-ND.          CL**4
00778                                                                   ECS025
00779  0400-C-B-X.                                                      ECS025
00780      EXIT.                                                        ECS025
00781                                                                   ECS025
00782  0410-CARR-XX.                                                    ECS025
00783      IF THS-CARR = AM-CARRIER                                     ECS025
00784         GO TO 0170-MAIN-CHECK                                     ECS025
00785          ELSE                                                     ECS025
00786          GO TO 0430-CARRIER-TOTALS.                               ECS025
00787                                                                   ECS025
00788  EJECT                                                            ECS025
00789  0430-CARRIER-TOTALS.                                             ECS025
00790      MOVE SPACES           TO DTL.                                ECS025
00791      MOVE 'CARRIER TOTALS' TO P-NAME.                             ECS025
00792      MOVE CA-LPRM          TO P-LPRM.                             ECS025
00793      MOVE CA-LCOM          TO P-LCOM.                             ECS025
00794      MOVE CA-APRM          TO P-APRM.                             ECS025
00795      MOVE CA-ACOM          TO P-ACOM.                             ECS025
00796      MOVE CA-NET           TO P-NET.                              ECS025
00797      MOVE '0'              TO X.                                  ECS025
00798      MOVE DTL              TO PRT.                                ECS025
00799      PERFORM 0460-PRT-LINE THRU 0470-PRT-EXIT.                    ECS025
00800      MOVE SPACES           TO DTL PRT.                            ECS025
00801      MOVE ZEROS            TO CA-LPRM CA-LCOM CA-APRM CA-ACOM     ECS025
00802                               CA-NET.                             ECS025
00803                                                                   ECS025
00804  0440-CARR-XIT.  EXIT.                                            ECS025
00805                                                                   ECS025
00806  0450-C-B-XX.                                                     ECS025
00807      GO TO 0170-MAIN-CHECK.                                       ECS025
00808                                                                   ECS025
00809  0460-PRT-LINE.                                                   ECS025
00810                              COPY ELCPRT2.                        ECS025
00811  0470-PRT-EXIT.                                                   ECS025
00812      EXIT.                                                        ECS025
00813                                                                   ECS025
00814  EJECT                                                            ECS025
00815  0490-LOCATE-STATE.                                               ECS025
00816      MOVE ZEROS TO CLAS-INDEXS.                                   ECS025
00817                                                                   ECS025
00818  0500-LOCATE-LOOP.                                                ECS025
00819      ADD +1 TO CLAS-INDEXS.                                       ECS025
00820      IF WK-ST NOT = STATE-SUB (CLAS-INDEXS)                       ECS025
00821         GO TO 0500-LOCATE-LOOP.                                   ECS025
00822                                                                   ECS025
00823      MOVE CLAS-INDEXS TO W-ST.                                    ECS025
00824                                                                   ECS025
00825  0510-LOCATE-XIT.  EXIT.                                          ECS025
00826                                                                   ECS025
00827  0520-END-RTN.                                                    ECS025
00828      MOVE HIGH-VALUES TO THIS-CTL.                                ECS025
00829      PERFORM 0230-MAIN-BREAK THRU 0270-M-B-X.                     ECS025
00830      PERFORM 0300-STATE-BREAK THRU 0310-S-B-X.                    ECS025
00831      PERFORM 0340-COMPANY-BREAK THRU 0400-C-B-X.                  ECS025
00832      PERFORM 0430-CARRIER-TOTALS THRU 0440-CARR-XIT.              ECS025
00833                                                                   ECS025
00834      CLOSE RE-RECAP  AM-MSTR  PRNTR.                              ECS025
00835                                                                   ECS025
00836  0530-FICH-CLOSE.                                                 ECS025
00837                              COPY ELCPRTC.                        ECS025
00838  0540-EOJ-EXIT.                                                   ECS025
00839                                                                   ECS025
00840      GOBACK.                                                      ECS025
00841                                                                   ECS025
00842  EJECT                                                            ECS025
00843  0560-PRINT-DETAIL-LINE.                                          ECS025
00844      IF LN-CT GREATER THAN LN-MX                                  ECS025
00845          PERFORM 0240-PT-HDNG.                                    ECS025
00846                                                                   ECS025
00847      MOVE DE-ACCOUNT           TO D-AM-ACCOUNT.                   ECS025
00848      MOVE DE-EFF               TO WK-DATE-EFF.                    ECS025
00849      MOVE WK-YY                TO D-EFF-YR.                       ECS025
00850      MOVE WK-MM                TO D-EFF-MO.                       ECS025
00851      MOVE WK-DD                TO D-EFF-DA.                       ECS025
00852      MOVE '-'                  TO D-MIN-1  D-MIN-2.               ECS025
00853      MOVE DE-CERT              TO D-CERT-NO.                      ECS025
00854                                                                   ECS025
00855      IF DE-CANCEL MOVE 'CANCEL' TO D-TYPE ELSE                    ECS025
00856          MOVE 'ISSUE '         TO D-TYPE.                         ECS025
00857                                                                   ECS025
00858      MOVE DE-LF-TERM           TO D-LF-TERM.                      ECS025
00859                                                                   ECS025
00860      IF DE-LF-TYPE = ZEROS                                        ECS025
00861          MOVE SPACES           TO D-L-TYPE                        ECS025
00862      ELSE                                                         ECS025
00863          MOVE DE-LF-TYPE       TO D-L-TYPE.                       ECS025
00864                                                                   ECS025
00865      MOVE L-PREM               TO D-LIFE-PR.                      ECS025
00866      MOVE D-LC                 TO D-LIFE-CM.                      ECS025
00867      MOVE DE-AH-TERM           TO D-AH-TERM.                      ECS025
00868                                                                   ECS025
00869      IF DE-AH-TYPE = ZEROS                                        ECS025
00870          MOVE SPACES           TO D-AH-TYPE                       ECS025
00871      ELSE                                                         ECS025
00872          MOVE DE-AH-TYPE       TO D-AH-TYPE.                      ECS025
00873                                                                   ECS025
00874      MOVE A-PREM               TO D-AH-PREM.                      ECS025
00875      MOVE D-AC                 TO D-AH-CM.                        ECS025
00876      COMPUTE DET-NET-DUE = ((L-PREM + A-PREM) -                   ECS025
00877          (D-LC + D-AC)).                                          ECS025
00878                                                                   ECS025
00879      MOVE DET-NET-DUE          TO D-NET-DUE.                      ECS025
00880      MOVE PRT-DETAIL           TO P-DATA.                         ECS025
00881      MOVE ' '                  TO X.                              ECS025
00882      PERFORM 0460-PRT-LINE THRU 0470-PRT-EXIT.                    ECS025
00883      ADD 1 TO LN-CT.                                              ECS025
00884      MOVE +0                   TO D-LC  D-AC.                     ECS025
00885                                                                   ECS025
00886  0570-LINE-EXIT.                                                  ECS025
00887      EXIT.                                                        ECS025
00888                                                                   ECS025
00889  ABEND-PGM SECTION.                                               ECS025
00890                              COPY ELCABEND.                       ECS025
00891 /                                                                 ECS025
00892  LCP-WRITE-POS-PRT SECTION.                                       ECS025
00893      IF LCP-ASA = '+'                                             ECS025
00894          WRITE PRT AFTER 0 LINE                                   ECS025
00895      ELSE                                                         ECS025
00896      IF LCP-ASA = ' '                                             ECS025
00897          WRITE PRT AFTER ADVANCING 1 LINE                         ECS025
00898      ELSE                                                         ECS025
00899      IF LCP-ASA = '0'                                             ECS025
00900          WRITE PRT AFTER ADVANCING 2 LINE                         ECS025
00901      ELSE                                                         ECS025
00902      IF LCP-ASA = '-'                                             ECS025
00903          WRITE PRT AFTER ADVANCING 3 LINE                         ECS025
00904      ELSE                                                         ECS025
00905      IF LCP-ASA = '1'                                             ECS025
00906          WRITE PRT AFTER ADVANCING PAGE                           ECS025
00907      ELSE                                                         ECS025
00908      IF LCP-ASA = '2'                                             ECS025
00909          WRITE PRT AFTER ADVANCING LCP-CH2                        ECS025
00910      ELSE                                                         ECS025
00911      IF LCP-ASA = '3'                                             ECS025
00912          WRITE PRT AFTER ADVANCING LCP-CH3                        ECS025
00913      ELSE                                                         ECS025
00914      IF LCP-ASA = '4'                                             ECS025
00915          WRITE PRT AFTER ADVANCING LCP-CH4                        ECS025
00916      ELSE                                                         ECS025
00917      IF LCP-ASA = '5'                                             ECS025
00918          WRITE PRT AFTER ADVANCING LCP-CH5                        ECS025
00919      ELSE                                                         ECS025
00920      IF LCP-ASA = '6'                                             ECS025
00921          WRITE PRT AFTER ADVANCING LCP-CH6                        ECS025
00922      ELSE                                                         ECS025
00923      IF LCP-ASA = '7'                                             ECS025
00924          WRITE PRT AFTER ADVANCING LCP-CH7                        ECS025
00925      ELSE                                                         ECS025
00926      IF LCP-ASA = '8'                                             ECS025
00927          WRITE PRT AFTER ADVANCING LCP-CH8                        ECS025
00928      ELSE                                                         ECS025
00929      IF LCP-ASA = '9'                                             ECS025
00930          WRITE PRT AFTER ADVANCING LCP-CH9                        ECS025
00931      ELSE                                                         ECS025
00932      IF LCP-ASA = 'A'                                             ECS025
00933          WRITE PRT AFTER ADVANCING LCP-CH10                       ECS025
00934      ELSE                                                         ECS025
00935      IF LCP-ASA = 'B'                                             ECS025
00936          WRITE PRT AFTER ADVANCING LCP-CH11                       ECS025
00937      ELSE                                                         ECS025
00938      IF LCP-ASA = 'C'                                             ECS025
00939          WRITE PRT AFTER ADVANCING LCP-CH12                       ECS025
00940      ELSE                                                         ECS025
00941      IF LCP-ASA = 'V'                                             ECS025
00942          WRITE PRT AFTER ADVANCING LCP-P01                        ECS025
00943      ELSE                                                         ECS025
00944      IF LCP-ASA = 'W'                                             ECS025
00945          WRITE PRT AFTER ADVANCING LCP-P02                        ECS025
00946      ELSE                                                         ECS025
00947      DISPLAY 'ASA CODE ERROR'.                                    ECS025
00948  LCP-WRITE-END-PRT.                                               ECS025
00949      EXIT.                                                        ECS025
