00001  IDENTIFICATION DIVISION.                                         03/09/98
00002                                                                   EL544
00003  PROGRAM-ID.                 EL544 .                                 LV003
00004 *              PROGRAM CONVERTED BY                               EL544
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL544
00006 *              CONVERSION DATE 03/05/96 15:31:37.                 EL544
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL544
00008 *                            VMOD=2.003.                          EL544
00009                                                                   EL544
00010 *AUTHOR. LOGIC, INC.                                              EL544
00011 *        DALLAS, TEXAS.                                           EL544
00012                                                                   EL544
00013 *SECURITY.   *****************************************************   CL**3
00014 *            *                                                   *   CL**3
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**3
00016 *            *                                                   *   CL**3
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**3
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**3
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**3
00020 *            *                                                   *   CL**3
00021 *            *****************************************************   CL**3
00022                                                                      CL**3
00023 *REMARKS.  READS MONTH END BALANCES PRIOR TO RUNNING FOLLOWING    EL544
00024 *          MONTH, TO ENSURE EVERYTHING NECESSARY HAS BEEN DONE.   EL544
00025                                                                   EL544
070714******************************************************************
070714*                   C H A N G E   L O G
070714*
070714* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
070714*-----------------------------------------------------------------
070714*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
070714* EFFECTIVE    NUMBER
070714*-----------------------------------------------------------------
070714* 070714  CR2013060600001  PEMA  AUTOMATE MONTH END BALANCING
070714******************************************************************
00026  ENVIRONMENT DIVISION.                                            EL544
00027  CONFIGURATION SECTION.                                           EL544
00028  SPECIAL-NAMES.                                                   EL544
00029      C02 IS LCP-CH2                                               EL544
00030      C03 IS LCP-CH3                                               EL544
00031      C04 IS LCP-CH4                                               EL544
00032      C05 IS LCP-CH5                                               EL544
00033      C06 IS LCP-CH6                                               EL544
00034      C07 IS LCP-CH7                                               EL544
00035      C08 IS LCP-CH8                                               EL544
00036      C09 IS LCP-CH9                                               EL544
00037      C10 IS LCP-CH10                                              EL544
00038      C11 IS LCP-CH11                                              EL544
00039      C12 IS LCP-CH12                                              EL544
00040      S01 IS LCP-P01                                               EL544
00041      S02 IS LCP-P02.                                              EL544
00042  INPUT-OUTPUT SECTION.                                            EL544
00043  FILE-CONTROL.                                                    EL544
00044      SELECT CRMEBL ASSIGN SYS025-FBA1-CRMEBL                      EL544
00045                           ORGANIZATION INDEXED                    EL544
00046                           ACCESS SEQUENTIAL                       EL544
00047                           RECORD KEY MEB-KEY.                     EL544
00048                                                                   EL544
00049      SELECT PRINT-FILE ASSIGN SYS008-UR-1403-S-SYS008.            EL544
00050                                                                   EL544
00051      EJECT                                                        EL544
00052  DATA DIVISION.                                                   EL544
00053  FILE SECTION.                                                    EL544
00054                                                                   EL544
00055  FD  CRMEBL.                                                      EL544
00056  01  MO-END-BALANCES.                                             EL544
00057      12  FILLER                  PIC XX.                          EL544
00058      12  MEB-KEY.                                                 EL544
00059          16  MEB-CO              PIC XXX.                         EL544
00060          16  MEB-MOYR            PIC 9999 COMP.                   EL544
00061      12  FILLER                  PIC X(758).                      EL544
00062                                                                   EL544
00063  FD  PRINT-FILE COPY ELCPRTFD.                                    EL544
00064      EJECT                                                        EL544
00065  WORKING-STORAGE SECTION.                                         EL544
00066  77  LCP-ASA                       PIC X.                         EL544
00067  77  FILLER  PIC X(32) VALUE '********************************'.  EL544
00068  77  FILLER  PIC X(32) VALUE '*    EL544 WORKING-STORAGE     *'.  EL544
00069  77  FILLER  PIC X(32) VALUE '************V/M 2.003 **********'.  EL544
00070                                                                   EL544
00071  77  WS-ZERO                     PIC S9      COMP-3 VALUE +0.     EL544
00072  77  WS-RETURN-CODE              PIC S9999   COMP   VALUE +0.     EL544
00073  77  WS-ABEND-FILE-STATUS        PIC XX             VALUE ZERO.   EL544
00074  77  WS-ABEND-MESSAGE            PIC X(80)          VALUE SPACES. EL544
00075  77  X                           PIC X.                           EL544
00076  77  OLC-REPORT-NAME             PIC X(6)           VALUE 'EL544'.EL544
00077                                                                   EL544
00078  01  WS-ACCEPT-DATE.                                              EL544
00079      12  WS-AD-YY                    PIC 99.                      EL544
00080      12  WS-AD-MM                    PIC 99.                      EL544
00081      12  WS-AD-DD                    PIC 99.                      EL544
00082                                                                   EL544
00083  01  WS-CURRENT-DATE.                                             EL544
00084      12  WS-CD-MM                    PIC 99.                      EL544
00085      12  FILLER                      PIC X      VALUE '/'.        EL544
00086      12  WS-CD-DD                    PIC 99.                      EL544
00087      12  FILLER                      PIC X      VALUE '/'.        EL544
00088      12  WS-CD-YY                    PIC 99.                      EL544
00089                                                                   EL544
00090  01  WS-TIME-OF-DAY.                                              EL544
00091      12  WS-TIME                     PIC 9(6).                    EL544
00092      12  WS-HUN-SEC                  PIC 99.                      EL544
00093                                                                   EL544
00094  01  PGM-SUB                     PIC S999 COMP      VALUE +544.   EL544
00095                                                                   EL544
00096  01  DTE-REPLACEMENTS.                                            EL544
00097      12  DTE-VSAM-FLAGS VALUE ZERO.                               EL544
00098          16  DTE-F-1             PIC X.                           EL544
00099          16  DTE-F-2             PIC X.                           EL544
00100      12  DTE-ABEND-WORK.                                          EL544
00101          16  DTE-ABEND-CD-1      PIC XX VALUE SPACE.              EL544
00102          16  DTE-ABEND-CD-2      PIC XX VALUE SPACE.              EL544
00103      12  DTE-TOT-LINES           PIC S9(8) COMP.                  EL544
00104      12  DTE-FICH                PIC X.                           EL544
00105      12  DTE-PRT-OPT             PIC 9.                           EL544
00106      12  REPT-OPEN               PIC X VALUE SPACE.               EL544
00107                                                                   EL544
00108  COPY ERCMEBL.                                                    EL544
00109                                                                   EL544
00110  01  HD-1.                                                        EL544
00111      12  FILLER                  PIC X(26) VALUE SPACE.           EL544
00112      12  FILLER                  PIC X(28) VALUE                  EL544
00113          'MONTH-END ACTIVITY/INVENTORY'.                          EL544
00114      12  FILLER                  PIC X(20) VALUE SPACE.           EL544
00115      12  FILLER                  PIC X(6) VALUE ' EL544'.         EL544
00116                                                                   EL544
00117  01  HD-2.                                                        EL544
00118      12  FILLER                  PIC X(38) VALUE SPACE.           EL544
00119      12  HD2-CO                  PIC X(3).                        EL544
00120      12  FILLER                  PIC X(31) VALUE SPACE.           EL544
00121      12  HD2-RUN                 PIC X(8).                        EL544
00122                                                                   EL544
00123  01  HD-3.                                                        EL544
00124      12  FILLER                  PIC X(37) VALUE SPACE.           EL544
00125      12  HD3-DATE.                                                EL544
00126          16  HD3-MO              PIC 99.                          EL544
00127          16  HD3-DSH             PIC X.                           EL544
00128          16  HD3-YR              PIC 99.                          EL544
00129      12  FILLER                  PIC X(31) VALUE SPACE.           EL544
00130      12  FILLER                  PIC X(5) VALUE 'PAGE'.           EL544
00131      12  HD3-PG                  PIC Z9.                          EL544
00132                                                                   EL544
00133  01  HD-4.                                                        EL544
00134      12  FILLER                  PIC X(40) VALUE                  EL544
00135          'PREMIUMS/REFUNDS            -------- PRE'.              EL544
00136      12  FILLER                  PIC X(40) VALUE                  EL544
00137          'MIUMS ------- --------- REFUNDS -------'.               EL544
00138                                                                   EL544
00139  01  HD-5.                                                        EL544
00140      12  FILLER                  PIC X(36) VALUE SPACE.           EL544
00141      12  FILLER                  PIC XXXX VALUE 'LIFE'.           EL544
00142      12  FILLER                  PIC X(40) VALUE                  EL544
00143          '          A/H         LIFE          A/H'.               EL544
00144  01  HD-6.                                                        EL544
00145      12  FILLER                  PIC X(40) VALUE                  EL544
00146          'NET WRITTEN                 ------ NET W'.              EL544
00147      12  FILLER                  PIC X(13) VALUE                  EL544
00148          'RITTEN ------'.                                         EL544
00149                                                                   EL544
00150  01  HD-7.                                                        EL544
00151      12  FILLER                  PIC X(40) VALUE                  EL544
00152          'COMMISSIONS                 --- ACCOUNT'.               EL544
00153      12  FILLER                  PIC X(40) VALUE                  EL544
00154          'COMMISSION -- -- OVERRIDE COMMISSION --'.               EL544
00155                                                                   EL544
00156  01  HD-8.                                                        EL544
00157      12  FILLER                  PIC X(40) VALUE                  EL544
00158          'CLAIMS                      ---------PAY'.              EL544
00159      12  FILLER                  PIC X(40) VALUE                  EL544
00160          'MENTS ------- ------- RESERVES -------'.                EL544
00161                                                                   EL544
00162  01  HD-8A.                                                       EL544
00163      12  FILLER                  PIC X(40) VALUE                  EL544
00164          'RETROS                             TOTAL'.              EL544
00165                                                                   EL544
00166  01  HD-9.                                                        EL544
00167      12  FILLER                  PIC X(28) VALUE SPACE.           EL544
00168      12  FILLER                  PIC X(23) VALUE                  EL544
00169          'MONTH-END DISCREPANCIES'.                               EL544
00170      12  FILLER                  PIC X(23) VALUE SPACE.           EL544
00171      12  FILLER                  PIC X(6) VALUE ' EL544'.         EL544
00172                                                                   EL544
00173  01  HD-A.                                                        EL544
00174      12  FILLER                  PIC X(29) VALUE SPACE.           EL544
00175      12  FILLER                  PIC X(22) VALUE                  EL544
00176          'TASK DURATION ANALYSIS'.                                EL544
00177      12  FILLER                  PIC X(23) VALUE SPACE.           EL544
00178      12  FILLER                  PIC X(6) VALUE ' EL544'.         EL544
00179                                                                   EL544
00180  01  HD-B.                                                        EL544
00181      12  FILLER                  PIC X(20) VALUE SPACE.           EL544
00182      12  FILLER                  PIC X(6) VALUE 'RUN ON'.         EL544
00183                                                                   EL544
00184  01  HD-C.                                                        EL544
00185      12  FILLER                  PIC X(10) VALUE SPACE.           EL544
00186      12  FILLER                  PIC X(30) VALUE                  EL544
00187          'JOB #    MO DA YR    STARTED '.                         EL544
00188      12  FILLER                  PIC X(30) VALUE                  EL544
00189          ' FINISHED   DURATION     RUN #'.                        EL544
00190                                                                   EL544
00191  01  DT-1.                                                        EL544
00192      12  D1-TITLE                PIC X(20).                       EL544
00193      12  FILLER                  PIC X(8).                        EL544
00194      12  D1-TTL1                 PIC Z,ZZZ,ZZZ.ZZ-.               EL544
00195      12  D1-TTL2                 PIC Z,ZZZ,ZZZ.ZZ-.               EL544
00196      12  D1-TTL3                 PIC Z,ZZZ,ZZZ.ZZ-.               EL544
00197      12  D1-TTL4                 PIC Z,ZZZ,ZZZ.ZZ-.               EL544
00198                                                                   EL544
00199  01  DT-2.                                                        EL544
00200      12  FILLER                  PIC X(10).                       EL544
00201      12  D2-JOBNO                PIC X(9).                        EL544
00202      12  D2-RUNMO                PIC Z9.                          EL544
00203      12  D2-SL1                  PIC X.                           EL544
00204      12  D2-RUNDA                PIC 99.                          EL544
00205      12  D2-SL2                  PIC X.                           EL544
00206      12  D2-RUNYR                PIC 99.                          EL544
00207      12  FILLER                  PIC XXX.                         EL544
00208      12  D2-BGHR                 PIC 99.                          EL544
00209      12  D2-SC1                  PIC X.                           EL544
00210      12  D2-BGMN                 PIC 99.                          EL544
00211      12  D2-SC2                  PIC X.                           EL544
00212      12  D2-BGSC                 PIC 99.                          EL544
00213      12  FILLER                  PIC XXX.                         EL544
00214      12  D2-FNHR                 PIC 99.                          EL544
00215      12  D2-SC3                  PIC X.                           EL544
00216      12  D2-FNMN                 PIC 99.                          EL544
00217      12  D2-SC4                  PIC X.                           EL544
00218      12  D2-FNSC                 PIC 99.                          EL544
00219      12  FILLER                  PIC XXX.                         EL544
00220      12  D2-DUHR                 PIC 99.                          EL544
00221      12  D2-SC5                  PIC X.                           EL544
00222      12  D2-DUMN                 PIC 99.                          EL544
00223      12  D2-SC6                  PIC X.                           EL544
00224      12  D2-DUSC                 PIC 99.                          EL544
00225      12  FILLER                  PIC X(6).                        EL544
00226      12  D2-RRNO                 PIC ZZZ.                         EL544
00227                                                                   EL544
00228  01  ME-STATUS.                                                   EL544
00229      12  ME-STAT-1               PIC X.                           EL544
00230      12  ME-STAT-2               PIC X.                           EL544
00231                                                                   EL544
00232  01  PGCT                        PIC 99 VALUE ZERO.               EL544
00233  01  LNCT                        PIC 99 VALUE 80.                 EL544
00234                                                                   EL544
00235  01  RFMT.                                                        EL544
00236      12  RFYRDA                  PIC 99.                          EL544
00237      12  RFMOMO                  PIC 99.                          EL544
00238      12  RFDAYR                  PIC 99.                          EL544
00239                                                                   EL544
00240  01  WK-AREAS.                                                    EL544
00241      12  WK1                     PIC S9(7)V99 COMP-3.             EL544
00242      12  WK2                     PIC S9(7)V99 COMP-3.             EL544
00243      12  WK3                     PIC 9(7)V99  COMP-3.             EL544
00244      12  H-MASK                  PIC 9(10).                       EL544
00245      12  HR-MASK REDEFINES H-MASK.                                EL544
00246          16  HRM-1               PIC 99.                          EL544
00247          16  HRM-2               PIC 9999.                        EL544
00248          16  HRM-3               PIC 9999.                        EL544
00249      12  P-MASK                  PIC 9(10).                       EL544
00250      12  PR-MASK REDEFINES P-MASK.                                EL544
00251          16  PRM-1               PIC 99.                          EL544
00252          16  PRM-2               PIC 9999.                        EL544
00253          16  PRM-3               PIC 9999.                        EL544
00254      12  CODED-YRMO              PIC S9(5) COMP-3.                EL544
00255      12  PRIOR-HIST-RECS         PIC S9(7) COMP-3.                EL544
00256      12  PRIOR-CERTS             PIC S9(7) COMP-3.                EL544
00257      12  ERRX                    PIC 9999  COMP.                  EL544
00258      12  ERR-TBL                 PIC X(10) OCCURS 50.             EL544
00259      12  ERR-AMT                 PIC 9(7)V99 OCCURS 50.           EL544
00260      12  CURRX                   PIC 9999 COMP.                   EL544
00261      12  ETBL-1-VALS.                                             EL544
00262          16  FILLER PIC X(24) VALUE 'LIFE PREMIUMS           '.   EL544
00263          16  FILLER PIC X(24) VALUE 'A/H PREMIUMS            '.   EL544
00264          16  FILLER PIC X(24) VALUE 'LIFE REFUNDS            '.   EL544
00265          16  FILLER PIC X(24) VALUE 'A/H REFUNDS             '.   EL544
00266          16  FILLER PIC X(24) VALUE 'LIFE NET WRITTEN        '.   EL544
00267          16  FILLER PIC X(24) VALUE 'A/H NET WRITTEN         '.   EL544
00268          16  FILLER PIC X(24) VALUE 'LIFE ACCT COMMISSIONS   '.   EL544
00269          16  FILLER PIC X(24) VALUE 'A/H ACCT COMMISSIONS    '.   EL544
00270          16  FILLER PIC X(24) VALUE 'LIFE OVERRIDES          '.   EL544
00271          16  FILLER PIC X(24) VALUE 'A/H OVERRIDES           '.   EL544
00272          16  FILLER PIC X(24) VALUE 'LIFE CLAIMS             '.   EL544
00273          16  FILLER PIC X(24) VALUE 'A/H CLAIMS              '.   EL544
00274          16  FILLER PIC X(24) VALUE 'LIFE RESERVES           '.   EL544
00275          16  FILLER PIC X(24) VALUE 'A/H RESERVES            '.   EL544
00276          16  FILLER PIC X(24) VALUE 'CERTIFICATES            '.   EL544
00277          16  FILLER PIC X(24) VALUE 'CLAIM HISTORY RECORDS   '.   EL544
00278          16  FILLER PIC X(24) VALUE 'PAYMENTS AND ADJUSTMENTS'.   EL544
00279          16  FILLER PIC X(24) VALUE 'RETRO PAYMENTS          '.   EL544
00280          16  FILLER PIC X(24) VALUE 'NET ACCT COMMISSIONS    '.   EL544
00281          16  FILLER PIC X(24) VALUE 'NET OVERRIDES           '.   EL544
00282          16  FILLER PIC X(24) VALUE 'NET TOTAL PREMIUMS      '.   EL544
00283          16  FILLER PIC X(24) VALUE 'TOTAL RESERVES'.             EL544
00284      12  ETBL-1-ITM REDEFINES ETBL-1-VALS PIC X(24) OCCURS 22.    EL544
00285      12  ETBL-2-VALS.                                             EL544
00286          16  FILLER              PIC X(6) VALUE 'ECS010'.         EL544
00287          16  FILLER              PIC X(6) VALUE 'ECS018'.         EL544
00288          16  FILLER              PIC X(6) VALUE 'ECS019'.         EL544
00289          16  FILLER              PIC X(6) VALUE 'ECS030'.         EL544
00290          16  FILLER              PIC X(6) VALUE 'ECS032'.         EL544
00291          16  FILLER              PIC X(6) VALUE 'ECS035'.         EL544
00292          16  FILLER              PIC X(6) VALUE 'ECS038'.         EL544
00293          16  FILLER              PIC X(6) VALUE 'ECS041'.         EL544
00294          16  FILLER              PIC X(6) VALUE 'ECS050'.         EL544
00295          16  FILLER              PIC X(6) VALUE 'ECS061'.         EL544
00296          16  FILLER              PIC X(6) VALUE 'ECS080'.         EL544
00297          16  FILLER              PIC X(6) VALUE 'EL315 '.         EL544
00298          16  FILLER              PIC X(6) VALUE 'EL331 '.         EL544
00299          16  FILLER              PIC X(6) VALUE 'EL341 '.         EL544
00300          16  FILLER              PIC X(6) VALUE 'EL501 '.         EL544
00301          16  FILLER              PIC X(6) VALUE 'EL509 '.         EL544
00302          16  FILLER              PIC X(6) VALUE 'EL522 '.         EL544
00303          16  FILLER              PIC X(6) VALUE 'EL524 '.         EL544
00304          16  FILLER              PIC X(6) VALUE 'EL525 '.         EL544
00305      12  ETBL-2-ITM REDEFINES ETBL-2-VALS PIC X(6) OCCURS 19.     EL544
00306      12  ETBL-3-VALS.                                             EL544
00307          16  FILLER PIC X(18) VALUE '(PROCESSED OR NOT)'.         EL544
00308          16  FILLER PIC X(18) VALUE '(PROCESSED ONLY)  '.         EL544
00309          16  FILLER PIC X(18) VALUE '(PRIOR MONTH OUT) '.         EL544
00310          16  FILLER PIC X(18) VALUE '(STANDARD)        '.         EL544
00311          16  FILLER PIC X(18) VALUE '(RECALCULATED)    '.         EL544
00312      12  ETBL-3-ITM REDEFINES ETBL-3-VALS PIC X(18) OCCURS 5.     EL544
00313      12  SPL-MSG-1.                                               EL544
00314          16  FILLER              PIC X(10) VALUE SPACE.           EL544
00315          16  FILLER              PIC X(40) VALUE                  EL544
00316          'YOU ARE ABSOLUTELY PERFECT. THANK YOU.  '.              EL544
00317      12  SPL-MSG-2.                                               EL544
00318          16  FILLER              PIC X(10) VALUE SPACE.           EL544
00319          16  FILLER              PIC X(40) VALUE                  EL544
00320          'THERE ARE NO DISCREPANCIES.             '.              EL544
00321      12  DECODE-P.                                                EL544
00322          16  DP-1                PIC 99.                          EL544
00323          16  DP-2                PIC 99.                          EL544
00324          16  DP-3                PIC 99.                          EL544
00325          16  DP-4                PIC 99.                          EL544
00326          16  DP-5                PIC 99.                          EL544
00327      12  X1                      PIC 9999 COMP.                   EL544
00328      12  X2                      PIC 9999 COMP.                   EL544
00329      12  X3                      PIC 9999 COMP.                   EL544
00330      12  SVTOP                   PIC 9999 COMP.                   EL544
00331      12  DUR-DATA.                                                EL544
00332          16  DH-JOB              PIC X(6).                        EL544
00333          16  DH-RUN              PIC 9(6).                        EL544
00334          16  DH-DTS REDEFINES DH-RUN.                             EL544
00335              20  DH-RUNMO        PIC 99.                          EL544
00336              20  DH-RUNDA        PIC 99.                          EL544
00337              20  DH-RUNYR        PIC 99.                          EL544
00338          16  DH-START            PIC 9(6).                        EL544
00339          16  DH-STS REDEFINES DH-START.                           EL544
00340              20  DH-STHR         PIC 99.                          EL544
00341              20  DH-STMN         PIC 99.                          EL544
00342              20  DH-STSC         PIC 99.                          EL544
00343          16  DH-END              PIC 9(6).                        EL544
00344          16  DH-ENS REDEFINES DH-END.                             EL544
00345              20  DH-ENHR         PIC 99.                          EL544
00346              20  DH-ENMN         PIC 99.                          EL544
00347              20  DH-ENSC         PIC 99.                          EL544
00348          16  DH-COUNT            PIC S999 COMP-3.                 EL544
00349      12  SIGN-HR                 PIC S999.                        EL544
00350      12  SIGN-MN                 PIC S999.                        EL544
00351      12  SIGN-SC                 PIC S999.                        EL544
00352      12  EXPND-HR                PIC S999.                        EL544
00353      12  EXPND-MN                PIC S999.                        EL544
00354      12  EXPND-SC                PIC S999.                        EL544
00355      12  WS-CCYY                 PIC 9(04).                       EL544
00356      12  WS-CCYR  REDEFINES WS-CCYY.                              EL544
00357          16  WS-CC               PIC 99.                          EL544
00358          16  WS-YY               PIC 99.                          EL544
00359  01  EXP-LINE.                                                    EL544
00360      12  EXL-1                   PIC X(25).                       EL544
00361      12  FILLER                  PIC X(5) VALUE 'FROM'.           EL544
00362      12  EXL-2                   PIC X(7).                        EL544
00363      12  EXL-3                   PIC X(19).                       EL544
00364      12  FILLER                  PIC X(5) VALUE 'OVER'.           EL544
00365      12  EXL-4                   PIC X(7).                        EL544
00366      12  EXL-5                   PIC X(19).                       EL544
00367      12  FILLER                  PIC XXX  VALUE 'BY'.             EL544
00368      12  EXL-AMT                 PIC Z,ZZZ,ZZZ.99.                EL544
00369  01  CMP-LINE.                                                    EL544
00370      12  CHAR                    PIC X OCCURS 102.                EL544
00371      EJECT                                                        EL544
00372  PROCEDURE DIVISION.                                              EL544
00373  0100-GET-CURR.                                                   EL544
00374      OPEN INPUT CRMEBL                                            EL544
00375          OUTPUT PRINT-FILE.                                       EL544
00376                                                                   EL544
00377  0200-SET-UPS.                                                    EL544
00378      MOVE LOW-VALUE              TO ME-CONTROL-PRIMARY.           EL544
00379                                                                   EL544
00380      ACCEPT WS-ACCEPT-DATE FROM DATE.                             EL544
00381      MOVE WS-AD-YY               TO WS-CD-YY.                     EL544
00382      MOVE WS-AD-MM               TO WS-CD-MM.                     EL544
00383      MOVE WS-AD-DD               TO WS-CD-DD.                     EL544
00384                                                                   EL544
00385  0205-RETURN-A.                                                   EL544
00386      READ CRMEBL  AT END GO TO EOJ.                               EL544
00387                                                                   EL544
00388      IF MEB-CO = '999'                                            EL544
00389          GO TO EOJ.                                               EL544
00390                                                                   EL544
00391      IF ME-CONTROL-PRIMARY = LOW-VALUE                            EL544
00392          MOVE MO-END-BALANCES    TO MONTH-END-BALANCES            EL544
00393          MOVE ZERO               TO PRIOR-CERTS PRIOR-HIST-RECS   EL544
00394          GO TO 0205-RETURN-A.                                     EL544
00395                                                                   EL544
00396      IF MEB-CO NOT = ME-COMPANY                                   EL544
00397          GO TO 0300-PAGE-1.                                       EL544
00398                                                                   EL544
00399  0230-RETURN-C.                                                   EL544
00400      MOVE ME-010-CERT-IN         TO PRIOR-CERTS.                  EL544
00401      MOVE ME-038-RECS-IN         TO PRIOR-HIST-RECS.              EL544
00402                                                                   EL544
00403      IF MEB-CO NOT = ME-COMPANY                                   EL544
00404          MOVE ZERO               TO PRIOR-CERTS  PRIOR-HIST-RECS. EL544
00405                                                                   EL544
00406      MOVE MO-END-BALANCES        TO MONTH-END-BALANCES.           EL544
00407      GO TO 0205-RETURN-A.                                         EL544
00408                                                                   EL544
00409  0300-PAGE-1.                                                     EL544
00410      MOVE HD-1                   TO P-DATA.                       EL544
00411      MOVE  1  TO P-CTL.                                           EL544
00412      PERFORM 1300-PRINT-LINE.                                     EL544
00413                                                                   EL544
00414      MOVE ME-COMPANY             TO HD2-CO.                       EL544
00415      DIVIDE 12 INTO ME-MOYR GIVING WS-CCYY.                       EL544
00416      MOVE WS-YY                  TO HD3-YR.                       EL544
00417      COMPUTE HD3-MO = ME-MOYR - (WS-CCYY * 12).                   EL544
00418                                                                   EL544
00419      IF HD3-MO = ZERO MOVE 12 TO HD3-MO                           EL544
00420          SUBTRACT 1 FROM HD3-YR                                   EL544
00421          IF HD3-YR < 0                                            EL544
00422             ADD 100 TO HD3-YR                                     EL544
00423          END-IF.                                                  EL544
00424                                                                   EL544
00425      MOVE '/'                    TO HD3-DSH.                      EL544
00426                                                                   EL544
00427      MOVE WS-CURRENT-DATE        TO HD2-RUN.                      EL544
00428      MOVE HD-2                   TO P-DATA.                       EL544
00429      MOVE 1                      TO P-CTL.                        EL544
00430      PERFORM 1300-PRINT-LINE.                                     EL544
00431                                                                   EL544
00432      ADD 1 TO PGCT.                                               EL544
00433      MOVE PGCT                   TO HD3-PG.                       EL544
00434      MOVE HD-3                   TO P-DATA.                       EL544
00435      MOVE 1                      TO P-CTL.                        EL544
00436      PERFORM 1300-PRINT-LINE.                                     EL544
00437                                                                   EL544
00438      MOVE HD-4                   TO P-DATA.                       EL544
00439      MOVE ZERO                   TO P-CTL.                        EL544
00440      PERFORM 1300-PRINT-LINE.                                     EL544
00441                                                                   EL544
00442      MOVE HD-5 TO                P-DATA.                          EL544
00443      MOVE 1                      TO P-CTL.                        EL544
00444      PERFORM 1300-PRINT-LINE.                                     EL544
00445                                                                   EL544
00446      MOVE '         FROM ECS010' TO DT-1.                         EL544
00447      MOVE ME-010-PREM-L          TO D1-TTL1.                      EL544
00448      MOVE ME-010-PREM-AH         TO D1-TTL2.                      EL544
00449      MOVE ME-010-REF-L           TO D1-TTL3.                      EL544
00450      MOVE ME-010-REF-AH          TO D1-TTL4.                      EL544
00451      MOVE DT-1                   TO P-DATA.                       EL544
00452      MOVE ZERO                   TO P-CTL.                        EL544
00453      PERFORM 1300-PRINT-LINE.                                     EL544
00454                                                                   EL544
00455      MOVE '              ECS019' TO DT-1.                         EL544
00456      MOVE ME-019-PREM-L          TO D1-TTL1.                      EL544
00457      MOVE ME-019-PREM-AH         TO D1-TTL2.                      EL544
00458      MOVE ME-019-REF-L           TO D1-TTL3.                      EL544
00459      MOVE ME-019-REF-AH          TO D1-TTL4.                      EL544
00460      MOVE DT-1                   TO P-DATA.                       EL544
00461      MOVE 1                      TO P-CTL.                        EL544
00462      PERFORM 1300-PRINT-LINE.                                     EL544
00463                                                                   EL544
00464      MOVE '   TOTAL FROM ECS061' TO DT-1.                         EL544
00465      MOVE ME-061-PREM            TO D1-TTL1.                      EL544
00466      MOVE DT-1                   TO P-DATA.                       EL544
00467      MOVE 1                      TO P-CTL.                        EL544
00468      PERFORM 1300-PRINT-LINE.                                     EL544
00469                                                                   EL544
00470      MOVE '               EL522' TO DT-1.                         EL544
00471      MOVE ME-522-PREM-L          TO D1-TTL1.                      EL544
00472      MOVE ME-522-PREM-AH         TO D1-TTL2.                      EL544
00473      MOVE ME-522-REF-L           TO D1-TTL3.                      EL544
00474      MOVE ME-522-REF-AH          TO D1-TTL4.                      EL544
00475      MOVE DT-1                   TO P-DATA.                       EL544
00476      MOVE 1     TO P-CTL.                                         EL544
00477      PERFORM 1300-PRINT-LINE.                                     EL544
00478                                                                   EL544
00479      MOVE HD-6                   TO P-DATA.                       EL544
00480      MOVE  1                     TO P-CTL.                        EL544
00481      PERFORM 1300-PRINT-LINE.                                     EL544
00482                                                                   EL544
00483      MOVE HD-5                   TO P-DATA.                       EL544
00484      MOVE 1     TO P-CTL.                                         EL544
00485      PERFORM 1300-PRINT-LINE.                                     EL544
00486                                                                   EL544
00487      MOVE 'ISSUES-CANCEL ECS010' TO DT-1.                         EL544
00488      MOVE ME-010-NET-L         TO D1-TTL1.                        EL544
00489      MOVE ME-010-NET-AH          TO D1-TTL2.                      EL544
00490      MOVE DT-1                   TO P-DATA.                       EL544
00491      MOVE ZERO TO P-CTL.                                          EL544
00492      PERFORM 1300-PRINT-LINE.                                     EL544
00493                                                                   EL544
00494      MOVE 'MONTH TO DATE-ECS035' TO DT-1.                         EL544
00495      MOVE ME-035-NET-L           TO D1-TTL1.                      EL544
00496      MOVE ME-035-NET-AH          TO D1-TTL2.                      EL544
00497      MOVE DT-1                   TO P-DATA.                       EL544
00498      MOVE 1                      TO P-CTL.                        EL544
00499      PERFORM 1300-PRINT-LINE.                                     EL544
00500                                                                   EL544
00501      MOVE HD-7                   TO P-DATA.                       EL544
00502      MOVE  1                     TO P-CTL.                        EL544
00503      PERFORM 1300-PRINT-LINE.                                     EL544
00504                                                                   EL544
00505      MOVE HD-5                   TO P-DATA.                       EL544
00506      MOVE 1                      TO P-CTL.                        EL544
00507      PERFORM 1300-PRINT-LINE.                                     EL544
00508                                                                   EL544
00509      MOVE '         FROM ECS010' TO DT-1.                         EL544
00510      MOVE ME-010-COMM-L          TO D1-TTL1.                      EL544
00511      MOVE ME-010-COMM-AH         TO D1-TTL2.                      EL544
00512      MOVE DT-1                   TO P-DATA.                       EL544
00513      MOVE ZERO                   TO P-CTL.                        EL544
00514      PERFORM 1300-PRINT-LINE.                                     EL544
00515                                                                   EL544
00516      MOVE '              ECS018' TO DT-1.                         EL544
070714     compute d1-ttl1 = me-018-comm-y + me-018-comm-1
00517 *    MOVE ME-018-COMM-L          TO D1-TTL1.                      EL544
00518 *    MOVE ME-018-COMM-AH         TO D1-TTL2.                      EL544
070714     compute d1-ttl3 = me-018-ow-y + me-018-ow-1
00519 *    MOVE ME-018-OR-L            TO D1-TTL3.                      EL544
00520 *    MOVE ME-018-OR-AH           TO D1-TTL4.                      EL544
00521      MOVE DT-1                   TO P-DATA.                       EL544
00522      MOVE 1                      TO P-CTL.                        EL544
00523      PERFORM 1300-PRINT-LINE.                                     EL544
00524                                                                   EL544
00525      MOVE '              ECS019' TO DT-1.                         EL544
00526      MOVE ME-019-COMM-L          TO D1-TTL1.                      EL544
00527      MOVE ME-019-COMM-AH         TO D1-TTL2.                      EL544
00528      MOVE ME-019-OR-L            TO D1-TTL3.                      EL544
00529      MOVE ME-019-OR-AH           TO D1-TTL4.                      EL544
00530      MOVE DT-1                   TO P-DATA.                       EL544
00531      MOVE 1                      TO P-CTL.                        EL544
00532      PERFORM 1300-PRINT-LINE.                                     EL544
00533                                                                   EL544
00534      MOVE '   TOTAL FROM ECS061' TO DT-1.                         EL544
00535      MOVE ME-061-COMM            TO D1-TTL1.                      EL544
00536      MOVE ME-061-OR              TO D1-TTL3.                      EL544
00537      MOVE DT-1                   TO P-DATA.                       EL544
00538      MOVE 1                      TO P-CTL.                        EL544
00539      PERFORM 1300-PRINT-LINE.                                     EL544
00540                                                                   EL544
00541      MOVE HD-8                   TO P-DATA.                       EL544
00542      MOVE  1                     TO P-CTL.                        EL544
00543      PERFORM 1300-PRINT-LINE.                                     EL544
00544                                                                   EL544
00545      MOVE HD-5                   TO P-DATA.                       EL544
00546      MOVE 1                      TO P-CTL.                        EL544
00547      PERFORM 1300-PRINT-LINE.                                     EL544
00548                                                                   EL544
00549      MOVE '           ALL EL522' TO DT-1.                         EL544
00550      MOVE ME-522-ALL-CLM-L       TO D1-TTL1.                      EL544
00551      MOVE ME-522-ALL-CLM-AH      TO D1-TTL2.                      EL544
00552      MOVE ME-522-ALL-RSV-L       TO D1-TTL3.                      EL544
00553      MOVE ME-522-ALL-RSV-AH      TO D1-TTL4.                      EL544
00554      MOVE DT-1 TO P-DATA.                                         EL544
00555      MOVE 1     TO P-CTL.                                         EL544
00556      PERFORM 1300-PRINT-LINE.                                     EL544
00557                                                                   EL544
00558      MOVE '               EL524' TO DT-1.                         EL544
00559      MOVE ME-524-CLMS-L          TO D1-TTL1.                      EL544
00560      MOVE ME-524-CLMS-AH         TO D1-TTL2.                      EL544
00561      MOVE ME-524-RESV-L          TO D1-TTL3.                      EL544
00562      MOVE ME-524-RESV-AH         TO D1-TTL4.                      EL544
00563      MOVE DT-1                   TO P-DATA.                       EL544
00564      MOVE 1                      TO P-CTL.                        EL544
00565      PERFORM 1300-PRINT-LINE.                                     EL544
00566                                                                   EL544
00567      MOVE '         FROM ECS010' TO DT-1.                         EL544
00568      MOVE ME-010-PMT-L           TO D1-TTL1.                      EL544
00569      MOVE ME-010-PMT-AH          TO D1-TTL2.                      EL544
00570      MOVE DT-1                   TO P-DATA.                       EL544
00571      MOVE ZERO                   TO P-CTL.                        EL544
00572      PERFORM 1300-PRINT-LINE.                                     EL544
00573                                                                   EL544
00574      MOVE '          GOOD EL522' TO DT-1.                         EL544
00575      MOVE ME-522-PROC-CLM-L      TO D1-TTL1.                      EL544
00576      MOVE ME-522-PROC-CLM-AH     TO D1-TTL2.                      EL544
00577      MOVE ME-522-PROC-RSV-L      TO D1-TTL3.                      EL544
00578      MOVE ME-522-PROC-RSV-AH     TO D1-TTL4.                      EL544
00579      MOVE DT-1                   TO P-DATA.                       EL544
00580      MOVE ZERO                   TO P-CTL.                        EL544
00581      PERFORM 1300-PRINT-LINE.                                     EL544
00582                                                                   EL544
00583      MOVE '              ECS030' TO DT-1.                         EL544
00584      MOVE ME-030-CLMS-L          TO D1-TTL1.                      EL544
00585      MOVE ME-030-CLMS-AH         TO D1-TTL2.                      EL544
00586      MOVE DT-1                   TO P-DATA.                       EL544
00587      MOVE 1                      TO P-CTL.                        EL544
00588      PERFORM 1300-PRINT-LINE.                                     EL544
00589                                                                   EL544
00590      MOVE '              ECS032' TO DT-1.                         EL544
00591      MOVE ME-032-RESV-L          TO D1-TTL3.                      EL544
00592      MOVE ME-032-RESV-AH         TO D1-TTL4.                      EL544
00593      MOVE DT-1                   TO P-DATA.                       EL544
00594      MOVE 1                      TO P-CTL.                        EL544
00595      PERFORM 1300-PRINT-LINE.                                     EL544
00596                                                                   EL544
00597      MOVE '      ALL FROM EL315' TO DT-1.                         EL544
00598      MOVE ME-315-RESV-L          TO D1-TTL3.                      EL544
00599      MOVE ME-315-RESV-AH         TO D1-TTL4.                      EL544
00600      MOVE DT-1                   TO P-DATA.                       EL544
00601      MOVE ZERO                   TO P-CTL.                        EL544
00602      PERFORM 1300-PRINT-LINE.                                     EL544
00603                                                                   EL544
00604      MOVE HD-8A                  TO P-DATA.                       EL544
00605      MOVE  1                     TO P-CTL.                        EL544
00606      PERFORM 1300-PRINT-LINE.                                     EL544
00607                                                                   EL544
00608      MOVE '          FROM EL522' TO DT-1.                         EL544
00609      MOVE ME-522-RETROS          TO D1-TTL1.                      EL544
00610      MOVE DT-1                   TO P-DATA.                       EL544
00611      MOVE ZERO                   TO P-CTL.                        EL544
00612      PERFORM 1300-PRINT-LINE.                                     EL544
00613                                                                   EL544
00614      MOVE '              ECS041' TO DT-1.                         EL544
00615      MOVE ME-041-RETROS          TO D1-TTL1.                      EL544
00616      MOVE DT-1                   TO P-DATA.                       EL544
00617      MOVE 1                      TO P-CTL.                        EL544
00618      PERFORM 1300-PRINT-LINE.                                     EL544
00619                                                                   EL544
00620      MOVE 'PAYMENTS AND ADJ'     TO P-DATA.                       EL544
00621      MOVE  1                     TO P-CTL.                        EL544
00622      PERFORM 1300-PRINT-LINE.                                     EL544
00623                                                                   EL544
00624      MOVE '         FROM ECS061' TO DT-1.                         EL544
00625      MOVE ME-061-PY-ADJ          TO D1-TTL1.                      EL544
00626      MOVE DT-1                   TO P-DATA.                       EL544
00627      MOVE 1     TO P-CTL.                                         EL544
00628      PERFORM 1300-PRINT-LINE.                                     EL544
00629                                                                   EL544
00630      MOVE '               EL522' TO DT-1.                         EL544
00631      MOVE ME-522-PY-ADJ          TO D1-TTL1.                      EL544
00632      MOVE DT-1                   TO P-DATA.                       EL544
00633      MOVE 1     TO P-CTL.                                         EL544
00634      PERFORM 1300-PRINT-LINE.                                     EL544
00635                                                                   EL544
00636  0500-TEST.                                                       EL544
00637      MOVE ZERO                   TO ERRX.                         EL544
00638                                                                   EL544
00639      MOVE ME-522-PREM-L          TO WK1.                          EL544
00640      MOVE ME-010-PREM-L          TO WK2.                          EL544
00641      MOVE 0117000100             TO H-MASK.                       EL544
00642                                                                   EL544
00643      IF ME-522-RUN-DT GREATER ZERO AND                            EL544
00644         ME-010-RUN-DT GREATER ZERO                                EL544
00645            PERFORM 1000-TEST.                                     EL544
00646                                                                   EL544
00647      MOVE ME-019-PREM-L          TO WK2.                          EL544
00648      MOVE 0300                   TO HRM-3.                        EL544
00649                                                                   EL544
00650      IF ME-522-RUN-DT GREATER ZERO AND                            EL544
00651         ME-019-RUN-DT GREATER ZERO                                EL544
00652            PERFORM 1000-TEST.                                     EL544
00653                                                                   EL544
00654      MOVE ME-010-PREM-L          TO WK1.                          EL544
00655      MOVE 0100                   TO HRM-2.                        EL544
00656                                                                   EL544
00657      IF ME-010-RUN-DT GREATER ZERO AND                            EL544
00658         ME-019-RUN-DT GREATER ZERO                                EL544
00659            PERFORM 1000-TEST.                                     EL544
00660                                                                   EL544
00661      MOVE ME-522-PREM-AH         TO WK1.                          EL544
00662      MOVE ME-010-PREM-AH         TO WK2.                          EL544
00663      MOVE 0217000100             TO H-MASK.                       EL544
00664                                                                   EL544
00665      IF ME-522-RUN-DT GREATER ZERO AND                            EL544
00666         ME-010-RUN-DT GREATER ZERO                                EL544
00667            PERFORM 1000-TEST.                                     EL544
00668                                                                   EL544
00669      MOVE ME-019-PREM-AH         TO WK2.                          EL544
00670      MOVE 0300                   TO HRM-3.                        EL544
00671                                                                   EL544
00672      IF ME-522-RUN-DT GREATER ZERO AND                            EL544
00673         ME-019-RUN-DT GREATER ZERO                                EL544
00674            PERFORM 1000-TEST.                                     EL544
00675                                                                   EL544
00676      MOVE ME-010-PREM-AH         TO WK1.                          EL544
00677      MOVE 0100                   TO HRM-2.                        EL544
00678                                                                   EL544
00679      IF ME-010-RUN-DT GREATER ZERO AND                            EL544
00680         ME-019-RUN-DT GREATER ZERO                                EL544
00681            PERFORM 1000-TEST.                                     EL544
00682                                                                   EL544
00683      MOVE ME-522-REF-L           TO WK1.                          EL544
00684      MOVE ME-010-REF-L           TO WK2.                          EL544
00685      MOVE 0317000100             TO H-MASK.                       EL544
00686                                                                   EL544
00687      IF ME-522-RUN-DT GREATER ZERO AND                            EL544
00688         ME-010-RUN-DT GREATER ZERO                                EL544
00689            PERFORM 1000-TEST.                                     EL544
00690                                                                   EL544
00691      MOVE ME-019-REF-L           TO WK2.                          EL544
00692      MOVE 0300                   TO HRM-3.                        EL544
00693                                                                   EL544
00694      IF ME-522-RUN-DT GREATER ZERO AND                            EL544
00695         ME-019-RUN-DT GREATER ZERO                                EL544
00696            PERFORM 1000-TEST.                                     EL544
00697                                                                   EL544
00698      MOVE ME-010-REF-L           TO WK1.                          EL544
00699      MOVE 0100                   TO HRM-2.                        EL544
00700                                                                   EL544
00701      IF ME-010-RUN-DT GREATER ZERO AND                            EL544
00702         ME-019-RUN-DT GREATER ZERO                                EL544
00703            PERFORM 1000-TEST.                                     EL544
00704                                                                   EL544
00705      MOVE ME-522-REF-AH          TO WK1.                          EL544
00706      MOVE ME-010-REF-AH          TO WK2.                          EL544
00707      MOVE 0417000100             TO H-MASK.                       EL544
00708                                                                   EL544
00709      IF ME-522-RUN-DT GREATER ZERO AND                            EL544
00710         ME-010-RUN-DT GREATER ZERO                                EL544
00711            PERFORM 1000-TEST.                                     EL544
00712                                                                   EL544
00713      MOVE ME-019-REF-AH          TO WK2.                          EL544
00714      MOVE 0300                   TO HRM-3.                        EL544
00715                                                                   EL544
00716      IF ME-522-RUN-DT GREATER ZERO AND                            EL544
00717         ME-019-RUN-DT GREATER ZERO                                EL544
00718            PERFORM 1000-TEST.                                     EL544
00719                                                                   EL544
00720      MOVE ME-010-REF-AH          TO WK1.                          EL544
00721      MOVE 0100                   TO HRM-2.                        EL544
00722                                                                   EL544
00723      IF ME-010-RUN-DT GREATER ZERO AND                            EL544
00724         ME-019-RUN-DT GREATER ZERO                                EL544
00725            PERFORM 1000-TEST.                                     EL544
00726                                                                   EL544
00727      SUBTRACT ME-010-REF-L FROM ME-010-PREM-L GIVING WK1.         EL544
00728      MOVE ME-035-NET-L           TO WK2.                          EL544
00729      MOVE 0501000600             TO H-MASK.                       EL544
00730                                                                   EL544
00731      IF ME-010-RUN-DT GREATER ZERO AND                            EL544
00732         ME-035-RUN-DT GREATER ZERO                                EL544
00733            PERFORM 1000-TEST.                                     EL544
00734                                                                   EL544
00735      SUBTRACT ME-010-REF-AH FROM ME-010-PREM-AH GIVING WK1.       EL544
00736      MOVE ME-035-NET-AH          TO WK2.                          EL544
00737      MOVE 06                     TO HRM-1.                        EL544
00738                                                                   EL544
00739      IF ME-010-RUN-DT GREATER ZERO AND                            EL544
00740         ME-035-RUN-DT GREATER ZERO                                EL544
00741            PERFORM 1000-TEST.                                     EL544
00742                                                                   EL544
00743      COMPUTE WK1 = ME-522-PREM-L + ME-522-PREM-AH                 EL544
00744                  - ME-522-REF-L - ME-522-REF-AH.                  EL544
00745      MOVE ME-061-PREM            TO WK2.                          EL544
00746      MOVE 2117001000             TO H-MASK.                       EL544
00747                                                                   EL544
00748      IF ME-522-RUN-DT GREATER ZERO AND                            EL544
00749         ME-061-RUN-DT GREATER ZERO                                EL544
00750            PERFORM 1000-TEST.                                     EL544
00751                                                                   EL544
00752      COMPUTE WK1 = ME-010-PREM-L + ME-010-PREM-AH                 EL544
00753                  - ME-010-REF-L - ME-010-REF-AH.                  EL544
00754      MOVE 0100                   TO HRM-2.                        EL544
00755                                                                   EL544
00756      IF ME-010-RUN-DT GREATER ZERO AND                            EL544
00757         ME-061-RUN-DT GREATER ZERO                                EL544
00758            PERFORM 1000-TEST.                                     EL544
00759                                                                   EL544
00760      COMPUTE WK1 = ME-019-PREM-L + ME-019-PREM-AH                 EL544
00761                  - ME-019-REF-L - ME-019-REF-AH.                  EL544
00762      MOVE 0300                   TO HRM-2.                        EL544
00763                                                                   EL544
00764      IF ME-019-RUN-DT GREATER ZERO AND                            EL544
00765         ME-061-RUN-DT GREATER ZERO                                EL544
00766            PERFORM 1000-TEST.                                     EL544
00767                                                                   EL544
00768      MOVE ME-010-COMM-L          TO WK1.                          EL544
00769      MOVE ME-019-COMM-L          TO WK2.                          EL544
00770      MOVE 0701000300             TO H-MASK.                       EL544
00771                                                                   EL544
00772      IF ME-010-RUN-DT GREATER ZERO AND                            EL544
00773         ME-019-RUN-DT GREATER ZERO                                EL544
00774            PERFORM 1000-TEST.                                     EL544
00775                                                                   EL544
00776      MOVE ME-010-COMM-AH         TO WK1.                          EL544
00777      MOVE ME-019-COMM-AH         TO WK2.                          EL544
00778      MOVE 08                     TO HRM-1.                        EL544
00779                                                                   EL544
00780      IF ME-010-RUN-DT GREATER ZERO AND                            EL544
00781         ME-019-RUN-DT GREATER ZERO                                EL544
00782            PERFORM 1000-TEST.                                     EL544
00783                                                                   EL544
00784      ADD ME-010-COMM-L  ME-010-COMM-AH GIVING WK1.                EL544
00785      MOVE ME-061-COMM            TO WK2.                          EL544
00786      MOVE 1901001004             TO H-MASK.                       EL544
00787                                                                   EL544
00788      IF ME-010-RUN-DT GREATER ZERO AND                            EL544
00789         ME-061-RUN-DT GREATER ZERO                                EL544
00790            PERFORM 1000-TEST.                                     EL544
00791                                                                   EL544
00792      ADD ME-019-COMM-L  ME-019-COMM-AH GIVING WK1.                EL544
00793      MOVE 0300                   TO HRM-2.                        EL544
00794                                                                   EL544
00795      IF ME-019-RUN-DT GREATER ZERO AND                            EL544
00796         ME-061-RUN-DT GREATER ZERO                                EL544
00797            PERFORM 1000-TEST.                                     EL544
00798                                                                   EL544
070714     compute wk1 = me-018-comm-y + me-018-comm-1
00799 *    ADD ME-018-COMM-L  ME-018-COMM-AH GIVING WK1.                EL544
00800      MOVE ME-061-COMM-RCALC      TO WK2.                          EL544
00801      MOVE 1902001005             TO H-MASK.                       EL544
00802                                                                   EL544
00803      IF ME-018-RUN-DT GREATER ZERO AND                            EL544
00804         ME-061-RUN-DT GREATER ZERO                                EL544
00805            PERFORM 1000-TEST.                                     EL544
00806                                                                   EL544
00807      ADD ME-019-OR-L  ME-019-OR-AH GIVING WK1.                    EL544
00808      MOVE ME-061-OR              TO WK2.                          EL544
00809      MOVE 2003001004             TO H-MASK.                       EL544
00810                                                                   EL544
00811      IF ME-019-RUN-DT GREATER ZERO AND                            EL544
00812         ME-061-RUN-DT GREATER ZERO                                EL544
00813            PERFORM 1000-TEST.                                     EL544
00814                                                                   EL544
070714     compute wk1 = me-018-ow-y + me-018-ow-1
00815 *    ADD ME-018-OR-L  ME-018-OR-AH GIVING WK1.                    EL544
00816      MOVE ME-061-OR-RCALC        TO WK2.                          EL544
00817      MOVE 2002001005             TO H-MASK.                       EL544
00818                                                                   EL544
00819      IF ME-018-RUN-DT GREATER ZERO AND                            EL544
00820         ME-061-RUN-DT GREATER ZERO                                EL544
00821            PERFORM 1000-TEST.                                     EL544
00822                                                                   EL544
00823      MOVE ME-524-CLMS-L          TO WK1.                          EL544
00824      MOVE ME-522-ALL-CLM-L       TO WK2.                          EL544
00825      MOVE 1118001701             TO H-MASK.                       EL544
00826                                                                   EL544
00827      IF ME-524-RUN-DT GREATER ZERO AND                            EL544
00828         ME-522-RUN-DT GREATER ZERO                                EL544
00829            PERFORM 1000-TEST.                                     EL544
00830                                                                   EL544
00831      MOVE ME-522-PROC-CLM-L      TO WK1.                          EL544
00832      MOVE ME-010-PMT-L           TO WK2.                          EL544
00833      MOVE 1117020100             TO H-MASK.                       EL544
00834                                                                   EL544
00835      IF ME-522-RUN-DT GREATER ZERO AND                            EL544
00836         ME-010-RUN-DT GREATER ZERO                                EL544
00837            PERFORM 1000-TEST.                                     EL544
00838                                                                   EL544
00839      MOVE ME-030-CLMS-L          TO WK2.                          EL544
00840      MOVE 0400                   TO HRM-3.                        EL544
00841                                                                   EL544
00842      IF ME-522-RUN-DT GREATER ZERO AND                            EL544
00843         ME-030-RUN-DT GREATER ZERO                                EL544
00844            PERFORM 1000-TEST.                                     EL544
00845                                                                   EL544
00846      MOVE ME-010-PMT-L           TO WK1.                          EL544
00847      MOVE 0100                   TO HRM-2.                        EL544
00848                                                                   EL544
00849      IF ME-010-RUN-DT GREATER ZERO AND                            EL544
00850         ME-030-RUN-DT GREATER ZERO                                EL544
00851            PERFORM 1000-TEST.                                     EL544
00852                                                                   EL544
00853      MOVE ME-524-CLMS-AH         TO WK1.                          EL544
00854      MOVE ME-522-ALL-CLM-AH      TO WK2.                          EL544
00855      MOVE 1218001701             TO H-MASK.                       EL544
00856                                                                   EL544
00857      IF ME-524-RUN-DT GREATER ZERO AND                            EL544
00858         ME-522-RUN-DT GREATER ZERO                                EL544
00859            PERFORM 1000-TEST.                                     EL544
00860                                                                   EL544
00861      MOVE ME-522-PROC-CLM-AH     TO WK1.                          EL544
00862      MOVE ME-010-PMT-AH          TO WK2.                          EL544
00863      MOVE 1217020100             TO H-MASK.                       EL544
00864                                                                   EL544
00865      IF ME-522-RUN-DT GREATER ZERO AND                            EL544
00866         ME-010-RUN-DT GREATER ZERO                                EL544
00867            PERFORM 1000-TEST.                                     EL544
00868                                                                   EL544
00869      MOVE ME-030-CLMS-AH         TO WK2.                          EL544
00870      MOVE 0400 TO HRM-3.                                          EL544
00871                                                                   EL544
00872      IF ME-522-RUN-DT GREATER ZERO AND                            EL544
00873         ME-030-RUN-DT GREATER ZERO                                EL544
00874            PERFORM 1000-TEST.                                     EL544
00875                                                                   EL544
00876      MOVE ME-010-PMT-AH          TO WK1.                          EL544
00877      MOVE 0100                   TO HRM-2.                        EL544
00878                                                                   EL544
00879      IF ME-010-RUN-DT GREATER ZERO AND                            EL544
00880         ME-030-RUN-DT GREATER ZERO                                EL544
00881            PERFORM 1000-TEST.                                     EL544
00882                                                                   EL544
00883      MOVE ME-315-RESV-L          TO WK1.                          EL544
00884      ADD ME-524-RESV-L  ME-524-RESV-AH GIVING WK2.                EL544
00885      MOVE 2212001800             TO H-MASK.                       EL544
00886                                                                   EL544
00887      IF ME-315-RUN-DT GREATER ZERO AND                            EL544
00888         ME-524-RUN-DT GREATER ZERO                                EL544
00889            PERFORM 1000-TEST.                                     EL544
00890                                                                   EL544
00891      ADD ME-522-ALL-RSV-L  ME-522-ALL-RSV-AH GIVING WK2.          EL544
00892      MOVE 2212001701             TO H-MASK.                       EL544
00893                                                                   EL544
00894      IF ME-315-RUN-DT GREATER ZERO AND                            EL544
00895         ME-522-RUN-DT GREATER ZERO                                EL544
00896            PERFORM 1000-TEST.                                     EL544
00897                                                                   EL544
00898      MOVE ME-524-RESV-L          TO WK1.                          EL544
00899      MOVE ME-522-ALL-RSV-L       TO WK2.                          EL544
00900      MOVE 1318001701             TO H-MASK.                       EL544
00901                                                                   EL544
00902      IF ME-524-RUN-DT GREATER ZERO AND                            EL544
00903         ME-522-RUN-DT GREATER ZERO                                EL544
00904            PERFORM 1000-TEST.                                     EL544
00905                                                                   EL544
00906      MOVE ME-522-PROC-RSV-L      TO WK1.                          EL544
00907      MOVE ME-032-RESV-L          TO WK2.                          EL544
00908      MOVE 1317020500             TO H-MASK.                       EL544
00909                                                                   EL544
00910      IF ME-522-RUN-DT GREATER ZERO AND                            EL544
00911         ME-032-RUN-DT GREATER ZERO                                EL544
00912            PERFORM 1000-TEST.                                     EL544
00913                                                                   EL544
00914      MOVE ME-522-ALL-RSV-AH      TO WK2.                          EL544
00915      MOVE ME-524-RESV-AH         TO WK1.                          EL544
00916      MOVE 1418001701             TO H-MASK.                       EL544
00917                                                                   EL544
00918      IF ME-524-RUN-DT GREATER ZERO AND                            EL544
00919         ME-522-RUN-DT GREATER ZERO                                EL544
00920            PERFORM 1000-TEST.                                     EL544
00921                                                                   EL544
00922      MOVE ME-522-PROC-RSV-AH     TO WK1.                          EL544
00923      MOVE ME-032-RESV-AH         TO WK2.                          EL544
00924      MOVE 1417020500             TO H-MASK.                       EL544
00925                                                                   EL544
00926      IF ME-522-RUN-DT GREATER ZERO AND                            EL544
00927         ME-032-RUN-DT GREATER ZERO                                EL544
00928            PERFORM 1000-TEST.                                     EL544
00929                                                                   EL544
00930      MOVE ME-041-RETROS          TO WK1.                          EL544
00931      MOVE ME-522-RETROS          TO WK2.                          EL544
00932      MOVE 1808001700             TO H-MASK.                       EL544
00933                                                                   EL544
00934      IF ME-041-RUN-DT GREATER ZERO AND                            EL544
00935         ME-522-RUN-DT GREATER ZERO                                EL544
00936            PERFORM 1000-TEST.                                     EL544
00937                                                                   EL544
00938      MOVE ME-061-PY-ADJ          TO WK1.                          EL544
00939      MOVE ME-522-PY-ADJ          TO WK2.                          EL544
00940      MOVE 1710001700             TO H-MASK.                       EL544
00941                                                                   EL544
00942      IF ME-061-RUN-DT GREATER ZERO AND                            EL544
00943         ME-522-RUN-DT GREATER ZERO                                EL544
00944            PERFORM 1000-TEST.                                     EL544
00945                                                                   EL544
00946                                                                   EL544
00947  0600-PAGE-2.                                                     EL544
00948      MOVE HD-9                   TO P-DATA.                       EL544
00949      MOVE  1                     TO P-CTL.                        EL544
00950      PERFORM 1300-PRINT-LINE.                                     EL544
00951      MOVE HD-2                   TO P-DATA.                       EL544
00952      MOVE 1                      TO P-CTL.                        EL544
00953      PERFORM 1300-PRINT-LINE.                                     EL544
00954      ADD 1   TO PGCT.                                             EL544
00955      MOVE PGCT                   TO HD3-PG.                       EL544
00956      MOVE HD-3                   TO P-DATA.                       EL544
00957      MOVE 1                      TO P-CTL.                        EL544
00958      PERFORM 1300-PRINT-LINE.                                     EL544
00959                                                                   EL544
00960      IF ERRX = ZERO GO TO 0610-SPCL-MSG.                          EL544
00961                                                                   EL544
00962      MOVE ZERO TO P-CTL.                                          EL544
00963      PERFORM 0620-FORM-LINE THRU 0650-FL-EXIT                     EL544
00964          VARYING CURRX FROM 1 BY 1 UNTIL CURRX GREATER ERRX.      EL544
00965                                                                   EL544
00966      GO TO 0700-PAGE-3.                                           EL544
00967                                                                   EL544
00968  0610-SPCL-MSG.                                                   EL544
00969      MOVE SPL-MSG-1              TO P-DATA.                       EL544
00970      MOVE ZERO                   TO P-CTL.                        EL544
00971      PERFORM 1300-PRINT-LINE.                                     EL544
00972      MOVE SPL-MSG-2              TO P-DATA.                       EL544
00973      MOVE 1                      TO P-CTL.                        EL544
00974      PERFORM 1300-PRINT-LINE.                                     EL544
00975      GO TO 0700-PAGE-3.                                           EL544
00976                                                                   EL544
00977  0620-FORM-LINE.                                                  EL544
00978      MOVE ERR-TBL (CURRX)        TO DECODE-P.                     EL544
00979      MOVE ETBL-1-ITM (DP-1)      TO EXL-1.                        EL544
00980      MOVE ETBL-2-ITM (DP-2)      TO EXL-2.                        EL544
00981      MOVE ETBL-2-ITM (DP-4)      TO EXL-4.                        EL544
00982                                                                   EL544
00983      IF DP-3 = ZERO                                               EL544
00984          MOVE SPACE              TO EXL-3                         EL544
00985         ELSE                                                      EL544
00986          MOVE ETBL-3-ITM (DP-3)  TO EXL-3.                        EL544
00987                                                                   EL544
00988      IF DP-5 = ZERO                                               EL544
00989          MOVE SPACE              TO EXL-5                         EL544
00990         ELSE                                                      EL544
00991          MOVE ETBL-3-ITM (DP-5)  TO EXL-5.                        EL544
00992                                                                   EL544
00993      MOVE ERR-AMT (CURRX)        TO EXL-AMT.                      EL544
00994      MOVE EXP-LINE               TO CMP-LINE.                     EL544
00995      MOVE 1 TO X1.                                                EL544
00996      MOVE 102                    TO SVTOP.                        EL544
00997                                                                   EL544
00998  0630-CMPRS-LOOP.                                                 EL544
00999      IF SVTOP LESS 1                                              EL544
01000          GO TO 0645-CMPRS-X.                                      EL544
01001                                                                   EL544
01002      IF CHAR (SVTOP) NOT = SPACE                                  EL544
01003          GO TO 0630-CL-BOT.                                       EL544
01004                                                                   EL544
01005      SUBTRACT 1 FROM SVTOP.                                       EL544
01006      GO TO 0630-CMPRS-LOOP.                                       EL544
01007                                                                   EL544
01008  0630-CL-BOT.                                                     EL544
01009      ADD 1  X1 GIVING X2.                                         EL544
01010                                                                   EL544
01011      IF X2 GREATER SVTOP                                          EL544
01012          GO TO 0645-CMPRS-X.                                      EL544
01013                                                                   EL544
01014      IF SPACE = CHAR (X1) AND CHAR (X2)                           EL544
01015          GO TO 0633-DOWNER.                                       EL544
01016                                                                   EL544
01017      ADD 1 TO X1.                                                 EL544
01018      GO TO 0630-CL-BOT.                                           EL544
01019                                                                   EL544
01020  0633-DOWNER.                                                     EL544
01021      ADD 1 X2 GIVING X3.                                          EL544
01022                                                                   EL544
01023      IF X3 GREATER SVTOP                                          EL544
01024            GO TO 0630-CL-BOT.                                     EL544
01025                                                                   EL544
01026      MOVE CHAR (X3)              TO CHAR (X2).                    EL544
01027                                                                   EL544
01028      IF X3 = SVTOP                                                EL544
01029          MOVE SPACE              TO CHAR (SVTOP)                  EL544
01030          SUBTRACT 1 FROM SVTOP.                                   EL544
01031                                                                   EL544
01032      ADD 1 TO X2.                                                 EL544
01033      GO TO 0633-DOWNER.                                           EL544
01034                                                                   EL544
01035  0645-CMPRS-X.                                                    EL544
01036      MOVE CMP-LINE               TO P-DATA.                       EL544
01037      PERFORM 1300-PRINT-LINE.                                     EL544
01038      MOVE ZERO                   TO P-CTL.                        EL544
01039                                                                   EL544
01040  0650-FL-EXIT.                                                    EL544
01041      EXIT.                                                        EL544
01042                                                                   EL544
01043  0700-PAGE-3.                                                     EL544
01044      IF ME-010-CERT-IN NOT = PRIOR-CERTS                          EL544
01045          MOVE 'CERTIFICATE COUNT INPUT DOES NOT = PRIOR OUTPUT'   EL544
01046          TO P-DATA                                                EL544
01047          MOVE 1                  TO P-CTL                         EL544
01048          PERFORM 1300-PRINT-LINE.                                 EL544
01049                                                                   EL544
01050      IF ME-038-RECS-IN NOT = PRIOR-HIST-RECS                      EL544
01051          MOVE 'HISTORY RECORDS INPUT DO NOT = PRIOR OUTPUT'       EL544
01052          TO P-DATA                                                EL544
01053          MOVE 1                  TO P-CTL                         EL544
01054          PERFORM 1300-PRINT-LINE.                                 EL544
01055                                                                   EL544
01056      IF ME-341-NOT-FOUND NOT = ZERO                               EL544
01057          MOVE 'EL341 HAS ERRORS' TO P-DATA                        EL544
01058          MOVE 1                  TO P-CTL                         EL544
01059          PERFORM 1300-PRINT-LINE.                                 EL544
01060                                                                   EL544
01061      IF ME-080-MORT-ERRS GREATER 25                               EL544
01062          MOVE 'ECS080 HAS OVER 25 ERRORS'   TO P-DATA             EL544
01063          MOVE 1                  TO P-CTL                         EL544
01064          PERFORM 1300-PRINT-LINE.                                 EL544
01065                                                                   EL544
01066      IF ME-331-FLAG NOT = 1                                       EL544
01067          MOVE 'EL331 NOT COMPLETED'    TO P-DATA                  EL544
01068          MOVE 1                  TO P-CTL                         EL544
01069          PERFORM 1300-PRINT-LINE.                                 EL544
01070                                                                   EL544
01071      IF ME-501-FLAG NOT = 1                                       EL544
01072          MOVE 'EL501 NOT COMPLETED'    TO P-DATA                  EL544
01073          MOVE 1                  TO P-CTL                         EL544
01074          PERFORM 1300-PRINT-LINE.                                 EL544
01075                                                                   EL544
01076      IF ME-509-FLAG NOT = 1                                       EL544
01077          MOVE 'EL509 NOT COMPLETED'    TO P-DATA                  EL544
01078          MOVE 1     TO P-CTL                                      EL544
01079          PERFORM 1300-PRINT-LINE.                                 EL544
01080                                                                   EL544
01081      IF ME-525-FLAG NOT = 1                                       EL544
01082          MOVE 'EL525 NOT COMPLETED'    TO P-DATA                  EL544
01083          MOVE 1     TO P-CTL                                      EL544
01084          PERFORM 1300-PRINT-LINE.                                 EL544
01085                                                                   EL544
01086      MOVE HD-A                   TO P-DATA.                       EL544
01087      MOVE  1                     TO P-CTL.                        EL544
01088      PERFORM 1300-PRINT-LINE.                                     EL544
01089      MOVE HD-2                   TO P-DATA.                       EL544
01090      MOVE 1                      TO P-CTL.                        EL544
01091      PERFORM 1300-PRINT-LINE.                                     EL544
01092      ADD 1 TO PGCT.                                               EL544
01093      MOVE PGCT                   TO HD3-PG.                       EL544
01094      MOVE HD-3                   TO P-DATA.                       EL544
01095      MOVE 1                      TO P-CTL.                        EL544
01096      PERFORM 1300-PRINT-LINE.                                     EL544
01097      MOVE HD-B                   TO P-DATA.                       EL544
01098      MOVE ZERO                   TO P-CTL.                        EL544
01099      PERFORM 1300-PRINT-LINE.                                     EL544
01100      MOVE HD-C                   TO P-DATA.                       EL544
01101      MOVE 1                      TO P-CTL.                        EL544
01102      PERFORM 1300-PRINT-LINE.                                     EL544
01103                                                                   EL544
01104      MOVE ZERO                   TO P-CTL  CURRX.                 EL544
01105      MOVE ME-010-RUN-DT          TO DH-RUN.                       EL544
070714*    MOVE ME-010-START           TO DH-START.                     EL544
070714*    MOVE ME-010-END             TO DH-END.                       EL544
01108      MOVE ME-010-RUN-CT          TO DH-COUNT.                     EL544
01109      PERFORM 0720-SETLN-3.                                        EL544
01110                                                                   EL544
01111      MOVE ME-018-RUN-DT          TO DH-RUN.                       EL544
070714*    MOVE ME-018-START           TO DH-START.                     EL544
070714*    MOVE ME-018-END             TO DH-END.                       EL544
01114      MOVE ME-018-RUN-CT          TO DH-COUNT.                     EL544
01115      PERFORM 0720-SETLN-3.                                        EL544
01116                                                                   EL544
01117      MOVE ME-019-RUN-DT          TO DH-RUN.                       EL544
070714*    MOVE ME-019-START           TO DH-START.                     EL544
070714*    MOVE ME-019-END             TO DH-END.                       EL544
01120      MOVE ME-019-RUN-CT          TO DH-COUNT.                     EL544
01121      PERFORM 0720-SETLN-3.                                        EL544
01122                                                                   EL544
01123      MOVE ME-030-RUN-DT          TO DH-RUN.                       EL544
070714*    MOVE ME-030-START           TO DH-START.                     EL544
070714*    MOVE ME-030-END             TO DH-END.                       EL544
01126      MOVE ME-030-RUN-CT          TO DH-COUNT.                     EL544
01127      PERFORM 0720-SETLN-3.                                        EL544
01128                                                                   EL544
01129      MOVE ME-032-RUN-DT          TO DH-RUN.                       EL544
070714*    MOVE ME-032-START           TO DH-START.                     EL544
070714*    MOVE ME-032-END             TO DH-END.                       EL544
01132      MOVE ME-032-RUN-CT          TO DH-COUNT.                     EL544
01133      PERFORM 0720-SETLN-3.                                        EL544
01134                                                                   EL544
01135      MOVE ME-035-RUN-DT          TO DH-RUN.                       EL544
070714*    MOVE ME-035-START           TO DH-START.                     EL544
070714*    MOVE ME-035-END             TO DH-END.                       EL544
01138      MOVE ME-035-RUN-CT          TO DH-COUNT.                     EL544
01139      PERFORM 0720-SETLN-3.                                        EL544
01140                                                                   EL544
01141      MOVE ME-038-RUN-DT          TO DH-RUN.                       EL544
070714*    MOVE ME-038-START           TO DH-START.                     EL544
070714*    MOVE ME-038-END             TO DH-END.                       EL544
01144      MOVE ME-038-RUN-CT          TO DH-COUNT.                     EL544
01145      PERFORM 0720-SETLN-3.                                        EL544
01146                                                                   EL544
01147      MOVE ME-041-RUN-DT          TO DH-RUN.                       EL544
070714*    MOVE ME-041-START           TO DH-START.                     EL544
070714*    MOVE ME-041-END             TO DH-END.                       EL544
01150      MOVE ME-041-RUN-CT          TO DH-COUNT.                     EL544
01151      PERFORM 0720-SETLN-3.                                        EL544
01152                                                                   EL544
01153      MOVE ME-050-RUN-DT          TO DH-RUN.                       EL544
070714*    MOVE ME-050-START           TO DH-START.                     EL544
070714*    MOVE ME-050-END             TO DH-END.                       EL544
01156      MOVE ME-050-RUN-CT          TO DH-COUNT.                     EL544
01157      PERFORM 0720-SETLN-3.                                        EL544
01158                                                                   EL544
01159      MOVE ME-061-RUN-DT          TO DH-RUN.                       EL544
070714*    MOVE ME-061-START           TO DH-START.                     EL544
070714*    MOVE ME-061-END             TO DH-END.                       EL544
01162      MOVE ME-061-RUN-CT          TO DH-COUNT.                     EL544
01163      PERFORM 0720-SETLN-3.                                        EL544
01164                                                                   EL544
01165      MOVE ME-080-RUN-DT          TO DH-RUN.                       EL544
070714*    MOVE ME-080-START           TO DH-START.                     EL544
070714*    MOVE ME-080-END             TO DH-END.                       EL544
01168      MOVE ME-080-RUN-CT          TO DH-COUNT.                     EL544
01169      PERFORM 0720-SETLN-3.                                        EL544
01170                                                                   EL544
01171      MOVE ME-315-RUN-DT          TO DH-RUN.                       EL544
070714*    MOVE ME-315-START           TO DH-START.                     EL544
070714*    MOVE ME-315-END             TO DH-END.                       EL544
01174      MOVE ME-315-RUN-CT          TO DH-COUNT.                     EL544
01175      PERFORM 0720-SETLN-3.                                        EL544
01176                                                                   EL544
01177      MOVE ME-331-RUN-DT          TO DH-RUN.                       EL544
070714*    MOVE ME-331-START           TO DH-START.                     EL544
070714*    MOVE ME-331-END             TO DH-END.                       EL544
01180      MOVE ME-331-RUN-CT          TO DH-COUNT.                     EL544
01181      PERFORM 0720-SETLN-3.                                        EL544
01182                                                                   EL544
01183      MOVE ME-341-RUN-DT          TO DH-RUN.                       EL544
070714*    MOVE ME-341-START           TO DH-START.                     EL544
070714*    MOVE ME-341-END             TO DH-END.                       EL544
01186      MOVE ME-341-RUN-CT          TO DH-COUNT.                     EL544
01187      PERFORM 0720-SETLN-3.                                        EL544
01188                                                                   EL544
01189      MOVE ME-501-RUN-DT          TO DH-RUN.                       EL544
01190      MOVE ME-501-START           TO DH-START.                     EL544
01191      MOVE ME-501-END             TO DH-END.                       EL544
01192      MOVE ME-501-RUN-CT          TO DH-COUNT.                     EL544
01193      PERFORM 0720-SETLN-3.                                        EL544
01194                                                                   EL544
01195      MOVE ME-509-RUN-DT          TO DH-RUN.                       EL544
01196      MOVE ME-509-START           TO DH-START.                     EL544
01197      MOVE ME-509-END             TO DH-END.                       EL544
01198      MOVE ME-509-RUN-CT          TO DH-COUNT.                     EL544
01199      PERFORM 0720-SETLN-3.                                        EL544
01200                                                                   EL544
01201      MOVE ME-522-RUN-DT          TO DH-RUN.                       EL544
01202      MOVE ME-522-START           TO DH-START.                     EL544
01203      MOVE ME-522-END             TO DH-END.                       EL544
01204      MOVE ME-522-RUN-CT          TO DH-COUNT.                     EL544
01205      PERFORM 0720-SETLN-3.                                        EL544
01206                                                                   EL544
01207      MOVE ME-524-RUN-DT          TO DH-RUN.                       EL544
01208      MOVE ME-524-START           TO DH-START.                     EL544
01209      MOVE ME-524-END             TO DH-END.                       EL544
01210      MOVE ME-524-RUN-CT          TO DH-COUNT.                     EL544
01211      PERFORM 0720-SETLN-3.                                        EL544
01212                                                                   EL544
01213      MOVE ME-525-RUN-DT          TO DH-RUN.                       EL544
01214      MOVE ME-525-START           TO DH-START.                     EL544
01215      MOVE ME-525-END             TO DH-END.                       EL544
01216      MOVE ME-525-RUN-CT          TO DH-COUNT.                     EL544
01217      PERFORM 0720-SETLN-3.                                        EL544
01218                                                                   EL544
01219  0710-EOJ.                                                        EL544
01220      GO TO 0230-RETURN-C.                                         EL544
01221                                                                   EL544
01222  0720-SETLN-3.                                                    EL544
01223      ADD 1 TO CURRX.                                              EL544
01224      MOVE ETBL-2-ITM (CURRX)     TO DH-JOB.                       EL544
01225      MOVE SPACE                  TO DT-2.                         EL544
01226      MOVE DH-JOB                 TO D2-JOBNO.                     EL544
01227      MOVE DH-RUNMO               TO D2-RUNMO.                     EL544
01228      MOVE DH-RUNDA               TO D2-RUNDA.                     EL544
01229      MOVE DH-RUNYR               TO D2-RUNYR.                     EL544
01230      MOVE '/'                    TO D2-SL1, D2-SL2.               EL544
01231      MOVE DH-STHR                TO D2-BGHR.                      EL544
01232      MOVE DH-STMN                TO D2-BGMN.                      EL544
01233      MOVE DH-STSC                TO D2-BGSC.                      EL544
01234      MOVE DH-ENHR                TO D2-FNHR.                      EL544
01235      MOVE DH-ENMN                TO D2-FNMN.                      EL544
01236      MOVE DH-ENSC                TO D2-FNSC.                      EL544
01237      MOVE DH-COUNT               TO D2-RRNO.                      EL544
01238      MOVE DH-ENHR                TO SIGN-HR.                      EL544
01239      MOVE DH-ENMN                TO SIGN-MN.                      EL544
01240      MOVE DH-ENSC                TO SIGN-SC.                      EL544
01241      MOVE DH-STHR                TO EXPND-HR.                     EL544
01242      MOVE DH-STMN                TO EXPND-MN.                     EL544
01243      MOVE DH-STSC                TO EXPND-SC.                     EL544
01244                                                                   EL544
01245      IF SIGN-SC LESS EXPND-SC                                     EL544
01246          ADD 60 TO SIGN-SC                                        EL544
01247          SUBTRACT 1 FROM SIGN-MN.                                 EL544
01248                                                                   EL544
01249      IF SIGN-MN NEGATIVE                                          EL544
01250          ADD 60 TO SIGN-MN.                                       EL544
01251                                                                   EL544
01252      IF SIGN-MN LESS EXPND-MN                                     EL544
01253          ADD 60 TO SIGN-MN                                        EL544
01254          SUBTRACT 1 FROM SIGN-HR.                                 EL544
01255                                                                   EL544
01256      IF SIGN-HR NEGATIVE                                          EL544
01257          ADD 24 TO SIGN-HR.                                       EL544
01258                                                                   EL544
01259      SUBTRACT EXPND-HR FROM SIGN-HR.                              EL544
01260      SUBTRACT EXPND-MN FROM SIGN-MN.                              EL544
01261      SUBTRACT EXPND-SC FROM SIGN-SC.                              EL544
01262      MOVE SIGN-HR                TO D2-DUHR.                      EL544
01263      MOVE SIGN-MN                TO D2-DUMN.                      EL544
01264      MOVE SIGN-SC                TO D2-DUSC.                      EL544
01265      MOVE '.'                    TO D2-SC1  D2-SC2                EL544
01266                                     D2-SC3  D2-SC4                EL544
01267                                     D2-SC5  D2-SC6.               EL544
01268      MOVE DT-2                   TO P-DATA.                       EL544
01269      PERFORM 1300-PRINT-LINE.                                     EL544
01270      MOVE 1                      TO P-CTL.                        EL544
01271                                                                   EL544
01272  1000-TEST SECTION.                                               EL544
01273      SUBTRACT WK2 FROM WK1 GIVING WK3.                            EL544
01274                                                                   EL544
01275      IF WK3 = ZERO                                                EL544
01276          GO TO 1099-TESTX.                                        EL544
01277                                                                   EL544
01278      ADD 1 TO ERRX.                                               EL544
01279                                                                   EL544
01280      IF ERRX GREATER 50                                           EL544
01281          MOVE 'ERROR TABLE SIZE EXCEEDED'    TO WS-ABEND-MESSAGE  EL544
01282          PERFORM ABEND-PGM.                                       EL544
01283                                                                   EL544
01284      IF WK1 GREATER WK2                                           EL544
01285          MOVE H-MASK             TO P-MASK                        EL544
01286         ELSE                                                      EL544
01287          MOVE HRM-1              TO PRM-1                         EL544
01288          MOVE HRM-2              TO PRM-3                         EL544
01289          MOVE HRM-3              TO PRM-2.                        EL544
01290                                                                   EL544
01291      MOVE P-MASK                 TO ERR-TBL (ERRX).               EL544
01292      MOVE WK3                    TO ERR-AMT (ERRX).               EL544
01293                                                                   EL544
01294  1099-TESTX.                                                      EL544
01295      EXIT.                                                        EL544
01296                                                                   EL544
01297  1300-PRINT-LINE SECTION.                                         EL544
01298      MOVE P-CTL                  TO X.                            EL544
01299      MOVE X TO LCP-ASA                                            EL544
01300      PERFORM LCP-WRITE-POS-PRT                                    EL544
01301          THRU LCP-WRITE-END-PRT.                                  EL544
01302                                                                   EL544
01303  1400-PGCHG SECTION.                                              EL544
01304      ADD 1   TO PGCT.                                             EL544
01305      MOVE ZERO                   TO LNCT.                         EL544
01306      MOVE PGCT                   TO HD3-PG.                       EL544
01307      MOVE  1                     TO P-CTL.                        EL544
01308      MOVE HD-1                   TO P-DATA.                       EL544
01309      PERFORM 1300-PRINT-LINE.                                     EL544
01310      MOVE 1                      TO P-CTL.                        EL544
01311      MOVE HD-2                   TO P-DATA.                       EL544
01312      PERFORM 1300-PRINT-LINE.                                     EL544
01313      MOVE 1                      TO P-CTL.                        EL544
01314      MOVE HD-3                   TO P-DATA.                       EL544
01315      PERFORM 1300-PRINT-LINE.                                     EL544
01316      MOVE ZERO                   TO P-CTL.                        EL544
01317      MOVE HD-4                   TO P-DATA.                       EL544
01318      PERFORM 1300-PRINT-LINE.                                     EL544
01319      MOVE HD-5                   TO P-DATA.                       EL544
01320      MOVE ZERO                   TO P-CTL.                        EL544
01321      PERFORM 1300-PRINT-LINE.                                     EL544
01322      MOVE HD-6                   TO P-DATA.                       EL544
01323      MOVE  1                     TO P-CTL.                        EL544
01324      PERFORM 1300-PRINT-LINE.                                     EL544
01325                                                                   EL544
01326  1490-PGCHG-EX.                                                   EL544
01327      EXIT.                                                        EL544
01328                                                                   EL544
01329  2000-MINORS SECTION.                                             EL544
01330                                                                   EL544
01331  ABEND-PGM SECTION.                                               EL544
01332      COPY ELCABEND SUPPRESS.                                      EL544
01333                                                                   EL544
01334  EOJ SECTION.                                                     EL544
01335      CLOSE CRMEBL.                                                EL544
01336                                                                   EL544
01337      PERFORM 0300-PAGE-1 THRU 0700-PAGE-3.                        EL544
01338                                                                   EL544
01339      CLOSE PRINT-FILE.                                            EL544
01340                                                                   EL544
01341      GOBACK.                                                      EL544
01342                                                                   EL544
01343  LCP-WRITE-POS-PRT SECTION.                                       EL544
01344      IF LCP-ASA = '+'                                             EL544
01345          WRITE PRT AFTER 0 LINE                                   EL544
01346      ELSE                                                         EL544
01347      IF LCP-ASA = ' '                                             EL544
01348          WRITE PRT AFTER ADVANCING 1 LINE                         EL544
01349      ELSE                                                         EL544
01350      IF LCP-ASA = '0'                                             EL544
01351          WRITE PRT AFTER ADVANCING 2 LINE                         EL544
01352      ELSE                                                         EL544
01353      IF LCP-ASA = '-'                                             EL544
01354          WRITE PRT AFTER ADVANCING 3 LINE                         EL544
01355      ELSE                                                         EL544
01356      IF LCP-ASA = '1'                                             EL544
01357          WRITE PRT AFTER ADVANCING PAGE                           EL544
01358      ELSE                                                         EL544
01359      IF LCP-ASA = '2'                                             EL544
01360          WRITE PRT AFTER ADVANCING LCP-CH2                        EL544
01361      ELSE                                                         EL544
01362      IF LCP-ASA = '3'                                             EL544
01363          WRITE PRT AFTER ADVANCING LCP-CH3                        EL544
01364      ELSE                                                         EL544
01365      IF LCP-ASA = '4'                                             EL544
01366          WRITE PRT AFTER ADVANCING LCP-CH4                        EL544
01367      ELSE                                                         EL544
01368      IF LCP-ASA = '5'                                             EL544
01369          WRITE PRT AFTER ADVANCING LCP-CH5                        EL544
01370      ELSE                                                         EL544
01371      IF LCP-ASA = '6'                                             EL544
01372          WRITE PRT AFTER ADVANCING LCP-CH6                        EL544
01373      ELSE                                                         EL544
01374      IF LCP-ASA = '7'                                             EL544
01375          WRITE PRT AFTER ADVANCING LCP-CH7                        EL544
01376      ELSE                                                         EL544
01377      IF LCP-ASA = '8'                                             EL544
01378          WRITE PRT AFTER ADVANCING LCP-CH8                        EL544
01379      ELSE                                                         EL544
01380      IF LCP-ASA = '9'                                             EL544
01381          WRITE PRT AFTER ADVANCING LCP-CH9                        EL544
01382      ELSE                                                         EL544
01383      IF LCP-ASA = 'A'                                             EL544
01384          WRITE PRT AFTER ADVANCING LCP-CH10                       EL544
01385      ELSE                                                         EL544
01386      IF LCP-ASA = 'B'                                             EL544
01387          WRITE PRT AFTER ADVANCING LCP-CH11                       EL544
01388      ELSE                                                         EL544
01389      IF LCP-ASA = 'C'                                             EL544
01390          WRITE PRT AFTER ADVANCING LCP-CH12                       EL544
01391      ELSE                                                         EL544
01392      IF LCP-ASA = 'V'                                             EL544
01393          WRITE PRT AFTER ADVANCING LCP-P01                        EL544
01394      ELSE                                                         EL544
01395      IF LCP-ASA = 'W'                                             EL544
01396          WRITE PRT AFTER ADVANCING LCP-P02                        EL544
01397      ELSE                                                         EL544
01398      DISPLAY 'ASA CODE ERROR'.                                    EL544
01399  LCP-WRITE-END-PRT.                                               EL544
01400      EXIT.                                                        EL544
