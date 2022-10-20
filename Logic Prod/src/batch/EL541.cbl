00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL541
00003  PROGRAM-ID.                 EL541 .                                 LV006
00004 *              PROGRAM CONVERTED BY                               EL541
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL541
00006 *              CONVERSION DATE 02/12/96 16:39:54.                 EL541
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          EL541
00008 *                            VMOD=2.007.                             CL**5
00009                                                                   EL541
00010 *AUTHOR.        LOGIC, INC.                                       EL541
00011 *               DALLAS, TEXAS.                                    EL541
00012                                                                   EL541
00013 *DATE-COMPILED.                                                   EL541
00014                                                                   EL541
00015 *SECURITY.   *****************************************************EL541
00016 *            *                                                   *EL541
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL541
00018 *            *                                                   *EL541
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL541
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL541
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL541
00022 *            *                                                   *EL541
00023 *            *****************************************************EL541
00024                                                                   EL541
00025 *REMARKS.                                                         EL541
00026 *        REPORT WRITER FOR MONTH-ENDING BALANCES, DISCREPANCIES   EL541
00027 *        AND RUN TIMES.                                           EL541
00028                                                                   EL541
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
00029  ENVIRONMENT DIVISION.                                            EL541
00030  CONFIGURATION SECTION.                                           EL541
00031  INPUT-OUTPUT SECTION.                                            EL541
00032  FILE-CONTROL.                                                    EL541
00033                                                                   EL541
00034      SELECT  PRINT-FILE      ASSIGN SYS008-UR-1403-S-SYS008.      EL541
00035      SELECT  ELREPT          ASSIGN SYS010-FBA1-ELREPT            EL541
00036                              ORGANIZATION INDEXED                 EL541
00037                              ACCESS DYNAMIC                       EL541
00038                              RECORD KEY RF-CONTROL-PRIMARY        EL541
00039                              FILE STATUS DTE-VSAM-FLAGS.          EL541
00040      SELECT  DISK-DATE       ASSIGN SYS019-UT-FBA1-SYS019.        EL541
00041      SELECT  FICH            ASSIGN SYS020-UT-2400-S-SYS020.      EL541
00042      SELECT  ERMEBL          ASSIGN SYS025-FBA1-ERMEBL            EL541
00043                              ORGANIZATION INDEXED                 EL541
00044                              ACCESS DYNAMIC                       EL541
00045                              RECORD KEY ME-CONTROL-PRIMARY        EL541
00046                              FILE STATUS ME-STATUS.               EL541
00047  EJECT                                                            EL541
00048  DATA DIVISION.                                                   EL541
00049  FILE SECTION.                                                    EL541
00050                                                                   EL541
00051  FD  PRINT-FILE                                                   EL541
00052                              COPY ELCPRTFD.                       EL541
00053  EJECT                                                            EL541
00054  FD  ELREPT                                                       EL541
00055                              COPY ELCRPTFD.                       EL541
00056                                                                   EL541
00057  COPY ELCREPT.                                                    EL541
00058  EJECT                                                            EL541
00059  FD  DISK-DATE                                                    EL541
00060                              COPY ELCDTEFD.                       EL541
00061  EJECT                                                            EL541
00062  FD  FICH                                                         EL541
00063                              COPY ELCFCHFD.                       EL541
00064  EJECT                                                            EL541
00065  FD  ERMEBL.                                                      EL541
00066                                                                   EL541
00067  COPY ERCMEBL.                                                    EL541
00068  EJECT                                                            EL541
00069  WORKING-STORAGE SECTION.                                         EL541
00070  77  LCP-ASA                       PIC X.                         EL541
00071  77  FILLER  PIC X(32) VALUE '********************************'.  EL541
00072  77  FILLER  PIC X(32) VALUE '*    EL541 WORKING-STORAGE     *'.  EL541
00073  77  FILLER  PIC X(32) VALUE '************V/M 2.007 **********'.     CL**5
00074                                                                   EL541
00075  77  PGM-SUB                 PIC S9(3)   COMP    VALUE +541.      EL541
00076  77  WS-RETURN-CODE          PIC S9(4)   COMP    VALUE +0.        EL541
00077  77  WS-ZERO                 PIC S9      COMP-3  VALUE +0.        EL541
00078  77  X                       PIC  X.                              EL541
00079  77  CURRENT-RECORD-SWITCH   PIC  X              VALUE 'Y'.       EL541
00080      88  CURRENT-RECORD                          VALUE 'Y'.       EL541
00081  77  WS-ABEND-FILE-STATUS    PIC  XX             VALUE ZERO.      EL541
00082  77  PGCT                    PIC  99             VALUE ZERO.      EL541
00083  77  LNCT                    PIC  99             VALUE 80.        EL541
00084  77  OLC-REPORT-NAME         PIC  X(6)           VALUE 'EL541'.   EL541
00085  77  WS-ABEND-MESSAGE        PIC  X(80)          VALUE SPACES.    EL541
00086                                                                   EL541
00087  01  HD-1.                                                        EL541
00088      12  FILLER              PIC  X(26)          VALUE SPACE.     EL541
00089      12  FILLER              PIC  X(28)          VALUE            EL541
00090              'MONTH-END ACTIVITY/INVENTORY'.                      EL541
00091      12  FILLER              PIC  X(20)          VALUE SPACE.     EL541
00092      12  FILLER              PIC  X(6)           VALUE ' EL541'.  EL541
00093                                                                   EL541
00094  01  HD-2.                                                        EL541
00095      12  FILLER              PIC  X(25)          VALUE SPACE.     EL541
00096      12  HD2-CO              PIC  X(47)          VALUE            EL541
00097              '     LOGIC INCORPORATED'.                           EL541
00098      12  HD2-RUN             PIC  X(8).                           EL541
00099                                                                   EL541
00100  01  HD-3.                                                        EL541
00101      12  FILLER              PIC  X(33)          VALUE SPACE.     EL541
00102      12  HD3-DATE            PIC  X(40).                          EL541
00103      12  FILLER              PIC  X(5)           VALUE 'PAGE'.    EL541
00104      12  HD3-PG              PIC Z9.                              EL541
00105                                                                   EL541
00106  01  HD-4.                                                        EL541
00107      12  FILLER              PIC  X(40)          VALUE            EL541
00108              'PREMIUMS/REFUNDS            -------- PRE'.          EL541
00109      12  FILLER              PIC  X(40)          VALUE            EL541
00110              'MIUMS ------- --------- REFUNDS -------'.           EL541
00111                                                                   EL541
00112  01  HD-5.                                                        EL541
00113      12  FILLER              PIC  X(34)          VALUE SPACE.     EL541
00114      12  HD-5-L1             PIC  X(6)           VALUE SPACE.     EL541
00115      12  FILLER              PIC  X(7)           VALUE SPACE.     EL541
00116      12  HD-5-A1             PIC  X(6)           VALUE SPACE.     EL541
00117      12  FILLER              PIC  X(7)           VALUE SPACE.     EL541
00118      12  HD-5-L2             PIC  X(6)           VALUE SPACE.     EL541
00119      12  FILLER              PIC  X(7)           VALUE SPACE.     EL541
00120      12  HD-5-A2             PIC  X(6)           VALUE SPACE.     EL541
00121                                                                   EL541
00122  01  HD-6.                                                        EL541
00123      12  FILLER              PIC  X(40)          VALUE            EL541
00124              'NET WRITTEN                 ------ NET W'.          EL541
00125      12  FILLER              PIC  X(13)          VALUE            EL541
00126              'RITTEN ------'.                                     EL541
00127                                                                   EL541
00128  01  HD-7.                                                        EL541
00129      12  FILLER              PIC  X(40)          VALUE            EL541
00130              'COMMISSIONS                 --- ACCOUNT'.           EL541
00131      12  FILLER              PIC  X(40)          VALUE            EL541
00132              'COMMISSION -- -- OVERRIDE COMMISSION --'.           EL541
00133                                                                   EL541
00134  01  HD-8.                                                        EL541
00135      12  FILLER              PIC  X(40)          VALUE            EL541
00136              'CLAIMS                      ---------PAY'.          EL541
00137      12  FILLER              PIC  X(40)          VALUE            EL541
00138              'MENTS ------- ------- RESERVES -------'.            EL541
00139                                                                   EL541
00140  01  HD-8A.                                                       EL541
00141      12  FILLER              PIC  X(40)          VALUE            EL541
00142              'RETROS                             TOTAL'.          EL541
00143                                                                   EL541
00144  01  HD-8B.                                                          CL**5
00145      12  FILLER              PIC  X(40)          VALUE               CL**5
00146              'PAYMENTS AND ADJ                        '.             CL**5
00147                                                                      CL**5
00148  01  HD-8C.                                                          CL**5
00149      12  FILLER              PIC  X(40)          VALUE               CL**5
00150              'REINSURANCE ADJUSTMENTS                 '.             CL**5
00151                                                                      CL**5
00152  01  HD-9.                                                        EL541
00153      12  FILLER              PIC  X(28)          VALUE SPACE.     EL541
00154      12  FILLER              PIC  X(23)          VALUE            EL541
00155              'MONTH-END DISCREPANCIES'.                           EL541
00156      12  FILLER              PIC  X(23)          VALUE SPACE.     EL541
00157      12  FILLER              PIC  X(6)           VALUE ' EL541'.  EL541
00158                                                                   EL541
00159  01  HD-A.                                                        EL541
00160      12  FILLER              PIC  X(29)          VALUE SPACE.     EL541
00161      12  FILLER              PIC  X(22)          VALUE            EL541
00162              'TASK DURATION ANALYSIS'.                            EL541
00163      12  FILLER              PIC  X(23)          VALUE SPACE.     EL541
00164      12  FILLER              PIC  X(6)           VALUE ' EL541'.  EL541
00165                                                                   EL541
00166  01  HD-B.                                                        EL541
00167      12  FILLER              PIC  X(20)          VALUE SPACE.     EL541
00168      12  FILLER              PIC  X(6)           VALUE 'RUN ON'.  EL541
00169                                                                   EL541
00170  01  HD-C.                                                        EL541
00171      12  FILLER              PIC  X(10)          VALUE SPACE.     EL541
00172      12  FILLER              PIC  X(30)          VALUE            EL541
00173              'JOB #    MO DA YR    STARTED '.                     EL541
00174      12  FILLER              PIC  X(30)          VALUE            EL541
00175              ' FINISHED   DURATION     RUN #'.                    EL541
00176                                                                   EL541
00177  01  DT-1.                                                        EL541
00178      12  D1-TITLE            PIC  X(20).                          EL541
00179      12  FILLER              PIC  X(8).                           EL541
00180      12  D1-TTL1             PIC Z,ZZZ,ZZZ.ZZ-.                   EL541
00181      12  D1-TTL2             PIC Z,ZZZ,ZZZ.ZZ-.                   EL541
00182      12  D1-TTL3             PIC Z,ZZZ,ZZZ.ZZ-.                   EL541
00183      12  D1-TTL4             PIC Z,ZZZ,ZZZ.ZZ-.                   EL541
00184                                                                   EL541
00185  01  DT-2.                                                        EL541
00186      12  FILLER              PIC  X(10).                          EL541
00187      12  D2-JOBNO            PIC  X(9).                           EL541
00188      12  D2-RUNMO            PIC Z9.                              EL541
00189      12  D2-SL1              PIC  X.                              EL541
00190      12  D2-RUNDA            PIC  99.                             EL541
00191      12  D2-SL2              PIC  X.                              EL541
00192      12  D2-RUNYR            PIC  99.                             EL541
00193      12  FILLER              PIC  X(3).                           EL541
00194      12  D2-BGHR             PIC  99.                             EL541
00195      12  D2-SC1              PIC  X.                              EL541
00196      12  D2-BGMN             PIC  99.                             EL541
00197      12  D2-SC2              PIC  X.                              EL541
00198      12  D2-BGSC             PIC  99.                             EL541
00199      12  FILLER              PIC  X(3).                           EL541
00200      12  D2-FNHR             PIC  99.                             EL541
00201      12  D2-SC3              PIC  X.                              EL541
00202      12  D2-FNMN             PIC  99.                             EL541
00203      12  D2-SC4              PIC  X.                              EL541
00204      12  D2-FNSC             PIC  99.                             EL541
00205      12  FILLER              PIC  X(3).                           EL541
00206      12  D2-DUHR             PIC  99.                             EL541
00207      12  D2-SC5              PIC  X.                              EL541
00208      12  D2-DUMN             PIC  99.                             EL541
00209      12  D2-SC6              PIC  X.                              EL541
00210      12  D2-DUSC             PIC  99.                             EL541
00211      12  FILLER              PIC  X(6).                           EL541
00212      12  D2-RRNO             PIC ZZZ.                             EL541
00213                                                                   EL541
00214  01  ME-STATUS.                                                   EL541
00215      12  ME-STAT-1           PIC  X.                              EL541
00216      12  ME-STAT-2           PIC  X.                              EL541
00217                                                                   EL541
00218  01  RFMT.                                                        EL541
00219      12  RFYRDA              PIC  99.                             EL541
00220      12  RFMOMO              PIC  99.                             EL541
00221      12  RFDAYR              PIC  99.                             EL541
00222                                                                   EL541
00223  01  WK-AREAS.                                                    EL541
00224      12  WK1                 PIC S9(7)V99 COMP-3.                 EL541
00225      12  WK2                 PIC S9(7)V99 COMP-3.                 EL541
00226      12  WK3                 PIC  9(7)V99 COMP-3.                 EL541
00227      12  H-MASK              PIC  9(10).                          EL541
00228      12  HR-MASK REDEFINES H-MASK.                                EL541
00229          16  HRM-1           PIC  99.                             EL541
00230          16  HRM-2           PIC  9(4).                           EL541
00231          16  HRM-3           PIC  9(4).                           EL541
00232      12  P-MASK              PIC  9(10).                          EL541
00233      12  PR-MASK REDEFINES P-MASK.                                EL541
00234          16  PRM-1           PIC  99.                             EL541
00235          16  PRM-2           PIC  9(4).                           EL541
00236          16  PRM-3           PIC  9(4).                           EL541
00237      12  CODED-YRMO          PIC  9(4)        COMP.               EL541
00238      12  PRIOR-CLM-HIST-RECS PIC S9(7)        COMP-3 VALUE ZERO.  EL541
00239      12  PRIOR-RSV-HIST-RECS PIC S9(7)        COMP-3 VALUE ZERO.  EL541
00240      12  PRIOR-CERTS         PIC S9(7)        COMP-3.             EL541
00241      12  ERRX                PIC  9(4)        COMP.               EL541
00242      12  ERR-TBL             PIC  X(10)          OCCURS 50.       EL541
00243      12  ERR-AMT             PIC  9(7)V99        OCCURS 50.       EL541
00244      12  CURRX               PIC  9(4)        COMP.               EL541
00245      12  ETBL-1-VALS.                                             EL541
00246          16  FILLER PIC  X(6) VALUE 'LIFE'.                       EL541
00247          16  FILLER PIC  X(18) VALUE 'PREMIUMS'.                  EL541
00248          16  FILLER PIC  X(6) VALUE 'A/H'.                        EL541
00249          16  FILLER PIC  X(18) VALUE 'PREMIUMS'.                  EL541
00250          16  FILLER PIC  X(6) VALUE 'LIFE'.                       EL541
00251          16  FILLER PIC  X(18) VALUE 'REUNDS'.                    EL541
00252          16  FILLER PIC  X(6) VALUE 'A/H'.                        EL541
00253          16  FILLER PIC  X(18) VALUE 'REFUNDS'.                   EL541
00254          16  FILLER PIC  X(6) VALUE 'LIFE'.                       EL541
00255          16  FILLER PIC  X(18) VALUE 'NET WRITTEN'.               EL541
00256          16  FILLER PIC  X(6) VALUE 'A/H'.                        EL541
00257          16  FILLER PIC  X(18) VALUE 'NET WRITTEN'.               EL541
00258          16  FILLER PIC  X(6) VALUE 'LIFE'.                       EL541
00259          16  FILLER PIC  X(18) VALUE 'ACCT COMMISIONS'.           EL541
00260          16  FILLER PIC  X(6) VALUE 'A/H'.                        EL541
00261          16  FILLER PIC  X(18) VALUE 'ACCT COMMISSIONS'.          EL541
00262          16  FILLER PIC  X(6) VALUE 'LIFE'.                       EL541
00263          16  FILLER PIC  X(18) VALUE 'OVERRIDES'.                 EL541
00264          16  FILLER PIC  X(6) VALUE 'A/H'.                        EL541
00265          16  FILLER PIC  X(18) VALUE 'OVERRIDES'.                 EL541
00266          16  FILLER PIC  X(6) VALUE 'LIFE'.                       EL541
00267          16  FILLER PIC  X(18) VALUE 'CLAIMS'.                    EL541
00268          16  FILLER PIC  X(6) VALUE 'A/H'.                        EL541
00269          16  FILLER PIC  X(18) VALUE 'CLAIMS'.                    EL541
00270          16  FILLER PIC  X(6) VALUE 'LIFE'.                       EL541
00271          16  FILLER PIC  X(18) VALUE 'RESERVES'.                  EL541
00272          16  FILLER PIC  X(6) VALUE 'A/H'.                        EL541
00273          16  FILLER PIC  X(18) VALUE 'RESERVES'.                  EL541
00274          16  FILLER PIC  X(6) VALUE 'CERTIF'.                     EL541
00275          16  FILLER PIC  X(18) VALUE 'ICATES'.                    EL541
00276          16  FILLER PIC  X(6) VALUE 'CLAIM'.                      EL541
00277          16  FILLER PIC  X(18) VALUE 'HISTORY RECORDS'.           EL541
00278          16  FILLER PIC  X(6) VALUE 'PAYMEN'.                     EL541
00279          16  FILLER PIC  X(18) VALUE 'TS AND ADJUSTMENTS'.        EL541
00280          16  FILLER PIC  X(6) VALUE 'RETRO'.                      EL541
00281          16  FILLER PIC  X(18) VALUE 'PAYMENTS'.                  EL541
00282          16  FILLER PIC  X(6) VALUE 'NET AC'.                     EL541
00283          16  FILLER PIC  X(18) VALUE 'CT COMMISSIONS'.            EL541
00284          16  FILLER PIC  X(6) VALUE 'NET OV'.                     EL541
00285          16  FILLER PIC  X(18) VALUE 'ERRIDES'.                   EL541
00286          16  FILLER PIC  X(6) VALUE 'NET TO'.                     EL541
00287          16  FILLER PIC  X(18) VALUE 'TAL PREMIUMS'.              EL541
00288          16  FILLER PIC  X(6) VALUE 'TOTAL'.                      EL541
00289          16  FILLER PIC  X(18) VALUE 'RESERVES'.                  EL541
00290      12  ETBL-1-ITM REDEFINES                                     EL541
00291          ETBL-1-VALS         PIC  X(24)          OCCURS 22.       EL541
00292      12  ETBL-1-SET REDEFINES                                     EL541
00293              ETBL-1-VALS     OCCURS 22.                           EL541
00294          16  ETBL-1-SET-LAH       PIC X(6).                       EL541
00295          16  ETBL-1-SET-REST      PIC X(18).                      EL541
00296      12  ETBL-2-VALS.                                             EL541
00297          16  FILLER          PIC  X(6)           VALUE 'ECS010'.  EL541
00298          16  FILLER          PIC  X(6)           VALUE 'ECS018'.  EL541
00299          16  FILLER          PIC  X(6)           VALUE 'ECS019'.  EL541
00300          16  FILLER          PIC  X(6)           VALUE 'ECS030'.  EL541
00301          16  FILLER          PIC  X(6)           VALUE 'ECS032'.  EL541
00302          16  FILLER          PIC  X(6)           VALUE 'ECS035'.  EL541
00303          16  FILLER          PIC  X(6)           VALUE 'ECS038'.  EL541
00304          16  FILLER          PIC  X(6)           VALUE 'ECS041'.  EL541
00305          16  FILLER          PIC  X(6)           VALUE 'ECS048'.  EL541
00306          16  FILLER          PIC  X(6)           VALUE 'ECS050'.  EL541
00307          16  FILLER          PIC  X(6)           VALUE 'ECS061'.  EL541
00308          16  FILLER          PIC  X(6)           VALUE 'ECS080'.  EL541
00309          16  FILLER          PIC  X(6)           VALUE 'EL315'.   EL541
00310          16  FILLER          PIC  X(6)           VALUE 'EL331'.   EL541
00311          16  FILLER          PIC  X(6)           VALUE 'EL341'.   EL541
00312          16  FILLER          PIC  X(6)           VALUE 'EL501'.   EL541
00313          16  FILLER          PIC  X(6)           VALUE 'EL509'.   EL541
00314          16  FILLER          PIC  X(6)           VALUE 'EL522'.   EL541
00315          16  FILLER          PIC  X(6)           VALUE 'EL524'.   EL541
00316          16  FILLER          PIC  X(6)           VALUE 'EL525'.   EL541
00317      12  ETBL-2-ITM REDEFINES                                     EL541
00318          ETBL-2-VALS         PIC  X(6)           OCCURS 20.       EL541
00319      12  ETBL-3-VALS.                                             EL541
00320          16  FILLER PIC  X(18) VALUE '(PROCESSED OR NOT)'.        EL541
00321          16  FILLER PIC  X(18) VALUE '(PROCESSED ONLY)  '.        EL541
00322          16  FILLER PIC  X(18) VALUE '(PRIOR MONTH OUT) '.        EL541
00323          16  FILLER PIC  X(18) VALUE '(STANDARD)        '.        EL541
00324          16  FILLER PIC  X(18) VALUE '(RECALCULATED)    '.        EL541
00325      12  ETBL-3-ITM REDEFINES                                     EL541
00326          ETBL-3-VALS         PIC  X(18)          OCCURS 5.        EL541
00327      12  SPL-MSG-1.                                               EL541
00328          16  FILLER          PIC  X(10)          VALUE SPACE.     EL541
00329          16  FILLER          PIC  X(40)          VALUE            EL541
00330                  'YOU ARE ABSOLUTELY PERFECT. THANK YOU.'.        EL541
00331      12  SPL-MSG-2.                                               EL541
00332          16  FILLER          PIC  X(10)          VALUE SPACE.     EL541
00333          16  FILLER          PIC  X(40)          VALUE            EL541
00334                  'THERE ARE NO DISCREPANCIES.'.                   EL541
00335      12  DECODE-P.                                                EL541
00336          16  DP-1            PIC  99.                             EL541
00337          16  DP-2            PIC  99.                             EL541
00338          16  DP-3            PIC  99.                             EL541
00339          16  DP-4            PIC  99.                             EL541
00340          16  DP-5            PIC  99.                             EL541
00341      12  X1                  PIC  9(4)   COMP.                    EL541
00342      12  X2                  PIC  9(4)   COMP.                    EL541
00343      12  X3                  PIC  9(4)   COMP.                    EL541
00344      12  X4                  PIC  9(4)   COMP.                    EL541
00345      12  SVTOP               PIC  9(4)   COMP.                    EL541
00346      12  DUR-DATA.                                                EL541
00347          16  DH-JOB          PIC  X(6).                           EL541
00348          16  DH-RUN          PIC  9(6).                           EL541
00349          16  DH-DTS REDEFINES DH-RUN.                             EL541
00350              20  DH-RUNMO    PIC  99.                             EL541
00351              20  DH-RUNDA    PIC  99.                             EL541
00352              20  DH-RUNYR    PIC  99.                             EL541
00353          16  DH-START        PIC  9(6).                           EL541
00354          16  DH-STS REDEFINES DH-START.                           EL541
00355              20  DH-STHR     PIC  99.                             EL541
00356              20  DH-STMN     PIC  99.                             EL541
00357              20  DH-STSC     PIC  99.                             EL541
00358          16  DH-END          PIC  9(6).                           EL541
00359          16  DH-ENS REDEFINES DH-END.                             EL541
00360              20  DH-ENHR     PIC  99.                             EL541
00361              20  DH-ENMN     PIC  99.                             EL541
00362              20  DH-ENSC     PIC  99.                             EL541
00363          16  DH-COUNT        PIC S9(3)   COMP-3.                  EL541
00364      12  SIGN-HR             PIC S9(3).                           EL541
00365      12  SIGN-MN             PIC S9(3).                           EL541
00366      12  SIGN-SC             PIC S9(3).                           EL541
00367      12  EXPND-HR            PIC S9(3).                           EL541
00368      12  EXPND-MN            PIC S9(3).                           EL541
00369      12  EXPND-SC            PIC S9(3).                           EL541
00370                                                                   EL541
00371  01  EXP-LINE.                                                    EL541
00372      12  EXL-1              PIC  X(25).                           EL541
00373      12  FILLER             PIC  X(5)            VALUE 'FROM'.    EL541
00374      12  EXL-2              PIC  X(7).                            EL541
00375      12  EXL-3              PIC  X(19).                           EL541
00376      12  FILLER             PIC  X(5)            VALUE 'OVER'.    EL541
00377      12  EXL-4              PIC  X(7).                            EL541
00378      12  EXL-5              PIC  X(19).                           EL541
00379      12  FILLER             PIC  X(3)            VALUE 'BY'.      EL541
00380      12  EXL-AMT            PIC Z,ZZZ,ZZZ.99.                     EL541
00381                                                                   EL541
00382  01  CMP-LINE.                                                    EL541
00383      12  CHAR               PIC  X           OCCURS 102.          EL541
00384  EJECT                                                            EL541
00385     COPY ELCDATE.                                                    CL**6
00386                                                                   EL541
00387     COPY ELCDTECX.                                                EL541
00388                                                                   EL541
00389     COPY ELCDTEVR.                                                EL541
00390                                                                   EL541
00391  EJECT                                                            EL541
00392  PROCEDURE DIVISION.                                              EL541
00393                                                                   EL541
00394  0000-GET-DATE.                                                   EL541
00395                              COPY ELCDTERX.                       EL541
00396                                                                   EL541
00397  0100-GET-CURR.                                                   EL541
00398      MOVE  COMPANY-NAME          TO  HD2-CO.                      EL541
00399      MOVE  ALPH-DATE             TO  HD3-DATE.                    EL541
00400                                                                   EL541
00401      OPEN INPUT ERMEBL.                                           EL541
00402                                                                   EL541
00403      IF ME-STATUS  = '00' OR '97'                                 EL541
00404          NEXT SENTENCE                                            EL541
00405        ELSE                                                       EL541
00406          MOVE ME-STATUS          TO WS-ABEND-FILE-STATUS          EL541
00407          MOVE 'ERMEBL OPEN ERROR- ' TO WS-ABEND-MESSAGE           EL541
00408          PERFORM ABEND-PGM.                                       EL541
00409                                                                   EL541
00410      IF DTE-PRT-OPT  = 'P'  OR  'B'  OR  'T'                      EL541
00411          OPEN OUTPUT PRINT-FILE.                                  EL541
00412                                                                   EL541
00413      MOVE LIFE-OVERRIDE-L6       TO HD-5-L1                       EL541
00414                                     HD-5-L2.                      EL541
00415      MOVE AH-OVERRIDE-L6         TO HD-5-A1                       EL541
00416                                     HD-5-A2.                      EL541
00417                                                                   EL541
00418      PERFORM 0800-SET-LAH THRU 0800-SET-LAH-EXIT                  EL541
00419         VARYING X4 FROM 1 BY 1 UNTIL X4 GREATER THAN 22.          EL541
00420                                                                   EL541
00421  0200-SET-UPS.                                                    EL541
00422      MOVE  DTE-CLIENT            TO  ME-COMPANY.                  EL541
00423                                                                   EL541
00424      COMPUTE  CODED-YRMO  =  RUN-CCYY  *  12  +  RUN-MO  -  1.    EL541
00425                                                                   EL541
00426      MOVE  ZERO                  TO  PRIOR-CLM-HIST-RECS          EL541
00427                                      PRIOR-CERTS.                 EL541
00428      MOVE  CODED-YRMO            TO  ME-MOYR.                     EL541
00429                                                                   EL541
00430      READ  ERMEBL  INVALID KEY                                    EL541
00431          GO TO 0210-NO-PRIORS.                                    EL541
00432                                                                   EL541
00433      MOVE  ME-010-CERT-OUT       TO  PRIOR-CERTS.                 EL541
00434      MOVE  ME-038-RECS-OUT       TO  PRIOR-CLM-HIST-RECS.         EL541
00435                                                                   EL541
00436      IF ME-048-RECS-OUT NUMERIC                                   EL541
00437          MOVE ME-048-RECS-OUT    TO  PRIOR-RSV-HIST-RECS          EL541
00438      ELSE                                                         EL541
00439          MOVE ZEROS              TO  PRIOR-RSV-HIST-RECS.         EL541
00440                                                                   EL541
00441  0210-NO-PRIORS.                                                  EL541
00442      ADD  1                      TO  ME-MOYR.                     EL541
00443                                                                   EL541
00444      READ  ERMEBL  INVALID KEY                                    EL541
00445          MOVE  'N'               TO  CURRENT-RECORD-SWITCH        EL541
00446          DISPLAY 'NO INPUT RECORD FOR EL541'  UPON CONSOLE.       EL541
00447                                                                   EL541
00448  0300-PAGE-1.                                                     EL541
00449      MOVE  HD-1                  TO  P-DATA.                      EL541
00450      MOVE  '1'                   TO  P-CTL.                          CL**2
00451                                                                   EL541
00452      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00453                                                                   EL541
00454      MOVE  WS-CURRENT-DATE       TO  HD2-RUN.                     EL541
00455      MOVE  HD-2                  TO  P-DATA.                      EL541
00456      MOVE  SPACE                 TO  P-CTL.                          CL**3
00457      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00458                                                                   EL541
00459      ADD  1                      TO  PGCT.                        EL541
00460                                                                   EL541
00461      MOVE  PGCT                  TO  HD3-PG.                      EL541
00462      MOVE  HD-3                  TO  P-DATA.                      EL541
00463      MOVE  SPACE                 TO  P-CTL.                          CL**3
00464      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00465                                                                   EL541
00466      MOVE  HD-4                  TO  P-DATA.                      EL541
00467      MOVE  ZERO                  TO  P-CTL.                       EL541
00468      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00469                                                                   EL541
00470      MOVE  HD-5                  TO  P-DATA.                      EL541
00471      MOVE  SPACE                 TO  P-CTL.                          CL**3
00472      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00473                                                                   EL541
00474      IF CURRENT-RECORD                                            EL541
00475          NEXT SENTENCE                                            EL541
00476      ELSE                                                         EL541
00477          MOVE  'NO INPUT RECORD FOR EL541'  TO  DT-1              EL541
00478          MOVE  DT-1                         TO  P-DATA            EL541
00479          MOVE  ZERO                         TO  P-CTL             EL541
00480          PERFORM 1300-PRINT-LINE  THRU  1399-EXIT                 EL541
00481          GO TO EOJ.                                               EL541
00482                                                                   EL541
00483      MOVE  '         FROM ECS010'  TO  DT-1.                      EL541
00484      MOVE  ME-010-PREM-L           TO  D1-TTL1.                   EL541
00485      MOVE  ME-010-PREM-AH          TO  D1-TTL2.                   EL541
00486      MOVE  ME-010-REF-L            TO  D1-TTL3.                   EL541
00487      MOVE  ME-010-REF-AH           TO  D1-TTL4.                   EL541
00488      MOVE  DT-1                    TO  P-DATA.                    EL541
00489      MOVE  ZERO                    TO  P-CTL.                     EL541
00490                                                                   EL541
00491      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00492                                                                   EL541
00493      MOVE  '              ECS019'  TO  DT-1.                      EL541
00494      MOVE  ME-019-PREM-L           TO  D1-TTL1.                   EL541
00495      MOVE  ME-019-PREM-AH          TO  D1-TTL2.                   EL541
00496      MOVE  ME-019-REF-L            TO  D1-TTL3.                   EL541
00497      MOVE  ME-019-REF-AH           TO  D1-TTL4.                   EL541
00498      MOVE  DT-1                    TO  P-DATA.                    EL541
00499      MOVE  SPACE                   TO  P-CTL.                        CL**3
00500      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00501                                                                   EL541
00502      MOVE  '   TOTAL FROM ECS061'  TO  DT-1.                      EL541
00503      MOVE  ME-061-PREM             TO  D1-TTL1.                   EL541
00504      MOVE  DT-1                    TO  P-DATA.                    EL541
00505      MOVE  SPACE                   TO  P-CTL.                        CL**3
00506      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00507                                                                   EL541
00508      MOVE  '               EL522'  TO  DT-1.                      EL541
00509      MOVE  ME-522-PREM-L           TO  D1-TTL1.                   EL541
00510      MOVE  ME-522-PREM-AH          TO  D1-TTL2.                   EL541
00511      MOVE  ME-522-REF-L            TO  D1-TTL3.                   EL541
00512      MOVE  ME-522-REF-AH           TO  D1-TTL4.                   EL541
00513      MOVE  DT-1                    TO  P-DATA.                    EL541
00514      MOVE  SPACE                   TO  P-CTL.                        CL**3
00515      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00516                                                                   EL541
00517      MOVE  HD-6                    TO  P-DATA.                    EL541
00518      MOVE  '-'                     TO  P-CTL.                        CL**2
00519      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00520                                                                   EL541
00521      MOVE  HD-5                    TO  P-DATA.                    EL541
00522      MOVE  SPACE                   TO  P-CTL.                        CL**3
00523      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00524                                                                   EL541
00525      MOVE  'ISSUES-CANCEL ECS010'  TO  DT-1.                      EL541
00526      MOVE  ME-010-NET-L            TO  D1-TTL1.                   EL541
00527      MOVE  ME-010-NET-AH           TO  D1-TTL2.                   EL541
00528      MOVE  DT-1                    TO  P-DATA.                    EL541
00529      MOVE  ZERO                    TO  P-CTL.                     EL541
00530      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00531                                                                   EL541
00532      MOVE  'MONTH TO DATE-ECS035'  TO  DT-1.                      EL541
00533      MOVE  ME-035-NET-L            TO  D1-TTL1.                   EL541
00534      MOVE  ME-035-NET-AH           TO  D1-TTL2.                   EL541
00535      MOVE  DT-1                    TO  P-DATA.                    EL541
00536      MOVE  SPACE                   TO  P-CTL.                        CL**3
00537      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00538                                                                   EL541
00539      MOVE  HD-7                    TO  P-DATA.                    EL541
00540      MOVE  '-'                     TO  P-CTL.                        CL**2
00541      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00542                                                                   EL541
00543      MOVE  HD-5                    TO  P-DATA.                    EL541
00544      MOVE  SPACE                   TO  P-CTL.                        CL**3
00545      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00546                                                                   EL541
00547      MOVE  '         FROM ECS010'  TO  DT-1.                      EL541
00548      MOVE  ME-010-COMM-L           TO  D1-TTL1.                   EL541
00549      MOVE  ME-010-COMM-AH          TO  D1-TTL2.                   EL541
00550      MOVE  DT-1                    TO  P-DATA.                    EL541
00551      MOVE  ZERO                    TO  P-CTL.                     EL541
00552      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00553                                                                   EL541
00554      MOVE  '              ECS018'  TO  DT-1.                      EL541
070714     compute d1-ttl1 = me-018-comm-y + me-018-comm-1
00555 *    MOVE  ME-018-COMM-L           TO  D1-TTL1.                   EL541
00556 *    MOVE  ME-018-COMM-AH          TO  D1-TTL2.                   EL541
070714     compute d1-ttl3 = me-018-ow-y + me-018-ow-1
00557 *    MOVE  ME-018-OR-L             TO  D1-TTL3.                   EL541
00558 *    MOVE  ME-018-OR-AH            TO  D1-TTL4.                   EL541
00559      MOVE  DT-1                    TO  P-DATA.                    EL541
00560      MOVE  SPACE                   TO  P-CTL.                        CL**3
00561      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00562                                                                   EL541
00563      MOVE  '              ECS019'  TO  DT-1.                      EL541
00564      MOVE  ME-019-COMM-L           TO  D1-TTL1.                   EL541
00565      MOVE  ME-019-COMM-AH          TO  D1-TTL2.                   EL541
00566      MOVE  ME-019-OR-L             TO  D1-TTL3.                   EL541
00567      MOVE  ME-019-OR-AH            TO  D1-TTL4.                   EL541
00568      MOVE  DT-1                    TO  P-DATA.                    EL541
00569      MOVE  SPACE                   TO  P-CTL.                        CL**3
00570      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00571                                                                   EL541
00572      MOVE  '   TOTAL FROM ECS061'  TO  DT-1.                      EL541
00573      MOVE  ME-061-COMM             TO  D1-TTL1.                   EL541
00574      MOVE  ME-061-OR               TO  D1-TTL3.                   EL541
00575      MOVE  DT-1                    TO  P-DATA.                    EL541
00576      MOVE  SPACE                   TO  P-CTL.                        CL**3
00577      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00578                                                                   EL541
00579      MOVE  HD-8                    TO  P-DATA.                    EL541
00580      MOVE  '-'                     TO  P-CTL.                        CL**2
00581      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00582                                                                   EL541
00583      MOVE  HD-5                    TO  P-DATA.                    EL541
00584      MOVE  SPACE                   TO  P-CTL.                        CL**3
00585      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00586                                                                   EL541
00587      MOVE  '           ALL EL522'  TO  DT-1.                      EL541
00588      MOVE  ME-522-ALL-CLM-L        TO  D1-TTL1.                   EL541
00589      MOVE  ME-522-ALL-CLM-AH       TO  D1-TTL2.                   EL541
00590      MOVE  ME-522-ALL-RSV-L        TO  D1-TTL3.                   EL541
00591      MOVE  ME-522-ALL-RSV-AH       TO  D1-TTL4.                   EL541
00592      MOVE  DT-1                    TO  P-DATA.                    EL541
00593      MOVE  SPACE                   TO  P-CTL.                        CL**3
00594      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00595                                                                   EL541
00596      MOVE  '               EL524'  TO  DT-1.                      EL541
00597      MOVE  ME-524-CLMS-L           TO  D1-TTL1.                   EL541
00598      MOVE  ME-524-CLMS-AH          TO  D1-TTL2.                   EL541
00599      MOVE  ME-524-RESV-L           TO  D1-TTL3.                   EL541
00600      MOVE  ME-524-RESV-AH          TO  D1-TTL4.                   EL541
00601      MOVE  DT-1                    TO  P-DATA.                    EL541
00602      MOVE  SPACE                   TO  P-CTL.                        CL**3
00603      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00604                                                                   EL541
00605      MOVE  '         FROM ECS010'  TO  DT-1.                      EL541
00606      MOVE  ME-010-PMT-L            TO  D1-TTL1.                   EL541
00607      MOVE  ME-010-PMT-AH           TO  D1-TTL2.                   EL541
00608      MOVE  DT-1                    TO  P-DATA.                    EL541
00609      MOVE  ZERO                    TO  P-CTL.                     EL541
00610      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00611                                                                   EL541
00612      MOVE  '          GOOD EL522'  TO  DT-1.                      EL541
00613      MOVE  ME-522-PROC-CLM-L       TO  D1-TTL1.                   EL541
00614      MOVE  ME-522-PROC-CLM-AH      TO  D1-TTL2.                   EL541
00615      MOVE  ME-522-PROC-RSV-L       TO  D1-TTL3.                   EL541
00616      MOVE  ME-522-PROC-RSV-AH      TO  D1-TTL4.                   EL541
00617      MOVE  DT-1                    TO  P-DATA.                    EL541
00618      MOVE  ZERO                    TO  P-CTL.                     EL541
00619      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00620                                                                   EL541
00621      MOVE  '              ECS030'  TO  DT-1.                      EL541
00622      MOVE  ME-030-CLMS-L           TO  D1-TTL1.                   EL541
00623      MOVE  ME-030-CLMS-AH          TO  D1-TTL2.                   EL541
00624      MOVE  DT-1                    TO  P-DATA.                    EL541
00625      MOVE  SPACE                   TO  P-CTL.                        CL**3
00626      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00627                                                                   EL541
00628      MOVE  '              ECS032'  TO  DT-1.                      EL541
00629      MOVE  ME-032-RESV-L           TO  D1-TTL3.                   EL541
00630      MOVE  ME-032-RESV-AH          TO  D1-TTL4.                   EL541
00631      MOVE  DT-1                    TO  P-DATA.                    EL541
00632      MOVE  SPACE                   TO  P-CTL.                        CL**3
00633      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00634                                                                   EL541
00635      MOVE  '      ALL FROM EL315'  TO  DT-1.                      EL541
00636      MOVE  ME-315-RESV-L           TO  D1-TTL3.                   EL541
00637      MOVE  ME-315-RESV-AH          TO  D1-TTL4.                   EL541
00638      MOVE  DT-1                    TO  P-DATA.                    EL541
00639      MOVE  ZERO                    TO  P-CTL.                     EL541
00640      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00641                                                                   EL541
00642      MOVE  HD-8A                   TO  P-DATA.                    EL541
00643      MOVE  '-'                     TO  P-CTL.                        CL**2
00644      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00645                                                                   EL541
00646      MOVE  '          FROM EL522'  TO  DT-1.                      EL541
00647      MOVE  ME-522-RETROS           TO  D1-TTL1.                   EL541
00648      MOVE  DT-1                    TO  P-DATA.                    EL541
00649      MOVE  ZERO                    TO  P-CTL.                     EL541
00650      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00651                                                                   EL541
00652      MOVE  '              ECS041'  TO  DT-1.                      EL541
00653      MOVE  ME-041-RETROS           TO  D1-TTL1.                   EL541
00654      MOVE  DT-1                    TO  P-DATA.                    EL541
00655      MOVE  SPACE                   TO  P-CTL.                        CL**3
00656      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00657                                                                   EL541
00658      MOVE  HD-8B                   TO  P-DATA.                       CL**5
00659      MOVE  '-'                     TO  P-CTL.                        CL**2
00660      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00661                                                                   EL541
00662      MOVE  '         FROM ECS061'  TO  DT-1.                      EL541
00663      MOVE  ME-061-PY-ADJ           TO  D1-TTL1.                   EL541
00664      MOVE  DT-1                    TO  P-DATA.                    EL541
00665      MOVE  SPACE                   TO  P-CTL.                        CL**3
00666      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00667                                                                   EL541
00668      MOVE  '               EL522'  TO  DT-1.                      EL541
00669      MOVE  ME-522-PY-ADJ           TO  D1-TTL1.                   EL541
00670      MOVE  DT-1                    TO  P-DATA.                    EL541
00671      MOVE  SPACE                   TO  P-CTL.                        CL**3
00672      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
00673                                                                      CL**5
00674      MOVE  HD-8C                   TO  P-DATA.                       CL**5
00675      MOVE  '-'                     TO  P-CTL.                        CL**5
00676      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                       CL**5
00677                                                                      CL**5
00678      MOVE  '         FROM ECS041'  TO  DT-1.                         CL**5
00679      IF    ME-041-REIN-ADJ NOT NUMERIC                               CL**5
00680          MOVE ZEROS TO ME-041-REIN-ADJ.                              CL**5
00681      MOVE  ME-041-REIN-ADJ         TO  D1-TTL1.                      CL**5
00682      MOVE  DT-1                    TO  P-DATA.                       CL**5
00683      MOVE  SPACE                   TO  P-CTL.                        CL**5
00684      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                       CL**5
00685                                                                   EL541
00686  0500-TEST.                                                       EL541
00687      MOVE  ZERO                  TO  ERRX.                        EL541
00688      MOVE  ME-522-PREM-L         TO  WK1.                         EL541
00689      MOVE  ME-010-PREM-L         TO  WK2.                         EL541
00690      MOVE  0118000100            TO  H-MASK.                      EL541
00691                                                                   EL541
00692      IF ME-522-RUN-DT GREATER ZERO  AND                           EL541
00693         ME-010-RUN-DT GREATER ZERO                                EL541
00694          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00695                                                                   EL541
00696      MOVE  ME-019-PREM-L         TO  WK2.                         EL541
00697      MOVE  0300                  TO  HRM-3.                       EL541
00698                                                                   EL541
00699      IF ME-522-RUN-DT GREATER ZERO  AND                           EL541
00700         ME-019-RUN-DT GREATER ZERO                                EL541
00701          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00702                                                                   EL541
00703      MOVE  ME-010-PREM-L         TO  WK1.                         EL541
00704      MOVE  0100                  TO  HRM-2.                       EL541
00705                                                                   EL541
00706      IF ME-010-RUN-DT GREATER ZERO  AND                           EL541
00707         ME-019-RUN-DT GREATER ZERO                                EL541
00708          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00709                                                                   EL541
00710      MOVE  ME-522-PREM-AH        TO  WK1.                         EL541
00711      MOVE  ME-010-PREM-AH        TO  WK2.                         EL541
00712      MOVE  0218000100            TO  H-MASK.                      EL541
00713                                                                   EL541
00714      IF ME-522-RUN-DT GREATER ZERO  AND                           EL541
00715         ME-010-RUN-DT GREATER ZERO                                EL541
00716          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00717                                                                   EL541
00718      MOVE  ME-019-PREM-AH        TO  WK2.                         EL541
00719      MOVE  0300                  TO  HRM-3.                       EL541
00720                                                                   EL541
00721      IF ME-522-RUN-DT GREATER ZERO  AND                           EL541
00722         ME-019-RUN-DT GREATER ZERO                                EL541
00723          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00724                                                                   EL541
00725      MOVE  ME-010-PREM-AH        TO  WK1.                         EL541
00726      MOVE  0100                  TO  HRM-2.                       EL541
00727                                                                   EL541
00728      IF ME-010-RUN-DT GREATER ZERO AND                            EL541
00729         ME-019-RUN-DT GREATER ZERO                                EL541
00730          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00731                                                                   EL541
00732      MOVE  ME-522-REF-L          TO  WK1.                         EL541
00733      MOVE  ME-010-REF-L          TO  WK2.                         EL541
00734      MOVE  0318000100            TO  H-MASK.                      EL541
00735                                                                   EL541
00736      IF ME-522-RUN-DT GREATER ZERO AND                            EL541
00737         ME-010-RUN-DT GREATER ZERO                                EL541
00738          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00739                                                                   EL541
00740      MOVE  ME-019-REF-L          TO  WK2.                         EL541
00741      MOVE  0300                  TO  HRM-3.                       EL541
00742                                                                   EL541
00743      IF ME-522-RUN-DT GREATER ZERO AND                            EL541
00744         ME-019-RUN-DT GREATER ZERO                                EL541
00745          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00746                                                                   EL541
00747      MOVE  ME-010-REF-L          TO  WK1.                         EL541
00748      MOVE  0100                  TO  HRM-2.                       EL541
00749                                                                   EL541
00750      IF ME-010-RUN-DT GREATER ZERO AND                            EL541
00751         ME-019-RUN-DT GREATER ZERO                                EL541
00752          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00753                                                                   EL541
00754      MOVE  ME-522-REF-AH         TO  WK1.                         EL541
00755      MOVE  ME-010-REF-AH         TO  WK2.                         EL541
00756      MOVE  0418000100            TO  H-MASK.                      EL541
00757                                                                   EL541
00758      IF ME-522-RUN-DT GREATER ZERO AND                            EL541
00759         ME-010-RUN-DT GREATER ZERO                                EL541
00760          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00761                                                                   EL541
00762      MOVE  ME-019-REF-AH         TO  WK2.                         EL541
00763      MOVE  0300                  TO  HRM-3.                       EL541
00764                                                                   EL541
00765      IF ME-522-RUN-DT GREATER ZERO AND                            EL541
00766         ME-019-RUN-DT GREATER ZERO                                EL541
00767          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00768                                                                   EL541
00769      MOVE  ME-010-REF-AH         TO  WK1.                         EL541
00770      MOVE  0100                  TO  HRM-2.                       EL541
00771                                                                   EL541
00772      IF ME-010-RUN-DT GREATER ZERO AND                            EL541
00773         ME-019-RUN-DT GREATER ZERO                                EL541
00774          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00775                                                                   EL541
00776      SUBTRACT  ME-010-REF-L  FROM  ME-010-PREM-L  GIVING  WK1.    EL541
00777                                                                   EL541
00778      MOVE  ME-035-NET-L          TO  WK2.                         EL541
00779      MOVE  0501000600            TO  H-MASK.                      EL541
00780                                                                   EL541
00781      IF ME-010-RUN-DT GREATER ZERO AND                            EL541
00782         ME-035-RUN-DT GREATER ZERO                                EL541
00783          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00784                                                                   EL541
00785      SUBTRACT  ME-010-REF-AH  FROM  ME-010-PREM-AH  GIVING  WK1.  EL541
00786                                                                   EL541
00787      MOVE  ME-035-NET-AH         TO  WK2.                         EL541
00788      MOVE  06                    TO  HRM-1.                       EL541
00789                                                                   EL541
00790      IF ME-010-RUN-DT GREATER ZERO AND                            EL541
00791         ME-035-RUN-DT GREATER ZERO                                EL541
00792          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00793                                                                   EL541
00794      COMPUTE  WK1  =  ME-522-PREM-L  +  ME-522-PREM-AH            EL541
00795                    -  ME-522-REF-L  -  ME-522-REF-AH.             EL541
00796                                                                   EL541
00797      MOVE  ME-061-PREM           TO  WK2.                         EL541
00798      MOVE  2118001100            TO  H-MASK.                      EL541
00799                                                                   EL541
00800      IF ME-522-RUN-DT GREATER ZERO AND                            EL541
00801         ME-061-RUN-DT GREATER ZERO                                EL541
00802          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00803                                                                   EL541
00804      COMPUTE  WK1  =  ME-010-PREM-L  +  ME-010-PREM-AH            EL541
00805                    -  ME-010-REF-L  -  ME-010-REF-AH.             EL541
00806                                                                   EL541
00807      MOVE  0100                  TO  HRM-2.                       EL541
00808                                                                   EL541
00809      IF ME-010-RUN-DT GREATER ZERO AND                            EL541
00810         ME-061-RUN-DT GREATER ZERO                                EL541
00811          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00812                                                                   EL541
00813      COMPUTE  WK1  =  ME-019-PREM-L  +  ME-019-PREM-AH            EL541
00814                    -  ME-019-REF-L  -  ME-019-REF-AH.             EL541
00815                                                                   EL541
00816      MOVE  0300                  TO  HRM-2.                       EL541
00817                                                                   EL541
00818      IF ME-019-RUN-DT GREATER ZERO AND                            EL541
00819         ME-061-RUN-DT GREATER ZERO                                EL541
00820          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00821                                                                   EL541
00822      MOVE  ME-010-COMM-L         TO  WK1.                         EL541
00823      MOVE  ME-019-COMM-L         TO  WK2.                         EL541
00824      MOVE  0701000300            TO  H-MASK.                      EL541
00825                                                                   EL541
00826      IF ME-010-RUN-DT GREATER ZERO AND                            EL541
00827         ME-019-RUN-DT GREATER ZERO                                EL541
00828          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00829                                                                   EL541
00830      MOVE  ME-010-COMM-AH        TO  WK1.                         EL541
00831      MOVE  ME-019-COMM-AH        TO  WK2.                         EL541
00832      MOVE  08                    TO  HRM-1.                       EL541
00833                                                                   EL541
00834      IF ME-010-RUN-DT GREATER ZERO AND                            EL541
00835         ME-019-RUN-DT GREATER ZERO                                EL541
00836          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00837                                                                   EL541
00838      ADD  ME-010-COMM-L  ME-010-COMM-AH  GIVING  WK1.             EL541
00839                                                                   EL541
00840      MOVE  ME-061-COMM           TO  WK2.                         EL541
00841      MOVE  1901001104            TO  H-MASK.                      EL541
00842                                                                   EL541
00843      IF ME-010-RUN-DT GREATER ZERO AND                            EL541
00844         ME-061-RUN-DT GREATER ZERO                                EL541
00845          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00846                                                                   EL541
00847      ADD  ME-019-COMM-L  ME-019-COMM-AH  GIVING  WK1.             EL541
00848                                                                   EL541
00849      MOVE  0300                  TO  HRM-2.                       EL541
00850                                                                   EL541
00851      IF ME-019-RUN-DT GREATER ZERO AND                            EL541
00852         ME-061-RUN-DT GREATER ZERO                                EL541
00853          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00854                                                                   EL541
070714     compute wk1 = me-018-comm-y + me-018-comm-1
00855 *    ADD  ME-018-COMM-L  ME-018-COMM-AH  GIVING  WK1.             EL541
00856                                                                   EL541
00857      MOVE  ME-061-COMM-RCALC     TO  WK2.                         EL541
00858      MOVE  1902001105            TO  H-MASK.                      EL541
00859                                                                   EL541
00860      IF ME-018-RUN-DT GREATER ZERO AND                            EL541
00861         ME-061-RUN-DT GREATER ZERO                                EL541
00862          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00863                                                                   EL541
00864      ADD  ME-019-OR-L  ME-019-OR-AH  GIVING  WK1.                 EL541
00865                                                                   EL541
00866      MOVE  ME-061-OR             TO  WK2.                         EL541
00867      MOVE  2003001104            TO  H-MASK.                      EL541
00868                                                                   EL541
00869      IF ME-019-RUN-DT GREATER ZERO AND                            EL541
00870         ME-061-RUN-DT GREATER ZERO                                EL541
00871          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00872                                                                   EL541
070714     compute wk1 = me-018-ow-y + me-018-ow-1
00873 *    ADD  ME-018-OR-L  ME-018-OR-AH  GIVING  WK1.                 EL541
00874                                                                   EL541
00875      MOVE  ME-061-OR-RCALC       TO  WK2.                         EL541
00876      MOVE  2002001105            TO  H-MASK.                      EL541
00877                                                                   EL541
00878      IF ME-018-RUN-DT GREATER ZERO AND                            EL541
00879         ME-061-RUN-DT GREATER ZERO                                EL541
00880          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00881                                                                   EL541
00882      MOVE  ME-524-CLMS-L         TO  WK1.                         EL541
00883      MOVE  ME-522-ALL-CLM-L      TO  WK2.                         EL541
00884      MOVE  1119001801            TO  H-MASK.                      EL541
00885                                                                   EL541
00886      IF ME-524-RUN-DT GREATER ZERO AND                            EL541
00887         ME-522-RUN-DT GREATER ZERO                                EL541
00888          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00889                                                                   EL541
00890      MOVE  ME-522-PROC-CLM-L     TO  WK1.                         EL541
00891      MOVE  ME-010-PMT-L          TO  WK2.                         EL541
00892      MOVE  1118020100            TO  H-MASK.                      EL541
00893                                                                   EL541
00894      IF ME-522-RUN-DT GREATER ZERO AND                            EL541
00895         ME-010-RUN-DT GREATER ZERO                                EL541
00896          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00897                                                                   EL541
00898      MOVE  ME-030-CLMS-L         TO  WK2.                         EL541
00899      MOVE  0400                  TO  HRM-3.                       EL541
00900                                                                   EL541
00901      IF ME-522-RUN-DT GREATER ZERO AND                            EL541
00902         ME-030-RUN-DT GREATER ZERO                                EL541
00903          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00904                                                                   EL541
00905      MOVE  ME-010-PMT-L          TO  WK1.                         EL541
00906      MOVE  0100                  TO  HRM-2.                       EL541
00907                                                                   EL541
00908      IF ME-010-RUN-DT GREATER ZERO AND                            EL541
00909         ME-030-RUN-DT GREATER ZERO                                EL541
00910          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00911                                                                   EL541
00912      MOVE  ME-524-CLMS-AH        TO  WK1.                         EL541
00913      MOVE  ME-522-ALL-CLM-AH     TO  WK2.                         EL541
00914      MOVE  1219001801            TO  H-MASK.                      EL541
00915                                                                   EL541
00916      IF ME-524-RUN-DT GREATER ZERO AND                            EL541
00917         ME-522-RUN-DT GREATER ZERO                                EL541
00918          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00919                                                                   EL541
00920      MOVE  ME-522-PROC-CLM-AH    TO  WK1.                         EL541
00921      MOVE  ME-010-PMT-AH         TO  WK2.                         EL541
00922      MOVE  1218020100            TO  H-MASK.                      EL541
00923                                                                   EL541
00924      IF ME-522-RUN-DT GREATER ZERO AND                            EL541
00925         ME-010-RUN-DT GREATER ZERO                                EL541
00926          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00927                                                                   EL541
00928      MOVE  ME-030-CLMS-AH        TO  WK2.                         EL541
00929      MOVE  0400                  TO  HRM-3.                       EL541
00930                                                                   EL541
00931      IF ME-522-RUN-DT GREATER ZERO AND                            EL541
00932         ME-030-RUN-DT GREATER ZERO                                EL541
00933          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00934                                                                   EL541
00935      MOVE  ME-010-PMT-AH         TO  WK1.                         EL541
00936      MOVE  0100                  TO  HRM-2.                       EL541
00937                                                                   EL541
00938      IF ME-010-RUN-DT GREATER ZERO AND                            EL541
00939         ME-030-RUN-DT GREATER ZERO                                EL541
00940          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00941                                                                   EL541
00942      MOVE  ME-315-RESV-L         TO  WK1.                         EL541
00943                                                                   EL541
00944      ADD  ME-524-RESV-L  ME-524-RESV-AH  GIVING  WK2.             EL541
00945                                                                   EL541
00946      MOVE  2213001900            TO  H-MASK.                      EL541
00947                                                                   EL541
00948      IF ME-315-RUN-DT GREATER ZERO AND                            EL541
00949         ME-524-RUN-DT GREATER ZERO                                EL541
00950          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00951                                                                   EL541
00952      ADD  ME-522-ALL-RSV-L  ME-522-ALL-RSV-AH  GIVING  WK2.       EL541
00953                                                                   EL541
00954      MOVE  2213001801            TO  H-MASK.                      EL541
00955                                                                   EL541
00956      IF ME-315-RUN-DT GREATER ZERO AND                            EL541
00957         ME-522-RUN-DT GREATER ZERO                                EL541
00958          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00959                                                                   EL541
00960      MOVE  ME-524-RESV-L         TO  WK1.                         EL541
00961      MOVE  ME-522-ALL-RSV-L      TO  WK2.                         EL541
00962      MOVE  1319001801            TO  H-MASK.                      EL541
00963                                                                   EL541
00964      IF ME-524-RUN-DT GREATER ZERO AND                            EL541
00965         ME-522-RUN-DT GREATER ZERO                                EL541
00966          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00967                                                                   EL541
00968      MOVE  ME-522-PROC-RSV-L     TO  WK1.                         EL541
00969      MOVE  ME-032-RESV-L         TO  WK2.                         EL541
00970      MOVE  1318020500            TO  H-MASK.                      EL541
00971                                                                   EL541
00972      IF ME-522-RUN-DT GREATER ZERO AND                            EL541
00973         ME-032-RUN-DT GREATER ZERO                                EL541
00974          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00975                                                                   EL541
00976      MOVE  ME-522-ALL-RSV-AH     TO  WK2.                         EL541
00977      MOVE  ME-524-RESV-AH        TO  WK1.                         EL541
00978      MOVE  1419001801            TO  H-MASK.                      EL541
00979                                                                   EL541
00980      IF ME-524-RUN-DT GREATER ZERO AND                            EL541
00981         ME-522-RUN-DT GREATER ZERO                                EL541
00982          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00983                                                                   EL541
00984      MOVE  ME-522-PROC-RSV-AH    TO  WK1.                         EL541
00985      MOVE  ME-032-RESV-AH        TO  WK2.                         EL541
00986      MOVE  1418020500            TO  H-MASK.                      EL541
00987                                                                   EL541
00988      IF ME-522-RUN-DT GREATER ZERO AND                            EL541
00989         ME-032-RUN-DT GREATER ZERO                                EL541
00990          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00991                                                                   EL541
00992      MOVE  ME-041-RETROS         TO  WK1.                         EL541
00993      MOVE  ME-522-RETROS         TO  WK2.                         EL541
00994      MOVE  1808001800            TO  H-MASK.                      EL541
00995                                                                   EL541
00996      IF ME-041-RUN-DT GREATER ZERO AND                            EL541
00997         ME-522-RUN-DT GREATER ZERO                                EL541
00998          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
00999                                                                   EL541
01000      MOVE  ME-061-PY-ADJ         TO  WK1.                         EL541
01001      MOVE  ME-522-PY-ADJ         TO  WK2.                         EL541
01002      MOVE  1711001800            TO  H-MASK.                      EL541
01003                                                                   EL541
01004      IF ME-061-RUN-DT GREATER ZERO AND                            EL541
01005         ME-522-RUN-DT GREATER ZERO                                EL541
01006          PERFORM 1000-TEST  THRU  1099-TESTX.                     EL541
01007                                                                   EL541
01008      EJECT                                                        EL541
01009  0600-PAGE-2.                                                     EL541
01010      MOVE  HD-9                  TO   P-DATA.                     EL541
01011      MOVE  '1'                   TO   P-CTL.                         CL**2
01012      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
01013                                                                   EL541
01014      MOVE  HD-2                  TO   P-DATA.                     EL541
01015      MOVE  SPACE                 TO   P-CTL.                         CL**3
01016      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
01017                                                                   EL541
01018      ADD  1                      TO   PGCT.                       EL541
01019                                                                   EL541
01020      MOVE  PGCT                  TO   HD3-PG.                     EL541
01021      MOVE  HD-3                  TO   P-DATA.                     EL541
01022      MOVE  SPACE                 TO   P-CTL.                         CL**3
01023      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
01024                                                                   EL541
01025      IF ERRX = ZERO                                               EL541
01026          GO TO 0610-SPCL-MSG.                                     EL541
01027                                                                   EL541
01028      MOVE  ZERO                  TO  P-CTL.                       EL541
01029                                                                   EL541
01030      PERFORM 0620-FORM-LINE  THRU  0650-FL-EXIT                   EL541
01031          VARYING  CURRX  FROM  1  BY  1                           EL541
01032              UNTIL  CURRX GREATER ERRX.                           EL541
01033                                                                   EL541
01034      GO TO 0700-PAGE-3.                                           EL541
01035                                                                   EL541
01036  0610-SPCL-MSG.                                                   EL541
01037      MOVE  SPL-MSG-1             TO  P-DATA.                      EL541
01038      MOVE  ZERO                  TO  P-CTL.                       EL541
01039      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
01040                                                                   EL541
01041      MOVE  SPL-MSG-2             TO  P-DATA.                      EL541
01042      MOVE  SPACE                 TO  P-CTL.                          CL**4
01043      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
01044                                                                   EL541
01045      GO TO 0700-PAGE-3.                                           EL541
01046                                                                   EL541
01047  0620-FORM-LINE.                                                  EL541
01048      MOVE  ERR-TBL (CURRX)       TO  DECODE-P.                    EL541
01049      MOVE  ETBL-1-ITM (DP-1)     TO  EXL-1.                       EL541
01050      MOVE  ETBL-2-ITM (DP-2)     TO  EXL-2.                       EL541
01051      MOVE  ETBL-2-ITM (DP-4)     TO  EXL-4.                       EL541
01052                                                                   EL541
01053      IF DP-3  = ZERO                                              EL541
01054          MOVE  SPACE             TO  EXL-3                        EL541
01055      ELSE                                                         EL541
01056          MOVE  ETBL-3-ITM (DP-3) TO  EXL-3.                       EL541
01057                                                                   EL541
01058      IF DP-5  = ZERO                                              EL541
01059          MOVE  SPACE             TO  EXL-5                        EL541
01060      ELSE                                                         EL541
01061          MOVE  ETBL-3-ITM (DP-5) TO  EXL-5.                       EL541
01062                                                                   EL541
01063      MOVE  ERR-AMT (CURRX)       TO  EXL-AMT.                     EL541
01064      MOVE  EXP-LINE              TO  CMP-LINE.                    EL541
01065      MOVE  1                     TO  X1.                          EL541
01066      MOVE  102                   TO  SVTOP.                       EL541
01067                                                                   EL541
01068  0630-CMPRS-LOOP.                                                 EL541
01069      IF SVTOP LESS 1                                              EL541
01070          GO TO 0645-CMPRS-X.                                      EL541
01071                                                                   EL541
01072      IF CHAR (SVTOP) NOT = SPACE                                  EL541
01073          GO TO 0630-CL-BOT.                                       EL541
01074                                                                   EL541
01075      SUBTRACT  1  FROM  SVTOP.                                    EL541
01076                                                                   EL541
01077      GO TO 0630-CMPRS-LOOP.                                       EL541
01078                                                                   EL541
01079  0630-CL-BOT.                                                     EL541
01080      ADD  1  X1  GIVING  X2.                                      EL541
01081                                                                   EL541
01082      IF X2 GREATER SVTOP                                          EL541
01083          GO TO 0645-CMPRS-X.                                      EL541
01084                                                                   EL541
01085      IF SPACE  = CHAR (X1)  AND  CHAR (X2)                        EL541
01086          GO TO 0633-DOWNER.                                       EL541
01087                                                                   EL541
01088      ADD  1                      TO  X1.                          EL541
01089                                                                   EL541
01090      GO TO 0630-CL-BOT.                                           EL541
01091                                                                   EL541
01092  0633-DOWNER.                                                     EL541
01093      ADD  1  X2  GIVING  X3.                                      EL541
01094                                                                   EL541
01095      IF X3 GREATER SVTOP                                          EL541
01096            GO TO 0630-CL-BOT.                                     EL541
01097                                                                   EL541
01098      MOVE  CHAR (X3)             TO  CHAR (X2).                   EL541
01099                                                                   EL541
01100      IF X3  = SVTOP                                               EL541
01101          MOVE  SPACE             TO  CHAR (SVTOP)                 EL541
01102          SUBTRACT  1  FROM  SVTOP.                                EL541
01103                                                                   EL541
01104      ADD  1                      TO  X2.                          EL541
01105                                                                   EL541
01106      GO TO 0633-DOWNER.                                           EL541
01107                                                                   EL541
01108  0645-CMPRS-X.                                                    EL541
01109      MOVE  CMP-LINE              TO  P-DATA.                      EL541
01110      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
01111                                                                   EL541
01112      MOVE  ZERO                  TO  P-CTL.                       EL541
01113                                                                   EL541
01114  0650-FL-EXIT.                                                    EL541
01115      EXIT.                                                        EL541
01116                                                                   EL541
01117      EJECT                                                        EL541
01118  0700-PAGE-3.                                                     EL541
01119      IF ME-010-CERT-IN  NOT = PRIOR-CERTS                         EL541
01120          MOVE  'CERTIFICATE COUNT INPUT DOES NOT = PRIOR OUTPUT'  EL541
01121                                  TO  P-DATA                       EL541
01122          MOVE  SPACE             TO  P-CTL                           CL**4
01123          PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                EL541
01124                                                                   EL541
01125      IF ME-038-RECS-IN NOT = PRIOR-CLM-HIST-RECS                  EL541
01126          MOVE 'CLAIM HISTORY RECORDS INPUT DO NOT = PRIOR OUTPUT' EL541
01127                                  TO  P-DATA                       EL541
01128          MOVE  SPACE             TO  P-CTL                           CL**4
01129          PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                EL541
01130                                                                   EL541
01131      IF ME-048-RECS-IN NOT NUMERIC                                EL541
01132          MOVE ZERO               TO  ME-048-RECS-IN.              EL541
01133                                                                   EL541
01134      IF ME-048-RECS-IN NOT = PRIOR-RSV-HIST-RECS                  EL541
01135         MOVE 'RESERVE HISTORY RECORDS INPUT DO NOT = PRIOR OUTPUT'EL541
01136                                  TO  P-DATA                       EL541
01137          MOVE  SPACE             TO  P-CTL                           CL**4
01138          PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                EL541
01139                                                                   EL541
01140      IF ME-341-NOT-FOUND NOT = ZERO                               EL541
01141          MOVE  'EL341 HAS ERRORS'  TO  P-DATA                     EL541
01142          MOVE  SPACE               TO  P-CTL                         CL**4
01143          PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                EL541
01144                                                                   EL541
01145      IF ME-080-MORT-ERRS GREATER 25                               EL541
01146          MOVE  'ECS080 HAS OVER 25 ERRORS'  TO  P-DATA            EL541
01147          MOVE  SPACE                    TO  P-CTL                    CL**4
01148          PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                EL541
01149                                                                   EL541
01150      IF ME-331-FLAG NOT = 1                                       EL541
01151          MOVE  'EL331 NOT COMPLETED'  TO  P-DATA                  EL541
01152          MOVE  SPACE              TO  P-CTL                          CL**4
01153          PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                EL541
01154                                                                   EL541
01155      IF ME-501-FLAG NOT = 1                                       EL541
01156          MOVE  'EL501 NOT COMPLETED'  TO  P-DATA                  EL541
01157          MOVE  SPACE              TO  P-CTL                          CL**4
01158          PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                EL541
01159                                                                   EL541
01160      IF ME-509-FLAG NOT = 1                                       EL541
01161          MOVE  'EL509 NOT COMPLETED'  TO  P-DATA                  EL541
01162          MOVE  SPACE              TO  P-CTL                          CL**4
01163          PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                EL541
01164                                                                   EL541
01165      IF ME-525-FLAG NOT = 1                                       EL541
01166          MOVE  'EL525 NOT COMPLETED'  TO  P-DATA                  EL541
01167          MOVE  SPACE              TO  P-CTL                          CL**4
01168          PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                EL541
01169                                                                   EL541
01170      MOVE  HD-A                  TO  P-DATA.                      EL541
01171      MOVE  '1'                   TO  P-CTL.                       EL541
01172      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
01173                                                                   EL541
01174      MOVE  HD-2                  TO  P-DATA.                      EL541
01175      MOVE  SPACE                 TO  P-CTL.                          CL**3
01176      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
01177                                                                   EL541
01178      ADD  1                      TO  PGCT.                        EL541
01179                                                                   EL541
01180      MOVE  PGCT                  TO  HD3-PG.                      EL541
01181      MOVE  HD-3                  TO  P-DATA.                      EL541
01182      MOVE  SPACE                 TO  P-CTL.                          CL**3
01183      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
01184                                                                   EL541
01185      MOVE  HD-B                  TO  P-DATA.                      EL541
01186      MOVE  ZERO                  TO  P-CTL.                       EL541
01187      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
01188                                                                   EL541
01189      MOVE  HD-C                  TO  P-DATA.                      EL541
01190      MOVE  SPACE                 TO  P-CTL.                          CL**3
01191      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
01192                                                                   EL541
01193      MOVE  ZERO                  TO  P-CTL, CURRX.                EL541
01194      MOVE  ME-010-RUN-DT         TO  DH-RUN.                      EL541
070714*    MOVE  ME-010-START          TO  DH-START.                    EL541
070714*    MOVE  ME-010-END            TO  DH-END.                      EL541
01197      MOVE  ME-010-RUN-CT         TO  DH-COUNT.                    EL541
01198                                                                   EL541
01199      PERFORM 0720-SETLN-3  THRU  0729-EXIT.                       EL541
01200                                                                   EL541
01201      MOVE  ME-018-RUN-DT         TO  DH-RUN.                      EL541
070714*    MOVE  ME-018-START          TO  DH-START.                    EL541
070714*    MOVE  ME-018-END            TO  DH-END.                      EL541
01204      MOVE  ME-018-RUN-CT         TO  DH-COUNT.                    EL541
01205                                                                   EL541
01206      PERFORM 0720-SETLN-3  THRU  0729-EXIT.                       EL541
01207                                                                   EL541
01208      MOVE  ME-019-RUN-DT         TO  DH-RUN.                      EL541
070714*    MOVE  ME-019-START          TO  DH-START.                    EL541
070714*    MOVE  ME-019-END            TO  DH-END.                      EL541
01211      MOVE  ME-019-RUN-CT         TO  DH-COUNT.                    EL541
01212                                                                   EL541
01213      PERFORM 0720-SETLN-3  THRU  0729-EXIT.                       EL541
01214                                                                   EL541
01215      MOVE  ME-030-RUN-DT         TO  DH-RUN.                      EL541
070714*    MOVE  ME-030-START          TO  DH-START.                    EL541
070714*    MOVE  ME-030-END            TO  DH-END.                      EL541
01218      MOVE  ME-030-RUN-CT         TO  DH-COUNT.                    EL541
01219                                                                   EL541
01220      PERFORM 0720-SETLN-3  THRU  0729-EXIT.                       EL541
01221                                                                   EL541
01222      MOVE  ME-032-RUN-DT         TO  DH-RUN.                      EL541
070714*    MOVE  ME-032-START          TO  DH-START.                    EL541
070714*    MOVE  ME-032-END            TO  DH-END.                      EL541
01225      MOVE  ME-032-RUN-CT         TO  DH-COUNT.                    EL541
01226                                                                   EL541
01227      PERFORM 0720-SETLN-3  THRU  0729-EXIT.                       EL541
01228                                                                   EL541
01229      MOVE  ME-035-RUN-DT         TO  DH-RUN.                      EL541
070714*    MOVE  ME-035-START          TO  DH-START.                    EL541
070714*    MOVE  ME-035-END            TO  DH-END.                      EL541
01232      MOVE  ME-035-RUN-CT         TO  DH-COUNT.                    EL541
01233                                                                   EL541
01234      PERFORM 0720-SETLN-3  THRU  0729-EXIT.                       EL541
01235                                                                   EL541
01236      MOVE  ME-038-RUN-DT         TO  DH-RUN.                      EL541
070714*    MOVE  ME-038-START          TO  DH-START.                    EL541
070714*    MOVE  ME-038-END            TO  DH-END.                      EL541
01239      MOVE  ME-038-RUN-CT         TO  DH-COUNT.                    EL541
01240                                                                   EL541
01241      PERFORM 0720-SETLN-3  THRU  0729-EXIT.                       EL541
01242                                                                   EL541
01243      MOVE  ME-041-RUN-DT         TO  DH-RUN.                      EL541
070714*    MOVE  ME-041-START          TO  DH-START.                    EL541
070714*    MOVE  ME-041-END            TO  DH-END.                      EL541
01246      MOVE  ME-041-RUN-CT         TO  DH-COUNT.                    EL541
01247                                                                   EL541
01248      PERFORM 0720-SETLN-3  THRU  0729-EXIT.                       EL541
01249                                                                   EL541
01250      MOVE  ME-048-RUN-DT         TO  DH-RUN.                      EL541
01251      MOVE  ME-048-START          TO  DH-START.                    EL541
01252      MOVE  ME-048-END            TO  DH-END.                      EL541
01253      MOVE  ME-048-RUN-CT         TO  DH-COUNT.                    EL541
01254                                                                   EL541
01255      PERFORM 0720-SETLN-3  THRU  0729-EXIT.                       EL541
01256                                                                   EL541
01257      MOVE  ME-050-RUN-DT         TO  DH-RUN.                      EL541
070714*    MOVE  ME-050-START          TO  DH-START.                    EL541
070714*    MOVE  ME-050-END            TO  DH-END.                      EL541
01260      MOVE  ME-050-RUN-CT         TO  DH-COUNT.                    EL541
01261                                                                   EL541
01262      PERFORM 0720-SETLN-3  THRU  0729-EXIT.                       EL541
01263                                                                   EL541
01264      MOVE  ME-061-RUN-DT         TO  DH-RUN.                      EL541
070714*    MOVE  ME-061-START          TO  DH-START.                    EL541
070714*    MOVE  ME-061-END            TO  DH-END.                      EL541
01267      MOVE  ME-061-RUN-CT         TO  DH-COUNT.                    EL541
01268                                                                   EL541
01269      PERFORM 0720-SETLN-3  THRU  0729-EXIT.                       EL541
01270                                                                   EL541
01271      MOVE  ME-080-RUN-DT         TO  DH-RUN.                      EL541
070714*    MOVE  ME-080-START          TO  DH-START.                    EL541
070714*    MOVE  ME-080-END            TO  DH-END.                      EL541
01274      MOVE  ME-080-RUN-CT         TO  DH-COUNT.                    EL541
01275                                                                   EL541
01276      PERFORM 0720-SETLN-3  THRU  0729-EXIT.                       EL541
01277                                                                   EL541
01278      MOVE  ME-315-RUN-DT         TO  DH-RUN.                      EL541
070714*    MOVE  ME-315-START          TO  DH-START.                    EL541
070714*    MOVE  ME-315-END            TO  DH-END.                      EL541
01281      MOVE  ME-315-RUN-CT         TO  DH-COUNT.                    EL541
01282                                                                   EL541
01283      PERFORM 0720-SETLN-3  THRU  0729-EXIT.                       EL541
01284                                                                   EL541
01285      MOVE  ME-331-RUN-DT         TO  DH-RUN.                      EL541
070714*    MOVE  ME-331-START          TO  DH-START.                    EL541
070714*    MOVE  ME-331-END            TO  DH-END.                      EL541
01288      MOVE  ME-331-RUN-CT         TO  DH-COUNT.                    EL541
01289                                                                   EL541
01290      PERFORM 0720-SETLN-3  THRU  0729-EXIT.                       EL541
01291                                                                   EL541
01292      MOVE  ME-341-RUN-DT         TO  DH-RUN.                      EL541
070714*    MOVE  ME-341-START          TO  DH-START.                    EL541
070714*    MOVE  ME-341-END            TO  DH-END.                      EL541
01295      MOVE  ME-341-RUN-CT         TO  DH-COUNT.                    EL541
01296                                                                   EL541
01297      PERFORM 0720-SETLN-3  THRU  0729-EXIT.                       EL541
01298                                                                   EL541
01299      MOVE  ME-501-RUN-DT         TO  DH-RUN.                      EL541
01300      MOVE  ME-501-START          TO  DH-START.                    EL541
01301      MOVE  ME-501-END            TO  DH-END.                      EL541
01302      MOVE  ME-501-RUN-CT         TO  DH-COUNT.                    EL541
01303                                                                   EL541
01304      PERFORM 0720-SETLN-3  THRU  0729-EXIT.                       EL541
01305                                                                   EL541
01306      MOVE  ME-509-RUN-DT         TO  DH-RUN.                      EL541
01307      MOVE  ME-509-START          TO  DH-START.                    EL541
01308      MOVE  ME-509-END            TO  DH-END.                      EL541
01309      MOVE  ME-509-RUN-CT         TO  DH-COUNT.                    EL541
01310                                                                   EL541
01311      PERFORM 0720-SETLN-3  THRU  0729-EXIT.                       EL541
01312                                                                   EL541
01313      MOVE  ME-522-RUN-DT         TO  DH-RUN.                      EL541
01314      MOVE  ME-522-START          TO  DH-START.                    EL541
01315      MOVE  ME-522-END            TO  DH-END.                      EL541
01316      MOVE  ME-522-RUN-CT         TO  DH-COUNT.                    EL541
01317                                                                   EL541
01318      PERFORM 0720-SETLN-3  THRU  0729-EXIT.                       EL541
01319                                                                   EL541
01320      MOVE  ME-524-RUN-DT         TO  DH-RUN.                      EL541
01321      MOVE  ME-524-START          TO  DH-START.                    EL541
01322      MOVE  ME-524-END            TO  DH-END.                      EL541
01323      MOVE  ME-524-RUN-CT         TO  DH-COUNT.                    EL541
01324                                                                   EL541
01325      PERFORM 0720-SETLN-3  THRU  0729-EXIT.                       EL541
01326                                                                   EL541
01327      MOVE  ME-525-RUN-DT         TO  DH-RUN.                      EL541
01328      MOVE  ME-525-START          TO  DH-START.                    EL541
01329      MOVE  ME-525-END            TO  DH-END.                      EL541
01330      MOVE  ME-525-RUN-CT         TO  DH-COUNT.                    EL541
01331                                                                   EL541
01332      PERFORM 0720-SETLN-3  THRU  0729-EXIT.                       EL541
01333                                                                   EL541
01334      GO TO EOJ.                                                   EL541
01335                                                                   EL541
01336      EJECT                                                        EL541
01337  0720-SETLN-3.                                                    EL541
01338      ADD  1  TO  CURRX.                                           EL541
01339                                                                   EL541
01340      MOVE  ETBL-2-ITM (CURRX)    TO  DH-JOB.                      EL541
01341      MOVE  SPACE                 TO  DT-2.                        EL541
01342      MOVE  DH-JOB                TO  D2-JOBNO.                    EL541
01343      MOVE  DH-RUNMO              TO  D2-RUNMO.                    EL541
01344      MOVE  DH-RUNDA              TO  D2-RUNDA.                    EL541
01345      MOVE  DH-RUNYR              TO  D2-RUNYR.                    EL541
01346      MOVE  '/'                   TO  D2-SL1  D2-SL2.              EL541
01347      MOVE  DH-STHR               TO  D2-BGHR.                     EL541
01348      MOVE  DH-STMN               TO  D2-BGMN.                     EL541
01349      MOVE  DH-STSC               TO  D2-BGSC.                     EL541
01350      MOVE  DH-ENHR               TO  D2-FNHR.                     EL541
01351      MOVE  DH-ENMN               TO  D2-FNMN.                     EL541
01352      MOVE  DH-ENSC               TO  D2-FNSC.                     EL541
01353      MOVE  DH-COUNT              TO  D2-RRNO.                     EL541
01354      MOVE  DH-ENHR               TO  SIGN-HR.                     EL541
01355      MOVE  DH-ENMN               TO  SIGN-MN.                     EL541
01356      MOVE  DH-ENSC               TO  SIGN-SC.                     EL541
01357      MOVE  DH-STHR               TO  EXPND-HR.                    EL541
01358      MOVE  DH-STMN               TO  EXPND-MN.                    EL541
01359      MOVE  DH-STSC               TO  EXPND-SC.                    EL541
01360                                                                   EL541
01361      IF SIGN-SC LESS  EXPND-SC                                    EL541
01362          ADD  60  TO  SIGN-SC                                     EL541
01363          SUBTRACT  1  FROM  SIGN-MN.                              EL541
01364                                                                   EL541
01365      IF SIGN-MN  NEGATIVE                                         EL541
01366          ADD 60 TO SIGN-MN.                                       EL541
01367                                                                   EL541
01368      IF SIGN-MN LESS  EXPND-MN                                    EL541
01369          ADD  60  TO  SIGN-MN                                     EL541
01370          SUBTRACT  1  FROM  SIGN-HR.                              EL541
01371                                                                   EL541
01372      IF SIGN-HR  NEGATIVE                                         EL541
01373          ADD  24                 TO  SIGN-HR.                     EL541
01374                                                                   EL541
01375      SUBTRACT  EXPND-HR  FROM  SIGN-HR.                           EL541
01376      SUBTRACT  EXPND-MN  FROM  SIGN-MN.                           EL541
01377      SUBTRACT  EXPND-SC  FROM  SIGN-SC.                           EL541
01378                                                                   EL541
01379      MOVE  SIGN-HR               TO  D2-DUHR.                     EL541
01380      MOVE  SIGN-MN               TO  D2-DUMN.                     EL541
01381      MOVE  SIGN-SC               TO  D2-DUSC.                     EL541
01382      MOVE  '.'                   TO  D2-SC1  D2-SC2               EL541
01383                                      D2-SC3  D2-SC4               EL541
01384                                      D2-SC5  D2-SC6.              EL541
01385      MOVE  DT-2                  TO  P-DATA.                      EL541
01386                                                                   EL541
01387      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
01388                                                                   EL541
01389      MOVE  SPACE                 TO  P-CTL.                          CL**4
01390                                                                   EL541
01391  0729-EXIT.                                                       EL541
01392      EXIT.                                                        EL541
01393                                                                   EL541
01394      EJECT                                                        EL541
01395  0800-SET-LAH.                                                    EL541
01396      IF ETBL-1-SET-LAH (X4) = 'LIFE  '                            EL541
01397          MOVE LIFE-OVERRIDE-L6    TO ETBL-1-SET-LAH(X4)           EL541
01398      ELSE                                                         EL541
01399          IF ETBL-1-SET-LAH (X4) = 'A/H   '                        EL541
01400              MOVE AH-OVERRIDE-L6  TO ETBL-1-SET-LAH (X4).         EL541
01401                                                                   EL541
01402  0800-SET-LAH-EXIT.                                               EL541
01403      EXIT.                                                        EL541
01404                                                                   EL541
01405      EJECT                                                        EL541
01406  1000-TEST SECTION.                                               EL541
01407      SUBTRACT  WK2  FROM  WK1  GIVING  WK3.                       EL541
01408                                                                   EL541
01409      IF WK3  =  ZERO                                              EL541
01410          GO TO 1099-TESTX.                                        EL541
01411                                                                   EL541
01412      ADD  1                      TO  ERRX.                        EL541
01413                                                                   EL541
01414      IF ERRX GREATER 50                                           EL541
01415          MOVE  'ERROR TABLE SIZE EXCEEDED'  TO  WS-ABEND-MESSAGE  EL541
01416          PERFORM ABEND-PGM.                                       EL541
01417                                                                   EL541
01418      IF WK1 GREATER WK2                                           EL541
01419          MOVE  H-MASK            TO  P-MASK                       EL541
01420      ELSE                                                         EL541
01421          MOVE  HRM-1             TO  PRM-1                        EL541
01422          MOVE  HRM-2             TO  PRM-3                        EL541
01423          MOVE  HRM-3             TO  PRM-2.                       EL541
01424                                                                   EL541
01425      MOVE  P-MASK                TO  ERR-TBL (ERRX).              EL541
01426      MOVE  WK3                   TO  ERR-AMT (ERRX).              EL541
01427                                                                   EL541
01428  1099-TESTX.                                                      EL541
01429      EXIT.                                                        EL541
01430                                                                   EL541
01431  1300-PRINT-LINE SECTION.                                         EL541
01432      MOVE  P-CTL                 TO  X.                           EL541
01433                                                                   EL541
01434                              COPY ELCPRT2X.                       EL541
01435                                                                   EL541
01436  1399-EXIT.                                                       EL541
01437      EXIT.                                                        EL541
01438                                                                   EL541
01439  1400-PGCHG SECTION.                                              EL541
01440      ADD  1                      TO  PGCT.                        EL541
01441      MOVE  ZERO                  TO  LNCT.                        EL541
01442      MOVE  PGCT                  TO  HD3-PG.                      EL541
01443      MOVE  '1'                   TO  P-CTL.                          CL**2
01444      MOVE  HD-1                  TO  P-DATA.                      EL541
01445      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
01446                                                                   EL541
01447      MOVE  SPACE                 TO  P-CTL.                          CL**3
01448                                                                   EL541
01449      MOVE  HD-2                  TO  P-DATA.                      EL541
01450      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
01451                                                                   EL541
01452      MOVE  SPACE                 TO  P-CTL.                          CL**3
01453      MOVE  HD-3                  TO  P-DATA.                      EL541
01454      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
01455                                                                   EL541
01456      MOVE  ZERO                  TO  P-CTL.                       EL541
01457      MOVE  HD-4                  TO  P-DATA.                      EL541
01458      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
01459                                                                   EL541
01460      MOVE  HD-5                  TO  P-DATA.                      EL541
01461      MOVE  ZERO                  TO  P-CTL.                       EL541
01462      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
01463                                                                   EL541
01464      MOVE  HD-6                  TO  P-DATA.                      EL541
01465      MOVE  ' '                   TO  P-CTL.                          CL**2
01466      PERFORM 1300-PRINT-LINE  THRU  1399-EXIT.                    EL541
01467                                                                   EL541
01468  1490-PGCHG-EX.                                                   EL541
01469      EXIT.                                                        EL541
01470                                                                   EL541
01471  2000-MINORS SECTION.                                             EL541
01472                                                                   EL541
01473  ABEND-PGM SECTION.                                               EL541
01474                              COPY ELCABEND SUPPRESS.              EL541
01475                                                                   EL541
01476  EOJ SECTION.                                                     EL541
01477      CLOSE ERMEBL.                                                EL541
01478                                                                   EL541
01479      IF DTE-PRT-OPT  =  'P'  OR  'B'  OR  'T'                     EL541
01480          CLOSE PRINT-FILE.                                        EL541
01481                                                                   EL541
01482                              COPY ELCPRTCX SUPPRESS.              EL541
01483      GOBACK.                                                      EL541
