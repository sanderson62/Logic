00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   ECS034
00003  PROGRAM-ID.                ECS034.                                  LV004
00004 *              PROGRAM CONVERTED BY                               ECS034
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS034
00006 *              CONVERSION DATE 11/28/95 11:09:27.                 ECS034
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS034
00008 *                            VMOD=2.008.                          ECS034
00009                                                                   ECS034
00010 *AUTHOR.        LOGIC,INC.                                        ECS034
00011 *               DALLAS, TEXAS.                                    ECS034
00012                                                                   ECS034
00013 *DATE-COMPILED.                                                   ECS034
00014                                                                   ECS034
00015 *SECURITY.   *****************************************************ECS034
00016 *            *                                                    ECS034
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS034
00018 *            *                                                   *ECS034
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS034
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS034
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS034
00022 *            *                                                   *ECS034
00023 *            *****************************************************ECS034
00024                                                                   ECS034
00025 *REMARKS.                                                         ECS034
00026 *        PRINT CLAIM SPREADS.                                     ECS034
031102******************************************************************
031102*                   C H A N G E   L O G
031102*
031102* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031102*-----------------------------------------------------------------
031102*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031102* EFFECTIVE    NUMBER
031102*-----------------------------------------------------------------
062002* 062002    2002061700006  PEMA  INCREASE INCURRED MONTHS TO 120
092702* 092702                   SMVA  INCREASE D1-AMT TO 7 DIGITS & 
092702*                               REMOVE COMMA FOR SPACE PURPOSES 
031102******************************************************************
00027                                                                   ECS034
00028  ENVIRONMENT DIVISION.                                            ECS034
00029  CONFIGURATION SECTION.                                           ECS034
00030                                                                      CL**3
00031  INPUT-OUTPUT SECTION.                                            ECS034
00032  FILE-CONTROL.                                                    ECS034
00033                                                                   ECS034
00034      SELECT PRT-RPT      ASSIGN TO SYS008-UR-1403-S-SYS008.       ECS034
00035      SELECT CLM-XTR      ASSIGN TO SYS010-UT-2400-S-SYS010.       ECS034
00036      SELECT DISK-DATE    ASSIGN TO SYS019-UT-3380-S-SYS019.       ECS034
00037      SELECT FICH         ASSIGN TO SYS020-UT-2400-S-SYS020.       ECS034
00038  EJECT                                                            ECS034
00039  DATA DIVISION.                                                   ECS034
00040  FILE SECTION.                                                    ECS034
00041                                                                   ECS034
00042  FD  PRT-RPT                                                      ECS034
00043                              COPY ELCPRTFD.                       ECS034
00044  EJECT                                                            ECS034
00045  FD  CLM-XTR                                                      ECS034
00046      BLOCK CONTAINS 0 RECORDS
00047      RECORDING MODE F.                                            ECS034
00048  01  CLMS-XTRCT.                                                  ECS034
00049      12  X-CNTRL.                                                 ECS034
00050          16  X-TYPE              PIC  X.                          ECS034
00051          16  X-ID                PIC  X.                          ECS034
00052          16  X-COMP.                                              ECS034
00053              20  X-CARR          PIC  X.                          ECS034
00054              20  X-GROUP         PIC  X(6).                       ECS034
00055          16  X-STATE             PIC  XX.                         ECS034
00056          16  X-ACC.                                               ECS034
00057              20  X-ACCT-PREFIX   PIC  X(4).                       ECS034
00058              20  X-GA            PIC  XX.                         ECS034
00059              20  FILLER          PIC  X(4).                       ECS034
00060          16 X-CLASS              PIC  XX.                         ECS034
00061      12  X-PD                    PIC S99.                         ECS034
00062      12  X-INC                   PIC S999    COMP-3.              ECS034
00063      12  X-AMT                   PIC S9(9)V99    COMP-3.          ECS034
00064      12  X-ECS033-PGM-OPT        PIC X.                           ECS034
00065      12  FILLER                  PIC X(14).                          CL**3
00066                                                                   ECS034
00067  FD  DISK-DATE                                                    ECS034
00068                              COPY ELCDTEFD.                       ECS034
00069  EJECT                                                            ECS034
00070  FD  FICH                                                         ECS034
00071                              COPY ELCFCHFD.                       ECS034
00072  EJECT                                                            ECS034
00073  WORKING-STORAGE SECTION.                                         ECS034
00074  77  FILLER  PIC  X(32) VALUE '********************************'. ECS034
00075  77  FILLER  PIC  X(32) VALUE '     ECS034 WORKING STORAGE     '. ECS034
00076  77  FILLER  PIC  X(32) VALUE '********** VMOD=2.008 **********'.    CL**3
00077                                                                   ECS034
00078  77  WS-ECS033-PGM-OPT       PIC X       VALUE ' '.               ECS034
00079 *77  CA                      PIC S99     COMP.                    ECS034
062002 77  CA                      PIC S999    COMP-3.                  ECS034
00080  77  CB                      PIC S99     COMP.                    ECS034
00081  77  PGM-SUB                 PIC S9(3)   COMP    VALUE +34.       ECS034
00082  77  WK-YR                   PIC S9(5)   COMP-3  VALUE +0.        ECS034
00083  77  PAGER                   PIC S9(5)   COMP-3  VALUE +1.        ECS034
00084  77  WORK-ACC-12A            PIC S9(9)V99 COMP-3   VALUE ZERO.    ECS034
00085  77  WORK-ACC-12B            PIC S9(9)V99 COMP-3   VALUE ZERO.    ECS034
00086  77  WORK-ACC-12C            PIC S9(9)V99 COMP-3   VALUE ZERO.    ECS034
00087  77  WORK-ACCUM              PIC S9(9)V99 COMP-3   VALUE ZERO.    ECS034
00088  77  X                       PIC  X.                              ECS034
00089  EJECT                                                            ECS034
00090  01  HEAD-A.                                                      ECS034
00091      12  FILLER              PIC  X(32)          VALUE SPACES.    ECS034
00092      12  HB-DESC.                                                 ECS034
00093          16  HB-DESC-1       PIC  X(5)           VALUE SPACES.    ECS034
00094          16  HB-DESC-2       PIC  XX             VALUE SPACES.    ECS034
00095          16  FILLER          PIC  X              VALUE SPACES.    ECS034
00096      12  FILLER              PIC  X(34)          VALUE            ECS034
00097              'CLAIMS PAID DURING LAST 12 MONTHS'.                 ECS034
00098      12  FILLER              PIC  X(17)          VALUE            ECS034
00099              'BY MONTH INCURRED'.                                 ECS034
00100      12  FILLER              PIC  X(29)          VALUE SPACES.    ECS034
00101      12  FILLER              PIC  X(8)           VALUE 'ECS034 '. ECS034
00102                                                                   ECS034
00103  01  HEAD-B.                                                      ECS034
00104      12  FILLER              PIC  X(48)          VALUE SPACE.     ECS034
00105      12  HD-COMP             PIC  X(30)          VALUE SPACES.    ECS034
00106      12  FILLER              PIC  X(42)          VALUE SPACES.    ECS034
00107      12  HA-IPL              PIC  X(8).                           ECS034
00108                                                                   ECS034
00109  01  HEAD-B2.                                                     ECS034
00110      12  FILLER              PIC  X(54)          VALUE SPACES.    ECS034
00111      12  HA-ASOF-DT          PIC  X(18)          VALUE SPACES.    ECS034
00112      12  FILLER              PIC  X(48)          VALUE SPACES.    ECS034
00113      12  FILLER              PIC  X(5)           VALUE 'PAGE'.    ECS034
00114      12  HB-PAGE             PIC ZZ,ZZZ.                          ECS034
00115                                                                   ECS034
00116  01  HEAD-C.                                                      ECS034
00117      12  FILLER              PIC  X(4)           VALUE SPACES.    ECS034
00118      12  HC-GADEC            PIC  X(22)          VALUE            ECS034
00119              'CARRIER/GROUP TOTAL -'.                             ECS034
00120      12  HC-GA               PIC  X(10).                          ECS034
00121      12  HC-DES              PIC  X(15)          VALUE SPACES.    ECS034
00122      12  HC-CO               PIC  X(7)           VALUE SPACES.    ECS034
00123                                                                   ECS034
00124  01  HEAD-R.                                                      ECS034
00125      12  FILLER              PIC  X(4)           VALUE SPACES.    ECS034
00126      12  FILLER              PIC  X(22)          VALUE            ECS034
00127              'REINSURANCE COMPANY -'.                             ECS034
00128      12  HR-GA               PIC  X(10).                          ECS034
00129      12  FILLER              PIC  X(22)          VALUE SPACES.    ECS034
00130                                                                   ECS034
00131  01  HEAD-RA.                                                     ECS034
00132      12  FILLER              PIC  X(4)           VALUE SPACES.    ECS034
00133      12  HRA-TOT             PIC  X(22)          VALUE SPACES.    ECS034
00134      12  HRA-ACCT            PIC  X(10).                          ECS034
00135      12  FILLER              PIC  X(22)          VALUE SPACES.    ECS034
00136                                                                   ECS034
00137  01  HEAD-D.                                                      ECS034
00138      12  FILLER              PIC  X(4)           VALUE SPACES.    ECS034
00139      12  FILLER-D            PIC  X(22)          VALUE            ECS034
00140              'BUSINESS CLASS/DESC -'.                             ECS034
00141      12  HD-CLSCD            PIC  XX             VALUE SPACES.    ECS034
00142      12  FILLER              PIC  X              VALUE SPACES.    ECS034
00143      12  HD-CLASS            PIC  X(24)          VALUE SPACES.    ECS034
00144      12  HD-DES              PIC  X(15)          VALUE SPACES.    ECS034
00145      12  HD-CO               PIC  X(7)           VALUE SPACES.    ECS034
00146                                                                   ECS034
00147  01  HEAD-D1.                                                     ECS034
00148      12  FILLER              PIC  X(4)           VALUE SPACES.    ECS034
00149      12  FILLER              PIC  X(8)           VALUE 'STATE -'. ECS034
00150      12  HD1-STATE           PIC  XX.                             ECS034
00151      12  FILLER              PIC  X(8)           VALUE SPACE.     ECS034
00152      12  HD1-ST-NAME         PIC  X(20)          VALUE SPACE.     ECS034
00153                                                                   ECS034
00154  01  HEAD-D2.                                                     ECS034
00155      12  FILLER              PIC  X(04)          VALUE SPACES.    ECS034
00156      12  FILLER              PIC  X(15)                           ECS034
00157                                  VALUE 'ISSUE PERIOD - '.         ECS034
00158      12  HD2-BEGIN           PIC X(12).                           ECS034
00159      12  FILLER              PIC X(06)           VALUE ' THRU '.  ECS034
00160      12  HD2-END             PIC X(12).                           ECS034
00161                                                                   ECS034
00162  01  HEAD-E.                                                      ECS034
00163      12  FILLER              PIC  X(13)          VALUE SPACES.    ECS034
00164      12  HD-E-R              PIC  X(96).                          ECS034
00165      12  HD-E  REDEFINES                                          ECS034
00166          HD-E-R              PIC  X(8)       OCCURS  12  TIMES.   ECS034
00167      12  FILLER              PIC  X(24)          VALUE            ECS034
00168              ' LAST 12-MO  INC-TO-DATE'.                          ECS034
00169                                                                   ECS034
00170  01  DATA-1.                                                      ECS034
00171      12  FILLER              PIC  XX             VALUE SPACES.    ECS034
00172      12  D1-COM              PIC  X.                              ECS034
00173      12  FILLER              PIC  X(3)           VALUE SPACES.    ECS034
00174      12  D1-DTE              PIC  X(5).                           ECS034
00175      12  FILLER              PIC  X              VALUE SPACES.    ECS034
092702     12  D1-AMT              PIC ZZZZZZ9-      OCCURS  12  TIMES. ECS034
00177      12  D1-ACCUM            PIC ZZZ,ZZZ,ZZ9-.                    ECS034
00178      12  FILLER              PIC  X              VALUE SPACES.    ECS034
00179      12  D1-TOT              PIC ZZZ,ZZZ,ZZ9-.                    ECS034
00180                                                                   ECS034
00181  01  DATA-2.                                                      ECS034
00182      12  FILLER              PIC  X(30)          VALUE SPACES.    ECS034
00183      12  FILLER              PIC  X(34)          VALUE            ECS034
00184              'TOTAL CLAIMS PAID DURING THE LAST '.                ECS034
00185      12  D2-MOS              PIC Z9.                              ECS034
00186      12  FILLER              PIC  X(8)           VALUE ' MONTHS'. ECS034
00187      12  D2-AMT              PIC ZZZ,ZZZ,ZZ9-.                    ECS034
00188                                                                   ECS034
00189  01  LINE-COMMENTS.                                               ECS034
00190      12  FILLER              PIC  X(25)          VALUE            ECS034
00191              'PRIOR 2-73 3-73 4-73 5-73'.                         ECS034
00192      12  FILLER              PIC  X(25)          VALUE            ECS034
00193              ' 6-73 7-73 8-73 9-7310-73'.                         ECS034
00194      12  FILLER              PIC  X(25)          VALUE            ECS034
00195              '11-7312-73 1-74 2-74 3-74'.                         ECS034
00196      12  FILLER              PIC  X(25)          VALUE            ECS034
00197              ' 4-74 5-74 6-74 7-74 8-74'.                         ECS034
00198      12  FILLER              PIC  X(25)          VALUE            ECS034
00199              ' 9-7410-7411-7412-74 1-75'.                         ECS034
00200      12  FILLER              PIC  X(25)          VALUE            ECS034
00201              ' 2-75 3-75 4-75 5-75 6-75'.                         ECS034
00202      12  FILLER              PIC  X(25)          VALUE            ECS034
00203              ' 7-75 8-75 9-7510-7511-75'.                         ECS034
00204      12  FILLER              PIC  X(25)          VALUE            ECS034
00205              '12-75 1-76 2-76 3-76 4-76'.                         ECS034
00206      12  FILLER              PIC  X(25)          VALUE            ECS034
00207              ' 5-76 6-76 7-76 8-76 9-76'.                         ECS034
00208      12  FILLER              PIC  X(25)          VALUE            ECS034
00209              '10-7611-7612-76 1-77 2-77'.                         ECS034
00210      12  FILLER              PIC  X(25)          VALUE            ECS034
00211              ' 3-77 4-77 5-77 6-77 7-77'.                         ECS034
00212      12  FILLER              PIC  X(25)          VALUE            ECS034
00213              ' 8-77 9-7710-7711-7712-77'.                         ECS034
00214      12  FILLER              PIC  X(25)          VALUE            ECS034
00215              ' 1-78 2-78 3-78 4-78 5-78'.                         ECS034
00216      12  FILLER              PIC  X(25)          VALUE            ECS034
00217              ' 6-78 7-78 8-78 9-7810-78'.                         ECS034
00218      12  FILLER              PIC  X(25)          VALUE            ECS034
00219              '11-7812-78 1-79 2-79 3-79'.                         ECS034
00220      12  FILLER              PIC  X(25)          VALUE            ECS034
00221              ' 4-79 5-79 6-79 7-79 8-79'.                         ECS034
00222      12  FILLER              PIC  X(20)          VALUE            ECS034
00223              ' 9-7910-7911-7912-79'.                              ECS034
00224                                                                   ECS034
00225  01  L-COM  REDEFINES  LINE-COMMENTS.                             ECS034
00226 *    12  LN-CMT          OCCURS  84  TIMES.                       ECS034
062002     12  LN-CMT          OCCURS 120  TIMES.                       ECS034
00227          16  LN-MO           PIC Z9.                              ECS034
00228          16  LN-D            PIC  X.                              ECS034
00229          16  LN-YR           PIC  99.                             ECS034
00230  EJECT                                                            ECS034
00231  01  ACCUM-1         COMP-3.                                      ECS034
00232 *    12  INC-SETS-1      OCCURS  85  TIMES.                       ECS034
062002     12  INC-SETS-1      OCCURS 121  TIMES.                       ECS034
00233          16  PD-SETS-1       PIC S9(9)V99    OCCURS  13  TIMES.   ECS034
00234                                                                   ECS034
00235  01  ACCUM-2         COMP-3.                                      ECS034
00236 *    12  INC-SETS-2      OCCURS  85  TIMES.                       ECS034
062002     12  INC-SETS-2      OCCURS 121  TIMES.                       ECS034
00237          16  PD-SETS-2       PIC S9(9)V99    OCCURS  13  TIMES.   ECS034
00238                                                                   ECS034
00239  01  ACCUM-3         COMP-3    SYNC.                              ECS034
00240 *    12  INC-SETS-3      OCCURS  85  TIMES.                       ECS034
062002     12  INC-SETS-3      OCCURS 121  TIMES.                       ECS034
00241          16  PD-SETS-3       PIC S9(9)V99    OCCURS  13  TIMES.   ECS034
00242                                                                   ECS034
00243 *01  SV-ZERO-ACCUM           PIC  X(6630).                        ECS034
062002 01  SV-ZERO-ACCUM           PIC  X(9438).                        ECS034
00244                                                                   ECS034
00245  01  OTHER-STUFF.                                                 ECS034
00246      12  SV-CNTRL.                                                ECS034
00247          16  SV-TYPE         PIC  X.                              ECS034
00248          16  SV-ID           PIC  X.                              ECS034
00249          16  SV-COMP.                                             ECS034
00250              20  SV-CARR     PIC  X.                              ECS034
00251              20  SV-GROUP    PIC  X(6).                           ECS034
00252          16  SV-STATE        PIC  XX.                             ECS034
00253          16  SV-GA           PIC  X(10).                          ECS034
00254          16  SV-CLASS        PIC  XX.                             ECS034
00255      12  WORK-ACC-6B         PIC S9(9)V99    COMP-3  VALUE ZERO.  ECS034
00256      12  WORK-ACC-6A         PIC S9(9)V99    COMP-3  VALUE ZERO.  ECS034
00257      12  WORK-ACC-6C         PIC S9(9)V99    COMP-3  VALUE ZERO.  ECS034
00258                                                                      CL**3
00259  01  MONTH-TAB.                                                   ECS034
00260      12  MONTH-TB1.                                               ECS034
00261          16  FILLER          PIC  X(9)       VALUE '   JAN'.      ECS034
00262          16  FILLER          PIC  X(9)       VALUE '   FEB'.      ECS034
00263          16  FILLER          PIC  X(9)       VALUE '  MARCH'.     ECS034
00264          16  FILLER          PIC  X(9)       VALUE '  APRIL'.     ECS034
00265          16  FILLER          PIC  X(9)       VALUE '   MAY'.      ECS034
00266          16  FILLER          PIC  X(9)       VALUE '  JUNE'.      ECS034
00267          16  FILLER          PIC  X(9)       VALUE '  JULY'.      ECS034
00268          16  FILLER          PIC  X(9)       VALUE '   AUG'.      ECS034
00269          16  FILLER          PIC  X(9)       VALUE '   SEP'.      ECS034
00270          16  FILLER          PIC  X(9)       VALUE '   OCT'.      ECS034
00271          16  FILLER          PIC  X(9)       VALUE '   NOV'.      ECS034
00272          16  FILLER          PIC  X(9)       VALUE '   DEC'.      ECS034
00273      12  MONTH-TB2  REDEFINES  MONTH-TB1.                         ECS034
00274          16  MNTH            PIC  X(9)   OCCURS  12  TIMES.       ECS034
00275                                                                   ECS034
00276  01  WS-WK.                                                       ECS034
00277      12  WS-RETURN-CODE          PIC  X(4)       VALUE SPACES.    ECS034
00278      12  WS-ABEND-MESSAGE        PIC  X(80)      VALUE SPACES.    ECS034
00279      12  WS-ZERO                 PIC S9          VALUE ZERO.      ECS034
00280      12  WS-ABEND-FILE-STATUS    PIC  XX         VALUE SPACES.    ECS034
00281                                                                   ECS034
00282      COPY ELCDTECX.                                               ECS034
00283                                                                   ECS034
00284      COPY ELCDTEVR.                                               ECS034
00285                                                                   ECS034
00286      COPY ELCDATE.                                                   CL**4
00287  EJECT                                                            ECS034
00288  PROCEDURE DIVISION.                                              ECS034
00289                                                                   ECS034
00290  0000-START-IT.                                                   ECS034
00291                              COPY ELCDTERX.                       ECS034
00292                                                                   ECS034
00293      MOVE SPACES                 TO  HD-E-R.                      ECS034
00294      MOVE COMPANY-NAME           TO  HD-COMP.                     ECS034
00295                                                                      CL**3
00296      COMPUTE CA = RUN-MO + +1.                                    ECS034
00297      IF CA GREATER +12                                            ECS034
00298          MOVE +1                 TO  CA.                          ECS034
00299      MOVE +1                     TO  CB.                          ECS034
00300                                                                   ECS034
00301  0100-MNTH-LOOP.                                                  ECS034
00302      MOVE MNTH (CA)              TO  HD-E (CB).                   ECS034
00303                                                                   ECS034
00304      ADD +1                      TO  CA.                          ECS034
00305                                                                   ECS034
00306      IF CA GREATER +12                                            ECS034
00307          MOVE +1                 TO  CA.                          ECS034
00308                                                                   ECS034
00309      ADD  +1  TO  CB.                                             ECS034
00310                                                                   ECS034
00311      IF CB NOT GREATER +12                                        ECS034
00312          GO TO  0100-MNTH-LOOP.                                   ECS034
00313                                                                   ECS034
00314      IF RUN-YR LESS THAN 70                                          CL**3
00315 *        COMPUTE WK-YR = (((100 + RUN-YR) * 12) + RUN-MO) - +84      CL**3
062002         COMPUTE WK-YR = (((100 + RUN-YR) * 12) + RUN-MO) - +120     CL**3
00316      ELSE                                                            CL**3
00317 *        COMPUTE WK-YR = ((RUN-YR * 12) + RUN-MO) - +84.             CL**3
062002         COMPUTE WK-YR = ((RUN-YR * 12) + RUN-MO) - +120.            CL**3
00318                                                                   ECS034
00319      COMPUTE WK-YR = (WK-YR / +12).                               ECS034
00320                                                                   ECS034
00321      MOVE +1                     TO  CA.                             CL**3
00322                                                                      CL**3
00323      COMPUTE CB = RUN-MO + +1.                                       CL**3
00324      IF CB GREATER +12                                            ECS034
00325          SUBTRACT +12            FROM  CB.                        ECS034
00326                                                                   ECS034
00327  0110-PLUG-YR.                                                    ECS034
00328      ADD +1                      TO  CA.                          ECS034
00329                                                                   ECS034
00330 *    IF CA GREATER +84                                            ECS034
062002     IF CA GREATER +120                                           ECS034
00331          GO TO  0120-OPEN-EM.                                     ECS034
00332                                                                   ECS034
00333      ADD +1                      TO  CB.                          ECS034
00334                                                                   ECS034
00335      IF CB GREATER +12                                            ECS034
00336          MOVE +1                 TO  CB                           ECS034
00337          ADD  +1                 TO  WK-YR.                       ECS034
00338                                                                   ECS034
00339      MOVE CB                     TO  LN-MO (CA).                  ECS034
00340      MOVE WK-YR                  TO  LN-YR (CA).                  ECS034
00341                                                                   ECS034
00342      GO TO  0110-PLUG-YR.                                         ECS034
00343                                                                   ECS034
00344  0120-OPEN-EM.                                                    ECS034
00345                                                                   ECS034
00346      OPEN INPUT   CLM-XTR                                         ECS034
00347           OUTPUT  PRT-RPT.                                        ECS034
00348                                                                   ECS034
00349  0130-SET-INITIAL.                                                ECS034
00350                                                                   ECS034
00351      PERFORM 0400-Z-1                                             ECS034
00352          VARYING  CA  FROM  +1  BY  +1                            ECS034
00353              UNTIL CA GREATER +13.                                ECS034
00354                                                                   ECS034
00355      PERFORM 0500-Z-2                                             ECS034
00356          VARYING  CA  FROM  +2  BY  +1                            ECS034
00357 *            UNTIL CA GREATER +85.                                ECS034
062002             UNTIL CA GREATER +121.                               ECS034
00358                                                                   ECS034
00359  0135-ZERO-OTHER-TABLES.                                          ECS034
00360                                                                   ECS034
00361      MOVE ACCUM-1                TO  ACCUM-2                      ECS034
00362                                      ACCUM-3                      ECS034
00363                                      SV-ZERO-ACCUM.               ECS034
00364                                                                   ECS034
00365  0140-SET-END.                                                    ECS034
00366      MOVE WS-CURRENT-DATE        TO  HA-IPL.                      ECS034
00367      MOVE ALPH-DATE              TO  HA-ASOF-DT.                  ECS034
00368                                                                   ECS034
00369      PERFORM 0600-R-LOOP THRU 0699-EXIT.                          ECS034
00370                                                                   ECS034
00371      IF X-ECS033-PGM-OPT EQUAL '2'                                ECS034
00372         MOVE '2'                 TO WS-ECS033-PGM-OPT.            ECS034
00373                                                                   ECS034
00374  0150-SET-HEADS.                                                  ECS034
00375                                                                   ECS034
00376      IF  X-STATE = HIGH-VALUES                                    ECS034
00377          MOVE 'ALL'              TO  HD1-STATE                    ECS034
00378      ELSE                                                         ECS034
00379          PERFORM 0200-LOCATE-STATE THRU 0299-EXIT                 ECS034
00380          MOVE X-STATE            TO  HD1-STATE.                   ECS034
00381                                                                   ECS034
00382      IF WS-ECS033-PGM-OPT EQUAL '2'                               ECS034
00383         MOVE X-GROUP             TO  HR-GA                        ECS034
00384         IF DTE-TOT-OPT EQUAL '2'                                  ECS034
00385            MOVE 'ACCOUNT NUMBER      -'                           ECS034
00386                                  TO  HRA-TOT                      ECS034
00387            MOVE X-ACC            TO  HRA-ACCT.                    ECS034
00388                                                                   ECS034
00389      IF X-COMP = HIGH-VALUES                                      ECS034
00390          MOVE 'ALL'              TO  HC-GA                        ECS034
00391          MOVE X-COMP             TO  HD-CO                        ECS034
00392      ELSE                                                         ECS034
00393          MOVE X-COMP             TO  HC-GA  HD-CO.                ECS034
00394                                                                   ECS034
00395      IF X-CLASS = HIGH-VALUES                                     ECS034
00396          MOVE 'ALL'              TO  HD-CLASS                     ECS034
00397          GO TO 0310-CLASS-OKAY                                    ECS034
00398      ELSE                                                         ECS034
00399          MOVE 'OTHER'            TO  HD-CLASS.                    ECS034
00400                                                                   ECS034
00401      MOVE CLAS-STARTB            TO  CLAS-INDEXB.                 ECS034
00402                                                                   ECS034
00403      GO TO 0300-FIND-CLASS-BUS.                                   ECS034
00404                                                                   ECS034
00405  0200-LOCATE-STATE.                                               ECS034
00406      MOVE ZEROS                  TO  CLAS-INDEXS.                 ECS034
00407                                                                   ECS034
00408  0210-LOCATE-LOOP.                                                ECS034
00409      ADD +1                      TO  CLAS-INDEXS.                 ECS034
00410                                                                   ECS034
00411      IF  CLAS-INDEXS GREATER THAN CLAS-MAXS                       ECS034
00412          MOVE 'UNKNOWN'          TO  HD1-ST-NAME                  ECS034
00413          GO TO 0299-EXIT.                                         ECS034
00414                                                                   ECS034
00415      IF X-STATE NOT = STATE-SUB (CLAS-INDEXS)                     ECS034
00416          GO TO 0210-LOCATE-LOOP.                                  ECS034
00417                                                                   ECS034
00418      MOVE STATE-PIC (CLAS-INDEXS)                                 ECS034
00419                                  TO  HD1-ST-NAME.                 ECS034
00420                                                                   ECS034
00421  0299-EXIT.                                                       ECS034
00422      EXIT.                                                        ECS034
00423                                                                   ECS034
00424  0300-FIND-CLASS-BUS.                                             ECS034
00425      IF CLAS-INDEXB GREATER CLAS-MAXB                             ECS034
00426          MOVE 'UNKNOWN ' TO HD-CLASS                              ECS034
00427          MOVE X-CLASS TO HD-CLSCD                                 ECS034
00428          GO TO 0310-CLASS-OKAY.                                   ECS034
00429                                                                   ECS034
00430      IF X-CLASS = CLAS-BUSC-CODE (CLAS-INDEXB)                    ECS034
00431          MOVE CLAS-BUSC-DESC (CLAS-INDEXB)                        ECS034
00432                                  TO  HD-CLASS                     ECS034
00433          MOVE X-CLASS            TO  HD-CLSCD                     ECS034
00434          GO TO 0310-CLASS-OKAY.                                   ECS034
00435                                                                   ECS034
00436      ADD +1                      TO  CLAS-INDEXB.                 ECS034
00437                                                                   ECS034
00438      GO TO 0300-FIND-CLASS-BUS.                                   ECS034
00439                                                                   ECS034
00440  0310-CLASS-OKAY.                                                 ECS034
00441      MOVE X-CNTRL                TO  SV-CNTRL.                    ECS034
00442                                                                   ECS034
00443      GO TO 0710-ADD-IN.                                           ECS034
00444                                                                   ECS034
00445  0400-Z-1.                                                        ECS034
00446      MOVE ZERO                   TO  PD-SETS-1 (1  CA).           ECS034
00447                                                                   ECS034
00448  0500-Z-2.                                                        ECS034
00449      MOVE INC-SETS-1 (1)         TO  INC-SETS-1 (CA).             ECS034
00450                                                                   ECS034
00451  0600-R-LOOP.                                                     ECS034
00452      READ CLM-XTR  AT END                                         ECS034
00453          GO TO 1300-FINAL-TOTS.                                   ECS034
00454                                                                   ECS034
00455  0699-EXIT.                                                       ECS034
00456      EXIT.                                                        ECS034
00457                                                                   ECS034
00458  0700-CHECK-EM.                                                   ECS034
00459                                                                   ECS034
00460      IF WS-ECS033-PGM-OPT EQUAL '2'                               ECS034
00461         IF DTE-TOT-OPT EQUAL '2'                                  ECS034
00462            IF X-ACC NOT EQUAL SV-GA                               ECS034
00463               GO TO 0720-SOME-KINDA-TOTALS                        ECS034
00464            ELSE                                                   ECS034
00465               GO TO 0710-ADD-IN                                   ECS034
00466         ELSE                                                      ECS034
00467            IF X-GROUP NOT EQUAL SV-GROUP                          ECS034
00468               GO TO 0720-SOME-KINDA-TOTALS                        ECS034
00469            ELSE                                                   ECS034
00470               GO TO 0710-ADD-IN.                                  ECS034
00471                                                                   ECS034
00472      IF X-CNTRL NOT = SV-CNTRL                                    ECS034
00473          GO TO 0720-SOME-KINDA-TOTALS.                            ECS034
00474                                                                   ECS034
00475  0710-ADD-IN.                                                     ECS034
00476      MOVE X-PD                   TO  CB.                          ECS034
00477      MOVE X-INC                  TO  CA.                          ECS034
00478                                                                   ECS034
00479      ADD X-AMT                   TO  PD-SETS-1 (CA  CB)           ECS034
00480                                      PD-SETS-2 (CA  CB)           ECS034
00481                                      PD-SETS-3 (CA  CB)           ECS034
00482 *                                    PD-SETS-1 (85  CB)           ECS034
00483 *                                    PD-SETS-2 (85  CB)           ECS034
00484 *                                    PD-SETS-3 (85  CB).          ECS034
062002                                     PD-SETS-1 (121 CB)           ECS034
062002                                     PD-SETS-2 (121 CB)           ECS034
062002                                     PD-SETS-3 (121 CB).          ECS034
00485                                                                   ECS034
00486      IF CB NOT = +13                                              ECS034
00487          ADD X-AMT               TO  PD-SETS-1 (CA  13)           ECS034
00488                                      PD-SETS-2 (CA  13)           ECS034
00489                                      PD-SETS-3 (CA  13)           ECS034
00490 *                                    PD-SETS-1 (85  13)           ECS034
00491 *                                    PD-SETS-2 (85  13)           ECS034
00492 *                                    PD-SETS-3 (85  13).          ECS034
062002                                     PD-SETS-1 (121 13)           ECS034
062002                                     PD-SETS-2 (121 13)           ECS034
062002                                     PD-SETS-3 (121 13).          ECS034
00493                                                                   ECS034
00494      IF X-PD = +13                                                ECS034
00495          GO TO 0600-R-LOOP.                                       ECS034
00496                                                                   ECS034
00497      IF CB GREATER +6                                             ECS034
00498          ADD X-AMT               TO  WORK-ACC-6A                  ECS034
00499                                      WORK-ACC-6B                  ECS034
00500                                      WORK-ACC-6C.                 ECS034
00501                                                                   ECS034
00502      ADD X-AMT                   TO  WORK-ACC-12A                 ECS034
00503                                      WORK-ACC-12B                 ECS034
00504                                      WORK-ACC-12C.                ECS034
00505                                                                   ECS034
00506      GO TO 0600-R-LOOP.                                           ECS034
00507                                                                   ECS034
00508  0720-SOME-KINDA-TOTALS.                                          ECS034
00509      IF X-TYPE = SV-TYPE                                          ECS034
00510          GO TO 0730-SOME-KINDA-TOTALS-B.                          ECS034
00511                                                                   ECS034
00512      PERFORM 0740-CLASS-TOTALS-HEADS THRU 0750-CLASS-TOTS-FILL.   ECS034
00513                                                                   ECS034
00514      PERFORM 0130-SET-INITIAL.                                    ECS034
00515      MOVE ACCUM-1                TO  ACCUM-2                      ECS034
00516                                      ACCUM-3                      ECS034
00517                                      SV-ZERO-ACCUM.               ECS034
00518                                                                   ECS034
00519      MOVE ZEROS                  TO  WORK-ACC-6A                  ECS034
00520                                      WORK-ACC-6B                  ECS034
00521                                      WORK-ACC-6C                  ECS034
00522                                      WORK-ACC-12A                 ECS034
00523                                      WORK-ACC-12B                 ECS034
00524                                      WORK-ACC-12C                 ECS034
00525                                      WORK-ACCUM.                  ECS034
00526      MOVE +1                     TO  PAGER.                       ECS034
00527                                                                   ECS034
00528      GO TO 0150-SET-HEADS.                                        ECS034
00529                                                                   ECS034
00530  0730-SOME-KINDA-TOTALS-B.                                        ECS034
00531                                                                   ECS034
00532      IF WS-ECS033-PGM-OPT EQUAL '2'                               ECS034
00533         IF DTE-TOT-OPT EQUAL '2'                                  ECS034
00534            IF X-GROUP NOT EQUAL SV-GROUP                          ECS034
00535               PERFORM 0740-CLASS-TOTALS-HEADS THRU                ECS034
00536                       0750-CLASS-TOTS-FILL                        ECS034
00537               MOVE ACCUM-2 TO ACCUM-1                             ECS034
00538               MOVE WORK-ACC-6B   TO WORK-ACC-6A                   ECS034
00539               MOVE WORK-ACC-12B  TO WORK-ACC-12A                  ECS034
00540               MOVE SPACES        TO HRA-ACCT                      ECS034
00541               PERFORM 0740-CLASS-TOTALS-HEADS THRU                ECS034
00542                       0750-CLASS-TOTS-FILL                        ECS034
00543               MOVE ACCUM-1       TO ACCUM-2                       ECS034
00544               MOVE WORK-ACC-6A   TO WORK-ACC-6B                   ECS034
00545               MOVE WORK-ACC-12A  TO WORK-ACC-12B                  ECS034
00546               GO TO 0150-SET-HEADS                                ECS034
00547            ELSE                                                   ECS034
00548            IF X-ACC NOT EQUAL SV-GA                               ECS034
00549               PERFORM 0740-CLASS-TOTALS-HEADS THRU                ECS034
00550                       0750-CLASS-TOTS-FILL                        ECS034
00551               GO TO 0150-SET-HEADS                                ECS034
00552            ELSE                                                   ECS034
00553               GO TO 0150-SET-HEADS                                ECS034
00554         ELSE                                                      ECS034
00555            IF X-GROUP NOT EQUAL SV-GROUP                          ECS034
00556               MOVE SPACES        TO HRA-ACCT                      ECS034
00557               PERFORM 0740-CLASS-TOTALS-HEADS THRU                ECS034
00558                       0750-CLASS-TOTS-FILL                        ECS034
00559               GO TO 0150-SET-HEADS.                               ECS034
00560                                                                   ECS034
00561      IF X-COMP NOT = SV-COMP                                      ECS034
00562          GO TO 1100-GA-TOTALS.                                    ECS034
00563                                                                   ECS034
00564      IF X-GA NOT = SV-GA                                          ECS034
00565          GO TO 1100-GA-TOTALS.                                    ECS034
00566                                                                   ECS034
00567  0740-CLASS-TOTALS-HEADS.                                         ECS034
00568                                                                   ECS034
00569      IF SV-TYPE = '1'                                             ECS034
00570          MOVE LIFE-OVERRIDE-L6   TO  HB-DESC.                     ECS034
00571                                                                   ECS034
00572      IF SV-TYPE = '2'                                             ECS034
00573          MOVE AH-OVERRIDE-L6     TO  HB-DESC.                     ECS034
00574                                                                   ECS034
00575      IF SV-TYPE = '3'                                             ECS034
00576          MOVE SPACES             TO  HB-DESC                      ECS034
00577          MOVE '  OB-'            TO  HB-DESC-1                    ECS034
00578          MOVE LIFE-OVERRIDE-L2   TO  HB-DESC-2.                   ECS034
00579                                                                   ECS034
00580      IF SV-TYPE = '4'                                             ECS034
00581          MOVE SPACES             TO  HB-DESC                      ECS034
00582          MOVE '  OB-'            TO  HB-DESC-1                    ECS034
00583          MOVE   AH-OVERRIDE-L2   TO  HB-DESC-2.                   ECS034
00584                                                                   ECS034
00585      MOVE PAGER                  TO  HB-PAGE.                     ECS034
00586                                                                   ECS034
00587      ADD +1                      TO  PAGER.                       ECS034
00588                                                                   ECS034
00589      MOVE HEAD-A                 TO  PRT.                         ECS034
00590      MOVE '1'                    TO  X.                           ECS034
00591                                                                   ECS034
00592      PERFORM 0800-PRT-RTN THRU 0899-EXIT.                         ECS034
00593                                                                   ECS034
00594      MOVE HEAD-B                 TO  PRT.                         ECS034
00595      MOVE ' '                    TO  X.                           ECS034
00596                                                                   ECS034
00597      PERFORM 0800-PRT-RTN THRU 0899-EXIT.                         ECS034
00598                                                                   ECS034
00599      MOVE HEAD-B2                TO  PRT.                         ECS034
00600      MOVE ' '                    TO  X.                           ECS034
00601                                                                   ECS034
00602      PERFORM 0800-PRT-RTN THRU 0899-EXIT.                         ECS034
00603                                                                   ECS034
00604      MOVE ' '                    TO  X.                           ECS034
00605                                                                   ECS034
00606      IF SV-ID = '1'                                               ECS034
00607          MOVE HEAD-D1            TO  PRT                          ECS034
00608      ELSE                                                         ECS034
00609      IF SV-ID = '2'                                               ECS034
00610         MOVE HEAD-C              TO  PRT                          ECS034
00611      ELSE                                                         ECS034
00612      IF SV-ID = '3'                                               ECS034
00613         MOVE HEAD-D              TO  PRT                          ECS034
00614      ELSE                                                         ECS034
00615      IF SV-ID = '4'                                               ECS034
00616         MOVE HEAD-D1             TO  PRT                          ECS034
00617      ELSE                                                         ECS034
00618      IF SV-ID = '5'                                               ECS034
00619         MOVE HEAD-C              TO  PRT                          ECS034
00620      ELSE                                                         ECS034
00621      IF SV-ID = '6'                                               ECS034
00622         MOVE HEAD-C              TO  PRT                          ECS034
00623      ELSE                                                         ECS034
00624      IF SV-ID = '9'                                               ECS034
00625         MOVE HEAD-R              TO  PRT.                         ECS034
00626                                                                   ECS034
00627      PERFORM 0800-PRT-RTN THRU 0899-EXIT.                         ECS034
00628                                                                   ECS034
00629      IF SV-ID = '9'                                               ECS034
00630         MOVE HEAD-RA             TO  PRT                          ECS034
00631         MOVE ' '                TO  X                             ECS034
00632         PERFORM 0800-PRT-RTN THRU 0899-EXIT.                      ECS034
00633                                                                   ECS034
00634      IF SV-ID = '1'  OR  '2'                                      ECS034
00635          MOVE HEAD-D             TO  PRT                          ECS034
00636          MOVE ' '                TO  X                            ECS034
00637          PERFORM 0800-PRT-RTN THRU 0899-EXIT.                     ECS034
00638                                                                   ECS034
00639      MOVE HEAD-E                 TO  PRT.                         ECS034
00640      MOVE '0'                    TO  X.                           ECS034
00641                                                                   ECS034
00642      PERFORM 0800-PRT-RTN THRU 0899-EXIT.                         ECS034
00643                                                                   ECS034
00644      MOVE SPACES                 TO  PRT                          ECS034
00645      MOVE ' '                    TO  X.                           ECS034
00646                                                                   ECS034
00647      PERFORM 0800-PRT-RTN THRU 0899-EXIT.                         ECS034
00648                                                                   ECS034
00649  0740-EXIT.                                                       ECS034
00650       EXIT.                                                       ECS034
00651       EJECT                                                       ECS034
00652  0750-CLASS-TOTS-FILL.                                            ECS034
00653      PERFORM 0900-LINE-FILLER                                     ECS034
00654          VARYING  CA  FROM  +1  BY  +1                            ECS034
00655 *            UNTIL CA GREATER +84.                                ECS034
062002             UNTIL CA GREATER +120.                               ECS034
00656                                                                   ECS034
00657      MOVE SPACES                 TO  D1-DTE.                      ECS034
00658 *    MOVE +85                    TO  CA.                          ECS034
062002     MOVE +121                   TO  CA.                          ECS034
00659                                                                   ECS034
00660      PERFORM 1000-LN-FILL-2                                       ECS034
00661          VARYING  CB  FROM  +1  BY  +1                            ECS034
00662              UNTIL CB GREATER +12.                                ECS034
00663                                                                   ECS034
00664      MOVE WORK-ACCUM             TO  D1-ACCUM.                    ECS034
00665      MOVE PD-SETS-1 (CA  13)     TO  D1-TOT.                      ECS034
00666      MOVE DATA-1                 TO  PRT.                         ECS034
00667      MOVE '0'                    TO  X.                           ECS034
00668                                                                   ECS034
00669      PERFORM 0800-PRT-RTN THRU 0899-EXIT.                         ECS034
00670                                                                   ECS034
00671      MOVE ZERO                   TO  WORK-ACCUM.                  ECS034
00672      MOVE 06                     TO  D2-MOS.                      ECS034
00673      MOVE WORK-ACC-6A            TO  D2-AMT.                      ECS034
00674      MOVE DATA-2                 TO  PRT.                         ECS034
00675      MOVE '0'                    TO  X.                           ECS034
00676                                                                   ECS034
00677      PERFORM 0800-PRT-RTN THRU 0899-EXIT.                         ECS034
00678                                                                   ECS034
00679      MOVE 12                     TO  D2-MOS.                      ECS034
00680      MOVE WORK-ACC-12A           TO  D2-AMT.                      ECS034
00681      MOVE DATA-2                 TO  PRT.                         ECS034
00682      MOVE ' '                    TO  X.                           ECS034
00683                                                                   ECS034
00684      PERFORM 0800-PRT-RTN THRU 0899-EXIT.                         ECS034
00685                                                                   ECS034
00686      MOVE ZEROS                  TO  WORK-ACC-6A  WORK-ACC-12A.   ECS034
00687      MOVE SV-ZERO-ACCUM          TO  ACCUM-1.                     ECS034
00688                                                                   ECS034
00689  0760-E-CLASS-TOT-FILL.                                           ECS034
00690      PERFORM 0130-SET-INITIAL.                                    ECS034
00691      MOVE ACCUM-1                TO  ACCUM-2                      ECS034
00692                                      ACCUM-3                      ECS034
00693                                      SV-ZERO-ACCUM.               ECS034
00694      GO TO 0150-SET-HEADS.                                        ECS034
00695                                                                   ECS034
00696  0800-PRT-RTN.                                                    ECS034
00697                              COPY ELCPRT2.                        ECS034
00698                                                                   ECS034
00699  0899-EXIT.                                                       ECS034
00700      EXIT.                                                        ECS034
00701                                                                   ECS034
00702  0900-LINE-FILLER.                                                ECS034
00703      PERFORM 1000-LN-FILL-2                                       ECS034
00704          VARYING  CB  FROM  +1  BY  +1                            ECS034
00705              UNTIL CB GREATER +12.                                ECS034
00706                                                                   ECS034
00707      MOVE PD-SETS-1 (CA  13)     TO  D1-TOT.                      ECS034
00708      MOVE WORK-ACCUM             TO  D1-ACCUM.                    ECS034
00709      MOVE SPACES                 TO  D1-COM.                      ECS034
00710                                                                   ECS034
00711      IF CA = +12                                                  ECS034
00712          MOVE 'I'                TO  D1-COM.                      ECS034
00713                                                                   ECS034
00714      IF CA = +14                                                  ECS034
00715          MOVE 'N'                TO  D1-COM.                      ECS034
00716                                                                   ECS034
00717      IF CA = +16                                                  ECS034
00718          MOVE 'C'                TO  D1-COM.                      ECS034
00719                                                                   ECS034
00720      IF CA = +18                                                  ECS034
00721          MOVE 'U'                TO  D1-COM.                      ECS034
00722                                                                   ECS034
00723      IF CA = +20                                                  ECS034
00724          MOVE 'R'                TO  D1-COM.                      ECS034
00725                                                                   ECS034
00726      IF CA = +22                                                  ECS034
00727          MOVE 'R'                TO  D1-COM.                      ECS034
00728                                                                   ECS034
00729      IF CA = +24                                                  ECS034
00730          MOVE 'E'                TO  D1-COM.                      ECS034
00731                                                                   ECS034
00732      IF CA = +26                                                  ECS034
00733          MOVE 'D'                TO  D1-COM.                      ECS034
00734                                                                   ECS034
00735      MOVE LN-CMT (CA)            TO  D1-DTE.                      ECS034
00736      MOVE DATA-1                 TO  PRT.                         ECS034
00737      MOVE ' '                    TO  X.                           ECS034
00738                                                                   ECS034
00739      PERFORM 0800-PRT-RTN THRU 0899-EXIT.                         ECS034
00740                                                                   ECS034
00741      MOVE ZERO                   TO  WORK-ACCUM.                  ECS034
00742                                                                   ECS034
00743 *    IF  CA   =   +48                                             ECS034
062002     IF  CA   =   +48   or +96                                    ECS034
00744          PERFORM 0740-CLASS-TOTALS-HEADS THRU 0740-EXIT.          ECS034
00745                                                                   ECS034
00746  1000-LN-FILL-2.                                                  ECS034
00747      MOVE PD-SETS-1 (CA  CB)     TO  D1-AMT (CB).                 ECS034
00748                                                                   ECS034
00749      ADD PD-SETS-1 (CA  CB)      TO  WORK-ACCUM.                  ECS034
00750                                                                   ECS034
00751  1100-GA-TOTALS.                                                  ECS034
00752      PERFORM 0740-CLASS-TOTALS-HEADS THRU 0750-CLASS-TOTS-FILL.   ECS034
00753                                                                   ECS034
00754      IF SV-ID = '1' OR  '2'                                       ECS034
00755          MOVE ZEROS              TO  WORK-ACC-6B  WORK-ACC-12B    ECS034
00756          MOVE SV-ZERO-ACCUM      TO  ACCUM-2.                     ECS034
00757                                                                   ECS034
00758      GO TO 1199-EXIT.                                             ECS034
00759                                                                   ECS034
00760  1199-EXIT.                                                       ECS034
00761      PERFORM 0130-SET-INITIAL.                                    ECS034
00762      MOVE ACCUM-1                TO  ACCUM-2                      ECS034
00763                                      ACCUM-3                      ECS034
00764                                      SV-ZERO-ACCUM.               ECS034
00765      GO TO 0150-SET-HEADS.                                        ECS034
00766                                                                   ECS034
00767  1200-COMP-TOT.                                                   ECS034
00768      PERFORM 0740-CLASS-TOTALS-HEADS THRU 0750-CLASS-TOTS-FILL.   ECS034
00769                                                                   ECS034
00770      MOVE 'ALL CLASSES'          TO  HD-CLASS.                    ECS034
00771      MOVE ACCUM-3                TO  ACCUM-1.                     ECS034
00772      MOVE WORK-ACC-6C            TO  WORK-ACC-6A.                 ECS034
00773      MOVE WORK-ACC-12C           TO  WORK-ACC-12A.                ECS034
00774      MOVE ZEROS                  TO  WORK-ACC-6C  WORK-ACC-12C.   ECS034
00775      MOVE SV-ZERO-ACCUM          TO  ACCUM-3.                     ECS034
00776                                                                   ECS034
00777      PERFORM 0740-CLASS-TOTALS-HEADS THRU 0750-CLASS-TOTS-FILL.   ECS034
00778                                                                   ECS034
00779  1299-EXIT.                                                       ECS034
00780      GO TO 0150-SET-HEADS.                                        ECS034
00781                                                                   ECS034
00782  1300-FINAL-TOTS.                                                 ECS034
00783                                                                   ECS034
00784      IF WS-ECS033-PGM-OPT EQUAL '2'                               ECS034
00785         PERFORM 0740-CLASS-TOTALS-HEADS THRU                      ECS034
00786                 0750-CLASS-TOTS-FILL                              ECS034
00787         IF DTE-TOT-OPT EQUAL '2'                                  ECS034
00788            MOVE ACCUM-2          TO ACCUM-1                       ECS034
00789            MOVE WORK-ACC-6B      TO WORK-ACC-6A                   ECS034
00790            MOVE WORK-ACC-12B     TO WORK-ACC-12A                  ECS034
00791            MOVE SPACES           TO HRA-ACCT                      ECS034
00792            PERFORM 0740-CLASS-TOTALS-HEADS THRU                   ECS034
00793                    0750-CLASS-TOTS-FILL                           ECS034
00794            MOVE ACCUM-3          TO ACCUM-1                       ECS034
00795            MOVE WORK-ACC-6C      TO WORK-ACC-6A                   ECS034
00796            MOVE WORK-ACC-12C     TO WORK-ACC-12A                  ECS034
00797            MOVE SPACES           TO HEAD-RA                       ECS034
00798            MOVE SPACES           TO HEAD-R                        ECS034
00799            MOVE '    REPORT TOTALS '                              ECS034
00800                                  TO HEAD-R                        ECS034
00801            PERFORM 0740-CLASS-TOTALS-HEADS THRU                   ECS034
00802                    0750-CLASS-TOTS-FILL                           ECS034
00803         ELSE                                                      ECS034
00804            MOVE ACCUM-3          TO ACCUM-1                       ECS034
00805            MOVE WORK-ACC-6C      TO WORK-ACC-6A                   ECS034
00806            MOVE WORK-ACC-12C     TO WORK-ACC-12A                  ECS034
00807            MOVE SPACES           TO HEAD-RA                       ECS034
00808            MOVE SPACES           TO HEAD-R                        ECS034
00809            MOVE '    REPORT TOTALS '                              ECS034
00810                                  TO HEAD-R                        ECS034
00811            PERFORM 0740-CLASS-TOTALS-HEADS THRU                   ECS034
00812                    0750-CLASS-TOTS-FILL                           ECS034
00813      ELSE                                                         ECS034
00814         PERFORM 0740-CLASS-TOTALS-HEADS THRU                      ECS034
00815                 0750-CLASS-TOTS-FILL.                             ECS034
00816                                                                   ECS034
00817  1310-FINAL-TOTS-END.                                             ECS034
00818                                                                   ECS034
00819      MOVE SPACES                 TO  PRT.                         ECS034
00820      MOVE '1'                    TO  X.                           ECS034
00821                                                                   ECS034
00822      PERFORM 0800-PRT-RTN THRU 0899-EXIT.                         ECS034
00823                                                                   ECS034
00824      CLOSE CLM-XTR  PRT-RPT.                                      ECS034
00825                                                                   ECS034
00826  1320-CLOSE-FICH.                                                 ECS034
00827                              COPY ELCPRTC.                        ECS034
00828  1330-END-JOB.                                                    ECS034
00829      GOBACK.                                                      ECS034
00830                                                                   ECS034
00831  ABEND-PGM SECTION.                                               ECS034
00832                              COPY ELCABEND.                       ECS034
00833                                                                      CL**3
