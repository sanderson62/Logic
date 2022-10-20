00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   ECS042
00003  PROGRAM-ID.                ECS042.                                  LV009
00004 *              PROGRAM CONVERTED BY                               ECS042
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS042
00006 *              CONVERSION DATE 02/08/96 14:32:11.                 ECS042
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS042
00008 *                           VMOD=2.005                            ECS042
00009                                                                   ECS042
00010 *AUTHOR.        LOGIC, INC.                                       ECS042
00011 *               DALLAS, TEXAS.                                    ECS042
00012                                                                   ECS042
00013 *DATE-COMPILED.                                                   ECS042
00014                                                                   ECS042
00015 *SECURITY.   *****************************************************ECS042
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS042
00017 *            *                                                   *ECS042
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS042
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS042
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS042
00021 *            *                                                   *ECS042
00022 *            *****************************************************ECS042
00023                                                                   ECS042
00024 *REMARKS.                                                         ECS042
00025 *        PRINTS DETAIL LOSS RESERVE REPORT AND LAST 36 MONTHS -   ECS042
00026 *        OF PROCESS TOTALS FOR -                                  ECS042
00027                                                                   ECS042
00028 *        1.  STATE                                                ECS042
00029 *        2.  GROUP                                                ECS042
00030 *        3.  CARRIER                                              ECS042
00031 *        4.  GRAND                                                ECS042
00032                                                                   ECS042
00033  ENVIRONMENT DIVISION.                                            ECS042
00034  INPUT-OUTPUT SECTION.                                            ECS042
00035  FILE-CONTROL.                                                    ECS042
00036                                                                   ECS042
00037      SELECT SORT-FILE    ASSIGN TO
      *                   '/data/seqfiles/sortwk1'.
                          SYS001-UT-FBA1-S-SORTWK1.
00038      SELECT PRNTR        ASSIGN TO
      *                   '/data/seqfiles/sys008'.
                          SYS008-UR-1403-S-SYS008.
00039      SELECT RESERV-HIST  ASSIGN TO
      *                   '/data/seqfiles/CI.XX.RESERVES_00'.
                          SYS010-UT-2400-S-SYS010.
00040      SELECT DISK-DATE    ASSIGN TO
      *       '/data/seqfiles/CI.DD.ER.DATECARD'.
                          SYS019-UT-FBA1-S-SYS019.
00041      SELECT FICH         ASSIGN TO
      *       '/data/seqfiles/CI.EX.FICH042'.
                          SYS020-UT-2400-S-SYS020.
00042  EJECT                                                            ECS042
00043  DATA DIVISION.                                                   ECS042
00044  FILE SECTION.                                                    ECS042
00045                                                                   ECS042
00046  SD  SORT-FILE.                                                      CL**5
00047                                                                      CL**5
00048  01  SORT-FILE-REC.                                               ECS042
00049      12 SORT-CONTROL-74.                                          ECS042
00050          16  SORT-CARRIER        PIC  X.                          ECS042
00051          16  SORT-GROUPING       PIC  X(6).                       ECS042
00052          16  SORT-STATE          PIC  XX.                         ECS042
00053          16  SORT-ACCOUNT        PIC  X(10).                      ECS042
00054          16  SORT-PROCESS-DATE   PIC  9(11)  COMP-3.              ECS042
00055          16  SORT-CERT           PIC  X(11).                      ECS042
00056          16  SORT-CLMNO          PIC  X(7).                       ECS042
00057      12  SORT-REST.                                               ECS042
00058          16  FILLER              PIC  X(390).                     ECS042
00059          16  SORT-RSV-ACC-NAME   PIC  X(30).                      ECS042
00060          16  FILLER              PIC  X(90).                      ECS042
00061  EJECT                                                            ECS042
00062  FD  PRNTR                                                        ECS042
00063                              COPY ELCPRTFD.                       ECS042
00064  EJECT                                                            ECS042
00065  FD  RESERV-HIST                                                  ECS042
00066      BLOCK CONTAINS 0 RECORDS
00067      RECORDING MODE F.                                               CL**5
00068                                                                      CL**5
00069  01  RESERV-REC              PIC X(510).                          ECS042
00070                                                                   ECS042
00071  EJECT                                                            ECS042
00072  FD  DISK-DATE                                                    ECS042
00073                              COPY ELCDTEFD.                       ECS042
00074  EJECT                                                            ECS042
00075  FD  FICH                                                         ECS042
00076                              COPY ELCFCHFD.                       ECS042
00077  EJECT                                                            ECS042
00078  WORKING-STORAGE SECTION.                                         ECS042
00079  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS042
00080  77  FILLER  PIC X(32) VALUE '********************************'.  ECS042
00081  77  FILLER  PIC X(32) VALUE '     ECS042 WORKING STORAGE     '.  ECS042
00082  77  FILLER  PIC X(32) VALUE '********VMOD=2.005**************'.  ECS042
00083                                                                   ECS042
00084  77  X1                      PIC S9(3) COMP      VALUE +000.      ECS042
00085  77  Y1                      PIC S9(3) COMP      VALUE +000.      ECS042
00086  77  PGM-SUB                 PIC S9(3) COMP      VALUE +042.      ECS042
00087  77  LINE-CNT                PIC S9(3) COMP-3    VALUE ZEROS.     ECS042
00088  77  PAGE-CNT                PIC S9(5) COMP-3    VALUE ZEROS.     ECS042
00089  77  X                       PIC  X.                              ECS042
00090  77  SPACE-NP                PIC  X              VALUE '1'.       ECS042
00091  77  SPACE-1                 PIC  X              VALUE ' '.       ECS042
00092  77  SPACE-2                 PIC  X              VALUE '0'.       ECS042
00093  77  SPACE-3                 PIC  X              VALUE '-'.       ECS042
00094  77  EOF-DEX                 PIC  X              VALUE SPACE.     ECS042
00095  77  QTR-SW                  PIC  X              VALUE SPACE.     ECS042
00096      88  QTR-END                                 VALUE '1'.       ECS042
00097  77  QTR-COMP                PIC  X              VALUE SPACE.     ECS042
00098      88  QTR-CO                                  VALUE '1'.       ECS042
00099  77  FIRST-TIME-SWITCH       PIC  X              VALUE 'Y'.       ECS042
00100      88  FIRST-TIME                              VALUE 'Y'.       ECS042
00101  77  FROM-ACCOUNT-BREAK-SW   PIC  X              VALUE 'N'.       ECS042
00102      88  FROM-ACCOUNT-BREAK                      VALUE 'Y'.       ECS042
00103  77  FROM-STATE-BREAK-SW     PIC  X              VALUE 'N'.       ECS042
00104      88  FROM-STATE-BREAK                        VALUE 'Y'.       ECS042
00105  77  FROM-GROUP-BREAK-SW     PIC  X              VALUE 'N'.       ECS042
00106      88  FROM-GROUP-BREAK                        VALUE 'Y'.       ECS042
00107  77  FROM-CARR-BREAK-SW      PIC  X              VALUE 'N'.       ECS042
00108      88  FROM-CARR-BREAK                         VALUE 'Y'.       ECS042
00109  77  FROM-GRAND-BREAK-SW     PIC  X              VALUE 'N'.       ECS042
00110      88  FROM-GRAND-BREAK                        VALUE 'Y'.       ECS042
00111  77  ST-SUB                  PIC  99             VALUE ZEROS.     ECS042
00112  EJECT                                                            ECS042
00113  01  SAVE-CONTROL.                                                ECS042
00114      12  SAVE-CARRIER        PIC  X.                              ECS042
00115      12  SAVE-GROUPING       PIC  X(6).                           ECS042
00116      12  SAVE-STATE          PIC  XX.                             ECS042
00117      12  SAVE-ACCOUNT        PIC  X(10).                          ECS042
00118      12  SAVE-PROCESS-DATE   PIC  9(11)   COMP-3.                 ECS042
00119      12  SAVE-CERT           PIC  X(11).                          ECS042
00120      12  SAVE-CLMNO          PIC  X(7).                           ECS042
00121                                                                   ECS042
00122  01  HOLD-AREA.                                                   ECS042
00123      12  HOLD-CARRIER        PIC  X.                              ECS042
00124      12  HOLD-GROUPING       PIC  X(6).                           ECS042
00125      12  HOLD-STATE          PIC  XX.                             ECS042
00126      12  HOLD-ACCOUNT        PIC  X(10).                          ECS042
00127      12  HOLD-ACCT-NAME      PIC  X(30).                          ECS042
00128                                                                   ECS042
00129  01  RUN-DT.                                                      ECS042
00130      12  RUN-DT-CC           PIC  99             VALUE ZEROS.     ECS042
00131      12  RUN-DT-YR           PIC  99             VALUE ZEROS.     ECS042
00132      12  RUN-DT-MO           PIC  99             VALUE ZEROS.     ECS042
00133                                                                   ECS042
00134  01  RUN9DT  REDEFINES                                            ECS042
00135      RUN-DT                  PIC  9(6).                           ECS042
00136                                                                   ECS042
00137  01  WS.                                                          ECS042
00138      12  WS-RETURN-CODE          PIC S9(4) COMP VALUE ZERO.       ECS042
00139      12  WS-ZERO                 PIC S9(1)       VALUE ZERO.      ECS042
00140      12  WS-ABEND-MESSAGE        PIC  X(80)      VALUE SPACES.    ECS042
00141      12  WS-ABEND-FILE-STATUS    PIC  XX         VALUE ZERO.      ECS042
00142      12  WS-SORT-PROCESS-DATE    PIC  9(11)      VALUE 0.            CL**4
00143      12  WS-SORT-PROCESS-DATE-R REDEFINES WS-SORT-PROCESS-DATE.      CL**4
00144          16  FILLER              PIC  999.                        ECS042
00145          16  SORT-PROC-CCYY      PIC  9(04).                      ECS042
00146          16  SORT-PROC-CCYR REDEFINES SORT-PROC-CCYY.             ECS042
00147              20  SORT-PROC-CC    PIC  99.                         ECS042
00148              20  SORT-PROC-YR    PIC  99.                         ECS042
00149          16  SORT-PROC-MO        PIC  99.                         ECS042
00150          16  SORT-PROC-DA        PIC  99.                         ECS042
00151                                                                   ECS042
00152  01  COMPARE-DATE-TABLE.                                          ECS042
00153      12  COMPARE-DTS     OCCURS  36  TIMES.                       ECS042
00154          16  COMPARE-DT.                                          ECS042
00155              20  COMP-CC     PIC  99.                             ECS042
00156              20  COMP-YR     PIC  99.                             ECS042
00157              20  COMP-MO     PIC  99.                             ECS042
00158                                                                   ECS042
00159  01  COMPARE-DATE9TABLE  REDEFINES  COMPARE-DATE-TABLE.           ECS042
00160      12  COMPARE9DTS     OCCURS  36  TIMES.                       ECS042
00161          16  COMPARE9DT      PIC  9(6).                           ECS042
00162                                                                   ECS042
00163  01  THREE-YEAR-OLD-DATE     PIC  9(6).                           ECS042
00164                                                                   ECS042
00165  01  DE-DATE.                                                     ECS042
00166      12  DE-CC               PIC  XX             VALUE ZEROS.     ECS042
00167      12  DE-YR               PIC  XX             VALUE ZEROS.     ECS042
00168      12  DE-MO               PIC  XX             VALUE ZEROS.     ECS042
00169                                                                   ECS042
00170  01  DE9DATE  REDEFINES                                           ECS042
00171      DE-DATE                 PIC  9(6).                           ECS042
00172                                                                   ECS042
00173  01  CK-DE-DATE.                                                  ECS042
00174      12  CK-DE-CC            PIC  XX.                             ECS042
00175      12  CK-DE-YR            PIC  XX.                             ECS042
00176      12  CK-DE-MO            PIC  XX.                             ECS042
00177                                                                   ECS042
00178  01  CK-DE9DATE  REDEFINES                                        ECS042
00179      CK-DE-DATE              PIC  9(6).                           ECS042
00180  EJECT                                                            ECS042
00181  01  WORK-ACCUM      COMP-3.                                      ECS042
00182      12  WA-FUTURE-RESV      PIC S9(9)V99        VALUE ZEROS.     ECS042
00183      12  WA-IBNR-RESV        PIC S9(9)V99        VALUE ZEROS.     ECS042
00184      12  WA-PAY-TO-CURRENT   PIC S9(9)V99        VALUE ZEROS.     ECS042
00185                                                                   ECS042
00186  01  ZERO-ACCUM      COMP-3.                                      ECS042
00187      12  FILLER              PIC S9(9)V99        VALUE ZEROS.     ECS042
00188      12  FILLER              PIC S9(9)V99        VALUE ZEROS.     ECS042
00189      12  FILLER              PIC S9(9)V99        VALUE ZEROS.     ECS042
00190                                                                   ECS042
00191  01  ACCUMULATORS    COMP-3.                                      ECS042
00192      12  ACCOUNT-ACCUM   OCCURS  36  TIMES.                       ECS042
00193          16  AC-FUTURE       PIC S9(9)V99.                        ECS042
00194          16  AC-IBNR         PIC S9(9)V99.                        ECS042
00195          16  AC-PTC          PIC S9(9)V99.                        ECS042
00196      12  STATE-ACCUM     OCCURS  36  TIMES.                       ECS042
00197          16  ST-FUTURE       PIC S9(9)V99.                        ECS042
00198          16  ST-IBNR         PIC S9(9)V99.                        ECS042
00199          16  ST-PTC          PIC S9(9)V99.                        ECS042
00200      12  GROUPING-ACCUM  OCCURS  36  TIMES.                       ECS042
00201          16  GP-FUTURE       PIC S9(9)V99.                        ECS042
00202          16  GP-IBNR         PIC S9(9)V99.                        ECS042
00203          16  GP-PTC          PIC S9(9)V99.                        ECS042
00204      12  CARRIER-ACCUM   OCCURS  36  TIMES.                       ECS042
00205          16  CA-FUTURE       PIC S9(9)V99.                        ECS042
00206          16  CA-IBNR         PIC S9(9)V99.                        ECS042
00207          16  CA-PTC          PIC S9(9)V99.                        ECS042
00208      12  GRAND-ACCUM     OCCURS  36  TIMES.                       ECS042
00209          16  GT-FUTURE       PIC S9(9)V99.                        ECS042
00210          16  GT-IBNR         PIC S9(9)V99.                        ECS042
00211          16  GT-PTC          PIC S9(9)V99.                        ECS042
00212  EJECT                                                            ECS042
00213                              COPY ECSEXT01.                       ECS042
00214                              COPY ELCEXTVR.                       ECS042
00215  EJECT                                                            ECS042
00216  01  HD-1.                                                        ECS042
00217      12  FILLER              PIC  X(50)          VALUE SPACES.    ECS042
00218      12  FILLER              PIC  X(19)          VALUE            ECS042
00219              'LOSS RESERVE DETAIL'.                               ECS042
00220      12  FILLER              PIC  X(50)
                  VALUE ' 60 MOS AND UNDER '.
00221      12  FILLER              PIC  X(12)          VALUE 'ECS042'.  ECS042
00222                                                                   ECS042
00223  01  HD-2.                                                        ECS042
00224      12  FILLER              PIC  X(11)          VALUE            ECS042
00225              ' CARRIER - '.                                       ECS042
00226      12  HD-CARRIER          PIC  X              VALUE SPACES.    ECS042
00227      12  FILLER              PIC  X(33)          VALUE SPACES.    ECS042
00228      12  HD-COMPANY-NAME     PIC  X(30)          VALUE SPACES.    ECS042
00229      12  FILLER              PIC  X(44)          VALUE SPACES.    ECS042
00230      12  HD-IPL-DATE         PIC  X(8)           VALUE SPACES.    ECS042
00231      12  FILLER              PIC  X(4)           VALUE SPACES.    ECS042
00232                                                                   ECS042
00233  01  HD-3.                                                        ECS042
00234      12  FILLER              PIC  X(11)          VALUE            ECS042
00235              ' GROUP   - '.                                       ECS042
00236      12  HD-GROUPING         PIC  X(6)           VALUE SPACES.    ECS042
00237      12  FILLER              PIC  X(33)          VALUE SPACES.    ECS042
00238      12  HD-ALPHA-DATE       PIC  X(18)          VALUE SPACES.    ECS042
00239      12  FILLER              PIC  X(51)          VALUE SPACES.    ECS042
00240      12  FILLER              PIC  X(5)           VALUE 'PAGE '.   ECS042
00241      12  HD-PAGE             PIC ZZ,ZZ9.                          ECS042
00242      12  FILLER              PIC  X(12)          VALUE SPACES.    ECS042
00243                                                                   ECS042
00244  01  HD-4.                                                        ECS042
00245      12  FILLER              PIC  X(11)          VALUE            ECS042
00246              ' STATE   - '.                                       ECS042
00247      12  HD-STATE            PIC  XX             VALUE SPACES.    ECS042
00248      12  FILLER              PIC  X(119)         VALUE SPACES.    ECS042
00249                                                                   ECS042
00250  01  HD-5.                                                        ECS042
00251      12  FILLER              PIC  X(11)          VALUE            ECS042
00252              ' ACCOUNT - '.                                       ECS042
00253      12  HD-ACCOUNT          PIC  X(10)          VALUE SPACES.    ECS042
00254      12  FILLER              PIC  XX             VALUE SPACES.    ECS042
00255      12  HD-ACCT-NAME        PIC  X(30)          VALUE SPACES.    ECS042
00256      12  FILLER              PIC  X(79)          VALUE SPACES.    ECS042
00257  EJECT                                                            ECS042
00258  01  HD-6.                                                        ECS042
00259      12  FILLER              PIC  X(44)          VALUE            ECS042
00260              '   CERTIFICATE   EFFECTIVE     CLAIM        '.      ECS042
00261      12  FILLER              PIC  X(44)          VALUE            ECS042
00262              '       INCURRED     PROCESS      FUTURE(CDT)'.      ECS042
00263      12  FILLER              PIC  X(44)          VALUE            ECS042
00264              '         I.B.N.R.         PAY TO CURRENT    '.      ECS042
00265                                                                   ECS042
00266  01  HD-6A.                                                       ECS042
00267      12  FILLER              PIC  X(44)          VALUE SPACES.    ECS042
00268      12  FILLER              PIC  X(44)          VALUE            ECS042
00269              '                    PROCESS      FUTURE(CDT)'.      ECS042
00270      12  FILLER              PIC  X(44)          VALUE            ECS042
00271              '         I.B.N.R.         PAY TO CURRENT    '.      ECS042
00272                                                                   ECS042
00273  01  HD-7.                                                        ECS042
00274      12  FILLER              PIC  X(44)          VALUE            ECS042
00275              '     NUMBER         DATE       NUMBER     TY'.      ECS042
00276      12  FILLER              PIC  X(44)          VALUE            ECS042
00277              'PE       DATE        DATE          RESERVE  '.      ECS042
00278      12  FILLER              PIC  X(44)          VALUE            ECS042
00279              '          RESERVE            RESERVE        '.      ECS042
00280                                                                   ECS042
00281  01  HD-7A.                                                       ECS042
00282      12  FILLER              PIC  X(28)          VALUE SPACES.    ECS042
00283      12  HD-7A-TITLE         PIC  X(10)          VALUE SPACES.    ECS042
00284      12  FILLER              PIC  X(6)           VALUE 'TOTALS'.  ECS042
00285      12  FILLER              PIC  X(44)          VALUE            ECS042
00286              '                     DATE          RESERVE  '.      ECS042
00287      12  FILLER              PIC  X(44)          VALUE            ECS042
00288              '          RESERVE            RESERVE        '.      ECS042
00289  EJECT                                                            ECS042
00290  01  DETAIL-LINE.                                                 ECS042
00291      12  FILLER              PIC  X(3).                           ECS042
00292      12  DET-CERTIFICATE     PIC  X(11).                          ECS042
00293      12  FILLER              PIC  X(4).                           ECS042
00294      12  DET-EFF-MO          PIC  XX.                             ECS042
00295      12  DET-EFF-MOD         PIC  X.                              ECS042
00296      12  DET-EFF-DA          PIC  XX.                             ECS042
00297      12  DET-EFF-DAD         PIC  X.                              ECS042
00298      12  DET-EFF-YR          PIC  XX.                             ECS042
00299      12  FILLER              PIC  X(4).                           ECS042
00300      12  DET-CLAIM-NUMBER    PIC  X(7).                           ECS042
00301      12  FILLER              PIC  X(4).                           ECS042
00302      12  DET-TYPE            PIC  X(6).                           ECS042
00303      12  FILLER              PIC  X(4).                           ECS042
00304      12  DET-INC-MO          PIC  XX.                             ECS042
00305      12  DET-INC-MOD         PIC  X.                              ECS042
00306      12  DET-INC-DA          PIC  XX.                             ECS042
00307      12  DET-INC-DAD         PIC  X.                              ECS042
00308      12  DET-INC-YR          PIC  XX.                             ECS042
00309      12  FILLER              PIC  X(4).                           ECS042
00310      12  DET-PROCESS-MO      PIC  XX.                             ECS042
00311      12  DET-PROCESS-MOD     PIC  X.                              ECS042
00312      12  DET-PROCESS-DA      PIC  XX.                             ECS042
00313      12  DET-PROCESS-DAD     PIC  X.                              ECS042
00314      12  DET-PROCESS-YR      PIC  XX.                             ECS042
00315      12  FILLER              PIC  X(4).                           ECS042
00316      12  DET-FUTURE-RESV     PIC ZZZ,ZZZ,ZZZ.99-.                 ECS042
00317      12  FILLER              PIC  X(4).                           ECS042
00318      12  DET-IBNR-RESV       PIC ZZZ,ZZZ,ZZZ.99-.                 ECS042
00319      12  FILLER              PIC  X(4).                           ECS042
00320      12  DET-PAY-TO-CURRENT  PIC ZZZ,ZZZ,ZZZ.99-.                 ECS042
00321      12  FILLER              PIC  X(4).                           ECS042
00322                                                                   ECS042
00323  01  PROCESS-DATE-TOTAL-LINE  REDEFINES  DETAIL-LINE.             ECS042
00324      12  FILLER              PIC  X(33).                          ECS042
00325      12  PDTL-TITLE          PIC  X(26).                          ECS042
00326      12  FILLER              PIC  X(73).                          ECS042
00327                                                                   ECS042
00328  01  TOTAL-LINE  REDEFINES  DETAIL-LINE.                          ECS042
00329      12  FILLER              PIC  X(64).                          ECS042
00330      12  TOT-PROCESS-DATE.                                        ECS042
00331          16  TOT-PRO-MO      PIC  X(3).                           ECS042
00332          16  FILLER          PIC  X.                              ECS042
00333          16  TOT-PRO-YR      PIC  XX.                             ECS042
00334      12  FILLER              PIC  X(62).                          ECS042
00335                                                                   ECS042
00336  COPY ELCDATE.                                                       CL**9
00337                              COPY ELCDTECX.                       ECS042
00338  EJECT                                                            ECS042
00339                              COPY ELCDTEVR.                       ECS042
00340  EJECT                                                            ECS042
00341  PROCEDURE DIVISION.                                              ECS042
00342                                                                   ECS042
00343  0000-INITIALIZE.                                                 ECS042
00344                              COPY ELCDTERX.                       ECS042
00345                                                                   ECS042
00346      MOVE WS-CURRENT-DATE        TO  HD-IPL-DATE.                 ECS042
00347      MOVE COMPANY-NAME           TO  HD-COMPANY-NAME.             ECS042
00348      MOVE ALPH-DATE              TO  HD-ALPHA-DATE.               ECS042
00349                                                                   ECS042
00350      IF DTE-QTR-CO = '1'                                          ECS042
00351          MOVE '1'                TO  QTR-COMP.                    ECS042
00352                                                                   ECS042
00353  0100-SORT-ROUTINE SECTION.                                       ECS042
00354      SORT SORT-FILE  ASCENDING KEY SORT-CONTROL-74                ECS042
00355          INPUT  PROCEDURE 1000-READ-CLAIMS    THRU  1999-EXIT     ECS042
00356          OUTPUT PROCEDURE 2000-PRINT-ROUTINE  THRU  3799-EXIT.    ECS042
00357                                                                   ECS042
00358      IF SORT-RETURN NOT = ZEROS                                   ECS042
00359          MOVE '0101'             TO  WS-RETURN-CODE               ECS042
00360          GO TO 9000-ABORT.                                        ECS042
00361                                                                   ECS042
00362      GOBACK.                                                      ECS042
00363  EJECT                                                            ECS042
00364  1000-READ-CLAIMS SECTION.                                        ECS042
00365      OPEN INPUT  RESERV-HIST.                                     ECS042
00366                                                                   ECS042
00367  1100-READ-RESERV-HISTORY.                                        ECS042
00368      READ RESERV-HIST  INTO  DETAIL-EXTRACT  AT END               ECS042
00369          GO TO 1990-END-OF-INPUT.                                 ECS042
00370                                                                   ECS042
00371      IF NOT  VALID-DE-ID                                          ECS042
00372          GO TO 1100-READ-RESERV-HISTORY.                          ECS042
00373                                                                   ECS042
00374      IF DE-REIN = 'R'                                             ECS042
00375          GO TO 1100-READ-RESERV-HISTORY.                          ECS042
00376                                                                   ECS042
00377      IF NOT  DE-RESERVE                                           ECS042
00378          GO TO 1100-READ-RESERV-HISTORY.                          ECS042
00379                                                                   ECS042
PEMTMP     IF (DE-RSV-PROC-DT = 20081231)
              AND (DE-STATE = 'MN')
              CONTINUE
           ELSE
              GO TO 1100-READ-RESERV-HISTORY
           END-IF
           
           IF DE-LF-TERM NOT NUMERIC
              MOVE ZEROS               TO DE-LF-TERM
           END-IF
           IF DE-AH-TERM NOT NUMERIC
              MOVE ZEROS               TO DE-AH-TERM
           END-IF

           IF ((DE-LF-TERM NOT = ZERO)
              AND (DE-LF-TERM < 61))
                      OR
              ((DE-AH-TERM NOT = ZEROS)
              AND (DE-AH-TERM < 61))
              CONTINUE
           ELSE
              GO TO 1100-READ-RESERV-HISTORY
           END-IF
              
00380      MOVE DE-CARRIER             TO  SORT-CARRIER.                ECS042
00381      MOVE DE-GROUPING            TO  SORT-GROUPING.               ECS042
00382      MOVE DE-STATE               TO  SORT-STATE.                  ECS042
00383      MOVE DE-ACCOUNT             TO  SORT-ACCOUNT.                ECS042
00384      MOVE DE-RSV-PROC-DT         TO  SORT-PROCESS-DATE            ECS042
00385                                      WS-SORT-PROCESS-DATE            CL**4
00386      MOVE DE-CERT                TO  SORT-CERT.                   ECS042
00387      MOVE DE-CLMNO               TO  SORT-CLMNO.                  ECS042
00388      MOVE DETAIL-EXTRACT         TO  SORT-REST.                   ECS042
00389                                                                   ECS042
00390      RELEASE SORT-FILE-REC.                                       ECS042
00391                                                                   ECS042
00392      GO TO 1100-READ-RESERV-HISTORY.                              ECS042
00393                                                                   ECS042
00394  1990-END-OF-INPUT.                                               ECS042
00395      CLOSE RESERV-HIST.                                           ECS042
00396                                                                   ECS042
00397  1999-EXIT.                                                       ECS042
00398      EXIT.                                                        ECS042
00399  EJECT                                                            ECS042
00400  2000-PRINT-ROUTINE SECTION.                                      ECS042
00401      OPEN OUTPUT PRNTR.                                           ECS042
00402                                                                   ECS042
00403      MOVE LOW-VALUES             TO  SAVE-CONTROL.                ECS042
00404                                                                   ECS042
00405  2100-CLEAR-CNTRS.                                                ECS042
00406      PERFORM 2300-ZERO-ACCUM-ACCOUNT  THRU  2399-EXIT.            ECS042
00407      PERFORM 2400-ZERO-ACCUM-STATE    THRU  2499-EXIT.            ECS042
00408      PERFORM 2500-ZERO-ACCUM-GROUP    THRU  2599-EXIT.            ECS042
00409      PERFORM 2600-ZERO-ACCUM-CARRIER  THRU  2699-EXIT.            ECS042
00410      PERFORM 2700-ZERO-ACCUM-GRAND    THRU  2799-EXIT.            ECS042
00411                                                                   ECS042
00412      IF QTR-CO                                                    ECS042
00413          IF RUN-MO = 03  OR  06  OR  09  OR  12                   ECS042
00414              MOVE '1'            TO  QTR-SW.                      ECS042
00415                                                                   ECS042
00416      MOVE RUN-MO                 TO  RUN-DT-MO.                   ECS042
00417      MOVE RUN-YR                 TO  RUN-DT-YR.                   ECS042
00418      MOVE RUN-CC                 TO  RUN-DT-CC.                   ECS042
00419      MOVE +1                     TO  X1.                          ECS042
00420      MOVE +0                     TO  Y1.                          ECS042
00421                                                                   ECS042
00422      IF QTR-CO  AND  QTR-END                                      ECS042
00423          COMPUTE COMPARE9DT (X1) = RUN9DT - 899                   ECS042
00424          COMPUTE THREE-YEAR-OLD-DATE = RUN9DT - 900               ECS042
00425      ELSE                                                         ECS042
00426          COMPUTE COMPARE9DT (X1) = RUN9DT - 299                   ECS042
00427          COMPUTE THREE-YEAR-OLD-DATE = RUN9DT - 300.              ECS042
00428                                                                   ECS042
00429      IF COMP-MO (X1) GREATER 12                                   ECS042
00430          COMPUTE COMPARE9DT (X1) = COMPARE9DT (X1) + 0088.        ECS042
00431                                                                   ECS042
00432  2110-BUILD-DATE-TABLE.                                           ECS042
00433      ADD +1                      TO  X1  Y1.                      ECS042
00434                                                                   ECS042
00435      IF X1 GREATER +36                                            ECS042
00436          GO TO 2200-RETURN-SORT-FILE.                             ECS042
00437                                                                   ECS042
00438      IF QTR-CO                                                    ECS042
00439        AND  QTR-END                                               ECS042
00440          COMPUTE COMPARE9DT (X1) = COMPARE9DT (Y1) + 0003         ECS042
00441      ELSE                                                         ECS042
00442          COMPUTE COMPARE9DT (X1) = COMPARE9DT (Y1) + 0001.        ECS042
00443                                                                   ECS042
00444      IF COMP-MO (X1) GREATER 12                                   ECS042
00445          COMPUTE COMPARE9DT (X1) = COMPARE9DT (X1) + 0088.        ECS042
00446                                                                   ECS042
00447      GO TO 2110-BUILD-DATE-TABLE.                                 ECS042
00448  EJECT                                                            ECS042
00449  2200-RETURN-SORT-FILE.                                           ECS042
00450      RETURN SORT-FILE  AT END                                     ECS042
00451          MOVE '1'                TO  EOF-DEX                      ECS042
00452          GO TO 3600-PRINT-GRAND-TOTALS.                           ECS042
00453                                                                   ECS042
00454  2210-CHECK-DATE-RANGE.                                           ECS042

           MOVE SORT-PROCESS-DATE      TO WS-SORT-PROCESS-DATE

00455      MOVE SORT-PROC-CC           TO  CK-DE-CC.                    ECS042
00456      MOVE SORT-PROC-YR           TO  CK-DE-YR.                    ECS042
00457      MOVE SORT-PROC-MO           TO  CK-DE-MO.                    ECS042
00458                                                                   ECS042
00459      IF CK-DE9DATE GREATER THREE-YEAR-OLD-DATE                    ECS042
00460          NEXT SENTENCE                                            ECS042
00461      ELSE                                                         ECS042
00462          GO TO 2200-RETURN-SORT-FILE.                             ECS042
00463                                                                   ECS042
00464  2220-CHECK-EXTRACT-SEQ.                                          ECS042
00465      IF SORT-CONTROL-74 LESS SAVE-CONTROL                         ECS042
00466          DISPLAY 'SORTED CLAIM RESERVES OUT OF SEQUENCE'          ECS042
00467          DISPLAY 'SAVE = ' SAVE-CONTROL                           ECS042
00468          DISPLAY 'SORT = ' SORT-CONTROL-74                        ECS042
00469          MOVE '0610'             TO  WS-RETURN-CODE               ECS042
00470          GO TO 9000-ABORT.                                        ECS042
00471                                                                   ECS042
00472  2230-COMPARE-RECORDS.                                            ECS042
00473      IF SORT-CARRIER NOT = SAVE-CARRIER                           ECS042
00474          PERFORM 3200-CARRIER-BREAK  THRU  3299-EXIT              ECS042
00475          GO TO 2240-ACCUMULATE.                                   ECS042
00476                                                                   ECS042
00477      IF SORT-GROUPING NOT = SAVE-GROUPING                         ECS042
00478          PERFORM 3100-GROUPING-BREAK  THRU  3199-EXIT             ECS042
00479          GO TO 2240-ACCUMULATE.                                   ECS042
00480                                                                   ECS042
00481      IF SORT-STATE NOT = SAVE-STATE                               ECS042
00482          PERFORM 3000-STATE-BREAK  THRU  3099-EXIT                ECS042
00483          GO TO 2240-ACCUMULATE.                                   ECS042
00484                                                                   ECS042
00485      IF SORT-ACCOUNT NOT = SAVE-ACCOUNT                           ECS042
00486          PERFORM 2900-ACCOUNT-BREAK  THRU  2999-EXIT              ECS042
00487          GO TO 2240-ACCUMULATE.                                   ECS042
00488                                                                   ECS042
00489      IF SORT-PROCESS-DATE NOT = SAVE-PROCESS-DATE                 ECS042
00490          PERFORM 2800-DATE-BREAK  THRU  2899-EXIT.                ECS042
00491  EJECT                                                            ECS042
00492  2240-ACCUMULATE.                                                 ECS042
00493      MOVE SORT-CERT              TO  SAVE-CERT.                   ECS042
00494      MOVE SORT-CLMNO             TO  SAVE-CLMNO.                  ECS042
00495      MOVE SORT-REST              TO  DETAIL-EXTRACT.              ECS042
00496                                                                   ECS042
00497      ADD DE-FUTRSV               TO  WA-FUTURE-RESV.              ECS042
00498      ADD DE-IBNR                 TO  WA-IBNR-RESV.                ECS042
00499      ADD DE-PAYCUR               TO  WA-PAY-TO-CURRENT.           ECS042
00500                                                                   ECS042
00501      MOVE SPACES                 TO  DETAIL-LINE.                 ECS042
00502      MOVE DE-CERT                TO  DET-CERTIFICATE.             ECS042
00503      MOVE DE-EFF                 TO  WS-DE-EFF-N.                    CL**5
00504      MOVE DE-EF-MO               TO  DET-EFF-MO.                  ECS042
00505      MOVE DE-EF-DA               TO  DET-EFF-DA.                  ECS042
00506      MOVE DE-EF-YR               TO  DET-EFF-YR.                  ECS042
00507      MOVE '-'                    TO  DET-EFF-MOD                  ECS042
00508                                      DET-EFF-DAD.                 ECS042
00509      MOVE DE-CLMNO               TO  DET-CLAIM-NUMBER.            ECS042
00510                                                                   ECS042
00511      IF DE-LIFE-RSV                                               ECS042
00512          MOVE LIFE-OVERRIDE-L6   TO  DET-TYPE                     ECS042
00513      ELSE                                                         ECS042
00514          MOVE AH-OVERRIDE-L6     TO  DET-TYPE.                    ECS042
00515                                                                   ECS042
00516      MOVE DE-RSV-INCUR           TO  WS-DE-RSV-INCUR-N.              CL**7
00517      MOVE DE-RSV-INCUR-MO        TO  DET-INC-MO.                  ECS042
00518      MOVE DE-RSV-INCUR-DA        TO  DET-INC-DA.                  ECS042
00519      MOVE DE-RSV-INCUR-YR        TO  DET-INC-YR.                  ECS042
00520      MOVE '-'                    TO  DET-INC-MOD                  ECS042
00521                                      DET-INC-DAD.                 ECS042
00522      MOVE DE-RSV-PROC-DT         TO  WS-DE-RSV-PROC-DT-N.            CL**8
00523      MOVE DE-RP-MO               TO  DET-PROCESS-MO.              ECS042
00524      MOVE DE-RP-DA               TO  DET-PROCESS-DA.              ECS042
00525      MOVE DE-RP-YR               TO  DET-PROCESS-YR.              ECS042
00526      MOVE '-'                    TO  DET-PROCESS-MOD              ECS042
00527                                      DET-PROCESS-DAD.             ECS042
00528      MOVE DE-FUTRSV              TO  DET-FUTURE-RESV.             ECS042
00529      MOVE DE-IBNR                TO  DET-IBNR-RESV.               ECS042
00530      MOVE DE-PAYCUR              TO  DET-PAY-TO-CURRENT.          ECS042
00531      MOVE SPACE-1                TO  X.                           ECS042
00532      MOVE DETAIL-LINE            TO  P-DATA.                      ECS042
00533                                                                   ECS042
00534      PERFORM 3400-WRITE-PRINT  THRU  3499-EXIT.                   ECS042
00535                                                                   ECS042
00536      IF LINE-CNT GREATER +54                                      ECS042
00537          PERFORM 3300-HEAD-RTN  THRU  3399-EXIT.                  ECS042
00538                                                                   ECS042
00539      GO TO 2200-RETURN-SORT-FILE.                                 ECS042
00540  EJECT                                                            ECS042
00541  2300-ZERO-ACCUM-ACCOUNT.                                         ECS042
00542      MOVE ZERO                   TO  X1.                          ECS042
00543                                                                   ECS042
00544  2310-ZERO-ACCOUNT-LOOP.                                          ECS042
00545      ADD +1                      TO  X1.                          ECS042
00546                                                                   ECS042
00547      IF X1 GREATER +36                                            ECS042
00548          GO TO 2399-EXIT.                                         ECS042
00549                                                                   ECS042
00550      MOVE ZERO-ACCUM             TO  ACCOUNT-ACCUM (X1).          ECS042
00551                                                                   ECS042
00552      GO TO 2310-ZERO-ACCOUNT-LOOP.                                ECS042
00553                                                                   ECS042
00554  2399-EXIT.                                                       ECS042
00555      EXIT.                                                        ECS042
00556                                                                   ECS042
00557  2400-ZERO-ACCUM-STATE.                                           ECS042
00558      MOVE ZERO                   TO  X1.                          ECS042
00559                                                                   ECS042
00560  2410-ZERO-STATE-LOOP.                                            ECS042
00561      ADD +1                      TO  X1.                          ECS042
00562                                                                   ECS042
00563      IF X1 GREATER +36                                            ECS042
00564          GO TO 2499-EXIT.                                         ECS042
00565                                                                   ECS042
00566      MOVE ZERO-ACCUM             TO  STATE-ACCUM (X1).            ECS042
00567                                                                   ECS042
00568      GO TO 2410-ZERO-STATE-LOOP.                                  ECS042
00569                                                                   ECS042
00570  2499-EXIT.                                                       ECS042
00571      EXIT.                                                        ECS042
00572  EJECT                                                            ECS042
00573  2500-ZERO-ACCUM-GROUP.                                           ECS042
00574      MOVE ZERO                   TO  X1.                          ECS042
00575                                                                   ECS042
00576  2510-ZERO-GROUP-LOOP.                                            ECS042
00577      ADD +1                      TO  X1.                          ECS042
00578                                                                   ECS042
00579      IF X1 GREATER +36                                            ECS042
00580          GO TO 2599-EXIT.                                         ECS042
00581                                                                   ECS042
00582      MOVE ZERO-ACCUM             TO  GROUPING-ACCUM (X1).         ECS042
00583                                                                   ECS042
00584      GO TO 2510-ZERO-GROUP-LOOP.                                  ECS042
00585                                                                   ECS042
00586  2599-EXIT.                                                       ECS042
00587      EXIT.                                                        ECS042
00588                                                                   ECS042
00589  2600-ZERO-ACCUM-CARRIER.                                         ECS042
00590      MOVE ZERO                   TO  X1.                          ECS042
00591                                                                   ECS042
00592  2610-ZERO-CARRIER-LOOP.                                          ECS042
00593      ADD +1                      TO  X1.                          ECS042
00594                                                                   ECS042
00595      IF X1 GREATER +36                                            ECS042
00596          GO TO 2699-EXIT.                                         ECS042
00597                                                                   ECS042
00598      MOVE ZERO-ACCUM             TO  CARRIER-ACCUM (X1).          ECS042
00599                                                                   ECS042
00600      GO TO 2610-ZERO-CARRIER-LOOP.                                ECS042
00601                                                                   ECS042
00602  2699-EXIT.                                                       ECS042
00603      EXIT.                                                        ECS042
00604                                                                   ECS042
00605  2700-ZERO-ACCUM-GRAND.                                           ECS042
00606      MOVE ZERO                   TO  X1.                          ECS042
00607                                                                   ECS042
00608  2710-ZERO-GRAND-LOOP.                                            ECS042
00609      ADD +1                      TO  X1.                          ECS042
00610                                                                   ECS042
00611      IF X1 GREATER +36                                            ECS042
00612          GO TO 2799-EXIT.                                         ECS042
00613                                                                   ECS042
00614      MOVE ZERO-ACCUM             TO  GRAND-ACCUM (X1).            ECS042
00615                                                                   ECS042
00616      GO TO 2710-ZERO-GRAND-LOOP.                                  ECS042
00617                                                                   ECS042
00618  2799-EXIT.                                                       ECS042
00619      EXIT.                                                        ECS042
00620  EJECT                                                            ECS042
00621  2800-DATE-BREAK.                                                 ECS042
00622      IF FIRST-TIME                                                ECS042
00623          GO TO 2820-SET-DATE-RANGE.                               ECS042
00624                                                                   ECS042
00625      MOVE +0                     TO  X1.                          ECS042
00626                                                                   ECS042
00627  2810-DATE-LOOP.                                                  ECS042
00628      ADD +1                      TO  X1.                          ECS042
00629                                                                   ECS042
00630      IF X1 GREATER 36                                             ECS042
00631          DISPLAY 'DATE TABLE ERROR OR EP-RUN-DATE ERROR'          ECS042
00632          DISPLAY 'DE-RSV-PROC-DT     = ' DE-RSV-PROC-DT           ECS042
00633          DISPLAY 'DE-DATE            = ' DE-DATE                  ECS042
00634          DISPLAY 'RUN-DT             = ' RUN-DT                   ECS042
00635          DISPLAY 'COMPARE-DATE-TABLE = ' COMPARE-DATE-TABLE       ECS042
00636          MOVE '0301'             TO  WS-RETURN-CODE               ECS042
00637          GO TO 9000-ABORT.                                        ECS042
00638                                                                   ECS042
00639      IF COMPARE9DT (X1) NOT = DE9DATE                             ECS042
00640          GO TO 2810-DATE-LOOP.                                    ECS042
00641                                                                   ECS042
00642      ADD WA-FUTURE-RESV          TO  AC-FUTURE (X1).              ECS042
00643      ADD WA-IBNR-RESV            TO  AC-IBNR (X1).                ECS042
00644      ADD WA-PAY-TO-CURRENT       TO  AC-PTC (X1).                 ECS042
00645                                                                   ECS042
00646      MOVE SPACES                 TO  DETAIL-LINE.                 ECS042
00647      MOVE 'TOTAL MONTH END RESERVE  -'                            ECS042
00648                                  TO  PDTL-TITLE.                  ECS042
00649      MOVE WA-FUTURE-RESV         TO  DET-FUTURE-RESV.             ECS042
00650      MOVE WA-IBNR-RESV           TO  DET-IBNR-RESV.               ECS042
00651      MOVE WA-PAY-TO-CURRENT      TO  DET-PAY-TO-CURRENT.          ECS042
00652      MOVE SPACE-3                TO  X.                           ECS042
00653      MOVE DETAIL-LINE            TO  P-DATA.                      ECS042
00654                                                                   ECS042
00655      PERFORM 3400-WRITE-PRINT  THRU  3499-EXIT.                   ECS042
00656                                                                   ECS042
00657      MOVE SPACE-2                TO  X.                           ECS042
00658      MOVE SPACES                 TO  P-DATA.                      ECS042
00659                                                                   ECS042
00660      PERFORM 3400-WRITE-PRINT  THRU  3499-EXIT.                   ECS042
00661                                                                   ECS042
00662  2820-SET-DATE-RANGE.                                             ECS042
00663      IF EOF-DEX = '1'                                             ECS042
00664          GO TO 2899-EXIT.                                         ECS042
00665                                                                   ECS042
00666      MOVE ZERO                   TO  WA-FUTURE-RESV               ECS042
00667                                      WA-IBNR-RESV                 ECS042
00668                                      WA-PAY-TO-CURRENT.           ECS042
00669      MOVE SORT-PROCESS-DATE      TO  SAVE-PROCESS-DATE
                                           WS-SORT-PROCESS-DATE
00670      MOVE SORT-PROC-CC           TO  DE-CC.                       ECS042
00671      MOVE SORT-PROC-YR           TO  DE-YR.                       ECS042
00672      MOVE SORT-PROC-MO           TO  DE-MO.                       ECS042
00673                                                                   ECS042
00674  2899-EXIT.                                                       ECS042
00675      EXIT.                                                        ECS042
00676  EJECT                                                            ECS042
00677  2900-ACCOUNT-BREAK.                                              ECS042
00678      PERFORM 2800-DATE-BREAK  THRU  2899-EXIT.                    ECS042
00679                                                                   ECS042
00680      IF FIRST-TIME                                                ECS042
00681          GO TO 2920-SET-ACCOUNT.                                  ECS042
00682                                                                   ECS042
00683 ***  MOVE 'Y'                    TO  FROM-ACCOUNT-BREAK-SW.       ECS042
00684 ***  MOVE ' ACCOUNT'             TO  HD-7A-TITLE.                 ECS042
00685                                                                   ECS042
00686 ***  PERFORM 3300-HEAD-RTN  THRU  3399-EXIT.                      ECS042
00687                                                                   ECS042
00688 ***  MOVE 'N'                    TO  FROM-ACCOUNT-BREAK-SW.       ECS042
00689      MOVE +0                     TO  X1.                          ECS042
00690                                                                   ECS042
00691  2910-ACCOUNT-LOOP.                                               ECS042
00692      ADD +1                      TO  X1.                          ECS042
00693                                                                   ECS042
00694      IF X1 GREATER +36                                            ECS042
00695          GO TO 2920-SET-ACCOUNT.                                  ECS042
00696                                                                   ECS042
00697      ADD AC-FUTURE (X1)          TO  ST-FUTURE (X1).              ECS042
00698      ADD AC-IBNR (X1)            TO  ST-IBNR (X1).                ECS042
00699      ADD AC-PTC (X1)             TO  ST-PTC (X1).                 ECS042
00700                                                                   ECS042
00701      MOVE SPACES                 TO  DETAIL-LINE.                 ECS042
00702 ***  MOVE COMP-MO (X1)           TO  TOT-PRO-MO.                  ECS042
00703                                                                   ECS042
00704 ***  PERFORM 3500-LOAD-ALPHA-MONTH  THRU  3599-EXIT.              ECS042
00705                                                                   ECS042
00706 ***  MOVE COMP-YR (X1)           TO  TOT-PRO-YR.                  ECS042
00707 ***  MOVE AC-FUTURE (X1)         TO  DET-FUTURE-RESV.             ECS042
00708 ***  MOVE AC-IBNR (X1)           TO  DET-IBNR-RESV.               ECS042
00709 ***  MOVE AC-PTC (X1)            TO  DET-PAY-TO-CURRENT.          ECS042
00710 ***  MOVE SPACE-1                TO  X.                           ECS042
00711 ***  MOVE DETAIL-LINE            TO  P-DATA.                      ECS042
00712                                                                   ECS042
00713 ***  PERFORM 3400-WRITE-PRINT  THRU  3499-EXIT.                   ECS042
00714                                                                   ECS042
00715      GO TO 2910-ACCOUNT-LOOP.                                     ECS042
00716  EJECT                                                            ECS042
00717  2920-SET-ACCOUNT.                                                ECS042
00718      IF EOF-DEX = '1'                                             ECS042
00719          GO TO 2999-EXIT.                                         ECS042
00720                                                                   ECS042
00721      PERFORM 2300-ZERO-ACCUM-ACCOUNT  THRU  2399-EXIT.            ECS042
00722                                                                   ECS042
00723      IF FROM-STATE-BREAK                                          ECS042
00724          MOVE SORT-ACCOUNT       TO  SAVE-ACCOUNT                 ECS042
00725                                      HOLD-ACCOUNT                 ECS042
00726          MOVE SORT-RSV-ACC-NAME  TO  HOLD-ACCT-NAME               ECS042
00727      ELSE                                                         ECS042
00728          MOVE SORT-ACCOUNT       TO  SAVE-ACCOUNT                 ECS042
00729                                      HD-ACCOUNT                   ECS042
00730          MOVE SORT-RSV-ACC-NAME  TO  HD-ACCT-NAME                 ECS042
00731          PERFORM 3300-HEAD-RTN  THRU  3399-EXIT.                  ECS042
00732                                                                   ECS042
00733  2999-EXIT.                                                       ECS042
00734      EXIT.                                                        ECS042
00735  EJECT                                                            ECS042
00736  3000-STATE-BREAK.                                                ECS042
00737      MOVE 'Y'                    TO  FROM-STATE-BREAK-SW.         ECS042
00738                                                                   ECS042
00739      PERFORM 2900-ACCOUNT-BREAK  THRU  2999-EXIT.                 ECS042
00740                                                                   ECS042
00741      MOVE 'N'                    TO  FROM-STATE-BREAK-SW.         ECS042
00742                                                                   ECS042
00743      IF FIRST-TIME                                                ECS042
00744          MOVE HOLD-ACCOUNT       TO  HD-ACCOUNT                   ECS042
00745          MOVE HOLD-ACCT-NAME     TO  HD-ACCT-NAME                 ECS042
00746          GO TO 3020-SET-STATE.                                    ECS042
00747                                                                   ECS042
00748      MOVE 'Y'                    TO  FROM-STATE-BREAK-SW.         ECS042
00749      MOVE '   STATE'             TO  HD-7A-TITLE.                 ECS042
00750      MOVE SPACES                 TO  HD-ACCOUNT  HD-ACCT-NAME.    ECS042
00751                                                                   ECS042
00752      PERFORM 3300-HEAD-RTN  THRU  3399-EXIT.                      ECS042
00753                                                                   ECS042
00754      MOVE 'N'                    TO  FROM-STATE-BREAK-SW.         ECS042
00755      MOVE HOLD-ACCOUNT           TO  HD-ACCOUNT.                  ECS042
00756      MOVE HOLD-ACCT-NAME         TO  HD-ACCT-NAME.                ECS042
00757      MOVE +0                     TO  X1.                          ECS042
00758                                                                   ECS042
00759  3010-STATE-LOOP.                                                 ECS042
00760      ADD +1                      TO  X1.                          ECS042
00761                                                                   ECS042
00762      IF X1 GREATER +36                                            ECS042
00763          GO TO 3020-SET-STATE.                                    ECS042
00764                                                                   ECS042
00765      ADD ST-FUTURE (X1)          TO  GP-FUTURE (X1).              ECS042
00766      ADD ST-IBNR (X1)            TO  GP-IBNR (X1).                ECS042
00767      ADD ST-PTC (X1)             TO  GP-PTC (X1).                 ECS042
00768                                                                   ECS042
00769      MOVE SPACES                 TO  DETAIL-LINE.                 ECS042
00770      MOVE COMP-MO (X1)           TO  TOT-PRO-MO.                  ECS042
00771                                                                   ECS042
00772      PERFORM 3500-LOAD-ALPHA-MONTH  THRU  3599-EXIT.              ECS042
00773                                                                   ECS042
00774      MOVE COMP-YR (X1)           TO  TOT-PRO-YR.                  ECS042
00775      MOVE ST-FUTURE (X1)         TO  DET-FUTURE-RESV.             ECS042
00776      MOVE ST-IBNR (X1)           TO  DET-IBNR-RESV.               ECS042
00777      MOVE ST-PTC (X1)            TO  DET-PAY-TO-CURRENT.          ECS042
00778      MOVE SPACE-1                TO  X.                           ECS042
00779      MOVE DETAIL-LINE            TO  P-DATA.                      ECS042
00780                                                                   ECS042
00781      PERFORM 3400-WRITE-PRINT  THRU  3499-EXIT.                   ECS042
00782                                                                   ECS042
00783      GO TO 3010-STATE-LOOP.                                       ECS042
00784  EJECT                                                            ECS042
00785  3020-SET-STATE.                                                  ECS042
00786      IF EOF-DEX = '1'                                             ECS042
00787          GO TO 3099-EXIT.                                         ECS042
00788                                                                   ECS042
00789      PERFORM 2400-ZERO-ACCUM-STATE  THRU  2499-EXIT.              ECS042
00790                                                                   ECS042
00791      IF FROM-GROUP-BREAK                                          ECS042
00792          MOVE SORT-STATE         TO  SAVE-STATE                   ECS042
00793                                      HOLD-STATE                   ECS042
00794      ELSE                                                         ECS042
00795          MOVE SORT-STATE         TO  SAVE-STATE                   ECS042
00796                                      HD-STATE                     ECS042
00797          PERFORM 3300-HEAD-RTN  THRU  3399-EXIT.                  ECS042
00798                                                                   ECS042
00799  3099-EXIT.                                                       ECS042
00800      EXIT.                                                        ECS042
00801  EJECT                                                            ECS042
00802  3100-GROUPING-BREAK.                                             ECS042
00803      MOVE 'Y'                    TO  FROM-GROUP-BREAK-SW.         ECS042
00804                                                                   ECS042
00805      PERFORM 3000-STATE-BREAK  THRU  3099-EXIT.                   ECS042
00806                                                                   ECS042
00807      MOVE 'N'                    TO  FROM-GROUP-BREAK-SW.         ECS042
00808                                                                   ECS042
00809      IF FIRST-TIME                                                ECS042
00810          MOVE HOLD-STATE         TO  HD-STATE                     ECS042
00811          GO TO 3120-SET-GROUPING.                                 ECS042
00812                                                                   ECS042
00813      MOVE 'Y'                    TO  FROM-GROUP-BREAK-SW.         ECS042
00814      MOVE 'GROUPING'             TO  HD-7A-TITLE.                 ECS042
00815      MOVE SPACES                 TO  HD-STATE                     ECS042
00816                                      HD-ACCOUNT                   ECS042
00817                                      HD-ACCT-NAME.                ECS042
00818                                                                   ECS042
00819      PERFORM 3300-HEAD-RTN  THRU  3399-EXIT.                      ECS042
00820                                                                   ECS042
00821      MOVE 'N'                    TO  FROM-GROUP-BREAK-SW.         ECS042
00822      MOVE HOLD-STATE             TO  HD-STATE.                    ECS042
00823      MOVE HOLD-ACCOUNT           TO  HD-ACCOUNT.                  ECS042
00824      MOVE HOLD-ACCT-NAME         TO  HD-ACCT-NAME.                ECS042
00825      MOVE +0                     TO  X1.                          ECS042
00826                                                                   ECS042
00827  3110-GROUPING-LOOP.                                              ECS042
00828      ADD +1                      TO  X1.                          ECS042
00829                                                                   ECS042
00830      IF X1 GREATER +36                                            ECS042
00831          GO TO 3120-SET-GROUPING.                                 ECS042
00832                                                                   ECS042
00833      ADD GP-FUTURE (X1)          TO  CA-FUTURE (X1).              ECS042
00834      ADD GP-IBNR (X1)            TO  CA-IBNR (X1).                ECS042
00835      ADD GP-PTC (X1)             TO  CA-PTC (X1).                 ECS042
00836                                                                   ECS042
00837      MOVE SPACES                 TO  DETAIL-LINE.                 ECS042
00838      MOVE COMP-MO (X1)           TO  TOT-PRO-MO.                  ECS042
00839                                                                   ECS042
00840      PERFORM 3500-LOAD-ALPHA-MONTH  THRU  3599-EXIT.              ECS042
00841                                                                   ECS042
00842      MOVE COMP-YR (X1)           TO  TOT-PRO-YR.                  ECS042
00843      MOVE GP-FUTURE (X1)         TO  DET-FUTURE-RESV.             ECS042
00844      MOVE GP-IBNR (X1)           TO  DET-IBNR-RESV.               ECS042
00845      MOVE GP-PTC (X1)            TO  DET-PAY-TO-CURRENT.          ECS042
00846      MOVE SPACE-1                TO  X.                           ECS042
00847      MOVE DETAIL-LINE            TO  P-DATA.                      ECS042
00848                                                                   ECS042
00849      PERFORM 3400-WRITE-PRINT  THRU  3499-EXIT.                   ECS042
00850                                                                   ECS042
00851      GO TO 3110-GROUPING-LOOP.                                    ECS042
00852  EJECT                                                            ECS042
00853  3120-SET-GROUPING.                                               ECS042
00854      IF EOF-DEX = '1'                                             ECS042
00855          GO TO 3199-EXIT.                                         ECS042
00856                                                                   ECS042
00857      PERFORM 2500-ZERO-ACCUM-GROUP  THRU  2599-EXIT.              ECS042
00858                                                                   ECS042
00859      IF FROM-CARR-BREAK                                           ECS042
00860          MOVE SORT-GROUPING      TO  SAVE-GROUPING                ECS042
00861                                      HOLD-GROUPING                ECS042
00862      ELSE                                                         ECS042
00863          MOVE SORT-GROUPING      TO  SAVE-GROUPING                ECS042
00864                                      HD-GROUPING                  ECS042
00865          PERFORM 3300-HEAD-RTN  THRU  3399-EXIT.                  ECS042
00866                                                                   ECS042
00867  3199-EXIT.                                                       ECS042
00868      EXIT.                                                        ECS042
00869  EJECT                                                            ECS042
00870  3200-CARRIER-BREAK.                                              ECS042
00871      MOVE 'Y'                    TO  FROM-CARR-BREAK-SW.          ECS042
00872                                                                   ECS042
00873      PERFORM 3100-GROUPING-BREAK  THRU  3199-EXIT.                ECS042
00874                                                                   ECS042
00875      MOVE 'N'                    TO  FROM-CARR-BREAK-SW.          ECS042
00876                                                                   ECS042
00877      IF FIRST-TIME                                                ECS042
00878          MOVE 'N'                TO  FIRST-TIME-SWITCH            ECS042
00879          MOVE HOLD-GROUPING      TO  HD-GROUPING                  ECS042
00880          GO TO 3220-SET-CARRIER.                                  ECS042
00881                                                                   ECS042
00882      MOVE 'Y'                    TO  FROM-CARR-BREAK-SW.          ECS042
00883      MOVE ' CARRIER'             TO  HD-7A-TITLE.                 ECS042
00884      MOVE SPACES                 TO  HD-GROUPING                  ECS042
00885                                      HD-STATE                     ECS042
00886                                      HD-ACCOUNT                   ECS042
00887                                      HD-ACCT-NAME.                ECS042
00888                                                                   ECS042
00889      PERFORM 3300-HEAD-RTN  THRU  3399-EXIT.                      ECS042
00890                                                                   ECS042
00891      MOVE 'N'                    TO  FROM-CARR-BREAK-SW.          ECS042
00892      MOVE HOLD-GROUPING          TO  HD-GROUPING.                 ECS042
00893      MOVE HOLD-STATE             TO  HD-STATE.                    ECS042
00894      MOVE HOLD-ACCOUNT           TO  HD-ACCOUNT.                  ECS042
00895      MOVE HOLD-ACCT-NAME         TO  HD-ACCT-NAME.                ECS042
00896      MOVE +0                     TO  X1.                          ECS042
00897                                                                   ECS042
00898  3210-CARRIER-LOOP.                                               ECS042
00899      ADD +1                      TO  X1.                          ECS042
00900                                                                   ECS042
00901      IF X1 GREATER +36                                            ECS042
00902          GO TO 3220-SET-CARRIER.                                  ECS042
00903                                                                   ECS042
00904      ADD CA-FUTURE (X1)          TO  GT-FUTURE (X1).              ECS042
00905      ADD CA-IBNR (X1)            TO  GT-IBNR (X1).                ECS042
00906      ADD CA-PTC (X1)             TO  GT-PTC (X1).                 ECS042
00907                                                                   ECS042
00908      MOVE SPACES                 TO  DETAIL-LINE.                 ECS042
00909      MOVE COMP-MO (X1)           TO  TOT-PRO-MO.                  ECS042
00910                                                                   ECS042
00911      PERFORM 3500-LOAD-ALPHA-MONTH  THRU  3599-EXIT.              ECS042
00912                                                                   ECS042
00913      MOVE COMP-YR (X1)           TO  TOT-PRO-YR.                  ECS042
00914      MOVE CA-FUTURE (X1)         TO  DET-FUTURE-RESV.             ECS042
00915      MOVE CA-IBNR (X1)           TO  DET-IBNR-RESV.               ECS042
00916      MOVE CA-PTC (X1)            TO  DET-PAY-TO-CURRENT.          ECS042
00917      MOVE SPACE-1                TO  X.                           ECS042
00918      MOVE DETAIL-LINE            TO  P-DATA.                      ECS042
00919                                                                   ECS042
00920      PERFORM 3400-WRITE-PRINT  THRU  3499-EXIT.                   ECS042
00921                                                                   ECS042
00922      GO TO 3210-CARRIER-LOOP.                                     ECS042
00923  EJECT                                                            ECS042
00924  3220-SET-CARRIER.                                                ECS042
00925      IF EOF-DEX = '1'                                             ECS042
00926          GO TO 3299-EXIT.                                         ECS042
00927                                                                   ECS042
00928      PERFORM 2600-ZERO-ACCUM-CARRIER  THRU  2699-EXIT.            ECS042
00929                                                                   ECS042
00930      MOVE SORT-CARRIER           TO  SAVE-CARRIER                 ECS042
00931                                      HD-CARRIER.                  ECS042
00932                                                                   ECS042
00933      PERFORM 3300-HEAD-RTN  THRU  3399-EXIT.                      ECS042
00934                                                                   ECS042
00935  3299-EXIT.                                                       ECS042
00936      EXIT.                                                        ECS042
00937  EJECT                                                            ECS042
00938  3300-HEAD-RTN.                                                   ECS042
00939      ADD +1                      TO  PAGE-CNT.                    ECS042
00940                                                                   ECS042
00941      MOVE PAGE-CNT               TO  HD-PAGE.                     ECS042
00942      MOVE SPACE-NP               TO  X.                           ECS042
00943      MOVE HD-1                   TO  P-DATA.                      ECS042
00944                                                                   ECS042
00945      PERFORM 3400-WRITE-PRINT  THRU  3499-EXIT.                   ECS042
00946                                                                   ECS042
00947      MOVE SPACE-1                TO  X.                           ECS042
00948      MOVE HD-2                   TO  P-DATA.                      ECS042
00949                                                                   ECS042
00950      PERFORM 3400-WRITE-PRINT  THRU  3499-EXIT.                   ECS042
00951                                                                   ECS042
00952      MOVE SPACE-1                TO  X.                           ECS042
00953      MOVE HD-3                   TO  P-DATA.                      ECS042
00954                                                                   ECS042
00955      PERFORM 3400-WRITE-PRINT  THRU  3499-EXIT.                   ECS042
00956                                                                   ECS042
00957      MOVE SPACE-1                TO  X.                           ECS042
00958      MOVE HD-4                   TO  P-DATA.                      ECS042
00959                                                                   ECS042
00960      PERFORM 3400-WRITE-PRINT  THRU  3499-EXIT.                   ECS042
00961                                                                   ECS042
00962      MOVE SPACE-1                TO  X.                           ECS042
00963      MOVE HD-5                   TO  P-DATA.                      ECS042
00964                                                                   ECS042
00965      PERFORM 3400-WRITE-PRINT  THRU  3499-EXIT.                   ECS042
00966                                                                   ECS042
00967      IF FROM-ACCOUNT-BREAK  OR                                    ECS042
00968         FROM-STATE-BREAK    OR                                    ECS042
00969         FROM-GROUP-BREAK    OR                                    ECS042
00970         FROM-CARR-BREAK     OR                                    ECS042
00971         FROM-GRAND-BREAK                                          ECS042
00972           MOVE SPACE-3            TO  X                           ECS042
00973           MOVE HD-6A              TO  P-DATA                      ECS042
00974           PERFORM 3400-WRITE-PRINT  THRU  3499-EXIT               ECS042
00975           MOVE SPACE-1            TO  X                           ECS042
00976           MOVE HD-7A              TO  P-DATA                      ECS042
00977      ELSE                                                         ECS042
00978           MOVE SPACE-3            TO  X                           ECS042
00979           MOVE HD-6               TO  P-DATA                      ECS042
00980           PERFORM 3400-WRITE-PRINT  THRU  3499-EXIT               ECS042
00981           MOVE SPACE-1            TO  X                           ECS042
00982           MOVE HD-7               TO  P-DATA.                     ECS042
00983                                                                   ECS042
00984      PERFORM 3400-WRITE-PRINT  THRU  3499-EXIT.                   ECS042
00985                                                                   ECS042
00986      MOVE SPACE-1                TO  X.                           ECS042
00987      MOVE SPACES                 TO  P-DATA.                      ECS042
00988                                                                   ECS042
00989      PERFORM 3400-WRITE-PRINT  THRU  3499-EXIT.                   ECS042
00990                                                                   ECS042
00991  3399-EXIT.                                                       ECS042
00992      EXIT.                                                        ECS042
00993  EJECT                                                            ECS042
00994  3400-WRITE-PRINT.                                                ECS042
00995      IF X = SPACE-NP                                              ECS042
00996          MOVE +1                 TO  LINE-CNT                     ECS042
00997      ELSE                                                         ECS042
00998          IF X = SPACE-1                                           ECS042
00999              ADD +1              TO  LINE-CNT                     ECS042
01000          ELSE                                                     ECS042
01001              IF X = SPACE-2                                       ECS042
01002                  ADD +2          TO  LINE-CNT                     ECS042
01003              ELSE                                                 ECS042
01004                  ADD +3          TO  LINE-CNT.                    ECS042
01005                                                                   ECS042
01006                              COPY ELCPRT2.                        ECS042
01007                                                                   ECS042
01008  3499-EXIT.                                                       ECS042
01009      EXIT.                                                        ECS042
01010  EJECT                                                            ECS042
01011  3500-LOAD-ALPHA-MONTH.                                           ECS042
01012      IF TOT-PRO-MO = '01 '                                        ECS042
01013          MOVE 'JAN'              TO  TOT-PRO-MO.                  ECS042
01014                                                                   ECS042
01015      IF TOT-PRO-MO = '02 '                                        ECS042
01016          MOVE 'FEB'              TO  TOT-PRO-MO.                  ECS042
01017                                                                   ECS042
01018      IF TOT-PRO-MO = '03 '                                        ECS042
01019          MOVE 'MAR'              TO  TOT-PRO-MO.                  ECS042
01020                                                                   ECS042
01021      IF TOT-PRO-MO = '04 '                                        ECS042
01022          MOVE 'APR'              TO  TOT-PRO-MO.                  ECS042
01023                                                                   ECS042
01024      IF TOT-PRO-MO = '05 '                                        ECS042
01025          MOVE 'MAY'              TO  TOT-PRO-MO.                  ECS042
01026                                                                   ECS042
01027      IF TOT-PRO-MO = '06 '                                        ECS042
01028          MOVE 'JUN'              TO  TOT-PRO-MO.                  ECS042
01029                                                                   ECS042
01030      IF TOT-PRO-MO = '07 '                                        ECS042
01031          MOVE 'JUL'              TO  TOT-PRO-MO.                  ECS042
01032                                                                   ECS042
01033      IF TOT-PRO-MO = '08 '                                        ECS042
01034          MOVE 'AUG'              TO  TOT-PRO-MO.                  ECS042
01035                                                                   ECS042
01036      IF TOT-PRO-MO = '09 '                                        ECS042
01037          MOVE 'SEP'              TO  TOT-PRO-MO.                  ECS042
01038                                                                   ECS042
01039      IF TOT-PRO-MO = '10 '                                        ECS042
01040          MOVE 'OCT'              TO  TOT-PRO-MO.                  ECS042
01041                                                                   ECS042
01042      IF TOT-PRO-MO = '11 '                                        ECS042
01043          MOVE 'NOV'              TO  TOT-PRO-MO.                  ECS042
01044                                                                   ECS042
01045      IF TOT-PRO-MO = '12 '                                        ECS042
01046          MOVE 'DEC'              TO  TOT-PRO-MO.                  ECS042
01047                                                                   ECS042
01048  3599-EXIT.                                                       ECS042
01049      EXIT.                                                        ECS042
01050  EJECT                                                            ECS042
01051  3600-PRINT-GRAND-TOTALS.                                         ECS042
01052      PERFORM 3200-CARRIER-BREAK THRU  3299-EXIT                   ECS042
01053                                                                   ECS042
01054      MOVE 'Y'                    TO  FROM-GRAND-BREAK-SW.         ECS042
01055      MOVE SPACES                 TO  HD-CARRIER                   ECS042
01056                                      HD-GROUPING                  ECS042
01057                                      HD-STATE                     ECS042
01058                                      HD-ACCOUNT                   ECS042
01059                                      HD-ACCT-NAME.                ECS042
01060      MOVE '   GRAND'             TO  HD-7A-TITLE.                 ECS042
01061                                                                   ECS042
01062      PERFORM 3300-HEAD-RTN  THRU  3399-EXIT.                      ECS042
01063                                                                   ECS042
01064      MOVE 'N'                    TO  FROM-GRAND-BREAK-SW.         ECS042
01065      MOVE +1                     TO  X1.                          ECS042
01066                                                                   ECS042
01067  3610-GRAND-LOOP.                                                 ECS042
01068      ADD +1                      TO  X1.                          ECS042
01069                                                                   ECS042
01070      IF X1 GREATER +36                                            ECS042
01071          GO TO 3700-END-OF-JOB.                                   ECS042
01072                                                                   ECS042
01073      MOVE SPACES                 TO  DETAIL-LINE.                 ECS042
01074      MOVE COMP-MO (X1)           TO  TOT-PRO-MO.                  ECS042
01075                                                                   ECS042
01076      PERFORM 3500-LOAD-ALPHA-MONTH  THRU  3599-EXIT.              ECS042
01077                                                                   ECS042
01078      MOVE COMP-YR (X1)           TO  TOT-PRO-YR.                  ECS042
01079      MOVE GT-FUTURE (X1)         TO  DET-FUTURE-RESV.             ECS042
01080      MOVE GT-IBNR (X1)           TO  DET-IBNR-RESV.               ECS042
01081      MOVE GT-PTC (X1)            TO  DET-PAY-TO-CURRENT.          ECS042
01082      MOVE SPACE-1                TO  X.                           ECS042
01083      MOVE DETAIL-LINE            TO  P-DATA.                      ECS042
01084                                                                   ECS042
01085      PERFORM 3400-WRITE-PRINT  THRU  3499-EXIT.                   ECS042
01086                                                                   ECS042
01087      GO TO 3610-GRAND-LOOP.                                       ECS042
01088  EJECT                                                            ECS042
01089  3700-END-OF-JOB.                                                 ECS042
01090                              COPY ELCPRTC.                        ECS042
01091                                                                   ECS042
01092      CLOSE PRNTR.                                                 ECS042
01093                                                                   ECS042
01094  3799-EXIT.                                                       ECS042
01095      EXIT.                                                        ECS042
01096                                                                   ECS042
01097  COPY ELCDCS.                                                     ECS042
01098                                                                   ECS042
01099  9000-ABORT SECTION.                                              ECS042
01100                                                                   ECS042
01101  ABEND-PGM.                                                       ECS042
01102                                  COPY ELCABEND.                   ECS042
