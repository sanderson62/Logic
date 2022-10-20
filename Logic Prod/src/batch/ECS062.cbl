00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   ECS062
00003  PROGRAM-ID.                ECS062.                                  LV002
00004 *              PROGRAM CONVERTED BY                               ECS062
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS062
00006 *              CONVERSION DATE 02/23/95 07:32:24.                 ECS062
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS062
00008 *                           VMOD=2.024.                           ECS062
00009                                                                   ECS062
00010 *AUTHOR.        LOGIC, INC.                                       ECS062
00011 *               DALLAS, TEXAS.                                    ECS062
00012                                                                   ECS062
00013 *DATE-COMPILED.                                                   ECS062
00014                                                                   ECS062
00015 *SECURITY.   *****************************************************ECS062
00016 *            *                                                   *ECS062
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS062
00018 *            *                                                   *ECS062
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS062
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS062
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS062
00022 *            *                                                   *ECS062
00023 *            *****************************************************ECS062
00024                                                                   ECS062
00025 *REMARKS.                                                         ECS062
00026 *        THIS PROGRAM PRINTS COMPENSATION STATEMENTS.             ECS062
00027 *                                                                 ECS062
00028 *        PROGRAM SWITCHES                                         ECS062
00029 *                                                                 ECS062
00030 *        1 - PRINT STATEMENTS WITH ACTIVITY OR BALANCE FORWARD    ECS062
00031 *        2 - PRINT STATEMENTS WITH ACTIVITY ONLY                  ECS062
00032 *        3 - PRINT NO COMPENSATION STATEMENTS                     ECS062
00033  EJECT                                                            ECS062
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
080612* 080612  CR2012042700005  PEMA  ADD OVER 120 DAYS FOR AHL
      ******************************************************************
00034  ENVIRONMENT DIVISION.                                            ECS062
00035  CONFIGURATION SECTION.                                           ECS062
00036  SPECIAL-NAMES.                                                   ECS062
00037      C02 IS LCP-CH2                                               ECS062
00038      C03 IS LCP-CH3                                               ECS062
00039      C04 IS LCP-CH4                                               ECS062
00040      C05 IS LCP-CH5                                               ECS062
00041      C06 IS LCP-CH6                                               ECS062
00042      C07 IS LCP-CH7                                               ECS062
00043      C08 IS LCP-CH8                                               ECS062
00044      C09 IS LCP-CH9                                               ECS062
00045      C10 IS LCP-CH10                                              ECS062
00046      C11 IS LCP-CH11                                              ECS062
00047      C12 IS LCP-CH12                                              ECS062
00048      S01 IS LCP-P01                                               ECS062
00049      S02 IS LCP-P02.                                              ECS062
00050  INPUT-OUTPUT SECTION.                                            ECS062
00051  FILE-CONTROL.                                                    ECS062
00052                                                                   ECS062
00053      SELECT ERACCT         ASSIGN TO SYS011-FBA1-ERACCT           ECS062
00054                                ORGANIZATION  INDEXED              ECS062
00055                                ACCESS        DYNAMIC              ECS062
00056                                RECORD KEY    AM-CONTROL-PRIMARY   ECS062
00057                                FILE STATUS   ERACCT-FILE-STATUS.  ECS062
00058                                                                   ECS062
00059      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS062
00060      SELECT BILLING-DATA-FILE                                     ECS062
00061                              ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS062
00062      SELECT COMM-MSTR-IN     ASSIGN TO SYS015-UT-FBA1-S-SYS015.   ECS062
00063      SELECT COMM-MSTR-OUT    ASSIGN TO SYS016-UT-FBA1-S-SYS016.   ECS062
00064      SELECT COMM-TRAN-IN     ASSIGN TO SYS017-UT-FBA1-S-SYS017.   ECS062
00065      SELECT SUMM-TRAN-OUT    ASSIGN TO SYS018-UT-FBA1-S-SYS018.   ECS062
00066      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS062
00067      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS062
00068  EJECT                                                            ECS062
00069  DATA DIVISION.                                                   ECS062
00070  FILE SECTION.                                                    ECS062
00071                                                                   ECS062
00072  FD  ERACCT.                                                      ECS062
00073      COPY ERCACCT.                                                ECS062
00074  EJECT                                                            ECS062
00075  FD  PRNTR                                                        ECS062
00076                              COPY ELCPRTFD.                       ECS062
00077  EJECT                                                            ECS062
00078  FD  BILLING-DATA-FILE                                            ECS062
00079      BLOCK CONTAINS 0 RECORDS
00080      RECORDING MODE F.                                            ECS062
00081                                                                   ECS062
00082  01  BILLING-DATA-RECORD     PIC  X(274).                         ECS062
00083  EJECT                                                            ECS062
00084  FD  COMM-MSTR-IN                                                 ECS062
00085                              COPY ECSCOIFD.                       ECS062
00086  EJECT                                                            ECS062
00087  FD  COMM-MSTR-OUT                                                ECS062
00088                              COPY ECSCOOFD.                       ECS062
00089  EJECT                                                            ECS062
00090  FD  COMM-TRAN-IN                                                 ECS062
00091                              COPY ECSCOMFD.                       ECS062
00092                                                                   ECS062
00093                              COPY ECSCOM01.                       ECS062
00094  EJECT                                                            ECS062
00095  FD  SUMM-TRAN-OUT                                                ECS062
00096      BLOCK CONTAINS 0 RECORDS
00097      RECORDING MODE F.                                            ECS062
00098                                                                   ECS062
00099  01  CCM-WK.                                                      ECS062
00100      12  COMM-TRAN-WORK-REC.                                      ECS062
00101          16  CCW-ID              PIC  XX.                         ECS062
00102          16  CCW-CARR-GROUP      PIC  X(7).                       ECS062
00103          16  CCW-RESP-NO         PIC  X(10).                      ECS062
00104          16  CCW-ACCOUNT         PIC  X(10).                      ECS062
00105          16  CCW-AM-NO           PIC  X(10).                      ECS062
00106          16  CCW-TYPE            PIC  X.                          ECS062
00107              88  CCW-ACCTG                   VALUE '5'.           ECS062
00108              88  CCW-OVERWT                  VALUE '6'.           ECS062
00109              88  CCW-SUMMARY                 VALUE '7'.           ECS062
00110          16  CCW-BAL-CTL         PIC  X.                          ECS062
00111          16  CCW-NAME            PIC  X(30).                      ECS062
00112          16  CCW-PREM            PIC S9(7)V99           COMP-3.   ECS062
00113          16  CCW-COMM            PIC S9(7)V99           COMP-3.   ECS062
00114          16  CCW-PMTS            PIC S9(7)V99           COMP-3.   ECS062
00115          16  CCW-BEG-BAL         PIC S9(7)V99           COMP-3.   ECS062
00116          16  CCW-END-BAL         PIC S9(7)V99           COMP-3.   ECS062
00117          16  CCW-OV-L-PREM       PIC S9(7)V99           COMP-3.   ECS062
00118          16  CCW-OV-A-PREM       PIC S9(7)V99           COMP-3.   ECS062
00119          16  CCW-OV-LIFE         PIC S9(7)V99           COMP-3.   ECS062
00120          16  CCW-OV-AH           PIC S9(7)V99           COMP-3.   ECS062
00121          16  CCW-OV-B-L-PREM     PIC S9(7)V99           COMP-3.   ECS062
00122          16  CCW-OV-B-A-PREM     PIC S9(7)V99           COMP-3.   ECS062
00123          16  CCW-OV-B-LIFE       PIC S9(7)V99           COMP-3.   ECS062
00124          16  CCW-OV-B-AH         PIC S9(7)V99           COMP-3.   ECS062
00125  EJECT                                                            ECS062
00126  FD  DISK-DATE                                                    ECS062
00127                              COPY ELCDTEFD.                       ECS062
00128  EJECT                                                            ECS062
00129  FD  FICH                                                         ECS062
00130                              COPY ELCFCHFD.                       ECS062
00131  EJECT                                                            ECS062
00132  WORKING-STORAGE SECTION.                                         ECS062
00133  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS062
00134  77  LCP-ASA                       PIC X.                         ECS062
00135  77  FILLER PIC  X(32) VALUE '********************************'.  ECS062
00136  77  FILLER PIC  X(32) VALUE '*            ECS062            *'.  ECS062
00137  77  FILLER PIC  X(32) VALUE '*********** VMOD=2.024 *********'.  ECS062
00138                                                                   ECS062
00139  77  STMT-SW                 PIC S9          VALUE +0   COMP-3.   ECS062
00140  77  SUMM-SW                 PIC S9          VALUE +0   COMP-3.   ECS062
00141  77  PGM-SUB                 PIC S9(3)       VALUE +62  COMP-3.   ECS062
00142  77  LJ-NDX1                 PIC S9(3)       VALUE +0   COMP-3.   ECS062
00143  77  LJ-NDX2                 PIC S9(3)       VALUE +0   COMP-3.   ECS062
00144  77  SPACE-NP                PIC  X          VALUE '1'.           ECS062
00145  77  SPACE-1                 PIC  X          VALUE ' '.           ECS062
00146  77  SPACE-2                 PIC  X          VALUE '0'.           ECS062
00147  77  SPACE-3                 PIC  X          VALUE '-'.           ECS062
00148  77  X                       PIC  X          VALUE '1'.           ECS062
00149  77  OB-SWITCH               PIC  X          VALUE 'N'.           ECS062
00150      88  OB-ON                               VALUE 'Y'.           ECS062
00151  77  AH-ONLY-SWITCH          PIC  X          VALUE 'N'.           ECS062
00152      88  AH-ONLY                             VALUE 'Y'.           ECS062
00153  77  ACCOUNT-TYPE-SWITCH     PIC  X          VALUE 'N'.           ECS062
00154      88  FROM-ACCOUNT-PART                   VALUE 'Y'.           ECS062
00155  77  FIRST-TIME-SW           PIC  X          VALUE 'Y'.           ECS062
00156      88  FIRST-TIME                          VALUE 'Y'.           ECS062
00157  77  PREV-ISSUE-CANCEL-SW    PIC  X          VALUE 'N'.           ECS062
00158      88  PREV-ISSUE-CANCEL                   VALUE 'Y'.           ECS062
00159                                                                   ECS062
00160      COPY ELCDATE.                                                   CL**2
00161                                                                   ECS062
00162  01  WS-DATES.                                                    ECS062
00163      16  SAVE-CP-EFF         PIC 9(11)   COMP-3.                  ECS062
00164      16  WS-EFF-DATE         PIC 9(11)   COMP-3.                  ECS062
00165      16  WS-EFF-DATE-R.                                           ECS062
00166          20  FILLER               PIC 999.                        ECS062
00167          20  WS-EFF-DATE-CCYY.                                    ECS062
00168              24  WS-EFF-CC   PIC 99.                              ECS062
00169              24  WS-EFF-YR   PIC 99.                              ECS062
00170          20  WS-EFF-MO       PIC 99.                              ECS062
00171          20  WS-EFF-DA       PIC 99.                              ECS062
00172      16  WS-EXP-DATE         PIC 9(11)     COMP-3.                ECS062
00173      16  WS-EXP-DATE-R.                                           ECS062
00174          20  FILLER          PIC 999.                             ECS062
00175          20  WS-EXP-DATE-CCYY.                                    ECS062
00176              24  WS-EXP-CC   PIC 99.                              ECS062
00177              24  WS-EXP-YR   PIC 99.                              ECS062
00178          20  WS-EXP-DATE-MO  PIC 99.                              ECS062
00179          20  WS-EXP-DATE-DA  PIC 99.                              ECS062
00180      16  WS-920101 COMP-3.                                        ECS062
00181          20  FILLER          PIC 999 VALUE 0.                     ECS062
00182          20  WS-920101-DATE  PIC 9(08) VALUE 19920101.            ECS062
00183                                                                   ECS062
00184                                                                   ECS062
00185  01  PRT-TOTAL-SWITCH        PIC  X(3)       VALUE 'NNN'.         ECS062
00186      88  PRT-TOT-1                           VALUE 'YNN'.         ECS062
00187      88  PRT-TOT-2                           VALUE 'NYN'.         ECS062
00188      88  PRT-TOT-3                           VALUE 'NNY'.         ECS062
00189                                                                   ECS062
00190  01  FILLER  REDEFINES  PRT-TOTAL-SWITCH.                         ECS062
00191      16  PRT-TOT-1-SW        PIC  X.                              ECS062
00192      16  PRT-TOT-2-SW        PIC  X.                              ECS062
00193      16  PRT-TOT-3-SW        PIC  X.                              ECS062
00194                                                                   ECS062
00195  01  REQUIRED-STORAGE.                                            ECS062
00196      12  WS-RETURN-CODE          PIC S9(4)              COMP.     ECS062
00197      12  WS-ABEND-MESSAGE        PIC  X(80).                      ECS062
00198      12  WS-ABEND-FILE-STATUS    PIC  XX     VALUE ZEROS.         ECS062
00199      12  WS-ZERO                 PIC S9      VALUE ZERO COMP-3.   ECS062
00200      12  ERACCT-FILE-STATUS.                                      ECS062
00201          16  ERACCT-STAT-1       PIC X.                           ECS062
00202          16  ERACCT-STAT-2       PIC X.                           ECS062
00203      12  SAV-KSM-CARR-GROUP.                                      ECS062
00204          16  SAV-KSM-CARRIER     PIC X.                           ECS062
00205          16  SAV-KSM-GROUPING    PIC X(6).                        ECS062
00206      12  SAV-KSM-ACCOUNT         PIC X(10).                       ECS062
00207      12  SAV-KSM-ACCT-NAME       PIC X(30).                       ECS062
00208      12  SAV-KSM-ADDR-1          PIC X(30).                       ECS062
00209      12  SAV-KSM-ADDR-2          PIC X(30).                       ECS062
00210      12  SAV-KSM-CITY-STATE      PIC X(30).                       ECS062
00211      12  SAV-KSM-ZIP             PIC X(9).                        ECS062
00212      12  SAV-KSM-DATE.                                            ECS062
00213          16  SAV-KSM-YY          PIC XX.                          ECS062
00214          16  SAV-KSM-MM          PIC XX.                          ECS062
00215          16  SAV-KSM-DD          PIC XX.                          ECS062
00216  EJECT                                                            ECS062
00217  01  WS-BILLING-DETAIL-RECORD.                                    ECS062
00218      12  BDR-RECORD-TYPE         PIC X.                           ECS062
00219      12  BDR-CARR-GROUP.                                          ECS062
00220          16  BDR-CARRIER         PIC X.                           ECS062
00221          16  BDR-GROUPING        PIC X(6).                        ECS062
00222      12  BDR-ACCOUNT             PIC X(10).                       ECS062
00223      12  BDR-ACCT-NAME           PIC X(30).                       ECS062
00224      12  BDR-ADDR-1              PIC X(30).                       ECS062
00225      12  BDR-ADDR-2              PIC X(30).                       ECS062
00226      12  BDR-CITY-STATE          PIC X(30).                       ECS062
00227      12  BDR-ZIP                 PIC X(9).                        ECS062
00228      12  BDR-DATE.                                                ECS062
00229          16  BDR-YY              PIC XX.                          ECS062
00230          16  BDR-MM              PIC XX.                          ECS062
00231          16  BDR-DD              PIC XX.                          ECS062
00232      12  BDR-NAME.                                                ECS062
00233          16  BDR-LAST-NAME       PIC X(15).                       ECS062
00234          16  BDR-FIRST-NAME      PIC X(10).                       ECS062
00235          16  BDR-INITIAL         PIC X.                           ECS062
00236      12  BDR-CERT.                                                ECS062
00237          16  BDR-CERT-NO         PIC X(10).                       ECS062
00238          16  BDR-CERT-SFX        PIC X.                           ECS062
00239      12  BDR-EFFECT-DATE.                                         ECS062
00240          16  BDR-EFF-YY          PIC XX.                          ECS062
00241          16  BDR-EFF-MM          PIC XX.                          ECS062
00242          16  BDR-EFF-DD          PIC XX.                          ECS062
00243      12  BDR-AGE                 PIC 99.                          ECS062
00244      12  BDR-LIFE-BEN            PIC XX.                          ECS062
00245      12  BDR-LIFE-BEN-TYPE       PIC X(3).                        ECS062
00246      12  BDR-LIFE-TERM           PIC S999.                        ECS062
00247      12  BDR-FACE-AMOUNT         PIC S9(9)V99        COMP-3.      ECS062
00248      12  BDR-LIFE-PREMIUM        PIC S9(7)V99        COMP-3.      ECS062
00249      12  BDR-LIFE-COMMISSION     PIC S9(7)V99        COMP-3.      ECS062
00250      12  BDR-FACE-AMT-ALT        PIC S9(9)V99        COMP-3.      ECS062
00251      12  BDR-LIFE-PREM-ALT       PIC S9(7)V99        COMP-3.      ECS062
00252      12  BDR-LIFE-COMM-ALT       PIC S9(7)V99        COMP-3.      ECS062
00253      12  BDR-LF-CANCEL-DATE.                                      ECS062
00254          16  BDR-LF-CAN-YY       PIC XX.                          ECS062
00255          16  BDR-LF-CAN-MM       PIC XX.                          ECS062
00256          16  BDR-LF-CAN-DD       PIC XX.                          ECS062
00257      12  BDR-AH-BEN              PIC XX.                          ECS062
00258      12  BDR-AH-BEN-TYPE         PIC X(3).                        ECS062
00259      12  BDR-AH-TERM             PIC S999.                        ECS062
00260      12  BDR-AH-MO-BENEFIT       PIC S9(9)V99        COMP-3.      ECS062
00261      12  BDR-AH-PREMIUM          PIC S9(7)V99        COMP-3.      ECS062
00262      12  BDR-AH-COMMISSION       PIC S9(7)V99        COMP-3.      ECS062
00263      12  BDR-AH-CANCEL-DATE.                                      ECS062
00264          16  BDR-AH-CAN-YY       PIC XX.                          ECS062
00265          16  BDR-AH-CAN-MM       PIC XX.                          ECS062
00266          16  BDR-AH-CAN-DD       PIC XX.                          ECS062
00267                                                                   ECS062
00268  01  WS-BILLING-SUMMARY-RECORD.                                   ECS062
00269      12  BSR-RECORD-TYPE         PIC X.                           ECS062
00270      12  BSR-CARR-GROUP.                                          ECS062
00271          16  BSR-CARRIER         PIC X.                           ECS062
00272          16  BSR-GROUPING        PIC X(6).                        ECS062
00273      12  BSR-ACCOUNT             PIC X(10).                       ECS062
00274      12  BSR-ACCT-NAME           PIC X(30).                       ECS062
00275      12  BSR-ADDR-1              PIC X(30).                       ECS062
00276      12  BSR-ADDR-2              PIC X(30).                       ECS062
00277      12  BSR-CITY-STATE          PIC X(30).                       ECS062
00278      12  BSR-ZIP                 PIC X(9).                        ECS062
00279      12  BSR-DATE.                                                ECS062
00280          16  BSR-YY              PIC XX.                          ECS062
00281          16  BSR-MM              PIC XX.                          ECS062
00282          16  BSR-DD              PIC XX.                          ECS062
00283      12  BSR-NET-LIFE-PREM       PIC S9(9)V99    COMP-3.          ECS062
00284      12  BSR-NET-AH-PREM         PIC S9(9)V99    COMP-3.          ECS062
00285      12  BSR-LIFE-COMM           PIC S9(9)V99    COMP-3.          ECS062
00286      12  BSR-AH-COMM             PIC S9(9)V99    COMP-3.          ECS062
00287      12  BSR-TOTAL-PREM          PIC S9(9)V99    COMP-3.          ECS062
00288      12  BSR-TOTAL-COMM          PIC S9(9)V99    COMP-3.          ECS062
00289      12  BSR-TOTAL-DUE           PIC S9(9)V99    COMP-3.          ECS062
00290      12  BSR-GROSS-LIFE-PREM     PIC S9(9)V99    COMP-3.          ECS062
00291      12  BSR-GROSS-AH-PREM       PIC S9(9)V99    COMP-3.          ECS062
00292      12  BSR-GROSS-LIFE-REFUNDS  PIC S9(9)V99    COMP-3.          ECS062
00293      12  BSR-GROSS-AH-REFUNDS    PIC S9(9)V99    COMP-3.          ECS062
00294      12  BSR-GROSS-WRITTEN       PIC S9(9)V99    COMP-3.          ECS062
00295      12  BSR-GROSS-REFUNDED      PIC S9(9)V99    COMP-3.          ECS062
00296      12  BSR-ISS-LF-CNT          PIC S9(7)       COMP-3.          ECS062
00297      12  BSR-ISS-AH-CNT          PIC S9(7)       COMP-3.          ECS062
00298      12  BSR-CAN-LF-CNT          PIC S9(7)       COMP-3.          ECS062
00299      12  BSR-CAN-AH-CNT          PIC S9(7)       COMP-3.          ECS062
00300      12  BSR-LO-CERT-DATE        PIC X(6).                        ECS062
00301      12  BSR-HI-CERT-DATE        PIC X(6).                        ECS062
00302      12  BSR-CAN-PREM-91         PIC S9(9)V99    COMP-3.          ECS062
00303      12  BSR-ACCT-STATUS         PIC X.                           ECS062
00304      12  BSR-REI-TABLE           PIC XXX.                         ECS062
00305      12  FILLER                  PIC X(5).                        ECS062
00306                                                                   ECS062
00307  01  KSM-TOTALS-AREA     COMP-3.                                  ECS062
00308      12  KSM-NET-LIFE-PREM       PIC S9(9)V99    VALUE ZEROS.     ECS062
00309      12  KSM-NET-AH-PREM         PIC S9(9)V99    VALUE ZEROS.     ECS062
00310      12  KSM-LIFE-COMM           PIC S9(9)V99    VALUE ZEROS.     ECS062
00311      12  KSM-AH-COMM             PIC S9(9)V99    VALUE ZEROS.     ECS062
00312      12  KSM-TOTAL-PREM          PIC S9(9)V99    VALUE ZEROS.     ECS062
00313      12  KSM-TOTAL-COMM          PIC S9(9)V99    VALUE ZEROS.     ECS062
00314      12  KSM-TOTAL-DUE           PIC S9(9)V99    VALUE ZEROS.     ECS062
00315      12  KSM-GROSS-LIFE-PREM     PIC S9(9)V99    VALUE ZEROS.     ECS062
00316      12  KSM-GROSS-AH-PREM       PIC S9(9)V99    VALUE ZEROS.     ECS062
00317      12  KSM-GROSS-LIFE-REFUNDS  PIC S9(9)V99    VALUE ZEROS.     ECS062
00318      12  KSM-GROSS-AH-REFUNDS    PIC S9(9)V99    VALUE ZEROS.     ECS062
00319      12  KSM-GROSS-WRITTEN       PIC S9(9)V99    VALUE ZEROS.     ECS062
00320      12  KSM-GROSS-REFUNDED      PIC S9(9)V99    VALUE ZEROS.     ECS062
00321      12  KSM-COUNT               PIC S9(7)       VALUE ZEROS.     ECS062
00322      12  KSM-DETAIL-COUNT        PIC S9(7)       VALUE ZEROS.     ECS062
00323  EJECT                                                            ECS062
00324  01  MISC-WORKING-STORAGE.                                        ECS062
00325      12  SAVE-ACCOUNT        PIC  X(10).                          ECS062
00326      12  SAVE-COMPANY-NAME   PIC  X(30)  OCCURS 3 TIMES.          ECS062
00327 *  LEFT JUSTIFY COMPANY NAME.                                     ECS062
00328      12  LJ-NAME.                                                 ECS062
00329          16  LJ-CHAR         PIC  X      OCCURS 30 TIMES.         ECS062
00330      12  RMTX.                                                    ECS062
00331          16  RMT0            PIC  X(10).                          ECS062
00332          16  RMT1            PIC  X(30).                          ECS062
00333          16  RMT2            PIC  X(30).                          ECS062
00334          16  RMT3            PIC  X(30).                          ECS062
00335          16  RMT4            PIC  X(30).                          ECS062
00336          16  RMT5.                                                ECS062
00337              20  FILLER      PIC  X(20).                          ECS062
00338              20  RMT-ZIP     PIC  X(10).                          ECS062
00339          16  RMT6            PIC  X(14).                          ECS062
00340      12  BILX.                                                    ECS062
00341          16  BIL0            PIC  X(10).                          ECS062
00342          16  BIL1            PIC  X(30).                          ECS062
00343          16  BIL2            PIC  X(30).                          ECS062
00344          16  BIL3            PIC  X(30).                          ECS062
00345          16  BIL4            PIC  X(30).                          ECS062
00346          16  BIL5            PIC  X(30).                          ECS062
00347          16  BIL6            PIC  X(30).                          ECS062
00348      12  REMIT-LEVELS.                                            ECS062
00349          16  RMT-LEVEL       PIC  X(190) OCCURS 4 TIMES.          ECS062
00350      12  TELE-ZIP-LINE.                                           ECS062
00351          16  LINE5-TELE          PIC  X(12).                      ECS062
00352          16  FILLER              PIC  X(01).                      ECS062
00353          16  LINE5-ZIP-CODE.                                      ECS062
00354              20  LINE5-ZIP-FIVE  PIC  X(05).                      ECS062
00355              20  LINE5-ZIP-DASH  PIC  X(01).                      ECS062
00356              20  LINE5-ZIP-FOUR  PIC  X(04).                      ECS062
00357          16  LINE5-POSTAL-CODE  REDEFINES  LINE5-ZIP-CODE.        ECS062
00358              20  LINE5-POST-CODE1                                 ECS062
00359                                  PIC  X(03).                      ECS062
00360              20  FILLER          PIC  X(01).                      ECS062
00361              20  LINE5-POST-CODE2                                 ECS062
00362                                  PIC  X(03).                      ECS062
00363              20  FILLER          PIC  X(03).                      ECS062
00364          16  FILLER              PIC  X(07).                      ECS062
00365      12  WORK-ZIP-CODE.                                           ECS062
00366          16  WZC-PRIME       PIC  X(05).                          ECS062
00367          16  WZC-PLUS4       PIC  X(04).                          ECS062
00368      12  WORK-ZIP-CD  REDEFINES  WORK-ZIP-CODE.                   ECS062
00369          16  WZC-POS-1       PIC  X(01).                          ECS062
00370          16  FILLER          PIC  X(08).                          ECS062
00371      12  WORK-POSTAL-CD  REDEFINES  WORK-ZIP-CODE.                ECS062
00372          16  WZC-POST-CD1    PIC  X(03).                          ECS062
00373          16  WZC-POST-CD2    PIC  X(03).                          ECS062
00374          16  FILLER          PIC  X(03).                          ECS062
00375      12  COMP-3-AREA     COMP-3.                                  ECS062
00376          16  PGCTR           PIC S9(5)           VALUE +0.        ECS062
00377          16  LNCTR           PIC S9(3)           VALUE +0.        ECS062
00378          16  VARY-LO         PIC S99V99          VALUE -00.00.    ECS062
00379          16  VARY-HI         PIC S99V99          VALUE +00.00.    ECS062
00380          16  WORK-PERC       PIC S99V999         VALUE +0.        ECS062
00381          16  TOTAL-DUE       PIC S9(7)V99        VALUE +0.        ECS062
00382          16  C-TT-SAVE-PREM  PIC S9(9)V99.                        ECS062
00383          16  C-TT-SAVE-COMM  PIC S9(9)V99.                        ECS062
00384                                                                   ECS062
00385      12  CERT-TOTALS     COMP-3.                                  ECS062
00386          16  C-TT-FACE       PIC S9(9)V99.                        ECS062
00387          16  C-TT-FACE-ALT   PIC S9(9)V99.                        ECS062
00388          16  C-TT-AH-BEN     PIC S9(9)V99.                        ECS062
00389          16  C-LF-PREM       PIC S9(9)V99.                        ECS062
00390          16  C-LF-PREM-ALT   PIC S9(9)V99.                        ECS062
00391          16  C-AH-PREM       PIC S9(9)V99.                        ECS062
00392          16  C-TT-PREM       PIC S9(9)V99.                        ECS062
00393          16  C-LF-COMM       PIC S9(9)V99.                        ECS062
00394          16  C-LF-COMM-ALT   PIC S9(9)V99.                        ECS062
00395          16  C-AH-COMM       PIC S9(9)V99.                        ECS062
00396          16  C-TT-COMM       PIC S9(9)V99.                        ECS062
00397          16  C-RR-PYMT       PIC S9(9)V99.                        ECS062
00398          16  C-CC-PYMT       PIC S9(9)V99.                        ECS062
00399          16  C-TT-PYMT       PIC S9(9)V99.                        ECS062
00400          16  C-LF-CLM        PIC S9(9)V99.                        ECS062
00401          16  C-AH-CLM        PIC S9(9)V99.                        ECS062
00402          16  C-TT-CLM        PIC S9(9)V99.                        ECS062
00403          16  C-ISS-AH-CNT    PIC S9(7).                           ECS062
00404          16  C-ISS-LF-CNT    PIC S9(7).                           ECS062
00405          16  C-CAN-AH-CNT    PIC S9(7).                           ECS062
00406          16  C-CAN-LF-CNT    PIC S9(7).                           ECS062
00407          16  C-CAN-PREM-91   PIC S9(9)V99.                        ECS062
00408      12  FILLER.                                                  ECS062
00409          16  C-LO-CERT-DATE  PIC X(6).                            ECS062
00410          16  C-HI-CERT-DATE  PIC X(6).                            ECS062
00411          16  C-ACCT-STATUS   PIC X.                               ECS062
00412          16  C-REI-TABLE     PIC XXX.                             ECS062
00413  EJECT                                                            ECS062
00414      12  STMT-TOTALS     COMP-3.                                  ECS062
00415          16  S-TT-FACE       PIC S9(9)V99.                        ECS062
00416          16  S-TT-AH-BEN     PIC S9(9)V99.                        ECS062
00417          16  S-LF-PREM       PIC S9(9)V99.                        ECS062
00418          16  S-AH-PREM       PIC S9(9)V99.                        ECS062
00419          16  S-TT-PREM       PIC S9(9)V99.                        ECS062
00420          16  S-LF-COMM       PIC S9(9)V99.                        ECS062
00421          16  S-AH-COMM       PIC S9(9)V99.                        ECS062
00422          16  S-TT-COMM       PIC S9(9)V99.                        ECS062
00423          16  S-RR-PYMT       PIC S9(9)V99.                        ECS062
00424          16  S-CC-PYMT       PIC S9(9)V99.                        ECS062
00425          16  S-TT-PYMT       PIC S9(9)V99.                        ECS062
00426          16  S-LF-CLM        PIC S9(9)V99.                        ECS062
00427          16  S-AH-CLM        PIC S9(9)V99.                        ECS062
00428          16  S-TT-CLM        PIC S9(9)V99.                        ECS062
00429          16  S-ISS-AH-CNT    PIC S9(7).                           ECS062
00430          16  S-ISS-LF-CNT    PIC S9(7).                           ECS062
00431          16  S-CAN-AH-CNT    PIC S9(7).                           ECS062
00432          16  S-CAN-LF-CNT    PIC S9(7).                           ECS062
00433          16  S-CAN-PREM-91   PIC S9(9)V99.                        ECS062
00434          16  S-BEG-BAL       PIC S9(9)V99.                        ECS062
00435          16  S-WRT-OFF       PIC S9(9)V99.                        ECS062
00436          16  S-END-BAL       PIC S9(9)V99.                        ECS062
00437                                                                   ECS062
00438      12  FILLER.                                                  ECS062
00439          16  S-LO-CERT-DATE  PIC 9(11)  COMP-3.                   ECS062
00440          16  S-HI-CERT-DATE  PIC 9(11)  COMP-3.                   ECS062
00441          16  S-ACCT-STATUS   PIC X.                               ECS062
00442          16  S-REI-TABLE     PIC XXX.                             ECS062
00443                                                                   ECS062
00444      12  FINAL-TOTALS    COMP-3.                                  ECS062
00445          16  T-BEG-BAL       PIC S9(9)V99        VALUE +0.        ECS062
00446          16  T-TOT-PRM       PIC S9(9)V99        VALUE +0.        ECS062
00447          16  T-TOT-COM       PIC S9(9)V99        VALUE +0.        ECS062
00448          16  T-ADJ-COM       PIC S9(9)V99        VALUE +0.        ECS062
00449          16  T-WRT-OFF       PIC S9(9)V99        VALUE +0.        ECS062
00450          16  T-TOT-PMT       PIC S9(9)V99        VALUE +0.        ECS062
00451          16  T-END-BAL       PIC S9(9)V99        VALUE +0.        ECS062
00452                                                                   ECS062
00453      12  SPECIAL-TOTALS  COMP-3.                                  ECS062
00454          16  TOT-DUE-MONTH   PIC S9(9)V99        VALUE +0.        ECS062
00455          16  TOT-UNPAID-BAL  PIC S9(9)V99        VALUE +0.        ECS062
00456          16  TOT-TOTAL-DUE   PIC S9(9)V99        VALUE +0.        ECS062
00457                                                                   ECS062
00458      12  CUR-CP-SEQ.                                              ECS062
00459          16  CUR-CP-CTL-1.                                        ECS062
00460              20  CUR-CP-CARR-GROUP.                               ECS062
00461                  24  CUR-CP-CARRIER  PIC  X.                      ECS062
00462                  24  CUR-CP-GROUPING PIC  X(6).                   ECS062
00463              20  CUR-CP-RESP-NO      PIC  X(10).                  ECS062
00464          16  CUR-CP-CTL-2.                                        ECS062
00465              20  CUR-CP-ACCOUNT      PIC  X(10).                  ECS062
00466                                                                   ECS062
00467      12  PRE-CONTROL.                                             ECS062
00468          16  PRE-CTL-1.                                           ECS062
00469              20  PRE-CARR-GROUP.                                  ECS062
00470                  24  PRE-CARRIER     PIC  X.                      ECS062
00471                  24  PRE-GROUPING    PIC  X(6).                   ECS062
00472              20  PRE-RESP-NO         PIC  X(10).                  ECS062
00473          16  PRE-CTL-2.                                           ECS062
00474              20  PRE-ACCOUNT         PIC  X(10).                  ECS062
00475                                                                   ECS062
00476      12  FIRST-CLAIM-SW              PIC  X.                      ECS062
00477          88  FIRST-CLAIM                     VALUE 'Y'.           ECS062
00478          88  ALREADY-PRINTED-TOTALS          VALUE 'N'.           ECS062
00479                                                                   ECS062
00480      12  SPECIAL-PRINT-SW    PIC  X          VALUE 'N'.           ECS062
00481          88  FROM-SPECIAL-PRINT              VALUE 'Y'.           ECS062
00482                                                                   ECS062
00483      12  PP-TOT-DESC.                                             ECS062
00484          16  FILLER          PIC  X(6)       VALUE 'TOTAL '.      ECS062
00485          16  PP-TOT-TITLE    PIC  X(14).                          ECS062
00486  EJECT                                                            ECS062
00487  01  HD1.                                                         ECS062
00488      12  FILLER              PIC  X(51)      VALUE SPACES.        ECS062
00489      12  FILLER              PIC  X(30)      VALUE                ECS062
00490              'STATEMENT FOR CREDIT INSURANCE'.                    ECS062
00491      12  FILLER              PIC  X(43)      VALUE SPACES.        ECS062
00492      12  FILLER              PIC  X(8)       VALUE 'ECS062'.      ECS062
00493                                                                   ECS062
00494  01  HD2.                                                         ECS062
00495      12  FILLER              PIC  X(51)      VALUE SPACES.        ECS062
00496      12  HD-CO               PIC  X(30).                          ECS062
00497      12  FILLER              PIC  X(43)      VALUE SPACES.        ECS062
00498      12  HD-RUN-DT           PIC  X(8)       VALUE SPACES.        ECS062
00499                                                                   ECS062
00500  01  HD3.                                                         ECS062
00501      12  FILLER              PIC  X(57)      VALUE SPACES.        ECS062
00502      12  HD-DT               PIC  X(18).                          ECS062
00503      12  FILLER              PIC  X(37)      VALUE SPACES.        ECS062
00504      12  FILLER              PIC  X(5)       VALUE 'PAGE'.        ECS062
00505      12  HD-PG               PIC ZZ,ZZ9.                          ECS062
00506      12  FILLER              PIC  X(9).                           ECS062
00507                                                                   ECS062
00508  01  HD4.                                                         ECS062
00509      12  HD-M1               PIC  X(14).                          ECS062
00510      12  HD-CARR-GROUP       PIC  X(7).                           ECS062
00511      12  HD-C-A              PIC  X.                              ECS062
00512      12  HD-ACCOUNT          PIC  X(10).                          ECS062
00513      12  FILLER              PIC  XX.                             ECS062
00514      12  HD-NAME             PIC  X(30).                          ECS062
00515      12  FILLER              PIC  X(11).                          ECS062
00516      12  HD-M2               PIC  X(11).                          ECS062
00517      12  HD-M3               PIC  X(10).                          ECS062
00518      12  FILLER              PIC  XX.                             ECS062
00519      12  HD-REMIT            PIC  X(30).                          ECS062
00520      12  FILLER              PIC  X(4).                           ECS062
00521                                                                   ECS062
00522  01  HD5.                                                         ECS062
00523      12  FILLER              PIC  X(44)      VALUE                ECS062
00524              '                                 CERT.      '.      ECS062
00525      12  FILLER              PIC  X(44)      VALUE                ECS062
00526              'EFF.    CANCEL         BENEFIT    FACE AMT. '.      ECS062
00527      12  FILLER              PIC  X(44)      VALUE                ECS062
00528              '/         WRITTEN    COMP.                  '.      ECS062
00529                                                                   ECS062
00530  01  HD5A.                                                        ECS062
00531      12  FILLER              PIC  X(44)      VALUE                ECS062
00532              '                                 CERT.      '.      ECS062
00533      12  FILLER              PIC  X(44)      VALUE                ECS062
00534              'EFF.    CANCEL         BENEFIT    FACE AMT. '.      ECS062
00535      12  FILLER              PIC  X(44)      VALUE                ECS062
00536              '/         WRITTEN    TOTAL                  '.      ECS062
00537                                                                   ECS062
00538  01  HD6.                                                         ECS062
00539      12  FILLER              PIC  X(27)      VALUE                ECS062
00540              '       NAME OF INSURED'.                            ECS062
00541      12  HD6-ST              PIC  XX         VALUE SPACE.         ECS062
00542      12  FILLER              PIC  X(15)      VALUE                ECS062
00543              '    NUMBER'.                                        ECS062
00544      12  FILLER              PIC  X(44)      VALUE                ECS062
00545              'DATE     DATE  AGE TERM  TYPE  MTHLY. BENEFI'.      ECS062
00546      12  FILLER              PIC  X(44)      VALUE                ECS062
00547              'T         PREMIUM     PCT.     COMPENSATION '.      ECS062
00548                                                                   ECS062
00549  01  HD6A.                                                        ECS062
00550      12  FILLER              PIC  X(44)      VALUE                ECS062
00551              '       NAME OF INSURED           NUMBER     '.      ECS062
00552      12  FILLER              PIC  X(44)      VALUE                ECS062
00553              'DATE     DATE  AGE TERM  TYPE  MTHLY. BENEFI'.      ECS062
00554      12  FILLER              PIC  X(44)      VALUE                ECS062
00555              'T         PREMIUM   PREMIUM    COMPENSATION '.      ECS062
00556  EJECT                                                            ECS062
00557  01  P-REC.                                                       ECS062
00558      12  P-CCSW                  PIC  X.                          ECS062
00559      12  P-LINE.                                                  ECS062
00560          16  FILLER              PIC  X(132).                     ECS062
00561      12  P-LINE-1  REDEFINES  P-LINE.                             ECS062
00562          16  P-CP-NAME.                                           ECS062
00563              20  P-CP-LNAME      PIC  X(15).                      ECS062
00564              20  FILLER          PIC  X.                          ECS062
00565              20  P-CP-FNAME      PIC  X(10).                      ECS062
00566              20  FILLER          PIC  X.                          ECS062
00567              20  P-CP-UNDRWRTR.                                   ECS062
00568                  24  P-CP-INITIAL PIC  X.                         ECS062
00569                  24  P-INIT-DOT   PIC  X.                         ECS062
00570          16  P-CP-NAME-RECALC  REDEFINES  P-CP-NAME.              ECS062
00571              20  P-CP-RECALC-FL  PIC  X(3).                       ECS062
00572              20  P-CP-LNAME-R    PIC  X(12).                      ECS062
00573              20  FILLER          PIC  X.                          ECS062
00574              20  P-CP-FNAME-R    PIC  X(10).                      ECS062
00575              20  FILLER          PIC  X(3).                       ECS062
00576          16  FILLER              PIC  X.                          ECS062
00577          16  P-CP-CERT           PIC  X(11).                      ECS062
00578          16  FILLER              PIC  X.                          ECS062
00579          16  P-CP-EMO            PIC  XX.                         ECS062
00580          16  P-CP-EMOD           PIC  X.                          ECS062
00581          16  P-CP-EDA            PIC  XX.                         ECS062
00582          16  P-CP-EDAD           PIC  X.                          ECS062
00583          16  P-CP-EYR            PIC  XX.                         ECS062
00584          16  FILLER              PIC  X.                          ECS062
00585          16  P-CP-CANCEL.                                         ECS062
00586              20  P-CP-CMO        PIC  XX.                         ECS062
00587              20  P-CP-CMOD       PIC  X.                          ECS062
00588              20  P-CP-CDA        PIC  XX.                         ECS062
00589              20  P-CP-CDAD       PIC  X.                          ECS062
00590              20  P-CP-CYR        PIC  XX.                         ECS062
00591          16  FILLER              PIC  X.                          ECS062
00592          16  P-CP-AGE            PIC  XX.                         ECS062
00593          16  FILLER              PIC  X.                          ECS062
00594          16  P-CP-TERM           PIC ZZZ.                         ECS062
00595          16  FILLER              PIC  X.                          ECS062
00596          16  P-CP-BEN            PIC  XX.                         ECS062
00597          16  P-CP-BEN-DASH       PIC  X.                          ECS062
00598          16  P-CP-BEN-TYPE       PIC  X(4).                       ECS062
00599          16  P-CP-FACE           PIC ZZZZ,ZZZ,ZZZ.ZZ-.            ECS062
00600          16  P-CP-PREMIUM        PIC ZZZZ,ZZZ,ZZZ.ZZ-.            ECS062
00601          16  P-OTHER-PRT.                                         ECS062
00602              20  P-FILL-1        PIC  XX.                         ECS062
00603              20  P-CP-COMP-PCT   PIC ZZZ.ZZZ.                     ECS062
00604              20  P-FILL-2        PIC  X.                          ECS062
00605          16  P-CP-TOT-PREM  REDEFINES                             ECS062
00606              P-OTHER-PRT         PIC ZZ,ZZZ.ZZ-.                  ECS062
00607          16  P-CP-COMP           PIC ZZZZ,ZZZ,ZZZ.ZZ-.            ECS062
00608  EJECT                                                            ECS062
00609      12  P-LINE-2  REDEFINES  P-LINE.                             ECS062
00610          16  P-CP-DESC           PIC  X(30).                      ECS062
00611          16  FILLER              PIC  X(44).                      ECS062
00612          16  P-CP-PYMT           PIC ZZZZ,ZZZ,ZZZ.ZZ-.            ECS062
00613          16  FILLER              PIC  X(12).                      ECS062
00614          16  P-ACCTG-COMMENT     PIC  X(30).                      ECS062
00615      12  P-LINE-3  REDEFINES  P-LINE.                             ECS062
00616          16  FILLER              PIC  X(57).                      ECS062
00617          16  P-CLM-MESSAGE       PIC  X(35).                      ECS062
00618          16  FILLER              PIC  X(7).                       ECS062
00619          16  P-CLM-PYMT          PIC ZZZZ,ZZZ,ZZZ.ZZ-.            ECS062
00620          16  FILLER              PIC  X(17).                      ECS062
00621      12  P-LINE-4  REDEFINES  P-LINE.                             ECS062
00622          16  FILLER              PIC  X(42).                      ECS062
00623          16  P-TOT-DESC          PIC  X(20).                      ECS062
00624          16  FILLER              PIC  X(12).                      ECS062
00625          16  P-TOT-FACE          PIC ZZZZ,ZZZ,ZZZ.ZZ-.            ECS062
00626          16  P-TOT-PREMIUM       PIC ZZZZ,ZZZ,ZZZ.99-.            ECS062
00627          16  P-TOT-PREM-X  REDEFINES                              ECS062
00628              P-TOT-PREMIUM       PIC  X(16).                      ECS062
00629          16  FILLER              PIC  X(10).                      ECS062
00630          16  P-TOT-COMP          PIC ZZZZ,ZZZ,ZZZ.99-.            ECS062
00631      12  P-LINE-5  REDEFINES  P-LINE.                             ECS062
00632          16  FILLER              PIC  X(49).                      ECS062
00633          16  P-TOT-DESC1         PIC  X(30).                      ECS062
00634          16  FILLER              PIC  X(11).                      ECS062
00635          16  P-TOT-PREMIUM1      PIC ZZZZ,ZZZ,ZZZ.99-.            ECS062
00636          16  P-TOT-PREM-X1 REDEFINES                              ECS062
00637              P-TOT-PREMIUM1      PIC  X(16).                      ECS062
00638          16  FILLER              PIC  X(26).                      ECS062
00639      12  P-LINE-5-KSM  REDEFINES  P-LINE.                         ECS062
00640          16  FILLER              PIC  X(36).                      ECS062
00641          16  P-TOT-DESC1-KSM     PIC  X(58).                      ECS062
00642          16  P-TOT-DESC2-KSM  REDEFINES  P-TOT-DESC1-KSM.         ECS062
00643              20  P-PAY-KSM-1     PIC  X(28).                      ECS062
00644              20  P-PAY-KSM-2     PIC  X(30).                      ECS062
00645          16  FILLER              PIC  X(38).                      ECS062
00646      12  P-LINE-6  REDEFINES  P-LINE.                             ECS062
00647          16  FILLER              PIC  X(44).                      ECS062
00648          16  L6-DESC.                                             ECS062
00649              20  L6-STAR1        PIC  X.                          ECS062
00650              20  FILLER          PIC  X(4).                       ECS062
00651              20  L6-DESC1        PIC  X(39).                      ECS062
00652              20  FILLER          PIC  X(4).                       ECS062
00653              20  L6-STAR2        PIC  X.                          ECS062
00654          16  FILLER              PIC  X(39).                      ECS062
00655      12  P-LINE-7  REDEFINES  P-LINE.                             ECS062
00656          16  FILLER              PIC  X(34).                      ECS062
00657          16  L7-NAME             PIC  X(30).                      ECS062
00658          16  FILLER              PIC  X(68).                      ECS062
00659      12  P-LINE-8  REDEFINES  P-LINE.                             ECS062
00660          16  FILLER              PIC  X(97).                      ECS062
00661          16  P-TOT-REC-COUNT     PIC Z,ZZZ,ZZ9-.                  ECS062
00662          16  FILLER              PIC  X(25).                      ECS062
00663                                                                   ECS062
00664  EJECT                                                            ECS062
00665  01  KSM-SPEC-LINE5-1.                                            ECS062
00666      12  FILLER              PIC  X(58)      VALUE                ECS062
00667      '     WE APPRECIATE YOUR BUSINESS AND PROMPT PAYMENT!      '.ECS062
00668                                                                   ECS062
00669  01  KSM-SPEC-LINE5-2.                                            ECS062
00670      12  FILLER              PIC  X(58)      VALUE                ECS062
00671      'STATEMENT IS DUE UPON RECEIPT. PLEASE PAY AS BILLED.      '.ECS062
00672                                                                   ECS062
00673  01  KSM-SPEC-LINE5-3.                                            ECS062
00674      12  FILLER              PIC  X(58)      VALUE                ECS062
00675      'CERTIFICATE TRANSACTIONS, INCLUDING REFUNDS NOT APPEARING '.ECS062
00676                                                                   ECS062
00677  01  KSM-SPEC-LINE5-4.                                            ECS062
00678      12  FILLER              PIC  X(58)      VALUE                ECS062
00679      'ON THIS STATEMENT WILL APPEAR ON THE NEXT MONTHLY BILLING.'.ECS062
00680                                                                   ECS062
00681  01  KSM-SPEC-LINE5-5.                                            ECS062
00682      12  FILLER              PIC  X(28)      VALUE                ECS062
00683      'MAKE CHECKS PAYABLE TO:     '.                              ECS062
00684                                                                   ECS062
00685  EJECT                                                            ECS062
00686                              COPY ERCCOMP.                        ECS062
00687                                                                   ECS062
00688                              COPY ELCDTECX.                       ECS062
00689                                                                   ECS062
00690                              COPY ELCDTEVR.                       ECS062
00691                                                                   ECS062
00692                              COPY ELCBILVR.                       ECS062
00693  EJECT                                                            ECS062
00694  PROCEDURE DIVISION.                                              ECS062
00695                                                                   ECS062
00696  0000-STANDARD-RTN.                                               ECS062
00697                              COPY ELCDTERX.                       ECS062
00698                                                                   ECS062
00699      OPEN INPUT  COMM-TRAN-IN   COMM-MSTR-IN                      ECS062
00700           OUTPUT SUMM-TRAN-OUT  COMM-MSTR-OUT  PRNTR.             ECS062
00701                                                                   ECS062
00702      IF DTE-CLIENT  =  'KSM'                                      ECS062
00703          OPEN INPUT  ERACCT                                       ECS062
00704               OUTPUT BILLING-DATA-FILE                            ECS062
00705          IF ERACCT-FILE-STATUS NOT = ZERO AND '97'                ECS062
00706              MOVE 'ERROR OCCURRED OPEN - ERACCT'                  ECS062
00707                                      TO  WS-ABEND-MESSAGE         ECS062
00708              MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS      ECS062
00709              GO TO ABEND-PGM                                      ECS062
00710           ELSE                                                    ECS062
00711          PERFORM 2510-CLEAR-BILLING-SUM-RECORD THRU 2599-EXIT     ECS062
00712          PERFORM 2610-CLEAR-BILLING-DET-RECORD THRU 2699-EXIT.    ECS062
00713                                                                   ECS062
00714      MOVE LOW-VALUES             TO  COMPENSATION-MASTER          ECS062
00715                                      COMP-IN-RECORD               ECS062
00716                                      COMP-OUT-RECORD              ECS062
00717                                      CUR-CP-SEQ                   ECS062
00718                                      PRE-CONTROL.                 ECS062
00719                                                                   ECS062
00720      MOVE COMPANY-NAME           TO  SAVE-COMPANY-NAME (1)        ECS062
00721                                      SAVE-COMPANY-NAME (2)        ECS062
00722                                      SAVE-COMPANY-NAME (3).       ECS062
00723                                                                   ECS062
00724      MOVE ALPH-DATE              TO  HD-DT.                       ECS062
00725      MOVE WS-CURRENT-DATE        TO  HD-RUN-DT.                   ECS062
00726      MOVE SPACES                 TO  RMTX.                        ECS062
00727      MOVE CNT-NAME (1)           TO  RMT1.                        ECS062
00728      MOVE CNT-NAME (2)           TO  RMT2.                        ECS062
00729      MOVE CNT-NAME (3)           TO  RMT3.                        ECS062
00730      MOVE CNT-NAME (4)           TO  RMT4.                        ECS062
00731      MOVE CNT-NAME (5)           TO  TELE-ZIP-LINE.               ECS062
00732      MOVE SPACES                 TO  RMT5.                        ECS062
00733      MOVE LINE5-ZIP-CODE         TO  RMT-ZIP.                     ECS062
00734                                                                   ECS062
00735 *  SYSTEM REMIT TO                                                ECS062
00736      MOVE RMTX                   TO  RMT-LEVEL (1).               ECS062
00737 *  CARRIER REMIT TO                                               ECS062
00738      MOVE RMTX                   TO  RMT-LEVEL (2).               ECS062
00739 *  COMPANY REMIT TO                                               ECS062
00740      MOVE RMTX                   TO  RMT-LEVEL (3).               ECS062
00741 *  AGENT REMIT TO                                                 ECS062
00742      MOVE RMTX                   TO  RMT-LEVEL (4).               ECS062
00743                                                                   ECS062
00744      IF DTE-WRT-OFF  IS NOT NUMERIC                               ECS062
00745          MOVE ZEROS              TO  DTE-WRT-OFF                  ECS062
00746                                      VARY-HI  VARY-LO             ECS062
00747      ELSE                                                         ECS062
00748          MOVE DTE-WRT-OFF        TO  VARY-HI                      ECS062
00749          COMPUTE VARY-LO  =  VARY-HI  *  -1.                      ECS062
00750                                                                   ECS062
00751      PERFORM 8000-MSTR-CONTROL-RTN  THRU  8099-EXIT.              ECS062
00752                                                                   ECS062
00753      PERFORM 8200-RD-CCM-RTN  THRU  8299-EXIT.                    ECS062
00754                                                                   ECS062
00755      IF DTE-CLIENT  = 'DMD'                                       ECS062
00756          MOVE  'ST'              TO  HD6-ST.                      ECS062
00757                                                                   ECS062
00758      GO TO 1000-PROCESS-RTN.                                      ECS062
00759  EJECT                                                            ECS062
00760  1000-PROCESS-RTN.                                                ECS062
00761      IF CUR-CP-SEQ  LESS CO-CONTROL                               ECS062
00762          MOVE COMM-PREM-RECORD   TO  CCM-WK                       ECS062
00763          PERFORM 8300-FORM-CCM-RTN  THRU  8399-EXIT               ECS062
00764          PERFORM 8200-RD-CCM-RTN    THRU  8299-EXIT               ECS062
00765          GO TO 1000-PROCESS-RTN.                                  ECS062
00766                                                                   ECS062
00767      IF CUR-CP-SEQ  GREATER CO-CONTROL                            ECS062
00768          PERFORM 2400-PRT-TOTAL-RTN     THRU  2499-EXIT           ECS062
00769          PERFORM 4200-UPDATE-MSTR-RTN   THRU  4299-EXIT           ECS062
00770          PERFORM 8000-MSTR-CONTROL-RTN  THRU  8099-EXIT           ECS062
00771          GO TO 1000-PROCESS-RTN.                                  ECS062
00772                                                                   ECS062
00773      IF CUR-CP-SEQ  =  HIGH-VALUES                                ECS062
00774          GO TO 9990-E-O-J.                                        ECS062
00775                                                                   ECS062
00776      IF DTE-PGM-OPT  NOT = 3                                      ECS062
00777          MOVE +1                 TO  STMT-SW.                     ECS062
00778                                                                   ECS062
00779      PERFORM 2000-PRT-DETAIL-RTN  THRU  2099-EXIT.                ECS062
00780                                                                   ECS062
00781      PERFORM 8200-RD-CCM-RTN  THRU  8299-EXIT.                    ECS062
00782                                                                   ECS062
00783      GO TO 1000-PROCESS-RTN.                                      ECS062
00784  EJECT                                                            ECS062
00785  1500-CHECK-EFF-CHANGE.                                           ECS062
00786      IF FIRST-TIME                                                ECS062
00787          MOVE CP-EFF                 TO  SAVE-CP-EFF              ECS062
00788          MOVE CP-ACCOUNT             TO  SAVE-ACCOUNT             ECS062
00789          MOVE 'N'                    TO  FIRST-TIME-SW.           ECS062
00790                                                                   ECS062
00791      IF (CP-EFF NOT = SAVE-CP-EFF) OR                             ECS062
00792         (CUR-CP-ACCOUNT NOT = SAVE-ACCOUNT) OR                    ECS062
00793         (NOT CP-ISSUE AND NOT CP-CANCEL AND NOT CP-ACCTG          ECS062
00794          AND NOT CP-RC-ISSUE AND NOT CP-RC-CANCEL)                ECS062
00795          MOVE CP-EFF                 TO  SAVE-CP-EFF              ECS062
00796          MOVE CP-ACCOUNT             TO  SAVE-ACCOUNT             ECS062
00797          MOVE 'EFF. DATE TOTALS'     TO  P-TOT-DESC               ECS062
00798          MOVE C-TT-SAVE-PREM         TO  P-TOT-PREMIUM            ECS062
00799          MOVE C-TT-SAVE-COMM         TO  P-TOT-COMP               ECS062
00800          MOVE ZEROS                  TO  P-TOT-FACE               ECS062
00801                                          C-TT-SAVE-PREM           ECS062
00802                                          C-TT-SAVE-COMM           ECS062
00803          MOVE SPACE-1                TO  P-CCSW                   ECS062
00804          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    ECS062
00805          MOVE SPACE-1                TO  P-CCSW                   ECS062
00806          PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                   ECS062
00807                                                                   ECS062
00808 *    THIS IF WILL SHUT OFF SWITCH WHEN THE PROGRAM GETS TO THE    ECS062
00809 *    END OF TRANSACTIONS WITH ACCOUNT NUMBERS.                    ECS062
00810 *                                                                 ECS062
00811      IF CUR-CP-ACCOUNT = LOW-VALUES OR HIGH-VALUES                ECS062
00812          MOVE 'N'            TO PREV-ISSUE-CANCEL-SW.             ECS062
00813                                                                   ECS062
00814  1500-EXIT.                                                       ECS062
00815       EXIT.                                                       ECS062
00816                                                                   ECS062
00817  2000-PRT-DETAIL-RTN.                                             ECS062
00818      IF DTE-CLIENT = 'KSM'                                        ECS062
00819          PERFORM 8500-START-ERACCT THRU  8599-EXIT.               ECS062
00820                                                                   ECS062
00821      IF DTE-CLIENT = 'DMD'                                        ECS062
00822          IF PREV-ISSUE-CANCEL                                     ECS062
00823              PERFORM 1500-CHECK-EFF-CHANGE THRU 1500-EXIT.        ECS062
00824                                                                   ECS062
00825      PERFORM 3000-CALC-DETAIL-RTN  THRU  3099-EXIT.               ECS062
00826                                                                   ECS062
00827      MOVE RUN-YR                 TO  CO-ACT-YEAR.                 ECS062
00828      MOVE RUN-MO                 TO  CO-ACT-MONTH.                ECS062
00829      MOVE RUN-DA                 TO  CO-ACT-DAY.                  ECS062
00830                                                                   ECS062
00831      IF STMT-SW  = +0                                             ECS062
00832          GO TO 2090-PRE-EXIT.                                     ECS062
00833                                                                   ECS062
00834      IF DTE-CLIENT  =  'KSM'                                      ECS062
00835          IF LNCTR  GREATER +45                                    ECS062
00836              PERFORM 2900-PRT-STMT-HD-RTN  THRU  2999-EXIT        ECS062
00837          ELSE                                                     ECS062
00838              NEXT SENTENCE                                        ECS062
00839      ELSE                                                         ECS062
00840          IF LNCTR  GREATER +30                                    ECS062
00841              PERFORM 2900-PRT-STMT-HD-RTN  THRU  2999-EXIT.       ECS062
00842                                                                   ECS062
00843      MOVE SPACE              TO PREV-ISSUE-CANCEL-SW.             ECS062
00844                                                                   ECS062
00845      IF CP-CLAIM                                                  ECS062
00846          GO TO 2070-PRT-CLAIM.                                    ECS062
00847                                                                   ECS062
00848      IF CP-ACCTG                                                  ECS062
00849          GO TO 2060-PRT-ACCTG.                                    ECS062
00850                                                                   ECS062
00851      IF DTE-CLIENT  =  'KSM'                                      ECS062
00852          NEXT SENTENCE                                            ECS062
00853      ELSE                                                         ECS062
00854          GO TO 2020-PRT-ISSUE-OR-CANCEL.                          ECS062
00855                                                                   ECS062
00856      MOVE 'NNN'                  TO  PRT-TOTAL-SWITCH.            ECS062
00857                                                                   ECS062
00858      IF CP-LF-TYPE  =  '00'                                       ECS062
00859          MOVE 'N'                TO  PRT-TOT-1-SW                 ECS062
00860      ELSE                                                         ECS062
00861          MOVE 'Y'                TO  PRT-TOT-1-SW.                ECS062
00862                                                                   ECS062
00863      IF C-TT-FACE-ALT  = ZERO                                     ECS062
00864        AND  C-LF-PREM-ALT  = ZERO                                 ECS062
00865        AND  C-LF-COMM-ALT  = ZERO                                 ECS062
00866          MOVE 'N'                TO  PRT-TOT-2-SW                 ECS062
00867      ELSE                                                         ECS062
00868          MOVE 'N'                TO  PRT-TOT-1-SW                 ECS062
00869          MOVE 'Y'                TO  PRT-TOT-2-SW.                ECS062
00870                                                                   ECS062
00871      IF CP-AH-TYPE  = '00'                                        ECS062
00872          MOVE 'N'                TO  PRT-TOT-3-SW                 ECS062
00873      ELSE                                                         ECS062
00874          MOVE 'N'                TO  PRT-TOT-1-SW                 ECS062
00875          MOVE 'N'                TO  PRT-TOT-2-SW                 ECS062
00876          MOVE 'Y'                TO  PRT-TOT-3-SW.                ECS062
00877                                                                   ECS062
00878  2020-PRT-ISSUE-OR-CANCEL.                                        ECS062
00879      MOVE 'Y'                    TO  PREV-ISSUE-CANCEL-SW.        ECS062
00880      MOVE CP-EFF                 TO  SAVE-CP-EFF.                 ECS062
00881      MOVE CUR-CP-ACCOUNT         TO  SAVE-ACCOUNT.                ECS062
00882                                                                   ECS062
00883      IF DTE-CLIENT = 'DMD'                                        ECS062
00884          MOVE 'N'                TO FIRST-TIME-SW.                ECS062
00885                                                                   ECS062
00886      MOVE CP-FNAME               TO  P-CP-FNAME.                  ECS062
00887                                                                   ECS062
00888      MOVE CP-INITIAL             TO  P-CP-INITIAL.                ECS062
00889                                                                   ECS062
00890      IF CP-INITIAL  NOT = SPACE                                   ECS062
00891          MOVE '.'                TO  P-INIT-DOT                   ECS062
00892      ELSE                                                         ECS062
00893          MOVE ' '                TO  P-INIT-DOT.                  ECS062
00894                                                                   ECS062
00895      MOVE CP-LNAME               TO  P-CP-LNAME.                  ECS062
00896      MOVE CP-CERT                TO  P-CP-CERT.                   ECS062
00897      MOVE CP-MO                  TO  P-CP-EMO.                    ECS062
00898      MOVE CP-DA                  TO  P-CP-EDA.                    ECS062
00899      MOVE CP-YR                  TO  P-CP-EYR.                    ECS062
00900      MOVE '-'                    TO  P-CP-EMOD  P-CP-EDAD.        ECS062
00901      MOVE CP-AGE                 TO  P-CP-AGE.                    ECS062
00902                                                                   ECS062
00903      IF DTE-CLIENT  = 'KSM'                                       ECS062
00904          MOVE CP-LNAME           TO  BDR-LAST-NAME                ECS062
00905          MOVE CP-FNAME           TO  BDR-FIRST-NAME               ECS062
00906          MOVE CP-INITIAL         TO  BDR-INITIAL                  ECS062
00907          MOVE CP-CERT            TO  BDR-CERT                     ECS062
00908          MOVE CP-MO              TO  BDR-EFF-MM                   ECS062
00909          MOVE CP-DA              TO  BDR-EFF-DD                   ECS062
00910          MOVE CP-YR              TO  BDR-EFF-YY                   ECS062
00911          MOVE CP-AGE             TO  BDR-AGE.                     ECS062
00912                                                                   ECS062
00913  2030-FIRST-BENEFIT.                                              ECS062
00914      MOVE SPACES                 TO  P-CP-BEN-TYPE.               ECS062
00915      MOVE CP-LF-TYPE             TO  CLAS-LOOK.                   ECS062
00916                                                                   ECS062
00917      IF CLAS-LOOK  = '00'                                         ECS062
00918          IF DTE-CLIENT = 'KSM'                                    ECS062
00919              MOVE ZEROS              TO  BDR-LIFE-TERM            ECS062
00920                                          BDR-FACE-AMOUNT          ECS062
00921                                          BDR-LIFE-PREMIUM         ECS062
00922                                          BDR-LIFE-COMMISSION      ECS062
00923                                          BDR-FACE-AMT-ALT         ECS062
00924                                          BDR-LIFE-PREM-ALT        ECS062
00925                                          BDR-LIFE-COMM-ALT        ECS062
00926              MOVE 'Y'                TO  AH-ONLY-SWITCH           ECS062
00927              GO TO 2050-CHECK-AH                                  ECS062
00928          ELSE                                                     ECS062
00929              MOVE 'Y'                TO  AH-ONLY-SWITCH           ECS062
00930              GO TO 2050-CHECK-AH.                                 ECS062
00931                                                                   ECS062
00932      IF CP-LF-CNC  IS NUMERIC                                     ECS062
00933        AND  CP-LF-CNC  GREATER  ZERO                              ECS062
00934          MOVE CP-LF-C-MO         TO  P-CP-CMO                     ECS062
00935          MOVE CP-LF-C-DA         TO  P-CP-CDA                     ECS062
00936          MOVE CP-LF-C-YR         TO  P-CP-CYR                     ECS062
00937          MOVE '-'                TO  P-CP-CMOD  P-CP-CDAD         ECS062
00938      ELSE                                                         ECS062
00939          MOVE SPACES             TO  P-CP-CANCEL.                 ECS062
00940                                                                   ECS062
00941      MOVE CP-LF-TERM             TO  P-CP-TERM.                   ECS062
00942                                                                   ECS062
00943      IF DTE-CLIENT  = 'KSM'                                       ECS062
00944          MOVE CP-LF-TERM         TO  BDR-LIFE-TERM                ECS062
00945          IF CP-LF-CNC  IS NUMERIC                                 ECS062
00946            AND  CP-LF-CNC  IS GREATER THAN  ZERO                  ECS062
00947              MOVE CP-LF-C-MO     TO  BDR-LF-CAN-MM                ECS062
00948              MOVE CP-LF-C-DA     TO  BDR-LF-CAN-DD                ECS062
00949              MOVE CP-LF-C-YR     TO  BDR-LF-CAN-YY.               ECS062
00950                                                                   ECS062
00951      PERFORM 2300-FIND-LF  THRU  2399-XIT.                        ECS062
00952                                                                   ECS062
00953      MOVE LIFE-OVERRIDE-L2          TO  P-CP-BEN.                 ECS062
00954      MOVE '-'                       TO  P-CP-BEN-DASH.            ECS062
00955      MOVE CLAS-I-AB3 (CLAS-INDEXL)  TO  P-CP-BEN-TYPE.            ECS062
00956                                                                   ECS062
00957      IF DTE-CLIENT = 'DMD'                                        ECS062
00958          MOVE CLAS-I-BEN (CLAS-INDEXL)  TO  P-CP-BEN-TYPE.        ECS062
00959                                                                   ECS062
00960      IF DTE-CLIENT  = 'KSM'                                       ECS062
00961          MOVE LIFE-OVERRIDE-L2      TO  BDR-LIFE-BEN              ECS062
00962          MOVE CLAS-I-AB3 (CLAS-INDEXL)                            ECS062
00963                                     TO  BDR-LIFE-BEN-TYPE.        ECS062
00964                                                                   ECS062
00965      IF CLAS-I-BAL (CLAS-INDEXL)  = 'B'                           ECS062
00966          MOVE 'OUTSTANDING BAL'  TO  P-CP-NAME.                   ECS062
00967                                                                   ECS062
00968      MOVE C-TT-FACE              TO  P-CP-FACE.                   ECS062
00969      MOVE C-LF-PREM              TO  P-CP-PREMIUM.                ECS062
00970                                                                   ECS062
00971      IF DTE-CLIENT  = 'KSM'                                       ECS062
00972          MOVE C-TT-FACE          TO  BDR-FACE-AMOUNT              ECS062
00973          MOVE C-LF-PREM          TO  BDR-LIFE-PREMIUM             ECS062
00974          IF CLAS-I-BAL (CLAS-INDEXL)  = 'B'                       ECS062
00975              MOVE 'OUTSTANDING BAL'  TO  BDR-NAME.                ECS062
00976                                                                   ECS062
00977      IF C-LF-COMM  = +0                                           ECS062
00978          MOVE +0                 TO  WORK-PERC                    ECS062
00979      ELSE                                                         ECS062
00980          COMPUTE WORK-PERC  =  CP-LPC  *  +100.                   ECS062
00981                                                                   ECS062
00982      IF DTE-CLIENT  =  'KSM'                                      ECS062
00983          IF PRT-TOT-1                                             ECS062
00984              COMPUTE P-CP-TOT-PREM  =  C-LF-PREM                  ECS062
00985                                     +  C-LF-PREM-ALT              ECS062
00986                                     +  C-AH-PREM                  ECS062
00987          ELSE                                                     ECS062
00988              MOVE ZEROS          TO  P-CP-TOT-PREM                ECS062
00989      ELSE                                                         ECS062
00990          MOVE SPACES             TO  P-FILL-1                     ECS062
00991          MOVE WORK-PERC          TO  P-CP-COMP-PCT                ECS062
00992          MOVE SPACE              TO  P-FILL-2.                    ECS062
00993                                                                   ECS062
00994      MOVE C-LF-COMM              TO  P-CP-COMP.                   ECS062
00995                                                                   ECS062
00996      IF CP-RC-ISSUE                                               ECS062
00997        OR  CP-RC-CANCEL                                           ECS062
00998          MOVE SPACES             TO  P-CP-NAME                    ECS062
00999          MOVE '(R)'              TO  P-CP-RECALC-FL               ECS062
01000          MOVE CP-LNAME           TO  P-CP-LNAME-R                 ECS062
01001          MOVE CP-FNAME           TO  P-CP-FNAME.                  ECS062
01002                                                                   ECS062
01003      IF DTE-CLIENT  = 'KSM'                                       ECS062
01004          MOVE C-LF-COMM          TO  BDR-LIFE-COMMISSION          ECS062
01005          IF CP-RC-ISSUE                                           ECS062
01006            OR  CP-RC-CANCEL                                       ECS062
01007              MOVE P-CP-NAME      TO  BDR-LAST-NAME.               ECS062
01008                                                                   ECS062
01009      IF DTE-CLIENT  = 'DMD'                                       ECS062
01010          MOVE CP-STATE               TO  P-CP-UNDRWRTR            ECS062
01011          MOVE SPACE-1                TO  P-CCSW                   ECS062
01012       ELSE                                                        ECS062
01013          MOVE SPACE-2                TO  P-CCSW.                  ECS062
01014                                                                   ECS062
01015      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01016                                                                   ECS062
01017      MOVE SPACES                 TO  P-LINE.                      ECS062
01018                                                                   ECS062
01019  2040-SECOND-BENEFIT.                                             ECS062
01020      IF C-TT-FACE-ALT  =  ZERO                                    ECS062
01021        AND  C-LF-PREM-ALT  = ZERO                                 ECS062
01022        AND  C-LF-COMM-ALT  = ZERO                                 ECS062
01023          IF DTE-CLIENT = 'KSM'                                    ECS062
01024              MOVE ZEROS              TO  BDR-FACE-AMT-ALT         ECS062
01025                                          BDR-LIFE-PREM-ALT        ECS062
01026                                          BDR-LIFE-COMM-ALT        ECS062
01027              GO TO 2050-CHECK-AH                                  ECS062
01028          ELSE                                                     ECS062
01029              GO TO 2050-CHECK-AH.                                 ECS062
01030                                                                   ECS062
01031      MOVE C-TT-FACE-ALT          TO  P-CP-FACE.                   ECS062
01032      MOVE C-LF-PREM-ALT          TO  P-CP-PREMIUM.                ECS062
01033                                                                   ECS062
01034      IF C-LF-COMM-ALT  = +0                                       ECS062
01035          MOVE +0                 TO  WORK-PERC                    ECS062
01036      ELSE                                                         ECS062
01037          COMPUTE WORK-PERC  =  CP-LPC  *  +100.                   ECS062
01038                                                                   ECS062
01039      IF DTE-CLIENT  =  'KSM'                                      ECS062
01040          IF PRT-TOT-2                                             ECS062
01041              COMPUTE P-CP-TOT-PREM  =  C-LF-PREM                  ECS062
01042                                     +  C-LF-PREM-ALT              ECS062
01043                                     +  C-AH-PREM                  ECS062
01044          ELSE                                                     ECS062
01045              MOVE ZEROS          TO  P-CP-TOT-PREM                ECS062
01046      ELSE                                                         ECS062
01047          MOVE SPACES             TO  P-FILL-1                     ECS062
01048          MOVE WORK-PERC          TO  P-CP-COMP-PCT                ECS062
01049          MOVE SPACE              TO  P-FILL-2.                    ECS062
01050                                                                   ECS062
01051      MOVE C-LF-COMM-ALT          TO  P-CP-COMP.                   ECS062
01052                                                                   ECS062
01053      IF DTE-CLIENT  = 'KSM'                                       ECS062
01054          MOVE C-TT-FACE-ALT      TO  BDR-FACE-AMT-ALT             ECS062
01055          MOVE C-LF-PREM-ALT      TO  BDR-LIFE-PREM-ALT            ECS062
01056          MOVE C-LF-COMM-ALT      TO  BDR-LIFE-COMM-ALT.           ECS062
01057                                                                   ECS062
01058      MOVE SPACE-1                TO  P-CCSW.                      ECS062
01059                                                                   ECS062
01060      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01061                                                                   ECS062
01062      MOVE SPACES                 TO  P-LINE.                      ECS062
01063                                                                   ECS062
01064  2050-CHECK-AH.                                                   ECS062
01065      MOVE CP-AH-TYPE             TO  CLAS-LOOK.                   ECS062
01066                                                                   ECS062
01067      IF CLAS-LOOK  = '00'                                         ECS062
01068          IF DTE-CLIENT = 'KSM'                                    ECS062
01069              MOVE ZEROS          TO  BDR-AH-TERM                  ECS062
01070                                      BDR-AH-MO-BENEFIT            ECS062
01071                                      BDR-AH-PREMIUM               ECS062
01072                                      BDR-AH-COMMISSION            ECS062
01073              GO TO 2090-PRE-EXIT                                  ECS062
01074          ELSE                                                     ECS062
01075              GO TO 2090-PRE-EXIT.                                 ECS062
01076                                                                   ECS062
01077      IF CP-AH-CNC  IS NUMERIC                                     ECS062
01078        AND  CP-AH-CNC  GREATER ZERO                               ECS062
01079          MOVE CP-AH-C-MO         TO  P-CP-CMO                     ECS062
01080          MOVE CP-AH-C-DA         TO  P-CP-CDA                     ECS062
01081          MOVE CP-AH-C-YR         TO  P-CP-CYR                     ECS062
01082          MOVE '-'                TO  P-CP-CMOD  P-CP-CDAD         ECS062
01083      ELSE                                                         ECS062
01084          MOVE SPACES             TO  P-CP-CANCEL.                 ECS062
01085                                                                   ECS062
01086      MOVE CP-AH-TERM             TO  P-CP-TERM.                   ECS062
01087                                                                   ECS062
01088      IF DTE-CLIENT  = 'KSM'                                       ECS062
01089          MOVE CP-AH-TERM         TO  BDR-AH-TERM                  ECS062
01090          IF CP-AH-CNC  IS NUMERIC                                 ECS062
01091            AND  CP-AH-CNC  GREATER ZERO                           ECS062
01092              MOVE CP-AH-C-MO     TO  BDR-AH-CAN-MM                ECS062
01093              MOVE CP-AH-C-DA     TO  BDR-AH-CAN-DD                ECS062
01094              MOVE CP-AH-C-YR     TO  BDR-AH-CAN-YY.               ECS062
01095                                                                   ECS062
01096      PERFORM 2200-FIND-AH  THRU  2299-XIT.                        ECS062
01097                                                                   ECS062
01098      MOVE AH-OVERRIDE-L2            TO  P-CP-BEN.                 ECS062
01099      MOVE '-'                       TO  P-CP-BEN-DASH.            ECS062
01100      MOVE CLAS-I-AB3 (CLAS-INDEXA)  TO  P-CP-BEN-TYPE.            ECS062
01101                                                                   ECS062
01102      IF DTE-CLIENT  = 'KSM'                                       ECS062
01103          MOVE AH-OVERRIDE-L2        TO  BDR-AH-BEN                ECS062
01104          MOVE CLAS-I-AB3 (CLAS-INDEXA)                            ECS062
01105                                     TO  BDR-AH-BEN-TYPE.          ECS062
01106                                                                   ECS062
01107      IF CLAS-I-BAL (CLAS-INDEXA)  = 'B'                           ECS062
01108          MOVE 'OUTSTANDING BAL'  TO  P-CP-NAME.                   ECS062
01109                                                                   ECS062
01110      IF DTE-CLIENT = 'DMD'                                        ECS062
01111          MOVE CLAS-I-BEN (CLAS-INDEXA)  TO  P-CP-BEN-TYPE         ECS062
01112          IF CP-RC-ISSUE OR                                        ECS062
01113             CP-RC-CANCEL                                          ECS062
01114          MOVE SPACES             TO  P-CP-NAME                    ECS062
01115          MOVE '(R)'              TO  P-CP-RECALC-FL               ECS062
01116          MOVE CP-LNAME           TO  P-CP-LNAME-R                 ECS062
01117          MOVE CP-FNAME           TO  P-CP-FNAME.                  ECS062
01118                                                                   ECS062
01119      IF DTE-CLIENT = 'DMD'                                        ECS062
01120          MOVE CP-STATE           TO  P-CP-UNDRWRTR.               ECS062
01121                                                                   ECS062
01122      IF DTE-CLIENT  = 'KSM'                                       ECS062
01123          IF CLAS-I-BAL (CLAS-INDEXA)  = 'B'                       ECS062
01124              MOVE 'OUTSTANDING BAL'  TO  BDR-NAME.                ECS062
01125                                                                   ECS062
01126      MOVE C-AH-PREM              TO  P-CP-PREMIUM.                ECS062
01127                                                                   ECS062
01128      IF C-AH-COMM  = +0                                           ECS062
01129          MOVE +0                 TO  WORK-PERC                    ECS062
01130      ELSE                                                         ECS062
01131          COMPUTE WORK-PERC  =  CP-APC  *  +100.                   ECS062
01132                                                                   ECS062
01133      IF DTE-CLIENT  =  'KSM'                                      ECS062
01134          IF PRT-TOT-3                                             ECS062
01135              COMPUTE P-CP-TOT-PREM  =  C-LF-PREM                  ECS062
01136                                     +  C-LF-PREM-ALT              ECS062
01137                                     +  C-AH-PREM                  ECS062
01138          ELSE                                                     ECS062
01139              MOVE ZEROS          TO  P-CP-TOT-PREM                ECS062
01140      ELSE                                                         ECS062
01141          MOVE SPACES             TO  P-FILL-1                     ECS062
01142          MOVE WORK-PERC          TO  P-CP-COMP-PCT                ECS062
01143          MOVE SPACE              TO  P-FILL-2.                    ECS062
01144                                                                   ECS062
01145      MOVE C-AH-COMM              TO  P-CP-COMP.                   ECS062
01146      MOVE C-TT-AH-BEN            TO  P-CP-FACE.                   ECS062
01147                                                                   ECS062
01148      IF DTE-CLIENT  = 'KSM'                                       ECS062
01149          MOVE C-AH-PREM          TO  BDR-AH-PREMIUM               ECS062
01150          MOVE C-AH-COMM          TO  BDR-AH-COMMISSION            ECS062
01151          MOVE C-TT-AH-BEN        TO  BDR-AH-MO-BENEFIT.           ECS062
01152                                                                   ECS062
01153      IF AH-ONLY                                                   ECS062
01154          MOVE 'N'                TO  AH-ONLY-SWITCH               ECS062
01155          MOVE SPACE-2            TO  P-CCSW                       ECS062
01156      ELSE                                                         ECS062
01157          MOVE SPACE-1            TO  P-CCSW.                      ECS062
01158                                                                   ECS062
01159      IF DTE-CLIENT = 'DMD'                                        ECS062
01160          MOVE SPACE-1            TO  P-CCSW.                      ECS062
01161                                                                   ECS062
01162      GO TO 2080-PRT-DETAIL.                                       ECS062
01163                                                                   ECS062
01164  2060-PRT-ACCTG.                                                  ECS062
01165      MOVE CP-AC-DESC              TO  P-CP-DESC.                  ECS062
01166      MOVE C-TT-PYMT               TO  P-CP-PYMT.                  ECS062
01167      MOVE '* ACCOUNTING ENTRY *'  TO  P-ACCTG-COMMENT.            ECS062
01168      MOVE SPACE-1                 TO  P-CCSW.                     ECS062
01169                                                                   ECS062
01170      GO TO 2080-PRT-DETAIL.                                       ECS062
01171                                                                   ECS062
01172  2070-PRT-CLAIM.                                                  ECS062
01173      IF FIRST-CLAIM                                               ECS062
01174          PERFORM 2400-PRT-TOTAL-RTN  THRU  2499-EXIT              ECS062
01175      ELSE                                                         ECS062
01176          MOVE SPACE-1            TO  P-CCSW.                      ECS062
01177                                                                   ECS062
01178      IF DTE-CLIENT  = 'KSM'                                       ECS062
01179          IF LNCTR  GREATER +45                                    ECS062
01180              PERFORM 2900-PRT-STMT-HD-RTN  THRU  2999-EXIT        ECS062
01181          ELSE                                                     ECS062
01182              NEXT SENTENCE                                        ECS062
01183      ELSE                                                         ECS062
01184          IF LNCTR  GREATER +30                                    ECS062
01185              PERFORM 2900-PRT-STMT-HD-RTN  THRU  2999-EXIT.       ECS062
01186                                                                   ECS062
01187      MOVE CP-FNAME               TO  P-CP-FNAME.                  ECS062
01188                                                                   ECS062
01189      MOVE CP-INITIAL             TO  P-CP-INITIAL.                ECS062
01190                                                                   ECS062
01191      IF CP-INITIAL  NOT = SPACE                                   ECS062
01192          MOVE '.'                TO  P-INIT-DOT                   ECS062
01193      ELSE                                                         ECS062
01194          MOVE ' '                TO  P-INIT-DOT.                  ECS062
01195                                                                   ECS062
01196  2070-PRT-CONTINUE.                                               ECS062
01197      MOVE CP-LNAME               TO  P-CP-LNAME.                  ECS062
01198      MOVE CP-CERT                TO  P-CP-CERT.                   ECS062
01199      MOVE CP-MO                  TO  P-CP-EMO.                    ECS062
01200      MOVE CP-DA                  TO  P-CP-EDA.                    ECS062
01201      MOVE CP-YR                  TO  P-CP-EYR.                    ECS062
01202      MOVE '-'                    TO  P-CP-EMOD  P-CP-EDAD.        ECS062
01203                                                                   ECS062
01204      IF C-LF-CLM  = +0                                            ECS062
01205          MOVE 'A&H CLAIM PAID'    TO  P-CLM-MESSAGE               ECS062
01206          MOVE C-AH-CLM            TO  P-CLM-PYMT                  ECS062
01207      ELSE                                                         ECS062
01208          MOVE 'LIFE CLAIM PAID'   TO  P-CLM-MESSAGE               ECS062
01209          MOVE C-LF-CLM            TO  P-CLM-PYMT.                 ECS062
01210                                                                   ECS062
01211  2080-PRT-DETAIL.                                                 ECS062
01212      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01213                                                                   ECS062
01214      IF SAVE-CP-EFF = 0                                           ECS062
01215          MOVE CP-EFF             TO SAVE-CP-EFF.                  ECS062
01216                                                                   ECS062
01217      IF CP-CLAIM                                                  ECS062
01218        OR CP-ACCTG                                                ECS062
01219          GO TO 2099-EXIT.                                         ECS062
01220                                                                   ECS062
01221  2090-PRE-EXIT.                                                   ECS062
01222      IF DTE-CLIENT  =  'KSM'                                      ECS062
01223          MOVE TOT-DUE-MONTH      TO  BSR-TOTAL-DUE                ECS062
01224          MOVE 'D'                TO  BDR-RECORD-TYPE              ECS062
01225          MOVE SAV-KSM-CARR-GROUP TO  BDR-CARR-GROUP               ECS062
01226          MOVE SAV-KSM-ACCOUNT    TO  BDR-ACCOUNT                  ECS062
01227          MOVE SAV-KSM-ACCT-NAME  TO  BDR-ACCT-NAME                ECS062
01228          MOVE SAV-KSM-ADDR-1     TO  BDR-ADDR-1                   ECS062
01229          MOVE SAV-KSM-ADDR-2     TO  BDR-ADDR-2                   ECS062
01230          MOVE SAV-KSM-CITY-STATE TO  BDR-CITY-STATE               ECS062
01231          MOVE SAV-KSM-ZIP        TO  BDR-ZIP                      ECS062
01232          MOVE SAV-KSM-DATE       TO  BDR-DATE                     ECS062
01233          IF BDR-CARRIER NOT EQUAL '3'                             ECS062
01234             PERFORM 2600-WRITE-BILLING-DET THRU 2699-EXIT.        ECS062
01235                                                                   ECS062
01236  2099-EXIT.                                                       ECS062
01237      EXIT.                                                        ECS062
01238  EJECT                                                            ECS062
01239  2200-FIND-AH.                                                    ECS062
01240      IF CLAS-STARTA  = ZERO                                       ECS062
01241          MOVE '0402'             TO  WS-RETURN-CODE               ECS062
01242          MOVE 'INVALID A&H TYPE'                                  ECS062
01243                                  TO  WS-ABEND-MESSAGE             ECS062
01244          GO TO ABEND-PGM.                                         ECS062
01245                                                                   ECS062
01246      MOVE CLAS-STARTA            TO  CLAS-INDEXA.                 ECS062
01247                                                                   ECS062
01248  2210-AH-LOOP.                                                    ECS062
01249      IF CLAS-INDEXA  GREATER  CLAS-MAXA                           ECS062
01250          DISPLAY 'AH BENEFIT ' CLAS-LOOK ' NOT ON FILE'           ECS062
01251          MOVE '0402'             TO  WS-RETURN-CODE               ECS062
01252          MOVE 'INVALID A&H TYPE'                                  ECS062
01253                                  TO  WS-ABEND-MESSAGE             ECS062
01254          GO TO ABEND-PGM.                                         ECS062
01255                                                                   ECS062
01256      IF CLAS-I-BEN (CLAS-INDEXA)  NOT = CLAS-LOOK                 ECS062
01257          ADD +1                  TO  CLAS-INDEXA                  ECS062
01258          GO TO 2210-AH-LOOP.                                      ECS062
01259                                                                   ECS062
01260  2299-XIT.                                                        ECS062
01261      EXIT.                                                        ECS062
01262                                                                   ECS062
01263  2300-FIND-LF.                                                    ECS062
01264      IF CLAS-STARTL  = ZERO                                       ECS062
01265          MOVE '0401'             TO  WS-RETURN-CODE               ECS062
01266          MOVE 'INVALID LIFE TYPE'                                 ECS062
01267                                  TO  WS-ABEND-MESSAGE             ECS062
01268          GO TO ABEND-PGM.                                         ECS062
01269                                                                   ECS062
01270      MOVE CLAS-STARTL            TO  CLAS-INDEXL.                 ECS062
01271                                                                   ECS062
01272  2310-LF-LOOP.                                                    ECS062
01273      IF CLAS-INDEXL  GREATER  CLAS-MAXL                           ECS062
01274          DISPLAY 'LIFE BENEFIT ' CLAS-LOOK ' NOT IN TABLE'        ECS062
01275          MOVE '0401'             TO  WS-RETURN-CODE               ECS062
01276          MOVE 'INVALID LIFE TYPE'                                 ECS062
01277                                  TO  WS-ABEND-MESSAGE             ECS062
01278          GO TO ABEND-PGM.                                         ECS062
01279                                                                   ECS062
01280      IF CLAS-I-BEN (CLAS-INDEXL)  NOT = CLAS-LOOK                 ECS062
01281          ADD +1                  TO  CLAS-INDEXL                  ECS062
01282          GO TO 2310-LF-LOOP.                                      ECS062
01283                                                                   ECS062
01284  2399-XIT.                                                        ECS062
01285      EXIT.                                                        ECS062
01286  EJECT                                                            ECS062
01287  2400-PRT-TOTAL-RTN.                                              ECS062
01288      IF ALREADY-PRINTED-TOTALS                                    ECS062
01289          GO TO 2499-EXIT.                                         ECS062
01290                                                                   ECS062
01291      IF DTE-CLIENT = 'DMD'                                        ECS062
01292          IF PREV-ISSUE-CANCEL                                     ECS062
01293              PERFORM 1500-CHECK-EFF-CHANGE THRU 1500-EXIT.        ECS062
01294                                                                   ECS062
01295      MOVE 'N'                    TO  FIRST-CLAIM-SW.              ECS062
01296                                                                   ECS062
01297      PERFORM 3200-CALC-TOTAL-RTN  THRU  3299-EXIT.                ECS062
01298                                                                   ECS062
01299      IF STMT-SW  = +0                                             ECS062
01300          GO TO 2499-EXIT.                                         ECS062
01301                                                                   ECS062
01302      IF DTE-CLIENT  = 'KSM'                                       ECS062
01303          IF LNCTR  GREATER +35                                    ECS062
01304              PERFORM 2900-PRT-STMT-HD-RTN  THRU  2999-EXIT        ECS062
01305          ELSE                                                     ECS062
01306              NEXT SENTENCE                                        ECS062
01307      ELSE                                                         ECS062
01308          IF LNCTR  GREATER +20  OR                                ECS062
01309             DTE-CLIENT  = 'DMD'                                   ECS062
01310              PERFORM 2900-PRT-STMT-HD-RTN  THRU  2999-EXIT.       ECS062
01311                                                                   ECS062
01312      MOVE LIFE-OVERRIDE-L6       TO  PP-TOT-TITLE.                ECS062
01313      MOVE PP-TOT-DESC            TO  P-TOT-DESC.                  ECS062
01314      MOVE S-LF-PREM              TO  P-TOT-PREMIUM.               ECS062
01315      MOVE S-LF-COMM              TO  P-TOT-COMP.                  ECS062
01316      MOVE S-TT-FACE              TO  P-TOT-FACE.                  ECS062
01317                                                                   ECS062
01318      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01319                                                                   ECS062
01320      MOVE AH-OVERRIDE-L6         TO  PP-TOT-TITLE.                ECS062
01321      MOVE PP-TOT-DESC            TO  P-TOT-DESC.                  ECS062
01322      MOVE S-AH-PREM              TO  P-TOT-PREMIUM.               ECS062
01323      MOVE S-AH-COMM              TO  P-TOT-COMP.                  ECS062
01324      MOVE S-TT-AH-BEN            TO  P-TOT-FACE.                  ECS062
01325                                                                   ECS062
01326      IF DTE-CLIENT  =  'KSM'                                      ECS062
01327          MOVE S-LF-PREM          TO  BSR-NET-LIFE-PREM            ECS062
01328          MOVE S-LF-COMM          TO  BSR-LIFE-COMM                ECS062
01329          MOVE S-AH-PREM          TO  BSR-NET-AH-PREM              ECS062
01330          MOVE S-AH-COMM          TO  BSR-AH-COMM                  ECS062
01331          MOVE S-ISS-AH-CNT       TO  BSR-ISS-AH-CNT               ECS062
01332          MOVE S-ISS-LF-CNT       TO  BSR-ISS-LF-CNT               ECS062
01333          MOVE S-CAN-AH-CNT       TO  BSR-CAN-AH-CNT               ECS062
01334          MOVE S-CAN-LF-CNT       TO  BSR-CAN-LF-CNT               ECS062
01335          MOVE S-CAN-PREM-91      TO  BSR-CAN-PREM-91              ECS062
01336          MOVE S-LO-CERT-DATE     TO  BSR-LO-CERT-DATE             ECS062
01337          MOVE S-HI-CERT-DATE     TO  BSR-HI-CERT-DATE             ECS062
01338          MOVE S-ACCT-STATUS      TO  BSR-ACCT-STATUS              ECS062
01339          MOVE S-REI-TABLE        TO  BSR-REI-TABLE.               ECS062
01340                                                                   ECS062
01341      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01342                                                                   ECS062
01343      MOVE 'TOTAL PREMIUM'        TO  P-TOT-DESC.                  ECS062
01344      MOVE S-TT-PREM              TO  P-TOT-PREMIUM.               ECS062
01345      MOVE S-TT-COMM              TO  P-TOT-COMP.                  ECS062
01346      MOVE ZEROS                  TO  P-TOT-FACE.                  ECS062
01347                                                                   ECS062
01348      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01349                                                                   ECS062
01350      IF DTE-CLIENT  = 'KSM'                                       ECS062
01351          IF LNCTR  GREATER +35                                    ECS062
01352              PERFORM 2900-PRT-STMT-HD-RTN  THRU  2999-EXIT        ECS062
01353          ELSE                                                     ECS062
01354              NEXT SENTENCE                                        ECS062
01355      ELSE                                                         ECS062
01356          IF LNCTR  GREATER +20                                    ECS062
01357              PERFORM 2900-PRT-STMT-HD-RTN  THRU  2999-EXIT.       ECS062
01358                                                                   ECS062
01359      MOVE SPACE-2                TO  P-CCSW.                      ECS062
01360                                                                   ECS062
01361  2410-PRT-BAL-FWD.                                                ECS062
01362      IF DTE-CLIENT  =  'KSM'                                      ECS062
01363          GO TO 2490-PRINT-KSM-TOTALS.                             ECS062
01364                                                                   ECS062
01365      IF S-BEG-BAL  =  +0                                          ECS062
01366          GO TO 2420-PRT-TOT-PRM.                                  ECS062
01367                                                                   ECS062
01368      MOVE 'BALANCE FORWARD'      TO  P-TOT-DESC1.                 ECS062
01369      MOVE S-BEG-BAL              TO  P-TOT-PREMIUM1.              ECS062
01370      MOVE SPACE-3                TO  P-CCSW.                      ECS062
01371                                                                   ECS062
01372      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01373                                                                   ECS062
01374  2420-PRT-TOT-PRM.                                                ECS062
01375      IF S-TT-PREM  = +0                                           ECS062
01376          GO TO 2430-PRT-TOT-COMM.                                 ECS062
01377                                                                   ECS062
01378      MOVE 'THIS MONTHS PREMIUM'  TO  P-TOT-DESC1.                 ECS062
01379      MOVE S-TT-PREM              TO  P-TOT-PREMIUM1.              ECS062
01380      MOVE SPACE-2                TO  P-CCSW.                      ECS062
01381                                                                   ECS062
01382      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01383                                                                   ECS062
01384  2430-PRT-TOT-COMM.                                               ECS062
01385      IF S-TT-COMM  = +0                                           ECS062
01386          GO TO 2440-PRT-TOT-PMT.                                  ECS062
01387                                                                   ECS062
01388      MOVE 'LESS COMPENSATION'    TO  P-TOT-DESC1.                 ECS062
01389      MOVE S-TT-COMM              TO  P-TOT-PREMIUM1.              ECS062
01390      MOVE SPACE-2                TO  P-CCSW.                      ECS062
01391                                                                   ECS062
01392      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01393                                                                   ECS062
01394  2440-PRT-TOT-PMT.                                                ECS062
01395      IF S-TT-PYMT  = +0                                           ECS062
01396          GO TO 2450-PRT-WRT-OFF.                                  ECS062
01397                                                                   ECS062
01398      MOVE 'LESS PAYMENTS'        TO  P-TOT-DESC1.                 ECS062
01399      MOVE S-TT-PYMT              TO  P-TOT-PREMIUM1.              ECS062
01400      MOVE SPACE-2                TO  P-CCSW.                      ECS062
01401                                                                   ECS062
01402      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01403                                                                   ECS062
01404  2450-PRT-WRT-OFF.                                                ECS062
01405      IF S-WRT-OFF  = +0                                           ECS062
01406          GO TO 2460-PRT-TOTAL.                                    ECS062
01407                                                                   ECS062
01408      MOVE 'ADJUSTMENT'           TO  P-TOT-DESC1.                 ECS062
01409      MOVE S-WRT-OFF              TO  P-TOT-PREMIUM1.              ECS062
01410      MOVE SPACE-2                TO  P-CCSW.                      ECS062
01411                                                                   ECS062
01412      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01413                                                                   ECS062
01414  2460-PRT-TOTAL.                                                  ECS062
01415      MOVE ALL '-'                TO  P-TOT-PREM-X1.               ECS062
01416      MOVE SPACE-2                TO  P-CCSW.                      ECS062
01417                                                                   ECS062
01418      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01419                                                                   ECS062
01420      IF DTE-CLIENT  =  'KSM'                                      ECS062
01421          IF LNCTR  GREATER +35                                    ECS062
01422              PERFORM 2900-PRT-STMT-HD-RTN  THRU  2999-EXIT        ECS062
01423          ELSE                                                     ECS062
01424              NEXT SENTENCE                                        ECS062
01425      ELSE                                                         ECS062
01426          IF LNCTR  GREATER +20                                    ECS062
01427              PERFORM 2900-PRT-STMT-HD-RTN  THRU  2999-EXIT.       ECS062
01428                                                                   ECS062
01429      MOVE 'PLEASE REMIT'         TO  P-TOT-DESC1.                 ECS062
01430      MOVE S-END-BAL              TO  P-TOT-PREMIUM1.              ECS062
01431                                                                   ECS062
01432      IF S-END-BAL  LESS ZERO                                      ECS062
01433          MOVE 'WE WILL REFUND'   TO  P-TOT-DESC1.                 ECS062
01434                                                                   ECS062
01435      IF S-END-BAL  LESS +1.00                                     ECS062
01436        AND  S-END-BAL  GREATER  -1.00                             ECS062
01437          MOVE SPACES             TO  P-TOT-DESC1.                 ECS062
01438                                                                   ECS062
01439      MOVE SPACE-2                TO  P-CCSW.                      ECS062
01440                                                                   ECS062
01441      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01442                                                                   ECS062
01443      IF DTE-CLIENT  = 'LOK'  OR  'MNL'                            ECS062
01444          GO TO 2499-EXIT.                                         ECS062
01445                                                                   ECS062
01446      IF CO-CARRY-BALANCE                                          ECS062
01447          IF S-END-BAL  GREATER  +1.00                             ECS062
01448              MOVE '  PLEASE RETURN ONE COPY WITH REMITTANCE '     ECS062
01449                                  TO  P-LINE                       ECS062
01450              MOVE SPACE-2        TO  P-CCSW                       ECS062
01451              PERFORM 8800-PRT-RTN  THRU  8899-EXIT.               ECS062
01452                                                                   ECS062
01453      GO TO 2499-EXIT.                                             ECS062
01454                                                                   ECS062
01455  2480-REFUND.                                                     ECS062
01456      MOVE 'WE WILL REFUND'       TO  P-TOT-DESC1.                 ECS062
01457                                                                   ECS062
01458      IF S-END-BAL  LESS  +1.00                                    ECS062
01459        AND  S-END-BAL  GREATER  -1.00                             ECS062
01460          MOVE SPACES             TO  P-TOT-DESC1.                 ECS062
01461                                                                   ECS062
01462      MOVE SPACE-2                TO  P-CCSW.                      ECS062
01463                                                                   ECS062
01464      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01465                                                                   ECS062
01466      GO TO 2499-EXIT.                                             ECS062
01467                                                                   ECS062
01468  2490-PRINT-KSM-TOTALS.                                           ECS062
01469      IF DTE-CLIENT  =  'KSM'                                      ECS062
01470          IF LNCTR  GREATER +30                                    ECS062
01471              PERFORM 2900-PRT-STMT-HD-RTN  THRU  2999-EXIT.       ECS062
01472                                                                   ECS062
01473      MOVE 'THIS MONTHS PREMIUM'  TO  P-TOT-DESC1.                 ECS062
01474      MOVE S-TT-PREM              TO  P-TOT-PREMIUM1.              ECS062
01475      MOVE SPACE-3                TO  P-CCSW.                      ECS062
01476                                                                   ECS062
01477      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01478                                                                   ECS062
01479      MOVE 'LESS COMPENSATION'    TO  P-TOT-DESC1.                 ECS062
01480      MOVE S-TT-COMM              TO  P-TOT-PREMIUM1.              ECS062
01481      MOVE SPACE-2                TO  P-CCSW.                      ECS062
01482                                                                   ECS062
01483      IF DTE-CLIENT  =  'KSM'                                      ECS062
01484          MOVE S-TT-COMM          TO  BSR-TOTAL-COMM               ECS062
01485          MOVE S-TT-PREM          TO  BSR-TOTAL-PREM.              ECS062
01486                                                                   ECS062
01487      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01488                                                                   ECS062
01489      IF DTE-CLIENT = 'KSM'                                        ECS062
01490          MOVE ALL '-'            TO  P-TOT-PREM-X1                ECS062
01491          MOVE SPACE-2            TO  P-CCSW                       ECS062
01492          PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                   ECS062
01493                                                                   ECS062
01494      COMPUTE TOT-DUE-MONTH = S-TT-PREM - S-TT-COMM.               ECS062
01495                                                                   ECS062
01496      IF DTE-CLIENT = 'KSM'                                        ECS062
01497          MOVE 'DUE THIS MONTH'   TO  P-TOT-DESC1                  ECS062
01498      ELSE                                                         ECS062
01499          MOVE 'TOTAL DUE THIS MONTH'                              ECS062
01500                                  TO  P-TOT-DESC1.                 ECS062
01501                                                                   ECS062
01502      MOVE TOT-DUE-MONTH          TO  P-TOT-PREMIUM1.              ECS062
01503      MOVE SPACE-2                TO  P-CCSW.                      ECS062
01504                                                                   ECS062
01505      IF DTE-CLIENT  =  'KSM'                                      ECS062
01506          MOVE TOT-DUE-MONTH      TO  BSR-TOTAL-DUE                ECS062
01507          MOVE 'S'                TO  BSR-RECORD-TYPE              ECS062
01508          MOVE SAV-KSM-CARR-GROUP TO  BSR-CARR-GROUP               ECS062
01509          MOVE SAV-KSM-ACCOUNT    TO  BSR-ACCOUNT                  ECS062
01510          MOVE SAV-KSM-ACCT-NAME  TO  BSR-ACCT-NAME                ECS062
01511          MOVE SAV-KSM-ADDR-1     TO  BSR-ADDR-1                   ECS062
01512          MOVE SAV-KSM-ADDR-2     TO  BSR-ADDR-2                   ECS062
01513          MOVE SAV-KSM-CITY-STATE TO  BSR-CITY-STATE               ECS062
01514          MOVE SAV-KSM-ZIP        TO  BSR-ZIP                      ECS062
01515          MOVE SAV-KSM-DATE       TO  BSR-DATE                     ECS062
01516          PERFORM 2500-WRITE-BILLING-SUM THRU 2599-EXIT.           ECS062
01517                                                                   ECS062
01518      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01519                                                                   ECS062
01520      COMPUTE TOT-UNPAID-BAL = S-BEG-BAL - S-TT-PYMT.              ECS062
01521                                                                   ECS062
01522      IF DTE-CLIENT = 'KSM'                                        ECS062
01523          MOVE 'PREVIOUS UNPAID BALANCE'                           ECS062
01524                                  TO  P-TOT-DESC1                  ECS062
01525      ELSE                                                         ECS062
01526          MOVE 'UNPAID BALANCE:'  TO  P-TOT-DESC1.                 ECS062
01527                                                                   ECS062
01528      MOVE SPACE-2                TO  P-CCSW.                      ECS062
01529                                                                   ECS062
01530      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01531                                                                   ECS062
01532      MOVE 'DISREGARD IF PAID'    TO  P-TOT-DESC1.                 ECS062
01533      MOVE TOT-UNPAID-BAL         TO  P-TOT-PREMIUM1.              ECS062
01534      MOVE SPACE-1                TO  P-CCSW.                      ECS062
01535                                                                   ECS062
01536      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01537                                                                   ECS062
01538      MOVE ALL '-'                TO  P-TOT-PREM-X1.               ECS062
01539      MOVE SPACE-2                TO  P-CCSW.                      ECS062
01540                                                                   ECS062
01541      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01542                                                                   ECS062
01543      IF DTE-CLIENT  =  'KSM'                                      ECS062
01544          NEXT SENTENCE                                            ECS062
01545      ELSE                                                         ECS062
01546          IF LNCTR  GREATER +20                                    ECS062
01547              PERFORM 2900-PRT-STMT-HD-RTN  THRU  2999-EXIT.       ECS062
01548                                                                   ECS062
01549      COMPUTE TOT-TOTAL-DUE = TOT-DUE-MONTH + TOT-UNPAID-BAL.      ECS062
01550                                                                   ECS062
01551      IF DTE-CLIENT = 'KSM'                                        ECS062
01552          MOVE 'TOTAL DUE'        TO  P-TOT-DESC1                  ECS062
01553      ELSE                                                         ECS062
01554          MOVE 'GRAND TOTAL DUE'  TO  P-TOT-DESC1.                 ECS062
01555                                                                   ECS062
01556      MOVE TOT-TOTAL-DUE          TO  P-TOT-PREMIUM1.              ECS062
01557      MOVE SPACE-2                TO  P-CCSW.                      ECS062
01558                                                                   ECS062
01559      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01560                                                                   ECS062
01561      MOVE SPACES                 TO  P-REC.                       ECS062
01562                                                                   ECS062
01563      IF DTE-CLIENT = 'KSM'                                        ECS062
01564          MOVE SPACE-2            TO  P-CCSW                       ECS062
01565          PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                   ECS062
01566                                                                   ECS062
01567      IF DTE-CLIENT = 'KSM'                                        ECS062
01568          MOVE KSM-SPEC-LINE5-1   TO  P-TOT-DESC1-KSM              ECS062
01569      ELSE                                                         ECS062
01570          MOVE 'PLEASE PAY AS INVOICED'                            ECS062
01571                                  TO  P-TOT-DESC1.                 ECS062
01572                                                                   ECS062
01573      IF DTE-CLIENT = 'KSM'                                        ECS062
01574          MOVE SPACE-2            TO  P-CCSW                       ECS062
01575          PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                   ECS062
01576                                                                   ECS062
01577      MOVE SPACE-2                TO  P-CCSW.                      ECS062
01578                                                                   ECS062
01579      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01580                                                                   ECS062
01581      IF DTE-CLIENT = 'KSM'                                        ECS062
01582          MOVE SPACES             TO  P-REC                        ECS062
01583          MOVE SPACE-1            TO  P-CCSW                       ECS062
01584          PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                   ECS062
01585                                                                   ECS062
01586      IF DTE-CLIENT = 'KSM'                                        ECS062
01587          MOVE KSM-SPEC-LINE5-2   TO  P-TOT-DESC1-KSM              ECS062
01588      ELSE                                                         ECS062
01589          MOVE 'ALL CERTIFICATE TRANSACTIONS'                      ECS062
01590                                  TO  P-TOT-DESC1.                 ECS062
01591                                                                   ECS062
01592      MOVE SPACE-1                TO  P-CCSW.                      ECS062
01593                                                                   ECS062
01594      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01595                                                                   ECS062
01596      IF DTE-CLIENT = 'KSM'                                        ECS062
01597          MOVE KSM-SPEC-LINE5-3   TO  P-TOT-DESC1-KSM              ECS062
01598      ELSE                                                         ECS062
01599          MOVE 'NOT APPEARING ON THIS'                             ECS062
01600                                  TO  P-TOT-DESC1.                 ECS062
01601                                                                   ECS062
01602      MOVE SPACE-1                TO  P-CCSW.                      ECS062
01603                                                                   ECS062
01604      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01605                                                                   ECS062
01606      IF DTE-CLIENT = 'KSM'                                        ECS062
01607          MOVE KSM-SPEC-LINE5-4   TO  P-TOT-DESC1-KSM              ECS062
01608      ELSE                                                         ECS062
01609          MOVE 'STATEMENT WILL APPEAR'                             ECS062
01610                                  TO  P-TOT-DESC1.                 ECS062
01611                                                                   ECS062
01612      MOVE SPACE-1                TO  P-CCSW.                      ECS062
01613                                                                   ECS062
01614      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01615                                                                   ECS062
01616      IF DTE-CLIENT = 'KSM'                                        ECS062
01617          MOVE SPACES             TO  P-TOT-DESC1-KSM              ECS062
01618      ELSE                                                         ECS062
01619          MOVE 'ON THE NEXT BILLING REPORT'                        ECS062
01620                                  TO  P-TOT-DESC1.                 ECS062
01621                                                                   ECS062
01622      MOVE SPACE-1                TO  P-CCSW.                      ECS062
01623                                                                   ECS062
01624      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01625                                                                   ECS062
01626      IF DTE-CLIENT = 'KSM'                                        ECS062
01627          MOVE KSM-SPEC-LINE5-5   TO  P-PAY-KSM-1                  ECS062
01628          MOVE SAVE-COMPANY-NAME (3)                               ECS062
01629                                  TO  P-PAY-KSM-2                  ECS062
01630          MOVE SPACE-1            TO  P-CCSW                       ECS062
01631          PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                   ECS062
01632                                                                   ECS062
01633  2499-EXIT.                                                       ECS062
01634      EXIT.                                                        ECS062
01635  EJECT                                                            ECS062
01636  2500-WRITE-BILLING-SUM.                                          ECS062
01637      IF ZERO = BSR-NET-LIFE-PREM      AND                         ECS062
01638                BSR-LIFE-COMM          AND                         ECS062
01639                BSR-NET-AH-PREM        AND                         ECS062
01640                BSR-AH-COMM            AND                         ECS062
01641                BSR-TOTAL-PREM         AND                         ECS062
01642                BSR-TOTAL-COMM         AND                         ECS062
01643                BSR-TOTAL-DUE          AND                         ECS062
01644                BSR-GROSS-LIFE-PREM    AND                         ECS062
01645                BSR-GROSS-AH-PREM      AND                         ECS062
01646                BSR-GROSS-LIFE-REFUNDS AND                         ECS062
01647                BSR-GROSS-AH-REFUNDS   AND                         ECS062
01648                BSR-GROSS-WRITTEN      AND                         ECS062
01649                BSR-GROSS-REFUNDED                                 ECS062
01650          GO TO 2510-CLEAR-BILLING-SUM-RECORD.                     ECS062
01651                                                                   ECS062
01652      IF BSR-CARRIER EQUAL '3'                                     ECS062
01653         GO TO 2510-CLEAR-BILLING-SUM-RECORD.                      ECS062
01654                                                                   ECS062
01655      COMPUTE KSM-NET-LIFE-PREM = KSM-NET-LIFE-PREM                ECS062
01656                                + BSR-NET-LIFE-PREM.               ECS062
01657      COMPUTE KSM-LIFE-COMM = KSM-LIFE-COMM + BSR-LIFE-COMM.       ECS062
01658      COMPUTE KSM-NET-AH-PREM = KSM-NET-AH-PREM + BSR-NET-AH-PREM. ECS062
01659      COMPUTE KSM-AH-COMM = KSM-AH-COMM + BSR-AH-COMM.             ECS062
01660      COMPUTE KSM-TOTAL-PREM = KSM-TOTAL-PREM + BSR-TOTAL-PREM.    ECS062
01661      COMPUTE KSM-TOTAL-COMM = KSM-TOTAL-COMM + BSR-TOTAL-COMM.    ECS062
01662      COMPUTE KSM-TOTAL-DUE = KSM-TOTAL-DUE + BSR-TOTAL-DUE.       ECS062
01663      COMPUTE KSM-GROSS-LIFE-PREM = KSM-GROSS-LIFE-PREM            ECS062
01664                                  + BSR-GROSS-LIFE-PREM.           ECS062
01665      COMPUTE KSM-GROSS-AH-PREM = KSM-GROSS-AH-PREM                ECS062
01666                                + BSR-GROSS-AH-PREM.               ECS062
01667      COMPUTE KSM-GROSS-LIFE-REFUNDS = KSM-GROSS-LIFE-REFUNDS      ECS062
01668                                     + BSR-GROSS-LIFE-REFUNDS.     ECS062
01669      COMPUTE KSM-GROSS-AH-REFUNDS = KSM-GROSS-AH-REFUNDS          ECS062
01670                                   + BSR-GROSS-AH-REFUNDS.         ECS062
01671      COMPUTE KSM-GROSS-WRITTEN = KSM-GROSS-WRITTEN                ECS062
01672                                + BSR-GROSS-WRITTEN.               ECS062
01673      COMPUTE KSM-GROSS-REFUNDED = KSM-GROSS-REFUNDED              ECS062
01674                                 + BSR-GROSS-REFUNDED.             ECS062
01675                                                                   ECS062
01676      WRITE BILLING-DATA-RECORD  FROM  WS-BILLING-SUMMARY-RECORD.  ECS062
01677                                                                   ECS062
01678      ADD +1                      TO  KSM-COUNT.                   ECS062
01679                                                                   ECS062
01680  2510-CLEAR-BILLING-SUM-RECORD.                                   ECS062
01681      MOVE SPACES                 TO  WS-BILLING-SUMMARY-RECORD.   ECS062
01682      MOVE ZEROS                  TO  BSR-NET-LIFE-PREM            ECS062
01683                                      BSR-NET-AH-PREM              ECS062
01684                                      BSR-LIFE-COMM                ECS062
01685                                      BSR-AH-COMM                  ECS062
01686                                      BSR-TOTAL-PREM               ECS062
01687                                      BSR-TOTAL-COMM               ECS062
01688                                      BSR-TOTAL-DUE                ECS062
01689                                      BSR-GROSS-LIFE-PREM          ECS062
01690                                      BSR-GROSS-AH-PREM            ECS062
01691                                      BSR-GROSS-LIFE-REFUNDS       ECS062
01692                                      BSR-GROSS-AH-REFUNDS         ECS062
01693                                      BSR-GROSS-WRITTEN            ECS062
01694                                      BSR-GROSS-REFUNDED           ECS062
01695                                      BSR-ISS-LF-CNT               ECS062
01696                                      BSR-ISS-AH-CNT               ECS062
01697                                      BSR-CAN-LF-CNT               ECS062
01698                                      BSR-CAN-AH-CNT               ECS062
01699                                      BSR-CAN-PREM-91.             ECS062
01700                                                                   ECS062
01701  2599-EXIT.                                                       ECS062
01702      EXIT.                                                        ECS062
01703  EJECT                                                            ECS062
01704  2600-WRITE-BILLING-DET.                                          ECS062
01705      WRITE BILLING-DATA-RECORD  FROM  WS-BILLING-DETAIL-RECORD.   ECS062
01706                                                                   ECS062
01707      ADD +1                      TO  KSM-DETAIL-COUNT.            ECS062
01708                                                                   ECS062
01709  2610-CLEAR-BILLING-DET-RECORD.                                   ECS062
01710      MOVE SPACES                 TO  WS-BILLING-DETAIL-RECORD.    ECS062
01711                                                                   ECS062
01712  2699-EXIT.                                                       ECS062
01713      EXIT.                                                        ECS062
01714  EJECT                                                            ECS062
01715                                                                   ECS062
01716  2900-PRT-STMT-HD-RTN.                                            ECS062
01717      IF PGCTR  = +0                                               ECS062
01718          GO TO 2910-PRT-FIRST-PAGE.                               ECS062
01719                                                                   ECS062
01720      MOVE SPACE-3                             TO  P-CCSW.         ECS062
01721      MOVE '          CONTINUED ON NEXT PAGE'  TO  P-LINE.         ECS062
01722                                                                   ECS062
01723      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01724                                                                   ECS062
01725  2910-PRT-FIRST-PAGE.                                             ECS062
01726      MOVE SAVE-COMPANY-NAME (3)  TO  HD-CO.                       ECS062
01727      MOVE HD1                    TO  P-LINE.                      ECS062
01728      MOVE SPACE-NP               TO  P-CCSW.                      ECS062
01729                                                                   ECS062
01730      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01731                                                                   ECS062
01732      ADD +1                      TO  PGCTR.                       ECS062
01733                                                                   ECS062
01734      MOVE PGCTR                  TO  HD-PG.                       ECS062
01735      MOVE HD2                    TO  P-LINE.                      ECS062
01736      MOVE SPACE-1                TO  P-CCSW.                      ECS062
01737                                                                   ECS062
01738      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01739                                                                   ECS062
01740      MOVE HD3                    TO  P-LINE.                      ECS062
01741      MOVE SPACE-1                TO  P-CCSW.                      ECS062
01742                                                                   ECS062
01743      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01744                                                                   ECS062
01745  2930-PRT-SUB-HD.                                                 ECS062
01746      MOVE 'ACCOUNT NO. - '       TO  HD-M1.                       ECS062
01747      MOVE CO-CARR-GROUP          TO  HD-CARR-GROUP.               ECS062
01748      MOVE '-'                    TO  HD-C-A.                      ECS062
01749      MOVE BIL0                   TO  HD-ACCOUNT.                  ECS062
01750      MOVE BIL1                   TO  HD-NAME.                     ECS062
01751      MOVE 'REMIT TO -'           TO  HD-M2.                       ECS062
01752      MOVE RMT0                   TO  HD-M3.                       ECS062
01753      MOVE RMT1                   TO  HD-REMIT.                    ECS062
01754      MOVE HD4                    TO  P-LINE.                      ECS062
01755                                                                   ECS062
01756      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01757                                                                   ECS062
01758      MOVE SPACES                 TO  HD-M1                        ECS062
01759                                      HD-CARR-GROUP                ECS062
01760                                      HD-C-A                       ECS062
01761                                      HD-ACCOUNT.                  ECS062
01762      MOVE BIL2                   TO  HD-NAME.                     ECS062
01763      MOVE SPACES                 TO  HD-M2                        ECS062
01764                                      HD-M3.                       ECS062
01765      MOVE RMT2                   TO  HD-REMIT.                    ECS062
01766      MOVE SPACE-1                TO  P-CCSW.                      ECS062
01767      MOVE HD4                    TO  P-LINE.                      ECS062
01768                                                                   ECS062
01769      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01770                                                                   ECS062
01771      MOVE SPACES                 TO  HD-M1                        ECS062
01772                                      HD-CARR-GROUP                ECS062
01773                                      HD-C-A                       ECS062
01774                                      HD-ACCOUNT.                  ECS062
01775      MOVE BIL3                   TO  HD-NAME.                     ECS062
01776      MOVE SPACES                 TO  HD-M2                        ECS062
01777                                      HD-M3.                       ECS062
01778      MOVE RMT3                   TO  HD-REMIT.                    ECS062
01779      MOVE SPACE-1                TO  P-CCSW.                      ECS062
01780      MOVE HD4                    TO  P-LINE.                      ECS062
01781                                                                   ECS062
01782      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01783                                                                   ECS062
01784      MOVE SPACES                 TO  HD-M1                        ECS062
01785                                      HD-CARR-GROUP                ECS062
01786                                      HD-C-A                       ECS062
01787                                      HD-ACCOUNT.                  ECS062
01788      MOVE BIL4                   TO  HD-NAME.                     ECS062
01789      MOVE SPACES                 TO  HD-M2                        ECS062
01790                                      HD-M3.                       ECS062
01791      MOVE RMT4                   TO  HD-REMIT.                    ECS062
01792      MOVE SPACE-1                TO  P-CCSW.                      ECS062
01793      MOVE HD4                    TO  P-LINE.                      ECS062
01794                                                                   ECS062
01795      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01796                                                                   ECS062
01797  2940-CHK-LN5.                                                    ECS062
01798      IF BIL5  = SPACES                                            ECS062
01799        AND  RMT5  = SPACES                                        ECS062
01800          GO TO 2950-CHK-LN6.                                      ECS062
01801                                                                   ECS062
01802      MOVE SPACES                 TO  HD-M1                        ECS062
01803                                      HD-CARR-GROUP                ECS062
01804                                      HD-C-A                       ECS062
01805                                      HD-ACCOUNT                   ECS062
01806      MOVE BIL5                   TO  HD-NAME.                     ECS062
01807      MOVE SPACES                 TO  HD-M2                        ECS062
01808                                      HD-M3.                       ECS062
01809      MOVE RMT5                   TO  HD-REMIT.                    ECS062
01810      MOVE SPACE-1                TO  P-CCSW.                      ECS062
01811      MOVE HD4                    TO  P-LINE.                      ECS062
01812                                                                   ECS062
01813      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01814                                                                   ECS062
01815  2950-CHK-LN6.                                                    ECS062
01816      IF BIL6  =  SPACES                                           ECS062
01817        AND  RMT6  = SPACES                                        ECS062
01818          GO TO 2960-PRT-HD5.                                      ECS062
01819                                                                   ECS062
01820      MOVE SPACES                 TO  HD-M1                        ECS062
01821                                      HD-CARR-GROUP                ECS062
01822                                      HD-C-A                       ECS062
01823                                      HD-ACCOUNT.                  ECS062
01824      MOVE BIL6                   TO  HD-NAME.                     ECS062
01825      MOVE SPACES                 TO  HD-M2                        ECS062
01826                                      HD-M3.                       ECS062
01827      MOVE RMT6                   TO  HD-REMIT.                    ECS062
01828      MOVE SPACE-1                TO  P-CCSW.                      ECS062
01829      MOVE HD4                    TO  P-LINE.                      ECS062
01830                                                                   ECS062
01831      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01832                                                                   ECS062
01833  2960-PRT-HD5.                                                    ECS062
01834      IF FROM-SPECIAL-PRINT                                        ECS062
01835          GO TO 2999-EXIT.                                         ECS062
01836                                                                   ECS062
01837      IF DTE-CLIENT  =  'KSM'                                      ECS062
01838          MOVE HD5A               TO  P-LINE                       ECS062
01839      ELSE                                                         ECS062
01840          MOVE HD5                TO  P-LINE.                      ECS062
01841                                                                   ECS062
01842      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01843                                                                   ECS062
01844  2970-PRT-HD6.                                                    ECS062
01845      IF DTE-CLIENT  =  'KSM'                                      ECS062
01846          MOVE HD6A               TO  P-LINE                       ECS062
01847      ELSE                                                         ECS062
01848          MOVE HD6                TO  P-LINE.                      ECS062
01849                                                                   ECS062
01850      MOVE SPACE-1                TO  P-CCSW.                      ECS062
01851                                                                   ECS062
01852      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01853                                                                   ECS062
01854      MOVE SPACE-1                TO  P-CCSW.                      ECS062
01855                                                                   ECS062
01856      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
01857                                                                   ECS062
01858      MOVE +0                     TO  LNCTR.                       ECS062
01859                                                                   ECS062
01860  2999-EXIT.                                                       ECS062
01861      EXIT.                                                        ECS062
01862  EJECT                                                            ECS062
01863  3000-CALC-DETAIL-RTN.                                            ECS062
01864      MOVE +0                     TO  C-TT-FACE  C-TT-FACE-ALT     ECS062
01865                                      C-LF-PREM  C-LF-PREM-ALT     ECS062
01866                                      C-AH-PREM  C-TT-PREM         ECS062
01867                                      C-LF-COMM  C-LF-COMM-ALT     ECS062
01868                                      C-AH-COMM  C-TT-COMM         ECS062
01869                                      C-RR-PYMT  C-CC-PYMT         ECS062
01870                                      C-TT-PYMT  C-LF-CLM          ECS062
01871                                      C-AH-CLM   C-TT-CLM          ECS062
01872                                      C-TT-AH-BEN                  ECS062
01873                                      C-ISS-AH-CNT                 ECS062
01874                                      C-ISS-LF-CNT                 ECS062
01875                                      C-CAN-AH-CNT                 ECS062
01876                                      C-CAN-LF-CNT                 ECS062
01877                                      C-CAN-PREM-91.               ECS062
01878                                                                   ECS062
01879      IF CP-CLAIM                                                  ECS062
01880          GO TO 3050-CALC-CLAIM.                                   ECS062
01881                                                                   ECS062
01882      IF CP-RC-ISSUE                                               ECS062
01883        OR  CP-RC-CANCEL                                           ECS062
01884        OR  CP-ISSUE                                               ECS062
01885        OR  CP-CANCEL                                              ECS062
01886          GO TO 3010-CALC-ISSUE.                                   ECS062
01887                                                                   ECS062
01888      IF CP-ACCTG                                                  ECS062
01889          GO TO 3040-CALC-ACCTG.                                   ECS062
01890                                                                   ECS062
01891      MOVE '0301'                 TO  WS-RETURN-CODE.              ECS062
01892      MOVE 'FATAL DATA ERROR'     TO  WS-ABEND-MESSAGE.            ECS062
01893                                                                   ECS062
01894      GO TO ABEND-PGM.                                             ECS062
01895                                                                   ECS062
01896  3010-CALC-ISSUE.                                                 ECS062
01897      MOVE CP-LF-AMT              TO  C-TT-FACE.                   ECS062
01898      MOVE CP-LF-AMT-ALT          TO  C-TT-FACE-ALT.               ECS062
01899      MOVE CP-AH-AMT              TO  C-TT-AH-BEN.                 ECS062
01900      MOVE CP-AH-TYPE             TO  CLAS-LOOK.                   ECS062
01901                                                                   ECS062
01902      IF CLAS-LOOK  = '00'                                         ECS062
01903          GO TO 3020-CHECK-LF.                                     ECS062
01904                                                                   ECS062
01905      PERFORM 2200-FIND-AH  THRU  2299-XIT.                        ECS062
01906                                                                   ECS062
01907      MOVE AH-OVERRIDE-L2            TO  P-CP-BEN.                 ECS062
01908      MOVE '-'                       TO  P-CP-BEN-DASH.            ECS062
01909      MOVE CLAS-I-AB3 (CLAS-INDEXA)  TO  P-CP-BEN-TYPE.            ECS062
01910                                                                   ECS062
01911      IF DTE-CLIENT = 'DMD'                                        ECS062
01912          MOVE CLAS-I-BEN (CLAS-INDEXA)  TO  P-CP-BEN-TYPE.        ECS062
01913                                                                   ECS062
01914      IF CLAS-I-BAL (CLAS-INDEXA)  = 'B'  OR  'Z'                  ECS062
01915          MOVE 'Y'                TO  OB-SWITCH.                   ECS062
01916                                                                   ECS062
01917  3020-CHECK-LF.                                                   ECS062
01918      MOVE CP-LF-TYPE             TO  CLAS-LOOK.                   ECS062
01919                                                                   ECS062
01920      IF CLAS-LOOK  = '00'                                         ECS062
01921          GO TO 3030-CONTINUE.                                     ECS062
01922                                                                   ECS062
01923      PERFORM 2300-FIND-LF  THRU  2399-XIT.                        ECS062
01924                                                                   ECS062
01925      MOVE LIFE-OVERRIDE-L2          TO  P-CP-BEN.                 ECS062
01926      MOVE '-'                       TO  P-CP-BEN-DASH.            ECS062
01927      MOVE CLAS-I-AB3 (CLAS-INDEXL)  TO  P-CP-BEN-TYPE.            ECS062
01928                                                                   ECS062
01929      IF DTE-CLIENT = 'DMD'                                        ECS062
01930          MOVE CLAS-I-BEN (CLAS-INDEXA)  TO  P-CP-BEN-TYPE.        ECS062
01931                                                                   ECS062
01932      IF CLAS-I-BAL (CLAS-INDEXL)  = 'B'  OR  'Z'                  ECS062
01933          MOVE 'Y'                TO  OB-SWITCH.                   ECS062
01934                                                                   ECS062
01935  3030-CONTINUE.                                                   ECS062
01936      MOVE CP-LF-PRM              TO  C-LF-PREM.                   ECS062
01937      MOVE CP-LF-PRM-ALT          TO  C-LF-PREM-ALT.               ECS062
01938      MOVE CP-AH-PRM              TO  C-AH-PREM.                   ECS062
01939                                                                   ECS062
01940      IF C-TT-SAVE-PREM NOT NUMERIC                                ECS062
01941         MOVE ZEROS               TO  C-TT-SAVE-PREM.              ECS062
01942      IF C-TT-SAVE-COMM NOT NUMERIC                                ECS062
01943         MOVE ZEROS               TO  C-TT-SAVE-COMM.              ECS062
01944                                                                   ECS062
01945      COMPUTE C-TT-PREM  =  C-LF-PREM  +  C-LF-PREM-ALT            ECS062
01946                         +  C-AH-PREM.                             ECS062
01947                                                                   ECS062
01948      ADD C-TT-PREM               TO  C-TT-SAVE-PREM.              ECS062
01949                                                                   ECS062
01950      IF DTE-CLIENT  =  'KSM'                                      ECS062
01951          PERFORM 3100-GROSS-RTN  THRU  3199-EXIT.                 ECS062
01952                                                                   ECS062
01953      MOVE CP-LF-COM              TO  C-LF-COMM.                   ECS062
01954      MOVE CP-LF-COM-ALT          TO  C-LF-COMM-ALT.               ECS062
01955      MOVE CP-AH-COM              TO  C-AH-COMM.                   ECS062
01956                                                                   ECS062
01957      COMPUTE C-TT-COMM  =  C-LF-COMM  +  C-LF-COMM-ALT            ECS062
01958                         +  C-AH-COMM.                             ECS062
01959      ADD C-TT-COMM               TO  C-TT-SAVE-COMM.              ECS062
01960                                                                   ECS062
01961      IF DTE-CLIENT NOT = 'KSM'                                    ECS062
01962          GO TO 3070-ROLL-DETAIL.                                  ECS062
01963                                                                   ECS062
01964      MOVE +0                     TO C-ISS-LF-CNT                  ECS062
01965                                     C-ISS-AH-CNT                  ECS062
01966                                     C-CAN-LF-CNT                  ECS062
01967                                     C-CAN-AH-CNT                  ECS062
01968                                     C-CAN-PREM-91.                ECS062
01969                                                                   ECS062
01970      IF CP-LF-TYPE  = '00'                                        ECS062
01971          NEXT SENTENCE                                            ECS062
01972       ELSE                                                        ECS062
01973          IF CP-ISSUE                                              ECS062
01974              MOVE +1             TO C-ISS-LF-CNT                  ECS062
01975            ELSE                                                   ECS062
01976              IF CP-CANCEL                                         ECS062
01977                  MOVE +1         TO C-CAN-LF-CNT.                 ECS062
01978                                                                   ECS062
01979      IF CP-AH-TYPE  = '00'                                        ECS062
01980          NEXT SENTENCE                                            ECS062
01981       ELSE                                                        ECS062
01982          IF CP-ISSUE                                              ECS062
01983              MOVE +1             TO C-ISS-AH-CNT                  ECS062
01984            ELSE                                                   ECS062
01985              IF CP-CANCEL                                         ECS062
01986                  MOVE +1         TO C-CAN-AH-CNT.                 ECS062
01987                                                                   ECS062
01988      IF CP-ISSUE  OR                                              ECS062
01989         CP-CANCEL                                                 ECS062
01990        IF CP-EFF LESS WS-920101-DATE                              ECS062
01991          COMPUTE C-CAN-PREM-91 = CP-AH-PRM                        ECS062
01992                                + CP-LF-PRM                        ECS062
01993                                + CP-LF-PRM-ALT.                   ECS062
01994                                                                   ECS062
01995      GO TO 3070-ROLL-DETAIL.                                      ECS062
01996                                                                   ECS062
01997  3040-CALC-ACCTG.                                                 ECS062
01998      MOVE CP-AC-PMT              TO  C-RR-PYMT.                   ECS062
01999      MOVE CP-AC-CHG              TO  C-CC-PYMT.                   ECS062
02000                                                                   ECS062
02001      COMPUTE C-TT-PYMT  =  C-RR-PYMT  -  C-CC-PYMT.               ECS062
02002                                                                   ECS062
02003      GO TO 3070-ROLL-DETAIL.                                      ECS062
02004                                                                   ECS062
02005  3050-CALC-CLAIM.                                                 ECS062
02006      MOVE CP-CLM-LF-AMT          TO  C-LF-CLM.                    ECS062
02007      MOVE CP-CLM-AH-AMT          TO  C-AH-CLM.                    ECS062
02008                                                                   ECS062
02009      COMPUTE C-TT-CLM  =  C-LF-CLM  +  C-AH-CLM.                  ECS062
02010                                                                   ECS062
02011  3070-ROLL-DETAIL.                                                ECS062
02012      ADD C-TT-FACE               TO  S-TT-FACE.                   ECS062
02013      ADD C-TT-FACE-ALT           TO  S-TT-FACE.                   ECS062
02014      ADD C-TT-AH-BEN             TO  S-TT-AH-BEN.                 ECS062
02015      ADD C-AH-PREM               TO  S-AH-PREM.                   ECS062
02016      ADD C-LF-PREM               TO  S-LF-PREM.                   ECS062
02017      ADD C-LF-PREM-ALT           TO  S-LF-PREM.                   ECS062
02018      ADD C-TT-PREM               TO  S-TT-PREM.                   ECS062
02019      ADD C-LF-COMM               TO  S-LF-COMM.                   ECS062
02020      ADD C-LF-COMM-ALT           TO  S-LF-COMM.                   ECS062
02021      ADD C-AH-COMM               TO  S-AH-COMM.                   ECS062
02022      ADD C-TT-COMM               TO  S-TT-COMM.                   ECS062
02023      ADD C-RR-PYMT               TO  S-RR-PYMT.                   ECS062
02024      ADD C-CC-PYMT               TO  S-CC-PYMT.                   ECS062
02025      ADD C-TT-PYMT               TO  S-TT-PYMT.                   ECS062
02026      ADD C-LF-CLM                TO  S-LF-CLM.                    ECS062
02027      ADD C-AH-CLM                TO  S-AH-CLM.                    ECS062
02028      ADD C-TT-CLM                TO  S-TT-CLM.                    ECS062
02029                                                                   ECS062
02030      IF DTE-CLIENT NOT = 'KSM'                                    ECS062
02031          GO TO 3099-EXIT.                                         ECS062
02032                                                                   ECS062
02033      IF C-ISS-AH-CNT NUMERIC                                      ECS062
02034          ADD C-ISS-AH-CNT        TO  S-ISS-AH-CNT.                ECS062
02035      IF C-ISS-LF-CNT NUMERIC                                      ECS062
02036          ADD C-ISS-LF-CNT        TO  S-ISS-LF-CNT.                ECS062
02037      IF C-CAN-AH-CNT NUMERIC                                      ECS062
02038          ADD C-CAN-AH-CNT        TO  S-CAN-AH-CNT.                ECS062
02039      IF C-CAN-LF-CNT NUMERIC                                      ECS062
02040          ADD C-CAN-LF-CNT        TO  S-CAN-LF-CNT.                ECS062
02041      IF C-CAN-PREM-91 NUMERIC                                     ECS062
02042          ADD C-CAN-PREM-91       TO  S-CAN-PREM-91.               ECS062
02043                                                                   ECS062
02044      MOVE C-ACCT-STATUS          TO  S-ACCT-STATUS.               ECS062
02045      MOVE C-REI-TABLE            TO  S-REI-TABLE.                 ECS062
02046                                                                   ECS062
02047      IF CP-ISSUE OR                                               ECS062
02048         CP-CANCEL                                                 ECS062
02049          NEXT SENTENCE                                            ECS062
02050        ELSE                                                       ECS062
02051          GO TO 3099-EXIT.                                         ECS062
02052                                                                   ECS062
02053      IF CP-EFF LESS S-LO-CERT-DATE                                ECS062
02054          MOVE CP-EFF         TO S-LO-CERT-DATE.                   ECS062
02055                                                                   ECS062
02056      IF CP-EFF GREATER S-HI-CERT-DATE                             ECS062
02057          MOVE CP-EFF         TO S-HI-CERT-DATE.                   ECS062
02058                                                                   ECS062
02059  3099-EXIT.                                                       ECS062
02060      EXIT.                                                        ECS062
02061  EJECT                                                            ECS062
02062  3100-GROSS-RTN.                                                  ECS062
02063      IF CP-RC-CANCEL                                              ECS062
02064        OR  CP-CANCEL                                              ECS062
02065          GO TO 3110-GROSS-CAN.                                    ECS062
02066                                                                   ECS062
02067      COMPUTE BSR-GROSS-LIFE-PREM = BSR-GROSS-LIFE-PREM            ECS062
02068                                  + C-LF-PREM + C-LF-PREM-ALT.     ECS062
02069      COMPUTE BSR-GROSS-AH-PREM = BSR-GROSS-AH-PREM                ECS062
02070                                + C-AH-PREM.                       ECS062
02071      COMPUTE BSR-GROSS-WRITTEN = BSR-GROSS-WRITTEN                ECS062
02072                                  + C-TT-PREM.                     ECS062
02073                                                                   ECS062
02074      GO TO 3199-EXIT.                                             ECS062
02075                                                                   ECS062
02076  3110-GROSS-CAN.                                                  ECS062
02077      COMPUTE BSR-GROSS-LIFE-REFUNDS = BSR-GROSS-LIFE-REFUNDS      ECS062
02078          + ((C-LF-PREM * -1) + (C-LF-PREM-ALT * -1)).             ECS062
02079      COMPUTE BSR-GROSS-AH-REFUNDS = BSR-GROSS-AH-REFUNDS          ECS062
02080                                   + (C-AH-PREM * -1).             ECS062
02081      COMPUTE BSR-GROSS-REFUNDED = BSR-GROSS-REFUNDED              ECS062
02082                                 + (C-TT-PREM * -1).               ECS062
02083                                                                   ECS062
02084  3199-EXIT.                                                       ECS062
02085      EXIT.                                                        ECS062
02086  EJECT                                                            ECS062
02087  3200-CALC-TOTAL-RTN.                                             ECS062
02088      MOVE CO-BAL-FWD             TO  S-BEG-BAL.                   ECS062
02089                                                                   ECS062
02090      COMPUTE S-END-BAL  =  S-BEG-BAL  +  S-TT-PREM                ECS062
02091                         -  S-TT-PYMT  -  S-TT-COMM.               ECS062
02092                                                                   ECS062
02093      IF S-END-BAL  IS NEGATIVE                                    ECS062
02094          IF S-END-BAL  LESS VARY-LO                               ECS062
02095              GO TO 3210-CALC-TOTAL.                               ECS062
02096                                                                   ECS062
02097      IF S-END-BAL  IS POSITIVE                                    ECS062
02098          IF S-END-BAL  GREATER VARY-HI                            ECS062
02099              GO TO 3210-CALC-TOTAL.                               ECS062
02100                                                                   ECS062
02101      COMPUTE S-WRT-OFF  =  S-END-BAL  *  -1.                      ECS062
02102                                                                   ECS062
02103      ADD S-WRT-OFF               TO  S-END-BAL.                   ECS062
02104                                                                   ECS062
02105  3210-CALC-TOTAL.                                                 ECS062
02106      IF SUMM-SW  = +1                                             ECS062
02107          PERFORM 4000-BUILD-SUMMARY-RTN  THRU  4099-EXIT.         ECS062
02108                                                                   ECS062
02109      ADD S-BEG-BAL               TO  T-BEG-BAL.                   ECS062
02110      ADD S-TT-PREM               TO  T-TOT-PRM.                   ECS062
02111      ADD S-TT-COMM               TO  T-TOT-COM.                   ECS062
02112      ADD S-TT-PYMT               TO  T-TOT-PMT.                   ECS062
02113      ADD S-WRT-OFF               TO  T-WRT-OFF.                   ECS062
02114      ADD S-END-BAL               TO  T-END-BAL.                   ECS062
02115      ADD S-TT-COMM               TO  T-ADJ-COM.                   ECS062
02116                                                                   ECS062
02117      SUBTRACT S-WRT-OFF          FROM  T-ADJ-COM.                 ECS062
02118                                                                   ECS062
02119  3299-EXIT.                                                       ECS062
02120      EXIT.                                                        ECS062
02121  EJECT                                                            ECS062
02122  4000-BUILD-SUMMARY-RTN.                                          ECS062
02123      MOVE SPACES                 TO  CCM-WK.                      ECS062
02124      MOVE '#'                    TO  CCW-ID.                      ECS062
02125      MOVE CO-CARR-GROUP          TO  CCW-CARR-GROUP.              ECS062
02126      MOVE CO-RESP-NO             TO  CCW-RESP-NO.                 ECS062
02127      MOVE LOW-VALUES             TO  CCW-ACCOUNT.                 ECS062
02128      MOVE CO-ACCOUNT             TO  CCW-AM-NO.                   ECS062
02129      MOVE '7'                    TO  CCW-TYPE.                    ECS062
02130      MOVE S-TT-PREM              TO  CCW-PREM.                    ECS062
02131      MOVE S-TT-COMM              TO  CCW-COMM.                    ECS062
02132                                                                   ECS062
02133 * ADJUSTING COMMISSIONS BY WRT-OFF AMOUNT.                        ECS062
02134      SUBTRACT S-WRT-OFF          FROM  CCW-COMM.                  ECS062
02135                                                                   ECS062
02136      MOVE S-TT-PYMT              TO  CCW-PMTS.                    ECS062
02137      MOVE S-BEG-BAL              TO  CCW-BEG-BAL.                 ECS062
02138      MOVE S-END-BAL              TO  CCW-END-BAL.                 ECS062
02139      MOVE +0                     TO  CCW-OV-L-PREM                ECS062
02140                                      CCW-OV-A-PREM                ECS062
02141                                      CCW-OV-LIFE                  ECS062
02142                                      CCW-OV-AH.                   ECS062
02143      MOVE +0                     TO  CCW-OV-B-L-PREM              ECS062
02144                                      CCW-OV-B-A-PREM              ECS062
02145                                      CCW-OV-B-LIFE                ECS062
02146                                      CCW-OV-B-AH.                 ECS062
02147      MOVE CO-BALANCE-CONTROL     TO  CCW-BAL-CTL.                 ECS062
02148      MOVE CO-ACCT-NAME           TO  CCW-NAME.                    ECS062
02149                                                                   ECS062
02150      IF CO-ACCT-NAME  = SPACES                                    ECS062
02151          MOVE CO-MAIL-NAME       TO  CCW-NAME.                    ECS062
02152                                                                   ECS062
02153      PERFORM 8400-WRT-CCM-RTN  THRU  8499-EXIT.                   ECS062
02154                                                                   ECS062
02155  4099-EXIT.                                                       ECS062
02156      EXIT.                                                        ECS062
02157  EJECT                                                            ECS062
02158  4200-UPDATE-MSTR-RTN.                                            ECS062
02159      ADD S-TT-COMM               TO  CO-YTD-COM.                  ECS062
02160                                                                   ECS062
02161      SUBTRACT S-WRT-OFF          FROM  CO-YTD-COM.                ECS062
02162                                                                   ECS062
02163      IF STMT-SW  = +1                                             ECS062
02164          MOVE 'Y'                TO  CO-INTERNAL-CONTROL-2        ECS062
02165      ELSE                                                         ECS062
02166          MOVE 'N'                TO  CO-INTERNAL-CONTROL-2.       ECS062
02167                                                                   ECS062
02168      IF CO-CARRY-BALANCE                                          ECS062
02169          NEXT SENTENCE                                            ECS062
02170      ELSE                                                         ECS062
02171          GO TO 4299-EXIT.                                         ECS062
02172                                                                   ECS062
02173      MOVE S-TT-COMM              TO  CO-CUR-COM.                  ECS062
02174                                                                   ECS062
02175 * ADJUSTING COMMISSION BY WRT-OFF AMOUNT                          ECS062
02176      SUBTRACT S-WRT-OFF          FROM  CO-CUR-COM.                ECS062
02177                                                                   ECS062
02178      MOVE S-TT-PREM              TO  CO-CUR-CHG.                  ECS062
02179      MOVE S-TT-PYMT              TO  CO-CUR-PMT.                  ECS062
02180      MOVE S-END-BAL              TO  CO-END-BAL.                  ECS062
02181                                                                   ECS062
02182      COMPUTE CO-CUR  =  S-TT-PREM  -  S-TT-COMM  -  S-WRT-OFF.    ECS062
02183                                                                   ECS062
02184      MOVE S-LF-CLM               TO  CO-LF-CLM-AMT.               ECS062
02185      MOVE S-AH-CLM               TO  CO-AH-CLM-AMT.               ECS062
02186                                                                   ECS062
02187      IF CO-CUR-OVR-UNDR  IS NOT NUMERIC                           ECS062
02188          MOVE ZEROS              TO  CO-CUR-OVR-UNDR.             ECS062
02189                                                                   ECS062
02190      IF CO-YTD-OVR-UNDR  IS NOT NUMERIC                           ECS062
02191          MOVE ZEROS              TO  CO-YTD-OVR-UNDR.             ECS062
02192                                                                   ECS062
02193      ADD S-WRT-OFF               TO  CO-YTD-OVR-UNDR.             ECS062
02194                                                                   ECS062
02195      MOVE S-WRT-OFF              TO  CO-CUR-OVR-UNDR.             ECS062
02196                                                                   ECS062
02197 * ADJUSTING AGING AMOUNTS BY PAYMENT AMOUNTS                      ECS062
02198      IF CO-CUR-PMT  IS NEGATIVE                                   ECS062
02199          SUBTRACT CO-CUR-PMT     FROM  CO-CUR                     ECS062
02200      ELSE                                                         ECS062
080612        if dte-client = 'AHL'
080612           subtract co-cur-pmt   from co-ov120
080612        else
02201            SUBTRACT CO-CUR-PMT   FROM  CO-OV90
080612        end-if
080612     end-if
02202                                                                   ECS062
02203      IF CO-CUR  IS NEGATIVE                                       ECS062
080612        if dte-client = 'AHL'
080612           add co-cur            to co-ov120
080612        else
02204            ADD CO-CUR            TO  CO-OV90
080612        end-if
02205         MOVE +0                  TO  CO-CUR
080612     end-if

080612     if dte-client = 'AHL'
080612        if co-ov120 is negative
080612           add co-ov120          to co-ov90
080612           move +0               to co-ov120
080612        end-if
080612     end-if
02206                                                                   ECS062
02207      IF CO-OV90  IS NEGATIVE                                      ECS062
02208          ADD CO-OV90             TO  CO-OV60                      ECS062
02209          MOVE +0                 TO  CO-OV90.                     ECS062
02210                                                                   ECS062
02211      IF CO-OV60  IS NEGATIVE                                      ECS062
02212          ADD CO-OV60             TO  CO-OV30                      ECS062
02213          MOVE +0                 TO  CO-OV60.                     ECS062
02214                                                                   ECS062
02215      IF CO-OV30  IS NEGATIVE                                      ECS062
02216          ADD CO-OV30             TO  CO-CUR                       ECS062
02217          MOVE +0                 TO  CO-OV30.                     ECS062
02218                                                                   ECS062
02219      COMPUTE TOTAL-DUE  =  CO-CUR  +  CO-OV30                     ECS062
02220                         +  CO-OV60  +  CO-OV90.                   ECS062
           if dte-client = 'AHL'
              compute total-due = total-due + co-ov120
           end-if
02221                                                                   ECS062
02222      IF CO-END-BAL  NOT = TOTAL-DUE                               ECS062
02223        OR  CO-END-BAL  LESS  ZERO                                 ECS062
02224          MOVE CO-END-BAL         TO  CO-CUR                       ECS062
02225          MOVE +0                 TO  CO-OV30                      ECS062
02226                                      CO-OV60                      ECS062
02227                                      CO-OV90
                                           co-ov120
           end-if

           .
02229  4299-EXIT.                                                       ECS062
02230      EXIT.                                                        ECS062
02231       EJECT                                                       ECS062
02232  8000-MSTR-CONTROL-RTN.                                           ECS062
02233      MOVE COMPENSATION-MASTER    TO  COMP-OUT-RECORD.             ECS062
02234                                                                   ECS062
02235      IF COO-ID  NOT = 'CO'                                        ECS062
02236          GO TO 8010-READ-MSTR.                                    ECS062
02237                                                                   ECS062
02238      WRITE COMP-OUT-RECORD.                                       ECS062
02239                                                                   ECS062
02240  8010-READ-MSTR.                                                  ECS062
02241      IF COI-ID  = HIGH-VALUES                                     ECS062
02242          MOVE COMP-IN-RECORD     TO  COMPENSATION-MASTER          ECS062
02243          GO TO 8099-EXIT.                                         ECS062
02244                                                                   ECS062
02245      READ COMM-MSTR-IN                                            ECS062
02246          AT END                                                   ECS062
02247              MOVE HIGH-VALUE     TO  COMP-IN-RECORD               ECS062
02248                                      COMPENSATION-MASTER          ECS062
02249              GO TO 8099-EXIT.                                     ECS062
02250                                                                   ECS062
02251      MOVE COMP-IN-RECORD         TO  COMPENSATION-MASTER.         ECS062
02252                                                                   ECS062
02253      IF CO-CTL-1  = PRE-CTL-1                                     ECS062
02254          GO TO 8060-SET-NEW-CARR-COMP.                            ECS062
02255                                                                   ECS062
02256  8020-RESET-REMIT.                                                ECS062
02257      IF CO-CARRIER  = PRE-CARRIER                                 ECS062
02258          GO TO 8040-SET-COMPANY.                                  ECS062
02259                                                                   ECS062
02260  8030-SET-CARRIER.                                                ECS062
02261      MOVE CO-CARRIER             TO  PRE-CARRIER.                 ECS062
02262      MOVE SAVE-COMPANY-NAME (1)  TO  SAVE-COMPANY-NAME (2)        ECS062
02263                                      SAVE-COMPANY-NAME (3).       ECS062
02264      MOVE RMT-LEVEL (1)          TO  RMT-LEVEL (2)                ECS062
02265                                      RMT-LEVEL (3)                ECS062
02266                                      RMT-LEVEL (4).               ECS062
02267                                                                   ECS062
02268  8040-SET-COMPANY.                                                ECS062
02269      IF CO-GROUPING  = PRE-GROUPING                               ECS062
02270          GO TO 8050-SET-AGENT.                                    ECS062
02271                                                                   ECS062
02272      MOVE CO-GROUPING            TO  PRE-GROUPING.                ECS062
02273      MOVE SAVE-COMPANY-NAME (2)  TO  SAVE-COMPANY-NAME (3).       ECS062
02274      MOVE RMT-LEVEL (2)          TO  RMT-LEVEL (3)                ECS062
02275                                      RMT-LEVEL (4).               ECS062
02276                                                                   ECS062
02277  8050-SET-AGENT.                                                  ECS062
02278      IF CO-RESP-NO  = PRE-RESP-NO                                 ECS062
02279          GO TO 8060-SET-NEW-CARR-COMP.                            ECS062
02280                                                                   ECS062
02281      MOVE +0                     TO  SUMM-SW.                     ECS062
02282      MOVE CO-RESP-NO             TO  PRE-RESP-NO.                 ECS062
02283      MOVE RMT-LEVEL (3)          TO  RMT-LEVEL (4).               ECS062
02284                                                                   ECS062
02285  8060-SET-NEW-CARR-COMP.                                          ECS062
02286      IF NOT CO-COMPANY-TYPE                                       ECS062
02287          GO TO 8070-SET-NEW-AGENT.                                ECS062
02288                                                                   ECS062
02289      MOVE CO-ACCT-NAME           TO  SAVE-COMPANY-NAME (2)        ECS062
02290                                      SAVE-COMPANY-NAME (3).       ECS062
02291                                                                   ECS062
02292      PERFORM 8100-FORM-RMT-RTN  THRU  8199-EXIT.                  ECS062
02293                                                                   ECS062
02294      MOVE RMTX                   TO  RMT-LEVEL (2)                ECS062
02295                                      RMT-LEVEL (3)                ECS062
02296                                      RMT-LEVEL (4).               ECS062
02297                                                                   ECS062
02298      GO TO 8000-MSTR-CONTROL-RTN.                                 ECS062
02299                                                                   ECS062
02300  8070-SET-NEW-AGENT.                                              ECS062
02301      IF NOT CO-GEN-AGENT-TYPE                                     ECS062
02302          GO TO 8080-SET-REMIT.                                    ECS062
02303                                                                   ECS062
02304      MOVE +1                     TO  SUMM-SW.                     ECS062
02305                                                                   ECS062
02306      PERFORM 8100-FORM-RMT-RTN  THRU  8199-EXIT.                  ECS062
02307                                                                   ECS062
02308      MOVE CO-RESP-NO             TO  RMT0.                        ECS062
02309      MOVE RMTX                   TO  RMT-LEVEL (4).               ECS062
02310                                                                   ECS062
02311      GO TO 8000-MSTR-CONTROL-RTN.                                 ECS062
02312                                                                   ECS062
02313  8080-SET-REMIT.                                                  ECS062
02314      IF NOT CO-ACCOUNT-TYPE                                       ECS062
02315          MOVE '0301'             TO  WS-RETURN-CODE               ECS062
02316          MOVE 'FATAL DATA ERROR'                                  ECS062
02317                                  TO  WS-ABEND-MESSAGE             ECS062
02318          GO TO ABEND-PGM.                                         ECS062
02319                                                                   ECS062
02320      MOVE 'Y'                    TO  ACCOUNT-TYPE-SWITCH.         ECS062
02321                                                                   ECS062
02322      PERFORM 8100-FORM-RMT-RTN  THRU  8199-EXIT.                  ECS062
02323                                                                   ECS062
02324      MOVE 'N'                    TO  ACCOUNT-TYPE-SWITCH.         ECS062
02325                                                                   ECS062
02326      MOVE CO-ACCOUNT             TO  RMT0.                        ECS062
02327      MOVE RMTX                   TO  BILX.                        ECS062
02328      MOVE RMT-LEVEL (4)          TO  RMTX.                        ECS062
02329      MOVE +0                     TO  S-TT-FACE  S-LF-PREM         ECS062
02330                                      S-AH-PREM  S-TT-PREM         ECS062
02331                                      S-LF-COMM  S-AH-COMM         ECS062
02332                                      S-TT-COMM  S-CC-PYMT         ECS062
02333                                      S-RR-PYMT  S-TT-PYMT         ECS062
02334                                      S-LF-CLM   S-AH-CLM          ECS062
02335                                      S-TT-CLM   S-BEG-BAL         ECS062
02336                                      S-WRT-OFF  S-END-BAL         ECS062
02337                                      S-TT-AH-BEN                  ECS062
02338                                      S-ISS-AH-CNT                 ECS062
02339                                      S-ISS-LF-CNT                 ECS062
02340                                      S-CAN-AH-CNT                 ECS062
02341                                      S-CAN-LF-CNT                 ECS062
02342                                      S-CAN-PREM-91.               ECS062
02343      MOVE 99999999999            TO  S-LO-CERT-DATE.              ECS062
02344      MOVE 00000000000            TO  S-HI-CERT-DATE.              ECS062
02345      MOVE +0                     TO  PGCTR.                       ECS062
02346      MOVE +066                   TO  LNCTR.                       ECS062
02347      MOVE +0                     TO  STMT-SW.                     ECS062
02348                                                                   ECS062
02349      IF DTE-PGM-OPT  = 1                                          ECS062
02350        AND  CO-BAL-FWD  NOT = +0                                  ECS062
02351          MOVE +1                 TO  STMT-SW.                     ECS062
02352                                                                   ECS062
02353      MOVE 'Y'                    TO  FIRST-CLAIM-SW.              ECS062
02354                                                                   ECS062
02355  8099-EXIT.                                                       ECS062
02356      EXIT.                                                        ECS062
02357  EJECT                                                            ECS062
02358  8100-FORM-RMT-RTN.                                               ECS062
02359      MOVE CO-ACCT-NAME           TO  LJ-NAME.                     ECS062
02360      MOVE +1                     TO  LJ-NDX1  LJ-NDX2.            ECS062
02361                                                                   ECS062
02362      IF LJ-CHAR (LJ-NDX2)  NOT = SPACES                           ECS062
02363        OR  LJ-NAME  = SPACES                                      ECS062
02364          GO TO 8140-FORM-RMT.                                     ECS062
02365                                                                   ECS062
02366  8110-LEFT-JUSTIFY.                                               ECS062
02367      ADD +1 TO LJ-NDX2.                                           ECS062
02368                                                                   ECS062
02369      IF LJ-CHAR (LJ-NDX2)  = SPACE                                ECS062
02370          GO TO 8110-LEFT-JUSTIFY.                                 ECS062
02371                                                                   ECS062
02372  8120-LOOP-2.                                                     ECS062
02373      MOVE LJ-CHAR (LJ-NDX2)      TO  LJ-CHAR (LJ-NDX1).           ECS062
02374                                                                   ECS062
02375      ADD +1                      TO  LJ-NDX2.                     ECS062
02376                                                                   ECS062
02377  8130-LOOP-3.                                                     ECS062
02378      ADD +1                      TO  LJ-NDX1.                     ECS062
02379                                                                   ECS062
02380      IF LJ-NDX1  GREATER +30                                      ECS062
02381          GO TO 8140-FORM-RMT.                                     ECS062
02382                                                                   ECS062
02383      IF LJ-NDX2  GREATER +30                                      ECS062
02384          MOVE SPACES             TO  LJ-CHAR (LJ-NDX1)            ECS062
02385          GO TO 8130-LOOP-3.                                       ECS062
02386                                                                   ECS062
02387      GO TO 8120-LOOP-2.                                           ECS062
02388                                                                   ECS062
02389  8140-FORM-RMT.                                                   ECS062
02390      MOVE SPACES                 TO  RMTX.                        ECS062
02391      MOVE CO-MAIL-NAME           TO  RMT1.                        ECS062
02392      MOVE LJ-NAME                TO  RMT2.                        ECS062
02393      MOVE CO-ADDR-1              TO  RMT3.                        ECS062
02394      MOVE CO-ADDR-2              TO  RMT4.                        ECS062
051810     MOVE SPACES                 TO  RMT5
051810     STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
051810        DELIMITED BY '  ' INTO RMT5
051810     END-STRING
02396      MOVE CO-ZIP                 TO  WORK-ZIP-CODE.               ECS062
02397      MOVE SPACES                 TO  LINE5-ZIP-CODE.              ECS062
02398                                                                   ECS062
02399      IF WZC-POS-1  IS NOT NUMERIC                                 ECS062
02400          MOVE WZC-POST-CD1       TO  LINE5-POST-CODE1             ECS062
02401          MOVE WZC-POST-CD2       TO  LINE5-POST-CODE2             ECS062
02402      ELSE                                                         ECS062
02403          MOVE WZC-PRIME          TO  LINE5-ZIP-FIVE               ECS062
02404          IF WZC-PLUS4 = '0000'  OR  SPACES                        ECS062
02405              MOVE SPACES         TO  LINE5-ZIP-DASH               ECS062
02406                                      LINE5-ZIP-FOUR               ECS062
02407          ELSE                                                     ECS062
02408              MOVE '-'            TO  LINE5-ZIP-DASH               ECS062
02409              MOVE WZC-PLUS4      TO  LINE5-ZIP-FOUR.              ECS062
02410                                                                   ECS062
02411      MOVE LINE5-ZIP-CODE         TO  RMT-ZIP.                     ECS062
02412                                                                   ECS062
02413      IF RMT1  = RMT2                                              ECS062
02414          MOVE SPACES             TO  RMT1.                        ECS062
02415                                                                   ECS062
02416      MOVE +0                     TO  LJ-NDX1.                     ECS062
02417                                                                   ECS062
02418  8150-CHECK-TWICE.                                                ECS062
02419      ADD +1                      TO  LJ-NDX1.                     ECS062
02420                                                                   ECS062
02421      IF RMT1  = SPACES                                            ECS062
02422          MOVE RMT2               TO  RMT1                         ECS062
02423          MOVE SPACES             TO  RMT2.                        ECS062
02424                                                                   ECS062
02425      IF RMT2  = SPACES                                            ECS062
02426          MOVE RMT3               TO  RMT2                         ECS062
02427          MOVE SPACES             TO  RMT3.                        ECS062
02428                                                                   ECS062
02429      IF RMT3  = SPACES                                            ECS062
02430          MOVE RMT4               TO  RMT3                         ECS062
02431          MOVE SPACES             TO  RMT4.                        ECS062
02432                                                                   ECS062
02433      IF RMT4  = SPACES                                            ECS062
02434          MOVE RMT5               TO  RMT4                         ECS062
02435          MOVE SPACES             TO  RMT5.                        ECS062
02436                                                                   ECS062
02437      IF RMT5  = SPACES                                            ECS062
02438          MOVE RMT6               TO  RMT5                         ECS062
02439          MOVE SPACES             TO  RMT6.                        ECS062
02440                                                                   ECS062
02441      IF LJ-NDX1  LESS +2                                          ECS062
02442          GO TO 8150-CHECK-TWICE.                                  ECS062
02443                                                                   ECS062
02444      IF FROM-ACCOUNT-PART                                         ECS062
02445          NEXT SENTENCE                                            ECS062
02446      ELSE                                                         ECS062
02447          GO TO 8199-EXIT.                                         ECS062
02448                                                                   ECS062
02449      IF DTE-CLIENT  =  'KSM'                                      ECS062
02450          NEXT SENTENCE                                            ECS062
02451      ELSE                                                         ECS062
02452          GO TO 8199-EXIT.                                         ECS062
02453                                                                   ECS062
02454      MOVE CO-CARR-GROUP          TO  SAV-KSM-CARR-GROUP.          ECS062
02455      MOVE CO-ACCOUNT             TO  SAV-KSM-ACCOUNT.             ECS062
02456      MOVE CO-ACCT-NAME           TO  SAV-KSM-ACCT-NAME.           ECS062
02457      MOVE CO-ADDR-1              TO  SAV-KSM-ADDR-1.              ECS062
02458      MOVE CO-ADDR-2              TO  SAV-KSM-ADDR-2.              ECS062
051810     MOVE SPACES                 TO  SAV-KSM-CITY-STATE.          ECS062
051810     STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
051810        DELIMITED BY '  ' INTO SAV-KSM-CITY-STATE
051810     END-STRING
02460      MOVE CO-ZIP                 TO  SAV-KSM-ZIP.                 ECS062
02461      MOVE RUN-YR                 TO  SAV-KSM-YY.                  ECS062
02462      MOVE RUN-MO                 TO  SAV-KSM-MM.                  ECS062
02463      MOVE RUN-DA                 TO  SAV-KSM-DD.                  ECS062
02464                                                                   ECS062
02465  8199-EXIT.                                                       ECS062
02466      EXIT.                                                        ECS062
02467  EJECT                                                            ECS062
02468  8200-RD-CCM-RTN.                                                 ECS062
02469      READ COMM-TRAN-IN                                            ECS062
02470          AT END                                                   ECS062
02471              MOVE HIGH-VALUES    TO  COMM-PREM-RECORD             ECS062
02472                                      CUR-CP-SEQ                   ECS062
02473              GO TO 8299-EXIT.                                     ECS062
02474                                                                   ECS062
02475      COPY ELCCOMM1.                                               ECS062
02476                                                                   ECS062
02477      MOVE CP-CARR-GROUP          TO  CUR-CP-CARR-GROUP.           ECS062
02478      MOVE CP-REMIT               TO  CUR-CP-RESP-NO.              ECS062
02479      MOVE CP-ACCOUNT             TO  CUR-CP-ACCOUNT.              ECS062
02480                                                                   ECS062
02481  8299-EXIT.                                                       ECS062
02482      EXIT.                                                        ECS062
02483                                                                   ECS062
02484  8300-FORM-CCM-RTN.                                               ECS062
02485      MOVE '#'                    TO  CCM-WK.                      ECS062
02486      MOVE CP-CARR-GROUP          TO  CCW-CARR-GROUP.              ECS062
02487      MOVE CP-REMIT               TO  CCW-RESP-NO.                 ECS062
02488      MOVE CP-ACCOUNT             TO  CCW-ACCOUNT.                 ECS062
02489      MOVE CP-AM-NO               TO  CCW-AM-NO.                   ECS062
02490      MOVE +0                     TO  CCW-PREM                     ECS062
02491                                      CCW-COMM                     ECS062
02492                                      CCW-PMTS                     ECS062
02493                                      CCW-BEG-BAL                  ECS062
02494                                      CCW-END-BAL                  ECS062
02495                                      CCW-OV-L-PREM                ECS062
02496                                      CCW-OV-A-PREM                ECS062
02497                                      CCW-OV-LIFE                  ECS062
02498                                      CCW-OV-AH                    ECS062
02499                                      CCW-OV-B-L-PREM              ECS062
02500                                      CCW-OV-B-A-PREM              ECS062
02501                                      CCW-OV-B-LIFE                ECS062
02502                                      CCW-OV-B-AH.                 ECS062
02503      MOVE CP-AC-DESC             TO  CCW-NAME.                    ECS062
02504      MOVE 'N'                    TO  CCW-BAL-CTL.                 ECS062
02505                                                                   ECS062
02506      IF CP-ACCTG                                                  ECS062
02507          GO TO 8310-ACCTG.                                        ECS062
02508                                                                   ECS062
02509      IF CP-OVERWT                                                 ECS062
02510          GO TO 8320-OVERWT.                                       ECS062
02511                                                                   ECS062
02512      IF CP-RC-OVERWT                                              ECS062
02513          GO TO 8330-RC-OVERWT.                                    ECS062
02514                                                                   ECS062
02515      GO TO 8399-EXIT.                                             ECS062
02516                                                                   ECS062
02517  8310-ACCTG.                                                      ECS062
02518      MOVE '5'                    TO  CCW-TYPE.                    ECS062
02519      MOVE LOW-VALUE              TO  CCW-AM-NO.                   ECS062
02520                                                                   ECS062
02521      COMPUTE CCW-PMTS  =  CP-AC-PMT  -  CP-AC-CHG.                ECS062
02522                                                                   ECS062
02523      GO TO 8340-WRT.                                              ECS062
02524                                                                   ECS062
02525  8320-OVERWT.                                                     ECS062
02526      IF CP-OW-LF-PRM-BILLED  IS NOT NUMERIC                       ECS062
02527          MOVE ZEROS              TO  CP-OW-LF-PRM-BILLED.         ECS062
02528                                                                   ECS062
02529      IF CP-OW-AH-PRM-BILLED  IS NOT NUMERIC                       ECS062
02530          MOVE ZEROS              TO  CP-OW-AH-PRM-BILLED.         ECS062
02531                                                                   ECS062
02532      MOVE CP-OW-LF-PRM           TO  CCW-OV-L-PREM.               ECS062
02533      ADD  CP-OW-LF-PRM-ALT       TO  CCW-OV-L-PREM.               ECS062
02534      MOVE CP-OW-AH-PRM           TO  CCW-OV-A-PREM.               ECS062
02535      MOVE CP-OW-LF-PRM-BILLED    TO  CCW-OV-B-L-PREM.             ECS062
02536      MOVE CP-OW-AH-PRM-BILLED    TO  CCW-OV-B-A-PREM.             ECS062
02537                                                                   ECS062
02538  8330-RC-OVERWT.                                                  ECS062
02539      IF CP-OW-LF-COM-BILLED  IS NOT NUMERIC                       ECS062
02540          MOVE ZEROS              TO  CP-OW-LF-COM-BILLED.         ECS062
02541                                                                   ECS062
02542      IF CP-OW-AH-COM-BILLED  IS NOT NUMERIC                       ECS062
02543          MOVE ZEROS              TO  CP-OW-AH-COM-BILLED.         ECS062
02544                                                                   ECS062
02545      MOVE '6'                    TO  CCW-TYPE.                    ECS062
02546      MOVE CP-OW-LF-COM           TO  CCW-OV-LIFE.                 ECS062
02547      ADD  CP-OW-LF-COM-ALT       TO  CCW-OV-LIFE.                 ECS062
02548      MOVE CP-OW-AH-COM           TO  CCW-OV-AH.                   ECS062
02549      MOVE CP-OW-LF-COM-BILLED    TO  CCW-OV-B-LIFE.               ECS062
02550      MOVE CP-OW-AH-COM-BILLED    TO  CCW-OV-B-AH.                 ECS062
02551                                                                   ECS062
02552  8340-WRT.                                                        ECS062
02553      PERFORM 8400-WRT-CCM-RTN  THRU  8499-EXIT.                   ECS062
02554                                                                   ECS062
02555  8399-EXIT.                                                       ECS062
02556      EXIT.                                                        ECS062
02557                                                                   ECS062
02558  8400-WRT-CCM-RTN.                                                ECS062
02559      WRITE CCM-WK.                                                ECS062
02560                                                                   ECS062
02561  8499-EXIT.                                                       ECS062
02562      EXIT.                                                        ECS062
02563  EJECT                                                            ECS062
02564  8500-START-ERACCT.                                               ECS062
02565      IF CP-ISSUE    OR CP-CANCEL OR                               ECS062
02566         CP-RC-ISSUE OR CP-RC-CANCEL                               ECS062
02567           NEXT SENTENCE                                           ECS062
02568         ELSE                                                      ECS062
02569           GO TO 8599-EXIT.                                        ECS062
02570                                                                   ECS062
02571      MOVE LOW-VALUES            TO AM-CONTROL-PRIMARY.            ECS062
02572                                                                   ECS062
02573      MOVE DTE-CLASIC-COMPANY-CD TO AM-COMPANY-CD.                 ECS062
02574      MOVE CP-CARRIER            TO AM-CARRIER.                    ECS062
02575      MOVE CP-GROUPING           TO AM-GROUPING.                   ECS062
02576      MOVE CP-STATE              TO AM-STATE.                      ECS062
02577      MOVE CP-ACCOUNT            TO AM-ACCOUNT.                    ECS062
02578                                                                   ECS062
02579      START ERACCT                                                 ECS062
02580             KEY NOT LESS AM-CONTROL-PRIMARY.                      ECS062
02581                                                                   ECS062
02582      IF ERACCT-FILE-STATUS NOT = ZERO                             ECS062
02583          MOVE 'ERROR OCCURED START - ERACCT'                      ECS062
02584                                   TO WS-ABEND-MESSAGE             ECS062
02585          MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS          ECS062
02586          GO TO ABEND-PGM.                                         ECS062
02587                                                                   ECS062
02588  8550-READ-ACCT.                                                  ECS062
02589      READ ERACCT NEXT RECORD.                                     ECS062
02590                                                                   ECS062
02591      IF ERACCT-STAT-1 = '1'                                       ECS062
02592          MOVE 'NO ACCT FOUND - EOF' TO WS-ABEND-MESSAGE           ECS062
02593          MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS          ECS062
02594          GO TO ABEND-PGM.                                         ECS062
02595                                                                   ECS062
02596      IF ERACCT-FILE-STATUS NOT = ZERO                             ECS062
02597          MOVE 'ERROR OCCURED READ - ERACCT'                       ECS062
02598                                   TO WS-ABEND-MESSAGE             ECS062
02599          MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS          ECS062
02600          GO TO ABEND-PGM.                                         ECS062
02601                                                                   ECS062
02602      IF AM-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 ECS062
02603          MOVE 'NO ACCT FOUND - CD' TO WS-ABEND-MESSAGE            ECS062
02604          MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS          ECS062
02605          GO TO ABEND-PGM.                                         ECS062
02606                                                                   ECS062
02607      IF CP-CARRIER  = AM-CARRIER  AND                             ECS062
02608         CP-GROUPING = AM-GROUPING AND                             ECS062
02609         CP-STATE    = AM-STATE    AND                             ECS062
02610         CP-ACCOUNT  = AM-ACCOUNT                                  ECS062
02611            NEXT SENTENCE                                          ECS062
02612          ELSE                                                     ECS062
02613          DISPLAY '** NO ACCT   CP-CAR= ' CP-CARRIER               ECS062
02614                              ' AM-CAR= ' AM-CARRIER               ECS062
02615                              ' CP-ACC= ' CP-ACCOUNT               ECS062
02616                              ' AM-ACC= ' AM-ACCOUNT               ECS062
02617          GO TO 8599-EXIT.                                         ECS062
02618                                                                   ECS062
02619      MOVE AM-EFFECTIVE-DT       TO DC-BIN-DATE-1.                 ECS062
02620      MOVE ' '                   TO DC-OPTION-CODE.                ECS062
02621      PERFORM DATE-CONVERSION.                                     ECS062
02622      MOVE DC-GREG-DATE-CYMD     TO WS-EFF-DATE-R.                 ECS062
02623      MOVE WS-EFF-DATE-R         TO WS-EFF-DATE.                   ECS062
02624                                                                   ECS062
02625      IF AM-EXPIRATION-DT = HIGH-VALUES                            ECS062
02626          MOVE '99999999'        TO WS-EXP-DATE-R                  ECS062
02627       ELSE                                                        ECS062
02628          MOVE AM-EXPIRATION-DT  TO DC-BIN-DATE-1                  ECS062
02629          MOVE ' '               TO DC-OPTION-CODE                 ECS062
02630          PERFORM DATE-CONVERSION                                  ECS062
02631          MOVE DC-GREG-DATE-CYMD  TO WS-EXP-DATE-R                 ECS062
02632          MOVE WS-EFF-DATE-R      TO WS-EFF-DATE.                  ECS062
02633                                                                   ECS062
02634      IF CP-EFF LESS    WS-EFF-DATE OR                             ECS062
02635         CP-EFF GREATER WS-EXP-DATE                                ECS062
02636            GO TO 8550-READ-ACCT.                                  ECS062
02637                                                                   ECS062
02638      MOVE AM-REI-TABLE          TO C-REI-TABLE.                   ECS062
02639      MOVE AM-STATUS             TO C-ACCT-STATUS.                 ECS062
02640                                                                   ECS062
02641  8599-EXIT.                                                       ECS062
02642       EXIT.                                                       ECS062
02643       EJECT                                                       ECS062
02644  8800-PRT-RTN.                                                    ECS062
02645      MOVE P-REC                  TO  PRT.                         ECS062
02646      MOVE P-CCSW                 TO  X.                           ECS062
02647                                                                   ECS062
02648      IF P-CCSW  = SPACE-1                                         ECS062
02649          ADD +1                  TO  LNCTR                        ECS062
02650      ELSE                                                         ECS062
02651          IF P-CCSW  = SPACE-2                                     ECS062
02652              ADD +2              TO  LNCTR                        ECS062
02653          ELSE                                                     ECS062
02654              IF P-CCSW  = SPACE-3                                 ECS062
02655                  ADD +3          TO  LNCTR.                       ECS062
02656                                                                   ECS062
02657      MOVE SPACES                 TO  P-REC.                       ECS062
02658      MOVE SPACE-2                TO  P-CCSW.                      ECS062
02659                                                                   ECS062
02660  8810-COPY-PRT-RTN.                                               ECS062
02661                              COPY ELCPRT2.                        ECS062
02662                                                                   ECS062
02663  8899-EXIT.                                                       ECS062
02664      EXIT.                                                        ECS062
02665                                                                   ECS062
02666  9990-E-O-J.                                                      ECS062
02667      MOVE +0                     TO  PGCTR.                       ECS062
02668                                                                   ECS062
02669      PERFORM 2910-PRT-FIRST-PAGE.                                 ECS062
02670                                                                   ECS062
02671      MOVE 'BALANCE FORWARD'      TO  P-TOT-DESC1.                 ECS062
02672      MOVE T-BEG-BAL              TO  P-TOT-PREMIUM1.              ECS062
02673      MOVE SPACE-2                TO  P-CCSW.                      ECS062
02674                                                                   ECS062
02675      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
02676                                                                   ECS062
02677      MOVE 'THIS MONTHS PREMIUM'  TO  P-TOT-DESC1.                 ECS062
02678      MOVE T-TOT-PRM              TO  P-TOT-PREMIUM1.              ECS062
02679      MOVE SPACE-2                TO  P-CCSW.                      ECS062
02680                                                                   ECS062
02681      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
02682                                                                   ECS062
02683      MOVE 'LESS COMPENSATION'    TO  P-TOT-DESC1.                 ECS062
02684      MOVE T-TOT-COM              TO  P-TOT-PREMIUM1.              ECS062
02685      MOVE SPACE-2                TO  P-CCSW.                      ECS062
02686                                                                   ECS062
02687      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
02688                                                                   ECS062
02689      MOVE 'LESS PAYMENTS'        TO  P-TOT-DESC1.                 ECS062
02690      MOVE T-TOT-PMT              TO  P-TOT-PREMIUM1.              ECS062
02691      MOVE SPACE-2                TO  P-CCSW.                      ECS062
02692                                                                   ECS062
02693      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
02694                                                                   ECS062
02695      IF T-WRT-OFF  NOT = +0                                       ECS062
02696          MOVE 'WRITTEN OFF'      TO  P-TOT-DESC1                  ECS062
02697          MOVE T-WRT-OFF          TO  P-TOT-PREMIUM1               ECS062
02698          MOVE SPACE-2            TO  P-CCSW                       ECS062
02699          PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                   ECS062
02700                                                                   ECS062
02701      MOVE ALL '-'                TO  P-TOT-PREM-X1.               ECS062
02702      MOVE SPACE-2                TO  P-CCSW.                      ECS062
02703                                                                   ECS062
02704      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
02705                                                                   ECS062
02706      MOVE 'BAL.CARRIED FORWARD'  TO  P-TOT-DESC1.                 ECS062
02707      MOVE T-END-BAL              TO  P-TOT-PREMIUM1.              ECS062
02708      MOVE SPACE-2                TO  P-CCSW.                      ECS062
02709                                                                   ECS062
02710      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS062
02711                                                                   ECS062
02712      IF T-TOT-COM  NOT = T-ADJ-COM                                ECS062
02713          MOVE 'ADJUSTED COMMISSION'  TO  P-TOT-DESC1              ECS062
02714          MOVE T-ADJ-COM              TO  P-TOT-PREMIUM1           ECS062
02715          MOVE SPACE-3                TO  P-CCSW                   ECS062
02716          PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                   ECS062
02717                                                                   ECS062
02718      IF DTE-CLIENT  =  'KSM'                                      ECS062
02719          CLOSE BILLING-DATA-FILE                                  ECS062
02720          MOVE +0                 TO  PGCTR                        ECS062
02721          PERFORM 2910-PRT-FIRST-PAGE                              ECS062
02722          MOVE 'TAPE DETAIL RECORDS        = '                     ECS062
02723                                  TO  P-TOT-DESC1                  ECS062
02724          MOVE KSM-DETAIL-COUNT   TO  P-TOT-REC-COUNT              ECS062
02725          MOVE SPACE-2            TO  P-CCSW                       ECS062
02726          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    ECS062
02727          MOVE 'TAPE SUMMARY RECORDS       = '                     ECS062
02728                                  TO  P-TOT-DESC1                  ECS062
02729          MOVE KSM-COUNT          TO  P-TOT-REC-COUNT              ECS062
02730          MOVE SPACE-2            TO  P-CCSW                       ECS062
02731          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    ECS062
02732          MOVE 'TOTAL LIFE WRITTEN PREMIUM = '                     ECS062
02733                                  TO  P-TOT-DESC1                  ECS062
02734          MOVE KSM-GROSS-LIFE-PREM                                 ECS062
02735                                  TO  P-TOT-PREMIUM1               ECS062
02736          MOVE SPACE-2            TO  P-CCSW                       ECS062
02737          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    ECS062
02738          MOVE 'TOTAL LIFE REFUNDS         = '                     ECS062
02739                                  TO  P-TOT-DESC1                  ECS062
02740          MOVE KSM-GROSS-LIFE-REFUNDS                              ECS062
02741                                  TO  P-TOT-PREMIUM1               ECS062
02742          MOVE SPACE-2            TO  P-CCSW                       ECS062
02743          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    ECS062
02744          MOVE 'TOTAL NET LIFE PREMIUM     = '                     ECS062
02745                                  TO  P-TOT-DESC1                  ECS062
02746          MOVE KSM-NET-LIFE-PREM  TO  P-TOT-PREMIUM1               ECS062
02747          MOVE SPACE-2            TO  P-CCSW                       ECS062
02748          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    ECS062
02749          MOVE 'TOTAL A&H WRITTEN PREMIUM  = '                     ECS062
02750                                  TO  P-TOT-DESC1                  ECS062
02751          MOVE KSM-GROSS-AH-PREM  TO  P-TOT-PREMIUM1               ECS062
02752          MOVE SPACE-2            TO  P-CCSW                       ECS062
02753          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    ECS062
02754          MOVE 'TOTAL A&H REFUNDS          = '                     ECS062
02755                                  TO  P-TOT-DESC1                  ECS062
02756          MOVE KSM-GROSS-AH-REFUNDS                                ECS062
02757                                  TO  P-TOT-PREMIUM1               ECS062
02758          MOVE SPACE-2            TO  P-CCSW                       ECS062
02759          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    ECS062
02760          MOVE 'TOTAL NET A&H  PREMIUM     = '                     ECS062
02761                                  TO  P-TOT-DESC1                  ECS062
02762          MOVE KSM-NET-AH-PREM    TO  P-TOT-PREMIUM1               ECS062
02763          MOVE SPACE-2            TO  P-CCSW                       ECS062
02764          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    ECS062
02765          MOVE 'TOTAL LIFE COMMISSION      = '                     ECS062
02766                                  TO  P-TOT-DESC1                  ECS062
02767          MOVE KSM-LIFE-COMM      TO  P-TOT-PREMIUM1               ECS062
02768          MOVE SPACE-2            TO  P-CCSW                       ECS062
02769          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    ECS062
02770          MOVE 'TOTAL A&H  COMMISSION      = '                     ECS062
02771                                  TO  P-TOT-DESC1                  ECS062
02772          MOVE KSM-AH-COMM        TO  P-TOT-PREMIUM1               ECS062
02773          MOVE SPACE-2            TO  P-CCSW                       ECS062
02774          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    ECS062
02775          MOVE 'TOTAL WRITTEN PREMIUM      = '                     ECS062
02776                                  TO  P-TOT-DESC1                  ECS062
02777          MOVE KSM-GROSS-WRITTEN  TO  P-TOT-PREMIUM1               ECS062
02778          MOVE SPACE-2            TO  P-CCSW                       ECS062
02779          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    ECS062
02780          MOVE 'TOTAL PREMIUM REFUNDED     = '                     ECS062
02781                                  TO  P-TOT-DESC1                  ECS062
02782          MOVE KSM-GROSS-REFUNDED                                  ECS062
02783                                  TO  P-TOT-PREMIUM1               ECS062
02784          MOVE SPACE-2            TO  P-CCSW                       ECS062
02785          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    ECS062
02786          MOVE 'TOTAL NET PREMIUM          = '                     ECS062
02787                                  TO  P-TOT-DESC1                  ECS062
02788          MOVE KSM-TOTAL-PREM     TO  P-TOT-PREMIUM1               ECS062
02789          MOVE SPACE-2            TO  P-CCSW                       ECS062
02790          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    ECS062
02791          MOVE 'TOTAL      COMMISSION      = '                     ECS062
02792                                  TO  P-TOT-DESC1                  ECS062
02793          MOVE KSM-TOTAL-COMM     TO  P-TOT-PREMIUM1               ECS062
02794          MOVE SPACE-2            TO  P-CCSW                       ECS062
02795          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    ECS062
02796          MOVE 'DUE THIS MONTH             = '                     ECS062
02797                                  TO  P-TOT-DESC1                  ECS062
02798          MOVE KSM-TOTAL-DUE      TO  P-TOT-PREMIUM1               ECS062
02799          MOVE SPACE-2            TO  P-CCSW                       ECS062
02800          PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                   ECS062
02801                                                                   ECS062
02802      CLOSE COMM-MSTR-IN   COMM-TRAN-IN                            ECS062
02803            COMM-MSTR-OUT  SUMM-TRAN-OUT                           ECS062
02804            PRNTR.                                                 ECS062
02805                                                                   ECS062
02806      IF DTE-CLIENT NOT = 'KSM'                                    ECS062
02807          GO TO 9995-CLOSE-FICH.                                   ECS062
02808                                                                   ECS062
02809      CLOSE ERACCT.                                                ECS062
02810                                                                   ECS062
02811      IF ERACCT-FILE-STATUS NOT = ZERO                             ECS062
02812          MOVE 'ERROR OCCURRED CLOSE - ERACCT'                     ECS062
02813                                  TO  WS-ABEND-MESSAGE             ECS062
02814          MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS          ECS062
02815          GO TO ABEND-PGM.                                         ECS062
02816                                                                   ECS062
02817  9995-CLOSE-FICH.                                                 ECS062
02818                              COPY ELCPRTC.                        ECS062
02819                                                                   ECS062
02820      MOVE ZEROS  TO RETURN-CODE.                                  ECS062
02821      GOBACK.                                                      ECS062
02822                                                                   ECS062
02823  DATE-CONVERSION  SECTION.                                        ECS062
02824      CALL 'ELDATCX'  USING DATE-CONVERSION-DATA.                  ECS062
02825                                                                   ECS062
02826  DATE-EXIT.                                                       ECS062
02827       EXIT.                                                       ECS062
02828                                                                   ECS062
02829  ABEND-PGM SECTION.                                               ECS062
02830                              COPY ELCABEND.                       ECS062
02831 /                                                                 ECS062
02832  LCP-WRITE-POS-PRT SECTION.                                       ECS062
02833      IF LCP-ASA = '+'                                             ECS062
02834          WRITE PRT AFTER 0 LINE                                   ECS062
02835      ELSE                                                         ECS062
02836      IF LCP-ASA = ' '                                             ECS062
02837          WRITE PRT AFTER ADVANCING 1 LINE                         ECS062
02838      ELSE                                                         ECS062
02839      IF LCP-ASA = '0'                                             ECS062
02840          WRITE PRT AFTER ADVANCING 2 LINE                         ECS062
02841      ELSE                                                         ECS062
02842      IF LCP-ASA = '-'                                             ECS062
02843          WRITE PRT AFTER ADVANCING 3 LINE                         ECS062
02844      ELSE                                                         ECS062
02845      IF LCP-ASA = '1'                                             ECS062
02846          WRITE PRT AFTER ADVANCING PAGE                           ECS062
02847      ELSE                                                         ECS062
02848      IF LCP-ASA = '2'                                             ECS062
02849          WRITE PRT AFTER ADVANCING LCP-CH2                        ECS062
02850      ELSE                                                         ECS062
02851      IF LCP-ASA = '3'                                             ECS062
02852          WRITE PRT AFTER ADVANCING LCP-CH3                        ECS062
02853      ELSE                                                         ECS062
02854      IF LCP-ASA = '4'                                             ECS062
02855          WRITE PRT AFTER ADVANCING LCP-CH4                        ECS062
02856      ELSE                                                         ECS062
02857      IF LCP-ASA = '5'                                             ECS062
02858          WRITE PRT AFTER ADVANCING LCP-CH5                        ECS062
02859      ELSE                                                         ECS062
02860      IF LCP-ASA = '6'                                             ECS062
02861          WRITE PRT AFTER ADVANCING LCP-CH6                        ECS062
02862      ELSE                                                         ECS062
02863      IF LCP-ASA = '7'                                             ECS062
02864          WRITE PRT AFTER ADVANCING LCP-CH7                        ECS062
02865      ELSE                                                         ECS062
02866      IF LCP-ASA = '8'                                             ECS062
02867          WRITE PRT AFTER ADVANCING LCP-CH8                        ECS062
02868      ELSE                                                         ECS062
02869      IF LCP-ASA = '9'                                             ECS062
02870          WRITE PRT AFTER ADVANCING LCP-CH9                        ECS062
02871      ELSE                                                         ECS062
02872      IF LCP-ASA = 'A'                                             ECS062
02873          WRITE PRT AFTER ADVANCING LCP-CH10                       ECS062
02874      ELSE                                                         ECS062
02875      IF LCP-ASA = 'B'                                             ECS062
02876          WRITE PRT AFTER ADVANCING LCP-CH11                       ECS062
02877      ELSE                                                         ECS062
02878      IF LCP-ASA = 'C'                                             ECS062
02879          WRITE PRT AFTER ADVANCING LCP-CH12                       ECS062
02880      ELSE                                                         ECS062
02881      IF LCP-ASA = 'V'                                             ECS062
02882          WRITE PRT AFTER ADVANCING LCP-P01                        ECS062
02883      ELSE                                                         ECS062
02884      IF LCP-ASA = 'W'                                             ECS062
02885          WRITE PRT AFTER ADVANCING LCP-P02                        ECS062
02886      ELSE                                                         ECS062
02887      DISPLAY 'ASA CODE ERROR'.                                    ECS062
02888  LCP-WRITE-END-PRT.                                               ECS062
02889      EXIT.                                                        ECS062
