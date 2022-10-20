00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   ECS045
00003  PROGRAM-ID.                 AHL045EXT.                            LV021
00004 *              PROGRAM CONVERTED BY                               ECS045
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS045
00006 *              CONVERSION DATE 04/28/94 09:51:04.                 ECS045
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS045
00008 *                            VMOD=2.046                              CL*15
00009                                                                   ECS045
00010                                                                   ECS045
00011 *AUTHOR.        LOGIC, INC.                                       ECS045
00012 *               DALLAS, TEXAS.                                    ECS045
00013                                                                   ECS045
00014 *DATE-COMPILED.                                                   ECS045
00015                                                                   ECS045
00016 *SECURITY.   *****************************************************ECS045
00017 *            *                                                   *ECS045
00018 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS045
00019 *            *                                                   *ECS045
00020 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS045
00021 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS045
00022 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS045
00023 *            *                                                   *ECS045
00024 *            *****************************************************ECS045
00025                                                                   ECS045
00026 *REMARKS.                                                         ECS045
00027 *        THIS PROGRAM WILL READ THE EP-EC FILE AND PRINT          ECS045
00028 *        REINSURANCE CEDING STATEMENTS.                           ECS045
060903******************************************************************
060903*                   C H A N G E   L O G
060903*
060903* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
060903*-----------------------------------------------------------------
060903*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
060903* EFFECTIVE    NUMBER
060903*-----------------------------------------------------------------
060903* 060903    2001061800003  PEMA  CHANGE MORT RESERVES TO LFUEP
060903*                                FOR DCC ONLY
100703* 100703    2003080800002  PEMA  ADD SUPER GAP PROCESSING
092705* 092705  CR2005050300006  PEMA  ADD SPP LEASES
032707* 032707    2007032100006  PEMA  ADD EXCISE TAX CAPABILITY
071207* 071207                   PEMA  
061008* 061008    2008052100004  AJRA  CREATE EXTRACT FILE OF ACCOUNT TOTALS
122408* 122408  CR2008120300002  PEMA  ADD ADJUSTMENT PROCESSING
032910* 032910  IR2010032300001  PEMA  SET OVERRIDES FOR SPEC 045
020612* 020612  CR2011110200001  AJRA  AHL EXTRACT
022912* 022912  CR2011110200001  AJRA  ADD TOTAL COMMISSION TO EXTRACT
032612* 032612  CR2011110200001  PEMA  AHL CHANGES
060903******************************************************************
00029                                                                   ECS045
00030  ENVIRONMENT DIVISION.                                            ECS045
00031  CONFIGURATION SECTION.                                           ECS045
00032  SPECIAL-NAMES.                                                   ECS045
00033  INPUT-OUTPUT SECTION.                                            ECS045
00034  FILE-CONTROL.                                                    ECS045
00035                                                                   ECS045
00036      SELECT  SORT-WORK       ASSIGN TO SYS001-DA-FBA1-S-SORTWK1.  ECS045
00037      SELECT  REIN-WORK       ASSIGN TO SYS002-UT-FBA1-S-SYS002.   ECS045
00038      SELECT  PRNTR           ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS045
00039      SELECT  PRNTR-2         ASSIGN TO SYS009-UR-1403-S-SYS009.   ECS045
00040      SELECT  EPEC-FILE       ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS045
00041      SELECT  REIN-TBL-FILE   ASSIGN TO ERRTBLT                    ECS045
00042                              ACCESS IS DYNAMIC                    ECS045
00043                              ORGANIZATION IS INDEXED              ECS045
00044                              FILE STATUS IS REIN-FILE-STATUS      ECS045
00045                              RECORD KEY IS RE-CONTROL-PRIMARY.    ECS045
00046      SELECT  ACC-MSTR        ASSIGN TO ERACCTT                    ECS045
00047                              ACCESS IS SEQUENTIAL                 ECS045
00048                              ORGANIZATION IS INDEXED              ECS045
00049                              FILE STATUS IS ERACCTT-FILE-STATUS   ECS045
00050                              RECORD KEY IS AM-CONTROL-PRIMARY.    ECS045
00051      SELECT  DISK-DATE       ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS045
00052      SELECT  FICH            ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS045
00053      SELECT  FICH-2          ASSIGN TO SYS021-UT-2400-S-SYS021.   ECS045
061008     SELECT  DATA-OUT        ASSIGN TO SYS022-UT-2400-S-SYS022
061008                             ORGANIZATION IS LINE SEQUENTIAL.
020612     SELECT  DATA-OUT-AHL    ASSIGN TO SYS024-UT-2400-S-SYS024
020612                             ORGANIZATION IS LINE SEQUENTIAL.
00054  EJECT                                                            ECS045
00055  DATA DIVISION.                                                   ECS045
00056  FILE SECTION.                                                    ECS045
00057                                                                   ECS045
00058  SD  SORT-WORK.                                                   ECS045
00059                                                                   ECS045
00060  01  SW-REC.                                                      ECS045
00061      12  SW-REPORT-ID        PIC X.                               ECS045
00062      12  SW-REPORT-A-CNTL.                                        ECS045
00063          16  SW-A-REI-PRIME  PIC X(3).                            ECS045
00064          16  SW-A-REI-GRP-A  PIC X(6).                            ECS045
00065          16  SW-A-CARR       PIC X.                               ECS045
00066          16  SW-A-COMP       PIC X(6).                            ECS045
00067          16  SW-A-STATE      PIC XX.                              ECS045
00068          16  SW-A-ACCT       PIC X(10).                           ECS045
00069          16  SW-A-REI-SUB    PIC X(3).                            ECS045
00070          16  SW-A-EXP-DT     PIC 9(11)  COMP-3.                      CL*16
00071          16  SW-A-EFF-DT     PIC 9(11)  COMP-3.                      CL*16
00072      12  SW-REPORT-B-CNTL.                                        ECS045
00073          16  SW-B-CARR       PIC X.                               ECS045
00074          16  SW-B-REI-GRP-B  PIC X(6).                            ECS045
00075          16  SW-B-REI-PRIME  PIC X(3).                            ECS045
00076          16  SW-B-STATE      PIC XX.                              ECS045
00077          16  SW-B-REI-SUB    PIC X(3).                            ECS045
00078          16  FILLER          PIC X(28).                           ECS045
00079      12  SW-REPORT-C-CNTL.                                        ECS045
00080          16  SW-C-FROM       PIC X(30).                           ECS045
00081          16  SW-C-TO         PIC X(30).                           ECS045
00082          16  SW-C-TYPE       PIC X.                               ECS045
00083      12  SW-REPORT-D-CNTL.                                        ECS045
00084          16  SW-D-REI-GRP    PIC X(6).                            ECS045
00085          16  SW-D-CARR       PIC X.                               ECS045
00086          16  SW-D-REI-PRIME  PIC X(3).                            ECS045
00087          16  FILLER          PIC X(33).                           ECS045
00088      12  SW-REPORT-E-CNTL.                                        ECS045
00089          16  SW-E-REI-PRIME  PIC X(3).                            ECS045
00090          16  SW-E-REI-SUB    PIC X(3).                            ECS045
00091          16  FILLER          PIC X(37).                           ECS045
00092      12  SW-RECORD-DATA.                                          ECS045
PEMMOD*        16  FILLER          PIC X(1238).                         ECS045
PEMMOD*        16  FILLER          PIC X(1286).                         ECS045
032707         16  FILLER          PIC X(1289).
00094          16  SW-REI-COMP     PIC X(6).                            ECS045
00095                                                                   ECS045
00096  EJECT                                                            ECS045
00097  FD  REIN-WORK                                                    ECS045
00098      BLOCK CONTAINS 0 RECORDS
00099      RECORDING MODE F.                                            ECS045
00100                                                                   ECS045
00101 *01  RW-REC                  PIC X(1526).                         ECS045
032707 01  RW-REC                  PIC X(1529).
00102  EJECT                                                            ECS045
00103  FD  PRNTR                                                        ECS045
00104                     COPY ELCPRTFD.                                ECS045
00105                                                                   ECS045
00106  EJECT                                                            ECS045
00107  FD  PRNTR-2                                                      ECS045
00108      RECORDING MODE F                                             ECS045
00109      BLOCK CONTAINS 0 RECORDS.
00110                                                                   ECS045
00111  01  PRT-2.                                                       ECS045
00112      12  P-CTL-2            PIC X.                                ECS045
00113      12  P-DATA-2           PIC X(150).                           ECS045
00114                                                                   ECS045
00115  EJECT                                                            ECS045
00116  FD  EPEC-FILE                                                    ECS045
00117                              COPY ECSEPCFD.                       ECS045
00118                                                                   ECS045
00119                              COPY ECSEPC01.                       ECS045
00120  EJECT                                                            ECS045
00121  FD  REIN-TBL-FILE                                                ECS045
00122                              COPY ECSRTFDD.                       ECS045
00123                                                                   ECS045
00124                              COPY ERCREIN.                        ECS045
00125  EJECT                                                            ECS045
00126  FD  ACC-MSTR.                                                       CL*20
00127                              COPY ERCACCT.                        ECS045
00128  EJECT                                                            ECS045
00129  FD  DISK-DATE                                                    ECS045
00130                              COPY ELCDTEFD.                       ECS045
00131  EJECT                                                            ECS045
00132  FD  FICH                                                         ECS045
00133                              COPY ELCFCHFD.                       ECS045
00134  EJECT                                                            ECS045
00135                                                                   ECS045
00136  FD  FICH-2                                                       ECS045
00137      RECORDING MODE F                                             ECS045
00138      BLOCK CONTAINS 0 RECORDS.
00139                                                                   ECS045
00140                                                                   ECS045
00141  01  FICH-REC-2                 PIC X(150).                       ECS045
00142                                                                   ECS045
061008 FD  DATA-OUT
061008     RECORDING MODE F 
061008     BLOCK CONTAINS 0 RECORDS.
061008                             
061008 01  DATA-OUT-REC               PIC X(2300).
020612
020612 FD  DATA-OUT-AHL
020612     RECORDING MODE F 
020612     BLOCK CONTAINS 0 RECORDS.
020612                             
022912 01  DATA-OUT-AHL-REC           PIC X(4500).
00143                                                                   ECS045
00144  EJECT                                                            ECS045
00145  WORKING-STORAGE SECTION.                                         ECS045
00146  77  FILLER  PIC X(32) VALUE '********************************'.  ECS045
00147  77  FILLER  PIC X(32) VALUE '*    ECS045 WORKING STORAGE    *'.  ECS045
00148  77  FILLER  PIC X(32) VALUE '******** VMOD=2.046 ************'.     CL*15
00149                                                                   ECS045
00150  77  EPECS-READ-COUNT        PIC S9(7)     COMP-3 VALUE +0.       ECS045
00151  77  EPECS-SELECTED          PIC S9(7)     COMP-3 VALUE +0.       ECS045
00152  77  EPECS-SORTED-SORT1      PIC S9(7)     COMP-3 VALUE +0.       ECS045
00153  77  EPECS-RETURNED-SORT1    PIC S9(7)     COMP-3 VALUE +0.       ECS045
00154  77  RW-WRITE-CNT            PIC S9(7)     COMP-3 VALUE +0.       ECS045
00155  77  SA                      PIC S999      COMP.                  ECS045
00156  77  SB                      PIC S999      COMP.                  ECS045
00157  77  SC                      PIC S999      COMP.                  ECS045
00158  77  SE                      PIC S999      COMP.                  ECS045
00159  77  SF                      PIC S999      COMP.                  ECS045
00160  77  SG                      PIC S999      COMP.                  ECS045
00161  77  SH                      PIC S999      COMP.                  ECS045
00162  77  RX                      PIC S9(4)     COMP.                  ECS045
00163  77  FR                      PIC S999      COMP.                  ECS045
00164  77  LINE-CNT                PIC S999      COMP-3 VALUE +99.      ECS045
00165  77  PGM-SUB                 PIC S999      COMP-3 VALUE +45.      ECS045
00166  77  PAGE-CNT                PIC S9(5)     COMP-3 VALUE +0.       ECS045
00167  77  WRK-CALC                PIC S9(11)V99 COMP-3 VALUE +0.       ECS045
00168  77  X                       PIC X.                               ECS045
00169  77  HDR-SW                  PIC X.                               ECS045
00170  77  WS-IBNR                 PIC 999.                             ECS045
00171  77  REIN-PREMIUMS           PIC S9(11)V99 COMP-3 VALUE +0.       ECS045
00172  77  LAST-FEE-THRU-AMT       PIC S9(7)V99  COMP-3 VALUE +0.       ECS045
00173  77  FEE-AMOUNT              PIC S9(11)V99 COMP-3 VALUE +0.       ECS045
00174  77  WS-MRC-SC               PIC S9(9)V99  COMP-3 VALUE +0.       ECS045
00175  77  WS-MRO-SC               PIC S9(9)V99  COMP-3 VALUE +0.       ECS045
00176  77  WS-MRC-SE               PIC S9(9)V99  COMP-3 VALUE +0.       ECS045
00177  77  WS-MRO-SE               PIC S9(9)V99  COMP-3 VALUE +0.       ECS045
00178  77  WS-MRC-SC-PERCENT       PIC S9V9(5)   COMP-3 VALUE +0.       ECS045
00179  77  WS-MRO-SC-PERCENT       PIC S9V9(5)   COMP-3 VALUE +0.       ECS045
00180  77  WS-MRC-SE-PERCENT       PIC S9V9(5)   COMP-3 VALUE +0.       ECS045
00181  77  WS-MRO-SE-PERCENT       PIC S9V9(5)   COMP-3 VALUE +0.       ECS045
00182  77  WS-CANC-RESERVE         PIC S9(11)V99 COMP-3 VALUE +0.       ECS045
00183  77  WS-BEG-CRSV             PIC S9(11)V99 COMP-3 VALUE +0.       ECS045
00184  77  WS-END-CRSV             PIC S9(11)V99 COMP-3 VALUE +0.       ECS045
       77  WS-DISPLAY-AMT          PIC ZZZ,ZZZ.99.
061008 77  WS-DO-SUB               PIC S9(3)     COMP-3 VALUE +0.       
00185                                                                   ECS045
00186  01  WS-FILE-STATUS.                                              ECS045
00187      12  REIN-FILE-STATUS.                                        ECS045
00188          16  REIN-FILE-STATUS-1  PIC X.                           ECS045
00189          16  REIN-FILE-STATUS-2  PIC X.                           ECS045
00190      12  ERACCTT-FILE-STATUS     PIC XX.                          ECS045
00191                                                                   ECS045
00192  01  WS-ABEND-STORAGE.                                            ECS045
00193      12  WS-RETURN-CODE          PIC S9(4)  VALUE +0 COMP.        ECS045
00194      12  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.         ECS045
00195      12  WS-ABEND-FILE-STATUS    PIC XX     VALUE ZEROS.          ECS045
00196      12  WS-ZERO                 PIC S9     VALUE ZERO COMP-3.    ECS045
00197      12  WS-ABEND-CODE           PIC S9(4).                       ECS045
00198      12  WORK-ABEND-CODE  REDEFINES  WS-ABEND-CODE.               ECS045
00199          16  WAC-1               PIC X.                           ECS045
00200          16  WAC-2               PIC X.                           ECS045
00201          16  WAC-3-4             PIC 99.                          ECS045
00202                                                                   ECS045
00203  01  WS.                                                          ECS045
PEMMOD     12  WS-TAX-WRK1         PIC S9V9(5) COMP-3 VALUE +0.         ECS045
PEMMOD     12  WS-TAX-WRK2         PIC S9V9(5) COMP-3 VALUE +0.         ECS045
PEMMOD     12  WS-SC-EARN          PIC S9(9)V99 COMP-3 VALUE +0.        ECS045
PEMMOD     12  WS-SE-EARN          PIC S9(9)V99 COMP-3 VALUE +0.        ECS045
PEMMOD     12  WS-SC-NW            PIC S9(9)V99 COMP-3 VALUE +0.        ECS045
PEMMOD     12  WS-SE-NW            PIC S9(9)V99 COMP-3 VALUE +0.        ECS045
00204      12  SAVE-RR-REPORT-CNTL     PIC X(43).                       ECS045
00205      12  LIFE-PROCESS-SWITCH PIC X           VALUE 'N'.           ECS045
00206          88  LIFE-PERCENTS-AVAILABLE         VALUE 'Y'.           ECS045
00207      12  FINAL-TOT           PIC X           VALUE 'N'.           ECS045
00208          88  CREATE-FINAL-TOTAL              VALUE 'Y'.           ECS045
00209      12  WORK-DATE           PIC 9(11).                              CL**8
00210      12  WORK-DATE-R  REDEFINES  WORK-DATE.                          CL*18
00211          16  FILLER          PIC 999.                                CL**9
00212          16  WD-CC           PIC 99.                                 CL**9
00213          16  WD-YR           PIC 99.                                 CL**9
00214          16  WD-MO           PIC 99.                                 CL**9
00215          16  WD-DA           PIC 99.                                 CL**9
00216      12  WS-AGT.                                                  ECS045
00217          16  FILLER          PIC X(4).                            ECS045
00218          16  WS-REI-AGT      PIC X(6).                            ECS045
00219      12  WS-MORT-DESC.                                            ECS045
00220          16  FILLER          PIC XX          VALUE SPACES.        ECS045
00221          16  WS-MORT-DSC     PIC X(26).                           ECS045
00222          16  FILLER          PIC XX          VALUE '..'.          ECS045
00223      12  WS-MORT-STD.                                             ECS045
00224          16  FILLER          PIC X(30)       VALUE                ECS045
00225                  '  ............................'.                ECS045
00226      12  BIN-BEG-DATE        PIC XX          VALUE SPACES.        ECS045
00227      12  BIN-DTE-CONV-DT     PIC XX          VALUE SPACES.        ECS045
00228                                                                   ECS045
00229      EJECT                                                        ECS045
00230  01  HDR-1.                                                       ECS045
00231      12  FILLER          PIC X(52)   VALUE SPACES.                ECS045
00232      12  FILLER          PIC X(20)   VALUE 'STATEMENT OF REINSUR'.ECS045
00233      12  FILLER          PIC X(48)   VALUE 'ANCE CEDED'.          ECS045
00234      12  FILLER          PIC X(6)    VALUE 'ECS045'.              ECS045
00235      12  H1-RUN-CODE     PIC X       VALUE 'A'.                   ECS045
00236                                                                   ECS045
00237  01  HDR-1A.                                                      ECS045
00238      12  FILLER          PIC X(54)   VALUE SPACES.                ECS045
00239      12  FILLER          PIC X(16)   VALUE 'RECAP OF REINSUR'.    ECS045
00240      12  FILLER          PIC X(50)   VALUE 'ANCE CEDED'.          ECS045
00241      12  FILLER          PIC X(6)    VALUE 'ECS045'.              ECS045
00242      12  FILLER          PIC X       VALUE 'C'.                   ECS045
00243                                                                   ECS045
00244  01  HDR-2.                                                       ECS045
00245      12  FILLER          PIC X       VALUE SPACES.                ECS045
00246      12  H2-COMMENT-1    PIC X(10)   VALUE SPACES.                ECS045
00247      12  FILLER          PIC X       VALUE SPACES.                ECS045
00248      12  H2-CARR-1       PIC X       VALUE SPACES.                ECS045
00249      12  FILLER          PIC X(40)   VALUE SPACES.                ECS045
00250      12  H2-COMP         PIC X(30)   VALUE SPACES.                ECS045
00251      12  FILLER          PIC X(37)   VALUE SPACES.                ECS045
00252      12  H2-RUN-DT       PIC X(8)    VALUE SPACES.                ECS045
00253                                                                   ECS045
00254                                                                   ECS045
00255  01  HDR-3.                                                       ECS045
00256      12  FILLER          PIC X       VALUE SPACE.                 ECS045
00257      12  H3-COMMENT      PIC X(15)   VALUE 'REINSURED BY - '.     ECS045
00258      12  FILLER          PIC X       VALUE SPACES.                ECS045
00259      12  H3-COMMENT-1    PIC X(6)    VALUE SPACES.                ECS045
00260      12  FILLER          PIC X       VALUE SPACES.                ECS045
00261      12  H3-COMP-NAME    PIC X(30)   VALUE SPACES.                ECS045
00262      12  FILLER          PIC X(7)    VALUE SPACES.                ECS045
00263      12  H3-DATE         PIC X(18)   VALUE SPACES.                ECS045
00264      12  FILLER          PIC X(41)   VALUE SPACES.                ECS045
00265      12  FILLER          PIC X(5)    VALUE 'PAGE'.                ECS045
00266      12  HD-PG-NO        PIC ZZ,ZZZ.                              ECS045
00267                                                                   ECS045
00268  01  HDR-3A.                                                      ECS045
00269      12  FILLER          PIC X(8)    VALUE SPACE.                 ECS045
00270      12  FILLER          PIC X(13)   VALUE 'CEDED FROM - '.       ECS045
00271      12  H3A-COMP-NAME   PIC X(30)   VALUE SPACES.                ECS045
00272                                                                   ECS045
00273  01  HDR-3AX.                                                     ECS045
00274      12  FILLER          PIC X(21)   VALUE '           WITH'.     ECS045
00275                                                                   ECS045
00276  01  HDR-4-A.                                                     ECS045
00277      12  FILLER          PIC X       VALUE SPACE.                 ECS045
00278      12  H4-A-DESC       PIC X(51) VALUE                          ECS045
00279             'REINS ACCTREIN         CERT                   REINS'.ECS045
00280                                                                   ECS045
00281  01  HDR-4-B.                                                     ECS045
00282      12  FILLER          PIC X       VALUE SPACE.                 ECS045
00283      12  H4-B-DESC       PIC X(46) VALUE                          ECS045
00284             'PRIME GROUP-A CARRIER  GROUP  STATE   ACCOUNT'.      ECS045
00285                                                                   ECS045
00286      12  H4-B-DESC1      PIC X(46) VALUE                          ECS045
00287             '   ACCOUNT NAME                  SUB   EFF DT*'.     ECS045
00288                                                                   ECS045
00289      12  H4-B-DESC2      PIC X(10) VALUE                          ECS045
00290             '   EXP-DT*'.                                         ECS045
00291                                                                   ECS045
00292  01  HDR-5.                                                       ECS045
00293      12  FILLER          PIC X       VALUE SPACE.                 ECS045
00294      12  FILLER          PIC X.                                   ECS045
00295      12  H5-REIN-PRIME   PIC X(3).                                ECS045
00296      12  FILLER          PIC X(3).                                ECS045
00297      12  H5-REIN-GRP-A   PIC X(6).                                ECS045
00298      12  FILLER          PIC X(4).                                ECS045
00299      12  H5-CARR         PIC X.                                   ECS045
00300      12  FILLER          PIC X(4).                                ECS045
00301      12  H5-CERT-GRP     PIC X(6).                                ECS045
00302      12  FILLER          PIC X(4).                                ECS045
00303      12  H5-ST           PIC XX.                                  ECS045
00304      12  FILLER          PIC XX.                                  ECS045
00305      12  H5-ACCT         PIC X(10).                               ECS045
00306      12  FILLER          PIC XX.                                  ECS045
00307      12  H5-ACCT-NAME    PIC X(30).                               ECS045
00308      12  FILLER          PIC X.                                   ECS045
00309      12  H5-REIN-SUB     PIC X(3).                                ECS045
00310      12  FILLER          PIC XX.                                  ECS045
00311      12  H5-EFF-MO       PIC XX.                                  ECS045
00312      12  H5-DASH-1       PIC X.                                   ECS045
00313      12  H5-EFF-DA       PIC XX.                                  ECS045
00314      12  H5-DASH-2       PIC X.                                   ECS045
00315      12  H5-EFF-YR       PIC XX.                                  ECS045
00316      12  FILLER          PIC XX.                                  ECS045
00317      12  H5-EXP-MO       PIC XX.                                  ECS045
00318      12  H5-DASH-3       PIC X.                                   ECS045
00319      12  H5-EXP-DA       PIC XX.                                  ECS045
00320      12  H5-DASH-4       PIC X.                                   ECS045
00321      12  H5-EXP-YR       PIC XX.                                  ECS045
00322                                                                   ECS045
00323  01  HDR-5-B.                                                     ECS045
00324      12  FILLER          PIC X       VALUE SPACE.                 ECS045
00325      12  FILLER          PIC X(4).                                ECS045
00326      12  H5-B-CARR       PIC X.                                   ECS045
00327      12  FILLER          PIC X(5).                                ECS045
00328      12  H5-B-REIN-GRP-B PIC X(6).                                ECS045
00329      12  FILLER          PIC XX.                                  ECS045
00330      12  H5-B-REIN-PRIME PIC X(3).                                ECS045
00331      12  FILLER          PIC X(5).                                ECS045
00332      12  H5-B-ST         PIC XX.                                  ECS045
00333      12  FILLER          PIC XXXX.                                ECS045
00334      12  H5-B-REIN-SUB   PIC X(3).                                ECS045
00335                                                                   ECS045
00336  01  HDR-5-D.                                                     ECS045
00337      12  FILLER          PIC X       VALUE SPACE.                 ECS045
00338      12  H5-D-REIN-GRP   PIC X(6).                                ECS045
00339      12  FILLER          PIC X(4).                                ECS045
00340      12  H5-D-CARR       PIC X.                                   ECS045
00341      12  FILLER          PIC X(5).                                ECS045
00342      12  H5-D-REIN-PRIME PIC X(3).                                ECS045
00343      12  FILLER          PIC X(5).                                ECS045
00344                                                                   ECS045
00345  01  HDR-5-E.                                                     ECS045
00346      12  FILLER          PIC XX      VALUE SPACE.                 ECS045
00347      12  H5-E-REIN-PRIME PIC X(3).                                ECS045
00348      12  FILLER          PIC X(4).                                ECS045
00349      12  H5-E-REIN-SUB   PIC X(3).                                ECS045
00350                                                                   ECS045
00351  01  HDR-6.                                                       ECS045
00352      12  FILLER          PIC X       VALUE SPACE.                 ECS045
00353      12  FILLER          PIC X(14)   VALUE '*-------------'.      ECS045
00354      12  HDR-6A          PIC X(20)   VALUE SPACES.                ECS045
00355      12  FILLER          PIC X(14)   VALUE '-------------*'.      ECS045
00356      12  FILLER          PIC X(36)   VALUE SPACES.                ECS045
00357      12  FILLER          PIC X(14)   VALUE '*-------------'.      ECS045
00358      12  HDR-6B          PIC X(20)   VALUE SPACES.                ECS045
00359      12  FILLER          PIC X(14)   VALUE '-------------*'.      ECS045
00360                                                                   ECS045
00361  01  HDR-7.                                                       ECS045
00362      12  FILLER          PIC X       VALUE SPACE.                 ECS045
00363      12  FILLER          PIC X(8)    VALUE SPACES.                ECS045
00364      12  LIFE-PLUG-1     PIC X(6)    VALUE ' LIFE '.              ECS045
00365      12  FILLER          PIC X(7)    VALUE SPACES.                ECS045
00366      12  AH-PLUG-1       PIC X(6)    VALUE ' A/H  '.              ECS045
00367      12  FILLER          PIC X(13)   VALUE SPACES.                ECS045
00368      12  FILLER          PIC X(8)    VALUE 'TOTAL   '.            ECS045
00369      12  FILLER          PIC X(36)   VALUE SPACES.                ECS045
00370      12  FILLER          PIC X(8)    VALUE SPACES.                ECS045
00371      12  LIFE-PLUG-2     PIC X(6)    VALUE ' LIFE '.              ECS045
00372      12  FILLER          PIC X(7)    VALUE SPACES.                ECS045
00373      12  AH-PLUG-2       PIC X(6)    VALUE ' A/H  '.              ECS045
00374      12  FILLER          PIC X(13)   VALUE SPACES.                ECS045
00375      12  FILLER          PIC X(8)    VALUE 'TOTAL   '.            ECS045
00376                                                                   ECS045
00377  01  HDR-8.                                                       ECS045
00378      12  FILLER          PIC X       VALUE SPACE.                 ECS045
00379      12  FILLER          PIC X(39)   VALUE '    BEGINNING'.       ECS045
00380      12  FILLER          PIC X(49)   VALUE 'ENDING'.              ECS045
00381      12  FILLER          PIC X(35)   VALUE 'BEGINNING'.           ECS045
00382      12  FILLER          PIC X(9)    VALUE 'ENDING'.              ECS045
00383                                                                   ECS045
00384  01  HDR-6-FILLING.                                               ECS045
00385      12  CM-HEADING      PIC X(20)   VALUE '-- CURRENT  MONTH --'.ECS045
00386      12  CM-HEADING-2    PIC X(20)   VALUE '-- CURRENT MONTH  --'.ECS045
00387      12  CQ-HEADING      PIC X(20)   VALUE '- CURRENT  QUARTER -'.ECS045
00388      12  QTD-HEADING     PIC X(20)   VALUE '-- QUARTER-TO-DATE -'.ECS045
00389      12  QTD-HEADING-2   PIC X(20)   VALUE '- QUARTER-TO-DATE --'.ECS045
00390      12  YTD-HEADING     PIC X(20)   VALUE '--- YEAR-TO-DATE ---'.ECS045
00391      12  ITD-HEADING     PIC X(20)   VALUE ' INCEPTION-TO-DATE -'.ECS045
00392      12  ITD-HEADING-2   PIC X(20)   VALUE 'INCEPTION-TO-DATE  -'.ECS045
00393                                                                   ECS045
00394  01  DTL-1.                                                       ECS045
00395      12  FILLER          PIC X               VALUE SPACE.         ECS045
00396      12  D1-CLF          PIC ZZZZZ,ZZZ,ZZZ.99-.                   ECS045
00397      12  D1-CAH          PIC ZZZZZ,ZZZ,ZZZ.99-.                   ECS045
00398      12  D1-CT           PIC ZZZZZ,ZZZ,ZZZ.99-.                   ECS045
00399      12  D1-DESC         PIC X(30).                               ECS045
00400      12  D1-YLF          PIC ZZZZZ,ZZZ,ZZZ.99-.                   ECS045
00401      12  D1-YAH          PIC ZZZZZ,ZZZ,ZZZ.99-.                   ECS045
00402      12  D1-YT           PIC ZZZZZ,ZZZ,ZZZ.99-.                   ECS045
00403                                                                   ECS045
00404  01  DTL-2.                                                       ECS045
00405      12  D2-CBEG         PIC ZZZZZ,ZZZ,ZZZ.99-.                   ECS045
00406      12  FILLER          PIC X(15).                               ECS045
00407      12  D2-CEND         PIC ZZZZZ,ZZZ,ZZZ.99-.                   ECS045
00408      12  FILLER          PIC XXX.                                 ECS045
00409      12  D2-DESC         PIC X(30).                               ECS045
00410      12  FILLER          PIC XX.                                  ECS045
00411      12  D2-YBEG         PIC ZZZZZ,ZZZ,ZZZ.99-.                   ECS045
00412      12  FILLER          PIC X(15).                               ECS045
00413      12  D2-YEND         PIC ZZZZZ,ZZZ,ZZZ.99-.                   ECS045
00414                                                                   ECS045
00415      EJECT                                                        ECS045
00416  01  DESC-TABLE.                                                  ECS045
00417      12  FILLER              PIC X(34)   VALUE                    ECS045
00418              'GROSS PREMIUMS RECEIVED.......0Y01'.                ECS045
00419      12  FILLER              PIC X(34)   VALUE                    ECS045
00420              'LESS PREMIUMS RETURNED........ Y02'.                ECS045
00421      12  FILLER              PIC X(34)   VALUE                    ECS045
00422              'NET PREMIUMS RECEIVED......... Y03'.                ECS045
00423      12  FILLER              PIC X(34)   VALUE                    ECS045
00424              'PLUS BEGINNING RESERVES.......0Y04'.                ECS045
00425      12  FILLER              PIC X(34)   VALUE                    ECS045
00426              'LESS ENDING RESERVES.......... Y05'.                ECS045
00427      12  FILLER              PIC X(34)   VALUE                    ECS045
00428              'REINSURANCE PREMIUMS..........0Y06'.                ECS045
00429      12  FILLER              PIC X(34)   VALUE                    ECS045
00430              'LESS REINSURANCE FEE..........0Y07'.                ECS045
00431      12  FILLER              PIC X(34)   VALUE                    ECS045
00432              'LESS CLAIMS PAID.............. Y08'.                ECS045
00433      12  FILLER              PIC X(34)   VALUE                    ECS045
00434              '   CLAIMS PAID................ Y21'.                ECS045
00435      12  FILLER              PIC X(34)   VALUE                    ECS045
00436              '   CLAIM RESERVE BEGINNING.... Y16'.                ECS045
00437      12  FILLER              PIC X(34)   VALUE                    ECS045
00438              '   CLAIM RESERVE ENDING....... Y17'.                ECS045
00439      12  FILLER              PIC X(34)   VALUE                    ECS045
00440              'LESS INCURRED CLAIMS.......... Y18'.                ECS045
00441      12  FILLER              PIC X(34)   VALUE                    ECS045
00442              'LESS ACCOUNT COMM............. Y09'.                ECS045
00443      12  FILLER              PIC X(34)   VALUE                    ECS045
00444              'LESS OVERWRITE COMM........... Y10'.                ECS045
00445      12  FILLER              PIC X(34)   VALUE                    ECS045
00446              'LESS STATE TAXES.............. Y11'.                ECS045
00447      12  FILLER              PIC X(34)   VALUE                    ECS045
00448              'CANCELLATION RESERVE EXPENSE.. Y12'.                ECS045
00449      12  FILLER              PIC X(34)   VALUE                    ECS045
032707*            'PAYMENTS AND/OR ADJUSTMENTS... Y13'.
122408             'LESS EXCISE TAXES/ ADJUSTMENTS Y13'.                ECS045
00451      12  FILLER              PIC X(34)   VALUE                    ECS045
00452              'TOTAL DEDUCTIONS..............0Y14'.                ECS045
00453      12  FILLER              PIC X(34)   VALUE                    ECS045
00454              'NET BALANCE DUE REINSUROR.....0Y15'.                ECS045
00455      12  FILLER              PIC X(34)   VALUE                    ECS045
00456              'LIFE MORTALITY RESERVE         Y00'.                ECS045
00457      12  FILLER              PIC X(34)   VALUE                    ECS045
00458              '  ............................ Y01'.                ECS045
00459      12  FILLER              PIC X(34)   VALUE                    ECS045
00460              'A&H UNEARNED PREMIUM RESERVE.. Y02'.                ECS045
00461      12  FILLER              PIC X(34)   VALUE                    ECS045
00462              'LIFE CLAIM RESERVE             Y00'.                ECS045
00463      12  FILLER              PIC X(34)   VALUE                    ECS045
00464              '     ACCRUED.................. Y03'.                ECS045
00465      12  FILLER              PIC X(34)   VALUE                    ECS045
00466              '     UNREPORTED............... Y04'.                ECS045
00467      12  FILLER              PIC X(34)   VALUE                    ECS045
00468              '     TOTAL.................... Y05'.                ECS045
00469      12  FILLER              PIC X(34)   VALUE                    ECS045
00470              'A&H CLAIM RESERVE              Y00'.                ECS045
00471      12  FILLER              PIC X(34)   VALUE                    ECS045
00472              '     ACCRUED.................. Y06'.                ECS045
00473      12  FILLER              PIC X(34)   VALUE                    ECS045
00474              '     FUTURE................... Y07'.                ECS045
00475      12  FILLER              PIC X(34)   VALUE                    ECS045
00476              '     UNREPORTED............... Y08'.                ECS045
00477      12  FILLER              PIC X(34)   VALUE                    ECS045
00478              '     TOTAL.................... Y09'.                ECS045
00479      12  FILLER              PIC X(34)   VALUE                    ECS045
00480              'CUSTODIAL ACCOUNT             0Y00'.                ECS045
00481      12  FILLER              PIC X(34)   VALUE                    ECS045
00482              '     MINIMUM BALANCE.......... Y10'.                ECS045
00483      12  FILLER              PIC X(34)   VALUE                    ECS045
00484              'DECREASED AMOUNT OF LIFE      0Y00'.                ECS045
00485      12  FILLER              PIC X(34)   VALUE                    ECS045
00486              '     INSURANCE IN FORCE....... Y11'.                ECS045
00487      12  FILLER              PIC X(34)   VALUE                    ECS045
00488              'PREMIUM RESERVES              0Y00'.                ECS045
00489      12  FILLER              PIC X(34)   VALUE                    ECS045
00490              '     LIFE UNEARNED............ Y12'.                ECS045
00491      12  FILLER              PIC X(34)   VALUE                    ECS045
00492              '     A&H UNEARNED............. Y13'.                ECS045
00493                                                                   ECS045
00494  01  DESC-TABLE-R    REDEFINES DESC-TABLE.                        ECS045
00495      12  DT-TB-ENT   OCCURS 38.                                   ECS045
00496          16  DT-ENTRY.                                            ECS045
CIDMOD             20  FILLER  PIC X(29).                               ECS045
CIDMOD*            20  FILLER  PIC X(22).                               ECS045
CIDMOD*            20  DT-FEE  PIC Z.9999-.                             ECS045
00499              20  FILLER  PIC X.                                   ECS045
00500          16  DT-CTL-CHAR PIC X.                                   ECS045
00501          16  DT-PRT-SW   PIC X.                                   ECS045
00502          16  DT-SS       PIC S99.                                 ECS045
00503                                                                   ECS045
00504  EJECT                                                            ECS045
00505  01  DESC-TABLE-2.                                                ECS045
00506      12  FILLER              PIC X(12)   VALUE                    ECS045
00507              'GROSS PREM'.                                        ECS045
00508      12  FILLER              PIC X(12)   VALUE                    ECS045
00509              'PREM RETURN'.                                       ECS045
00510      12  FILLER              PIC X(12)   VALUE                    ECS045
00511              'NET PREM...'.                                       ECS045
00512      12  FILLER              PIC X(12)   VALUE                    ECS045
00513              'BEG RESV...'.                                       ECS045
00514      12  FILLER              PIC X(12)   VALUE                    ECS045
00515              'END RESV...'.                                       ECS045
00516      12  FILLER              PIC X(12)   VALUE                    ECS045
00517              'REIN PREM..'.                                       ECS045
00518      12  FILLER              PIC X(12)   VALUE                    ECS045
00519              'REIN FEE...'.                                       ECS045
00520      12  FILLER              PIC X(12)   VALUE                    ECS045
00521              'CLAIMS PAID.'.                                      ECS045
00522      12  FILLER              PIC X(12)   VALUE                    ECS045
00523              ' CLAIMS PAID'.                                      ECS045
00524      12  FILLER              PIC X(12)   VALUE                    ECS045
00525              ' RESV BEGIN '.                                      ECS045
00526      12  FILLER              PIC X(12)   VALUE                    ECS045
00527              ' RESV ENDING'.                                      ECS045
00528      12  FILLER              PIC X(12)   VALUE                    ECS045
00529              'INC CLAIMS  '.                                      ECS045
00530      12  FILLER              PIC X(12)   VALUE                    ECS045
00531              'ACCT COMM...'.                                      ECS045
00532      12  FILLER              PIC X(12)   VALUE                    ECS045
00533              'OW   COMM...'.                                      ECS045
00534      12  FILLER              PIC X(12)   VALUE                    ECS045
00535              'STATE TAXES.'.                                      ECS045
00536      12  FILLER              PIC X(12)   VALUE                    ECS045
00537              'CNC RESV EXP'.                                      ECS045
00538      12  FILLER              PIC X(12)   VALUE                    ECS045
00539 *            'PAYMENTS/ADJ'.                                      ECS045
                   'EXCISE TAXES'.
00540      12  FILLER              PIC X(12)   VALUE                    ECS045
00541              'TOTAL DEDUCT'.                                      ECS045
00542      12  FILLER              PIC X(12)   VALUE                    ECS045
00543              'NET BAL DUE.'.                                      ECS045
00544      12  FILLER              PIC X(12)   VALUE                    ECS045
00545              'LF MORT RESV'.                                      ECS045
00546      12  FILLER              PIC X(12)   VALUE                    ECS045
00547              '  ..........'.                                      ECS045
00548      12  FILLER              PIC X(12)   VALUE                    ECS045
00549              'AH PREM RESV'.                                      ECS045
00550      12  FILLER              PIC X(12)   VALUE                    ECS045
00551              'LF CLM RESV.'.                                      ECS045
00552      12  FILLER              PIC X(12)   VALUE                    ECS045
00553              '  ACCRUED...'.                                      ECS045
00554      12  FILLER              PIC X(12)   VALUE                    ECS045
00555              '  UNREPORTED'.                                      ECS045
00556      12  FILLER              PIC X(12)   VALUE                    ECS045
00557              '     TOTAL..'.                                      ECS045
00558      12  FILLER              PIC X(12)   VALUE                    ECS045
00559              'AH CLM RESV.'.                                      ECS045
00560      12  FILLER              PIC X(12)   VALUE                    ECS045
00561              '  ACCRUED...'.                                      ECS045
00562      12  FILLER              PIC X(12)   VALUE                    ECS045
00563              '  FUTURE....'.                                      ECS045
00564      12  FILLER              PIC X(12)   VALUE                    ECS045
00565              '  UNREPORTED'.                                      ECS045
00566      12  FILLER              PIC X(12)   VALUE                    ECS045
00567              '     TOTAL..'.                                      ECS045
00568      12  FILLER              PIC X(12)   VALUE                    ECS045
00569              'CUSTODIAL   '.                                      ECS045
00570      12  FILLER              PIC X(12)   VALUE                    ECS045
00571              '  MIN BAL...'.                                      ECS045
00572      12  FILLER              PIC X(12)   VALUE                    ECS045
00573              'DECREASED LF'.                                      ECS045
00574      12  FILLER              PIC X(12)   VALUE                    ECS045
00575              '  IN FORCE..'.                                      ECS045
00576      12  FILLER              PIC X(12)   VALUE                    ECS045
00577              'PREM RESVS..'.                                      ECS045
00578      12  FILLER              PIC X(12)   VALUE                    ECS045
00579              '  LF UNEARN.'.                                      ECS045
00580      12  FILLER              PIC X(12)   VALUE                    ECS045
00581              '  AH UNEARN.'.                                      ECS045
00582                                                                   ECS045
00583  01  DESC-TABLE-A-2 REDEFINES DESC-TABLE-2.                       ECS045
00584      12  DT-ENTRY-2    OCCURS 38.                                 ECS045
00585          16  FILLER      PIC X(12).                               ECS045
00586                                                                   ECS045
00587  01  DESC-TABLE-X-2.                                              ECS045
00588      12  FILLER          PIC X(16)    VALUE                       ECS045
00589          '            0Y01'.                                      ECS045
00590      12  FILLER          PIC X(16)    VALUE                       ECS045
00591          '             Y02'.                                      ECS045
00592      12  FILLER          PIC X(16)    VALUE                       ECS045
00593          '             Y03'.                                      ECS045
00594      12  FILLER          PIC X(16)    VALUE                       ECS045
00595          '            0Y04'.                                      ECS045
00596      12  FILLER          PIC X(16)    VALUE                       ECS045
00597          '             Y05'.                                      ECS045
00598      12  FILLER          PIC X(16)    VALUE                       ECS045
00599          '            0Y06'.                                      ECS045
00600      12  FILLER          PIC X(16)    VALUE                       ECS045
00601          '            0Y07'.                                      ECS045
00602      12  FILLER          PIC X(16)    VALUE                       ECS045
00603          '             Y08'.                                      ECS045
00604      12  FILLER          PIC X(16)    VALUE                       ECS045
00605          '             Y21'.                                      ECS045
00606      12  FILLER          PIC X(16)    VALUE                       ECS045
00607          '             Y16'.                                      ECS045
00608      12  FILLER          PIC X(16)    VALUE                       ECS045
00609          '             Y17'.                                      ECS045
00610      12  FILLER          PIC X(16)    VALUE                       ECS045
00611          '             Y18'.                                      ECS045
00612      12  FILLER          PIC X(16)    VALUE                       ECS045
00613          '             Y09'.                                      ECS045
00614      12  FILLER          PIC X(16)    VALUE                       ECS045
00615          '             Y10'.                                      ECS045
00616      12  FILLER          PIC X(16)    VALUE                       ECS045
00617          '             Y11'.                                      ECS045
00618      12  FILLER          PIC X(16)    VALUE                       ECS045
00619          '             Y12'.                                      ECS045
00620      12  FILLER          PIC X(16)    VALUE                       ECS045
00621          '             Y13'.                                      ECS045
00622      12  FILLER          PIC X(16)    VALUE                       ECS045
00623          '            0Y14'.                                      ECS045
00624      12  FILLER          PIC X(16)    VALUE                       ECS045
00625          '            0Y15'.                                      ECS045
00626      12  FILLER          PIC X(16)    VALUE                       ECS045
00627          '             Y00'.                                      ECS045
00628      12  FILLER          PIC X(16)    VALUE                       ECS045
00629          '             Y01'.                                      ECS045
00630      12  FILLER          PIC X(16)    VALUE                       ECS045
00631          '             Y02'.                                      ECS045
00632      12  FILLER          PIC X(16)    VALUE                       ECS045
00633          '             Y00'.                                      ECS045
00634      12  FILLER          PIC X(16)    VALUE                       ECS045
00635          '             Y03'.                                      ECS045
00636      12  FILLER          PIC X(16)    VALUE                       ECS045
00637          '             Y04'.                                      ECS045
00638      12  FILLER          PIC X(16)    VALUE                       ECS045
00639          '             Y05'.                                      ECS045
00640      12  FILLER          PIC X(16)    VALUE                       ECS045
00641          '             Y00'.                                      ECS045
00642      12  FILLER          PIC X(16)    VALUE                       ECS045
00643          '             Y06'.                                      ECS045
00644      12  FILLER          PIC X(16)    VALUE                       ECS045
00645          '             Y07'.                                      ECS045
00646      12  FILLER          PIC X(16)    VALUE                       ECS045
00647          '             Y08'.                                      ECS045
00648      12  FILLER          PIC X(16)    VALUE                       ECS045
00649          '             Y09'.                                      ECS045
00650      12  FILLER          PIC X(16)    VALUE                       ECS045
00651          '            0Y00'.                                      ECS045
00652      12  FILLER          PIC X(16)    VALUE                       ECS045
00653          '             Y10'.                                      ECS045
00654      12  FILLER          PIC X(16)    VALUE                       ECS045
00655          '            0Y00'.                                      ECS045
00656      12  FILLER          PIC X(16)    VALUE                       ECS045
00657          '             Y10'.                                      ECS045
00658      12  FILLER          PIC X(16)    VALUE                       ECS045
00659          '            0Y00'.                                      ECS045
00660      12  FILLER          PIC X(16)    VALUE                       ECS045
00661          '             Y12'.                                      ECS045
00662      12  FILLER          PIC X(16)    VALUE                       ECS045
00663          '             Y13'.                                      ECS045
00664  01  DESC-TABLE-R-2 REDEFINES DESC-TABLE-X-2.                     ECS045
00665      12  DT-TB-ENT-2   OCCURS 38.                                 ECS045
00666          16  DT-ENTRY-2-2.                                        ECS045
00667              20  FILLER    PIC X(4).                              ECS045
CIDMOD             20  FILLER    PIC X(7).                              ECS045
CIDMOD*            20  DT-FEE-2  PIC Z.9999-.                           ECS045
00669              20  FILLER    PIC X.                                 ECS045
00670          16  DT-CTL-CHAR-2 PIC X.                                 ECS045
00671          16  DT-PRT-SW-2   PIC X.                                 ECS045
00672          16  DT-SS-2       PIC S99.                               ECS045
00673      EJECT                                                        ECS045
00674                                                                   ECS045
00675  01  HDR-6-2.                                                     ECS045
00676      12  FILLER          PIC X(17)   VALUE SPACES.                ECS045
00677      12  FILLER          PIC X       VALUE SPACE.                 ECS045
00678      12  FILLER          PIC X(13)   VALUE '*------------'.       ECS045
00679      12  HDR-6A-2        PIC X(17)   VALUE SPACES.                ECS045
00680      12  FILLER          PIC X(13)   VALUE '------------*'.       ECS045
00681      12  FILLER          PIC X(13)   VALUE '*------------'.       ECS045
00682      12  HDR-6B-2        PIC X(17)   VALUE SPACES.                ECS045
00683      12  FILLER          PIC X(13)   VALUE '------------*'.       ECS045
00684      12  FILLER          PIC X(13)   VALUE '*------------'.       ECS045
00685      12  HDR-6C-2        PIC X(17)   VALUE SPACES.                ECS045
00686      12  FILLER          PIC X(13)   VALUE '------------*'.       ECS045
00687                                                                   ECS045

00688  01  HDR-7-2.                                                     ECS045
00689      12  FILLER          PIC X(18)   VALUE SPACES.                ECS045
00690      12  FILLER          PIC X       VALUE SPACE.                 ECS045
00691      12  FILLER          PIC X(2)    VALUE SPACES.                ECS045
00692      12  LIFE-PLUG-12    PIC X(6)    VALUE ' LIFE '.              ECS045
00693      12  FILLER          PIC X(10)   VALUE SPACES.                ECS045
00694      12  AH-PLUG-12      PIC X(6)    VALUE ' A/H  '.              ECS045
00695      12  FILLER          PIC X(8)    VALUE SPACES.                ECS045
00696      12  FILLER          PIC X(8)    VALUE 'TOTAL   '.            ECS045
00697      12  FILLER          PIC X(7)    VALUE SPACES.                ECS045
00698      12  LIFE-PLUG-22    PIC X(6)    VALUE ' LIFE '.              ECS045
00699      12  FILLER          PIC X(10)   VALUE SPACES.                ECS045
00700      12  AH-PLUG-22      PIC X(6)    VALUE ' A/H  '.              ECS045
00701      12  FILLER          PIC X(8)    VALUE SPACES.                ECS045
00702      12  FILLER          PIC X(8)    VALUE 'TOTAL   '.
041108     12  FILLER          PIC X(8)    VALUE SPACES.
041108     12  LIFE-PLUG-32    PIC X(6)    VALUE ' LIFE '.
041108     12  FILLER          PIC X(11)   VALUE SPACES.
041108     12  AH-PLUG-32      PIC X(6)    VALUE ' A/H  '.
041108     12  FILLER          PIC X(9)    VALUE SPACES.
041108     12  FILLER          PIC X(5)    VALUE 'TOTAL'.
00703                                                                   ECS045
00704  01  HDR-8-2.                                                     ECS045
00705      12  FILLER          PIC X(15)   VALUE SPACE.                 ECS045
00706      12  FILLER          PIC X(34)   VALUE '    BEGINNING'.       ECS045
00707      12  FILLER          PIC X(19)   VALUE 'ENDING'.              ECS045
00708      12  FILLER          PIC X(30)   VALUE 'BEGINNING'.           ECS045
00709      12  FILLER          PIC X(9)    VALUE 'ENDING'.              ECS045
00710      12  FILLER          PIC X(5)    VALUE SPACES.                ECS045
00711      12  FILLER          PIC X(30)   VALUE 'BEGINNING'.           ECS045
00712      12  FILLER          PIC X(9)    VALUE 'ENDING'.              ECS045
00713                                                                   ECS045
00714                                                                   ECS045
00715  01  DTL-1-2.                                                     ECS045
00716      12  FILLER          PIC X               VALUE SPACE.         ECS045
00717      12  D1-DESC-2       PIC X(12).                               ECS045
00718      12  D1-CLF-2        PIC ZZZ,ZZZ,ZZZ.99-.                     ECS045
00719      12  D1-CAH-2        PIC ZZZ,ZZZ,ZZZ.99-.                     ECS045
00720      12  D1-CT-2         PIC ZZZ,ZZZ,ZZZ.99-.                     ECS045
00721      12  D1-YLF-2        PIC ZZZ,ZZZ,ZZZ.99-.                     ECS045
00722      12  D1-YAH-2        PIC ZZZ,ZZZ,ZZZ.99-.                     ECS045
00723      12  D1-YT-2         PIC ZZZ,ZZZ,ZZZ.99-.                     ECS045
00724      12  D1-OLF-2        PIC ZZZZ,ZZZ,ZZZ.99-.                    ECS045
00725      12  D1-OAH-2        PIC ZZZZ,ZZZ,ZZZ.99-.                    ECS045
00726      12  D1-OT-2         PIC ZZZZ,ZZZ,ZZZ.99-.                    ECS045
00727                                                                   ECS045
00728  01  DTL-2-2.                                                     ECS045
00729      12  FILLER          PIC X.                                   ECS045
00730      12  D2-DESC-2       PIC X(12).                               ECS045
00731      12  D2-CBEG-2       PIC ZZZZZ,ZZZ,ZZZ.99-.                   ECS045
00732      12  FILLER          PIC X(10).                               ECS045
00733      12  D2-CEND-2       PIC ZZZZZ,ZZZ,ZZZ.99-.                   ECS045
00734      12  FILLER          PIC XXX.                                 ECS045
00735      12  FILLER          PIC XX.                                  ECS045
00736      12  D2-YBEG-2       PIC ZZZZZ,ZZZ,ZZZ.99-.                   ECS045
00737      12  FILLER          PIC X(10).                               ECS045
00738      12  D2-YEND-2       PIC ZZZZZ,ZZZ,ZZZ.99-.                   ECS045
00739      12  D2-OBEG-2       PIC ZZZZZ,ZZZ,ZZZ.99-.                   ECS045
00740      12  FILLER          PIC X(10).                               ECS045
00741      12  D2-OEND-2       PIC ZZZZZ,ZZZ,ZZZ.99-.                   ECS045
00742                                                                   ECS045
00743  01  TRLR-1.                                                      ECS045
00744      12  FILLER          PIC X(50)  VALUE SPACES.                 ECS045
00745      12  FILLER          PIC X(50)                                ECS045
00746                 VALUE 'NEGATIVE INDICATES NET CEDED'.             ECS045
00747                                                                   ECS045
00748  01  TRLR-2.                                                      ECS045
00749      12  FILLER          PIC X(50)  VALUE SPACES.                 ECS045
00750      12  FILLER          PIC X(50)                                ECS045
00751                 VALUE 'POSITIVE INDICATES NET ASSUMED'.           ECS045
00752                                                                   ECS045
00753      EJECT                                                        ECS045
00754  01  RR-REC.                                                      ECS045
00755      12  RR-REPORT-ID        PIC X.                               ECS045
00756          88  RR-REPORT-ID-A           VALUE 'A'.                  ECS045
00757          88  RR-REPORT-ID-B           VALUE 'B'.                  ECS045
00758          88  RR-REPORT-ID-C           VALUE 'C'.                  ECS045
00759          88  RR-REPORT-ID-D           VALUE 'D'.                  ECS045
00760          88  RR-REPORT-ID-E           VALUE 'E'.                  ECS045
00761      12  RR-REPORT-A-CNTL.                                        ECS045
00762          16  RR-A-REI-PRIME  PIC X(3).                            ECS045
00763          16  RR-A-REI-GRP-A  PIC X(6).                            ECS045
00764          16  RR-A-CARR       PIC X.                               ECS045
00765          16  RR-A-CERT-GROUP PIC X(6).                            ECS045
00766          16  RR-A-STATE      PIC XX.                              ECS045
00767          16  RR-A-ACCT       PIC X(10).                           ECS045
00768          16  RR-A-REI-SUB    PIC X(3).                            ECS045
00769          16  RR-A-EXP-DT     PIC 9(11)    COMP-3.                    CL*10
00770          16  RR-A-EFF-DT     PIC 9(11)    COMP-3.                    CL*10
00771      12  RR-REPORT-B-CNTL.                                        ECS045
00772          16  RR-B-CARR       PIC X.                               ECS045
00773          16  RR-B-REI-GRP-B  PIC X(6).                            ECS045
00774          16  RR-B-REI-PRIME  PIC X(3).                            ECS045
00775          16  RR-B-STATE      PIC XX.                              ECS045
00776          16  RR-B-REI-SUB    PIC X(3).                            ECS045
00777          16  FILLER          PIC X(28).                           ECS045
00778      12  RR-REPORT-C-CNTL.                                        ECS045
00779          16  RR-C-REI-NAME   PIC X(30).                           ECS045
00780          16  RR-C-CEDE-NAME  PIC X(30).                           ECS045
00781          16  RR-C-TYPE       PIC X.                               ECS045
00782      12  RR-REPORT-D-CNTL.                                        ECS045
00783          16  RR-D-REI-GRP    PIC X(6).                            ECS045
00784          16  RR-D-CARR       PIC X.                               ECS045
00785          16  RR-D-REI-PRIME  PIC X(3).                            ECS045
00786          16  FILLER          PIC X(33).                           ECS045
00787      12  RR-REPORT-E-CNTL.                                        ECS045
00788          16  RR-E-REI-PRIME  PIC X(3).                            ECS045
00789          16  RR-E-REI-SUB    PIC X(3).                            ECS045
00790          16  FILLER          PIC X(37).                           ECS045
00791      12  RR-RECORD-DATA.                                          ECS045
00792          16  RR-ACC-NAME     PIC X(30).                           ECS045
00793          16  RR-MORT         PIC X(4).                            ECS045
00794          16  RR-PE-LF        PIC X.                               ECS045
00795          16  RR-PE-AH        PIC X.                               ECS045
00796          16  RR-PRT-ST       PIC X.                               ECS045
00797          16  RR-PRT-OW       PIC X.                               ECS045
00798          16  RR-PRT-CRSV     PIC X.                               ECS045
00799          16  RR-CLAIM-CODE   PIC X.                               ECS045
00800          16  RR-0-PE-FEE-LF  PIC X.                               ECS045
00801          16  RR-0-PE-FEE-AH  PIC X.                               ECS045
00802          16  RR-FEE-LF       PIC S9V9(4)     COMP-3.              ECS045
00803          16  RR-FEE-AH       PIC S9V9(4)     COMP-3.
032707         16  RR-EXCISE-TAX   PIC S9V9(4)     COMP-3.
00804          16  RR-ST-TAX       PIC S9V9(4)     COMP-3.              ECS045
00805          16  RR-LF-TAX       PIC S9V9(4)     COMP-3.              ECS045
00806          16  RR-AH-TAX       PIC S9V9(4)     COMP-3.              ECS045
00807          16  RR-AH-PR-PCT    PIC S9V9(4)     COMP-3.              ECS045
00808          16  RR-AH-78-PCT    PIC S9V9(4)     COMP-3.              ECS045
PEMMOD*        16  RR-ACCUMS       PIC X(1152).                         ECS045
PEMMOD         16  RR-ACCUMS       PIC X(1200).                         ECS045
00810          16  RR-PE-COMM-LF   PIC X.                               ECS045
00811          16  RR-PE-COMM-AH   PIC X.                               ECS045
00812          16  RR-PE-TAX-LF    PIC X.                               ECS045
00813          16  RR-PE-TAX-AH    PIC X.                               ECS045
00814          16  RR-PI-CLM-LF    PIC X.                               ECS045
00815          16  RR-PI-CLM-AH    PIC X.                               ECS045
00816          16  RR-LF-IBNR-PCT  PIC V999         COMP-3.             ECS045
00817          16  RR-AH-IBNR-PCT  PIC V999         COMP-3.             ECS045
00818          16  RR-045A-SW      PIC X.                               ECS045
00819              88  ACCOUNT-NOT-PRINTED      VALUE 'N'.              ECS045
00820          16  FILLER          PIC X(6).                            ECS045
00821          16  RR-LF-PR-PCT    PIC S9V9(4)       COMP-3.            ECS045
00822          16  RR-LF-78-PCT    PIC S9V9(4)       COMP-3.            ECS045
00823          16  RR-REI-COMP.                                         ECS045
00824              20  RR-REI-PRIME PIC X(3).                           ECS045
00825              20  RR-REI-SUB   PIC X(3).                           ECS045
00826      EJECT                                                        ECS045
00827  01  BEG-DATE                PIC 9(11).                              CL**3
00828  01  BEG-DATE-R  REDEFINES BEG-DATE.                                 CL**3
00829      12 FILLER               PIC 999.                             ECS045
00830      12  BD-CCYY             PIC 9(04).                           ECS045
00831      12  BD-CCYR REDEFINES BD-CCYY.                               ECS045
00832          16  BD-CC           PIC 99.                              ECS045
00833          16  BD-YR           PIC 99.                              ECS045
00834      12  BD-MO               PIC 99.                              ECS045
00835      12  BD-DA               PIC 99.                              ECS045
00836                                                                   ECS045
00837  01  PRV-MONTH           PIC 9(11).                                  CL**3
00838  01  PRV-MONTH-R REDEFINES PRV-MONTH.                                CL**3
00839      12  FILLER          PIC 999.                                 ECS045
00840      12  PM-CCYY         PIC 9(04).                               ECS045
00841      12  PM-CCYR REDEFINES PM-CCYY.                               ECS045
00842          16  PM-CC       PIC 99.                                  ECS045
00843          16  PM-YR       PIC 99.                                  ECS045
00844      12  PM-MO           PIC 99.                                  ECS045
00845      12  PM-DA           PIC 99.                                  ECS045
00846                                                                   ECS045
00847  01  PRV-QTR             PIC 9(11).                                  CL**3
00848  01  PRV-QTR-R REDEFINES PRV-QTR.                                    CL**3
00849      12  FILLER          PIC 999.                                 ECS045
00850      12  PQ-CCYY         PIC 9(04).                               ECS045
00851      12  PQ-CCYR REDEFINES PQ-CCYY.                               ECS045
00852          16  PQ-CC       PIC 99.                                  ECS045
00853          16  PQ-YR       PIC 99.                                  ECS045
00854      12  PQ-MO           PIC 99.                                  ECS045
00855      12  PQ-DA           PIC 99.                                  ECS045
00856                                                                   ECS045
00857  01  RUN-DT                  PIC 9(11).                              CL**6
00858  01  RUN-DT-R REDEFINES RUN-DT.                                      CL**3
00859      12  FILLER              PIC 999.                             ECS045
00860      12  RD-CCYY             PIC 9(04).                           ECS045
00861      12  RD-CCYR REDEFINES RD-CCYY.                               ECS045
00862          16  RD-CC           PIC 99.                              ECS045
00863          16  RD-YR           PIC 99.                              ECS045
00864      12  RD-MO               PIC 99.                              ECS045
00865      12  RD-DA               PIC 99.                              ECS045
00866                                                                   ECS045
00867      EJECT                                                        ECS045
00868 *                                                                 ECS045
00869 * THE DETAIL AND AGENTS ACCUMULATORS ARE USED AS FOLLOWS          ECS045
00870 *                                                                 ECS045
00871 *    OCCURRENCE 1 = LIFE BEGINNING DATA                           ECS045
00872 *    OCCURRENCE 2 = A&H BEGINNING DATA                            ECS045
00873 *    OCCURRENCE 3 = LIFE PREVIOUS MONTH DATA                      ECS045
00874 *    OCCURRENCE 4 = A&H PREVIOUS MONTH DATA                       ECS045
00875 *    OCCURRENCE 5 = LIFE CURRENT DATA                             ECS045
00876 *    OCCURRENCE 6 = A&H CURRENT DATA                              ECS045
00877 *    OCCURRENCE 7 = LF DATA TO CALCULATE ITD                      ECS045
00878 *    OCCURRENCE 8 = AH DATA TO CALCULATE ITD                      ECS045
00879 *                                                                 ECS045
00880 **********************************************************        ECS045
00881 * (DA-UPP)  UNEARNED PREMIUM PRO-RATA                    *        ECS045
00882 * (DA-UPR)  UNEARNED PREMIUM RULE-78                     *        ECS045
00883 * (DA-UCP)  UNEARNED ACCOUNT LEVEL COMMISSIONS PRO-RATA  *        ECS045
00884 * (DA-UCR)  UNEARNED ACCOUNT LEVEL COMMISSIONS RULE-78   *        ECS045
00885 * (DA-UOR)  UNEARNED OVERWRITE COMMISSIONS PRO-RATA      *        ECS045
00886 * (DA-UOP)  UNEARNED OVERWRITE COMMISSIONS RULE-78       *        ECS045
00887 * (DA-UP)   UNEARNED PREMIUM                             *        ECS045
00888 * (DA-UC)   UNEARNED COMMISSIONS ACCOUNT LEVEL           *        ECS045
00889 * (DA-UO)   UNEARNED COMMISSIONS OVERWRITE LEVEL         *        ECS045
00890 * (DA-CU)   IBNR CLAIM RSVS (IBNR PRCNT * UNEARNED PREM) *        ECS045
00891 **********************************************************        ECS045
00892  01  DETAIL-ACCUMULATORS     COMP-3.                              ECS045
00893      12  DETAIL-ACCUMS   OCCURS 8 TIMES.                          ECS045
00894          16  DA-ISGP     PIC S9(9)V99.                            ECS045
00895          16  DA-ISP      PIC S9(9)V99.                            ECS045
PEMMOD         16  DA-TAX      PIC S9(9)V99.                            ECS045
00896          16  DA-CNGP     PIC S9(9)V99.                            ECS045
00897          16  DA-CNP      PIC S9(9)V99.                            ECS045
00898          16  DA-UPR      PIC S9(9)V99.                            ECS045
00899          16  DA-UPP      PIC S9(9)V99.                            ECS045
00900          16  DA-UP       PIC S9(9)V99.                            ECS045
00901          16  DA-CLM      PIC S9(9)V99.                            ECS045
00902          16  DA-ISC      PIC S9(9)V99.                            ECS045
00903          16  DA-CNC      PIC S9(9)V99.                            ECS045
00904          16  DA-UCR      PIC S9(9)V99.                            ECS045
00905          16  DA-UCP      PIC S9(9)V99.                            ECS045
00906          16  DA-UC       PIC S9(9)V99.                            ECS045
00907          16  DA-ISO      PIC S9(9)V99.                            ECS045
00908          16  DA-CNO      PIC S9(9)V99.                            ECS045
00909          16  DA-UOR      PIC S9(9)V99.                            ECS045
00910          16  DA-UOP      PIC S9(9)V99.                            ECS045
00911          16  DA-UO       PIC S9(9)V99.                            ECS045
00912          16  DA-ADJ      PIC S9(9)V99.                            ECS045
00913          16  DA-MORT     PIC S9(9)V99.                            ECS045
00914          16  DA-CA       PIC S9(9)V99.                            ECS045
00915          16  DA-CF       PIC S9(9)V99.                            ECS045
00916          16  DA-CU       PIC S9(9)V99.                            ECS045
00917          16  DA-IF       PIC S9(9)V99.                            ECS045
00918                                                                   ECS045
00919      EJECT                                                        ECS045
00920 *                                                                 ECS045
00921 * THE PRINT ACCUMULATORS ARE USED AS FOLLOWS                      ECS045
00922 *                                                                 ECS045
00923 *    THERE ARE 9 BREAKS FOR EACH CATAGORY                         ECS045
00924 *                                                                 ECS045
00925 *        SET 1 = LEVEL 1 (DATE RANGE TOTALS)                      ECS045
00926 *        SET 2 = LEVEL 2 (REINS COMPANY SUB)                      ECS045
00927 *        SET 3 = LEVEL 3 (ACCOUNT TOTALS)                         ECS045
00928 *        SET 4 = LEVEL 4 (STATE TOTALS)                           ECS045
00929 *        SET 5 = LEVEL 5 (CERT GROUP TOTALS)                      ECS045
00930 *        SET 6 = LEVEL 6 (CARRIER TOTALS)                         ECS045
00931 *        SET 7 = LEVEL 7 (REINS GROUP-A TOTALS)                   ECS045
00932 *        SET 8 = LEVEL 8 (REINSURANCE COMPANY PRIME TOTALS)       ECS045
00933 *        SET 9 = LEVEL 9 (FINAL TOTALS)                           ECS045
00934 *                                                                 ECS045
00935 *    SET 'A' CONSISTS OF 9 CATAGORIES                             ECS045
00936 *                                                                 ECS045
00937 *        OCCURRENCE 1 = LIFE TOTAL FOR CURRENT MONTH              ECS045
00938 *        OCCURRENCE 2 = A&H TOTAL FOR CURRENT MONTH               ECS045
00939 *        OCCURRENCE 3 = COMBINED TOTAL FOR CURRENT MONTH          ECS045
00940 *        OCCURRENCE 4 = LIFE TOTAL FOR YEAR-TO-DATE               ECS045
00941 *        OCCURRENCE 5 = A&H TOTAL FOR YEAR-TO-DATE                ECS045
00942 *        OCCURRENCE 6 = COMBINED TOTAL FOR YEAR-TO-DATE           ECS045
00943 *        OCCURRENCE 7 = LIFE TOTAL FOR INCEPTION-TO-DATE          ECS045
00944 *        OCCURRENCE 8 = A&H TOTAL  FOR INCEPTION-TO-DATE          ECS045
00945 *        OCCURRENCE 9 = COMBINED TOTAL FOR INCEPTION-TO-DATE      ECS045
00946 *                                                                 ECS045
00947 *    (SET 'A' (9) DIFFERENT BREAKS * (9) CATAGORIES               ECS045
00948 *                                        = (81) OCCURRENCES       ECS045
00949 *                                                                 ECS045
00950  01  PRINT-ACCUMULATORS-A    COMP-3.                              ECS045
00951      12  PRINT-ACCUMS-A  OCCURS 81.                               ECS045
00952          16  PAA-GP      PIC S9(11)V99.                           ECS045
00953          16  PAA-PR      PIC S9(11)V99.                           ECS045
00954          16  PAA-NP      PIC S9(11)V99.                           ECS045
00955          16  PAA-BUP     PIC S9(11)V99.                           ECS045
00956          16  PAA-EUP     PIC S9(11)V99.                           ECS045
00957          16  PAA-RP      PIC S9(11)V99.                           ECS045
00958          16  PAA-RF      PIC S9(11)V99.                           ECS045
00959          16  PAA-CP      PIC S9(11)V99.                           ECS045
00960          16  PAA-AC      PIC S9(11)V99.                           ECS045
00961          16  PAA-OW      PIC S9(11)V99.                           ECS045
00962          16  PAA-ST      PIC S9(11)V99.                           ECS045
00963          16  PAA-CRSV    PIC S9(11)V99.                           ECS045
00964          16  PAA-AJ      PIC S9(11)V99.                           ECS045
00965          16  PAA-TD      PIC S9(11)V99.                           ECS045
00966          16  PAA-NB      PIC S9(11)V99.                           ECS045
00967          16  PAA-CRB     PIC S9(11)V99.                           ECS045
00968          16  PAA-CRE     PIC S9(11)V99.                           ECS045
00969          16  PAA-IC      PIC S9(11)V99.                           ECS045
00970          16  PAA-SBUP    PIC S9(11)V99.                           ECS045
00971          16  PAA-SEUP    PIC S9(11)V99.                           ECS045
00972          16  PAA-CPI     PIC S9(11)V99.                           ECS045
022912         16  PAA-TCOM    PIC S9(11)V99.
00973                                                                   ECS045
00974  01  PRINT-ACCUMS-AR REDEFINES PRINT-ACCUMULATORS-A COMP-3.       ECS045
00975      12  PRINT-ACCUMS-AR1            OCCURS 9.                    ECS045
00976          16  PRINT-ACCUMS-AR2        OCCURS 9.                    ECS045
022912             20  PRINT-ACCUMS-AR3    OCCURS 22.                   ECS045
00978                  24  PAA-AMT         PIC S9(11)V99.               ECS045
00979      EJECT                                                        ECS045
00980 *                                                                 ECS045
00981 * THE PRINT ACCUMULATORS ARE USED AS FOLLOWS                      ECS045
00982 *                                                                 ECS045
00983 *    THERE ARE 9 BREAKS FOR EACH CATAGORY                         ECS045
00984 *                                                                 ECS045
00985 *        SET 1 = LEVEL 1 (DATE RANGE TOTALS)                      ECS045
00986 *        SET 2 = LEVEL 2 (REINS COMPANY SUB)                      ECS045
00987 *        SET 3 = LEVEL 3 (ACCOUNT TOTALS)                         ECS045
00988 *        SET 4 = LEVEL 4 (STATE TOTALS)                           ECS045
00989 *        SET 5 = LEVEL 5 (CERT GROUP TOTALS)                      ECS045
00990 *        SET 6 = LEVEL 6 (CARRIER TOTALS)                         ECS045
00991 *        SET 7 = LEVEL 7 (REINS GROUP-A TOTALS)                   ECS045
00992 *        SET 8 = LEVEL 8 (REINSURANCE COMPANY PRIME TOTALS)       ECS045
00993 *        SET 9 = LEVEL 9 (FINAL TOTALS)                           ECS045
00994 *                                                                 ECS045
00995 *    SET 'B' CONSISTS OF 6 CATAGORIES                             ECS045
00996 *                                                                 ECS045
00997 *        OCCURRENCE 1 = BEGINNING FIGURES FOR CURRENT MONTH       ECS045
00998 *        OCCURRENCE 2 = ENDING    FIGURES FOR CURRENT MONTH       ECS045
00999 *        OCCURRENCE 3 = BEGINNING FIGURES FOR YEAR                ECS045
01000 *        OCCURRENCE 4 = ENDING    FIGURES FOR YEAR                ECS045
01001 *        OCCURRENCE 5 = BEGINNING FIGURES FOR INCEPTION           ECS045
01002 *        OCCURRENCE 6 = ENDING    FIGURES FOR INCEPTION           ECS045
01003 *                                                                 ECS045
01004 *    (SET 'B' (9) DIFFERENT BREAKS IN (6) CATAGORIES              ECS045
01005 *                                      = (54) OCCURRENCES         ECS045
01006                                                                   ECS045
01007  01  PRINT-ACCUMULATORS-B                COMP-3.                  ECS045
01008      12  PRINT-ACCUMS-B  OCCURS 54.                               ECS045
01009          16  PAB-LMR     PIC S9(11)V99.                           ECS045
01010          16  PAB-AUP1    PIC S9(11)V99.                           ECS045
01011          16  PAB-LA      PIC S9(11)V99.                           ECS045
01012          16  PAB-LU      PIC S9(11)V99.                           ECS045
01013          16  PAB-LT      PIC S9(11)V99.                           ECS045
01014          16  PAB-AA      PIC S9(11)V99.                           ECS045
01015          16  PAB-AF      PIC S9(11)V99.                           ECS045
01016          16  PAB-AU      PIC S9(11)V99.                           ECS045
01017          16  PAB-AT      PIC S9(11)V99.                           ECS045
01018          16  PAB-MB      PIC S9(11)V99.                           ECS045
01019          16  PAB-IF      PIC S9(11)V99.                           ECS045
01020          16  PAB-LUP     PIC S9(11)V99.                           ECS045
01021          16  PAB-AUP2    PIC S9(11)V99.                           ECS045
01022                                                                   ECS045
01023  01  PRINT-ACCUMS-BR REDEFINES PRINT-ACCUMULATORS-B COMP-3.       ECS045
01024      12  PRINT-ACCUMS-BR1            OCCURS 9.                    ECS045
01025          16  PRINT-ACCUMS-BR2        OCCURS 6.                    ECS045
01026              20  PRINT-ACCUMS-BR3    OCCURS 13.                   ECS045
01027                  24  PAB-AMT         PIC S9(11)V99.               ECS045
01028                                                                   ECS045
01029      EJECT                                                        ECS045
01030                                                                   ECS045
01031 ***************************************************************** ECS045
01032 *  EPEC-CONTROL IS CURRENT KEY ON INPUT PROCEDURE LOOP.  IT IS  * ECS045
01033 *  COMPARED WITH SAVE-EPEC-CONTROL TO SEE IF A BREAK HAS OCCURR-* ECS045
01034 *  ED AND A SORT RECORD SHOULD BE GENERATED.                    * ECS045
01035 ***************************************************************** ECS045
01036                                                                   ECS045
01037  01  MISC.                                                        ECS045
01038      12  EPEC-CONTROL.                                            ECS045
01039          16  E-C-CTL.                                             ECS045
01040              20  E-C-CNTRL-1.                                     ECS045
01041                  24  E-C-COMPANY.                                 ECS045
01042                      28  E-C-CARRIER  PIC X.                      ECS045
01043                      28  E-C-GROUPING PIC X(6).                   ECS045
01044                  24  E-C-STATE        PIC XX.                     ECS045
01045                  24  E-C-ACCOUNT      PIC X(10).                  ECS045
01046              20  E-C-CNTRL-2.                                     ECS045
01047                  24  E-C-DATES.                                   ECS045
01048                      28  E-C-EXP-DATE PIC 9(11)  COMP-3.             CL*18
01049                      28  E-C-EFF-DATE PIC 9(11)  COMP-3.             CL*18
01050          16  E-C-REI-COMP.                                        ECS045
01051              20  E-C-REI-COMP-PRIME  PIC X(3).                    ECS045
01052              20  E-C-REI-COMP-SUB    PIC X(3).                    ECS045
01053                                                                   ECS045
01054      12  SAVE-EPEC-CONTROL.                                       ECS045
01055          16  SAVE-E-C-CTL.                                        ECS045
01056              20  SAVE-E-C-CNTRL-1.                                ECS045
01057                  24  SAVE-E-C-COMPANY.                            ECS045
01058                      28  SAVE-E-C-CARRIER  PIC X.                 ECS045
01059                      28  SAVE-E-C-GROUPING PIC X(6).              ECS045
01060                  24  SAVE-E-C-STATE        PIC XX.                ECS045
01061                  24  SAVE-E-C-ACCOUNT      PIC X(10).             ECS045
01062              20  SAVE-E-C-CNTRL-2.                                ECS045
01063                  24  SAVE-E-C-DATES.                              ECS045
01064                      28  SAVE-E-C-EXP-DATE PIC 9(11)    COMP-3.      CL*11
01065                      28  SAVE-E-C-EFF-DATE PIC 9(11)    COMP-3.      CL*11
01066          16  SAVE-E-C-REI-COMP.                                   ECS045
01067              20  SAVE-E-C-REI-COMP-PRIME  PIC X(3).               ECS045
01068              20  SAVE-E-C-REI-COMP-SUB    PIC X(3).               ECS045
01069                                                                   ECS045
01070 ***************************************************************** ECS045
01071 *  RR-ACCTMSTR-CNTRL IS THE KEY USED TO READ THE ACCOUNT MASTER * ECS045
01072 *  RECORD ASSOCIATED WITH THE RR-REC TO BE RELEASED TO SORT.    * ECS045
01073 *  RR-REC IS UPDATED WITH VARIOUS FIELDS PROVIDED.              * ECS045
01074 ***************************************************************** ECS045
01075                                                                   ECS045
01076      12  RR-ACCTMSTR-CNTRL.                                       ECS045
01077          16  RR-ACCTMSTR-CARR        PIC X.                       ECS045
01078          16  RR-ACCTMSTR-GROUP       PIC X(6).                    ECS045
01079          16  RR-ACCTMSTR-STATE       PIC XX.                      ECS045
01080          16  RR-ACCTMSTR-ACCOUNT     PIC X(10).                   ECS045
01081          16  RR-ACCTMSTR-EXP-DT      PIC 9(11)    COMP-3.            CL*11
01082                                                                   ECS045
01083      EJECT                                                        ECS045
01084 ***************************************************************** ECS045
01085 *  RR-CONTROL-B IS CURRENT KEY ON OUTPUT PROCEDURE LOOP.  IT IS * ECS045
01086 *  COMPARED WITH SAVE-RR-CONTROL-B TO SEE IF A BREAK HAS OCCURR-* ECS045
01087 *  ED AND A REPORT PAGE SHOULD BE GENERATED. (45-A) REPORT      * ECS045
01088 ***************************************************************** ECS045
01089                                                                   ECS045
01090      12  RR-CONTROL-B.                                            ECS045
01091          16  R-B-REI-COMP.                                        ECS045
01092              20  R-B-REI-COMP-PRIME  PIC X(3).                    ECS045
01093              20  R-B-REI-COMP-SUB    PIC X(3).                    ECS045
01094          16  R-B-GROUP               PIC X(6).                    ECS045
01095          16  R-B-CTL.                                             ECS045
01096              20  R-B-CCSA.                                        ECS045
01097                  24  R-B-CARR        PIC X.                       ECS045
01098                  24  R-B-COMP        PIC X(6).                    ECS045
01099                  24  R-B-STATE       PIC XX.                      ECS045
01100                  24  R-B-ACCT        PIC X(10).                   ECS045
01101              20  R-B-EXP             PIC 9(11)    COMP-3.            CL*11
01102              20  R-B-EFF             PIC 9(11)    COMP-3.            CL*11
01103                                                                   ECS045
01104      12  SAVE-RR-CONTROL-B.                                       ECS045
01105          16  SAVE-R-B-REI-COMP.                                   ECS045
01106              20  SAVE-R-B-REI-COMP-PRIME PIC X(3).                ECS045
01107              20  SAVE-R-B-REI-COMP-SUB   PIC X(3).                ECS045
01108          16  SAVE-R-B-GROUP              PIC X(6).                ECS045
01109          16  SAVE-R-B-CTL.                                        ECS045
01110              20  SAVE-R-B-CCSA.                                   ECS045
01111                  24  SAVE-R-B-CARR       PIC X.                   ECS045
01112                  24  SAVE-R-B-COMP       PIC X(6).                ECS045
01113                  24  SAVE-R-B-STATE      PIC XX.                  ECS045
01114                  24  SAVE-R-B-ACCT       PIC X(10).               ECS045
01115              20  SAVE-R-B-EXP            PIC 9(11)    COMP-3.        CL*12
01116              20  SAVE-R-B-EFF            PIC 9(11)    COMP-3.        CL*12
01117                                                                   ECS045
01118      12  SAVE-REI-NAME                   PIC X(30).               ECS045
01119      12  SAVE-ACC-NAME                   PIC X(30).               ECS045
01120      12  SAVE-CEDE-NAME                  PIC X(30).               ECS045
01121      12  SAVE-LF-TAX                     PIC S9V9(4) COMP-3.      ECS045
01122      12  SAVE-AH-TAX                     PIC S9V9(4) COMP-3.      ECS045
01123      12  SAVE-LF-FEE                     PIC S9V9(4) COMP-3.      ECS045
01124      12  SAVE-AH-FEE                     PIC S9V9(4) COMP-3.      ECS045
01125      12  SAVE-PRT-ST                     PIC X.                   ECS045
01126      12  SAVE-PRT-OW                     PIC X.                   ECS045
01127      12  SAVE-PRT-CRSV                   PIC X.                   ECS045
01128      12  SAVE-CLAIM-CODE                 PIC X.                   ECS045
01129      12  SAVE-ZERO-LF-FEE                PIC X.                   ECS045
01130      12  SAVE-ZERO-AH-FEE                PIC X.                   ECS045
01131      12  SAVE-CARR                       PIC X.                   ECS045
020612     12  SAVE-LF-PE                      PIC X.
020612     12  SAVE-AH-PE                      PIC X.
01132                                                                   ECS045
01133      EJECT                                                        ECS045
01134 ***************************************************************** ECS045
01135 *  RR-CONTROL-B-B IS CURRENT KEY ON OUTPUT PROCEDURE LOOP. IT IS* ECS045
01136 *  COMPARED WITH SAVE-RR-CONTROL-B TO SEE IF A BREAK HAS OCCURR-* ECS045
01137 *  ED AND A REPORT PAGE SHOULD BE GENERATED. (45-B) REPORT      * ECS045
01138 ***************************************************************** ECS045
01139                                                                   ECS045
01140      12  RR-CONTROL-B-B.                                          ECS045
01141          16  R-B-REI-COMP-B.                                      ECS045
01142              20  R-B-REI-COMP-PRIME-B PIC X(3).                   ECS045
01143              20  R-B-REI-COMP-SUB-B   PIC X(3).                   ECS045
01144          16  R-B-GROUP-B              PIC X(6).                   ECS045
01145          16  R-B-CARR-B               PIC X.                      ECS045
01146          16  R-B-STATE-B              PIC XX.                     ECS045
01147                                                                   ECS045
01148      12  SAVE-RR-CONTROL-B-B.                                     ECS045
01149          16  SAVE-R-B-REI-COMP-B.                                 ECS045
01150              20  SAVE-R-B-REI-COMP-PRIME-B PIC X(3).              ECS045
01151              20  SAVE-R-B-REI-COMP-SUB-B   PIC X(3).              ECS045
01152          16  SAVE-R-B-GROUP-B              PIC X(6).              ECS045
01153          16  SAVE-R-B-CARR-B               PIC X.                 ECS045
01154          16  SAVE-R-B-STATE-B              PIC XX.                ECS045
01155                                                                   ECS045
01156      EJECT                                                        ECS045
01157 ***************************************************************** ECS045
01158 *  RR-CONTROL-B-D IS CURRENT KEY ON OUTPUT PROCEDURE LOOP. IT IS* ECS045
01159 *  COMPARED WITH SAVE-RR-CONTROL-D TO SEE IF A BREAK HAS OCCURR-* ECS045
01160 *  ED AND A REPORT PAGE SHOULD BE GENERATED. (45-D) REPORT      * ECS045
01161 ***************************************************************** ECS045
01162                                                                   ECS045
01163      12  RR-CONTROL-B-D.                                          ECS045
01164          16  R-B-GROUP-D              PIC X(6).                   ECS045
01165          16  R-B-CARR-D               PIC X.                      ECS045
01166          16  R-B-REI-COMP-D.                                      ECS045
01167              20  R-B-REI-COMP-PRIME-D PIC X(3).                   ECS045
01168              20  R-B-REI-COMP-SUB-D   PIC X(3).                   ECS045
01169                                                                   ECS045
01170      12  SAVE-RR-CONTROL-B-D.                                     ECS045
01171          16  SAVE-R-B-GROUP-D           PIC X(6).                 ECS045
01172          16  SAVE-R-B-CARR-D            PIC X.                    ECS045
01173          16  SAVE-R-B-REI-COMP-D.                                 ECS045
01174              20  SAVE-R-B-REI-COMP-PRIME-D PIC X(3).              ECS045
01175              20  SAVE-R-B-REI-COMP-SUB-D   PIC X(3).              ECS045
01176                                                                   ECS045
01177      EJECT                                                        ECS045
01178 ***************************************************************** ECS045
01179 *  RR-CONTROL-B-E IS CURRENT KEY ON OUTPUT PROCEDURE LOOP. IT IS* ECS045
01180 *  COMPARED WITH SAVE-RR-CONTROL-E TO SEE IF A BREAK HAS OCCURR-* ECS045
01181 *  ED AND A REPORT PAGE SHOULD BE GENERATED. (45-E) REPORT      * ECS045
01182 ***************************************************************** ECS045
01183                                                                   ECS045
01184      12  RR-CONTROL-B-E.                                          ECS045
01185          16  R-B-REI-COMP-E.                                      ECS045
01186              20  R-B-REI-COMP-PRIME-E PIC X(3).                   ECS045
01187              20  R-B-REI-COMP-SUB-E   PIC X(3).                   ECS045
01188                                                                   ECS045
01189      12  SAVE-RR-CONTROL-B-E.                                     ECS045
01190          16  SAVE-R-B-REI-COMP-E.                                 ECS045
01191              20  SAVE-R-B-REI-COMP-PRIME-E PIC X(3).              ECS045
01192              20  SAVE-R-B-REI-COMP-SUB-E   PIC X(3).              ECS045
01193                                                                   ECS045
01194      EJECT                                                        ECS045
01195  01  REIN-COMPANY-TABLE.                                          ECS045
01196      12  REIN-COMPANY-ENTRIES  OCCURS  1500  TIMES.               ECS045
01197          16  RCT-REIN-CO.                                         ECS045
01198              20  RCT-REIN-CO-PRIME PIC X(3).                      ECS045
01199              20  RCT-REIN-CO-SUB   PIC X(3).                      ECS045
01200          16  RCT-LF-ISS-CPRM       PIC S9(11)V99      COMP-3.     ECS045
01201          16  RCT-LF-CAN-CPRM       PIC S9(11)V99      COMP-3.     ECS045
01202          16  RCT-LF-ISS-GPRM       PIC S9(11)V99      COMP-3.     ECS045
01203          16  RCT-LF-CAN-GPRM       PIC S9(11)V99      COMP-3.     ECS045
01204          16  RCT-AH-ISS-CPRM       PIC S9(11)V99      COMP-3.     ECS045
01205          16  RCT-AH-CAN-CPRM       PIC S9(11)V99      COMP-3.     ECS045
01206          16  RCT-AH-ISS-GPRM       PIC S9(11)V99      COMP-3.     ECS045
01207          16  RCT-AH-CAN-GPRM       PIC S9(11)V99      COMP-3.     ECS045
01208          16  RCT-NAME              PIC X(30).                     ECS045
01209          16  RCT-LF-PE             PIC X.                         ECS045
01210          16  RCT-AH-PE             PIC X.                         ECS045
01211          16  RCT-LF-FEE            PIC S9V9999        COMP-3.     ECS045
01212          16  RCT-AH-FEE            PIC S9V9999        COMP-3.     ECS045
01213          16  RCT-PRT-ST            PIC X.                         ECS045
01214          16  RCT-PRT-OW            PIC X.                         ECS045
01215          16  RCT-PRT-CRSV          PIC X.                         ECS045
01216          16  RCT-MORT-CODE         PIC X(4).                      ECS045
01217          16  RCT-MORT-SW           PIC X.                         ECS045
01218          16  RCT-CLAIM-CODE        PIC X.                         ECS045
01219          16  RCT-ZERO-LF-FEE       PIC X.                         ECS045
01220          16  RCT-ZERO-AH-FEE       PIC X.                         ECS045
01221          16  RCT-CEDE-NAME         PIC X(30).                     ECS045
01222          16  RCT-LF-COMM           PIC X.                         ECS045
01223          16  RCT-AH-COMM           PIC X.                         ECS045
01224          16  RCT-LF-TAX            PIC X.                         ECS045
01225          16  RCT-AH-TAX            PIC X.                         ECS045
01226          16  RCT-LF-IBNR-PCT       PIC SV999          COMP-3.     ECS045
01227          16  RCT-AH-IBNR-PCT       PIC SV999          COMP-3.     ECS045
01228          16  RCT-LF-FEE-METHOD     PIC X.                         ECS045
01229              88  RCT-LF-FEE-BRACKETED         VALUE '1' '2'.      ECS045
01230              88  RCT-LF-FEE-METHOD-1          VALUE '1'.          ECS045
01231              88  RCT-LF-FEE-METHOD-2          VALUE '2'.          ECS045
01232              88  RCT-LF-FEE-PERCENT           VALUE ' ' 'P'.      ECS045
01233          16  RCT-LF-FEE-BASIS      PIC X.                         ECS045
01234              88  RCT-LF-GROSS-CEDED           VALUE '1'.          ECS045
01235              88  RCT-LF-NET-CEDED             VALUE '2'.          ECS045
01236              88  RCT-LF-GROSS-WRITTEN         VALUE '3'.          ECS045
01237              88  RCT-LF-NET-WRITTEN           VALUE '4'.          ECS045
01238              88  RCT-LF-COMBINE-GROSS-CEDED   VALUE '5'.          ECS045
01239              88  RCT-LF-COMBINE-NET-CEDED     VALUE '6'.          ECS045
01240              88  RCT-LF-COMBINE-GROSS-WRITTEN VALUE '7'.          ECS045
01241              88  RCT-LF-COMBINE-NET-WRITTEN   VALUE '8'.          ECS045
01242          16  RCT-AH-FEE-METHOD     PIC X.                         ECS045
01243              88  RCT-AH-FEE-BRACKETED         VALUE '1' '2'.      ECS045
01244              88  RCT-AH-FEE-METHOD-1          VALUE '1'.          ECS045
01245              88  RCT-AH-FEE-METHOD-2          VALUE '2'.          ECS045
01246              88  RCT-AH-FEE-PERCENT           VALUE ' ' 'P'.      ECS045
01247          16  RCT-AH-FEE-BASIS      PIC X.                         ECS045
01248              88  RCT-AH-GROSS-CEDED           VALUE '1'.          ECS045
01249              88  RCT-AH-NET-CEDED             VALUE '2'.          ECS045
01250              88  RCT-AH-GROSS-WRITTEN         VALUE '3'.          ECS045
01251              88  RCT-AH-NET-WRITTEN           VALUE '4'.          ECS045
01252              88  RCT-AH-COMBINE-GROSS-CEDED   VALUE '5'.          ECS045
01253              88  RCT-AH-COMBINE-NET-CEDED     VALUE '6'.          ECS045
01254              88  RCT-AH-COMBINE-GROSS-WRITTEN VALUE '7'.          ECS045
01255              88  RCT-AH-COMBINE-NET-WRITTEN   VALUE '8'.          ECS045
01256          16  RCT-FEE-RANGES  OCCURS  6  TIMES.                    ECS045
01257              20  RCT-LF-FEE-RANGE-PCT  PIC S9V9999   COMP-3.      ECS045
01258              20  RCT-LF-FEE-THRU-AMT   PIC S9(9)V99  COMP-3.      ECS045
01259              20  RCT-AH-FEE-RANGE-PCT  PIC S9V9999   COMP-3.      ECS045
01260              20  RCT-AH-FEE-THRU-AMT   PIC S9(9)V99  COMP-3.      ECS045
01261                                                                   ECS045
01262  01  RCT-ENTRY-COUNT           PIC S9(5)     COMP-3   VALUE +1500.ECS045
01263                                                                   ECS045
01264  01  REIN-GROUP-TABLE.                                            ECS045
01265      12  REIN-GROUP-ENTRIES  OCCURS  1500  TIMES.                 ECS045
01266          16  RCT-REIN-GROUP       PIC X(6).                       ECS045
01267          16  RCT-LF-PR-PCT         PIC S9V9999    COMP-3.         ECS045
01268          16  RCT-LF-78-PCT         PIC S9V9999    COMP-3.         ECS045
01269          16  RCT-AH-PR-PCT         PIC S9V9999    COMP-3.         ECS045
01270          16  RCT-AH-78-PCT         PIC S9V9999    COMP-3.         ECS045
032707         16  RCT-EXCISE-TAX        PIC S9V9999    COMP-3.         ECS045
01271          16  RCT-CEDING-STMT-OPT-A  PIC X.                        ECS045
01272              88  RCTRPT-A-WANTED  VALUES ARE ' ' 'Y'.             ECS045
01273          16  RCT-CEDING-STMT-OPT-B  PIC X.                        ECS045
01274              88  RCTRPT-B-WANTED  VALUES ARE ' ' 'Y'.             ECS045
01275          16  RCT-CEDING-STMT-OPT-C  PIC X.                        ECS045
01276              88  RCTRPT-C-WANTED  VALUES ARE ' ' 'Y'.             ECS045
01277          16  RCT-CEDING-STMT-OPT-D  PIC X.                        ECS045
01278              88  RCTRPT-D-WANTED  VALUES ARE ' ' 'Y'.             ECS045
01279          16  RCT-CEDING-STMT-OPT-E  PIC X.                        ECS045
01280              88  RCTRPT-E-WANTED  VALUES ARE ' ' 'Y'.             ECS045
061008
061008 01  DATA-OUT-RECORD.
061008     05  DO-REIN-BY-NUM       PIC X(6).
061008     05  DO-REIN-BY-NAME      PIC X(30).
061008     05  DO-CEDED-FROM-NAME   PIC X(30).
061008     05  DO-REIN-PRIME        PIC X(3).
061008     05  DO-REIN-GROUP        PIC X(6).
061008     05  DO-CARRIER           PIC X(1).
061008     05  DO-CERT-GROUP        PIC X(6).
061008     05  DO-STATE             PIC X(2).
061008     05  DO-ACCOUNT-NUM       PIC X(10).
061008     05  DO-ACCOUNT-NAME      PIC X(30).
061008     05  DO-ACCUMULATORS OCCURS 22 TIMES.
061008         10  DO-ID-NUM        PIC 99.
061008         10  DO-DESCR         PIC X(12).
061008         10  DO-CLF           PIC -(11).99.
061008         10  DO-CBG REDEFINES DO-CLF PIC -(11).99.
061008         10  DO-YLF           PIC -(11).99.
061008         10  DO-CEN REDEFINES DO-YLF PIC -(11).99.
061008         10  DO-ILF           PIC -(11).99.
061008         10  DO-YBG REDEFINES DO-ILF PIC -(11).99.
061008         10  DO-CAH           PIC -(11).99.
061008         10  DO-YEN REDEFINES DO-CAH PIC -(11).99.
061008         10  DO-YAH           PIC -(11).99.
061008         10  DO-IBG REDEFINES DO-YAH PIC -(11).99.
061008         10  DO-IAH           PIC -(11).99.
061008         10  DO-IEN REDEFINES DO-IAH PIC -(11).99.
061008     05  DO-TOTAL-TYPE        PIC X(1).
061008     05  DO-CLAIM-PI          PIC X(1).
061008     05  FILLER               PIC X(18).
061008
061008 01  DATA-OUT-SELECTIONS.
061008     05  DO-SUBSCRIPTS.
061008         10 DO-SUB-1        PIC X(3) VALUE '001'.
061008         10 DO-SUB-2        PIC X(3) VALUE '002'.
061008         10 DO-SUB-3        PIC X(3) VALUE '003'.
061008         10 DO-SUB-4        PIC X(3) VALUE '004'.
061008         10 DO-SUB-5        PIC X(3) VALUE '005'.
061008         10 DO-SUB-6        PIC X(3) VALUE '006'.
061008         10 DO-SUB-7        PIC X(3) VALUE '008'.
061008         10 DO-SUB-8        PIC X(3) VALUE '022'.
061008         10 DO-SUB-9        PIC X(3) VALUE '012'.
061008         10 DO-SUB-10       PIC X(3) VALUE '013'.
061008         10 DO-SUB-11       PIC X(3) VALUE '020'.
061008         10 DO-SUB-12       PIC X(3) VALUE '000'.
061008         10 DO-SUB-13       PIC X(3) VALUE '007'.
061008         10 DO-SUB-14       PIC X(3) VALUE '011'.
061008         10 DO-SUB-15       PIC X(3) VALUE '009'.
061008         10 DO-SUB-16       PIC X(3) VALUE '000'.
061008         10 DO-SUB-17       PIC X(3) VALUE '010'.
061008         10 DO-SUB-18       PIC X(3) VALUE '014'.
061008         10 DO-SUB-19       PIC X(3) VALUE '015'.
061008         10 DO-SUB-20       PIC X(3) VALUE '000'.
061008         10 DO-SUB-21       PIC X(3) VALUE '016'.
061008         10 DO-SUB-22       PIC X(3) VALUE '017'.
061008         10 DO-SUB-23       PIC X(3) VALUE '000'.
061008         10 DO-SUB-24       PIC X(3) VALUE '000'.
061008         10 DO-SUB-25       PIC X(3) VALUE '000'.
061008         10 DO-SUB-26       PIC X(3) VALUE '018'.
061008         10 DO-SUB-27       PIC X(3) VALUE '000'.
061008         10 DO-SUB-28       PIC X(3) VALUE '000'.
061008         10 DO-SUB-29       PIC X(3) VALUE '000'.
061008         10 DO-SUB-30       PIC X(3) VALUE '000'.
061008         10 DO-SUB-31       PIC X(3) VALUE '019'.
061008         10 DO-SUB-32       PIC X(3) VALUE '000'.
061008         10 DO-SUB-33       PIC X(3) VALUE '021'.
061008         10 DO-SUB-34       PIC X(3) VALUE '000'.
061008         10 DO-SUB-35       PIC X(3) VALUE '000'.
061008         10 DO-SUB-36       PIC X(3) VALUE '000'.
061008         10 DO-SUB-37       PIC X(3) VALUE '000'.
061008         10 DO-SUB-38       PIC X(3) VALUE '000'.
061008     05  DO-SELECT-SUBSCRIPT REDEFINES DO-SUBSCRIPTS 
061008             OCCURS 38 TIMES.
061008         10 DO-OUT-SUBSCRIPT        PIC 9(3).
061008
020612
020612 01  DATA-OUT-AHL-RECORD.
020612     05  DOA-ME-DATE.
020612         10  DOA-ME-MO         PIC 9(2).
020612         10  FILLER            PIC X(1)    VALUE '/'.
020612         10  DOA-ME-DD         PIC 9(2).
020612         10  FILLER            PIC X(1)    VALUE '/'.
020612         10  DOA-ME-YR         PIC 9(4).
020612     05  FILLER                PIC X(1)    VALUE ';'.
020612     05  DOA-REIN-BY-NUM       PIC X(6).
020612     05  FILLER                PIC X(1)    VALUE ';'.
020612     05  DOA-REIN-BY-NAME      PIC X(30).
020612     05  FILLER                PIC X(1)    VALUE ';'.
020612     05  DOA-CEDED-FROM-NAME   PIC X(30).
020612     05  FILLER                PIC X(1)    VALUE ';'.
020612     05  DOA-REIN-PRIME        PIC X(3).
020612     05  FILLER                PIC X(1)    VALUE ';'.
020612     05  DOA-REIN-GROUP        PIC X(6).
020612     05  FILLER                PIC X(1)    VALUE ';'.
020612     05  DOA-CARRIER           PIC X(1).
020612     05  FILLER                PIC X(1)    VALUE ';'.
020612     05  DOA-CERT-GROUP        PIC X(6).
020612     05  FILLER                PIC X(1)    VALUE ';'.
020612     05  DOA-STATE             PIC X(2).
020612     05  FILLER                PIC X(1)    VALUE ';'.
020612     05  DOA-ACCOUNT-NUM       PIC X(10).
020612     05  FILLER                PIC X(1)    VALUE ';'.
020612     05  DOA-ACCOUNT-NAME      PIC X(30).
020612     05  FILLER                PIC X(1)    VALUE ';'.
020612     05  DOA-REIN-SUB          PIC X(3).
020612     05  FILLER                PIC X(1)    VALUE ';'.
020612     05  DOA-ACCUMULATORS OCCURS 19 TIMES.
020612         10  DOA-ID-NUM        PIC 99.
020612         10  FILLER            PIC X(1)    VALUE ';'.
020612         10  DOA-DESCR         PIC X(12).
020612         10  FILLER            PIC X(1)    VALUE ';'.
020612         10  DOA-CLF           PIC -(11).99.
020612         10  FILLER            PIC X(1)    VALUE ';'.
020612         10  DOA-CAH           PIC -(11).99.
020612         10  FILLER            PIC X(1)    VALUE ';'.
020612         10  DOA-CCMB          PIC -(11).99.
020612         10  FILLER            PIC X(1)    VALUE ';'.
020612         10  DOA-YLF           PIC -(11).99.
020612         10  FILLER            PIC X(1)    VALUE ';'.
020612         10  DOA-YAH           PIC -(11).99.
020612         10  FILLER            PIC X(1)    VALUE ';'.
020612         10  DOA-YCMB          PIC -(11).99.
020612         10  FILLER            PIC X(1)    VALUE ';'.
020612         10  DOA-ILF           PIC -(11).99.
020612         10  FILLER            PIC X(1)    VALUE ';'.
020612         10  DOA-IAH           PIC -(11).99.
020612         10  FILLER            PIC X(1)    VALUE ';'.
020612         10  DOA-ICMB          PIC -(11).99.
020612         10  FILLER            PIC X(1)    VALUE ';'.
020612     05  DOB-ACCUMULATORS OCCURS 13 TIMES.
020612         10  DOB-ID-NUM        PIC 99.
020612         10  FILLER            PIC X(1)    VALUE ';'.
020612         10  DOB-DESCR         PIC X(12).
020612         10  FILLER            PIC X(1)    VALUE ';'.
020612         10  DOB-CBG           PIC -(11).99.
020612         10  FILLER            PIC X(1)    VALUE ';'.
020612         10  DOB-CEN           PIC -(11).99.
020612         10  FILLER            PIC X(1)    VALUE ';'.
020612         10  DOB-YBG           PIC -(11).99.
020612         10  FILLER            PIC X(1)    VALUE ';'.
020612         10  DOB-YEN           PIC -(11).99.
020612         10  FILLER            PIC X(1)    VALUE ';'.
020612         10  DOB-IBG           PIC -(11).99.
020612         10  FILLER            PIC X(1)    VALUE ';'.
020612         10  DOB-IEN           PIC -(11).99.
020612         10  FILLER            PIC X(1)    VALUE ';'.
020612     05  DOA-TOTAL-TYPE        PIC X(1).
020612     05  FILLER                PIC X(1)    VALUE ';'.
020612     05  DOA-CLAIM-PI          PIC X(1).
020612     05  FILLER                PIC X(1)    VALUE ';'.
020612     05  DOA-LF-PE             PIC X(1).
020612     05  FILLER                PIC X(1)    VALUE ';'.
020612     05  DOA-AH-PE             PIC X(1).
020612     05  FILLER                PIC X(1)    VALUE ';'.
020612     05  FILLER                PIC X(1)    VALUE 'E'.
020612     05  FILLER                PIC X(95).
020612
020612 01  DATA-OUT-AHL-SELECTIONS.
020612     05  DOA-SUBSCRIPTS.
020612         10 DOA-SUB-1        PIC X(3) VALUE '001'.
020612         10 DOA-SUB-2        PIC X(3) VALUE '002'.
020612         10 DOA-SUB-3        PIC X(3) VALUE '003'.
020612         10 DOA-SUB-4        PIC X(3) VALUE '004'.
020612         10 DOA-SUB-5        PIC X(3) VALUE '005'.
020612         10 DOA-SUB-6        PIC X(3) VALUE '006'.
020612         10 DOA-SUB-7        PIC X(3) VALUE '007'.
020612         10 DOA-SUB-8        PIC X(3) VALUE '008'.
020612         10 DOA-SUB-9        PIC X(3) VALUE '009'.
020612         10 DOA-SUB-10       PIC X(3) VALUE '010'.
020612         10 DOA-SUB-11       PIC X(3) VALUE '011'.
020612         10 DOA-SUB-12       PIC X(3) VALUE '012'.
020612         10 DOA-SUB-13       PIC X(3) VALUE '013'.
020612         10 DOA-SUB-14       PIC X(3) VALUE '014'.
020612         10 DOA-SUB-15       PIC X(3) VALUE '015'.
020612         10 DOA-SUB-16       PIC X(3) VALUE '000'.
020612         10 DOA-SUB-17       PIC X(3) VALUE '016'.
020612         10 DOA-SUB-18       PIC X(3) VALUE '017'.
020612         10 DOA-SUB-19       PIC X(3) VALUE '018'.
020612         10 DOA-SUB-20       PIC X(3) VALUE '000'.
020612         10 DOA-SUB-21       PIC X(3) VALUE '001'.
020612         10 DOA-SUB-22       PIC X(3) VALUE '002'.
020612         10 DOA-SUB-23       PIC X(3) VALUE '000'.
020612         10 DOA-SUB-24       PIC X(3) VALUE '003'.
020612         10 DOA-SUB-25       PIC X(3) VALUE '004'.
020612         10 DOA-SUB-26       PIC X(3) VALUE '005'.
020612         10 DOA-SUB-27       PIC X(3) VALUE '000'.
020612         10 DOA-SUB-28       PIC X(3) VALUE '006'.
020612         10 DOA-SUB-29       PIC X(3) VALUE '007'.
020612         10 DOA-SUB-30       PIC X(3) VALUE '008'.
020612         10 DOA-SUB-31       PIC X(3) VALUE '009'.
020612         10 DOA-SUB-32       PIC X(3) VALUE '000'.
020612         10 DOA-SUB-33       PIC X(3) VALUE '010'.
020612         10 DOA-SUB-34       PIC X(3) VALUE '000'.
020612         10 DOA-SUB-35       PIC X(3) VALUE '011'.
020612         10 DOA-SUB-36       PIC X(3) VALUE '000'.
020612         10 DOA-SUB-37       PIC X(3) VALUE '012'.
020612         10 DOA-SUB-38       PIC X(3) VALUE '013'.
020612     05  DOA-SELECT-SUBSCRIPT REDEFINES DOA-SUBSCRIPTS 
020612             OCCURS 38 TIMES.
020612         10 DOA-OUT-SUBSCRIPT        PIC 9(3).
020612
01281                                                                   ECS045
01282                              COPY ELCDATE.                           CL*21
01283                                                                   ECS045
01284                              COPY ELCDTECX.                       ECS045
01285                                                                   ECS045
01286                              COPY ELCDTEVR.                       ECS045
01287                                                                   ECS045
01288                              COPY ELCEPCVR.                       ECS045
01289  EJECT                                                            ECS045
01290  PROCEDURE DIVISION.                                              ECS045
01291                                                                   ECS045
01292                                                                   ECS045
01293  0000-READ-DATE-CARD.                                             ECS045
01294                              COPY ELCDTERX.                       ECS045
01295 ***************************************************************** ECS045
01296 * PROCESS OPTION MEANINGS:                                      * ECS045
01297 *    1 = CURRENT MONTH   AND YEAR-TO-DATE                       * ECS045
01298 *    2 = CURRENT MONTH   AND INCEPTION-TO-DATE                  * ECS045
01299 *    3 = YEAR-TO-DATE    AND INCEPTION-TO-DATE                  * ECS045
01300 *    4 = QUARTER-TO-DATE AND YEAR-TO-DATE                       * ECS045
01301 *    5 = QUARTER-TO-DATE AND INCEPTION-TO-DATE                  * ECS045
01302 *    6 = CURRENT MONTH   AND QUARTER-TO-DATE                    * ECS045
01303 *    7 = CURRENT MONTH   AND YEAR-TO-DATE (NO ACCT DETAIL)      * ECS045
01304 *    8 = CURRENT MONTH   AND INCEPTION-TO-DATE (NO ACCT DETAIL) * ECS045
01305 ***************************************************************** ECS045
01306                                                                   ECS045
01307  0100-INITIALIZE-DATA.                                            ECS045
061008     PERFORM 3300-INIT-DATA-OUT THRU 3399-EXIT.
020612     PERFORM 3400-INIT-DATA-OUT-AHL THRU 3499-EXIT.
01308                                                                   ECS045
01309      IF DTE-FMT-OPT EQUAL '2'                                     ECS045
01310          MOVE LIFE-OVERRIDE-L6  TO LIFE-PLUG-12                   ECS045
01311                                    LIFE-PLUG-22
041108                                   LIFE-PLUG-32
01312          MOVE AH-OVERRIDE-L6    TO AH-PLUG-12                     ECS045
01313                                    AH-PLUG-22
041108                                   AH-PLUG-32
01314      ELSE                                                         ECS045
01315          MOVE LIFE-OVERRIDE-L6  TO LIFE-PLUG-1                    ECS045
01316                                    LIFE-PLUG-2                    ECS045
01317          MOVE AH-OVERRIDE-L6    TO AH-PLUG-1                      ECS045
01318                                    AH-PLUG-2.                     ECS045
01319                                                                   ECS045
01320      MOVE WS-CURRENT-DATE        TO H2-RUN-DT.                    ECS045
01321      MOVE ALPH-DATE              TO H3-DATE.                      ECS045
01322      MOVE COMPANY-NAME           TO H2-COMP.                      ECS045
01323                                                                   ECS045
01324 **************************************                            ECS045
01325 * EXAMPLE: DATE CARD LOAD=CLAS033198 *                               CL*17
01326 ******************************************************            ECS045
01327 * BUILD RUN-DT FROM RUN-DATE(DATE PROVIDED BY EL300) *            ECS045
01328 ******************************************************            ECS045
01329                                                                   ECS045
01330      MOVE RUN-DATE               TO RUN-DT                        ECS045
020612     MOVE RD-MO                  TO DOA-ME-MO.
020612     MOVE RD-DA                  TO DOA-ME-DD.
020612     MOVE RD-CCYY                TO DOA-ME-YR.
01331      MOVE ZERO                   TO RD-DA.                        ECS045
01332 ********************                                                 CL*17
01333 * RUN-DT 19980300  *                                                 CL*17
01334 ********************                                                 CL*17
01335                                                                   ECS045
01336      MOVE RUN-DT                 TO BEG-DATE                      ECS045
01337                                     PRV-MONTH                     ECS045
01338                                     PRV-QTR.                      ECS045
01339                                                                   ECS045
01340      MOVE 12                     TO BD-MO.                           CL*17
01341      SUBTRACT 1 FROM BD-CCYY.                                        CL*17
01342                                                                      CL*17
01343      IF BEG-DATE LESS DTE-CONV-DT                                    CL*19
01344          MOVE DTE-CONV-DT        TO BEG-DATE                         CL*19
01345          MOVE ZERO               TO BD-DA.                           CL*19
01346                                                                      CL**3
01347 **********************                                               CL*17
01348 * BEG-DATE 19971200  *                                               CL*17
01349 **********************                                               CL*17
01350                                                                      CL*17
01351      SUBTRACT 1 FROM PM-MO.                                          CL*14
01352                                                                   ECS045
01353      IF PM-MO = ZERO                                              ECS045
01354          MOVE 12                 TO PM-MO                         ECS045
01355          SUBTRACT 1 FROM PM-CCYY.                                    CL*14
01356 ***********************                                              CL*17
01357 * PRV-MONTH 19980200  *                                              CL*17
01358 ***********************                                              CL*17
01359                                                                      CL*17
01360      IF PQ-MO LESS 04                                             ECS045
01361          MOVE 12 TO PQ-MO                                         ECS045
01362          SUBTRACT 1 FROM PQ-CCYY                                     CL*14
01363      ELSE                                                         ECS045
01364          IF PQ-MO LESS 07                                         ECS045
01365              MOVE 03 TO PQ-MO                                     ECS045
01366          ELSE                                                     ECS045
01367              IF PQ-MO LESS 10                                     ECS045
01368                  MOVE 06 TO PQ-MO                                 ECS045
01369              ELSE                                                 ECS045
01370                  MOVE 09 TO PQ-MO.                                ECS045
01371 **********************                                               CL*17
01372 * PRV-QTR  19971200  *                                               CL*17
01373 **********************                                               CL*17
01374                                                                   ECS045
01375 ************************************************                  ECS045
01376 * DTE-QTR-CO = 1 (COMPANY PROCESSED QUARTERLY) *                  ECS045
01377 ************************************************                  ECS045
01378                                                                   ECS045
01379      IF DTE-QTR-CO = '1'                                          ECS045
01380          IF DTE-PGM-OPT = '3' OR '4' OR '5'                       ECS045
01381              NEXT SENTENCE                                        ECS045
01382          ELSE                                                     ECS045
01383              IF DTE-PGM-OPT = '1' OR '6' OR '7'                   ECS045
01384                  MOVE '4'        TO DTE-PGM-OPT                   ECS045
01385              ELSE                                                 ECS045
01386                  MOVE '5'        TO DTE-PGM-OPT.                  ECS045
01387                                                                   ECS045
01388      IF DTE-PGM-OPT = '1' OR '7' OR '2' OR '8' OR '3'             ECS045
01389          MOVE CM-HEADING-2       TO HDR-6A-2                      ECS045
01390          MOVE YTD-HEADING        TO HDR-6B-2                      ECS045
01391          MOVE ITD-HEADING-2      TO HDR-6C-2.                     ECS045
01392                                                                   ECS045
01393      IF DTE-PGM-OPT = '1' OR '7'                                  ECS045
01394          MOVE CM-HEADING         TO HDR-6A                        ECS045
01395          MOVE YTD-HEADING        TO HDR-6B.                       ECS045
01396                                                                   ECS045
01397      IF DTE-PGM-OPT = '2' OR '8'                                  ECS045
01398          MOVE CM-HEADING         TO HDR-6A                        ECS045
01399          MOVE ITD-HEADING        TO HDR-6B.                       ECS045
01400                                                                   ECS045
01401      IF DTE-PGM-OPT = '3'                                         ECS045
01402          MOVE YTD-HEADING        TO HDR-6A                        ECS045
01403          MOVE ITD-HEADING        TO HDR-6B.                       ECS045
01404                                                                   ECS045
01405      IF DTE-PGM-OPT = '4'                                         ECS045
01406          MOVE QTD-HEADING        TO HDR-6A                        ECS045
01407          MOVE YTD-HEADING        TO HDR-6B.                       ECS045
01408                                                                   ECS045
01409      IF DTE-PGM-OPT = '5'                                         ECS045
01410          MOVE QTD-HEADING        TO HDR-6A                        ECS045
01411          MOVE ITD-HEADING        TO HDR-6B.                       ECS045
01412                                                                   ECS045
01413      IF DTE-PGM-OPT = '4' OR '5'                                  ECS045
01414          MOVE PRV-QTR            TO PRV-MONTH                     ECS045
01415          MOVE YTD-HEADING        TO HDR-6A-2                      ECS045
01416          MOVE QTD-HEADING-2      TO HDR-6B-2                      ECS045
01417          MOVE ITD-HEADING-2      TO HDR-6C-2.                     ECS045
01418                                                                   ECS045
01419      IF DTE-PGM-OPT = '6'                                         ECS045
01420          MOVE PRV-QTR            TO BEG-DATE                      ECS045
01421          MOVE CM-HEADING-2       TO HDR-6A-2                      ECS045
01422          MOVE CM-HEADING         TO HDR-6A                        ECS045
01423          MOVE QTD-HEADING-2      TO HDR-6B-2                      ECS045
01424          MOVE QTD-HEADING        TO HDR-6B                        ECS045
01425          MOVE ITD-HEADING-2      TO HDR-6C-2.                     ECS045
01426                                                                   ECS045
01427                                                                   ECS045
01428      IF DTE-QTR-CO = '1'  AND                                     ECS045
01429         DTE-PGM-OPT NOT = '3'                                     ECS045
01430          MOVE CQ-HEADING         TO HDR-6A                        ECS045
01431          MOVE CQ-HEADING         TO HDR-6A-2.                     ECS045
01432                                                                   ECS045
060903     IF DTE-CLIENT = 'DCC'
060903        MOVE 'LIFE UNEARNED PREMIUM '
060903                                 TO DT-ENTRY (20)
060903     END-IF
060903     .
01433      EJECT                                                        ECS045
01434  0110-SORT-CONTROL-STATEMENTS.                                    ECS045
01435                                                                   ECS045
01436      OPEN INPUT EPEC-FILE                                         ECS045
01437                 ACC-MSTR                                          ECS045
01438                 REIN-TBL-FILE                                     ECS045
01439          OUTPUT REIN-WORK                                         ECS045
01440                 PRNTR-2                                           ECS045
01441                 PRNTR.                                            ECS045

061008     IF DTE-FMT-OPT = '2'
061008        OPEN OUTPUT DATA-OUT
020612        OPEN OUTPUT DATA-OUT-AHL
061008     END-IF

01442                                                                   ECS045
01443      IF REIN-FILE-STATUS  = '00' OR '97'                          ECS045
01444          NEXT SENTENCE                                            ECS045
01445        ELSE                                                       ECS045
01446          MOVE '**** BAD OPEN ON REIN FILE ****'                   ECS045
01447                                TO WS-ABEND-MESSAGE                ECS045
01448          MOVE '2'              TO WAC-1                           ECS045
01449          MOVE '1'              TO WAC-2                           ECS045
01450          MOVE REIN-FILE-STATUS TO WAC-3-4                         ECS045
01451                                   WS-ABEND-FILE-STATUS            ECS045
01452          MOVE WS-ABEND-CODE    TO WS-RETURN-CODE                  ECS045
01453          GO TO ABEND-PGM.                                         ECS045
01454                                                                   ECS045
01455      IF ERACCTT-FILE-STATUS  = '00' OR '97'                       ECS045
01456          NEXT SENTENCE                                            ECS045
01457        ELSE                                                       ECS045
01458          MOVE '**** BAD OPEN ON ACCTMSTR FILE ****'               ECS045
01459                                   TO WS-ABEND-MESSAGE             ECS045
01460          MOVE '1'                 TO WAC-1                        ECS045
01461                                      WAC-2                        ECS045
01462          MOVE ERACCTT-FILE-STATUS TO WAC-3-4                      ECS045
01463                                      WS-ABEND-FILE-STATUS         ECS045
01464          MOVE WS-ABEND-CODE       TO WS-RETURN-CODE               ECS045
01465          GO TO ABEND-PGM.                                         ECS045
01466                                                                   ECS045
01467  0120-SORT-ONE-DEFINITION.                                        ECS045
01468                                                                   ECS045
01469      SORT SORT-WORK ON ASCENDING SW-REPORT-ID                     ECS045
01470                                  SW-REPORT-A-CNTL                 ECS045
01471          INPUT PROCEDURE                                          ECS045
01472              0130-SELECT-EPEC-RECS THRU 0380-SER-EXIT             ECS045
01473          OUTPUT PROCEDURE                                         ECS045
01474              0390-PRINT-STATEMENT  THRU 0990-PS-EXIT.             ECS045
01475                                                                   ECS045
01476      IF SORT-RETURN NOT = ZERO                                    ECS045
01477          MOVE '**** UNSUCCESSFUL SORT #1 BAD RETURN CODE ****'    ECS045
01478                              TO WS-ABEND-MESSAGE                  ECS045
01479          MOVE SORT-RETURN    TO WS-ABEND-CODE                     ECS045
01480          MOVE '0'            TO WAC-1                             ECS045
01481          MOVE '1'            TO WAC-2                             ECS045
01482          MOVE WS-ABEND-CODE  TO WS-RETURN-CODE                    ECS045
01483          GO TO ABEND-PGM.                                         ECS045
01484                                                                   ECS045
01485      EJECT                                                        ECS045
01486  0125-SORT-TWO-DEFINITION.                                        ECS045
01487      IF RW-WRITE-CNT = ZERO                                       ECS045
01488          GO TO 9999-END-OF-JOB.                                   ECS045
01489                                                                   ECS045
01490      SORT SORT-WORK ON ASCENDING SW-REPORT-C-CNTL                 ECS045
01491          INPUT PROCEDURE                                          ECS045
01492              1000-SELECT-RECAP THRU 1999-EXIT                     ECS045
01493          OUTPUT PROCEDURE                                         ECS045
01494              2000-PRINT-RECAP  THRU 2999-EXIT.                    ECS045
01495                                                                   ECS045
01496      IF SORT-RETURN NOT = ZERO                                    ECS045
01497          MOVE '**** UNSUCCESSFUL SORT #2 BAD RETURN CODE ****'    ECS045
01498                                  TO WS-ABEND-MESSAGE              ECS045
01499          MOVE SORT-RETURN        TO WS-ABEND-CODE                 ECS045
01500          MOVE '1'                TO WAC-1                         ECS045
01501                                     WAC-2                         ECS045
01502          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS045
01503          GO TO ABEND-PGM.                                         ECS045
01504                                                                   ECS045
01505      GO TO 9999-END-OF-JOB.                                       ECS045
01506                                                                   ECS045
01507  EJECT                                                            ECS045
01508  0130-SELECT-EPEC-RECS SECTION.                                   ECS045
01509 *************************************************************     ECS045
01510 *  READ REINSURANCE FILE AND TABLE ALL REINSURANCE COMPANY  *     ECS045
01511 *  RECORDS (B-TYPE) FOR REPORTING INFORMATION AND DECISIONS *     ECS045
01512 *************************************************************     ECS045
01513                                                                   ECS045
01514      MOVE +1                     TO RX.                           ECS045
01515      MOVE LOW-VALUES             TO RE-CONTROL-PRIMARY.           ECS045
01516      START REIN-TBL-FILE                                          ECS045
01517           KEY IS GREATER THAN RE-CONTROL-PRIMARY                  ECS045
01518           INVALID KEY MOVE HIGH-VALUES TO REIN-COMPANY-ENTRIES (1)ECS045
01519                       GO TO 0138-BUILD-DEFAULT-ENTRY.             ECS045
01520                                                                   ECS045
01521  0135-REIN-CO-TABLE-BUILD.                                        ECS045
01522      READ REIN-TBL-FILE  NEXT RECORD.                             ECS045
01523                                                                   ECS045
01524      IF REIN-FILE-STATUS  = '10'                                  ECS045
01525          MOVE HIGH-VALUES        TO REIN-COMPANY-ENTRIES (RX)     ECS045
01526          GO TO 0138-BUILD-DEFAULT-ENTRY.                          ECS045
01527                                                                   ECS045
01528      IF REIN-FILE-STATUS NOT = '00'                               ECS045
01529          MOVE '**** BAD READ ON REIN FILE ****'                   ECS045
01530                                  TO WS-ABEND-MESSAGE              ECS045
01531          MOVE '2'                TO WAC-1                         ECS045
01532          MOVE '4'                TO WAC-2                         ECS045
01533          MOVE REIN-FILE-STATUS   TO WAC-3-4                       ECS045
01534                                     WS-ABEND-FILE-STATUS          ECS045
01535          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS045
01536          GO TO ABEND-PGM.                                         ECS045
01537                                                                   ECS045
01538      IF RE-CODE NOT = 'B'                                         ECS045
01539          GO TO 0135-REIN-CO-TABLE-BUILD.                          ECS045
01540                                                                   ECS045
01541      MOVE RE-COMPANY             TO RCT-REIN-CO (RX).             ECS045
01542      MOVE ZEROS TO RCT-LF-ISS-CPRM (RX)  RCT-LF-CAN-CPRM (RX)     ECS045
01543                    RCT-LF-ISS-GPRM (RX)  RCT-LF-CAN-GPRM (RX)     ECS045
01544                    RCT-AH-ISS-CPRM (RX)  RCT-AH-CAN-CPRM (RX)     ECS045
01545                    RCT-AH-ISS-GPRM (RX)  RCT-AH-CAN-GPRM (RX).    ECS045
01546                                                                   ECS045
01547      MOVE RE-NAME                TO RCT-NAME   (RX).              ECS045
01548      MOVE RE-REINS-GROUPING-CODE TO RCT-REIN-GROUP (RX).          ECS045
01549      MOVE RE-LF-PE               TO RCT-LF-PE  (RX).              ECS045
01550      MOVE RE-AH-PE               TO RCT-AH-PE  (RX).              ECS045
01551      MOVE RE-LF-FEE              TO RCT-LF-FEE (RX).              ECS045
01552      MOVE RE-AH-FEE              TO RCT-AH-FEE (RX).              ECS045
01553                                                                   ECS045
032612     if dte-client = 'AHL'
032612        move zeros               to re-lf-78-pct
032612                                    re-lf-pr-pct
032612     end-if
01554      IF RE-LF-PR-PCT NOT NUMERIC                                  ECS045
01555          MOVE ZEROS              TO RE-LF-PR-PCT.                 ECS045
01556                                                                   ECS045
01557      MOVE RE-LF-PR-PCT           TO RCT-LF-PR-PCT (RX).           ECS045
01558      MOVE RE-AH-PR-PCT           TO RCT-AH-PR-PCT (RX).           ECS045
01559                                                                   ECS045
01560      IF RE-LF-78-PCT NOT NUMERIC                                  ECS045
01561          MOVE ZEROS              TO RE-LF-78-PCT.                 ECS045
01562                                                                   ECS045
01563      MOVE RE-LF-78-PCT           TO RCT-LF-78-PCT (RX).           ECS045
01564      MOVE RE-AH-78-PCT           TO RCT-AH-78-PCT (RX).           ECS045
01565      MOVE RE-PRT-ST              TO RCT-PRT-ST    (RX).           ECS045
01566      MOVE RE-PRT-OW              TO RCT-PRT-OW    (RX)
032707     IF RE-EXCISE-TAX NOT NUMERIC
032707        MOVE ZEROS               TO RE-EXCISE-TAX
032707     END-IF
032707     MOVE RE-EXCISE-TAX          TO RCT-EXCISE-TAX (RX)
01567                                                                   ECS045
01568      IF DTE-CLIENT = 'NCL'                                        ECS045
01569        IF RE-PRT-CRSV = 'Y' OR 'A' OR 'F'                         ECS045
01570            MOVE RE-PRT-CRSV      TO RCT-PRT-CRSV  (RX)            ECS045
01571        ELSE                                                       ECS045
01572            MOVE 'N'              TO RCT-PRT-CRSV  (RX).           ECS045
01573                                                                   ECS045
01574      MOVE RE-MORT-CODE           TO RCT-MORT-CODE (RX).           ECS045
01575      MOVE RE-MORT-SW             TO RCT-MORT-SW   (RX).           ECS045
01576      MOVE RE-CLAIM-CODE          TO RCT-CLAIM-CODE(RX).           ECS045
01577      MOVE RE-ZERO-LF-FEE         TO RCT-ZERO-LF-FEE (RX).         ECS045
01578      MOVE RE-ZERO-AH-FEE         TO RCT-ZERO-AH-FEE (RX).         ECS045
01579      MOVE RE-CEDE-NAME           TO RCT-CEDE-NAME   (RX).         ECS045
01580      MOVE RE-LF-COMM             TO RCT-LF-COMM     (RX).         ECS045
01581      MOVE RE-AH-COMM             TO RCT-AH-COMM     (RX).         ECS045
01582      MOVE RE-LF-TAX              TO RCT-LF-TAX      (RX).         ECS045
01583      MOVE RE-AH-TAX              TO RCT-AH-TAX      (RX).         ECS045
01584      MOVE RE-LF-IBNR-PCT         TO RCT-LF-IBNR-PCT (RX).         ECS045
01585      MOVE RE-AH-IBNR-PCT         TO RCT-AH-IBNR-PCT (RX).         ECS045
01586                                                                   ECS045
01587      MOVE RE-CEDING-STMT-OPT-A   TO RCT-CEDING-STMT-OPT-A  (RX).  ECS045
01588      MOVE RE-CEDING-STMT-OPT-B   TO RCT-CEDING-STMT-OPT-B  (RX).  ECS045
01589      MOVE RE-CEDING-STMT-OPT-C   TO RCT-CEDING-STMT-OPT-C  (RX).  ECS045
01590      MOVE RE-CEDING-STMT-OPT-D   TO RCT-CEDING-STMT-OPT-D  (RX).  ECS045
01591      MOVE RE-CEDING-STMT-OPT-E   TO RCT-CEDING-STMT-OPT-E  (RX).  ECS045
01592                                                                   ECS045
01593      IF DTE-CLIENT = 'NCL'                                        ECS045
01594          MOVE 'Y'                TO RCT-CEDING-STMT-OPT-E  (RX).  ECS045
01595                                                                   ECS045
01596      MOVE RE-LF-FEE-METHOD       TO RCT-LF-FEE-METHOD (RX).       ECS045
01597      MOVE RE-LF-FEE-BASIS        TO RCT-LF-FEE-BASIS  (RX).       ECS045
01598      MOVE RE-AH-FEE-METHOD       TO RCT-AH-FEE-METHOD (RX).       ECS045
01599      MOVE RE-AH-FEE-BASIS        TO RCT-AH-FEE-BASIS  (RX)
032707     MOVE RE-EXCISE-TAX          TO RCT-EXCISE-TAX    (RX)
01600                                                                   ECS045
01601      MOVE +1                     TO FR.                           ECS045
01602                                                                   ECS045
01603  0137-FEE-RANGE-LOOP.                                             ECS045
01604                                                                   ECS045
01605      IF RE-LF-FEE-BASIS = '5'  OR  '6'  OR  '7'  OR  '8'          ECS045
01606          MOVE RE-LF-FEE-RANGES (FR)                               ECS045
01607                                  TO RE-AH-FEE-RANGES (FR).        ECS045
01608                                                                   ECS045
01609      IF RE-LF-FEE-RANGE-PCT (FR) NOT NUMERIC                      ECS045
01610          MOVE ZEROS              TO RE-LF-FEE-RANGE-PCT (FR).     ECS045
01611      IF RE-LF-FEE-THRU-AMT  (FR) NOT NUMERIC                      ECS045
01612          MOVE ZEROS              TO RE-LF-FEE-THRU-AMT  (FR).     ECS045
01613      IF RE-AH-FEE-RANGE-PCT (FR) NOT NUMERIC                      ECS045
01614          MOVE ZEROS              TO RE-AH-FEE-RANGE-PCT (FR).     ECS045
01615      IF RE-AH-FEE-THRU-AMT  (FR) NOT NUMERIC                      ECS045
01616          MOVE ZEROS              TO RE-AH-FEE-THRU-AMT  (FR).     ECS045
01617                                                                   ECS045
01618      MOVE RE-LF-FEE-RANGE-PCT (FR) TO                             ECS045
01619           RCT-LF-FEE-RANGE-PCT (RX FR).                           ECS045
01620                                                                   ECS045
01621      IF RE-LF-FEE-THRU-AMT (FR) = +9999999.99                     ECS045
01622          MOVE +999999999.99            TO                         ECS045
01623               RCT-LF-FEE-THRU-AMT  (RX FR)                        ECS045
01624      ELSE                                                         ECS045
01625          MOVE RE-LF-FEE-THRU-AMT  (FR) TO                         ECS045
01626               RCT-LF-FEE-THRU-AMT  (RX FR).                       ECS045
01627                                                                   ECS045
01628      MOVE RE-AH-FEE-RANGE-PCT (FR) TO                             ECS045
01629           RCT-AH-FEE-RANGE-PCT (RX FR).                           ECS045
01630                                                                   ECS045
01631      IF RE-AH-FEE-THRU-AMT (FR) = +9999999.99                     ECS045
01632          MOVE +999999999.99            TO                         ECS045
01633               RCT-AH-FEE-THRU-AMT  (RX FR)                        ECS045
01634      ELSE                                                         ECS045
01635          MOVE RE-AH-FEE-THRU-AMT  (FR) TO                         ECS045
01636               RCT-AH-FEE-THRU-AMT  (RX FR).                       ECS045
01637                                                                   ECS045
01638      ADD +1                      TO FR.                           ECS045
01639                                                                   ECS045
01640      IF FR LESS THAN +7                                           ECS045
01641          GO TO 0137-FEE-RANGE-LOOP.                               ECS045
01642                                                                   ECS045
01643      ADD +1                      TO RX.                           ECS045
01644                                                                   ECS045
01645      IF RX GREATER THAN RCT-ENTRY-COUNT                           ECS045
01646          MOVE '**** REINSURANCE COMPANY TABLE SIZE EXCEEDED ****' ECS045
01647                                  TO WS-ABEND-MESSAGE              ECS045
01648          MOVE '0'                TO WAC-1                         ECS045
01649          MOVE '2'                TO WAC-2                         ECS045
01650          MOVE '01'               TO WAC-3-4                       ECS045
01651          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS045
01652          GO TO ABEND-PGM.                                         ECS045
01653                                                                   ECS045
01654      GO TO 0135-REIN-CO-TABLE-BUILD.                              ECS045
01655                                                                   ECS045
01656  0138-BUILD-DEFAULT-ENTRY.                                        ECS045
01657      MOVE SPACES                 TO REIN-COMPANY-ENTRIES (RX).    ECS045
01658      MOVE ZEROS TO RCT-LF-ISS-CPRM (RX)  RCT-LF-CAN-CPRM (RX)     ECS045
01659                    RCT-LF-ISS-GPRM (RX)  RCT-LF-CAN-GPRM (RX)     ECS045
01660                    RCT-AH-ISS-CPRM (RX)  RCT-AH-CAN-CPRM (RX)     ECS045
01661                    RCT-AH-ISS-GPRM (RX)  RCT-AH-CAN-GPRM (RX).    ECS045
01662                                                                   ECS045
01663      MOVE 'UNKNOWN'              TO RCT-NAME (RX)                 ECS045
01664                                     RCT-CEDE-NAME (RX).           ECS045
01665      MOVE 'P'                    TO RCT-LF-PE     (RX)            ECS045
01666                                     RCT-AH-PE     (RX).           ECS045
01667      MOVE +0.0                   TO RCT-LF-FEE    (RX)            ECS045
01668                                     RCT-AH-FEE    (RX)            ECS045
01669                                     RCT-LF-PR-PCT (RX)            ECS045
01670                                     RCT-LF-78-PCT (RX)            ECS045
01671                                     RCT-AH-PR-PCT (RX)
032707                                    RCT-EXCISE-TAX (RX)
01672      MOVE +1.0                   TO RCT-AH-78-PCT (RX).           ECS045
01673      MOVE 'Y'                    TO RCT-PRT-ST    (RX)            ECS045
01674                                     RCT-PRT-OW    (RX)            ECS045
01675                                     RCT-PRT-CRSV  (RX).           ECS045
           MOVE 'N'                    TO RCT-CEDING-STMT-OPT-A (RX)
                                          RCT-CEDING-STMT-OPT-B (RX)
                                          RCT-CEDING-STMT-OPT-C (RX)
                                          RCT-CEDING-STMT-OPT-D (RX)
                                          RCT-CEDING-STMT-OPT-E (RX)



01676      MOVE +1                     TO FR.                           ECS045
01677                                                                   ECS045
01678  0138-FEE-ZERO-LOOP.                                              ECS045
01679      MOVE ZEROS                  TO RCT-LF-FEE-RANGE-PCT (RX FR)  ECS045
01680                                     RCT-LF-FEE-THRU-AMT  (RX FR)  ECS045
01681                                     RCT-AH-FEE-RANGE-PCT (RX FR)  ECS045
01682                                     RCT-AH-FEE-THRU-AMT  (RX FR). ECS045
01683                                                                   ECS045
01684      ADD +1                      TO FR.                           ECS045
01685                                                                   ECS045
01686      IF FR LESS THAN +7                                           ECS045
01687          GO TO 0138-FEE-ZERO-LOOP.                                ECS045
01688                                                                   ECS045
01689      MOVE RX                     TO RCT-ENTRY-COUNT.              ECS045
01690                                                                   ECS045
01691      EJECT                                                        ECS045
01692 ************************************************                  ECS045
01693 *  (139) ONE TIME ROUTINE FORM PRIMING READS   *                  ECS045
01694 ************************************************                  ECS045
01695                                                                   ECS045
01696  0139-INIT-DETAIL-ACCUMULATORS.                                   ECS045
01697      PERFORM 0210-ZERO-DA THRU 0220-EXIT                          ECS045
01698          VARYING SA                                               ECS045
01699              FROM +1 BY +1                                        ECS045
01700                  UNTIL SA GREATER +8.                             ECS045
01701                                                                   ECS045
01702      PERFORM 0170-READ-EPEC            THRU 0180-EXIT.            ECS045
01703                                                                   ECS045
01704      IF EPECS-SELECTED = ZEROS                                    ECS045
01705          GO TO 0370-END-SELECT-EPEC-RECS.                         ECS045
01706                                                                   ECS045
01707      MOVE EPEC-CONTROL           TO SAVE-EPEC-CONTROL.            ECS045
01708      PERFORM 0305-SEARCH-REIN-CO-TABLE THRU 0309-EXIT.            ECS045
01709      PERFORM 0330-READ-ACC-MSTR        THRU 0340-EXIT.            ECS045
01710                                                                   ECS045
01711      EJECT                                                        ECS045
01712 *******************************************                       ECS045
01713 *  START OF SORT(1) INPUT PROCEDURE LOOP  *                       ECS045
01714 *******************************************                       ECS045
01715                                                                   ECS045
01716  0140-CHECK-NEW-ACCOUNT.                                          ECS045
01717                                                                   ECS045
01718      IF EPEC-CONTROL NOT = SAVE-EPEC-CONTROL                      ECS045
01719          PERFORM 0190-BUILD-SORT-REC THRU 0200-EXIT               ECS045
01720              IF RCT-REIN-CO (RX) NOT = SAVE-E-C-REI-COMP          ECS045
01721                  PERFORM 0305-SEARCH-REIN-CO-TABLE THRU 0309-EXIT.ECS045
01722                                                                   ECS045
01723      IF EPEC-CONTROL = HIGH-VALUES                                ECS045
01724          GO TO 0370-END-SELECT-EPEC-RECS.                         ECS045
01725                                                                   ECS045
01726 *******************************************                       ECS045
01727 *  ACCUMULATE EPEC AMOUNTS IN (DA-CNTRS)  *                       ECS045
01728 *******************************************                       ECS045
01729                                                                   ECS045
01730 **************************************************************    ECS045
01731 *  IF EPEC RUN DATE NOT GREATER THAN PREVIOUS YEAR-END DATE  *    ECS045
01732 *  ADD EPEC COUNTERS TO LIFE OR A&H BEGINNING PERIOD CNTRS   *    ECS045
01733 **************************************************************    ECS045
01734                                                                   ECS045
01735      IF EP-RECORD-ID = 'EP'                                       ECS045
01736          IF WS-EP-RUN-DTE  NOT GREATER BEG-DATE                   ECS045
01737              IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                    ECS045
01738                  ADD EP-ADJUST                                    ECS045
01739                      EP-RETRO-PAYMENTS TO DA-ADJ (1)              ECS045
01740              ELSE                                                 ECS045
01741                  ADD EP-ADJUST                                    ECS045
01742                      EP-RETRO-PAYMENTS TO DA-ADJ (2).             ECS045
01743                                                                   ECS045
122408     IF EP-RECORD-ID = 'EP'
122408        IF WS-EP-RUN-DTE  <= BEG-DATE
122408           IF EP-RCD-TYPE = LIFE-OVERRIDE-L1
122408              ADD EP-CLAIM-ADJ TO DA-CLM (1)
122408           ELSE
122408              ADD EP-CLAIM-ADJ TO DA-CLM (2)
122408           END-IF
122408        END-IF
122408     END-IF

01744 ***************************************************************   ECS045
01745 *  IF EPEC RUN DATE NOT GREATER THAN PREVIOUS MONTH-END DATE  *   ECS045
01746 *  ADD EPEC COUNTERS TO LIFE OR A&H PREVIOUS MONTH CNTRS      *   ECS045
01747 ***************************************************************   ECS045
01748                                                                   ECS045
01749      IF EP-RECORD-ID = 'EP'                                       ECS045
01750          IF WS-EP-RUN-DTE  NOT GREATER PRV-MONTH                  ECS045
01751              IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                    ECS045
01752                  ADD EP-ADJUST                                    ECS045
01753                      EP-RETRO-PAYMENTS TO DA-ADJ (3)              ECS045
01754              ELSE                                                 ECS045
01755                  ADD EP-ADJUST                                    ECS045
01756                      EP-RETRO-PAYMENTS TO DA-ADJ (4).             ECS045
01757                                                                   ECS045
122408     IF EP-RECORD-ID = 'EP'
122408        IF WS-EP-RUN-DTE  <= PRV-MONTH
122408           IF EP-RCD-TYPE = LIFE-OVERRIDE-L1
122408              ADD EP-CLAIM-ADJ TO DA-CLM (3)
122408           ELSE
122408              ADD EP-CLAIM-ADJ TO DA-CLM (4)
122408           END-IF
122408        END-IF
122408     END-IF

01758 ***************************************************************   ECS045
01759 *  ADD EPEC COUNTERS TO LIFE OR A&H CURRENT MONTH CNTRS       *   ECS045
01760 ***************************************************************   ECS045
01761                                                                   ECS045
01762      IF EP-RECORD-ID = 'EP'                                       ECS045
01763          IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                        ECS045
01764              ADD EP-ADJUST                                        ECS045
01765                  EP-RETRO-PAYMENTS TO DA-ADJ (5)                  ECS045
01766          ELSE                                                     ECS045
01767              ADD EP-ADJUST                                        ECS045
01768                  EP-RETRO-PAYMENTS TO DA-ADJ (6).                 ECS045
01769                                                                   ECS045
122408     IF EP-RECORD-ID = 'EP'
122408        IF EP-RCD-TYPE = LIFE-OVERRIDE-L1
122408           ADD EP-CLAIM-ADJ TO DA-CLM (5)
122408        ELSE
122408           ADD EP-CLAIM-ADJ TO DA-CLM (6)
122408        END-IF
122408     END-IF

01770 ******************************************************************ECS045
01771 * IF EP-PURGE=P AND EP-RUN-DTE > PREVIOUS MONTH DATE CHANGE TO (Q)ECS045
01772 * IF EP-PURGE=P AND EP-RUN-DTE > PREVIOUS YEAR  DATE CHANGE TO (R)ECS045
01773 ******************************************************************ECS045
01774                                                                   ECS045
01775      IF EP-PURGE = 'P'                                            ECS045
01776          IF WS-EP-RUN-DTE  GREATER PRV-MONTH                      ECS045
01777              MOVE 'Q' TO EP-PURGE                                 ECS045
01778              GO TO 0150-TEST-ACCUM                                ECS045
01779          ELSE                                                     ECS045
01780              IF WS-EP-RUN-DTE  GREATER BEG-DATE                   ECS045
01781                  MOVE 'R' TO EP-PURGE                             ECS045
01782                  GO TO 0150-TEST-ACCUM                            ECS045
01783              ELSE                                                 ECS045
01784                  GO TO 0150-TEST-ACCUM.                           ECS045
01785                                                                   ECS045
01786 ******************************************************************ECS045
01787 * IF EP-RUN-DTE = PREV YEAR-END AND PREV MONTH CHANGE TO (4)      ECS045
01788 * IF EP-RUN-DTE = PREV YEAR-END                CHANGE TO (1)      ECS045
01789 ******************************************************************ECS045
01790                                                                   ECS045
01791      IF WS-EP-RUN-DTE  = BEG-DATE                                 ECS045
01792          IF BEG-DATE = PRV-MONTH                                  ECS045
01793              MOVE '4'            TO EP-PURGE                      ECS045
01794              GO TO 0150-TEST-ACCUM                                ECS045
01795          ELSE                                                     ECS045
01796              MOVE '1'            TO EP-PURGE                      ECS045
01797              GO TO 0150-TEST-ACCUM.                               ECS045
01798                                                                   ECS045
01799 ******************************************************************ECS045
01800 * IF EP-RUN-DTE = PREV MONTH-END                   CHANGE TO (2)  ECS045
01801 * IF EP-RUN-DTE = CURRENT MONTH                    CHANGE TO (3)  ECS045
01802 ******************************************************************ECS045
01803                                                                   ECS045
01804      IF WS-EP-RUN-DTE  = PRV-MONTH                                ECS045
01805          MOVE '2'                TO EP-PURGE                      ECS045
01806          GO TO 0150-TEST-ACCUM.                                   ECS045
01807                                                                   ECS045
01808      IF WS-EP-RUN-DTE  = RUN-DT                                   ECS045
01809          MOVE '3'                TO EP-PURGE                      ECS045
01810          GO TO 0150-TEST-ACCUM.                                   ECS045
01811                                                                   ECS045
01812      GO TO 0160-BYPASS-ACCUM.                                     ECS045
01813                                                                   ECS045
01814      EJECT                                                        ECS045
01815  0150-TEST-ACCUM.                                                 ECS045
01816                                                                   ECS045
01817 ******************************************************************ECS045
01818 * IF PURGE RCD AND EP-RUN-DTE > PREVIOUS MONTH-END  EP-PURGE=(Q) *ECS045
01819 * IF PURGE RCD AND EP-RUN-DTE > PREVIOUS YEAR       EP-PURGE=(R) *ECS045
01820 * EP-RUN-DTE = PREV YEAR-END                        EP-PURGE=(1) *ECS045
01821 * EP-RUN-DTE = PREV MONTH-END                       EP-PURGE=(2) *ECS045
01822 * EP-RUN-DTE = CURRENT MONTH-END                    EP-PURGE=(3) *ECS045
01823 * EP-RUN-DTE = PREV YEAR-END AND PREV MONTH-END     EP-PURGE=(4) *ECS045
01824 ******************************************************************ECS045
01825 * SA = 1 (LIFE BENEFIT BEGINING)       *                          ECS045
01826 * SA = 2 (AH   BENEFIT BEGINING)       *                          ECS045
01827 * SA = 3 (LIFE BENEFIT PREVIOUS MONTH) *                          ECS045
01828 * SA = 4 (AH   BENEFIT PREVIOUS MONTH) *                          ECS045
01829 * SA = 5 (LIFE BENEFIT CURRENT MONTH)  *                          ECS045
01830 * SA = 6 (AH   BENEFIT CURRENT MONTH)  *                          ECS045
01831 ****************************************                          ECS045
01832                                                                   ECS045
01833      IF EP-RCD-TYPE = AH-OVERRIDE-L1                              ECS045
01834          MOVE CLAS-STARTA        TO CLAS-INDEXA                   ECS045
01835          GO TO 0155-EP-A-H-LOOP.                                  ECS045
01836                                                                   ECS045
01837      MOVE CLAS-STARTL            TO CLAS-INDEXL.                  ECS045
01838                                                                   ECS045
01839  0153-EP-LIFE-LOOP.                                               ECS045
01840      IF (CLAS-INDEXL GREATER CLAS-MAXL) OR (CLAS-INDEXL = +0)     ECS045
01841          DISPLAY 'INVALID LIFE BENEFIT TYPE - ' EP-BEN-CODE       ECS045
01842          MOVE '**** INVALID LIFE BENEFIT TYPE ****'               ECS045
01843                                  TO WS-ABEND-MESSAGE              ECS045
01844          MOVE '0'                TO WAC-1                         ECS045
01845          MOVE '4'                TO WAC-2                         ECS045
01846          MOVE '01'               TO WAC-3-4                       ECS045
01847          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS045
01848          GO TO ABEND-PGM.                                         ECS045
01849                                                                   ECS045
01850      IF EP-BEN-CODE NOT = CLAS-I-BEN (CLAS-INDEXL)                ECS045
01851          ADD +1                  TO CLAS-INDEXL                   ECS045
01852          GO TO 0153-EP-LIFE-LOOP.                                 ECS045
01853                                                                   ECS045
01854      GO TO 0159-CONTINUE-ACCUM-TEST.                              ECS045
01855                                                                   ECS045
01856  0155-EP-A-H-LOOP.                                                ECS045
01857      IF (CLAS-INDEXA GREATER CLAS-MAXA) OR (CLAS-INDEXA = +0)     ECS045
01858          DISPLAY 'INVALID A&H BENEFIT TYPE - ' EP-BEN-CODE        ECS045
01859          MOVE '**** INVALID A&H BENEFIT TYPE ****'                ECS045
01860                                  TO WS-ABEND-MESSAGE              ECS045
01861          MOVE '0'                TO WAC-1                         ECS045
01862          MOVE '4'                TO WAC-2                         ECS045
01863          MOVE '01'               TO WAC-3-4                       ECS045
01864          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS045
01865          GO TO ABEND-PGM.                                         ECS045
01866                                                                   ECS045
01867      IF EP-BEN-CODE NOT = CLAS-I-BEN (CLAS-INDEXA)                ECS045
01868          ADD +1                  TO CLAS-INDEXA                   ECS045
01869          GO TO 0155-EP-A-H-LOOP.                                  ECS045
01870                                                                   ECS045
01871  0159-CONTINUE-ACCUM-TEST.                                        ECS045
01872                                                                   ECS045
01873      IF EP-RECORD-ID = 'EP'                                       ECS045
01874          IF EP-PURGE = ('1' OR '4' OR 'P')                        ECS045
01875              IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                    ECS045
01876                  MOVE +1         TO SA                            ECS045
01877                  PERFORM 0230-EP-ADD THRU 0250-EXIT               ECS045
01878              ELSE                                                 ECS045
01879                  MOVE +2         TO SA                            ECS045
01880                  PERFORM 0230-EP-ADD THRU 0250-EXIT.              ECS045
01881                                                                   ECS045
01882      IF EP-RECORD-ID = 'EP'                                       ECS045
01883          IF EP-PURGE = ('2' OR '4' OR 'P' OR 'R')                 ECS045
01884              IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                    ECS045
01885                  MOVE +3         TO SA                            ECS045
01886                  PERFORM 0230-EP-ADD THRU 0250-EXIT               ECS045
01887              ELSE                                                 ECS045
01888                  MOVE +4         TO SA                            ECS045
01889                  PERFORM 0230-EP-ADD THRU 0250-EXIT.              ECS045
01890                                                                   ECS045
01891      IF EP-RECORD-ID = 'EP'                                       ECS045
01892          IF EP-PURGE = ('3' OR 'P' OR 'R' OR 'Q')                 ECS045
01893              IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                    ECS045
01894                  MOVE +5         TO SA                            ECS045
01895                  PERFORM 0230-EP-ADD THRU 0250-EXIT               ECS045
01896              ELSE                                                 ECS045
01897                  MOVE +6         TO SA                            ECS045
01898                  PERFORM 0230-EP-ADD THRU 0250-EXIT.              ECS045
01899                                                                   ECS045
01900      IF EP-RECORD-ID = 'EC'                                       ECS045
01901          IF EP-PURGE = ('1' OR '4' OR 'P')                        ECS045
01902              IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                    ECS045
01903                  MOVE +1         TO SA                            ECS045
01904                  PERFORM 0260-EC-ADD THRU 0300-EXIT               ECS045
01905              ELSE                                                 ECS045
01906                  MOVE +2         TO SA                            ECS045
01907                  PERFORM 0260-EC-ADD THRU 0300-EXIT.              ECS045
01908                                                                   ECS045
01909      IF EP-RECORD-ID = 'EC'                                       ECS045
01910          IF EP-PURGE = ('2' OR '4' OR 'P' OR 'R')                 ECS045
01911              IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                    ECS045
01912                  MOVE +3         TO SA                            ECS045
01913                  PERFORM 0260-EC-ADD THRU 0300-EXIT               ECS045
01914              ELSE                                                 ECS045
01915                  MOVE +4         TO SA                            ECS045
01916                  PERFORM 0260-EC-ADD THRU 0300-EXIT.              ECS045
01917                                                                   ECS045
01918      IF EP-RECORD-ID = 'EC'                                       ECS045
01919          IF EP-PURGE = ('3' OR 'P' OR 'R' OR 'Q')                 ECS045
01920              IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                    ECS045
01921                  MOVE +5         TO SA                            ECS045
01922                  PERFORM 0260-EC-ADD THRU 0300-EXIT               ECS045
01923              ELSE                                                 ECS045
01924                  MOVE +6         TO SA                            ECS045
01925                  PERFORM 0260-EC-ADD THRU 0300-EXIT.              ECS045
01926                                                                   ECS045
01927  0160-BYPASS-ACCUM.                                               ECS045
01928      PERFORM 0170-READ-EPEC THRU 0180-EXIT.                       ECS045
01929      GO TO 0140-CHECK-NEW-ACCOUNT.                                ECS045
01930                                                                   ECS045
01931 *****************************************                         ECS045
01932 *  END OF SORT(1) INPUT PROCEDURE LOOP  *                         ECS045
01933 *****************************************                         ECS045
01934                                                                   ECS045
01935      EJECT                                                        ECS045
01936 ****************************                                      ECS045
01937 *  READ ROUTINE EPEC FILE  *                                      ECS045
01938 ****************************                                      ECS045
01939                                                                   ECS045
01940  0170-READ-EPEC.                                                  ECS045
01941      READ EPEC-FILE AT END                                        ECS045
01942          MOVE HIGH-VALUES TO EP-RECORD                            ECS045
01943          GO TO 0180-EXIT.                                         ECS045
01944                                                                   ECS045
01945      ADD +1                   TO EPECS-READ-COUNT.                ECS045
01946                                                                   ECS045
01947      IF EP-RECORD-ID NOT = 'EP' AND 'EC'                          ECS045
01948          GO TO 0170-READ-EPEC.                                    ECS045
01949                                                                   ECS045
01950      IF EP-REIN NOT = 'R'                                         ECS045
01951          GO TO 0170-READ-EPEC.                                    ECS045
01952                                                                   ECS045
01953 ***********************************************************       ECS045
01954 *  DROP EPECS WITH EP-RUN-DATE GREATER THAN CURRENT DATE  *       ECS045
01955 ***********************************************************       ECS045
01956                                                                   ECS045
01957      COPY ELCEPCM1.                                               ECS045
01958                                                                   ECS045
01959      MOVE ZEROS                  TO EP-RUN-DA.                    ECS045
01960                                                                   ECS045
01961      IF WS-EP-RUN-DTE GREATER RUN-DT                              ECS045
01962          GO TO 0170-READ-EPEC.                                    ECS045
01963                                                                   ECS045
01964      ADD +1                      TO EPECS-SELECTED.               ECS045
01965                                                                   ECS045
01966  0180-EXIT.                                                       ECS045
01967                                                                   ECS045
01968      MOVE EP-CONTROL             TO E-C-CTL.                      ECS045
01969      MOVE EP-REI-CO              TO E-C-REI-COMP.                 ECS045
01970                                                                   ECS045
01971      EJECT                                                        ECS045
01972 *******************************************************           ECS045
01973 *  CONSTRUCT RR-REC AND RELEASE SORT-REC FORM RR-REC  *           ECS045
01974 *  FOR THE (45A) (45B) (45D) (45E)                    *           ECS045
01975 *******************************************************           ECS045
01976                                                                   ECS045
01977  0190-BUILD-SORT-REC.                                             ECS045
01978      MOVE SPACES                 TO RR-REC.                       ECS045
01979      MOVE ZEROS                  TO RR-LF-TAX                     ECS045
01980                                     RR-AH-TAX                     ECS045
01981                                     RR-ST-TAX.                    ECS045
01982                                                                   ECS045
01983 **********  BUILD REPORT (A / B / C / D) SORT KEY  ***********    ECS045
01984                                                                   ECS045
01985      MOVE SAVE-E-C-CARRIER       TO RR-A-CARR                     ECS045
01986                                     RR-B-CARR                     ECS045
01987                                     RR-D-CARR.                    ECS045
01988      MOVE SAVE-E-C-GROUPING      TO RR-A-CERT-GROUP.              ECS045
01989      MOVE SAVE-E-C-STATE         TO RR-A-STATE                    ECS045
01990                                     RR-B-STATE.                   ECS045
01991      MOVE SAVE-E-C-ACCOUNT       TO RR-A-ACCT.                    ECS045
01992      MOVE SAVE-E-C-EXP-DATE      TO RR-A-EXP-DT.                  ECS045
01993      MOVE SAVE-E-C-EFF-DATE      TO RR-A-EFF-DT.                  ECS045
01994      IF DTE-CLIENT = 'NCL'                                        ECS045
01995          MOVE 99999999999        TO RR-A-EXP-DT                      CL*18
01996          MOVE ZEROS              TO RR-A-EFF-DT.                  ECS045
01997      MOVE SAVE-E-C-REI-COMP      TO RR-REI-COMP.                  ECS045
01998      MOVE RR-REI-PRIME           TO RR-A-REI-PRIME                ECS045
01999                                     RR-B-REI-PRIME                ECS045
02000                                     RR-D-REI-PRIME                ECS045
02001                                     RR-E-REI-PRIME.               ECS045
02002      MOVE RR-REI-SUB             TO RR-A-REI-SUB                  ECS045
02003                                     RR-B-REI-SUB                  ECS045
02004                                     RR-E-REI-SUB.                 ECS045
02005      MOVE RCT-NAME (RX)          TO RR-C-REI-NAME.                ECS045
02006      MOVE RCT-REIN-GROUP (RX)    TO RR-D-REI-GRP.                 ECS045
02007      MOVE RCT-CEDE-NAME (RX)     TO RR-C-CEDE-NAME.               ECS045
02008                                                                   ECS045
02009      MOVE RCT-LF-PE (RX)         TO RR-PE-LF.                     ECS045
02010      MOVE RCT-AH-PE (RX)         TO RR-PE-AH.                     ECS045
02011      MOVE RCT-LF-FEE (RX)        TO RR-FEE-LF.                    ECS045
02012      MOVE RCT-AH-FEE (RX)        TO RR-FEE-AH
032707     MOVE RCT-EXCISE-TAX (RX)    TO RR-EXCISE-TAX
02013      MOVE RCT-AH-PR-PCT (RX)     TO RR-AH-PR-PCT.                 ECS045
02014      MOVE RCT-AH-78-PCT (RX)     TO RR-AH-78-PCT.                 ECS045
02015      MOVE RCT-LF-PR-PCT (RX)     TO RR-LF-PR-PCT.                 ECS045
02016      MOVE RCT-LF-78-PCT (RX)     TO RR-LF-78-PCT.                 ECS045
02017      MOVE RCT-PRT-ST (RX)        TO RR-PRT-ST.                    ECS045
02018      MOVE RCT-PRT-OW (RX)        TO RR-PRT-OW.                    ECS045
02019      IF DTE-CLIENT = 'NCL'                                        ECS045
02020          MOVE RCT-PRT-CRSV (RX)  TO RR-PRT-CRSV.                  ECS045
02021      MOVE RCT-MORT-CODE (RX)     TO RR-MORT.                      ECS045
02022      MOVE RCT-CLAIM-CODE (RX)    TO RR-CLAIM-CODE.                ECS045
02023                                                                   ECS045
02024      IF RR-CLAIM-CODE = 'P'                                       ECS045
02025          MOVE 'P'                TO RR-PI-CLM-LF  RR-PI-CLM-AH.   ECS045
02026      IF RR-CLAIM-CODE = 'I'                                       ECS045
02027          MOVE 'I'                TO RR-PI-CLM-LF  RR-PI-CLM-AH.   ECS045
02028      IF RR-CLAIM-CODE = 'X'                                       ECS045
02029          MOVE 'P'                TO RR-PI-CLM-LF                  ECS045
02030          MOVE 'I'                TO RR-PI-CLM-AH.                 ECS045
02031      IF RR-CLAIM-CODE = 'Y'                                       ECS045
02032          MOVE 'I'                TO RR-PI-CLM-LF                  ECS045
02033          MOVE 'P'                TO RR-PI-CLM-AH.                 ECS045
02034                                                                   ECS045
02035      MOVE RCT-ZERO-LF-FEE (RX)   TO RR-0-PE-FEE-LF.               ECS045
02036      MOVE RCT-ZERO-AH-FEE (RX)   TO RR-0-PE-FEE-AH.               ECS045
02037      MOVE RCT-LF-COMM (RX)       TO RR-PE-COMM-LF.                ECS045
02038      MOVE RCT-AH-COMM (RX)       TO RR-PE-COMM-AH.                ECS045
02039      MOVE RCT-LF-TAX (RX)        TO RR-PE-TAX-LF.                 ECS045
02040      MOVE RCT-AH-TAX (RX)        TO RR-PE-TAX-AH.                 ECS045
02041                                                                   ECS045
02042      IF RCT-LF-IBNR-PCT (RX) NOT NUMERIC                          ECS045
02043         MOVE ZERO                TO RCT-LF-IBNR-PCT (RX).         ECS045
02044                                                                   ECS045
02045      IF RCT-AH-IBNR-PCT (RX) NOT NUMERIC                          ECS045
02046         MOVE ZERO                TO RCT-AH-IBNR-PCT (RX).         ECS045
02047                                                                   ECS045
02048      MOVE RCT-LF-IBNR-PCT (RX)   TO RR-LF-IBNR-PCT.               ECS045
02049      MOVE RCT-AH-IBNR-PCT (RX)   TO RR-AH-IBNR-PCT.               ECS045
02050                                                                   ECS045
02051      IF DTE-CLIENT = 'LAP'                                        ECS045
02052          IF RR-A-STATE = 'WV'                                     ECS045
02053              MOVE .200           TO RR-AH-IBNR-PCT                ECS045
02054          ELSE                                                     ECS045
02055              MOVE .150           TO RR-AH-IBNR-PCT.               ECS045
02056                                                                   ECS045
02057      MOVE DETAIL-ACCUMULATORS    TO RR-ACCUMS.                    ECS045
02058      PERFORM 0310-MATCH-FILES THRU 0320-EXIT.                     ECS045
02059                                                                   ECS045
02060      IF RR-PRT-ST NOT = 'F' AND 'N'                               ECS045
02061          MOVE 'Y'                TO RR-PRT-ST.                    ECS045
02062                                                                   ECS045
02063      IF RR-PRT-OW NOT = 'F' AND 'A' AND 'N'                       ECS045
02064          MOVE 'Y'                TO RR-PRT-OW.                    ECS045
02065                                                                   ECS045
02066      EJECT                                                        ECS045
02067  0195-UPDATE-REIN-CO-TABLE.                                       ECS045
02068                                                                   ECS045
02069      IF RCT-REIN-CO (RX) = HIGH-VALUES                            ECS045
02070          GO TO 0199-RELEASE-SORT-A.                               ECS045
02071                                                                   ECS045
02072      IF DTE-PGM-OPT = '3'                                         ECS045
02073          COMPUTE RCT-LF-ISS-CPRM (RX) = RCT-LF-ISS-CPRM (RX) +    ECS045
02074                                       (DA-ISP (5) - DA-ISP (3))   ECS045
02075          COMPUTE RCT-LF-ISS-GPRM (RX) = RCT-LF-ISS-GPRM (RX) +    ECS045
02076                                       (DA-ISGP (5) - DA-ISGP (3)) ECS045
02077          COMPUTE RCT-LF-CAN-CPRM (RX) = RCT-LF-CAN-CPRM (RX) +    ECS045
02078                                       (DA-CNP (5) - DA-CNP (3))   ECS045
02079          COMPUTE RCT-LF-CAN-GPRM (RX) = RCT-LF-CAN-GPRM (RX) +    ECS045
02080                                       (DA-CNGP (5) - DA-CNGP (3)) ECS045
02081          COMPUTE RCT-AH-ISS-CPRM (RX) = RCT-AH-ISS-CPRM (RX) +    ECS045
02082                                       (DA-ISP (6) - DA-ISP (4))   ECS045
02083          COMPUTE RCT-AH-ISS-GPRM (RX) = RCT-AH-ISS-GPRM (RX) +    ECS045
02084                                       (DA-ISGP (6) - DA-ISGP (4)) ECS045
02085          COMPUTE RCT-AH-CAN-CPRM (RX) = RCT-AH-CAN-CPRM (RX) +    ECS045
02086                                       (DA-CNP (6) - DA-CNP (4))   ECS045
02087          COMPUTE RCT-AH-CAN-GPRM (RX) = RCT-AH-CAN-GPRM (RX) +    ECS045
02088                                       (DA-CNGP (6) - DA-CNGP (4)) ECS045
02089      ELSE                                                         ECS045
02090          COMPUTE RCT-LF-ISS-CPRM (RX) = RCT-LF-ISS-CPRM (RX) +    ECS045
02091                                       (DA-ISP (5) - DA-ISP (1))   ECS045
02092          COMPUTE RCT-LF-ISS-GPRM (RX) = RCT-LF-ISS-GPRM (RX) +    ECS045
02093                                       (DA-ISGP (5) - DA-ISGP (1)) ECS045
02094          COMPUTE RCT-LF-CAN-CPRM (RX) = RCT-LF-CAN-CPRM (RX) +    ECS045
02095                                       (DA-CNP (5) - DA-CNP (1))   ECS045
02096          COMPUTE RCT-LF-CAN-GPRM (RX) = RCT-LF-CAN-GPRM (RX) +    ECS045
02097                                       (DA-CNGP (5) - DA-CNGP (1)) ECS045
02098          COMPUTE RCT-AH-ISS-CPRM (RX) = RCT-AH-ISS-CPRM (RX) +    ECS045
02099                                       (DA-ISP (6) - DA-ISP (2))   ECS045
02100          COMPUTE RCT-AH-ISS-GPRM (RX) = RCT-AH-ISS-GPRM (RX) +    ECS045
02101                                       (DA-ISGP (6) - DA-ISGP (2)) ECS045
02102          COMPUTE RCT-AH-CAN-CPRM (RX) = RCT-AH-CAN-CPRM (RX) +    ECS045
02103                                       (DA-CNP (6) - DA-CNP (2))   ECS045
02104          COMPUTE RCT-AH-CAN-GPRM (RX) = RCT-AH-CAN-GPRM (RX) +    ECS045
02105                                       (DA-CNGP (6) - DA-CNGP (2)).ECS045
02106                                                                   ECS045
02107      EJECT                                                        ECS045
02108  0199-RELEASE-SORT-A.                                             ECS045
02109 ***********************************                               ECS045
02110 *  RELEASE REPORT(A) SORT RECORD  *                               ECS045
02111 ***********************************                               ECS045
02112                                                                   ECS045
02113      IF ACCOUNT-NOT-PRINTED                                       ECS045
02114          GO TO 0199-RELEASE-SORT-B.                               ECS045
02115                                                                   ECS045
02116      IF RCTRPT-A-WANTED (RX)                                      ECS045
02117          MOVE 'A'                TO RR-REPORT-ID                  ECS045
02118          RELEASE SW-REC  FROM RR-REC                              ECS045
02119          ADD +1                  TO EPECS-SORTED-SORT1.           ECS045
02120                                                                   ECS045
02121  0199-RELEASE-SORT-B.                                             ECS045
02122 ***********************************                               ECS045
02123 *  RELEASE REPORT(B) SORT RECORD  *                               ECS045
02124 ***********************************                               ECS045
02125                                                                   ECS045
02126      IF RCTRPT-B-WANTED (RX)                                      ECS045
02127          MOVE 'B'                TO RR-REPORT-ID                  ECS045
02128          MOVE RR-REPORT-A-CNTL   TO SAVE-RR-REPORT-CNTL           ECS045
02129          MOVE RR-REPORT-B-CNTL   TO RR-REPORT-A-CNTL              ECS045
02130          MOVE SAVE-RR-REPORT-CNTL                                 ECS045
02131                                  TO RR-REPORT-B-CNTL              ECS045
02132          RELEASE SW-REC FROM RR-REC                               ECS045
02133          MOVE RR-REPORT-A-CNTL   TO SAVE-RR-REPORT-CNTL           ECS045
02134          MOVE RR-REPORT-B-CNTL   TO RR-REPORT-A-CNTL              ECS045
02135          MOVE SAVE-RR-REPORT-CNTL                                 ECS045
02136                                  TO RR-REPORT-B-CNTL              ECS045
02137          ADD +1                  TO EPECS-SORTED-SORT1.           ECS045
02138                                                                   ECS045
02139  0199-WRITE-SORT-C.                                               ECS045
02140 ***********************************                               ECS045
02141 *   WRITE REPORT(C) SORT RECORD   *                               ECS045
02142 ***********************************                               ECS045
02143                                                                   ECS045
02144      IF RCTRPT-C-WANTED (RX)                                      ECS045
02145          MOVE 'C'                TO RR-REPORT-ID                  ECS045
02146          MOVE RR-REC             TO RW-REC                        ECS045
02147          ADD +1                  TO RW-WRITE-CNT                  ECS045
02148          WRITE RW-REC.                                            ECS045
02149                                                                   ECS045
02150  0199-RELEASE-SORT-D.                                             ECS045
02151 ***********************************                               ECS045
02152 *  RELEASE REPORT(D) SORT RECORD  *                               ECS045
02153 ***********************************                               ECS045
02154                                                                   ECS045
02155      IF RCTRPT-D-WANTED (RX)                                      ECS045
02156          MOVE 'D'                TO RR-REPORT-ID                  ECS045
02157          MOVE RR-REPORT-A-CNTL   TO SAVE-RR-REPORT-CNTL           ECS045
02158          MOVE RR-REPORT-D-CNTL   TO RR-REPORT-A-CNTL              ECS045
02159          MOVE SAVE-RR-REPORT-CNTL                                 ECS045
02160                                  TO RR-REPORT-D-CNTL              ECS045
02161          RELEASE SW-REC  FROM RR-REC                              ECS045
02162          MOVE RR-REPORT-A-CNTL   TO SAVE-RR-REPORT-CNTL           ECS045
02163          MOVE RR-REPORT-D-CNTL   TO RR-REPORT-A-CNTL              ECS045
02164          MOVE SAVE-RR-REPORT-CNTL                                 ECS045
02165                                  TO RR-REPORT-D-CNTL              ECS045
02166          ADD +1                  TO EPECS-SORTED-SORT1.           ECS045
02167                                                                   ECS045
02168  0199-RELEASE-SORT-E.                                             ECS045
02169 ***********************************                               ECS045
02170 *  RELEASE REPORT(E) SORT RECORD  *                               ECS045
02171 ***********************************                               ECS045
02172                                                                   ECS045
02173      IF RCTRPT-E-WANTED (RX)                                      ECS045
02174          MOVE 'E'                TO RR-REPORT-ID                  ECS045
02175          MOVE RR-REPORT-A-CNTL   TO SAVE-RR-REPORT-CNTL           ECS045
02176          MOVE RR-REPORT-E-CNTL   TO RR-REPORT-A-CNTL              ECS045
02177          MOVE SAVE-RR-REPORT-CNTL                                 ECS045
02178                                  TO RR-REPORT-E-CNTL              ECS045
02179          RELEASE SW-REC FROM RR-REC                               ECS045
02180          MOVE RR-REPORT-A-CNTL   TO SAVE-RR-REPORT-CNTL           ECS045
02181          MOVE RR-REPORT-E-CNTL   TO RR-REPORT-A-CNTL              ECS045
02182          MOVE SAVE-RR-REPORT-CNTL                                 ECS045
02183                                  TO RR-REPORT-E-CNTL              ECS045
02184          ADD +1                  TO EPECS-SORTED-SORT1.           ECS045
02185                                                                   ECS045
02186      MOVE EPEC-CONTROL           TO SAVE-EPEC-CONTROL.            ECS045
02187                                                                   ECS045
02188      PERFORM 0210-ZERO-DA THRU 0220-EXIT VARYING SA               ECS045
02189              FROM +1 BY +1  UNTIL SA GREATER +8.                  ECS045
02190                                                                   ECS045
02191  0200-EXIT.                                                       ECS045
02192      EXIT.                                                        ECS045
02193                                                                   ECS045
02194      EJECT                                                        ECS045
02195  0210-ZERO-DA.                                                    ECS045
02196      MOVE ZEROS                  TO DA-ISGP (SA)                  ECS045
02197                                     DA-ISP  (SA)                  ECS045
PEMMOD                                    DA-TAX  (SA)                  ECS045
02198                                     DA-CNGP (SA)                  ECS045
02199                                     DA-CNP  (SA)                  ECS045
02200                                     DA-UPR  (SA)                  ECS045
02201                                     DA-UPP  (SA)                  ECS045
02202                                     DA-UP   (SA)                  ECS045
02203                                     DA-CLM  (SA)                  ECS045
02204                                     DA-ADJ  (SA)                  ECS045
02205                                     DA-MORT (SA)                  ECS045
02206                                     DA-ISC  (SA)                  ECS045
02207                                     DA-CNC  (SA)                  ECS045
02208                                     DA-UCR  (SA)                  ECS045
02209                                     DA-UCP  (SA)                  ECS045
02210                                     DA-UC   (SA)                  ECS045
02211                                     DA-ISO  (SA)                  ECS045
02212                                     DA-CNO  (SA)                  ECS045
02213                                     DA-UOR  (SA)                  ECS045
02214                                     DA-UOP  (SA)                  ECS045
02215                                     DA-UO   (SA)                  ECS045
02216                                     DA-CA   (SA)                  ECS045
02217                                     DA-CU   (SA)                  ECS045
02218                                     DA-CF   (SA)                  ECS045
02219                                     DA-IF   (SA).                 ECS045
02220                                                                   ECS045
02221  0220-EXIT.                                                       ECS045
02222      EXIT.                                                        ECS045
02223                                                                   ECS045
02224      EJECT                                                        ECS045
02225  0230-EP-ADD.                                                     ECS045
02226                                                                   ECS045
02227      IF EP-PRM-78-ADJ NOT NUMERIC                                 ECS045
02228          MOVE ZEROS              TO EP-PRM-78-ADJ.                ECS045
02229      IF EP-PRM-PR-ADJ NOT NUMERIC                                 ECS045
02230          MOVE ZEROS              TO EP-PRM-PR-ADJ.                ECS045
02231      IF EP-PRM-ST-ADJ NOT NUMERIC                                 ECS045
02232          MOVE ZEROS              TO EP-PRM-ST-ADJ.                ECS045
02233      IF EP-CLM-DU  NOT NUMERIC                                    ECS045
02234         MOVE ZERO                TO EP-CLM-DU.                    ECS045
02235      IF EP-CLM-PV  NOT NUMERIC                                    ECS045
02236         MOVE ZERO                TO EP-CLM-PV.                    ECS045
02237      IF EP-CLM-IBNR NOT NUMERIC                                   ECS045
02238         MOVE ZERO                TO EP-CLM-IBNR.                  ECS045
02239      IF EP-LOSS-RESV NOT NUMERIC                                  ECS045
02240         MOVE ZERO                TO EP-LOSS-RESV.

122408     IF EP-CLAIM-ADJ NOT NUMERIC
122408        MOVE ZEROS               TO EP-CLAIM-ADJ
122408     END-IF

02242      IF DTE-CLIENT = 'GTL'                                        ECS045
02243          MOVE EP-PRM-ST          TO EP-PRM-78                     ECS045
02244                                     EP-PRM-PR                     ECS045
02245          MOVE EP-PRM-ST-ADJ      TO EP-PRM-78-ADJ                 ECS045
02246                                     EP-PRM-PR-ADJ.                ECS045
02247                                                                   ECS045
100703     IF (DTE-CLIENT = 'DCC')
              AND (EP-RCD-TYPE = AH-OVERRIDE-L1)
092405        AND (CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L')
              MOVE EP-PRM-ST           TO EP-PRM-78
                                          EP-PRM-PR
              MOVE EP-PRM-ST-ADJ       TO EP-PRM-78-ADJ
                                          EP-PRM-PR-ADJ
100703     END-IF

02248      IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                            ECS045
02249          IF CLAS-I-EP (CLAS-INDEXL) = 'B'                         ECS045
02250              MOVE EP-PRM-ST      TO EP-PRM-78                     ECS045
02251                                     EP-PRM-PR                     ECS045
02252              MOVE EP-PRM-ST-ADJ  TO EP-PRM-78-ADJ                 ECS045
02253                                     EP-PRM-PR-ADJ.                ECS045
02254                                                                   ECS045
02255                                                                   ECS045
02256      ADD EP-ISS-PRM-GROSS        TO DA-ISGP (SA).                 ECS045
PEMMOD     ADD EP-PRM-TAX              TO DA-TAX (SA)
02257                                                                   ECS045
02258      IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                            ECS045
02259          IF CLAS-I-EP (CLAS-INDEXL) = 'R' OR 'N' OR 'T' OR        ECS045
02260                                       'A' OR 'B'                  ECS045
02261              COMPUTE DA-ISP (SA) = DA-ISP (SA) +                  ECS045
02262                                    (EP-ISS-PRM - EP-PRM-78-ADJ)   ECS045
02263          ELSE                                                     ECS045
02264              COMPUTE DA-ISP (SA) = DA-ISP (SA) +                  ECS045
02265                                    (EP-ISS-PRM - EP-PRM-PR-ADJ)   ECS045
02266      ELSE                                                         ECS045
02267          IF CLAS-I-EP (CLAS-INDEXA) = 'R'                         ECS045
02268              COMPUTE DA-ISP (SA) = DA-ISP (SA) +                  ECS045
02269                                    (EP-ISS-PRM - EP-PRM-78-ADJ)   ECS045
02270          ELSE                                                     ECS045
02271              IF CLAS-I-EP (CLAS-INDEXA) = 'M'                     ECS045
02272                  COMPUTE DA-ISP (SA) = DA-ISP (SA) + EP-ISS-PRM - ECS045
02273                           ((EP-PRM-78-ADJ + EP-PRM-PR-ADJ) / 2)   ECS045
02274              ELSE                                                 ECS045
02275                  COMPUTE DA-ISP (SA) = DA-ISP (SA) +              ECS045
02276                                     (EP-ISS-PRM - EP-PRM-PR-ADJ). ECS045
02277                                                                   ECS045
02278      ADD EP-CNC-PRM-GROSS        TO DA-CNGP (SA).                 ECS045
02279      ADD EP-CNC-PRM              TO DA-CNP  (SA).                 ECS045
02280      ADD EP-CLM-AMT              TO DA-CLM  (SA).                 ECS045
060903     IF DTE-CLIENT NOT = 'DCC'
060903        ADD EP-MORT-RESV         TO DA-MORT (SA)
060903     END-IF
02282      ADD EP-CLM-DU               TO DA-CA   (SA).                 ECS045
02283      ADD EP-CLM-IBNR             TO DA-CU   (SA).                 ECS045
02284      ADD EP-CLM-PV               TO DA-CF   (SA).                 ECS045
02285      ADD EP-LOSS-RESV            TO DA-CF   (SA).                 ECS045
02286      ADD EP-IN-FORCE             TO DA-IF   (SA).                 ECS045
02287                                                                   ECS045
02288      IF LIFE-PERCENTS-AVAILABLE  OR                               ECS045
02289         EP-RCD-TYPE = AH-OVERRIDE-L1                              ECS045
02290          COMPUTE DA-UPR (SA) = DA-UPR (SA) +                      ECS045
02291                          ((EP-ISS-PRM - EP-CNC-PRM) -             ECS045
02292                           (EP-PRM-78 + EP-PRM-78-ADJ))            ECS045
02293          COMPUTE DA-UPP (SA) = DA-UPP (SA) +                      ECS045
02294                          ((EP-ISS-PRM - EP-CNC-PRM) -             ECS045
02295                           (EP-PRM-PR + EP-PRM-PR-ADJ))            ECS045
02296          GO TO 0250-EXIT.                                         ECS045
02297                                                                   ECS045
02298      IF CLAS-I-EP (CLAS-INDEXL) = 'R' OR 'N' OR 'T' OR 'A' OR 'B' ECS045
02299          COMPUTE DA-UPR (SA) = DA-UPR (SA) +                      ECS045
02300                                ((EP-ISS-PRM - EP-CNC-PRM) -       ECS045
02301                                 (EP-PRM-78 + EP-PRM-78-ADJ))      ECS045
02302      ELSE                                                         ECS045
02303          COMPUTE DA-UPP (SA) = DA-UPP (SA) +                      ECS045
02304                                ((EP-ISS-PRM - EP-CNC-PRM) -       ECS045
02305                                 (EP-PRM-PR + EP-PRM-PR-ADJ))      ECS045
060903     END-IF
060903        .
02306                                                                   ECS045
02307  0250-EXIT.                                                       ECS045
02308      EXIT.                                                        ECS045
02309                                                                   ECS045
02310      EJECT                                                        ECS045
02311  0260-EC-ADD.                                                     ECS045
02312      MOVE ZERO                   TO SB.                           ECS045
02313                                                                   ECS045
02314  0280-EC-ADD-LOOP.                                                ECS045
02315                                                                   ECS045
02316 ******************************************************************ECS045
02317 * EC-AGT-TYPE=(R) ACCOUNT LEVEL COMMISSION REINSURANCE ONLY      *ECS045
02318 * EC-AGT-TYPE=(D) ACCOUNT LEVEL COMMISSION GROSS AND REINSURANCE *ECS045
02319 * EC-AGT-TYPE=(T) OVERWRITE COMMISSION REINSURANCE ONLY          *ECS045
02320 * EC-AGT-TYPE=(P) OVERWRITE COMMISSION GROSS AND REINSURANCE     *ECS045
02321 * EC-AGT-TYPE=(W) REINSURANCE COMMISSION ONLY NO AGENTS INVOLVED *ECS045
02322 *                                                                *ECS045
02323 * EC-AGT-TYPE=(U) REINSURANCE SERVICE FEE REINSURANCE ONLY       *ECS045
02324 * EC-AGT-TYPE=(S) REINSURANCE SERVICE FEE GROSS AND REINSURANCE  *ECS045
02325 *                                                                *ECS045
02326 * EC-AGT-TYPE=(V) OVERWRITE SERVICE FEE REINSURANCE ONLY         *ECS045
02327 * EC-AGT-TYPE=(B) OVERWRITE SERVICE ON GROS AND REINSURANCE      *ECS045
02328 *                                                                *ECS045
02329 ******************************************************************ECS045
02330 * (DA-ISC)  ACCOUNT   LEVEL ISSUE  COMMISSION            *        ECS045
02331 * (DA-CNC)  ACCOUNT   LEVEL CANCEL COMMISSION            *        ECS045
02332 * (DA-ISO)  OVERWRITE LEVEL ISSUE  COMMISSION            *        ECS045
02333 * (DA-CNO)  OVERWRITE LEVEL CANCEL COMMISSION            *        ECS045
02334 * (DA-UCR)  UNEARNED ACCOUNT COMMISSIONS   RULE-78       *        ECS045
02335 * (DA-UCP)  UNEARNED ACCOUNT COMMISSIONS   PRO-RATA      *        ECS045
02336 * (DA-UOR)  UNEARNED OVERWRITE COMMISSIONS RULE-78       *        ECS045
02337 * (DA-UOP)  UNEARNED OVERWRITE COMMISSIONS PRO-RATA      *        ECS045
02338 **********************************************************        ECS045
02339                                                                   ECS045
02340      ADD +1                      TO SB.                           ECS045
02341                                                                   ECS045
02342      IF SB GREATER +5                                             ECS045
02343          GO TO 0300-EXIT.                                         ECS045
02344                                                                   ECS045
02345      IF EC-AGT-TYPE (SB) = 'W'                                    ECS045
02346          MOVE EC-AGT-NO (SB)     TO WS-AGT                        ECS045
02347          IF WS-REI-AGT NOT = EC-REI-CO                            ECS045
02348              GO TO 0280-EC-ADD-LOOP.                              ECS045
02349                                                                   ECS045
02350      IF EC-AGT-TYPE (SB) NOT = 'R' AND 'D' AND                    ECS045
02351                                'T' AND 'P' AND 'W'                ECS045
100703                          AND 'K' AND 'M'
02352          GO TO 0280-EC-ADD-LOOP.                                  ECS045
02353                                                                   ECS045
02354      IF EC-COMM-78-ADJ (SB) NOT NUMERIC                           ECS045
02355          MOVE ZEROS              TO EC-COMM-78-ADJ (SB).          ECS045
02356      IF EC-COMM-PR-ADJ (SB) NOT NUMERIC                           ECS045
02357          MOVE ZEROS              TO EC-COMM-PR-ADJ (SB).          ECS045
02358                                                                   ECS045
02359      IF DTE-CLIENT = 'GTL'                                        ECS045
02360          MOVE EC-COMM-ST (SB)      TO EC-COMM-78 (SB)             ECS045
02361                                       EC-COMM-PR (SB).            ECS045
02362                                                                   ECS045
02363      IF EC-RCD-TYPE = LIFE-OVERRIDE-L1                            ECS045
02364          IF CLAS-I-EP (CLAS-INDEXL) = 'B'                         ECS045
02365              MOVE EC-COMM-ST (SB)  TO EC-COMM-78 (SB)             ECS045
02366                                       EC-COMM-PR (SB).            ECS045
02367                                                                   ECS045
02368      IF (EC-AGT-TYPE (SB) = 'R' OR 'D')
071207        OR ((EC-AGT-TYPE (SB) = 'K')
071207           AND (EC-CARRIER = '4'))
02369         ADD EC-CNC-COMM (SB)     TO DA-CNC (SA)
02370      ELSE
02371         ADD EC-CNC-COMM (SB)     TO DA-CNO (SA)
           END-IF
02372                                                                   ECS045
02373      IF EC-RCD-TYPE = LIFE-OVERRIDE-L1                            ECS045
02374          GO TO 0290-EC-LIFE.                                      ECS045
02375                                                                   ECS045
02376      IF EC-AGT-TYPE (SB) = 'R' OR 'D'                             ECS045
071207        OR ((EC-AGT-TYPE (SB) = 'K')
071207           AND (EC-CARRIER = '4'))
02377          IF CLAS-I-EP (CLAS-INDEXA) = 'R'                         ECS045
02378              COMPUTE DA-ISC (SA) = DA-ISC (SA) +                  ECS045
02379                     (EC-ISS-COMM (SB) - EC-COMM-78-ADJ (SB))      ECS045
02380          ELSE                                                     ECS045
02381              IF CLAS-I-EP (CLAS-INDEXA) = 'M'                     ECS045
02382                  COMPUTE DA-ISC (SA) = DA-ISC (SA) +              ECS045
02383                                        EC-ISS-COMM (SB) -         ECS045
02384                  ((EC-COMM-78-ADJ (SB) + EC-COMM-PR-ADJ (SB)) / 2)ECS045
02385              ELSE                                                 ECS045
02386                  COMPUTE DA-ISC (SA) = DA-ISC (SA) +              ECS045
02387                         (EC-ISS-COMM (SB) - EC-COMM-PR-ADJ (SB))  ECS045
02388      ELSE                                                         ECS045
02389          IF CLAS-I-EP (CLAS-INDEXA) = 'R'                         ECS045
02390              COMPUTE DA-ISO (SA) = DA-ISO (SA) +                  ECS045
02391                     (EC-ISS-COMM (SB) - EC-COMM-78-ADJ (SB))      ECS045
02392          ELSE                                                     ECS045
02393              IF CLAS-I-EP (CLAS-INDEXA) = 'M'                     ECS045
02394                  COMPUTE DA-ISO (SA) = DA-ISO (SA) +              ECS045
02395                                        EC-ISS-COMM (SB) -         ECS045
02396                  ((EC-COMM-78-ADJ (SB) + EC-COMM-PR-ADJ (SB)) / 2)ECS045
02397              ELSE                                                 ECS045
02398                  COMPUTE DA-ISO (SA) = DA-ISO (SA) +              ECS045
02399                         (EC-ISS-COMM (SB) - EC-COMM-PR-ADJ (SB)). ECS045
02400                                                                   ECS045
02401      IF EC-AGT-TYPE (SB) = 'R' OR 'D'                             ECS045
071207        OR ((EC-AGT-TYPE (SB) = 'K')
071207           AND (EC-CARRIER = '4'))
02402          COMPUTE DA-UCR (SA) = DA-UCR (SA) +                      ECS045
02403                  ((EC-ISS-COMM (SB) - EC-CNC-COMM (SB)) -         ECS045
02404                   (EC-COMM-78 (SB) + EC-COMM-78-ADJ (SB)))        ECS045
02405          COMPUTE DA-UCP (SA) = DA-UCP (SA) +                      ECS045
02406                  ((EC-ISS-COMM (SB) - EC-CNC-COMM (SB)) -         ECS045
02407                   (EC-COMM-PR (SB) + EC-COMM-PR-ADJ (SB)))        ECS045
02408      ELSE                                                         ECS045
02409          COMPUTE DA-UOR (SA) = DA-UOR (SA) +                      ECS045
02410                  ((EC-ISS-COMM (SB) - EC-CNC-COMM (SB)) -         ECS045
02411                   (EC-COMM-78 (SB) + EC-COMM-78-ADJ (SB)))        ECS045
02412          COMPUTE DA-UOP (SA) = DA-UOP (SA) +                      ECS045
02413                  ((EC-ISS-COMM (SB) - EC-CNC-COMM (SB)) -         ECS045
02414                   (EC-COMM-PR (SB) + EC-COMM-PR-ADJ (SB))).       ECS045
02415                                                                   ECS045
02416      GO TO 0280-EC-ADD-LOOP.                                      ECS045
02417                                                                   ECS045
02418  0290-EC-LIFE.                                                    ECS045
02419                                                                   ECS045
02420      IF EC-AGT-TYPE (SB) = 'R' OR 'D'                             ECS045
071207        OR ((EC-AGT-TYPE (SB) = 'K')
071207           AND (EC-CARRIER = '4'))
02421          IF CLAS-I-EP (CLAS-INDEXL) = 'R' OR 'N' OR 'T' OR        ECS045
02422                                       'A' OR 'B'                  ECS045
02423              COMPUTE DA-ISC (SA) = DA-ISC (SA) +                  ECS045
02424                   (EC-ISS-COMM (SB) - EC-COMM-78-ADJ (SB))        ECS045
02425          ELSE                                                     ECS045
02426              COMPUTE DA-ISC (SA) = DA-ISC (SA) +                  ECS045
02427                   (EC-ISS-COMM (SB) - EC-COMM-PR-ADJ (SB))        ECS045
02428      ELSE                                                         ECS045
02429          IF CLAS-I-EP (CLAS-INDEXL) = 'R' OR 'N' OR 'T' OR        ECS045
02430                                       'A' OR 'B'                  ECS045
02431              COMPUTE DA-ISO (SA) = DA-ISO (SA) +                  ECS045
02432                   (EC-ISS-COMM (SB) - EC-COMM-78-ADJ (SB))        ECS045
02433          ELSE                                                     ECS045
02434              COMPUTE DA-ISO (SA) = DA-ISO (SA) +                  ECS045
02435                   (EC-ISS-COMM (SB) - EC-COMM-PR-ADJ (SB)).       ECS045
02436                                                                   ECS045
02437      IF LIFE-PERCENTS-AVAILABLE                                   ECS045
02438          GO TO 0295-LIFE-PERCENTS-AVAILABLE.                      ECS045
02439                                                                   ECS045
02440      IF EC-AGT-TYPE (SB) = 'R' OR 'D'                             ECS045
071207        OR ((EC-AGT-TYPE (SB) = 'K')
071207           AND (EC-CARRIER = '4'))
02441          IF CLAS-I-EP (CLAS-INDEXL) = 'R' OR 'N' OR 'T' OR        ECS045
02442                                       'A' OR 'B'                  ECS045
02443              COMPUTE DA-UCR (SA) = DA-UCR (SA) +                  ECS045
02444                  ((EC-ISS-COMM (SB) - EC-CNC-COMM (SB)) -         ECS045
02445                   (EC-COMM-78 (SB) + EC-COMM-78-ADJ (SB)))        ECS045
02446          ELSE                                                     ECS045
02447              COMPUTE DA-UCP (SA) = DA-UCP (SA) +                  ECS045
02448                  ((EC-ISS-COMM (SB) - EC-CNC-COMM (SB)) -         ECS045
02449                   (EC-COMM-PR (SB) + EC-COMM-PR-ADJ (SB)))        ECS045
02450      ELSE                                                         ECS045
02451          IF CLAS-I-EP (CLAS-INDEXL) = 'R' OR 'N' OR 'T' OR        ECS045
02452                                       'A' OR 'B'                  ECS045
02453              COMPUTE DA-UOR (SA) = DA-UOR (SA) +                  ECS045
02454                  ((EC-ISS-COMM (SB) - EC-CNC-COMM (SB)) -         ECS045
02455                   (EC-COMM-78 (SB) + EC-COMM-78-ADJ (SB)))        ECS045
02456          ELSE                                                     ECS045
02457              COMPUTE DA-UOP (SA) = DA-UOP (SA) +                  ECS045
02458                  ((EC-ISS-COMM (SB) - EC-CNC-COMM (SB)) -         ECS045
02459                   (EC-COMM-PR (SB) + EC-COMM-PR-ADJ (SB))).       ECS045
02460                                                                   ECS045
02461      GO TO 0280-EC-ADD-LOOP.                                      ECS045
02462                                                                   ECS045
02463  0295-LIFE-PERCENTS-AVAILABLE.                                    ECS045
02464                                                                   ECS045
02465      IF EC-AGT-TYPE (SB) = 'R' OR 'D'                             ECS045
071207        OR ((EC-AGT-TYPE (SB) = 'K')
071207           AND (EC-CARRIER = '4'))
02466          COMPUTE DA-UCR (SA) = DA-UCR (SA) +                      ECS045
02467              ((EC-ISS-COMM (SB) - EC-CNC-COMM (SB)) -             ECS045
02468               (EC-COMM-78 (SB) + EC-COMM-78-ADJ (SB)))            ECS045
02469          COMPUTE DA-UCP (SA) = DA-UCP (SA) +                      ECS045
02470              ((EC-ISS-COMM (SB) - EC-CNC-COMM (SB)) -             ECS045
02471               (EC-COMM-PR (SB) + EC-COMM-PR-ADJ (SB)))            ECS045
02472      ELSE                                                         ECS045
02473          COMPUTE DA-UOR (SA) = DA-UOR (SA) +                      ECS045
02474              ((EC-ISS-COMM (SB) - EC-CNC-COMM (SB)) -             ECS045
02475               (EC-COMM-78 (SB) + EC-COMM-78-ADJ (SB)))            ECS045
02476          COMPUTE DA-UOP (SA) = DA-UOP (SA) +                      ECS045
02477              ((EC-ISS-COMM (SB) - EC-CNC-COMM (SB)) -             ECS045
02478               (EC-COMM-PR (SB) + EC-COMM-PR-ADJ (SB))).           ECS045
02479                                                                   ECS045
02480      GO TO 0280-EC-ADD-LOOP.                                      ECS045
02481                                                                   ECS045
02482  0300-EXIT.                                                       ECS045
02483      EXIT.                                                        ECS045
02484                                                                   ECS045
02485      EJECT                                                        ECS045
02486  0305-SEARCH-REIN-CO-TABLE.                                       ECS045
02487      MOVE 'N'                    TO LIFE-PROCESS-SWITCH.          ECS045
02488      MOVE +1                     TO RX.                           ECS045
02489                                                                   ECS045
02490  0306-SEARCH-TABLE-LOOP.                                          ECS045
02491      IF RX = RCT-ENTRY-COUNT                                      ECS045
02492          GO TO 0307-REIN-CO-MATCH.                                ECS045
02493                                                                   ECS045
02494      IF SAVE-E-C-REI-COMP = RCT-REIN-CO (RX)                      ECS045
02495          GO TO 0307-REIN-CO-MATCH.                                ECS045
02496                                                                   ECS045
02497      ADD +1                      TO RX.                           ECS045
02498      GO TO 0306-SEARCH-TABLE-LOOP.                                ECS045
02499                                                                   ECS045
02500  0307-REIN-CO-MATCH.                                              ECS045
02501      IF RCT-LF-PR-PCT (RX) GREATER THAN ZERO OR                   ECS045
02502         RCT-LF-78-PCT (RX) GREATER THAN ZERO                      ECS045
02503             MOVE 'Y'             TO LIFE-PROCESS-SWITCH.          ECS045
02504                                                                   ECS045
02505  0309-EXIT.                                                       ECS045
02506      EXIT.                                                        ECS045
02507                                                                   ECS045
02508      EJECT                                                        ECS045
02509  0310-MATCH-FILES.                                                ECS045
02510      MOVE SAVE-E-C-CNTRL-1       TO RR-ACCTMSTR-CNTRL.            ECS045
02511      MOVE SAVE-E-C-EXP-DATE      TO RR-ACCTMSTR-EXP-DT.           ECS045
02512                                                                   ECS045
02513      IF AM-MSTR-CNTRL LESS THAN RR-ACCTMSTR-CNTRL                 ECS045
02514          PERFORM 0330-READ-ACC-MSTR THRU 0340-EXIT                ECS045
02515          GO TO 0310-MATCH-FILES.                                  ECS045
02516                                                                   ECS045
02517      IF AM-MSTR-CNTRL GREATER THAN RR-ACCTMSTR-CNTRL              ECS045
02518          DISPLAY 'NO ACCOUNT MASTER FOR THIS CONTROL '            ECS045
02519          DISPLAY RR-ACCTMSTR-CNTRL  (1:19) ' '
032612           rr-acctmstr-exp-dt
032612         ' ' am-control-a ' ' am-expire-dt
02520          GO TO 0320-EXIT.                                         ECS045
02521                                                                   ECS045
02522 ***********************************************                   ECS045
02523 *  INITIALIZE REINS DATA FROM ACCOUNT MASTER  *                   ECS045
02524 ***********************************************                   ECS045
02525                                                                   ECS045
PEMMOD*    IF AM-REI-LF-TAX NOT NUMERIC                                 ECS045
PEMMOD*        MOVE ZEROS              TO AM-REI-LF-TAX.                ECS045
PEMMOD*    IF AM-REI-AH-TAX NOT NUMERIC                                 ECS045
PEMMOD*        MOVE ZEROS              TO AM-REI-AH-TAX.                ECS045
02530      IF AM-REI-FEE-LF NOT NUMERIC                                 ECS045
02531          MOVE ZEROS              TO AM-REI-FEE-LF.                ECS045
02532      IF AM-REI-FEE-AH NOT NUMERIC                                 ECS045
02533          MOVE ZEROS              TO AM-REI-FEE-AH.                ECS045
02534      IF AM-REI-PR-PCT NOT NUMERIC                                 ECS045
02535          MOVE ZEROS              TO AM-REI-PR-PCT.                ECS045
02536      IF AM-REI-78-PCT NOT NUMERIC                                 ECS045
02537          MOVE ZEROS              TO AM-REI-78-PCT.                ECS045
02538                                                                   ECS045
02539 *********************************************                     ECS045
02540 *  RETRIEVE REINS DATA FROM ACCOUNT MASTER  *                     ECS045
02541 *********************************************                     ECS045
02542                                                                   ECS045
02543      MOVE AM-REI-GROUP-A         TO RR-A-REI-GRP-A.               ECS045
02544      MOVE AM-REI-GROUP-B         TO RR-B-REI-GRP-B.               ECS045
02545                                                                   ECS045
02546      MOVE AM-RPT045A-SWITCH      TO RR-045A-SW.                   ECS045
02547                                                                   ECS045
02548      IF DTE-CLIENT = 'LAP'                                        ECS045
02549         MOVE AM-STATE            TO RR-A-REI-GRP-A.               ECS045
02550                                                                   ECS045
02551      MOVE AM-NAME                TO RR-ACC-NAME.                  ECS045
02552      MOVE AM-REI-LF-TAX          TO RR-LF-TAX.                    ECS045
02553      MOVE AM-REI-AH-TAX          TO RR-AH-TAX.                    ECS045
02554                                                                   ECS045
02555      IF RR-PE-LF = SPACES                                         ECS045
02556          MOVE AM-REI-PE-LF       TO RR-PE-LF.                     ECS045
02557      IF RR-PE-AH = SPACES                                         ECS045
02558          MOVE AM-REI-PE-AH       TO RR-PE-AH.                     ECS045
02559      IF RR-PRT-ST = SPACES                                        ECS045
02560          MOVE AM-REI-PRT-ST      TO RR-PRT-ST.                    ECS045
02561                                                                   ECS045
02562      IF DTE-CLIENT = ('LAP' OR 'GLI')  AND                        ECS045
02563         RR-PRT-ST  = 'N'                                          ECS045
02564          MOVE ZEROS              TO RR-LF-TAX RR-AH-TAX.          ECS045
02565                                                                   ECS045
02566      IF RR-PRT-OW = SPACES                                        ECS045
02567          MOVE AM-REI-PRT-OW      TO RR-PRT-OW.                    ECS045
CIDMOD                                                                  ECS045
CIDMOD     IF RR-MORT = LOW-VALUES                                      ECS045
CIDMOD         MOVE SPACES             TO RR-MORT.                      ECS045
02568                                                                   ECS045
02569      IF RR-MORT = SPACES                                          ECS045
02570          MOVE AM-REI-MORT        TO RR-MORT.                      ECS045
02571                                                                   ECS045
02572      IF RR-AH-PR-PCT = ZEROS  AND  RR-AH-78-PCT = ZEROS           ECS045
02573          MOVE AM-REI-PR-PCT      TO RR-AH-PR-PCT                  ECS045
02574          MOVE AM-REI-78-PCT      TO RR-AH-78-PCT.                 ECS045
02575                                                                   ECS045
02576      IF RR-FEE-LF = ZEROS                                         ECS045
02577          MOVE AM-REI-FEE-LF      TO RR-FEE-LF.                    ECS045
02578      IF RR-FEE-AH = ZEROS                                         ECS045
02579          MOVE AM-REI-FEE-AH      TO RR-FEE-AH.                    ECS045
02580                                                                   ECS045
02581      IF DTE-CLIENT = 'HER'                                        ECS045
02582          IF RR-REI-PRIME = 'LNL'                                  ECS045
02583              MOVE ZEROS          TO RR-FEE-LF.                    ECS045
02584                                                                   ECS045
02585 *  DEFAULT VALUES FOR REINS DATA NOT IN TABLES OR IN ACCT MASTER. ECS045
02586                                                                   ECS045
02587      IF RR-PE-LF = SPACE                                          ECS045
02588          MOVE 'P'                TO RR-PE-LF.                     ECS045
02589      IF RR-PE-AH = SPACE                                          ECS045
02590          MOVE 'E'                TO RR-PE-AH.                     ECS045
02591      IF RR-PRT-ST = SPACE                                         ECS045
02592          MOVE 'Y'                TO RR-PRT-ST.                    ECS045
02593      IF RR-PRT-OW = SPACE                                         ECS045
02594          MOVE 'Y'                TO RR-PRT-OW.                    ECS045
02595      IF DTE-CLIENT = 'NCL'                                        ECS045
02596        IF RR-PRT-CRSV = SPACE                                     ECS045
02597          MOVE 'Y'                TO RR-PRT-CRSV.                  ECS045
02598      IF RR-0-PE-FEE-LF = SPACE  OR  'N'                           ECS045
02599          MOVE RR-PE-LF           TO RR-0-PE-FEE-LF.               ECS045
02600      IF RR-0-PE-FEE-AH = SPACE  OR  'N'                           ECS045
02601          MOVE RR-PE-AH           TO RR-0-PE-FEE-AH.               ECS045
02602      IF RR-PE-COMM-LF = SPACE                                     ECS045
02603          MOVE RR-PE-LF           TO RR-PE-COMM-LF.                ECS045
02604      IF RR-PE-COMM-AH = SPACE                                     ECS045
02605          MOVE RR-PE-AH           TO RR-PE-COMM-AH.                ECS045
02606      IF RR-PE-TAX-LF = SPACE                                      ECS045
02607          MOVE RR-PE-LF           TO RR-PE-TAX-LF.                 ECS045
02608      IF RR-PE-TAX-AH = SPACE                                      ECS045
02609          MOVE RR-PE-AH           TO RR-PE-TAX-AH.                 ECS045
02610      IF RR-CLAIM-CODE = ' '                                       ECS045
02611          MOVE 'P'                TO RR-CLAIM-CODE  RR-PI-CLM-LF   ECS045
02612                                     RR-PI-CLM-AH.                 ECS045
02613                                                                   ECS045
02614  0320-EXIT.                                                       ECS045
02615      EXIT.                                                        ECS045
02616                                                                   ECS045
02617  0330-READ-ACC-MSTR.                                              ECS045
02618      READ ACC-MSTR AT END                                         ECS045
02619          MOVE HIGH-VALUES        TO ACCOUNT-MASTER.               ECS045
02620                                                                   ECS045
02621  0340-EXIT.                                                       ECS045
02622      EXIT.                                                        ECS045
02623                                                                   ECS045
02624  0370-END-SELECT-EPEC-RECS.                                       ECS045
02625      CLOSE EPEC-FILE                                              ECS045
02626            ACC-MSTR.                                              ECS045
02627                                                                   ECS045
02628  0380-SER-EXIT.                                                   ECS045
02629      EXIT.                                                        ECS045
02630                                                                   ECS045
02631      EJECT                                                        ECS045
02632 ******************************************************            ECS045
02633 *  (0390) ONE TIME ROUTINE FOR OUTPUT PROCEDURE (1)  *            ECS045
02634 ******************************************************            ECS045
02635                                                                   ECS045
02636  0390-PRINT-STATEMENT SECTION.                                    ECS045
02637                                                                   ECS045
02638      IF EPECS-SELECTED = ZEROS                                    ECS045
02639          GO TO 0990-END-PRINT-STATEMENT.                          ECS045
02640                                                                   ECS045
02641      PERFORM 0440-ZERO-PAA THRU 0450-EXIT VARYING SA              ECS045
02642              FROM +1 BY +1  UNTIL SA GREATER +81.                 ECS045
02643      PERFORM 0460-ZERO-PAB THRU 0470-EXIT VARYING SA              ECS045
02644              FROM +1 BY +1  UNTIL SA GREATER +54.                 ECS045
02645                                                                   ECS045
02646      MOVE RCT-ENTRY-COUNT        TO RX.                           ECS045
02647                                                                   ECS045
02648      PERFORM 0940-RETURN-SW-FILE THRU 0950-EXIT.                  ECS045
02649                                                                   ECS045
02650      MOVE RR-REPORT-ID           TO H1-RUN-CODE.                  ECS045
02651                                                                   ECS045
02652      IF RR-REPORT-ID-B                                            ECS045
02653          GO TO BBBB-ACCUM-DETAIL.                                 ECS045
02654                                                                   ECS045
02655      IF RR-REPORT-ID-D                                            ECS045
02656          GO TO DDDD-ACCUM-DETAIL.                                 ECS045
02657                                                                   ECS045
02658      IF RR-REPORT-ID-E                                            ECS045
02659          GO TO EEEE-ACCUM-DETAIL.                                 ECS045
02660                                                                   ECS045
02661      GO TO 0410-ACCUM-DETAIL.                                     ECS045
02662                                                                   ECS045
02663      EJECT                                                        ECS045
02664 **********************************************************        ECS045
02665 *  0400 ROUTINE IS THE START OF THE OUTPUT PROCEDURE (1) *        ECS045
02666 *  MAINLINE LOOP.  REPORT(45-A)                          *        ECS045
02667 **********************************************************        ECS045
02668                                                                   ECS045
02669  0400-CHECK-CONTROL-BREAK.                                        ECS045
02670 ****  CHECK FOR REINS COMPANY PRIME BREAK  ****                   ECS045
02671                                                                   ECS045
02672      IF (R-B-REI-COMP-PRIME NOT = SAVE-R-B-REI-COMP-PRIME) OR     ECS045
02673         (NOT RR-REPORT-ID-A)                                      ECS045
02674          PERFORM 0640-PRT-DATE-RANGE          THRU 0660-EXIT      ECS045
02675          PERFORM 0670-PRT-REINS-SUB           THRU 0690-EXIT      ECS045
02676          PERFORM 0700-PRT-ACCT                THRU 0720-EXIT      ECS045
02677          PERFORM 0730-PRT-ST                  THRU 0750-EXIT      ECS045
02678          PERFORM 0760-PRT-CERT-GROUP          THRU 0780-EXIT      ECS045
02679          PERFORM 0790-PRT-CARR                THRU 0800-EXIT      ECS045
02680          PERFORM 0810-PRT-REINS-GROUP-A       THRU 0820-EXIT      ECS045
02681          PERFORM 0830-PRT-REINS-COMPANY-PRIME THRU 0840-EXIT      ECS045
02682          GO TO 0410-ACCUM-DETAIL.                                 ECS045
02683                                                                   ECS045
02684 ****  CHECK FOR REINS COMPANY GROUP-A BREAK  ****                 ECS045
02685                                                                   ECS045
02686      IF R-B-GROUP NOT = SAVE-R-B-GROUP                            ECS045
02687          PERFORM 0640-PRT-DATE-RANGE    THRU 0660-EXIT            ECS045
02688          PERFORM 0670-PRT-REINS-SUB     THRU 0690-EXIT            ECS045
02689          PERFORM 0700-PRT-ACCT          THRU 0720-EXIT            ECS045
02690          PERFORM 0730-PRT-ST            THRU 0750-EXIT            ECS045
02691          PERFORM 0760-PRT-CERT-GROUP    THRU 0780-EXIT            ECS045
02692          PERFORM 0790-PRT-CARR          THRU 0800-EXIT            ECS045
02693          PERFORM 0810-PRT-REINS-GROUP-A THRU 0820-EXIT            ECS045
02694          GO TO 0410-ACCUM-DETAIL.                                 ECS045
02695                                                                   ECS045
02696 ****  CHECK FOR CARRIER BREAK  ****                               ECS045
02697                                                                   ECS045
02698      IF R-B-CARR NOT = SAVE-R-B-CARR                              ECS045
02699          PERFORM 0640-PRT-DATE-RANGE THRU 0660-EXIT               ECS045
02700          PERFORM 0670-PRT-REINS-SUB  THRU 0690-EXIT               ECS045
02701          PERFORM 0700-PRT-ACCT       THRU 0720-EXIT               ECS045
02702          PERFORM 0730-PRT-ST         THRU 0750-EXIT               ECS045
02703          PERFORM 0760-PRT-CERT-GROUP THRU 0780-EXIT               ECS045
02704          PERFORM 0790-PRT-CARR       THRU 0800-EXIT               ECS045
02705          GO TO 0410-ACCUM-DETAIL.                                 ECS045
02706                                                                   ECS045
02707 ****  CHECK FOR CERT GROUP BREAK  ****                            ECS045
02708                                                                   ECS045
02709      IF R-B-COMP NOT = SAVE-R-B-COMP                              ECS045
02710          PERFORM 0640-PRT-DATE-RANGE THRU 0660-EXIT               ECS045
02711          PERFORM 0670-PRT-REINS-SUB  THRU 0690-EXIT               ECS045
02712          PERFORM 0700-PRT-ACCT       THRU 0720-EXIT               ECS045
02713          PERFORM 0730-PRT-ST         THRU 0750-EXIT               ECS045
02714          PERFORM 0760-PRT-CERT-GROUP THRU 0780-EXIT               ECS045
02715          GO TO 0410-ACCUM-DETAIL.                                 ECS045
02716                                                                   ECS045
02717 ****  CHECK FOR STATE BREAK  ****                                 ECS045
02718                                                                   ECS045
02719      IF R-B-STATE NOT = SAVE-R-B-STATE                            ECS045
02720          PERFORM 0640-PRT-DATE-RANGE THRU 0660-EXIT               ECS045
02721          PERFORM 0670-PRT-REINS-SUB  THRU 0690-EXIT               ECS045
02722          PERFORM 0700-PRT-ACCT       THRU 0720-EXIT               ECS045
02723          PERFORM 0730-PRT-ST         THRU 0750-EXIT               ECS045
02724          GO TO 0410-ACCUM-DETAIL.                                 ECS045
02725                                                                   ECS045
02726 ****  CHECK FOR ACCOUNT BREAK  ****                               ECS045
02727                                                                   ECS045
02728      IF R-B-ACCT NOT = SAVE-R-B-ACCT                              ECS045
02729          PERFORM 0640-PRT-DATE-RANGE THRU 0660-EXIT               ECS045
02730          PERFORM 0670-PRT-REINS-SUB  THRU 0690-EXIT               ECS045
02731          PERFORM 0700-PRT-ACCT       THRU 0720-EXIT               ECS045
02732          GO TO 0410-ACCUM-DETAIL.                                 ECS045
02733                                                                   ECS045
02734 ****  CHECK FOR REINS COMPANY SUB BREAK  ****                     ECS045
02735                                                                   ECS045
02736      IF R-B-REI-COMP-SUB NOT = SAVE-R-B-REI-COMP-SUB              ECS045
02737          PERFORM 0640-PRT-DATE-RANGE THRU 0660-EXIT               ECS045
02738          PERFORM 0670-PRT-REINS-SUB  THRU 0690-EXIT               ECS045
02739          GO TO 0410-ACCUM-DETAIL.                                 ECS045
02740                                                                   ECS045
02741 ****  CHECK FOR ACCOUNT MASTER DATE RANGE BREAK  ****             ECS045
02742                                                                   ECS045
02743      IF R-B-EXP NOT = SAVE-R-B-EXP  OR                            ECS045
02744         R-B-EFF NOT = SAVE-R-B-EFF                                ECS045
02745          PERFORM 0640-PRT-DATE-RANGE THRU 0660-EXIT.              ECS045
02746                                                                   ECS045
02747      EJECT                                                        ECS045
02748  0410-ACCUM-DETAIL.                                               ECS045
02749      IF R-B-REI-COMP = HIGH-VALUES                                ECS045
02750          MOVE 'Y'                TO FINAL-TOT                     ECS045
02751          PERFORM 0850-FINAL-TOTALS THRU 0855-EXIT                 ECS045
02752          MOVE 'N'                TO FINAL-TOT                     ECS045
02753          GO TO 0990-END-PRINT-STATEMENT.                          ECS045
02754                                                                   ECS045
02755      IF NOT RR-REPORT-ID-A                                        ECS045
02756          MOVE 'Y'                TO FINAL-TOT                     ECS045
02757          PERFORM 0850-FINAL-TOTALS THRU 0855-EXIT                 ECS045
02758          MOVE 'N'                TO FINAL-TOT                     ECS045
02759          PERFORM 0440-ZERO-PAA THRU 0450-EXIT VARYING SA          ECS045
02760              FROM +1 BY +1      UNTIL SA GREATER +81              ECS045
02761          PERFORM 0460-ZERO-PAB THRU 0470-EXIT VARYING SA          ECS045
02762              FROM +1 BY +1      UNTIL SA GREATER +54              ECS045
02763          MOVE RR-REPORT-ID       TO H1-RUN-CODE                   ECS045
02764          MOVE +0                 TO PAGE-CNT                      ECS045
02765          GO TO BBBB-ACCUM-DETAIL.                                 ECS045
02766                                                                   ECS045
02767      MOVE RR-CONTROL-B           TO SAVE-RR-CONTROL-B.            ECS045
02768      MOVE RR-ACC-NAME            TO SAVE-ACC-NAME.                ECS045
02769      MOVE RR-C-REI-NAME          TO SAVE-REI-NAME.                ECS045
02770      MOVE RR-C-CEDE-NAME         TO SAVE-CEDE-NAME.               ECS045
02771      MOVE RR-CLAIM-CODE          TO SAVE-CLAIM-CODE.              ECS045
02772      MOVE RR-PRT-ST              TO SAVE-PRT-ST.                  ECS045
02773      MOVE RR-PRT-OW              TO SAVE-PRT-OW.                  ECS045
02774      MOVE RR-PRT-CRSV            TO SAVE-PRT-CRSV.                ECS045
02775      MOVE RR-ACCUMS              TO DETAIL-ACCUMULATORS.          ECS045
020612     MOVE RR-PE-LF               TO SAVE-LF-PE.
020612     MOVE RR-PE-AH               TO SAVE-AH-PE.
CIDMOD                                                                  ECS045
CIDMOD     MOVE SPACES TO RR-MORT.                                      ECS045
02776                                                                   ECS045
02777      IF RR-MORT = SPACES                                          ECS045
02778          MOVE WS-MORT-STD        TO DT-ENTRY (21)                 ECS045
02779                                     DT-ENTRY-2 (21)               ECS045
02780          GO TO 0430-ACCUM-LEVEL-1-DETAIL.                         ECS045
02781                                                                   ECS045
02782      MOVE CLAS-STARTM            TO CLAS-INDEXM.                  ECS045
02783                                                                   ECS045
02784 *****************************                                     ECS045
02785 *  VALIDATE MORTALITY CODE  *                                     ECS045
02786 *****************************                                     ECS045
02787                                                                   ECS045
02788  0420-FIND-MORT-LOOP.                                             ECS045
02789      IF CLAS-INDEXM GREATER CLAS-MAXM OR                          ECS045
02790         CLAS-INDEXM = +0                                          ECS045
02791          DISPLAY 'INVALID MORTALITY TABLE CODE (' RR-MORT ')'     ECS045
CIDMOD         DISPLAY 'CLAS-INDEXM  = ' CLAS-INDEXM                    ECS045
CIDMOD         DISPLAY 'CLAS-MAXM    = ' CLAS-MAXM                      ECS045
02792          MOVE '**** INVALID MORTALITY TABLE CODE ****'            ECS045
02793                                  TO WS-ABEND-MESSAGE              ECS045
02794          MOVE '0'                TO WAC-1                         ECS045
02795          MOVE '3'                TO WAC-2                         ECS045
02796          MOVE '01'               TO WAC-3-4                       ECS045
02797          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS045
02798          GO TO ABEND-PGM.                                         ECS045
02799                                                                   ECS045
02800      IF RR-MORT NOT = CLAS-MORT-CODE (CLAS-INDEXM)                ECS045
02801          ADD +1                  TO CLAS-INDEXM                   ECS045
02802          GO TO 0420-FIND-MORT-LOOP.                               ECS045
02803                                                                   ECS045
02804      MOVE CLAS-MORT-DESC (CLAS-INDEXM) TO WS-MORT-DSC.            ECS045
02805      MOVE WS-MORT-DESC                 TO DT-ENTRY (21)           ECS045
02806                                           DT-ENTRY-2 (21).        ECS045
02807                                                                   ECS045
02808  0430-ACCUM-LEVEL-1-DETAIL.                                       ECS045
02809      PERFORM 0480-CALCULATE-LEVEL-1 THRU 0490-EXIT.               ECS045
02810                                                                   ECS045
02811  0430-ACCUM-EXIT.                                                 ECS045
02812      PERFORM 0940-RETURN-SW-FILE THRU 0950-EXIT.                  ECS045
02813                                                                   ECS045
02814      GO TO 0400-CHECK-CONTROL-BREAK.                              ECS045
02815                                                                   ECS045
02816 **********************************************************        ECS045
02817 *  END OUTPUT PROCEDURE(1) MAINLINE LOOP  REPORT(45-A)   *        ECS045
02818 **********************************************************        ECS045
02819      EJECT                                                        ECS045
02820 **********************************************************        ECS045
02821 *  BBBB ROUTINE IS THE START OF THE OUTPUT PROCEDURE (1) *        ECS045
02822 *  MAINLINE LOOP.  REPORT(45-B)                          *        ECS045
02823 **********************************************************        ECS045
02824                                                                   ECS045
02825  BBBB-CHECK-CONTROL-BREAK.                                        ECS045
02826 ****  CHECK FOR REINS SUB BREAK  ****                             ECS045
02827                                                                   ECS045
02828      IF (R-B-CARR-B NOT = SAVE-R-B-CARR-B) OR                     ECS045
02829         (NOT RR-REPORT-ID-B)                                      ECS045
02830          PERFORM BBBB-PRT-REINS-SUB THRU BBBB-PRT-REINS-SUB-X     ECS045
02831          PERFORM BBBB-PRT-ST        THRU BBBB-PRT-ST-X            ECS045
02832          PERFORM BBBB-PRT-REINS-COMPANY-PRIME THRU                ECS045
02833                                 BBBB-PRT-REINS-COMPANY-PRIME-X    ECS045
02834          PERFORM BBBB-PRT-REINS-GROUP-B THRU                      ECS045
02835                                 BBBB-PRT-REINS-GROUP-B-X          ECS045
02836          PERFORM BBBB-PRT-CARR      THRU BBBB-PRT-CARR-X          ECS045
02837          GO TO BBBB-ACCUM-DETAIL.                                 ECS045
02838                                                                   ECS045
02839 ****  CHECK FOR REINSURANCE GROUP-B BREAK  ****                   ECS045
02840                                                                   ECS045
02841      IF R-B-GROUP-B NOT = SAVE-R-B-GROUP-B                        ECS045
02842          PERFORM BBBB-PRT-REINS-SUB THRU BBBB-PRT-REINS-SUB-X     ECS045
02843          PERFORM BBBB-PRT-ST        THRU BBBB-PRT-ST-X            ECS045
02844          PERFORM BBBB-PRT-REINS-COMPANY-PRIME THRU                ECS045
02845                                 BBBB-PRT-REINS-COMPANY-PRIME-X    ECS045
02846          PERFORM BBBB-PRT-REINS-GROUP-B THRU                      ECS045
02847                                 BBBB-PRT-REINS-GROUP-B-X          ECS045
02848          GO TO BBBB-ACCUM-DETAIL.                                 ECS045
02849                                                                   ECS045
02850 ****  CHECK FOR REINSURANCE COMPANY PRIME BREAK  ****             ECS045
02851                                                                   ECS045
02852      IF R-B-REI-COMP-PRIME-B NOT = SAVE-R-B-REI-COMP-PRIME-B      ECS045
02853          PERFORM BBBB-PRT-REINS-SUB THRU BBBB-PRT-REINS-SUB-X     ECS045
02854          PERFORM BBBB-PRT-ST        THRU BBBB-PRT-ST-X            ECS045
02855          PERFORM BBBB-PRT-REINS-COMPANY-PRIME THRU                ECS045
02856                                 BBBB-PRT-REINS-COMPANY-PRIME-X    ECS045
02857          GO TO BBBB-ACCUM-DETAIL.                                 ECS045
02858                                                                   ECS045
02859 ****  CHECK FOR STATE BREAK  ****                                 ECS045
02860                                                                   ECS045
02861      IF R-B-STATE-B NOT = SAVE-R-B-STATE-B                        ECS045
02862          PERFORM BBBB-PRT-REINS-SUB THRU BBBB-PRT-REINS-SUB-X     ECS045
02863          PERFORM BBBB-PRT-ST        THRU BBBB-PRT-ST-X            ECS045
02864          GO TO BBBB-ACCUM-DETAIL.                                 ECS045
02865                                                                   ECS045
02866 ****  CHECK FOR REINS COMPANY SUB BREAK  ****                     ECS045
02867                                                                   ECS045
02868      IF R-B-REI-COMP-SUB-B NOT = SAVE-R-B-REI-COMP-SUB-B          ECS045
02869          PERFORM BBBB-PRT-REINS-SUB THRU BBBB-PRT-REINS-SUB-X.    ECS045
02870                                                                   ECS045
02871      EJECT                                                        ECS045
02872  BBBB-ACCUM-DETAIL.                                               ECS045
02873      IF R-B-CARR-B = HIGH-VALUES                                  ECS045
02874          MOVE 'Y'                TO FINAL-TOT                     ECS045
02875          PERFORM BBBB-FINAL-TOTALS THRU BBBB-FINAL-TOTALS-X       ECS045
02876          MOVE 'N'                TO FINAL-TOT                     ECS045
02877          GO TO 0990-END-PRINT-STATEMENT.                          ECS045
02878                                                                   ECS045
02879      IF NOT RR-REPORT-ID-B                                        ECS045
02880          MOVE 'Y'                TO FINAL-TOT                     ECS045
02881          PERFORM BBBB-FINAL-TOTALS THRU BBBB-FINAL-TOTALS-X       ECS045
02882          MOVE 'N'                TO FINAL-TOT                     ECS045
02883          PERFORM 0440-ZERO-PAA THRU 0450-EXIT VARYING SA          ECS045
02884              FROM +1 BY +1      UNTIL SA GREATER +81              ECS045
02885          PERFORM 0460-ZERO-PAB THRU 0470-EXIT VARYING SA          ECS045
02886              FROM +1 BY +1      UNTIL SA GREATER +54              ECS045
02887          MOVE RR-REPORT-ID       TO H1-RUN-CODE                   ECS045
02888          MOVE +0                 TO PAGE-CNT                      ECS045
02889          GO TO DDDD-ACCUM-DETAIL.                                 ECS045
02890                                                                   ECS045
02891      MOVE RR-CONTROL-B-B         TO SAVE-RR-CONTROL-B-B.          ECS045
02892      MOVE RR-ACC-NAME            TO SAVE-ACC-NAME.                ECS045
02893      MOVE RR-C-REI-NAME          TO SAVE-REI-NAME.                ECS045
02894      MOVE RR-C-CEDE-NAME         TO SAVE-CEDE-NAME.               ECS045
02895      MOVE RR-CLAIM-CODE          TO SAVE-CLAIM-CODE.              ECS045
02896      MOVE RR-PRT-ST              TO SAVE-PRT-ST.                  ECS045
02897      MOVE RR-PRT-OW              TO SAVE-PRT-OW.                  ECS045
02898      MOVE RR-PRT-CRSV            TO SAVE-PRT-CRSV.                ECS045
02899      MOVE RR-ACCUMS              TO DETAIL-ACCUMULATORS.          ECS045
020612     MOVE RR-PE-LF               TO SAVE-LF-PE.
020612     MOVE RR-PE-AH               TO SAVE-AH-PE.
CIDMOD                                                                  ECS045
CIDMOD     IF RR-MORT = LOW-VALUES                                      ECS045
CIDMOD         MOVE SPACES             TO RR-MORT.                      ECS045
02900                                                                   ECS045
02901      IF RR-MORT = SPACES                                          ECS045
02902          MOVE WS-MORT-STD        TO DT-ENTRY (21)                 ECS045
02903                                     DT-ENTRY-2 (21)               ECS045
02904          GO TO BBBB-ACCUM-LEVEL-1-DETAIL.                         ECS045
02905                                                                   ECS045
02906      MOVE CLAS-STARTM            TO CLAS-INDEXM.                  ECS045
02907                                                                   ECS045
02908 *****************************                                     ECS045
02909 *  VALIDATE MORTALITY CODE  *                                     ECS045
02910 *****************************                                     ECS045
02911                                                                   ECS045
02912  BBBB-FIND-MORT-LOOP.                                             ECS045
02913      IF CLAS-INDEXM GREATER CLAS-MAXM OR                          ECS045
02914         CLAS-INDEXM = +0                                          ECS045
02915          DISPLAY 'INVALID MORTALITY TABLE CODE (' RR-MORT ')'     ECS045
02916          MOVE '**** INVALID MORTALITY TABLE CODE ****'            ECS045
02917                                  TO WS-ABEND-MESSAGE              ECS045
02918          MOVE '0'                TO WAC-1                         ECS045
02919          MOVE '3'                TO WAC-2                         ECS045
02920          MOVE '01'               TO WAC-3-4                       ECS045
02921          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS045
02922          GO TO ABEND-PGM.                                         ECS045
02923                                                                   ECS045
02924      IF RR-MORT NOT = CLAS-MORT-CODE (CLAS-INDEXM)                ECS045
02925          ADD +1                  TO CLAS-INDEXM                   ECS045
02926          GO TO BBBB-FIND-MORT-LOOP.                               ECS045
02927                                                                   ECS045
02928      MOVE CLAS-MORT-DESC (CLAS-INDEXM) TO WS-MORT-DSC.            ECS045
02929      MOVE WS-MORT-DESC                 TO DT-ENTRY (21)           ECS045
02930                                           DT-ENTRY-2 (21).        ECS045
02931                                                                   ECS045
02932  BBBB-ACCUM-LEVEL-1-DETAIL.                                       ECS045
02933      PERFORM 0480-CALCULATE-LEVEL-1 THRU 0490-EXIT.               ECS045
02934                                                                   ECS045
02935  BBBB-ACCUM-EXIT.                                                 ECS045
02936      PERFORM 0940-RETURN-SW-FILE THRU 0950-EXIT.                  ECS045
02937                                                                   ECS045
02938      GO TO BBBB-CHECK-CONTROL-BREAK.                              ECS045
02939                                                                   ECS045
02940 **********************************************************        ECS045
02941 *  END OUTPUT PROCEDURE(1) MAINLINE LOOP  REPORT(45-B)   *        ECS045
02942 **********************************************************        ECS045
02943                                                                   ECS045
02944      EJECT                                                        ECS045
02945 **********************************************************        ECS045
02946 *  DDDD ROUTINE IS THE START OF THE OUTPUT PROCEDURE (1) *        ECS045
02947 *  MAINLINE LOOP.  REPORT(45-D)                          *        ECS045
02948 **********************************************************        ECS045
02949                                                                   ECS045
02950  DDDD-CHECK-CONTROL-BREAK.                                        ECS045
02951 ****  CHECK FOR REINSURANCE GROUP BREAK  ****                     ECS045
02952                                                                   ECS045
02953      IF (R-B-GROUP-D NOT = SAVE-R-B-GROUP-D) OR                   ECS045
02954         (NOT RR-REPORT-ID-D)                                      ECS045
02955          PERFORM DDDD-PRT-REINS-COMPANY-PRIME THRU                ECS045
02956                                 DDDD-PRT-REINS-COMPANY-PRIME-X    ECS045
02957          PERFORM DDDD-PRT-CARR          THRU DDDD-PRT-CARR-X      ECS045
02958          PERFORM DDDD-PRT-REINS-GROUP-D       THRU                ECS045
02959                                 DDDD-PRT-REINS-GROUP-D-X          ECS045
02960          GO TO DDDD-ACCUM-DETAIL.                                 ECS045
02961                                                                   ECS045
02962 ****  CHECK FOR CARRIER BREAK  ****                               ECS045
02963                                                                   ECS045
02964      IF R-B-CARR-D NOT = SAVE-R-B-CARR-D                          ECS045
02965          PERFORM DDDD-PRT-REINS-COMPANY-PRIME THRU                ECS045
02966                                 DDDD-PRT-REINS-COMPANY-PRIME-X    ECS045
02967          PERFORM DDDD-PRT-CARR          THRU DDDD-PRT-CARR-X      ECS045
02968          GO TO DDDD-ACCUM-DETAIL.                                 ECS045
02969                                                                   ECS045
02970 ****  CHECK FOR REINSURANCE COMPANY PRIME BREAK  ****             ECS045
02971                                                                   ECS045
02972      IF R-B-REI-COMP-PRIME-D NOT = SAVE-R-B-REI-COMP-PRIME-D      ECS045
02973          PERFORM DDDD-PRT-REINS-COMPANY-PRIME THRU                ECS045
02974                                 DDDD-PRT-REINS-COMPANY-PRIME-X.   ECS045
02975                                                                   ECS045
02976      EJECT                                                        ECS045
02977  DDDD-ACCUM-DETAIL.                                               ECS045
02978      IF R-B-GROUP-D = HIGH-VALUES                                 ECS045
02979          MOVE 'Y'                TO FINAL-TOT                     ECS045
02980          PERFORM DDDD-FINAL-TOTALS THRU DDDD-FINAL-TOTALS-X       ECS045
02981          MOVE 'N'                TO FINAL-TOT                     ECS045
02982          GO TO 0990-END-PRINT-STATEMENT.                          ECS045
02983                                                                   ECS045
02984      IF NOT RR-REPORT-ID-D                                        ECS045
02985          MOVE 'Y'                TO FINAL-TOT                     ECS045
02986          PERFORM DDDD-FINAL-TOTALS THRU DDDD-FINAL-TOTALS-X       ECS045
02987          MOVE 'N'                TO FINAL-TOT                     ECS045
02988          PERFORM 0440-ZERO-PAA THRU 0450-EXIT VARYING SA          ECS045
02989              FROM +1 BY +1      UNTIL SA GREATER +81              ECS045
02990          PERFORM 0460-ZERO-PAB THRU 0470-EXIT VARYING SA          ECS045
02991              FROM +1 BY +1      UNTIL SA GREATER +54              ECS045
02992          MOVE RR-REPORT-ID       TO H1-RUN-CODE                   ECS045
02993          MOVE +0                 TO PAGE-CNT                      ECS045
02994          GO TO EEEE-ACCUM-DETAIL.                                 ECS045
02995                                                                   ECS045
02996      MOVE RR-CONTROL-B-D         TO SAVE-RR-CONTROL-B-D.          ECS045
02997      MOVE RR-ACC-NAME            TO SAVE-ACC-NAME.                ECS045
02998      MOVE RR-C-REI-NAME          TO SAVE-REI-NAME.                ECS045
02999      MOVE RR-C-CEDE-NAME         TO SAVE-CEDE-NAME.               ECS045
03000      MOVE RR-CLAIM-CODE          TO SAVE-CLAIM-CODE.              ECS045
03001      MOVE RR-PRT-ST              TO SAVE-PRT-ST.                  ECS045
03002      MOVE RR-PRT-OW              TO SAVE-PRT-OW.                  ECS045
03003      MOVE RR-PRT-CRSV            TO SAVE-PRT-CRSV.                ECS045
03004      MOVE RR-ACCUMS              TO DETAIL-ACCUMULATORS.          ECS045
020612     MOVE RR-PE-LF               TO SAVE-LF-PE.
020612     MOVE RR-PE-AH               TO SAVE-AH-PE.
CIDMOD                                                                  ECS045
CIDMOD     IF RR-MORT = LOW-VALUES                                      ECS045
CIDMOD         MOVE SPACES             TO RR-MORT.                      ECS045
03005                                                                   ECS045
03006      IF RR-MORT = SPACES                                          ECS045
03007          MOVE WS-MORT-STD        TO DT-ENTRY (21)                 ECS045
03008                                     DT-ENTRY-2 (21)               ECS045
03009          GO TO DDDD-ACCUM-LEVEL-1-DETAIL.                         ECS045
03010                                                                   ECS045
03011      MOVE CLAS-STARTM            TO CLAS-INDEXM.                  ECS045
03012                                                                   ECS045
03013 *****************************                                     ECS045
03014 *  VALIDATE MORTALITY CODE  *                                     ECS045
03015 *****************************                                     ECS045
03016                                                                   ECS045
03017  DDDD-FIND-MORT-LOOP.                                             ECS045
03018      IF CLAS-INDEXM GREATER CLAS-MAXM  OR                         ECS045
03019         CLAS-INDEXM = +0                                          ECS045
03020          DISPLAY 'INVALID MORTALITY TABLE CODE (' RR-MORT ')'     ECS045
03021          MOVE '**** INVALID MORTALITY TABLE CODE ****'            ECS045
03022                                  TO WS-ABEND-MESSAGE              ECS045
03023          MOVE '0'                TO WAC-1                         ECS045
03024          MOVE '3'                TO WAC-2                         ECS045
03025          MOVE '01'               TO WAC-3-4                       ECS045
03026          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS045
03027          GO TO ABEND-PGM.                                         ECS045
03028                                                                   ECS045
03029      IF RR-MORT NOT = CLAS-MORT-CODE (CLAS-INDEXM)                ECS045
03030          ADD +1                  TO CLAS-INDEXM                   ECS045
03031          GO TO DDDD-FIND-MORT-LOOP.                               ECS045
03032                                                                   ECS045
03033      MOVE CLAS-MORT-DESC (CLAS-INDEXM) TO WS-MORT-DSC.            ECS045
03034      MOVE WS-MORT-DESC                 TO DT-ENTRY (21)           ECS045
03035                                           DT-ENTRY-2 (21).        ECS045
03036                                                                   ECS045
03037  DDDD-ACCUM-LEVEL-1-DETAIL.                                       ECS045
03038      PERFORM 0480-CALCULATE-LEVEL-1 THRU 0490-EXIT.               ECS045
03039                                                                   ECS045
03040  DDDD-ACCUM-EXIT.                                                 ECS045
03041      PERFORM 0940-RETURN-SW-FILE THRU 0950-EXIT.                  ECS045
03042                                                                   ECS045
03043      GO TO DDDD-CHECK-CONTROL-BREAK.                              ECS045
03044                                                                   ECS045
03045 **********************************************************        ECS045
03046 *  END OUTPUT PROCEDURE(1) MAINLINE LOOP  REPORT(45-D)   *        ECS045
03047 **********************************************************        ECS045
03048                                                                   ECS045
03049      EJECT                                                        ECS045
03050 **********************************************************        ECS045
03051 *  EEEE ROUTINE IS THE START OF THE OUTPUT PROCEDURE (1) *        ECS045
03052 *  MAINLINE LOOP.  REPORT(45-E)                          *        ECS045
03053 **********************************************************        ECS045
03054                                                                   ECS045
03055  EEEE-CHECK-CONTROL-BREAK.                                        ECS045
03056 ****  CHECK FOR REINSURANCE COMPANY PRIME BREAK  ****             ECS045
03057                                                                   ECS045
03058      IF R-B-REI-COMP-PRIME-E NOT = SAVE-R-B-REI-COMP-PRIME-E      ECS045
03059          PERFORM EEEE-PRT-REINS-COMPANY-SUB THRU                  ECS045
03060                                 EEEE-PRT-REINS-COMPANY-SUB-X      ECS045
03061          PERFORM EEEE-PRT-REINS-COMPANY-PRIME THRU                ECS045
03062                                 EEEE-PRT-REINS-COMPANY-PRIME-X    ECS045
03063          GO TO EEEE-ACCUM-DETAIL.                                 ECS045
03064                                                                   ECS045
03065 ****  CHECK FOR REINSURANCE COMPANY SUB BREAK  ****               ECS045
03066                                                                   ECS045
03067      IF R-B-REI-COMP-SUB-E NOT = SAVE-R-B-REI-COMP-SUB-E          ECS045
03068          PERFORM EEEE-PRT-REINS-COMPANY-SUB THRU                  ECS045
03069                                 EEEE-PRT-REINS-COMPANY-SUB-X.     ECS045
03070                                                                   ECS045
03071      EJECT                                                        ECS045
03072  EEEE-ACCUM-DETAIL.                                               ECS045
03073      IF R-B-REI-COMP-E = HIGH-VALUES                              ECS045
03074          MOVE 'Y'                TO FINAL-TOT                     ECS045
03075          PERFORM EEEE-FINAL-TOTALS THRU EEEE-FINAL-TOTALS-X       ECS045
03076          MOVE 'N'                TO FINAL-TOT                     ECS045
03077          GO TO 0990-END-PRINT-STATEMENT.                          ECS045
03078                                                                   ECS045
03079      MOVE RR-CONTROL-B-E         TO SAVE-RR-CONTROL-B-E.          ECS045
03080      MOVE RR-ACC-NAME            TO SAVE-ACC-NAME.                ECS045
03081      MOVE RR-C-REI-NAME          TO SAVE-REI-NAME.                ECS045
03082      MOVE RR-C-CEDE-NAME         TO SAVE-CEDE-NAME.               ECS045
03083      MOVE RR-CLAIM-CODE          TO SAVE-CLAIM-CODE.              ECS045
03084      MOVE RR-PRT-ST              TO SAVE-PRT-ST.                  ECS045
03085      MOVE RR-PRT-OW              TO SAVE-PRT-OW.                  ECS045
03086      MOVE RR-PRT-CRSV            TO SAVE-PRT-CRSV.                ECS045
03087      MOVE RR-ACCUMS              TO DETAIL-ACCUMULATORS.          ECS045
020612     MOVE RR-PE-LF               TO SAVE-LF-PE.
020612     MOVE RR-PE-AH               TO SAVE-AH-PE.
CIDMOD                                                                  ECS045
CIDMOD     IF RR-MORT = LOW-VALUES                                      ECS045
CIDMOD         MOVE SPACES             TO RR-MORT.                      ECS045
03088                                                                   ECS045
03089      IF RR-MORT = SPACES                                          ECS045
03090          MOVE WS-MORT-STD        TO DT-ENTRY (21)                 ECS045
03091                                     DT-ENTRY-2 (21)               ECS045
03092          GO TO EEEE-ACCUM-LEVEL-1-DETAIL.                         ECS045
03093                                                                   ECS045
03094      MOVE CLAS-STARTM            TO CLAS-INDEXM.                  ECS045
03095                                                                   ECS045
03096 *****************************                                     ECS045
03097 *  VALIDATE MORTALITY CODE  *                                     ECS045
03098 *****************************                                     ECS045
03099                                                                   ECS045
03100  EEEE-FIND-MORT-LOOP.                                             ECS045
03101      IF CLAS-INDEXM GREATER CLAS-MAXM  OR                         ECS045
03102         CLAS-INDEXM = +0                                          ECS045
03103          DISPLAY 'INVALID MORTALITY TABLE CODE (' RR-MORT ')'     ECS045
03104          MOVE '**** INVALID MORTALITY TABLE CODE ****'            ECS045
03105                                  TO WS-ABEND-MESSAGE              ECS045
03106          MOVE '0'                TO WAC-1                         ECS045
03107          MOVE '3'                TO WAC-2                         ECS045
03108          MOVE '01'               TO WAC-3-4                       ECS045
03109          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS045
03110          GO TO ABEND-PGM.                                         ECS045
03111                                                                   ECS045
03112      IF RR-MORT NOT = CLAS-MORT-CODE (CLAS-INDEXM)                ECS045
03113          ADD +1                  TO CLAS-INDEXM                   ECS045
03114          GO TO EEEE-FIND-MORT-LOOP.                               ECS045
03115                                                                   ECS045
03116      MOVE CLAS-MORT-DESC (CLAS-INDEXM) TO WS-MORT-DSC.            ECS045
03117      MOVE WS-MORT-DESC                 TO DT-ENTRY (21)           ECS045
03118                                           DT-ENTRY-2 (21).        ECS045
03119                                                                   ECS045
03120  EEEE-ACCUM-LEVEL-1-DETAIL.                                       ECS045
03121      PERFORM 0480-CALCULATE-LEVEL-1 THRU 0490-EXIT.               ECS045
03122                                                                   ECS045
03123  EEEE-ACCUM-EXIT.                                                 ECS045
03124      PERFORM 0940-RETURN-SW-FILE THRU 0950-EXIT.                  ECS045
03125                                                                   ECS045
03126      GO TO EEEE-CHECK-CONTROL-BREAK.                              ECS045
03127                                                                   ECS045
03128 **********************************************************        ECS045
03129 *  END OUTPUT PROCEDURE(1) MAINLINE LOOP  REPORT(45-E)   *        ECS045
03130 **********************************************************        ECS045
03131                                                                   ECS045
03132      EJECT                                                        ECS045
03133  0440-ZERO-PAA.                                                   ECS045
03134      MOVE ZEROS                  TO PAA-GP   (SA)                 ECS045
03135                                     PAA-PR   (SA)                 ECS045
03136                                     PAA-NP   (SA)                 ECS045
03137                                     PAA-BUP  (SA)                 ECS045
03138                                     PAA-EUP  (SA)                 ECS045
03139                                     PAA-RP   (SA)                 ECS045
03140                                     PAA-RF   (SA)                 ECS045
03141                                     PAA-CP   (SA)                 ECS045
03142                                     PAA-AC   (SA)                 ECS045
03143                                     PAA-OW   (SA)                 ECS045
03144                                     PAA-CRSV (SA)                 ECS045
03145                                     PAA-ST   (SA)                 ECS045
03146                                     PAA-AJ   (SA)                 ECS045
03147                                     PAA-TD   (SA)                 ECS045
03148                                     PAA-NB   (SA)                 ECS045
03149                                     PAA-CRB  (SA)                 ECS045
03150                                     PAA-CRE  (SA)                 ECS045
03151                                     PAA-IC   (SA)                 ECS045
03152                                     PAA-SBUP (SA)                 ECS045
03153                                     PAA-SEUP (SA)                 ECS045
03154                                     PAA-CPI  (SA).                ECS045
022612     MOVE ZEROS                  TO PAA-TCOM (SA).
03155                                                                   ECS045
03156  0450-EXIT.                                                       ECS045
03157      EXIT.                                                        ECS045
03158                                                                   ECS045
03159  0460-ZERO-PAB.                                                   ECS045
03160      MOVE ZEROS                  TO PAB-LMR  (SA)                 ECS045
03161                                     PAB-AUP1 (SA)                 ECS045
03162                                     PAB-LA   (SA)                 ECS045
03163                                     PAB-LU   (SA)                 ECS045
03164                                     PAB-LT   (SA)                 ECS045
03165                                     PAB-AA   (SA)                 ECS045
03166                                     PAB-AF   (SA)                 ECS045
03167                                     PAB-AU   (SA)                 ECS045
03168                                     PAB-AT   (SA)                 ECS045
03169                                     PAB-MB   (SA)                 ECS045
03170                                     PAB-IF   (SA)                 ECS045
03171                                     PAB-LUP  (SA)                 ECS045
03172                                     PAB-AUP2 (SA).                ECS045
03173                                                                   ECS045
03174  0470-EXIT.                                                       ECS045
03175      EXIT.                                                        ECS045
03176                                                                   ECS045
03177      EJECT                                                        ECS045
03178  0480-CALCULATE-LEVEL-1.                                          ECS045
03179      PERFORM 0500-COMPUTE-UP THRU 0510-EXIT VARYING SA            ECS045
03180                            FROM +1 BY +1 UNTIL SA GREATER +8.     ECS045
03181                                                                   ECS045
03182 ** SB(1) PAA-ACCUMS (DATE RANGE TOTALS / LIFE TOTALS CURR MNTH) **ECS045
03183 ** SC(5) DA-ACCUMS  (LIFE CURRENT DATA)                         **ECS045
03184 ** SE(3) DA-ACCUMS  (LIFE PREVIOUS MONTH DATA)                  **ECS045
03185      MOVE +1 TO SB.                                               ECS045
03186      MOVE +5 TO SC.                                               ECS045
03187      MOVE +3 TO SE.                                               ECS045
03188      PERFORM 0560-ACCUM-PAA THRU 0570-EXIT.                       ECS045
03189                                                                   ECS045
03190 ** SB(4) PAA-ACCUMS (DATE RANGE TOTALS / LIFE TOTALS YTD)       **ECS045
03191 ** SC(5) DA-ACCUMS  (LIFE CURRENT DATA)                         **ECS045
03192 ** SE(1) DA-ACCUMS  (LIFE BEGINING DATA)                        **ECS045
03193      MOVE +4 TO SB.                                               ECS045
03194      MOVE +1 TO SE.                                               ECS045
03195      PERFORM 0560-ACCUM-PAA THRU 0570-EXIT.                       ECS045
03196                                                                   ECS045
03197 ** SB(7) PAA-ACCUMS (DATE RANGE TOTALS / LIFE TOTALS ITD)       **ECS045
03198 ** SC(5) DA-ACCUMS  (LIFE CURRENT DATA)                         **ECS045
03199 ** SE(7) DA-ACCUMS  (ZEROED DATA)                                 ECS045
03200      MOVE +7 TO SB.                                               ECS045
03201      MOVE +7 TO SE.                                               ECS045
03202      PERFORM 0560-ACCUM-PAA THRU 0570-EXIT.                       ECS045
03203                                                                   ECS045
03204 ** SB(2) PAA-ACCUMS (DATE RANGE TOTALS / AH TOTALS CURR MNTH)   **ECS045
03205 ** SC(6) DA-ACCUMS  (AH CURRENT DATA)                           **ECS045
03206 ** SE(4) DA-ACCUMS  (AH PREVIOUS MONTH DATA)                    **ECS045
03207      MOVE +2 TO SB.                                               ECS045
03208      MOVE +6 TO SC.                                               ECS045
03209      MOVE +4 TO SE.                                               ECS045
03210      PERFORM 0560-ACCUM-PAA THRU 0570-EXIT.                       ECS045
03211                                                                   ECS045
03212 ** SB(5) PAA-ACCUMS (DATE RANGE TOTALS / AH TOTALS YTD)         **ECS045
03213 ** SC(6) DA-ACCUMS  (AH CURRENT DATA)                           **ECS045
03214 ** SE(2) DA-ACCUMS  (AH BEGINING DATA)                          **ECS045
03215      MOVE +5 TO SB.                                               ECS045
03216      MOVE +2 TO SE.                                               ECS045
03217      PERFORM 0560-ACCUM-PAA THRU 0570-EXIT.                       ECS045
03218                                                                   ECS045
03219 ** SB(8) PAA-ACCUMS (DATE RANGE TOTALS / AH TOTALS ITD)         **ECS045
03220 ** SC(6) DA-ACCUMS  (AH CURRENT DATA)                           **ECS045
03221 ** SE(8) DA-ACCUMS  (ZEROED DATA)                                 ECS045
03222      MOVE +8 TO SB.                                               ECS045
03223      MOVE +8 TO SE.                                               ECS045
03224      PERFORM 0560-ACCUM-PAA THRU 0570-EXIT.                       ECS045
03225                                                                   ECS045
03226                                                                   ECS045
03227 ** SB(1) PAB-ACCUMS (DATE RANGE TOTALS / BEGINING CURR MNTH)    **ECS045
03228 ** SE(3) DA-ACCUMS  (LIFE PREVIOUS MONTH DATA)                  **ECS045
03229 ** SF(4) DA-ACCUMS  (AH PREVIOUS MONTH DATA)                    **ECS045
03230      MOVE +1 TO SB.                                               ECS045
03231      MOVE +3 TO SE.                                               ECS045
03232      MOVE +4 TO SF.                                               ECS045
03233      PERFORM 0580-ACCUM-PAB THRU 0590-EXIT.                       ECS045
03234                                                                   ECS045
03235 ** SB(2) PAB-ACCUMS (DATE RANGE TOTALS / ENDING CURR MNTH)      **ECS045
03236 ** SE(5) DA-ACCUMS  (LIFE CURRENT MONTH DATA)                   **ECS045
03237 ** SF(6) DA-ACCUMS  (AH CURRENT MONTH DATA)                     **ECS045
03238      MOVE +2 TO SB.                                               ECS045
03239      MOVE +5 TO SE.                                               ECS045
03240      MOVE +6 TO SF.                                               ECS045
03241      PERFORM 0580-ACCUM-PAB THRU 0590-EXIT.                       ECS045
03242                                                                   ECS045
03243 ** SB(4) PAB-ACCUMS (DATE RANGE TOTALS / ENDING YTD)            **ECS045
03244 ** SE(5) DA-ACCUMS  (LIFE CURRENT MONTH DATA)                   **ECS045
03245 ** SF(6) DA-ACCUMS  (AH CURRENT MONTH DATA)                     **ECS045
03246      MOVE +4 TO SB.                                               ECS045
03247      PERFORM 0580-ACCUM-PAB THRU 0590-EXIT.                       ECS045
03248                                                                   ECS045
03249 ** SB(3) PAB-ACCUMS (DATE RANGE TOTALS / BEGINING YTD)          **ECS045
03250 ** SE(1) DA-ACCUMS  (LIFE BEGINING MONTH DATA)                  **ECS045
03251 ** SF(2) DA-ACCUMS  (AH BEGINING MONTH DATA)                    **ECS045
03252      MOVE +3 TO SB.                                               ECS045
03253      MOVE +1 TO SE.                                               ECS045
03254      MOVE +2 TO SF.                                               ECS045
03255      PERFORM 0580-ACCUM-PAB THRU 0590-EXIT.                       ECS045
03256                                                                   ECS045
03257 ** SB(5) PAB-ACCUMS (DATE RANGE TOTALS / BEGINING ITD)          **ECS045
03258 ** SE(7) DA-ACCUMS  (ZEROD DATA)                                  ECS045
03259 ** SF(8) DA-ACCUMS  (ZEROD DATA)                                  ECS045
03260      MOVE +5 TO SB.                                               ECS045
03261      MOVE +7 TO SE.                                               ECS045
03262      MOVE +8 TO SF.                                               ECS045
03263      PERFORM 0580-ACCUM-PAB THRU 0590-EXIT.                       ECS045
03264                                                                   ECS045
03265 ** SB(6) PAB-ACCUMS (DATE RANGE TOTALS / BEGINING ITD)          **ECS045
03266 ** SE(5) DA-ACCUMS  (LIFE CURRENT MONTH DATA)                   **ECS045
03267 ** SF(6) DA-ACCUMS  (AH CURRENT MONTH DATA)                     **ECS045
03268      MOVE +6 TO SB.                                               ECS045
03269      MOVE +5 TO SE.                                               ECS045
03270      MOVE +6 TO SF.                                               ECS045
03271      PERFORM 0580-ACCUM-PAB THRU 0590-EXIT.                       ECS045
03272                                                                   ECS045
03273  0490-EXIT.                                                       ECS045
03274      EXIT.                                                        ECS045
03275                                                                   ECS045
03276      EJECT                                                        ECS045
03277 ***************************************                           ECS045
03278 *  COMPLETE DA-CNTR CALCULATIONS      *                           ECS045
03279 *  1) UNEARNED PREMIUM                *                           ECS045
03280 *  2) UNEARNED COMMISSIONS            *                           ECS045
03281 *  3) UNEARNED OVERWRITE COMMISSIONS  *                           ECS045
03282 ***************************************                           ECS045
03283                                                                   ECS045
03284  0500-COMPUTE-UP.                                                 ECS045
03285 *************************************************                 ECS045
03286 *  IF PERCENTS ARE PROVIDED BY REINSURANCE      *                 ECS045
03287 *  RECORD COMPUTE UNEARNED AS A PORTION OF      *                 ECS045
03288 *  RULE-78 AND PRO-RATA OTHERWISE LIFE          *                 ECS045
03289 *  CALCULATIONS WERE DONE BY RULE-78(REDUCING)  *                 ECS045
03290 *  PRO-RATA(LEVEL).                             *                 ECS045
03291 **********************************************************        ECS045
03292 * (DA-UPP)  UNEARNED PREMIUM PRO-RATA                    *        ECS045
03293 * (DA-UPR)  UNEARNED PREMIUM RULE-78                     *        ECS045
03294 * (DA-UCP)  UNEARNED ACCOUNT LEVEL COMMISSIONS PRO-RATA  *        ECS045
03295 * (DA-UCR)  UNEARNED ACCOUNT LEVEL COMMISSIONS RULE-78   *        ECS045
03296 * (DA-UOP)  UNEARNED OVERWRITE COMMISSIONS PRO-RATA      *        ECS045
03297 * (DA-UOR)  UNEARNED OVERWRITE COMMISSIONS RULE-78       *        ECS045
03298 * (DA-UP)   UNEARNED PREMIUM                             *        ECS045
03299 * (DA-UC)   UNEARNED COMMISSIONS ACCOUNT LEVEL           *        ECS045
03300 * (DA-UO)   UNEARNED COMMISSIONS OVERWRITE LEVEL         *        ECS045
03301 * (DA-CU)   IBNR CLAIM RSVS (IBNR PRCNT * UNEARNED PREM) *        ECS045
03302 **********************************************************        ECS045
03303                                                                   ECS045
03304      IF SA = +2 OR +4 OR +6                                       ECS045
03305          GO TO 0508-AH-COMPUTATIONS.                              ECS045
03306                                                                   ECS045
03307      IF LIFE-PERCENTS-AVAILABLE                                   ECS045
03308          GO TO 0504-LF-BY-PERCENTS.                               ECS045
03309                                                                   ECS045
03310  0502-LF-BY-LEVEL-REDUCING.                                       ECS045
03311      ADD DA-UPR (SA) DA-UPP (SA) GIVING DA-UP (SA).               ECS045
060903     IF DTE-CLIENT = 'DCC'
060903        MOVE DA-UP (SA)          TO DA-MORT (SA)
060903     END-IF
03312      ADD DA-UCR (SA) DA-UCP (SA) GIVING DA-UC (SA).               ECS045
03313      ADD DA-UOR (SA) DA-UOP (SA) GIVING DA-UO (SA).               ECS045
03314                                                                   ECS045
03315      GO TO 0506-IBNR-COMPUTATIONS.                                ECS045
03316                                                                   ECS045
03317  0504-LF-BY-PERCENTS.                                             ECS045
03318      COMPUTE DA-UP (SA) = (DA-UPR (SA) * RR-LF-78-PCT) +          ECS045
03319                               (DA-UPP (SA) * RR-LF-PR-PCT).       ECS045
060903     IF DTE-CLIENT = 'DCC'
060903        MOVE DA-UP (SA)          TO DA-MORT (SA)
060903     END-IF
03320      COMPUTE DA-UC (SA) = (DA-UCR (SA) * RR-LF-78-PCT) +          ECS045
03321                               (DA-UCP (SA) * RR-LF-PR-PCT).       ECS045
03322      COMPUTE DA-UO (SA) = (DA-UOR (SA) * RR-LF-78-PCT) +          ECS045
03323                               (DA-UOP (SA) * RR-LF-PR-PCT).       ECS045
03324                                                                   ECS045
03325  0506-IBNR-COMPUTATIONS.                                          ECS045
03326      IF RR-LF-IBNR-PCT NOT = ZERO                                 ECS045
03327          COMPUTE DA-CU (SA) ROUNDED =  DA-CU (SA) +               ECS045
03328                                    (DA-UP (SA) * RR-LF-IBNR-PCT). ECS045
03329                                                                   ECS045
03330      GO TO 0510-EXIT.                                             ECS045
03331                                                                   ECS045
03332  0508-AH-COMPUTATIONS.                                            ECS045
03333      COMPUTE DA-UP (SA) = (DA-UPR (SA) * RR-AH-78-PCT) +          ECS045
03334                               (DA-UPP (SA) * RR-AH-PR-PCT)        ECS045
03335      COMPUTE DA-UC (SA) = (DA-UCR (SA) * RR-AH-78-PCT) +          ECS045
03336                               (DA-UCP (SA) * RR-AH-PR-PCT)        ECS045
03337      COMPUTE DA-UO (SA) = (DA-UOR (SA) * RR-AH-78-PCT) +          ECS045
03338                               (DA-UOP (SA) * RR-AH-PR-PCT).       ECS045
03339                                                                   ECS045
03340      IF RR-AH-IBNR-PCT NOT = ZERO                                 ECS045
03341          COMPUTE DA-CU (SA) ROUNDED =  DA-CU (SA) +               ECS045
03342                                    (DA-UP (SA) * RR-AH-IBNR-PCT). ECS045
03343                                                                   ECS045
03344  0510-EXIT.                                                       ECS045
03345      EXIT.                                                        ECS045
03346                                                                   ECS045
03347      EJECT                                                        ECS045
03348 *****************************************                         ECS045
03349 *  ADD A LEVEL OF THE PAA-ACCUMULATORS  *                         ECS045
03350 *  TO THE NEXT HIGHEST LEVEL IN TABLE   *                         ECS045
03351 *****************************************************             ECS045
03352 * USED AT REPORT BREAK TIME RTNS (640- THRU 0830-) *              ECS045
03353 *****************************************************             ECS045
03354                                                                   ECS045
03355  0520-ADD-PAA.                                                    ECS045
03356      ADD PAA-GP   (SB)           TO PAA-GP   (SC).                ECS045
03357      ADD PAA-PR   (SB)           TO PAA-PR   (SC).                ECS045
03358      ADD PAA-NP   (SB)           TO PAA-NP   (SC).                ECS045
03359      ADD PAA-BUP  (SB)           TO PAA-BUP  (SC).                ECS045
03360      ADD PAA-EUP  (SB)           TO PAA-EUP  (SC).                ECS045
03361      ADD PAA-RP   (SB)           TO PAA-RP   (SC).                ECS045
03362      ADD PAA-RF   (SB)           TO PAA-RF   (SC).                ECS045
03363      ADD PAA-CP   (SB)           TO PAA-CP   (SC).                ECS045
03364      ADD PAA-AC   (SB)           TO PAA-AC   (SC).                ECS045
03365      ADD PAA-OW   (SB)           TO PAA-OW   (SC).                ECS045
03366      ADD PAA-ST   (SB)           TO PAA-ST   (SC).                ECS045
03367      ADD PAA-CRSV (SB)           TO PAA-CRSV (SC).                ECS045
03368      ADD PAA-AJ   (SB)           TO PAA-AJ   (SC).                ECS045
03369      ADD PAA-TD   (SB)           TO PAA-TD   (SC).                ECS045
03370      ADD PAA-NB   (SB)           TO PAA-NB   (SC).                ECS045
03371      ADD PAA-CRB  (SB)           TO PAA-CRB  (SC).                ECS045
03372      ADD PAA-CRE  (SB)           TO PAA-CRE  (SC).                ECS045
03373      ADD PAA-IC   (SB)           TO PAA-IC   (SC).                ECS045
03374      ADD PAA-SBUP (SB)           TO PAA-SBUP (SC).                ECS045
03375      ADD PAA-SEUP (SB)           TO PAA-SEUP (SC).                ECS045
03376      ADD PAA-CPI  (SB)           TO PAA-CPI  (SC).                ECS045
022912     ADD PAA-TCOM (SB)           TO PAA-TCOM (SC).
03377      ADD +1                      TO SB SC.                        ECS045
03378                                                                   ECS045
03379  0530-EXIT.                                                       ECS045
03380      EXIT.                                                        ECS045
03381                                                                   ECS045
03382      EJECT                                                        ECS045
03383 *****************************************                         ECS045
03384 *  ADD A LEVEL OF THE PAB-ACCUMULATORS  *                         ECS045
03385 *  TO THE NEXT HIGHEST LEVEL IN TABLE   *                         ECS045
03386 *****************************************************             ECS045
03387 * USED AT REPORT BREAK TIME RTNS (640- THRU 0830-) *              ECS045
03388 *****************************************************             ECS045
03389                                                                   ECS045
03390  0540-ADD-PAB.                                                    ECS045
03391      ADD PAB-LMR  (SB)           TO PAB-LMR  (SC).                ECS045
03392      ADD PAB-AUP1 (SB)           TO PAB-AUP1 (SC).                ECS045
03393      ADD PAB-LA   (SB)           TO PAB-LA   (SC).                ECS045
03394      ADD PAB-LU   (SB)           TO PAB-LU   (SC).                ECS045
03395      ADD PAB-LT   (SB)           TO PAB-LT   (SC).                ECS045
03396      ADD PAB-AA   (SB)           TO PAB-AA   (SC).                ECS045
03397      ADD PAB-AF   (SB)           TO PAB-AF   (SC).                ECS045
03398      ADD PAB-AU   (SB)           TO PAB-AU   (SC).                ECS045
03399      ADD PAB-AT   (SB)           TO PAB-AT   (SC).                ECS045
03400      ADD PAB-MB   (SB)           TO PAB-MB   (SC).                ECS045
03401      ADD PAB-IF   (SB)           TO PAB-IF   (SC).                ECS045
03402      ADD PAB-LUP  (SB)           TO PAB-LUP  (SC).                ECS045
03403      ADD PAB-AUP2 (SB)           TO PAB-AUP2 (SC).                ECS045
03404      ADD +1                      TO SB SC.                        ECS045
03405                                                                   ECS045
03406  0550-EXIT.                                                       ECS045
03407      EXIT.                                                        ECS045
03408                                                                   ECS045
03409      EJECT                                                        ECS045
03410 ***********************************************************       ECS045
03411 * (PAA-GP)   GROSS PREMIUM                                *       ECS045
03412 * (PAA-PR)   PREMIUM REFUNDED                             *       ECS045
03413 * (PAA-NP)   NET PREMIUM                                  *       ECS045
03414 * (PAA-BUP)  BEGINING UNEARNED PREMIUM (SEE NOTE #1)      *       ECS045
03415 * (PAA-EUP)  ENDING UNEARNED PREMIUM   (SEE NOTE #1)      *       ECS045
03416 * (PAA-SBUP) BEGINING UNEARNED PREMIUM (STD)              *       ECS045
03417 * (PAA-SEUP) ENDING UNEARNED PREMIUM   (STD)              *       ECS045
03418 * (PAA-RP)   REINSURANCE PREMIUM (EARNED PREMIUM)         *       ECS045
03419 * (PAA-AJ)   PAYMENTS AND ADJUSTMENTS  ********           *       ECS045
PEMTST* (PAA-AJ)   IS NOW EXCISE TAX AND ADJUSTMENTS*           *
03420 * (PAA-RF)   REINSURANCE FEE                              *       ECS045
03421 * (PAA-CP)   CLAIM PAYMENTS                               *       ECS045
03422 * (PAA-CPI)  CLAIM PAYMENTS INCURRED                      *       ECS045
03423 * (PAA-CRB)  CLAIM RESERVE BEGINNING                      *       ECS045
03424 * (PAA-CRE)  CLAIM RESERVE ENDING                         *       ECS045
03425 * (PAA-AC)   ACCOUNT LEVEL COMMISSIONS                    *       ECS045
03426 * (PAA-OW)   OVERWRITE LEVEL COMMISSIONS                  *       ECS045
03427 * (PAA-ST)   STATE TAXES                                  *       ECS045
03428 * (PAA-CRSV) CANCELLATION RESERVES                        *       ECS045
022912* (PAA-TCOM) TOTAL COMMISSIONS
03429 *                                                         *       ECS045
03430 *NOTE #1  UNEARNED PREMIUM FORMULA BASED ON SWITCH SETTING*       ECS045
03431 ***********************************************************       ECS045
03432                                                                   ECS045
03433  0560-ACCUM-PAA.                                                  ECS045
03434      COMPUTE PAA-GP (SB) = PAA-GP (SB) +                          ECS045
03435                     (DA-ISP (SC) - DA-ISP (SE)).                  ECS045
03436      COMPUTE PAA-PR (SB) = PAA-PR (SB) +                          ECS045
03437                     (DA-CNP (SC) - DA-CNP (SE)).                  ECS045
03438      COMPUTE PAA-NP (SB) =                                        ECS045
03439                     (PAA-GP (SB) - PAA-PR (SB)).                  ECS045
03440                                                                   ECS045
03441      IF SB = +2 OR +5 OR +8                                       ECS045
03442          IF RR-PE-AH = 'P'                                        ECS045
03443              NEXT SENTENCE                                        ECS045
03444          ELSE                                                     ECS045
03445              COMPUTE PAA-BUP (SB) = PAA-BUP (SB) + DA-UP (SE)     ECS045
03446              COMPUTE PAA-EUP (SB) = PAA-EUP (SB) + DA-UP (SC)     ECS045
03447      ELSE                                                         ECS045
03448          IF RR-PE-LF = 'P'                                        ECS045
03449              NEXT SENTENCE                                        ECS045
03450          ELSE                                                     ECS045
03451              IF RR-PE-LF = 'M'                                    ECS045
03452                 COMPUTE PAA-BUP (SB) = PAA-BUP (SB) + DA-MORT (SE)ECS045
03453                 COMPUTE PAA-EUP (SB) = PAA-EUP (SB) + DA-MORT (SC)ECS045
03454              ELSE                                                 ECS045
03455                  COMPUTE PAA-BUP (SB) = PAA-BUP (SB) + DA-UP (SE) ECS045
03456                  COMPUTE PAA-EUP (SB) = PAA-EUP (SB) + DA-UP (SC).ECS045
03457                                                                   ECS045
03458      COMPUTE PAA-RP (SB) = (PAA-NP (SB) + PAA-BUP (SB))           ECS045
03459                           - PAA-EUP (SB).                         ECS045
03460                                                                   ECS045
03461      COMPUTE PAA-SBUP (SB) = PAA-SBUP (SB) + DA-UP (SE).          ECS045
03462      COMPUTE PAA-SEUP (SB) = PAA-SEUP (SB) + DA-UP (SC).          ECS045

122408     COMPUTE PAA-AJ   (SB) = PAA-AJ (SB) +
122408                             (DA-ADJ (SC) - DA-ADJ (SE))

03466      IF SB = +2 OR +5 OR +8                                       ECS045
03467          GO TO 0565-ACCUM-PAA-AH.                                 ECS045
03468                                                                   ECS045
03469  0562-ACCUM-PAA-LIFE.                                             ECS045
03470 ******************************                                    ECS045
03471 *  COMPUTE REINSURANCE FEE    *                                   ECS045
03472 ******************************                                    ECS045
03473                                                                   ECS045
03474      IF DTE-CLIENT = 'MON'  AND                                   ECS045
03475        (RR-REI-PRIME = 'R31' OR 'R32' OR 'R34' OR 'R47')          ECS045
03476          COMPUTE PAA-RF (SB) ROUNDED = PAA-RF (SB) +              ECS045
03477                ( (DA-ISP (SC) - DA-ISP (SE) ) * RR-FEE-LF)        ECS045
03478      ELSE                                                         ECS045
03479        IF RR-0-PE-FEE-LF = 'M'                                    ECS045
03480          COMPUTE PAA-RF (SB) ROUNDED = PAA-RF (SB) +              ECS045
03481             ( ( ( (DA-ISP (SC) - DA-CNP (SC)) - DA-MORT (SC)) -   ECS045
03482                 ( (DA-ISP (SE) - DA-CNP (SE)) - DA-MORT (SE)) ) * ECS045
03483                    RR-FEE-LF)                                     ECS045
03484        ELSE                                                       ECS045
03485          IF RR-0-PE-FEE-LF = 'P'                                  ECS045
03486            COMPUTE PAA-RF (SB) ROUNDED = PAA-RF (SB) +            ECS045
03487                ( ( (DA-ISP (SC) - DA-CNP (SC)) -                  ECS045
03488                    (DA-ISP (SE) - DA-CNP (SE)) ) * RR-FEE-LF)     ECS045
03489          ELSE                                                     ECS045
03490            COMPUTE PAA-RF (SB) ROUNDED = PAA-RF (SB) +            ECS045
03491                ( ( ( (DA-ISP (SC) - DA-CNP (SC)) - DA-UP (SC)) -  ECS045
03492                  ( (DA-ISP (SE) - DA-CNP (SE)) - DA-UP (SE)) ) *  ECS045
03493                     RR-FEE-LF).                                   ECS045
03494                                                                   ECS045
03470 ******************************
03471 *  COMPUTE EXCISE TAX        *
03472 ******************************

032707     EVALUATE RR-PE-LF
032707        WHEN 'M'
032707           COMPUTE PAA-AJ (SB) ROUNDED = PAA-AJ (SB) +
032707            ((((DA-ISP (SC) - DA-CNP (SC)) - DA-MORT (SC)) -
032707              ((DA-ISP (SE) - DA-CNP (SE)) - DA-MORT (SE))) *
032707                RR-EXCISE-TAX)
032707        WHEN 'P'
032707           COMPUTE PAA-AJ (SB) ROUNDED = PAA-AJ (SB) +
032707            (((DA-ISP (SC) - DA-CNP (SC)) -
032707              (DA-ISP (SE) - DA-CNP (SE))) * RR-EXCISE-TAX)
032707        WHEN OTHER
032707           COMPUTE PAA-AJ (SB) ROUNDED = PAA-AJ (SB) +
032707            ((((DA-ISP (SC) - DA-CNP (SC)) - DA-UP (SC)) -
032707              ((DA-ISP (SE) - DA-CNP (SE)) - DA-UP (SE))) *
032707                RR-EXCISE-TAX)
032707     END-EVALUATE
03494                                                                   ECS045
03495 ******************************                                    ECS045
03496 *  COMPUTE CLAIMS PAID       *                                    ECS045
03497 ******************************                                    ECS045
03498                                                                   ECS045
03499      IF RR-PI-CLM-LF = 'P'                                        ECS045
03500          COMPUTE PAA-CP (SB) = PAA-CP (SB) +                      ECS045
03501                        (DA-CLM (SC) - DA-CLM (SE))                ECS045
03502      ELSE                                                         ECS045
03503          COMPUTE PAA-CRB (SB) = PAA-CRB (SB) +                    ECS045
03504                   DA-CA (SE) + DA-CU (SE) + DA-CF (SE)            ECS045
03505          COMPUTE PAA-CRE (SB) = PAA-CRE (SB) +                    ECS045
03506                   DA-CA (SC) + DA-CU (SC) + DA-CF (SC)            ECS045
03507          COMPUTE PAA-CPI (SB) = PAA-CPI (SB) +                    ECS045
03508                         (DA-CLM (SC) - DA-CLM (SE)).              ECS045
03509                                                                   ECS045
03510 *****************************************************             ECS045
03511 *  COMPUTE ACCOUNT LEVEL AND OVERWRITE COMMISSIONS  *             ECS045
03512 *****************************************************             ECS045
03513                                                                   ECS045
022912     COMPUTE PAA-TCOM (SB) = PAA-TCOM (SB) +
022912           ((DA-ISC (SC) - DA-CNC (SC)) -
022912            (DA-ISC (SE) - DA-CNC (SE))).
022912
03514      IF RR-0-PE-FEE-LF = 'M'                                      ECS045
03515          GO TO 0563-MORTALITY-BASIS.                              ECS045
03516                                                                   ECS045
03517      IF RR-PE-COMM-LF = 'E'                                       ECS045
03518          GO TO 0563-LF-EARNED-BASIS.                              ECS045
03519                                                                   ECS045
03520      COMPUTE PAA-AC (SB) = PAA-AC (SB) +                          ECS045
03521          ( (DA-ISC (SC) - DA-CNC (SC)) -                          ECS045
03522            (DA-ISC (SE) - DA-CNC (SE)) ).                         ECS045
03523                                                                   ECS045
03524      IF RR-PRT-OW = 'A'                                           ECS045
03525          COMPUTE PAA-AC (SB) = PAA-AC (SB) +                      ECS045
03526              ( (DA-ISO (SC) - DA-CNO (SC)) -                      ECS045
03527                (DA-ISO (SE) - DA-CNO (SE)) ).                     ECS045
03528                                                                   ECS045
03529      IF RR-PRT-OW = 'F'                                           ECS045
03530          COMPUTE PAA-RF (SB) = PAA-RF (SB) +                      ECS045
03531              ( (DA-ISO (SC) - DA-CNO (SC)) -                      ECS045
03532                (DA-ISO (SE) - DA-CNO (SE)) ).                     ECS045
03533                                                                   ECS045
03534      IF RR-PRT-OW = 'Y'                                           ECS045
03535          COMPUTE PAA-OW (SB) = PAA-OW (SB) +                      ECS045
03536              ( (DA-ISO (SC) - DA-CNO (SC)) -                      ECS045
03537                (DA-ISO (SE) - DA-CNO (SE)) ).                     ECS045
03538                                                                   ECS045
03539      GO TO 0564-LF-STATE-TAX.                                     ECS045
03540                                                                   ECS045
03541  0563-LF-EARNED-BASIS.                                            ECS045
03542                                                                   ECS045
03543      COMPUTE PAA-AC (SB) = PAA-AC (SB) +                          ECS045
03544          ( ((DA-ISC (SC) - DA-CNC (SC)) - DA-UC (SC)) -           ECS045
03545            ((DA-ISC (SE) - DA-CNC (SE)) - DA-UC (SE)) ).          ECS045
03546                                                                   ECS045
03547      IF RR-PRT-OW = 'A'                                           ECS045
03548          COMPUTE PAA-AC (SB) = PAA-AC (SB) +                      ECS045
03549              ( ((DA-ISO (SC) - DA-CNO (SC)) - DA-UO (SC)) -       ECS045
03550                ((DA-ISO (SE) - DA-CNO (SE)) - DA-UO (SE)) ).      ECS045
03551                                                                   ECS045
03552      IF RR-PRT-OW = 'F'                                           ECS045
03553          COMPUTE PAA-RF (SB) = PAA-RF (SB) +                      ECS045
03554              ( ((DA-ISO (SC) - DA-CNO (SC)) - DA-UO (SC)) -       ECS045
03555                ((DA-ISO (SE) - DA-CNO (SE)) - DA-UO (SE)) ).      ECS045
03556                                                                   ECS045
03557      IF RR-PRT-OW = 'Y'                                           ECS045
03558          COMPUTE PAA-OW (SB) = PAA-OW (SB) +                      ECS045
03559              ( ((DA-ISO (SC) - DA-CNO (SC)) - DA-UO (SC)) -       ECS045
03560                ((DA-ISO (SE) - DA-CNO (SE)) - DA-UO (SE)) ).      ECS045
03561                                                                   ECS045
03562      GO TO 0564-LF-STATE-TAX.                                     ECS045
03563                                                                   ECS045
03564  0563-MORTALITY-BASIS.                                            ECS045
03565                                                                   ECS045
03566      COMPUTE WS-MRC-SC-PERCENT =                                  ECS045
03567           (( DA-ISC (SC) - DA-CNC (SC) ) /                        ECS045
03568            ( DA-ISP (SC) - DA-CNP (SC) ))                         ECS045
03569         ON SIZE ERROR                                             ECS045
03570            MOVE +0  TO WS-MRC-SC-PERCENT.                         ECS045
03571                                                                   ECS045
03572      COMPUTE WS-MRC-SC =                                          ECS045
03573             DA-MORT (SC) * WS-MRC-SC-PERCENT.                     ECS045
03574                                                                   ECS045
03575      COMPUTE WS-MRO-SC-PERCENT =                                  ECS045
03576           (( DA-ISO (SC) - DA-CNO (SC) ) /                        ECS045
03577            ( DA-ISP (SC) - DA-CNP (SC) ))                         ECS045
03578         ON SIZE ERROR                                             ECS045
03579            MOVE +0 TO WS-MRO-SC-PERCENT.                          ECS045
03580                                                                   ECS045
03581      COMPUTE WS-MRO-SC =                                          ECS045
03582             DA-MORT (SC)   * WS-MRO-SC-PERCENT.                   ECS045
03583                                                                   ECS045
03584 *    COMPUTE WS-MRO-SC =                                          ECS045
03585 *        (( DA-MORT (SC) ) * (( DA-ISO (SC) - DA-CNO (SC) ) /     ECS045
03586 *          ( DA-ISP (SC) - DA-CNP (SC) )))                        ECS045
03587 *       ON SIZE ERROR                                             ECS045
03588 *          MOVE +0 TO WS-MRO-SC.                                  ECS045
03589                                                                   ECS045
03590      COMPUTE WS-MRC-SE-PERCENT =                                  ECS045
03591           ( DA-ISC (SE) - DA-CNC (SE) ) /                         ECS045
03592            ( DA-ISP (SE) - DA-CNP (SE) )                          ECS045
03593         ON SIZE ERROR                                             ECS045
03594            MOVE +0  TO WS-MRC-SE-PERCENT.                         ECS045
03595                                                                   ECS045
03596 *    COMPUTE WS-MRC-SE =                                          ECS045
03597 *        ( ( DA-MORT (SE) ) * ( DA-ISC (SE) - DA-CNC (SE) ) /     ECS045
03598 *          ( DA-ISP (SE) - DA-CNP (SE) ) )                        ECS045
03599 *       ON SIZE ERROR                                             ECS045
03600 *          MOVE +0  TO WS-MRC-SE.                                 ECS045
03601                                                                   ECS045
03602      COMPUTE WS-MRC-SE =                                          ECS045
03603           DA-MORT (SE) * WS-MRC-SE-PERCENT.                       ECS045
03604                                                                   ECS045
03605      COMPUTE WS-MRO-SE-PERCENT =                                  ECS045
03606           ( DA-ISO (SE) - DA-CNO (SE) ) /                         ECS045
03607            ( DA-ISP (SE) - DA-CNP (SE) )                          ECS045
03608         ON SIZE ERROR                                             ECS045
03609            MOVE +0  TO WS-MRO-SE-PERCENT.                         ECS045
03610                                                                   ECS045
03611      COMPUTE WS-MRO-SE =                                          ECS045
03612           DA-MORT (SE) * WS-MRO-SE-PERCENT.                       ECS045
03613                                                                   ECS045
03614      COMPUTE PAA-AC (SB) = PAA-AC (SB) +                          ECS045
03615        ((DA-ISC (SC) - DA-CNC (SC)) - WS-MRC-SC)                  ECS045
03616        -                                                          ECS045
03617        ((DA-ISC (SE) - DA-CNC (SE)) - WS-MRC-SE).                 ECS045
03618                                                                   ECS045
03619  0563-MR-BASIS-A.                                                 ECS045
03620                                                                   ECS045
03621      IF RR-PRT-OW = 'A'                                           ECS045
03622        COMPUTE PAA-AC (SB) = PAA-AC (SB) +                        ECS045
03623        ((DA-ISO (SC) - DA-CNO (SC)) -  WS-MRO-SC)                 ECS045
03624        -                                                          ECS045
03625        ((DA-ISO (SE) - DA-CNO (SE)) - WS-MRO-SC).                 ECS045
03626                                                                   ECS045
03627  0563-MR-BASIS-F.                                                 ECS045
03628                                                                   ECS045
03629      IF RR-PRT-OW = 'F'                                           ECS045
03630        COMPUTE PAA-RF (SB) = PAA-RF (SB) +                        ECS045
03631        ((DA-ISO (SC) - DA-CNO (SC)) -  WS-MRO-SC)                 ECS045
03632        -                                                          ECS045
03633        ((DA-ISO (SE) - DA-CNO (SE)) -  WS-MRO-SE).                ECS045
03634                                                                   ECS045
03635  0563-MR-BASIS-Y.                                                 ECS045
03636                                                                   ECS045
03637      IF RR-PRT-OW = 'Y'                                           ECS045
03638        COMPUTE PAA-OW (SB) = PAA-OW (SB) +                        ECS045
03639        ((DA-ISO (SC) - DA-CNO (SC)) - WS-MRO-SC)                  ECS045
03640        -                                                          ECS045
03641        ((DA-ISO (SE) - DA-CNO (SE)) - WS-MRO-SE).                 ECS045
03642                                                                   ECS045
03643 ******************************                                    ECS045
03644 *  COMPUTE STATE TAX AMOUNT  *                                    ECS045
03645 ******************************                                    ECS045
03646                                                                   ECS045
03647  0564-LF-STATE-TAX.                                               ECS045

03648      IF RR-PRT-ST = 'Y'                                           ECS045
03649         IF RR-PE-TAX-LF = 'P'                                     ECS045
03650            COMPUTE PAA-ST (SB) ROUNDED = PAA-ST (SB) +            ECS045
03651               (DA-TAX (SC) - DA-TAX (SE))                         ECS045
03653         ELSE                                                      ECS045
                 COMPUTE WS-SC-EARN =
                    (DA-ISP (SC) - DA-CNP (SC) - DA-UP (SC))
                 COMPUTE WS-SC-NW =
                    (DA-ISP (SC) - DA-CNP (SC))
                 COMPUTE WS-SE-EARN =
                    (DA-ISP (SE) - DA-CNP (SE) - DA-UP (SE))
                 COMPUTE WS-SE-NW =
                    (DA-ISP (SE) - DA-CNP (SE))
                 IF WS-SC-NW NOT = ZERO
                    COMPUTE WS-TAX-WRK1 ROUNDED =
                       WS-SC-EARN / WS-SC-NW
                 END-IF
                 IF WS-SE-NW NOT = ZERO
                    COMPUTE WS-TAX-WRK2 ROUNDED =
                       WS-SE-EARN / WS-SE-NW
                 END-IF
                 COMPUTE PAA-ST (SB) ROUNDED = PAA-ST (SB) +
                    ((WS-TAX-WRK1 * DA-TAX (SC)) -
                    (WS-TAX-WRK2 * DA-TAX (SE)))
03653         END-IF                                                    ECS045
03653      END-IF                                                       ECS045
03658                                                                   ECS045
03659      IF RR-PRT-ST = 'F'                                           ECS045
03660         IF RR-PE-TAX-LF = 'P'                                     ECS045
03661            COMPUTE PAA-RF (SB) ROUNDED = PAA-RF (SB) +            ECS045
03662               (DA-TAX (SC) - DA-TAX (SE))                         ECS045
03664         ELSE                                                      ECS045
                 COMPUTE WS-SC-EARN =
                    (DA-ISP (SC) - DA-CNP (SC) - DA-UP (SC))
                 COMPUTE WS-SC-NW =
                    (DA-ISP (SC) - DA-CNP (SC))
                 COMPUTE WS-SE-EARN =
                    (DA-ISP (SE) - DA-CNP (SE) - DA-UP (SE))
                 COMPUTE WS-SE-NW =
                    (DA-ISP (SE) - DA-CNP (SE))
                 IF WS-SC-NW NOT = ZERO
                    COMPUTE WS-TAX-WRK1 ROUNDED =
                       WS-SC-EARN / WS-SC-NW
                 END-IF
                 IF WS-SE-NW NOT = ZERO
                    COMPUTE WS-TAX-WRK2 ROUNDED =
                       WS-SE-EARN / WS-SE-NW
                 END-IF
                 COMPUTE PAA-RF (SB) ROUNDED = PAA-RF (SB) +
                    ((WS-TAX-WRK1 * DA-TAX (SC)) -
                    (WS-TAX-WRK2 * DA-TAX (SE)))
              END-IF
           END-IF
03669                                                                   ECS045
03670 ***************************************                           ECS045
03671 *  COMPUTE CANCELLATION RESERVE EXPENSE                           ECS045
03672 ***************************************                           ECS045
03673                                                                   ECS045
03674      IF DTE-CLIENT NOT = 'NCL'                                    ECS045
03675          GO TO 0570-EXIT.                                         ECS045
03676                                                                   ECS045
03677      COMPUTE WS-BEG-CRSV ROUNDED =                                ECS045
03678                            (DA-UP (SE) - DA-MORT (SE)) * .20.     ECS045
03679      IF WS-BEG-CRSV LESS THAN ZERO                                ECS045
03680          MOVE ZEROS               TO WS-BEG-CRSV.                 ECS045
03681                                                                   ECS045
03682      COMPUTE WS-END-CRSV ROUNDED =                                ECS045
03683                            (DA-UP (SC) - DA-MORT (SC)) * .20.     ECS045
03684      IF WS-END-CRSV LESS THAN ZERO                                ECS045
03685          MOVE ZEROS               TO WS-END-CRSV.                 ECS045
03686                                                                   ECS045
03687      COMPUTE WS-CANC-RESERVE =  WS-END-CRSV - WS-BEG-CRSV.        ECS045
03688                                                                   ECS045
03689      IF RR-PRT-CRSV = 'Y'                                         ECS045
03690          COMPUTE PAA-CRSV (SB) ROUNDED = PAA-CRSV (SB) +          ECS045
03691                                          WS-CANC-RESERVE.         ECS045
03692                                                                   ECS045
03693      IF RR-PRT-CRSV = 'F'                                         ECS045
03694          COMPUTE PAA-RF (SB) ROUNDED = PAA-RF (SB) +              ECS045
03695                                        WS-CANC-RESERVE.           ECS045
03696                                                                   ECS045
03697      IF RR-PRT-CRSV = 'A'                                         ECS045
03698          COMPUTE PAA-AC (SB) ROUNDED = PAA-AC (SB) +              ECS045
03699                                        WS-CANC-RESERVE.           ECS045
03700                                                                   ECS045
03701      GO TO 0570-EXIT.                                             ECS045
03702                                                                   ECS045
03704  0565-ACCUM-PAA-AH.                                               ECS045
03705 ******************************                                    ECS045
03706 *  COMPUTE REINSURANCE FEE    *                                   ECS045
03707 ******************************                                    ECS045
03708                                                                   ECS045
03709                                                                   ECS045
03710      IF DTE-CLIENT = 'MON'  AND                                   ECS045
03711        (RR-REI-PRIME = 'R31' OR 'R32' OR 'R34' OR 'R47')          ECS045
03712          COMPUTE PAA-RF (SB) ROUNDED = PAA-RF (SB) +              ECS045
03713                ( (DA-ISP (SC) - DA-ISP (SE) ) * RR-FEE-AH)        ECS045
03714      ELSE                                                         ECS045
03715        IF RR-0-PE-FEE-AH = 'P'                                    ECS045
03716          COMPUTE PAA-RF (SB) ROUNDED = PAA-RF (SB) +              ECS045
03717                (((DA-ISP (SC) - DA-CNP (SC)) -                    ECS045
03718                 (DA-ISP (SE) - DA-CNP (SE))) * RR-FEE-AH)         ECS045
03719        ELSE                                                       ECS045
03720          COMPUTE PAA-RF (SB) ROUNDED = PAA-RF (SB) +              ECS045
03721                ((((DA-ISP (SC) - DA-CNP (SC)) - DA-UP (SC)) -     ECS045
03722                 ((DA-ISP (SE) - DA-CNP (SE)) - DA-UP (SE))) *     ECS045
03723                   RR-FEE-AH).                                     ECS045
03724                                                                   ECS045
03705 ******************************                                    ECS045
03706 *  COMPUTE EXCISE TAX        *
03707 ******************************                                    ECS045
03709                                                                   ECS045
           EVALUATE RR-PE-AH
              WHEN 'P'
03716            COMPUTE PAA-AJ (SB) ROUNDED = PAA-AJ (SB) +
03717             (((DA-ISP (SC) - DA-CNP (SC)) -
03718               (DA-ISP (SE) - DA-CNP (SE))) * RR-EXCISE-TAX)
03719        WHEN OTHER
03720           COMPUTE PAA-AJ (SB) ROUNDED = PAA-AJ (SB) +
03721            ((((DA-ISP (SC) - DA-CNP (SC)) - DA-UP (SC)) -
03722              ((DA-ISP (SE) - DA-CNP (SE)) - DA-UP (SE))) *
03723                RR-EXCISE-TAX)
           END-EVALUATE
03724                                                                   ECS045
03725 ******************************                                    ECS045
03726 *  COMPUTE CLAIMS PAID       *                                    ECS045
03727 ******************************                                    ECS045
03728                                                                   ECS045
03729      IF RR-PI-CLM-AH = 'P'                                        ECS045
03730          COMPUTE PAA-CP (SB) = PAA-CP (SB) +                      ECS045
03731                        (DA-CLM (SC) - DA-CLM (SE))                ECS045
03732      ELSE                                                         ECS045
03733          COMPUTE PAA-CRB (SB) = PAA-CRB (SB) +                    ECS045
03734                   DA-CA (SE) + DA-CU (SE) + DA-CF (SE)            ECS045
03735          COMPUTE PAA-CRE (SB) = PAA-CRE (SB) +                    ECS045
03736                   DA-CA (SC) + DA-CU (SC) + DA-CF (SC)            ECS045
03737          COMPUTE PAA-CPI (SB) = PAA-CPI (SB) +                    ECS045
03738                         (DA-CLM (SC) - DA-CLM (SE)).              ECS045
03739                                                                   ECS045
03740 *****************************************************             ECS045
03741 *  COMPUTE ACCOUNT LEVEL AND OVERWRITE COMMISSIONS  *             ECS045
03742 *****************************************************             ECS045
03743                                                                   ECS045
022912     COMPUTE PAA-TCOM (SB) = PAA-TCOM (SB) +
022912           ((DA-ISC (SC) - DA-CNC (SC)) -
022912            (DA-ISC (SE) - DA-CNC (SE))).
022912
03744      IF RR-PE-COMM-AH = 'E'                                       ECS045
03745          GO TO 0566-AH-EARNED-BASIS.                              ECS045
03746                                                                   ECS045
03747      COMPUTE PAA-AC (SB) = PAA-AC (SB) +                          ECS045
03748          ( (DA-ISC (SC) - DA-CNC (SC)) -                          ECS045
03749            (DA-ISC (SE) - DA-CNC (SE)) ).                         ECS045
03750                                                                   ECS045
03751      IF RR-PRT-OW = 'A'                                           ECS045
03752          COMPUTE PAA-AC (SB) = PAA-AC (SB) +                      ECS045
03753              ( (DA-ISO (SC) - DA-CNO (SC)) -                      ECS045
03754                (DA-ISO (SE) - DA-CNO (SE)) ).                     ECS045
03755                                                                   ECS045
03756      IF RR-PRT-OW = 'F'                                           ECS045
03757          COMPUTE PAA-RF (SB) = PAA-RF (SB) +                      ECS045
03758              ( (DA-ISO (SC) - DA-CNO (SC)) -                      ECS045
03759                (DA-ISO (SE) - DA-CNO (SE)) ).                     ECS045
03760                                                                   ECS045
03761      IF RR-PRT-OW = 'Y'                                           ECS045
03762          COMPUTE PAA-OW (SB) = PAA-OW (SB) +                      ECS045
03763              ( (DA-ISO (SC) - DA-CNO (SC)) -                      ECS045
03764                (DA-ISO (SE) - DA-CNO (SE)) ).                     ECS045
03765                                                                   ECS045
03766      GO TO 0567-AH-STATE-TAX.                                     ECS045
03767                                                                   ECS045
03768  0566-AH-EARNED-BASIS.                                            ECS045
03769      COMPUTE PAA-AC (SB) = PAA-AC (SB) +                          ECS045
03770          ( ((DA-ISC (SC) - DA-CNC (SC)) - DA-UC (SC)) -           ECS045
03771            ((DA-ISC (SE) - DA-CNC (SE)) - DA-UC (SE)) ).          ECS045
03772                                                                   ECS045
03773      IF RR-PRT-OW = 'A'                                           ECS045
03774          COMPUTE PAA-AC (SB) = PAA-AC (SB) +                      ECS045
03775              ( ((DA-ISO (SC) - DA-CNO (SC)) - DA-UO (SC)) -       ECS045
03776                ((DA-ISO (SE) - DA-CNO (SE)) - DA-UO (SE)) ).      ECS045
03777                                                                   ECS045
03778      IF RR-PRT-OW = 'F'                                           ECS045
03779          COMPUTE PAA-RF (SB) = PAA-RF (SB) +                      ECS045
03780              ( ((DA-ISO (SC) - DA-CNO (SC)) - DA-UO (SC)) -       ECS045
03781                ((DA-ISO (SE) - DA-CNO (SE)) - DA-UO (SE)) ).      ECS045
03782                                                                   ECS045
03783      IF RR-PRT-OW = 'Y'                                           ECS045
03784          COMPUTE PAA-OW (SB) = PAA-OW (SB) +                      ECS045
03785              ( ((DA-ISO (SC) - DA-CNO (SC)) - DA-UO (SC)) -       ECS045
03786                ((DA-ISO (SE) - DA-CNO (SE)) - DA-UO (SE)) ).      ECS045
03787                                                                   ECS045
03788 ******************************                                    ECS045
03789 *  COMPUTE STATE TAX AMOUNT  *                                    ECS045
03790 ******************************                                    ECS045
03791                                                                   ECS045
03792  0567-AH-STATE-TAX.                                               ECS045
PEMMOD                                                                  ECS045
03793      IF RR-PRT-ST = 'Y'                                           ECS045
03794         IF RR-PE-TAX-AH = 'P'                                     ECS045
03795            COMPUTE PAA-ST (SB) ROUNDED = PAA-ST (SB) +            ECS045
03796               (DA-TAX (SC) - DA-TAX (SE))                         ECS045
03798         ELSE                                                      ECS045
PEMMOD           COMPUTE WS-SC-EARN =
PEMMOD              (DA-ISP (SC) - DA-CNP (SC) - DA-UP (SC))
PEMMOD           COMPUTE WS-SC-NW =
PEMMOD              (DA-ISP (SC) - DA-CNP (SC))
PEMMOD           COMPUTE WS-SE-EARN =
PEMMOD              (DA-ISP (SE) - DA-CNP (SE) - DA-UP (SE))
PEMMOD           COMPUTE WS-SE-NW =
PEMMOD              (DA-ISP (SE) - DA-CNP (SE))
PEMMOD           IF WS-SC-NW NOT = ZERO
PEMMOD              COMPUTE WS-TAX-WRK1 ROUNDED =
PEMMOD                 WS-SC-EARN / WS-SC-NW
PEMMOD           END-IF
PEMMOD           IF WS-SE-NW NOT = ZERO
PEMMOD              COMPUTE WS-TAX-WRK2 ROUNDED =
PEMMOD                 WS-SE-EARN / WS-SE-NW
PEMMOD           END-IF
PEMMOD           COMPUTE PAA-ST (SB) ROUNDED = PAA-ST (SB) +
PEMMOD              ((WS-TAX-WRK1 * DA-TAX (SC)) -
PEMMOD              (WS-TAX-WRK2 * DA-TAX (SE)))
PEMMOD        END-IF
PEMMOD     END-IF
03803                                                                   ECS045
03804      IF RR-PRT-ST = 'F'                                           ECS045
03805         IF RR-PE-TAX-AH = 'P'                                     ECS045
03806            COMPUTE PAA-RF (SB) ROUNDED = PAA-RF (SB) +            ECS045
03807               (DA-TAX (SC) - DA-TAX (SE))                         ECS045
03809         ELSE                                                      ECS045
PEMMOD           COMPUTE WS-SC-EARN =
PEMMOD              (DA-ISP (SC) - DA-CNP (SC) - DA-UP (SC))
PEMMOD           COMPUTE WS-SC-NW =
PEMMOD              (DA-ISP (SC) - DA-CNP (SC))
PEMMOD           COMPUTE WS-SE-EARN =
PEMMOD              (DA-ISP (SE) - DA-CNP (SE) - DA-UP (SE))
PEMMOD           COMPUTE WS-SE-NW =
PEMMOD              (DA-ISP (SE) - DA-CNP (SE))
PEMMOD           IF WS-SC-NW NOT = ZERO
PEMMOD              COMPUTE WS-TAX-WRK1 ROUNDED =
PEMMOD                 WS-SC-EARN / WS-SC-NW
PEMMOD           END-IF
PEMMOD           IF WS-SE-NW NOT = ZERO
PEMMOD              COMPUTE WS-TAX-WRK2 ROUNDED =
PEMMOD                 WS-SE-EARN / WS-SE-NW
PEMMOD           END-IF
PEMMOD           COMPUTE PAA-RF (SB) ROUNDED = PAA-RF (SB) +
PEMMOD              ((WS-TAX-WRK1 * DA-TAX (SC)) -
PEMMOD              (WS-TAX-WRK2 * DA-TAX (SE)))
PEMMOD        END-IF
PEMMOD     END-IF
03814      .                                                            ECS045
03815  0570-EXIT.                                                       ECS045
03816      EXIT.                                                        ECS045
03817                                                                   ECS045
03818      EJECT                                                        ECS045
03819 ***********************************************************       ECS045
03820 * (PAB-LMR)  LIFE MORTALITY RESERVE                       *       ECS045
03821 * (PAB-LA)   LIFE CLAIM RESERVE (DUE UNPAID + FUTURE)     *       ECS045
03822 * (PAB-LU)   LIFE CLAIM RESERVE (INCURRED NOT REPORTED)   *       ECS045
03823 * (PAB-IF)   LIFE INFORCE AMOUNT                          *       ECS045
03824 * (PAB-LUP)  LIFE UNEARNED PREMIUM                        *       ECS045
03825 * (PAB-AUP1) A&H UNEARNED PREMIUM (IF ON PAID BASIS)      *       ECS045
03826 * (PAB-AA)   A&H CLAIM RESERVE (DUE UNPAID)               *       ECS045
03827 * (PAB-AU)   A&H CLAIM RESERVE (INCURRED NOT REPORTED)    *       ECS045
03828 * (PAB-AF)   A&H CLAIM RESERVE (FUTURE)                   *       ECS045
03829 * (PAB-AUP2) A&H UNEARNED PREMIUM                         *       ECS045
03830 ***********************************************************       ECS045
03831                                                                   ECS045
03832  0580-ACCUM-PAB.                                                  ECS045
03833 *******************************************************           ECS045
03834 *  LIFE COVERAGE'S IMPACT ON MINIMUM BALANCE ACCOUNT  *           ECS045
03835 *******************************************************           ECS045
03836                                                                   ECS045
03837      COMPUTE PAB-LMR  (SB)  = PAB-LMR      (SB) + DA-MORT (SE).   ECS045
03838      COMPUTE PAB-LA   (SB)  = PAB-LA       (SB)                   ECS045
03839          + DA-CA (SE)   + DA-CF (SE).                             ECS045
03840      COMPUTE PAB-LU   (SB)  = PAB-LU       (SB) + DA-CU   (SE).   ECS045
03841      COMPUTE PAB-IF   (SB)  = PAB-IF       (SB) + DA-IF   (SE).   ECS045
03842                                                                   ECS045
03843      COMPUTE PAB-LUP  (SB)  = PAB-LUP      (SB) + DA-UP   (SE).   ECS045
03844                                                                   ECS045
03845      MULTIPLY DA-UP (SE) BY FAC-1 GIVING WRK-CALC.                ECS045
03846      ADD WRK-CALC                TO PAB-LU (SB).                  ECS045
03847                                                                   ECS045
03848      COMPUTE PAB-LT (SB) = PAB-LT (SB)                            ECS045
03849          + DA-CA (SE)   + DA-CF (SE)   + DA-CU (SE)   + WRK-CALC. ECS045
03850                                                                   ECS045
03851      IF RCT-MORT-SW (RX) NOT = 'N'                                ECS045
03852          COMPUTE PAB-MB (SB) = PAB-MB (SB) + DA-MORT (SE).        ECS045
03853                                                                   ECS045
03854      IF RR-PI-CLM-LF = 'P'                                        ECS045
03855          COMPUTE PAB-MB (SB) = PAB-MB (SB)                        ECS045
03856          + DA-CA (SE)   + DA-CF (SE)   + DA-CU (SE)   + WRK-CALC. ECS045
03857                                                                   ECS045
03858 *******************************************************           ECS045
03859 *  A&H  COVERAGE'S IMPACT ON MINIMUM BALANCE ACCOUNT  *           ECS045
03860 *******************************************************           ECS045
03861                                                                   ECS045
03862      IF RR-PE-AH = 'P'                                            ECS045
03863          COMPUTE PAB-AUP1 (SB)  = PAB-AUP1 (SB) + DA-UP (SF)      ECS045
03864          COMPUTE PAB-MB   (SB)  = PAB-MB   (SB) + DA-UP (SF).     ECS045
03865                                                                   ECS045
03866      COMPUTE PAB-AA   (SB)  = PAB-AA       (SB) + DA-CA   (SF).   ECS045
03867      COMPUTE PAB-AU   (SB)  = PAB-AU       (SB) + DA-CU   (SF).   ECS045
03868      COMPUTE PAB-AF   (SB)  = PAB-AF       (SB) + DA-CF   (SF).   ECS045
03869      COMPUTE PAB-AUP2 (SB)  = PAB-AUP2     (SB) + DA-UP   (SF).   ECS045
03870                                                                   ECS045
03871      MULTIPLY DA-UP (SF) BY FAC-2 GIVING WRK-CALC.                ECS045
03872      ADD WRK-CALC                TO PAB-AU (SB).                  ECS045
03873                                                                   ECS045
03874      COMPUTE PAB-AT (SB) = PAB-AT (SB)                            ECS045
03875          + DA-CA (SF)   + DA-CF (SF)   + DA-CU (SF)   + WRK-CALC. ECS045
03876                                                                   ECS045
03877      IF RR-PI-CLM-AH = 'P'                                        ECS045
03878          COMPUTE PAB-MB (SB) = PAB-MB (SB)                        ECS045
03879          + DA-CA (SF)   + DA-CF (SF)   + DA-CU (SF)   + WRK-CALC. ECS045
03880                                                                   ECS045
03881  0590-EXIT.                                                       ECS045
03882      EXIT.                                                        ECS045
03883                                                                   ECS045
03884      EJECT                                                        ECS045
03885 ******************************************                        ECS045
03886 * (PAA-IC)   INCURRED CLAIMS             *                        ECS045
03887 * (PAA-CPI)  CLAIM PAYMENTS INCURRED     *                        ECS045
03888 * (PAA-CRB)  CLAIM RESERVE (BEGINNING)   *                        ECS045
03889 * (PAA-CRE)  CLAIM RESERVE (ENDING)      *                        ECS045
03890 * (PAA-TD)   TOTAL DEDUCTIONS            *                        ECS045
03891 * (PAA-NB)   NET BALANCE DUE REINSURER   *                        ECS045
03892 ******************************************                        ECS045
03893                                                                   ECS045
03894  0600-COMPUTE-PAA.                                                ECS045
03895      IF SB = (+1 OR +4)                                           ECS045
03896          MULTIPLY PAA-SBUP (SB) BY FAC-1 GIVING WRK-CALC          ECS045
03897          ADD WRK-CALC           TO PAA-CRB (SB)                   ECS045
03898          MULTIPLY PAA-SEUP (SB) BY FAC-1 GIVING WRK-CALC          ECS045
03899          ADD WRK-CALC           TO PAA-CRE (SB).                  ECS045
03900                                                                   ECS045
03901      IF SB = (+2 OR +5)                                           ECS045
03902          MULTIPLY PAA-SBUP (SB) BY FAC-2 GIVING WRK-CALC          ECS045
03903          ADD WRK-CALC           TO PAA-CRB (SB)                   ECS045
03904          MULTIPLY PAA-SEUP (SB) BY FAC-2 GIVING WRK-CALC          ECS045
03905          ADD WRK-CALC           TO PAA-CRE (SB).                  ECS045
03906                                                                   ECS045
03907      COMPUTE PAA-IC (SB) = PAA-IC (SB) +                          ECS045
03908          (PAA-CPI (SB) - PAA-CRB (SB) + PAA-CRE (SB)).            ECS045
03909                                                                   ECS045
03910      COMPUTE PAA-TD (SB) = PAA-TD (SB) + (PAA-RF (SB) +           ECS045
03911          PAA-AC (SB) + PAA-OW (SB) + PAA-ST (SB) +                ECS045
03912          PAA-CRSV (SB) +                                          ECS045
03913          PAA-CP (SB) + PAA-IC (SB) + PAA-AJ (SB)).                ECS045
03914                                                                   ECS045
03915      COMPUTE PAA-NB (SB) = PAA-NB (SB) +                          ECS045
03916          (PAA-RP (SB) - PAA-TD (SB)).                             ECS045
03917                                                                   ECS045
03918      IF SAVE-PRT-ST = 'Y'                                         ECS045
03919          MOVE 'Y'                TO DT-PRT-SW (15)                ECS045
03920                                     DT-PRT-SW-2 (15)              ECS045
03921      ELSE                                                         ECS045
03922          MOVE 'N'                TO DT-PRT-SW (15)                ECS045
03923                                     DT-PRT-SW-2 (15).             ECS045
03924                                                                   ECS045
03925      IF SAVE-PRT-OW = 'Y'                                         ECS045
03926          MOVE 'Y'                TO DT-PRT-SW (14)                ECS045
03927                                     DT-PRT-SW-2 (14)              ECS045
03928      ELSE                                                         ECS045
03929          MOVE 'N'                TO DT-PRT-SW (14)                ECS045
03930                                     DT-PRT-SW-2 (14).             ECS045
03931                                                                   ECS045
03932      IF SAVE-PRT-CRSV = 'Y'  AND                                  ECS045
03933         DTE-CLIENT = 'NCL'                                        ECS045
03934          MOVE 'Y'                TO DT-PRT-SW (16)                ECS045
03935                                     DT-PRT-SW-2 (16)              ECS045
03936      ELSE                                                         ECS045
03937          MOVE 'N'                TO DT-PRT-SW (16)                ECS045
03938                                     DT-PRT-SW-2 (16).             ECS045
03939                                                                   ECS045
03940      IF SAVE-CLAIM-CODE = 'P'                                     ECS045
03941          MOVE 'Y'                TO DT-PRT-SW (8)                 ECS045
03942                                     DT-PRT-SW-2 (8)               ECS045
03943          MOVE 'N'                TO DT-PRT-SW (9)                 ECS045
03944                                     DT-PRT-SW-2 (9)               ECS045
03945                                     DT-PRT-SW (10)                ECS045
03946                                     DT-PRT-SW-2 (10)              ECS045
03947                                     DT-PRT-SW (11)                ECS045
03948                                     DT-PRT-SW-2 (11)              ECS045
03949                                     DT-PRT-SW (12)                ECS045
03950                                     DT-PRT-SW-2 (12).             ECS045
03951      IF SAVE-CLAIM-CODE = 'I'                                     ECS045
03952          MOVE 'N'                TO DT-PRT-SW (8)                 ECS045
03953                                     DT-PRT-SW-2 (8)               ECS045
03954          MOVE 'Y'                TO DT-PRT-SW (9)                 ECS045
03955                                     DT-PRT-SW-2 (9)               ECS045
03956                                     DT-PRT-SW (10)                ECS045
03957                                     DT-PRT-SW-2 (10)              ECS045
03958                                     DT-PRT-SW (11)                ECS045
03959                                     DT-PRT-SW-2 (11)              ECS045
03960                                     DT-PRT-SW (12)                ECS045
03961                                     DT-PRT-SW-2 (12).             ECS045
03962      IF SAVE-CLAIM-CODE = 'X' OR 'Y'                              ECS045
03963          MOVE 'Y'                TO DT-PRT-SW (8)                 ECS045
03964                                     DT-PRT-SW-2 (8)               ECS045
03965                                     DT-PRT-SW (9)                 ECS045
03966                                     DT-PRT-SW-2 (9)               ECS045
03967                                     DT-PRT-SW (10)                ECS045
03968                                     DT-PRT-SW-2 (10)              ECS045
03969                                     DT-PRT-SW (11)                ECS045
03970                                     DT-PRT-SW-2 (11)              ECS045
03971                                     DT-PRT-SW (12)                ECS045
03972                                     DT-PRT-SW-2 (12).             ECS045
03973                                                                   ECS045
03974  0610-EXIT.                                                       ECS045
03975      EXIT.                                                        ECS045
03976                                                                   ECS045
03977      EJECT                                                        ECS045
03978  0640-PRT-DATE-RANGE.                                             ECS045
03979                                                                   ECS045
03980  BBBB-PRT-REINS-SUB.                                              ECS045
03981                                                                   ECS045
03982  DDDD-PRT-REINS-COMPANY-PRIME.                                    ECS045
03983                                                                   ECS045
03984  EEEE-PRT-REINS-COMPANY-SUB.                                      ECS045
03985                                                                   ECS045
03986 ***************************************************************   ECS045
03987 *  ROUTINE 0600-COMPUTE-PAA DERIVES VARIOUS PAA-ACCUMULATORS  *   ECS045
03988 *  FROM THE BASIC PAA ACCUMULATORS WHICH ARE COMPUTED FROM    *   ECS045
03989 *  THE DA-ACCUMULATORS IN THE SORT RECORD.                    *   ECS045
03990 ***************************************************************   ECS045
03991                                                                   ECS045
03992 **** COMPUTE PAA'S FOR LIFE AND AH CURRENT MONTH ****             ECS045
03993                                                                   ECS045
03994      PERFORM 0600-COMPUTE-PAA THRU 0610-EXIT VARYING SB           ECS045
03995              FROM +1 BY +1 UNTIL SB GREATER +2.                   ECS045
03996                                                                   ECS045
03997 **** COMPUTE PAA'S FOR LIFE AND AH YEAR TO DATE  ****             ECS045
03998                                                                   ECS045
03999      PERFORM 0600-COMPUTE-PAA THRU 0610-EXIT VARYING SB           ECS045
04000              FROM +4 BY +1 UNTIL SB GREATER +5.                   ECS045
04001                                                                   ECS045
04002 **** COMPUTE PAA'S FOR LIFE AND AH INCEPTION-TO-DATE ****         ECS045
04003                                                                   ECS045
04004      PERFORM 0600-COMPUTE-PAA THRU 0610-EXIT VARYING SB           ECS045
04005              FROM +7 BY +1 UNTIL SB GREATER +8.                   ECS045
04006                                                                   ECS045
04007 ** SB(1) PAA-ACCUMS (DATE RANGE TOTS / LIFE   TOTALS CURR MNTH) **ECS045
04008 ** SC(3) PAA-ACCUMS (DATE RANGE TOTS / COMBINE LF-AH CURR MNTH) **ECS045
04009      MOVE +1                     TO SB.                           ECS045
04010      MOVE +3                     TO SC.                           ECS045
04011      PERFORM 0520-ADD-PAA THRU 0530-EXIT.                         ECS045
04012                                                                   ECS045
04013 ** SB(2) PAA-ACCUMS (DATE RANGE TOTS / AH     TOTALS CURR MNTH) **ECS045
04014 ** SC(3) PAA-ACCUMS (DATE RANGE TOTS / COMBINE LF-AH CURR MNTH) **ECS045
04015      MOVE +2                     TO SB.                           ECS045
04016      MOVE +3                     TO SC.                           ECS045
04017      PERFORM 0520-ADD-PAA THRU 0530-EXIT.                         ECS045
04018                                                                   ECS045
04019 ** SB(4) PAA-ACCUMS (DATE RANGE TOTS / LIFE   TOTALS Y.T.D.   ) **ECS045
04020 ** SC(6) PAA-ACCUMS (DATE RANGE TOTS / COMBINE LF-AH Y.T.D.   ) **ECS045
04021      MOVE +4                     TO SB.                           ECS045
04022      MOVE +6                     TO SC.                           ECS045
04023      PERFORM 0520-ADD-PAA THRU 0530-EXIT.                         ECS045
04024                                                                   ECS045
04025 ** SB(5) PAA-ACCUMS (DATE RANGE TOTS / AH     TOTALS Y.T.D.   ) **ECS045
04026 ** SC(6) PAA-ACCUMS (DATE RANGE TOTS / COMBINE LF-AH Y.T.D.   ) **ECS045
04027      MOVE +5                     TO SB.                           ECS045
04028      MOVE +6                     TO SC.                           ECS045
04029      PERFORM 0520-ADD-PAA THRU 0530-EXIT.                         ECS045
04030                                                                   ECS045
04031 ** SB(7) PAA-ACCUMS (DATE RANGE TOTS / LIFE   TOTALS I.T.D.   ) **ECS045
04032 ** SC(9) PAA-ACCUMS (DATE RANGE TOTS / COMBINE LF-AH I.T.D.   ) **ECS045
04033      MOVE +7                     TO SB.                           ECS045
04034      MOVE +9                     TO SC.                           ECS045
04035      PERFORM 0520-ADD-PAA THRU 0530-EXIT.                         ECS045
04036                                                                   ECS045
04037 ** SB(8) PAA-ACCUMS (DATE RANGE TOTS / AH     TOTALS I.T.D.   ) **ECS045
04038 ** SC(9) PAA-ACCUMS (DATE RANGE TOTS / COMBINE LF-AH I.T.D.   ) **ECS045
04039      MOVE +8                     TO SB.                           ECS045
04040      MOVE +9                     TO SC.                           ECS045
04041      PERFORM 0520-ADD-PAA THRU 0530-EXIT.                         ECS045
04042                                                                   ECS045
04043 ** CHECK TO SEE IF DATE RANGE BREAKS ARE REQUESTED **             ECS045
04044                                                                   ECS045
04045      IF H1-RUN-CODE = 'B' OR 'D' OR 'E'                           ECS045
04046          MOVE 'Y'                TO DT-PRT-SW (8)                 ECS045
04047                                     DT-PRT-SW-2 (8)               ECS045
04048                                     DT-PRT-SW (9)                 ECS045
04049                                     DT-PRT-SW-2 (9)               ECS045
04050                                     DT-PRT-SW (10)                ECS045
04051                                     DT-PRT-SW-2 (10)              ECS045
04052                                     DT-PRT-SW (11)                ECS045
04053                                     DT-PRT-SW-2 (11)              ECS045
04054                                     DT-PRT-SW (12)                ECS045
04055                                     DT-PRT-SW-2 (12)              ECS045
04056                                     DT-PRT-SW (14)                ECS045
04057                                     DT-PRT-SW-2 (14)              ECS045
04058                                     DT-PRT-SW (15)                ECS045
04059                                     DT-PRT-SW-2 (15)              ECS045
04060          GO TO 0645-DO-NOT-BYPASS-BREAK.                          ECS045
04061                                                                   ECS045
04062      IF DTE-PGM-OPT = '7' OR '8'                                  ECS045
04063          GO TO 0650-SKIP-DATE-RANGE-PRT.                          ECS045
04064                                                                   ECS045
04065      IF DTE-CLIENT = 'HER'  AND                                   ECS045
04066         H1-RUN-CODE = 'A'                                         ECS045
04067          IF SAVE-R-B-REI-COMP-PRIME GREATER THAN '799'  AND       ECS045
04068             SAVE-R-B-REI-COMP-PRIME LESS    THAN '900'            ECS045
04069              NEXT SENTENCE                                        ECS045
04070          ELSE                                                     ECS045
04071              GO TO 0650-SKIP-DATE-RANGE-PRT.                      ECS045
04072                                                                   ECS045
04073      IF DTE-CLIENT = 'LAP' AND                                    ECS045
04074         SAVE-R-B-REI-COMP NUMERIC                                 ECS045
04075           GO TO 0650-SKIP-DATE-RANGE-PRT.                         ECS045
04076                                                                   ECS045
04077      IF DTE-CLIENT = 'NCL' AND                                    ECS045
04078         H1-RUN-CODE = 'A'                                         ECS045
04079           GO TO 0650-SKIP-DATE-RANGE-PRT.                         ECS045
04080                                                                   ECS045
04081  0645-DO-NOT-BYPASS-BREAK.                                        ECS045
04082                                                                   ECS045
04083 *    IF DTE-CLIENT = 'HER'  AND                                   ECS045
04084 *       H1-RUN-CODE = 'E'                                         ECS045
04085 *        GO TO 0650-SKIP-DATE-RANGE-PRT.                          ECS045
04086                                                                   ECS045
04087      MOVE '1'                    TO HDR-SW.                       ECS045
04088      PERFORM 0900-HDR-RTN THRU 0910-EXIT.                         ECS045
04089                                                                   ECS045
04090 ** PRODUCE FIRST 19 LINES OF REPORT BREAK USING PAA ACCUMULATORS *ECS045
04091                                                                   ECS045
04092      MOVE +1                     TO SA.                           ECS045
04093                                                                   ECS045
04094      PERFORM 0860-PRT-DTL-1 THRU 0870-EXIT VARYING SG             ECS045
04095                    FROM +1 BY +1 UNTIL SG GREATER +19.            ECS045
04096                                                                   ECS045
PEMMOD     MOVE ALL '_'                TO PRT
PEMMOD                                    PRT-2
PEMMOD     MOVE ' '                    TO X
PEMMOD     PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
PEMMOD                                                                  ECS045
04097      MOVE HDR-8                  TO PRT.                          ECS045
04098      MOVE HDR-8-2                TO PRT-2.                        ECS045
PEMMOD*    MOVE '0'                    TO X.                            ECS045
PEMMOD     MOVE ' '                    TO X.                            ECS045
04100      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
04101                                                                   ECS045
04102 ** PRODUCE LAST 19 LINES OF REPORT BREAK USING PAB ACCUMULATORS **ECS045
04103                                                                   ECS045
04104      IF DTE-FMT-OPT EQUAL '2'                                     ECS045
04105          PERFORM 0882-PRT-DTL-2 THRU 0892-EXIT VARYING SG         ECS045
04106                 FROM +20 BY +1 UNTIL SG GREATER +38               ECS045
04107      ELSE                                                         ECS045
04108          PERFORM 0880-PRT-DTL-2 THRU 0890-EXIT VARYING SG         ECS045
04109                 FROM +20 BY +1 UNTIL SG GREATER +38.              ECS045
04110                                                                   ECS045
04111  0650-SKIP-DATE-RANGE-PRT.                                        ECS045
04112 ** SB(1) PAA-ACCUMS (DATE RANGE TOTS)         **                  ECS045
04113 ** SC(10) PAA-ACCUMS (REINSURANCE COMPANY SUB) **                 ECS045
04114      MOVE +1                     TO SB.                           ECS045
04115      MOVE +10                    TO SC.                           ECS045
04116      PERFORM 0520-ADD-PAA THRU 0530-EXIT 9 TIMES.                 ECS045
04117                                                                   ECS045
04118 ** SB(1) PAB-ACCUMS (DATE RANGE TOTS)         **                  ECS045
04119 ** SC(7) PAB-ACCUMS (REINSURANCE COMPANY SUB) **                  ECS045
04120      MOVE +1                     TO SB.                           ECS045
04121      MOVE +7                     TO SC.                           ECS045
04122      PERFORM 0540-ADD-PAB THRU 0550-EXIT 6 TIMES.                 ECS045
04123                                                                   ECS045
04124 ** ZERO OUT DATE RANGE ACCUMULATORS IN PAA AND PAB **             ECS045
04125                                                                   ECS045
04126      PERFORM 0440-ZERO-PAA THRU 0450-EXIT VARYING SA              ECS045
04127              FROM +1 BY +1 UNTIL SA GREATER +9.                   ECS045
04128      PERFORM 0460-ZERO-PAB THRU 0470-EXIT VARYING SA              ECS045
04129              FROM +1 BY +1 UNTIL SA GREATER +6.                   ECS045
04130                                                                   ECS045
04131  EEEE-PRT-REINS-COMPANY-SUB-X.                                    ECS045
04132      EXIT.                                                        ECS045
04133                                                                   ECS045
04134  DDDD-PRT-REINS-COMPANY-PRIME-X.                                  ECS045
04135      EXIT.                                                        ECS045
04136                                                                   ECS045
04137  BBBB-PRT-REINS-SUB-X.                                            ECS045
04138      EXIT.                                                        ECS045
04139                                                                   ECS045
04140  0660-EXIT.                                                       ECS045
04141      EXIT.                                                        ECS045
04142                                                                   ECS045
04143      EJECT                                                        ECS045
04144  0670-PRT-REINS-SUB.                                              ECS045
04145                                                                   ECS045
04146  BBBB-PRT-ST.                                                     ECS045
04147                                                                   ECS045
04148  DDDD-PRT-CARR.                                                   ECS045
04149                                                                   ECS045
04150  EEEE-PRT-REINS-COMPANY-PRIME.                                    ECS045
04151                                                                   ECS045
04152 ** CHECK TO SEE IF REINS SUB BREAKS ARE REQUESTED **              ECS045
04153                                                                   ECS045
04154      IF H1-RUN-CODE = 'B' OR 'D' OR 'E'                           ECS045
04155          MOVE 'Y'                TO DT-PRT-SW (8)                 ECS045
04156                                     DT-PRT-SW-2 (8)               ECS045
04157                                     DT-PRT-SW (9)                 ECS045
04158                                     DT-PRT-SW-2 (9)               ECS045
04159                                     DT-PRT-SW (10)                ECS045
04160                                     DT-PRT-SW-2 (10)              ECS045
04161                                     DT-PRT-SW (11)                ECS045
04162                                     DT-PRT-SW-2 (11)              ECS045
04163                                     DT-PRT-SW (12)                ECS045
04164                                     DT-PRT-SW-2 (12)              ECS045
04165                                     DT-PRT-SW (14)                ECS045
04166                                     DT-PRT-SW-2 (14)              ECS045
04167                                     DT-PRT-SW (15)                ECS045
04168                                     DT-PRT-SW-2 (15)              ECS045
04169          GO TO 0675-DO-NOT-BYPASS-BREAK.                          ECS045
04170                                                                   ECS045
04171      IF DTE-PGM-OPT = '7' OR '8'                                  ECS045
04172          GO TO 0680-SKIP-REIN-SUB-PRT.                            ECS045
04173                                                                   ECS045
04174 *    IF DTE-CLIENT = 'HER'  AND                                   ECS045
04175 *       H1-RUN-CODE = 'A'                                         ECS045
04176 *        IF SAVE-R-B-REI-COMP-PRIME GREATER THAN '799'  AND       ECS045
04177 *           SAVE-R-B-REI-COMP-PRIME LESS    THAN '900'            ECS045
04178 *            NEXT SENTENCE                                        ECS045
04179 *        ELSE                                                     ECS045
04180 *            GO TO 0680-SKIP-REIN-SUB-PRT.                        ECS045
04181                                                                   ECS045
04182      IF DTE-CLIENT = 'LAP' AND                                    ECS045
04183         SAVE-R-B-REI-COMP NUMERIC                                 ECS045
04184          GO TO 0680-SKIP-REIN-SUB-PRT.                            ECS045
04185                                                                   ECS045
04186  0675-DO-NOT-BYPASS-BREAK.                                        ECS045
04187                                                                   ECS045
04188      MOVE '2'                    TO HDR-SW.                       ECS045
04189      PERFORM 0900-HDR-RTN THRU 0910-EXIT.                         ECS045
04190                                                                   ECS045
04191 ** PRODUCE FIRST 19 LINES OF REPORT BREAK USING PAA ACCUMULATORS *ECS045
04192                                                                   ECS045
04193      MOVE +2                     TO SA.                           ECS045
04194      PERFORM 0860-PRT-DTL-1 THRU 0870-EXIT VARYING SG             ECS045
04195              FROM +1 BY +1  UNTIL SG GREATER +19.                 ECS045
PEMMOD     MOVE ALL '_'                TO PRT
PEMMOD                                    PRT-2
PEMMOD     MOVE ' '                    TO X
PEMMOD     PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
PEMMOD                                                                  ECS045
04196      MOVE HDR-8                  TO PRT.                          ECS045
04197      MOVE HDR-8-2                TO PRT-2.                        ECS045
PEMMOD*    MOVE '0'                    TO X.                            ECS045
PEMMOD     MOVE ' '                    TO X.                            ECS045
04199      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
04200                                                                   ECS045
04201 ** PRODUCE LAST 19 LINES OF REPORT BREAK USING PAB ACCUMULATORS **ECS045
04202                                                                   ECS045
04203      IF DTE-FMT-OPT EQUAL '2'                                     ECS045
04204          PERFORM 0882-PRT-DTL-2 THRU 0892-EXIT VARYING SG         ECS045
04205                 FROM +20 BY +1 UNTIL SG GREATER +38               ECS045
04206      ELSE                                                         ECS045
04207          PERFORM 0880-PRT-DTL-2 THRU 0890-EXIT VARYING SG         ECS045
04208                 FROM +20 BY +1 UNTIL SG GREATER +38.              ECS045
020612
020612     IF DTE-FMT-OPT EQUAL '2' AND
020612       (DOA-TOTAL-TYPE EQUAL '1' OR '2' OR '3' OR '4')
020612         WRITE DATA-OUT-AHL-REC FROM DATA-OUT-AHL-RECORD
020612         PERFORM 3400-INIT-DATA-OUT-AHL THRU 3499-EXIT
020612     END-IF.
04209                                                                   ECS045
04210  0680-SKIP-REIN-SUB-PRT.                                          ECS045
04211 ** SB(10) PAA-ACCUMS (REINSURANCE COMPANY SUB TOTS) **            ECS045
04212 ** SC(19) PAA-ACCUMS (ACCOUNT LEVEL SUB TOTS)       **            ECS045
04213      MOVE +10                    TO SB.                           ECS045
04214      MOVE +19                    TO SC.                           ECS045
04215      PERFORM 0520-ADD-PAA THRU 0530-EXIT 9 TIMES.                 ECS045
04216                                                                   ECS045
04217 ** SB(7)  PAB-ACCUMS (REINSURANCE COMPANY SUB TOTS)  **           ECS045
04218 ** SC(12) PAB-ACCUMS (ACCOUNT LEVEL SUB TOTS)        **           ECS045
04219      MOVE +7                     TO SB.                           ECS045
04220      MOVE +13                    TO SC.                           ECS045
04221      PERFORM 0540-ADD-PAB THRU 0550-EXIT 6 TIMES.                 ECS045
04222                                                                   ECS045
04223 ** ZERO OUT DATE RANGE ACCUMULATORS IN PAA AND PAB **             ECS045
04224                                                                   ECS045
04225      PERFORM 0440-ZERO-PAA THRU 0450-EXIT VARYING SA              ECS045
04226              FROM +10 BY +1  UNTIL SA GREATER +18.                ECS045
04227      PERFORM 0460-ZERO-PAB THRU 0470-EXIT VARYING SA              ECS045
04228              FROM +7 BY +1  UNTIL SA GREATER +12.                 ECS045
04229                                                                   ECS045
04230  EEEE-PRT-REINS-COMPANY-PRIME-X.                                  ECS045
04231      EXIT.                                                        ECS045
04232                                                                   ECS045
04233  DDDD-PRT-CARR-X.                                                 ECS045
04234      EXIT.                                                        ECS045
04235                                                                   ECS045
04236  BBBB-PRT-ST-X.                                                   ECS045
04237      EXIT.                                                        ECS045
04238                                                                   ECS045
04239  0690-EXIT.                                                       ECS045
04240      EXIT.                                                        ECS045
04241                                                                   ECS045
04242      EJECT                                                        ECS045
04243  0700-PRT-ACCT.                                                   ECS045
04244                                                                   ECS045
04245  BBBB-PRT-REINS-COMPANY-PRIME.                                    ECS045
04246                                                                   ECS045
04247  DDDD-PRT-REINS-GROUP-D.                                          ECS045
04248                                                                   ECS045
04249  EEEE-FINAL-TOTALS.                                               ECS045
04250                                                                   ECS045
04251 ** CHECK TO SEE IF ACCOUNT LEVEL BREAKS ARE REQUESTED **          ECS045
04252                                                                   ECS045
04253      IF H1-RUN-CODE = 'B' OR 'D' OR 'E'                           ECS045
04254          MOVE 'Y'                TO DT-PRT-SW (8)                 ECS045
04255                                     DT-PRT-SW-2 (8)               ECS045
04256                                     DT-PRT-SW (9)                 ECS045
04257                                     DT-PRT-SW-2 (9)               ECS045
04258                                     DT-PRT-SW (10)                ECS045
04259                                     DT-PRT-SW-2 (10)              ECS045
04260                                     DT-PRT-SW (11)                ECS045
04261                                     DT-PRT-SW-2 (11)              ECS045
04262                                     DT-PRT-SW (12)                ECS045
04263                                     DT-PRT-SW-2 (12)              ECS045
04264                                     DT-PRT-SW (14)                ECS045
04265                                     DT-PRT-SW-2 (14)              ECS045
04266                                     DT-PRT-SW (15)                ECS045
04267                                     DT-PRT-SW-2 (15)              ECS045
04268          GO TO 0705-DO-NOT-SKIP-BREAK.                            ECS045
04269                                                                   ECS045
04270      IF DTE-PGM-OPT = '7' OR '8'                                  ECS045
04271        IF DTE-CLIENT NOT = 'NCL'                                  ECS045
04272          GO TO 0710-SKIP-ACCT-PRT.                                ECS045
04273                                                                   ECS045
04274      IF DTE-CLIENT = 'LAP' AND                                    ECS045
04275         SAVE-R-B-REI-COMP NUMERIC                                 ECS045
04276          GO TO 0710-SKIP-ACCT-PRT.                                ECS045
04277                                                                   ECS045
04278  0705-DO-NOT-SKIP-BREAK.                                          ECS045
04279      MOVE '3'                    TO HDR-SW.                       ECS045
04280      PERFORM 0900-HDR-RTN THRU 0910-EXIT.                         ECS045
04281                                                                   ECS045
04282 ** PRODUCE FIRST 19 LINES OF REPORT BREAK USING PAA ACCUMULATORS *ECS045
04283                                                                   ECS045
04284      MOVE +3                     TO SA.                           ECS045
04285      PERFORM 0860-PRT-DTL-1 THRU 0870-EXIT VARYING SG             ECS045
04286              FROM +1 BY +1  UNTIL SG GREATER +19.                 ECS045
PEMMOD     MOVE ALL '_'                TO PRT
PEMMOD                                    PRT-2
PEMMOD     MOVE ' '                    TO X
PEMMOD     PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
PEMMOD                                                                  ECS045
04287      MOVE HDR-8                  TO PRT.                          ECS045
04288      MOVE HDR-8-2                TO PRT-2.                        ECS045
PEMMOD*    MOVE '0'                    TO X.                            ECS045
PEMMOD     MOVE ' '                    TO X.                            ECS045
04290      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
04291                                                                   ECS045
04292 ** PRODUCE LAST 19 LINES OF REPORT BREAK USING PAB ACCUMULATORS **ECS045
04293                                                                   ECS045
04294      IF DTE-FMT-OPT EQUAL '2'                                     ECS045
04295          PERFORM 0882-PRT-DTL-2 THRU 0892-EXIT VARYING SG         ECS045
04296                 FROM +20 BY +1 UNTIL SG GREATER +38               ECS045
04297      ELSE                                                         ECS045
04298          PERFORM 0880-PRT-DTL-2 THRU 0890-EXIT VARYING SG         ECS045
04299                 FROM +20 BY +1 UNTIL SG GREATER +38.              ECS045
061008
061008     IF DTE-FMT-OPT EQUAL '2' AND
061008       (DO-TOTAL-TYPE EQUAL '1' OR '2' OR '3')
061008         WRITE DATA-OUT-REC FROM DATA-OUT-RECORD
061008         PERFORM 3300-INIT-DATA-OUT THRU 3399-EXIT
061008     END-IF.
04300                                                                   ECS045
04301  0710-SKIP-ACCT-PRT.                                              ECS045
04302 ** SB(19) PAA-ACCUMS (ACCOUNT LEVEL SUB TOTS)       **            ECS045
04303 ** SC(28) PAA-ACCUMS (STATE LEVEL SUB TOTS)         **            ECS045
04304      MOVE +19                    TO SB.                           ECS045
04305      MOVE +28                    TO SC.                           ECS045
04306      PERFORM 0520-ADD-PAA THRU 0530-EXIT 9 TIMES.                 ECS045
04307                                                                   ECS045
04308 ** SB(13) PAB-ACCUMS (ACCOUNT LEVEL SUB TOTS)        **           ECS045
04309 ** SC(19) PAB-ACCUMS (STATE LEVEL SUB TOTS)          **           ECS045
04310      MOVE +13                    TO SB.                           ECS045
04311      MOVE +19                    TO SC.                           ECS045
04312      PERFORM 0540-ADD-PAB THRU 0550-EXIT 6 TIMES.                 ECS045
04313                                                                   ECS045
04314 ** ZERO OUT ACCOUNT RANGE ACCUMULATORS IN PAA AND PAB **          ECS045
04315                                                                   ECS045
04316      PERFORM 0440-ZERO-PAA THRU 0450-EXIT VARYING SA              ECS045
04317                      FROM +1 BY +1  UNTIL SA GREATER +27.         ECS045
04318                                                                   ECS045
04319      PERFORM 0460-ZERO-PAB THRU 0470-EXIT VARYING SA              ECS045
04320                      FROM +13 BY +1  UNTIL SA GREATER +18.        ECS045
04321                                                                   ECS045
04322  EEEE-FINAL-TOTALS-X.                                             ECS045
04323      EXIT.                                                        ECS045
04324                                                                   ECS045
04325  DDDD-PRT-REINS-GROUP-D-X.                                        ECS045
04326      EXIT.                                                        ECS045
04327                                                                   ECS045
04328  BBBB-PRT-REINS-COMPANY-PRIME-X.                                  ECS045
04329      EXIT.                                                        ECS045
04330                                                                   ECS045
04331  0720-EXIT.                                                       ECS045
04332      EXIT.                                                        ECS045
04333                                                                   ECS045
04334      EJECT                                                        ECS045
04335  0730-PRT-ST.                                                     ECS045
04336                                                                   ECS045
04337  BBBB-PRT-REINS-GROUP-B.                                          ECS045
04338                                                                   ECS045
04339  DDDD-FINAL-TOTALS.                                               ECS045
04340 ** CHECK TO SEE IF STATE LEVEL BREAKS ARE REQUESTED **            ECS045
04341                                                                   ECS045
04342      MOVE 'Y'                    TO DT-PRT-SW (8)                 ECS045
04343                                     DT-PRT-SW-2 (8)               ECS045
04344                                     DT-PRT-SW (9)                 ECS045
04345                                     DT-PRT-SW-2 (9)               ECS045
04346                                     DT-PRT-SW (10)                ECS045
04347                                     DT-PRT-SW-2 (10)              ECS045
04348                                     DT-PRT-SW (11)                ECS045
04349                                     DT-PRT-SW-2 (11)              ECS045
04350                                     DT-PRT-SW (12)                ECS045
04351                                     DT-PRT-SW-2 (12)              ECS045
04352                                     DT-PRT-SW (14)                ECS045
04353                                     DT-PRT-SW-2 (14)              ECS045
04354                                     DT-PRT-SW (15)                ECS045
04355                                     DT-PRT-SW-2 (15).             ECS045
04356                                                                   ECS045
04357      MOVE '4'                    TO HDR-SW.                       ECS045
04358      PERFORM 0900-HDR-RTN THRU 0910-EXIT.                         ECS045
04359                                                                   ECS045
04360 ** PRODUCE FIRST 19 LINES OF REPORT BREAK USING PAA ACCUMULATORS *ECS045
04361                                                                   ECS045
04362      MOVE +4                     TO SA.                           ECS045
04363      PERFORM 0860-PRT-DTL-1 THRU 0870-EXIT VARYING SG             ECS045
04364              FROM +1 BY +1  UNTIL SG GREATER +19.                 ECS045
PEMMOD     MOVE ALL '_'                TO PRT
PEMMOD                                    PRT-2
PEMMOD     MOVE ' '                    TO X
PEMMOD     PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
PEMMOD                                                                  ECS045
04365      MOVE HDR-8                  TO PRT.                          ECS045
04366      MOVE HDR-8-2                TO PRT-2.                        ECS045
PEMMOD*    MOVE '0'                    TO X.                            ECS045
PEMMOD     MOVE ' '                    TO X.                            ECS045
04368      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
04369                                                                   ECS045
04370 ** PRODUCE LAST 19 LINES OF REPORT BREAK USING PAB ACCUMULATORS **ECS045
04371                                                                   ECS045
04372      IF DTE-FMT-OPT EQUAL '2'                                     ECS045
04373          PERFORM 0882-PRT-DTL-2 THRU 0892-EXIT VARYING SG         ECS045
04374                 FROM +20 BY +1 UNTIL SG GREATER +38               ECS045
04375      ELSE                                                         ECS045
04376          PERFORM 0880-PRT-DTL-2 THRU 0890-EXIT VARYING SG         ECS045
04377                 FROM +20 BY +1 UNTIL SG GREATER +38.              ECS045
020612
020612     IF DTE-FMT-OPT EQUAL '2' AND
020612       (DOA-TOTAL-TYPE EQUAL '1' OR '2' OR '3' OR '4')
020612         WRITE DATA-OUT-AHL-REC FROM DATA-OUT-AHL-RECORD
020612         PERFORM 3400-INIT-DATA-OUT-AHL THRU 3499-EXIT
020612     END-IF.
04378                                                                   ECS045
04379  0740-SKIP-ST-PRT.                                                ECS045
04380 ** SB(28) PAA-ACCUMS (STATE LEVEL SUB TOTS)         **            ECS045
04381 ** SC(37) PAA-ACCUMS (CERT GROUP LEVEL SUB TOTS)    **            ECS045
04382      MOVE +28                    TO SB.                           ECS045
04383      MOVE +37                    TO SC.                           ECS045
04384      PERFORM 0520-ADD-PAA THRU 0530-EXIT 9 TIMES.                 ECS045
04385                                                                   ECS045
04386 ** SB(19) PAB-ACCUMS (STATE LEVEL SUB TOTS)          **           ECS045
04387 ** SC(25) PAB-ACCUMS (CERT GROUP LEVEL SUB TOTS)     **           ECS045
04388      MOVE +19                    TO SB.                           ECS045
04389      MOVE +25                    TO SC.                           ECS045
04390      PERFORM 0540-ADD-PAB THRU 0550-EXIT 6 TIMES.                 ECS045
04391                                                                   ECS045
04392 ** ZERO OUT STATE ACCUMULATORS IN PAA AND PAB **                  ECS045
04393                                                                   ECS045
04394      PERFORM 0440-ZERO-PAA THRU 0450-EXIT VARYING SA              ECS045
04395              FROM +1 BY +1 UNTIL SA GREATER +36.                  ECS045
04396                                                                   ECS045
04397      PERFORM 0460-ZERO-PAB THRU 0470-EXIT VARYING SA              ECS045
04398              FROM +1 BY +1 UNTIL SA GREATER +24.                  ECS045
04399                                                                   ECS045
04400  DDDD-FINAL-TOTALS-X.                                             ECS045
04401      EXIT.                                                        ECS045
04402                                                                   ECS045
04403  BBBB-PRT-REINS-GROUP-B-X.                                        ECS045
04404      EXIT.                                                        ECS045
04405                                                                   ECS045
04406  0750-EXIT.                                                       ECS045
04407      EXIT.                                                        ECS045
04408                                                                   ECS045
04409      EJECT                                                        ECS045
04410  0760-PRT-CERT-GROUP.                                             ECS045
04411                                                                   ECS045
04412  BBBB-PRT-CARR.                                                   ECS045
04413                                                                   ECS045
04414 ** CHECK TO SEE IF CERTGROUP LEVEL BREAKS ARE REQUESTED **        ECS045
04415                                                                   ECS045
04416      IF H1-RUN-CODE = 'B'                                         ECS045
04417          GO TO 0765-DO-NOT-SKIP-BREAK.                            ECS045
04418                                                                   ECS045
04419  0765-DO-NOT-SKIP-BREAK.                                          ECS045
04420      MOVE '5'                    TO HDR-SW.                       ECS045
04421      PERFORM 0900-HDR-RTN THRU 0910-EXIT.                         ECS045
04422                                                                   ECS045
04423 ** PRODUCE FIRST 19 LINES OF REPORT BREAK USING PAA ACCUMULATORS *ECS045
04424                                                                   ECS045
04425      MOVE +5                     TO SA.                           ECS045
04426      PERFORM 0860-PRT-DTL-1 THRU 0870-EXIT VARYING SG             ECS045
04427              FROM +1 BY +1 UNTIL SG GREATER +19.                  ECS045
PEMMOD     MOVE ALL '_'                TO PRT
PEMMOD                                    PRT-2
PEMMOD     MOVE ' '                    TO X
PEMMOD     PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
PEMMOD                                                                  ECS045
04428      MOVE HDR-8                  TO PRT.                          ECS045
04429      MOVE HDR-8-2                TO PRT-2.                        ECS045
PEMMOD*    MOVE '0'                    TO X.                            ECS045
PEMMOD     MOVE ' '                    TO X.                            ECS045
04431      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
04432                                                                   ECS045
04433 ** PRODUCE LAST 19 LINES OF REPORT BREAK USING PAB ACCUMULATORS **ECS045
04434                                                                   ECS045
04435                                                                   ECS045
04436      IF DTE-FMT-OPT EQUAL '2'                                     ECS045
04437          PERFORM 0882-PRT-DTL-2 THRU 0892-EXIT VARYING SG         ECS045
04438                 FROM +20 BY +1 UNTIL SG GREATER +38               ECS045
04439      ELSE                                                         ECS045
04440          PERFORM 0880-PRT-DTL-2 THRU 0890-EXIT VARYING SG         ECS045
04441                 FROM +20 BY +1 UNTIL SG GREATER +38.              ECS045
04442                                                                   ECS045
04443  0770-SKIP-CERT-GROUP-PRT.                                        ECS045
04444 ** SB(37) PAA-ACCUMS (CERT GROUP LEVEL SUB TOTS)    **            ECS045
04445 ** SC(46) PAA-ACCUMS (CARRIER SUB TOTS)             **            ECS045
04446      MOVE +37                    TO SB.                           ECS045
04447      MOVE +46                    TO SC.                           ECS045
04448      PERFORM 0520-ADD-PAA THRU 0530-EXIT 9 TIMES.                 ECS045
04449                                                                   ECS045
04450 ** SB(25) PAA-ACCUMS (CERT GROUP LEVEL SUB TOTS)    **            ECS045
04451 ** SC(31) PAA-ACCUMS (CARRIER SUB TOTS)             **            ECS045
04452      MOVE +25                    TO SB.                           ECS045
04453      MOVE +31                    TO SC.                           ECS045
04454      PERFORM 0540-ADD-PAB THRU 0550-EXIT 6 TIMES.                 ECS045
04455                                                                   ECS045
04456 ** ZERO OUT CERT GROUP ACCUMULATORS IN PAA AND PAB **             ECS045
04457                                                                   ECS045
04458      PERFORM 0440-ZERO-PAA THRU 0450-EXIT VARYING SA              ECS045
04459              FROM +2 BY +1  UNTIL SA GREATER +45.                 ECS045
04460                                                                   ECS045
04461      PERFORM 0460-ZERO-PAB THRU 0470-EXIT VARYING SA              ECS045
04462              FROM +1 BY +1 UNTIL SA GREATER +30.                  ECS045
04463                                                                   ECS045
04464  BBBB-PRT-CARR-X.                                                 ECS045
04465      EXIT.                                                        ECS045
04466                                                                   ECS045
04467  0780-EXIT.                                                       ECS045
04468      EXIT.                                                        ECS045
04469                                                                   ECS045
04470      EJECT                                                        ECS045
04471  0790-PRT-CARR.                                                   ECS045
04472                                                                   ECS045
04473  BBBB-FINAL-TOTALS.                                               ECS045
04474                                                                   ECS045
04475 ** CHECK TO SEE IF CARRIER LEVEL BREAKS ARE REQUESTED **          ECS045
04476                                                                   ECS045
04477      MOVE '6'                    TO HDR-SW.                       ECS045
04478      PERFORM 0900-HDR-RTN THRU 0910-EXIT.                         ECS045
04479                                                                   ECS045
04480 ** PRODUCE FIRST 19 LINES OF REPORT BREAK USING PAA ACCUMULATORS *ECS045
04481                                                                   ECS045
04482      MOVE +6                     TO SA.                           ECS045
04483      PERFORM 0860-PRT-DTL-1 THRU 0870-EXIT VARYING SG             ECS045
04484              FROM +1 BY +1  UNTIL SG GREATER +19.                 ECS045
PEMMOD     MOVE ALL '_'                TO PRT
PEMMOD                                    PRT-2
PEMMOD     MOVE ' '                    TO X
PEMMOD     PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
PEMMOD                                                                  ECS045
04485      MOVE HDR-8                  TO PRT.                          ECS045
04486      MOVE HDR-8-2                TO PRT-2.                        ECS045
PEMMOD*    MOVE '0'                    TO X.                            ECS045
PEMMOD     MOVE ' '                    TO X.                            ECS045
04488      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
04489                                                                   ECS045
04490 ** PRODUCE LAST 19 LINES OF REPORT BREAK USING PAB ACCUMULATORS **ECS045
04491                                                                   ECS045
04492      IF DTE-FMT-OPT EQUAL '2'                                     ECS045
04493          PERFORM 0882-PRT-DTL-2 THRU 0892-EXIT VARYING SG         ECS045
04494                 FROM +20 BY +1 UNTIL SG GREATER +38               ECS045
04495      ELSE                                                         ECS045
04496          PERFORM 0880-PRT-DTL-2 THRU 0890-EXIT VARYING SG         ECS045
04497                 FROM +20 BY +1 UNTIL SG GREATER +38.              ECS045
04498                                                                   ECS045
04499  0795-SKIP-CARR-PRT.                                              ECS045
04500 ** SB(46) PAA-ACCUMS (CARRIER SUB TOTS)             **            ECS045
04501 ** SC(55) PAA-ACCUMS (REINS GROUP-A TOTS)           **            ECS045
04502      MOVE +46                    TO SB.                           ECS045
04503      MOVE +55                    TO SC.                           ECS045
04504      PERFORM 0520-ADD-PAA THRU 0530-EXIT 9 TIMES.                 ECS045
04505                                                                   ECS045
04506 ** SB(31) PAA-ACCUMS (CARRIER SUB TOTS)             **            ECS045
04507 ** SC(37) PAA-ACCUMS (REINS GROUP-A TOTS)           **            ECS045
04508      MOVE +31                    TO SB.                           ECS045
04509      MOVE +37                    TO SC.                           ECS045
04510      PERFORM 0540-ADD-PAB THRU 0550-EXIT 6 TIMES.                 ECS045
04511                                                                   ECS045
04512 ** ZERO OUT CARR ACCUMULATORS IN PAA AND PAB **                   ECS045
04513                                                                   ECS045
04514      PERFORM 0440-ZERO-PAA THRU 0450-EXIT VARYING SA              ECS045
04515              FROM +3 BY +1  UNTIL SA GREATER +54.                 ECS045
04516                                                                   ECS045
04517      PERFORM 0460-ZERO-PAB THRU 0470-EXIT VARYING SA              ECS045
04518              FROM +2 BY +1  UNTIL SA GREATER +36.                 ECS045
04519                                                                   ECS045
04520  BBBB-FINAL-TOTALS-X.                                             ECS045
04521      EXIT.                                                        ECS045
04522                                                                   ECS045
04523  0800-EXIT.                                                       ECS045
04524      EXIT.                                                        ECS045
04525                                                                   ECS045
04526      EJECT                                                        ECS045
04527  0810-PRT-REINS-GROUP-A.                                          ECS045
04528 ** CHECK TO SEE IF REINS GROUP-A BREAKS ARE REQUESTED **          ECS045
04529                                                                   ECS045
04530      IF SAVE-R-B-GROUP = SPACES                                   ECS045
04531          GO TO 0815-SKIP-REINS-GROUP-A-PRT.                       ECS045
04532                                                                   ECS045
04533      MOVE '7'                    TO HDR-SW.                       ECS045
04534      PERFORM 0900-HDR-RTN THRU 0910-EXIT.                         ECS045
04535                                                                   ECS045
04536 ** PRODUCE FIRST 19 LINES OF REPORT BREAK USING PAA ACCUMULATORS *ECS045
04537                                                                   ECS045
04538      MOVE +7                     TO SA.                           ECS045
04539      PERFORM 0860-PRT-DTL-1 THRU 0870-EXIT VARYING SG             ECS045
04540              FROM +1 BY +1  UNTIL SG GREATER +19.                 ECS045
PEMMOD     MOVE ALL '_'                TO PRT
PEMMOD                                    PRT-2
PEMMOD     MOVE ' '                    TO X
PEMMOD     PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
PEMMOD                                                                  ECS045
04541      MOVE HDR-8                  TO PRT.                          ECS045
04542      MOVE HDR-8-2                TO PRT-2.                        ECS045
PEMMOD*    MOVE '0'                    TO X.                            ECS045
PEMMOD     MOVE ' '                    TO X.                            ECS045
04544      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
04545                                                                   ECS045
04546 ** PRODUCE LAST 19 LINES OF REPORT BREAK USING PAB ACCUMULATORS **ECS045
04547                                                                   ECS045
04548      IF DTE-FMT-OPT EQUAL '2'                                     ECS045
04549          PERFORM 0882-PRT-DTL-2 THRU 0892-EXIT VARYING SG         ECS045
04550                 FROM +20 BY +1 UNTIL SG GREATER +38               ECS045
04551      ELSE                                                         ECS045
04552          PERFORM 0880-PRT-DTL-2 THRU 0890-EXIT VARYING SG         ECS045
04553                 FROM +20 BY +1 UNTIL SG GREATER +38.              ECS045
04554                                                                   ECS045
04555  0815-SKIP-REINS-GROUP-A-PRT.                                     ECS045
04556 ** SB(55) PAA-ACCUMS (REINS GROUP-A TOTS)           **            ECS045
04557 ** SC(64) PAA-ACCUMS (REINS COMPANY PRIME TOTS)     **            ECS045
04558      MOVE +55                    TO SB.                           ECS045
04559      MOVE +64                    TO SC.                           ECS045
04560      PERFORM 0520-ADD-PAA THRU 0530-EXIT 9 TIMES.                 ECS045
04561                                                                   ECS045
04562 ** SB(37) PAA-ACCUMS (REINS GROUP-A TOTS)           **            ECS045
04563 ** SC(43) PAA-ACCUMS (REINS COMPANY PRIME TOTS)     **            ECS045
04564      MOVE +37                    TO SB.                           ECS045
04565      MOVE +43                    TO SC.                           ECS045
04566      PERFORM 0540-ADD-PAB THRU 0550-EXIT 6 TIMES.                 ECS045
04567                                                                   ECS045
04568 ** ZERO OUT REINS GROUP-A ACCUMULATORS IN PAA AND PAB **          ECS045
04569                                                                   ECS045
04570      PERFORM 0440-ZERO-PAA THRU 0450-EXIT VARYING SA              ECS045
04571              FROM +3 BY +1  UNTIL SA GREATER +63.                 ECS045
04572                                                                   ECS045
04573      PERFORM 0460-ZERO-PAB THRU 0470-EXIT VARYING SA              ECS045
04574              FROM +2 BY +1  UNTIL SA GREATER +42.                 ECS045
04575                                                                   ECS045
04576  0820-EXIT.                                                       ECS045
04577      EXIT.                                                        ECS045
04578                                                                   ECS045
04579      EJECT                                                        ECS045
04580  0830-PRT-REINS-COMPANY-PRIME.                                    ECS045
04581      MOVE '8'                    TO HDR-SW.                       ECS045
04582      PERFORM 0900-HDR-RTN THRU 0910-EXIT.                         ECS045
04583                                                                   ECS045
04584 ** PRODUCE FIRST 19 LINES OF REPORT BREAK USING PAA ACCUMULATORS *ECS045
04585                                                                   ECS045
04586      MOVE +8                     TO SA.                           ECS045
04587      PERFORM 0860-PRT-DTL-1 THRU 0870-EXIT VARYING SG             ECS045
04588              FROM +1 BY +1  UNTIL SG GREATER +19.                 ECS045
PEMMOD     MOVE ALL '_'                TO PRT
PEMMOD                                    PRT-2
PEMMOD     MOVE ' '                    TO X
PEMMOD     PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
PEMMOD                                                                  ECS045
04589      MOVE HDR-8                  TO PRT.                          ECS045
04590      MOVE HDR-8-2                TO PRT-2.                        ECS045
PEMMOD*    MOVE '0'                    TO X.                            ECS045
PEMMOD     MOVE ' '                    TO X.                            ECS045
04592      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
04593                                                                   ECS045
04594 ** PRODUCE LAST 19 LINES OF REPORT BREAK USING PAB ACCUMULATORS **ECS045
04595                                                                   ECS045
04596      IF DTE-FMT-OPT EQUAL '2'                                     ECS045
04597          PERFORM 0882-PRT-DTL-2 THRU 0892-EXIT VARYING SG         ECS045
04598                 FROM +20 BY +1 UNTIL SG GREATER +38               ECS045
04599      ELSE                                                         ECS045
04600          PERFORM 0880-PRT-DTL-2 THRU 0890-EXIT VARYING SG         ECS045
04601                 FROM +20 BY +1 UNTIL SG GREATER +38.              ECS045
061008
061008     IF DTE-FMT-OPT EQUAL '2' AND
061008       (DO-TOTAL-TYPE EQUAL '1' OR '2' OR '3')
061008         WRITE DATA-OUT-REC FROM DATA-OUT-RECORD
061008         PERFORM 3300-INIT-DATA-OUT THRU 3399-EXIT
061008     END-IF.
020612
020612     IF DTE-FMT-OPT EQUAL '2' AND
020612       (DOA-TOTAL-TYPE EQUAL '1' OR '2' OR '3' OR '4')
020612         WRITE DATA-OUT-AHL-REC FROM DATA-OUT-AHL-RECORD
020612         PERFORM 3400-INIT-DATA-OUT-AHL THRU 3499-EXIT
020612     END-IF.
04602                                                                   ECS045
04603  0835-SKIP-REINS-COMPANY-PRIME.                                   ECS045
04604                                                                   ECS045
04605 ** SB(64) PAA-ACCUMS (REINS COMPANY PRIME TOTS)     **            ECS045
04606 ** SC(73) PAA-ACCUMS (FINAL TOTS)                   **            ECS045
04607      MOVE +64                    TO SB.                           ECS045
04608      MOVE +73                    TO SC.                           ECS045
04609      PERFORM 0520-ADD-PAA THRU 0530-EXIT 9 TIMES.                 ECS045
04610                                                                   ECS045
04611 ** SB(43) PAA-ACCUMS (REINS COMPANY PRIME TOTS)     **            ECS045
04612 ** SC(49) PAA-ACCUMS (FINAL TOTS)                   **            ECS045
04613      MOVE +43                    TO SB.                           ECS045
04614      MOVE +49                    TO SC.                           ECS045
04615      PERFORM 0540-ADD-PAB THRU 0550-EXIT 6 TIMES.                 ECS045
04616                                                                   ECS045
04617 ** ZERO OUT CARR ACCUMULATORS IN PAA AND PAB **                   ECS045
04618                                                                   ECS045
04619      PERFORM 0440-ZERO-PAA THRU 0450-EXIT VARYING SA              ECS045
04620              FROM +4 BY +1  UNTIL SA GREATER +72.                 ECS045
04621                                                                   ECS045
04622      PERFORM 0460-ZERO-PAB THRU 0470-EXIT VARYING SA              ECS045
04623              FROM +2 BY +1  UNTIL SA GREATER +48.                 ECS045
04624                                                                   ECS045
04625  0840-EXIT.                                                       ECS045
04626      EXIT.                                                        ECS045
04627                                                                   ECS045
04628      EJECT                                                        ECS045
04629  0850-FINAL-TOTALS.                                               ECS045
04630      MOVE '9'                    TO HDR-SW.                       ECS045
04631      PERFORM 0900-HDR-RTN THRU 0910-EXIT.                         ECS045
04632                                                                   ECS045
04633 ** PRODUCE FIRST 19 LINES OF REPORT BREAK USING PAA ACCUMULATORS *ECS045
04634                                                                   ECS045
04635      MOVE +9                     TO SA.                           ECS045
04636      PERFORM 0860-PRT-DTL-1 THRU 0870-EXIT VARYING SG             ECS045
04637              FROM +1 BY +1  UNTIL SG GREATER +19.                 ECS045
PEMMOD     MOVE ALL '_'                TO PRT
PEMMOD                                    PRT-2
PEMMOD     MOVE ' '                    TO X
PEMMOD     PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
PEMMOD                                                                  ECS045
04638      MOVE HDR-8                  TO PRT.                          ECS045
04639      MOVE HDR-8-2                TO PRT-2.                        ECS045
PEMMOD*    MOVE '0'                    TO X.                            ECS045
PEMMOD     MOVE ' '                    TO X.                            ECS045
04641      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
04642                                                                   ECS045
04643 ** PRODUCE LAST 19 LINES OF REPORT BREAK USING PAB ACCUMULATORS **ECS045
04644                                                                   ECS045
04645      IF DTE-FMT-OPT EQUAL '2'                                     ECS045
04646          PERFORM 0882-PRT-DTL-2 THRU 0892-EXIT VARYING SG         ECS045
04647                 FROM +20 BY +1 UNTIL SG GREATER +38               ECS045
04648      ELSE                                                         ECS045
04649          PERFORM 0880-PRT-DTL-2 THRU 0890-EXIT VARYING SG         ECS045
04650                 FROM +20 BY +1 UNTIL SG GREATER +38.              ECS045
061008
061008     IF DTE-FMT-OPT EQUAL '2' AND 
061008       (DO-TOTAL-TYPE EQUAL '1' OR '2' OR '3')
061008         WRITE DATA-OUT-REC FROM DATA-OUT-RECORD
061008         PERFORM 3300-INIT-DATA-OUT THRU 3399-EXIT
061008     END-IF.
020612
020612     IF DTE-FMT-OPT EQUAL '2' AND 
020612       (DOA-TOTAL-TYPE EQUAL '1' OR '2' OR '3' OR '4')
020612         WRITE DATA-OUT-AHL-REC FROM DATA-OUT-AHL-RECORD
020612         PERFORM 3400-INIT-DATA-OUT-AHL THRU 3499-EXIT
020612     END-IF.
04651                                                                   ECS045
04652      PERFORM 0440-ZERO-PAA THRU 0450-EXIT VARYING SA              ECS045
04653              FROM +4 BY +1   UNTIL SA GREATER +81.                ECS045
04654                                                                   ECS045
04655      PERFORM 0460-ZERO-PAB THRU 0470-EXIT VARYING SA              ECS045
04656              FROM +3 BY +1   UNTIL SA GREATER +54.                ECS045
04657                                                                   ECS045
04658  0855-EXIT.                                                       ECS045
04659      EXIT.                                                        ECS045
04660                                                                   ECS045
04661      EJECT                                                        ECS045
04662  0860-PRT-DTL-1.                                                  ECS045
04663                                                                   ECS045
04664 ***************************************************************** ECS045
04665 *  (SA) CONTROLS THE BREAK LEVEL (DTE RANGE THRU FINAL TOTALS)  * ECS045
04666 *  (SG) CONTROLS THE DETAIL LINE BEING PRODUCED IN THE BREAK    * ECS045
04667 *  (SH) CONTROLS WHICH PAA COUNTERS ARE MOVED INTO PRINT LINE   * ECS045
04668 ***************************************************************** ECS045
04669                                                                   ECS045
04670 ************************************************                  ECS045
04671 *  COMPUTE FEE PERCENTAGE FOR DETAIL LINE (7)  *                  ECS045
04672 ************************************************                  ECS045
04673                                                                   ECS045
04674      IF DTE-FMT-OPT EQUAL '2'                                     ECS045
04675          MOVE DT-SS-2 (SG)    TO DT-SS (SG).                      ECS045
04676                                                                   ECS045
CIDMOD*    IF DT-SS (SG) = +7                                           ECS045
CIDMOD*        IF PAA-AMT (SA 6 6) NOT = ZERO                           ECS045
CIDMOD*            COMPUTE DT-FEE (7) ROUNDED =                         ECS045
CIDMOD*                PAA-AMT (SA 6 7) / PAA-AMT (SA 6 6)              ECS045
CIDMOD*        ELSE                                                     ECS045
CIDMOD*            MOVE ZEROS          TO DT-FEE (7).                   ECS045
04683                                                                   ECS045
04684 ************************************************                  ECS045
04685 *  COMPUTE ACCT COMM PCT FOR DETAIL LINE (13)  *                  ECS045
04686 ************************************************                  ECS045
04687                                                                   ECS045
CIDMOD*    IF DT-SS (SG) = +9                                           ECS045
CIDMOD*        IF PAA-AMT (SA 6 6) NOT = ZERO                           ECS045
CIDMOD*            COMPUTE DT-FEE (13) ROUNDED =                        ECS045
CIDMOD*                PAA-AMT (SA 6 9) / PAA-AMT (SA 6 6)              ECS045
CIDMOD*        ELSE                                                     ECS045
CIDMOD*            MOVE ZEROS          TO DT-FEE (13).                  ECS045
04694                                                                   ECS045
04695 ************************************************                  ECS045
04696 *  COMPUTE OVWT COMM PCT FOR DETAIL LINE (14)  *                  ECS045
04697 ************************************************                  ECS045
04698                                                                   ECS045
CIDMOD*    IF DT-SS (SG) = +10                                          ECS045
CIDMOD*        IF PAA-AMT (SA 6 6) NOT = ZERO                           ECS045
CIDMOD*            COMPUTE DT-FEE (14) ROUNDED =                        ECS045
CIDMOD*                PAA-AMT (SA 6 10) / PAA-AMT (SA 6 6)             ECS045
CIDMOD*        ELSE                                                     ECS045
CIDMOD*            MOVE ZEROS          TO DT-FEE (14).                  ECS045
CIDMOD*                                                                 ECS045
04706 ************************************************                  ECS045
04707 *  COMPUTE STATE TAX PCT FOR DETAIL LINE (15)  *                  ECS045
04708 ************************************************                  ECS045
04709                                                                   ECS045
CIDMOD*    IF DT-SS (SG) = +11                                          ECS045
CIDMOD*        IF PAA-AMT (SA 6 6) NOT = ZERO                           ECS045
CIDMOD*            COMPUTE DT-FEE (15) ROUNDED =                        ECS045
CIDMOD*                PAA-AMT (SA 6 11) / PAA-AMT (SA 6 6)             ECS045
CIDMOD*        ELSE                                                     ECS045
CIDMOD*            MOVE ZEROS          TO DT-FEE (15).                  ECS045
04716                                                                   ECS045
04717      MOVE SPACES                 TO DTL-1                         ECS045
04718                                     DTL-1-2.                      ECS045
04719      IF DTE-FMT-OPT EQUAL '2'                                     ECS045
CIDMOD*        MOVE DT-FEE (7)         TO DT-FEE-2 (7)                  ECS045
CIDMOD*        MOVE DT-FEE (13)        TO DT-FEE-2 (13)                 ECS045
CIDMOD*        MOVE DT-FEE (14)        TO DT-FEE-2 (14)                 ECS045
CIDMOD*        MOVE DT-FEE (15)        TO DT-FEE-2 (15)                 ECS045
04724          MOVE DT-PRT-SW-2 (SG)   TO DT-PRT-SW (SG)                ECS045
04725          MOVE DT-ENTRY-2  (SG)   TO D1-DESC-2                     ECS045
04726          MOVE DT-CTL-CHAR-2 (SG) TO X                             ECS045
04727      ELSE                                                         ECS045
04728          MOVE DT-PRT-SW   (SG)   TO DT-PRT-SW (SG)                ECS045
04729          MOVE DT-ENTRY (SG)      TO D1-DESC                       ECS045
04730          MOVE DT-CTL-CHAR (SG)   TO X.                            ECS045
04731                                                                   ECS045
04732      IF DTE-FMT-OPT EQUAL '2'                                     ECS045
04733         MOVE DT-PRT-SW-2 (SG)    TO DT-PRT-SW (SG).               ECS045
04734                                                                   ECS045
04735      IF DT-PRT-SW (SG) NOT = 'Y'                                  ECS045
04736         GO TO 0870-EXIT.                                          ECS045
04737                                                                   ECS045
04738 **********************************************                    ECS045
04739 * (1)-CURR LIFE (2)-CURR AH (3)-CURR COMBINE *                    ECS045
04740 * (4)-YTD  LIFE (5)-YTD  AH (6)-YTD  COMBINE *                    ECS045
04741 **********************************************                    ECS045
04742                                                                   ECS045
04743      IF DTE-FMT-OPT NOT EQUAL '2'                                 ECS045
04744          GO TO  0870-OPTION-1.                                    ECS045
04745                                                                   ECS045
04746      IF DT-SS-2 (SG) NOT = ZEROS                                  ECS045
04747          MOVE DT-SS-2 (SG)       TO SH                            ECS045
04748          MOVE PAA-AMT (SA 1 SH)  TO D1-CLF-2                      ECS045
04749          MOVE PAA-AMT (SA 2 SH)  TO D1-CAH-2                      ECS045
04750          MOVE PAA-AMT (SA 3 SH)  TO D1-CT-2                       ECS045
04751          MOVE PAA-AMT (SA 4 SH)  TO D1-YLF-2                      ECS045
04752          MOVE PAA-AMT (SA 5 SH)  TO D1-YAH-2                      ECS045
04753          MOVE PAA-AMT (SA 6 SH)  TO D1-YT-2                       ECS045
04754          MOVE PAA-AMT (SA 7 SH)  TO D1-OLF-2                      ECS045
04755          MOVE PAA-AMT (SA 8 SH)  TO D1-OAH-2                      ECS045
04756          MOVE PAA-AMT (SA 9 SH)  TO D1-OT-2.                      ECS045
061008
061008     IF DT-SS-2 (SG) NOT = ZEROS
061008         IF DO-OUT-SUBSCRIPT (SG) > 0
061008            MOVE DO-OUT-SUBSCRIPT (SG) TO WS-DO-SUB
061008            MOVE DT-SS-2 (SG)       TO DO-ID-NUM (WS-DO-SUB)
061008            MOVE DT-ENTRY-2 (SG)    TO DO-DESCR (WS-DO-SUB)
061008            MOVE PAA-AMT (SA 1 SH)  TO DO-CLF (WS-DO-SUB)
061008            MOVE PAA-AMT (SA 4 SH)  TO DO-YLF (WS-DO-SUB)
061008            MOVE PAA-AMT (SA 7 SH)  TO DO-ILF (WS-DO-SUB)
061008            MOVE PAA-AMT (SA 2 SH)  TO DO-CAH (WS-DO-SUB)
061008            MOVE PAA-AMT (SA 5 SH)  TO DO-YAH (WS-DO-SUB)
061008            MOVE PAA-AMT (SA 8 SH)  TO DO-IAH (WS-DO-SUB)
061008         END-IF
061008     END-IF.
020612
020612     IF DT-SS-2 (SG) NOT = ZEROS
020612         IF DOA-OUT-SUBSCRIPT (SG) > 0
020612            MOVE DOA-OUT-SUBSCRIPT (SG) TO WS-DO-SUB
020612            MOVE DT-SS-2 (SG)       TO DOA-ID-NUM (WS-DO-SUB)
020612            MOVE DT-ENTRY-2 (SG)    TO DOA-DESCR (WS-DO-SUB)
020612            MOVE PAA-AMT (SA 1 SH)  TO DOA-CLF (WS-DO-SUB)
020612            MOVE PAA-AMT (SA 2 SH)  TO DOA-CAH (WS-DO-SUB)
020612            MOVE PAA-AMT (SA 3 SH)  TO DOA-CCMB (WS-DO-SUB)
020612            MOVE PAA-AMT (SA 4 SH)  TO DOA-YLF (WS-DO-SUB)
020612            MOVE PAA-AMT (SA 5 SH)  TO DOA-YAH (WS-DO-SUB)
020612            MOVE PAA-AMT (SA 6 SH)  TO DOA-YCMB (WS-DO-SUB)
020612            MOVE PAA-AMT (SA 7 SH)  TO DOA-ILF (WS-DO-SUB)
020612            MOVE PAA-AMT (SA 8 SH)  TO DOA-IAH (WS-DO-SUB)
020612            MOVE PAA-AMT (SA 9 SH)  TO DOA-ICMB (WS-DO-SUB)
020612         END-IF
020612     END-IF.
022912
022912****MOVE TOTAL COMM TO EXTRACT AFTER LAST GROUP
022912     IF SG = 19
022912         MOVE 22                 TO DOA-ID-NUM (SG)
022912         MOVE 'TOTAL COMM'       TO DOA-DESCR (SG)
022912         MOVE PAA-AMT (SA 1 22)  TO DOA-CLF (SG)
022912         MOVE PAA-AMT (SA 2 22)  TO DOA-CAH (SG)
022912         MOVE PAA-AMT (SA 3 22)  TO DOA-CCMB (SG)
022912         MOVE PAA-AMT (SA 4 22)  TO DOA-YLF (SG)
022912         MOVE PAA-AMT (SA 5 22)  TO DOA-YAH (SG)
022912         MOVE PAA-AMT (SA 6 22)  TO DOA-YCMB (SG)
022912         MOVE PAA-AMT (SA 7 22)  TO DOA-ILF (SG)
022912         MOVE PAA-AMT (SA 8 22)  TO DOA-IAH (SG)
022912         MOVE PAA-AMT (SA 9 22)  TO DOA-ICMB (SG)
022912     END-IF.
04757                                                                   ECS045
04758      MOVE DTL-1-2                TO PRT-2.                        ECS045
04759      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
04760      GO TO 0870-EXIT.                                             ECS045
04761                                                                   ECS045
04762  0870-OPTION-1.                                                   ECS045
04763                                                                   ECS045
04764      IF DT-SS (SG) NOT = ZEROS                                    ECS045
04765          NEXT SENTENCE                                            ECS045
04766      ELSE                                                         ECS045
04767          MOVE DTL-1                  TO PRT                       ECS045
04768          PERFORM 0920-PRT-RTN THRU 0930-EXIT                      ECS045
04769          GO TO 0870-EXIT.                                         ECS045
04770                                                                   ECS045
04771      IF DTE-PGM-OPT EQUAL '1' OR '2' OR '6' OR '7' OR '8'         ECS045
04772          MOVE DT-SS   (SG)       TO SH                            ECS045
04773          MOVE PAA-AMT (SA 1 SH)  TO D1-CLF                        ECS045
04774          MOVE PAA-AMT (SA 2 SH)  TO D1-CAH                        ECS045
04775          MOVE PAA-AMT (SA 3 SH)  TO D1-CT.                        ECS045
04776                                                                   ECS045
04777      IF DTE-PGM-OPT EQUAL '3'                                     ECS045
04778          MOVE DT-SS   (SG)       TO SH                            ECS045
04779          MOVE PAA-AMT (SA 4 SH)  TO D1-CLF                        ECS045
04780          MOVE PAA-AMT (SA 5 SH)  TO D1-CAH                        ECS045
04781          MOVE PAA-AMT (SA 6 SH)  TO D1-CT.                        ECS045
04782                                                                   ECS045
04783      IF DTE-PGM-OPT EQUAL '4' OR '5'                              ECS045
04784          MOVE DT-SS   (SG)       TO SH                            ECS045
04785          MOVE PAA-AMT (SA 4 SH)  TO D1-CLF                        ECS045
04786          MOVE PAA-AMT (SA 5 SH)  TO D1-CAH                        ECS045
04787          MOVE PAA-AMT (SA 6 SH)  TO D1-CT.                        ECS045
04788                                                                   ECS045
04789      IF DTE-PGM-OPT EQUAL '1' OR '4' OR '7'                       ECS045
04790          MOVE PAA-AMT (SA 4 SH)  TO D1-YLF                        ECS045
04791          MOVE PAA-AMT (SA 5 SH)  TO D1-YAH                        ECS045
04792          MOVE PAA-AMT (SA 6 SH)  TO D1-YT.                        ECS045
04793                                                                   ECS045
04794      IF DTE-PGM-OPT EQUAL '2' OR '3' OR '5' OR '8'                ECS045
04795          MOVE PAA-AMT (SA 7 SH)  TO D1-YLF                        ECS045
04796          MOVE PAA-AMT (SA 8 SH)  TO D1-YAH                        ECS045
04797          MOVE PAA-AMT (SA 9 SH)  TO D1-YT.                        ECS045
04798                                                                   ECS045
04799      IF DTE-PGM-OPT EQUAL '6'                                     ECS045
04800          MOVE PAA-AMT (SA 4 SH)  TO D1-YLF                        ECS045
04801          MOVE PAA-AMT (SA 5 SH)  TO D1-YAH                        ECS045
04802          MOVE PAA-AMT (SA 6 SH)  TO D1-YT.                        ECS045
04803                                                                   ECS045
04804      MOVE DTL-1                  TO PRT.                          ECS045
04805      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
04806  0870-EXIT.                                                       ECS045
04807      EXIT.                                                        ECS045
04808                                                                   ECS045
04809      EJECT                                                        ECS045
04810  0880-PRT-DTL-2.                                                  ECS045
04811                                                                   ECS045
04812 ***************************************************************** ECS045
04813 *  (SA) CONTROLS THE BREAK LEVEL (DTE RANGE THRU FINAL TOTALS)  * ECS045
04814 *  (SG) CONTROLS THE DETAIL LINE BEING PRODUCED IN THE BREAK    * ECS045
04815 *  (SH) CONTROLS WHICH PAB COUNTERS ARE MOVED INTO PRINT LINE   * ECS045
04816 ***************************************************************** ECS045
04817                                                                   ECS045
04818      MOVE SPACES                 TO DTL-2.                        ECS045
04819      MOVE DT-ENTRY (SG)          TO D2-DESC.                      ECS045
04820      MOVE DT-CTL-CHAR (SG)       TO X.                            ECS045
04821                                                                   ECS045
04822      IF DT-PRT-SW (SG) NOT = 'Y'                                  ECS045
04823          GO TO 0890-EXIT.                                         ECS045
04824                                                                   ECS045
04825 *****************************                                     ECS045
04826 * (1)-BEGN CURR (2)-END CURR*                                     ECS045
04827 * (3)-BEGN YTD  (4)-END YTD *                                     ECS045
04828 * (5)-BEGN ITD  (6)-END ITD *                                     ECS045
04829 *****************************                                     ECS045
04830                                                                   ECS045
04831      IF DT-SS (SG) NOT = ZEROS                                    ECS045
04832          MOVE DT-SS   (SG)       TO SH                            ECS045
04833      ELSE                                                         ECS045
04834          GO TO 0882-CONTINUE.                                     ECS045
04835                                                                   ECS045
04836      IF DTE-PGM-OPT EQUAL '1' OR '2' OR '6' OR '7' OR '8'         ECS045
04837          MOVE PAB-AMT (SA 1 SH)  TO D2-CBEG                       ECS045
04838          MOVE PAB-AMT (SA 2 SH)  TO D2-CEND                       ECS045
04839      ELSE                                                         ECS045
04840      IF DTE-PGM-OPT EQUAL '3'                                     ECS045
04841          MOVE PAB-AMT (SA 3 SH)  TO D2-CBEG                       ECS045
04842          MOVE PAB-AMT (SA 4 SH)  TO D2-CEND                       ECS045
04843      ELSE                                                         ECS045
04844      IF DTE-PGM-OPT EQUAL '4'  OR '5'                             ECS045
04845          MOVE PAB-AMT (SA 5 SH)  TO D2-CBEG                       ECS045
04846          MOVE PAB-AMT (SA 6 SH)  TO D2-CEND.                      ECS045
04847                                                                   ECS045
04848      IF DTE-PGM-OPT EQUAL '1' OR '4' OR '7'                       ECS045
04849          MOVE PAB-AMT (SA 3 SH)  TO D2-YBEG                       ECS045
04850          MOVE PAB-AMT (SA 4 SH)  TO D2-YEND                       ECS045
04851      ELSE                                                         ECS045
04852      IF DTE-PGM-OPT EQUAL '2' OR '3' OR '5' OR '8'                ECS045
04853          MOVE PAB-AMT (SA 5 SH)  TO D2-YBEG                       ECS045
04854          MOVE PAB-AMT (SA 6 SH)  TO D2-YEND                       ECS045
04855      ELSE                                                         ECS045
04856      IF DTE-PGM-OPT EQUAL '6'                                     ECS045
04857          MOVE PAB-AMT (SA 5 SH)  TO D2-YBEG                       ECS045
04858          MOVE PAB-AMT (SA 6 SH)  TO D2-YEND.                      ECS045
04859                                                                   ECS045
04860  0882-CONTINUE.                                                   ECS045
04861                                                                   ECS045
04862      MOVE DTL-2                  TO PRT.                          ECS045
04863      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
04864                                                                   ECS045
04865  0890-EXIT.                                                       ECS045
04866      EXIT.                                                        ECS045
04867  EJECT                                                            ECS045
04868  0882-PRT-DTL-2.                                                  ECS045
04869                                                                   ECS045
04870 ***************************************************************** ECS045
04871 *  (SA) CONTROLS THE BREAK LEVEL (DTE RANGE THRU FINAL TOTALS)  * ECS045
04872 *  (SG) CONTROLS THE DETAIL LINE BEING PRODUCED IN THE BREAK    * ECS045
04873 *  (SH) CONTROLS WHICH PAB COUNTERS ARE MOVED INTO PRINT LINE   * ECS045
04874 ***************************************************************** ECS045
04875                                                                   ECS045
04876      MOVE SPACES                 TO DTL-2-2.                      ECS045
04877      MOVE DT-ENTRY-2 (SG)        TO D2-DESC-2.                    ECS045
04878      MOVE DT-CTL-CHAR-2 (SG)     TO X.                            ECS045
04879                                                                   ECS045
04880      IF DT-PRT-SW-2 (SG) NOT = 'Y'                                ECS045
04881          GO TO 0892-EXIT.                                         ECS045
04882                                                                   ECS045
04883 *****************************                                     ECS045
04884 * (1)-BEGN CURR (2)-END CURR*                                     ECS045
04885 * (3)-BEGN YTD  (4)-END YTD *                                     ECS045
04886 * (5)-BEGN ITD  (6)-END ITD *                                     ECS045
04887 *****************************                                     ECS045
04888                                                                   ECS045
04889      IF DT-SS (SG) NOT = ZEROS                                    ECS045
04890          MOVE DT-SS   (SG)       TO SH                            ECS045
04891          MOVE PAB-AMT (SA 1 SH)  TO D2-CBEG-2                     ECS045
04892          MOVE PAB-AMT (SA 2 SH)  TO D2-CEND-2                     ECS045
04893          MOVE PAB-AMT (SA 3 SH)  TO D2-YBEG-2                     ECS045
04894          MOVE PAB-AMT (SA 4 SH)  TO D2-YEND-2                     ECS045
04895          MOVE PAB-AMT (SA 5 SH)  TO D2-OBEG-2                     ECS045
04896          MOVE PAB-AMT (SA 6 SH)  TO D2-OEND-2.                    ECS045
061008
061008     IF DT-SS (SG) NOT = ZEROS
061008         IF DO-OUT-SUBSCRIPT (SG) > 0
061008            MOVE DO-OUT-SUBSCRIPT (SG) TO WS-DO-SUB
061008            MOVE DT-SS (SG)         TO DO-ID-NUM (WS-DO-SUB)
061008            MOVE DT-ENTRY-2 (SG)    TO DO-DESCR (WS-DO-SUB)
061008            MOVE PAB-AMT (SA 1 SH)  TO DO-CBG (WS-DO-SUB)
061008            MOVE PAB-AMT (SA 2 SH)  TO DO-CEN (WS-DO-SUB)
061008            MOVE PAB-AMT (SA 3 SH)  TO DO-YBG (WS-DO-SUB)
061008            MOVE PAB-AMT (SA 4 SH)  TO DO-YEN (WS-DO-SUB)
061008            MOVE PAB-AMT (SA 5 SH)  TO DO-IBG (WS-DO-SUB)
061008            MOVE PAB-AMT (SA 6 SH)  TO DO-IEN (WS-DO-SUB)
061008         END-IF
061008     END-IF.
020612
020612     IF DT-SS (SG) NOT = ZEROS
020612         IF DOA-OUT-SUBSCRIPT (SG) > 0
020612            MOVE DOA-OUT-SUBSCRIPT (SG) TO WS-DO-SUB
020612            MOVE DT-SS (SG)         TO DOB-ID-NUM (WS-DO-SUB)
020612            MOVE DT-ENTRY-2 (SG)    TO DOB-DESCR (WS-DO-SUB)
020612            MOVE PAB-AMT (SA 1 SH)  TO DOB-CBG (WS-DO-SUB)
020612            MOVE PAB-AMT (SA 2 SH)  TO DOB-CEN (WS-DO-SUB)
020612            MOVE PAB-AMT (SA 3 SH)  TO DOB-YBG (WS-DO-SUB)
020612            MOVE PAB-AMT (SA 4 SH)  TO DOB-YEN (WS-DO-SUB)
020612            MOVE PAB-AMT (SA 5 SH)  TO DOB-IBG (WS-DO-SUB)
020612            MOVE PAB-AMT (SA 6 SH)  TO DOB-IEN (WS-DO-SUB)
020612         END-IF
020612     END-IF.
04897                                                                   ECS045
04898      MOVE DTL-2-2                TO PRT-2.                        ECS045
04899      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
04900                                                                   ECS045
04901  0892-EXIT.                                                       ECS045
04902      EXIT.                                                        ECS045
04903                                                                   ECS045
04904      EJECT                                                        ECS045
04905  0900-HDR-RTN.                                                    ECS045
04906                                                                   ECS045
04907 *******************************************                       ECS045
04908 *  HEADING ROUTINE IS COMMON FOR LINES    *                       ECS045
04909 *  (1-2) IN (45-A THRU 45-E)              *                       ECS045
04910 *  AND THEN SPLITS AFTER THAT             *                       ECS045
04911 *******************************************                       ECS045
04912                                                                   ECS045
04913 ****  PRODUCE HEADING LINE (1) FOR REPORT  ****                   ECS045
04914                                                                   ECS045
04915      ADD +1                      TO PAGE-CNT.                     ECS045
04916      MOVE PAGE-CNT               TO HD-PG-NO.                     ECS045
04917      MOVE HDR-1                  TO PRT                           ECS045
04918                                     PRT-2.                        ECS045
04919      MOVE '1'                    TO X.                            ECS045
04920      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
04921                                                                   ECS045
04922 ****  PRODUCE HEADING LINE (2) FOR REPORT  ****                   ECS045
04923                                                                   ECS045
04924      MOVE HDR-2                  TO PRT                           ECS045
04925                                     PRT-2.                        ECS045
04926      MOVE ' '                    TO X.                            ECS045
04927      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
04928                                                                   ECS045
04929 ***************************************************               ECS045
04930 *  SPLIT BETWEEN  A & C / B / D / E  REPORT HERE  *               ECS045
04931 ***************************************************               ECS045
04932                                                                   ECS045
04933      IF H1-RUN-CODE = 'B'                                         ECS045
04934          GO TO 0905-HDR-RTN-RPT-B.                                ECS045
04935                                                                   ECS045
04936      IF H1-RUN-CODE = 'D'                                         ECS045
04937          GO TO 0907-HDR-RTN-RPT-D.                                ECS045
04938                                                                   ECS045
04939      IF H1-RUN-CODE = 'E'                                         ECS045
04940          GO TO 0909-HDR-RTN-RPT-E.                                ECS045
04941                                                                   ECS045
04942 ***********************************                               ECS045
04943 *  045-A REPORT HEADINGS ROUTINE  *                               ECS045
04944 ***********************************                               ECS045
04945                                                                   ECS045
04946 ****  PRODUCE HEADING LINE (3) FOR REPORT  ****                   ECS045
04947                                                                   ECS045
04948      IF HDR-SW LESS '8'                                           ECS045
04949          MOVE 'REINSURED BY - '  TO H3-COMMENT                    ECS045
04950          MOVE SAVE-R-B-REI-COMP  TO H3-COMMENT-1                  ECS045
04951          MOVE SAVE-REI-NAME      TO H3-COMP-NAME                  ECS045
04952      ELSE                                                         ECS045
04953          IF HDR-SW = '8'                                          ECS045
04954              MOVE 'REIN COMP TOTAL' TO H3-COMMENT                 ECS045
04955              MOVE SPACES            TO H3-COMP-NAME               ECS045
04956                                        H3-COMMENT-1               ECS045
04957          ELSE                                                     ECS045
04958              IF HDR-SW = '9'                                      ECS045
04959                  MOVE 'FINAL TOTALS' TO H3-COMMENT                ECS045
04960                  MOVE SPACES         TO H3-COMP-NAME              ECS045
04961                                         H3-COMMENT-1.             ECS045
04962                                                                   ECS045
04963      IF H1-RUN-CODE = 'C'                                         ECS045
04964          MOVE SPACES             TO H3-COMMENT-1.                 ECS045
04965                                                                   ECS045
04966      MOVE HDR-3                  TO PRT                           ECS045
04967                                     PRT-2.                        ECS045
04968      MOVE ' '                    TO X.                            ECS045
04969      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
04970                                                                   ECS045
04971 ****  PRODUCE HEADING LINE (3A) FOR REPORT  ****                  ECS045
04972                                                                   ECS045
04973      IF (HDR-SW LESS '8') AND (SAVE-CEDE-NAME NOT = SPACES)       ECS045
04974          MOVE SAVE-CEDE-NAME     TO H3A-COMP-NAME                 ECS045
04975          MOVE ' '                TO X                             ECS045
04976          MOVE HDR-3A             TO PRT                           ECS045
04977                                     PRT-2                         ECS045
04978          PERFORM 0920-PRT-RTN THRU 0930-EXIT.                     ECS045
04979                                                                   ECS045
04980 ****  PRODUCE HEADING LINE (4A AND 4B) FOR REPORT  ****           ECS045
04981                                                                   ECS045
04982      MOVE SPACES                 TO HDR-4-A                       ECS045
04983                                     HDR-4-B                       ECS045
04984                                     HDR-5.                        ECS045
04985                                                                   ECS045
04986      IF H1-RUN-CODE = 'C'                                         ECS045
04987          GO TO 0900-SKIP-HEADINGS-C-REPORT.                       ECS045
04988                                                                   ECS045
04989      IF HDR-SW LESS THAN '3'                                      ECS045
04990         MOVE 'REINS ACCTREIN         CERT                   REINS'ECS045
04991                                                 TO H4-A-DESC      ECS045
04992      ELSE                                                         ECS045
04993          IF HDR-SW LESS THAN '6'                                  ECS045
04994              MOVE 'REINS ACCTREIN         CERT' TO H4-A-DESC      ECS045
04995          ELSE                                                     ECS045
04996              IF HDR-SW LESS THAN '8'                              ECS045
04997                  MOVE 'REINS ACCTREIN'          TO H4-A-DESC      ECS045
04998              ELSE                                                 ECS045
04999                  IF HDR-SW LESS THAN '9'                          ECS045
05000                      MOVE 'REINS'               TO H4-A-DESC.     ECS045
05001                                                                   ECS045
05002      MOVE HDR-4-A                TO PRT                           ECS045
05003                                     PRT-2.                        ECS045
05004      MOVE '0'                    TO X.                            ECS045
05005      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05006                                                                   ECS045
05007      IF HDR-SW = '1'                                              ECS045
05008          MOVE 'PRIME GROUP-A CARRIER  GROUP  STATE   ACCOUNT'     ECS045
05009                                       TO H4-B-DESC                ECS045
05010          MOVE '   ACCOUNT NAME                  SUB   EFF DT*'    ECS045
05011                                       TO H4-B-DESC1               ECS045
05012          MOVE '   EXP-DT*'            TO H4-B-DESC2               ECS045
05013          MOVE SAVE-R-B-REI-COMP-PRIME TO H5-REIN-PRIME            ECS045
05014          MOVE SAVE-R-B-GROUP          TO H5-REIN-GRP-A            ECS045
05015          MOVE SAVE-R-B-CARR           TO H5-CARR                  ECS045
05016          MOVE SAVE-R-B-COMP           TO H5-CERT-GRP              ECS045
05017          MOVE SAVE-R-B-STATE          TO STATE-L                  ECS045
05018          PERFORM 1100-STATE-CODE-LOOKUP THRU 1100-STATE-LOOKUP-X  ECS045
05019          MOVE STATE-ABBR(CLAS-INDEXS) TO H5-ST                    ECS045
05020          MOVE SAVE-R-B-ACCT           TO H5-ACCT                  ECS045
05021          MOVE SAVE-ACC-NAME           TO H5-ACCT-NAME             ECS045
05022          MOVE SAVE-R-B-REI-COMP-SUB   TO H5-REIN-SUB              ECS045
05023          MOVE '-' TO H5-DASH-1  H5-DASH-2  H5-DASH-3  H5-DASH-4   ECS045
05024          MOVE SAVE-R-B-EFF            TO WORK-DATE                ECS045
05025          MOVE WD-MO                   TO H5-EFF-MO                ECS045
05026          MOVE WD-DA                   TO H5-EFF-DA                ECS045
05027          MOVE WD-YR                   TO H5-EFF-YR                ECS045
05028          MOVE SAVE-R-B-EXP            TO WORK-DATE                ECS045
05029          MOVE WD-MO                   TO H5-EXP-MO                ECS045
05030          MOVE WD-DA                   TO H5-EXP-DA                ECS045
05031          MOVE WD-YR                   TO H5-EXP-YR.               ECS045
05032                                                                   ECS045
05033      IF HDR-SW = '2'                                              ECS045
05034         MOVE 'PRIME GROUP-A CARRIER  GROUP  STATE   ACCOUNT'      ECS045
05035                                       TO H4-B-DESC                ECS045
05036          MOVE '   ACCOUNT NAME                  SUB*'             ECS045
05037                                       TO H4-B-DESC1               ECS045
05038          MOVE SAVE-R-B-REI-COMP-PRIME TO H5-REIN-PRIME            ECS045
05039          MOVE SAVE-R-B-GROUP          TO H5-REIN-GRP-A            ECS045
05040          MOVE SAVE-R-B-CARR           TO H5-CARR                  ECS045
05041          MOVE SAVE-R-B-COMP           TO H5-CERT-GRP              ECS045
05042          MOVE SAVE-R-B-STATE          TO STATE-L                  ECS045
05043          PERFORM 1100-STATE-CODE-LOOKUP THRU 1100-STATE-LOOKUP-X  ECS045
05044          MOVE STATE-ABBR(CLAS-INDEXS) TO H5-ST                    ECS045
05045          MOVE SAVE-R-B-ACCT           TO H5-ACCT                  ECS045
05046          MOVE SAVE-ACC-NAME           TO H5-ACCT-NAME             ECS045
05047          MOVE SAVE-R-B-REI-COMP-SUB   TO H5-REIN-SUB.             ECS045
020612
020612     IF HDR-SW = '2'
020612         MOVE SAVE-R-B-REI-COMP       TO DOA-REIN-BY-NUM
020612         MOVE SAVE-REI-NAME           TO DOA-REIN-BY-NAME
020612         MOVE SAVE-CEDE-NAME          TO DOA-CEDED-FROM-NAME
020612         MOVE SAVE-R-B-REI-COMP-PRIME TO DOA-REIN-PRIME
020612         MOVE SAVE-R-B-GROUP          TO DOA-REIN-GROUP
020612         MOVE SAVE-R-B-CARR           TO DOA-CARRIER
020612         MOVE SAVE-R-B-COMP           TO DOA-CERT-GROUP
020612         MOVE STATE-ABBR(CLAS-INDEXS) TO DOA-STATE
020612         MOVE SAVE-R-B-ACCT           TO DOA-ACCOUNT-NUM
020612         MOVE SAVE-ACC-NAME           TO DOA-ACCOUNT-NAME
020612         MOVE SAVE-R-B-REI-COMP-SUB   TO DOA-REIN-SUB
020612         MOVE '1'                     TO DOA-TOTAL-TYPE
020612         MOVE SAVE-CLAIM-CODE         TO DOA-CLAIM-PI
020612         MOVE SAVE-LF-PE              TO DOA-LF-PE
020612         MOVE SAVE-AH-PE              TO DOA-AH-PE
020612     END-IF.
05048                                                                   ECS045
05049      IF HDR-SW = '3'                                              ECS045
05050          MOVE 'PRIME GROUP-A CARRIER  GROUP  STATE   ACCOUNT'     ECS045
05051                                       TO H4-B-DESC                ECS045
05052          MOVE '   ACCOUNT NAME*'      TO H4-B-DESC1               ECS045
05053          MOVE SAVE-R-B-REI-COMP-PRIME TO H5-REIN-PRIME            ECS045
05054          MOVE SAVE-R-B-GROUP          TO H5-REIN-GRP-A            ECS045
05055          MOVE SAVE-R-B-CARR           TO H5-CARR                  ECS045
05056          MOVE SAVE-R-B-COMP           TO H5-CERT-GRP              ECS045
05057          MOVE SAVE-R-B-STATE          TO STATE-L                  ECS045
05058          PERFORM 1100-STATE-CODE-LOOKUP THRU 1100-STATE-LOOKUP-X  ECS045
05059          MOVE STATE-ABBR(CLAS-INDEXS) TO H5-ST                    ECS045
05060          MOVE SAVE-R-B-ACCT           TO H5-ACCT                  ECS045
05061          MOVE SAVE-ACC-NAME           TO H5-ACCT-NAME.            ECS045
061008
061008     IF HDR-SW = '3'
061008         MOVE SAVE-R-B-REI-COMP       TO DO-REIN-BY-NUM
061008         MOVE SAVE-REI-NAME           TO DO-REIN-BY-NAME
061008         MOVE SAVE-CEDE-NAME          TO DO-CEDED-FROM-NAME
061008         MOVE SAVE-R-B-REI-COMP-PRIME TO DO-REIN-PRIME
061008         MOVE SAVE-R-B-GROUP          TO DO-REIN-GROUP
061008         MOVE SAVE-R-B-CARR           TO DO-CARRIER
061008         MOVE SAVE-R-B-COMP           TO DO-CERT-GROUP
061008         MOVE STATE-ABBR(CLAS-INDEXS) TO DO-STATE
061008         MOVE SAVE-R-B-ACCT           TO DO-ACCOUNT-NUM
061008         MOVE SAVE-ACC-NAME           TO DO-ACCOUNT-NAME
061008         MOVE '1'                     TO DO-TOTAL-TYPE
061008         MOVE SAVE-CLAIM-CODE         TO DO-CLAIM-PI
061008     END-IF.
05062                                                                   ECS045
05063      IF HDR-SW = '4'                                              ECS045
05064          MOVE 'PRIME GROUP-A CARRIER  GROUP  STATE*'              ECS045
05065                                       TO H4-B-DESC                ECS045
05066          MOVE SAVE-R-B-REI-COMP-PRIME TO H5-REIN-PRIME            ECS045
05067          MOVE SAVE-R-B-GROUP          TO H5-REIN-GRP-A            ECS045
05068          MOVE SAVE-R-B-CARR           TO H5-CARR                  ECS045
05069          MOVE SAVE-R-B-COMP           TO H5-CERT-GRP              ECS045
05070          MOVE SAVE-R-B-STATE          TO H5-ST.                   ECS045
020612
020612     IF HDR-SW = '4'
020612         MOVE SAVE-R-B-REI-COMP       TO DOA-REIN-BY-NUM
020612         MOVE SAVE-REI-NAME           TO DOA-REIN-BY-NAME
020612         MOVE SAVE-CEDE-NAME          TO DOA-CEDED-FROM-NAME
020612         MOVE SAVE-R-B-REI-COMP-PRIME TO DOA-REIN-PRIME
020612         MOVE SAVE-R-B-GROUP          TO DOA-REIN-GROUP
020612         MOVE SAVE-R-B-CARR           TO DOA-CARRIER
020612         MOVE SAVE-R-B-COMP           TO DOA-CERT-GROUP
020612         MOVE STATE-ABBR(CLAS-INDEXS) TO DOA-STATE
020612         MOVE 'STATE TOTAL'           TO DOA-ACCOUNT-NAME
020612         MOVE '4'                     TO DOA-TOTAL-TYPE
020612         MOVE SAVE-CLAIM-CODE         TO DOA-CLAIM-PI
020612         MOVE SAVE-LF-PE              TO DOA-LF-PE
020612         MOVE SAVE-AH-PE              TO DOA-AH-PE
020612     END-IF.
05071                                                                   ECS045
05072      IF HDR-SW = '5'                                              ECS045
05073          MOVE 'PRIME GROUP-A CARRIER  GROUP*'                     ECS045
05074                                       TO H4-B-DESC                ECS045
05075          MOVE SAVE-R-B-REI-COMP-PRIME TO H5-REIN-PRIME            ECS045
05076          MOVE SAVE-R-B-GROUP          TO H5-REIN-GRP-A            ECS045
05077          MOVE SAVE-R-B-CARR           TO H5-CARR                  ECS045
05078          MOVE SAVE-R-B-COMP           TO H5-CERT-GRP.             ECS045
05079                                                                   ECS045
05080      IF HDR-SW = '6'                                              ECS045
05081          MOVE 'PRIME GROUP-A CARRIER*'                            ECS045
05082                                       TO H4-B-DESC                ECS045
05083          MOVE SAVE-R-B-REI-COMP-PRIME TO H5-REIN-PRIME            ECS045
05084          MOVE SAVE-R-B-GROUP          TO H5-REIN-GRP-A            ECS045
05085          MOVE SAVE-R-B-CARR           TO H5-CARR.                 ECS045
05086                                                                   ECS045
05087      IF HDR-SW = '7'                                              ECS045
05088          MOVE 'PRIME GROUP-A*'                                    ECS045
05089                                       TO H4-B-DESC                ECS045
05090          MOVE SAVE-R-B-REI-COMP-PRIME TO H5-REIN-PRIME            ECS045
05091          MOVE SAVE-R-B-GROUP          TO H5-REIN-GRP-A.           ECS045
05092                                                                   ECS045
05093      IF HDR-SW = '8'                                              ECS045
05094          MOVE 'PRIME*'                                            ECS045
05095                                       TO H4-B-DESC                ECS045
05096          MOVE SAVE-R-B-REI-COMP-PRIME TO H5-REIN-PRIME.           ECS045
061008
061008     IF HDR-SW = '8'
061008         MOVE SAVE-REI-NAME           TO DO-REIN-BY-NAME
061008         MOVE SAVE-R-B-REI-COMP-PRIME TO DO-REIN-PRIME
061008         MOVE 'REIN COMP TOTAL'       TO DO-ACCOUNT-NAME
061008         MOVE '2'                     TO DO-TOTAL-TYPE
061008         MOVE SAVE-CLAIM-CODE         TO DO-CLAIM-PI
061008     END-IF.
061008
020612     IF HDR-SW = '8'
020612         MOVE SAVE-REI-NAME           TO DOA-REIN-BY-NAME
020612         MOVE SAVE-R-B-REI-COMP-PRIME TO DOA-REIN-PRIME
020612         MOVE 'REIN COMP TOTAL'       TO DOA-ACCOUNT-NAME
020612         MOVE '2'                     TO DOA-TOTAL-TYPE
020612         MOVE SAVE-CLAIM-CODE         TO DOA-CLAIM-PI
020612         MOVE SAVE-LF-PE              TO DOA-LF-PE
020612         MOVE SAVE-AH-PE              TO DOA-AH-PE
020612     END-IF.
020612
05097                                                                   ECS045
05098      MOVE HDR-4-B                TO PRT                           ECS045
05099                                     PRT-2                         ECS045
05100      MOVE ' '                    TO X.                            ECS045
05101      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05102                                                                   ECS045
05103 ****  PRODUCE HEADING LINE (5 THRU 7) FOR REPORT  ****            ECS045
05104                                                                   ECS045
05105      IF FINAL-TOT = 'Y'                                           ECS045
05106          MOVE SPACE              TO HDR-5.                        ECS045
05107                                                                   ECS045
05108      MOVE HDR-5                  TO PRT                           ECS045
05109                                     PRT-2                         ECS045
05110      MOVE ' '                    TO X.                            ECS045
05111      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05112                                                                   ECS045
05113  0900-SKIP-HEADINGS-C-REPORT.                                     ECS045
05114      IF DTE-FMT-OPT EQUAL '2'                                     ECS045
05115          MOVE HDR-6-2            TO PRT-2                         ECS045
05116      ELSE                                                         ECS045
05117          MOVE HDR-6              TO PRT.                          ECS045
05118      MOVE '0'                    TO X.                            ECS045
05119      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05120                                                                   ECS045
05121      IF DTE-FMT-OPT EQUAL '2'                                     ECS045
05122          MOVE HDR-7-2            TO PRT-2                         ECS045
05123      ELSE                                                         ECS045
05124          MOVE HDR-7              TO PRT.                          ECS045
05125      MOVE ' '                    TO X.                            ECS045
05126      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05127      GO TO 0910-EXIT.                                             ECS045
05128                                                                   ECS045
05129      EJECT                                                        ECS045
05130  0905-HDR-RTN-RPT-B.                                              ECS045
05131 ***********************************                               ECS045
05132 *  045-B REPORT HEADINGS ROUTINE  *                               ECS045
05133 ***********************************                               ECS045
05134                                                                   ECS045
05135 ****  PRODUCE HEADING LINE (3) FOR REPORT  ****                   ECS045
05136                                                                   ECS045
05137      MOVE SPACES                 TO H3-COMMENT                    ECS045
05138                                     H3-COMP-NAME                  ECS045
05139                                     H3-COMMENT-1.                 ECS045
05140                                                                   ECS045
05141      IF HDR-SW LESS '4'                                           ECS045
05142         MOVE 'REINSURED BY - '   TO H3-COMMENT                    ECS045
05143         MOVE SAVE-R-B-REI-COMP-B TO H3-COMMENT-1                  ECS045
05144         MOVE SAVE-REI-NAME       TO H3-COMP-NAME.                 ECS045
05145                                                                   ECS045
05146      IF HDR-SW = '6'                                              ECS045
05147          MOVE 'FINAL TOTALS'     TO H3-COMMENT.                   ECS045
05148                                                                   ECS045
05149      MOVE HDR-3                  TO PRT                           ECS045
05150                                     PRT-2.                        ECS045
05151      MOVE ' '                    TO X.                            ECS045
05152      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05153                                                                   ECS045
05154 ****  PRODUCE HEADING LINE (3A) FOR REPORT  ****                  ECS045
05155                                                                   ECS045
05156      IF (HDR-SW LESS '4') AND (SAVE-CEDE-NAME NOT = SPACES)       ECS045
05157          MOVE SAVE-CEDE-NAME     TO H3A-COMP-NAME                 ECS045
05158          MOVE ' '                TO X                             ECS045
05159          MOVE HDR-3A             TO PRT                           ECS045
05160                                     PRT-2                         ECS045
05161          PERFORM 0920-PRT-RTN THRU 0930-EXIT.                     ECS045
05162                                                                   ECS045
05163 ****  PRODUCE HEADING LINE (4A AND 4B) FOR REPORT  ****           ECS045
05164                                                                   ECS045
05165      MOVE SPACES                 TO HDR-4-A                       ECS045
05166                                     HDR-4-B                       ECS045
05167                                     HDR-5-B.                      ECS045
05168                                                                   ECS045
05169      IF HDR-SW LESS THAN '2'                                      ECS045
05170          MOVE '        ACCTREIN REINS         REINS'              ECS045
05171                                                TO H4-A-DESC       ECS045
05172      ELSE                                                         ECS045
05173          IF HDR-SW LESS THAN '3'                                  ECS045
05174              MOVE '        ACCTREIN REINS'     TO H4-A-DESC       ECS045
05175          ELSE                                                     ECS045
05176              IF HDR-SW LESS THAN '5'                              ECS045
05177                  MOVE '        ACCTREIN'       TO H4-A-DESC.      ECS045
05178                                                                   ECS045
05179      MOVE HDR-4-A                TO PRT                           ECS045
05180                                     PRT-2.                        ECS045
05181      MOVE '0'                    TO X.                            ECS045
05182      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05183                                                                   ECS045
05184      IF HDR-SW = '1'                                              ECS045
05185          MOVE 'CARRIER GROUP-B  PRIME  STATE   SUB*'              ECS045
05186                                         TO H4-B-DESC              ECS045
05187          MOVE SAVE-R-B-CARR-B           TO H5-B-CARR              ECS045
05188          MOVE SAVE-R-B-GROUP-B          TO H5-B-REIN-GRP-B        ECS045
05189          MOVE SAVE-R-B-REI-COMP-PRIME-B TO H5-B-REIN-PRIME        ECS045
05190          MOVE SAVE-R-B-STATE-B          TO H5-B-ST                ECS045
05191          MOVE SAVE-R-B-REI-COMP-SUB-B   TO H5-B-REIN-SUB.         ECS045
05192                                                                   ECS045
05193      IF HDR-SW = '2'                                              ECS045
05194          MOVE 'CARRIER GROUP-B  PRIME  STATE*'                    ECS045
05195                                         TO H4-B-DESC              ECS045
05196          MOVE SAVE-R-B-CARR-B           TO H5-B-CARR              ECS045
05197          MOVE SAVE-R-B-GROUP-B          TO H5-B-REIN-GRP-B        ECS045
05198          MOVE SAVE-R-B-REI-COMP-PRIME-B TO H5-B-REIN-PRIME        ECS045
05199          MOVE SAVE-R-B-STATE-B          TO H5-B-ST.               ECS045
05200                                                                   ECS045
05201      IF HDR-SW = '3'                                              ECS045
05202          MOVE 'CARRIER GROUP-B  PRIME*'                           ECS045
05203                                         TO H4-B-DESC              ECS045
05204          MOVE SAVE-R-B-CARR-B           TO H5-B-CARR              ECS045
05205          MOVE SAVE-R-B-GROUP-B          TO H5-B-REIN-GRP-B        ECS045
05206          MOVE SAVE-R-B-REI-COMP-PRIME-B TO H5-B-REIN-PRIME.       ECS045
05207                                                                   ECS045
05208      IF HDR-SW = '4'                                              ECS045
05209          MOVE 'CARRIER GROUP-B*'                                  ECS045
05210                                  TO H4-B-DESC                     ECS045
05211          MOVE SAVE-R-B-CARR-B    TO H5-B-CARR                     ECS045
05212          MOVE SAVE-R-B-GROUP-B   TO H5-B-REIN-GRP-B.              ECS045
05213                                                                   ECS045
05214      IF HDR-SW = '5'                                              ECS045
05215          MOVE 'CARRIER*'                                          ECS045
05216                                  TO H4-B-DESC                     ECS045
05217          MOVE SAVE-R-B-CARR-B    TO H5-B-CARR.                    ECS045
05218                                                                   ECS045
05219      MOVE HDR-4-B                TO PRT                           ECS045
05220                                     PRT-2.                        ECS045
05221      MOVE ' '                    TO X.                            ECS045
05222      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05223                                                                   ECS045
05224 ****  PRODUCE HEADING LINE (5 THRU 7) FOR REPORT  ****            ECS045
05225                                                                   ECS045
05226      IF FINAL-TOT = 'Y'                                           ECS045
05227          MOVE SPACE              TO HDR-5-B.                      ECS045
05228      MOVE HDR-5-B                TO PRT                           ECS045
05229                                     PRT-2.                        ECS045
05230      MOVE ' '                    TO X.                            ECS045
05231      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05232      IF DTE-FMT-OPT EQUAL '2'                                     ECS045
05233          MOVE HDR-6-2            TO PRT-2                         ECS045
05234      ELSE                                                         ECS045
05235          MOVE HDR-6              TO PRT.                          ECS045
05236      MOVE '0'                    TO X.                            ECS045
05237      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05238                                                                   ECS045
05239      IF DTE-FMT-OPT EQUAL '2'                                     ECS045
05240          MOVE HDR-7-2            TO PRT-2                         ECS045
05241      ELSE                                                         ECS045
05242          MOVE HDR-7              TO PRT.                          ECS045
05243      MOVE ' '                    TO X.                            ECS045
05244      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05245      GO TO 0910-EXIT.                                             ECS045
05246                                                                   ECS045
05247      EJECT                                                        ECS045
05248  0907-HDR-RTN-RPT-D.                                              ECS045
05249                                                                   ECS045
05250 ***********************************                               ECS045
05251 *  045-D REPORT HEADINGS ROUTINE  *                               ECS045
05252 ***********************************                               ECS045
05253                                                                   ECS045
05254 ****  PRODUCE HEADING LINE (3) FOR REPORT  ****                   ECS045
05255                                                                   ECS045
05256      MOVE SPACES                 TO H3-COMMENT                    ECS045
05257                                     H3-COMP-NAME                  ECS045
05258                                     H3-COMMENT-1.                 ECS045
05259                                                                   ECS045
05260      IF HDR-SW LESS '2'                                           ECS045
05261         MOVE 'REINSURED BY - '   TO H3-COMMENT                    ECS045
05262         MOVE SAVE-R-B-REI-COMP-D TO H3-COMMENT-1                  ECS045
05263         MOVE SAVE-REI-NAME       TO H3-COMP-NAME.                 ECS045
05264                                                                   ECS045
05265      IF HDR-SW = '4'                                              ECS045
05266          MOVE 'FINAL TOTALS'     TO H3-COMMENT.                   ECS045
05267                                                                   ECS045
05268      MOVE HDR-3                  TO PRT                           ECS045
05269                                     PRT-2.                        ECS045
05270      MOVE ' '                    TO X.                            ECS045
05271      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05272                                                                   ECS045
05273 ****  PRODUCE HEADING LINE (3A) FOR REPORT  ****                  ECS045
05274                                                                   ECS045
05275      IF (HDR-SW LESS '2') AND (SAVE-CEDE-NAME NOT = SPACES)       ECS045
05276          MOVE SAVE-CEDE-NAME     TO H3A-COMP-NAME                 ECS045
05277          MOVE ' '                TO X                             ECS045
05278          MOVE HDR-3A             TO PRT                           ECS045
05279                                     PRT-2                         ECS045
05280          PERFORM 0920-PRT-RTN THRU 0930-EXIT.                     ECS045
05281                                                                   ECS045
05282 ****  PRODUCE HEADING LINE (4A AND 4B) FOR REPORT  ****           ECS045
05283                                                                   ECS045
05284      MOVE SPACES                 TO HDR-4-A                       ECS045
05285                                     HDR-4-B                       ECS045
05286                                     HDR-5-D.                      ECS045
05287                                                                   ECS045
05288      IF HDR-SW = '1'                                              ECS045
05289          MOVE 'REINS         REINS'     TO H4-A-DESC              ECS045
05290      ELSE                                                         ECS045
05291          IF HDR-SW LESS THAN '4'                                  ECS045
05292              MOVE 'REINS'               TO H4-A-DESC.             ECS045
05293                                                                   ECS045
05294      MOVE HDR-4-A                TO PRT                           ECS045
05295                                     PRT-2.                        ECS045
05296      MOVE '0'                    TO X.                            ECS045
05297      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05298                                                                   ECS045
05299      IF HDR-SW = '1'                                              ECS045
05300          MOVE 'GROUP CARRIER PRIME*'                              ECS045
05301                                  TO H4-B-DESC                     ECS045
05302          MOVE SAVE-R-B-GROUP-D   TO H5-D-REIN-GRP                 ECS045
05303          MOVE SAVE-R-B-CARR-D    TO H5-D-CARR                     ECS045
05304          MOVE SAVE-R-B-REI-COMP-PRIME-D TO H5-D-REIN-PRIME.       ECS045
05305                                                                   ECS045
05306      IF HDR-SW = '2'                                              ECS045
05307          MOVE 'GROUP CARRIER*'   TO H4-B-DESC                     ECS045
05308          MOVE SAVE-R-B-GROUP-D   TO H5-D-REIN-GRP                 ECS045
05309          MOVE SAVE-R-B-CARR-D    TO H5-D-CARR.                    ECS045
05310                                                                   ECS045
05311      IF HDR-SW = '3'                                              ECS045
05312          MOVE 'GROUP*'           TO H4-B-DESC                     ECS045
05313          MOVE SAVE-R-B-GROUP-D   TO H5-D-REIN-GRP.                ECS045
05314                                                                   ECS045
05315      MOVE HDR-4-B                TO PRT                           ECS045
05316                                     PRT-2.                        ECS045
05317      MOVE ' '                    TO X.                            ECS045
05318      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05319                                                                   ECS045
05320 ****  PRODUCE HEADING LINE (5 THRU 7) FOR REPORT  ****            ECS045
05321                                                                   ECS045
05322      IF FINAL-TOT = 'Y'                                           ECS045
05323          MOVE SPACE              TO HDR-5-D.                      ECS045
05324      MOVE HDR-5-D                TO PRT                           ECS045
05325                                     PRT-2.                        ECS045
05326      MOVE ' '                    TO X.                            ECS045
05327      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05328                                                                   ECS045
05329      IF DTE-FMT-OPT EQUAL '2'                                     ECS045
05330          MOVE HDR-6-2            TO PRT-2                         ECS045
05331      ELSE                                                         ECS045
05332          MOVE HDR-6              TO PRT.                          ECS045
05333      MOVE '0'                    TO X.                            ECS045
05334      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05335                                                                   ECS045
05336      IF DTE-FMT-OPT EQUAL '2'                                     ECS045
05337          MOVE HDR-7-2            TO PRT-2                         ECS045
05338      ELSE                                                         ECS045
05339          MOVE HDR-7              TO PRT.                          ECS045
05340      MOVE '0'                    TO X.                            ECS045
05341      MOVE ' '                    TO X.                            ECS045
05342      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05343      GO TO 0910-EXIT.                                             ECS045
05344                                                                   ECS045
05345      EJECT                                                        ECS045
05346  0909-HDR-RTN-RPT-E.                                              ECS045
05347                                                                   ECS045
05348 ***********************************                               ECS045
05349 *  045-E REPORT HEADINGS ROUTINE  *                               ECS045
05350 ***********************************                               ECS045
05351                                                                   ECS045
05352 ****  PRODUCE HEADING LINE (3) FOR REPORT  ****                   ECS045
05353                                                                   ECS045
05354      MOVE SPACES                 TO H3-COMMENT                    ECS045
05355                                     H3-COMP-NAME                  ECS045
05356                                     H3-COMMENT-1.                 ECS045
05357                                                                   ECS045
05358      IF HDR-SW LESS '3'                                           ECS045
05359         MOVE 'REINSURED BY - '   TO H3-COMMENT                    ECS045
05360         MOVE SAVE-R-B-REI-COMP-E TO H3-COMMENT-1                  ECS045
05361         MOVE SAVE-REI-NAME       TO H3-COMP-NAME.                 ECS045
05362                                                                   ECS045
05363      IF HDR-SW = '3'                                              ECS045
05364          MOVE 'FINAL TOTALS'     TO H3-COMMENT.                   ECS045
05365                                                                   ECS045
05366      MOVE HDR-3                  TO PRT                           ECS045
05367                                     PRT-2.                        ECS045
05368      MOVE ' '                    TO X.                            ECS045
05369      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05370                                                                   ECS045
05371 ****  PRODUCE HEADING LINE (3A) FOR REPORT  ****                  ECS045
05372                                                                   ECS045
05373      IF (HDR-SW LESS '3') AND (SAVE-CEDE-NAME NOT = SPACES)       ECS045
05374          MOVE SAVE-CEDE-NAME     TO H3A-COMP-NAME                 ECS045
05375          MOVE ' '                TO X                             ECS045
05376          MOVE HDR-3A             TO PRT                           ECS045
05377                                     PRT-2                         ECS045
05378          PERFORM 0920-PRT-RTN THRU 0930-EXIT.                     ECS045
05379                                                                   ECS045
05380 ****  PRODUCE HEADING LINE (4A AND 4B) FOR REPORT  ****           ECS045
05381                                                                   ECS045
05382      MOVE SPACES                 TO HDR-4-A                       ECS045
05383                                     HDR-4-B                       ECS045
05384                                     HDR-5-E.                      ECS045
05385                                                                   ECS045
05386      IF HDR-SW = '1'                                              ECS045
05387          MOVE 'REINS  REINS'            TO H4-A-DESC              ECS045
05388      ELSE                                                         ECS045
05389          IF HDR-SW = '2'                                          ECS045
05390              MOVE 'REINS'               TO H4-A-DESC.             ECS045
05391                                                                   ECS045
05392      MOVE HDR-4-A                TO PRT                           ECS045
05393                                     PRT-2.                        ECS045
05394      MOVE '0'                    TO X.                            ECS045
05395      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05396                                                                   ECS045
05397      IF HDR-SW = '1'                                              ECS045
05398          MOVE 'PRIME   SUB*'            TO H4-B-DESC              ECS045
05399          MOVE SAVE-R-B-REI-COMP-PRIME-E TO H5-E-REIN-PRIME        ECS045
05400          MOVE SAVE-R-B-REI-COMP-SUB-E   TO H5-E-REIN-SUB.         ECS045
05401                                                                   ECS045
05402      IF HDR-SW = '2'                                              ECS045
05403          MOVE 'PRIME*'                  TO H4-B-DESC              ECS045
05404          MOVE SAVE-R-B-REI-COMP-PRIME-E TO H5-E-REIN-PRIME.       ECS045
05405                                                                   ECS045
05406      MOVE HDR-4-B                TO PRT                           ECS045
05407                                     PRT-2.                        ECS045
05408      MOVE ' '                    TO X.                            ECS045
05409      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05410                                                                   ECS045
05411 ****  PRODUCE HEADING LINE (5 THRU 7) FOR REPORT  ****            ECS045
05412                                                                   ECS045
05413      IF FINAL-TOT = 'Y'                                           ECS045
05414          MOVE SPACE              TO HDR-5-E.                      ECS045
05415      MOVE HDR-5-E                TO PRT                           ECS045
05416                                     PRT-2.                        ECS045
05417      MOVE ' '                    TO X.                            ECS045
05418      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05419                                                                   ECS045
05420      IF DTE-FMT-OPT EQUAL '2'                                     ECS045
05421          MOVE HDR-6-2            TO PRT-2                         ECS045
05422      ELSE                                                         ECS045
05423          MOVE HDR-6              TO PRT.                          ECS045
05424      MOVE '0'                    TO X.                            ECS045
05425      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05426                                                                   ECS045
05427      IF DTE-FMT-OPT EQUAL '2'                                     ECS045
05428          MOVE HDR-7-2            TO PRT-2                         ECS045
05429      ELSE                                                         ECS045
05430          MOVE HDR-7              TO PRT.                          ECS045
05431      MOVE ' '                    TO X.                            ECS045
05432      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
05433                                                                   ECS045
05434  0910-EXIT.                                                       ECS045
05435      EXIT.                                                        ECS045
05436      EJECT                                                        ECS045
05437  0920-PRT-RTN.                                                    ECS045
CIDMOD                       COPY PRTN045.
05438                                                                   ECS045
CIDMOD*    IF DTE-FICH NOT = SPACE AND                                  ECS045
CIDMOD*       FICH-OPEN    = SPACE                                      ECS045
CIDMOD*        MOVE 'X' TO FICH-OPEN                                    ECS045
CIDMOD*        IF DTE-FMT-OPT EQUAL '2'                                 ECS045
CIDMOD*            OPEN OUTPUT FICH-2                                   ECS045
CIDMOD*        ELSE                                                     ECS045
CIDMOD*            OPEN OUTPUT FICH.                                    ECS045
CIDMOD*                                                                 ECS045
CIDMOD*    IF DTE-FICH NOT = SPACE                                      ECS045
CIDMOD*        IF DTE-FMT-OPT EQUAL '2'                                 ECS045
CIDMOD*            MOVE X TO P-CTL-2                                    ECS045
CIDMOD*            WRITE FICH-REC-2 FROM PRT-2                          ECS045
CIDMOD*        ELSE                                                     ECS045
CIDMOD*            MOVE X TO P-CTL                                      ECS045
CIDMOD*            WRITE FICH-REC   FROM PRT.                           ECS045
CIDMOD*                                                                 ECS045
CIDMOD*    IF DTE-FICH = SPACE OR '2'                                   ECS045
CIDMOD*        IF DTE-FMT-OPT EQUAL '2'                                 ECS045
CIDMOD*            MOVE X TO P-CTL-2                                    ECS045
CIDMOD*            PERFORM 0938-PRINT-ROUTINE THRU 0938-PRINT-EXIT      ECS045
CIDMOD*        ELSE                                                     ECS045
CIDMOD*            MOVE X TO P-CTL                                      ECS045
CIDMOD*            PERFORM 0939-PRINT-ROUTINE THRU 0939-PRINT-EXIT.     ECS045
05462                                                                   ECS045
05463  0930-EXIT.                                                       ECS045
05464      EXIT.                                                        ECS045
05465                                                                   ECS045
05466      EJECT                                                        ECS045
CIDMOD*0938-PRINT-ROUTINE.                                              ECS045
CIDMOD*                                                                 ECS045
CIDMOD*    IF P-CTL-2 EQUAL ' '                                         ECS045
CIDMOD*        WRITE PRT-2 AFTER ADVANCING 1 LINE                       ECS045
CIDMOD*    ELSE                                                         ECS045
CIDMOD*        IF P-CTL-2 EQUAL '0'                                     ECS045
CIDMOD*            WRITE PRT-2 AFTER ADVANCING 2 LINE                   ECS045
CIDMOD*        ELSE                                                     ECS045
CIDMOD*            IF P-CTL-2 EQUAL '-'                                 ECS045
CIDMOD*                WRITE PRT-2 AFTER ADVANCING 3 LINE               ECS045
CIDMOD*            ELSE                                                 ECS045
CIDMOD*                WRITE PRT-2 AFTER ADVANCING PAGE.                ECS045
CIDMOD*                                                                 ECS045
CIDMOD*0938-PRINT-EXIT.                                                 ECS045
CIDMOD*                                                                 ECS045
CIDMOD*0939-PRINT-ROUTINE.                                              ECS045
CIDMOD*                                                                 ECS045
CIDMOD*    IF P-CTL   EQUAL ' '                                         ECS045
CIDMOD*        WRITE PRT   AFTER ADVANCING 1 LINE                       ECS045
CIDMOD*    ELSE                                                         ECS045
CIDMOD*        IF P-CTL   EQUAL '0'                                     ECS045
CIDMOD*            WRITE PRT   AFTER ADVANCING 2 LINE                   ECS045
CIDMOD*        ELSE                                                     ECS045
CIDMOD*            IF P-CTL   EQUAL '-'                                 ECS045
CIDMOD*                WRITE PRT   AFTER ADVANCING 3 LINE               ECS045
CIDMOD*            ELSE                                                 ECS045
CIDMOD*                WRITE PRT   AFTER ADVANCING PAGE.                ECS045
CIDMOD*                                                                 ECS045
CIDMOD*0939-PRINT-EXIT.                                                 ECS045
05496                                                                   ECS045
05497  0940-RETURN-SW-FILE.                                             ECS045
05498                                                                   ECS045
05499      MOVE 'N'                    TO LIFE-PROCESS-SWITCH.          ECS045
05500                                                                   ECS045
05501      RETURN SORT-WORK INTO RR-REC                                 ECS045
05502          AT END MOVE HIGH-VALUES TO RR-REC                        ECS045
05503                                     R-B-REI-COMP                  ECS045
05504                                     R-B-CARR-B                    ECS045
05505                                     R-B-GROUP-D                   ECS045
05506                                     R-B-REI-COMP-E                ECS045
05507                 GO TO 0950-EXIT.                                  ECS045
05508                                                                   ECS045
05509      ADD +1                      TO EPECS-RETURNED-SORT1.         ECS045
05510                                                                   ECS045
05511      IF RR-REPORT-ID-B                                            ECS045
05512          MOVE RR-REPORT-A-CNTL    TO SAVE-RR-REPORT-CNTL          ECS045
05513          MOVE RR-REPORT-B-CNTL    TO RR-REPORT-A-CNTL             ECS045
05514          MOVE SAVE-RR-REPORT-CNTL TO RR-REPORT-B-CNTL.            ECS045
05515                                                                   ECS045
05516      IF RR-REPORT-ID-D                                            ECS045
05517          MOVE RR-REPORT-A-CNTL    TO SAVE-RR-REPORT-CNTL          ECS045
05518          MOVE RR-REPORT-D-CNTL    TO RR-REPORT-A-CNTL             ECS045
05519          MOVE SAVE-RR-REPORT-CNTL TO RR-REPORT-D-CNTL.            ECS045
05520                                                                   ECS045
05521      IF RR-REPORT-ID-E                                            ECS045
05522          MOVE RR-REPORT-A-CNTL    TO SAVE-RR-REPORT-CNTL          ECS045
05523          MOVE RR-REPORT-E-CNTL    TO RR-REPORT-A-CNTL             ECS045
05524          MOVE SAVE-RR-REPORT-CNTL TO RR-REPORT-E-CNTL.            ECS045
05525                                                                   ECS045
05526      MOVE +1                      TO RX.                          ECS045
05527      PERFORM 0960-GET-FEE-PERCENTS THRU 0989-EXIT.                ECS045
05528                                                                   ECS045
05529      MOVE SAVE-LF-FEE            TO RR-FEE-LF.                    ECS045
05530      MOVE SAVE-AH-FEE            TO RR-FEE-AH.                    ECS045
05531                                                                   ECS045
05532      IF RR-0-PE-FEE-LF = 'Y'                                      ECS045
05533          MOVE ZEROS              TO RR-FEE-LF.                    ECS045
05534      IF RR-0-PE-FEE-AH = 'Y'                                      ECS045
05535          MOVE ZEROS              TO RR-FEE-AH.                    ECS045
05536                                                                   ECS045
05537      IF RR-REPORT-ID-A                                            ECS045
05538          MOVE RR-REI-COMP        TO R-B-REI-COMP                  ECS045
05539          MOVE RR-A-REI-GRP-A     TO R-B-GROUP                     ECS045
05540          MOVE RR-A-CARR          TO R-B-CARR                      ECS045
05541          MOVE RR-A-CERT-GROUP    TO R-B-COMP                      ECS045
05542          MOVE RR-A-STATE         TO R-B-STATE                     ECS045
05543          MOVE RR-A-ACCT          TO R-B-ACCT                      ECS045
05544          MOVE RR-A-EXP-DT        TO R-B-EXP                       ECS045
05545          MOVE RR-A-EFF-DT        TO R-B-EFF.                      ECS045
05546                                                                   ECS045
05547      IF RR-REPORT-ID-B                                            ECS045
05548          MOVE RR-REI-COMP        TO R-B-REI-COMP-B                ECS045
05549          MOVE RR-B-REI-GRP-B     TO R-B-GROUP-B                   ECS045
05550          MOVE RR-B-CARR          TO R-B-CARR-B                    ECS045
05551          MOVE RR-B-STATE         TO R-B-STATE-B.                  ECS045
05552                                                                   ECS045
05553      IF RR-REPORT-ID-D                                            ECS045
05554          MOVE RR-REI-COMP        TO R-B-REI-COMP-D                ECS045
05555          MOVE RR-D-REI-GRP       TO R-B-GROUP-D                   ECS045
05556          MOVE RR-D-CARR          TO R-B-CARR-D.                   ECS045
05557                                                                   ECS045
05558      IF RR-REPORT-ID-E                                            ECS045
05559          MOVE RR-REI-COMP        TO R-B-REI-COMP-E.               ECS045
05560                                                                   ECS045
05561      IF RR-LF-PR-PCT GREATER THAN ZERO OR                         ECS045
05562         RR-LF-78-PCT GREATER THAN ZERO                            ECS045
05563             MOVE 'Y'             TO LIFE-PROCESS-SWITCH.          ECS045
05564                                                                   ECS045
05565  0950-EXIT.                                                       ECS045
05566      EXIT.                                                        ECS045
05567                                                                   ECS045
05568      EJECT                                                        ECS045
05569  0960-GET-FEE-PERCENTS.                                           ECS045
05570      IF RCT-REIN-CO (RX) = HIGH-VALUES OR                         ECS045
05571         RX GREATER THAN RCT-ENTRY-COUNT                           ECS045
05572          MOVE RR-FEE-LF          TO SAVE-LF-FEE                   ECS045
05573          MOVE RR-FEE-AH          TO SAVE-AH-FEE                   ECS045
05574          GO TO 0989-EXIT.                                         ECS045
05575                                                                   ECS045
05576      IF RR-REI-COMP NOT = RCT-REIN-CO (RX)                        ECS045
05577          ADD +1                  TO RX                            ECS045
05578          GO TO 0960-GET-FEE-PERCENTS.                             ECS045
05579                                                                   ECS045
05580  0970-GET-LIFE-FEE.                                               ECS045
05581      IF NOT RCT-LF-FEE-BRACKETED (RX)                             ECS045
05582          MOVE RR-FEE-LF          TO SAVE-LF-FEE                   ECS045
05583          GO TO 0980-GET-A-H-FEE.                                  ECS045
05584                                                                   ECS045
05585      IF RCT-LF-GROSS-CEDED (RX)                                   ECS045
05586          MOVE RCT-LF-ISS-CPRM (RX) TO REIN-PREMIUMS.              ECS045
05587                                                                   ECS045
05588      IF RCT-LF-NET-CEDED (RX)                                     ECS045
05589          COMPUTE REIN-PREMIUMS =                                  ECS045
05590                 (RCT-LF-ISS-CPRM (RX) - RCT-LF-CAN-CPRM (RX)).    ECS045
05591                                                                   ECS045
05592      IF RCT-LF-GROSS-WRITTEN (RX)                                 ECS045
05593          MOVE RCT-LF-ISS-GPRM (RX) TO REIN-PREMIUMS.              ECS045
05594                                                                   ECS045
05595      IF RCT-LF-NET-WRITTEN (RX)                                   ECS045
05596          COMPUTE REIN-PREMIUMS =                                  ECS045
05597                 (RCT-LF-ISS-GPRM (RX) - RCT-LF-CAN-GPRM (RX)).    ECS045
05598                                                                   ECS045
05599      IF RCT-LF-COMBINE-GROSS-CEDED (RX)                           ECS045
05600          COMPUTE REIN-PREMIUMS =                                  ECS045
05601                 (RCT-LF-ISS-CPRM (RX) + RCT-AH-ISS-CPRM (RX)).    ECS045
05602                                                                   ECS045
05603      IF RCT-LF-COMBINE-NET-CEDED (RX)                             ECS045
05604          COMPUTE REIN-PREMIUMS =                                  ECS045
05605                 (RCT-LF-ISS-CPRM (RX) - RCT-LF-CAN-CPRM (RX)) +   ECS045
05606                 (RCT-AH-ISS-CPRM (RX) - RCT-AH-CAN-CPRM (RX)).    ECS045
05607                                                                   ECS045
05608      IF RCT-LF-COMBINE-GROSS-WRITTEN (RX)                         ECS045
05609          COMPUTE REIN-PREMIUMS =                                  ECS045
05610                 (RCT-LF-ISS-GPRM (RX) + RCT-AH-ISS-GPRM (RX)).    ECS045
05611                                                                   ECS045
05612      IF RCT-LF-COMBINE-NET-WRITTEN (RX)                           ECS045
05613          COMPUTE REIN-PREMIUMS =                                  ECS045
05614                 (RCT-LF-ISS-GPRM (RX) - RCT-LF-CAN-GPRM (RX)) +   ECS045
05615                 (RCT-AH-ISS-GPRM (RX) - RCT-AH-CAN-GPRM (RX)).    ECS045
05616                                                                   ECS045
05617      IF REIN-PREMIUMS NOT GREATER THAN ZERO                       ECS045
05618          MOVE RCT-LF-FEE-RANGE-PCT (RX 1) TO SAVE-LF-FEE          ECS045
05619          GO TO 0980-GET-A-H-FEE.                                  ECS045
05620                                                                   ECS045
05621      MOVE ZEROS TO FEE-AMOUNT                                     ECS045
05622                    LAST-FEE-THRU-AMT.                             ECS045
05623                                                                   ECS045
05624      MOVE +1 TO FR.                                               ECS045
05625      IF RCT-LF-FEE-METHOD-2 (RX)                                  ECS045
05626          GO TO 0975-CALCULATE-LF-FEE-AMOUNT.                      ECS045
05627                                                                   ECS045
05628  0973-LF-FEE-BY-VOLUME.                                           ECS045
05629      IF REIN-PREMIUMS NOT GREATER THAN RCT-LF-FEE-THRU-AMT (RX FR)ECS045
05630          MOVE RCT-LF-FEE-RANGE-PCT (RX FR) TO SAVE-LF-FEE         ECS045
05631          GO TO 0980-GET-A-H-FEE.                                  ECS045
05632                                                                   ECS045
05633      ADD +1                      TO FR.                           ECS045
05634      IF FR GREATER THAN +6                                        ECS045
05635          MOVE RCT-LF-FEE-RANGE-PCT (RX 6) TO SAVE-LF-FEE          ECS045
05636          GO TO 0980-GET-A-H-FEE.                                  ECS045
05637                                                                   ECS045
05638      GO TO 0973-LF-FEE-BY-VOLUME.                                 ECS045
05639                                                                   ECS045
05640  0975-CALCULATE-LF-FEE-AMOUNT.                                    ECS045
05641      IF REIN-PREMIUMS GREATER THAN RCT-LF-FEE-THRU-AMT (RX FR)    ECS045
05642          COMPUTE FEE-AMOUNT ROUNDED =                             ECS045
05643              FEE-AMOUNT + (RCT-LF-FEE-RANGE-PCT (RX FR) *         ECS045
05644              (RCT-LF-FEE-THRU-AMT (RX FR) - LAST-FEE-THRU-AMT))   ECS045
05645          MOVE RCT-LF-FEE-THRU-AMT (RX FR) TO LAST-FEE-THRU-AMT    ECS045
05646      ELSE                                                         ECS045
05647          COMPUTE FEE-AMOUNT ROUNDED =                             ECS045
05648                  FEE-AMOUNT + (RCT-LF-FEE-RANGE-PCT (RX FR)       ECS045
05649                 * (REIN-PREMIUMS - LAST-FEE-THRU-AMT))            ECS045
05650          GO TO 0977-CALCULATE-LF-FEE-PERCENTS.                    ECS045
05651                                                                   ECS045
05652      ADD +1                      TO FR.                           ECS045
05653                                                                   ECS045
05654      IF FR LESS THAN +7                                           ECS045
05655          GO TO 0975-CALCULATE-LF-FEE-AMOUNT.                      ECS045
05656                                                                   ECS045
05657  0977-CALCULATE-LF-FEE-PERCENTS.                                  ECS045
05658      COMPUTE SAVE-LF-FEE ROUNDED = FEE-AMOUNT / REIN-PREMIUMS.    ECS045
05659                                                                   ECS045
05660  0980-GET-A-H-FEE.                                                ECS045
05661      IF NOT RCT-AH-FEE-BRACKETED (RX)                             ECS045
05662          MOVE RR-FEE-AH TO SAVE-AH-FEE                            ECS045
05663          GO TO 0989-EXIT.                                         ECS045
05664                                                                   ECS045
05665      IF RCT-AH-GROSS-CEDED (RX)                                   ECS045
05666          MOVE RCT-AH-ISS-CPRM (RX) TO REIN-PREMIUMS.              ECS045
05667                                                                   ECS045
05668      IF RCT-AH-NET-CEDED (RX)                                     ECS045
05669          COMPUTE REIN-PREMIUMS =                                  ECS045
05670                 (RCT-AH-ISS-CPRM (RX) - RCT-AH-CAN-CPRM (RX)).    ECS045
05671                                                                   ECS045
05672      IF RCT-AH-GROSS-WRITTEN (RX)                                 ECS045
05673          MOVE RCT-AH-ISS-GPRM (RX) TO REIN-PREMIUMS.              ECS045
05674                                                                   ECS045
05675      IF RCT-AH-NET-WRITTEN (RX)                                   ECS045
05676          COMPUTE REIN-PREMIUMS =                                  ECS045
05677                 (RCT-AH-ISS-GPRM (RX) - RCT-AH-CAN-GPRM (RX)).    ECS045
05678                                                                   ECS045
05679      IF RCT-AH-COMBINE-GROSS-CEDED (RX)                           ECS045
05680          COMPUTE REIN-PREMIUMS =                                  ECS045
05681                 (RCT-LF-ISS-CPRM (RX) + RCT-AH-ISS-CPRM (RX)).    ECS045
05682                                                                   ECS045
05683      IF RCT-AH-COMBINE-NET-CEDED (RX)                             ECS045
05684          COMPUTE REIN-PREMIUMS =                                  ECS045
05685                 (RCT-LF-ISS-CPRM (RX) - RCT-LF-CAN-CPRM (RX)) +   ECS045
05686                 (RCT-AH-ISS-CPRM (RX) - RCT-AH-CAN-CPRM (RX)).    ECS045
05687                                                                   ECS045
05688      IF RCT-AH-COMBINE-GROSS-WRITTEN (RX)                         ECS045
05689          COMPUTE REIN-PREMIUMS =                                  ECS045
05690                 (RCT-LF-ISS-GPRM (RX) + RCT-AH-ISS-GPRM (RX)).    ECS045
05691                                                                   ECS045
05692      IF RCT-AH-COMBINE-NET-WRITTEN (RX)                           ECS045
05693          COMPUTE REIN-PREMIUMS =                                  ECS045
05694                 (RCT-LF-ISS-GPRM (RX) - RCT-LF-CAN-GPRM (RX)) +   ECS045
05695                 (RCT-AH-ISS-GPRM (RX) - RCT-AH-CAN-GPRM (RX)).    ECS045
05696                                                                   ECS045
05697      IF REIN-PREMIUMS NOT GREATER THAN ZERO                       ECS045
05698          MOVE RCT-AH-FEE-RANGE-PCT (RX 1) TO SAVE-AH-FEE          ECS045
05699          GO TO 0989-EXIT.                                         ECS045
05700                                                                   ECS045
05701      MOVE ZEROS TO FEE-AMOUNT                                     ECS045
05702                    LAST-FEE-THRU-AMT.                             ECS045
05703                                                                   ECS045
05704      MOVE +1 TO FR.                                               ECS045
05705                                                                   ECS045
05706      IF RCT-AH-FEE-METHOD-2 (RX)                                  ECS045
05707          GO TO 0985-CALCULATE-AH-FEE-AMOUNT.                      ECS045
05708                                                                   ECS045
05709  0983-AH-FEE-BY-VOLUME.                                           ECS045
05710      IF REIN-PREMIUMS NOT GREATER THAN RCT-AH-FEE-THRU-AMT (RX FR)ECS045
05711          MOVE RCT-AH-FEE-RANGE-PCT (RX FR) TO SAVE-AH-FEE         ECS045
05712          GO TO 0989-EXIT.                                         ECS045
05713                                                                   ECS045
05714      ADD +1                      TO FR.                           ECS045
05715      IF FR GREATER THAN +6                                        ECS045
05716          MOVE RCT-AH-FEE-RANGE-PCT (RX 6) TO SAVE-AH-FEE          ECS045
05717          GO TO 0989-EXIT.                                         ECS045
05718                                                                   ECS045
05719      GO TO 0983-AH-FEE-BY-VOLUME.                                 ECS045
05720                                                                   ECS045
05721  0985-CALCULATE-AH-FEE-AMOUNT.                                    ECS045
05722      IF REIN-PREMIUMS GREATER THAN RCT-AH-FEE-THRU-AMT (RX FR)    ECS045
05723          COMPUTE FEE-AMOUNT ROUNDED =                             ECS045
05724              FEE-AMOUNT + (RCT-AH-FEE-RANGE-PCT (RX FR)           ECS045
05725              * (RCT-AH-FEE-THRU-AMT (RX FR) - LAST-FEE-THRU-AMT)) ECS045
05726          MOVE RCT-AH-FEE-THRU-AMT (RX FR) TO LAST-FEE-THRU-AMT    ECS045
05727      ELSE                                                         ECS045
05728          COMPUTE FEE-AMOUNT ROUNDED =                             ECS045
05729                  FEE-AMOUNT + (RCT-AH-FEE-RANGE-PCT (RX FR)       ECS045
05730                 * (REIN-PREMIUMS - LAST-FEE-THRU-AMT))            ECS045
05731          GO TO 0987-CALCULATE-AH-FEE-PERCENTS.                    ECS045
05732                                                                   ECS045
05733      ADD +1                      TO FR.                           ECS045
05734                                                                   ECS045
05735      IF FR LESS THAN +7                                           ECS045
05736          GO TO 0985-CALCULATE-AH-FEE-AMOUNT.                      ECS045
05737                                                                   ECS045
05738  0987-CALCULATE-AH-FEE-PERCENTS.                                  ECS045
05739      COMPUTE SAVE-AH-FEE ROUNDED = FEE-AMOUNT / REIN-PREMIUMS.    ECS045
05740                                                                   ECS045
05741  0989-EXIT.                                                       ECS045
05742      EXIT.                                                        ECS045
05743                                                                   ECS045
05744      EJECT                                                        ECS045
05745  0990-END-PRINT-STATEMENT.                                        ECS045
05746                                                                   ECS045
05747      CLOSE REIN-WORK                                              ECS045
05748            REIN-TBL-FILE.                                         ECS045
05749                                                                   ECS045
05750      IF REIN-FILE-STATUS NOT = '00'                               ECS045
05751          MOVE '**** BAD CLOSE REIN-TBL-FILE   ****'               ECS045
05752                                  TO WS-ABEND-MESSAGE              ECS045
05753          MOVE '2'                TO WAC-1                         ECS045
05754                                     WAC-2                         ECS045
05755          MOVE REIN-FILE-STATUS   TO WAC-3-4                       ECS045
05756          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS045
05757          GO TO ABEND-PGM.                                         ECS045
05758                                                                   ECS045
05759  0990-PS-EXIT.                                                    ECS045
05760      EXIT.                                                        ECS045
05761      EJECT                                                        ECS045
05762  1000-SELECT-RECAP SECTION.                                       ECS045
05763                                                                   ECS045
05764      OPEN INPUT REIN-WORK.                                        ECS045
05765                                                                   ECS045
05766      MOVE HDR-1A                 TO HDR-1.                        ECS045
05767      MOVE 'C'                    TO H1-RUN-CODE.                  ECS045
05768      MOVE +0                     TO PAGE-CNT.                     ECS045
05769      MOVE SPACES                 TO H2-COMMENT-1                  ECS045
05770                                     H2-CARR-1.                    ECS045
05771      MOVE HDR-3AX                TO HDR-3A.                       ECS045
05772                                                                   ECS045
05773   1000-RECAP-LOOP.                                                ECS045
05774      READ REIN-WORK INTO RR-REC                                   ECS045
05775          AT END  GO TO 1900-END-SELECT.                           ECS045
05776                                                                   ECS045
05777      MOVE RR-REC                 TO SW-REC.                       ECS045
05778      MOVE 'F'                    TO SW-C-TYPE.                    ECS045
05779                                                                   ECS045
05780      RELEASE SW-REC.                                              ECS045
05781                                                                   ECS045
05782      MOVE RR-ACCUMS              TO DETAIL-ACCUMULATORS.          ECS045
05783      MOVE +1                     TO SA.                           ECS045
05784                                                                   ECS045
05785  1000-NEG-LOOP.                                                   ECS045
05786      COMPUTE DA-ISP  (SA)  = DA-ISP  (SA) * -1.                   ECS045
05786      COMPUTE DA-TAX  (SA)  = DA-TAX  (SA) * -1.                   ECS045
05787      COMPUTE DA-CNP  (SA)  = DA-CNP  (SA) * -1.                   ECS045
05788      COMPUTE DA-UPR  (SA)  = DA-UPR  (SA) * -1.                   ECS045
05789      COMPUTE DA-UPP  (SA)  = DA-UPP  (SA) * -1.                   ECS045
05790      COMPUTE DA-UP   (SA)  = DA-UP   (SA) * -1.                   ECS045
05791      COMPUTE DA-CLM  (SA)  = DA-CLM  (SA) * -1.                   ECS045
05792      COMPUTE DA-ISC  (SA)  = DA-ISC  (SA) * -1.                   ECS045
05793      COMPUTE DA-CNC  (SA)  = DA-CNC  (SA) * -1.                   ECS045
05794      COMPUTE DA-UCR  (SA)  = DA-UCR  (SA) * -1.                   ECS045
05795      COMPUTE DA-UCP  (SA)  = DA-UCP  (SA) * -1.                   ECS045
05796      COMPUTE DA-UC   (SA)  = DA-UC   (SA) * -1.                   ECS045
05797      COMPUTE DA-ISO  (SA)  = DA-ISO  (SA) * -1.                   ECS045
05798      COMPUTE DA-CNO  (SA)  = DA-CNO  (SA) * -1.                   ECS045
05799      COMPUTE DA-UOR  (SA)  = DA-UOR  (SA) * -1.                   ECS045
05800      COMPUTE DA-UOP  (SA)  = DA-UOP  (SA) * -1.                   ECS045
05801      COMPUTE DA-UO   (SA)  = DA-UO   (SA) * -1.                   ECS045
05802      COMPUTE DA-ADJ  (SA)  = DA-ADJ  (SA) * -1.                   ECS045
05803      COMPUTE DA-MORT (SA)  = DA-MORT (SA) * -1.                   ECS045
05804      COMPUTE DA-CA   (SA)  = DA-CA   (SA) * -1.                   ECS045
05805      COMPUTE DA-CF   (SA)  = DA-CF   (SA) * -1.                   ECS045
05806      COMPUTE DA-CU   (SA)  = DA-CU   (SA) * -1.                   ECS045
05807      COMPUTE DA-IF   (SA)  = DA-IF   (SA) * -1.                   ECS045
05808                                                                   ECS045
05809      ADD +1                      TO SA.                           ECS045
05810                                                                   ECS045
05811      IF SA NOT = +7                                               ECS045
05812          GO TO 1000-NEG-LOOP.                                     ECS045
05813                                                                   ECS045
05814      MOVE DETAIL-ACCUMULATORS    TO RR-ACCUMS.                    ECS045
05815      MOVE RR-REC                 TO SW-REC.                       ECS045
05816      MOVE 'T'                    TO SW-C-TYPE.                    ECS045
05817      MOVE SW-C-FROM              TO SW-C-TO.                      ECS045
05818      MOVE RR-C-CEDE-NAME         TO SW-C-FROM.                    ECS045
05819                                                                   ECS045
05820      RELEASE SW-REC.                                              ECS045
05821                                                                   ECS045
05822      GO TO 1000-RECAP-LOOP.                                       ECS045
05823                                                                   ECS045
05824  1100-STATE-CODE-LOOKUP.                                          ECS045
05825                                                                   ECS045
05826      IF  CLAS-INDEXS  NOT GREATER  CLAS-MAXS   AND                ECS045
05827          CLAS-INDEXS  NOT  =  ZERO                                ECS045
05828          IF  STATE-L    =    STATE-SUB (CLAS-INDEXS)              ECS045
05829              GO TO  1100-STATE-LOOKUP-X.                          ECS045
05830                                                                   ECS045
05831      MOVE  CLAS-STARTS               TO  CLAS-INDEXS.             ECS045
05832                                                                   ECS045
05833  1110-STATE-LOOKUP-LOOP.                                          ECS045
05834                                                                   ECS045
05835      IF  CLAS-INDEXS  GREATER  CLAS-MAXS  OR  CLAS-INDEXS = ZERO  ECS045
05836          MOVE  0302                  TO  WS-ABEND-CODE            ECS045
05837          DISPLAY  'STATE CODE ' STATE-L ' NOT IN STATE TABLE'     ECS045
05838          MOVE  WS-ABEND-CODE         TO  WS-RETURN-CODE           ECS045
05839          GO TO  ABEND-PGM.                                        ECS045
05840                                                                   ECS045
05841      IF  STATE-L   NOT =  STATE-SUB (CLAS-INDEXS)                 ECS045
05842          ADD  +1                     TO  CLAS-INDEXS              ECS045
05843          GO TO  1110-STATE-LOOKUP-LOOP.                           ECS045
05844                                                                   ECS045
05845  1100-STATE-LOOKUP-X.                                             ECS045
05846      EXIT.                                                        ECS045
05847  1900-END-SELECT.                                                 ECS045
05848      CLOSE REIN-WORK.                                             ECS045
05849                                                                   ECS045
05850  1999-EXIT.                                                       ECS045
05851      EXIT.                                                        ECS045
05852                                                                   ECS045
05853      EJECT                                                        ECS045
05854  2000-PRINT-RECAP SECTION.                                        ECS045
05855                                                                   ECS045
05856      PERFORM 0440-ZERO-PAA THRU 0450-EXIT VARYING SA              ECS045
05857              FROM +1 BY +1 UNTIL SA GREATER +81.                  ECS045
05858      PERFORM 0460-ZERO-PAB THRU 0470-EXIT VARYING SA              ECS045
05859              FROM +1 BY +1 UNTIL SA GREATER +54.                  ECS045
05860                                                                   ECS045
05861  2000-GET-RECAP.                                                  ECS045
05862      RETURN SORT-WORK INTO RR-REC                                 ECS045
05863          AT END MOVE HIGH-VALUES TO RR-REC                        ECS045
05864                                     RR-CONTROL-B.                 ECS045
05865                                                                   ECS045
05866      MOVE 'N'                    TO LIFE-PROCESS-SWITCH.          ECS045
05867      MOVE +1                     TO RX.                           ECS045
05868                                                                   ECS045
05869  2000-REIN-TABLE-SEARCH.                                          ECS045
05870      IF RCT-REIN-CO (RX) = HIGH-VALUES OR                         ECS045
05871          RX GREATER THAN RCT-ENTRY-COUNT                          ECS045
05872              GO TO 2000-GET-RECAP-X.                              ECS045
05873                                                                   ECS045
05874      IF RR-REI-COMP NOT = RCT-REIN-CO (RX)                        ECS045
05875          ADD +1                  TO RX                            ECS045
05876          GO TO 2000-REIN-TABLE-SEARCH.                            ECS045
05877                                                                   ECS045
05878  2000-GET-RECAP-X.                                                ECS045
05879      EXIT.                                                        ECS045
05880                                                                   ECS045
05881  2000-SET-CTL.                                                    ECS045
05882      IF RR-REI-COMP = HIGH-VALUES                                 ECS045
05883          GO TO 2999-EXIT.                                         ECS045
05884                                                                   ECS045
05885      MOVE RR-REI-COMP            TO R-B-REI-COMP.                 ECS045
05886      MOVE RR-A-REI-GRP-A         TO R-B-GROUP.                    ECS045
05887      MOVE RR-A-CARR              TO R-B-CARR.                     ECS045
05888      MOVE RR-A-CERT-GROUP        TO R-B-COMP.                     ECS045
05889      MOVE RR-A-STATE             TO R-B-STATE.                    ECS045
05890      MOVE RR-A-ACCT              TO R-B-ACCT.                     ECS045
05891      MOVE RR-A-EXP-DT            TO R-B-EXP.                      ECS045
05892      MOVE RR-A-EFF-DT            TO R-B-EFF.                      ECS045
05893      GO TO 2200-ACCUM-DETAIL.                                     ECS045
05894                                                                   ECS045
05895  2100-CHECK-CONTROL.                                              ECS045
05896      IF RR-C-REI-NAME NOT = SAVE-REI-NAME                         ECS045
05897          PERFORM 2400-PRT-TO THRU 2499-EXIT                       ECS045
05898          MOVE '* NET OVER-ALL *' TO SAVE-CEDE-NAME                ECS045
061008         MOVE SAVE-REI-NAME           TO DO-REIN-BY-NAME
061008         MOVE 'OVER-ALL TOTAL'        TO DO-ACCOUNT-NAME
061008         MOVE '3'                     TO DO-TOTAL-TYPE
061008         MOVE SAVE-CLAIM-CODE         TO DO-CLAIM-PI
020612         MOVE SAVE-REI-NAME           TO DOA-REIN-BY-NAME
020612         MOVE 'OVER-ALL TOTAL'        TO DOA-ACCOUNT-NAME
020612         MOVE '3'                     TO DOA-TOTAL-TYPE
020612         MOVE SAVE-CLAIM-CODE         TO DOA-CLAIM-PI
020612         MOVE SAVE-LF-PE              TO DOA-LF-PE
020612         MOVE SAVE-AH-PE              TO DOA-AH-PE
05899          PERFORM 2500-PRT-FROM THRU 2599-EXIT.                    ECS045
05900                                                                   ECS045
05901      IF RR-C-CEDE-NAME NOT = SAVE-CEDE-NAME                       ECS045
05902          PERFORM 2400-PRT-TO THRU 2499-EXIT.                      ECS045
05903                                                                   ECS045
05904  2200-ACCUM-DETAIL.                                               ECS045
05905      IF R-B-REI-COMP = HIGH-VALUES                                ECS045
05906          MOVE 'Y'                TO FINAL-TOT                     ECS045
05907          PERFORM 0850-FINAL-TOTALS THRU 0855-EXIT                 ECS045
05908          GO TO 2999-EXIT.                                         ECS045
05909                                                                   ECS045
05910      MOVE RR-CONTROL-B           TO SAVE-RR-CONTROL-B.            ECS045
05911      MOVE RR-ACC-NAME            TO SAVE-ACC-NAME.                ECS045
05912      MOVE RR-C-REI-NAME          TO SAVE-REI-NAME.                ECS045
05913      MOVE RR-C-CEDE-NAME         TO SAVE-CEDE-NAME.               ECS045
05914      MOVE RR-CLAIM-CODE          TO SAVE-CLAIM-CODE.              ECS045
05915      MOVE RR-PRT-ST              TO SAVE-PRT-ST.                  ECS045
05916      MOVE RR-PRT-OW              TO SAVE-PRT-OW.                  ECS045
05917      MOVE RR-PRT-CRSV            TO SAVE-PRT-CRSV.                ECS045
05918      MOVE RR-ACCUMS              TO DETAIL-ACCUMULATORS.          ECS045
020612     MOVE RR-PE-LF               TO SAVE-LF-PE.
020612     MOVE RR-PE-AH               TO SAVE-AH-PE.
CIDMOD                                                                  ECS045
CIDMOD     IF RR-MORT = LOW-VALUES                                      ECS045
CIDMOD         MOVE SPACES             TO RR-MORT.                      ECS045
05919                                                                   ECS045
05920      IF RR-MORT = SPACES                                          ECS045
05921          MOVE WS-MORT-STD        TO DT-ENTRY (21)                 ECS045
05922                                     DT-ENTRY-2 (21)               ECS045
05923          GO TO 2230-ACCUM-LEVEL-1-DETAIL.                         ECS045
05924                                                                   ECS045
05925      MOVE CLAS-STARTM            TO CLAS-INDEXM.                  ECS045
05926                                                                   ECS045
05927 *****************************                                     ECS045
05928 *  VALIDATE MORTALITY CODE  *                                     ECS045
05929 *****************************                                     ECS045
05930                                                                   ECS045
05931  2220-FIND-MORT-LOOP.                                             ECS045
05932      IF CLAS-INDEXM GREATER CLAS-MAXM OR                          ECS045
05933         CLAS-INDEXM = +0                                          ECS045
05934          DISPLAY 'INVALID MORTALITY TABLE CODE (' RR-MORT ')'     ECS045
05935          MOVE '**** INVALID MORTALITY TABLE CODE ****'            ECS045
05936                                  TO WS-ABEND-MESSAGE              ECS045
05937          MOVE '0'                TO WAC-1                         ECS045
05938          MOVE '3'                TO WAC-2                         ECS045
05939          MOVE '01'               TO WAC-3-4                       ECS045
05940          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS045
05941          GO TO ABEND-PGM.                                         ECS045
05942                                                                   ECS045
05943      IF RR-MORT NOT = CLAS-MORT-CODE (CLAS-INDEXM)                ECS045
05944          ADD +1                  TO CLAS-INDEXM                   ECS045
05945          GO TO 2220-FIND-MORT-LOOP.                               ECS045
05946                                                                   ECS045
05947      MOVE CLAS-MORT-DESC (CLAS-INDEXM) TO WS-MORT-DSC.            ECS045
05948      MOVE WS-MORT-DESC                 TO DT-ENTRY (21)           ECS045
05949                                           DT-ENTRY-2 (21).        ECS045
05950                                                                   ECS045
05951  2230-ACCUM-LEVEL-1-DETAIL.                                       ECS045
05952                                                                   ECS045
05953      IF RR-LF-PR-PCT GREATER THAN +0 OR                           ECS045
05954         RR-LF-78-PCT GREATER THAN +0                              ECS045
05955         MOVE 'Y'                 TO LIFE-PROCESS-SWITCH.          ECS045
05956                                                                   ECS045
05957      PERFORM 0480-CALCULATE-LEVEL-1 THRU 0490-EXIT.               ECS045
05958      PERFORM 2000-GET-RECAP         THRU 2000-GET-RECAP-X.        ECS045
05959      GO TO 2100-CHECK-CONTROL.                                    ECS045
05960                                                                   ECS045
05961      EJECT                                                        ECS045
05962  2400-PRT-TO.                                                     ECS045
05963      IF DTE-CLIENT NOT = 'LBL'                                    ECS045
05964          MOVE 'Y'                TO SAVE-PRT-ST                   ECS045
05965                                     SAVE-PRT-OW.                  ECS045
05966                                                                   ECS045
05967      PERFORM 0600-COMPUTE-PAA THRU 0610-EXIT VARYING SB           ECS045
05968              FROM +1 BY +1 UNTIL SB GREATER +2.                   ECS045
05969                                                                   ECS045
05970      PERFORM 0600-COMPUTE-PAA THRU 0610-EXIT VARYING SB           ECS045
05971              FROM +4 BY +1 UNTIL SB GREATER +5.                   ECS045
05972                                                                   ECS045
05973      PERFORM 0600-COMPUTE-PAA THRU 0610-EXIT VARYING SB           ECS045
05974              FROM +7 BY +1 UNTIL SB GREATER +8.                   ECS045
05975                                                                   ECS045
05976      MOVE 'Y'                    TO DT-PRT-SW (8)                 ECS045
05977                                     DT-PRT-SW (9)                 ECS045
05978                                     DT-PRT-SW (10)                ECS045
05979                                     DT-PRT-SW (11)                ECS045
05980                                     DT-PRT-SW (12).               ECS045
05981                                                                   ECS045
032910     MOVE 'Y'                    TO DT-PRT-SW-2 (8)
032910                                    DT-PRT-SW-2 (9)
032910                                    DT-PRT-SW-2 (10)
032910                                    DT-PRT-SW-2 (11)
032910                                    DT-PRT-SW-2 (12)

05982      MOVE +1                     TO SB.                           ECS045
05983      MOVE +3                     TO SC.                           ECS045
05984      PERFORM 0520-ADD-PAA THRU 0530-EXIT.                         ECS045
05985      MOVE +2                     TO SB.                           ECS045
05986      MOVE +3                     TO SC.                           ECS045
05987      PERFORM 0520-ADD-PAA THRU 0530-EXIT.                         ECS045
05988      MOVE +4                     TO SB.                           ECS045
05989      MOVE +6                     TO SC.                           ECS045
05990      PERFORM 0520-ADD-PAA THRU 0530-EXIT.                         ECS045
05991      MOVE +5                     TO SB.                           ECS045
05992      MOVE +6                     TO SC.                           ECS045
05993      PERFORM 0520-ADD-PAA THRU 0530-EXIT.                         ECS045
05994      MOVE +7                     TO SB.                           ECS045
05995      MOVE +9                     TO SC.                           ECS045
05996      PERFORM 0520-ADD-PAA THRU 0530-EXIT.                         ECS045
05997      MOVE +8                     TO SB.                           ECS045
05998      MOVE +9                     TO SC.                           ECS045
05999      PERFORM 0520-ADD-PAA THRU 0530-EXIT.                         ECS045
06000                                                                   ECS045
06001      MOVE '6'                    TO HDR-SW.                       ECS045
06002      PERFORM 0900-HDR-RTN THRU 0910-EXIT.                         ECS045
06003      MOVE +1                     TO SA.                           ECS045
06004      PERFORM 0860-PRT-DTL-1 THRU 0870-EXIT VARYING SG             ECS045
06005              FROM +1 BY +1 UNTIL SG GREATER +19.                  ECS045
06006                                                                   ECS045
PEMMOD     MOVE ALL '_'                TO PRT
PEMMOD                                    PRT-2
PEMMOD     MOVE ' '                    TO X
PEMMOD     PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
PEMMOD                                                                  ECS045
06007      MOVE HDR-8                  TO PRT.                          ECS045
06008      MOVE HDR-8-2                TO PRT-2.                        ECS045
PEMMOD*    MOVE '0'                    TO X.                            ECS045
PEMMOD     MOVE ' '                    TO X.                            ECS045
06010      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
06011 *    IF DTE-FMT-OPT EQUAL '1'                                     ECS045
06012 *        PERFORM 0880-PRT-DTL-2 THRU 0890-EXIT VARYING SG         ECS045
06013 *               FROM +20 BY +1 UNTIL SG GREATER +38               ECS045
06014 *    ELSE                                                         ECS045
06015 *        PERFORM 0882-PRT-DTL-2 THRU 0892-EXIT VARYING SG         ECS045
06016 *               FROM +20 BY +1 UNTIL SG GREATER +38.              ECS045
CIDMOD     IF DTE-FMT-OPT EQUAL '2'                                     ECS045
CIDMOD         PERFORM 0882-PRT-DTL-2 THRU 0892-EXIT VARYING SG         ECS045
CIDMOD                FROM +20 BY +1 UNTIL SG GREATER +38               ECS045
CIDMOD     ELSE                                                         ECS045
CIDMOD         PERFORM 0880-PRT-DTL-2 THRU 0890-EXIT VARYING SG         ECS045
CIDMOD                FROM +20 BY +1 UNTIL SG GREATER +38.              ECS045
061008
061008     IF DTE-FMT-OPT EQUAL '2' AND
061008       (DO-TOTAL-TYPE EQUAL '1' OR '2' OR '3')
061008         WRITE DATA-OUT-REC FROM DATA-OUT-RECORD
061008         PERFORM 3300-INIT-DATA-OUT THRU 3399-EXIT
061008     END-IF.
020612
020612     IF DTE-FMT-OPT EQUAL '2' AND
020612       (DOA-TOTAL-TYPE EQUAL '1' OR '2' OR '3' OR '4')
020612         WRITE DATA-OUT-AHL-REC FROM DATA-OUT-AHL-RECORD
020612         PERFORM 3400-INIT-DATA-OUT-AHL THRU 3499-EXIT
020612     END-IF.
06017                                                                   ECS045
06018      MOVE SPACES                 TO PRT                           ECS045
06019                                     PRT-2.                        ECS045
PEMMOD*    MOVE '0'                    TO X.                            ECS045
PEMMOD     MOVE ' '                    TO X.                            ECS045
06021      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
06022                                                                   ECS045
06023      MOVE TRLR-1                 TO PRT                           ECS045
06024                                     PRT-2.                        ECS045
06025      MOVE '0'                    TO X.                            ECS045
06026      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
06027                                                                   ECS045
PEMMOD     MOVE ' '                    TO X.                            ECS045
06028      MOVE TRLR-2                 TO PRT                           ECS045
06029                                     PRT-2.                        ECS045
06030      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
06031                                                                   ECS045
06032      MOVE +1                     TO SB.                           ECS045
06033      MOVE +10                    TO SC.                           ECS045
06034      PERFORM 0520-ADD-PAA THRU 0530-EXIT 9 TIMES.                 ECS045
06035      MOVE +1                     TO SB.                           ECS045
06036      MOVE +7                     TO SC.                           ECS045
06037      PERFORM 0540-ADD-PAB THRU 0550-EXIT 6 TIMES.                 ECS045
06038                                                                   ECS045
06039      PERFORM 0440-ZERO-PAA THRU 0450-EXIT VARYING SA              ECS045
06040              FROM +1 BY +1 UNTIL SA GREATER +9.                   ECS045
06041      PERFORM 0460-ZERO-PAB THRU 0470-EXIT VARYING SA              ECS045
06042              FROM +1 BY +1 UNTIL SA GREATER +6.                   ECS045
06043                                                                   ECS045
06044      MOVE RR-C-CEDE-NAME         TO SAVE-CEDE-NAME.               ECS045
06045                                                                   ECS045
06046  2499-EXIT.                                                       ECS045
06047      EXIT.                                                        ECS045
06048                                                                   ECS045
06049      EJECT                                                        ECS045
06050  2500-PRT-FROM.                                                   ECS045
06051      MOVE '6'                    TO HDR-SW.                       ECS045
06052      PERFORM 0900-HDR-RTN THRU 0910-EXIT.                         ECS045
06053      MOVE +2                     TO SA.                           ECS045
06054      PERFORM 0860-PRT-DTL-1 THRU 0870-EXIT VARYING SG             ECS045
06055              FROM +1 BY +1 UNTIL SG GREATER +19.                  ECS045
06056                                                                   ECS045
PEMMOD     MOVE ALL '_'                TO PRT
PEMMOD                                    PRT-2
PEMMOD     MOVE ' '                    TO X
PEMMOD     PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
PEMMOD                                                                  ECS045
06057      MOVE HDR-8                  TO PRT.                          ECS045
06058      MOVE HDR-8-2                TO PRT-2.                        ECS045
PEMMOD*    MOVE '0'                    TO X.                            ECS045
PEMMOD     MOVE ' '                    TO X.                            ECS045
06060      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
06061      IF DTE-FMT-OPT EQUAL '2'                                     ECS045
06062          PERFORM 0882-PRT-DTL-2 THRU 0892-EXIT VARYING SG         ECS045
06063                 FROM +20 BY +1 UNTIL SG GREATER +38               ECS045
06064      ELSE                                                         ECS045
06065          PERFORM 0880-PRT-DTL-2 THRU 0890-EXIT VARYING SG         ECS045
06066                 FROM +20 BY +1 UNTIL SG GREATER +38.              ECS045
061008
061008     IF DTE-FMT-OPT EQUAL '2' AND
061008       (DO-TOTAL-TYPE EQUAL '1' OR '2' OR '3')
061008         WRITE DATA-OUT-REC FROM DATA-OUT-RECORD
061008         PERFORM 3300-INIT-DATA-OUT THRU 3399-EXIT
061008     END-IF.
020612
020612     IF DTE-FMT-OPT EQUAL '2' AND
020612       (DOA-TOTAL-TYPE EQUAL '1' OR '2' OR '3' OR '4')
020612         WRITE DATA-OUT-AHL-REC FROM DATA-OUT-AHL-RECORD
020612         PERFORM 3400-INIT-DATA-OUT-AHL THRU 3499-EXIT
020612     END-IF.
06067                                                                   ECS045
06068      MOVE SPACES                 TO PRT                           ECS045
06069                                     PRT-2.                        ECS045
06070      MOVE '0'                    TO X.                            ECS045
06071      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
06072                                                                   ECS045
PEMMOD     MOVE ' '                    TO X.                            ECS045
06073      MOVE TRLR-1                 TO PRT                           ECS045
06074                                     PRT-2.                        ECS045
PEMMOD*    MOVE '0'                    TO X.                            ECS045
PEMMOD     MOVE ' '                    TO X.                            ECS045
06076      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
06077                                                                   ECS045
06078      MOVE TRLR-2                 TO PRT                           ECS045
06079                                     PRT-2.                        ECS045
06080      PERFORM 0920-PRT-RTN THRU 0930-EXIT.                         ECS045
06081                                                                   ECS045
06082      MOVE +10                    TO SB.                           ECS045
06083      MOVE +19                    TO SC.                           ECS045
06084      PERFORM 0520-ADD-PAA THRU 0530-EXIT 9 TIMES.                 ECS045
06085      MOVE +7                     TO SB.                           ECS045
06086      MOVE +13                    TO SC.                           ECS045
06087      PERFORM 0540-ADD-PAB THRU 0550-EXIT 6 TIMES.                 ECS045
06088                                                                   ECS045
06089      PERFORM 0440-ZERO-PAA THRU 0450-EXIT VARYING SA              ECS045
06090              FROM +10 BY +1 UNTIL SA GREATER +18.                 ECS045
06091      PERFORM 0460-ZERO-PAB THRU 0470-EXIT VARYING SA              ECS045
06092              FROM +7 BY +1 UNTIL SA GREATER +12.                  ECS045
06093                                                                   ECS045
06094      IF RR-C-REI-NAME = HIGH-VALUE                                ECS045
06095          GO TO 2999-EXIT.                                         ECS045
06096                                                                   ECS045
06097      MOVE RR-C-REI-NAME          TO SAVE-REI-NAME.                ECS045
06098      MOVE RR-C-CEDE-NAME         TO SAVE-CEDE-NAME.               ECS045
06099                                                                   ECS045
06100  2599-EXIT.                                                       ECS045
06101      EXIT.                                                        ECS045
06102                                                                   ECS045
06103  2999-EXIT.                                                       ECS045
06104      EXIT.                                                        ECS045
061008
061008 3300-INIT-DATA-OUT.
061008     MOVE SPACES TO DO-REIN-BY-NUM
061008                    DO-REIN-BY-NAME
061008                    DO-CEDED-FROM-NAME
061008                    DO-REIN-PRIME
061008                    DO-REIN-GROUP
061008                    DO-CARRIER
061008                    DO-CERT-GROUP
061008                    DO-STATE
061008                    DO-ACCOUNT-NUM
061008                    DO-ACCOUNT-NAME
061008                    DO-TOTAL-TYPE
061008                    DO-CLAIM-PI.
061008
061008     PERFORM VARYING WS-DO-SUB FROM 1 BY 1 UNTIL 
061008                     WS-DO-SUB > 22
061008          MOVE SPACES TO DO-DESCR (WS-DO-SUB)
061008          MOVE ZEROS  TO DO-ID-NUM (WS-DO-SUB)
061008                         DO-CLF (WS-DO-SUB)
061008                         DO-YLF (WS-DO-SUB)
061008                         DO-ILF (WS-DO-SUB)
061008                         DO-CAH (WS-DO-SUB)
061008                         DO-YAH (WS-DO-SUB)
061008                         DO-IAH (WS-DO-SUB)
061008     END-PERFORM.
061008
061008     MOVE +0     TO WS-DO-SUB.
061008                         
061008 3399-EXIT.
061008     EXIT.
020612
020612 3400-INIT-DATA-OUT-AHL.
020612     MOVE SPACES TO DOA-REIN-BY-NUM
020612                    DOA-REIN-BY-NAME
020612                    DOA-CEDED-FROM-NAME
020612                    DOA-REIN-PRIME
020612                    DOA-REIN-GROUP
020612                    DOA-CARRIER
020612                    DOA-CERT-GROUP
020612                    DOA-STATE
020612                    DOA-ACCOUNT-NUM
020612                    DOA-ACCOUNT-NAME
020612                    DOA-REIN-SUB
020612                    DOA-TOTAL-TYPE
020612                    DOA-CLAIM-PI.
020612
020612     PERFORM VARYING WS-DO-SUB FROM 1 BY 1 UNTIL 
022912                     WS-DO-SUB > 19
020612          MOVE SPACES TO DOA-DESCR (WS-DO-SUB)
020612          MOVE ZEROS  TO DOA-ID-NUM (WS-DO-SUB)
020612                         DOA-CLF (WS-DO-SUB)
020612                         DOA-YLF (WS-DO-SUB)
020612                         DOA-ILF (WS-DO-SUB)
020612                         DOA-CAH (WS-DO-SUB)
020612                         DOA-YAH (WS-DO-SUB)
020612                         DOA-IAH (WS-DO-SUB)
020612                         DOA-CCMB (WS-DO-SUB)
020612                         DOA-YCMB (WS-DO-SUB)
020612                         DOA-ICMB (WS-DO-SUB)
020612     END-PERFORM.
020612
020612     PERFORM VARYING WS-DO-SUB FROM 1 BY 1 UNTIL 
020612                     WS-DO-SUB > 13
020612          MOVE SPACES TO DOB-DESCR (WS-DO-SUB)
020612          MOVE ZEROS  TO DOB-ID-NUM (WS-DO-SUB)
020612                         DOB-CBG (WS-DO-SUB)
020612                         DOB-CEN (WS-DO-SUB)
020612                         DOB-YBG (WS-DO-SUB)
020612                         DOB-YEN (WS-DO-SUB)
020612                         DOB-IBG (WS-DO-SUB)
020612                         DOB-IEN (WS-DO-SUB)
020612     END-PERFORM.
020612
020612     MOVE +0     TO WS-DO-SUB.
020612                         
020612 3499-EXIT.
020612     EXIT.
06105                                                                   ECS045
06106  COPY ELCDCS.                                                     ECS045
06107                                                                   ECS045
06108  EJECT                                                            ECS045
06109  END-OF-JOB SECTION.                                              ECS045
06110                                                                   ECS045
06111  9999-END-OF-JOB.                                                 ECS045
06112                                                                   ECS045
06113      IF FICH-OPEN NOT = SPACE                                     ECS045
06114          IF DTE-FMT-OPT EQUAL '2'                                 ECS045
06115               CLOSE FICH-2                                        ECS045
06116          ELSE                                                     ECS045
06117               CLOSE FICH.                                         ECS045
06118                                                                   ECS045
06119      CLOSE PRNTR-2                                                ECS045
06120            PRNTR.                                                 ECS045

061008     IF DTE-FMT-OPT = '2'
061008        CLOSE DATA-OUT
020612        CLOSE DATA-OUT-AHL
061008     END-IF
06121                                                                   ECS045
06122      GOBACK.                                                      ECS045
06123                                                                   ECS045
06124  ABEND-PGM SECTION.                                               ECS045
06125            COPY ELCABEND.                                         ECS045
