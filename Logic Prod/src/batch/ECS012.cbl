00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   ECS012
00003  PROGRAM-ID.                ECS012.                                  LV008
00004 *              PROGRAM CONVERTED BY                               ECS012
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS012
00006 *              CONVERSION DATE 11/28/95 10:54:18.                 ECS012
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            ECS012
00008 *                           VMOD=2.009.                           ECS012
00009 *AUTHOR.     LOGIC, INC.                                          ECS012
00010 *            DALLAS, TEXAS.                                       ECS012
00011                                                                   ECS012
00012 *DATE-COMPILED.                                                   ECS012
00013                                                                   ECS012
00014 *SECURITY.   *****************************************************ECS012
00015 *            *                                                   *ECS012
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS012
00017 *            *                                                   *ECS012
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS012
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS012
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS012
00021 *            *                                                   *ECS012
00022 *            *****************************************************ECS012
00023                                                                   ECS012
00024 *REMARKS.                                                         ECS012
00025 *         PRINT PREMIUM DISTRIBUTION FOR STATE AND LOCAL TAXES.   ECS012
00026                                                                   ECS012
00027  EJECT                                                            ECS012
00028  ENVIRONMENT DIVISION.                                            ECS012
00029  CONFIGURATION SECTION.                                           ECS012
00030  SPECIAL-NAMES.                                                   ECS012
00031      C02 IS LCP-CH2                                               ECS012
00032      C03 IS LCP-CH3                                               ECS012
00033      C04 IS LCP-CH4                                               ECS012
00034      C05 IS LCP-CH5                                               ECS012
00035      C06 IS LCP-CH6                                               ECS012
00036      C07 IS LCP-CH7                                               ECS012
00037      C08 IS LCP-CH8                                               ECS012
00038      C09 IS LCP-CH9                                               ECS012
00039      C10 IS LCP-CH10                                              ECS012
00040      C11 IS LCP-CH11                                              ECS012
00041      C12 IS LCP-CH12                                              ECS012
00042      S01 IS LCP-P01                                               ECS012
00043      S02 IS LCP-P02.                                              ECS012
00044  INPUT-OUTPUT SECTION.                                            ECS012
00045  FILE-CONTROL.                                                    ECS012
00046                                                                   ECS012
00047      SELECT  SORT-WORK-A     ASSIGN TO SYS001-DA-3380-S-SORTWK1.  ECS012
00048                                                                   ECS012
00049      SELECT  PRNTR           ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS012
00050                                                                   ECS012
00051      SELECT  EPEC-FILE       ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS012
00052                                                                   ECS012
00053      SELECT  ACC-MSTR        ASSIGN TO SYS015-FBA1-SYS015         ECS012
00054                              ACCESS IS SEQUENTIAL                 ECS012
00055                              ORGANIZATION IS INDEXED              ECS012
00056                              FILE STATUS IS ERACCT-FILE-STATUS    ECS012
00057                              RECORD KEY IS AM-CONTROL-PRIMARY.    ECS012
00058                                                                   ECS012
00059      SELECT  DISK-DATE       ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS012
00060                                                                   ECS012
00061      SELECT  FICH            ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS012
00062                                                                   ECS012
00063  EJECT                                                            ECS012
00064  DATA DIVISION.                                                   ECS012
00065  FILE SECTION.                                                    ECS012
00066                                                                   ECS012
00067 *    THE FOLLOWING SORT WORK FILE IS TO PLACE THE OUTPUT EXTRACTS ECS012
00068 *    INTO CARR,GRP,ST,CITY,COUNT, AND ACCOUNT TYPE SEQUENCE.      ECS012
00069  SD  SORT-WORK-A.                                                 ECS012
00070                                                                   ECS012
00071  01  SORT-RECORD-OUT.                                             ECS012
00072      12  SWA-CONTROL.                                             ECS012
00073          16  SORT-CARRIER    PIC X(01).                           ECS012
00074          16  SORT-GROUP      PIC X(06).                           ECS012
00075          16  SORT-STATE      PIC X(02).                           ECS012
00076          16  SORT-CT-CD      PIC X(01).                           ECS012
00077          16  SORT-CITY       PIC X(04).                           ECS012
00078          16  SORT-COUNTY     PIC X(06).                           ECS012
00079          16  SORT-ACCOUNT    PIC X(10).                           ECS012
00080      12  SORT-REST-OF-RECORD PIC X(325).                          ECS012
00081                                                                   ECS012
00082  EJECT                                                            ECS012
00083  FD  PRNTR                                                        ECS012
00084                              COPY ELCPRTFD.                       ECS012
00085  EJECT                                                            ECS012
00086  FD  EPEC-FILE                                                    ECS012
00087                              COPY ECSEPCFD.                       ECS012
00088                              COPY ECSEPC01.                       ECS012
00089      EJECT                                                        ECS012
00090  FD  ACC-MSTR.                                                    ECS012
00091                              COPY ERCACCT.                        ECS012
00092                                                                   ECS012
00093  EJECT                                                            ECS012
00094  FD  DISK-DATE                                                    ECS012
00095                              COPY ELCDTEFD.                       ECS012
00096  EJECT                                                            ECS012
00097  FD  FICH                                                         ECS012
00098                              COPY ELCFCHFD.                       ECS012
00099                                                                   ECS012
00100  EJECT                                                            ECS012
00101  WORKING-STORAGE SECTION.                                         ECS012
00102  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS012
00103  77  LCP-ASA                       PIC X.                         ECS012
00104                                                                   ECS012
00105  77  FILLER  PIC X(32) VALUE '********************************'.  ECS012
00106  77  FILLER  PIC X(32) VALUE '     ECS012 WORKING STORAGE     '.  ECS012
00107  77  FILLER  PIC X(32) VALUE '***********VMOD=2.009 **********'.  ECS012
00108                                                                   ECS012
00109  77  NO-OF-RECORDS-RELEASED      PIC S9(9)      COMP-3  VALUE +0. ECS012
00110  77  WS-DONT-BREAK               PIC X VALUE ' '.                 ECS012
00111  77  SUB                         PIC S9(04)     COMP-3  VALUE +0. ECS012
00112  77  SUB1                        PIC S9(04)     COMP.             ECS012
00113  77  X3                          PIC S9(04)     COMP.             ECS012
00114  77  PGM-SUB                     PIC S999       COMP-3 VALUE +012.ECS012
00115  77  X                           PIC X(01).                       ECS012
00116  77  WS-AVG-AGE                  PIC S9(07)     COMP-3 VALUE +0.  ECS012
00117  77  WS-AVG-TRM                  PIC S9(07)     COMP-3 VALUE +0.  ECS012
00118  77  WS-AVG-REM-TRM              PIC S9(07)     COMP-3 VALUE +0.  ECS012
00119  77  WS-AVG-CNT                  PIC S9(05)     COMP-3 VALUE +0.  ECS012
00120  77  WS-EP-CODE                  PIC X(01).                       ECS012
00121  77  WS-GA-FOUND                 PIC X(01).                       ECS012
00122  77  WS-BIN-RUN-DATE             PIC X(02).                       ECS012
00123  77  WS-BIN-MTD-DATE             PIC X(02).                       ECS012
00124  77  WS-BIN-QTD-DATE             PIC X(02).                       ECS012
00125  77  WS-BIN-L12-DATE             PIC X(02).                       ECS012
00126  77  WS-BIN-P12-DATE             PIC X(02).                       ECS012
00127  77  WS-SAVE-ST                  PIC X(02)   VALUE SPACES.        ECS012
00128  77  WS-SAVE-LFTYP               PIC X(02)   VALUE SPACES.        ECS012
00129  77  WS-SAVE-AHTYP               PIC X(02)   VALUE SPACES.        ECS012
00130  77  PGCTR                       PIC 9(05)   VALUE 0    COMP-3.   ECS012
00131  77  LNCTR                       PIC 9(03)   VALUE 90   COMP-3.   ECS012
00132  77  HLD-CTL                     PIC X(01)   VALUE SPACE.         ECS012
00133                                                                   ECS012
00134  77  WS-DISPLAY-SUB              PIC 9(04)   VALUE ZEROS.         ECS012
00135  77  WS-DISPLAY-CNT              PIC 9(09)   VALUE ZEROS.         ECS012
00136  77  WS-DISPLAY-AMT              PIC 9(11).99 VALUE ZEROS.        ECS012
00137                                                                   ECS012
00138      EJECT                                                        ECS012
00139  01  WS-ABEND-STORAGE.                                            ECS012
00140      12  WS-RETURN-CODE          PIC S9(4)  VALUE ZERO COMP.      ECS012
00141      12  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.         ECS012
00142      12  WS-ABEND-FILE-STATUS    PIC XX     VALUE ZERO.           ECS012
00143      12  WS-ZERO                 PIC S9     VALUE ZERO COMP-3.    ECS012
00144                                                                   ECS012
00145      12  ERACCT-FILE-STATUS      PIC XX.                          ECS012
00146                                                                   ECS012
00147      EJECT                                                        ECS012
00148                                                                   ECS012
00149  01  WS-MISC.                                                     ECS012
00150      12  WS-PRINT-TOTAL-SW       PIC X(01)   VALUE 'N'.           ECS012
00151      12  WS-PRINT-HEADING-SW     PIC 9(01)   VALUE 0.             ECS012
00152      12  WS-HIGH-EXP-DT          PIC X(06)   VALUE LOW-VALUES.    ECS012
00153      12  WS-HIGH-EXP-DT-R REDEFINES                               ECS012
00154          WS-HIGH-EXP-DT          PIC 9(11)   COMP-3.              ECS012
00155      12  WS-HOLD-RECORD          PIC X(648).                      ECS012
00156      12  WS-ACCT-CONTROL.                                         ECS012
00157          16  WS-MSTR-CNTRL       PIC X(25)   VALUE LOW-VALUES.    ECS012
00158          16  WS-EFFECT           PIC 9(11)   COMP-3 VALUE 0.         CL**5
00159      12  WS-HOLD-CONTROL.                                         ECS012
00160          16  WS-HOLD-CARRIER     PIC X(01) VALUE LOW-VALUES.      ECS012
00161          16  WS-HOLD-GROUP       PIC X(06) VALUE LOW-VALUES.      ECS012
00162          16  WS-HOLD-STATE       PIC X(02) VALUE LOW-VALUES.      ECS012
00163          16  WS-HOLD-CITY        PIC X(04) VALUE LOW-VALUES.      ECS012
00164          16  WS-HOLD-ACCOUNT     PIC X(10) VALUE LOW-VALUES.      ECS012
00165          16  WS-HOLD-COUNTY      PIC X(04) VALUE LOW-VALUES.      ECS012
00166      12  WS-EP-CONTROL           PIC X(31).                       ECS012
00167                                                                   ECS012
00168      12  MTD-DATE                PIC 9(11).                       ECS012
00169      12  MTD-DATE-R  REDEFINES MTD-DATE.                          ECS012
00170          16  FILLER              PIC 999.                         ECS012
00171          16  MTD-CC              PIC 99.                          ECS012
00172          16  MTD-YY              PIC 99.                          ECS012
00173          16  MTD-MM              PIC 99.                          ECS012
00174          16  MTD-DD              PIC 99.                          ECS012
00175      12  QTD-DATE                PIC 9(11).                       ECS012
00176      12  QTD-DATE-R  REDEFINES QTD-DATE.                          ECS012
00177          16  FILLER              PIC 999.                         ECS012
00178          16  QTD-CC              PIC 99.                          ECS012
00179          16  QTD-YY              PIC 99.                          ECS012
00180          16  QTD-MM              PIC 99.                          ECS012
00181          16  QTD-DD              PIC 99.                          ECS012
00182      12  YTD-DATE                PIC 9(11).                       ECS012
00183      12  YTD-DATE-R  REDEFINES YTD-DATE.                          ECS012
00184          16  FILLER              PIC 999.                         ECS012
00185          16  YTD-CCYY            PIC 9(04).                       ECS012
00186          16  YTD-CCYR  REDEFINES YTD-CCYY.                        ECS012
00187              20  YTD-CC          PIC 99.                          ECS012
00188              20  YTD-YY          PIC 99.                          ECS012
00189          16  YTD-MM              PIC 99.                          ECS012
00190          16  YTD-DD              PIC 99.                          ECS012
00191      12  PYTD-DATE               PIC 9(11).                          CL**4
00192      12  PYTD-DATE-R REDEFINES PYTD-DATE.                            CL**4
00193          16  FILLER              PIC 999.                         ECS012
00194          16  PYTD-CCYY           PIC 9(04).                       ECS012
00195          16  PYTD-CCYR  REDEFINES PYTD-CCYY.                      ECS012
00196              20  PYTD-CC         PIC 99.                          ECS012
00197              20  PYTD-YY         PIC 99.                          ECS012
00198          16  PYTD-MM             PIC 99.                          ECS012
00199          16  PYTD-DD             PIC 99.                          ECS012
00200      12  L12-DATE.                                                ECS012
00201          16  L12-YY              PIC 99.                          ECS012
00202          16  L12-MM              PIC 99.                          ECS012
00203          16  L12-DD              PIC 99.                          ECS012
00204      12  P12-DATE.                                                ECS012
00205          16  P12-YY              PIC 99.                          ECS012
00206          16  P12-MM              PIC 99.                          ECS012
00207          16  P12-DD              PIC 99.                          ECS012
00208      12  BEG-DATE                PIC 9(11).                          CL**4
00209      12  BEG-DATE-R REDEFINES BEG-DATE.                              CL**4
00210          16  FILLER              PIC 999.                         ECS012
00211          16  BD-CC               PIC 99.                          ECS012
00212          16  BD-YR               PIC 99.                          ECS012
00213          16  BD-MO               PIC 99.                          ECS012
00214          16  BD-DA               PIC 99.                          ECS012
00215      12  RUN-DT                  PIC 9(11).                          CL**3
00216      12  RUN-DT-R  REDEFINES RUN-DT.                                 CL**3
00217          16  FILLER              PIC 999.                         ECS012
00218          16  RD-CC               PIC 99.                          ECS012
00219          16  RD-YR               PIC 99.                          ECS012
00220          16  RD-MO               PIC 99.                          ECS012
00221          16  RD-DA               PIC 99.                          ECS012
00222                                                                   ECS012
00223  01  MISC-TOTALS.                                                 ECS012
00224      12  WS-CARRIER-TOTALS       OCCURS 7 TIMES.                  ECS012
00225          16  WS-CARR-LF-ISS-CNT     PIC S9(07)     COMP-3.        ECS012
00226          16  WS-CARR-LF-ISS-AMT     PIC S9(11)V99  COMP-3.        ECS012
00227          16  WS-CARR-LF-CAN-CNT     PIC S9(07)     COMP-3.        ECS012
00228          16  WS-CARR-LF-CAN-AMT     PIC S9(11)V99  COMP-3.        ECS012
00229          16  WS-CARR-AH-ISS-CNT     PIC S9(07)     COMP-3.        ECS012
00230          16  WS-CARR-AH-ISS-AMT     PIC S9(11)V99  COMP-3.        ECS012
00231          16  WS-CARR-AH-CAN-CNT     PIC S9(07)     COMP-3.        ECS012
00232          16  WS-CARR-AH-CAN-AMT     PIC S9(11)V99  COMP-3.        ECS012
00233      12  WS-GROUPING-TOTALS      OCCURS 7 TIMES.                  ECS012
00234          16  WS-GRP-LF-ISS-CNT      PIC S9(07)     COMP-3.        ECS012
00235          16  WS-GRP-LF-ISS-AMT      PIC S9(11)V99  COMP-3.        ECS012
00236          16  WS-GRP-LF-CAN-CNT      PIC S9(07)     COMP-3.        ECS012
00237          16  WS-GRP-LF-CAN-AMT      PIC S9(11)V99  COMP-3.        ECS012
00238          16  WS-GRP-AH-ISS-CNT      PIC S9(07)     COMP-3.        ECS012
00239          16  WS-GRP-AH-ISS-AMT      PIC S9(11)V99  COMP-3.        ECS012
00240          16  WS-GRP-AH-CAN-CNT      PIC S9(07)     COMP-3.        ECS012
00241          16  WS-GRP-AH-CAN-AMT      PIC S9(11)V99  COMP-3.        ECS012
00242      12  WS-STATE-TOTALS         OCCURS 7 TIMES.                  ECS012
00243          16  WS-ST-LF-ISS-CNT       PIC S9(07)     COMP-3.        ECS012
00244          16  WS-ST-LF-ISS-AMT       PIC S9(11)V99  COMP-3.        ECS012
00245          16  WS-ST-LF-CAN-CNT       PIC S9(07)     COMP-3.        ECS012
00246          16  WS-ST-LF-CAN-AMT       PIC S9(11)V99  COMP-3.        ECS012
00247          16  WS-ST-AH-ISS-CNT       PIC S9(07)     COMP-3.        ECS012
00248          16  WS-ST-AH-ISS-AMT       PIC S9(11)V99  COMP-3.        ECS012
00249          16  WS-ST-AH-CAN-CNT       PIC S9(07)     COMP-3.        ECS012
00250          16  WS-ST-AH-CAN-AMT       PIC S9(11)V99  COMP-3.        ECS012
00251      12  WS-CITY-TOTALS          OCCURS 7 TIMES.                  ECS012
00252          16  WS-CITY-LF-ISS-CNT     PIC S9(07)     COMP-3.        ECS012
00253          16  WS-CITY-LF-ISS-AMT     PIC S9(11)V99  COMP-3.        ECS012
00254          16  WS-CITY-LF-CAN-CNT     PIC S9(07)     COMP-3.        ECS012
00255          16  WS-CITY-LF-CAN-AMT     PIC S9(11)V99  COMP-3.        ECS012
00256          16  WS-CITY-AH-ISS-CNT     PIC S9(07)     COMP-3.        ECS012
00257          16  WS-CITY-AH-ISS-AMT     PIC S9(11)V99  COMP-3.        ECS012
00258          16  WS-CITY-AH-CAN-CNT     PIC S9(07)     COMP-3.        ECS012
00259          16  WS-CITY-AH-CAN-AMT     PIC S9(11)V99  COMP-3.        ECS012
00260      12  WS-ACCOUNT-TOTALS       OCCURS 7 TIMES.                  ECS012
00261          16  WS-ACCT-LF-ISS-CNT     PIC S9(07)     COMP-3.        ECS012
00262          16  WS-ACCT-LF-ISS-AMT     PIC S9(11)V99  COMP-3.        ECS012
00263          16  WS-ACCT-LF-CAN-CNT     PIC S9(07)     COMP-3.        ECS012
00264          16  WS-ACCT-LF-CAN-AMT     PIC S9(11)V99  COMP-3.        ECS012
00265          16  WS-ACCT-AH-ISS-CNT     PIC S9(07)     COMP-3.        ECS012
00266          16  WS-ACCT-AH-ISS-AMT     PIC S9(11)V99  COMP-3.        ECS012
00267          16  WS-ACCT-AH-CAN-CNT     PIC S9(07)     COMP-3.        ECS012
00268          16  WS-ACCT-AH-CAN-AMT     PIC S9(11)V99  COMP-3.        ECS012
00269      12  WS-COUNTY-TOTALS        OCCURS 7 TIMES.                  ECS012
00270          16  WS-CNTY-LF-ISS-CNT     PIC S9(07)     COMP-3.        ECS012
00271          16  WS-CNTY-LF-ISS-AMT     PIC S9(11)V99  COMP-3.        ECS012
00272          16  WS-CNTY-LF-CAN-CNT     PIC S9(07)     COMP-3.        ECS012
00273          16  WS-CNTY-LF-CAN-AMT     PIC S9(11)V99  COMP-3.        ECS012
00274          16  WS-CNTY-AH-ISS-CNT     PIC S9(07)     COMP-3.        ECS012
00275          16  WS-CNTY-AH-ISS-AMT     PIC S9(11)V99  COMP-3.        ECS012
00276          16  WS-CNTY-AH-CAN-CNT     PIC S9(07)     COMP-3.        ECS012
00277          16  WS-CNTY-AH-CAN-AMT     PIC S9(11)V99  COMP-3.        ECS012
00278      12  WS-FINAL-TOTALS         OCCURS 7 TIMES.                  ECS012
00279          16  WS-TOTAL-LF-ISS-CNT    PIC S9(07)     COMP-3.        ECS012
00280          16  WS-TOTAL-LF-ISS-AMT    PIC S9(11)V99  COMP-3.        ECS012
00281          16  WS-TOTAL-LF-CAN-CNT    PIC S9(07)     COMP-3.        ECS012
00282          16  WS-TOTAL-LF-CAN-AMT    PIC S9(11)V99  COMP-3.        ECS012
00283          16  WS-TOTAL-AH-ISS-CNT    PIC S9(07)     COMP-3.        ECS012
00284          16  WS-TOTAL-AH-ISS-AMT    PIC S9(11)V99  COMP-3.        ECS012
00285          16  WS-TOTAL-AH-CAN-CNT    PIC S9(07)     COMP-3.        ECS012
00286          16  WS-TOTAL-AH-CAN-AMT    PIC S9(11)V99  COMP-3.        ECS012
00287      12  WS-INTERMEDIATE-TOTALS  OCCURS 4 TIMES.                  ECS012
00288          16  WS-INTER-LF-ISS-CNT    PIC S9(07)     COMP-3.        ECS012
00289          16  WS-INTER-LF-ISS-AMT    PIC S9(11)V99  COMP-3.        ECS012
00290          16  WS-INTER-LF-CAN-CNT    PIC S9(07)     COMP-3.        ECS012
00291          16  WS-INTER-LF-CAN-AMT    PIC S9(11)V99  COMP-3.        ECS012
00292          16  WS-INTER-AH-ISS-CNT    PIC S9(07)     COMP-3.        ECS012
00293          16  WS-INTER-AH-ISS-AMT    PIC S9(11)V99  COMP-3.        ECS012
00294          16  WS-INTER-AH-CAN-CNT    PIC S9(07)     COMP-3.        ECS012
00295          16  WS-INTER-AH-CAN-AMT    PIC S9(11)V99  COMP-3.        ECS012
00296                                                                   ECS012
00297  01  PREV-SEQ.                                                    ECS012
00298      12  PREV-CARRIER            PIC X(01)   VALUE LOW-VALUE.     ECS012
00299      12  PREV-GROUPING           PIC X(06).                       ECS012
00300      12  PREV-STATE              PIC X(02).                       ECS012
00301      12  PREV-CITY               PIC X(04).                       ECS012
00302      12  PREV-ACCOUNT            PIC X(10).                       ECS012
00303      12  PREV-COUNTY             PIC X(06).                       ECS012
00304      12  PREV-CT-CD              PIC X(01).                       ECS012
00305                                                                   ECS012
00306      EJECT                                                        ECS012
00307  01  HD1.                                                         ECS012
00308      12  FILLER                  PIC X(44)   VALUE SPACES.        ECS012
00309      12  FILLER                  PIC X(38)                        ECS012
00310              VALUE 'PREMIUM BREAKDOWN FOR TAX COMPUTATIONS'.      ECS012
00311      12  FILLER                  PIC X(37)   VALUE SPACES.        ECS012
00312      12  FILLER                  PIC X(07)   VALUE 'ECS012 '.     ECS012
00313      12  HD1-RPTNO               PIC X(01)   VALUE SPACE.         ECS012
00314                                                                   ECS012
00315  01  HD2.                                                         ECS012
00316      12  SUB-HD2                 PIC X(48)   VALUE SPACES.        ECS012
00317      12  HD-CO                   PIC X(40).                       ECS012
00318      12  FILLER                  PIC X(31)   VALUE SPACES.        ECS012
00319      12  HD-RD                   PIC X(08).                       ECS012
00320                                                                   ECS012
00321  01  HD3.                                                         ECS012
00322      12  SUB-HD3                 PIC X(55)   VALUE SPACES.        ECS012
00323      12  HD-DT                   PIC X(20).                       ECS012
00324      12  FILLER                  PIC X(44)   VALUE SPACES.        ECS012
00325      12  FILLER                  PIC X(05)   VALUE 'PAGE '.       ECS012
00326      12  HD-PAGE                 PIC ZZZZ9.                       ECS012
00327                                                                   ECS012
00328  01  HD4.                                                         ECS012
00329      12  H4-CARR-HD              PIC X(11)   VALUE ' CARRIER -'.  ECS012
00330      12  H4-CARR-CD              PIC X(04).                       ECS012
00331      12  H4-CARR-NM              PIC X(30).                       ECS012
00332                                                                   ECS012
00333  01  HD5.                                                         ECS012
00334      12  H5-GRP-HD               PIC X(11)   VALUE ' GROUPING -'. ECS012
00335      12  H5-GRP-CD               PIC X(06).                       ECS012
00336                                                                   ECS012
00337  01  HD6.                                                         ECS012
00338      12  H6-ST-HD                PIC X(11)   VALUE ' STATE    -'. ECS012
00339      12  H6-ST-CD                PIC X(04).                       ECS012
00340      12  H6-ST-NM                PIC X(18).                       ECS012
00341                                                                   ECS012
00342  01  HD7.                                                         ECS012
00343      12  H7-SUB-HD               PIC X(14)                        ECS012
00344              VALUE ' SUBTOTALS BY '.                              ECS012
00345      12  H7-SUB-BY               PIC X(06).                       ECS012
00346                                                                   ECS012
00347  01  HD8.                                                         ECS012
00348      12  FILLER                  PIC X(27)   VALUE SPACES.        ECS012
00349      12  FILLER                  PIC X(17)                        ECS012
00350              VALUE '- - - - - - - -  '.                           ECS012
00351      12  HD8-OVRD-1              PIC X(06).                       ECS012
00352      12  FILLER                  PIC X(17)                        ECS012
00353              VALUE '- - - - - - - -  '.                           ECS012
00354      12  FILLER                  PIC X(17)   VALUE SPACES.        ECS012
00355      12  FILLER                  PIC X(17)                        ECS012
00356              VALUE '- - - - - - - -  '.                           ECS012
00357      12  HD8-OVRD-2              PIC X(06).                       ECS012
00358      12  FILLER                  PIC X(16)                        ECS012
00359              VALUE '- - - - - - - - '.                            ECS012
00360      12  FILLER                  PIC X(10)   VALUE SPACES.        ECS012
00361                                                                   ECS012
00362  01  HD9.                                                         ECS012
00363      12  FILLER                  PIC X(05)   VALUE SPACES.        ECS012
00364      12  HD9-CTY-CNTY            PIC X(04)   VALUE SPACES.        ECS012
00365      12  FILLER                  PIC X(23)                        ECS012
00366              VALUE '   ACCOUNT        ISSUE'.                     ECS012
00367      12  FILLER                  PIC X(35)                        ECS012
00368              VALUE '        ISSUE   CANCEL     REFUNDED'.         ECS012
00369      12  FILLER                  PIC X(17)   VALUE SPACES.        ECS012
00370      12  FILLER                  PIC X(40)                        ECS012
00371              VALUE 'ISSUE        ISSUE   CANCEL    REFUNDED'.     ECS012
00372                                                                   ECS012
00373  01  HDA.                                                         ECS012
00374      12  FILLER                  PIC X(05)   VALUE SPACES.        ECS012
00375      12  FILLER                  PIC X(27)                        ECS012
00376              VALUE 'CODE    NUMBER        COUNT'.                 ECS012
00377      12  FILLER                  PIC X(35)                        ECS012
00378              VALUE '       PREMIUM   COUNT      PREMIUM'.         ECS012
00379      12  FILER                   PIC X(17)   VALUE SPACES.        ECS012
00380      12  FILLER                  PIC X(40)                        ECS012
00381              VALUE 'COUNT       PREMIUM   COUNT      PREMIUM'.    ECS012
00382                                                                   ECS012
00383  01  DT1.                                                         ECS012
00384      12  LINE-LEFT.                                               ECS012
00385          16  FILLER              PIC X(05).                       ECS012
00386          16  DT1-CITY-CNTY       PIC X(06).                       ECS012
00387          16  FILLER              PIC X(02).                       ECS012
00388          16  DT1-ACCT            PIC X(10).                       ECS012
00389      12  FILLER.                                                  ECS012
00390          16  FILLER              PIC X(02).                       ECS012
00391          16  DT1-TYPE            PIC X(05).                       ECS012
00392          16  DT1-LISS-CT         PIC Z(05).                       ECS012
00393          16  FILLER              PIC X(01).                       ECS012
00394          16  DT1-LISS-AMT        PIC ZZ,ZZZ,ZZZ.ZZ.               ECS012
00395          16  FILLER              PIC X(03).                       ECS012
00396          16  DT1-LCAN-CNT        PIC Z(05).                       ECS012
00397          16  FILLER              PIC X(01).                       ECS012
00398          16  DT1-LCAN-AMT        PIC Z,ZZZ,ZZZ.ZZ.                ECS012
00399          16  FILLER              PIC X(17).                       ECS012
00400          16  DT1-AHISS-CT        PIC Z(05).                       ECS012
00401          16  FILLER              PIC X(01).                       ECS012
00402          16  DT1-AHISS-AMT       PIC ZZ,ZZZ,ZZZ.ZZ.               ECS012
00403          16  FILLER              PIC X(03).                       ECS012
00404          16  DT1-AHCAN-CT        PIC Z(05).                       ECS012
00405          16  FILLER              PIC X(01).                       ECS012
00406          16  DT1-AHCAN-AMT       PIC Z,ZZZ,ZZZ.ZZ.                ECS012
00407                                                                   ECS012
00408      EJECT                                                        ECS012
00409                                  COPY ELCDATE.                       CL**8
00410                                                                   ECS012
00411      EJECT                                                        ECS012
00412                                  COPY ELCDTECX.                   ECS012
00413      EJECT                                                        ECS012
00414                                  COPY ELCDTEVR.                   ECS012
00415      EJECT                                                        ECS012
00416                                  COPY ELCEPCVR.                   ECS012
00417                                                                   ECS012
00418      EJECT                                                        ECS012
00419  PROCEDURE DIVISION.                                              ECS012
00420                                                                   ECS012
00421  0000-BEGIN.                                                      ECS012
00422                                  COPY ELCDTERX.                   ECS012
00423                                                                   ECS012
00424      MOVE WS-CURRENT-DATE        TO HD-RD.                        ECS012
00425      MOVE COMPANY-NAME           TO HD-CO.                        ECS012
00426      MOVE ALPH-DATE              TO HD-DT.                        ECS012
00427      MOVE LIFE-OVERRIDE-L6       TO HD8-OVRD-1.                   ECS012
00428      MOVE AH-OVERRIDE-L6         TO HD8-OVRD-2.                   ECS012
00429                                                                   ECS012
00430      MOVE RUN-DATE               TO RUN-DT                        ECS012
00431                                     BEG-DATE.                        CL**7
00432      DISPLAY ' RUN DATE = ' RUN-DT.                                  CL**7
00433                                                                   ECS012
00434      MOVE BIN-RUN-DATE           TO WS-BIN-RUN-DATE.                 CL**7
00435                                                                   ECS012
00436 * SET BEGIN DATE FOR MTD TOTALS                                   ECS012
00437                                                                   ECS012
00438      MOVE WS-BIN-RUN-DATE        TO DC-BIN-DATE-1.                   CL**7
00439      MOVE '6'                    TO DC-OPTION-CODE.                  CL**7
00440      MOVE +0                     TO DC-ELAPSED-DAYS.                 CL**7
00441      MOVE -1                     TO DC-ELAPSED-MONTHS.               CL**7
00442      MOVE '1'                    TO DC-END-OF-MONTH.                 CL**7
00443      PERFORM 0800-DATE-CONVERT THRU 0899-EXIT.                       CL**7
00444                                                                   ECS012
00445      IF NO-CONVERSION-ERROR                                       ECS012
00446         MOVE DC-BIN-DATE-2       TO WS-BIN-MTD-DATE               ECS012
00447         MOVE DC-GREG-DATE-1-YMD  TO MTD-DATE                      ECS012
00448         MOVE DC-ALPHA-CEN-N   TO MTD-CC                           ECS012
00449      ELSE                                                         ECS012
00450         MOVE 'INVALID MTD DATE CONVERSION'                        ECS012
00451                                  TO WS-ABEND-MESSAGE              ECS012
00452         MOVE DC-ERROR-CODE       TO WS-ABEND-FILE-STATUS          ECS012
00453         GO TO ABEND-PGM.                                          ECS012
00454                                                                   ECS012
00455      DISPLAY ' MTD DATE = ' MTD-DATE.                                CL**7
00456 * SET BEGIN DATE FOR QTD TOTALS                                   ECS012
00457                                                                   ECS012
00458      MOVE WS-BIN-RUN-DATE        TO DC-BIN-DATE-1.                   CL**7
00459      MOVE '6'                    TO DC-OPTION-CODE.                  CL**7
00460      MOVE +0                     TO DC-ELAPSED-DAYS.                 CL**7
00461      MOVE -3                     TO DC-ELAPSED-MONTHS.               CL**7
00462      MOVE '1'                    TO DC-END-OF-MONTH.                 CL**7
00463      PERFORM 0800-DATE-CONVERT THRU 0899-EXIT.                       CL**7
00464                                                                   ECS012
00465      IF NO-CONVERSION-ERROR                                       ECS012
00466         MOVE DC-BIN-DATE-2       TO WS-BIN-QTD-DATE               ECS012
00467         MOVE DC-GREG-DATE-1-YMD  TO QTD-DATE                      ECS012
00468         MOVE DC-ALPHA-CEN-N      TO QTD-CC                        ECS012
00469      ELSE                                                         ECS012
00470         MOVE 'INVALID QTD DATE CONVERSION'                        ECS012
00471                                  TO WS-ABEND-MESSAGE              ECS012
00472         MOVE DC-ERROR-CODE       TO WS-ABEND-FILE-STATUS          ECS012
00473         GO TO ABEND-PGM.                                          ECS012
00474                                                                   ECS012
00475      DISPLAY ' QTD DATE = ' QTD-DATE.                                CL**7
00476 * SET BEGIN DATE FOR L12 TOTALS                                   ECS012
00477                                                                   ECS012
00478      MOVE WS-BIN-RUN-DATE        TO DC-BIN-DATE-1.                   CL**7
00479      MOVE '6'                    TO DC-OPTION-CODE.                  CL**7
00480      MOVE +0                     TO DC-ELAPSED-DAYS.                 CL**7
00481      MOVE -12                    TO DC-ELAPSED-MONTHS.               CL**7
00482      MOVE '1'                    TO DC-END-OF-MONTH.                 CL**7
00483      PERFORM 0800-DATE-CONVERT THRU 0899-EXIT.                       CL**7
00484                                                                   ECS012
00485      IF NO-CONVERSION-ERROR                                       ECS012
00486         MOVE DC-BIN-DATE-2       TO WS-BIN-L12-DATE               ECS012
00487         MOVE DC-GREG-DATE-1-YMD  TO L12-DATE                      ECS012
00488      ELSE                                                         ECS012
00489         MOVE 'INVALID L12 DATE CONVERSION'                        ECS012
00490                                  TO WS-ABEND-MESSAGE              ECS012
00491         MOVE DC-ERROR-CODE       TO WS-ABEND-FILE-STATUS          ECS012
00492         GO TO ABEND-PGM.                                          ECS012
00493                                                                   ECS012
00494      DISPLAY ' L12 DATE = ' L12-DATE.                                CL**7
00495 * SET BEGIN DATE FOR P12 TOTALS                                   ECS012
00496                                                                   ECS012
00497      MOVE WS-BIN-RUN-DATE        TO DC-BIN-DATE-1.                   CL**7
00498      MOVE '6'                    TO DC-OPTION-CODE.                  CL**7
00499      MOVE +0                     TO DC-ELAPSED-DAYS.                 CL**7
00500      MOVE -24                    TO DC-ELAPSED-MONTHS.               CL**7
00501      MOVE '1'                    TO DC-END-OF-MONTH.                 CL**7
00502      PERFORM 0800-DATE-CONVERT THRU 0899-EXIT.                       CL**7
00503                                                                   ECS012
00504      IF NO-CONVERSION-ERROR                                       ECS012
00505         MOVE DC-BIN-DATE-2       TO WS-BIN-P12-DATE               ECS012
00506         MOVE DC-GREG-DATE-1-YMD  TO P12-DATE                      ECS012
00507      ELSE                                                         ECS012
00508         MOVE 'INVALID P12 DATE CONVERSION'                        ECS012
00509                                  TO WS-ABEND-MESSAGE              ECS012
00510         MOVE DC-ERROR-CODE       TO WS-ABEND-FILE-STATUS          ECS012
00511         GO TO ABEND-PGM.                                          ECS012
00512                                                                   ECS012
00513      DISPLAY ' P12 DATE = ' P12-DATE.                                CL**7
00514 * SET BEGIN DATE FOR YTD TOTALS                                   ECS012
00515                                                                   ECS012
00516      MOVE RUN-DT                 TO YTD-DATE.                        CL**7
00517      SUBTRACT 1                  FROM YTD-CCYY.                      CL**7
00518      MOVE 12                     TO YTD-MM.                          CL**7
00519      MOVE 31                     TO YTD-DD.                          CL**7
00520                                                                   ECS012
00521      DISPLAY ' YTD DATE = ' YTD-DATE                              ECS012
00522 * SET BEGIN DATE FOR PYTD TOTALS                                  ECS012
00523                                                                   ECS012
00524      MOVE YTD-DATE               TO PYTD-DATE.                       CL**7
00525      SUBTRACT 1                  FROM PYTD-CCYY.                  ECS012
00526                                                                   ECS012
00527      DISPLAY ' PYTD DATE = ' PYTD-DATE.                           ECS012
00528  0000-SORT-ONE.                                                   ECS012
00529                                                                   ECS012
00530      OPEN INPUT ACC-MSTR                                          ECS012
00531                 EPEC-FILE                                         ECS012
00532          OUTPUT PRNTR.                                            ECS012
00533                                                                   ECS012
00534      IF ERACCT-FILE-STATUS EQUAL '00' OR '97'                     ECS012
00535         NEXT SENTENCE                                             ECS012
00536      ELSE                                                         ECS012
00537         MOVE 'ERROR OCURRED OPEN ERACCT '                         ECS012
00538                                  TO WS-ABEND-MESSAGE              ECS012
00539         MOVE ERACCT-FILE-STATUS  TO WS-ABEND-FILE-STATUS          ECS012
00540         GO TO ABEND-PGM.                                          ECS012
00541                                                                   ECS012
00542      SORT SORT-WORK-A ON ASCENDING                                ECS012
00543                          SWA-CONTROL                              ECS012
00544          INPUT PROCEDURE   0010-BUILD-EXTRACTS THRU 0099-EXIT     ECS012
00545          OUTPUT PROCEDURE  0100-SORT-ONE-OUTPUT THRU 0700-EXIT.   ECS012
00546                                                                   ECS012
00547      IF SORT-RETURN NOT = ZERO AND 4                              ECS012
00548          MOVE SORT-RETURN        TO WS-RETURN-CODE                ECS012
00549          MOVE 'UNSUCCESSFUL SORT - SORT 1' TO WS-ABEND-MESSAGE    ECS012
00550          GO TO ABEND-PGM.                                         ECS012
00551                                                                   ECS012
00552      CLOSE  ACC-MSTR                                              ECS012
00553             EPEC-FILE                                             ECS012
00554             PRNTR.                                                ECS012
00555                                                                   ECS012
00556      IF ERACCT-FILE-STATUS EQUAL '00'                             ECS012
00557         NEXT SENTENCE                                             ECS012
00558      ELSE                                                         ECS012
00559         MOVE 'ERROR OCURRED CLOSE ERACCT '                        ECS012
00560                                  TO WS-ABEND-MESSAGE              ECS012
00561         MOVE ERACCT-FILE-STATUS  TO WS-ABEND-FILE-STATUS          ECS012
00562         GO TO ABEND-PGM.                                          ECS012
00563                                                                   ECS012
00564      GOBACK.                                                      ECS012
00565                                                                   ECS012
00566      EJECT                                                        ECS012
00567  0010-BUILD-EXTRACTS SECTION.                                     ECS012
00568                                                                   ECS012
00569      MOVE SPACES                 TO SORT-RECORD-OUT.              ECS012
00570                                                                   ECS012
00571  0020-READ-EXTRACT.                                               ECS012
00572                                                                   ECS012
00573      READ EPEC-FILE AT END                                        ECS012
00574          MOVE HIGH-VALUES        TO EP-CONTROL                    ECS012
00575          GO TO 0099-EXIT.                                         ECS012
00576                                                                   ECS012
00577      IF EP-RECORD-ID NOT EQUAL 'EP'                               ECS012
00578          GO TO 0020-READ-EXTRACT.                                 ECS012
00579                                                                   ECS012
00580      IF EP-REIN IS EQUAL TO 'R'                                   ECS012
00581          GO TO 0020-READ-EXTRACT.                                 ECS012
00582                                                                   ECS012
00583      IF WS-EP-RUN-DTE  GREATER RUN-DT                             ECS012
00584          GO TO 0020-READ-EXTRACT.                                 ECS012
00585                                                                   ECS012
00586      COPY ELCEPCM1.                                               ECS012
00587                                                                   ECS012
00588      IF EP-PURGE NOT EQUAL 'P'                                    ECS012
00589         IF WS-EP-RUN-DTE  LESS THAN YTD-DATE                      ECS012
00590             GO TO 0020-READ-EXTRACT.                              ECS012
00591                                                                   ECS012
00592      IF EP-PURGE NOT EQUAL 'P'                                    ECS012
00593         IF WS-EP-RUN-DTE  EQUAL MTD-DATE OR QTD-DATE              ECS012
00594                          OR YTD-DATE OR RUN-DT                    ECS012
00595            NEXT SENTENCE                                          ECS012
00596         ELSE                                                      ECS012
00597            GO TO 0020-READ-EXTRACT.                               ECS012
00598                                                                   ECS012
00599      MOVE EP-CONTROL             TO WS-EP-CONTROL.                ECS012
00600                                                                   ECS012
00601  0030-CHECK-ACCT-MSTR.                                            ECS012
00602                                                                   ECS012
00603      IF WS-ACCT-CONTROL LESS WS-EP-CONTROL                        ECS012
00604         PERFORM 0750-READ-ACCT THRU 0799-EXIT                     ECS012
00605         GO TO 0030-CHECK-ACCT-MSTR.                               ECS012
00606                                                                   ECS012
00607      IF WS-ACCT-CONTROL GREATER WS-EP-CONTROL                     ECS012
00608         DISPLAY 'NO MATCHING ACCOUNT ' EP-CONTROL                 ECS012
00609         MOVE SPACES              TO  SORT-CITY                    ECS012
00610                                      SORT-COUNTY                  ECS012
00611         GO TO 0020-READ-EXTRACT.                                  ECS012
00612                                                                   ECS012
00613  0050-PREMIUM.                                                    ECS012
00614                                                                   ECS012
00615      MOVE EP-CARRIER             TO  SORT-CARRIER.                ECS012
00616      MOVE EP-GROUPING            TO  SORT-GROUP.                  ECS012
00617      MOVE EP-STATE               TO  SORT-STATE.                  ECS012
00618      MOVE EP-ACCOUNT             TO  SORT-ACCOUNT.                ECS012
00619      MOVE AM-CITY-CODE           TO  SORT-CITY.                   ECS012
00620      MOVE AM-COUNTY-PARISH       TO  SORT-COUNTY.                 ECS012
00621                                                                   ECS012
00622      MOVE '1'                    TO  SORT-CT-CD.                  ECS012
00623      COPY ELCEPCM2.                                               ECS012
00624      MOVE EP-RECORD              TO  SORT-REST-OF-RECORD.         ECS012
00625      RELEASE SORT-RECORD-OUT.                                     ECS012
00626                                                                   ECS012
00627      MOVE '2'                    TO  SORT-CT-CD.                  ECS012
00628      RELEASE SORT-RECORD-OUT.                                     ECS012
00629                                                                   ECS012
00630      MOVE SPACES                 TO  SORT-RECORD-OUT.             ECS012
00631      GO TO 0020-READ-EXTRACT.                                     ECS012
00632                                                                   ECS012
00633  0099-EXIT.                                                       ECS012
00634      EXIT.                                                        ECS012
00635                                                                   ECS012
00636      EJECT                                                        ECS012
00637  0100-SORT-ONE-OUTPUT SECTION.                                    ECS012
00638                                                                   ECS012
00639  0100-INITIAL-WORK-AREAS.                                         ECS012
00640                                                                   ECS012
00641      MOVE +0                     TO  SUB.                         ECS012
00642      PERFORM 1060-INITIAL-CARR-TOTALS THRU 1069-IC-EXIT.          ECS012
00643      MOVE +0                     TO  SUB.                         ECS012
00644      PERFORM 1070-INITIAL-GRP-TOTALS THRU 1079-IG-EXIT.           ECS012
00645      MOVE +0                     TO  SUB.                         ECS012
00646      PERFORM 1080-INITIAL-STATE-TOTALS THRU 1089-IS-EXIT.         ECS012
00647      MOVE +0                     TO  SUB.                         ECS012
00648      PERFORM 1090-INITIAL-CITY-TOTALS THRU 1099-ICITY-EXIT.       ECS012
00649      MOVE +0                     TO  SUB.                         ECS012
00650      PERFORM 1100-INITIAL-ACCT-TOTALS THRU 1109-IA-EXIT.          ECS012
00651      MOVE +0                     TO  SUB.                         ECS012
00652      PERFORM 1110-INITIAL-CNTY-TOTALS THRU 1119-ICNTY-EXIT.       ECS012
00653      MOVE +0                     TO  SUB.                         ECS012
00654      PERFORM 1120-INITIAL-FINAL-TOTALS THRU 1129-IF-EXIT.         ECS012
00655      MOVE +0                     TO  SUB.                         ECS012
00656      PERFORM 1130-INITIAL-INTERMED-TOTALS THRU 1139-II-EXIT.      ECS012
00657                                                                   ECS012
00658  0200-RETURN-RECORD.                                              ECS012
00659                                                                   ECS012
00660      RETURN SORT-WORK-A AT END                                    ECS012
00661         PERFORM 0900-COMPUTE-ACCT-TOTALS THRU 0919-ACCT-EXIT      ECS012
00662         PERFORM 0930-COMPUTE-CNTY-TOTALS THRU 0939-CNTY-EXIT      ECS012
00663         MOVE +90                 TO  LNCTR                        ECS012
00664         PERFORM 0940-COMPUTE-STATE-TOTALS THRU 0949-STATE-EXIT    ECS012
00665         MOVE +90                 TO  LNCTR                        ECS012
00666         PERFORM 0950-COMPUTE-GRP-TOTALS THRU 0959-GRP-EXIT        ECS012
00667         MOVE +90                 TO  LNCTR                        ECS012
00668         PERFORM 0960-COMPUTE-CARR-TOTALS THRU 0969-CARR-EXIT      ECS012
00669         MOVE +90                 TO  LNCTR                        ECS012
00670         PERFORM 0970-COMPUTE-TOTAL-TOTALS THRU 0979-TOTAL-EXIT    ECS012
00671         GO TO 0700-EXIT.                                          ECS012
00672                                                                   ECS012
00673      MOVE SORT-REST-OF-RECORD    TO  EP-RECORD.                   ECS012
00674                                                                   ECS012
00675      COPY ELCEPCM1.                                               ECS012
00676                                                                   ECS012
00677      IF PREV-CARRIER IS EQUAL TO LOW-VALUE                        ECS012
00678          MOVE SORT-CARRIER       TO  PREV-CARRIER                 ECS012
00679          MOVE SORT-GROUP         TO  PREV-GROUPING                ECS012
00680          MOVE SORT-STATE         TO  PREV-STATE                   ECS012
00681          MOVE SORT-CITY          TO  PREV-CITY                    ECS012
00682          MOVE SORT-ACCOUNT       TO  PREV-ACCOUNT                 ECS012
00683          MOVE SORT-COUNTY        TO  PREV-COUNTY                  ECS012
00684          MOVE SORT-CT-CD         TO  PREV-CT-CD                   ECS012
00685          MOVE 'CITY'             TO  H7-SUB-BY                    ECS012
00686                                      HD9-CTY-CNTY                 ECS012
00687          PERFORM 1020-SET-CARRIER THRU 1029-SET-CARR-EXIT         ECS012
00688          PERFORM 1030-SET-GROUPING THRU 1039-SET-GRP-EXIT         ECS012
00689          PERFORM 1040-SET-STATE THRU 1049-SET-STATE-EXIT.         ECS012
00690                                                                   ECS012
00691      EJECT                                                        ECS012
00692  0500-CONTINUE.                                                   ECS012
00693                                                                   ECS012
00694      IF SORT-CT-CD IS EQUAL TO '2'                                ECS012
00695          IF PREV-CT-CD IS EQUAL TO '1'                            ECS012
00696              PERFORM 0900-COMPUTE-ACCT-TOTALS THRU 0919-ACCT-EXIT ECS012
00697              PERFORM 0920-COMPUTE-CITY-TOTALS THRU 0929-EXIT      ECS012
00698              MOVE +0             TO  SUB                          ECS012
00699              PERFORM 1100-INITIAL-ACCT-TOTALS THRU 1109-IA-EXIT   ECS012
00700              MOVE +0             TO  SUB                          ECS012
00701              PERFORM 1090-INITIAL-CITY-TOTALS THRU 1099-ICITY-EXITECS012
00702              MOVE 90             TO  LNCTR                        ECS012
00703              MOVE SORT-CARRIER   TO  PREV-CARRIER                 ECS012
00704              MOVE SORT-GROUP     TO  PREV-GROUPING                ECS012
00705              MOVE SORT-STATE     TO  PREV-STATE                   ECS012
00706              MOVE SORT-CITY      TO  PREV-CITY                    ECS012
00707              MOVE SORT-ACCOUNT   TO  PREV-ACCOUNT                 ECS012
00708              MOVE SORT-COUNTY    TO  PREV-COUNTY                  ECS012
00709              MOVE SORT-CT-CD     TO  PREV-CT-CD                   ECS012
00710              GO TO 0550-PROCESS-CNTY-RCDS                         ECS012
00711          ELSE                                                     ECS012
00712              GO TO 0550-PROCESS-CNTY-RCDS.                        ECS012
00713                                                                   ECS012
00714      IF SORT-CT-CD IS EQUAL TO '1'                                ECS012
00715          IF PREV-CT-CD IS EQUAL TO '2'                            ECS012
00716              MOVE SORT-CT-CD     TO  PREV-CT-CD                   ECS012
00717          ELSE                                                     ECS012
00718              GO TO 0525-PROCESS-CITY-RCDS.                        ECS012
00719                                                                   ECS012
00720      PERFORM 0900-COMPUTE-ACCT-TOTALS THRU 0919-ACCT-EXIT.        ECS012
00721      MOVE SORT-ACCOUNT           TO  PREV-ACCOUNT.                ECS012
00722      MOVE +0                     TO  SUB.                         ECS012
00723      PERFORM 1100-INITIAL-ACCT-TOTALS THRU 1109-IA-EXIT.          ECS012
00724      PERFORM 0930-COMPUTE-CNTY-TOTALS THRU 0939-CNTY-EXIT.        ECS012
00725      MOVE SORT-COUNTY            TO  PREV-COUNTY.                 ECS012
00726      MOVE +0                     TO  SUB.                         ECS012
00727      PERFORM 1110-INITIAL-CNTY-TOTALS THRU 1119-ICNTY-EXIT.       ECS012
00728                                                                   ECS012
00729      IF SORT-CARRIER IS NOT EQUAL TO PREV-CARRIER                 ECS012
00730          MOVE 90                 TO  LNCTR                        ECS012
00731          PERFORM 0940-COMPUTE-STATE-TOTALS THRU 0949-STATE-EXIT   ECS012
00732          MOVE SORT-STATE         TO  PREV-STATE                   ECS012
00733          PERFORM 1040-SET-STATE THRU 1049-SET-STATE-EXIT          ECS012
00734          PERFORM 0950-COMPUTE-GRP-TOTALS THRU 0959-GRP-EXIT       ECS012
00735          MOVE SORT-GROUP         TO  PREV-GROUPING                ECS012
00736          PERFORM 1030-SET-GROUPING THRU 1039-SET-GRP-EXIT         ECS012
00737          PERFORM 0960-COMPUTE-CARR-TOTALS THRU 0969-CARR-EXIT     ECS012
00738          MOVE SORT-CARRIER       TO  PREV-CARRIER                 ECS012
00739          PERFORM 1020-SET-CARRIER THRU 1029-SET-CARR-EXIT         ECS012
00740          MOVE +0                 TO  SUB                          ECS012
00741          PERFORM 1080-INITIAL-STATE-TOTALS THRU 1089-IS-EXIT      ECS012
00742          MOVE +0                 TO  SUB                          ECS012
00743          PERFORM 1070-INITIAL-GRP-TOTALS THRU 1079-IG-EXIT        ECS012
00744          MOVE +0                 TO  SUB                          ECS012
00745          PERFORM 1060-INITIAL-CARR-TOTALS THRU 1069-IC-EXIT.      ECS012
00746                                                                   ECS012
00747      IF SORT-GROUP IS NOT EQUAL TO PREV-GROUPING                  ECS012
00748          MOVE 90                 TO  LNCTR                        ECS012
00749          PERFORM 0940-COMPUTE-STATE-TOTALS THRU 0949-STATE-EXIT   ECS012
00750          MOVE SORT-STATE         TO  PREV-STATE                   ECS012
00751          PERFORM 1040-SET-STATE THRU 1049-SET-STATE-EXIT          ECS012
00752          PERFORM 0950-COMPUTE-GRP-TOTALS THRU 0959-GRP-EXIT       ECS012
00753          MOVE SORT-GROUP         TO  PREV-GROUPING                ECS012
00754          PERFORM 1030-SET-GROUPING THRU 1039-SET-GRP-EXIT         ECS012
00755          MOVE +0                 TO  SUB                          ECS012
00756          PERFORM 1080-INITIAL-STATE-TOTALS THRU 1089-IS-EXIT      ECS012
00757          MOVE +0                 TO  SUB                          ECS012
00758          PERFORM 1070-INITIAL-GRP-TOTALS THRU 1079-IG-EXIT.       ECS012
00759                                                                   ECS012
00760      IF SORT-STATE IS NOT EQUAL TO PREV-STATE                     ECS012
00761          MOVE 90                 TO  LNCTR                        ECS012
00762          PERFORM 0940-COMPUTE-STATE-TOTALS THRU 0949-STATE-EXIT   ECS012
00763          MOVE SORT-STATE         TO  PREV-STATE                   ECS012
00764          PERFORM 1040-SET-STATE THRU 1049-SET-STATE-EXIT          ECS012
00765          MOVE +0                 TO  SUB                          ECS012
00766          PERFORM 1080-INITIAL-STATE-TOTALS THRU 1089-IS-EXIT.     ECS012
00767                                                                   ECS012
00768  0525-PROCESS-CITY-RCDS.                                          ECS012
00769                                                                   ECS012
00770      MOVE 'CITY'                 TO  H7-SUB-BY                    ECS012
00771                                      HD9-CTY-CNTY.                ECS012
00772                                                                   ECS012
00773      IF SORT-ACCOUNT IS NOT EQUAL TO PREV-ACCOUNT                 ECS012
00774          PERFORM 0900-COMPUTE-ACCT-TOTALS THRU 0919-ACCT-EXIT     ECS012
00775          MOVE SORT-ACCOUNT       TO  PREV-ACCOUNT                 ECS012
00776          MOVE +0                 TO  SUB                          ECS012
00777          PERFORM 1100-INITIAL-ACCT-TOTALS THRU 1109-IA-EXIT.      ECS012
00778                                                                   ECS012
00779      IF SORT-CITY IS NOT EQUAL TO PREV-CITY                       ECS012
00780          PERFORM 0900-COMPUTE-ACCT-TOTALS THRU 0919-ACCT-EXIT     ECS012
00781          PERFORM 0920-COMPUTE-CITY-TOTALS THRU 0929-EXIT          ECS012
00782          MOVE +0                 TO  SUB                          ECS012
00783          PERFORM 1100-INITIAL-ACCT-TOTALS THRU 1109-IA-EXIT       ECS012
00784          MOVE +0                 TO  SUB                          ECS012
00785          PERFORM 1090-INITIAL-CITY-TOTALS THRU 1099-ICITY-EXIT    ECS012
00786          MOVE 90                 TO  LNCTR                        ECS012
00787          MOVE SORT-ACCOUNT       TO  PREV-ACCOUNT                 ECS012
00788          MOVE SORT-CITY          TO  PREV-CITY.                   ECS012
00789                                                                   ECS012
00790      GO TO 0575-PROCESS-EPEC-RCDS.                                ECS012
00791                                                                   ECS012
00792  0550-PROCESS-CNTY-RCDS.                                          ECS012
00793                                                                   ECS012
00794      MOVE 'COUNTY'               TO  H7-SUB-BY.                   ECS012
00795      MOVE 'CNTY'                 TO  HD9-CTY-CNTY.                ECS012
00796                                                                   ECS012
00797      IF SORT-ACCOUNT IS NOT EQUAL TO PREV-ACCOUNT                 ECS012
00798          PERFORM 0900-COMPUTE-ACCT-TOTALS THRU 0919-ACCT-EXIT     ECS012
00799          MOVE SORT-ACCOUNT       TO  PREV-ACCOUNT                 ECS012
00800          MOVE +0                 TO  SUB                          ECS012
00801          PERFORM 1100-INITIAL-ACCT-TOTALS THRU 1109-IA-EXIT.      ECS012
00802                                                                   ECS012
00803      EJECT                                                        ECS012
00804  0575-PROCESS-EPEC-RCDS.                                          ECS012
00805                                                                   ECS012
00806      IF EP-PURGE EQUAL 'P'                                        ECS012
00807         NEXT SENTENCE                                             ECS012
00808      ELSE                                                         ECS012
00809         GO TO 0576-NOT-PURGE.                                     ECS012
00810                                                                   ECS012
00811      IF WS-EP-RUN-DTE  NOT GREATER THAN YTD-DATE                  ECS012
00812         MOVE +1 TO SUB1                                           ECS012
00813         PERFORM 0600-ADD-TO-TOTALS THRU 0699-EXIT                 ECS012
00814         MOVE +2 TO SUB1                                           ECS012
00815         PERFORM 0600-ADD-TO-TOTALS THRU 0699-EXIT                 ECS012
00816         MOVE +3 TO SUB1                                           ECS012
00817         PERFORM 0600-ADD-TO-TOTALS THRU 0699-EXIT                 ECS012
00818         MOVE +7 TO SUB1                                           ECS012
00819         PERFORM 0600-ADD-TO-TOTALS THRU 0699-EXIT                 ECS012
00820      ELSE                                                         ECS012
00821      IF WS-EP-RUN-DTE NOT GREATER THAN QTD-DATE                   ECS012
00822         MOVE +1 TO SUB1                                           ECS012
00823         PERFORM 0600-ADD-TO-TOTALS THRU 0699-EXIT                 ECS012
00824         MOVE +2 TO SUB1                                           ECS012
00825         PERFORM 0600-ADD-TO-TOTALS THRU 0699-EXIT                 ECS012
00826         MOVE +7 TO SUB1                                           ECS012
00827         PERFORM 0600-ADD-TO-TOTALS THRU 0699-EXIT                 ECS012
00828      ELSE                                                         ECS012
00829      IF WS-EP-RUN-DTE  NOT GREATER THAN MTD-DATE                  ECS012
00830         MOVE +1 TO SUB1                                           ECS012
00831         PERFORM 0600-ADD-TO-TOTALS THRU 0699-EXIT                 ECS012
00832         MOVE +7 TO SUB1                                           ECS012
00833         PERFORM 0600-ADD-TO-TOTALS THRU 0699-EXIT                 ECS012
00834      ELSE                                                         ECS012
00835      IF WS-EP-RUN-DTE  NOT GREATER THAN RUN-DT                    ECS012
00836         MOVE +7 TO SUB1                                           ECS012
00837         PERFORM 0600-ADD-TO-TOTALS THRU 0699-EXIT.                ECS012
00838                                                                   ECS012
00839      GO TO 0200-RETURN-RECORD.                                    ECS012
00840                                                                   ECS012
00841  0576-NOT-PURGE.                                                  ECS012
00842                                                                   ECS012
00843      IF WS-EP-RUN-DTE EQUAL YTD-DATE                              ECS012
00844         MOVE +3 TO SUB1                                           ECS012
00845         PERFORM 0600-ADD-TO-TOTALS THRU 0699-EXIT.                ECS012
00846                                                                   ECS012
00847      IF WS-EP-RUN-DTE  EQUAL QTD-DATE                             ECS012
00848         MOVE +2 TO SUB1                                           ECS012
00849         PERFORM 0600-ADD-TO-TOTALS THRU 0699-EXIT.                ECS012
00850                                                                   ECS012
00851      IF WS-EP-RUN-DTE  EQUAL MTD-DATE                             ECS012
00852         MOVE +1 TO SUB1                                           ECS012
00853         PERFORM 0600-ADD-TO-TOTALS THRU 0699-EXIT.                ECS012
00854                                                                   ECS012
00855      IF WS-EP-RUN-DTE  EQUAL RUN-DT                               ECS012
00856         MOVE +7 TO SUB1                                           ECS012
00857         PERFORM 0600-ADD-TO-TOTALS THRU 0699-EXIT.                ECS012
00858                                                                   ECS012
00859      GO TO 0200-RETURN-RECORD.                                    ECS012
00860                                                                   ECS012
00861      EJECT                                                        ECS012
00862  0600-ADD-TO-TOTALS.                                              ECS012
00863                                                                   ECS012
00864      IF WS-EP-EXP-DTE  GREATER THAN WS-HIGH-EXP-DT                ECS012
00865         MOVE EP-EXP-DTE          TO WS-HIGH-EXP-DT.               ECS012
00866                                                                   ECS012
00867      IF EP-ISS-CNT IS NOT NUMERIC                                 ECS012
00868          MOVE +0                 TO EP-ISS-CNT.                   ECS012
00869                                                                   ECS012
00870      IF SORT-CT-CD IS EQUAL TO '1'                                ECS012
00871          IF EP-RCD-TYPE IS EQUAL TO LIFE-OVERRIDE-L1              ECS012
00872              ADD EP-ISS-CNT      TO WS-CITY-LF-ISS-CNT  (SUB1)    ECS012
00873                                     WS-ACCT-LF-ISS-CNT  (SUB1)    ECS012
00874              ADD EP-CNC-CNT      TO WS-CITY-LF-CAN-CNT  (SUB1)    ECS012
00875                                     WS-ACCT-LF-CAN-CNT  (SUB1)    ECS012
00876              ADD EP-ISS-PRM      TO WS-CITY-LF-ISS-AMT  (SUB1)    ECS012
00877                                     WS-ACCT-LF-ISS-AMT  (SUB1)    ECS012
00878              ADD EP-CNC-PRM      TO WS-CITY-LF-CAN-AMT  (SUB1)    ECS012
00879                                     WS-ACCT-LF-CAN-AMT  (SUB1)    ECS012
00880              GO TO 0699-EXIT                                      ECS012
00881          ELSE                                                     ECS012
00882              ADD EP-ISS-CNT      TO WS-CITY-AH-ISS-CNT  (SUB1)    ECS012
00883                                     WS-ACCT-AH-ISS-CNT  (SUB1)    ECS012
00884              ADD EP-CNC-CNT      TO WS-CITY-AH-CAN-CNT  (SUB1)    ECS012
00885                                     WS-ACCT-AH-CAN-CNT  (SUB1)    ECS012
00886              ADD EP-ISS-PRM      TO WS-CITY-AH-ISS-AMT  (SUB1)    ECS012
00887                                     WS-ACCT-AH-ISS-AMT  (SUB1)    ECS012
00888              ADD EP-CNC-PRM      TO WS-CITY-AH-CAN-AMT  (SUB1)    ECS012
00889                                     WS-ACCT-AH-CAN-AMT  (SUB1)    ECS012
00890              GO TO 0699-EXIT.                                     ECS012
00891                                                                   ECS012
00892      IF EP-RCD-TYPE EQUAL LIFE-OVERRIDE-L1                        ECS012
00893          ADD EP-ISS-CNT          TO WS-CARR-LF-ISS-CNT  (SUB1)    ECS012
00894                                     WS-GRP-LF-ISS-CNT   (SUB1)    ECS012
00895                                     WS-ST-LF-ISS-CNT    (SUB1)    ECS012
00896                                     WS-ACCT-LF-ISS-CNT  (SUB1)    ECS012
00897                                     WS-CNTY-LF-ISS-CNT  (SUB1)    ECS012
00898                                     WS-TOTAL-LF-ISS-CNT (SUB1)    ECS012
00899          ADD EP-CNC-CNT          TO WS-CARR-LF-CAN-CNT  (SUB1)    ECS012
00900                                     WS-GRP-LF-CAN-CNT   (SUB1)    ECS012
00901                                     WS-ST-LF-CAN-CNT    (SUB1)    ECS012
00902                                     WS-ACCT-LF-CAN-CNT  (SUB1)    ECS012
00903                                     WS-CNTY-LF-CAN-CNT  (SUB1)    ECS012
00904                                     WS-TOTAL-LF-CAN-CNT (SUB1)    ECS012
00905          ADD EP-ISS-PRM          TO WS-CARR-LF-ISS-AMT  (SUB1)    ECS012
00906                                     WS-GRP-LF-ISS-AMT   (SUB1)    ECS012
00907                                     WS-ST-LF-ISS-AMT    (SUB1)    ECS012
00908                                     WS-ACCT-LF-ISS-AMT  (SUB1)    ECS012
00909                                     WS-CNTY-LF-ISS-AMT  (SUB1)    ECS012
00910                                     WS-TOTAL-LF-ISS-AMT (SUB1)    ECS012
00911          ADD EP-CNC-PRM          TO WS-CARR-LF-CAN-AMT  (SUB1)    ECS012
00912                                     WS-GRP-LF-CAN-AMT   (SUB1)    ECS012
00913                                     WS-ST-LF-CAN-AMT    (SUB1)    ECS012
00914                                     WS-ACCT-LF-CAN-AMT  (SUB1)    ECS012
00915                                     WS-CNTY-LF-CAN-AMT  (SUB1)    ECS012
00916                                     WS-TOTAL-LF-CAN-AMT (SUB1)    ECS012
00917      ELSE                                                         ECS012
00918          ADD EP-ISS-CNT          TO WS-CARR-AH-ISS-CNT  (SUB1)    ECS012
00919                                     WS-GRP-AH-ISS-CNT   (SUB1)    ECS012
00920                                     WS-ST-AH-ISS-CNT    (SUB1)    ECS012
00921                                     WS-ACCT-AH-ISS-CNT  (SUB1)    ECS012
00922                                     WS-CNTY-AH-ISS-CNT  (SUB1)    ECS012
00923                                     WS-TOTAL-AH-ISS-CNT (SUB1)    ECS012
00924          ADD EP-CNC-CNT          TO WS-CARR-AH-CAN-CNT  (SUB1)    ECS012
00925                                     WS-GRP-AH-CAN-CNT   (SUB1)    ECS012
00926                                     WS-ST-AH-CAN-CNT    (SUB1)    ECS012
00927                                     WS-ACCT-AH-CAN-CNT  (SUB1)    ECS012
00928                                     WS-CNTY-AH-CAN-CNT  (SUB1)    ECS012
00929                                     WS-TOTAL-AH-CAN-CNT (SUB1)    ECS012
00930          ADD EP-ISS-PRM          TO WS-CARR-AH-ISS-AMT  (SUB1)    ECS012
00931                                     WS-GRP-AH-ISS-AMT   (SUB1)    ECS012
00932                                     WS-ST-AH-ISS-AMT    (SUB1)    ECS012
00933                                     WS-ACCT-AH-ISS-AMT  (SUB1)    ECS012
00934                                     WS-CNTY-AH-ISS-AMT  (SUB1)    ECS012
00935                                     WS-TOTAL-AH-ISS-AMT (SUB1)    ECS012
00936          ADD EP-CNC-PRM          TO WS-CARR-AH-CAN-AMT  (SUB1)    ECS012
00937                                     WS-GRP-AH-CAN-AMT   (SUB1)    ECS012
00938                                     WS-ST-AH-CAN-AMT    (SUB1)    ECS012
00939                                     WS-ACCT-AH-CAN-AMT  (SUB1)    ECS012
00940                                     WS-CNTY-AH-CAN-AMT  (SUB1)    ECS012
00941                                     WS-TOTAL-AH-CAN-AMT (SUB1).   ECS012
00942                                                                   ECS012
00943  0699-EXIT.                                                       ECS012
00944      EXIT.                                                        ECS012
00945                                                                   ECS012
00946  0700-EXIT.                                                       ECS012
00947      EXIT.                                                        ECS012
00948                                                                   ECS012
00949      EJECT                                                        ECS012
00950  0725-PERFORM-ROUTINES SECTION.                                   ECS012
00951                                                                   ECS012
00952  0750-READ-ACCT.                                                  ECS012
00953                                                                   ECS012
00954      READ ACC-MSTR AT END                                         ECS012
00955         MOVE HIGH-VALUES         TO AM-MSTR-CNTRL                 ECS012
00956         MOVE ZEROS               TO AM-EFFECT-DT.                 ECS012
00957                                                                   ECS012
00958      IF ERACCT-FILE-STATUS EQUAL '10'                             ECS012
00959         MOVE HIGH-VALUES         TO AM-MSTR-CNTRL                 ECS012
00960         MOVE ZEROS               TO AM-EFFECT-DT                     CL**7
00961         MOVE AM-MSTR-CNTRL       TO WS-MSTR-CNTRL                 ECS012
00962         MOVE AM-EFFECT-DT        TO WS-EFFECT                     ECS012
00963         GO TO 0799-EXIT.                                          ECS012
00964                                                                   ECS012
00965      IF ERACCT-FILE-STATUS EQUAL '00'                             ECS012
00966         NEXT SENTENCE                                             ECS012
00967      ELSE                                                         ECS012
00968         MOVE 'ERROR OCURRED READ ERACCT '                         ECS012
00969                                  TO WS-ABEND-MESSAGE              ECS012
00970         MOVE ERACCT-FILE-STATUS  TO WS-ABEND-FILE-STATUS          ECS012
00971         GO TO ABEND-PGM.                                          ECS012
00972                                                                   ECS012
00973      MOVE AM-MSTR-CNTRL          TO WS-MSTR-CNTRL                 ECS012
00974      MOVE AM-EFFECT-DT           TO WS-EFFECT.                    ECS012
00975                                                                   ECS012
00976  0799-EXIT.                                                       ECS012
00977      EXIT.                                                        ECS012
00978                                                                   ECS012
00979  0800-DATE-CONVERT.                                               ECS012
00980                                                                   ECS012
00981      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   ECS012
00982                                                                   ECS012
00983  0899-EXIT.                                                       ECS012
00984      EXIT.                                                        ECS012
00985                                                                   ECS012
00986      EJECT                                                        ECS012
00987  0900-COMPUTE-ACCT-TOTALS.                                        ECS012
00988                                                                   ECS012
00989      MOVE SPACES                 TO  DT1.                         ECS012
00990                                                                   ECS012
00991 ***   COMPUTE MTD                                                 ECS012
00992                                                                   ECS012
00993       COMPUTE WS-INTER-LF-ISS-CNT (1)  EQUAL                      ECS012
00994          WS-ACCT-LF-ISS-CNT (7) - WS-ACCT-LF-ISS-CNT (1).         ECS012
00995       COMPUTE WS-INTER-LF-CAN-CNT (1)  EQUAL                      ECS012
00996          WS-ACCT-LF-CAN-CNT (7) - WS-ACCT-LF-CAN-CNT (1).         ECS012
00997       COMPUTE WS-INTER-LF-ISS-AMT (1)  EQUAL                      ECS012
00998          WS-ACCT-LF-ISS-AMT (7) - WS-ACCT-LF-ISS-AMT (1).         ECS012
00999       COMPUTE WS-INTER-LF-CAN-AMT (1)  EQUAL                      ECS012
01000          WS-ACCT-LF-CAN-AMT (7) - WS-ACCT-LF-CAN-AMT (1).         ECS012
01001       COMPUTE WS-INTER-AH-ISS-CNT (1)  EQUAL                      ECS012
01002          WS-ACCT-AH-ISS-CNT (7) - WS-ACCT-AH-ISS-CNT (1).         ECS012
01003       COMPUTE WS-INTER-AH-CAN-CNT (1)  EQUAL                      ECS012
01004          WS-ACCT-AH-CAN-CNT (7) - WS-ACCT-AH-CAN-CNT (1).         ECS012
01005       COMPUTE WS-INTER-AH-ISS-AMT (1)  EQUAL                      ECS012
01006          WS-ACCT-AH-ISS-AMT (7) - WS-ACCT-AH-ISS-AMT (1).         ECS012
01007       COMPUTE WS-INTER-AH-CAN-AMT (1)  EQUAL                      ECS012
01008          WS-ACCT-AH-CAN-AMT (7) - WS-ACCT-AH-CAN-AMT (1).         ECS012
01009                                                                   ECS012
01010 ***   COMPUTE QTD                                                 ECS012
01011                                                                   ECS012
01012      COMPUTE WS-INTER-LF-ISS-CNT (2)   EQUAL                      ECS012
01013         WS-ACCT-LF-ISS-CNT (7) - WS-ACCT-LF-ISS-CNT (2).          ECS012
01014      COMPUTE WS-INTER-LF-CAN-CNT (2)   EQUAL                      ECS012
01015         WS-ACCT-LF-CAN-CNT (7) - WS-ACCT-LF-CAN-CNT (2).          ECS012
01016      COMPUTE WS-INTER-LF-ISS-AMT (2)   EQUAL                      ECS012
01017         WS-ACCT-LF-ISS-AMT (7) - WS-ACCT-LF-ISS-AMT (2).          ECS012
01018      COMPUTE WS-INTER-LF-CAN-AMT (2)   EQUAL                      ECS012
01019         WS-ACCT-LF-CAN-AMT (7) - WS-ACCT-LF-CAN-AMT (2).          ECS012
01020      COMPUTE WS-INTER-AH-ISS-CNT (2)   EQUAL                      ECS012
01021         WS-ACCT-AH-ISS-CNT (7) - WS-ACCT-AH-ISS-CNT (2).          ECS012
01022      COMPUTE WS-INTER-AH-CAN-CNT (2)   EQUAL                      ECS012
01023         WS-ACCT-AH-CAN-CNT (7) - WS-ACCT-AH-CAN-CNT (2).          ECS012
01024      COMPUTE WS-INTER-AH-ISS-AMT (2)   EQUAL                      ECS012
01025         WS-ACCT-AH-ISS-AMT (7) - WS-ACCT-AH-ISS-AMT (2).          ECS012
01026      COMPUTE WS-INTER-AH-CAN-AMT (2)   EQUAL                      ECS012
01027         WS-ACCT-AH-CAN-AMT (7) - WS-ACCT-AH-CAN-AMT (2).          ECS012
01028                                                                   ECS012
01029                                                                   ECS012
01030 ***   COMPUTE YTD                                                 ECS012
01031                                                                   ECS012
01032      COMPUTE WS-INTER-LF-ISS-CNT (3)   EQUAL                      ECS012
01033         WS-ACCT-LF-ISS-CNT (7) - WS-ACCT-LF-ISS-CNT (3).          ECS012
01034      COMPUTE WS-INTER-LF-CAN-CNT (3)   EQUAL                      ECS012
01035         WS-ACCT-LF-CAN-CNT (7) - WS-ACCT-LF-CAN-CNT (3).          ECS012
01036      COMPUTE WS-INTER-LF-ISS-AMT (3)   EQUAL                      ECS012
01037         WS-ACCT-LF-ISS-AMT (7) - WS-ACCT-LF-ISS-AMT (3).          ECS012
01038      COMPUTE WS-INTER-LF-CAN-AMT (3)   EQUAL                      ECS012
01039         WS-ACCT-LF-CAN-AMT (7) - WS-ACCT-LF-CAN-AMT (3).          ECS012
01040      COMPUTE WS-INTER-AH-ISS-CNT (3)   EQUAL                      ECS012
01041         WS-ACCT-AH-ISS-CNT (7) - WS-ACCT-AH-ISS-CNT (3).          ECS012
01042      COMPUTE WS-INTER-AH-CAN-CNT (3)   EQUAL                      ECS012
01043         WS-ACCT-AH-CAN-CNT (7) - WS-ACCT-AH-CAN-CNT (3).          ECS012
01044      COMPUTE WS-INTER-AH-ISS-AMT (3)   EQUAL                      ECS012
01045         WS-ACCT-AH-ISS-AMT (7) - WS-ACCT-AH-ISS-AMT (3).          ECS012
01046      COMPUTE WS-INTER-AH-CAN-AMT (3)   EQUAL                      ECS012
01047         WS-ACCT-AH-CAN-AMT (7) - WS-ACCT-AH-CAN-AMT (3).          ECS012
01048                                                                   ECS012
01049  0910-PRINT-ACCT-TOTALS.                                          ECS012
01050                                                                   ECS012
01051      PERFORM 1010-CHECK-PRINT THRU 1019-CHECK-EXIT.               ECS012
01052      IF WS-PRINT-TOTAL-SW EQUAL 'Y'                               ECS012
01053          NEXT SENTENCE                                            ECS012
01054      ELSE                                                         ECS012
01055          GO TO 0919-ACCT-EXIT.                                    ECS012
01056                                                                   ECS012
01057      MOVE 1                          TO  WS-PRINT-HEADING-SW.     ECS012
01058      MOVE WS-INTER-LF-ISS-CNT (1)    TO  DT1-LISS-CT.             ECS012
01059      MOVE WS-INTER-LF-ISS-AMT (1)    TO  DT1-LISS-AMT.            ECS012
01060      MOVE WS-INTER-LF-CAN-CNT (1)    TO  DT1-LCAN-CNT.            ECS012
01061      MOVE WS-INTER-LF-CAN-AMT (1)    TO  DT1-LCAN-AMT.            ECS012
01062      MOVE WS-INTER-AH-ISS-CNT (1)    TO  DT1-AHISS-CT.            ECS012
01063      MOVE WS-INTER-AH-ISS-AMT (1)    TO  DT1-AHISS-AMT.           ECS012
01064      MOVE WS-INTER-AH-CAN-CNT (1)    TO  DT1-AHCAN-CT.            ECS012
01065      MOVE WS-INTER-AH-CAN-AMT (1)    TO  DT1-AHCAN-AMT.           ECS012
01066                                                                   ECS012
01067 *    MOVE SPACES                     TO  DT1-ACCT.                ECS012
01068      MOVE SPACES                     TO  LINE-LEFT.               ECS012
01069      MOVE ZERO                       TO  P-CTL.                   ECS012
01070      MOVE 'MTD'                      TO  DT1-TYPE.                ECS012
01071      PERFORM 0980-PRINT THRU 0989-PRINT-EXIT.                     ECS012
01072                                                                   ECS012
01073      MOVE WS-INTER-LF-ISS-CNT (2)    TO  DT1-LISS-CT.             ECS012
01074      MOVE WS-INTER-LF-ISS-AMT (2)    TO  DT1-LISS-AMT.            ECS012
01075      MOVE WS-INTER-LF-CAN-CNT (2)    TO  DT1-LCAN-CNT.            ECS012
01076      MOVE WS-INTER-LF-CAN-AMT (2)    TO  DT1-LCAN-AMT.            ECS012
01077      MOVE WS-INTER-AH-ISS-CNT (2)    TO  DT1-AHISS-CT.            ECS012
01078      MOVE WS-INTER-AH-ISS-AMT (2)    TO  DT1-AHISS-AMT.           ECS012
01079      MOVE WS-INTER-AH-CAN-CNT (2)    TO  DT1-AHCAN-CT.            ECS012
01080      MOVE WS-INTER-AH-CAN-AMT (2)    TO  DT1-AHCAN-AMT.           ECS012
01081                                                                   ECS012
01082      MOVE 'Y'                        TO  WS-DONT-BREAK.           ECS012
01083      MOVE SPACE                      TO  P-CTL.                      CL**2
01084      IF SORT-CT-CD IS EQUAL TO '1'                                ECS012
01085          MOVE PREV-CITY              TO  DT1-CITY-CNTY            ECS012
01086      ELSE                                                         ECS012
01087          MOVE PREV-COUNTY            TO  DT1-CITY-CNTY.           ECS012
01088      MOVE PREV-ACCOUNT               TO  DT1-ACCT.                ECS012
01089      MOVE 'QTD'                      TO  DT1-TYPE.                ECS012
01090      PERFORM 0980-PRINT THRU 0989-PRINT-EXIT.                     ECS012
01091                                                                   ECS012
01092      MOVE WS-INTER-LF-ISS-CNT (3)    TO  DT1-LISS-CT.             ECS012
01093      MOVE WS-INTER-LF-ISS-AMT (3)    TO  DT1-LISS-AMT.            ECS012
01094      MOVE WS-INTER-LF-CAN-CNT (3)    TO  DT1-LCAN-CNT.            ECS012
01095      MOVE WS-INTER-LF-CAN-AMT (3)    TO  DT1-LCAN-AMT.            ECS012
01096      MOVE WS-INTER-AH-ISS-CNT (3)    TO  DT1-AHISS-CT.            ECS012
01097      MOVE WS-INTER-AH-ISS-AMT (3)    TO  DT1-AHISS-AMT.           ECS012
01098      MOVE WS-INTER-AH-CAN-CNT (3)    TO  DT1-AHCAN-CT.            ECS012
01099      MOVE WS-INTER-AH-CAN-AMT (3)    TO  DT1-AHCAN-AMT.           ECS012
01100                                                                   ECS012
01101      MOVE SPACE                      TO  P-CTL.                      CL**2
01102      MOVE SPACES                     TO  LINE-LEFT.               ECS012
01103      MOVE 'Y'                        TO  WS-DONT-BREAK.           ECS012
01104      MOVE 'YTD'                      TO  DT1-TYPE.                ECS012
01105      PERFORM 0980-PRINT THRU 0989-PRINT-EXIT.                     ECS012
01106                                                                   ECS012
01107      MOVE +0                         TO  SUB.                     ECS012
01108      PERFORM 1130-INITIAL-INTERMED-TOTALS THRU 1139-II-EXIT.      ECS012
01109                                                                   ECS012
01110  0919-ACCT-EXIT.                                                  ECS012
01111      EXIT.                                                        ECS012
01112                                                                   ECS012
01113      EJECT                                                        ECS012
01114  0920-COMPUTE-CITY-TOTALS.                                        ECS012
01115                                                                   ECS012
01116      MOVE SPACES                 TO  DT1.                         ECS012
01117                                                                   ECS012
01118 ***   COMPUTE MTD                                                 ECS012
01119                                                                   ECS012
01120       COMPUTE WS-INTER-LF-ISS-CNT (1)  EQUAL                      ECS012
01121          WS-CITY-LF-ISS-CNT (7) - WS-CITY-LF-ISS-CNT (1).         ECS012
01122       COMPUTE WS-INTER-LF-CAN-CNT (1)  EQUAL                      ECS012
01123          WS-CITY-LF-CAN-CNT (7) - WS-CITY-LF-CAN-CNT (1).         ECS012
01124       COMPUTE WS-INTER-LF-ISS-AMT (1)  EQUAL                      ECS012
01125          WS-CITY-LF-ISS-AMT (7) - WS-CITY-LF-ISS-AMT (1).         ECS012
01126       COMPUTE WS-INTER-LF-CAN-AMT (1)  EQUAL                      ECS012
01127          WS-CITY-LF-CAN-AMT (7) - WS-CITY-LF-CAN-AMT (1).         ECS012
01128       COMPUTE WS-INTER-AH-ISS-CNT (1)  EQUAL                      ECS012
01129          WS-CITY-AH-ISS-CNT (7) - WS-CITY-AH-ISS-CNT (1).         ECS012
01130       COMPUTE WS-INTER-AH-CAN-CNT (1)  EQUAL                      ECS012
01131          WS-CITY-AH-CAN-CNT (7) - WS-CITY-AH-CAN-CNT (1).         ECS012
01132       COMPUTE WS-INTER-AH-ISS-AMT (1)  EQUAL                      ECS012
01133          WS-CITY-AH-ISS-AMT (7) - WS-CITY-AH-ISS-AMT (1).         ECS012
01134       COMPUTE WS-INTER-AH-CAN-AMT (1)  EQUAL                      ECS012
01135          WS-CITY-AH-CAN-AMT (7) - WS-CITY-AH-CAN-AMT (1).         ECS012
01136                                                                   ECS012
01137                                                                   ECS012
01138 ***   COMPUTE QTD                                                 ECS012
01139                                                                   ECS012
01140      COMPUTE WS-INTER-LF-ISS-CNT (2)   EQUAL                      ECS012
01141         WS-CITY-LF-ISS-CNT (7) - WS-CITY-LF-ISS-CNT (2).          ECS012
01142      COMPUTE WS-INTER-LF-CAN-CNT (2)   EQUAL                      ECS012
01143         WS-CITY-LF-CAN-CNT (7) - WS-CITY-LF-CAN-CNT (2).          ECS012
01144      COMPUTE WS-INTER-LF-ISS-AMT (2)   EQUAL                      ECS012
01145         WS-CITY-LF-ISS-AMT (7) - WS-CITY-LF-ISS-AMT (2).          ECS012
01146      COMPUTE WS-INTER-LF-CAN-AMT (2)   EQUAL                      ECS012
01147         WS-CITY-LF-CAN-AMT (7) - WS-CITY-LF-CAN-AMT (2).          ECS012
01148      COMPUTE WS-INTER-AH-ISS-CNT (2)   EQUAL                      ECS012
01149         WS-CITY-AH-ISS-CNT (7) - WS-CITY-AH-ISS-CNT (2).          ECS012
01150      COMPUTE WS-INTER-AH-CAN-CNT (2)   EQUAL                      ECS012
01151         WS-CITY-AH-CAN-CNT (7) - WS-CITY-AH-CAN-CNT (2).          ECS012
01152      COMPUTE WS-INTER-AH-ISS-AMT (2)   EQUAL                      ECS012
01153         WS-CITY-AH-ISS-AMT (7) - WS-CITY-AH-ISS-AMT (2).          ECS012
01154      COMPUTE WS-INTER-AH-CAN-AMT (2)   EQUAL                      ECS012
01155         WS-CITY-AH-CAN-AMT (7) - WS-CITY-AH-CAN-AMT (2).          ECS012
01156                                                                   ECS012
01157                                                                   ECS012
01158 ***   COMPUTE YTD                                                 ECS012
01159                                                                   ECS012
01160      COMPUTE WS-INTER-LF-ISS-CNT (3)   EQUAL                      ECS012
01161         WS-CITY-LF-ISS-CNT (7) - WS-CITY-LF-ISS-CNT (3).          ECS012
01162      COMPUTE WS-INTER-LF-CAN-CNT (3)   EQUAL                      ECS012
01163         WS-CITY-LF-CAN-CNT (7) - WS-CITY-LF-CAN-CNT (3).          ECS012
01164      COMPUTE WS-INTER-LF-ISS-AMT (3)   EQUAL                      ECS012
01165         WS-CITY-LF-ISS-AMT (7) - WS-CITY-LF-ISS-AMT (3).          ECS012
01166      COMPUTE WS-INTER-LF-CAN-AMT (3)   EQUAL                      ECS012
01167         WS-CITY-LF-CAN-AMT (7) - WS-CITY-LF-CAN-AMT (3).          ECS012
01168      COMPUTE WS-INTER-AH-ISS-CNT (3)   EQUAL                      ECS012
01169         WS-CITY-AH-ISS-CNT (7) - WS-CITY-AH-ISS-CNT (3).          ECS012
01170      COMPUTE WS-INTER-AH-CAN-CNT (3)   EQUAL                      ECS012
01171         WS-CITY-AH-CAN-CNT (7) - WS-CITY-AH-CAN-CNT (3).          ECS012
01172      COMPUTE WS-INTER-AH-ISS-AMT (3)   EQUAL                      ECS012
01173         WS-CITY-AH-ISS-AMT (7) - WS-CITY-AH-ISS-AMT (3).          ECS012
01174      COMPUTE WS-INTER-AH-CAN-AMT (3)   EQUAL                      ECS012
01175         WS-CITY-AH-CAN-AMT (7) - WS-CITY-AH-CAN-AMT (3).          ECS012
01176                                                                   ECS012
01177                                                                   ECS012
01178  0925-PRINT-CITY-TOTALS.                                          ECS012
01179                                                                   ECS012
01180      PERFORM 1010-CHECK-PRINT THRU 1019-CHECK-EXIT.               ECS012
01181      IF WS-PRINT-TOTAL-SW EQUAL 'Y'                               ECS012
01182          NEXT SENTENCE                                            ECS012
01183      ELSE                                                         ECS012
01184          GO TO 0929-EXIT.                                         ECS012
01185                                                                   ECS012
01186      MOVE 2                          TO  WS-PRINT-HEADING-SW.     ECS012
01187      MOVE WS-INTER-LF-ISS-CNT (1)    TO  DT1-LISS-CT.             ECS012
01188      MOVE WS-INTER-LF-ISS-AMT (1)    TO  DT1-LISS-AMT.            ECS012
01189      MOVE WS-INTER-LF-CAN-CNT (1)    TO  DT1-LCAN-CNT.            ECS012
01190      MOVE WS-INTER-LF-CAN-AMT (1)    TO  DT1-LCAN-AMT.            ECS012
01191      MOVE WS-INTER-AH-ISS-CNT (1)    TO  DT1-AHISS-CT.            ECS012
01192      MOVE WS-INTER-AH-ISS-AMT (1)    TO  DT1-AHISS-AMT.           ECS012
01193      MOVE WS-INTER-AH-CAN-CNT (1)    TO  DT1-AHCAN-CT.            ECS012
01194      MOVE WS-INTER-AH-CAN-AMT (1)    TO  DT1-AHCAN-AMT.           ECS012
01195                                                                   ECS012
01196      MOVE SPACES                     TO  LINE-LEFT.               ECS012
01197      MOVE ZERO                       TO  P-CTL.                   ECS012
01198      MOVE 'MTD'                      TO  DT1-TYPE.                ECS012
01199      PERFORM 0980-PRINT THRU 0989-PRINT-EXIT.                     ECS012
01200                                                                   ECS012
01201      MOVE WS-INTER-LF-ISS-CNT (2)    TO  DT1-LISS-CT.             ECS012
01202      MOVE WS-INTER-LF-ISS-AMT (2)    TO  DT1-LISS-AMT.            ECS012
01203      MOVE WS-INTER-LF-CAN-CNT (2)    TO  DT1-LCAN-CNT.            ECS012
01204      MOVE WS-INTER-LF-CAN-AMT (2)    TO  DT1-LCAN-AMT.            ECS012
01205      MOVE WS-INTER-AH-ISS-CNT (2)    TO  DT1-AHISS-CT.            ECS012
01206      MOVE WS-INTER-AH-ISS-AMT (2)    TO  DT1-AHISS-AMT.           ECS012
01207      MOVE WS-INTER-AH-CAN-CNT (2)    TO  DT1-AHCAN-CT.            ECS012
01208      MOVE WS-INTER-AH-CAN-AMT (2)    TO  DT1-AHCAN-AMT.           ECS012
01209                                                                   ECS012
01210      MOVE 'Y'                        TO  WS-DONT-BREAK.              CL**7
01211      MOVE SPACE                      TO  P-CTL.                      CL**7
01212      MOVE ' TOTALS FOR CITY'         TO  LINE-LEFT.               ECS012
01213      MOVE 'QTD'                      TO  DT1-TYPE.                   CL**7
01214      PERFORM 0980-PRINT THRU 0989-PRINT-EXIT.                        CL**7
01215                                                                   ECS012
01216      MOVE WS-INTER-LF-ISS-CNT (3)    TO  DT1-LISS-CT.             ECS012
01217      MOVE WS-INTER-LF-ISS-AMT (3)    TO  DT1-LISS-AMT.            ECS012
01218      MOVE WS-INTER-LF-CAN-CNT (3)    TO  DT1-LCAN-CNT.            ECS012
01219      MOVE WS-INTER-LF-CAN-AMT (3)    TO  DT1-LCAN-AMT.            ECS012
01220      MOVE WS-INTER-AH-ISS-CNT (3)    TO  DT1-AHISS-CT.            ECS012
01221      MOVE WS-INTER-AH-ISS-AMT (3)    TO  DT1-AHISS-AMT.           ECS012
01222      MOVE WS-INTER-AH-CAN-CNT (3)    TO  DT1-AHCAN-CT.            ECS012
01223      MOVE WS-INTER-AH-CAN-AMT (3)    TO  DT1-AHCAN-AMT.           ECS012
01224                                                                   ECS012
01225      MOVE SPACE                      TO  P-CTL.                      CL**7
01226      MOVE SPACES                     TO  LINE-LEFT.                  CL**7
01227      MOVE 'Y'                        TO  WS-DONT-BREAK.              CL**7
01228      MOVE 'YTD'                      TO  DT1-TYPE.                   CL**7
01229      PERFORM 0980-PRINT THRU 0989-PRINT-EXIT.                     ECS012
01230                                                                   ECS012
01231      MOVE +0                         TO  SUB.                     ECS012
01232      PERFORM 1130-INITIAL-INTERMED-TOTALS THRU 1139-II-EXIT.      ECS012
01233                                                                   ECS012
01234  0929-EXIT.                                                       ECS012
01235      EXIT.                                                        ECS012
01236                                                                   ECS012
01237      EJECT                                                        ECS012
01238  0930-COMPUTE-CNTY-TOTALS.                                        ECS012
01239                                                                   ECS012
01240      MOVE SPACES                 TO  DT1.                         ECS012
01241                                                                   ECS012
01242 ***   COMPUTE MTD                                                 ECS012
01243                                                                   ECS012
01244       COMPUTE WS-INTER-LF-ISS-CNT (1)  EQUAL                      ECS012
01245          WS-CNTY-LF-ISS-CNT (7) - WS-CNTY-LF-ISS-CNT (1).         ECS012
01246       COMPUTE WS-INTER-LF-CAN-CNT (1)  EQUAL                      ECS012
01247          WS-CNTY-LF-CAN-CNT (7) - WS-CNTY-LF-CAN-CNT (1).         ECS012
01248       COMPUTE WS-INTER-LF-ISS-AMT (1)  EQUAL                      ECS012
01249          WS-CNTY-LF-ISS-AMT (7) - WS-CNTY-LF-ISS-AMT (1).         ECS012
01250       COMPUTE WS-INTER-LF-CAN-AMT (1)  EQUAL                      ECS012
01251          WS-CNTY-LF-CAN-AMT (7) - WS-CNTY-LF-CAN-AMT (1).         ECS012
01252       COMPUTE WS-INTER-AH-ISS-CNT (1)  EQUAL                      ECS012
01253          WS-CNTY-AH-ISS-CNT (7) - WS-CNTY-AH-ISS-CNT (1).         ECS012
01254       COMPUTE WS-INTER-AH-CAN-CNT (1)  EQUAL                      ECS012
01255          WS-CNTY-AH-CAN-CNT (7) - WS-CNTY-AH-CAN-CNT (1).         ECS012
01256       COMPUTE WS-INTER-AH-ISS-AMT (1)  EQUAL                      ECS012
01257          WS-CNTY-AH-ISS-AMT (7) - WS-CNTY-AH-ISS-AMT (1).         ECS012
01258       COMPUTE WS-INTER-AH-CAN-AMT (1)  EQUAL                      ECS012
01259          WS-CNTY-AH-CAN-AMT (7) - WS-CNTY-AH-CAN-AMT (1).         ECS012
01260                                                                   ECS012
01261                                                                   ECS012
01262 ***   COMPUTE QTD                                                 ECS012
01263                                                                   ECS012
01264      COMPUTE WS-INTER-LF-ISS-CNT (2)   EQUAL                      ECS012
01265         WS-CNTY-LF-ISS-CNT (7) - WS-CNTY-LF-ISS-CNT (2).          ECS012
01266      COMPUTE WS-INTER-LF-CAN-CNT (2)   EQUAL                      ECS012
01267         WS-CNTY-LF-CAN-CNT (7) - WS-CNTY-LF-CAN-CNT (2).          ECS012
01268      COMPUTE WS-INTER-LF-ISS-AMT (2)   EQUAL                      ECS012
01269         WS-CNTY-LF-ISS-AMT (7) - WS-CNTY-LF-ISS-AMT (2).          ECS012
01270      COMPUTE WS-INTER-LF-CAN-AMT (2)   EQUAL                      ECS012
01271         WS-CNTY-LF-CAN-AMT (7) - WS-CNTY-LF-CAN-AMT (2).          ECS012
01272      COMPUTE WS-INTER-AH-ISS-CNT (2)   EQUAL                      ECS012
01273         WS-CNTY-AH-ISS-CNT (7) - WS-CNTY-AH-ISS-CNT (2).          ECS012
01274      COMPUTE WS-INTER-AH-CAN-CNT (2)   EQUAL                      ECS012
01275         WS-CNTY-AH-CAN-CNT (7) - WS-CNTY-AH-CAN-CNT (2).          ECS012
01276      COMPUTE WS-INTER-AH-ISS-AMT (2)   EQUAL                      ECS012
01277         WS-CNTY-AH-ISS-AMT (7) - WS-CNTY-AH-ISS-AMT (2).          ECS012
01278      COMPUTE WS-INTER-AH-CAN-AMT (2)   EQUAL                      ECS012
01279         WS-CNTY-AH-CAN-AMT (7) - WS-CNTY-AH-CAN-AMT (2).          ECS012
01280                                                                   ECS012
01281                                                                   ECS012
01282 ***   COMPUTE YTD                                                 ECS012
01283                                                                   ECS012
01284      COMPUTE WS-INTER-LF-ISS-CNT (3)   EQUAL                      ECS012
01285         WS-CNTY-LF-ISS-CNT (7) - WS-CNTY-LF-ISS-CNT (3).          ECS012
01286      COMPUTE WS-INTER-LF-CAN-CNT (3)   EQUAL                      ECS012
01287         WS-CNTY-LF-CAN-CNT (7) - WS-CNTY-LF-CAN-CNT (3).          ECS012
01288      COMPUTE WS-INTER-LF-ISS-AMT (3)   EQUAL                      ECS012
01289         WS-CNTY-LF-ISS-AMT (7) - WS-CNTY-LF-ISS-AMT (3).          ECS012
01290      COMPUTE WS-INTER-LF-CAN-AMT (3)   EQUAL                      ECS012
01291         WS-CNTY-LF-CAN-AMT (7) - WS-CNTY-LF-CAN-AMT (3).          ECS012
01292      COMPUTE WS-INTER-AH-ISS-CNT (3)   EQUAL                      ECS012
01293         WS-CNTY-AH-ISS-CNT (7) - WS-CNTY-AH-ISS-CNT (3).          ECS012
01294      COMPUTE WS-INTER-AH-CAN-CNT (3)   EQUAL                      ECS012
01295         WS-CNTY-AH-CAN-CNT (7) - WS-CNTY-AH-CAN-CNT (3).          ECS012
01296      COMPUTE WS-INTER-AH-ISS-AMT (3)   EQUAL                      ECS012
01297         WS-CNTY-AH-ISS-AMT (7) - WS-CNTY-AH-ISS-AMT (3).          ECS012
01298      COMPUTE WS-INTER-AH-CAN-AMT (3)   EQUAL                      ECS012
01299         WS-CNTY-AH-CAN-AMT (7) - WS-CNTY-AH-CAN-AMT (3).          ECS012
01300                                                                   ECS012
01301                                                                   ECS012
01302  0935-PRINT-CNTY-TOTALS.                                          ECS012
01303                                                                   ECS012
01304      PERFORM 1010-CHECK-PRINT THRU 1019-CHECK-EXIT.               ECS012
01305      IF WS-PRINT-TOTAL-SW EQUAL 'Y'                               ECS012
01306          NEXT SENTENCE                                            ECS012
01307      ELSE                                                         ECS012
01308          GO TO 0939-CNTY-EXIT.                                    ECS012
01309                                                                   ECS012
01310      MOVE 2                          TO  WS-PRINT-HEADING-SW.     ECS012
01311      MOVE WS-INTER-LF-ISS-CNT (1)    TO  DT1-LISS-CT.             ECS012
01312      MOVE WS-INTER-LF-ISS-AMT (1)    TO  DT1-LISS-AMT.            ECS012
01313      MOVE WS-INTER-LF-CAN-CNT (1)    TO  DT1-LCAN-CNT.            ECS012
01314      MOVE WS-INTER-LF-CAN-AMT (1)    TO  DT1-LCAN-AMT.            ECS012
01315      MOVE WS-INTER-AH-ISS-CNT (1)    TO  DT1-AHISS-CT.            ECS012
01316      MOVE WS-INTER-AH-ISS-AMT (1)    TO  DT1-AHISS-AMT.           ECS012
01317      MOVE WS-INTER-AH-CAN-CNT (1)    TO  DT1-AHCAN-CT.            ECS012
01318      MOVE WS-INTER-AH-CAN-AMT (1)    TO  DT1-AHCAN-AMT.           ECS012
01319                                                                   ECS012
01320      MOVE PREV-COUNTY                TO  DT1-CITY-CNTY.           ECS012
01321      MOVE SPACES                     TO  DT1-ACCT.                ECS012
01322      MOVE ZERO                       TO  P-CTL.                   ECS012
01323      MOVE 'MTD'                      TO  DT1-TYPE.                ECS012
01324      PERFORM 0980-PRINT THRU 0989-PRINT-EXIT.                     ECS012
01325                                                                   ECS012
01326      MOVE WS-INTER-LF-ISS-CNT (2)    TO  DT1-LISS-CT.             ECS012
01327      MOVE WS-INTER-LF-ISS-AMT (2)    TO  DT1-LISS-AMT.            ECS012
01328      MOVE WS-INTER-LF-CAN-CNT (2)    TO  DT1-LCAN-CNT.            ECS012
01329      MOVE WS-INTER-LF-CAN-AMT (2)    TO  DT1-LCAN-AMT.            ECS012
01330      MOVE WS-INTER-AH-ISS-CNT (2)    TO  DT1-AHISS-CT.            ECS012
01331      MOVE WS-INTER-AH-ISS-AMT (2)    TO  DT1-AHISS-AMT.           ECS012
01332      MOVE WS-INTER-AH-CAN-CNT (2)    TO  DT1-AHCAN-CT.            ECS012
01333      MOVE WS-INTER-AH-CAN-AMT (2)    TO  DT1-AHCAN-AMT.           ECS012
01334                                                                   ECS012
01335      MOVE 'Y'                        TO  WS-DONT-BREAK.              CL**7
01336      MOVE SPACE                      TO  P-CTL.                      CL**7
01337      MOVE ' TOTALS FOR COUNTY'       TO  LINE-LEFT.               ECS012
01338      MOVE 'QTD'                      TO  DT1-TYPE.                   CL**7
01339      PERFORM 0980-PRINT THRU 0989-PRINT-EXIT.                        CL**7
01340                                                                   ECS012
01341      MOVE WS-INTER-LF-ISS-CNT (3)    TO  DT1-LISS-CT.             ECS012
01342      MOVE WS-INTER-LF-ISS-AMT (3)    TO  DT1-LISS-AMT.            ECS012
01343      MOVE WS-INTER-LF-CAN-CNT (3)    TO  DT1-LCAN-CNT.            ECS012
01344      MOVE WS-INTER-LF-CAN-AMT (3)    TO  DT1-LCAN-AMT.            ECS012
01345      MOVE WS-INTER-AH-ISS-CNT (3)    TO  DT1-AHISS-CT.            ECS012
01346      MOVE WS-INTER-AH-ISS-AMT (3)    TO  DT1-AHISS-AMT.           ECS012
01347      MOVE WS-INTER-AH-CAN-CNT (3)    TO  DT1-AHCAN-CT.            ECS012
01348      MOVE WS-INTER-AH-CAN-AMT (3)    TO  DT1-AHCAN-AMT.           ECS012
01349                                                                   ECS012
01350      MOVE 'Y'                        TO  WS-DONT-BREAK.              CL**7
01351      MOVE  SPACE                     TO  P-CTL.                      CL**7
01352      MOVE SPACES                     TO  LINE-LEFT.                  CL**7
01353      MOVE 'YTD'                      TO  DT1-TYPE.                   CL**7
01354      PERFORM 0980-PRINT THRU 0989-PRINT-EXIT.                     ECS012
01355                                                                   ECS012
01356      MOVE +0                         TO  SUB.                     ECS012
01357      PERFORM 1130-INITIAL-INTERMED-TOTALS THRU 1139-II-EXIT.      ECS012
01358                                                                   ECS012
01359  0939-CNTY-EXIT.                                                  ECS012
01360      EXIT.                                                        ECS012
01361                                                                   ECS012
01362      EJECT                                                        ECS012
01363  0940-COMPUTE-STATE-TOTALS.                                       ECS012
01364                                                                   ECS012
01365      MOVE SPACES                 TO  DT1.                         ECS012
01366                                                                   ECS012
01367 ***   COMPUTE MTD                                                 ECS012
01368                                                                   ECS012
01369       COMPUTE WS-INTER-LF-ISS-CNT (1)  EQUAL                      ECS012
01370          WS-ST-LF-ISS-CNT (7) - WS-ST-LF-ISS-CNT (1).             ECS012
01371       COMPUTE WS-INTER-LF-CAN-CNT (1)  EQUAL                      ECS012
01372          WS-ST-LF-CAN-CNT (7) - WS-ST-LF-CAN-CNT (1).             ECS012
01373       COMPUTE WS-INTER-LF-ISS-AMT (1)  EQUAL                      ECS012
01374          WS-ST-LF-ISS-AMT (7) - WS-ST-LF-ISS-AMT (1).             ECS012
01375       COMPUTE WS-INTER-LF-CAN-AMT (1)  EQUAL                      ECS012
01376          WS-ST-LF-CAN-AMT (7) - WS-ST-LF-CAN-AMT (1).             ECS012
01377       COMPUTE WS-INTER-AH-ISS-CNT (1)  EQUAL                      ECS012
01378          WS-ST-AH-ISS-CNT (7) - WS-ST-AH-ISS-CNT (1).             ECS012
01379       COMPUTE WS-INTER-AH-CAN-CNT (1)  EQUAL                      ECS012
01380          WS-ST-AH-CAN-CNT (7) - WS-ST-AH-CAN-CNT (1).             ECS012
01381       COMPUTE WS-INTER-AH-ISS-AMT (1)  EQUAL                      ECS012
01382          WS-ST-AH-ISS-AMT (7) - WS-ST-AH-ISS-AMT (1).             ECS012
01383       COMPUTE WS-INTER-AH-CAN-AMT (1)  EQUAL                      ECS012
01384          WS-ST-AH-CAN-AMT (7) - WS-ST-AH-CAN-AMT (1).             ECS012
01385                                                                   ECS012
01386 ***   COMPUTE QTD                                                 ECS012
01387                                                                   ECS012
01388      COMPUTE WS-INTER-LF-ISS-CNT (2)   EQUAL                      ECS012
01389         WS-ST-LF-ISS-CNT (7) - WS-ST-LF-ISS-CNT (2).              ECS012
01390      COMPUTE WS-INTER-LF-CAN-CNT (2)   EQUAL                      ECS012
01391         WS-ST-LF-CAN-CNT (7) - WS-ST-LF-CAN-CNT (2).              ECS012
01392      COMPUTE WS-INTER-LF-ISS-AMT (2)   EQUAL                      ECS012
01393         WS-ST-LF-ISS-AMT (7) - WS-ST-LF-ISS-AMT (2).              ECS012
01394      COMPUTE WS-INTER-LF-CAN-AMT (2)   EQUAL                      ECS012
01395         WS-ST-LF-CAN-AMT (7) - WS-ST-LF-CAN-AMT (2).              ECS012
01396      COMPUTE WS-INTER-AH-ISS-CNT (2)   EQUAL                      ECS012
01397         WS-ST-AH-ISS-CNT (7) - WS-ST-AH-ISS-CNT (2).              ECS012
01398      COMPUTE WS-INTER-AH-CAN-CNT (2)   EQUAL                      ECS012
01399         WS-ST-AH-CAN-CNT (7) - WS-ST-AH-CAN-CNT (2).              ECS012
01400      COMPUTE WS-INTER-AH-ISS-AMT (2)   EQUAL                      ECS012
01401         WS-ST-AH-ISS-AMT (7) - WS-ST-AH-ISS-AMT (2).              ECS012
01402      COMPUTE WS-INTER-AH-CAN-AMT (2)   EQUAL                      ECS012
01403         WS-ST-AH-CAN-AMT (7) - WS-ST-AH-CAN-AMT (2).              ECS012
01404                                                                   ECS012
01405                                                                   ECS012
01406 ***   COMPUTE YTD                                                 ECS012
01407                                                                   ECS012
01408      COMPUTE WS-INTER-LF-ISS-CNT (3)   EQUAL                      ECS012
01409         WS-ST-LF-ISS-CNT (7) - WS-ST-LF-ISS-CNT (3).              ECS012
01410      COMPUTE WS-INTER-LF-CAN-CNT (3)   EQUAL                      ECS012
01411         WS-ST-LF-CAN-CNT (7) - WS-ST-LF-CAN-CNT (3).              ECS012
01412      COMPUTE WS-INTER-LF-ISS-AMT (3)   EQUAL                      ECS012
01413         WS-ST-LF-ISS-AMT (7) - WS-ST-LF-ISS-AMT (3).              ECS012
01414      COMPUTE WS-INTER-LF-CAN-AMT (3)   EQUAL                      ECS012
01415         WS-ST-LF-CAN-AMT (7) - WS-ST-LF-CAN-AMT (3).              ECS012
01416      COMPUTE WS-INTER-AH-ISS-CNT (3)   EQUAL                      ECS012
01417         WS-ST-AH-ISS-CNT (7) - WS-ST-AH-ISS-CNT (3).              ECS012
01418      COMPUTE WS-INTER-AH-CAN-CNT (3)   EQUAL                      ECS012
01419         WS-ST-AH-CAN-CNT (7) - WS-ST-AH-CAN-CNT (3).              ECS012
01420      COMPUTE WS-INTER-AH-ISS-AMT (3)   EQUAL                      ECS012
01421         WS-ST-AH-ISS-AMT (7) - WS-ST-AH-ISS-AMT (3).              ECS012
01422      COMPUTE WS-INTER-AH-CAN-AMT (3)   EQUAL                      ECS012
01423         WS-ST-AH-CAN-AMT (7) - WS-ST-AH-CAN-AMT (3).              ECS012
01424                                                                   ECS012
01425  0945-PRINT-STATE-TOTALS.                                         ECS012
01426                                                                   ECS012
01427      PERFORM 1010-CHECK-PRINT THRU 1019-CHECK-EXIT.               ECS012
01428      IF WS-PRINT-TOTAL-SW EQUAL 'Y'                               ECS012
01429          NEXT SENTENCE                                            ECS012
01430      ELSE                                                         ECS012
01431          GO TO 0949-STATE-EXIT.                                   ECS012
01432                                                                   ECS012
01433      MOVE 3                          TO  WS-PRINT-HEADING-SW.     ECS012
01434      MOVE WS-INTER-LF-ISS-CNT (1)    TO  DT1-LISS-CT.             ECS012
01435      MOVE WS-INTER-LF-ISS-AMT (1)    TO  DT1-LISS-AMT.            ECS012
01436      MOVE WS-INTER-LF-CAN-CNT (1)    TO  DT1-LCAN-CNT.            ECS012
01437      MOVE WS-INTER-LF-CAN-AMT (1)    TO  DT1-LCAN-AMT.            ECS012
01438      MOVE WS-INTER-AH-ISS-CNT (1)    TO  DT1-AHISS-CT.            ECS012
01439      MOVE WS-INTER-AH-ISS-AMT (1)    TO  DT1-AHISS-AMT.           ECS012
01440      MOVE WS-INTER-AH-CAN-CNT (1)    TO  DT1-AHCAN-CT.            ECS012
01441      MOVE WS-INTER-AH-CAN-AMT (1)    TO  DT1-AHCAN-AMT.           ECS012
01442                                                                   ECS012
01443      MOVE ZERO                       TO  P-CTL.                   ECS012
01444      MOVE 'MTD'                      TO  DT1-TYPE.                ECS012
01445      PERFORM 0980-PRINT THRU 0989-PRINT-EXIT.                     ECS012
01446                                                                   ECS012
01447      MOVE WS-INTER-LF-ISS-CNT (2)    TO  DT1-LISS-CT.             ECS012
01448      MOVE WS-INTER-LF-ISS-AMT (2)    TO  DT1-LISS-AMT.            ECS012
01449      MOVE WS-INTER-LF-CAN-CNT (2)    TO  DT1-LCAN-CNT.            ECS012
01450      MOVE WS-INTER-LF-CAN-AMT (2)    TO  DT1-LCAN-AMT.            ECS012
01451      MOVE WS-INTER-AH-ISS-CNT (2)    TO  DT1-AHISS-CT.            ECS012
01452      MOVE WS-INTER-AH-ISS-AMT (2)    TO  DT1-AHISS-AMT.           ECS012
01453      MOVE WS-INTER-AH-CAN-CNT (2)    TO  DT1-AHCAN-CT.            ECS012
01454      MOVE WS-INTER-AH-CAN-AMT (2)    TO  DT1-AHCAN-AMT.           ECS012
01455                                                                   ECS012
01456      MOVE 'Y'                        TO  WS-DONT-BREAK.              CL**7
01457      MOVE SPACE                      TO  P-CTL.                      CL**2
01458      MOVE ' TOTALS FOR STATE'        TO  LINE-LEFT.               ECS012
01459      MOVE 'QTD'                      TO  DT1-TYPE.                ECS012
01460      PERFORM 0980-PRINT THRU 0989-PRINT-EXIT.                     ECS012
01461                                                                   ECS012
01462      MOVE WS-INTER-LF-ISS-CNT (3)    TO  DT1-LISS-CT.             ECS012
01463      MOVE WS-INTER-LF-ISS-AMT (3)    TO  DT1-LISS-AMT.            ECS012
01464      MOVE WS-INTER-LF-CAN-CNT (3)    TO  DT1-LCAN-CNT.            ECS012
01465      MOVE WS-INTER-LF-CAN-AMT (3)    TO  DT1-LCAN-AMT.            ECS012
01466      MOVE WS-INTER-AH-ISS-CNT (3)    TO  DT1-AHISS-CT.            ECS012
01467      MOVE WS-INTER-AH-ISS-AMT (3)    TO  DT1-AHISS-AMT.           ECS012
01468      MOVE WS-INTER-AH-CAN-CNT (3)    TO  DT1-AHCAN-CT.            ECS012
01469      MOVE WS-INTER-AH-CAN-AMT (3)    TO  DT1-AHCAN-AMT.           ECS012
01470                                                                   ECS012
01471      MOVE SPACE                      TO  P-CTL.                      CL**2
01472      MOVE SPACES                     TO  LINE-LEFT.               ECS012
01473      MOVE 'Y'                        TO  WS-DONT-BREAK.              CL**7
01474      MOVE 'YTD'                      TO  DT1-TYPE.                ECS012
01475      PERFORM 0980-PRINT THRU 0989-PRINT-EXIT.                     ECS012
01476                                                                   ECS012
01477      MOVE +0                         TO  SUB.                     ECS012
01478      PERFORM 1130-INITIAL-INTERMED-TOTALS THRU 1139-II-EXIT.      ECS012
01479                                                                   ECS012
01480  0949-STATE-EXIT.                                                 ECS012
01481      EXIT.                                                        ECS012
01482                                                                   ECS012
01483      EJECT                                                        ECS012
01484  0950-COMPUTE-GRP-TOTALS.                                         ECS012
01485                                                                   ECS012
01486      MOVE SPACES                 TO  DT1.                         ECS012
01487                                                                   ECS012
01488 ***   COMPUTE MTD                                                 ECS012
01489                                                                   ECS012
01490       COMPUTE WS-INTER-LF-ISS-CNT (1)  EQUAL                      ECS012
01491          WS-GRP-LF-ISS-CNT (7) - WS-GRP-LF-ISS-CNT (1).           ECS012
01492       COMPUTE WS-INTER-LF-CAN-CNT (1)  EQUAL                      ECS012
01493          WS-GRP-LF-CAN-CNT (7) - WS-GRP-LF-CAN-CNT (1).           ECS012
01494       COMPUTE WS-INTER-LF-ISS-AMT (1)  EQUAL                      ECS012
01495          WS-GRP-LF-ISS-AMT (7) - WS-GRP-LF-ISS-AMT (1).           ECS012
01496       COMPUTE WS-INTER-LF-CAN-AMT (1)  EQUAL                      ECS012
01497          WS-GRP-LF-CAN-AMT (7) - WS-GRP-LF-CAN-AMT (1).           ECS012
01498       COMPUTE WS-INTER-AH-ISS-CNT (1)  EQUAL                      ECS012
01499          WS-GRP-AH-ISS-CNT (7) - WS-GRP-AH-ISS-CNT (1).           ECS012
01500       COMPUTE WS-INTER-AH-CAN-CNT (1)  EQUAL                      ECS012
01501          WS-GRP-AH-CAN-CNT (7) - WS-GRP-AH-CAN-CNT (1).           ECS012
01502       COMPUTE WS-INTER-AH-ISS-AMT (1)  EQUAL                      ECS012
01503          WS-GRP-AH-ISS-AMT (7) - WS-GRP-AH-ISS-AMT (1).           ECS012
01504       COMPUTE WS-INTER-AH-CAN-AMT (1)  EQUAL                      ECS012
01505          WS-GRP-AH-CAN-AMT (7) - WS-GRP-AH-CAN-AMT (1).           ECS012
01506                                                                   ECS012
01507 ***   COMPUTE QTD                                                 ECS012
01508                                                                   ECS012
01509      COMPUTE WS-INTER-LF-ISS-CNT (2)   EQUAL                      ECS012
01510         WS-GRP-LF-ISS-CNT (7) - WS-GRP-LF-ISS-CNT (2).            ECS012
01511      COMPUTE WS-INTER-LF-CAN-CNT (2)   EQUAL                      ECS012
01512         WS-GRP-LF-CAN-CNT (7) - WS-GRP-LF-CAN-CNT (2).            ECS012
01513      COMPUTE WS-INTER-LF-ISS-AMT (2)   EQUAL                      ECS012
01514         WS-GRP-LF-ISS-AMT (7) - WS-GRP-LF-ISS-AMT (2).            ECS012
01515      COMPUTE WS-INTER-LF-CAN-AMT (2)   EQUAL                      ECS012
01516         WS-GRP-LF-CAN-AMT (7) - WS-GRP-LF-CAN-AMT (2).            ECS012
01517      COMPUTE WS-INTER-AH-ISS-CNT (2)   EQUAL                      ECS012
01518         WS-GRP-AH-ISS-CNT (7) - WS-GRP-AH-ISS-CNT (2).            ECS012
01519      COMPUTE WS-INTER-AH-CAN-CNT (2)   EQUAL                      ECS012
01520         WS-GRP-AH-CAN-CNT (7) - WS-GRP-AH-CAN-CNT (2).            ECS012
01521      COMPUTE WS-INTER-AH-ISS-AMT (2)   EQUAL                      ECS012
01522         WS-GRP-AH-ISS-AMT (7) - WS-GRP-AH-ISS-AMT (2).            ECS012
01523      COMPUTE WS-INTER-AH-CAN-AMT (2)   EQUAL                      ECS012
01524         WS-GRP-AH-CAN-AMT (7) - WS-GRP-AH-CAN-AMT (2).            ECS012
01525                                                                   ECS012
01526                                                                   ECS012
01527 ***   COMPUTE YTD                                                 ECS012
01528                                                                   ECS012
01529      COMPUTE WS-INTER-LF-ISS-CNT (3)   EQUAL                      ECS012
01530         WS-GRP-LF-ISS-CNT (7) - WS-GRP-LF-ISS-CNT (3).            ECS012
01531      COMPUTE WS-INTER-LF-CAN-CNT (3)   EQUAL                      ECS012
01532         WS-GRP-LF-CAN-CNT (7) - WS-GRP-LF-CAN-CNT (3).            ECS012
01533      COMPUTE WS-INTER-LF-ISS-AMT (3)   EQUAL                      ECS012
01534         WS-GRP-LF-ISS-AMT (7) - WS-GRP-LF-ISS-AMT (3).            ECS012
01535      COMPUTE WS-INTER-LF-CAN-AMT (3)   EQUAL                      ECS012
01536         WS-GRP-LF-CAN-AMT (7) - WS-GRP-LF-CAN-AMT (3).            ECS012
01537      COMPUTE WS-INTER-AH-ISS-CNT (3)   EQUAL                      ECS012
01538         WS-GRP-AH-ISS-CNT (7) - WS-GRP-AH-ISS-CNT (3).            ECS012
01539      COMPUTE WS-INTER-AH-CAN-CNT (3)   EQUAL                      ECS012
01540         WS-GRP-AH-CAN-CNT (7) - WS-GRP-AH-CAN-CNT (3).            ECS012
01541      COMPUTE WS-INTER-AH-ISS-AMT (3)   EQUAL                      ECS012
01542         WS-GRP-AH-ISS-AMT (7) - WS-GRP-AH-ISS-AMT (3).            ECS012
01543      COMPUTE WS-INTER-AH-CAN-AMT (3)   EQUAL                      ECS012
01544         WS-GRP-AH-CAN-AMT (7) - WS-GRP-AH-CAN-AMT (3).            ECS012
01545                                                                   ECS012
01546  0955-PRINT-GRP-TOTALS.                                           ECS012
01547                                                                   ECS012
01548      PERFORM 1010-CHECK-PRINT THRU 1019-CHECK-EXIT.               ECS012
01549      IF WS-PRINT-TOTAL-SW EQUAL 'Y'                               ECS012
01550          NEXT SENTENCE                                            ECS012
01551      ELSE                                                         ECS012
01552          GO TO 0959-GRP-EXIT.                                     ECS012
01553                                                                   ECS012
01554      MOVE 4                          TO  WS-PRINT-HEADING-SW.     ECS012
01555      MOVE WS-INTER-LF-ISS-CNT (1)    TO  DT1-LISS-CT.             ECS012
01556      MOVE WS-INTER-LF-ISS-AMT (1)    TO  DT1-LISS-AMT.            ECS012
01557      MOVE WS-INTER-LF-CAN-CNT (1)    TO  DT1-LCAN-CNT.            ECS012
01558      MOVE WS-INTER-LF-CAN-AMT (1)    TO  DT1-LCAN-AMT.            ECS012
01559      MOVE WS-INTER-AH-ISS-CNT (1)    TO  DT1-AHISS-CT.            ECS012
01560      MOVE WS-INTER-AH-ISS-AMT (1)    TO  DT1-AHISS-AMT.           ECS012
01561      MOVE WS-INTER-AH-CAN-CNT (1)    TO  DT1-AHCAN-CT.            ECS012
01562      MOVE WS-INTER-AH-CAN-AMT (1)    TO  DT1-AHCAN-AMT.           ECS012
01563                                                                   ECS012
01564      MOVE ZERO                       TO  P-CTL.                   ECS012
01565      MOVE 'MTD'                      TO  DT1-TYPE.                ECS012
01566      PERFORM 0980-PRINT THRU 0989-PRINT-EXIT.                     ECS012
01567                                                                   ECS012
01568      MOVE WS-INTER-LF-ISS-CNT (2)    TO  DT1-LISS-CT.             ECS012
01569      MOVE WS-INTER-LF-ISS-AMT (2)    TO  DT1-LISS-AMT.            ECS012
01570      MOVE WS-INTER-LF-CAN-CNT (2)    TO  DT1-LCAN-CNT.            ECS012
01571      MOVE WS-INTER-LF-CAN-AMT (2)    TO  DT1-LCAN-AMT.            ECS012
01572      MOVE WS-INTER-AH-ISS-CNT (2)    TO  DT1-AHISS-CT.            ECS012
01573      MOVE WS-INTER-AH-ISS-AMT (2)    TO  DT1-AHISS-AMT.           ECS012
01574      MOVE WS-INTER-AH-CAN-CNT (2)    TO  DT1-AHCAN-CT.            ECS012
01575      MOVE WS-INTER-AH-CAN-AMT (2)    TO  DT1-AHCAN-AMT.           ECS012
01576                                                                   ECS012
01577      MOVE 'Y'                        TO  WS-DONT-BREAK.              CL**7
01578      MOVE SPACE                      TO  P-CTL.                      CL**2
01579      MOVE ' TOTALS FOR GROUP'        TO  LINE-LEFT.               ECS012
01580      MOVE 'QTD'                      TO  DT1-TYPE.                ECS012
01581      PERFORM 0980-PRINT THRU 0989-PRINT-EXIT.                     ECS012
01582                                                                   ECS012
01583      MOVE WS-INTER-LF-ISS-CNT (3)    TO  DT1-LISS-CT.             ECS012
01584      MOVE WS-INTER-LF-ISS-AMT (3)    TO  DT1-LISS-AMT.            ECS012
01585      MOVE WS-INTER-LF-CAN-CNT (3)    TO  DT1-LCAN-CNT.            ECS012
01586      MOVE WS-INTER-LF-CAN-AMT (3)    TO  DT1-LCAN-AMT.            ECS012
01587      MOVE WS-INTER-AH-ISS-CNT (3)    TO  DT1-AHISS-CT.            ECS012
01588      MOVE WS-INTER-AH-ISS-AMT (3)    TO  DT1-AHISS-AMT.           ECS012
01589      MOVE WS-INTER-AH-CAN-CNT (3)    TO  DT1-AHCAN-CT.            ECS012
01590      MOVE WS-INTER-AH-CAN-AMT (3)    TO  DT1-AHCAN-AMT.           ECS012
01591                                                                   ECS012
01592      MOVE 'Y'                        TO  WS-DONT-BREAK.              CL**7
01593      MOVE SPACE                      TO  P-CTL.                      CL**2
01594      MOVE SPACES                     TO  LINE-LEFT.               ECS012
01595      MOVE 'YTD'                      TO  DT1-TYPE.                ECS012
01596      PERFORM 0980-PRINT THRU 0989-PRINT-EXIT.                     ECS012
01597                                                                   ECS012
01598      MOVE +0                         TO  SUB.                     ECS012
01599      PERFORM 1130-INITIAL-INTERMED-TOTALS THRU 1139-II-EXIT.      ECS012
01600                                                                   ECS012
01601  0959-GRP-EXIT.                                                   ECS012
01602      EXIT.                                                        ECS012
01603      EJECT                                                        ECS012
01604  0960-COMPUTE-CARR-TOTALS.                                        ECS012
01605                                                                   ECS012
01606      MOVE SPACES                 TO  DT1.                         ECS012
01607                                                                   ECS012
01608 ***   COMPUTE MTD                                                 ECS012
01609                                                                   ECS012
01610       COMPUTE WS-INTER-LF-ISS-CNT (1)  EQUAL                      ECS012
01611          WS-CARR-LF-ISS-CNT (7) - WS-CARR-LF-ISS-CNT (1).         ECS012
01612       COMPUTE WS-INTER-LF-CAN-CNT (1)  EQUAL                      ECS012
01613          WS-CARR-LF-CAN-CNT (7) - WS-CARR-LF-CAN-CNT (1).         ECS012
01614       COMPUTE WS-INTER-LF-ISS-AMT (1)  EQUAL                      ECS012
01615          WS-CARR-LF-ISS-AMT (7) - WS-CARR-LF-ISS-AMT (1).         ECS012
01616       COMPUTE WS-INTER-LF-CAN-AMT (1)  EQUAL                      ECS012
01617          WS-CARR-LF-CAN-AMT (7) - WS-CARR-LF-CAN-AMT (1).         ECS012
01618       COMPUTE WS-INTER-AH-ISS-CNT (1)  EQUAL                      ECS012
01619          WS-CARR-AH-ISS-CNT (7) - WS-CARR-AH-ISS-CNT (1).         ECS012
01620       COMPUTE WS-INTER-AH-CAN-CNT (1)  EQUAL                      ECS012
01621          WS-CARR-AH-CAN-CNT (7) - WS-CARR-AH-CAN-CNT (1).         ECS012
01622       COMPUTE WS-INTER-AH-ISS-AMT (1)  EQUAL                      ECS012
01623          WS-CARR-AH-ISS-AMT (7) - WS-CARR-AH-ISS-AMT (1).         ECS012
01624       COMPUTE WS-INTER-AH-CAN-AMT (1)  EQUAL                      ECS012
01625          WS-CARR-AH-CAN-AMT (7) - WS-CARR-AH-CAN-AMT (1).         ECS012
01626                                                                   ECS012
01627 ***   COMPUTE QTD                                                 ECS012
01628                                                                   ECS012
01629      COMPUTE WS-INTER-LF-ISS-CNT (2)   EQUAL                      ECS012
01630         WS-CARR-LF-ISS-CNT (7) - WS-CARR-LF-ISS-CNT (2).          ECS012
01631      COMPUTE WS-INTER-LF-CAN-CNT (2)   EQUAL                      ECS012
01632         WS-CARR-LF-CAN-CNT (7) - WS-CARR-LF-CAN-CNT (2).          ECS012
01633      COMPUTE WS-INTER-LF-ISS-AMT (2)   EQUAL                      ECS012
01634         WS-CARR-LF-ISS-AMT (7) - WS-CARR-LF-ISS-AMT (2).          ECS012
01635      COMPUTE WS-INTER-LF-CAN-AMT (2)   EQUAL                      ECS012
01636         WS-CARR-LF-CAN-AMT (7) - WS-CARR-LF-CAN-AMT (2).          ECS012
01637      COMPUTE WS-INTER-AH-ISS-CNT (2)   EQUAL                      ECS012
01638         WS-CARR-AH-ISS-CNT (7) - WS-CARR-AH-ISS-CNT (2).          ECS012
01639      COMPUTE WS-INTER-AH-CAN-CNT (2)   EQUAL                      ECS012
01640         WS-CARR-AH-CAN-CNT (7) - WS-CARR-AH-CAN-CNT (2).          ECS012
01641      COMPUTE WS-INTER-AH-ISS-AMT (2)   EQUAL                      ECS012
01642         WS-CARR-AH-ISS-AMT (7) - WS-CARR-AH-ISS-AMT (2).          ECS012
01643      COMPUTE WS-INTER-AH-CAN-AMT (2)   EQUAL                      ECS012
01644         WS-CARR-AH-CAN-AMT (7) - WS-CARR-AH-CAN-AMT (2).          ECS012
01645                                                                   ECS012
01646                                                                   ECS012
01647 ***   COMPUTE YTD                                                 ECS012
01648                                                                   ECS012
01649      COMPUTE WS-INTER-LF-ISS-CNT (3)   EQUAL                      ECS012
01650         WS-CARR-LF-ISS-CNT (7) - WS-CARR-LF-ISS-CNT (3).          ECS012
01651      COMPUTE WS-INTER-LF-CAN-CNT (3)   EQUAL                      ECS012
01652         WS-CARR-LF-CAN-CNT (7) - WS-CARR-LF-CAN-CNT (3).          ECS012
01653      COMPUTE WS-INTER-LF-ISS-AMT (3)   EQUAL                      ECS012
01654         WS-CARR-LF-ISS-AMT (7) - WS-CARR-LF-ISS-AMT (3).          ECS012
01655      COMPUTE WS-INTER-LF-CAN-AMT (3)   EQUAL                      ECS012
01656         WS-CARR-LF-CAN-AMT (7) - WS-CARR-LF-CAN-AMT (3).          ECS012
01657      COMPUTE WS-INTER-AH-ISS-CNT (3)   EQUAL                      ECS012
01658         WS-CARR-AH-ISS-CNT (7) - WS-CARR-AH-ISS-CNT (3).          ECS012
01659      COMPUTE WS-INTER-AH-CAN-CNT (3)   EQUAL                      ECS012
01660         WS-CARR-AH-CAN-CNT (7) - WS-CARR-AH-CAN-CNT (3).          ECS012
01661      COMPUTE WS-INTER-AH-ISS-AMT (3)   EQUAL                      ECS012
01662         WS-CARR-AH-ISS-AMT (7) - WS-CARR-AH-ISS-AMT (3).          ECS012
01663      COMPUTE WS-INTER-AH-CAN-AMT (3)   EQUAL                      ECS012
01664         WS-CARR-AH-CAN-AMT (7) - WS-CARR-AH-CAN-AMT (3).          ECS012
01665                                                                   ECS012
01666  0965-PRINT-CARR-TOTALS.                                          ECS012
01667                                                                   ECS012
01668      PERFORM 1010-CHECK-PRINT THRU 1019-CHECK-EXIT.               ECS012
01669      IF WS-PRINT-TOTAL-SW EQUAL 'Y'                               ECS012
01670          NEXT SENTENCE                                            ECS012
01671      ELSE                                                         ECS012
01672          GO TO 0969-CARR-EXIT.                                    ECS012
01673                                                                   ECS012
01674      MOVE 5                          TO  WS-PRINT-HEADING-SW.     ECS012
01675      MOVE WS-INTER-LF-ISS-CNT (1)    TO  DT1-LISS-CT.             ECS012
01676      MOVE WS-INTER-LF-ISS-AMT (1)    TO  DT1-LISS-AMT.            ECS012
01677      MOVE WS-INTER-LF-CAN-CNT (1)    TO  DT1-LCAN-CNT.            ECS012
01678      MOVE WS-INTER-LF-CAN-AMT (1)    TO  DT1-LCAN-AMT.            ECS012
01679      MOVE WS-INTER-AH-ISS-CNT (1)    TO  DT1-AHISS-CT.            ECS012
01680      MOVE WS-INTER-AH-ISS-AMT (1)    TO  DT1-AHISS-AMT.           ECS012
01681      MOVE WS-INTER-AH-CAN-CNT (1)    TO  DT1-AHCAN-CT.            ECS012
01682      MOVE WS-INTER-AH-CAN-AMT (1)    TO  DT1-AHCAN-AMT.           ECS012
01683                                                                   ECS012
01684      MOVE ZERO                       TO  P-CTL.                   ECS012
01685      MOVE 'MTD'                      TO  DT1-TYPE.                ECS012
01686      PERFORM 0980-PRINT THRU 0989-PRINT-EXIT.                     ECS012
01687                                                                   ECS012
01688      MOVE WS-INTER-LF-ISS-CNT (2)    TO  DT1-LISS-CT.             ECS012
01689      MOVE WS-INTER-LF-ISS-AMT (2)    TO  DT1-LISS-AMT.            ECS012
01690      MOVE WS-INTER-LF-CAN-CNT (2)    TO  DT1-LCAN-CNT.            ECS012
01691      MOVE WS-INTER-LF-CAN-AMT (2)    TO  DT1-LCAN-AMT.            ECS012
01692      MOVE WS-INTER-AH-ISS-CNT (2)    TO  DT1-AHISS-CT.            ECS012
01693      MOVE WS-INTER-AH-ISS-AMT (2)    TO  DT1-AHISS-AMT.           ECS012
01694      MOVE WS-INTER-AH-CAN-CNT (2)    TO  DT1-AHCAN-CT.            ECS012
01695      MOVE WS-INTER-AH-CAN-AMT (2)    TO  DT1-AHCAN-AMT.           ECS012
01696                                                                   ECS012
01697      MOVE SPACE                      TO  P-CTL.                      CL**2
01698      MOVE 'Y'                        TO  WS-DONT-BREAK.              CL**7
01699      MOVE ' TOTALS FOR CARRIER'      TO  LINE-LEFT.               ECS012
01700      MOVE 'QTD'                      TO  DT1-TYPE.                ECS012
01701      PERFORM 0980-PRINT THRU 0989-PRINT-EXIT.                     ECS012
01702                                                                   ECS012
01703      MOVE WS-INTER-LF-ISS-CNT (3)    TO  DT1-LISS-CT.             ECS012
01704      MOVE WS-INTER-LF-ISS-AMT (3)    TO  DT1-LISS-AMT.            ECS012
01705      MOVE WS-INTER-LF-CAN-CNT (3)    TO  DT1-LCAN-CNT.            ECS012
01706      MOVE WS-INTER-LF-CAN-AMT (3)    TO  DT1-LCAN-AMT.            ECS012
01707      MOVE WS-INTER-AH-ISS-CNT (3)    TO  DT1-AHISS-CT.            ECS012
01708      MOVE WS-INTER-AH-ISS-AMT (3)    TO  DT1-AHISS-AMT.           ECS012
01709      MOVE WS-INTER-AH-CAN-CNT (3)    TO  DT1-AHCAN-CT.            ECS012
01710      MOVE WS-INTER-AH-CAN-AMT (3)    TO  DT1-AHCAN-AMT.           ECS012
01711                                                                   ECS012
01712      MOVE SPACE                      TO  P-CTL.                      CL**2
01713      MOVE 'Y'                        TO  WS-DONT-BREAK.              CL**7
01714      MOVE SPACES                     TO  LINE-LEFT.               ECS012
01715      MOVE 'YTD'                      TO  DT1-TYPE.                ECS012
01716      PERFORM 0980-PRINT THRU 0989-PRINT-EXIT.                     ECS012
01717                                                                   ECS012
01718      MOVE +0                         TO  SUB.                     ECS012
01719      PERFORM 1130-INITIAL-INTERMED-TOTALS THRU 1139-II-EXIT.      ECS012
01720                                                                   ECS012
01721  0969-CARR-EXIT.                                                  ECS012
01722      EXIT.                                                        ECS012
01723                                                                   ECS012
01724      EJECT                                                        ECS012
01725  0970-COMPUTE-TOTAL-TOTALS.                                       ECS012
01726                                                                   ECS012
01727      MOVE SPACES                 TO  DT1.                         ECS012
01728                                                                   ECS012
01729 ***   COMPUTE MTD                                                 ECS012
01730                                                                   ECS012
01731       COMPUTE WS-INTER-LF-ISS-CNT (1)  EQUAL                      ECS012
01732          WS-TOTAL-LF-ISS-CNT (7) - WS-TOTAL-LF-ISS-CNT (1).       ECS012
01733       COMPUTE WS-INTER-LF-CAN-CNT (1)  EQUAL                      ECS012
01734          WS-TOTAL-LF-CAN-CNT (7) - WS-TOTAL-LF-CAN-CNT (1).       ECS012
01735       COMPUTE WS-INTER-LF-ISS-AMT (1)  EQUAL                      ECS012
01736          WS-TOTAL-LF-ISS-AMT (7) - WS-TOTAL-LF-ISS-AMT (1).       ECS012
01737       COMPUTE WS-INTER-LF-CAN-AMT (1)  EQUAL                      ECS012
01738          WS-TOTAL-LF-CAN-AMT (7) - WS-TOTAL-LF-CAN-AMT (1).       ECS012
01739       COMPUTE WS-INTER-AH-ISS-CNT (1)  EQUAL                      ECS012
01740          WS-TOTAL-AH-ISS-CNT (7) - WS-TOTAL-AH-ISS-CNT (1).       ECS012
01741       COMPUTE WS-INTER-AH-CAN-CNT (1)  EQUAL                      ECS012
01742          WS-TOTAL-AH-CAN-CNT (7) - WS-TOTAL-AH-CAN-CNT (1).       ECS012
01743       COMPUTE WS-INTER-AH-ISS-AMT (1)  EQUAL                      ECS012
01744          WS-TOTAL-AH-ISS-AMT (7) - WS-TOTAL-AH-ISS-AMT (1).       ECS012
01745       COMPUTE WS-INTER-AH-CAN-AMT (1)  EQUAL                      ECS012
01746          WS-TOTAL-AH-CAN-AMT (7) - WS-TOTAL-AH-CAN-AMT (1).       ECS012
01747                                                                   ECS012
01748 ***   COMPUTE QTD                                                 ECS012
01749                                                                   ECS012
01750      COMPUTE WS-INTER-LF-ISS-CNT (2)   EQUAL                      ECS012
01751         WS-TOTAL-LF-ISS-CNT (7) - WS-TOTAL-LF-ISS-CNT (2).        ECS012
01752      COMPUTE WS-INTER-LF-CAN-CNT (2)   EQUAL                      ECS012
01753         WS-TOTAL-LF-CAN-CNT (7) - WS-TOTAL-LF-CAN-CNT (2).        ECS012
01754      COMPUTE WS-INTER-LF-ISS-AMT (2)   EQUAL                      ECS012
01755         WS-TOTAL-LF-ISS-AMT (7) - WS-TOTAL-LF-ISS-AMT (2).        ECS012
01756      COMPUTE WS-INTER-LF-CAN-AMT (2)   EQUAL                      ECS012
01757         WS-TOTAL-LF-CAN-AMT (7) - WS-TOTAL-LF-CAN-AMT (2).        ECS012
01758      COMPUTE WS-INTER-AH-ISS-CNT (2)   EQUAL                      ECS012
01759         WS-TOTAL-AH-ISS-CNT (7) - WS-TOTAL-AH-ISS-CNT (2).        ECS012
01760      COMPUTE WS-INTER-AH-CAN-CNT (2)   EQUAL                      ECS012
01761         WS-TOTAL-AH-CAN-CNT (7) - WS-TOTAL-AH-CAN-CNT (2).        ECS012
01762      COMPUTE WS-INTER-AH-ISS-AMT (2)   EQUAL                      ECS012
01763         WS-TOTAL-AH-ISS-AMT (7) - WS-TOTAL-AH-ISS-AMT (2).        ECS012
01764      COMPUTE WS-INTER-AH-CAN-AMT (2)   EQUAL                      ECS012
01765         WS-TOTAL-AH-CAN-AMT (7) - WS-TOTAL-AH-CAN-AMT (2).        ECS012
01766                                                                   ECS012
01767                                                                   ECS012
01768 ***   COMPUTE YTD                                                 ECS012
01769                                                                   ECS012
01770      COMPUTE WS-INTER-LF-ISS-CNT (3)   EQUAL                      ECS012
01771         WS-TOTAL-LF-ISS-CNT (7) - WS-TOTAL-LF-ISS-CNT (3).        ECS012
01772      COMPUTE WS-INTER-LF-CAN-CNT (3)   EQUAL                      ECS012
01773         WS-TOTAL-LF-CAN-CNT (7) - WS-TOTAL-LF-CAN-CNT (3).        ECS012
01774      COMPUTE WS-INTER-LF-ISS-AMT (3)   EQUAL                      ECS012
01775         WS-TOTAL-LF-ISS-AMT (7) - WS-TOTAL-LF-ISS-AMT (3).        ECS012
01776      COMPUTE WS-INTER-LF-CAN-AMT (3)   EQUAL                      ECS012
01777         WS-TOTAL-LF-CAN-AMT (7) - WS-TOTAL-LF-CAN-AMT (3).        ECS012
01778      COMPUTE WS-INTER-AH-ISS-CNT (3)   EQUAL                      ECS012
01779         WS-TOTAL-AH-ISS-CNT (7) - WS-TOTAL-AH-ISS-CNT (3).        ECS012
01780      COMPUTE WS-INTER-AH-CAN-CNT (3)   EQUAL                      ECS012
01781         WS-TOTAL-AH-CAN-CNT (7) - WS-TOTAL-AH-CAN-CNT (3).        ECS012
01782      COMPUTE WS-INTER-AH-ISS-AMT (3)   EQUAL                      ECS012
01783         WS-TOTAL-AH-ISS-AMT (7) - WS-TOTAL-AH-ISS-AMT (3).        ECS012
01784      COMPUTE WS-INTER-AH-CAN-AMT (3)   EQUAL                      ECS012
01785         WS-TOTAL-AH-CAN-AMT (7) - WS-TOTAL-AH-CAN-AMT (3).        ECS012
01786                                                                   ECS012
01787  0975-PRINT-TOTAL-TOTALS.                                         ECS012
01788                                                                   ECS012
01789      PERFORM 1010-CHECK-PRINT THRU 1019-CHECK-EXIT.               ECS012
01790      IF WS-PRINT-TOTAL-SW EQUAL 'Y'                               ECS012
01791          NEXT SENTENCE                                            ECS012
01792      ELSE                                                         ECS012
01793          GO TO 0979-TOTAL-EXIT.                                   ECS012
01794                                                                   ECS012
01795      MOVE 6                          TO  WS-PRINT-HEADING-SW.     ECS012
01796      MOVE WS-INTER-LF-ISS-CNT (1)    TO  DT1-LISS-CT.             ECS012
01797      MOVE WS-INTER-LF-ISS-AMT (1)    TO  DT1-LISS-AMT.            ECS012
01798      MOVE WS-INTER-LF-CAN-CNT (1)    TO  DT1-LCAN-CNT.            ECS012
01799      MOVE WS-INTER-LF-CAN-AMT (1)    TO  DT1-LCAN-AMT.            ECS012
01800      MOVE WS-INTER-AH-ISS-CNT (1)    TO  DT1-AHISS-CT.            ECS012
01801      MOVE WS-INTER-AH-ISS-AMT (1)    TO  DT1-AHISS-AMT.           ECS012
01802      MOVE WS-INTER-AH-CAN-CNT (1)    TO  DT1-AHCAN-CT.            ECS012
01803      MOVE WS-INTER-AH-CAN-AMT (1)    TO  DT1-AHCAN-AMT.           ECS012
01804                                                                   ECS012
01805      MOVE ZERO                       TO  P-CTL.                   ECS012
01806      MOVE 'MTD'                      TO  DT1-TYPE.                ECS012
01807      PERFORM 0980-PRINT THRU 0989-PRINT-EXIT.                     ECS012
01808                                                                   ECS012
01809      MOVE WS-INTER-LF-ISS-CNT (2)    TO  DT1-LISS-CT.             ECS012
01810      MOVE WS-INTER-LF-ISS-AMT (2)    TO  DT1-LISS-AMT.            ECS012
01811      MOVE WS-INTER-LF-CAN-CNT (2)    TO  DT1-LCAN-CNT.            ECS012
01812      MOVE WS-INTER-LF-CAN-AMT (2)    TO  DT1-LCAN-AMT.            ECS012
01813      MOVE WS-INTER-AH-ISS-CNT (2)    TO  DT1-AHISS-CT.            ECS012
01814      MOVE WS-INTER-AH-ISS-AMT (2)    TO  DT1-AHISS-AMT.           ECS012
01815      MOVE WS-INTER-AH-CAN-CNT (2)    TO  DT1-AHCAN-CT.            ECS012
01816      MOVE WS-INTER-AH-CAN-AMT (2)    TO  DT1-AHCAN-AMT.           ECS012
01817                                                                   ECS012
01818      MOVE 'Y'                        TO  WS-DONT-BREAK.              CL**7
01819      MOVE SPACE                      TO  P-CTL.                      CL**2
01820      MOVE ' OVERALL TOTALS'          TO  LINE-LEFT.               ECS012
01821      MOVE 'QTD'                      TO  DT1-TYPE.                ECS012
01822      PERFORM 0980-PRINT THRU 0989-PRINT-EXIT.                     ECS012
01823                                                                   ECS012
01824      MOVE WS-INTER-LF-ISS-CNT (3)    TO  DT1-LISS-CT.             ECS012
01825      MOVE WS-INTER-LF-ISS-AMT (3)    TO  DT1-LISS-AMT.            ECS012
01826      MOVE WS-INTER-LF-CAN-CNT (3)    TO  DT1-LCAN-CNT.            ECS012
01827      MOVE WS-INTER-LF-CAN-AMT (3)    TO  DT1-LCAN-AMT.            ECS012
01828      MOVE WS-INTER-AH-ISS-CNT (3)    TO  DT1-AHISS-CT.            ECS012
01829      MOVE WS-INTER-AH-ISS-AMT (3)    TO  DT1-AHISS-AMT.           ECS012
01830      MOVE WS-INTER-AH-CAN-CNT (3)    TO  DT1-AHCAN-CT.            ECS012
01831      MOVE WS-INTER-AH-CAN-AMT (3)    TO  DT1-AHCAN-AMT.           ECS012
01832                                                                   ECS012
01833      MOVE 'Y'                        TO  WS-DONT-BREAK.              CL**7
01834      MOVE SPACE                      TO  P-CTL.                      CL**2
01835      MOVE SPACES                     TO  LINE-LEFT.               ECS012
01836      MOVE 'YTD'                      TO  DT1-TYPE.                ECS012
01837      PERFORM 0980-PRINT THRU 0989-PRINT-EXIT.                     ECS012
01838                                                                   ECS012
01839      MOVE +0                         TO  SUB.                     ECS012
01840      PERFORM 1130-INITIAL-INTERMED-TOTALS THRU 1139-II-EXIT.      ECS012
01841                                                                   ECS012
01842  0979-TOTAL-EXIT.                                                 ECS012
01843      EXIT.                                                        ECS012
01844                                                                   ECS012
01845      EJECT                                                        ECS012
01846  0980-PRINT.                                                      ECS012
01847                                                                   ECS012
01848      IF (LNCTR IS GREATER THAN 48) AND                            ECS012
01849         (WS-DONT-BREAK EQUAL ' ')                                 ECS012
01850          MOVE P-CTL              TO HLD-CTL                       ECS012
01851          PERFORM 0990-HD-RTN THRU 0999-HD-EXIT                    ECS012
01852          MOVE HLD-CTL            TO P-CTL.                        ECS012
01853                                                                   ECS012
01854      MOVE ' '                    TO  WS-DONT-BREAK.                  CL**7
01855      MOVE P-CTL                  TO  X.                           ECS012
01856      MOVE DT1                    TO  P-DATA.                      ECS012
01857      PERFORM 1000-PRINT-COPY THRU 1009-COPY-EXIT.                 ECS012
01858                                                                   ECS012
01859      IF X IS EQUAL TO ZERO                                        ECS012
01860          ADD 2                   TO  LNCTR                        ECS012
01861      ELSE                                                         ECS012
01862          ADD 1                   TO  LNCTR.                       ECS012
01863                                                                   ECS012
01864  0989-PRINT-EXIT.                                                 ECS012
01865      EXIT.                                                        ECS012
01866                                                                   ECS012
01867  0990-HD-RTN.                                                     ECS012
01868                                                                   ECS012
01869      ADD 1                       TO  PGCTR.                       ECS012
01870      MOVE PGCTR                  TO  HD-PAGE.                     ECS012
01871      MOVE HD1                    TO  P-DATA.                      ECS012
01872      MOVE '1'                    TO  P-CTL.                       ECS012
01873      PERFORM 1000-PRINT-COPY THRU 1009-COPY-EXIT.                 ECS012
01874      MOVE HD2                    TO  P-DATA.                      ECS012
01875      MOVE SPACE                  TO  P-CTL.                          CL**2
01876      PERFORM 1000-PRINT-COPY THRU 1009-COPY-EXIT.                 ECS012
01877      MOVE HD3                    TO  P-DATA.                      ECS012
01878      MOVE SPACE                  TO  P-CTL.                          CL**2
01879      PERFORM 1000-PRINT-COPY THRU 1009-COPY-EXIT.                 ECS012
01880                                                                   ECS012
01881      IF WS-PRINT-HEADING-SW IS GREATER THAN 5                     ECS012
01882          MOVE SPACES             TO  P-DATA                       ECS012
01883      ELSE                                                         ECS012
01884          MOVE HD4                TO  P-DATA.                      ECS012
01885                                                                      CL**7
01886      MOVE SPACE                  TO  P-CTL.                          CL**2
01887      PERFORM 1000-PRINT-COPY THRU 1009-COPY-EXIT.                 ECS012
01888                                                                   ECS012
01889      IF WS-PRINT-HEADING-SW IS GREATER THAN 4                     ECS012
01890          MOVE SPACES             TO  P-DATA                       ECS012
01891      ELSE                                                         ECS012
01892          MOVE HD5                TO  P-DATA.                      ECS012
01893                                                                      CL**7
01894      MOVE SPACE                  TO  P-CTL.                          CL**2
01895      PERFORM 1000-PRINT-COPY THRU 1009-COPY-EXIT.                 ECS012
01896                                                                   ECS012
01897      IF WS-PRINT-HEADING-SW IS GREATER THAN 3                     ECS012
01898          MOVE SPACES             TO  P-DATA                       ECS012
01899      ELSE                                                         ECS012
01900          MOVE HD6                TO  P-DATA.                      ECS012
01901                                                                      CL**7
01902      MOVE SPACE                  TO  P-CTL.                          CL**2
01903      PERFORM 1000-PRINT-COPY THRU 1009-COPY-EXIT.                 ECS012
01904                                                                   ECS012
01905      IF WS-PRINT-HEADING-SW IS GREATER THAN 2                     ECS012
01906          MOVE SPACES             TO  P-DATA                       ECS012
01907      ELSE                                                         ECS012
01908          MOVE HD7                TO  P-DATA.                      ECS012
01909                                                                      CL**7
01910      MOVE SPACE              TO  P-CTL.                              CL**2
01911      PERFORM 1000-PRINT-COPY THRU 1009-COPY-EXIT.                 ECS012
01912      MOVE HD8                    TO  P-DATA.                      ECS012
01913      MOVE SPACE                  TO  P-CTL.                          CL**2
01914      PERFORM 1000-PRINT-COPY THRU 1009-COPY-EXIT.                 ECS012
01915      MOVE HD9                    TO  P-DATA.                      ECS012
01916      MOVE SPACE                  TO  P-CTL.                          CL**2
01917      PERFORM 1000-PRINT-COPY THRU 1009-COPY-EXIT.                 ECS012
01918      MOVE HDA                    TO  P-DATA.                      ECS012
01919      MOVE SPACE                  TO  P-CTL.                          CL**2
01920      PERFORM 1000-PRINT-COPY THRU 1009-COPY-EXIT.                 ECS012
01921      MOVE ZERO                   TO  LNCTR.                       ECS012
01922                                                                   ECS012
01923  0999-HD-EXIT.                                                    ECS012
01924      EXIT.                                                        ECS012
01925                                                                   ECS012
01926  1000-PRINT-COPY.                                                 ECS012
01927      MOVE P-CTL                  TO  X.                           ECS012
01928                                  COPY ELCPRT2.                    ECS012
01929                                                                   ECS012
01930  1009-COPY-EXIT.                                                  ECS012
01931      EXIT.                                                        ECS012
01932                                                                   ECS012
01933      EJECT                                                        ECS012
01934  1010-CHECK-PRINT.                                                ECS012
01935                                                                   ECS012
01936      IF WS-INTER-LF-ISS-CNT (1) EQUAL +0 AND                      ECS012
01937         WS-INTER-LF-ISS-AMT (1) EQUAL +0 AND                      ECS012
01938         WS-INTER-LF-CAN-CNT (1) EQUAL +0 AND                      ECS012
01939         WS-INTER-LF-CAN-AMT (1) EQUAL +0 AND                      ECS012
01940         WS-INTER-AH-ISS-CNT (1) EQUAL +0 AND                      ECS012
01941         WS-INTER-AH-ISS-AMT (1) EQUAL +0 AND                      ECS012
01942         WS-INTER-AH-CAN-CNT (1) EQUAL +0 AND                      ECS012
01943         WS-INTER-AH-CAN-AMT (1) EQUAL +0 AND                      ECS012
01944         WS-INTER-LF-ISS-CNT (2) EQUAL +0 AND                      ECS012
01945         WS-INTER-LF-ISS-AMT (2) EQUAL +0 AND                      ECS012
01946         WS-INTER-LF-CAN-CNT (2) EQUAL +0 AND                      ECS012
01947         WS-INTER-LF-CAN-AMT (2) EQUAL +0 AND                      ECS012
01948         WS-INTER-AH-ISS-CNT (2) EQUAL +0 AND                      ECS012
01949         WS-INTER-AH-ISS-AMT (2) EQUAL +0 AND                      ECS012
01950         WS-INTER-AH-CAN-CNT (2) EQUAL +0 AND                      ECS012
01951         WS-INTER-AH-CAN-AMT (2) EQUAL +0 AND                      ECS012
01952         WS-INTER-LF-ISS-CNT (3) EQUAL +0 AND                      ECS012
01953         WS-INTER-LF-ISS-AMT (3) EQUAL +0 AND                      ECS012
01954         WS-INTER-LF-CAN-CNT (3) EQUAL +0 AND                      ECS012
01955         WS-INTER-LF-CAN-AMT (3) EQUAL +0 AND                      ECS012
01956         WS-INTER-AH-ISS-CNT (3) EQUAL +0 AND                      ECS012
01957         WS-INTER-AH-ISS-AMT (3) EQUAL +0 AND                      ECS012
01958         WS-INTER-AH-CAN-CNT (3) EQUAL +0 AND                      ECS012
01959         WS-INTER-AH-CAN-AMT (3) EQUAL +0                          ECS012
01960          MOVE 'N'                TO  WS-PRINT-TOTAL-SW            ECS012
01961      ELSE                                                         ECS012
01962          MOVE 'Y'                TO  WS-PRINT-TOTAL-SW.           ECS012
01963                                                                   ECS012
01964  1019-CHECK-EXIT.                                                 ECS012
01965      EXIT.                                                        ECS012
01966                                                                   ECS012
01967      EJECT                                                        ECS012
01968  1020-SET-CARRIER.                                                ECS012
01969      MOVE PREV-CARRIER           TO  H4-CARR-CD.                  ECS012
01970      PERFORM 1050-SRCH VARYING X3 FROM 1 BY 1                     ECS012
01971          UNTIL X3 EQUAL 76 OR PREV-CARRIER EQUAL CARRIER-SUB (X3).ECS012
01972                                                                   ECS012
01973      IF X3 EQUAL 26                                               ECS012
01974          MOVE SPACES             TO  H4-CARR-NM                   ECS012
01975      ELSE                                                         ECS012
01976          MOVE CARRIER-PIC (X3)   TO  H4-CARR-NM.                  ECS012
01977                                                                   ECS012
01978      MOVE 90                     TO  LNCTR.                       ECS012
01979                                                                   ECS012
01980  1029-SET-CARR-EXIT.                                              ECS012
01981      EXIT.                                                        ECS012
01982                                                                   ECS012
01983  1030-SET-GROUPING.                                               ECS012
01984      MOVE PREV-GROUPING          TO  H5-GRP-CD.                   ECS012
01985                                                                   ECS012
01986      MOVE 90                     TO  LNCTR.                       ECS012
01987                                                                   ECS012
01988  1039-SET-GRP-EXIT.                                               ECS012
01989      EXIT.                                                        ECS012
01990                                                                   ECS012
01991  1040-SET-STATE.                                                  ECS012
01992      MOVE PREV-STATE             TO  H6-ST-CD.                    ECS012
01993      PERFORM 1050-SRCH VARYING X3 FROM 1 BY 1                     ECS012
01994          UNTIL X3 EQUAL 76 OR PREV-STATE EQUAL STATE-SUB (X3).    ECS012
01995                                                                   ECS012
01996      IF X3 EQUAL 76                                               ECS012
01997          MOVE SPACES             TO  H6-ST-NM                     ECS012
01998      ELSE                                                         ECS012
01999          MOVE STATE-PIC (X3)     TO  H6-ST-NM.                    ECS012
02000                                                                   ECS012
02001      MOVE 90                     TO  LNCTR.                       ECS012
02002                                                                   ECS012
02003  1049-SET-STATE-EXIT.                                             ECS012
02004      EXIT.                                                        ECS012
02005                                                                   ECS012
02006  1050-SRCH.                                                       ECS012
02007      EXIT.                                                        ECS012
02008                                                                   ECS012
02009  EJECT                                                            ECS012
02010  1060-INITIAL-CARR-TOTALS.                                        ECS012
02011      ADD +1                      TO  SUB.                         ECS012
02012                                                                   ECS012
02013      IF SUB IS GREATER THAN +7                                    ECS012
02014          GO TO 1069-IC-EXIT.                                      ECS012
02015                                                                   ECS012
02016      MOVE +0                     TO                               ECS012
02017          WS-CARR-LF-ISS-CNT  (SUB)   WS-CARR-LF-ISS-AMT     (SUB) ECS012
02018          WS-CARR-LF-CAN-CNT  (SUB)   WS-CARR-LF-CAN-AMT     (SUB) ECS012
02019          WS-CARR-AH-ISS-CNT  (SUB)   WS-CARR-AH-ISS-AMT     (SUB) ECS012
02020          WS-CARR-AH-CAN-CNT  (SUB)   WS-CARR-AH-CAN-AMT     (SUB).ECS012
02021                                                                   ECS012
02022      GO TO 1060-INITIAL-CARR-TOTALS.                              ECS012
02023                                                                   ECS012
02024  1069-IC-EXIT.                                                    ECS012
02025      EXIT.                                                        ECS012
02026                                                                   ECS012
02027  1070-INITIAL-GRP-TOTALS.                                         ECS012
02028      ADD +1                      TO  SUB.                         ECS012
02029                                                                   ECS012
02030      IF SUB IS GREATER THAN +7                                    ECS012
02031          GO TO 1079-IG-EXIT.                                      ECS012
02032                                                                   ECS012
02033      MOVE +0                     TO                               ECS012
02034          WS-GRP-LF-ISS-CNT   (SUB)   WS-GRP-LF-ISS-AMT      (SUB) ECS012
02035          WS-GRP-LF-CAN-CNT   (SUB)   WS-GRP-LF-CAN-AMT      (SUB) ECS012
02036          WS-GRP-AH-ISS-CNT   (SUB)   WS-GRP-AH-ISS-AMT      (SUB) ECS012
02037          WS-GRP-AH-CAN-CNT   (SUB)   WS-GRP-AH-CAN-AMT      (SUB).ECS012
02038                                                                   ECS012
02039      GO TO 1070-INITIAL-GRP-TOTALS.                               ECS012
02040                                                                   ECS012
02041  1079-IG-EXIT.                                                    ECS012
02042      EXIT.                                                        ECS012
02043                                                                   ECS012
02044  1080-INITIAL-STATE-TOTALS.                                       ECS012
02045      ADD +1                      TO  SUB.                         ECS012
02046                                                                   ECS012
02047      IF SUB IS GREATER THAN +7                                    ECS012
02048          GO TO 1089-IS-EXIT.                                      ECS012
02049                                                                   ECS012
02050      MOVE +0                     TO                               ECS012
02051          WS-ST-LF-ISS-CNT    (SUB)   WS-ST-LF-ISS-AMT       (SUB) ECS012
02052          WS-ST-LF-CAN-CNT    (SUB)   WS-ST-LF-CAN-AMT       (SUB) ECS012
02053          WS-ST-AH-ISS-CNT    (SUB)   WS-ST-AH-ISS-AMT       (SUB) ECS012
02054          WS-ST-AH-CAN-CNT    (SUB)   WS-ST-AH-CAN-AMT       (SUB).ECS012
02055                                                                   ECS012
02056      GO TO 1080-INITIAL-STATE-TOTALS.                             ECS012
02057                                                                   ECS012
02058  1089-IS-EXIT.                                                    ECS012
02059      EXIT.                                                        ECS012
02060                                                                   ECS012
02061  1090-INITIAL-CITY-TOTALS.                                        ECS012
02062      ADD +1                      TO  SUB.                         ECS012
02063                                                                   ECS012
02064      IF SUB IS GREATER THAN +7                                    ECS012
02065          GO TO 1099-ICITY-EXIT.                                   ECS012
02066                                                                   ECS012
02067      MOVE +0                     TO                               ECS012
02068          WS-CITY-LF-ISS-CNT  (SUB)   WS-CITY-LF-ISS-AMT     (SUB) ECS012
02069          WS-CITY-LF-CAN-CNT  (SUB)   WS-CITY-LF-CAN-AMT     (SUB) ECS012
02070          WS-CITY-AH-ISS-CNT  (SUB)   WS-CITY-AH-ISS-AMT     (SUB) ECS012
02071          WS-CITY-AH-CAN-CNT  (SUB)   WS-CITY-AH-CAN-AMT     (SUB).ECS012
02072                                                                   ECS012
02073      GO TO 1090-INITIAL-CITY-TOTALS.                              ECS012
02074                                                                   ECS012
02075  1099-ICITY-EXIT.                                                 ECS012
02076      EXIT.                                                        ECS012
02077                                                                   ECS012
02078  1100-INITIAL-ACCT-TOTALS.                                        ECS012
02079      ADD +1                      TO  SUB.                         ECS012
02080                                                                   ECS012
02081      IF SUB IS GREATER THAN +7                                    ECS012
02082          GO TO 1109-IA-EXIT.                                      ECS012
02083                                                                   ECS012
02084      MOVE +0                     TO                               ECS012
02085          WS-ACCT-LF-ISS-CNT  (SUB)   WS-ACCT-LF-ISS-AMT     (SUB) ECS012
02086          WS-ACCT-LF-CAN-CNT  (SUB)   WS-ACCT-LF-CAN-AMT     (SUB) ECS012
02087          WS-ACCT-AH-ISS-CNT  (SUB)   WS-ACCT-AH-ISS-AMT     (SUB) ECS012
02088          WS-ACCT-AH-CAN-CNT  (SUB)   WS-ACCT-AH-CAN-AMT     (SUB).ECS012
02089                                                                   ECS012
02090      GO TO 1100-INITIAL-ACCT-TOTALS.                              ECS012
02091                                                                   ECS012
02092  1109-IA-EXIT.                                                    ECS012
02093      EXIT.                                                        ECS012
02094                                                                   ECS012
02095  1110-INITIAL-CNTY-TOTALS.                                        ECS012
02096      ADD +1                      TO  SUB.                         ECS012
02097                                                                   ECS012
02098      IF SUB IS GREATER THAN +7                                    ECS012
02099          GO TO 1119-ICNTY-EXIT.                                   ECS012
02100                                                                   ECS012
02101      MOVE +0                     TO                               ECS012
02102          WS-CNTY-LF-ISS-CNT  (SUB)   WS-CNTY-LF-ISS-AMT     (SUB) ECS012
02103          WS-CNTY-LF-CAN-CNT  (SUB)   WS-CNTY-LF-CAN-AMT     (SUB) ECS012
02104          WS-CNTY-AH-ISS-CNT  (SUB)   WS-CNTY-AH-ISS-AMT     (SUB) ECS012
02105          WS-CNTY-AH-CAN-CNT  (SUB)   WS-CNTY-AH-CAN-AMT     (SUB).ECS012
02106                                                                   ECS012
02107      GO TO 1110-INITIAL-CNTY-TOTALS.                              ECS012
02108                                                                   ECS012
02109  1119-ICNTY-EXIT.                                                 ECS012
02110      EXIT.                                                        ECS012
02111                                                                   ECS012
02112  1120-INITIAL-FINAL-TOTALS.                                       ECS012
02113      ADD +1                      TO  SUB.                         ECS012
02114                                                                   ECS012
02115      IF SUB IS GREATER THAN +7                                    ECS012
02116          GO TO 1129-IF-EXIT.                                      ECS012
02117                                                                   ECS012
02118      MOVE +0                     TO                               ECS012
02119          WS-TOTAL-LF-ISS-CNT (SUB)   WS-TOTAL-LF-ISS-AMT    (SUB) ECS012
02120          WS-TOTAL-LF-CAN-CNT (SUB)   WS-TOTAL-LF-CAN-AMT    (SUB) ECS012
02121          WS-TOTAL-AH-ISS-CNT (SUB)   WS-TOTAL-AH-ISS-AMT    (SUB) ECS012
02122          WS-TOTAL-AH-CAN-CNT (SUB)   WS-TOTAL-AH-CAN-AMT    (SUB).ECS012
02123                                                                   ECS012
02124      GO TO 1120-INITIAL-FINAL-TOTALS.                             ECS012
02125                                                                   ECS012
02126  1129-IF-EXIT.                                                    ECS012
02127      EXIT.                                                        ECS012
02128                                                                   ECS012
02129  1130-INITIAL-INTERMED-TOTALS.                                    ECS012
02130      ADD +1                      TO  SUB.                         ECS012
02131      IF SUB IS GREATER THAN +4                                    ECS012
02132          GO TO 1139-II-EXIT.                                      ECS012
02133                                                                   ECS012
02134      MOVE +0                     TO                               ECS012
02135          WS-INTER-LF-ISS-CNT (SUB)    WS-INTER-LF-ISS-AMT    (SUB)ECS012
02136          WS-INTER-LF-CAN-CNT (SUB)    WS-INTER-LF-CAN-AMT    (SUB)ECS012
02137          WS-INTER-AH-ISS-CNT (SUB)    WS-INTER-AH-ISS-AMT    (SUB)ECS012
02138          WS-INTER-AH-CAN-CNT (SUB)    WS-INTER-AH-CAN-AMT    (SUB)ECS012
02139                                                                   ECS012
02140      GO TO 1130-INITIAL-INTERMED-TOTALS.                          ECS012
02141                                                                   ECS012
02142  1139-II-EXIT.                                                    ECS012
02143      EXIT.                                                        ECS012
02144                                                                   ECS012
02145      EJECT                                                        ECS012
02146  9100-E-O-J.                                                      ECS012
02147                                                                   ECS012
02148  ABEND-PGM.                                                       ECS012
02149                              COPY ELCABEND SUPPRESS.              ECS012
02150 /                                                                 ECS012
02151  LCP-WRITE-POS-PRT SECTION.                                       ECS012
02152      IF LCP-ASA = '+'                                             ECS012
02153          WRITE PRT AFTER 0 LINE                                   ECS012
02154      ELSE                                                         ECS012
02155      IF LCP-ASA = ' '                                             ECS012
02156          WRITE PRT AFTER ADVANCING 1 LINE                         ECS012
02157      ELSE                                                         ECS012
02158      IF LCP-ASA = '0'                                             ECS012
02159          WRITE PRT AFTER ADVANCING 2 LINE                         ECS012
02160      ELSE                                                         ECS012
02161      IF LCP-ASA = '-'                                             ECS012
02162          WRITE PRT AFTER ADVANCING 3 LINE                         ECS012
02163      ELSE                                                         ECS012
02164      IF LCP-ASA = '1'                                             ECS012
02165          WRITE PRT AFTER ADVANCING PAGE                           ECS012
02166      ELSE                                                         ECS012
02167      IF LCP-ASA = '2'                                             ECS012
02168          WRITE PRT AFTER ADVANCING LCP-CH2                        ECS012
02169      ELSE                                                         ECS012
02170      IF LCP-ASA = '3'                                             ECS012
02171          WRITE PRT AFTER ADVANCING LCP-CH3                        ECS012
02172      ELSE                                                         ECS012
02173      IF LCP-ASA = '4'                                             ECS012
02174          WRITE PRT AFTER ADVANCING LCP-CH4                        ECS012
02175      ELSE                                                         ECS012
02176      IF LCP-ASA = '5'                                             ECS012
02177          WRITE PRT AFTER ADVANCING LCP-CH5                        ECS012
02178      ELSE                                                         ECS012
02179      IF LCP-ASA = '6'                                             ECS012
02180          WRITE PRT AFTER ADVANCING LCP-CH6                        ECS012
02181      ELSE                                                         ECS012
02182      IF LCP-ASA = '7'                                             ECS012
02183          WRITE PRT AFTER ADVANCING LCP-CH7                        ECS012
02184      ELSE                                                         ECS012
02185      IF LCP-ASA = '8'                                             ECS012
02186          WRITE PRT AFTER ADVANCING LCP-CH8                        ECS012
02187      ELSE                                                         ECS012
02188      IF LCP-ASA = '9'                                             ECS012
02189          WRITE PRT AFTER ADVANCING LCP-CH9                        ECS012
02190      ELSE                                                         ECS012
02191      IF LCP-ASA = 'A'                                             ECS012
02192          WRITE PRT AFTER ADVANCING LCP-CH10                       ECS012
02193      ELSE                                                         ECS012
02194      IF LCP-ASA = 'B'                                             ECS012
02195          WRITE PRT AFTER ADVANCING LCP-CH11                       ECS012
02196      ELSE                                                         ECS012
02197      IF LCP-ASA = 'C'                                             ECS012
02198          WRITE PRT AFTER ADVANCING LCP-CH12                       ECS012
02199      ELSE                                                         ECS012
02200      IF LCP-ASA = 'V'                                             ECS012
02201          WRITE PRT AFTER ADVANCING LCP-P01                        ECS012
02202      ELSE                                                         ECS012
02203      IF LCP-ASA = 'W'                                             ECS012
02204          WRITE PRT AFTER ADVANCING LCP-P02                        ECS012
02205      ELSE                                                         ECS012
02206      DISPLAY 'ASA CODE ERROR'.                                    ECS012
02207  LCP-WRITE-END-PRT.                                               ECS012
02208      EXIT.                                                        ECS012
