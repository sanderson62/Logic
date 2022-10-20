00001  IDENTIFICATION DIVISION.                                         09/02/97
00002                                                                   ECS015
00003  PROGRAM-ID.          ECS015.                                        LV001
00004 *              PROGRAM CONVERTED BY                               ECS015
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS015
00006 *              CONVERSION DATE 12/06/95 07:59:35.                 ECS015
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS015
00008 *                     VMOD=2.009.                                 ECS015
00009                                                                   ECS015
00010 *AUTHOR.        LOGIC, INC.                                       ECS015
00011 *               DALLAS, TEXAS.                                    ECS015
00012                                                                   ECS015
00013 *DATE-COMPILED.                                                   ECS015
00014                                                                   ECS015
00015 *SECURITY.   *****************************************************ECS015
00016 *            *                                                   *ECS015
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS015
00018 *            *                                                   *ECS015
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS015
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS015
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *ECS015
00022 *            *                                                   *ECS015
00023 *            *****************************************************ECS015
00024                                                                   ECS015
00025 *REMARKS.                                                         ECS015
00026 *        PROGRAM PRINTS ACTIVITY CONCERNING REINSURANCE IN THE    ECS015
00027 *        CURRENT UPDATE TRANSACTIONS.                             ECS015
101403******************************************************************
101403*                   C H A N G E   L O G
101403*
101403* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101403*-----------------------------------------------------------------
101403*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101403* EFFECTIVE    NUMBER
101403*-----------------------------------------------------------------
100703* 100703    2003080800002  PEMA  ADD SECURE PAY PLUS PROCESSING
101403* 101403                   SMVA  DONT LET SORT ABEND IF NO REIN RECS
012609* 012609    2009012300002  AJRA  CREATE EXTRACT FILE OF DETAIL
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
101403******************************************************************
00028                                                                   ECS015
00029  ENVIRONMENT DIVISION.                                            ECS015
00030  INPUT-OUTPUT SECTION.                                            ECS015
00031  FILE-CONTROL.                                                    ECS015
00032                                                                   ECS015
00033      SELECT SRT-EXT          ASSIGN TO SYS001-UT-3380-S-SORTWK1.  ECS015
00034      SELECT PRNT-OUT         ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS015
pemuni     SELECT RTBL-FILE        ASSIGN TO ERRTBLT                    ECS015
00036                              ACCESS IS DYNAMIC                    ECS015
00037                              ORGANIZATION IS INDEXED              ECS015
00038                              FILE STATUS IS REIN-FILE-STATUS      ECS015
00039                              RECORD KEY IS RE-CONTROL-PRIMARY.    ECS015
00040      SELECT CUR-REIN         ASSIGN TO SYS018-UT-2400-S-SYS018.   ECS015
00041      SELECT DISK-DATE        ASSIGN TO SYS019-UT-3380-S-SYS019.   ECS015
00042      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS015
012609     SELECT EXT-FILE         ASSIGN TO SYS022-UT-2400-S-SYS022
012609                             ORGANIZATION IS LINE SEQUENTIAL.
00043  EJECT                                                            ECS015
00044  DATA DIVISION.                                                   ECS015
00045  FILE SECTION.                                                    ECS015
00046                                                                   ECS015
00047  SD  SRT-EXT.                                                     ECS015
00048                                                                   ECS015
00049  01  S-REC.                                                       ECS015
00050      12  FILLER                  PIC  X(4).                       ECS015
00051      12  S-CTL-1                 PIC  X(19).                      ECS015
00052      12  S-CTL-2                 PIC  X(17).                      ECS015
00053      12  FILLER                  PIC  X(470).                     ECS015
00054      12  S-REIN-COMPANY.                                          ECS015
00055          16  S-REINCO            PIC  X(3).                       ECS015
00056          16  S-REINCO-SUB        PIC  X(3).                       ECS015
00057  EJECT                                                            ECS015
00058  FD  PRNT-OUT                                                     ECS015
00059                                  COPY ELCPRTFD.                   ECS015
00060  EJECT                                                            ECS015
00061  FD  RTBL-FILE                                                    ECS015
00062                                  COPY ECSRTFDD.                   ECS015
00063                                                                   ECS015
00064                                  COPY ERCREIN.                    ECS015
00065  EJECT                                                            ECS015
00066  FD  CUR-REIN                                                     ECS015
00067                                  COPY ECSEXTFD.                   ECS015
00068                                                                   ECS015
00069  01  DE-EXTRACT-RECORD           PIC  X(510).                     ECS015
00070  EJECT                                                            ECS015
00071  FD  DISK-DATE                                                    ECS015
00072                                  COPY ELCDTEFD.                   ECS015
00073  EJECT                                                            ECS015
00074  FD  FICH                                                         ECS015
00075                                  COPY ELCFCHFD.                   ECS015
012609 FD  EXT-FILE
012609     BLOCK CONTAINS 0 RECORDS.
012609 01  EXT-RECORD                  PIC X(192).
00076  EJECT                                                            ECS015
00077  WORKING-STORAGE SECTION.                                         ECS015
00078  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS015
00079  77  FILLER  PIC X(32) VALUE '********************************'.  ECS015
00080  77  FILLER  PIC X(32) VALUE '     ECS015 WORKING STORAGE     '.  ECS015
00081  77  FILLER  PIC X(32) VALUE '********* VMOD=2.009 ***********'.  ECS015
00082                                                                   ECS015
00083  77  SA                      PIC S9(5)   COMP    VALUE +0.        ECS015
00084  77  PAGER                   PIC S9(5)   COMP-3  VALUE +0.        ECS015
00085  77  SELECT-COUNT            PIC S9(9)   COMP-3  VALUE +0.        ECS015
00086  77  REIN-IDX                PIC S9(3)   COMP    VALUE +0.        ECS015
00087  77  X                       PIC  X.                              ECS015
00088  77  FIRST-TIME-THRU-SW      PIC  X              VALUE '1'.       ECS015
00089  77  LINE-COUNT                  PIC  99             VALUE 99.    ECS015
00090                                                                   ECS015
00091  01  WS-ABEND-FIELDS.                                             ECS015
00092      12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.      ECS015
00093      12  WS-ZERO                 PIC S9          VALUE ZERO.      ECS015
00094      12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.    ECS015
00095      12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.      ECS015
00096                                                                   ECS015
00097  01  REIN-FILE-STATUS.                                            ECS015
00098      12  REIN-FILE-STATUS-1      PIC  X.                          ECS015
00099      12  REIN-FILE-STATUS-2      PIC  X.                          ECS015
00100                                                                   ECS015
00101  01  HEAD-A.                                                      ECS015
00102      12  FILLER              PIC  X(48)          VALUE SPACES.    ECS015
00103      12  FILLER              PIC  X(27)          VALUE            ECS015
00104              'REINSURANCE ACTIVITY REPORT'.                       ECS015
00105      12  FILLER              PIC  X(44)          VALUE SPACES.    ECS015
00106      12  FILLER              PIC  X(8)           VALUE 'ECS015 '. ECS015
00107                                                                   ECS015
00108  01  HEAD-B.                                                      ECS015
00109      12  FILLER              PIC  X(47)          VALUE SPACES.    ECS015
00110      12  HB-1                PIC  X(30).                          ECS015
00111      12  FILLER              PIC  X(42)          VALUE SPACES.    ECS015
00112      12  HD-RD               PIC  X(8).                           ECS015
00113                                                                   ECS015
00114  01  HEAD-C.                                                      ECS015
00115      12  FILLER              PIC  X(53)          VALUE SPACES.    ECS015
00116      12  HC-DATE             PIC  X(18).                          ECS015
00117      12  FILLER              PIC  X(48)          VALUE SPACES.    ECS015
00118      12  FILLER              PIC  X(5)           VALUE 'PAGE '.   ECS015
00119      12  PGNO                PIC  ZZ,ZZ9.                         ECS015
00120                                                                   ECS015
00121  01  HEAD-D SYNC.                                                 ECS015
00122      12  HD-DESC-1           PIC  X(23)          VALUE            ECS015
00123              ' REINSURANCE COMPANY - '.                           ECS015
00124      12  HD-1                PIC  X(3).                           ECS015
00125      12  FILLER              PIC  X              VALUE SPACE.     ECS015
00126      12  HD-2                PIC  X(30).                          ECS015
00127      12  FILLER              PIC  X(75)          VALUE SPACES.    ECS015
00128                                                                   ECS015
00129  01  HEAD-E SYNC.                                                 ECS015
00130      12  FILLER              PIC  X(12)          VALUE            ECS015
00131              ' CEDED FROM '.                                      ECS015
00132      12  HE-1                PIC  X(30).                          ECS015
00133                                                                   ECS015
00134  01  HEAD-F.                                                      ECS015
00135      12  FILLER              PIC  X(11)          VALUE            ECS015
00136              ' CARRIER - '.                                       ECS015
00137      12  HF-1                PIC  X.                              ECS015
00138      12  FILLER              PIC  X              VALUE SPACE.     ECS015
00139      12  HF-2                PIC  X(30).                          ECS015
00140                                                                   ECS015
00141  01  HEAD-G.                                                      ECS015
00142      12  FILLER              PIC  X(12)          VALUE            ECS015
00143              ' GROUPING - '.                                      ECS015
00144      12  HG-1                PIC  X(6).                           ECS015
00145                                                                   ECS015
00146  01  HEAD-H.                                                      ECS015
00147      12  FILLER              PIC  X(9)           VALUE            ECS015
00148              ' STATE - '.                                         ECS015
00149      12  HH-1                PIC  XX.                             ECS015
00150      12  FILLER              PIC  X              VALUE SPACE.     ECS015
00151      12  HH-2                PIC  X(30).                          ECS015
00152                                                                   ECS015
00153  01  HEAD-I SYNC.                                                 ECS015
00154      12  FILLER              PIC  X(44)          VALUE            ECS015
00155              '   ACCOUNT  REINS  CERTIFICATE   INSURED NAM'.      ECS015
00156      12  FILLER              PIC  X(44)          VALUE            ECS015
00157              'E  AGE S  BENE BENE TERM      BENEFIT       '.      ECS015
00158      12  FILLER              PIC  X(44)          VALUE            ECS015
00159              'PREMIUM     COMMISSION         CLAIM AMOUNT '.      ECS015
00160                                                                   ECS015
00161  01  HEAD-J SYNC.                                                 ECS015
00162      12  FILLER              PIC  X(44)          VALUE            ECS015
00163              '   NUMBER    DATE    NUMBER                 '.      ECS015
00164      12  FILLER              PIC  X(44)          VALUE            ECS015
00165              '       X  KIND TYPE                         '.      ECS015
00166      12  FILLER              PIC  X(44)          VALUE SPACES.    ECS015
00167                                                                   ECS015
00168  01  HEAD-RECAP SYNC.                                             ECS015
00169      12  FILLER              PIC  X(44)          VALUE            ECS015
00170              '   ****  REINSURANCE COMPANY RECAP  ****    '.      ECS015
00171      12  FILLER              PIC  X(44)          VALUE            ECS015
00172              '   ****  REINSURANCE COMPANY RECAP  ****    '.      ECS015
00173      12  FILLER              PIC  X(44)          VALUE            ECS015
00174              '   ****  REINSURANCE COMPANY RECAP  ****    '.      ECS015
00175                                                                   ECS015
00176  01  HEAD-TOTAL SYNC.                                             ECS015
00177      12  FILLER              PIC  X(44)          VALUE            ECS015
00178              '                                            '.      ECS015
00179      12  FILLER              PIC  X(44)          VALUE            ECS015
00180              '                              BENEFIT       '.      ECS015
00181      12  FILLER              PIC  X(44)          VALUE            ECS015
00182              'PREMIUM     COMMISSION         CLAIM AMOUNT '.      ECS015
00183                                                                   ECS015
00184      EJECT                                                        ECS015
00185  01  DATAL1 SYNC.                                                 ECS015
00186      12  FILLER                  PIC  X              VALUE SPACES.ECS015
00187      12  D1-ACCT                 PIC  X(10).                      ECS015
00188      12  FILLER                  PIC  X              VALUE SPACE. ECS015
00189      12  D1-REIN-MO              PIC  XX.                         ECS015
00190      12  D1-REIN-DA              PIC  XX.                         ECS015
00191      12  D1-REIN-YR              PIC  XX.                         ECS015
00192      12  FILLER                  PIC  X              VALUE SPACE. ECS015
00193      12  D1-CERT                 PIC  X(11).                      ECS015
00194      12  FILLER                  PIC  X              VALUE SPACE. ECS015
00195      12  D1-LAST-NAME            PIC  X(13).                      ECS015
00196      12  FILLER                  PIC  X              VALUE SPACE. ECS015
00197      12  D1-INIT-1               PIC  X              VALUE SPACE. ECS015
00198      12  D1-INIT-2               PIC  X              VALUE SPACE. ECS015
00199      12  FILLER                  PIC  X              VALUE SPACE. ECS015
00200      12  D1-AGE                  PIC  XX.                         ECS015
00201      12  FILLER                  PIC  X              VALUE SPACES.ECS015
00202      12  D1-SEX                  PIC  X.                          ECS015
00203      12  FILLER                  PIC  X              VALUE SPACES.ECS015
00204      12  D1-BENE-AREA.                                            ECS015
00205          16  D1-KIND             PIC  X(6).                       ECS015
00206          16  FILLER              PIC  X              VALUE SPACES.ECS015
00207          16  D1-TYPE             PIC  X(3).                       ECS015
00208          16  FILLER              PIC  X              VALUE SPACES.ECS015
00209          16  D1-TERM             PIC  Z99.                        ECS015
00210          16  FILLER              PIC  X              VALUE SPACES.ECS015
00211          16  D1-BENEFIT          PIC  ZZZ,ZZZ,ZZZ.99-.            ECS015
00212          16  FILLER              PIC  X              VALUE SPACES.ECS015
00213          16  D1-PREMIUM          PIC  Z,ZZZ,ZZZ.99-.              ECS015
00214          16  D1-I-DATE REDEFINES D1-PREMIUM.                      ECS015
00215              20  FILLER          PIC  X(3).                       ECS015
00216              20  D1-I-MO         PIC  Z9.                         ECS015
00217              20  D1-I-D1         PIC  X.                          ECS015
00218              20  D1-I-DA         PIC  99.                         ECS015
00219              20  D1-I-D2         PIC  X.                          ECS015
00220              20  D1-I-YR         PIC  99.                         ECS015
00221              20  FILLER          PIC  XX.                         ECS015
00222          16  FILLER              PIC  X              VALUE SPACES.ECS015
00223          16  D1-C-DATE.                                           ECS015
00224              20  FILLER          PIC  X(3).                       ECS015
00225              20  D1-C-MO         PIC  Z9.                         ECS015
00226              20  D1-C-D1         PIC  X.                          ECS015
00227              20  D1-C-DA         PIC  99.                         ECS015
00228              20  D1-C-D2         PIC  X.                          ECS015
00229              20  D1-C-YR         PIC  99.                         ECS015
00230              20  FILLER          PIC  XX.                         ECS015
00231          16  D1-COMM REDEFINES D1-C-DATE.                         ECS015
00232              20  D1-COMMISSION   PIC  Z,ZZZ,ZZZ.99-.              ECS015
00233          16  FILLER              PIC  X              VALUE SPACES.ECS015
00234          16  D1-CLAIM.                                            ECS015
00235              20  FILLER          PIC  X(6).                       ECS015
00236              20  D1-CLAMT        PIC  ZZZ,ZZZ,ZZZ.99-.            ECS015
00237          16  D1-CNC-DATE REDEFINES D1-CLAIM.                      ECS015
00238              20  D1-MES          PIC  X(6).                       ECS015
00239              20  FILLER          PIC  X(3).                       ECS015
00240              20  D1-CNC-MO       PIC  XX.                         ECS015
00241              20  D1-CNC-MOD      PIC  X.                          ECS015
00242              20  D1-CNC-DA       PIC  XX.                         ECS015
00243              20  D1-CNC-DAD      PIC  X.                          ECS015
00244              20  D1-CNC-YR       PIC  XX.                         ECS015
00245              20  FILLER          PIC  X(4).                       ECS015
00246                                                                   ECS015
00247  01  DATAL2 SYNC.                                                 ECS015
00248      12  FILLER                  PIC  X(53)          VALUE SPACES.ECS015
00249      12  D2-BENE-AREA.                                            ECS015
00250          16  D2-KIND             PIC  X(6).                       ECS015
00251          16  FILLER              PIC  X              VALUE SPACES.ECS015
00252          16  D2-TYPE             PIC  X(3).                       ECS015
00253          16  FILLER              PIC  X              VALUE SPACES.ECS015
00254          16  D2-TERM             PIC  Z99.                        ECS015
00255          16  FILLER              PIC  X              VALUE SPACES.ECS015
00256          16  D2-BENEFIT          PIC  ZZZ,ZZZ,ZZZ.99-.            ECS015
00257          16  FILLER              PIC  X              VALUE SPACES.ECS015
00258          16  D2-PREMIUM          PIC  Z,ZZZ,ZZZ.99-.              ECS015
00259          16  D2-I-DATE REDEFINES D2-PREMIUM.                      ECS015
00260              20  FILLER          PIC  X(3).                       ECS015
00261              20  D2-I-MO         PIC  Z9.                         ECS015
00262              20  D2-I-D1         PIC  X.                          ECS015
00263              20  D2-I-DA         PIC  99.                         ECS015
00264              20  D2-I-D2         PIC  X.                          ECS015
00265              20  D2-I-YR         PIC  99.                         ECS015
00266              20  FILLER          PIC  XX.                         ECS015
00267          16  FILLER              PIC  X              VALUE SPACES.ECS015
00268          16  D2-C-DATE.                                           ECS015
00269              20  FILLER          PIC  X(3).                       ECS015
00270              20  D2-C-MO         PIC  Z9.                         ECS015
00271              20  D2-C-D1         PIC  X.                          ECS015
00272              20  D2-C-DA         PIC  99.                         ECS015
00273              20  D2-C-D2         PIC  X.                          ECS015
00274              20  D2-C-YR         PIC  99.                         ECS015
00275              20  FILLER          PIC  XX.                         ECS015
00276          16  D2-COMM REDEFINES D2-C-DATE.                         ECS015
00277              20  D2-COMMISSION   PIC  Z,ZZZ,ZZZ.99-.              ECS015
00278          16  FILLER              PIC  X              VALUE SPACES.ECS015
00279          16  D2-CLAIM.                                            ECS015
00280              20  FILLER          PIC  X(6)           VALUE SPACES.ECS015
00281              20  D2-CLAMT        PIC  ZZZ,ZZZ,ZZZ.99-.            ECS015
00282          16  D2-CNC-DATE REDEFINES D2-CLAIM.                      ECS015
00283              20  D2-MES          PIC  X(6).                       ECS015
00284              20  FILLER          PIC  X(3).                       ECS015
00285              20  D2-CNC-MO       PIC  XX.                         ECS015
00286              20  D2-CNC-MOD      PIC  X.                          ECS015
00287              20  D2-CNC-DA       PIC  XX.                         ECS015
00288              20  D2-CNC-DAD      PIC  X.                          ECS015
00289              20  D2-CNC-YR       PIC  XX.                         ECS015
00290              20  FILLER          PIC  X(4).                       ECS015
00291                                                                   ECS015
00292  01  TOT-1.                                                       ECS015
00293      12  FILLER                  PIC  X(23).                      ECS015
00294      12  T1-DESC.                                                 ECS015
00295          16  T1-DESC-1           PIC  X(14).                      ECS015
00296          16  T1-REINCO-SUB       PIC  X(3).                       ECS015
00297          16  T1-DESC-2           PIC  X(13).                      ECS015
00298      12  T1-BENE-AREA.                                            ECS015
00299          16  T1-KIND             PIC  X(6).                       ECS015
00300          16  FILLER              PIC  X(6)           VALUE SPACES.ECS015
00301          16  T1-BENEFIT          PIC  ZZ,ZZZ,ZZZ,ZZZ.99-.         ECS015
00302          16  T1-PREMIUM          PIC  ZZZ,ZZZ,ZZZ.99-.            ECS015
00303          16  T1-COMMISSION       PIC  ZZZ,ZZZ,ZZZ.99-.            ECS015
00304          16  T1-CLAIMS           PIC  ZZZZ,ZZZ,ZZZ,ZZZ.99-.       ECS015
00305                                                                   ECS015
00306  01  TOT-2.                                                       ECS015
00307      12  FILLER                  PIC  X(53).                      ECS015
00308      12  T2-BENE-AREA.                                            ECS015
00309          16  T2-KIND             PIC  X(6).                       ECS015
00310          16  FILLER              PIC  X(6)           VALUE SPACES.ECS015
00311          16  T2-BENEFIT          PIC  ZZ,ZZZ,ZZZ,ZZZ.99-.         ECS015
00312          16  T2-PREMIUM          PIC  ZZZ,ZZZ,ZZZ.99-.            ECS015
00313          16  T2-COMMISSION       PIC  ZZZ,ZZZ,ZZZ.99-.            ECS015
00314          16  T2-CLAIMS           PIC  ZZZZ,ZZZ,ZZZ,ZZZ.99-.       ECS015
00315                                                                   ECS015
00316  01  WS-CLAIM-DESC.                                               ECS015
00317      12  FILLER                  PIC X(5)    VALUE 'LESS '.       ECS015
00318      12  CD-OVRD                 PIC X(6).                        ECS015
00319      12  FILLER                  PIC X(7)    VALUE ' CLAIMS'.     ECS015
00320                                                                   ECS015
00321  01  ZERO-AREAS          COMP-3    SYNC.                          ECS015
00322      12  FILLER                  PIC S9(9)V99        VALUE +0.    ECS015
00323      12  FILLER                  PIC S9(9)V99        VALUE +0.    ECS015
00324      12  FILLER                  PIC S9(11)V99       VALUE +0.    ECS015
00325      12  FILLER                  PIC S9(11)V99       VALUE +0.    ECS015
00326      12  FILLER                  PIC S9(9)V99        VALUE +0.    ECS015
00327      12  FILLER                  PIC S9(9)V99        VALUE +0.    ECS015
00328      12  FILLER                  PIC S9(11)V99       VALUE +0.    ECS015
00329      12  FILLER                  PIC S9(9)V99        VALUE +0.    ECS015
00330      12  FILLER                  PIC S9(9)V99        VALUE +0.    ECS015
00331      12  FILLER                  PIC S9(9)V99        VALUE +0.    ECS015
00332  01  ACCUM-WORK-AREAS    COMP-3    SYNC.                          ECS015
00333      12  T-LPREM                 PIC S9(9)V99        VALUE +0.    ECS015
00334      12  T-APREM                 PIC S9(9)V99        VALUE +0.    ECS015
00335      12  T-LBEN                  PIC S9(11)V99       VALUE +0.    ECS015
00336      12  T-ABEN                  PIC S9(11)V99       VALUE +0.    ECS015
00337      12  T-CANCL                 PIC S9(9)V99        VALUE +0.    ECS015
00338      12  T-CANCA                 PIC S9(9)V99        VALUE +0.    ECS015
00339      12  T-DEATH                 PIC S9(11)V99       VALUE +0.    ECS015
00340      12  T-DISAB                 PIC S9(9)V99        VALUE +0.    ECS015
00341      12  T-LCOM                  PIC S9(9)V99        VALUE +0.    ECS015
00342      12  T-ACOM                  PIC S9(9)V99        VALUE +0.    ECS015
00343  01  GROUP-WORK-AREAS    COMP-3    SYNC.                          ECS015
00344      12  G-LPREM                 PIC S9(9)V99        VALUE +0.    ECS015
00345      12  G-APREM                 PIC S9(9)V99        VALUE +0.    ECS015
00346      12  G-LBEN                  PIC S9(11)V99       VALUE +0.    ECS015
00347      12  G-ABEN                  PIC S9(11)V99       VALUE +0.    ECS015
00348      12  G-CANCL                 PIC S9(9)V99        VALUE +0.    ECS015
00349      12  G-CANCA                 PIC S9(9)V99        VALUE +0.    ECS015
00350      12  G-DEATH                 PIC S9(11)V99       VALUE +0.    ECS015
00351      12  G-DISAB                 PIC S9(9)V99        VALUE +0.    ECS015
00352      12  G-LCOM                  PIC S9(9)V99        VALUE +0.    ECS015
00353      12  G-ACOM                  PIC S9(9)V99        VALUE +0.    ECS015
00354  01  CARR-WORK-AREAS    COMP-3    SYNC.                           ECS015
00355      12  C-LPREM                 PIC S9(9)V99        VALUE +0.    ECS015
00356      12  C-APREM                 PIC S9(9)V99        VALUE +0.    ECS015
00357      12  C-LBEN                  PIC S9(11)V99       VALUE +0.    ECS015
00358      12  C-ABEN                  PIC S9(11)V99       VALUE +0.    ECS015
00359      12  C-CANCL                 PIC S9(9)V99        VALUE +0.    ECS015
00360      12  C-CANCA                 PIC S9(9)V99        VALUE +0.    ECS015
00361      12  C-DEATH                 PIC S9(11)V99       VALUE +0.    ECS015
00362      12  C-DISAB                 PIC S9(9)V99        VALUE +0.    ECS015
00363      12  C-LCOM                  PIC S9(9)V99        VALUE +0.    ECS015
00364      12  C-ACOM                  PIC S9(9)V99        VALUE +0.    ECS015
00365  01  REIN-SUB-WORK-AREAS    COMP-3    SYNC.                       ECS015
00366      12  S-LPREM                 PIC S9(9)V99        VALUE +0.    ECS015
00367      12  S-APREM                 PIC S9(9)V99        VALUE +0.    ECS015
00368      12  S-LBEN                  PIC S9(11)V99       VALUE +0.    ECS015
00369      12  S-ABEN                  PIC S9(11)V99       VALUE +0.    ECS015
00370      12  S-CANCL                 PIC S9(9)V99        VALUE +0.    ECS015
00371      12  S-CANCA                 PIC S9(9)V99        VALUE +0.    ECS015
00372      12  S-DEATH                 PIC S9(11)V99       VALUE +0.    ECS015
00373      12  S-DISAB                 PIC S9(9)V99        VALUE +0.    ECS015
00374      12  S-LCOM                  PIC S9(9)V99        VALUE +0.    ECS015
00375      12  S-ACOM                  PIC S9(9)V99        VALUE +0.    ECS015
00376  01  REINS-WORK-AREAS    COMP-3    SYNC.                          ECS015
00377      12  R-LPREM                 PIC S9(9)V99        VALUE +0.    ECS015
00378      12  R-APREM                 PIC S9(9)V99        VALUE +0.    ECS015
00379      12  R-LBEN                  PIC S9(11)V99       VALUE +0.    ECS015
00380      12  R-ABEN                  PIC S9(11)V99       VALUE +0.    ECS015
00381      12  R-CANCL                 PIC S9(9)V99        VALUE +0.    ECS015
00382      12  R-CANCA                 PIC S9(9)V99        VALUE +0.    ECS015
00383      12  R-DEATH                 PIC S9(11)V99       VALUE +0.    ECS015
00384      12  R-DISAB                 PIC S9(9)V99        VALUE +0.    ECS015
00385      12  R-LCOM                  PIC S9(9)V99        VALUE +0.    ECS015
00386      12  R-ACOM                  PIC S9(9)V99        VALUE +0.    ECS015
00387                                                                   ECS015
00388                                                                   ECS015
00389  01  FINAL-WORK-AREAS    COMP-3    SYNC.                          ECS015
00390      12  F-LPREM                 PIC S9(9)V99        VALUE +0.    ECS015
00391      12  F-APREM                 PIC S9(9)V99        VALUE +0.    ECS015
00392      12  F-LBEN                  PIC S9(11)V99       VALUE +0.    ECS015
00393      12  F-ABEN                  PIC S9(11)V99       VALUE +0.    ECS015
00394      12  F-CANCL                 PIC S9(9)V99        VALUE +0.    ECS015
00395      12  F-CANCA                 PIC S9(9)V99        VALUE +0.    ECS015
00396      12  F-DEATH                 PIC S9(11)V99       VALUE +0.    ECS015
00397      12  F-DISAB                 PIC S9(9)V99        VALUE +0.    ECS015
00398      12  F-LCOM                  PIC S9(9)V99        VALUE +0.    ECS015
00399      12  F-ACOM                  PIC S9(9)V99        VALUE +0.    ECS015
00400                                                                   ECS015
00401  01  ACCOUNT-ACCUMS      COMP-3.                                  ECS015
00402      12  A-LPREM                 PIC S9(9)V99        VALUE +0.    ECS015
00403      12  A-APREM                 PIC S9(9)V99        VALUE +0.    ECS015
00404      12  A-LBEN                  PIC S9(11)V99       VALUE +0.    ECS015
00405      12  A-ABEN                  PIC S9(11)V99       VALUE +0.    ECS015
00406      12  A-CANCL                 PIC S9(9)V99        VALUE +0.    ECS015
00407      12  A-CANCA                 PIC S9(9)V99        VALUE +0.    ECS015
00408      12  A-DEATH                 PIC S9(11)V99       VALUE +0.    ECS015
00409      12  A-DISAB                 PIC S9(9)V99        VALUE +0.    ECS015
00410      12  A-LCOM                  PIC S9(9)V99        VALUE +0.    ECS015
00411      12  A-ACOM                  PIC S9(9)V99        VALUE +0.    ECS015
00412                                                                   ECS015
00413  01  DETAIL-ACCUMS       COMP-3.                                  ECS015
00414      12  DET-LCOM                PIC S9(9)V99        VALUE +0.    ECS015
00415      12  DET-ACOM                PIC S9(9)V99        VALUE +0.    ECS015
00416                                                                   ECS015
00417  01  WORK-REI-FIELDS     COMP-3.                                  ECS015
00418      12  WRK-REI-LFAMT           PIC S9(11)V99       VALUE +0.    ECS015
00419      12  WRK-REI-AHAMT           PIC S9(9)V99        VALUE +0.    ECS015
00420      12  WRK-REI-CNAMT           PIC S9(11)V99       VALUE +0.    ECS015
00421                                                                   ECS015
00422  01  MISC-WORK-AREAS.                                             ECS015
00423      12  LAST-REIN.                                               ECS015
00424          16  LAST-REINCO         PIC  X(3).                       ECS015
00425          16  LAST-REINCO-SUB     PIC  X(3).                       ECS015
00426      12  LAST-CARRIER            PIC  X.                          ECS015
00427      12  LAST-GROUPING           PIC  X(6).                       ECS015
00428      12  LAST-STATE              PIC  XX.                         ECS015
00429      12  LAST-ACCOUNT            PIC  X(10)      VALUE SPACES.    ECS015
00430      12  W-ST.                                                    ECS015
00431          16  W-S                 PIC  99.                         ECS015
00432      12  WS-AGT                  PIC  X(10).                      ECS015
00433      12  REIN-SEARCH.                                             ECS015
00434          16  REIN-CODE           PIC  X               VALUE 'B'.  ECS015
00435          16  REIN-SRCH-CO        PIC  XXX.                        ECS015
00436          16  REIN-SRCH-SUB       PIC  XXX.                        ECS015
00437      12  LAST-REIN-SEARCH        PIC  X(7)       VALUE LOW-VALUES.ECS015
00438      12  REIN-DESCRIPT.                                           ECS015
00439          16  PRT-REINS-COMP      PIC  X(6)       VALUE SPACES.    ECS015
00440          16  FILLER              PIC  X(8)       VALUE ' RECAP  '.ECS015
00441      12  REIN-NAME               PIC  X(30).                      ECS015
00442      12  CEDE-NAME               PIC  X(30).                      ECS015
00443                                                                   ECS015
00444  01  MISC.                                                        ECS015
00445      12  PGM-SUB                 PIC S999    COMP    VALUE +015.  ECS015
00446  EJECT                                                            ECS015
00447                                  COPY ECSEXT01.                   ECS015
00448  EJECT                                                            ECS015
00449                                  COPY ELCEXTVR.                   ECS015
00450  EJECT                                                            ECS015
00451                                  COPY ELCDTECX.                   ECS015
00452  EJECT                                                            ECS015
00453                                  COPY ELCDTEVR.                   ECS015
00454  EJECT                                                            ECS015
00455  01  REIN-COMPANY-RECAP-TABLE.                                    ECS015
00456      12  TOTL-COMPANY-AREAS        COMP-3.                        ECS015
00457          16  TOTL-LPREM              PIC S9(9)V99    VALUE +0.    ECS015
00458          16  TOTL-APREM              PIC S9(9)V99    VALUE +0.    ECS015
00459          16  TOTL-LBEN               PIC S9(11)V99   VALUE +0.    ECS015
00460          16  TOTL-ABEN               PIC S9(11)V99   VALUE +0.    ECS015
00461          16  TOTL-CANCL              PIC S9(9)V99    VALUE +0.    ECS015
00462          16  TOTL-CANCA              PIC S9(9)V99    VALUE +0.    ECS015
00463          16  TOTL-DEATH              PIC S9(11)V99   VALUE +0.    ECS015
00464          16  TOTL-DISAB              PIC S9(9)V99    VALUE +0.    ECS015
00465          16  TOTL-LCOM               PIC S9(9)V99    VALUE +0.    ECS015
00466          16  TOTL-ACOM               PIC S9(9)V99    VALUE +0.    ECS015
00467                                                                   ECS015
00468      12  REIN-COMPANY-INFORMATION   OCCURS   500 TIMES.           ECS015
00469          16  REIN-COMPANY.                                        ECS015
00470              20  REIN-PRIME          PIC X(03).                   ECS015
00471              20  REIN-SUB            PIC X(03).                   ECS015
00472          16  REIN-COMPANY-AREAS        COMP-3.                    ECS015
00473              20  REIN-LPREM          PIC S9(9)V99.                ECS015
00474              20  REIN-APREM          PIC S9(9)V99.                ECS015
00475              20  REIN-LBEN           PIC S9(11)V99.               ECS015
00476              20  REIN-ABEN           PIC S9(11)V99.               ECS015
00477              20  REIN-CANCL          PIC S9(9)V99.                ECS015
00478              20  REIN-CANCA          PIC S9(9)V99.                ECS015
00479              20  REIN-DEATH          PIC S9(11)V99.               ECS015
00480              20  REIN-DISAB          PIC S9(9)V99.                ECS015
00481              20  REIN-LCOM           PIC S9(9)V99.                ECS015
00482              20  REIN-ACOM           PIC S9(9)V99.                ECS015
012609
012609 01  EXT-DATAL SYNC.
012609     12  EXT-MOE-MO              PIC  9(2).
012609     12  FILLER                  PIC  X              VALUE '/'.
012609     12  EXT-MOE-DA              PIC  9(2).
012609     12  FILLER                  PIC  X              VALUE '/'.
012609     12  EXT-MOE-YR              PIC  9(4).
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-CARRIER             PIC  X(1).
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-STATE               PIC  X(2).
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-ACCT                PIC  X(10).
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-REIN-CO             PIC  X(6).
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-REIN-MO             PIC  99.
012609     12  FILLER                  PIC  X              VALUE '/'.
012609     12  EXT-REIN-DA             PIC  99.
012609     12  FILLER                  PIC  X              VALUE '/'.
012609     12  EXT-REIN-YR             PIC  9(4).
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-CERT                PIC  X(11). 
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-LAST-NAME           PIC  X(13). 
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-INIT-1              PIC  X              VALUE SPACE.
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-INIT-2              PIC  X              VALUE SPACE.
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-AGE                 PIC  XX.
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-SEX                 PIC  X.
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-KIND                PIC  X(6).
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-TYPE                PIC  X(3).
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-TERM                PIC  Z99.           
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-BENEFIT             PIC  -(9).99.
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-PREMIUM             PIC  -(7).99.
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-COMMISSION          PIC  -(7).99.                      
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-MES                 PIC  X(6).
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-CANCEL-DATE.
012609         16  EXT-CNC-MO          PIC  99.
012609         16  EXT-SL-1            PIC  X              VALUE SPACE.
012609         16  EXT-CNC-DA          PIC  99.  
012609         16  EXT-SL-2            PIC  X              VALUE SPACE.
012609         16  EXT-CNC-YR          PIC  9(4).
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-CLAIM-NUMBER        PIC  X(7).                      
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-CLAIM-AMT           PIC  -(9).99.
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-CLAIM-INCURRED.           
012609         16  EXT-INC-MO          PIC  99.
012609         16  EXT-SL-3            PIC  X              VALUE SPACE.
012609         16  EXT-INC-DA          PIC  99.  
012609         16  EXT-SL-4            PIC  X              VALUE SPACE.
012609         16  EXT-INC-YR          PIC  9(4).
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  EXT-CLAIM-PAID.
012609         16  EXT-PD-MO           PIC  99.
012609         16  EXT-SL-5            PIC  X              VALUE SPACE.
012609         16  EXT-PD-DA           PIC  99.  
012609         16  EXT-SL-6            PIC  X              VALUE SPACE.
012609         16  EXT-PD-YR           PIC  9(4).
012609     12  FILLER                  PIC  X              VALUE ';'.
012609     12  FILLER                  PIC  X              VALUE 'E'.
012609
012609 01  HOLD-EXT-AH.
012609     12  HOLD-KIND               PIC  X(6).
012609     12  HOLD-TYPE               PIC  X(3).
012609     12  HOLD-TERM               PIC  Z99.           
012609     12  HOLD-BENEFIT            PIC  -(9).99.
012609     12  HOLD-PREMIUM            PIC  -(7).99.
012609     12  HOLD-COMMISSION         PIC  -(7).99.                      
012609     12  HOLD-MES                PIC  X(6).
012609     12  HOLD-CANCEL-DATE.
012609         16  HOLD-CNC-MO         PIC  99.
012609         16  HOLD-SL-1           PIC  X              VALUE SPACE.
012609         16  HOLD-CNC-DA         PIC  99.  
012609         16  HOLD-SL-2           PIC  X              VALUE SPACE.
012609         16  HOLD-CNC-YR         PIC  9(4).
012609     12  HOLD-CLAIM-NUMBER       PIC  X(7).                      
012609     12  HOLD-CLAIM-AMT          PIC  -(9).99.
012609     12  HOLD-CLAIM-INCURRED.           
012609         16  HOLD-INC-MO         PIC  99.
012609         16  HOLD-SL-3           PIC  X              VALUE SPACE.
012609         16  HOLD-INC-DA         PIC  99.  
012609         16  HOLD-SL-4           PIC  X              VALUE SPACE.
012609         16  HOLD-INC-YR         PIC  9(4).
012609     12  HOLD-CLAIM-PAID.
012609         16  HOLD-PD-MO          PIC  99.
012609         16  HOLD-SL-5           PIC  X              VALUE SPACE.
012609         16  HOLD-PD-DA          PIC  99.  
012609         16  HOLD-SL-6           PIC  X              VALUE SPACE.
012609         16  HOLD-PD-YR          PIC  9(4).
012609
00483                                                                   ECS015
00484  EJECT                                                            ECS015
00485  PROCEDURE DIVISION.                                              ECS015
00486                                                                   ECS015
00487  0000-INITIALIZE-RTN SECTION.                                     ECS015
00488                                                                   ECS015
00489  0001-STANDARD-RTN.                                               ECS015
00490                                  COPY ELCDTERX SUPPRESS.          ECS015
00491                                                                   ECS015
00492      MOVE ALPH-DATE              TO  HC-DATE.                     ECS015
00493      MOVE COMPANY-NAME           TO  HB-1.                        ECS015
00494      MOVE WS-CURRENT-DATE        TO  HD-RD.                       ECS015
00495      MOVE DTE-CLASIC-COMPANY-CD  TO  RE-COMPANY-CD.               ECS015
012609     MOVE RUN-MO                 TO  EXT-MOE-MO.
012609     MOVE RUN-DA                 TO  EXT-MOE-DA.
012609     MOVE RUN-CCYY               TO  EXT-MOE-YR.
00496                                                                   ECS015
00497  0010-INITIAL-REIN-TABLE.                                         ECS015
00498                                                                   ECS015
00499      MOVE +1                     TO REIN-IDX.                     ECS015
00500                                                                   ECS015
00501  0015-INITIAL-LOOP.                                               ECS015
00502                                                                   ECS015
00503      MOVE SPACES                 TO REIN-COMPANY       (REIN-IDX).ECS015
00504      MOVE ZERO-AREAS             TO REIN-COMPANY-AREAS (REIN-IDX).ECS015
00505                                                                   ECS015
00506      IF REIN-IDX LESS THAN +500                                   ECS015
00507          ADD +1                  TO REIN-IDX                      ECS015
00508              GO TO 0015-INITIAL-LOOP.                             ECS015
00509                                                                   ECS015
00510      GO TO 0110-SORT-RTN.                                         ECS015
00511                                                                   ECS015
00512  EJECT                                                            ECS015
00513  0100-SORT-REINSURANCE SECTION.                                   ECS015
00514                                                                   ECS015
00515  0110-SORT-RTN.                                                   ECS015
00516                                                                   ECS015
00517      OPEN INPUT  RTBL-FILE                                        ECS015
00518                  CUR-REIN                                         ECS015
00519           OUTPUT PRNT-OUT                                         ECS015
012609                 EXT-FILE
00520                                                                   ECS015
00521      IF REIN-FILE-STATUS  EQUAL '35'                              ECS015
00522          GO TO 9010-E-O-J.                                        ECS015
00523                                                                   ECS015
00524      IF REIN-FILE-STATUS  EQUAL '00' OR '97'                      ECS015
00525          NEXT SENTENCE                                            ECS015
00526        ELSE                                                       ECS015
00527          MOVE 'OPEN ERROR - REIN'    TO  WS-ABEND-MESSAGE         ECS015
00528          MOVE REIN-FILE-STATUS       TO  WS-ABEND-FILE-STATUS     ECS015
00529          GO TO ABEND-PGM.                                         ECS015
00530                                                                   ECS015
00531      SORT SRT-EXT ON ASCENDING KEY                                ECS015
00532                        S-REINCO  S-CTL-1  S-REINCO-SUB  S-CTL-2   ECS015
00533          INPUT PROCEDURE IS 0200-SELECT-REINS  THRU               ECS015
00534              0299-SET-SELECT-XIT                                  ECS015
00535          OUTPUT PROCEDURE IS 0300-PRINT-REINS  THRU               ECS015
00536              1999-EXIT-REINS.                                     ECS015
00537                                                                   ECS015
00538      IF SORT-RETURN  GREATER THAN 4                               ECS015
101403         IF SELECT-COUNT > ZERO 
00539              MOVE 'SORT ERROR '  TO  WS-ABEND-MESSAGE             ECS015
00540              MOVE 101            TO  WS-RETURN-CODE               ECS015
00541              GO TO ABEND-PGM.
00542                                                                   ECS015
00543      GO TO 9010-E-O-J.                                            ECS015
00544                                                                   ECS015
00545  EJECT                                                            ECS015
00546  0200-SELECT-REINS SECTION.                                       ECS015
00547                                                                   ECS015
00548  0210-SET-SELECT.                                                 ECS015
00549                                                                   ECS015
00550  0220-ML1.                                                        ECS015
00551                                                                   ECS015
00552      READ CUR-REIN  AT END                                        ECS015
00553          GO TO 0230-END-SELECT.                                   ECS015
00554                                                                   ECS015
00555      MOVE DE-EXTRACT-RECORD      TO  DETAIL-EXTRACT.              ECS015
00556                                                                   ECS015
00557      IF DE-RECORD-ID  NOT EQUAL 'DE'                              ECS015
00558          GO TO 0220-ML1.                                          ECS015
00559                                                                   ECS015
00560      IF DE-REIN  NOT EQUAL 'R'                                    ECS015
101403*        DISPLAY 'DE-REIN not = R dont release to sort'
00561          GO TO 0220-ML1.                                          ECS015
00562                                                                   ECS015
00563      IF NOT DE-ISSUE      AND  NOT DE-CANCEL     AND              ECS015
00564         NOT DE-CLAIM      AND  NOT DE-RC-ISSUE   AND              ECS015
00565         NOT DE-RC-CANCEL  AND  NOT DE-RR-RC-ISS  AND              ECS015
00566         NOT DE-RR-RC-CNC  AND  NOT DE-RR-RC-CLM                   ECS015
00567          GO TO  0220-ML1.                                         ECS015
00568                                                                   ECS015
00569      IF (DE-RC-ISSUE  OR  DE-RC-CANCEL)  AND                      ECS015
00570         DTE-CLIENT EQUAL 'FLA'                                    ECS015
00571          GO TO 0220-ML1.                                          ECS015
00572                                                                   ECS015
00573      COPY ELCEXTM1.                                               ECS015
00574                                                                   ECS015
00575      MOVE DETAIL-EXTRACT         TO  S-REC.                       ECS015
00576      MOVE DE-REINCO              TO  S-REINCO.                    ECS015
00577      MOVE DE-REINCO-SUB          TO  S-REINCO-SUB.                ECS015
00578                                                                   ECS015
00579      RELEASE S-REC.                                               ECS015
00580                                                                   ECS015
00581      ADD +1  TO  SELECT-COUNT.                                    ECS015
00582                                                                   ECS015
00583      GO TO 0220-ML1.                                              ECS015
00584                                                                   ECS015
00585  0230-END-SELECT.                                                 ECS015
00586                                                                   ECS015
00587      CLOSE CUR-REIN.                                              ECS015
00588                                                                   ECS015
00589  0299-SET-SELECT-XIT.                                             ECS015
00590      EXIT.                                                        ECS015
00591                                                                   ECS015
00592  EJECT                                                            ECS015
00593  0300-PRINT-REINS SECTION.                                        ECS015
00594                                                                   ECS015
00595      IF SELECT-COUNT  EQUAL +0                                    ECS015
101403         DISPLAY 'no records selected for sort'
00596          GO TO 1900-NO-INPUT-RTN.                                 ECS015
00597                                                                   ECS015
00598  0310-R-LOOP.                                                     ECS015
00599                                                                   ECS015
00600      RETURN SRT-EXT  AT END                                       ECS015
00601         GO TO 1910-END-PRINT.                                     ECS015
00602                                                                   ECS015
00603      MOVE +1                     TO REIN-IDX.                     ECS015
00604                                                                   ECS015
00605  0315-REIN-MATCH-LOOP.                                            ECS015
00606                                                                   ECS015
00607      IF REIN-COMPANY (REIN-IDX) EQUAL S-REIN-COMPANY              ECS015
00608          GO TO 0320-REIN-TABLE-MATCHED.                           ECS015
00609                                                                   ECS015
00610      IF REIN-COMPANY (REIN-IDX) EQUAL SPACES                      ECS015
00611          MOVE S-REINCO           TO REIN-PRIME (REIN-IDX)         ECS015
00612          MOVE S-REINCO-SUB       TO REIN-SUB   (REIN-IDX)         ECS015
00613      ELSE                                                         ECS015
00614          ADD +1                  TO REIN-IDX                      ECS015
00615          GO TO 0315-REIN-MATCH-LOOP.                              ECS015
00616                                                                   ECS015
00617  0320-REIN-TABLE-MATCHED.                                         ECS015
00618                                                                   ECS015
00619      MOVE S-REC                  TO DETAIL-EXTRACT.               ECS015
00620                                                                   ECS015
00621      COPY ELCEXTM1.                                               ECS015
00622                                                                   ECS015
00623      IF FIRST-TIME-THRU-SW  EQUAL '1'                             ECS015
00624          GO TO 1410-T-O-X.                                        ECS015
00625                                                                   ECS015
00626      IF DE-ACCOUNT  NOT EQUAL LAST-ACCOUNT                        ECS015
00627          PERFORM 0900-REINCO-SUB-PRINT THRU 0999-RSUB-PRINT-X     ECS015
00628          MOVE S-REINCO-SUB       TO  LAST-REINCO-SUB              ECS015
00629          PERFORM 1000-ACCTS-PRINT THRU 1099-ACCTS-PRINT-X         ECS015
00630          MOVE DE-ACCOUNT         TO  LAST-ACCOUNT                 ECS015
00631      ELSE                                                         ECS015
00632          IF S-REINCO-SUB NOT EQUAL LAST-REINCO-SUB                ECS015
00633              PERFORM 0900-REINCO-SUB-PRINT THRU 0999-RSUB-PRINT-X ECS015
00634              MOVE S-REINCO-SUB   TO  LAST-REINCO-SUB.             ECS015
00635                                                                   ECS015
00636      IF S-REINCO    NOT EQUAL LAST-REINCO   OR                    ECS015
00637         DE-CARRIER  NOT EQUAL LAST-CARRIER  OR                    ECS015
00638         DE-GROUPING NOT EQUAL LAST-GROUPING OR                    ECS015
00639         DE-STATE    NOT EQUAL LAST-STATE                          ECS015
00640         MOVE 'STATE TOTALS'      TO T1-DESC                       ECS015
00641         GO TO 1300-TOTS-OUT.                                      ECS015
00642                                                                   ECS015
00643  0325-SET-DATAL.                                                  ECS015
00644                                                                   ECS015
00645      MOVE SPACES                 TO DATAL1 DATAL2.                ECS015
012609     MOVE SPACES                 TO EXT-CANCEL-DATE.
012609     MOVE SPACES                 TO EXT-CLAIM-NUMBER.
012609     MOVE SPACES                 TO EXT-CLAIM-INCURRED.
012609     MOVE SPACES                 TO EXT-CLAIM-PAID.
012609     MOVE SPACES                 TO HOLD-CANCEL-DATE.
012609     MOVE SPACES                 TO HOLD-CLAIM-NUMBER.
012609     MOVE SPACES                 TO HOLD-CLAIM-INCURRED.
012609     MOVE SPACES                 TO HOLD-CLAIM-PAID.
012609     MOVE ZEROS                  TO EXT-BENEFIT  HOLD-BENEFIT
012609                                  EXT-PREMIUM  HOLD-PREMIUM
012609                                  EXT-COMMISSION HOLD-COMMISSION
012609                                  EXT-CLAIM-AMT  HOLD-CLAIM-AMT.
00646                                                                   ECS015
00647      IF DE-CANCEL     OR                                          ECS015
00648         DE-RC-CANCEL  OR                                          ECS015
00649         DE-RR-RC-CNC                                              ECS015
00650          COMPUTE DE-REI-CNAMT   =  DE-REI-CNAMT   *  -1           ECS015
00651          COMPUTE DE-REI-LFRFND  =  DE-REI-LFRFND  *  -1           ECS015
00652          COMPUTE DE-REI-AHAMT   =  DE-REI-AHAMT   *  -1           ECS015
00653          COMPUTE DE-REI-AHRFND  =  DE-REI-AHRFND  *  -1.          ECS015
00654                                                                   ECS015
00655      MOVE DE-ACCOUNT             TO  D1-ACCT.                     ECS015
00656      MOVE DE-EF-YR               TO  D1-REIN-YR.                  ECS015
00657      MOVE DE-EF-MO               TO  D1-REIN-MO.                  ECS015
00658      MOVE DE-EF-DA               TO  D1-REIN-DA.                  ECS015
00659      MOVE DE-CERT                TO  D1-CERT.                     ECS015
00660      MOVE DE-LNAME               TO  D1-LAST-NAME.                ECS015
00661      MOVE DE-FNAME               TO  D1-INIT-1.                   ECS015
00662      MOVE DE-INIT                TO  D1-INIT-2.                   ECS015
00663      MOVE DE-AGE                 TO  D1-AGE.                      ECS015
00664      MOVE DE-SEX                 TO  D1-SEX.                      ECS015
012609
012609     MOVE DE-CARRIER             TO  EXT-CARRIER.
012609     MOVE DE-STATE               TO  EXT-STATE.
012609     MOVE DE-ACCOUNT             TO  EXT-ACCT.
012609     MOVE S-REIN-COMPANY         TO  EXT-REIN-CO.
012609     MOVE DE-EF-MO               TO  EXT-REIN-MO.
012609     MOVE DE-EF-DA               TO  EXT-REIN-DA.
012609     MOVE DE-EF-CCYY             TO  EXT-REIN-YR.
012609     MOVE DE-CERT                TO  EXT-CERT.
012609     MOVE DE-LNAME               TO  EXT-LAST-NAME.
012609     MOVE DE-1ST-INIT-FNAME      TO  EXT-INIT-1.
012609     MOVE DE-INIT                TO  EXT-INIT-2.
012609     MOVE DE-AGE                 TO  EXT-AGE.
012609     MOVE DE-SEX                 TO  EXT-SEX.
012609
00665      MOVE LIFE-OVERRIDE-L6       TO  D1-KIND.                     ECS015
00666      MOVE   AH-OVERRIDE-L6       TO  D2-KIND.                     ECS015
012609     MOVE LIFE-OVERRIDE-L6       TO  EXT-KIND.
012609     MOVE   AH-OVERRIDE-L6       TO  HOLD-KIND.
00667      MOVE DE-LF-TYPE             TO  CLAS-LOOK.                   ECS015
00668      MOVE CLAS-STARTL            TO  CLAS-INDEXL.                 ECS015
00669      MOVE CLAS-STARTA            TO  CLAS-INDEXA.                 ECS015
00670                                                                   ECS015
00671      IF CLAS-LOOK  =  ZEROS                                       ECS015
00672          MOVE DE-AH-TYPE         TO  CLAS-LOOK                    ECS015
00673          GO TO 0340-FIND-AH-TYPE.                                 ECS015
00674                                                                   ECS015
00675  0330-FIND-LIFE-TYPE.                                             ECS015
00676      IF CLAS-MAXL  = ZEROS                                        ECS015
00677          MOVE 'MAX LIFE-TYPE TABLE ERROR'   TO  WS-ABEND-MESSAGE  ECS015
00678          MOVE 401                TO  WS-RETURN-CODE               ECS015
00679          GO TO ABEND-PGM.                                         ECS015
00680                                                                   ECS015
00681      IF CLAS-INDEXL  GREATER THAN  CLAS-MAXL                      ECS015
00682          MOVE 'INDEX LIFE-TYPE TABLE ERROR' TO  WS-ABEND-MESSAGE  ECS015
00683          MOVE 401                TO  WS-RETURN-CODE               ECS015
00684          GO TO ABEND-PGM.                                         ECS015
00685                                                                   ECS015
00686      IF CLAS-I-BEN (CLAS-INDEXL)  NOT = CLAS-LOOK                 ECS015
00687          ADD +1  TO  CLAS-INDEXL                                  ECS015
00688          GO TO  0330-FIND-LIFE-TYPE.                              ECS015
00689                                                                   ECS015
00690      MOVE CLAS-I-AB3 (CLAS-INDEXL)  TO  D1-TYPE.                  ECS015
012609     MOVE CLAS-I-AB3 (CLAS-INDEXL)  TO  EXT-TYPE.
00691      MOVE DE-AH-TYPE               TO  CLAS-LOOK.                 ECS015
00692                                                                   ECS015
00693  0340-FIND-AH-TYPE.                                               ECS015
00694      IF DE-AH-TYPE  = ZERO                                        ECS015
00695          GO TO 0350-END-AH-LOOK.                                  ECS015
00696                                                                   ECS015
00697      IF CLAS-MAXA  = ZEROS                                        ECS015
00698          MOVE 'MAX A&H-TYPE TABLE ERROR'   TO  WS-ABEND-MESSAGE   ECS015
00699          MOVE 402                TO  WS-RETURN-CODE               ECS015
00700          GO TO ABEND-PGM.                                         ECS015
00701                                                                   ECS015
00702      IF CLAS-INDEXA  GREATER THAN  CLAS-MAXA                      ECS015
00703          MOVE 'INDEX A&H-TYPE TABLE ERROR' TO  WS-ABEND-MESSAGE   ECS015
00704          MOVE 402                TO  WS-RETURN-CODE               ECS015
00705          GO TO ABEND-PGM.                                         ECS015
00706                                                                   ECS015
00707      IF CLAS-I-BEN (CLAS-INDEXA)  NOT =  CLAS-LOOK                ECS015
00708          ADD +1  TO  CLAS-INDEXA                                  ECS015
00709          GO TO 0340-FIND-AH-TYPE.                                 ECS015
00710                                                                   ECS015
00711      MOVE CLAS-I-AB3 (CLAS-INDEXA)  TO  D2-TYPE.                  ECS015
012609     MOVE CLAS-I-AB3 (CLAS-INDEXA)  TO  HOLD-TYPE.
00712                                                                   ECS015
00713  0350-END-AH-LOOK.                                                ECS015
00714                                                                   ECS015
00715      IF DE-LF-TERM IS NOT NUMERIC                                 ECS015
00716          MOVE +0                 TO  DE-LF-TERM.                  ECS015
00717                                                                   ECS015
00718      IF DE-AH-TERM IS NOT NUMERIC                                 ECS015
00719          MOVE +0                 TO  DE-AH-TERM.                  ECS015
00720                                                                   ECS015
00721      IF DE-REI-LFAMT IS NOT NUMERIC                               ECS015
00722          MOVE +0                 TO  DE-REI-LFAMT.                ECS015
00723                                                                   ECS015
00724      IF DE-REI-AHAMT IS NOT NUMERIC                               ECS015
00725          MOVE +0                 TO  DE-REI-AHAMT.                ECS015
00726                                                                   ECS015
00727      IF DE-REI-CNAMT IS NOT NUMERIC                               ECS015
00728          MOVE +0                 TO  DE-REI-CNAMT.                ECS015
00729                                                                   ECS015
00730      MOVE DE-LF-TERM             TO  D1-TERM.                     ECS015
00731      MOVE DE-AH-TERM             TO  D2-TERM.                     ECS015
012609     MOVE DE-LF-TERM             TO  EXT-TERM.
012609     MOVE DE-AH-TERM             TO  HOLD-TERM.
00732                                                                   ECS015
00733      IF DE-ISSUE      OR                                          ECS015
00734         DE-RC-ISSUE   OR                                          ECS015
00735         DE-RR-RC-ISS                                              ECS015
00736          PERFORM 0400-ADD-ISSUE THRU 0499-ADD-ISSUE-X             ECS015
00737          MOVE DE-REI-LFPRM       TO  D1-PREMIUM                   ECS015
00738          MOVE DE-REI-AHPRM       TO  D2-PREMIUM                   ECS015
00739          MOVE DE-REI-LFAMT       TO  D1-BENEFIT                   ECS015
00740          MOVE DE-REI-AHAMT       TO  D2-BENEFIT.                  ECS015
012609     IF DE-ISSUE      OR
012609        DE-RC-ISSUE   OR
012609        DE-RR-RC-ISS
012609         MOVE DE-REI-LFPRM       TO EXT-PREMIUM
012609         MOVE DE-REI-AHPRM       TO HOLD-PREMIUM
012609         MOVE DE-REI-LFAMT       TO EXT-BENEFIT
012609         MOVE DE-REI-AHAMT       TO HOLD-BENEFIT.
00741                                                                   ECS015
00742      IF DE-ISSUE      OR                                          ECS015
00743         DE-RR-RC-ISS                                              ECS015
00744          ADD DE-REI-LFAMT        TO  T-LBEN   S-LBEN              ECS015
00745                                      REIN-LBEN (REIN-IDX)         ECS015
00746          ADD DE-REI-AHAMT        TO  T-ABEN   S-ABEN              ECS015
00747                                      REIN-ABEN (REIN-IDX)         ECS015
00748          ADD DE-REI-LFPRM        TO  T-LPREM  S-LPREM             ECS015
00749                                      REIN-LPREM(REIN-IDX)         ECS015
00750          ADD DE-REI-AHPRM        TO  T-APREM  S-APREM             ECS015
00751                                      REIN-APREM(REIN-IDX).        ECS015
00752                                                                   ECS015
00753      IF DE-CANCEL     OR                                          ECS015
00754         DE-RC-CANCEL  OR                                          ECS015
00755         DE-RR-RC-CNC                                              ECS015
00756          PERFORM 0500-ADD-CANCEL THRU 0599-ADD-CANCEL-X           ECS015
00757          MOVE DE-REI-LFRFND      TO  D1-PREMIUM                   ECS015
00758          MOVE DE-REI-AHRFND      TO  D2-PREMIUM                   ECS015
00759          MOVE DE-REI-CNAMT       TO  D1-BENEFIT                   ECS015
00760          MOVE DE-REI-AHAMT       TO  D2-BENEFIT.                  ECS015
012609     IF DE-CANCEL     OR
012609        DE-RC-CANCEL  OR
012609        DE-RR-RC-CNC
012609         MOVE DE-REI-LFRFND      TO EXT-PREMIUM
012609         MOVE DE-REI-AHRFND      TO HOLD-PREMIUM
012609         MOVE DE-REI-CNAMT       TO EXT-BENEFIT
012609         MOVE DE-REI-AHAMT       TO HOLD-BENEFIT.
00761                                                                   ECS015
00762      IF DE-CANCEL     OR                                          ECS015
00763         DE-RR-RC-CNC                                              ECS015
00764          ADD DE-REI-CNAMT        TO  T-LBEN   S-LBEN              ECS015
00765                                      REIN-LBEN (REIN-IDX)         ECS015
00766          ADD DE-REI-AHAMT        TO  T-ABEN   S-ABEN              ECS015
00767                                      REIN-ABEN (REIN-IDX)         ECS015
00768          ADD DE-REI-LFRFND       TO  T-CANCL  S-CANCL             ECS015
00769                                      REIN-CANCL(REIN-IDX)         ECS015
00770          ADD DE-REI-AHRFND       TO  T-CANCA  S-CANCA             ECS015
00771                                      REIN-CANCA(REIN-IDX).        ECS015
00772                                                                   ECS015
00773      IF DE-ISSUE                                                  ECS015
00774          MOVE 'CEDED '           TO  D1-MES  D2-MES               ECS015
012609         MOVE 'CEDED '           TO  EXT-MES HOLD-MES
00775          GO TO 0360-WRITE-OUT.                                    ECS015
00776                                                                   ECS015
00777      IF DE-CANCEL                                                 ECS015
00778          MOVE 'CANCEL'           TO  D1-MES  D2-MES               ECS015
00779          MOVE DE-LF-CANC-MO      TO  D1-CNC-MO                    ECS015
00780          MOVE DE-LF-CANC-DA      TO  D1-CNC-DA                    ECS015
00781          MOVE DE-LF-CANC-YR      TO  D1-CNC-YR                    ECS015
00782          MOVE '-'                TO  D1-CNC-MOD  D1-CNC-DAD       ECS015
00783          MOVE DE-AH-CANC-MO      TO  D2-CNC-MO                    ECS015
00784          MOVE DE-AH-CANC-DA      TO  D2-CNC-DA                    ECS015
00785          MOVE DE-AH-CANC-YR      TO  D2-CNC-YR                    ECS015
00786          MOVE '-'                TO  D2-CNC-MOD  D2-CNC-DAD       ECS015
012609         MOVE 'CANCEL'           TO  EXT-MES HOLD-MES
012609         IF DE-LF-CANC-DTE > ZERO
012609             MOVE DE-LF-CANC-MO      TO  EXT-CNC-MO
012609             MOVE DE-LF-CANC-DA      TO  EXT-CNC-DA
012609             MOVE DE-LF-CANC-CCYY    TO  EXT-CNC-YR
012609             MOVE '/'                TO  EXT-SL-1 EXT-SL-2
012609         END-IF
012609         IF DE-AH-CANC-DTE > ZERO
012609             MOVE DE-AH-CANC-MO      TO  HOLD-CNC-MO
012609             MOVE DE-AH-CANC-DA      TO  HOLD-CNC-DA
012609             MOVE DE-AH-CANC-CCYY    TO  HOLD-CNC-YR
012609             MOVE '/'                TO  HOLD-SL-1 HOLD-SL-2
012609         END-IF
00787          GO TO 0360-WRITE-OUT.                                    ECS015
00788                                                                   ECS015
00789      IF (DE-RC-ISSUE  OR  DE-RC-CANCEL)  AND                      ECS015
00790         (DET-LCOM  = ZERO  AND                                    ECS015
00791         DET-ACOM   = ZERO)                                        ECS015
00792          GO TO 0310-R-LOOP.                                       ECS015
00793                                                                   ECS015
00794      IF DE-RC-ISSUE   OR                                          ECS015
00795         DE-RC-CANCEL                                              ECS015
00796          MOVE 'RC-COM'           TO  D1-MES  D2-MES               ECS015
012609         MOVE 'RC-COM'           TO  EXT-MES HOLD-MES
00797          GO TO 0360-WRITE-OUT.                                    ECS015
00798                                                                   ECS015
00799      IF DE-RR-RC-ISS                                              ECS015
00800          MOVE 'RC-ISS'           TO  D1-MES  D2-MES               ECS015
012609         MOVE 'RC-ISS'           TO  EXT-MES HOLD-MES
00801          GO TO 0360-WRITE-OUT.                                    ECS015
00802                                                                   ECS015
00803      IF DE-RR-RC-CNC                                              ECS015
00804          MOVE 'RC-CNC'           TO  D1-MES  D2-MES               ECS015
012609         MOVE 'RC-CNC'           TO  EXT-MES HOLD-MES
00805          GO TO 0360-WRITE-OUT.                                    ECS015
00806                                                                   ECS015
00807      MOVE DE-INCUR-MO            TO  D1-I-MO  D2-I-MO.            ECS015
00808      MOVE DE-INCUR-DA            TO  D1-I-DA  D2-I-DA.            ECS015
00809      MOVE DE-INCUR-YR            TO  D1-I-YR  D2-I-YR.            ECS015
012609     IF DE-INCUR NUMERIC
012609         MOVE DE-INCUR-MO        TO  EXT-INC-MO HOLD-INC-MO
012609         MOVE DE-INCUR-DA        TO  EXT-INC-DA HOLD-INC-DA
012609         MOVE DE-INCUR-CCYY      TO  EXT-INC-YR HOLD-INC-YR
012609         MOVE '/'                TO  EXT-SL-3   EXT-SL-4
012609                                     HOLD-SL-3  HOLD-SL-4
012609     END-IF.
00810                                                                   ECS015
00811      MOVE DE-PAY-MO              TO  D1-C-MO  D2-C-MO.            ECS015
00812      MOVE DE-PAY-DA              TO  D1-C-DA  D2-C-DA.            ECS015
00813      MOVE DE-PAY-YR              TO  D1-C-YR  D2-C-YR.            ECS015
012609     IF DE-PAY NUMERIC
012609         MOVE DE-PAY-MO          TO  EXT-PD-MO HOLD-PD-MO
012609         MOVE DE-PAY-DA          TO  EXT-PD-DA HOLD-PD-DA
012609         MOVE DE-PAY-CCYY        TO  EXT-PD-YR HOLD-PD-YR
012609         MOVE '/'                TO  EXT-SL-5   EXT-SL-6
012609                                     HOLD-SL-5  HOLD-SL-6
012609     END-IF.
00814                                                                   ECS015
00815      MOVE  '-'                   TO  D1-I-D1  D1-I-D2             ECS015
00816                                      D1-C-D1  D1-C-D2             ECS015
00817                                      D2-I-D1  D2-I-D2             ECS015
00818                                      D2-C-D1  D2-C-D2.            ECS015
00819                                                                   ECS015
00820      MOVE 'CLAIM'                TO  D1-MES   D2-MES.             ECS015
012609     MOVE 'CLAIM'                TO  EXT-MES  HOLD-MES.
012609     MOVE DE-CNUM                TO  EXT-CLAIM-NUMBER 
012609                                     HOLD-CLAIM-NUMBER.
00821                                                                   ECS015
00822      IF DE-DTH OR DE-OB-DTH                                       ECS015
00823          MOVE DE-REI-CLAIM-AMT   TO  D1-CLAMT                     ECS015
012609         MOVE DE-REI-CLAIM-AMT   TO  EXT-CLAIM-AMT
00824          ADD DE-REI-CLAIM-AMT    TO  T-DEATH  S-DEATH             ECS015
00825                                      REIN-DEATH (REIN-IDX)        ECS015
00826          MOVE ZEROS              TO  DE-AH-TYPE                   ECS015
00827      ELSE                                                         ECS015
00828          MOVE ZEROS              TO  DE-LF-TYPE                   ECS015
00829          MOVE DE-REI-CLAIM-AMT   TO  D2-CLAMT                     ECS015
012609         MOVE DE-REI-CLAIM-AMT   TO  HOLD-CLAIM-AMT
00830          ADD DE-REI-CLAIM-AMT    TO  T-DISAB  S-DISAB             ECS015
00831                                      REIN-DISAB (REIN-IDX).       ECS015
00832                                                                   ECS015
00833      IF DE-RR-RC-CLM                                              ECS015
012609         MOVE 'RC-CLM'           TO  EXT-MES HOLD-MES 
00834          MOVE 'RC-CLM'           TO  D1-MES  D2-MES.              ECS015
00835                                                                   ECS015
00836  0360-WRITE-OUT.                                                  ECS015
00837      IF LINE-COUNT  GREATER THAN  52                              ECS015
00838          PERFORM 0700-PRINT-HEAD-A THRU 0799-PRINT-HEAD-EXIT.     ECS015
00839                                                                   ECS015
00840      MOVE DE-CARRIER             TO  LAST-CARRIER.                ECS015
00841      MOVE DE-GROUPING            TO  LAST-GROUPING.               ECS015
00842                                                                   ECS015
00843      IF DE-LF-TYPE NOT = ZEROS                                    ECS015
00844          MOVE ' '                TO  X                            ECS015
00845          MOVE DATAL1             TO  PRT                          ECS015
00846          PERFORM 0600-PRXX THRU 0699-PRXX-EXIT                    ECS015
012609         WRITE EXT-RECORD FROM EXT-DATAL
00847          IF DE-AH-TYPE NOT = ZEROS                                ECS015
00848              MOVE ' '            TO  X                            ECS015
00849              MOVE DATAL2         TO  PRT                          ECS015
00850              PERFORM 0600-PRXX THRU 0699-PRXX-EXIT                ECS015
00851              ADD 2               TO  LINE-COUNT                   ECS015
012609             PERFORM 0380-PRINT-FROM-HOLD THRU 0380-EXIT
00852          ELSE                                                     ECS015
00853              ADD 1               TO  LINE-COUNT                   ECS015
00854      ELSE                                                         ECS015
00855          IF DE-AH-TYPE NOT = ZEROS                                ECS015
00856              MOVE ' '            TO  X                            ECS015
00857              MOVE D2-BENE-AREA   TO  D1-BENE-AREA                 ECS015
00858              MOVE DATAL1         TO  PRT                          ECS015
00859              PERFORM 0600-PRXX THRU 0699-PRXX-EXIT                ECS015
012609             PERFORM 0380-PRINT-FROM-HOLD THRU 0380-EXIT
00860              ADD 1               TO  LINE-COUNT.                  ECS015
00861                                                                   ECS015
00862      GO TO 0310-R-LOOP.                                           ECS015
00863                                                                   ECS015
00864      EJECT                                                        ECS015
012609
012609 0380-PRINT-FROM-HOLD.
012609      MOVE HOLD-MES            TO  EXT-MES.
012609      MOVE HOLD-KIND           TO  EXT-KIND.
012609      MOVE HOLD-TYPE           TO  EXT-TYPE.
012609      MOVE HOLD-TERM           TO  EXT-TERM.
012609      MOVE HOLD-BENEFIT        TO  EXT-BENEFIT.
012609      MOVE HOLD-PREMIUM        TO  EXT-PREMIUM.
012609      MOVE HOLD-COMMISSION     TO  EXT-COMMISSION.
012609      MOVE HOLD-CANCEL-DATE    TO  EXT-CANCEL-DATE.
012609      MOVE HOLD-CLAIM-NUMBER   TO  EXT-CLAIM-NUMBER.
012609      MOVE HOLD-CLAIM-AMT      TO  EXT-CLAIM-AMT.
012609      MOVE HOLD-CLAIM-INCURRED TO  EXT-CLAIM-INCURRED.
012609      MOVE HOLD-CLAIM-PAID     TO  EXT-CLAIM-PAID.
012609      WRITE EXT-RECORD FROM EXT-DATAL.
012609 0380-EXIT.
012609     EXIT.
00865  0400-ADD-ISSUE.                                                  ECS015
00866      MOVE +1                     TO  SA.                          ECS015
00867      MOVE +0                     TO  DET-LCOM  DET-ACOM.          ECS015
00868                                                                   ECS015
00869  0410-ADD-ISSUE-LOOP.                                             ECS015
00870      IF DE-AGT-TYPE (SA)  = 'W'                                   ECS015
00871          IF DE-AGT-PRIME (SA) NOT = DE-REI-COMP                   ECS015
00872              GO TO 0420-ADD-ISS-INCR.                             ECS015
00873                                                                   ECS015
00874      IF DE-AGT-TYPE (SA)  = ('D' OR 'R' OR 'W' OR                 ECS015
011410                             'P' OR 'T' OR 'M')
00876          COMPUTE DET-LCOM  ROUNDED  =  DET-LCOM  +                ECS015
00877                          (DE-REI-LFPRM  *  DE-L-PC (SA))          ECS015
00878          COMPUTE DET-ACOM  ROUNDED  =  DET-ACOM  +                ECS015
00879                          (DE-REI-AHPRM  *  DE-A-PC (SA))
100703     ELSE
100703        IF DE-AGT-TYPE (SA) = 'K'
100703           COMPUTE DET-LCOM = DET-LCOM +
100703              DE-L-PC (SA) * +1000
100703           COMPUTE DET-ACOM = DET-ACOM +
100703              DE-A-PC (SA) * +1000
100703        END-IF
100703     END-IF
100703     .
00880                                                                   ECS015
00881  0420-ADD-ISS-INCR.                                               ECS015
00882      ADD +1  TO  SA.                                              ECS015
00883                                                                   ECS015
00884      IF SA  LESS THAN  +11                                        ECS015
00885          GO TO 0410-ADD-ISSUE-LOOP.                               ECS015
00886                                                                   ECS015
00887      MOVE DET-LCOM               TO  D1-COMMISSION.               ECS015
00888      MOVE DET-ACOM               TO  D2-COMMISSION.               ECS015
012609     MOVE DET-LCOM               TO  EXT-COMMISSION.
012609     MOVE DET-ACOM               TO  HOLD-COMMISSION.
00889      ADD DET-LCOM                TO  T-LCOM  S-LCOM               ECS015
00890                                      REIN-LCOM(REIN-IDX).         ECS015
00891      ADD DET-ACOM                TO  T-ACOM  S-ACOM               ECS015
00892                                      REIN-ACOM(REIN-IDX).         ECS015
00893                                                                   ECS015
00894  0499-ADD-ISSUE-X.                                                ECS015
00895      EXIT.                                                        ECS015
00896                                                                   ECS015
00897      EJECT                                                        ECS015
00898  0500-ADD-CANCEL.                                                 ECS015
00899      MOVE +1                     TO  SA.                          ECS015
00900      MOVE +0                     TO  DET-LCOM  DET-ACOM.          ECS015
00901                                                                   ECS015
00902  0510-ADD-CANCEL-LOOP.                                            ECS015
00903      IF DE-AGT-TYPE (SA)  = 'W'                                   ECS015
00904          IF DE-AGT-PRIME (SA) NOT = DE-REI-COMP                   ECS015
00905              GO TO 0520-ADD-CANCEL-INCR.                          ECS015
00906                                                                   ECS015
00907      IF DE-AGT-TYPE (SA)  = ('D' OR 'R' OR 'W' OR                 ECS015
011410                             'P' OR 'T' OR 'M')
00909          COMPUTE DET-LCOM ROUNDED  =  DET-LCOM  +                 ECS015
00910                          (DE-REI-LFRFND  *  DE-L-PC (SA))         ECS015
00911          COMPUTE DET-ACOM ROUNDED  =  DET-ACOM  +                 ECS015
00912                          (DE-REI-AHRFND  *  DE-A-PC (SA))
           ELSE
100703        IF DE-AGT-TYPE (SA) = 'K'
100703           COMPUTE DET-LCOM = DET-LCOM +
100703              DE-L-PC (SA) * -1000
100703           COMPUTE DET-ACOM = DET-ACOM +
100703              DE-A-PC (SA) * -1000
100703        END-IF
100703     END-IF
100703     .
00913                                                                   ECS015
00914  0520-ADD-CANCEL-INCR.                                            ECS015
00915      ADD +1  TO  SA.                                              ECS015
00916                                                                   ECS015
00917      IF SA  LESS THAN  +11                                        ECS015
00918          GO TO 0510-ADD-CANCEL-LOOP.                              ECS015
00919                                                                   ECS015
00920      MOVE DET-LCOM               TO  D1-COMMISSION.               ECS015
00921      MOVE DET-ACOM               TO  D2-COMMISSION.               ECS015
012609     MOVE DET-LCOM               TO  EXT-COMMISSION.
012609     MOVE DET-ACOM               TO  HOLD-COMMISSION.
00922      ADD DET-LCOM                TO  T-LCOM  S-LCOM               ECS015
00923                                      REIN-LCOM(REIN-IDX).         ECS015
00924      ADD DET-ACOM                TO  T-ACOM  S-ACOM               ECS015
00925                                      REIN-ACOM(REIN-IDX).         ECS015
00926                                                                   ECS015
00927  0599-ADD-CANCEL-X.                                               ECS015
00928      EXIT.                                                        ECS015
00929                                                                   ECS015
00930  EJECT                                                            ECS015
00931 ***************************************                           ECS015
00932 *                                     *                           ECS015
00933 *     PRINT   ROUTINE                 *                           ECS015
00934 *                                     *                           ECS015
00935 ***************************************                           ECS015
00936  0600-PRXX.                                                       ECS015
00937                              COPY ELCPRT2.                        ECS015
00938                                                                   ECS015
00939  0699-PRXX-EXIT.                                                  ECS015
00940      EXIT.                                                        ECS015
00941                                                                   ECS015
00942  EJECT                                                            ECS015
00943 ***************************************                           ECS015
00944 *                                     *                           ECS015
00945 *      HEADING  ROUTINES              *                           ECS015
00946 *                                     *                           ECS015
00947 ***************************************                           ECS015
00948  0700-PRINT-HEAD-A.                                               ECS015
00949                                                                   ECS015
00950      ADD 1                       TO  PAGER.                       ECS015
00951      MOVE PAGER                  TO  PGNO.                        ECS015
00952      MOVE '1'                    TO  X.                           ECS015
00953      MOVE HEAD-A                 TO  PRT.                         ECS015
00954      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
00955                                                                   ECS015
00956      MOVE ' '                    TO  X.                           ECS015
00957      MOVE HEAD-B                 TO  PRT.                         ECS015
00958      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
00959                                                                   ECS015
00960      MOVE HEAD-C                TO  PRT.                          ECS015
00961      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
00962                                                                   ECS015
00963  0730-PRINT-HEAD-D.                                               ECS015
00964                                                                   ECS015
00965      MOVE '0'                    TO  X.                           ECS015
00966      MOVE HEAD-D                 TO  PRT.                         ECS015
00967      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
00968                                                                   ECS015
00969      MOVE ' '                    TO  X.                           ECS015
00970                                                                   ECS015
00971      IF HE-1  NOT =  SPACES                                       ECS015
00972          MOVE HEAD-E            TO  PRT                           ECS015
00973          PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                   ECS015
00974                                                                   ECS015
00975  0740-PRINT-HEAD-F.                                               ECS015
00976                                                                   ECS015
00977      MOVE HEAD-F                TO  PRT.                          ECS015
00978      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
00979                                                                   ECS015
00980  0750-PRINT-HEAD-G.                                               ECS015
00981                                                                   ECS015
00982      IF LAST-GROUPING = ZEROS                                     ECS015
00983          GO TO 0760-PRINT-HEAD-H.                                 ECS015
00984                                                                   ECS015
00985      MOVE ' '                    TO  X.                           ECS015
00986      MOVE HEAD-G                 TO  PRT.                         ECS015
00987      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
00988                                                                   ECS015
00989  0760-PRINT-HEAD-H.                                               ECS015
00990                                                                   ECS015
00991      MOVE ' '                    TO  X.                           ECS015
00992      MOVE HEAD-H                 TO  PRT.                         ECS015
00993      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
00994                                                                   ECS015
00995  0770-PRINT-HEAD-I.                                               ECS015
00996                                                                   ECS015
00997      MOVE '0'                    TO  X.                           ECS015
00998      MOVE HEAD-I                 TO  PRT.                         ECS015
00999      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01000                                                                   ECS015
01001      MOVE ' '                    TO  X.                           ECS015
01002      MOVE HEAD-J                 TO  PRT.                         ECS015
01003      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01004                                                                   ECS015
01005      MOVE SPACES                 TO PRT.                          ECS015
01006                                                                   ECS015
01007      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01008                                                                   ECS015
01009      MOVE 13 TO LINE-COUNT.                                       ECS015
01010                                                                   ECS015
01011  0799-PRINT-HEAD-EXIT.                                            ECS015
01012      EXIT.                                                        ECS015
01013  EJECT                                                            ECS015
01014 ***************************************                           ECS015
01015 *                                     *                           ECS015
01016 *   REIN. SUB  BREAK  ROUTINE         *                           ECS015
01017 *                                     *                           ECS015
01018 ***************************************                           ECS015
01019  0900-REINCO-SUB-PRINT.                                           ECS015
01020                                                                   ECS015
01021      ADD S-LPREM                 TO  A-LPREM.                     ECS015
01022      ADD S-APREM                 TO  A-APREM.                     ECS015
01023      ADD S-LBEN                  TO  A-LBEN.                      ECS015
01024      ADD S-ABEN                  TO  A-ABEN.                      ECS015
01025      ADD S-CANCL                 TO  A-CANCL.                     ECS015
01026      ADD S-CANCA                 TO  A-CANCA.                     ECS015
01027      ADD S-DISAB                 TO  A-DISAB.                     ECS015
01028      ADD S-DEATH                 TO  A-DEATH.                     ECS015
01029      ADD S-LCOM                  TO  A-LCOM.                      ECS015
01030      ADD S-ACOM                  TO  A-ACOM.                      ECS015
01031                                                                   ECS015
01032      IF REIN-SUB-WORK-AREAS EQUAL ZERO-AREAS                      ECS015
01033          GO TO 0999-RSUB-PRINT-X.                                 ECS015
01034                                                                   ECS015
01035      ADD +4                      TO  LINE-COUNT.                  ECS015
01036      MOVE SPACES                 TO  TOT-1  TOT-2.                ECS015
01037      MOVE '*** REIN. SUB '       TO  T1-DESC-1.                   ECS015
01038      MOVE LAST-REINCO-SUB        TO  T1-REINCO-SUB.               ECS015
01039      MOVE ' TOTALS ***'          TO  T1-DESC-2.                   ECS015
01040      MOVE LIFE-OVERRIDE-L6       TO  T1-KIND.                     ECS015
01041      MOVE   AH-OVERRIDE-L6       TO  T2-KIND.                     ECS015
01042      MOVE S-LBEN                 TO  T1-BENEFIT.                  ECS015
01043      MOVE S-ABEN                 TO  T2-BENEFIT.                  ECS015
01044      ADD  S-LPREM  S-CANCL       GIVING  T1-PREMIUM.              ECS015
01045      ADD  S-APREM  S-CANCA       GIVING  T2-PREMIUM.              ECS015
01046      MOVE S-LCOM                 TO  T1-COMMISSION.               ECS015
01047      MOVE S-ACOM                 TO  T2-COMMISSION.               ECS015
01048      MOVE S-DEATH                TO  T1-CLAIMS.                   ECS015
01049      MOVE S-DISAB                TO  T2-CLAIMS.                   ECS015
01050      MOVE TOT-1                  TO  PRT.                         ECS015
01051      MOVE '0'                    TO  X.                           ECS015
01052      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01053      MOVE TOT-2                  TO  PRT.                         ECS015
01054      MOVE ' '                    TO  X.                           ECS015
01055      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01056      MOVE SPACES                 TO  PRT.                         ECS015
01057      MOVE ' '                    TO  X.                           ECS015
01058      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01059                                                                   ECS015
01060      MOVE ZERO-AREAS             TO REIN-SUB-WORK-AREAS.          ECS015
01061                                                                   ECS015
01062  0999-RSUB-PRINT-X.                                               ECS015
01063      EXIT.                                                        ECS015
01064                                                                   ECS015
01065  EJECT                                                            ECS015
01066 ***************************************                           ECS015
01067 *     ACCOUNT  BREAK  ROUTINE         *                           ECS015
01068 ***************************************                           ECS015
01069  1000-ACCTS-PRINT.                                                ECS015
01070                                                                   ECS015
01071      IF ACCOUNT-ACCUMS EQUAL ZERO-AREAS                           ECS015
01072          GO TO 1099-ACCTS-PRINT-X.                                ECS015
01073                                                                   ECS015
01074      ADD +4                      TO  LINE-COUNT.                  ECS015
01075      MOVE SPACES                 TO  TOT-1  TOT-2.                ECS015
01076      MOVE '*** ACCOUNT TOTALS ***'   TO  T1-DESC.                 ECS015
01077      MOVE LIFE-OVERRIDE-L6       TO  T1-KIND.                     ECS015
01078      MOVE   AH-OVERRIDE-L6       TO  T2-KIND.                     ECS015
01079      MOVE A-LBEN                 TO  T1-BENEFIT.                  ECS015
01080      MOVE A-ABEN                 TO  T2-BENEFIT.                  ECS015
01081      ADD  A-LPREM  A-CANCL       GIVING  T1-PREMIUM.              ECS015
01082      ADD  A-APREM  A-CANCA       GIVING  T2-PREMIUM.              ECS015
01083      MOVE A-LCOM                 TO  T1-COMMISSION.               ECS015
01084      MOVE A-ACOM                 TO  T2-COMMISSION.               ECS015
01085      MOVE A-DEATH                TO  T1-CLAIMS.                   ECS015
01086      MOVE A-DISAB                TO  T2-CLAIMS.                   ECS015
01087      MOVE TOT-1                  TO  PRT.                         ECS015
01088      MOVE '0'                    TO  X.                           ECS015
01089      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01090      MOVE TOT-2                  TO  PRT.                         ECS015
01091      MOVE ' '                    TO  X.                           ECS015
01092      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01093      MOVE SPACES                 TO  PRT.                         ECS015
01094      MOVE ' '                    TO  X.                           ECS015
01095      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01096                                                                   ECS015
01097      MOVE ZERO-AREAS             TO ACCOUNT-ACCUMS.               ECS015
01098                                                                   ECS015
01099  1099-ACCTS-PRINT-X.                                              ECS015
01100      EXIT.                                                        ECS015
01101  EJECT                                                            ECS015
01102 ***************************************                           ECS015
01103 *   ROUTINE TO SET UP THE HEADINGS    *                           ECS015
01104 ***************************************                           ECS015
01105  1100-HEADS-OUT.                                                  ECS015
01106      MOVE LAST-REINCO            TO  HD-1.                        ECS015
01107      MOVE LAST-REINCO            TO  REIN-SRCH-CO.                ECS015
01108                                                                   ECS015
01109      IF DTE-CLIENT EQUAL 'MTL'                                    ECS015
01110          MOVE LAST-REINCO-SUB    TO  REIN-SRCH-SUB                ECS015
01111      ELSE                                                         ECS015
01112          MOVE LOW-VALUES         TO  REIN-SRCH-SUB.               ECS015
01113                                                                   ECS015
01114      MOVE REIN-SEARCH            TO  RE-KEY.                      ECS015
01115                                                                   ECS015
01116      PERFORM 1200-READ-REIN-TBL THRU 1299-READ-REIN-TBL-X.        ECS015
01117                                                                   ECS015
01118      MOVE REIN-NAME              TO  HD-2.                        ECS015
01119      MOVE CEDE-NAME              TO  HE-1.                        ECS015
01120                                                                   ECS015
01121  1140-BUILD-CARR-HDR.                                             ECS015
01122      MOVE LAST-CARRIER           TO  HF-1.                        ECS015
01123      MOVE ZERO                   TO CLAS-INDEXCN.                 ECS015
01124                                                                   ECS015
01125  CARRIER--LOOP.                                                   ECS015
01126      ADD 1 TO CLAS-INDEXCN.                                       ECS015
01127                                                                   ECS015
01128      IF CLAS-INDEXCN IS GREATER THAN CLAS-MAXCN                   ECS015
01129          MOVE SPACE              TO HF-2                          ECS015
01130          GO TO CARRIER--EXIT.                                     ECS015
01131                                                                   ECS015
01132      IF LAST-CARRIER NOT = CARRIER-SUB (CLAS-INDEXCN)             ECS015
01133          GO TO CARRIER--LOOP.                                     ECS015
01134                                                                   ECS015
01135      MOVE CARRIER-PIC (CLAS-INDEXCN)         TO HF-2.             ECS015
01136                                                                   ECS015
01137  CARRIER--EXIT.                                                   ECS015
01138      EXIT.                                                        ECS015
01139                                                                   ECS015
01140  1150-BUILD-GROUP-HDR.                                            ECS015
01141                                                                   ECS015
01142      MOVE LAST-GROUPING          TO  HG-1.                        ECS015
01143                                                                   ECS015
01144  1160-BUILD-STATE-HDR.                                            ECS015
01145                                                                   ECS015
01146      MOVE LAST-STATE             TO  STATE-L.                     ECS015
01147      MOVE LAST-STATE             TO  HH-1.                        ECS015
01148                                                                   ECS015
01149                              COPY ECSSTLOK.                       ECS015
01150                                                                   ECS015
01151      IF STATE-L = SPACES                                          ECS015
01152          MOVE SPACES                   TO  HH-2                   ECS015
01153      ELSE                                                         ECS015
01154          MOVE STATE-PIC (CLAS-INDEXS)  TO  HH-2.                  ECS015
01155                                                                   ECS015
01156      PERFORM 0700-PRINT-HEAD-A THRU 0799-PRINT-HEAD-EXIT.         ECS015
01157                                                                   ECS015
01158  1199-HEADS-OUT-X.                                                ECS015
01159      EXIT.                                                        ECS015
01160                                                                   ECS015
01161  EJECT                                                            ECS015
01162 ***************************************                           ECS015
01163 *   ROUTINE TO READ THE REIN TBL FILE *                           ECS015
01164 ***************************************                           ECS015
01165  1200-READ-REIN-TBL.                                              ECS015
01166                                                                   ECS015
01167      IF REIN-SEARCH  =  LAST-REIN-SEARCH                          ECS015
01168          GO TO 1299-READ-REIN-TBL-X.                              ECS015
01169                                                                   ECS015
01170      MOVE REIN-SEARCH            TO  LAST-REIN-SEARCH.            ECS015
01171                                                                   ECS015
01172      START RTBL-FILE KEY IS NOT LESS THAN RE-CONTROL-PRIMARY.     ECS015
01173                                                                   ECS015
01174      IF REIN-FILE-STATUS  = '10' OR '23'                          ECS015
01175          MOVE SPACES             TO  REIN-NAME                    ECS015
01176          MOVE SPACES             TO  CEDE-NAME                    ECS015
01177          GO TO 1299-READ-REIN-TBL-X.                              ECS015
01178                                                                   ECS015
01179      IF REIN-FILE-STATUS  NOT =  '00'                             ECS015
01180          MOVE 'START ERROR - REIN'   TO  WS-ABEND-MESSAGE         ECS015
01181          MOVE REIN-FILE-STATUS       TO  WS-ABEND-FILE-STATUS     ECS015
01182          GO TO ABEND-PGM.                                         ECS015
01183                                                                   ECS015
01184      READ RTBL-FILE NEXT.                                         ECS015
01185                                                                   ECS015
01186      IF REIN-FILE-STATUS  = '10' OR '23'                          ECS015
01187          MOVE SPACES             TO  REIN-NAME                    ECS015
01188          MOVE SPACES             TO  CEDE-NAME                    ECS015
01189          GO TO 1299-READ-REIN-TBL-X.                              ECS015
01190                                                                   ECS015
01191      IF REIN-FILE-STATUS  NOT = '00'                              ECS015
01192          MOVE 'READ ERROR - REIN'    TO  WS-ABEND-MESSAGE         ECS015
01193          MOVE REIN-FILE-STATUS       TO  WS-ABEND-FILE-STATUS     ECS015
01194          GO TO ABEND-PGM.                                         ECS015
01195                                                                   ECS015
01196      IF RE-COMP-PRIME = REIN-SRCH-CO                              ECS015
01197          MOVE RE-NAME            TO  REIN-NAME                    ECS015
01198          MOVE RE-CEDE-NAME       TO  CEDE-NAME                    ECS015
01199      ELSE                                                         ECS015
01200          MOVE SPACES             TO  REIN-NAME                    ECS015
01201          MOVE SPACES             TO  CEDE-NAME.                   ECS015
01202                                                                   ECS015
01203  1299-READ-REIN-TBL-X.                                            ECS015
01204      EXIT.                                                        ECS015
01205                                                                   ECS015
01206  EJECT                                                            ECS015
01207 ***************************************                           ECS015
01208 *                                     *                           ECS015
01209 *   ROUTINE TO PRINT THE TOTALS       *                           ECS015
01210 *                                     *                           ECS015
01211 ***************************************                           ECS015
01212  1300-TOTS-OUT.                                                   ECS015
01213                                                                   ECS015
01214      ADD T-LPREM                 TO  G-LPREM.                     ECS015
01215      ADD T-APREM                 TO  G-APREM.                     ECS015
01216      ADD T-LBEN                  TO  G-LBEN.                      ECS015
01217      ADD T-ABEN                  TO  G-ABEN.                      ECS015
01218      ADD T-CANCL                 TO  G-CANCL.                     ECS015
01219      ADD T-CANCA                 TO  G-CANCA.                     ECS015
01220      ADD T-DISAB                 TO  G-DISAB.                     ECS015
01221      ADD T-DEATH                 TO  G-DEATH.                     ECS015
01222      ADD T-LCOM                  TO  G-LCOM.                      ECS015
01223      ADD T-ACOM                  TO  G-ACOM.                      ECS015
01224                                                                   ECS015
01225      IF LINE-COUNT  GREATER THAN  52                              ECS015
01226          PERFORM 0700-PRINT-HEAD-A THRU 0799-PRINT-HEAD-EXIT.     ECS015
01227                                                                   ECS015
01228  1310-TOTS-OUT.                                                   ECS015
01229                                                                   ECS015
01230      MOVE LIFE-OVERRIDE-L6       TO  T1-KIND.                     ECS015
01231      MOVE   AH-OVERRIDE-L6       TO  T2-KIND.                     ECS015
01232      MOVE T-LBEN                 TO  T1-BENEFIT.                  ECS015
01233      MOVE T-ABEN                 TO  T2-BENEFIT.                  ECS015
01234      ADD  T-LPREM  T-CANCL       GIVING T1-PREMIUM.               ECS015
01235      ADD  T-APREM  T-CANCA       GIVING T2-PREMIUM.               ECS015
01236      MOVE T-LCOM                 TO  T1-COMMISSION.               ECS015
01237      MOVE T-ACOM                 TO  T2-COMMISSION.               ECS015
01238      MOVE T-DEATH                TO  T1-CLAIMS.                   ECS015
01239      MOVE T-DISAB                TO  T2-CLAIMS.                   ECS015
01240      MOVE '0'                    TO  X.                           ECS015
01241      MOVE TOT-1                  TO  PRT.                         ECS015
01242      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01243      MOVE SPACES                 TO  TOT-1.                       ECS015
01244      MOVE ' '                    TO  X.                           ECS015
01245      MOVE TOT-2                  TO  PRT.                         ECS015
01246      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01247                                                                   ECS015
01248      MOVE SPACES                 TO  TOT-1.                       ECS015
01249      MOVE 'TOTAL ISSUED'     TO  T1-DESC.                         ECS015
01250                                                                   ECS015
01251      ADD  T-LPREM  T-APREM       GIVING  T1-BENEFIT.              ECS015
01252                                                                   ECS015
01253      MOVE '0'                    TO  X.                           ECS015
01254      MOVE TOT-1                  TO  PRT.                         ECS015
01255                                                                   ECS015
01256      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01257                                                                   ECS015
01258      MOVE 'LESS CANCELLATIONS'   TO  T1-DESC.                     ECS015
01259                                                                   ECS015
01260      COMPUTE T1-BENEFIT  =  (T-CANCL  +  T-CANCA)  *  -1.         ECS015
01261                                                                   ECS015
01262      MOVE TOT-1                  TO  PRT.                         ECS015
01263      MOVE ' '                    TO  X.                           ECS015
01264                                                                   ECS015
01265      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01266                                                                   ECS015
01267      MOVE 'LESS COMMISSIONS'     TO  T1-DESC.                     ECS015
01268                                                                   ECS015
01269      ADD  T-LCOM  T-ACOM         GIVING  T1-BENEFIT.              ECS015
01270                                                                   ECS015
01271      MOVE TOT-1                  TO  PRT.                         ECS015
01272                                                                   ECS015
01273      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01274                                                                   ECS015
01275      IF T-DISAB NOT = ZERO                                        ECS015
01276          MOVE   AH-OVERRIDE-L6      TO  CD-OVRD                   ECS015
01277          MOVE WS-CLAIM-DESC         TO  T1-DESC                   ECS015
01278          MOVE T-DISAB               TO  T1-BENEFIT                ECS015
01279          MOVE TOT-1                 TO  PRT                       ECS015
01280          PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                   ECS015
01281                                                                   ECS015
01282      IF T-DEATH NOT = ZERO                                        ECS015
01283          MOVE LIFE-OVERRIDE-L6      TO  CD-OVRD                   ECS015
01284          MOVE WS-CLAIM-DESC         TO  T1-DESC                   ECS015
01285          MOVE T-DEATH               TO  T1-BENEFIT                ECS015
01286          MOVE TOT-1                 TO  PRT                       ECS015
01287          PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                   ECS015
01288                                                                   ECS015
01289      MOVE 'NET TOTAL'            TO  T1-DESC.                     ECS015
01290                                                                   ECS015
01291      COMPUTE T1-BENEFIT  =  T-LPREM  +  T-APREM  +  T-CANCL       ECS015
01292                          +  T-CANCA  -  T-LCOM   -  T-ACOM        ECS015
01293                          -  T-DISAB  -  T-DEATH.                  ECS015
01294                                                                   ECS015
01295      MOVE TOT-1                  TO  PRT.                         ECS015
01296      MOVE '0'                    TO  X.                           ECS015
01297                                                                   ECS015
01298      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01299                                                                   ECS015
01300  1399-TOTS-OUT-X.                                                 ECS015
01301      EXIT.                                                        ECS015
01302                                                                   ECS015
01303  1400-T-GP-TST.                                                   ECS015
01304                                                                   ECS015
01305      IF LAST-REINCO     NOT = S-REINCO   OR                       ECS015
01306         LAST-CARRIER    NOT = DE-CARRIER OR                       ECS015
01307         LAST-GROUPING   NOT = DE-GROUPING                         ECS015
01308          GO TO 1430-GROUP-TOT-OUT.                                ECS015
01309                                                                   ECS015
01310  1410-T-O-X.                                                      ECS015
01311                                                                   ECS015
01312      MOVE DE-STATE               TO  W-ST.                        ECS015
01313      MOVE 99                     TO  LINE-COUNT.                  ECS015
01314      MOVE ' '                    TO  FIRST-TIME-THRU-SW.          ECS015
01315      MOVE S-REINCO               TO  LAST-REINCO.                 ECS015
01316      MOVE DE-CARRIER             TO  LAST-CARRIER.                ECS015
01317      MOVE DE-GROUPING            TO  LAST-GROUPING.               ECS015
01318      MOVE DE-STATE               TO  LAST-STATE.                  ECS015
01319      MOVE DE-ACCOUNT             TO  LAST-ACCOUNT.                ECS015
01320      MOVE S-REINCO-SUB           TO  LAST-REINCO-SUB.             ECS015
01321      MOVE ZERO-AREAS             TO  ACCUM-WORK-AREAS             ECS015
01322                                      ACCOUNT-ACCUMS               ECS015
01323                                      REIN-SUB-WORK-AREAS.         ECS015
01324                                                                   ECS015
01325      PERFORM 1100-HEADS-OUT THRU 1199-HEADS-OUT-X.                ECS015
01326                                                                   ECS015
01327      GO TO 0325-SET-DATAL.                                        ECS015
01328                                                                   ECS015
01329  EJECT                                                            ECS015
01330 ***************************************                           ECS015
01331 *                                     *                           ECS015
01332 *          GROUPING     BREAK ROUTINE *                           ECS015
01333 *                                     *                           ECS015
01334 ***************************************                           ECS015
01335  1430-GROUP-TOT-OUT.                                              ECS015
01336                                                                   ECS015
01337      IF LAST-GROUPING NOT = ZEROS                                 ECS015
01338          PERFORM 0700-PRINT-HEAD-A THRU 0750-PRINT-HEAD-G         ECS015
01339          PERFORM 0770-PRINT-HEAD-I THRU 0799-PRINT-HEAD-EXIT.     ECS015
01340                                                                   ECS015
01341      MOVE GROUP-WORK-AREAS       TO  ACCUM-WORK-AREAS.            ECS015
01342                                                                   ECS015
01343      ADD G-LPREM                 TO  C-LPREM.                     ECS015
01344      ADD G-APREM                 TO  C-APREM.                     ECS015
01345      ADD G-LBEN                  TO  C-LBEN.                      ECS015
01346      ADD G-ABEN                  TO  C-ABEN.                      ECS015
01347      ADD G-CANCL                 TO  C-CANCL.                     ECS015
01348      ADD G-DEATH                 TO  C-DEATH.                     ECS015
01349      ADD G-DISAB                 TO  C-DISAB.                     ECS015
01350      ADD G-CANCA                 TO  C-CANCA.                     ECS015
01351      ADD G-LCOM                  TO  C-LCOM.                      ECS015
01352      ADD G-ACOM                  TO  C-ACOM.                      ECS015
01353                                                                   ECS015
01354      MOVE 'GROUPING TOTALS'      TO T1-DESC.                      ECS015
01355                                                                   ECS015
01356      IF LAST-GROUPING NOT = ZEROS                                 ECS015
01357          PERFORM 1310-TOTS-OUT THRU 1399-TOTS-OUT-X.              ECS015
01358                                                                   ECS015
01359      MOVE ZERO-AREAS             TO GROUP-WORK-AREAS.             ECS015
01360                                                                   ECS015
01361  1434-GROUP-TOT-OUT-X.                                            ECS015
01362      EXIT.                                                        ECS015
01363                                                                   ECS015
01364  1435-CHK-CARRIER.                                                ECS015
01365      IF LAST-REINCO   NOT = S-REINCO OR                           ECS015
01366         LAST-CARRIER  NOT = DE-CARRIER                            ECS015
01367          GO TO 1500-CARRIER-BREAK.                                ECS015
01368                                                                   ECS015
01369      GO TO 1410-T-O-X.                                            ECS015
01370                                                                   ECS015
01371  EJECT                                                            ECS015
01372 ***************************************                           ECS015
01373 *      CARRIER BREAK ROUTINE          *                           ECS015
01374 ***************************************                           ECS015
01375  1500-CARRIER-BREAK.                                              ECS015
01376                                                                   ECS015
01377      PERFORM 0700-PRINT-HEAD-A THRU 0740-PRINT-HEAD-F.            ECS015
01378      PERFORM 0770-PRINT-HEAD-I THRU 0799-PRINT-HEAD-EXIT.         ECS015
01379                                                                   ECS015
01380      MOVE CARR-WORK-AREAS        TO  ACCUM-WORK-AREAS.            ECS015
01381                                                                   ECS015
01382      ADD C-LPREM                 TO  R-LPREM.                     ECS015
01383      ADD C-APREM                 TO  R-APREM.                     ECS015
01384      ADD C-LBEN                  TO  R-LBEN.                      ECS015
01385      ADD C-ABEN                  TO  R-ABEN.                      ECS015
01386      ADD C-CANCL                 TO  R-CANCL.                     ECS015
01387      ADD C-DEATH                 TO  R-DEATH.                     ECS015
01388      ADD C-DISAB                 TO  R-DISAB.                     ECS015
01389      ADD C-CANCA                 TO  R-CANCA.                     ECS015
01390      ADD C-LCOM                  TO  R-LCOM.                      ECS015
01391      ADD C-ACOM                  TO  R-ACOM.                      ECS015
01392                                                                   ECS015
01393      MOVE 'CARRIER TOTALS'       TO T1-DESC.                      ECS015
01394                                                                   ECS015
01395      PERFORM 1310-TOTS-OUT THRU 1399-TOTS-OUT-X.                  ECS015
01396                                                                   ECS015
01397      MOVE ZERO-AREAS             TO CARR-WORK-AREAS.              ECS015
01398                                                                   ECS015
01399  1589-CARRIER-BREAK-X.                                            ECS015
01400      EXIT.                                                        ECS015
01401                                                                   ECS015
01402  1690-CHK-REIN-CO.                                                ECS015
01403                                                                   ECS015
01404      IF LAST-REINCO NOT = S-REINCO                                ECS015
01405          GO TO 1700-REIN-CO-BREAK.                                ECS015
01406                                                                   ECS015
01407      GO TO 1410-T-O-X.                                            ECS015
01408                                                                   ECS015
01409  EJECT                                                            ECS015
01410 ***************************************                           ECS015
01411 *                                     *                           ECS015
01412 * REINSURANCE COMPANY BREAK ROUTINE   *                           ECS015
01413 *                                     *                           ECS015
01414 ***************************************                           ECS015
01415  1700-REIN-CO-BREAK.                                              ECS015
01416                                                                   ECS015
01417      PERFORM 0700-PRINT-HEAD-A THRU 0730-PRINT-HEAD-D.            ECS015
01418      PERFORM 0770-PRINT-HEAD-I THRU 0799-PRINT-HEAD-EXIT.         ECS015
01419                                                                   ECS015
01420      MOVE REINS-WORK-AREAS       TO  ACCUM-WORK-AREAS.            ECS015
01421                                                                   ECS015
01422      ADD R-LPREM                 TO  F-LPREM.                     ECS015
01423      ADD R-APREM                 TO  F-APREM.                     ECS015
01424      ADD R-LBEN                  TO  F-LBEN.                      ECS015
01425      ADD R-ABEN                  TO  F-ABEN.                      ECS015
01426      ADD R-CANCL                 TO  F-CANCL.                     ECS015
01427      ADD R-DEATH                 TO  F-DEATH.                     ECS015
01428      ADD R-DISAB                 TO  F-DISAB.                     ECS015
01429      ADD R-CANCA                 TO  F-CANCA.                     ECS015
01430      ADD R-LCOM                  TO  F-LCOM.                      ECS015
01431      ADD R-ACOM                  TO  F-ACOM.                      ECS015
01432                                                                   ECS015
01433      MOVE 'REIN. COMPANY TOTALS'         TO T1-DESC.              ECS015
01434                                                                   ECS015
01435      PERFORM 1310-TOTS-OUT THRU 1399-TOTS-OUT-X.                  ECS015
01436                                                                   ECS015
01437      MOVE ZERO-AREAS             TO REINS-WORK-AREAS.             ECS015
01438                                                                   ECS015
01439  1789-REIN-CO-BREAK-X.                                            ECS015
01440      EXIT.                                                        ECS015
01441                                                                   ECS015
01442  1790-G-T-O-X.                                                    ECS015
01443                                                                   ECS015
01444      GO TO 1410-T-O-X.                                            ECS015
01445                                                                   ECS015
01446  EJECT                                                            ECS015
01447 ***************************************                           ECS015
01448 *                                     *                           ECS015
01449 *   FINAL TOTALS ROUTINE              *                           ECS015
01450 *                                     *                           ECS015
01451 ***************************************                           ECS015
01452                                                                   ECS015
01453  1800-FINAL-TOT-SUB.                                              ECS015
01454                                                                   ECS015
01455      PERFORM 0700-PRINT-HEAD-A.                                   ECS015
01456      PERFORM 0770-PRINT-HEAD-I THRU 0799-PRINT-HEAD-EXIT.         ECS015
01457                                                                   ECS015
01458      MOVE FINAL-WORK-AREAS       TO  ACCUM-WORK-AREAS.            ECS015
01459                                                                   ECS015
01460      MOVE 'FINAL TOTALS'         TO T1-DESC.                      ECS015
01461                                                                   ECS015
01462      PERFORM 1310-TOTS-OUT.                                       ECS015
01463                                                                   ECS015
01464  1899-FINAL-X.                                                    ECS015
01465      EXIT.                                                        ECS015
01466                                                                   ECS015
01467  EJECT                                                            ECS015
01468 ***************************************                           ECS015
01469 *                                     *                           ECS015
01470 *    END OF FILE ROUTINES             *                           ECS015
01471 *                                     *                           ECS015
01472 ***************************************                           ECS015
01473  1900-NO-INPUT-RTN.                                               ECS015
01474                                                                   ECS015
01475      PERFORM 0700-PRINT-HEAD-A.                                   ECS015
01476                                                                   ECS015
01477      MOVE ' NO REINSURANCE RECORDS RECEIVED THIS CYCLE '          ECS015
01478                                  TO  PRT.                         ECS015
01479      MOVE '0'                    TO  X.                           ECS015
01480                                                                   ECS015
01481      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01482                                                                   ECS015
01483      GO TO 1920-CLOSE-FICH.                                       ECS015
01484                                                                   ECS015
01485  1910-END-PRINT.                                                  ECS015
01486                                                                   ECS015
01487      PERFORM 0900-REINCO-SUB-PRINT THRU 0999-RSUB-PRINT-X.        ECS015
01488                                                                   ECS015
01489      PERFORM 1000-ACCTS-PRINT THRU 1099-ACCTS-PRINT-X.            ECS015
01490                                                                   ECS015
01491      MOVE 'STATE TOTALS' TO T1-DESC.                              ECS015
01492      PERFORM 1300-TOTS-OUT THRU 1399-TOTS-OUT-X.                  ECS015
01493                                                                   ECS015
01494      PERFORM 1430-GROUP-TOT-OUT THRU 1434-GROUP-TOT-OUT-X.        ECS015
01495                                                                   ECS015
01496      PERFORM 1500-CARRIER-BREAK THRU 1589-CARRIER-BREAK-X.        ECS015
01497                                                                   ECS015
01498      PERFORM 1700-REIN-CO-BREAK THRU 1789-REIN-CO-BREAK-X.        ECS015
01499                                                                   ECS015
01500      PERFORM 1800-FINAL-TOT-SUB THRU 1899-FINAL-X.                ECS015
01501                                                                   ECS015
01502      MOVE '1'                    TO  X.                           ECS015
01503      MOVE SPACES                 TO  PRT.                         ECS015
01504                                                                   ECS015
01505      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01506                                                                   ECS015
01507 ***********************************************                   ECS015
01508 *  PERFORM REINSURANCE COMPANY RECAP ROUTINE  *                   ECS015
01509 ***********************************************                   ECS015
01510                                                                   ECS015
01511      PERFORM 2000-RECAP-REINS-COMPANIES THRU 2000-EXIT.           ECS015
01512                                                                   ECS015
01513      CLOSE RTBL-FILE                                              ECS015
012609           EXT-FILE
01514            PRNT-OUT.                                              ECS015
01515                                                                   ECS015
01516      IF REIN-FILE-STATUS  NOT = '00'                              ECS015
01517          MOVE 'CLOSE ERROR - REIN'   TO  WS-ABEND-MESSAGE         ECS015
01518          MOVE REIN-FILE-STATUS       TO  WS-ABEND-FILE-STATUS     ECS015
01519          GO TO ABEND-PGM.                                         ECS015
01520                                                                   ECS015
01521      EJECT                                                        ECS015
01522  1920-CLOSE-FICH.                                                 ECS015
01523                                  COPY ELCPRTC.                    ECS015
01524                                                                   ECS015
01525  1999-EXIT-REINS.                                                 ECS015
01526      EXIT.                                                        ECS015
01527                                                                   ECS015
01528      EJECT                                                        ECS015
01529 ******************************************************            ECS015
01530 *                                                    *            ECS015
01531 *   ROUTINE TO PRINT THE REINSURANCE COMPANY RECAP   *            ECS015
01532 *                                                    *            ECS015
01533 ******************************************************            ECS015
01534                                                                   ECS015
01535  2000-RECAP-REINS-COMPANIES.                                      ECS015
01536                                                                   ECS015
01537      MOVE +1                     TO  REIN-IDX.                    ECS015
01538      MOVE REIN-COMPANY(REIN-IDX) TO  LAST-REIN.                   ECS015
01539      MOVE ZEROS                  TO  PAGER.                       ECS015
01540                                                                   ECS015
01541  2000-PRINT-RECAP-HEADINGS.                                       ECS015
01542                                                                   ECS015
01543      ADD 1                       TO  PAGER.                       ECS015
01544      MOVE PAGER                  TO  PGNO.                        ECS015
01545      MOVE '1'                    TO  X.                           ECS015
01546      MOVE HEAD-A                 TO  PRT.                         ECS015
01547      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01548                                                                   ECS015
01549      MOVE ' '                    TO  X.                           ECS015
01550      MOVE HEAD-B                 TO  PRT.                         ECS015
01551      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01552                                                                   ECS015
01553      MOVE HEAD-C                 TO  PRT.                         ECS015
01554      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01555                                                                   ECS015
01556      MOVE '0'                    TO  X.                           ECS015
01557      MOVE HEAD-RECAP             TO  PRT.                         ECS015
01558      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01559                                                                   ECS015
01560      MOVE '0'                    TO  X.                           ECS015
01561      MOVE HEAD-TOTAL             TO  PRT.                         ECS015
01562      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01563                                                                   ECS015
01564      MOVE +7                     TO  LINE-COUNT.                  ECS015
01565                                                                   ECS015
01566  2000-PRINT-RECAP-LOOP.                                           ECS015
01567                                                                   ECS015
01568      IF LINE-COUNT GREATER THAN +50                               ECS015
01569          GO TO 2000-PRINT-RECAP-HEADINGS.                         ECS015
01570                                                                   ECS015
01571      IF LAST-REINCO EQUAL REIN-PRIME (REIN-IDX)                   ECS015
01572          ADD REIN-LPREM (REIN-IDX)                                ECS015
01573                                  TO TOTL-LPREM                    ECS015
01574          ADD REIN-APREM (REIN-IDX)                                ECS015
01575                                  TO TOTL-APREM                    ECS015
01576          ADD REIN-LBEN  (REIN-IDX)                                ECS015
01577                                  TO TOTL-LBEN                     ECS015
01578          ADD REIN-ABEN  (REIN-IDX)                                ECS015
01579                                  TO TOTL-ABEN                     ECS015
01580          ADD REIN-CANCL (REIN-IDX)                                ECS015
01581                                  TO TOTL-CANCL                    ECS015
01582          ADD REIN-CANCA (REIN-IDX)                                ECS015
01583                                  TO TOTL-CANCA                    ECS015
01584          ADD REIN-DEATH (REIN-IDX)                                ECS015
01585                                  TO TOTL-DEATH                    ECS015
01586          ADD REIN-DISAB (REIN-IDX)                                ECS015
01587                                  TO TOTL-DISAB                    ECS015
01588          ADD REIN-LCOM  (REIN-IDX)                                ECS015
01589                                  TO TOTL-LCOM                     ECS015
01590          ADD REIN-ACOM  (REIN-IDX)                                ECS015
01591                                  TO TOTL-ACOM                     ECS015
01592      ELSE                                                         ECS015
01593          PERFORM 2100-PRINT-PRIME-TOTAL THRU 2100-EXIT            ECS015
01594          GO TO 2000-PRINT-RECAP-LOOP.                             ECS015
01595                                                                   ECS015
01596      MOVE REIN-COMPANY(REIN-IDX) TO  PRT-REINS-COMP.              ECS015
01597      MOVE REIN-DESCRIPT          TO  T1-DESC.                     ECS015
01598      MOVE LIFE-OVERRIDE-L6       TO  T1-KIND.                     ECS015
01599      MOVE   AH-OVERRIDE-L6       TO  T2-KIND.                     ECS015
01600      MOVE REIN-LBEN (REIN-IDX)   TO  T1-BENEFIT.                  ECS015
01601      MOVE REIN-ABEN (REIN-IDX)   TO  T2-BENEFIT.                  ECS015
01602                                                                   ECS015
01603      ADD  REIN-LPREM(REIN-IDX) REIN-CANCL(REIN-IDX)               ECS015
01604                                  GIVING T1-PREMIUM.               ECS015
01605                                                                   ECS015
01606      ADD  REIN-APREM(REIN-IDX) REIN-CANCA(REIN-IDX)               ECS015
01607                                  GIVING T2-PREMIUM.               ECS015
01608                                                                   ECS015
01609      MOVE REIN-LCOM (REIN-IDX)   TO  T1-COMMISSION.               ECS015
01610      MOVE REIN-ACOM (REIN-IDX)   TO  T2-COMMISSION.               ECS015
01611      MOVE REIN-DEATH(REIN-IDX)   TO  T1-CLAIMS.                   ECS015
01612      MOVE REIN-DISAB(REIN-IDX)   TO  T2-CLAIMS.                   ECS015
01613      MOVE '-'                    TO  X.                           ECS015
01614      MOVE TOT-1                  TO  PRT.                         ECS015
01615      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01616      MOVE SPACES                 TO  TOT-1.                       ECS015
01617      MOVE ' '                    TO  X.                           ECS015
01618      MOVE TOT-2                  TO  PRT.                         ECS015
01619      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01620                                                                   ECS015
01621      MOVE SPACES                 TO  TOT-1.                       ECS015
01622      MOVE 'TOTAL ISSUED'     TO  T1-DESC.                         ECS015
01623                                                                   ECS015
01624      ADD  REIN-LPREM (REIN-IDX) REIN-APREM (REIN-IDX)             ECS015
01625                                  GIVING  T1-BENEFIT.              ECS015
01626                                                                   ECS015
01627      MOVE '0'                    TO  X.                           ECS015
01628      MOVE TOT-1                  TO  PRT.                         ECS015
01629                                                                   ECS015
01630      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01631                                                                   ECS015
01632      MOVE 'LESS CANCELLATIONS'   TO  T1-DESC.                     ECS015
01633                                                                   ECS015
01634      COMPUTE T1-BENEFIT  EQUAL                                    ECS015
01635          ( REIN-CANCL (REIN-IDX)  +  REIN-CANCA (REIN-IDX) ) * -1.ECS015
01636                                                                   ECS015
01637      MOVE TOT-1                  TO  PRT.                         ECS015
01638      MOVE ' '                    TO  X.                           ECS015
01639                                                                   ECS015
01640      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01641                                                                   ECS015
01642      MOVE 'LESS COMMISSIONS'     TO  T1-DESC.                     ECS015
01643                                                                   ECS015
01644      ADD  REIN-LCOM (REIN-IDX) REIN-ACOM (REIN-IDX)               ECS015
01645           GIVING  T1-BENEFIT.                                     ECS015
01646                                                                   ECS015
01647      MOVE TOT-1                  TO  PRT.                         ECS015
01648                                                                   ECS015
01649      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01650                                                                   ECS015
01651      IF REIN-DISAB (REIN-IDX) NOT EQUAL ZERO                      ECS015
01652          MOVE   AH-OVERRIDE-L6      TO  CD-OVRD                   ECS015
01653          MOVE WS-CLAIM-DESC         TO  T1-DESC                   ECS015
01654          MOVE REIN-DISAB (REIN-IDX) TO  T1-BENEFIT                ECS015
01655          MOVE TOT-1                 TO  PRT                       ECS015
01656          ADD +1                     TO  LINE-COUNT                ECS015
01657          PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                   ECS015
01658                                                                   ECS015
01659      IF REIN-DEATH (REIN-IDX) NOT EQUAL ZERO                      ECS015
01660          MOVE LIFE-OVERRIDE-L6      TO  CD-OVRD                   ECS015
01661          MOVE WS-CLAIM-DESC         TO  T1-DESC                   ECS015
01662          MOVE REIN-DEATH (REIN-IDX) TO  T1-BENEFIT                ECS015
01663          MOVE TOT-1                 TO  PRT                       ECS015
01664          ADD +1                     TO  LINE-COUNT                ECS015
01665          PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                   ECS015
01666                                                                   ECS015
01667      MOVE 'NET TOTAL'            TO  T1-DESC.                     ECS015
01668                                                                   ECS015
01669      COMPUTE T1-BENEFIT  EQUAL REIN-LPREM (REIN-IDX) +            ECS015
01670                                REIN-APREM (REIN-IDX) +            ECS015
01671                                REIN-CANCL (REIN-IDX) +            ECS015
01672                                REIN-CANCA (REIN-IDX) -            ECS015
01673                                REIN-LCOM  (REIN-IDX) -            ECS015
01674                                REIN-ACOM  (REIN-IDX) -            ECS015
01675                                REIN-DISAB (REIN-IDX) -            ECS015
01676                                REIN-DEATH (REIN-IDX).             ECS015
01677                                                                   ECS015
01678      MOVE TOT-1                  TO  PRT.                         ECS015
01679      MOVE '0'                    TO  X.                           ECS015
01680                                                                   ECS015
01681      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01682                                                                   ECS015
01683      ADD +9                      TO  LINE-COUNT.                  ECS015
01684      ADD +1                      TO  REIN-IDX.                    ECS015
01685                                                                   ECS015
01686      IF REIN-COMPANY (REIN-IDX) NOT EQUAL SPACES                  ECS015
01687          GO TO 2000-PRINT-RECAP-LOOP.                             ECS015
01688                                                                   ECS015
01689      PERFORM 2100-PRINT-PRIME-TOTAL THRU 2100-EXIT.               ECS015
01690                                                                   ECS015
01691  2000-EXIT.                                                       ECS015
01692      EXIT.                                                        ECS015
01693                                                                   ECS015
01694      EJECT                                                        ECS015
01695 ************************************************************      ECS015
01696 *                                                          *      ECS015
01697 *   ROUTINE TO PRINT THE REINSURANCE COMPANY PRIME TOTAL   *      ECS015
01698 *                                                          *      ECS015
01699 ************************************************************      ECS015
01700                                                                   ECS015
01701  2100-PRINT-PRIME-TOTAL.                                          ECS015
01702                                                                   ECS015
01703      IF LINE-COUNT GREATER THAN +50                               ECS015
01704          PERFORM 2000-PRINT-RECAP-HEADINGS.                       ECS015
01705                                                                   ECS015
01706      MOVE SPACES                 TO  PRT-REINS-COMP.              ECS015
01707      MOVE LAST-REINCO            TO  PRT-REINS-COMP.              ECS015
01708      MOVE REIN-DESCRIPT          TO  T1-DESC.                     ECS015
01709      MOVE LIFE-OVERRIDE-L6       TO  T1-KIND.                     ECS015
01710      MOVE   AH-OVERRIDE-L6       TO  T2-KIND.                     ECS015
01711      MOVE TOTL-LBEN              TO  T1-BENEFIT.                  ECS015
01712      MOVE TOTL-ABEN              TO  T2-BENEFIT.                  ECS015
01713                                                                   ECS015
01714      ADD  TOTL-LPREM   TOTL-CANCL  GIVING  T1-PREMIUM.            ECS015
01715      ADD  TOTL-APREM   TOTL-CANCA  GIVING  T2-PREMIUM.            ECS015
01716                                                                   ECS015
01717      MOVE TOTL-LCOM              TO  T1-COMMISSION.               ECS015
01718      MOVE TOTL-ACOM              TO  T2-COMMISSION.               ECS015
01719      MOVE TOTL-DEATH             TO  T1-CLAIMS.                   ECS015
01720      MOVE TOTL-DISAB             TO  T2-CLAIMS.                   ECS015
01721      MOVE '-'                    TO  X.                           ECS015
01722      MOVE TOT-1                  TO  PRT.                         ECS015
01723      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01724      MOVE SPACES                 TO  TOT-1.                       ECS015
01725      MOVE ' '                    TO  X.                           ECS015
01726      MOVE TOT-2                  TO  PRT.                         ECS015
01727      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01728                                                                   ECS015
01729      MOVE SPACES                 TO  TOT-1.                       ECS015
01730      MOVE 'TOTAL ISSUED'         TO  T1-DESC.                     ECS015
01731                                                                   ECS015
01732      ADD  TOTL-LPREM   TOTL-APREM  GIVING  T1-BENEFIT.            ECS015
01733                                                                   ECS015
01734      MOVE '0'                    TO  X.                           ECS015
01735      MOVE TOT-1                  TO  PRT.                         ECS015
01736                                                                   ECS015
01737      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01738                                                                   ECS015
01739      MOVE 'LESS CANCELLATIONS'   TO  T1-DESC.                     ECS015
01740                                                                   ECS015
01741      COMPUTE T1-BENEFIT  EQUAL                                    ECS015
01742          ( TOTL-CANCL  +  TOTL-CANCA ) * -1.                      ECS015
01743                                                                   ECS015
01744      MOVE TOT-1                  TO  PRT.                         ECS015
01745      MOVE ' '                    TO  X.                           ECS015
01746                                                                   ECS015
01747      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01748                                                                   ECS015
01749      MOVE 'LESS COMMISSIONS'     TO  T1-DESC.                     ECS015
01750                                                                   ECS015
01751      ADD  TOTL-LCOM    TOTL-ACOM   GIVING   T1-BENEFIT.           ECS015
01752                                                                   ECS015
01753      MOVE TOT-1                  TO  PRT.                         ECS015
01754                                                                   ECS015
01755      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01756                                                                   ECS015
01757      IF TOTL-DISAB            NOT EQUAL ZERO                      ECS015
01758          MOVE   AH-OVERRIDE-L6      TO  CD-OVRD                   ECS015
01759          MOVE WS-CLAIM-DESC         TO  T1-DESC                   ECS015
01760          MOVE TOTL-DISAB            TO  T1-BENEFIT                ECS015
01761          MOVE TOT-1                 TO  PRT                       ECS015
01762          ADD +1                     TO  LINE-COUNT                ECS015
01763          PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                   ECS015
01764                                                                   ECS015
01765      IF TOTL-DEATH            NOT EQUAL ZERO                      ECS015
01766          MOVE LIFE-OVERRIDE-L6      TO  CD-OVRD                   ECS015
01767          MOVE WS-CLAIM-DESC         TO  T1-DESC                   ECS015
01768          MOVE TOTL-DEATH            TO  T1-BENEFIT                ECS015
01769          MOVE TOT-1                 TO  PRT                       ECS015
01770          ADD +1                     TO  LINE-COUNT                ECS015
01771          PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                   ECS015
01772                                                                   ECS015
01773      MOVE 'NET TOTAL'            TO  T1-DESC.                     ECS015
01774                                                                   ECS015
01775      COMPUTE T1-BENEFIT  EQUAL TOTL-LPREM  +                      ECS015
01776                                TOTL-APREM  +                      ECS015
01777                                TOTL-CANCL  +                      ECS015
01778                                TOTL-CANCA  -                      ECS015
01779                                TOTL-LCOM   -                      ECS015
01780                                TOTL-ACOM   -                      ECS015
01781                                TOTL-DISAB  -                      ECS015
01782                                TOTL-DEATH.                        ECS015
01783                                                                   ECS015
01784      MOVE TOT-1                  TO  PRT.                         ECS015
01785      MOVE '0'                    TO  X.                           ECS015
01786                                                                   ECS015
01787      PERFORM 0600-PRXX THRU 0699-PRXX-EXIT.                       ECS015
01788                                                                   ECS015
01789      ADD +9                      TO  LINE-COUNT.                  ECS015
01790      MOVE ZERO-AREAS             TO  TOTL-COMPANY-AREAS.          ECS015
01791      MOVE REIN-COMPANY(REIN-IDX) TO  LAST-REIN.                   ECS015
01792                                                                   ECS015
01793  2100-EXIT.                                                       ECS015
01794      EXIT.                                                        ECS015
01795                                                                   ECS015
01796  EJECT                                                            ECS015
01797  9000-END-OF-JOB SECTION.                                         ECS015
01798                                                                   ECS015
01799  9010-E-O-J.                                                      ECS015
01800      GOBACK.                                                      ECS015
01801                                                                   ECS015
01802  ABEND-PGM SECTION.              COPY ELCABEND SUPPRESS.          ECS015
