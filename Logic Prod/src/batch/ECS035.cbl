00001  IDENTIFICATION DIVISION.                                         05/15/98
00002                                                                   ECS035
00003  PROGRAM-ID.                 ECS035.                                 LV032
00004 *              PROGRAM CONVERTED BY                               ECS035
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS035
00006 *              CONVERSION DATE 11/28/95 11:10:06.                 ECS035
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS035
00008 *                            VMOD=2.018.                          ECS035
00009                                                                   ECS035
00010 *AUTHOR.        LOGIC, INC.                                       ECS035
00011 *               DALLAS, TEXAS.                                    ECS035
00012                                                                   ECS035
00013 *DATE-COMPILED.                                                   ECS035
00014                                                                   ECS035
00015 *SECURITY.   *****************************************************ECS035
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS035
00017 *            *                                                   *ECS035
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS035
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS035
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS035
00021 *            *                                                   *ECS035
00022 *            *****************************************************ECS035
00023                                                                   ECS035
00024 *REMARKS.                                                         ECS035
00025 *    1.  PRINTS MONTHLY PRODUCTION REPORTS FOR LAST 12 MONTHS BY  ECS035
00026 *          ACCOUNT, STATE, GROUPING, CARRIER, WITH GRAND TOTALS.  ECS035
00027 *                                                                 ECS035
00028 *    2.  CREATES EXTRACT RECORDS FOR PRINTING BY ECS036 FOR       ECS035
00029 *            A.  OVERALL STATE REPORT                             ECS035
00030 *            B.  CARRIER AND GROUPING WITHIN BUSINESS TYPE        ECS035
00031 *            C.  CARRIER AND GROUPING WITHIN AGENCY (LEVEL 2-10)  ECS035
00032 *                (GA FOR NMC)                                     ECS035
00033 *            D.  OVERALL SPECIAL REPORT-CODE-1 REPORT             ECS035
00034 *            E.  SPECIAL REPORT-CODE-2 WITHIN CARRIER & GROUP     ECS035
00035 *                                                                 ECS035
100703******************************************************************
100703*                   C H A N G E   L O G
100703*
100703* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
100703*-----------------------------------------------------------------
100703*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100703* EFFECTIVE    NUMBER
100703*-----------------------------------------------------------------
100703* 100703    2003080800002  PEMA  ADD SUPER GAP PROCESSING
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
031811* 031811 CR2011012700001   PEMA  ADD ACCT STATUS S - SUSPENDED
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
070714* 070714  CR2013060600001  PEMA  AUTOMATE MONTH END BALANCING
010716* 010716  CR2015082500001  PEMA  VPP CHANGES
021916* 021916  CR2014010900001  TANA  ADD ACCT STATUS D,L,R,P
100703******************************************************************
00036  EJECT                                                            ECS035
00037  ENVIRONMENT DIVISION.                                            ECS035
00038  CONFIGURATION SECTION.                                           ECS035
00039                                                                      CL*32
00040  INPUT-OUTPUT SECTION.                                            ECS035
00041  FILE-CONTROL.                                                    ECS035
00042                                                                   ECS035
00043      SELECT AM-MAST-IN   ASSIGN TO SYS021-FBA1-ERACCTT            ECS035
00044                          ORGANIZATION  INDEXED                    ECS035
00045                          ACCESS        SEQUENTIAL                 ECS035
00046                          RECORD KEY    AM-CONTROL-PRIMARY         ECS035
00047                          FILE STATUS   ERACCT-FILE-STATUS.        ECS035
00048      SELECT EARNED-PREM  ASSIGN TO SYS011-UT-2400-S-SYS011.       ECS035
00049      SELECT EXTRACT-OT   ASSIGN TO SYS012-UT-FBA1-S-SYS012.       ECS035
00050      SELECT PRNT         ASSIGN TO SYS008-UR-1403-S-SYS008.       ECS035
00051      SELECT DISK-DATE    ASSIGN TO SYS019-UT-FBA1-S-SYS019.       ECS035
00052      SELECT FICH         ASSIGN TO SYS020-UT-2400-S-SYS020.       ECS035
00053      SELECT ERMEBL       ASSIGN TO SYS024-FBA1-ERMEBL             ECS035
00054                          ORGANIZATION  INDEXED                    ECS035
00055                          ACCESS        DYNAMIC                    ECS035
00056                          RECORD KEY    ME-CONTROL-PRIMARY         ECS035
00057                          FILE STATUS   ERMEBL-FILE-STATUS.        ECS035
00058  EJECT                                                            ECS035
00059  DATA DIVISION.                                                   ECS035
00060  FILE SECTION.                                                    ECS035
00061                                                                   ECS035
00062  FD  AM-MAST-IN.                                                  ECS035
00063                                                                   ECS035
00064                                  COPY ERCACCT.                    ECS035
00065  EJECT                                                            ECS035
00066  FD  EARNED-PREM                                                  ECS035
00067                                  COPY ECSEPCFD.                   ECS035
00068                                                                   ECS035
00069                                  COPY ECSEPC01.                   ECS035
00070  EJECT                                                            ECS035
00071  FD  EXTRACT-OT                                                   ECS035
00072      BLOCK CONTAINS 0 RECORDS
00073      RECORDING MODE F.                                            ECS035
00074                                                                   ECS035
CIDMOD 01  EXTRACT-OT-REC              PIC X(196).                      ECS035
CIDMOD*01  EXTRACT-OT-REC              PIC X(166).                      ECS035
00076  EJECT                                                            ECS035
00077  FD  PRNT                                                         ECS035
00078                                  COPY ELCPRTFD.                   ECS035
00079  EJECT                                                            ECS035
00080  FD  DISK-DATE                                                    ECS035
00081                                  COPY ELCDTEFD.                   ECS035
00082  EJECT                                                            ECS035
00083  FD  FICH                                                         ECS035
00084                                  COPY ELCFCHFD.                   ECS035
00085  EJECT                                                            ECS035
00086  FD  ERMEBL.                                                      ECS035
00087                                                                   ECS035
00088                                  COPY ERCMEBL.                    ECS035
00089  EJECT                                                            ECS035
00090  WORKING-STORAGE SECTION.                                         ECS035
00091  77  FILLER  PIC X(32) VALUE '********************************'.  ECS035
00092  77  FILLER  PIC X(32) VALUE '     ECS035 WORKING STORAGE     '.  ECS035
00093  77  FILLER  PIC X(32) VALUE '********** VMOD=2.018 **********'.  ECS035
00094                                                                   ECS035
00095  77  Z                           PIC 9(4)  COMP   VALUE 0.        ECS035
00096  77  Z1                          PIC 9(4)  COMP   VALUE 0.        ECS035
00097  77  CNT-IN                      PIC S9(9) COMP-3 VALUE ZEROS.    ECS035
00098  77  CNT-OT                      PIC S9(9) COMP-3 VALUE ZEROS.    ECS035
00099  77  AM-CNT-IN                   PIC  9(9)        VALUE ZEROS.    ECS035
00100  77  WS-ISS-FLAG                 PIC X            VALUE SPACE.    ECS035
CIDMOD 77  PRNT-TOT-SW                 PIC X            VALUE 'N'.      ECS035
00101  77  WS-ISS-CNT                  PIC S9(9)        VALUE ZEROS.    ECS035
00102  77  LINE-CNT                    PIC S999  COMP-3 VALUE ZEROS.    ECS035
00103  77  PAGE-CNT                    PIC S9(5) COMP-3 VALUE ZEROS.    ECS035
00104  77  PGM-SUB                     PIC S999  COMP   VALUE +035.     ECS035
00105  77  X                           PIC X.                           ECS035
00106  77  A1                          PIC S999  COMP   VALUE +000.     ECS035
00107  77  X1                          PIC S999  COMP   VALUE +000.     ECS035
00108  77  Y1                          PIC S999  COMP   VALUE +000.     ECS035
00109  77  SAVE-X1                     PIC S999  COMP   VALUE +000.     ECS035
00110  77  SET-CTR                     PIC S999  COMP-3 VALUE ZERO.     ECS035
00111  77  DATE-RANGE-MAX              PIC 99           VALUE 13.       ECS035
00112  77  DATE-RANGE-MAX-1            PIC 99           VALUE 12.       ECS035
00113  77  CARRIER-L                   PIC X            VALUE LOW-VALUE.ECS035
00114  77  ERACCT-FILE-STATUS          PIC XX           VALUE ZERO.     ECS035
00115  EJECT                                                            ECS035
00116  01  WS.                                                          ECS035
00117      12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.      ECS035
00118      12  WS-ZERO                 PIC S9          VALUE ZERO.      ECS035
00119      12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.    ECS035
00120      12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.      ECS035
00121      12  WS-EE-DTE               PIC 9(07).                          CL*16
00122      12  WS-EE-DTE-R REDEFINES WS-EE-DTE.                            CL*16
00123          16  FILLER              PIC 9.                              CL*18
00124          16  EE-CCYY             PIC 9(4).                           CL*31
00125          16  EE-MO               PIC 99.                             CL*16
00126                                                                   ECS035
00127  01  MONTH-END-DATA.                                              ECS035
00128      12  ME-START-DATE.                                           ECS035
00129          16  ME-START-MO         PIC 99.                          ECS035
00130          16  FILLER              PIC X.                           ECS035
00131          16  ME-START-DA         PIC 99.                          ECS035
00132          16  FILLER              PIC X.                           ECS035
00133          16  ME-START-YR         PIC 99.                          ECS035
00134      12  ME-CNDS-DATE            PIC 9(6).                        ECS035
00135      12  ME-CNDS-DATE-R REDEFINES ME-CNDS-DATE.                   ECS035
00136          16  ME-CNDS-MO          PIC 99.                          ECS035
00137          16  ME-CNDS-DA          PIC 99.                          ECS035
00138          16  ME-CNDS-YR          PIC 99.                          ECS035
00139      12  ME-START-TIME           PIC 9(6).                        ECS035
00140      12  ME-UPDATE-FLAG          PIC X          VALUE 'Y'.        ECS035
00141          88  ME-DO-UPDATE                       VALUE 'Y'.        ECS035
00142          88  ME-NO-UPDATE                       VALUE 'N'.        ECS035
00143      12  ERMEBL-FILE-STATUS      PIC XX.                          ECS035
00144      12  MONTH-END-MOYR          PIC S9(5) COMP-3.                ECS035
00145                                                                   ECS035
00146  01  W-HD-WORK-AREA.                                              ECS035
00147      12  W-ZEROS                 PIC S9(04) COMP VALUE +0.        ECS035
00148      12  W-WL-NDX                PIC S9(04) COMP.                 ECS035
00149      12  W-WA2-NDX               PIC S9(04) COMP.                 ECS035
00150      12  W-WORK-LINE.                                             ECS035
00151          16  FILLER              PIC X(10).                       ECS035
00152          16  W-WORK-AREA-2.                                       ECS035
00153              20  W-WA2-CHAR OCCURS 86 TIMES                       ECS035
00154                                  PIC X(01).                       ECS035
00155      12  W-WORKING-LINE.                                          ECS035
00156          16  W-WL-CHAR OCCURS 86 TIMES                            ECS035
00157                                  PIC X(01).                       ECS035
00158                                                                   ECS035
00159  01  QTR-SW                      PIC X      VALUE SPACE.          ECS035
00160      88  QTR-END                            VALUE '1'.            ECS035
00161                                                                   ECS035
00162  01  QTR-COMP                    PIC X      VALUE SPACE.          ECS035
00163      88  QTR-CO                             VALUE '1'.            ECS035
00164                                                                   ECS035
00165  01  B-E-AGT-TABLE.                                               ECS035
00166      05  B-E-AGT                 PIC X(10)  OCCURS 9 TIMES.       ECS035
00167                                                                   ECS035
00168  01  WS-EXTR-REC.                                                 ECS035
00169      05  EE-PASS-NO              PIC X.                           ECS035
00170      05  EE-CNTL.                                                 ECS035
00171          10  EE-CNTL-1.                                           ECS035
00172              15  EE-CNTL-GA      PIC X(10).                       ECS035
00173              15  EE-CNTL-ACCT    PIC X(10).                       ECS035
00174          10  EE-CNTL-2.                                           ECS035
00175              15  EE-CARR         PIC X.                           ECS035
00176              15  EE-GROUP        PIC X(6).                        ECS035
00177          10  FILLER              PIC X(12).                       ECS035
00178      05  EE-ALT-RPT-CNTL  REDEFINES  EE-CNTL.                     ECS035
00179          10  EE-A-RPT-CD-1       PIC X(10).                       ECS035
00180          10  EE-A-CARR           PIC X.                           ECS035
00181          10  EE-A-GROUP          PIC X(6).                        ECS035
00182          10  EE-A-RPT-CD-2       PIC X(10).                       ECS035
00183          10  EE-A-STATE          PIC XX.                          ECS035
00184          10  EE-A-ACCT           PIC X(10).                       ECS035
00185      05  EE-DTE                  PIC 9(07)   COMP-3.                 CL*15
00186      05  EE-CNTRS    COMP-3.                                      ECS035
00187          10  EE-CERT             PIC S9(11)V99.                   ECS035
00188          10  EE-LBEN             PIC S9(11)V99.                   ECS035
00189          10  EE-LPRM             PIC S9(11)V99.                   ECS035
00190          10  EE-LCLM             PIC S9(11)V99.                   ECS035
00191          10  EE-ABEN             PIC S9(11)V99.                   ECS035
00192          10  EE-APRM             PIC S9(11)V99.                   ECS035
00193          10  EE-ACLM             PIC S9(11)V99.                   ECS035
00194          10  EE-TPRM             PIC S9(11)V99.                   ECS035
00195          10  EE-TCOM             PIC S9(11)V99.                   ECS035
00196      05  FILLER                  PIC X(3).                        ECS035
00197      05  EE-ISS-FLAG             PIC X.                           ECS035
00198      05  EE-ACCT-STATUS          PIC X.                           ECS035
00199      05  EE-ACCT-NAME            PIC X(30).                       ECS035
00200      05  EE-AM-EXPIRES           PIC 9(11) COMP-3.                   CL*31
00201      05  EE-AM-HI-CERT           PIC 9(11) COMP-3.                   CL*31
00202      05  EE-MTH-HI-CERT          PIC 9(11) COMP-3.                ECS035
00203      05  EE-ISS-CNT              PIC S9(9) COMP-3.                ECS035
00204      05  FILLER                  PIC X.                           ECS035
CIDMOD     05  EE-ACCT-CITY            PIC X(30).                       ECS035
00205                                                                   ECS035
00206  01  PRINT-SWITCHES.                                              ECS035
00207      05  P-ACC-SW                PIC X VALUE ' '.                 ECS035
00208      05  P-ST-SW                 PIC X VALUE ' '.                 ECS035
00209      05  P-GRP-SW                PIC X VALUE ' '.                 ECS035
00210      05  P-CA-SW                 PIC X VALUE ' '.                 ECS035
00211                                                                   ECS035
00212  01  RUN-DT.                                                      ECS035
00213      05  RUN-DT-CCYY             PIC 9(4).                           CL*31
00214      05  RUN-DT-MO               PIC 99.                             CL*31
00215                                                                   ECS035
00216  01  RUN9DT REDEFINES RUN-DT     PIC 9(6).                        ECS035
00217                                                                   ECS035
00218  01  WORK-ACCUM  COMP-3.                                          ECS035
00219      05  CERT            PIC S9(11)V99   VALUE ZEROS.             ECS035
00220      05  LBEN            PIC S9(12)V99   VALUE ZEROS.             ECS035
00221      05  LPRM            PIC S9(11)V99   VALUE ZEROS.             ECS035
00222      05  LCLM            PIC S9(12)V99   VALUE ZEROS.             ECS035
00223      05  ABEN            PIC S9(12)V99   VALUE ZEROS.             ECS035
00224      05  APRM            PIC S9(11)V99   VALUE ZEROS.             ECS035
00225      05  ACLM            PIC S9(12)V99   VALUE ZEROS.             ECS035
00226      05  TPRM            PIC S9(11)V99   VALUE ZEROS.             ECS035
00227      05  TCOM            PIC S9(11)V99   VALUE ZEROS.             ECS035
00228  EJECT                                                            ECS035
00229  01  HD-1-ACCOUNT.                                                ECS035
00230      05  FILLER          PIC X(45)   VALUE SPACES.                ECS035
00231      05  ACCT-PER-HD     PIC X(10)   VALUE '  MONTHLY '.          ECS035
00232      05  FILLER          PIC X(12)   VALUE 'ACCOUNT PROD'.        ECS035
00233      05  FILLER          PIC X(13)   VALUE 'UCTION REPORT'.       ECS035
00234      05  FILLER          PIC X(39)   VALUE SPACES.                ECS035
00235      05  FILLER          PIC X(6)    VALUE 'ECS035'.              ECS035
00236                                                                   ECS035
00237  01  HD-1-STATE.                                                  ECS035
00238      05  FILLER          PIC X(46)   VALUE SPACES.                ECS035
00239      05  STATE-PER-HD    PIC X(10)   VALUE '  MONTHLY '.          ECS035
00240      05  FILLER          PIC X(12)   VALUE 'STATE PRODUC'.        ECS035
00241      05  FILLER          PIC X(11)   VALUE 'TION REPORT'.         ECS035
00242      05  FILLER          PIC X(40)   VALUE SPACES.                ECS035
00243      05  FILLER          PIC X(6)    VALUE 'ECS035'.              ECS035
00244                                                                   ECS035
00245  01  HD-1-GROUPING.                                               ECS035
00246      05  FILLER          PIC X(45)   VALUE SPACES.                ECS035
00247      05  GRP-PER-HD      PIC X(10)   VALUE '  MONTHLY '.          ECS035
00248      05  FILLER          PIC X(13)   VALUE 'GROUPING PROD'.       ECS035
00249      05  FILLER          PIC X(13)   VALUE 'UCTION REPORT'.       ECS035
00250      05  FILLER          PIC X(38)   VALUE SPACES.                ECS035
00251      05  FILLER          PIC X(6)    VALUE 'ECS035'.              ECS035
00252                                                                   ECS035
00253  01  HD-1-CARRIER.                                                ECS035
00254      05  FILLER          PIC X(45)   VALUE SPACES.                ECS035
00255      05  CARR-PER-HD     PIC X(10)   VALUE '  MONTHLY '.          ECS035
00256      05  FILLER          PIC X(12)   VALUE 'CARRIER PROD'.        ECS035
00257      05  FILLER          PIC X(13)   VALUE 'UCTION REPORT'.       ECS035
00258      05  FILLER          PIC X(39)   VALUE SPACES.                ECS035
00259      05  FILLER          PIC X(6)    VALUE 'ECS035'.              ECS035
00260                                                                   ECS035
00261  01  HD-1-GRAND-TOTALS.                                           ECS035
00262      05  FILLER          PIC X(47)   VALUE SPACES.                ECS035
00263      05  FILLER          PIC X(20)   VALUE 'PRODUCTION REPORT GR'.ECS035
00264      05  FILLER          PIC X(10)   VALUE 'AND TOTALS'.          ECS035
00265      05  FILLER          PIC X(42)   VALUE SPACES.                ECS035
00266      05  FILLER          PIC X(6)    VALUE 'ECS035'.              ECS035
00267                                                                   ECS035
00268  01  HD-2.                                                        ECS035
00269      05  FILLER          PIC X(47)   VALUE SPACES.                ECS035
00270      05  HD-COMPANY-NAME PIC X(30)   VALUE SPACES.                ECS035
00271      05  FILLER          PIC X(42)   VALUE SPACES.                ECS035
00272      05  HD-IPL-DATE     PIC X(8)    VALUE SPACES.                ECS035
00273                                                                   ECS035
00274  01  HD-3.                                                        ECS035
00275      05  FILLER          PIC X(53)   VALUE SPACES.                ECS035
00276      05  HD-ALPHA-DATE   PIC X(18)   VALUE SPACES.                ECS035
00277      05  FILLER          PIC X(48)   VALUE SPACES.                ECS035
00278      05  FILLER          PIC X(5)    VALUE 'PAGE '.               ECS035
00279      05  HD-PAGE         PIC ZZ,ZZ9.                              ECS035
00280                                                                   ECS035
00281  01  HD-4.                                                        ECS035
00282      05  HD4-TOTAL-FOR   PIC X(10)   VALUE 'TOTAL FOR '.          ECS035
00283      05  FILLER          PIC X(06)   VALUE 'GROUP '.              ECS035
00284      05  HD-GROUPING     PIC X(6)    VALUE SPACES.                ECS035
00285                                                                   ECS035
00286  01  HD-5.                                                        ECS035
00287      05  HD5-TOTAL-FOR   PIC X(10)   VALUE 'TOTAL FOR '.          ECS035
00288      05  FILLER          PIC X(08)   VALUE 'ACCOUNT '.            ECS035
00289      05  HD-ACCOUNT      PIC X(10)   VALUE SPACES.                ECS035
00290      05  FILLER          PIC X(2)    VALUE ' ('.                  ECS035
00291      05  HD-ACCT-NAME    PIC X(30)   VALUE SPACES.                ECS035
00292      05  FILLER          PIC X(4)    VALUE ' AT '.                ECS035
00293      05  HD-ACCT-ADDRESS PIC X(30)   VALUE SPACES.                ECS035
00294      05  FILLER          PIC X(1)    VALUE ')'.                   ECS035
00295                                                                   ECS035
00296  01  HD-6.                                                        ECS035
00297      05  HD6-TOTAL-FOR   PIC X(10)   VALUE 'TOTAL FOR '.          ECS035
00298      05  FILLER          PIC X(06)   VALUE 'STATE '.              ECS035
00299      05  HD-STATE-ABBR   PIC X(02)   VALUE SPACES.                ECS035
00300      05  FILLER          PIC X(02)   VALUE ' ('.                  ECS035
00301      05  HD-STATE        PIC X(30)   VALUE SPACES.                ECS035
00302      05  FILLER          PIC X(1)    VALUE ')'.                   ECS035
00303                                                                   ECS035
00304  01  HD-7.                                                        ECS035
00305      05  HD7-TOTAL-FOR   PIC X(10)   VALUE 'TOTAL FOR '.          ECS035
00306      05  FILLER          PIC X(08)   VALUE 'CARRIER '.            ECS035
00307      05  HD-CARRIER      PIC X       VALUE SPACES.                ECS035
00308      05  FILLER          PIC X(2)    VALUE ' ('.                  ECS035
00309      05  HD-CARR-NAME    PIC X(30)   VALUE SPACES.                ECS035
00310      05  FILLER          PIC X(1)    VALUE ')'.                   ECS035
00311                                                                   ECS035
00312  01  HD-8.                                                        ECS035
00313      05  FILLER          PIC X(20)   VALUE 'GRAND TOTALS '.       ECS035
00314                                                                   ECS035
00315  01  HD-9.                                                        ECS035
00316      05  HD-9-1          PIC X(20)   VALUE '               NET  '.ECS035
00317      05  FILLER          PIC X(5)    VALUE SPACES.                ECS035
00318      05  HD-9-LF-OVRD-1  PIC X(6).                                ECS035
00319      05  FILLER          PIC X(8)    VALUE SPACES.                ECS035
00320      05  HD-9-LF-OVRD-2  PIC X(6).                                ECS035
00321      05  FILLER          PIC X(9)    VALUE SPACES.                ECS035
00322      05  HD-9-LF-OVRD-3  PIC X(6).                                ECS035
00323      05  FILLER          PIC X(8)    VALUE SPACES.                ECS035
00324      05  HD-9-AH-OVRD-1  PIC X(6).                                ECS035
00325      05  FILLER          PIC X(8)    VALUE SPACES.                ECS035
00326      05  HD-9-AH-OVRD-2  PIC X(6).                                ECS035
00327      05  FILLER          PIC X(9)    VALUE SPACES.                ECS035
00328      05  HD-9-AH-OVRD-3  PIC X(6).                                ECS035
00329      05  FILLER          PIC X(9)    VALUE SPACES.                ECS035
00330      05  FILLER          PIC X(20)   VALUE 'TOTAL        TOTAL  '.ECS035
00331                                                                   ECS035
00332  01  HD-TMS-9.                                                    ECS035
CIDMOD*    05  FILLER          PIC X(13)   VALUE SPACES.                ECS035
CIDMOD*    05  FILLER          PIC X(5)    VALUE 'NO OF'.               ECS035
CIDMOD     05  FILLER          PIC X(7)    VALUE SPACES.                ECS035
CIDMOD     05  FILLER          PIC X(12)   VALUE 'TOTAL ISSUED'.        ECS035
CIDMOD     05  FILLER          PIC X(4)    VALUE SPACES.                ECS035
00336      05  FILLER          PIC X(5)    VALUE 'NO OF'.               ECS035
00337      05  FILLER          PIC X(7)    VALUE SPACES.                ECS035
00338      05  FILLER          PIC X(6)    VALUE 'NO OF '.              ECS035
00339      05  FILLER          PIC X(9)    VALUE SPACES.                ECS035
00340      05  FILLER          PIC X(5)    VALUE 'GROSS'.               ECS035
00341      05  FILLER          PIC X(10)   VALUE SPACES.                ECS035
00342      05  FILLER          PIC X(5)    VALUE 'TOTAL'.               ECS035
00343      05  FILLER          PIC X(12)   VALUE SPACES.                ECS035
00344      05  FILLER          PIC X(3)    VALUE 'NET'.                 ECS035
00345      05  FILLER          PIC X(9)    VALUE SPACES.                ECS035
00346      05  FILLER          PIC X(5)    VALUE 'TOTAL'.               ECS035
00347      05  FILLER          PIC X(6)    VALUE SPACES.                ECS035
00348      05  FILLER          PIC X(12)   VALUE 'PREMIUM LESS'.        ECS035
00349      05  FILLER          PIC X(9)    VALUE SPACES.                ECS035
00350      05  FILLER          PIC X(5)    VALUE 'TOTAL'.               ECS035
00351                                                                   ECS035
00352  01  HD-TMS-10.                                                   ECS035
00353      05  FILLER          PIC X(10)   VALUE SPACES.                ECS035
CIDMOD*    05  FILLER          PIC X(9)    VALUE 'COVERAGES'.           ECS035
00353      05  FILLER          PIC X(3)    VALUE SPACES.                ECS035
00356      05  FILLER          PIC X(7)    VALUE 'CANCELS'.             ECS035
00357      05  FILLER          PIC X(4)    VALUE SPACES.                ECS035
00358      05  FILLER          PIC X(9)    VALUE 'COVERAGES'.           ECS035
00359      05  FILLER          PIC X(7)    VALUE SPACES.                ECS035
00360      05  FILLER          PIC X(7)    VALUE 'PREMIUM'.             ECS035
00361      05  FILLER          PIC X(8)    VALUE SPACES.                ECS035
00362      05  FILLER          PIC X(7)    VALUE 'REFUNDS'.             ECS035
00363      05  FILLER          PIC X(9)    VALUE SPACES.                ECS035
00364      05  FILLER          PIC X(7)    VALUE 'PREMIUM'.             ECS035
00365      05  FILLER          PIC X(4)    VALUE SPACES.                ECS035
00366      05  FILLER          PIC X(10)   VALUE 'COMMISSION'.          ECS035
00367      05  FILLER          PIC X(5)    VALUE SPACES.                ECS035
00368      05  FILLER          PIC X(10)   VALUE 'COMMISSION'.          ECS035
00369      05  FILLER          PIC X(10)   VALUE SPACES.                ECS035
00370      05  FILLER          PIC X(6)    VALUE 'CLAIMS'.              ECS035
00371                                                                   ECS035
00372  01  HD-10.                                                       ECS035
CIDMOD*    05  HD-10-1         PIC X(20)   VALUE '            ACTIVITY'.ECS035
CIDMOD     05  HD-10-1         PIC X(20)   VALUE '           COVERAGES'.ECS035
00374      05  FILLER          PIC X(20)   VALUE '    BENEFITS       P'.ECS035
00375      05  FILLER          PIC X(20)   VALUE 'REMIUM        CLAIMS'.ECS035
00376      05  FILLER          PIC X(20)   VALUE '       BENEFITS     '.ECS035
00377      05  FILLER          PIC X(20)   VALUE '  PREMIUM        CLA'.ECS035
00378      05  FILLER          PIC X(20)   VALUE 'IMS        PREMIUM  '.ECS035
00379      05  FILLER          PIC X(12)   VALUE '  COMMISSION'.        ECS035
00380                                                                   ECS035
00381  01  DETAIL-LINE.                                                 ECS035
00382      05  DET-TITLE.                                               ECS035
00383          10  DET-DATE.                                            ECS035
00384              15  DET-MO          PIC XXX     VALUE SPACES.        ECS035
00385              15  FILLER          PIC X       VALUE SPACES.        ECS035
00386              15  DET-YR          PIC XX      VALUE SPACES.        ECS035
00387              15  FILLER          PIC X       VALUE SPACES.        ECS035
00388          10  DET-CERTS           PIC ZZZ,ZZZ,Z99-.                ECS035
00389      05  DET-LBEN                PIC ZZZZZ,ZZZ,Z99-.              ECS035
00390      05  DET-LPRM                PIC ZZ,ZZZ,ZZ9.99-.              ECS035
00391      05  DET-LCLM                PIC ZZ,ZZZ,ZZ9.99-.              ECS035
00392      05  DET-ABEN                PIC ZZZ,ZZZ,ZZ9.99-.             ECS035
00393      05  DET-APRM                PIC ZZ,ZZZ,ZZ9.99-.              ECS035
00394      05  DET-ACLM                PIC ZZ,ZZZ,ZZ9.99-.              ECS035
00395      05  DET-TPRM                PIC ZZZ,ZZZ,ZZ9.99-.             ECS035
00396      05  DET-TCOM                PIC ZZZZZ,ZZ9.99-.               ECS035
00397                                                                   ECS035
00398  01  DETAIL-TMS-LINE-1.                                           ECS035
00399      05  DET-TMS-TITLE.                                           ECS035
00400          10  DET-TMS-DATE.                                        ECS035
00401              15  DET-TMS-MO      PIC XXX     VALUE SPACES.        ECS035
00402              15  FILLER          PIC X       VALUE SPACES.        ECS035
00403              15  DET-TMS-YR      PIC XX      VALUE SPACES.        ECS035
00404              15  FILLER          PIC X       VALUE SPACES.        ECS035
00405          10  DET-TMS-CERTS       PIC ZZ,ZZZ,Z99-.                 ECS035
00406          10  DET-TMS-CANCELS     PIC Z,ZZZ,Z99-.                  ECS035
00407          10  FILLER              PIC X.                           ECS035
00408          10  DET-TMS-LDESC       PIC XX.                          ECS035
00409          10  FILLER              PIC X.                           ECS035
00410      05  DET-TMS-LCOVERAG        PIC Z,ZZZ,Z99-.                  ECS035
00411      05  DET-TMS-LPRM            PIC ZZ,ZZZ,ZZ9.99-.              ECS035
00412      05  FILLER                  PIC X.                           ECS035
00413      05  DET-TMS-LCAN            PIC ZZ,ZZZ,ZZ9.99-.              ECS035
00414      05  FILLER                  PIC X.                           ECS035
00415      05  DET-TMS-TPRM            PIC ZZZ,ZZZ,ZZ9.99-.             ECS035
00416      05  FILLER                  PIC X.                           ECS035
00417      05  DET-TMS-TCOM            PIC ZZZZZ,ZZ9.99-.               ECS035
00418      05  FILLER                  PIC X.                           ECS035
00419      05  DET-TMS-NPRM            PIC ZZZ,ZZZ,ZZ9.99-.             ECS035
00420      05  FILLER                  PIC X.                           ECS035
00421      05  DET-TMS-LCLM            PIC ZZ,ZZZ,ZZ9.99-.              ECS035
00422                                                                   ECS035
00423  01  DETAIL-TMS-LINE-2.                                           ECS035
00424      05  FILLER                  PIC X(29).                       ECS035
00425      05  DET-TMS-ADESC           PIC XX.                          ECS035
00426      05  FILLER                  PIC X.                           ECS035
00427      05  DET-TMS-ACOVERAG        PIC Z,ZZZ,Z99-.                  ECS035
00428      05  DET-TMS-APRM            PIC ZZ,ZZZ,ZZ9.99-.              ECS035
00429      05  FILLER                  PIC X.                           ECS035
00430      05  DET-TMS-ACAN            PIC ZZ,ZZZ,ZZ9.99-.              ECS035
00431      05  FILLER                  PIC X(47).                       ECS035
00432      05  DET-TMS-ACLM            PIC ZZ,ZZZ,ZZ9.99-.              ECS035
00433                                                                   ECS035
00434  01  DETAIL-TMS-LINE-3.                                           ECS035
00435      05  FILLER                  PIC X(131)      VALUE ALL '-'.   ECS035
00436                                                                   ECS035
00437  01  DET-DATE-LINE.                                               ECS035
00438      05  DET-DATE-DESC           PIC X(17)       VALUE SPACES.    ECS035
00439      05  DET-DATE-MO             PIC XX.                          ECS035
00440      05  DET-DATE-SLASH-1        PIC X           VALUE '/'.       ECS035
00441      05  DET-DATE-DA             PIC XX.                          ECS035
00442      05  DET-DATE-SLASH-2        PIC X           VALUE '/'.       ECS035
00443      05  DET-DATE-YR             PIC XX.                          ECS035
00444      05  FILLER                  PIC X(107)      VALUE SPACES.    ECS035
00445  EJECT                                                            ECS035
00446  01  ACCOUNT-ACCUMULATORS.                                        ECS035
00447      05  ACCOUNT-ACCUM           OCCURS 13 TIMES.                 ECS035
00448          10  AC-CERT             PIC S9(9)        COMP-3.         ECS035
00449          10  AC-LBEN             PIC S9(12)V99    COMP-3.         ECS035
00450          10  AC-LPRM             PIC S9(11)V99    COMP-3.         ECS035
00451          10  AC-LCLM             PIC S9(12)V99    COMP-3.         ECS035
00452          10  AC-ABEN             PIC S9(11)V99    COMP-3.         ECS035
00453          10  AC-APRM             PIC S9(11)V99    COMP-3.         ECS035
00454          10  AC-ACLM             PIC S9(11)V99    COMP-3.         ECS035
00455          10  AC-TPRM             PIC S9(11)V99    COMP-3.         ECS035
00456          10  AC-TCOM             PIC S9(11)V99    COMP-3.         ECS035
00457          10  AC-DATE             PIC  9(11)       COMP-3.            CL*30
00458      05  AC-DATE-R               OCCURS 13 TIMES.                 ECS035
00459          10  FILLER          PIC 999.                             ECS035
00460          10  AC-DATE-CC      PIC 99.                              ECS035
00461          10  AC-DATE-YR      PIC 99.                              ECS035
00462          10  AC-DATE-MO      PIC 99.                              ECS035
00463          10  AC-DATE-DA      PIC 99.                              ECS035
00464      05  ZERO-ACCOUNT-ACCUM.                                      ECS035
00465          10  FILLER              PIC S9(9)      COMP-3  VALUE +0. ECS035
00466          10  FILLER              PIC S9(12)V99  COMP-3  VALUE +0. ECS035
00467          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00468          10  FILLER              PIC S9(12)V99  COMP-3  VALUE +0. ECS035
00469          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00470          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00471          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00472          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00473          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00474          10  FILLER              PIC  9(11)     COMP-3  VALUE  0.    CL*30
00475                                                                   ECS035
00476  01  DATE-RANGE-ACCUMULATORS.                                     ECS035
00477      05  DATE-RANGE-ACCUM        OCCURS 13 TIMES.                 ECS035
00478          10  DR-CERT             PIC S9(9)        COMP-3.         ECS035
00479          10  DR-ISS              PIC S9(9)        COMP-3.         ECS035
00480          10  DR-LBEN             PIC S9(12)V99    COMP-3.         ECS035
00481          10  DR-LPRM             PIC S9(11)V99    COMP-3.         ECS035
00482          10  DR-LCLM             PIC S9(12)V99    COMP-3.         ECS035
00483          10  DR-ABEN             PIC S9(11)V99    COMP-3.         ECS035
00484          10  DR-APRM             PIC S9(11)V99    COMP-3.         ECS035
00485          10  DR-ACLM             PIC S9(11)V99    COMP-3.         ECS035
00486          10  DR-TPRM             PIC S9(11)V99    COMP-3.         ECS035
00487          10  DR-TCOM             PIC S9(11)V99    COMP-3.         ECS035
00488          10  DR-DATE             PIC  9(11)       COMP-3.            CL*31
00489      05  ZERO-DATE-RANGE-ACCUM.                                   ECS035
00490          10  FILLER              PIC S9(9)      COMP-3  VALUE +0. ECS035
00491          10  FILLER              PIC S9(9)      COMP-3  VALUE +0. ECS035
00492          10  FILLER              PIC S9(12)V99  COMP-3  VALUE +0. ECS035
00493          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00494          10  FILLER              PIC S9(12)V99  COMP-3  VALUE +0. ECS035
00495          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00496          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00497          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00498          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00499          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00500          10  FILLER              PIC 9(11)      COMP-3  VALUE  0.    CL*24
00501                                                                   ECS035
00502  01  ACCUMULATORS COMP-3.                                         ECS035
00503      05  STATE-ACCUM             OCCURS 13 TIMES.                 ECS035
00504          10  ST-CERT             PIC S9(9).                       ECS035
00505          10  ST-LBEN             PIC S9(12)V99.                   ECS035
00506          10  ST-LPRM             PIC S9(11)V99.                   ECS035
00507          10  ST-LCLM             PIC S9(12)V99.                   ECS035
00508          10  ST-ABEN             PIC S9(11)V99.                   ECS035
00509          10  ST-APRM             PIC S9(11)V99.                   ECS035
00510          10  ST-ACLM             PIC S9(11)V99.                   ECS035
00511          10  ST-TPRM             PIC S9(11)V99.                   ECS035
00512          10  ST-TCOM             PIC S9(11)V99.                   ECS035
00513      05  GROUPING-ACCUM          OCCURS 13 TIMES.                 ECS035
00514          10  GP-CERT             PIC S9(9).                       ECS035
00515          10  GP-LBEN             PIC S9(12)V99.                   ECS035
00516          10  GP-LPRM             PIC S9(11)V99.                   ECS035
00517          10  GP-LCLM             PIC S9(12)V99.                   ECS035
00518          10  GP-ABEN             PIC S9(11)V99.                   ECS035
00519          10  GP-APRM             PIC S9(11)V99.                   ECS035
00520          10  GP-ACLM             PIC S9(11)V99.                   ECS035
00521          10  GP-TPRM             PIC S9(11)V99.                   ECS035
00522          10  GP-TCOM             PIC S9(11)V99.                   ECS035
00523      05  CARRIER-ACCUM   OCCURS 13 TIMES.                         ECS035
00524          10  CA-CERT             PIC S9(9).                       ECS035
00525          10  CA-LBEN             PIC S9(12)V99.                   ECS035
00526          10  CA-LPRM             PIC S9(11)V99.                   ECS035
00527          10  CA-LCLM             PIC S9(12)V99.                   ECS035
00528          10  CA-ABEN             PIC S9(11)V99.                   ECS035
00529          10  CA-APRM             PIC S9(11)V99.                   ECS035
00530          10  CA-ACLM             PIC S9(11)V99.                   ECS035
00531          10  CA-TPRM             PIC S9(11)V99.                   ECS035
00532          10  CA-TCOM             PIC S9(11)V99.                   ECS035
00533      05  GRAND-TOTALS.                                            ECS035
00534          10  GR-CERT             PIC S9(9)      VALUE ZEROS.      ECS035
00535          10  GR-LBEN             PIC S9(12)V99  VALUE ZEROS.      ECS035
00536          10  GR-LPRM             PIC S9(11)V99  VALUE ZEROS.      ECS035
00537          10  GR-LCLM             PIC S9(12)V99  VALUE ZEROS.      ECS035
00538          10  GR-ABEN             PIC S9(11)V99  VALUE ZEROS.      ECS035
00539          10  GR-APRM             PIC S9(11)V99  VALUE ZEROS.      ECS035
00540          10  GR-ACLM             PIC S9(11)V99  VALUE ZEROS.      ECS035
00541          10  GR-TPRM             PIC S9(11)V99  VALUE ZEROS.      ECS035
00542          10  GR-TCOM             PIC S9(11)V99  VALUE ZEROS.      ECS035
00543      05  GRAND-ACCUM     OCCURS 13 TIMES.                         ECS035
00544          10  GT-CERT             PIC S9(9).                       ECS035
00545          10  GT-LBEN             PIC S9(12)V99.                   ECS035
00546          10  GT-LPRM             PIC S9(11)V99.                   ECS035
00547          10  GT-LCLM             PIC S9(12)V99.                   ECS035
00548          10  GT-ABEN             PIC S9(11)V99.                   ECS035
00549          10  GT-APRM             PIC S9(11)V99.                   ECS035
00550          10  GT-ACLM             PIC S9(11)V99.                   ECS035
00551          10  GT-TPRM             PIC S9(11)V99.                   ECS035
00552          10  GT-TCOM             PIC S9(11)V99.                   ECS035
00553      05  GRAND-TOTALS-LAST-12-MO.                                 ECS035
00554          10  L12GD-CERT          PIC S9(9)        VALUE ZEROS.    ECS035
00555          10  L12GD-LBEN          PIC S9(12)V99    VALUE ZEROS.    ECS035
00556          10  L12GD-LPRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00557          10  L12GD-LCLM          PIC S9(12)V99    VALUE ZEROS.    ECS035
00558          10  L12GD-ABEN          PIC S9(11)V99    VALUE ZEROS.    ECS035
00559          10  L12GD-APRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00560          10  L12GD-ACLM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00561          10  L12GD-TPRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00562          10  L12GD-TCOM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00563      05  GRAND-T-Y-T-D.                                           ECS035
00564          10  YTDGD-CERT          PIC S9(9)        VALUE ZEROS.    ECS035
00565          10  YTDGD-LBEN          PIC S9(12)V99    VALUE ZEROS.    ECS035
00566          10  YTDGD-LPRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00567          10  YTDGD-LCLM          PIC S9(12)V99    VALUE ZEROS.    ECS035
00568          10  YTDGD-ABEN          PIC S9(11)V99    VALUE ZEROS.    ECS035
00569          10  YTDGD-APRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00570          10  YTDGD-ACLM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00571          10  YTDGD-TPRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00572          10  YTDGD-TCOM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00573      05  LAST-12-ACCOUNT-ACCUM.                                   ECS035
00574          10  L12AC-LBEN          PIC S9(12)V99    VALUE ZEROS.    ECS035
00575          10  L12AC-LPRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00576          10  L12AC-LCLM          PIC S9(12)V99    VALUE ZEROS.    ECS035
00577          10  L12AC-ABEN          PIC S9(11)V99    VALUE ZEROS.    ECS035
00578          10  L12AC-APRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00579          10  L12AC-ACLM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00580          10  L12AC-TPRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00581          10  L12AC-TCOM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00582      05  LAST-12-STATE-ACCUM.                                     ECS035
00583          10  L12ST-LBEN          PIC S9(12)V99    VALUE ZEROS.    ECS035
00584          10  L12ST-LPRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00585          10  L12ST-LCLM          PIC S9(12)V99    VALUE ZEROS.    ECS035
00586          10  L12ST-ABEN          PIC S9(11)V99    VALUE ZEROS.    ECS035
00587          10  L12ST-APRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00588          10  L12ST-ACLM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00589          10  L12ST-TPRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00590          10  L12ST-TCOM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00591      05  LAST-12-GROUPING-ACCUM.                                  ECS035
00592          10  L12GP-LBEN          PIC S9(12)V99    VALUE ZEROS.    ECS035
00593          10  L12GP-LPRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00594          10  L12GP-LCLM          PIC S9(12)V99    VALUE ZEROS.    ECS035
00595          10  L12GP-ABEN          PIC S9(11)V99    VALUE ZEROS.    ECS035
00596          10  L12GP-APRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00597          10  L12GP-ACLM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00598          10  L12GP-TPRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00599          10  L12GP-TCOM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00600      05  LAST-12-CARRIER-ACCUM.                                   ECS035
00601          10  L12CA-LBEN          PIC S9(12)V99    VALUE ZEROS.    ECS035
00602          10  L12CA-LPRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00603          10  L12CA-LCLM          PIC S9(12)V99    VALUE ZEROS.    ECS035
00604          10  L12CA-ABEN          PIC S9(11)V99    VALUE ZEROS.    ECS035
00605          10  L12CA-APRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00606          10  L12CA-ACLM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00607          10  L12CA-TPRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00608          10  L12CA-TCOM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00609      05  YTD-ACCOUNT-ACCUM.                                       ECS035
00610          10  YTDAC-LBEN          PIC S9(12)V99    VALUE ZEROS.    ECS035
00611          10  YTDAC-LPRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00612          10  YTDAC-LCLM          PIC S9(12)V99    VALUE ZEROS.    ECS035
00613          10  YTDAC-ABEN          PIC S9(11)V99    VALUE ZEROS.    ECS035
00614          10  YTDAC-APRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00615          10  YTDAC-ACLM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00616          10  YTDAC-TPRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00617          10  YTDAC-TCOM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00618      05  YTD-STATE-ACCUM.                                         ECS035
00619          10  YTDST-LBEN          PIC S9(12)V99    VALUE ZEROS.    ECS035
00620          10  YTDST-LPRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00621          10  YTDST-LCLM          PIC S9(12)V99    VALUE ZEROS.    ECS035
00622          10  YTDST-ABEN          PIC S9(11)V99    VALUE ZEROS.    ECS035
00623          10  YTDST-APRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00624          10  YTDST-ACLM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00625          10  YTDST-TPRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00626          10  YTDST-TCOM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00627      05  YTD-GROUPING-ACCUM.                                      ECS035
00628          10  YTDGP-LBEN          PIC S9(12)V99    VALUE ZEROS.    ECS035
00629          10  YTDGP-LPRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00630          10  YTDGP-LCLM          PIC S9(12)V99    VALUE ZEROS.    ECS035
00631          10  YTDGP-ABEN          PIC S9(11)V99    VALUE ZEROS.    ECS035
00632          10  YTDGP-APRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00633          10  YTDGP-ACLM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00634          10  YTDGP-TPRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00635          10  YTDGP-TCOM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00636      05  YTD-CARRIER-ACCUM.                                       ECS035
00637          10  YTDCA-LBEN          PIC S9(12)V99    VALUE ZEROS.    ECS035
00638          10  YTDCA-LPRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00639          10  YTDCA-LCLM          PIC S9(12)V99    VALUE ZEROS.    ECS035
00640          10  YTDCA-ABEN          PIC S9(11)V99    VALUE ZEROS.    ECS035
00641          10  YTDCA-APRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00642          10  YTDCA-ACLM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00643          10  YTDCA-TPRM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00644          10  YTDCA-TCOM          PIC S9(11)V99    VALUE ZEROS.    ECS035
00645      05  ZERO-OCCURS-ACCUM.                                       ECS035
00646          10  FILLER              PIC S9(9)        VALUE ZEROS.    ECS035
00647          10  FILLER              PIC S9(12)V99    VALUE ZEROS.    ECS035
00648          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00649          10  FILLER              PIC S9(12)V99    VALUE ZEROS.    ECS035
00650          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00651          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00652          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00653          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00654          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00655      05  ZERO-ACCUM.                                              ECS035
00656          10  FILLER              PIC S9(12)V99    VALUE ZEROS.    ECS035
00657          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00658          10  FILLER              PIC S9(12)V99    VALUE ZEROS.    ECS035
00659          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00660          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00661          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00662          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00663          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00664  EJECT                                                            ECS035
00665  01  PRINT-DECISION-TABLE  COMP-3.                                ECS035
00666      05  PRINT-TABLE OCCURS 12 TIMES.                             ECS035
00667          10  PLBEN               PIC S9(12)V99.                   ECS035
00668          10  PLPRM               PIC S9(11)V99.                   ECS035
00669          10  PLCLM               PIC S9(12)V99.                   ECS035
00670          10  PABEN               PIC S9(11)V99.                   ECS035
00671          10  PAPRM               PIC S9(11)V99.                   ECS035
00672          10  PACLM               PIC S9(11)V99.                   ECS035
00673          10  PTCOM               PIC S9(11)V99.                   ECS035
00674                                                                   ECS035
00675  01  PRINT-ZERO-TABLE  COMP-3.                                    ECS035
00676      05  FILLER                  PIC S9(12)V99    VALUE ZEROS.    ECS035
00677      05  FILLER                  PIC S9(11)V99    VALUE ZEROS.    ECS035
00678      05  FILLER                  PIC S9(12)V99    VALUE ZEROS.    ECS035
00679      05  FILLER                  PIC S9(11)V99    VALUE ZEROS.    ECS035
00680      05  FILLER                  PIC S9(11)V99    VALUE ZEROS.    ECS035
00681      05  FILLER                  PIC S9(11)V99    VALUE ZEROS.    ECS035
00682      05  FILLER                  PIC S9(11)V99    VALUE ZEROS.    ECS035
00683  EJECT                                                            ECS035
00684 **************************************************************    ECS035
00685 * THE FOLLOWING TABLES WERE ADDED FEBRUARY 1990 TO GENERATE  *    ECS035
00686 * A SPECIAL REPORT SHOWING GROSS TOTALS  VS NET TOTALS       *    ECS035
00687 * PER CLIENT REQUEST                                         *    ECS035
00688 **************************************************************    ECS035
00689  01  WORK-ACCUM-T  COMP-3.                                        ECS035
00690      05  CERT-T          PIC S9(9)       VALUE ZEROS.             ECS035
00691      05  CANCEL-T        PIC S9(9)       VALUE ZEROS.             ECS035
00692      05  L-COVERAGE-T    PIC S9(9)       VALUE ZEROS.             ECS035
00693      05  A-COVERAGE-T    PIC S9(9)       VALUE ZEROS.             ECS035
00694      05  LPRM-T          PIC S9(11)V99   VALUE ZEROS.             ECS035
00695      05  LCAN-T          PIC S9(11)V99   VALUE ZEROS.             ECS035
00696      05  LCLM-T          PIC S9(12)V99   VALUE ZEROS.             ECS035
00697      05  APRM-T          PIC S9(11)V99   VALUE ZEROS.             ECS035
00698      05  ACAN-T          PIC S9(11)V99   VALUE ZEROS.             ECS035
00699      05  ACLM-T          PIC S9(12)V99   VALUE ZEROS.             ECS035
00700      05  TPRM-T          PIC S9(11)V99   VALUE ZEROS.             ECS035
00701      05  TCOM-T          PIC S9(11)V99   VALUE ZEROS.             ECS035
00702  EJECT                                                            ECS035
00703  01  ACCOUNT-ACCUMULATORS-T.                                      ECS035
00704      05  ACCOUNT-ACCUM-T         OCCURS 13 TIMES.                 ECS035
00705          10  AC-CERT-T           PIC S9(9)        COMP-3.         ECS035
00706          10  AC-CANCEL-T         PIC S9(9)        COMP-3.         ECS035
00707          10  AC-L-COVERAGE-T     PIC S9(9)        COMP-3.         ECS035
00708          10  AC-A-COVERAGE-T     PIC S9(9)        COMP-3.         ECS035
00709          10  AC-LPRM-T           PIC S9(11)V99    COMP-3.         ECS035
00710          10  AC-LCAN-T           PIC S9(11)V99    COMP-3.         ECS035
00711          10  AC-LCLM-T           PIC S9(12)V99    COMP-3.         ECS035
00712          10  AC-APRM-T           PIC S9(11)V99    COMP-3.         ECS035
00713          10  AC-ACAN-T           PIC S9(11)V99    COMP-3.         ECS035
00714          10  AC-ACLM-T           PIC S9(11)V99    COMP-3.         ECS035
00715          10  AC-TPRM-T           PIC S9(11)V99    COMP-3.         ECS035
00716          10  AC-TCOM-T           PIC S9(11)V99    COMP-3.         ECS035
00717          10  AC-DATE-T           PIC  9(11)       COMP-3.            CL*31
00718      05  ZERO-ACCOUNT-ACCUM-T.                                       CL*12
00719          10  FILLER              PIC S9(9)      COMP-3  VALUE +0. ECS035
00720          10  FILLER              PIC S9(9)      COMP-3  VALUE +0. ECS035
00721          10  FILLER              PIC S9(9)      COMP-3  VALUE +0. ECS035
00722          10  FILLER              PIC S9(9)      COMP-3  VALUE +0. ECS035
00723          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00724          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00725          10  FILLER              PIC S9(12)V99  COMP-3  VALUE +0. ECS035
00726          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00727          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00728          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00729          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00730          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00731          10  FILLER              PIC  9(11)     COMP-3  VALUE  0.    CL*31
00732                                                                   ECS035
00733  01  DATE-RANGE-ACCUMULATORS-T.                                   ECS035
00734      05  DATE-RANGE-ACCUM-T      OCCURS 13 TIMES.                 ECS035
00735          10  DR-CERT-T           PIC S9(9)        COMP-3.         ECS035
00736          10  DR-CANCEL-T         PIC S9(9)        COMP-3.         ECS035
00737          10  DR-L-COVERAGE-T     PIC S9(9)        COMP-3.         ECS035
00738          10  DR-A-COVERAGE-T     PIC S9(9)        COMP-3.         ECS035
00739          10  DR-LPRM-T           PIC S9(11)V99    COMP-3.         ECS035
00740          10  DR-LCAN-T           PIC S9(11)V99    COMP-3.         ECS035
00741          10  DR-LCLM-T           PIC S9(12)V99    COMP-3.         ECS035
00742          10  DR-APRM-T           PIC S9(11)V99    COMP-3.         ECS035
00743          10  DR-ACAN-T           PIC S9(11)V99    COMP-3.         ECS035
00744          10  DR-ACLM-T           PIC S9(11)V99    COMP-3.         ECS035
00745          10  DR-TPRM-T           PIC S9(11)V99    COMP-3.         ECS035
00746          10  DR-TCOM-T           PIC S9(11)V99    COMP-3.         ECS035
00747          10  DR-DATE-T           PIC 9(11)        COMP-3.            CL*24
00748      05  ZERO-DATE-RANGE-ACCUM-T.                                 ECS035
00749          10  FILLER              PIC S9(9)      COMP-3  VALUE +0. ECS035
00750          10  FILLER              PIC S9(9)      COMP-3  VALUE +0. ECS035
00751          10  FILLER              PIC S9(9)      COMP-3  VALUE +0. ECS035
00752          10  FILLER              PIC S9(9)      COMP-3  VALUE +0. ECS035
00753          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00754          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00755          10  FILLER              PIC S9(12)V99  COMP-3  VALUE +0. ECS035
00756          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00757          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00758          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00759          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00760          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. ECS035
00761          10  FILLER              PIC  9(11)     COMP-3  VALUE  0.    CL*31
00762                                                                   ECS035
00763  01  ACCUMULATORS-T COMP-3.                                       ECS035
00764      05  STATE-ACCUM-T           OCCURS 13 TIMES.                 ECS035
00765          10  ST-CERT-T           PIC S9(9).                       ECS035
00766          10  ST-CANCEL-T         PIC S9(9).                       ECS035
00767          10  ST-L-COVERAGE-T     PIC S9(9).                       ECS035
00768          10  ST-A-COVERAGE-T     PIC S9(9).                       ECS035
00769          10  ST-LPRM-T           PIC S9(11)V99.                   ECS035
00770          10  ST-LCAN-T           PIC S9(11)V99.                   ECS035
00771          10  ST-LCLM-T           PIC S9(12)V99.                   ECS035
00772          10  ST-APRM-T           PIC S9(11)V99.                   ECS035
00773          10  ST-ACAN-T           PIC S9(11)V99.                   ECS035
00774          10  ST-ACLM-T           PIC S9(11)V99.                   ECS035
00775          10  ST-TPRM-T           PIC S9(11)V99.                   ECS035
00776          10  ST-TCOM-T           PIC S9(11)V99.                   ECS035
00777      05  GROUPING-ACCUM-T        OCCURS 13 TIMES.                 ECS035
00778          10  GP-CERT-T           PIC S9(9).                       ECS035
00779          10  GP-CANCEL-T         PIC S9(9).                       ECS035
00780          10  GP-L-COVERAGE-T     PIC S9(9).                       ECS035
00781          10  GP-A-COVERAGE-T     PIC S9(9).                       ECS035
00782          10  GP-LPRM-T           PIC S9(11)V99.                   ECS035
00783          10  GP-LCAN-T           PIC S9(11)V99.                   ECS035
00784          10  GP-LCLM-T           PIC S9(12)V99.                   ECS035
00785          10  GP-APRM-T           PIC S9(11)V99.                   ECS035
00786          10  GP-ACAN-T           PIC S9(11)V99.                   ECS035
00787          10  GP-ACLM-T           PIC S9(11)V99.                   ECS035
00788          10  GP-TPRM-T           PIC S9(11)V99.                   ECS035
00789          10  GP-TCOM-T           PIC S9(11)V99.                   ECS035
00790      05  CARRIER-ACCUM-T  OCCURS 13 TIMES.                        ECS035
00791          10  CA-CERT-T           PIC S9(9).                       ECS035
00792          10  CA-CANCEL-T         PIC S9(9).                       ECS035
00793          10  CA-L-COVERAGE-T     PIC S9(9).                       ECS035
00794          10  CA-A-COVERAGE-T     PIC S9(9).                       ECS035
00795          10  CA-LPRM-T           PIC S9(11)V99.                   ECS035
00796          10  CA-LCAN-T           PIC S9(11)V99.                   ECS035
00797          10  CA-LCLM-T           PIC S9(12)V99.                   ECS035
00798          10  CA-APRM-T           PIC S9(11)V99.                   ECS035
00799          10  CA-ACAN-T           PIC S9(11)V99.                   ECS035
00800          10  CA-ACLM-T           PIC S9(11)V99.                   ECS035
00801          10  CA-TPRM-T           PIC S9(11)V99.                   ECS035
00802          10  CA-TCOM-T           PIC S9(11)V99.                   ECS035
00803      05  GRAND-TOTALS-T.                                          ECS035
00804          10  GR-CERT-T           PIC S9(9)      VALUE ZEROS.      ECS035
00805          10  GR-CANCEL-T         PIC S9(9)      VALUE ZEROS.      ECS035
00806          10  GR-L-COVERAGE-T     PIC S9(9)      VALUE ZEROS.      ECS035
00807          10  GR-A-COVERAGE-T     PIC S9(9)      VALUE ZEROS.      ECS035
00808          10  GR-LPRM-T           PIC S9(11)V99  VALUE ZEROS.      ECS035
00809          10  GR-LCAN-T           PIC S9(11)V99  VALUE ZEROS.      ECS035
00810          10  GR-LCLM-T           PIC S9(12)V99  VALUE ZEROS.      ECS035
00811          10  GR-APRM-T           PIC S9(11)V99  VALUE ZEROS.      ECS035
00812          10  GR-ACAN-T           PIC S9(11)V99  VALUE ZEROS.      ECS035
00813          10  GR-ACLM-T           PIC S9(11)V99  VALUE ZEROS.      ECS035
00814          10  GR-TPRM-T           PIC S9(11)V99  VALUE ZEROS.      ECS035
00815          10  GR-TCOM-T           PIC S9(11)V99  VALUE ZEROS.      ECS035
00816      05  GRAND-ACCUM-T   OCCURS 13 TIMES.                         ECS035
00817          10  GT-CERT-T           PIC S9(9).                       ECS035
00818          10  GT-CANCEL-T         PIC S9(9).                       ECS035
00819          10  GT-L-COVERAGE-T     PIC S9(9).                       ECS035
00820          10  GT-A-COVERAGE-T     PIC S9(9).                       ECS035
00821          10  GT-LPRM-T           PIC S9(11)V99.                   ECS035
00822          10  GT-LCAN-T           PIC S9(11)V99.                   ECS035
00823          10  GT-LCLM-T           PIC S9(12)V99.                   ECS035
00824          10  GT-APRM-T           PIC S9(11)V99.                   ECS035
00825          10  GT-ACAN-T           PIC S9(11)V99.                   ECS035
00826          10  GT-ACLM-T           PIC S9(11)V99.                   ECS035
00827          10  GT-TPRM-T           PIC S9(11)V99.                   ECS035
00828          10  GT-TCOM-T           PIC S9(11)V99.                   ECS035
00829      05  GRAND-TOTALS-LAST-12-MO-T.                               ECS035
00830          10  L12GD-CERT-T        PIC S9(9)        VALUE ZEROS.    ECS035
00831          10  L12GD-CANCEL-T      PIC S9(9)        VALUE ZEROS.    ECS035
00832          10  L12GD-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    ECS035
00833          10  L12GD-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    ECS035
00834          10  L12GD-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00835          10  L12GD-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00836          10  L12GD-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    ECS035
00837          10  L12GD-APRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00838          10  L12GD-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00839          10  L12GD-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00840          10  L12GD-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00841          10  L12GD-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00842      05  GRAND-T-Y-T-D-T.                                         ECS035
00843          10  YTDGD-CERT-T        PIC S9(9)        VALUE ZEROS.    ECS035
00844          10  YTDGD-CANCEL-T      PIC S9(9)        VALUE ZEROS.    ECS035
00845          10  YTDGD-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    ECS035
00846          10  YTDGD-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    ECS035
00847          10  YTDGD-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00848          10  YTDGD-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00849          10  YTDGD-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    ECS035
00850          10  YTDGD-APRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00851          10  YTDGD-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00852          10  YTDGD-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00853          10  YTDGD-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00854          10  YTDGD-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00855      05  LAST-12-ACCOUNT-ACCUM-T.                                 ECS035
00856          10  L12AC-CERT-T        PIC S9(9)        VALUE ZEROS.    ECS035
00857          10  L12AC-CANCEL-T      PIC S9(9)        VALUE ZEROS.    ECS035
00858          10  L12AC-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    ECS035
00859          10  L12AC-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    ECS035
00860          10  L12AC-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00861          10  L12AC-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00862          10  L12AC-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    ECS035
00863          10  L12AC-APRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00864          10  L12AC-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00865          10  L12AC-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00866          10  L12AC-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00867          10  L12AC-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00868      05  LAST-12-STATE-ACCUM-T.                                   ECS035
00869          10  L12ST-CERT-T        PIC S9(9)        VALUE ZEROS.    ECS035
00870          10  L12ST-CANCEL-T      PIC S9(9)        VALUE ZEROS.    ECS035
00871          10  L12ST-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    ECS035
00872          10  L12ST-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    ECS035
00873          10  L12ST-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00874          10  L12ST-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00875          10  L12ST-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    ECS035
00876          10  L12ST-APRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00877          10  L12ST-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00878          10  L12ST-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00879          10  L12ST-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00880          10  L12ST-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00881      05  LAST-12-GROUPING-ACCUM-T.                                ECS035
00882          10  L12GP-CERT-T        PIC S9(9)        VALUE ZEROS.    ECS035
00883          10  L12GP-CANCEL-T      PIC S9(9)        VALUE ZEROS.    ECS035
00884          10  L12GP-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    ECS035
00885          10  L12GP-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    ECS035
00886          10  L12GP-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00887          10  L12GP-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00888          10  L12GP-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    ECS035
00889          10  L12GP-APRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00890          10  L12GP-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00891          10  L12GP-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00892          10  L12GP-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00893          10  L12GP-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00894      05  LAST-12-CARRIER-ACCUM-T.                                 ECS035
00895          10  L12CA-CERT-T        PIC S9(9)        VALUE ZEROS.    ECS035
00896          10  L12CA-CANCEL-T      PIC S9(9)        VALUE ZEROS.    ECS035
00897          10  L12CA-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    ECS035
00898          10  L12CA-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    ECS035
00899          10  L12CA-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00900          10  L12CA-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00901          10  L12CA-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    ECS035
00902          10  L12CA-APRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00903          10  L12CA-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00904          10  L12CA-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00905          10  L12CA-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00906          10  L12CA-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00907      05  YTD-ACCOUNT-ACCUM-T.                                     ECS035
00908          10  YTDAC-CERT-T        PIC S9(9)        VALUE ZEROS.    ECS035
00909          10  YTDAC-CANCEL-T      PIC S9(9)        VALUE ZEROS.    ECS035
00910          10  YTDAC-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    ECS035
00911          10  YTDAC-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    ECS035
00912          10  YTDAC-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00913          10  YTDAC-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00914          10  YTDAC-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    ECS035
00915          10  YTDAC-APRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00916          10  YTDAC-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00917          10  YTDAC-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00918          10  YTDAC-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00919          10  YTDAC-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00920      05  YTD-STATE-ACCUM-T.                                       ECS035
00921          10  YTDST-CERT-T        PIC S9(9)        VALUE ZEROS.    ECS035
00922          10  YTDST-CANCEL-T      PIC S9(9)        VALUE ZEROS.    ECS035
00923          10  YTDST-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    ECS035
00924          10  YTDST-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    ECS035
00925          10  YTDST-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00926          10  YTDST-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00927          10  YTDST-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    ECS035
00928          10  YTDST-APRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00929          10  YTDST-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00930          10  YTDST-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00931          10  YTDST-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00932          10  YTDST-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00933      05  YTD-GROUPING-ACCUM-T.                                    ECS035
00934          10  YTDGP-CERT-T        PIC S9(9)        VALUE ZEROS.    ECS035
00935          10  YTDGP-CANCEL-T      PIC S9(9)        VALUE ZEROS.    ECS035
00936          10  YTDGP-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    ECS035
00937          10  YTDGP-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    ECS035
00938          10  YTDGP-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00939          10  YTDGP-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00940          10  YTDGP-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    ECS035
00941          10  YTDGP-APRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00942          10  YTDGP-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00943          10  YTDGP-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00944          10  YTDGP-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00945          10  YTDGP-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00946      05  YTD-CARRIER-ACCUM-T.                                     ECS035
00947          10  YTDCA-CERT-T        PIC S9(9)        VALUE ZEROS.    ECS035
00948          10  YTDCA-CANCEL-T      PIC S9(9)        VALUE ZEROS.    ECS035
00949          10  YTDCA-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    ECS035
00950          10  YTDCA-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    ECS035
00951          10  YTDCA-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00952          10  YTDCA-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00953          10  YTDCA-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    ECS035
00954          10  YTDCA-APRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00955          10  YTDCA-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00956          10  YTDCA-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00957          10  YTDCA-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00958          10  YTDCA-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    ECS035
00959      05  ZERO-OCCURS-ACCUM-T.                                     ECS035
00960          10  FILLER              PIC S9(9)        VALUE ZEROS.    ECS035
00961          10  FILLER              PIC S9(9)        VALUE ZEROS.    ECS035
00962          10  FILLER              PIC S9(9)        VALUE ZEROS.    ECS035
00963          10  FILLER              PIC S9(9)        VALUE ZEROS.    ECS035
00964          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00965          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00966          10  FILLER              PIC S9(12)V99    VALUE ZEROS.    ECS035
00967          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00968          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00969          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00970          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00971          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00972      05  ZERO-ACCUM-T.                                            ECS035
00973          10  FILLER              PIC S9(9)        VALUE ZEROS.    ECS035
00974          10  FILLER              PIC S9(9)        VALUE ZEROS.    ECS035
00975          10  FILLER              PIC S9(9)        VALUE ZEROS.    ECS035
00976          10  FILLER              PIC S9(9)        VALUE ZEROS.    ECS035
00977          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00978          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00979          10  FILLER              PIC S9(12)V99    VALUE ZEROS.    ECS035
00980          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00981          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00982          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00983          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00984          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    ECS035
00985  EJECT                                                            ECS035
00986  01  PRINT-DECISION-TABLE-T  COMP-3.                              ECS035
00987      05  PRINT-TABLE-T OCCURS 12 TIMES.                           ECS035
00988          10  PLPRM-T             PIC S9(11)V99.                   ECS035
00989          10  PLCAN-T             PIC S9(11)V99.                   ECS035
00990          10  PLCLM-T             PIC S9(12)V99.                   ECS035
00991          10  PAPRM-T             PIC S9(11)V99.                   ECS035
00992          10  PACAN-T             PIC S9(11)V99.                   ECS035
00993          10  PACLM-T             PIC S9(11)V99.                   ECS035
00994          10  PTCOM-T             PIC S9(11)V99.                   ECS035
00995                                                                   ECS035
00996  01  PRINT-ZERO-TABLE-T  COMP-3.                                  ECS035
00997      05  FILLER                  PIC S9(11)V99    VALUE ZEROS.    ECS035
00998      05  FILLER                  PIC S9(11)V99    VALUE ZEROS.    ECS035
00999      05  FILLER                  PIC S9(12)V99    VALUE ZEROS.    ECS035
01000      05  FILLER                  PIC S9(11)V99    VALUE ZEROS.    ECS035
01001      05  FILLER                  PIC S9(11)V99    VALUE ZEROS.    ECS035
01002      05  FILLER                  PIC S9(11)V99    VALUE ZEROS.    ECS035
01003      05  FILLER                  PIC S9(11)V99    VALUE ZEROS.    ECS035
01004  EJECT                                                            ECS035
01005  01  CONVERSION-DATE             PIC 9(08) VALUE ZEROS.              CL**4
01006  01  CONVERSION-DATE-R REDEFINES CONVERSION-DATE.                    CL**3
01007      05  CONV-CCYY               PIC 9(4).                           CL*31
01008      05  CONV-MO                 PIC 99.                             CL*22
01009      05  CONV-DA                 PIC 99.                             CL*22
01010                                                                   ECS035
01011  01  COMPARE-DATE-TABLE.                                          ECS035
01012      05  COMPARE-DTS OCCURS 13 TIMES.                             ECS035
01013          10  COMPARE-DT.                                          ECS035
01014              15  COMP-CCYY       PIC 9(04).                       ECS035
01015              15  COMP-CCYR REDEFINES COMP-CCYY.                   ECS035
01016                  20  COMP-CC     PIC 99.                          ECS035
01017                  20  COMP-YR     PIC 99.                          ECS035
01018              15  COMP-MO         PIC 99.                          ECS035
01019                                                                   ECS035
01020  01  COMPARE-DATE9TABLE REDEFINES COMPARE-DATE-TABLE.             ECS035
01021      05  COMPARE9DTS OCCURS 13 TIMES.                             ECS035
01022          10  COMPARE9DT          PIC 9(06).                       ECS035
01023                                                                   ECS035
01024  01  YEAR-OLD-DATE               PIC 9(06).                       ECS035
01025                                                                   ECS035
01026  01  FISCAL-DATE       REDEFINES YEAR-OLD-DATE.                   ECS035
01027      05  FISCAL-CCYY             PIC 9(04).                       ECS035
01028      05  FISCAL-MO               PIC 99.                          ECS035
01029                                                                   ECS035
01030  01  SAVE-EPX.                                                    ECS035
01031      05  S-EPX-CARR              PIC X       VALUE ZEROS.         ECS035
01032      05  S-EPX-GRP               PIC X(6)    VALUE ZEROS.         ECS035
01033      05  S-EPX-STATE             PIC XX      VALUE ZEROS.         ECS035
01034      05  S-EPX-ACCT              PIC X(10)   VALUE ZEROS.         ECS035
01035      05  S-EPX-EXP-DT            PIC 9(11)   COMP-3 VALUE 0.         CL*20
01036                                                                   ECS035
01037  01  ACCOUNT-FULL-CONTROL.                                        ECS035
01038      05  ACCT-CONTROL            PIC X(25)   VALUE LOW-VALUES.    ECS035
01039      05  ACCT-EFF-DT             PIC 9(11) COMP-3 VALUE ZEROES.   ECS035
01040  EJECT                                                            ECS035
01041  01  SAVE-ACCOUNT-DATES.                                          ECS035
01042      05  SAVE-AM-EXPIRE-DT       PIC 9(11).                          CL**9
01043      05  SAVE-AM-EXPIRE-DT-R REDEFINES                               CL**9
01044            SAVE-AM-EXPIRE-DT.                                        CL**9
01045          10  FILLER              PIC 999.                         ECS035
01046          10  SAVE-AM-EXP-CCYY    PIC 9(04).                       ECS035
01047          10  SAVE-AM-EXP-CCYR REDEFINES SAVE-AM-EXP-CCYY.         ECS035
01048              15  SAVE-AM-EXP-CC  PIC 99.                          ECS035
01049              15  SAVE-AM-EXP-YR  PIC 99.                          ECS035
01050          10  SAVE-AM-EXP-MO      PIC 99.                          ECS035
01051          10  SAVE-AM-EXP-DA      PIC 99.                          ECS035
01052      05  SAVE-AM-HI-CERT-DATE    PIC 9(11).                          CL**7
01053      05  SAVE-AM-HI-CERT-DATE-R REDEFINES                            CL**7
01054             SAVE-AM-HI-CERT-DATE.                                    CL**7
01055          10  FILLER              PIC 999.                         ECS035
01056          10  SAVE-AM-HI-CC       PIC 99.                          ECS035
01057          10  SAVE-AM-HI-YR       PIC 99.                          ECS035
01058          10  SAVE-AM-HI-MO       PIC 99.                          ECS035
01059          10  SAVE-AM-HI-DA       PIC 99.                          ECS035
01060                                                                   ECS035
01061  01  SAVE-ACCT-NAME-ADDRESS.                                      ECS035
01062      05  SAVE-ACCT-NAME          PIC X(30)   VALUE SPACES.        ECS035
01063      05  SAVE-ACCT-ADDRS         PIC X(30)   VALUE SPACES.        ECS035
01064                                                                   ECS035
01065  01  EP-DATE.                                                     ECS035
01066      05  E-CCYY                  PIC 9(04).                          CL*31
01067      05  E-MO                    PIC 99.                          ECS035
01068                                                                   ECS035
01069  01  EP9DATE REDEFINES EP-DATE   PIC 9(6).                        ECS035
01070  EJECT                                                            ECS035
01071  COPY ELCDATE.                                                       CL*29
01072                                                                   ECS035
01073                                  COPY ELCDTECX.                   ECS035
01074                                                                   ECS035
01075                                  COPY ELCDTEVR.                   ECS035
01076                                                                   ECS035
01077                                  COPY ELCEPCVR.                   ECS035
01078  EJECT                                                            ECS035
01079  PROCEDURE DIVISION.                                              ECS035
01080                                                                   ECS035
01081  CAPTURE-START.                                                   ECS035
01082                                                                   ECS035
01083      OPEN I-O ERMEBL.                                             ECS035
01084                                                                   ECS035
01085      IF ERMEBL-FILE-STATUS  = '00' OR '97'                        ECS035
01086          NEXT SENTENCE                                            ECS035
01087        ELSE                                                       ECS035
01088          MOVE 'N'                TO ME-UPDATE-FLAG.               ECS035
01089                                                                   ECS035
01090  0100-START-HERE.                                                 ECS035
01091                              COPY ELCDTERX SUPPRESS.              ECS035
01092                                                                   ECS035
01093      MOVE WS-TIME                TO ME-START-TIME.                ECS035
01094      MOVE WS-CURRENT-DATE        TO ME-START-DATE.                ECS035
01095      MOVE ME-START-MO            TO ME-CNDS-MO.                   ECS035
01096      MOVE ME-START-DA            TO ME-CNDS-DA.                   ECS035
01097      MOVE ME-START-YR            TO ME-CNDS-YR.                   ECS035
01098  EJECT                                                            ECS035
01099      MOVE LIFE-OVERRIDE-L6       TO HD-9-LF-OVRD-1                ECS035
01100                                     HD-9-LF-OVRD-2                ECS035
01101                                     HD-9-LF-OVRD-3.               ECS035
01102      MOVE   AH-OVERRIDE-L6       TO HD-9-AH-OVRD-1                ECS035
01103                                     HD-9-AH-OVRD-2                ECS035
01104                                     HD-9-AH-OVRD-3.               ECS035
01105                                                                   ECS035
01106      MOVE DTE-CLIENT             TO ME-COMPANY.                   ECS035
01107      COMPUTE MONTH-END-MOYR = (RUN-CCYY * 12) + RUN-MO.              CL*31
01108      MOVE MONTH-END-MOYR         TO ME-MOYR.                      ECS035
01109                                                                   ECS035
01110      IF ME-DO-UPDATE                                              ECS035
01111          READ ERMEBL INVALID KEY                                  ECS035
01112              MOVE 'N'            TO ME-UPDATE-FLAG                ECS035
01113              CLOSE ERMEBL.                                        ECS035
01114                                                                   ECS035
01115  0110-MOVE-DATES.                                                 ECS035
01116                                                                   ECS035
01117      MOVE DTE-CONV-DT            TO CONVERSION-DATE.              ECS035
01118      MOVE WS-CURRENT-DATE        TO HD-IPL-DATE.                  ECS035
01119      MOVE COMPANY-NAME           TO HD-COMPANY-NAME.              ECS035
01120      MOVE ALPH-DATE              TO HD-ALPHA-DATE.                ECS035
01121      MOVE ZEROS                  TO SAVE-AM-EXPIRE-DT             ECS035
01122                                     SAVE-AM-HI-CERT-DATE.            CL*31
01123                                                                   ECS035
01124      IF DTE-QTR-CO = '1'                                          ECS035
01125          MOVE '1'                TO QTR-COMP.                     ECS035
01126                                                                   ECS035
01127  0120-OPEN-FILES.                                                 ECS035
01128      OPEN INPUT  AM-MAST-IN                                       ECS035
01129                  EARNED-PREM                                      ECS035
01130                  DISK-DATE                                        ECS035
01131           OUTPUT EXTRACT-OT                                       ECS035
01132                  PRNT.                                            ECS035
01133                                                                   ECS035
01134      IF ERACCT-FILE-STATUS NOT = ZERO AND '97'                    ECS035
01135          MOVE 'OPEN ERROR - ERACCTT' TO WS-ABEND-MESSAGE          ECS035
01136          MOVE ERACCT-FILE-STATUS     TO WS-ABEND-FILE-STATUS      ECS035
01137          GO TO ABEND-PGM.                                         ECS035
01138                                                                   ECS035
CIDMOD                                                                  ECS035
030612     IF  DTE-CLIENT = 'CSO' OR 'CID' OR 'AHL'                     ECS035
CIDMOD         MOVE '2' TO DTE-FMT-OPT.                                 ECS035
CIDMOD                                                                  ECS035
01139  0130-CLEAR-TOTAL-AREAS.                                          ECS035
01140      MOVE +000                   TO X1.                           ECS035
01141      PERFORM 0140-ZERO-PRINT-TABLE THRU 0140-EXIT 12 TIMES.       ECS035
01142      MOVE +000                   TO X1.                           ECS035
01143      PERFORM 0150-ZERO-ACCUM-ACC   THRU 0150-EXIT 13 TIMES.       ECS035
01144      MOVE +000                   TO X1.                           ECS035
01145      PERFORM 0155-ZERO-DATE-RANGE-ACC THRU 0155-EXIT 13 TIMES.    ECS035
01146      MOVE +000                   TO X1.                           ECS035
01147      PERFORM 0160-ZERO-ACCUM-ST   THRU  0160-EXIT 13 TIMES.       ECS035
01148      MOVE +000                   TO X1.                           ECS035
01149      PERFORM 0170-ZERO-ACCUM-GRP  THRU 0170-EXIT  13 TIMES.       ECS035
01150      MOVE +000                   TO X1.                           ECS035
01151      PERFORM 0180-ZERO-ACCUM-CARR THRU 0180-EXIT  13 TIMES.       ECS035
01152      MOVE +000                   TO X1.                           ECS035
01153      PERFORM 0190-ZERO-ACCUM-GRAND THRU 0190-EXIT 13 TIMES.       ECS035
01154      MOVE ZERO-OCCURS-ACCUM      TO GRAND-TOTALS.                 ECS035
01155      MOVE ZEROS                  TO EP-CONTROL                    ECS035
01156                                     X1.                           ECS035
01157      PERFORM 0200-ZERO-DATE-AREA THRU 0200-EXIT   13 TIMES.       ECS035
01158      GO TO 0210-FIND-DATE-RANGE.                                  ECS035
01159                                                                   ECS035
01160  0140-ZERO-PRINT-TABLE.                                           ECS035
01161      ADD +1                      TO X1.                           ECS035
01162      MOVE PRINT-ZERO-TABLE       TO PRINT-TABLE (X1).             ECS035
01163      MOVE PRINT-ZERO-TABLE-T     TO PRINT-TABLE-T (X1).           ECS035
01164  0140-EXIT.                                                       ECS035
01165                                                                   ECS035
01166  0150-ZERO-ACCUM-ACC.                                             ECS035
01167      ADD +1                      TO X1.                           ECS035
01168      MOVE ZERO-ACCOUNT-ACCUM     TO ACCOUNT-ACCUM (X1).           ECS035
01169      MOVE ZERO-ACCOUNT-ACCUM-T   TO ACCOUNT-ACCUM-T (X1).         ECS035
01170  0150-EXIT.                                                       ECS035
01171                                                                   ECS035
01172  0155-ZERO-DATE-RANGE-ACC.                                        ECS035
01173      ADD +1                          TO  X1.                      ECS035
01174      MOVE ZERO-DATE-RANGE-ACCUM      TO  DATE-RANGE-ACCUM (X1).   ECS035
01175      MOVE ZERO-DATE-RANGE-ACCUM-T    TO  DATE-RANGE-ACCUM-T (X1). ECS035
01176  0155-EXIT.                                                       ECS035
01177                                                                   ECS035
01178  0160-ZERO-ACCUM-ST.                                              ECS035
01179      ADD +1                      TO X1.                           ECS035
01180      MOVE ZERO-OCCURS-ACCUM      TO STATE-ACCUM (X1).             ECS035
01181      MOVE ZERO-OCCURS-ACCUM-T    TO STATE-ACCUM-T (X1).           ECS035
01182  0160-EXIT.                                                       ECS035
01183                                                                   ECS035
01184  0170-ZERO-ACCUM-GRP.                                             ECS035
01185      ADD +1                      TO X1.                           ECS035
01186      MOVE ZERO-OCCURS-ACCUM      TO GROUPING-ACCUM (X1).          ECS035
01187      MOVE ZERO-OCCURS-ACCUM-T    TO GROUPING-ACCUM-T (X1).        ECS035
01188  0170-EXIT.                                                       ECS035
01189                                                                   ECS035
01190  0180-ZERO-ACCUM-CARR.                                            ECS035
01191      ADD +1                      TO X1.                           ECS035
01192      MOVE ZERO-OCCURS-ACCUM      TO CARRIER-ACCUM (X1).           ECS035
01193      MOVE ZERO-OCCURS-ACCUM-T    TO CARRIER-ACCUM-T (X1).         ECS035
01194  0180-EXIT.                                                       ECS035
01195                                                                   ECS035
01196  0190-ZERO-ACCUM-GRAND.                                           ECS035
01197      ADD +1                      TO X1.                           ECS035
01198      MOVE ZERO-OCCURS-ACCUM      TO GRAND-ACCUM (X1).             ECS035
01199      MOVE ZERO-OCCURS-ACCUM-T    TO GRAND-ACCUM-T (X1).           ECS035
01200  0190-EXIT.                                                       ECS035
01201                                                                   ECS035
01202  0200-ZERO-DATE-AREA.                                             ECS035
01203      ADD 1                       TO X1.                           ECS035
01204      MOVE ZEROES                 TO COMPARE9DT (X1).              ECS035
01205  0200-EXIT.                                                       ECS035
01206                                                                   ECS035
01207  0210-FIND-DATE-RANGE.                                            ECS035
01208      IF QTR-CO                                                    ECS035
01209          IF RUN-MO = 03 OR 06 OR 09 OR 12                         ECS035
01210              MOVE '1'            TO QTR-SW.                       ECS035
01211                                                                   ECS035
01212      MOVE RUN-CCYY               TO RUN-DT-CCYY.                     CL*31
01213      MOVE RUN-MO                 TO RUN-DT-MO.                       CL*31
01214      MOVE +1                     TO X1.                           ECS035
01215      MOVE +0                     TO Y1.                           ECS035
01216                                                                   ECS035
01217      IF QTR-CO AND QTR-END                                        ECS035
01218          COMPUTE COMPARE9DT (X1) = RUN9DT - 300                   ECS035
01219      ELSE                                                         ECS035
01220          COMPUTE COMPARE9DT (X1) = RUN9DT  - 100.                 ECS035
01221                                                                   ECS035
01222      COMPUTE YEAR-OLD-DATE = RUN9DT - 100.                        ECS035
01223                                                                   ECS035
01224 ****** THE FOLLOWING LINES OF CODE ARE FOR AFL ONLY **************ECS035
01225                                                                   ECS035
01226      IF DTE-CLIENT NOT = 'AFL'                                    ECS035
01227          GO TO 0220-BUILD-DATE-TABLE.                             ECS035
01228                                                                   ECS035
01229      IF RUN-MO GREATER 06                                         ECS035
01230          MOVE RUN-CCYY           TO FISCAL-CCYY                   ECS035
01231      ELSE                                                         ECS035
01232          COMPUTE FISCAL-CCYY = RUN-CCYY - 1.                      ECS035
01233                                                                   ECS035
01234      MOVE 06                     TO FISCAL-MO.                    ECS035
01235 ************ END OF AFL CLIENT CODING ****************************ECS035
01236                                                                   ECS035
01237  0220-BUILD-DATE-TABLE.                                           ECS035
01238                                                                   ECS035
01239      ADD +1                      TO X1 Y1.                        ECS035
01240                                                                   ECS035
01241      IF X1 GREATER THAN DATE-RANGE-MAX                            ECS035
01242          GO TO 0230-MODIFY-HEADINGS.                              ECS035
01243                                                                   ECS035
01244      IF QTR-CO AND QTR-END                                        ECS035
01245          COMPUTE COMPARE9DT (X1) = COMPARE9DT (Y1) + 0003         ECS035
01246      ELSE                                                         ECS035
01247          COMPUTE COMPARE9DT (X1) = COMPARE9DT (Y1) + 0001.        ECS035
01248                                                                   ECS035
01249      IF COMP-MO (X1) GREATER THAN 12                              ECS035
01250          COMPUTE COMPARE9DT (X1) = COMPARE9DT (X1) + 0088.        ECS035
01251                                                                   ECS035
01252      GO TO 0220-BUILD-DATE-TABLE.                                 ECS035
01253                                                                   ECS035
01254  0230-MODIFY-HEADINGS.                                            ECS035
01255      IF QTR-CO AND QTR-END                                        ECS035
01256          MOVE 'QUARTERLY'        TO ACCT-PER-HD  STATE-PER-HD     ECS035
01257                                     GRP-PER-HD   CARR-PER-HD.     ECS035
01258  EJECT                                                            ECS035
01259  0240-READ-FIRST-RECORD.                                          ECS035
01260      PERFORM 0250-READ-EARNED-PREM THRU 0259-READ-EXIT.           ECS035
01261      GO TO 0270-SAVE-EP-CONTROL.                                  ECS035
01262                                                                   ECS035
01263  0250-READ-EARNED-PREM.                                           ECS035
01264      READ EARNED-PREM                                             ECS035
01265                  AT END GO TO 9990-END-OF-JOB.                    ECS035
01266                                                                   ECS035
01267      IF EP-REIN = 'R'                                             ECS035
01268          GO TO 0250-READ-EARNED-PREM.                             ECS035
01269                                                                   ECS035
01270      COPY ELCEPCM1.                                               ECS035
01271                                                                   ECS035
01272      IF QTR-CO                                                    ECS035
01273          IF QTR-END                                               ECS035
01274              IF EP-RUN-MO = 03 OR 06 OR 09 OR 12                  ECS035
01275                  NEXT SENTENCE                                    ECS035
01276              ELSE                                                 ECS035
01277                  GO TO 0250-READ-EARNED-PREM                      ECS035
01278          ELSE                                                     ECS035
01279              GO TO 0250-READ-EARNED-PREM.                         ECS035
01280                                                                   ECS035
01281  0259-READ-EXIT.                                                  ECS035
01282      EXIT.                                                        ECS035
01283                                                                   ECS035
01284  0260-COMPARE-RECORDS.                                            ECS035
01285      IF DTE-CLIENT EQUAL 'TMS'                                    ECS035
01286         OR DTE-FMT-OPT EQUAL '2'                                  ECS035
01287          NEXT SENTENCE                                            ECS035
01288      ELSE                                                         ECS035
01289          GO TO 0260-COMPARE-CONTINUE.                             ECS035
01290                                                                   ECS035
01291      IF EP-CARRIER NOT = S-EPX-CARR                               ECS035
01292          PERFORM 2700-CARRIER-BREAK THRU 2750-ZERO-CARRIER        ECS035
01293          GO TO 0270-SAVE-EP-CONTROL.                              ECS035
01294                                                                   ECS035
01295      IF EP-GROUPING NOT = S-EPX-GRP                               ECS035
01296          PERFORM 2630-GROUPING-BREAK THRU 2680-ZERO-GROUPING      ECS035
01297          GO TO 0270-SAVE-EP-CONTROL.                              ECS035
01298                                                                   ECS035
01299      IF EP-STATE NOT = S-EPX-STATE                                ECS035
01300          PERFORM 2560-STATE-BREAK THRU 2610-ZERO-STATE            ECS035
01301          GO TO 0270-SAVE-EP-CONTROL.                              ECS035
01302                                                                   ECS035
01303      IF (EP-EXP-DTE NOT = S-EPX-EXP-DT) OR                           CL*28
01304         (EP-ACCOUNT NOT = S-EPX-ACCT)                             ECS035
01305          MOVE 0                  TO A1                            ECS035
01306          PERFORM 2400-BUILD-EXTRACTS THRU 2470-EXIT               ECS035
01307           VARYING X1 FROM 2 BY 1 UNTIL                            ECS035
01308            X1 IS GREATER THAN DATE-RANGE-MAX                      ECS035
01309          MOVE SPACES             TO  B-E-AGT-TABLE                ECS035
01310          MOVE +0                 TO  X1                           ECS035
01311          PERFORM 0155-ZERO-DATE-RANGE-ACC THRU 0155-EXIT 13 TIMES ECS035
01312          IF EP-ACCOUNT NOT = S-EPX-ACCT                           ECS035
01313              PERFORM 2380-ACCOUNT-BREAK THRU 2540-ZERO-ACCOUNT.   ECS035
01314                                                                   ECS035
01315      GO TO 0270-SAVE-EP-CONTROL.                                  ECS035
01316                                                                   ECS035
01317  0260-COMPARE-CONTINUE.                                           ECS035
01318                                                                   ECS035
01319      IF EP-CARRIER NOT = S-EPX-CARR                               ECS035
01320          PERFORM 0700-CARRIER-BREAK THRU 0750-ZERO-CARRIER        ECS035
01321          GO TO 0270-SAVE-EP-CONTROL.                              ECS035
01322                                                                   ECS035
01323      IF EP-GROUPING NOT = S-EPX-GRP                               ECS035
01324          PERFORM 0630-GROUPING-BREAK THRU 0680-ZERO-GROUPING      ECS035
01325          GO TO 0270-SAVE-EP-CONTROL.                              ECS035
01326                                                                   ECS035
01327      IF EP-STATE NOT = S-EPX-STATE                                ECS035
01328          PERFORM 0560-STATE-BREAK THRU 0610-ZERO-STATE            ECS035
01329          GO TO 0270-SAVE-EP-CONTROL.                              ECS035
01330                                                                   ECS035
01331      IF (EP-EXP-DTE NOT = S-EPX-EXP-DT) OR                           CL*28
01332         (EP-ACCOUNT NOT = S-EPX-ACCT)                             ECS035
01333          MOVE 0                  TO A1                            ECS035
01334          PERFORM 0400-BUILD-EXTRACTS THRU 0470-EXIT               ECS035
01335           VARYING X1 FROM 2 BY 1 UNTIL                            ECS035
01336            X1 IS GREATER THAN DATE-RANGE-MAX                      ECS035
01337          MOVE SPACES             TO  B-E-AGT-TABLE                ECS035
01338          MOVE +0                 TO  X1                           ECS035
01339          PERFORM 0155-ZERO-DATE-RANGE-ACC THRU 0155-EXIT 13 TIMES ECS035
01340          IF EP-ACCOUNT NOT = S-EPX-ACCT                           ECS035
01341              PERFORM 0380-ACCOUNT-BREAK THRU 0540-ZERO-ACCOUNT.   ECS035
01342                                                                   ECS035
01343  0270-SAVE-EP-CONTROL.                                            ECS035
01344      MOVE EP-CONTROL             TO SAVE-EPX.                     ECS035
01345                                                                   ECS035
01346      IF EP-CONTROL NOT = ACCOUNT-FULL-CONTROL                     ECS035
01347          PERFORM 0900-MATCH-ACCT-MASTER THRU 0909-MATCH-EXIT.     ECS035
01348                                                                   ECS035
01349      PERFORM 0290-ACCUMULATE THRU 0370-ACCUMULATE-EXIT.           ECS035
01350                                                                   ECS035
01351      GO TO 0250-READ-EARNED-PREM.                                 ECS035
01352  EJECT                                                            ECS035
01353  0290-ACCUMULATE.                                                 ECS035
01354      MOVE EP-RUN-CCYY            TO E-CCYY.                          CL*31
01355      MOVE EP-RUN-MO              TO E-MO.                         ECS035
01356                                                                   ECS035
01357      IF EP-EXP-DTE  IS GREATER THAN  SAVE-AM-EXPIRE-DT               CL*28
01358          MOVE EP-EXP-DTE         TO  SAVE-AM-EXPIRE-DT.              CL**9
01359                                                                      CL**9
01360      IF EP-HI-CERT  IS GREATER THAN  SAVE-AM-HI-CERT-DATE            CL**9
01361          MOVE EP-HI-CERT         TO  SAVE-AM-HI-CERT-DATE.           CL**9
01362                                                                   ECS035
01363      IF EP9DATE  LESS THAN COMPARE9DT (1)                         ECS035
01364          GO TO 0370-ACCUMULATE-EXIT.                              ECS035
01365                                                                   ECS035
01366      IF (E-CCYY GREATER THAN RUN-CCYY)  OR                        ECS035
01367         (E-CCYY = RUN-CCYY AND E-MO GREATER THAN RUN-MO)             CL*31
01368          GO TO 0370-ACCUMULATE-EXIT.                              ECS035
01369                                                                   ECS035
01370      IF EP-PURGE = 'P'                                            ECS035
01371          GO TO 0320-ACCUMULATE-PURGE.                             ECS035
01372                                                                   ECS035
01373      IF VALID-EP-ID                                               ECS035
01374          GO TO 0300-ACCUMULATE-A.                                 ECS035
01375                                                                   ECS035
01376      IF VALID-EC-ID                                               ECS035
01377          GO TO 0350-ACCUMULATE-1.                                 ECS035
01378                                                                   ECS035
01379      GO TO 0370-ACCUMULATE-EXIT.                                  ECS035
01380                                                                   ECS035
01381  0300-ACCUMULATE-A.                                               ECS035
01382      MOVE +0 TO X1.                                               ECS035
01383                                                                   ECS035
01384  0310-ACCUMULATE-B.                                               ECS035
01385      ADD +1 TO X1.                                                ECS035
01386                                                                   ECS035
01387      IF X1 GREATER THAN DATE-RANGE-MAX                            ECS035
01388          DISPLAY 'DATE TABLE ERROR OR EP-RUN-DATE ERROR'          ECS035
01389          DISPLAY 'EP-DATE ' EP-DATE                               ECS035
01390          DISPLAY 'RUN-DT  ' RUN-DT                                ECS035
01391          DISPLAY 'COMPARE-DATE-TABLE ' COMPARE-DATE-TABLE         ECS035
01392          MOVE '0301'             TO WS-RETURN-CODE                ECS035
01393          GO TO ABEND-PGM.                                         ECS035
01394                                                                   ECS035
01395      IF COMPARE-DT (X1) NOT = EP-DATE                             ECS035
01396          GO TO 0310-ACCUMULATE-B.                                 ECS035
01397                                                                   ECS035
01398      IF EP-HI-CERT  IS GREATER THAN  AC-DATE (X1)                    CL*13
01399          MOVE EP-HI-CERT         TO  AC-DATE (X1)                    CL*13
01400                                      AC-DATE-T (X1)                  CL*13
01401                                      AC-DATE-R (X1).                 CL*13
01402                                                                   ECS035
01403      IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                            ECS035
01404          COMPUTE CERT = EP-ISS-CNT - EP-CNC-CNT                   ECS035
01405          COMPUTE LBEN = EP-ISS-BEN - EP-CNC-BEN                   ECS035
01406          COMPUTE LPRM = EP-ISS-PRM - EP-CNC-PRM                   ECS035
01407          ADD CERT TO AC-CERT (X1)                                 ECS035
01408                      DR-CERT (X1)                                 ECS035
01409          ADD LBEN TO AC-LBEN (X1)                                 ECS035
01410                      DR-LBEN (X1)                                 ECS035
01411          ADD LPRM TO AC-LPRM (X1) AC-TPRM (X1)                    ECS035
01412                      DR-LPRM (X1) DR-TPRM (X1)                    ECS035
01413          ADD EP-CLM-AMT TO AC-LCLM (X1)                           ECS035
01414                            DR-LCLM (X1)                           ECS035
01415 ******  THE FOLLOWING TOTALS ARE NET                              ECS035
01416          ADD EP-ISS-CNT TO AC-CERT-T (X1)                         ECS035
01417                            DR-ISS    (X1)                         ECS035
01418                            DR-CERT-T (X1)                         ECS035
01419                            AC-L-COVERAGE-T (X1)                   ECS035
01420                            DR-L-COVERAGE-T (X1)                   ECS035
01421          ADD EP-CNC-CNT TO AC-CANCEL-T (X1)                       ECS035
01422                            DR-CANCEL-T (X1)                       ECS035
01423          ADD EP-ISS-PRM TO AC-LPRM-T (X1)   AC-TPRM-T (X1)        ECS035
01424                            DR-LPRM-T (X1)   DR-TPRM-T (X1)        ECS035
01425          ADD EP-CNC-PRM TO AC-LCAN-T (X1)                         ECS035
01426                            DR-LCAN-T (X1)                         ECS035
01427          ADD EP-CLM-AMT TO AC-LCLM-T (X1)                         ECS035
01428                            DR-LCLM-T (X1).                        ECS035
01429                                                                   ECS035
01430      IF EP-RCD-TYPE = AH-OVERRIDE-L1                              ECS035
01431          COMPUTE CERT = EP-ISS-CNT - EP-CNC-CNT                   ECS035
01432          COMPUTE ABEN = EP-ISS-BEN - EP-CNC-BEN                   ECS035
01433          COMPUTE APRM = EP-ISS-PRM - EP-CNC-PRM                   ECS035
01434          ADD CERT TO AC-CERT (X1)                                 ECS035
01435                      DR-CERT (X1)                                 ECS035
01436          ADD ABEN TO AC-ABEN (X1)                                 ECS035
01437                      DR-ABEN (X1)                                 ECS035
01438          ADD APRM TO AC-APRM (X1) AC-TPRM (X1)                    ECS035
01439                      DR-APRM (X1) DR-TPRM (X1)                    ECS035
01440          ADD EP-CLM-AMT TO AC-ACLM (X1)                           ECS035
01441                            DR-ACLM (X1)                           ECS035
01442 ******  THE FOLLOWING TOTALS ARE NET                              ECS035
01443          ADD EP-ISS-CNT   TO AC-CERT-T (X1)                       ECS035
01444                              DR-ISS    (X1)                       ECS035
01445                              DR-CERT-T (X1)                       ECS035
01446                              AC-A-COVERAGE-T (X1)                 ECS035
01447                              DR-A-COVERAGE-T (X1)                 ECS035
01448          ADD EP-CNC-CNT   TO AC-CANCEL-T (X1)                     ECS035
01449                              DR-CANCEL-T (X1)                     ECS035
01450          ADD EP-ISS-PRM   TO AC-APRM-T (X1) AC-TPRM-T (X1)        ECS035
01451                              DR-APRM-T (X1) DR-TPRM-T (X1)        ECS035
01452          ADD EP-CNC-PRM   TO AC-ACAN-T (X1)                       ECS035
01453                              DR-ACAN-T (X1)                       ECS035
01454          ADD EP-CLM-AMT   TO AC-ACLM-T (X1)                       ECS035
01455                              DR-ACLM-T (X1).                      ECS035
01456                                                                   ECS035
01457      GO TO 0370-ACCUMULATE-EXIT.                                  ECS035
01458                                                                   ECS035
01459  0320-ACCUMULATE-PURGE.                                           ECS035
01460      IF EP-REIN NOT = ' '                                         ECS035
01461          GO TO 0370-ACCUMULATE-EXIT.                              ECS035
01462                                                                   ECS035
01463      MOVE +0                     TO X1.                           ECS035
01464                                                                   ECS035
01465  0330-ACCUMULATE-PURGE-1.                                         ECS035
01466      ADD +1 TO X1.                                                ECS035
01467                                                                   ECS035
01468      IF X1 GREATER THAN DATE-RANGE-MAX                            ECS035
01469          DISPLAY 'DATE-TABLE ERROR OR EP-RUN-DATE ERROR'          ECS035
01470          DISPLAY 'EP-DATE  '  EP-DATE                             ECS035
01471          DISPLAY 'RUN-DT   '  RUN-DT                              ECS035
01472          DISPLAY 'COMPARE-DATE-TABLE  '  COMPARE-DATE-TABLE       ECS035
01473          MOVE '0301'             TO WS-RETURN-CODE                ECS035
01474          GO TO ABEND-PGM.                                         ECS035
01475                                                                   ECS035
01476      IF COMPARE-DT (X1) LESS THAN EP-DATE                         ECS035
01477          GO TO 0330-ACCUMULATE-PURGE-1.                           ECS035
01478                                                                   ECS035
01479      IF VALID-EC-ID                                               ECS035
01480          IF EC-AGT-TYPE (1) = 'C'  OR  'D'                        ECS035
01481              COMPUTE TCOM = EC-ISS-COMM (1) - EC-CNC-COMM (1)     ECS035
01482              ADD TCOM            TO AC-TCOM (X1)                  ECS035
01483                                     AC-TCOM-T (X1)                ECS035
01484                                     DR-TCOM (X1)                  ECS035
01485                                     DR-TCOM-T (X1).               ECS035
01486      IF VALID-EC-ID                                               ECS035
01487          IF EC-AGT-TYPE (2) = 'C'  OR  'D'                        ECS035
01488              COMPUTE TCOM = EC-ISS-COMM (2) - EC-CNC-COMM (2)     ECS035
01489              COMPUTE TCOM-T = EC-ISS-COMM (2) - EC-CNC-COMM (2)   ECS035
01490              ADD TCOM            TO AC-TCOM (X1)                  ECS035
01491                                     AC-TCOM-T (X1)                ECS035
01492                                     DR-TCOM (X1)                  ECS035
01493                                     DR-TCOM-T (X1).               ECS035
01494      IF VALID-EC-ID                                               ECS035
01495          IF EC-AGT-TYPE (3) = 'C'  OR  'D'                        ECS035
01496              COMPUTE TCOM = EC-ISS-COMM (3) - EC-CNC-COMM (3)     ECS035
01497              COMPUTE TCOM-T = EC-ISS-COMM (3) - EC-CNC-COMM (3)   ECS035
01498              ADD TCOM            TO AC-TCOM (X1)                  ECS035
01499                                     AC-TCOM-T (X1)                ECS035
01500                                     DR-TCOM (X1)                  ECS035
01501                                     DR-TCOM-T (X1).               ECS035
01502      IF VALID-EC-ID                                               ECS035
01503          IF EC-AGT-TYPE (4) = 'C'  OR  'D'                        ECS035
01504              COMPUTE TCOM = EC-ISS-COMM (4) - EC-CNC-COMM (4)     ECS035
01505              COMPUTE TCOM-T = EC-ISS-COMM (4) - EC-CNC-COMM (4)   ECS035
01506              ADD TCOM            TO AC-TCOM (X1)                  ECS035
01507                                     AC-TCOM-T (X1)                ECS035
01508                                     DR-TCOM (X1)                  ECS035
01509                                     DR-TCOM-T (X1).               ECS035
01510      IF VALID-EC-ID                                               ECS035
01511          IF EC-AGT-TYPE (5) = 'C'  OR  'D'                        ECS035
01512              COMPUTE TCOM-T = EC-ISS-COMM (5) - EC-CNC-COMM (5)   ECS035
01513              COMPUTE TCOM = EC-ISS-COMM (5) - EC-CNC-COMM (5)     ECS035
01514              ADD TCOM            TO AC-TCOM (X1)                  ECS035
01515                                     AC-TCOM-T (X1)                ECS035
01516                                     DR-TCOM (X1)                  ECS035
01517                                     DR-TCOM-T (X1).               ECS035
01518                                                                   ECS035
01519      IF NOT VALID-EP-ID                                           ECS035
01520          GO TO 0340-P-LOOP-END.                                   ECS035
01521                                                                   ECS035
01522      IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                            ECS035
01523          COMPUTE CERT = EP-ISS-CNT - EP-CNC-CNT                   ECS035
01524          COMPUTE LBEN = EP-ISS-BEN - EP-CNC-BEN                   ECS035
01525          COMPUTE LPRM = EP-ISS-PRM - EP-CNC-PRM                   ECS035
01526          ADD CERT       TO AC-CERT (X1)                           ECS035
01527                            DR-CERT (X1)                           ECS035
01528          ADD LBEN       TO AC-LBEN (X1)                           ECS035
01529                            DR-LBEN (X1)                           ECS035
01530          ADD LPRM       TO AC-LPRM (X1)                           ECS035
01531                            AC-TPRM (X1)                           ECS035
01532                            DR-LPRM (X1)                           ECS035
01533                            DR-TPRM (X1)                           ECS035
01534          ADD EP-CLM-AMT TO AC-LCLM (X1)                           ECS035
01535                            DR-LCLM (X1)                           ECS035
01536 ******  THE FOLLOWING TOTALS ARE NET                              ECS035
01537          ADD EP-ISS-CNT TO AC-CERT-T (X1)                         ECS035
01538                            DR-ISS    (X1)                         ECS035
01539                            DR-CERT-T (X1)                         ECS035
01540                            AC-L-COVERAGE-T (X1)                   ECS035
01541                            DR-L-COVERAGE-T (X1)                   ECS035
01542          ADD EP-CNC-CNT TO AC-CANCEL-T (X1)                       ECS035
01543                            DR-CANCEL-T (X1)                       ECS035
01544          ADD EP-ISS-PRM TO AC-LPRM-T (X1)                         ECS035
01545                            AC-TPRM-T (X1)                         ECS035
01546                            DR-LPRM-T (X1)                         ECS035
01547                            DR-TPRM-T (X1)                         ECS035
01548          ADD EP-CNC-PRM TO AC-LCAN-T (X1)                         ECS035
01549                            DR-LCAN-T (X1)                         ECS035
01550          ADD EP-CLM-AMT TO AC-LCLM-T (X1)                         ECS035
01551                            DR-LCLM-T (X1).                        ECS035
01552                                                                   ECS035
01553                                                                   ECS035
01554                                                                   ECS035
01555      IF EP-RCD-TYPE = AH-OVERRIDE-L1                              ECS035
01556          COMPUTE CERT = EP-ISS-CNT - EP-CNC-CNT                   ECS035
01557          COMPUTE ABEN = EP-ISS-BEN - EP-CNC-BEN                   ECS035
01558          COMPUTE APRM = EP-ISS-PRM - EP-CNC-PRM                   ECS035
01559          ADD CERT         TO AC-CERT (X1)                         ECS035
01560                              DR-CERT (X1)                         ECS035
01561          ADD ABEN         TO AC-ABEN (X1)                         ECS035
01562                              DR-ABEN (X1)                         ECS035
01563          ADD APRM         TO AC-APRM (X1)                         ECS035
01564                              AC-TPRM (X1)                         ECS035
01565                              DR-APRM (X1)                         ECS035
01566                              DR-TPRM (X1)                         ECS035
01567          ADD EP-CLM-AMT   TO AC-ACLM (X1)                         ECS035
01568                              DR-ACLM (X1)                         ECS035
01569          ADD EP-ISS-CNT   TO AC-CERT-T (X1)                       ECS035
01570                              DR-ISS    (X1)                       ECS035
01571                              DR-CERT-T (X1)                       ECS035
01572                              AC-A-COVERAGE-T (X1)                 ECS035
01573                              DR-A-COVERAGE-T (X1)                 ECS035
01574          ADD EP-CNC-CNT   TO AC-CANCEL-T (X1)                     ECS035
01575                              DR-CANCEL-T (X1)                     ECS035
01576          ADD EP-ISS-PRM   TO AC-APRM-T (X1) AC-TPRM-T (X1)        ECS035
01577                              DR-APRM-T (X1) DR-TPRM-T (X1)        ECS035
01578          ADD EP-CNC-PRM   TO AC-ACAN-T (X1)                       ECS035
01579                              DR-ACAN-T (X1)                       ECS035
01580          ADD EP-CLM-AMT   TO AC-ACLM-T (X1)                       ECS035
01581                              DR-ACLM-T (X1).                      ECS035
01582                                                                   ECS035
01583                                                                   ECS035
01584  0340-P-LOOP-END.                                                 ECS035
01585      IF X1 LESS THAN DATE-RANGE-MAX                               ECS035
01586          GO TO 0330-ACCUMULATE-PURGE-1.                           ECS035
01587                                                                   ECS035
01588      GO TO 0370-ACCUMULATE-EXIT.                                  ECS035
01589                                                                   ECS035
01590  0350-ACCUMULATE-1.                                               ECS035
01591      MOVE +0                     TO X1.                           ECS035
01592                                                                   ECS035
01593  0360-ACCUMULATE-2.                                               ECS035
01594      ADD +1 TO X1.                                                ECS035
01595                                                                   ECS035
01596      IF X1 GREATER THAN DATE-RANGE-MAX                            ECS035
01597          DISPLAY 'DATE TABLE ERROR OR EP-RUN-DATE ERROR '         ECS035
01598          DISPLAY 'EP-DATE ' EP-DATE                               ECS035
01599          DISPLAY 'RUN-DT ' RUN-DATE                               ECS035
01600          DISPLAY 'COMPARE-DATE-TABLE ' COMPARE-DATE-TABLE         ECS035
01601          MOVE '0301'             TO WS-RETURN-CODE                ECS035
01602          GO TO ABEND-PGM.                                         ECS035
01603                                                                   ECS035
01604      IF COMPARE-DT (X1) NOT = EP-DATE                             ECS035
01605          GO TO 0360-ACCUMULATE-2.                                 ECS035
01606                                                                   ECS035
01607      IF EC-AGT-TYPE (1) = 'C'  OR  'D'                            ECS035
01608          COMPUTE TCOM = EC-ISS-COMM (1) - EC-CNC-COMM (1)         ECS035
01609          ADD TCOM           TO AC-TCOM (X1)                       ECS035
01610                                AC-TCOM-T (X1)                     ECS035
01611                                DR-TCOM (X1)                       ECS035
01612                                DR-TCOM-T (X1).                    ECS035
01613      IF EC-AGT-TYPE (2) = 'C'  OR  'D'                            ECS035
01614          COMPUTE TCOM = EC-ISS-COMM (2) - EC-CNC-COMM (2)         ECS035
01615          ADD TCOM TO          AC-TCOM (X1)                        ECS035
01616                               AC-TCOM-T (X1)                      ECS035
01617                               DR-TCOM (X1)                        ECS035
01618                               DR-TCOM-T (X1).                     ECS035
01619      IF EC-AGT-TYPE (3) = 'C'  OR  'D'                            ECS035
01620          COMPUTE TCOM = EC-ISS-COMM (3) - EC-CNC-COMM (3)         ECS035
01621          ADD TCOM TO          AC-TCOM (X1)                        ECS035
01622                               AC-TCOM-T (X1)                      ECS035
01623                               DR-TCOM (X1)                        ECS035
01624                               DR-TCOM-T (X1).                     ECS035
01625      IF EC-AGT-TYPE (4) = 'C'  OR  'D'                            ECS035
01626          COMPUTE TCOM = EC-ISS-COMM (4) - EC-CNC-COMM (4)         ECS035
01627          ADD TCOM TO          AC-TCOM (X1)                        ECS035
01628                               AC-TCOM-T (X1)                      ECS035
01629                               DR-TCOM (X1)                        ECS035
01630                               DR-TCOM-T (X1).                     ECS035
01631      IF EC-AGT-TYPE (5) = 'C'  OR  'D'                            ECS035
01632          COMPUTE TCOM = EC-ISS-COMM (5) - EC-CNC-COMM (5)         ECS035
01633          ADD TCOM TO          AC-TCOM (X1)                        ECS035
01634                               AC-TCOM-T (X1)                      ECS035
01635                               DR-TCOM (X1)                        ECS035
01636                               DR-TCOM-T (X1).                     ECS035
01637                                                                   ECS035
01638  0370-ACCUMULATE-EXIT.                                            ECS035
01639      EXIT.                                                        ECS035
01640  EJECT                                                            ECS035
01641  0380-ACCOUNT-BREAK.                                              ECS035
01642      MOVE +0                     TO X1.                           ECS035
01643      PERFORM 0140-ZERO-PRINT-TABLE THRU 0140-EXIT 12 TIMES.       ECS035
01644      PERFORM 0770-PRINT-DECISION THRU 0800-EXIT.                  ECS035
01645                                                                   ECS035
01646      IF P-ACC-SW = '1'                                            ECS035
01647          MOVE ' '                TO P-ACC-SW                      ECS035
01648      ELSE                                                         ECS035
01649          GO TO 0540-ZERO-ACCOUNT.                                 ECS035
01650                                                                   ECS035
01651      PERFORM 0810-ACC-HD  THRU  0840-EXIT.                        ECS035
01652      MOVE +1                     TO X1.                           ECS035
01653      MOVE +0                     TO Y1.                           ECS035
01654                                                                   ECS035
01655  0390-ACCOUNT-BREAK-PRINT.                                        ECS035
01656      ADD 1               TO X1 Y1.                                ECS035
01657                                                                   ECS035
01658      IF X1 GREATER THAN DATE-RANGE-MAX                            ECS035
01659          GO TO 0520-PRINT-LAST-AC-12.                             ECS035
01660                                                                   ECS035
01661      COMPUTE CERT = AC-CERT (X1) - AC-CERT (Y1).                  ECS035
01662      COMPUTE LBEN = AC-LBEN (X1) - AC-LBEN (Y1).                  ECS035
01663      COMPUTE LPRM = AC-LPRM (X1) - AC-LPRM (Y1).                  ECS035
01664      COMPUTE LCLM = AC-LCLM (X1) - AC-LCLM (Y1).                  ECS035
01665      COMPUTE ABEN = AC-ABEN (X1) - AC-ABEN (Y1).                  ECS035
01666      COMPUTE APRM = AC-APRM (X1) - AC-APRM (Y1).                  ECS035
01667      COMPUTE ACLM = AC-ACLM (X1) - AC-ACLM (Y1).                  ECS035
01668      COMPUTE TCOM = AC-TCOM (X1) - AC-TCOM (Y1).                  ECS035
01669      COMPUTE TPRM = LPRM + APRM.                                  ECS035
01670      MOVE COMP-YR (X1)           TO DET-YR.                       ECS035
01671      MOVE COMP-MO (X1)           TO DET-MO.                       ECS035
01672      PERFORM 0940-LOAD-ALPHA-MONTH THRU 0950-EXIT.                ECS035
01673      MOVE CERT                   TO DET-CERTS.                    ECS035
01674      MOVE LBEN                   TO DET-LBEN.                     ECS035
01675      MOVE LPRM                   TO DET-LPRM.                     ECS035
01676      MOVE LCLM                   TO DET-LCLM.                     ECS035
01677      MOVE ABEN                   TO DET-ABEN.                     ECS035
01678      MOVE APRM                   TO DET-APRM.                     ECS035
01679      MOVE ACLM                   TO DET-ACLM.                     ECS035
01680      MOVE TPRM                   TO DET-TPRM.                     ECS035
01681      MOVE TCOM                   TO DET-TCOM.                     ECS035
01682                                                                   ECS035
01683      IF COMP-CCYY (X1) = CONV-CCYY  AND  COMP-MO (X1) = CONV-MO      CL*31
01684          MOVE ZEROS TO DET-CERTS DET-LBEN DET-LPRM DET-LCLM       ECS035
01685                        DET-ABEN  DET-APRM DET-ACLM                ECS035
01686                        DET-TPRM  DET-TCOM                         ECS035
01687      ELSE                                                         ECS035
01688          PERFORM 0500-ADD-TO-OTHERS  THRU 0510-EXIT.              ECS035
01689                                                                   ECS035
01690 *    PERFORM 0400-BUILD-EXTRACTS THRU 0470-EXIT.                  ECS035
01691                                                                   ECS035
01692      MOVE DETAIL-LINE            TO P-DATA.                       ECS035
01693                                                                   ECS035
01694      IF X1 = 2                                                    ECS035
01695          MOVE '0'                TO X                             ECS035
01696      ELSE                                                         ECS035
01697          MOVE ' ' TO X.                                           ECS035
01698                                                                   ECS035
01699      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
01700                                                                   ECS035
01701      GO TO 0390-ACCOUNT-BREAK-PRINT.                              ECS035
01702  EJECT                                                            ECS035
01703  0400-BUILD-EXTRACTS.                                             ECS035
01704      IF X1 = 2                                                    ECS035
01705          MOVE 1                  TO X1.                           ECS035
01706      ADD 1                       TO A1.                           ECS035
01707  0410-B-E-RESUME.                                                 ECS035
01708                                                                      CL*30
01709      MOVE ZEROS                  TO WS-EE-DTE.                       CL*30
01710      MOVE COMP-CCYY (X1)         TO EE-CCYY.                         CL*31
01711      MOVE COMP-MO (X1)           TO EE-MO.                        ECS035
01712      MOVE WS-EE-DTE              TO EE-DTE.                          CL*15
01713                                                                   ECS035
01714      MOVE SPACE                  TO WS-ISS-FLAG.                  ECS035
01715                                                                   ECS035
01716      IF DTE-CLIENT EQUAL 'HER' OR 'HSL'                           ECS035
01717          COMPUTE WS-ISS-CNT = DR-ISS (X1) - DR-ISS (A1)           ECS035
01718          IF WS-ISS-CNT GREATER THAN ZEROS                         ECS035
01719              MOVE 'Y'            TO WS-ISS-FLAG.                  ECS035
01720                                                                   ECS035
01721      COMPUTE EE-CERT = DR-CERT (X1).                              ECS035
01722      COMPUTE EE-LBEN = DR-LBEN (X1).                              ECS035
01723      COMPUTE EE-LPRM = DR-LPRM (X1).                              ECS035
01724      COMPUTE EE-LCLM = DR-LCLM (X1).                              ECS035
01725      COMPUTE EE-ABEN = DR-ABEN (X1).                              ECS035
01726      COMPUTE EE-APRM = DR-APRM (X1).                              ECS035
01727      COMPUTE EE-ACLM = DR-ACLM (X1).                              ECS035
01728      COMPUTE EE-TPRM = EE-LPRM + EE-APRM.                         ECS035
01729      COMPUTE EE-TCOM = DR-TCOM (X1).                              ECS035
01730      COMPUTE EE-ISS-CNT = DR-ISS (X1).                            ECS035
01731                                                                   ECS035
01732      MOVE AC-DATE (X1)           TO EE-MTH-HI-CERT.               ECS035
01733      MOVE ZERO                   TO EE-CNTL.                      ECS035
01734      MOVE SPACES                 TO EE-ACCT-NAME.                 ECS035
CIDMOD     MOVE SPACES                 TO EE-ACCT-CITY.                 ECS035
010716     IF DTE-CLIENT = 'HER' OR 'HSL' OR 'CID' OR 'DCC' or 'VPP'
030612      OR 'AHL'
022808        EVALUATE AM-STATUS
022808           WHEN '1'
022808              MOVE 'I'           TO EE-ACCT-STATUS
022808           WHEN '2'
022808              MOVE 'T'           TO EE-ACCT-STATUS
022808           WHEN '3'
022808              MOVE 'C'           TO EE-ACCT-STATUS
022808           WHEN '4'
022808              MOVE 'I'           TO EE-ACCT-STATUS
031811           WHEN '5'
031811              MOVE 'S'           TO EE-ACCT-STATUS
021916           WHEN '6'
021916              MOVE 'D'           TO EE-ACCT-STATUS
021916           WHEN '7'
021916              MOVE 'L'           TO EE-ACCT-STATUS
021916           WHEN '8'
021916              MOVE 'R'           TO EE-ACCT-STATUS
021916           WHEN '9'
021916              MOVE 'P'           TO EE-ACCT-STATUS
022808           WHEN OTHER
022808              MOVE 'A'           TO EE-ACCT-STATUS
022808        END-EVALUATE
01743      ELSE
01744          MOVE SPACES             TO EE-ACCT-STATUS
           END-IF

01746      IF AM-REPORT-CODE-1 = SPACES OR ZEROS OR LOW-VALUES          ECS035
01747          NEXT SENTENCE                                            ECS035
01748      ELSE                                                         ECS035
01749          MOVE '4'                TO EE-PASS-NO                    ECS035
01750          MOVE AM-REPORT-CODE-1   TO EE-A-RPT-CD-1                 ECS035
01751          MOVE S-EPX-CARR         TO EE-A-CARR                     ECS035
01752          MOVE S-EPX-GRP          TO EE-A-GROUP                    ECS035
CIDMOD*        MOVE AM-REPORT-CODE-2   TO EE-A-RPT-CD-2                 ECS035
CIDMOD         MOVE SPACES             TO EE-A-RPT-CD-2                 ECS035
01754          MOVE S-EPX-STATE        TO EE-A-STATE                    ECS035
01755          MOVE S-EPX-ACCT         TO EE-A-ACCT                     ECS035
01756          MOVE AM-NAME            TO EE-ACCT-NAME                  ECS035
051810         MOVE SPACES             TO EE-ACCT-CITY                  ECS035
051810         STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
051810            DELIMITED BY '  ' INTO EE-ACCT-CITY
051810         END-STRING
01757          PERFORM 0480-WRITE-EXTRACT THRU 0490-EXIT.               ECS035
01758                                                                   ECS035
01759      IF AM-REPORT-CODE-2 = SPACES OR ZEROS OR LOW-VALUES          ECS035
01760          NEXT SENTENCE                                            ECS035
01761      ELSE                                                         ECS035
01762          MOVE '5'                TO EE-PASS-NO                    ECS035
01763          MOVE LOW-VALUES         TO EE-A-RPT-CD-1                 ECS035
01764          MOVE S-EPX-CARR         TO EE-A-CARR                     ECS035
01765          MOVE S-EPX-GRP          TO EE-A-GROUP                    ECS035
01766          MOVE AM-REPORT-CODE-2   TO EE-A-RPT-CD-2                 ECS035
01767          MOVE S-EPX-STATE        TO EE-A-STATE                    ECS035
01768          MOVE S-EPX-ACCT         TO EE-A-ACCT                     ECS035
01769          MOVE AM-NAME            TO EE-ACCT-NAME                  ECS035
051810         MOVE SPACES             TO EE-ACCT-CITY                  ECS035
051810         STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
051810            DELIMITED BY '  ' INTO EE-ACCT-CITY
051810         END-STRING
01770          PERFORM 0480-WRITE-EXTRACT THRU 0490-EXIT.               ECS035
01771                                                                   ECS035
01772      MOVE ZERO                   TO EE-CNTL.                      ECS035
01773                                                                   ECS035
01774      MOVE '1'                    TO EE-PASS-NO.                   ECS035
01775      MOVE S-EPX-STATE            TO EE-CNTL-1.                    ECS035
01776      PERFORM 0480-WRITE-EXTRACT THRU 0490-EXIT.                   ECS035
01777                                                                   ECS035
01778      MOVE '2'                    TO EE-PASS-NO.                   ECS035
01779      MOVE AM-GPCD                TO EE-CNTL-1.                    ECS035
01780      MOVE S-EPX-GRP              TO EE-GROUP.                     ECS035
01781      MOVE S-EPX-CARR             TO EE-CARR.                      ECS035
01782      PERFORM 0480-WRITE-EXTRACT THRU 0490-EXIT.                   ECS035
01783                                                                   ECS035
01784      MOVE '3'                    TO EE-PASS-NO.                   ECS035
01785                                                                   ECS035
051810     MOVE SPACES                 TO EE-ACCT-CITY                  ECS035
051810     STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
051810        DELIMITED BY '  ' INTO EE-ACCT-CITY
051810     END-STRING
01786      MOVE AM-NAME                TO EE-ACCT-NAME.                 ECS035
01787                                                                   ECS035
01788      MOVE SAVE-AM-EXPIRE-DT      TO EE-AM-EXPIRES.                ECS035
01789      MOVE SAVE-AM-HI-CERT-DATE   TO EE-AM-HI-CERT.                ECS035
01790                                                                   ECS035
01791      MOVE 1                      TO Z                             ECS035
01792                                     Z1.                           ECS035
01793                                                                   ECS035
01794  0420-B-E-LOOP-1.                                                 ECS035
01795      ADD 1 TO Z.                                                  ECS035
01796                                                                   ECS035
01797      IF Z GREATER 10                                              ECS035
01798          MOVE 0                  TO Z Z1                          ECS035
01799          GO TO 0430-B-E-LOOP-2.                                   ECS035
01800                                                                   ECS035
01801      IF (AM-COM-TYP (Z) = 'O' OR 'P'
052814         OR 'B' OR 'I' OR 'L' OR 'K' OR 'S')
01802          MOVE AM-AGT (Z)         TO B-E-AGT (Z1)                  ECS035
01803          ADD 1                   TO Z1.                           ECS035
01804                                                                   ECS035
01805      GO TO 0420-B-E-LOOP-1.                                       ECS035
01806                                                                   ECS035
01807  0430-B-E-LOOP-2.                                                 ECS035
01808      ADD 1 TO Z.                                                  ECS035
01809                                                                   ECS035
01810      IF Z GREATER 9                                               ECS035
01811          MOVE 0                  TO Z Z1                          ECS035
01812          GO TO 0450-B-E-LOOP-4.                                   ECS035
01813                                                                   ECS035
01814  0440-B-E-LOOP-3.                                                 ECS035
01815      ADD 1 TO Z1.                                                 ECS035
01816                                                                   ECS035
01817      IF Z1 GREATER 9                                              ECS035
01818          MOVE 0                  TO Z1                            ECS035
01819          GO TO 0430-B-E-LOOP-2.                                   ECS035
01820                                                                   ECS035
01821      IF Z = Z1                                                    ECS035
01822          GO TO 0440-B-E-LOOP-3.                                   ECS035
01823                                                                   ECS035
01824      IF B-E-AGT (Z) = B-E-AGT (Z1)                                ECS035
01825          MOVE SPACE              TO B-E-AGT (Z1).                 ECS035
01826                                                                   ECS035
01827      GO TO 0440-B-E-LOOP-3.                                       ECS035
01828                                                                   ECS035
01829  0450-B-E-LOOP-4.                                                 ECS035
01830      ADD 1 TO Z.                                                  ECS035
01831                                                                   ECS035
01832      IF DTE-CLIENT = 'HER'                                        ECS035
01833          IF Z GREATER 1                                           ECS035
01834              GO TO 0460-B-E-CHECK.                                ECS035
01835                                                                   ECS035
01836      IF Z GREATER 9                                               ECS035
01837          GO TO 0460-B-E-CHECK.                                    ECS035
01838                                                                   ECS035
01839      IF (B-E-AGT (Z) = SPACE OR ZERO OR LOW-VALUES)               ECS035
01840          GO TO 0450-B-E-LOOP-4.                                   ECS035
01841                                                                   ECS035
01842      MOVE B-E-AGT (Z)            TO EE-CNTL-GA.                   ECS035
01843      MOVE S-EPX-ACCT             TO EE-CNTL-ACCT.                 ECS035
01844                                                                   ECS035
01845      IF DTE-CLIENT = 'HER'  OR  'VSL'  OR  'MON' OR 'HSL'         ECS035
01846          PERFORM 0480-WRITE-EXTRACT THRU 0490-EXIT.               ECS035
01847                                                                   ECS035
01848      MOVE HIGH-VALUE             TO EE-CNTL-ACCT.                 ECS035
01849      MOVE ZEROS                  TO EE-AM-EXPIRES                 ECS035
01850                                     EE-AM-HI-CERT.                ECS035
01851                                                                   ECS035
01852      PERFORM 0480-WRITE-EXTRACT THRU 0490-EXIT.                   ECS035
01853                                                                   ECS035
01854      MOVE SAVE-AM-EXPIRE-DT      TO EE-AM-EXPIRES.                ECS035
01855      MOVE SAVE-AM-HI-CERT-DATE   TO EE-AM-HI-CERT.                ECS035
01856                                                                   ECS035
01857      GO TO 0450-B-E-LOOP-4.                                       ECS035
01858                                                                   ECS035
01859  0460-B-E-CHECK.                                                  ECS035
01860      IF X1 = 1                                                    ECS035
01861          MOVE 2                  TO X1                            ECS035
01862          GO TO 0410-B-E-RESUME.                                   ECS035
01863                                                                   ECS035
01864  0470-EXIT.                                                       ECS035
01865       EXIT.                                                       ECS035
01866                                                                   ECS035
01867  EJECT                                                            ECS035
01868  0480-WRITE-EXTRACT.                                              ECS035
01869                                                                   ECS035
01870      MOVE X1                   TO  SAVE-X1.                       ECS035
01871      MOVE +0                   TO  X1.                            ECS035
01872      PERFORM 0140-ZERO-PRINT-TABLE THRU 0140-EXIT 12 TIMES.       ECS035
01873      PERFORM 0770-PRINT-DECISION THRU 0800-EXIT.                  ECS035
01874      IF P-ACC-SW IS EQUAL TO '1'                                  ECS035
01875          MOVE SAVE-X1          TO  X1                             ECS035
01876      ELSE                                                         ECS035
01877          MOVE SAVE-X1          TO  X1                             ECS035
01878          GO TO 0490-EXIT.                                         ECS035
01879                                                                   ECS035
01880      IF DTE-CLIENT EQUAL 'HER' OR 'HSL'                           ECS035
01881         NEXT SENTENCE                                             ECS035
01882      ELSE                                                         ECS035
01883      IF EE-CERT = ZERO AND EE-LBEN = ZERO AND EE-LPRM = ZERO AND  ECS035
01884         EE-LCLM = ZERO AND EE-ABEN = ZERO AND EE-APRM = ZERO AND  ECS035
01885         EE-ACLM = ZERO AND EE-TPRM = ZERO AND EE-TCOM = ZERO      ECS035
01886         GO TO 0490-EXIT.                                          ECS035
01887                                                                   ECS035
01888      IF DTE-CLIENT EQUAL 'HER' OR 'HSL'                           ECS035
01889         IF EE-PASS-NO EQUAL '3'                                   ECS035
01890             MOVE WS-ISS-FLAG         TO EE-ISS-FLAG.              ECS035
01891                                                                   ECS035
01892      WRITE EXTRACT-OT-REC FROM WS-EXTR-REC.                       ECS035
01893                                                                   ECS035
01894      MOVE SPACE                      TO EE-ISS-FLAG.              ECS035
01895                                                                   ECS035
01896  0490-EXIT.                                                       ECS035
01897       EXIT.                                                       ECS035
01898                                                                   ECS035
01899  0500-ADD-TO-OTHERS.                                              ECS035
01900      ADD CERT TO ST-CERT (Y1) GP-CERT (Y1) CA-CERT (Y1) GR-CERT   ECS035
01901                  GT-CERT (Y1).                                    ECS035
01902      ADD LBEN TO ST-LBEN (Y1) GP-LBEN (Y1) CA-LBEN (Y1) GR-LBEN   ECS035
01903                  GT-LBEN (Y1).                                    ECS035
01904      ADD LPRM TO ST-LPRM (Y1) GP-LPRM (Y1) CA-LPRM (Y1) GR-LPRM   ECS035
01905                  GT-LPRM (Y1).                                    ECS035
01906      ADD LCLM TO ST-LCLM (Y1) GP-LCLM (Y1) CA-LCLM (Y1) GR-LCLM   ECS035
01907                  GT-LCLM (Y1).                                    ECS035
01908      ADD ABEN TO ST-ABEN (Y1) GP-ABEN (Y1) CA-ABEN (Y1) GR-ABEN   ECS035
01909                  GT-ABEN (Y1).                                    ECS035
01910      ADD APRM TO ST-APRM (Y1) GP-APRM (Y1) CA-APRM (Y1) GR-APRM   ECS035
01911                  GT-APRM (Y1).                                    ECS035
01912      ADD ACLM TO ST-ACLM (Y1) GP-ACLM (Y1) CA-ACLM (Y1) GR-ACLM   ECS035
01913                  GT-ACLM (Y1).                                    ECS035
01914      ADD TPRM TO ST-TPRM (Y1) GP-TPRM (Y1) CA-TPRM (Y1) GR-TPRM   ECS035
01915                  GT-TPRM (Y1).                                    ECS035
01916      ADD TCOM TO ST-TCOM (Y1) GP-TCOM (Y1) CA-TCOM (Y1) GR-TCOM   ECS035
01917                  GT-TCOM (Y1).                                    ECS035
01918                                                                   ECS035
01919      IF COMPARE9DT (X1) GREATER THAN YEAR-OLD-DATE                ECS035
01920          ADD LBEN TO L12AC-LBEN L12ST-LBEN L12GP-LBEN L12CA-LBEN  ECS035
01921                      L12GD-LBEN                                   ECS035
01922          ADD LPRM TO L12AC-LPRM L12ST-LPRM L12GP-LPRM L12CA-LPRM  ECS035
01923                      L12GD-LPRM                                   ECS035
01924          ADD LCLM TO L12AC-LCLM L12ST-LCLM L12GP-LCLM L12CA-LCLM  ECS035
01925                      L12GD-LCLM                                   ECS035
01926          ADD ABEN TO L12AC-ABEN L12ST-ABEN L12GP-ABEN L12CA-ABEN  ECS035
01927                      L12GD-ABEN                                   ECS035
01928          ADD APRM TO L12AC-APRM L12ST-APRM L12GP-APRM L12CA-APRM  ECS035
01929                      L12GD-APRM                                   ECS035
01930          ADD ACLM TO L12AC-ACLM L12ST-ACLM L12GP-ACLM L12CA-ACLM  ECS035
01931                      L12GD-ACLM                                   ECS035
01932          ADD TPRM TO L12AC-TPRM L12ST-TPRM L12GP-TPRM L12CA-TPRM  ECS035
01933                      L12GD-TPRM                                   ECS035
01934          ADD TCOM TO L12AC-TCOM L12ST-TCOM L12GP-TCOM L12CA-TCOM  ECS035
01935                      L12GD-TCOM.                                  ECS035
01936                                                                   ECS035
01937      IF RUN-CCYY = COMP-CCYY (X1)                                    CL*31
01938          ADD LBEN TO YTDAC-LBEN YTDST-LBEN YTDGP-LBEN YTDCA-LBEN  ECS035
01939                      YTDGD-LBEN                                   ECS035
01940          ADD LPRM TO YTDAC-LPRM YTDST-LPRM YTDGP-LPRM YTDCA-LPRM  ECS035
01941                      YTDGD-LPRM                                   ECS035
01942                      YTDAC-TPRM YTDST-TPRM YTDGP-TPRM YTDCA-TPRM  ECS035
01943                      YTDGD-TPRM                                   ECS035
01944          ADD LCLM TO YTDAC-LCLM YTDST-LCLM YTDGP-LCLM YTDCA-LCLM  ECS035
01945                      YTDGD-LCLM                                   ECS035
01946          ADD TCOM TO YTDAC-TCOM YTDST-TCOM YTDGP-TCOM YTDCA-TCOM  ECS035
01947                      YTDGD-TCOM                                   ECS035
01948          ADD ABEN TO YTDAC-ABEN YTDST-ABEN YTDGP-ABEN YTDCA-ABEN  ECS035
01949                      YTDGD-ABEN                                   ECS035
01950          ADD APRM TO YTDAC-APRM YTDST-APRM YTDGP-APRM YTDCA-APRM  ECS035
01951                      YTDGD-APRM                                   ECS035
01952                      YTDAC-TPRM YTDST-TPRM YTDGP-TPRM YTDCA-TPRM  ECS035
01953                      YTDGD-TPRM                                   ECS035
01954          ADD ACLM TO YTDAC-ACLM YTDST-ACLM YTDGP-ACLM YTDCA-ACLM  ECS035
01955                      YTDGD-ACLM.                                  ECS035
01956                                                                   ECS035
01957  0510-EXIT.                                                       ECS035
01958       EXIT.                                                       ECS035
01959                                                                   ECS035
01960  0520-PRINT-LAST-AC-12.                                           ECS035
01961      MOVE L12AC-LBEN             TO DET-LBEN.                     ECS035
01962      MOVE L12AC-LPRM             TO DET-LPRM.                     ECS035
01963      MOVE L12AC-LCLM             TO DET-LCLM.                     ECS035
01964      MOVE L12AC-ABEN             TO DET-ABEN.                     ECS035
01965      MOVE L12AC-APRM             TO DET-APRM.                     ECS035
01966      MOVE L12AC-ACLM             TO DET-ACLM.                     ECS035
01967      MOVE L12AC-TPRM             TO DET-TPRM.                     ECS035
01968      MOVE L12AC-TCOM             TO DET-TCOM.                     ECS035
01969      MOVE 'LAST 12 MONTHS'       TO DET-TITLE.                    ECS035
01970                                                                   ECS035
01971 ****** AFL CODING **************                                  ECS035
01972                                                                   ECS035
01973      IF DTE-CLIENT = 'AFL'                                        ECS035
01974          MOVE 'FISCAL TO DATE'   TO DET-TITLE.                    ECS035
01975                                                                   ECS035
01976 ****** END OF AFL CODING ***************                          ECS035
01977                                                                   ECS035
01978      MOVE '0'                    TO X.                            ECS035
01979      MOVE DETAIL-LINE            TO P-DATA.                       ECS035
01980      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
01981      MOVE ZERO-ACCUM             TO LAST-12-ACCOUNT-ACCUM.        ECS035
01982                                                                   ECS035
01983  0530-PRINT-AC-YTD.                                               ECS035
01984      MOVE YTDAC-LBEN             TO DET-LBEN.                     ECS035
01985      MOVE YTDAC-LPRM             TO DET-LPRM.                     ECS035
01986      MOVE YTDAC-LCLM             TO DET-LCLM.                     ECS035
01987      MOVE YTDAC-ABEN             TO DET-ABEN.                     ECS035
01988      MOVE YTDAC-APRM             TO DET-APRM.                     ECS035
01989      MOVE YTDAC-ACLM             TO DET-ACLM.                     ECS035
01990      MOVE YTDAC-TPRM             TO DET-TPRM.                     ECS035
01991      MOVE YTDAC-TCOM             TO DET-TCOM.                     ECS035
01992      MOVE 'YEAR TO DATE'         TO DET-TITLE.                    ECS035
01993      MOVE ' '                    TO X.                            ECS035
01994      MOVE DETAIL-LINE            TO P-DATA.                       ECS035
01995      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
01996      MOVE SPACES                 TO DET-TITLE.                    ECS035
01997      MOVE ZERO-ACCUM             TO YTD-ACCOUNT-ACCUM.            ECS035
01998                                                                   ECS035
01999      IF DTE-CLIENT  =  'MON'                                      ECS035
02000          NEXT SENTENCE                                            ECS035
02001      ELSE                                                         ECS035
02002          GO TO 0540-ZERO-ACCOUNT.                                 ECS035
02003                                                                   ECS035
02004      MOVE 'HIGH CERT DATE'       TO  DET-DATE-DESC.               ECS035
02005      MOVE SAVE-AM-HI-MO          TO  DET-DATE-MO.                 ECS035
02006      MOVE SAVE-AM-HI-DA          TO  DET-DATE-DA.                 ECS035
02007      MOVE SAVE-AM-HI-YR          TO  DET-DATE-YR.                 ECS035
02008      MOVE '/'                    TO  DET-DATE-SLASH-1             ECS035
02009                                      DET-DATE-SLASH-2.            ECS035
02010      MOVE ' '                    TO  X.                           ECS035
02011      MOVE DET-DATE-LINE          TO  P-DATA.                      ECS035
02012                                                                   ECS035
02013      PERFORM 0920-WRITE-PRINT  THRU  0930-EXIT.                   ECS035
02014                                                                   ECS035
02015      MOVE SPACES                 TO  DET-DATE-LINE.                  CL**8
02016      MOVE ZEROS                  TO  SAVE-AM-HI-CERT-DATE.           CL*31
02017      MOVE 'EXPIRATION DATE'      TO  DET-DATE-DESC.               ECS035
02018      MOVE SAVE-AM-EXP-MO         TO  DET-DATE-MO.                 ECS035
02019      MOVE SAVE-AM-EXP-DA         TO  DET-DATE-DA.                 ECS035
02020      MOVE SAVE-AM-EXP-YR         TO  DET-DATE-YR.                 ECS035
02021      MOVE '/'                    TO  DET-DATE-SLASH-1             ECS035
02022                                      DET-DATE-SLASH-2.            ECS035
02023      MOVE ' '                    TO  X.                           ECS035
02024      MOVE DET-DATE-LINE          TO  P-DATA.                      ECS035
02025                                                                   ECS035
02026      PERFORM 0920-WRITE-PRINT  THRU  0930-EXIT.                   ECS035
02027                                                                   ECS035
02028      MOVE SPACES                 TO  DET-DATE-LINE.               ECS035
02029      MOVE ZEROS                  TO  SAVE-AM-EXPIRE-DT.              CL**9
02030                                                                   ECS035
02031  0540-ZERO-ACCOUNT.                                               ECS035
02032      MOVE +0                     TO X1.                           ECS035
02033      PERFORM 0150-ZERO-ACCUM-ACC THRU 0150-EXIT 13 TIMES.         ECS035
02034  EJECT                                                            ECS035
02035  0560-STATE-BREAK.                                                ECS035
02036      MOVE 0                     TO A1                             ECS035
02037      PERFORM 0400-BUILD-EXTRACTS THRU 0470-EXIT                   ECS035
02038       VARYING X1 FROM 2 BY 1 UNTIL                                ECS035
02039        X1 IS GREATER THAN DATE-RANGE-MAX.                         ECS035
02040      MOVE SPACES                 TO  B-E-AGT-TABLE.               ECS035
02041      MOVE +0                     TO  X1.                          ECS035
02042      PERFORM 0155-ZERO-DATE-RANGE-ACC THRU 0155-EXIT 13 TIMES.    ECS035
02043      PERFORM 0380-ACCOUNT-BREAK THRU 0540-ZERO-ACCOUNT.           ECS035
02044                                                                   ECS035
02045  0570-STATE-BREAK-1.                                              ECS035
02046      IF P-ST-SW = '1'                                             ECS035
02047          MOVE ' '                TO P-ST-SW                       ECS035
02048      ELSE                                                         ECS035
02049          GO TO 0610-ZERO-STATE.                                   ECS035
02050                                                                   ECS035
02051      PERFORM 0850-ST-HD          THRU 0850-EXIT.                  ECS035
02052      MOVE +1                     TO X1.                           ECS035
02053                                                                   ECS035
02054  0580-STATE-BREAK-2.                                              ECS035
02055      IF X1 GREATER THAN DATE-RANGE-MAX-1                          ECS035
02056          GO TO 0590-PRINT-LAST-ST-12.                             ECS035
02057                                                                   ECS035
02058      MOVE ST-CERT (X1)           TO DET-CERTS.                    ECS035
02059      MOVE ST-LBEN (X1)           TO DET-LBEN.                     ECS035
02060      MOVE ST-LPRM (X1)           TO DET-LPRM.                     ECS035
02061      MOVE ST-LCLM (X1)           TO DET-LCLM.                     ECS035
02062      MOVE ST-ABEN (X1)           TO DET-ABEN.                     ECS035
02063      MOVE ST-APRM (X1)           TO DET-APRM.                     ECS035
02064      MOVE ST-ACLM (X1)           TO DET-ACLM.                     ECS035
02065      MOVE ST-TPRM (X1)           TO DET-TPRM.                     ECS035
02066      MOVE ST-TCOM (X1)           TO DET-TCOM.                     ECS035
02067      ADD +1                      TO X1.                           ECS035
02068      MOVE COMP-YR (X1)           TO DET-YR.                       ECS035
02069      MOVE COMP-MO (X1)           TO DET-MO.                       ECS035
02070      PERFORM 0940-LOAD-ALPHA-MONTH THRU 0950-EXIT.                ECS035
02071      MOVE DETAIL-LINE            TO P-DATA.                       ECS035
02072                                                                   ECS035
02073      IF X1 = 2                                                    ECS035
02074          MOVE '0'                TO X                             ECS035
02075      ELSE                                                         ECS035
02076          MOVE ' '                TO X.                            ECS035
02077                                                                   ECS035
02078      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02079      GO TO 0580-STATE-BREAK-2.                                    ECS035
02080                                                                   ECS035
02081  0590-PRINT-LAST-ST-12.                                           ECS035
02082      MOVE L12ST-LBEN             TO DET-LBEN.                     ECS035
02083      MOVE L12ST-LPRM             TO DET-LPRM.                     ECS035
02084      MOVE L12ST-LCLM             TO DET-LCLM.                     ECS035
02085      MOVE L12ST-ABEN             TO DET-ABEN.                     ECS035
02086      MOVE L12ST-APRM             TO DET-APRM.                     ECS035
02087      MOVE L12ST-ACLM             TO DET-ACLM.                     ECS035
02088      MOVE L12ST-TPRM             TO DET-TPRM.                     ECS035
02089      MOVE L12ST-TCOM             TO DET-TCOM.                     ECS035
02090      MOVE 'LAST 12 MONTHS'       TO DET-TITLE.                    ECS035
02091                                                                   ECS035
02092 ****** AFL CODING ****************                                ECS035
02093                                                                   ECS035
02094      IF DTE-CLIENT = 'AFL'                                        ECS035
02095          MOVE 'FISCAL TO DATE'   TO DET-TITLE.                    ECS035
02096                                                                   ECS035
02097 ****** END OF AFL CODING *********                                ECS035
02098                                                                   ECS035
02099      MOVE '0'                    TO X.                            ECS035
02100      MOVE DETAIL-LINE            TO P-DATA.                       ECS035
02101      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02102      MOVE ZERO-ACCUM             TO LAST-12-STATE-ACCUM.          ECS035
02103                                                                   ECS035
02104  0600-PRINT-ST-YTD.                                               ECS035
02105      MOVE YTDST-LBEN             TO DET-LBEN.                     ECS035
02106      MOVE YTDST-LPRM             TO DET-LPRM.                     ECS035
02107      MOVE YTDST-LCLM             TO DET-LCLM.                     ECS035
02108      MOVE YTDST-ABEN             TO DET-ABEN.                     ECS035
02109      MOVE YTDST-APRM             TO DET-APRM.                     ECS035
02110      MOVE YTDST-ACLM             TO DET-ACLM.                     ECS035
02111      MOVE YTDST-TPRM             TO DET-TPRM.                     ECS035
02112      MOVE YTDST-TCOM             TO DET-TCOM.                     ECS035
02113      MOVE 'YEAR TO DATE'         TO DET-TITLE.                    ECS035
02114      MOVE ' '                    TO X.                            ECS035
02115      MOVE DETAIL-LINE            TO P-DATA.                       ECS035
02116      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02117      MOVE SPACES                 TO DET-TITLE.                    ECS035
02118      MOVE ZERO-ACCUM             TO YTD-STATE-ACCUM.              ECS035
02119                                                                   ECS035
02120  0610-ZERO-STATE.                                                 ECS035
02121      MOVE +0                     TO X1.                           ECS035
02122      PERFORM 0160-ZERO-ACCUM-ST THRU 0160-EXIT 12 TIMES.          ECS035
02123  EJECT                                                            ECS035
02124  0630-GROUPING-BREAK.                                             ECS035
02125      MOVE 0                     TO A1                             ECS035
02126      PERFORM 0400-BUILD-EXTRACTS THRU 0470-EXIT                   ECS035
02127       VARYING X1 FROM 2 BY 1 UNTIL                                ECS035
02128        X1 IS GREATER THAN DATE-RANGE-MAX.                         ECS035
02129      MOVE SPACES                 TO  B-E-AGT-TABLE.               ECS035
02130      MOVE +0                     TO  X1.                          ECS035
02131      PERFORM 0155-ZERO-DATE-RANGE-ACC THRU 0155-EXIT 13 TIMES.    ECS035
02132      PERFORM 0380-ACCOUNT-BREAK THRU 0540-ZERO-ACCOUNT.           ECS035
02133      PERFORM 0570-STATE-BREAK-1 THRU 0610-ZERO-STATE.             ECS035
02134                                                                   ECS035
02135  0640-GROUPING-BREAK-1.                                           ECS035
02136      IF P-GRP-SW = '1'                                            ECS035
02137          MOVE ' '                TO P-GRP-SW                      ECS035
02138      ELSE                                                         ECS035
02139          GO TO 0680-ZERO-GROUPING.                                ECS035
02140                                                                   ECS035
02141      PERFORM 0860-GP-HD          THRU 0860-EXIT.                  ECS035
02142      MOVE +1                     TO X1.                           ECS035
02143                                                                   ECS035
02144  0650-GROUPING-BREAK-2.                                           ECS035
02145      IF X1 GREATER THAN DATE-RANGE-MAX-1                          ECS035
02146          GO TO 0660-PRINT-LAST-GP-12.                             ECS035
02147                                                                   ECS035
02148      MOVE GP-CERT (X1)           TO DET-CERTS.                    ECS035
02149      MOVE GP-LBEN (X1)           TO DET-LBEN.                     ECS035
02150      MOVE GP-LPRM (X1)           TO DET-LPRM.                     ECS035
02151      MOVE GP-LCLM (X1)           TO DET-LCLM.                     ECS035
02152      MOVE GP-ABEN (X1)           TO DET-ABEN.                     ECS035
02153      MOVE GP-APRM (X1)           TO DET-APRM.                     ECS035
02154      MOVE GP-ACLM (X1)           TO DET-ACLM.                     ECS035
02155      MOVE GP-TPRM (X1)           TO DET-TPRM.                     ECS035
02156      MOVE GP-TCOM (X1)           TO DET-TCOM.                     ECS035
02157      ADD +1                      TO X1.                           ECS035
02158      MOVE COMP-YR (X1)           TO DET-YR.                       ECS035
02159      MOVE COMP-MO (X1)           TO DET-MO.                       ECS035
02160      PERFORM 0940-LOAD-ALPHA-MONTH THRU 0950-EXIT.                ECS035
02161      MOVE DETAIL-LINE            TO P-DATA.                       ECS035
02162                                                                   ECS035
02163      IF X1 = 2                                                    ECS035
02164          MOVE '0'                TO X                             ECS035
02165      ELSE                                                         ECS035
02166          MOVE ' '                TO X.                            ECS035
02167                                                                   ECS035
02168      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02169      GO TO 0650-GROUPING-BREAK-2.                                 ECS035
02170                                                                   ECS035
02171  0660-PRINT-LAST-GP-12.                                           ECS035
02172      MOVE L12GP-LBEN             TO DET-LBEN.                     ECS035
02173      MOVE L12GP-LPRM             TO DET-LPRM.                     ECS035
02174      MOVE L12GP-LCLM             TO DET-LCLM.                     ECS035
02175      MOVE L12GP-ABEN             TO DET-ABEN.                     ECS035
02176      MOVE L12GP-APRM             TO DET-APRM.                     ECS035
02177      MOVE L12GP-ACLM             TO DET-ACLM.                     ECS035
02178      MOVE L12GP-TPRM             TO DET-TPRM.                     ECS035
02179      MOVE L12GP-TCOM             TO DET-TCOM.                     ECS035
02180      MOVE 'LAST 12 MONTHS'       TO DET-TITLE.                    ECS035
02181                                                                   ECS035
02182 ****** AFL CODING ***************                                 ECS035
02183                                                                   ECS035
02184      IF DTE-CLIENT = 'AFL'                                        ECS035
02185          MOVE 'FISCAL TO DATE'   TO DET-TITLE.                    ECS035
02186                                                                   ECS035
02187 ****** END OF AFL CODING *********                                ECS035
02188                                                                   ECS035
02189      MOVE '0'                    TO X.                            ECS035
02190      MOVE DETAIL-LINE            TO P-DATA.                       ECS035
02191      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02192      MOVE ZERO-ACCUM             TO LAST-12-GROUPING-ACCUM.       ECS035
02193                                                                   ECS035
02194  0670-PRINT-GP-YTD.                                               ECS035
02195      MOVE YTDGP-LBEN             TO DET-LBEN.                     ECS035
02196      MOVE YTDGP-LPRM             TO DET-LPRM.                     ECS035
02197      MOVE YTDGP-LCLM             TO DET-LCLM.                     ECS035
02198      MOVE YTDGP-ABEN             TO DET-ABEN.                     ECS035
02199      MOVE YTDGP-APRM             TO DET-APRM.                     ECS035
02200      MOVE YTDGP-ACLM             TO DET-ACLM.                     ECS035
02201      MOVE YTDGP-TPRM             TO DET-TPRM.                     ECS035
02202      MOVE YTDGP-TCOM             TO DET-TCOM.                     ECS035
02203      MOVE 'YEAR TO DATE'         TO DET-TITLE.                    ECS035
02204      MOVE ' '                    TO X.                            ECS035
02205      MOVE DETAIL-LINE            TO P-DATA.                       ECS035
02206      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02207      MOVE SPACES                 TO DET-TITLE.                    ECS035
02208      MOVE ZERO-ACCUM             TO YTD-GROUPING-ACCUM.           ECS035
02209                                                                   ECS035
02210  0680-ZERO-GROUPING.                                              ECS035
02211      MOVE +0                     TO X1.                           ECS035
02212      PERFORM 0170-ZERO-ACCUM-GRP THRU 0170-EXIT 12 TIMES.         ECS035
02213  EJECT                                                            ECS035
02214  0700-CARRIER-BREAK.                                              ECS035
02215      MOVE 0                     TO A1                             ECS035
02216      PERFORM 0400-BUILD-EXTRACTS THRU 0470-EXIT                   ECS035
02217       VARYING X1 FROM 2 BY 1 UNTIL                                ECS035
02218        X1 IS GREATER THAN DATE-RANGE-MAX.                         ECS035
02219      MOVE SPACES                 TO  B-E-AGT-TABLE.               ECS035
02220      MOVE +0                     TO  X1.                          ECS035
02221      PERFORM 0155-ZERO-DATE-RANGE-ACC THRU 0155-EXIT 13 TIMES.    ECS035
02222      PERFORM 0380-ACCOUNT-BREAK    THRU 0540-ZERO-ACCOUNT.        ECS035
02223      PERFORM 0570-STATE-BREAK-1    THRU 0610-ZERO-STATE.          ECS035
02224      PERFORM 0640-GROUPING-BREAK-1 THRU 0680-ZERO-GROUPING.       ECS035
02225                                                                   ECS035
02226  0710-CARRIER-BREAK-1.                                            ECS035
02227      IF P-CA-SW = '1'                                             ECS035
02228          MOVE ' '                TO P-CA-SW                       ECS035
02229      ELSE                                                         ECS035
02230          GO TO 0750-ZERO-CARRIER.                                 ECS035
02231                                                                   ECS035
02232      PERFORM 0870-CA-HD          THRU 0870-EXIT.                  ECS035
02233      MOVE +1                     TO X1.                           ECS035
02234                                                                   ECS035
02235  0720-CARRIER-BREAK-2.                                            ECS035
02236      IF X1 GREATER THAN DATE-RANGE-MAX-1                          ECS035
02237          GO TO 0730-PRINT-LAST-CA-12.                             ECS035
02238                                                                   ECS035
02239      MOVE CA-CERT (X1)           TO DET-CERTS.                    ECS035
02240      MOVE CA-LBEN (X1)           TO DET-LBEN.                     ECS035
02241      MOVE CA-LPRM (X1)           TO DET-LPRM.                     ECS035
02242      MOVE CA-LCLM (X1)           TO DET-LCLM.                     ECS035
02243      MOVE CA-ABEN (X1)           TO DET-ABEN.                     ECS035
02244      MOVE CA-APRM (X1)           TO DET-APRM.                     ECS035
02245      MOVE CA-ACLM (X1)           TO DET-ACLM.                     ECS035
02246      MOVE CA-TPRM (X1)           TO DET-TPRM.                     ECS035
02247      MOVE CA-TCOM (X1)           TO DET-TCOM.                     ECS035
02248      ADD +1                      TO X1.                           ECS035
02249      MOVE COMP-YR (X1)           TO DET-YR.                       ECS035
02250      MOVE COMP-MO (X1)           TO DET-MO.                       ECS035
02251      PERFORM 0940-LOAD-ALPHA-MONTH THRU 0950-EXIT.                ECS035
02252      MOVE DETAIL-LINE            TO P-DATA.                       ECS035
02253                                                                   ECS035
02254      IF X1 = 2                                                    ECS035
02255          MOVE '0'                TO X                             ECS035
02256      ELSE                                                         ECS035
02257          MOVE ' '                TO X.                            ECS035
02258                                                                   ECS035
02259      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02260      GO TO 0720-CARRIER-BREAK-2.                                  ECS035
02261                                                                   ECS035
02262  0730-PRINT-LAST-CA-12.                                           ECS035
02263      MOVE L12CA-LBEN             TO DET-LBEN.                     ECS035
02264      MOVE L12CA-LPRM             TO DET-LPRM.                     ECS035
02265      MOVE L12CA-LCLM             TO DET-LCLM.                     ECS035
02266      MOVE L12CA-ABEN             TO DET-ABEN.                     ECS035
02267      MOVE L12CA-APRM             TO DET-APRM.                     ECS035
02268      MOVE L12CA-ACLM             TO DET-ACLM.                     ECS035
02269      MOVE L12CA-TPRM             TO DET-TPRM.                     ECS035
02270      MOVE L12CA-TCOM             TO DET-TCOM.                     ECS035
02271      MOVE 'LAST 12 MONTHS'       TO DET-TITLE.                    ECS035
02272                                                                   ECS035
02273 ****** AFL CODING ***************                                 ECS035
02274                                                                   ECS035
02275      IF DTE-CLIENT = 'AFL'                                        ECS035
02276          MOVE 'FISCAL TO DATE'   TO DET-TITLE.                    ECS035
02277                                                                   ECS035
02278 ****** END OF AFL CODING ********                                 ECS035
02279                                                                   ECS035
02280      MOVE '0'                    TO X.                            ECS035
02281      MOVE DETAIL-LINE            TO P-DATA.                       ECS035
02282      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02283      MOVE ZERO-ACCUM             TO LAST-12-CARRIER-ACCUM.        ECS035
02284                                                                   ECS035
02285  0740-PRINT-CA-YTD.                                               ECS035
02286      MOVE YTDCA-LBEN             TO DET-LBEN.                     ECS035
02287      MOVE YTDCA-LPRM             TO DET-LPRM.                     ECS035
02288      MOVE YTDCA-LCLM             TO DET-LCLM.                     ECS035
02289      MOVE YTDCA-ABEN             TO DET-ABEN.                     ECS035
02290      MOVE YTDCA-APRM             TO DET-APRM.                     ECS035
02291      MOVE YTDCA-ACLM             TO DET-ACLM.                     ECS035
02292      MOVE YTDCA-TPRM             TO DET-TPRM.                     ECS035
02293      MOVE YTDCA-TCOM             TO DET-TCOM.                     ECS035
02294      MOVE 'YEAR TO DATE'         TO DET-TITLE.                    ECS035
02295      MOVE ' '                    TO X.                            ECS035
02296      MOVE DETAIL-LINE            TO P-DATA.                       ECS035
02297      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02298      MOVE SPACES                 TO DET-TITLE.                    ECS035
02299      MOVE ZERO-ACCUM             TO YTD-CARRIER-ACCUM.            ECS035
02300                                                                   ECS035
02301  0750-ZERO-CARRIER.                                               ECS035
02302      MOVE +0                     TO X1.                           ECS035
02303      PERFORM 0180-ZERO-ACCUM-CARR THRU 0180-EXIT 12 TIMES.        ECS035
02304  EJECT                                                            ECS035
02305  0770-PRINT-DECISION.                                             ECS035
02306      MOVE +1                     TO X1.                           ECS035
02307      MOVE +0                     TO Y1.                           ECS035
02308                                                                   ECS035
02309  0780-PRINT-DECISION-1.                                           ECS035
02310      ADD 1 TO X1 Y1.                                              ECS035
02311                                                                   ECS035
02312      IF X1 GREATER THAN DATE-RANGE-MAX                            ECS035
02313          MOVE +0                 TO X1                            ECS035
02314          GO TO 0790-PRINT-DECISION-2.                             ECS035
02315                                                                   ECS035
02316                                                                   ECS035
02317      COMPUTE LBEN = AC-LBEN (X1) - AC-LBEN (Y1).                  ECS035
02318      COMPUTE LPRM = AC-LPRM (X1) - AC-LPRM (Y1).                  ECS035
02319      COMPUTE LCLM = AC-LCLM (X1) - AC-LCLM (Y1).                  ECS035
02320      COMPUTE ABEN = AC-ABEN (X1) - AC-ABEN (Y1).                  ECS035
02321      COMPUTE APRM = AC-APRM (X1) - AC-APRM (Y1).                  ECS035
02322      COMPUTE ACLM = AC-ACLM (X1) - AC-ACLM (Y1).                  ECS035
02323      COMPUTE TCOM = AC-TCOM (X1) - AC-TCOM (Y1).                  ECS035
02324                                                                   ECS035
02325      IF COMP-CCYY (X1) = CONV-CCYY AND                               CL*31
02326         COMP-MO (X1) = CONV-MO                                    ECS035
02327           MOVE ZEROS TO LBEN LPRM LCLM ABEN APRM ACLM TCOM.       ECS035
02328                                                                   ECS035
02329      MOVE LBEN                   TO PLBEN (Y1).                   ECS035
02330      MOVE LPRM                   TO PLPRM (Y1).                   ECS035
02331      MOVE LCLM                   TO PLCLM (Y1).                   ECS035
02332      MOVE ABEN                   TO PABEN (Y1).                   ECS035
02333      MOVE APRM                   TO PAPRM (Y1).                   ECS035
02334      MOVE ACLM                   TO PACLM (Y1).                   ECS035
02335      MOVE TCOM                   TO PTCOM (Y1).                   ECS035
02336      GO TO 0780-PRINT-DECISION-1.                                 ECS035
02337                                                                   ECS035
02338  0790-PRINT-DECISION-2.                                           ECS035
02339      ADD +1 TO X1.                                                ECS035
02340                                                                   ECS035
02341      IF X1 GREATER THAN DATE-RANGE-MAX-1                          ECS035
02342          GO TO 0800-EXIT.                                         ECS035
02343                                                                   ECS035
02344      IF PRINT-ZERO-TABLE NOT = PRINT-TABLE (X1)                   ECS035
02345          MOVE '1' TO P-ACC-SW P-ST-SW P-GRP-SW P-CA-SW            ECS035
02346          GO TO 0800-EXIT.                                         ECS035
02347                                                                   ECS035
02348      GO TO 0790-PRINT-DECISION-2.                                 ECS035
02349                                                                   ECS035
02350  0800-EXIT.                                                       ECS035
02351       EXIT.                                                       ECS035
02352                                                                   ECS035
02353  EJECT                                                            ECS035
02354  0810-ACC-HD.                                                     ECS035
02355      IF DTE-PGM-OPT NOT = 2                                       ECS035
02356          GO TO 0820-ACC-HD-A.                                     ECS035
02357                                                                   ECS035
02358      IF (DTE-CLIENT EQUAL 'TMS'                                   ECS035
02359         OR DTE-FMT-OPT EQUAL '2')                                 ECS035
02360          GO TO 0820-ACC-HD-A.                                     ECS035
02361                                                                   ECS035
02362      IF SET-CTR = +1                                              ECS035
02363          MOVE SPACES             TO P-DATA                        ECS035
02364          MOVE '-'                TO X                             ECS035
02365          PERFORM 0920-WRITE-PRINT THRU 0930-EXIT                  ECS035
02366          MOVE SPACES             TO P-DATA                        ECS035
02367          MOVE '-'                TO X                             ECS035
02368          PERFORM 0920-WRITE-PRINT THRU 0930-EXIT                  ECS035
02369          MOVE ZERO               TO SET-CTR                       ECS035
02370          MOVE ' '                TO X                             ECS035
02371          GO TO 0830-ACC-HD-B.                                     ECS035
02372                                                                   ECS035
02373      MOVE +1                     TO SET-CTR.                      ECS035
02374                                                                   ECS035
02375  0810-EXIT.                                                       ECS035
02376  EJECT                                                            ECS035
02377  0820-ACC-HD-A.                                                   ECS035
02378      ADD +1 TO PAGE-CNT.                                          ECS035
02379      MOVE PAGE-CNT               TO HD-PAGE.                      ECS035
02380      MOVE '1'                    TO X.                            ECS035
02381      MOVE HD-1-ACCOUNT           TO P-DATA.                       ECS035
02382      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02383      MOVE ' '                    TO X.                            ECS035
02384      MOVE HD-2                   TO P-DATA.                       ECS035
02385      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02386      MOVE ' '                    TO X.                            ECS035
02387      MOVE HD-3                   TO P-DATA.                       ECS035
02388      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02389      MOVE '-'                    TO X.                            ECS035
02390                                                                   ECS035
02391  0820-EXIT.                                                       ECS035
02392  EJECT                                                            ECS035
02393  0830-ACC-HD-B.                                                   ECS035
02394                                                                   ECS035
02395      MOVE S-EPX-ACCT             TO HD-ACCOUNT.                   ECS035
02396      MOVE HD-5                   TO W-WORK-LINE                   ECS035
02397      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT                    ECS035
02398      MOVE W-WORK-LINE            TO P-DATA.                       ECS035
02399      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02400                                                                   ECS035
02401      MOVE ' '                    TO X.                            ECS035
02402      MOVE S-EPX-STATE            TO STATE-L.                      ECS035
02403      PERFORM 1000-STATE-PRT THRU 1010-EXIT.                       ECS035
02404      MOVE STATE-ABBR (CLAS-INDEXS)                                ECS035
02405                                  TO HD-STATE-ABBR.                ECS035
02406      MOVE STATE-PIC (CLAS-INDEXS)                                 ECS035
02407                                  TO HD-STATE.                     ECS035
02408      MOVE '       IN '           TO HD6-TOTAL-FOR.                ECS035
02409      MOVE HD-6                   TO W-WORK-LINE.                  ECS035
02410      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT.                   ECS035
02411      MOVE W-WORK-LINE            TO P-DATA.                       ECS035
02412      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02413                                                                   ECS035
02414      MOVE ' '                    TO X.                            ECS035
02415      MOVE S-EPX-GRP              TO HD-GROUPING.                  ECS035
02416      MOVE '       IN '           TO HD4-TOTAL-FOR.                ECS035
02417      MOVE HD-4                   TO P-DATA.                       ECS035
02418      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02419                                                                   ECS035
02420      MOVE ' '                    TO X.                            ECS035
02421      MOVE S-EPX-CARR             TO HD-CARRIER, CARRIER-L.        ECS035
02422      PERFORM 1100-CARRIER-PRT THRU 1190-EXIT.                     ECS035
02423      MOVE CARRIER-PIC (CLAS-INDEXCN) TO HD-CARR-NAME.             ECS035
02424      MOVE '       IN '           TO HD7-TOTAL-FOR.                ECS035
02425      MOVE HD-7                   TO W-WORK-LINE                   ECS035
02426      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT                    ECS035
02427      MOVE W-WORK-LINE            TO P-DATA.                       ECS035
02428      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02429                                                                   ECS035
02430      MOVE '0'                    TO X.                            ECS035
02431      IF (DTE-CLIENT EQUAL 'TMS'                                   ECS035
02432         OR DTE-FMT-OPT EQUAL '2')                                 ECS035
02433          MOVE SPACES             TO P-DATA                        ECS035
02434          PERFORM 0920-WRITE-PRINT THRU 0930-EXIT                  ECS035
02435          MOVE HD-TMS-9           TO P-DATA                        ECS035
02436      ELSE                                                         ECS035
02437          MOVE HD-9               TO P-DATA.                       ECS035
02438      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02439                                                                   ECS035
02440      MOVE ' '                    TO X.                            ECS035
02441      IF (DTE-CLIENT EQUAL 'TMS'                                   ECS035
02442         OR DTE-FMT-OPT EQUAL '2')                                 ECS035
02443          MOVE HD-TMS-10          TO P-DATA                        ECS035
02444      ELSE                                                         ECS035
02445          MOVE HD-10              TO P-DATA.                       ECS035
02446      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02447                                                                   ECS035
02448  0840-EXIT.                                                       ECS035
02449      EXIT.                                                        ECS035
02450  EJECT                                                            ECS035
02451  0850-ST-HD.                                                      ECS035
02452                                                                   ECS035
02453      MOVE ZERO                   TO SET-CTR.                      ECS035
02454      ADD +1 TO PAGE-CNT.                                          ECS035
02455      MOVE PAGE-CNT               TO HD-PAGE.                      ECS035
02456      MOVE '1'                    TO X.                            ECS035
02457      MOVE HD-1-STATE             TO P-DATA.                       ECS035
02458      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02459                                                                   ECS035
02460      MOVE ' '                    TO X.                            ECS035
02461      MOVE HD-2                   TO P-DATA.                       ECS035
02462      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02463      MOVE ' '                    TO X.                            ECS035
02464      MOVE HD-3                   TO P-DATA.                       ECS035
02465      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02466                                                                   ECS035
02467      MOVE '-'                    TO X.                            ECS035
02468      MOVE S-EPX-STATE            TO STATE-L.                      ECS035
02469      PERFORM 1000-STATE-PRT THRU 1010-EXIT.                       ECS035
02470      MOVE STATE-ABBR (CLAS-INDEXS)                                ECS035
02471                                  TO HD-STATE-ABBR.                ECS035
02472      MOVE STATE-PIC (CLAS-INDEXS)                                 ECS035
02473                                  TO HD-STATE.                     ECS035
02474      MOVE 'TOTAL FOR '           TO HD6-TOTAL-FOR.                ECS035
02475      MOVE HD-6                   TO W-WORK-LINE                   ECS035
02476      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT                    ECS035
02477      MOVE W-WORK-LINE            TO P-DATA.                       ECS035
02478      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02479                                                                   ECS035
02480      MOVE ' '                    TO X.                            ECS035
02481      MOVE S-EPX-GRP              TO HD-GROUPING.                  ECS035
02482      MOVE '       IN '           TO HD4-TOTAL-FOR.                ECS035
02483      MOVE HD-4                   TO P-DATA.                       ECS035
02484      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02485                                                                   ECS035
02486      MOVE ' '                    TO X.                            ECS035
02487      MOVE S-EPX-CARR             TO HD-CARRIER, CARRIER-L.        ECS035
02488      PERFORM 1100-CARRIER-PRT THRU 1190-EXIT.                     ECS035
02489      MOVE CARRIER-PIC (CLAS-INDEXCN) TO HD-CARR-NAME.             ECS035
02490      MOVE '       IN '           TO HD7-TOTAL-FOR.                ECS035
02491      MOVE HD-7                   TO W-WORK-LINE                   ECS035
02492      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT                    ECS035
02493      MOVE W-WORK-LINE            TO P-DATA.                       ECS035
02494      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02495                                                                   ECS035
02496      MOVE '0'                    TO X.                            ECS035
02497      IF (DTE-CLIENT EQUAL 'TMS'                                   ECS035
02498         OR DTE-FMT-OPT EQUAL '2')                                 ECS035
02499          MOVE SPACES             TO P-DATA                        ECS035
02500          PERFORM 0920-WRITE-PRINT THRU 0930-EXIT                  ECS035
02501          MOVE HD-TMS-9           TO P-DATA                        ECS035
02502      ELSE                                                         ECS035
02503          MOVE HD-9               TO P-DATA.                       ECS035
02504      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02505      MOVE ' '                    TO X.                            ECS035
02506      IF (DTE-CLIENT EQUAL 'TMS'                                   ECS035
02507         OR DTE-FMT-OPT EQUAL '2')                                 ECS035
02508          MOVE HD-TMS-10          TO P-DATA                        ECS035
02509      ELSE                                                         ECS035
02510          MOVE HD-10              TO P-DATA.                       ECS035
02511      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02512  0850-EXIT.                                                       ECS035
02513  EJECT                                                            ECS035
02514  0860-GP-HD.                                                      ECS035
02515                                                                   ECS035
02516      ADD +1 TO PAGE-CNT.                                          ECS035
02517      MOVE PAGE-CNT               TO HD-PAGE.                      ECS035
02518      MOVE '1'                    TO X.                            ECS035
02519      MOVE HD-1-GROUPING          TO P-DATA.                       ECS035
02520                                                                   ECS035
02521      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02522      MOVE ' '                    TO X.                            ECS035
02523      MOVE HD-2                   TO P-DATA.                       ECS035
02524                                                                   ECS035
02525      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02526      MOVE ' '                    TO X.                            ECS035
02527      MOVE HD-3                   TO P-DATA.                       ECS035
02528      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02529                                                                   ECS035
02530      MOVE '-'                    TO X.                            ECS035
02531      MOVE S-EPX-GRP              TO HD-GROUPING.                  ECS035
02532      MOVE 'TOTAL FOR '           TO HD4-TOTAL-FOR.                ECS035
02533      MOVE HD-4                   TO W-WORK-LINE                   ECS035
02534      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT                    ECS035
02535      MOVE W-WORK-LINE            TO P-DATA.                       ECS035
02536      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02537                                                                   ECS035
02538      MOVE ' '                    TO X.                            ECS035
02539      MOVE S-EPX-CARR             TO HD-CARRIER, CARRIER-L.        ECS035
02540      PERFORM 1100-CARRIER-PRT THRU 1190-EXIT.                     ECS035
02541      MOVE CARRIER-PIC (CLAS-INDEXCN) TO HD-CARR-NAME.             ECS035
02542      MOVE '       IN '           TO HD7-TOTAL-FOR.                ECS035
02543      MOVE HD-7                   TO W-WORK-LINE                   ECS035
02544      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT                    ECS035
02545      MOVE W-WORK-LINE            TO P-DATA.                       ECS035
02546      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02547                                                                   ECS035
02548      MOVE '0'                    TO X.                            ECS035
02549      IF (DTE-CLIENT EQUAL 'TMS'                                   ECS035
02550         OR DTE-FMT-OPT EQUAL '2')                                 ECS035
02551          MOVE SPACES             TO P-DATA                        ECS035
02552          PERFORM 0920-WRITE-PRINT THRU 0930-EXIT                  ECS035
02553          MOVE HD-TMS-9           TO P-DATA                        ECS035
02554      ELSE                                                         ECS035
02555          MOVE HD-9               TO P-DATA.                       ECS035
02556      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02557                                                                   ECS035
02558      MOVE ' '                    TO X.                            ECS035
02559      IF (DTE-CLIENT EQUAL 'TMS'                                   ECS035
02560         OR DTE-FMT-OPT EQUAL '2')                                 ECS035
02561          MOVE HD-TMS-10          TO P-DATA                        ECS035
02562      ELSE                                                         ECS035
02563          MOVE HD-10              TO P-DATA.                       ECS035
02564      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02565  0860-EXIT.                                                       ECS035
02566  EJECT                                                            ECS035
02567                                                                   ECS035
02568  0870-CA-HD.                                                      ECS035
02569                                                                   ECS035
02570      ADD +1 TO PAGE-CNT.                                          ECS035
02571      MOVE PAGE-CNT               TO HD-PAGE.                      ECS035
02572      MOVE '1'                    TO X.                            ECS035
02573      MOVE HD-1-CARRIER           TO P-DATA.                       ECS035
02574      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02575                                                                   ECS035
02576      MOVE ' '                    TO X.                            ECS035
02577      MOVE HD-2                   TO P-DATA.                       ECS035
02578      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02579                                                                   ECS035
02580      MOVE ' '                    TO X.                            ECS035
02581      MOVE HD-3                   TO P-DATA.                       ECS035
02582      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02583                                                                   ECS035
02584      MOVE '-'                    TO X.                            ECS035
02585      MOVE S-EPX-CARR             TO HD-CARRIER, CARRIER-L.        ECS035
02586      PERFORM 1100-CARRIER-PRT THRU 1190-EXIT.                     ECS035
02587      MOVE CARRIER-PIC (CLAS-INDEXCN) TO HD-CARR-NAME.             ECS035
02588      MOVE 'TOTAL FOR '           TO HD7-TOTAL-FOR.                ECS035
02589      MOVE HD-7                   TO W-WORK-LINE                   ECS035
02590      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT                    ECS035
02591      MOVE W-WORK-LINE            TO P-DATA.                       ECS035
02592      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02593                                                                   ECS035
02594      MOVE '0'                    TO X.                            ECS035
02595      IF (DTE-CLIENT EQUAL 'TMS'                                   ECS035
02596         OR DTE-FMT-OPT EQUAL '2')                                 ECS035
02597          MOVE SPACES             TO P-DATA                        ECS035
02598          PERFORM 0920-WRITE-PRINT THRU 0930-EXIT                  ECS035
02599          MOVE HD-TMS-9           TO P-DATA                        ECS035
02600      ELSE                                                         ECS035
02601          MOVE HD-9               TO P-DATA.                       ECS035
02602      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02603                                                                   ECS035
02604      MOVE ' '                    TO X.                            ECS035
02605      IF (DTE-CLIENT EQUAL 'TMS'                                   ECS035
02606         OR DTE-FMT-OPT EQUAL '2')                                 ECS035
02607          MOVE HD-TMS-10          TO P-DATA                        ECS035
02608      ELSE                                                         ECS035
02609          MOVE HD-10              TO P-DATA.                       ECS035
02610      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02611  0870-EXIT.                                                       ECS035
02612  EJECT                                                            ECS035
02613                                                                   ECS035
02614  0880-GR-HD.                                                      ECS035
02615      ADD +1 TO PAGE-CNT.                                          ECS035
02616      MOVE PAGE-CNT               TO HD-PAGE.                      ECS035
02617      MOVE '1'                    TO X.                            ECS035
02618      MOVE HD-1-GRAND-TOTALS      TO P-DATA.                       ECS035
02619      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02620      MOVE ' '                    TO X.                            ECS035
02621      MOVE HD-2                   TO P-DATA.                       ECS035
02622      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02623      MOVE ' '                    TO X.                            ECS035
02624      MOVE HD-3                   TO P-DATA.                       ECS035
02625      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02626      MOVE '-'                    TO X.                            ECS035
02627      MOVE HD-8                   TO P-DATA.                       ECS035
02628      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02629      MOVE '-'                    TO X.                            ECS035
02630      MOVE SPACES                 TO HD-9-1 HD-10-1.               ECS035
02631      IF (DTE-CLIENT EQUAL 'TMS'                                   ECS035
02632         OR DTE-FMT-OPT EQUAL '2')                                 ECS035
02633          MOVE SPACES             TO P-DATA                        ECS035
02634          PERFORM 0920-WRITE-PRINT THRU 0930-EXIT                  ECS035
02635          MOVE HD-TMS-9           TO P-DATA                        ECS035
02636      ELSE                                                         ECS035
02637          MOVE HD-9               TO P-DATA.                       ECS035
02638      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02639      MOVE ' '                    TO X.                            ECS035
02640      IF (DTE-CLIENT EQUAL 'TMS'                                   ECS035
02641         OR DTE-FMT-OPT EQUAL '2')                                 ECS035
02642          MOVE HD-TMS-10          TO P-DATA                        ECS035
02643      ELSE                                                         ECS035
02644          MOVE HD-10              TO P-DATA.                       ECS035
02645      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02646  0880-EXIT.                                                       ECS035
02647                                  EJECT                            ECS035
02648  0890-REMOVE-SPACES.                                              ECS035
02649                                                                   ECS035
02650      MOVE W-WORK-AREA-2          TO W-WORKING-LINE.               ECS035
02651      MOVE SPACES                 TO W-WORK-AREA-2.                ECS035
02652      MOVE ZEROS                  TO W-WA2-NDX.                    ECS035
02653                                                                   ECS035
02654      PERFORM 0892-TRANSFER-DATA THRU 0892-EXIT                    ECS035
02655              VARYING                                              ECS035
02656          W-WL-NDX FROM 1 BY 1                                     ECS035
02657              UNTIL                                                ECS035
02658          W-WL-NDX GREATER THAN +86.                               ECS035
02659                                                                   ECS035
02660  0890-EXIT.                                                       ECS035
02661      EXIT.                                                        ECS035
02662                                                                   ECS035
02663  0892-TRANSFER-DATA.                                              ECS035
02664                                                                   ECS035
02665      IF  W-WL-CHAR (W-WL-NDX) EQUAL SPACES                        ECS035
02666              AND                                                  ECS035
02667          W-WA2-CHAR (W-WA2-NDX) EQUAL SPACES                      ECS035
02668          NEXT SENTENCE                                            ECS035
02669                                                                   ECS035
02670      ELSE                                                         ECS035
02671          IF  W-WL-CHAR (W-WL-NDX) EQUAL ')'                       ECS035
02672                  AND                                              ECS035
02673              W-WA2-CHAR (W-WA2-NDX) EQUAL SPACES                  ECS035
02674              MOVE W-WL-CHAR (W-WL-NDX)                            ECS035
02675                                  TO W-WA2-CHAR (W-WA2-NDX)        ECS035
02676                                                                   ECS035
02677          ELSE                                                     ECS035
02678              ADD +1              TO W-WA2-NDX                     ECS035
02679              MOVE W-WL-CHAR (W-WL-NDX)                            ECS035
02680                                  TO W-WA2-CHAR (W-WA2-NDX).       ECS035
02681                                                                   ECS035
02682  0892-EXIT.                                                       ECS035
02683      EXIT.                                                        ECS035
02684                                  EJECT                            ECS035
02685  0900-MATCH-ACCT-MASTER.                                          ECS035
02686      IF ACCOUNT-MASTER = HIGH-VALUES                              ECS035
02687          GO TO 0909-MATCH-EXIT.                                   ECS035
02688                                                                   ECS035
02689      READ AM-MAST-IN.                                             ECS035
02690                                                                   ECS035
02691      IF ERACCT-FILE-STATUS = '10'                                 ECS035
02692          MOVE HIGH-VALUES        TO ACCOUNT-MASTER                ECS035
02693          GO TO 0909-MATCH-EXIT.                                   ECS035
02694                                                                   ECS035
02695      IF ERACCT-FILE-STATUS NOT = ZERO                             ECS035
02696          MOVE 'ERROR OCCURED READ - ERACCTT'  TO  WS-ABEND-MESSAGEECS035
02697          MOVE ERACCT-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        ECS035
02698          GO TO ABEND-PGM.                                         ECS035
02699                                                                   ECS035
02700      MOVE AM-MSTR-CNTRL          TO ACCT-CONTROL.                 ECS035
02701      MOVE AM-EFFECT-DT           TO ACCT-EFF-DT.                  ECS035
02702                                                                   ECS035
02703      IF ACCOUNT-FULL-CONTROL LESS THAN EP-CONTROL                 ECS035
02704          GO TO 0900-MATCH-ACCT-MASTER.                            ECS035
02705                                                                   ECS035
02706      IF ACCOUNT-FULL-CONTROL GREATER THAN EP-CONTROL              ECS035
02707          DISPLAY 'NO ACCOUNT MASTER FOR THIS EXTRACT ' EP-CONTROL ECS035
02708                  ' EXP DATE ' EP-EXP-DTE ' EFF DATE ' EP-EFF-DTE  ECS035
02709          MOVE '0302'             TO WS-RETURN-CODE                ECS035
02710          GO TO ABEND-PGM.                                         ECS035
02711                                                                   ECS035
02712      MOVE AM-NAME                TO HD-ACCT-NAME                  ECS035
02713                                     SAVE-ACCT-NAME.               ECS035
051810     MOVE SPACES                 TO HD-ACCT-ADDRESS.              ECS035
051810     STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
051810        DELIMITED BY '  ' INTO HD-ACCT-ADDRESS
051810     END-STRING

           .
02716  0909-MATCH-EXIT.                                                 ECS035
02717      EXIT.                                                        ECS035
02718                                                                   ECS035
02719  EJECT                                                            ECS035
02720  0920-WRITE-PRINT.                                                ECS035
CIDMOD*                            COPY ELCPRT2.                        ECS035
CIDMOD*                                                                 ECS035
CIDMOD******************************************************************ECS035
CIDMOD**                                                                ECS035
CIDMOD** PRINT COPY MODULE HARD COPIED TO ALLOW CHGS THAT WILL PRINT    ECS035
CIDMOD**  ONLY FINAL TOTALS ON PAPER, BUT RETAIN THE ENTIRE REPORT OM   ECS035
CIDMOD**  FICH (PER CID REQUEST #913240174).                            ECS035
CIDMOD**                                                                ECS035
CIDMOD*                                                                 ECS035
00001 ******************************************************************04/15/98
00002 *                                                                *ELCPRT2
00002 *                                                                *ELCPRT2
00003 *                            ELCPRT2                             *   LV005
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE               CL**5
00005 *                            VMOD=2.002                          *   CL**5
00006 *                                                                *   CL**4
00007 ******************************************************************   CL**4
00008                                                                      CL**4
00009      IF DTE-FICH NOT = SPACE AND                                     CL**4
00010         FICH-OPEN    = SPACE                                         CL**4
00011          MOVE 'X'                TO  FICH-OPEN                       CL**4
00012          OPEN OUTPUT FICH.                                           CL**4
00013                                                                      CL**4
00014      IF DTE-FICH NOT = SPACE                                         CL**4
00015          MOVE X                  TO  P-CTL                           CL**4
00016          WRITE FICH-REC FROM PRT.                                    CL**4
02733                                                                   ECS035
CIDMOD     IF PRNT-TOT-SW = 'Y'                                         ECS035
CIDMOD         NEXT SENTENCE                                            ECS035
CIDMOD      ELSE                                                        ECS035
CIDMOD         GO  TO  0930-EXIT.                                       ECS035
CIDMOD                                                                  ECS035
00018      IF DTE-FICH = SPACE OR '2'                                      CL**4
00019        MOVE X                    TO  P-CTL                           CL**4
00020        IF P-CTL = ' '                                                CL**4
00021          WRITE PRT AFTER ADVANCING 1 LINE                            CL**4
00022        ELSE                                                          CL**4
00023          IF P-CTL = '0'                                              CL**4
00024            WRITE PRT AFTER ADVANCING 2 LINES                         CL**4
00025          ELSE                                                        CL**4
00026            IF P-CTL = '-'                                            CL**4
00027              WRITE PRT AFTER ADVANCING 3 LINES                       CL**4
00028            ELSE                                                      CL**4
00029              WRITE PRT AFTER ADVANCING PAGE.                         CL**4
00030                                                                      CL**4
00031 ******************************************************************   CL**4
CIDMOD*                                                                 ECS035
02722  0930-EXIT.                                                       ECS035
02723      EXIT.                                                        ECS035
02724                                                                   ECS035
02725  EJECT                                                            ECS035
02726  0940-LOAD-ALPHA-MONTH.                                           ECS035
02727      IF DET-MO = '01 ' MOVE 'JAN' TO DET-MO                       ECS035
02728                                      DET-TMS-MO.                  ECS035
02729      IF DET-MO = '02 ' MOVE 'FEB' TO DET-MO                       ECS035
02730                                      DET-TMS-MO.                  ECS035
02731      IF DET-MO = '03 ' MOVE 'MAR' TO DET-MO                       ECS035
02732                                      DET-TMS-MO.                  ECS035
02733      IF DET-MO = '04 ' MOVE 'APR' TO DET-MO                       ECS035
02734                                      DET-TMS-MO.                  ECS035
02735      IF DET-MO = '05 ' MOVE 'MAY' TO DET-MO                       ECS035
02736                                      DET-TMS-MO.                  ECS035
02737      IF DET-MO = '06 ' MOVE 'JUN' TO DET-MO                       ECS035
02738                                      DET-TMS-MO.                  ECS035
02739      IF DET-MO = '07 ' MOVE 'JUL' TO DET-MO                       ECS035
02740                                      DET-TMS-MO.                  ECS035
02741      IF DET-MO = '08 ' MOVE 'AUG' TO DET-MO                       ECS035
02742                                      DET-TMS-MO.                  ECS035
02743      IF DET-MO = '09 ' MOVE 'SEP' TO DET-MO                       ECS035
02744                                      DET-TMS-MO.                  ECS035
02745      IF DET-MO = '10 ' MOVE 'OCT' TO DET-MO                       ECS035
02746                                      DET-TMS-MO.                  ECS035
02747      IF DET-MO = '11 ' MOVE 'NOV' TO DET-MO                       ECS035
02748                                      DET-TMS-MO.                  ECS035
02749      IF DET-MO = '12 ' MOVE 'DEC' TO DET-MO                       ECS035
02750                                      DET-TMS-MO.                  ECS035
02751                                                                   ECS035
02752  0950-EXIT.                                                       ECS035
02753       EXIT.                                                       ECS035
02754  EJECT                                                            ECS035
02755  0960-PRINT-GRAND-TOTALS.                                         ECS035
CIDMOD
CIDMOD     MOVE  'Y'  TO  PRNT-TOT-SW.
CIDMOD
02756      IF DTE-FICH = '1'                                            ECS035
02757          MOVE '2'                TO DTE-FICH.                     ECS035
02758                                                                   ECS035
02759      PERFORM 0880-GR-HD          THRU 0880-EXIT.                  ECS035
02760      MOVE +1                     TO X1.                           ECS035
02761                                                                   ECS035
02762  0970-GRAND-TOTAL-1.                                              ECS035
02763      IF X1 GREATER THAN DATE-RANGE-MAX-1                          ECS035
02764          GO TO 0980-GRAND-TOTAL-2.                                ECS035
02765                                                                   ECS035
02766      MOVE GT-LBEN (X1)           TO DET-LBEN.                     ECS035
02767      MOVE GT-LPRM (X1)           TO DET-LPRM.                     ECS035
02768                                                                   ECS035
02769      IF X1 = 12                                                   ECS035
02770          MOVE GT-LPRM (12)       TO ME-035-NET-L                  ECS035
02771          MOVE GT-APRM (12)       TO ME-035-NET-AH.                ECS035
02772                                                                   ECS035
02773      MOVE GT-LCLM (X1)           TO DET-LCLM.                     ECS035
02774      MOVE GT-ABEN (X1)           TO DET-ABEN.                     ECS035
02775      MOVE GT-APRM (X1)           TO DET-APRM.                     ECS035
02776      MOVE GT-ACLM (X1)           TO DET-ACLM.                     ECS035
02777      MOVE GT-TPRM (X1)           TO DET-TPRM.                     ECS035
02778      MOVE GT-TCOM (X1)           TO DET-TCOM.                     ECS035
02779      ADD +1 TO X1.                                                ECS035
02780      MOVE COMP-YR (X1)           TO DET-YR.                       ECS035
02781      MOVE COMP-MO (X1)           TO DET-MO.                       ECS035
02782      PERFORM 0940-LOAD-ALPHA-MONTH THRU 0950-EXIT.                ECS035
02783      MOVE DETAIL-LINE            TO P-DATA.                       ECS035
02784                                                                   ECS035
02785      IF X1 = 2                                                    ECS035
02786          MOVE '0'                TO X                             ECS035
02787      ELSE                                                         ECS035
02788          MOVE ' '                TO X.                            ECS035
02789                                                                   ECS035
02790      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02791      GO TO 0970-GRAND-TOTAL-1.                                    ECS035
02792                                                                   ECS035
02793  0980-GRAND-TOTAL-2.                                              ECS035
02794      MOVE 'LAST 12 MONTHS'       TO DET-TITLE.                    ECS035
02795      MOVE '-'                    TO X.                            ECS035
02796      MOVE L12GD-LBEN             TO DET-LBEN.                     ECS035
02797      MOVE L12GD-LPRM             TO DET-LPRM.                     ECS035
02798      MOVE L12GD-LCLM             TO DET-LCLM.                     ECS035
02799      MOVE L12GD-ABEN             TO DET-ABEN.                     ECS035
02800      MOVE L12GD-APRM             TO DET-APRM.                     ECS035
02801      MOVE L12GD-ACLM             TO DET-ACLM.                     ECS035
02802      MOVE L12GD-TPRM             TO DET-TPRM.                     ECS035
02803      MOVE L12GD-TCOM             TO DET-TCOM.                     ECS035
02804      MOVE DETAIL-LINE            TO P-DATA.                       ECS035
02805      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02806      MOVE 'YEAR TO DATE'         TO DET-TITLE.                    ECS035
02807      MOVE ' '                    TO X.                            ECS035
02808      MOVE YTDGD-LBEN             TO DET-LBEN.                     ECS035
02809      MOVE YTDGD-LPRM             TO DET-LPRM.                     ECS035
02810      MOVE YTDGD-LCLM             TO DET-LCLM.                     ECS035
02811      MOVE YTDGD-ABEN             TO DET-ABEN.                     ECS035
02812      MOVE YTDGD-APRM             TO DET-APRM.                     ECS035
02813      MOVE YTDGD-ACLM             TO DET-ACLM.                     ECS035
02814      MOVE YTDGD-TPRM             TO DET-TPRM.                     ECS035
02815      MOVE YTDGD-TCOM             TO DET-TCOM.                     ECS035
02816      MOVE DETAIL-LINE            TO P-DATA.                       ECS035
02817      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02818                                                                   ECS035
02819  0990-EXIT.                                                       ECS035
02820      EXIT.                                                        ECS035
02821  EJECT                                                            ECS035
02822  1000-STATE-PRT.                                                  ECS035
02823                              COPY ECSSTLOK.                       ECS035
02824  1010-EXIT.                                                       ECS035
02825      EXIT.                                                        ECS035
02826  EJECT                                                            ECS035
02827  1100-CARRIER-PRT.                                                ECS035
02828      MOVE ZERO               TO CLAS-INDEXCN.                     ECS035
02829                                                                   ECS035
02830  1110-CARRIER-LOOP.                                               ECS035
02831      ADD 1 TO CLAS-INDEXCN.                                       ECS035
02832                                                                   ECS035
02833      IF CLAS-INDEXCN IS GREATER THAN CLAS-MAXCN                   ECS035
02834          MOVE SPACE          TO CARRIER-L                         ECS035
02835          GO TO 1190-EXIT.                                         ECS035
02836                                                                   ECS035
02837      IF CARRIER-L NOT = CARRIER-SUB (CLAS-INDEXCN)                ECS035
02838          GO TO 1110-CARRIER-LOOP.                                 ECS035
02839                                                                   ECS035
02840  1190-EXIT.                                                       ECS035
02841      EXIT.                                                        ECS035
02842  EJECT                                                            ECS035
02843 **********************************************************        ECS035
02844 *  THIS SECTION CONTAINS SPECIAL CODE FOR TOYATA.        *        ECS035
02845 * A REQUEST WAS MADE IN FEBRUARY 1990 TO REPORT THE      *        ECS035
02846 * FOLLOWING INFORMATION USING GROSS DATA VS NET DATA :   *        ECS035
02847 *  ISSUE COUNTS,                                         *        ECS035
02848 *  CANCEL COUNTS,                                        *        ECS035
02849 *  LIFE AND AH PREMIUM AMTS                              *        ECS035
02850 *  LIFE AND AH CANCEL PREMIUM AMTS                       *        ECS035
02851 *  LIFE ANC AH CLAIM AMTS                                *        ECS035
02852 *                                                        *        ECS035
02853 *  A REQUEST WAS ALSO MADE TO KEEP THE ECS036 REPORT     *        ECS035
02854 *  'AS IS' SO THE EXTRACT CREATED FROM ECS035 NEEDED     *        ECS035
02855 *  TO REMAIN ON THE 'NET' BASIS.                         *        ECS035
02856 *                                                        *        ECS035
02857 **********************************************************        ECS035
02858                                                                   ECS035
02859  2380-ACCOUNT-BREAK.                                              ECS035
02860      MOVE +0                     TO X1.                           ECS035
02861      PERFORM 0140-ZERO-PRINT-TABLE THRU 0140-EXIT 12 TIMES.       ECS035
02862      PERFORM 2770-PRINT-DECISION THRU 2800-EXIT.                  ECS035
02863                                                                   ECS035
02864      IF P-ACC-SW = '1'                                            ECS035
02865          MOVE ' '                TO P-ACC-SW                      ECS035
02866      ELSE                                                         ECS035
02867          GO TO 2540-ZERO-ACCOUNT.                                 ECS035
02868                                                                   ECS035
02869      PERFORM 0810-ACC-HD  THRU  0840-EXIT.                        ECS035
02870      MOVE +1                     TO X1.                           ECS035
02871      MOVE +0                     TO Y1.                           ECS035
02872                                                                   ECS035
02873  2390-ACCOUNT-BREAK-PRINT.                                        ECS035
02874      ADD 1                       TO X1 Y1.                        ECS035
02875                                                                   ECS035
02876      IF X1 GREATER THAN DATE-RANGE-MAX                            ECS035
02877          GO TO 2520-PRINT-LAST-AC-12.                             ECS035
02878                                                                   ECS035
02879      MOVE SPACES                 TO DETAIL-TMS-LINE-1             ECS035
02880                                     DETAIL-TMS-LINE-2.            ECS035
02881                                                                   ECS035
02882      COMPUTE CERT-T   = AC-CERT-T (X1)   - AC-CERT-T (Y1).        ECS035
02883      COMPUTE CANCEL-T = AC-CANCEL-T (X1) - AC-CANCEL-T (Y1).      ECS035
02884      COMPUTE L-COVERAGE-T = AC-L-COVERAGE-T (X1)  -               ECS035
02885                              AC-L-COVERAGE-T (Y1).                ECS035
02886      COMPUTE A-COVERAGE-T = AC-A-COVERAGE-T (X1)  -               ECS035
02887                              AC-A-COVERAGE-T (Y1).                ECS035
02888      COMPUTE LPRM-T   = AC-LPRM-T (X1)   - AC-LPRM-T (Y1).        ECS035
02889      COMPUTE LCAN-T   = AC-LCAN-T (X1)   - AC-LCAN-T (Y1).        ECS035
02890      COMPUTE LCLM-T   = AC-LCLM-T (X1)   - AC-LCLM-T (Y1).        ECS035
02891      COMPUTE APRM-T   = AC-APRM-T (X1)   - AC-APRM-T (Y1).        ECS035
02892      COMPUTE ACAN-T   = AC-ACAN-T (X1)   - AC-ACAN-T (Y1).        ECS035
02893      COMPUTE ACLM-T   = AC-ACLM-T (X1)   - AC-ACLM-T (Y1).        ECS035
02894      COMPUTE TCOM-T   = AC-TCOM-T (X1)   - AC-TCOM-T (Y1).        ECS035
02895      COMPUTE TPRM-T   = (LPRM-T + APRM-T) -                       ECS035
02896             (LCAN-T + ACAN-T).                                    ECS035
02897      MOVE COMP-YR (X1)           TO DET-TMS-YR.                   ECS035
02898      MOVE COMP-MO (X1)           TO DET-MO                        ECS035
02899                                     DET-TMS-MO.                   ECS035
02900      PERFORM 0940-LOAD-ALPHA-MONTH THRU 0950-EXIT.                ECS035
02901      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                ECS035
02902      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                ECS035
02903      MOVE CERT-T                 TO DET-TMS-CERTS.                ECS035
02904      MOVE CANCEL-T               TO DET-TMS-CANCELS.              ECS035
02905      MOVE L-COVERAGE-T           TO DET-TMS-LCOVERAG.             ECS035
02906      MOVE LPRM-T                 TO DET-TMS-LPRM.                 ECS035
02907      MOVE LCAN-T                 TO DET-TMS-LCAN.                 ECS035
02908      MOVE LCLM-T                 TO DET-TMS-LCLM.                 ECS035
02909      MOVE APRM-T                 TO DET-TMS-APRM.                 ECS035
02910      MOVE ACAN-T                 TO DET-TMS-ACAN.                 ECS035
02911      MOVE A-COVERAGE-T           TO DET-TMS-ACOVERAG.             ECS035
02912      MOVE ACLM-T                 TO DET-TMS-ACLM.                 ECS035
02913      MOVE TPRM-T                 TO DET-TMS-TPRM.                 ECS035
02914      MOVE TCOM-T                 TO DET-TMS-TCOM.                 ECS035
02915                                                                   ECS035
02916      COMPUTE DET-TMS-NPRM = (TPRM-T - TCOM-T).                    ECS035
02917                                                                   ECS035
02918      IF COMP-CCYY (X1) = CONV-CCYY  AND  COMP-MO (X1) = CONV-MO      CL*31
02919          MOVE ZEROS TO DET-TMS-CERTS                              ECS035
02920                        DET-TMS-LCOVERAG                           ECS035
02921                        DET-TMS-ACOVERAG                           ECS035
02922                        DET-TMS-LPRM                               ECS035
02923                        DET-TMS-LCAN                               ECS035
02924                        DET-TMS-LCLM                               ECS035
02925                        DET-TMS-APRM                               ECS035
02926                        DET-TMS-ACAN                               ECS035
02927                        DET-TMS-ACLM                               ECS035
02928                        DET-TMS-TPRM                               ECS035
02929                        DET-TMS-TCOM                               ECS035
02930      ELSE                                                         ECS035
02931          PERFORM 2500-ADD-TO-OTHERS  THRU 2510-EXIT.              ECS035
02932                                                                   ECS035
02933 *    PERFORM 2400-BUILD-EXTRACTS THRU 2470-EXIT.                  ECS035
02934                                                                   ECS035
02935      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       ECS035
02936                                                                   ECS035
02937      MOVE '0'                    TO X.                            ECS035
02938                                                                   ECS035
02939      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02940                                                                   ECS035
02941      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       ECS035
02942                                                                   ECS035
02943      MOVE ' '                    TO X.                            ECS035
02944                                                                   ECS035
02945      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02946      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       ECS035
02947                                                                   ECS035
02948      MOVE ' '                    TO X.                            ECS035
02949                                                                   ECS035
02950      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
02951                                                                   ECS035
02952      GO TO 2390-ACCOUNT-BREAK-PRINT.                              ECS035
02953  EJECT                                                            ECS035
02954  2400-BUILD-EXTRACTS.                                             ECS035
02955      IF X1 = 2                                                    ECS035
02956          MOVE 1                  TO X1.                           ECS035
02957                                                                   ECS035
02958      ADD 1                       TO A1.                           ECS035
02959  2410-B-E-RESUME.                                                 ECS035
02960                                                                      CL*30
02961      MOVE ZEROS                  TO WS-EE-DTE.                       CL*30
02962      MOVE COMP-CCYY (X1)         TO EE-CCYY.                         CL*31
02963      MOVE COMP-MO (X1)           TO EE-MO.                        ECS035
02964      MOVE WS-EE-DTE              TO EE-DTE.                          CL*15
02965                                                                   ECS035
02966      MOVE SPACE                  TO WS-ISS-FLAG.                  ECS035
02967                                                                   ECS035
02968      IF DTE-CLIENT EQUAL 'HER'  OR 'HSL'                          ECS035
02969          COMPUTE WS-ISS-CNT = DR-ISS (X1) - DR-ISS (A1)           ECS035
02970          IF WS-ISS-CNT GREATER THAN ZEROS                         ECS035
02971              MOVE 'Y'            TO WS-ISS-FLAG.                  ECS035
02972                                                                   ECS035
02973      COMPUTE EE-CERT = DR-CERT (X1).                              ECS035
02974      COMPUTE EE-LBEN = DR-LBEN (X1).                              ECS035
02975      COMPUTE EE-LPRM = DR-LPRM (X1).                              ECS035
02976      COMPUTE EE-LCLM = DR-LCLM (X1).                              ECS035
02977      COMPUTE EE-ABEN = DR-ABEN (X1).                              ECS035
02978      COMPUTE EE-APRM = DR-APRM (X1).                              ECS035
02979      COMPUTE EE-ACLM = DR-ACLM (X1).                              ECS035
02980      COMPUTE EE-TPRM = EE-LPRM + EE-APRM.                         ECS035
02981      COMPUTE EE-TCOM = DR-TCOM (X1).                              ECS035
02982                                                                   ECS035
02983      COMPUTE EE-ISS-CNT = DR-ISS (X1).                            ECS035
02984                                                                   ECS035
02985      MOVE AC-DATE (X1)           TO  EE-MTH-HI-CERT.              ECS035
02986      MOVE ZERO                   TO EE-CNTL.                      ECS035
02987      MOVE SPACES                 TO EE-ACCT-NAME.                 ECS035
CIDMOD     MOVE SPACES                 TO EE-ACCT-CITY.                 ECS035
010716     IF DTE-CLIENT = 'HER' OR 'HSL' OR 'CID' OR 'DCC' or 'VPP'
030612       OR 'AHL'
022808        EVALUATE AM-STATUS
022808           WHEN '1'
022808              MOVE 'I'           TO EE-ACCT-STATUS
022808           WHEN '2'
022808              MOVE 'T'           TO EE-ACCT-STATUS
022808           WHEN '3'
022808              MOVE 'C'           TO EE-ACCT-STATUS
022808           WHEN '4'
022808              MOVE 'I'           TO EE-ACCT-STATUS
031811           WHEN '5'
031811              MOVE 'S'           TO EE-ACCT-STATUS
021916           WHEN '6'
021916              MOVE 'D'           TO EE-ACCT-STATUS
021916           WHEN '7'
021916              MOVE 'L'           TO EE-ACCT-STATUS
021916           WHEN '8'
021916              MOVE 'R'           TO EE-ACCT-STATUS
021916           WHEN '9'
021916              MOVE 'P'           TO EE-ACCT-STATUS
022808           WHEN OTHER
022808              MOVE 'A'           TO EE-ACCT-STATUS
022808        END-EVALUATE
02996      ELSE
02997          MOVE SPACES             TO EE-ACCT-STATUS
           END-IF

03000      IF AM-REPORT-CODE-1 = SPACES OR ZEROS OR LOW-VALUES          ECS035
03001          NEXT SENTENCE                                            ECS035
03002      ELSE                                                         ECS035
03003          MOVE '4'                TO EE-PASS-NO                    ECS035
03004          MOVE AM-REPORT-CODE-1   TO EE-A-RPT-CD-1                 ECS035
03005          MOVE S-EPX-CARR         TO EE-A-CARR                     ECS035
03006          MOVE S-EPX-GRP          TO EE-A-GROUP                    ECS035
CIDMOD*        MOVE AM-REPORT-CODE-2   TO EE-A-RPT-CD-2                 ECS035
CIDMOD         MOVE SPACES             TO EE-A-RPT-CD-2                 ECS035
03008          MOVE S-EPX-STATE        TO EE-A-STATE                    ECS035
03009          MOVE S-EPX-ACCT         TO EE-A-ACCT                     ECS035
03010          MOVE AM-NAME            TO EE-ACCT-NAME                  ECS035
051810         MOVE SPACES             TO EE-ACCT-CITY                  ECS035
051810         STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
051810            DELIMITED BY '  ' INTO EE-ACCT-CITY
051810         END-STRING
03011          PERFORM 2480-WRITE-EXTRACT THRU 2490-EXIT.               ECS035
03012                                                                   ECS035
03013      IF AM-REPORT-CODE-2 = SPACES OR ZEROS OR LOW-VALUES          ECS035
03014          NEXT SENTENCE                                            ECS035
03015      ELSE                                                         ECS035
03016          MOVE '5'                TO EE-PASS-NO                    ECS035
03017          MOVE LOW-VALUES         TO EE-A-RPT-CD-1                 ECS035
03018          MOVE S-EPX-CARR         TO EE-A-CARR                     ECS035
03019          MOVE S-EPX-GRP          TO EE-A-GROUP                    ECS035
03020          MOVE AM-REPORT-CODE-2   TO EE-A-RPT-CD-2                 ECS035
03021          MOVE S-EPX-STATE        TO EE-A-STATE                    ECS035
03022          MOVE S-EPX-ACCT         TO EE-A-ACCT                     ECS035
03023          MOVE AM-NAME            TO EE-ACCT-NAME                  ECS035
051810         MOVE SPACES             TO EE-ACCT-CITY                  ECS035
051810         STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
051810            DELIMITED BY '  ' INTO EE-ACCT-CITY
051810         END-STRING
03024          PERFORM 2480-WRITE-EXTRACT THRU 2490-EXIT.               ECS035
03025                                                                   ECS035
03026      MOVE ZERO                   TO EE-CNTL.                      ECS035
03027                                                                   ECS035
03028      MOVE '1'                    TO EE-PASS-NO.                   ECS035
03029      MOVE S-EPX-STATE            TO EE-CNTL-1.                    ECS035
03030      PERFORM 2480-WRITE-EXTRACT THRU 2490-EXIT.                   ECS035
03031                                                                   ECS035
03032      MOVE '2'                    TO EE-PASS-NO.                   ECS035
03033      MOVE AM-GPCD                TO EE-CNTL-1.                    ECS035
03034      MOVE S-EPX-GRP              TO EE-GROUP.                     ECS035
03035      MOVE S-EPX-CARR             TO EE-CARR.                      ECS035
03036      PERFORM 2480-WRITE-EXTRACT THRU 2490-EXIT.                   ECS035
03037                                                                   ECS035
03038      MOVE '3'                    TO EE-PASS-NO.                   ECS035
03039                                                                   ECS035
051810     MOVE SPACES                 TO EE-ACCT-CITY                  ECS035
051810     STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
051810        DELIMITED BY '  ' INTO EE-ACCT-CITY
051810     END-STRING
03040      MOVE AM-NAME                TO EE-ACCT-NAME.                 ECS035
03041                                                                   ECS035
03042      MOVE SAVE-AM-EXPIRE-DT      TO EE-AM-EXPIRES.                ECS035
03043      MOVE SAVE-AM-HI-CERT-DATE   TO EE-AM-HI-CERT.                ECS035
03044                                                                   ECS035
03045      MOVE 1                      TO Z                             ECS035
03046                                     Z1.                           ECS035
03047                                                                   ECS035
03048  2420-B-E-LOOP-1.                                                 ECS035
03049      ADD 1                       TO Z.                            ECS035
03050                                                                   ECS035
03051      IF Z GREATER 10                                              ECS035
03052          MOVE 0                  TO Z Z1                          ECS035
03053          GO TO 2430-B-E-LOOP-2.                                   ECS035
03054                                                                   ECS035
03055      IF (AM-COM-TYP (Z) = 'O' OR 'P'
052814         OR 'B' OR 'I' OR 'L' OR 'K' OR 'S')
03056          MOVE AM-AGT (Z)         TO B-E-AGT (Z1)                  ECS035
03057          ADD 1                   TO Z1.                           ECS035
03058                                                                   ECS035
03059      GO TO 2420-B-E-LOOP-1.                                       ECS035
03060                                                                   ECS035
03061  2430-B-E-LOOP-2.                                                 ECS035
03062      ADD 1                       TO Z.                            ECS035
03063                                                                   ECS035
03064      IF Z GREATER 9                                               ECS035
03065          MOVE 0                  TO Z Z1                          ECS035
03066          GO TO 2450-B-E-LOOP-4.                                   ECS035
03067                                                                   ECS035
03068  2440-B-E-LOOP-3.                                                 ECS035
03069      ADD 1                       TO Z1.                           ECS035
03070                                                                   ECS035
03071      IF Z1 GREATER 9                                              ECS035
03072          MOVE 0                  TO Z1                            ECS035
03073          GO TO 2430-B-E-LOOP-2.                                   ECS035
03074                                                                   ECS035
03075      IF Z = Z1                                                    ECS035
03076          GO TO 2440-B-E-LOOP-3.                                   ECS035
03077                                                                   ECS035
03078      IF B-E-AGT (Z) = B-E-AGT (Z1)                                ECS035
03079          MOVE SPACE              TO B-E-AGT (Z1).                 ECS035
03080                                                                   ECS035
03081      GO TO 2440-B-E-LOOP-3.                                       ECS035
03082                                                                   ECS035
03083  2450-B-E-LOOP-4.                                                 ECS035
03084      ADD 1                       TO Z.                            ECS035
03085                                                                   ECS035
03086      IF DTE-CLIENT = 'HER'                                        ECS035
03087          IF Z GREATER 1                                           ECS035
03088              GO TO 2460-B-E-CHECK.                                ECS035
03089                                                                   ECS035
03090      IF Z GREATER 9                                               ECS035
03091          GO TO 2460-B-E-CHECK.                                    ECS035
03092                                                                   ECS035
03093      IF (B-E-AGT (Z) = SPACE OR ZERO OR LOW-VALUES)               ECS035
03094          GO TO 2450-B-E-LOOP-4.                                   ECS035
03095                                                                   ECS035
03096      MOVE B-E-AGT (Z)            TO EE-CNTL-GA.                   ECS035
03097      MOVE S-EPX-ACCT             TO EE-CNTL-ACCT.                 ECS035
03098                                                                   ECS035
03099      IF DTE-CLIENT = 'HER'  OR  'VSL'  OR  'MON'  OR 'HSL'        ECS035
03100          PERFORM 2480-WRITE-EXTRACT THRU 2490-EXIT.               ECS035
03101                                                                   ECS035
03102      MOVE HIGH-VALUE             TO EE-CNTL-ACCT.                 ECS035
03103      MOVE ZEROS                  TO EE-AM-EXPIRES                 ECS035
03104                                     EE-AM-HI-CERT.                ECS035
03105                                                                   ECS035
03106      PERFORM 2480-WRITE-EXTRACT THRU 2490-EXIT.                   ECS035
03107                                                                   ECS035
03108      MOVE SAVE-AM-EXPIRE-DT      TO EE-AM-EXPIRES.                ECS035
03109      MOVE SAVE-AM-HI-CERT-DATE   TO EE-AM-HI-CERT.                ECS035
03110                                                                   ECS035
03111      GO TO 2450-B-E-LOOP-4.                                       ECS035
03112                                                                   ECS035
03113  2460-B-E-CHECK.                                                  ECS035
03114      IF X1 = 1                                                    ECS035
03115          MOVE 2                  TO X1                            ECS035
03116          GO TO 2410-B-E-RESUME.                                   ECS035
03117                                                                   ECS035
03118  2470-EXIT.                                                       ECS035
03119       EXIT.                                                       ECS035
03120                                                                   ECS035
03121  EJECT                                                            ECS035
03122  2480-WRITE-EXTRACT.                                              ECS035
03123                                                                   ECS035
03124      MOVE X1                   TO  SAVE-X1.                       ECS035
03125      MOVE +0                   TO  X1.                            ECS035
03126      PERFORM 0140-ZERO-PRINT-TABLE THRU 0140-EXIT 12 TIMES.       ECS035
03127      PERFORM 2770-PRINT-DECISION THRU 2800-EXIT.                  ECS035
03128      IF P-ACC-SW IS EQUAL TO '1'                                  ECS035
03129          MOVE SAVE-X1          TO  X1                             ECS035
03130      ELSE                                                         ECS035
03131          MOVE SAVE-X1          TO  X1                             ECS035
03132          GO TO 2490-EXIT.                                         ECS035
03133                                                                   ECS035
03134      IF DTE-CLIENT EQUAL 'HER' OR 'HSL'                           ECS035
03135         NEXT SENTENCE                                             ECS035
03136      ELSE                                                         ECS035
03137      IF EE-CERT = ZERO AND EE-LBEN = ZERO AND EE-LPRM = ZERO AND  ECS035
03138         EE-LCLM = ZERO AND EE-ABEN = ZERO AND EE-APRM = ZERO AND  ECS035
03139         EE-ACLM = ZERO AND EE-TPRM = ZERO AND EE-TCOM = ZERO      ECS035
03140         GO TO 2490-EXIT.                                          ECS035
03141                                                                   ECS035
03142      IF DTE-CLIENT EQUAL 'HER' OR 'HSL'                           ECS035
03143         IF EE-PASS-NO EQUAL '3'                                   ECS035
03144             MOVE WS-ISS-FLAG         TO EE-ISS-FLAG.              ECS035
03145                                                                   ECS035
03146      WRITE EXTRACT-OT-REC FROM WS-EXTR-REC.                       ECS035
03147                                                                   ECS035
03148      MOVE SPACE                      TO EE-ISS-FLAG.              ECS035
03149                                                                   ECS035
03150  2490-EXIT.                                                       ECS035
03151       EXIT.                                                       ECS035
03152                                                                   ECS035
03153  2500-ADD-TO-OTHERS.                                              ECS035
03154                                                                   ECS035
03155      ADD CERT-T                TO ST-CERT-T (Y1)                  ECS035
03156                                   GP-CERT-T (Y1)                  ECS035
03157                                   CA-CERT-T (Y1)                  ECS035
03158                                   GR-CERT-T                       ECS035
03159                                   GT-CERT-T (Y1).                 ECS035
03160      ADD CANCEL-T              TO ST-CANCEL-T (Y1)                ECS035
03161                                   GP-CANCEL-T (Y1)                ECS035
03162                                   CA-CANCEL-T (Y1)                ECS035
03163                                   GR-CANCEL-T                     ECS035
03164                                   GT-CANCEL-T (Y1).               ECS035
03165      ADD L-COVERAGE-T          TO ST-L-COVERAGE-T (Y1)            ECS035
03166                                   GP-L-COVERAGE-T (Y1)            ECS035
03167                                   CA-L-COVERAGE-T (Y1)            ECS035
03168                                   GR-L-COVERAGE-T                 ECS035
03169                                   GT-L-COVERAGE-T (Y1).           ECS035
03170                                                                   ECS035
03171      ADD A-COVERAGE-T          TO ST-A-COVERAGE-T (Y1)            ECS035
03172                                   GP-A-COVERAGE-T (Y1)            ECS035
03173                                   CA-A-COVERAGE-T (Y1)            ECS035
03174                                   GR-A-COVERAGE-T                 ECS035
03175                                   GT-A-COVERAGE-T (Y1).           ECS035
03176                                                                   ECS035
03177      ADD LPRM-T                TO ST-LPRM-T (Y1)                  ECS035
03178                                   GP-LPRM-T (Y1)                  ECS035
03179                                   CA-LPRM-T (Y1)                  ECS035
03180                                   GR-LPRM-T                       ECS035
03181                                   GT-LPRM-T (Y1).                 ECS035
03182      ADD LCAN-T                TO ST-LCAN-T (Y1)                  ECS035
03183                                   GP-LCAN-T (Y1)                  ECS035
03184                                   CA-LCAN-T (Y1)                  ECS035
03185                                   GR-LCAN-T                       ECS035
03186                                   GT-LCAN-T (Y1).                 ECS035
03187      ADD LCLM-T                TO ST-LCLM-T (Y1)                  ECS035
03188                                   GP-LCLM-T (Y1)                  ECS035
03189                                   CA-LCLM-T (Y1)                  ECS035
03190                                   GR-LCLM-T                       ECS035
03191                                   GT-LCLM-T (Y1).                 ECS035
03192      ADD APRM-T                TO ST-APRM-T (Y1)                  ECS035
03193                                   GP-APRM-T (Y1)                  ECS035
03194                                   CA-APRM-T (Y1)                  ECS035
03195                                   GR-APRM-T                       ECS035
03196                                   GT-APRM-T (Y1).                 ECS035
03197      ADD ACAN-T                TO ST-ACAN-T (Y1)                  ECS035
03198                                   GP-ACAN-T (Y1)                  ECS035
03199                                   CA-ACAN-T (Y1)                  ECS035
03200                                   GR-ACAN-T                       ECS035
03201                                   GT-ACAN-T (Y1).                 ECS035
03202      ADD ACLM-T                TO ST-ACLM-T (Y1)                  ECS035
03203                                   GP-ACLM-T (Y1)                  ECS035
03204                                   CA-ACLM-T (Y1)                  ECS035
03205                                   GR-ACLM-T                       ECS035
03206                                   GT-ACLM-T (Y1).                 ECS035
03207      ADD TPRM-T                TO ST-TPRM-T (Y1)                  ECS035
03208                                   GP-TPRM-T (Y1)                  ECS035
03209                                   CA-TPRM-T (Y1)                  ECS035
03210                                   GR-TPRM-T                       ECS035
03211                                   GT-TPRM-T (Y1).                 ECS035
03212      ADD TCOM-T                TO ST-TCOM-T (Y1)                  ECS035
03213                                   GP-TCOM-T (Y1)                  ECS035
03214                                   CA-TCOM-T (Y1)                  ECS035
03215                                   GR-TCOM-T                       ECS035
03216                                   GT-TCOM-T (Y1).                 ECS035
03217                                                                   ECS035
03218      IF COMPARE9DT (X1) GREATER THAN YEAR-OLD-DATE                ECS035
03219          ADD CERT-T            TO L12AC-CERT-T                    ECS035
03220                                   L12ST-CERT-T                    ECS035
03221                                   L12GP-CERT-T                    ECS035
03222                                   L12CA-CERT-T                    ECS035
03223                                   L12GD-CERT-T                    ECS035
03224          ADD CANCEL-T          TO L12AC-CANCEL-T                  ECS035
03225                                   L12ST-CANCEL-T                  ECS035
03226                                   L12GP-CANCEL-T                  ECS035
03227                                   L12CA-CANCEL-T                  ECS035
03228                                   L12GD-CANCEL-T                  ECS035
03229          ADD L-COVERAGE-T      TO L12AC-L-COVERAGE-T              ECS035
03230                                   L12ST-L-COVERAGE-T              ECS035
03231                                   L12GP-L-COVERAGE-T              ECS035
03232                                   L12CA-L-COVERAGE-T              ECS035
03233                                   L12GD-L-COVERAGE-T              ECS035
03234          ADD A-COVERAGE-T      TO L12AC-A-COVERAGE-T              ECS035
03235                                   L12ST-A-COVERAGE-T              ECS035
03236                                   L12GP-A-COVERAGE-T              ECS035
03237                                   L12CA-A-COVERAGE-T              ECS035
03238                                   L12GD-A-COVERAGE-T              ECS035
03239          ADD LPRM-T            TO L12AC-LPRM-T                    ECS035
03240                                   L12ST-LPRM-T                    ECS035
03241                                   L12GP-LPRM-T                    ECS035
03242                                   L12CA-LPRM-T                    ECS035
03243                                   L12GD-LPRM-T                    ECS035
03244          ADD LCAN-T            TO L12AC-LCAN-T                    ECS035
03245                                   L12ST-LCAN-T                    ECS035
03246                                   L12GP-LCAN-T                    ECS035
03247                                   L12CA-LCAN-T                    ECS035
03248                                   L12GD-LCAN-T                    ECS035
03249          ADD LCLM-T            TO L12AC-LCLM-T                    ECS035
03250                                   L12ST-LCLM-T                    ECS035
03251                                   L12GP-LCLM-T                    ECS035
03252                                   L12CA-LCLM-T                    ECS035
03253                                   L12GD-LCLM-T                    ECS035
03254          ADD APRM-T            TO L12AC-APRM-T                    ECS035
03255                                   L12ST-APRM-T                    ECS035
03256                                   L12GP-APRM-T                    ECS035
03257                                   L12CA-APRM-T                    ECS035
03258                                   L12GD-APRM-T                    ECS035
03259          ADD ACAN-T            TO L12AC-ACAN-T                    ECS035
03260                                   L12ST-ACAN-T                    ECS035
03261                                   L12GP-ACAN-T                    ECS035
03262                                   L12CA-ACAN-T                    ECS035
03263                                   L12GD-ACAN-T                    ECS035
03264          ADD ACLM-T            TO L12AC-ACLM-T                    ECS035
03265                                   L12ST-ACLM-T                    ECS035
03266                                   L12GP-ACLM-T                    ECS035
03267                                   L12CA-ACLM-T                    ECS035
03268                                   L12GD-ACLM-T                    ECS035
03269          ADD TPRM-T            TO L12AC-TPRM-T                    ECS035
03270                                   L12ST-TPRM-T                    ECS035
03271                                   L12GP-TPRM-T                    ECS035
03272                                   L12CA-TPRM-T                    ECS035
03273                                   L12GD-TPRM-T                    ECS035
03274          ADD TCOM-T            TO L12AC-TCOM-T                    ECS035
03275                                   L12ST-TCOM-T                    ECS035
03276                                   L12GP-TCOM-T                    ECS035
03277                                   L12CA-TCOM-T                    ECS035
03278                                   L12GD-TCOM-T.                   ECS035
03279                                                                   ECS035
03280      IF RUN-CCYY = COMP-CCYY (X1)                                    CL*31
03281          ADD CERT-T            TO YTDAC-CERT-T                    ECS035
03282                                   YTDST-CERT-T                    ECS035
03283                                   YTDGP-CERT-T                    ECS035
03284                                   YTDCA-CERT-T                    ECS035
03285                                   YTDGD-CERT-T                    ECS035
03286          ADD CANCEL-T          TO YTDAC-CANCEL-T                  ECS035
03287                                   YTDST-CANCEL-T                  ECS035
03288                                   YTDGP-CANCEL-T                  ECS035
03289                                   YTDCA-CANCEL-T                  ECS035
03290                                   YTDGD-CANCEL-T                  ECS035
03291          ADD L-COVERAGE-T      TO YTDAC-L-COVERAGE-T              ECS035
03292                                   YTDST-L-COVERAGE-T              ECS035
03293                                   YTDGP-L-COVERAGE-T              ECS035
03294                                   YTDCA-L-COVERAGE-T              ECS035
03295                                   YTDGD-L-COVERAGE-T              ECS035
03296          ADD A-COVERAGE-T      TO YTDAC-A-COVERAGE-T              ECS035
03297                                   YTDST-A-COVERAGE-T              ECS035
03298                                   YTDGP-A-COVERAGE-T              ECS035
03299                                   YTDCA-A-COVERAGE-T              ECS035
03300                                   YTDGD-A-COVERAGE-T              ECS035
03301          ADD LPRM-T            TO YTDAC-LPRM-T                    ECS035
03302                                   YTDST-LPRM-T                    ECS035
03303                                   YTDGP-LPRM-T                    ECS035
03304                                   YTDCA-LPRM-T                    ECS035
03305                                   YTDGD-LPRM-T                    ECS035
03306          ADD LCAN-T            TO YTDAC-LCAN-T                    ECS035
03307                                   YTDST-LCAN-T                    ECS035
03308                                   YTDGP-LCAN-T                    ECS035
03309                                   YTDCA-LCAN-T                    ECS035
03310                                   YTDGD-LCAN-T                    ECS035
03311          ADD LCLM-T            TO YTDAC-LCLM-T                    ECS035
03312                                   YTDST-LCLM-T                    ECS035
03313                                   YTDGP-LCLM-T                    ECS035
03314                                   YTDCA-LCLM-T                    ECS035
03315                                   YTDGD-LCLM-T                    ECS035
03316          ADD APRM-T            TO YTDAC-APRM-T                    ECS035
03317                                   YTDST-APRM-T                    ECS035
03318                                   YTDGP-APRM-T                    ECS035
03319                                   YTDCA-APRM-T                    ECS035
03320                                   YTDGD-APRM-T                    ECS035
03321          ADD ACLM-T            TO YTDAC-ACLM-T                    ECS035
03322                                   YTDST-ACLM-T                    ECS035
03323                                   YTDGP-ACLM-T                    ECS035
03324                                   YTDCA-ACLM-T                    ECS035
03325                                   YTDGD-ACLM-T                    ECS035
03326          ADD ACAN-T            TO YTDAC-ACAN-T                    ECS035
03327                                   YTDST-ACAN-T                    ECS035
03328                                   YTDGP-ACAN-T                    ECS035
03329                                   YTDCA-ACAN-T                    ECS035
03330                                   YTDGD-ACAN-T                    ECS035
03331          ADD TPRM-T            TO YTDAC-TPRM-T                    ECS035
03332                                   YTDST-TPRM-T                    ECS035
03333                                   YTDGP-TPRM-T                    ECS035
03334                                   YTDCA-TPRM-T                    ECS035
03335                                   YTDGD-TPRM-T                    ECS035
03336          ADD TCOM-T            TO YTDAC-TCOM-T                    ECS035
03337                                   YTDST-TCOM-T                    ECS035
03338                                   YTDGP-TCOM-T                    ECS035
03339                                   YTDCA-TCOM-T                    ECS035
03340                                   YTDGD-TCOM-T.                   ECS035
03341                                                                   ECS035
03342  2510-EXIT.                                                       ECS035
03343       EXIT.                                                       ECS035
03344                                                                   ECS035
03345  2520-PRINT-LAST-AC-12.                                           ECS035
03346                                                                   ECS035
03347      MOVE SPACES                 TO DETAIL-TMS-LINE-1             ECS035
03348                                     DETAIL-TMS-LINE-2.            ECS035
03349                                                                   ECS035
03350      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                ECS035
03351      MOVE L12AC-CERT-T           TO DET-TMS-CERTS.                ECS035
03352      MOVE L12AC-CANCEL-T         TO DET-TMS-CANCELS.              ECS035
03353      MOVE L12AC-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             ECS035
03354      MOVE L12AC-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             ECS035
03355      MOVE L12AC-LPRM-T           TO DET-TMS-LPRM.                 ECS035
03356      MOVE L12AC-LCLM-T           TO DET-TMS-LCLM.                 ECS035
03357      MOVE L12AC-LCAN-T           TO DET-TMS-LCAN.                 ECS035
03358      MOVE L12AC-APRM-T           TO DET-TMS-APRM.                 ECS035
03359      MOVE L12AC-ACAN-T           TO DET-TMS-ACAN.                 ECS035
03360      MOVE L12AC-ACLM-T           TO DET-TMS-ACLM.                 ECS035
03361      MOVE L12AC-TPRM-T           TO DET-TMS-TPRM.                 ECS035
03362      MOVE L12AC-TCOM-T           TO DET-TMS-TCOM.                 ECS035
03363      COMPUTE DET-TMS-NPRM = (L12AC-TPRM-T  - L12AC-TCOM-T).       ECS035
03364      MOVE '*12 MO '              TO DET-TMS-DATE.                 ECS035
03365      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                ECS035
03366                                                                   ECS035
03367      MOVE '0'                    TO X.                            ECS035
03368      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       ECS035
03369      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03370      MOVE ' '                    TO X.                            ECS035
03371      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       ECS035
03372      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03373      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       ECS035
03374      MOVE ' '                    TO X.                            ECS035
03375      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03376                                                                   ECS035
03377      MOVE ZERO-ACCUM             TO LAST-12-ACCOUNT-ACCUM.        ECS035
03378      MOVE ZERO-ACCUM-T           TO LAST-12-ACCOUNT-ACCUM-T.      ECS035
03379                                                                   ECS035
03380  2530-PRINT-AC-YTD.                                               ECS035
03381                                                                   ECS035
03382      MOVE SPACES                 TO DETAIL-TMS-LINE-1             ECS035
03383                                     DETAIL-TMS-LINE-2.            ECS035
03384                                                                   ECS035
03385      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                ECS035
03386      MOVE YTDAC-CERT-T           TO DET-TMS-CERTS.                ECS035
03387      MOVE YTDAC-CANCEL-T         TO DET-TMS-CANCELS.              ECS035
03388      MOVE YTDAC-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             ECS035
03389      MOVE YTDAC-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             ECS035
03390      MOVE YTDAC-LPRM-T           TO DET-TMS-LPRM.                 ECS035
03391      MOVE YTDAC-LCAN-T           TO DET-TMS-LCAN.                 ECS035
03392      MOVE YTDAC-LCLM-T           TO DET-TMS-LCLM.                 ECS035
03393      MOVE YTDAC-APRM-T           TO DET-TMS-APRM.                 ECS035
03394      MOVE YTDAC-ACAN-T           TO DET-TMS-ACAN.                 ECS035
03395      MOVE YTDAC-ACLM-T           TO DET-TMS-ACLM.                 ECS035
03396      MOVE YTDAC-TPRM-T           TO DET-TMS-TPRM.                 ECS035
03397      MOVE YTDAC-TCOM-T           TO DET-TMS-TCOM.                 ECS035
03398      COMPUTE DET-TMS-NPRM = (YTDAC-TPRM-T  - YTDAC-TCOM-T).       ECS035
03399                                                                   ECS035
03400 *    MOVE 'YEAR TO DATE'         TO DET-TMS-TITLE.                ECS035
03401      MOVE '*YTD   '              TO DET-TMS-DATE.                 ECS035
03402      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                ECS035
03403      MOVE '0'                    TO X.                            ECS035
03404      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       ECS035
03405      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03406      MOVE ' '                    TO X.                            ECS035
03407      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       ECS035
03408      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03409      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       ECS035
03410      MOVE ' '                    TO X.                            ECS035
03411      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03412                                                                   ECS035
03413      MOVE SPACES                 TO DET-TITLE.                    ECS035
03414      MOVE ZERO-ACCUM             TO YTD-ACCOUNT-ACCUM.            ECS035
03415      MOVE ZERO-ACCUM-T           TO YTD-ACCOUNT-ACCUM-T.          ECS035
03416                                                                   ECS035
03417  2540-ZERO-ACCOUNT.                                               ECS035
03418      MOVE +0                     TO X1.                           ECS035
03419      PERFORM 0150-ZERO-ACCUM-ACC THRU 0150-EXIT 13 TIMES.         ECS035
03420  EJECT                                                            ECS035
03421                                                                   ECS035
03422  2560-STATE-BREAK.                                                ECS035
03423      MOVE 0                      TO A1.                           ECS035
03424      PERFORM 2400-BUILD-EXTRACTS THRU 2470-EXIT                   ECS035
03425       VARYING X1 FROM 2 BY 1 UNTIL                                ECS035
03426        X1 IS GREATER THAN DATE-RANGE-MAX.                         ECS035
03427      MOVE SPACES                 TO  B-E-AGT-TABLE.               ECS035
03428      MOVE +0                     TO  X1.                          ECS035
03429      PERFORM 0155-ZERO-DATE-RANGE-ACC THRU 0155-EXIT 13 TIMES.    ECS035
03430      PERFORM 2380-ACCOUNT-BREAK THRU 2540-ZERO-ACCOUNT.           ECS035
03431                                                                   ECS035
03432  2570-STATE-BREAK-1.                                              ECS035
03433      IF P-ST-SW = '1'                                             ECS035
03434          MOVE ' '                TO P-ST-SW                       ECS035
03435      ELSE                                                         ECS035
03436          GO TO 2610-ZERO-STATE.                                   ECS035
03437                                                                   ECS035
03438      PERFORM 0850-ST-HD          THRU 0850-EXIT.                  ECS035
03439      MOVE +1                     TO X1.                           ECS035
03440                                                                   ECS035
03441  2580-STATE-BREAK-2.                                              ECS035
03442      IF X1 GREATER THAN DATE-RANGE-MAX-1                          ECS035
03443          GO TO 2590-PRINT-LAST-ST-12.                             ECS035
03444                                                                   ECS035
03445      MOVE SPACES                 TO DETAIL-TMS-LINE-1             ECS035
03446                                     DETAIL-TMS-LINE-2.            ECS035
03447                                                                   ECS035
03448      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                ECS035
03449      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                ECS035
03450      MOVE ST-CERT-T (X1)         TO DET-TMS-CERTS.                ECS035
03451      MOVE ST-CANCEL-T (X1)       TO DET-TMS-CANCELS.              ECS035
03452      MOVE ST-L-COVERAGE-T (X1)   TO DET-TMS-LCOVERAG.             ECS035
03453      MOVE ST-A-COVERAGE-T (X1)   TO DET-TMS-ACOVERAG.             ECS035
03454      MOVE ST-LPRM-T (X1)         TO DET-TMS-LPRM.                 ECS035
03455      MOVE ST-LCAN-T (X1)         TO DET-TMS-LCAN.                 ECS035
03456      MOVE ST-LCLM-T (X1)         TO DET-TMS-LCLM.                 ECS035
03457      MOVE ST-APRM-T (X1)         TO DET-TMS-APRM.                 ECS035
03458      MOVE ST-ACAN-T (X1)         TO DET-TMS-ACAN.                 ECS035
03459      MOVE ST-ACLM-T (X1)         TO DET-TMS-ACLM.                 ECS035
03460      MOVE ST-TPRM-T (X1)         TO DET-TMS-TPRM.                 ECS035
03461      MOVE ST-TCOM-T (X1)         TO DET-TMS-TCOM.                 ECS035
03462      COMPUTE DET-TMS-NPRM = (ST-TPRM-T (X1) - ST-TCOM-T (X1)).    ECS035
03463                                                                   ECS035
03464      ADD +1                      TO X1.                           ECS035
03465      MOVE COMP-YR (X1)           TO DET-TMS-YR.                   ECS035
03466      MOVE COMP-MO (X1)           TO DET-MO                        ECS035
03467                                     DET-TMS-MO.                   ECS035
03468      PERFORM 0940-LOAD-ALPHA-MONTH THRU 0950-EXIT.                ECS035
03469      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       ECS035
03470                                                                   ECS035
03471      MOVE '0'                    TO X.                            ECS035
03472                                                                   ECS035
03473      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03474      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       ECS035
03475                                                                   ECS035
03476      MOVE ' '                    TO X.                            ECS035
03477                                                                   ECS035
03478      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03479      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       ECS035
03480      MOVE ' '                    TO X.                            ECS035
03481      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03482                                                                   ECS035
03483                                                                   ECS035
03484      GO TO 2580-STATE-BREAK-2.                                    ECS035
03485                                                                   ECS035
03486  2590-PRINT-LAST-ST-12.                                           ECS035
03487                                                                   ECS035
03488      MOVE SPACES                 TO DETAIL-TMS-LINE-1             ECS035
03489                                     DETAIL-TMS-LINE-2.            ECS035
03490                                                                   ECS035
03491      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                ECS035
03492      MOVE L12ST-CERT-T           TO DET-TMS-CERTS.                ECS035
03493      MOVE L12ST-CANCEL-T         TO DET-TMS-CANCELS.              ECS035
03494      MOVE L12ST-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             ECS035
03495      MOVE L12ST-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             ECS035
03496      MOVE L12ST-LPRM-T           TO DET-TMS-LPRM.                 ECS035
03497      MOVE L12ST-LCAN-T           TO DET-TMS-LCAN.                 ECS035
03498      MOVE L12ST-LCLM-T           TO DET-TMS-LCLM.                 ECS035
03499      MOVE L12ST-APRM-T           TO DET-TMS-APRM.                 ECS035
03500      MOVE L12ST-ACAN-T           TO DET-TMS-ACAN.                 ECS035
03501      MOVE L12ST-ACLM-T           TO DET-TMS-ACLM.                 ECS035
03502      MOVE L12ST-TPRM-T           TO DET-TMS-TPRM.                 ECS035
03503      MOVE L12ST-TCOM-T           TO DET-TMS-TCOM.                 ECS035
03504      COMPUTE DET-TMS-NPRM = (L12ST-TPRM-T  - L12ST-TCOM-T).       ECS035
03505                                                                   ECS035
03506 *    MOVE 'LAST 12 MO'           TO DET-TMS-TITLE.                ECS035
03507      MOVE '*12 MO '              TO DET-TMS-DATE.                 ECS035
03508      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                ECS035
03509                                                                   ECS035
03510      MOVE '0'                    TO X.                            ECS035
03511      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       ECS035
03512      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03513      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       ECS035
03514      MOVE ' '                    TO X.                            ECS035
03515                                                                   ECS035
03516      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03517                                                                   ECS035
03518      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       ECS035
03519      MOVE ' '                    TO X.                            ECS035
03520      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03521                                                                   ECS035
03522      MOVE ZERO-ACCUM             TO LAST-12-STATE-ACCUM.          ECS035
03523      MOVE ZERO-ACCUM-T           TO LAST-12-STATE-ACCUM-T.        ECS035
03524                                                                   ECS035
03525  2600-PRINT-ST-YTD.                                               ECS035
03526      MOVE SPACES                 TO DETAIL-TMS-LINE-1             ECS035
03527                                     DETAIL-TMS-LINE-2.            ECS035
03528                                                                   ECS035
03529      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                ECS035
03530      MOVE YTDST-CERT-T           TO DET-TMS-CERTS.                ECS035
03531      MOVE YTDST-CANCEL-T         TO DET-TMS-CANCELS.              ECS035
03532      MOVE YTDST-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             ECS035
03533      MOVE YTDST-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             ECS035
03534      MOVE YTDST-LPRM-T           TO DET-TMS-LPRM.                 ECS035
03535      MOVE YTDST-LCAN-T           TO DET-TMS-LCAN.                 ECS035
03536      MOVE YTDST-LCLM-T           TO DET-TMS-LCLM.                 ECS035
03537      MOVE YTDST-APRM-T           TO DET-TMS-APRM.                 ECS035
03538      MOVE YTDST-ACAN-T           TO DET-TMS-ACAN.                 ECS035
03539      MOVE YTDST-ACLM-T           TO DET-TMS-ACLM.                 ECS035
03540      MOVE YTDST-TPRM-T           TO DET-TMS-TPRM.                 ECS035
03541      MOVE YTDST-TCOM-T           TO DET-TMS-TCOM.                 ECS035
03542      COMPUTE DET-TMS-NPRM = (YTDST-TPRM-T - YTDST-TCOM-T).        ECS035
03543                                                                   ECS035
03544 *    MOVE 'YEAR TO DATE'         TO DET-TMS-TITLE.                ECS035
03545      MOVE '*YTD   '              TO DET-TMS-DATE.                 ECS035
03546      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                ECS035
03547      MOVE '0'                    TO X.                            ECS035
03548      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       ECS035
03549      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03550      MOVE SPACES                 TO DET-TMS-TITLE.                ECS035
03551      MOVE ' '                    TO X.                            ECS035
03552      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       ECS035
03553      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03554      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       ECS035
03555      MOVE ' '                    TO X.                            ECS035
03556      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03557                                                                   ECS035
03558      MOVE ZERO-ACCUM             TO YTD-STATE-ACCUM.              ECS035
03559      MOVE ZERO-ACCUM-T           TO YTD-STATE-ACCUM-T.            ECS035
03560                                                                   ECS035
03561  2610-ZERO-STATE.                                                 ECS035
03562      MOVE +0                     TO X1.                           ECS035
03563      PERFORM 0160-ZERO-ACCUM-ST THRU 0160-EXIT 12 TIMES.          ECS035
03564  EJECT                                                            ECS035
03565  2630-GROUPING-BREAK.                                             ECS035
03566      MOVE 0                      TO A1.                           ECS035
03567      PERFORM 2400-BUILD-EXTRACTS THRU 2470-EXIT                   ECS035
03568       VARYING X1 FROM 2 BY 1 UNTIL                                ECS035
03569        X1 IS GREATER THAN DATE-RANGE-MAX.                         ECS035
03570      MOVE SPACES                 TO  B-E-AGT-TABLE.               ECS035
03571      MOVE +0                     TO  X1.                          ECS035
03572      PERFORM 0155-ZERO-DATE-RANGE-ACC THRU 0155-EXIT 13 TIMES.    ECS035
03573      PERFORM 2380-ACCOUNT-BREAK THRU 2540-ZERO-ACCOUNT.           ECS035
03574      PERFORM 2570-STATE-BREAK-1 THRU 2610-ZERO-STATE.             ECS035
03575                                                                   ECS035
03576  2640-GROUPING-BREAK-1.                                           ECS035
03577      IF P-GRP-SW = '1'                                            ECS035
03578          MOVE ' '                TO P-GRP-SW                      ECS035
03579      ELSE                                                         ECS035
03580          GO TO 2680-ZERO-GROUPING.                                ECS035
03581                                                                   ECS035
03582      PERFORM 0860-GP-HD          THRU 0860-EXIT.                  ECS035
03583      MOVE +1                     TO X1.                           ECS035
03584                                                                   ECS035
03585  2650-GROUPING-BREAK-2.                                           ECS035
03586      IF X1 GREATER THAN DATE-RANGE-MAX-1                          ECS035
03587          GO TO 2660-PRINT-LAST-GP-12.                             ECS035
03588                                                                   ECS035
03589      MOVE SPACES                 TO DETAIL-TMS-LINE-1             ECS035
03590                                     DETAIL-TMS-LINE-2.            ECS035
03591                                                                   ECS035
03592      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                ECS035
03593      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                ECS035
03594      MOVE GP-CERT-T (X1)           TO DET-TMS-CERTS.              ECS035
03595      MOVE GP-CANCEL-T (X1)         TO DET-TMS-CANCELS.            ECS035
03596      MOVE GP-L-COVERAGE-T (X1)     TO DET-TMS-LCOVERAG.           ECS035
03597      MOVE GP-A-COVERAGE-T (X1)     TO DET-TMS-ACOVERAG.           ECS035
03598      MOVE GP-LPRM-T (X1)           TO DET-TMS-LPRM.               ECS035
03599      MOVE GP-LCAN-T (X1)           TO DET-TMS-LCAN.               ECS035
03600      MOVE GP-LCLM-T (X1)           TO DET-TMS-LCLM.               ECS035
03601      MOVE GP-APRM-T (X1)           TO DET-TMS-APRM.               ECS035
03602      MOVE GP-ACAN-T (X1)           TO DET-TMS-ACAN.               ECS035
03603      MOVE GP-ACLM-T (X1)           TO DET-TMS-ACLM.               ECS035
03604      MOVE GP-TPRM-T (X1)           TO DET-TMS-TPRM.               ECS035
03605      MOVE GP-TCOM-T (X1)           TO DET-TMS-TCOM.               ECS035
03606      COMPUTE DET-TMS-NPRM = (GP-TPRM-T (X1) - GP-TCOM-T (X1)).    ECS035
03607                                                                   ECS035
03608      ADD +1                      TO X1.                           ECS035
03609      MOVE COMP-YR (X1)           TO DET-TMS-YR.                   ECS035
03610      MOVE COMP-MO (X1)           TO DET-MO                        ECS035
03611                                     DET-TMS-MO.                   ECS035
03612      PERFORM 0940-LOAD-ALPHA-MONTH THRU 0950-EXIT.                ECS035
03613      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       ECS035
03614                                                                   ECS035
03615      MOVE '0'                    TO X.                            ECS035
03616                                                                   ECS035
03617      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03618      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       ECS035
03619                                                                   ECS035
03620      MOVE ' '                    TO X.                            ECS035
03621                                                                   ECS035
03622      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03623      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       ECS035
03624      MOVE ' '                    TO X.                            ECS035
03625      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03626      GO TO 2650-GROUPING-BREAK-2.                                 ECS035
03627                                                                   ECS035
03628  2660-PRINT-LAST-GP-12.                                           ECS035
03629      MOVE SPACES                 TO DETAIL-TMS-LINE-1             ECS035
03630                                     DETAIL-TMS-LINE-2.            ECS035
03631                                                                   ECS035
03632      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                ECS035
03633      MOVE L12GP-CERT-T           TO DET-TMS-CERTS.                ECS035
03634      MOVE L12GP-CANCEL-T         TO DET-TMS-CANCELS.              ECS035
03635      MOVE L12GP-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             ECS035
03636      MOVE L12GP-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             ECS035
03637      MOVE L12GP-LPRM-T           TO DET-TMS-LPRM.                 ECS035
03638      MOVE L12GP-LCAN-T           TO DET-TMS-LCAN.                 ECS035
03639      MOVE L12GP-LCLM-T           TO DET-TMS-LCLM.                 ECS035
03640      MOVE L12GP-APRM-T           TO DET-TMS-APRM.                 ECS035
03641      MOVE L12GP-ACAN-T           TO DET-TMS-ACAN.                 ECS035
03642      MOVE L12GP-ACLM-T           TO DET-TMS-ACLM.                 ECS035
03643      MOVE L12GP-TPRM-T           TO DET-TMS-TPRM.                 ECS035
03644      MOVE L12GP-TCOM-T           TO DET-TMS-TCOM.                 ECS035
03645      COMPUTE DET-TMS-NPRM = (L12GP-TPRM-T - L12GP-TCOM-T).        ECS035
03646                                                                   ECS035
03647 *    MOVE 'LAST 12 MO'           TO DET-TMS-TITLE.                ECS035
03648      MOVE '*12 MO '              TO DET-TMS-DATE.                 ECS035
03649      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                ECS035
03650                                                                   ECS035
03651      MOVE '0'                    TO X.                            ECS035
03652      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       ECS035
03653                                                                   ECS035
03654      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03655      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       ECS035
03656                                                                   ECS035
03657      MOVE ' '                    TO X.                            ECS035
03658                                                                   ECS035
03659      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03660      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       ECS035
03661      MOVE ' '                    TO X.                            ECS035
03662      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03663      MOVE ZERO-ACCUM             TO LAST-12-GROUPING-ACCUM.       ECS035
03664      MOVE ZERO-ACCUM-T           TO LAST-12-GROUPING-ACCUM-T.     ECS035
03665                                                                   ECS035
03666  2670-PRINT-GP-YTD.                                               ECS035
03667      MOVE SPACES                 TO DETAIL-TMS-LINE-1             ECS035
03668                                     DETAIL-TMS-LINE-2.            ECS035
03669                                                                   ECS035
03670      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                ECS035
03671      MOVE YTDGP-CERT-T           TO DET-TMS-CERTS.                ECS035
03672      MOVE YTDGP-CANCEL-T         TO DET-TMS-CANCELS.              ECS035
03673      MOVE YTDGP-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             ECS035
03674      MOVE YTDGP-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             ECS035
03675      MOVE YTDGP-LPRM-T           TO DET-TMS-LPRM.                 ECS035
03676      MOVE YTDGP-LCAN-T           TO DET-TMS-LCAN.                 ECS035
03677      MOVE YTDGP-LCLM-T           TO DET-TMS-LCLM.                 ECS035
03678      MOVE YTDGP-APRM-T           TO DET-TMS-APRM.                 ECS035
03679      MOVE YTDGP-ACAN-T           TO DET-TMS-ACAN.                 ECS035
03680      MOVE YTDGP-ACLM-T           TO DET-TMS-ACLM.                 ECS035
03681      MOVE YTDGP-TPRM-T           TO DET-TMS-TPRM.                 ECS035
03682      MOVE YTDGP-TCOM-T           TO DET-TMS-TCOM.                 ECS035
03683      COMPUTE DET-TMS-NPRM = (YTDGP-TPRM-T - YTDGP-TCOM-T).        ECS035
03684                                                                   ECS035
03685 *    MOVE 'YEAR TO DATE'         TO DET-TMS-TITLE.                ECS035
03686      MOVE '*YTD   '              TO DET-TMS-DATE.                 ECS035
03687      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                ECS035
03688      MOVE ' '                    TO X.                            ECS035
03689      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       ECS035
03690                                                                   ECS035
03691      MOVE '0'                    TO X.                            ECS035
03692                                                                   ECS035
03693      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03694      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       ECS035
03695                                                                   ECS035
03696      MOVE ' '                    TO X.                            ECS035
03697                                                                   ECS035
03698      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03699      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       ECS035
03700      MOVE ' '                    TO X.                            ECS035
03701      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03702      MOVE ZERO-ACCUM             TO YTD-GROUPING-ACCUM.           ECS035
03703      MOVE ZERO-ACCUM-T           TO YTD-GROUPING-ACCUM-T.         ECS035
03704                                                                   ECS035
03705  2680-ZERO-GROUPING.                                              ECS035
03706      MOVE +0                     TO X1.                           ECS035
03707      PERFORM 0170-ZERO-ACCUM-GRP THRU 0170-EXIT 12 TIMES.         ECS035
03708  EJECT                                                            ECS035
03709  2700-CARRIER-BREAK.                                              ECS035
03710      MOVE 0                      TO A1.                           ECS035
03711      PERFORM 2400-BUILD-EXTRACTS THRU 2470-EXIT                   ECS035
03712       VARYING X1 FROM 2 BY 1 UNTIL                                ECS035
03713        X1 IS GREATER THAN DATE-RANGE-MAX.                         ECS035
03714      MOVE SPACES                 TO  B-E-AGT-TABLE.               ECS035
03715      MOVE +0                     TO  X1.                          ECS035
03716      PERFORM 0155-ZERO-DATE-RANGE-ACC THRU 0155-EXIT 13 TIMES.    ECS035
03717      PERFORM 2380-ACCOUNT-BREAK    THRU 2540-ZERO-ACCOUNT.        ECS035
03718      PERFORM 2570-STATE-BREAK-1    THRU 2610-ZERO-STATE.          ECS035
03719      PERFORM 2640-GROUPING-BREAK-1 THRU 2680-ZERO-GROUPING.       ECS035
03720                                                                   ECS035
03721  2710-CARRIER-BREAK-1.                                            ECS035
03722      IF P-CA-SW = '1'                                             ECS035
03723          MOVE ' '                TO P-CA-SW                       ECS035
03724      ELSE                                                         ECS035
03725          GO TO 2750-ZERO-CARRIER.                                 ECS035
03726                                                                   ECS035
03727      PERFORM 0870-CA-HD          THRU 0870-EXIT.                  ECS035
03728      MOVE +1                     TO X1.                           ECS035
03729                                                                   ECS035
03730  2720-CARRIER-BREAK-2.                                            ECS035
03731      IF X1 GREATER THAN DATE-RANGE-MAX-1                          ECS035
03732          GO TO 2730-PRINT-LAST-CA-12.                             ECS035
03733                                                                   ECS035
03734      MOVE SPACES                 TO DETAIL-TMS-LINE-1.            ECS035
03735      MOVE SPACES                 TO DETAIL-TMS-LINE-2.            ECS035
03736      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                ECS035
03737      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                ECS035
03738      MOVE CA-CERT-T (X1)         TO DET-TMS-CERTS.                ECS035
03739      MOVE CA-CANCEL-T (X1)       TO DET-TMS-CANCELS.              ECS035
03740      MOVE CA-L-COVERAGE-T (X1)   TO DET-TMS-LCOVERAG.             ECS035
03741      MOVE CA-A-COVERAGE-T (X1)   TO DET-TMS-ACOVERAG.             ECS035
03742      MOVE CA-LPRM-T (X1)         TO DET-TMS-LPRM.                 ECS035
03743      MOVE CA-LCAN-T (X1)         TO DET-TMS-LCAN.                 ECS035
03744      MOVE CA-LCLM-T (X1)         TO DET-TMS-LCLM.                 ECS035
03745      MOVE CA-TPRM-T (X1)         TO DET-TMS-TPRM.                 ECS035
03746      MOVE CA-TCOM-T (X1)         TO DET-TMS-TCOM.                 ECS035
03747      COMPUTE DET-TMS-NPRM = (CA-TPRM-T (X1) - CA-TCOM-T (X1)).    ECS035
03748                                                                   ECS035
03749      MOVE CA-APRM-T (X1)         TO DET-TMS-APRM.                 ECS035
03750      MOVE CA-ACAN-T (X1)         TO DET-TMS-ACAN.                 ECS035
03751      MOVE CA-ACLM-T (X1)         TO DET-TMS-ACLM.                 ECS035
03752                                                                   ECS035
03753      ADD +1                      TO X1.                           ECS035
03754      MOVE COMP-YR (X1)           TO DET-TMS-YR.                   ECS035
03755      MOVE COMP-MO (X1)           TO DET-MO                        ECS035
03756                                     DET-TMS-MO.                   ECS035
03757      PERFORM 0940-LOAD-ALPHA-MONTH THRU 0950-EXIT.                ECS035
03758      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       ECS035
03759                                                                   ECS035
03760      MOVE '0'                    TO X.                            ECS035
03761                                                                   ECS035
03762      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03763                                                                   ECS035
03764      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       ECS035
03765                                                                   ECS035
03766      MOVE ' '                    TO X.                            ECS035
03767                                                                   ECS035
03768      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03769                                                                   ECS035
03770      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       ECS035
03771      MOVE ' '                    TO X.                            ECS035
03772      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03773      GO TO 2720-CARRIER-BREAK-2.                                  ECS035
03774                                                                   ECS035
03775  2730-PRINT-LAST-CA-12.                                           ECS035
03776                                                                   ECS035
03777      MOVE SPACES                 TO DETAIL-TMS-LINE-1.            ECS035
03778      MOVE SPACES                 TO DETAIL-TMS-LINE-2.            ECS035
03779      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                ECS035
03780      MOVE L12CA-CERT-T           TO DET-TMS-CERTS.                ECS035
03781      MOVE L12CA-CANCEL-T         TO DET-TMS-CANCELS.              ECS035
03782      MOVE L12CA-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             ECS035
03783      MOVE L12CA-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             ECS035
03784      MOVE L12CA-APRM-T           TO DET-TMS-APRM.                 ECS035
03785      MOVE L12CA-ACAN-T           TO DET-TMS-ACAN.                 ECS035
03786      MOVE L12CA-ACLM-T           TO DET-TMS-ACLM.                 ECS035
03787      MOVE L12CA-LPRM-T           TO DET-TMS-LPRM.                 ECS035
03788      MOVE L12CA-LCAN-T           TO DET-TMS-LCAN.                 ECS035
03789      MOVE L12CA-LCLM-T           TO DET-TMS-LCLM.                 ECS035
03790      MOVE L12CA-TPRM-T           TO DET-TMS-TPRM.                 ECS035
03791      MOVE L12CA-TCOM-T           TO DET-TMS-TCOM.                 ECS035
03792                                                                   ECS035
03793      COMPUTE DET-TMS-NPRM = (L12CA-TPRM-T  -                      ECS035
03794          L12CA-TCOM-T ).                                          ECS035
03795                                                                   ECS035
03796 *    MOVE 'LAST 12 MO'           TO DET-TMS-TITLE.                ECS035
03797      MOVE '*12 MO '              TO DET-TMS-DATE.                 ECS035
03798      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                ECS035
03799                                                                   ECS035
03800      MOVE '0'                    TO X.                            ECS035
03801      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       ECS035
03802      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03803      MOVE ' '                    TO X.                            ECS035
03804      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       ECS035
03805      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03806                                                                   ECS035
03807      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       ECS035
03808      MOVE ' '                    TO X.                            ECS035
03809      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03810      MOVE ZERO-ACCUM             TO LAST-12-CARRIER-ACCUM.        ECS035
03811      MOVE ZERO-ACCUM-T           TO LAST-12-CARRIER-ACCUM-T.      ECS035
03812                                                                   ECS035
03813  2740-PRINT-CA-YTD.                                               ECS035
03814                                                                   ECS035
03815      MOVE SPACES                 TO DETAIL-TMS-LINE-1.            ECS035
03816      MOVE SPACES                 TO DETAIL-TMS-LINE-2.            ECS035
03817      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                ECS035
03818      MOVE YTDCA-CERT-T           TO DET-TMS-CERTS.                ECS035
03819      MOVE YTDCA-CANCEL-T         TO DET-TMS-CANCELS.              ECS035
03820      MOVE YTDCA-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             ECS035
03821      MOVE YTDCA-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             ECS035
03822      MOVE YTDCA-LPRM-T           TO DET-TMS-LPRM.                 ECS035
03823      MOVE YTDCA-LCAN-T           TO DET-TMS-LCAN.                 ECS035
03824      MOVE YTDCA-LCLM-T           TO DET-TMS-LCLM.                 ECS035
03825      MOVE YTDCA-TPRM-T           TO DET-TMS-TPRM.                 ECS035
03826      MOVE YTDCA-TCOM-T           TO DET-TMS-TCOM.                 ECS035
03827      MOVE YTDCA-APRM-T           TO DET-TMS-APRM.                 ECS035
03828      MOVE YTDCA-ACAN-T           TO DET-TMS-ACAN.                 ECS035
03829      MOVE YTDCA-ACLM-T           TO DET-TMS-ACLM.                 ECS035
03830                                                                   ECS035
03831      COMPUTE DET-TMS-NPRM = (YTDCA-TPRM-T  -                      ECS035
03832          YTDCA-TCOM-T).                                           ECS035
03833                                                                   ECS035
03834 *    MOVE 'YEAR TO DATE'         TO DET-TMS-TITLE.                ECS035
03835      MOVE '*YTD   '              TO DET-TMS-DATE.                 ECS035
03836      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                ECS035
03837      MOVE '0'                    TO X.                            ECS035
03838      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       ECS035
03839      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03840                                                                   ECS035
03841      MOVE SPACES                 TO DET-TMS-TITLE.                ECS035
03842                                                                   ECS035
03843      MOVE ' '                    TO X.                            ECS035
03844      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       ECS035
03845      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03846                                                                   ECS035
03847      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       ECS035
03848      MOVE ' '                    TO X.                            ECS035
03849      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03850      MOVE ZERO-ACCUM             TO YTD-CARRIER-ACCUM.            ECS035
03851      MOVE ZERO-ACCUM-T           TO YTD-CARRIER-ACCUM-T.          ECS035
03852                                                                   ECS035
03853  2750-ZERO-CARRIER.                                               ECS035
03854      MOVE +0                     TO X1.                           ECS035
03855      PERFORM 0180-ZERO-ACCUM-CARR THRU 0180-EXIT 12 TIMES.        ECS035
03856  EJECT                                                            ECS035
03857  2770-PRINT-DECISION.                                             ECS035
03858      MOVE +1                     TO X1.                           ECS035
03859      MOVE +0                     TO Y1.                           ECS035
03860                                                                   ECS035
03861  2780-PRINT-DECISION-1.                                           ECS035
03862      ADD 1                       TO X1                            ECS035
03863                                     Y1.                           ECS035
03864                                                                   ECS035
03865      IF X1 GREATER THAN DATE-RANGE-MAX                            ECS035
03866          MOVE +0                 TO X1                            ECS035
03867          GO TO 2790-PRINT-DECISION-2.                             ECS035
03868                                                                   ECS035
03869      COMPUTE LPRM-T = AC-LPRM-T (X1) - AC-LPRM-T (Y1).            ECS035
03870      COMPUTE LCAN-T = AC-LCAN-T (X1) - AC-LCAN-T (Y1).            ECS035
03871      COMPUTE LCLM-T = AC-LCLM-T (X1) - AC-LCLM-T (Y1).            ECS035
03872      COMPUTE L-COVERAGE-T = AC-L-COVERAGE-T (X1) -                ECS035
03873                       AC-L-COVERAGE-T (Y1).                       ECS035
03874      COMPUTE APRM-T = AC-APRM-T (X1) - AC-APRM-T (Y1).            ECS035
03875      COMPUTE ACAN-T = AC-ACAN-T (X1) - AC-ACAN-T (Y1).            ECS035
03876      COMPUTE ACLM-T = AC-ACLM-T (X1) - AC-ACLM-T (Y1).            ECS035
03877      COMPUTE A-COVERAGE-T = AC-A-COVERAGE-T (X1) -                ECS035
03878                       AC-A-COVERAGE-T (Y1).                       ECS035
03879      COMPUTE TCOM-T = AC-TCOM-T (X1) - AC-TCOM-T (Y1).            ECS035
03880                                                                   ECS035
03881      IF COMP-CCYY (X1) = CONV-CCYY AND                               CL*31
03882         COMP-MO (X1) = CONV-MO                                    ECS035
03883           MOVE ZEROS         TO LCAN-T                            ECS035
03884                                 LPRM-T                            ECS035
03885                                 LCLM-T                            ECS035
03886                                 L-COVERAGE-T                      ECS035
03887                                 A-COVERAGE-T                      ECS035
03888                                 APRM-T                            ECS035
03889                                 ACAN-T                            ECS035
03890                                 ACLM-T                            ECS035
03891                                 TCOM-T.                           ECS035
03892                                                                   ECS035
03893      MOVE LPRM-T                 TO PLPRM-T (Y1).                 ECS035
03894      MOVE LCAN-T                 TO PLCAN-T (Y1).                 ECS035
03895      MOVE LCLM-T                 TO PLCLM-T (Y1).                 ECS035
03896      MOVE APRM-T                 TO PAPRM-T (Y1).                 ECS035
03897      MOVE ACAN-T                 TO PACAN-T (Y1).                 ECS035
03898      MOVE ACLM-T                 TO PACLM-T (Y1).                 ECS035
03899      MOVE TCOM-T                 TO PTCOM-T (Y1).                 ECS035
03900      GO TO 2780-PRINT-DECISION-1.                                 ECS035
03901                                                                   ECS035
03902  2790-PRINT-DECISION-2.                                           ECS035
03903      ADD +1                      TO X1.                           ECS035
03904                                                                   ECS035
03905      IF X1 GREATER THAN DATE-RANGE-MAX-1                          ECS035
03906          GO TO 2800-EXIT.                                         ECS035
03907                                                                   ECS035
03908      IF PRINT-ZERO-TABLE-T NOT = PRINT-TABLE-T (X1)               ECS035
03909          MOVE '1'             TO P-ACC-SW                         ECS035
03910                                  P-ST-SW                          ECS035
03911                                  P-GRP-SW                         ECS035
03912                                  P-CA-SW                          ECS035
03913          GO TO 2800-EXIT.                                         ECS035
03914                                                                   ECS035
03915      GO TO 2790-PRINT-DECISION-2.                                 ECS035
03916                                                                   ECS035
03917  2800-EXIT.                                                       ECS035
03918       EXIT.                                                       ECS035
03919                                                                   ECS035
03920  EJECT                                                            ECS035
03921  2960-PRINT-GRAND-TOTALS.                                         ECS035
03922      IF DTE-FICH = '1'                                            ECS035
03923          MOVE '2'                TO DTE-FICH.                     ECS035
CIDMOD
CIDMOD     MOVE  'Y'  TO  PRNT-TOT-SW.
03924                                                                   ECS035
03925      PERFORM 0880-GR-HD          THRU 0880-EXIT.                  ECS035
03926      MOVE +1                     TO X1.                           ECS035
03927                                                                   ECS035
03928  2970-GRAND-TOTAL-1.                                              ECS035
03929      IF X1 GREATER THAN DATE-RANGE-MAX-1                          ECS035
03930          GO TO 2980-GRAND-TOTAL-2.                                ECS035
03931                                                                   ECS035
03932      MOVE SPACES                 TO DETAIL-TMS-LINE-1.            ECS035
03933      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                ECS035
03934      MOVE SPACES                 TO DETAIL-TMS-LINE-2.            ECS035
03935      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                ECS035
03936      MOVE GT-CERT-T (X1)         TO DET-TMS-CERTS.                ECS035
03937      MOVE GT-CANCEL-T (X1)       TO DET-TMS-CANCELS.              ECS035
03938      MOVE GT-L-COVERAGE-T (X1)   TO DET-TMS-LCOVERAG.             ECS035
03939      MOVE GT-A-COVERAGE-T (X1)   TO DET-TMS-ACOVERAG.             ECS035
03940      MOVE GT-LPRM-T (X1)         TO DET-TMS-LPRM.                 ECS035
03941      MOVE GT-LCAN-T (X1)         TO DET-TMS-LCAN.                 ECS035
03942                                                                   ECS035
03943      IF X1 = 12                                                   ECS035
03944          MOVE GT-LPRM (12)       TO ME-035-NET-L                  ECS035
03945          MOVE GT-APRM (12)       TO ME-035-NET-AH.                ECS035
03946                                                                   ECS035
03947      MOVE GT-LCLM-T (X1)         TO DET-TMS-LCLM.                 ECS035
03948      MOVE GT-APRM-T (X1)         TO DET-TMS-APRM.                 ECS035
03949      MOVE GT-ACAN-T (X1)         TO DET-TMS-ACAN.                 ECS035
03950      MOVE GT-ACLM-T (X1)         TO DET-TMS-ACLM.                 ECS035
03951      MOVE GT-TPRM-T (X1)         TO DET-TMS-TPRM.                 ECS035
03952      MOVE GT-TCOM-T (X1)         TO DET-TMS-TCOM.                 ECS035
03953      COMPUTE DET-TMS-NPRM = (GT-TPRM-T (X1) -                     ECS035
03954          GT-TCOM-T (X1)).                                         ECS035
03955                                                                   ECS035
03956      ADD +1                      TO X1.                           ECS035
03957      MOVE COMP-YR (X1)           TO DET-TMS-YR.                   ECS035
03958      MOVE COMP-MO (X1)           TO DET-TMS-MO                    ECS035
03959                                     DET-MO.                       ECS035
03960      PERFORM 0940-LOAD-ALPHA-MONTH THRU 0950-EXIT.                ECS035
03961      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       ECS035
03962                                                                   ECS035
03963      MOVE '0'                    TO X.                            ECS035
03964                                                                   ECS035
03965      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03966      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       ECS035
03967                                                                   ECS035
03968      MOVE ' '                    TO X.                            ECS035
03969                                                                   ECS035
03970      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03971      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       ECS035
03972      MOVE ' '                    TO X.                            ECS035
03973      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
03974      GO TO 2970-GRAND-TOTAL-1.                                    ECS035
03975                                                                   ECS035
03976  2980-GRAND-TOTAL-2.                                              ECS035
03977      MOVE SPACES                 TO DETAIL-TMS-LINE-1.            ECS035
03978      MOVE SPACES                 TO DETAIL-TMS-LINE-2.            ECS035
03979      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                ECS035
03980 *    MOVE 'LAST 12 MO'           TO DET-TMS-TITLE.                ECS035
03981      MOVE '*12 MO '              TO DET-TMS-DATE.                 ECS035
03982      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                ECS035
03983      MOVE '-'                    TO X.                            ECS035
03984      MOVE L12GD-CERT-T           TO DET-TMS-CERTS.                ECS035
03985      MOVE L12GD-CANCEL-T         TO DET-TMS-CANCELS.              ECS035
03986      MOVE L12GD-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             ECS035
03987      MOVE L12GD-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             ECS035
03988      MOVE L12GD-LPRM-T           TO DET-TMS-LPRM.                 ECS035
03989      MOVE L12GD-LCAN-T           TO DET-TMS-LCAN.                 ECS035
03990      MOVE L12GD-LCLM-T           TO DET-TMS-LCLM.                 ECS035
03991      MOVE L12GD-APRM-T           TO DET-TMS-APRM.                 ECS035
03992      MOVE L12GD-ACLM-T           TO DET-TMS-ACLM.                 ECS035
03993      MOVE L12GD-ACAN-T           TO DET-TMS-ACAN.                 ECS035
03994      MOVE L12GD-TPRM-T           TO DET-TMS-TPRM.                 ECS035
03995      MOVE L12GD-TCOM-T           TO DET-TMS-TCOM.                 ECS035
03996      COMPUTE DET-TMS-NPRM = (L12GD-TPRM-T  -                      ECS035
03997          L12GD-TCOM-T).                                           ECS035
03998                                                                   ECS035
03999      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       ECS035
04000      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
04001                                                                   ECS035
04002      MOVE ' '                    TO X.                            ECS035
04003      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       ECS035
04004      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
04005      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       ECS035
04006      MOVE ' '                    TO X.                            ECS035
04007      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
04008                                                                   ECS035
04009      MOVE SPACES                 TO DETAIL-TMS-LINE-1.            ECS035
04010      MOVE SPACES                 TO DETAIL-TMS-LINE-2.            ECS035
04011      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                ECS035
04012 *    MOVE 'YEAR TO DATE'         TO DET-TMS-TITLE.                ECS035
04013      MOVE '*YTD   '              TO DET-TMS-DATE.                 ECS035
04014      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                ECS035
04015      MOVE ' '                    TO X.                            ECS035
04016      MOVE YTDGD-CERT-T           TO DET-TMS-CERTS.                ECS035
04017      MOVE YTDGD-CANCEL-T         TO DET-TMS-CANCELS.              ECS035
04018      MOVE YTDGD-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             ECS035
04019      MOVE YTDGD-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             ECS035
04020      MOVE YTDGD-LPRM-T           TO DET-TMS-LPRM.                 ECS035
04021      MOVE YTDGD-LCAN-T           TO DET-TMS-LCAN.                 ECS035
04022      MOVE YTDGD-LCLM-T           TO DET-TMS-LCLM.                 ECS035
04023      MOVE YTDGD-APRM-T           TO DET-TMS-APRM.                 ECS035
04024      MOVE YTDGD-ACAN-T           TO DET-TMS-ACAN.                 ECS035
04025      MOVE YTDGD-ACLM-T           TO DET-TMS-ACLM.                 ECS035
04026      MOVE YTDGD-TPRM-T           TO DET-TMS-TPRM.                 ECS035
04027      MOVE YTDGD-TCOM-T           TO DET-TMS-TCOM.                 ECS035
04028      COMPUTE DET-TMS-NPRM = (YTDGD-TPRM-T  -                      ECS035
04029          YTDGD-TCOM-T).                                           ECS035
04030                                                                   ECS035
04031      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       ECS035
04032      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
04033                                                                   ECS035
04034      MOVE ' '                    TO X.                            ECS035
04035      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       ECS035
04036      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
04037                                                                   ECS035
04038      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       ECS035
04039      MOVE ' '                    TO X.                            ECS035
04040      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     ECS035
04041  2990-EXIT.                                                       ECS035
04042      EXIT.                                                        ECS035
04043  EJECT                                                            ECS035
04044  9990-END-OF-JOB.                                                 ECS035
04045      IF (DTE-CLIENT EQUAL 'TMS'                                   ECS035
04046         OR DTE-FMT-OPT EQUAL '2')                                 ECS035
04047          PERFORM 2700-CARRIER-BREAK  THRU 2750-ZERO-CARRIER       ECS035
04048          PERFORM 2960-PRINT-GRAND-TOTALS THRU 2990-EXIT           ECS035
04049      ELSE                                                         ECS035
04050          PERFORM 0700-CARRIER-BREAK  THRU 0750-ZERO-CARRIER       ECS035
04051          PERFORM 0960-PRINT-GRAND-TOTALS THRU 0990-EXIT.          ECS035
04052                                                                   ECS035
04053                                                                   ECS035
04054  9999-E-O-J.                                                      ECS035
04055                              COPY ELCPRTC.                        ECS035
04056                                                                   ECS035
04057      CLOSE AM-MAST-IN                                             ECS035
04058            EARNED-PREM                                            ECS035
04059            EXTRACT-OT                                             ECS035
04060            PRNT.                                                  ECS035
04061                                                                   ECS035
04062      IF ME-DO-UPDATE                                              ECS035
070714*        MOVE ME-START-TIME      TO ME-035-START                  ECS035
04064          MOVE ME-CNDS-DATE       TO ME-035-RUN-DT                 ECS035
04065          ACCEPT WS-TIME-OF-DAY   FROM TIME                        ECS035
070714*        MOVE WS-TIME            TO ME-035-END                    ECS035
04067          ADD 1                   TO ME-035-RUN-CT                 ECS035
04068          REWRITE MONTH-END-BALANCES                               ECS035
04069          CLOSE ERMEBL.                                            ECS035
04070                                                                   ECS035
04071      GOBACK.                                                      ECS035
04072                                                                   ECS035
04073                                                                   ECS035
04074  ABEND-PGM SECTION.                                               ECS035
04075                                  COPY ELCABEND.                   ECS035
04076                                                                      CL*32
