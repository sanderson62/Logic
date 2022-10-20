00001  IDENTIFICATION DIVISION.                                         09/28/98
00002                                                                   ECS151
00003  PROGRAM-ID.                 ECS151.                                 LV009
00004 *              PROGRAM CONVERTED BY                                  CL**1
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**1
00006 *              CONVERSION DATE 10/17/97 08:53:48.                    CL**1
00007 *                            VMOD=2.007                              CL**1
00008 *                                                                    CL**1
00009 *AUTHOR.     LOGIC, INC.                                             CL**1
00010 *            DALLAS, TEXAS                                           CL**1
00011                                                                      CL**1
00012 *DATE-COMPILED.                                                      CL**1
00013 *SECURITY.   *****************************************************   CL**1
00014 *            *                                                   *   CL**1
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**1
00016 *            *                                                   *   CL**1
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**1
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**1
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**1
00020 *            *                                                   *   CL**1
00021 *            *****************************************************   CL**1
00022                                                                      CL**1
00023 *REMARKS.  PRINT POLICY EXHIBIT.                                     CL**1
00024 *          OPTION '1' WILL PRINT ALL TOTALS.                         CL**1
00025 *          OPTION '2' WILL PRINT ALL TOTALS EXCEPT ACCOUNT.          CL**1
00026 *          OPTION '3' WILL PRINT ALL STATE TOTALS ONLY.              CL**1
00027 *          OPTION '4' WILL PRINT ALL TOTALS - GROSS ONLY.            CL**1
00028 *          OPTION '5' WILL SKIP ACCNT TOTS - PRINT GROSS ONLY.       CL**1
00029 *          OPTION '6' WILL PRINT STATE TOTS -  GROSS ONLY.           CL**1
00030 *          OPTION '7' WILL PRINT ALL TOTALS - REINS ONLY.            CL**1
00031 *          OPTION '8' WILL SKIP ACCNT TOTS - PRINT REINS ONLY.       CL**1
00032  EJECT                                                               CL**1
00033  ENVIRONMENT DIVISION.                                               CL**1
00034  CONFIGURATION SECTION.                                              CL**1
00035  SPECIAL-NAMES.                                                      CL**1
00036      C02 IS LCP-CH2                                                  CL**1
00037      C03 IS LCP-CH3                                                  CL**1
00038      C04 IS LCP-CH4                                                  CL**1
00039      C05 IS LCP-CH5                                                  CL**1
00040      C06 IS LCP-CH6                                                  CL**1
00041      C07 IS LCP-CH7                                                  CL**1
00042      C08 IS LCP-CH8                                                  CL**1
00043      C09 IS LCP-CH9                                                  CL**1
00044      C10 IS LCP-CH10                                                 CL**1
00045      C11 IS LCP-CH11                                                 CL**1
00046      C12 IS LCP-CH12                                                 CL**1
00047      S01 IS LCP-P01                                                  CL**1
00048      S02 IS LCP-P02.                                                 CL**1
00049  INPUT-OUTPUT SECTION.                                               CL**1
00050  FILE-CONTROL.                                                       CL**1
00051      SELECT EXTR-IN      ASSIGN TO SYS010-UT-2400-S-SYS010.          CL**1
00052      SELECT DISK-DATE    ASSIGN TO SYS019-UT-FBA1-S-SYS019.          CL**1
00053      SELECT PRT-FILE     ASSIGN TO SYS008-UR-1403-S-SYS008.          CL**1
00054      SELECT FICH         ASSIGN TO SYS020-UT-2400-S-SYS020.          CL**1
00055      SELECT SORT-FILE    ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.         CL**1
00056  EJECT                                                               CL**1
00057  DATA DIVISION.                                                      CL**1
00058  FILE SECTION.                                                       CL**1
00059  FD  EXTR-IN                                                         CL**1
00060      BLOCK CONTAINS 0 RECORDS
00061      RECORDING MODE F.                                               CL**1
00062                                                                      CL**1
00063  01  EXTR-RECORD.                                                    CL**1
00064      03  EXTR-KEY.                                                   CL**1
00065          05  EX-REINS-CO         PIC X(06).                          CL**1
00066          05  EX-CARRIER          PIC X(01).                          CL**1
00067          05  EX-COMPANY          PIC X(06).                          CL**1
00068          05  EX-STATE            PIC X(02).                          CL**1
00069          05  EX-ACCOUNT          PIC X(10).                          CL**1
00070          05  EX-ST-SEQ           PIC X(02).                          CL**1
00071          05  EX-REC-TYPE         PIC 99.                             CL**1
00072          05  EX-PERIOD-START     PIC 9(11)   COMP-3.                 CL**2
00073          05  EX-PERIOD-END       PIC 9(11)   COMP-3.                 CL**2
00074      03  EXTR-RECORD-BODY        PIC X(2660).                        CL**2
00075  EJECT                                                               CL**1
00076  FD  DISK-DATE                                                       CL**1
00077                              COPY ELCDTEFD.                          CL**1
00078  EJECT                                                               CL**1
00079  FD  PRT-FILE                                                        CL**1
00080                              COPY ELCPRTFD.                          CL**1
00081                                                                      CL**1
00082  FD  FICH                                                            CL**1
00083                              COPY ELCFCHFD.                          CL**1
00084  EJECT                                                               CL**1
00085  SD  SORT-FILE.                                                      CL**1
00086                                                                      CL**1
00087  01  SORT-REC.                                                       CL**1
00088      03  SORT-KEY.                                                   CL**1
00089          05  SR-REINS-CO         PIC X(06).                          CL**1
00090          05  SR-STATE-SEQ        PIC X(02).                          CL**1
00091          05  SR-CARRIER          PIC X(01).                          CL**1
00092          05  SR-COMPANY          PIC X(06).                          CL**1
00093          05  SR-STATE            PIC X(02).                          CL**1
00094          05  SR-ACCOUNT          PIC X(10).                          CL**1
00095          05  SR-ST-SEQ           PIC X(02).                          CL**1
00096          05  SR-REC-TYPE         PIC 99.                             CL**1
00097          05  SR-PERIOD-START     PIC 9(11)   COMP-3.                 CL**2
00098          05  SR-PERIOD-END       PIC 9(11)   COMP-3.                 CL**2
00099      03  SORT-RECORD-BODY        PIC X(2660).                        CL**1
00100  EJECT                                                               CL**1
00101  WORKING-STORAGE SECTION.                                            CL**1
00102  77  FILLER  PIC X(32) VALUE '********************************'.     CL**1
00103  77  FILLER  PIC X(32) VALUE '     ECS151 WORKING STORAGE     '.     CL**1
00104  77  FILLER  PIC X(32) VALUE '**** VMOD=2.007 ****************'.     CL**1
00105                                                                      CL**1
00106  77  LCP-ASA                     PIC X.                              CL**1
00107  77  EOJ-SW                      PIC X VALUE 'N'.                    CL**1
00108  77  PAGE-CNT                    PIC 9(5) VALUE ZERO.                CL**1
00109  77  FIRST-READ                  PIC X VALUE 'Y'.                    CL**1
00110  77  STATE-INDEX                 PIC XX VALUE SPACES.                CL**1
00111  77  DO-PRINT                    PIC X VALUE SPACE.                  CL**1
00112  77  PRINT-SW                    PIC X VALUE SPACE.                  CL**1
00113                                                                      CL**1
00114  01  WS-ABEND-AREAS.                                                 CL**1
00115      05  WS-ZERO                 PIC S9     VALUE +0 COMP-3.         CL**1
00116      05  WS-RETURN-CODE          PIC S9(4)  VALUE +0 COMP-3.         CL**1
00117      05  WS-ABEND-MESSAGE        PIC X(8)   VALUE SPACES.            CL**1
00118      05  WS-ABEND-FILE-STATUS    PIC XX     VALUE ZERO.              CL**1
00119                                                                      CL**1
CIDMOD 01  WS-REPORT-TITLE             PIC X(10)  VALUE SPACES.
00120  01  WS.                                                             CL**1
00121      03  ABEND-CODE              PIC X(4)    VALUE ZERO.             CL**1
00122      03  ABEND-OPTION            PIC X       VALUE 'Y'.              CL**1
00123      03  PGM-SUB                 PIC S9(3)   VALUE +151 COMP.        CL**1
00124                                                                      CL**1
00125  01  WS-WORK-DATE                PIC 9(11).                          CL**2
00126  01  WS-WORK-DATE-R  REDEFINES  WS-WORK-DATE.                        CL**2
00127      03  FILLER                  PIC 999.                            CL**2
00128      03  WS-CCYY                 PIC 9(4).                           CL**2
00129      03  WS-CCYR  REDEFINES  WS-CCYY.                                CL**2
00130          05  WS-CENT             PIC 99.                             CL**2
00131          05  WS-YEAR             PIC 99.                             CL**2
00132      03  WS-MONTH                PIC 99.                             CL**2
00133      03  WS-DAY                  PIC X9.                             CL**2
00134  EJECT                                                               CL**1
00135  01  HD-1.                                                           CL**1
00136      03  FILLER                  PIC X(12) VALUE '1TOTALS FOR '.     CL**1
00137      03  HD-1-MSG                PIC X(21) VALUE SPACES.             CL**1
CIDMOD     03  HD-1-MSGA               PIC X(17) VALUE SPACES.
CIDMOD*    03  FILLER                  PIC X(17) VALUE SPACES.             CL**1
00139      03  HD-1-COMP               PIC X(30) VALUE SPACES.             CL**1
00140      03  FILLER                  PIC X(45) VALUE SPACES.             CL**1
00141      03  FILLER                  PIC X(8)  VALUE 'ECS-151'.          CL**1
00142                                                                      CL**1
00143  01  HD-2.                                                           CL**1
00144      03  FILLER                  PIC X(12) VALUE '   CARRIER  '.     CL**1
00145      03  HD-2-CARRIER            PIC X     VALUE SPACE.              CL**1
00146      03  FILLER                  PIC X(38) VALUE SPACES.             CL**1
00147      03  HD-2-REIN.                                                  CL**1
00148          05  HD-2-REIN-MSG       PIC X(26) VALUE SPACES.             CL**1
00149          05  HD-2-REIN-COMP      PIC X(4)  VALUE SPACES.             CL**1
00150      03  HD-2-AREA.                                                  CL**1
00151          05  FILLER              PIC X(44).                          CL**1
00152          05  HD-2-IPL            PIC X(8).                           CL**1
00153      03  HD-2-AREA-RDF REDEFINES HD-2-AREA.                          CL**1
00154          05  FILLER              PIC X(37).                          CL**1
00155          05  HD-2-PAGE-LIT       PIC X(5).                           CL**1
00156          05  HD-2-PAGE           PIC ZZ,ZZZ.                         CL**1
00157          05  FILLER              PIC X(4).                           CL**1
00158                                                                      CL**1
00159  01  HD-3.                                                           CL**1
00160      03  FILLER                  PIC X(12) VALUE '  GROUPING  '.     CL**1
00161      03  HD-3-COMPANY            PIC X(6)  VALUE SPACES.             CL**1
00162      03  FILLER                  PIC X(25) VALUE SPACES.             CL**1
00163      03  HD-3-TITLE              PIC X(26) VALUE SPACES.             CL**1
00164      03  HD-3-MSG                PIC X(20) VALUE SPACES.             CL**1
00165      03  FILLER                  PIC X(44) VALUE SPACES.             CL**1
00166                                                                      CL**1
00167  01  HD-4.                                                           CL**1
00168      03  FILLER                  PIC X(12) VALUE '   STATE    '.     CL**1
00169      03  HD-4-STATE              PIC X(20) VALUE SPACES.             CL**1
00170      03  FILLER                  PIC X(49) VALUE SPACES.             CL**1
00171      03  HD-4-AREA.                                                  CL**1
00172          05  FILLER                  PIC X(37).                      CL**1
00173          05  HD-4-PAGE-LIT           PIC X(5).                       CL**1
00174          05  HD-4-PAGE               PIC ZZ,ZZZ.                     CL**1
00175          05  FILLER                  PIC X(4).                       CL**1
00176      03  HD-4-AREA-RDF REDEFINES HD-4-AREA.                          CL**1
00177          05  FILLER                  PIC X(44).                      CL**1
00178          05  HD-4-IPL                PIC X(8).                       CL**1
00179                                                                      CL**1
00180  01  HD-5.                                                           CL**1
00181      03  FILLER                  PIC X(12) VALUE '   ACCOUNT  '.     CL**1
00182      03  HD-5-ACCOUNT            PIC X(10) VALUE SPACES.             CL**1
00183      03  FILLER                  PIC X(26) VALUE SPACES.             CL**1
00184      03  FILLER                  PIC X(22)                           CL**1
00185          VALUE 'FOR PERIOD STARTING - '.                             CL**1
00186      03  HD-5-MONTH              PIC X(10).                          CL**1
00187      03  HD-5-DAY                PIC X(2).                           CL**1
00188      03  FILLER                  PIC X(2) VALUE ', '.                CL**9
00189      03  HD-5-CENT               PIC X(2).                           CL**9
00190      03  HD-5-YEAR               PIC X(2).                           CL**1
00191      03  FILLER                  PIC X(45) VALUE SPACES.             CL**1
00192                                                                      CL**1
00193  01  HD-6.                                                           CL**1
00194      03  FILLER                  PIC X(3)  VALUE SPACES.             CL**1
00195      03  HD-6-MSG                PIC X(53) VALUE SPACES.             CL**1
00196      03  FILLER                  PIC X(10)                           CL**1
00197          VALUE 'ENDING  - '.                                         CL**1
00198      03  HD-6-MONTH              PIC X(10).                          CL**1
00199      03  HD-6-DAY                PIC X(2).                           CL**1
00200      03  FILLER                  PIC X(2) VALUE ', '.                CL**9
00201      03  HD-6-CENT               PIC X(2).                           CL**9
00202      03  HD-6-YEAR               PIC X(2).                           CL**1
00203      03  FILLER                  PIC X(49) VALUE SPACES.             CL**1
00204                                                                      CL**1
00205  01  HD-7.                                                           CL**1
00206      03  FILLER                  PIC X(41) VALUE '0'.                CL**1
00207      03  FILLER                  PIC X(43)                           CL**1
00208          VALUE '*****  G R O U P   C E R T I F I C A T E S '.        CL**1
00209      03  FILLER                  PIC X(6)                            CL**1
00210          VALUE ' *****'.                                             CL**1
00211      03  FILLER                  PIC X(43) VALUE SPACES.             CL**1
00212                                                                      CL**1
00213  01  HD-8.                                                           CL**1
00214      03  FILLER                  PIC X(41) VALUE '0'.                CL**1
00215      03  FILLER                  PIC X(44)                           CL**1
00216          VALUE '*****  I N D I V I D U A L   P O L I C I E S'.       CL**1
00217      03  FILLER                  PIC X(7) VALUE '  *****'.           CL**1
00218      03  FILLER                  PIC X(41) VALUE SPACES.             CL**1
00219                                                                      CL**1
00220  01  HD-9.                                                           CL**1
00221      03  FILLER                  PIC X(50) VALUE SPACES.             CL**1
00222      03  HD-9-MSG                PIC X(32) VALUE SPACES.             CL**1
00223      03  FILLER                  PIC X(51) VALUE SPACES.             CL**1
00224                                                                      CL**1
00225  01  HD-MESSAGES.                                                    CL**1
00226      03  HD-MSG1                 PIC X(20)                           CL**1
00227          VALUE 'COMPLETE BUSINESS - '.                               CL**1
00228      03  HD-MSG2                 PIC X(20)                           CL**1
00229          VALUE 'REINSURED BUSINESS -'.                               CL**1
00230      03  HD-MSG3                 PIC X(26)                           CL**1
00231          VALUE 'FOR REINSURANCE COMPANY - '.                         CL**1
00232      03  HD-MSG4                 PIC X(26)                           CL**1
00233          VALUE '- CERTIFICATE ACTIVITY ON '.                         CL**1
00234      03  HD-MSG5                 PIC X(26)                           CL**1
00235          VALUE ' - CERTIFICATE EXHIBIT ON '.                         CL**1
00236      03  HD-MSG6.                                                    CL**1
00237          05  FILLER              PIC X(21)                           CL**1
00238              VALUE 'REINSURANCE COMPANY  '.                          CL**1
00239          05  HD-MSG6-REIN        PIC X(4).                           CL**1
00240      03  HD-MSG7                 PIC X(34)                           CL**1
00241          VALUE '**********************************'.                 CL**1
00242                                                                      CL**1
00243  01  HD-10.                                                          CL**1
00244      03  FILLER                  PIC X(26)                           CL**1
00245          VALUE '0 BENEFIT TYPE'.                                     CL**1
00246      03  FILLER                  PIC X(37)                           CL**1
00247          VALUE 'I S S U E D'.                                        CL**1
00248      03  FILLER                  PIC X(36)                           CL**1
00249          VALUE 'C A N C E L L E D'.                                  CL**1
00250      03  FILLER                  PIC X(23)                           CL**1
00251          VALUE 'C L A I M S'.                                        CL**1
00252      03  FILLER                  PIC X(11)                           CL**1
00253          VALUE 'E A R N E D'.                                        CL**1
00254                                                                      CL**1
00255  01  HD-11.                                                          CL**1
00256      03  FILLER                  PIC X(17)                           CL**1
00257          VALUE ' **************'.                                    CL**1
00258      03  FILLER                  PIC X(40)                           CL**1
00259          VALUE '************************************'.               CL**1
00260      03  FILLER                  PIC X(39)                           CL**1
00261          VALUE '************************************'.               CL**1
00262      03  FILLER                  PIC X(24)                           CL**1
00263          VALUE '*********************'.                              CL**1
00264      03  FILLER                  PIC X(13)                           CL**1
00265          VALUE '*************'.                                      CL**1
00266                                                                      CL**1
00267  01  HD-12.                                                          CL**1
00268      03  FILLER                  PIC X(17) VALUE SPACES.             CL**1
00269      03  FILLER                  PIC X(8) VALUE '-COUNT-'.           CL**1
00270      03  FILLER                  PIC X(16)                           CL**1
00271          VALUE '----AMOUNT----'.                                     CL**1
00272      03  FILLER                  PIC X(12)                           CL**1
00273          VALUE '--PREMIUM---'.                                       CL**1
00274      03  FILLER                  PIC X(4) VALUE SPACES.              CL**1
00275      03  FILLER                  PIC X(8) VALUE '-COUNT-'.           CL**1
00276      03  FILLER                  PIC X(16)                           CL**1
00277          VALUE '----AMOUNT----'.                                     CL**1
00278      03  FILLER                  PIC X(12)                           CL**1
00279          VALUE '--PREMIUM---'.                                       CL**1
00280      03  FILLER                  PIC X(3) VALUE SPACES.              CL**1
00281      03  FILLER                  PIC X(8) VALUE '-COUNT-'.           CL**1
00282      03  FILLER                  PIC X(16)                           CL**1
00283          VALUE '---AMOUNT----'.                                      CL**1
00284      03  FILLER                  PIC X(13)                           CL**1
00285          VALUE '----AMOUNT---'.                                      CL**1
00286                                                                      CL**1
00287  01  HD-13.                                                          CL**1
00288      03  FILLER                  PIC X(7) VALUE '-'.                 CL**1
00289      03  FILLER                  PIC X(33)                           CL**1
00290          VALUE '**** E X H I B I T   I T E M ****'.                  CL**1
00291      03  FILLER                  PIC X(10) VALUE SPACES.             CL**1
00292      03  FILLER                  PIC X(30)                           CL**1
00293          VALUE '***** GROUP CERTIFICATES *****'.                     CL**1
00294      03  FILLER                  PIC X(10) VALUE SPACES.             CL**1
00295      03  FILLER                  PIC X(31)                           CL**1
00296          VALUE '***** INDIVIDUAL POLICIES *****'.                    CL**1
00297      03  FILLER                  PIC X(12) VALUE SPACES.             CL**1
00298                                                                      CL**1
00299  01  HD-14.                                                          CL**1
00300      03  FILLER                  PIC X(50) VALUE SPACES.             CL**1
00301      03  FILLER                  PIC X(16) VALUE '-COUNT-'.          CL**1
00302      03  FILLER                  PIC X(24) VALUE '----AMOUNT----'.   CL**1
00303      03  FILLER                  PIC X(17) VALUE '-COUNT-'.          CL**1
00304      03  FILLER                  PIC X(26) VALUE '----AMOUNT----'.   CL**1
00305                                                                      CL**1
00306  01  HD-15.                                                          CL**1
00307      03  FILLER                  PIC X(41) VALUE '-'.                CL**1
00308      03  FILLER                  PIC X(46)                           CL**1
00309          VALUE '****  A AND H CLAIMS PAID IN THIS PERIOD  ****'.     CL**1
00310      03  FILLER                  PIC X(46) VALUE SPACES.             CL**1
00311  EJECT                                                               CL**1
00312  01  DETAIL-1.                                                       CL**1
00313      03  FILLER                  PIC X VALUE SPACE.                  CL**1
00314      03  D1-MSG                  PIC X(15) VALUE SPACES.             CL**1
00315      03  D1-ISS-CNT              PIC ZZZ,ZZZ-.                       CL**1
00316      03  D1-ISS-AMT              PIC ZZZZ,ZZZ,ZZZ.ZZ-.               CL**1
00317      03  D1-ISS-PRM              PIC ZZ,ZZZ,ZZZ.ZZ-.                 CL**1
00318      03  FILLER                  PIC XX VALUE SPACES.                CL**1
00319      03  D1-CAN-CNT              PIC ZZZ,ZZZ-.                       CL**1
00320      03  D1-CAN-AMT              PIC ZZZZ,ZZZ,ZZZ.ZZ-.               CL**1
00321      03  D1-CAN-PRM              PIC ZZ,ZZZ,ZZZ.ZZ-.                 CL**1
00322      03  FILLER                  PIC X VALUE SPACES.                 CL**1
00323      03  D1-CLM-CNT              PIC ZZZ,ZZZ-.                       CL**1
00324      03  D1-CLM-AMT              PIC ZZ,ZZZ,ZZZ.ZZ-.                 CL**1
00325      03  FILLER                  PIC X VALUE SPACE.                  CL**1
00326      03  D1-ERN-AMT              PIC ZZZ,ZZZ,ZZZ.ZZ-.                CL**1
00327                                                                      CL**1
00328  01  DETAIL-2.                                                       CL**1
00329      03  FILLER                  PIC X(7) VALUE SPACES.              CL**1
00330      03  D2-MSG                  PIC X(34) VALUE SPACES.             CL**1
00331      03  FILLER                  PIC X(6) VALUE SPACES.              CL**1
00332      03  D2-GRP-CNT              PIC ZZ,ZZZ,ZZZ-.                    CL**1
00333      03  FILLER                  PIC X(6) VALUE SPACES.              CL**1
00334      03  D2-GRP-AMT              PIC ZZ,ZZZ,ZZZ,ZZZ.ZZ-.             CL**1
00335      03  FILLER                  PIC X(5) VALUE SPACES.              CL**1
00336      03  D2-IND-CNT              PIC ZZ,ZZZ,ZZZ-.                    CL**1
00337      03  FILLER                  PIC X(7) VALUE SPACES.              CL**1
00338      03  D2-IND-AMT              PIC ZZ,ZZZ,ZZZ,ZZZ.ZZ-.             CL**1
00339      03  FILLER                  PIC X(10) VALUE SPACES.             CL**1
00340                                                                      CL**1
00341  01  WORK-INDEX.                                                     CL**1
00342      03 Y                        PIC S9(2) COMP.                     CL**1
00343      03 Z                        PIC S9(2) COMP.                     CL**1
00344                                                                      CL**1
00345  01  DETAIL-3.                                                       CL**1
00346      03  FILLER                  PIC X(41) VALUE SPACES.             CL**1
00347      03  D3-MSG                  PIC X(27) VALUE SPACES.             CL**1
00348      03  D3-AH-AMT               PIC ZZ,ZZZ,ZZZ,ZZZ.99-.             CL**1
00349      03  FILLER                  PIC X(47) VALUE SPACES.             CL**1
00350  EJECT                                                               CL**1
00351  01  MONTH-TABLE.                                                    CL**1
00352      03  FILLER                  PIC X(10) VALUE 'JANUARY   '.       CL**1
00353      03  FILLER                  PIC X(10) VALUE 'FEBRUARY  '.       CL**1
00354      03  FILLER                  PIC X(10) VALUE 'MARCH     '.       CL**1
00355      03  FILLER                  PIC X(10) VALUE 'APRIL     '.       CL**1
00356      03  FILLER                  PIC X(10) VALUE 'MAY       '.       CL**1
00357      03  FILLER                  PIC X(10) VALUE 'JUNE      '.       CL**1
00358      03  FILLER                  PIC X(10) VALUE 'JULY      '.       CL**1
00359      03  FILLER                  PIC X(10) VALUE 'AUGUST    '.       CL**1
00360      03  FILLER                  PIC X(10) VALUE 'SEPTEMBER '.       CL**1
00361      03  FILLER                  PIC X(10) VALUE 'OCTOBER   '.       CL**1
00362      03  FILLER                  PIC X(10) VALUE 'NOVEMBER  '.       CL**1
00363      03  FILLER                  PIC X(10) VALUE 'DECEMBER  '.       CL**1
00364                                                                      CL**1
00365  01  MONTH-TABLE-A     REDEFINES MONTH-TABLE.                        CL**1
00366      03  MONTH-SUB               OCCURS 12 TIMES.                    CL**1
00367          05  MONTH-ALPHA         PIC X(10).                          CL**1
00368  EJECT                                                               CL**1
00369  01  TABLE-1.                                                        CL**1
00370      03  TABLE-1-GRP     OCCURS 3 TIMES.                             CL**1
00371          05  TABLE-1-DTL OCCURS 10 TIMES.                            CL**1
00372              07  TBL-ISSUE-CNT   PIC S9(13)          COMP-3.         CL**1
00373              07  TBL-ISSUE-AMT   PIC S9(11)V99       COMP-3.         CL**1
00374              07  TBL-ISSUE-PREM  PIC S9(11)V99       COMP-3.         CL**1
00375              07  TBL-CANCEL-CNT  PIC S9(13)          COMP-3.         CL**1
00376              07  TBL-CANCEL-AMT  PIC S9(11)V99       COMP-3.         CL**1
00377              07  TBL-CANCEL-PREM PIC S9(11)V99       COMP-3.         CL**1
00378              07  TBL-CLAIM-CNT   PIC S9(13)          COMP-3.         CL**1
00379              07  TBL-CLAIM-AMT   PIC S9(11)V99       COMP-3.         CL**1
00380              07  TBL-EARN-PREM   PIC S9(11)V99       COMP-3.         CL**1
00381                                                                      CL**1
00382  01  TABLE-2.                                                        CL**1
00383      03  TABLE-2-GRP     OCCURS 3 TIMES.                             CL**1
00384          05  TBL-2-DTL   OCCURS 9 TIMES.                             CL**1
00385              07  TBL-GRP-CNT     PIC S9(13)          COMP-3.         CL**1
00386              07  TBL-GRP-AMT     PIC S9(11)V99       COMP-3.         CL**1
00387              07  TBL-IND-CNT     PIC S9(13)          COMP-3.         CL**1
00388              07  TBL-IND-AMT     PIC S9(11)V99       COMP-3.         CL**1
00389                                                                      CL**1
00390  01  TABLE-3.                                                        CL**1
00391      03  TABLE-3-GRP     OCCURS 9 TIMES.                             CL**1
00392          05  T3-GRP-CNT          PIC S9(13)          COMP-3.         CL**1
00393          05  T3-GRP-AMT          PIC S9(11)V99       COMP-3.         CL**1
00394          05  T3-IND-CNT          PIC S9(13)          COMP-3.         CL**1
00395          05  T3-IND-AMT          PIC S9(11)V99       COMP-3.         CL**1
00396                                                                      CL**1
00397  01  P1-MESS-R.                                                      CL**1
00398      03  FILLER                  PIC X(32)                           CL**1
00399          VALUE 'TERMS OF 1 MONTH THRU 120 MONTHS'.                   CL**1
00400      03  FILLER                  PIC X(32)                           CL**1
00401          VALUE 'TERMS 61 MONTHS THRU 120 MONTHS '.                   CL**1
00402      03  FILLER                  PIC X(32)                           CL**1
00403          VALUE 'TERMS GREATER THAN 120 MONTHS   '.                   CL**1
00404                                                                      CL**1
00405  01  PAGE-1-MESSAGE REDEFINES P1-MESS-R.                             CL**1
00406      03  P1-MESS         OCCURS 3 TIMES.                             CL**1
00407          05  P1-MSG-1            PIC X(32).                          CL**1
00408                                                                      CL**1
00409  01  TABLE-FOR-PRINT-G.                                              CL**1
00410      03  REPORT-G-SWITCHES  OCCURS 3 TIMES.                          CL**1
00411          05  R-SW-G              PIC X.                              CL**1
00412                                                                      CL**1
00413  01  TABLE-FOR-PRINT-I.                                              CL**1
00414      03  REPORT-I-SWITCHES  OCCURS 3 TIMES.                          CL**1
00415          05  R-SW-I              PIC X.                              CL**1
00416  EJECT                                                               CL**1
00417  01  TOTAL-PRINT-AMOUNTS         COMP-3.                             CL**1
00418      03  G1-ISS-CNT              PIC S9(13).                         CL**1
00419      03  G1-ISS-AMT              PIC S9(11)V99.                      CL**1
00420      03  G1-ISS-PRM              PIC S9(11)V99.                      CL**1
00421      03  G1-CAN-CNT              PIC S9(13).                         CL**1
00422      03  G1-CAN-AMT              PIC S9(11)V99.                      CL**1
00423      03  G1-CAN-PRM              PIC S9(11)V99.                      CL**1
00424      03  G1-CLM-CNT              PIC S9(13).                         CL**1
00425      03  G1-CLM-AMT              PIC S9(11)V99.                      CL**1
00426      03  G1-ERN-PRM              PIC S9(11)V99.                      CL**1
00427                                                                      CL**1
00428  01  TOTAL-AMOUNTS               COMP-3.                             CL**1
00429      03  T1-ISS-CNT              PIC S9(13).                         CL**1
00430      03  T1-ISS-AMT              PIC S9(11)V99.                      CL**1
00431      03  T1-ISS-PRM              PIC S9(11)V99.                      CL**1
00432      03  T1-CAN-CNT              PIC S9(13).                         CL**1
00433      03  T1-CAN-AMT              PIC S9(11)V99.                      CL**1
00434      03  T1-CAN-PRM              PIC S9(11)V99.                      CL**1
00435      03  T1-CLM-CNT              PIC S9(13).                         CL**1
00436      03  T1-CLM-AMT              PIC S9(11)V99.                      CL**1
00437      03  T1-ERN-PRM              PIC S9(11)V99.                      CL**1
00438                                                                      CL**1
00439  01  TOT-AH-ACCUM.                                                   CL**1
00440      03  TOT-AH-PD-THIS          PIC S9(11)V99 COMP-3.               CL**1
00441      03  TOT-AH-PD-LAST          PIC S9(11)V99 COMP-3.               CL**1
00442                                                                      CL**1
00443  01  ZERO-FIELDS-1               COMP-3.                             CL**1
00444      03  FILLER                  PIC S9(13)      VALUE +0.           CL**1
00445      03  FILLER                  PIC S9(11)V99   VALUE +0.           CL**1
00446      03  FILLER                  PIC S9(11)V99   VALUE +0.           CL**1
00447      03  FILLER                  PIC S9(13)      VALUE +0.           CL**1
00448      03  FILLER                  PIC S9(11)V99   VALUE +0.           CL**1
00449      03  FILLER                  PIC S9(11)V99   VALUE +0.           CL**1
00450      03  FILLER                  PIC S9(13)      VALUE +0.           CL**1
00451      03  FILLER                  PIC S9(11)V99   VALUE +0.           CL**1
00452      03  FILLER                  PIC S9(11)V99   VALUE +0.           CL**1
00453                                                                      CL**1
00454  01  ZERO-FIELDS-2               COMP-3.                             CL**1
00455      03  FILLER                  PIC S9(13)      VALUE +0.           CL**1
00456      03  FILLER                  PIC S9(11)V99   VALUE +0.           CL**1
00457      03  FILLER                  PIC S9(13)      VALUE +0.           CL**1
00458      03  FILLER                  PIC S9(11)V99   VALUE +0.           CL**1
00459  EJECT                                                               CL**1
00460  01  CONTROL-AREA.                                                   CL**1
00461      03  CONTROL-A.                                                  CL**1
00462          05  CNTRL-REIN-CO       PIC X(6)  VALUE LOW-VALUES.         CL**1
00463          05  CNTRL-STATE-SEQ     PIC X(2)  VALUE LOW-VALUES.         CL**1
00464          05  CNTRL-CARRIER       PIC X     VALUE LOW-VALUES.         CL**1
00465          05  CNTRL-COMPANY       PIC X(6)  VALUE LOW-VALUES.         CL**1
00466          05  CNTRL-STATE         PIC X(2)  VALUE LOW-VALUES.         CL**1
00467          05  CNTRL-ACCOUNT       PIC X(10) VALUE LOW-VALUES.         CL**1
00468      03  CNTRL-ST-SEQ            PIC X(2)  VALUE LOW-VALUES.         CL**1
00469      03  CNTRL-REC-TYPE          PIC 99    VALUE ZERO.               CL**1
00470                                                                      CL**1
00471  01  RANGE-DATES.                                                    CL**1
00472      03  CNTRL-START             PIC 9(11).                          CL**2
00473      03  CNTRL-END               PIC 9(11).                          CL**2
00474  EJECT                                                               CL**1
00475  01  THE-RECORD.                                                     CL**1
00476      03  ER-SORT-KEY.                                                CL**1
00477          05  ER-CONTROL.                                             CL**1
00478              07  ER-REINS-CO     PIC X(06).                          CL**1
00479              07  ER-STATE-SEQ    PIC X(02).                          CL**1
00480              07  ER-CARRIER      PIC X(01).                          CL**1
00481              07  ER-COMPANY      PIC X(06).                          CL**1
00482              07  ER-STATE        PIC X(02).                          CL**1
00483              07  ER-ACCOUNT      PIC X(10).                          CL**1
00484          05  ER-ST-SEQ           PIC X(02).                          CL**1
00485          05  ER-REC-TYPE         PIC 99.                             CL**1
00486      03  ER-PERIOD-START         PIC 9(11)   COMP-3.                 CL**2
00487      03  ER-PERIOD-END           PIC 9(11)   COMP-3.                 CL**2
00488      03  ER-ACTIVITY-TOTALS.                                         CL**1
00489          05  ER-TERM-GROUP       OCCURS 3 TIMES.                     CL**1
00490 *                1 = 0-60 MONTHS                                     CL**1
00491 *                2 = 61-120 MONTHS                                   CL**1
00492 *                3 = 121 AND OVER.                                   CL**1
00493              07  ER-TYPE-GROUPS  OCCURS 10 TIMES.                    CL**1
00494 *                    1 = LIFE(R-GP)      6 = LIFE(R-IND)             CL**1
00495 *                    2 = LIFE(L-GP)      7 = LIFE(L-IND)             CL**1
00496 *                    3 = LIFE(OB-GP)     8 = LIFE(OB-IND)            CL**1
00497 *                    4 = AH(GP)          9 = AH(IND)                 CL**1
00498 *                    5 = AH(OB-GP)      10 = AH(OB-IND)              CL**1
00499                  09  ER-ISSUE-CNT    PIC S9(13)      COMP-3.         CL**1
00500                  09  ER-ISSUE-AMT    PIC S9(11)V99   COMP-3.         CL**1
00501                  09  ER-ISSUE-PREM   PIC S9(11)V99   COMP-3.         CL**1
00502                  09  ER-CANCEL-CNT   PIC S9(13)      COMP-3.         CL**1
00503                  09  ER-CANCEL-AMT   PIC S9(11)V99   COMP-3.         CL**1
00504                  09  ER-CANCEL-PREM  PIC S9(11)V99   COMP-3.         CL**1
00505                  09  ER-CLAIM-CNT    PIC S9(13)      COMP-3.         CL**1
00506                  09  ER-CLAIM-AMT    PIC S9(11)V99   COMP-3.         CL**1
00507                  09  ER-EARN-PREM    PIC S9(11)V99   COMP-3.         CL**1
00508      03  ER-EXHIBIT-TOTALS.                                          CL**1
00509          05  ER-TERM-GROUP-E     OCCURS 3 TIMES.                     CL**1
00510 *                1 = 0-60 MONTHS                                     CL**1
00511 *                2 = 61-120 MONTHS                                   CL**1
00512 *                3 = 121 AND OVER                                    CL**1
00513              07  ER-LINE-DETAIL  OCCURS 9 TIMES.                     CL**1
00514 *                1 = INFORCE PREVIOUS PER  6 = CANCEL IN PERIOD      CL**1
00515 *                2 = ISSUED IN PERIOD      7 = DECREASES IN PERIOD   CL**1
00516 *                3 = TOTAL INFORCE-START   8 = TOTAL DEC IN PERIOD   CL**1
00517 *                4 = DEATHS IN PERIOD      9 = TOTAL INFORCE (END)   CL**1
00518 *                5 = EXPIRED IN PERIOD                               CL**1
00519                                                                      CL**1
00520                  09  ER-GROUP-CNT    PIC S9(13)      COMP-3.         CL**1
00521                  09  ER-GROUP-AMT    PIC S9(11)V99   COMP-3.         CL**1
00522                  09  ER-IND-CNT      PIC S9(13)      COMP-3.         CL**1
00523                  09  ER-IND-AMT      PIC S9(11)V99   COMP-3.         CL**1
00524                                                                      CL**1
00525      03  ER-AH-PD-THIS               PIC S9(11)V99   COMP-3.         CL**1
00526      03  ER-AH-PD-LAST               PIC S9(11)V99   COMP-3.         CL**1
00527  EJECT                                                               CL**1
00528                              COPY ELCDTECX.                          CL**1
00529  EJECT                                                               CL**1
00530                              COPY ELCDTEVR.                          CL**2
00531  EJECT                                                               CL**2
00532  PROCEDURE DIVISION.                                                 CL**1
00533  0100-READ-DATE.                                                     CL**1
CIDMOD
CIDMOD     ACCEPT      WS-REPORT-TITLE.
CIDMOD
CIDMOD     IF WS-REPORT-TITLE = 'NON CREDIT'
CIDMOD        MOVE ' NON CREDIT '      TO HD-1-MSGA
CIDMOD     ELSE
CIDMOD        IF WS-REPORT-TITLE = 'CREDIT'
CIDMOD           MOVE ' ONLY CREDIT '  TO HD-1-MSGA
CIDMOD        ELSE
CIDMOD           MOVE SPACES           TO HD-1-MSGA
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD
00534                              COPY ELCDTERX.                          CL**1
00535                                                                      CL**1
00536      IF DTE-CLIENT = 'UAL'                                           CL**1
00537          MOVE 9 TO DTE-PGM-OPT.                                      CL**1
00538                                                                      CL**1
00539      MOVE COMPANY-NAME           TO HD-1-COMP.                       CL**1
00540      MOVE SPACES                 TO HD-2-AREA.                       CL**3
00541      MOVE SPACES                 TO HD-4-AREA.                       CL**3
00542                                                                      CL**3
00543      IF DTE-PGM-OPT = 9                                              CL**1
00544          MOVE  WS-CURRENT-DATE   TO HD-4-IPL                         CL**3
00545      ELSE                                                            CL**1
00546          MOVE WS-CURRENT-DATE    TO HD-2-IPL.                        CL**3
00547                                                                      CL**1
00548  EJECT                                                               CL**1
00549  0120-OPEN-FILES.                                                    CL**1
00550      OPEN INPUT EXTR-IN  OUTPUT PRT-FILE.                            CL**1
00551      PERFORM 0510-CLEAR-WORK-TABLES THRU 0520-C-W-EXIT.              CL**1
00552                                                                      CL**1
00553  0130-SORT-PROCEDURE.                                                CL**1
00554      SORT SORT-FILE ASCENDING SORT-KEY                               CL**1
00555          INPUT PROCEDURE 0150-GET-EXTRACT THRU 0160-GET-EXIT         CL**1
00556          OUTPUT PROCEDURE 0180-READ-EXTRACT THRU 0910-EOJ-EXIT.      CL**1
00557                                                                      CL**1
00558      IF SORT-RETURN NOT = ZEROS                                      CL**3
00559          MOVE '0101'             TO ABEND-CODE                       CL**1
00560          GO TO ABEND-PGM.                                            CL**1
00561                                                                      CL**1
00562      MOVE ZEROS  TO RETURN-CODE.
00562      GOBACK.                                                         CL**1
00563                                                                      CL**1
00564  EJECT                                                               CL**1
00565  0150-GET-EXTRACT  SECTION.                                          CL**1
00566      READ EXTR-IN AT END                                             CL**1
00567          CLOSE EXTR-IN                                               CL**1
00568          GO TO 0160-GET-EXIT.                                        CL**1
00569                                                                      CL**1
00570      MOVE EXTR-RECORD-BODY TO SORT-RECORD-BODY.                      CL**1
00571      MOVE EX-REINS-CO      TO SR-REINS-CO.                           CL**1
00572      MOVE EX-CARRIER       TO SR-CARRIER.                            CL**1
00573      MOVE EX-COMPANY       TO SR-COMPANY.                            CL**1
00574      MOVE EX-STATE         TO SR-STATE.                              CL**1
00575      MOVE EX-ACCOUNT       TO SR-ACCOUNT.                            CL**1
00576      MOVE EX-ST-SEQ        TO SR-ST-SEQ.                             CL**1
00577      MOVE EX-REC-TYPE      TO SR-REC-TYPE.                           CL**1
00578      MOVE EX-PERIOD-START  TO SR-PERIOD-START.                       CL**1
00579      MOVE EX-PERIOD-END    TO SR-PERIOD-END.                         CL**1
00580                                                                      CL**1
00581      IF DTE-PGM-OPT = 9                                              CL**1
00582          MOVE EX-STATE  TO SR-STATE-SEQ                              CL**1
00583      ELSE                                                            CL**1
00584          MOVE SPACES    TO SR-STATE-SEQ.                             CL**1
00585                                                                      CL**1
00586      RELEASE SORT-REC.                                               CL**1
00587      GO TO 0150-GET-EXTRACT.                                         CL**1
00588                                                                      CL**1
00589  0160-GET-EXIT.                                                      CL**3
00590      EXIT.                                                           CL**3
00591  EJECT                                                               CL**1
00592  0180-READ-EXTRACT  SECTION.                                         CL**1
00593      RETURN SORT-FILE INTO THE-RECORD AT END                         CL**1
00594          MOVE 'Y'                TO EOJ-SW                           CL**1
00595          GO TO 0230-CONTROL-BREAK.                                   CL**1
00596                                                                      CL**1
00597      IF DTE-PGM-OPT = 1                                              CL**3
00598          GO TO 0190-SET-CONTROL.                                     CL**3
00599                                                                      CL**3
00600      IF DTE-PGM-OPT = 2                                              CL**1
00601          IF ER-REC-TYPE = 01 OR 02                                   CL**1
00602              GO TO 0180-READ-EXTRACT                                 CL**1
00603          ELSE                                                        CL**3
00604              GO TO 0190-SET-CONTROL.                                 CL**1
00605                                                                      CL**1
00606      IF DTE-PGM-OPT = 3                                              CL**1
00607          IF DTE-CLIENT = 'CVL'                                       CL**1
00608              IF ER-REC-TYPE LESS THAN 07                             CL**1
00609                  GO TO 0180-READ-EXTRACT                             CL**1
00610              ELSE                                                    CL**1
00611                  GO TO 0190-SET-CONTROL                              CL**1
00612          ELSE                                                        CL**1
00613              IF ER-REC-TYPE LESS THAN 11                             CL**1
00614                  GO TO 0180-READ-EXTRACT                             CL**1
00615              ELSE                                                    CL**1
00616                  GO TO 0190-SET-CONTROL.                             CL**1
00617                                                                      CL**1
00618      IF DTE-PGM-OPT = 4                                              CL**1
00619          IF ER-REC-TYPE = 02 OR 04 OR 06 OR 08 OR                    CL**1
00620              10 OR 12 OR 14                                          CL**1
00621              GO TO 0180-READ-EXTRACT                                 CL**3
00622          ELSE                                                        CL**3
00623              GO TO 0190-SET-CONTROL.                                 CL**3
00624                                                                      CL**3
00625      IF DTE-PGM-OPT = 5                                              CL**1
00626          IF ER-REC-TYPE = 01 OR 02 OR 04 OR 06 OR                    CL**1
00627              08 OR 10 OR 12 OR 14                                    CL**1
00628              GO TO 0180-READ-EXTRACT                                 CL**3
00629          ELSE                                                        CL**3
00630              GO TO 0190-SET-CONTROL.                                 CL**3
00631                                                                      CL**3
00632      IF DTE-PGM-OPT = 6                                              CL**1
00633          IF ER-REC-TYPE = 11 OR 13                                   CL**1
00634              GO TO 0190-SET-CONTROL                                  CL**1
00635          ELSE                                                        CL**3
00636              GO TO 0180-READ-EXTRACT.                                CL**1
00637                                                                      CL**3
00638      IF DTE-PGM-OPT = 7                                              CL**1
00639          IF ER-REC-TYPE = 01 OR 03 OR 05 OR                          CL**1
00640              07 OR 09 OR 11 OR 13                                    CL**1
00641              GO TO 0180-READ-EXTRACT                                 CL**3
00642          ELSE                                                        CL**3
00643              GO TO 0190-SET-CONTROL.                                 CL**3
00644                                                                      CL**3
00645      IF DTE-PGM-OPT = 8                                              CL**1
00646          IF ER-REC-TYPE = 01 OR 02 OR 03 OR 05 OR                    CL**1
00647              07 OR 09 OR 11 OR 13                                    CL**1
00648              GO TO 0180-READ-EXTRACT                                 CL**3
00649          ELSE                                                        CL**3
00650              GO TO 0190-SET-CONTROL.                                 CL**3
00651                                                                      CL**3
00652      IF DTE-PGM-OPT = 9                                              CL**1
00653          IF ER-REC-TYPE = 07 OR 08 OR 11 OR 12 OR 13 OR 14           CL**1
00654              GO TO 0190-SET-CONTROL                                  CL**1
00655          ELSE                                                        CL**5
00656              GO TO 0180-READ-EXTRACT.                                CL**1
00657                                                                      CL**3
00658      DISPLAY 'PROGRAM OPTION SWITCH = ' DTE-PGM-OPT.                 CL**1
00659      MOVE '0301'                 TO ABEND-CODE.                      CL**1
00660      GO TO ABEND-PGM.                                                CL**1
00661                                                                      CL**1
00662  0190-SET-CONTROL.                                                   CL**1
00663                                                                      CL**1
00664      IF FIRST-READ = 'Y'                                             CL**1
00665          MOVE ER-SORT-KEY        TO CONTROL-AREA                     CL**1
00666          MOVE ER-PERIOD-START    TO CNTRL-START                      CL**1
00667          MOVE ER-PERIOD-END      TO CNTRL-END                        CL**1
00668          MOVE 'N'                TO FIRST-READ.                      CL**1
00669                                                                      CL**1
00670  0200-CHECK-SEQUENCE.                                                CL**1
00671      IF ER-SORT-KEY LESS CONTROL-AREA                                CL**1
00672          DISPLAY 'ECS151 - SEQUENCE ERROR PROGRAM TERMINATED'        CL**1
00673          MOVE '0610'             TO ABEND-CODE                       CL**1
00674          GO TO ABEND-PGM.                                            CL**1
00675                                                                      CL**1
00676      IF ER-SORT-KEY NOT EQUAL CONTROL-AREA                           CL**1
00677          GO TO 0230-CONTROL-BREAK.                                   CL**1
00678                                                                      CL**1
00679  0210-ACCUM-AMOUNTS.                                                 CL**1
00680      PERFORM 0320-ADD-UP-DETAIL THRU 0330-ADD-DETAIL-EXIT            CL**1
00681          VARYING Y FROM 1 BY 1 UNTIL Y GREATER 3 AFTER               CL**1
00682              Z FROM 1 BY 1 UNTIL Z GREATER 10.                       CL**1
00683                                                                      CL**1
00684      PERFORM 0340-ADD-UP-A-H THRU 0350-ADD-AH-EXIT                   CL**1
00685          VARYING Y FROM 1 BY 1 UNTIL Y GREATER 3 AFTER               CL**1
00686              Z FROM 1 BY 1 UNTIL Z GREATER 9.                        CL**1
00687                                                                      CL**1
00688      ADD ER-AH-PD-THIS TO TOT-AH-PD-THIS.                            CL**1
00689      ADD ER-AH-PD-LAST TO TOT-AH-PD-LAST.                            CL**1
00690                                                                      CL**1
00691      GO TO 0180-READ-EXTRACT.                                        CL**1
00692  EJECT                                                               CL**1
00693  0230-CONTROL-BREAK.                                                 CL**1
00694      MOVE '*                   ' TO  HD-2-CARRIER                    CL**3
00695                                      HD-3-COMPANY                    CL**3
00696                                      HD-4-STATE                      CL**3
00697                                      HD-5-ACCOUNT.                   CL**3
00698                                                                      CL**1
00699      IF EOJ-SW = 'Y' AND PRINT-SW = SPACE                            CL**3
00700          GO TO 0890-NO-PRINT.                                        CL**3
00701                                                                      CL**1
00702      MOVE CNTRL-STATE            TO STATE-INDEX.                     CL**1
00703                                                                      CL**1
00704      IF CNTRL-REC-TYPE LESS 03                                       CL**1
00705          MOVE 'ACCOUNT              ' TO HD-1-MSG                    CL**1
00706          MOVE CNTRL-CARRIER      TO HD-2-CARRIER                     CL**1
00707          MOVE CNTRL-COMPANY      TO HD-3-COMPANY                     CL**1
00708          PERFORM 0600-FIND-STATE THRU 0620-FIND-S-EXIT               CL**1
00709          MOVE CNTRL-ACCOUNT      TO HD-5-ACCOUNT                     CL**1
00710          GO TO 0250-END-BREAK.                                       CL**1
00711                                                                      CL**1
00712      IF CNTRL-REC-TYPE LESS 05                                       CL**1
00713          MOVE 'STATE                ' TO HD-1-MSG                    CL**1
00714          MOVE CNTRL-CARRIER      TO HD-2-CARRIER                     CL**1
00715          MOVE CNTRL-COMPANY      TO HD-3-COMPANY                     CL**1
00716          PERFORM 0600-FIND-STATE THRU 0620-FIND-S-EXIT               CL**1
00717          GO TO 0250-END-BREAK.                                       CL**1
00718                                                                      CL**1
00719      IF CNTRL-REC-TYPE LESS 07                                       CL**1
00720          MOVE 'COMPANY              ' TO HD-1-MSG                    CL**1
00721          MOVE CNTRL-CARRIER      TO HD-2-CARRIER                     CL**1
00722          MOVE CNTRL-COMPANY      TO HD-3-COMPANY                     CL**1
00723          GO TO 0250-END-BREAK.                                       CL**1
00724                                                                      CL**1
00725      IF CNTRL-REC-TYPE LESS 09                                       CL**1
00726          IF DTE-PGM-OPT = 9                                          CL**1
00727              MOVE 'STATE / CARRIER      ' TO HD-1-MSG                CL**1
00728              MOVE CNTRL-CARRIER      TO HD-2-CARRIER                 CL**1
00729              PERFORM 0600-FIND-STATE THRU 0620-FIND-S-EXIT           CL**1
00730              GO TO 0250-END-BREAK                                    CL**1
00731          ELSE                                                        CL**1
00732              MOVE 'CARRIER / STATE      ' TO HD-1-MSG                CL**1
00733              MOVE CNTRL-CARRIER      TO HD-2-CARRIER                 CL**1
00734              PERFORM 0600-FIND-STATE THRU 0620-FIND-S-EXIT           CL**1
00735              GO TO 0250-END-BREAK.                                   CL**1
00736                                                                      CL**1
00737      IF CNTRL-REC-TYPE LESS 11                                       CL**1
00738          MOVE 'CARRIER              ' TO HD-1-MSG                    CL**1
00739          MOVE CNTRL-CARRIER      TO HD-2-CARRIER                     CL**1
00740          GO TO 0250-END-BREAK.                                       CL**1
00741                                                                      CL**1
00742      IF CNTRL-REC-TYPE LESS 13                                       CL**1
00743          MOVE 'GRAND TOTAL          ' TO HD-1-MSG                    CL**1
00744          GO TO 0250-END-BREAK.                                       CL**1
00745                                                                      CL**1
00746      MOVE 'STATE OVERALL        ' TO HD-1-MSG.                       CL**1
00747      MOVE CNTRL-ST-SEQ           TO STATE-INDEX.                     CL**1
00748      PERFORM 0600-FIND-STATE THRU 0620-FIND-S-EXIT.                  CL**1
00749  EJECT                                                               CL**1
00750  0250-END-BREAK.                                                     CL**1
00751      MOVE CNTRL-START            TO WS-WORK-DATE.                    CL**1
00752      MOVE WS-DAY                 TO HD-5-DAY.                        CL**1
00753      MOVE WS-YEAR                TO HD-5-YEAR.                       CL**1
00754      MOVE WS-CENT                TO HD-5-CENT.                       CL**9
00755      MOVE MONTH-ALPHA (WS-MONTH) TO HD-5-MONTH.                      CL**1
00756                                                                      CL**1
00757      MOVE CNTRL-END              TO WS-WORK-DATE.                    CL**1
00758      MOVE WS-DAY                 TO HD-6-DAY.                        CL**1
00759      MOVE WS-CENT                TO HD-6-CENT.                       CL**9
00760      MOVE WS-YEAR                TO HD-6-YEAR.                       CL**9
00761      MOVE MONTH-ALPHA (WS-MONTH) TO HD-6-MONTH.                      CL**1
00762                                                                      CL**1
00763      IF CNTRL-REC-TYPE = 01 OR 03 OR 05 OR 07 OR 09 OR 11 OR 13      CL**1
00764          MOVE HD-MSG1            TO HD-3-MSG                         CL**1
00765      ELSE                                                            CL**1
00766          MOVE HD-MSG2            TO HD-3-MSG.                        CL**1
00767                                                                      CL**1
00768      PERFORM 0370-ACCUMULATE-1-120 THRU 0380-A-1-120-EXIT            CL**1
00769          VARYING Z FROM 1 BY 1 UNTIL Z GREATER 10.                   CL**1
00770                                                                      CL**1
00771      PERFORM 0390-ADD-3-AH-LEVELS THRU 0400-ADD-3-EXIT               CL**1
00772          VARYING Z FROM 1 BY 1 UNTIL Z GREATER 9 AFTER               CL**1
00773              Y FROM 1 BY 1 UNTIL Y GREATER 3.                        CL**1
00774                                                                      CL**1
00775      MOVE +1                      TO Y.                              CL**1
00776      PERFORM 0740-CHECK-FOR-PRINT THRU 0750-CHECK-PRINT-EXIT         CL**1
00777          VARYING Z FROM 1 BY 1 UNTIL Z GREATER 5.                    CL**1
00778      MOVE DO-PRINT                TO R-SW-G (Y).                     CL**1
00779                                                                      CL**1
00780      MOVE +2                     TO Y.                               CL**1
00781      PERFORM 0740-CHECK-FOR-PRINT THRU 0750-CHECK-PRINT-EXIT         CL**1
00782          VARYING Z FROM 1 BY 1 UNTIL Z GREATER 5.                    CL**1
00783      MOVE DO-PRINT               TO R-SW-G (Y).                      CL**1
00784                                                                      CL**1
00785      MOVE +3                     TO Y.                               CL**1
00786      PERFORM 0740-CHECK-FOR-PRINT THRU 0750-CHECK-PRINT-EXIT         CL**1
00787          VARYING Z FROM 1 BY 1 UNTIL Z GREATER 5.                    CL**1
00788      MOVE DO-PRINT               TO R-SW-G (Y).                      CL**1
00789                                                                      CL**1
00790      MOVE +1                     TO Y.                               CL**1
00791      PERFORM 0740-CHECK-FOR-PRINT THRU 0750-CHECK-PRINT-EXIT         CL**1
00792          VARYING Z FROM 6 BY 1 UNTIL Z GREATER 10.                   CL**1
00793      MOVE DO-PRINT               TO R-SW-I (Y).                      CL**1
00794                                                                      CL**1
00795      MOVE +2                     TO Y.                               CL**1
00796      PERFORM 0740-CHECK-FOR-PRINT THRU 0750-CHECK-PRINT-EXIT         CL**1
00797          VARYING Z FROM 6 BY 1 UNTIL Z GREATER 10.                   CL**1
00798      MOVE DO-PRINT               TO R-SW-I (Y).                      CL**1
00799                                                                      CL**1
00800      MOVE +3                     TO Y.                               CL**1
00801      PERFORM 0740-CHECK-FOR-PRINT THRU 0750-CHECK-PRINT-EXIT         CL**1
00802          VARYING Z FROM 6 BY 1 UNTIL Z GREATER 10.                   CL**1
00803      MOVE DO-PRINT               TO R-SW-I (Y).                      CL**1
00804  EJECT                                                               CL**1
00805                                                                      CL**1
00806  0270-CONTROL-BREAK-PRINT.                                           CL**1
00807      MOVE HD-MSG4                TO HD-3-TITLE.                      CL**1
00808      IF R-SW-G (1) = SPACE AND R-SW-G (2) = SPACE AND                CL**1
00809         R-SW-G (3) = SPACE AND R-SW-I (1) = SPACE AND                CL**3
00810         R-SW-I (2) = SPACE AND R-SW-I (3) = SPACE                    CL**3
00811          GO TO 0300-CONTROL-4.                                       CL**3
00812                                                                      CL**1
00813      IF PRINT-SW = SPACE                                             CL**3
00814          MOVE 'Y'                TO PRINT-SW.                        CL**3
00815                                                                      CL**1
00816      PERFORM 0640-PRINT-HEADINGS THRU 0650-P-H-X.                    CL**1
00817                                                                      CL**1
00818      IF R-SW-G (1) = SPACE AND R-SW-G (2) = SPACE AND                CL**1
00819         R-SW-G (3) = SPACE                                           CL**3
00820          GO TO 0280-CONTROL-2.                                       CL**3
00821                                                                      CL**1
00822      PERFORM 0770-DUMP-PAGE-1 THRU 0790-DUMP-1-EXIT                  CL**1
00823          VARYING Y FROM 1 BY 1 UNTIL Y GREATER 3.                    CL**1
00824                                                                      CL**1
00825      IF R-SW-I (1) = SPACE AND R-SW-I (2) = SPACE AND                CL**1
00826          R-SW-I (3) = SPACE                                          CL**1
00827              MOVE +3             TO Y                                CL**1
00828              PERFORM 0830-DUMP-2-FINAL THRU 0840-DUMP-2-EXIT         CL**1
00829              GO TO 0290-CONTROL-3.                                   CL**1
00830                                                                      CL**1
00831      PERFORM 0640-PRINT-HEADINGS THRU 0650-P-H-X.                    CL**1
00832                                                                      CL**1
00833  0280-CONTROL-2.                                                     CL**1
00834      PERFORM 0810-DUMP-PAGE-2 THRU 0840-DUMP-2-EXIT                  CL**1
00835          VARYING Y FROM 1 BY 1 UNTIL Y GREATER 3.                    CL**1
00836                                                                      CL**1
00837  0290-CONTROL-3.                                                     CL**1
00838      MOVE HD-MSG5                TO HD-3-TITLE.                      CL**1
00839      PERFORM 0640-PRINT-HEADINGS THRU 0650-P-H-X.                    CL**1
00840      PERFORM 0860-DUMP-PAGE-3 THRU 0870-DUMP-3-EXIT.                 CL**1
00841                                                                      CL**1
00842  0300-CONTROL-4.                                                     CL**1
00843      MOVE 'Y'                    TO FIRST-READ.                      CL**1
00844                                                                      CL**1
00845      IF EOJ-SW = 'Y'                                                 CL**1
00846          GO TO 0900-END-JOB.                                         CL**1
00847                                                                      CL**1
00848      PERFORM 0510-CLEAR-WORK-TABLES THRU 0520-C-W-EXIT.              CL**1
00849      GO TO 0190-SET-CONTROL.                                         CL**1
00850  EJECT                                                               CL**1
00851 *                                                                    CL**1
00852 *        PERFORMED PROCEDURES NEXT                                   CL**1
00853 *                                                                    CL**1
00854  0320-ADD-UP-DETAIL.                                                 CL**1
00855      ADD ER-ISSUE-CNT   (Y, Z) TO TBL-ISSUE-CNT   (Y, Z).            CL**3
00856      ADD ER-ISSUE-AMT   (Y, Z) TO TBL-ISSUE-AMT   (Y, Z).            CL**3
00857      ADD ER-ISSUE-PREM  (Y, Z) TO TBL-ISSUE-PREM  (Y, Z).            CL**3
00858      ADD ER-CANCEL-CNT  (Y, Z) TO TBL-CANCEL-CNT  (Y, Z).            CL**3
00859      ADD ER-CANCEL-AMT  (Y, Z) TO TBL-CANCEL-AMT  (Y, Z).            CL**3
00860      ADD ER-CANCEL-PREM (Y, Z) TO TBL-CANCEL-PREM (Y, Z).            CL**1
00861      ADD ER-CLAIM-CNT   (Y, Z) TO TBL-CLAIM-CNT   (Y, Z).            CL**3
00862      ADD ER-CLAIM-AMT   (Y, Z) TO TBL-CLAIM-AMT   (Y, Z).            CL**3
00863      ADD ER-EARN-PREM   (Y, Z) TO TBL-EARN-PREM   (Y, Z).            CL**3
00864  0330-ADD-DETAIL-EXIT.                                               CL**1
00865      EXIT.                                                           CL**1
00866                                                                      CL**1
00867  0340-ADD-UP-A-H.                                                    CL**1
00868      ADD ER-GROUP-CNT (Y, Z) TO TBL-GRP-CNT (Y, Z).                  CL**1
00869      ADD ER-GROUP-AMT (Y, Z) TO TBL-GRP-AMT (Y, Z).                  CL**1
00870      ADD ER-IND-CNT   (Y, Z) TO TBL-IND-CNT (Y, Z).                  CL**1
00871      ADD ER-IND-AMT   (Y, Z) TO TBL-IND-AMT (Y, Z).                  CL**1
00872  0350-ADD-AH-EXIT.                                                   CL**1
00873      EXIT.                                                           CL**1
00874  EJECT                                                               CL**1
00875                                                                      CL**3
00876  0370-ACCUMULATE-1-120.                                              CL**1
00877      ADD TBL-ISSUE-CNT   (2, Z)   TO TBL-ISSUE-CNT   (1, Z).         CL**1
00878      ADD TBL-ISSUE-AMT   (2, Z)   TO TBL-ISSUE-AMT   (1, Z).         CL**1
00879      ADD TBL-ISSUE-PREM  (2, Z)   TO TBL-ISSUE-PREM  (1, Z).         CL**1
00880      ADD TBL-CANCEL-CNT  (2, Z)   TO TBL-CANCEL-CNT  (1, Z).         CL**1
00881      ADD TBL-CANCEL-AMT  (2, Z)   TO TBL-CANCEL-AMT  (1, Z).         CL**1
00882      ADD TBL-CANCEL-PREM (2, Z)   TO TBL-CANCEL-PREM (1, Z).         CL**1
00883      ADD TBL-CLAIM-CNT   (2, Z)   TO TBL-CLAIM-CNT   (1, Z).         CL**1
00884      ADD TBL-CLAIM-AMT   (2, Z)   TO TBL-CLAIM-AMT   (1, Z).         CL**1
00885      ADD TBL-EARN-PREM   (2, Z)   TO TBL-EARN-PREM   (1, Z).         CL**1
00886  0380-A-1-120-EXIT.                                                  CL**1
00887      EXIT.                                                           CL**1
00888                                                                      CL**1
00889  0390-ADD-3-AH-LEVELS.                                               CL**1
00890      ADD TBL-GRP-CNT (Y, Z) TO T3-GRP-CNT (Z).                       CL**1
00891      ADD TBL-GRP-AMT (Y, Z) TO T3-GRP-AMT (Z).                       CL**1
00892      ADD TBL-IND-CNT (Y, Z) TO T3-IND-CNT (Z).                       CL**1
00893      ADD TBL-IND-AMT (Y, Z) TO T3-IND-AMT (Z).                       CL**1
00894  0400-ADD-3-EXIT.                                                    CL**1
00895      EXIT.                                                           CL**1
00896                                                                      CL**1
00897  0410-ADD-SUB-TOTALS.                                                CL**1
00898      ADD TBL-ISSUE-CNT   (Y, Z) TO G1-ISS-CNT   T1-ISS-CNT.          CL**1
00899      ADD TBL-ISSUE-AMT   (Y, Z) TO G1-ISS-AMT   T1-ISS-AMT.          CL**1
00900      ADD TBL-ISSUE-PREM  (Y, Z) TO G1-ISS-PRM   T1-ISS-PRM.          CL**1
00901      ADD TBL-CANCEL-CNT  (Y, Z) TO G1-CAN-CNT   T1-CAN-CNT.          CL**1
00902      ADD TBL-CANCEL-AMT  (Y, Z) TO G1-CAN-AMT   T1-CAN-AMT.          CL**1
00903      ADD TBL-CANCEL-PREM (Y, Z) TO G1-CAN-PRM   T1-CAN-PRM.          CL**1
00904      ADD TBL-CLAIM-CNT   (Y, Z) TO G1-CLM-CNT   T1-CLM-CNT.          CL**1
00905      ADD TBL-CLAIM-AMT   (Y, Z) TO G1-CLM-AMT   T1-CLM-AMT.          CL**1
00906      ADD TBL-EARN-PREM   (Y, Z) TO G1-ERN-PRM   T1-ERN-PRM.          CL**1
00907  0420-ADD-SUB-EXIT.                                                  CL**1
00908      EXIT.                                                           CL**1
00909                                                                      CL**4
00910  0440-BUILD-DETAIL-1.                                                CL**1
00911      MOVE TBL-ISSUE-CNT   (Y, Z) TO D1-ISS-CNT.                      CL**1
00912      MOVE TBL-ISSUE-AMT   (Y, Z) TO D1-ISS-AMT.                      CL**1
00913      MOVE TBL-ISSUE-PREM  (Y, Z) TO D1-ISS-PRM.                      CL**1
00914      MOVE TBL-CANCEL-CNT  (Y, Z) TO D1-CAN-CNT.                      CL**1
00915      MOVE TBL-CANCEL-AMT  (Y, Z) TO D1-CAN-AMT.                      CL**1
00916      MOVE TBL-CANCEL-PREM (Y, Z) TO D1-CAN-PRM.                      CL**1
00917      MOVE TBL-CLAIM-CNT   (Y, Z) TO D1-CLM-CNT.                      CL**1
00918      MOVE TBL-CLAIM-AMT   (Y, Z) TO D1-CLM-AMT.                      CL**1
00919      MOVE TBL-EARN-PREM   (Y, Z) TO D1-ERN-AMT.                      CL**1
00920  0450-BUILD-D-EXIT.                                                  CL**1
00921      EXIT.                                                           CL**1
00922                                                                      CL**1
00923  0460-BUILD-DETAIL-2.                                                CL**1
00924      MOVE T3-GRP-CNT (Z)         TO D2-GRP-CNT.                      CL**1
00925      MOVE T3-GRP-AMT (Z)         TO D2-GRP-AMT.                      CL**1
00926      MOVE T3-IND-CNT (Z)         TO D2-IND-CNT.                      CL**1
00927      MOVE T3-IND-AMT (Z)         TO D2-IND-AMT.                      CL**1
00928  0470-BUILD-2-EXIT.                                                  CL**1
00929      EXIT.                                                           CL**1
00930                                                                      CL**1
00931  0480-BUILD-DETAIL-2A.                                               CL**1
00932      MOVE TBL-GRP-CNT (Y, Z)     TO D2-GRP-CNT.                      CL**1
00933      MOVE TBL-GRP-AMT (Y, Z)     TO D2-GRP-AMT.                      CL**1
00934      MOVE TBL-IND-CNT (Y, Z)     TO D2-IND-CNT.                      CL**1
00935      MOVE TBL-IND-AMT (Y, Z)     TO D2-IND-AMT.                      CL**1
00936  0490-BUILD-2A-EXIT.                                                 CL**1
00937      EXIT.                                                           CL**1
00938  EJECT                                                               CL**1
00939  0510-CLEAR-WORK-TABLES.                                             CL**1
00940      PERFORM 0530-ZERO-TABLE-1 THRU 0540-Z-T1-EXIT                   CL**1
00941          VARYING Y FROM 1 BY 1 UNTIL Y GREATER 3 AFTER               CL**1
00942              Z FROM 1 BY 1 UNTIL Z GREATER 10.                       CL**1
00943                                                                      CL**1
00944      PERFORM 0550-ZERO-TABLE-2 THRU 0560-Z-T2-EXIT                   CL**1
00945          VARYING Y FROM 1 BY 1 UNTIL Y GREATER 3 AFTER               CL**1
00946              Z FROM 1 BY 1 UNTIL Z GREATER 9.                        CL**1
00947                                                                      CL**1
00948      PERFORM 0570-ZERO-TABLE-3 THRU 0580-Z-T3-EXIT                   CL**1
00949          VARYING Z FROM 1 BY 1 UNTIL Z GREATER 9.                    CL**1
00950                                                                      CL**1
00951      MOVE +0                     TO TOT-AH-PD-THIS                   CL**4
00952                                     TOT-AH-PD-LAST.                  CL**4
00953  0520-C-W-EXIT.                                                      CL**1
00954      EXIT.                                                           CL**1
00955                                                                      CL**1
00956  0530-ZERO-TABLE-1.                                                  CL**1
00957      MOVE ZERO-FIELDS-1          TO TABLE-1-DTL (Y, Z).              CL**1
00958  0540-Z-T1-EXIT.                                                     CL**1
00959      EXIT.                                                           CL**1
00960                                                                      CL**1
00961  0550-ZERO-TABLE-2.                                                  CL**1
00962      MOVE ZERO-FIELDS-2          TO TBL-2-DTL (Y, Z).                CL**1
00963  0560-Z-T2-EXIT.                                                     CL**1
00964      EXIT.                                                           CL**1
00965                                                                      CL**1
00966  0570-ZERO-TABLE-3.                                                  CL**1
00967      MOVE ZERO-FIELDS-2          TO TABLE-3-GRP (Z).                 CL**1
00968  0580-Z-T3-EXIT.                                                     CL**1
00969      EXIT.                                                           CL**1
00970  EJECT                                                               CL**1
00971                                                                      CL**4
00972  0600-FIND-STATE.                                                    CL**1
00973      MOVE +1                     TO Y.                               CL**1
00974  0610-FIND-STATE-NOW.                                                CL**1
00975      IF Y GREATER +75                                                CL**1
00976          MOVE 'UNKNOWN             ' TO HD-4-STATE                   CL**1
00977          GO TO 0620-FIND-S-EXIT.                                     CL**1
00978                                                                      CL**1
00979      IF STATE-INDEX = STATE-SUB (Y) OR STATE-ABBR (Y)                CL**1
00980          MOVE STATE-PIC (Y)      TO HD-4-STATE                       CL**1
00981          GO TO 0620-FIND-S-EXIT.                                     CL**1
00982                                                                      CL**1
00983      ADD +1 TO Y.                                                    CL**1
00984      GO TO 0610-FIND-STATE-NOW.                                      CL**1
00985  0620-FIND-S-EXIT.                                                   CL**1
00986      EXIT.                                                           CL**1
00987  EJECT                                                               CL**1
00988                                                                      CL**4
00989  0640-PRINT-HEADINGS.                                                CL**1
00990      ADD 1 TO PAGE-CNT.                                              CL**1
00991                                                                      CL**1
00992      IF DTE-PGM-OPT = 9                                              CL**1
00993          MOVE PAGE-CNT           TO HD-2-PAGE                        CL**1
00994          MOVE 'PAGE '            TO HD-2-PAGE-LIT                    CL**1
00995      ELSE                                                            CL**1
00996          MOVE PAGE-CNT           TO HD-4-PAGE                        CL**1
00997          MOVE 'PAGE '            TO HD-4-PAGE-LIT.                   CL**1
00998                                                                      CL**4
00999      MOVE HD-1                   TO PRT.                             CL**1
01000      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01001                                                                      CL**4
01002      IF DTE-PGM-OPT = 9                                              CL**1
01003          MOVE HD-4                   TO PRT                          CL**1
01004          PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X                    CL**1
01005          MOVE HD-3                   TO PRT                          CL**1
01006          PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X                    CL**1
01007          MOVE HD-2                   TO PRT                          CL**1
01008      ELSE                                                            CL**1
01009          MOVE HD-2                   TO PRT                          CL**1
01010          PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X                    CL**1
01011          MOVE HD-3                   TO PRT                          CL**1
01012          PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X                    CL**1
01013          MOVE HD-4                   TO PRT.                         CL**1
01014                                                                      CL**4
01015      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01016      MOVE HD-5                   TO PRT.                             CL**1
01017      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01018      MOVE HD-6                   TO PRT.                             CL**1
01019      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01020  0650-P-H-X.                                                         CL**1
01021      EXIT.                                                           CL**1
01022                                                                      CL**1
01023                                                                      CL**1
01024  0660-PRT-RTN.                                                       CL**1
01025      IF DTE-FICH IS NOT EQUAL TO SPACE AND                           CL**1
01026          FICH-OPEN IS EQUAL TO SPACE                                 CL**1
01027          OPEN OUTPUT FICH                                            CL**1
01028          MOVE 'X'                TO FICH-OPEN.                       CL**1
01029                                                                      CL**1
01030      IF DTE-FICH IS NOT EQUAL TO SPACE                               CL**1
01031          WRITE FICH-REC FROM PRT.                                    CL**1
01032                                                                      CL**1
01033      IF DTE-FICH IS EQUAL TO SPACE OR '2'                            CL**1
01034          MOVE P-CTL TO LCP-ASA                                       CL**1
01035          PERFORM LCP-WRITE-POS-PRT                                   CL**1
01036              THRU LCP-WRITE-END-PRT.                                 CL**1
01037                                                                      CL**1
01038  0670-PRT-RTN-X.                                                     CL**1
01039      EXIT.                                                           CL**1
01040  EJECT                                                               CL**1
01041  0690-MOVE-G1-TO-PRINT.                                              CL**1
01042      MOVE G1-ISS-CNT             TO D1-ISS-CNT.                      CL**1
01043      MOVE G1-ISS-AMT             TO D1-ISS-AMT.                      CL**1
01044      MOVE G1-ISS-PRM             TO D1-ISS-PRM.                      CL**1
01045      MOVE G1-CAN-CNT             TO D1-CAN-CNT.                      CL**1
01046      MOVE G1-CAN-AMT             TO D1-CAN-AMT.                      CL**1
01047      MOVE G1-CAN-PRM             TO D1-CAN-PRM.                      CL**1
01048      MOVE G1-CLM-CNT             TO D1-CLM-CNT.                      CL**1
01049      MOVE G1-CLM-AMT             TO D1-CLM-AMT.                      CL**1
01050      MOVE G1-ERN-PRM             TO D1-ERN-AMT.                      CL**1
01051  0700-MOVE-G1-EXIT.                                                  CL**1
01052      EXIT.                                                           CL**1
01053                                                                      CL**1
01054  0710-MOVE-T1-TO-PRINT.                                              CL**1
01055      MOVE T1-ISS-CNT             TO D1-ISS-CNT.                      CL**1
01056      MOVE T1-ISS-AMT             TO D1-ISS-AMT.                      CL**1
01057      MOVE T1-ISS-PRM             TO D1-ISS-PRM.                      CL**1
01058      MOVE T1-CAN-CNT             TO D1-CAN-CNT.                      CL**1
01059      MOVE T1-CAN-AMT             TO D1-CAN-AMT.                      CL**1
01060      MOVE T1-CAN-PRM             TO D1-CAN-PRM.                      CL**1
01061      MOVE T1-CLM-CNT             TO D1-CLM-CNT.                      CL**1
01062      MOVE T1-CLM-AMT             TO D1-CLM-AMT.                      CL**1
01063      MOVE T1-ERN-PRM             TO D1-ERN-AMT.                      CL**1
01064  0720-MOVE-T1-EXIT.                                                  CL**1
01065      EXIT.                                                           CL**1
01066  EJECT                                                               CL**1
01067                                                                      CL**4
01068  0740-CHECK-FOR-PRINT.                                               CL**1
01069      MOVE SPACE TO DO-PRINT.                                         CL**1
01070                                                                      CL**4
01071      IF TBL-ISSUE-CNT (Y, Z) NOT = +0                                CL**1
01072          MOVE 'Y'                TO DO-PRINT                         CL**1
01073          MOVE +11                TO Z                                CL**1
01074          GO TO 0750-CHECK-PRINT-EXIT.                                CL**1
01075                                                                      CL**1
01076      IF TBL-ISSUE-AMT (Y, Z) NOT = +0                                CL**1
01077          MOVE 'Y'                TO DO-PRINT                         CL**1
01078          MOVE +11                TO Z                                CL**1
01079          GO TO 0750-CHECK-PRINT-EXIT.                                CL**1
01080                                                                      CL**1
01081      IF TBL-ISSUE-PREM (Y, Z) NOT = +0                               CL**1
01082          MOVE 'Y'                TO DO-PRINT                         CL**1
01083          MOVE +11                TO Z                                CL**1
01084          GO TO 0750-CHECK-PRINT-EXIT.                                CL**1
01085                                                                      CL**1
01086      IF TBL-CANCEL-CNT (Y, Z) NOT = +0                               CL**1
01087          MOVE 'Y'                TO DO-PRINT                         CL**1
01088          MOVE +11                TO Z                                CL**1
01089          GO TO 0750-CHECK-PRINT-EXIT.                                CL**1
01090                                                                      CL**1
01091      IF TBL-CANCEL-AMT (Y, Z) NOT = +0                               CL**1
01092          MOVE 'Y'                TO DO-PRINT                         CL**1
01093          MOVE +11                TO Z                                CL**1
01094          GO TO 0750-CHECK-PRINT-EXIT.                                CL**1
01095                                                                      CL**1
01096      IF TBL-CANCEL-PREM (Y, Z) NOT = +0                              CL**1
01097          MOVE 'Y'                TO DO-PRINT                         CL**1
01098          MOVE +11                TO Z                                CL**1
01099          GO TO 0750-CHECK-PRINT-EXIT.                                CL**1
01100                                                                      CL**1
01101      IF TBL-CLAIM-CNT (Y, Z) NOT = +0                                CL**1
01102          MOVE 'Y'                TO DO-PRINT                         CL**1
01103          MOVE +11                TO Z                                CL**1
01104          GO TO 0750-CHECK-PRINT-EXIT.                                CL**1
01105                                                                      CL**1
01106      IF TBL-CLAIM-AMT (Y, Z) NOT = +0                                CL**1
01107          MOVE 'Y'                TO DO-PRINT                         CL**1
01108          MOVE +11                TO Z                                CL**1
01109          GO TO 0750-CHECK-PRINT-EXIT.                                CL**1
01110                                                                      CL**1
01111      IF TBL-EARN-PREM (Y, Z) NOT = +0                                CL**1
01112          MOVE 'Y'                TO DO-PRINT                         CL**1
01113          MOVE +11                TO Z                                CL**1
01114          GO TO 0750-CHECK-PRINT-EXIT.                                CL**1
01115                                                                      CL**1
01116  0750-CHECK-PRINT-EXIT.                                              CL**1
01117      EXIT.                                                           CL**1
01118  EJECT                                                               CL**1
01119                                                                      CL**4
01120  0770-DUMP-PAGE-1.                                                   CL**1
01121 *                                                                    CL**1
01122 *    THIS PRINT STARTS WITH LINE 8 THRU END OF PAGE 1.               CL**1
01123 *                                                                    CL**1
01124      IF R-SW-G (Y) = SPACE                                           CL**1
01125          GO TO 0790-DUMP-1-EXIT.                                     CL**1
01126                                                                      CL**1
01127      MOVE HD-7                   TO PRT.                             CL**1
01128      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01129                                                                      CL**4
01130      MOVE P1-MESS (Y)            TO HD-9-MSG.                        CL**1
01131      MOVE HD-9                   TO PRT.                             CL**1
01132      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01133                                                                      CL**4
01134      IF Y GREATER +1                                                 CL**4
01135          GO TO 0780-CONT-DUMP-1.                                     CL**4
01136                                                                      CL**4
01137      MOVE HD-10                  TO PRT.                             CL**1
01138      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01139                                                                      CL**4
01140      MOVE HD-11                  TO PRT.                             CL**1
01141      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01142                                                                      CL**4
01143      MOVE HD-12                  TO PRT.                             CL**1
01144      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01145                                                                      CL**1
01146  0780-CONT-DUMP-1.                                                   CL**1
01147      MOVE 'LIFE REDUCING  '      TO D1-MSG.                          CL**1
01148      MOVE +1                     TO Z.                               CL**1
01149      PERFORM 0440-BUILD-DETAIL-1 THRU 0450-BUILD-D-EXIT.             CL**1
01150                                                                      CL**4
01151      MOVE DETAIL-1               TO PRT.                             CL**1
01152      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01153                                                                      CL**4
01154      MOVE 'LIFE LEVEL     '      TO D1-MSG.                          CL**1
01155      MOVE +2                     TO Z.                               CL**1
01156      PERFORM 0440-BUILD-DETAIL-1 THRU 0450-BUILD-D-EXIT.             CL**1
01157      MOVE DETAIL-1               TO PRT.                             CL**1
01158      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01159                                                                      CL**4
01160      MOVE 'LIFE OUT BAL   '      TO D1-MSG.                          CL**1
01161      MOVE +3                     TO Z.                               CL**1
01162      PERFORM 0440-BUILD-DETAIL-1 THRU 0450-BUILD-D-EXIT.             CL**1
01163      MOVE DETAIL-1               TO PRT.                             CL**1
01164      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01165                                                                      CL**4
01166      MOVE ZERO-FIELDS-1          TO TOTAL-PRINT-AMOUNTS              CL**4
01167                                     TOTAL-AMOUNTS.                   CL**4
01168      PERFORM 0410-ADD-SUB-TOTALS THRU 0420-ADD-SUB-EXIT              CL**1
01169          VARYING Z FROM 1 BY 1 UNTIL Z GREATER 3.                    CL**1
01170      PERFORM 0690-MOVE-G1-TO-PRINT THRU 0700-MOVE-G1-EXIT.           CL**1
01171      MOVE '  TOTAL LIFE   '      TO D1-MSG.                          CL**1
01172      MOVE DETAIL-1               TO PRT.                             CL**1
01173      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01174                                                                      CL**4
01175      MOVE 'A-H RETRO/ELIM '      TO D1-MSG.                          CL**1
01176      MOVE +4                     TO Z.                               CL**1
01177      PERFORM 0440-BUILD-DETAIL-1 THRU 0450-BUILD-D-EXIT.             CL**1
01178      MOVE DETAIL-1               TO PRT.                             CL**1
01179      MOVE '0'                    TO P-CTL.                           CL**1
01180      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01181                                                                      CL**4
01182      MOVE 'A-H OUT BAL    '      TO D1-MSG.                          CL**1
01183      MOVE +5                     TO Z.                               CL**1
01184      PERFORM 0440-BUILD-DETAIL-1 THRU 0450-BUILD-D-EXIT.             CL**1
01185      MOVE DETAIL-1               TO PRT.                             CL**1
01186      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01187                                                                      CL**4
01188      MOVE ZERO-FIELDS-1          TO TOTAL-PRINT-AMOUNTS.             CL**1
01189      PERFORM 0410-ADD-SUB-TOTALS THRU 0420-ADD-SUB-EXIT              CL**1
01190              VARYING Z FROM 4 BY 1 UNTIL Z GREATER 5.                CL**1
01191      PERFORM 0690-MOVE-G1-TO-PRINT THRU 0700-MOVE-G1-EXIT.           CL**1
01192      MOVE '  TOTAL A-H    '      TO D1-MSG.                          CL**1
01193      MOVE DETAIL-1               TO PRT.                             CL**1
01194      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01195                                                                      CL**4
01196      PERFORM 0710-MOVE-T1-TO-PRINT THRU 0720-MOVE-T1-EXIT.           CL**1
01197      MOVE '  TOTAL INS    '      TO D1-MSG.                          CL**1
01198      MOVE DETAIL-1               TO PRT.                             CL**1
01199      MOVE '0'                    TO P-CTL.                           CL**1
01200      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01201                                                                      CL**4
01202  0790-DUMP-1-EXIT.                                                   CL**1
01203      EXIT.                                                           CL**1
01204  EJECT                                                               CL**1
01205  0810-DUMP-PAGE-2.                                                   CL**1
01206 *                                                                    CL**1
01207 *    THIS PRINT STARTS WITH LINE 8 OF PAGE 2 THRU END OF PAGE 2.     CL**1
01208 *                                                                    CL**1
01209      IF R-SW-I (Y) = SPACE                                           CL**1
01210          GO TO 0830-DUMP-2-FINAL.                                    CL**1
01211                                                                      CL**1
01212      MOVE HD-8                   TO PRT.                             CL**1
01213      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01214                                                                      CL**4
01215      MOVE P1-MESS (Y)            TO HD-9-MSG.                        CL**1
01216      MOVE HD-9                   TO PRT.                             CL**1
01217      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01218                                                                      CL**4
01219      IF Y GREATER +1                                                 CL**4
01220          GO TO 0820-CONT-DUMP-2.                                     CL**4
01221                                                                      CL**4
01222      MOVE HD-10                  TO PRT.                             CL**1
01223      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01224                                                                      CL**4
01225      MOVE HD-11                  TO PRT.                             CL**1
01226      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01227                                                                      CL**4
01228      MOVE HD-12                  TO PRT.                             CL**1
01229      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01230                                                                      CL**1
01231  0820-CONT-DUMP-2.                                                   CL**1
01232                                                                      CL**4
01233      MOVE 'LIFE REDUCING  '      TO D1-MSG.                          CL**1
01234      MOVE +6                     TO Z.                               CL**1
01235      PERFORM 0440-BUILD-DETAIL-1 THRU 0450-BUILD-D-EXIT.             CL**1
01236      MOVE DETAIL-1               TO PRT.                             CL**1
01237      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01238                                                                      CL**4
01239      MOVE 'LIFE LEVEL     '      TO D1-MSG.                          CL**1
01240      MOVE +7                     TO Z.                               CL**1
01241      PERFORM 0440-BUILD-DETAIL-1 THRU 0450-BUILD-D-EXIT.             CL**1
01242      MOVE DETAIL-1               TO PRT.                             CL**1
01243      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01244                                                                      CL**4
01245      MOVE 'LIFE OUT BAL   '      TO D1-MSG.                          CL**1
01246      MOVE +8                     TO Z.                               CL**1
01247      PERFORM 0440-BUILD-DETAIL-1 THRU 0450-BUILD-D-EXIT.             CL**1
01248      MOVE DETAIL-1               TO PRT.                             CL**1
01249      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01250                                                                      CL**4
01251      MOVE ZERO-FIELDS-1          TO TOTAL-PRINT-AMOUNTS              CL**4
01252                                     TOTAL-AMOUNTS.                   CL**4
01253      PERFORM 0410-ADD-SUB-TOTALS THRU 0420-ADD-SUB-EXIT              CL**1
01254          VARYING Z FROM 6 BY 1 UNTIL Z GREATER 8.                    CL**1
01255      PERFORM 0690-MOVE-G1-TO-PRINT THRU 0700-MOVE-G1-EXIT.           CL**1
01256      MOVE '  TOTAL LIFE   '      TO D1-MSG.                          CL**1
01257      MOVE DETAIL-1               TO PRT.                             CL**1
01258      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01259                                                                      CL**4
01260      MOVE 'A-H RETRO/ELIM '      TO D1-MSG.                          CL**1
01261      MOVE +9                     TO Z.                               CL**1
01262      PERFORM 0440-BUILD-DETAIL-1 THRU 0450-BUILD-D-EXIT.             CL**1
01263      MOVE DETAIL-1               TO PRT.                             CL**1
01264      MOVE '0'                    TO P-CTL.                           CL**1
01265      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01266                                                                      CL**4
01267      MOVE 'A-H OUT BAL    '      TO D1-MSG.                          CL**1
01268      MOVE +10                    TO Z.                               CL**1
01269      PERFORM 0440-BUILD-DETAIL-1 THRU 0450-BUILD-D-EXIT.             CL**1
01270      MOVE DETAIL-1               TO PRT.                             CL**1
01271      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01272                                                                      CL**4
01273      MOVE ZERO-FIELDS-1          TO TOTAL-PRINT-AMOUNTS.             CL**1
01274      PERFORM 0410-ADD-SUB-TOTALS THRU 0420-ADD-SUB-EXIT              CL**1
01275          VARYING Z FROM 9 BY 1 UNTIL Z GREATER 10.                   CL**1
01276      PERFORM 0690-MOVE-G1-TO-PRINT THRU 0700-MOVE-G1-EXIT.           CL**1
01277      MOVE '  TOTAL A-H    '      TO D1-MSG.                          CL**1
01278      MOVE DETAIL-1               TO PRT.                             CL**1
01279      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01280                                                                      CL**4
01281      PERFORM 0710-MOVE-T1-TO-PRINT THRU 0720-MOVE-T1-EXIT.           CL**1
01282      MOVE '  TOTAL INS    '      TO D1-MSG.                          CL**1
01283      MOVE DETAIL-1               TO PRT.                             CL**1
01284      MOVE '0'                    TO P-CTL.                           CL**1
01285      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01286                                                                      CL**1
01287  0830-DUMP-2-FINAL.                                                  CL**1
01288                                                                      CL**4
01289      IF Y LESS +3                                                    CL**4
01290          GO TO 0840-DUMP-2-EXIT.                                     CL**4
01291                                                                      CL**4
01292      MOVE HD-15                  TO PRT.                             CL**1
01293      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01294                                                                      CL**4
01295      MOVE 'INCURRED IN THIS PERIOD -  ' TO D3-MSG.                   CL**1
01296      MOVE TOT-AH-PD-THIS         TO D3-AH-AMT.                       CL**1
01297      MOVE DETAIL-3               TO PRT.                             CL**1
01298      MOVE '0'                    TO P-CTL.                           CL**1
01299      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01300                                                                      CL**4
01301      MOVE 'INCURRED IN PRIOR PERIOD - ' TO D3-MSG.                   CL**1
01302      MOVE TOT-AH-PD-LAST         TO D3-AH-AMT.                       CL**1
01303      MOVE DETAIL-3               TO PRT.                             CL**1
01304      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01305                                                                      CL**4
01306  0840-DUMP-2-EXIT.                                                   CL**1
01307      EXIT.                                                           CL**1
01308  EJECT                                                               CL**1
01309  0860-DUMP-PAGE-3.                                                   CL**1
01310 *                                                                    CL**1
01311 *    THIS PRINT,  LIKE THE OTHERS,  STARTS WITH LINE 8.              CL**1
01312 *                                                                    CL**1
01313      MOVE HD-13                  TO PRT.                             CL**1
01314      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01315                                                                      CL**4
01316      MOVE HD-14                  TO PRT.                             CL**1
01317      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01318                                                                      CL**4
01319      MOVE 'IN FORCE AT END OF PREVIOUS PERIOD' TO D2-MSG.            CL**1
01320      MOVE +1                     TO Z.                               CL**1
01321      PERFORM 0460-BUILD-DETAIL-2 THRU 0470-BUILD-2-EXIT.             CL**1
01322      MOVE DETAIL-2               TO PRT.                             CL**1
01323      MOVE '0'                    TO P-CTL.                           CL**1
01324      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01325                                                                      CL**4
01326      MOVE 'ISSUED DURING THIS PERIOD         ' TO D2-MSG.            CL**1
01327      MOVE +2                     TO Z.                               CL**1
01328      PERFORM 0460-BUILD-DETAIL-2 THRU 0470-BUILD-2-EXIT.             CL**1
01329      MOVE DETAIL-2               TO PRT.                             CL**1
01330      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01331                                                                      CL**4
01332      MOVE '         SUB-TOTAL IN FORCE       ' TO D2-MSG.            CL**1
01333      MOVE +3                     TO Z.                               CL**1
01334      PERFORM 0460-BUILD-DETAIL-2 THRU 0470-BUILD-2-EXIT.             CL**1
01335      MOVE DETAIL-2               TO PRT.                             CL**1
01336      MOVE '0'                    TO P-CTL.                           CL**1
01337      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01338                                                                      CL**4
01339      MOVE 'DEATH CLAIMS PAID THIS PERIOD     ' TO D2-MSG.            CL**1
01340      MOVE +4                     TO Z.                               CL**1
01341      PERFORM 0460-BUILD-DETAIL-2 THRU 0470-BUILD-2-EXIT.             CL**1
01342      MOVE DETAIL-2               TO PRT.                             CL**1
01343      MOVE '0'                    TO P-CTL.                           CL**1
01344      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01345                                                                      CL**4
01346      MOVE 'EXPIRATIONS IN THIS PERIOD        ' TO D2-MSG.            CL**1
01347      MOVE +5                     TO Z.                               CL**1
01348      PERFORM 0460-BUILD-DETAIL-2 THRU 0470-BUILD-2-EXIT.             CL**1
01349      MOVE DETAIL-2               TO PRT.                             CL**1
01350      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01351                                                                      CL**4
01352      MOVE 'CANCELLATIONS IN THIS PERIOD      ' TO D2-MSG.            CL**1
01353      MOVE +6                     TO Z.                               CL**1
01354      PERFORM 0460-BUILD-DETAIL-2 THRU 0470-BUILD-2-EXIT.             CL**1
01355      MOVE DETAIL-2               TO PRT.                             CL**1
01356      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01357                                                                      CL**4
01358      MOVE 'OTHER DECREASES                   ' TO D2-MSG.            CL**1
01359      MOVE +7                     TO Z.                               CL**1
01360      PERFORM 0460-BUILD-DETAIL-2 THRU 0470-BUILD-2-EXIT.             CL**1
01361      MOVE DETAIL-2               TO PRT.                             CL**1
01362      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01363                                                                      CL**4
01364      MOVE '         SUB-TOTAL OF DEDUCTIONS  ' TO D2-MSG.            CL**1
01365      MOVE +8                     TO Z.                               CL**1
01366      PERFORM 0460-BUILD-DETAIL-2 THRU 0470-BUILD-2-EXIT.             CL**1
01367      MOVE DETAIL-2               TO PRT.                             CL**1
01368      MOVE '0'                    TO P-CTL.                           CL**1
01369      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01370                                                                      CL**4
01371      MOVE 'IN FORCE AT END OF CURRENT PERIOD ' TO D2-MSG.            CL**1
01372      MOVE +9                     TO Z.                               CL**1
01373      PERFORM 0460-BUILD-DETAIL-2 THRU 0470-BUILD-2-EXIT.             CL**1
01374      MOVE DETAIL-2               TO PRT.                             CL**1
01375      MOVE '0'                    TO P-CTL.                           CL**1
01376      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01377                                                                      CL**4
01378      MOVE ZERO TO D2-GRP-CNT  D2-GRP-AMT  D2-IND-CNT  D2-IND-AMT.    CL**1
01379      MOVE ALL '*'                TO D2-MSG.                          CL**1
01380      MOVE DETAIL-2               TO PRT.                             CL**1
01381      MOVE '0'                    TO P-CTL.                           CL**1
01382      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01383                                                                      CL**4
01384      MOVE 'IN FORCE AT END OF PREVIOUS PERIOD' TO D2-MSG.            CL**1
01385      MOVE DETAIL-2               TO PRT.                             CL**1
01386      MOVE '0'                    TO P-CTL.                           CL**1
01387      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01388                                                                      CL**4
01389      MOVE '    TERMS OF 1 THRU 60 MONTHS     ' TO D2-MSG.            CL**1
01390      MOVE +1                     TO Y.                               CL**1
01391      MOVE +1                     TO Z.                               CL**1
01392      PERFORM 0480-BUILD-DETAIL-2A THRU 0490-BUILD-2A-EXIT.           CL**1
01393      MOVE DETAIL-2               TO PRT.                             CL**1
01394      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01395                                                                      CL**4
01396      MOVE '    TERMS OF 61 THRU 120 MONTHS   ' TO D2-MSG.            CL**1
01397      MOVE +2                     TO Y.                               CL**1
01398      PERFORM 0480-BUILD-DETAIL-2A THRU 0490-BUILD-2A-EXIT.           CL**1
01399      MOVE DETAIL-2               TO PRT.                             CL**1
01400      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01401                                                                      CL**4
01402      MOVE '    TERMS GREATER THAN 120 MONTHS ' TO D2-MSG.            CL**1
01403      MOVE +3                     TO Y.                               CL**1
01404      PERFORM 0480-BUILD-DETAIL-2A THRU 0490-BUILD-2A-EXIT.           CL**1
01405      MOVE DETAIL-2               TO PRT.                             CL**1
01406      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01407                                                                      CL**4
01408      MOVE ZERO TO D2-GRP-CNT  D2-GRP-AMT  D2-IND-CNT  D2-IND-AMT.    CL**1
01409      MOVE 'IN FORCE AT END OF CURRENT PERIOD ' TO D2-MSG.            CL**1
01410      MOVE DETAIL-2               TO PRT.                             CL**1
01411      MOVE '0'                    TO P-CTL.                           CL**1
01412      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01413                                                                      CL**4
01414      MOVE '    TERMS OF 1 THRU 60 MONTHS     ' TO D2-MSG.            CL**1
01415      MOVE +1                     TO Y.                               CL**1
01416      MOVE +9                     TO Z.                               CL**1
01417      PERFORM 0480-BUILD-DETAIL-2A THRU 0490-BUILD-2A-EXIT.           CL**1
01418      MOVE DETAIL-2               TO PRT.                             CL**1
01419      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01420                                                                      CL**4
01421      MOVE '    TERMS OF 61 THRU 120 MONTHS   ' TO D2-MSG.            CL**1
01422      MOVE +2                     TO Y.                               CL**1
01423      PERFORM 0480-BUILD-DETAIL-2A THRU 0490-BUILD-2A-EXIT.           CL**1
01424      MOVE DETAIL-2               TO PRT.                             CL**1
01425      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01426                                                                      CL**4
01427      MOVE '    TERMS GREATER THAN 120 MONTHS ' TO D2-MSG.            CL**1
01428      MOVE +3                     TO Y.                               CL**1
01429      PERFORM 0480-BUILD-DETAIL-2A THRU 0490-BUILD-2A-EXIT.           CL**1
01430      MOVE DETAIL-2               TO PRT.                             CL**1
01431      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01432                                                                      CL**4
01433  0870-DUMP-3-EXIT.                                                   CL**1
01434      EXIT.                                                           CL**1
01435  EJECT                                                               CL**1
01436                                                                      CL**4
01437  0890-NO-PRINT.                                                      CL**1
01438      MOVE RUN-DATE               TO WS-WORK-DATE.                    CL**1
01439      MOVE WS-DAY                 TO HD-5-DAY.                        CL**1
01440      MOVE WS-YEAR                TO HD-5-YEAR.                       CL**1
01441      MOVE MONTH-ALPHA (WS-MONTH) TO HD-5-MONTH.                      CL**1
01442                                                                      CL**1
01443      MOVE EP-DT                  TO WS-WORK-DATE.                    CL**1
01444      MOVE WS-DAY                 TO HD-6-DAY.                        CL**1
01445      MOVE WS-YEAR                TO HD-6-YEAR.                       CL**1
01446      MOVE MONTH-ALPHA (WS-MONTH) TO HD-6-MONTH.                      CL**1
01447                                                                      CL**1
01448      PERFORM 0640-PRINT-HEADINGS THRU 0650-P-H-X.                    CL**1
01449      MOVE 'PROGRAM COMPLETED - NO PRINT DATA' TO P-DATA.             CL**1
01450      PERFORM 0660-PRT-RTN THRU 0670-PRT-RTN-X.                       CL**1
01451                                                                      CL**1
01452  0900-END-JOB.                                                       CL**1
01453      CLOSE  PRT-FILE.                                                CL**1
01454      IF FICH-OPEN = 'X'                                              CL**1
01455          CLOSE FICH.                                                 CL**1
01456  0910-EOJ-EXIT.                                                      CL**4
01457       EXIT.                                                          CL**4
01458  EJECT                                                               CL**1
01459                                                                      CL**1
01460  ABEND-PGM.                      COPY ELCABEND.                      CL**1
01461 /                                                                    CL**1
01462  LCP-WRITE-POS-PRT SECTION.                                          CL**1
01463      IF LCP-ASA = '+'                                                CL**1
01464          WRITE PRT AFTER 0 LINE                                      CL**1
01465      ELSE                                                            CL**1
01466      IF LCP-ASA = ' '                                                CL**1
01467          WRITE PRT AFTER ADVANCING 1 LINE                            CL**1
01468      ELSE                                                            CL**1
01469      IF LCP-ASA = '0'                                                CL**1
01470          WRITE PRT AFTER ADVANCING 2 LINE                            CL**1
01471      ELSE                                                            CL**1
01472      IF LCP-ASA = '-'                                                CL**1
01473          WRITE PRT AFTER ADVANCING 3 LINE                            CL**1
01474      ELSE                                                            CL**1
01475      IF LCP-ASA = '1'                                                CL**1
01476          WRITE PRT AFTER ADVANCING PAGE                              CL**1
01477      ELSE                                                            CL**1
01478      IF LCP-ASA = '2'                                                CL**1
01479          WRITE PRT AFTER ADVANCING LCP-CH2                           CL**1
01480      ELSE                                                            CL**1
01481      IF LCP-ASA = '3'                                                CL**1
01482          WRITE PRT AFTER ADVANCING LCP-CH3                           CL**1
01483      ELSE                                                            CL**1
01484      IF LCP-ASA = '4'                                                CL**1
01485          WRITE PRT AFTER ADVANCING LCP-CH4                           CL**1
01486      ELSE                                                            CL**1
01487      IF LCP-ASA = '5'                                                CL**1
01488          WRITE PRT AFTER ADVANCING LCP-CH5                           CL**1
01489      ELSE                                                            CL**1
01490      IF LCP-ASA = '6'                                                CL**1
01491          WRITE PRT AFTER ADVANCING LCP-CH6                           CL**1
01492      ELSE                                                            CL**1
01493      IF LCP-ASA = '7'                                                CL**1
01494          WRITE PRT AFTER ADVANCING LCP-CH7                           CL**1
01495      ELSE                                                            CL**1
01496      IF LCP-ASA = '8'                                                CL**1
01497          WRITE PRT AFTER ADVANCING LCP-CH8                           CL**1
01498      ELSE                                                            CL**1
01499      IF LCP-ASA = '9'                                                CL**1
01500          WRITE PRT AFTER ADVANCING LCP-CH9                           CL**1
01501      ELSE                                                            CL**1
01502      IF LCP-ASA = 'A'                                                CL**1
01503          WRITE PRT AFTER ADVANCING LCP-CH10                          CL**1
01504      ELSE                                                            CL**1
01505      IF LCP-ASA = 'B'                                                CL**1
01506          WRITE PRT AFTER ADVANCING LCP-CH11                          CL**1
01507      ELSE                                                            CL**1
01508      IF LCP-ASA = 'C'                                                CL**1
01509          WRITE PRT AFTER ADVANCING LCP-CH12                          CL**1
01510      ELSE                                                            CL**1
01511      IF LCP-ASA = 'V'                                                CL**1
01512          WRITE PRT AFTER ADVANCING LCP-P01                           CL**1
01513      ELSE                                                            CL**1
01514      IF LCP-ASA = 'W'                                                CL**1
01515          WRITE PRT AFTER ADVANCING LCP-P02                           CL**1
01516      ELSE                                                            CL**1
01517      DISPLAY 'ASA CODE ERROR'.                                       CL**1
01518  LCP-WRITE-END-PRT.                                                  CL**1
01519      EXIT.                                                           CL**1
01520                                                                      CL**1
01521                                                                      CL**1
