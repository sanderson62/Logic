00001  IDENTIFICATION DIVISION.                                         03/28/98
00002                                                                   EL315
00003  PROGRAM-ID.                 EL315 .                                 LV003
00004 *              PROGRAM CONVERTED BY                               EL315
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL315
00006 *              CONVERSION DATE 02/13/96 09:47:51.                 EL315
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL315
00008 *                            VMOD=2.014                           EL315
00009                                                                   EL315
00009                                                                   EL315
00010 *AUTHOR.     LOGIC,INC.                                           EL315
00011 *            DALLAS, TEXAS.                                       EL315
00012                                                                   EL315
00013 *DATE-COMPILED.                                                   EL315
00014                                                                   EL315
00015 *SECURITY.   *****************************************************EL315
00016 *            *                                                   *EL315
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL315
00018 *            *                                                   *EL315
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL315
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL315
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL315
00022 *            *                                                   *EL315
00023 *            *****************************************************EL315
00024 *                                                                 EL315
00025 *REMARKS.                                                         EL315
00026 *        THIS REPORT PROGRAM PROVIDES A DETAILED AND SUMMARY      EL315
00027 *    HISTORY OF THE CURRENT LOSS RESERVES COMPUTED ON ALL OPEN    EL315
00028 *    CLAIMS.  CLOSED CLAIMS DO NOT PARTICIPATE IN ANY OF THE      EL315
00029 *    RESERVE CALCULATIONS.                                        EL315
00030                                                                   EL315
00031      EJECT                                                        EL315
00032  ENVIRONMENT DIVISION.                                            EL315
00033  CONFIGURATION SECTION.                                           EL315
00034                                                                   EL315
00035  INPUT-OUTPUT SECTION.                                            EL315
00036                                                                   EL315
00037  FILE-CONTROL.                                                    EL315
00038                                                                   EL315
00039      SELECT REPORTS-EXTRACT-FILE                                  EL315
00040          ASSIGN TO SYS010-UT-2400-S-SYS010.                       EL315
00041                                                                   EL315
00042      SELECT SORT-WORK-FILE                                        EL315
00043          ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.                      EL315
00044                                                                   EL315
00045      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   EL315
00046                                                                   EL315
00047      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   EL315
00048                                                                   EL315
00049      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   EL315
00050                                                                   EL315
00051      SELECT ELREPT           ASSIGN TO SYS018-FBA1-ELREPT         EL315
00052                              ORGANIZATION IS INDEXED              EL315
00053                              ACCESS IS DYNAMIC                    EL315
00054                              RECORD KEY IS RF-CONTROL-PRIMARY     EL315
00055                              FILE STATUS IS DTE-VSAM-FLAGS.       EL315
00056                                                                   EL315
00057      SELECT ERMEBL                                                EL315
00058              ASSIGN SYS024-FBA1-ERMEBL                            EL315
00059              ORGANIZATION INDEXED                                 EL315
00060              ACCESS DYNAMIC                                       EL315
00061              RECORD KEY ME-CONTROL-PRIMARY                        EL315
00062              FILE STATUS ERMEBL-FILE-STATUS.                      EL315
00063      EJECT                                                        EL315
00064  DATA DIVISION.                                                   EL315
00065                                                                   EL315
00066  FILE SECTION.                                                    EL315
00067                                                                   EL315
00068  FD  REPORTS-EXTRACT-FILE                                         EL315
00069      COPY ELCEXTFD.                                               EL315
00070  01  FILLER                      PIC X(314).                      EL315
00071      EJECT                                                        EL315
00072                                                                      CL**2
00073  SD  SORT-WORK-FILE.                                                 CL**2
00074      COPY ELCEXTR.                                                EL315
00075      EJECT                                                        EL315
00076  FD  DISK-DATE               COPY ELCDTEFD SUPPRESS.              EL315
00077                                                                   EL315
00078  FD  PRNTR                   COPY ELCPRTFD SUPPRESS.              EL315
00079                                                                   EL315
00080  FD  FICH                    COPY ELCFCHFD SUPPRESS.              EL315
00081                                                                   EL315
00082  FD  ELREPT                  COPY ELCRPTFD SUPPRESS.              EL315
00083      COPY ELCREPT SUPPRESS.                                       EL315
00084                                                                   EL315
00085  FD  ERMEBL.                                                         CL**2
00086      COPY ERCMEBL.                                                EL315
00087                                                                   EL315
00088      EJECT                                                        EL315
00089  WORKING-STORAGE SECTION.                                         EL315
00090                                                                   EL315
00091  77  FILLER PIC X(32) VALUE '********************************'.   EL315
00092  77  FILLER PIC X(32) VALUE '*     EL315 WORKING-STORAGE    *'.   EL315
00093  77  FILLER PIC X(32) VALUE '***********VMOD=2.014 **********'.   EL315
00094                                                                   EL315
00095  01  MONTH-END-DATA.                                              EL315
00096      12  ME-START-DATE.                                           EL315
00097          16  ME-START-MO         PIC 99.                          EL315
00098          16  FILLER              PIC X.                           EL315
00099          16  ME-START-DA         PIC 99.                          EL315
00100          16  FILLER              PIC X.                           EL315
00101          16  ME-START-YR         PIC 99.                          EL315
00102      12  ME-CNDS-DATE            PIC 9(6).                        EL315
00103      12  ME-CNDS-DATE-R REDEFINES ME-CNDS-DATE.                   EL315
00104          16  ME-CNDS-MO          PIC 99.                          EL315
00105          16  ME-CNDS-DA          PIC 99.                          EL315
00106          16  ME-CNDS-YR          PIC 99.                          EL315
00107      12  ME-START-TIME           PIC 9(6).                        EL315
00108      12  ME-UPDATE-FLAG          PIC X VALUE 'Y'.                 EL315
00109          88  ME-DO-UPDATE        VALUE 'Y'.                       EL315
00110          88  ME-NO-UPDATE        VALUE 'N'.                       EL315
00111      12  ERMEBL-FILE-STATUS      PIC XX.                          EL315
00112      12  MONTH-END-MOYR          PIC S9(5) COMP-3.                EL315
00113                                                                   EL315
00114  01  FILLER                          COMP-3.                      EL315
00115      05  WS-LINE-COUNT               PIC S9(3)       VALUE +99.   EL315
00116      05  WS-LINE-COUNT-MAX           PIC S9(3)       VALUE +56.   EL315
00117      05  WS-LINE-COUNT-MAX-2         PIC S9(3)       VALUE +54.   EL315
00118      05  WS-PAGE                     PIC S9(5)       VALUE ZERO.  EL315
00119      05  WS-REPORT-SW                PIC S9          VALUE ZERO.  EL315
00120      05  WS-PRINT-SW                 PIC S9          VALUE ZERO.  EL315
00121      05  WS-RECORD-COUNT             PIC S9(9)       VALUE ZERO.  EL315
00122      05  WS-RELEASED-COUNT           PIC S9(9)       VALUE ZERO.  EL315
00123      05  WS-RETURN-CODE              PIC S9(3)       VALUE ZERO.  EL315
00124      05  WS-ZERO                     PIC S9          VALUE ZERO.  EL315
00125                                                                   EL315
00126      05  WS-INCURRED-AGE             PIC S9(3)       VALUE ZERO.  EL315
00127      05  WS-YEAR                     REDEFINES                    EL315
00128          WS-INCURRED-AGE             PIC S9(3).                   EL315
00129                                                                   EL315
00130      EJECT                                                        EL315
00131      05  WS-ACCOUNT-TOTALS.                                       EL315
00132          10  WS-AT-NUMBER-OF-CLAIMS  PIC S9(5)        VALUE ZERO. EL315
00133          10  WS-AT-ORIG-TERM         PIC S9(5)        VALUE ZERO. EL315
00134          10  WS-AT-REMAINING-TERM    PIC S9(5)        VALUE ZERO. EL315
00135          10  WS-AT-ISSUE-AGE         PIC S9(5)        VALUE ZERO. EL315
00136          10  WS-AT-INCURRED-AGE      PIC S9(5)        VALUE ZERO. EL315
00137          10  WS-AT-INCURRED-AGE-CNT  PIC S9(3)        VALUE ZERO. EL315
00138          10  WS-AT-ORIG-BENEFIT      PIC S9(7)V99 VALUE ZERO.     EL315
00139          10  WS-AT-REM-BENEFIT       PIC S9(7)V99 VALUE ZERO.     EL315
00140          10  WS-AT-MANUAL-RESERVE    PIC S9(7)V99 VALUE ZERO.     EL315
00141          10  WS-AT-FUTURE-RESERVE    PIC S9(7)V99 VALUE ZERO.     EL315
00142          10  WS-AT-PTC-RESERVE       PIC S9(7)V99 VALUE ZERO.     EL315
00143          10  WS-AT-IBNR-RESERVE      PIC S9(7)V99 VALUE ZERO.     EL315
00144          10  WS-AT-PTC-RESERVE-AH    PIC S9(7)V99 VALUE ZERO.     EL315
00145          10  WS-AT-PTC-RESERVE-LF    PIC S9(7)V99 VALUE ZERO.     EL315
00146                                                                   EL315
00147      05  WS-STATE-TOTALS.                                         EL315
00148          10  WS-ST-NUMBER-OF-CLAIMS  PIC S9(5)        VALUE ZERO. EL315
00149          10  WS-ST-ORIG-TERM         PIC S9(5)        VALUE ZERO. EL315
00150          10  WS-ST-REMAINING-TERM    PIC S9(5)        VALUE ZERO. EL315
00151          10  WS-ST-ISSUE-AGE         PIC S9(5)        VALUE ZERO. EL315
00152          10  WS-ST-INCURRED-AGE      PIC S9(5)        VALUE ZERO. EL315
00153          10  WS-ST-INCURRED-AGE-CNT  PIC S9(3)        VALUE ZERO. EL315
00154          10  WS-ST-ORIG-BENEFIT      PIC S9(8)V99 VALUE ZERO.     EL315
00155          10  WS-ST-REM-BENEFIT       PIC S9(8)V99 VALUE ZERO.     EL315
00156          10  WS-ST-MANUAL-RESERVE    PIC S9(8)V99 VALUE ZERO.     EL315
00157          10  WS-ST-FUTURE-RESERVE    PIC S9(8)V99 VALUE ZERO.     EL315
00158          10  WS-ST-PTC-RESERVE       PIC S9(8)V99 VALUE ZERO.     EL315
00159          10  WS-ST-IBNR-RESERVE      PIC S9(8)V99 VALUE ZERO.     EL315
00160          10  WS-ST-PTC-RESERVE-AH    PIC S9(7)V99 VALUE ZERO.     EL315
00161          10  WS-ST-PTC-RESERVE-LF    PIC S9(7)V99 VALUE ZERO.     EL315
00162                                                                   EL315
00163      05  WS-CARRIER-TOTALS.                                       EL315
00164          10  WS-CT-NUMBER-OF-CLAIMS  PIC S9(5)        VALUE ZERO. EL315
00165          10  WS-CT-ORIG-TERM         PIC S9(5)        VALUE ZERO. EL315
00166          10  WS-CT-REMAINING-TERM    PIC S9(5)        VALUE ZERO. EL315
00167          10  WS-CT-ISSUE-AGE         PIC S9(5)        VALUE ZERO. EL315
00168          10  WS-CT-INCURRED-AGE      PIC S9(5)        VALUE ZERO. EL315
00169          10  WS-CT-INCURRED-AGE-CNT  PIC S9(3)        VALUE ZERO. EL315
00170          10  WS-CT-ORIG-BENEFIT      PIC S9(8)V99 VALUE ZERO.     EL315
00171          10  WS-CT-REM-BENEFIT       PIC S9(8)V99 VALUE ZERO.     EL315
00172          10  WS-CT-MANUAL-RESERVE    PIC S9(8)V99 VALUE ZERO.     EL315
00173          10  WS-CT-FUTURE-RESERVE    PIC S9(8)V99 VALUE ZERO.     EL315
00174          10  WS-CT-PTC-RESERVE       PIC S9(8)V99 VALUE ZERO.     EL315
00175          10  WS-CT-IBNR-RESERVE      PIC S9(8)V99 VALUE ZERO.     EL315
00176          10  WS-CT-PTC-RESERVE-AH    PIC S9(8)V99 VALUE ZERO.     EL315
00177          10  WS-CT-PTC-RESERVE-LF    PIC S9(8)V99 VALUE ZERO.     EL315
00178                                                                   EL315
00179      05  WS-GRAND-TOTALS.                                         EL315
00180          10  WS-GT-NUMBER-OF-CLAIMS  PIC S9(5)        VALUE ZERO. EL315
00181          10  WS-GT-ORIG-TERM         PIC S9(5)        VALUE ZERO. EL315
00182          10  WS-GT-REMAINING-TERM    PIC S9(5)        VALUE ZERO. EL315
00183          10  WS-GT-ISSUE-AGE         PIC S9(5)        VALUE ZERO. EL315
00184          10  WS-GT-INCURRED-AGE      PIC S9(5)        VALUE ZERO. EL315
00185          10  WS-GT-INCURRED-AGE-CNT  PIC S9(3)        VALUE ZERO. EL315
00186          10  WS-GT-ORIG-BENEFIT      PIC S9(9)V99 VALUE ZERO.     EL315
00187          10  WS-GT-REM-BENEFIT       PIC S9(9)V99 VALUE ZERO.     EL315
00188          10  WS-GT-MANUAL-RESERVE    PIC S9(9)V99 VALUE ZERO.     EL315
00189          10  WS-GT-FUTURE-RESERVE    PIC S9(9)V99 VALUE ZERO.     EL315
00190          10  WS-GT-PTC-RESERVE       PIC S9(9)V99 VALUE ZERO.     EL315
00191          10  WS-GT-IBNR-RESERVE      PIC S9(9)V99 VALUE ZERO.     EL315
00192          10  WS-GT-PTC-RESERVE-AH    PIC S9(9)V99 VALUE ZERO.     EL315
00193          10  WS-GT-PTC-RESERVE-LF    PIC S9(9)V99 VALUE ZERO.     EL315
00194                                                                   EL315
00195      EJECT                                                        EL315
00196  01  FILLER                          COMP SYNC.                   EL315
00197      05  PGM-SUB                     PIC S9(4)       VALUE +315.  EL315
00198      05  W-TIMES-COUNTER             PIC S9(04)      VALUE ZERO.  EL315
00199      05  WS-INDEX                    PIC S9(4)       VALUE ZERO.  EL315
00200                                                                   EL315
00201  01  FILLER.                                                      EL315
00202      05  W-STATE-FIRST-TIME-IND      PIC X(01) VALUE SPACES.      EL315
00203          88  W-STATE-FIRST-TIME                VALUE SPACES.      EL315
00204          88  W-NOT-STATE-FIRST-TIME            VALUE 'N'.         EL315
00205      05  ABEND-CODE                  PIC X(4).                    EL315
00206      05  ABEND-OPTION                PIC X.                       EL315
00207      05  OLC-REPORT-NAME             PIC X(5) VALUE 'EL315'.      EL315
00208      05  X                           PIC X           VALUE SPACE. EL315
00209                                                                   EL315
00210      05  WS-RPT                  PIC X VALUE ' '.                 EL315
00211          88  WS-RPT-A            VALUE 'A'.                       EL315
00212          88  ME-RPT-B            VALUE ' '.                       EL315
00213                                                                   EL315
00214      05  WS-SAVE-PRINT-RECORD        PIC X(133)      VALUE SPACES.EL315
00215                                                                   EL315
00216      05  WS-LAST-ACCOUNT             PIC X(10)       VALUE SPACES.EL315
00217      05  WS-LAST-STATE               PIC XX          VALUE SPACES.EL315
00218      05  WS-LAST-CARRIER             PIC X           VALUE SPACES.EL315
00219                                                                   EL315
00220      05  WS-ABEND-MESSAGE            PIC X(80)       VALUE SPACES.EL315
00221                                                                   EL315
00222      05  WS-ABEND-FILE-STATUS        PIC XX          VALUE ZERO.  EL315
00223                                                                   EL315
00224      05  WS-FILE-ERROR-MESSAGE.                                   EL315
00225          10  FILLER                  PIC X(24)       VALUE        EL315
00226              'ERROR OCCURED OPENING - '.                          EL315
00227          10  WS-FEM-FILE-NAME        PIC X(8).                    EL315
00228                                                                   EL315
00229      05  WS-CARRIER                  PIC X(11)       VALUE        EL315
00230              ' CARRIER -'.                                        EL315
00231                                                                   EL315
00232      05  WS-DATE-WORK.                                            EL315
00233          10  FILLER                  PIC X(6).                    EL315
00234          10  WS-DW-YEAR              PIC 99          VALUE ZERO.  EL315
00235                                                                   EL315
00236      EJECT                                                        EL315
00237  01  CARRIER-TOTALS-AREA.                                         EL315
00238      05  CARRIER-TOTALS.                                          EL315
00239          10  CT-CARRIER              PIC X.                       EL315
00240                                                                   EL315
00241          10  CT-RESERVES-BY-YEAR     COMP-3                       EL315
00242              OCCURS 13 TIMES         INDEXED BY YEAR-INDEX.       EL315
00243              15  CT-MANUAL-BY-YEAR   PIC S9(8)V99.                EL315
00244              15  CT-FUTURE-BY-YEAR   PIC S9(8)V99.                EL315
00245              15  CT-PTC-BY-YEAR      PIC S9(8)V99.                EL315
00246              15  CT-PTC-BY-YEAR-AH   PIC S9(8)V99.                EL315
00247              15  CT-PTC-BY-YEAR-LF   PIC S9(8)V99.                EL315
00248              15  CT-IBNR-BY-YEAR     PIC S9(8)V99.                EL315
00249                                                                   EL315
00250          10  CT-RESERVES-BY-STATE    OCCURS 60 TIMES              EL315
00251                                      INDEXED BY STATE-INDEX.      EL315
00252                                                                   EL315
00253              15  CT-STATE            PIC XX.                      EL315
00254                                                                   EL315
00255              15  CT-MANUAL-BY-STATE  PIC S9(8)V99 COMP-3.         EL315
00256              15  CT-FUTURE-BY-STATE  PIC S9(8)V99 COMP-3.         EL315
00257              15  CT-PTC-BY-STATE     PIC S9(8)V99 COMP-3.         EL315
00258              15  CT-PTC-BY-STATE-AH  PIC S9(8)V99 COMP-3.         EL315
00259              15  CT-PTC-BY-STATE-LF  PIC S9(8)V99 COMP-3.         EL315
00260              15  CT-IBNR-BY-STATE    PIC S9(8)V99 COMP-3.         EL315
00261                                                                   EL315
00262          10  CT-NUMBER-OF-STATES     PIC S9(4)       VALUE ZERO   EL315
00263                                      COMP                         EL315
00264                                      SYNCHRONIZED.                EL315
00265                                                                   EL315
00266      05  CARRIER-TOTALS-ENTRY                                     EL315
00267          OCCURS 30 TIMES             INDEXED BY CT-INDEX          EL315
00268                                                 CT-INDEX2         EL315
00269                                                 CT-INDEX-MAX.     EL315
00270          10  CTE-CARRIER             PIC X.                       EL315
00271          10  CTE-RESERVES-BY-YEAR    OCCURS 13 TIMES.             EL315
00272                                                                   EL315
00273              15  CTE-MANUAL-BY-YEAR  PIC S9(8)V99                 EL315
00274                                      COMP-3.                      EL315
00275              15  CTE-FUTURE-BY-YEAR  PIC S9(8)V99                 EL315
00276                                      COMP-3.                      EL315
00277              15  CTE-PTC-BY-YEAR     PIC S9(8)V99                 EL315
00278                                      COMP-3.                      EL315
00279              15  CTE-PTC-BY-YEAR-AH  PIC S9(8)V99                 EL315
00280                                      COMP-3.                      EL315
00281              15  CTE-PTC-BY-YEAR-LF  PIC S9(8)V99                 EL315
00282                                      COMP-3.                      EL315
00283              15  CTE-IBNR-BY-YEAR    PIC S9(8)V99                 EL315
00284                                      COMP-3.                      EL315
00285                                                                   EL315
00286          10  CTE-RESERVES-BY-STATE   OCCURS 60 TIMES              EL315
00287                                      INDEXED BY STATE-INDEX2      EL315
00288                                                 STATE-INDEX3      EL315
00289                                                 STATE-INDEX-MAX.  EL315
00290                                                                   EL315
00291              15  CTE-STATE           PIC XX.                      EL315
00292                                                                   EL315
00293              15  CTE-MANUAL-BY-STATE PIC S9(8)V99 COMP-3.         EL315
00294              15  CTE-FUTURE-BY-STATE PIC S9(8)V99 COMP-3.         EL315
00295              15  CTE-PTC-BY-STATE    PIC S9(8)V99 COMP-3.         EL315
00296              15  CTE-PTC-BY-STATE-AH PIC S9(8)V99 COMP-3.         EL315
00297              15  CTE-PTC-BY-STATE-LF PIC S9(8)V99 COMP-3.         EL315
00298              15  CTE-IBNR-BY-STATE   PIC S9(8)V99 COMP-3.         EL315
00299                                                                   EL315
00300          10  CTE-NUMBER-OF-STATES    PIC S9(4)                    EL315
00301                                      COMP SYNC.                   EL315
00302                                                                   EL315
00303      05  CARRIER-TOTALS-ZERO         PIC X(2752).                 EL315
00304                                                                   EL315
00305      EJECT                                                        EL315
00306  01  WS-HEADING1.                                                 EL315
00307      05  FILLER                      PIC X(48)       VALUE '1'.   EL315
00308      05  WS-H1-TITLE                 PIC X(72)       VALUE        EL315
00309          'CREDIT LIFE CLAIM RESERVES'.                            EL315
00310      05  WS-H1-REPORT-NUMBER         PIC X(5) VALUE 'EL315'.      EL315
00311      05  WS-H1-REPORT-TYPE           PIC X           VALUE 'A'.   EL315
00312                                                                   EL315
00313  01  WS-HEADING2.                                                 EL315
00314      05  FILLER                      PIC X(46)       VALUE SPACES.EL315
00315      05  WS-H2-CLIENT-NAME           PIC X(74)       VALUE SPACES.EL315
00316      05  WS-H2-DATE                  PIC X(8)        VALUE SPACES.EL315
00317                                                                   EL315
00318  01  WS-HEADING3.                                                 EL315
00319      05  WS-H3-C                     PIC X(11)       VALUE        EL315
00320          ' CARRIER -'.                                            EL315
00321      05  WS-H3-CARRIER               PIC X           VALUE SPACES.EL315
00322      05  FILLER                      PIC X(41)       VALUE SPACES.EL315
00323      05  WS-H3-DATE                  PIC X(67)       VALUE SPACES.EL315
00324      05  FILLER                      PIC X(5)        VALUE 'PAGE'.EL315
00325      05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL315
00326      05  FILLER                      PIC X(11)       VALUE SPACES.EL315
00327                                                                   EL315
00328  01  WS-HEADING4.                                                 EL315
00329      05  FILLER                      PIC X(11)       VALUE SPACES.EL315
00330      05  WS-H4-CARRIER-NAME          PIC X(30)       VALUE SPACES.EL315
00331      05  FILLER                      PIC X(92)       VALUE SPACES.EL315
00332                                                                   EL315
00333  01  WS-CL315A-HDG5-CDT.                                          EL315
00334      05  FILLER                      PIC X     VALUE '-'.         EL315
00335      05  FILLER                      PIC X(18) VALUE SPACE.       EL315
00336      05  FILLER                      PIC X(114)      VALUE        EL315
00337          'CLAIM    CERT     EFFECTIVE  ORIG TRM  ISS AGE  INCURREDEL315
00338 -        '    ORIG BEN   FACTOR          FUTURE          P.T.C.'. EL315
00339                                                                   EL315
00340  01  WS-CL315A-HDG5-CIDA.                                         EL315
00341      05  FILLER                      PIC X VALUE '-'.             EL315
00342      05  FILLER                      PIC X(18) VALUE SPACE.       EL315
00343      05  FILLER                      PIC X(114)      VALUE        EL315
00344          'CLAIM    CERT     EFFECTIVE  ORIG TRM ISS AGE INCURRED  EL315
00345 -        '                                                     '. EL315
00346                                                                   EL315
00347  01  WS-CL315B-HDG5-CDT.                                          EL315
00348      05  FILLER                      PIC X(67) VALUE              EL315
00349          '-   ****** SUMMARY OF RESERVES BY YEAR INCURRED ******'.EL315
00350      05  FILLER                      PIC X(65)       VALUE        EL315
00351          '   ********** SUMMARY OF RESERVES BY STATE **********'. EL315
00352                                                                   EL315
00353  01  WS-CL315B-HDG5-CIDA.                                         EL315
00354      05  FILLER                      PIC X(53) VALUE              EL315
00355          '- **************** SUMMARY BY YEAR INCURRED *********'. EL315
00356      05  FILLER                      PIC X(14) VALUE              EL315
00357          '********'.                                              EL315
00358      05  FILLER                      PIC X(53) VALUE              EL315
00359          '********************* SUMMARY BY STATE **************'. EL315
00360      05  FILLER                      PIC X(07) VALUE              EL315
00361          '*******'.                                               EL315
00362                                                                   EL315
00363  01  WS-CL315A-HDG6-CDT.                                          EL315
00364      05  FILLER                    PIC X(37)      VALUE           EL315
00365          ' STATE ACCOUNT    NUMBER   NUMBER   '.                  EL315
00366      05  EL315A-CDT-HDG            PIC X(12) VALUE 'PAID THRU   '.EL315
00367      05  FILLER                    PIC X(54)      VALUE           EL315
00368          'REM TRM  INC AGE  REPORTED    REM BEN    CDT TBL'.      EL315
00369      05  FILLER                    PIC X(30)       VALUE          EL315
00370          '   MANUAL                '.                             EL315
00371                                                                   EL315
00372  01  WS-CL315A-HDG6-CIDA.                                         EL315
00373      05  FILLER                    PIC X(37)      VALUE           EL315
00374          ' STATE ACCOUNT    NUMBER   NUMBER   '.                  EL315
00375      05  EL315A-CIDA-HDG           PIC X(12) VALUE 'PAID THRU   '.EL315
00376      05  FILLER                    PIC X(54)      VALUE           EL315
00377          'REM TRM INC AGE REPORTED    BENEFIT AMT  CIDA FACTOR'.  EL315
00378      05  FILLER                    PIC X(30)       VALUE          EL315
00379          'CIDA RESERVE  P.T.C. RESERVE '.                         EL315
00380                                                                   EL315
00381  01  WS-CL315B-HDG6-CDT.                                          EL315
00382      05  FILLER                      PIC X(49) VALUE              EL315
00383          '  YEAR     FUTURE        P.T.C.        MANUAL    '.     EL315
00384      05  FILLER                      PIC X(16) VALUE              EL315
00385          '  I.B.N.R.     '.                                       EL315
00386      05  FILLER                      PIC X(50) VALUE              EL315
00387          '   STATE    FUTURE        P.T.C.        MANUAL'.        EL315
00388      05  FILLER                      PIC X(10) VALUE              EL315
00389          '  I.B.N.R.'.                                            EL315
00390                                                                   EL315
00391                                                                   EL315
00392  01  WS-CL315B-HDG6-CIDA.                                         EL315
00393      05  FILLER                      PIC X(35) VALUE              EL315
00394          '  YEAR CIDA RESERVE     TOTAL PTC'.                     EL315
00395      05  FILLER                      PIC X(27) VALUE              EL315
00396          '     A&H PTC      LIFE PTC'.                            EL315
00397      05  FILLER                      PIC X(03) VALUE  SPACES.     EL315
00398      05  FILLER                      PIC X(35) VALUE              EL315
00399          '  STATE CIDA RESERVE   PTC RESERVE'.                    EL315
00400      05  FILLER                      PIC X(27) VALUE              EL315
00401          '     A&H PTC      LIFE PTC'.                            EL315
00402                                                                   EL315
00403      EJECT                                                        EL315
00404  01  WS-DETAIL1.                                                  EL315
00405      05  WS-D1-CARRIAGE-CONTROL            PIC X.                 EL315
00406      05  FILLER                            PIC X.                 EL315
00407      05  WS-D1-STATE                       PIC XX.                EL315
00408      05  FILLER                            PIC XX.                EL315
00409      05  WS-D1-ACCOUNT                     PIC X(10).             EL315
00410      05  FILLER                            PIC X.                 EL315
00411      05  WS-D1-CLAIM-NO                    PIC X(7).              EL315
00412      05  FILLER                            PIC X.                 EL315
00413      05  WS-D1-CERT-NO                     PIC X(11).             EL315
00414      05  FILLER                            PIC XX.                EL315
00415      05  WS-D1-EFFECTIVE-DATE              PIC X(8).              EL315
00416                                                                   EL315
00417      05  WS-D1-PAID-THRU-DATE  REDEFINES                          EL315
00418          WS-D1-EFFECTIVE-DATE  PIC X(8).                          EL315
00419                                                                   EL315
00420      05  FILLER                            PIC X(4).              EL315
00421      05  WS-D1-ORIGINAL-TERM               PIC ZZ9-.              EL315
00422                                                                   EL315
00423      05  WS-D1-REMAINING-TERM  REDEFINES                          EL315
00424          WS-D1-ORIGINAL-TERM   PIC ZZ9-.                          EL315
00425                                                                   EL315
00426      05  FILLER                            PIC X(5).              EL315
00427      05  WS-D1-ISSUED-AGE                  PIC ZZ9.               EL315
00428                                                                   EL315
00429      05  WS-D1-INCURRED-AGE    REDEFINES                          EL315
00430          WS-D1-ISSUED-AGE      PIC ZZ9.                           EL315
00431                                                                   EL315
00432      05  FILLER                            PIC X(3).              EL315
00433      05  WS-D1-INCURRED-DATE               PIC X(8).              EL315
00434                                                                   EL315
00435      05  WS-D1-REPORTED-DATE   REDEFINES                          EL315
00436          WS-D1-INCURRED-DATE   PIC X(8).                          EL315
00437                                                                   EL315
00438      05  FILLER                            PIC X(1).              EL315
00439      05  WS-D1-ORIG-BENEFIT             PIC ZZZ,ZZZ,ZZ9.99-.      EL315
00440                                                                   EL315
00441      05  WS-D1-REMAINING-BENEFIT REDEFINES                        EL315
00442          WS-D1-ORIG-BENEFIT PIC ZZZ,ZZZ,ZZ9.99-                   EL315
00443                                BLANK WHEN ZERO.                   EL315
00444                                                                   EL315
00445      05  WS-D1-FACTOR                      PIC ZZZZ9.999999.      EL315
00446                                                                   EL315
00447      05  WS-D1-LIFE       REDEFINES                               EL315
00448          WS-D1-FACTOR.                                            EL315
00449          10  FILLER           PIC X(6).                           EL315
00450          10  WS-D1-CDT-TABLE  PIC X.                              EL315
00451          10  FILLER           PIC X(5).                           EL315
00452                                                                   EL315
00453      05  WS-D1-FUTURE-RESERVE              PIC ZZZZ,ZZ9.99.       EL315
00454                                                                   EL315
00455      05  WS-D1-MANUAL-RESERVE  REDEFINES                          EL315
00456          WS-D1-FUTURE-RESERVE  PIC ZZZZ,ZZ9.99.                   EL315
00457                                                                   EL315
00458      05  WS-D1-FUTURE-RESERVE-FLAG         PIC X.                 EL315
00459      05  FILLER                            PIC X(3).              EL315
00460      05  WS-D1-PAY-TO-CURR-RESERVE         PIC Z,ZZZ,ZZ9.99-.     EL315
00461                                                                   EL315
00462      05  FILLER                            PIC X(4).              EL315
00463                                                                   EL315
00464      EJECT                                                        EL315
00465  01  WS-DETAIL2                      REDEFINES                    EL315
00466      WS-DETAIL1.                                                  EL315
00467                                                                   EL315
00468      05  FILLER                      PIC X.                       EL315
00469      05  WS-D2-YEAR.                                              EL315
00470          10  FILLER                  PIC X.                       EL315
00471          10  WS-D2-CENTURY           PIC XX.                      EL315
00472          10  WS-D2-YR                PIC XX.                      EL315
00473                                                                   EL315
00474      05  FILLER            REDEFINES                              EL315
00475          WS-D2-YEAR.                                              EL315
00476          10  FILLER                  PIC X(3).                    EL315
00477          10  WS-D2-YR2               PIC 9.                       EL315
00478          10  FILLER                  PIC X.                       EL315
00479                                                                   EL315
00480      05  FILLER                      PIC X(1).                    EL315
00481      05  WS-D2-YEAR-FUTURE           PIC ZZZZZ,ZZ9.99-.           EL315
00482      05  FILLER                      PIC X.                       EL315
00483      05  WS-D2-YEAR-PTC              PIC ZZZZZ,ZZ9.99-.           EL315
00484      05  FILLER                      PIC X.                       EL315
00485      05  WS-D2-YEAR-MANUAL           PIC ZZZZZ,ZZ9.99-.           EL315
00486      05  WS-D2-YEAR-MAN-X REDEFINES WS-D2-YEAR-MANUAL             EL315
00487                                       PIC X(13).                  EL315
00488      05  WS-D2-YEAR-PTC-AH REDEFINES WS-D2-YEAR-MANUAL            EL315
00489                                      PIC ZZZZZ,ZZ9.99-.           EL315
00490      05  FILLER                      PIC X.                       EL315
00491      05  WS-D2-YEAR-IBNR             PIC ZZZZZ,ZZ9.99-.           EL315
00492      05  WS-D2-YEAR-IBNR-X REDEFINES WS-D2-YEAR-IBNR              EL315
00493                                      PIC X(13).                   EL315
00494      05  WS-D2-YEAR-PTC-LF REDEFINES WS-D2-YEAR-IBNR              EL315
00495                                      PIC ZZZZZ,ZZ9.99-.           EL315
00496                                                                   EL315
00497      05  FILLER                      PIC X(5).                    EL315
00498                                                                   EL315
00499      05  WS-D2-TOTAL.                                             EL315
00500          10  FILLER                  PIC X(3).                    EL315
00501          10  WS-D2-STATE             PIC XX.                      EL315
00502      05  FILLER                      PIC X(1).                    EL315
00503      05  WS-D2-STATE-FUTURE          PIC ZZZZZ,ZZ9.99-.           EL315
00504      05  FILLER                      PIC X.                       EL315
00505      05  WS-D2-STATE-PTC             PIC ZZZZZ,ZZ9.99-.           EL315
00506      05  FILLER                      PIC X.                       EL315
00507      05  WS-D2-STATE-MANUAL          PIC ZZZZZ,ZZ9.99-.           EL315
00508      05  WS-D2-STATE-MAN-X REDEFINES WS-D2-STATE-MANUAL           EL315
00509                                      PIC X(13).                   EL315
00510      05  WS-D2-STATE-PTC-AH REDEFINES WS-D2-STATE-MANUAL          EL315
00511                                      PIC ZZZZZ,ZZ9.99-.           EL315
00512      05  FILLER                      PIC X.                       EL315
00513      05  WS-D2-STATE-IBNR            PIC ZZZZZ,ZZ9.99-.           EL315
00514      05  WS-D2-STATE-IBNR-X REDEFINES WS-D2-STATE-IBNR            EL315
00515                                      PIC X(13).                   EL315
00516      05  WS-D2-STATE-PTC-LF REDEFINES WS-D2-STATE-IBNR            EL315
00517                                      PIC ZZZZZ,ZZ9.99-.           EL315
00518      05  FILLER                      PIC X(5).                    EL315
00519                                                                   EL315
00520      EJECT                                                        EL315
00521  01  WS-TOTAL-LINE1                  REDEFINES                    EL315
00522      WS-DETAIL1.                                                  EL315
00523                                                                   EL315
00524      05  FILLER                      PIC X.                       EL315
00525                                                                   EL315
00526      05  WS-T1-DESCRIPTION           PIC X(29).                   EL315
00527                                                                   EL315
00528      05  FILLER                      REDEFINES                    EL315
00529          WS-T1-DESCRIPTION.                                       EL315
00530          10  FILLER                  PIC X(10).                   EL315
00531          10  WS-T1-STATE             PIC X(2).                    EL315
00532          10  FILLER                  PIC X(17).                   EL315
00533                                                                   EL315
00534      05  FILLER                      REDEFINES                    EL315
00535          WS-T1-DESCRIPTION.                                       EL315
00536          10  FILLER                  PIC X(12).                   EL315
00537          10  WS-T1-ACCOUNT.                                       EL315
00538              15  WS-T1-CARRIER       PIC X.                       EL315
00539              15  FILLER              PIC X(10).                   EL315
00540          10  FILLER                  PIC X(06).                   EL315
00541                                                                   EL315
00542      05  WS-T1-NUMBER-OF-CLAIMS      PIC ZZZZ9.                   EL315
00543      05  FILLER                      PIC X(2).                    EL315
00544      05  WS-T1-CLAIMS-DESCRIPTION    PIC X(6).                    EL315
00545      05  FILLER                      PIC X(5).                    EL315
00546      05  WS-T1-ORIG-TERM             PIC ZZ9.                     EL315
00547      05  WS-T1-REMAINING-TERM        REDEFINES                    EL315
00548          WS-T1-ORIG-TERM             PIC ZZ9.                     EL315
00549      05  WS-T1-AVERAGE-TERM-DESC     PIC X(7).                    EL315
00550      05  WS-T1-ISSUE-AGE             PIC Z9.                      EL315
00551      05  WS-T1-INCURRED-AGE          REDEFINES                    EL315
00552          WS-T1-ISSUE-AGE             PIC Z9.                      EL315
00553      05  WS-T1-AVERAGE-AGE-DESC      PIC X(5).                    EL315
00554      05  FILLER                      PIC X(10).                   EL315
00555      05  WS-T1-ORIG-BENEFIT          PIC ZZ,ZZZ,ZZ9.99-.          EL315
00556      05  WS-T1-REMAINING-BENEFIT     REDEFINES                    EL315
00557          WS-T1-ORIG-BENEFIT          PIC ZZ,ZZZ,ZZ9.99-.          EL315
00558      05  FILLER                      PIC X(10).                   EL315
00559      05  WS-T1-FUTURE-RESERVE        PIC ZZ,ZZZ,ZZ9.99-.          EL315
00560      05  WS-T1-MANUAL-RESERVE        REDEFINES                    EL315
00561          WS-T1-FUTURE-RESERVE        PIC ZZ,ZZZ,ZZ9.99-.          EL315
00562      05  FILLER                      PIC XX.                      EL315
00563      05  WS-T1-PTC-RESERVE           PIC ZZ,ZZZ,ZZ9.99-.          EL315
00564      05  WS-T1-IBNR-RESERVE          REDEFINES                    EL315
00565          WS-T1-PTC-RESERVE           PIC ZZ,ZZZ,ZZ9.99-.          EL315
00566      05  FILLER                      PIC X(4).                    EL315
00567                                                                   EL315
00568                                                                   EL315
00569      EJECT                                                        EL315
00570      COPY ELCDTECX.                                               EL315
00571      EJECT                                                        EL315
00572      COPY ELCDTEVR.                                               EL315
00573      EJECT                                                        EL315
00574      COPY ELCDATE.                                                EL315
00575      EJECT                                                        EL315
00576  PROCEDURE DIVISION.                                              EL315
00577                                                                   EL315
00578  CAPTURE-START.                                                   EL315
00579      OPEN I-O ERMEBL.                                             EL315
00580                                                                   EL315
00581      IF   ERMEBL-FILE-STATUS NOT = ZERO                           EL315
00582        AND ERMEBL-FILE-STATUS NOT = '97'                          EL315
00583           MOVE 'N'  TO ME-UPDATE-FLAG.                            EL315
00584                                                                   EL315
00585  0000-DATE-CARD-READ SECTION. COPY ELCDTERX SUPPRESS.             EL315
00586                                                                   EL315
00587      MOVE WS-TIME                TO ME-START-TIME.                EL315
00588      MOVE WS-CURRENT-DATE        TO ME-START-DATE.                EL315
00589      MOVE ME-START-MO            TO ME-CNDS-MO.                   EL315
00590      MOVE ME-START-DA            TO ME-CNDS-DA.                   EL315
00591      MOVE ME-START-YR            TO ME-CNDS-YR.                   EL315
00592      MOVE    DTE-CLIENT             TO ME-COMPANY.                EL315
00593      COMPUTE MONTH-END-MOYR = (RUN-CCYY * 12) + RUN-MO.              CL**3
00594      MOVE    MONTH-END-MOYR         TO ME-MOYR.                   EL315
00595                                                                   EL315
00596      IF  ME-DO-UPDATE                                             EL315
00597          READ ERMEBL INVALID KEY                                  EL315
00598          MOVE  'N' TO ME-UPDATE-FLAG                              EL315
00599          CLOSE ERMEBL.                                            EL315
00600                                                                   EL315
00601  1000-MAIN-LOGIC SECTION.                                         EL315
00602                                                                   EL315
00603      PERFORM OPEN-FILES.                                          EL315
00604                                                                   EL315
00605      IF DTE-CLAIM-PAID-THRU-TO EQUAL '1'                          EL315
00606          IF DTE-OPT-RESERVE-METHOD-AUTH                           EL315
00607              MOVE 'PAID          TO  ' TO EL315A-CIDA-HDG         EL315
00608          ELSE                                                     EL315
00609              MOVE 'PAID          TO  ' TO EL315A-CDT-HDG.         EL315
00610                                                                   EL315
00611      SORT SORT-WORK-FILE                                          EL315
00612          ON ASCENDING KEY EX-AA-CARRIER                           EL315
00613                           EX-AA-STATE                             EL315
00614                           EX-AA-ACCOUNT                           EL315
00615                           EX-SA-CLAIM-NO                          EL315
00616                           EX-SA-CERT-NO                           EL315
00617                           EX-AA-CERT-EFF-DT                       EL315
00618                           EX-RECORD-TYPE                          EL315
00619          INPUT PROCEDURE  IS 2000-SORT-INPUT-PROCEDURE            EL315
00620          OUTPUT PROCEDURE IS 3000-SORT-OUTPUT-PROCEDURE           EL315
00621                                                                   EL315
00622      IF SORT-RETURN GREATER THAN ZERO                             EL315
00623          MOVE    'SORT FAILED'  TO  WS-ABEND-MESSAGE              EL315
00624          MOVE    SORT-RETURN    TO  WS-RETURN-CODE                EL315
00625          GO TO ABEND-PGM.                                         EL315
00626                                                                   EL315
00627      PERFORM CLOSE-FILES.                                         EL315
00628                                                                   EL315
00629      ADD  WS-GT-MANUAL-RESERVE                                    EL315
00630           WS-GT-FUTURE-RESERVE                                    EL315
00631           WS-GT-PTC-RESERVE                                       EL315
00632           WS-GT-IBNR-RESERVE                                      EL315
00633           GIVING ME-315-RESV-L.                                   EL315
00634                                                                   EL315
00635      MOVE ZERO           TO  ME-315-RESV-AH.                      EL315
00636      MOVE ME-CNDS-DATE   TO  ME-315-RUN-DT.                       EL315
00637      MOVE ME-START-TIME  TO  ME-315-START.                        EL315
00638                                                                   EL315
00639      IF  ME-DO-UPDATE                                             EL315
00640          ADD     1            TO  ME-315-RUN-CT                   EL315
00641          ACCEPT WS-TIME-OF-DAY FROM TIME                          EL315
00642          MOVE    WS-TIME         TO  ME-315-END                   EL315
00643          REWRITE MONTH-END-BALANCES                               EL315
00644          CLOSE   ERMEBL.                                          EL315
00645                                                                   EL315
00646      GOBACK.                                                      EL315
00647                                                                   EL315
00648      EJECT                                                        EL315
00649  2000-SORT-INPUT-PROCEDURE SECTION.                               EL315
00650                                                                   EL315
00651 *    NOTE ******************************************************* EL315
00652 *         *      THIS SECTION SELECTS ONLY THE EXTRACT AA       * EL315
00653 *         *      RECORD FOR THIS REPORT.                        * EL315
00654 *         *******************************************************.EL315
00655                                                                   EL315
00656  2100-SORT-INPUT-PROCEDURE.                                       EL315
00657                                                                   EL315
00658      READ REPORTS-EXTRACT-FILE INTO REPORTS-EXTRACT-RECORD        EL315
00659          AT END                                                   EL315
00660              GO TO 2900-EXIT.                                     EL315
00661                                                                   EL315
00662      ADD +1  TO  WS-RECORD-COUNT.                                 EL315
00663                                                                   EL315
00664      IF EX-POSITIONING-CODE GREATER THAN '1'                      EL315
00665          GO TO 2900-EXIT.                                         EL315
00666                                                                   EL315
00667      IF EX-EXTRACT-CODE GREATER THAN 'A'                          EL315
00668          GO TO 2900-EXIT.                                         EL315
00669                                                                   EL315
00670      IF EX-COMPANY-CD LESS THAN DTE-CLASIC-COMPANY-CD             EL315
00671          GO TO 2100-SORT-INPUT-PROCEDURE.                         EL315
00672                                                                   EL315
00673      IF EX-COMPANY-CD GREATER THAN DTE-CLASIC-COMPANY-CD          EL315
00674          GO TO 2900-EXIT.                                         EL315
00675                                                                   EL315
00676      IF EX-RECORD-TYPE GREATER THAN 'A'                           EL315
00677          GO TO 2900-EXIT.                                         EL315
00678                                                                   EL315
00679      IF EX-AA-CERT-STATUS EQUAL 'D' OR 'V'                        EL315
00680          GO TO 2100-SORT-INPUT-PROCEDURE.                         EL315
00681                                                                   EL315
           IF EX-SA-CLAIM-NO (1:2) NOT = 'SA'
              GO TO 2100-SORT-INPUT-PROCEDURE
           END-IF
00682      IF EX-AA-REMAINING-TERM IS NOT GREATER THAN ZERO             EL315
00683          MOVE ZERO  TO  EX-AA-REMAINING-TERM.                     EL315
00684                                                                   EL315
00685      IF EX-AA-REMAINING-BENEFIT IS NOT GREATER THAN ZERO          EL315
00686          MOVE ZERO  TO  EX-AA-REMAINING-BENEFIT.                  EL315
00687                                                                   EL315
00688      IF  DTE-OPT-RESERVE-METHOD-AUTH                              EL315
00689          PERFORM 2300-TEST-RESERVES-OPT THRU 2300-EXIT            EL315
00690          MOVE ZERO  TO  EX-AA-PAY-CURRENT-RESERVE                 EL315
00691                         EX-AA-IBNR-RESERVE                        EL315
00692                         EX-AA-FUTURE-RESERVE                      EL315
00693                         EX-AA-MANUAL-RESERVE                      EL315
00694          GO TO 2200-SORT-INPUT-PROCEDURE.                         EL315
00695                                                                   EL315
00696      IF EX-AA-PAY-CURRENT-RESERVE IS NOT GREATER THAN ZERO        EL315
00697          MOVE ZERO  TO  EX-AA-PAY-CURRENT-RESERVE.                EL315
00698                                                                   EL315
00699      IF EX-AA-IBNR-RESERVE IS NOT GREATER THAN ZERO               EL315
00700          MOVE ZERO  TO  EX-AA-IBNR-RESERVE.                       EL315
00701                                                                   EL315
00702      IF EX-AA-FUTURE-RESERVE IS NOT GREATER THAN ZERO             EL315
00703          MOVE ZERO  TO  EX-AA-FUTURE-RESERVE.                     EL315
00704                                                                   EL315
00705      IF EX-AA-MANUAL-RESERVE IS NOT GREATER THAN ZERO             EL315
00706          MOVE ZERO  TO  EX-AA-MANUAL-RESERVE.                     EL315
00707                                                                   EL315
00708  2200-SORT-INPUT-PROCEDURE.                                       EL315
00709                                                                   EL315
00710      RELEASE REPORTS-EXTRACT-RECORD.                              EL315
00711                                                                   EL315
00712      IF SORT-RETURN GREATER THAN ZERO                             EL315
00713          MOVE 'ERROR OCCURED SORT - RELEASE'  TO  WS-ABEND-MESSAGEEL315
00714          MOVE SORT-RETURN  TO  WS-RETURN-CODE                     EL315
00715          GO TO ABEND-PGM.                                         EL315
00716                                                                   EL315
00717      ADD +1  TO  WS-RELEASED-COUNT.                               EL315
00718      GO TO 2100-SORT-INPUT-PROCEDURE.                             EL315
00719                                                                   EL315
00720      EJECT                                                        EL315
00721  2300-TEST-RESERVES-OPT.                                          EL315
00722                                                                   EL315
00723      IF  EX-AA-PAY-CURRENT-RSV-OPT IS NOT GREATER THAN ZERO       EL315
00724          MOVE ZERO               TO EX-AA-PAY-CURRENT-RSV-OPT.    EL315
00725                                                                   EL315
00726      IF  EX-AA-IBNR-RSV-OPT IS NOT GREATER THAN ZERO              EL315
00727          MOVE ZERO               TO EX-AA-IBNR-RSV-OPT.           EL315
00728                                                                   EL315
00729      IF  EX-AA-FUTURE-RSV-OPT IS NOT GREATER THAN ZERO            EL315
00730          MOVE ZERO               TO EX-AA-FUTURE-RSV-OPT.         EL315
00731                                                                   EL315
00732      IF  EX-AA-MANUAL-RSV-OPT IS NOT GREATER THAN ZERO            EL315
00733          MOVE ZERO               TO EX-AA-MANUAL-RSV-OPT.         EL315
00734                                                                   EL315
00735  2300-EXIT.                                                       EL315
00736      EXIT.                                                        EL315
00737                                                                   EL315
00738  2900-EXIT.                                                       EL315
00739      EXIT.                                                        EL315
00740      EJECT                                                        EL315
00741  3000-SORT-OUTPUT-PROCEDURE SECTION.                              EL315
00742                                                                   EL315
00743  3100-SORT-OUTPUT-PROCEDURE.                                      EL315
00744                                                                   EL315
00745      RETURN SORT-WORK-FILE                                        EL315
00746          AT END                                                   EL315
00747              MOVE HIGH-VALUES    TO  EX-AA-ACCOUNT                EL315
00748                                      EX-AA-STATE                  EL315
00749                                      EX-AA-CARRIER.               EL315
00750                                                                   EL315
00751      IF SORT-RETURN GREATER THAN ZERO                             EL315
00752          MOVE 'ERROR OCCURED SORT - RETURN'  TO  WS-ABEND-MESSAGE EL315
00753          MOVE SORT-RETURN        TO  WS-RETURN-CODE               EL315
00754          GO TO ABEND-PGM.                                         EL315
00755                                                                   EL315
00756      IF  WS-LAST-ACCOUNT EQUAL SPACES                             EL315
00757          NEXT SENTENCE                                            EL315
00758      ELSE                                                         EL315
00759          GO TO 3110-SORT-OUTPUT-PROCEDURE.                        EL315
00760                                                                   EL315
00761      MOVE    EX-AA-ACCOUNT     TO  WS-LAST-ACCOUNT.               EL315
00762      MOVE    EX-AA-STATE       TO  WS-LAST-STATE.                 EL315
00763      MOVE    EX-AA-CARRIER     TO  WS-LAST-CARRIER                EL315
00764                                    CT-CARRIER.                    EL315
00765      PERFORM 8100-GET-CARRIER-NAME.                               EL315
00766                                                                   EL315
00767      IF WS-RELEASED-COUNT GREATER THAN ZERO                       EL315
00768          GO TO 3110-SORT-OUTPUT-PROCEDURE.                        EL315
00769                                                                   EL315
00770      MOVE    '0 NO RECORDS EXTRACTED FOR THIS REPORT' TO PRT.     EL315
00771      PERFORM WRITE-A-LINE.                                        EL315
00772      GO TO 3900-EXIT.                                                CL**2
00773                                                                   EL315
00774      EJECT                                                        EL315
00775  3110-SORT-OUTPUT-PROCEDURE.                                      EL315
00776                                                                   EL315
00777 *    NOTE ******************************************************* EL315
00778 *         *                                                     * EL315
00779 *         *          CONTROL BREAK LOGIC FOR ACCOUNT            * EL315
00780 *         *******************************************************.EL315
00781                                                                   EL315
00782      IF    WS-LAST-CARRIER = EX-AA-CARRIER                        EL315
00783        AND WS-LAST-STATE   = EX-AA-STATE                          EL315
00784        AND WS-LAST-ACCOUNT = EX-AA-ACCOUNT                        EL315
00785            GO TO 3200-SORT-OUTPUT-PROCEDURE.                      EL315
00786                                                                   EL315
00787      IF WS-LINE-COUNT GREATER THAN WS-LINE-COUNT-MAX-2            EL315
00788         MOVE +99      TO WS-LINE-COUNT.                           EL315
00789                                                                   EL315
00790      MOVE '-'                       TO  WS-TOTAL-LINE1.           EL315
00791      MOVE '*** ACCOUNT XXXXXXXXXX ***'  TO  WS-T1-DESCRIPTION.    EL315
00792      MOVE WS-LAST-ACCOUNT           TO  WS-T1-ACCOUNT.            EL315
00793      MOVE WS-AT-NUMBER-OF-CLAIMS    TO  WS-T1-NUMBER-OF-CLAIMS.   EL315
00794      MOVE 'CLAIMS'                  TO  WS-T1-CLAIMS-DESCRIPTION. EL315
00795                                                                   EL315
00796      IF WS-AT-NUMBER-OF-CLAIMS NOT = ZERO                         EL315
00797          DIVIDE WS-AT-ORIG-TERM BY WS-AT-NUMBER-OF-CLAIMS         EL315
00798                 GIVING WS-T1-ORIG-TERM ROUNDED                    EL315
00799      ELSE                                                         EL315
00800          MOVE   ZERO  TO  WS-T1-ORIG-TERM.                        EL315
00801                                                                   EL315
00802      MOVE '(AVG)'  TO  WS-T1-AVERAGE-TERM-DESC.                   EL315
00803                                                                   EL315
00804      IF WS-AT-NUMBER-OF-CLAIMS NOT = ZERO                         EL315
00805          DIVIDE WS-AT-ISSUE-AGE BY WS-AT-NUMBER-OF-CLAIMS         EL315
00806                 GIVING WS-T1-ISSUE-AGE ROUNDED                    EL315
00807      ELSE                                                         EL315
00808          MOVE   ZERO  TO  WS-T1-ISSUE-AGE.                        EL315
00809                                                                   EL315
00810      MOVE    '(AVG)'               TO  WS-T1-AVERAGE-AGE-DESC.    EL315
00811      MOVE    WS-AT-ORIG-BENEFIT    TO  WS-T1-ORIG-BENEFIT.        EL315
00812      MOVE    WS-AT-FUTURE-RESERVE  TO  WS-T1-FUTURE-RESERVE.      EL315
00813      MOVE    WS-AT-PTC-RESERVE     TO  WS-T1-PTC-RESERVE.         EL315
00814      MOVE    WS-TOTAL-LINE1        TO  PRT.                       EL315
00815      PERFORM WRITE-A-LINE.                                        EL315
00816      MOVE    SPACES                TO  WS-TOTAL-LINE1.            EL315
00817                                                                   EL315
00818      IF WS-AT-NUMBER-OF-CLAIMS NOT = ZERO                         EL315
00819          DIVIDE WS-AT-REMAINING-TERM BY WS-AT-NUMBER-OF-CLAIMS    EL315
00820                 GIVING WS-T1-REMAINING-TERM ROUNDED               EL315
00821      ELSE                                                         EL315
00822          MOVE   ZERO  TO  WS-T1-REMAINING-TERM.                   EL315
00823                                                                   EL315
00824      MOVE '(AVG)'  TO  WS-T1-AVERAGE-TERM-DESC.                   EL315
00825                                                                   EL315
00826      IF WS-AT-INCURRED-AGE-CNT NOT = ZERO                         EL315
00827          DIVIDE WS-AT-INCURRED-AGE BY WS-AT-INCURRED-AGE-CNT      EL315
00828                 GIVING WS-T1-INCURRED-AGE ROUNDED                 EL315
00829      ELSE                                                         EL315
00830          MOVE   ZERO  TO  WS-T1-INCURRED-AGE.                     EL315
00831                                                                   EL315
00832      MOVE    '(AVG)'                TO  WS-T1-AVERAGE-AGE-DESC.   EL315
00833                                                                   EL315
00834      IF DTE-OPT-RESERVE-METHOD-AUTH                               EL315
00835          GO TO 3110-BYPASS-ACCOUNT-CIDA.                          EL315
00836                                                                   EL315
00837      MOVE    WS-AT-REM-BENEFIT      TO  WS-T1-REMAINING-BENEFIT.  EL315
00838      MOVE    WS-AT-MANUAL-RESERVE   TO  WS-T1-MANUAL-RESERVE.     EL315
00839 *RTK MOVE    WS-AT-IBNR-RESERVE     TO  WS-T1-IBNR-RESERVE.       EL315
00840                                                                   EL315
00841  3110-BYPASS-ACCOUNT-CIDA.                                        EL315
00842                                                                   EL315
00843      MOVE    WS-TOTAL-LINE1         TO  PRT.                      EL315
00844      PERFORM WRITE-A-LINE.                                        EL315
00845      MOVE    +1                     TO  WS-PRINT-SW.              EL315
00846      ADD     WS-AT-NUMBER-OF-CLAIMS TO  WS-ST-NUMBER-OF-CLAIMS.   EL315
00847      ADD     WS-AT-ORIG-TERM        TO  WS-ST-ORIG-TERM.          EL315
00848      ADD     WS-AT-REMAINING-TERM   TO  WS-ST-REMAINING-TERM.     EL315
00849      ADD     WS-AT-ISSUE-AGE        TO  WS-ST-ISSUE-AGE.          EL315
00850      ADD     WS-AT-INCURRED-AGE     TO  WS-ST-INCURRED-AGE.       EL315
00851      ADD     WS-AT-INCURRED-AGE-CNT TO  WS-ST-INCURRED-AGE-CNT.   EL315
00852      ADD     WS-AT-ORIG-BENEFIT     TO  WS-ST-ORIG-BENEFIT.       EL315
00853      ADD     WS-AT-REM-BENEFIT      TO  WS-ST-REM-BENEFIT.        EL315
00854      ADD     WS-AT-MANUAL-RESERVE   TO  WS-ST-MANUAL-RESERVE.     EL315
00855      ADD     WS-AT-FUTURE-RESERVE   TO  WS-ST-FUTURE-RESERVE.     EL315
00856      ADD     WS-AT-PTC-RESERVE      TO  WS-ST-PTC-RESERVE.        EL315
00857      ADD     WS-AT-PTC-RESERVE-AH   TO  WS-ST-PTC-RESERVE-AH.     EL315
00858      ADD     WS-AT-PTC-RESERVE-LF   TO  WS-ST-PTC-RESERVE-LF.     EL315
00859      ADD     WS-AT-IBNR-RESERVE     TO  WS-ST-IBNR-RESERVE.       EL315
00860                                                                   EL315
00861      MOVE    ZERO                   TO  WS-AT-NUMBER-OF-CLAIMS    EL315
00862                                         WS-AT-ORIG-TERM           EL315
00863                                         WS-AT-REMAINING-TERM      EL315
00864                                         WS-AT-ISSUE-AGE           EL315
00865                                         WS-AT-INCURRED-AGE        EL315
00866                                         WS-AT-INCURRED-AGE-CNT    EL315
00867                                         WS-AT-ORIG-BENEFIT        EL315
00868                                         WS-AT-REM-BENEFIT         EL315
00869                                         WS-AT-MANUAL-RESERVE      EL315
00870                                         WS-AT-FUTURE-RESERVE      EL315
00871                                         WS-AT-PTC-RESERVE         EL315
00872                                         WS-AT-PTC-RESERVE-AH      EL315
00873                                         WS-AT-PTC-RESERVE-LF      EL315
00874                                         WS-AT-IBNR-RESERVE.       EL315
00875      MOVE    EX-AA-ACCOUNT          TO  WS-LAST-ACCOUNT.          EL315
00876                                                                   EL315
00877      EJECT                                                        EL315
00878 *    NOTE ******************************************************* EL315
00879 *         *                                                     * EL315
00880 *         *          CONTROL BREAK LOGIC FOR STATE              * EL315
00881 *         *******************************************************.EL315
00882                                                                   EL315
00883      IF   WS-LAST-CARRIER NOT = EX-AA-CARRIER                     EL315
00884        OR WS-LAST-STATE NOT = EX-AA-STATE                         EL315
00885           NEXT SENTENCE                                           EL315
00886      ELSE                                                         EL315
00887           GO TO 3200-SORT-OUTPUT-PROCEDURE.                       EL315
00888                                                                   EL315
00889      IF WS-LINE-COUNT GREATER THAN WS-LINE-COUNT-MAX-2            EL315
00890         MOVE +99      TO WS-LINE-COUNT.                           EL315
00891                                                                   EL315
00892      MOVE '-'                    TO  WS-TOTAL-LINE1.              EL315
00893      MOVE '*** STATE XX ***'     TO  WS-T1-DESCRIPTION.           EL315
00894      MOVE WS-LAST-STATE          TO  WS-T1-STATE.                 EL315
00895      MOVE WS-ST-NUMBER-OF-CLAIMS TO  WS-T1-NUMBER-OF-CLAIMS.      EL315
00896      MOVE 'CLAIMS'               TO  WS-T1-CLAIMS-DESCRIPTION.    EL315
00897                                                                   EL315
00898      IF WS-ST-NUMBER-OF-CLAIMS NOT = ZERO                         EL315
00899          DIVIDE WS-ST-ORIG-TERM BY WS-ST-NUMBER-OF-CLAIMS         EL315
00900                 GIVING WS-T1-ORIG-TERM ROUNDED                    EL315
00901      ELSE                                                         EL315
00902          MOVE   ZERO  TO  WS-T1-ORIG-TERM.                        EL315
00903                                                                   EL315
00904      MOVE '(AVG)' TO  WS-T1-AVERAGE-TERM-DESC.                    EL315
00905                                                                   EL315
00906      IF WS-ST-NUMBER-OF-CLAIMS NOT = ZERO                         EL315
00907          DIVIDE WS-ST-ISSUE-AGE BY WS-ST-NUMBER-OF-CLAIMS         EL315
00908                 GIVING WS-T1-ISSUE-AGE ROUNDED                    EL315
00909      ELSE                                                         EL315
00910          MOVE   ZERO  TO  WS-T1-ISSUE-AGE.                        EL315
00911                                                                   EL315
00912      MOVE    '(AVG)'               TO  WS-T1-AVERAGE-AGE-DESC.    EL315
00913      MOVE    WS-ST-ORIG-BENEFIT    TO  WS-T1-ORIG-BENEFIT.        EL315
00914      MOVE    WS-ST-FUTURE-RESERVE  TO  WS-T1-FUTURE-RESERVE.      EL315
00915      MOVE    WS-ST-PTC-RESERVE     TO  WS-T1-PTC-RESERVE.         EL315
00916      MOVE    WS-TOTAL-LINE1        TO  PRT.                       EL315
00917      PERFORM WRITE-A-LINE.                                        EL315
00918      MOVE    SPACES                TO  WS-TOTAL-LINE1.            EL315
00919                                                                   EL315
00920      IF WS-ST-NUMBER-OF-CLAIMS NOT = ZERO                         EL315
00921          DIVIDE WS-ST-REMAINING-TERM BY WS-ST-NUMBER-OF-CLAIMS    EL315
00922                 GIVING WS-T1-REMAINING-TERM ROUNDED               EL315
00923      ELSE                                                         EL315
00924          MOVE   ZERO  TO  WS-T1-REMAINING-TERM.                   EL315
00925                                                                   EL315
00926      MOVE '(AVG)'  TO  WS-T1-AVERAGE-TERM-DESC.                   EL315
00927                                                                   EL315
00928      IF WS-ST-INCURRED-AGE-CNT NOT = ZERO                         EL315
00929          DIVIDE WS-ST-INCURRED-AGE BY WS-ST-INCURRED-AGE-CNT      EL315
00930                 GIVING WS-T1-INCURRED-AGE ROUNDED                 EL315
00931      ELSE                                                         EL315
00932          MOVE   ZERO  TO  WS-T1-INCURRED-AGE.                     EL315
00933                                                                   EL315
00934      MOVE '(AVG)'               TO  WS-T1-AVERAGE-AGE-DESC.       EL315
00935                                                                   EL315
00936      IF DTE-OPT-RESERVE-METHOD-AUTH                               EL315
00937          GO TO 3110-BYPASS-STATE-CIDA.                            EL315
00938                                                                   EL315
00939      MOVE WS-ST-REM-BENEFIT     TO  WS-T1-REMAINING-BENEFIT.      EL315
00940      MOVE WS-ST-MANUAL-RESERVE  TO  WS-T1-MANUAL-RESERVE.         EL315
00941 *RTK MOVE WS-ST-IBNR-RESERVE    TO  WS-T1-IBNR-RESERVE.           EL315
00942                                                                   EL315
00943  3110-BYPASS-STATE-CIDA.                                          EL315
00944                                                                   EL315
00945      MOVE WS-TOTAL-LINE1        TO  PRT.                          EL315
00946      PERFORM WRITE-A-LINE.                                        EL315
00947      MOVE +1                    TO  WS-PRINT-SW.                  EL315
00948      ADD WS-ST-NUMBER-OF-CLAIMS TO  WS-CT-NUMBER-OF-CLAIMS.       EL315
00949      ADD WS-ST-ORIG-TERM        TO  WS-CT-ORIG-TERM.              EL315
00950      ADD WS-ST-REMAINING-TERM   TO  WS-CT-REMAINING-TERM.         EL315
00951      ADD WS-ST-ISSUE-AGE        TO  WS-CT-ISSUE-AGE.              EL315
00952      ADD WS-ST-INCURRED-AGE     TO  WS-CT-INCURRED-AGE.           EL315
00953      ADD WS-ST-INCURRED-AGE-CNT TO  WS-CT-INCURRED-AGE-CNT.       EL315
00954      ADD WS-ST-ORIG-BENEFIT     TO  WS-CT-ORIG-BENEFIT.           EL315
00955      ADD WS-ST-REM-BENEFIT      TO  WS-CT-REM-BENEFIT.            EL315
00956                                                                   EL315
00957      ADD WS-ST-MANUAL-RESERVE TO WS-CT-MANUAL-RESERVE             EL315
00958                                  CT-MANUAL-BY-STATE (STATE-INDEX).EL315
00959      ADD WS-ST-FUTURE-RESERVE TO WS-CT-FUTURE-RESERVE             EL315
00960                                  CT-FUTURE-BY-STATE (STATE-INDEX).EL315
00961      ADD WS-ST-PTC-RESERVE    TO WS-CT-PTC-RESERVE                EL315
00962                                  CT-PTC-BY-STATE (STATE-INDEX).   EL315
00963      ADD WS-ST-PTC-RESERVE-AH TO WS-CT-PTC-RESERVE-AH             EL315
00964                                  CT-PTC-BY-STATE-AH (STATE-INDEX).EL315
00965      ADD WS-ST-PTC-RESERVE-LF TO WS-CT-PTC-RESERVE-LF             EL315
00966                                  CT-PTC-BY-STATE-LF (STATE-INDEX).EL315
00967      ADD WS-ST-IBNR-RESERVE   TO WS-CT-IBNR-RESERVE               EL315
00968                                  CT-IBNR-BY-STATE (STATE-INDEX).  EL315
00969                                                                   EL315
00970      MOVE WS-LAST-STATE       TO CT-STATE (STATE-INDEX).          EL315
00971                                                                   EL315
00972      SET STATE-INDEX UP BY +1.                                    EL315
00973                                                                   EL315
00974      MOVE ZERO         TO  WS-ST-NUMBER-OF-CLAIMS                 EL315
00975                            WS-ST-ORIG-TERM                        EL315
00976                            WS-ST-REMAINING-TERM                   EL315
00977                            WS-ST-ISSUE-AGE                        EL315
00978                            WS-ST-INCURRED-AGE                     EL315
00979                            WS-ST-INCURRED-AGE-CNT                 EL315
00980                            WS-ST-ORIG-BENEFIT                     EL315
00981                            WS-ST-REM-BENEFIT                      EL315
00982                            WS-ST-MANUAL-RESERVE                   EL315
00983                            WS-ST-FUTURE-RESERVE                   EL315
00984                            WS-ST-PTC-RESERVE                      EL315
00985                            WS-ST-PTC-RESERVE-AH                   EL315
00986                            WS-ST-PTC-RESERVE-LF                   EL315
00987                            WS-ST-IBNR-RESERVE.                    EL315
00988                                                                   EL315
00989      MOVE EX-AA-STATE  TO  WS-LAST-STATE.                         EL315
00990                                                                   EL315
00991      EJECT                                                        EL315
00992 *    NOTE ******************************************************* EL315
00993 *         *                                                     * EL315
00994 *         *          CONTROL BREAK LOGIC FOR CARRIER            * EL315
00995 *         *******************************************************.EL315
00996                                                                   EL315
00997      IF EX-AA-CARRIER = WS-LAST-CARRIER                           EL315
00998          GO TO 3200-SORT-OUTPUT-PROCEDURE.                        EL315
00999                                                                   EL315
01000      IF WS-LINE-COUNT GREATER THAN WS-LINE-COUNT-MAX-2            EL315
01001         MOVE +99      TO WS-LINE-COUNT.                           EL315
01002                                                                   EL315
01003      MOVE '-'                       TO  WS-TOTAL-LINE1.           EL315
01004      MOVE    '*** CARRIER X TOTALS ***'  TO  WS-T1-DESCRIPTION.   EL315
01005      MOVE    WS-LAST-CARRIER        TO  WS-T1-CARRIER.            EL315
01006      MOVE    WS-CT-NUMBER-OF-CLAIMS TO  WS-T1-NUMBER-OF-CLAIMS.   EL315
01007      MOVE    'CLAIMS'               TO  WS-T1-CLAIMS-DESCRIPTION. EL315
01008      MOVE    WS-CT-ORIG-BENEFIT     TO  WS-T1-ORIG-BENEFIT.       EL315
01009      MOVE    WS-CT-FUTURE-RESERVE   TO  WS-T1-FUTURE-RESERVE.     EL315
01010      MOVE    WS-CT-PTC-RESERVE      TO  WS-T1-PTC-RESERVE.        EL315
01011      MOVE    WS-TOTAL-LINE1         TO  PRT.                      EL315
01012      PERFORM WRITE-A-LINE.                                        EL315
01013      MOVE    SPACES                 TO  WS-TOTAL-LINE1.           EL315
01014                                                                   EL315
01015      IF DTE-OPT-RESERVE-METHOD-AUTH                               EL315
01016          GO TO 3110-BYPASS-CARRIER-CIDA.                          EL315
01017                                                                   EL315
01018      MOVE    WS-CT-REM-BENEFIT      TO  WS-T1-REMAINING-BENEFIT.  EL315
01019      MOVE    WS-CT-MANUAL-RESERVE   TO  WS-T1-MANUAL-RESERVE.     EL315
01020 *RTK MOVE    WS-CT-IBNR-RESERVE     TO  WS-T1-IBNR-RESERVE.       EL315
01021      MOVE    WS-TOTAL-LINE1         TO  PRT.                      EL315
01022      PERFORM WRITE-A-LINE.                                        EL315
01023                                                                   EL315
01024  3110-BYPASS-CARRIER-CIDA.                                        EL315
01025                                                                   EL315
01026      ADD     WS-CT-NUMBER-OF-CLAIMS TO  WS-GT-NUMBER-OF-CLAIMS.   EL315
01027      ADD     WS-CT-ORIG-TERM        TO  WS-GT-ORIG-TERM.          EL315
01028      ADD     WS-CT-REMAINING-TERM   TO  WS-GT-REMAINING-TERM.     EL315
01029      ADD     WS-CT-ISSUE-AGE        TO  WS-GT-ISSUE-AGE.          EL315
01030      ADD     WS-CT-INCURRED-AGE     TO  WS-GT-INCURRED-AGE.       EL315
01031      ADD     WS-CT-INCURRED-AGE-CNT TO  WS-GT-INCURRED-AGE-CNT.   EL315
01032      ADD     WS-CT-ORIG-BENEFIT     TO  WS-GT-ORIG-BENEFIT.       EL315
01033      ADD     WS-CT-REM-BENEFIT      TO  WS-GT-REM-BENEFIT.        EL315
01034      ADD     WS-CT-MANUAL-RESERVE   TO  WS-GT-MANUAL-RESERVE.     EL315
01035      ADD     WS-CT-FUTURE-RESERVE   TO  WS-GT-FUTURE-RESERVE.     EL315
01036      ADD     WS-CT-PTC-RESERVE      TO  WS-GT-PTC-RESERVE.        EL315
01037      ADD     WS-CT-PTC-RESERVE-AH   TO  WS-GT-PTC-RESERVE-AH.     EL315
01038      ADD     WS-CT-PTC-RESERVE-LF   TO  WS-GT-PTC-RESERVE-LF.     EL315
01039      ADD     WS-CT-IBNR-RESERVE     TO  WS-GT-IBNR-RESERVE.       EL315
01040      MOVE    ZERO                   TO  WS-CT-NUMBER-OF-CLAIMS    EL315
01041                                         WS-CT-ORIG-TERM           EL315
01042                                         WS-CT-REMAINING-TERM      EL315
01043                                         WS-CT-ISSUE-AGE           EL315
01044                                         WS-CT-INCURRED-AGE        EL315
01045                                         WS-CT-INCURRED-AGE-CNT    EL315
01046                                         WS-CT-ORIG-BENEFIT        EL315
01047                                         WS-CT-REM-BENEFIT         EL315
01048                                         WS-CT-MANUAL-RESERVE      EL315
01049                                         WS-CT-FUTURE-RESERVE      EL315
01050                                         WS-CT-PTC-RESERVE         EL315
01051                                         WS-CT-PTC-RESERVE-AH      EL315
01052                                         WS-CT-PTC-RESERVE-LF      EL315
01053                                         WS-CT-IBNR-RESERVE.       EL315
01054                                                                   EL315
01055      SET     STATE-INDEX DOWN BY +1.                              EL315
01056      SET     CT-NUMBER-OF-STATES TO STATE-INDEX.                  EL315
01057      MOVE    CARRIER-TOTALS   TO  CARRIER-TOTALS-ENTRY (CT-INDEX).EL315
01058      MOVE 'A'                    TO WS-RPT.                       EL315
01059      PERFORM 4000-PRINT-CARRIER-TOTALS.                           EL315
01060      SET     CT-INDEX            UP BY +1.                        EL315
01061      MOVE    CARRIER-TOTALS-ZERO TO CARRIER-TOTALS.               EL315
01062      SET     STATE-INDEX         TO +1.                           EL315
01063      MOVE    EX-AA-CARRIER       TO  CT-CARRIER                   EL315
01064                                      WS-LAST-CARRIER.             EL315
01065      PERFORM 8100-GET-CARRIER-NAME.                               EL315
01066                                                                   EL315
01067      EJECT                                                        EL315
01068 *    NOTE ******************************************************* EL315
01069 *         *                                                     * EL315
01070 *         *          CONTROL BREAK LOGIC FOR FINAL              * EL315
01071 *         *******************************************************.EL315
01072                                                                   EL315
01073      IF EX-AA-CARRIER NOT = HIGH-VALUES                           EL315
01074          GO TO 3200-SORT-OUTPUT-PROCEDURE.                        EL315
01075                                                                   EL315
01076      IF WS-LINE-COUNT GREATER THAN WS-LINE-COUNT-MAX-2            EL315
01077         MOVE +99      TO WS-LINE-COUNT.                           EL315
01078                                                                   EL315
01079      MOVE    SPACES                 TO  WS-H3-C                   EL315
01080                                         WS-H3-CARRIER.            EL315
01081      MOVE    '-'                    TO  WS-TOTAL-LINE1.           EL315
01082      MOVE    '*** GRAND TOTALS ***' TO  WS-T1-DESCRIPTION.        EL315
01083      MOVE    WS-GT-NUMBER-OF-CLAIMS TO  WS-T1-NUMBER-OF-CLAIMS.   EL315
01084      MOVE    'CLAIMS'               TO  WS-T1-CLAIMS-DESCRIPTION. EL315
01085      MOVE    WS-GT-ORIG-BENEFIT     TO  WS-T1-ORIG-BENEFIT.       EL315
01086      MOVE    WS-GT-FUTURE-RESERVE   TO  WS-T1-FUTURE-RESERVE.     EL315
01087      MOVE    WS-GT-PTC-RESERVE      TO  WS-T1-PTC-RESERVE.        EL315
01088      MOVE    WS-TOTAL-LINE1         TO  PRT.                      EL315
01089      PERFORM WRITE-A-LINE.                                        EL315
01090      MOVE    SPACES                 TO  WS-TOTAL-LINE1.           EL315
01091      MOVE    WS-CARRIER             TO  WS-H3-C.                  EL315
01092                                                                   EL315
01093      IF DTE-OPT-RESERVE-METHOD-AUTH                               EL315
01094          GO TO 3110-BYPASS-GRAND-TOTAL-CIDA.                      EL315
01095                                                                   EL315
01096      MOVE    WS-GT-REM-BENEFIT      TO  WS-T1-REMAINING-BENEFIT.  EL315
01097      MOVE    WS-GT-MANUAL-RESERVE   TO  WS-T1-MANUAL-RESERVE.     EL315
01098 *RTK MOVE    WS-GT-IBNR-RESERVE     TO  WS-T1-IBNR-RESERVE.       EL315
01099 *    MOVE    WS-TOTAL-LINE1         TO  PRT.                      EL315
01100 *    PERFORM WRITE-A-LINE.                                        EL315
01101                                                                   EL315
01102  3110-BYPASS-GRAND-TOTAL-CIDA.                                    EL315
01103                                                                   EL315
01104      MOVE    WS-TOTAL-LINE1         TO  PRT.                      EL315
01105      PERFORM WRITE-A-LINE.                                        EL315
01106      MOVE    ZERO                   TO  WS-PAGE.                  EL315
01107      SET     CT-INDEX              DOWN BY +1.                    EL315
01108      SET     CT-INDEX-MAX           TO  CT-INDEX.                 EL315
01109      SET     STATE-INDEX-MAX        TO  +1.                       EL315
01110      MOVE    +1                     TO  WS-REPORT-SW.             EL315
01111      MOVE    CARRIER-TOTALS-ZERO    TO  CARRIER-TOTALS-ENTRY (30).EL315
01112      PERFORM 4000-PRINT-CARRIER-TOTALS                            EL315
01113              VARYING CT-INDEX FROM +1 BY +1                       EL315
01114              UNTIL CT-INDEX IS GREATER THAN CT-INDEX-MAX.         EL315
01115      SET     CT-INDEX               TO  +30.                      EL315
01116      SET     CTE-NUMBER-OF-STATES (30) TO STATE-INDEX-MAX.        EL315
01117      PERFORM 4000-PRINT-CARRIER-TOTALS.                           EL315
01118                                                                   EL315
01119      GO TO 3900-EXIT.                                             EL315
01120                                                                   EL315
01121      EJECT                                                        EL315
01122  3200-SORT-OUTPUT-PROCEDURE.                                      EL315
01123                                                                   EL315
01124 *    NOTE ******************************************************* EL315
01125 *         *                                                     * EL315
01126 *         *          DETAIL RECORD PROCESSING                   * EL315
01127 *         *******************************************************.EL315
01128                                                                   EL315
01129      IF EX-RECORD-TYPE NOT = 'A'                                  EL315
01130          GO TO 3300-SORT-OUTPUT-PROCEDURE.                        EL315
01131                                                                   EL315
01132      MOVE    EX-AA-STATE              TO  WS-LAST-STATE.          EL315
01133      MOVE    EX-AA-CARRIER            TO  WS-LAST-CARRIER.        EL315
01134                                                                   EL315
01135      IF WS-PRINT-SW = +1                                          EL315
01136          MOVE ZERO    TO  WS-PRINT-SW                             EL315
01137          MOVE '-'     TO  WS-DETAIL1                              EL315
01138      ELSE                                                         EL315
01139          MOVE SPACES  TO  WS-DETAIL1.                             EL315
01140                                                                   EL315
01141      MOVE    EX-AA-STATE              TO  WS-D1-STATE.            EL315
01142      MOVE    EX-AA-ACCOUNT            TO  WS-D1-ACCOUNT.          EL315
01143      MOVE    EX-SA-CLAIM-NO           TO  WS-D1-CLAIM-NO.         EL315
01144      MOVE    EX-SA-CERT-NO            TO  WS-D1-CERT-NO.          EL315
01145      MOVE    EX-AA-CERT-EFF-DT        TO  DC-BIN-DATE-1.          EL315
01146      MOVE    SPACES                   TO  DC-OPTION-CODE.         EL315
01147      PERFORM 8500-DATE-CONVERSION.                                EL315
01148      MOVE    DC-GREG-DATE-1-EDIT      TO  WS-D1-EFFECTIVE-DATE.   EL315
01149      MOVE    EX-AA-ORIG-TERM          TO  WS-D1-ORIGINAL-TERM.    EL315
01150      MOVE    EX-AA-INSURED-ISSUE-AGE  TO  WS-D1-ISSUED-AGE.       EL315
01151                                                                   EL315
01152      IF EX-AA-INCURRED-DT = LOW-VALUES                            EL315
01153          SET     YEAR-INDEX           TO  +1                      EL315
01154      ELSE                                                         EL315
01155          MOVE    EX-AA-INCURRED-DT    TO  DC-BIN-DATE-1           EL315
01156          MOVE    SPACES               TO  DC-OPTION-CODE          EL315
01157          PERFORM 8500-DATE-CONVERSION                             EL315
01158          MOVE    DC-GREG-DATE-1-EDIT  TO  WS-D1-INCURRED-DATE     EL315
01159                                           WS-DATE-WORK            EL315
01160          COMPUTE WS-YEAR = (RUN-YR - WS-DW-YEAR) + 1              EL315
01161          IF  WS-YEAR < 0                                          EL315
01162              COMPUTE WS-YEAR = ((100 + RUN-YR) - WS-DW-YEAR) + 1     CL**3
01163          END-IF                                                   EL315
01164          IF  WS-YEAR GREATER THAN +11                             EL315
01165              SET YEAR-INDEX  TO  +11                              EL315
01166          ELSE                                                     EL315
01167              IF  WS-YEAR LESS THAN +1                             EL315
01168                  SET YEAR-INDEX  TO  +1                           EL315
01169              ELSE                                                 EL315
01170                  SET YEAR-INDEX  TO  WS-YEAR.                     EL315
01171                                                                   EL315
01172      MOVE    EX-AA-BENEFIT-AMT     TO  WS-D1-ORIG-BENEFIT.        EL315
01173                                                                   EL315
01174 ****  UTILIZING THE ALTERNATE RESERVING METHOD REQUIRES A  ****   EL315
01175 ****  CHANGE IN THE REPORT FORMAT.                         ****   EL315
01176                                                                   EL315
01177      IF DTE-OPT-RESERVE-METHOD-AUTH                               EL315
01178          GO  TO 3260-BUILD-CIDA-REPORT.                           EL315
01179                                                                   EL315
01180 ****  UTILIZING THE ALTERNATE RESERVING METHOD REQUIRES A  ****   EL315
01181 ****  CHANGE IN THE REPORT FORMAT.                         ****   EL315
01182                                                                   EL315
01183      IF  EX-AA-CLAIM-TYPE =         AH-OVERRIDE-L1                EL315
01184          MOVE EX-AA-CDT-FACTOR  TO  WS-D1-FACTOR                  EL315
01185      ELSE                                                         EL315
01186          MOVE LIFE-OVERRIDE-L6  TO  WS-D1-LIFE.                   EL315
01187                                                                   EL315
01188      MOVE  EX-AA-FUTURE-RESERVE      TO WS-D1-FUTURE-RESERVE.     EL315
01189      MOVE  EX-AA-FUTURE-RESERVE-FLAG TO WS-D1-FUTURE-RESERVE-FLAG.EL315
01190      MOVE  EX-AA-PAY-CURRENT-RESERVE TO WS-D1-PAY-TO-CURR-RESERVE.EL315
01191      MOVE  WS-DETAIL1                TO PRT.                      EL315
01192      PERFORM WRITE-A-LINE.                                        EL315
01193      MOVE  SPACES                    TO WS-DETAIL1.               EL315
01194                                                                   EL315
01195      IF EX-AA-PAID-THRU-DT NOT EQUAL LOW-VALUES                   EL315
01196         IF DTE-CLAIM-PAID-THRU-TO EQUAL ' '                       EL315
01197            MOVE    EX-AA-PAID-THRU-DT   TO  DC-BIN-DATE-1         EL315
01198            MOVE    SPACES               TO  DC-OPTION-CODE        EL315
01199            PERFORM 8500-DATE-CONVERSION                           EL315
01200            MOVE    DC-GREG-DATE-1-EDIT  TO  WS-D1-PAID-THRU-DATE  EL315
01201         ELSE                                                      EL315
01202            MOVE    EX-AA-PAID-THRU-DT   TO  DC-BIN-DATE-1         EL315
01203            MOVE    '6'                  TO  DC-OPTION-CODE        EL315
01204            MOVE +1                      TO DC-ELAPSED-DAYS        EL315
01205            MOVE +0                      TO DC-ELAPSED-MONTHS      EL315
01206            PERFORM 8500-DATE-CONVERSION                           EL315
01207            MOVE    DC-GREG-DATE-1-EDIT  TO  WS-D1-PAID-THRU-DATE. EL315
01208                                                                   EL315
01209      MOVE EX-AA-REMAINING-TERM  TO  WS-D1-REMAINING-TERM.         EL315
01210                                                                   EL315
01211      IF DTE-CLIENT = 'CSL'                                        EL315
01212          IF EX-AA-INSURED-BIRTH-DT NOT = LOW-VALUES  AND          EL315
01213             EX-AA-INCURRED-DT NOT = LOW-VALUES                    EL315
01214              MOVE EX-AA-INSURED-BIRTH-DT TO DC-BIN-DATE-1         EL315
01215              MOVE EX-AA-INCURRED-DT      TO DC-BIN-DATE-2         EL315
01216              MOVE '1'                    TO DC-OPTION-CODE        EL315
01217              PERFORM 8500-DATE-CONVERSION                         EL315
01218              COMPUTE WS-INCURRED-AGE = DC-ELAPSED-MONTHS / 12     EL315
01219              MOVE WS-INCURRED-AGE        TO WS-D1-INCURRED-AGE    EL315
01220          ELSE                                                     EL315
01221              MOVE EX-AA-INCURRED-AGE     TO WS-D1-INCURRED-AGE    EL315
01222                                             WS-INCURRED-AGE.      EL315
01223                                                                   EL315
01224      IF DTE-CLIENT NOT = 'CSL'                                    EL315
01225          MOVE EX-AA-INCURRED-AGE   TO WS-D1-INCURRED-AGE          EL315
01226                                       WS-INCURRED-AGE.            EL315
01227                                                                   EL315
01228 *    IF    EX-AA-INSURED-BIRTH-DT NOT = LOW-VALUES                EL315
01229 *      AND EX-AA-INCURRED-DT NOT = LOW-VALUES                     EL315
01230 *          MOVE    EX-AA-INSURED-BIRTH-DT TO DC-BIN-DATE-1        EL315
01231 *          MOVE    EX-AA-INCURRED-DT      TO  DC-BIN-DATE-2       EL315
01232 *          MOVE    '1'                    TO  DC-OPTION-CODE      EL315
01233 *          PERFORM 8500-DATE-CONVERSION                           EL315
01234 *          DIVIDE  DC-ELAPSED-MONTHS BY +12 GIVING WS-INCURRED-AGEEL315
01235 *          MOVE    WS-INCURRED-AGE        TO  WS-D1-INCURRED-AGE. EL315
01236                                                                   EL315
01237      IF EX-AA-REPORTED-DT NOT = LOW-VALUES                        EL315
01238          MOVE    EX-AA-REPORTED-DT    TO  DC-BIN-DATE-1           EL315
01239          MOVE    SPACES               TO  DC-OPTION-CODE          EL315
01240          PERFORM 8500-DATE-CONVERSION                             EL315
01241          MOVE    DC-GREG-DATE-1-EDIT  TO  WS-D1-REPORTED-DATE.    EL315
01242                                                                   EL315
01243      IF EX-AA-CLAIM-TYPE = AH-OVERRIDE-L1                         EL315
01244          MULTIPLY EX-AA-BENEFIT-AMT BY EX-AA-REMAINING-TERM       EL315
01245                   GIVING EX-AA-REMAINING-BENEFIT.                 EL315
01246                                                                   EL315
01247      MOVE EX-AA-REMAINING-BENEFIT TO  WS-D1-REMAINING-BENEFIT.    EL315
01248                                                                   EL315
01249      IF EX-AA-CLAIM-TYPE =         AH-OVERRIDE-L1                 EL315
01250          MOVE EX-AA-CDT-TABLE  TO  WS-D1-CDT-TABLE.               EL315
01251                                                                   EL315
01252 *RTK MOVE EX-AA-IBNR-RESERVE   TO  WS-D1-IBNR-RESERVE.            EL315
01253      MOVE EX-AA-MANUAL-RESERVE TO  WS-D1-MANUAL-RESERVE.          EL315
01254                                                                   EL315
01255      IF EX-AA-CERT-STATUS EQUAL '9'                               EL315
01256         GO TO 3250-WRITE-PRINT.                                   EL315
01257                                                                   EL315
01258      ADD EX-AA-MANUAL-RESERVE  TO WS-AT-MANUAL-RESERVE            EL315
01259                                   CT-MANUAL-BY-YEAR (YEAR-INDEX). EL315
01260      ADD EX-AA-IBNR-RESERVE    TO WS-AT-IBNR-RESERVE              EL315
01261                                   CT-IBNR-BY-YEAR (YEAR-INDEX).   EL315
01262      ADD EX-AA-REMAINING-BENEFIT TO WS-AT-REM-BENEFIT.            EL315
01263                                                                   EL315
01264      IF    EX-AA-INSURED-BIRTH-DT NOT = LOW-VALUES                EL315
01265        AND EX-AA-INCURRED-DT      NOT = LOW-VALUES                EL315
01266            ADD +1               TO  WS-AT-INCURRED-AGE-CNT        EL315
01267            ADD WS-INCURRED-AGE  TO  WS-AT-INCURRED-AGE.           EL315
01268                                                                   EL315
01269      ADD EX-AA-REMAINING-TERM      TO WS-AT-REMAINING-TERM.       EL315
01270      ADD EX-AA-BENEFIT-AMT         TO WS-AT-ORIG-BENEFIT.         EL315
01271                                                                   EL315
01272      ADD EX-AA-FUTURE-RESERVE    TO  WS-AT-FUTURE-RESERVE         EL315
01273                                    CT-FUTURE-BY-YEAR (YEAR-INDEX).EL315
01274                                                                   EL315
01275      ADD EX-AA-PAY-CURRENT-RESERVE TO WS-AT-PTC-RESERVE           EL315
01276                                       CT-PTC-BY-YEAR (YEAR-INDEX).EL315
01277      ADD EX-AA-INSURED-ISSUE-AGE   TO WS-AT-ISSUE-AGE.            EL315
01278      ADD EX-AA-ORIG-TERM           TO WS-AT-ORIG-TERM.            EL315
01279      ADD +1                        TO WS-AT-NUMBER-OF-CLAIMS.     EL315
01280                                                                   EL315
01281  3250-WRITE-PRINT.                                                EL315
01282                                                                   EL315
01283      MOVE    WS-DETAIL1   TO  PRT.                                EL315
01284      PERFORM WRITE-A-LINE.                                        EL315
01285      GO TO 3100-SORT-OUTPUT-PROCEDURE.                               CL**2
01286                                                                   EL315
01287      EJECT                                                        EL315
01288  3260-BUILD-CIDA-REPORT.                                          EL315
01289                                                                   EL315
01290 ****  UTILIZING THE ALTERNATE RESERVING METHOD REQUIRES A  ****   EL315
01291 ****  CHANGE IN THE REPORT FORMAT.                         ****   EL315
01292                                                                   EL315
01293      IF  EX-AA-CLAIM-TYPE =         AH-OVERRIDE-L1                EL315
01294          MOVE EX-AA-CIDA-FACTOR TO  WS-D1-FACTOR                  EL315
01295      ELSE                                                         EL315
01296          MOVE LIFE-OVERRIDE-L6  TO  WS-D1-LIFE.                   EL315
01297                                                                   EL315
01298      MOVE  EX-AA-FUTURE-RSV-OPT      TO WS-D1-FUTURE-RESERVE.     EL315
01299      MOVE  EX-AA-FUTURE-RSV-OPT-FLAG TO WS-D1-FUTURE-RESERVE-FLAG.EL315
01300      MOVE  EX-AA-PAY-CURRENT-RSV-OPT TO WS-D1-PAY-TO-CURR-RESERVE.EL315
01301      MOVE  WS-DETAIL1                TO PRT.                      EL315
01302      PERFORM WRITE-A-LINE.                                        EL315
01303      MOVE  SPACES                    TO WS-DETAIL1.               EL315
01304                                                                   EL315
01305      IF EX-AA-PAID-THRU-DT NOT EQUAL LOW-VALUES                   EL315
01306         IF DTE-CLAIM-PAID-THRU-TO EQUAL ' '                       EL315
01307            MOVE    EX-AA-PAID-THRU-DT   TO  DC-BIN-DATE-1         EL315
01308            MOVE    SPACES               TO  DC-OPTION-CODE        EL315
01309            PERFORM 8500-DATE-CONVERSION                           EL315
01310            MOVE    DC-GREG-DATE-1-EDIT  TO  WS-D1-PAID-THRU-DATE  EL315
01311         ELSE                                                      EL315
01312            MOVE    EX-AA-PAID-THRU-DT   TO  DC-BIN-DATE-1         EL315
01313            MOVE    '6'                  TO  DC-OPTION-CODE        EL315
01314            MOVE +1                      TO DC-ELAPSED-DAYS        EL315
01315            MOVE +0                      TO DC-ELAPSED-MONTHS      EL315
01316            PERFORM 8500-DATE-CONVERSION                           EL315
01317            MOVE    DC-GREG-DATE-1-EDIT  TO  WS-D1-PAID-THRU-DATE. EL315
01318                                                                   EL315
01319      MOVE EX-AA-REMAINING-TERM  TO  WS-D1-REMAINING-TERM.         EL315
01320                                                                   EL315
01321      IF DTE-CLIENT = 'CSL'                                        EL315
01322          IF EX-AA-INSURED-BIRTH-DT NOT = LOW-VALUES  AND          EL315
01323             EX-AA-INCURRED-DT NOT = LOW-VALUES                    EL315
01324              MOVE EX-AA-INSURED-BIRTH-DT TO DC-BIN-DATE-1         EL315
01325              MOVE EX-AA-INCURRED-DT      TO DC-BIN-DATE-2         EL315
01326              MOVE '1'                    TO DC-OPTION-CODE        EL315
01327              PERFORM 8500-DATE-CONVERSION                         EL315
01328              COMPUTE WS-INCURRED-AGE = DC-ELAPSED-MONTHS / 12     EL315
01329              MOVE WS-INCURRED-AGE        TO WS-D1-INCURRED-AGE    EL315
01330          ELSE                                                     EL315
01331              MOVE EX-AA-INCURRED-AGE     TO WS-D1-INCURRED-AGE    EL315
01332                                             WS-INCURRED-AGE.      EL315
01333                                                                   EL315
01334      IF DTE-CLIENT NOT = 'CSL'                                    EL315
01335          MOVE EX-AA-INCURRED-AGE   TO WS-D1-INCURRED-AGE          EL315
01336                                       WS-INCURRED-AGE.            EL315
01337                                                                   EL315
01338 *    IF    EX-AA-INSURED-BIRTH-DT NOT = LOW-VALUES                EL315
01339 *      AND EX-AA-INCURRED-DT NOT = LOW-VALUES                     EL315
01340 *          MOVE    EX-AA-INSURED-BIRTH-DT TO DC-BIN-DATE-1        EL315
01341 *          MOVE    EX-AA-INCURRED-DT      TO  DC-BIN-DATE-2       EL315
01342 *          MOVE    '1'                    TO  DC-OPTION-CODE      EL315
01343 *          PERFORM 8500-DATE-CONVERSION                           EL315
01344 *          DIVIDE  DC-ELAPSED-MONTHS BY +12 GIVING WS-INCURRED-AGEEL315
01345 *          MOVE    WS-INCURRED-AGE        TO  WS-D1-INCURRED-AGE. EL315
01346                                                                   EL315
01347      IF EX-AA-REPORTED-DT NOT = LOW-VALUES                        EL315
01348          MOVE    EX-AA-REPORTED-DT    TO  DC-BIN-DATE-1           EL315
01349          MOVE    SPACES               TO  DC-OPTION-CODE          EL315
01350          PERFORM 8500-DATE-CONVERSION                             EL315
01351          MOVE    DC-GREG-DATE-1-EDIT  TO  WS-D1-REPORTED-DATE.    EL315
01352                                                                   EL315
01353      IF EX-AA-CERT-STATUS EQUAL '9'                               EL315
01354         GO TO 3270-CIDA-WRITE-PRINT.                              EL315
01355                                                                   EL315
01356      ADD EX-AA-MANUAL-RSV-OPT  TO WS-AT-MANUAL-RESERVE            EL315
01357                                   CT-MANUAL-BY-YEAR (YEAR-INDEX). EL315
01358      MOVE ZEROS                TO WS-AT-IBNR-RESERVE              EL315
01359                                   CT-IBNR-BY-YEAR (YEAR-INDEX).   EL315
01360      ADD EX-AA-REMAINING-BENEFIT TO WS-AT-REM-BENEFIT.            EL315
01361                                                                   EL315
01362      IF    EX-AA-INSURED-BIRTH-DT NOT = LOW-VALUES                EL315
01363        AND EX-AA-INCURRED-DT      NOT = LOW-VALUES                EL315
01364            ADD +1               TO  WS-AT-INCURRED-AGE-CNT        EL315
01365            ADD WS-INCURRED-AGE  TO  WS-AT-INCURRED-AGE.           EL315
01366                                                                   EL315
01367      ADD EX-AA-REMAINING-TERM      TO WS-AT-REMAINING-TERM.       EL315
01368      ADD EX-AA-BENEFIT-AMT         TO WS-AT-ORIG-BENEFIT.         EL315
01369                                                                   EL315
01370      ADD EX-AA-FUTURE-RSV-OPT     TO  WS-AT-FUTURE-RESERVE        EL315
01371                                   CT-FUTURE-BY-YEAR (YEAR-INDEX). EL315
01372                                                                   EL315
01373      ADD EX-AA-PAY-CURRENT-RSV-OPT                                EL315
01374          TO WS-AT-PTC-RESERVE                                     EL315
01375             CT-PTC-BY-YEAR (YEAR-INDEX).                          EL315
01376                                                                   EL315
01377      IF  EX-AA-CLAIM-TYPE EQUAL AH-OVERRIDE-L1                    EL315
01378          ADD EX-AA-PAY-CURRENT-RSV-OPT                            EL315
01379              TO WS-AT-PTC-RESERVE-AH                              EL315
01380                 CT-PTC-BY-YEAR-AH (YEAR-INDEX)                    EL315
01381      ELSE                                                         EL315
01382          ADD EX-AA-PAY-CURRENT-RSV-OPT                            EL315
01383              TO WS-AT-PTC-RESERVE-LF                              EL315
01384                 CT-PTC-BY-YEAR-LF (YEAR-INDEX).                   EL315
01385                                                                   EL315
01386      ADD EX-AA-INSURED-ISSUE-AGE   TO WS-AT-ISSUE-AGE.            EL315
01387      ADD EX-AA-ORIG-TERM           TO WS-AT-ORIG-TERM.            EL315
01388      ADD +1                        TO WS-AT-NUMBER-OF-CLAIMS.     EL315
01389                                                                   EL315
01390  3270-CIDA-WRITE-PRINT.                                           EL315
01391                                                                   EL315
01392      MOVE    WS-DETAIL1   TO  PRT.                                EL315
01393      PERFORM WRITE-A-LINE.                                        EL315
01394      GO TO 3100-SORT-OUTPUT-PROCEDURE.                            EL315
01395                                                                   EL315
01396      EJECT                                                        EL315
01397  3300-SORT-OUTPUT-PROCEDURE.                                      EL315
01398                                                                   EL315
01399      MOVE SPACES                TO WS-DETAIL1.                    EL315
01400      MOVE EX-AB-ADDL-RESERVE    TO WS-D1-MANUAL-RESERVE.          EL315
01401                                                                   EL315
01402      IF EX-AB-CERT-STATUS EQUAL '9'                               EL315
01403         GO TO 3350-WRITE-PRINT.                                   EL315
01404                                                                   EL315
01405      ADD EX-AB-ADDL-RESERVE     TO WS-AT-MANUAL-RESERVE           EL315
01406                                    CT-MANUAL-BY-YEAR (YEAR-INDEX).EL315
01407                                                                   EL315
01408  3350-WRITE-PRINT.                                                EL315
01409      MOVE    WS-DETAIL1   TO PRT.                                 EL315
01410      PERFORM WRITE-A-LINE.                                        EL315
01411      GO TO 3100-SORT-OUTPUT-PROCEDURE.                            EL315
01412                                                                   EL315
01413  3900-EXIT.                                                       EL315
01414      EXIT.                                                        EL315
01415                                                                   EL315
01416      EJECT                                                        EL315
01417  4000-PRINT-CARRIER-TOTALS SECTION.                               EL315
01418                                                                   EL315
01419      MOVE CARRIER-TOTALS-ENTRY (CT-INDEX)  TO  CARRIER-TOTALS.    EL315
01420                                                                   EL315
01421      IF CT-CARRIER NOT = WS-H3-CARRIER                            EL315
01422          PERFORM 8100-GET-CARRIER-NAME.                           EL315
01423                                                                   EL315
01424      MOVE +99         TO  WS-LINE-COUNT.                          EL315
01425      IF  WS-RPT EQUAL 'A'                                         EL315
01426          MOVE 'A'     TO  WS-H1-REPORT-TYPE                       EL315
01427      ELSE                                                         EL315
01428          MOVE 'B'     TO  WS-H1-REPORT-TYPE.                      EL315
01429      MOVE SPACE       TO  WS-RPT.                                 EL315
01430      SET  YEAR-INDEX                                              EL315
01431           STATE-INDEX TO  +1.                                     EL315
01432                                                                   EL315
01433  4100-PRINT-CARRIER-TOTALS.                                       EL315
01434      MOVE SPACES  TO  WS-DETAIL2.                                 EL315
01435                                                                   EL315
01436      IF   YEAR-INDEX GREATER THAN +13                             EL315
01437        OR YEAR-INDEX =            +12                             EL315
01438           GO TO 4200-PRINT-CARRIER-TOTALS.                        EL315
01439                                                                   EL315
01440      IF  YEAR-INDEX = +1                                          EL315
01441          MOVE RUN-CC  TO  WS-D2-CENTURY                           EL315
01442          MOVE RUN-YR  TO  WS-D2-YR                                EL315
01443      ELSE                                                         EL315
01444          IF  YEAR-INDEX = +11                                     EL315
01445              MOVE 'PRIOR'  TO  WS-D2-YEAR                         EL315
01446          ELSE                                                     EL315
01447              IF  YEAR-INDEX =         +13                         EL315
01448                  MOVE 'TOTAL'     TO  WS-D2-YEAR                  EL315
01449                  SET  YEAR-INDEX  TO  +12                         EL315
01450              ELSE                                                 EL315
01451                  MOVE     ' -'    TO  WS-D2-CENTURY               EL315
01452                  SET      WS-YEAR TO  YEAR-INDEX                  EL315
01453                  SUBTRACT +1     FROM WS-YEAR GIVING WS-D2-YR2.   EL315
01454                                                                   EL315
01455      IF  YEAR-INDEX NOT = +12                                     EL315
01456          ADD   CT-MANUAL-BY-YEAR (YEAR-INDEX)                     EL315
01457             TO CT-MANUAL-BY-YEAR (12)                             EL315
01458          ADD   CT-FUTURE-BY-YEAR (YEAR-INDEX)                     EL315
01459             TO CT-FUTURE-BY-YEAR (12)                             EL315
01460          ADD   CT-PTC-BY-YEAR    (YEAR-INDEX)                     EL315
01461             TO CT-PTC-BY-YEAR    (12)                             EL315
01462          ADD   CT-PTC-BY-YEAR-AH (YEAR-INDEX)                     EL315
01463             TO CT-PTC-BY-YEAR-AH (12)                             EL315
01464          ADD   CT-PTC-BY-YEAR-LF (YEAR-INDEX)                     EL315
01465             TO CT-PTC-BY-YEAR-LF (12)                             EL315
01466          ADD   CT-IBNR-BY-YEAR   (YEAR-INDEX)                     EL315
01467             TO CT-IBNR-BY-YEAR   (12)                             EL315
01468          IF  WS-REPORT-SW = +1                                    EL315
01469              ADD CT-MANUAL-BY-YEAR (YEAR-INDEX)                   EL315
01470                 TO CTE-MANUAL-BY-YEAR (CT-INDEX2, YEAR-INDEX)     EL315
01471              ADD CT-FUTURE-BY-YEAR (YEAR-INDEX)                   EL315
01472                 TO CTE-FUTURE-BY-YEAR (CT-INDEX2, YEAR-INDEX)     EL315
01473              ADD CT-PTC-BY-YEAR (YEAR-INDEX)                      EL315
01474                 TO CTE-PTC-BY-YEAR (CT-INDEX2, YEAR-INDEX)        EL315
01475              ADD CT-PTC-BY-YEAR-AH (YEAR-INDEX)                   EL315
01476                 TO CTE-PTC-BY-YEAR-AH (CT-INDEX2, YEAR-INDEX)     EL315
01477              ADD CT-PTC-BY-YEAR-LF (YEAR-INDEX)                   EL315
01478                 TO CTE-PTC-BY-YEAR-LF (CT-INDEX2, YEAR-INDEX)     EL315
01479              ADD CT-IBNR-BY-YEAR (YEAR-INDEX)                     EL315
01480                 TO CTE-IBNR-BY-YEAR (CT-INDEX2, YEAR-INDEX).      EL315
01481                                                                   EL315
01482      MOVE CT-FUTURE-BY-YEAR (YEAR-INDEX) TO WS-D2-YEAR-FUTURE.    EL315
01483      MOVE CT-PTC-BY-YEAR (YEAR-INDEX)    TO WS-D2-YEAR-PTC.       EL315
01484                                                                   EL315
01485      IF  DTE-OPT-RESERVE-METHOD-AUTH                              EL315
01486          MOVE CT-PTC-BY-YEAR-AH (YEAR-INDEX)                      EL315
01487                                          TO WS-D2-YEAR-PTC-AH     EL315
01488          MOVE CT-PTC-BY-YEAR-LF (YEAR-INDEX)                      EL315
01489                                          TO WS-D2-YEAR-PTC-LF     EL315
01490      ELSE                                                         EL315
01491          MOVE CT-MANUAL-BY-YEAR (YEAR-INDEX)                      EL315
01492                                          TO WS-D2-YEAR-MANUAL     EL315
01493          MOVE CT-IBNR-BY-YEAR (YEAR-INDEX)                        EL315
01494                                          TO WS-D2-YEAR-IBNR.      EL315
01495                                                                   EL315
01496      IF WS-D2-YEAR = 'TOTAL'                                      EL315
01497          SET YEAR-INDEX TO +13.                                   EL315
01498                                                                   EL315
01499      EJECT                                                        EL315
01500  4200-PRINT-CARRIER-TOTALS.                                       EL315
01501      IF   STATE-INDEX GREATER THAN (CT-NUMBER-OF-STATES + 2)      EL315
01502        OR STATE-INDEX =            (CT-NUMBER-OF-STATES + 1)      EL315
01503           GO TO 4300-PRINT-CARRIER-TOTALS.                        EL315
01504                                                                   EL315
01505      IF  STATE-INDEX = (CT-NUMBER-OF-STATES + 2)                  EL315
01506          SET  STATE-INDEX  TO +60                                 EL315
01507          MOVE 'TOTAL'      TO WS-D2-TOTAL                         EL315
01508      ELSE                                                         EL315
01509          MOVE CT-STATE (STATE-INDEX)  TO  WS-D2-STATE.            EL315
01510                                                                   EL315
01511      MOVE CT-FUTURE-BY-STATE (STATE-INDEX)                        EL315
01512                                            TO WS-D2-STATE-FUTURE. EL315
01513      MOVE CT-PTC-BY-STATE    (STATE-INDEX) TO WS-D2-STATE-PTC.    EL315
01514                                                                   EL315
01515      IF  DTE-OPT-RESERVE-METHOD-AUTH                              EL315
01516 *        MOVE SPACES                     TO WS-D2-STATE-IBNR-X    EL315
01517 *                                           WS-D2-STATE-MAN-X     EL315
01518          MOVE CT-PTC-BY-STATE-AH (STATE-INDEX)                    EL315
01519                                          TO WS-D2-STATE-PTC-AH    EL315
01520          MOVE CT-PTC-BY-STATE-LF (STATE-INDEX)                    EL315
01521                                          TO WS-D2-STATE-PTC-LF    EL315
01522      ELSE                                                         EL315
01523          MOVE CT-MANUAL-BY-STATE (STATE-INDEX)                    EL315
01524                                          TO WS-D2-STATE-MANUAL    EL315
01525          MOVE CT-IBNR-BY-STATE (STATE-INDEX)                      EL315
01526                                          TO WS-D2-STATE-IBNR.     EL315
01527                                                                   EL315
01528      IF  STATE-INDEX NOT = +60                                    EL315
01529          ADD CT-MANUAL-BY-STATE (STATE-INDEX)                     EL315
01530                                  TO  CT-MANUAL-BY-STATE (60)      EL315
01531          ADD CT-FUTURE-BY-STATE (STATE-INDEX)                     EL315
01532                                  TO  CT-FUTURE-BY-STATE (60)      EL315
01533          ADD CT-PTC-BY-STATE (STATE-INDEX)                        EL315
01534                                  TO  CT-PTC-BY-STATE (60)         EL315
01535          ADD CT-PTC-BY-STATE-AH (STATE-INDEX)                     EL315
01536                                  TO  CT-PTC-BY-STATE-AH (60)      EL315
01537          ADD CT-PTC-BY-STATE-LF (STATE-INDEX)                     EL315
01538                                  TO  CT-PTC-BY-STATE-LF (60)      EL315
01539          ADD CT-IBNR-BY-STATE (STATE-INDEX)                       EL315
01540                                  TO  CT-IBNR-BY-STATE (60)        EL315
01541          IF  WS-REPORT-SW EQUAL  TO +1                            EL315
01542              PERFORM 5000-ACCUMULATE-STATE-TOTALS.                EL315
01543                                                                   EL315
01544      EJECT                                                        EL315
01545  4300-PRINT-CARRIER-TOTALS.                                       EL315
01546      MOVE    WS-DETAIL2     TO  PRT.                              EL315
01547      PERFORM WRITE-A-LINE.                                        EL315
01548      SET     YEAR-INDEX                                           EL315
01549              STATE-INDEX UP BY  +1.                               EL315
01550                                                                   EL315
01551      IF  W-TIMES-COUNTER EQUAL 1000                               EL315
01552          MOVE    '4300-LOOP'     TO WS-ABEND-MESSAGE              EL315
01553          GO TO ABEND-PGM                                          EL315
01554      ELSE                                                         EL315
01555          ADD +1                  TO W-TIMES-COUNTER.              EL315
01556                                                                   EL315
01557      IF    YEAR-INDEX  GREATER THAN +13                           EL315
01558        AND STATE-INDEX GREATER THAN +60                           EL315
01559            NEXT SENTENCE                                          EL315
01560        ELSE                                                       EL315
01561            GO TO 4100-PRINT-CARRIER-TOTALS.                       EL315
01562                                                                   EL315
01563      MOVE +99  TO  WS-LINE-COUNT.                                 EL315
01564      MOVE 'A'  TO  WS-H1-REPORT-TYPE.                             EL315
01565                                                                   EL315
01566  4900-EXIT.                                                       EL315
01567      EXIT.                                                        EL315
01568                                                                   EL315
01569      EJECT                                                        EL315
01570  5000-ACCUMULATE-STATE-TOTALS SECTION.                            EL315
01571                                                                   EL315
01572 *    NOTE ******************************************************* EL315
01573 *         *                                                     * EL315
01574 *         *      THIS SECTION ACCUMULATES THE TOTALS BY STATE   * EL315
01575 *         *  FOR PRINTING AT FINAL TOTAL TIME.                  * EL315
01576 *         *                                                     * EL315
01577 *         *      SINCE THE STATES MUST BE PRINTED IN SEQUENCE   * EL315
01578 *         *  THE FOLLOWING LOGIC IS USED:                       * EL315
01579 *         *                                                     * EL315
01580 *         *      1.  SEARCH THE STATE ARRAY TO SEE IF THE STATE * EL315
01581 *         *          ALREADY EXIST, IF THE STATE ALREADY EXIST  * EL315
01582 *         *          ACCUMULATE THE TOTALS AND EXIT.            * EL315
01583 *         *                                                     * EL315
01584 *         *      2.  IF THE STATE IS GREATER THAN THE LAST      * EL315
01585 *         *          ADD THE STATE TO THE END OF THE TABLE,     * EL315
01586 *         *          ACCUMULATE THE TOTALS AND EXIT.            * EL315
01587 *         *                                                     * EL315
01588 *         *      3.  MOVE THE ENTRY UP ONE, ZERO THE ENTRY,     * EL315
01589 *         *          SET THE INDEXES DOWN BY 1 AND GO TO STEP   * EL315
01590 *         *          TWO.                                       * EL315
01591 *         *                                                     * EL315
01592 *         *******************************************************.EL315
01593                                                                   EL315
01594      SET STATE-INDEX2 TO +1                                       EL315
01595                                                                   EL315
01596      IF  W-STATE-FIRST-TIME                                       EL315
01597          MOVE 'N'                TO W-STATE-FIRST-TIME-IND        EL315
01598          MOVE CT-STATE  (STATE-INDEX)                             EL315
01599              TO CTE-STATE (CT-INDEX2, STATE-INDEX2).              EL315
01600                                                                   EL315
01601  5100-ACCUMULATE-STATE-TOTALS.                                    EL315
01602      IF  CT-STATE (STATE-INDEX)                                   EL315
01603              = CTE-STATE (CT-INDEX2, STATE-INDEX2)                EL315
01604          ADD CT-MANUAL-BY-STATE (STATE-INDEX)                     EL315
01605             TO CTE-MANUAL-BY-STATE (CT-INDEX2, STATE-INDEX2)      EL315
01606          ADD CT-FUTURE-BY-STATE (STATE-INDEX)                     EL315
01607             TO CTE-FUTURE-BY-STATE (CT-INDEX2, STATE-INDEX2)      EL315
01608          ADD CT-PTC-BY-STATE (STATE-INDEX)                        EL315
01609             TO CTE-PTC-BY-STATE (CT-INDEX2, STATE-INDEX2)         EL315
01610          ADD CT-PTC-BY-STATE-AH (STATE-INDEX)                     EL315
01611             TO CTE-PTC-BY-STATE-AH (CT-INDEX2, STATE-INDEX2)      EL315
01612          ADD CT-PTC-BY-STATE-LF (STATE-INDEX)                     EL315
01613             TO CTE-PTC-BY-STATE-LF (CT-INDEX2, STATE-INDEX2)      EL315
01614          ADD CT-IBNR-BY-STATE (STATE-INDEX)                       EL315
01615             TO CTE-IBNR-BY-STATE (CT-INDEX2, STATE-INDEX2)        EL315
01616          GO TO 5900-EXIT.                                         EL315
01617                                                                   EL315
01618      IF STATE-INDEX2 LESS THAN STATE-INDEX-MAX                    EL315
01619          SET STATE-INDEX2 UP BY +1                                EL315
01620          GO TO 5100-ACCUMULATE-STATE-TOTALS.                      EL315
01621                                                                   EL315
01622      SET STATE-INDEX-MAX UP BY +1.                                EL315
01623      SET STATE-INDEX3    TO STATE-INDEX-MAX.                      EL315
01624                                                                   EL315
01625  5200-ACCUMULATE-STATE-TOTALS.                                    EL315
01626      IF  CT-STATE  (STATE-INDEX) GREATER THAN                     EL315
01627          CTE-STATE (CT-INDEX2, STATE-INDEX2)                      EL315
01628          MOVE CT-STATE (STATE-INDEX) TO                           EL315
01629               CTE-STATE (CT-INDEX2, STATE-INDEX3)                 EL315
01630          SET  STATE-INDEX2 TO STATE-INDEX3                        EL315
01631          GO TO 5100-ACCUMULATE-STATE-TOTALS.                      EL315
01632                                                                   EL315
01633      MOVE CTE-RESERVES-BY-STATE (CT-INDEX2, STATE-INDEX2) TO      EL315
01634           CTE-RESERVES-BY-STATE (CT-INDEX2, STATE-INDEX3).        EL315
01635                                                                   EL315
01636      MOVE SPACE TO  CTE-STATE           (CT-INDEX2, STATE-INDEX2).EL315
01637      MOVE ZERO  TO  CTE-MANUAL-BY-STATE (CT-INDEX2, STATE-INDEX2) EL315
01638                     CTE-FUTURE-BY-STATE (CT-INDEX2, STATE-INDEX2) EL315
01639                     CTE-PTC-BY-STATE    (CT-INDEX2, STATE-INDEX2) EL315
01640                     CTE-PTC-BY-STATE-AH (CT-INDEX2, STATE-INDEX2) EL315
01641                     CTE-PTC-BY-STATE-LF (CT-INDEX2, STATE-INDEX2) EL315
01642                     CTE-IBNR-BY-STATE   (CT-INDEX2, STATE-INDEX2).EL315
01643                                                                   EL315
01644      IF STATE-INDEX2 GREATER THAN ZERO                            EL315
01645          SET STATE-INDEX2                                         EL315
01646              STATE-INDEX3 DOWN BY +1                              EL315
01647          GO TO 5200-ACCUMULATE-STATE-TOTALS.                      EL315
01648                                                                   EL315
01649      MOVE CT-STATE  (STATE-INDEX) TO                              EL315
01650           CTE-STATE (CT-INDEX2, STATE-INDEX2)                     EL315
01651                                                                   EL315
01652      GO TO 5100-ACCUMULATE-STATE-TOTALS.                          EL315
01653                                                                   EL315
01654  5900-EXIT.                                                       EL315
01655      EXIT.                                                        EL315
01656                                                                   EL315
01657      EJECT                                                        EL315
01658  8100-GET-CARRIER-NAME SECTION.                                   EL315
01659                                                                   EL315
01660      MOVE +1           TO  WS-INDEX.                              EL315
01661      MOVE CT-CARRIER   TO  WS-H3-CARRIER.                         EL315
01662                                                                   EL315
01663      IF CT-CARRIER = SPACES                                       EL315
01664          MOVE SPACES                  TO  WS-H3-C                 EL315
01665          MOVE '*** GRAND TOTALS ***'  TO  WS-H4-CARRIER-NAME      EL315
01666          GO TO 8190-EXIT.                                         EL315
01667                                                                   EL315
01668  8110-GET-CARRIER-NAME.                                           EL315
01669      IF  CT-CARRIER = CARRIER-SUB (WS-INDEX)                      EL315
01670          MOVE CARRIER-PIC (WS-INDEX) TO WS-H4-CARRIER-NAME        EL315
01671      ELSE                                                         EL315
01672          IF  WS-INDEX LESS THAN +25                               EL315
01673              ADD +1  TO  WS-INDEX                                 EL315
01674              GO TO 8110-GET-CARRIER-NAME.                         EL315
01675                                                                   EL315
01676  8190-EXIT.                                                       EL315
01677      EXIT.                                                        EL315
01678                                                                   EL315
01679      EJECT                                                        EL315
01680  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       EL315
01681                                                                   EL315
01682      EJECT                                                        EL315
01683  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL315
01684                                                                   EL315
01685      EJECT                                                        EL315
01686  WRITE-HEADINGS SECTION.                                          EL315
01687                                                                   EL315
01688  WHS-010.                                                         EL315
01689      IF  WS-H2-DATE EQUAL SPACES                                  EL315
01690          MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   EL315
01691          MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME            EL315
01692          MOVE ALPH-DATE          TO  WS-H3-DATE.                  EL315
01693                                                                   EL315
01694      ADD +1  TO  WS-PAGE.                                         EL315
01695      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL315
01696      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL315
01697      MOVE ZERO                   TO  WS-LINE-COUNT.               EL315
01698                                                                   EL315
01699      MOVE WS-HEADING1            TO  PRT.                         EL315
01700      MOVE '1'                    TO  X.                           EL315
01701      PERFORM WRITE-PRINTER.                                       EL315
01702                                                                   EL315
01703      MOVE WS-HEADING2            TO  PRT.                         EL315
01704      MOVE ' '                    TO  X.                           EL315
01705      PERFORM WRITE-PRINTER.                                       EL315
01706                                                                   EL315
01707      MOVE WS-HEADING3            TO  PRT.                         EL315
01708      MOVE ' '                    TO  X.                           EL315
01709      PERFORM WRITE-PRINTER.                                       EL315
01710                                                                   EL315
01711      MOVE WS-HEADING4            TO  PRT.                         EL315
01712      MOVE ' '                    TO  X.                           EL315
01713      PERFORM WRITE-PRINTER.                                       EL315
01714                                                                   EL315
01715      IF  WS-H1-REPORT-TYPE = 'A'                                  EL315
01716          IF  DTE-OPT-RESERVE-METHOD-AUTH                          EL315
01717              MOVE WS-CL315A-HDG5-CIDA                             EL315
01718                                  TO  PRT                          EL315
01719          ELSE                                                     EL315
01720              MOVE WS-CL315A-HDG5-CDT                              EL315
01721                                  TO  PRT                          EL315
01722      ELSE                                                         EL315
01723          IF  DTE-OPT-RESERVE-METHOD-AUTH                          EL315
01724              MOVE WS-CL315B-HDG5-CIDA                             EL315
01725                                  TO  PRT                          EL315
01726          ELSE                                                     EL315
01727              MOVE WS-CL315B-HDG5-CDT                              EL315
01728                                  TO  PRT.                         EL315
01729                                                                   EL315
01730      PERFORM WRITE-PRINTER.                                       EL315
01731                                                                   EL315
01732      IF  WS-H1-REPORT-TYPE = 'A'                                  EL315
01733          IF  DTE-OPT-RESERVE-METHOD-AUTH                          EL315
01734              MOVE WS-CL315A-HDG6-CIDA                             EL315
01735                                  TO  PRT                          EL315
01736          ELSE                                                     EL315
01737              MOVE WS-CL315A-HDG6-CDT                              EL315
01738                                  TO  PRT                          EL315
01739      ELSE                                                         EL315
01740          IF  DTE-OPT-RESERVE-METHOD-AUTH                          EL315
01741              MOVE WS-CL315B-HDG6-CIDA                             EL315
01742                                  TO  PRT                          EL315
01743          ELSE                                                     EL315
01744              MOVE WS-CL315B-HDG6-CDT                              EL315
01745                                  TO  PRT.                         EL315
01746                                                                   EL315
01747      PERFORM WRITE-PRINTER.                                       EL315
01748                                                                   EL315
01749      MOVE +8  TO  WS-LINE-COUNT.                                  EL315
01750                                                                   EL315
01751  WHS-020. COPY ELCWHS2.                                           EL315
01752                                                                   EL315
01753      EJECT                                                        EL315
01754  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL315
01755                                                                   EL315
01756  WPS-020.                                                         EL315
01757                                                                   EL315
01758      IF DTE-FICH NOT = SPACE AND                                  EL315
01759          FICH-OPEN   = SPACE                                      EL315
01760          MOVE 'X' TO FICH-OPEN                                    EL315
01761          OPEN OUTPUT FICH.                                        EL315
01762                                                                   EL315
01763      IF DTE-PRT-OPT = 'S' OR 'T'                                  EL315
01764          IF (REPT-OPEN = SPACE) AND (DTE-ABEND-CD-1 = SPACE)      EL315
01765              OPEN I-O ELREPT                                      EL315
01766              IF DTE-F-1 NOT = ZERO AND                            EL315
01767                 DTE-VSAM-FLAGS NOT = '97'                         EL315
01768                  MOVE DTE-VSAM-FLAGS  TO  WS-ABEND-FILE-STATUS    EL315
01769                  MOVE 'ERROR OCCURED OPEN - ELREPT'               EL315
01770                                  TO  WS-ABEND-MESSAGE             EL315
01771                  GO TO ABEND-PGM                                  EL315
01772              ELSE                                                 EL315
01773                  MOVE '1'                   TO REPT-OPEN          EL315
01774                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL315
01775                  MOVE '1'                   TO RF-RECORD-TYPE     EL315
01776                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL315
01777                  MOVE ZERO                  TO RF-LINE-NUMBER     EL315
01778                  START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY    EL315
01779                  PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT   EL315
01780                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL315
01781                  MOVE '2'                   TO RF-RECORD-TYPE     EL315
01782                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL315
01783                  MOVE ZERO                  TO RF-LINE-NUMBER     EL315
01784                  START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY    EL315
01785                  PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT   EL315
01786                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL315
01787                  MOVE '1'                   TO RF-RECORD-TYPE     EL315
01788                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL315
01789                  MOVE SPACES                TO RF-REPORT-LINE-133.EL315
01790                                                                   EL315
01791      IF DTE-ABEND-CD-1 = '81' AND                                 EL315
01792         DTE-PRT-OPT    = 'S'                                      EL315
01793          MOVE +0302  TO WS-RETURN-CODE                            EL315
01794          GO TO ABEND-PGM.                                         EL315
01795                                                                   EL315
01796      IF DTE-PRT-OPT = 'S' OR 'T'                                  EL315
01797          MOVE X      TO RF-CTL-CHAR-133                           EL315
01798          MOVE P-DATA TO RF-DATA-133                               EL315
01799              IF DTE-ABEND-CD-1 = SPACES                           EL315
01800                  ADD +1 TO DTE-TOT-LINES                          EL315
01801                  MOVE DTE-TOT-LINES TO RF-LINE-NUMBER             EL315
01802                  WRITE REPORT-SAVE-FILE                           EL315
01803                      INVALID KEY                                  EL315
01804                          MOVE '88' TO DTE-ABEND-CD-1              EL315
01805                          CLOSE ELREPT                             EL315
01806                          MOVE SPACE TO REPT-OPEN.                 EL315
01807                                                                   EL315
01808      IF DTE-FICH NOT = SPACE                                      EL315
01809          WRITE FICH-REC FROM PRT.                                 EL315
01810                                                                   EL315
01811      IF DTE-PRT-OPT = 'P' OR 'B' OR 'T'                           EL315
01812          WRITE PRT.                                               EL315
01813                                                                   EL315
01814      GO TO DTE-PRINT-EXIT.                                        EL315
01815                                                                   EL315
01816  DTE-REPORT-DELETE.                                               EL315
01817      IF DTE-F-1 NOT = ZERO                                        EL315
01818          MOVE ZERO TO DTE-VSAM-FLAGS                              EL315
01819          GO TO DTE-DELETE-EXIT.                                   EL315
01820                                                                   EL315
01821      READ ELREPT   NEXT RECORD                                    EL315
01822            AT END   GO TO DTE-DELETE-EXIT.                        EL315
01823                                                                   EL315
01824      IF DTE-CLASIC-COMPANY-CD = RF-COMPANY-CD  AND                EL315
01825         OLC-REPORT-NAME       = RF-REPORT-ID                      EL315
01826          DELETE ELREPT RECORD                                     EL315
01827          GO TO DTE-REPORT-DELETE.                                 EL315
01828                                                                   EL315
01829  DTE-DELETE-EXIT.                                                 EL315
01830      EXIT.                                                        EL315
01831                                                                   EL315
01832  DTE-PRINT-EXIT.                                                  EL315
01833      EXIT.                                                        EL315
01834 ******************************************************************EL315
01835                                                                   EL315
01836      EJECT                                                        EL315
01837  OPEN-FILES SECTION.                                              EL315
01838                                                                   EL315
01839  OFS-010.                                                         EL315
01840      OPEN  INPUT  REPORTS-EXTRACT-FILE                            EL315
01841            OUTPUT PRNTR.                                          EL315
01842                                                                   EL315
01843      MOVE SPACES   TO  CARRIER-TOTALS-AREA.                       EL315
01844                                                                   EL315
01845      MOVE ZERO     TO  CT-FUTURE-BY-YEAR (1)                      EL315
01846                        CT-PTC-BY-YEAR    (1)                      EL315
01847                        CT-PTC-BY-YEAR-AH (1)                      EL315
01848                        CT-PTC-BY-YEAR-LF (1)                      EL315
01849                        CT-IBNR-BY-YEAR   (1)                      EL315
01850                        CT-MANUAL-BY-YEAR (1).                     EL315
01851                                                                   EL315
01852      SET YEAR-INDEX TO +2.                                        EL315
01853                                                                   EL315
01854  OFS-020.                                                         EL315
01855      MOVE CT-RESERVES-BY-YEAR (1) TO                              EL315
01856           CT-RESERVES-BY-YEAR (YEAR-INDEX).                       EL315
01857                                                                   EL315
01858      IF YEAR-INDEX LESS THAN +12                                  EL315
01859          SET YEAR-INDEX UP BY +1                                  EL315
01860          GO  TO OFS-020.                                          EL315
01861                                                                   EL315
01862      MOVE ZEROS  TO  CT-FUTURE-BY-STATE (1)                       EL315
01863                      CT-PTC-BY-STATE    (1)                       EL315
01864                      CT-PTC-BY-STATE-AH (1)                       EL315
01865                      CT-PTC-BY-STATE-LF (1)                       EL315
01866                      CT-IBNR-BY-STATE   (1)                       EL315
01867                      CT-MANUAL-BY-STATE (1).                      EL315
01868                                                                   EL315
01869      SET STATE-INDEX TO +2.                                       EL315
01870                                                                   EL315
01871  OFS-030.                                                         EL315
01872      MOVE CT-RESERVES-BY-STATE (1) TO                             EL315
01873           CT-RESERVES-BY-STATE (STATE-INDEX).                     EL315
01874                                                                   EL315
01875      IF STATE-INDEX LESS THAN +60                                 EL315
01876          SET STATE-INDEX UP BY +1                                 EL315
01877          GO  TO OFS-030.                                          EL315
01878                                                                   EL315
01879      MOVE CARRIER-TOTALS  TO  CARRIER-TOTALS-ZERO.                EL315
01880                                                                   EL315
01881      SET  CT-INDEX                                                EL315
01882           CT-INDEX-MAX                                            EL315
01883           YEAR-INDEX                                              EL315
01884           STATE-INDEX                                             EL315
01885           STATE-INDEX-MAX      TO +1.                             EL315
01886      MOVE CARRIER-TOTALS-ZERO  TO CARRIER-TOTALS-ENTRY (30).      EL315
01887      SET  CT-INDEX2            TO +1.                             EL315
01888                                                                   EL315
01889  OFS-040.                                                         EL315
01890      MOVE CARRIER-TOTALS-ZERO TO                                  EL315
01891           CARRIER-TOTALS-ENTRY (CT-INDEX2).                       EL315
01892                                                                   EL315
01893      IF CT-INDEX2 LESS THAN +30                                   EL315
01894          SET CT-INDEX2 UP BY +1                                   EL315
01895          GO  TO OFS-040.                                          EL315
01896                                                                   EL315
01897  OFS-EXIT.                                                        EL315
01898      EXIT.                                                        EL315
01899                                                                   EL315
01900                                                                   EL315
01901      EJECT                                                        EL315
01902  CLOSE-FILES SECTION.                                             EL315
01903                                                                   EL315
01904  CFS-010. COPY ELCPRTCX SUPPRESS.                                 EL315
01905                                                                   EL315
01906      CLOSE REPORTS-EXTRACT-FILE                                   EL315
01907            PRNTR.                                                 EL315
01908                                                                   EL315
01909  CFS-EXIT.                                                        EL315
01910      EXIT.                                                        EL315
01911                                                                   EL315
01912  ABEND-PGM SECTION. COPY ELCABEND SUPPRESS.                       EL315
