00001  IDENTIFICATION DIVISION.                                         03/06/98
00002                                                                   ECS052
00003  PROGRAM-ID.                ECS052.                                  LV005
00004 *              PROGRAM CONVERTED BY                               ECS052
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS052
00006 *              CONVERSION DATE 02/07/96 10:20:25.                 ECS052
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS052
00008 *                           VMOD=2.013.                              CL**5
00009                                                                   ECS052
00010 *AUTHOR.        LOGIC, INC.                                       ECS052
00011 *               DALLAS, TEXAS.                                    ECS052
00012                                                                   ECS052
00013 *DATE-COMPILED.                                                   ECS052
00014                                                                   ECS052
00015 *SECURITY.   *****************************************************ECS052
00016 *            *                                                   *ECS052
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS052
00018 *            *                                                   *ECS052
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS052
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS052
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS052
00022 *            *                                                   *ECS052
00023 *            *****************************************************ECS052
00024                                                                   ECS052
00025 *REMARKS.                                                         ECS052
00026                                                                   ECS052
00027 *    PRINTS ONE-LINE STATUS REPORT, USING THE FOLLOWING OPTIONS - ECS052
00028                                                                   ECS052
00029 *       OPTION    FUNCTION                                        ECS052
00030 *       ------    --------------------------                      ECS052
00031 *         1       CERTIFICATE SEQUENCE                            ECS052
00032 *         2       CERTIFICATE WITHIN ACCOUNT                      ECS052
00033 *         3       ACCOUNT WITHIN STATE WITHIN CERTIFICATE         ECS052
00034 *         4       ALPHA WITHIN ACCOUNT                            ECS052
00035 *         5       ACCOUNT WITHIN ALPHA                            ECS052
00036 *         6       NAME WITHIN EFF DATE WITHIN ACCOUNT             ECS052
00037 *         7       CERTIFICATE WITHIN CARRIER                      ECS052
00038                                                                   ECS052
00039  ENVIRONMENT DIVISION.                                            ECS052
00040  CONFIGURATION SECTION.                                           ECS052
00041  INPUT-OUTPUT SECTION.                                            ECS052
00042  FILE-CONTROL.                                                    ECS052
00043                                                                   ECS052
00044      SELECT SORT-FILE    ASSIGN TO SYS001-DA-3380-S-SORTWK1.      ECS052
00045      SELECT PRNTR        ASSIGN TO   SYS008-UR-1403-S-SYS008.     ECS052
00046      SELECT ALPH-EXTC    ASSIGN TO   SYS013-UT-2400-S-SYS013.     ECS052
00047      SELECT DISK-DATE    ASSIGN TO   SYS019-UT-3380-S-SYS019.     ECS052
00048      SELECT FICH         ASSIGN TO   SYS020-UT-2400-S-SYS020.     ECS052
00049      EJECT                                                        ECS052
00050  DATA DIVISION.                                                   ECS052
00051  FILE SECTION.                                                    ECS052
00052                                                                   ECS052
00053  SD  SORT-FILE.                                                   ECS052
00054                                                                   ECS052
00055  01  SORT-FILE-REC.                                               ECS052
00056      12  FILLER              PIC X(3).                            ECS052
00057      12  SORT-CARRIER        PIC X.                               ECS052
00058      12  SORT-GROUP          PIC X(6).                            ECS052
00059      12  SORT-STATE          PIC XX.                              ECS052
00060      12  SORT-ACCT           PIC X(10).                           ECS052
00061      12  SORT-DATE           PIC X(6).                            ECS052
00062      12  SORT-CERT           PIC X(11).                           ECS052
00063      12  SORT-NAME           PIC X(26).                           ECS052
00064      12  FILLER              PIC X(14).                           ECS052
00065      12  SORT-RTERM          PIC S999   COMP-3.                   ECS052
00066      12  FILLER              PIC X(219).                          ECS052
00067                                                                   ECS052
00068  FD  PRNTR                                                        ECS052
00069                              COPY ELCPRTFD.                       ECS052
00070                                                                   ECS052
00071  FD  ALPH-EXTC                                                    ECS052
00072                              COPY ECSAEXFD.                       ECS052
00073  EJECT                                                            ECS052
00074  FD  DISK-DATE                                                    ECS052
00075                              COPY ELCDTEFD.                       ECS052
00076                                                                   ECS052
00077  FD  FICH                                                         ECS052
00078                              COPY ELCFCHFD.                       ECS052
00079  EJECT                                                            ECS052
00080  WORKING-STORAGE SECTION.                                         ECS052
00081  77  FILLER  PIC X(32) VALUE '********************************'.  ECS052
00082  77  FILLER  PIC X(32) VALUE '     ECS052 WORKING-STORAGE     '.  ECS052
00083  77  FILLER  PIC X(32) VALUE '******* VMOD=2.012 *************'.  ECS052
00084                                                                   ECS052
00085  77  FIRST-TIME-SW           PIC X               VALUE 'Y'.       ECS052
00086      88  FIRST-TIME                              VALUE 'Y'.       ECS052
00087  77  OB-SWITCH               PIC X               VALUE 'N'.       ECS052
00088      88  OB-CERT                                 VALUE 'Y'.       ECS052
00089                                                                   ECS052
00090  01  WS.                                                          ECS052
00091      12  WS-RETURN-CODE       PIC S9(4) COMP      VALUE +0.       ECS052
00092      12  WS-ABEND-MESSAGE     PIC  X(80)          VALUE SPACES.   ECS052
00093      12  WS-ABEND-FILE-STATUS PIC  XX             VALUE ZEROS.    ECS052
00094      12  WS-ZERO              PIC S9              VALUE +0.       ECS052
00095                                                                   ECS052
00096  01  SORT-INFO.                                                   ECS052
00097      05  RECS-RETURNED-FLAG   PIC X               VALUE 'N'.      ECS052
00098          88  ALL-RECS-RETURNED                    VALUE 'Y'.      ECS052
00099      05  TOTAL-RECS-RETURNED  PIC 9(9) COMP-3     VALUE ZEROS.    ECS052
00100                                                                   ECS052
00101  01  PGM-SUB                 PIC S999    COMP    VALUE +052.      ECS052
00102                                                                   ECS052
00103  01  WS-AX-LF-AMT            PIC S9(9)V99 COMP-3 VALUE ZEROS.     ECS052
00104                                                                   ECS052
00105  01  WS-AX-LF-AMT-OB REDEFINES                                    ECS052
00106      WS-AX-LF-AMT            PIC S9(11)    COMP-3.                ECS052
00107                                                                   ECS052
00108  01  WS-AX-AH-AMT            PIC S9(5)V99 COMP-3 VALUE ZEROS.     ECS052
00109                                                                   ECS052
00110  01  WS-AX-AH-AMT-OB REDEFINES                                    ECS052
00111      WS-AX-AH-AMT            PIC S9(7)    COMP-3.                 ECS052
00112                                                                   ECS052
00113  01  LF-AH-WORK-AREAS.                                            ECS052
00114      05  WS-XX-AMT           PIC S9(9)V99    VALUE ZEROS.         ECS052
00115      05  WS-XX-REMAMT        PIC S9(9)V99    VALUE ZEROS.         ECS052
00116      05  WS-XX-PREM          PIC S9(9)V99    VALUE ZEROS.         ECS052
00117                                                                   ECS052
00118  01  WORK-DATES.                                                  ECS052
00119      05  EXP-DATE            PIC S9(5)    COMP-3 VALUE ZEROS.     ECS052
00120      05  CUR-DATE            PIC S9(5)    COMP-3 VALUE ZEROS.     ECS052
00121      05  WS-DATE-FORMAT-IN   PIC 9(11)    VALUE ZEROS.               CL**4
00122      05  WS-DATE-FORMAT-IN-R REDEFINES WS-DATE-FORMAT-IN.            CL**3
00123          10  FILLER           PIC  999.                           ECS052
00124          10  WS-DATEIN-CCYY   PIC 9(04).                             CL**4
00125          10  WS-DATEIN-CCYR REDEFINES WS-DATEIN-CCYY.             ECS052
00126              15  WS-DATEIN-CC PIC  99.                            ECS052
00127              15  WS-DATEIN-YR PIC  99.                            ECS052
00128          10  WS-DATEIN-MO     PIC  99.                            ECS052
00129          10  WS-DATEIN-DA     PIC  99.                            ECS052
00130      05  WS-DATE-FORMAT1.                                         ECS052
00131          10  WS-DATE1-MO      PIC  99       VALUE ZEROS.          ECS052
00132          10  WS-DATE1-SLH1    PIC  X        VALUE '/'.            ECS052
00133          10  WS-DATE1-DA      PIC  99       VALUE ZEROS.          ECS052
00134          10  WS-DATE1-SLH2    PIC  X        VALUE '/'.            ECS052
00135          10  WS-DATE1-YR      PIC  99       VALUE ZEROS.          ECS052
00136      05  WS-DATE-FORMAT2.                                         ECS052
00137          10  WS-DATE2-MO      PIC  99       VALUE ZEROS.          ECS052
00138          10  WS-DATE2-SLH1    PIC  X        VALUE '/'.            ECS052
00139          10  WS-DATE2-YR      PIC  99       VALUE ZEROS.          ECS052
00140                                                                   ECS052
00141  01  MISC-WORK-AREAS.                                             ECS052
00142      12  LST-COMP            PIC X(4)            VALUE SPACES.    ECS052
00143      12  LAS-AX-CONTROL      PIC X(36)           VALUE SPACES.    ECS052
00144      12  X                   PIC X               VALUE SPACE.     ECS052
00145      12  LAST-CARRIER        PIC X               VALUE SPACE.     ECS052
00146                                                                   ECS052
00147  01  COMP-3-WORK         COMP-3.                                  ECS052
00148      12  PG-NO               PIC 9(5)            VALUE  0.        ECS052
00149      12  LN-CT               PIC 9(5)            VALUE  80.       ECS052
00150      12  MAX-LN              PIC 9(5)            VALUE  56.       ECS052
00151      12  TOT-REM-PRM         PIC S9(9)V99        VALUE ZEROS.     ECS052
00152      12  FIN-REM-PRM         PIC S9(9)V99        VALUE ZEROS.     ECS052
00153      12  TOT-CRT-CNT         PIC S9(9)V99        VALUE ZEROS.     ECS052
00154      12  FIN-CRT-CNT         PIC S9(9)V99        VALUE ZEROS.     ECS052
00155      12  TOT-CAR-CNT         PIC S9(9)V99        VALUE ZEROS.     ECS052
00156      12  TOT-CAR-PRM         PIC S9(9)V99        VALUE ZEROS.     ECS052
00157                                                                   ECS052
00158                                                                   ECS052
00159  01  WORK-ALPH.                                                   ECS052
00160      12  FILLER              PIC X.                               ECS052
00161      12  AX-CNTRL            PIC X(12).                           ECS052
00162      12  FILLER              PIC X(6).                            ECS052
00163  EJECT                                                            ECS052
00164      COPY ECSAEX01.                                               ECS052
00165  EJECT                                                            ECS052
00166  01  RPT-TITLE-WORDS.                                             ECS052
00167      05  SORT-SEQ1          PIC X(20)  VALUE                      ECS052
00168          'CERTIFICATE, ACCOUNT'.                                  ECS052
00169      05  SORT-SEQ2          PIC X(20)  VALUE                      ECS052
00170          'ACCOUNT, CERTIFICATE'.                                  ECS052
00171      05  SORT-SEQ3          PIC X(20)  VALUE                      ECS052
00172          'CERT, STATE, ACCOUNT'.                                  ECS052
00173      05  SORT-SEQ4          PIC X(20)  VALUE                      ECS052
00174          'ACCOUNT, NAME       '.                                  ECS052
00175      05  SORT-SEQ5          PIC X(20)  VALUE                      ECS052
00176          'NAME, ACCOUNT       '.                                  ECS052
00177      05  SORT-SEQ6          PIC X(20)  VALUE                      ECS052
00178          'ACCT, EFF. DTE, NAME'.                                  ECS052
00179      05  SORT-SEQ7          PIC X(20)  VALUE                      ECS052
00180          'CARRIER, CERTIFICATE'.                                  ECS052
00181      05  SORT-SEQ8          PIC X(20)  VALUE                      ECS052
00182          'REM TERM, CARR, ACCT'.                                  ECS052
00183                                                                   ECS052
00184  01  HD4-BY-REPORT-SEQUENCE.                                      ECS052
00185      05  HD4-SEQ1.                                                ECS052
00186          10  FILLER              PIC X(12)   VALUE                ECS052
00187              'CERTIFICATE '.                                      ECS052
00188          10  FILLER              PIC X(7)    VALUE                ECS052
00189              'ACCOUNT'.                                           ECS052
00190          10  FILLER              PIC X(19)   VALUE  SPACES.       ECS052
00191      05  HD4-SEQ2.                                                ECS052
00192          10  FILLER              PIC X(7)    VALUE                ECS052
00193              'ACCOUNT'.                                           ECS052
00194          10  FILLER              PIC X(4)    VALUE  SPACES.       ECS052
00195          10  FILLER              PIC X(11)   VALUE                ECS052
00196              'CERTIFICATE'.                                       ECS052
00197          10  FILLER              PIC X(16)   VALUE  SPACES.       ECS052
00198      05  HD4-SEQ3.                                                ECS052
00199          10  FILLER              PIC X(12)   VALUE                ECS052
00200              'CERTIFICATE '.                                      ECS052
00201          10  FILLER              PIC X(3)    VALUE                ECS052
00202              'ST '.                                               ECS052
00203          10  FILLER              PIC X(7)    VALUE                ECS052
00204              'ACCOUNT'.                                           ECS052
00205          10  FILLER              PIC X(16)   VALUE  SPACES.       ECS052
00206      05  HD4-SEQ4.                                                ECS052
00207          10  FILLER              PIC X(7)    VALUE                ECS052
00208              'ACCOUNT'.                                           ECS052
00209          10  FILLER              PIC X(4)    VALUE  SPACES.       ECS052
00210          10  FILLER              PIC X(4)    VALUE                ECS052
00211              'NAME'.                                              ECS052
00212          10  FILLER              PIC X(10)   VALUE  SPACES.       ECS052
00213          10  FILLER              PIC X(4)    VALUE                ECS052
00214              'INIT'.                                              ECS052
00215          10  FILLER              PIC X(9)    VALUE  SPACES.       ECS052
00216      05  HD4-SEQ5.                                                ECS052
00217          10  FILLER              PIC X(4)    VALUE                ECS052
00218              'NAME'.                                              ECS052
00219          10  FILLER              PIC X(10)   VALUE  SPACES.       ECS052
00220          10  FILLER              PIC X(5)    VALUE                ECS052
00221              'INIT '.                                             ECS052
00222          10  FILLER              PIC X(7)    VALUE                ECS052
00223              'ACCOUNT'.                                           ECS052
00224          10  FILLER              PIC X(12)   VALUE  SPACES.       ECS052
00225      05  HD4-SEQ6.                                                ECS052
00226          10  FILLER              PIC X(7)    VALUE                ECS052
00227              'ACCOUNT'.                                           ECS052
00228          10  FILLER              PIC X(4)    VALUE  SPACES.       ECS052
00229          10  FILLER              PIC X(13)   VALUE                ECS052
00230              'ISSUE DT NAME'.                                     ECS052
00231          10  FILLER              PIC X(9)    VALUE  SPACES.       ECS052
00232          10  FILLER              PIC X(4)    VALUE                ECS052
00233              'INIT'.                                              ECS052
00234          10  FILLER              PIC X       VALUE  SPACES.       ECS052
00235      05  HD4-SEQ7.                                                ECS052
00236          10  FILLER              PIC X(16)   VALUE                ECS052
00237              'CARR CERTIFICATE'.                                  ECS052
00238          10  FILLER              PIC X(22)   VALUE  SPACES.       ECS052
00239      05  HD4-SEQ8.                                                ECS052
00240          10  FILLER              PIC X(34)   VALUE                ECS052
00241              'REM TERM CARR   ACCOUNT     CERT #'.                ECS052
00242          10  FILLER              PIC X(14)   VALUE  SPACES.       ECS052
00243      EJECT                                                        ECS052
00244  01  HD1.                                                         ECS052
00245      12  FILLER              PIC X(53)       VALUE SPACES.        ECS052
00246      12  FILLER              PIC X(18)       VALUE                ECS052
00247              'CERTIFICATE STATUS'.                                ECS052
00248      12  FILLER              PIC X(48)       VALUE SPACES.        ECS052
00249      12  FILLER              PIC X(8)        VALUE 'ECS052  '.    ECS052
00250                                                                   ECS052
00251  01  HD2.                                                         ECS052
00252      12  FILLER              PIC X(47)       VALUE SPACES.        ECS052
00253      12  HD-CO               PIC X(30).                           ECS052
00254      12  FILLER              PIC X(42)       VALUE SPACES.        ECS052
00255      12  HD-RD               PIC X(8).                            ECS052
00256                                                                   ECS052
00257  01  HD3.                                                         ECS052
00258      12  FILLER              PIC X(13)       VALUE                ECS052
00259          ' REPORT SEQ- '.                                         ECS052
00260      12  HD3-RPT-SEQ         PIC X(20)       VALUE SPACES.        ECS052
00261      12  FILLER              PIC X(20)       VALUE SPACES.        ECS052
00262      12  HD-DT               PIC X(18).                           ECS052
00263      12  FILLER              PIC X(61)       VALUE SPACES.        ECS052
00264      12  FILLER              PIC X(5)        VALUE 'PAGE'.        ECS052
00265      12  HD-PG               PIC ZZ,ZZ9.                          ECS052
00266                                                                   ECS052
00267  01  HD4.                                                         ECS052
00268      05  HD4-SEQ-TITLE           PIC X(38)   VALUE  SPACES.       ECS052
00269      05  HD4-C-TITLE             PIC X(4)    VALUE                ECS052
00270          'CARR'.                                                  ECS052
00271      05  FILLER                  PIC X       VALUE  SPACES.       ECS052
00272      05  FILLER                  PIC X(5)    VALUE                ECS052
00273          'GROUP'.                                                 ECS052
00274      05  FILLER                  PIC XX      VALUE  SPACES.       ECS052
00275      05  HD4-ST-TITLE            PIC XX      VALUE                ECS052
00276          'ST'.                                                    ECS052
00277      05  FILLER                  PIC X       VALUE  SPACES.       ECS052
00278      05  HD4-ACCOUNT-TITLE       PIC X(7)    VALUE                ECS052
00279          'ACCOUNT'.                                               ECS052
00280      05  FILLER                  PIC X(4)    VALUE  SPACES.       ECS052
00281      05  HD4-CERT-TITLE          PIC X(11)   VALUE                ECS052
00282          'CERTIFICATE'.                                           ECS052
00283      05  FILLER                  PIC X       VALUE  SPACES.       ECS052
00284      05  HD4-NAME-TITLE          PIC X(4)    VALUE                ECS052
00285          'NAME'.                                                  ECS052
00286      05  FILLER                  PIC X(10)   VALUE SPACES.        ECS052
00287      05  HD4-INIT-TITLE          PIC X(5)    VALUE                ECS052
00288          'INIT '.                                                 ECS052
00289      05  FILLER                  PIC X(4)    VALUE                ECS052
00290          'AGE '.                                                  ECS052
00291      05  HD4-ISSUE-TITLE         PIC X(8)    VALUE                ECS052
00292          'ISSUE DT'.                                              ECS052
00293      05  FILLER                  PIC X(24)   VALUE                ECS052
00294          '  ENTRY REIN PF   APR   '.                              ECS052
00295  01  HD5.                                                         ECS052
00296      12  FILLER.                                                  ECS052
00297          16  FILLER          PIC X(05)           VALUE SPACES.    ECS052
00298          16  HD5-SSN         PIC X(13)           VALUE            ECS052
00299              '    SSN    '.                                       ECS052
00300          16  HD5-MICRO-NUMS  PIC X(20)           VALUE            ECS052
00301              'ISS/CAN MICROFICHE'.                                ECS052
00302      12  FILLER              PIC X(5)            VALUE            ECS052
00303          'TYPE '.                                                 ECS052
00304      12  FILLER              PIC X(13)           VALUE            ECS052
00305          'BEN   ORG REM'.                                         ECS052
00306      12  FILLER              PIC X(4)            VALUE SPACES.    ECS052
00307      12  FILLER              PIC X(8)            VALUE            ECS052
00308          'ORIGINAL'.                                              ECS052
00309      12  FILLER              PIC X(3)            VALUE SPACES.    ECS052
00310      12  FILLER              PIC X(9)            VALUE            ECS052
00311          'REMAINING'.                                             ECS052
00312      12  FILLER              PIC X(5)            VALUE SPACES.    ECS052
00313      12  FILLER              PIC X(7)            VALUE            ECS052
00314          'PREMIUM'.                                               ECS052
00315      12  FILLER              PIC X(6)            VALUE SPACES.    ECS052
00316      12  FILLER              PIC X(5)            VALUE            ECS052
00317          'CLAIM'.                                                 ECS052
00318      12  FILLER              PIC X(7)            VALUE SPACES.    ECS052
00319      12  FILLER              PIC X(6)            VALUE            ECS052
00320          'REFUND'.                                                ECS052
00321      12  FILLER              PIC X(3)            VALUE SPACES.    ECS052
00322      12  FILLER              PIC X(14)           VALUE            ECS052
00323          'MSG  MSG DATE '.                                        ECS052
00324                                                                   ECS052
00325  01  HD6.                                                         ECS052
00326      12  FILLER              PIC X(49)           VALUE SPACES.    ECS052
00327      12  FILLER              PIC X(7)            VALUE            ECS052
00328          'TRM TRM'.                                               ECS052
00329      12  FILLER              PIC X(5)            VALUE SPACES.    ECS052
00330      12  FILLER              PIC X(7)            VALUE            ECS052
00331          'BENEFIT'.                                               ECS052
00332      12  FILLER              PIC X(5)            VALUE SPACES.    ECS052
00333      12  FILLER              PIC X(7)            VALUE            ECS052
00334          'BENEFIT'.                                               ECS052
00335      12  FILLER              PIC X(52)           VALUE SPACES.    ECS052
00336      EJECT                                                        ECS052
00337  01  DTL.                                                         ECS052
00338      05  P-DTL-LINE1.                                             ECS052
00339          10  FILLER              PIC X.                           ECS052
00340          10  P-DTL-L1-SEQ1.                                       ECS052
00341              15  P-CERT-SEQ1        PIC X(11).                    ECS052
00342              15  FILLER             PIC X.                        ECS052
00343              15  P-ACCT-SEQ1        PIC X(10).                    ECS052
00344              15  FILLER             PIC X(19).                    ECS052
00345          10  P-DTL-L1-SEQ2 REDEFINES P-DTL-L1-SEQ1.               ECS052
00346              15  P-ACCT-SEQ2        PIC X(10).                    ECS052
00347              15  FILLER             PIC X.                        ECS052
00348              15  P-CERT-SEQ2        PIC X(11).                    ECS052
00349              15  FILLER             PIC X(19).                    ECS052
00350          10  P-DTL-L1-SEQ3 REDEFINES P-DTL-L1-SEQ1.               ECS052
00351              15  P-CERT-SEQ3        PIC X(11).                    ECS052
00352              15  FILLER             PIC X.                        ECS052
00353              15  P-ST-SEQ3          PIC XX.                       ECS052
00354              15  FILLER             PIC X.                        ECS052
00355              15  P-ACCT-SEQ3        PIC X(10).                    ECS052
00356              15  FILLER             PIC X(16).                    ECS052
00357          10  P-DTL-L1-SEQ4 REDEFINES P-DTL-L1-SEQ1.               ECS052
00358              15  P-ACCT-SEQ4        PIC X(10).                    ECS052
00359              15  FILLER             PIC X.                        ECS052
00360              15  P-NAME-SEQ4        PIC X(15).                    ECS052
00361              15  FILLER             PIC X.                        ECS052
00362              15  P-NAME-INIT1-SEQ4  PIC X.                        ECS052
00363              15  P-NAME-INIT2-SEQ4  PIC X.                        ECS052
00364              15  FILLER             PIC X(12).                    ECS052
00365          10  P-DTL-L1-SEQ5 REDEFINES P-DTL-L1-SEQ1.               ECS052
00366              15  P-NAME-SEQ5        PIC X(15).                    ECS052
00367              15  FILLER             PIC X.                        ECS052
00368              15  P-NAME-INIT1-SEQ5  PIC X.                        ECS052
00369              15  P-NAME-INIT2-SEQ5  PIC X.                        ECS052
00370              15  FILLER             PIC X.                        ECS052
00371              15  P-ACCT-SEQ5        PIC X(10).                    ECS052
00372              15  FILLER             PIC X(12).                    ECS052
00373          10  P-DTL-L1-SEQ6 REDEFINES P-DTL-L1-SEQ1.               ECS052
00374              15  P-ACCT-SEQ6        PIC X(10).                    ECS052
00375              15  FILLER             PIC X.                        ECS052
00376              15  P-ISSUE-DATE-SEQ6  PIC X(8).                     ECS052
00377              15  FILLER             PIC X.                        ECS052
00378              15  P-NAME-SEQ6        PIC X(15).                    ECS052
00379              15 P-NAME-INIT1-SEQ6   PIC X.                        ECS052
00380              15 P-NAME-INIT2-SEQ6   PIC X.                        ECS052
00381              15 FILLER              PIC X(4).                     ECS052
00382          10  P-DTL-L1-SEQ7 REDEFINES P-DTL-L1-SEQ1.               ECS052
00383              15  FILLER             PIC X(3).                     ECS052
00384              15  P-CARRIER-SEQ7     PIC X.                        ECS052
00385              15  FILLER             PIC X.                        ECS052
00386              15  P-CERT-SEQ7        PIC X(11).                    ECS052
00387              15  FILLER             PIC X(25).                    ECS052
00388          10  P-DTL-L1-SEQ8 REDEFINES P-DTL-L1-SEQ1.               ECS052
00389              15  FILLER             PIC X(3).                     ECS052
00390              15  P-TERM-SEQ8        PIC XXX.                      ECS052
00391              15  FILLER             PIC X(5).                     ECS052
00392              15  P-CARR-SEQ8        PIC X.                        ECS052
00393              15  FILLER             PIC XX.                       ECS052
00394              15  P-ACCT-SEQ8        PIC X(10).                    ECS052
00395              15  FILLER             PIC X.                        ECS052
00396              15  P-CERT-SEQ8        PIC X(11).                    ECS052
00397              15  FILLER             PIC X(5).                     ECS052
00398          10  P-DTL-L1-REST.                                       ECS052
00399              15  P-CARRIER          PIC X.                        ECS052
00400              15  FILLER             PIC X.                        ECS052
00401              15  P-GROUP            PIC X(6).                     ECS052
00402              15  FILLER             PIC X.                        ECS052
00403              15  P-ST               PIC XX.                       ECS052
00404              15  FILLER             PIC X.                        ECS052
00405              15  P-ACCT             PIC X(10).                    ECS052
00406              15  FILLER             PIC X.                        ECS052
00407              15  P-CERT             PIC X(11).                    ECS052
00408              15  FILLER             PIC X.                        ECS052
00409              15  P-NAME             PIC X(15).                    ECS052
00410              15  FILLER             PIC X.                        ECS052
00411              15  P-NAME-INIT1       PIC X.                        ECS052
00412              15  P-NAME-INIT2       PIC X.                        ECS052
00413              15  FILLER             PIC X.                        ECS052
00414              15  P-AGE              PIC XX.                       ECS052
00415              15  FILLER             PIC X.                        ECS052
00416              15  P-ISSUE-DATE       PIC X(8).                     ECS052
00417              15  FILLER             PIC X.                        ECS052
00418              15  P-SR               PIC X.                        ECS052
00419              15  FILLER             PIC X.                        ECS052
00420              15  P-ENTRY-DATE       PIC X(5).                     ECS052
00421              15  FILLER             PIC X.                        ECS052
00422              15  P-REIN-CODE.                                     ECS052
00423                  20 P-REIN-CODE1    PIC X.                        ECS052
00424                  20 P-REIN-CODE2    PIC X.                        ECS052
00425                  20 P-REIN-CODE3    PIC X.                        ECS052
00426              15  FILLER             PIC XX.                       ECS052
00427              15  P-PF               PIC 99      BLANK WHEN ZERO.  ECS052
00428              15  FILLER             PIC XX.                       ECS052
00429              15  P-APR              PIC ZZZ.9999 BLANK WHEN ZERO. ECS052
00430                                                                   ECS052
00431      05  P-DTL-LINE2 REDEFINES P-DTL-LINE1.                       ECS052
00432          10  P-LINE2-ONLY.                                        ECS052
00433              16  FILLER             PIC X(05).                    ECS052
00434              16  P-SSN              PIC X(11).                    ECS052
00435              16  FILLER             PIC X(02).                    ECS052
00436              16  P-ISS-MICRO-NO     PIC 9(09).                    ECS052
00437              16  P-ISS-MICRO-NO-X REDEFINES P-ISS-MICRO-NO        ECS052
00438                                     PIC X(09).                    ECS052
00439              16  P-MICRO-SLASH      PIC X(01).                    ECS052
00440              16  P-CAN-MICRO-NO     PIC 9(09).                    ECS052
00441              16  P-CAN-MICRO-NO-X REDEFINES P-CAN-MICRO-NO        ECS052
00442                                     PIC X(09).                    ECS052
00443              16  FILLER             PIC X(04).                    ECS052
00444          10  P-TYPE-L-A             PIC XX.                       ECS052
00445          10  FILLER                 PIC X.                        ECS052
00446          10  P-BENEFIT-CODE1        PIC XX.                       ECS052
00447          10  P-BENEFIT-CODE2.                                     ECS052
00448              15  P-BENEFIT-CODE2A   PIC XX.                       ECS052
00449              15  P-BENEFIT-CODE2B   PIC X.                        ECS052
00450          10  FILLER                 PIC X.                        ECS052
00451          10  P-TRM                  PIC 9(3).                     ECS052
00452          10  FILLER                 PIC X.                        ECS052
00453          10  P-R-TRM                PIC X(3).                     ECS052
00454          10  FILLER                 PIC X.                        ECS052
00455          10  P-FACE                 PIC ZZZZ,ZZZ.99-              ECS052
00456                                         BLANK WHEN ZERO.          ECS052
00457          10  P-FACE-OB REDEFINES P-FACE.                          ECS052
00458              15  P-FACE-DOL         PIC ZZZ,ZZZ,ZZZ-              ECS052
00459                                         BLANK WHEN ZERO.          ECS052
00460          10  P-REM                  PIC ZZZZ,ZZZ.99-              ECS052
00461                                         BLANK WHEN ZERO.          ECS052
00462          10  P-REM-OB  REDEFINES P-REM.                           ECS052
00463              15  P-REM-DOL          PIC ZZ,ZZZ,ZZZ-               ECS052
00464                                         BLANK WHEN ZERO.          ECS052
00465              15  FILLER             PIC X.                        ECS052
00466          10  P-EXIT-MSG-N-DATE REDEFINES P-REM.                   ECS052
00467              15  P-EXIT-MSG         PIC X(4).                     ECS052
00468              15  P-EXIT-DATE        PIC X(8).                     ECS052
00469          10  P-PREMIUM           PIC ZZZZ,ZZZ.99-                 ECS052
00470                                         BLANK WHEN ZERO.          ECS052
00471          10  P-CLAIM-PMTS        PIC ZZZZ,ZZZ.99-                 ECS052
00472                                         BLANK WHEN ZERO.          ECS052
00473          10  P-REFUND            PIC ZZZZ,ZZZ.99-                 ECS052
00474                                         BLANK WHEN ZERO.          ECS052
00475          10  FILLER              PIC XX.                          ECS052
00476          10  P-MSG               PIC X(4).                        ECS052
00477          10  FILLER              PIC X.                           ECS052
00478          10  P-MSG-DATE          PIC X(8).                        ECS052
00479                                                                   ECS052
00480  01  TOTAL-PRT-LINE.                                              ECS052
00481      12  FILLER              PIC X(19)           VALUE SPACES.    ECS052
00482      12  FILLER              PIC X(21)           VALUE            ECS052
00483              'REMAINING BENEFIT ON '.                             ECS052
00484      12  TL-CNT              PIC ZZZ,ZZZ,ZZ9.                     ECS052
00485      12  FILLER              PIC X(17)           VALUE            ECS052
00486              ' CERTIFICATES IS '.                                 ECS052
00487      12  TL-PRM              PIC ZZZ,ZZZ,ZZ9.99.                  ECS052
00488      12  TL-MES              PIC X(17)           VALUE SPACES.    ECS052
00489      12  TL-COMP             PIC XXXX            VALUE SPACES.    ECS052
00490                                                                   ECS052
00491           COPY ELCDTECX.                                          ECS052
00492                                                                   ECS052
00493           COPY ELCDTEVR.                                          ECS052
00494  EJECT                                                            ECS052
00495  PROCEDURE DIVISION.                                              ECS052
00496                                                                   ECS052
00497  0000-SET-START SECTION.                                          ECS052
00498                              COPY ELCDTERX.                       ECS052
00499                                                                   ECS052
00500  100-SORT-PROCESSING-SECTION.                                     ECS052
00501                                                                   ECS052
00502      PERFORM 400-INITIALIZATION THRU 400-EXIT.                    ECS052
00503                                                                   ECS052
00504      IF  DTE-PGM-OPT = 2                                          ECS052
00505          SORT SORT-FILE ASCENDING KEY SORT-ACCT                   ECS052
00506                                       SORT-CERT                   ECS052
00507              USING ALPH-EXTC                                      ECS052
00508              OUTPUT PROCEDURE 200-OUTPUT-PROCESS THRU 200-EXIT    ECS052
00509              GO TO 150-CHECK-SORT-CODE.                           ECS052
00510                                                                   ECS052
00511      IF  DTE-PGM-OPT = 3                                          ECS052
00512          SORT SORT-FILE ASCENDING KEY SORT-CERT                   ECS052
00513                                       SORT-STATE                  ECS052
00514                                       SORT-ACCT                   ECS052
00515              USING ALPH-EXTC                                      ECS052
00516              OUTPUT PROCEDURE 200-OUTPUT-PROCESS THRU 200-EXIT    ECS052
00517              GO TO 150-CHECK-SORT-CODE.                           ECS052
00518                                                                   ECS052
00519      IF  DTE-PGM-OPT = 4                                          ECS052
00520          SORT SORT-FILE ASCENDING KEY SORT-ACCT                   ECS052
00521                                       SORT-NAME                   ECS052
00522              USING ALPH-EXTC                                      ECS052
00523              OUTPUT PROCEDURE 200-OUTPUT-PROCESS THRU 200-EXIT    ECS052
00524              GO TO 150-CHECK-SORT-CODE.                           ECS052
00525                                                                   ECS052
00526      IF  DTE-PGM-OPT = 5                                          ECS052
00527          SORT SORT-FILE ASCENDING KEY SORT-NAME                   ECS052
00528                                       SORT-ACCT                   ECS052
00529              USING ALPH-EXTC                                      ECS052
00530              OUTPUT PROCEDURE 200-OUTPUT-PROCESS THRU 200-EXIT    ECS052
00531              GO TO 150-CHECK-SORT-CODE.                           ECS052
00532                                                                   ECS052
00533      IF  DTE-PGM-OPT = 6                                          ECS052
00534          SORT SORT-FILE ASCENDING KEY SORT-ACCT                   ECS052
00535                                       SORT-DATE                   ECS052
00536                                       SORT-NAME                   ECS052
00537              USING ALPH-EXTC                                      ECS052
00538              OUTPUT PROCEDURE 200-OUTPUT-PROCESS THRU 200-EXIT    ECS052
00539              GO TO 150-CHECK-SORT-CODE.                           ECS052
00540                                                                   ECS052
00541      IF  DTE-PGM-OPT = 7                                          ECS052
00542          SORT SORT-FILE ASCENDING KEY SORT-CARRIER                ECS052
00543                                       SORT-CERT                   ECS052
00544              USING ALPH-EXTC                                      ECS052
00545              OUTPUT PROCEDURE 200-OUTPUT-PROCESS THRU 200-EXIT    ECS052
00546              GO TO 150-CHECK-SORT-CODE.                           ECS052
00547                                                                   ECS052
00548      IF  DTE-PGM-OPT = 8                                          ECS052
00549          SORT SORT-FILE ASCENDING KEY SORT-RTERM                  ECS052
00550                                       SORT-CARRIER                ECS052
00551                                       SORT-ACCT                   ECS052
00552                                       SORT-CERT                   ECS052
00553              INPUT PROCEDURE  0110-INPUT-PROCEDURE THRU 0149-EXIT ECS052
00554              OUTPUT PROCEDURE 200-OUTPUT-PROCESS THRU 200-EXIT    ECS052
00555              GO TO 150-CHECK-SORT-CODE.                           ECS052
00556                                                                   ECS052
00557                                                                   ECS052
00558      SORT SORT-FILE ASCENDING KEY SORT-CERT                       ECS052
00559                                   SORT-ACCT                       ECS052
00560              USING ALPH-EXTC                                      ECS052
00561              OUTPUT PROCEDURE 200-OUTPUT-PROCESS THRU 200-EXIT.   ECS052
00562                                                                   ECS052
00563  150-CHECK-SORT-CODE.                                             ECS052
00564      IF SORT-RETURN NOT = ZERO                                    ECS052
00565          MOVE +0101                 TO WS-RETURN-CODE             ECS052
00566          MOVE 'INTERNAL SORT FAILED' TO WS-ABEND-MESSAGE          ECS052
00567          GO TO ABEND-PGM.                                         ECS052
00568                                                                   ECS052
00569      PERFORM 198-END-JOB     THRU 198-EXIT.                       ECS052
00570                                                                   ECS052
00571      PERFORM 199-CLOSE-FILES THRU 199-EXIT.                       ECS052
00572                                                                   ECS052
00573      GOBACK.                                                      ECS052
00574      EJECT                                                        ECS052
00575  198-END-JOB.                                                     ECS052
00576      IF  DTE-PGM-OPT = '7'                                        ECS052
00577          MOVE TOT-CAR-PRM          TO  TL-PRM                     ECS052
00578          MOVE TOT-CAR-CNT          TO  TL-CNT                     ECS052
00579          MOVE TOTAL-PRT-LINE       TO  PRT                        ECS052
00580          MOVE '0'                  TO  P-CTL                      ECS052
00581          PERFORM 420-PRINT-DETAIL THRU 420-EXIT.                  ECS052
00582                                                                   ECS052
00583      MOVE    TOT-REM-PRM       TO  TL-PRM.                        ECS052
00584      MOVE    TOT-CRT-CNT       TO  TL-CNT.                        ECS052
00585      MOVE    TOTAL-PRT-LINE    TO  PRT.                           ECS052
00586      MOVE    '0'               TO  P-CTL.                         ECS052
00587      PERFORM 420-PRINT-DETAIL THRU 420-EXIT.                      ECS052
00588                                                                   ECS052
00589      ADD     TOT-REM-PRM       TO  FIN-REM-PRM.                   ECS052
00590      ADD     TOT-CRT-CNT       TO  FIN-CRT-CNT.                   ECS052
00591                                                                   ECS052
00592      MOVE    ZEROS             TO  TOT-CRT-CNT                    ECS052
00593                                    TOT-REM-PRM.                   ECS052
00594      MOVE    SPACES            TO  P-DATA.                        ECS052
00595                                                                   ECS052
00596      MOVE    '1'               TO  P-CTL.                         ECS052
00597      PERFORM 420-PRINT-DETAIL THRU 420-EXIT.                      ECS052
00598                                                                   ECS052
00599  198-EXIT.                                                        ECS052
00600      EXIT.                                                        ECS052
00601                                                                   ECS052
00602  199-CLOSE-FILES.                                                 ECS052
00603                              COPY ELCPRTC.                        ECS052
00604                                                                   ECS052
00605      CLOSE PRNTR.                                                 ECS052
00606                                                                   ECS052
00607  199-EXIT.                                                        ECS052
00608      EXIT.                                                        ECS052
00609      EJECT                                                        ECS052
00610  0110-INPUT-PROCEDURE SECTION.                                    ECS052
00611                                                                   ECS052
00612      OPEN INPUT ALPH-EXTC.                                        ECS052
00613                                                                   ECS052
00614  0111-READ.                                                       ECS052
00615                                                                   ECS052
00616      READ ALPH-EXTC INTO ALPHA-RECORD                             ECS052
00617          AT END                                                   ECS052
00618              CLOSE ALPH-EXTC                                      ECS052
00619              GO TO 0149-EXIT.                                     ECS052
00620                                                                   ECS052
00621      IF DTE-CLIENT = 'FLC'                                        ECS052
00622          IF (AX-LF-STATUS = '1' OR '2' OR '3' OR '4')             ECS052
00623                           OR                                      ECS052
00624             (AX-AH-STATUS = '1' OR '2' OR '3' OR '4')             ECS052
00625          NEXT SENTENCE                                            ECS052
00626       ELSE                                                        ECS052
00627          GO TO 0111-READ.                                         ECS052
00628                                                                   ECS052
00629      IF AX-AH-REMTERM NOT NUMERIC                                 ECS052
00630          MOVE ZEROS              TO  AX-AH-REMTERM.               ECS052
00631                                                                   ECS052
00632      IF AX-LF-REMTERM NOT NUMERIC                                 ECS052
00633          MOVE ZEROS              TO  AX-LF-REMTERM.               ECS052
00634                                                                   ECS052
00635      IF AX-AH-REMTERM GREATER THAN AX-LF-REMTERM                  ECS052
00636          MOVE AX-AH-REMTERM      TO  AX-LF-REMTERM.               ECS052
00637                                                                   ECS052
00638      RELEASE SORT-FILE-REC FROM ALPHA-RECORD.                     ECS052
00639      GO TO 0111-READ.                                             ECS052
00640                                                                   ECS052
00641  0149-EXIT.                                                       ECS052
00642      EXIT.                                                        ECS052
00643      EJECT                                                        ECS052
00644                                                                   ECS052
00645  200-OUTPUT-PROCESS  SECTION.                                     ECS052
00646                                                                   ECS052
00647      PERFORM 410-PRNT-PG-HEADINGS THRU 410-EXIT.                  ECS052
00648                                                                   ECS052
00649      PERFORM 250-RETURN-RECS THRU 250-EXIT                        ECS052
00650              UNTIL ALL-RECS-RETURNED.                             ECS052
00651                                                                   ECS052
00652  200-EXIT.                                                        ECS052
00653      EXIT.                                                        ECS052
00654                                                                   ECS052
00655  250-RETURN-RECS.                                                 ECS052
00656      RETURN SORT-FILE INTO ALPHA-RECORD                           ECS052
00657             AT END                                                ECS052
00658                   MOVE 'Y' TO RECS-RETURNED-FLAG                  ECS052
00659                   GO TO 250-EXIT.                                 ECS052
00660                                                                   ECS052
00661      IF AX-JOINT-ALPHA OR AX-REIN-ALPHA OR AX-JOINT-REIN-ALPHA    ECS052
00662          GO TO 250-RETURN-RECS.                                   ECS052
00663                                                                   ECS052
00664      ADD 1 TO TOTAL-RECS-RETURNED.                                ECS052
00665                                                                   ECS052
00666      IF FIRST-TIME                                                ECS052
00667          MOVE 'N'                TO  FIRST-TIME-SW                ECS052
00668          MOVE AX-CARRIER         TO  LAST-CARRIER.                ECS052
00669                                                                   ECS052
00670      IF  DTE-PGM-OPT = '7'   AND                                  ECS052
00671          AX-CARRIER NOT = LAST-CARRIER                            ECS052
00672            MOVE    AX-CARRIER            TO  LAST-CARRIER         ECS052
00673            MOVE    TOT-CAR-PRM           TO  TL-PRM               ECS052
00674            MOVE    TOT-CAR-CNT           TO  TL-CNT               ECS052
00675            MOVE    TOTAL-PRT-LINE        TO  PRT                  ECS052
00676            MOVE    '0'                   TO  P-CTL                ECS052
00677            PERFORM 420-PRINT-DETAIL     THRU 420-EXIT             ECS052
00678            MOVE    ZEROS                 TO  TOT-CAR-CNT          ECS052
00679                                              TOT-CAR-PRM          ECS052
00680            PERFORM 410-PRNT-PG-HEADINGS THRU 410-EXIT.            ECS052
00681                                                                   ECS052
00682      IF  LN-CT GREATER MAX-LN                                     ECS052
00683          PERFORM 410-PRNT-PG-HEADINGS THRU 410-EXIT.              ECS052
00684                                                                   ECS052
00685      PERFORM 300-BUILD-PRNT-LINE1    THRU 300-EXIT.               ECS052
00686                                                                   ECS052
00687      MOVE    P-DTL-LINE1              TO  PRT.                    ECS052
00688      PERFORM 420-PRINT-DETAIL        THRU 420-EXIT.               ECS052
00689                                                                   ECS052
00690      IF  AX-LF-TYP = ZEROS                                        ECS052
00691          NEXT SENTENCE                                            ECS052
00692      ELSE                                                         ECS052
00693          PERFORM 310-BUILD-PRNT-LINE2-LF THRU 310-EXIT            ECS052
00694          MOVE    P-DTL-LINE2              TO  PRT                 ECS052
00695          PERFORM 420-PRINT-DETAIL        THRU 420-EXIT            ECS052
00696          IF LN-CT IS GREATER THAN MAX-LN                          ECS052
00697              PERFORM 410-PRNT-PG-HEADINGS THRU 410-EXIT.          ECS052
00698                                                                   ECS052
00699      IF  AX-AH-TYP = ZEROS                                        ECS052
00700          NEXT SENTENCE                                            ECS052
00701      ELSE                                                         ECS052
00702          PERFORM 320-BUILD-PRNT-LINE3-AH THRU 320-EXIT            ECS052
00703          MOVE    P-DTL-LINE2              TO  PRT                 ECS052
00704          PERFORM 420-PRINT-DETAIL        THRU 420-EXIT            ECS052
00705          IF LN-CT IS GREATER THAN MAX-LN                          ECS052
00706              PERFORM 410-PRNT-PG-HEADINGS THRU 410-EXIT.          ECS052
00707                                                                   ECS052
00708  250-EXIT.                                                        ECS052
00709      EXIT.                                                        ECS052
00710      EJECT                                                        ECS052
00711  300-BUILD-PRNT-LINE1.                                            ECS052
00712      IF   AX-CONTROL = LAS-AX-CONTROL                             ECS052
00713          NEXT SENTENCE                                            ECS052
00714      ELSE                                                         ECS052
00715           ADD  1 TO TOT-CAR-CNT                                   ECS052
00716                     TOT-CRT-CNT.                                  ECS052
00717                                                                   ECS052
00718      MOVE AX-CONTROL        TO LAS-AX-CONTROL.                    ECS052
00719                                                                   ECS052
00720      MOVE SPACES            TO  DTL.                              ECS052
00721 **                                                                ECS052
00722 ** SET UP THE VARIABLE PORTION OF DETAIL LINE1.                   ECS052
00723 **                                                                ECS052
00724      IF  DTE-PGM-OPT = 2                                          ECS052
00725          MOVE AX-CERT       TO  P-CERT-SEQ2                       ECS052
00726          MOVE AX-ACCOUNT    TO  P-ACCT-SEQ2                       ECS052
00727      ELSE                                                         ECS052
00728          IF  DTE-PGM-OPT = 3                                      ECS052
00729              MOVE AX-CERT    TO  P-CERT-SEQ3                      ECS052
00730              MOVE AX-ACCOUNT TO  P-ACCT-SEQ3                      ECS052
00731              MOVE AX-STATE   TO  P-ST-SEQ3                        ECS052
00732          ELSE                                                     ECS052
00733              IF DTE-PGM-OPT = 4                                   ECS052
00734                  MOVE AX-ACCOUNT        TO P-ACCT-SEQ4            ECS052
00735                  MOVE AX-LNAME          TO P-NAME-SEQ4            ECS052
00736                  MOVE AX-1ST-INIT-FNAME TO P-NAME-INIT1-SEQ4      ECS052
00737                  MOVE AX-INIT           TO P-NAME-INIT2-SEQ4      ECS052
00738              ELSE                                                 ECS052
00739                  IF DTE-PGM-OPT = 5                               ECS052
00740                      MOVE AX-ACCOUNT        TO P-ACCT-SEQ5        ECS052
00741                      MOVE AX-LNAME          TO P-NAME-SEQ5        ECS052
00742                      MOVE AX-1ST-INIT-FNAME TO P-NAME-INIT1-SEQ5  ECS052
00743                      MOVE AX-INIT           TO P-NAME-INIT2-SEQ5  ECS052
00744                  ELSE                                             ECS052
00745                      IF DTE-PGM-OPT = 6                           ECS052
00746                          MOVE AX-ACCOUNT      TO P-ACCT-SEQ6      ECS052
00747                          MOVE AX-DT          TO  WS-DATE-FORMAT-INECS052
00748                          PERFORM 500-DATE-FORMAT1 THRU 500-EXIT   ECS052
00749                          MOVE WS-DATE-FORMAT1 TO P-ISSUE-DATE-SEQ6ECS052
00750                          MOVE AX-LNAME        TO P-NAME-SEQ6      ECS052
00751                          MOVE AX-1ST-INIT-FNAME                   ECS052
00752                                               TO P-NAME-INIT1-SEQ6ECS052
00753                          MOVE AX-INIT                             ECS052
00754                                               TO P-NAME-INIT2-SEQ6ECS052
00755                      ELSE                                         ECS052
00756                          IF DTE-PGM-OPT = 7                       ECS052
00757                              MOVE AX-CERT    TO P-CERT-SEQ7       ECS052
00758                              MOVE AX-CARRIER TO P-CARRIER-SEQ7    ECS052
00759                          ELSE                                     ECS052
00760                              IF DTE-PGM-OPT = 8                   ECS052
00761                                  MOVE AX-LF-REMTERM               ECS052
00762                                                  TO P-TERM-SEQ8   ECS052
00763                                  MOVE AX-CARRIER TO P-CARR-SEQ8   ECS052
00764                                  MOVE AX-ACCOUNT TO P-ACCT-SEQ8   ECS052
00765                                  MOVE AX-CERT    TO P-CERT-SEQ8   ECS052
00766                              ELSE                                 ECS052
00767                                  MOVE AX-CERT TO P-CERT-SEQ1      ECS052
00768                                  MOVE AX-ACCOUNT TO P-ACCT-SEQ1.  ECS052
00769                                                                   ECS052
00770 **                                                                ECS052
00771 ** SET UP THE FIXED PORTION OF DETAIL LINE1.                      ECS052
00772 **                                                                ECS052
00773                                                                   ECS052
00774      IF  DTE-PGM-OPT = 7                                          ECS052
00775          NEXT SENTENCE                                            ECS052
00776      ELSE                                                         ECS052
00777          MOVE AX-CARRIER  TO  P-CARRIER.                          ECS052
00778                                                                   ECS052
00779      MOVE AX-GROUPING     TO  P-GROUP.                            ECS052
00780                                                                   ECS052
00781      IF  DTE-PGM-OPT = 3                                          ECS052
00782          NEXT SENTENCE                                            ECS052
00783      ELSE                                                         ECS052
00784          MOVE AX-STATE TO  P-ST.                                  ECS052
00785                                                                   ECS052
00786      IF  DTE-PGM-OPT = 7                                          ECS052
00787          MOVE AX-ACCOUNT  TO  P-ACCT.                             ECS052
00788                                                                   ECS052
00789      IF  DTE-PGM-OPT = 4 OR 5 OR 6                                ECS052
00790          MOVE AX-CERT  TO  P-CERT.                                ECS052
00791                                                                   ECS052
00792      IF  DTE-PGM-OPT = 4 OR 5 OR 6                                ECS052
00793          NEXT SENTENCE                                            ECS052
00794      ELSE                                                         ECS052
00795          MOVE AX-LNAME          TO  P-NAME                        ECS052
00796          MOVE AX-1ST-INIT-FNAME TO  P-NAME-INIT1                  ECS052
00797          MOVE AX-INIT           TO  P-NAME-INIT2.                 ECS052
00798                                                                   ECS052
00799      MOVE AX-AGE   TO  P-AGE.                                     ECS052
00800                                                                   ECS052
00801      IF  DTE-PGM-OPT = 6                                          ECS052
00802          NEXT SENTENCE                                            ECS052
00803      ELSE                                                         ECS052
00804          MOVE AX-DT           TO WS-DATE-FORMAT-IN                ECS052
00805          PERFORM 500-DATE-FORMAT1 THRU 500-EXIT                   ECS052
00806          MOVE WS-DATE-FORMAT1 TO P-ISSUE-DATE.                    ECS052
00807                                                                   ECS052
00808      MOVE AX-ENTRY-STATUS      TO  P-SR.                          ECS052
00809                                                                   ECS052
00810      MOVE AX-ENTRY    TO WS-DATE-FORMAT-IN.                       ECS052
00811                                                                   ECS052
00812      PERFORM 510-DATE-FORMAT2 THRU 510-EXIT.                      ECS052
00813                                                                   ECS052
00814      MOVE WS-DATE-FORMAT2   TO  P-ENTRY-DATE.                     ECS052
00815                                                                   ECS052
00816      IF  AX-SPEC-REIN = SPACES                                    ECS052
00817                          OR ZEROS                                 ECS052
00818          MOVE AX-REIN-TABLE TO P-REIN-CODE                        ECS052
00819      ELSE                                                         ECS052
00820          MOVE AX-SPEC-REIN  TO P-REIN-CODE1                       ECS052
00821                                P-REIN-CODE2                       ECS052
00822                                P-REIN-CODE3.                      ECS052
00823                                                                   ECS052
00824      IF  AX-PMT-FREQ = ZEROS                                      ECS052
00825          MOVE ZEROS TO P-PF                                       ECS052
00826      ELSE                                                         ECS052
00827          MOVE AX-PMT-FREQ TO P-PF.                                ECS052
00828                                                                   ECS052
00829      IF  AX-APR = ZEROS                                           ECS052
00830          MOVE ZEROS TO P-APR                                      ECS052
00831      ELSE                                                         ECS052
00832          MOVE AX-APR TO P-APR.                                    ECS052
00833                                                                   ECS052
00834  300-EXIT.                                                        ECS052
00835      EXIT.                                                        ECS052
00836      EJECT                                                        ECS052
00837  310-BUILD-PRNT-LINE2-LF.                                         ECS052
00838      MOVE SPACES            TO  DTL.                              ECS052
00839                                                                   ECS052
00840      IF  AX-SOC-NO GREATER THAN SPACES                            ECS052
00841          MOVE AX-SOC-NO          TO P-SSN                         ECS052
00842          MOVE SPACES             TO AX-SOC-NO                     ECS052
00843      ELSE                                                         ECS052
00844          MOVE SPACES             TO P-SSN.                        ECS052
00845                                                                   ECS052
00846      IF  AX-ISS-MICROFILM-NO GREATER THAN ZEROS                   ECS052
00847          MOVE AX-ISS-MICROFILM-NO                                 ECS052
00848                                  TO P-ISS-MICRO-NO                ECS052
00849          MOVE ZEROS              TO AX-ISS-MICROFILM-NO           ECS052
00850          IF  AX-CAN-MICROFILM-NO GREATER THAN ZEROS               ECS052
00851              MOVE '/'            TO P-MICRO-SLASH                 ECS052
00852              MOVE AX-CAN-MICROFILM-NO                             ECS052
00853                                  TO P-CAN-MICRO-NO                ECS052
00854              MOVE ZEROS          TO AX-CAN-MICROFILM-NO           ECS052
00855          ELSE                                                     ECS052
00856              MOVE SPACES         TO P-MICRO-SLASH                 ECS052
00857                                     P-CAN-MICRO-NO-X              ECS052
00858      ELSE                                                         ECS052
00859          IF  AX-CAN-MICROFILM-NO GREATER THAN ZEROS               ECS052
00860              MOVE SPACES         TO P-ISS-MICRO-NO-X              ECS052
00861                                     P-MICRO-SLASH                 ECS052
00862              MOVE AX-CAN-MICROFILM-NO                             ECS052
00863                                  TO P-CAN-MICRO-NO                ECS052
00864              MOVE ZEROS          TO AX-CAN-MICROFILM-NO           ECS052
00865          ELSE                                                     ECS052
00866              MOVE SPACES         TO P-MICRO-SLASH                 ECS052
00867                                     P-CAN-MICRO-NO-X              ECS052
00868                                     P-ISS-MICRO-NO-X.             ECS052
00869                                                                   ECS052
00870      MOVE LIFE-OVERRIDE-L2  TO  P-TYPE-L-A.                       ECS052
00871                                                                   ECS052
00872      IF  AX-LF-TYP  = SPACES OR ZEROS                             ECS052
00873          MOVE SPACES TO P-BENEFIT-CODE1                           ECS052
00874                         P-BENEFIT-CODE2                           ECS052
00875        ELSE                                                       ECS052
00876          MOVE    AX-LF-TYP  TO  CLAS-LOOK                         ECS052
00877          PERFORM 520-LIFE-CLAS-LOOK-RTN THRU 520-EXIT             ECS052
00878          MOVE    CLAS-I-BEN (CLAS-INDEXL) TO P-BENEFIT-CODE1      ECS052
00879          MOVE    CLAS-I-AB3 (CLAS-INDEXL) TO P-BENEFIT-CODE2.     ECS052
00880                                                                   ECS052
00881                                                                   ECS052
00882      MOVE AX-LF-TERM        TO P-TRM.                             ECS052
00883                                                                   ECS052
00884      MOVE AX-LF-REMTERM     TO P-R-TRM.                           ECS052
00885                                                                   ECS052
00886      COMPUTE WS-XX-AMT = AX-LF-AMT + AX-LF-AMT-ALT.               ECS052
00887                                                                   ECS052
00888      COMPUTE WS-XX-REMAMT = AX-LF-REMAMT + AX-LF-REMAMT-ALT.      ECS052
00889                                                                   ECS052
00890      IF  OB-CERT                                                  ECS052
00891          MOVE    'N'             TO OB-SWITCH                     ECS052
00892          MOVE    WS-XX-AMT       TO WS-AX-LF-AMT                  ECS052
00893          MOVE    WS-AX-LF-AMT-OB TO P-FACE-DOL                    ECS052
00894          MOVE    WS-XX-REMAMT    TO WS-AX-LF-AMT                  ECS052
00895          MOVE    WS-AX-LF-AMT-OB TO P-REM-DOL                     ECS052
00896          COMPUTE TOT-REM-PRM = TOT-REM-PRM + (WS-XX-REMAMT * 100) ECS052
00897          COMPUTE TOT-CAR-PRM = TOT-CAR-PRM + (WS-XX-REMAMT * 100) ECS052
00898      ELSE                                                         ECS052
00899          MOVE WS-XX-AMT    TO P-FACE                              ECS052
00900          MOVE WS-XX-REMAMT TO P-REM                               ECS052
00901          ADD  WS-XX-REMAMT TO TOT-REM-PRM                         ECS052
00902                               TOT-CAR-PRM.                        ECS052
00903                                                                   ECS052
00904      COMPUTE WS-XX-PREM = AX-LF-PRM + AX-LF-PRM-ALT.              ECS052
00905                                                                   ECS052
00906      MOVE WS-XX-PREM        TO  P-PREMIUM.                        ECS052
00907                                                                   ECS052
00908      MOVE AX-LF-CLAIM-PMTS  TO  P-CLAIM-PMTS.                     ECS052
00909                                                                   ECS052
00910      MOVE AX-LF-REFUND      TO  P-REFUND.                         ECS052
00911                                                                   ECS052
00912      PERFORM 540-LF-MSG-FORMAT THRU 540-EXIT.                     ECS052
00913                                                                   ECS052
00914  310-EXIT.                                                        ECS052
00915      EXIT.                                                        ECS052
00916      EJECT                                                        ECS052
00917  320-BUILD-PRNT-LINE3-AH.                                         ECS052
00918                                                                   ECS052
00919      MOVE SPACES          TO  DTL.                                ECS052
00920                                                                   ECS052
00921      IF  AX-SOC-NO GREATER THAN SPACES                            ECS052
00922          MOVE AX-SOC-NO          TO P-SSN                         ECS052
00923          MOVE SPACES             TO AX-SOC-NO                     ECS052
00924      ELSE                                                         ECS052
00925          MOVE SPACES             TO P-SSN.                        ECS052
00926                                                                   ECS052
00927      IF  AX-ISS-MICROFILM-NO GREATER THAN ZEROS                   ECS052
00928          MOVE AX-ISS-MICROFILM-NO                                 ECS052
00929                                  TO P-ISS-MICRO-NO                ECS052
00930          MOVE ZEROS              TO AX-ISS-MICROFILM-NO           ECS052
00931          IF  AX-CAN-MICROFILM-NO GREATER THAN ZEROS               ECS052
00932              MOVE '/'            TO P-MICRO-SLASH                 ECS052
00933              MOVE AX-CAN-MICROFILM-NO                             ECS052
00934                                  TO P-CAN-MICRO-NO                ECS052
00935              MOVE ZEROS          TO AX-CAN-MICROFILM-NO           ECS052
00936          ELSE                                                     ECS052
00937              MOVE SPACES         TO P-MICRO-SLASH                 ECS052
00938                                     P-CAN-MICRO-NO-X              ECS052
00939      ELSE                                                         ECS052
00940          IF  AX-CAN-MICROFILM-NO GREATER THAN ZEROS               ECS052
00941              MOVE SPACES         TO P-ISS-MICRO-NO-X              ECS052
00942                                     P-MICRO-SLASH                 ECS052
00943              MOVE AX-CAN-MICROFILM-NO                             ECS052
00944                                  TO P-CAN-MICRO-NO                ECS052
00945              MOVE ZEROS          TO AX-CAN-MICROFILM-NO           ECS052
00946          ELSE                                                     ECS052
00947              MOVE SPACES         TO P-MICRO-SLASH                 ECS052
00948                                     P-CAN-MICRO-NO-X              ECS052
00949                                     P-ISS-MICRO-NO-X.             ECS052
00950                                                                   ECS052
00951      MOVE ZEROS           TO  P-REM.                              ECS052
00952                                                                   ECS052
00953      MOVE AH-OVERRIDE-L2  TO  P-TYPE-L-A.                         ECS052
00954                                                                   ECS052
00955      IF  AX-AH-TYP = SPACES OR ZEROS                              ECS052
00956          MOVE SPACES TO P-BENEFIT-CODE1                           ECS052
00957                         P-BENEFIT-CODE2                           ECS052
00958      ELSE                                                         ECS052
00959          MOVE    AX-AH-TYP  TO  CLAS-LOOK                         ECS052
00960          PERFORM 530-AH-CLAS-LOOK-RTN THRU 530-EXIT               ECS052
00961          MOVE    CLAS-I-BEN (CLAS-INDEXA) TO P-BENEFIT-CODE1      ECS052
00962          MOVE    CLAS-I-AB3 (CLAS-INDEXA) TO P-BENEFIT-CODE2      ECS052
00963          IF  DTE-CLIENT = 'MIL'                                   ECS052
00964              MOVE AX-IND-GRP TO P-BENEFIT-CODE2B.                 ECS052
00965                                                                   ECS052
00966                                                                   ECS052
00967      MOVE AX-AH-TERM        TO P-TRM.                             ECS052
00968                                                                   ECS052
00969      MOVE AX-AH-REMTERM     TO P-R-TRM.                           ECS052
00970                                                                   ECS052
00971      IF OB-CERT                                                   ECS052
00972          MOVE 'N'             TO OB-SWITCH                        ECS052
00973          MOVE AX-AH-AMT       TO WS-AX-AH-AMT                     ECS052
00974          MOVE WS-AX-AH-AMT-OB TO P-FACE-DOL                       ECS052
00975      ELSE                                                         ECS052
00976          MOVE AX-AH-AMT       TO P-FACE                           ECS052
00977          MOVE AX-AH-REMAMT    TO P-REM.                           ECS052
00978                                                                   ECS052
00979      MOVE AX-AH-PRM         TO  P-PREMIUM.                        ECS052
00980      MOVE AX-AH-CLAIM-PMTS  TO  P-CLAIM-PMTS.                     ECS052
00981      MOVE AX-AH-REFUND      TO  P-REFUND.                         ECS052
00982                                                                   ECS052
00983      PERFORM 550-AH-MSG-FORMAT THRU 550-EXIT.                     ECS052
00984                                                                   ECS052
00985  320-EXIT.                                                        ECS052
00986      EXIT.                                                        ECS052
00987      EJECT                                                        ECS052
00988  400-INITIALIZATION-N-PRINTING  SECTION.                          ECS052
00989                                                                   ECS052
00990  400-INITIALIZATION.                                              ECS052
00991      IF FICH-ONLY                                                 ECS052
00992          MOVE +52 TO MAX-LN.                                      ECS052
00993                                                                   ECS052
00994      MOVE COMPANY-NAME TO HD-CO.                                  ECS052
00995      MOVE ALPH-DATE    TO HD-DT.                                  ECS052
00996                                                                   ECS052
00997      OPEN OUTPUT PRNTR.                                           ECS052
00998      MOVE WS-CURRENT-DATE TO HD-RD.                               ECS052
00999      COMPUTE CUR-DATE = (RUN-CCYY * 12) + RUN-MO.                 ECS052
01000 **                                                                ECS052
01001 ** SET UP THE REPORT SEQUENCE TITLE IN HD3 AND                    ECS052
01002 ** SET UP THE VARIABLE REPORT HEADINGS IN HD4.                    ECS052
01003 **                                                                ECS052
01004      IF  DTE-PGM-OPT = 2                                          ECS052
01005          MOVE SORT-SEQ2 TO HD3-RPT-SEQ                            ECS052
01006          MOVE HD4-SEQ2  TO HD4-SEQ-TITLE                          ECS052
01007          MOVE SPACES    TO HD4-ACCOUNT-TITLE                      ECS052
01008                            HD4-CERT-TITLE                         ECS052
01009      ELSE                                                         ECS052
01010          IF  DTE-PGM-OPT = 3                                      ECS052
01011              MOVE SORT-SEQ3 TO HD3-RPT-SEQ                        ECS052
01012              MOVE HD4-SEQ3  TO HD4-SEQ-TITLE                      ECS052
01013              MOVE SPACES    TO HD4-ACCOUNT-TITLE                  ECS052
01014                                HD4-CERT-TITLE                     ECS052
01015                                HD4-ST-TITLE                       ECS052
01016          ELSE                                                     ECS052
01017              IF  DTE-PGM-OPT = 4                                  ECS052
01018                  MOVE SORT-SEQ4 TO HD3-RPT-SEQ                    ECS052
01019                  MOVE HD4-SEQ4  TO HD4-SEQ-TITLE                  ECS052
01020                  MOVE SPACES    TO HD4-ACCOUNT-TITLE              ECS052
01021                                    HD4-NAME-TITLE                 ECS052
01022                                    HD4-INIT-TITLE                 ECS052
01023              ELSE                                                 ECS052
01024                  IF DTE-PGM-OPT = 5                               ECS052
01025                      MOVE SORT-SEQ5 TO HD3-RPT-SEQ                ECS052
01026                      MOVE HD4-SEQ5  TO HD4-SEQ-TITLE              ECS052
01027                      MOVE SPACES    TO HD4-ACCOUNT-TITLE          ECS052
01028                                        HD4-NAME-TITLE             ECS052
01029                                        HD4-INIT-TITLE             ECS052
01030                  ELSE                                             ECS052
01031                      IF DTE-PGM-OPT = 6                           ECS052
01032                          MOVE SORT-SEQ6 TO HD3-RPT-SEQ            ECS052
01033                          MOVE HD4-SEQ6  TO HD4-SEQ-TITLE          ECS052
01034                          MOVE SPACES    TO HD4-ACCOUNT-TITLE      ECS052
01035                                            HD4-NAME-TITLE         ECS052
01036                                            HD4-INIT-TITLE         ECS052
01037                                            HD4-ISSUE-TITLE        ECS052
01038                      ELSE                                         ECS052
01039                          IF DTE-PGM-OPT = 7                       ECS052
01040                              MOVE SORT-SEQ7 TO HD3-RPT-SEQ        ECS052
01041                              MOVE HD4-SEQ7  TO HD4-SEQ-TITLE      ECS052
01042                              MOVE SPACES    TO HD4-C-TITLE        ECS052
01043                                                HD4-CERT-TITLE     ECS052
01044                          ELSE                                     ECS052
01045                              IF DTE-PGM-OPT = 8                   ECS052
01046                                  MOVE SORT-SEQ8 TO HD3-RPT-SEQ    ECS052
01047                                  MOVE HD4-SEQ8  TO HD4-SEQ-TITLE  ECS052
01048                                  MOVE SPACES    TO HD4-C-TITLE    ECS052
01049                                                    HD4-CERT-TITLE ECS052
01050                              ELSE                                 ECS052
01051                                  MOVE SORT-SEQ1 TO HD3-RPT-SEQ    ECS052
01052                                  MOVE HD4-SEQ1                    ECS052
01053                                             TO HD4-SEQ-TITLE      ECS052
01054                                  MOVE SPACES                      ECS052
01055                                             TO HD4-ACCOUNT-TITLE  ECS052
01056                                                HD4-CERT-TITLE.    ECS052
01057                                                                   ECS052
01058  400-EXIT.                                                        ECS052
01059      EXIT.                                                        ECS052
01060      EJECT                                                        ECS052
01061  410-PRNT-PG-HEADINGS.                                            ECS052
01062      ADD     1             TO  PG-NO.                             ECS052
01063      MOVE    PG-NO         TO  HD-PG.                             ECS052
01064      MOVE    HD1           TO  P-DATA.                            ECS052
01065      MOVE    '1'           TO  P-CTL.                             ECS052
01066      PERFORM 420-PRINT-DETAIL THRU 420-EXIT.                      ECS052
01067                                                                   ECS052
01068      MOVE    HD2           TO  P-DATA.                            ECS052
01069      MOVE    SPACES        TO  P-CTL.                                CL**2
01070      PERFORM 420-PRINT-DETAIL THRU 420-EXIT.                      ECS052
01071                                                                   ECS052
01072      MOVE    HD3           TO  P-DATA.                            ECS052
01073      MOVE    SPACES        TO  P-CTL.                                CL**2
01074      PERFORM 420-PRINT-DETAIL THRU 420-EXIT.                      ECS052
01075                                                                   ECS052
01076      MOVE    HD4           TO  P-DATA.                            ECS052
01077      MOVE    '0'           TO  P-CTL.                             ECS052
01078      PERFORM 420-PRINT-DETAIL THRU 420-EXIT.                      ECS052
01079                                                                   ECS052
01080      MOVE    HD5           TO  P-DATA.                            ECS052
01081      MOVE    SPACES        TO  P-CTL.                                CL**2
01082      PERFORM 420-PRINT-DETAIL THRU 420-EXIT.                      ECS052
01083                                                                   ECS052
01084      MOVE    HD6           TO  P-DATA.                            ECS052
01085      MOVE    SPACES        TO  P-CTL.                                CL**2
01086      PERFORM 420-PRINT-DETAIL THRU 420-EXIT.                      ECS052
01087                                                                   ECS052
01088      MOVE    SPACES        TO  P-DATA                                CL**2
01089                                P-CTL.                             ECS052
01090      PERFORM 420-PRINT-DETAIL THRU 420-EXIT.                      ECS052
01091                                                                   ECS052
01092      MOVE    8             TO  LN-CT.                             ECS052
01093      MOVE    SPACES        TO  PRT.                               ECS052
01094                                                                   ECS052
01095  410-EXIT.                                                        ECS052
01096      EXIT.                                                        ECS052
01097      EJECT                                                        ECS052
01098  420-PRINT-DETAIL.                                                ECS052
01099      MOVE P-CTL TO X.                                             ECS052
01100                           COPY ELCPRT2.                           ECS052
01101                                                                   ECS052
01102      ADD 1 TO LN-CT.                                              ECS052
01103                                                                   ECS052
01104  420-EXIT.                                                        ECS052
01105      EXIT.                                                        ECS052
01106      EJECT                                                        ECS052
01107  500-DATE-FORMAT1.                                                ECS052
01108      MOVE WS-DATEIN-YR  TO  WS-DATE1-YR.                          ECS052
01109      MOVE WS-DATEIN-MO  TO  WS-DATE1-MO.                          ECS052
01110      MOVE WS-DATEIN-DA  TO  WS-DATE1-DA.                          ECS052
01111                                                                   ECS052
01112      MOVE '/'           TO  WS-DATE1-SLH1                         ECS052
01113                             WS-DATE1-SLH2.                        ECS052
01114                                                                   ECS052
01115  500-EXIT.                                                        ECS052
01116      EXIT.                                                        ECS052
01117                                                                   ECS052
01118  510-DATE-FORMAT2.                                                ECS052
01119      MOVE WS-DATEIN-YR  TO  WS-DATE2-YR.                          ECS052
01120      MOVE WS-DATEIN-MO  TO  WS-DATE2-MO.                          ECS052
01121      MOVE '/'           TO  WS-DATE2-SLH1.                        ECS052
01122                                                                   ECS052
01123  510-EXIT.                                                        ECS052
01124      EXIT.                                                        ECS052
01125      EJECT                                                        ECS052
01126  520-LIFE-CLAS-LOOK-RTN.                                          ECS052
01127      MOVE CLAS-STARTL TO CLAS-INDEXL.                             ECS052
01128                                                                   ECS052
01129  520-LIFE-CLAS-LOOP.                                              ECS052
01130      IF    CLAS-INDEXL GREATER CLAS-MAXL                          ECS052
01131         OR CLAS-STARTL = ZEROS                                    ECS052
01132            GO TO 520-LIFE-CLAS-ABEND.                             ECS052
01133                                                                   ECS052
01134      IF CLAS-LOOK = CLAS-I-BEN (CLAS-INDEXL)                      ECS052
01135          GO TO 520-BAL-TEST.                                      ECS052
01136                                                                   ECS052
01137      ADD +1 TO CLAS-INDEXL.                                       ECS052
01138      GO TO 520-LIFE-CLAS-LOOP.                                       CL**5
01139                                                                   ECS052
01140  520-LIFE-CLAS-ABEND.                                             ECS052
01141      MOVE +0401 TO WS-RETURN-CODE.                                ECS052
01142      MOVE 'LIFE BENEFIT NOT IN TABLE' TO WS-ABEND-MESSAGE.        ECS052
01143      GO TO ABEND-PGM.                                             ECS052
01144                                                                   ECS052
01145  520-BAL-TEST.                                                    ECS052
01146      IF CLAS-I-BAL (CLAS-INDEXL) = 'B' OR 'Z'                     ECS052
01147          MOVE 'Y' TO OB-SWITCH.                                   ECS052
01148                                                                   ECS052
01149  520-EXIT.                                                        ECS052
01150      EXIT.                                                        ECS052
01151      EJECT                                                        ECS052
01152  530-AH-CLAS-LOOK-RTN.                                            ECS052
01153      MOVE CLAS-STARTA TO CLAS-INDEXA.                             ECS052
01154                                                                   ECS052
01155  530-AH-CLAS-LOOP.                                                ECS052
01156      IF   CLAS-INDEXA GREATER CLAS-MAXA                           ECS052
01157        OR CLAS-STARTA = ZEROS                                     ECS052
01158           GO TO 530-AH-CLAS-ABEND.                                ECS052
01159                                                                   ECS052
01160      IF CLAS-LOOK = CLAS-I-BEN (CLAS-INDEXA)                      ECS052
01161          GO TO 530-AH-BAL-TEST.                                   ECS052
01162                                                                   ECS052
01163      ADD +1 TO CLAS-INDEXA.                                       ECS052
01164      GO TO 530-AH-CLAS-LOOP.                                         CL**5
01165                                                                   ECS052
01166  530-AH-CLAS-ABEND.                                               ECS052
01167      MOVE +0402 TO WS-RETURN-CODE.                                ECS052
01168      MOVE 'AH BENEFIT NOT IN TABLE' TO WS-ABEND-MESSAGE.          ECS052
01169      GO TO ABEND-PGM.                                             ECS052
01170                                                                   ECS052
01171  530-AH-BAL-TEST.                                                 ECS052
01172      IF CLAS-I-BAL (CLAS-INDEXA) = 'B' OR 'Z'                     ECS052
01173          MOVE 'Y' TO OB-SWITCH.                                   ECS052
01174                                                                   ECS052
01175  530-EXIT.                                                        ECS052
01176      EXIT.                                                        ECS052
01177      EJECT                                                        ECS052
01178  540-LF-MSG-FORMAT.                                               ECS052
01179      IF  AX-LF-STATUS = '5'                                       ECS052
01180          MOVE 'R-IS' TO P-MSG                                     ECS052
01181          GO TO 540-EXIT                                           ECS052
01182      ELSE                                                         ECS052
01183          IF AX-LF-STATUS = '9'                                    ECS052
01184              MOVE 'REIN' TO P-MSG                                 ECS052
01185              GO TO 540-EXIT                                       ECS052
01186          ELSE                                                     ECS052
01187              IF AX-LF-STATUS = 'E'                                ECS052
01188                  MOVE 'EXPR' TO P-MSG                             ECS052
01189                  GO TO 540-EXIT                                   ECS052
01190              ELSE                                                 ECS052
01191                  IF AX-LF-STATUS = 'F'                            ECS052
01192                      MOVE 'FUTR' TO P-MSG                         ECS052
01193                      GO TO 540-EXIT                               ECS052
01194                  ELSE                                             ECS052
01195                      IF AX-LF-STATUS = 'D'                        ECS052
01196                         MOVE 'DCLN' TO P-MSG                      ECS052
01197                         GO TO 540-EXIT                            ECS052
01198                      ELSE                                         ECS052
01199                         IF AX-LF-STATUS = 'V'                     ECS052
01200                            MOVE 'VOID' TO P-MSG                   ECS052
01201                            GO TO 540-EXIT.                        ECS052
01202                                                                   ECS052
01203      IF AX-LF-STATUS = '8'                                        ECS052
01204          MOVE    'CANC'            TO  P-MSG                      ECS052
01205          MOVE    'XIT '            TO  P-EXIT-MSG                 ECS052
01206          MOVE    AX-LF-CNCL        TO  WS-DATE-FORMAT-IN          ECS052
01207          PERFORM 500-DATE-FORMAT1 THRU 500-EXIT                   ECS052
01208          MOVE    WS-DATE-FORMAT1   TO  P-MSG-DATE                 ECS052
01209          MOVE    AX-LF-EXIT        TO  WS-DATE-FORMAT-IN          ECS052
01210          PERFORM 500-DATE-FORMAT1 THRU 500-EXIT                   ECS052
01211          MOVE    WS-DATE-FORMAT1   TO  P-EXIT-DATE                ECS052
01212          GO TO 540-EXIT.                                          ECS052
01213                                                                   ECS052
01214      IF AX-LF-STATUS = '7'                                        ECS052
01215          MOVE    'CLM '            TO  P-MSG                      ECS052
01216          MOVE    'XIT '            TO  P-EXIT-MSG                 ECS052
01217          MOVE    AX-DEATH          TO  WS-DATE-FORMAT-IN          ECS052
01218          PERFORM 500-DATE-FORMAT1 THRU 500-EXIT                   ECS052
01219          MOVE    WS-DATE-FORMAT1   TO  P-MSG-DATE                 ECS052
01220          MOVE    AX-LF-EXIT        TO  WS-DATE-FORMAT-IN          ECS052
01221          PERFORM 500-DATE-FORMAT1 THRU 500-EXIT                   ECS052
01222          MOVE    WS-DATE-FORMAT1   TO  P-EXIT-DATE                ECS052
01223          GO TO 540-EXIT.                                          ECS052
01224                                                                   ECS052
01225  540-EXIT.                                                        ECS052
01226      EXIT.                                                        ECS052
01227      EJECT                                                        ECS052
01228  550-AH-MSG-FORMAT.                                               ECS052
01229      IF  AX-AH-STATUS = '5'                                       ECS052
01230          MOVE 'R-IS' TO P-MSG                                     ECS052
01231          GO TO 550-EXIT                                           ECS052
01232      ELSE                                                         ECS052
01233          IF AX-AH-STATUS = '9'                                    ECS052
01234              MOVE 'REIN' TO P-MSG                                 ECS052
01235              GO TO 550-EXIT                                       ECS052
01236          ELSE                                                     ECS052
01237              IF AX-AH-STATUS = 'E'                                ECS052
01238                  MOVE 'EXPR' TO P-MSG                             ECS052
01239                  GO TO 550-EXIT                                   ECS052
01240              ELSE                                                 ECS052
01241                  IF AX-AH-STATUS = 'F'                            ECS052
01242                      MOVE 'FUTR' TO P-MSG                         ECS052
01243                      GO TO 550-EXIT                               ECS052
01244                  ELSE                                             ECS052
01245                      IF AX-AH-STATUS = 'D'                        ECS052
01246                         MOVE 'DCLN' TO P-MSG                      ECS052
01247                         GO TO 550-EXIT                            ECS052
01248                     ELSE                                          ECS052
01249                         IF AX-AH-STATUS = 'V'                     ECS052
01250                            MOVE 'VOID' TO P-MSG                   ECS052
01251                            GO TO 550-EXIT.                        ECS052
01252                                                                   ECS052
01253      IF AX-AH-STATUS = '8'                                        ECS052
01254          MOVE    'CANC'            TO  P-MSG                      ECS052
01255          MOVE    'XIT '            TO  P-EXIT-MSG                 ECS052
01256          MOVE    AX-AH-CNCL        TO  WS-DATE-FORMAT-IN          ECS052
01257          PERFORM 500-DATE-FORMAT1 THRU 500-EXIT                   ECS052
01258          MOVE    WS-DATE-FORMAT1   TO  P-MSG-DATE                 ECS052
01259          MOVE    AX-AH-EXIT        TO  WS-DATE-FORMAT-IN          ECS052
01260          PERFORM 500-DATE-FORMAT1 THRU 500-EXIT                   ECS052
01261          MOVE    WS-DATE-FORMAT1   TO  P-EXIT-DATE                ECS052
01262          GO TO 550-EXIT.                                          ECS052
01263                                                                   ECS052
01264      IF AX-AH-STATUS = '6'                                        ECS052
01265          MOVE    'LUMP'            TO  P-MSG                      ECS052
01266          MOVE    'XIT '            TO  P-EXIT-MSG                 ECS052
01267          MOVE    AX-LUMP-SUM       TO  WS-DATE-FORMAT-IN          ECS052
01268          PERFORM 500-DATE-FORMAT1 THRU 500-EXIT                   ECS052
01269          MOVE    WS-DATE-FORMAT1   TO  P-MSG-DATE                 ECS052
01270          MOVE    AX-AH-EXIT        TO  WS-DATE-FORMAT-IN          ECS052
01271          PERFORM 500-DATE-FORMAT1 THRU 500-EXIT                   ECS052
01272          MOVE    WS-DATE-FORMAT1   TO  P-EXIT-DATE                ECS052
01273          GO TO 550-EXIT.                                          ECS052
01274                                                                   ECS052
01275      IF AX-AH-PRE-PLST = '6'                                      ECS052
01276          MOVE    'CLM '            TO  P-MSG                      ECS052
01277          IF   AX-LUMP-SUM NUMERIC                                 ECS052
01278           AND AX-LUMP-SUM GREATER THAN ZEROS                      ECS052
01279              MOVE AX-LUMP-SUM          TO  WS-DATE-FORMAT-IN      ECS052
01280              PERFORM 500-DATE-FORMAT1 THRU 500-EXIT               ECS052
01281              MOVE WS-DATE-FORMAT1      TO  P-MSG-DATE             ECS052
01282          ELSE                                                     ECS052
01283              MOVE SPACES               TO  P-MSG-DATE.            ECS052
01284                                                                   ECS052
01285  550-EXIT.                                                        ECS052
01286      EXIT.                                                        ECS052
01287                                                                   ECS052
01288  ABEND-PGM SECTION.                                               ECS052
01289                         COPY ELCABEND.                            ECS052
01290                                                                   ECS052
