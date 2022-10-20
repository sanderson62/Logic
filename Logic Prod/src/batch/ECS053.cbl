00001  IDENTIFICATION DIVISION.                                         03/09/98
00002                                                                   ECS053
00003  PROGRAM-ID.                ECS053.                                  LV006
00004 *              PROGRAM CONVERTED BY                               ECS053
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS053
00006 *              CONVERSION DATE 02/08/96 09:33:58.                 ECS053
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS053
00008 *                           VMOD=2.008.                           ECS053
00009                                                                   ECS053
00010 *AUTHOR.        LOGIC, INC.                                       ECS053
00011 *               DALLAS, TEXAS.                                    ECS053
00012                                                                   ECS053
00013 *DATE-COMPILED.                                                   ECS053
00014                                                                   ECS053
00015 *SECURITY.   *****************************************************ECS053
00016 *            *                                                   *ECS053
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS053
00018 *            *                                                   *ECS053
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS053
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS053
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS053
00022 *            *                                                   *ECS053
00023 *            *****************************************************ECS053
00024                                                                   ECS053
00025 *REMARKS.                                                         ECS053
00026                                                                   ECS053
00027 *    PRINTS A STATUS REPORT FOR ALL CERTS EXPIRED, CANCELLED, OR  ECS053
00028 *    DEATH CLAIMED IN CURRENT MONTH, USING THE FOLLOWING OPTIONS -ECS053
00029                                                                   ECS053
00030 *       OPTION FUNCTION                                           ECS053
00031 *       ------    --------------------------                      ECS053
00032 *         1       CERTIFICATE SEQUENCE                            ECS053
00033 *         2       CERTIFICATE WITHIN ACCOUNT                      ECS053
00034 *         3       ACCOUNT WITHIN STATE WITHIN CERTIFICATE         ECS053
00035 *         4       ALPHA WITHIN ACCOUNT                            ECS053
00036 *         5       ACCOUNT WITHIN ALPHA                            ECS053
00037 *         6       NAME WITHIN EFF DATE WITHIN ACCOUNT             ECS053
00038 *         7       CERTIFICATE WITHIN CARRIER                      ECS053
00039                                                                   ECS053
00040  ENVIRONMENT DIVISION.                                            ECS053
00041  INPUT-OUTPUT SECTION.                                            ECS053
00042  FILE-CONTROL.                                                    ECS053
00043                                                                   ECS053
00044      SELECT SORT-FILE    ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.      ECS053
00045      SELECT PRNTR        ASSIGN TO SYS008-UR-1403-S-SYS008.       ECS053
00046      SELECT ALPH-EXTC    ASSIGN TO SYS013-UT-2400-S-SYS013.       ECS053
00047      SELECT DISK-DATE    ASSIGN TO SYS019-UT-3380-S-SYS019.       ECS053
00048      SELECT FICH         ASSIGN TO SYS020-UT-2400-S-SYS020.       ECS053
00049      EJECT                                                        ECS053
00050  DATA DIVISION.                                                   ECS053
00051  FILE SECTION.                                                    ECS053
00052                                                                   ECS053
00053  SD  SORT-FILE.                                                   ECS053
00054                                                                   ECS053
00055  01  SORT-FILE-REC.                                               ECS053
00056      12  FILLER              PIC XXX.                             ECS053
00057      12  SORT-CARRIER        PIC X.                               ECS053
00058      12  SORT-GROUP          PIC X(6).                            ECS053
00059      12  SORT-STATE          PIC XX.                              ECS053
00060      12  SORT-ACCT           PIC X(10).                           ECS053
00061      12  SORT-DATE           PIC X(6).                            ECS053
00062      12  SORT-CERT           PIC X(11).                           ECS053
00063      12  SORT-NAME           PIC X(26).                           ECS053
00064      12  FILLER              PIC X(235).                          ECS053
00065  EJECT                                                            ECS053
00066  FD  PRNTR                                                        ECS053
00067                              COPY ELCPRTFD.                       ECS053
00068  EJECT                                                            ECS053
00069  FD  ALPH-EXTC                                                    ECS053
00070                              COPY ECSAEXFD.                       ECS053
00071  EJECT                                                            ECS053
00072  FD  DISK-DATE                                                    ECS053
00073                              COPY ELCDTEFD.                       ECS053
00074  EJECT                                                            ECS053
00075  FD  FICH                                                         ECS053
00076                              COPY ELCFCHFD.                       ECS053
00077  EJECT                                                            ECS053
00078  WORKING-STORAGE SECTION.                                         ECS053
00079  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.   ECS053
00080  01  LCP-CURRENT-DATE-68.                                         ECS053
00081      05  LCP-MONTH                 PIC X(2).                      ECS053
00082      05  FILLER                    PIC X VALUE '/'.               ECS053
00083      05  LCP-DAY1                  PIC X(2).                      ECS053
00084      05  FILLER                    PIC X VALUE '/'.               ECS053
00085      05  LCP-YEAR                  PIC X(2).                      ECS053
00086  01  LCP-DATE-NEW-74.                                             ECS053
00087      05  LCP-YEAR                  PIC X(2).                      ECS053
00088      05  LCP-MONTH                 PIC X(2).                      ECS053
00089      05  LCP-DAY1                  PIC X(2).                      ECS053
00090  77  FILLER  PIC X(32) VALUE '********************************'.  ECS053
00091  77  FILLER  PIC X(32) VALUE '     ECS053 WORKING-STORAGE     '.  ECS053
00092  77  FILLER  PIC X(32) VALUE '******* VMOD=2.008 *************'.  ECS053
00093                                                                   ECS053
00094  77  OB-SWITCH               PIC X               VALUE 'N'.       ECS053
00095      88  OB-CERT                                 VALUE 'Y'.       ECS053
00096                                                                   ECS053
00097  01  WS.                                                          ECS053
00098      12  WS-RETURN-CODE       PIC S9(4) COMP      VALUE +0.       ECS053
00099      12  WS-ABEND-MESSAGE     PIC  X(80)          VALUE SPACES.   ECS053
00100      12  WS-ABEND-FILE-STATUS PIC  XX             VALUE ZEROS.    ECS053
00101      12  WS-ZERO              PIC S9              VALUE +0.       ECS053
00102                                                                   ECS053
00103  01  SORT-INFO.                                                   ECS053
00104      05  RECS-RETURNED-FLAG   PIC X               VALUE 'N'.      ECS053
00105          88  ALL-RECS-RETURNED                    VALUE 'Y'.      ECS053
00106      05  TOTAL-RECS-RETURNED  PIC 9(9) COMP-3     VALUE ZEROS.    ECS053
00107                                                                   ECS053
00108  01  PGM-SUB                 PIC S999    COMP    VALUE +053.      ECS053
00109                                                                   ECS053
00110  01  WS-AX-LF-AMT            PIC S9(9)V99 COMP-3 VALUE ZEROS.     ECS053
00111                                                                   ECS053
00112  01  WS-AX-LF-AMT-OB REDEFINES                                    ECS053
00113      WS-AX-LF-AMT            PIC S9(11)    COMP-3.                ECS053
00114                                                                   ECS053
00115  01  WS-AX-AH-AMT            PIC S9(5)V99 COMP-3 VALUE ZEROS.     ECS053
00116                                                                   ECS053
00117  01  WS-AX-AH-AMT-OB REDEFINES                                    ECS053
00118      WS-AX-AH-AMT            PIC S9(7)    COMP-3.                 ECS053
00119                                                                   ECS053
00120  01  LF-AH-WORK-AREAS.                                            ECS053
00121      05  WS-XX-AMT           PIC S9(9)V99    VALUE ZEROS.         ECS053
00122      05  WS-XX-REMAMT        PIC S9(9)V99    VALUE ZEROS.         ECS053
00123      05  WS-XX-PREM          PIC S9(9)V99    VALUE ZEROS.         ECS053
00124                                                                   ECS053
00125  01  WORK-DATES.                                                  ECS053
00126      05  EXP-DATE            PIC S9(5)    COMP-3 VALUE ZEROS.     ECS053
00127      05  CUR-DATE            PIC S9(5)    COMP-3 VALUE ZEROS.     ECS053
00128      05  WS-DATE-FORMAT-IN   PIC 9(11)      VALUE 0.                 CL**5
00129      05  WS-DATE-FORMAT-IN-R REDEFINES WS-DATE-FORMAT-IN.            CL**4
00130          10  FILLER           PIC  999.                              CL**5
00131          10  WS-DATEIN-CCYY   PIC 9(04).                             CL**5
00132          10  WS-DATEIN-CCYR REDEFINES WS-DATEIN-CCYY.             ECS053
00133              15  WS-DATEIN-CC PIC  99.                            ECS053
00134              15  WS-DATEIN-YR PIC  99.                            ECS053
00135          10  WS-DATEIN-MO     PIC  99.                               CL**5
00136          10  WS-DATEIN-DA     PIC  99.                               CL**5
00137      05  WS-DATE-FORMAT1.                                         ECS053
00138          10  WS-DATE1-MO      PIC  99       VALUE ZEROS.          ECS053
00139          10  WS-DATE1-SLH1    PIC  X        VALUE '/'.            ECS053
00140          10  WS-DATE1-DA      PIC  99       VALUE ZEROS.          ECS053
00141          10  WS-DATE1-SLH2    PIC  X        VALUE '/'.            ECS053
00142          10  WS-DATE1-YR      PIC  99       VALUE ZEROS.          ECS053
00143      05  WS-DATE-FORMAT2.                                         ECS053
00144          10  WS-DATE2-MO      PIC  99       VALUE ZEROS.          ECS053
00145          10  WS-DATE2-SLH1    PIC  X        VALUE '/'.            ECS053
00146          10  WS-DATE2-YR      PIC  99       VALUE ZEROS.          ECS053
00147                                                                   ECS053
00148  01  MISC-WORK-AREAS.                                             ECS053
00149      12  LST-COMP            PIC X(4)            VALUE SPACES.    ECS053
00150      12  LAS-AX-CONTROL      PIC X(36)           VALUE SPACES.    ECS053
00151      12  X                   PIC X               VALUE SPACE.     ECS053
00152      12  LAST-CARRIER        PIC X               VALUE SPACE.     ECS053
00153                                                                   ECS053
00154  01  COMP-3-WORK         COMP-3.                                  ECS053
00155      12  PG-NO               PIC 9(5)            VALUE  0.        ECS053
00156      12  LN-CT               PIC 9(5)            VALUE  80.       ECS053
00157      12  MAX-LN              PIC 9(5)            VALUE  70.       ECS053
00158      12  TOT-REM-PRM         PIC S9(9)V99        VALUE ZEROS.     ECS053
00159      12  FIN-REM-PRM         PIC S9(9)V99        VALUE ZEROS.     ECS053
00160      12  TOT-CRT-CNT         PIC S9(9)V99        VALUE ZEROS.     ECS053
00161      12  FIN-CRT-CNT         PIC S9(9)V99        VALUE ZEROS.     ECS053
00162      12  TOT-CAR-CNT         PIC S9(9)V99        VALUE ZEROS.     ECS053
00163      12  TOT-CAR-PRM         PIC S9(9)V99        VALUE ZEROS.     ECS053
00164                                                                   ECS053
00165                                                                   ECS053
00166  01  WORK-ALPH.                                                   ECS053
00167      12  FILLER              PIC X.                               ECS053
00168      12  AX-CNTRL            PIC X(12).                           ECS053
00169      12  FILLER              PIC X(6).                            ECS053
00170  EJECT                                                            ECS053
00171                              COPY ECSAEX01.                       ECS053
00172  EJECT                                                            ECS053
00173  01  RPT-TITLE-WORDS.                                             ECS053
00174      05  SORT-SEQ1          PIC X(20)  VALUE                      ECS053
00175          'CERTIFICATE, ACCOUNT'.                                  ECS053
00176      05  SORT-SEQ2          PIC X(20)  VALUE                      ECS053
00177          'ACCOUNT, CERTIFICATE'.                                  ECS053
00178      05  SORT-SEQ3          PIC X(20)  VALUE                      ECS053
00179          'CERT, STATE, ACCOUNT'.                                  ECS053
00180      05  SORT-SEQ4          PIC X(20)  VALUE                      ECS053
00181          'ACCOUNT, NAME       '.                                  ECS053
00182      05  SORT-SEQ5          PIC X(20)  VALUE                      ECS053
00183          'NAME, ACCOUNT       '.                                  ECS053
00184      05  SORT-SEQ6          PIC X(20)  VALUE                      ECS053
00185          'ACCT, EFF. DTE, NAME'.                                  ECS053
00186      05  SORT-SEQ7          PIC X(20)  VALUE                      ECS053
00187          'CARRIER, CERTIFICATE'.                                  ECS053
00188                                                                   ECS053
00189  01  HD4-BY-REPORT-SEQUENCE.                                      ECS053
00190      05  HD4-SEQ1.                                                ECS053
00191          10  FILLER              PIC X(12)   VALUE                ECS053
00192              'CERTIFICATE '.                                      ECS053
00193          10  FILLER              PIC X(7)    VALUE                ECS053
00194              'ACCOUNT'.                                           ECS053
00195          10  FILLER              PIC X(19)   VALUE  SPACES.       ECS053
00196      05  HD4-SEQ2.                                                ECS053
00197          10  FILLER              PIC X(7)    VALUE                ECS053
00198              'ACCOUNT'.                                           ECS053
00199          10  FILLER              PIC X(4)    VALUE  SPACES.       ECS053
00200          10  FILLER              PIC X(11)   VALUE                ECS053
00201              'CERTIFICATE'.                                       ECS053
00202          10  FILLER              PIC X(16)   VALUE  SPACES.       ECS053
00203      05  HD4-SEQ3.                                                ECS053
00204          10  FILLER              PIC X(12)   VALUE                ECS053
00205              'CERTIFICATE '.                                      ECS053
00206          10  FILLER              PIC X(3)    VALUE                ECS053
00207              'ST '.                                               ECS053
00208          10  FILLER              PIC X(7)    VALUE                ECS053
00209              'ACCOUNT'.                                           ECS053
00210          10  FILLER              PIC X(16)   VALUE  SPACES.       ECS053
00211      05  HD4-SEQ4.                                                ECS053
00212          10  FILLER              PIC X(7)    VALUE                ECS053
00213              'ACCOUNT'.                                           ECS053
00214          10  FILLER              PIC X(4)    VALUE  SPACES.       ECS053
00215          10  FILLER              PIC X(4)    VALUE                ECS053
00216              'NAME'.                                              ECS053
00217          10  FILLER              PIC X(10)   VALUE  SPACES.       ECS053
00218          10  FILLER              PIC X(4)    VALUE                ECS053
00219              'INIT'.                                              ECS053
00220          10  FILLER              PIC X(9)    VALUE  SPACES.       ECS053
00221      05  HD4-SEQ5.                                                ECS053
00222          10  FILLER              PIC X(4)    VALUE                ECS053
00223              'NAME'.                                              ECS053
00224          10  FILLER              PIC X(10)   VALUE  SPACES.       ECS053
00225          10  FILLER              PIC X(5)    VALUE                ECS053
00226              'INIT '.                                             ECS053
00227          10  FILLER              PIC X(7)    VALUE                ECS053
00228              'ACCOUNT'.                                           ECS053
00229          10  FILLER              PIC X(12)   VALUE  SPACES.       ECS053
00230      05  HD4-SEQ6.                                                ECS053
00231          10  FILLER              PIC X(7)    VALUE                ECS053
00232              'ACCOUNT'.                                           ECS053
00233          10  FILLER              PIC X(4)    VALUE  SPACES.       ECS053
00234          10  FILLER              PIC X(13)   VALUE                ECS053
00235              'ISSUE DT NAME'.                                     ECS053
00236          10  FILLER              PIC X(9)    VALUE  SPACES.       ECS053
00237          10  FILLER              PIC X(4)    VALUE                ECS053
00238              'INIT'.                                              ECS053
00239          10  FILLER              PIC X       VALUE  SPACES.       ECS053
00240      05  HD4-SEQ7.                                                ECS053
00241          10  FILLER              PIC X(16)   VALUE                ECS053
00242              'CARR CERTIFICATE'.                                  ECS053
00243          10  FILLER              PIC X(22)   VALUE  SPACES.       ECS053
00244      EJECT                                                        ECS053
00245  01  HD1.                                                         ECS053
00246      12  FILLER              PIC X(49)       VALUE SPACES.        ECS053
00247      12  FILLER              PIC X(26)       VALUE                ECS053
00248              'EXITING CERTIFICATE STATUS'.                        ECS053
00249      12  FILLER              PIC X(44)       VALUE SPACES.        ECS053
00250      12  FILLER              PIC X(8)        VALUE 'ECS053  '.    ECS053
00251                                                                   ECS053
00252  01  HD2.                                                         ECS053
00253      12  FILLER              PIC X(47)       VALUE SPACES.        ECS053
00254      12  HD-CO               PIC X(30).                           ECS053
00255      12  FILLER              PIC X(42)       VALUE SPACES.        ECS053
00256      12  HD-RD               PIC X(8).                            ECS053
00257                                                                   ECS053
00258  01  HD3.                                                         ECS053
00259      12  FILLER              PIC X(13)       VALUE                ECS053
00260          ' REPORT SEQ- '.                                         ECS053
00261      12  HD3-RPT-SEQ         PIC X(20)       VALUE SPACES.        ECS053
00262      12  FILLER              PIC X(20)       VALUE SPACES.        ECS053
00263      12  HD-DT               PIC X(18).                           ECS053
00264      12  FILLER              PIC X(48)       VALUE SPACES.        ECS053
00265      12  FILLER              PIC X(5)        VALUE 'PAGE'.        ECS053
00266      12  HD-PG               PIC ZZ,ZZ9.                          ECS053
00267                                                                   ECS053
00268  01  HD4.                                                         ECS053
00269      05  HD4-SEQ-TITLE           PIC X(38)   VALUE  SPACES.       ECS053
00270      05  HD4-C-TITLE             PIC X(4)    VALUE                ECS053
00271          'CARR'.                                                  ECS053
00272      05  FILLER                  PIC X       VALUE  SPACES.       ECS053
00273      05  FILLER                  PIC X(5)    VALUE                ECS053
00274          'GROUP'.                                                 ECS053
00275      05  FILLER                  PIC XX      VALUE  SPACES.       ECS053
00276      05  HD4-ST-TITLE            PIC XX      VALUE                ECS053
00277          'ST'.                                                    ECS053
00278      05  FILLER                  PIC X       VALUE  SPACES.       ECS053
00279      05  HD4-ACCOUNT-TITLE       PIC X(7)    VALUE                ECS053
00280          'ACCOUNT'.                                               ECS053
00281      05  FILLER                  PIC X(4)    VALUE  SPACES.       ECS053
00282      05  HD4-CERT-TITLE          PIC X(11)   VALUE                ECS053
00283          'CERTIFICATE'.                                           ECS053
00284      05  FILLER                  PIC X       VALUE  SPACES.       ECS053
00285      05  HD4-NAME-TITLE          PIC X(4)    VALUE                ECS053
00286          'NAME'.                                                  ECS053
00287      05  FILLER                  PIC X(10)   VALUE SPACES.        ECS053
00288      05  HD4-INIT-TITLE          PIC X(5)    VALUE                ECS053
00289          'INIT '.                                                 ECS053
00290      05  FILLER                  PIC X(4)    VALUE                ECS053
00291          'AGE '.                                                  ECS053
00292      05  HD4-ISSUE-TITLE         PIC X(8)    VALUE                ECS053
00293          'ISSUE DT'.                                              ECS053
00294      05  FILLER                  PIC X(24)   VALUE                ECS053
00295          '  ENTRY REIN PF   APR   '.                              ECS053
00296  01  HD5.                                                         ECS053
00297      12  FILLER              PIC X(38)           VALUE SPACES.    ECS053
00298      12  FILLER              PIC X(5)            VALUE            ECS053
00299          'TYPE '.                                                 ECS053
00300      12  FILLER              PIC X(13)           VALUE            ECS053
00301          'BEN   ORG REM'.                                         ECS053
00302      12  FILLER              PIC X(4)            VALUE SPACES.    ECS053
00303      12  FILLER              PIC X(8)            VALUE            ECS053
00304          'ORIGINAL'.                                              ECS053
00305      12  FILLER              PIC X(3)            VALUE SPACES.    ECS053
00306      12  FILLER              PIC X(9)            VALUE            ECS053
00307          'REMAINING'.                                             ECS053
00308      12  FILLER              PIC X(5)            VALUE SPACES.    ECS053
00309      12  FILLER              PIC X(7)            VALUE            ECS053
00310          'PREMIUM'.                                               ECS053
00311      12  FILLER              PIC X(6)            VALUE SPACES.    ECS053
00312      12  FILLER              PIC X(5)            VALUE            ECS053
00313          'CLAIM'.                                                 ECS053
00314      12  FILLER              PIC X(7)            VALUE SPACES.    ECS053
00315      12  FILLER              PIC X(6)            VALUE            ECS053
00316          'REFUND'.                                                ECS053
00317      12  FILLER              PIC X(3)            VALUE SPACES.    ECS053
00318      12  FILLER              PIC X(14)           VALUE            ECS053
00319          'MSG  MSG DATE '.                                        ECS053
00320                                                                   ECS053
00321  01  HD6.                                                         ECS053
00322      12  FILLER              PIC X(49)           VALUE SPACES.    ECS053
00323      12  FILLER              PIC X(7)            VALUE            ECS053
00324          'TRM TRM'.                                               ECS053
00325      12  FILLER              PIC X(5)            VALUE SPACES.    ECS053
00326      12  FILLER              PIC X(7)            VALUE            ECS053
00327          'BENEFIT'.                                               ECS053
00328      12  FILLER              PIC X(5)            VALUE SPACES.    ECS053
00329      12  FILLER              PIC X(7)            VALUE            ECS053
00330          'BENEFIT'.                                               ECS053
00331      12  FILLER              PIC X(52)           VALUE SPACES.    ECS053
00332      EJECT                                                        ECS053
00333  01  DTL.                                                         ECS053
00334      05  P-DTL-LINE1.                                             ECS053
00335          10  FILLER              PIC X.                           ECS053
00336          10  P-DTL-L1-SEQ1.                                       ECS053
00337              15  P-CERT-SEQ1        PIC X(11).                    ECS053
00338              15  FILLER             PIC X.                        ECS053
00339              15  P-ACCT-SEQ1        PIC X(10).                    ECS053
00340              15  FILLER             PIC X(19).                    ECS053
00341          10  P-DTL-L1-SEQ2 REDEFINES P-DTL-L1-SEQ1.               ECS053
00342              15  P-ACCT-SEQ2        PIC X(10).                    ECS053
00343              15  FILLER             PIC X.                        ECS053
00344              15  P-CERT-SEQ2        PIC X(11).                    ECS053
00345              15  FILLER             PIC X(19).                    ECS053
00346          10  P-DTL-L1-SEQ3 REDEFINES P-DTL-L1-SEQ1.               ECS053
00347              15  P-CERT-SEQ3        PIC X(11).                    ECS053
00348              15  FILLER             PIC X.                        ECS053
00349              15  P-ST-SEQ3          PIC XX.                       ECS053
00350              15  FILLER             PIC X.                        ECS053
00351              15  P-ACCT-SEQ3        PIC X(10).                    ECS053
00352              15  FILLER             PIC X(16).                    ECS053
00353          10  P-DTL-L1-SEQ4 REDEFINES P-DTL-L1-SEQ1.               ECS053
00354              15  P-ACCT-SEQ4        PIC X(10).                    ECS053
00355              15  FILLER             PIC X.                        ECS053
00356              15  P-NAME-SEQ4        PIC X(15).                    ECS053
00357              15  FILLER             PIC X.                        ECS053
00358              15  P-NAME-INIT1-SEQ4  PIC X.                        ECS053
00359              15  P-NAME-INIT2-SEQ4  PIC X.                        ECS053
00360              15  FILLER             PIC X(12).                    ECS053
00361          10  P-DTL-L1-SEQ5 REDEFINES P-DTL-L1-SEQ1.               ECS053
00362              15  P-NAME-SEQ5        PIC X(15).                    ECS053
00363              15  FILLER             PIC X.                        ECS053
00364              15  P-NAME-INIT1-SEQ5  PIC X.                        ECS053
00365              15  P-NAME-INIT2-SEQ5  PIC X.                        ECS053
00366              15  FILLER             PIC X.                        ECS053
00367              15  P-ACCT-SEQ5        PIC X(10).                    ECS053
00368              15  FILLER             PIC X(12).                    ECS053
00369          10  P-DTL-L1-SEQ6 REDEFINES P-DTL-L1-SEQ1.               ECS053
00370              15  P-ACCT-SEQ6        PIC X(10).                    ECS053
00371              15  FILLER             PIC X.                        ECS053
00372              15  P-ISSUE-DATE-SEQ6  PIC X(8).                     ECS053
00373              15  FILLER             PIC X.                        ECS053
00374              15  P-NAME-SEQ6        PIC X(15).                    ECS053
00375              15 P-NAME-INIT1-SEQ6   PIC X.                        ECS053
00376              15 P-NAME-INIT2-SEQ6   PIC X.                        ECS053
00377              15 FILLER              PIC X(4).                     ECS053
00378          10  P-DTL-L1-SEQ7 REDEFINES P-DTL-L1-SEQ1.               ECS053
00379              15  FILLER             PIC X(3).                     ECS053
00380              15  P-CARRIER-SEQ7     PIC X.                        ECS053
00381              15  FILLER             PIC X.                        ECS053
00382              15  P-CERT-SEQ7        PIC X(11).                    ECS053
00383              15  FILLER             PIC X(23).                    ECS053
00384          10  P-DTL-L1-REST.                                       ECS053
00385              15  P-CARRIER          PIC X.                        ECS053
00386              15  FILLER             PIC X.                        ECS053
00387              15  P-GROUP            PIC X(6).                     ECS053
00388              15  FILLER             PIC X.                        ECS053
00389              15  P-ST               PIC XX.                       ECS053
00390              15  FILLER             PIC X.                        ECS053
00391              15  P-ACCT             PIC X(10).                    ECS053
00392              15  FILLER             PIC X.                        ECS053
00393              15  P-CERT             PIC X(11).                    ECS053
00394              15  FILLER             PIC X.                        ECS053
00395              15  P-NAME             PIC X(15).                    ECS053
00396              15  FILLER             PIC X.                        ECS053
00397              15  P-NAME-INIT1       PIC X.                        ECS053
00398              15  P-NAME-INIT2       PIC X.                        ECS053
00399              15  FILLER             PIC X.                        ECS053
00400              15  P-AGE              PIC XX.                       ECS053
00401              15  FILLER             PIC X.                        ECS053
00402              15  P-ISSUE-DATE       PIC X(8).                     ECS053
00403              15  FILLER             PIC X.                        ECS053
00404              15  P-SR               PIC X.                        ECS053
00405              15  FILLER             PIC X.                        ECS053
00406              15  P-ENTRY-DATE       PIC X(5).                     ECS053
00407              15  FILLER             PIC X.                        ECS053
00408              15  P-REIN-CODE.                                     ECS053
00409                  20 P-REIN-CODE1    PIC X.                        ECS053
00410                  20 P-REIN-CODE2    PIC X.                        ECS053
00411                  20 P-REIN-CODE3    PIC X.                        ECS053
00412              15  FILLER             PIC XX.                       ECS053
00413              15  P-PF               PIC 99      BLANK WHEN ZERO.  ECS053
00414              15  FILLER             PIC XX.                       ECS053
00415              15  P-APR              PIC ZZZ.9999 BLANK WHEN ZERO. ECS053
00416                                                                   ECS053
00417      05  P-DTL-LINE2 REDEFINES P-DTL-LINE1.                       ECS053
00418          10  FILLER                 PIC X(41).                    ECS053
00419          10  P-TYPE-L-A             PIC XX.                       ECS053
00420          10  FILLER                 PIC X.                        ECS053
00421          10  P-BENEFIT-CODE1        PIC XX.                       ECS053
00422          10  P-BENEFIT-CODE2.                                     ECS053
00423              15  P-BENEFIT-CODE2A   PIC XX.                       ECS053
00424              15  P-BENEFIT-CODE2B   PIC X.                        ECS053
00425          10  FILLER                 PIC X.                        ECS053
00426          10  P-TRM                  PIC 9(3).                     ECS053
00427          10  FILLER                 PIC X.                        ECS053
00428          10  P-R-TRM                PIC X(3).                     ECS053
00429          10  FILLER                 PIC X.                        ECS053
00430          10  P-FACE                 PIC ZZZZ,ZZZ.99-              ECS053
00431                                         BLANK WHEN ZERO.          ECS053
00432          10  P-FACE-OB REDEFINES P-FACE.                          ECS053
00433              15  P-FACE-DOL         PIC ZZZ,ZZZ,ZZZ-              ECS053
00434                                         BLANK WHEN ZERO.          ECS053
00435          10  P-REM                  PIC ZZZZ,ZZZ.99-              ECS053
00436                                         BLANK WHEN ZERO.          ECS053
00437          10  P-REM-OB  REDEFINES P-REM.                           ECS053
00438              15  P-REM-DOL          PIC ZZ,ZZZ,ZZZ-               ECS053
00439                                         BLANK WHEN ZERO.          ECS053
00440              15  FILLER             PIC X.                        ECS053
00441          10  P-EXIT-MSG-N-DATE REDEFINES P-REM.                   ECS053
00442              15  P-EXIT-MSG         PIC X(4).                     ECS053
00443              15  P-EXIT-DATE        PIC X(8).                     ECS053
00444          10  P-PREMIUM           PIC ZZZZ,ZZZ.99-                 ECS053
00445                                         BLANK WHEN ZERO.          ECS053
00446          10  P-CLAIM-PMTS        PIC ZZZZ,ZZZ.99-                 ECS053
00447                                         BLANK WHEN ZERO.          ECS053
00448          10  P-REFUND            PIC ZZZZ,ZZZ.99-                 ECS053
00449                                         BLANK WHEN ZERO.          ECS053
00450          10  FILLER              PIC XX.                          ECS053
00451          10  P-MSG               PIC X(4).                        ECS053
00452          10  FILLER              PIC X.                           ECS053
00453          10  P-MSG-DATE          PIC X(8).                        ECS053
00454      EJECT                                                        ECS053
00455  01  TOTAL-PRT-LINE.                                              ECS053
00456      12  FILLER              PIC X(13)           VALUE SPACES.    ECS053
00457      12  TL-MES              PIC X(6)            VALUE SPACES.    ECS053
00458      12  FILLER              PIC X(42)           VALUE            ECS053
00459              'NUMBER OF CERTIFICATES EXITING THIS MONTH '.        ECS053
00460      12  TL-CNT              PIC ZZZ,ZZZ,ZZ9.                     ECS053
00461                                                                   ECS053
00462                              COPY ELCDTECX.                       ECS053
00463                                                                   ECS053
00464                              COPY ELCDTEVR.                       ECS053
00465                                                                   ECS053
00466                              COPY ELCAEXVR.                       ECS053
00467  EJECT                                                            ECS053
00468  PROCEDURE DIVISION.                                              ECS053
00469                                                                   ECS053
00470  000-SET-START SECTION.                                           ECS053
00471                              COPY ELCDTERX.                       ECS053
00472                                                                   ECS053
00473  070-SORT-PROCESSING-SECTION.                                     ECS053
00474                                                                   ECS053
00475      PERFORM 400-INITIALIZATION THRU 400-EXIT.                    ECS053
00476                                                                   ECS053
00477      IF  DTE-PGM-OPT = 2                                          ECS053
00478          SORT SORT-FILE ASCENDING KEY SORT-ACCT                   ECS053
00479                                       SORT-CERT                   ECS053
00480              INPUT PROCEDURE 100-INPUT-PROCESS THRU 199-EXIT      ECS053
00481              OUTPUT PROCEDURE 200-OUTPUT-PROCESS THRU 200-EXIT    ECS053
00482              GO TO 090-CHECK-SORT-CODE.                           ECS053
00483                                                                   ECS053
00484                                                                   ECS053
00485      IF  DTE-PGM-OPT = 3                                          ECS053
00486          SORT SORT-FILE ASCENDING KEY SORT-CERT                   ECS053
00487                                       SORT-STATE                  ECS053
00488                                       SORT-ACCT                   ECS053
00489              INPUT PROCEDURE 100-INPUT-PROCESS THRU 199-EXIT      ECS053
00490              OUTPUT PROCEDURE 200-OUTPUT-PROCESS THRU 200-EXIT    ECS053
00491              GO TO 090-CHECK-SORT-CODE.                           ECS053
00492                                                                   ECS053
00493                                                                   ECS053
00494      IF  DTE-PGM-OPT = 4                                          ECS053
00495          SORT SORT-FILE ASCENDING KEY SORT-ACCT                   ECS053
00496                                       SORT-NAME                   ECS053
00497              INPUT PROCEDURE 100-INPUT-PROCESS THRU 199-EXIT      ECS053
00498              OUTPUT PROCEDURE 200-OUTPUT-PROCESS THRU 200-EXIT    ECS053
00499              GO TO 090-CHECK-SORT-CODE.                           ECS053
00500                                                                   ECS053
00501                                                                   ECS053
00502      IF  DTE-PGM-OPT = 5                                          ECS053
00503          SORT SORT-FILE ASCENDING KEY SORT-NAME                   ECS053
00504                                       SORT-ACCT                   ECS053
00505              INPUT PROCEDURE 100-INPUT-PROCESS THRU 199-EXIT      ECS053
00506              OUTPUT PROCEDURE 200-OUTPUT-PROCESS THRU 200-EXIT    ECS053
00507              GO TO 090-CHECK-SORT-CODE.                           ECS053
00508                                                                   ECS053
00509                                                                   ECS053
00510      IF  DTE-PGM-OPT = 6                                          ECS053
00511          SORT SORT-FILE ASCENDING KEY SORT-ACCT                   ECS053
00512                                       SORT-DATE                   ECS053
00513                                       SORT-NAME                   ECS053
00514              INPUT PROCEDURE 100-INPUT-PROCESS THRU 199-EXIT      ECS053
00515              OUTPUT PROCEDURE 200-OUTPUT-PROCESS THRU 200-EXIT    ECS053
00516              GO TO 090-CHECK-SORT-CODE.                           ECS053
00517                                                                   ECS053
00518                                                                   ECS053
00519      IF  DTE-PGM-OPT = 7                                          ECS053
00520          SORT SORT-FILE ASCENDING KEY SORT-CARRIER                ECS053
00521                                       SORT-CERT                   ECS053
00522              INPUT PROCEDURE 100-INPUT-PROCESS THRU 199-EXIT      ECS053
00523              OUTPUT PROCEDURE 200-OUTPUT-PROCESS THRU 200-EXIT    ECS053
00524              GO TO 090-CHECK-SORT-CODE.                           ECS053
00525                                                                   ECS053
00526                                                                   ECS053
00527      SORT SORT-FILE ASCENDING KEY SORT-CERT                       ECS053
00528                                   SORT-ACCT                       ECS053
00529              INPUT PROCEDURE 100-INPUT-PROCESS THRU 199-EXIT      ECS053
00530              OUTPUT PROCEDURE 200-OUTPUT-PROCESS THRU 200-EXIT.   ECS053
00531                                                                   ECS053
00532  090-CHECK-SORT-CODE.                                             ECS053
00533      IF SORT-RETURN NOT = ZERO                                    ECS053
00534          MOVE +0101 TO WS-RETURN-CODE                             ECS053
00535          MOVE 'INTERNAL SORT HAS FAILED' TO WS-ABEND-MESSAGE      ECS053
00536          GO TO ABEND-PGM.                                         ECS053
00537                                                                   ECS053
00538      GO TO 999-END-THE-JOB.                                       ECS053
00539                                                                   ECS053
00540      EJECT                                                        ECS053
00541  100-INPUT-PROCESS  SECTION.                                      ECS053
00542                                                                   ECS053
00543  110-OPEN-ALPHA-EXTRACT.                                          ECS053
00544                                                                   ECS053
00545      OPEN INPUT ALPH-EXTC.                                        ECS053
00546                                                                   ECS053
00547  120-READ-ALPHA-EXTRACT.                                          ECS053
00548      READ ALPH-EXTC  INTO  ALPHA-RECORD                           ECS053
00549                    AT END CLOSE ALPH-EXTC                         ECS053
00550                           GO TO 199-EXIT.                         ECS053
00551                                                                   ECS053
00552      COPY ELCAEXM1.                                               ECS053
00553                                                                   ECS053
00554  130-SELECT-ALPHA-EXTRACT.                                        ECS053
00555      IF AX-JOINT-ALPHA OR AX-REIN-ALPHA OR AX-JOINT-REIN-ALPHA    ECS053
00556          GO TO 120-READ-ALPHA-EXTRACT.                            ECS053
00557                                                                   ECS053
00558      IF AX-ENTRY-STATUS = '5'                                     ECS053
00559          GO TO 120-READ-ALPHA-EXTRACT.                            ECS053
00560                                                                   ECS053
00561      IF (AX-LF-STATUS = ' ' OR 'E' OR '6' OR '7' OR '8' OR 'V'    ECS053
00562                     OR 'D') AND                                   ECS053
00563         (AX-AH-STATUS = ' ' OR 'E' OR '6' OR '7' OR '8' OR 'V'    ECS053
00564                     OR 'D')                                       ECS053
00565          NEXT SENTENCE                                            ECS053
00566      ELSE                                                         ECS053
00567          GO TO 120-READ-ALPHA-EXTRACT.                            ECS053
00568                                                                   ECS053
00569      IF AX-LF-STATUS = 'E'                                        ECS053
00570          IF AX-LE-YR = RUN-YR  AND                                ECS053
00571             AX-LE-MO = RUN-MO                                     ECS053
00572              GO TO 140-RELEASE-ALPHA-TO-SORT.                     ECS053
00573                                                                   ECS053
00574      IF AX-LF-STATUS = '6'  OR  '7'  OR  '8' OR 'V' OR 'D'        ECS053
00575          IF AX-LX-YR = RUN-YR  AND                                ECS053
00576             AX-LX-MO = RUN-MO                                     ECS053
00577              GO TO 140-RELEASE-ALPHA-TO-SORT.                     ECS053
00578                                                                   ECS053
00579      IF AX-AH-STATUS = 'E'                                        ECS053
00580          IF AX-AE-YR = RUN-YR  AND                                ECS053
00581             AX-AE-MO = RUN-MO                                     ECS053
00582              GO TO 140-RELEASE-ALPHA-TO-SORT.                     ECS053
00583                                                                   ECS053
00584      IF AX-AH-STATUS = '6'  OR  '7'  OR  '8' OR 'V' OR 'D'        ECS053
00585          IF AX-AX-YR = RUN-YR  AND                                ECS053
00586             AX-AX-MO = RUN-MO                                     ECS053
00587              GO TO 140-RELEASE-ALPHA-TO-SORT.                     ECS053
00588                                                                   ECS053
00589      GO TO 120-READ-ALPHA-EXTRACT.                                ECS053
00590                                                                   ECS053
00591  140-RELEASE-ALPHA-TO-SORT.                                       ECS053
00592      RELEASE SORT-FILE-REC FROM ALPHA-RECORD.                     ECS053
00593                                                                   ECS053
00594      GO TO 120-READ-ALPHA-EXTRACT.                                ECS053
00595                                                                   ECS053
00596  199-EXIT.                                                        ECS053
00597      EXIT.                                                        ECS053
00598                                                                   ECS053
00599      EJECT                                                        ECS053
00600  200-OUTPUT-PROCESS  SECTION.                                     ECS053
00601                                                                   ECS053
00602      PERFORM 410-PRNT-PG-HEADINGS THRU 410-EXIT.                  ECS053
00603                                                                   ECS053
00604      PERFORM 250-RETURN-RECS THRU 250-EXIT                        ECS053
00605              UNTIL ALL-RECS-RETURNED.                             ECS053
00606                                                                   ECS053
00607                                                                   ECS053
00608  200-EXIT.                                                        ECS053
00609      EXIT.                                                        ECS053
00610      EJECT                                                        ECS053
00611                                                                   ECS053
00612  250-RETURN-RECS.                                                 ECS053
00613      RETURN SORT-FILE INTO ALPHA-RECORD                           ECS053
00614             AT END                                                ECS053
00615                   MOVE 'Y' TO RECS-RETURNED-FLAG                  ECS053
00616                   GO TO 250-EXIT.                                 ECS053
00617                                                                   ECS053
00618      ADD 1 TO TOTAL-RECS-RETURNED.                                ECS053
00619                                                                   ECS053
00620      COPY ELCAEXM1.                                               ECS053
00621                                                                   ECS053
00622      IF LCP-ONCTR-01 =  0                                         ECS053
00623          ADD 1 TO LCP-ONCTR-01                                    ECS053
00624          MOVE AX-CARRIER TO LAST-CARRIER.                         ECS053
00625                                                                   ECS053
00626      IF DTE-PGM-OPT = '7'                                         ECS053
00627          IF AX-CARRIER NOT = LAST-CARRIER                         ECS053
00628            MOVE    AX-CARRIER            TO  LAST-CARRIER         ECS053
00629 *          MOVE    TOT-CAR-PRM           TO  TL-PRM               ECS053
00630            MOVE    TOT-CAR-CNT           TO  TL-CNT               ECS053
00631            MOVE    TOTAL-PRT-LINE        TO  PRT                  ECS053
00632            MOVE    '0'                   TO  P-CTL                ECS053
00633            PERFORM 420-PRINT-DETAIL     THRU 420-EXIT             ECS053
00634            MOVE    ZEROS                 TO  TOT-CAR-CNT          ECS053
00635                                              TOT-CAR-PRM          ECS053
00636            PERFORM 410-PRNT-PG-HEADINGS THRU 410-EXIT.            ECS053
00637                                                                   ECS053
00638      IF  LN-CT GREATER MAX-LN                                     ECS053
00639          PERFORM 410-PRNT-PG-HEADINGS THRU 410-EXIT.              ECS053
00640                                                                   ECS053
00641      PERFORM 300-BUILD-PRNT-LINE1    THRU 300-EXIT.               ECS053
00642                                                                   ECS053
00643      MOVE    P-DTL-LINE1              TO  PRT.                    ECS053
00644                                                                   ECS053
00645      PERFORM 420-PRINT-DETAIL        THRU 420-EXIT.               ECS053
00646                                                                   ECS053
00647      IF  AX-LF-TYP NOT = ZEROS                                    ECS053
00648          PERFORM 310-BUILD-PRNT-LINE2-LF THRU 310-EXIT            ECS053
00649          MOVE    P-DTL-LINE2              TO  PRT                 ECS053
00650          PERFORM 420-PRINT-DETAIL        THRU 420-EXIT.           ECS053
00651                                                                   ECS053
00652      IF  AX-AH-TYP NOT = ZEROS                                    ECS053
00653          PERFORM 320-BUILD-PRNT-LINE3-AH THRU 320-EXIT            ECS053
00654          MOVE    P-DTL-LINE2              TO  PRT                 ECS053
00655          PERFORM 420-PRINT-DETAIL        THRU 420-EXIT.           ECS053
00656                                                                   ECS053
00657  250-EXIT.                                                        ECS053
00658      EXIT.                                                        ECS053
00659      EJECT                                                        ECS053
00660  300-BUILD-PRNT-LINE1.                                            ECS053
00661      IF AX-CONTROL NOT = LAS-AX-CONTROL                           ECS053
00662           ADD  1 TO TOT-CAR-CNT                                   ECS053
00663                     TOT-CRT-CNT.                                  ECS053
00664                                                                   ECS053
00665      MOVE AX-CONTROL        TO LAS-AX-CONTROL.                    ECS053
00666                                                                   ECS053
00667      MOVE SPACES            TO  DTL.                              ECS053
00668 **                                                                ECS053
00669 ** SET UP THE VARIABLE PORTION OF DETAIL LINE1.                   ECS053
00670 **                                                                ECS053
00671      IF  DTE-PGM-OPT = 2                                          ECS053
00672          MOVE AX-CERT       TO  P-CERT-SEQ2                       ECS053
00673          MOVE AX-ACCOUNT    TO  P-ACCT-SEQ2                       ECS053
00674      ELSE                                                         ECS053
00675          IF  DTE-PGM-OPT = 3                                      ECS053
00676              MOVE AX-CERT   TO  P-CERT-SEQ3                       ECS053
00677              MOVE AX-ACCOUNT TO  P-ACCT-SEQ3                      ECS053
00678              MOVE AX-STATE  TO  P-ST-SEQ3                         ECS053
00679          ELSE                                                     ECS053
00680              IF DTE-PGM-OPT = 4                                   ECS053
00681                  MOVE AX-ACCOUNT        TO P-ACCT-SEQ4            ECS053
00682                  MOVE AX-LNAME          TO P-NAME-SEQ4            ECS053
00683                  MOVE AX-1ST-INIT-FNAME TO P-NAME-INIT1-SEQ4      ECS053
00684                  MOVE AX-INIT           TO P-NAME-INIT2-SEQ4      ECS053
00685              ELSE                                                 ECS053
00686                  IF DTE-PGM-OPT = 5                               ECS053
00687                      MOVE AX-ACCOUNT        TO P-ACCT-SEQ5        ECS053
00688                      MOVE AX-LNAME          TO P-NAME-SEQ5        ECS053
00689                      MOVE AX-1ST-INIT-FNAME TO P-NAME-INIT1-SEQ5  ECS053
00690                      MOVE AX-INIT           TO P-NAME-INIT2-SEQ5  ECS053
00691                  ELSE                                             ECS053
00692                      IF DTE-PGM-OPT = 6                           ECS053
00693                          MOVE AX-ACCOUNT      TO P-ACCT-SEQ6      ECS053
00694                          MOVE AX-DT          TO  WS-DATE-FORMAT-INECS053
00695                          PERFORM 500-DATE-FORMAT1 THRU 500-EXIT   ECS053
00696                          MOVE WS-DATE-FORMAT1 TO P-ISSUE-DATE-SEQ6ECS053
00697                          MOVE AX-LNAME        TO P-NAME-SEQ6      ECS053
00698                          MOVE AX-1ST-INIT-FNAME                   ECS053
00699                                               TO P-NAME-INIT1-SEQ6ECS053
00700                          MOVE AX-INIT                             ECS053
00701                                               TO P-NAME-INIT2-SEQ6ECS053
00702                      ELSE                                         ECS053
00703                          IF DTE-PGM-OPT = 7                       ECS053
00704                              MOVE AX-CERT    TO P-CERT-SEQ7       ECS053
00705                              MOVE AX-CARRIER TO P-CARRIER-SEQ7    ECS053
00706                          ELSE                                     ECS053
00707                              MOVE AX-CERT TO P-CERT-SEQ1          ECS053
00708                              MOVE AX-ACCOUNT TO P-ACCT-SEQ1.      ECS053
00709                                                                   ECS053
00710 **                                                                ECS053
00711 ** SET UP THE FIXED PORTION OF DETAIL LINE1.                      ECS053
00712 **                                                                ECS053
00713                                                                   ECS053
00714      IF  DTE-PGM-OPT NOT = 7                                      ECS053
00715          MOVE AX-CARRIER  TO  P-CARRIER.                          ECS053
00716                                                                   ECS053
00717      MOVE AX-GROUPING     TO  P-GROUP.                            ECS053
00718                                                                   ECS053
00719      IF  DTE-PGM-OPT NOT = 3                                      ECS053
00720          MOVE AX-STATE TO  P-ST.                                  ECS053
00721                                                                   ECS053
00722      IF  DTE-PGM-OPT = 7                                          ECS053
00723          MOVE AX-ACCOUNT  TO  P-ACCT.                             ECS053
00724                                                                   ECS053
00725      IF  DTE-PGM-OPT = 4  OR  5  OR  6                            ECS053
00726          MOVE AX-CERT  TO  P-CERT                                 ECS053
00727      ELSE                                                         ECS053
00728          MOVE AX-LNAME          TO  P-NAME                        ECS053
00729          MOVE AX-1ST-INIT-FNAME TO  P-NAME-INIT1                  ECS053
00730          MOVE AX-INIT           TO  P-NAME-INIT2.                 ECS053
00731                                                                   ECS053
00732      MOVE AX-AGE   TO  P-AGE.                                     ECS053
00733                                                                   ECS053
00734      IF  DTE-PGM-OPT NOT = 6                                      ECS053
00735          MOVE AX-DT  TO  WS-DATE-FORMAT-IN                        ECS053
00736          PERFORM 500-DATE-FORMAT1 THRU 500-EXIT                   ECS053
00737          MOVE WS-DATE-FORMAT1 TO P-ISSUE-DATE.                    ECS053
00738                                                                   ECS053
00739      MOVE AX-ENTRY-STATUS      TO  P-SR.                          ECS053
00740                                                                   ECS053
00741      MOVE AX-ENTRY    TO WS-DATE-FORMAT-IN.                       ECS053
00742                                                                   ECS053
00743      PERFORM 510-DATE-FORMAT2 THRU 510-EXIT.                      ECS053
00744                                                                   ECS053
00745      MOVE WS-DATE-FORMAT2   TO  P-ENTRY-DATE.                     ECS053
00746                                                                   ECS053
00747      IF  AX-SPEC-REIN = SPACES                                    ECS053
00748                          OR ZEROS                                 ECS053
00749          MOVE AX-REIN-TABLE TO P-REIN-CODE                        ECS053
00750      ELSE                                                         ECS053
00751          MOVE AX-SPEC-REIN  TO P-REIN-CODE1                       ECS053
00752                                P-REIN-CODE2                       ECS053
00753                                P-REIN-CODE3.                      ECS053
00754                                                                   ECS053
00755      IF  AX-PMT-FREQ = ZEROS                                      ECS053
00756          MOVE ZEROS TO P-PF                                       ECS053
00757      ELSE                                                         ECS053
00758          MOVE AX-PMT-FREQ TO P-PF.                                ECS053
00759                                                                   ECS053
00760      IF  AX-APR = ZEROS                                           ECS053
00761          MOVE ZEROS TO P-APR                                      ECS053
00762      ELSE                                                         ECS053
00763          MOVE AX-APR TO P-APR.                                    ECS053
00764                                                                   ECS053
00765  300-EXIT.                                                        ECS053
00766      EXIT.                                                        ECS053
00767      EJECT                                                        ECS053
00768  310-BUILD-PRNT-LINE2-LF.                                         ECS053
00769      MOVE SPACES            TO  DTL.                              ECS053
00770                                                                   ECS053
00771      MOVE LIFE-OVERRIDE-L2  TO  P-TYPE-L-A.                       ECS053
00772                                                                   ECS053
00773      IF  AX-LF-TYP  = SPACES                                      ECS053
00774                        OR ZEROS                                   ECS053
00775          MOVE SPACES TO P-BENEFIT-CODE1                           ECS053
00776                         P-BENEFIT-CODE2                           ECS053
00777      ELSE                                                         ECS053
00778          MOVE    AX-LF-TYP  TO  CLAS-LOOK                         ECS053
00779          PERFORM 520-LIFE-CLAS-LOOK-RTN THRU 520-EXIT             ECS053
00780          MOVE    CLAS-I-BEN (CLAS-INDEXL) TO P-BENEFIT-CODE1      ECS053
00781          MOVE    CLAS-I-AB3 (CLAS-INDEXL) TO P-BENEFIT-CODE2.     ECS053
00782                                                                   ECS053
00783      PERFORM 540-LF-MSG-FORMAT THRU 540-EXIT.                     ECS053
00784                                                                   ECS053
00785      MOVE AX-LF-TERM        TO P-TRM.                             ECS053
00786                                                                   ECS053
00787      MOVE AX-LF-REMTERM     TO P-R-TRM.                           ECS053
00788                                                                   ECS053
00789      COMPUTE WS-XX-AMT = AX-LF-AMT + AX-LF-AMT-ALT.               ECS053
00790                                                                   ECS053
00791      COMPUTE WS-XX-REMAMT = AX-LF-REMAMT + AX-LF-REMAMT-ALT.      ECS053
00792                                                                   ECS053
00793      IF  OB-CERT                                                  ECS053
00794          MOVE    'N' TO OB-SWITCH                                 ECS053
00795          MOVE    WS-XX-AMT       TO WS-AX-LF-AMT                  ECS053
00796          MOVE    WS-AX-LF-AMT-OB TO P-FACE-DOL                    ECS053
00797          MOVE    WS-XX-REMAMT    TO WS-AX-LF-AMT                  ECS053
00798          MOVE    WS-AX-LF-AMT-OB TO P-REM-DOL                     ECS053
00799          COMPUTE TOT-REM-PRM = TOT-REM-PRM + (WS-XX-REMAMT * 100) ECS053
00800          COMPUTE TOT-CAR-PRM = TOT-CAR-PRM + (WS-XX-REMAMT * 100) ECS053
00801      ELSE                                                         ECS053
00802          MOVE WS-XX-AMT    TO P-FACE                              ECS053
00803          MOVE WS-XX-REMAMT TO P-REM                               ECS053
00804          ADD  WS-XX-REMAMT TO TOT-REM-PRM                         ECS053
00805                               TOT-CAR-PRM.                        ECS053
00806                                                                   ECS053
00807      COMPUTE WS-XX-PREM = AX-LF-PRM + AX-LF-PRM-ALT.              ECS053
00808                                                                   ECS053
00809      MOVE WS-XX-PREM        TO  P-PREMIUM.                        ECS053
00810                                                                   ECS053
00811      MOVE AX-LF-CLAIM-PMTS  TO  P-CLAIM-PMTS.                     ECS053
00812                                                                   ECS053
00813      MOVE AX-LF-REFUND      TO  P-REFUND.                         ECS053
00814                                                                   ECS053
00815                                                                   ECS053
00816  310-EXIT.                                                        ECS053
00817      EXIT.                                                        ECS053
00818      EJECT                                                        ECS053
00819  320-BUILD-PRNT-LINE3-AH.                                         ECS053
00820      MOVE SPACES          TO  DTL.                                ECS053
00821                                                                   ECS053
00822      MOVE ZEROS           TO  P-REM.                              ECS053
00823                                                                   ECS053
00824      MOVE AH-OVERRIDE-L2  TO  P-TYPE-L-A.                         ECS053
00825                                                                   ECS053
00826      IF  AX-AH-TYP = SPACES                                       ECS053
00827                       OR ZEROS                                    ECS053
00828          MOVE SPACES TO P-BENEFIT-CODE1                           ECS053
00829                         P-BENEFIT-CODE2                           ECS053
00830      ELSE                                                         ECS053
00831          MOVE    AX-AH-TYP  TO  CLAS-LOOK                         ECS053
00832          PERFORM 530-AH-CLAS-LOOK-RTN THRU 530-EXIT               ECS053
00833          MOVE    CLAS-I-BEN (CLAS-INDEXA) TO P-BENEFIT-CODE1      ECS053
00834          MOVE    CLAS-I-AB3 (CLAS-INDEXA) TO P-BENEFIT-CODE2      ECS053
00835          IF  DTE-CLIENT = 'MIL'                                   ECS053
00836              MOVE AX-IND-GRP TO P-BENEFIT-CODE2B.                 ECS053
00837                                                                   ECS053
00838      PERFORM 550-AH-MSG-FORMAT THRU 550-EXIT.                     ECS053
00839                                                                   ECS053
00840      MOVE AX-AH-TERM        TO P-TRM.                             ECS053
00841                                                                   ECS053
00842      MOVE AX-AH-REMTERM     TO P-R-TRM.                           ECS053
00843                                                                   ECS053
00844      IF OB-CERT                                                   ECS053
00845          MOVE 'N'             TO OB-SWITCH                        ECS053
00846          MOVE AX-AH-AMT       TO WS-AX-AH-AMT                     ECS053
00847          MOVE WS-AX-AH-AMT-OB TO P-FACE-DOL                       ECS053
00848      ELSE                                                         ECS053
00849          MOVE AX-AH-AMT    TO P-FACE                              ECS053
00850          MOVE AX-AH-REMAMT TO P-REM.                              ECS053
00851                                                                   ECS053
00852      MOVE AX-AH-PRM         TO  P-PREMIUM.                        ECS053
00853                                                                   ECS053
00854      MOVE AX-AH-CLAIM-PMTS  TO  P-CLAIM-PMTS.                     ECS053
00855                                                                   ECS053
00856      MOVE AX-AH-REFUND      TO  P-REFUND.                         ECS053
00857                                                                   ECS053
00858  320-EXIT.                                                        ECS053
00859      EXIT.                                                        ECS053
00860      EJECT                                                        ECS053
00861  400-INITIALIZATION-N-PRINTING  SECTION.                          ECS053
00862  400-INITIALIZATION.                                              ECS053
00863                                                                   ECS053
00864      IF FICH-ONLY                                                 ECS053
00865          MOVE +52 TO MAX-LN.                                      ECS053
00866                                                                   ECS053
00867      MOVE COMPANY-NAME TO HD-CO.                                  ECS053
00868      MOVE ALPH-DATE    TO HD-DT.                                  ECS053
00869                                                                   ECS053
00870      OPEN OUTPUT PRNTR.                                           ECS053
00871      ACCEPT  LCP-DATE-NEW-74 FROM DATE                            ECS053
00872      MOVE CORRESPONDING LCP-DATE-NEW-74 TO LCP-CURRENT-DATE-68    ECS053
00873      MOVE  LCP-CURRENT-DATE-68 TO HD-RD.                          ECS053
00874      COMPUTE CUR-DATE = (RUN-CCYY * 12) + RUN-MO.                 ECS053
00875 **                                                                ECS053
00876 ** SET UP THE REPORT SEQUENCE TITLE IN HD3 AND                    ECS053
00877 ** SET UP THE VARIABLE REPORT HEADINGS IN HD4.                    ECS053
00878 **                                                                ECS053
00879      IF  DTE-PGM-OPT = 2                                          ECS053
00880          MOVE SORT-SEQ2 TO HD3-RPT-SEQ                            ECS053
00881          MOVE HD4-SEQ2  TO HD4-SEQ-TITLE                          ECS053
00882          MOVE SPACES    TO HD4-ACCOUNT-TITLE                      ECS053
00883                            HD4-CERT-TITLE                         ECS053
00884      ELSE                                                         ECS053
00885          IF  DTE-PGM-OPT = 3                                      ECS053
00886              MOVE SORT-SEQ3 TO HD3-RPT-SEQ                        ECS053
00887              MOVE HD4-SEQ3  TO HD4-SEQ-TITLE                      ECS053
00888              MOVE SPACES    TO HD4-ACCOUNT-TITLE                  ECS053
00889                                HD4-CERT-TITLE                     ECS053
00890                                HD4-ST-TITLE                       ECS053
00891          ELSE                                                     ECS053
00892              IF  DTE-PGM-OPT = 4                                  ECS053
00893                  MOVE SORT-SEQ4 TO HD3-RPT-SEQ                    ECS053
00894                  MOVE HD4-SEQ4  TO HD4-SEQ-TITLE                  ECS053
00895                  MOVE SPACES    TO HD4-ACCOUNT-TITLE              ECS053
00896                                    HD4-NAME-TITLE                 ECS053
00897                                    HD4-INIT-TITLE                 ECS053
00898              ELSE                                                 ECS053
00899                  IF DTE-PGM-OPT = 5                               ECS053
00900                      MOVE SORT-SEQ5 TO HD3-RPT-SEQ                ECS053
00901                      MOVE HD4-SEQ5  TO HD4-SEQ-TITLE              ECS053
00902                      MOVE SPACES    TO HD4-ACCOUNT-TITLE          ECS053
00903                                        HD4-NAME-TITLE             ECS053
00904                                        HD4-INIT-TITLE             ECS053
00905                  ELSE                                             ECS053
00906                      IF DTE-PGM-OPT = 6                           ECS053
00907                          MOVE SORT-SEQ6 TO HD3-RPT-SEQ            ECS053
00908                          MOVE HD4-SEQ6  TO HD4-SEQ-TITLE          ECS053
00909                          MOVE SPACES    TO HD4-ACCOUNT-TITLE      ECS053
00910                                            HD4-NAME-TITLE         ECS053
00911                                            HD4-INIT-TITLE         ECS053
00912                                            HD4-ISSUE-TITLE        ECS053
00913                      ELSE                                         ECS053
00914                          IF DTE-PGM-OPT = 7                       ECS053
00915                              MOVE SORT-SEQ7 TO HD3-RPT-SEQ        ECS053
00916                              MOVE HD4-SEQ7  TO HD4-SEQ-TITLE      ECS053
00917                              MOVE SPACES    TO HD4-C-TITLE        ECS053
00918                                                HD4-CERT-TITLE     ECS053
00919                          ELSE                                     ECS053
00920                              MOVE SORT-SEQ1 TO HD3-RPT-SEQ        ECS053
00921                              MOVE HD4-SEQ1  TO HD4-SEQ-TITLE      ECS053
00922                              MOVE SPACES    TO HD4-ACCOUNT-TITLE  ECS053
00923                                                HD4-CERT-TITLE.    ECS053
00924                                                                   ECS053
00925  400-EXIT.                                                        ECS053
00926      EXIT.                                                        ECS053
00927      EJECT                                                        ECS053
00928  410-PRNT-PG-HEADINGS.                                            ECS053
00929      ADD     1             TO  PG-NO.                             ECS053
00930      MOVE    PG-NO         TO  HD-PG.                             ECS053
00931      MOVE    HD1           TO  P-DATA.                            ECS053
00932      MOVE    '1'           TO  P-CTL.                             ECS053
00933      PERFORM 420-PRINT-DETAIL THRU 420-EXIT.                      ECS053
00934                                                                   ECS053
00935      MOVE    HD2           TO  P-DATA.                            ECS053
00936      MOVE    SPACES        TO  P-CTL.                                CL**2
00937      PERFORM 420-PRINT-DETAIL THRU 420-EXIT.                      ECS053
00938                                                                   ECS053
00939      MOVE    HD3           TO  P-DATA.                            ECS053
00940      MOVE    SPACES        TO  P-CTL.                                CL**2
00941      PERFORM 420-PRINT-DETAIL THRU 420-EXIT.                      ECS053
00942                                                                   ECS053
00943      MOVE    HD4           TO  P-DATA.                            ECS053
00944      MOVE    '0'           TO  P-CTL.                             ECS053
00945      PERFORM 420-PRINT-DETAIL THRU 420-EXIT.                      ECS053
00946                                                                   ECS053
00947      MOVE    HD5           TO  P-DATA.                            ECS053
00948      MOVE    SPACES        TO  P-CTL.                                CL**2
00949      PERFORM 420-PRINT-DETAIL THRU 420-EXIT.                      ECS053
00950                                                                   ECS053
00951      MOVE    HD6           TO  P-DATA.                            ECS053
00952      MOVE    SPACES        TO  P-CTL.                                CL**3
00953      PERFORM 420-PRINT-DETAIL THRU 420-EXIT.                      ECS053
00954                                                                   ECS053
00955      MOVE    SPACES        TO  P-DATA.                            ECS053
00956      MOVE    SPACES        TO  P-CTL.                                CL**2
00957      PERFORM 420-PRINT-DETAIL THRU 420-EXIT.                      ECS053
00958                                                                   ECS053
00959      MOVE    ZEROS         TO  LN-CT.                             ECS053
00960      MOVE    SPACES        TO  PRT.                               ECS053
00961                                                                   ECS053
00962  410-EXIT.                                                        ECS053
00963      EXIT.                                                        ECS053
00964      EJECT                                                        ECS053
00965  420-PRINT-DETAIL.                                                ECS053
00966      MOVE P-CTL TO X.                                             ECS053
00967                                                                   ECS053
00968                           COPY ELCPRT2.                           ECS053
00969                                                                   ECS053
00970      ADD 1 TO LN-CT.                                              ECS053
00971                                                                   ECS053
00972  420-EXIT.                                                        ECS053
00973      EXIT.                                                        ECS053
00974      EJECT                                                        ECS053
00975  500-DATE-FORMAT1.                                                ECS053
00976      MOVE WS-DATEIN-YR  TO  WS-DATE1-YR.                          ECS053
00977      MOVE WS-DATEIN-MO  TO  WS-DATE1-MO.                          ECS053
00978      MOVE WS-DATEIN-DA  TO  WS-DATE1-DA.                          ECS053
00979                                                                   ECS053
00980      MOVE '/'           TO  WS-DATE1-SLH1                         ECS053
00981                             WS-DATE1-SLH2.                        ECS053
00982                                                                   ECS053
00983  500-EXIT.                                                        ECS053
00984      EXIT.                                                        ECS053
00985                                                                   ECS053
00986  510-DATE-FORMAT2.                                                ECS053
00987      MOVE WS-DATEIN-YR  TO  WS-DATE2-YR.                          ECS053
00988      MOVE WS-DATEIN-MO  TO  WS-DATE2-MO.                          ECS053
00989      MOVE '/'           TO  WS-DATE2-SLH1.                        ECS053
00990                                                                   ECS053
00991  510-EXIT.                                                        ECS053
00992      EXIT.                                                        ECS053
00993      EJECT                                                        ECS053
00994  520-LIFE-CLAS-LOOK-RTN.                                          ECS053
00995      MOVE CLAS-STARTL TO CLAS-INDEXL.                             ECS053
00996                                                                   ECS053
00997  520-LIFE-CLAS-LOOP.                                              ECS053
00998      IF    CLAS-INDEXL GREATER CLAS-MAXL                          ECS053
00999         OR CLAS-STARTL = ZEROS                                    ECS053
01000            GO TO 520-LIFE-CLAS-ABEND.                             ECS053
01001                                                                   ECS053
01002      IF CLAS-LOOK = CLAS-I-BEN (CLAS-INDEXL)                      ECS053
01003          GO TO 520-BAL-TEST.                                      ECS053
01004                                                                   ECS053
01005      ADD +1 TO CLAS-INDEXL.                                       ECS053
01006      GO     TO 520-LIFE-CLAS-LOOP.                                ECS053
01007                                                                   ECS053
01008  520-LIFE-CLAS-ABEND.                                             ECS053
01009      MOVE +0401 TO WS-RETURN-CODE.                                ECS053
01010      MOVE 'LIFE BENEFIT NOT IN TABLE' TO WS-ABEND-MESSAGE.        ECS053
01011      GO TO ABEND-PGM.                                             ECS053
01012                                                                   ECS053
01013  520-BAL-TEST.                                                    ECS053
01014      IF CLAS-I-BAL (CLAS-INDEXL) = 'B' OR 'Z'                     ECS053
01015          MOVE 'Y' TO OB-SWITCH.                                   ECS053
01016                                                                   ECS053
01017  520-EXIT.                                                        ECS053
01018      EXIT.                                                        ECS053
01019      EJECT                                                        ECS053
01020  530-AH-CLAS-LOOK-RTN.                                            ECS053
01021      MOVE CLAS-STARTA TO CLAS-INDEXA.                             ECS053
01022                                                                   ECS053
01023  530-AH-CLAS-LOOP.                                                ECS053
01024      IF   CLAS-INDEXA GREATER CLAS-MAXA                           ECS053
01025        OR CLAS-STARTA = ZEROS                                     ECS053
01026           GO TO 530-AH-CLAS-ABEND.                                ECS053
01027                                                                   ECS053
01028      IF CLAS-LOOK = CLAS-I-BEN (CLAS-INDEXA)                      ECS053
01029          GO TO 530-AH-BAL-TEST.                                   ECS053
01030                                                                   ECS053
01031      ADD +1 TO CLAS-INDEXA.                                       ECS053
01032      GO TO 530-AH-CLAS-LOOP.                                         CL**6
01033                                                                   ECS053
01034  530-AH-CLAS-ABEND.                                               ECS053
01035      MOVE +0402 TO WS-RETURN-CODE.                                ECS053
01036      MOVE 'AH BENEFIT NOT IN TABLE' TO WS-ABEND-MESSAGE.          ECS053
01037      GO TO ABEND-PGM.                                             ECS053
01038                                                                   ECS053
01039  530-AH-BAL-TEST.                                                 ECS053
01040      IF CLAS-I-BAL (CLAS-INDEXA) = 'B' OR 'Z'                     ECS053
01041          MOVE 'Y' TO OB-SWITCH.                                   ECS053
01042                                                                   ECS053
01043  530-EXIT.                                                        ECS053
01044      EXIT.                                                        ECS053
01045      EJECT                                                        ECS053
01046  540-LF-MSG-FORMAT.                                               ECS053
01047      IF  AX-LF-STATUS = '5'                                       ECS053
01048          MOVE 'R-IS' TO P-MSG                                     ECS053
01049          GO TO 540-EXIT                                           ECS053
01050      ELSE                                                         ECS053
01051          IF AX-LF-STATUS = '9'                                    ECS053
01052              MOVE 'REIN' TO P-MSG                                 ECS053
01053              GO TO 540-EXIT                                       ECS053
01054          ELSE                                                     ECS053
01055              IF AX-LF-STATUS = 'E'                                ECS053
01056                  MOVE 'EXPR' TO P-MSG                             ECS053
01057                  GO TO 540-EXIT                                   ECS053
01058              ELSE                                                 ECS053
01059                  IF AX-LF-STATUS = 'F'                            ECS053
01060                      MOVE 'FUTR' TO P-MSG                         ECS053
01061                      GO TO 540-EXIT                               ECS053
01062              ELSE                                                 ECS053
01063                  IF AX-LF-STATUS = 'V'                            ECS053
01064                      MOVE 'VOID' TO P-MSG                         ECS053
01065                      GO TO 540-EXIT                               ECS053
01066              ELSE                                                 ECS053
01067                  IF AX-LF-STATUS = 'D'                            ECS053
01068                      MOVE 'DECL' TO P-MSG                         ECS053
01069                      GO TO 540-EXIT.                              ECS053
01070                                                                   ECS053
01071      IF AX-LF-STATUS = '8'  OR  '6'                               ECS053
01072          MOVE    'CANC'            TO  P-MSG                      ECS053
01073          MOVE    'EXIT'            TO  P-EXIT-MSG                 ECS053
01074          MOVE    AX-LF-CNCL        TO  WS-DATE-FORMAT-IN          ECS053
01075          PERFORM 500-DATE-FORMAT1 THRU 500-EXIT                   ECS053
01076          MOVE    WS-DATE-FORMAT1   TO  P-MSG-DATE                 ECS053
01077          MOVE    AX-LF-EXIT        TO  WS-DATE-FORMAT-IN          ECS053
01078          PERFORM 500-DATE-FORMAT1 THRU 500-EXIT                   ECS053
01079          MOVE    WS-DATE-FORMAT1   TO  P-EXIT-DATE                ECS053
01080          GO TO 540-EXIT.                                          ECS053
01081                                                                   ECS053
01082      IF AX-LF-STATUS = '7'                                        ECS053
01083          MOVE    'CLM '            TO  P-MSG                      ECS053
01084          MOVE    'EXIT'            TO  P-EXIT-MSG                 ECS053
01085          MOVE    AX-DEATH          TO  WS-DATE-FORMAT-IN          ECS053
01086          PERFORM 500-DATE-FORMAT1 THRU 500-EXIT                   ECS053
01087          MOVE    WS-DATE-FORMAT1   TO  P-MSG-DATE                 ECS053
01088          MOVE    AX-LF-EXIT        TO  WS-DATE-FORMAT-IN          ECS053
01089          PERFORM 500-DATE-FORMAT1 THRU 500-EXIT                   ECS053
01090          MOVE    WS-DATE-FORMAT1   TO  P-EXIT-DATE                ECS053
01091          GO TO 540-EXIT.                                          ECS053
01092                                                                   ECS053
01093  540-EXIT.                                                        ECS053
01094      EXIT.                                                        ECS053
01095      EJECT                                                        ECS053
01096  550-AH-MSG-FORMAT.                                               ECS053
01097      IF  AX-AH-STATUS = '5'                                       ECS053
01098          MOVE 'R-IS' TO P-MSG                                     ECS053
01099          GO TO 550-EXIT                                           ECS053
01100      ELSE                                                         ECS053
01101          IF AX-AH-STATUS = '9'                                    ECS053
01102              MOVE 'REIN' TO P-MSG                                 ECS053
01103              GO TO 550-EXIT                                       ECS053
01104          ELSE                                                     ECS053
01105              IF AX-AH-STATUS = 'E'                                ECS053
01106                  MOVE 'EXPR' TO P-MSG                             ECS053
01107                  GO TO 550-EXIT                                   ECS053
01108              ELSE                                                 ECS053
01109                  IF AX-AH-STATUS = 'F'                            ECS053
01110                      MOVE 'FUTR' TO P-MSG                         ECS053
01111                      GO TO 550-EXIT                               ECS053
01112              ELSE                                                 ECS053
01113                  IF AX-AH-STATUS = 'V'                            ECS053
01114                      MOVE 'VOID' TO P-MSG                         ECS053
01115                      GO TO 550-EXIT                               ECS053
01116              ELSE                                                 ECS053
01117                  IF AX-AH-STATUS = 'D'                            ECS053
01118                      MOVE 'DECL' TO P-MSG                         ECS053
01119                      GO TO 550-EXIT.                              ECS053
01120                                                                   ECS053
01121      IF AX-AH-STATUS = '8'  OR  '7'                               ECS053
01122          MOVE    'CANC'            TO  P-MSG                      ECS053
01123          MOVE    'EXIT'            TO  P-EXIT-MSG                 ECS053
01124          MOVE    AX-AH-CNCL        TO  WS-DATE-FORMAT-IN          ECS053
01125          PERFORM 500-DATE-FORMAT1 THRU 500-EXIT                   ECS053
01126          MOVE    WS-DATE-FORMAT1   TO  P-MSG-DATE                 ECS053
01127          MOVE    AX-AH-EXIT        TO  WS-DATE-FORMAT-IN          ECS053
01128          PERFORM 500-DATE-FORMAT1 THRU 500-EXIT                   ECS053
01129          MOVE    WS-DATE-FORMAT1   TO  P-EXIT-DATE                ECS053
01130          GO TO 550-EXIT.                                          ECS053
01131                                                                   ECS053
01132      IF AX-AH-STATUS = '6'                                        ECS053
01133          MOVE    'LUMP'            TO  P-MSG                      ECS053
01134          MOVE    'EXIT'            TO  P-EXIT-MSG                 ECS053
01135          MOVE    AX-LUMP-SUM       TO  WS-DATE-FORMAT-IN          ECS053
01136          PERFORM 500-DATE-FORMAT1 THRU 500-EXIT                   ECS053
01137          MOVE    WS-DATE-FORMAT1   TO  P-MSG-DATE                 ECS053
01138          MOVE    AX-AH-EXIT        TO  WS-DATE-FORMAT-IN          ECS053
01139          PERFORM 500-DATE-FORMAT1 THRU 500-EXIT                   ECS053
01140          MOVE    WS-DATE-FORMAT1   TO  P-EXIT-DATE                ECS053
01141          GO TO 550-EXIT.                                          ECS053
01142                                                                   ECS053
01143      IF AX-AH-PRE-PLST = '6'                                      ECS053
01144          MOVE    'CLM '            TO  P-MSG                      ECS053
01145          IF   AX-LUMP-SUM NUMERIC                                 ECS053
01146           AND AX-LUMP-SUM GREATER THAN ZEROS                      ECS053
01147              MOVE AX-LUMP-SUM          TO  WS-DATE-FORMAT-IN      ECS053
01148              PERFORM 500-DATE-FORMAT1 THRU 500-EXIT               ECS053
01149              MOVE WS-DATE-FORMAT1      TO  P-MSG-DATE             ECS053
01150          ELSE                                                     ECS053
01151              MOVE SPACES               TO  P-MSG-DATE.            ECS053
01152                                                                   ECS053
01153  550-EXIT.                                                        ECS053
01154      EXIT.                                                        ECS053
01155      EJECT                                                        ECS053
01156  ABEND-PGM SECTION.                                               ECS053
01157                         COPY ELCABEND.                            ECS053
01158                                                                   ECS053
01159      EJECT                                                        ECS053
01160  999-END-THE-JOB.                                                 ECS053
01161      IF  DTE-PGM-OPT = '7'                                        ECS053
01162 *        MOVE TOT-CAR-PRM          TO  TL-PRM                     ECS053
01163          MOVE TOT-CAR-CNT          TO  TL-CNT                     ECS053
01164          MOVE TOTAL-PRT-LINE       TO  PRT                        ECS053
01165          MOVE '0'                  TO  P-CTL                      ECS053
01166          PERFORM 420-PRINT-DETAIL THRU 420-EXIT.                  ECS053
01167                                                                   ECS053
01168      MOVE    'TOTAL '          TO  TL-MES.                        ECS053
01169 *    MOVE    TOT-REM-PRM       TO  TL-PRM.                        ECS053
01170      MOVE    TOT-CRT-CNT       TO  TL-CNT.                        ECS053
01171      MOVE    TOTAL-PRT-LINE    TO  PRT.                           ECS053
01172      MOVE    '0'               TO  P-CTL.                         ECS053
01173      PERFORM 420-PRINT-DETAIL THRU 420-EXIT.                      ECS053
01174                                                                   ECS053
01175      ADD     TOT-REM-PRM       TO  FIN-REM-PRM.                   ECS053
01176      ADD     TOT-CRT-CNT       TO  FIN-CRT-CNT.                   ECS053
01177                                                                   ECS053
01178      MOVE    ZEROS             TO  TOT-CRT-CNT                    ECS053
01179                                    TOT-REM-PRM.                   ECS053
01180                                                                   ECS053
01181      MOVE    SPACES            TO  P-DATA.                        ECS053
01182      MOVE    '1'               TO  P-CTL.                         ECS053
01183      PERFORM 420-PRINT-DETAIL THRU 420-EXIT.                      ECS053
01184                                                                   ECS053
01185  999-CLOSE-FILES.                                                 ECS053
01186                              COPY ELCPRTC.                        ECS053
01187                                                                   ECS053
01188      CLOSE PRNTR.                                                 ECS053
01189                                                                   ECS053
01190      GOBACK.                                                      ECS053
