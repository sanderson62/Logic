00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   ECS095
00003  PROGRAM-ID.                 ECS095.                                 LV023
00004 *              PROGRAM CONVERTED BY                                  CL**8
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**8
00006 *              CONVERSION DATE 05/16/94 15:17:23.                    CL**8
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             CL**8
00008 *                            VMOD=2.014.                             CL**8
00009 *AUTHOR.     LOGIC, INC.                                             CL**8
00010 *            DALLAS, TEXAS.                                          CL**8
00011                                                                      CL**8
00012 *DATE-COMPILED.                                                      CL**8
00013                                                                      CL**8
00014 *SECURITY.   *****************************************************   CL**8
00015 *            *                                                   *   CL**8
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**8
00017 *            *                                                   *   CL**8
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**8
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**8
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *   CL**8
00021 *            *                                                   *   CL**8
00022 *            *****************************************************   CL**8
00023                                                                      CL**8
00024 *REMARKS.                       (DOCUMENTATION CHANGED 04/29/87.)    CL**8
00025 *    PROGRAM PURGES THE CERTIFICATE FILE OF ALL EXPIRED,             CL**8
00026 *    CANCELLED, OR 'DEATHS' PRIOR TO A DATE SPECIFIED.  IT           CL**8
00027 *    PUTS THE PURGED CERTS ON A 'PURGED' CERT FILE IN THE NORMAL     CL**8
00028 *    CERT FILE SEQUENCE.  IT PRINTS A SMALL REPORT AT THE END        CL**8
00029 *    OF THE JOB SHOWING HOW MANY CERTIFICATES WERE READ AND HOW      CL**8
00030 *    MANY WERE PURGED DUE TO EXPIRATION, 'DEATH' OR CANCELATION.     CL**8
00031 *    A NEW PROCESS OPTION IS AVAILABLE TO CREATE AN ALPHA FILE       CL**8
00032 *    CONTAINING DETAIL INFORMATION OF THOSE CERTS PURGED.            CL**8
00033 *    *** NOTE *** ALL QUALIFIED CERTIFICATES WITHIN A SPECIFIC       CL**8
00034 *                 DATE RANGE WILL BE PURGED.                         CL**8
00035                                                                      CL**8
00036  EJECT                                                               CL**8
00037  ENVIRONMENT DIVISION.                                               CL**8
00038                                                                      CL**8
00039  INPUT-OUTPUT SECTION.                                               CL**8
00040  FILE-CONTROL.                                                       CL**8
00041                                                                      CL**8
00042      SELECT DISK-DATE     ASSIGN TO SYS019-UT-FBA1-S-SYS019.         CL**8
00043                                                                      CL**8
00044      SELECT ACC-MSTR      ASSIGN TO SYS015-UT-2400-S-SYS015.         CL**8
00045                                                                      CL**8
00046      SELECT CERTS-IN      ASSIGN TO SYS010-UT-2400-S-SYS010.         CL**8
00047                                                                      CL**8
00048      SELECT CERTS-OUT     ASSIGN TO SYS011-UT-2400-S-SYS011.         CL**8
00049                                                                      CL**8
00050      SELECT PURGE-CRT     ASSIGN TO SYS012-UT-2400-S-SYS012.         CL**8
00051                                                                      CL**8
00052      SELECT ALPHA-EXTRACT ASSIGN TO SYS013-UT-2400-S-SYS013.         CL**8
00053                                                                      CL**8
00054      SELECT PRT-PURGE     ASSIGN TO SYS008-UR-1403-S-SYS008.         CL**8
00055                                                                      CL**8
00056      SELECT FICH          ASSIGN TO SYS020-UT-2400-S-SYS020.         CL**8
00057                                                                      CL**8
00058                                                                      CL**8
00059  EJECT                                                               CL**8
00060  DATA DIVISION.                                                      CL**8
00061  FILE SECTION.                                                       CL**8
00062                                                                      CL**8
00063  FD  CERTS-IN                                                        CL**8
00064                              COPY ECSCRIFD.                          CL**8
00065      COPY ECSCRT01.                                                  CL**8
00066  EJECT                                                               CL**8
00067  FD  CERTS-OUT                                                       CL**8
00068                              COPY ECSCRIFD.                          CL**8
00069  01  OUT-CERT                 PIC X(1056).                           CL**8
00070  EJECT                                                               CL**8
00071  FD  PURGE-CRT                                                       CL**8
00072                              COPY ECSCRIFD.                          CL**8
00073  01  CERT-PURGE               PIC X(1056).                           CL**8
00074  EJECT                                                               CL**8
00075  FD  ALPHA-EXTRACT                                                   CL**8
00076                              COPY ECSAEXFD.                          CL**8
00077  EJECT                                                               CL**8
00078  FD  PRT-PURGE                                                       CL**8
00079                              COPY ELCPRTFD.                          CL**8
00080  EJECT                                                               CL**8
00081  FD  DISK-DATE                                                       CL**8
00082                              COPY ELCDTEFD.                          CL**8
00083  EJECT                                                               CL**8
00084  FD  FICH                                                            CL**8
00085                              COPY ELCFCHFD.                          CL**8
00086  EJECT                                                               CL**8
00087  FD  ACC-MSTR                                                        CL**8
00088                              COPY ECSFDAMD.                          CL**8
00089      COPY ERCACCT.                                                   CL**8
00090  EJECT                                                               CL**8
00091  WORKING-STORAGE SECTION.                                            CL**8
00092  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.         CL**8
00093  77  FILLER  PIC X(32) VALUE '********************************'.     CL**8
00094  77  FILLER  PIC X(32) VALUE '     ECS095 WORKING STORAGE     '.     CL**8
00095  77  FILLER  PIC X(32) VALUE '********** VMOD=2.014 **********'.     CL**8
00096                                                                      CL**8
00097  77  PGM-SUB                     PIC S999     VALUE +095  COMP.      CL**8
00098  77  CTR                         PIC S999     COMP.                  CL**8
00099  77  PAGE-NO                     PIC S9(5)    VALUE +0    COMP-3.    CL**8
00100  77  PD-SW                       PIC X        VALUE SPACES.          CL**8
00101  77  DS                          PIC X        VALUE '0'.             CL**8
00102  77  SS                          PIC X        VALUE ' '.             CL**8
00103  77  TS                          PIC X        VALUE '-'.             CL**8
00104  77  TP                          PIC X        VALUE '1'.             CL**8
00105  77  X                           PIC X.                              CL**8
00106  77  LINE-CT                     PIC S99      VALUE ZERO.            CL**8
00107  77  TOTX-CERTS                  PIC S9(7)    VALUE ZERO  COMP-3.    CL**8
00108  77  TOTX-LIFE                   PIC S9(7)    VALUE ZERO  COMP-3.    CL**8
00109  77  TOTX-AH                     PIC S9(7)    VALUE ZERO  COMP-3.    CL**8
00110  77  TOTAL-ALPHA                 PIC S9(7)    VALUE ZERO  COMP-3.    CL**8
00111  77  CERTS-PURGE                 PIC S9(7)    VALUE ZERO  COMP-3.    CL**8
00112  77  CERTS-GOOD                  PIC S9(7)    VALUE ZERO  COMP-3.    CL**8
00113  77  CERTS-DEATH-LF              PIC S9(7)    VALUE ZERO  COMP-3.    CL**8
00114  77  CERTS-DEATH-AH              PIC S9(7)    VALUE ZERO  COMP-3.    CL**8
00115  77  CERTS-CANCEL-LF             PIC S9(7)    VALUE ZERO  COMP-3.    CL**8
00116  77  CERTS-CANCEL-AH             PIC S9(7)    VALUE ZERO  COMP-3.    CL**8
00117  77  CERTS-EXPIRED-LF            PIC S9(7)    VALUE ZERO  COMP-3.    CL**8
00118  77  CERTS-EXPIRED-AH            PIC S9(7)    VALUE ZERO  COMP-3.    CL**8
00119  77  A                           PIC S99      VALUE ZERO  COMP-3.    CL**8
00120  77  B                           PIC S99      VALUE ZERO  COMP-3.    CL**8
00121  77  I-TOTAL                     PIC S9(5)V99 VALUE ZERO  COMP-3.    CL**8
00122  77  LAS-CRT                     PIC X(7).                           CL**8
00123  77  D-FAC                       PIC S9V9(8)  COMP-3.                CL**8
00124  77  LINE-LIM                    PIC S99      VALUE +50.             CL**8
00125  77  PURGE-CNT                   PIC 9(7)     VALUE ZERO.            CL**8
00126  77  CERT-CNT                    PIC 9(7)     VALUE ZERO.            CL**8
00127                                                                      CL**8
00128  01  INITIALIZED-ALPHA-RECORD    PIC X(300).                         CL**8
00129                                                                      CL**8
00130  01  WS-SAVE-PRINT               PIC X(133).                         CL**8
00131                                                                      CL**8
00132  01  WS-TOTAL-ACCUMS.                                                CL**8
00133      03  WS-LEVEL-1              PIC S9(9)V99  OCCURS 8  COMP-3.     CL**8
00134                                                                      CL**8
00135  01  WS-ACC-EFF-CNTRL.                                               CL**8
00136      12  FILLER                  PIC X(19)    VALUE LOW-VALUES.      CL**8
00137      12  WS-ACC-EFF-DATE         PIC 9(11) COMP-3 VALUE 0.           CL*16
00138                                                                      CL**8
00139  01  WS-ACC-EXP-CNTRL            PIC X(25)    VALUE LOW-VALUES.      CL**8
00140                                                                      CL**8
00141  01  WS-HOLD-ACCUMS.                                                 CL**8
00142      03  WS-HOLD-1               PIC S9(9)V99  OCCURS 8  COMP-3.     CL**8
00143                                                                      CL**8
00144  01  WS-ABEND-FIELDS.                                                CL**8
00145      03  WS-RETURN-CODE          PIC S9(3)    VALUE +0.              CL**8
00146      03  WS-ZERO                 PIC S9       VALUE +0.              CL**8
00147      03  WS-ABEND-CODE           PIC X(4)     VALUE ZEROS.           CL**8
00148      03  WS-ABEND-OPTION         PIC X        VALUE 'Y'.             CL**8
00149      03  WS-ABEND-MESSAGE        PIC X(80)    VALUE SPACES.          CL**8
00150      03  WS-ABEND-FILE-STATUS    PIC XX       VALUE SPACES.          CL**8
00151                                                                      CL**8
00152  01  WS-LF-EXIT-DATE             PIC XX.                             CL**8
00153  01  WS-AH-EXIT-DATE             PIC XX.                             CL**8
00154                                                                      CL**8
00155  01  BIN-EP-DT                   PIC XX.                             CL**8
00156  01  WS-CERT-LF-EXP-DT           PIC XX.                             CL**8
00157  01  WS-CERT-AH-EXP-DT           PIC XX.                             CL**8
00158                                                                      CL**8
00159  01  MISC.                                                           CL**8
00160      05  LF-REM-TRM2             PIC S999V99.                        CL**8
00161      05  AH-REM-TRM2             PIC S999V99.                        CL**8
00162                                                                      CL**8
00163      05  LF-BAL-REMTERM          PIC S999V99.                        CL**8
00164                                                                      CL**8
00165      05  O-B-SWITCH              PIC X        VALUE SPACES.          CL**8
00166          88  O-B-CERTIFICATE                  VALUE '*'.             CL**8
00167      05  SUMMARY-SWITCH          PIC X        VALUE SPACES.          CL**8
00168          88  SUMMARY-CERTIFICATE              VALUE '*'.             CL**8
00169      05  BIN-ENTRY-DT            PIC XX       VALUE SPACES.          CL**8
00170      05  ENTRY-CENTURY           PIC 99       VALUE 0.               CL**8
00171                                                                      CL**8
00172  EJECT                                                               CL**8
00173  01  HEAD-A.                                                         CL**8
00174      03  FILLER                  PIC X(53)    VALUE SPACES.          CL**8
00175      03  FILLER                  PIC X(26)    VALUE                  CL**8
00176                      'SUMMARY OF PURGED ACTIVITY'.                   CL**8
00177      03  FILLER                  PIC X(41)    VALUE SPACES.          CL**8
00178      03  FILLER                  PIC X(8)     VALUE 'ECS095 '.       CL**8
00179                                                                      CL**8
00180  01  HEAD-B.                                                         CL**8
00181      03  FILLER                  PIC X(51)   VALUE SPACES.           CL**8
00182      03  HA-COM-NAME             PIC X(30).                          CL**8
00183      03  FILLER                  PIC X(39)   VALUE SPACES.           CL**8
00184      03  HB-IPL                  PIC X(8).                           CL**8
00185                                                                      CL**8
00186  01  HEAD-C.                                                         CL**8
00187      03  FILLER                  PIC X(57)   VALUE SPACES.           CL**8
00188      03  HD-DATE                 PIC X(18).                          CL**8
00189      03  FILLER                  PIC X(45)   VALUE SPACES.           CL**8
00190      03  FILLER                  PIC X(5)    VALUE 'PAGE '.          CL**8
00191      03  HD-PAGE                 PIC ZZ,ZZ9.                         CL**8
00192                                                                      CL**8
00193  01  HEAD-D.                                                         CL**8
00194      03  FILLER                  PIC X       VALUE SPACES.           CL**8
00195      03  FILLER                  PIC X(24)   VALUE                   CL**8
00196                            'EFFECTIVE DATE OF PURGE '.               CL**8
00197      03  HC-MO                   PIC Z9.                             CL**8
00198      03  FILLER                  PIC X       VALUE '/'.              CL**8
00199      03  HC-DAY                  PIC 99.                             CL**8
00200      03  FILLER                  PIC X       VALUE '/'.              CL**8
00201      03  HC-YEAR                 PIC 99.                             CL**8
00202                                                                      CL**8
00203  EJECT                                                               CL**8
00204  01  DETAIL-LINE-1.                                                  CL**8
00205      03  FILLER                  PIC XXX    VALUE SPACES.            CL**8
00206      03  FILLER                  PIC X(33)  VALUE                    CL**8
00207              'CERTIFICATES ON ORIGINAL MASTER :'.                    CL**8
00208      03  DL-CERT-ORIG            PIC ZZZZZZ9-.                       CL**8
00209                                                                      CL**8
00210  01  DETAIL-LINE-2.                                                  CL**8
00211      03  FILLER                  PIC XXX    VALUE SPACES.            CL**8
00212      03  FILLER                  PIC X(33)  VALUE                    CL**8
00213              'CERTIFICATES ON UPDATED MASTER  :'.                    CL**8
00214      03  DL-CERT-UPDT            PIC ZZZZZZ9-.                       CL**8
00215                                                                      CL**8
00216  01  DETAIL-LINE-3.                                                  CL**8
00217      03  FILLER                  PIC XXX    VALUE SPACES.            CL**8
00218      03  FILLER                  PIC X(33)  VALUE                    CL**8
00219              'PURGED CERTIFICATES             :'.                    CL**8
00220      03  DL-CERT-PURGE           PIC ZZZZZZ9-.                       CL**8
00221                                                                      CL**8
00222  01  DETAIL-LINE-4.                                                  CL**8
00223      03  FILLER                  PIC XXX    VALUE SPACES.            CL**8
00224      03  FILLER                  PIC X(33)  VALUE                    CL**8
00225              'ALPHA RECORDS WRITTEN           :'.                    CL**8
00226      03  DL-ALPHA-WRT            PIC ZZZZZZ9-.                       CL**8
00227                                                                      CL**8
00228  01  DETAIL-LINE-5.                                                  CL**8
00229      03  FILLER                  PIC X(33)  VALUE SPACES.            CL**8
00230      03  FILLER                  PIC X(4)   VALUE 'LIFE'.            CL**8
00231      03  FILLER                  PIC X(12)  VALUE SPACES.            CL**8
00232      03  FILLER                  PIC X(4)   VALUE 'A&H '.            CL**8
00233                                                                      CL**8
00234  01  DETAIL-LINE-6.                                                  CL**8
00235      03  FILLER                  PIC X      VALUE SPACE.             CL**8
00236      03  FILLER                  PIC X(20)  VALUE                    CL**8
00237                'PURGED STATISTICS***'.                               CL**8
00238                                                                      CL**8
00239  01  DETAIL-LINE-7.                                                  CL**8
00240      03  FILLER                  PIC XXX    VALUE SPACES.            CL**8
00241      03  FILLER                  PIC X(25)  VALUE                    CL**8
00242              'EXPIRED    COVERAGES    :'.                            CL**8
00243      03  FILLER                  PIC XXX    VALUE SPACES.            CL**8
00244      03  DL-EXPIRED-LF           PIC ZZZZZZ9-.                       CL**8
00245      03  FILLER                  PIC X(8)   VALUE SPACES.            CL**8
00246      03  DL-EXPIRED-AH           PIC ZZZZZZ9-.                       CL**8
00247                                                                      CL**8
00248  01  DETAIL-LINE-8.                                                  CL**8
00249      03  FILLER                  PIC XXX    VALUE SPACES.            CL**8
00250      03  FILLER                  PIC X(25)  VALUE                    CL**8
00251              'CANCELLED  COVERAGES    :'.                            CL**8
00252      03  FILLER                  PIC XXX    VALUE SPACES.            CL**8
00253      03  DL-CANCLED-LF           PIC ZZZZZZ9-.                       CL**8
00254      03  FILLER                  PIC X(8)   VALUE SPACES.            CL**8
00255      03  DL-CANCLED-AH           PIC ZZZZZZ9-.                       CL**8
00256                                                                      CL**8
00257  01  DETAIL-LINE-9.                                                  CL**8
00258      03  FILLER                  PIC XXX    VALUE SPACES.            CL**8
00259      03  FILLER                  PIC X(25)  VALUE                    CL**8
00260              'CLAIMED    COVERAGES    :'.                            CL**8
00261      03  FILLER                  PIC XXX    VALUE SPACES.            CL**8
00262      03  DL-DEATH-LF             PIC ZZZZZZ9-.                       CL**8
00263      03  FILLER                  PIC X(8)   VALUE SPACES.            CL**8
00264      03  DL-DEATH-AH             PIC ZZZZZZ9-.                       CL**8
00265  01  DETAIL-LINE-10.                                                 CL**8
00266      03  FILLER                  PIC XXX    VALUE SPACES.            CL**8
00267      03  FILLER                  PIC X(25)  VALUE                    CL**8
00268              'TOTAL PURGED COVERAGES  :'.                            CL**8
00269      03  FILLER                  PIC XXX    VALUE SPACES.            CL**8
00270      03  DL-TOTAL-LF             PIC ZZZZZZ9-.                       CL**8
00271      03  FILLER                  PIC X(8)   VALUE SPACES.            CL**8
00272      03  DL-TOTAL-AH             PIC ZZZZZZ9-.                       CL**8
00273  01  FOOTNOTE-1.                                                     CL**8
00274      03  FILLER                  PIC X(30)  VALUE SPACES.            CL**8
00275      03  FILLER                  PIC X(19)  VALUE                    CL**8
00276              '** END OF REPORT **'.                                  CL**8
00277  EJECT                                                               CL**8
00278      COPY ELCDTECX.                                                  CL**8
00279                                                                      CL**8
00280      COPY ELCDTEVR.                                                  CL**8
00281                                                                      CL**8
00282      COPY ELCCRTVR.                                                  CL**8
00283                                                                      CL**8
00284  01  TOTAL-LINE-A.                                                   CL**8
00285      03  FILLER                  PIC X(12)  VALUE SPACES.            CL**8
00286      03  FILLER                  PIC X(19)  VALUE                    CL**8
00287                              'PURGED TOTALS'.                        CL**8
00288      03  TLA-1                   PIC ZZZ,ZZZ,ZZZ.99.                 CL**8
00289      03  FILLER                  PIC X(7)   VALUE SPACES.            CL**8
00290      03  TLA-2                   PIC ZZZ,ZZZ,ZZZ.99.                 CL**8
00291      03  FILLER                  PIC X(7)   VALUE SPACES.            CL**8
00292      03  TLA-3                   PIC ZZZ,ZZZ,ZZZ.99.                 CL**8
00293      03  FILLER                  PIC X(8)   VALUE SPACES.            CL**8
00294      03  TLA-4                   PIC ZZZ,ZZZ,ZZZ.99.                 CL**8
00295                                                                      CL**8
00296  01  TOTAL-LINE-B.                                                   CL**8
00297      03  FILLER                  PIC X(42)  VALUE SPACES.            CL**8
00298      03  TLB-1                   PIC ZZZ,ZZZ,ZZZ.99.                 CL**8
00299      03  FILLER                  PIC X(6)   VALUE SPACES.            CL**8
00300      03  TLB-2                   PIC ZZZ,ZZZ,ZZZ.99.                 CL**8
00301      03  FILLER                  PIC X(8)   VALUE SPACES.            CL**8
00302      03  TLB-3                   PIC ZZZ,ZZZ,ZZZ.99.                 CL**8
00303      03  FILLER                  PIC X(7)   VALUE SPACES.            CL**8
00304      03  TLB-4                   PIC ZZZ,ZZZ,ZZZ.99.                 CL**8
00305                                                                      CL**8
00306  01  TOTAL-LINE-C.                                                   CL**8
00307      03  FILLER                  PIC X(23)   VALUE                   CL**8
00308                                '     CERTIFICATES READ'.             CL**8
00309      03  TLC-1                   PIC Z,ZZZ,ZZZ.                      CL**8
00310      03  FILLER                  PIC X(26)   VALUE                   CL**8
00311                                 '    CERTIFICATES WRITTEN'.          CL**8
00312      03  TLC-2                   PIC Z,ZZZ,ZZZ.                      CL**8
00313      03  FILLER                  PIC X(26)   VALUE                   CL**8
00314                                 '    CERTIFICATES PURGED'.           CL**8
00315      03  TLC-3                   PIC Z,ZZZ,ZZZ.                      CL**8
00316                                                                      CL**8
00317  01  TOTAL-ACCUMS.                                                   CL**8
00318      03  LEVEL-1                 PIC S9(9)V99  OCCURS 8 COMP-3.      CL**8
00319                                                                      CL**8
00320  EJECT                                                               CL**8
00321      COPY ELCDATE.                                                   CL*23
00322  EJECT                                                               CL**8
00323      COPY ELCCALC.                                                   CL**8
00324  EJECT                                                               CL**8
00325      COPY ECSAEX01.                                                  CL**8
00326  EJECT                                                               CL**9
00327      COPY ELCAEXVR.                                                  CL**9
00328                                                                      CL**8
00329  EJECT                                                               CL**8
00330  PROCEDURE DIVISION.                                                 CL**8
00331  0000-GET-DATE-CARD.                                                 CL**8
00332                              COPY ELCDTERX.                          CL**8
00333                                                                      CL**8
00334  0100-CHECK-PURGE-DATE.                                              CL**8
00335                                                                      CL**8
00336      IF EP-MO = 12  AND                                              CL**8
00337         EP-DA = 31  AND                                              CL**8
00338         EP-CCYY LESS THAN RUN-CCYY                                   CL**8
00339          GO TO 0110-OPEN-FILES.                                      CL**8
00340                                                                      CL**8
00341      MOVE 'PURGE DATE NOT ACCEPTABLE - RUN IS STOPPED'               CL**8
00342                                  TO WS-ABEND-MESSAGE.                CL**8
00343      MOVE '0998'                 TO WS-ABEND-CODE.                   CL**8
00344      GO TO ABEND-PGM.                                                CL**8
00345                                                                      CL**8
00346  0110-OPEN-FILES.                                                    CL**8
00347                                                                      CL**8
00348      OPEN INPUT  CERTS-IN                                            CL**8
00349                  ACC-MSTR                                            CL**8
00350           OUTPUT CERTS-OUT                                           CL**8
00351                  PURGE-CRT                                           CL**8
00352                  PRT-PURGE.                                          CL**8
00353                                                                      CL**8
00354      IF DTE-PRC-OPT = '1'                                            CL**8
00355          OPEN OUTPUT ALPHA-EXTRACT.                                  CL**8
00356                                                                      CL**8
00357      MOVE COMPANY-NAME           TO HA-COM-NAME.                     CL**8
00358      ACCEPT WS-ACCEPT-DATE       FROM DATE.                          CL**8
00359      MOVE WS-AD-YY               TO WS-CD-YY.                        CL**8
00360      MOVE WS-AD-MM               TO WS-CD-MM.                        CL**8
00361      MOVE WS-AD-DD               TO WS-CD-DD.                        CL**8
00362      MOVE WS-CURRENT-DATE        TO HB-IPL.                          CL**8
00363      MOVE EP-MO                  TO HC-MO.                           CL**8
00364      MOVE EP-DA                  TO HC-DAY.                          CL**8
00365      MOVE EP-YR                  TO HC-YEAR.                         CL**8
00366      MOVE ALPH-DATE              TO HD-DATE.                         CL**8
00367                                                                      CL**8
00368      MOVE EP-DT                  TO DC-GREG-DATE-CYMD                CL*20
00369      MOVE 'L'                    TO DC-OPTION-CODE.                  CL*20
00370      PERFORM 0600-DATE-CONVERSION-ROUTINE THRU 0699-EXIT.            CL**8
00371      MOVE DC-BIN-DATE-1          TO BIN-EP-DT.                       CL**8
00372                                                                      CL**8
00373      MOVE SPACES                 TO ALPHA-RECORD.                    CL**8
00374      MOVE ZEROS                  TO AX-LF-TYP        AX-AH-TYP       CL**8
00375                                     AX-LF-TERM       AX-AH-TERM      CL**8
00376                                     AX-LF-REMTERM    AX-AH-REMTERM   CL**8
00377                                     AX-LF-AMT        AX-AH-AMT       CL**8
00378                                     AX-LF-REMAMT     AX-AH-REMAMT    CL**8
00379                                     AX-LF-PRM        AX-AH-PRM       CL**8
00380                                     AX-LF-REFUND     AX-AH-REFUND    CL**8
00381                                     AX-LF-CLAIM-PMTS                 CL**8
00382                                     AX-AH-CLAIM-PMTS                 CL**8
00383                                     AX-ISS-MICROFILM-NO              CL**8
00384                                     AX-CAN-MICROFILM-NO              CL**8
00385                                     AX-LF-AMT-ALT                    CL**8
00386                                     AX-LF-REMAMT-ALT                 CL**8
00387                                     AX-LF-PRM-ALT.                   CL**8
00388                                                                      CL**8
00389      MOVE ALPHA-RECORD           TO INITIALIZED-ALPHA-RECORD.        CL**8
00390                                                                      CL**8
00391      COPY ELCAEXM1.                                                  CL*12
00392                                                                      CL**8
00393      PERFORM 0410-READ-ACCOUNT-B THRU 0499-READ-EXIT.                CL**8
00394                                                                      CL**8
00395  0190-PRINT-HEADERS.                                                 CL**8
00396                                                                      CL**8
00397      ADD 1                       TO PAGE-NO.                         CL**8
00398      MOVE PAGE-NO                TO HD-PAGE.                         CL**8
00399      MOVE TP                     TO X.                               CL**8
00400      MOVE HEAD-A                 TO PRT.                             CL**8
00401      PERFORM 0500-PRT-RTN        THRU 0599-PRT-EXIT.                 CL**8
00402                                                                      CL**8
00403      MOVE SS                     TO X.                               CL**8
00404      MOVE HEAD-B                 TO PRT.                             CL**8
00405      PERFORM 0500-PRT-RTN        THRU 0599-PRT-EXIT.                 CL**8
00406                                                                      CL**8
00407      MOVE HEAD-C                 TO PRT.                             CL**8
00408      PERFORM 0500-PRT-RTN        THRU 0599-PRT-EXIT.                 CL**8
00409                                                                      CL**8
00410      MOVE TS                     TO X.                               CL**8
00411      MOVE HEAD-D                 TO PRT.                             CL**8
00412      PERFORM 0500-PRT-RTN        THRU 0599-PRT-EXIT.                 CL**8
00413                                                                      CL**8
00414      MOVE SPACES                 TO PRT.                             CL**8
00415                                                                      CL**8
00416  EJECT                                                               CL**8
00417  0200-READ-LOOP.                                                     CL**8
00418                                                                      CL**8
00419      READ CERTS-IN                                                   CL**8
00420         AT END  GO TO 0900-EOJ-FIRST.                                CL**8
00421                                                                      CL**8
00422      ADD +1                      TO TOTX-CERTS.                      CL**8
00423                                                                      CL**8
00424      MOVE SPACES                 TO O-B-SWITCH                       CL**8
00425                                     SUMMARY-SWITCH.                  CL**8
00426                                                                      CL**8
00427      COPY ELCCRTM1.                                                  CL*13
00428                                                                      CL**8
00429      IF CR-LF-EXPIRE-DATE NOT NUMERIC                                CL**8
00430          MOVE ZEROS              TO CR-LF-EXPIRE-DATE.               CL**8
00431      IF CR-AH-EXPIRE-DATE NOT NUMERIC                                CL**8
00432          MOVE ZEROS              TO CR-AH-EXPIRE-DATE.               CL**8
00433                                                                      CL**8
00434  0210-MATCH-TO-ACCOUNT.                                              CL**8
00435                                                                      CL**8
00436      IF CR-CONTROL-1 NOT LESS WS-ACC-EXP-CNTRL                       CL**8
00437          PERFORM 0400-READ-ACCOUNT THRU 0499-READ-EXIT               CL**8
00438          GO TO 0210-MATCH-TO-ACCOUNT.                                CL**8
00439                                                                      CL**8
00440      IF CR-CONTROL-1 LESS WS-ACC-EFF-CNTRL                           CL**8
00441          DISPLAY CR-CONTROL-1 ' NO ACCOUNT MASTER FOR THIS CERT'     CL**8
00442          MOVE 'NO ACCOUNT MASTER FOR CERTIFICATE '                   CL**8
00443                                  TO WS-ABEND-MESSAGE                 CL**8
00444          MOVE '0998'             TO WS-ABEND-CODE                    CL**8
00445          GO TO ABEND-PGM.                                            CL**8
00446                                                                      CL**8
00447 *  VOIDS AND DECLINES ARE KEPT FOR AN AUDIT TRAIL OF                 CL**8
00448 *  DECLINED OR VOIDED CERTIFICATES....                               CL**8
00449                                                                      CL**8
00450      IF CR-POLICY-IS-DECLINED  OR                                    CL**8
00451         CR-POLICY-IS-VOID                                            CL**8
00452          GO TO 0290-WRITE-GOOD-CERT.                                 CL**8
00453                                                                      CL**8
00454      MOVE CR-ENTRY-DATE          TO DC-GREG-DATE-CYMD.               CL*20
00455      MOVE 'L'                    TO DC-OPTION-CODE.                  CL*20
00456      PERFORM 0600-DATE-CONVERSION-ROUTINE THRU 0699-EXIT.            CL**8
00457      MOVE DC-BIN-DATE-1          TO BIN-ENTRY-DT.                    CL**8
00458      MOVE DC-ALPHA-CEN-N         TO ENTRY-CENTURY.                   CL**8
00459      IF BIN-ENTRY-DT GREATER THAN BIN-EP-DT OR                       CL**8
00460         CR-CCYY GREATER THAN EP-CCYY                                 CL**8
00461          GO TO 0290-WRITE-GOOD-CERT.                                 CL**8
00462                                                                      CL**8
00463      IF CR-DISAMT-YTD NOT = ZEROS                                    CL**8
00464          GO TO 0290-WRITE-GOOD-CERT.                                 CL**8
00465                                                                      CL**8
00466      MOVE LOW-VALUES             TO WS-LF-EXIT-DATE                  CL**8
00467                                     WS-AH-EXIT-DATE.                 CL**8
00468                                                                      CL**8
00469      IF CR-LF-CURRENT-STATUS = '7'                                   CL**8
00470          MOVE CR-DTH-DT          TO DC-GREG-DATE-CYMD                CL*22
00471          MOVE 'L'                TO DC-OPTION-CODE                   CL*19
00472          PERFORM 0600-DATE-CONVERSION-ROUTINE THRU 0699-EXIT         CL**8
00473          IF NO-CONVERSION-ERROR                                      CL**8
00474             MOVE DC-BIN-DATE-1   TO WS-LF-EXIT-DATE                  CL**8
00475          ELSE                                                        CL**8
00476             MOVE 'CR-DTH-DT ERROR'  TO WS-ABEND-MESSAGE              CL**8
00477             MOVE DC-OPTION-CODE  TO  WS-ABEND-FILE-STATUS            CL**8
00478             GO TO ABEND-PGM.                                         CL*17
00479                                                                      CL**8
00480      IF CR-LF-CURRENT-STATUS = '6'  OR  '8'                          CL**8
CIDMOD         MOVE CR-LF-CANCEL-EXIT-DATE
CIDMOD                                 TO DC-GREG-DATE-CYMD
00482          MOVE 'L'                TO DC-OPTION-CODE                   CL*18
00483          PERFORM 0600-DATE-CONVERSION-ROUTINE  THRU 0699-EXIT        CL**8
00484          IF NO-CONVERSION-ERROR                                      CL**8
00485             MOVE DC-BIN-DATE-1   TO WS-LF-EXIT-DATE                  CL**8
00486          ELSE                                                        CL**8
CIDMOD            MOVE 'CR-LF-CANCEL-EXIT-DATE ERROR'
CIDMOD                               TO WS-ABEND-MESSAGE
00488             MOVE DC-OPTION-CODE     TO  WS-ABEND-FILE-STATUS         CL**8
00489             GO TO ABEND-PGM.                                         CL*17
00490                                                                      CL**8
00491      IF CR-AH-CURRENT-STATUS =  '6'                                  CL**8
00492          MOVE CR-AH-SETTLEMENT-EXIT-DATE TO                          CL*19
00493                              DC-GREG-DATE-CYMD                       CL*19
00494          MOVE 'L'                TO DC-OPTION-CODE                   CL*19
00495          PERFORM 0600-DATE-CONVERSION-ROUTINE THRU 0699-EXIT         CL**8
00496          IF NO-CONVERSION-ERROR                                      CL**8
00497             MOVE DC-BIN-DATE-1   TO WS-AH-EXIT-DATE                  CL**8
00498          ELSE                                                        CL**8
00499             MOVE 'CR-AH-SETTLEMENT-EXIT-DATE ERROR'  TO              CL**8
00500                                    WS-ABEND-MESSAGE                  CL**8
00501             MOVE DC-OPTION-CODE  TO  WS-ABEND-FILE-STATUS            CL**8
00502             GO TO ABEND-PGM.                                         CL*17
00503                                                                      CL**8
00504      IF CR-AH-CURRENT-STATUS =  '7'  OR  '8'                         CL**8
CIDMOD         MOVE CR-AH-CANCEL-EXIT-DATE TO                              CL*19
00506                              DC-GREG-DATE-CYMD                       CL*19
00507          MOVE 'L'                TO DC-OPTION-CODE                   CL*19
00508          PERFORM 0600-DATE-CONVERSION-ROUTINE THRU 0699-EXIT         CL**8
00509          IF NO-CONVERSION-ERROR                                      CL**8
00510             MOVE DC-BIN-DATE-1   TO WS-AH-EXIT-DATE                  CL**8
00511          ELSE                                                        CL**8
CIDMOD            MOVE 'CR-AH-CANCEL-EXIT-DATE ERROR'  TO                  CL**8
00513                                    WS-ABEND-MESSAGE                  CL**8
00514             MOVE DC-OPTION-CODE  TO  WS-ABEND-FILE-STATUS            CL**8
00515             GO TO ABEND-PGM.                                         CL*17
00516                                                                      CL**8
00517      IF WS-LF-EXIT-DATE GREATER THAN BIN-EP-DT OR                    CL**8
00518         WS-AH-EXIT-DATE GREATER THAN BIN-EP-DT                       CL**8
00519          GO TO 0290-WRITE-GOOD-CERT.                                 CL**8
00520                                                                      CL**8
00521      MOVE LOW-VALUES             TO WS-CERT-LF-EXP-DT                CL**8
00522                                     WS-CERT-AH-EXP-DT.               CL**8
00523                                                                      CL**8
00524      IF CR-LF-EXPIRE-DATE NOT = ZEROS                                CL**8
00525          MOVE CR-LF-EXPIRE-DATE  TO DC-GREG-DATE-CYMD                CL*19
00526          MOVE 'L'                TO DC-OPTION-CODE                   CL*19
00527          PERFORM 0600-DATE-CONVERSION-ROUTINE THRU 0699-EXIT         CL**8
00528          IF NO-CONVERSION-ERROR                                      CL**8
00529              MOVE DC-BIN-DATE-1  TO WS-CERT-LF-EXP-DT.               CL**8
00530                                                                      CL**8
00531      IF CR-AH-EXPIRE-DATE NOT = ZEROS                                CL**8
00532          MOVE CR-AH-EXPIRE-DATE  TO DC-GREG-DATE-CYMD                CL*19
00533          MOVE 'L'                TO DC-OPTION-CODE                   CL*19
00534          PERFORM 0600-DATE-CONVERSION-ROUTINE THRU 0699-EXIT         CL**8
00535          IF NO-CONVERSION-ERROR                                      CL**8
00536              MOVE DC-BIN-DATE-1  TO WS-CERT-AH-EXP-DT.               CL**8
00537                                                                      CL**8
CIDMOD     IF WS-LF-EXIT-DATE = LOW-VALUES                                 CL**8
00539          IF WS-CERT-LF-EXP-DT GREATER THAN BIN-EP-DT                 CL**8
00540              GO TO 0290-WRITE-GOOD-CERT.                             CL**8
00541                                                                      CL**8
00542      IF WS-AH-EXIT-DATE = LOW-VALUES                                 CL**8
00543          IF WS-CERT-AH-EXP-DT GREATER THAN BIN-EP-DT                 CL**8
00544              GO TO 0290-WRITE-GOOD-CERT.                             CL**8
00545                                                                      CL**8
00546      GO TO 0300-PROCESS-PURGES.                                      CL**8
00547                                                                      CL**8
00548                                                                      CL**8
00549  0290-WRITE-GOOD-CERT.                                               CL**8
00550                                                                      CL**8
00551      COPY ELCCRTM2.                                                  CL**8
00552                                                                      CL**8
00553      WRITE OUT-CERT              FROM CERTIFICATE-RECORD.            CL**8
00554                                                                      CL**8
00555      ADD +1                      TO CERTS-GOOD.                      CL**8
00556                                                                      CL**8
00557      GO TO 0200-READ-LOOP.                                           CL**8
00558                                                                      CL**8
00559                                                                      CL**8
00560  EJECT                                                               CL**8
00561  0300-PROCESS-PURGES.                                                CL**8
00562                                                                      CL**8
00563      MOVE CLAS-STARTL            TO CLAS-INDEXL.                     CL**8
00564      MOVE CLAS-STARTA            TO CLAS-INDEXA.                     CL**8
00565      MOVE CLAS-STARTS            TO CLAS-INDEXS.                     CL**8
00566                                                                      CL**8
00567  0310-STATE-LOOKUP.                                                  CL**8
00568                                                                      CL**8
00569      IF CR-STATE NOT = STATE-SUB (CLAS-INDEXS)                       CL**8
00570          IF CLAS-INDEXS NOT = CLAS-MAXS                              CL**8
00571              ADD +1              TO CLAS-INDEXS                      CL**8
00572              GO TO 0310-STATE-LOOKUP.                                CL**8
00573                                                                      CL**8
00574      IF CR-LFTYP = ZERO                                              CL**8
00575          MOVE ZERO               TO CLAS-INDEXL                      CL**8
00576          GO TO 0330-FIND-AH.                                         CL**8
00577                                                                      CL**8
00578  0320-FIND-LIFE-LOOP.                                                CL**8
00579                                                                      CL**8
00580      IF CLAS-INDEXL GREATER CLAS-MAXL                                CL**8
00581          OR CLAS-STARTL = ZERO                                       CL**8
00582          DISPLAY 'LIFE BENEFIT ' CR-LFTYP ' NOT IN TABLE'            CL**8
00583          MOVE 0401               TO WS-RETURN-CODE                   CL**8
00584          GO TO ABEND-PGM.                                            CL**8
00585                                                                      CL**8
00586      IF CR-LFTYP NOT = CLAS-I-BEN (CLAS-INDEXL)                      CL**8
00587          ADD +1                  TO CLAS-INDEXL                      CL**8
00588          GO TO 0320-FIND-LIFE-LOOP.                                  CL**8
00589                                                                      CL**8
00590      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'B'                         CL**8
00591          MOVE '*'                TO O-B-SWITCH.                      CL**8
00592                                                                      CL**8
00593      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'Z'                         CL**8
00594          MOVE '*'                TO SUMMARY-SWITCH.                  CL**8
00595                                                                      CL**8
00596  0330-FIND-AH.                                                       CL**8
00597                                                                      CL**8
00598      IF CR-AHTYP = ZERO                                              CL**8
00599          MOVE ZERO               TO CLAS-INDEXA                      CL**8
00600          GO TO 0340-CALC-REM-TERM.                                   CL**8
00601                                                                      CL**8
00602  0335-FIND-AH-LOOP.                                                  CL**8
00603                                                                      CL**8
00604      IF CLAS-INDEXA GREATER CLAS-MAXA                                CL**8
00605         OR CLAS-STARTA = ZEROS                                       CL**8
00606          DISPLAY 'A&H BENEFIT ' CR-AHTYP ' NOT IN TABLE'             CL**8
00607          MOVE 0402               TO WS-RETURN-CODE                   CL**8
00608          GO TO ABEND-PGM.                                            CL**8
00609                                                                      CL**8
00610      IF CR-AHTYP NOT = CLAS-I-BEN (CLAS-INDEXA)                      CL**8
00611          ADD +1                  TO CLAS-INDEXA                      CL**8
00612          GO TO 0335-FIND-AH-LOOP.                                    CL**8
00613                                                                      CL**8
00614      IF CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'B'                         CL**8
00615          MOVE '*'                TO O-B-SWITCH.                      CL**8
00616                                                                      CL**8
00617      IF CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'Z'                         CL**8
00618          MOVE '*'                TO SUMMARY-SWITCH.                  CL**8
00619                                                                      CL**8
00620  0340-CALC-REM-TERM.                                                 CL**8
00621                                                                      CL**8
00622      MOVE CR-DT                  TO DC-GREG-DATE-CYMD                CL*21
00623      MOVE 'L'                    TO DC-OPTION-CODE.                  CL*21
00624      PERFORM 0600-DATE-CONVERSION-ROUTINE THRU 0699-EXIT             CL**8
00625      MOVE DC-BIN-DATE-1          TO CP-CERT-EFF-DT.                  CL**8
00626                                                                      CL**8
00627      IF CR-LOAN-1ST-PMT-DT NOT NUMERIC                               CL**8
00628          MOVE ZEROS              TO CR-LOAN-1ST-PMT-DT.              CL**8
00629                                                                      CL**8
00630      MOVE LOW-VALUES             TO CP-FIRST-PAY-DATE.               CL**8
00631                                                                      CL**8
00632      IF CR-LOAN-1ST-PMT-DT NOT = ZEROS                               CL**8
00633          MOVE CR-LOAN-1ST-PMT-DT TO DC-GREG-DATE-1-YMD               CL**8
00634          MOVE ZEROS              TO DC-ELAPSED-MONTHS                CL**8
00635                                     DC-ELAPSED-DAYS                  CL**8
00636          MOVE '3'                TO DC-OPTION-CODE                   CL**8
00637          PERFORM 0600-DATE-CONVERSION-ROUTINE THRU 0699-EXIT         CL**8
00638          IF NO-CONVERSION-ERROR                                      CL**8
00639              MOVE DC-BIN-DATE-1  TO CP-FIRST-PAY-DATE                CL**8
00640          ELSE                                                        CL**8
00641              MOVE ZEROS          TO CR-LOAN-1ST-PMT-DT.              CL**8
00642                                                                      CL**8
00643      IF CP-FIRST-PAY-DATE  LESS THAN  CP-CERT-EFF-DT                 CL**8
00644          MOVE ZEROS              TO CR-LOAN-1ST-PMT-DT.              CL**8
00645                                                                      CL**8
00646      IF CR-LOAN-1ST-PMT-DT = ZEROS                                   CL**8
00647          MOVE CP-CERT-EFF-DT     TO DC-BIN-DATE-1                    CL**8
00648          MOVE +1                 TO DC-ELAPSED-MONTHS                CL**8
00649          MOVE ZEROS              TO DC-ELAPSED-DAYS                  CL**8
00650          MOVE '6'                TO DC-OPTION-CODE                   CL**8
00651          PERFORM 0600-DATE-CONVERSION-ROUTINE THRU 0699-EXIT         CL**8
00652          MOVE DC-BIN-DATE-2      TO CP-FIRST-PAY-DATE                CL**8
00653          MOVE DC-GREG-DATE-1-YMD TO CR-LOAN-1ST-PMT-DT.              CL**8
00654                                                                      CL**8
00655      MOVE BIN-EP-DT              TO CP-VALUATION-DT.                 CL**8
00656                                                                      CL**8
00657      MOVE STATE-SUB (CLAS-INDEXS)                                    CL**8
00658                                  TO CP-STATE.                        CL**8
00659      MOVE STATE-ABBR (CLAS-INDEXS)                                   CL**8
00660                                  TO CP-STATE-STD-ABBRV.              CL**8
00661      MOVE '3'                    TO CP-PROCESS-TYPE.                 CL**8
00662      MOVE DTE-CLASIC-COMPANY-CD  TO CP-COMPANY-CD.                   CL**8
00663      MOVE DTE-CLIENT             TO CP-COMPANY-ID.                   CL**8
00664      MOVE SPACES                 TO CP-ACCT-FLD-5.                   CL**8
00665      MOVE DTE-REM-TRM            TO CP-REM-TERM-METHOD.              CL**8
00666                                                                      CL**8
00667      IF CR-LFTYP = ZEROS                                             CL**8
00668          MOVE ZEROS              TO LF-REM-TRM2                      CL**8
00669          GO TO 0350-CALC-REM-TERM-AH.                                CL**8
00670                                                                      CL**8
00671      MOVE CLAS-I-RL-AH (CLAS-INDEXL)                                 CL**8
00672                                  TO CP-BENEFIT-TYPE.                 CL**8
00673      MOVE CLAS-I-BAL (CLAS-INDEXL)                                   CL**8
00674                                  TO CP-SPECIAL-CALC-CD.              CL**8
00675      MOVE CR-LF-TERM             TO CP-ORIGINAL-TERM                 CL**8
00676                                     CP-LOAN-TERM.                    CL**8
00677                                                                      CL*17
00678      IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')  AND           CL**8
00679         CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L'                     CL**8
00680          ADD +1                  TO CP-ORIGINAL-TERM                 CL**8
00681          ADD +1                  TO CP-LOAN-TERM.                    CL**8
00682                                                                      CL*17
00683      IF CP-TERM-IS-DAYS                                              CL**8
00684          MOVE CR-LF-TERM-IN-DAYS TO CP-TERM-OR-EXT-DAYS              CL**8
00685      ELSE                                                            CL**8
00686          MOVE ZEROS              TO CP-TERM-OR-EXT-DAYS.             CL**8
00687                                                                      CL**8
00688      MOVE DTE-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.         CL**8
00689                                                                      CL**8
00690      PERFORM 0700-GET-REMAINING-TERM THRU 0799-REM-TERM-EXIT.        CL**8
00691                                                                      CL**8
00692      IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')  AND           CL**8
00693         CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L'                     CL**8
00694          MOVE CP-REMAINING-TERM-2 TO LF-BAL-REMTERM                  CL**8
00695          COMPUTE CP-REMAINING-TERM-2 = CP-REMAINING-TERM-2 - 1.      CL**8
00696                                                                      CL**8
00697      IF CP-REMAINING-TERM-2 LESS THAN ZERO                           CL**8
00698          MOVE ZERO               TO CP-REMAINING-TERM-2.             CL**8
00699                                                                      CL**8
00700      MOVE CP-REMAINING-TERM-2    TO LF-REM-TRM2.                     CL**8
00701                                                                      CL**8
00702  0350-CALC-REM-TERM-AH.                                              CL**8
00703                                                                      CL**8
00704      IF CR-AHTYP = ZEROS                                             CL**8
00705          MOVE ZEROS              TO AH-REM-TRM2                      CL**8
00706          GO TO 0360-SET-UP-ALPHA.                                    CL**8
00707                                                                      CL**8
00708      IF CR-AH-TERM = CR-LF-TERM                                      CL**8
00709          MOVE CP-REMAINING-TERM-2    TO AH-REM-TRM2                  CL**8
00710          GO TO 0360-SET-UP-ALPHA.                                    CL**8
00711                                                                      CL**8
00712      MOVE CLAS-I-RL-AH (CLAS-INDEXA)                                 CL**8
00713                                  TO CP-BENEFIT-TYPE.                 CL**8
00714      MOVE CLAS-I-BAL (CLAS-INDEXA)                                   CL**8
00715                                  TO CP-SPECIAL-CALC-CD.              CL**8
00716      MOVE CR-AH-TERM             TO CP-ORIGINAL-TERM                 CL**8
00717                                     CP-LOAN-TERM.                    CL**8
00718      MOVE ZEROS                  TO CP-TERM-OR-EXT-DAYS.             CL**8
00719      MOVE DTE-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.         CL**8
00720                                                                      CL**8
00721      PERFORM 0700-GET-REMAINING-TERM THRU 0799-REM-TERM-EXIT.        CL**8
00722                                                                      CL**8
00723      MOVE CP-REMAINING-TERM-2    TO AH-REM-TRM2.                     CL**8
00724                                                                      CL**8
00725  0360-SET-UP-ALPHA.                                                  CL**8
00726                                                                      CL**8
00727      MOVE INITIALIZED-ALPHA-RECORD                                   CL**8
00728                                  TO ALPHA-RECORD.                    CL**8
00729                                                                      CL**8
00730      MOVE 'AX'                   TO AX-RECORD-ID.                    CL**8
00731                                                                      CL**8
00732      MOVE DTE-CLASIC-COMPANY-CD  TO AX-COMPANY-CD.                   CL**8
00733                                                                      CL**8
00734      MOVE CR-FULL-CONTROL        TO AX-CONTROL.                      CL**8
00735                                                                      CL**8
00736      MOVE CR-APR                 TO AX-APR.                          CL**8
00737      MOVE CR-IND-GRP             TO AX-IND-GRP.                      CL**8
00738      MOVE CR-PMT-FREQ            TO AX-PMT-FREQ.                     CL**8
00739                                                                      CL**8
00740      MOVE CR-ENTRY-STATUS        TO AX-ENTRY-STATUS.                 CL**8
00741      MOVE CR-ENTRY-DATE          TO AX-ENTRY.                        CL**8
00742      MOVE ENTRY-CENTURY          TO AX-E-CC.                         CL**8
00743 *    MOVE CR-ISS-MICROFILM-NO    TO AX-ISS-MICROFILM-NO.             CL**8
00744 *    MOVE CR-CAN-MICROFILM-NO    TO AX-CAN-MICROFILM-NO.             CL**8
00745                                                                      CL**8
00746      MOVE CR-NAME                TO AX-NAME.                         CL**8
00747      MOVE CR-AGE                 TO AX-AGE.                          CL**8
00748      MOVE CR-SEX                 TO AX-SEX.                          CL**8
00749                                                                      CL**8
00750      MOVE 'I'                    TO AX-ALPHA-TYPE-CODE.              CL**8
00751                                                                      CL**8
00752      IF O-B-CERTIFICATE                                              CL**8
00753          MOVE 'OUTSTANDING BAL'  TO AX-NAME                          CL**8
00754          MOVE 'O'                TO AX-ALPHA-TYPE-CODE.              CL**8
00755                                                                      CL**8
00756      IF SUMMARY-CERTIFICATE                                          CL**8
00757          MOVE 'SUMMARY CERT.'    TO AX-NAME                          CL**8
00758          MOVE 'S'                TO AX-ALPHA-TYPE-CODE.              CL**8
00759                                                                      CL**8
00760      MOVE CR-REIN-SPEC           TO AX-SPEC-REIN.                    CL**8
00761      MOVE CR-REIN-TABLE          TO AX-REIN-TABLE.                   CL**8
00762      MOVE CR-MEMBER-NO           TO AX-MEM-NO.                       CL**8
00763      MOVE CR-SOC-SEC             TO AX-SOC-NO.                       CL**8
00764                                                                      CL**8
00765  0370-ALPHA-LIFE-DATA.                                               CL**8
00766                                                                      CL**8
00767      IF CR-LFTYP = ZERO                                              CL**8
00768           GO TO 0380-ALPHA-A-H-DATA.                                 CL**8
00769                                                                      CL**8
00770      MOVE CR-LFTYP               TO AX-LF-TYP.                       CL**8
00771      MOVE CR-LF-TERM             TO AX-LF-TERM.                      CL**8
00772      MOVE LF-REM-TRM2            TO AX-LF-REMTERM.                   CL**8
00773      MOVE CR-LFAMT               TO AX-LF-AMT.                       CL**8
00774      MOVE CR-LFPRM               TO AX-LF-PRM.                       CL**8
00775                                                                      CL**8
00776      IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')                CL**8
00777          MOVE CR-LFAMT-ALT       TO AX-LF-AMT-ALT                    CL**8
00778          MOVE CR-LFPRM-ALT       TO AX-LF-PRM-ALT.                   CL**8
00779                                                                      CL**8
00780      MOVE CR-LFRFND              TO AX-LF-REFUND.                    CL**8
00781                                                                      CL**8
00782      MOVE CR-DTHAMT              TO AX-LF-CLAIM-PMTS.                CL**8
00783                                                                      CL**8
00784      MOVE CR-LF-CURRENT-STATUS   TO AX-LF-STATUS.                    CL**8
00785                                                                      CL**8
00786      MOVE ZEROS                  TO AX-LF-CNCL                       CL**8
00787                                     AX-DEATH                         CL**8
00788                                     AX-LF-EXIT.                      CL**8
00789                                                                      CL**8
00790      MOVE CR-LF-EXPIRE-DATE      TO AX-LF-EXPIRES.                   CL**8
00791                                                                      CL**8
00792      IF AX-LF-STATUS = '7'                                           CL**8
00793          MOVE CR-DTH-DT          TO AX-DEATH                         CL**8
00794          MOVE CR-LF-CLAIM-EXIT-DATE                                  CL**8
00795                                  TO AX-LF-EXIT                       CL**8
00796          GO TO 0380-ALPHA-A-H-DATA.                                  CL**8
00797                                                                      CL**8
00798      IF AX-LF-STATUS = '6'  OR  '8'                                  CL**8
00799          MOVE CR-LF-CANC-DT      TO AX-LF-CNCL                       CL**8
00800          MOVE CR-LF-CANCEL-EXIT-DATE                                 CL**8
00801                                  TO AX-LF-EXIT                       CL**8
00802          GO TO 0380-ALPHA-A-H-DATA.                                  CL**8
00803                                                                      CL**8
00804      IF ((CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')  AND          CL**8
00805          CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L'  AND               CL**8
00806          LF-BAL-REMTERM NOT GREATER THAN ZERO)                       CL**8
00807                        OR                                            CL**8
00808         ((CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')  AND          CL**8
00809          CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'L'  AND                   CL**8
00810          AX-LF-REMTERM NOT GREATER THAN ZERO)                        CL**8
00811                        OR                                            CL**8
00812         ((CLAS-I-EP (CLAS-INDEXL) NOT = 'B' AND 'K' AND 'L')  AND    CL**8
00813          AX-LF-REMTERM NOT GREATER ZERO)                             CL**8
00814          MOVE ZERO               TO AX-LF-REMTERM                    CL**8
00815          MOVE 'E'                TO AX-LF-STATUS                     CL**8
00816          GO TO 0380-ALPHA-A-H-DATA.                                  CL**8
00817                                                                      CL**8
00818      IF AX-LF-REMTERM GREATER AX-LF-TERM                             CL**8
00819          MOVE AX-LF-TERM         TO AX-LF-REMTERM                    CL**8
00820          MOVE 'F'                TO AX-LF-STATUS.                    CL**8
00821                                                                      CL**8
00822  0380-ALPHA-A-H-DATA.                                                CL**8
00823                                                                      CL**8
00824      IF CR-AHTYP = ZERO                                              CL**8
00825          GO TO 0390-CHECK-INFORCE-STATUS.                            CL**8
00826                                                                      CL**8
00827      MOVE CR-AHTYP               TO AX-AH-TYP.                       CL**8
00828      MOVE CR-AH-TERM             TO AX-AH-TERM.                      CL**8
00829      MOVE AH-REM-TRM2            TO AX-AH-REMTERM.                   CL**8
00830      MOVE CR-AHAMT               TO AX-AH-AMT.                       CL**8
00831      MOVE CR-AHPRM               TO AX-AH-PRM.                       CL**8
00832                                                                      CL**8
00833      MOVE CR-AHRFND              TO AX-AH-REFUND.                    CL**8
00834      MOVE CR-DISAMT              TO AX-AH-CLAIM-PMTS.                CL**8
00835                                                                      CL**8
00836      MOVE CR-AH-CURRENT-STATUS   TO AX-AH-STATUS.                    CL**8
00837      MOVE ZEROS                  TO AX-AH-CNCL                       CL**8
00838                                     AX-LUMP-SUM                      CL**8
00839                                     AX-AH-EXIT.                      CL**8
00840      MOVE CR-AH-EXPIRE-DATE      TO AX-AH-EXPIRES.                   CL**8
00841                                                                      CL**8
00842      IF AX-AH-STATUS = '6'                                           CL**8
00843          MOVE CR-DIS-DT          TO AX-LUMP-SUM                      CL**8
00844          MOVE CR-AH-SETTLEMENT-EXIT-DATE                             CL**8
00845                                  TO AX-AH-EXIT                       CL**8
00846          GO TO 0390-CHECK-INFORCE-STATUS.                            CL**8
00847                                                                      CL**8
00848      IF AX-AH-STATUS = '7'  OR  '8'                                  CL**8
00849          MOVE CR-AH-CANC-DT       TO AX-AH-CNCL                      CL**8
00850          MOVE CR-AH-CANCEL-EXIT-DATE                                 CL**8
00851                                   TO AX-AH-EXIT                      CL**8
00852          GO TO 0390-CHECK-INFORCE-STATUS.                            CL**8
00853                                                                      CL**8
00854      IF AX-AH-REMTERM NOT GREATER ZERO                               CL**8
00855          MOVE ZERO                TO AX-AH-REMTERM                   CL**8
00856          MOVE 'E'                 TO AX-AH-STATUS                    CL**8
00857          GO TO 0390-CHECK-INFORCE-STATUS.                            CL**8
00858                                                                      CL**8
00859      IF AX-AH-REMTERM GREATER AX-AH-TERM                             CL**8
00860          MOVE AX-AH-TERM          TO AX-AH-REMTERM                   CL**8
00861          MOVE 'F'                 TO AX-AH-STATUS.                   CL**8
00862                                                                      CL**8
00863  0390-CHECK-INFORCE-STATUS.                                          CL**8
00864                                                                      CL**8
00865      IF ((AX-LF-STATUS = ' '  OR  'E'  OR  '6'  OR  '7'  OR  '8')    CL**8
00866                 AND                                                  CL**8
00867          (AX-AH-STATUS = ' '  OR  'E'  OR  '6'  OR  '7'  OR  '8'))   CL**8
00868          PERFORM 0800-WRITE-ALPHA-EXTRACT THRU 0899-WRITE-ALPHA-X    CL**8
00869          GO TO 0395-WRITE-PURGED-CERTS.                              CL**8
00870                                                                      CL**8
00871      IF (CR-LFTYP NOT = ZEROS  AND                                   CL**8
00872          CR-LF-EXPIRE-DATE NOT = ZEROS)                              CL**8
00873                      OR                                              CL**8
00874         (CR-AHTYP NOT = ZEROS  AND                                   CL**8
00875          CR-AH-EXPIRE-DATE NOT = ZEROS)                              CL**8
00876          DISPLAY 'ACTIVE RCD: ' CR-FULL-CONTROL ' '                  CL**8
00877             CR-PMT-EXTENSION-DAYS '-' CR-LOAN-1ST-PMT-DT ' '         CL**8
00878             AX-ENTRY-STATUS ' LF: ' AX-LF-TYP '-' AX-LF-STATUS ' '   CL**8
00879             AX-LF-TERM '-' AX-LF-EXPIRES ' ' AX-LF-REMTERM ' '       CL**8
00880             '  AH: ' AX-AH-TYP '-' AX-AH-STATUS ' ' AX-AH-TERM '-'   CL**8
00881             AX-AH-EXPIRES ' ' AX-AH-REMTERM.                         CL**8
00882                                                                      CL**8
00883      COPY ELCCRTM1.                                                  CL**8
00884                                                                      CL**8
00885      GO TO 0290-WRITE-GOOD-CERT.                                     CL**8
00886                                                                      CL**8
00887                                                                      CL**8
00888  0395-WRITE-PURGED-CERTS.                                            CL**8
00889                                                                      CL**8
00890      IF AX-LF-TYP NOT = ZERO                                         CL**8
00891          ADD +1                  TO TOTX-LIFE                        CL**8
00892          IF AX-LF-STATUS = '7'                                       CL**8
00893              ADD +1              TO CERTS-DEATH-LF                   CL**8
00894          ELSE                                                        CL**8
00895              IF AX-LF-STATUS = '6'  OR  '8'                          CL**8
00896                  ADD +1          TO CERTS-CANCEL-LF                  CL**8
00897              ELSE                                                    CL**8
00898                  ADD +1          TO CERTS-EXPIRED-LF.                CL**8
00899                                                                      CL**8
00900      IF AX-AH-TYP NOT = ZERO                                         CL**8
00901          ADD +1                  TO TOTX-AH                          CL**8
00902          IF AX-AH-STATUS = '6'  OR  '7'  OR  '8'                     CL**8
00903              ADD +1              TO CERTS-CANCEL-AH                  CL**8
00904          ELSE                                                        CL**8
00905              ADD +1              TO CERTS-EXPIRED-AH.                CL**8
00906                                                                      CL**8
00907      COPY ELCCRTM2.                                                  CL**8
00908                                                                      CL**8
00909      WRITE CERT-PURGE  FROM CERTIFICATE-RECORD.                      CL**8
00910      ADD +1 TO CERTS-PURGE.                                          CL**8
00911                                                                      CL**8
00912      GO TO 0200-READ-LOOP.                                           CL**8
00913                                                                      CL**8
00914                                                                      CL**8
00915  EJECT                                                               CL**8
00916  0400-READ-ACCOUNT.                                                  CL**8
00917                                                                      CL**8
00918      IF ACCOUNT-MASTER = HIGH-VALUES                                 CL**8
00919          DISPLAY CR-CONTROL-1 ' NO MORE ACCOUNTS TO READ'            CL**8
00920          DISPLAY 'PROGRAM TERMINATING'                               CL**8
00921          STOP RUN.                                                   CL**8
00922                                                                      CL**8
00923  0410-READ-ACCOUNT-B.                                                CL**8
00924                                                                      CL**8
00925      READ ACC-MSTR                                                   CL**8
00926          AT END MOVE HIGH-VALUES TO ACCOUNT-MASTER.                  CL**8
00927                                                                      CL**8
00928      MOVE AM-MSTR-CNTRL          TO WS-ACC-EXP-CNTRL                 CL**8
00929                                     WS-ACC-EFF-CNTRL.                CL**8
00930                                                                      CL**8
00931      MOVE AM-EFFECT-DT           TO WS-ACC-EFF-DATE.                 CL**8
00932                                                                      CL**8
00933  0499-READ-EXIT.                                                     CL**8
00934      EXIT.                                                           CL**8
00935                                                                      CL**8
00936                                                                      CL**8
00937  0500-PRT-RTN.                                                       CL**8
00938                              COPY ELCPRT2.                           CL**8
00939  0599-PRT-EXIT.                                                      CL**8
00940      EXIT.                                                           CL**8
00941                                                                      CL**8
00942  EJECT                                                               CL**8
00943  0600-DATE-CONVERSION-ROUTINE.                                       CL**8
00944                                                                      CL**8
00945      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                      CL**8
00946                                                                      CL**8
00947  0699-EXIT.                                                          CL**8
00948      EXIT.                                                           CL**8
00949                                                                      CL**8
00950                                                                      CL**8
00951  0700-GET-REMAINING-TERM.                                            CL**8
00952                                                                      CL**8
00953      CALL 'ELRTRMX' USING CALCULATION-PASS-AREA.                     CL**8
00954                                                                      CL**8
00955  0799-REM-TERM-EXIT.                                                 CL**8
00956      EXIT.                                                           CL**8
00957                                                                      CL**8
00958                                                                      CL**8
00959  0800-WRITE-ALPHA-EXTRACT.                                           CL**8
00960                                                                      CL**8
00961      COPY ELCAEXM2.                                                  CL**8
00962                                                                      CL**8
00963      IF DTE-PRC-OPT = '1'                                            CL**8
00964          WRITE ALPHA-EXTRACT-RECORD FROM ALPHA-RECORD                CL**8
00965          ADD +1                      TO TOTAL-ALPHA.                 CL**8
00966                                                                      CL**8
00967  0899-WRITE-ALPHA-X.                                                 CL**8
00968      EXIT.                                                           CL**8
00969                                                                      CL**8
00970  EJECT                                                               CL**8
00971  0900-EOJ-FIRST.                                                     CL**8
00972                                                                      CL**8
00973      MOVE 1                       TO LINE-CT.                        CL**8
00974      MOVE TOTX-CERTS              TO DL-CERT-ORIG.                   CL**8
00975      MOVE CERTS-GOOD              TO DL-CERT-UPDT.                   CL**8
00976      MOVE CERTS-PURGE             TO DL-CERT-PURGE.                  CL**8
00977      MOVE TOTAL-ALPHA             TO DL-ALPHA-WRT.                   CL**8
00978      MOVE CERTS-CANCEL-LF         TO DL-CANCLED-LF.                  CL**8
00979      MOVE CERTS-CANCEL-AH         TO DL-CANCLED-AH.                  CL**8
00980      MOVE CERTS-DEATH-LF          TO DL-DEATH-LF.                    CL**8
00981      MOVE CERTS-DEATH-AH          TO DL-DEATH-AH.                    CL**8
00982      MOVE CERTS-EXPIRED-LF        TO DL-EXPIRED-LF.                  CL**8
00983      MOVE CERTS-EXPIRED-AH        TO DL-EXPIRED-AH.                  CL**8
00984      MOVE TOTX-LIFE               TO DL-TOTAL-LF.                    CL**8
00985      MOVE TOTX-AH                 TO DL-TOTAL-AH.                    CL**8
00986                                                                      CL**8
00987      MOVE TS                      TO X.                              CL**8
00988      MOVE DETAIL-LINE-1           TO PRT.                            CL**8
00989      PERFORM 0500-PRT-RTN         THRU 0599-PRT-EXIT.                CL**8
00990                                                                      CL**8
00991      MOVE SS                      TO X.                              CL**8
00992      MOVE DETAIL-LINE-2           TO PRT.                            CL**8
00993      PERFORM 0500-PRT-RTN         THRU 0599-PRT-EXIT.                CL**8
00994                                                                      CL**8
00995      MOVE DETAIL-LINE-3           TO PRT.                            CL**8
00996      PERFORM 0500-PRT-RTN         THRU 0599-PRT-EXIT.                CL**8
00997                                                                      CL**8
00998      MOVE DETAIL-LINE-4           TO PRT.                            CL**8
00999      PERFORM 0500-PRT-RTN         THRU 0599-PRT-EXIT.                CL**8
01000                                                                      CL**8
01001      MOVE TS                      TO X.                              CL**8
01002      MOVE DETAIL-LINE-5           TO PRT.                            CL**8
01003      PERFORM 0500-PRT-RTN         THRU 0599-PRT-EXIT.                CL**8
01004                                                                      CL**8
01005      MOVE SS                      TO X.                              CL**8
01006      MOVE DETAIL-LINE-6           TO PRT.                            CL**8
01007      PERFORM 0500-PRT-RTN         THRU 0599-PRT-EXIT.                CL**8
01008                                                                      CL**8
01009      MOVE DETAIL-LINE-7           TO PRT.                            CL**8
01010      PERFORM 0500-PRT-RTN         THRU 0599-PRT-EXIT.                CL**8
01011                                                                      CL**8
01012      MOVE DETAIL-LINE-8           TO PRT.                            CL**8
01013      PERFORM 0500-PRT-RTN         THRU 0599-PRT-EXIT.                CL**8
01014                                                                      CL**8
01015      MOVE DETAIL-LINE-9           TO PRT.                            CL**8
01016      PERFORM 0500-PRT-RTN         THRU 0599-PRT-EXIT.                CL**8
01017                                                                      CL**8
01018      MOVE DETAIL-LINE-10          TO PRT.                            CL**8
01019      PERFORM 0500-PRT-RTN         THRU 0599-PRT-EXIT.                CL**8
01020                                                                      CL**8
01021      MOVE TS                      TO X.                              CL**8
01022      MOVE SPACES                  TO PRT.                            CL**8
01023      PERFORM 0500-PRT-RTN         THRU 0599-PRT-EXIT.                CL**8
01024                                                                      CL**8
01025      MOVE FOOTNOTE-1              TO PRT.                            CL**8
01026      PERFORM 0500-PRT-RTN         THRU 0599-PRT-EXIT.                CL**8
01027                                                                      CL**8
01028      CLOSE CERTS-IN                                                  CL**8
01029            CERTS-OUT                                                 CL**8
01030            ACC-MSTR                                                  CL**8
01031            PURGE-CRT                                                 CL**8
01032            PRT-PURGE.                                                CL**8
01033                                                                      CL**8
01034      IF DTE-PRC-OPT = '1'                                            CL**8
01035          CLOSE ALPHA-EXTRACT.                                        CL**8
01036                                                                      CL**8
01037  9999-CLOSE-JOB.                                                     CL**8
01038                              COPY ELCPRTC.                           CL**8
01039      GOBACK.                                                         CL**8
01040                                                                      CL**8
01041  ABEND-PGM SECTION.                                                  CL**8
01042                              COPY ELCABEND.                          CL**8
01043 ******************************************************************   CL**8
