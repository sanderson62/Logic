00001  IDENTIFICATION DIVISION.                                         01/09/98
00002                                                                   EL527
00003  PROGRAM-ID.                 EL527 .                                 LV002
00004 *              PROGRAM CONVERTED BY                               EL527
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL527
00006 *              CONVERSION DATE 04/10/96 10:09:13.                 EL527
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL527
00008 *                            VMOD=2.007                           EL527
00009                                                                   EL527
00010 *AUTHOR.     LOGIC, INC.                                          EL527
00011 *            DALLAS, TEXAS.                                       EL527
00012                                                                   EL527
00013 *DATE-COMPILED.                                                   EL527
00014                                                                   EL527
00015                                                                   EL527
00016 *SECURITY.   *****************************************************EL527
00017 *            *                                                   *EL527
00018 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL527
00019 *            *                                                   *EL527
00020 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL527
00021 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL527
00022 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL527
00023 *            *                                                   *EL527
00024 *            *****************************************************EL527
00025                                                                   EL527
00026 *REMARKS.                                                         EL527
00027 *        THIS PROGRAM PRINTS A CHECK USAGE REPORT.                EL527
00028                                                                   EL527
00029      EJECT                                                        EL527
00030  ENVIRONMENT DIVISION.                                            EL527
00031                                                                   EL527
00032  INPUT-OUTPUT SECTION.                                            EL527
00033                                                                   EL527
00034  FILE-CONTROL.                                                    EL527
00035                                                                   EL527
00036      SELECT DISK-DATE        ASSIGN TO SYS019-FBA1-S-SYS019.      EL527
00037                                                                   EL527
00038      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   EL527
00039                                                                   EL527
00040      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   EL527
00041                                                                   EL527
00042      SELECT ELCNTL           ASSIGN TO SYS022-FBA1-ELCNTL         EL527
00043                              ORGANIZATION IS INDEXED              EL527
00044                              ACCESS IS DYNAMIC                    EL527
00045                              RECORD KEY IS CF-CONTROL-PRIMARY     EL527
00046                              FILE STATUS IS ELCNTL-FILE-STATUS.   EL527
00047                                                                   EL527
00048      SELECT ERCHKQ           ASSIGN TO SYS018-FBA1-ERCHKQ         EL527
00049                              ORGANIZATION IS INDEXED              EL527
00050                              ACCESS IS DYNAMIC                    EL527
00051                              RECORD KEY IS CQ-CONTROL-PRIMARY     EL527
00052                              FILE STATUS IS ERCHKQ-FILE-STATUS.   EL527
00053                                                                   EL527
00054      SELECT ELREPT           ASSIGN TO SYS024-FBA1-ELREPT         EL527
00055                              ORGANIZATION IS INDEXED              EL527
00056                              ACCESS IS DYNAMIC                    EL527
00057                              RECORD KEY IS RF-CONTROL-PRIMARY     EL527
00058                              FILE STATUS IS DTE-VSAM-FLAGS.       EL527
00059                                                                   EL527
00060      SELECT SORT-FILE        ASSIGN TO SYS001-FBA1-S-SORTWK1.     EL527
00061                                                                   EL527
00062      EJECT                                                        EL527
00063  DATA DIVISION.                                                   EL527
00064                                                                   EL527
00065  FILE SECTION.                                                    EL527
00066                                                                   EL527
00067  FD  DISK-DATE               COPY ELCDTEFD.                       EL527
00068                                                                   EL527
00069  FD  PRNTR                   COPY ELCPRTFD.                       EL527
00070                                                                   EL527
00071  FD  FICH                    COPY ELCFCHFD.                       EL527
00072                                                                   EL527
00073  FD  ELCNTL.                                                      EL527
00074                                                                   EL527
00075                                  COPY ELCCNTL.                    EL527
00076                                                                   EL527
00077      EJECT                                                        EL527
00078  FD  ERCHKQ.                                                      EL527
00079                                                                   EL527
00080                                  COPY ERCCHKQ.                    EL527
00081                                                                   EL527
00082      EJECT                                                        EL527
00083                                                                   EL527
00084  FD  ELREPT                      COPY ELCRPTFD.                   EL527
00085                                                                   EL527
00086                                  COPY ELCREPT.                    EL527
00087                                                                   EL527
00088      EJECT                                                        EL527
00089  SD  SORT-FILE.                                                   EL527
00090                                                                   EL527
00091  01  SORT-RECORD.                                                 EL527
00092      05  SR-CONTROL-PRIMARY.                                      EL527
00093          10  SR-CARRIER               PIC X.                      EL527
00094          10  SR-CONTROL-NUMBER        PIC S9(8)           COMP.   EL527
00095          10  SR-CHECK-NO              PIC X(7).                   EL527
00096      05  SR-CK-RECORD.                                            EL527
00097          10  FILLER                   PIC X(3).                   EL527
00098          10  SR-CK-CONTROL-NUMBER     PIC S9(8)           COMP.   EL527
00099          10  SR-CK-SEQUENCE-NUMBER    PIC S9(4)           COMP.   EL527
00100          10  SR-CK-ENTRY-TYPE         PIC X.                      EL527
00101              88  SR-CHECK-ON-QUE                 VALUE 'Q'.       EL527
00102              88  SR-ALIGNMENT-CHECK              VALUE 'A'.       EL527
00103              88  SR-MANUAL-CHECK                 VALUE 'M'.       EL527
00104              88  SR-SPOILED-CHECK                VALUE 'S'.       EL527
00105              88  SR-VOIDED-CHECK                 VALUE 'V'.       EL527
00106              88  SR-PAYMENT-ABORTED              VALUE 'X'.       EL527
00107          10  SR-CK-PYAJ-CONTROL.                                  EL527
00108              15  SR-CK-PYAJ-CARRIER   PIC X.                      EL527
00109              15  SR-CK-PYAJ-GROUP     PIC X(6).                   EL527
00110              15  SR-CK-PYAJ-FIN-RESP  PIC X(10).                  EL527
00111              15  SR-CK-PYAJ-ACCOUNT   PIC X(10).                  EL527
00112              15  SR-CK-PYAJ-SEQ       PIC S9(8)           COMP.   EL527
00113              15  SR-CK-PYAJ-REC-TYPE  PIC X.                      EL527
00114              15  FILLER               PIC X(18).                  EL527
00115          10  SR-CK-CHEK-CONTROL       REDEFINES                   EL527
00116              SR-CK-PYAJ-CONTROL.                                  EL527
00117              15  SR-CK-CHEK-CARRIER   PIC X.                      EL527
00118              15  SR-CK-CHEK-GROUP     PIC X(6).                   EL527
00119              15  SR-CK-CHEK-STATE     PIC XX.                     EL527
00120              15  SR-CK-CHEK-ACCOUNT   PIC X(10).                  EL527
00121              15  SR-CK-CHEK-EFF-DT    PIC XX.                     EL527
00122              15  SR-CK-CHEK-CERT-NO   PIC X(11).                  EL527
00123              15  SR-CK-CHEK-SEQ-NO    PIC S9(4)           COMP.   EL527
00124              15  SR-CK-CHEK-FIN-RESP  PIC X(10).                  EL527
00125              15  FILLER               PIC X(6).                   EL527
00126          10  SR-CK-CHECK-NUMBER       PIC X(7).                   EL527
00127          10  SR-CK-AMOUNT             PIC S9(7)V99        COMP-3. EL527
00128          10  SR-CK-PAYMENT-TYPE       PIC X.                      EL527
00129              88  BILLING-CREDIT                  VALUE '1'.       EL527
00130              88  REFUND-PMT                      VALUE '2'.       EL527
00131              88  CHECK-MAINT-PMT                 VALUE '3'.       EL527
00132          10  SR-CK-VOID-INDICATOR     PIC X.                      EL527
00133              88  CHECK-IS-VOIDED                 VALUE 'V'.       EL527
00134          10  SR-CK-TIMES-PRINTED      PIC S9(4)           COMP.   EL527
00135          10  SR-CK-PRINT-AT-HHMM      PIC S9(4)           COMP.   EL527
00136          10  SR-CK-CHECK-BY-USER      PIC X(4).                   EL527
00137          10  SR-CK-PRE-NUMBERING-SW   PIC X.                      EL527
00138          10  SR-CK-WRITTEN-DT         PIC XX.                     EL527
00139          10  SR-CK-LAST-UPDATED-BY    PIC S9(4)           COMP.   EL527
00140          10  SR-CK-ACCOUNT-AGENT      PIC X(10).                  EL527
00141          10  FILLER                   PIC X(3).                   EL527
00142                                                                   EL527
00143      EJECT                                                        EL527
00144  WORKING-STORAGE SECTION.                                         EL527
00145  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL527
00146  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.   EL527
00147  01  LCP-TIME-OF-DAY-68            PIC 9(6).                      EL527
00148  01  LCP-TIME-OF-DAY-74.                                          EL527
00149      05  LCP-TIME-74               PIC 9(6).                      EL527
00150      05  FILLER                    PIC 9(2).                      EL527
00151                                                                   EL527
00152  77  FILLER  PIC X(32)   VALUE '********************************'.EL527
00153  77  FILLER  PIC X(32)   VALUE '*     EL527  WORKING STORAGE   *'.EL527
00154  77  FILLER  PIC X(32)   VALUE '******** VMOD=2.007 ************'.EL527
00155                                                                   EL527
00156  01  FILLER                          COMP-3.                      EL527
00157      05  WS-LINE-COUNT               PIC S9(3)   VALUE +99.       EL527
00158      05  WS-LINE-COUNT-MAX           PIC S9(3)   VALUE +60.       EL527
00159      05  WS-PAGE                     PIC S9(5)   VALUE ZERO.      EL527
00160      05  WS-REPORT-SW                PIC S9      VALUE ZERO.      EL527
00161      05  WS-HEADING-SW               PIC S9      VALUE ZERO.      EL527
00162      05  WS-PRINT-SW                 PIC S9      VALUE ZERO.      EL527
00163      05  WS-RECORD-COUNT             PIC S9(9)   VALUE ZERO.      EL527
00164      05  WS-RETURN-CODE              PIC S9(3)   VALUE ZERO.      EL527
00165      05  WS-ZERO                     PIC S9      VALUE ZERO.      EL527
00166      05  WS-NO-RECORDS-RELEASED      PIC S9(5)   VALUE ZERO.      EL527
00167                                                                   EL527
00168      05  WS-INCURRED-AGE             PIC S9(3)   VALUE ZERO.      EL527
00169      05  WS-YEAR                     REDEFINES                    EL527
00170          WS-INCURRED-AGE             PIC S9(3).                   EL527
00171                                                                   EL527
00172      05  WS-AMOUNT                   PIC S9(7)V99 VALUE ZERO.     EL527
00173                                                                   EL527
00174      EJECT                                                        EL527
00175  01  FILLER                          COMP SYNC.                   EL527
00176      05  PGM-SUB                     PIC S9(4)   VALUE +527.      EL527
00177      05  WS-INDEX                    PIC S9(4)   VALUE ZERO.      EL527
00178      05  WS-LENGTH                   REDEFINES                    EL527
00179          WS-INDEX                    PIC S9(4).                   EL527
00180                                                                   EL527
00181  01  FILLER.                                                      EL527
00182      05  WS-DISPLAY-TIME             PIC 99B99B99.                EL527
00183      05  ABEND-CODE                  PIC X(4).                    EL527
00184      05  ABEND-OPTION                PIC X.                       EL527
00185      05  OLC-REPORT-NAME             PIC X(5) VALUE 'EL527'.      EL527
00186      05  X                           PIC X       VALUE SPACE.     EL527
00187                                                                   EL527
00188      05  POS                         PIC X(3)    VALUE 'POS'.     EL527
00189      05  WSL                         PIC X(3)    VALUE 'WSL'.     EL527
00190                                                                   EL527
00191      05  WS-SAVE-PRINT-RECORD        PIC X(133)  VALUE SPACES.    EL527
00192                                                                   EL527
00193      05  WS-LAST-CONTROL             PIC S9(8)   VALUE +0  COMP.  EL527
00194      05  WS-LAST-CARRIER             PIC X       VALUE SPACES.    EL527
00195                                                                   EL527
00196      05  WS-ABEND-MESSAGE            PIC X(80)   VALUE SPACES.    EL527
00197                                                                   EL527
00198      05  WS-LAST-MONTH               PIC 99      VALUE ZERO.      EL527
00199      05  WS-LAST-MONTH-X             REDEFINES                    EL527
00200          WS-LAST-MONTH               PIC XX.                      EL527
00201                                                                   EL527
00202      05  WS-MONTH                    PIC XX      VALUE ZERO.      EL527
00203      05  WS-BIN-RUN-DATE             PIC XX      VALUE LOW-VALUES.EL527
00204                                                                   EL527
00205      05  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZERO.      EL527
00206      05  ELCNTL-FILE-STATUS          PIC XX      VALUE ZERO.      EL527
00207      05  ERCHKQ-FILE-STATUS          PIC XX      VALUE ZERO.      EL527
00208                                                                   EL527
00209      05  WS-FILE-ERROR-MESSAGE.                                   EL527
00210          10  FILLER                  PIC X(24)   VALUE            EL527
00211              'ERROR OCCURED OPENING - '.                          EL527
00212          10  WS-FEM-FILE-NAME        PIC X(8).                    EL527
00213                                                                   EL527
00214      05  WS-COMPANY-ID               PIC X(3).                    EL527
00215      05  WS-COMPANY-CD               PIC X.                       EL527
00216                                                                   EL527
00217      05  WS-COMPANY-NAME.                                         EL527
00218          10  WS-CN-CHAR              PIC X                        EL527
00219              OCCURS 30 TIMES         INDEXED BY CN1.              EL527
00220                                                                   EL527
00221      05  WS-COMPANY-NAME2.                                        EL527
00222          10  WS-CN2-CHAR             PIC X                        EL527
00223              OCCURS 30 TIMES         INDEXED BY CN2.              EL527
00224      05  WS-INITIALS.                                             EL527
00225          10  WS-INITIAL1             PIC X.                       EL527
00226          10  WS-INITIAL2             PIC X.                       EL527
00227                                                                   EL527
00228      05  WS-PHONETIC-WORK-AREA.                                   EL527
00229          10  WS-PWA-PHONETIC-NAME    PIC X(4).                    EL527
00230          10  WS-PWA-NAME             PIC X(16).                   EL527
00231          10  WS-PWA-LANGUAGE         PIC X.                       EL527
00232                                                                   EL527
00233      05  WS-BIN-DATE-WORK-X.                                      EL527
00234          10  WS-BIN-DATE-WORK        PIC S9(4)                    EL527
00235                                      COMP.                        EL527
00236                                                                   EL527
00237      05  WS-DATE-WORK.                                            EL527
00238          10  WS-DW-MONTH             PIC 99.                      EL527
00239          10  FILLER                  PIC X.                       EL527
00240          10  WS-DW-DAY               PIC 99.                      EL527
00241          10  FILLER                  PIC X.                       EL527
00242          10  WS-DW-YEAR              PIC 99.                      EL527
00243                                                                   EL527
00244      05  WS-DATE-PAID.                                            EL527
00245          10  WS-MONTH-PAID           PIC 99.                      EL527
00246          10  FILLER                  PIC X.                       EL527
00247          10  WS-DAY-PAID             PIC 99.                      EL527
00248          10  FILLER                  PIC X.                       EL527
00249          10  WS-YEAR-PAID            PIC 99.                      EL527
00250                                                                   EL527
00251      05  WS-CONTROL-DESC             PIC X(30)     VALUE          EL527
00252          ' ** TOTAL CONTROL GROUP **'.                            EL527
00253      05  WS-TOT-CONTROL-AMT          PIC S9(9)V99  VALUE +0.      EL527
00254                                                                   EL527
00255      05  WS-CARRIER-DESC             PIC X(30)     VALUE          EL527
00256          ' ** TOTAL CARRIER **'.                                  EL527
00257      05  WS-TOT-CARRIER-AMT          PIC S9(9)V99  VALUE +0.      EL527
00258                                                                   EL527
00259      05  WS-TOTAL-DESC               PIC X(30)     VALUE          EL527
00260          ' ** TOTAL CHECKS **'.                                   EL527
00261      05  WS-TOT-CHECKS-AMT           PIC S9(9)V99  VALUE +0.      EL527
00262                                                                   EL527
00263      EJECT                                                        EL527
00264                                                                   EL527
00265  01  WS-HEADING1.                                                 EL527
00266      05  FILLER                      PIC X(51)       VALUE '1'.   EL527
00267      05  WS-H1-TITLE                 PIC X(21)       VALUE        EL527
00268          'CHECK USAGE REPORT - '.                                 EL527
00269      05  WS-H1-PERIOD                PIC X(48)       VALUE        EL527
00270          'MONTHLY'.                                               EL527
00271      05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL527  '.    EL527
00272                                                                   EL527
00273  01  WS-HEADING2.                                                 EL527
00274      05  FILLER                      PIC X(46)       VALUE SPACES.EL527
00275      05  WS-H2-CLIENT-NAME           PIC X(74)       VALUE SPACES.EL527
00276      05  WS-H2-DATE                  PIC X(8)        VALUE SPACES.EL527
00277      05  FILLER                      PIC X           VALUE SPACES.EL527
00278                                                                   EL527
00279  01  WS-HEADING3.                                                 EL527
00280      05  FILLER                      PIC X(51)       VALUE SPACES.EL527
00281      05  WS-H3-DATE                  PIC X(60)       VALUE SPACES.EL527
00282      05  FILLER                      PIC X(5)        VALUE 'PAGE'.EL527
00283      05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL527
00284      05  FILLER                      PIC X(11)       VALUE SPACES.EL527
00285                                                                   EL527
00286  01  WS-HEADING4.                                                 EL527
00287      05  FILLER                      PIC X(28)       VALUE        EL527
00288          '0 CHECK'.                                               EL527
00289      05  FILLER                      PIC X(34)       VALUE        EL527
00290          ' DATE   CONTROL'.                                       EL527
00291      05  FILLER                      PIC X(62)       VALUE        EL527
00292          'TIMES  PAYMENT             FIN.RESP'.                   EL527
00293                                                                   EL527
00294  01  WS-HEADING5.                                                 EL527
00295      05  FILLER                      PIC X(54)       VALUE        EL527
00296          ' NUMBER  CAR     AMOUNT      PAID    GROUP    USED FOR'.EL527
00297      05  FILLER                      PIC X(7)        VALUE SPACE. EL527
00298      05  FILLER                      PIC X(41)       VALUE        EL527
00299          'PRINTED   BY     GROUP      OR STATE     '.             EL527
00300      05  FILLER                      PIC X(31)       VALUE        EL527
00301          'ACCOUNT    CERT NO.'.                                   EL527
00302                                                                   EL527
00303      EJECT                                                        EL527
00304  01  WS-DETAIL1.                                                  EL527
00305      05  FILLER                      PIC X.                       EL527
00306      05  WS-D1-CHECK-NUMBER          PIC X(7).                    EL527
00307      05  FILLER                      PIC XX.                      EL527
00308      05  WS-D1-CARRIER               PIC X.                       EL527
00309      05  FILLER                      PIC XX.                      EL527
00310      05  WS-D1-AMOUNT                PIC Z,ZZZ,ZZ9.99-.           EL527
00311      05  WS-D1-ASTERISK-AMT  REDEFINES  WS-D1-AMOUNT              EL527
00312                                      PIC X(13).                   EL527
00313      05  FILLER                      PIC X.                       EL527
00314      05  WS-D1-DATE-PAID             PIC X(8).                    EL527
00315      05  FILLER                      PIC X.                       EL527
00316      05  WS-D1-CONTROL-GROUP         PIC 9(8).                    EL527
00317      05  FILLER                      PIC XX.                      EL527
00318      05  WS-D1-USED-FOR              PIC X(18).                   EL527
00319      05  FILLER                      PIC X.                       EL527
00320      05  WS-D1-TIMES-PRINTED         PIC 9(01).                   EL527
00321      05  FILLER                      PIC X(5).                    EL527
00322      05  WS-D1-PAYMENT-BY            PIC X(4).                    EL527
00323      05  FILLER                      PIC X(3).                    EL527
00324      05  WS-D1-GROUPING              PIC X(6).                    EL527
00325      05  FILLER                      PIC X(3).                    EL527
00326      05  WS-D1-FIN-RESP              PIC X(10).                   EL527
00327                                                                   EL527
00328      05  FILLER                      REDEFINES                    EL527
00329          WS-D1-FIN-RESP.                                          EL527
00330          10  FILLER                  PIC X(6).                    EL527
00331          10  WS-D1-STATE             PIC XX.                      EL527
00332          10  FILLER                  PIC XX.                      EL527
00333                                                                   EL527
00334      05  FILLER                      PIC X(3).                    EL527
00335      05  WS-D1-ACCOUNT               PIC X(10).                   EL527
00336      05  FILLER                      PIC XX.                      EL527
00337      05  WS-D1-MESSAGE               PIC X(15).                   EL527
00338      05  FILLER                      PIC X(6).                    EL527
00339                                                                   EL527
00340      EJECT                                                        EL527
00341  01  WS-DETAIL2                      REDEFINES                    EL527
00342      WS-DETAIL1.                                                  EL527
00343      05  FILLER                      PIC X(23).                   EL527
00344      05  WS-D2-START-CHECK-NO        PIC X(7).                    EL527
00345      05  FILLER                      PIC X(6).                    EL527
00346      05  WS-D2-END-CHECK-NO          PIC X(7).                    EL527
00347      05  FILLER                      PIC X(90).                   EL527
00348                                                                   EL527
00349      EJECT                                                        EL527
00350  01  WS-TOTAL-LINE1                  REDEFINES                    EL527
00351      WS-DETAIL1.                                                  EL527
00352      05  FILLER                      PIC X(7).                    EL527
00353      05  WS-T1-AMOUNT                PIC ZZZ,ZZZ,ZZ9.99-.         EL527
00354      05  FILLER                      PIC X(1).                    EL527
00355      05  WS-T1-DESCRIPTION           PIC X(34).                   EL527
00356      05  FILLER                      PIC X(76).                   EL527
00357                                                                   EL527
00358      EJECT                                                        EL527
00359                           COPY ELCDATE.                           EL527
00360                                                                   EL527
00361                           COPY ELCDTECX.                          EL527
00362                                                                   EL527
00363                           COPY ELCDTEVR.                          EL527
00364                                                                   EL527
00365      EJECT                                                        EL527
00366  PROCEDURE DIVISION.                                              EL527
00367                                                                   EL527
00368  0000-DATE-CARD-READ SECTION. COPY ELCDTERX SUPPRESS.             EL527
00369                                                                   EL527
00370  1000-MAIN-LOGIC SECTION.                                         EL527
00371      PERFORM OPEN-FILES.                                          EL527
00372                                                                   EL527
00373      SORT SORT-FILE                                               EL527
00374          ON ASCENDING KEY SR-CONTROL-PRIMARY                      EL527
00375              INPUT  PROCEDURE 2000-SORT-INPUT-PROCEDURE           EL527
00376              OUTPUT PROCEDURE 3000-SORT-OUTPUT-PROCEDURE.         EL527
00377                                                                   EL527
00378      IF SORT-RETURN GREATER THAN ZERO                             EL527
00379          MOVE 'SORT FAILED'      TO  WS-ABEND-MESSAGE             EL527
00380          MOVE SORT-RETURN        TO  WS-RETURN-CODE               EL527
00381          PERFORM ABEND-PGM.                                       EL527
00382                                                                   EL527
00383      PERFORM CLOSE-FILES.                                         EL527
00384                                                                   EL527
00385      GOBACK.                                                      EL527
00386                                                                   EL527
00387  1099-EXIT.                                                       EL527
00388      EXIT.                                                        EL527
00389                                                                   EL527
00390      EJECT                                                        EL527
00391                                                                   EL527
00392  2000-SORT-INPUT-PROCEDURE SECTION.                               EL527
00393                                                                   EL527
00394      IF DTE-PRC-OPT = '2'                                         EL527
00395          MOVE 'DAILY'            TO WS-H1-PERIOD.                 EL527
00396      IF DTE-PRC-OPT = '3'                                         EL527
00397          MOVE 'YEAR-TO-DATE'     TO WS-H1-PERIOD.                 EL527
00398      IF DTE-PRC-OPT = '4'                                         EL527
00399          MOVE 'INCEPTION-TO-DATE' TO WS-H1-PERIOD.                EL527
00400                                                                   EL527
00401      MOVE BIN-RUN-DATE           TO  WS-BIN-RUN-DATE.             EL527
00402                                                                   EL527
00403      MOVE LOW-VALUES             TO  CF-CONTROL-PRIMARY.          EL527
00404      MOVE DTE-CLIENT             TO  CF-COMPANY-ID.               EL527
00405      MOVE '1'                    TO  CF-RECORD-TYPE.              EL527
00406      MOVE SPACES                 TO  CF-ACCESS-CD-GENL.           EL527
00407      MOVE +0                     TO  CF-SEQUENCE-NO.              EL527
00408                                                                   EL527
00409      READ ELCNTL.                                                 EL527
00410                                                                   EL527
00411      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL527
00412          MOVE 'ERROR OCCURED READ INITIAL - ELCNTL'               EL527
00413                                  TO  WS-ABEND-MESSAGE             EL527
00414          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL527
00415          PERFORM ABEND-PGM.                                       EL527
00416                                                                   EL527
00417      MOVE CF-CL-MAIL-TO-NAME     TO  WS-COMPANY-NAME              EL527
00418                                      WS-H2-CLIENT-NAME.           EL527
00419      MOVE CF-COMPANY-ID          TO  WS-COMPANY-ID.               EL527
00420      MOVE CF-COMPANY-CD          TO  WS-COMPANY-CD.               EL527
00421      MOVE LOW-VALUES             TO  WS-LAST-CARRIER.             EL527
00422      ACCEPT LCP-TIME-OF-DAY-74 FROM TIME                          EL527
00423      MOVE LCP-TIME-74 TO LCP-TIME-OF-DAY-68                       EL527
00424                                                                   EL527
00425      MOVE  LCP-TIME-OF-DAY-68 TO WS-DISPLAY-TIME.                 EL527
00426      INSPECT WS-DISPLAY-TIME CONVERTING SPACES TO '.'.            EL527
00427      DISPLAY 'BEGIN PROCESSING ' WS-H2-CLIENT-NAME ' AT '         EL527
00428              WS-DISPLAY-TIME UPON CONSOLE.                        EL527
00429                                                                   EL527
00430      MOVE SPACES                 TO  WS-COMPANY-NAME2.            EL527
00431      SET CN1 TO +30.                                              EL527
00432                                                                   EL527
00433  2020-SIP.                                                        EL527
00434      IF WS-CN-CHAR (CN1) = SPACES                                 EL527
00435          IF CN1 GREATER THAN +1                                   EL527
00436              SET CN1 DOWN BY +1                                   EL527
00437              GO TO 2020-SIP                                       EL527
00438          ELSE                                                     EL527
00439              GO TO 2100-SIP.                                      EL527
00440                                                                   EL527
00441      SET WS-LENGTH TO CN1.                                        EL527
00442                                                                   EL527
00443      SUBTRACT WS-LENGTH FROM +30 GIVING WS-LENGTH.                EL527
00444      DIVIDE +2 INTO WS-LENGTH ROUNDED.                            EL527
00445                                                                   EL527
00446      IF WS-LENGTH NOT GREATER THAN ZERO                           EL527
00447          GO TO 2100-SIP.                                          EL527
00448                                                                   EL527
00449      SET CN2 TO CN1.                                              EL527
00450      SET CN2 UP BY WS-LENGTH.                                     EL527
00451                                                                   EL527
00452  2030-SIP.                                                        EL527
00453      MOVE WS-CN-CHAR (CN1) TO WS-CN2-CHAR (CN2).                  EL527
00454                                                                   EL527
00455      IF CN1 GREATER THAN +1                                       EL527
00456          SET CN1                                                  EL527
00457              CN2 DOWN BY +1                                       EL527
00458          GO TO 2030-SIP.                                          EL527
00459                                                                   EL527
00460      MOVE WS-COMPANY-NAME2       TO  WS-COMPANY-NAME.             EL527
00461                                                                   EL527
00462      EJECT                                                        EL527
00463                                                                   EL527
00464  2100-SIP.                                                        EL527
00465      MOVE LOW-VALUES             TO  CQ-CONTROL-PRIMARY.          EL527
00466      MOVE CF-COMPANY-CD          TO  CQ-COMPANY-CD.               EL527
00467                                                                   EL527
00468      START ERCHKQ                                                 EL527
00469          KEY IS GREATER THAN CQ-CONTROL-PRIMARY.                  EL527
00470                                                                   EL527
00471      IF ERCHKQ-FILE-STATUS NOT = ZERO                             EL527
00472          MOVE 'ERROR OCCURED START - ERCHKQ'                      EL527
00473                                  TO  WS-ABEND-MESSAGE             EL527
00474          MOVE ERCHKQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL527
00475          PERFORM ABEND-PGM.                                       EL527
00476                                                                   EL527
00477      EJECT                                                        EL527
00478                                                                   EL527
00479  2150-SIP.                                                        EL527
00480      READ ERCHKQ NEXT RECORD.                                     EL527
00481                                                                   EL527
00482      IF ERCHKQ-FILE-STATUS = '10'                                 EL527
00483          GO TO 2190-EXIT.                                         EL527
00484                                                                   EL527
00485      IF ERCHKQ-FILE-STATUS NOT = ZERO                             EL527
00486          MOVE 'ERROR OCCURED READNEXT - ERCHKQ'                   EL527
00487                                  TO  WS-ABEND-MESSAGE             EL527
00488          MOVE ERCHKQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL527
00489          PERFORM ABEND-PGM.                                       EL527
00490                                                                   EL527
00491      IF CF-COMPANY-CD NOT = CQ-COMPANY-CD                         EL527
00492          GO TO 2190-EXIT.                                         EL527
00493                                                                   EL527
00494      IF CQ-TIMES-PRINTED = ZEROS                                  EL527
00495          GO TO 2150-SIP.                                          EL527
00496                                                                   EL527
00497      IF CQ-CHECK-WRITTEN-DT GREATER THAN WS-BIN-RUN-DATE          EL527
00498          GO TO 2150-SIP.                                          EL527
00499                                                                   EL527
00500      IF CQ-CHECK-VOIDED-DT GREATER THAN WS-BIN-RUN-DATE           EL527
00501          MOVE LOW-VALUES         TO CQ-CHECK-VOIDED-DT            EL527
00502          MOVE 'Q'                TO CQ-ENTRY-TYPE                 EL527
00503          MOVE SPACE              TO CQ-VOID-INDICATOR.            EL527
00504                                                                   EL527
00505      IF VOIDED-CHECK  AND                                         EL527
00506         CQ-CHECK-VOIDED-DT GREATER THAN CQ-CHECK-WRITTEN-DT       EL527
00507          MOVE CQ-CHECK-VOIDED-DT TO  DC-BIN-DATE-1                EL527
00508      ELSE                                                         EL527
00509          MOVE CQ-CHECK-WRITTEN-DT TO  DC-BIN-DATE-1.              EL527
00510      MOVE SPACES                 TO  DC-OPTION-CODE.              EL527
00511      PERFORM 8500-DATE-CONVERSION.                                EL527
00512      MOVE DC-GREG-DATE-1-EDIT    TO  WS-DATE-WORK.                EL527
00513                                                                   EL527
00514      IF  DTE-PRC-OPT = '4'                                        EL527
00515            GO TO 2160-SIP.                                        EL527
00516                                                                   EL527
00517      IF  DTE-PRC-OPT = '3'                                        EL527
00518        AND WS-DW-YEAR  = RUN-YR                                   EL527
00519            GO TO 2160-SIP.                                        EL527
00520                                                                   EL527
00521      IF  DTE-PRC-OPT = '2'                                        EL527
00522        AND WS-DW-MONTH = RUN-MO                                   EL527
00523        AND WS-DW-DAY   = RUN-DA                                   EL527
00524        AND WS-DW-YEAR  = RUN-YR                                   EL527
00525            GO TO 2160-SIP.                                        EL527
00526                                                                   EL527
00527      IF  DTE-PRC-OPT = '1'                                        EL527
00528        AND WS-DW-MONTH = RUN-MO                                   EL527
00529        AND WS-DW-YEAR = RUN-YR                                    EL527
00530          GO TO 2160-SIP.                                          EL527
00531                                                                   EL527
00532      GO TO 2150-SIP.                                              EL527
00533                                                                   EL527
00534  2160-SIP.                                                        EL527
00535      MOVE CQ-CHEK-CARRIER        TO  SR-CARRIER.                  EL527
00536      MOVE CQ-CONTROL-NUMBER      TO  SR-CONTROL-NUMBER.           EL527
00537      MOVE CQ-CHECK-NUMBER        TO  SR-CHECK-NO.                 EL527
00538      MOVE CHECK-QUE              TO  SR-CK-RECORD.                EL527
00539                                                                   EL527
00540      RELEASE SORT-RECORD.                                         EL527
00541                                                                   EL527
00542      ADD +1                      TO  WS-NO-RECORDS-RELEASED.      EL527
00543                                                                   EL527
00544      IF NOT CHECK-IS-VOIDED                                       EL527
00545          GO TO 2150-SIP.                                          EL527
00546                                                                   EL527
00547      MOVE SR-CK-WRITTEN-DT       TO  DC-BIN-DATE-1.               EL527
00548      MOVE SPACES                 TO  DC-OPTION-CODE.              EL527
00549      PERFORM 8500-DATE-CONVERSION.                                EL527
00550      MOVE DC-GREG-DATE-1-EDIT    TO  WS-DATE-WORK.                EL527
00551                                                                   EL527
00552      IF  DTE-PRC-OPT   = '1'                                      EL527
00553        IF WS-DW-MONTH = RUN-MO  AND                               EL527
00554           WS-DW-YEAR = RUN-YR                                     EL527
00555            NEXT SENTENCE                                          EL527
00556        ELSE                                                       EL527
00557            GO TO 2150-SIP.                                        EL527
00558                                                                   EL527
00559      IF  DTE-PRC-OPT   = '2'                                      EL527
00560        IF WS-DW-MONTH = RUN-MO  AND                               EL527
00561           WS-DW-DAY   = RUN-DA  AND                               EL527
00562           WS-DW-YEAR  = RUN-YR                                    EL527
00563            NEXT SENTENCE                                          EL527
00564        ELSE                                                       EL527
00565            GO TO 2150-SIP.                                        EL527
00566                                                                   EL527
00567      IF  DTE-PRC-OPT   = '3'                                      EL527
00568        IF WS-DW-YEAR  = RUN-YR                                    EL527
00569            NEXT SENTENCE                                          EL527
00570        ELSE                                                       EL527
00571            GO TO 2150-SIP.                                        EL527
00572                                                                   EL527
00573      MOVE 'Q'                    TO SR-CK-ENTRY-TYPE.             EL527
00574      MOVE SPACE                  TO SR-CK-VOID-INDICATOR.         EL527
00575                                                                   EL527
00576      RELEASE SORT-RECORD.                                         EL527
00577                                                                   EL527
00578      ADD +1                      TO  WS-NO-RECORDS-RELEASED.      EL527
00579                                                                   EL527
00580      GO TO 2150-SIP.                                              EL527
00581                                                                   EL527
00582  2190-EXIT.                                                       EL527
00583      EXIT.                                                        EL527
00584      EJECT                                                        EL527
00585                                                                   EL527
00586  3000-SORT-OUTPUT-PROCEDURE SECTION.                              EL527
00587                                                                   EL527
00588      IF WS-NO-RECORDS-RELEASED = ZEROS                            EL527
00589          DISPLAY '*** EL527  NO CHECKS ON FILE FOR - '            EL527
00590              RUN-MO '-' RUN-YR                                    EL527
00591          GO TO 3190-EXIT.                                         EL527
00592                                                                   EL527
00593  3100-SOP.                                                        EL527
00594      RETURN SORT-FILE                                             EL527
00595          AT END                                                   EL527
00596              GO TO 3180-SOP.                                      EL527
00597                                                                   EL527
00598      ADD +1                      TO  WS-RECORD-COUNT.             EL527
00599                                                                   EL527
00600      IF LCP-ONCTR-01 =  0                                         EL527
00601          ADD 1 TO LCP-ONCTR-01                                    EL527
00602          MOVE SR-CARRIER         TO WS-LAST-CARRIER               EL527
00603          MOVE SR-CONTROL-NUMBER  TO WS-LAST-CONTROL.              EL527
00604                                                                   EL527
00605      IF SR-CARRIER NOT = WS-LAST-CARRIER                          EL527
00606          MOVE WS-CONTROL-DESC    TO  WS-T1-DESCRIPTION            EL527
00607          MOVE WS-TOT-CONTROL-AMT TO  WS-T1-AMOUNT                 EL527
00608          MOVE ZEROS              TO  WS-TOT-CONTROL-AMT           EL527
00609          MOVE WS-DETAIL1         TO  PRT                          EL527
00610          PERFORM WRITE-A-LINE                                     EL527
00611          MOVE SR-CONTROL-NUMBER  TO  WS-LAST-CONTROL              EL527
00612          MOVE WS-CARRIER-DESC    TO  WS-T1-DESCRIPTION            EL527
00613          MOVE WS-TOT-CARRIER-AMT TO  WS-T1-AMOUNT                 EL527
00614          MOVE ZEROS              TO  WS-TOT-CARRIER-AMT           EL527
00615          MOVE WS-DETAIL1         TO  PRT                          EL527
00616          MOVE '0'                TO  P-CTL                           CL**2
00617          PERFORM WRITE-A-LINE                                     EL527
00618          MOVE SR-CARRIER         TO  WS-LAST-CARRIER              EL527
00619          ADD WS-LINE-COUNT-MAX   TO  WS-LINE-COUNT.               EL527
00620                                                                   EL527
00621      IF SR-CONTROL-NUMBER NOT = WS-LAST-CONTROL                   EL527
00622          MOVE WS-CONTROL-DESC    TO  WS-T1-DESCRIPTION            EL527
00623          MOVE WS-TOT-CONTROL-AMT TO  WS-T1-AMOUNT                 EL527
00624          MOVE ZEROS              TO  WS-TOT-CONTROL-AMT           EL527
00625          MOVE WS-DETAIL1         TO  PRT                          EL527
00626          PERFORM WRITE-A-LINE                                     EL527
00627          MOVE SR-CONTROL-NUMBER  TO  WS-LAST-CONTROL              EL527
00628          MOVE SPACES             TO  PRT                          EL527
00629          PERFORM WRITE-A-LINE.                                    EL527
00630                                                                   EL527
00631      MOVE +1                     TO  WS-REPORT-SW.                EL527
00632                                                                   EL527
00633  3200-SOP.                                                        EL527
00634      MOVE SPACES                 TO  WS-DETAIL1.                  EL527
00635                                                                   EL527
00636      IF NOT BILLING-CREDIT                                        EL527
00637          MOVE SR-CK-CHEK-GROUP   TO  WS-D1-GROUPING               EL527
00638          MOVE SR-CK-CHEK-STATE   TO  WS-D1-STATE                  EL527
00639          MOVE SR-CK-CHEK-ACCOUNT TO  WS-D1-ACCOUNT                EL527
00640          MOVE SR-CK-CHEK-CERT-NO TO  WS-D1-MESSAGE                EL527
00641      ELSE                                                         EL527
00642          MOVE SR-CK-PYAJ-GROUP   TO  WS-D1-GROUPING               EL527
00643          MOVE SR-CK-PYAJ-ACCOUNT TO  WS-D1-ACCOUNT                EL527
00644          MOVE SR-CK-PYAJ-FIN-RESP TO WS-D1-FIN-RESP.              EL527
00645                                                                   EL527
00646      MOVE SR-CARRIER             TO  WS-D1-CARRIER.               EL527
00647      MOVE SR-CK-CHECK-NUMBER     TO  WS-D1-CHECK-NUMBER.          EL527
00648                                                                   EL527
00649      IF SR-ALIGNMENT-CHECK  OR                                    EL527
00650         SR-SPOILED-CHECK  OR                                      EL527
00651         SR-PAYMENT-ABORTED                                        EL527
00652          MOVE ZEROS              TO  SR-CK-AMOUNT                 EL527
00653          MOVE '    *,***.** '    TO  WS-D1-ASTERISK-AMT           EL527
00654      ELSE                                                         EL527
00655          MOVE SR-CK-AMOUNT       TO  WS-D1-AMOUNT.                EL527
00656                                                                   EL527
00657      MOVE SR-CK-CONTROL-NUMBER   TO  WS-D1-CONTROL-GROUP.         EL527
00658      MOVE SR-CK-TIMES-PRINTED    TO  WS-D1-TIMES-PRINTED.         EL527
00659      MOVE SR-CK-CHECK-BY-USER    TO  WS-D1-PAYMENT-BY.            EL527
00660                                                                   EL527
00661      IF CHECK-IS-VOIDED                                           EL527
00662          SUBTRACT SR-CK-AMOUNT FROM  WS-TOT-CONTROL-AMT           EL527
00663                                      WS-TOT-CARRIER-AMT           EL527
00664                                      WS-TOT-CHECKS-AMT            EL527
00665      ELSE                                                         EL527
00666          ADD SR-CK-AMOUNT        TO  WS-TOT-CONTROL-AMT           EL527
00667                                      WS-TOT-CARRIER-AMT           EL527
00668                                      WS-TOT-CHECKS-AMT.           EL527
00669                                                                   EL527
00670      IF BILLING-CREDIT                                            EL527
00671          MOVE 'BILLING-CREDIT'   TO  WS-D1-USED-FOR               EL527
00672      ELSE                                                         EL527
00673          IF REFUND-PMT                                            EL527
00674              MOVE 'REFUND-PMT'   TO  WS-D1-USED-FOR               EL527
00675          ELSE                                                     EL527
00676              MOVE 'CHECK-MAINT'  TO  WS-D1-USED-FOR.              EL527
00677                                                                   EL527
00678      IF SR-MANUAL-CHECK                                           EL527
00679          IF BILLING-CREDIT                                        EL527
00680              MOVE '*MANUAL* BILL-CRED'     TO  WS-D1-USED-FOR     EL527
00681          ELSE                                                     EL527
00682              IF REFUND-PMT                                        EL527
00683                  MOVE '*MANUAL* REFND-PMT' TO  WS-D1-USED-FOR     EL527
00684              ELSE                                                 EL527
00685                  MOVE '*MANUAL* CHK-MAINT' TO  WS-D1-USED-FOR.    EL527
00686                                                                   EL527
00687      IF CHECK-IS-VOIDED                                           EL527
00688          IF BILLING-CREDIT                                        EL527
00689              MOVE '*VOIDED* BILL-CRED'     TO  WS-D1-USED-FOR     EL527
00690          ELSE                                                     EL527
00691              IF REFUND-PMT                                        EL527
00692                  MOVE '*VOIDED* REFND-PMT' TO  WS-D1-USED-FOR     EL527
00693              ELSE                                                 EL527
00694                  MOVE '*VOIDED* CHK-MAINT' TO  WS-D1-USED-FOR.    EL527
00695                                                                   EL527
00696      IF SR-ALIGNMENT-CHECK                                        EL527
00697          MOVE 'ALIGNMENT-CHECK'  TO  WS-D1-USED-FOR               EL527
00698      ELSE                                                         EL527
00699          IF SR-SPOILED-CHECK                                      EL527
00700              MOVE 'SPOILED-CHECK'    TO  WS-D1-USED-FOR           EL527
00701          ELSE                                                     EL527
00702              IF SR-PAYMENT-ABORTED                                EL527
00703                  MOVE 'ABORTED-CHECK'  TO  WS-D1-USED-FOR.        EL527
00704                                                                   EL527
00705      IF SR-CK-WRITTEN-DT NOT = LOW-VALUES                         EL527
00706          MOVE SR-CK-WRITTEN-DT       TO DC-BIN-DATE-1             EL527
00707          MOVE SPACES                 TO  DC-OPTION-CODE           EL527
00708          PERFORM 8500-DATE-CONVERSION                             EL527
00709          MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-DATE-PAID          EL527
00710                                          WS-DATE-PAID.            EL527
00711                                                                   EL527
00712      MOVE WS-DETAIL1             TO  PRT.                         EL527
00713      PERFORM WRITE-A-LINE.                                        EL527
00714                                                                   EL527
00715      MOVE SPACES                 TO  WS-DETAIL1.                  EL527
00716                                                                   EL527
00717      MOVE WS-DETAIL1             TO  PRT.                         EL527
00718      PERFORM WRITE-A-LINE.                                        EL527
00719                                                                   EL527
00720      GO TO 3100-SOP.                                              EL527
00721                                                                   EL527
00722  3180-SOP.                                                        EL527
00723      MOVE WS-CONTROL-DESC        TO  WS-T1-DESCRIPTION.           EL527
00724      MOVE WS-TOT-CONTROL-AMT     TO  WS-T1-AMOUNT.                EL527
00725      MOVE ZEROS                  TO  WS-TOT-CONTROL-AMT.          EL527
00726      MOVE WS-DETAIL1             TO  PRT.                         EL527
00727      PERFORM WRITE-A-LINE.                                        EL527
00728                                                                   EL527
00729      MOVE WS-CARRIER-DESC        TO  WS-T1-DESCRIPTION.           EL527
00730      MOVE WS-TOT-CARRIER-AMT     TO  WS-T1-AMOUNT.                EL527
00731      MOVE ZEROS                  TO  WS-TOT-CARRIER-AMT.          EL527
00732      MOVE WS-DETAIL1             TO  PRT.                         EL527
00733      MOVE '0'                    TO  P-CTL.                          CL**2
00734      PERFORM WRITE-A-LINE.                                        EL527
00735                                                                   EL527
00736      MOVE WS-TOTAL-DESC          TO  WS-T1-DESCRIPTION.           EL527
00737      MOVE WS-TOT-CHECKS-AMT      TO  WS-T1-AMOUNT.                EL527
00738      MOVE WS-DETAIL1             TO  PRT.                         EL527
00739      MOVE '-'                    TO  P-CTL.                          CL**2
00740      PERFORM WRITE-A-LINE.                                        EL527
00741                                                                   EL527
00742  3190-EXIT.                                                       EL527
00743      EXIT.                                                        EL527
00744      EJECT                                                        EL527
00745  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       EL527
00746                                                                   EL527
00747  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL527
00748                                                                   EL527
00749  WRITE-HEADINGS SECTION.                                          EL527
00750 ***************************************************************** EL527
00751 *                            ELCWHS1.                           * EL527
00752 *                            VMOD=2.001                         * EL527
00753 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          * EL527
00754 *****************************************************************.EL527
00755  WHS-010.                                                         EL527
00756      IF  WS-H2-DATE EQUAL SPACES                                  EL527
00757          MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   EL527
00758          MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME            EL527
00759          MOVE ALPH-DATE          TO  WS-H3-DATE.                  EL527
00760                                                                   EL527
00761      ADD +1  TO  WS-PAGE.                                         EL527
00762      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL527
00763      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL527
00764      MOVE ZERO                   TO  WS-LINE-COUNT.               EL527
00765                                                                   EL527
00766      MOVE WS-HEADING1            TO  PRT.                         EL527
00767      MOVE '1'                    TO  X.                           EL527
00768      PERFORM WRITE-PRINTER.                                       EL527
00769                                                                   EL527
00770      MOVE WS-HEADING2            TO  PRT.                         EL527
00771      MOVE ' '                    TO  X.                           EL527
00772      PERFORM WRITE-PRINTER.                                       EL527
00773                                                                   EL527
00774      MOVE WS-HEADING3            TO  PRT.                         EL527
00775      MOVE ' '                    TO  X.                           EL527
00776      PERFORM WRITE-PRINTER.                                       EL527
00777                                                                   EL527
00778      MOVE WS-HEADING4            TO  PRT.                         EL527
00779      MOVE ' '                    TO  X.                           EL527
00780      PERFORM WRITE-PRINTER.                                       EL527
00781                                                                   EL527
00782                                                                   EL527
00783      MOVE WS-HEADING5            TO  PRT.                         EL527
00784      PERFORM WRITE-PRINTER.                                       EL527
00785                                                                   EL527
00786      MOVE +10                    TO  WS-LINE-COUNT.               EL527
00787                                                                   EL527
00788  WHS-020. COPY ELCWHS2.                                           EL527
00789                                                                   EL527
00790  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL527
00791                                                                   EL527
00792  WPS-020. COPY ELCPRT2X.                                          EL527
00793                                                                   EL527
00794  OPEN-FILES SECTION.                                              EL527
00795  OFS-010.                                                         EL527
00796      OPEN INPUT ERCHKQ                                            EL527
00797                 ELCNTL                                            EL527
00798           OUTPUT PRNTR.                                           EL527
00799                                                                   EL527
00800      IF ELCNTL-FILE-STATUS  = '00' OR '97'                        EL527
00801          NEXT SENTENCE                                            EL527
00802        ELSE                                                       EL527
00803          MOVE 'ERROR OCCURED OPEN - ELCNTL'                       EL527
00804                                  TO  WS-ABEND-MESSAGE             EL527
00805          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL527
00806          PERFORM ABEND-PGM.                                       EL527
00807                                                                   EL527
00808      IF ERCHKQ-FILE-STATUS  = '00' OR '97'                        EL527
00809          NEXT SENTENCE                                            EL527
00810        ELSE                                                       EL527
00811          MOVE 'ERROR OCCURED OPEN - ERCHKQ'                       EL527
00812                                  TO  WS-ABEND-MESSAGE             EL527
00813          MOVE ERCHKQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL527
00814          PERFORM ABEND-PGM.                                       EL527
00815                                                                   EL527
00816  OFS-EXIT.                                                        EL527
00817      EXIT.                                                        EL527
00818                                                                   EL527
00819  CLOSE-FILES SECTION.                                             EL527
00820  CFS-010. COPY ELCPRTCX.                                          EL527
00821                                                                   EL527
00822      CLOSE ELCNTL                                                 EL527
00823            ERCHKQ                                                 EL527
00824            PRNTR.                                                 EL527
00825                                                                   EL527
00826      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL527
00827          MOVE 'ERROR OCCURED CLOSE - ELCNTL'                      EL527
00828                                  TO  WS-ABEND-MESSAGE             EL527
00829          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL527
00830          PERFORM ABEND-PGM.                                       EL527
00831                                                                   EL527
00832      IF ERCHKQ-FILE-STATUS NOT = ZERO                             EL527
00833          MOVE 'ERROR OCCURED CLOSE - ERCHKQ'                      EL527
00834                                  TO  WS-ABEND-MESSAGE             EL527
00835          MOVE ERCHKQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL527
00836          PERFORM ABEND-PGM.                                       EL527
00837                                                                   EL527
00838  CFS-EXIT.                                                        EL527
00839      EXIT.                                                        EL527
00840                                                                   EL527
00841  ABEND-PGM SECTION. COPY ELCABEND SUPPRESS.                       EL527
00842                                                                   EL527
