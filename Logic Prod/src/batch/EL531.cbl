00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL531
00003  PROGRAM-ID.                 EL531 .                                 LV013
00004 *              PROGRAM CONVERTED BY                               EL531
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL531
00006 *              CONVERSION DATE 04/10/96 10:15:48.                 EL531
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL531
00008 *                            VMOD=2.012.                          EL531
00009                                                                   EL531
00010 *AUTHOR.     LOGIC, INC.                                          EL531
00011 *            DALLAS, TEXAS.                                       EL531
00012                                                                   EL531
00013 *DATE-COMPILED.                                                   EL531
00014                                                                   EL531
00015 *SECURITY.   *****************************************************EL531
00016 *            *                                                   *EL531
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL531
00018 *            *                                                   *EL531
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL531
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL531
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL531
00022 *            *                                                   *EL531
00023 *            *****************************************************EL531
00024                                                                   EL531
00025 *REMARKS.                                                         EL531
00026 *    PRINT CONFIRMATION REPORT FOR NEW ISSUES, CANCELLATIONS      EL531
00027 *    WILL CREATE AN ENTRY TO CHECK FILE FOR COMPUTED REFUNDS.     EL531
00028                                                                   EL531
00029 *    OPTIONS -                                                    EL531
00030 *            PROCESS - 1 - DO NOT CREATE CHECK ENTRY              EL531
00031 *            PROCESS - 2 - CREATE CHECK ENTRY                     EL531
00032 *            PROCESS - 3 - RESET CONFIRMATION FLAG IF DATE IN     EL531
00033 *                          FLAG IS EQUAL TO DATE CARD DATE.       EL531
00034 *                          (TOTAL OPTION IS IGNORED)              EL531
00035 *            PROCESS - 4 - PRINT ONLY ENTRIES THAT HAVE BEEN      EL531
00036 *                          ENTERED ON THE SAME DAY AS THE REPORT  EL531
00037 *                          IS RUN. (USES DATE-CARD DATE AND ENTRY EL531
00038 *                          DATE AS MATCH).                        EL531
00039                                                                   EL531
00040 *            TOTAL   - 1 - TOTAL ALL ENTRIES THAT HAVE NOT HAD    EL531
00041 *                          A CHECK CREATED.                       EL531
00042 *            TOTAL   - 2 - TOTAL ALL ENTRIES IN FILE WITH NO      EL531
00043 *                          ERRORS                                 EL531
00044 *            TOTAL   - 3 - TOTAL ALL ENTRIES IN FILE WITH OR      EL531
00045 *                          WITHOUT ERRORS.                        EL531
00046                                                                   EL531
00047  ENVIRONMENT DIVISION.                                            EL531
00048  INPUT-OUTPUT SECTION.                                            EL531
00049  FILE-CONTROL.                                                    EL531
00050      SELECT ERPNDB           ASSIGN TO SYS010-FBA1-ERPNDB         EL531
00051                                ORGANIZATION IS INDEXED            EL531
00052                                ACCESS IS DYNAMIC                  EL531
00053                                RECORD KEY IS PB-CONTROL-PRIMARY   EL531
00054                                FILE STATUS IS ERPNDB-FILE-STATUS. EL531
00055                                                                   EL531
00056      SELECT ERACCT            ASSIGN TO SYS015-FBA1-ERACCT        EL531
00057                                ORGANIZATION IS INDEXED            EL531
00058                                ACCESS IS DYNAMIC                  EL531
00059                                RECORD KEY IS AM-CONTROL-PRIMARY   EL531
00060                                FILE STATUS IS ERACCT-FILE-STATUS. EL531
00061                                                                   EL531
00062      SELECT ERPYAJ           ASSIGN TO SYS011-FBA1-ERPYAJ         EL531
00063                                ORGANIZATION IS INDEXED            EL531
00064                                ACCESS IS DYNAMIC                  EL531
00065                                RECORD KEY IS PY-CONTROL-PRIMARY   EL531
00066                                FILE STATUS IS ERPYAJ-FILE-STATUS. EL531
00067                                                                   EL531
00068      SELECT ELCNTL           ASSIGN TO SYS012-FBA1-ELCNTL         EL531
00069                                ORGANIZATION IS INDEXED            EL531
00070                                ACCESS IS DYNAMIC                  EL531
00071                                RECORD KEY IS CF-CONTROL-PRIMARY   EL531
00072                                FILE STATUS IS ELCNTL-FILE-STATUS. EL531
00073                                                                   EL531
00074      SELECT DISK-DATE         ASSIGN TO SYS019-UT-2314-S-SYS019.  EL531
00075      SELECT SORT-FILE         ASSIGN TO SYS001-UT-2314-S-SORTWK1. EL531
00076      SELECT PRNTR             ASSIGN TO SYS008-UR-1403-S-SYS008.  EL531
00077      SELECT FICH              ASSIGN TO SYS020-UT-2400-S-SYS020.  EL531
00078                                                                   EL531
00079      SELECT ELREPT            ASSIGN TO SYS010-FBA1-ELREPT        EL531
00080                                ORGANIZATION IS INDEXED            EL531
00081                                ACCESS IS DYNAMIC                  EL531
00082                                RECORD KEY IS RF-CONTROL-PRIMARY   EL531
00083                                FILE STATUS IS DTE-VSAM-FLAGS.     EL531
00084      EJECT                                                        EL531
00085  DATA DIVISION.                                                   EL531
00086  FILE SECTION.                                                    EL531
00087  FD  ERPNDB.                                                      EL531
00088                              COPY ERCPNDB.                        EL531
00089      EJECT                                                        EL531
00090                                                                   EL531
00091  FD  ERACCT.                                                      EL531
00092                              COPY ERCACCT.                        EL531
00093  EJECT                                                            EL531
00094                                                                   EL531
00095  FD  ERPYAJ.                                                      EL531
00096                              COPY ERCPYAJ.                        EL531
00097      EJECT                                                        EL531
00098                                                                   EL531
00099  FD  ELCNTL.                                                      EL531
00100                              COPY ELCCNTL.                           CL**2
00101                                                                   EL531
00102  FD  DISK-DATE               COPY ELCDTEFD.                          CL**2
00103                                                                   EL531
00104  FD  PRNTR                   COPY ELCPRTFD.                       EL531
00105                                                                   EL531
00106  FD  FICH                    COPY ELCFCHFD.                       EL531
00107      EJECT                                                        EL531
00108                                                                   EL531
00109  FD  ELREPT                  COPY ELCRPTFD.                       EL531
00110                              COPY ELCREPT.                        EL531
00111      EJECT                                                        EL531
00112                                                                   EL531
00113  SD  SORT-FILE.                                                   EL531
00114                                                                   EL531
00115  01  SORT-REC.                                                    EL531
00116      05  FILLER       PIC XX.                                     EL531
00117      05  SR-CAR-CO    PIC X(7).                                   EL531
00118      05  SR-STATE     PIC XX.                                     EL531
00119      05  SR-ACCT      PIC X(10).                                  EL531
00120      05  FILLER       PIC X(6).                                   EL531
00121      05  SR-CERT-NO   PIC X(11).                                  EL531
00122      05  SR-TYPE      PIC X.                                      EL531
00123      05  SR-MEMB-NO   PIC X(12).                                  EL531
00124      05  FILLER       PIC X(149).                                 EL531
00125      EJECT                                                        EL531
00126  WORKING-STORAGE SECTION.                                         EL531
00127  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL531
00128  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL531
00129  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.   EL531
00130  77  FILLER  PIC X(32) VALUE '********************************'.  EL531
00131  77  FILLER  PIC X(32) VALUE '     EL531  WORKING STORAGE     '.  EL531
00132  77  FILLER  PIC X(32) VALUE '*****VMOD=2.012 ****************'.  EL531
00133                                                                   EL531
00134  77  LINE-CNT                 PIC 99              VALUE ZEROS.    EL531
00135  77  PAGE-SIZE                PIC 99              VALUE 56.       EL531
00136  77  X                        PIC X               VALUE SPACE.    EL531
00137  77  PAGE-CNT                 PIC S999            VALUE ZEROS.    EL531
00138  77  RECORDS-RELEASED         PIC S9(7)           VALUE +0.       EL531
00139  77  SAVE-POST-DATE           PIC XX              VALUE LOW-VALUE.EL531
00140  77  SAVE-POST-DATE-BEG       PIC XX              VALUE LOW-VALUE.EL531
00141  77  SAVE-POST-DATE-END       PIC XX              VALUE LOW-VALUE.EL531
00142  77  SAVE-ACCT-EXP-DT         PIC XX              VALUE LOW-VALUE.EL531
00143  77  SAVE-ACCT-CARRIER        PIC X               VALUE LOW-VALUE.EL531
00144  77  Z-DATE                   PIC X(6).                           EL531
00145                                                                   EL531
00146  01  DTE-INTERFACE-CODES.                                         EL531
00147      12  ABEND-CODE           PIC X(4)             VALUE SPACES.  EL531
00148      12  ABEND-OPTION         PIC X                VALUE 'Y'.     EL531
00149      12  PGM-SUB              PIC S999     COMP    VALUE +531.    EL531
00150      12  OLC-REPORT-NAME      PIC X(6)             VALUE 'EL531'. EL531
00151      12  WS-ABEND-MESSAGE     PIC X(80)            VALUE SPACE.   EL531
00152      12  WS-ABEND-FILE-STATUS PIC X(4)             VALUE SPACE.   EL531
00153      12  WS-RETURN-CODE       PIC S9(4)    COMP    VALUE ZERO.    EL531
00154      12  WS-ZERO              PIC S9(4)    COMP    VALUE ZERO.    EL531
00155      12  WS-CHECK-FIRST-TIME  PIC X                VALUE 'Y'.     EL531
00156          88  CHECK-FIRST-TIME                      VALUE 'Y'.     EL531
00157      12  WS-ACCESS-COMP-CNTL        PIC X.                        EL531
00158          88  WS-USE-ACTUAL-CARRIER      VALUE ' '.                EL531
00159          88  WS-ZERO-CARRIER            VALUE '1'.                EL531
00160          88  WS-ZERO-GROUPING           VALUE '2'.                EL531
00161          88  WS-ZERO-CAR-GROUP          VALUE '3'.                EL531
00162      12  WS-CR-CHECK-NO-METHOD  PIC X.                            EL531
00163          88  WS-CHECK-NO-MANUAL       VALUE '1'.                  EL531
00164          88  WS-CHECK-NO-AUTO-SEQ     VALUE '2'.                  EL531
00165          88  WS-CHECK-NO-AT-PRINT     VALUE '4'.                  EL531
00166      12  WS-CR-MONTH-END-DT     PIC XX.                           EL531
00167                                                                   EL531
00168  01  ERPNDB-FILE-STATUS.                                          EL531
00169      12  ERPNDB-FILE-STATUS-1    PIC X.                           EL531
00170      12  ERPNDB-FILE-STATUS-2    PIC X.                           EL531
00171                                                                   EL531
00172  01  ERACCT-FILE-STATUS.                                          EL531
00173      12  ERACCT-FILE-STATUS-1    PIC X.                           EL531
00174      12  ERACCT-FILE-STATUS-2    PIC X.                           EL531
00175                                                                   EL531
00176  01  ERPYAJ-FILE-STATUS.                                          EL531
00177      12  ERPYAJ-FILE-STATUS-1    PIC X.                           EL531
00178      12  ERPYAJ-FILE-STATUS-2    PIC X.                           EL531
00179                                                                   EL531
00180  01  ELCNTL-FILE-STATUS.                                          EL531
00181      12  ELCNTL-FILE-STATUS-1    PIC X.                           EL531
00182      12  ELCNTL-FILE-STATUS-2    PIC X.                           EL531
00183                                                                   EL531
00184  01  WS-PB-CONTROL-PRIMARY.                                       EL531
00185      12  WS-PB-COMPANY-CD                PIC X.                   EL531
00186      12  WS-PB-ENTRY-BATCH               PIC X(6).                EL531
00187      12  WS-PB-SEQ-NO                    PIC S9(4)  COMP.         EL531
00188      12  WS-PB-CHG-SEQ-NO                PIC S9(4)  COMP.         EL531
00189                                                                   EL531
00190                                  COPY ELCDATE.                       CL*13
00191                                                                   EL531
00192  01  SAVE-CONTROL.                                                EL531
00193      12  SAVE-CNTRL1.                                             EL531
00194          16  S-CAR-CO.                                            EL531
00195              20  S-CAR            PIC X.                          EL531
00196              20  S-GROUPING       PIC X(6).                       EL531
00197          16  S-ACCT               PIC X(10).                      EL531
00198      12  S-STATE                  PIC XX.                         EL531
00199      12  S-TYPE                   PIC X.                          EL531
00200                                                                   EL531
00201  01  BUILD-CONTROL.                                               EL531
00202      12  BUILD-CNTRL1.                                            EL531
00203          16  B-CAR-CO.                                            EL531
00204              20  B-CAR        PIC X.                              EL531
00205              20  B-GROUPING   PIC X(6).                           EL531
00206          16  B-ACCT           PIC X(10).                          EL531
00207      12  B-STATE              PIC XX.                             EL531
00208      12  B-TYPE               PIC X.                              EL531
00209                                                                   EL531
00210  01  TOTAL-AREAS.                                                 EL531
00211      12  T-LIFE-PREMIUM       PIC S9(7)V99 COMP-3 VALUE ZEROS.    EL531
00212      12  T-LIFE-REFUND        PIC S9(7)V99 COMP-3 VALUE ZEROS.    EL531
00213      12  T-DISB-PREMIUM       PIC S9(7)V99 COMP-3 VALUE ZEROS.    EL531
00214      12  T-DISB-REFUND        PIC S9(7)V99 COMP-3 VALUE ZEROS.    EL531
00215      12  T-CERTS              PIC S9(7)    COMP-3 VALUE ZEROS.    EL531
00216      12  T-LIFE-CERTS         PIC S9(7)    COMP-3 VALUE ZEROS.    EL531
00217      12  T-AH-CERTS           PIC S9(7)    COMP-3 VALUE ZEROS.    EL531
00218      12  T-TERM               PIC S9(7)    COMP-3 VALUE ZEROS.    EL531
00219      12  T-AGE                PIC S9(7)    COMP-3 VALUE ZEROS.    EL531
00220      12  WS-CHECK-AMT         PIC S9(9)V99 COMP-3 VALUE ZEROS.    EL531
00221                                                                   EL531
00222      EJECT                                                        EL531
00223                                COPY ERCCONF.                      EL531
00224      EJECT                                                        EL531
00225  01  HEAD-1.                                                      EL531
00226      12  FILLER          PIC X(52) VALUE SPACES.                  EL531
00227      12  FILLER          PIC X(19) VALUE 'CONFIRMATION REPORT'.   EL531
00228      12  FILLER          PIC X(48) VALUE SPACES.                  EL531
00229      12  FILLER          PIC X(8)  VALUE 'EL531   '.              EL531
00230                                                                   EL531
00231  01  HEAD-2.                                                      EL531
00232      12  FILLER          PIC X(47)         VALUE SPACES.          EL531
00233      12  HD-CO-NAME      PIC X(30)         VALUE SPACES.          EL531
00234      12  FILLER          PIC X(42)         VALUE SPACES.          EL531
00235      12  HD-DATE         PIC X(8)          VALUE SPACES.          EL531
00236                                                                   EL531
00237  01  HEAD-3.                                                      EL531
00238      12  FILLER          PIC X(51)         VALUE SPACES.          EL531
00239      12  HD-DATE-RANGE.                                           EL531
00240          16  HD-DATE-FROM  PIC X(8)        VALUE SPACES.          EL531
00241          16  FILLER        PIC X(6)        VALUE ' THRU '.        EL531
00242          16  HD-DATE-TO    PIC X(8)        VALUE SPACES.          EL531
00243      12  FILLER          PIC X(46)         VALUE SPACES.          EL531
00244      12  FILLER          PIC X(5)          VALUE 'PAGE'.          EL531
00245      12  HD-PAGE         PIC ZZ,ZZZ.                              EL531
00246                                                                   EL531
00247  01  HEAD-4.                                                      EL531
00248      12  FILLER          PIC X(11)         VALUE ' ACCOUNT - '.   EL531
00249      12  HD-ACCT-NO      PIC X(12)         VALUE SPACES.          EL531
00250      12  FILLER          PIC X(7)          VALUE 'NAME - '.       EL531
00251      12  HD-NAME1        PIC X(20)         VALUE SPACES.          EL531
00252                                                                   EL531
00253  01  HEAD-5.                                                      EL531
00254      12  FILLER          PIC X(27)         VALUE SPACES.          EL531
00255      12  HD-NAME2        PIC X(20)         VALUE SPACES.          EL531
00256                                                                   EL531
00257  01  HEAD-6.                                                      EL531
00258      12  FILLER          PIC X(22)         VALUE                  EL531
00259                                          '    CARRIER/COMPANY - '.EL531
00260      12  HD-CAR-CO       PIC X(8)          VALUE SPACES.          EL531
00261      12  FILLER          PIC X(8)          VALUE 'STATE - '.      EL531
00262      12  HD-STATE        PIC X(20)         VALUE SPACES.          EL531
00263                                                                   EL531
00264  01  HEAD-7.                                                      EL531
00265      12  FILLER          PIC X(52)         VALUE SPACES.          EL531
00266      12  HD-REPORT       PIC X(20).                               EL531
00267                                                                   EL531
00268  01  HEAD-DETAIL-1.                                               EL531
00269      12  FILLER          PIC X(32) VALUE '   MEMBER     NAME'.    EL531
00270      12  FILLER          PIC X(19) VALUE 'CERT     EFF.  ISS '.   EL531
00271      12  HEAD-1-REP      PIC X(16) VALUE 'PMT NO. SK TRM  '.      EL531
00272      12  FILLER          PIC X(10) VALUE '---------'.             EL531
00273      12  HD-T-LF         PIC X(6)  VALUE 'LIFE'.                  EL531
00274      12  FILLER          PIC X(11) VALUE '---------'.             EL531
00275      12  FILLER          PIC X(10) VALUE '---------'.             EL531
00276      12  HD-T-AH         PIC X(6)  VALUE 'A/H'.                   EL531
00277      12  FILLER          PIC X(11) VALUE '---------'.             EL531
00278      12  HD-NET          PIC X(11) VALUE SPACES.                  EL531
00279                                                                   EL531
00280  01  HEAD-DETAIL-2.                                               EL531
00281      12  FILLER          PIC X(32) VALUE '   NUMBER'.             EL531
00282      12  FILLER          PIC X(19) VALUE ' NO.     DATE  AGE '.   EL531
00283      12  HEAD-2-REP      PIC X(16) VALUE 'FRQ PMT CD'.            EL531
00284      12  FILLER          PIC X(27) VALUE                          EL531
00285                                     'TYP    PREMIUM     REFUND'.  EL531
00286      12  FILLER          PIC X(27) VALUE                          EL531
00287                                     'TYP    PREMIUM     REFUND'.  EL531
00288      12  HD-ADDON        PIC X(11) VALUE SPACES.                  EL531
00289                                                                   EL531
00290  01  HEAD-DETAIL-2A.                                              EL531
00291      12  FILLER          PIC X(32) VALUE '   NUMBER'.             EL531
00292      12  FILLER          PIC X(19) VALUE ' NO.     DATE  AGE '.   EL531
00293      12  FILLER          PIC X(16) VALUE 'FRQ PMT CD'.            EL531
00294      12  FILLER          PIC X(27) VALUE                          EL531
00295                                  'TYP     AMOUNT    PREMIUM'.     EL531
00296      12  FILLER          PIC X(27) VALUE                          EL531
00297                                  'TYP    PAYMENT    PREMIUM'.     EL531
00298                                                                   EL531
00299  01  RPT-ENDED.                                                   EL531
00300      12  FILLER                PIC X(30)                          EL531
00301                 VALUE '* CONFIRMATION REPORT ENDED - '.           EL531
00302      12  RE-MESSAGE            PIC X(30).                         EL531
00303                                                                   EL531
00304  01  DETAIL-LINE.                                                 EL531
00305      12  D-MEMBER              PIC X(12).                         EL531
00306      12  D-FLAG                PIC XX VALUE SPACE.                EL531
00307      12  D-INITIAL             PIC X(3).                          EL531
00308      12  D-LAST-NAME           PIC X(9).                          EL531
00309      12  FILLER                PIC X  VALUE SPACE.                EL531
00310      12  D-CERT-NO             PIC X(12).                         EL531
00311      12  D-EFF-DATE.                                              EL531
00312          16  D-EFF-MO          PIC 99.                            EL531
00313          16  FILLER            PIC X VALUE '-'.                   EL531
00314          16  D-EFF-DAY         PIC 99.                            EL531
00315          16  FILLER            PIC X VALUE '-'.                   EL531
00316          16  D-EFF-YR          PIC 99.                            EL531
00317      12  FILLER                PIC X VALUE SPACE.                 EL531
00318      12  D-ISS-AGE             PIC 99.                            EL531
00319      12  FILLER                PIC X VALUE SPACE.                 EL531
00320      12  D-PMT-FRQ             PIC X(4).                          EL531
00321      12  D-NO-PMT              PIC ZZZ.                           EL531
00322      12  FILLER                PIC X VALUE SPACES.                EL531
00323      12  D-SKIP-CODE           PIC ZZ.                            EL531
00324      12  FILLER                PIC X VALUE SPACES.                EL531
00325      12  D-TERM                PIC ZZZ.                           EL531
00326      12  FILLER                PIC XX VALUE SPACES.               EL531
00327      12  D-LIFE-TYPE           PIC X(3).                          EL531
00328      12  D-LIFE-PREM           PIC ZZZZ,ZZZ.ZZ-.                  EL531
00329      12  D-LIFE-REFUND         PIC ZZZ,ZZZ.ZZ-.                   EL531
00330      12  FILLER                PIC X VALUE SPACE.                 EL531
00331      12  D-DISB-TYPE           PIC X(3).                          EL531
00332      12  D-DISB-PREM           PIC ZZZZ,ZZZ.ZZ-.                  EL531
00333      12  D-DISB-REFUND         PIC ZZZ,ZZZ.ZZ-.                   EL531
00334      12  D-CANCEL-DATE.                                           EL531
00335          16  FILLER            PIC XXX     VALUE SPACES.          EL531
00336          16  D-CANC-DATE.                                         EL531
00337              20  D-CANC-MO     PIC 99.                            EL531
00338              20  FILLER        PIC X.                             EL531
00339              20  D-CANC-DA     PIC 99.                            EL531
00340              20  FILLER        PIC X.                             EL531
00341              20  D-CANC-YR     PIC 99.                            EL531
00342          16  FILLER            PIC X.                             EL531
00343                                                                   EL531
00344  01  TOTAL-LINE.                                                  EL531
00345      12  FILLER                PIC X(41) VALUE SPACES.            EL531
00346      12  FILLER                PIC X(29) VALUE '*** TOTAL ***'.   EL531
00347      12  TL-LIFE-PREM          PIC ZZZZ,ZZZ.99-.                  EL531
00348      12  TL-LIFE-PREM-R  REDEFINES TL-LIFE-PREM                   EL531
00349                                PIC ZZZZ,ZZZ.ZZ-.                  EL531
00350      12  TL-LIFE-REFUND        PIC ZZZ,ZZZ.99-.                   EL531
00351      12  FILLER                PIC X(4) VALUE SPACES.             EL531
00352      12  TL-DISB-PREM          PIC ZZZZ,ZZZ.99-.                  EL531
00353      12  TL-DISB-PREM-R  REDEFINES TL-DISB-PREM                   EL531
00354                                PIC ZZZZ,ZZZ.ZZ-.                  EL531
00355      12  TL-DISB-REFUND        PIC ZZZ,ZZZ.99-.                   EL531
00356      12  FILLER                PIC X(12)     VALUE SPACES.        EL531
00357                                                                   EL531
00358  01  TOTAL-LINE2.                                                 EL531
00359      12  FILLER                PIC X(41) VALUE SPACES.            EL531
00360      12  FILLER                PIC X(29) VALUE                    EL531
00361          '*** CHECK AMOUNT ***'.                                  EL531
00362      12  TL-CHK-AMOUNT         PIC ZZZZ,ZZZ.99-.                  EL531
00363                                                                   EL531
00364  01  ISSUE-LINE1.                                                 EL531
00365      12  FILLER                PIC X(10) VALUE SPACES.            EL531
00366      12  ISSUE-CERTS           PIC ZZZ,ZZ9.                       EL531
00367      12  FILLER                PIC X(16) VALUE ' CERTS ISSUED'.   EL531
00368      12  FILLER                PIC X(10) VALUE 'WITH AN'.         EL531
00369      12  FILLER                PIC X(25) VALUE                    EL531
00370                          'AVERAGE PREMIUM OF'.                    EL531
00371      12  ISSUE-AVERAGE         PIC ZZZ,ZZZ.99.                    EL531
00372                                                                   EL531
00373  01  ISSUE-LINE2.                                                 EL531
00374      12  FILLER                PIC X(43) VALUE SPACES.            EL531
00375      12  FILLER                PIC X(32) VALUE                    EL531
00376                          'AVERAGE AGE OF'.                        EL531
00377      12  ISSUE-AGE             PIC ZZ9.                           EL531
00378                                                                   EL531
00379  01  ISSUE-LINE3.                                                 EL531
00380      12  FILLER                PIC X(43) VALUE SPACES.            EL531
00381      12  FILLER                PIC X(31) VALUE                    EL531
00382                          'AVERAGE TERM OF'.                       EL531
00383      12  ISSUE-TERM            PIC ZZZ9.                          EL531
00384                                                                   EL531
00385  01  ISSUE-LINE4.                                                 EL531
00386      12  FILLER                PIC X(43) VALUE SPACES.            EL531
00387      12  FILLER                PIC X(23) VALUE                    EL531
00388                          'AVERAGE LIFE BENEFIT OF'.               EL531
00389      12  ISSUE-L-BEN           PIC Z,ZZZ,ZZZ.99.                  EL531
00390                                                                   EL531
00391  01  ISSUE-LINE5.                                                 EL531
00392      12  FILLER                PIC X(43) VALUE SPACES.            EL531
00393      12  FILLER                PIC X(23) VALUE                    EL531
00394                          'AVERAGE A&H BENEFIT OF'.                EL531
00395      12  ISSUE-D-BEN           PIC Z,ZZZ,ZZZ.99.                  EL531
00396                                                                   EL531
00397  01  CANCEL-LINE.                                                 EL531
00398      12  FILLER              PIC X(10) VALUE SPACES.              EL531
00399      12  CANC-CERTS          PIC ZZZ,ZZ9.                         EL531
00400      12  FILLER              PIC X(20) VALUE ' CERTS CANCELLED'.  EL531
00401      12  CANC-PERCENT        PIC ZZ9.                             EL531
00402      12  FILLER              PIC X(18) VALUE '% OF GROSS PREMIUM'.EL531
00403                                                                   EL531
00404                      COPY ELCDTECX.                                  CL**2
00405                                                                      CL**3
00406                      COPY ELCDTEVR.                                  CL**3
00407      EJECT                                                        EL531
00408                                                                   EL531
00409  01  FIA-DETAIL.                                                  EL531
00410      05  FIA-LINE-NAME       PIC X(36).                           EL531
00411      05  FIA-CNT             PIC ZZZ9.                            EL531
00412      05  FILLER              PIC X(10) VALUE '-CANCELS  '.        EL531
00413      05  FIA-ISSUE-PRM       PIC Z,ZZZ,ZZ9.99.                    EL531
00414      05  FILLER              PIC X(17) VALUE '-ORIGINAL PREMIUM'. EL531
00415      05  FILLER              PIC X(2) VALUE SPACES.               EL531
00416      05  FIA-CANCEL-PRM      PIC Z,ZZZ,ZZ9.99.                    EL531
00417      05  FILLER              PIC X(20) VALUE '-REFUNDED PREMIUM'. EL531
00418                                                                   EL531
00419  01  FIA-ACCUMS.                                                  EL531
00420      05  P-1101-CNT-L        PIC S9(5)     COMP-3    VALUE ZERO.  EL531
00421      05  P-1101-CNT-A        PIC S9(5)     COMP-3    VALUE ZERO.  EL531
00422      05  A-1101-CNT-A        PIC S9(5)     COMP-3    VALUE ZERO.  EL531
00423      05  A-1101-CNT-L        PIC S9(5)     COMP-3    VALUE ZERO.  EL531
00424      05  A-1101-ISSUE-PRM-L  PIC S9(7)V99  COMP-3    VALUE ZERO.  EL531
00425      05  P-1101-ISSUE-PRM-L  PIC S9(7)V99  COMP-3    VALUE ZERO.  EL531
00426      05  A-1101-ISSUE-PRM-A  PIC S9(7)V99  COMP-3    VALUE ZERO.  EL531
00427      05  P-1101-ISSUE-PRM-A  PIC S9(7)V99  COMP-3    VALUE ZERO.  EL531
00428      05  P-1101-CANCEL-PRM-L PIC S9(7)V99  COMP-3    VALUE ZERO.  EL531
00429      05  A-1101-CANCEL-PRM-L PIC S9(7)V99  COMP-3    VALUE ZERO.  EL531
00430      05  P-1101-CANCEL-PRM-A PIC S9(7)V99  COMP-3    VALUE ZERO.  EL531
00431      05  A-1101-CANCEL-PRM-A PIC S9(7)V99  COMP-3    VALUE ZERO.  EL531
00432                                                                   EL531
00433      EJECT                                                        EL531
00434  PROCEDURE DIVISION.                                              EL531
00435                                                                   EL531
00436  0100-DATE-ROUTINE.                                               EL531
00437                                                                   EL531
00438      COPY ELCDTERX.                                                  CL**2
00439                                                                   EL531
00440      MOVE LIFE-OVERRIDE-L6    TO HD-T-LF.                         EL531
00441      MOVE AH-OVERRIDE-L6      TO HD-T-AH.                         EL531
00442                                                                   EL531
00443  0110-SETUP-HEADINGS.                                             EL531
00444      DISPLAY 'OPTIONS IN FORCE FOR THIS RUN -'                    EL531
00445      DISPLAY '    TOTAL OPTION = ', DTE-TOT-OPT.                  EL531
00446      DISPLAY '    PROCESS OPTION = ', DTE-PRC-OPT.                EL531
00447                                                                   EL531
00448      MOVE WS-CURRENT-DATE TO HD-DATE.                             EL531
00449      MOVE COMPANY-NAME    TO HD-CO-NAME.                          EL531
00450                                                                   EL531
00451      COMPUTE LINE-CNT = PAGE-SIZE + 1.                            EL531
00452                                                                   EL531
00453      MOVE BIN-RUN-DATE  TO SAVE-POST-DATE-END                     EL531
00454                             SAVE-POST-DATE                           CL**5
00455                             DC-BIN-DATE-1.                           CL**5
00456      MOVE ' '             TO DC-OPTION-CODE.                      EL531
00457      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   EL531
00458      IF NO-CONVERSION-ERROR                                       EL531
00459          MOVE DC-GREG-DATE-1-EDIT TO HD-DATE-TO                   EL531
00460      ELSE                                                         EL531
00461          MOVE LOW-VALUES    TO SAVE-POST-DATE-END                 EL531
00462                                SAVE-POST-DATE.                    EL531
00463                                                                   EL531
00464      MOVE EP-DT        TO DC-GREG-DATE-CYMD.                         CL**7
00465      MOVE 'L'          TO DC-OPTION-CODE.                            CL**5
00466      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   EL531
00467                                                                   EL531
00468      IF NO-CONVERSION-ERROR                                       EL531
00469          MOVE DC-BIN-DATE-1 TO SAVE-POST-DATE-BEG                 EL531
00470          MOVE DC-GREG-DATE-1-EDIT TO HD-DATE-FROM                 EL531
00471      ELSE                                                         EL531
00472          MOVE LOW-VALUES    TO SAVE-POST-DATE-BEG.                EL531
00473                                                                   EL531
00474  0120-SORT-PROCEDURE.                                             EL531
00475      SORT SORT-FILE  ASCENDING KEY                                EL531
00476                      SR-CAR-CO SR-ACCT SR-STATE SR-TYPE           EL531
00477                      SR-MEMB-NO SR-CERT-NO                        EL531
00478                                                                   EL531
00479          INPUT PROCEDURE 1000-SECTION                             EL531
00480                      THRU 1999-READ-EXIT                          EL531
00481                                                                   EL531
00482          OUTPUT PROCEDURE 3000-PRINT-SECTION                      EL531
00483                      THRU 3999-PRINT-EXIT.                        EL531
00484                                                                   EL531
00485      IF SORT-RETURN NOT = ZERO                                    EL531
00486          MOVE 'ERROR OCCURED SORTING FILE' TO WS-ABEND-MESSAGE    EL531
00487          MOVE '99' TO WS-ABEND-FILE-STATUS                        EL531
00488          GO TO ABEND-PGM.                                         EL531
00489                                                                   EL531
00490      EJECT                                                        EL531
00491                                                                   EL531
00492 ****************************************************************  EL531
00493 *    SPECIAL TOTALS FOR FIA                                       EL531
00494 ****************************************************************  EL531
00495      IF DTE-CLIENT NOT = 'FIA'                                    EL531
00496          GO TO 9900-END-OF-JOB.                                   EL531
00497                                                                   EL531
00498      MOVE ZERO                TO PAGE-CNT.                        EL531
00499      PERFORM 3650-HEADING     THRU 3650-EXIT.                     EL531
00500      MOVE 'LIFE BUSINESS PRIOR TO 01/01/81 ' TO FIA-LINE-NAME.    EL531
00501      MOVE P-1101-ISSUE-PRM-L  TO FIA-ISSUE-PRM.                   EL531
00502      MOVE P-1101-CANCEL-PRM-L TO FIA-CANCEL-PRM.                  EL531
00503      MOVE P-1101-CNT-L        TO FIA-CNT.                         EL531
00504      MOVE FIA-DETAIL          TO P-DATA.                          EL531
00505      MOVE '0'                 TO X.                               EL531
00506      PERFORM 3600-PRINT-RTN   THRU 3600-PRINT-EXIT.               EL531
00507                                                                   EL531
00508      MOVE 'LIFE BUSINESS ON/AFTER 01/01/81 ' TO FIA-LINE-NAME.    EL531
00509      MOVE A-1101-ISSUE-PRM-L TO FIA-ISSUE-PRM.                    EL531
00510      MOVE A-1101-CANCEL-PRM-L TO FIA-CANCEL-PRM.                  EL531
00511      MOVE A-1101-CNT-L        TO FIA-CNT.                         EL531
00512      MOVE FIA-DETAIL          TO P-DATA.                          EL531
00513      MOVE '0'                 TO X.                               EL531
00514      PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT.                 EL531
00515                                                                   EL531
00516      MOVE 'A/H BUSINESS PRIOR TO 01/01/81 ' TO FIA-LINE-NAME.     EL531
00517      MOVE P-1101-ISSUE-PRM-A  TO FIA-ISSUE-PRM.                   EL531
00518      MOVE P-1101-CANCEL-PRM-A TO FIA-CANCEL-PRM.                  EL531
00519      MOVE P-1101-CNT-A        TO FIA-CNT.                         EL531
00520      MOVE FIA-DETAIL          TO P-DATA.                          EL531
00521      MOVE '0'                 TO X.                               EL531
00522      PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT.                 EL531
00523                                                                   EL531
00524      MOVE 'A/H BUSINESS ON/AFTER 01/01/81 ' TO FIA-LINE-NAME.     EL531
00525      MOVE A-1101-ISSUE-PRM-A  TO FIA-ISSUE-PRM.                   EL531
00526      MOVE A-1101-CANCEL-PRM-A TO FIA-CANCEL-PRM.                  EL531
00527      MOVE A-1101-CNT-A        TO FIA-CNT.                         EL531
00528      MOVE FIA-DETAIL          TO P-DATA.                          EL531
00529      MOVE '0'                 TO X.                               EL531
00530      PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT.                 EL531
00531                                                                   EL531
00532      GO TO 9900-END-OF-JOB.                                       EL531
00533                                                                   EL531
00534      EJECT                                                        EL531
00535                                                                   EL531
00536  1000-SECTION   SECTION.                                          EL531
00537                                                                   EL531
00538  1000-READ-CONFIRMATIONS.                                         EL531
00539      OPEN I-O    ERPNDB                                           EL531
00540                  ERPYAJ                                           EL531
00541                  ELCNTL                                           EL531
00542           INPUT  ERACCT                                           EL531
00543           OUTPUT PRNTR.                                           EL531
00544                                                                   EL531
00545      IF ERACCT-FILE-STATUS  = '00' OR '97'                        EL531
00546          NEXT SENTENCE                                            EL531
00547        ELSE                                                       EL531
00548          MOVE 'ERROR OCCURED OPENING ERACCT' TO WS-ABEND-MESSAGE  EL531
00549          MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL531
00550          GO TO ABEND-PGM.                                         EL531
00551                                                                   EL531
00552      IF ERPNDB-FILE-STATUS  = '00' OR '97'                        EL531
00553          NEXT SENTENCE                                            EL531
00554        ELSE                                                       EL531
00555          MOVE 'ERROR OCCURED OPENING ERPNDB' TO WS-ABEND-MESSAGE  EL531
00556          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL531
00557          GO TO ABEND-PGM.                                         EL531
00558                                                                   EL531
00559      IF ERPYAJ-FILE-STATUS  = '00' OR '97'                        EL531
00560          NEXT SENTENCE                                            EL531
00561        ELSE                                                       EL531
00562          MOVE 'ERROR OCCURED OPENING ERPYAJ' TO WS-ABEND-MESSAGE  EL531
00563          MOVE ERPYAJ-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL531
00564          GO TO ABEND-PGM.                                         EL531
00565                                                                   EL531
00566      IF ELCNTL-FILE-STATUS  = '00' OR '97'                        EL531
00567          NEXT SENTENCE                                            EL531
00568        ELSE                                                       EL531
00569          MOVE 'ERROR OCCURED OPENING ELCNTL' TO WS-ABEND-MESSAGE  EL531
00570          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL531
00571          GO TO ABEND-PGM.                                         EL531
00572                                                                   EL531
00573      MOVE LOW-VALUES              TO WS-PB-CONTROL-PRIMARY.       EL531
00574      MOVE DTE-CLASIC-COMPANY-CD   TO WS-PB-COMPANY-CD.            EL531
00575      MOVE WS-PB-CONTROL-PRIMARY   TO PB-CONTROL-PRIMARY.          EL531
00576                                                                   EL531
00577      START ERPNDB                                                 EL531
00578          KEY NOT LESS PB-CONTROL-PRIMARY.                         EL531
00579                                                                   EL531
00580      IF ERPNDB-FILE-STATUS = '23' OR '10'                         EL531
00581          MOVE 'DURING START COMMAND' TO RE-MESSAGE                EL531
00582          GO TO 1999-READ-EXIT.                                    EL531
00583                                                                   EL531
00584      IF ERPNDB-FILE-STATUS NOT = ZERO                             EL531
00585          MOVE 'ERROR OCCURED IN OPENING FILE' TO WS-ABEND-MESSAGE EL531
00586          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL531
00587          GO TO ABEND-PGM.                                         EL531
00588                                                                   EL531
00589  1100-READ-CONF.                                                  EL531
00590      READ ERPNDB NEXT RECORD AT END                               EL531
00591          MOVE 'END-OF-FILE - NO DATA' TO RE-MESSAGE               EL531
00592          GO TO 1999-READ-EXIT.                                    EL531
00593                                                                   EL531
00594      IF ERPNDB-FILE-STATUS = '10'                                 EL531
00595          MOVE 'CRPNDB FILE-STATUS = 10' TO RE-MESSAGE             EL531
00596          GO TO 1999-READ-EXIT.                                    EL531
00597                                                                   EL531
00598      IF ERPNDB-FILE-STATUS NOT = ZERO                             EL531
00599          MOVE 'ERROR OCCURED READING ERPNDB ' TO WS-ABEND-MESSAGE EL531
00600          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL531
00601          GO TO ABEND-PGM.                                         EL531
00602                                                                   EL531
00603      IF PB-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL531
00604          MOVE 'NO MATCH - COMPANY CODE' TO RE-MESSAGE             EL531
00605          GO TO 1999-READ-EXIT.                                    EL531
00606                                                                   EL531
00607      IF PB-CREDIT-ACCEPT-DT = LOW-VALUES OR SPACES                EL531
00608          NEXT SENTENCE                                            EL531
00609       ELSE                                                        EL531
00610          GO TO 1100-READ-CONF.                                    EL531
00611                                                                   EL531
00612      IF PB-ALT-CHG-SEQ-NO NOT = ZEROS                             EL531
00613          GO TO 1100-READ-CONF.                                    EL531
00614                                                                   EL531
00615      IF PB-REIN-ONLY-CERT                                         EL531
00616         OR PB-POLICY-IS-DECLINED                                  EL531
00617         OR PB-POLICY-IS-VOIDED                                    EL531
00618          GO TO 1100-READ-CONF.                                    EL531
00619                                                                   EL531
00620      IF DTE-CLIENT = 'FIA'                                        EL531
00621          IF PB-SV-CARRIER = '2' OR '4'                            EL531
00622             GO TO 1100-READ-CONF.                                 EL531
00623                                                                   EL531
00624      IF DTE-CLIENT = 'FIA'                                        EL531
00625          IF PB-ISSUE                                              EL531
00626              IF (PB-I-LF-BENEFIT-CD  EQUAL '09'                   EL531
00627                OR PB-I-AH-BENEFIT-CD EQUAL '09')                  EL531
00628                  DISPLAY 'BYPASSED CODE 09 ' PB-CONTROL-PRIMARY   EL531
00629                   GO TO 1100-READ-CONF.                           EL531
00630                                                                   EL531
00631      IF DTE-CLIENT = 'FIA'                                        EL531
00632          IF PB-CANCELLATION                                       EL531
00633              IF (PB-CI-LF-BENEFIT-CD  EQUAL '09'                  EL531
00634                OR PB-CI-AH-BENEFIT-CD EQUAL '09')                 EL531
00635                 DISPLAY 'BYPASSED CODE 09 ' PB-CONTROL-PRIMARY    EL531
00636                   GO TO 1100-READ-CONF.                           EL531
00637                                                                   EL531
00638      IF PB-ISSUE OR PB-CANCELLATION                               EL531
00639          NEXT SENTENCE                                            EL531
00640       ELSE                                                        EL531
00641          GO TO 1100-READ-CONF.                                    EL531
00642                                                                   EL531
00643       IF DTE-TOT-OPT = '3'                                        EL531
00644           MOVE '1'  TO DTE-PRC-OPT.                               EL531
00645                                                                   EL531
00646       IF DTE-PRC-OPT = '4'                                        EL531
00647          IF ((PB-INPUT-DT NOT LESS THAN SAVE-POST-DATE-BEG)       EL531
00648           AND (PB-INPUT-DT NOT GREATER THAN SAVE-POST-DATE-END))  EL531
00649             NEXT SENTENCE                                         EL531
00650           ELSE                                                    EL531
00651             GO TO 1100-READ-CONF.                                 EL531
00652                                                                   EL531
00653       IF DTE-PRC-OPT = '3'                                        EL531
00654          IF PB-CONFIRMATION-REPT-DT = SAVE-POST-DATE              EL531
00655             MOVE '3'  TO DTE-TOT-OPT.                             EL531
00656                                                                   EL531
00657       IF DTE-TOT-OPT = '1'                                        EL531
00658         IF PB-FATAL-ERRORS                                        EL531
00659           OR PB-UNFORCED-ERRORS                                   EL531
00660             GO TO 1100-READ-CONF.                                 EL531
00661                                                                   EL531
00662       IF DTE-TOT-OPT = '1'                                        EL531
00663          IF (PB-CONFIRMATION-REPT-DT = LOW-VALUE OR SPACES)       EL531
00664              NEXT SENTENCE                                        EL531
00665           ELSE                                                    EL531
00666              GO TO 1100-READ-CONF.                                EL531
00667                                                                   EL531
00668       IF DTE-TOT-OPT = '2'                                        EL531
00669          IF  PB-FATAL-ERRORS                                      EL531
00670           OR PB-UNFORCED-ERRORS                                   EL531
00671             GO TO 1100-READ-CONF.                                 EL531
00672                                                                   EL531
00673      IF PB-ISSUE                                                  EL531
00674          IF PB-I-NO-OF-PAYMENTS NOT NUMERIC                       EL531
00675              MOVE ZERO TO PB-I-NO-OF-PAYMENTS.                    EL531
00676                                                                   EL531
00677      IF PB-ISSUE                                                  EL531
00678          IF PB-I-NO-OF-PAYMENTS = ZERO                            EL531
00679              MOVE PB-I-LOAN-TERM TO PB-I-NO-OF-PAYMENTS.          EL531
00680                                                                   EL531
00681      IF PB-ISSUE                                                  EL531
00682          IF PB-I-SKIP-CODE NOT NUMERIC                            EL531
00683              MOVE ZERO           TO PB-I-SKIP-CODE.               EL531
00684                                                                   EL531
00685      MOVE PB-CARRIER             TO AC-CARRIER.                   EL531
00686      MOVE PB-GROUPING            TO AC-GROUPING.                  EL531
00687      MOVE PB-STATE               TO AC-STATE.                     EL531
00688      IF PB-SV-CARRIER NOT = SPACES                                EL531
00689          MOVE PB-SV-CARRIER      TO AC-CARRIER.                   EL531
00690      IF PB-SV-GROUPING NOT = SPACES                               EL531
00691          MOVE PB-SV-GROUPING     TO AC-GROUPING.                  EL531
00692      IF PB-SV-STATE NOT = SPACES                                  EL531
00693          MOVE PB-SV-STATE        TO AC-STATE.                     EL531
00694                                                                   EL531
00695      MOVE PB-ACCOUNT             TO AC-ACCOUNT.                   EL531
00696      MOVE AC-CARRIER             TO AC-SAVE-CARRIER.              EL531
00697      MOVE PB-CERT-EFF-DT         TO DC-BIN-DATE-1.                EL531
00698      MOVE SPACE                  TO DC-OPTION-CODE.               EL531
00699      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   EL531
00700      MOVE DC-GREG-DATE-CYMD      TO WS-AC-CERT-EFF-DATE-N.           CL*12
00701      MOVE PB-CERT-NO             TO AC-CERTIFICATE-NO.            EL531
00702                                                                   EL531
00703      IF PB-ACCT-EXP-DT = LOW-VALUE OR SPACES OR ZERO              EL531
00704          MOVE HIGH-VALUE         TO PB-ACCT-EXP-DT.               EL531
00705                                                                   EL531
00706      MOVE PB-ACCT-EXP-DT         TO AC-ACCT-EXP-DT.               EL531
00707                                                                   EL531
00708      IF PB-ISSUE                                                  EL531
00709          MOVE 'W'                  TO AC-RECORD-TYPE              EL531
00710          MOVE PB-I-MEMBER-NO       TO AC-MEMBER-NO                EL531
00711          MOVE PB-I-INSURED-LAST-NAME TO AC-LAST-NAME              EL531
00712          MOVE PB-I-INSURED-1ST-INIT  TO AC-INITIALS               EL531
00713          MOVE PB-I-BIRTHDAY        TO DC-BIN-DATE-1               EL531
00714          MOVE SPACE                TO DC-OPTION-CODE              EL531
00715          CALL 'ELDATCX' USING DATE-CONVERSION-DATA                EL531
00716          MOVE DC-GREG-DATE-1-MDY   TO AC-DATE-OF-BIRTH            EL531
00717          MOVE PB-I-SOC-SEC-NO      TO AC-SOC-SEC-NO               EL531
00718          MOVE PB-I-INSURED-SEX     TO AC-MEMBER-SEX               EL531
00719          MOVE PB-I-AGE             TO AC-MEMBER-AGE               EL531
00720          MOVE PB-I-LF-BENEFIT-AMT  TO AC-LIFE-BENEFIT             EL531
00721          MOVE PB-I-LF-PREMIUM-AMT  TO AC-LIFE-PREMIUM             EL531
00722          MOVE PB-I-LF-BENEFIT-CD   TO AC-LIFE-TYPE                EL531
00723          MOVE ZERO                 TO AC-LIFE-REFUND              EL531
00724          MOVE PB-I-AH-BENEFIT-AMT  TO AC-AH-BENEFIT               EL531
00725          MOVE PB-I-AH-PREMIUM-AMT  TO AC-AH-PREMIUM               EL531
00726          MOVE PB-I-AH-BENEFIT-CD   TO AC-AH-TYPE                  EL531
00727          MOVE ZERO                 TO AC-AH-REFUND                EL531
00728          MOVE PB-I-SKIP-CODE       TO AC-PAY-SKIP-CODE            EL531
00729          MOVE PB-I-TERM-TYPE       TO AC-MODE-OF-PAYMENT          EL531
00730          MOVE PB-I-NO-OF-PAYMENTS  TO AC-LIFE-TERM-OR-PAYMENT     EL531
00731                                       AC-AH-TERM-OR-PAYMENT       EL531
00732                                       AC-COMPUTED-PAY-NO          EL531
00733          MOVE PB-I-LF-TERM         TO AC-LIFE-COMPUTED-TERM       EL531
00734          MOVE PB-I-AH-TERM         TO AC-AH-COMPUTED-TERM.        EL531
00735                                                                   EL531
00736      IF PB-ISSUE AND (AC-AH-BENEFIT NOT = ZERO)                   EL531
00737        IF PB-PAID-WEEKLY                                          EL531
00738          COMPUTE AC-AH-BENEFIT ROUNDED = AC-AH-BENEFIT / 4.333333 EL531
00739       ELSE                                                        EL531
00740        IF PB-PAID-SEMI-MONTHLY                                    EL531
00741          COMPUTE AC-AH-BENEFIT ROUNDED = AC-AH-BENEFIT / 2        EL531
00742       ELSE                                                        EL531
00743        IF PB-PAID-BI-WEEKLY                                       EL531
00744          COMPUTE AC-AH-BENEFIT ROUNDED = AC-AH-BENEFIT / 2.166667.EL531
00745                                                                   EL531
00746      IF PB-CANCELLATION                                           EL531
00747          MOVE 'C'                  TO AC-RECORD-TYPE              EL531
00748                                                                   EL531
00749          MOVE PB-C-LF-CANCEL-DT    TO DC-BIN-DATE-1               EL531
00750          MOVE SPACE                TO DC-OPTION-CODE              EL531
00751          CALL 'ELDATCX' USING DATE-CONVERSION-DATA                EL531
00752          MOVE DC-GREG-DATE-CYMD    TO WS-AC-LIFE-CANCEL-DATE-N       CL*11
00753                                                                   EL531
00754          MOVE PB-C-AH-CANCEL-DT    TO DC-BIN-DATE-1               EL531
00755          MOVE SPACE                TO DC-OPTION-CODE              EL531
00756          CALL 'ELDATCX' USING DATE-CONVERSION-DATA                EL531
00757          MOVE DC-GREG-DATE-CYMD    TO WS-AC-AH-CANCEL-DATE-N         CL*11
00758                                                                   EL531
00759          MOVE PB-CI-MEMBER-NO      TO AC-MEMBER-NO                EL531
00760          MOVE PB-CI-INSURED-NAME   TO AC-MEMBER-NAME              EL531
00761          MOVE ZEROS                TO AC-DATE-OF-BIRTH            EL531
00762          MOVE PB-CI-SOC-SEC-NO     TO AC-SOC-SEC-NO               EL531
00763          MOVE PB-CI-INSURED-SEX    TO AC-MEMBER-SEX               EL531
00764          MOVE PB-CI-INSURED-AGE    TO AC-MEMBER-AGE               EL531
00765          MOVE PB-CI-LF-BENEFIT-AMT TO AC-LIFE-BENEFIT             EL531
00766          MOVE PB-CI-LF-PREMIUM-AMT TO AC-LIFE-PREMIUM             EL531
00767          MOVE PB-CI-LF-BENEFIT-CD  TO AC-LIFE-TYPE                EL531
00768          MOVE PB-C-LF-CANCEL-AMT   TO AC-LIFE-REFUND              EL531
00769          MOVE PB-CI-AH-BENEFIT-AMT TO AC-AH-BENEFIT               EL531
00770          MOVE PB-CI-AH-PREMIUM-AMT TO AC-AH-PREMIUM               EL531
00771          MOVE PB-CI-AH-BENEFIT-CD  TO AC-AH-TYPE                  EL531
00772          MOVE PB-C-AH-CANCEL-AMT   TO AC-AH-REFUND                EL531
00773          MOVE 'M '                 TO AC-MODE-OF-PAYMENT          EL531
00774          MOVE ZERO                 TO AC-PAY-SKIP-CODE            EL531
00775          MOVE PB-CI-LOAN-TERM      TO AC-COMPUTED-PAY-NO          EL531
00776                                       AC-LIFE-TERM-OR-PAYMENT     EL531
00777                                       AC-AH-TERM-OR-PAYMENT       EL531
00778                                       AC-LIFE-COMPUTED-TERM       EL531
00779                                       AC-AH-COMPUTED-TERM.        EL531
00780                                                                   EL531
00781      MOVE '  '                     TO AC-ERROR-FLAG.              EL531
00782      IF PB-UNFORCED-ERRORS                                        EL531
00783          MOVE '* '                 TO AC-ERROR-FLAG.              EL531
00784      IF PB-FATAL-ERRORS                                           EL531
00785          MOVE '**'                 TO AC-ERROR-FLAG.              EL531
00786                                                                   EL531
00787      IF DTE-CLIENT = 'FIA'                                        EL531
00788          MOVE '1'   TO AC-CARRIER                                 EL531
00789          MOVE '000' TO AC-GROUPING.                               EL531
00790                                                                   EL531
00791      COPY ELCCONM2.                                                  CL**4
00792                                                                      CL**4
00793      MOVE ADDON-CONF-TRANSACTIONS TO SORT-REC.                    EL531
00794                                                                   EL531
00795      RELEASE SORT-REC.                                            EL531
00796                                                                   EL531
00797      ADD +1                        TO RECORDS-RELEASED.           EL531
00798                                                                   EL531
00799      IF DTE-PRC-OPT = '3'                                         EL531
00800         MOVE LOW-VALUE             TO PB-CONFIRMATION-REPT-DT     EL531
00801        ELSE                                                       EL531
00802         IF DTE-PRC-OPT = '2'                                      EL531
00803            MOVE SAVE-POST-DATE     TO PB-CONFIRMATION-REPT-DT.    EL531
00804                                                                   EL531
00805      REWRITE PENDING-BUSINESS.                                    EL531
00806                                                                   EL531
00807      IF ERPNDB-FILE-STATUS NOT = '00'                             EL531
00808          MOVE 'ERROR OCCURED IN WRITING FILE' TO WS-ABEND-MESSAGE EL531
00809          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL531
00810          GO TO ABEND-PGM.                                         EL531
00811                                                                   EL531
00812      GO TO 1100-READ-CONF.                                        EL531
00813                                                                   EL531
00814  1999-READ-EXIT.                                                  EL531
00815      EXIT.                                                        EL531
00816      EJECT                                                        EL531
00817  3000-PRINT-SECTION  SECTION.                                     EL531
00818                                                                   EL531
00819      IF RECORDS-RELEASED EQUAL +0                                 EL531
00820          PERFORM 3650-HEADING                                     EL531
00821          MOVE '0'                       TO X                      EL531
00822          MOVE 'NO RECORDS SELECTED    ' TO RE-MESSAGE             EL531
00823          MOVE RPT-ENDED                 TO P-DATA                 EL531
00824          PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT              EL531
00825          DISPLAY 'NO RECORDS SELECTED '                           EL531
00826          GO TO 3900-RETURN-EXIT.                                  EL531
00827                                                                   EL531
00828  3100-RETURN-CONF.                                                EL531
00829      RETURN SORT-FILE RECORD INTO ADDON-CONF-TRANSACTIONS         EL531
00830          AT END PERFORM 3500-TOTAL-ACCT THRU 3599-EXIT            EL531
00831                 PERFORM 5000-GENERATE-CHECK THRU 5999-EXIT        EL531
00832                 GO TO 3900-RETURN-EXIT.                           EL531
00833                                                                   EL531
00834      COPY ELCCONM1.                                                  CL**4
00835                                                                      CL**4
00836      MOVE AC-CARRIER       TO B-CAR.                              EL531
00837      MOVE AC-GROUPING      TO B-GROUPING.                         EL531
00838      MOVE AC-STATE         TO B-STATE.                            EL531
00839      MOVE AC-ACCOUNT       TO B-ACCT.                             EL531
00840      MOVE AC-RECORD-TYPE   TO B-TYPE.                             EL531
00841                                                                   EL531
00842      IF LCP-ONCTR-01 =  0                                         EL531
00843          ADD 1 TO LCP-ONCTR-01                                    EL531
00844                                MOVE BUILD-CONTROL TO SAVE-CONTROL EL531
00845           MOVE AC-ACCT-EXP-DT    TO SAVE-ACCT-EXP-DT              EL531
00846           MOVE AC-SAVE-CARRIER   TO SAVE-ACCT-CARRIER.            EL531
00847                                                                   EL531
00848      IF BUILD-CONTROL NOT = SAVE-CONTROL                          EL531
00849          PERFORM 3500-TOTAL-ACCT THRU 3599-EXIT                   EL531
00850          PERFORM 5000-GENERATE-CHECK THRU 5999-EXIT               EL531
00851          MOVE AC-ACCT-EXP-DT  TO SAVE-ACCT-EXP-DT                 EL531
00852          MOVE AC-SAVE-CARRIER TO SAVE-ACCT-CARRIER                EL531
00853          MOVE BUILD-CONTROL   TO SAVE-CONTROL.                    EL531
00854                                                                   EL531
00855  3200-PRINT-DETAIL.                                               EL531
00856      IF LINE-CNT GREATER THAN PAGE-SIZE                           EL531
00857          PERFORM  3650-HEADING THRU 3699-EXIT.                    EL531
00858                                                                   EL531
00859      MOVE SPACES             TO D-CANCEL-DATE.                    EL531
00860      MOVE AC-MEMBER-NO       TO D-MEMBER.                         EL531
00861      MOVE AC-INITIALS        TO D-INITIAL.                        EL531
00862      MOVE AC-LAST-NAME       TO D-LAST-NAME.                      EL531
00863      MOVE AC-CERTIFICATE-NO  TO D-CERT-NO.                        EL531
00864      MOVE AC-CERT-YR         TO D-EFF-YR.                         EL531
00865      MOVE AC-CERT-MO         TO D-EFF-MO.                         EL531
00866      MOVE AC-CERT-DA         TO D-EFF-DAY.                        EL531
00867      MOVE AC-MEMBER-AGE      TO D-ISS-AGE.                        EL531
00868      MOVE SPACES             TO D-PMT-FRQ.                        EL531
00869      MOVE AC-ERROR-FLAG      TO D-FLAG.                           EL531
00870                                                                   EL531
00871      MOVE 'M '               TO D-PMT-FRQ.                        EL531
00872      IF PAY-SEMIMONTHLY                                           EL531
00873          MOVE 'SM '          TO D-PMT-FRQ.                        EL531
00874      IF PAY-WEEKLY                                                EL531
00875          MOVE 'W '           TO D-PMT-FRQ.                        EL531
00876      IF PAY-BIWEEKLY                                              EL531
00877          MOVE 'BW '          TO D-PMT-FRQ.                        EL531
00878                                                                   EL531
00879      PERFORM 3300-LIFE-TYPE THRU 3399-EXIT.                       EL531
00880                                                                   EL531
00881      IF AC-WRITTEN                                                EL531
00882          MOVE AC-LIFE-BENEFIT       TO D-LIFE-PREM                EL531
00883          MOVE AC-LIFE-PREMIUM       TO D-LIFE-REFUND              EL531
00884          MOVE AC-COMPUTED-PAY-NO    TO D-NO-PMT                   EL531
00885          MOVE AC-PAY-SKIP-CODE      TO D-SKIP-CODE                EL531
00886          MOVE AC-LIFE-COMPUTED-TERM TO D-TERM                     EL531
00887       ELSE                                                        EL531
00888          MOVE AC-LIFE-PREMIUM       TO D-LIFE-PREM                EL531
00889          MOVE AC-LIFE-REFUND        TO D-LIFE-REFUND              EL531
00890          MOVE SPACE                 TO D-PMT-FRQ                  EL531
00891          MOVE ZERO                  TO D-NO-PMT                   EL531
00892                                        D-SKIP-CODE                EL531
00893                                        D-TERM.                    EL531
00894                                                                   EL531
00895      PERFORM 3400-DISB-TYPE THRU 3499-EXIT.                       EL531
00896                                                                   EL531
00897      IF AC-WRITTEN                                                EL531
00898          MOVE AC-AH-BENEFIT       TO D-DISB-PREM                  EL531
00899          MOVE AC-AH-PREMIUM       TO D-DISB-REFUND                EL531
00900          MOVE AC-AH-COMPUTED-TERM TO D-TERM                       EL531
00901        ELSE                                                       EL531
00902          MOVE AC-AH-PREMIUM       TO D-DISB-PREM                  EL531
00903          MOVE AC-AH-REFUND        TO D-DISB-REFUND.               EL531
00904                                                                   EL531
00905      IF AC-CANCELLATION                                           EL531
00906         IF AC-AH-TYPE EQUAL '  '  OR '00'                         EL531
00907             MOVE AC-LIFE-CANCEL-MO  TO D-CANC-MO                  EL531
00908             MOVE AC-LIFE-CANCEL-DA  TO D-CANC-DA                  EL531
00909             MOVE AC-LIFE-CANCEL-YR  TO D-CANC-YR                  EL531
00910             INSPECT D-CANC-DATE CONVERTING SPACES TO '-'          EL531
00911         ELSE                                                      EL531
00912             MOVE AC-AH-CANCEL-MO    TO D-CANC-MO                  EL531
00913             MOVE AC-AH-CANCEL-DA    TO D-CANC-DA                  EL531
00914             MOVE AC-AH-CANCEL-YR    TO D-CANC-YR                  EL531
00915             INSPECT D-CANC-DATE CONVERTING SPACES TO '-'.         EL531
00916                                                                   EL531
00917      MOVE DETAIL-LINE TO P-DATA.                                  EL531
00918      MOVE SPACE       TO X.                                       EL531
00919      PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT.                 EL531
00920                                                                   EL531
00921      IF AC-WRITTEN                                                EL531
00922          ADD AC-LIFE-BENEFIT      TO T-LIFE-PREMIUM               EL531
00923          ADD AC-LIFE-PREMIUM      TO T-LIFE-REFUND                EL531
00924          ADD AC-AH-BENEFIT        TO T-DISB-PREMIUM               EL531
00925          ADD AC-AH-PREMIUM        TO T-DISB-REFUND                EL531
00926          ADD AC-AH-COMPUTED-TERM  TO T-TERM                       EL531
00927          ADD AC-MEMBER-AGE        TO T-AGE                        EL531
00928       ELSE                                                        EL531
00929          ADD AC-LIFE-PREMIUM      TO T-LIFE-PREMIUM               EL531
00930          ADD AC-LIFE-REFUND       TO T-LIFE-REFUND                EL531
00931          ADD AC-AH-PREMIUM        TO T-DISB-PREMIUM               EL531
00932          ADD AC-AH-REFUND         TO T-DISB-REFUND.               EL531
00933                                                                   EL531
00934      ADD 1 TO LINE-CNT T-CERTS.                                   EL531
00935                                                                   EL531
00936      IF AC-LIFE-BENEFIT GREATER ZEROS                             EL531
00937          ADD 1 TO T-LIFE-CERTS.                                   EL531
00938                                                                   EL531
00939      IF AC-AH-BENEFIT GREATER ZEROS                               EL531
00940          ADD 1 TO T-AH-CERTS.                                     EL531
00941                                                                   EL531
00942 ****************************************************************  EL531
00943 *    SPECIAL TOTALS FOR FIA                                       EL531
00944 ***************************************************************   EL531
00945      IF DTE-CLIENT NOT = 'FIA'                                    EL531
00946          GO TO 3100-RETURN-CONF.                                  EL531
00947                                                                   EL531
00948      IF NOT AC-CANCELLATION                                       EL531
00949          GO TO 3100-RETURN-CONF.                                  EL531
00950                                                                   EL531
00951      IF AC-CERT-EFF-DATE LESS 19810101                            EL531
00952          ADD AC-LIFE-PREMIUM TO P-1101-ISSUE-PRM-L                EL531
00953          ADD AC-AH-PREMIUM   TO P-1101-ISSUE-PRM-A                EL531
00954          ADD AC-AH-REFUND    TO P-1101-CANCEL-PRM-A               EL531
00955          ADD AC-LIFE-REFUND  TO P-1101-CANCEL-PRM-L               EL531
00956      ELSE                                                         EL531
00957          ADD AC-LIFE-PREMIUM TO A-1101-ISSUE-PRM-L                EL531
00958          ADD AC-AH-PREMIUM   TO A-1101-ISSUE-PRM-A                EL531
00959          ADD AC-AH-REFUND    TO A-1101-CANCEL-PRM-A               EL531
00960          ADD AC-LIFE-REFUND  TO A-1101-CANCEL-PRM-L.              EL531
00961                                                                   EL531
00962      IF AC-AH-REFUND NOT = ZERO                                   EL531
00963          IF AC-CERT-EFF-DATE LESS 19810101                        EL531
00964              ADD 1 TO P-1101-CNT-A                                EL531
00965           ELSE                                                    EL531
00966              ADD 1 TO A-1101-CNT-A.                               EL531
00967                                                                   EL531
00968      IF AC-LIFE-REFUND NOT = ZERO                                 EL531
00969          IF AC-CERT-EFF-DATE LESS 19810101                        EL531
00970              ADD 1 TO P-1101-CNT-L                                EL531
00971           ELSE                                                    EL531
00972              ADD 1 TO A-1101-CNT-L.                               EL531
00973                                                                   EL531
00974      GO TO 3100-RETURN-CONF.                                      EL531
00975                                                                   EL531
00976  3300-LIFE-TYPE.                                                  EL531
00977      IF AC-LIFE-TYPE = ZEROS                                      EL531
00978         GO TO 3390-NO-LIFE-TYPE.                                  EL531
00979                                                                   EL531
00980      MOVE AC-LIFE-TYPE TO CLAS-LOOK.                              EL531
00981      MOVE CLAS-STARTL  TO CLAS-INDEXL.                            EL531
00982                                                                   EL531
00983      IF CLAS-STARTL = CLAS-MAXL                                   EL531
00984         GO TO 3390-NO-LIFE-TYPE.                                  EL531
00985                                                                   EL531
00986  3320-LIFE-LOOKUP.                                                EL531
00987      IF CLAS-INDEXL GREATER CLAS-MAXL                             EL531
00988          GO TO 3390-NO-LIFE-TYPE.                                 EL531
00989                                                                   EL531
00990      IF CLAS-I-BEN (CLAS-INDEXL) = CLAS-LOOK                      EL531
00991          MOVE CLAS-I-AB3 (CLAS-INDEXL) TO D-LIFE-TYPE             EL531
00992          GO TO 3399-EXIT.                                         EL531
00993                                                                   EL531
00994      ADD 1 TO CLAS-INDEXL.                                        EL531
00995                                                                   EL531
00996      GO TO 3320-LIFE-LOOKUP.                                      EL531
00997                                                                   EL531
00998  3390-NO-LIFE-TYPE.                                               EL531
00999      MOVE SPACES TO D-LIFE-TYPE.                                  EL531
01000                                                                   EL531
01001  3399-EXIT.                                                       EL531
01002      EXIT.                                                        EL531
01003  3400-DISB-TYPE.                                                  EL531
01004      IF AC-AH-TYPE = ZERO                                         EL531
01005         GO TO 3490-NO-DISB-TYPE.                                  EL531
01006                                                                   EL531
01007      MOVE AC-AH-TYPE  TO CLAS-LOOK.                               EL531
01008      MOVE CLAS-STARTA TO CLAS-INDEXA.                             EL531
01009                                                                   EL531
01010      IF CLAS-STARTA = CLAS-MAXA                                   EL531
01011         GO TO 3490-NO-DISB-TYPE.                                  EL531
01012                                                                   EL531
01013  3420-DISB-LOOKUP.                                                EL531
01014      IF CLAS-INDEXA GREATER CLAS-MAXA                             EL531
01015          GO TO 3490-NO-DISB-TYPE.                                 EL531
01016                                                                   EL531
01017      IF CLAS-I-BEN (CLAS-INDEXA) = CLAS-LOOK                      EL531
01018          MOVE CLAS-I-AB3 (CLAS-INDEXA) TO D-DISB-TYPE             EL531
01019          GO TO 3499-EXIT.                                         EL531
01020                                                                   EL531
01021      ADD 1 TO CLAS-INDEXA.                                        EL531
01022                                                                   EL531
01023      GO TO 3420-DISB-LOOKUP.                                      EL531
01024                                                                   EL531
01025  3490-NO-DISB-TYPE.                                               EL531
01026      MOVE SPACES TO D-DISB-TYPE.                                  EL531
01027                                                                   EL531
01028  3499-EXIT.                                                       EL531
01029      EXIT.                                                        EL531
01030                                                                   EL531
01031  3500-TOTAL-ACCT.                                                 EL531
01032      MOVE ZEROS              TO TL-LIFE-PREM-R                    EL531
01033                                 TL-DISB-PREM-R.                   EL531
01034      MOVE T-LIFE-REFUND      TO TL-LIFE-REFUND.                   EL531
01035      MOVE T-DISB-REFUND      TO TL-DISB-REFUND.                   EL531
01036      MOVE TOTAL-LINE         TO P-DATA.                           EL531
01037      MOVE '0'                TO X.                                EL531
01038      PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT.                 EL531
01039                                                                   EL531
01040      IF T-LIFE-CERTS = ZERO                                       EL531
01041          MOVE 1 TO T-LIFE-CERTS.                                  EL531
01042                                                                   EL531
01043      IF T-AH-CERTS = ZERO                                         EL531
01044          MOVE 1 TO T-AH-CERTS.                                    EL531
01045                                                                   EL531
01046      IF S-TYPE = 'W'                                              EL531
01047          COMPUTE ISSUE-AGE   ROUNDED = T-AGE / T-CERTS            EL531
01048          COMPUTE ISSUE-TERM  ROUNDED = T-TERM / T-CERTS           EL531
01049          COMPUTE ISSUE-L-BEN ROUNDED =                            EL531
01050                          T-LIFE-PREMIUM / T-LIFE-CERTS            EL531
01051          COMPUTE ISSUE-D-BEN ROUNDED =                            EL531
01052                          T-DISB-PREMIUM / T-AH-CERTS              EL531
01053          COMPUTE ISSUE-AVERAGE ROUNDED =                          EL531
01054              (T-LIFE-REFUND + T-DISB-REFUND) / T-CERTS            EL531
01055          MOVE T-CERTS     TO ISSUE-CERTS                          EL531
01056          MOVE ISSUE-LINE1 TO P-DATA                               EL531
01057          PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT              EL531
01058          MOVE SPACE       TO X                                    EL531
01059          MOVE ISSUE-LINE2 TO P-DATA                               EL531
01060          PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT              EL531
01061          MOVE ISSUE-LINE3 TO P-DATA                               EL531
01062          PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT              EL531
01063          MOVE ISSUE-LINE4 TO P-DATA                               EL531
01064          PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT              EL531
01065          MOVE ISSUE-LINE5 TO P-DATA                               EL531
01066          PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT.             EL531
01067                                                                   EL531
01068      IF ((S-TYPE = 'C')                                           EL531
01069          AND (T-LIFE-PREMIUM + T-DISB-PREMIUM NOT = ZERO))        EL531
01070           MOVE T-CERTS TO CANC-CERTS                              EL531
01071           COMPUTE CANC-PERCENT ROUNDED = ((T-LIFE-REFUND +        EL531
01072               T-DISB-REFUND) / (T-LIFE-PREMIUM                    EL531
01073               + T-DISB-PREMIUM)) * 100                            EL531
01074           MOVE CANCEL-LINE TO P-DATA                              EL531
01075           PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT.            EL531
01076                                                                   EL531
01077      COMPUTE WS-CHECK-AMT = T-LIFE-REFUND + T-DISB-REFUND.        EL531
01078                                                                   EL531
01079      IF DTE-PRC-OPT NOT = '2'                                     EL531
01080         NEXT SENTENCE                                             EL531
01081      ELSE                                                         EL531
01082         IF S-TYPE = 'C'                                           EL531
01083            MOVE WS-CHECK-AMT TO TL-CHK-AMOUNT                     EL531
01084            MOVE TOTAL-LINE2  TO P-DATA                            EL531
01085            MOVE '0'          TO X                                 EL531
01086            PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT.           EL531
01087                                                                   EL531
01088      MOVE ZERO TO T-LIFE-PREMIUM T-LIFE-REFUND                    EL531
01089                   T-DISB-PREMIUM T-DISB-REFUND                    EL531
01090                   T-CERTS        T-TERM   T-AGE                   EL531
01091                   T-LIFE-CERTS   T-AH-CERTS.                      EL531
01092                                                                   EL531
01093      COMPUTE LINE-CNT = PAGE-SIZE + 1.                            EL531
01094                                                                   EL531
01095      IF B-ACCT NOT = S-ACCT                                       EL531
01096          MOVE ZEROS TO PAGE-CNT.                                  EL531
01097                                                                   EL531
01098  3599-EXIT.                                                       EL531
01099      EXIT.                                                        EL531
01100  3600-PRINT-RTN.                                                  EL531
01101              COPY ELCPRT2X.                                       EL531
01102                                                                   EL531
01103  3600-PRINT-EXIT.                                                 EL531
01104      EXIT.                                                        EL531
01105  3650-HEADING.                                                    EL531
01106      MOVE '1'      TO X.                                          EL531
01107      MOVE ZEROS    TO LINE-CNT.                                   EL531
01108      ADD 1 TO PAGE-CNT.                                           EL531
01109      MOVE PAGE-CNT TO HD-PAGE.                                    EL531
01110      MOVE HEAD-1   TO P-DATA.                                     EL531
01111      PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT.                 EL531
01112      MOVE SPACE    TO X.                                          EL531
01113      MOVE HEAD-2   TO P-DATA.                                     EL531
01114      PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT.                 EL531
01115      MOVE HEAD-3   TO P-DATA.                                     EL531
01116      PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT.                 EL531
01117                                                                   EL531
01118  3650-EXIT.                                                       EL531
01119      EXIT.                                                        EL531
01120                                                                   EL531
01121  3650-CONTINUE.                                                   EL531
01122      MOVE '0'    TO X.                                            EL531
01123      MOVE S-ACCT TO HD-ACCT-NO.                                   EL531
01124                                                                   EL531
01125  3660-GET-ACCT-NAME.                                              EL531
01126      MOVE LOW-VALUES            TO AM-CONTROL-PRIMARY.            EL531
01127      MOVE DTE-CLASIC-COMPANY-CD TO AM-COMPANY-CD.                 EL531
01128      MOVE S-ACCT                TO AM-ACCOUNT.                    EL531
01129                                                                   EL531
01130      START ERACCT   KEY NOT LESS THAN AM-CONTROL-PRIMARY.         EL531
01131                                                                   EL531
01132      IF ERACCT-FILE-STATUS = '23'                                 EL531
01133          GO TO 3680-NO-NAME.                                      EL531
01134                                                                   EL531
01135      IF ERACCT-FILE-STATUS-1 NOT = ZERO                           EL531
01136          MOVE ERACCT-FILE-STATUS          TO  WS-ABEND-FILE-STATUSEL531
01137          MOVE 'ERROR OCCURED START - ERACCT'                      EL531
01138                                  TO  WS-ABEND-MESSAGE             EL531
01139          GO TO ABEND-PGM.                                         EL531
01140      EJECT                                                        EL531
01141  3665-READ-ERACCT.                                                EL531
01142      READ ERACCT  NEXT RECORD.                                    EL531
01143                                                                   EL531
01144      IF ERACCT-FILE-STATUS-1  = '1'                               EL531
01145          GO TO 3680-NO-NAME.                                      EL531
01146                                                                   EL531
01147      IF ERACCT-FILE-STATUS-1 NOT = ZERO                           EL531
01148          MOVE ERACCT-FILE-STATUS          TO  WS-ABEND-FILE-STATUSEL531
01149          MOVE 'ERROR OCCURED READ - ERACCT'                       EL531
01150                                  TO  WS-ABEND-MESSAGE             EL531
01151          GO TO ABEND-PGM.                                         EL531
01152                                                                   EL531
01153      IF AM-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL531
01154          GO TO 3680-NO-NAME.                                      EL531
01155                                                                   EL531
01156      IF S-ACCT NOT =  AM-ACCOUNT                                  EL531
01157          GO TO 3665-READ-ERACCT.                                  EL531
01158                                                                   EL531
01159      MOVE AM-NAME             TO HD-NAME1.                        EL531
01160      MOVE HEAD-4              TO P-DATA.                          EL531
01161      PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT.                 EL531
01162      MOVE SPACE               TO X.                               EL531
01163      MOVE HEAD-5              TO P-DATA.                          EL531
01164      PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT.                 EL531
01165      ADD 1                    TO LINE-CNT.                        EL531
01166                                                                   EL531
01167      MOVE S-CAR-CO  TO HD-CAR-CO.                                 EL531
01168      MOVE S-STATE   TO STATE-L.                                   EL531
01169      PERFORM 3700-STATE-LOOKUP THRU 3700-EXIT.                    EL531
01170      MOVE HEAD-6    TO P-DATA.                                    EL531
01171      MOVE '0'       TO X.                                         EL531
01172      PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT.                 EL531
01173      MOVE SPACES    TO HD-NET HD-ADDON.                           EL531
01174                                                                   EL531
01175      IF S-TYPE = 'C'                                              EL531
01176          MOVE '*** CANCELLATION ***' TO HD-REPORT                 EL531
01177          MOVE '    CANCEL'           TO HD-NET                    EL531
01178          MOVE '     DATE'            TO HD-ADDON                  EL531
01179          MOVE SPACES                 TO HEAD-1-REP  HEAD-2-REP    EL531
01180       ELSE                                                        EL531
01181          MOVE 'PMT NO. SK TRM  '     TO HEAD-1-REP                EL531
01182          MOVE 'FRQ PMT CD'           TO HEAD-2-REP                EL531
01183          MOVE '*****  ISSUES  *****' TO HD-REPORT.                EL531
01184                                                                   EL531
01185      MOVE HEAD-7 TO P-DATA.                                       EL531
01186      PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT.                 EL531
01187      MOVE HEAD-DETAIL-1 TO P-DATA.                                EL531
01188      PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT.                 EL531
01189      MOVE SPACE TO X.                                             EL531
01190                                                                   EL531
01191      IF S-TYPE = 'W'                                              EL531
01192          MOVE HEAD-DETAIL-2A TO P-DATA                            EL531
01193       ELSE                                                        EL531
01194          MOVE HEAD-DETAIL-2 TO P-DATA.                            EL531
01195                                                                   EL531
01196      PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT.                 EL531
01197                                                                   EL531
01198      MOVE SPACES TO P-DATA.                                       EL531
01199      PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT.                 EL531
01200      ADD 11 TO LINE-CNT.                                          EL531
01201      GO TO 3699-EXIT.                                             EL531
01202                                                                   EL531
01203  3680-NO-NAME.                                                    EL531
01204      MOVE '****************** ' TO HD-NAME1.                      EL531
01205      MOVE HEAD-4 TO P-DATA.                                       EL531
01206      PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT.                 EL531
01207                                                                   EL531
01208  3699-EXIT.                                                       EL531
01209      EXIT.                                                        EL531
01210                                                                   EL531
01211  3700-STATE-LOOKUP.                                               EL531
01212  STATE--START.                                                    EL531
01213      MOVE ZERO TO CLAS-INDEXS.                                    EL531
01214                                                                   EL531
01215  STATE--LOOP.                                                     EL531
01216      ADD 1 TO CLAS-INDEXS.                                        EL531
01217      IF CLAS-INDEXS GREATER CLAS-MAXS                             EL531
01218          MOVE SPACE TO STATE-L                                    EL531
01219          GO TO STATE--EXIT.                                       EL531
01220                                                                   EL531
01221      IF STATE-L NOT = STATE-SUB (CLAS-INDEXS) AND                 EL531
01222                      STATE-ABBR (CLAS-INDEXS)                     EL531
01223          GO TO STATE--LOOP.                                       EL531
01224                                                                   EL531
01225  STATE--EXIT.                                                     EL531
01226      IF STATE-L = SPACES                                          EL531
01227          MOVE SPACES TO HD-STATE                                  EL531
01228        ELSE                                                       EL531
01229          MOVE STATE-PIC (CLAS-INDEXS) TO HD-STATE.                EL531
01230                                                                   EL531
01231  3700-EXIT.                                                       EL531
01232      EXIT.                                                        EL531
01233                                                                   EL531
01234  3900-RETURN-EXIT.                                                EL531
01235      CLOSE ERACCT                                                 EL531
01236            ERPNDB                                                 EL531
01237            ERPYAJ                                                 EL531
01238            ELCNTL.                                                EL531
01239                                                                   EL531
01240  3999-PRINT-EXIT.                                                 EL531
01241      EXIT.                                                        EL531
01242      EJECT                                                        EL531
01243                                                                   EL531
01244  5000-GENERATE-CHECK      SECTION.                                EL531
01245      IF DTE-PRC-OPT NOT = '2'                                     EL531
01246         GO TO 5999-EXIT.                                          EL531
01247                                                                   EL531
01248      IF ERACCT-FILE-STATUS NOT = ZERO                             EL531
01249         GO TO 5999-EXIT.                                          EL531
01250                                                                   EL531
01251      IF CHECK-FIRST-TIME                                          EL531
01252          MOVE SPACE                    TO WS-CHECK-FIRST-TIME     EL531
01253          PERFORM 6000-READ-COMPANY-RECORD                         EL531
01254          MOVE CF-CAR-GROUP-ACCESS-CNTL TO WS-ACCESS-COMP-CNTL     EL531
01255          MOVE CF-CR-CHECK-NO-METHOD    TO WS-CR-CHECK-NO-METHOD   EL531
01256          MOVE CF-CR-MONTH-END-DT       TO WS-CR-MONTH-END-DT.     EL531
01257                                                                   EL531
01258      IF WS-CHECK-NO-MANUAL                                        EL531
01259          MOVE 'INVALID CHECK CONTROL - MANUAL' TO                 EL531
01260                               WS-ABEND-MESSAGE                    EL531
01261          GO TO ABEND-PGM.                                         EL531
01262                                                                   EL531
01263      MOVE SPACES                 TO PENDING-PAY-ADJ.              EL531
01264      MOVE 'PY'                   TO PY-RECORD-ID.                 EL531
01265      MOVE AM-COMPANY-CD          TO PY-COMPANY-CD.                EL531
01266                                                                   EL531
01267      IF WS-ZERO-CAR-GROUP                                         EL531
01268        OR WS-ZERO-CARRIER                                         EL531
01269          MOVE ZEROS              TO PY-CARRIER                    EL531
01270      ELSE                                                         EL531
01271          MOVE AM-CARRIER         TO PY-CARRIER.                   EL531
01272                                                                   EL531
01273      IF WS-ZERO-CAR-GROUP                                         EL531
01274        OR WS-ZERO-GROUPING                                        EL531
01275          MOVE ZEROS              TO PY-GROUPING                   EL531
01276      ELSE                                                         EL531
01277          MOVE AM-GROUPING        TO PY-GROUPING.                  EL531
01278                                                                   EL531
01279      MOVE AM-AGT (AM-REMIT-TO)   TO PY-FIN-RESP.                  EL531
01280      MOVE AM-ACCOUNT             TO PY-ACCOUNT.                   EL531
01281      MOVE 'C'                    TO PY-RECORD-TYPE.               EL531
01282                                                                   EL531
01283      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      EL531
01284                                                                   EL531
01285      MOVE WS-TIME                TO PY-FILE-SEQ-NO.               EL531
01286                                                                   EL531
01287      MOVE 'REFUND REIMBURSEMENT' TO PY-ENTRY-COMMENT.             EL531
01288      MOVE WS-CHECK-AMT           TO PY-ENTRY-AMT.                 EL531
01289                                                                   EL531
01290      IF WS-CHECK-NO-AT-PRINT                                      EL531
01291          MOVE ZEROS              TO PY-CHECK-NUMBER               EL531
01292      ELSE                                                         EL531
01293          PERFORM 6000-READ-COMPANY-RECORD                         EL531
01294          MOVE CF-CR-CHECK-COUNTER TO PY-CHECK-NUMBER              EL531
01295          PERFORM 6100-REWRITE-COMPANY.                            EL531
01296                                                                   EL531
01297      MOVE DTE-CLIENT             TO PY-LAST-MAINT-BY.             EL531
01298                                                                   EL531
01299      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      EL531
01300                                                                   EL531
01301      MOVE WS-TIME                TO PY-LAST-MAINT-HHMMSS.         EL531
01302      MOVE SAVE-POST-DATE         TO PY-LAST-MAINT-DT              EL531
01303                                     PY-INPUT-DT.                  EL531
01304      MOVE LOW-VALUES             TO PY-BILLED-DATE.               EL531
01305      MOVE ZEROS                  TO PY-CHECK-QUE-CONTROL          EL531
01306                                     PY-CHECK-QUE-SEQUENCE.        EL531
01307      MOVE LOW-VALUES             TO PY-CREDIT-ACCEPT-DT           EL531
01308                                     PY-REPORTED-DT                EL531
01309                                     PY-CHECK-WRITTEN-DT.          EL531
01310      MOVE WS-CR-MONTH-END-DT     TO PY-CREDIT-SELECT-DT.          EL531
01311      MOVE 'C'                    TO PY-CHECK-ORIGIN-SW.           EL531
01312                                                                   EL531
01313      WRITE PENDING-PAY-ADJ.                                       EL531
01314                                                                   EL531
01315      IF ERPYAJ-FILE-STATUS NOT = ZERO                             EL531
01316          MOVE 'ERROR OCCURED WRITING RECORD ' TO WS-ABEND-MESSAGE EL531
01317          MOVE ERPYAJ-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL531
01318          GO TO ABEND-PGM.                                         EL531
01319                                                                   EL531
01320      MOVE ZEROS                  TO WS-CHECK-AMT.                 EL531
01321                                                                   EL531
01322  5999-EXIT.                                                       EL531
01323      EXIT.                                                        EL531
01324  EJECT                                                            EL531
01325  6000-READ-COMPANY-RECORD     SECTION.                            EL531
01326      MOVE SPACES                 TO CF-CONTROL-PRIMARY.           EL531
01327      MOVE DTE-CLIENT             TO CF-COMPANY-ID.                EL531
01328      MOVE '1'                    TO CF-RECORD-TYPE.               EL531
01329      MOVE ZEROS                  TO CF-SEQUENCE-NO.               EL531
01330      READ ELCNTL.                                                 EL531
01331      IF ELCNTL-FILE-STATUS NOT = ZEROS                            EL531
01332          MOVE 'ERROR OCCURED READING CNTL RECORD ' TO             EL531
01333                                            WS-ABEND-MESSAGE       EL531
01334          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL531
01335          GO TO ABEND-PGM.                                         EL531
01336                                                                   EL531
01337  6000-EXIT.                                                       EL531
01338      EXIT.                                                        EL531
01339                                                                   EL531
01340  6100-REWRITE-COMPANY     SECTION.                                EL531
01341      IF NOT CHECK-CNT-RESET-VALUE                                 EL531
01342          ADD +1              TO CF-CR-CHECK-COUNTER               EL531
01343      ELSE                                                         EL531
01344          MOVE +1             TO CF-CR-CHECK-COUNTER.              EL531
01345                                                                   EL531
01346      REWRITE CONTROL-FILE.                                        EL531
01347                                                                   EL531
01348      IF ELCNTL-FILE-STATUS NOT = ZEROS                            EL531
01349          MOVE 'ERROR OCCURED REWRITING CNTL RECORD ' TO           EL531
01350                                            WS-ABEND-MESSAGE       EL531
01351          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL531
01352          GO TO ABEND-PGM.                                         EL531
01353                                                                   EL531
01354  6100-EXIT.                                                       EL531
01355      EXIT.                                                        EL531
01356      EJECT                                                        EL531
01357  9000-END-OF-JOB  SECTION.                                        EL531
01358      PERFORM 3650-HEADING.                                        EL531
01359      MOVE '0'        TO X.                                        EL531
01360      MOVE RPT-ENDED  TO P-DATA.                                   EL531
01361      PERFORM 3600-PRINT-RTN THRU 3600-PRINT-EXIT.                 EL531
01362                                                                   EL531
01363  9900-END-OF-JOB.                                                 EL531
01364              COPY ELCPRTCX.                                          CL**2
01365      CLOSE PRNTR.                                                 EL531
01366      GOBACK.                                                      EL531
01367                                                                   EL531
01368  ABEND-PGM   SECTION.                                             EL531
01369              COPY ELCABEND.                                          CL**2
