00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL502
00003  PROGRAM-ID.                 EL502 .                                 LV020
00004 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL502
00005 *                            VMOD=2.006.                             CL*15
00006 *                                                                 EL502
00007 *AUTHOR.     LOGIC INC.                                           EL502
00008 *            DALLAS, TEXAS.                                       EL502
00009                                                                   EL502
00009                                                                   EL502
00010 *SECURITY.   **************************************************** EL502
00011 *            *                                                  * EL502
00012 *            *  THIS PROGRAM IS THE PROPERTY OF LOGIC INC.      * EL502
00013 *            *                                                  * EL502
00014 *            *  USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES * EL502
00015 *            *  OF LOGIC INC. IS EXPRESSLY PROHIBITED WITHOUT   * EL502
00016 *            *  THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      * EL502
00017 *            *                                                  * EL502
00018 *            **************************************************** EL502
00019                                                                   EL502
00020 *REMARKS.                                                         EL502
00021 *      THIS PROGRAM IS USED TO UNLOAD THE ONLINE ACCOUNT MASTER   EL502
00022 *      TO A VSAM FILE. THE CONTROL FILE WILL BE UPDATED           EL502
00023 *      TO REFLECT THE FILE CREATE DATE.                           EL502
00024  EJECT                                                            EL502
00025  ENVIRONMENT DIVISION.                                            EL502
00026  INPUT-OUTPUT SECTION.                                            EL502
00027  FILE-CONTROL.                                                    EL502
00028                                                                   EL502
00029      SELECT PRNTR          ASSIGN TO SYS008-UR-1403-S-SYS008.     EL502
00030                                                                   EL502
00031      SELECT FICH           ASSIGN TO SYS020-UT-2400-S-SYS020.     EL502
00032                                                                   EL502
00033      SELECT DISK-DATE      ASSIGN TO SYS019-FBA1-S-SYS019.        EL502
00034                                                                   EL502
00035      SELECT ERACCTT        ASSIGN TO SYS010-FBA1-ERACCTT          EL502
00036                            ORGANIZATION IS INDEXED                EL502
00037                            ACCESS IS SEQUENTIAL                   EL502
00038                            RECORD KEY IS ERACCTT-KEY              EL502
00039                            FILE STATUS IS ERACCTT-FILE-STATUS.    EL502
00040                                                                   EL502
00041      SELECT ERACCT         ASSIGN TO SYS011-FBA1-ERACCT           EL502
00042                            ORGANIZATION IS INDEXED                EL502
00043                            ACCESS IS DYNAMIC                      EL502
00044                            RECORD KEY IS AM-CONTROL-PRIMARY       EL502
00045                            FILE STATUS IS ACCOUNT-FILE-STATUS.    EL502
00046                                                                   EL502
00047      SELECT ELCNTL         ASSIGN TO SYS012-FBA1-ELCNTL           EL502
00048                            ORGANIZATION IS INDEXED                EL502
00049                            ACCESS IS DYNAMIC                      EL502
00050                            RECORD KEY IS CF-CONTROL-PRIMARY       EL502
00051                            FILE STATUS IS CONTROL-FILE-STATUS.    EL502
00052                                                                   EL502
00053      SELECT ELREPT         ASSIGN TO SYS018-FBA1-ELREPT           EL502
00054                            ORGANIZATION IS INDEXED                EL502
00055                            ACCESS IS DYNAMIC                      EL502
00056                            RECORD KEY IS RF-CONTROL-PRIMARY       EL502
00057                            FILE STATUS IS DTE-VSAM-FLAGS.         EL502
00058                                                                   EL502
00059      SELECT ERREIN         ASSIGN TO SYS014-FBA1-ERREIN           EL502
00060                            ACCESS IS SEQUENTIAL                   EL502
00061                            ORGANIZATION IS INDEXED                EL502
00062                            FILE STATUS IS REIN-FILE-STATUS        EL502
00063                            RECORD KEY IS RE-CONTROL-PRIMARY.      EL502
00064                                                                   EL502
00065      SELECT ERCTBL         ASSIGN TO SYS017-FBA1-ERCTBL           EL502
00066                            ACCESS IS SEQUENTIAL                   EL502
00067                            ORGANIZATION IS INDEXED                EL502
00068                            FILE STATUS IS COMM-FILE-STATUS        EL502
00069                            RECORD KEY IS CT-CONTROL-PRIMARY.      EL502
00070  EJECT                                                            EL502
00071  DATA DIVISION.                                                   EL502
00072  FILE SECTION.                                                    EL502
00073                                                                   EL502
00074  FD  PRNTR                   COPY ELCPRTFD.                       EL502
00075  EJECT                                                            EL502
00076  FD  FICH                    COPY ELCFCHFD.                       EL502
00077  EJECT                                                            EL502
00078  FD  DISK-DATE               COPY ELCDTEFD.                       EL502
00079  EJECT                                                            EL502
00080  FD  ELREPT.                                                      EL502
00081                              COPY ELCREPT.                        EL502
00082  EJECT                                                            EL502
00083  FD  ERACCTT.                                                     EL502
00084                                                                   EL502
00085  01  ACCT-MSTR-REC.                                               EL502
00086      12  RECORD-ID           PIC XX.                              EL502
00087      12  ERACCTT-KEY         PIC X(26).                           EL502
00088      12  FILLER              PIC X(1972).                         EL502
00089  EJECT                                                            EL502
00090  FD  ERACCT.                                                      EL502
00091                                                                   EL502
00092                              COPY ERCACCT.                        EL502
00093  EJECT                                                            EL502
00094  FD  ELCNTL.                                                      EL502
00095                                                                   EL502
00096                              COPY ELCCNTL.                        EL502
00097  EJECT                                                            EL502
00098  FD  ERREIN.                                                      EL502
00099                                                                   EL502
00100                              COPY ERCREIN.                        EL502
00101  EJECT                                                            EL502
00102  FD  ERCTBL.                                                      EL502
00103                                                                   EL502
00104                              COPY ERCCTBL.                        EL502
00105  EJECT                                                            EL502
00106  WORKING-STORAGE SECTION.                                         EL502
00107  77  FILLER  PIC X(32) VALUE '********************************'.  EL502
00108  77  FILLER  PIC X(32) VALUE '      EL502 WORKING-STORAGE     '.  EL502
00109  77  FILLER  PIC X(32) VALUE '*********** VM 2.006 ***********'.     CL*15
00110                                                                   EL502
00111  01  WORK-AREAS.                                                  EL502
00112      12  WS-LINE-COUNT          PIC S9(3)  VALUE +99 COMP-3.      EL502
00113      12  WS-LINE-COUNT-MAX      PIC S9(3)  VALUE +60 COMP-3.      EL502
00114      12  WS-PAGE                PIC S9(3)  VALUE +0  COMP-3.      EL502
00115      12  WS-CURRENT-BIN-DT      PIC XX.                           EL502
00116      12  WS-RETURN-CODE         PIC S9(4) COMP.                   EL502
00117      12  WS-ABEND-MESSAGE       PIC X(80).                        EL502
00118      12  WS-ABEND-FILE-STATUS   PIC XX  VALUE ZEROS.              EL502
00119      12  WS-ZERO                PIC S9  VALUE ZERO COMP-3.        EL502
00120      12  ACCOUNT-OPEN           PIC X   VALUE SPACES.             EL502
00121      12  ACCOUNT-FILE-STATUS    PIC XX  VALUE ZEROS.              EL502
00122      12  ACC REDEFINES ACCOUNT-FILE-STATUS.                       EL502
00123          16  ACC1               PIC X.                            EL502
00124          16  ACC2               PIC X.                            EL502
00125      12  ERACCTT-FILE-STATUS    PIC XX  VALUE ZEROS.              EL502
00126      12  CONTROL-FILE-STATUS    PIC XX  VALUE ZEROS.              EL502
00127      12  WS-SAVE-PRINT-RECORD   PIC X(133) VALUE SPACES.          EL502
00128      12  WS-WORK-DATE-CYMD.                                          CL*16
00129          16  FILLER             PIC 999.                             CL*16
00130          16  WS-WORK-CC         PIC 99.                              CL*16
00131          16  WS-WORK-YY         PIC 99.                              CL*16
00132          16  WS-WORK-MM         PIC 99.                              CL*16
00133          16  WS-WORK-DD         PIC 99.                              CL*16
00134       12  WS-WORK-DATE  REDEFINES                                    CL*16
00135           WS-WORK-DATE-CYMD     PIC 9(11).                           CL*16
00136                                                                   EL502
00137  01  DTE-INTERFACE-CODES.                                         EL502
00138      12  X                 PIC X           VALUE SPACE.           EL502
00139      12  PGM-SUB           PIC S9(4)  COMP VALUE +502.            EL502
00140      12  ABEND-CODE        PIC 9999        VALUE ZERO.            EL502
00141      12  ABEND-OPTION      PIC X           VALUE SPACE.           EL502
00142      12  OLC-REPORT-NAME   PIC X(6)        VALUE 'EL502'.         EL502
00143      12  WS-IN             PIC 9(5)        VALUE ZEROS.           EL502
00144      12  WS-OUT            PIC 9(5)        VALUE ZEROS.           EL502
00145      12  WS-CONTROL-PRIMARY.                                      EL502
00146          16  WS-COMPANY-CD     PIC X.                             EL502
00147          16  FILLER            PIC X(25).                         EL502
00148      12  WS-SAVE-PRIMARY.                                         EL502
00149          16  FILLER                   PIC X.                      EL502
00150          16  WS-SAVE-KEY.                                         EL502
00151              20  WS-SAVE-CARRIER      PIC X.                      EL502
00152              20  WS-SAVE-GROUPING     PIC X(6).                   EL502
00153              20  WS-SAVE-STATE        PIC XX.                     EL502
00154              20  WS-SAVE-ACCOUNT      PIC X(10).                  EL502
00155              20  WS-SAVE-EXPIRE-DT    PIC 9(11)  COMP-3.             CL**4
00156     12  END-OF-PROCESS-SWT        PIC X VALUE 'N'.                EL502
00157         88  END-OF-PROCESS         VALUE 'Y'.                     EL502
00158         88  NOT-END-OF-PROCESS     VALUE 'N'.                     EL502
00159                                                                   EL502
00160  01  COMP-3-WORK-AREA.                                            EL502
00161      12  K1                      PIC S9(7)  VALUE +1.             EL502
00162      12  K2                      PIC S9(7)  VALUE +2.             EL502
00163      12  DELETE-COUNT            PIC S9(7)  VALUE +0.             EL502
00164      12  DATE-ERROR-COUNT        PIC S9(7)  VALUE +0.             EL502
00165                                                                   EL502
00166  01  REIN-FILE-STATUS.                                            EL502
00167      12  REIN-FILE-STATUS-1      PIC X.                           EL502
00168      12  REIN-FILE-STATUS-2      PIC X.                           EL502
00169                                                                   EL502
00170  01  COMM-FILE-STATUS.                                            EL502
00171      12  COMM-FILE-STATUS-1      PIC X.                           EL502
00172      12  COMM-FILE-STATUS-2      PIC X.                           EL502
00173                                                                   EL502
00174  01  ACCT-DATE-CK.                                                EL502
00175      12  ACC-XC-SEQ.                                              EL502
00176          16  ACC-XC-CARR     PIC X               VALUE SPACES.    EL502
00177          16  ACC-XC-GROUP    PIC X(6)            VALUE SPACES.    EL502
00178          16  ACC-XC-ST       PIC XX              VALUE SPACES.    EL502
00179          16  ACC-XC-NO       PIC X(10)           VALUE SPACES.    EL502
00180      12  ACC-DC-SEQ.                                              EL502
00181          16  ACC-DC-CARR     PIC X               VALUE SPACES.    EL502
00182          16  ACC-DC-GROUP    PIC X(6)            VALUE SPACES.    EL502
00183          16  ACC-DC-ST       PIC XX              VALUE SPACES.    EL502
00184          16  ACC-DC-NO       PIC X(10)           VALUE SPACES.    EL502
00185      12  ACC-DC-X            PIC 9(11)  COMP-3.                      CL**8
00186                                                                   EL502
00187  01  TABLE-AREA.                                                  EL502
00188      12  XR-TABLE.                                                EL502
00189          16  FILLER          PIC X(6000)        VALUE HIGH-VALUE. EL502
00190      12  XR-TABLE-X REDEFINES XR-TABLE.                           EL502
00191          16  XR-TAB          PIC XXX         OCCURS 2000 TIMES.   EL502
00192      12  XC-TABLE.                                                EL502
00193          16  FILLER          PIC X(6000)        VALUE HIGH-VALUE. EL502
00194      12  XC-TABLE-X REDEFINES XC-TABLE.                           EL502
00195          16  XC-TAB          PIC XXX         OCCURS 2000 TIMES.   EL502
00196      12  RTB-SW              PIC S9      COMP-3  VALUE +0.        EL502
00197      12  CTB-SW              PIC S9      COMP-3  VALUE +0.        EL502
00198      12  RTB-ERR             PIC S9      COMP-3  VALUE +0.        EL502
00199      12  CTB-ERR             PIC S9      COMP-3  VALUE +0.        EL502
00200      12  PRE-RTB             PIC XXX             VALUE LOW-VALUE. EL502
00201      12  PRE-CTB             PIC XXX             VALUE LOW-VALUE. EL502
00202      12  X-CTB               PIC XXX             VALUE LOW-VALUE. EL502
00203      12  X1                  PIC S9(4)   COMP    VALUE +0.        EL502
00204      12  X2                  PIC S9(4)   COMP    VALUE +0.        EL502
00205  EJECT                                                            EL502
00206  01  WS-HEADING1.                                                 EL502
00207      12  FILLER                      PIC X(51)       VALUE '1'.   EL502
00208      12  WS-H1-TITLE                 PIC X(69)                    EL502
00209             VALUE 'SEQUENTIAL ACCOUNT FILE LOAD'.                 EL502
00210      12  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL502  '.    EL502
00211                                                                   EL502
00212  01  WS-HEADING2.                                                 EL502
00213      12  FILLER                      PIC X(46)       VALUE SPACES.EL502
00214      12  WS-H2-CLIENT-NAME           PIC X(74)       VALUE SPACES.EL502
00215      12  WS-H2-DATE                  PIC X(8)        VALUE SPACES.EL502
00216                                                                   EL502
00217  01  WS-HEADING3.                                                 EL502
00218      12  FILLER                      PIC X(51)       VALUE SPACES.EL502
00219      12  WS-H3-DATE                  PIC X(69)       VALUE SPACES.EL502
00220      12  FILLER                      PIC X(5)        VALUE 'PAGE'.EL502
00221      12  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL502
00222                                                                   EL502
00223  01  WS-HEADING4                     PIC X(133)      VALUE SPACES.EL502
00224                                                                   EL502
00225  01  WS-DETAIL1.                                                  EL502
00226      12  FILLER             PIC X.                                EL502
00227      12  DTL-MSG1           PIC X(18) VALUE 'SUCCESSFUL UNLOAD'.  EL502
00228      12  DTL-MSG2           PIC X(18) VALUE SPACE.                EL502
00229      12  DTL-MSG3           PIC X(18) VALUE SPACE.                EL502
00230      12  D-RECORD-COUNT     PIC Z,ZZZ,ZZ9.                        EL502
00231      12  FILLER             PIC X(10) VALUE ' RECORDS'.           EL502
00232  EJECT                                                            EL502
00233  01  COMP-3-WORK-AREA    COMP-3.                                  EL502
00234      12  CNT                 PIC S9(7)           VALUE +0.        EL502
00235      12  BLK                 PIC S9(7)           VALUE +0.        EL502
00236                                                                   EL502
00237  01  DTL-1.                                                       EL502
00238      12  FILLER              PIC X(20)           VALUE            EL502
00239              'SUCCESSFULLY LOADED'.                               EL502
00240      12  D-CNT               PIC Z,ZZZ,ZZ9.                       EL502
00241      12  FILLER              PIC X(10)           VALUE            EL502
00242              ' RECORDS ('.                                        EL502
00243      12  D-BLK               PIC Z,ZZZ,ZZ9.                       EL502
00244      12  FILLER              PIC X(10)           VALUE            EL502
00245              ' BLOCKS)'.                                          EL502
00246                                                                   EL502
00247  01  BASIC-COMM.                                                  EL502
00248      12  FILLER              PIC X(24)           VALUE            EL502
00249              '  THE COMMISSION STRUCTU'.                          EL502
00250      12  FILLER              PIC X(24)           VALUE            EL502
00251              'RE HAS BEEN CHANGED ON A'.                          EL502
00252      12  FILLER              PIC X(8)            VALUE 'CCOUNT'.  EL502
00253      12  BC-CARR             PIC X.                               EL502
00254      12  FILLER              PIC X               VALUE '-'.       EL502
00255      12  BC-GRP              PIC X(6).                            EL502
00256      12  FILLER              PIC X               VALUE '-'.       EL502
00257      12  BC-STATE            PIC XX.                              EL502
00258      12  FILLER              PIC X               VALUE '-'.       EL502
00259      12  BC-ACCT             PIC X(10).                           EL502
00260      12  FILLER              PIC X               VALUE '-'.       EL502
00261      12  BC-EXP.                                                     CL*16
00262          16  BC-EXP-YR       PIC 99.                                 CL*16
00263          16  BC-EXP-MO       PIC 99.                                 CL*16
00264          16  BC-EXP-DA       PIC 99.                                 CL*16
00265      12  FILLER              PIC X(21)           VALUE            EL502
00266              ' AND THE COMMISSIONS '.                             EL502
00267      12  BC-MES              PIC X(24).                           EL502
00268      12  FILLER              PIC X               VALUE '-'.       EL502
00269      12  BC-RECALC-CD        PIC X.                               EL502
00270                                                                   EL502
00271  01  BASIC-REIN.                                                  EL502
00272      12  FILLER              PIC X(24)           VALUE            EL502
00273              ' REINSURANCE TABLE CODE '.                          EL502
00274      12  FILLER              PIC X(16)           VALUE            EL502
00275              'WAS CHANGED TO ('.                                  EL502
00276      12  BR-REIN-TABLE       PIC XXX.                             EL502
00277      12  FILLER              PIC X(5)            VALUE ') ON '.   EL502
00278      12  FILLER              PIC X(8)            VALUE 'ACCOUNT'. EL502
00279      12  BR-CARR             PIC X.                               EL502
00280      12  FILLER              PIC X               VALUE '-'.       EL502
00281      12  BR-GRP              PIC X(6).                            EL502
00282      12  FILLER              PIC X               VALUE '-'.       EL502
00283      12  BR-STATE            PIC XX.                              EL502
00284      12  FILLER              PIC X               VALUE '-'.       EL502
00285      12  BR-ACCT             PIC X(10).                           EL502
00286      12  FILLER              PIC X               VALUE '-'.       EL502
00287      12  BR-EXP.                                                     CL*16
00288          16  BR-EXP-YR       PIC 99.                                 CL*16
00289          16  BR-EXP-MO       PIC 99.                                 CL*16
00290          16  BR-EXP-DA       PIC 99.                                 CL*16
00291      12  FILLER              PIC X(21)           VALUE            EL502
00292              ' AND THE REINSURANCE'.                              EL502
00293      12  BR-MES              PIC X(24).                           EL502
00294      12  FILLER              PIC X               VALUE '-'.       EL502
00295      12  BR-RECALC-CD        PIC X.                               EL502
00296                                                                   EL502
00297  01  BASIC-FILLS.                                                 EL502
00298      12  WILL-MES            PIC X(24)           VALUE            EL502
00299              'WILL BE RECALCULATED'.                              EL502
00300      12  WONT-MES            PIC X(24)           VALUE            EL502
00301              'WILL NOT BE RECALCULATED'.                          EL502
00302                                                                   EL502
00303  01  ACCT-DTE-OVER.                                               EL502
00304      12  FILLER              PIC XXX             VALUE SPACES.    EL502
00305      12  FILLER              PIC X(47)           VALUE            EL502
00306              'THERE IS A DATE-RANGE OVERLAP PROBLEM ON'.          EL502
00307                                                                   EL502
00308  01  ACCT-DTE-GAP.                                                EL502
00309      12  FILLER              PIC XXX             VALUE SPACES.    EL502
00310      12  FILLER              PIC X(47)           VALUE            EL502
00311              'THERE IS A POSSIBLE DATE-RANGE GAP PROBLEM ON'.     EL502
00312                                                                   EL502
00313  01  EXP-DATE-MSG.                                                EL502
00314      12  FILLER              PIC XXX             VALUE SPACES.    EL502
00315      12  FILLER              PIC X(47)           VALUE            EL502
00316              'EFFECTIVE DATE GREATER THAN EXPIRATION DATE'.       EL502
00317                                                                   EL502
00318  01  DATE-RANGE-ERR-MSG.                                          EL502
00319      12  MSG-SLOT            PIC X(52).                           EL502
00320      12  ERR-PREFIX          PIC X.                               EL502
00321      12  FILLER              PIC X               VALUE '-'.       EL502
00322      12  ERR-GROUPING        PIC X(6).                            EL502
00323      12  FILLER              PIC X               VALUE '-'.       EL502
00324      12  ERR-ST              PIC XX.                              EL502
00325      12  FILLER              PIC X               VALUE '-'.       EL502
00326      12  ERR-ACCT            PIC X(10).                           EL502
00327      12  FILLER              PIC X(5)            VALUE ' EFF-'.   EL502
00328      12  ERR-EFF-DATE.                                               CL*16
00329          16  ERR-EFF-MO      PIC 99.                                 CL*19
00330          16  ERR-EFF-DA      PIC 99.                                 CL*19
00331          16  ERR-EFF-YR      PIC 99.                                 CL*19
00332      12  FILLER              PIC X(5)            VALUE ' EXP-'.   EL502
00333      12  ERR-EXP-DATE.                                               CL*16
00334          16  ERR-EXP-MO      PIC 99.                                 CL*19
00335          16  ERR-EXP-DA      PIC 99.                                 CL*19
00336          16  ERR-EXP-YR      PIC 99.                                 CL*19
00337      12  FILLER              PIC X               VALUE ' '.       EL502
00338      12  ERR-NAME            PIC X(30).                           EL502
00339                                                                   EL502
00340  01  RTB-ERR-MSG.                                                 EL502
00341      12  FILLER              PIC X(55)           VALUE            EL502
00342        'SET UP REINSURANCE TABLE AND RERUN - TABLE NOT ON FILE '. EL502
00343      12  ERR-RTB             PIC XXX.                             EL502
00344      12  FILLER              PIC X(14)           VALUE            EL502
00345              '  FOR ACCOUNT '.                                    EL502
00346      12  ERR-RTB-ACC         PIC X(10).                           EL502
00347                                                                   EL502
00348  01  CTB-ERR-MSG.                                                 EL502
00349      12  FILLER              PIC X(55)           VALUE            EL502
00350        ' SET UP COMMISSION TABLE AND RERUN - TABLE NOT ON FILE '. EL502
00351      12  ERR-CTB             PIC XXX.                             EL502
00352      12  FILLER              PIC X(14)           VALUE            EL502
00353              '  FOR ACCOUNT '.                                    EL502
00354      12  ERR-CTB-ACC         PIC X(10).                           EL502
00355  EJECT                                                            EL502
00356                              COPY ELCDATE.                           CL*20
00357  EJECT                                                            EL502
00358                              COPY ELCDTECX.                       EL502
00359  EJECT                                                            EL502
00360                              COPY ELCDTEVR.                       EL502
00361  EJECT                                                            EL502
00362  PROCEDURE DIVISION.                                              EL502
00363                                                                   EL502
00364  0000-LOAD-DATE-WS.                                               EL502
00365                              COPY ELCDTERX.                       EL502
00366  EJECT                                                            EL502
00367  0100-MAINLINE.                                                   EL502
00368      PERFORM 0200-INITIALIZE     THRU 0299-EXIT.                  EL502
00369                                                                   EL502
00370      PERFORM 0400-UNLOAD-ACCOUNT THRU 0499-EXIT.                  EL502
00371                                                                   EL502
00372      PERFORM 9000-EOJ            THRU 9000-EXIT.                  EL502
00373                                                                   EL502
00374      PERFORM 8000-FINALIZE       THRU 8000-EXIT.                  EL502
00375                                                                   EL502
00376      PERFORM 0300-READ-CNTL      THRU 0399-EXIT.                  EL502
00377                                                                   EL502
00378  0199-MAINLINE-EXIT.                                              EL502
00379      GOBACK.                                                      EL502
00380                                                                   EL502
00381  0200-INITIALIZE.                                                 EL502
00382      MOVE ZEROS                  TO WS-RETURN-CODE.               EL502
00383      MOVE COMPANY-NAME           TO WS-H2-CLIENT-NAME.            EL502
00384      MOVE WS-CURRENT-DATE        TO WS-H2-DATE.                   EL502
00385      MOVE ALPH-DATE              TO WS-H3-DATE.                   EL502
00386                                                                   EL502
00387      OPEN INPUT  ERACCT                                           EL502
00388           OUTPUT PRNTR.                                           EL502
00389                                                                   EL502
00390      IF ACCOUNT-FILE-STATUS  = '00' OR '97'                       EL502
00391          NEXT SENTENCE                                            EL502
00392      ELSE                                                         EL502
00393          MOVE ACCOUNT-FILE-STATUS                                 EL502
00394                                   TO WS-ABEND-FILE-STATUS         EL502
00395          MOVE 'ERACCT OPEN ERROR-'                                EL502
00396                                   TO WS-ABEND-MESSAGE             EL502
00397          GO TO ABEND-PGM.                                         EL502
00398                                                                   EL502
00399      MOVE WS-CURRENT-DATE         TO DC-GREG-DATE-1-EDIT.         EL502
00400      MOVE '2'                     TO DC-OPTION-CODE.              EL502
00401                                                                   EL502
00402      PERFORM 0500-DATE-RTN THRU 0599-EXIT.                        EL502
00403                                                                   EL502
00404      MOVE DC-BIN-DATE-1           TO WS-CURRENT-BIN-DT.           EL502
00405      MOVE LOW-VALUES              TO WS-CONTROL-PRIMARY.          EL502
00406      MOVE DTE-CLASIC-COMPANY-CD   TO WS-COMPANY-CD.               EL502
00407                                                                   EL502
00408  0299-EXIT.                                                       EL502
00409      EXIT.                                                        EL502
00410  EJECT                                                            EL502
00411  0300-READ-CNTL.                                                  EL502
00412      IF EP-SW = '1'  OR  '2'                                      EL502
00413          OPEN I-O ELCNTL                                          EL502
00414      ELSE                                                         EL502
00415          OPEN INPUT ELCNTL.                                       EL502
00416                                                                   EL502
00417      IF CONTROL-FILE-STATUS  = '00' OR '97'                       EL502
00418          NEXT SENTENCE                                            EL502
00419      ELSE                                                         EL502
00420          MOVE CONTROL-FILE-STATUS                                 EL502
00421                                   TO WS-ABEND-FILE-STATUS         EL502
00422          MOVE ' CNTL OPEN ERROR- '                                EL502
00423                                   TO WS-ABEND-MESSAGE             EL502
00424          GO TO ABEND-PGM.                                         EL502
00425                                                                   EL502
00426      MOVE SPACES                  TO CF-ACCESS-CD-GENL.           EL502
00427      MOVE DTE-CLIENT              TO CF-COMPANY-ID.               EL502
00428      MOVE '1'                     TO CF-RECORD-TYPE.              EL502
00429      MOVE ZEROS                   TO CF-SEQUENCE-NO.              EL502
00430                                                                   EL502
00431      READ ELCNTL.                                                 EL502
00432                                                                   EL502
00433      IF CONTROL-FILE-STATUS NOT = ZERO                            EL502
00434          MOVE CONTROL-FILE-STATUS                                 EL502
00435                                   TO WS-ABEND-FILE-STATUS         EL502
00436          MOVE ' CNTL READ ERROR- '                                EL502
00437                                   TO WS-ABEND-MESSAGE             EL502
00438          GO TO ABEND-PGM.                                         EL502
00439                                                                   EL502
00440      IF EP-SW = '1'  OR  '2'                                      EL502
00441          MOVE WS-CURRENT-BIN-DT   TO CF-ACCOUNT-MSTR-CREATE-DT    EL502
00442          MOVE LOW-VALUES          TO CF-ACCOUNT-MSTR-MAINT-DT     EL502
00443      ELSE                                                         EL502
00444          GO TO 0399-EXIT.                                         EL502
00445                                                                   EL502
00446      REWRITE CONTROL-FILE.                                        EL502
00447                                                                   EL502
00448      IF CONTROL-FILE-STATUS NOT = ZERO                            EL502
00449          MOVE CONTROL-FILE-STATUS                                 EL502
00450                                   TO WS-ABEND-FILE-STATUS         EL502
00451          MOVE ' CNTL REWRITE ERROR- '                             EL502
00452                                   TO WS-ABEND-MESSAGE             EL502
00453          GO TO ABEND-PGM.                                         EL502
00454                                                                   EL502
00455      CLOSE ELCNTL.                                                EL502
00456                                                                   EL502
00457  0399-EXIT.                                                       EL502
00458      EXIT.                                                        EL502
00459  EJECT                                                            EL502
00460  0400-UNLOAD-ACCOUNT.                                             EL502
00461      OPEN OUTPUT ERACCTT.                                         EL502
00462                                                                   EL502
00463      IF ERACCTT-FILE-STATUS  = '00' OR '97'                       EL502
00464          NEXT SENTENCE                                            EL502
00465      ELSE                                                         EL502
00466          MOVE ERACCTT-FILE-STATUS                                 EL502
00467                                   TO WS-ABEND-FILE-STATUS         EL502
00468          MOVE 'ERACCTT OPEN ERROR-'                               EL502
00469                                   TO WS-ABEND-MESSAGE             EL502
00470          GO TO ABEND-PGM.                                         EL502
00471                                                                   EL502
00472      MOVE 'Y'                     TO ACCOUNT-OPEN.                EL502
00473      MOVE WS-CONTROL-PRIMARY      TO AM-CONTROL-PRIMARY.          EL502
00474                                                                   EL502
00475  0410-START.                                                      EL502
00476      START ERACCT KEY NOT LESS AM-CONTROL-PRIMARY.                EL502
00477                                                                   EL502
00478      IF ACCOUNT-FILE-STATUS  = '10' OR '23'                       EL502
00479          GO TO 0499-EXIT.                                         EL502
00480                                                                   EL502
00481      IF ACCOUNT-FILE-STATUS NOT = ZERO                            EL502
00482          MOVE ACCOUNT-FILE-STATUS                                 EL502
00483                                  TO WS-ABEND-FILE-STATUS          EL502
00484          MOVE 'ERACCT START ERROR-'                               EL502
00485                                  TO WS-ABEND-MESSAGE              EL502
00486          GO TO ABEND-PGM.                                         EL502
00487                                                                   EL502
00488  0420-READNEXT.                                                   EL502
00489      READ ERACCT  NEXT RECORD.                                    EL502
00490                                                                   EL502
00491      IF ACC1 = '1'                                                EL502
00492          GO TO 0499-EXIT.                                         EL502
00493                                                                   EL502
00494      IF ACCOUNT-FILE-STATUS NOT = ZEROS                           EL502
00495          MOVE 'ERROR ON READ  '  TO WS-ABEND-MESSAGE              EL502
00496          MOVE ACCOUNT-FILE-STATUS                                 EL502
00497                                  TO WS-ABEND-FILE-STATUS          EL502
00498          GO TO ABEND-PGM.                                         EL502
00499                                                                   EL502
00500      IF DTE-CLASIC-COMPANY-CD NOT = AM-COMPANY-CD                 EL502
00501          GO TO 0499-EXIT.                                         EL502
00502                                                                   EL502
00503      MOVE AM-CONTROL-PRIMARY     TO WS-SAVE-PRIMARY.              EL502
00504                                                                   EL502
00505      IF AM-EXPIRATION-DT = HIGH-VALUES                            EL502
00506          MOVE 99999999999           TO AM-EXPIRE-DT                  CL*16
00507                                        WS-SAVE-EXPIRE-DT             CL*16
00508      ELSE                                                         EL502
00509          MOVE AM-EXPIRATION-DT      TO DC-BIN-DATE-1                 CL*16
00510          MOVE ' '                   TO DC-OPTION-CODE                CL*16
00511          PERFORM 0500-DATE-RTN THRU 0599-EXIT                     EL502
00512          IF NO-CONVERSION-ERROR                                   EL502
00513              MOVE DC-GREG-DATE-CYMD TO AM-EXPIRE-DT                  CL*16
00514                                        WS-SAVE-EXPIRE-DT             CL*16
00515          ELSE                                                     EL502
00516             DISPLAY '*EL502*  EXPIRE DATE INVALID - '             EL502
00517                 AM-EXPIRE-DT                                         CL*16
00518             DISPLAY '*EL502*  ACCOUNT - ' AM-CARRIER ' '          EL502
00519                 AM-GROUPING ' ' AM-STATE ' ' AM-ACCOUNT           EL502
00520             MOVE 'INVALID EXPIRE DATE ' TO WS-ABEND-MESSAGE       EL502
00521             IF DTE-CLIENT = 'MON'                                 EL502
00522               GO TO 0420-READNEXT                                 EL502
00523              ELSE                                                 EL502
00524               GO TO ABEND-PGM.                                    EL502
00525                                                                   EL502
00526      IF WS-SAVE-KEY = AM-MSTR-CNTRL                               EL502
00527          NEXT SENTENCE                                            EL502
00528      ELSE                                                         EL502
00529          DISPLAY '*EL502*  VSAM PRIMARY CONTROL DOES NOT '        EL502
00530              'MATCH ACCOUNT MASTER CONTROL'                       EL502
00531          DISPLAY '*EL502*  VSAM ACCOUNT KEY - ' WS-SAVE-KEY       EL502
00532          DISPLAY '*EL502*  ACCOUNT KEY      - ' AM-MSTR-CNTRL     EL502
00533          MOVE 'INVALID ACCT MASTER CONTROL ' TO WS-ABEND-MESSAGE  EL502
00534          GO TO ABEND-PGM.                                         EL502
00535                                                                   EL502
00536      IF AM-EFFECTIVE-DT = HIGH-VALUES                             EL502
00537          MOVE 99999999999           TO AM-EFFECT-DT                  CL*16
00538      ELSE                                                         EL502
00539          MOVE ' '                   TO DC-OPTION-CODE                CL*16
00540          MOVE AM-EFFECTIVE-DT       TO DC-BIN-DATE-1                 CL*16
00541          PERFORM 0500-DATE-RTN THRU 0599-EXIT                     EL502
00542          IF NO-CONVERSION-ERROR                                   EL502
00543              MOVE DC-GREG-DATE-CYMD TO AM-EFFECT-DT                  CL*16
00544          ELSE                                                     EL502
00545             DISPLAY '*EL502*  EFFECT DATE INVALID - '             EL502
00546                 AM-EFFECT-DT                                         CL*17
00547             DISPLAY '*EL502*  ACCOUNT - ' AM-CARRIER ' '          EL502
00548                 AM-GROUPING ' ' AM-STATE ' ' AM-ACCOUNT           EL502
00549             MOVE 'INVALID EFFECT DATE ' TO WS-ABEND-MESSAGE       EL502
00550             GO TO ABEND-PGM.                                      EL502
00551                                                                   EL502
00552      MOVE ACCOUNT-MASTER         TO ACCT-MSTR-REC.                EL502
00553                                                                   EL502
00554      WRITE ACCT-MSTR-REC.                                         EL502
00555                                                                   EL502
00556      IF ERACCTT-FILE-STATUS NOT = ZERO                            EL502
00557          MOVE ERACCTT-FILE-STATUS    TO WS-ABEND-FILE-STATUS      EL502
00558          MOVE 'ERACCTT WRITE ERROR-' TO WS-ABEND-MESSAGE          EL502
00559          GO TO ABEND-PGM.                                         EL502
00560                                                                   EL502
00561      ADD 1  TO CNT.                                               EL502
00562                                                                   EL502
00563      IF AM-RECALC-COMM  = SPACES                                  EL502
00564          GO TO 0440-CK-RECALC-REIN.                               EL502
00565                                                                   EL502
00566  0430-RECALC-MESSAGES.                                            EL502
00567      IF AM-RECALC-COMM = 'N'                                      EL502
00568          MOVE WONT-MES           TO BC-MES                        EL502
00569      ELSE                                                         EL502
00570          MOVE WILL-MES           TO BC-MES.                       EL502
00571                                                                   EL502
00572      MOVE AM-RECALC-COMM         TO BC-RECALC-CD.                 EL502
00573      MOVE AM-CARRIER             TO BC-CARR.                      EL502
00574      MOVE AM-GROUPING            TO BC-GRP.                       EL502
00575      MOVE AM-STATE               TO BC-STATE.                     EL502
00576      MOVE AM-ACCOUNT             TO BC-ACCT.                      EL502
00577      MOVE AM-EXPIRE-DT           TO WS-WORK-DATE.                    CL*16
00578      MOVE WS-WORK-YY             TO BC-EXP-YR.                       CL*17
00579      MOVE WS-WORK-MM             TO BC-EXP-MO.                       CL*17
00580      MOVE WS-WORK-DD             TO BC-EXP-DA.                       CL*17
00581      MOVE BASIC-COMM             TO P-DATA.                       EL502
00582      MOVE '0'                    TO P-CTL.                        EL502
00583                                                                   EL502
00584      PERFORM WRITE-A-LINE.                                        EL502
00585                                                                   EL502
00586  0440-CK-RECALC-REIN.                                             EL502
00587      IF AM-RECALC-REIN = SPACES                                   EL502
00588          GO TO 0450-DO-VERIFY.                                    EL502
00589                                                                   EL502
00590      IF AM-RECALC-REIN = 'Y'                                      EL502
00591          MOVE WILL-MES           TO BR-MES                        EL502
00592      ELSE                                                         EL502
00593          MOVE WONT-MES           TO BR-MES.                       EL502
00594                                                                   EL502
00595      MOVE AM-REI-TABLE           TO BR-REIN-TABLE.                EL502
00596      MOVE AM-RECALC-REIN         TO BR-RECALC-CD.                 EL502
00597      MOVE AM-CARRIER             TO BR-CARR.                      EL502
00598      MOVE AM-GROUPING            TO BR-GRP.                       EL502
00599      MOVE AM-STATE               TO BR-STATE.                     EL502
00600      MOVE AM-ACCOUNT             TO BR-ACCT.                      EL502
00601      MOVE AM-EXPIRE-DT           TO WS-WORK-DATE.                    CL*16
00602      MOVE WS-WORK-YY             TO BR-EXP-YR.                       CL*17
00603      MOVE WS-WORK-MM             TO BR-EXP-MO.                       CL*17
00604      MOVE WS-WORK-DD             TO BR-EXP-DA.                       CL*17
00605      MOVE BASIC-REIN             TO P-DATA.                       EL502
00606      MOVE '0'                    TO P-CTL.                        EL502
00607                                                                   EL502
00608      PERFORM WRITE-A-LINE.                                        EL502
00609                                                                   EL502
00610  0450-DO-VERIFY.                                                  EL502
00611      IF DTE-PGM-OPT = 2                                           EL502
00612          NEXT SENTENCE                                            EL502
00613      ELSE                                                         EL502
00614          PERFORM 0600-VERIFY-RTB-RTN THRU 0699-EXIT               EL502
00615          PERFORM 0700-VERIFY-CTB-RTN THRU 0799-EXIT.              EL502
00616                                                                   EL502
00617      PERFORM 1120-DATE-LAP-RTN THRU 1199-E-THIS-RTN.              EL502
00618                                                                   EL502
00619      GO TO 0420-READNEXT.                                         EL502
00620                                                                   EL502
00621  0499-EXIT.                                                       EL502
00622      EXIT.                                                        EL502
00623  EJECT                                                            EL502
00624  0500-DATE-RTN.                                                   EL502
00625      CALL 'ELDATCX'  USING DATE-CONVERSION-DATA.                  EL502
00626                                                                   EL502
00627      IF DC-ERROR-CODE NOT = SPACE                                 EL502
00628          MOVE  ZEROS             TO DC-BIN-DATE-1                 EL502
00629          ADD 1  TO DATE-ERROR-COUNT.                              EL502
00630                                                                   EL502
00631  0599-EXIT.                                                       EL502
00632      EXIT.                                                        EL502
00633  EJECT                                                            EL502
00634  0600-VERIFY-RTB-RTN.                                             EL502
00635 *    IF DTE-REINSURANCE NOT = '1'                                 EL502
00636          GO TO 0699-EXIT.                                         EL502
00637                                                                   EL502
00638      IF AM-REI-TABLE = SPACES OR ZEROS                            EL502
00639          GO TO 0699-EXIT.                                         EL502
00640                                                                   EL502
00641      IF AM-REI-TABLE = PRE-RTB                                    EL502
00642          GO TO 0699-EXIT.                                         EL502
00643                                                                   EL502
00644      MOVE AM-REI-TABLE           TO PRE-RTB.                      EL502
00645      MOVE +1                     TO X1.                           EL502
00646                                                                   EL502
00647      IF RTB-SW = +0                                               EL502
00648          OPEN INPUT ERREIN                                        EL502
00649          IF REIN-FILE-STATUS = '00' OR '97'                       EL502
00650              MOVE +1             TO RTB-SW                        EL502
00651          ELSE                                                     EL502
00652              MOVE ' REIN OPEN ERROR- ' TO WS-ABEND-MESSAGE        EL502
00653              MOVE REIN-FILE-STATUS     TO WS-ABEND-FILE-STATUS    EL502
00654              GO TO ABEND-PGM                                      EL502
00655      ELSE                                                         EL502
00656          GO TO 0630-RTB-LOOP.                                     EL502
00657                                                                   EL502
00658  0610-READ-RTB.                                                   EL502
00659      READ ERREIN.                                                 EL502
00660                                                                   EL502
00661      IF REIN-FILE-STATUS = '10'                                   EL502
00662          GO TO 0620-RTB-CLOSE.                                    EL502
00663                                                                   EL502
00664      IF REIN-FILE-STATUS NOT = '00'                               EL502
00665          MOVE ' REIN READ ERROR   ' TO WS-ABEND-MESSAGE           EL502
00666          MOVE REIN-FILE-STATUS      TO WS-ABEND-FILE-STATUS       EL502
00667          GO TO ABEND-PGM.                                         EL502
00668                                                                   EL502
00669      IF RE-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL502
00670          GO TO 0610-READ-RTB.                                     EL502
00671                                                                   EL502
00672      IF RE-CODE NOT = 'A'                                         EL502
00673          GO TO 0610-READ-RTB.                                     EL502
00674                                                                   EL502
00675      MOVE RE-TABLE               TO XR-TAB (X1).                  EL502
00676                                                                   EL502
00677      ADD +1        TO X1.                                         EL502
00678                                                                   EL502
00679      GO TO 0610-READ-RTB.                                         EL502
00680                                                                   EL502
00681  0620-RTB-CLOSE.                                                  EL502
00682      CLOSE ERREIN.                                                EL502
00683                                                                   EL502
00684      MOVE +1 TO X1.                                               EL502
00685                                                                   EL502
00686  0630-RTB-LOOP.                                                   EL502
00687      IF PRE-RTB = XR-TAB (X1)                                     EL502
00688          GO TO 0699-EXIT.                                         EL502
00689                                                                   EL502
00690      IF PRE-RTB GREATER THAN XR-TAB (X1)                          EL502
00691          ADD +1 TO X1                                             EL502
00692          GO TO 0630-RTB-LOOP.                                     EL502
00693                                                                   EL502
00694      MOVE +1                     TO RTB-ERR.                      EL502
00695      MOVE PRE-RTB                TO ERR-RTB.                      EL502
00696      MOVE AM-ACCOUNT             TO ERR-RTB-ACC.                  EL502
00697      MOVE RTB-ERR-MSG            TO P-DATA.                       EL502
00698      MOVE '0'                    TO P-CTL.                        EL502
00699      PERFORM WRITE-A-LINE.                                        EL502
00700                                                                   EL502
00701  0699-EXIT.                                                       EL502
00702      EXIT.                                                        EL502
00703  EJECT                                                            EL502
00704  0700-VERIFY-CTB-RTN.                                             EL502
00705      IF DTE-COM-TBL-USED NOT = ' '                                EL502
00706          GO TO 0799-EXIT.                                         EL502
00707                                                                   EL502
00708      MOVE +1                     TO X2.                           EL502
00709                                                                   EL502
00710  0710-LOOP-COMM.                                                  EL502
00711      IF AM-L-COM (X2) NOT NUMERIC                                 EL502
00712          MOVE AM-L-COMA (X2)     TO X-CTB                         EL502
00713          PERFORM 0800-RD-CTB-RTN THRU 0899-RD-CTB-XIT.            EL502
00714                                                                   EL502
00715      IF AM-J-COM (X2) NOT NUMERIC                                 EL502
00716          MOVE AM-J-COMA (X2)     TO X-CTB                         EL502
00717          PERFORM 0800-RD-CTB-RTN THRU 0899-RD-CTB-XIT.            EL502
00718                                                                   EL502
00719      IF AM-A-COM (X2) NOT NUMERIC                                 EL502
00720          MOVE AM-A-COMA (X2)     TO X-CTB                         EL502
00721          PERFORM 0800-RD-CTB-RTN THRU 0899-RD-CTB-XIT.            EL502
00722                                                                   EL502
00723      ADD +1 TO X2.                                                EL502
00724                                                                   EL502
00725      IF X2 LESS THAN +011                                         EL502
00726          GO TO 0710-LOOP-COMM.                                    EL502
00727                                                                   EL502
00728  0799-EXIT.                                                       EL502
00729      EXIT.                                                        EL502
00730  EJECT                                                            EL502
00731  0800-RD-CTB-RTN.                                                 EL502
00732      IF X-CTB = PRE-CTB                                           EL502
00733          GO TO 0899-RD-CTB-XIT.                                   EL502
00734                                                                   EL502
00735      MOVE X-CTB                  TO PRE-CTB.                      EL502
00736      MOVE +1                     TO X1.                           EL502
00737                                                                   EL502
00738      IF CTB-SW = +0                                               EL502
00739          MOVE +1                 TO CTB-SW                        EL502
00740          OPEN INPUT ERCTBL                                        EL502
00741          MOVE LOW-VALUES         TO CT-CONTROL-PRIMARY            EL502
00742          IF COMM-FILE-STATUS = '00' OR '97'                       EL502
00743              NEXT SENTENCE                                        EL502
00744          ELSE                                                     EL502
00745              MOVE ' COMM OPEN ERROR  ' TO WS-ABEND-MESSAGE        EL502
00746              MOVE COMM-FILE-STATUS     TO WS-ABEND-FILE-STATUS    EL502
00747              GO TO ABEND-PGM                                      EL502
00748      ELSE                                                         EL502
00749          GO TO 0830-CTB-LOOP.                                     EL502
00750                                                                   EL502
00751  0810-RD-CTB.                                                     EL502
00752      READ ERCTBL.                                                 EL502
00753                                                                   EL502
00754      IF COMM-FILE-STATUS = '10'                                   EL502
00755          GO TO 0820-CTB-CLOSE.                                    EL502
00756                                                                   EL502
00757      IF COMM-FILE-STATUS NOT = '00'                               EL502
00758          MOVE ' COMM READ ERROR  ' TO WS-ABEND-MESSAGE            EL502
00759          MOVE COMM-FILE-STATUS     TO WS-ABEND-FILE-STATUS        EL502
00760          GO TO ABEND-PGM.                                         EL502
00761                                                                   EL502
00762      IF CT-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL502
00763          GO TO 0810-RD-CTB.                                       EL502
00764                                                                   EL502
00765      IF XC-TAB (X1) = HIGH-VALUES                                 EL502
00766          MOVE CT-TABLE           TO XC-TAB (X1).                  EL502
00767                                                                   EL502
00768      IF CT-TABLE = XC-TAB (X1)                                    EL502
00769          GO TO 0810-RD-CTB.                                       EL502
00770                                                                   EL502
00771      ADD +1         TO X1.                                        EL502
00772                                                                   EL502
00773      MOVE CT-TABLE               TO XC-TAB (X1).                  EL502
00774                                                                   EL502
00775      GO TO 0810-RD-CTB.                                           EL502
00776                                                                   EL502
00777  0820-CTB-CLOSE.                                                  EL502
00778      CLOSE ERCTBL.                                                EL502
00779                                                                   EL502
00780      MOVE +1                     TO X1.                           EL502
00781                                                                   EL502
00782  0830-CTB-LOOP.                                                   EL502
00783      IF PRE-CTB = XC-TAB (X1)                                     EL502
00784          GO TO 0899-RD-CTB-XIT.                                   EL502
00785                                                                   EL502
00786      IF PRE-CTB GREATER THAN XC-TAB (X1)                          EL502
00787          ADD +1 TO X1                                             EL502
00788          GO TO 0830-CTB-LOOP.                                     EL502
00789                                                                   EL502
00790      MOVE +1                     TO CTB-ERR.                      EL502
00791      MOVE PRE-CTB                TO ERR-CTB.                      EL502
00792      MOVE AM-ACCOUNT             TO ERR-CTB-ACC.                  EL502
00793      MOVE CTB-ERR-MSG            TO P-DATA.                       EL502
00794      MOVE '0'                    TO P-CTL.                        EL502
00795                                                                   EL502
00796      PERFORM WRITE-A-LINE.                                        EL502
00797                                                                   EL502
00798  0899-RD-CTB-XIT.                                                 EL502
00799      EXIT.                                                        EL502
00800  EJECT                                                            EL502
00801  1120-DATE-LAP-RTN.                                               EL502
00802      MOVE AM-CARRIER             TO ACC-XC-CARR.                  EL502
00803      MOVE AM-GROUPING            TO ACC-XC-GROUP.                 EL502
00804      MOVE AM-STATE               TO ACC-XC-ST.                    EL502
00805      MOVE AM-ACCOUNT             TO ACC-XC-NO.                    EL502
00806                                                                   EL502
00807      IF DTE-COMP-VG = SPACE                                       EL502
00808          MOVE SPACES             TO ACC-XC-CARR  ACC-XC-GROUP.    EL502
00809                                                                   EL502
00810      IF DTE-COMP-VG = '2'                                         EL502
00811          MOVE SPACES             TO ACC-XC-GROUP.                 EL502
00812                                                                   EL502
00813      IF DTE-COMP-VG = '3'                                         EL502
00814          MOVE SPACES             TO ACC-XC-CARR  ACC-XC-GROUP     EL502
00815                                     ACC-XC-ST.                    EL502
00816                                                                   EL502
00817      IF DTE-COMP-VG = '4'                                         EL502
00818          MOVE SPACES             TO ACC-XC-GROUP ACC-XC-ST.       EL502
00819                                                                   EL502
00820      IF ACC-XC-SEQ NOT = ACC-DC-SEQ                               EL502
00821          GO TO 1150-E-D-LAP.                                      EL502
00822                                                                   EL502
00823      IF AM-EFFECT-DT = ACC-DC-X                                      CL**8
00824          GO TO 1150-E-D-LAP.                                      EL502
00825                                                                   EL502
00826      IF AM-EFFECT-DT GREATER ACC-DC-X                                CL**8
00827          IF NOT NO-EP-EXTRACTS                                    EL502
00828              GO TO 1150-E-D-LAP                                   EL502
00829          ELSE                                                     EL502
00830              MOVE ACCT-DTE-GAP   TO MSG-SLOT                      EL502
00831      ELSE                                                         EL502
00832          MOVE ACCT-DTE-OVER      TO MSG-SLOT                      EL502
00833          MOVE '0302'             TO ABEND-CODE.                   EL502
00834                                                                   EL502
00835  1130-DATE-PRINT-ROUTINE.                                         EL502
00836      MOVE AM-CARRIER             TO ERR-PREFIX.                   EL502
00837      MOVE AM-GROUPING            TO ERR-GROUPING.                 EL502
00838      MOVE AM-STATE               TO ERR-ST.                       EL502
00839      MOVE AM-ACCOUNT             TO ERR-ACCT.                     EL502
00840      MOVE AM-EFFECT-DT           TO WS-WORK-DATE.                    CL*16
00841      MOVE WS-WORK-MM             TO ERR-EFF-MO.                      CL*17
00842      MOVE WS-WORK-DD             TO ERR-EFF-DA.                      CL*17
00843      MOVE WS-WORK-YY             TO ERR-EFF-YR.                      CL*17
00844      MOVE AM-EXPIRE-DT           TO WS-WORK-DATE.                    CL*16
00845      MOVE WS-WORK-MM             TO ERR-EXP-MO.                      CL*17
00846      MOVE WS-WORK-DD             TO ERR-EXP-DA.                      CL*17
00847      MOVE WS-WORK-YY             TO ERR-EXP-YR.                      CL*17
00848      MOVE AM-NAME                TO ERR-NAME.                     EL502
00849      MOVE DATE-RANGE-ERR-MSG     TO P-DATA.                       EL502
00850      MOVE '0'                    TO P-CTL.                        EL502
00851      PERFORM WRITE-A-LINE.                                        EL502
00852                                                                   EL502
00853  1140-DATE-RTN-END.                                               EL502
00854      GO TO 1150-E-D-LAP.                                          EL502
00855                                                                   EL502
00856  1150-E-D-LAP.                                                    EL502
00857      IF AM-EFFECT-DT GREATER THAN AM-EXPIRE-DT                       CL**8
00858          MOVE EXP-DATE-MSG       TO MSG-SLOT                      EL502
00859          PERFORM 1130-DATE-PRINT-ROUTINE                          EL502
00860          MOVE '0320'             TO ABEND-CODE.                   EL502
00861                                                                   EL502
00862      MOVE ACC-XC-SEQ             TO ACC-DC-SEQ.                   EL502
00863      MOVE AM-EXPIRE-DT           TO ACC-DC-X.                        CL**8
00864                                                                   EL502
00865  1199-E-THIS-RTN.                                                 EL502
00866      EXIT.                                                        EL502
00867  EJECT                                                            EL502
00868  8000-FINALIZE.                                                   EL502
00869      IF ACCOUNT-OPEN = 'Y'                                        EL502
00870         CLOSE ERACCTT.                                            EL502
00871                                                                   EL502
00872      CLOSE PRNTR                                                  EL502
00873            ERACCT.                                                EL502
00874                                                                   EL502
00875  8000-CLOSE-OTHER.                                                EL502
00876                              COPY ELCPRTCX.                       EL502
00877                                                                   EL502
00878  8000-EXIT.                                                       EL502
00879      EXIT.                                                        EL502
00880                                                                   EL502
00881  9000-EOJ.                                                        EL502
00882      IF DTE-PGM-OPT = 2                                           EL502
00883          GO TO 9010-PRT-END.                                      EL502
00884                                                                   EL502
00885      IF ABEND-CODE = '0302'                                       EL502
00886          MOVE    'DATE GAPS OR OVERLAPS ON ACCT MASTER'           EL502
00887                                  TO WS-ABEND-MESSAGE.             EL502
00888                                                                   EL502
00889      IF RTB-ERR NOT = +0                                          EL502
00890          MOVE    ' REINSURANCE TABLE ERROR'                       EL502
00891                                  TO WS-ABEND-MESSAGE              EL502
00892          MOVE '0302'             TO ABEND-CODE.                   EL502
00893                                                                   EL502
00894      IF CTB-ERR NOT = +0                                          EL502
00895          MOVE    ' COMMISSION TABLE ERROR'                        EL502
00896                                  TO WS-ABEND-MESSAGE              EL502
00897          MOVE '0302'             TO ABEND-CODE.                   EL502
00898                                                                   EL502
00899      IF ABEND-CODE = ZEROS                                        EL502
00900          GO TO 9010-PRT-END.                                      EL502
00901                                                                   EL502
00902      GO TO ABEND-PGM.                                             EL502
00903                                                                   EL502
00904  9010-PRT-END.                                                    EL502
00905      MOVE '0'                    TO P-CTL.                        EL502
00906      MOVE CNT                    TO D-CNT.                        EL502
00907                                                                   EL502
00908      COMPUTE BLK = (CNT / K2) + K1.                               EL502
00909                                                                   EL502
00910      MOVE BLK                    TO D-BLK.                        EL502
00911      MOVE DTL-1                  TO P-DATA.                       EL502
00912                                                                   EL502
00913      PERFORM WRITE-A-LINE.                                        EL502
00914                                                                   EL502
00915  9000-EXIT.                                                       EL502
00916      EXIT.                                                        EL502
00917  EJECT                                                            EL502
00918  WRITE-A-LINE SECTION.                                            EL502
00919                              COPY ELCWAL.                         EL502
00920                                                                   EL502
00921  WRITE-HEADINGS SECTION.                                          EL502
00922                                                                   EL502
00923  WHS-010.                                                         EL502
00924                                                                   EL502
00925      ADD +1  TO  WS-PAGE.                                         EL502
00926      MOVE WS-PAGE                TO WS-H3-PAGE.                   EL502
00927      MOVE PRT                    TO WS-SAVE-PRINT-RECORD.         EL502
00928      MOVE ZERO                   TO WS-LINE-COUNT.                EL502
00929                                                                   EL502
00930      MOVE WS-HEADING1            TO PRT.                          EL502
00931      PERFORM WRITE-PRINTER.                                       EL502
00932                                                                   EL502
00933      MOVE WS-HEADING2            TO PRT.                          EL502
00934      PERFORM WRITE-PRINTER.                                       EL502
00935                                                                   EL502
00936      MOVE WS-HEADING3            TO PRT.                          EL502
00937      PERFORM WRITE-PRINTER.                                       EL502
00938                                                                   EL502
00939      MOVE WS-HEADING4            TO PRT.                          EL502
00940      PERFORM WRITE-PRINTER.                                       EL502
00941                                                                   EL502
00942      MOVE +4                     TO WS-LINE-COUNT.                EL502
00943                                                                   EL502
00944  WHS-020.                                                         EL502
00945                                                                   EL502
00946      MOVE WS-SAVE-PRINT-RECORD   TO PRT.                          EL502
00947      MOVE '-'                    TO P-CTL.                           CL*12
00948                                                                   EL502
00949  WHS-EXIT.                                                        EL502
00950      EXIT.                                                        EL502
00951                                                                   EL502
00952                                                                   EL502
00953  WRITE-PRINTER SECTION.                                           EL502
00954                             COPY ELCWPS.                          EL502
00955                                                                   EL502
00956  WPS-020.                                                         EL502
00957                             COPY ELCPRT2X.                        EL502
00958  EJECT                                                            EL502
00959  ABEND-PGM  SECTION.                                              EL502
00960                             COPY ELCABEND.                        EL502
