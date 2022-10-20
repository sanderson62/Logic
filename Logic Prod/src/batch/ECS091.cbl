      $SET ALTER
00001  IDENTIFICATION DIVISION.                                         03/19/98
00002                                                                   ECS091
00003  PROGRAM-ID.                 ECS091.                                 LV003
00004 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS091
00005 *                            VMOD=2.006.                             CL**2
00006 *AUTHOR.         LOGIC, INC.                                      ECS091
00007 *                DALLAS, TEXAS.                                   ECS091
00008 *                                                                 ECS091
00009 *DATE-COMPILED.                                                   ECS091
00010                                                                   ECS091
00011 *SECURITY.   *****************************************************ECS091
00012 *            *                                                   *ECS091
00013 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS091
00014 *            *                                                   *ECS091
00015 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS091
00016 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS091
00017 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *ECS091
00018 *            *                                                   *ECS091
00019 *            *****************************************************ECS091
00020 *                                                                 ECS091
00021 *REMARKS.                                                         ECS091
00022 *        THIS PROGRAM RESTORES FILES FROM A BACKUP TAPE WITH      ECS091
00023 *        MULTI FILES CREATED IN 090. THE FILES ARE  DATE-FILE,    ECS091
00024 *        ACCOUNT MASTER, EPEC EXTRACT, DETAIL EXTRACT,REINSURANCE ECS091
00025 *        TABLES, COMMISSION TABLES, CREDIT FILES, AND COMPENSATIONECS091
00026 *        MASTER.                                                  ECS091
00027 *                                                                 ECS091
00028 *        THE DEFAULT OPTION RESTORES THE DATE, ACCOUNT MASTER,    ECS091
00029 *        EPEC REXTRACT, DETAIL EXTRACT, REINSURANCE TABLES,       ECS091
00030 *        COMMISSION TABLES, AND COMPENSATION MASTER.              ECS091
00031 *        WHEN A CONTROL CARD IS ENTERED, ONLY FILES REQUESTED     ECS091
00032 *        TO BE RESTORED WILL BE RESTORED.                         ECS091
00033 *                                                                 ECS091
00034 *        FORMAT IS   COL 1-6 ECS091                               ECS091
00035 *                    COL  8  DATE FILE          X = RESTORE       ECS091
00036 *                    COL 10  ACCOUNT MASTER     X = RESTORE       ECS091
00037 *                    COL 12  EPEC EXTRACT       X = RESTORE       ECS091
00038 *                    COL 14  DETAIL EXTRACT     X = RESTORE       ECS091
00039 *                    COL 16  REINS TABLES       X = RESTORE       ECS091
00040 *                    COL 18  COMM TABLES        X = RESTORE       ECS091
00041 *                    COL 20  CLASIC CREDIT FILE X = RESTORE       ECS091
00042 *                    COL 22  COMP MASTER        X = RESTORE       ECS091
00043 *                                                                 ECS091
00044 *        COLUMNS MUST CONTAIN EITHER AN X OR A SPACE.             ECS091
00045 *                                                                 ECS091
00046 *        FOR CLIENTS ON THE CLASIC CREDIT SYSTEM, THE EXTRACT     ECS091
00047 *        FILE FROM EL521 WILL BE RESTORED INSTEAD OF THE          ECS091
00048 *        PENDING FILE.                                            ECS091
00049  EJECT                                                            ECS091
00050  ENVIRONMENT DIVISION.                                            ECS091
00051  INPUT-OUTPUT SECTION.                                            ECS091
00052  FILE-CONTROL.                                                    ECS091
00053                                                                   ECS091
00054      SELECT CARD-FILE        ASSIGN TO SYS006-UR-2501-S-SYS006.   ECS091
00055      SELECT PRNT-FILE        ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS091
00056                                                                   ECS091
00057      SELECT BKUP-DATE        ASSIGN TO SYS013-UT-2400-S-SYS013.   ECS091
00058      SELECT BKUP-ACCT        ASSIGN TO SYS013-UT-2400-S-SYS013.   ECS091
00059      SELECT BKUP-EPEC        ASSIGN TO SYS013-UT-2400-S-SYS013.   ECS091
00060      SELECT BKUP-EXTR        ASSIGN TO SYS013-UT-2400-S-SYS013.   ECS091
00061      SELECT BKUP-REIN        ASSIGN TO SYS013-UT-2400-S-SYS013.   ECS091
00062      SELECT BKUP-CTBL        ASSIGN TO SYS013-UT-2400-S-SYS013.   ECS091
00063      SELECT BKUP-CRED        ASSIGN TO SYS013-UT-2400-S-SYS013.   ECS091
00064      SELECT BKUP-COMP        ASSIGN TO SYS013-UT-2400-S-SYS013.   ECS091
00065                                                                   ECS091
00066      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS091
00067      SELECT ACCT-FILE        ASSIGN TO SYS016-FBA1-ERACCTT        ECS091
00068                              ACCESS IS SEQUENTIAL                 ECS091
00069                              ORGANIZATION IS INDEXED              ECS091
00070                              FILE STATUS IS ERACCTT-FILE-STATUS   ECS091
00071                              RECORD KEY IS AM-CONTROL-KEY.        ECS091
00072      SELECT EPEC-FILE        ASSIGN TO SYS018-UT-2400-S-SYS018.   ECS091
00073      SELECT EXTR-FILE        ASSIGN TO SYS012-UT-2400-S-SYS012.   ECS091
00074      SELECT REIN-FILE        ASSIGN TO SYS014                     ECS091
00075                              ACCESS IS SEQUENTIAL                 ECS091
00076                              ORGANIZATION IS INDEXED              ECS091
00077                              FILE STATUS IS REIN-FILE-STATUS      ECS091
00078                              RECORD KEY IS REIN-KEY.              ECS091
00079      SELECT CTBL-FILE        ASSIGN TO SYS016                     ECS091
00080                              ACCESS IS SEQUENTIAL                 ECS091
00081                              ORGANIZATION IS INDEXED              ECS091
00082                              FILE STATUS IS COMM-FILE-STATUS      ECS091
00083                              RECORD KEY IS COMM-KEY.              ECS091
00084      SELECT CRED-FILE        ASSIGN TO SYS011-UT-2400-S-SYS011.   ECS091
00085      SELECT COMP-FILE        ASSIGN TO SYS018-UT-2400-S-SYS018.   ECS091
00086  EJECT                                                            ECS091
00087  DATA DIVISION.                                                   ECS091
00088  FILE SECTION.                                                    ECS091
00089                                                                   ECS091
00090  FD  CARD-FILE                                                    ECS091
00091      RECORDING MODE F                                             ECS091
00092      BLOCK CONTAINS 0 RECORDS
00093      LABEL RECORDS OMITTED.                                       ECS091
00094  01  C-REC.                                                       ECS091
00095      12  C-CODE              PIC X(6).                            ECS091
00096      12  FILLER              PIC X.                               ECS091
00097      12  C-DATE              PIC X.                               ECS091
00098      12  FILLER              PIC X.                               ECS091
00099      12  C-ACCT              PIC X.                               ECS091
00100      12  FILLER              PIC X.                               ECS091
00101      12  C-EPEC              PIC X.                               ECS091
00102      12  FILLER              PIC X.                               ECS091
00103      12  C-EXTR              PIC X.                               ECS091
00104      12  FILLER              PIC X.                               ECS091
00105      12  C-REIN              PIC X.                               ECS091
00106      12  FILLER              PIC X.                               ECS091
00107      12  C-CTBL              PIC X.                               ECS091
00108      12  FILLER              PIC X.                               ECS091
00109      12  C-CRED              PIC X.                               ECS091
00110      12  FILLER              PIC X.                               ECS091
00111      12  C-COMP              PIC X.                               ECS091
00112      12  FILLER              PIC X(58).                           ECS091
00113                                                                   ECS091
00114  FD  PRNT-FILE                                                    ECS091
00115      RECORDING MODE F                                             ECS091
00116      LABEL RECORDS OMITTED                                        ECS091
00117      RECORD CONTAINS 133 CHARACTERS.                              ECS091
00118  01  PRT-REC.                                                     ECS091
00119      12  PRT-LINE.                                                ECS091
00120          16  PRT-C           PIC X.                               ECS091
00121          16  PRT-RST         PIC X(132).                          ECS091
00122  EJECT                                                            ECS091
00123                                                                   ECS091
00124  FD  DISK-DATE                                                    ECS091
00125                              COPY ELCDTEFD.                       ECS091
00126                                                                   ECS091
00127  01  DATE-REC.                                                    ECS091
00128      12  FILLER              PIC X(100).                          ECS091
00129  EJECT                                                            ECS091
00130                                                                   ECS091
00131  FD  BKUP-DATE                                                    ECS091
00132      RECORDING MODE F                                             ECS091
00133      LABEL RECORDS STANDARD                                       ECS091
00134      BLOCK CONTAINS 0 RECORDS
00135      RECORD CONTAINS 100 CHARACTERS.                              ECS091
00136  01  BK-DATE.                                                     ECS091
00137      12  FILLER              PIC X(100).                          ECS091
00138                                                                   ECS091
00139  FD  BKUP-ACCT                                                    ECS091
00140      RECORDING MODE F                                             ECS091
00141      LABEL RECORDS STANDARD                                       ECS091
00142      BLOCK CONTAINS 0 RECORDS
00143      RECORD CONTAINS 2000 CHARACTERS.                             ECS091
00144  01  BK-ACCT.                                                     ECS091
00145      12  FILLER              PIC X(2000).                         ECS091
00146                                                                   ECS091
00147  FD  ACCT-FILE                                                    ECS091
00148      LABEL RECORDS ARE STANDARD                                   ECS091
00149      BLOCK CONTAINS 0 RECORDS
00150      RECORD CONTAINS 2000 CHARACTERS.                             ECS091
00151  01  ACCT-REC.                                                    ECS091
00152      12  FILLER              PIC XX.                              ECS091
00153      12  AM-CONTROL-KEY      PIC X(26).                           ECS091
00154      12  FILLER              PIC X(1972).                         ECS091
00155  EJECT                                                            ECS091
00156                                                                   ECS091
00157  FD  BKUP-EPEC                  COPY ECSEPCFD.                    ECS091
00158  01  BK-EPEC.                                                     ECS091
00159      12  FILLER              PIC X(325).                          ECS091
00160                                                                   ECS091
00161  FD  EPEC-FILE                                                    ECS091
00162      RECORDING MODE F                                             ECS091
00163      LABEL RECORDS STANDARD                                       ECS091
00164      BLOCK CONTAINS 0 RECORDS
00165      RECORD CONTAINS 325 CHARACTERS.                              ECS091
00166  01  EPEC-REC.                                                    ECS091
00167      12  FILLER              PIC X(325).                          ECS091
00168                                                                   ECS091
00169  FD  BKUP-EXTR                  COPY ECSEXTFD.                    ECS091
00170  01  BK-EXTR.                                                     ECS091
00171      12  FILLER              PIC X(510).                          ECS091
00172                                                                   ECS091
00173  FD  EXTR-FILE                                                    ECS091
00174      RECORDING MODE F                                             ECS091
00175      LABEL RECORDS STANDARD                                       ECS091
00176      BLOCK CONTAINS 0 RECORDS
00177      RECORD CONTAINS 510 CHARACTERS.                              ECS091
00178  01  EXTR-REC.                                                    ECS091
00179      12  FILLER              PIC X(510).                          ECS091
00180  EJECT                                                            ECS091
00181                                                                   ECS091
00182  FD  BKUP-REIN                                                    ECS091
00183      RECORDING MODE F                                             ECS091
00184      LABEL RECORDS STANDARD                                       ECS091
00185      BLOCK CONTAINS 0 RECORDS
00186      RECORD CONTAINS 4000 CHARACTERS.                             ECS091
00187  01  BK-REIN.                                                     ECS091
00188      12  FILLER              PIC X(4000).                         ECS091
00189                                                                   ECS091
00190  FD  REIN-FILE                                                    ECS091
00191      LABEL RECORDS STANDARD                                       ECS091
00192      RECORD CONTAINS 4000 CHARACTERS.                             ECS091
00193  01  REIN-REC.                                                    ECS091
00194      12  FILLER              PIC X(3).                            ECS091
00195      12  REIN-KEY            PIC X(7).                            ECS091
00196      12  FILLER              PIC X(3990).                         ECS091
00197                                                                   ECS091
00198  FD  BKUP-CTBL                                                    ECS091
00199      RECORDING MODE F                                             ECS091
00200      LABEL RECORDS STANDARD                                       ECS091
00201      BLOCK CONTAINS 0 RECORDS
00202      RECORD CONTAINS 200 CHARACTERS.                              ECS091
00203  01  BK-CTBL.                                                     ECS091
00204      12  FILLER              PIC X(200).                          ECS091
00205                                                                   ECS091
00206  FD  CTBL-FILE                                                    ECS091
00207      LABEL RECORDS STANDARD                                       ECS091
00208      RECORD CONTAINS 200 CHARACTERS.                              ECS091
00209  01  CTBL-REC.                                                    ECS091
00210      12  FILLER              PIC XX.                              ECS091
00211      12  COMM-KEY            PIC X(7).                            ECS091
00212      12  FILLER              PIC X(191).                          ECS091
00213  EJECT                                                            ECS091
00214  FD  BKUP-CRED                                                    ECS091
00215      RECORDING MODE F                                             ECS091
00216      LABEL RECORDS STANDARD                                       ECS091
00217      BLOCK CONTAINS 0 RECORDS
00218      RECORD CONTAINS 514 CHARACTERS.                              ECS091
00219  01  BK-CRED                 PIC X(514).                          ECS091
00220                                                                   ECS091
00221  FD  CRED-FILE                                                    ECS091
00222      RECORDING MODE F                                             ECS091
00223      LABEL RECORDS STANDARD                                       ECS091
00224      BLOCK CONTAINS 0 RECORDS
00225      RECORD CONTAINS 514 CHARACTERS.                              ECS091
00226  01  CRED-REC                PIC X(514).                          ECS091
00227                                                                   ECS091
00228  FD  BKUP-COMP                                                    ECS091
00229      LABEL RECORDS STANDARD.                                      ECS091
00230  01  BK-COMP                 PIC X(700).                          ECS091
00231                                                                   ECS091
00232  FD  COMP-FILE                                                    ECS091
00233                              COPY ECSCOIFD.                       ECS091
00234  01  COMP-MSTR-REC           PIC X(700).                          ECS091
00235  EJECT                                                            ECS091
00236  WORKING-STORAGE SECTION.                                         ECS091
00237  77  FILLER  PIC X(32) VALUE '********************************'.  ECS091
00238  77  FILLER  PIC X(32) VALUE '     ECS091 WORKING-STORAGE     '.  ECS091
00239  77  FILLER  PIC X(32) VALUE '*****V/M=2.006 *****************'.     CL**2
00240                                                                   ECS091
00241  77  LN-CTR                  PIC S999  COMP-3    VALUE +066.      ECS091
00242  77  SAV-NDX                 PIC S999  COMP-3    VALUE +0.        ECS091
00243  77  DATE-CTR                PIC S9(9) COMP-3    VALUE +0.        ECS091
00244  77  ACCT-CTR                PIC S9(9) COMP-3    VALUE +0.        ECS091
00245  77  EPEC-CTR                PIC S9(9) COMP-3    VALUE +0.        ECS091
00246  77  EXTR-CTR                PIC S9(9) COMP-3    VALUE +0.        ECS091
00247  77  REIN-CTR                PIC S9(9) COMP-3    VALUE +0.        ECS091
00248  77  CTBL-CTR                PIC S9(9) COMP-3    VALUE +0.        ECS091
00249  77  CRED-CTR                PIC S9(9) COMP-3    VALUE +0.        ECS091
00250  77  COMP-CTR                PIC S9(9) COMP-3    VALUE +0.        ECS091
00251  77  SPACE-NP                PIC X               VALUE '1'.       ECS091
00252  77  SPACE-1                 PIC X               VALUE ' '.       ECS091
00253  77  SPACE-2                 PIC X               VALUE '0'.       ECS091
00254  77  SPACE-3                 PIC X               VALUE '-'.       ECS091
00255  77  CCSW                    PIC 9               VALUE 0.         ECS091
00256  77  DATE-SW                 PIC X               VALUE 'X'.       ECS091
00257  77  ACCT-SW                 PIC X               VALUE 'X'.       ECS091
00258  77  EPEC-SW                 PIC X               VALUE 'X'.       ECS091
00259  77  EXTR-SW                 PIC X               VALUE 'X'.       ECS091
00260  77  REIN-SW                 PIC X               VALUE 'X'.       ECS091
00261  77  CTBL-SW                 PIC X               VALUE 'X'.       ECS091
00262  77  CRED-SW                 PIC X               VALUE ' '.       ECS091
00263  77  COMP-SW                 PIC X               VALUE 'X'.       ECS091
00264                                                                   ECS091
00265 * DEFAULT SWITCHES                                                ECS091
00266 *    RESTORE DATE FILE                                            ECS091
00267 *    RESTORE ACCOUNT MASTER                                       ECS091
00268 *    RESTORE EPEC EXTRACT FILE                                    ECS091
00269 *    RESTORE EXTRACT FILE                                         ECS091
00270 *    RESTORE REINSURANCE TABLES                                   ECS091
00271 *    RESTORE COMMISSION TABLES                                    ECS091
00272 *    RESTORE COMPENSATION MASTER                                  ECS091
00273 *    DO NOT RESTORE CRED TRANS FILE UNLESS CONTROL CARD           ECS091
00274 *           CONTAINS AN X IN COLUMN 20.                           ECS091
00275 * IF A CONTROL CARD IS ENTERED ALL SWITCHES WILL BE ALTERED.      ECS091
00276  EJECT                                                            ECS091
00277  01  MISC-WORK.                                                   ECS091
00278      12  ABEND-CODE.                                              ECS091
00279          16  ABEND-FILE-ID   PIC XX              VALUE ZEROS.     ECS091
00280          16  ABEND-REASON    PIC XX              VALUE ZEROS.     ECS091
00281      12  ABEND-OPTION        PIC X               VALUE 'Y'.       ECS091
00282      12  PGM-SUB             PIC S999 COMP       VALUE +091.      ECS091
00283      12  FIRST-TIME-SW       PIC X               VALUE 'Y'.       ECS091
00284          88  FIRST-TIME                          VALUE 'Y'.       ECS091
00285      12  FIRST-PASS-SWITCH   PIC 9               VALUE 1.         ECS091
00286          88  FIRST-PASS  VALUE 1.                                 ECS091
00287                                                                   ECS091
00288  01  WS-ABEND-AREAS.                                              ECS091
00289      12  WS-ABEND-MESSAGE       PIC X(80).                        ECS091
00290      12  WS-ABEND-FILE-STATUS   PIC XX           VALUE ZERO.      ECS091
00291      12  WS-RETURN-CODE         PIC S9(4)        COMP.            ECS091
00292      12 WS-ZERO                 PIC S9           VALUE ZERO       ECS091
00293                                                  COMP-3.          ECS091
00294  01  FILE-STATUS.                                                 ECS091
00295      12  ERACCTT-FILE-STATUS    PIC XX              VALUE '00'.   ECS091
00296      12  REIN-FILE-STATUS       PIC XX              VALUE '00'.   ECS091
00297      12  COMM-FILE-STATUS       PIC XX              VALUE '00'.   ECS091
00298                                                                   ECS091
00299  01  SAVE-PRINT.                                                  ECS091
00300      12  SAV-PRT-LINE    OCCURS 10 TIMES.                         ECS091
00301         16  SAV-CCSW         PIC X.                               ECS091
00302         16  SAV-LINE         PIC X(132).                          ECS091
00303                                                                   ECS091
00304  01  HEADINGS.                                                    ECS091
00305      12  HEAD-1.                                                  ECS091
00306          16  FILLER          PIC X(46)           VALUE SPACES.    ECS091
00307          16  FILLER          PIC X(32)           VALUE            ECS091
00308                  'SYSTEM BACKUP FILE RESTORATATION'.              ECS091
00309          16  FILLER          PIC X(46)           VALUE SPACES.    ECS091
00310          16  FILLER          PIC X(8)            VALUE 'ECS-091'. ECS091
00311      12  HEAD-2.                                                  ECS091
00312          16  FILLER          PIC X(47)           VALUE SPACES.    ECS091
00313          16  HD-CLIENT       PIC X(30).                           ECS091
00314          16  FILLER          PIC X(47)           VALUE SPACES.    ECS091
00315          16  HD-IPL          PIC X(8)            VALUE SPACES.    ECS091
00316      12  HEAD-3.                                                  ECS091
00317          16  FILLER          PIC X(52)           VALUE SPACES.    ECS091
00318          16  HD-DATE         PIC X(18).                           ECS091
00319          16  FILLER          PIC X(42)           VALUE SPACES.    ECS091
00320          16  FILLER          PIC X(11)           VALUE            ECS091
00321                  'PAGE      1'.                                   ECS091
00322      12  HEAD-4.                                                  ECS091
00323          16  FILLER     PIC X(32)                VALUE            ECS091
00324                  '     FILE NAME                  '.              ECS091
00325          16  FILLER     PIC X(14)                VALUE            ECS091
00326                  '  RECORD COUNT'.                                ECS091
00327                                                                   ECS091
00328  01  P-REC.                                                       ECS091
00329      12  P-CCSW              PIC X.                               ECS091
00330      12  P-LINE.                                                  ECS091
00331          16  P-DESC          PIC X(32).                           ECS091
00332          16  P-CTR           PIC Z,ZZZ,ZZZ,ZZZ-.                  ECS091
00333          16  FILLER          PIC X(6).                            ECS091
00334          16  P-MSG           PIC X(40).                           ECS091
00335          16  FILLER          PIC X(40).                           ECS091
00336                                                                   ECS091
00337                              COPY ELCDTECX.                       ECS091
00338                                                                   ECS091
00339                              COPY ELCDTEVR.                       ECS091
00340  EJECT                                                            ECS091
00341  PROCEDURE DIVISION.                                              ECS091
00342                                                                   ECS091
00343  0100-INTIALIZE-RTN.                                              ECS091
00344      OPEN INPUT  CARD-FILE                                        ECS091
00345           OUTPUT PRNT-FILE.                                       ECS091
00346                                                                   ECS091
00347      MOVE HIGH-VALUE             TO SAVE-PRINT.                   ECS091
00348      MOVE SPACES                 TO P-REC.                        ECS091
00349      MOVE 0                      TO CCSW.                         ECS091
00350                                                                   ECS091
00351  0110-READ-CONTROL.                                               ECS091
00352      READ CARD-FILE AT END                                        ECS091
00353          GO TO 0140-DEFAULT-RTN.                                  ECS091
00354                                                                   ECS091
00355  0120-HAVE-CONTROL-CARD.                                          ECS091
00356      IF FIRST-PASS                                                ECS091
00357          MOVE 2                 TO FIRST-PASS-SWITCH              ECS091
00358          ALTER 0140-DEFAULT-RTN TO PROCEED TO 0160-PROCESS-RTN.   ECS091
00359                                                                   ECS091
00360  0130-PROCESS-CONTROL-CARD.                                       ECS091
00361      MOVE C-REC                  TO P-DESC.                       ECS091
00362                                                                   ECS091
00363      IF C-CODE NOT = 'ECS091'                                     ECS091
00364          MOVE 'INVALID CODE'     TO P-MSG                         ECS091
00365          GO TO 9000-ABEND.                                        ECS091
00366                                                                   ECS091
00367      IF C-DATE = ' ' OR 'X'                                       ECS091
00368          MOVE C-DATE             TO DATE-SW                       ECS091
00369      ELSE                                                         ECS091
00370          MOVE 'DATE SWITCH ERROR' TO P-MSG                        ECS091
00371          GO TO 9000-ABEND.                                        ECS091
00372                                                                   ECS091
00373      IF C-ACCT = ' ' OR 'X'                                       ECS091
00374          MOVE C-ACCT             TO ACCT-SW                       ECS091
00375      ELSE                                                         ECS091
00376          MOVE 'ACCT MASTER SWITCH ERROR' TO P-MSG                 ECS091
00377          GO TO 9000-ABEND.                                        ECS091
00378                                                                   ECS091
00379      IF C-EPEC = ' ' OR 'X'                                       ECS091
00380          MOVE C-EXTR             TO EXTR-SW                       ECS091
00381      ELSE                                                         ECS091
00382          MOVE 'EPEC EXTRACT SWITCH ERROR' TO P-MSG                ECS091
00383          GO TO 9000-ABEND.                                        ECS091
00384                                                                   ECS091
00385      IF C-EXTR = ' ' OR 'X'                                       ECS091
00386          MOVE C-EXTR             TO EXTR-SW                       ECS091
00387      ELSE                                                         ECS091
00388          MOVE 'DETAIL EXTRACT SWITCH ERROR' TO P-MSG              ECS091
00389          GO TO 9000-ABEND.                                        ECS091
00390                                                                   ECS091
00391      IF C-REIN = ' ' OR 'X'                                       ECS091
00392          MOVE C-REIN             TO REIN-SW                       ECS091
00393      ELSE                                                         ECS091
00394          MOVE 'REIN TBL SWITCH ERROR' TO P-MSG                    ECS091
00395          GO TO 9000-ABEND.                                        ECS091
00396                                                                   ECS091
00397      IF C-CTBL = ' ' OR 'X'                                       ECS091
00398          MOVE C-CTBL             TO CTBL-SW                       ECS091
00399      ELSE                                                         ECS091
00400          MOVE 'COMM TBL SWITCH ERROR' TO P-MSG                    ECS091
00401          GO TO 9000-ABEND.                                        ECS091
00402                                                                   ECS091
00403      IF C-CRED = ' ' OR 'X'                                       ECS091
00404          MOVE C-CRED             TO CRED-SW                       ECS091
00405      ELSE                                                         ECS091
00406          MOVE 'CLASIC CREDIT SWITCH ERROR' TO P-MSG               ECS091
00407          GO TO 9000-ABEND.                                        ECS091
00408                                                                   ECS091
00409      IF C-COMP = ' ' OR 'X'                                       ECS091
00410          MOVE C-COMP             TO CTBL-SW                       ECS091
00411      ELSE                                                         ECS091
00412          MOVE 'COMP MASTER SWITCH ERROR' TO P-MSG                 ECS091
00413          GO TO 9000-ABEND.                                        ECS091
00414                                                                   ECS091
00415      MOVE 'CONTROL CARD ENTERED - REQUESTED OPTIONS' TO P-MSG.    ECS091
00416                                                                   ECS091
00417      PERFORM 0200-SAV-RTN THRU 0299-EXIT.                         ECS091
00418                                                                   ECS091
00419      GO TO 0110-READ-CONTROL.                                     ECS091
00420                                                                   ECS091
00421  0140-DEFAULT-RTN.                                                ECS091
00422      GO TO 0150-ACTUAL-DEFAULT.                                   ECS091
00423                                                                   ECS091
00424  0150-ACTUAL-DEFAULT.                                             ECS091
00425      MOVE 'NO CONTROL CARD ENTERED - DEFAULT OPTIONS' TO P-LINE.  ECS091
00426                                                                   ECS091
00427      PERFORM 0200-SAV-RTN THRU 0299-EXIT.                         ECS091
00428                                                                   ECS091
00429  0160-PROCESS-RTN.                                                ECS091
00430      CLOSE CARD-FILE.                                             ECS091
00431                                                                   ECS091
00432      PERFORM 0600-RESTORE-DATE THRU 0699-EXIT.                    ECS091
00433      PERFORM 0500-SET-UP-DATE  THRU 0599-EXIT.                    ECS091
00434      PERFORM 0700-RESTORE-ACCT THRU 0799-EXIT.                    ECS091
00435      PERFORM 0800-RESTORE-EPEC THRU 0899-EXIT.                    ECS091
00436      PERFORM 0900-RESTORE-EXTR THRU 0999-EXIT.                    ECS091
00437      PERFORM 1000-RESTORE-REIN THRU 1099-EXIT.                    ECS091
00438      PERFORM 1100-RESTORE-CTBL THRU 1199-EXIT.                    ECS091
00439      PERFORM 1200-RESTORE-CRED THRU 1299-EXIT.                    ECS091
00440      PERFORM 1300-RESTORE-COMP THRU 1399-EXIT.                    ECS091
00441                                                                   ECS091
00442      GO TO 1600-CLS-FILES.                                        ECS091
00443  EJECT                                                            ECS091
00444  0200-SAV-RTN.                                                    ECS091
00445      ADD +1 TO SAV-NDX.                                           ECS091
00446      MOVE P-REC                  TO SAV-PRT-LINE (SAV-NDX).       ECS091
00447                                                                   ECS091
00448  0299-EXIT.                                                       ECS091
00449      EXIT.                                                        ECS091
00450                                                                   ECS091
00451  0300-PRT-RTN.                                                    ECS091
00452      MOVE P-REC                  TO PRT-REC.                      ECS091
00453      MOVE P-CCSW                 TO CCSW.                         ECS091
00454                                                                   ECS091
00455      WRITE PRT-REC AFTER ADVANCING CCSW.                          ECS091
00456                                                                   ECS091
00457      MOVE SPACES                 TO P-REC.                        ECS091
00458      MOVE SPACE-2                TO P-CCSW.                       ECS091
00459                                                                   ECS091
00460      IF CCSW = SPACE-NP                                           ECS091
00461          MOVE +1                 TO LN-CTR                        ECS091
00462      ELSE                                                         ECS091
00463          IF CCSW = SPACE-1                                        ECS091
00464              ADD +1 TO LN-CTR                                     ECS091
00465          ELSE                                                     ECS091
00466              IF CCSW = SPACE-2                                    ECS091
00467                  ADD +2 TO LN-CTR                                 ECS091
00468              ELSE                                                 ECS091
00469                  ADD +3 TO LN-CTR.                                ECS091
00470                                                                   ECS091
00471  0399-EXIT.                                                       ECS091
00472      EXIT.                                                        ECS091
00473                                                                   ECS091
00474  0400-HEAD-RTN.                                                   ECS091
00475      MOVE SPACE-NP              TO P-CCSW.                        ECS091
00476      MOVE HEAD-1                TO P-LINE.                        ECS091
00477      PERFORM 0300-PRT-RTN THRU 0399-EXIT.                         ECS091
00478                                                                   ECS091
00479      MOVE SPACE-1               TO P-CCSW.                        ECS091
00480      MOVE HEAD-2                TO P-LINE.                        ECS091
00481      PERFORM 0300-PRT-RTN THRU 0399-EXIT.                         ECS091
00482                                                                   ECS091
00483      MOVE HEAD-3                TO P-LINE.                        ECS091
00484      PERFORM 0300-PRT-RTN THRU 0399-EXIT.                         ECS091
00485                                                                   ECS091
00486      MOVE HEAD-4                TO P-LINE.                        ECS091
00487      PERFORM 0300-PRT-RTN THRU 0399-EXIT 2 TIMES.                 ECS091
00488                                                                   ECS091
00489  0499-EXIT.                                                       ECS091
00490      EXIT.                                                        ECS091
00491  EJECT                                                            ECS091
00492  0500-SET-UP-DATE.                                                ECS091
00493                              COPY ELCDTERX.                       ECS091
00494  EJECT                                                            ECS091
00495      MOVE COMPANY-NAME          TO HD-CLIENT.                     ECS091
00496      MOVE WS-CURRENT-DATE       TO HD-IPL.                        ECS091
00497      MOVE ALPH-DATE             TO HD-DATE.                       ECS091
00498                                                                   ECS091
00499      PERFORM 0400-HEAD-RTN THRU 0499-EXIT.                        ECS091
00500                                                                   ECS091
00501      MOVE +0                    TO SAV-NDX.                       ECS091
00502                                                                   ECS091
00503  0510-LOOP-PRT.                                                   ECS091
00504      ADD +1                     TO SAV-NDX.                       ECS091
00505                                                                   ECS091
00506      IF SAV-CCSW (SAV-NDX) = HIGH-VALUES                          ECS091
00507          GO TO 0599-EXIT.                                         ECS091
00508                                                                   ECS091
00509      MOVE SAV-PRT-LINE (SAV-NDX) TO P-REC.                        ECS091
00510                                                                   ECS091
00511      PERFORM 0300-PRT-RTN THRU 0399-EXIT.                         ECS091
00512                                                                   ECS091
00513      GO TO 0510-LOOP-PRT.                                         ECS091
00514                                                                   ECS091
00515  0599-EXIT.                                                       ECS091
00516      EXIT.                                                        ECS091
00517  EJECT                                                            ECS091
00518  0600-RESTORE-DATE.                                               ECS091
00519      OPEN INPUT BKUP-DATE.                                        ECS091
00520 *  FIRST RESTORE - COMPLETE REWIND OF INPUT TAPE.                 ECS091
00521                                                                   ECS091
00522      IF DATE-SW = 'X'                                             ECS091
00523          OPEN OUTPUT DISK-DATE.                                   ECS091
00524                                                                   ECS091
00525  0610-READ-DATE.                                                  ECS091
00526      READ BKUP-DATE AT END                                        ECS091
00527          GO TO 0620-END-DATE.                                     ECS091
00528                                                                   ECS091
00529      ADD +1 TO DATE-CTR.                                          ECS091
00530                                                                   ECS091
00531      IF DATE-SW NOT = 'X'                                         ECS091
00532          GO TO 0610-READ-DATE.                                    ECS091
00533                                                                   ECS091
00534      MOVE BK-DATE                TO DATE-REC.                     ECS091
00535                                                                   ECS091
00536      WRITE DATE-REC.                                              ECS091
00537                                                                   ECS091
00538      GO TO 0610-READ-DATE.                                        ECS091
00539                                                                   ECS091
00540  0620-END-DATE.                                                   ECS091
00541      IF DATE-SW = 'X'                                             ECS091
00542          CLOSE DISK-DATE.                                         ECS091
00543                                                                   ECS091
00544      CLOSE BKUP-DATE WITH NO REWIND.                              ECS091
00545                                                                   ECS091
00546      MOVE 'DATE CARD'            TO P-DESC.                       ECS091
00547      MOVE DATE-CTR               TO P-CTR.                        ECS091
00548                                                                   ECS091
00549      IF DATE-SW = 'X'                                             ECS091
00550          MOVE 'FILE RESTORED'       TO P-MSG                      ECS091
00551      ELSE                                                         ECS091
00552          MOVE '* FILE NOT RESTORED' TO P-MSG.                     ECS091
00553                                                                   ECS091
00554      PERFORM 0200-SAV-RTN THRU 0299-EXIT.                         ECS091
00555                                                                   ECS091
00556  0699-EXIT.                                                       ECS091
00557      EXIT.                                                        ECS091
00558  EJECT                                                            ECS091
00559  0700-RESTORE-ACCT.                                               ECS091
00560      OPEN INPUT BKUP-ACCT WITH NO REWIND.                         ECS091
00561                                                                   ECS091
00562      IF ACCT-SW = 'X'                                             ECS091
00563          OPEN OUTPUT ACCT-FILE.                                   ECS091
00564                                                                   ECS091
00565      IF ERACCTT-FILE-STATUS  = '00' OR '97'                       ECS091
00566          NEXT SENTENCE                                            ECS091
00567        ELSE                                                       ECS091
00568          MOVE 'ERACCTT OPEN ERROR- ' TO WS-ABEND-MESSAGE          ECS091
00569          MOVE ERACCTT-FILE-STATUS    TO WS-ABEND-FILE-STATUS      ECS091
00570          PERFORM ABEND-PGM.                                       ECS091
00571                                                                   ECS091
00572  0710-READ-ACCT.                                                  ECS091
00573      READ BKUP-ACCT AT END                                        ECS091
00574          GO TO 0720-END-ACCT.                                     ECS091
00575                                                                   ECS091
00576      ADD +1 TO ACCT-CTR.                                          ECS091
00577                                                                   ECS091
00578      IF ACCT-SW NOT = 'X'                                         ECS091
00579          GO TO 0710-READ-ACCT.                                    ECS091
00580                                                                   ECS091
00581      MOVE BK-ACCT                TO ACCT-REC.                     ECS091
00582                                                                   ECS091
00583      WRITE ACCT-REC.                                              ECS091
00584                                                                   ECS091
00585      IF ERACCTT-FILE-STATUS NOT = ZERO                               CL**3
00586          MOVE 'ERACCTT WRITE ERROR-'   TO WS-ABEND-MESSAGE        ECS091
00587          MOVE ERACCTT-FILE-STATUS      TO WS-ABEND-FILE-STATUS    ECS091
00588          GO TO ABEND-PGM.                                         ECS091
00589                                                                   ECS091
00590      GO TO 0710-READ-ACCT.                                        ECS091
00591                                                                   ECS091
00592  0720-END-ACCT.                                                   ECS091
00593      MOVE 'ACCOUNT MASTER'       TO P-DESC.                       ECS091
00594      MOVE ACCT-CTR               TO P-CTR.                        ECS091
00595                                                                   ECS091
00596      IF ACCT-SW = 'X'                                             ECS091
00597          MOVE 'FILE RESTORED'       TO P-MSG                      ECS091
00598      ELSE                                                         ECS091
00599          MOVE '* FILE NOT RESTORED' TO P-MSG.                     ECS091
00600                                                                   ECS091
00601      PERFORM 0300-PRT-RTN THRU 0399-EXIT.                         ECS091
00602                                                                   ECS091
00603      IF ACCT-SW = 'X'                                             ECS091
00604          CLOSE ACCT-FILE.                                         ECS091
00605                                                                   ECS091
00606      CLOSE BKUP-ACCT WITH NO REWIND.                              ECS091
00607                                                                   ECS091
00608  0799-EXIT.                                                       ECS091
00609      EXIT.                                                        ECS091
00610  EJECT                                                            ECS091
00611  0800-RESTORE-EPEC.                                               ECS091
00612      OPEN INPUT BKUP-EPEC WITH NO REWIND.                         ECS091
00613                                                                   ECS091
00614      IF EPEC-SW = 'X'                                             ECS091
00615          OPEN OUTPUT EPEC-FILE.                                   ECS091
00616                                                                   ECS091
00617  0810-READ-EPEC.                                                  ECS091
00618      READ BKUP-EPEC AT END                                        ECS091
00619          GO TO 0820-END-EPEC.                                     ECS091
00620                                                                   ECS091
00621      ADD +1 TO EPEC-CTR.                                          ECS091
00622                                                                   ECS091
00623      IF EPEC-SW NOT = 'X'                                         ECS091
00624          GO TO 0810-READ-EPEC.                                    ECS091
00625                                                                   ECS091
00626      MOVE BK-EPEC                TO EPEC-REC.                     ECS091
00627                                                                   ECS091
00628      WRITE EPEC-REC.                                              ECS091
00629                                                                   ECS091
00630      GO TO 0810-READ-EPEC.                                        ECS091
00631                                                                   ECS091
00632  0820-END-EPEC.                                                   ECS091
00633      MOVE 'EPEC EXT FILE'        TO P-DESC.                       ECS091
00634      MOVE EPEC-CTR               TO P-CTR.                        ECS091
00635                                                                   ECS091
00636      IF EPEC-SW = 'X'                                             ECS091
00637          MOVE 'FILE RESTORED'    TO P-MSG                         ECS091
00638      ELSE                                                         ECS091
00639          MOVE '* FILE NOT RESTORED' TO P-MSG.                     ECS091
00640                                                                   ECS091
00641      PERFORM 0300-PRT-RTN THRU 0399-EXIT.                         ECS091
00642                                                                   ECS091
00643      IF EPEC-SW = 'X'                                             ECS091
00644          CLOSE EPEC-FILE.                                         ECS091
00645                                                                   ECS091
00646      CLOSE BKUP-EPEC WITH NO REWIND.                              ECS091
00647                                                                   ECS091
00648  0899-EXIT.                                                       ECS091
00649      EXIT.                                                        ECS091
00650      EJECT                                                        ECS091
00651                                                                   ECS091
00652  0900-RESTORE-EXTR.                                               ECS091
00653      OPEN INPUT BKUP-EXTR WITH NO REWIND.                         ECS091
00654                                                                   ECS091
00655      IF EXTR-SW = 'X'                                             ECS091
00656          OPEN OUTPUT EXTR-FILE.                                   ECS091
00657                                                                   ECS091
00658  0810-READ-EXTR.                                                  ECS091
00659      READ BKUP-EXTR AT END                                        ECS091
00660          GO TO 0820-END-EXTR.                                     ECS091
00661                                                                   ECS091
00662      ADD +1 TO EXTR-CTR.                                          ECS091
00663                                                                   ECS091
00664      IF EXTR-SW NOT = 'X'                                         ECS091
00665          GO TO 0810-READ-EXTR.                                    ECS091
00666                                                                   ECS091
00667      MOVE BK-EXTR                TO EXTR-REC.                     ECS091
00668                                                                   ECS091
00669      WRITE EXTR-REC.                                              ECS091
00670                                                                   ECS091
00671      GO TO 0810-READ-EXTR.                                        ECS091
00672                                                                   ECS091
00673  0820-END-EXTR.                                                   ECS091
00674      MOVE 'EXTRACT FILE'         TO P-DESC.                       ECS091
00675      MOVE EXTR-CTR               TO P-CTR.                        ECS091
00676                                                                   ECS091
00677      IF EXTR-SW = 'X'                                             ECS091
00678          MOVE 'FILE RESTORED'    TO P-MSG                         ECS091
00679      ELSE                                                         ECS091
00680          MOVE '* FILE NOT RESTORED' TO P-MSG.                     ECS091
00681                                                                   ECS091
00682      PERFORM 0300-PRT-RTN THRU 0399-EXIT.                         ECS091
00683                                                                   ECS091
00684      IF EXTR-SW = 'X'                                             ECS091
00685          CLOSE EXTR-FILE.                                         ECS091
00686                                                                   ECS091
00687      CLOSE BKUP-EXTR WITH NO REWIND.                              ECS091
00688                                                                   ECS091
00689  0999-EXIT.                                                       ECS091
00690      EXIT.                                                        ECS091
00691  EJECT                                                            ECS091
00692  1000-RESTORE-REIN.                                               ECS091
00693      OPEN INPUT BKUP-REIN WITH NO REWIND.                         ECS091
00694                                                                   ECS091
00695      IF REIN-SW = 'X'                                             ECS091
00696          OPEN OUTPUT REIN-FILE                                    ECS091
00697          IF REIN-FILE-STATUS  = '00' OR '97'                      ECS091
00698             NEXT SENTENCE                                         ECS091
00699          ELSE                                                     ECS091
00700              MOVE 'REIN OPEN ERROR-' TO WS-ABEND-MESSAGE          ECS091
00701              MOVE REIN-FILE-STATUS   TO WS-ABEND-FILE-STATUS      ECS091
00702              GO TO ABEND-PGM.                                     ECS091
00703                                                                   ECS091
00704  1010-READ-REIN.                                                  ECS091
00705      READ BKUP-REIN AT END                                        ECS091
00706          GO TO 1020-END-REIN.                                     ECS091
00707                                                                   ECS091
00708      ADD +1 TO REIN-CTR.                                          ECS091
00709                                                                   ECS091
00710      IF REIN-SW NOT = 'X'                                         ECS091
00711          GO TO 1010-READ-REIN.                                    ECS091
00712                                                                   ECS091
00713      MOVE BK-REIN                TO REIN-REC.                     ECS091
00714                                                                   ECS091
00715      WRITE REIN-REC.                                              ECS091
00716                                                                   ECS091
00717      IF REIN-FILE-STATUS NOT = '00'                               ECS091
00718          MOVE 'REIN WRITE ERROR- '      TO WS-ABEND-MESSAGE       ECS091
00719          MOVE REIN-FILE-STATUS          TO WS-ABEND-FILE-STATUS   ECS091
00720          GO TO ABEND-PGM.                                         ECS091
00721                                                                   ECS091
00722      GO TO 1010-READ-REIN.                                        ECS091
00723                                                                   ECS091
00724  1020-END-REIN.                                                   ECS091
00725      MOVE 'REINSURANCE TABLES'   TO P-DESC.                       ECS091
00726      MOVE REIN-CTR               TO P-CTR.                        ECS091
00727                                                                   ECS091
00728      IF REIN-SW = 'X'                                             ECS091
00729          MOVE 'FILE RESTORED'    TO P-MSG                         ECS091
00730      ELSE                                                         ECS091
00731          MOVE '* FILE NOT RESTORED' TO P-MSG.                     ECS091
00732                                                                   ECS091
00733      PERFORM 0300-PRT-RTN THRU 0399-EXIT.                         ECS091
00734                                                                   ECS091
00735      IF REIN-SW = 'X'                                             ECS091
00736          CLOSE REIN-FILE.                                         ECS091
00737                                                                   ECS091
00738      CLOSE BKUP-REIN WITH NO REWIND.                              ECS091
00739                                                                   ECS091
00740  1099-EXIT.                                                       ECS091
00741      EXIT.                                                        ECS091
00742  EJECT                                                            ECS091
00743  1100-RESTORE-CTBL.                                               ECS091
00744      OPEN INPUT BKUP-CTBL WITH NO REWIND.                         ECS091
00745                                                                   ECS091
00746      IF CTBL-SW = 'X'                                             ECS091
00747          OPEN OUTPUT CTBL-FILE                                    ECS091
00748          IF COMM-FILE-STATUS = '00' OR '97'                       ECS091
00749             NEXT SENTENCE                                         ECS091
00750          ELSE                                                     ECS091
00751              MOVE 'CTBL OPEN ERROE- ' TO WS-ABEND-MESSAGE         ECS091
00752              MOVE COMM-FILE-STATUS    TO WS-ABEND-FILE-STATUS     ECS091
00753              GO TO ABEND-PGM.                                     ECS091
00754                                                                   ECS091
00755  1110-READ-CTBL.                                                  ECS091
00756      READ BKUP-CTBL AT END                                        ECS091
00757          GO TO 1120-END-CTBL.                                     ECS091
00758                                                                   ECS091
00759      ADD +1 TO CTBL-CTR.                                          ECS091
00760                                                                   ECS091
00761      IF CTBL-SW NOT = 'X'                                         ECS091
00762          GO TO 1110-READ-CTBL.                                    ECS091
00763                                                                   ECS091
00764      MOVE BK-CTBL                TO CTBL-REC.                     ECS091
00765                                                                   ECS091
00766      WRITE CTBL-REC.                                              ECS091
00767                                                                   ECS091
00768      IF COMM-FILE-STATUS NOT = '00'                               ECS091
00769          MOVE 'CTBL WRITE ERROR- '     TO WS-ABEND-MESSAGE        ECS091
00770          MOVE COMM-FILE-STATUS         TO WS-ABEND-FILE-STATUS    ECS091
00771          GO TO ABEND-PGM.                                         ECS091
00772                                                                   ECS091
00773      GO TO 1110-READ-CTBL.                                        ECS091
00774                                                                   ECS091
00775  1120-END-CTBL.                                                   ECS091
00776      MOVE 'COMMISSION TABLES'    TO P-DESC.                       ECS091
00777      MOVE CTBL-CTR               TO P-CTR.                        ECS091
00778                                                                   ECS091
00779      IF CTBL-SW = 'X'                                             ECS091
00780          MOVE 'FILE RESTORED'    TO P-MSG                         ECS091
00781      ELSE                                                         ECS091
00782          MOVE '* FILE NOT RESTORED' TO P-MSG.                     ECS091
00783                                                                   ECS091
00784      PERFORM 0300-PRT-RTN THRU 0399-EXIT.                         ECS091
00785                                                                   ECS091
00786      IF CTBL-SW = 'X'                                             ECS091
00787          CLOSE CTBL-FILE.                                         ECS091
00788                                                                   ECS091
00789      CLOSE BKUP-CTBL WITH NO REWIND.                              ECS091
00790                                                                   ECS091
00791  1199-EXIT.                                                       ECS091
00792      EXIT.                                                        ECS091
00793  EJECT                                                            ECS091
00794  1200-RESTORE-CRED.                                               ECS091
00795      OPEN INPUT BKUP-CRED WITH NO REWIND.                         ECS091
00796                                                                   ECS091
00797      IF CRED-SW = 'X'                                             ECS091
00798          OPEN OUTPUT CRED-FILE.                                   ECS091
00799                                                                   ECS091
00800  1210-READ-CRED.                                                  ECS091
00801      READ BKUP-CRED AT END                                        ECS091
00802          GO TO 1220-END-CRED.                                     ECS091
00803                                                                   ECS091
00804      ADD +1 TO CRED-CTR.                                          ECS091
00805                                                                   ECS091
00806      IF CRED-SW NOT = 'X'                                         ECS091
00807          GO TO 1210-READ-CRED.                                    ECS091
00808                                                                   ECS091
00809      MOVE BK-CRED                TO CRED-REC.                     ECS091
00810                                                                   ECS091
00811      WRITE CRED-REC.                                              ECS091
00812                                                                   ECS091
00813      GO TO 1210-READ-CRED.                                        ECS091
00814                                                                   ECS091
00815  1220-END-CRED.                                                   ECS091
00816      MOVE 'CREDING FILE'         TO P-DESC.                       ECS091
00817      MOVE CRED-CTR               TO P-CTR.                        ECS091
00818                                                                   ECS091
00819      IF CRED-SW = 'X'                                             ECS091
00820          MOVE 'FILE RESTORED'       TO P-MSG                      ECS091
00821      ELSE                                                         ECS091
00822          MOVE '* FILE NOT RESTORED' TO P-MSG.                     ECS091
00823                                                                   ECS091
00824      PERFORM 0300-PRT-RTN THRU 0399-EXIT.                         ECS091
00825                                                                   ECS091
00826      IF CRED-SW = 'X'                                             ECS091
00827          CLOSE CRED-FILE.                                         ECS091
00828                                                                   ECS091
00829      CLOSE BKUP-CRED WITH LOCK.                                   ECS091
00830                                                                   ECS091
00831  1299-EXIT.                                                       ECS091
00832      EXIT.                                                        ECS091
00833  EJECT                                                            ECS091
00834                                                                   ECS091
00835  1300-RESTORE-COMP.                                               ECS091
00836      OPEN INPUT BKUP-COMP WITH NO REWIND.                         ECS091
00837                                                                   ECS091
00838      IF COMP-SW = 'X'                                             ECS091
00839          OPEN OUTPUT COMP-FILE.                                   ECS091
00840                                                                   ECS091
00841  1210-READ-COMP.                                                  ECS091
00842      READ BKUP-COMP AT END                                        ECS091
00843          GO TO 1220-END-COMP.                                     ECS091
00844                                                                   ECS091
00845      ADD +1 TO COMP-CTR.                                          ECS091
00846                                                                   ECS091
00847      IF COMP-SW NOT = 'X'                                         ECS091
00848          GO TO 1210-READ-COMP.                                    ECS091
00849                                                                   ECS091
00850      MOVE BK-COMP                TO COMP-MSTR-REC.                ECS091
00851                                                                   ECS091
00852      WRITE COMP-MSTR-REC.                                         ECS091
00853                                                                   ECS091
00854      GO TO 1210-READ-COMP.                                        ECS091
00855                                                                   ECS091
00856  1220-END-COMP.                                                   ECS091
00857      MOVE 'COMPENSATION FILE'    TO P-DESC.                       ECS091
00858      MOVE COMP-CTR               TO P-CTR.                        ECS091
00859                                                                   ECS091
00860      IF COMP-SW = 'X'                                             ECS091
00861          MOVE 'FILE RESTORED'    TO P-MSG                         ECS091
00862      ELSE                                                         ECS091
00863          MOVE '* FILE NOT RESTORED' TO P-MSG.                     ECS091
00864                                                                   ECS091
00865      PERFORM 0300-PRT-RTN THRU 0399-EXIT.                         ECS091
00866                                                                   ECS091
00867      IF COMP-SW = 'X'                                             ECS091
00868          CLOSE COMP-FILE.                                         ECS091
00869                                                                   ECS091
00870      CLOSE BKUP-COMP WITH LOCK.                                   ECS091
00871                                                                   ECS091
00872  1399-EXIT.                                                       ECS091
00873      EXIT.                                                        ECS091
00874  EJECT                                                            ECS091
00875  9000-ABEND.                                                      ECS091
00876      DISPLAY P-MSG.                                               ECS091
00877      DISPLAY ' JOB STEP ECS091 - ABORTED'.                        ECS091
00878                                                                   ECS091
00879      IF ABEND-CODE = ZEROS                                        ECS091
00880          MOVE '0301'             TO ABEND-CODE.                   ECS091
00881                                                                   ECS091
00882      GO TO ABEND-PGM.                                             ECS091
00883                                                                   ECS091
00884                                                                   ECS091
00885  1600-CLS-FILES.                                                  ECS091
00886      CLOSE PRNT-FILE.                                             ECS091
00887      GOBACK.                                                      ECS091
00888                                                                   ECS091
00889  ABEND-PGM SECTION.                                               ECS091
00890                                COPY ELCABEND.                     ECS091
