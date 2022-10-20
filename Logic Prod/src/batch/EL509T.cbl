00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL509
00003  PROGRAM-ID.                 EL509 .                                 LV004
00004 *              PROGRAM CONVERTED BY                               EL509
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL509
00006 *              CONVERSION DATE 03/21/96 10:04:58.                 EL509
00007 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             EL509
00008 *                            VMOD=2.018.                          EL509
00009                                                                   EL509
00009                                                                   EL509
00010 *AUTHOR.     LOGIC INC.                                           EL509
00011 *            DALLAS, TEXAS.                                       EL509
00012                                                                   EL509
00013 *DATE-COMPILED.                                                   EL509
00014                                                                   EL509
00015 *SECURITY.   *****************************************************EL509
00016 *            *                                                   *EL509
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL509
00018 *            *                                                   *EL509
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL509
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL509
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL509
00022 *            *                                                   *EL509
00023 *            *****************************************************EL509
00024                                                                   EL509
00025 *REMARKS.                                                         EL509
00026 *         THIS PROGRAM IS USED TO CREATE COMPENSATION MASTER      EL509
00027 *       DATA SET FOR THE CREDIT SYSTEM FROM (ERCCOMP). A CONTROL  EL509
00028 *       PAGE WILL BE PRINTED AT EOJ.                              EL509
00029                                                                   EL509
00030  ENVIRONMENT DIVISION.                                            EL509
00031                                                                   EL509
00032  INPUT-OUTPUT SECTION.                                            EL509
00033                                                                   EL509
00034  FILE-CONTROL.                                                    EL509
00035                                                                   EL509
00036      SELECT PRNTR          ASSIGN TO SYS008-UR-1403-S-SYS008.     EL509
00037                                                                   EL509
00038      SELECT FICH           ASSIGN TO SYS020-UT-2400-S-SYS020.     EL509
00039                                                                   EL509
00040      SELECT DISK-DATE      ASSIGN TO SYS019-FBA1-S-SYS019.        EL509
00041                                                                   EL509
00042      SELECT COMP-MSTR      ASSIGN TO SYS010-UT-FBA1-S-SYS010.     EL509
00043                                                                   EL509
00044      SELECT ERCOMP         ASSIGN TO SYS011-FBA1-ERCOMP           EL509
00045              ORGANIZATION IS INDEXED                              EL509
00046              ACCESS IS DYNAMIC                                    EL509
00047              RECORD KEY IS CO-CONTROL-PRIMARY                     EL509
00048              FILE STATUS IS COMPENSATION-FILE-STATUS.             EL509
00049                                                                   EL509
00050      SELECT ELCNTL         ASSIGN TO SYS012-FBA1-ELCNTL           EL509
00051              ORGANIZATION IS INDEXED                              EL509
00052              ACCESS IS DYNAMIC                                    EL509
00053              RECORD KEY IS CF-CONTROL-PRIMARY                     EL509
00054              FILE STATUS IS CONTROL-FILE-STATUS.                  EL509
00055                                                                   EL509
00056      SELECT ELREPT         ASSIGN TO SYS018-FBA1-ELREPT           EL509
00057              ORGANIZATION IS INDEXED                              EL509
00058              ACCESS IS DYNAMIC                                    EL509
00059              RECORD KEY IS RF-CONTROL-PRIMARY                     EL509
00060              FILE STATUS IS DTE-VSAM-FLAGS.                       EL509
00061                                                                   EL509
00062      SELECT ERMEBL                                                EL509
00063              ASSIGN SYS024-FBA1-ERMEBL                            EL509
00064              ORGANIZATION INDEXED                                 EL509
00065              ACCESS DYNAMIC                                       EL509
00066              RECORD KEY ME-CONTROL-PRIMARY                        EL509
00067              FILE STATUS ERMEBL-FILE-STATUS.                      EL509
00068                                                                   EL509
00069      EJECT                                                        EL509
00070  DATA DIVISION.                                                   EL509
00071                                                                   EL509
00072  FILE SECTION.                                                    EL509
00073                                                                   EL509
00074  FD  PRNTR                   COPY ELCPRTFD.                       EL509
00075                                                                   EL509
00076  FD  FICH                    COPY ELCFCHFD.                       EL509
00077                                                                   EL509
00078  FD  DISK-DATE               COPY ELCDTEFD.                       EL509
00079                                                                   EL509
00080      EJECT                                                        EL509
00081  FD  COMP-MSTR                                                    EL509
00082      BLOCK CONTAINS 0 RECORDS
00083      RECORDING MODE F.                                            EL509
00084                                                                   EL509
00085  01  COMP-MSTR-REC         PIC X(700).                            EL509
00086                                                                   EL509
00087      EJECT                                                        EL509
00088  FD  ERCOMP.                                                      EL509
00089                                                                   EL509
00090                            COPY ERCCOMP.                          EL509
00091                                                                   EL509
00092      EJECT                                                        EL509
00093  FD  ELCNTL.                                                      EL509
00094                                                                   EL509
00095                            COPY ELCCNTL.                          EL509
00096                                                                   EL509
00097      EJECT                                                        EL509
00098  FD  ELREPT.                                                      EL509
00099                                                                   EL509
00100                            COPY ELCREPT.                          EL509
00101                                                                   EL509
00102      EJECT                                                        EL509
00103                                                                   EL509
00104  FD  ERMEBL.                                                      EL509
00105                                                                   EL509
00106                         COPY ERCMEBL.                             EL509
00107      EJECT                                                        EL509
00108                                                                   EL509
00109  WORKING-STORAGE SECTION.                                         EL509
00110  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL509
00111                                                                   EL509
00112  77  FILLER  PIC X(32) VALUE '********************************'.  EL509
00113  77  FILLER  PIC X(32) VALUE '      EL509 WORKING-STORAGE     '.  EL509
00114  77  FILLER  PIC X(32) VALUE '********** VMOD=2.018 **********'.  EL509
00115                                                                   EL509
00116  01  MONTH-END-DATA.                                              EL509
00117      12  ME-START-DATE.                                           EL509
00118          16  ME-START-MO         PIC 99.                          EL509
00119          16  FILLER              PIC X.                           EL509
00120          16  ME-START-DA         PIC 99.                          EL509
00121          16  FILLER              PIC X.                           EL509
00122          16  ME-START-YR         PIC 99.                          EL509
00123      12  ME-CNDS-DATE            PIC 9(6).                        EL509
00124      12  ME-CNDS-DATE-R REDEFINES ME-CNDS-DATE.                   EL509
00125          16  ME-CNDS-MO          PIC 99.                          EL509
00126          16  ME-CNDS-DA          PIC 99.                          EL509
00127          16  ME-CNDS-YR          PIC 99.                          EL509
00128      12  ME-START-TIME           PIC 9(6).                        EL509
00129      12  ME-UPDATE-FLAG          PIC X VALUE 'Y'.                 EL509
00130          88  ME-DO-UPDATE        VALUE 'Y'.                       EL509
00131          88  ME-NO-UPDATE        VALUE 'N'.                       EL509
00132      12  ERMEBL-FILE-STATUS      PIC XX.                          EL509
00133      12  MONTH-END-MOYR          PIC S9(5)  COMP-3.                  CL**2
00134                                                                   EL509
00135  01  WORK-AREAS.                                                  EL509
00136      12  WS-LINE-COUNT          PIC S9(3)  VALUE +99 COMP-3.      EL509
00137      12  WS-LINE-COUNT-MAX      PIC S9(3)  VALUE +60 COMP-3.      EL509
00138      12  WS-PAGE                PIC S9(3)  VALUE +0  COMP-3.      EL509
00139      12  WS-ZERO                PIC S9     VALUE +0  COMP-3.      EL509
00140      12  WS-RETURN-CODE         PIC S9(4)            COMP.        EL509
00141                                                                   EL509
00142      12  WS-CURRENT-BIN-DT      PIC XX.                           EL509
00143      12  WS-ABEND-MESSAGE       PIC X(80).                        EL509
00144      12  WS-ABEND-FILE-STATUS   PIC XX     VALUE ZEROS.           EL509
00145      12  COMPENSATION-FILE-STATUS  PIC XX  VALUE ZEROS.           EL509
00146      12  CMP REDEFINES COMPENSATION-FILE-STATUS.                  EL509
00147          16  CMP1               PIC X.                            EL509
00148          16  CMP2               PIC X.                            EL509
00149      12  CONTROL-FILE-STATUS    PIC XX  VALUE ZEROS.              EL509
00150                                                                   EL509
00151      12  EOF-SW                 PIC X    VALUE SPACE.             EL509
00152          88  END-OF-FILE                 VALUE 'E'.               EL509
00153                                                                   EL509
00154      12  ERROR-SW               PIC X    VALUE SPACE.             EL509
00155          88  ERROR-OCCURRED              VALUE 'E'.               EL509
00156          88  NO-ERRORS                   VALUE ' '.               EL509
00157                                                                   EL509
00158      12  WS-CLCNTL-STATUS-SW    PIC S9   VALUE +0.                EL509
00159          88  ELCNTL-NOT-OPEN             VALUE +0.                EL509
00160          88  ELCNTL-OPEN                 VALUE +1.                EL509
00161                                                                   EL509
00162      12  WS-DUMMY-VARIABLE      PIC X    VALUE ZEROS.             EL509
00163          88  VAR-ACCT                    VALUE '3'.               EL509
00164          88  VAR-ACCT-STAT               VALUE ' '.               EL509
00165          88  VAR-ACCT-CARR               VALUE '4'.               EL509
00166          88  VAR-ACCT-CARR-STAT          VALUE '2'.               EL509
00167          88  VAR-ALL                     VALUE '1'.               EL509
00168                                                                   EL509
00169  01  DTE-INTERFACE-CODES.                                         EL509
00170      12  X                 PIC X           VALUE SPACE.           EL509
00171      12  PGM-SUB           PIC S9(4)  COMP VALUE +509.            EL509
00172      12  ABEND-CODE        PIC 9999        VALUE ZERO.            EL509
00173      12  ABEND-OPTION      PIC X           VALUE SPACE.           EL509
00174      12  OLC-REPORT-NAME   PIC X(6)        VALUE 'EL509'.         EL509
00175      12  WS-PROCESSOR      PIC X(4)        VALUE SPACES.          EL509
00176                                                                   EL509
00177  01  WS-CONTROL-PRIMARY.                                          EL509
00178      05  WS-COMPANY-CD      PIC X.                                EL509
00179      05  WS-CARRIER         PIC X.                                EL509
00180      05  WS-COMPANY         PIC X(3).                             EL509
00181      05  WS-RESP            PIC X(6).                             EL509
00182      05  WS-ACCT            PIC X(6).                             EL509
00183      05  WS-TYPE            PIC X.                                EL509
00184                                                                   EL509
00185  01  COMP-3-WORK-AREA.                                            EL509
00186      05  K1                 PIC S9(7)  VALUE +1.                  EL509
00187      05  K2                 PIC S9(7)  VALUE +2.                  EL509
00188      05  RECORD-COUNT       PIC S9(7)  VALUE +0.                  EL509
00189      05  DELETE-COUNT       PIC S9(7)  VALUE +0.                  EL509
00190      05  DATE-ERROR-COUNT   PIC S9(7)  VALUE +0.                  EL509
00191      05  BALANCE-AMT        PIC S9(8)V99  VALUE +0  COMP-3.       EL509
00192                                                                   EL509
00193  01  WS-SAVE-PRINT-RECORD   PIC X(133) VALUE SPACES.              EL509
00194                                                                   EL509
00195  01  WS-USER-TYPE           PIC X.                                EL509
00196      88  WS-AR-USER         VALUE 'Y'.                            EL509
00197                                                                   EL509
00198      EJECT                                                        EL509
00199                             COPY ELCDATE.                            CL**4
00200                                                                   EL509
00201      EJECT                                                        EL509
00202  01  WS-HEADING1.                                                 EL509
00203      05  FILLER                      PIC X(51)       VALUE '1'.   EL509
00204      05  WS-H1-TITLE                 PIC X(73)       VALUE        EL509
00205          'COMPENSATION FILE LOAD'.                                EL509
00206      05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL509  '.    EL509
00207                                                                   EL509
00208  01  WS-HEADING2.                                                 EL509
00209      05  FILLER                      PIC X(46)       VALUE SPACES.EL509
00210      05  WS-H2-CLIENT-NAME           PIC X(78)       VALUE SPACES.EL509
00211      05  WS-H2-DATE                  PIC X(8)        VALUE SPACES.EL509
00212      05  FILLER                      PIC X           VALUE SPACES.EL509
00213                                                                   EL509
00214  01  WS-HEADING3.                                                 EL509
00215      05  FILLER                      PIC X(51)       VALUE SPACES.EL509
00216      05  WS-H3-DATE                  PIC X(60)       VALUE SPACES.EL509
00217      05  FILLER                      PIC X(5)        VALUE 'PAGE'.EL509
00218      05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL509
00219      05  FILLER                      PIC X(11)       VALUE SPACES.EL509
00220                                                                   EL509
00221  01  WS-HEADING4                     PIC X(132)      VALUE SPACES.EL509
00222                                                                   EL509
00223  01  WS-DETAIL1.                                                  EL509
00224      05  FILLER             PIC X     VALUE ' '.                  EL509
00225      05  DTL-MSG1           PIC X(18) VALUE 'SUCCESSFUL LOAD'.    EL509
00226      05  DTL-MSG2           PIC X(18) VALUE SPACE.                EL509
00227      05  DTL-MSG3           PIC X(18) VALUE SPACE.                EL509
00228      05  D-RECORD-COUNT     PIC Z,ZZZ,ZZ9.                        EL509
00229      05  FILLER             PIC X(10) VALUE ' RECORDS ('.         EL509
00230      05  D-DELETE-COUNT     PIC Z,ZZZ,ZZ9.                        EL509
00231      05  FILLER             PIC X(11) VALUE ' DELETED)'.          EL509
00232      05  FILLER             PIC X(15) VALUE '   DATE ERRORS'.     EL509
00233      05  D-DATE-ERROR-COUNT PIC Z,ZZZ,ZZ9.                        EL509
00234                                                                   EL509
00235  01  WS-DETAIL2.                                                  EL509
00236      05  FILLER             PIC X     VALUE ' '.                  EL509
00237      05  DTL-BAL-MSG        PIC X(26) VALUE                       EL509
00238                      'CURRENT BALANCE ON FILE'.                   EL509
00239      05  FILLER             PIC XX      VALUE SPACES.             EL509
00240      05  DTL-BAL-AMT        PIC ZZ,ZZZ,ZZZ.99-.                   EL509
00241      05  FILLER             PIC X(91)   VALUE SPACES.             EL509
00242                                                                   EL509
00243                             COPY ELCDTECX.                        EL509
00244                                                                   EL509
00245                             COPY ELCDTEVR.                        EL509
00246                                                                   EL509
00247      EJECT                                                        EL509
00248  PROCEDURE DIVISION.                                              EL509
00249                                                                   EL509
00250  CAPTURE-START.                                                   EL509
00251      OPEN I-O ERMEBL.                                             EL509
00252                                                                   EL509
00253      IF ERMEBL-FILE-STATUS  = '00' OR '97'                        EL509
00254          NEXT SENTENCE                                            EL509
00255        ELSE                                                       EL509
00256          MOVE 'N'                TO ME-UPDATE-FLAG.               EL509
00257                                                                   EL509
00258  0000-LOAD-DATE-WS. COPY ELCDTERX.                                EL509
00259      MOVE WS-TIME                TO ME-START-TIME.                EL509
00260      MOVE WS-CURRENT-DATE        TO ME-START-DATE.                EL509
00261      MOVE ME-START-MO            TO ME-CNDS-MO.                   EL509
00262      MOVE ME-START-DA            TO ME-CNDS-DA.                   EL509
00263      MOVE ME-START-YR            TO ME-CNDS-YR.                   EL509
00264                                                                   EL509
00265      MOVE DTE-CLIENT             TO ME-COMPANY.                   EL509
00266      COMPUTE MONTH-END-MOYR = (RUN-CCYY * 12) + RUN-MO.              CL**2
00267      MOVE MONTH-END-MOYR         TO ME-MOYR.                      EL509
00268      IF ME-DO-UPDATE                                              EL509
00269          READ ERMEBL INVALID KEY                                  EL509
00270          MOVE 'N'                TO ME-UPDATE-FLAG                EL509
00271          CLOSE ERMEBL.                                            EL509
00272                                                                   EL509
00273      EJECT                                                        EL509
00274  000-MAINLINE.                                                    EL509
00275      PERFORM 100-INITIALIZE  THRU 100-EXIT.                       EL509
00276                                                                   EL509
00277      PERFORM 150-READ-CNTL THRU 150-EXIT.                         EL509
00278                                                                   EL509
00279      IF NO-ERRORS                                                 EL509
00280          PERFORM 200-DELETE-OLD THRU 200-EXIT.                    EL509
00281                                                                   EL509
00282      IF NO-ERRORS                                                 EL509
00283          PERFORM 300-LOAD THRU 300-EXIT                           EL509
00284                  UNTIL END-OF-FILE                                EL509
00285                  OR    ERROR-OCCURRED.                            EL509
00286                                                                   EL509
00287      IF NO-ERRORS                                                 EL509
00288          PERFORM 170-UPDATE-CNTL THRU 170-EXIT.                   EL509
00289                                                                   EL509
00290      MOVE RECORD-COUNT     TO D-RECORD-COUNT.                     EL509
00291      MOVE DELETE-COUNT     TO D-DELETE-COUNT.                     EL509
00292      MOVE DATE-ERROR-COUNT TO D-DATE-ERROR-COUNT.                 EL509
00293      MOVE WS-DETAIL1       TO PRT.                                EL509
00294      PERFORM WRITE-A-LINE.                                        EL509
00295                                                                   EL509
00296      MOVE BALANCE-AMT      TO DTL-BAL-AMT.                        EL509
00297      MOVE WS-DETAIL2       TO PRT.                                EL509
00298      PERFORM WRITE-A-LINE.                                        EL509
00299                                                                   EL509
00300      PERFORM 800-FINALIZE THRU 800-EXIT.                          EL509
00301                                                                   EL509
00302      IF ME-DO-UPDATE                                              EL509
00303          MOVE 1                  TO ME-509-FLAG                   EL509
00304          MOVE ME-START-TIME      TO ME-509-START                  EL509
00305          MOVE ME-CNDS-DATE       TO ME-509-RUN-DT                 EL509
00306          ACCEPT WS-TIME-OF-DAY   FROM TIME                        EL509
00307          MOVE WS-TIME            TO ME-509-END                    EL509
00308          ADD 1                   TO ME-509-RUN-CT                 EL509
00309          REWRITE MONTH-END-BALANCES                               EL509
00310          CLOSE ERMEBL.                                            EL509
00311                                                                   EL509
00312  000-MAINLINE-EXIT.                                               EL509
00313      GOBACK.                                                      EL509
00314                                                                   EL509
00315  100-INITIALIZE.                                                  EL509
00316      MOVE ZEROS        TO WS-RETURN-CODE.                         EL509
00317      MOVE COMPANY-NAME           TO WS-H2-CLIENT-NAME.            EL509
00318      MOVE WS-CURRENT-DATE        TO WS-H2-DATE.                   EL509
00319      MOVE ALPH-DATE              TO WS-H3-DATE.                   EL509
00320                                                                   EL509
00321      OPEN INPUT  COMP-MSTR                                        EL509
00322           I-O    ERCOMP                                           EL509
00323           OUTPUT PRNTR                                            EL509
00324                                                                   EL509
00325      IF COMPENSATION-FILE-STATUS = '00' OR '97'                   EL509
00326          NEXT SENTENCE                                            EL509
00327        ELSE                                                       EL509
00328          MOVE COMPENSATION-FILE-STATUS  TO WS-ABEND-FILE-STATUS   EL509
00329          MOVE ' COMP OPEN ERROR- '      TO WS-ABEND-MESSAGE       EL509
00330          GO TO ABEND-PGM.                                         EL509
00331                                                                   EL509
00332      MOVE LOW-VALUES             TO WS-CONTROL-PRIMARY.           EL509
00333      MOVE DTE-CLASIC-COMPANY-CD  TO WS-COMPANY-CD.                EL509
00334      MOVE DTE-CLIENT             TO WS-PROCESSOR.                 EL509
00335                                                                   EL509
00336      MOVE WS-CURRENT-DATE        TO DC-GREG-DATE-1-EDIT.          EL509
00337      MOVE '2'                    TO DC-OPTION-CODE.               EL509
00338      PERFORM 310-DATE-RTN THRU 310-EXIT.                          EL509
00339      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.            EL509
00340                                                                   EL509
00341  100-EXIT.                                                        EL509
00342      EXIT.                                                        EL509
00343      EJECT                                                        EL509
00344  150-READ-CNTL.                                                   EL509
00345      OPEN INPUT ELCNTL.                                           EL509
00346                                                                   EL509
00347      IF CONTROL-FILE-STATUS = '00' OR '97'                        EL509
00348          NEXT SENTENCE                                            EL509
00349        ELSE                                                       EL509
00350          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL509
00351          MOVE ' CNTL OPEN ERROR- ' TO WS-ABEND-MESSAGE            EL509
00352          GO TO ABEND-PGM.                                         EL509
00353                                                                   EL509
00354      MOVE +1                     TO WS-CLCNTL-STATUS-SW.          EL509
00355                                                                   EL509
00356      MOVE SPACES                 TO CF-ACCESS-CD-GENL.            EL509
00357      MOVE DTE-CLIENT             TO CF-COMPANY-ID                 EL509
00358      MOVE '1'                    TO CF-RECORD-TYPE                EL509
00359      MOVE ZEROS                  TO CF-SEQUENCE-NO.               EL509
00360                                                                   EL509
00361      READ ELCNTL.                                                 EL509
00362                                                                   EL509
00363      IF CONTROL-FILE-STATUS NOT = ZERO                            EL509
00364          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL509
00365          MOVE ' CNTL READ ERROR- ' TO WS-ABEND-MESSAGE            EL509
00366          GO TO ABEND-PGM.                                         EL509
00367                                                                   EL509
00368      IF EP-SW = '1'  OR  '2' OR '3'                               EL509
00369 *        IF CF-COMPENSATION-MSTR-CREATE-DT LESS THAN              EL509
00370 *           CF-COMPENSATION-MSTR-MAINT-DT                         EL509
00371 *            MOVE 'E' TO ERROR-SW                                 EL509
00372 *            MOVE '- ABORT LOAD     -' TO DTL-MSG1                EL509
00373 *            MOVE '- MAINT APPLIED  -' TO DTL-MSG3                EL509
00374 *            DISPLAY '*EL509*  LOAD ABORTED FOR - ' DTE-CLIENT    EL509
00375 *                UPON CONSOLE                                     EL509
00376 *            DISPLAY '*EL509*  MAINT APPLIED ' UPON CONSOLE       EL509
00377 *            MOVE SPACES               TO WS-ABEND-FILE-STATUS    EL509
00378 *            MOVE '*EL509* MAINT APPLIED' TO WS-ABEND-MESSAGE        CL**3
00379 *            GO TO ABEND-PGM                                      EL509
00380 *        ELSE                                                     EL509
00381              NEXT SENTENCE                                        EL509
00382      ELSE                                                         EL509
00383          MOVE 'E' TO ERROR-SW                                     EL509
00384          MOVE '- ABORT LOAD     -' TO DTL-MSG1                    EL509
00385          MOVE '- UPDATE NOT ON  -' TO DTL-MSG3                    EL509
00386          DISPLAY '*EL509*  LOAD ABORTED FOR - ' DTE-CLIENT        EL509
00387              UPON CONSOLE                                         EL509
00388          DISPLAY '*EL509*  NEED UPDATE SWITCH ON ' UPON CONSOLE   EL509
00389          MOVE SPACES               TO WS-ABEND-FILE-STATUS        EL509
00390          MOVE '*EL509* UPDATE NOT ON' TO WS-ABEND-MESSAGE         EL509
00391          GO TO ABEND-PGM.                                         EL509
00392                                                                   EL509
00393      MOVE CF-SYSTEM-E            TO  WS-USER-TYPE.                EL509
00394                                                                   EL509
00395      CLOSE ELCNTL.                                                EL509
00396                                                                   EL509
00397      IF CONTROL-FILE-STATUS NOT = ZERO                            EL509
00398          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL509
00399          MOVE ' CNTL CLOSE ERROR- ' TO WS-ABEND-MESSAGE           EL509
00400          GO TO ABEND-PGM.                                         EL509
00401                                                                   EL509
00402      MOVE +0                     TO WS-CLCNTL-STATUS-SW.          EL509
00403                                                                   EL509
00404  150-EXIT.                                                        EL509
00405      EXIT.                                                        EL509
00406                                                                   EL509
00407  170-UPDATE-CNTL.                                                 EL509
00408      OPEN I-O ELCNTL.                                             EL509
00409                                                                   EL509
00410      IF CONTROL-FILE-STATUS = '00' OR '97'                        EL509
00411          NEXT SENTENCE                                            EL509
00412        ELSE                                                       EL509
00413          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL509
00414          MOVE ' CNTL OPEN ERROR- ' TO WS-ABEND-MESSAGE            EL509
00415          GO TO ABEND-PGM.                                         EL509
00416                                                                   EL509
00417      MOVE +1                     TO WS-CLCNTL-STATUS-SW.          EL509
00418                                                                   EL509
00419      MOVE SPACES                 TO CF-ACCESS-CD-GENL.            EL509
00420      MOVE DTE-CLIENT             TO CF-COMPANY-ID                 EL509
00421      MOVE '1'                    TO CF-RECORD-TYPE                EL509
00422      MOVE ZEROS                  TO CF-SEQUENCE-NO.               EL509
00423                                                                   EL509
00424      READ ELCNTL                                                  EL509
00425                                                                   EL509
00426      IF CONTROL-FILE-STATUS NOT = ZERO                            EL509
00427          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL509
00428          MOVE ' CNTL READ ERROR- ' TO WS-ABEND-MESSAGE            EL509
00429          GO TO ABEND-PGM.                                         EL509
00430                                                                   EL509
00431      MOVE WS-CURRENT-BIN-DT TO CF-COMPENSATION-MSTR-MAINT-DT      EL509
00432                                CF-COMPENSATION-MSTR-CREATE-DT.    EL509
00433                                                                   EL509
00434      IF EP-SW = '2' OR '3'                                        EL509
00435          MOVE WS-CURRENT-BIN-DT TO  CF-ACCOUNT-MSTR-MAINT-DT      EL509
00436                                     CF-ACCOUNT-MSTR-CREATE-DT.    EL509
00437                                                                   EL509
00438      REWRITE CONTROL-FILE.                                        EL509
00439                                                                   EL509
00440      IF CONTROL-FILE-STATUS NOT = ZERO                            EL509
00441          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL509
00442          MOVE ' CNTL REWRITE ERROR- ' TO WS-ABEND-MESSAGE         EL509
00443          GO TO ABEND-PGM.                                         EL509
00444                                                                   EL509
00445      CLOSE ELCNTL.                                                EL509
00446                                                                   EL509
00447      IF CONTROL-FILE-STATUS NOT = ZERO                            EL509
00448          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL509
00449          MOVE ' CNTL CLOSE ERROR- ' TO WS-ABEND-MESSAGE           EL509
00450          GO TO ABEND-PGM.                                         EL509
00451                                                                   EL509
00452      MOVE +0                     TO WS-CLCNTL-STATUS-SW.          EL509
00453                                                                   EL509
00454  170-EXIT.  EXIT.                                                 EL509
00455                                                                   EL509
00456  200-DELETE-OLD.                                                  EL509
00457      MOVE WS-CONTROL-PRIMARY  TO CO-CONTROL-PRIMARY.              EL509
00458                                                                   EL509
00459  205-START.                                                       EL509
00460      START ERCOMP KEY NOT LESS CO-CONTROL-PRIMARY.                EL509
00461                                                                   EL509
00462      IF COMPENSATION-FILE-STATUS  = '10' OR '23'                  EL509
00463           GO TO 200-EXIT.                                         EL509
00464                                                                   EL509
00465      IF COMPENSATION-FILE-STATUS NOT = ZERO                       EL509
00466          MOVE COMPENSATION-FILE-STATUS  TO WS-ABEND-FILE-STATUS   EL509
00467          MOVE ' COMP START ERROR- '     TO WS-ABEND-MESSAGE       EL509
00468          GO TO ABEND-PGM.                                         EL509
00469                                                                   EL509
00470  210-READNEXT.                                                    EL509
00471      READ ERCOMP  NEXT RECORD.                                    EL509
00472                                                                   EL509
00473      IF CMP1 = '1'                                                EL509
00474              GO TO 200-EXIT.                                      EL509
00475                                                                   EL509
00476      IF COMPENSATION-FILE-STATUS NOT = ZEROS                      EL509
00477          MOVE ' -ERROR ON READ- '      TO WS-ABEND-MESSAGE        EL509
00478          MOVE COMPENSATION-FILE-STATUS TO WS-ABEND-FILE-STATUS    EL509
00479          GO TO ABEND-PGM.                                         EL509
00480                                                                   EL509
00481      IF DTE-CLASIC-COMPANY-CD NOT = CO-COMPANY-CD                 EL509
00482          GO TO 200-EXIT.                                          EL509
00483                                                                   EL509
00484      DELETE  ERCOMP  RECORD.                                      EL509
00485                                                                   EL509
00486      PERFORM 230-ERROR-CHECK  THRU 230-EXIT.                      EL509
00487                                                                   EL509
00488      IF ERROR-OCCURRED                                            EL509
00489          MOVE ' -ERR ON DELETE- ' TO DTL-MSG3                     EL509
00490          GO TO 200-EXIT.                                          EL509
00491                                                                   EL509
00492      ADD 1  TO DELETE-COUNT.                                      EL509
00493                                                                   EL509
00494      GO TO 210-READNEXT.                                          EL509
00495                                                                   EL509
00496  200-EXIT.                                                        EL509
00497      EXIT.                                                        EL509
00498                                                                   EL509
00499  230-ERROR-CHECK.                                                 EL509
00500      IF CMP1 NOT = '0'                                            EL509
00501          MOVE   CMP  TO DTL-MSG2                                  EL509
00502          MOVE   'E'  TO ERROR-SW.                                 EL509
00503                                                                   EL509
00504  230-EXIT.                                                        EL509
00505      EXIT.                                                        EL509
00506                                                                   EL509
00507      EJECT                                                        EL509
00508  300-LOAD.                                                        EL509
00509      READ COMP-MSTR                                               EL509
00510             AT END MOVE 'E' TO EOF-SW                             EL509
00511             GO TO 300-EXIT.                                       EL509
00512                                                                   EL509
00513      MOVE COMP-MSTR-REC          TO COMPENSATION-MASTER.          EL509
00514      MOVE DTE-CLASIC-COMPANY-CD  TO CO-COMPANY-CD.                EL509
00515                                                                   EL509
00516      IF CO-CUR-OVR-UNDR NOT NUMERIC                               EL509
00517          MOVE ZEROS              TO CO-CUR-OVR-UNDR.              EL509
00518                                                                   EL509
00519      IF CO-YTD-OVR-UNDR NOT NUMERIC                               EL509
00520          MOVE ZEROS              TO CO-YTD-OVR-UNDR.              EL509
00521                                                                   EL509
00522      IF CO-LAST-ACTIVITY-DATE NOT NUMERIC                         EL509
00523          MOVE ZEROS              TO CO-LAST-ACTIVITY-DATE.        EL509
00524                                                                   EL509
00525      IF CO-LAST-STMT-DT  NOT NUMERIC                              EL509
00526          MOVE ZEROS              TO CO-LAST-STMT-DT.              EL509
00527                                                                   EL509
00528      IF CO-CURRENT-LAST-STMT-DT NOT NUMERIC                       EL509
00529          MOVE ZEROS              TO CO-CURRENT-LAST-STMT-DT.      EL509
00530                                                                   EL509
           MOVE +0                     TO CO-MD-AMT
           MOVE ZEROS                  TO CO-MD-DIV
                                          CO-MD-CENTER

00531      IF WS-AR-USER                                                EL509
00532          IF EP-SW = '1'                                           EL509
00533              MOVE CO-CURRENT-MONTHLY-TOTALS                       EL509
00534                                  TO CO-MONTHLY-TOTALS             EL509
00535              MOVE CO-CURRENT-AGING-TOTALS                         EL509
00536                                  TO CO-AGING-TOTALS               EL509
00537              MOVE CO-CURRENT-YTD-TOTALS                           EL509
00538                                  TO CO-YTD-TOTALS                 EL509
00539              MOVE CO-CUR-OVR-UNDR                                 EL509
00540                                  TO CO-YTD-OVR-UNDR               EL509
00541              MOVE CO-CURRENT-LAST-STMT-DT                         EL509
00542                                  TO CO-LAST-STMT-DT               EL509
00543              IF RUN-MO = 12                                       EL509
00544                  MOVE 'A'        TO CO-AR-LAST-RUN-CODE           EL509
00545              ELSE                                                 EL509
00546                  MOVE 'M'        TO CO-AR-LAST-RUN-CODE           EL509
00547          ELSE                                                     EL509
00548              MOVE 'C'            TO CO-AR-LAST-RUN-CODE           EL509
00549      ELSE                                                         EL509
00550          MOVE CO-MONTHLY-TOTALS  TO CO-CURRENT-MONTHLY-TOTALS     EL509
00551          MOVE CO-AGING-TOTALS    TO CO-CURRENT-AGING-TOTALS       EL509
00552          MOVE CO-YTD-TOTALS      TO CO-CURRENT-YTD-TOTALS         EL509
00553          MOVE CO-LAST-STMT-DT    TO CO-CURRENT-LAST-STMT-DT.      EL509
00554                                                                   EL509
00555      WRITE COMPENSATION-MASTER.                                   EL509
00556                                                                   EL509
00557      PERFORM 230-ERROR-CHECK  THRU 230-EXIT.                      EL509
00558                                                                   EL509
00559      IF ERROR-OCCURRED                                            EL509
00560          MOVE ' -ERR ON WRITE- ' TO DTL-MSG3                      EL509
00561          GO TO 300-EXIT.                                          EL509
00562                                                                   EL509
00563      ADD  1 TO RECORD-COUNT.                                      EL509
00564      ADD  CO-CURRENT-END-BAL TO BALANCE-AMT.                      EL509
00565                                                                   EL509
00566  300-EXIT.                                                        EL509
00567      EXIT.                                                        EL509
00568                                                                   EL509
00569  310-DATE-RTN.                                                    EL509
00570      CALL 'ELDATCX'  USING DATE-CONVERSION-DATA.                  EL509
00571                                                                   EL509
00572      IF DC-ERROR-CODE NOT = SPACE                                 EL509
00573          MOVE  ZEROS TO DC-BIN-DATE-1                             EL509
00574          ADD 1  TO DATE-ERROR-COUNT.                              EL509
00575                                                                   EL509
00576  310-EXIT.                                                        EL509
00577      EXIT.                                                        EL509
00578                                                                   EL509
00579      EJECT                                                        EL509
00580                                                                   EL509
00581  800-FINALIZE.                                                    EL509
00582      CLOSE COMP-MSTR                                              EL509
00583            PRNTR                                                  EL509
00584            ERCOMP.                                                EL509
00585                                                                   EL509
00586      IF COMPENSATION-FILE-STATUS NOT = ZERO                       EL509
00587          MOVE COMPENSATION-FILE-STATUS  TO WS-ABEND-FILE-STATUS   EL509
00588          MOVE ' COMP CLOSE ERROR- ' TO WS-ABEND-MESSAGE           EL509
00589          GO TO ABEND-PGM.                                         EL509
00590                                                                   EL509
00591      IF ELCNTL-OPEN                                               EL509
00592          CLOSE ELCNTL                                             EL509
00593          IF CONTROL-FILE-STATUS NOT = ZERO                        EL509
00594              MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS    EL509
00595              MOVE ' CNTL CLOSE ERROR- ' TO WS-ABEND-MESSAGE       EL509
00596              GO TO ABEND-PGM.                                     EL509
00597                                                                   EL509
00598  800-CLOSE-OTHER. COPY ELCPRTCX.                                  EL509
00599                                                                   EL509
00600  800-EXIT.                                                        EL509
00601      EXIT.                                                        EL509
00602      EJECT                                                        EL509
00603  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL509
00604                                                                   EL509
00605  WRITE-HEADINGS SECTION.                                          EL509
00606 ***************************************************************** EL509
00607 *                            ELCWHS1.                           * EL509
00608 *                            VMOD=2.001                         * EL509
00609 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          * EL509
00610 *****************************************************************.EL509
00611  WHS-010.                                                         EL509
00612      ADD +1  TO  WS-PAGE.                                         EL509
00613      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL509
00614      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL509
00615      MOVE ZERO                   TO  WS-LINE-COUNT.               EL509
00616                                                                   EL509
00617      MOVE WS-HEADING1            TO  PRT.                         EL509
00618      MOVE '1'                    TO  X.                           EL509
00619      PERFORM WRITE-PRINTER.                                       EL509
00620                                                                   EL509
00621      MOVE WS-HEADING2            TO  PRT.                         EL509
00622      MOVE ' '                    TO  X.                           EL509
00623      PERFORM WRITE-PRINTER.                                       EL509
00624                                                                   EL509
00625      MOVE WS-HEADING3            TO  PRT.                         EL509
00626      MOVE ' '                    TO  X.                           EL509
00627      PERFORM WRITE-PRINTER.                                       EL509
00628                                                                   EL509
00629      MOVE WS-HEADING4            TO  PRT.                         EL509
00630      MOVE ' '                    TO  X.                           EL509
00631      PERFORM WRITE-PRINTER.                                       EL509
00632                                                                   EL509
00633      MOVE +4 TO WS-LINE-COUNT.                                    EL509
00634                                                                   EL509
00635  WHS-020. COPY ELCWHS2.                                           EL509
00636                                                                   EL509
00637  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL509
00638  WPS-020. COPY ELCPRT2X.                                          EL509
00639                                                                   EL509
00640  ABEND-PGM SECTION.                                               EL509
00641                      COPY ELCABEND.                               EL509
