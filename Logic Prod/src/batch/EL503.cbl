00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL503
00003  PROGRAM-ID.                 EL503 .                                 LV004
00004 *              PROGRAM CONVERTED BY                               EL503
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL503
00006 *              CONVERSION DATE 02/19/96 16:25:50.                 EL503
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL503
00008 *                            VMOD=2.003                           EL503
00009                                                                   EL503
00009                                                                   EL503
00010 *AUTHOR.     LOGIC INC.                                           EL503
00011 *            DALLAS, TEXAS.                                       EL503
00012                                                                   EL503
00013 *DATE-COMPILED.                                                   EL503
00014                                                                   EL503
00015 *SECURITY.   *****************************************************EL503
00016 *            *                                                   *EL503
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL503
00018 *            *                                                   *EL503
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL503
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL503
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL503
00022 *            *                                                   *EL503
00023 *            *****************************************************EL503
00024                                                                   EL503
00025 *REMARKS.                                                         EL503
00026 *         THIS PROGRAM IS USED TO CREATE A RATE MASTER FILE       EL503
00027 *       DATA SET FOR CLAIM SYSTEM FROM (ERCRATE). A CONTROL       EL503
00028 *       PAGE WILL BE PRINTED AT EOJ.                              EL503
00029                                                                   EL503
00030  ENVIRONMENT DIVISION.                                            EL503
00031                                                                   EL503
00032  INPUT-OUTPUT SECTION.                                            EL503
00033                                                                   EL503
00034  FILE-CONTROL.                                                    EL503
00035                                                                   EL503
00036      SELECT PRNTR          ASSIGN TO SYS008-UR-1403-S-SYS008.     EL503
00037                                                                   EL503
00038      SELECT FICH           ASSIGN TO SYS020-UT-2400-S-SYS020.     EL503
00039                                                                   EL503
00040      SELECT DISK-DATE      ASSIGN TO SYS019-FBA1-S-SYS019.        EL503
00041                                                                   EL503
00042      SELECT RATE-MSTR      ASSIGN TO SYS010                       EL503
00043              ORGANIZATION IS INDEXED                              EL503
00044              ACCESS IS SEQUENTIAL                                 EL503
00045              RECORD KEY IS VSAMRT-KEY                             EL503
00046              FILE STATUS IS VSAMRATE-FILE-STATUS.                 EL503
00047                                                                   EL503
00048      SELECT ERRATE         ASSIGN TO SYS011-FBA1-ERRATE           EL503
00049              ORGANIZATION IS INDEXED                              EL503
00050              ACCESS IS DYNAMIC                                    EL503
00051              RECORD KEY IS RT-CONTROL-PRIMARY                     EL503
00052              FILE STATUS IS RATE-FILE-STATUS.                     EL503
00053                                                                   EL503
00054      SELECT ELCNTL         ASSIGN TO SYS012-FBA1-ELCNTL           EL503
00055              ORGANIZATION IS INDEXED                              EL503
00056              ACCESS IS DYNAMIC                                    EL503
00057              RECORD KEY IS CF-CONTROL-PRIMARY                     EL503
00058              FILE STATUS IS CONTROL-FILE-STATUS.                  EL503
00059                                                                   EL503
00060      SELECT ELREPT         ASSIGN TO SYS018-FBA1-ELREPT           EL503
00061              ORGANIZATION IS INDEXED                              EL503
00062              ACCESS IS DYNAMIC                                    EL503
00063              RECORD KEY IS RF-CONTROL-PRIMARY                     EL503
00064              FILE STATUS IS DTE-VSAM-FLAGS.                       EL503
00065                                                                   EL503
00066      EJECT                                                        EL503
00067  DATA DIVISION.                                                   EL503
00068                                                                   EL503
00069  FILE SECTION.                                                    EL503
00070                                                                   EL503
00071  FD  PRNTR                   COPY ELCPRTFD.                          CL**2
00072                                                                   EL503
00073  FD  FICH                    COPY ELCFCHFD.                          CL**2
00074                                                                   EL503
00075  FD  DISK-DATE               COPY ELCDTEFD.                          CL**2
00076                                                                   EL503
00077      EJECT                                                        EL503
00078  FD  RATE-MSTR.                                                   EL503
00079  01  RATE-MSTR-REC.                                               EL503
00080      05  FILLER     PIC XX.                                       EL503
00081      05  VSAMRT-KEY PIC X(28).                                    EL503
00082      05  FILLER     PIC X(1735).                                  EL503
00083                                                                   EL503
00084      EJECT                                                        EL503
00085  FD  ERRATE.                                                      EL503
00086                                                                   EL503
00087      COPY ERCRATE.                                                EL503
00088                                                                   EL503
00089      EJECT                                                        EL503
00090  FD  ELCNTL.                                                      EL503
00091      COPY ELCCNTL.                                                EL503
00092                                                                   EL503
00093      EJECT                                                        EL503
00094  FD  ELREPT.                                                      EL503
00095      COPY ELCREPT.                                                EL503
00096                                                                   EL503
00097      EJECT                                                        EL503
00098  WORKING-STORAGE SECTION.                                         EL503
00099  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL503
00100                                                                   EL503
00101  77  FILLER  PIC X(32) VALUE '********************************'.  EL503
00102  77  FILLER  PIC X(32) VALUE '      EL503 WORKING-STORAGE     '.  EL503
00103  77  FILLER  PIC X(32) VALUE '******** VMOD=2.003 ************'.  EL503
00104                                                                   EL503
00105  01  WORK-AREAS.                                                  EL503
00106      05  WS-LINE-COUNT          PIC S9(3)  VALUE +99 COMP-3.      EL503
00107      05  WS-LINE-COUNT-MAX      PIC S9(3)  VALUE +60 COMP-3.      EL503
00108      05  WS-PAGE                PIC S9(3)  VALUE +0  COMP-3.      EL503
00109      05  WS-ZERO                PIC S9     VALUE +0  COMP-3.      EL503
00110      05  WS-RETURN-CODE         PIC S9(4)  VALUE +0  COMP.        EL503
00111                                                                   EL503
00112      05  WS-DATE                PIC X(7).                         EL503
00113      05  WS-CURRENT-BIN-DT      PIC XX.                           EL503
00114      05  WS-ABEND-MESSAGE       PIC X(80)            VALUE SPACES.EL503
00115      05  WS-ABEND-FILE-STATUS   PIC XX  VALUE ZEROS.              EL503
00116      05  RATE-FILE-STATUS       PIC XX  VALUE ZEROS.              EL503
00117      05  RTE REDEFINES RATE-FILE-STATUS.                          EL503
00118          10  RTE1               PIC X.                            EL503
00119          10  RTE2               PIC X.                            EL503
00120      05  CONTROL-FILE-STATUS    PIC XX  VALUE ZEROS.              EL503
00121      05  VSAMRATE-FILE-STATUS   PIC XX  VALUE ZEROS.              EL503
00122                                                                   EL503
00123      05  EOF-SW                 PIC X    VALUE SPACE.             EL503
00124          88  END-OF-FILE                 VALUE 'E'.               EL503
00125                                                                   EL503
00126      05  VSAM-END-SW            PIC X    VALUE SPACE.             EL503
00127          88  END-OF-VSAM-FILE                 VALUE 'E'.          EL503
00128                                                                   EL503
00129      05  WS-CLCNTL-STATUS-SW    PIC S9   VALUE +0.                EL503
00130          88  ELCNTL-NOT-OPEN             VALUE +0.                EL503
00131          88  ELCNTL-OPEN                 VALUE +1.                EL503
00132                                                                   EL503
00133      05  ERROR-SW               PIC X    VALUE SPACE.             EL503
00134          88  ERROR-OCCURRED              VALUE 'E'.               EL503
00135          88  NO-ERRORS                   VALUE ' '.               EL503
00136                                                                   EL503
00137      05  WS-DUMMY-VARIABLE      PIC X    VALUE ZEROS.             EL503
00138          88  VAR-ACCT                    VALUE '3'.               EL503
00139          88  VAR-ACCT-STAT               VALUE ' '.               EL503
00140          88  VAR-ACCT-CARR               VALUE '4'.               EL503
00141          88  VAR-ACCT-CARR-STAT          VALUE '2'.               EL503
00142          88  VAR-ALL                     VALUE '1'.               EL503
00143                                                                   EL503
00144  01  DTE-INTERFACE-CODES.                                         EL503
00145      05  X                 PIC X           VALUE SPACE.           EL503
00146      05  PGM-SUB           PIC S9(4)  COMP VALUE +503.            EL503
00147      05  ABEND-CODE        PIC 9999        VALUE ZERO.            EL503
00148      05  ABEND-OPTION      PIC X           VALUE SPACE.           EL503
00149      05  OLC-REPORT-NAME   PIC X(6)        VALUE 'EL503'.         EL503
00150      05  WS-PROCESSOR      PIC X(4)        VALUE SPACES.          EL503
00151                                                                   EL503
00152  01  WS-CONTROL-PRIMARY.                                          EL503
00153      05  WS-COMPANY-CD      PIC X.                                EL503
00154      05  WS-STATE-CODE.                                           EL503
00155          10  WS-ST-CODE     PIC XX.                               EL503
00156          10  WS-ST-CLASS    PIC XX.                               EL503
00157          10  WS-ST-DEV      PIC XXX.                              EL503
00158      05  WS-LIMITS.                                               EL503
00159          10  WS-HIGH-AGE    PIC 99.                               EL503
00160          10  WS-HIGH-AMT    PIC 9(6).                             EL503
00161          10  WS-FUTURE      PIC XX.                               EL503
00162          10  WS-SEX         PIC X.                                EL503
00163      05  WS-L-AH-CODE.                                            EL503
00164          10  WS-L-AH        PIC X.                                EL503
00165          10  WS-LAH-NUM     PIC 99.                               EL503
00166      05  WS-EXPIRY-DATE.                                          EL503
00167          10  WS-EXP-YR      PIC 99.                               EL503
00168          10  WS-EXP-MO      PIC 99.                               EL503
00169          10  WS-EXP-DA      PIC 99.                               EL503
00170      05  WS-FILLER          PIC X(20).                            EL503
00171                                                                   EL503
00172  01  COMP-3-WORK-AREA.                                            EL503
00173      05  K1                 PIC S9(7)  VALUE +1.                  EL503
00174      05  K2                 PIC S9(7)  VALUE +2.                  EL503
00175      05  RECORD-COUNT       PIC S9(7)  VALUE +0.                  EL503
00176      05  DELETE-COUNT       PIC S9(7)  VALUE +0.                  EL503
00177      05  DATE-ERROR-COUNT   PIC S9(7)  VALUE +0.                  EL503
00178                                                                   EL503
00179  01  WS-SAVE-PRINT-RECORD   PIC X(133) VALUE SPACES.              EL503
00180                                                                   EL503
00181      EJECT                                                        EL503
00182      COPY ELCDATE.                                                   CL**4
00183                                                                   EL503
00184      EJECT                                                        EL503
00185  01  WS-HEADING1.                                                 EL503
00186      05  FILLER                      PIC X(51)       VALUE '1'.   EL503
00187      05  WS-H1-TITLE                 PIC X(73)       VALUE        EL503
00188          'RATE FILE LOAD'.                                        EL503
00189      05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL503  '.    EL503
00190                                                                   EL503
00191  01  WS-HEADING2.                                                 EL503
00192      05  FILLER                      PIC X(46)       VALUE SPACES.EL503
00193      05  WS-H2-CLIENT-NAME           PIC X(78)       VALUE SPACES.EL503
00194      05  WS-H2-DATE                  PIC X(8)        VALUE SPACES.EL503
00195      05  FILLER                      PIC X           VALUE SPACES.EL503
00196                                                                   EL503
00197  01  WS-HEADING3.                                                 EL503
00198      05  FILLER                      PIC X(51)       VALUE SPACES.EL503
00199      05  WS-H3-DATE                  PIC X(60)       VALUE SPACES.EL503
00200      05  FILLER                      PIC X(5)        VALUE 'PAGE'.EL503
00201      05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL503
00202      05  FILLER                      PIC X(11)       VALUE SPACES.EL503
00203                                                                   EL503
00204  01  WS-HEADING4                     PIC X(132)      VALUE SPACES.EL503
00205                                                                   EL503
00206  01  WS-DETAIL1.                                                  EL503
00207      05  FILLER             PIC X.                                EL503
00208      05  DTL-MSG1           PIC X(18) VALUE 'SUCCESSFUL LOAD   '. EL503
00209      05  DTL-MSG2           PIC X(18) VALUE SPACE.                EL503
00210      05  DTL-MSG3           PIC X(18) VALUE SPACE.                EL503
00211      05  D-RECORD-COUNT     PIC Z,ZZZ,ZZ9.                        EL503
00212      05  FILLER             PIC X(10) VALUE ' RECORDS ('.         EL503
00213      05  D-DELETE-COUNT     PIC Z,ZZZ,ZZ9.                        EL503
00214      05  FILLER             PIC X(11) VALUE ' DELETED)'.          EL503
00215      05  FILLER             PIC X(15) VALUE '   DATE ERRORS '.    EL503
00216      05  D-DATE-ERROR-COUNT PIC Z,ZZZ,ZZ9.                        EL503
00217                                                                   EL503
00218      COPY ELCDTECX.                                               EL503
00219                                                                   EL503
00220      COPY ELCDTEVR.                                                  CL**3
00221                                                                      CL**3
00222      EJECT                                                        EL503
00223  PROCEDURE DIVISION.                                              EL503
00224                                                                   EL503
00225  0000-LOAD-DATE-WS. COPY ELCDTERX.                                   CL**2
00226                                                                   EL503
00227                                                                   EL503
00228  000-MAINLINE.                                                    EL503
00229      PERFORM 100-INITIALIZE  THRU 100-EXIT.                       EL503
00230                                                                   EL503
00231      PERFORM 150-READ-CNTL THRU 150-EXIT.                         EL503
00232                                                                   EL503
00233      IF NO-ERRORS                                                 EL503
00234          PERFORM 200-DELETE-OLD THRU 200-EXIT.                    EL503
00235                                                                   EL503
00236      IF NO-ERRORS                                                 EL503
00237          PERFORM 300-LOAD THRU 300-EXIT                           EL503
00238                  UNTIL END-OF-VSAM-FILE.                          EL503
00239                                                                   EL503
00240      IF NO-ERRORS                                                 EL503
00241          PERFORM 170-UPDATE-CNTL THRU 170-EXIT.                   EL503
00242                                                                   EL503
00243      MOVE RECORD-COUNT     TO D-RECORD-COUNT.                     EL503
00244      MOVE DELETE-COUNT     TO D-DELETE-COUNT.                     EL503
00245      MOVE DATE-ERROR-COUNT TO D-DATE-ERROR-COUNT.                 EL503
00246      MOVE WS-DETAIL1       TO PRT.                                EL503
00247      PERFORM WRITE-A-LINE.                                        EL503
00248                                                                   EL503
00249      PERFORM 800-FINALIZE THRU 800-EXIT.                          EL503
00250                                                                   EL503
00251  000-MAINLINE-EXIT.                                               EL503
00252      GOBACK.                                                      EL503
00253                                                                   EL503
00254  100-INITIALIZE.                                                  EL503
00255      MOVE ZEROS        TO WS-RETURN-CODE.                         EL503
00256      MOVE COMPANY-NAME           TO WS-H2-CLIENT-NAME.            EL503
00257      MOVE WS-CURRENT-DATE        TO WS-H2-DATE.                   EL503
00258      MOVE ALPH-DATE              TO WS-H3-DATE.                   EL503
00259                                                                   EL503
00260      OPEN INPUT  RATE-MSTR                                        EL503
00261           I-O    ERRATE                                           EL503
00262           OUTPUT PRNTR                                            EL503
00263                                                                   EL503
00264      IF RATE-FILE-STATUS  = '00' OR '97'                          EL503
00265          NEXT SENTENCE                                            EL503
00266        ELSE                                                       EL503
00267          MOVE RATE-FILE-STATUS  TO WS-ABEND-FILE-STATUS           EL503
00268          MOVE ' RATE OPEN ERROR- '      TO WS-ABEND-MESSAGE       EL503
00269          GO TO ABEND-PGM.                                         EL503
00270                                                                   EL503
00271      IF VSAMRATE-FILE-STATUS  = '00' OR '97'                      EL503
00272          NEXT SENTENCE                                            EL503
00273        ELSE                                                       EL503
00274          MOVE VSAMRATE-FILE-STATUS  TO WS-ABEND-FILE-STATUS       EL503
00275          MOVE 'VSAM RATE OPEN ERROR- '      TO WS-ABEND-MESSAGE   EL503
00276          GO TO ABEND-PGM.                                         EL503
00277                                                                   EL503
00278      MOVE LOW-VALUES            TO WS-CONTROL-PRIMARY.            EL503
00279      MOVE DTE-CLASIC-COMPANY-CD TO WS-COMPANY-CD.                 EL503
00280      MOVE DTE-CLIENT            TO WS-PROCESSOR.                  EL503
00281                                                                   EL503
00282      MOVE WS-CURRENT-DATE TO DC-GREG-DATE-1-EDIT.                 EL503
00283      MOVE '2'             TO DC-OPTION-CODE.                      EL503
00284      PERFORM 310-DATE-RTN THRU 310-EXIT.                          EL503
00285      MOVE DC-BIN-DATE-1   TO WS-CURRENT-BIN-DT.                   EL503
00286                                                                   EL503
00287  100-EXIT.                                                        EL503
00288      EXIT.                                                        EL503
00289      EJECT                                                        EL503
00290  150-READ-CNTL.                                                   EL503
00291      OPEN INPUT ELCNTL.                                           EL503
00292                                                                   EL503
00293      IF CONTROL-FILE-STATUS = '00' OR '97'                        EL503
00294          NEXT SENTENCE                                            EL503
00295        ELSE                                                       EL503
00296          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL503
00297          MOVE ' CNTL OPEN ERROR- ' TO WS-ABEND-MESSAGE            EL503
00298          GO TO ABEND-PGM.                                         EL503
00299                                                                   EL503
00300      MOVE +1                     TO WS-CLCNTL-STATUS-SW.          EL503
00301      MOVE SPACES                 TO CF-ACCESS-CD-GENL.            EL503
00302      MOVE DTE-CLIENT             TO CF-COMPANY-ID.                EL503
00303      MOVE '1'                    TO CF-RECORD-TYPE.               EL503
00304      MOVE ZEROS                  TO CF-SEQUENCE-NO.               EL503
00305                                                                   EL503
00306      READ ELCNTL.                                                 EL503
00307                                                                   EL503
00308      IF CONTROL-FILE-STATUS NOT = ZERO                            EL503
00309          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL503
00310          MOVE ' CNTL READ ERROR- ' TO WS-ABEND-MESSAGE            EL503
00311          GO TO ABEND-PGM.                                         EL503
00312                                                                   EL503
00313      IF EP-SW = '1'                                               EL503
00314          IF CF-RATES-FILE-CREATE-DT LESS THAN                     EL503
00315             CF-RATES-FILE-MAINT-DT                                EL503
00316              MOVE 'E' TO ERROR-SW                                 EL503
00317              MOVE '- ABORT LOAD     -' TO DTL-MSG1                EL503
00318              MOVE '- MAINT APPLIED  -' TO DTL-MSG3                EL503
00319              DISPLAY '*EL503*  LOAD ABORTED FOR - ' DTE-CLIENT    EL503
00320                  UPON CONSOLE                                     EL503
00321              GO TO 150-EXIT                                       EL503
00322          ELSE                                                     EL503
00323              NEXT SENTENCE                                        EL503
00324      ELSE                                                         EL503
00325          MOVE 'E' TO ERROR-SW                                     EL503
00326          MOVE '- ABORT LOAD     -' TO DTL-MSG1                    EL503
00327          MOVE '- UPDATE SW NOT 1-' TO DTL-MSG3                    EL503
00328          DISPLAY '*EL503*  LOAD ABORTED FOR - ' DTE-CLIENT        EL503
00329              UPON CONSOLE                                         EL503
00330          GO TO 150-EXIT.                                          EL503
00331                                                                   EL503
00332      CLOSE ELCNTL.                                                EL503
00333                                                                   EL503
00334      IF CONTROL-FILE-STATUS NOT = ZERO                            EL503
00335          MOVE CONTROL-FILE-STATUS   TO WS-ABEND-FILE-STATUS       EL503
00336          MOVE ' CNTL CLOSE ERROR- ' TO WS-ABEND-MESSAGE           EL503
00337          GO TO ABEND-PGM.                                         EL503
00338                                                                   EL503
00339      MOVE +0 TO WS-CLCNTL-STATUS-SW.                              EL503
00340                                                                   EL503
00341  150-EXIT.                                                        EL503
00342      EXIT.                                                        EL503
00343                                                                   EL503
00344  170-UPDATE-CNTL.                                                 EL503
00345      OPEN I-O ELCNTL.                                             EL503
00346                                                                   EL503
00347      IF CONTROL-FILE-STATUS = '00' OR '97'                        EL503
00348          NEXT SENTENCE                                            EL503
00349        ELSE                                                       EL503
00350          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL503
00351          MOVE ' CNTL OPEN ERROR- ' TO WS-ABEND-MESSAGE            EL503
00352          GO TO ABEND-PGM.                                         EL503
00353                                                                   EL503
00354      MOVE +1                     TO WS-CLCNTL-STATUS-SW.          EL503
00355      MOVE SPACES                 TO CF-ACCESS-CD-GENL.            EL503
00356      MOVE DTE-CLIENT             TO CF-COMPANY-ID.                EL503
00357      MOVE '1'                    TO CF-RECORD-TYPE.               EL503
00358      MOVE ZEROS                  TO CF-SEQUENCE-NO.               EL503
00359                                                                   EL503
00360      READ ELCNTL.                                                 EL503
00361                                                                   EL503
00362      IF CONTROL-FILE-STATUS NOT = ZERO                            EL503
00363          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL503
00364          MOVE ' CNTL READ ERROR- ' TO WS-ABEND-MESSAGE            EL503
00365          GO TO ABEND-PGM.                                         EL503
00366                                                                   EL503
00367      MOVE WS-CURRENT-BIN-DT TO CF-RATES-FILE-MAINT-DT             EL503
00368                                CF-RATES-FILE-CREATE-DT.           EL503
00369                                                                   EL503
00370      REWRITE CONTROL-FILE.                                        EL503
00371                                                                   EL503
00372      IF CONTROL-FILE-STATUS NOT = ZERO                            EL503
00373          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL503
00374          MOVE ' CNTL REWRITE ERROR- ' TO WS-ABEND-MESSAGE         EL503
00375          GO TO ABEND-PGM.                                         EL503
00376                                                                   EL503
00377      CLOSE ELCNTL.                                                EL503
00378                                                                   EL503
00379      IF CONTROL-FILE-STATUS NOT = ZERO                            EL503
00380          MOVE CONTROL-FILE-STATUS   TO WS-ABEND-FILE-STATUS       EL503
00381          MOVE ' CNTL CLOSE ERROR- ' TO WS-ABEND-MESSAGE           EL503
00382          GO TO ABEND-PGM.                                         EL503
00383                                                                   EL503
00384      MOVE +0 TO WS-CLCNTL-STATUS-SW.                              EL503
00385                                                                   EL503
00386  170-EXIT.                                                        EL503
00387      EXIT.                                                        EL503
00388                                                                   EL503
00389  200-DELETE-OLD.                                                  EL503
00390      MOVE WS-CONTROL-PRIMARY  TO RT-CONTROL-PRIMARY.              EL503
00391                                                                   EL503
00392  205-START.                                                       EL503
00393      START ERRATE KEY NOT LESS RT-CONTROL-PRIMARY.                EL503
00394                                                                   EL503
00395      IF RATE-FILE-STATUS  = '23' OR '10'                          EL503
00396           GO TO 200-EXIT.                                         EL503
00397                                                                   EL503
00398      IF RATE-FILE-STATUS NOT = ZERO                               EL503
00399          MOVE RATE-FILE-STATUS  TO WS-ABEND-FILE-STATUS           EL503
00400          MOVE ' RATE START ERROR- '     TO WS-ABEND-MESSAGE       EL503
00401          GO TO ABEND-PGM.                                         EL503
00402                                                                   EL503
00403  210-READNEXT.                                                    EL503
00404      READ ERRATE  NEXT RECORD.                                    EL503
00405                                                                   EL503
00406      IF RTE1 = '1'                                                EL503
00407              GO TO 200-EXIT.                                      EL503
00408                                                                   EL503
00409      IF RATE-FILE-STATUS NOT = ZEROS                              EL503
00410          MOVE ' -ERROR ON READ- '      TO WS-ABEND-MESSAGE        EL503
00411          MOVE RATE-FILE-STATUS TO WS-ABEND-FILE-STATUS            EL503
00412          GO TO ABEND-PGM.                                         EL503
00413                                                                   EL503
00414      IF DTE-CLASIC-COMPANY-CD NOT = RT-COMPANY-CD                 EL503
00415          GO TO 200-EXIT.                                          EL503
00416                                                                   EL503
00417      DELETE  ERRATE  RECORD.                                      EL503
00418                                                                   EL503
00419      IF RATE-FILE-STATUS NOT = ZEROS                              EL503
00420          MOVE 'ERROR OCCURED DELETE - ELRATE'  TO WS-ABEND-MESSAGEEL503
00421          MOVE RATE-FILE-STATUS TO WS-ABEND-FILE-STATUS            EL503
00422          GO TO ABEND-PGM.                                         EL503
00423                                                                   EL503
00424      ADD 1  TO DELETE-COUNT.                                      EL503
00425                                                                   EL503
00426      GO TO 210-READNEXT.                                          EL503
00427                                                                   EL503
00428  200-EXIT.                                                        EL503
00429      EXIT.                                                        EL503
00430                                                                   EL503
00431      EJECT                                                        EL503
00432  300-LOAD.                                                        EL503
00433      READ RATE-MSTR                                               EL503
00434         IF VSAMRATE-FILE-STATUS = '23' OR '10'                    EL503
00435             MOVE 'E' TO VSAM-END-SW                               EL503
00436             GO TO 300-EXIT.                                       EL503
00437                                                                   EL503
00438      IF VSAMRATE-FILE-STATUS NOT = '00'                           EL503
00439          MOVE VSAMRATE-FILE-STATUS TO WS-ABEND-FILE-STATUS        EL503
00440          MOVE ' VSAM RATE READ ERROR- ' TO WS-ABEND-MESSAGE       EL503
00441          GO TO ABEND-PGM.                                         EL503
00442                                                                   EL503
00443      MOVE SPACES                 TO RATE-RECORD.                  EL503
00444      MOVE RATE-MSTR-REC          TO RATE-RECORD.                  EL503
00445      MOVE 'RT'                   TO RT-RECORD-ID.                 EL503
00446      MOVE DTE-CLASIC-COMPANY-CD  TO RT-COMPANY-CD.                EL503
00447      MOVE '9'                    TO RT-SEX.                       EL503
00448      MOVE '99'                   TO RT-FUTURE.                    EL503
00449      MOVE '999999'               TO RT-HIGH-AMT.                  EL503
00450      MOVE WS-CURRENT-BIN-DT      TO RT-LAST-MAINT-DT.             EL503
00451      MOVE WS-PROCESSOR           TO RT-LAST-MAINT-USER.           EL503
00452      ACCEPT WS-TIME-OF-DAY   FROM TIME.                           EL503
00453      MOVE WS-TIME                TO RT-LAST-MAINT-HHMMSS.         EL503
00454                                                                   EL503
00455      WRITE RATE-RECORD.                                           EL503
00456                                                                   EL503
00457      IF RATE-FILE-STATUS  = '22'                                  EL503
00458          GO TO 300-EXIT.                                          EL503
00459                                                                   EL503
00460      IF RATE-FILE-STATUS NOT = ZEROS                              EL503
00461          MOVE 'ERROR OCCURED WRITE - ELRATE' TO WS-ABEND-MESSAGE  EL503
00462          MOVE RATE-FILE-STATUS TO WS-ABEND-FILE-STATUS            EL503
00463          GO TO ABEND-PGM.                                         EL503
00464                                                                   EL503
00465      ADD  1 TO RECORD-COUNT.                                      EL503
00466                                                                   EL503
00467  300-EXIT.                                                        EL503
00468      EXIT.                                                        EL503
00469                                                                   EL503
00470  310-DATE-RTN.                                                    EL503
00471      CALL 'ELDATCX'  USING DATE-CONVERSION-DATA.                  EL503
00472                                                                   EL503
00473      IF DC-ERROR-CODE NOT = SPACE                                 EL503
00474          MOVE  ZEROS TO DC-BIN-DATE-1                             EL503
00475          ADD 1  TO DATE-ERROR-COUNT.                              EL503
00476                                                                   EL503
00477  310-EXIT.                                                        EL503
00478      EXIT.                                                        EL503
00479                                                                   EL503
00480      EJECT                                                        EL503
00481                                                                   EL503
00482  800-FINALIZE.                                                    EL503
00483      CLOSE RATE-MSTR                                              EL503
00484            PRNTR                                                  EL503
00485            ERRATE.                                                EL503
00486                                                                   EL503
00487      IF RATE-FILE-STATUS NOT = ZEROS                              EL503
00488          MOVE 'ERROR OCCURED CLOSE - ERRATE' TO WS-ABEND-MESSAGE  EL503
00489          MOVE RATE-FILE-STATUS TO WS-ABEND-FILE-STATUS            EL503
00490          GO TO ABEND-PGM.                                         EL503
00491                                                                   EL503
00492      IF ELCNTL-OPEN                                               EL503
00493          CLOSE ELCNTL                                             EL503
00494          IF CONTROL-FILE-STATUS NOT = ZEROS                       EL503
00495              MOVE 'ERROR OCCURED CLOSE - ELCNTL'                  EL503
00496                                  TO WS-ABEND-MESSAGE              EL503
00497              MOVE CONTROL-FILE-STATUS TO WS-ABEND-FILE-STATUS     EL503
00498              GO TO ABEND-PGM.                                     EL503
00499                                                                   EL503
00500  800-CLOSE-OTHER. COPY ELCPRTCX.                                  EL503
00501                                                                   EL503
00502  800-EXIT.                                                        EL503
00503      EXIT.                                                        EL503
00504      EJECT                                                        EL503
00505  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL503
00506                                                                   EL503
00507  WRITE-HEADINGS SECTION.                                          EL503
00508 ***************************************************************** EL503
00509 *                                                               * EL503
00510 *                            ELCWHS1.                           * EL503
00511 *                            VMOD=2.001                         * EL503
00512 *                                                               * EL503
00513 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          * EL503
00514 *****************************************************************.EL503
00515  WHS-010.                                                         EL503
00516      ADD +1  TO  WS-PAGE.                                         EL503
00517      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL503
00518      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL503
00519      MOVE ZERO                   TO  WS-LINE-COUNT.               EL503
00520                                                                   EL503
00521      MOVE WS-HEADING1            TO  PRT.                         EL503
00522      MOVE '1'                    TO  X.                           EL503
00523      PERFORM WRITE-PRINTER.                                       EL503
00524                                                                   EL503
00525      MOVE WS-HEADING2            TO  PRT.                         EL503
00526      MOVE ' '                    TO  X.                           EL503
00527      PERFORM WRITE-PRINTER.                                       EL503
00528                                                                   EL503
00529      MOVE WS-HEADING3            TO  PRT.                         EL503
00530      MOVE ' '                    TO  X.                           EL503
00531      PERFORM WRITE-PRINTER.                                       EL503
00532                                                                   EL503
00533      MOVE WS-HEADING4            TO  PRT.                         EL503
00534      MOVE ' '                    TO  X.                           EL503
00535      PERFORM WRITE-PRINTER.                                       EL503
00536                                                                   EL503
00537      MOVE +4 TO WS-LINE-COUNT.                                    EL503
00538                                                                   EL503
00539  WHS-020. COPY ELCWHS2.                                           EL503
00540                                                                   EL503
00541  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL503
00542  WPS-020. COPY ELCPRT2X.                                          EL503
00543                                                                   EL503
00544  ABEND-PGM  SECTION.  COPY ELCABEND.                                 CL**2
00545                                                                   EL503
00546                                                                   EL503
