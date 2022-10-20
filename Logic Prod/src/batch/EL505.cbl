00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL505
00003  PROGRAM-ID.                 EL505 .                                 LV004
00004 *              PROGRAM CONVERTED BY                               EL505
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL505
00006 *              CONVERSION DATE 02/19/96 16:27:17.                 EL505
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL505
00008 *                            VMOD=2.004                           EL505
00009                                                                   EL505
00009                                                                   EL505
00010 *AUTHOR.     LOGIC INC.                                           EL505
00011 *            DALLAS, TEXAS.                                       EL505
00012                                                                   EL505
00013 *DATE-COMPILED.                                                   EL505
00014                                                                   EL505
00015 *SECURITY.   *****************************************************EL505
00016 *            *                                                   *EL505
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL505
00018 *            *                                                   *EL505
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL505
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL505
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL505
00022 *            *                                                   *EL505
00023 *            *****************************************************EL505
00024                                                                   EL505
00025 *REMARKS.                                                         EL505
00026 *         THIS PROGRAM IS USED TO CREATE COMMISSION MASTER        EL505
00027 *       DATA SET FOR THE ONLINE CREDIT SYSTEM.  A CONTROL         EL505
00028 *       PAGE WILL BE PRINTED AT EOJ.                              EL505
00029                                                                   EL505
00030  ENVIRONMENT DIVISION.                                            EL505
00031                                                                   EL505
00032  INPUT-OUTPUT SECTION.                                            EL505
00033                                                                   EL505
00034  FILE-CONTROL.                                                    EL505
00035                                                                   EL505
00036      SELECT PRNTR          ASSIGN TO SYS008-UR-1403-S-SYS008.     EL505
00037                                                                   EL505
00038      SELECT FICH           ASSIGN TO SYS020-UT-2400-S-SYS020.     EL505
00039                                                                   EL505
00040      SELECT DISK-DATE      ASSIGN TO SYS019-FBA1-S-SYS019.        EL505
00041                                                                   EL505
00042      SELECT COMM-MSTR      ASSIGN TO SYS010                       EL505
00043              ORGANIZATION IS INDEXED                              EL505
00044              ACCESS IS SEQUENTIAL                                 EL505
00045              RECORD KEY IS CTBL-KEY                               EL505
00046              FILE STATUS IS VSAMCTBL-FILE-STATUS.                 EL505
00047                                                                   EL505
00048      SELECT ERCTBL         ASSIGN TO SYS011-FBA1-ERCTBL           EL505
00049              ORGANIZATION IS INDEXED                              EL505
00050              ACCESS IS DYNAMIC                                    EL505
00051              RECORD KEY IS CT-CONTROL-PRIMARY                     EL505
00052              FILE STATUS IS COMMISSION-FILE-STATUS.               EL505
00053                                                                   EL505
00054      SELECT ELCNTL         ASSIGN TO SYS012-FBA1-ELCNTL           EL505
00055              ORGANIZATION IS INDEXED                              EL505
00056              ACCESS IS DYNAMIC                                    EL505
00057              RECORD KEY IS CF-CONTROL-PRIMARY                     EL505
00058              FILE STATUS IS CONTROL-FILE-STATUS.                  EL505
00059                                                                   EL505
00060      SELECT ELREPT         ASSIGN TO SYS018-FBA1-ELREPT           EL505
00061              ORGANIZATION IS INDEXED                              EL505
00062              ACCESS IS DYNAMIC                                    EL505
00063              RECORD KEY IS RF-CONTROL-PRIMARY                     EL505
00064              FILE STATUS IS DTE-VSAM-FLAGS.                       EL505
00065                                                                   EL505
00066      EJECT                                                        EL505
00067  DATA DIVISION.                                                   EL505
00068                                                                   EL505
00069  FILE SECTION.                                                    EL505
00070                                                                   EL505
00071  FD  PRNTR                   COPY ELCPRTFD.                          CL**2
00072                                                                   EL505
00073  FD  FICH                    COPY ELCFCHFD.                          CL**2
00074                                                                   EL505
00075  FD  DISK-DATE               COPY ELCDTEFD.                          CL**2
00076                                                                   EL505
00077      EJECT                                                        EL505
00078  FD  COMM-MSTR.                                                   EL505
00079  01  COMM-MSTR-REC.                                               EL505
00080      05  FILLER            PIC XX.                                EL505
00081      05  CTBL-KEY          PIC X(7).                              EL505
00082      05  FILLER            PIC X(191).                            EL505
00083                                                                   EL505
00084      EJECT                                                        EL505
00085  FD  ERCTBL.                                                      EL505
00086      COPY ERCCTBL.                                                EL505
00087                                                                   EL505
00088      EJECT                                                        EL505
00089  FD  ELCNTL.                                                      EL505
00090      COPY ELCCNTL.                                                EL505
00091                                                                   EL505
00092      EJECT                                                        EL505
00093  FD  ELREPT.                                                      EL505
00094      COPY ELCREPT.                                                EL505
00095                                                                   EL505
00096      EJECT                                                        EL505
00097  WORKING-STORAGE SECTION.                                         EL505
00098  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL505
00099                                                                   EL505
00100  77  FILLER  PIC X(32) VALUE '********************************'.  EL505
00101  77  FILLER  PIC X(32) VALUE '      EL505 WORKING-STORAGE     '.  EL505
00102  77  FILLER  PIC X(32) VALUE '************VMOD=2.004 *********'.  EL505
00103                                                                   EL505
00104  01  WORK-AREAS.                                                  EL505
00105      12  WS-LINE-COUNT          PIC S9(3)  VALUE +99 COMP-3.      EL505
00106      12  WS-LINE-COUNT-MAX      PIC S9(3)  VALUE +60 COMP-3.      EL505
00107      12  WS-PAGE                PIC S9(3)  VALUE +0  COMP-3.      EL505
00108      12  WS-ZERO                PIC S9     VALUE +0  COMP-3.      EL505
00109      12  WS-RETURN-CODE         PIC S9(4)            COMP.        EL505
00110                                                                   EL505
00111      12  WS-CURRENT-BIN-DT      PIC XX.                           EL505
00112      12  WS-ABEND-MESSAGE       PIC X(80).                        EL505
00113      12  WS-ABEND-FILE-STATUS   PIC XX   VALUE ZEROS.             EL505
00114      12  COMMISSION-FILE-STATUS PIC XX   VALUE ZEROS.             EL505
00115      12  COM REDEFINES COMMISSION-FILE-STATUS.                    EL505
00116          16  COM1               PIC X.                            EL505
00117          16  COM2               PIC X.                            EL505
00118      12  VSAMCTBL-FILE-STATUS   PIC XX   VALUE ZEROS.             EL505
00119      12  CONTROL-FILE-STATUS    PIC XX  VALUE ZEROS.              EL505
00120                                                                   EL505
00121      12  EOF-SW                 PIC X    VALUE SPACE.             EL505
00122          88  END-OF-FILE                 VALUE 'E'.               EL505
00123                                                                   EL505
00124      12  VSAM-EOF-SW            PIC X    VALUE SPACE.             EL505
00125          88  END-OF-VSAM-FILE            VALUE 'E'.               EL505
00126                                                                   EL505
00127      12  ERROR-SW               PIC X    VALUE SPACE.             EL505
00128          88  ERROR-OCCURRED              VALUE 'E'.               EL505
00129          88  NO-ERRORS                   VALUE ' '.               EL505
00130                                                                   EL505
00131      12  WS-CLCNTL-STATUS-SW    PIC S9   VALUE +0.                EL505
00132          88  ELCNTL-NOT-OPEN             VALUE +0.                EL505
00133          88  ELCNTL-OPEN                 VALUE +1.                EL505
00134                                                                   EL505
00135      12  WS-DUMMY-VARIABLE      PIC X    VALUE ZEROS.             EL505
00136          88  VAR-ACCT                    VALUE '3'.               EL505
00137          88  VAR-ACCT-STAT               VALUE ' '.               EL505
00138          88  VAR-ACCT-CARR               VALUE '4'.               EL505
00139          88  VAR-ACCT-CARR-STAT          VALUE '2'.               EL505
00140          88  VAR-ALL                     VALUE '1'.               EL505
00141                                                                   EL505
00142  01  DTE-INTERFACE-CODES.                                         EL505
00143      12  X                 PIC X           VALUE SPACE.           EL505
00144      12  PGM-SUB           PIC S9(4)  COMP VALUE +505.            EL505
00145      12  ABEND-CODE        PIC 9999        VALUE ZERO.            EL505
00146      12  ABEND-OPTION      PIC X           VALUE SPACE.           EL505
00147      12  OLC-REPORT-NAME   PIC X(6)        VALUE 'EL505'.         EL505
00148      12  WS-PROCESSOR      PIC X(4)        VALUE SPACES.          EL505
00149                                                                   EL505
00150  01  WS-CONTROL-PRIMARY.                                          EL505
00151      05  WS-COMPANY-CD      PIC X.                                EL505
00152      05  WS-TABLE           PIC X(3).                             EL505
00153      05  WS-CNTRL-2.                                              EL505
00154          10  WS-BEN-TYPE    PIC X.                                EL505
00155          10  WS-BEN-CODE    PIC X(2).                             EL505
00156                                                                   EL505
00157  01  COMP-3-WORK-AREA.                                            EL505
00158      05  K1                 PIC S9(7)  VALUE +1.                  EL505
00159      05  K2                 PIC S9(7)  VALUE +2.                  EL505
00160      05  RECORD-COUNT       PIC S9(7)  VALUE +0.                  EL505
00161      05  DELETE-COUNT       PIC S9(7)  VALUE +0.                  EL505
00162      05  DATE-ERROR-COUNT   PIC S9(7)  VALUE +0.                  EL505
00163                                                                   EL505
00164  01  WS-SAVE-PRINT-RECORD   PIC X(133) VALUE SPACES.              EL505
00165                                                                   EL505
00166      EJECT                                                        EL505
00167      COPY ELCDATE.                                                   CL**4
00168                                                                   EL505
00169      EJECT                                                        EL505
00170  01  WS-HEADING1.                                                 EL505
00171      05  FILLER                      PIC X(51)       VALUE '1'.   EL505
00172      05  WS-H1-TITLE                 PIC X(73)       VALUE        EL505
00173          'COMMISSION FILE LOAD  '.                                EL505
00174      05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL505  '.    EL505
00175                                                                   EL505
00176  01  WS-HEADING2.                                                 EL505
00177      05  FILLER                      PIC X(46)       VALUE SPACES.EL505
00178      05  WS-H2-CLIENT-NAME           PIC X(78)       VALUE SPACES.EL505
00179      05  WS-H2-DATE                  PIC X(8)        VALUE SPACES.EL505
00180      05  FILLER                      PIC X           VALUE SPACES.EL505
00181                                                                   EL505
00182  01  WS-HEADING3.                                                 EL505
00183      05  FILLER                      PIC X(51)       VALUE SPACES.EL505
00184      05  WS-H3-DATE                  PIC X(60)       VALUE SPACES.EL505
00185      05  FILLER                      PIC X(5)        VALUE 'PAGE'.EL505
00186      05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL505
00187      05  FILLER                      PIC X(11)       VALUE SPACES.EL505
00188                                                                   EL505
00189  01  WS-HEADING4                     PIC X(132)      VALUE SPACES.EL505
00190                                                                   EL505
00191  01  WS-DETAIL1.                                                  EL505
00192      05  FILLER             PIC X.                                EL505
00193      05  DTL-MSG1           PIC X(18) VALUE 'SUCCESSFUL LOAD   '. EL505
00194      05  DTL-MSG2           PIC X(18) VALUE SPACE.                EL505
00195      05  DTL-MSG3           PIC X(18) VALUE SPACE.                EL505
00196      05  D-RECORD-COUNT     PIC Z,ZZZ,ZZ9.                        EL505
00197      05  FILLER             PIC X(10) VALUE ' RECORDS ('.         EL505
00198      05  D-DELETE-COUNT     PIC Z,ZZZ,ZZ9.                        EL505
00199      05  FILLER             PIC X(11) VALUE ' DELETED)'.          EL505
00200      05  FILLER             PIC X(15) VALUE '   DATE ERRORS '.    EL505
00201      05  D-DATE-ERROR-COUNT PIC Z,ZZZ,ZZ9.                        EL505
00202                                                                   EL505
00203      COPY ELCDTECX.                                               EL505
00204                                                                      CL**3
00205      COPY ELCDTEVR.                                                  CL**3
00206                                                                   EL505
00207      EJECT                                                        EL505
00208  PROCEDURE DIVISION.                                              EL505
00209                                                                   EL505
00210  0000-LOAD-DATE-WS. COPY ELCDTERX.                                   CL**2
00211                                                                   EL505
00212  000-MAINLINE.                                                    EL505
00213      PERFORM 100-INITIALIZE  THRU 100-EXIT.                       EL505
00214                                                                   EL505
00215      PERFORM 150-READ-CNTL THRU 150-EXIT.                         EL505
00216                                                                   EL505
00217      IF NO-ERRORS                                                 EL505
00218          PERFORM 200-DELETE-OLD THRU 200-EXIT.                    EL505
00219                                                                   EL505
00220      IF NO-ERRORS                                                 EL505
00221          PERFORM 300-LOAD THRU 300-EXIT                           EL505
00222                  UNTIL END-OF-VSAM-FILE.                          EL505
00223                                                                   EL505
00224      IF NO-ERRORS                                                 EL505
00225          PERFORM 170-UPDATE-CNTL THRU 170-EXIT.                   EL505
00226                                                                   EL505
00227      MOVE RECORD-COUNT     TO D-RECORD-COUNT.                     EL505
00228      MOVE DELETE-COUNT     TO D-DELETE-COUNT.                     EL505
00229      MOVE DATE-ERROR-COUNT TO D-DATE-ERROR-COUNT.                 EL505
00230      MOVE WS-DETAIL1       TO PRT.                                EL505
00231      PERFORM WRITE-A-LINE.                                        EL505
00232                                                                   EL505
00233      PERFORM 800-FINALIZE THRU 800-EXIT.                          EL505
00234                                                                   EL505
00235  000-MAINLINE-EXIT.                                               EL505
00236      GOBACK.                                                      EL505
00237                                                                   EL505
00238  100-INITIALIZE.                                                  EL505
00239      MOVE ZEROS        TO WS-RETURN-CODE.                         EL505
00240      MOVE COMPANY-NAME           TO WS-H2-CLIENT-NAME.            EL505
00241      MOVE WS-CURRENT-DATE        TO WS-H2-DATE.                   EL505
00242      MOVE ALPH-DATE              TO WS-H3-DATE.                   EL505
00243                                                                   EL505
00244      OPEN INPUT  COMM-MSTR                                        EL505
00245           I-O    ERCTBL                                           EL505
00246           OUTPUT PRNTR.                                           EL505
00247                                                                   EL505
00248      IF COMMISSION-FILE-STATUS = '00' OR '97'                     EL505
00249          NEXT SENTENCE                                            EL505
00250        ELSE                                                       EL505
00251          MOVE COMMISSION-FILE-STATUS  TO WS-ABEND-FILE-STATUS     EL505
00252          MOVE ' COMM OPEN ERROR- '      TO WS-ABEND-MESSAGE       EL505
00253          GO TO ABEND-PGM.                                         EL505
00254                                                                   EL505
00255      IF VSAMCTBL-FILE-STATUS = '00' OR '97'                       EL505
00256          NEXT SENTENCE                                            EL505
00257        ELSE                                                       EL505
00258          MOVE VSAMCTBL-FILE-STATUS  TO WS-ABEND-FILE-STATUS       EL505
00259          MOVE 'VSAM COMM OPEN ERROR- '      TO WS-ABEND-MESSAGE   EL505
00260          GO TO ABEND-PGM.                                         EL505
00261                                                                   EL505
00262      MOVE LOW-VALUES            TO WS-CONTROL-PRIMARY.            EL505
00263      MOVE DTE-CLASIC-COMPANY-CD TO WS-COMPANY-CD.                 EL505
00264      MOVE DTE-CLIENT            TO WS-PROCESSOR.                  EL505
00265                                                                   EL505
00266      MOVE WS-CURRENT-DATE TO DC-GREG-DATE-1-EDIT.                 EL505
00267      MOVE '2'             TO DC-OPTION-CODE.                      EL505
00268      PERFORM 310-DATE-RTN THRU 310-EXIT.                          EL505
00269      MOVE DC-BIN-DATE-1   TO WS-CURRENT-BIN-DT.                   EL505
00270                                                                   EL505
00271  100-EXIT.                                                        EL505
00272      EXIT.                                                        EL505
00273      EJECT                                                        EL505
00274  150-READ-CNTL.                                                   EL505
00275      OPEN INPUT ELCNTL.                                           EL505
00276                                                                   EL505
00277      IF CONTROL-FILE-STATUS = '00' OR '97'                        EL505
00278          NEXT SENTENCE                                            EL505
00279        ELSE                                                       EL505
00280          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL505
00281          MOVE ' CNTL OPEN ERROR- ' TO WS-ABEND-MESSAGE            EL505
00282          GO TO ABEND-PGM.                                         EL505
00283                                                                   EL505
00284      MOVE +1                     TO WS-CLCNTL-STATUS-SW.          EL505
00285      MOVE SPACES                 TO CF-ACCESS-CD-GENL.            EL505
00286      MOVE DTE-CLIENT             TO CF-COMPANY-ID.                EL505
00287      MOVE '1'                    TO CF-RECORD-TYPE.               EL505
00288      MOVE ZEROS                  TO CF-SEQUENCE-NO.               EL505
00289                                                                   EL505
00290      READ ELCNTL.                                                 EL505
00291                                                                   EL505
00292      IF CONTROL-FILE-STATUS NOT = ZERO                            EL505
00293          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL505
00294          MOVE ' CNTL READ ERROR- ' TO WS-ABEND-MESSAGE            EL505
00295          GO TO ABEND-PGM.                                         EL505
00296                                                                   EL505
00297      IF EP-SW = '1'                                               EL505
00298          IF CF-COMMISSION-TAB-CREATE-DT LESS THAN                 EL505
00299             CF-COMMISSION-TAB-MAINT-DT                            EL505
00300              MOVE 'E' TO ERROR-SW                                 EL505
00301              MOVE '- ABORT LOAD     -' TO DTL-MSG1                EL505
00302              MOVE '- MAINT APPLIED  -' TO DTL-MSG3                EL505
00303              DISPLAY '*EL505*  LOAD ABORTED FOR - ' DTE-CLIENT    EL505
00304                  UPON CONSOLE                                     EL505
00305              GO TO 150-EXIT                                       EL505
00306          ELSE                                                     EL505
00307              NEXT SENTENCE                                        EL505
00308      ELSE                                                         EL505
00309          MOVE 'E' TO ERROR-SW                                     EL505
00310          MOVE '- ABORT LOAD     -' TO DTL-MSG1                    EL505
00311          MOVE '- MAINT APPLIED  -' TO DTL-MSG3                    EL505
00312          DISPLAY '*EL505*  LOAD ABORTED FOR - ' DTE-CLIENT        EL505
00313              UPON CONSOLE                                         EL505
00314          GO TO 150-EXIT.                                          EL505
00315                                                                   EL505
00316      CLOSE ELCNTL.                                                EL505
00317                                                                   EL505
00318      IF CONTROL-FILE-STATUS NOT = ZERO                            EL505
00319          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL505
00320          MOVE ' CNTL CLOSE ERROR- ' TO WS-ABEND-MESSAGE           EL505
00321          GO TO ABEND-PGM.                                         EL505
00322                                                                   EL505
00323      MOVE +0                     TO WS-CLCNTL-STATUS-SW.          EL505
00324                                                                   EL505
00325  150-EXIT.                                                        EL505
00326      EXIT.                                                        EL505
00327                                                                   EL505
00328  170-UPDATE-CNTL.                                                 EL505
00329      OPEN I-O ELCNTL.                                             EL505
00330                                                                   EL505
00331      IF CONTROL-FILE-STATUS = '00' OR '97'                        EL505
00332          NEXT SENTENCE                                            EL505
00333        ELSE                                                       EL505
00334          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL505
00335          MOVE ' CNTL OPEN ERROR- ' TO WS-ABEND-MESSAGE            EL505
00336          GO TO ABEND-PGM.                                         EL505
00337                                                                   EL505
00338      MOVE +1                     TO WS-CLCNTL-STATUS-SW.          EL505
00339      MOVE SPACES                 TO CF-ACCESS-CD-GENL.            EL505
00340      MOVE DTE-CLIENT             TO CF-COMPANY-ID.                EL505
00341      MOVE '1'                    TO CF-RECORD-TYPE.               EL505
00342      MOVE ZEROS                  TO CF-SEQUENCE-NO.               EL505
00343                                                                   EL505
00344      READ ELCNTL.                                                 EL505
00345                                                                   EL505
00346      IF CONTROL-FILE-STATUS NOT = ZERO                            EL505
00347          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL505
00348          MOVE ' CNTL READ ERROR- ' TO WS-ABEND-MESSAGE            EL505
00349          GO TO ABEND-PGM.                                         EL505
00350                                                                   EL505
00351      MOVE WS-CURRENT-BIN-DT TO CF-COMMISSION-TAB-MAINT-DT         EL505
00352                                CF-COMMISSION-TAB-CREATE-DT.       EL505
00353                                                                   EL505
00354      REWRITE CONTROL-FILE.                                        EL505
00355                                                                   EL505
00356      IF CONTROL-FILE-STATUS NOT = ZERO                            EL505
00357          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL505
00358          MOVE ' CNTL REWRITE ERROR- ' TO WS-ABEND-MESSAGE         EL505
00359          GO TO ABEND-PGM.                                         EL505
00360                                                                   EL505
00361      CLOSE ELCNTL.                                                EL505
00362                                                                   EL505
00363      IF CONTROL-FILE-STATUS NOT = ZERO                            EL505
00364          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL505
00365          MOVE ' CNTL CLOSE ERROR- ' TO WS-ABEND-MESSAGE           EL505
00366          GO TO ABEND-PGM.                                         EL505
00367                                                                   EL505
00368      MOVE +0                     TO WS-CLCNTL-STATUS-SW.          EL505
00369                                                                   EL505
00370  170-EXIT.                                                        EL505
00371      EXIT.                                                        EL505
00372                                                                   EL505
00373  200-DELETE-OLD.                                                  EL505
00374      MOVE WS-CONTROL-PRIMARY  TO CT-CONTROL-PRIMARY.              EL505
00375                                                                   EL505
00376  205-START.                                                       EL505
00377      START ERCTBL KEY NOT LESS CT-CONTROL-PRIMARY.                EL505
00378                                                                   EL505
00379      IF COMMISSION-FILE-STATUS  = '10' OR '23'                    EL505
00380           GO TO 200-EXIT.                                         EL505
00381                                                                   EL505
00382      IF COMMISSION-FILE-STATUS NOT = ZERO                         EL505
00383          MOVE COMMISSION-FILE-STATUS  TO WS-ABEND-FILE-STATUS     EL505
00384          MOVE ' COMM START ERROR- '     TO WS-ABEND-MESSAGE       EL505
00385          GO TO ABEND-PGM.                                         EL505
00386                                                                   EL505
00387  210-READNEXT.                                                    EL505
00388      READ ERCTBL  NEXT RECORD.                                    EL505
00389                                                                   EL505
00390      IF COM1 = '1'                                                EL505
00391              GO TO 200-EXIT.                                      EL505
00392                                                                   EL505
00393      IF COMMISSION-FILE-STATUS NOT = ZEROS                        EL505
00394          MOVE ' -ERROR ON READ- '      TO WS-ABEND-MESSAGE        EL505
00395          MOVE COMMISSION-FILE-STATUS TO WS-ABEND-FILE-STATUS      EL505
00396          GO TO ABEND-PGM.                                         EL505
00397                                                                   EL505
00398      IF DTE-CLASIC-COMPANY-CD NOT = CT-COMPANY-CD                 EL505
00399          GO TO 200-EXIT.                                          EL505
00400                                                                   EL505
00401      DELETE  ERCTBL  RECORD.                                      EL505
00402                                                                   EL505
00403      PERFORM 230-ERROR-CHECK  THRU 230-EXIT.                      EL505
00404                                                                   EL505
00405      IF ERROR-OCCURRED                                            EL505
00406          MOVE ' -ERR ON DELETE- ' TO DTL-MSG3                     EL505
00407          GO TO 200-EXIT.                                          EL505
00408                                                                   EL505
00409      ADD 1  TO DELETE-COUNT.                                      EL505
00410                                                                   EL505
00411      GO TO 210-READNEXT.                                          EL505
00412                                                                   EL505
00413  200-EXIT.                                                        EL505
00414      EXIT.                                                        EL505
00415                                                                   EL505
00416  230-ERROR-CHECK.                                                 EL505
00417      IF COM1 NOT = '0'                                            EL505
00418          MOVE   COM  TO DTL-MSG2                                  EL505
00419          MOVE   'E'  TO ERROR-SW.                                 EL505
00420                                                                   EL505
00421  230-EXIT.                                                        EL505
00422      EXIT.                                                        EL505
00423      EJECT                                                        EL505
00424                                                                   EL505
00425  300-LOAD.                                                        EL505
00426      READ COMM-MSTR.                                              EL505
00427             IF VSAMCTBL-FILE-STATUS = '23' OR '10'                EL505
00428                 MOVE 'E' TO VSAM-EOF-SW                           EL505
00429                 GO TO 300-EXIT.                                   EL505
00430                                                                   EL505
00431      IF VSAMCTBL-FILE-STATUS NOT = ZEROS                          EL505
00432          MOVE ' -ERROR ON VSAM READ- '      TO WS-ABEND-MESSAGE   EL505
00433          MOVE VSAMCTBL-FILE-STATUS TO WS-ABEND-FILE-STATUS        EL505
00434          GO TO ABEND-PGM.                                         EL505
00435                                                                   EL505
00436      MOVE SPACES                 TO COMM-TABLE-RECORD.            EL505
00437      MOVE COMM-MSTR-REC          TO COMM-TABLE-RECORD.            EL505
00438      MOVE DTE-CLASIC-COMPANY-CD  TO CT-COMPANY-CD.                EL505
00439                                                                   EL505
00440      MOVE WS-CURRENT-BIN-DT      TO CT-LAST-MAINT-DT.             EL505
00441      MOVE WS-PROCESSOR           TO CT-LAST-MAINT-USER.           EL505
00442      ACCEPT WS-TIME-OF-DAY   FROM TIME.                           EL505
00443      MOVE WS-TIME                TO CT-LAST-MAINT-HHMMSS.         EL505
00444                                                                   EL505
00445      WRITE COMM-TABLE-RECORD.                                     EL505
00446                                                                   EL505
00447      PERFORM 230-ERROR-CHECK  THRU 230-EXIT.                      EL505
00448                                                                   EL505
00449      IF ERROR-OCCURRED                                            EL505
00450          MOVE ' -ERR ON WRITE- ' TO DTL-MSG3                      EL505
00451          GO TO 300-EXIT.                                          EL505
00452                                                                   EL505
00453      ADD  1 TO RECORD-COUNT.                                      EL505
00454                                                                   EL505
00455  300-EXIT.                                                        EL505
00456      EXIT.                                                        EL505
00457                                                                   EL505
00458  310-DATE-RTN.                                                    EL505
00459      CALL 'ELDATCX'  USING DATE-CONVERSION-DATA.                  EL505
00460                                                                   EL505
00461      IF DC-ERROR-CODE NOT = SPACE                                 EL505
00462          MOVE  ZEROS TO DC-BIN-DATE-1                             EL505
00463          ADD 1  TO DATE-ERROR-COUNT.                              EL505
00464                                                                   EL505
00465  310-EXIT.                                                        EL505
00466      EXIT.                                                        EL505
00467                                                                   EL505
00468      EJECT                                                        EL505
00469                                                                   EL505
00470  800-FINALIZE.                                                    EL505
00471      CLOSE COMM-MSTR                                              EL505
00472            PRNTR                                                  EL505
00473            ERCTBL.                                                EL505
00474                                                                   EL505
00475      IF COMMISSION-FILE-STATUS NOT = ZERO                         EL505
00476          MOVE COMMISSION-FILE-STATUS  TO WS-ABEND-FILE-STATUS     EL505
00477          MOVE ' CTBL CLOSE ERROR- ' TO WS-ABEND-MESSAGE           EL505
00478          GO TO ABEND-PGM.                                         EL505
00479                                                                   EL505
00480      IF ELCNTL-OPEN                                               EL505
00481          CLOSE ELCNTL                                             EL505
00482          IF CONTROL-FILE-STATUS NOT = ZERO                        EL505
00483              MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS    EL505
00484              MOVE ' CNTL CLOSE ERROR- ' TO WS-ABEND-MESSAGE       EL505
00485              GO TO ABEND-PGM.                                     EL505
00486                                                                   EL505
00487  800-CLOSE-OTHER. COPY ELCPRTCX.                                  EL505
00488                                                                   EL505
00489  800-EXIT.                                                        EL505
00490      EXIT.                                                        EL505
00491      EJECT                                                        EL505
00492  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL505
00493                                                                   EL505
00494  WRITE-HEADINGS SECTION.                                          EL505
00495 ***************************************************************** EL505
00496 *                                                               * EL505
00497 *                            ELCWHS1.                           * EL505
00498 *                            VMOD=2.001                         * EL505
00499 *                                                               * EL505
00500 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          * EL505
00501 *****************************************************************.EL505
00502  WHS-010.                                                         EL505
00503      ADD +1  TO  WS-PAGE.                                         EL505
00504      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL505
00505      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL505
00506      MOVE ZERO                   TO  WS-LINE-COUNT.               EL505
00507                                                                   EL505
00508      MOVE WS-HEADING1            TO  PRT.                         EL505
00509      MOVE '1'                    TO  X.                           EL505
00510      PERFORM WRITE-PRINTER.                                       EL505
00511                                                                   EL505
00512      MOVE WS-HEADING2            TO  PRT.                         EL505
00513      MOVE ' '                    TO  X.                           EL505
00514      PERFORM WRITE-PRINTER.                                       EL505
00515                                                                   EL505
00516      MOVE WS-HEADING3            TO  PRT.                         EL505
00517      MOVE ' '                    TO  X.                           EL505
00518      PERFORM WRITE-PRINTER.                                       EL505
00519                                                                   EL505
00520      MOVE WS-HEADING4            TO  PRT.                         EL505
00521      MOVE ' '                    TO  X.                           EL505
00522      PERFORM WRITE-PRINTER.                                       EL505
00523                                                                   EL505
00524      MOVE +4 TO WS-LINE-COUNT.                                    EL505
00525                                                                   EL505
00526  WHS-020. COPY ELCWHS2.                                           EL505
00527                                                                   EL505
00528  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL505
00529  WPS-020. COPY ELCPRT2X.                                          EL505
00530                                                                   EL505
00531  ABEND-PGM  SECTION.  COPY ELCABEND.                                 CL**2
00532                                                                   EL505
