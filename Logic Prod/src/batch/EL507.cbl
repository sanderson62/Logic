00001  IDENTIFICATION DIVISION.                                         10/11/97
00002                                                                   EL507
00003  PROGRAM-ID.                 EL507 .                                 LV003
00004 *              PROGRAM CONVERTED BY                               EL507
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL507
00006 *              CONVERSION DATE 02/19/96 16:46:32.                 EL507
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL507
00008 *                            VMOD=2.003                           EL507
00009                                                                   EL507
00009                                                                   EL507
00010 *AUTHOR.     LOGIC INC.                                           EL507
00011 *            DALLAS, TEXAS.                                       EL507
00012                                                                   EL507
00013 *DATE-COMPILED.                                                   EL507
00014                                                                   EL507
00015 *SECURITY.   *****************************************************EL507
00016 *            *                                                   *EL507
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL507
00018 *            *                                                   *EL507
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL507
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL507
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL507
00022 *            *                                                   *EL507
00023 *            *****************************************************EL507
00024                                                                   EL507
00025 *REMARKS.                                                         EL507
00026 *         THIS PROGRAM IS USED TO CREATE THE REINSURANCE MASTER   EL507
00027 *       DATA SET FOR THE ONLINE CREDIT SYSTEM.  A CONTROL PAGE    EL507
00028 *       WILL BE PRINTED AT EOJ.                                   EL507
00029                                                                   EL507
00030  ENVIRONMENT DIVISION.                                            EL507
00031                                                                   EL507
00032  INPUT-OUTPUT SECTION.                                            EL507
00033                                                                   EL507
00034  FILE-CONTROL.                                                    EL507
00035                                                                   EL507
00036      SELECT PRNTR          ASSIGN TO SYS008-UR-1403-S-SYS008.     EL507
00037                                                                   EL507
00038      SELECT FICH           ASSIGN TO SYS020-UT-2400-S-SYS020.     EL507
00039                                                                   EL507
00040      SELECT DISK-DATE      ASSIGN TO SYS019-FBA1-S-SYS019.        EL507
00041                                                                   EL507
00042      SELECT REIN-MSTR      ASSIGN TO SYS010                       EL507
00043              ORGANIZATION IS INDEXED                              EL507
00044              ACCESS IS SEQUENTIAL                                 EL507
00045              RECORD KEY IS REIN-KEY                               EL507
00046              FILE STATUS IS VSAMREIN-FILE-STATUS.                 EL507
00047                                                                   EL507
00048      SELECT ERREIN         ASSIGN TO SYS011-FBA1-ERREIN           EL507
00049              ORGANIZATION IS INDEXED                              EL507
00050              ACCESS IS DYNAMIC                                    EL507
00051              RECORD KEY IS RE-CONTROL-PRIMARY                     EL507
00052              FILE STATUS IS REINSURANCE-FILE-STATUS.              EL507
00053                                                                   EL507
00054      SELECT ELCNTL         ASSIGN TO SYS012-FBA1-ELCNTL           EL507
00055              ORGANIZATION IS INDEXED                              EL507
00056              ACCESS IS DYNAMIC                                    EL507
00057              RECORD KEY IS CF-CONTROL-PRIMARY                     EL507
00058              FILE STATUS IS CONTROL-FILE-STATUS.                  EL507
00059                                                                   EL507
00060      SELECT ELREPT         ASSIGN TO SYS018-FBA1-ELREPT           EL507
00061              ORGANIZATION IS INDEXED                              EL507
00062              ACCESS IS DYNAMIC                                    EL507
00063              RECORD KEY IS RF-CONTROL-PRIMARY                     EL507
00064              FILE STATUS IS DTE-VSAM-FLAGS.                       EL507
00065                                                                   EL507
00066      EJECT                                                        EL507
00067  DATA DIVISION.                                                   EL507
00068                                                                   EL507
00069  FILE SECTION.                                                    EL507
00070                                                                   EL507
00071  FD  PRNTR                   COPY ELCPRTFD.                          CL**2
00072                                                                   EL507
00073  FD  FICH                    COPY ELCFCHFD.                          CL**2
00074                                                                   EL507
00075  FD  DISK-DATE               COPY ELCDTEFD.                          CL**2
00076                                                                   EL507
00077      EJECT                                                        EL507
00078  FD  REIN-MSTR.                                                   EL507
00079  01  REIN-MSTR-REC.                                               EL507
00080      05  FILLER            PIC XX.                                EL507
00081      05  REIN-KEY          PIC X(8).                              EL507
00082      05  FILLER            PIC X(3990).                           EL507
00083                                                                   EL507
00084      EJECT                                                        EL507
00085  FD  ERREIN.                                                      EL507
00086      COPY ERCREIN.                                                EL507
00087                                                                   EL507
00088      EJECT                                                        EL507
00089  FD  ELCNTL.                                                      EL507
00090      COPY ELCCNTL.                                                EL507
00091                                                                   EL507
00092      EJECT                                                        EL507
00093  FD  ELREPT.                                                      EL507
00094      COPY ELCREPT.                                                EL507
00095                                                                   EL507
00096      EJECT                                                        EL507
00097  WORKING-STORAGE SECTION.                                         EL507
00098  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL507
00099                                                                   EL507
00100  77  FILLER  PIC X(32) VALUE '********************************'.  EL507
00101  77  FILLER  PIC X(32) VALUE '      EL507 WORKING-STORAGE     '.  EL507
00102  77  FILLER  PIC X(32) VALUE '************VMOD=2.003 *********'.  EL507
00103                                                                   EL507
00104  01  WORK-AREAS.                                                  EL507
00105      12  WS-LINE-COUNT          PIC S9(3)  VALUE +99 COMP-3.      EL507
00106      12  WS-LINE-COUNT-MAX      PIC S9(3)  VALUE +60 COMP-3.      EL507
00107      12  WS-PAGE                PIC S9(3)  VALUE +0  COMP-3.      EL507
00108      12  WS-ZERO                PIC S9     VALUE +0  COMP-3.      EL507
00109      12  WS-RETURN-CODE         PIC S9(4)            COMP.        EL507
00110                                                                   EL507
00111      12  WS-CURRENT-BIN-DT      PIC XX.                           EL507
00112      12  WS-ABEND-MESSAGE       PIC X(80).                        EL507
00113      12  WS-ABEND-FILE-STATUS   PIC XX    VALUE ZEROS.            EL507
00114      12  REINSURANCE-FILE-STATUS  PIC XX  VALUE ZEROS.            EL507
00115      12  REN REDEFINES REINSURANCE-FILE-STATUS.                   EL507
00116          16  REN1               PIC X.                            EL507
00117          16  REN2               PIC X.                            EL507
00118      12  CONTROL-FILE-STATUS    PIC XX  VALUE ZEROS.              EL507
00119      12  VSAMREIN-FILE-STATUS   PIC XX  VALUE ZEROS.              EL507
00120                                                                   EL507
00121      12  EOF-SW                 PIC X    VALUE SPACE.             EL507
00122          88  END-OF-FILE                 VALUE 'E'.               EL507
00123                                                                   EL507
00124      12  VSAM-EOF-SW                 PIC X    VALUE SPACE.        EL507
00125          88  END-OF-VSAM-FILE                 VALUE 'E'.          EL507
00126                                                                   EL507
00127      12  ERROR-SW               PIC X    VALUE SPACE.             EL507
00128          88  ERROR-OCCURRED              VALUE 'E'.               EL507
00129          88  NO-ERRORS                   VALUE ' '.               EL507
00130                                                                   EL507
00131      12  WS-CLCNTL-STATUS-SW    PIC S9   VALUE +0.                EL507
00132          88  ELCNTL-NOT-OPEN             VALUE +0.                EL507
00133          88  ELCNTL-OPEN                 VALUE +1.                EL507
00134                                                                   EL507
00135      12  WS-DUMMY-VARIABLE      PIC X    VALUE ZEROS.             EL507
00136          88  VAR-ACCT                    VALUE '3'.               EL507
00137          88  VAR-ACCT-STAT               VALUE ' '.               EL507
00138          88  VAR-ACCT-CARR               VALUE '4'.               EL507
00139          88  VAR-ACCT-CARR-STAT          VALUE '2'.               EL507
00140          88  VAR-ALL                     VALUE '1'.               EL507
00141                                                                   EL507
00142  01  DTE-INTERFACE-CODES.                                         EL507
00143      12  X                 PIC X           VALUE SPACE.           EL507
00144      12  PGM-SUB           PIC S9(4)  COMP VALUE +507.            EL507
00145      12  ABEND-CODE        PIC 9999        VALUE ZERO.            EL507
00146      12  ABEND-OPTION      PIC X           VALUE SPACE.           EL507
00147      12  OLC-REPORT-NAME   PIC X(6)        VALUE 'EL507'.         EL507
00148      12  WS-PROCESSOR      PIC X(4)        VALUE SPACES.          EL507
00149                                                                   EL507
00150                                                                   EL507
00151  01  WS-CONTROL-PRIMARY.                                          EL507
00152      05  WS-COMPANY-CD      PIC X.                                EL507
00153      05  WS-CODE            PIC X.                                EL507
00154      05  WS-TABLE           PIC X(3).                             EL507
00155                                                                   EL507
00156  01  COMP-3-WORK-AREA.                                            EL507
00157      05  K1                 PIC S9(7)  VALUE +1.                  EL507
00158      05  K2                 PIC S9(7)  VALUE +2.                  EL507
00159      05  RECORD-COUNT       PIC S9(7)  VALUE +0.                  EL507
00160      05  DELETE-COUNT       PIC S9(7)  VALUE +0.                  EL507
00161      05  DATE-ERROR-COUNT   PIC S9(7)  VALUE +0.                  EL507
00162                                                                   EL507
00163  01  WS-SAVE-PRINT-RECORD   PIC X(133) VALUE SPACES.              EL507
00164                                                                   EL507
00165      EJECT                                                        EL507
00166      COPY ELCDATE.                                                EL507
00167                                                                   EL507
00168      EJECT                                                        EL507
00169  01  WS-HEADING1.                                                 EL507
00170      05  FILLER                      PIC X(51)       VALUE '1'.   EL507
00171      05  WS-H1-TITLE                 PIC X(73)       VALUE        EL507
00172          'REINSURANCE FILE LOAD  '.                               EL507
00173      05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL507  '.    EL507
00174                                                                   EL507
00175  01  WS-HEADING2.                                                 EL507
00176      05  FILLER                      PIC X(46)       VALUE SPACES.EL507
00177      05  WS-H2-CLIENT-NAME           PIC X(78)       VALUE SPACES.EL507
00178      05  WS-H2-DATE                  PIC X(8)        VALUE SPACES.EL507
00179      05  FILLER                      PIC X           VALUE SPACES.EL507
00180                                                                   EL507
00181  01  WS-HEADING3.                                                 EL507
00182      05  FILLER                      PIC X(51)       VALUE SPACES.EL507
00183      05  WS-H3-DATE                  PIC X(60)       VALUE SPACES.EL507
00184      05  FILLER                      PIC X(5)        VALUE 'PAGE'.EL507
00185      05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL507
00186      05  FILLER                      PIC X(11)       VALUE SPACES.EL507
00187                                                                   EL507
00188  01  WS-HEADING4                     PIC X(132)      VALUE SPACES.EL507
00189                                                                   EL507
00190  01  WS-DETAIL1.                                                  EL507
00191      05  FILLER             PIC X.                                EL507
00192      05  DTL-MSG1           PIC X(18) VALUE 'SUCCESSFUL LOAD'.    EL507
00193      05  DTL-MSG2           PIC X(18) VALUE SPACE.                EL507
00194      05  DTL-MSG3           PIC X(18) VALUE SPACE.                EL507
00195      05  D-RECORD-COUNT     PIC Z,ZZZ,ZZ9.                        EL507
00196      05  FILLER             PIC X(10) VALUE ' RECORDS ('.         EL507
00197      05  D-DELETE-COUNT     PIC Z,ZZZ,ZZ9.                        EL507
00198      05  FILLER             PIC X(11) VALUE ' DELETED)'.          EL507
00199      05  FILLER             PIC X(15) VALUE '   DATE ERRORS'.     EL507
00200      05  D-DATE-ERROR-COUNT PIC Z,ZZZ,ZZ9.                        EL507
00201                                                                   EL507
00202      COPY ELCDTECX.                                               EL507
00203                                                                   EL507
00204      COPY ELCDTEVR.                                                  CL**3
00205                                                                      CL**3
00206      EJECT                                                        EL507
00207  PROCEDURE DIVISION.                                              EL507
00208                                                                   EL507
00209  0000-LOAD-DATE-WS. COPY ELCDTERX.                                   CL**2
00210                                                                   EL507
00211  000-MAINLINE.                                                    EL507
00212      PERFORM 100-INITIALIZE  THRU 100-EXIT.                       EL507
00213                                                                   EL507
00214      PERFORM 150-READ-CNTL THRU 150-EXIT.                         EL507
00215                                                                   EL507
00216      IF NO-ERRORS                                                 EL507
00217          PERFORM 200-DELETE-OLD THRU 200-EXIT.                    EL507
00218                                                                   EL507
00219      IF NO-ERRORS                                                 EL507
00220          PERFORM 300-LOAD THRU 300-EXIT                           EL507
00221                  UNTIL END-OF-VSAM-FILE.                          EL507
00222                                                                   EL507
00223      IF NO-ERRORS                                                 EL507
00224          PERFORM 170-UPDATE-CNTL THRU 170-EXIT.                   EL507
00225                                                                   EL507
00226      MOVE RECORD-COUNT     TO D-RECORD-COUNT.                     EL507
00227      MOVE DELETE-COUNT     TO D-DELETE-COUNT.                     EL507
00228      MOVE DATE-ERROR-COUNT TO D-DATE-ERROR-COUNT.                 EL507
00229      MOVE WS-DETAIL1       TO PRT.                                EL507
00230      PERFORM WRITE-A-LINE.                                        EL507
00231                                                                   EL507
00232      PERFORM 800-FINALIZE THRU 800-EXIT.                          EL507
00233                                                                   EL507
00234                                                                   EL507
00235  000-MAINLINE-EXIT.                                               EL507
00236      GOBACK.                                                      EL507
00237                                                                   EL507
00238  100-INITIALIZE.                                                  EL507
00239      MOVE ZEROS        TO WS-RETURN-CODE.                         EL507
00240      MOVE COMPANY-NAME           TO WS-H2-CLIENT-NAME.            EL507
00241      MOVE WS-CURRENT-DATE        TO WS-H2-DATE.                   EL507
00242      MOVE ALPH-DATE              TO WS-H3-DATE.                   EL507
00243                                                                   EL507
00244      OPEN INPUT  REIN-MSTR                                        EL507
00245           I-O    ERREIN                                           EL507
00246           OUTPUT PRNTR.                                           EL507
00247                                                                   EL507
00248      IF REINSURANCE-FILE-STATUS = '00' OR '97'                    EL507
00249          NEXT SENTENCE                                            EL507
00250        ELSE                                                       EL507
00251          MOVE REINSURANCE-FILE-STATUS  TO WS-ABEND-FILE-STATUS    EL507
00252          MOVE ' REIN OPEN ERROR- '      TO WS-ABEND-MESSAGE       EL507
00253          GO TO ABEND-PGM.                                         EL507
00254                                                                   EL507
00255      IF VSAMREIN-FILE-STATUS = '00' OR '97'                       EL507
00256          NEXT SENTENCE                                            EL507
00257        ELSE                                                       EL507
00258          MOVE VSAMREIN-FILE-STATUS  TO WS-ABEND-FILE-STATUS       EL507
00259          MOVE 'VSAM REIN OPEN ERROR- '      TO WS-ABEND-MESSAGE   EL507
00260          GO TO ABEND-PGM.                                         EL507
00261                                                                   EL507
00262      MOVE LOW-VALUES            TO WS-CONTROL-PRIMARY.            EL507
00263      MOVE DTE-CLASIC-COMPANY-CD TO WS-COMPANY-CD.                 EL507
00264      MOVE DTE-CLIENT            TO WS-PROCESSOR.                  EL507
00265                                                                   EL507
00266      MOVE WS-CURRENT-DATE       TO DC-GREG-DATE-1-EDIT.           EL507
00267      MOVE '2'                   TO DC-OPTION-CODE.                EL507
00268      PERFORM 310-DATE-RTN THRU 310-EXIT.                          EL507
00269      MOVE DC-BIN-DATE-1         TO WS-CURRENT-BIN-DT.             EL507
00270                                                                   EL507
00271  100-EXIT.                                                        EL507
00272      EXIT.                                                        EL507
00273      EJECT                                                        EL507
00274  150-READ-CNTL.                                                   EL507
00275      OPEN INPUT ELCNTL.                                           EL507
00276                                                                   EL507
00277      IF CONTROL-FILE-STATUS = '00' OR '97'                        EL507
00278          NEXT SENTENCE                                            EL507
00279        ELSE                                                       EL507
00280          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL507
00281          MOVE ' CNTL OPEN ERROR- ' TO WS-ABEND-MESSAGE            EL507
00282          GO TO ABEND-PGM.                                         EL507
00283                                                                   EL507
00284      MOVE +1                     TO WS-CLCNTL-STATUS-SW.          EL507
00285                                                                   EL507
00286      MOVE SPACES                 TO CF-ACCESS-CD-GENL.            EL507
00287      MOVE DTE-CLIENT             TO CF-COMPANY-ID.                EL507
00288      MOVE '1'                    TO CF-RECORD-TYPE.               EL507
00289      MOVE ZEROS                  TO CF-SEQUENCE-NO.               EL507
00290                                                                   EL507
00291      READ ELCNTL.                                                 EL507
00292                                                                   EL507
00293      IF CONTROL-FILE-STATUS NOT = ZERO                            EL507
00294          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL507
00295          MOVE ' CNTL READ ERROR- ' TO WS-ABEND-MESSAGE            EL507
00296          GO TO ABEND-PGM.                                         EL507
00297                                                                   EL507
00298      IF EP-SW = '1'                                               EL507
00299          IF CF-REINSURANCE-TAB-CREATE-DT LESS THAN                EL507
00300             CF-REINSURANCE-TAB-MAINT-DT                           EL507
00301              MOVE 'E' TO ERROR-SW                                 EL507
00302              MOVE '- ABORT LOAD     -' TO DTL-MSG1                EL507
00303              MOVE '- MAINT APPLIED  -' TO DTL-MSG3                EL507
00304              DISPLAY '*EL507*  LOAD ABORTED FOR - ' DTE-CLIENT    EL507
00305                  UPON CONSOLE                                     EL507
00306              GO TO 150-EXIT                                       EL507
00307          ELSE                                                     EL507
00308              NEXT SENTENCE                                        EL507
00309      ELSE                                                         EL507
00310          MOVE 'E' TO ERROR-SW                                     EL507
00311          MOVE '- ABORT LOAD     -' TO DTL-MSG1                    EL507
00312          MOVE '- MAINT APPLIED  -' TO DTL-MSG3                    EL507
00313          DISPLAY '*EL507*  LOAD ABORTED FOR - ' DTE-CLIENT        EL507
00314              UPON CONSOLE                                         EL507
00315          GO TO 150-EXIT.                                          EL507
00316                                                                   EL507
00317      CLOSE ELCNTL.                                                EL507
00318                                                                   EL507
00319      IF CONTROL-FILE-STATUS NOT = ZERO                            EL507
00320          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL507
00321          MOVE ' CNTL CLOSE ERROR- ' TO WS-ABEND-MESSAGE           EL507
00322          GO TO ABEND-PGM.                                         EL507
00323                                                                   EL507
00324      MOVE +0                     TO WS-CLCNTL-STATUS-SW.          EL507
00325                                                                   EL507
00326  150-EXIT.                                                        EL507
00327      EXIT.                                                        EL507
00328                                                                   EL507
00329  170-UPDATE-CNTL.                                                 EL507
00330      OPEN I-O ELCNTL.                                             EL507
00331                                                                   EL507
00332      IF CONTROL-FILE-STATUS = '00' OR '97'                        EL507
00333          NEXT SENTENCE                                            EL507
00334        ELSE                                                       EL507
00335          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL507
00336          MOVE ' CNTL OPEN ERROR- ' TO WS-ABEND-MESSAGE            EL507
00337          GO TO ABEND-PGM.                                         EL507
00338                                                                   EL507
00339      MOVE +1                     TO WS-CLCNTL-STATUS-SW.          EL507
00340                                                                   EL507
00341      MOVE SPACES                 TO CF-ACCESS-CD-GENL.            EL507
00342      MOVE DTE-CLIENT             TO CF-COMPANY-ID.                EL507
00343      MOVE '1'                    TO CF-RECORD-TYPE.               EL507
00344      MOVE ZEROS                  TO CF-SEQUENCE-NO.               EL507
00345                                                                   EL507
00346      READ ELCNTL.                                                 EL507
00347                                                                   EL507
00348      IF CONTROL-FILE-STATUS NOT = ZERO                            EL507
00349          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL507
00350          MOVE ' CNTL READ ERROR- ' TO WS-ABEND-MESSAGE            EL507
00351          GO TO ABEND-PGM.                                         EL507
00352                                                                   EL507
00353      MOVE WS-CURRENT-BIN-DT TO CF-REINSURANCE-TAB-MAINT-DT        EL507
00354                                CF-REINSURANCE-TAB-CREATE-DT.      EL507
00355                                                                   EL507
00356      REWRITE CONTROL-FILE.                                        EL507
00357                                                                   EL507
00358      IF CONTROL-FILE-STATUS NOT = ZERO                            EL507
00359          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL507
00360          MOVE ' CNTL REWRITE ERROR- ' TO WS-ABEND-MESSAGE         EL507
00361          GO TO ABEND-PGM.                                         EL507
00362                                                                   EL507
00363      CLOSE ELCNTL.                                                EL507
00364                                                                   EL507
00365      IF CONTROL-FILE-STATUS NOT = ZERO                            EL507
00366          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL507
00367          MOVE ' CNTL CLOSE ERROR- ' TO WS-ABEND-MESSAGE           EL507
00368          GO TO ABEND-PGM.                                         EL507
00369                                                                   EL507
00370      MOVE +0                     TO WS-CLCNTL-STATUS-SW.          EL507
00371                                                                   EL507
00372  170-EXIT.                                                        EL507
00373      EXIT.                                                        EL507
00374                                                                   EL507
00375  200-DELETE-OLD.                                                  EL507
00376      MOVE WS-CONTROL-PRIMARY  TO RE-CONTROL-PRIMARY.              EL507
00377                                                                   EL507
00378  205-START.                                                       EL507
00379      START ERREIN KEY NOT LESS RE-CONTROL-PRIMARY.                EL507
00380                                                                   EL507
00381      IF REINSURANCE-FILE-STATUS  = '10' OR '23'                   EL507
00382           GO TO 200-EXIT.                                         EL507
00383                                                                   EL507
00384      IF REINSURANCE-FILE-STATUS NOT = ZERO                        EL507
00385          MOVE REINSURANCE-FILE-STATUS  TO WS-ABEND-FILE-STATUS    EL507
00386          MOVE ' REIN START ERROR- '     TO WS-ABEND-MESSAGE       EL507
00387          GO TO ABEND-PGM.                                         EL507
00388                                                                   EL507
00389  210-READNEXT.                                                    EL507
00390      READ ERREIN  NEXT RECORD.                                    EL507
00391                                                                   EL507
00392      IF REN1 = '1'                                                EL507
00393              GO TO 200-EXIT.                                      EL507
00394                                                                   EL507
00395      IF REINSURANCE-FILE-STATUS NOT = ZEROS                       EL507
00396          MOVE ' -ERROR ON READ- '      TO WS-ABEND-MESSAGE        EL507
00397          MOVE REINSURANCE-FILE-STATUS TO WS-ABEND-FILE-STATUS     EL507
00398          GO TO ABEND-PGM.                                         EL507
00399                                                                   EL507
00400      IF DTE-CLASIC-COMPANY-CD NOT = RE-COMPANY-CD                 EL507
00401          GO TO 200-EXIT.                                          EL507
00402                                                                   EL507
00403      DELETE  ERREIN  RECORD.                                      EL507
00404                                                                   EL507
00405      PERFORM 230-ERROR-CHECK  THRU 230-EXIT.                      EL507
00406                                                                   EL507
00407      IF ERROR-OCCURRED                                            EL507
00408          MOVE ' -ERR ON DELETE- ' TO DTL-MSG3                     EL507
00409          GO TO 200-EXIT.                                          EL507
00410                                                                   EL507
00411      ADD 1  TO DELETE-COUNT.                                      EL507
00412                                                                   EL507
00413      GO TO 210-READNEXT.                                          EL507
00414                                                                   EL507
00415  200-EXIT.                                                        EL507
00416      EXIT.                                                        EL507
00417                                                                   EL507
00418  230-ERROR-CHECK.                                                 EL507
00419      IF REN1 NOT = '0'                                            EL507
00420          MOVE   REN  TO DTL-MSG2                                  EL507
00421          MOVE   'E'  TO ERROR-SW.                                 EL507
00422                                                                   EL507
00423  230-EXIT.                                                        EL507
00424      EXIT.                                                        EL507
00425                                                                   EL507
00426      EJECT                                                        EL507
00427  300-LOAD.                                                        EL507
00428      READ REIN-MSTR                                               EL507
00429             IF VSAMREIN-FILE-STATUS = '23' OR '10'                EL507
00430                 MOVE 'E' TO VSAM-EOF-SW                           EL507
00431                 GO TO 300-EXIT.                                   EL507
00432                                                                   EL507
00433      IF VSAMREIN-FILE-STATUS NOT = ZEROS                          EL507
00434          MOVE ' -ERROR ON VSAM READ- '      TO WS-ABEND-MESSAGE   EL507
00435          MOVE VSAMREIN-FILE-STATUS TO WS-ABEND-FILE-STATUS        EL507
00436          GO TO ABEND-PGM.                                         EL507
00437                                                                   EL507
00438      MOVE REIN-MSTR-REC          TO REINSURANCE-RECORD.           EL507
00439      MOVE DTE-CLASIC-COMPANY-CD  TO RE-COMPANY-CD.                EL507
00440                                                                   EL507
00441      MOVE WS-CURRENT-BIN-DT      TO RE-LAST-MAINT-DT.             EL507
00442      MOVE WS-PROCESSOR           TO RE-LAST-MAINT-USER.           EL507
00443      ACCEPT WS-TIME-OF-DAY   FROM TIME.                           EL507
00444      MOVE WS-TIME                TO RE-LAST-MAINT-HHMMSS.         EL507
00445                                                                   EL507
00446      IF RE-CODE = 'B'                                             EL507
00447         IF RE-LF-IBNR-PCT NOT NUMERIC                             EL507
00448            MOVE +0 TO RE-LF-IBNR-PCT.                             EL507
00449                                                                   EL507
00450      IF RE-CODE = 'B'                                             EL507
00451         IF RE-AH-IBNR-PCT NOT NUMERIC                             EL507
00452            MOVE +0 TO RE-AH-IBNR-PCT.                             EL507
00453                                                                   EL507
00454      WRITE REINSURANCE-RECORD.                                    EL507
00455                                                                   EL507
00456      PERFORM 230-ERROR-CHECK  THRU 230-EXIT.                      EL507
00457                                                                   EL507
00458      IF ERROR-OCCURRED                                            EL507
00459          MOVE ' -ERR ON WRITE- ' TO DTL-MSG3                      EL507
00460          GO TO 300-EXIT.                                          EL507
00461                                                                   EL507
00462      ADD  1 TO RECORD-COUNT.                                      EL507
00463                                                                   EL507
00464  300-EXIT.                                                        EL507
00465      EXIT.                                                        EL507
00466                                                                   EL507
00467  310-DATE-RTN.                                                    EL507
00468      CALL 'ELDATCX'  USING DATE-CONVERSION-DATA.                  EL507
00469                                                                   EL507
00470      IF DC-ERROR-CODE NOT = SPACE                                 EL507
00471          MOVE  ZEROS TO DC-BIN-DATE-1                             EL507
00472          ADD 1  TO DATE-ERROR-COUNT.                              EL507
00473                                                                   EL507
00474  310-EXIT.                                                        EL507
00475      EXIT.                                                        EL507
00476                                                                   EL507
00477      EJECT                                                        EL507
00478                                                                   EL507
00479  800-FINALIZE.                                                    EL507
00480      CLOSE REIN-MSTR                                              EL507
00481            PRNTR                                                  EL507
00482            ERREIN.                                                EL507
00483                                                                   EL507
00484      IF REINSURANCE-FILE-STATUS NOT = ZERO                        EL507
00485          MOVE REINSURANCE-FILE-STATUS  TO WS-ABEND-FILE-STATUS    EL507
00486          MOVE ' RTBL CLOSE ERROR- ' TO WS-ABEND-MESSAGE           EL507
00487          GO TO ABEND-PGM.                                         EL507
00488                                                                   EL507
00489      IF ELCNTL-OPEN                                               EL507
00490          CLOSE ELCNTL                                             EL507
00491          IF CONTROL-FILE-STATUS NOT = ZERO                        EL507
00492              MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS    EL507
00493              MOVE ' CNTL CLOSE ERROR- ' TO WS-ABEND-MESSAGE       EL507
00494              GO TO ABEND-PGM.                                     EL507
00495                                                                   EL507
00496  800-CLOSE-OTHER. COPY ELCPRTCX.                                  EL507
00497                                                                   EL507
00498  800-EXIT.                                                        EL507
00499      EXIT.                                                        EL507
00500      EJECT                                                        EL507
00501  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL507
00502                                                                   EL507
00503  WRITE-HEADINGS SECTION.                                          EL507
00504 ***************************************************************** EL507
00505 *                                                               * EL507
00506 *                            ELCWHS1.                           * EL507
00507 *                            VMOD=2.001                         * EL507
00508 *                                                               * EL507
00509 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          * EL507
00510 *****************************************************************.EL507
00511  WHS-010.                                                         EL507
00512      ADD +1  TO  WS-PAGE.                                         EL507
00513      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL507
00514      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL507
00515      MOVE ZERO                   TO  WS-LINE-COUNT.               EL507
00516                                                                   EL507
00517      MOVE WS-HEADING1            TO  PRT.                         EL507
00518      MOVE '1'                    TO  X.                           EL507
00519      PERFORM WRITE-PRINTER.                                       EL507
00520                                                                   EL507
00521      MOVE WS-HEADING2            TO  PRT.                         EL507
00522      MOVE ' '                    TO  X.                           EL507
00523      PERFORM WRITE-PRINTER.                                       EL507
00524                                                                   EL507
00525      MOVE WS-HEADING3            TO  PRT.                         EL507
00526      MOVE ' '                    TO  X.                           EL507
00527      PERFORM WRITE-PRINTER.                                       EL507
00528                                                                   EL507
00529      MOVE WS-HEADING4            TO  PRT.                         EL507
00530      MOVE ' '                    TO  X.                           EL507
00531      PERFORM WRITE-PRINTER.                                       EL507
00532                                                                   EL507
00533      MOVE +4 TO WS-LINE-COUNT.                                    EL507
00534                                                                   EL507
00535  WHS-020. COPY ELCWHS2.                                           EL507
00536                                                                   EL507
00537  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL507
00538  WPS-020. COPY ELCPRT2X.                                          EL507
00539                                                                   EL507
00540  ABEND-PGM  SECTION.  COPY ELCABEND.                                 CL**2
