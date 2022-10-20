00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL508
00003  PROGRAM-ID.                 EL508 .                                 LV004
00004 *              PROGRAM CONVERTED BY                               EL508
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL508
00006 *              CONVERSION DATE 02/19/96 16:47:17.                 EL508
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL508
00008 *                            VMOD=2.003                           EL508
00009                                                                   EL508
00009                                                                   EL508
00010 *AUTHOR.     LOGIC INC.                                           EL508
00011 *            DALLAS, TEXAS.                                       EL508
00012                                                                   EL508
00013 *DATE-COMPILED.                                                   EL508
00014                                                                   EL508
00015 *SECURITY.   *****************************************************EL508
00016 *            *                                                   *EL508
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL508
00018 *            *                                                   *EL508
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL508
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL508
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL508
00022 *            *                                                   *EL508
00023 *            *****************************************************EL508
00024                                                                   EL508
00025 *REMARKS.                                                         EL508
00026 *      THIS PROGRAM IS USED TO UNLOAD THE ONLINE REINSURANCE      EL508
00027 *      MASTER TO A VSAM FILE. THE CONTROL FILE WILL BE            EL508
00028 *      UPDATED TO REFLECT THE DATE THAT THE FILE IS CREATED.      EL508
00029 *      THE FILE WILL NOT BE CREATED IF NO MAINTENANCE HAS BEEN    EL508
00030 *      APPLIED.                                                   EL508
00031                                                                   EL508
00032  ENVIRONMENT DIVISION.                                            EL508
00033                                                                   EL508
00034  INPUT-OUTPUT SECTION.                                            EL508
00035                                                                   EL508
00036  FILE-CONTROL.                                                    EL508
00037                                                                   EL508
00038      SELECT PRNTR          ASSIGN TO SYS008-UR-1403-S-SYS008.     EL508
00039                                                                   EL508
00040      SELECT FICH           ASSIGN TO SYS020-UT-2400-S-SYS020.     EL508
00041                                                                   EL508
00042      SELECT DISK-DATE      ASSIGN TO SYS019-FBA1-S-SYS019.        EL508
00043                                                                   EL508
pemuni     SELECT REIN-MSTR      ASSIGN TO ERRTBLT                      EL508
00045              ORGANIZATION IS INDEXED                              EL508
00046              ACCESS IS SEQUENTIAL                                 EL508
00047              RECORD KEY IS RT-KEY                                 EL508
00048              FILE STATUS IS REIN-FILE-STATUS.                     EL508
00049                                                                   EL508
00050      SELECT ERREIN         ASSIGN TO SYS011-FBA1-ERREIN           EL508
00051              ORGANIZATION IS INDEXED                              EL508
00052              ACCESS IS DYNAMIC                                    EL508
00053              RECORD KEY IS RE-CONTROL-PRIMARY                     EL508
00054              FILE STATUS IS REINSURANCE-FILE-STATUS.              EL508
00055                                                                   EL508
00056      SELECT ELCNTL         ASSIGN TO SYS012-FBA1-ELCNTL           EL508
00057              ORGANIZATION IS INDEXED                              EL508
00058              ACCESS IS DYNAMIC                                    EL508
00059              RECORD KEY IS CF-CONTROL-PRIMARY                     EL508
00060              FILE STATUS IS CONTROL-FILE-STATUS.                  EL508
00061                                                                   EL508
00062      SELECT ELREPT         ASSIGN TO SYS018-FBA1-ELREPT           EL508
00063              ORGANIZATION IS INDEXED                              EL508
00064              ACCESS IS DYNAMIC                                    EL508
00065              RECORD KEY IS RF-CONTROL-PRIMARY                     EL508
00066              FILE STATUS IS DTE-VSAM-FLAGS.                       EL508
00067                                                                   EL508
00068      EJECT                                                        EL508
00069  DATA DIVISION.                                                   EL508
00070                                                                   EL508
00071  FILE SECTION.                                                    EL508
00072                                                                   EL508
00073  FD  PRNTR COPY ELCPRTFD SUPPRESS.                                EL508
00074                                                                   EL508
00075  FD  FICH COPY ELCFCHFD SUPPRESS.                                 EL508
00076                                                                   EL508
00077  FD  DISK-DATE COPY ELCDTEFD SUPPRESS.                            EL508
00078                                                                   EL508
00079  FD  ELREPT    COPY ELCRPTFD SUPPRESS.                            EL508
00080                                                                   EL508
00081      COPY ELCREPT.                                                EL508
00082      EJECT                                                        EL508
00083  FD  REIN-MSTR.                                                   EL508
00084  01  REIN-MSTR-REC.                                               EL508
00085      12  FILLER            PIC XX.                                EL508
00086      12  RT-KEY            PIC X(8).                              EL508
00087      12  FILLER            PIC X(3990).                           EL508
00088      EJECT                                                        EL508
00089  FD  ERREIN.                                                      EL508
00090      COPY ERCREIN.                                                EL508
00091                                                                   EL508
00092      EJECT                                                        EL508
00093  FD  ELCNTL.                                                      EL508
00094      COPY ELCCNTL.                                                EL508
00095                                                                   EL508
00096      EJECT                                                        EL508
00097  WORKING-STORAGE SECTION.                                         EL508
00098  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL508
00099                                                                   EL508
00100  77  FILLER  PIC X(32) VALUE '********************************'.  EL508
00101  77  FILLER  PIC X(32) VALUE '      EL508 WORKING-STORAGE     '.  EL508
00102  77  FILLER  PIC X(32) VALUE '***********VMOD=2.003 **********'.  EL508
00103                                                                   EL508
00104  01  WORK-AREAS.                                                  EL508
00105      12  INDX                   PIC S9(3)  VALUE +0  COMP-3.      EL508
00106      12  WS-LINE-COUNT          PIC S9(3)  VALUE +99 COMP-3.      EL508
00107      12  WS-LINE-COUNT-MAX      PIC S9(3)  VALUE +60 COMP-3.      EL508
00108      12  WS-PAGE                PIC S9(3)  VALUE +0  COMP-3.      EL508
00109      12  WS-CURRENT-BIN-DT      PIC XX.                           EL508
00110      12  WS-RETURN-CODE         PIC S9(4) COMP.                   EL508
00111      12  WS-ABEND-MESSAGE       PIC X(80).                        EL508
00112      12  WS-ABEND-FILE-STATUS   PIC XX  VALUE ZEROS.              EL508
00113      12  WS-ZERO                PIC S9  VALUE ZERO COMP-3.        EL508
00114      12  REINSURANCE-OPEN           PIC X   VALUE SPACES.         EL508
00115      12  REINSURANCE-FILE-STATUS    PIC XX  VALUE ZEROS.          EL508
00116      12  REN REDEFINES REINSURANCE-FILE-STATUS.                   EL508
00117          16  REN1               PIC X.                            EL508
00118          16  REN2               PIC X.                            EL508
00119      12  CONTROL-FILE-STATUS    PIC XX  VALUE ZEROS.              EL508
00120      12  REIN-FILE-STATUS       PIC XX  VALUE ZEROS.              EL508
00121                                                                   EL508
00122      12  ERROR-SW               PIC X    VALUE SPACE.             EL508
00123          88  ERROR-OCCURRED              VALUE 'E'.               EL508
00124          88  NO-ERRORS                   VALUE ' '.               EL508
00125      12  WS-SAVE-PRINT-RECORD   PIC X(133) VALUE SPACES.          EL508
00126                                                                   EL508
00127  01  DTE-INTERFACE-CODES.                                         EL508
00128      05  X                 PIC X           VALUE SPACE.           EL508
00129      05  PGM-SUB           PIC S9(4)  COMP VALUE +508.            EL508
00130      05  ABEND-CODE        PIC 9999        VALUE ZERO.            EL508
00131      05  ABEND-OPTION      PIC X           VALUE SPACE.           EL508
00132      05  OLC-REPORT-NAME   PIC X(6)        VALUE 'EL508'.         EL508
00133                                                                   EL508
00134      05 WS-IN                   PIC 9(5) VALUE ZEROS.             EL508
00135      05 WS-OUT                  PIC 9(5) VALUE ZEROS.             EL508
00136                                                                   EL508
00137      05  WS-CONTROL-PRIMARY.                                      EL508
00138          10  WS-COMPANY-CD      PIC X.                            EL508
00139          10  WS-CODE            PIC X.                            EL508
00140          10  WS-TABLE           PIC X(3).                         EL508
00141                                                                   EL508
00142     05 END-OF-PROCESS-SWT        PIC X VALUE 'N'.                 EL508
00143        88 END-OF-PROCESS         VALUE 'Y'.                       EL508
00144        88 NOT-END-OF-PROCESS     VALUE 'N'.                       EL508
00145                                                                   EL508
00146  01  COMP-3-WORK-AREA.                                            EL508
00147      05  K1                 PIC S9(7)  VALUE +1.                  EL508
00148      05  K2                 PIC S9(7)  VALUE +2.                  EL508
00149      05  RECORD-COUNT       PIC S9(7)  VALUE +0.                  EL508
00150      05  DELETE-COUNT       PIC S9(7)  VALUE +0.                  EL508
00151      05  DATE-ERROR-COUNT   PIC S9(7)  VALUE +0.                  EL508
00152                                                                   EL508
00153      EJECT                                                        EL508
00154      COPY ELCDATE.                                                   CL**4
00155                                                                   EL508
00156      EJECT                                                        EL508
00157                                                                   EL508
00158  01  WS-HEADING1.                                                 EL508
00159      05  FILLER                      PIC X(51)       VALUE '1'.   EL508
00160      05  WS-H1-TITLE                 PIC X(73)       VALUE        EL508
00161          'REINSURANCE FILE UNLOAD'.                               EL508
00162      05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL508  '.    EL508
00163                                                                   EL508
00164  01  WS-HEADING2.                                                 EL508
00165      05  FILLER                      PIC X(46)       VALUE SPACES.EL508
00166      05  WS-H2-CLIENT-NAME           PIC X(78)       VALUE SPACES.EL508
00167      05  WS-H2-DATE                  PIC X(8)        VALUE SPACES.EL508
00168      05  FILLER                      PIC X           VALUE SPACES.EL508
00169                                                                   EL508
00170  01  WS-HEADING3.                                                 EL508
00171      05  FILLER                      PIC X(51)       VALUE SPACES.EL508
00172      05  WS-H3-DATE                  PIC X(60)       VALUE SPACES.EL508
00173      05  FILLER                      PIC X(5)        VALUE 'PAGE'.EL508
00174      05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL508
00175      05  FILLER                      PIC X(11)       VALUE SPACES.EL508
00176                                                                   EL508
00177  01  WS-HEADING4                     PIC X(132)      VALUE SPACES.EL508
00178                                                                   EL508
00179  01  WS-DETAIL1.                                                  EL508
00180      05  FILLER             PIC X.                                EL508
00181      05  DTL-MSG1           PIC X(18) VALUE 'SUCCESSFUL UNLOAD '. EL508
00182      05  DTL-MSG2           PIC X(18) VALUE SPACE.                EL508
00183      05  DTL-MSG3           PIC X(18) VALUE SPACE.                EL508
00184      05  D-RECORD-COUNT     PIC Z,ZZZ,ZZ9.                        EL508
00185      05  FILLER             PIC X(10) VALUE ' RECORDS  '.         EL508
00186                                                                   EL508
00187      COPY ELCDTECX.                                               EL508
00188                                                                   EL508
00189      COPY ELCDTEVR.                                               EL508
00190                                                                   EL508
00191      COPY ELCREINV.                                               EL508
00192                                                                   EL508
00193      EJECT                                                        EL508
00194  PROCEDURE DIVISION.                                              EL508
00195                                                                   EL508
00196  0000-LOAD-DATE-WS. COPY ELCDTERX SUPPRESS.                       EL508
00197                                                                   EL508
00198  000-MAINLINE.                                                    EL508
00199      PERFORM 100-INITIALIZE  THRU 100-EXIT.                       EL508
00200                                                                   EL508
00201      PERFORM 150-READ-CNTL THRU 150-EXIT.                         EL508
00202                                                                   EL508
00203      IF NO-ERRORS                                                 EL508
00204          PERFORM 300-UNLOAD-REINSURANCE THRU 300-EXIT.            EL508
00205                                                                   EL508
00206      MOVE RECORD-COUNT           TO D-RECORD-COUNT.               EL508
00207      MOVE WS-DETAIL1             TO PRT.                          EL508
00208      PERFORM WRITE-A-LINE.                                        EL508
00209                                                                   EL508
00210      PERFORM 800-FINALIZE THRU 800-EXIT.                          EL508
00211                                                                   EL508
00212  000-MAINLINE-EXIT.                                               EL508
00213      GOBACK.                                                      EL508
00214                                                                   EL508
00215      EJECT                                                        EL508
00216                                                                   EL508
00217  100-INITIALIZE.                                                  EL508
00218      MOVE ZEROS                  TO WS-RETURN-CODE.               EL508
00219      MOVE COMPANY-NAME           TO WS-H2-CLIENT-NAME.            EL508
00220      MOVE WS-CURRENT-DATE        TO WS-H2-DATE.                   EL508
00221      MOVE ALPH-DATE              TO WS-H3-DATE.                   EL508
00222                                                                   EL508
00223      OPEN INPUT  ERREIN                                           EL508
00224           OUTPUT PRNTR.                                           EL508
00225                                                                   EL508
00226      IF EP-SW = '1'                                               EL508
00227          OPEN I-O ELCNTL                                          EL508
00228      ELSE                                                         EL508
00229          OPEN INPUT ELCNTL.                                       EL508
00230                                                                   EL508
00231      IF CONTROL-FILE-STATUS = '00' OR '97'                        EL508
00232          NEXT SENTENCE                                            EL508
00233        ELSE                                                       EL508
00234          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL508
00235          MOVE ' CNTL OPEN ERROR- ' TO WS-ABEND-MESSAGE            EL508
00236          PERFORM ABEND-PGM.                                       EL508
00237                                                                   EL508
00238      IF REINSURANCE-FILE-STATUS = '00' OR '97'                    EL508
00239          NEXT SENTENCE                                            EL508
00240        ELSE                                                       EL508
00241          MOVE REINSURANCE-FILE-STATUS  TO WS-ABEND-FILE-STATUS    EL508
00242          MOVE ' REIN OPEN ERROR- ' TO WS-ABEND-MESSAGE            EL508
00243          PERFORM ABEND-PGM.                                       EL508
00244                                                                   EL508
00245      MOVE LOW-VALUES             TO WS-CONTROL-PRIMARY.           EL508
00246      MOVE DTE-CLASIC-COMPANY-CD  TO WS-COMPANY-CD.                EL508
00247                                                                   EL508
00248      MOVE WS-CURRENT-DATE        TO DC-GREG-DATE-1-EDIT.          EL508
00249      MOVE '2'                    TO DC-OPTION-CODE.               EL508
00250      PERFORM 310-DATE-RTN THRU 310-EXIT.                          EL508
00251      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.            EL508
00252                                                                   EL508
00253  100-EXIT.                                                        EL508
00254      EXIT.                                                        EL508
00255      EJECT                                                        EL508
00256                                                                   EL508
00257  150-READ-CNTL.                                                   EL508
00258      MOVE SPACES                 TO CF-ACCESS-CD-GENL.            EL508
00259      MOVE DTE-CLIENT             TO CF-COMPANY-ID                 EL508
00260      MOVE '1'                    TO CF-RECORD-TYPE                EL508
00261      MOVE ZEROS                  TO CF-SEQUENCE-NO.               EL508
00262                                                                   EL508
00263      READ ELCNTL                                                  EL508
00264                                                                   EL508
00265      IF CONTROL-FILE-STATUS NOT = ZERO                            EL508
00266          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL508
00267          MOVE ' CNTL READ ERROR- ' TO WS-ABEND-MESSAGE            EL508
00268          PERFORM ABEND-PGM.                                       EL508
00269                                                                   EL508
00270      IF EP-SW = '1'                                               EL508
00271          MOVE WS-CURRENT-BIN-DT  TO CF-REINSURANCE-TAB-CREATE-DT  EL508
00272 *        MOVE LOW-VALUES         TO CF-REINSURANCE-TAB-MAINT-DT   EL508
00273      ELSE                                                         EL508
00274          GO TO 150-EXIT.                                          EL508
00275                                                                   EL508
00276      REWRITE CONTROL-FILE.                                        EL508
00277                                                                   EL508
00278      IF CONTROL-FILE-STATUS NOT = ZERO                            EL508
00279          MOVE CONTROL-FILE-STATUS     TO WS-ABEND-FILE-STATUS     EL508
00280          MOVE ' CNTL REWRITE ERROR- ' TO WS-ABEND-MESSAGE         EL508
00281          PERFORM ABEND-PGM.                                       EL508
00282                                                                   EL508
00283  150-EXIT.                                                        EL508
00284      EXIT.                                                        EL508
00285                                                                   EL508
00286      EJECT                                                        EL508
00287  300-UNLOAD-REINSURANCE.                                          EL508
00288      OPEN OUTPUT REIN-MSTR.                                       EL508
00289                                                                   EL508
00290      IF REIN-FILE-STATUS    = '00' OR '97'                        EL508
00291          NEXT SENTENCE                                            EL508
00292        ELSE                                                       EL508
00293          MOVE REIN-FILE-STATUS                                    EL508
00294                                  TO WS-ABEND-FILE-STATUS          EL508
00295          MOVE ' REIN OUTPUT OPEN ERROR- '                         EL508
00296                                  TO WS-ABEND-MESSAGE              EL508
00297          PERFORM ABEND-PGM.                                       EL508
00298                                                                   EL508
00299      MOVE 'Y'                    TO REINSURANCE-OPEN.             EL508
00300      MOVE WS-CONTROL-PRIMARY     TO RE-CONTROL-PRIMARY.           EL508
00301                                                                   EL508
00302  305-START.                                                       EL508
00303      START ERREIN KEY NOT LESS RE-CONTROL-PRIMARY.                EL508
00304                                                                   EL508
00305      IF REINSURANCE-FILE-STATUS  = '10' OR '23'                   EL508
00306           GO TO 300-EXIT.                                         EL508
00307                                                                   EL508
00308      IF REINSURANCE-FILE-STATUS NOT = ZERO                        EL508
00309          MOVE REINSURANCE-FILE-STATUS  TO WS-ABEND-FILE-STATUS    EL508
00310          MOVE ' REIN START ERROR- '    TO WS-ABEND-MESSAGE        EL508
00311          PERFORM ABEND-PGM.                                       EL508
00312                                                                   EL508
00313  310-READNEXT.                                                    EL508
00314      READ ERREIN  NEXT RECORD.                                    EL508
00315                                                                   EL508
00316      IF RE-CODE NOT = 'A'                                            CL**3
00317         PERFORM DATE-VARIABLES-LOAD.                                 CL**3
00318                                                                   EL508
00319      IF REN1 = '1'                                                EL508
00320          GO TO 300-EXIT.                                          EL508
00321                                                                   EL508
00322      IF REINSURANCE-FILE-STATUS NOT = ZEROS                       EL508
00323          MOVE 'ERROR ON READ  '       TO WS-ABEND-MESSAGE         EL508
00324          MOVE REINSURANCE-FILE-STATUS TO WS-ABEND-FILE-STATUS     EL508
00325          PERFORM ABEND-PGM.                                       EL508
00326                                                                   EL508
00327      IF DTE-CLASIC-COMPANY-CD NOT = RE-COMPANY-CD                 EL508
00328          GO TO 300-EXIT.                                          EL508
00329                                                                   EL508
00330      IF RE-TABLE-RECORD                                           EL508
00331          MOVE ZERO               TO INDX                          EL508
00332          PERFORM 320-FIX-REIN-COMPANY THRU 320-EXIT  20 TIMES.    EL508
00333                                                                   EL508
00334      IF RE-CODE NOT = 'A'                                            CL**3
00335         PERFORM DATE-VARIABLES-RELOAD.                               CL**3
00336                                                                   EL508
00337      MOVE REINSURANCE-RECORD     TO REIN-MSTR-REC.                EL508
00338                                                                   EL508
00339      WRITE REIN-MSTR-REC.                                         EL508
00340                                                                   EL508
00341      IF REIN-FILE-STATUS NOT = '00'                               EL508
00342          MOVE REIN-FILE-STATUS                                    EL508
00343                                  TO WS-ABEND-FILE-STATUS          EL508
00344          MOVE ' REIN OUTPUT WRITE ERROR- '                        EL508
00345                                  TO WS-ABEND-MESSAGE              EL508
00346          PERFORM ABEND-PGM.                                       EL508
00347                                                                   EL508
00348      ADD 1  TO RECORD-COUNT.                                      EL508
00349                                                                   EL508
00350      GO TO 310-READNEXT.                                          EL508
00351                                                                   EL508
00352  300-EXIT.                                                        EL508
00353      EXIT.                                                        EL508
00354                                                                   EL508
00355  EJECT                                                            EL508
00356                                                                   EL508
00357  310-DATE-RTN.                                                    EL508
00358      CALL 'ELDATCX'  USING DATE-CONVERSION-DATA.                  EL508
00359                                                                   EL508
00360      IF DC-ERROR-CODE NOT = SPACE                                 EL508
00361          MOVE  ZEROS             TO DC-BIN-DATE-1                 EL508
00362          ADD 1  TO DATE-ERROR-COUNT.                              EL508
00363                                                                   EL508
00364  310-EXIT.                                                        EL508
00365      EXIT.                                                        EL508
00366                                                                   EL508
00367  320-FIX-REIN-COMPANY.                                            EL508
00368      ADD +1 TO INDX                                               EL508
00369                                                                   EL508
00370      IF RE-REI-COMP (INDX) = SPACES                               EL508
00371                           MOVE SPACES TO RE-REI-COMP-NO (INDX).   EL508
00372                                                                   EL508
00373  320-EXIT.                                                        EL508
00374      EXIT.                                                        EL508
00375                                                                   EL508
00376      EJECT                                                        EL508
00377  800-FINALIZE.                                                    EL508
00378      IF REINSURANCE-OPEN = 'Y'                                    EL508
00379         CLOSE REIN-MSTR.                                          EL508
00380                                                                   EL508
00381      CLOSE PRNTR                                                  EL508
00382            ERREIN ELCNTL.                                         EL508
00383                                                                   EL508
00384  800-CLOSE-OTHER. COPY ELCPRTCX.                                  EL508
00385                                                                   EL508
00386  800-EXIT.                                                        EL508
00387      EXIT.                                                        EL508
00388      EJECT                                                        EL508
00389  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL508
00390                                                                   EL508
00391  WRITE-HEADINGS SECTION.                                          EL508
00392 ***************************************************************** EL508
00393 *                                                               * EL508
00394 *                            ELCWHS1.                           * EL508
00395 *                            VMOD=2.001                         * EL508
00396 *                                                               * EL508
00397 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          * EL508
00398 *****************************************************************.EL508
00399  WHS-010.                                                         EL508
00400      IF  WS-H2-DATE EQUAL SPACES                                  EL508
00401          MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   EL508
00402          MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME            EL508
00403          MOVE ALPH-DATE          TO  WS-H3-DATE.                  EL508
00404                                                                   EL508
00405      ADD +1  TO  WS-PAGE.                                         EL508
00406      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL508
00407      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL508
00408      MOVE ZERO                   TO  WS-LINE-COUNT.               EL508
00409                                                                   EL508
00410      MOVE WS-HEADING1            TO  PRT.                         EL508
00411      MOVE '1'                    TO  X.                           EL508
00412      PERFORM WRITE-PRINTER.                                       EL508
00413                                                                   EL508
00414      MOVE WS-HEADING2            TO  PRT.                         EL508
00415      MOVE ' '                    TO  X.                           EL508
00416      PERFORM WRITE-PRINTER.                                       EL508
00417                                                                   EL508
00418      MOVE WS-HEADING3            TO  PRT.                         EL508
00419      MOVE ' '                    TO  X.                           EL508
00420      PERFORM WRITE-PRINTER.                                       EL508
00421                                                                   EL508
00422      MOVE WS-HEADING4            TO  PRT.                         EL508
00423      MOVE ' '                    TO  X.                           EL508
00424      PERFORM WRITE-PRINTER.                                       EL508
00425                                                                   EL508
00426      MOVE +4 TO WS-LINE-COUNT.                                    EL508
00427                                                                   EL508
00428  WHS-020.      COPY ELCWHS2.                                      EL508
00429                                                                   EL508
00430  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL508
00431  WPS-020. COPY ELCPRT2X.                                          EL508
00432                                                                   EL508
00433  DATE-VARIABLES-LOAD.                                                CL**2
00434      COPY ELCRENM1.                                                  CL**2
00435                                                                      CL**2
00436  DATE-VARIABLES-RELOAD.                                              CL**2
00437      COPY ELCRENM2.                                                  CL**2
00438                                                                      CL**2
00439  ABEND-PGM  SECTION.  COPY ELCABEND SUPPRESS.                     EL508
