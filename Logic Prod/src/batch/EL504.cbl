00001  IDENTIFICATION DIVISION.                                         03/06/98
00002                                                                   EL504
00003  PROGRAM-ID.                 EL504 .                                 LV003
00004 *              PROGRAM CONVERTED BY                               EL504
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL504
00006 *              CONVERSION DATE 02/19/96 16:26:33.                 EL504
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL504
00008 *                            VMOD=2.003                              CL**3
00009                                                                   EL504
00009                                                                   EL504
00010 *AUTHOR.     LOGIC INC.                                           EL504
00011 *            DALLAS, TEXAS.                                       EL504
00012                                                                   EL504
00013 *DATE-COMPILED.                                                   EL504
00014                                                                   EL504
00015 *SECURITY.   *****************************************************EL504
00016 *            *                                                   *EL504
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL504
00018 *            *                                                   *EL504
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL504
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL504
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL504
00022 *            *                                                   *EL504
00023 *            *****************************************************EL504
00024                                                                   EL504
00025 *REMARKS.                                                         EL504
00026 *      THIS PROGRAM IS USED TO UNLOAD THE ONLINE RATE FILE        EL504
00027 *      MASTER TO A VSAM FILE. THE CONTROL FILE WILL BE            EL504
00028 *      UPDATED TO REFLECT THE DATE THAT THE FILE IS CREATED.      EL504
00029 *      THE FILE WILL NOT BE CREATED IF NO MAINTENANCE HAS BEEN    EL504
00030 *      APPLIED.                                                   EL504
00031                                                                   EL504
00032  ENVIRONMENT DIVISION.                                            EL504
00033                                                                   EL504
00034  INPUT-OUTPUT SECTION.                                            EL504
00035                                                                   EL504
00036  FILE-CONTROL.                                                    EL504
00037                                                                   EL504
00038      SELECT PRNTR          ASSIGN TO SYS008-UR-1403-S-SYS008.     EL504
00039                                                                   EL504
00040      SELECT FICH           ASSIGN TO SYS020-UT-2400-S-SYS020.     EL504
00041                                                                   EL504
00042      SELECT DISK-DATE      ASSIGN TO SYS019-FBA1-S-SYS019.        EL504
00043                                                                   EL504
pemuni     SELECT RATE-MSTR      ASSIGN TO ERRATET                      EL504
00045              ORGANIZATION IS INDEXED                              EL504
00046              ACCESS IS SEQUENTIAL                                 EL504
00047              RECORD KEY IS RT-KEY                                 EL504
00048              FILE STATUS IS VSAMRATE-FILE-STATUS.                 EL504
00049                                                                   EL504
00050      SELECT ERRATE         ASSIGN TO SYS011-FBA1-ERRATE           EL504
00051              ORGANIZATION IS INDEXED                              EL504
00052              ACCESS IS DYNAMIC                                    EL504
00053              RECORD KEY IS RT-CONTROL-PRIMARY                     EL504
00054              FILE STATUS IS RATE-FILE-STATUS.                     EL504
00055                                                                   EL504
00056      SELECT ELCNTL         ASSIGN TO SYS012-FBA1-ELCNTL           EL504
00057              ORGANIZATION IS INDEXED                              EL504
00058              ACCESS IS DYNAMIC                                    EL504
00059              RECORD KEY IS CF-CONTROL-PRIMARY                     EL504
00060              FILE STATUS IS CONTROL-FILE-STATUS.                  EL504
00061                                                                   EL504
00062      SELECT ELREPT         ASSIGN TO SYS018-FBA1-ELREPT           EL504
00063              ORGANIZATION IS INDEXED                              EL504
00064              ACCESS IS DYNAMIC                                    EL504
00065              RECORD KEY IS RF-CONTROL-PRIMARY                     EL504
00066              FILE STATUS IS DTE-VSAM-FLAGS.                       EL504
00067                                                                   EL504
00068      EJECT                                                        EL504
00069  DATA DIVISION.                                                   EL504
00070                                                                   EL504
00071  FILE SECTION.                                                    EL504
00072                                                                   EL504
00073  FD  PRNTR COPY ELCPRTFD SUPPRESS.                                EL504
00074                                                                   EL504
00075  FD  FICH COPY ELCFCHFD SUPPRESS.                                 EL504
00076                                                                   EL504
00077  FD  DISK-DATE COPY ELCDTEFD SUPPRESS.                            EL504
00078                                                                   EL504
00079  FD  ELREPT    COPY ELCRPTFD SUPPRESS.                            EL504
00080                                                                   EL504
00081      COPY ELCREPT.                                                EL504
00082      EJECT                                                        EL504
00083  FD  RATE-MSTR.                                                   EL504
00084  01  RATE-MSTR-REC.                                               EL504
00085      12  FILLER              PIC XX.                              EL504
00086      12  RT-KEY              PIC X(28).                           EL504
00087      12  FILLER              PIC X(1735).                         EL504
00088                                                                   EL504
00089      EJECT                                                        EL504
00090  FD  ERRATE.                                                      EL504
00091      COPY ERCRATE.                                                EL504
00092                                                                   EL504
00093      EJECT                                                        EL504
00094  FD  ELCNTL.                                                      EL504
00095      COPY ELCCNTL.                                                EL504
00096                                                                   EL504
00097      EJECT                                                        EL504
00098  WORKING-STORAGE SECTION.                                         EL504
00099  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL504
00100                                                                   EL504
00101  77  FILLER  PIC X(32) VALUE '********************************'.  EL504
00102  77  FILLER  PIC X(32) VALUE '      EL504 WORKING-STORAGE     '.  EL504
00103  77  FILLER  PIC X(32) VALUE '***********VMOD=2.003 **********'.     CL**3
00104                                                                   EL504
00105  01  WORK-AREAS.                                                  EL504
00106      12  WS-LINE-COUNT          PIC S9(3)  VALUE +99 COMP-3.      EL504
00107      12  WS-LINE-COUNT-MAX      PIC S9(3)  VALUE +60 COMP-3.      EL504
00108      12  WS-PAGE                PIC S9(3)  VALUE +0  COMP-3.      EL504
00109      12  WS-CURRENT-BIN-DT      PIC XX.                           EL504
00110      12  WS-RETURN-CODE         PIC S9(4) COMP.                   EL504
00111      12  WS-ABEND-MESSAGE       PIC X(80).                        EL504
00112      12  WS-ABEND-FILE-STATUS   PIC XX  VALUE ZEROS.              EL504
00113      12  WS-ZERO                PIC S9  VALUE ZERO COMP-3.        EL504
00114      12  RATE-OPEN              PIC X   VALUE SPACES.             EL504
00115      12  RATE-FILE-STATUS       PIC XX  VALUE ZEROS.              EL504
00116      12  RTE REDEFINES RATE-FILE-STATUS.                          EL504
00117          16  RTE1               PIC X.                            EL504
00118          16  RTE2               PIC X.                            EL504
00119      12  CONTROL-FILE-STATUS    PIC XX  VALUE ZEROS.              EL504
00120      12  VSAMRATE-FILE-STATUS   PIC XX  VALUE ZEROS.              EL504
00121                                                                   EL504
00122      12  ERROR-SW               PIC X    VALUE SPACE.             EL504
00123          88  ERROR-OCCURRED              VALUE 'E'.               EL504
00124          88  NO-ERRORS                   VALUE ' '.               EL504
00125      12  WS-SAVE-PRINT-RECORD   PIC X(133) VALUE SPACES.          EL504
00126                                                                   EL504
00127  01  DTE-INTERFACE-CODES.                                         EL504
00128      05  X                 PIC X           VALUE SPACE.           EL504
00129      05  PGM-SUB           PIC S9(4)  COMP VALUE +504.            EL504
00130      05  ABEND-CODE        PIC 9999        VALUE ZERO.            EL504
00131      05  ABEND-OPTION      PIC X           VALUE SPACE.           EL504
00132      05  OLC-REPORT-NAME   PIC X(6)        VALUE 'EL504'.         EL504
00133                                                                   EL504
00134      05 WS-IN              PIC 9(5) VALUE ZEROS.                  EL504
00135      05 WS-OUT             PIC 9(5) VALUE ZEROS.                  EL504
00136                                                                   EL504
00137  01  WS-CONTROL-PRIMARY.                                          EL504
00138      05  WS-COMPANY-CD      PIC X.                                EL504
00139      05  WS-STATE-CODE.                                           EL504
00140          10  WS-ST-CODE     PIC X(2).                             EL504
00141          10  WS-ST-CLASS    PIC X(2).                             EL504
00142          10  WS-ST-DEV      PIC X(3).                             EL504
00143      05  WS-LIMITS.                                               EL504
00144          10  WS-HIGH-AGE    PIC 9(2).                             EL504
00145          10  WS-HIGH-AMT    PIC 9(6).                             EL504
00146          10  WS-FUTURE      PIC X(2).                             EL504
00147          10  WS-SEX         PIC X.                                EL504
00148      05  WS-L-AH-CODE.                                            EL504
00149          10  WS-L-AH        PIC X.                                EL504
00150          10  WS-LAH-NUM     PIC 9(2).                             EL504
00151      05  WS-EXPIRY-DATE.                                          EL504
00152          10  WS-EXP-YR      PIC 9(2).                             EL504
00153          10  WS-EXP-MO      PIC 9(2).                             EL504
00154          10  WS-EXP-DA      PIC 9(2).                             EL504
00155                                                                   EL504
00156                                                                   EL504
00157     05 END-OF-PROCESS-SWT        PIC X VALUE 'N'.                 EL504
00158        88 END-OF-PROCESS         VALUE 'Y'.                       EL504
00159        88 NOT-END-OF-PROCESS     VALUE 'N'.                       EL504
00160                                                                   EL504
00161  01  COMP-3-WORK-AREA.                                            EL504
00162      05  K1                 PIC S9(7)  VALUE +1.                  EL504
00163      05  K2                 PIC S9(7)  VALUE +2.                  EL504
00164      05  RECORD-COUNT       PIC S9(7)  VALUE +0.                  EL504
00165      05  DELETE-COUNT       PIC S9(7)  VALUE +0.                  EL504
00166      05  DATE-ERROR-COUNT   PIC S9(7)  VALUE +0.                  EL504
00167                                                                   EL504
00168      EJECT                                                        EL504
00169      COPY ELCDATE.                                                EL504
00170                                                                   EL504
00171      EJECT                                                        EL504
00172                                                                   EL504
00173  01  WS-HEADING1.                                                 EL504
00174      05  FILLER                      PIC X(51)       VALUE '1'.   EL504
00175      05  WS-H1-TITLE                 PIC X(73)       VALUE        EL504
00176          'RATE FILE UNLOAD'.                                      EL504
00177      05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL504  '.    EL504
00178                                                                   EL504
00179  01  WS-HEADING2.                                                 EL504
00180      05  FILLER                      PIC X(46)       VALUE SPACES.EL504
00181      05  WS-H2-CLIENT-NAME           PIC X(78)       VALUE SPACES.EL504
00182      05  WS-H2-DATE                  PIC X(8)        VALUE SPACES.EL504
00183      05  FILLER                      PIC X           VALUE SPACES.EL504
00184                                                                   EL504
00185  01  WS-HEADING3.                                                 EL504
00186      05  FILLER                      PIC X(51)       VALUE SPACES.EL504
00187      05  WS-H3-DATE                  PIC X(60)       VALUE SPACES.EL504
00188      05  FILLER                      PIC X(5)        VALUE 'PAGE'.EL504
00189      05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL504
00190      05  FILLER                      PIC X(11)       VALUE SPACES.EL504
00191                                                                   EL504
00192  01  WS-HEADING4                     PIC X(132)      VALUE SPACES.EL504
00193                                                                   EL504
00194  01  WS-DETAIL1.                                                  EL504
00195      05  FILLER             PIC X.                                EL504
00196      05  DTL-MSG1           PIC X(18) VALUE 'SUCCESSFUL UNLOAD '. EL504
00197      05  DTL-MSG2           PIC X(18) VALUE SPACE.                EL504
00198      05  DTL-MSG3           PIC X(18) VALUE SPACE.                EL504
00199      05  D-RECORD-COUNT     PIC Z,ZZZ,ZZ9.                        EL504
00200      05  FILLER             PIC X(10) VALUE ' RECORDS  '.         EL504
00201                                                                   EL504
00202      COPY ELCDTECX.                                               EL504
00203                                                                   EL504
00204      COPY ELCDTEVR.                                               EL504
00205                                                                   EL504
00206      EJECT                                                        EL504
00207  PROCEDURE DIVISION.                                              EL504
00208                                                                   EL504
00209  0000-LOAD-DATE-WS. COPY ELCDTERX SUPPRESS.                       EL504
00210                                                                   EL504
00211  000-MAINLINE.                                                    EL504
00212      PERFORM 100-INITIALIZE  THRU 100-EXIT.                       EL504
00213                                                                   EL504
00214      PERFORM 150-READ-CNTL THRU 150-EXIT.                         EL504
00215                                                                   EL504
00216      IF NO-ERRORS                                                 EL504
00217          PERFORM 300-UNLOAD-RATE THRU 300-EXIT.                   EL504
00218                                                                   EL504
00219      MOVE RECORD-COUNT           TO D-RECORD-COUNT.               EL504
00220      MOVE WS-DETAIL1             TO PRT.                          EL504
00221      PERFORM WRITE-A-LINE.                                        EL504
00222                                                                   EL504
00223      PERFORM 800-FINALIZE THRU 800-EXIT.                          EL504
00224                                                                   EL504
00225  000-MAINLINE-EXIT.                                               EL504
00226      GOBACK.                                                      EL504
00227                                                                   EL504
00228      EJECT                                                        EL504
00229                                                                   EL504
00230  100-INITIALIZE.                                                  EL504
00231      MOVE ZEROS                  TO WS-RETURN-CODE.               EL504
00232      MOVE COMPANY-NAME           TO WS-H2-CLIENT-NAME.            EL504
00233      MOVE WS-CURRENT-DATE        TO WS-H2-DATE.                   EL504
00234      MOVE ALPH-DATE              TO WS-H3-DATE.                   EL504
00235                                                                   EL504
00236      OPEN INPUT  ERRATE                                           EL504
00237           OUTPUT PRNTR.                                           EL504
00238                                                                   EL504
00239      IF EP-SW = '1'                                               EL504
00240           OPEN I-O ELCNTL                                         EL504
00241      ELSE                                                         EL504
00242          OPEN INPUT ELCNTL.                                       EL504
00243                                                                   EL504
00244      IF CONTROL-FILE-STATUS = '00' OR '97'                        EL504
00245          NEXT SENTENCE                                            EL504
00246        ELSE                                                       EL504
00247          MOVE CONTROL-FILE-STATUS                                 EL504
00248                                  TO WS-ABEND-FILE-STATUS          EL504
00249          MOVE ' CNTL OPEN ERROR- '                                EL504
00250                                  TO WS-ABEND-MESSAGE              EL504
00251          GO TO ABEND-PGM.                                         EL504
00252                                                                   EL504
00253      IF RATE-FILE-STATUS = '00' OR '97'                           EL504
00254          NEXT SENTENCE                                            EL504
00255        ELSE                                                       EL504
00256          MOVE RATE-FILE-STATUS                                    EL504
00257                                  TO WS-ABEND-FILE-STATUS          EL504
00258          MOVE ' RATE MASTER OPEN ERROR- '                         EL504
00259                                  TO WS-ABEND-MESSAGE              EL504
00260          GO TO ABEND-PGM.                                         EL504
00261                                                                   EL504
00262      MOVE LOW-VALUES TO WS-CONTROL-PRIMARY.                       EL504
00263      MOVE DTE-CLASIC-COMPANY-CD  TO WS-COMPANY-CD.                EL504
00264                                                                   EL504
00265      MOVE WS-CURRENT-DATE        TO DC-GREG-DATE-1-EDIT.          EL504
00266      MOVE '2'                    TO DC-OPTION-CODE.               EL504
00267      PERFORM 310-DATE-RTN THRU 310-EXIT.                          EL504
00268      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.            EL504
00269                                                                   EL504
00270  100-EXIT.                                                        EL504
00271      EXIT.                                                        EL504
00272      EJECT                                                        EL504
00273  150-READ-CNTL.                                                   EL504
00274      MOVE SPACES                 TO CF-ACCESS-CD-GENL.            EL504
00275      MOVE DTE-CLIENT             TO CF-COMPANY-ID                 EL504
00276      MOVE '1'                    TO CF-RECORD-TYPE                EL504
00277      MOVE ZEROS                  TO CF-SEQUENCE-NO.               EL504
00278                                                                   EL504
00279      READ ELCNTL                                                  EL504
00280                                                                   EL504
00281      IF CONTROL-FILE-STATUS NOT = ZERO                            EL504
00282          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL504
00283          MOVE ' CNTL READ ERROR- ' TO WS-ABEND-MESSAGE            EL504
00284          GO TO ABEND-PGM.                                         EL504
00285                                                                   EL504
00286      IF EP-SW = '1'                                               EL504
00287          MOVE WS-CURRENT-BIN-DT   TO CF-RATES-FILE-CREATE-DT      EL504
00288 *        MOVE LOW-VALUES          TO CF-RATES-FILE-MAINT-DT       EL504
00289      ELSE                                                         EL504
00290          GO TO 150-EXIT.                                          EL504
00291                                                                   EL504
00292      REWRITE CONTROL-FILE.                                        EL504
00293                                                                   EL504
00294      IF CONTROL-FILE-STATUS NOT = ZERO                            EL504
00295          MOVE CONTROL-FILE-STATUS                                 EL504
00296                                  TO WS-ABEND-FILE-STATUS          EL504
00297          MOVE ' CONTROL FILE REWRITE ERROR '                      EL504
00298                                  TO WS-ABEND-MESSAGE              EL504
00299          GO TO ABEND-PGM.                                         EL504
00300                                                                   EL504
00301  150-EXIT.                                                        EL504
00302      EXIT.                                                        EL504
00303      EJECT                                                        EL504
00304                                                                   EL504
00305  300-UNLOAD-RATE.                                                 EL504
00306      OPEN OUTPUT RATE-MSTR.                                       EL504
00307                                                                   EL504
00308      IF VSAMRATE-FILE-STATUS NOT = '00' AND '97'                  EL504
00309          MOVE VSAMRATE-FILE-STATUS                                EL504
00310                                  TO WS-ABEND-FILE-STATUS          EL504
00311          MOVE ' VSAM RATE MASTER OPEN ERROR '                     EL504
00312                                  TO WS-ABEND-MESSAGE              EL504
00313          GO TO ABEND-PGM.                                         EL504
00314                                                                   EL504
00315      MOVE 'Y'                    TO RATE-OPEN.                    EL504
00316      MOVE WS-CONTROL-PRIMARY     TO RT-CONTROL-PRIMARY.           EL504
00317                                                                   EL504
00318  305-START.                                                       EL504
00319      START ERRATE KEY NOT LESS RT-CONTROL-PRIMARY.                EL504
00320                                                                   EL504
00321      IF RATE-FILE-STATUS  = '10' OR '23'                          EL504
00322           GO TO 300-EXIT.                                         EL504
00323                                                                   EL504
00324      IF RATE-FILE-STATUS NOT = ZERO                               EL504
00325          MOVE RATE-FILE-STATUS                                    EL504
00326                                  TO WS-ABEND-FILE-STATUS          EL504
00327          MOVE ' RATE MASTER START ERROR- '                        EL504
00328                                  TO WS-ABEND-MESSAGE              EL504
00329          GO TO ABEND-PGM.                                         EL504
00330                                                                   EL504
00331  310-READNEXT.                                                    EL504
00332      READ ERRATE  NEXT RECORD.                                    EL504
00333                                                                   EL504
00334      COPY ELCRTEM1.                                               EL504
00335      IF RTE1 = '1'                                                EL504
00336              GO TO 300-EXIT.                                      EL504
00337                                                                   EL504
00338      IF RATE-FILE-STATUS NOT = ZEROS                              EL504
00339          MOVE 'ERROR ON READ OF RATE MASTER '                     EL504
00340                                  TO WS-ABEND-MESSAGE              EL504
00341          MOVE RATE-FILE-STATUS   TO  WS-ABEND-FILE-STATUS         EL504
00342          GO TO ABEND-PGM.                                         EL504
00343                                                                   EL504
00344      IF DTE-CLASIC-COMPANY-CD NOT = RT-COMPANY-CD                 EL504
00345          GO TO 300-EXIT.                                          EL504
00346                                                                   EL504
00347      COPY ELCRTEM2.                                               EL504
00348      MOVE RATE-RECORD            TO RATE-MSTR-REC.                EL504
00349                                                                   EL504
00350      WRITE RATE-MSTR-REC                                          EL504
00351                                                                   EL504
00352      ADD 1  TO RECORD-COUNT.                                      EL504
00353                                                                   EL504
00354      GO TO 310-READNEXT.                                          EL504
00355                                                                   EL504
00356  300-EXIT.                                                        EL504
00357      EXIT.                                                        EL504
00358                                                                   EL504
00359      EJECT                                                        EL504
00360                                                                   EL504
00361  310-DATE-RTN.                                                    EL504
00362      CALL 'ELDATCX'  USING DATE-CONVERSION-DATA.                  EL504
00363                                                                   EL504
00364      IF DC-ERROR-CODE NOT = SPACE                                 EL504
00365          MOVE  ZEROS             TO DC-BIN-DATE-1                 EL504
00366          ADD 1  TO DATE-ERROR-COUNT.                              EL504
00367                                                                   EL504
00368  310-EXIT.                                                        EL504
00369      EXIT.                                                        EL504
00370                                                                   EL504
00371      EJECT                                                        EL504
00372  800-FINALIZE.                                                    EL504
00373      IF RATE-OPEN = 'Y'                                           EL504
00374         CLOSE RATE-MSTR.                                          EL504
00375                                                                   EL504
00376      CLOSE PRNTR                                                  EL504
00377            ERRATE ELCNTL.                                         EL504
00378                                                                   EL504
00379  800-CLOSE-OTHER. COPY ELCPRTCX.                                  EL504
00380                                                                   EL504
00381  800-EXIT.                                                        EL504
00382      EXIT.                                                        EL504
00383      EJECT                                                        EL504
00384  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL504
00385                                                                   EL504
00386  WRITE-HEADINGS SECTION.                                          EL504
00387 ***************************************************************** EL504
00388 *                                                               * EL504
00389 *                            ELCWHS1.                           * EL504
00390 *                            VMOD=2.001                         * EL504
00391 *                                                               * EL504
00392 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          * EL504
00393 *****************************************************************.EL504
00394  WHS-010.                                                         EL504
00395      ADD +1  TO  WS-PAGE.                                         EL504
00396      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL504
00397      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL504
00398      MOVE ZERO                   TO  WS-LINE-COUNT.               EL504
00399                                                                   EL504
00400      MOVE WS-HEADING1            TO  PRT.                         EL504
00401      MOVE '1'                    TO  X.                           EL504
00402      PERFORM WRITE-PRINTER.                                       EL504
00403                                                                   EL504
00404      MOVE WS-HEADING2            TO  PRT.                         EL504
00405      MOVE ' '                    TO  X.                           EL504
00406      PERFORM WRITE-PRINTER.                                       EL504
00407                                                                   EL504
00408      MOVE WS-HEADING3            TO  PRT.                         EL504
00409      MOVE ' '                    TO  X.                           EL504
00410      PERFORM WRITE-PRINTER.                                       EL504
00411                                                                   EL504
00412      MOVE WS-HEADING4            TO  PRT.                         EL504
00413      MOVE ' '                    TO  X.                           EL504
00414      PERFORM WRITE-PRINTER.                                       EL504
00415                                                                   EL504
00416      MOVE +4                     TO WS-LINE-COUNT.                EL504
00417                                                                   EL504
00418  WHS-020.                                                         EL504
00419 ***************************************************************** EL504
00420 *                                                               * EL504
00421 *                            ELCWHS2.                           * EL504
00422 *                            VMOD=2.001                         * EL504
00423 *****************************************************************.EL504
00424      MOVE WS-SAVE-PRINT-RECORD   TO  PRT.                         EL504
00425      MOVE '-'                    TO  P-CTL.                          CL**2
00426                                                                   EL504
00427  WHS-EXIT.                                                        EL504
00428      EXIT.                                                        EL504
00429                                                                   EL504
00430                                                                   EL504
00431  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL504
00432  WPS-020. COPY ELCPRT2X.                                          EL504
00433                                                                   EL504
00434  ABEND-PGM  SECTION.  COPY ELCABEND SUPPRESS.                     EL504
00435                                                                   EL504
