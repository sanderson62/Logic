00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL536
00003  PROGRAM-ID.                 EL536 .                                 LV004
00004 *              PROGRAM CONVERTED BY                               EL536
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL536
00006 *              CONVERSION DATE 04/10/96 10:20:02.                 EL536
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL536
00008 *                            VMOD=2.006                           EL536
00009                                                                   EL536
00010 *AUTHOR.     LOGIC INC.                                           EL536
00011 *            DALLAS, TEXAS.                                       EL536
00012                                                                   EL536
00013 *DATE-COMPILED.                                                   EL536
00014                                                                   EL536
00015 *SECURITY.   *****************************************************EL536
00016 *            *                                                   *EL536
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL536
00018 *            *                                                   *EL536
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL536
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL536
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL536
00022 *            *                                                   *EL536
00023 *            *****************************************************EL536
00024                                                                   EL536
00025 *REMARKS.                                                         EL536
00026 *         THIS PROGRAM IS USED TO BACKUP THE ACCOUNT MASTER       EL536
00027 *       AND COMPENSATION MASTER AS USED IN MONTH END PROCESSING.  EL536
00028                                                                   EL536
00029  ENVIRONMENT DIVISION.                                            EL536
00030                                                                   EL536
00031  INPUT-OUTPUT SECTION.                                            EL536
00032                                                                   EL536
00033  FILE-CONTROL.                                                    EL536
00034                                                                   EL536
00035      SELECT PRNTR          ASSIGN TO SYS008-UR-1403-S-SYS008.     EL536
00036                                                                   EL536
00037      SELECT FICH           ASSIGN TO SYS020-UT-2400-S-SYS020.     EL536
00038                                                                   EL536
00039      SELECT DISK-DATE      ASSIGN TO SYS019-FBA1-S-SYS019.        EL536
00040                                                                   EL536
00041      SELECT ACC-MSTR-IN    ASSIGN TO SYS010-FBA1-ERACCTT          EL536
00042                            ORGANIZATION IS INDEXED                EL536
00043                            ACCESS IS SEQUENTIAL                   EL536
00044                            RECORD KEY IS ERACCTT-KEY              EL536
00045                            FILE STATUS IS ERACCTT-FILE-STATUS.    EL536
00046                                                                   EL536
00047      SELECT ACC-MSTR-OUT   ASSIGN TO SYS011-UT-2400-S-SYS011.     EL536
00048                                                                   EL536
00049                                                                   EL536
00050      SELECT COMP-MSTR-IN   ASSIGN TO SYS012-UT-FBA1-S-SYS012.     EL536
00051                                                                   EL536
00052      SELECT COMP-MSTR-OUT  ASSIGN TO SYS013-UT-2400-S-SYS013.     EL536
00053                                                                   EL536
00054      SELECT ELCNTL         ASSIGN TO SYS021-FBA1-ELCNTL           EL536
00055                            ORGANIZATION IS INDEXED                EL536
00056                            ACCESS IS DYNAMIC                      EL536
00057                            RECORD KEY IS CF-CONTROL-PRIMARY       EL536
00058                            FILE STATUS IS CONTROL-FILE-STATUS.    EL536
00059                                                                   EL536
00060      SELECT ELREPT         ASSIGN TO SYS022-FBA1-ELREPT           EL536
00061                            ORGANIZATION IS INDEXED                EL536
00062                            ACCESS IS DYNAMIC                      EL536
00063                            RECORD KEY IS RF-CONTROL-PRIMARY       EL536
00064                            FILE STATUS IS DTE-VSAM-FLAGS.         EL536
00065                                                                   EL536
00066      EJECT                                                        EL536
00067  DATA DIVISION.                                                   EL536
00068                                                                   EL536
00069  FILE SECTION.                                                    EL536
00070                                                                   EL536
00071  FD  PRNTR                   COPY ELCPRTFD.                          CL**2
00072                                                                   EL536
00073  FD  FICH                    COPY ELCFCHFD.                          CL**2
00074                                                                   EL536
00075  FD  DISK-DATE               COPY ELCDTEFD.                          CL**2
00076                                                                   EL536
00077      EJECT                                                        EL536
00078  FD  ACC-MSTR-IN.                                                 EL536
00079                                                                   EL536
00080  01  ACC-MSTR-REC.                                                EL536
00081      12  FILLER           PIC XX.                                 EL536
00082      12  ERACCTT-KEY      PIC X(26).                              EL536
00083      12  FILLER           PIC X(1972).                            EL536
00084                                                                   EL536
00085  FD  ACC-MSTR-OUT                                                 EL536
00086      BLOCK CONTAINS 0 RECORDS
00087      RECORDING MODE F.                                            EL536
00088                                                                   EL536
00089  01  ACC-MSTR-RECORD      PIC X(2000).                            EL536
00090                                                                   EL536
00091  FD  COMP-MSTR-IN                                                 EL536
00092      BLOCK CONTAINS 0 RECORDS
00093      RECORDING MODE F.                                            EL536
00094                                                                   EL536
00095  01  COMP-MSTR-REC              PIC X(700).                       EL536
00096                                                                   EL536
00097  FD  COMP-MSTR-OUT                                                EL536
00098      BLOCK CONTAINS 0 RECORDS
00099      RECORDING MODE F.                                            EL536
00100                                                                   EL536
00101  01  COMP-MSTR-RECORD      PIC X(700).                            EL536
00102                                                                   EL536
00103      EJECT                                                        EL536
00104  FD  ELCNTL.                                                      EL536
00105                            COPY ELCCNTL.                          EL536
00106                                                                   EL536
00107      EJECT                                                        EL536
00108  FD  ELREPT.                                                      EL536
00109                            COPY ELCREPT.                          EL536
00110                                                                   EL536
00111      EJECT                                                        EL536
00112  WORKING-STORAGE SECTION.                                         EL536
00113  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL536
00114                                                                   EL536
00115  77  FILLER  PIC X(32) VALUE '********************************'.  EL536
00116  77  FILLER  PIC X(32) VALUE '      EL536 WORKING-STORAGE     '.  EL536
00117  77  FILLER  PIC X(32) VALUE '********* V/M 2.006 ************'.  EL536
00118                                                                   EL536
00119  01  WORK-AREAS.                                                  EL536
00120      12  WS-LINE-COUNT           PIC S9(3)   VALUE +99 COMP-3.    EL536
00121      12  WS-LINE-COUNT-MAX       PIC S9(3)   VALUE +60 COMP-3.    EL536
00122      12  WS-PAGE                 PIC S9(3)   VALUE +0  COMP-3.    EL536
00123      12  WS-ZERO                 PIC S9      VALUE +0  COMP-3.    EL536
00124      12  WS-RETURN-CODE          PIC S9(4)   VALUE +0  COMP.      EL536
00125      12  K1                      PIC S9(7)   VALUE +1 COMP-3.     EL536
00126      12  K2                      PIC S9(7)   VALUE +2 COMP-3.     EL536
00127      12  WS-RECORDS-IN           PIC S9(7)   VALUE +0 COMP-3.     EL536
00128      12  WS-RECORDS-OUT          PIC S9(7)   VALUE +0 COMP-3.     EL536
00129      12  WS-ABEND-MESSAGE        PIC X(80).                       EL536
00130      12  WS-ABEND-FILE-STATUS    PIC XX.                          EL536
00131      12  CONTROL-FILE-STATUS     PIC XX      VALUE ZEROS.         EL536
00132      12  ERACCTT-FILE-STATUS     PIC XX      VALUE ZEROS.         EL536
00133                                                                   EL536
00134  01  DTE-INTERFACE-CODES.                                         EL536
00135      12  X                       PIC X       VALUE SPACE.         EL536
00136      12  PGM-SUB                 PIC S9(4)   VALUE +536 COMP.     EL536
00137      12  ABEND-CODE              PIC 9999    VALUE ZERO.          EL536
00138      12  ABEND-OPTION            PIC X       VALUE SPACE.         EL536
00139      12  OLC-REPORT-NAME         PIC X(6)    VALUE 'EL536'.       EL536
00140                                                                   EL536
00141  01  WS-CONTROL-PRIMARY.                                          EL536
00142      05  WS-COMPANY-CD           PIC X.                           EL536
00143      05  FILLER                  PIC X(14).                       EL536
00144                                                                   EL536
00145  01  WS-SAVE-PRINT-RECORD        PIC X(133)  VALUE SPACES.        EL536
00146                                                                   EL536
00147      EJECT                                                        EL536
00148                             COPY ELCDATE.                            CL**4
00149                                                                   EL536
00150      EJECT                                                        EL536
00151  01  WS-HEADING1.                                                 EL536
00152      05  FILLER                  PIC X(51)   VALUE '1'.           EL536
00153      05  WS-H1-TITLE             PIC X(73)   VALUE                EL536
00154          'CREDIT FILES BACKUP'.                                   EL536
00155      05  WS-H1-REPORT-NUMBER     PIC X(9) VALUE 'EL536'.          EL536
00156                                                                   EL536
00157  01  WS-HEADING2.                                                 EL536
00158      05  FILLER                  PIC X(46)   VALUE SPACES.        EL536
00159      05  WS-H2-CLIENT-NAME       PIC X(78)   VALUE SPACES.        EL536
00160      05  WS-H2-DATE              PIC X(8)    VALUE SPACES.        EL536
00161      05  FILLER                  PIC X       VALUE SPACES.        EL536
00162                                                                   EL536
00163  01  WS-HEADING3.                                                 EL536
00164      05  FILLER                  PIC X(51)   VALUE SPACES.        EL536
00165      05  WS-H3-DATE              PIC X(60)   VALUE SPACES.        EL536
00166      05  FILLER                  PIC X(5)    VALUE 'PAGE'.        EL536
00167      05  WS-H3-PAGE              PIC ZZ,ZZ9.                      EL536
00168      05  FILLER                  PIC X(11)   VALUE SPACES.        EL536
00169                                                                   EL536
00170  01  WS-HEADING4                 PIC X(132)  VALUE SPACES.        EL536
00171                                                                   EL536
00172  01  WS-DETAIL1.                                                  EL536
00173      05  FILLER                  PIC X         VALUE SPACES.      EL536
00174      05  DTL-MSG1                PIC X(18)     VALUE              EL536
00175              'SUCCESSFUL BACKUP '.                                EL536
00176      05  FILLER                  PIC X(15)     VALUE              EL536
00177              '  RECORDS IN - '.                                   EL536
00178      05  DTL-RECORDS-IN          PIC Z,ZZZ,ZZ9-.                  EL536
00179      05  FILLER                  PIC X(16)     VALUE              EL536
00180              '  RECORDS OUT - '.                                  EL536
00181      05  DTL-RECORDS-OUT         PIC Z,ZZZ,ZZ9-.                  EL536
00182                                                                   EL536
00183                             COPY ELCDTECX.                           CL**2
00184                                                                      CL**3
00185                             COPY ELCDTEVR.                           CL**3
00186                                                                   EL536
00187      EJECT                                                        EL536
00188  PROCEDURE DIVISION.                                              EL536
00189                                                                   EL536
00190  0000-LOAD-DATE-WS.         COPY ELCDTERX.                           CL**2
00191                                                                   EL536
00192                                                                   EL536
00193  0000-MAINLINE SECTION.                                           EL536
00194                                                                   EL536
00195      IF EP-SW = '1'                                               EL536
00196          NEXT SENTENCE                                            EL536
00197      ELSE                                                         EL536
00198          GO TO 0000-EOJ.                                          EL536
00199                                                                   EL536
00200      PERFORM 0800-INITIALIZE.                                     EL536
00201                                                                   EL536
00202      PERFORM 0100-BACKUP-ACCT-MSTR.                               EL536
00203                                                                   EL536
00204      PERFORM 0200-BACKUP-COMP-MSTR.                               EL536
00205                                                                   EL536
00206      PERFORM 0900-FINALIZE.                                       EL536
00207                                                                   EL536
00208  0000-EOJ.                                                        EL536
00209      GOBACK.                                                      EL536
00210                                                                   EL536
00211  0000-MAINLINE-EXIT.                                              EL536
00212      EXIT.                                                        EL536
00213      EJECT                                                        EL536
00214                                                                   EL536
00215  0100-BACKUP-ACCT-MSTR SECTION.                                   EL536
00216                                                                   EL536
00217      OPEN INPUT  ACC-MSTR-IN                                      EL536
00218           OUTPUT ACC-MSTR-OUT.                                    EL536
00219                                                                   EL536
00220      IF ERACCTT-FILE-STATUS  = '00' OR '97'                       EL536
00221          NEXT SENTENCE                                            EL536
00222        ELSE                                                       EL536
00223          MOVE ERACCTT-FILE-STATUS                                 EL536
00224                                  TO WS-ABEND-FILE-STATUS          EL536
00225          MOVE ' ERACCTT OPEN ERROR-'                              EL536
00226                                  TO WS-ABEND-MESSAGE              EL536
00227          GO TO ABEND-PGM.                                         EL536
00228                                                                   EL536
00229      MOVE ZEROS                  TO  WS-RECORDS-IN                EL536
00230                                      WS-RECORDS-OUT.              EL536
00231                                                                   EL536
00232  0110-PROCESS-ACCT.                                               EL536
00233      READ ACC-MSTR-IN                                             EL536
00234                                                                   EL536
00235          IF ERACCTT-FILE-STATUS = '10'                            EL536
00236              GO TO 0120-CLOSE-FILES.                              EL536
00237                                                                   EL536
00238      IF ERACCTT-FILE-STATUS NOT = '00'                            EL536
00239          MOVE ERACCTT-FILE-STATUS                                 EL536
00240                                  TO WS-ABEND-FILE-STATUS          EL536
00241          MOVE ' ERACCTT READ ERROR-'                              EL536
00242                                  TO WS-ABEND-MESSAGE              EL536
00243          GO TO ABEND-PGM.                                         EL536
00244                                                                   EL536
00245      ADD +1                      TO  WS-RECORDS-IN.               EL536
00246                                                                   EL536
00247      MOVE ACC-MSTR-REC           TO  ACC-MSTR-RECORD.             EL536
00248                                                                   EL536
00249      WRITE ACC-MSTR-RECORD.                                       EL536
00250                                                                   EL536
00251      ADD +1                      TO  WS-RECORDS-OUT.              EL536
00252                                                                   EL536
00253      GO TO 0110-PROCESS-ACCT.                                     EL536
00254                                                                   EL536
00255  0120-CLOSE-FILES.                                                EL536
00256      CLOSE ACC-MSTR-IN                                            EL536
00257            ACC-MSTR-OUT.                                          EL536
00258                                                                   EL536
00259      MOVE WS-RECORDS-IN          TO  DTL-RECORDS-IN.              EL536
00260      MOVE WS-RECORDS-OUT         TO  DTL-RECORDS-OUT.             EL536
00261      MOVE WS-DETAIL1             TO  PRT.                         EL536
00262      PERFORM WRITE-A-LINE.                                        EL536
00263                                                                   EL536
00264  0100-EXIT.                                                       EL536
00265      EXIT.                                                        EL536
00266                                                                   EL536
00267      EJECT                                                        EL536
00268                                                                   EL536
00269  0200-BACKUP-COMP-MSTR SECTION.                                   EL536
00270                                                                   EL536
00271      OPEN INPUT  COMP-MSTR-IN                                     EL536
00272           OUTPUT COMP-MSTR-OUT.                                   EL536
00273                                                                   EL536
00274      MOVE ZEROS                  TO  WS-RECORDS-IN                EL536
00275                                      WS-RECORDS-OUT.              EL536
00276                                                                   EL536
00277  0210-PROCESS-COMP.                                               EL536
00278      READ COMP-MSTR-IN                                            EL536
00279          AT END                                                   EL536
00280              GO TO 0220-CLOSE-FILES.                              EL536
00281                                                                   EL536
00282      ADD +1                      TO  WS-RECORDS-IN.               EL536
00283                                                                   EL536
00284      MOVE COMP-MSTR-REC          TO  COMP-MSTR-RECORD.            EL536
00285                                                                   EL536
00286      WRITE COMP-MSTR-RECORD.                                      EL536
00287                                                                   EL536
00288      ADD +1                      TO  WS-RECORDS-OUT.              EL536
00289                                                                   EL536
00290      GO TO 0210-PROCESS-COMP.                                     EL536
00291                                                                   EL536
00292  0220-CLOSE-FILES.                                                EL536
00293      CLOSE COMP-MSTR-IN                                           EL536
00294            COMP-MSTR-OUT.                                         EL536
00295                                                                   EL536
00296      MOVE WS-RECORDS-IN          TO  DTL-RECORDS-IN.              EL536
00297      MOVE WS-RECORDS-OUT         TO  DTL-RECORDS-OUT.             EL536
00298      MOVE WS-DETAIL1             TO  PRT.                         EL536
00299      PERFORM WRITE-A-LINE.                                        EL536
00300                                                                   EL536
00301  0200-EXIT.                                                       EL536
00302      EXIT.                                                        EL536
00303                                                                   EL536
00304      EJECT                                                        EL536
00305                                                                   EL536
00306  0800-INITIALIZE SECTION.                                         EL536
00307                                                                   EL536
00308      MOVE COMPANY-NAME           TO WS-H2-CLIENT-NAME.            EL536
00309      MOVE WS-CURRENT-DATE        TO WS-H2-DATE                    EL536
00310      MOVE ALPH-DATE              TO WS-H3-DATE.                   EL536
00311                                                                   EL536
00312      OPEN INPUT  ELCNTL                                           EL536
00313           OUTPUT PRNTR.                                           EL536
00314                                                                   EL536
00315      IF CONTROL-FILE-STATUS  = '00' OR '97'                       EL536
00316          NEXT SENTENCE                                            EL536
00317        ELSE                                                       EL536
00318          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL536
00319          MOVE ' CNTL OPEN ERROR- ' TO WS-ABEND-MESSAGE            EL536
00320          GO TO ABEND-PGM.                                         EL536
00321                                                                   EL536
00322      MOVE LOW-VALUES             TO WS-CONTROL-PRIMARY.           EL536
00323      MOVE DTE-CLASIC-COMPANY-CD  TO WS-COMPANY-CD.                EL536
00324                                                                   EL536
00325      MOVE SPACES                 TO CF-ACCESS-CD-GENL.            EL536
00326      MOVE DTE-CLIENT             TO CF-COMPANY-ID.                EL536
00327      MOVE '1'                    TO CF-RECORD-TYPE.               EL536
00328      MOVE ZEROS                  TO CF-SEQUENCE-NO.               EL536
00329                                                                   EL536
00330      READ ELCNTL                                                  EL536
00331                                                                   EL536
00332      IF CONTROL-FILE-STATUS NOT = ZERO                            EL536
00333          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL536
00334          MOVE ' CNTL READ ERROR- ' TO WS-ABEND-MESSAGE            EL536
00335          GO TO ABEND-PGM.                                         EL536
00336                                                                   EL536
00337  0800-EXIT.                                                       EL536
00338      EXIT.                                                        EL536
00339                                                                   EL536
00340      EJECT                                                        EL536
00341                                                                   EL536
00342  0900-FINALIZE SECTION.                                           EL536
00343                                                                   EL536
00344      CLOSE PRNTR.                                                 EL536
00345                                                                   EL536
00346  0900-CLOSE-OTHER. COPY ELCPRTCX.                                 EL536
00347                                                                   EL536
00348  0900-EXIT.                                                       EL536
00349      EXIT.                                                        EL536
00350      EJECT                                                        EL536
00351                                                                   EL536
00352  WRITE-A-LINE SECTION.           COPY ELCWAL.                     EL536
00353      EJECT                                                        EL536
00354  WRITE-HEADINGS SECTION.                                          EL536
00355 ***************************************************************** EL536
00356 *                            ELCWHS1.                           * EL536
00357 *                            VMOD=2.001                         * EL536
00358 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          * EL536
00359 *****************************************************************.EL536
00360  WHS-010.                                                         EL536
00361      ADD +1  TO  WS-PAGE.                                         EL536
00362      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL536
00363      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL536
00364      MOVE ZERO                   TO  WS-LINE-COUNT.               EL536
00365                                                                   EL536
00366      MOVE WS-HEADING1            TO  PRT.                         EL536
00367      MOVE '1'                    TO  X.                           EL536
00368      PERFORM WRITE-PRINTER.                                       EL536
00369                                                                   EL536
00370      MOVE WS-HEADING2            TO  PRT.                         EL536
00371      MOVE ' '                    TO  X.                           EL536
00372      PERFORM WRITE-PRINTER.                                       EL536
00373                                                                   EL536
00374      MOVE WS-HEADING3            TO  PRT.                         EL536
00375      MOVE ' '                    TO  X.                           EL536
00376      PERFORM WRITE-PRINTER.                                       EL536
00377                                                                   EL536
00378      MOVE WS-HEADING4            TO  PRT.                         EL536
00379      MOVE ' '                    TO  X.                           EL536
00380      PERFORM WRITE-PRINTER.                                       EL536
00381                                                                   EL536
00382      MOVE +4                     TO WS-LINE-COUNT.                EL536
00383  WHS-020.                        COPY ELCWHS2.                    EL536
00384      EJECT                                                        EL536
00385  WRITE-PRINTER SECTION.          COPY ELCWPS.                     EL536
00386  WPS-020.                        COPY ELCPRT2X.                   EL536
00387                                                                   EL536
00388  ABEND-PGM  SECTION.             COPY ELCABEND.                      CL**2
00389                                                                   EL536
00390                                                                   EL536
