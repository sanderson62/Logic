00001  IDENTIFICATION DIVISION.                                         04/22/98
00002                                                                   EL501X
00003  PROGRAM-ID.                 EL501X.                                 LV012
00004 *              PROGRAM CONVERTED BY                                  CL**4
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**4
00006 *              CONVERSION DATE 02/19/96 16:25:01.                    CL**4
00007 *                            VMOD=2.005                              CL**7
00008 *                                                                 EL501X
00008 *                                                                 EL501X
00009 *AUTHOR.     LOGIC INC.                                              CL**4
00010 *            DALLAS, TEXAS.                                          CL**4
00011                                                                   EL501X
00012 *DATE-COMPILED.                                                      CL**4
00013                                                                   EL501X
00014 *SECURITY.   *****************************************************   CL**4
00015 *            *                                                   *   CL**4
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**4
00017 *            *                                                   *   CL**4
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**4
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**4
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**4
00021 *            *                                                   *   CL**4
00022 *            *****************************************************   CL**4
00023                                                                   EL501X
00024 *REMARKS.                                                            CL**4
00025 *         THIS PROGRAM IS USED TO CREATE ACCOUNT MASTER DATA SET     CL**4
00026 *       FOR THE ONLINE CREDIT SYSTEM.  A CONTROL PAGE WILL BE        CL**4
00027 *       PRINTED AT EOJ.                                              CL**4
00028                                                                   EL501X
00029  ENVIRONMENT DIVISION.                                            EL501X
00030                                                                   EL501X
00031  INPUT-OUTPUT SECTION.                                            EL501X
00032                                                                   EL501X
00033  FILE-CONTROL.                                                    EL501X
00034                                                                   EL501X
00035      SELECT PRNTR          ASSIGN TO SYS008-UR-1403-S-SYS008.     EL501X
00036                                                                   EL501X
00037      SELECT FICH           ASSIGN TO SYS020-UT-2400-S-SYS020.     EL501X
00038                                                                   EL501X
00039      SELECT DISK-DATE      ASSIGN TO SYS019-UT-FBA1-S-SYS019.     EL501X
00040                                                                   EL501X
00041      SELECT ERACCTT       ASSIGN TO SYS010-FBA1-ERACCTT           EL501X
00042              ORGANIZATION IS INDEXED                              EL501X
00043              ACCESS IS SEQUENTIAL                                 EL501X
00044              RECORD KEY IS ERACCTT-KEY                            EL501X
00045              FILE STATUS IS ERACCTT-FILE-STATUS.                  EL501X
00046                                                                   EL501X
00047      SELECT ERAMT         ASSIGN TO SYS011-FBA1-ERACCT            EL501X
00048              ORGANIZATION IS INDEXED                              EL501X
00049              ACCESS IS DYNAMIC                                    EL501X
00050              RECORD KEY IS AM-CONTROL-PRIMARY                     EL501X
00051              FILE STATUS IS ACCOUNT-FILE-STATUS.                  EL501X
00052                                                                   EL501X
00053      SELECT ELCNTL         ASSIGN TO SYS012-FBA1-ELCNTL           EL501X
00054              ORGANIZATION IS INDEXED                              EL501X
00055              ACCESS IS DYNAMIC                                    EL501X
00056              RECORD KEY IS CF-CONTROL-PRIMARY                     EL501X
00057              FILE STATUS IS CONTROL-FILE-STATUS.                  EL501X
00058                                                                   EL501X
00059      SELECT ELREPT         ASSIGN TO SYS018-FBA1-ELREPT           EL501X
00060              ORGANIZATION IS INDEXED                              EL501X
00061              ACCESS IS DYNAMIC                                    EL501X
00062              RECORD KEY IS RF-CONTROL-PRIMARY                     EL501X
00063              FILE STATUS IS DTE-VSAM-FLAGS.                       EL501X
00064                                                                   EL501X
00065      SELECT ERMEBL                                                EL501X
00066              ASSIGN SYS024-FBA1-ERMEBL                            EL501X
00067              ORGANIZATION INDEXED                                 EL501X
00068              ACCESS DYNAMIC                                       EL501X
00069              RECORD KEY ME-CONTROL-PRIMARY                        EL501X
00070              FILE STATUS ERMEBL-FILE-STATUS.                      EL501X
00071                                                                   EL501X
00072      EJECT                                                        EL501X
00073  DATA DIVISION.                                                   EL501X
00074                                                                   EL501X
00075  FILE SECTION.                                                    EL501X
00076                                                                   EL501X
00077  FD  PRNTR                   COPY ELCPRTFD SUPPRESS.              EL501X
00078                                                                   EL501X
00079  FD  FICH                    COPY ELCFCHFD SUPPRESS.              EL501X
00080                                                                   EL501X
00081  FD  DISK-DATE               COPY ELCDTEFD SUPPRESS.              EL501X
00082                                                                   EL501X
00083      EJECT                                                        EL501X
00084  FD  ERACCTT.                                                        CL**4
00085  01  ERACCTT-REC.                                                 EL501X
00086      12  FILLER          PIC XX.                                  EL501X
00087      12  ERACCTT-KEY     PIC X(26).                               EL501X
00088      12  FILLER          PIC X(1972).                             EL501X
00089                                                                   EL501X
00090      EJECT                                                        EL501X
00091  FD  ERAMT.                                                          CL**4
00092                            COPY ERCACCT.                             CL**3
00093                                                                   EL501X
00094      EJECT                                                        EL501X
00095  FD  ELCNTL.                                                         CL**4
00096                            COPY ELCCNTL.                             CL**3
00097                                                                   EL501X
00098      EJECT                                                        EL501X
00099  FD  ELREPT.                                                         CL**4
00100                            COPY ELCREPT.                             CL**3
00101                                                                   EL501X
00102      EJECT                                                        EL501X
00103  FD  ERMEBL.                                                         CL**4
00104                         COPY ERCMEBL.                                CL**3
00105      EJECT                                                        EL501X
00106  WORKING-STORAGE SECTION.                                         EL501X
00107  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.         CL**4
00108  01  LCP-TIME-OF-DAY-68            PIC 9(6).                         CL**4
00109  01  LCP-TIME-OF-DAY-74.                                             CL**4
00110      05  LCP-TIME-74               PIC 9(6).                         CL**4
00111      05  FILLER                    PIC 9(2).                         CL**4
00112                                                                   EL501X
00113  77  FILLER  PIC X(32) VALUE '********************************'.  EL501X
00114  77  FILLER  PIC X(32) VALUE '    EL501X  WORKING-STORAGE     '.     CL**2
00115  77  FILLER  PIC X(32) VALUE '********* VMOD=2.005 ***********'.     CL**7
00116                                                                   EL501X
00117  01  MONTH-END-DATA.                                              EL501X
00118      12  ME-START-DATE.                                           EL501X
00119          16  ME-START-MO         PIC 99.                          EL501X
00120          16  FILLER              PIC X.                           EL501X
00121          16  ME-START-DA         PIC 99.                          EL501X
00122          16  FILLER              PIC X.                           EL501X
00123          16  ME-START-YR         PIC 99.                          EL501X
00124      12  ME-CNDS-DATE            PIC 9(6).                        EL501X
00125      12  ME-CNDS-DATE-R REDEFINES ME-CNDS-DATE.                   EL501X
00126          16  ME-CNDS-MO          PIC 99.                          EL501X
00127          16  ME-CNDS-DA          PIC 99.                          EL501X
00128          16  ME-CNDS-YR          PIC 99.                          EL501X
00129      12  ME-START-TIME           PIC 9(6).                        EL501X
00130      12  ME-UPDATE-FLAG          PIC X VALUE 'Y'.                 EL501X
00131          88  ME-DO-UPDATE        VALUE 'Y'.                       EL501X
00132          88  ME-NO-UPDATE        VALUE 'N'.                       EL501X
00133      12  ERMEBL-FILE-STATUS      PIC XX    VALUE ZERO.            EL501X
00134                                                                   EL501X
00135  01  WORK-AREAS.                                                  EL501X
00136      12  WS-LINE-COUNT          PIC S9(3)  VALUE +99 COMP-3.      EL501X
00137      12  WS-LINE-COUNT-MAX      PIC S9(3)  VALUE +60 COMP-3.      EL501X
00138      12  WS-PAGE                PIC S9(3)  VALUE +0  COMP-3.      EL501X
00139      12  WS-ZERO                PIC S9     VALUE +0  COMP-3.      EL501X
00140      12  WS-RETURN-CODE         PIC S9(4)            COMP.        EL501X
00141                                                                   EL501X
00142      12  WS-CURRENT-BIN-DT      PIC XX.                              CL**2
00143      12  WS-ABEND-MESSAGE       PIC X(80).                        EL501X
00144      12  WS-ABEND-FILE-STATUS   PIC XX  VALUE ZEROS.              EL501X
00145      12  ACCOUNT-FILE-STATUS    PIC XX  VALUE ZEROS.              EL501X
00146      12  AM REDEFINES ACCOUNT-FILE-STATUS.                        EL501X
00147          16  AM1                PIC X.                            EL501X
00148          16  AM2                PIC X.                            EL501X
00149      12  ERACCTT-FILE-STATUS    PIC XX    VALUE ZERO.             EL501X
00150      12  MONTH-END-MOYR         PIC 9999 COMP.                    EL501X
00151      12  CONTROL-FILE-STATUS    PIC XX  VALUE ZEROS.              EL501X
00152                                                                   EL501X
00153      12  EOF-SW                 PIC X    VALUE SPACE.             EL501X
00154          88  END-OF-FILE                 VALUE 'E'.               EL501X
00155                                                                   EL501X
00156      12  WS-CLCNTL-STATUS-SW    PIC S9   VALUE +0.                EL501X
00157          88  ELCNTL-NOT-OPEN             VALUE +0.                EL501X
00158          88  ELCNTL-OPEN                 VALUE +1.                EL501X
00159                                                                   EL501X
00160      12  ERROR-SW               PIC X    VALUE SPACE.             EL501X
00161          88  ERROR-OCCURRED              VALUE 'E'.               EL501X
00162          88  NO-ERRORS                   VALUE ' '.               EL501X
00163                                                                   EL501X
00164      12  WS-DUMMY-VARIABLE      PIC X    VALUE ZEROS.             EL501X
00165          88  VAR-AMT                    VALUE '3'.                EL501X
00166          88  VAR-AMT-STAT               VALUE ' '.                EL501X
00167          88  VAR-AMT-CARR               VALUE '4'.                EL501X
00168          88  VAR-AMT-CARR-STAT          VALUE '2'.                EL501X
00169          88  VAR-ALL                     VALUE '1'.               EL501X
00170                                                                   EL501X
00171  01  DTE-INTERFACE-CODES.                                         EL501X
00172      12  X                  PIC X           VALUE SPACE.          EL501X
00173      12  PGM-SUB            PIC S9(4)  COMP VALUE +501.           EL501X
00174      12  ABEND-CODE         PIC 9999        VALUE ZERO.           EL501X
00175      12  ABEND-OPTION       PIC X           VALUE SPACE.          EL501X
00176      12  OLC-REPORT-NAME    PIC X(6)        VALUE 'EL501'.        EL501X
00177      12  WS-PROCESSOR       PIC X(4)        VALUE SPACES.         EL501X
00178                                                                   EL501X
00179  01  WS-CONTROL-PRIMARY.                                          EL501X
00180      05  WS-COMPANY-CD      PIC X.                                EL501X
00181      05  FILLER             PIC X(21).                            EL501X
00182                                                                   EL501X
00183  01  COMP-3-WORK-AREA.                                            EL501X
00184      05  RECORD-COUNT       PIC S9(7)  VALUE +0.                  EL501X
00185      05  DELETE-COUNT       PIC S9(7)  VALUE +0.                  EL501X
00186      05  DATE-ERROR-COUNT   PIC S9(7)  VALUE +0.                  EL501X
00187                                                                   EL501X
00188  01  WS-SAVE-PRINT-RECORD   PIC X(133) VALUE SPACES.              EL501X
00189                                                                   EL501X
00190      EJECT                                                        EL501X
00191                             COPY ELCDATE.                            CL**3
00192                                                                   EL501X
00193      EJECT                                                        EL501X
00194  01  WS-HEADING1.                                                 EL501X
00195      05  FILLER                      PIC X(51)       VALUE '1'.   EL501X
00196      05  WS-H1-TITLE                 PIC X(73)       VALUE        EL501X
00197          'ACCOUNT FILE LOAD  '.                                   EL501X
00198      05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE '  EL501'.       CL**4
00199                                                                   EL501X
00200  01  WS-HEADING2.                                                 EL501X
00201      05  FILLER                      PIC X(46)       VALUE SPACES.EL501X
00202      05  WS-H2-CLIENT-NAME           PIC X(78)       VALUE SPACES.EL501X
00203      05  WS-H2-DATE                  PIC X(8)        VALUE SPACES.EL501X
00204      05  FILLER                      PIC X           VALUE SPACES.EL501X
00205                                                                   EL501X
00206  01  WS-HEADING3.                                                 EL501X
00207      05  FILLER                      PIC X(51)       VALUE SPACES.EL501X
00208      05  WS-H3-DATE                  PIC X(60)       VALUE SPACES.EL501X
00209      05  FILLER                      PIC X(5)        VALUE 'PAGE'.EL501X
00210      05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL501X
00211      05  FILLER                      PIC X(11)       VALUE SPACES.EL501X
00212                                                                   EL501X
00213  01  WS-HEADING4                     PIC X(132)      VALUE SPACES.EL501X
00214                                                                   EL501X
00215  01  WS-DETAIL1.                                                  EL501X
00216      05  FILLER             PIC X.                                EL501X
00217      05  DTL-MSG1           PIC X(18) VALUE 'SUCCESSFUL LOAD'.    EL501X
00218      05  DTL-MSG2           PIC X(18) VALUE SPACE.                EL501X
00219      05  DTL-MSG3           PIC X(18) VALUE SPACE.                EL501X
00220      05  D-RECORD-COUNT     PIC Z,ZZZ,ZZ9.                        EL501X
00221      05  FILLER             PIC X(10) VALUE ' RECORDS ('.         EL501X
00222      05  D-DELETE-COUNT     PIC Z,ZZZ,ZZ9.                        EL501X
00223      05  FILLER             PIC X(11) VALUE ' DELETED)'.          EL501X
00224      05  FILLER             PIC X(15) VALUE '   DATE ERRORS'.     EL501X
00225      05  D-DATE-ERROR-COUNT PIC Z,ZZZ,ZZ9.                        EL501X
00226                                                                   EL501X
00227      EJECT                                                        EL501X
00228                             COPY ELCDTECX.                           CL**3
00229                             COPY ELCDTEVR.                           CL**5
00230                                                                   EL501X
00231      EJECT                                                        EL501X
00232  PROCEDURE DIVISION.                                              EL501X
00233                                                                   EL501X
00234  CAPTURE-START.                                                   EL501X
00235      OPEN I-O ERMEBL.                                             EL501X
00236                                                                   EL501X
00237      IF ERMEBL-FILE-STATUS  = '00' OR '97'                        EL501X
00238          NEXT SENTENCE                                            EL501X
00239        ELSE                                                       EL501X
00240          MOVE 'N'                TO ME-UPDATE-FLAG.               EL501X
00241                                                                   EL501X
00242  0000-LOAD-DATE-WS. COPY ELCDTERX SUPPRESS.                          CL**4
00243                                                                   EL501X
00244      MOVE WS-TIME                TO ME-START-TIME.                   CL**2
00245      MOVE WS-CURRENT-DATE        TO ME-START-DATE.                   CL**2
00246      MOVE ME-START-MO            TO ME-CNDS-MO.                      CL**2
00247      MOVE ME-START-DA            TO ME-CNDS-DA.                      CL**2
00248      MOVE ME-START-YR            TO ME-CNDS-YR.                      CL**2
00249      MOVE DTE-CLIENT             TO ME-COMPANY.                   EL501X
00250                                                                      CL**2
00251      COMPUTE MONTH-END-MOYR = (RUN-CCYY * 12) + RUN-MO.              CL*12
00252      MOVE MONTH-END-MOYR         TO ME-MOYR.                      EL501X
00253      IF ME-DO-UPDATE                                              EL501X
00254          READ ERMEBL INVALID KEY                                  EL501X
00255          MOVE 'N'                TO ME-UPDATE-FLAG                EL501X
00256          CLOSE ERMEBL.                                            EL501X
00257                                                                   EL501X
00258      EJECT                                                        EL501X
00259                                                                   EL501X
00260  000-MAINLINE.                                                    EL501X
00261      PERFORM 100-INITIALIZE  THRU 100-EXIT.                       EL501X
00262                                                                   EL501X
00263      PERFORM 150-READ-CNTL THRU 150-EXIT.                         EL501X
00264                                                                   EL501X
00265      IF NO-ERRORS                                                 EL501X
00266          PERFORM 200-DELETE-OLD THRU 200-EXIT.                    EL501X
00267                                                                   EL501X
00268      IF NO-ERRORS                                                 EL501X
00269          PERFORM 300-LOAD THRU 300-EXIT                           EL501X
00270                  UNTIL END-OF-FILE                                EL501X
00271                  OR    ERROR-OCCURRED.                            EL501X
00272                                                                   EL501X
00273      IF NO-ERRORS                                                 EL501X
00274          PERFORM 170-UPDATE-CNTL THRU 170-EXIT.                   EL501X
00275                                                                   EL501X
00276      MOVE RECORD-COUNT           TO D-RECORD-COUNT.               EL501X
00277      MOVE DELETE-COUNT           TO D-DELETE-COUNT.               EL501X
00278      MOVE DATE-ERROR-COUNT       TO D-DATE-ERROR-COUNT.           EL501X
00279      MOVE WS-DETAIL1             TO PRT.                          EL501X
00280      PERFORM WRITE-A-LINE.                                        EL501X
00281                                                                   EL501X
00282      PERFORM 800-FINALIZE THRU 800-EXIT.                          EL501X
00283                                                                   EL501X
00284      IF ME-DO-UPDATE                                              EL501X
00285          MOVE 1                  TO ME-501-FLAG                   EL501X
00286          MOVE ME-START-TIME      TO ME-501-START                  EL501X
00287          MOVE ME-CNDS-DATE       TO ME-501-RUN-DT                 EL501X
00288          ACCEPT LCP-TIME-OF-DAY-74 FROM TIME                         CL**4
00289          MOVE LCP-TIME-74 TO LCP-TIME-OF-DAY-68                      CL**4
00290          MOVE  LCP-TIME-OF-DAY-68 TO ME-501-END                      CL**4
00291          ADD 1                   TO ME-501-RUN-CT                 EL501X
00292          REWRITE MONTH-END-BALANCES                               EL501X
00293          CLOSE ERMEBL.                                            EL501X
00294                                                                   EL501X
00295  000-MAINLINE-EXIT.                                               EL501X
00296      GOBACK.                                                         CL**4
00297                                                                   EL501X
00298  100-INITIALIZE.                                                  EL501X
00299      MOVE ZEROS                  TO WS-RETURN-CODE.               EL501X
00300      MOVE COMPANY-NAME           TO WS-H2-CLIENT-NAME             EL501X
00301      MOVE ALPH-DATE              TO WS-H2-DATE.                   EL501X
00302                                                                   EL501X
00303      OPEN INPUT  ERACCTT                                          EL501X
00304           I-O    ERAMT                                            EL501X
00305           OUTPUT PRNTR.                                           EL501X
00306                                                                   EL501X
00307      IF ACCOUNT-FILE-STATUS = '00' OR '97'                        EL501X
00308          NEXT SENTENCE                                            EL501X
00309        ELSE                                                       EL501X
00310          MOVE ACCOUNT-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL501X
00311          MOVE ' AMT OPEN ERROR- '  TO WS-ABEND-MESSAGE            EL501X
00312          GO TO ABEND-PGM.                                         EL501X
00313                                                                   EL501X
00314      IF ERACCTT-FILE-STATUS = '00' OR '97'                        EL501X
00315          NEXT SENTENCE                                            EL501X
00316        ELSE                                                       EL501X
00317          MOVE ERACCTT-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL501X
00318          MOVE ' ERACCTT OPEN ERROR- '  TO WS-ABEND-MESSAGE        EL501X
00319          GO TO ABEND-PGM.                                         EL501X
00320                                                                   EL501X
00321      MOVE LOW-VALUES TO WS-CONTROL-PRIMARY.                       EL501X
00322      MOVE DTE-CLASIC-COMPANY-CD  TO WS-COMPANY-CD.                EL501X
00323      MOVE DTE-CLIENT             TO WS-PROCESSOR.                 EL501X
00324                                                                   EL501X
00325      MOVE WS-CURRENT-DATE        TO DC-GREG-DATE-1-EDIT.             CL**2
00326      MOVE '2'                    TO DC-OPTION-CODE.               EL501X
00327      PERFORM 310-DATE-RTN THRU 310-EXIT.                          EL501X
00328      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.               CL**2
00329                                                                   EL501X
00330  100-EXIT.                                                        EL501X
00331      EXIT.                                                        EL501X
00332      EJECT                                                        EL501X
00333                                                                   EL501X
00334  150-READ-CNTL.                                                   EL501X
00335      OPEN INPUT ELCNTL.                                           EL501X
00336                                                                   EL501X
00337      IF CONTROL-FILE-STATUS = '00' OR '97'                        EL501X
00338          NEXT SENTENCE                                            EL501X
00339        ELSE                                                       EL501X
00340          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL501X
00341          MOVE ' CNTL OPEN ERROR- ' TO WS-ABEND-MESSAGE            EL501X
00342          GO TO ABEND-PGM.                                         EL501X
00343                                                                   EL501X
00344      MOVE +1                     TO WS-CLCNTL-STATUS-SW.          EL501X
00345      MOVE SPACES                 TO CF-ACCESS-CD-GENL.            EL501X
00346      MOVE DTE-CLIENT             TO CF-COMPANY-ID                 EL501X
00347      MOVE '1'                    TO CF-RECORD-TYPE                EL501X
00348      MOVE ZEROS                  TO CF-SEQUENCE-NO.               EL501X
00349                                                                   EL501X
00350      READ ELCNTL.                                                 EL501X
00351                                                                   EL501X
00352      IF CONTROL-FILE-STATUS NOT = ZERO                            EL501X
00353          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL501X
00354          MOVE ' CNTL READ ERROR- ' TO WS-ABEND-MESSAGE            EL501X
00355          GO TO ABEND-PGM.                                         EL501X
00356                                                                   EL501X
00357      IF EP-SW = '1'                                               EL501X
00358          IF CF-ACCOUNT-MSTR-CREATE-DT LESS THAN                   EL501X
00359             CF-ACCOUNT-MSTR-MAINT-DT                              EL501X
00360              MOVE 'E'            TO ERROR-SW                      EL501X
00361              MOVE '- ABORT LOAD     -' TO DTL-MSG1                EL501X
00362              MOVE '- MAINT APPLIED  -' TO DTL-MSG3                EL501X
00363              DISPLAY '*EL501*  LOAD ABORTED FOR - ' DTE-CLIENT    EL501X
00364                  UPON CONSOLE                                     EL501X
00365              DISPLAY '*EL501*  MAINT APPLIED ' UPON CONSOLE       EL501X
00366              MOVE SPACES         TO WS-ABEND-FILE-STATUS          EL501X
00367              MOVE '*EL501* MAINT APPLIED' TO WS-ABEND-MESSAGE     EL501X
00368              GO TO ABEND-PGM                                      EL501X
00369          ELSE                                                     EL501X
00370              NEXT SENTENCE                                        EL501X
00371      ELSE                                                         EL501X
00372          MOVE 'E'                TO ERROR-SW                      EL501X
00373          MOVE '- ABORT LOAD     -' TO DTL-MSG1                    EL501X
00374          MOVE '- UPDATE NOT ON  -' TO DTL-MSG3                    EL501X
00375          DISPLAY '*EL501*  LOAD ABORTED FOR - ' DTE-CLIENT        EL501X
00376              UPON CONSOLE                                         EL501X
00377          DISPLAY '*EL501*  NEED UPDATE SWITCH ON ' UPON CONSOLE   EL501X
00378          MOVE SPACES             TO WS-ABEND-FILE-STATUS          EL501X
00379          MOVE '*EL501* UPDATE NOT ON' TO WS-ABEND-MESSAGE         EL501X
00380          GO TO ABEND-PGM.                                         EL501X
00381                                                                   EL501X
00382      MOVE CF-CERT-ACCESS-CONTROL   TO WS-DUMMY-VARIABLE.          EL501X
00383                                                                   EL501X
00384      CLOSE ELCNTL.                                                EL501X
00385                                                                   EL501X
00386      IF CONTROL-FILE-STATUS NOT = ZERO                            EL501X
00387          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL501X
00388          MOVE ' CNTL OPEN ERROR- ' TO WS-ABEND-MESSAGE            EL501X
00389          GO TO ABEND-PGM.                                         EL501X
00390                                                                   EL501X
00391      MOVE +0                     TO WS-CLCNTL-STATUS-SW.          EL501X
00392                                                                   EL501X
00393  150-EXIT.                                                        EL501X
00394      EXIT.                                                        EL501X
00395                                                                   EL501X
00396  170-UPDATE-CNTL.                                                 EL501X
00397      OPEN I-O ELCNTL.                                             EL501X
00398                                                                   EL501X
00399      IF CONTROL-FILE-STATUS = '00' OR '97'                        EL501X
00400          NEXT SENTENCE                                            EL501X
00401        ELSE                                                       EL501X
00402          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL501X
00403          MOVE ' CNTL OPEN ERROR- ' TO WS-ABEND-MESSAGE            EL501X
00404          GO TO ABEND-PGM.                                         EL501X
00405                                                                   EL501X
00406      MOVE +1                     TO WS-CLCNTL-STATUS-SW.          EL501X
00407      MOVE SPACES                 TO CF-ACCESS-CD-GENL.            EL501X
00408      MOVE DTE-CLIENT             TO CF-COMPANY-ID                 EL501X
00409      MOVE '1'                    TO CF-RECORD-TYPE                EL501X
00410      MOVE ZEROS                  TO CF-SEQUENCE-NO.               EL501X
00411                                                                   EL501X
00412      READ ELCNTL.                                                 EL501X
00413                                                                   EL501X
00414      IF CONTROL-FILE-STATUS NOT = ZERO                            EL501X
00415          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL501X
00416          MOVE ' CNTL READ ERROR- ' TO WS-ABEND-MESSAGE            EL501X
00417          GO TO ABEND-PGM.                                         EL501X
00418                                                                   EL501X
00419      MOVE WS-CURRENT-BIN-DT      TO CF-ACCOUNT-MSTR-MAINT-DT         CL**2
00420                                     CF-ACCOUNT-MSTR-CREATE-DT     EL501X
00421                                                                   EL501X
00422      REWRITE CONTROL-FILE.                                        EL501X
00423                                                                   EL501X
00424      IF CONTROL-FILE-STATUS NOT = ZERO                            EL501X
00425          MOVE CONTROL-FILE-STATUS     TO WS-ABEND-FILE-STATUS     EL501X
00426          MOVE ' CNTL REWRITE ERROR- ' TO WS-ABEND-MESSAGE         EL501X
00427          GO TO ABEND-PGM.                                         EL501X
00428                                                                   EL501X
00429      CLOSE ELCNTL.                                                EL501X
00430                                                                   EL501X
00431      IF CONTROL-FILE-STATUS NOT = ZERO                            EL501X
00432          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL501X
00433          MOVE ' CNTL OPEN ERROR- ' TO WS-ABEND-MESSAGE            EL501X
00434          GO TO ABEND-PGM.                                         EL501X
00435                                                                   EL501X
00436      MOVE +0                     TO WS-CLCNTL-STATUS-SW.          EL501X
00437                                                                   EL501X
00438  170-EXIT.                                                        EL501X
00439      EXIT.                                                        EL501X
00440                                                                   EL501X
00441  200-DELETE-OLD.                                                  EL501X
00442      MOVE WS-CONTROL-PRIMARY     TO AM-CONTROL-PRIMARY.           EL501X
00443                                                                   EL501X
00444  205-START.                                                       EL501X
00445      START ERAMT KEY NOT LESS AM-CONTROL-PRIMARY.                 EL501X
00446                                                                   EL501X
00447      IF ACCOUNT-FILE-STATUS  = '10' OR '23'                       EL501X
00448           GO TO 200-EXIT.                                         EL501X
00449                                                                   EL501X
00450      IF ACCOUNT-FILE-STATUS NOT = ZERO                            EL501X
00451          MOVE ACCOUNT-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL501X
00452          MOVE ' AMT START ERROR- ' TO WS-ABEND-MESSAGE            EL501X
00453          GO TO ABEND-PGM.                                         EL501X
00454                                                                   EL501X
00455  210-READNEXT.                                                    EL501X
00456      READ ERAMT  NEXT RECORD.                                     EL501X
00457                                                                   EL501X
00458      IF AM1 = '1'                                                 EL501X
00459              GO TO 200-EXIT.                                      EL501X
00460                                                                   EL501X
00461      IF ACCOUNT-FILE-STATUS NOT = ZEROS                           EL501X
00462          MOVE ' -ERROR ON READ- ' TO WS-ABEND-MESSAGE             EL501X
00463          MOVE ACCOUNT-FILE-STATUS TO WS-ABEND-FILE-STATUS         EL501X
00464          GO TO ABEND-PGM.                                         EL501X
00465                                                                   EL501X
00466      IF DTE-CLASIC-COMPANY-CD NOT = AM-COMPANY-CD                 EL501X
00467          GO TO 200-EXIT.                                          EL501X
00468                                                                   EL501X
00469      DELETE  ERAMT  RECORD.                                       EL501X
00470                                                                   EL501X
00471      PERFORM 230-ERROR-CHECK  THRU 230-EXIT.                      EL501X
00472                                                                   EL501X
00473      IF ERROR-OCCURRED                                            EL501X
00474          MOVE ' -ERR ON DELETE- ' TO DTL-MSG3                     EL501X
00475          GO TO 200-EXIT.                                          EL501X
00476                                                                   EL501X
00477      ADD 1  TO DELETE-COUNT.                                      EL501X
00478                                                                   EL501X
00479      GO TO 210-READNEXT.                                          EL501X
00480                                                                   EL501X
00481  200-EXIT.                                                        EL501X
00482      EXIT.                                                        EL501X
00483                                                                   EL501X
00484  230-ERROR-CHECK.                                                 EL501X
00485      IF AM1 NOT = '0'                                             EL501X
00486          MOVE   AM               TO DTL-MSG2                      EL501X
00487          MOVE   'E'              TO ERROR-SW.                     EL501X
00488                                                                   EL501X
00489  230-EXIT.                                                        EL501X
00490      EXIT.                                                        EL501X
00491                                                                   EL501X
00492      EJECT                                                        EL501X
00493  300-LOAD.                                                        EL501X
00494      READ ERACCTT                                                 EL501X
00495             AT END MOVE 'E' TO EOF-SW                             EL501X
00496             GO TO 300-EXIT.                                       EL501X
00497                                                                   EL501X
00498      MOVE ERACCTT-REC          TO ACCOUNT-MASTER.                 EL501X
00499                                                                   EL501X
00500      IF AM-EXPIRE-DT = 99999999999                                   CL**7
00501         MOVE LOW-VALUES          TO AM-CNTRL-B                       CL*10
00502         MOVE HIGH-VALUES         TO AM-EXPIRATION-DT                 CL*10
00503      ELSE                                                         EL501X
00504         MOVE AM-EXPIRE-DT        TO DC-GREG-DATE-CYMD                CL*10
00505         MOVE 'L'                 TO DC-OPTION-CODE                   CL*10
00506         PERFORM 310-DATE-RTN THRU 310-EXIT                        EL501X
00507         IF NO-CONVERSION-ERROR                                    EL501X
00508            MOVE LOW-VALUES       TO AM-CNTRL-B                       CL*10
00509            MOVE DC-BIN-DATE-1    TO AM-EXPIRATION-DT                 CL*10
00510         ELSE                                                      EL501X
00511            DISPLAY '*EL501*  INVALID EXPIRE DATE - ' AM           EL501X
00512            DISPLAY '*EL501* ACCOUNT - ' AM-CARRIER ' ' AM-GROUPINGEL501X
00513                ' ' AM-STATE ' ' AM-ACCOUNT.                       EL501X
00514                                                                      CL**2
00515      IF AM-EFFECT-DT = 99999999999                                   CL**7
00516         MOVE HIGH-VALUES         TO AM-EFFECTIVE-DT                  CL*10
00517      ELSE                                                            CL*10
00518         MOVE AM-EFFECT-DT        TO DC-GREG-DATE-CYMD                CL*10
00519         MOVE 'L'                 TO DC-OPTION-CODE                   CL*10
00520         PERFORM 310-DATE-RTN THRU 310-EXIT                        EL501X
00521         IF NO-CONVERSION-ERROR                                    EL501X
00522            MOVE DC-BIN-DATE-1    TO AM-EFFECTIVE-DT               EL501X
00523         ELSE                                                      EL501X
00524            DISPLAY '*EL501*  INVALID EFFECT DATE - ' AM-EFFECT-DT EL501X
00525            DISPLAY '*EL501* ACCOUNT - ' AM-CARRIER ' ' AM-GROUPINGEL501X
00526                ' ' AM-STATE ' ' AM-ACCOUNT.                       EL501X
00527                                                                   EL501X
00528      MOVE SPACES                 TO AM-VG-CARRIER                 EL501X
00529                                     AM-VG-GROUPING                EL501X
00530                                     AM-VG-STATE.                  EL501X
00531                                                                   EL501X
00532      IF CF-ST-ACCNT-CNTL                                          EL501X
00533          MOVE AM-STATE           TO AM-VG-STATE.                  EL501X
00534                                                                   EL501X
00535      IF CF-CARR-ACCNT-CNTL                                        EL501X
00536          MOVE AM-CARRIER         TO AM-VG-CARRIER.                EL501X
00537                                                                   EL501X
00538      IF CF-CARR-ST-ACCNT-CNTL                                     EL501X
00539          MOVE AM-CARRIER         TO AM-VG-CARRIER                 EL501X
00540          MOVE AM-STATE           TO AM-VG-STATE.                  EL501X
00541                                                                   EL501X
00542      IF CF-CARR-GROUP-ST-ACCNT-CNTL                               EL501X
00543          MOVE AM-CARRIER         TO AM-VG-CARRIER                 EL501X
00544          MOVE AM-GROUPING        TO AM-VG-GROUPING                EL501X
00545          MOVE AM-STATE           TO AM-VG-STATE.                  EL501X
00546                                                                   EL501X
00547      WRITE ACCOUNT-MASTER.                                        EL501X
00548                                                                   EL501X
00549      IF AM1 NOT = '0'                                             EL501X
00550          MOVE   AM               TO DTL-MSG2                      EL501X
00551          MOVE ' -ERR ON WRITE- ' TO DTL-MSG3                      EL501X
00552          DISPLAY '*EL501* WRITE ERROR  ACCOUNT - ' AM-CARRIER ' ' EL501X
00553              AM-GROUPING ' ' AM-STATE ' ' AM-ACCOUNT ' '          EL501X
00554              AM-EXPIRATION-DT                                     EL501X
00555          DISPLAY '*EL501* VARIABLE GROUP KEY   - '                EL501X
00556              AM-CONTROL-BY-VAR-GRP                                EL501X
00557          MOVE 'E'                TO ERROR-SW.                     EL501X
00558                                                                   EL501X
00559      ADD  1 TO RECORD-COUNT.                                      EL501X
00560                                                                   EL501X
00561  300-EXIT.                                                        EL501X
00562      EXIT.                                                        EL501X
00563                                                                   EL501X
00564  310-DATE-RTN.                                                    EL501X
00565      CALL 'ELDATCX'  USING DATE-CONVERSION-DATA.                  EL501X
00566                                                                   EL501X
00567      IF DC-ERROR-CODE NOT = SPACE                                 EL501X
00568          MOVE  ZEROS             TO DC-BIN-DATE-1                 EL501X
00569          ADD 1  TO DATE-ERROR-COUNT.                              EL501X
00570                                                                   EL501X
00571  310-EXIT.                                                        EL501X
00572      EXIT.                                                        EL501X
00573                                                                   EL501X
00574      EJECT                                                        EL501X
00575                                                                   EL501X
00576  800-FINALIZE.                                                    EL501X
00577      CLOSE ERACCTT                                                EL501X
00578            PRNTR                                                  EL501X
00579            ERAMT.                                                 EL501X
00580                                                                   EL501X
00581      IF ACCOUNT-FILE-STATUS NOT = ZEROS                           EL501X
00582          MOVE ' -ERROR ON CLOSE- ' TO WS-ABEND-MESSAGE            EL501X
00583          MOVE ACCOUNT-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL501X
00584          GO TO ABEND-PGM.                                         EL501X
00585                                                                   EL501X
00586      IF ELCNTL-OPEN                                               EL501X
00587          CLOSE ELCNTL                                             EL501X
00588          IF CONTROL-FILE-STATUS NOT = ZEROS                       EL501X
00589              MOVE ' -ERROR ON CLOSE- ' TO WS-ABEND-MESSAGE        EL501X
00590              MOVE CONTROL-FILE-STATUS TO WS-ABEND-FILE-STATUS     EL501X
00591              GO TO ABEND-PGM.                                     EL501X
00592                                                                   EL501X
00593  800-CLOSE-OTHER. COPY ELCPRTCX.                                  EL501X
00594                                                                   EL501X
00595  800-EXIT.                                                        EL501X
00596      EXIT.                                                        EL501X
00597      EJECT                                                        EL501X
00598  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL501X
00599                                                                   EL501X
00600  WRITE-HEADINGS SECTION.                                             CL**4
00601 *****************************************************************    CL**4
00602 *                                                               *    CL**4
00603 *                            ELCWHS1.                           *    CL**4
00604 *                            VMOD=2.001                         *    CL**4
00605 *                                                               *    CL**4
00606 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          *    CL**4
00607 *****************************************************************.   CL**4
00608  WHS-010.                                                            CL**4
00609      IF  WS-H2-DATE EQUAL SPACES                                     CL**4
00610          MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                      CL**4
00611          MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME               CL**4
00612          MOVE ALPH-DATE          TO  WS-H3-DATE.                     CL**4
00613                                                                      CL**4
00614      ADD +1  TO  WS-PAGE.                                            CL**4
00615      MOVE WS-PAGE                TO  WS-H3-PAGE.                     CL**4
00616      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.           CL**4
00617      MOVE ZERO                   TO  WS-LINE-COUNT.                  CL**4
00618                                                                      CL**4
00619      MOVE WS-HEADING1            TO  PRT.                            CL**4
00620      MOVE '1'                    TO  X.                              CL**4
00621      PERFORM WRITE-PRINTER.                                          CL**4
00622                                                                      CL**4
00623      MOVE WS-HEADING2            TO  PRT.                            CL**4
00624      MOVE ' '                    TO  X.                              CL**4
00625      PERFORM WRITE-PRINTER.                                          CL**4
00626                                                                      CL**4
00627      MOVE WS-HEADING3            TO  PRT.                            CL**4
00628      MOVE ' '                    TO  X.                              CL**4
00629      PERFORM WRITE-PRINTER.                                          CL**4
00630                                                                      CL**4
00631      MOVE WS-HEADING4            TO  PRT.                            CL**4
00632      MOVE ' '                    TO  X.                              CL**4
00633      PERFORM WRITE-PRINTER.                                          CL**4
00634                                                                      CL**4
00635      MOVE +4 TO WS-LINE-COUNT.                                    EL501X
00636                                                                      CL**4
00637  WHS-020. COPY ELCWHS2.                                           EL501X
00638                                                                   EL501X
00639  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL501X
00640  WPS-020. COPY ELCPRT2X.                                             CL**4
00641                                                                   EL501X
00642  ABEND-PGM  SECTION.  COPY ELCABEND SUPPRESS.                        CL**4
