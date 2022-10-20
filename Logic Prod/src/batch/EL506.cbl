00001  IDENTIFICATION DIVISION.                                         03/06/98
00002                                                                   EL506
00003  PROGRAM-ID.                 EL506 .                                 LV003
00004 *              PROGRAM CONVERTED BY                               EL506
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL506
00006 *              CONVERSION DATE 02/19/96 16:45:49.                 EL506
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL506
00008 *                            VMOD=2.008                              CL**3
00009                                                                   EL506
00009                                                                   EL506
00010 *AUTHOR.     LOGIC INC.                                           EL506
00011 *            DALLAS, TEXAS.                                       EL506
00012                                                                   EL506
00013 *DATE-COMPILED.                                                   EL506
00014                                                                   EL506
00015 *SECURITY.   *****************************************************EL506
00016 *            *                                                   *EL506
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL506
00018 *            *                                                   *EL506
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL506
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL506
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL506
00022 *            *                                                   *EL506
00023 *            *****************************************************EL506
00024                                                                   EL506
00025 *REMARKS.                                                         EL506
00026 *      THIS PROGRAM LOADS THE ONLINE COMMISSION MASTER TO         EL506
00027 *      A VSAM FILE AFTER IT EXPLODES ALL TABLES TO COVER THE      EL506
00028 *      ENTIRE BENEFIT RANGE AS DEFINED BY THE BENEFIT TABLE       EL506
00029 *      IN THE DATE CARD FILE.                                     EL506
092602******************************************************************
092602*                   C H A N G E   L O G
092602*
092602* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
092602*-----------------------------------------------------------------
092602*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
092602* EFFECTIVE    NUMBER
092602*-----------------------------------------------------------------
092602* 092602    2002091900008  PEMA  INCREASE NUMBER OF MAXIMUM
092602*                                  BENEFIT CODES FROM 200 TO 450
092602******************************************************************
00030                                                                   EL506
00031  ENVIRONMENT DIVISION.                                            EL506
00032                                                                   EL506
00033  INPUT-OUTPUT SECTION.                                            EL506
00034                                                                   EL506
00035  FILE-CONTROL.                                                    EL506
00036                                                                   EL506
00037      SELECT PRNTR          ASSIGN TO SYS008-UR-1403-S-SYS008.     EL506
00038                                                                   EL506
00039      SELECT FICH           ASSIGN TO SYS020-UT-2400-S-SYS020.     EL506
00040                                                                   EL506
00041      SELECT DISK-DATE      ASSIGN TO SYS019-FBA1-S-SYS019.        EL506
00042                                                                   EL506
00043      SELECT ERCTBL         ASSIGN TO SYS011-FBA1-ERCTBL           EL506
00044              ORGANIZATION IS INDEXED                              EL506
00045              ACCESS IS DYNAMIC                                    EL506
00046              RECORD KEY IS CT-CONTROL-PRIMARY                     EL506
00047              FILE STATUS IS COMMISSION-FILE-STATUS.               EL506
00048                                                                   EL506
00049      SELECT ELCNTL         ASSIGN TO SYS012-FBA1-ELCNTL           EL506
00050              ORGANIZATION IS INDEXED                              EL506
00051              ACCESS IS DYNAMIC                                    EL506
00052              RECORD KEY IS CF-CONTROL-PRIMARY                     EL506
00053              FILE STATUS IS CONTROL-FILE-STATUS.                  EL506
00054                                                                   EL506
00055      SELECT ELREPT         ASSIGN TO SYS018-FBA1-ELREPT           EL506
00056              ORGANIZATION IS INDEXED                              EL506
00057              ACCESS IS DYNAMIC                                    EL506
00058              RECORD KEY IS RF-CONTROL-PRIMARY                     EL506
00059              FILE STATUS IS DTE-VSAM-FLAGS.                       EL506
00060                                                                   EL506
pemuni     SELECT  COMM-TABLE-DISK    ASSIGN TO ERCTBLT                 EL506
00062              ACCESS IS DYNAMIC                                    EL506
00063              ORGANIZATION IS INDEXED                              EL506
00064              FILE STATUS IS COMM-FILE-STATUS                      EL506
00065              RECORD KEY IS CTD-CTL.                               EL506
00066      EJECT                                                        EL506
00067  DATA DIVISION.                                                   EL506
00068                                                                   EL506
00069  FILE SECTION.                                                    EL506
00070                                                                   EL506
00071  FD  PRNTR     COPY ELCPRTFD SUPPRESS.                            EL506
00072                                                                   EL506
00073  FD  FICH      COPY ELCFCHFD SUPPRESS.                            EL506
00074                                                                   EL506
00075  FD  DISK-DATE COPY ELCDTEFD SUPPRESS.                            EL506
00076                                                                   EL506
00077  FD  ELREPT    COPY ELCRPTFD SUPPRESS.                            EL506
00078                                                                   EL506
00079                            COPY ELCREPT.                          EL506
00080      EJECT                                                        EL506
00081  FD  ERCTBL.                                                      EL506
00082                           COPY ERCCTBL.                           EL506
00083                                                                   EL506
00084      EJECT                                                        EL506
00085  FD  COMM-TABLE-DISK.                                             EL506
00086  01  COMM-TABLE-DK.                                               EL506
00087      12  FILLER                      PIC X(2).                    EL506
00088      12  CTD-CTL.                                                 EL506
00089          16  CTR-CONTROL             PIC X(4).                    EL506
00090          16  CTR-BEN-TYPE            PIC X.                       EL506
00091          16  CTR-BEN-CODE            PIC XX.                      EL506
00092      12  FILLER                      PIC X(41).                   EL506
00093      12  CTR-TOT-BEN     OCCURS 3 TIMES                           EL506
00094                                      PIC S9(7)V99     COMP-3.     EL506
00095      12  CTR-AGE         OCCURS 3 TIMES                           EL506
00096                                      PIC S9(2)        COMP-3.     EL506
00097      12  CTR-TERM        OCCURS 3 TIMES                           EL506
00098                                      PIC S9(3)        COMP-3.     EL506
00099      12  CTR-RATE        OCCURS 27 TIMES                          EL506
00100                                      PIC SV9(5)       COMP-3.     EL506
00101      12  FILLER                      PIC X(42).                   EL506
00102      EJECT                                                        EL506
00103  FD  ELCNTL.                                                      EL506
00104                            COPY ELCCNTL.                          EL506
00105                                                                   EL506
00106      EJECT                                                        EL506
00107  WORKING-STORAGE SECTION.                                         EL506
00108  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL506
00109  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.   EL506
00110                                                                   EL506
00111  77  FILLER  PIC X(32) VALUE '********************************'.  EL506
00112  77  FILLER  PIC X(32) VALUE '      EL506 WORKING-STORAGE     '.  EL506
00113  77  FILLER  PIC X(32) VALUE '********** VMOD=2.008 **********'.     CL**3
00114                                                                   EL506
00115  01  WORK-AREAS.                                                  EL506
00116      12  WS-LINE-COUNT          PIC S9(3)  VALUE +99 COMP-3.      EL506
00117      12  WS-LINE-COUNT-MAX      PIC S9(3)  VALUE +60 COMP-3.      EL506
00118      12  WS-PAGE                PIC S9(3)  VALUE +0  COMP-3.      EL506
00119      12  WS-CURRENT-BIN-DT      PIC XX.                           EL506
00120      12  WS-RETURN-CODE         PIC S9(4) COMP.                   EL506
00121      12  WS-ABEND-MESSAGE       PIC X(80).                        EL506
00122      12  WS-ABEND-FILE-STATUS   PIC XX  VALUE ZEROS.              EL506
00123      12  WS-ZERO                PIC S9  VALUE ZERO COMP-3.        EL506
00124      12  COMMISSION-OPEN           PIC X   VALUE SPACES.          EL506
00125      12  COMMISSION-FILE-STATUS    PIC XX  VALUE ZEROS.           EL506
00126      12  COM REDEFINES COMMISSION-FILE-STATUS.                    EL506
00127          16  COM1               PIC X.                            EL506
00128          16  COM2               PIC X.                            EL506
00129      12  CONTROL-FILE-STATUS    PIC XX  VALUE ZEROS.              EL506
00130                                                                   EL506
00131      12  COMM-FILE-STATUS       PIC XX  VALUE ZEROS.              EL506
00132      12  WS-SAVE-PRINT-RECORD   PIC X(133) VALUE SPACES.          EL506
00133      12  SUB1            PIC S9(4)       VALUE +0      COMP.      EL506
00134      12  SUB2            PIC S9(4)       VALUE +0      COMP.      EL506
00135      12  SAVE-CONTROL.                                            EL506
00136          16  SAVE-CO            PIC X    VALUE SPACES.            EL506
00137          16  SAVE-TABLE         PIC XXX  VALUE SPACES.            EL506
00138                                                                   EL506
00139      12  FIRST-READ-SW          PIC X    VALUE 'Y'.               EL506
00140                                                                   EL506
00141  01  DTE-INTERFACE-CODES.                                         EL506
00142      05  X                      PIC X           VALUE SPACE.      EL506
00143      05  PGM-SUB                PIC S9(4)  COMP VALUE +506.       EL506
00144      05  ABEND-CODE             PIC 9999        VALUE ZERO.       EL506
00145      05  ABEND-OPTION           PIC X           VALUE SPACE.      EL506
00146      05  OLC-REPORT-NAME        PIC X(6)        VALUE 'EL506'.    EL506
00147                                                                   EL506
00148      05 WS-IN                   PIC 9(5) VALUE ZEROS.             EL506
00149      05 WS-OUT                  PIC 9(5) VALUE ZEROS.             EL506
00150                                                                   EL506
00151      05  WS-CONTROL-PRIMARY.                                      EL506
00152          10  WS-COMPANY-CD      PIC X.                            EL506
00153          10  WS-TABLE           PIC X(3).                         EL506
00154          10  WS-CNTRL-2.                                          EL506
00155              15  WS-BEN-TYPE    PIC X.                            EL506
00156              15  WS-BEN-CODE    PIC X(2).                         EL506
00157                                                                   EL506
00158  01  COMP-3-WORK-AREA.                                            EL506
00159      05  K1                 PIC S9(7)  VALUE +1.                  EL506
00160      05  K2                 PIC S9(7)  VALUE +2.                  EL506
00161      05  RECORD-COUNT       PIC S9(7)  VALUE +0.                  EL506
00162      05  DELETE-COUNT       PIC S9(7)  VALUE +0.                  EL506
00163      05  DATE-ERROR-COUNT   PIC S9(7)  VALUE +0.                  EL506
00164                                                                   EL506
00165  01  COVERAGE-BENEFIT.                                            EL506
00166      05  COV-BEN-LIFE.                                            EL506
00167          12  COV-CODE-L         PIC X.                            EL506
00168          12  BEN-CODE-L         PIC XX      VALUE 'AA'.           EL506
00169      05  COV-BEN-A-H.                                             EL506
00170          12  COV-CODE-A         PIC X.                            EL506
00171          12  BEN-CODE-A         PIC XX      VALUE 'AA'.           EL506
00172                                                                   EL506
00173  01  DUMMY-COMM-TABLE.                                            EL506
00174      12  FILLER          PIC XX          VALUE 'CT'.              EL506
00175      12  FILLER          PIC X(48)       VALUE SPACES.            EL506
00176      12  DCT-TOT-BEN     OCCURS 3 TIMES                           EL506
00177                          PIC S9(7)V99                  COMP-3.    EL506
00178      12  DCT-AGE         OCCURS 3 TIMES                           EL506
00179                          PIC S9(2)                     COMP-3.    EL506
00180      12  DCT-TERM        OCCURS 3 TIMES                           EL506
00181                          PIC S9(3)                     COMP-3.    EL506
00182      12  DCT-RATE        OCCURS 27 TIMES                          EL506
00183                          PIC SV9(5)                    COMP-3.    EL506
00184      12  FILLER          PIC X(42)       VALUE SPACES.            EL506
00185                                                                   EL506
00186  01  SAVE-RECORD-TABLE.                                           EL506
092602     12  SAVE-RECORD     OCCURS 900 TIMES.                        EL506
00188          16  FILLER            PIC X(6).                          EL506
00189          16  SV-REC-BEN-TYPE   PIC X.                             EL506
00190          16  SV-REC-BEN-CODE   PIC XX.                            EL506
00191          16  FILLER            PIC X(191).                        EL506
00192                                                                   EL506
00193  01  SAVE-AA-RECORD-TABLE.                                        EL506
00194      12  SAVE-AA-RECORD-A      PIC X(200).                        EL506
00195      12  SAVE-AA-RECORD-L      PIC X(200).                        EL506
00196                                                                   EL506
00197                                                                   EL506
00198      EJECT                                                        EL506
00199                             COPY ELCDATE.                         EL506
00200                                                                   EL506
00201      EJECT                                                        EL506
00202                                                                   EL506
00203  01  WS-HEADING1.                                                 EL506
00204      05  FILLER                      PIC X(51)       VALUE '1'.   EL506
00205      05  WS-H1-TITLE                 PIC X(73)       VALUE        EL506
00206          'COMMISSION VSAM LOAD  '.                                EL506
00207      05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL506  '.    EL506
00208                                                                   EL506
00209  01  WS-HEADING2.                                                 EL506
00210      05  FILLER                      PIC X(46)       VALUE SPACES.EL506
00211      05  WS-H2-CLIENT-NAME           PIC X(78)       VALUE SPACES.EL506
00212      05  WS-H2-DATE                  PIC X(8)        VALUE SPACES.EL506
00213      05  FILLER                      PIC X           VALUE SPACES.EL506
00214                                                                   EL506
00215  01  WS-HEADING3.                                                 EL506
00216      05  FILLER                      PIC X(51)       VALUE SPACES.EL506
00217      05  WS-H3-DATE                  PIC X(60)       VALUE SPACES.EL506
00218      05  FILLER                      PIC X(5)        VALUE 'PAGE'.EL506
00219      05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL506
00220      05  FILLER                      PIC X(11)       VALUE SPACES.EL506
00221                                                                   EL506
00222  01  WS-HEADING4                     PIC X(132)      VALUE SPACES.EL506
00223                                                                   EL506
00224  01  WS-DETAIL1.                                                  EL506
00225      05  FILLER             PIC X.                                EL506
00226      05  DTL-MSG1           PIC X(18) VALUE 'SUCCESSFUL LOADED '. EL506
00227      05  DTL-MSG2           PIC X(18) VALUE SPACE.                EL506
00228      05  DTL-MSG3           PIC X(18) VALUE SPACE.                EL506
00229      05  D-RECORD-COUNT-1   PIC Z,ZZZ,ZZ9.                        EL506
00230      05  FILLER             PIC X(10) VALUE ' RECORDS  '.         EL506
00231                                                                   EL506
00232                             COPY ELCDTECX SUPPRESS.               EL506
00233                                                                   EL506
00234                             COPY ELCDTEVR SUPPRESS.               EL506
00235                                                                   EL506
00236      EJECT                                                        EL506
00237  PROCEDURE DIVISION.                                              EL506
00238                                                                   EL506
00239  0000-LOAD-DATE-WS. COPY ELCDTERX SUPPRESS.                       EL506
00240                                                                   EL506
00241  000-MAINLINE.                                                    EL506
00242                                                                   EL506
00243      PERFORM 100-INITIALIZE  THRU 120-EXIT.                       EL506
00244                                                                   EL506
00245      PERFORM 150-READ-CNTL THRU 150-EXIT.                         EL506
00246                                                                   EL506
00247      PERFORM 300-UNLOAD-COMMISSION THRU 300-EXIT.                 EL506
00248                                                                   EL506
00249      MOVE RECORD-COUNT TO D-RECORD-COUNT-1.                       EL506
00250      MOVE WS-DETAIL1   TO PRT.                                    EL506
00251      PERFORM WRITE-A-LINE.                                        EL506
00252      PERFORM 800-FINALIZE THRU 800-EXIT.                          EL506
00253                                                                   EL506
00254      GOBACK.                                                      EL506
00255                                                                   EL506
00256      EJECT                                                        EL506
00257  100-INITIALIZE.                                                  EL506
00258                                                                   EL506
00259      MOVE ZEROS                  TO WS-RETURN-CODE.               EL506
00260      MOVE COMPANY-NAME           TO WS-H2-CLIENT-NAME.            EL506
00261      MOVE WS-CURRENT-DATE        TO WS-H2-DATE.                   EL506
00262      MOVE ALPH-DATE              TO WS-H3-DATE.                   EL506
00263                                                                   EL506
00264      MOVE LIFE-OVERRIDE-L1       TO COV-CODE-L.                   EL506
00265      MOVE AH-OVERRIDE-L1         TO COV-CODE-A.                   EL506
00266                                                                   EL506
00267      OPEN INPUT  ERCTBL                                           EL506
00268           OUTPUT  PRNTR.                                          EL506
00269                                                                   EL506
00270      IF EP-SW = '1'                                               EL506
00271          OPEN I-O ELCNTL                                          EL506
00272      ELSE                                                         EL506
00273          OPEN INPUT ELCNTL.                                       EL506
00274                                                                   EL506
00275      IF CONTROL-FILE-STATUS = '00' OR '97'                        EL506
00276          NEXT SENTENCE                                            EL506
00277        ELSE                                                       EL506
00278          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL506
00279          MOVE ' CNTL OPEN ERROR- ' TO WS-ABEND-MESSAGE            EL506
00280          GO TO ABEND-PGM.                                         EL506
00281                                                                   EL506
00282      IF COMMISSION-FILE-STATUS = '00' OR '97'                     EL506
00283          NEXT SENTENCE                                            EL506
00284        ELSE                                                       EL506
00285          MOVE COMMISSION-FILE-STATUS  TO WS-ABEND-FILE-STATUS     EL506
00286          MOVE ' COMM OPEN ERROR- '    TO WS-ABEND-MESSAGE         EL506
00287          GO TO ABEND-PGM.                                         EL506
00288                                                                   EL506
00289      MOVE LOW-VALUES             TO WS-CONTROL-PRIMARY.           EL506
00290      MOVE DTE-CLASIC-COMPANY-CD  TO WS-COMPANY-CD.                EL506
00291                                                                   EL506
00292      MOVE WS-CURRENT-DATE        TO DC-GREG-DATE-1-EDIT.          EL506
00293      MOVE '2'                    TO DC-OPTION-CODE.               EL506
00294      PERFORM 310-DATE-RTN THRU 310-EXIT.                          EL506
00295      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.            EL506
00296                                                                   EL506
00297      MOVE HIGH-VALUES            TO SAVE-RECORD-TABLE             EL506
00298                                     SAVE-AA-RECORD-TABLE.         EL506
00299                                                                   EL506
00300      MOVE +9999999.99            TO DCT-TOT-BEN (1).              EL506
00301      MOVE +99                    TO DCT-AGE (1).                  EL506
00302      MOVE +999                   TO DCT-TERM (1).                 EL506
00303      MOVE +0 TO DCT-TOT-BEN (2)  DCT-TOT-BEN (3)                  EL506
00304                 DCT-AGE (2)      DCT-AGE (3)                      EL506
00305                 DCT-TERM (2)     DCT-TERM (3).                    EL506
00306                                                                   EL506
00307      PERFORM 110-CLEAR-DUMMY-RATES VARYING SUB1 FROM 1 BY 1       EL506
00308          UNTIL SUB1 GREATER +27.                                  EL506
00309                                                                   EL506
00310      MOVE +0                     TO SUB1.                         EL506
00311      GO TO 120-EXIT.                                              EL506
00312                                                                   EL506
00313  110-CLEAR-DUMMY-RATES.                                           EL506
00314                                                                   EL506
00315      MOVE +0.0                   TO DCT-RATE (SUB1).              EL506
00316                                                                   EL506
00317  120-EXIT.                                                        EL506
00318      EXIT.                                                        EL506
00319                                                                   EL506
00320      EJECT                                                        EL506
00321  150-READ-CNTL.                                                   EL506
00322                                                                   EL506
00323      MOVE SPACES                 TO CF-ACCESS-CD-GENL.            EL506
00324      MOVE DTE-CLIENT             TO CF-COMPANY-ID.                EL506
00325      MOVE '1'                    TO CF-RECORD-TYPE.               EL506
00326      MOVE ZEROS                  TO CF-SEQUENCE-NO.               EL506
00327                                                                   EL506
00328      READ ELCNTL                                                  EL506
00329                                                                   EL506
00330      IF CONTROL-FILE-STATUS NOT = ZERO                            EL506
00331          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL506
00332          MOVE ' CNTL READ ERROR- ' TO WS-ABEND-MESSAGE            EL506
00333          GO TO ABEND-PGM.                                         EL506
00334                                                                   EL506
00335      IF EP-SW = '1'                                               EL506
00336          MOVE WS-CURRENT-BIN-DT   TO CF-COMMISSION-TAB-CREATE-DT  EL506
00337      ELSE                                                         EL506
00338          GO TO 150-EXIT.                                          EL506
00339                                                                   EL506
00340      REWRITE CONTROL-FILE.                                        EL506
00341                                                                   EL506
00342      IF CONTROL-FILE-STATUS NOT = ZERO                            EL506
00343          MOVE CONTROL-FILE-STATUS     TO WS-ABEND-FILE-STATUS     EL506
00344          MOVE ' CNTL REWRITE ERROR- ' TO WS-ABEND-MESSAGE         EL506
00345          GO TO ABEND-PGM.                                         EL506
00346                                                                   EL506
00347  150-EXIT.                                                        EL506
00348      EXIT.                                                        EL506
00349                                                                   EL506
00350      EJECT                                                        EL506
00351                                                                   EL506
00352  300-UNLOAD-COMMISSION.                                           EL506
00353                                                                   EL506
00354      OPEN OUTPUT COMM-TABLE-DISK.                                 EL506
00355                                                                   EL506
00356      IF COMM-FILE-STATUS    = '00' OR '97'                        EL506
00357          NEXT SENTENCE                                            EL506
00358        ELSE                                                       EL506
00359          MOVE COMM-FILE-STATUS           TO WS-ABEND-FILE-STATUS  EL506
00360          MOVE ' COMM-TABLE OPEN ERROR- ' TO WS-ABEND-MESSAGE      EL506
00361          GO TO ABEND-PGM.                                         EL506
00362                                                                   EL506
00363      MOVE 'Y'                     TO COMMISSION-OPEN.             EL506
00364      MOVE WS-CONTROL-PRIMARY      TO CT-CONTROL-PRIMARY.          EL506
00365                                                                   EL506
00366  305-START.                                                       EL506
00367                                                                   EL506
00368      START ERCTBL KEY NOT LESS CT-CONTROL-PRIMARY.                EL506
00369                                                                   EL506
00370      IF COMMISSION-FILE-STATUS  = '10' OR '23'                    EL506
00371           GO TO 300-EXIT.                                         EL506
00372                                                                   EL506
00373      IF COMMISSION-FILE-STATUS NOT = ZERO                         EL506
00374          MOVE COMMISSION-FILE-STATUS  TO WS-ABEND-FILE-STATUS     EL506
00375          MOVE ' COMM  START ERROR- '  TO WS-ABEND-MESSAGE         EL506
00376          GO TO ABEND-PGM.                                         EL506
00377                                                                   EL506
00378  310-READNEXT.                                                    EL506
00379                                                                   EL506
00380      READ ERCTBL  NEXT RECORD.                                    EL506
00381                                                                   EL506
00382      IF FIRST-READ-SW  = 'N'                                      EL506
00383          NEXT SENTENCE                                            EL506
00384      ELSE                                                         EL506
00385          MOVE 'N'                     TO FIRST-READ-SW            EL506
00386          IF COM1 = '1' OR                                         EL506
00387             DTE-CLASIC-COMPANY-CD NOT = CT-COMPANY-CD             EL506
00388               GO TO 300-EXIT.                                     EL506
00389                                                                   EL506
00390      IF COM1 = '1'                                                EL506
00391          PERFORM 500-BUILD-OUTPUT THRU 599-EXIT                   EL506
00392          GO TO 300-EXIT.                                          EL506
00393                                                                   EL506
00394      IF COMMISSION-FILE-STATUS NOT = ZEROS                        EL506
00395          MOVE 'ERROR ON READ  '       TO WS-ABEND-MESSAGE         EL506
00396          MOVE COMMISSION-FILE-STATUS  TO WS-ABEND-FILE-STATUS     EL506
00397          GO TO ABEND-PGM.                                         EL506
00398                                                                   EL506
00399      IF DTE-CLASIC-COMPANY-CD NOT = CT-COMPANY-CD                 EL506
00400          PERFORM 500-BUILD-OUTPUT THRU 599-EXIT                   EL506
00401          GO TO 300-EXIT.                                          EL506
00402                                                                   EL506
00403      IF LCP-ONCTR-01 =  0                                         EL506
00404          ADD 1 TO LCP-ONCTR-01                                    EL506
00405           MOVE CT-TABLE          TO SAVE-TABLE                    EL506
00406           MOVE CT-COMPANY-CD     TO SAVE-CO.                      EL506
00407                                                                   EL506
00408      IF CT-TABLE NOT  =  SAVE-TABLE                               EL506
00409          PERFORM 500-BUILD-OUTPUT THRU 599-EXIT                   EL506
00410          MOVE HIGH-VALUES        TO SAVE-RECORD-TABLE             EL506
00411          MOVE HIGH-VALUES        TO SAVE-AA-RECORD-TABLE          EL506
00412          MOVE +0                 TO SUB1  SUB2                    EL506
00413          MOVE CT-COMPANY-CD      TO SAVE-CO                       EL506
00414          MOVE CT-TABLE           TO SAVE-TABLE.                   EL506
00415                                                                   EL506
00416          PERFORM 400-BUILD-INPUT-TABLE THRU 499-EXIT.             EL506
00417                                                                   EL506
00418      GO TO 310-READNEXT.                                          EL506
00419                                                                   EL506
00420  300-EXIT.                                                        EL506
00421      EXIT.                                                        EL506
00422                                                                   EL506
00423  EJECT                                                            EL506
00424  310-DATE-RTN.                                                    EL506
00425                                                                   EL506
00426      CALL 'ELDATCX'  USING DATE-CONVERSION-DATA.                  EL506
00427                                                                   EL506
00428      IF DC-ERROR-CODE NOT = SPACE                                 EL506
00429          MOVE  ZEROS TO DC-BIN-DATE-1                             EL506
00430          ADD 1  TO DATE-ERROR-COUNT.                              EL506
00431                                                                   EL506
00432  310-EXIT.                                                        EL506
00433      EXIT.                                                        EL506
00434                                                                   EL506
00435      EJECT                                                        EL506
00436  400-BUILD-INPUT-TABLE.                                           EL506
00437                                                                   EL506
00438      IF CT-CNTRL-2 = COV-BEN-LIFE                                 EL506
00439          MOVE COMM-TABLE-RECORD TO SAVE-AA-RECORD-L               EL506
00440          GO TO 499-EXIT.                                          EL506
00441                                                                   EL506
00442      IF CT-CNTRL-2 = COV-BEN-A-H                                  EL506
00443          MOVE COMM-TABLE-RECORD  TO SAVE-AA-RECORD-A              EL506
00444          GO TO 499-EXIT.                                          EL506
00445                                                                   EL506
00446      ADD +1 TO SUB1                                               EL506
00447                                                                   EL506
00448      MOVE COMM-TABLE-RECORD TO SAVE-RECORD (SUB1).                EL506
00449                                                                   EL506
00450  499-EXIT.                                                        EL506
00451      EXIT.                                                        EL506
00452                                                                   EL506
00453  EJECT                                                            EL506
00454  500-BUILD-OUTPUT.                                                EL506
00455                                                                   EL506
00456      MOVE +1                     TO SUB2.                         EL506
00457                                                                   EL506
00458      IF AH-OVERRIDE-L1  LESS THAN  LIFE-OVERRIDE-L1               EL506
00459          MOVE CLAS-STARTA            TO SUB1                      EL506
00460          PERFORM 600-OUTPUT-DISABILITY THRU 699-EXIT              EL506
00461              UNTIL SUB1 GREATER CLAS-MAXA                         EL506
00462          MOVE CLAS-STARTL            TO SUB1                      EL506
00463          PERFORM 700-OUTPUT-LIFE THRU 799-EXIT                    EL506
00464              UNTIL SUB1 GREATER  CLAS-MAXL                        EL506
00465      ELSE                                                         EL506
00466          MOVE CLAS-STARTL            TO SUB1                      EL506
00467          PERFORM 700-OUTPUT-LIFE THRU 799-EXIT                    EL506
00468              UNTIL SUB1 GREATER  CLAS-MAXL                        EL506
00469          MOVE CLAS-STARTA            TO SUB1                      EL506
00470          PERFORM 600-OUTPUT-DISABILITY THRU 699-EXIT              EL506
00471              UNTIL SUB1 GREATER CLAS-MAXA.                        EL506
00472                                                                   EL506
00473  599-EXIT.                                                        EL506
00474      EXIT.                                                        EL506
00475                                                                   EL506
00476  EJECT                                                            EL506
00477  600-OUTPUT-DISABILITY.                                           EL506
00478                                                                   EL506
00479      IF SV-REC-BEN-TYPE (SUB2) LESS AH-OVERRIDE-L1                EL506
00480          ADD +1 TO SUB2                                           EL506
00481          GO TO 600-OUTPUT-DISABILITY.                             EL506
00482                                                                   EL506
00483      IF SV-REC-BEN-TYPE (SUB2) GREATER AH-OVERRIDE-L1             EL506
00484          GO TO 610-CHECK-FOR-AA.                                  EL506
00485                                                                   EL506
00486      IF SV-REC-BEN-CODE (SUB2) LESS CLAS-I-BEN (SUB1)             EL506
00487          ADD +1 TO SUB2                                           EL506
00488          GO TO 600-OUTPUT-DISABILITY.                             EL506
00489                                                                   EL506
00490      IF SV-REC-BEN-CODE (SUB2) = CLAS-I-BEN (SUB1)                EL506
00491          GO TO 630-BUILD-GOOD-DISABILITY.                         EL506
00492                                                                   EL506
00493  610-CHECK-FOR-AA.                                                EL506
00494                                                                   EL506
00495      IF SAVE-AA-RECORD-A = HIGH-VALUES                            EL506
00496          GO TO 660-BUILD-DUMMY-DISABILITY.                        EL506
00497                                                                   EL506
00498      MOVE SAVE-AA-RECORD-A       TO COMM-TABLE-DK.                EL506
00499      MOVE CLAS-I-BEN (SUB1)      TO CTR-BEN-CODE.                 EL506
00500      GO TO 690-WRITE-DISABILITY.                                  EL506
00501                                                                   EL506
00502  630-BUILD-GOOD-DISABILITY.                                       EL506
00503                                                                   EL506
00504      MOVE SAVE-RECORD (SUB2)     TO COMM-TABLE-DK.                EL506
00505      GO TO 690-WRITE-DISABILITY.                                  EL506
00506                                                                   EL506
00507  660-BUILD-DUMMY-DISABILITY.                                      EL506
00508                                                                   EL506
00509      MOVE DUMMY-COMM-TABLE       TO COMM-TABLE-DK.                EL506
00510      MOVE SAVE-CONTROL           TO CTR-CONTROL.                  EL506
00511      MOVE AH-OVERRIDE-L1         TO CTR-BEN-TYPE.                 EL506
00512      MOVE CLAS-I-BEN (SUB1)      TO CTR-BEN-CODE.                 EL506
00513                                                                   EL506
00514  690-WRITE-DISABILITY.                                            EL506
00515                                                                   EL506
00516      PERFORM 900-WRITE-COMM-INDEX THRU 999-EXIT.                  EL506
00517      ADD +1 TO SUB1.                                              EL506
00518                                                                   EL506
00519  699-EXIT.                                                        EL506
00520      EXIT.                                                        EL506
00521                                                                   EL506
00522      EJECT                                                        EL506
00523  700-OUTPUT-LIFE.                                                 EL506
00524                                                                   EL506
00525      IF SV-REC-BEN-TYPE (SUB2) LESS LIFE-OVERRIDE-L1              EL506
00526          ADD +1 TO SUB2                                           EL506
00527          GO TO 700-OUTPUT-LIFE.                                   EL506
00528                                                                   EL506
00529      IF SV-REC-BEN-TYPE (SUB2) GREATER LIFE-OVERRIDE-L1           EL506
00530          GO TO 710-CHECK-FOR-AA.                                  EL506
00531                                                                   EL506
00532      IF SV-REC-BEN-CODE (SUB2) LESS CLAS-I-BEN (SUB1)             EL506
00533          ADD +1 TO SUB2                                           EL506
00534          GO TO 700-OUTPUT-LIFE.                                   EL506
00535                                                                   EL506
00536      IF SV-REC-BEN-CODE (SUB2) = CLAS-I-BEN (SUB1)                EL506
00537          GO TO 730-BUILD-GOOD-LIFE.                               EL506
00538                                                                   EL506
00539  710-CHECK-FOR-AA.                                                EL506
00540                                                                   EL506
00541      IF SAVE-AA-RECORD-L = HIGH-VALUES                            EL506
00542          GO TO 760-BUILD-DUMMY-LIFE.                              EL506
00543                                                                   EL506
00544      MOVE SAVE-AA-RECORD-L       TO COMM-TABLE-DK.                EL506
00545      MOVE CLAS-I-BEN (SUB1)      TO CTR-BEN-CODE.                 EL506
00546      GO TO 790-WRITE-LIFE.                                        EL506
00547                                                                   EL506
00548  730-BUILD-GOOD-LIFE.                                             EL506
00549                                                                   EL506
00550      MOVE SAVE-RECORD (SUB2)     TO COMM-TABLE-DK.                EL506
00551      GO TO 790-WRITE-LIFE.                                        EL506
00552                                                                   EL506
00553  760-BUILD-DUMMY-LIFE.                                            EL506
00554                                                                   EL506
00555      MOVE DUMMY-COMM-TABLE       TO COMM-TABLE-DK.                EL506
00556      MOVE SAVE-CONTROL           TO CTR-CONTROL.                  EL506
00557      MOVE LIFE-OVERRIDE-L1       TO CTR-BEN-TYPE.                 EL506
00558      MOVE CLAS-I-BEN (SUB1)      TO CTR-BEN-CODE.                 EL506
00559                                                                   EL506
00560  790-WRITE-LIFE.                                                  EL506
00561                                                                   EL506
00562      PERFORM 900-WRITE-COMM-INDEX THRU 999-EXIT.                  EL506
00563      ADD +1 TO SUB1.                                              EL506
00564                                                                   EL506
00565  799-EXIT.                                                        EL506
00566      EXIT.                                                        EL506
00567                                                                   EL506
00568      EJECT                                                        EL506
00569                                                                   EL506
00570  800-FINALIZE.                                                    EL506
00571                                                                   EL506
00572      IF COMMISSION-OPEN = 'Y'                                     EL506
00573         CLOSE COMM-TABLE-DISK.                                    EL506
00574                                                                   EL506
00575      CLOSE PRNTR                                                  EL506
00576            ERCTBL ELCNTL.                                         EL506
00577                                                                   EL506
00578  800-CLOSE-OTHER. COPY ELCPRTCX.                                  EL506
00579                                                                   EL506
00580  800-EXIT.                                                        EL506
00581      EXIT.                                                        EL506
00582      EJECT                                                        EL506
00583                                                                   EL506
00584  900-WRITE-COMM-INDEX.                                            EL506
00585                                                                   EL506
00586      ADD  1  TO RECORD-COUNT.                                     EL506
00587                                                                   EL506
00588 ****** EXPAND TOTAL BENEFIT TO MILLIONS                           EL506
00589                                                                   EL506
00590      IF CTR-TOT-BEN (1) = +999999.99                              EL506
00591          MOVE +9999999.99       TO CTR-TOT-BEN (1).               EL506
00592      IF CTR-TOT-BEN (2) = +999999.99                              EL506
00593          MOVE +9999999.99       TO CTR-TOT-BEN (2).               EL506
00594      IF CTR-TOT-BEN (3) = +999999.99                              EL506
00595          MOVE +9999999.99       TO CTR-TOT-BEN (3).               EL506
00596                                                                   EL506
00597      WRITE COMM-TABLE-DK.                                         EL506
00598                                                                   EL506
00599      IF COMM-FILE-STATUS NOT = ZEROS                              EL506
00600          MOVE 'COMM-TABLE WRITE ERROR' TO WS-ABEND-MESSAGE        EL506
00601          MOVE COMM-FILE-STATUS         TO WS-ABEND-FILE-STATUS    EL506
00602          GO TO ABEND-PGM.                                         EL506
00603                                                                   EL506
00604  999-EXIT.                                                        EL506
00605      EXIT.                                                        EL506
00606      EJECT                                                        EL506
00607                                                                   EL506
00608  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL506
00609                                                                   EL506
00610                                                                   EL506
00611  WRITE-HEADINGS SECTION.                                          EL506
00612 ***************************************************************** EL506
00613 *                                                               * EL506
00614 *                            ELCWHS1.                           * EL506
00615 *                            VMOD=2.001                         * EL506
00616 *                                                               * EL506
00617 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          * EL506
00618 *****************************************************************.EL506
00619  WHS-010.                                                         EL506
00620      ADD +1  TO  WS-PAGE.                                         EL506
00621      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL506
00622      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL506
00623      MOVE ZERO                   TO  WS-LINE-COUNT.               EL506
00624                                                                   EL506
00625      MOVE WS-HEADING1            TO  PRT.                         EL506
00626      MOVE '1'                    TO  X.                           EL506
00627      PERFORM WRITE-PRINTER.                                       EL506
00628                                                                   EL506
00629      MOVE WS-HEADING2            TO  PRT.                         EL506
00630      MOVE ' '                    TO  X.                           EL506
00631      PERFORM WRITE-PRINTER.                                       EL506
00632                                                                   EL506
00633      MOVE WS-HEADING3            TO  PRT.                         EL506
00634      MOVE ' '                    TO  X.                           EL506
00635      PERFORM WRITE-PRINTER.                                       EL506
00636                                                                   EL506
00637      MOVE WS-HEADING4            TO  PRT.                         EL506
00638      MOVE ' '                    TO  X.                           EL506
00639      PERFORM WRITE-PRINTER.                                       EL506
00640                                                                   EL506
00641      MOVE +4                     TO WS-LINE-COUNT.                EL506
00642                                                                   EL506
00643  WHS-020.                                                         EL506
00644 ***************************************************************** EL506
00645 *                                                               * EL506
00646 *                            ELCWHS2.                           * EL506
00647 *                            VMOD=2.001                         * EL506
00648 *****************************************************************.EL506
00649      MOVE WS-SAVE-PRINT-RECORD   TO  PRT.                         EL506
00650      MOVE '-'                    TO  P-CTL.                          CL**2
00651                                                                   EL506
00652  WHS-EXIT.                                                        EL506
00653      EXIT.                                                        EL506
00654                                                                   EL506
00655                                                                   EL506
00656  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL506
00657  WPS-020. COPY ELCPRT2X.                                          EL506
00658                                                                   EL506
00659                                                                   EL506
00660  ABEND-PGM  SECTION.  COPY ELCABEND SUPPRESS.                     EL506
00661                                                                   EL506
