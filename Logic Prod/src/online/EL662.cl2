00001  IDENTIFICATION DIVISION.                                         03/06/96
00002                                                                   EL662
00003  PROGRAM-ID.                 EL662 .                                 LV004
00004 *              PROGRAM CONVERTED BY                                  CL**4
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**4
00006 *              CONVERSION DATE 02/14/96 07:25:52.                    CL**4
00007 *                            VMOD=2.004                              CL**4
00008 *                                                                 EL662
00008 *                                                                 EL662
00009 *AUTHOR.     LOGIC INC.                                              CL**4
00010 *            DALLAS, TEXAS.                                          CL**4
00011                                                                   EL662
00012 *DATE-COMPILED.                                                      CL**4
00013 *SECURITY.   *****************************************************   CL**4
00014 *            *                                                   *   CL**4
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**4
00016 *            *                                                   *   CL**4
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**4
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**4
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**4
00020 *            *                                                   *   CL**4
00021 *            *****************************************************   CL**4
00022                                                                   EL662
00023 *REMARKS. TRANSACTION - EXK2 - ACCOUNT/MONTH-END BALANCING REPORT.EL662
00024                                                                   EL662
00025      EJECT                                                        EL662
00026  ENVIRONMENT DIVISION.                                            EL662
00027  DATA DIVISION.                                                   EL662
00028  WORKING-STORAGE SECTION.                                         EL662
00029  01  LCP-TIME-OF-DAY-XX.                                             CL**4
00030      05  LCP-TIME-OF-DAY-68        PIC 9(6).                         CL**4
00031      05  FILLER                    PIC 99.                           CL**4
00032  01  LCP-CICS-TIME                 PIC 9(15).                        CL**4
00033  77  FILLER  PIC X(32)  VALUE '********************************'. EL662
00034  77  FILLER  PIC X(32)  VALUE '*    EL662 WORKING STORAGE     *'. EL662
00035  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.004 *********'.    CL**4
00036                                                                   EL662
00037  77  CLEN        PIC S9(4)  COMP    VALUE +1300.                  EL662
00038  77  CTR         PIC S99    COMP    VALUE +0.                     EL662
00039                                                                   EL662
00040  01  WS-DATE-AREA.                                                EL662
00041      03  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL662
00042      03  SAVE-BIN-DATE           PIC XX      VALUE SPACES.        EL662
00043      03  SAVE-DATE-ALPHA         PIC X(18)   VALUE SPACES.        EL662
00044                                                                   EL662
00045  01  WORK-AREAS.                                                  EL662
00046      03  WS-REPORT-ID            PIC X(6).                        EL662
00047      03  PRT-LINE2               PIC X       VALUE SPACES.        EL662
00048         88 LINE2-PRINTED                     VALUE 'X'.           EL662
00049      03  GRAND-TOTAL-SW          PIC X       VALUE SPACES.        EL662
00050         88 NO-GRAND-TOTALS                   VALUE 'X'.           EL662
00051      03  PGM-NAME                PIC X(8)    VALUE SPACES.        EL662
00052      03  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     EL662
00053      03  EMI-LINE1               PIC X(72).                       EL662
00054      03  WS-NEXT-TRAN            PIC X(4).                        EL662
00055      03  WS-TERMINAL-ID.                                          EL662
00056          05  WS-TERM-PREFIX      PIC XX.                          EL662
00057          05  FILLER              PIC XX.                          EL662
00058      03  PRT-CNT                 PIC S9(3)   VALUE +99  COMP-3.   EL662
00059      03  WS-LINE-NUMBER          PIC S9(7)   VALUE ZERO COMP-3.   EL662
00060      03  WS-PAGE                 PIC S9(5)   VALUE ZERO COMP-3.   EL662
00061      03  WS-REPORT-SW            PIC S9      VALUE ZERO COMP-3.   EL662
00062      03  WS-PRINT-SW             PIC S9      VALUE ZERO COMP-3.   EL662
00063      03  WS-PROCESS-SW           PIC X       VALUE 'N'.           EL662
00064          88  WS-NO-MAINTENANCE               VALUE 'N'.           EL662
00065      03  REPT-FILE-ID            PIC X(8)    VALUE 'ELREPT'.      EL662
00066      03  PNDB2-FILE-ID           PIC X(8)    VALUE 'ERPNDB2'.     EL662
00067      03  CNTL-ID                 PIC X(8)    VALUE 'ELCNTL'.      EL662
00068      03  WS-DATE.                                                 EL662
00069          05  WS-YR               PIC XX.                          EL662
00070          05  WS-MO               PIC XX.                          EL662
00071          05  WS-DA               PIC XX.                          EL662
00072                                                                   EL662
00073                           COPY ELCREPT.                              CL**3
00074      EJECT                                                        EL662
00075                           COPY ELCDATE.                              CL**3
00076      EJECT                                                        EL662
00077                                                                   EL662
00078  01  HDR-LINES.                                                   EL662
00079      03  HDR-1.                                                   EL662
00080          05  FILLER          PIC X(32)   VALUE SPACES.            EL662
00081          05  FILLER          PIC X(33)   VALUE                    EL662
00082                  'ACCOUNT BALANCING'.                             EL662
00083          05  FILLER          PIC XXX     VALUE SPACES.            EL662
00084          05  FILLER          PIC X(8)    VALUE 'EL - 662'.        EL662
00085                                                                   EL662
00086      03  HDR-2.                                                   EL662
00087          05  FILLER          PIC X(30)   VALUE SPACES.            EL662
00088          05  H2-COMP         PIC X(30)   VALUE 'LOGIC, INC.'.     EL662
00089          05  FILLER          PIC X(8)    VALUE SPACES.            EL662
00090          05  H2-DATE         PIC X(8).                            EL662
00091                                                                   EL662
00092      03  HDR-3.                                                   EL662
00093          05  FILLER          PIC X(32)   VALUE SPACES.            EL662
00094          05  FILLER          PIC X(7)    VALUE 'AS OF'.           EL662
00095          05  H3-MONTH-END    PIC X(8)    VALUE SPACES.            EL662
00096          05  FILLER          PIC X(18)   VALUE SPACES.            EL662
00097          05  FILLER          PIC X(5)    VALUE 'PAGE '.           EL662
00098          05  H3-PAGE         PIC ZZ,ZZ9.                          EL662
00099                                                                   EL662
00100      03  HDR-4.                                                   EL662
00101          05  FILLER          PIC X(26)   VALUE                    EL662
00102          'CARR GROUPING ST   ACCOUNT'.                            EL662
00103                                                                   EL662
00104      03  HDR-5.                                                   EL662
00105          05  FILLER          PIC X       VALUE SPACES.            EL662
00106          05  H4-CARRIER      PIC X       VALUE SPACES.            EL662
00107          05  FILLER          PIC X(4)    VALUE SPACES.            EL662
00108          05  H4-GROUPING     PIC X(6)    VALUE SPACES.            EL662
00109          05  FILLER          PIC XX      VALUE SPACES.            EL662
00110          05  H4-STATE        PIC XX      VALUE SPACES.            EL662
00111          05  FILLER          PIC XX      VALUE SPACES.            EL662
00112          05  H4-ACCOUNT      PIC X(10)   VALUE SPACES.            EL662
00113                                                                   EL662
00114      03  HDR-6.                                                   EL662
00115          05  FILLER          PIC X(34)   VALUE SPACES.            EL662
00116          05  FILLER          PIC X(43)   VALUE                    EL662
00117             ' LIFE      ALT     LIFE      AH        AH'.          EL662
00118                                                                   EL662
00119      03  HDR-7.                                                   EL662
00120          05  FILLER          PIC X(34)   VALUE                    EL662
00121             'BATCH  SEQ  TYPE  CERT NO.'.                         EL662
00122          05  FILLER          PIC X(43)   VALUE                    EL662
00123             'PREMIUM  PREMIUM  REFUND   PREMIUM   REFUND'.        EL662
00124                                                                   EL662
00125  01  DATA-1.                                                      EL662
00126      03  D-BATCH             PIC X(6)            VALUE SPACES.    EL662
00127      03  FILLER              PIC X               VALUE SPACE.     EL662
00128      03  D-SEQ               PIC X(6).                            EL662
00129      03  D-TYPE              PIC XXX.                             EL662
00130      03  FILLER              PIC XX              VALUE SPACE.     EL662
00131      03  D-CERT              PIC X(14).                           EL662
00132      03  D-LFPREM            PIC Z(5).ZZ-.                        EL662
00133      03  D-ALPREM            PIC Z(5).ZZ-.                        EL662
00134      03  D-LFRFND            PIC Z(5).ZZ-.                        EL662
00135      03  D-AHPREM            PIC Z(5).ZZ-.                        EL662
00136      03  D-AHRFND            PIC Z(5).ZZ-.                        EL662
00137                                                                   EL662
00138  01  TOTAL-LINE.                                                  EL662
00139      03  TOTAL-TITLE         PIC X(31).                           EL662
00140      03  T-LFPREM            PIC Z(5).99-.                        EL662
00141      03  T-ALPREM            PIC Z(5).99-.                        EL662
00142      03  T-LFRFND            PIC Z(5).99-.                        EL662
00143      03  T-AHPREM            PIC Z(5).99-.                        EL662
00144      03  T-AHRFND            PIC Z(5).99-.                        EL662
00145                                                                   EL662
00146  01  PROCESSABLE-TOTALS.                                          EL662
00147      03  TOT-LFPREM-P        PIC S9(9)V99  COMP-3   VALUE +0.     EL662
00148      03  TOT-ALPREM-P        PIC S9(9)V99  COMP-3   VALUE +0.     EL662
00149      03  TOT-LFRFND-P        PIC S9(9)V99  COMP-3   VALUE +0.     EL662
00150      03  TOT-AHPREM-P        PIC S9(9)V99  COMP-3   VALUE +0.     EL662
00151      03  TOT-AHRFND-P        PIC S9(9)V99  COMP-3   VALUE +0.     EL662
00152                                                                   EL662
00153  01  NON-PROCESSABLE-TOTALS.                                      EL662
00154      03  TOT-LFPREM-N        PIC S9(9)V99  COMP-3   VALUE +0.     EL662
00155      03  TOT-ALPREM-N        PIC S9(9)V99  COMP-3   VALUE +0.     EL662
00156      03  TOT-LFRFND-N        PIC S9(9)V99  COMP-3   VALUE +0.     EL662
00157      03  TOT-AHPREM-N        PIC S9(9)V99  COMP-3   VALUE +0.     EL662
00158      03  TOT-AHRFND-N        PIC S9(9)V99  COMP-3   VALUE +0.     EL662
00159                                                                   EL662
00160  01  GRAND-TOTALS.                                                EL662
00161      03  TOT-LFPREM          PIC S9(9)V99  COMP-3   VALUE +0.     EL662
00162      03  TOT-ALPREM          PIC S9(9)V99  COMP-3   VALUE +0.     EL662
00163      03  TOT-LFRFND          PIC S9(9)V99  COMP-3   VALUE +0.     EL662
00164      03  TOT-AHPREM          PIC S9(9)V99  COMP-3   VALUE +0.     EL662
00165      03  TOT-AHRFND          PIC S9(9)V99  COMP-3   VALUE +0.     EL662
00166                                                                   EL662
00167  01  ACCESS-KEYS.                                                 EL662
00168      03  ELCNTL-KEY.                                              EL662
00169          05  CNTL-COMP-ID         PIC XXX.                        EL662
00170          05  CNTL-REC-TYPE        PIC X.                          EL662
00171          05  CNTL-ACCESS          PIC X(4).                       EL662
00172          05  CNTL-SEQ-NO          PIC S9(4)    COMP.              EL662
00173                                                                   EL662
00174      03  ERPNDB2-KEY.                                             EL662
00175          05  PNDB-COMP-CD         PIC X.                          EL662
00176          05  PNDB-CARRIER         PIC X.                          EL662
00177          05  PNDB-GROUPING        PIC X(6).                       EL662
00178          05  PNDB-STATE           PIC XX.                         EL662
00179          05  PNDB-ACCOUNT         PIC X(10).                      EL662
00180          05  PNDB-CERT-EFFDT      PIC XX.                         EL662
00181          05  PNDB-CERT-NO         PIC X(11).                      EL662
00182          05  PNDB-ALT-SEQ-NO      PIC S9(4)    COMP.              EL662
00183                                                                   EL662
00184      EJECT                                                        EL662
00185                                      COPY ELCAID.                    CL**3
00186  01  PF-AID REDEFINES DFHAID.                                     EL662
00187      05  FILLER                      PIC X(8).                    EL662
00188      05  PF-VALUES  OCCURS 24        PIC X.                       EL662
00189      EJECT                                                        EL662
00190                                      COPY ELCINTF.                   CL**3
00191           COPY ELC630PI.                                          EL662
00192      EJECT                                                        EL662
00193  LINKAGE SECTION.                                                 EL662
00194  01  DFHCOMMAREA                     PIC X(1300).                 EL662
00195 *01 PARM-LIST .                                                      CL**4
00196 *    02  FILLER              PIC S9(8)   COMP.                       CL**4
00197 *    02  PNDB2               PIC S9(8)   COMP.                       CL**4
00198 *    02  CNTL                PIC S9(8)   COMP.                       CL**4
00199                                                                   EL662
00200      EJECT                                                        EL662
00201                           COPY ERCPNDB.                              CL**3
00202                                                                   EL662
00203      EJECT                                                        EL662
00204                           COPY ELCCNTL.                              CL**3
00205      EJECT                                                        EL662
00206                                                                   EL662
00207  PROCEDURE DIVISION.                                              EL662
00208                                                                   EL662
00209      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL662
00210      MOVE '5'                   TO DC-OPTION-CODE.                EL662
00211      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL662
00212      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE                     EL662
00213                                     H2-DATE.                      EL662
00214      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL662
00215      MOVE DC-GREG-DATE-1-ALPHA  TO  SAVE-DATE-ALPHA.              EL662
00216                                                                   EL662
00217      MOVE DFHCOMMAREA           TO  PROGRAM-INTERFACE-BLOCK.      EL662
00218      MOVE 'EL662'               TO  WS-REPORT-ID.                 EL662
00219                                                                   EL662
00220  1000-START.                                                      EL662
00221      EXEC CICS  HANDLE CONDITION                                  EL662
00222             ERROR    (8800-ABEND)                                 EL662
00223             PGMIDERR (8900-PGMIDERR)                              EL662
00224      END-EXEC.                                                    EL662
00225                                                                   EL662
00226  2000-RECEIVE.                                                    EL662
00227      EXEC CICS RETRIEVE                                           EL662
00228          INTO   (PROGRAM-INTERFACE-BLOCK)                         EL662
00229          LENGTH (CLEN)                                            EL662
00230      END-EXEC.                                                    EL662
00231                                                                   EL662
00232      MOVE PI-NB-MONTH-END-DT    TO DC-BIN-DATE-1.                 EL662
00233      MOVE ' '                   TO DC-OPTION-CODE.                EL662
00234      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL662
00235      MOVE DC-GREG-DATE-1-EDIT   TO  H3-MONTH-END.                 EL662
00236                                                                   EL662
00237      MOVE PI-SAV-CARRIER        TO  H4-CARRIER.                   EL662
00238      MOVE PI-SAV-GROUPING       TO  H4-GROUPING.                  EL662
00239      MOVE PI-SAV-STATE          TO  H4-STATE.                     EL662
00240      MOVE PI-SAV-ACCOUNT        TO  H4-ACCOUNT.                   EL662
00241                                                                   EL662
00242  2000-CHECK-IN-PROGRESS.                                          EL662
00243      EXEC CICS  HANDLE CONDITION                                  EL662
00244             NOTFND   (2000-WRITE-INITIAL-TRAILER)                 EL662
00245      END-EXEC.                                                    EL662
00246                                                                   EL662
00247      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL662
00248      MOVE 'RF'                   TO  RF-RECORD-ID.                EL662
00249      MOVE '2'                    TO  RF-RECORD-TYPE.              EL662
00250      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                EL662
00251      MOVE ZEROS                  TO  RF-LINE-NUMBER.              EL662
00252                                                                   EL662
00253      EXEC CICS READ                                               EL662
00254          DATASET (REPT-FILE-ID)                                   EL662
00255          INTO    (REPORT-SAVE-FILE)                               EL662
00256          RIDFLD  (RF-CONTROL-PRIMARY)                             EL662
00257      END-EXEC.                                                    EL662
00258                                                                   EL662
00259 ********IF RECORD FOUND, THEN ANOTHER REPORT HAS ALREADY          EL662
00260 ********BEEN STARTED.  IF PREVIOUS REPORT ABENDED AND DIDN'T      EL662
00261 ********COMPLETE, THEN OPERATOR MUST PURGE REPORT AND CREATE      EL662
00262 ********A NEW ONE.                                                EL662
00263      GO TO 9999-RETURN-CICS.                                      EL662
00264                                                                   EL662
00265  2000-WRITE-INITIAL-TRAILER.                                      EL662
00266      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL662
00267      MOVE 'RF'                   TO  RF-RECORD-ID.                EL662
00268      MOVE '2'                    TO  RF-RECORD-TYPE.              EL662
00269      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                EL662
00270      MOVE ZEROS                  TO  RF-LINE-NUMBER.              EL662
00271                                                                   EL662
00272      MOVE SPACES                 TO  RF-TRAILER-RECORD.           EL662
00273      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL**4
00274      END-EXEC                                                        CL**4
00275      EXEC CICS FORMATTIME                                            CL**4
00276                ABSTIME(LCP-CICS-TIME)                                CL**4
00277                TIME(LCP-TIME-OF-DAY-XX)                              CL**4
00278      END-EXEC                                                        CL**4
00279      MOVE  LCP-TIME-OF-DAY-68 TO RF-PRINT-HH-MM-SS.                  CL**4
00280      MOVE 'STARTED'              TO  RF-CURRENT-DATE.             EL662
00281                                                                   EL662
00282      EXEC CICS WRITE                                              EL662
00283          DATASET (REPT-FILE-ID)                                   EL662
00284          FROM    (REPORT-SAVE-FILE)                               EL662
00285          RIDFLD  (RF-CONTROL-PRIMARY)                             EL662
00286      END-EXEC.                                                    EL662
00287                                                                   EL662
00288  2100-DELETE-REC.                                                 EL662
00289      MOVE 1 TO RF-LINE-NUMBER.                                    EL662
00290      EXEC CICS  HANDLE CONDITION                                  EL662
00291             NOTFND   (2300-DELETE-REC)                            EL662
00292      END-EXEC.                                                    EL662
00293                                                                   EL662
00294  2200-DELETE-1.                                                   EL662
00295      MOVE PI-COMPANY-CD TO RF-COMPANY-CD.                         EL662
00296      MOVE 'RF'          TO RF-RECORD-ID.                          EL662
00297      MOVE '1'           TO RF-RECORD-TYPE.                        EL662
00298      MOVE WS-REPORT-ID  TO RF-REPORT-ID.                          EL662
00299                                                                   EL662
00300      EXEC CICS DELETE                                             EL662
00301          DATASET   (REPT-FILE-ID)                                 EL662
00302          RIDFLD    (RF-CONTROL-PRIMARY)                           EL662
00303          KEYLENGTH (11)                                           EL662
00304      END-EXEC.                                                    EL662
00305                                                                   EL662
00306      ADD 1 TO RF-LINE-NUMBER.                                     EL662
00307      GO TO 2200-DELETE-1.                                         EL662
00308                                                                   EL662
00309  2300-DELETE-REC.                                                 EL662
00310      EXEC CICS  HANDLE CONDITION                                  EL662
00311             NOTFND   (3000-START-BROWSE)                          EL662
00312      END-EXEC.                                                    EL662
00313                                                                   EL662
00314  2400-DELETE-2.                                                   EL662
00315      MOVE PI-COMPANY-CD TO RF-COMPANY-CD.                         EL662
00316      MOVE 'RF'          TO RF-RECORD-ID.                          EL662
00317      MOVE '2'           TO RF-RECORD-TYPE.                        EL662
00318      MOVE WS-REPORT-ID  TO RF-REPORT-ID.                          EL662
00319                                                                   EL662
00320      EXEC CICS DELETE                                             EL662
00321          DATASET   (REPT-FILE-ID)                                 EL662
00322          RIDFLD    (RF-CONTROL-PRIMARY)                           EL662
00323          KEYLENGTH (11)                                           EL662
00324      END-EXEC.                                                    EL662
00325                                                                   EL662
00326      ADD 1 TO RF-LINE-NUMBER.                                     EL662
00327      GO TO 2400-DELETE-2.                                         EL662
00328                                                                   EL662
00329  3000-START-BROWSE.                                               EL662
00330      MOVE 0 TO RF-LINE-NUMBER.                                    EL662
00331                                                                   EL662
00332      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL662
00333      MOVE SPACES                 TO CNTL-ACCESS.                  EL662
00334      MOVE '1'                    TO CNTL-REC-TYPE.                EL662
00335      MOVE +0                     TO CNTL-SEQ-NO.                  EL662
00336                                                                   EL662
00337      PERFORM 9000-READ-CONTROL THRU 9000-EXIT.                    EL662
00338                                                                   EL662
00339      MOVE CF-CL-MAIL-TO-NAME TO H2-COMP.                          EL662
00340                                                                   EL662
00341      EXEC CICS  HANDLE CONDITION                                  EL662
00342             NOTFND   (9999-RETURN-CICS)                           EL662
00343      END-EXEC.                                                    EL662
00344                                                                   EL662
00345      MOVE SPACES          TO ERPNDB2-KEY.                         EL662
00346      MOVE PI-COMPANY-CD   TO PNDB-COMP-CD.                        EL662
00347      MOVE PI-SAV-ACCOUNT  TO PNDB-ACCOUNT.                        EL662
00348                                                                   EL662
00349      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL662
00350          MOVE PI-SAV-CARRIER  TO PNDB-CARRIER                     EL662
00351          MOVE PI-SAV-GROUPING TO PNDB-GROUPING                    EL662
00352          MOVE PI-SAV-STATE    TO PNDB-STATE.                      EL662
00353                                                                   EL662
00354      IF CARR-ST-ACCNT-CNTL                                        EL662
00355          MOVE PI-SAV-CARRIER  TO PNDB-CARRIER                     EL662
00356          MOVE PI-SAV-STATE    TO PNDB-STATE.                      EL662
00357                                                                   EL662
00358      IF ST-ACCNT-CNTL                                             EL662
00359          MOVE PI-SAV-STATE    TO PNDB-STATE.                      EL662
00360                                                                   EL662
00361      IF CARR-ACCNT-CNTL                                           EL662
00362          MOVE PI-SAV-CARRIER  TO PNDB-CARRIER.                    EL662
00363                                                                   EL662
00364      EXEC CICS STARTBR                                            EL662
00365           DATASET  (PNDB2-FILE-ID)                                EL662
00366           RIDFLD   (ERPNDB2-KEY)                                  EL662
00367           GTEQ                                                    EL662
00368      END-EXEC.                                                    EL662
00369                                                                   EL662
00370      EXEC CICS  HANDLE CONDITION                                  EL662
00371             ENDFILE  (5000-EOJ)                                   EL662
00372      END-EXEC.                                                    EL662
00373                                                                   EL662
00374  4000-READNEXT.                                                   EL662
00375      EXEC CICS READNEXT                                           EL662
00376           DATASET  (PNDB2-FILE-ID)                                EL662
00377           SET      (ADDRESS OF PENDING-BUSINESS)                     CL**4
00378           RIDFLD   (ERPNDB2-KEY)                                  EL662
00379      END-EXEC.                                                    EL662
00380                                                                   EL662
00381      IF PI-COMPANY-CD  NOT = PB-COMPANY-CD-A1                     EL662
00382          GO TO 5000-EOJ.                                          EL662
00383                                                                   EL662
00384      IF PI-SAV-ACCOUNT NOT = PB-ACCOUNT                           EL662
00385          GO TO 5000-EOJ.                                          EL662
00386                                                                   EL662
00387      IF PB-ISSUE OR PB-CANCELLATION                               EL662
00388          NEXT SENTENCE                                            EL662
00389        ELSE                                                       EL662
00390          GO TO 4000-READNEXT.                                     EL662
00391                                                                   EL662
00392      IF PI-NB-MONTH-END-DT = PB-CREDIT-SELECT-DT                  EL662
00393           NEXT SENTENCE                                           EL662
00394         ELSE                                                      EL662
00395           GO TO 4000-READNEXT.                                    EL662
00396                                                                   EL662
00397      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL662
00398      IF PI-SAV-CARRIER     = PB-CARRIER  AND                      EL662
00399         PI-SAV-GROUPING    = PB-GROUPING AND                      EL662
00400         PI-SAV-STATE       = PB-STATE                             EL662
00401           NEXT SENTENCE                                           EL662
00402         ELSE                                                      EL662
00403           GO TO 4000-READNEXT.                                    EL662
00404                                                                   EL662
00405      IF CARR-ST-ACCNT-CNTL                                        EL662
00406      IF PI-SAV-CARRIER     = PB-CARRIER AND                       EL662
00407         PI-SAV-STATE       = PB-STATE                             EL662
00408           NEXT SENTENCE                                           EL662
00409         ELSE                                                      EL662
00410           GO TO 4000-READNEXT.                                    EL662
00411                                                                   EL662
00412      IF ST-ACCNT-CNTL                                             EL662
00413      IF PI-SAV-STATE       = PB-STATE                             EL662
00414           NEXT SENTENCE                                           EL662
00415         ELSE                                                      EL662
00416           GO TO 4000-READNEXT.                                    EL662
00417                                                                   EL662
00418      IF CARR-ACCNT-CNTL                                           EL662
00419      IF PI-SAV-CARRIER     = PB-CARRIER                           EL662
00420           NEXT SENTENCE                                           EL662
00421         ELSE                                                      EL662
00422           GO TO 4000-READNEXT.                                    EL662
00423                                                                   EL662
00424      MOVE PB-ENTRY-BATCH               TO D-BATCH.                EL662
00425      MOVE PB-BATCH-SEQ-NO              TO D-SEQ.                  EL662
00426      MOVE PB-CERT-NO                   TO D-CERT.                 EL662
00427                                                                   EL662
00428      IF PB-ISSUE                                                  EL662
00429          PERFORM 4100-ISSUE  THRU 4100-EXIT                       EL662
00430        ELSE                                                       EL662
00431          PERFORM 4200-CANCEL THRU 4200-EXIT.                      EL662
00432                                                                   EL662
00433      IF PRT-CNT GREATER 50                                        EL662
00434          PERFORM 6500-HDR-RTN THRU 6500-EXIT.                     EL662
00435                                                                   EL662
00436      MOVE DATA-1         TO RF-DATA-133.                          EL662
00437      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL662
00438      ADD +1   TO PRT-CNT.                                         EL662
00439      GO TO 4000-READNEXT.                                         EL662
00440                                                                   EL662
00441  4100-ISSUE.                                                      EL662
00442      MOVE 'ISS'                    TO D-TYPE.                     EL662
00443      MOVE PB-I-LF-PREMIUM-AMT      TO D-LFPREM.                   EL662
00444      MOVE PB-I-LF-ALT-PREMIUM-AMT  TO D-ALPREM.                   EL662
00445      MOVE PB-I-AH-PREMIUM-AMT      TO D-AHPREM.                   EL662
00446      MOVE ZEROS                    TO D-LFRFND                    EL662
00447                                       D-AHRFND.                   EL662
00448      IF PB-FATAL-ERRORS OR                                        EL662
00449         PB-UNFORCED-ERRORS                                        EL662
00450           ADD PB-I-LF-PREMIUM-AMT       TO TOT-LFPREM-N           EL662
00451           ADD PB-I-LF-ALT-PREMIUM-AMT   TO TOT-ALPREM-N           EL662
00452           ADD PB-I-AH-PREMIUM-AMT       TO TOT-AHPREM-N           EL662
00453         ELSE                                                      EL662
00454           ADD PB-I-LF-PREMIUM-AMT       TO TOT-LFPREM-P           EL662
00455           ADD PB-I-LF-ALT-PREMIUM-AMT   TO TOT-ALPREM-P           EL662
00456           ADD PB-I-AH-PREMIUM-AMT       TO TOT-AHPREM-P.          EL662
00457                                                                   EL662
00458  4100-EXIT.                                                       EL662
00459       EXIT.                                                       EL662
00460                                                                   EL662
00461  4200-CANCEL.                                                     EL662
00462      MOVE 'CAN'                    TO D-TYPE.                     EL662
00463      MOVE PB-C-LF-CANCEL-AMT       TO D-LFRFND.                   EL662
00464      MOVE PB-C-AH-CANCEL-AMT       TO D-AHRFND.                   EL662
00465      MOVE ZEROS                    TO D-LFPREM                    EL662
00466                                       D-ALPREM                    EL662
00467                                       D-AHPREM.                   EL662
00468      IF PB-FATAL-ERRORS OR                                        EL662
00469         PB-UNFORCED-ERRORS                                        EL662
00470           ADD PB-C-LF-CANCEL-AMT        TO TOT-LFRFND-N           EL662
00471           ADD PB-C-AH-CANCEL-AMT        TO TOT-AHRFND-N           EL662
00472         ELSE                                                      EL662
00473           ADD PB-C-LF-CANCEL-AMT        TO TOT-LFRFND-P           EL662
00474           ADD PB-C-AH-CANCEL-AMT        TO TOT-AHRFND-P.          EL662
00475                                                                   EL662
00476  4200-EXIT.                                                       EL662
00477       EXIT.                                                       EL662
00478  EJECT                                                            EL662
00479  5000-EOJ.                                                        EL662
00480      MOVE ' '                TO RF-CTL-CHAR-133.                  EL662
00481      MOVE SPACES             TO RF-DATA-133.                      EL662
00482      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL662
00483                                                                   EL662
00484  5010-PRINT-PROCESSABLE.                                          EL662
00485      IF TOT-LFPREM-P = ZERO AND                                   EL662
00486         TOT-ALPREM-P = ZERO AND                                   EL662
00487         TOT-LFRFND-P = ZERO AND                                   EL662
00488         TOT-AHPREM-P = ZERO AND                                   EL662
00489         TOT-AHRFND-P = ZERO                                       EL662
00490            MOVE 'X'           TO GRAND-TOTAL-SW                   EL662
00491            GO TO 5020-PRINT-NON-PROCESSABLE.                      EL662
00492                                                                   EL662
00493      MOVE 'PROCESSABLE TOTALS..........'                          EL662
00494                              TO TOTAL-TITLE.                      EL662
00495      MOVE TOT-LFPREM-P       TO T-LFPREM.                         EL662
00496      MOVE TOT-ALPREM-P       TO T-ALPREM.                         EL662
00497      MOVE TOT-LFRFND-P       TO T-LFRFND.                         EL662
00498      MOVE TOT-AHPREM-P       TO T-AHPREM.                         EL662
00499      MOVE TOT-AHRFND-P       TO T-AHRFND.                         EL662
00500                                                                   EL662
00501      MOVE ' '                TO RF-CTL-CHAR-133.                  EL662
00502      MOVE SPACES             TO RF-DATA-133.                      EL662
00503      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL662
00504                                                                   EL662
00505      MOVE ' '                TO RF-CTL-CHAR-133.                  EL662
00506      MOVE TOTAL-LINE         TO RF-DATA-133.                      EL662
00507      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL662
00508                                                                   EL662
00509  5020-PRINT-NON-PROCESSABLE.                                      EL662
00510      IF TOT-LFPREM-N = ZERO AND                                   EL662
00511         TOT-ALPREM-N = ZERO AND                                   EL662
00512         TOT-LFRFND-N = ZERO AND                                   EL662
00513         TOT-AHPREM-N = ZERO AND                                   EL662
00514         TOT-AHRFND-N = ZERO                                       EL662
00515            MOVE 'X'           TO GRAND-TOTAL-SW                   EL662
00516            GO TO 5030-PRINT-GRAND-TOTALS.                         EL662
00517                                                                   EL662
00518      MOVE 'NON-PROCESSABLE TOTALS......'                          EL662
00519                              TO TOTAL-TITLE.                      EL662
00520      MOVE TOT-LFPREM-N       TO T-LFPREM.                         EL662
00521      MOVE TOT-ALPREM-N       TO T-ALPREM.                         EL662
00522      MOVE TOT-LFRFND-N       TO T-LFRFND.                         EL662
00523      MOVE TOT-AHPREM-N       TO T-AHPREM.                         EL662
00524      MOVE TOT-AHRFND-N       TO T-AHRFND.                         EL662
00525                                                                   EL662
00526      MOVE ' '                TO RF-CTL-CHAR-133.                  EL662
00527      MOVE SPACES             TO RF-DATA-133.                      EL662
00528      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL662
00529                                                                   EL662
00530      MOVE ' '                TO RF-CTL-CHAR-133.                  EL662
00531      MOVE TOTAL-LINE         TO RF-DATA-133.                      EL662
00532      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL662
00533                                                                   EL662
00534  5030-PRINT-GRAND-TOTALS.                                         EL662
00535      IF NO-GRAND-TOTALS                                           EL662
00536          GO TO 5050-END-OF-TOTALS.                                EL662
00537                                                                   EL662
00538      COMPUTE TOT-LFPREM = TOT-LFPREM-N + TOT-LFPREM-P.            EL662
00539      COMPUTE TOT-ALPREM = TOT-ALPREM-N + TOT-ALPREM-P.            EL662
00540      COMPUTE TOT-LFRFND = TOT-LFRFND-N + TOT-LFRFND-P.            EL662
00541      COMPUTE TOT-AHPREM = TOT-AHPREM-N + TOT-AHPREM-P.            EL662
00542      COMPUTE TOT-AHRFND = TOT-AHRFND-N + TOT-AHRFND-P.            EL662
00543                                                                   EL662
00544      MOVE 'GRAND-TOTALS................'                          EL662
00545                              TO TOTAL-TITLE.                      EL662
00546      MOVE TOT-LFPREM         TO T-LFPREM.                         EL662
00547      MOVE TOT-ALPREM         TO T-ALPREM.                         EL662
00548      MOVE TOT-LFRFND         TO T-LFRFND.                         EL662
00549      MOVE TOT-AHPREM         TO T-AHPREM.                         EL662
00550      MOVE TOT-AHRFND         TO T-AHRFND.                         EL662
00551                                                                   EL662
00552      MOVE ' '                TO RF-CTL-CHAR-133.                  EL662
00553      MOVE SPACES             TO RF-DATA-133.                      EL662
00554      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL662
00555                                                                   EL662
00556      MOVE ' '                TO RF-CTL-CHAR-133.                  EL662
00557      MOVE '0'                TO RF-CTL-CHAR-133.                  EL662
00558      MOVE TOTAL-LINE         TO RF-DATA-133.                      EL662
00559      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL662
00560                                                                   EL662
00561  5050-END-OF-TOTALS.                                              EL662
00562      MOVE '1'                TO RF-CTL-CHAR-133.                  EL662
00563      MOVE SPACES             TO RF-DATA-133.                      EL662
00564      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL662
00565                                                                   EL662
00566      EXEC CICS ENDBR                                              EL662
00567           DATASET  (PNDB2-FILE-ID)                                EL662
00568      END-EXEC.                                                    EL662
00569                                                                   EL662
00570      MOVE PI-COMPANY-CD   TO RF-COMPANY-CD.                       EL662
00571      MOVE 'RF'            TO RF-RECORD-ID.                        EL662
00572      MOVE '2'             TO RF-RECORD-TYPE.                      EL662
00573      MOVE WS-REPORT-ID    TO RF-REPORT-ID.                        EL662
00574      MOVE ZEROS           TO RF-LINE-NUMBER.                      EL662
00575                                                                   EL662
00576      EXEC CICS DELETE                                             EL662
00577          DATASET   (REPT-FILE-ID)                                 EL662
00578          RIDFLD    (RF-CONTROL-PRIMARY)                           EL662
00579          KEYLENGTH (11)                                           EL662
00580      END-EXEC.                                                    EL662
00581                                                                   EL662
00582      PERFORM 8999-WRITE-TRAILER.                                  EL662
00583                                                                   EL662
00584      GO TO 9999-RETURN-CICS.                                      EL662
00585                                                                   EL662
00586  EJECT                                                            EL662
00587  6500-HDR-RTN.                                                    EL662
00588      ADD +1       TO WS-PAGE.                                     EL662
00589      MOVE WS-PAGE TO H3-PAGE.                                     EL662
00590      MOVE '1'     TO RF-CTL-CHAR-133.                             EL662
00591      MOVE HDR-1   TO RF-DATA-133.                                 EL662
00592      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL662
00593      MOVE ' '     TO RF-CTL-CHAR-133.                             EL662
00594      MOVE HDR-2   TO RF-DATA-133.                                 EL662
00595      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL662
00596      MOVE ' '     TO RF-CTL-CHAR-133.                             EL662
00597      MOVE HDR-3   TO RF-DATA-133.                                 EL662
00598      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL662
00599      MOVE ' '     TO RF-CTL-CHAR-133.                             EL662
00600      MOVE SPACES  TO RF-DATA-133.                                 EL662
00601      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL662
00602      MOVE ' '     TO RF-CTL-CHAR-133.                             EL662
00603      MOVE HDR-4   TO RF-DATA-133.                                 EL662
00604      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL662
00605      MOVE ' '     TO RF-CTL-CHAR-133.                             EL662
00606      MOVE HDR-5   TO RF-DATA-133.                                 EL662
00607      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL662
00608      MOVE ' '     TO RF-CTL-CHAR-133.                             EL662
00609      MOVE HDR-6   TO RF-DATA-133.                                 EL662
00610      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL662
00611      MOVE ' '     TO RF-CTL-CHAR-133.                             EL662
00612      MOVE HDR-7   TO RF-DATA-133.                                 EL662
00613      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL662
00614      MOVE ' '     TO RF-CTL-CHAR-133.                             EL662
00615      MOVE SPACES  TO RF-DATA-133.                                 EL662
00616      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL662
00617      MOVE +0      TO PRT-CNT.                                     EL662
00618                                                                   EL662
00619  6500-EXIT.                                                       EL662
00620      EXIT.                                                        EL662
00621                                                                   EL662
00622  7000-PRT-LINE.                                                   EL662
00623      MOVE PI-COMPANY-CD  TO RF-COMPANY-CD.                        EL662
00624      MOVE 'RF'           TO RF-RECORD-ID.                         EL662
00625      MOVE '1'            TO RF-RECORD-TYPE.                       EL662
00626      MOVE WS-REPORT-ID   TO RF-REPORT-ID.                         EL662
00627      ADD 1               TO WS-LINE-NUMBER.                       EL662
00628      MOVE WS-LINE-NUMBER TO RF-LINE-NUMBER.                       EL662
00629                                                                   EL662
00630      EXEC CICS WRITE                                              EL662
00631          DATASET (REPT-FILE-ID)                                   EL662
00632          FROM    (REPORT-SAVE-FILE)                               EL662
00633          RIDFLD  (RF-CONTROL-PRIMARY)                             EL662
00634      END-EXEC.                                                    EL662
00635                                                                   EL662
00636  7000-EXIT.                                                       EL662
00637       EXIT.                                                       EL662
00638                                                                   EL662
00639  8500-DATE-CONVERT.                                               EL662
00640      MOVE LINK-ELDATCV           TO PGM-NAME.                     EL662
00641                                                                   EL662
00642      EXEC CICS LINK                                               EL662
00643          PROGRAM    (PGM-NAME)                                    EL662
00644          COMMAREA   (DATE-CONVERSION-DATA)                        EL662
00645          LENGTH     (DC-COMM-LENGTH)                              EL662
00646      END-EXEC.                                                    EL662
00647                                                                   EL662
00648  8500-EXIT.                                                       EL662
00649      EXIT.                                                        EL662
00650                                                                   EL662
00651  8800-ABEND.                                                      EL662
00652      MOVE DFHEIBLK TO EMI-LINE1.                                  EL662
00653      EXEC CICS LINK                                               EL662
00654          PROGRAM   ('EL004')                                      EL662
00655          COMMAREA  (EMI-LINE1)                                    EL662
00656          LENGTH    (72)                                           EL662
00657      END-EXEC.                                                    EL662
00658                                                                   EL662
00659      GO TO 9999-RETURN-CICS.                                      EL662
00660                                                                   EL662
00661  8900-PGMIDERR.                                                   EL662
00662      GO TO 9999-RETURN-CICS.                                      EL662
00663                                                                   EL662
00664  8999-WRITE-TRAILER.                                              EL662
00665      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL662
00666      MOVE 'RF'                   TO  RF-RECORD-ID.                EL662
00667      MOVE '2'                    TO  RF-RECORD-TYPE.              EL662
00668      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                EL662
00669      ADD +1                      TO  WS-LINE-NUMBER.              EL662
00670      MOVE WS-LINE-NUMBER         TO  RF-LINE-NUMBER.              EL662
00671                                                                   EL662
00672      MOVE SPACES                 TO  RF-TRAILER-RECORD.           EL662
00673      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL**4
00674      END-EXEC                                                        CL**4
00675      EXEC CICS FORMATTIME                                            CL**4
00676                ABSTIME(LCP-CICS-TIME)                                CL**4
00677                TIME(LCP-TIME-OF-DAY-XX)                              CL**4
00678      END-EXEC                                                        CL**4
00679      MOVE  LCP-TIME-OF-DAY-68 TO RF-PRINT-HH-MM-SS.                  CL**4
00680      MOVE SAVE-DATE              TO  RF-CURRENT-DATE.             EL662
00681                                                                   EL662
00682      EXEC CICS WRITE                                              EL662
00683          DATASET (REPT-FILE-ID)                                   EL662
00684          FROM    (REPORT-SAVE-FILE)                               EL662
00685          RIDFLD  (RF-CONTROL-PRIMARY)                             EL662
00686      END-EXEC.                                                    EL662
00687                                                                   EL662
00688  9000-READ-CONTROL.                                               EL662
00689      EXEC CICS READ                                               EL662
00690          DATASET     (CNTL-ID)                                    EL662
00691          SET         (ADDRESS OF CONTROL-FILE)                       CL**4
00692          RIDFLD      (ELCNTL-KEY)                                 EL662
00693      END-EXEC.                                                    EL662
00694                                                                   EL662
00695  9000-EXIT.                                                       EL662
00696       EXIT.                                                       EL662
00697                                                                   EL662
00698  9999-RETURN-CICS.                                                EL662
00699      EXEC CICS  RETURN                                            EL662
00700      END-EXEC.                                                    EL662
00701                                                                   EL662
00702  9999-EXIT.                                                       EL662
00703       EXIT.                                                       EL662
