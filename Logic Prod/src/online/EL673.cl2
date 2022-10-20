00001  IDENTIFICATION DIVISION.                                         04/22/98
00002                                                                   EL673
00003  PROGRAM-ID.                 EL673 .                                 LV015
00004 *              PROGRAM CONVERTED BY                                  CL**6
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**6
00006 *              CONVERSION DATE 02/14/96 07:27:28.                    CL**6
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE               CL*13
00008 *                            VMOD=2.007                              CL*13
00009 *                                                                 EL673
00009                                                                   EL673
00010 *AUTHOR.     LOGIC INC.                                              CL**6
00011 *            DALLAS, TEXAS.                                          CL**6
00012                                                                   EL673
00013 *DATE-COMPILED.                                                      CL**6
00014 *SECURITY.   *****************************************************   CL**6
00015 *            *                                                   *   CL**6
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**6
00017 *            *                                                   *   CL**6
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**6
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**6
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**6
00021 *            *                                                   *   CL**6
00022 *            *****************************************************   CL**6
00023                                                                   EL673
00024 *REMARKS. TRANSACTION - EXE7 - ACCOUNT MASTER SUMMARY                CL**6
00025 *         STARTED FROM EL671                                         CL**6
00026                                                                   EL673
00027      EJECT                                                        EL673
00028  ENVIRONMENT DIVISION.                                            EL673
00029  DATA DIVISION.                                                   EL673
00030  WORKING-STORAGE SECTION.                                         EL673
00031  01  LCP-TIME-OF-DAY-XX.                                             CL**6
00032      05  LCP-TIME-OF-DAY-68        PIC 9(6).                         CL**6
00033      05  FILLER                    PIC 99.                           CL**6
00034  01  LCP-CICS-TIME                 PIC 9(15).                        CL**6
00035  77  FILLER  PIC X(32)  VALUE '********************************'. EL673
00036  77  FILLER  PIC X(32)  VALUE '*    EL673 WORKING STORAGE     *'. EL673
00037  77  FILLER  PIC X(32)  VALUE '************ V/M 2.007 *********'.    CL*13
00038                                                                   EL673
00039  77  CLEN        PIC S9(4)  COMP    VALUE 1024.                   EL673
00040  77  CTR         PIC S99    COMP    VALUE +0.                     EL673
00041  77  SAV-CARRIER PIC X              VALUE LOW-VALUE.              EL673
00042                                                                   EL673
00043  01  WS-DATE-AREA.                                                EL673
00044      03  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL673
00045      03  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.        EL673
00046      03  SAVE-DATE-ALPHA         PIC X(18)   VALUE SPACES.        EL673
00047                                                                   EL673
00048  01  WORK-AREAS.                                                  EL673
00049      03  WS-REPORT-ID            PIC X(6).                        EL673
00050      03  PRT-LINE2               PIC X       VALUE SPACES.        EL673
00051         88 LINE2-PRINTED                     VALUE 'X'.           EL673
00052      03  PGM-NAME                PIC X(8)    VALUE SPACES.        EL673
00053      03  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV '.    EL673
00054      03  EMI-LINE1               PIC X(72).                       EL673
00055      03  WS-NEXT-TRAN            PIC X(4).                        EL673
00056      03  WS-TERMINAL-ID.                                          EL673
00057          05  WS-TERM-PREFIX      PIC XX.                          EL673
00058          05  FILLER              PIC XX.                          EL673
00059      03  PRT-CNT                 PIC S9(3)   VALUE +99  COMP-3.   EL673
00060      03  WS-LINE-NUMBER          PIC S9(7)   VALUE ZERO COMP-3.   EL673
00061      03  WS-PAGE                 PIC S9(5)   VALUE ZERO COMP-3.   EL673
00062      03  WS-REPORT-SW            PIC S9      VALUE ZERO COMP-3.   EL673
00063      03  WS-PRINT-SW             PIC S9      VALUE ZERO COMP-3.   EL673
00064      03  WS-PROCESS-SW           PIC X       VALUE 'N'.              CL**4
00065          88  WS-NO-MAINTENANCE               VALUE 'N'.              CL**4
00066      03  REPT-FILE-ID            PIC X(8)    VALUE 'ELREPT'.      EL673
00067      03  ACCT-FILE-ID            PIC X(8)    VALUE 'ERACCT'.      EL673
00068      03  CNTL-ID                 PIC X(8)    VALUE 'ELCNTL'.      EL673
00069      03  SA                      PIC S999    COMP.                EL673
00070      03  B-REC-FLAG              PIC X       VALUE SPACES.        EL673
00071                                                                      CL*11
00072 ***  Y2K PROJ 7744                                                   CL*10
00073      03  WS-HOLD-DATE.                                               CL*15
00074          05  WS-HOLD-DATE-FILLER PIC 9(03)   VALUE ZEROS.            CL*15
00075          05  WS-HOLD-DATE-CC     PIC 9(02)   VALUE ZEROS.            CL*15
00076          05  WS-HOLD-DATE-YY     PIC 9(02)   VALUE ZEROS.            CL*15
00077          05  WS-HOLD-DATE-MM     PIC 9(02)   VALUE ZEROS.            CL*15
00078          05  WS-HOLD-DATE-DD     PIC 9(02)   VALUE ZEROS.            CL*15
00079 ***  Y2K PROJ 7744                                                   CL*10
00080                                                                      CL*10
00081      03  WS-DATE.                                                 EL673
00082          05  WS-YR               PIC XX.                          EL673
00083          05  WS-MO               PIC XX.                          EL673
00084          05  WS-DA               PIC XX.                          EL673
00085      03  ABEND-AREA              PIC X(72).                       EL673
00086                                                                   EL673
00087      COPY ELCREPT.                                                   CL**5
00088      EJECT                                                        EL673
00089      COPY ELCDATE.                                                   CL**5
00090      EJECT                                                        EL673
00091                                                                   EL673
00092  01  HDR-LINES.                                                   EL673
00093      03  HDR-1.                                                   EL673
00094          05  FILLER          PIC X(24)   VALUE SPACES.            EL673
00095          05  FILLER          PIC X(33)   VALUE                    EL673
00096                  'ACCOUNT MASTER FILE SUMMARY'.                   EL673
00097          05  FILLER          PIC X(10)   VALUE SPACES.            EL673
00098          05  FILLER          PIC X(8)    VALUE 'EL673'.              CL**5
00099                                                                   EL673
00100      03  HDR-2.                                                   EL673
00101          05  FILLER          PIC X(29)   VALUE SPACES.            EL673
00102          05  H2-COMP         PIC X(30)   VALUE 'LOGIC, INC.'.     EL673
00103          05  FILLER          PIC X(8)    VALUE SPACES.            EL673
00104          05  H2-DATE         PIC X(8).                            EL673
00105                                                                   EL673
00106      03  HDR-3.                                                   EL673
00107          05  FILLER          PIC X(27)   VALUE SPACES.            EL673
00108          05  H3-DATE.                                             EL673
00109            07  H3-DATE1      PIC X(8)    VALUE SPACES.            EL673
00110            07  H3-THRU       PIC X(6)    VALUE SPACES.            EL673
00111            07  H3-DATE2      PIC X(8)    VALUE SPACES.            EL673
00112          05  FILLER          PIC X(15)   VALUE SPACES.            EL673
00113          05  FILLER          PIC X(5)    VALUE 'PAGE '.           EL673
00114          05  H3-PAGE         PIC ZZ,ZZ9.                          EL673
00115                                                                   EL673
00116      03  HDR-4.                                                   EL673
00117          05  FILLER          PIC X       VALUE SPACE.             EL673
00118          05  FILLER          PIC X(20)   VALUE                    EL673
00119                      '   ACCOUNT     NAME'.                       EL673
00120      03  HDR-5.                                                   EL673
00121          05  FILLER          PIC X(35)   VALUE                    EL673
00122                      'EFF.DTES CERT.DTES BUS P I REI RM R'.          CL**5
00123          05  FILLER          PIC X(44)   VALUE                    EL673
00124              'ATING ****** COMMISSION LEVELS ******* LAST '.      EL673
00125                                                                   EL673
00126      03  HDR-6.                                                   EL673
00127          05  FILLER          PIC X(35)   VALUE                    EL673
00128                      'FROM/TO    LO/HI   TYP E G TAB CD C'.       EL673
00129          05  FILLER          PIC X(44)   VALUE                    EL673
00130              'L DEV    AGENT    SINGLE  JOINT    A&H MAINT'.         CL**3
00131                                                                   EL673
00132  01  DATA-1.                                                      EL673
00133      03  D-CARRIER           PIC X               VALUE SPACES.    EL673
00134      03  FILLER              PIC X               VALUE '-'.       EL673
00135      03  D-GROUP             PIC X(6).                            EL673
00136      03  FILLER              PIC X               VALUE '-'.       EL673
00137      03  D-ST                PIC XX.                              EL673
00138      03  FILLER              PIC X               VALUE '-'.       EL673
00139      03  D-ACCT              PIC X(10).                           EL673
00140      03  FILLER              PIC X               VALUE SPACES.    EL673
00141      03  D-NAME              PIC X(20).                           EL673
00142                                                                   EL673
00143  01  DATA-2.                                                      EL673
00144      03  D-EFF-DT.                                                   CL**3
00145          05  D-F-MO          PIC XX.                                 CL**3
00146          05  D-F-D1          PIC X               VALUE '-'.          CL**3
00147          05  D-F-DA          PIC XX.                                 CL**3
00148          05  D-F-D2          PIC X               VALUE '-'.          CL**3
00149          05  D-F-YR          PIC XX.                                 CL**3
00150      03  FILLER              PIC XX              VALUE SPACES.    EL673
00151      03  D-LOWS.                                                  EL673
00152          05  D-L-MO          PIC XX.                              EL673
00153          05  D-L-D1          PIC X               VALUE '-'.       EL673
00154          05  D-L-DA          PIC XX.                              EL673
00155          05  D-L-D2          PIC X               VALUE '-'.       EL673
00156          05  D-L-YR          PIC XX.                              EL673
00157      03  FILLER              PIC XX              VALUE SPACES.    EL673
00158      03  D-BUS               PIC XX.                              EL673
00159      03  FILLER              PIC X               VALUE SPACES.    EL673
00160      03  D-PE                PIC X.                               EL673
00161      03  FILLER              PIC X               VALUE SPACES.    EL673
00162      03  D-IG                PIC X.                               EL673
00163      03  FILLER              PIC X               VALUE SPACES.    EL673
00164      03  D-REI               PIC XXX.                             EL673
00165      03  FILLER              PIC X               VALUE SPACES.    EL673
00166      03  D-REM               PIC XX.                              EL673
00167      03  FILLER              PIC X               VALUE SPACES.    EL673
00168      03  D-CLS               PIC XX.                              EL673
00169      03  FILLER              PIC X               VALUE SPACES.    EL673
00170      03  D-DEV               PIC XXX.                             EL673
00171      03  FILLER              PIC X               VALUE SPACES.    EL673
00172      03  D-COMM              PIC X(38).                           EL673
00173                                                                   EL673
00174  01  DATA-3.                                                      EL673
00175      03  D-EXP               PIC X(8).                            EL673
00176      03  FILLER              PIC X(2)            VALUE SPACES.    EL673
00177      03  D-HIGHS.                                                 EL673
00178          05  D-H-MO          PIC XX.                              EL673
00179          05  D-H-D1          PIC X               VALUE '-'.       EL673
00180          05  D-H-DA          PIC XX.                              EL673
00181          05  D-H-D2          PIC X               VALUE '-'.       EL673
00182          05  D-H-YR          PIC XX.                              EL673
00183      05  FILLER              PIC X(23)           VALUE SPACES.    EL673
00184      05  D3-COMM             PIC X(38).                           EL673
00185                                                                   EL673
00186  01  ACCESS-KEYS.                                                 EL673
00187      03  ELCNTL-KEY.                                              EL673
00188          05  CNTL-COMP-ID         PIC X(3).                       EL673
00189          05  CNTL-REC-TYPE        PIC X.                          EL673
00190          05  CNTL-ACCESS          PIC X(4).                       EL673
00191          05  CNTL-SEQ-NO          PIC S9(4)    COMP.              EL673
00192                                                                   EL673
00193  01  COMM-BLD.                                                    EL673
00194      03  CB-AGT              PIC X(10).                              CL**3
00195      03  CB-COM              PIC X.                               EL673
00196      03  FILLER              PIC X               VALUE SPACES.    EL673
00197      03  CB-SGL              PIC ZZ.999-.                         EL673
00198      03  CBR-SGL REDEFINES                                        EL673
00199          CB-SGL              PIC X(7).                            EL673
00200      03  CB-JNT              PIC ZZ.999-.                         EL673
00201      03  CBR-JNT REDEFINES                                        EL673
00202          CB-JNT              PIC X(7).                            EL673
00203      03  CB-AH               PIC ZZ.999-.                         EL673
00204      03  CBR-AH REDEFINES                                         EL673
00205          CB-AH               PIC X(7).                            EL673
00206      03  CB-MAINT.                                                EL673
00207          05  CB-M-MO         PIC XX.                              EL673
00208          05  CB-M-D1         PIC X               VALUE '-'.       EL673
00209          05  CB-M-YR         PIC XX.                              EL673
00210      EJECT                                                        EL673
00211      COPY ELCAID.                                                    CL**5
00212  01  PF-AID REDEFINES DFHAID.                                     EL673
00213      05  FILLER                      PIC X(8).                    EL673
00214      05  PF-VALUES  OCCURS 24        PIC X.                       EL673
00215      EJECT                                                        EL673
00216      COPY ELCINTF.                                                   CL**5
00217      12  PI-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.             EL673
00218          16  PI-LO-DATE          PIC XX.                          EL673
00219          16  PI-HI-DATE          PIC XX.                          EL673
00220                                                                   EL673
00221          16  PI-ERACCT-KEY.                                       EL673
00222              20  PI-ACC-COMPANY-CD  PIC X.                        EL673
00223              20  PI-ACC-CODE        PIC X.                        EL673
00224              20  PI-ACC-TABLE       PIC X(3).                     EL673
00225                                                                   EL673
00226          16  PI-SAVE-ERACCT-KEY     PIC X(5).                     EL673
00227                                                                   EL673
00228          16  PI-BROWSE-SW           PIC X.                        EL673
00229              88  BROWSE-STARTED              VALUE 'Y'.           EL673
00230          16  PI-ERACCT-EOF-SW       PIC X.                        EL673
00231              88  ERACCT-EOF                  VALUE 'Y'.           EL673
00232          16  PI-EXCESS-SW           PIC X.                        EL673
00233              88  EXCESS-LEVEL-EXISTS         VALUE 'X'.           EL673
00234          16  PI-COMPANY-ADD-SW      PIC X.                        EL673
00235              88  COMPANY-RECORD-ADDED        VALUE 'Y'.           EL673
00236                                                                   EL673
00237          16  PI-SUB                 PIC S99.                      EL673
00238          16  PI-LAST-LEVEL          PIC S99.                      EL673
00239          16  FILLER                 PIC X(618).                      CL**6
00240                                                                   EL673
00241      EJECT                                                        EL673
00242  LINKAGE SECTION.                                                 EL673
00243  01  DFHCOMMAREA                     PIC X(1024).                 EL673
00244 *01 PARM-LIST .                                                      CL**6
00245 *    02  FILLER              PIC S9(8)   COMP.                       CL**6
00246 *    02  ERACCT-POINTER      PIC S9(8)   COMP.                       CL**6
00247 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                       CL**6
00248      EJECT                                                        EL673
00249      COPY ERCACCT.                                                   CL**5
00250                                                                   EL673
00251      COPY ELCCNTL SUPPRESS.                                          CL**6
00252      EJECT                                                        EL673
00253                                                                   EL673
00254  PROCEDURE DIVISION.                                              EL673
00255                                                                   EL673
00256      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL673
00257      MOVE '5'                   TO DC-OPTION-CODE.                EL673
00258      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL673
00259      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL673
00260      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL673
00261      MOVE DC-GREG-DATE-1-ALPHA  TO  SAVE-DATE-ALPHA.              EL673
00262                                                                   EL673
00263      MOVE DFHCOMMAREA           TO  PROGRAM-INTERFACE-BLOCK.      EL673
00264      MOVE 'EL673'               TO  WS-REPORT-ID.                 EL673
00265                                                                   EL673
00266  1000-START.                                                      EL673
00267      EXEC CICS  HANDLE CONDITION                                  EL673
00268             ERROR    (8800-ABEND)                                 EL673
00269             PGMIDERR (8900-PGMIDERR)                              EL673
00270      END-EXEC.                                                    EL673
00271                                                                   EL673
00272  2000-RECEIVE.                                                    EL673
00273      EXEC CICS RETRIEVE                                           EL673
00274          INTO   (PROGRAM-INTERFACE-BLOCK)                         EL673
00275          LENGTH (CLEN)                                            EL673
00276      END-EXEC.                                                    EL673
00277                                                                   EL673
00278  2000-CHECK-IN-PROGRESS.                                          EL673
00279      EXEC CICS  HANDLE CONDITION                                  EL673
00280             NOTFND   (2000-WRITE-INITIAL-TRAILER)                 EL673
00281      END-EXEC.                                                    EL673
00282                                                                   EL673
00283      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL673
00284      MOVE 'RF'                   TO  RF-RECORD-ID.                EL673
00285      MOVE '2'                    TO  RF-RECORD-TYPE.              EL673
00286      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                EL673
00287      MOVE ZEROS                  TO  RF-LINE-NUMBER.              EL673
00288                                                                   EL673
00289      EXEC CICS READ                                               EL673
00290          DATASET (REPT-FILE-ID)                                   EL673
00291          INTO    (REPORT-SAVE-FILE)                               EL673
00292          RIDFLD  (RF-CONTROL-PRIMARY)                             EL673
00293      END-EXEC.                                                    EL673
00294                                                                   EL673
00295 ********IF RECORD FOUND, THEN ANOTHER REPORT HAS ALREADY          EL673
00296 ********BEEN STARTED.  IF PREVIOUS REPORT ABENDED AND DIDN'T      EL673
00297 ********COMPLETE, THEN OPERATOR MUST PURGE REPORT AND CREATE         CL**3
00298 ********A NEW ONE.                                                EL673
00299      GO TO 9999-RETURN-CICS.                                      EL673
00300                                                                   EL673
00301  2000-WRITE-INITIAL-TRAILER.                                      EL673
00302      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL673
00303      MOVE 'RF'                   TO  RF-RECORD-ID.                EL673
00304      MOVE '2'                    TO  RF-RECORD-TYPE.              EL673
00305      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                EL673
00306      MOVE ZEROS                  TO  RF-LINE-NUMBER.              EL673
00307                                                                   EL673
00308      MOVE SPACES                 TO  RF-TRAILER-RECORD.           EL673
00309      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL**6
00310      END-EXEC                                                        CL**6
00311      EXEC CICS FORMATTIME                                            CL**6
00312                ABSTIME(LCP-CICS-TIME)                                CL**6
00313                TIME(LCP-TIME-OF-DAY-XX)                              CL**6
00314      END-EXEC                                                        CL**6
00315      MOVE  LCP-TIME-OF-DAY-68 TO RF-PRINT-HH-MM-SS.                  CL**6
00316      MOVE 'STARTED'              TO  RF-CURRENT-DATE.             EL673
00317                                                                   EL673
00318      EXEC CICS WRITE                                              EL673
00319          DATASET (REPT-FILE-ID)                                   EL673
00320          FROM    (REPORT-SAVE-FILE)                               EL673
00321          RIDFLD  (RF-CONTROL-PRIMARY)                             EL673
00322      END-EXEC.                                                    EL673
00323                                                                   EL673
00324  2100-DELETE-REC.                                                 EL673
00325      MOVE 1 TO RF-LINE-NUMBER.                                    EL673
00326      EXEC CICS  HANDLE CONDITION                                  EL673
00327             NOTFND   (2300-DELETE-REC)                            EL673
00328      END-EXEC.                                                    EL673
00329                                                                   EL673
00330  2200-DELETE-1.                                                   EL673
00331      MOVE PI-COMPANY-CD TO RF-COMPANY-CD.                         EL673
00332      MOVE 'RF'          TO RF-RECORD-ID.                          EL673
00333      MOVE '1'           TO RF-RECORD-TYPE.                        EL673
00334      MOVE WS-REPORT-ID  TO RF-REPORT-ID.                          EL673
00335                                                                   EL673
00336      EXEC CICS DELETE                                             EL673
00337          DATASET   (REPT-FILE-ID)                                 EL673
00338          RIDFLD    (RF-CONTROL-PRIMARY)                           EL673
00339          KEYLENGTH (11)                                           EL673
00340      END-EXEC.                                                    EL673
00341                                                                   EL673
00342      ADD 1 TO RF-LINE-NUMBER.                                     EL673
00343      GO TO 2200-DELETE-1.                                         EL673
00344                                                                   EL673
00345  2300-DELETE-REC.                                                 EL673
00346      EXEC CICS  HANDLE CONDITION                                  EL673
00347             NOTFND   (3000-START-BROWSE)                          EL673
00348      END-EXEC.                                                    EL673
00349                                                                   EL673
00350  2400-DELETE-2.                                                   EL673
00351      MOVE PI-COMPANY-CD TO RF-COMPANY-CD.                         EL673
00352      MOVE 'RF'          TO RF-RECORD-ID.                          EL673
00353      MOVE '2'           TO RF-RECORD-TYPE.                        EL673
00354      MOVE WS-REPORT-ID  TO RF-REPORT-ID.                          EL673
00355                                                                   EL673
00356      EXEC CICS DELETE                                             EL673
00357          DATASET   (REPT-FILE-ID)                                 EL673
00358          RIDFLD    (RF-CONTROL-PRIMARY)                           EL673
00359          KEYLENGTH (11)                                           EL673
00360      END-EXEC.                                                    EL673
00361                                                                   EL673
00362      ADD 1 TO RF-LINE-NUMBER.                                     EL673
00363      GO TO 2400-DELETE-2.                                         EL673
00364                                                                   EL673
00365  3000-START-BROWSE.                                               EL673
00366      MOVE 0 TO RF-LINE-NUMBER.                                    EL673
00367                                                                   EL673
00368      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL673
00369      MOVE SPACES                 TO CNTL-ACCESS.                  EL673
00370      MOVE '1'                    TO CNTL-REC-TYPE.                EL673
00371      MOVE +0                     TO CNTL-SEQ-NO.                  EL673
00372                                                                   EL673
00373      PERFORM 9000-READ-CONTROL THRU 9000-EXIT.                    EL673
00374                                                                   EL673
00375      MOVE CF-CL-MAIL-TO-NAME TO H2-COMP.                          EL673
00376      MOVE SAVE-DATE          TO H2-DATE.                          EL673
00377      MOVE ' THRU '           TO H3-THRU.                          EL673
00378                                                                   EL673
00379      MOVE SPACE               TO DC-OPTION-CODE.                  EL673
00380      MOVE PI-LO-DATE          TO DC-BIN-DATE-1.                   EL673
00381      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL673
00382      MOVE DC-GREG-DATE-1-EDIT TO H3-DATE1.                        EL673
00383                                                                   EL673
00384      IF PI-LO-DATE = LOW-VALUES                                   EL673
00385         MOVE 'INCEPT.'        TO H3-DATE1.                        EL673
00386                                                                   EL673
00387      MOVE SPACE               TO DC-OPTION-CODE.                  EL673
00388      MOVE PI-HI-DATE          TO DC-BIN-DATE-1.                   EL673
00389      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL673
00390      MOVE DC-GREG-DATE-1-EDIT TO H3-DATE2.                        EL673
00391                                                                   EL673
00392      IF PI-HI-DATE = HIGH-VALUES                                  EL673
00393         MOVE 'CURRENT'        TO H3-DATE2.                        EL673
00394                                                                   EL673
00395      EXEC CICS  HANDLE CONDITION                                  EL673
00396             NOTFND   (9999-RETURN-CICS)                           EL673
00397      END-EXEC.                                                    EL673
00398                                                                   EL673
00399      MOVE LOW-VALUES    TO PI-ERACCT-KEY.                         EL673
00400      MOVE PI-COMPANY-CD TO PI-ACC-COMPANY-CD.                     EL673
00401                                                                   EL673
00402      EXEC CICS STARTBR                                            EL673
00403           DATASET  (ACCT-FILE-ID)                                 EL673
00404           RIDFLD   (PI-ERACCT-KEY)                                EL673
00405      END-EXEC.                                                    EL673
00406                                                                   EL673
00407      EXEC CICS  HANDLE CONDITION                                  EL673
00408             ENDFILE  (4500-ENDBROWSE)                             EL673
00409      END-EXEC.                                                    EL673
00410                                                                   EL673
00411  4000-READNEXT.                                                   EL673
00412      EXEC CICS READNEXT                                           EL673
00413           DATASET  (ACCT-FILE-ID)                                 EL673
00414           SET      (ADDRESS OF ACCOUNT-MASTER)                       CL**6
00415           RIDFLD   (PI-ERACCT-KEY)                                EL673
00416       END-EXEC.                                                   EL673
00417                                                                   EL673
00418      IF PI-COMPANY-CD = AM-COMPANY-CD                             EL673
00419         GO TO 5000-PRINT-IT.                                      EL673
00420                                                                   EL673
00421  4500-ENDBROWSE.                                                  EL673
00422      IF WS-NO-MAINTENANCE                                            CL**4
00423         PERFORM 6500-HDR-RTN THRU 6500-EXIT                          CL**4
00424         MOVE '-'                       TO RF-CTL-CHAR-133            CL**4
00425         MOVE ' NO MAINTENANCE APPLIED' TO RF-DATA-133                CL**4
00426         PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        CL**4
00427                                                                      CL**4
00428      MOVE '1'    TO RF-CTL-CHAR-133.                              EL673
00429      MOVE SPACES TO RF-DATA-133.                                  EL673
00430      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL673
00431      EXEC CICS ENDBR                                              EL673
00432           DATASET  (ACCT-FILE-ID)                                 EL673
00433      END-EXEC.                                                    EL673
00434                                                                   EL673
00435  4600-DELETE-INITIAL-2.                                           EL673
00436      MOVE PI-COMPANY-CD   TO RF-COMPANY-CD.                       EL673
00437      MOVE 'RF'            TO RF-RECORD-ID.                        EL673
00438      MOVE '2'             TO RF-RECORD-TYPE.                      EL673
00439      MOVE WS-REPORT-ID    TO RF-REPORT-ID.                        EL673
00440      MOVE ZEROS           TO RF-LINE-NUMBER.                      EL673
00441                                                                   EL673
00442      EXEC CICS DELETE                                             EL673
00443          DATASET   (REPT-FILE-ID)                                 EL673
00444          RIDFLD    (RF-CONTROL-PRIMARY)                           EL673
00445          KEYLENGTH (11)                                           EL673
00446      END-EXEC.                                                    EL673
00447                                                                   EL673
00448      PERFORM 8999-WRITE-TRAILER.                                  EL673
00449      GO TO 9999-RETURN-CICS.                                      EL673
00450                                                                   EL673
00451  5000-PRINT-IT.                                                   EL673
00452      IF AM-LAST-MAINT-DT LESS    PI-LO-DATE OR                    EL673
00453                          GREATER PI-HI-DATE                          CL**4
00454         GO TO 4000-READNEXT.                                      EL673
00455                                                                   EL673
00456      IF PRT-CNT GREATER 50                                        EL673
00457          PERFORM 6500-HDR-RTN THRU 6500-EXIT.                        CL**4
00458                                                                      CL**4
00459      MOVE 'Y'              TO WS-PROCESS-SW.                         CL**4
00460                                                                   EL673
00461      MOVE AM-CARRIER       TO D-CARRIER                           EL673
00462      MOVE AM-GROUPING      TO D-GROUP.                            EL673
00463      MOVE AM-STATE         TO D-ST.                               EL673
00464      MOVE AM-ACCOUNT       TO D-ACCT.                             EL673
00465      MOVE AM-NAME          TO D-NAME.                             EL673
00466      MOVE '0'              TO RF-CTL-CHAR-133.                    EL673
00467      MOVE DATA-1           TO RF-DATA-133.                        EL673
00468      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL673
00469      ADD 2 TO PRT-CNT.                                            EL673
00470                                                                   EL673
00471      MOVE AM-EFFECTIVE-DT  TO DC-BIN-DATE-1.                         CL**3
00472      MOVE ' '              TO DC-OPTION-CODE.                        CL**3
00473      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                       CL**3
00474      IF NO-CONVERSION-ERROR                                          CL**3
00475          MOVE DC-GREG-DATE-1-EDIT  TO D-EFF-DT                       CL**3
00476          MOVE '-'                  TO D-F-D1                         CL**3
00477                                       D-F-D2                         CL**3
00478      ELSE                                                            CL**3
00479          MOVE SPACES               TO D-EFF-DT.                      CL**3
00480                                                                      CL*12
00481 ***  Y2K PROJ 7744                                                   CL*12
00482      MOVE AM-LO-CERT-DATE  TO WS-HOLD-DATE.                          CL*12
00483                                                                   EL673
00484      MOVE WS-HOLD-DATE-YY  TO D-L-YR.                                CL*12
00485      MOVE WS-HOLD-DATE-MM  TO D-L-MO.                                CL*12
00486      MOVE WS-HOLD-DATE-DD  TO D-L-DA.                                CL*12
00487                                                                   EL673
00488      IF AM-LO-CERT-DATE = ZEROS                                      CL**8
00489          MOVE '  NONE'   TO D-LOWS                                EL673
00490      ELSE                                                         EL673
00491          MOVE '-'        TO D-L-D1 D-L-D2                            CL**8
00492      END-IF.                                                         CL**8
00493 ***  Y2K PROJ 7744                                                   CL**8
00494                                                                   EL673
00495      MOVE AM-GPCD          TO D-BUS.                              EL673
00496      MOVE AM-RET-P-E       TO D-PE.                               EL673
00497      MOVE AM-IG            TO D-IG.                               EL673
00498      MOVE AM-REI-TABLE     TO D-REI.                              EL673
00499      INSPECT D-IG CONVERTING '12' TO 'IG'.                           CL**6
00500      MOVE AM-REMIT-TO      TO D-REM.                              EL673
00501      MOVE AM-CAL-TABLE     TO D-CLS.                              EL673
00502      MOVE AM-LF-DEVIATION  TO D-DEV.                              EL673
00503                                                                   EL673
00504      MOVE +1 TO CTR.                                              EL673
00505      PERFORM 5100-COMM-BUILD THRU 5800-E-C-BLD.                   EL673
00506      MOVE COMM-BLD         TO D-COMM.                             EL673
00507      MOVE ' '              TO RF-CTL-CHAR-133.                    EL673
00508      MOVE DATA-2           TO RF-DATA-133.                        EL673
00509      MOVE SPACE            TO PRT-LINE2.                          EL673
00510      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL673
00511      ADD 1 TO PRT-CNT.                                            EL673
00512                                                                   EL673
00513      IF AM-EXPIRATION-DT = HIGH-VALUES                            EL673
00514          MOVE ' CURRENT' TO D-EXP                                 EL673
00515      ELSE                                                         EL673
00516          MOVE SPACE               TO DC-OPTION-CODE               EL673
00517          MOVE AM-EXPIRATION-DT    TO DC-BIN-DATE-1                EL673
00518          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                 EL673
00519          MOVE DC-GREG-DATE-1-EDIT TO D-EXP.                       EL673
00520                                                                   EL673
00521 ***  Y2K PROJ 7744                                                   CL*12
00522      MOVE AM-HI-CERT-DATE  TO WS-HOLD-DATE.                          CL*12
00523                                                                      CL*12
00524      MOVE WS-HOLD-DATE-YY  TO D-H-YR.                                CL*12
00525      MOVE WS-HOLD-DATE-MM  TO D-H-MO.                                CL*12
00526      MOVE WS-HOLD-DATE-DD  TO D-H-DA.                                CL*12
00527                                                                   EL673
00528      IF AM-HI-CERT-DATE = ZEROS                                      CL**7
00529          MOVE '  NONE'     TO D-HIGHS                             EL673
00530      ELSE                                                         EL673
00531          MOVE '-'          TO D-H-D1  D-H-D2                         CL**7
00532      END-IF.                                                         CL**7
00533 ***  Y2K PROJ 7744                                                   CL**7
00534                                                                   EL673
00535      MOVE AM-CARRIER     TO SAV-CARRIER.                          EL673
00536                                                                   EL673
00537      PERFORM 5100-COMM-BUILD THRU 5800-E-C-BLD 9 TIMES.           EL673
00538      GO TO 4000-READNEXT.                                         EL673
00539                                                                   EL673
00540  5100-COMM-BUILD.                                                 EL673
00541      IF AM-AGT (CTR) = ZEROS AND                                  EL673
00542         CTR NOT = +1                                              EL673
00543          GO TO 5200-PRT-LINE-2.                                   EL673
00544                                                                   EL673
00545      MOVE AM-AGT (CTR)      TO CB-AGT.                            EL673
00546      MOVE AM-COM-TYP (CTR)  TO CB-COM.                            EL673
00547                                                                   EL673
00548      IF AM-L-COM (CTR) NUMERIC                                    EL673
00549          MULTIPLY AM-L-COM (CTR) BY +100 GIVING CB-SGL            EL673
00550      ELSE                                                         EL673
00551          MOVE AM-L-COMA (CTR) TO CBR-SGL.                         EL673
00552                                                                   EL673
00553      IF AM-J-COM (CTR) NUMERIC                                    EL673
00554          MULTIPLY AM-J-COM (CTR) BY +100 GIVING CB-JNT            EL673
00555      ELSE                                                         EL673
00556          MOVE AM-J-COMA (CTR) TO CBR-JNT.                         EL673
00557                                                                   EL673
00558      IF AM-A-COM (CTR) NUMERIC                                    EL673
00559          MULTIPLY AM-A-COM (CTR) BY +100 GIVING CB-AH             EL673
00560      ELSE                                                         EL673
00561          MOVE AM-A-COMA (CTR) TO CBR-AH.                          EL673
00562                                                                   EL673
00563      IF CTR NOT = +1                                              EL673
00564          GO TO 5200-PRT-LINE-2.                                   EL673
00565                                                                   EL673
00566      IF AM-LAST-MAINT-DT NOT = SPACES OR LOW-VALUES OR ZEROS      EL673
00567          MOVE SPACE               TO DC-OPTION-CODE               EL673
00568          MOVE AM-LAST-MAINT-DT    TO DC-BIN-DATE-1                EL673
00569          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                 EL673
00570          MOVE DC-GREG-DATE-1-YMD  TO WS-DATE                         CL**2
00571          MOVE WS-MO               TO CB-M-MO                         CL**2
00572          MOVE '-'                 TO CB-M-D1                         CL**2
00573          MOVE WS-YR               TO CB-M-YR                         CL**2
00574        ELSE                                                       EL673
00575          MOVE 'NONE'              TO CB-MAINT.                    EL673
00576                                                                   EL673
00577      ADD +1 TO CTR.                                               EL673
00578      GO TO 5800-E-C-BLD.                                          EL673
00579                                                                   EL673
00580  5200-PRT-LINE-2.                                                 EL673
00581      IF LINE2-PRINTED  AND                                        EL673
00582         AM-AGT (CTR) = ZEROS                                      EL673
00583          ADD +1 TO CTR                                            EL673
00584          GO TO 5800-E-C-BLD.                                      EL673
00585                                                                   EL673
00586      MOVE 'X'         TO PRT-LINE2.                               EL673
00587      MOVE SPACES      TO CB-MAINT.                                EL673
00588                                                                   EL673
00589      IF AM-AGT (CTR) = ZEROS                                      EL673
00590          MOVE SPACES      TO D3-COMM                              EL673
00591         ELSE                                                      EL673
00592          MOVE COMM-BLD    TO D3-COMM.                             EL673
00593                                                                   EL673
00594      MOVE ' '         TO RF-CTL-CHAR-133.                         EL673
00595      MOVE DATA-3      TO RF-DATA-133.                             EL673
00596      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL673
00597      ADD 1 TO PRT-CNT.                                            EL673
00598      MOVE SPACES      TO DATA-3.                                  EL673
00599      ADD +1 TO CTR.                                               EL673
00600                                                                   EL673
00601  5800-E-C-BLD.                                                    EL673
00602      EXIT.                                                        EL673
00603                                                                   EL673
00604  6500-HDR-RTN.                                                    EL673
00605      ADD +1       TO WS-PAGE.                                     EL673
00606      MOVE WS-PAGE TO H3-PAGE.                                     EL673
00607      MOVE '1'     TO RF-CTL-CHAR-133.                             EL673
00608      MOVE HDR-1   TO RF-DATA-133.                                 EL673
00609      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL673
00610      MOVE ' '     TO RF-CTL-CHAR-133.                             EL673
00611      MOVE HDR-2   TO RF-DATA-133.                                 EL673
00612      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL673
00613      MOVE ' '     TO RF-CTL-CHAR-133.                             EL673
00614      MOVE HDR-3   TO RF-DATA-133.                                 EL673
00615      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL673
00616      MOVE ' '     TO RF-CTL-CHAR-133.                             EL673
00617      MOVE HDR-4   TO RF-DATA-133.                                 EL673
00618      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL673
00619      MOVE ' '     TO RF-CTL-CHAR-133.                             EL673
00620      MOVE HDR-5   TO RF-DATA-133.                                 EL673
00621      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL673
00622      MOVE ' '     TO RF-CTL-CHAR-133.                             EL673
00623      MOVE HDR-6   TO RF-DATA-133.                                 EL673
00624      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL673
00625      MOVE +0      TO PRT-CNT.                                     EL673
00626                                                                   EL673
00627  6500-EXIT.                                                          CL**4
00628      EXIT.                                                        EL673
00629                                                                   EL673
00630  7000-PRT-LINE.                                                   EL673
00631      MOVE PI-COMPANY-CD  TO RF-COMPANY-CD.                        EL673
00632      MOVE 'RF'           TO RF-RECORD-ID.                         EL673
00633      MOVE '1'            TO RF-RECORD-TYPE.                       EL673
00634      MOVE WS-REPORT-ID   TO RF-REPORT-ID.                         EL673
00635      ADD 1               TO WS-LINE-NUMBER.                       EL673
00636      MOVE WS-LINE-NUMBER TO RF-LINE-NUMBER.                       EL673
00637                                                                   EL673
00638      EXEC CICS WRITE                                              EL673
00639          DATASET (REPT-FILE-ID)                                   EL673
00640          FROM    (REPORT-SAVE-FILE)                               EL673
00641          RIDFLD  (RF-CONTROL-PRIMARY)                             EL673
00642      END-EXEC.                                                    EL673
00643                                                                   EL673
00644  7000-EXIT.                                                       EL673
00645       EXIT.                                                       EL673
00646                                                                   EL673
00647  8500-DATE-CONVERT.                                               EL673
00648      MOVE LINK-ELDATCV           TO PGM-NAME.                     EL673
00649                                                                   EL673
00650      EXEC CICS LINK                                               EL673
00651          PROGRAM    (PGM-NAME)                                    EL673
00652          COMMAREA   (DATE-CONVERSION-DATA)                        EL673
00653          LENGTH     (DC-COMM-LENGTH)                              EL673
00654      END-EXEC.                                                    EL673
00655                                                                   EL673
00656  8500-EXIT.                                                       EL673
00657      EXIT.                                                        EL673
00658                                                                   EL673
00659  8800-ABEND.                                                      EL673
00660      MOVE DFHEIBLK TO EMI-LINE1.                                  EL673
00661      EXEC CICS LINK                                               EL673
00662          PROGRAM   ('EL004')                                      EL673
00663          COMMAREA  (EMI-LINE1)                                    EL673
00664          LENGTH    (72)                                           EL673
00665      END-EXEC.                                                    EL673
00666                                                                   EL673
00667      GO TO 9999-RETURN-CICS.                                      EL673
00668                                                                   EL673
00669  8900-PGMIDERR.                                                   EL673
00670      GO TO 9999-RETURN-CICS.                                      EL673
00671                                                                   EL673
00672  8999-WRITE-TRAILER.                                              EL673
00673      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL673
00674      MOVE 'RF'                   TO  RF-RECORD-ID.                EL673
00675      MOVE '2'                    TO  RF-RECORD-TYPE.              EL673
00676      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                EL673
00677      ADD +1                      TO  WS-LINE-NUMBER.              EL673
00678      MOVE WS-LINE-NUMBER         TO  RF-LINE-NUMBER.              EL673
00679                                                                   EL673
00680      MOVE SPACES                 TO  RF-TRAILER-RECORD.           EL673
00681      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL**6
00682      END-EXEC                                                        CL**6
00683      EXEC CICS FORMATTIME                                            CL**6
00684                ABSTIME(LCP-CICS-TIME)                                CL**6
00685                TIME(LCP-TIME-OF-DAY-XX)                              CL**6
00686      END-EXEC                                                        CL**6
00687      MOVE  LCP-TIME-OF-DAY-68 TO RF-PRINT-HH-MM-SS.                  CL**6
00688      MOVE SAVE-DATE              TO  RF-CURRENT-DATE.             EL673
00689                                                                   EL673
00690      EXEC CICS WRITE                                              EL673
00691          DATASET (REPT-FILE-ID)                                   EL673
00692          FROM    (REPORT-SAVE-FILE)                               EL673
00693          RIDFLD  (RF-CONTROL-PRIMARY)                             EL673
00694      END-EXEC.                                                    EL673
00695                                                                   EL673
00696  9000-READ-CONTROL.                                               EL673
00697      EXEC CICS READ                                               EL673
00698          DATASET     (CNTL-ID)                                    EL673
00699          SET         (ADDRESS OF CONTROL-FILE)                       CL**6
00700          RIDFLD      (ELCNTL-KEY)                                 EL673
00701      END-EXEC.                                                    EL673
00702                                                                   EL673
00703  9000-EXIT.                                                       EL673
00704       EXIT.                                                       EL673
00705                                                                   EL673
00706  9999-RETURN-CICS.                                                EL673
00707      EXEC CICS  RETURN                                            EL673
00708      END-EXEC.                                                    EL673
00709                                                                   EL673
00710  9999-EXIT.                                                       EL673
00711       EXIT.                                                       EL673
