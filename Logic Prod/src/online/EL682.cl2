00001  IDENTIFICATION DIVISION.                                         10/25/94
00002                                                                   EL682
00003  PROGRAM-ID.                 EL682 .                                 LV012
00004 *              PROGRAM CONVERTED BY                                  CL*12
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*12
00006 *              CONVERSION DATE 10/07/94 15:15:03.                    CL*12
00007 *                            VMOD=2.012                              CL*12
00008 *                                                                 EL682
00008 *                                                                 EL682
00009 *AUTHOR.        LOGIC INC.                                           CL*12
00010 *               DALLAS, TEXAS.                                       CL*12
00011                                                                   EL682
00012 *DATE-COMPILED.                                                      CL*12
00013                                                                   EL682
00014 *SECURITY.   *****************************************************   CL*12
00015 *            *                                                   *   CL*12
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*12
00017 *            *                                                   *   CL*12
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*12
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*12
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*12
00021 *            *                                                   *   CL*12
00022 *            *****************************************************   CL*12
00023                                                                   EL682
00024 *REMARKS.                                                            CL*12
00025 *        TRANSACTION - (EXF8) - PAYMENT AND ADJUSTMENT PRINT         CL*12
00026 *        THIS PROGRAM IS STARTED  FROM EL671  TO PRINT               CL*12
00027 *        THE PAYMENT AND ADJUSTMENT SUMARY.
112001******************************************************************
112001*                   C H A N G E   L O G
112001*
112001* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
112001*-----------------------------------------------------------------
112001*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
112001* EFFECTIVE    NUMBER
112001*-----------------------------------------------------------------
112001******************************************************************
                                                                           CL*12
00028      EJECT                                                        EL682
00029  ENVIRONMENT DIVISION.                                            EL682
00030  DATA DIVISION.                                                   EL682
00031  WORKING-STORAGE SECTION.                                         EL682
00032  01  LCP-TIME-OF-DAY-XX.                                             CL*12
00033      05  LCP-TIME-OF-DAY-68        PIC 9(6).                         CL*12
00034      05  FILLER                    PIC 99.                           CL*12
00035  01  LCP-CICS-TIME                 PIC 9(15).                        CL*12
00036  77  FILLER PIC  X(32) VALUE '********************************'.  EL682
00037  77  FILLER PIC  X(32) VALUE '     EL682  WORKING-STORAGE     '.  EL682
00038  77  FILLER PIC  X(32) VALUE '************ V/M 2.012 *********'.     CL*12
00039                                                                   EL682
00040  77  CTR                     PIC S99    COMP    VALUE +0.         EL682
00041  77  CLEN                    PIC S9(4)  COMP    VALUE +1024.      EL682
00042  77  D-AMT                   PIC S9(7)V99       COMP-3.           EL682
00043  77  SAV-CO-PRE              PIC  X              VALUE LOW-VALUE. EL682
00044                                                                   EL682
00045  01  WORK-AREAS.                                                  EL682
00046      12  PGM-NAME            PIC  X(8)      VALUE SPACES.         EL682
00047      12  LINK-ELDATCV        PIC  X(8)      VALUE 'ELDATCV'.      EL682
00048      12  EMI-LINE1           PIC  X(72).                          EL682
00049      12  WS-NEXT-TRAN        PIC  X(4).                           EL682
00050      12  WS-TERMINAL-ID.                                          EL682
00051          16  WS-TERM-PREFIX  PIC  XX.                             EL682
00052          16  FILLER          PIC  XX.                             EL682
00053      12  PRT-CNT             PIC S9(3)       VALUE +0 COMP-3.     EL682
00054      12  REC-CNT             PIC S9(4)       VALUE +0 COMP-3.     EL682
00055      12  WS-LINE-NUMBER      PIC S9(7)       VALUE +0 COMP-3.     EL682
00056      12  WS-PAGE             PIC S9(5)       VALUE +0 COMP-3.     EL682
00057      12  WS-REPORT-SW        PIC S9          VALUE +0 COMP-3.     EL682
00058      12  WS-PRINT-SW         PIC S9          VALUE +0 COMP-3.     EL682
00059      12  TOT-ENTRY-AMT       PIC S9(7)V99    VALUE ZERO.          EL682
00060      12  SUB-ENTRY-AMT       PIC S9(7)V99    VALUE ZERO.          EL682
00061      12  REPT-FILE-ID        PIC  X(8)       VALUE 'ELREPT  '.    EL682
00062      12  PYAJ-FILE-ID        PIC  X(8)       VALUE 'ERPYAJ  '.    EL682
00063      12  CNTL-ID             PIC  X(8)       VALUE 'ELCNTL  '.    EL682
00064      12  SA                  PIC S9(3)  COMP.                     EL682
00065      12  B-REC-FLAG          PIC  X          VALUE SPACES.        EL682
00066      12  WS-DATE.                                                 EL682
00067          16  WS-YR           PIC  XX.                             EL682
00068          16  WS-MO           PIC  XX.                             EL682
00069          16  WS-DA           PIC  XX.                             EL682
00070      12  ABEND-AREA          PIC  X(72).                          EL682
00071      12  WS-CURRENT-DT       PIC  X(8)       VALUE SPACES.        EL682
00072      12  SV-CONTROL.                                              EL682
00073          16  SV-CARR         PIC  X          VALUE SPACES.        EL682
00074          16  SV-GROUP        PIC  X(6)       VALUE SPACES.        EL682
00075          16  SV-RESP         PIC  X(10)      VALUE SPACES.        EL682
00076          16  SV-ACCT         PIC  X(10)      VALUE SPACES.        EL682
00077      12  PY-CONTROL.                                              EL682
00078          16  PY-CARR         PIC  X.                              EL682
00079          16  PY-GROUP        PIC  X(6).                           EL682
00080          16  PY-RESP         PIC  X(10).                          EL682
00081          16  PY-ACCT         PIC  X(10).                          EL682
00082                                                                   EL682
00083  01  SUMMARY-TOTS.                                                EL682
00084      12  SUB-R               PIC S9(7)V99    COMP-3 VALUE +0.     EL682
00085      12  SUB-D               PIC S9(7)V99    COMP-3 VALUE +0.        CL**5
00086      12  SUB-C               PIC S9(7)V99    COMP-3 VALUE +0.     EL682
00087      12  SUB-S               PIC S9(7)V99    COMP-3 VALUE +0.        CL**5
00088      12  SUB-T               PIC S9(7)V99    COMP-3 VALUE +0.        CL**5
00089      12  SUB-U               PIC S9(7)V99    COMP-3 VALUE +0.        CL**5
00090      12  SUB-X               PIC S9(7)V99    COMP-3 VALUE +0.     EL682
00091      12  SUB-Y               PIC S9(7)V99    COMP-3 VALUE +0.     EL682
00092      12  SUB-Z               PIC S9(7)V99    COMP-3 VALUE +0.     EL682
00093      12  SUB-F               PIC S9(7)V99    COMP-3 VALUE +0.     EL682
00094      12  TOT-R               PIC S9(7)V99    COMP-3 VALUE +0.     EL682
00095      12  TOT-D               PIC S9(7)V99    COMP-3 VALUE +0.        CL**5
00096      12  TOT-C               PIC S9(7)V99    COMP-3 VALUE +0.     EL682
00097      12  TOT-S               PIC S9(7)V99    COMP-3 VALUE +0.        CL**5
00098      12  TOT-T               PIC S9(7)V99    COMP-3 VALUE +0.        CL**5
00099      12  TOT-U               PIC S9(7)V99    COMP-3 VALUE +0.        CL**5
00100      12  TOT-X               PIC S9(7)V99    COMP-3 VALUE +0.     EL682
00101      12  TOT-Y               PIC S9(7)V99    COMP-3 VALUE +0.     EL682
00102      12  TOT-Z               PIC S9(7)V99    COMP-3 VALUE +0.     EL682
00103      12  TOT-F               PIC S9(7)V99    COMP-3 VALUE +0.     EL682
00104      12  SUM-TOT             PIC S9(7)V9     COMP-3 VALUE +0.     EL682
00105                                                                   EL682
00106  01  ACCESS-KEYS.                                                 EL682
00107      12  ELCNTL-KEY.                                              EL682
00108          16  CNTL-COMP-ID    PIC  XXX.                            EL682
00109          16  CNTL-REC-TYPE   PIC  X.                              EL682
00110          16  CNTL-ACCESS     PIC  X(4).                           EL682
00111          16  CNTL-SEQ-NO     PIC S9(4)   COMP.                    EL682
00112      EJECT                                                        EL682
00113                              COPY ELCREPT.                           CL**5
00114      EJECT                                                        EL682
00115                              COPY ELCDATE.                           CL**5
00116      EJECT                                                        EL682
00117  01  HDR-LINES.                                                   EL682
00118      12  HDR-1.                                                   EL682
00119          16  FILLER          PIC  X(32)      VALUE SPACES.        EL682
00120          16  FILLER          PIC  X(33)      VALUE                EL682
00121                  'PAYMENT AND ADJUSTMENT SUMMARY'.                EL682
00122          16  FILLER          PIC  X(24)      VALUE SPACES.        EL682
00123          16  FILLER          PIC  X(8)       VALUE 'EL - 682'.    EL682
00124      12  HDR-2.                                                   EL682
00125          16  FILLER          PIC  X(33)      VALUE SPACES.        EL682
00126          16  H2-COMP         PIC  X(30)      VALUE                EL682
00127                  'LOGIC, INC.'.                                   EL682
00128          16  FILLER          PIC  X(27)      VALUE SPACES.        EL682
00129          16  H2-DATE         PIC  X(8).                           EL682
00130      12  HDR-3.                                                   EL682
00131          16  FILLER          PIC  X(37)      VALUE SPACES.        EL682
00132          16  H3-DATE.                                             EL682
00133              20  H3-DATE1    PIC  X(8)       VALUE SPACES.        EL682
00134              20  H3-THRU     PIC  X(6)       VALUE SPACES.        EL682
00135              20  H3-DATE2    PIC  X(8)       VALUE SPACES.        EL682
00136          16  FILLER          PIC  X(31)      VALUE SPACES.        EL682
00137          16  FILLER          PIC  X(5)       VALUE 'PAGE'.        EL682
00138          16  H3-PAGE         PIC Z,ZZ9.                           EL682
00139      12  HDR-4.                                                   EL682
00140          16  FILLER          PIC  X          VALUE SPACE.         EL682
00141          16  FILLER          PIC  X(28)      VALUE                EL682
00142                  'CARR  GROUP    FIN. RESP.'.                     EL682
00143          16  FILLER          PIC  X(33)      VALUE                EL682
00144                  'ACCOUNT       AMOUNT    TYPE'.                  EL682
00145          16  FILLER          PIC  X(10)      VALUE                EL682
00146                  '   COMMENT'.                                    EL682
00147          16  FILLER          PIC  X(26)      VALUE SPACES.           CL**2
00148          16  FILLER          PIC  XX         VALUE 'BY'.          EL682
00149          16  FILLER          PIC  X(4)       VALUE SPACES.        EL682
00150          16  HDR-4-DT        PIC  X(6)       VALUE 'EOM DT'.         CL*11
00151          16  FILLER          PIC  XX         VALUE SPACES.           CL**3
00152          16  HDR-4-INV       PIC  X(7)       VALUE 'INVOICE'.        CL**3
00153          16  FILLER          PIC  XX         VALUE SPACES.           CL**3
00154          16  HDR-4-REF       PIC  X(9)       VALUE 'REFERENCE'.      CL**3
00155                                                                   EL682
00156  01  DATA-1.                                                      EL682
00157      12  FILLER              PIC  XX         VALUE SPACES.        EL682
00158      12  D-CARR              PIC  X.                              EL682
00159      12  FILLER              PIC  X(4)       VALUE SPACES.        EL682
00160      12  D-GROUP             PIC  X(6).                           EL682
00161      12  D-TOTAL.                                                 EL682
00162          16  FILLER          PIC  X(3)       VALUE SPACES.        EL682
00163          16  D-RESP          PIC  X(10).                          EL682
00164          16  FILLER          PIC  XX         VALUE SPACES.        EL682
00165          16  D-ACCT          PIC  X(10).                          EL682
00166      12  FILLER              PIC  X          VALUE SPACES.        EL682
00167      12  D-AMOUNT            PIC ZZZZ,ZZZ.99-.                    EL682
00168      12  DET-AMT  REDEFINES                                       EL682
00169          D-AMOUNT            PIC  Z(12).                          EL682
00170      12  FILLER              PIC  XX         VALUE SPACES.        EL682
00171      12  D-TYPE              PIC  X(10).                          EL682
00172      12  FILLER              PIC  XX         VALUE SPACES.        EL682
00173      12  D-COMM              PIC  X(30).                             CL**2
00174      12  FILLER              PIC  XX         VALUE SPACE.         EL682
00175      12  D-BY                PIC  X(4)       VALUE SPACE.         EL682
00176      12  FILLER              PIC  XX         VALUE SPACE.         EL682
00177      12  D-MEDT              PIC  X(8)       VALUE SPACE.         EL682
00178      12  FILLER              PIC  X          VALUE SPACE.            CL**3
00179      12  D-INVOICE           PIC  X(6)       VALUE SPACE.            CL**3
00180      12  FILLER              PIC  XX         VALUE SPACE.            CL**3
00181      12  D-REFERENCE         PIC  X(12)      VALUE SPACE.            CL**3
00182                                                                   EL682
00183  01  DATA-2.                                                      EL682
00184      12  FILLER              PIC  X          VALUE SPACES.        EL682
00185      12  FILLER              PIC  X(22)      VALUE                EL682
00186              'REPORT NOT PRODUCED - '.                            EL682
00187      12  FILLER              PIC  X(41)      VALUE                EL682
00188              'NO RECORDS FOUND TO MATCH SELECTION DATES'.         EL682
00189      EJECT                                                        EL682
00190                              COPY ELCAID.                            CL**5
00191                                                                   EL682
00192  01  PF-AID  REDEFINES  DFHAID.                                   EL682
00193      12  FILLER                  PIC  X(8).                       EL682
00194      12  PF-VALUES  OCCURS 24    PIC  X.                          EL682
00195      EJECT                                                        EL682
00196                              COPY ELCINTF.                           CL**5
00197      12  PI-WORK-AREA  REDEFINES  PI-PROGRAM-WORK-AREA.           EL682
00198          16  PI-REPORT-RANGE-DATES.                               EL682
00199              20  PI-FROM-DT          PIC  XX.                     EL682
00200              20  PI-THRU-DT          PIC  XX.                     EL682
00201              20  PI-CL676-STOP-DT    PIC  XX.                     EL682
00202          16  PI-PYAJTYP              PIC  X.                      EL682
00203          16  PI-EL676-ENTER-DT       PIC  XX.                        CL**8
00204          16  PI-PYAJCAR              PIC  X.                         CL**8
00205          16  PI-PYAJDTT              PIC  X.                         CL*11
00206          16  PI-PYAJBY               PIC X(4).                       CL*12
00207          16  PI-CHECK-MAINT-TYPE     PIC  X.                      EL682
00208              88  VALID-MAINT-TYPE                VALUE 'S' 'A' 'C'EL682
00209                                                        'D' 'K'.   EL682
00210              88  ADD-FUNCTION                    VALUE 'A'.       EL682
00211              88  SHOW-FUNCTION                   VALUE 'S'.       EL682
00212              88  DELETE-FUNCTION                 VALUE 'D'.       EL682
00213              88  CHANGE-FUNCTION                 VALUE 'C'.       EL682
00214              88  COPY-FUNCTION                   VALUE 'K'.       EL682
00215          16  PI-PREV-MAINTYP         PIC  X.                      EL682
00216          16  PI-FIRST-TIME-SW        PIC  X.                      EL682
00217              88  FIRST-TIME                      VALUE 'Y'.       EL682
00218          16  PI-ERPYAJ-KEY.                                       EL682
00219              20  PI-PYA-COMPANY-CD   PIC  X.                      EL682
00220              20  PI-PYA-CODE         PIC  X.                      EL682
00221              20  PI-PYA-TABLE        PIC  XXX.                    EL682
00222          16  PI-SAVE-ERPYAJ-KEY      PIC  X(5).                   EL682
00223          16  PI-BROWSE-SW            PIC  X.                      EL682
00224              88  BROWSE-STARTED                  VALUE 'Y'.       EL682
00225          16  PI-ERPYAJ-EOF-SW        PIC  X.                      EL682
00226              88  ERPYAJ-EOF                      VALUE 'Y'.       EL682
00227          16  PI-EXCESS-SW            PIC  X.                      EL682
00228              88  EXCESS-LEVEL-EXISTS             VALUE 'X'.       EL682
00229          16  PI-COMPANY-ADD-SW       PIC  X.                      EL682
00230              88  COMPANY-RECORD-ADDED            VALUE 'Y'.       EL682
00231          16  PI-SUB                  PIC S99.                     EL682
00232          16  PI-LAST-LEVEL           PIC S99.                     EL682
00233          16  FILLER                  PIC X(604).                     CL*12
00234      EJECT                                                        EL682
00235  LINKAGE SECTION.                                                 EL682
00236  01  DFHCOMMAREA             PIC  X(1024).                        EL682
00237      EJECT                                                        EL682
00238 *01 PARM-LIST .                                                      CL*12
00239 *    12  FILLER              PIC S9(8)  COMP.                        CL*12
00240 *    12  ERPYAJ-POINTER      PIC S9(8)  COMP.                        CL*12
00241 *    12  ELCNTL-POINTER      PIC S9(8)  COMP.                        CL*12
00242      EJECT                                                        EL682
00243                              COPY ERCPYAJ.                           CL**5
00244      EJECT                                                        EL682
00245                              COPY ELCCNTL.                           CL**5
00246      EJECT                                                        EL682
00247  PROCEDURE DIVISION.                                              EL682
00248      CONTINUE.                                                       CL*12
00249                                                                   EL682
PEMUNI*    MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL682
00251      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL682
00252      MOVE '5'                    TO  DC-OPTION-CODE.              EL682
00253                                                                   EL682
00254      PERFORM 8500-DATE-CONVERT  THRU  8599-EXIT.                  EL682
00255                                                                   EL682
00256      MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DT.               EL682
00257                                                                   EL682
00258  1000-START.                                                      EL682
00259      EXEC CICS  HANDLE CONDITION                                  EL682
00260          ERROR     (8800-ABEND)                                   EL682
00261          PGMIDERR  (8900-PGMIDERR)                                EL682
00262      END-EXEC.                                                    EL682
00263                                                                   EL682
       2000-RECEIVE.                                                    EL682
           EXEC CICS  RETRIEVE                                          EL682
               INTO    (PROGRAM-INTERFACE-BLOCK)                        EL682
               LENGTH  (CLEN)                                           EL682
           END-EXEC.                                                    EL682
00269                                                                   EL682
00270  2100-CHECK-IN-PROGRESS.                                          EL682
00271      EXEC CICS  HANDLE CONDITION                                  EL682
00272          NOTFND  (2200-WRITE-INITIAL-TRAILER)                     EL682
00273      END-EXEC.                                                    EL682
00274                                                                   EL682
00275      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL682
00276      MOVE 'RF'                   TO  RF-RECORD-ID.                EL682
00277      MOVE '2'                    TO  RF-RECORD-TYPE.              EL682
00278      MOVE 'EL682'                TO  RF-REPORT-ID.                EL682
00279      MOVE ZEROS                  TO  RF-LINE-NUMBER.              EL682
00280                                                                   EL682
00281      EXEC CICS  READ                                              EL682
00282          DATASET  (REPT-FILE-ID)                                  EL682
00283          INTO     (REPORT-SAVE-FILE)                              EL682
00284          RIDFLD   (RF-CONTROL-PRIMARY)                            EL682
00285      END-EXEC.                                                    EL682
00286                                                                   EL682
00287 ********IF RECORD FOUND, THEN ANOTHER REPORT HAS ALREADY          EL682
00288 ********BEEN STARTED.  IF PREVIOUS REPORT ABENDED AND DIDN'T      EL682
00289 ********COMPLETE, THEN OPERATOR MUST PURGE REPORT AND CREATE      EL682
00290 ********A NEW ONE.                                                EL682
00291                                                                   EL682
00292      GO TO 9900-RETURN-CICS.                                      EL682
00293                                                                   EL682
00294  2200-WRITE-INITIAL-TRAILER.                                      EL682
00295      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL682
00296      MOVE 'RF'                   TO  RF-RECORD-ID.                EL682
00297      MOVE '2'                    TO  RF-RECORD-TYPE.              EL682
00298      MOVE 'EL682'                TO  RF-REPORT-ID.                EL682
00299      MOVE ZEROS                  TO  RF-LINE-NUMBER.              EL682
00300      MOVE SPACES                 TO  RF-TRAILER-RECORD.           EL682
00301      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL*12
00302      END-EXEC                                                        CL*12
00303      EXEC CICS FORMATTIME                                            CL*12
00304                ABSTIME(LCP-CICS-TIME)                                CL*12
00305                TIME(LCP-TIME-OF-DAY-XX)                              CL*12
00306      END-EXEC                                                        CL*12
00307      MOVE  LCP-TIME-OF-DAY-68 TO RF-PRINT-HH-MM-SS.                  CL*12
00308      MOVE 'STARTED'              TO  RF-CURRENT-DATE.             EL682
00309                                                                   EL682
00310      EXEC CICS  WRITE                                             EL682
00311          DATASET  (REPT-FILE-ID)                                  EL682
00312          FROM     (REPORT-SAVE-FILE)                              EL682
00313          RIDFLD   (RF-CONTROL-PRIMARY)                            EL682
00314      END-EXEC.                                                    EL682
00315                                                                   EL682
00316  2300-DELETE-REC.                                                 EL682
00317      MOVE 1                      TO  RF-LINE-NUMBER.              EL682
00318                                                                   EL682
00319      EXEC CICS  HANDLE CONDITION                                  EL682
00320          NOTFND  (2500-DELETE-REC)                                EL682
00321      END-EXEC.                                                    EL682
00322                                                                   EL682
00323  2400-DELETE-1.                                                   EL682
00324      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL682
00325      MOVE 'RF'                   TO  RF-RECORD-ID.                EL682
00326      MOVE '1'                    TO  RF-RECORD-TYPE.              EL682
00327      MOVE 'EL682'                TO  RF-REPORT-ID.                EL682
00328                                                                   EL682
00329      EXEC CICS  DELETE                                            EL682
00330          DATASET    (REPT-FILE-ID)                                EL682
00331          RIDFLD     (RF-CONTROL-PRIMARY)                          EL682
00332          KEYLENGTH  (11)                                          EL682
00333      END-EXEC.                                                    EL682
00334                                                                   EL682
00335      ADD 1                       TO  RF-LINE-NUMBER.              EL682
00336                                                                   EL682
00337      GO TO 2400-DELETE-1.                                         EL682
00338                                                                   EL682
00339  2500-DELETE-REC.                                                 EL682
00340      EXEC CICS  HANDLE CONDITION                                  EL682
00341          NOTFND  (3000-START-BROWSE)                              EL682
00342      END-EXEC.                                                    EL682
00343                                                                   EL682
00344  2600-DELETE-2.                                                   EL682
00345      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL682
00346      MOVE 'RF'                   TO  RF-RECORD-ID.                EL682
00347      MOVE '2'                    TO  RF-RECORD-TYPE.              EL682
00348      MOVE 'EL682'                TO  RF-REPORT-ID.                EL682
00349                                                                   EL682
00350      EXEC CICS  DELETE                                            EL682
00351          DATASET    (REPT-FILE-ID)                                EL682
00352          RIDFLD     (RF-CONTROL-PRIMARY)                          EL682
00353          KEYLENGTH  (11)                                          EL682
00354      END-EXEC.                                                    EL682
00355                                                                   EL682
00356      ADD 1                       TO  RF-LINE-NUMBER.              EL682
00357                                                                   EL682
00358      GO TO 2600-DELETE-2.                                         EL682
00359                                                                   EL682
00360  3000-START-BROWSE.                                               EL682
00361      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL682
00362      MOVE SPACES                 TO  CNTL-ACCESS.                 EL682
00363      MOVE '1'                    TO  CNTL-REC-TYPE.               EL682
00364      MOVE +0                     TO  CNTL-SEQ-NO.                 EL682
00365                                                                   EL682
00366      PERFORM 9000-READ-CONTROL  THRU  9099-EXIT.                  EL682
00367                                                                   EL682
00368      MOVE CF-CL-MAIL-TO-NAME     TO  H2-COMP.                     EL682
00369      MOVE WS-CURRENT-DT          TO  H2-DATE.                     EL682
00370      MOVE ' THRU '               TO  H3-THRU.                     EL682
00371      MOVE SPACE                  TO  DC-OPTION-CODE.              EL682
00372      MOVE PI-FROM-DT             TO  DC-BIN-DATE-1.               EL682
00373                                                                   EL682
00374      PERFORM 8500-DATE-CONVERT  THRU  8599-EXIT.                  EL682
00375                                                                   EL682
00376      MOVE DC-GREG-DATE-1-EDIT    TO  H3-DATE1.                    EL682
00377                                                                   EL682
00378      IF PI-FROM-DT = LOW-VALUES                                   EL682
00379          MOVE 'INCEPT.'          TO  H3-DATE1.                    EL682
00380                                                                   EL682
00381      MOVE SPACE                  TO  DC-OPTION-CODE.              EL682
00382      MOVE PI-THRU-DT             TO  DC-BIN-DATE-1.               EL682
00383                                                                   EL682
00384      PERFORM 8500-DATE-CONVERT  THRU  8599-EXIT.                  EL682
00385                                                                   EL682
00386      MOVE DC-GREG-DATE-1-EDIT    TO  H3-DATE2.                    EL682
00387                                                                   EL682
00388      IF PI-THRU-DT = HIGH-VALUES                                  EL682
00389          MOVE 'CURRENT'          TO  H3-DATE2.                    EL682
00390                                                                      CL**3
00391      IF NOT PI-AR-PROCESSING                                         CL**3
00392          MOVE SPACES             TO  HDR-4-INV                       CL**3
00393                                      HDR-4-REF.                      CL**3
00394      IF PI-PYAJDTT  =  'I'                                           CL*11
00395          MOVE 'INP DT'           TO  HDR-4-DT                        CL*11
00396      ELSE                                                            CL*11
00397          MOVE 'MNT DT'           TO  HDR-4-DT.                       CL*11
00398                                                                   EL682
00399      EXEC CICS  HANDLE CONDITION                                  EL682
00400          NOTFND  (9900-RETURN-CICS)                               EL682
00401      END-EXEC.                                                    EL682
00402                                                                   EL682
00403      MOVE LOW-VALUES             TO  PI-ERPYAJ-KEY.               EL682
00404      MOVE PI-COMPANY-CD          TO  PI-PYA-COMPANY-CD.           EL682
00405                                                                   EL682
00406      EXEC CICS  STARTBR                                           EL682
00407          DATASET  (PYAJ-FILE-ID)                                  EL682
00408          RIDFLD   (PI-ERPYAJ-KEY)                                 EL682
00409      END-EXEC.                                                    EL682
00410                                                                   EL682
00411      EXEC CICS  HANDLE CONDITION                                  EL682
00412          ENDFILE  (3200-ENDBROWSE)                                EL682
00413      END-EXEC.                                                    EL682
00414                                                                   EL682
00415  3100-READNEXT.                                                   EL682
00416      EXEC CICS  READNEXT                                          EL682
00417          DATASET  (PYAJ-FILE-ID)                                  EL682
00418          SET      (ADDRESS OF PENDING-PAY-ADJ)                       CL*12
00419          RIDFLD   (PI-ERPYAJ-KEY)                                 EL682
00420      END-EXEC.                                                    EL682
00421                                                                   EL682
00422      CONTINUE.                                                       CL*12
00423                                                                   EL682
00424      IF PI-COMPANY-CD = PY-COMPANY-CD                             EL682
00425          NEXT SENTENCE                                               CL*11
00426      ELSE                                                            CL*11
00427          GO TO 3200-ENDBROWSE.                                       CL*11
00428                                                                      CL*11
00429      IF PY-CREDIT-ACCEPT-DT = LOW-VALUES                             CL*11
00430          NEXT SENTENCE                                               CL*11
00431      ELSE                                                            CL*11
00432          GO TO 3100-READNEXT.                                        CL*11
00433                                                                      CL*11
00434      IF PI-PYAJDTT  =  'I'                                           CL*11
00435          IF PY-INPUT-DT  NOT LESS     PI-FROM-DT  AND                CL*11
00436             PY-INPUT-DT  NOT GREATER  PI-THRU-DT                     CL*11
00437                  GO TO 5000-PRINT-IT                                 CL*10
00438          ELSE                                                     EL682
00439                  GO TO 3100-READNEXT.                                CL*11
00440                                                                      CL*11
00441      IF PY-LAST-MAINT-DT NOT LESS    PI-FROM-DT  AND                 CL*11
00442         PY-LAST-MAINT-DT NOT GREATER PI-THRU-DT                      CL*11
00443              GO TO 5000-PRINT-IT                                     CL*11
00444      ELSE                                                            CL*11
00445              GO TO 3100-READNEXT.                                    CL**9
00446                                                                   EL682
00447  3200-ENDBROWSE.                                                  EL682
00448      IF WS-LINE-NUMBER = ZEROS                                    EL682
00449          MOVE DATA-2             TO  RF-DATA-133                  EL682
00450          PERFORM 7000-PRT-LINE  THRU  7099-EXIT                   EL682
00451          GO TO 3400-SKIP.                                         EL682
00452                                                                   EL682
00453      MOVE SPACES                 TO  DATA-1.                      EL682
00454                                                                   EL682
00455      IF PI-PYAJTYP = SPACE                                        EL682
00456          NEXT SENTENCE                                            EL682
00457      ELSE                                                         EL682
00458          PERFORM 4000-SPECIAL-TOTALS  THRU  4099-EXIT             EL682
00459          GO TO 3400-SKIP.                                         EL682
00460                                                                   EL682
00461      ADD SUB-X  SUB-Y  SUB-Z  SUB-F  GIVING  D-AMT.               EL682
00462                                                                   EL682
00463      IF D-AMT = ZERO                                              EL682
00464         MOVE '* CASH SUBTOTAL'   TO  D-TOTAL                      EL682
00465         COMPUTE D-AMT =                                              CL**5
00466                SUB-R + SUB-D + SUB-S + SUB-T - SUB-C - SUB-U         CL*12
00467         MOVE D-AMT               TO  D-AMOUNT                     EL682
00468         MOVE ' '                 TO  RF-CTL-CHAR-133              EL682
00469         MOVE DATA-1              TO  RF-DATA-133                  EL682
00470         PERFORM 7000-PRT-LINE  THRU  7099-EXIT                    EL682
00471         MOVE ' '                 TO  RF-CTL-CHAR-133              EL682
00472         MOVE DATA-1              TO  RF-DATA-133                  EL682
00473         PERFORM 7000-PRT-LINE  THRU  7099-EXIT                    EL682
00474         GO TO 3300-SKIP.                                          EL682
00475                                                                   EL682
00476      MOVE '* CASH'               TO  D-TOTAL.                     EL682
00477                                                                   EL682
00478      COMPUTE D-AMT =                                                 CL**5
00479             SUB-R + SUB-D + SUB-S + SUB-T - SUB-C - SUB-U.           CL*12
00480                                                                   EL682
00481      MOVE D-AMT                  TO  D-AMOUNT.                    EL682
00482      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL682
00483      MOVE DATA-1                 TO  RF-DATA-133.                 EL682
00484                                                                   EL682
00485      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00486                                                                   EL682
00487      MOVE '* NON-CASH '          TO  D-TOTAL.                     EL682
00488                                                                   EL682
00489      ADD SUB-X  SUB-Y  SUB-Z  SUB-F  GIVING  D-AMOUNT.            EL682
00490                                                                   EL682
00491      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL682
00492      MOVE DATA-1                 TO  RF-DATA-133.                 EL682
00493                                                                   EL682
00494      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00495                                                                   EL682
00496      MOVE '* SUB-TOTAL'          TO  D-TOTAL.                     EL682
00497      COMPUTE SUB-ENTRY-AMT =                                         CL**5
00498              SUB-R + SUB-D + SUB-S + SUB-T - SUB-C -SUB-U +          CL**5
00499              SUB-X + SUB-Y + SUB-Z + SUB-F.                          CL**5
00500      MOVE SUB-ENTRY-AMT          TO  D-AMOUNT.                    EL682
00501      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL682
00502      MOVE DATA-1                 TO  RF-DATA-133.                 EL682
00503                                                                   EL682
00504      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00505                                                                   EL682
00506  3300-SKIP.                                                       EL682
00507      MOVE SPACES                 TO  DATA-1.                      EL682
00508      MOVE 'REMITTANCES'          TO  D-TOTAL.                        CL**5
00509      COMPUTE D-AMOUNT = TOT-R + TOT-S.                               CL**5
00510      MOVE '-'                    TO  RF-CTL-CHAR-133.             EL682
00511      MOVE DATA-1                 TO  RF-DATA-133.                 EL682
00512                                                                   EL682
00513      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00514                                                                   EL682
00515      MOVE 'DEPOSITS  '           TO  D-TOTAL.                        CL**5
00516      COMPUTE D-AMOUNT = TOT-D + TOT-T.                               CL**5
00517      MOVE ' '                    TO  RF-CTL-CHAR-133.                CL**5
00518      MOVE DATA-1                 TO  RF-DATA-133.                    CL**5
00519                                                                      CL**5
00520      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                         CL**5
00521                                                                      CL**5
00522      MOVE 'AGENT CHARGES'        TO  D-TOTAL.                        CL**5
00523      COMPUTE D-AMOUNT = TOT-C + TOT-U.                               CL**5
00524      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL682
00525      MOVE DATA-1                 TO  RF-DATA-133.                 EL682
00526                                                                   EL682
00527      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00528                                                                   EL682
00529      MOVE 'CASH GRAND TOTAL'     TO  D-TOTAL.                     EL682
00530                                                                   EL682
00531      COMPUTE D-AMT = TOT-R + TOT-S + TOT-D + TOT-T - TOT-C - TOT-U   CL**5
00532                                                                   EL682
00533      MOVE D-AMT                  TO  D-AMOUNT.                    EL682
00534      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL682
00535      MOVE DATA-1                 TO  RF-DATA-133.                 EL682
00536                                                                   EL682
00537      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00538                                                                   EL682
00539      COMPUTE D-AMT = TOT-X + TOT-Y + TOT-Z + TOT-F.               EL682
00540                                                                   EL682
00541      IF D-AMT = ZERO                                              EL682
00542          GO TO 3400-SKIP.                                         EL682
00543                                                                   EL682
00544      MOVE 'ADD-TO-YTD'           TO  D-TOTAL.                     EL682
00545      MOVE TOT-X                  TO  D-AMOUNT.                    EL682
00546      MOVE '-'                    TO  RF-CTL-CHAR-133.                CL**5
00547      MOVE DATA-1                 TO  RF-DATA-133.                 EL682
00548                                                                   EL682
00549      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00550                                                                   EL682
00551      MOVE 'SUB-FM-YTD'           TO  D-TOTAL.                     EL682
00552      MOVE TOT-Y                  TO  D-AMOUNT.                    EL682
00553      MOVE ' '    TO RF-CTL-CHAR-133.                              EL682
00554      MOVE DATA-1 TO RF-DATA-133.                                  EL682
00555                                                                   EL682
00556      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00557                                                                   EL682
00558      MOVE 'ADJ-BAL   '           TO  D-TOTAL.                        CL**7
00559      MOVE TOT-Z                  TO  D-AMOUNT.                    EL682
00560      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL682
00561      MOVE DATA-1                 TO  RF-DATA-133.                 EL682
00562                                                                   EL682
00563      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00564                                                                   EL682
00565      MOVE 'FICA-ENTRY'           TO  D-TOTAL.                     EL682
00566      MOVE TOT-F                  TO  D-AMOUNT.                    EL682
00567      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL682
00568      MOVE DATA-1                 TO  RF-DATA-133.                 EL682
00569                                                                   EL682
00570      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00571                                                                   EL682
00572      MOVE 'OTHER GRAND TOTAL'    TO  D-TOTAL.                     EL682
00573                                                                   EL682
00574      COMPUTE D-AMT = TOT-X + TOT-Y + TOT-Z + TOT-F.               EL682
00575                                                                   EL682
00576      MOVE D-AMT                  TO  D-AMOUNT.                    EL682
00577      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL682
00578      MOVE DATA-1                 TO  RF-DATA-133.                 EL682
00579                                                                   EL682
00580      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00581                                                                      CL**5
00582      COMPUTE D-AMT = TOT-R + TOT-S + TOT-D + TOT-T - TOT-C - TOT-U   CL**5
00583                    + TOT-X + TOT-Y + TOT-Z + TOT-F.                  CL**5
00584                                                                   EL682
00585      MOVE '** GRAND TOTAL'       TO  D-TOTAL.                     EL682
00586      MOVE D-AMT                  TO  D-AMOUNT.                    EL682
00587      MOVE '0'                    TO  RF-CTL-CHAR-133.             EL682
00588      MOVE DATA-1                 TO  RF-DATA-133.                 EL682
00589                                                                   EL682
00590      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00591                                                                   EL682
00592  3400-SKIP.                                                       EL682
00593      MOVE SPACES                 TO  DATA-1.                      EL682
00594      MOVE '*TOTAL RECORDS '      TO  D-TOTAL.                     EL682
00595      MOVE REC-CNT                TO  DET-AMT.                     EL682
00596      MOVE '0'                    TO  RF-CTL-CHAR-133.             EL682
00597      MOVE DATA-1                 TO  RF-DATA-133.                 EL682
00598                                                                   EL682
00599      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00600                                                                   EL682
00601      MOVE '1'                    TO  RF-CTL-CHAR-133.             EL682
00602      MOVE SPACES                 TO  RF-DATA-133.                 EL682
00603                                                                   EL682
00604      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00605                                                                   EL682
00606      EXEC CICS  ENDBR                                             EL682
00607          DATASET  (PYAJ-FILE-ID)                                  EL682
00608      END-EXEC.                                                    EL682
00609                                                                   EL682
00610  3500-DELETE-INITIAL-2.                                           EL682
00611      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL682
00612      MOVE 'RF'                   TO  RF-RECORD-ID.                EL682
00613      MOVE '2'                    TO  RF-RECORD-TYPE.              EL682
00614      MOVE 'EL682'                TO  RF-REPORT-ID.                EL682
00615      MOVE ZEROS                  TO  RF-LINE-NUMBER.              EL682
00616                                                                   EL682
00617      EXEC CICS  DELETE                                            EL682
00618          DATASET    (REPT-FILE-ID)                                EL682
00619          RIDFLD     (RF-CONTROL-PRIMARY)                          EL682
00620          KEYLENGTH  (11)                                          EL682
00621      END-EXEC.                                                    EL682
00622                                                                   EL682
00623      PERFORM 8999-WRITE-TRAILER.                                  EL682
00624                                                                   EL682
00625      GO TO 9900-RETURN-CICS.                                      EL682
00626                                                                   EL682
00627  4000-SPECIAL-TOTALS.                                             EL682
00628      IF PI-PYAJTYP = 'R'                                          EL682
00629          MOVE SPACES             TO  DATA-1                       EL682
00630          MOVE 'REMIT-RECD'       TO  D-TOTAL                      EL682
00631          MOVE TOT-R              TO  D-AMOUNT                     EL682
00632          MOVE '-'                TO  RF-CTL-CHAR-133              EL682
00633          MOVE DATA-1             TO  RF-DATA-133                  EL682
00634          PERFORM 7000-PRT-LINE  THRU  7099-EXIT                   EL682
00635          GO TO 4099-EXIT.                                         EL682
00636                                                                   EL682
00637      IF PI-PYAJTYP = 'D'                                             CL**5
00638          MOVE SPACES             TO  DATA-1                          CL**5
00639          MOVE 'DEPOSIT   '       TO  D-TOTAL                         CL**5
00640          MOVE TOT-D              TO  D-AMOUNT                        CL**5
00641          MOVE '-'                TO  RF-CTL-CHAR-133                 CL**5
00642          MOVE DATA-1             TO  RF-DATA-133                     CL**5
00643          PERFORM 7000-PRT-LINE  THRU  7099-EXIT                      CL**5
00644          GO TO 4099-EXIT.                                            CL**5
00645                                                                      CL**5
00646      IF PI-PYAJTYP = 'C'                                          EL682
00647          MOVE 'CHG-TO-AGT'       TO  D-TOTAL                      EL682
00648          MOVE TOT-C              TO  D-AMOUNT                     EL682
00649          MOVE ' '                TO  RF-CTL-CHAR-133                 CL**5
00650          MOVE DATA-1             TO  RF-DATA-133                     CL**5
00651          PERFORM 7000-PRT-LINE  THRU  7099-EXIT                      CL**5
00652          GO TO 4099-EXIT.                                            CL**5
00653                                                                      CL**5
00654      IF PI-PYAJTYP = 'S'                                             CL**5
00655          MOVE SPACES             TO  DATA-1                          CL**5
00656          MOVE 'ADJ-REMIT '       TO  D-TOTAL                         CL**5
00657          MOVE TOT-S              TO  D-AMOUNT                        CL**5
00658          MOVE '-'                TO  RF-CTL-CHAR-133                 CL**5
00659          MOVE DATA-1             TO  RF-DATA-133                     CL**5
00660          PERFORM 7000-PRT-LINE  THRU  7099-EXIT                      CL**5
00661          GO TO 4099-EXIT.                                            CL**5
00662                                                                      CL**5
00663      IF PI-PYAJTYP = 'T'                                             CL**5
00664          MOVE SPACES             TO  DATA-1                          CL**5
00665          MOVE 'ADJ-DEP   '       TO  D-TOTAL                         CL**5
00666          MOVE TOT-T              TO  D-AMOUNT                        CL**5
00667          MOVE '-'                TO  RF-CTL-CHAR-133                 CL**5
00668          MOVE DATA-1             TO  RF-DATA-133                     CL**5
00669          PERFORM 7000-PRT-LINE  THRU  7099-EXIT                      CL**5
00670          GO TO 4099-EXIT.                                            CL**5
00671                                                                      CL**5
00672      IF PI-PYAJTYP = 'U'                                             CL**5
00673          MOVE 'ADJ-AGT-CG'       TO  D-TOTAL                         CL**5
00674          MOVE TOT-U              TO  D-AMOUNT                        CL**5
00675          MOVE ' '                TO  RF-CTL-CHAR-133              EL682
00676          MOVE DATA-1             TO  RF-DATA-133                  EL682
00677          PERFORM 7000-PRT-LINE  THRU  7099-EXIT                   EL682
00678          GO TO 4099-EXIT.                                         EL682
00679                                                                   EL682
00680      IF PI-PYAJTYP = 'X'                                          EL682
00681          MOVE 'ADD-TO-YTD'       TO  D-TOTAL                      EL682
00682          MOVE TOT-X              TO  D-AMOUNT                     EL682
00683          MOVE ' '                TO  RF-CTL-CHAR-133              EL682
00684          MOVE DATA-1             TO  RF-DATA-133                  EL682
00685          PERFORM 7000-PRT-LINE  THRU  7099-EXIT                   EL682
00686          GO TO 4099-EXIT.                                         EL682
00687                                                                   EL682
00688      IF PI-PYAJTYP = 'Y'                                          EL682
00689          MOVE 'SUB-FM-YTD'       TO  D-TOTAL                      EL682
00690          MOVE TOT-Y              TO  D-AMOUNT                     EL682
00691          MOVE ' '                TO  RF-CTL-CHAR-133              EL682
00692          MOVE DATA-1             TO  RF-DATA-133                  EL682
00693          PERFORM 7000-PRT-LINE  THRU  7099-EXIT                   EL682
00694          GO TO 4099-EXIT.                                         EL682
00695                                                                   EL682
00696      IF PI-PYAJTYP = 'Z'                                          EL682
00697          MOVE 'ADJ-BAL   '       TO  D-TOTAL                         CL**7
00698          MOVE TOT-Z              TO  D-AMOUNT                     EL682
00699          MOVE ' '                TO  RF-CTL-CHAR-133              EL682
00700          MOVE DATA-1             TO  RF-DATA-133                  EL682
00701          PERFORM 7000-PRT-LINE  THRU  7099-EXIT                   EL682
00702          GO TO 4099-EXIT.                                         EL682
00703                                                                   EL682
00704      IF PI-PYAJTYP = 'F'                                             CL**5
00705          MOVE 'FICA      '       TO  D-TOTAL                         CL**5
00706          MOVE TOT-F              TO  D-AMOUNT                        CL**5
00707          MOVE ' '                TO  RF-CTL-CHAR-133                 CL**5
00708          MOVE DATA-1             TO  RF-DATA-133                     CL**5
00709          PERFORM 7000-PRT-LINE  THRU  7099-EXIT                      CL**5
00710          GO TO 4099-EXIT.                                            CL**5
00711                                                                   EL682
00712  4099-EXIT.                                                       EL682
00713      EXIT.                                                        EL682
00714                                                                   EL682
00715  5000-PRINT-IT.                                                   EL682
00716      IF PRT-CNT GREATER THAN 60  OR                               EL682
00717         PRT-CNT = ZEROS                                           EL682
00718          PERFORM 6500-HDR-RTN  THRU  6599-EXIT.                   EL682
00719                                                                   EL682
00720      MOVE PY-CARRIER             TO  PY-CARR.                     EL682
00721      MOVE PY-GROUPING            TO  PY-GROUP.                    EL682
00722      MOVE PY-FIN-RESP            TO  PY-RESP.                     EL682
00723      MOVE PY-ACCOUNT             TO  PY-ACCT.                     EL682
00724                                                                      CL**8
00725      IF PI-PYAJCAR = SPACE                                           CL**8
00726          NEXT SENTENCE                                               CL**8
00727      ELSE                                                            CL**8
00728          IF PY-CARRIER = PI-PYAJCAR                                  CL**8
00729              NEXT SENTENCE                                           CL**8
00730          ELSE                                                        CL**8
00731              GO TO 3100-READNEXT.                                    CL**8
00732                                                                   EL682
00733      IF PI-PYAJBY  = SPACES                                          CL*12
00734          NEXT SENTENCE                                               CL*12
00735      ELSE                                                            CL*12
00736          IF PY-LAST-MAINT-BY = PI-PYAJBY                             CL*12
00737              NEXT SENTENCE                                           CL*12
00738          ELSE                                                        CL*12
00739              GO TO 3100-READNEXT.                                    CL*12
00740                                                                      CL*12
00741      IF PI-PYAJTYP = SPACE                                        EL682
00742          NEXT SENTENCE                                            EL682
00743      ELSE                                                         EL682
00744          IF PY-RECORD-TYPE = PI-PYAJTYP                           EL682
00745              GO TO 5100-SKIP                                      EL682
00746          ELSE                                                     EL682
00747              GO TO 3100-READNEXT.                                 EL682
00748                                                                   EL682
00749      IF PI-COMPANY-ID = 'FLA'                                        CL**4
00750        IF PY-CARR = SV-CARR OR                                       CL**4
00751           SV-CARR = SPACES                                           CL**4
00752            GO TO 5100-SKIP                                           CL**4
00753          ELSE                                                        CL**4
00754            NEXT SENTENCE                                             CL**4
00755      ELSE                                                            CL**4
00756      IF PY-CONTROL = SV-CONTROL  OR                               EL682
00757         SV-CONTROL = SPACES                                       EL682
00758          GO TO 5100-SKIP.                                         EL682
00759                                                                   EL682
00760      MOVE SPACES                 TO  DATA-1.                      EL682
00761                                                                   EL682
00762      COMPUTE D-AMT =  SUB-X + SUB-Y + SUB-Z + SUB-F.              EL682
00763                                                                   EL682
00764      IF D-AMT = ZERO                                              EL682
00765          MOVE '* CASH SUBTOTAL'  TO  D-TOTAL                      EL682
00766          COMPUTE D-AMT = SUB-R + SUB-D + SUB-S + SUB-T -             CL**5
00767                          SUB-C - SUB-U                               CL**6
00768          MOVE D-AMT              TO  D-AMOUNT                     EL682
00769          MOVE ' '                TO  RF-CTL-CHAR-133              EL682
00770          MOVE DATA-1             TO  RF-DATA-133                  EL682
00771          PERFORM 7000-PRT-LINE  THRU  7099-EXIT                   EL682
00772          MOVE ZEROS              TO  SUB-R SUB-S                     CL**6
00773                                      SUB-D SUB-T                     CL**6
00774                                      SUB-C SUB-U                     CL**6
00775          GO TO 5100-SKIP.                                         EL682
00776                                                                   EL682
00777      MOVE '* CASH'               TO  D-TOTAL.                     EL682
00778                                                                   EL682
00779      COMPUTE D-AMT = SUB-R + SUB-D + SUB-S + SUB-T -                 CL**5
00780                      SUB-C - SUB-U.                                  CL**5
00781                                                                   EL682
00782      MOVE D-AMT                  TO  D-AMOUNT.                    EL682
00783      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL682
00784      MOVE DATA-1                 TO  RF-DATA-133.                 EL682
00785                                                                   EL682
00786      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00787                                                                   EL682
00788      MOVE '* NON-CASH '          TO  D-TOTAL.                     EL682
00789                                                                   EL682
00790      ADD SUB-X SUB-Y SUB-Z SUB-F GIVING D-AMOUNT.                 EL682
00791                                                                   EL682
00792      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL682
00793      MOVE DATA-1                 TO  RF-DATA-133.                 EL682
00794                                                                   EL682
00795      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00796                                                                   EL682
00797      MOVE '* SUB-TOTAL'          TO  D-TOTAL.                     EL682
00798                                                                   EL682
00799      COMPUTE D-AMT =                                              EL682
00800          SUB-R + SUB-D + SUB-S + SUB-T + SUB-C + SUB-U +             CL**5
00801          SUB-X + SUB-Y + SUB-Z + SUB-F.                              CL**5
00802                                                                   EL682
00803      MOVE D-AMT                  TO  D-AMOUNT.                    EL682
00804      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL682
00805      MOVE DATA-1                 TO  RF-DATA-133.                 EL682
00806                                                                   EL682
00807      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00808                                                                   EL682
00809      MOVE SPACES                 TO  DATA-1.                      EL682
00810      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL682
00811      MOVE DATA-1                 TO  RF-DATA-133.                 EL682
00812      MOVE ZEROS                  TO  SUB-R SUB-D SUB-C               CL**5
00813                                      SUB-S SUB-T SUB-U               CL**5
00814                                      SUB-X SUB-Y SUB-Z               CL**5
00815                                      SUB-F.                          CL**5
00816                                                                   EL682
00817      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00818                                                                   EL682
00819  5100-SKIP.                                                       EL682
00820      MOVE PY-CONTROL             TO  SV-CONTROL.                  EL682
00821      MOVE SPACES                 TO  D-TOTAL.                     EL682
00822      MOVE PY-CARRIER             TO  D-CARR.                      EL682
00823      MOVE PY-GROUPING            TO  D-GROUP.                     EL682
00824      MOVE PY-FIN-RESP            TO  D-RESP.                      EL682
00825      MOVE PY-ACCOUNT             TO  D-ACCT.                      EL682
00826      MOVE PY-ENTRY-AMT           TO  D-AMOUNT.                    EL682
00827      MOVE PY-ENTRY-COMMENT       TO  D-COMM.                      EL682
00828      MOVE PY-LAST-MAINT-BY       TO  D-BY.                        EL682
00829      MOVE SPACE                  TO  DC-OPTION-CODE.              EL682
00830                                                                      CL**3
00831      IF PI-AR-PROCESSING                                             CL**3
00832          MOVE PY-BIL-INV         TO  D-INVOICE                       CL**3
00833          MOVE PY-REF-NO          TO  D-REFERENCE.                    CL**3
00834                                                                      CL**3
00835      IF PI-PYAJDTT  =  'I'                                           CL*11
00836          MOVE PY-INPUT-DT          TO  DC-BIN-DATE-1                 CL*11
00837      ELSE                                                            CL*11
00838          MOVE PY-LAST-MAINT-DT     TO DC-BIN-DATE-1.                 CL*11
00839                                                                   EL682
00840      PERFORM 8500-DATE-CONVERT  THRU  8599-EXIT.                  EL682
00841                                                                   EL682
00842      MOVE DC-GREG-DATE-1-EDIT    TO  D-MEDT.                      EL682
00843                                                                   EL682
00844      IF PY-RECORD-TYPE = 'R'                                      EL682
00845          ADD PY-ENTRY-AMT        TO  SUB-R TOT-R                  EL682
00846          MOVE 'REMIT-RECD'       TO  D-TYPE.                      EL682
00847                                                                   EL682
00848      IF PY-RECORD-TYPE = 'D'                                         CL**5
00849          ADD PY-ENTRY-AMT        TO  SUB-D TOT-D                     CL**5
00850          MOVE 'DEPOSIT   '       TO  D-TYPE.                         CL**5
00851                                                                      CL**5
00852      IF PY-RECORD-TYPE = 'C'                                      EL682
00853          ADD PY-ENTRY-AMT        TO  SUB-C TOT-C                  EL682
00854          MOVE 'CHG-TO-AGT'       TO  D-TYPE.                      EL682
00855                                                                      CL**5
00856      IF PY-RECORD-TYPE = 'S'                                         CL**5
00857          ADD PY-ENTRY-AMT        TO  SUB-S TOT-S                     CL**5
00858          MOVE 'ADJ REMIT '       TO  D-TYPE.                         CL**5
00859                                                                      CL**5
00860      IF PY-RECORD-TYPE = 'T'                                         CL**5
00861          ADD PY-ENTRY-AMT        TO  SUB-T TOT-T                     CL**5
00862          MOVE 'ADJ DEP   '       TO  D-TYPE.                         CL**5
00863                                                                      CL**5
00864      IF PY-RECORD-TYPE = 'U'                                         CL**5
00865          ADD PY-ENTRY-AMT        TO  SUB-U TOT-U                     CL**5
00866          MOVE 'ADJ CHARGE'       TO  D-TYPE.                         CL**5
00867                                                                   EL682
00868      IF PY-RECORD-TYPE = 'X'                                      EL682
00869          ADD PY-ENTRY-AMT        TO  SUB-X TOT-X                  EL682
00870          MOVE 'ADD-TO-YTD'       TO  D-TYPE.                      EL682
00871                                                                   EL682
00872      IF PY-RECORD-TYPE = 'Y'                                      EL682
00873          ADD PY-ENTRY-AMT        TO  SUB-Y TOT-Y                  EL682
00874          MOVE 'SUB-FM-YTD'       TO  D-TYPE.                      EL682
00875                                                                   EL682
00876      IF PY-RECORD-TYPE = 'Z'                                      EL682
00877          ADD PY-ENTRY-AMT        TO  SUB-Z TOT-Z                  EL682
00878          MOVE 'ADJ-BAL   '       TO  D-TYPE.                         CL**7
00879                                                                   EL682
00880      IF PY-RECORD-TYPE = 'F'                                      EL682
00881          ADD PY-ENTRY-AMT        TO  SUB-F TOT-F                  EL682
00882          MOVE 'FICA-ENTRY'       TO  D-TYPE.                      EL682
00883                                                                   EL682
00884      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL682
00885      MOVE DATA-1                 TO  RF-DATA-133.                 EL682
00886                                                                   EL682
00887      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00888                                                                   EL682
00889      ADD 1                       TO  REC-CNT.                     EL682
00890                                                                   EL682
00891      GO TO 3100-READNEXT.                                         EL682
00892                                                                   EL682
00893  6500-HDR-RTN.                                                    EL682
00894      MOVE +0                     TO  PRT-CNT.                     EL682
00895                                                                   EL682
00896      ADD +1                      TO  WS-PAGE.                     EL682
00897                                                                   EL682
00898      MOVE WS-PAGE                TO  H3-PAGE.                     EL682
00899      MOVE '1'                    TO  RF-CTL-CHAR-133.             EL682
00900      MOVE HDR-1                  TO  RF-DATA-133.                 EL682
00901                                                                   EL682
00902      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00903                                                                   EL682
00904      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL682
00905      MOVE HDR-2                  TO  RF-DATA-133.                 EL682
00906                                                                   EL682
00907      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00908                                                                   EL682
00909      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL682
00910      MOVE HDR-3                  TO  RF-DATA-133.                 EL682
00911                                                                   EL682
00912      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00913                                                                   EL682
00914      MOVE '0'                    TO  RF-CTL-CHAR-133.             EL682
00915      MOVE HDR-4                  TO  RF-DATA-133.                 EL682
00916                                                                   EL682
00917      PERFORM 7000-PRT-LINE  THRU  7099-EXIT.                      EL682
00918                                                                   EL682
00919  6599-EXIT.                                                       EL682
00920      EXIT.                                                        EL682
00921                                                                   EL682
00922  7000-PRT-LINE.                                                   EL682
00923      ADD 1                       TO  PRT-CNT.                     EL682
00924                                                                   EL682
00925      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL682
00926      MOVE 'RF'                   TO  RF-RECORD-ID.                EL682
00927      MOVE '1'                    TO  RF-RECORD-TYPE.              EL682
00928      MOVE 'EL682'                TO  RF-REPORT-ID.                EL682
00929                                                                   EL682
00930      ADD 1                       TO  WS-LINE-NUMBER.              EL682
00931                                                                   EL682
00932      MOVE WS-LINE-NUMBER         TO  RF-LINE-NUMBER.              EL682
00933                                                                   EL682
00934      EXEC CICS  WRITE                                             EL682
00935          DATASET  (REPT-FILE-ID)                                  EL682
00936          FROM     (REPORT-SAVE-FILE)                              EL682
00937          RIDFLD   (RF-CONTROL-PRIMARY)                            EL682
00938      END-EXEC.                                                    EL682
00939                                                                   EL682
00940       MOVE SPACES                TO  DATA-1.                      EL682
00941                                                                   EL682
00942  7099-EXIT.                                                       EL682
00943       EXIT.                                                       EL682
00944                                                                   EL682
00945  8500-DATE-CONVERT.                                               EL682
00946      EXEC CICS  LINK                                              EL682
00947          PROGRAM   (LINK-ELDATCV)                                 EL682
00948          COMMAREA  (DATE-CONVERSION-DATA)                         EL682
00949          LENGTH    (DC-COMM-LENGTH)                               EL682
00950      END-EXEC.                                                    EL682
00951                                                                   EL682
00952  8599-EXIT.                                                       EL682
00953      EXIT.                                                        EL682
00954                                                                   EL682
00955  8800-ABEND.                                                      EL682
00956      MOVE DFHEIBLK               TO  EMI-LINE1.                   EL682
00957                                                                   EL682
00958      EXEC CICS  LINK                                              EL682
00959          PROGRAM   ('EL004')                                      EL682
00960          COMMAREA  (EMI-LINE1)                                    EL682
00961          LENGTH    (72)                                           EL682
00962      END-EXEC.                                                    EL682
00963                                                                   EL682
00964      GO TO 9900-RETURN-CICS.                                      EL682
00965                                                                   EL682
00966  8900-PGMIDERR.                                                   EL682
00967      GO TO 9900-RETURN-CICS.                                      EL682
00968                                                                   EL682
00969  8999-WRITE-TRAILER.                                              EL682
00970      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD                EL682
00971      MOVE 'RF'                   TO  RF-RECORD-ID.                EL682
00972      MOVE '2'                    TO  RF-RECORD-TYPE               EL682
00973      MOVE 'EL682'                TO  RF-REPORT-ID                 EL682
00974                                                                   EL682
00975      ADD +1                      TO  WS-LINE-NUMBER               EL682
00976                                                                   EL682
00977      MOVE WS-LINE-NUMBER         TO  RF-LINE-NUMBER               EL682
00978      MOVE SPACES                 TO  RF-TRAILER-RECORD            EL682
00979      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL*12
00980      END-EXEC                                                        CL*12
00981      EXEC CICS FORMATTIME                                            CL*12
00982                ABSTIME(LCP-CICS-TIME)                                CL*12
00983                TIME(LCP-TIME-OF-DAY-XX)                              CL*12
00984      END-EXEC                                                        CL*12
00985      MOVE  LCP-TIME-OF-DAY-68 TO RF-PRINT-HH-MM-SS                   CL*12
00986      MOVE WS-CURRENT-DT          TO  RF-CURRENT-DATE              EL682
00987                                                                   EL682
00988      EXEC CICS  WRITE                                             EL682
00989          DATASET  (REPT-FILE-ID)                                  EL682
00990          FROM     (REPORT-SAVE-FILE)                              EL682
00991          RIDFLD   (RF-CONTROL-PRIMARY)                            EL682
00992      END-EXEC.                                                    EL682
00993                                                                   EL682
00994  9000-READ-CONTROL.                                               EL682
00995      EXEC CICS  READ                                              EL682
00996          DATASET  (CNTL-ID)                                       EL682
00997          SET      (ADDRESS OF CONTROL-FILE)                          CL*12
00998          RIDFLD   (ELCNTL-KEY)                                    EL682
00999      END-EXEC.                                                    EL682
01000                                                                   EL682
01001      CONTINUE.                                                       CL*12
01002                                                                   EL682
01003  9099-EXIT.                                                       EL682
01004       EXIT.                                                       EL682
01005                                                                   EL682
01006  9900-RETURN-CICS.                                                EL682
01007      EXEC CICS  RETURN                                            EL682
01008      END-EXEC.                                                    EL682
01009                                                                   EL682
01010      GOBACK.                                                      EL682
01011                                                                   EL682
01012  9999-EXIT.                                                       EL682
01013      EXIT.                                                        EL682
