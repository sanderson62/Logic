00001  IDENTIFICATION DIVISION.                                         03/06/96
00002                                                                   EL684
00003  PROGRAM-ID.                 EL684 .                                 LV002
00004 *              PROGRAM CONVERTED BY                                  CL**2
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**2
00006 *              CONVERSION DATE 02/14/96 08:01:21.                    CL**2
00007 *                            VMOD=2.002                              CL**2
00008 *                                                                 EL684
00008 *                                                                 EL684
00009 *AUTHOR.     LOGIC INC.                                              CL**2
00010 *            DALLAS, TEXAS.                                          CL**2
00011                                                                   EL684
00012 *DATE-COMPILED.                                                      CL**2
00013 *SECURITY.   *****************************************************   CL**2
00014 *            *                                                   *   CL**2
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**2
00016 *            *                                                   *   CL**2
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**2
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**2
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**2
00020 *            *                                                   *   CL**2
00021 *            *****************************************************   CL**2
00022                                                                   EL684
00023 *REMARKS.  TRANSACTION - (EXH2) - RETRO PAYMENT AND ADJUSTMENT       CL**2
00024 *         SUMMARY.  THIS PROGRAM IS STARTED  FROM EL671.             CL**2
00025      EJECT                                                        EL684
00026  ENVIRONMENT DIVISION.                                            EL684
00027  DATA DIVISION.                                                   EL684
00028  WORKING-STORAGE SECTION.                                         EL684
00029  01  LCP-TIME-OF-DAY-XX.                                             CL**2
00030      05  LCP-TIME-OF-DAY-68        PIC 9(6).                         CL**2
00031      05  FILLER                    PIC 99.                           CL**2
00032  01  LCP-CICS-TIME                 PIC 9(15).                        CL**2
00033  77  FILLER  PIC X(32) VALUE '********************************'.  EL684
00034  77  FILLER  PIC X(32) VALUE '     EL684  WORKING-STORAGE     '.  EL684
00035  77  FILLER  PIC X(32) VALUE '*********** V/M 2.002 **********'.     CL**2
00036                                                                   EL684
00037  77  CLEN        PIC S9(4)  COMP    VALUE +1024.                     CL**2
00038  77  CTR         PIC S99    COMP    VALUE +0.                     EL684
00039  77  SAV-CO-PRE  PIC X              VALUE LOW-VALUE.              EL684
00040  77  SUB         PIC S99    COMP-3  VALUE +0.                        CL**2
00041                                                                   EL684
00042  01  WS-DATE-AREA.                                                EL684
00043      03  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL684
00044      03  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.        EL684
00045      03  SAVE-DATE-ALPHA         PIC X(18)   VALUE SPACES.        EL684
00046                                                                   EL684
00047  01  WORK-AREAS.                                                  EL684
00048      03  WS-REPORT-ID            PIC X(6)    VALUE SPACES.        EL684
00049      03  PGM-NAME                PIC X(8)    VALUE SPACES.        EL684
00050      03  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV '.    EL684
00051      03  EMI-LINE1               PIC X(72).                       EL684
00052      03  WS-NEXT-TRAN            PIC X(4).                        EL684
00053      03  WS-TERMINAL-ID.                                          EL684
00054          05  WS-TERM-PREFIX      PIC XX.                          EL684
00055          05  FILLER              PIC XX.                          EL684
00056      03  PRT-CNT                 PIC S9(3)   VALUE +0   COMP-3.   EL684
00057      03  WS-LINE-NUMBER          PIC S9(7)   VALUE ZERO COMP-3.   EL684
00058      03  WS-PAGE                 PIC S9(5)   VALUE ZERO COMP-3.   EL684
00059      03  WS-REPORT-SW            PIC S9      VALUE ZERO COMP-3.   EL684
00060      03  WS-PRINT-SW             PIC S9      VALUE ZERO COMP-3.   EL684
00061      03  TOT-ENTRY-AMT           PIC S9(7)V99 VALUE ZERO.         EL684
00062      03  SUB-ENTRY-AMT           PIC S9(7)V99 VALUE ZERO.         EL684
00063      03  REPT-FILE-ID            PIC X(8)    VALUE 'ELREPT  '.    EL684
00064      03  PYAJ-FILE-ID            PIC X(8)    VALUE 'ERREPY  '.    EL684
00065      03  CNTL-ID                 PIC X(8)    VALUE 'ELCNTL  '.    EL684
00066      03  SA                      PIC S999    COMP.                EL684
00067      03  B-REC-FLAG              PIC X       VALUE SPACES.        EL684
00068      03  WS-DATE.                                                 EL684
00069          05  WS-YR           PIC XX.                              EL684
00070          05  WS-MO           PIC XX.                              EL684
00071          05  WS-DA           PIC XX.                              EL684
00072      03  ABEND-AREA          PIC X(72).                           EL684
00073                                                                   EL684
00074      03  SV-CONTROL.                                              EL684
00075          05  SV-CARR         PIC X      VALUE SPACES.             EL684
00076          05  SV-GROUP        PIC X(6)   VALUE SPACES.             EL684
00077          05  SV-RESP         PIC X(10)  VALUE SPACES.             EL684
00078          05  SV-ACCT         PIC X(10)  VALUE SPACES.             EL684
00079                                                                   EL684
00080      03  RP-CONTROL.                                              EL684
00081          05  RP-CARR         PIC X.                               EL684
00082          05  RP-GROUP        PIC X(6).                            EL684
00083          05  RP-STAT         PIC XX.                              EL684
00084          05  RP-ACCT         PIC X(10).                           EL684
00085                                                                   EL684
00086  01  ACCESS-KEYS.                                                 EL684
00087      12  ELCNTL-KEY.                                              EL684
00088          16  CNTL-COMP-ID         PIC X(3).                       EL684
00089          16  CNTL-REC-TYPE        PIC X.                          EL684
00090          16  CNTL-ACCESS          PIC X(4).                       EL684
00091          16  CNTL-SEQ-NO          PIC S9(4)    COMP.              EL684
00092                                                                   EL684
00093      COPY ELCREPT.                                                   CL**2
00094      EJECT                                                        EL684
00095      COPY ELCDATE.                                                   CL**2
00096      EJECT                                                        EL684
00097                                                                   EL684
00098  01  WS-HEADING.                                                     CL**2
00099      03  WS-HDG-1A           PIC XX      VALUE SPACES.               CL**2
00100      03  WS-HDG-1B           PIC X(10)   VALUE SPACES.               CL**2
00101                                                                      CL**2
00102  01  VARIABLE-AMOUNTS.                                               CL**2
00103      03  VAR-AMT OCCURS 10 TIMES    PIC S9(9)V99.                    CL**2
00104                                                                      CL**2
00105  01  VARIABLE-HEADINGS.                                              CL**2
00106      03  VAR-HDG OCCURS 10 TIMES    PIC X(12).                       CL**2
00107                                                                      CL**2
00108  01  HDR-LINES.                                                   EL684
00109      03  HDR-1.                                                   EL684
00110          05  FILLER          PIC X(21)   VALUE SPACES.            EL684
00111          05  FILLER          PIC X(36)   VALUE                    EL684
00112                  'RETRO PAYMENT AND ADJUSTMENT SUMMARY'.          EL684
00113          05  FILLER          PIC X(13)   VALUE SPACES.            EL684
00114          05  FILLER          PIC X(8)    VALUE 'EL - 684'.        EL684
00115                                                                   EL684
00116      03  HDR-2.                                                   EL684
00117          05  FILLER          PIC X(25)   VALUE SPACES.               CL**2
00118          05  H2-COMP         PIC X(30)   VALUE 'LOGIC, INC.'.     EL684
00119          05  FILLER          PIC X(16)   VALUE SPACES.               CL**2
00120          05  H2-DATE         PIC X(8).                            EL684
00121                                                                   EL684
00122      03  HDR-3.                                                   EL684
00123          05  FILLER          PIC X(29)   VALUE SPACES.               CL**2
00124          05  H3-DATE.                                             EL684
00125            07  H3-DATE1      PIC X(8)    VALUE SPACES.            EL684
00126            07  H3-THRU       PIC X(6)    VALUE SPACES.            EL684
00127            07  H3-DATE2      PIC X(8)    VALUE SPACES.            EL684
00128          05  FILLER          PIC X(17)   VALUE SPACES.               CL**2
00129          05  FILLER          PIC X(5)    VALUE 'PAGE '.           EL684
00130          05  H3-PAGE         PIC ZZ,ZZ9.                          EL684
00131                                                                   EL684
00132      03  HDR-4.                                                      CL**2
00133          05  FILLER          PIC X       VALUE SPACE.             EL684
00134          05  FILLER          PIC X(4)    VALUE 'CAR '.               CL**2
00135          05  FILLER          PIC X(5)    VALUE 'GROUP'.              CL**2
00136          05  FILLER          PIC X(6)    VALUE '  ST  '.             CL**2
00137          05  FILLER          PIC X(7)    VALUE 'ACCOUNT'.            CL**2
00138          05  FILLER          PIC X(3)    VALUE SPACES.               CL**2
00139          05  VAR-PRT-HDG1    PIC X(12)   VALUE SPACES.               CL**2
00140          05  FILLER          PIC X(2)    VALUE SPACES.               CL**2
00141          05  VAR-PRT-HDG2    PIC X(12)   VALUE SPACES.               CL**2
00142          05  FILLER          PIC X(2)    VALUE SPACES.               CL**2
00143          05  VAR-PRT-HDG3    PIC X(12)   VALUE SPACES.               CL**2
00144          05  FILLER          PIC X(2)    VALUE SPACES.               CL**2
00145          05  VAR-PRT-HDG4    PIC X(11)   VALUE SPACES.               CL**2
00146                                                                   EL684
00147  01  DATA-1.                                                      EL684
00148      03  FILLER              PIC XX              VALUE SPACES.       CL**2
00149      03  D-CARR              PIC X.                                  CL**2
00150      03  FILLER              PIC X(2)            VALUE SPACES.    EL684
00151      03  D-GROUP             PIC X(6).                               CL**2
00152      03  FILLER              PIC X               VALUE SPACES.       CL**2
00153      03  D-STATE             PIC X(2).                               CL**2
00154      03  FILLER              PIC X               VALUE SPACES.       CL**2
00155      03  D-ACCT              PIC X(10).                           EL684
00156      03  FILLER              PIC X               VALUE SPACES.       CL**2
00157      03  VAR-PRT-AMT1        PIC ZZZZ,ZZZ.ZZ-.                       CL**2
00158      03  FILLER              PIC XX             VALUE SPACES.        CL**2
00159      03  VAR-PRT-AMT2        PIC ZZZZ,ZZZ.ZZ-.                       CL**2
00160      03  FILLER              PIC XX              VALUE SPACES.       CL**2
00161      03  VAR-PRT-AMT3        PIC ZZZZ,ZZZ.ZZ-.                       CL**2
00162      03  FILLER              PIC XX              VALUE SPACES.       CL**2
00163      03  VAR-PRT-AMT4        PIC ZZZZ,ZZZ.ZZ-.                       CL**2
00164                                                                   EL684
00165      EJECT                                                        EL684
00166      COPY ELCAID.                                                    CL**2
00167  01  PF-AID REDEFINES DFHAID.                                     EL684
00168      05  FILLER                      PIC X(8).                    EL684
00169      05  PF-VALUES  OCCURS 24        PIC X.                       EL684
00170      EJECT                                                        EL684
00171      COPY ELCINTF.                                                   CL**2
00172      12  PI-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.             EL684
00173          16  PI-LO-DATE             PIC XX.                          CL**2
00174          16  PI-HI-DATE             PIC XX.                          CL**2
00175                                                                   EL684
00176          16  PI-ERPYAJ-KEY.                                       EL684
00177              20  PI-PYA-COMPANY-CD  PIC X.                        EL684
00178              20  PI-PYA-CODE        PIC X.                        EL684
00179              20  PI-PYA-TABLE       PIC X(3).                     EL684
00180                                                                   EL684
00181          16  PI-SAVE-ERPYAJ-KEY     PIC X(5).                     EL684
00182                                                                   EL684
00183          16  PI-BROWSE-SW           PIC X.                        EL684
00184              88  BROWSE-STARTED              VALUE 'Y'.           EL684
00185          16  PI-ERPYAJ-EOF-SW       PIC X.                        EL684
00186              88  ERPYAJ-EOF                  VALUE 'Y'.           EL684
00187          16  PI-EXCESS-SW           PIC X.                        EL684
00188              88  EXCESS-LEVEL-EXISTS         VALUE 'X'.           EL684
00189          16  PI-COMPANY-ADD-SW      PIC X.                        EL684
00190              88  COMPANY-RECORD-ADDED        VALUE 'Y'.           EL684
00191                                                                   EL684
00192          16  PI-SUB                 PIC S99.                      EL684
00193          16  PI-LAST-LEVEL          PIC S99.                      EL684
00194          16  FILLER                 PIC X(618).                      CL**2
00195                                                                   EL684
00196      EJECT                                                        EL684
00197  LINKAGE SECTION.                                                 EL684
00198  01  DFHCOMMAREA                     PIC X(1024).                 EL684
00199 *01 PARM-LIST .                                                      CL**2
00200 *    02  FILLER              PIC S9(8)   COMP.                       CL**2
00201 *    02  ERPYAJ-POINTER      PIC S9(8)   COMP.                       CL**2
00202 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                       CL**2
00203      EJECT                                                        EL684
00204      COPY ERCREPY.                                                   CL**2
00205                                                                   EL684
00206      COPY ELCCNTL.                                                   CL**2
00207      EJECT                                                        EL684
00208                                                                   EL684
00209  PROCEDURE DIVISION.                                              EL684
00210                                                                   EL684
00211      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL684
00212      MOVE '5'                   TO DC-OPTION-CODE.                EL684
00213      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL684
00214      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL684
00215      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL684
00216      MOVE DC-GREG-DATE-1-ALPHA  TO  SAVE-DATE-ALPHA.              EL684
00217                                                                   EL684
00218      MOVE DFHCOMMAREA           TO  PROGRAM-INTERFACE-BLOCK.      EL684
00219      MOVE 'EL684'               TO  WS-REPORT-ID.                 EL684
00220                                                                   EL684
00221  1000-START.                                                      EL684
00222      EXEC CICS  HANDLE CONDITION                                  EL684
00223             ERROR    (8800-ABEND)                                 EL684
00224             PGMIDERR (8900-PGMIDERR)                              EL684
00225      END-EXEC.                                                    EL684
00226                                                                   EL684
00227  2000-RECEIVE.                                                    EL684
00228      EXEC CICS RETRIEVE                                           EL684
00229          INTO   (PROGRAM-INTERFACE-BLOCK)                         EL684
00230          LENGTH (CLEN)                                            EL684
00231      END-EXEC.                                                    EL684
00232                                                                   EL684
00233  2000-CHECK-IN-PROGRESS.                                          EL684
00234      EXEC CICS  HANDLE CONDITION                                  EL684
00235             NOTFND   (2000-WRITE-INITIAL-TRAILER)                 EL684
00236      END-EXEC.                                                    EL684
00237                                                                   EL684
00238      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL684
00239      MOVE 'RF'                   TO  RF-RECORD-ID.                EL684
00240      MOVE '2'                    TO  RF-RECORD-TYPE.              EL684
00241      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                EL684
00242      MOVE ZEROS                  TO  RF-LINE-NUMBER.              EL684
00243                                                                   EL684
00244      EXEC CICS READ                                               EL684
00245          DATASET (REPT-FILE-ID)                                   EL684
00246          INTO    (REPORT-SAVE-FILE)                               EL684
00247          RIDFLD  (RF-CONTROL-PRIMARY)                             EL684
00248      END-EXEC.                                                    EL684
00249                                                                   EL684
00250 ********IF RECORD FOUND, THEN ANOTHER REPORT HAS ALREADY             CL**2
00251 ********BEEN STARTED.  IF PREVIOUS REPORT ABENDED AND DIDN'T         CL**2
00252 ********COMPLETE, THEN OPERATOR MUST PURGE REPORT AND CREATE         CL**2
00253 ********A NEW ONE.                                                   CL**2
00254      GO TO 9999-RETURN-CICS.                                         CL**2
00255                                                                   EL684
00256  2000-WRITE-INITIAL-TRAILER.                                      EL684
00257      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL684
00258      MOVE 'RF'                   TO  RF-RECORD-ID.                EL684
00259      MOVE '2'                    TO  RF-RECORD-TYPE.              EL684
00260      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                EL684
00261      MOVE ZEROS                  TO  RF-LINE-NUMBER.              EL684
00262                                                                   EL684
00263      MOVE SPACES                 TO  RF-TRAILER-RECORD.           EL684
00264                                                                      CL**2
00265      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL**2
00266      END-EXEC                                                        CL**2
00267      EXEC CICS FORMATTIME                                            CL**2
00268                ABSTIME(LCP-CICS-TIME)                                CL**2
00269                TIME(LCP-TIME-OF-DAY-XX)                              CL**2
00270      END-EXEC                                                        CL**2
00271      MOVE LCP-TIME-OF-DAY-68     TO  RF-PRINT-HH-MM-SS.              CL**2
00272      MOVE 'STARTED'              TO  RF-CURRENT-DATE.             EL684
00273                                                                   EL684
00274      EXEC CICS WRITE                                              EL684
00275          DATASET (REPT-FILE-ID)                                   EL684
00276          FROM    (REPORT-SAVE-FILE)                               EL684
00277          RIDFLD  (RF-CONTROL-PRIMARY)                             EL684
00278      END-EXEC.                                                    EL684
00279                                                                   EL684
00280  2100-DELETE-REC.                                                 EL684
00281      MOVE 1 TO RF-LINE-NUMBER.                                    EL684
00282                                                                   EL684
00283      EXEC CICS  HANDLE CONDITION                                  EL684
00284             NOTFND   (2300-DELETE-REC)                            EL684
00285      END-EXEC.                                                    EL684
00286                                                                   EL684
00287  2200-DELETE-1.                                                   EL684
00288      MOVE PI-COMPANY-CD TO RF-COMPANY-CD.                         EL684
00289      MOVE 'RF'          TO RF-RECORD-ID.                          EL684
00290      MOVE '1'           TO RF-RECORD-TYPE.                        EL684
00291      MOVE WS-REPORT-ID  TO RF-REPORT-ID.                          EL684
00292                                                                   EL684
00293      EXEC CICS DELETE                                             EL684
00294          DATASET   (REPT-FILE-ID)                                 EL684
00295          RIDFLD    (RF-CONTROL-PRIMARY)                           EL684
00296          KEYLENGTH (11)                                           EL684
00297      END-EXEC.                                                    EL684
00298                                                                   EL684
00299      ADD 1 TO RF-LINE-NUMBER.                                     EL684
00300      GO TO 2200-DELETE-1.                                         EL684
00301                                                                   EL684
00302  2300-DELETE-REC.                                                 EL684
00303      EXEC CICS  HANDLE CONDITION                                  EL684
00304             NOTFND   (3000-START-BROWSE)                          EL684
00305      END-EXEC.                                                    EL684
00306                                                                   EL684
00307  2400-DELETE-2.                                                   EL684
00308      MOVE PI-COMPANY-CD TO RF-COMPANY-CD.                         EL684
00309      MOVE 'RF'          TO RF-RECORD-ID.                          EL684
00310      MOVE '2'           TO RF-RECORD-TYPE.                        EL684
00311      MOVE WS-REPORT-ID  TO RF-REPORT-ID.                          EL684
00312                                                                   EL684
00313      EXEC CICS DELETE                                             EL684
00314          DATASET   (REPT-FILE-ID)                                 EL684
00315          RIDFLD    (RF-CONTROL-PRIMARY)                           EL684
00316          KEYLENGTH (11)                                           EL684
00317      END-EXEC.                                                    EL684
00318                                                                   EL684
00319      ADD 1 TO RF-LINE-NUMBER.                                     EL684
00320      GO TO 2400-DELETE-2.                                         EL684
00321                                                                   EL684
00322  3000-START-BROWSE.                                               EL684
00323      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL684
00324      MOVE SPACES                 TO CNTL-ACCESS.                  EL684
00325      MOVE '1'                    TO CNTL-REC-TYPE.                EL684
00326      MOVE +0                     TO CNTL-SEQ-NO.                  EL684
00327                                                                   EL684
00328      PERFORM 9000-READ-CONTROL THRU 9000-EXIT.                    EL684
00329                                                                   EL684
00330      MOVE CF-CL-MAIL-TO-NAME TO H2-COMP.                          EL684
00331      MOVE SAVE-DATE          TO H2-DATE.                          EL684
00332      MOVE ' THRU '           TO H3-THRU.                          EL684
00333                                                                   EL684
00334      MOVE SPACE              TO DC-OPTION-CODE.                   EL684
00335      MOVE PI-LO-DATE         TO DC-BIN-DATE-1.                    EL684
00336      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL684
00337      MOVE DC-GREG-DATE-1-EDIT TO H3-DATE1.                        EL684
00338                                                                   EL684
00339      IF PI-LO-DATE = LOW-VALUES                                   EL684
00340         MOVE 'INCEPT.'        TO H3-DATE1.                        EL684
00341                                                                   EL684
00342      MOVE SPACE               TO DC-OPTION-CODE.                  EL684
00343      MOVE PI-HI-DATE          TO DC-BIN-DATE-1.                   EL684
00344      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL684
00345      MOVE DC-GREG-DATE-1-EDIT TO H3-DATE2.                        EL684
00346                                                                   EL684
00347      IF PI-HI-DATE = HIGH-VALUES                                  EL684
00348         MOVE 'CURRENT'        TO H3-DATE2.                        EL684
00349                                                                   EL684
00350      PERFORM 6500-HDR-RTN THRU 6500-H-R-X.                        EL684
00351                                                                   EL684
00352      EXEC CICS  HANDLE CONDITION                                  EL684
00353             NOTFND   (4800-NO-DATA)                               EL684
00354      END-EXEC.                                                    EL684
00355                                                                   EL684
00356      MOVE LOW-VALUES    TO PI-ERPYAJ-KEY.                         EL684
00357      MOVE PI-COMPANY-CD TO PI-PYA-COMPANY-CD.                     EL684
00358                                                                   EL684
00359      EXEC CICS STARTBR                                            EL684
00360           DATASET  (PYAJ-FILE-ID)                                 EL684
00361           RIDFLD   (PI-ERPYAJ-KEY)                                EL684
00362       END-EXEC.                                                   EL684
00363                                                                   EL684
00364      EXEC CICS  HANDLE CONDITION                                  EL684
00365             ENDFILE  (4500-ENDBROWSE)                             EL684
00366      END-EXEC.                                                    EL684
00367                                                                   EL684
00368  4000-READNEXT.                                                   EL684
00369      EXEC CICS READNEXT                                           EL684
00370           DATASET  (PYAJ-FILE-ID)                                 EL684
00371           SET      (ADDRESS OF PENDING-RETRO-REIN-ADJUSTMENTS)       CL**2
00372           RIDFLD   (PI-ERPYAJ-KEY)                                EL684
00373      END-EXEC.                                                    EL684
00374                                                                   EL684
00375      IF PI-COMPANY-CD = RP-COMPANY-CD                             EL684
00376          IF RP-CREDIT-ACCEPT-DT = LOW-VALUES                      EL684
00377              GO TO 5000-PRINT-IT                                  EL684
00378          ELSE                                                     EL684
00379              GO TO 4000-READNEXT.                                 EL684
00380                                                                   EL684
00381  4500-ENDBROWSE.                                                  EL684
00382 *    MOVE SPACES           TO DATA-1.                             EL684
00383 *    MOVE '* SUB-TOTAL'    TO D-TOTAL.                            EL684
00384 *    MOVE SUB-ENTRY-AMT    TO D-AMOUNT.                           EL684
00385 *    MOVE ' '              TO RF-CTL-CHAR-133.                    EL684
00386 *    MOVE DATA-1           TO RF-DATA-133.                        EL684
00387 *    PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL684
00388 *    MOVE SPACES           TO DATA-1.                             EL684
00389 *    MOVE '** GRAND TOTAL' TO D-TOTAL.                            EL684
00390 *    MOVE TOT-ENTRY-AMT    TO D-AMOUNT.                           EL684
00391 *    MOVE '-'              TO RF-CTL-CHAR-133.                    EL684
00392 *    MOVE DATA-1           TO RF-DATA-133.                        EL684
00393 *    PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL684
00394 *    MOVE '1'              TO RF-CTL-CHAR-133.                    EL684
00395 *    MOVE SPACES           TO RF-DATA-133.                        EL684
00396 *    PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL684
00397                                                                   EL684
00398  4600-DELETE-INITIAL-2.                                           EL684
00399      MOVE PI-COMPANY-CD TO RF-COMPANY-CD.                         EL684
00400      MOVE 'RF'          TO RF-RECORD-ID.                          EL684
00401      MOVE '2'           TO RF-RECORD-TYPE.                        EL684
00402      MOVE WS-REPORT-ID  TO RF-REPORT-ID.                          EL684
00403      MOVE ZEROS         TO RF-LINE-NUMBER.                        EL684
00404                                                                   EL684
00405      EXEC CICS DELETE                                             EL684
00406          DATASET   (REPT-FILE-ID)                                 EL684
00407          RIDFLD    (RF-CONTROL-PRIMARY)                           EL684
00408          KEYLENGTH (11)                                           EL684
00409      END-EXEC.                                                    EL684
00410                                                                   EL684
00411      IF WS-LINE-NUMBER = 3                                        EL684
00412          MOVE '*** NO DATA AT THIS TIME ***' TO DATA-1            EL684
00413          MOVE '0'    TO RF-CTL-CHAR-133                           EL684
00414          MOVE DATA-1 TO RF-DATA-133                               EL684
00415          PERFORM 7000-PRT-LINE THRU 7000-EXIT.                    EL684
00416                                                                   EL684
00417      PERFORM 8999-WRITE-TRAILER.                                  EL684
00418      GO TO 9999-RETURN-CICS.                                      EL684
00419                                                                   EL684
00420  4800-NO-DATA.                                                    EL684
00421      MOVE '*** NO DATA AT THIS TIME ***' TO DATA-1.               EL684
00422      MOVE '0'    TO RF-CTL-CHAR-133.                              EL684
00423      MOVE DATA-1 TO RF-DATA-133.                                  EL684
00424      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL684
00425      GO TO 4600-DELETE-INITIAL-2.                                 EL684
00426                                                                   EL684
00427  5000-PRINT-IT.                                                   EL684
00428      IF RP-LAST-MAINT-DT LESS    PI-LO-DATE OR                    EL684
00429         RP-LAST-MAINT-DT GREATER PI-HI-DATE                       EL684
00430         GO TO 4000-READNEXT.                                      EL684
00431                                                                   EL684
00432      IF PRT-CNT GREATER 55                                        EL684
00433          PERFORM 6500-HDR-RTN THRU 6500-H-R-X.                    EL684
00434                                                                   EL684
00435      MOVE SPACES      TO DATA-1.                                  EL684
00436      MOVE RP-CARRIER  TO RP-CARR   D-CARR.                        EL684
00437      MOVE RP-GROUPING TO RP-GROUP  D-GROUP.                       EL684
00438      MOVE RP-STATE    TO RP-STAT   D-STATE.                          CL**2
00439      MOVE RP-ACCOUNT  TO RP-ACCT   D-ACCT.                        EL684
00440      MOVE +1          TO SUB.                                        CL**2
00441                                                                   EL684
00442      MOVE ZEROS                      TO  VAR-AMT (1) VAR-AMT (2)     CL**2
00443                                          VAR-AMT (3) VAR-AMT (4)     CL**2
00444                                          VAR-AMT (5) VAR-AMT (6)     CL**2
00445                                          VAR-AMT (7) VAR-AMT (8)     CL**2
00446                                          VAR-AMT (9) VAR-AMT (10).   CL**2
00447                                                                   EL684
00448      MOVE SPACES                     TO  VAR-HDG (1) VAR-HDG (2)     CL**2
00449                                          VAR-HDG (3) VAR-HDG (4)     CL**2
00450                                          VAR-HDG (5) VAR-HDG (6)     CL**2
00451                                          VAR-HDG (7) VAR-HDG (8)     CL**2
00452                                          VAR-HDG (9) VAR-HDG (10).   CL**2
00453                                                                   EL684
00454      IF RP-INS-AMT-INFORCE IS NOT EQUAL TO 0                         CL**2
00455          MOVE RP-INS-AMT-INFORCE     TO  VAR-AMT (SUB)               CL**2
00456          MOVE '   INS. AMT '         TO  VAR-HDG (SUB)               CL**2
00457          ADD +1                      TO  SUB.                        CL**2
00458                                                                   EL684
00459      IF RP-LIFE-MORTALITY-AMT IS NOT EQUAL TO 0                      CL**2
00460          MOVE RP-LIFE-MORTALITY-AMT  TO  VAR-AMT (SUB)               CL**2
00461          MOVE PI-LIFE-OVERRIDE-L2    TO  WS-HDG-1A                   CL**2
00462          MOVE ' MORT. AMT'           TO  WS-HDG-1B                   CL**2
00463          MOVE WS-HEADING             TO  VAR-HDG (SUB)               CL**2
00464          ADD +1                      TO  SUB.                        CL**2
00465                                                                      CL**2
00466      IF RP-FUTURE-RESERVE IS NOT EQUAL TO 0                          CL**2
00467          MOVE RP-FUTURE-RESERVE      TO  VAR-AMT (SUB)               CL**2
00468          MOVE 'FUTURE RESV '         TO  VAR-HDG (SUB)               CL**2
00469          ADD +1                      TO  SUB.                        CL**2
00470                                                                      CL**2
00471      IF RP-PTC-RESERVE IS NOT EQUAL TO 0                             CL**2
00472          MOVE RP-PTC-RESERVE         TO  VAR-AMT (SUB)               CL**2
00473          MOVE '  PTC RESV  '         TO  VAR-HDG (SUB)               CL**2
00474          ADD +1                      TO  SUB.                        CL**2
00475                                                                      CL**2
00476      IF RP-IBNR-RESERVE IS NOT EQUAL TO 0                            CL**2
00477          MOVE RP-IBNR-RESERVE        TO  VAR-AMT (SUB)               CL**2
00478          MOVE ' IBNR RESV  '         TO  VAR-HDG (SUB)               CL**2
00479          ADD +1                      TO  SUB.                        CL**2
00480                                                                      CL**2
00481      IF RP-CLAIM-ADJ-AMT IS NOT EQUAL TO 0                           CL**2
00482          MOVE RP-CLAIM-ADJ-AMT       TO  VAR-AMT (SUB)               CL**2
00483          MOVE ' CLAIM ADJ  '         TO  VAR-HDG (SUB)               CL**2
00484          ADD +1                      TO  SUB.                        CL**2
00485                                                                      CL**2
00486      IF RP-EXPENSES IS NOT EQUAL TO 0                                CL**2
00487          MOVE RP-EXPENSES            TO  VAR-AMT (SUB)               CL**2
00488          MOVE '  EXPENSES  '         TO  VAR-HDG (SUB)               CL**2
00489          ADD +1                      TO  SUB.                        CL**2
00490                                                                      CL**2
00491      IF RP-PAYMENTS IS NOT EQUAL TO 0                                CL**2
00492          MOVE RP-PAYMENTS            TO  VAR-AMT (SUB)               CL**2
00493          MOVE '  PAYMENTS  '         TO  VAR-HDG (SUB)               CL**2
00494          ADD +1                      TO  SUB.                        CL**2
00495                                                                      CL**2
00496      IF RP-OTHER-COMM IS NOT EQUAL TO 0                              CL**2
00497          MOVE RP-OTHER-COMM          TO  VAR-AMT (SUB)               CL**2
00498          MOVE ' OTHER COMM '         TO  VAR-HDG (SUB)               CL**2
00499          ADD +1                      TO  SUB.                        CL**2
00500                                                                      CL**2
00501      IF RP-REIN-PREM-ADJ IS NOT EQUAL TO 0                           CL**2
00502          MOVE RP-REIN-PREM-ADJ       TO  VAR-AMT (SUB)               CL**2
00503          MOVE ' REIN PREM '          TO  VAR-HDG (SUB)               CL**2
00504          ADD +1                      TO  SUB.                        CL**2
00505                                                                      CL**2
00506  5010-PRINT-IT.                                                      CL**2
00507      IF VAR-AMT (1) IS EQUAL TO 0                                    CL**2
00508          GO TO 4000-READNEXT                                         CL**2
00509      ELSE                                                            CL**2
00510          MOVE VAR-AMT (1)            TO  VAR-PRT-AMT1                CL**2
00511          MOVE VAR-HDG (1)            TO  VAR-PRT-HDG1.               CL**2
00512                                                                      CL**2
00513      IF VAR-AMT (2) IS EQUAL TO 0                                    CL**2
00514          PERFORM 6000-PRINT-IT THRU 6000-EXIT                        CL**2
00515          GO TO 4000-READNEXT                                         CL**2
00516      ELSE                                                            CL**2
00517          MOVE VAR-AMT (2)            TO  VAR-PRT-AMT2                CL**2
00518          MOVE VAR-HDG (2)            TO  VAR-PRT-HDG2.               CL**2
00519                                                                      CL**2
00520      IF VAR-AMT (3) IS EQUAL TO 0                                    CL**2
00521          PERFORM 6000-PRINT-IT THRU 6000-EXIT                        CL**2
00522          GO TO 4000-READNEXT                                         CL**2
00523      ELSE                                                            CL**2
00524          MOVE VAR-AMT (3)            TO  VAR-PRT-AMT3                CL**2
00525          MOVE VAR-HDG (3)            TO  VAR-PRT-HDG3.               CL**2
00526                                                                      CL**2
00527      IF VAR-AMT (4) IS EQUAL TO 0                                    CL**2
00528          PERFORM 6000-PRINT-IT THRU 6000-EXIT                        CL**2
00529          GO TO 4000-READNEXT                                         CL**2
00530      ELSE                                                            CL**2
00531          MOVE VAR-AMT (4)            TO  VAR-PRT-AMT4                CL**2
00532          MOVE VAR-HDG (4)            TO  VAR-PRT-HDG4.               CL**2
00533                                                                      CL**2
00534      PERFORM 6000-PRINT-IT THRU 6000-EXIT.                           CL**2
00535                                                                      CL**2
00536      MOVE ZEROS                      TO  VAR-PRT-AMT1                CL**2
00537                                          VAR-PRT-AMT2                CL**2
00538                                          VAR-PRT-AMT3                CL**2
00539                                          VAR-PRT-AMT4.               CL**2
00540                                                                      CL**2
00541      MOVE SPACES                     TO  VAR-PRT-HDG1                CL**2
00542                                          VAR-PRT-HDG2                CL**2
00543                                          VAR-PRT-HDG3                CL**2
00544                                          VAR-PRT-HDG4.               CL**2
00545                                                                      CL**2
00546      IF VAR-AMT (5) IS EQUAL TO 0                                    CL**2
00547          GO TO 4000-READNEXT                                         CL**2
00548      ELSE                                                            CL**2
00549          MOVE VAR-AMT (5)            TO  VAR-PRT-AMT1                CL**2
00550          MOVE VAR-HDG (5)            TO  VAR-PRT-HDG1.               CL**2
00551                                                                      CL**2
00552      IF VAR-AMT (6) IS EQUAL TO 0                                    CL**2
00553          PERFORM 6000-PRINT-IT THRU 6000-EXIT                        CL**2
00554          GO TO 4000-READNEXT                                         CL**2
00555      ELSE                                                            CL**2
00556          MOVE VAR-AMT (6)            TO  VAR-PRT-AMT2                CL**2
00557          MOVE VAR-HDG (6)            TO  VAR-PRT-HDG2.               CL**2
00558                                                                      CL**2
00559      IF VAR-AMT (7) IS EQUAL TO 0                                    CL**2
00560          PERFORM 6000-PRINT-IT THRU 6000-EXIT                        CL**2
00561          GO TO 4000-READNEXT                                         CL**2
00562      ELSE                                                            CL**2
00563          MOVE VAR-AMT (7)            TO  VAR-PRT-AMT3                CL**2
00564          MOVE VAR-HDG (7)            TO  VAR-PRT-HDG3.               CL**2
00565                                                                      CL**2
00566      IF VAR-AMT (8) IS EQUAL TO 0                                    CL**2
00567          PERFORM 6000-PRINT-IT THRU 6000-EXIT                        CL**2
00568          GO TO 4000-READNEXT                                         CL**2
00569      ELSE                                                            CL**2
00570          MOVE VAR-AMT (8)            TO  VAR-PRT-AMT4                CL**2
00571          MOVE VAR-HDG (8)            TO  VAR-PRT-HDG4.               CL**2
00572                                                                      CL**2
00573      PERFORM 6000-PRINT-IT THRU 6000-EXIT.                           CL**2
00574                                                                      CL**2
00575      MOVE ZEROS                      TO  VAR-PRT-AMT1                CL**2
00576                                          VAR-PRT-AMT2                CL**2
00577                                          VAR-PRT-AMT3                CL**2
00578                                          VAR-PRT-AMT4.               CL**2
00579                                                                      CL**2
00580      MOVE SPACES                     TO  VAR-PRT-HDG1                CL**2
00581                                          VAR-PRT-HDG2                CL**2
00582                                          VAR-PRT-HDG3                CL**2
00583                                          VAR-PRT-HDG4.               CL**2
00584                                                                      CL**2
00585      IF VAR-AMT (9) IS EQUAL TO 0                                    CL**2
00586          GO TO 4000-READNEXT                                         CL**2
00587      ELSE                                                            CL**2
00588          MOVE VAR-AMT (9)            TO  VAR-PRT-AMT1                CL**2
00589          MOVE VAR-HDG (9)            TO  VAR-PRT-HDG1.               CL**2
00590                                                                      CL**2
00591      IF VAR-AMT (10) IS EQUAL TO 0                                   CL**2
00592          PERFORM 6000-PRINT-IT THRU 6000-EXIT                        CL**2
00593          GO TO 4000-READNEXT                                         CL**2
00594      ELSE                                                            CL**2
00595          MOVE VAR-AMT (10)           TO  VAR-PRT-AMT2                CL**2
00596          MOVE VAR-HDG (10)           TO  VAR-PRT-HDG2.               CL**2
00597                                                                      CL**2
00598      PERFORM 6000-PRINT-IT THRU 6000-EXIT.                           CL**2
00599      GO TO 4000-READNEXT.                                         EL684
00600                                                                      CL**2
00601  6000-PRINT-IT.                                                      CL**2
00602                                                                      CL**2
00603      MOVE '0'                    TO  RF-CTL-CHAR-133.                CL**2
00604      MOVE HDR-4                  TO  RF-DATA-133.                    CL**2
00605      PERFORM 7000-PRT-LINE THRU 7000-EXIT                            CL**2
00606      MOVE ' '                    TO  RF-CTL-CHAR-133.                CL**2
00607      MOVE DATA-1                 TO  RF-DATA-133.                    CL**2
00608      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                           CL**2
00609      ADD 1                       TO  PRT-CNT.                        CL**2
00610                                                                      CL**2
00611  6000-EXIT.                                                          CL**2
00612      EXIT.                                                           CL**2
00613                                                                   EL684
00614  6500-HDR-RTN.                                                    EL684
00615      ADD +1  TO WS-PAGE.                                          EL684
00616      MOVE WS-PAGE                TO H3-PAGE.                      EL684
00617      MOVE ZERO                   TO PRT-CNT.                      EL684
00618      MOVE '1'                    TO RF-CTL-CHAR-133.              EL684
00619      MOVE HDR-1                  TO RF-DATA-133.                  EL684
00620      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL684
00621                                                                   EL684
00622      MOVE ' '                    TO RF-CTL-CHAR-133.              EL684
00623      MOVE HDR-2                  TO RF-DATA-133.                  EL684
00624      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL684
00625                                                                   EL684
00626      MOVE ' '                    TO RF-CTL-CHAR-133.              EL684
00627      MOVE HDR-3                  TO RF-DATA-133.                  EL684
00628      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL684
00629                                                                   EL684
00630      MOVE '0'                    TO RF-CTL-CHAR-133.              EL684
00631                                                                   EL684
00632  6500-H-R-X.                                                      EL684
00633      EXIT.                                                        EL684
00634                                                                   EL684
00635  7000-PRT-LINE.                                                   EL684
00636      MOVE PI-COMPANY-CD  TO RF-COMPANY-CD.                        EL684
00637      MOVE 'RF'           TO RF-RECORD-ID.                         EL684
00638      MOVE '1'            TO RF-RECORD-TYPE.                       EL684
00639      MOVE WS-REPORT-ID   TO RF-REPORT-ID.                         EL684
00640      ADD 1               TO WS-LINE-NUMBER.                       EL684
00641      MOVE WS-LINE-NUMBER TO RF-LINE-NUMBER.                       EL684
00642                                                                   EL684
00643      EXEC CICS WRITE                                              EL684
00644          DATASET (REPT-FILE-ID)                                   EL684
00645          FROM    (REPORT-SAVE-FILE)                               EL684
00646          RIDFLD  (RF-CONTROL-PRIMARY)                             EL684
00647      END-EXEC.                                                    EL684
00648                                                                   EL684
00649  7000-EXIT.                                                       EL684
00650       EXIT.                                                       EL684
00651                                                                   EL684
00652  8500-DATE-CONVERT.                                               EL684
00653      MOVE LINK-ELDATCV           TO PGM-NAME.                     EL684
00654                                                                   EL684
00655      EXEC CICS LINK                                               EL684
00656          PROGRAM    (PGM-NAME)                                    EL684
00657          COMMAREA   (DATE-CONVERSION-DATA)                        EL684
00658          LENGTH     (DC-COMM-LENGTH)                              EL684
00659      END-EXEC.                                                    EL684
00660                                                                   EL684
00661  8500-EXIT.                                                       EL684
00662      EXIT.                                                        EL684
00663                                                                   EL684
00664  8800-ABEND.                                                      EL684
00665      MOVE DFHEIBLK TO EMI-LINE1.                                  EL684
00666                                                                   EL684
00667      EXEC CICS LINK                                               EL684
00668          PROGRAM   ('EL004')                                      EL684
00669          COMMAREA  (EMI-LINE1)                                    EL684
00670          LENGTH    (72)                                           EL684
00671      END-EXEC.                                                    EL684
00672                                                                   EL684
00673      GO TO 9999-RETURN-CICS.                                      EL684
00674                                                                   EL684
00675  8900-PGMIDERR.                                                   EL684
00676      GO TO 9999-RETURN-CICS.                                      EL684
00677                                                                   EL684
00678  8999-WRITE-TRAILER.                                              EL684
00679      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL684
00680      MOVE 'RF'                   TO  RF-RECORD-ID.                EL684
00681      MOVE '2'                    TO  RF-RECORD-TYPE.              EL684
00682      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                EL684
00683      ADD +1                      TO  WS-LINE-NUMBER.              EL684
00684      MOVE WS-LINE-NUMBER         TO  RF-LINE-NUMBER.              EL684
00685                                                                   EL684
00686      MOVE SPACES                 TO  RF-TRAILER-RECORD.           EL684
00687      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL**2
00688      END-EXEC                                                        CL**2
00689      EXEC CICS FORMATTIME                                            CL**2
00690                ABSTIME(LCP-CICS-TIME)                                CL**2
00691                TIME(LCP-TIME-OF-DAY-XX)                              CL**2
00692      END-EXEC                                                        CL**2
00693      MOVE  LCP-TIME-OF-DAY-68 TO RF-PRINT-HH-MM-SS.                  CL**2
00694      MOVE SAVE-DATE              TO  RF-CURRENT-DATE.             EL684
00695                                                                   EL684
00696      EXEC CICS WRITE                                              EL684
00697          DATASET (REPT-FILE-ID)                                   EL684
00698          FROM    (REPORT-SAVE-FILE)                               EL684
00699          RIDFLD  (RF-CONTROL-PRIMARY)                             EL684
00700      END-EXEC.                                                    EL684
00701                                                                   EL684
00702  9000-READ-CONTROL.                                               EL684
00703      EXEC CICS READ                                               EL684
00704          DATASET     (CNTL-ID)                                    EL684
00705          SET         (ADDRESS OF CONTROL-FILE)                       CL**2
00706          RIDFLD      (ELCNTL-KEY)                                 EL684
00707      END-EXEC.                                                    EL684
00708                                                                   EL684
00709  9000-EXIT.                                                       EL684
00710       EXIT.                                                       EL684
00711                                                                   EL684
00712                                                                   EL684
00713  9999-RETURN-CICS.                                                EL684
00714         EXEC CICS  RETURN                                         EL684
00715              END-EXEC.                                            EL684
00716                                                                   EL684
00717  9999-EXIT.                                                       EL684
00718       EXIT.                                                       EL684
00719                                                                   EL684
