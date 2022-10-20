00001  IDENTIFICATION DIVISION.                                         03/06/96
00002                                                                   EL683
00003  PROGRAM-ID.                 EL683 .                                 LV004
00004 *              PROGRAM CONVERTED BY                                  CL**4
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**4
00006 *              CONVERSION DATE 02/14/96 08:00:32.                    CL**4
00007 *                            VMOD=2.004                              CL**4
00008 *                                                                 EL683
00008 *                                                                 EL683
00009 *AUTHOR.     LOGIC INC.                                              CL**4
00010 *            DALLAS, TEXAS.                                          CL**4
00011                                                                   EL683
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
00022                                                                   EL683
00023 *REMARKS.    TRANS - EXF9 - COMMISSION TABLE PRINT                   CL**4
00024 *            STARTED FROM EL671                                      CL**4
00025                                                                   EL683
00026      EJECT                                                        EL683
00027  ENVIRONMENT DIVISION.                                            EL683
00028  DATA DIVISION.                                                   EL683
00029  WORKING-STORAGE SECTION.                                         EL683
00030  01  LCP-TIME-OF-DAY-XX.                                             CL**4
00031      05  LCP-TIME-OF-DAY-68        PIC 9(6).                         CL**4
00032      05  FILLER                    PIC 99.                           CL**4
00033  01  LCP-CICS-TIME                 PIC 9(15).                        CL**4
00034  77  FILLER  PIC X(32) VALUE '********************************'.  EL683
00035  77  FILLER  PIC X(32) VALUE '     EL683  WORKING-STORAGE     '.  EL683
00036  77  FILLER  PIC X(32) VALUE '********** V/M 2.004 ***********'.     CL**4
00037                                                                   EL683
00038  77  CLEN            PIC S9(4)  COMP    VALUE 1024.               EL683
00039  77  CTR             PIC S99    COMP    VALUE +0.                 EL683
00040  77  X1              PIC S999   COMP    VALUE +0.                 EL683
00041  77  X2              PIC S999   COMP    VALUE +0.                 EL683
00042  77  B1              PIC S999   COMP    VALUE +1.                 EL683
00043  77  X               PIC X              VALUE SPACE.              EL683
00044  77  SUB             PIC S9(3)  COMP    VALUE +0.                 EL683
00045  77  CT-LINE-COUNT   PIC S9(3)  COMP-3  VALUE +99.                EL683
00046  77  CT-PAGE-COUNT   PIC S9(5)  COMP-3  VALUE +0.                 EL683
00047                                                                   EL683
00048  01  WS-DATE-AREA.                                                EL683
00049      03  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL683
00050      03  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.        EL683
00051      03  SAVE-DATE-ALPHA         PIC X(18)   VALUE SPACES.        EL683
00052                                                                   EL683
00053  01  WORK-AREAS.                                                  EL683
00054      03  WS-REPORT-ID            PIC X(6)    VALUE SPACES.        EL683
00055      03  PGM-NAME                PIC X(8)    VALUE SPACES.        EL683
00056      03  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     EL683
00057      03  EMI-LINE1               PIC X(72).                       EL683
00058      03  WS-NEXT-TRAN            PIC X(4).                        EL683
00059      03  WS-TERMINAL-ID.                                          EL683
00060          05  WS-TERM-PREFIX      PIC XX.                          EL683
00061          05  FILLER              PIC XX.                          EL683
00062      03  PRT-CNT                 PIC S9(3)   VALUE +6   COMP-3.   EL683
00063      03  WS-LINE-NUMBER          PIC S9(7)   VALUE ZERO COMP-3.   EL683
00064      03  WS-PAGE                 PIC S9(5)   VALUE ZERO COMP-3.   EL683
00065      03  WS-REPORT-SW            PIC S9      VALUE ZERO COMP-3.   EL683
00066      03  WS-PRINT-SW             PIC S9      VALUE ZERO COMP-3.   EL683
00067      03  REPT-FILE-ID            PIC X(8)    VALUE 'ELREPT'.      EL683
00068      03  COMM-FILE-ID            PIC X(8)    VALUE 'ERCTBL'.      EL683
00069      03  CNTL-ID                 PIC X(8)    VALUE 'ELCNTL'.      EL683
00070      03  B-REC-FLAG              PIC X       VALUE SPACES.        EL683
00071      03  WS-DATE.                                                 EL683
00072          05  WS-YR               PIC XX.                          EL683
00073          05  WS-MO               PIC XX.                          EL683
00074          05  WS-DA               PIC XX.                          EL683
00075      03  ABEND-AREA              PIC X(72).                       EL683
00076                                                                   EL683
00077  01  ACCESS-KEYS.                                                 EL683
00078      03  ELCNTL-KEY.                                              EL683
00079          05  CNTL-COMP-ID         PIC X(3).                       EL683
00080          05  CNTL-REC-TYPE        PIC X.                          EL683
00081          05  CNTL-ACCESS          PIC X(4).                       EL683
00082          05  CNTL-SEQ-NO          PIC S9(4)    COMP.              EL683
00083                                                                   EL683
00084                           COPY ELCREPT.                              CL**3
00085      EJECT                                                        EL683
00086                           COPY ELCDATE.                              CL**3
00087      EJECT                                                        EL683
00088                                                                   EL683
00089  01  PRT-LINES.                                                   EL683
00090      03  HDR-1.                                                   EL683
00091          05  FILLER          PIC X(27)   VALUE SPACES.            EL683
00092          05  FILLER          PIC X(40)   VALUE                    EL683
00093                  'COMMISSION TABLES'.                             EL683
00094          05  FILLER          PIC X(8)    VALUE 'EL - 683'.        EL683
00095                                                                   EL683
00096      03  HDR-2.                                                   EL683
00097          05  FILLER          PIC X(29)   VALUE SPACES.            EL683
00098          05  H2-COMP         PIC X(30)   VALUE 'LOGIC, INC.'.     EL683
00099          05  FILLER          PIC X(8)    VALUE SPACES.            EL683
00100          05  H2-DATE         PIC X(8).                            EL683
00101                                                                   EL683
00102      03  HDR-3.                                                   EL683
00103          05  FILLER          PIC X(27)   VALUE SPACES.            EL683
00104          05  H3-DATE.                                             EL683
00105            07  H3-DATE1      PIC X(8)    VALUE SPACES.            EL683
00106            07  H3-THRU       PIC X(6)    VALUE SPACES.            EL683
00107            07  H3-DATE2      PIC X(8)    VALUE SPACES.            EL683
00108          05  FILLER          PIC X(15)   VALUE SPACES.            EL683
00109          05  FILLER          PIC X(5)    VALUE 'PAGE '.           EL683
00110          05  H3-PAGE         PIC ZZ,ZZ9.                          EL683
00111                                                                   EL683
00112      03  HDR-4.                                                   EL683
00113          05  FILLER      PIC X(20)   VALUE ' COMMISSION TABLE -'. EL683
00114          05  H4-TAB      PIC X(3).                                EL683
00115          05  FILLER      PIC XX      VALUE SPACE.                 EL683
00116          05  FILLER      PIC X(14)   VALUE 'BENEFIT TYPE--'.      EL683
00117          05  H5-TYPE     PIC X(5).                                EL683
00118          05  FILLER      PIC X       VALUE SPACE.                 EL683
00119          05  FILLER      PIC X(8)    VALUE '..CODE--'.            EL683
00120          05  H6-CODE     PIC XX.                                  EL683
00121          05  FILLER      PIC X       VALUE SPACE.                 EL683
00122          05  FILLER      PIC X(9)    VALUE '..DESC.--'.           EL683
00123          05  H7-DESC     PIC X(13)   VALUE 'DONT KNOW YET'.       EL683
00124                                                                   EL683
00125      03  HDR-5.                                                   EL683
00126          05  FILLER      PIC X(26)   VALUE                        EL683
00127            '  BENEFIT **** TERMS **** '.                          EL683
00128          05  FILLER      PIC X(26)   VALUE                        EL683
00129            '  BENEFIT **** TERMS **** '.                          EL683
00130          05  FILLER      PIC X(26)   VALUE                        EL683
00131            '  BENEFIT **** TERMS **** '.                          EL683
00132                                                                   EL683
00133      03  HDR-6.                                                   EL683
00134          05  H6-X        OCCURS 3 TIMES.                          EL683
00135              07  H6-BEN     PIC ZZZZ,ZZZ.ZZ-.                        CL**3
00136              07  FILLER     PIC X.                                   CL**3
00137              07  H6-TRM1    PIC ZZZ.                              EL683
00138              07  FILLER     PIC X.                                EL683
00139              07  H6-TRM2    PIC ZZZ.                              EL683
00140              07  FILLER     PIC X.                                EL683
00141              07  H6-TRM3    PIC ZZZ.                              EL683
00142              07  FILLER     PIC XX.                               EL683
00143                                                                   EL683
00144      03  HDR-7.                                                   EL683
00145          05  H7-X       OCCURS 3 TIMES.                           EL683
00146              07  FILLER     PIC X.                                EL683
00147              07  H7-CD      PIC X.                                EL683
00148              07  H7-AGE     PIC ZZ.                               EL683
00149              07  FILLER     PIC X.                                EL683
00150              07  H7-PC1A.                                         EL683
00151                  09  H7-PC1 PIC .ZZZZZ.                           EL683
00152              07  FILLER     PIC X.                                EL683
00153              07  H7-PC2A.                                         EL683
00154                  09  H7-PC2 PIC .ZZZZZ.                           EL683
00155              07  FILLER     PIC X.                                EL683
00156              07  H7-PC3A.                                         EL683
00157                  09  H7-PC3 PIC .ZZZZZ.                           EL683
00158                                                                   EL683
00159      EJECT                                                        EL683
00160                                      COPY ELCAID.                    CL**3
00161  01  PF-AID REDEFINES DFHAID.                                     EL683
00162      05  FILLER                      PIC X(8).                    EL683
00163      05  PF-VALUES  OCCURS 24        PIC X.                       EL683
00164      EJECT                                                        EL683
00165                                      COPY ELCINTF.                   CL**3
00166      12  PI-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.             EL683
00167          16  PI-LO-DATE             PIC XX.                       EL683
00168          16  PI-HI-DATE             PIC XX.                       EL683
00169                                                                   EL683
00170          16  PI-ERCOMM-KEY.                                       EL683
00171              20  PI-COM-COMPANY-CD  PIC X.                        EL683
00172              20  PI-COM-CODE        PIC X.                        EL683
00173              20  PI-COM-TABLE       PIC X(3).                     EL683
00174                                                                   EL683
00175          16  PI-SAVE-ERCOMM-KEY     PIC X(5).                     EL683
00176                                                                   EL683
00177          16  PI-BROWSE-SW           PIC X.                        EL683
00178              88  BROWSE-STARTED              VALUE 'Y'.           EL683
00179          16  PI-ERCOMM-EOF-SW       PIC X.                        EL683
00180              88  ERCOMM-EOF                  VALUE 'Y'.           EL683
00181          16  PI-EXCESS-SW           PIC X.                        EL683
00182              88  EXCESS-LEVEL-EXISTS         VALUE 'X'.           EL683
00183          16  PI-COMPANY-ADD-SW      PIC X.                        EL683
00184              88  COMPANY-RECORD-ADDED        VALUE 'Y'.           EL683
00185                                                                   EL683
00186          16  PI-SUB                 PIC S99.                      EL683
00187          16  PI-LAST-LEVEL          PIC S99.                      EL683
00188          16  FILLER                 PIC X(618).                      CL**4
00189                                                                   EL683
00190      EJECT                                                        EL683
00191  LINKAGE SECTION.                                                 EL683
00192  01  DFHCOMMAREA                     PIC X(1024).                 EL683
00193 *01 PARM-LIST .                                                      CL**4
00194 *    02  FILLER              PIC S9(8)   COMP.                       CL**4
00195 *    02  ERCOMM-POINTER      PIC S9(8)   COMP.                       CL**4
00196 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                       CL**4
00197      EJECT                                                        EL683
00198                           COPY ERCCTBL.                              CL**3
00199                                                                   EL683
00200                           COPY ELCCNTL SUPPRESS.                     CL**4
00201      EJECT                                                        EL683
00202                                                                   EL683
00203  PROCEDURE DIVISION.                                              EL683
00204                                                                   EL683
00205      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL683
00206      MOVE '5'                   TO DC-OPTION-CODE.                EL683
00207      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL683
00208      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL683
00209      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL683
00210      MOVE DC-GREG-DATE-1-ALPHA  TO  SAVE-DATE-ALPHA.              EL683
00211                                                                   EL683
00212      MOVE DFHCOMMAREA  TO  PROGRAM-INTERFACE-BLOCK.               EL683
00213      MOVE 'EL683'      TO  WS-REPORT-ID.                          EL683
00214                                                                   EL683
00215  1000-START.                                                      EL683
00216      EXEC CICS  HANDLE CONDITION                                  EL683
00217             ERROR    (8800-ABEND)                                 EL683
00218             PGMIDERR (8900-PGMIDERR)                              EL683
00219      END-EXEC.                                                    EL683
00220                                                                   EL683
00221  2000-RECEIVE.                                                    EL683
00222      EXEC CICS RETRIEVE                                           EL683
00223          INTO   (PROGRAM-INTERFACE-BLOCK)                         EL683
00224          LENGTH (CLEN)                                            EL683
00225      END-EXEC.                                                    EL683
00226                                                                   EL683
00227  2000-CHECK-IN-PROGRESS.                                          EL683
00228      EXEC CICS  HANDLE CONDITION                                  EL683
00229             NOTFND   (2000-WRITE-INITIAL-TRAILER)                 EL683
00230      END-EXEC.                                                    EL683
00231                                                                   EL683
00232      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL683
00233      MOVE 'RF'                   TO  RF-RECORD-ID.                EL683
00234      MOVE '2'                    TO  RF-RECORD-TYPE.              EL683
00235      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                EL683
00236      MOVE ZEROS                  TO  RF-LINE-NUMBER.              EL683
00237                                                                   EL683
00238      EXEC CICS READ                                               EL683
00239          DATASET (REPT-FILE-ID)                                   EL683
00240          INTO    (REPORT-SAVE-FILE)                               EL683
00241          RIDFLD  (RF-CONTROL-PRIMARY)                             EL683
00242      END-EXEC.                                                    EL683
00243                                                                   EL683
00244 ********IF RECORD FOUND, THEN ANOTHER REPORT HAS ALREADY          EL683
00245 ********BEEN STARTED.  IF PREVIOUS REPORT ABENDED AND DIDN'T      EL683
00246 ********COMPLETE, THEN OPERATOR MUST PURGE REPORT AND EREATE      EL683
00247 ********A NEW ONE.                                                EL683
00248      GO TO 9999-RETURN-CICS.                                      EL683
00249                                                                   EL683
00250  2000-WRITE-INITIAL-TRAILER.                                      EL683
00251      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL683
00252      MOVE 'RF'                   TO  RF-RECORD-ID.                EL683
00253      MOVE '2'                    TO  RF-RECORD-TYPE.              EL683
00254      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                EL683
00255      MOVE ZEROS                  TO  RF-LINE-NUMBER.              EL683
00256                                                                   EL683
00257      MOVE SPACES                 TO  RF-TRAILER-RECORD.           EL683
00258      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL**4
00259      END-EXEC                                                        CL**4
00260      EXEC CICS FORMATTIME                                            CL**4
00261                ABSTIME(LCP-CICS-TIME)                                CL**4
00262                TIME(LCP-TIME-OF-DAY-XX)                              CL**4
00263      END-EXEC                                                        CL**4
00264      MOVE  LCP-TIME-OF-DAY-68 TO RF-PRINT-HH-MM-SS.                  CL**4
00265      MOVE 'STARTED'              TO  RF-CURRENT-DATE.             EL683
00266                                                                   EL683
00267      EXEC CICS WRITE                                              EL683
00268          DATASET (REPT-FILE-ID)                                   EL683
00269          FROM    (REPORT-SAVE-FILE)                               EL683
00270          RIDFLD  (RF-CONTROL-PRIMARY)                             EL683
00271      END-EXEC.                                                    EL683
00272                                                                   EL683
00273  2100-DELETE-REC.                                                 EL683
00274      MOVE 1 TO RF-LINE-NUMBER.                                    EL683
00275                                                                   EL683
00276      EXEC CICS  HANDLE CONDITION                                  EL683
00277             NOTFND   (2300-DELETE-REC)                            EL683
00278      END-EXEC.                                                    EL683
00279                                                                   EL683
00280  2200-DELETE-1.                                                   EL683
00281      MOVE PI-COMPANY-CD TO RF-COMPANY-CD.                         EL683
00282      MOVE 'RF'          TO RF-RECORD-ID.                          EL683
00283      MOVE '1'           TO RF-RECORD-TYPE.                        EL683
00284      MOVE WS-REPORT-ID  TO RF-REPORT-ID.                          EL683
00285                                                                   EL683
00286      EXEC CICS DELETE                                             EL683
00287          DATASET   (REPT-FILE-ID)                                 EL683
00288          RIDFLD    (RF-CONTROL-PRIMARY)                           EL683
00289          KEYLENGTH (11)                                           EL683
00290      END-EXEC.                                                    EL683
00291                                                                   EL683
00292      ADD 1 TO RF-LINE-NUMBER.                                     EL683
00293      GO TO 2200-DELETE-1.                                         EL683
00294                                                                   EL683
00295  2300-DELETE-REC.                                                 EL683
00296      EXEC CICS  HANDLE CONDITION                                  EL683
00297             NOTFND   (3000-START-BROWSE)                          EL683
00298      END-EXEC.                                                    EL683
00299                                                                   EL683
00300  2400-DELETE-2.                                                   EL683
00301      MOVE PI-COMPANY-CD TO RF-COMPANY-CD.                         EL683
00302      MOVE 'RF'          TO RF-RECORD-ID.                          EL683
00303      MOVE '2'           TO RF-RECORD-TYPE.                        EL683
00304      MOVE WS-REPORT-ID  TO RF-REPORT-ID.                          EL683
00305                                                                   EL683
00306      EXEC CICS DELETE                                             EL683
00307          DATASET   (REPT-FILE-ID)                                 EL683
00308          RIDFLD    (RF-CONTROL-PRIMARY)                           EL683
00309          KEYLENGTH (11)                                           EL683
00310      END-EXEC.                                                    EL683
00311                                                                   EL683
00312      ADD 1 TO RF-LINE-NUMBER.                                     EL683
00313      GO TO 2400-DELETE-2.                                         EL683
00314                                                                   EL683
00315  3000-START-BROWSE.                                               EL683
00316      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL683
00317      MOVE SPACES                 TO CNTL-ACCESS.                  EL683
00318      MOVE '1'                    TO CNTL-REC-TYPE.                EL683
00319      MOVE +0                     TO CNTL-SEQ-NO.                  EL683
00320                                                                   EL683
00321      PERFORM 9000-READ-CONTROL THRU 9000-EXIT.                    EL683
00322                                                                   EL683
00323      MOVE CF-CL-MAIL-TO-NAME TO H2-COMP.                          EL683
00324      MOVE SAVE-DATE          TO H2-DATE.                          EL683
00325      MOVE ' THRU '           TO H3-THRU.                          EL683
00326                                                                   EL683
00327      MOVE SPACE              TO DC-OPTION-CODE.                   EL683
00328      MOVE PI-LO-DATE         TO DC-BIN-DATE-1.                    EL683
00329      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL683
00330      MOVE DC-GREG-DATE-1-EDIT TO H3-DATE1.                        EL683
00331                                                                   EL683
00332      IF PI-LO-DATE = LOW-VALUES                                   EL683
00333         MOVE 'INCEPT.'        TO H3-DATE1.                        EL683
00334                                                                   EL683
00335      MOVE SPACE               TO DC-OPTION-CODE.                  EL683
00336      MOVE PI-HI-DATE          TO DC-BIN-DATE-1.                   EL683
00337      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL683
00338      MOVE DC-GREG-DATE-1-EDIT TO H3-DATE2.                        EL683
00339                                                                   EL683
00340      IF PI-HI-DATE = HIGH-VALUES                                  EL683
00341         MOVE 'CURRENT'        TO H3-DATE2.                        EL683
00342                                                                   EL683
00343      EXEC CICS  HANDLE CONDITION                                  EL683
00344             NOTFND   (9999-RETURN-CICS)                           EL683
00345      END-EXEC.                                                    EL683
00346                                                                   EL683
00347      MOVE LOW-VALUES    TO PI-ERCOMM-KEY.                         EL683
00348      MOVE PI-COMPANY-CD TO PI-COM-COMPANY-CD.                     EL683
00349                                                                   EL683
00350      EXEC CICS STARTBR                                            EL683
00351           DATASET  (COMM-FILE-ID)                                 EL683
00352           RIDFLD   (PI-ERCOMM-KEY)                                EL683
00353       END-EXEC.                                                   EL683
00354                                                                   EL683
00355      EXEC CICS  HANDLE CONDITION                                  EL683
00356             ENDFILE  (4500-ENDBROWSE)                             EL683
00357      END-EXEC.                                                    EL683
00358                                                                   EL683
00359  4000-READNEXT.                                                   EL683
00360      EXEC CICS READNEXT                                           EL683
00361           DATASET  (COMM-FILE-ID)                                 EL683
00362           SET      (ADDRESS OF COMM-TABLE-RECORD)                    CL**4
00363           RIDFLD   (PI-ERCOMM-KEY)                                EL683
00364      END-EXEC.                                                    EL683
00365                                                                   EL683
00366      IF PI-COMPANY-CD = CT-COMPANY-CD                             EL683
00367         GO TO 5000-PRINT-IT.                                      EL683
00368                                                                   EL683
00369  4500-ENDBROWSE.                                                  EL683
00370      MOVE '1'    TO RF-CTL-CHAR-133.                              EL683
00371      MOVE SPACES TO RF-DATA-133.                                  EL683
00372      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL683
00373                                                                   EL683
00374      EXEC CICS ENDBR                                              EL683
00375           DATASET  (COMM-FILE-ID)                                 EL683
00376      END-EXEC.                                                    EL683
00377                                                                   EL683
00378  4600-DELETE-INITIAL-2.                                           EL683
00379      MOVE PI-COMPANY-CD TO RF-COMPANY-CD.                         EL683
00380      MOVE 'RF'          TO RF-RECORD-ID.                          EL683
00381      MOVE '2'           TO RF-RECORD-TYPE.                        EL683
00382      MOVE WS-REPORT-ID  TO RF-REPORT-ID.                          EL683
00383      MOVE ZEROS         TO RF-LINE-NUMBER.                        EL683
00384                                                                   EL683
00385      EXEC CICS DELETE                                             EL683
00386          DATASET   (REPT-FILE-ID)                                 EL683
00387          RIDFLD    (RF-CONTROL-PRIMARY)                           EL683
00388          KEYLENGTH (11)                                           EL683
00389      END-EXEC.                                                    EL683
00390                                                                   EL683
00391      PERFORM 8999-WRITE-TRAILER.                                  EL683
00392      GO TO 9999-RETURN-CICS.                                      EL683
00393                                                                   EL683
00394  5000-PRINT-IT.                                                   EL683
00395      IF CT-LAST-MAINT-DT LESS    PI-LO-DATE OR                    EL683
00396         CT-LAST-MAINT-DT GREATER PI-HI-DATE                       EL683
00397         GO TO 4000-READNEXT.                                      EL683
00398                                                                   EL683
00399      IF PRT-CNT = 6                                               EL683
00400                PERFORM 6500-HDR-RTN THRU 6500-H-R-X.              EL683
00401                                                                   EL683
00402      MOVE SPACE TO HDR-6  HDR-7.                                  EL683
00403                                                                   EL683
00404      MOVE CT-TABLE TO H4-TAB.                                     EL683
00405                                                                   EL683
00406      IF CT-BEN-TYPE = 'L'                                         EL683
00407          MOVE 'LIFE'  TO H5-TYPE                                  EL683
00408       ELSE                                                        EL683
00409          MOVE 'A & H' TO H5-TYPE.                                 EL683
00410                                                                   EL683
00411      MOVE CT-BEN-CODE TO H6-CODE.                                 EL683
00412                                                                   EL683
00413      IF CT-TBF (1) NOT = ZERO                                     EL683
00414          MOVE CT-TRM (1) TO H6-TRM1 (1)                           EL683
00415          MOVE CT-TRM (2) TO H6-TRM2 (1)                           EL683
00416          MOVE CT-TRM (3) TO H6-TRM3 (1)                           EL683
00417          MOVE CT-TBF (1) TO H6-BEN (1).                           EL683
00418                                                                   EL683
00419      IF CT-TBF (2) NOT = ZERO                                     EL683
00420          MOVE CT-TRM (1) TO H6-TRM1 (2)                           EL683
00421          MOVE CT-TRM (2) TO H6-TRM2 (2)                           EL683
00422          MOVE CT-TRM (3) TO H6-TRM3 (2)                           EL683
00423          MOVE CT-TBF (2) TO H6-BEN (2).                           EL683
00424                                                                   EL683
00425      IF CT-TBF (3) NOT = ZERO                                     EL683
00426          MOVE CT-TRM (1) TO H6-TRM1 (3)                           EL683
00427          MOVE CT-TRM (2) TO H6-TRM2 (3)                           EL683
00428          MOVE CT-TRM (3) TO H6-TRM3 (3)                           EL683
00429          MOVE CT-TBF (3) TO H6-BEN (3).                           EL683
00430                                                                   EL683
00431  0140-PARTIAL-PRINT.                                              EL683
00432      MOVE '-'   TO RF-CTL-CHAR-133.                               EL683
00433      MOVE HDR-4 TO RF-DATA-133.                                   EL683
00434      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL683
00435                                                                   EL683
00436      MOVE ' '   TO RF-CTL-CHAR-133.                               EL683
00437      MOVE HDR-5 TO RF-DATA-133.                                   EL683
00438      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL683
00439                                                                   EL683
00440      MOVE ' '   TO RF-CTL-CHAR-133.                               EL683
00441      MOVE HDR-6 TO RF-DATA-133.                                   EL683
00442      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL683
00443                                                                   EL683
00444  0150-BUILD-HEAD-10.                                              EL683
00445      IF CT-TBF (1) NOT = ZERO                                     EL683
00446          MOVE 'A'         TO H7-CD (1)                            EL683
00447          MOVE CT-AGE (1) TO H7-AGE (1)                            EL683
00448          MOVE 1           TO X1                                   EL683
00449          MOVE 1           TO X2                                   EL683
00450          PERFORM 0160-MOVE-RATE THRU 0170-MOVE-RATE-EXIT.         EL683
00451                                                                   EL683
00452      IF CT-TBF (2) NOT = ZERO                                     EL683
00453          MOVE 'A'         TO H7-CD (2)                            EL683
00454          MOVE CT-AGE (1) TO H7-AGE (2)                            EL683
00455          MOVE 10          TO X1                                   EL683
00456          MOVE 2           TO X2                                   EL683
00457          PERFORM 0160-MOVE-RATE THRU 0170-MOVE-RATE-EXIT.         EL683
00458                                                                   EL683
00459      IF CT-TBF (3) NOT = ZERO                                     EL683
00460          MOVE 'A'         TO H7-CD (3)                            EL683
00461          MOVE CT-AGE (1) TO H7-AGE (3)                            EL683
00462          MOVE 19          TO X1                                   EL683
00463          MOVE 3           TO X2                                   EL683
00464          PERFORM 0160-MOVE-RATE THRU 0170-MOVE-RATE-EXIT.         EL683
00465                                                                   EL683
00466      MOVE '0'   TO RF-CTL-CHAR-133.                               EL683
00467      MOVE HDR-7 TO RF-DATA-133.                                   EL683
00468      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL683
00469                                                                   EL683
00470      ADD 1      TO PRT-CNT.                                       EL683
00471      MOVE SPACE TO HDR-7.                                         EL683
00472                                                                   EL683
00473      IF CT-TBF (1) NOT = ZERO                                     EL683
00474          MOVE 'G'         TO H7-CD (1)                            EL683
00475          MOVE CT-AGE (2) TO H7-AGE (1)                            EL683
00476          MOVE 4           TO X1                                   EL683
00477          MOVE 1           TO X2                                   EL683
00478          PERFORM 0160-MOVE-RATE THRU 0170-MOVE-RATE-EXIT.         EL683
00479                                                                   EL683
00480      IF CT-TBF (2) NOT = ZERO                                     EL683
00481          MOVE 'G'         TO H7-CD (2)                            EL683
00482          MOVE CT-AGE (2) TO H7-AGE (2)                            EL683
00483          MOVE 13          TO X1                                   EL683
00484          MOVE 2           TO X2                                   EL683
00485          PERFORM 0160-MOVE-RATE THRU 0170-MOVE-RATE-EXIT.         EL683
00486                                                                   EL683
00487      IF CT-TBF (3) NOT = ZERO                                     EL683
00488          MOVE 'G'         TO H7-CD (3)                            EL683
00489          MOVE CT-AGE (2) TO H7-AGE (3)                            EL683
00490          MOVE 22          TO X1                                   EL683
00491          MOVE 3           TO X2                                   EL683
00492          PERFORM 0160-MOVE-RATE THRU 0170-MOVE-RATE-EXIT.         EL683
00493                                                                   EL683
00494      MOVE ' '   TO RF-CTL-CHAR-133.                               EL683
00495      MOVE HDR-7 TO RF-DATA-133.                                   EL683
00496      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL683
00497                                                                   EL683
00498      MOVE SPACE TO HDR-7.                                         EL683
00499                                                                   EL683
00500      IF CT-TBF (1) NOT = ZERO                                     EL683
00501          MOVE 'E'         TO H7-CD (1)                            EL683
00502          MOVE CT-AGE (3) TO H7-AGE (1)                            EL683
00503          MOVE 7           TO X1                                   EL683
00504          MOVE 1           TO X2                                   EL683
00505          PERFORM 0160-MOVE-RATE THRU 0170-MOVE-RATE-EXIT.         EL683
00506                                                                   EL683
00507      IF CT-TBF (2) NOT = ZERO                                     EL683
00508          MOVE 'E'         TO H7-CD (2)                            EL683
00509          MOVE CT-AGE (3) TO H7-AGE (2)                            EL683
00510          MOVE 16          TO X1                                   EL683
00511          MOVE 2           TO X2                                   EL683
00512          PERFORM 0160-MOVE-RATE THRU 0170-MOVE-RATE-EXIT.         EL683
00513                                                                   EL683
00514      IF CT-TBF (3) NOT = ZERO                                     EL683
00515          MOVE 'E'         TO H7-CD (3)                            EL683
00516          MOVE CT-AGE (3) TO H7-AGE (3)                            EL683
00517          MOVE 25          TO X1                                   EL683
00518          MOVE 3           TO X2                                   EL683
00519          PERFORM 0160-MOVE-RATE THRU 0170-MOVE-RATE-EXIT.         EL683
00520                                                                   EL683
00521      MOVE ' '   TO RF-CTL-CHAR-133.                               EL683
00522      MOVE HDR-7 TO RF-DATA-133.                                   EL683
00523      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL683
00524                                                                   EL683
00525      GO TO 4000-READNEXT.                                         EL683
00526                                                                   EL683
00527  0160-MOVE-RATE.                                                  EL683
00528      IF CT-RT (X1) NUMERIC                                        EL683
00529          MOVE CT-RT (X1)  TO H7-PC1 (X2)                          EL683
00530      ELSE                                                         EL683
00531          MOVE CT-RTX (X1) TO H7-PC1A (X2).                        EL683
00532                                                                   EL683
00533      ADD B1 TO X1.                                                EL683
00534                                                                   EL683
00535      IF CT-RT (X1) NUMERIC                                        EL683
00536          MOVE CT-RT (X1)  TO H7-PC2 (X2)                          EL683
00537      ELSE                                                         EL683
00538          MOVE CT-RTX (X1) TO H7-PC2A (X2).                        EL683
00539                                                                   EL683
00540      ADD B1 TO X1.                                                EL683
00541                                                                   EL683
00542      IF CT-RT (X1) NUMERIC                                        EL683
00543          MOVE CT-RT (X1)  TO H7-PC3 (X2)                          EL683
00544      ELSE                                                         EL683
00545          MOVE CT-RTX (X1) TO H7-PC3A (X2).                        EL683
00546                                                                   EL683
00547  0170-MOVE-RATE-EXIT.                                             EL683
00548      EXIT.                                                        EL683
00549  EJECT                                                            EL683
00550 *0190-GET-DESC.                                                   EL683
00551 *    MOVE CLAS-STARTL TO CLAS-INDEXL.                             EL683
00552 *    MOVE CLAS-STARTA TO CLAS-INDEXA.                             EL683
00553                                                                   EL683
00554 *    IF CT-BEN-CODE = 'AA'                                        EL683
00555 *            MOVE 'ALL' TO H7-DESC                                EL683
00556 *            GO TO 0230-G-D-X.                                    EL683
00557                                                                   EL683
00558 *    IF CT-BEN-TYPE = 'A'                                         EL683
00559 *       GO TO 0210-GET-AH-DESC.                                   EL683
00560                                                                   EL683
00561 *    IF CLAS-MAXL = ZEROES                                        EL683
00562 *       MOVE 'UNKNOWN' TO H7-DESC                                 EL683
00563 *       GO TO 0230-G-D-X.                                         EL683
00564                                                                   EL683
00565 *0200-GET-LIFE-DESC.                                              EL683
00566 *    IF CLAS-INDEXL GREATER THAN CLAS-MAXL                        EL683
00567 *       MOVE 'UNKNOWN' TO H7-DESC                                 EL683
00568 *       GO TO 0230-G-D-X.                                         EL683
00569                                                                   EL683
00570 *    IF CT-BEN-CODE NOT = CLAS-I-BEN (CLAS-INDEXL)                EL683
00571 *       ADD 1 TO CLAS-INDEXL                                      EL683
00572 *       GO TO 0200-GET-LIFE-DESC.                                 EL683
00573                                                                   EL683
00574 *    MOVE CLAS-I-AB10 (CLAS-INDEXL) TO H7-DESC.                   EL683
00575 *    MOVE CLAS-INDEXL               TO SUB.                       EL683
00576 *    GO TO 0230-G-D-X.                                            EL683
00577                                                                   EL683
00578 *0210-GET-AH-DESC.                                                EL683
00579 *    IF CLAS-MAXA = ZEROES                                        EL683
00580 *       MOVE 'UNKNOWN' TO H7-DESC                                 EL683
00581 *       GO TO 0230-G-D-X.                                         EL683
00582                                                                   EL683
00583 *0220-GET-AH-LOOP.                                                EL683
00584 *    IF CLAS-INDEXA GREATER THAN CLAS-MAXA                        EL683
00585 *       MOVE 'UNKNOWN' TO H7-DESC                                 EL683
00586 *       GO TO 0230-G-D-X.                                         EL683
00587                                                                   EL683
00588 *    IF CT-BEN-CODE NOT = CLAS-I-BEN (CLAS-INDEXA)                EL683
00589 *       ADD 1 TO CLAS-INDEXA                                      EL683
00590 *       GO TO 0220-GET-AH-LOOP.                                   EL683
00591                                                                   EL683
00592 *    MOVE CLAS-I-AB10 (CLAS-INDEXA) TO H7-DESC.                   EL683
00593 *    MOVE CLAS-INDEXA TO SUB.                                     EL683
00594                                                                   EL683
00595 *0230-G-D-X.                                                      EL683
00596 *    EXIT.                                                        EL683
00597                                                                   EL683
00598  EJECT                                                            EL683
00599                                                                   EL683
00600  6500-HDR-RTN.                                                    EL683
00601      ADD +1       TO WS-PAGE.                                     EL683
00602      MOVE WS-PAGE TO H3-PAGE.                                     EL683
00603      MOVE '1'     TO RF-CTL-CHAR-133.                             EL683
00604      MOVE HDR-1   TO RF-DATA-133.                                 EL683
00605      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL683
00606      MOVE ' '     TO RF-CTL-CHAR-133.                             EL683
00607      MOVE HDR-2   TO RF-DATA-133.                                 EL683
00608      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL683
00609      MOVE ' '     TO RF-CTL-CHAR-133.                             EL683
00610      MOVE HDR-3   TO RF-DATA-133.                                 EL683
00611      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        EL683
00612      MOVE +0      TO PRT-CNT.                                     EL683
00613                                                                   EL683
00614  6500-H-R-X.                                                      EL683
00615      EXIT.                                                        EL683
00616                                                                   EL683
00617  7000-PRT-LINE.                                                   EL683
00618      MOVE PI-COMPANY-CD  TO RF-COMPANY-CD.                        EL683
00619      MOVE 'RF'           TO RF-RECORD-ID.                         EL683
00620      MOVE '1'            TO RF-RECORD-TYPE.                       EL683
00621      MOVE WS-REPORT-ID   TO RF-REPORT-ID.                         EL683
00622      ADD 1               TO WS-LINE-NUMBER.                       EL683
00623      MOVE WS-LINE-NUMBER TO RF-LINE-NUMBER.                       EL683
00624                                                                   EL683
00625      EXEC CICS WRITE                                              EL683
00626          DATASET (REPT-FILE-ID)                                   EL683
00627          FROM    (REPORT-SAVE-FILE)                               EL683
00628          RIDFLD  (RF-CONTROL-PRIMARY)                             EL683
00629      END-EXEC.                                                    EL683
00630                                                                   EL683
00631  7000-EXIT.                                                       EL683
00632       EXIT.                                                       EL683
00633                                                                   EL683
00634  8500-DATE-CONVERT.                                               EL683
00635      MOVE LINK-ELDATCV           TO PGM-NAME.                     EL683
00636                                                                   EL683
00637      EXEC CICS LINK                                               EL683
00638          PROGRAM    (PGM-NAME)                                    EL683
00639          COMMAREA   (DATE-CONVERSION-DATA)                        EL683
00640          LENGTH     (DC-COMM-LENGTH)                              EL683
00641      END-EXEC.                                                    EL683
00642                                                                   EL683
00643  8500-EXIT.                                                       EL683
00644      EXIT.                                                        EL683
00645                                                                   EL683
00646  8800-ABEND.                                                      EL683
00647      MOVE DFHEIBLK TO EMI-LINE1.                                  EL683
00648                                                                   EL683
00649      EXEC CICS LINK                                               EL683
00650          PROGRAM   ('EL004')                                      EL683
00651          COMMAREA  (EMI-LINE1)                                    EL683
00652          LENGTH    (72)                                           EL683
00653      END-EXEC.                                                    EL683
00654                                                                   EL683
00655      GO TO 9999-RETURN-CICS.                                      EL683
00656                                                                   EL683
00657  8900-PGMIDERR.                                                   EL683
00658      GO TO 9999-RETURN-CICS.                                      EL683
00659                                                                   EL683
00660  8999-WRITE-TRAILER.                                              EL683
00661      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL683
00662      MOVE 'RF'                   TO  RF-RECORD-ID.                EL683
00663      MOVE '2'                    TO  RF-RECORD-TYPE.              EL683
00664      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                EL683
00665      ADD +1                      TO  WS-LINE-NUMBER.              EL683
00666      MOVE WS-LINE-NUMBER         TO  RF-LINE-NUMBER.              EL683
00667                                                                   EL683
00668      MOVE SPACES                 TO  RF-TRAILER-RECORD.           EL683
00669      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL**4
00670      END-EXEC                                                        CL**4
00671      EXEC CICS FORMATTIME                                            CL**4
00672                ABSTIME(LCP-CICS-TIME)                                CL**4
00673                TIME(LCP-TIME-OF-DAY-XX)                              CL**4
00674      END-EXEC                                                        CL**4
00675      MOVE  LCP-TIME-OF-DAY-68 TO RF-PRINT-HH-MM-SS.                  CL**4
00676      MOVE SAVE-DATE              TO  RF-CURRENT-DATE.             EL683
00677                                                                   EL683
00678      EXEC CICS WRITE                                              EL683
00679          DATASET (REPT-FILE-ID)                                   EL683
00680          FROM    (REPORT-SAVE-FILE)                               EL683
00681          RIDFLD  (RF-CONTROL-PRIMARY)                             EL683
00682      END-EXEC.                                                    EL683
00683                                                                   EL683
00684  9000-READ-CONTROL.                                               EL683
00685      EXEC CICS READ                                               EL683
00686          DATASET     (CNTL-ID)                                    EL683
00687          SET         (ADDRESS OF CONTROL-FILE)                       CL**4
00688          RIDFLD      (ELCNTL-KEY)                                 EL683
00689      END-EXEC.                                                    EL683
00690                                                                   EL683
00691  9000-EXIT.                                                       EL683
00692       EXIT.                                                       EL683
00693                                                                   EL683
00694  9999-RETURN-CICS.                                                EL683
00695      EXEC CICS  RETURN                                            EL683
00696      END-EXEC.                                                    EL683
00697                                                                   EL683
00698  9999-EXIT.                                                       EL683
00699       EXIT.                                                       EL683
00700                                                                   EL683
