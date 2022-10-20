00001  IDENTIFICATION DIVISION.                                         06/26/96
00002                                                                   EL6411
00003  PROGRAM-ID.                 EL6411.                                 LV007
00004 *              PROGRAM CONVERTED BY                                  CL**6
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**6
00006 *              CONVERSION DATE 02/12/96 09:56:12.                    CL**6
00007 *                            VMOD=2.007.                             CL**7
00008 *                                                                 EL6411
00009 *AUTHOR.           LOGIC,INC.                                        CL**6
00010 *                  DALLAS,TEXAS.                                     CL**6
00011                                                                   EL6411
00012 *DATE-COMPILED.                                                      CL**6
00013                                                                   EL6411
00014 *SECURITY.   *****************************************************   CL**6
00015 *            *                                                   *   CL**6
00016 *            * THIS PROGRAM IS THE PROPERTY OF LOCIC, INC. *         CL**6
00017 *            *                                                   *   CL**6
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**6
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**6
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**6
00021 *            *                                                   *   CL**6
00022 *            *****************************************************   CL**6
00023                                                                   EL6411
00024 *REMARKS. TRANSACTION EXC9 - BILLING STATEMENT PRINT.                CL**3
00025 *        THIS PROGRAM IS USED TO PRINT THE STORED STATEMENTS AND     CL**3
00026 *        LABELS  DEPENDING ON THE VALUE OF THE PI-ENTRY-CODES.       CL**3
00027                                                                   EL6411
00028 *        PRINT INITIAL STATEMENTS CODE-1 = 1                         CL**3
00029 *                                 CODE-2 = 1                         CL**3
00030                                                                   EL6411
00031 *        RE-PRINT STATEMENTS      CODE-1 = 0                         CL**3
00032 *                                 CODE-2 = 3                         CL**3
00033                                                                   EL6411
00034 *        PRINT ADDRESS LABELS     CODE-1 = 0                         CL**3
00035 *                                 CODE-2 = 2                         CL**3
00036                                                                   EL6411
00037 *        PRINT SINGLE STATEMENT   CODE-1 = 0                         CL**3
00038 *                                 CODE-2 = 4                         CL**3
00039      EJECT                                                        EL6411
00040  ENVIRONMENT DIVISION.                                            EL6411
00041  DATA DIVISION.                                                   EL6411
00042  WORKING-STORAGE SECTION.                                         EL6411
00043  77  FILLER  PIC X(32)  VALUE '********************************'. EL6411
00044  77  FILLER  PIC X(32)  VALUE '*   EL6411 WORKING STORAGE     *'. EL6411
00045  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.007 ************'.    CL**7
00046                                                                   EL6411
00047                                  COPY ELCDMD34.                      CL**7
00048                                  COPY ELCSCTM.                       CL**3
00049                                  COPY ELCSCRTY.                      CL**3
00050                                                                   EL6411
00051     EJECT                                                         EL6411
00052  01  REMOTE-PRINTER-CTRL.                                            CL**4
00053      12  CTRL-BUFFER-LENGTH      PIC S9(04) COMP.                    CL**4
00054      12  CTRL-BUFFER-AREA        PIC X(20).                          CL**4
00055      SKIP1                                                           CL**4
00056  01  WS-CONSTANTS.                                                EL6411
00057      12  SC-ITEM                 PIC S9(4) COMP VALUE +1.         EL6411
00058      12  TIME-IN                 PIC S9(7).                       EL6411
00059      12  FILLER REDEFINES TIME-IN.                                EL6411
00060         16  FILLER               PIC X.                           EL6411
00061         16  TIME-OUT             PIC 99V99.                       EL6411
00062         16  FILLER               PIC XX.                          EL6411
00063      12  THIS-PGM                PIC X(8)    VALUE 'EL6411'.      EL6411
00064      12  PGM-EL6401              PIC X(8)    VALUE 'EL6401'.      EL6411
00065      12  PGM-NAME                PIC X(8).                        EL6411
00066      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     EL6411
00067      12  BILL-ID                 PIC X(8)    VALUE 'ERBILL'.      EL6411
00068      12  BILL-KEY.                                                EL6411
00069          16  BILL-PARTIAL-KEY.                                    EL6411
00070              20  BILL-CO         PIC X.                           EL6411
00071              20  BILL-CARRIER    PIC X            VALUE SPACE.    EL6411
00072              20  BILL-GROUPING   PIC X(6)         VALUE SPACES.   EL6411
00073              20  BILL-ACCOUNT    PIC X(10)        VALUE SPACES.   EL6411
00074              20  BILL-FIN-RESP   PIC X(10)        VALUE SPACES.   EL6411
00075          16  BILL-RECORD-TYPE    PIC X            VALUE SPACE.    EL6411
00076          16  BILL-SEQ-NO         PIC S9(4)  COMP  VALUE +0.       EL6411
00077                                                                   EL6411
00078      12  BILL-SAVE-KEY           PIC X(31)        VALUE SPACES.   EL6411
00079      12  BILL-SAVE-PARTIAL-KEY   PIC X(28)        VALUE SPACES.   EL6411
00080                                                                   EL6411
00081      12  ERROR-LINE              PIC X(80)        VALUE SPACES.   EL6411
00082      12  CURRENT-SAVE            PIC XX           VALUE SPACES.   EL6411
00083      12  WS-BROWSE-STARTED       PIC X VALUE 'N'.                 EL6411
00084      12  SUB                     PIC 9   COMP-3   VALUE ZEROS.    EL6411
00085      12  WS-SKIP                 PIC 99           VALUE ZEROS.    EL6411
00086      12  WS-COPIES               PIC 9            VALUE ZEROS.    EL6411
00087      12  HEADER-SW               PIC X            VALUE SPACE.    EL6411
00088          88  HEADER-REC-FOUND                     VALUE SPACE.    EL6411
00089                                                                   EL6411
00090      12  CORRESPOND-SW           PIC X            VALUE SPACE.    EL6411
00091          88  CORR-REC-FOUND                       VALUE SPACE.    EL6411
00092                                                                   EL6411
00093      12  ADDR-SW                 PIC X            VALUE SPACE.    EL6411
00094          88  ADDRESS-REC-FOUND                    VALUE SPACE.    EL6411
00095                                                                   EL6411
00096      12  OPTION-CODES            PIC XX           VALUE SPACES.   EL6411
00097          88  PRINT-STATEMENTS        VALUE '11'.                  EL6411
00098          88  PRINT-LABELS            VALUE ' 2'.                  EL6411
00099          88  REPRINT-STATEMENTS      VALUE ' 3'.                  EL6411
00100          88  PRINT-SINGLE-STATEMENT  VALUE ' 4'.                  EL6411
00101                                                                   EL6411
00102      12  COMP-132                PIC S9(4)   COMP VALUE +132.     EL6411
00103      12  COMP-80                 PIC S9(4)   COMP VALUE +80.      EL6411
00104      12  J-BILL-LENGTH           PIC S9(4)   COMP VALUE +210.     EL6411
00105      12  JOURNAL-LENGTH          PIC S9(4)   COMP.                EL6411
00106                                                                   EL6411
00107      12  WS-LABEL-HOLD-AREA.                                      EL6411
00108          16  WS-LABEL-LINES OCCURS 6 TIMES INDEXED BY L-INDX.     EL6411
00109            18  WS-LABEL-ZIP.                                      EL6411
00110              20  WS-LABEL-1ST-ZIP    PIC X(4).                    EL6411
00111              20  WS-LABEL-2ND-ZIP    PIC X(5).                    EL6411
00112            18  FILLER                PIC X(12).                   EL6411
00113            18  WS-LAST-ZIP.                                       EL6411
00114              20  WS-LAST-1ST-ZIP     PIC X(4).                    EL6411
00115              20  WS-LAST-2ND-ZIP     PIC X(5).                    EL6411
00116      EJECT                                                        EL6411
00117                                  COPY ELCINTF.                       CL**3
00118      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.                    EL6411
00119          16  PI-PRINT-DATE       PIC X(8).                        EL6411
00120          16  PI-PRINT-DATE-BIN   PIC XX.                          EL6411
00121          16  PI-PRINT-ID         PIC X(4).                        EL6411
00122          16  FILLER              PIC X(626).                         CL**6
00123      EJECT                                                        EL6411
00124                                  COPY ELCJPFX.                       CL**3
00125                                  PIC X(210).                      EL6411
00126      EJECT                                                        EL6411
00127                                  COPY ELPRTCVD.                      CL**3
00128      EJECT                                                        EL6411
00129                                  COPY ELCDATE.                       CL**3
00130      EJECT                                                        EL6411
00131  LINKAGE SECTION.                                                 EL6411
00132 *01 PARMLIST .                                                       CL**6
00133 *    02  FILLER            PIC S9(8)   COMP.                         CL**6
00134 *    02  BILL-POINTER      PIC S9(8)   COMP.                         CL**6
00135      EJECT                                                        EL6411
00136                              COPY ERCBILL.                           CL**3
00137      EJECT                                                        EL6411
00138  PROCEDURE DIVISION.                                              EL6411
00139                                                                   EL6411
00140      MOVE SPACES                 TO DL34-PROCESS-TYPE.               CL**7
00141                                                                      CL**7
00142  0100-RETRIEVE-LOOP.                                              EL6411
00143      EXEC CICS HANDLE CONDITION                                   EL6411
00144           ENDDATA  (0200-END-DATA)                                EL6411
00145           NOTFND   (0300-NOT-FOUND)                               EL6411
00146      END-EXEC.                                                    EL6411
00147                                                                   EL6411
00148      EXEC CICS RETRIEVE                                           EL6411
00149           INTO   (PROGRAM-INTERFACE-BLOCK)                        EL6411
00150           LENGTH (PI-COMM-LENGTH)                                 EL6411
00151      END-EXEC.                                                    EL6411
00152                                                                      CL**7
00153 * DLO034 OPEN WHEN DMD OR CID                                        CL**7
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**7
00155          IF DL34-PROCESS-TYPE IS EQUAL TO SPACES                     CL**7
00156              MOVE 'O'                TO DL34-PROCESS-TYPE            CL**7
00157              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID              CL**7
00158              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID        CL**7
00159              MOVE PI-PROCESSOR-ID    TO DL34-USERID                  CL**7
00160              MOVE SPACES             TO DL34-PRINT-LINE              CL**7
00161              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID     CL**7
00162              EXEC CICS LINK                                          CL**7
00163                  PROGRAM    ('DLO034')                               CL**7
00164                  COMMAREA   (DLO034-COMMUNICATION-AREA)              CL**7
00165                  LENGTH     (DLO034-REC-LENGTH)                      CL**7
00166              END-EXEC                                                CL**7
00167              IF DL34-RETURN-CODE NOT = 'OK'                          CL**7
00168                  MOVE  '**DLO034 OPEN ERROR - ABORT**'               CL**7
00169                                      TO ERROR-LINE                   CL**7
00170                  PERFORM 0400-SEND-TEXT                              CL**7
00171                  EXEC CICS RETURN                                    CL**7
00172                  END-EXEC.                                           CL**7
00173                                                                   EL6411
00199      PERFORM 1000-INITIALIZE             THRU 1000-EXIT.          EL6411
00200                                                                      CL**4
00201      PERFORM 2000-BROWSE-BILLING-HEADERS THRU 2099-EXIT.          EL6411
00211                                                                   EL6411
00212  0200-END-DATA.                                                   EL6411
00213                                                                      CL**7
00214 * DLO034 CLOSE                                                       CL**7
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**7
00216          MOVE 'C'                TO DL34-PROCESS-TYPE                CL**7
00217          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID                  CL**7
00218          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID            CL**7
00219          MOVE PI-PROCESSOR-ID    TO DL34-USERID                      CL**7
00220          MOVE SPACES             TO DL34-PRINT-LINE                  CL**7
00221                                     DL34-OVERRIDE-PRINTER-ID         CL**7
00222          EXEC CICS LINK                                              CL**7
00223              PROGRAM    ('DLO034')                                   CL**7
00224              COMMAREA   (DLO034-COMMUNICATION-AREA)                  CL**7
00225              LENGTH     (DLO034-REC-LENGTH)                          CL**7
00226          END-EXEC                                                    CL**7
00227          IF DL34-RETURN-CODE NOT = 'OK'                              CL**7
00228              MOVE  '**DLO034 CLOSE ERROR - ABORT**'                  CL**7
00229                                  TO ERROR-LINE                       CL**7
00230              PERFORM 0400-SEND-TEXT.                                 CL**7
00231                                                                      CL**7
00232      EXEC CICS RETURN                                             EL6411
00233      END-EXEC.                                                    EL6411
00234      GOBACK.                                                         CL**6
00235                                                                      CL**2
00236  0300-NOT-FOUND.                                                  EL6411
00237      MOVE 'NO COMMUNICATION AREA FOUND' TO ERROR-LINE             EL6411
00238      PERFORM 0400-SEND-TEXT                                       EL6411
00239      GO TO 0200-END-DATA.                                         EL6411
00240                                                                   EL6411
00241  0400-SEND-TEXT.                                                  EL6411
00242      EXEC CICS SEND TEXT                                          EL6411
00243           FROM   (ERROR-LINE)                                     EL6411
00244           LENGTH (70)                                             EL6411
00245      END-EXEC.                                                    EL6411
00246                                                                   EL6411
00247      EJECT                                                        EL6411
00248  1000-INITIALIZE.                                                 EL6411
00249      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL6411
00250      MOVE '5'                    TO DC-OPTION-CODE.               EL6411
00251      PERFORM 9700-DATE-LINK THRU  9700-EXIT.                      EL6411
00252      MOVE DC-BIN-DATE-1          TO CURRENT-SAVE.                 EL6411
00253      MOVE PI-COMPANY-CD          TO BILL-CO.                      EL6411
00254      MOVE PI-ENTRY-CODES         TO OPTION-CODES.                 EL6411
00255      MOVE COMP-132               TO WS-LINE-LEN.                  EL6411
00256                                                                   EL6411
00257  1000-EXIT.                                                       EL6411
00258      EXIT.                                                        EL6411
00259                                                                      CL**3
00260      EJECT                                                        EL6411
00261  2000-BROWSE-BILLING-HEADERS.                                     EL6411
00262      MOVE LOW-VALUES             TO BILL-KEY.                     EL6411
00263                                                                   EL6411
00264      IF PRINT-SINGLE-STATEMENT                                    EL6411
00265          MOVE PI-COMPANY-CD  TO  BILL-CO                          EL6411
00266          MOVE PI-CR-CARRIER  TO  BILL-CARRIER                     EL6411
00267          MOVE PI-CR-GROUPING TO  BILL-GROUPING                    EL6411
00268          MOVE PI-CR-ACCOUNT  TO  BILL-ACCOUNT                     EL6411
00269          MOVE PI-CR-FIN-RESP TO  BILL-FIN-RESP                    EL6411
00270          MOVE '1'            TO  BILL-RECORD-TYPE                 EL6411
00271      ELSE                                                         EL6411
00272          MOVE PI-COMPANY-CD  TO  BILL-CO.                         EL6411
00273                                                                   EL6411
00274      EXEC CICS HANDLE CONDITION                                   EL6411
00275           NOTFND (2099-EXIT)                                      EL6411
00276           NOTOPEN(8890-BILL-NOT-OPEN)                             EL6411
00277      END-EXEC.                                                    EL6411
00278                                                                   EL6411
00279  2005-START.                                                      EL6411
00280      EXEC CICS STARTBR                                            EL6411
00281           DATASET(BILL-ID)                                        EL6411
00282           RIDFLD (BILL-KEY)                                       EL6411
00283       END-EXEC.                                                   EL6411
00284                                                                   EL6411
00285      MOVE 'Y'                    TO WS-BROWSE-STARTED.            EL6411
00286                                                                   EL6411
00287  2010-READ-NEXT.                                                  EL6411
00288      EXEC CICS HANDLE CONDITION                                   EL6411
00289           NOTFND (2050-END-BR)                                    EL6411
00290           ENDFILE(2050-END-BR)                                    EL6411
00291           NOTOPEN(8890-BILL-NOT-OPEN)                             EL6411
00292      END-EXEC.                                                    EL6411
00293                                                                   EL6411
00294      EXEC CICS READNEXT                                           EL6411
00295           DATASET(BILL-ID)                                        EL6411
00296           RIDFLD (BILL-KEY)                                       EL6411
00297           SET    (ADDRESS OF BILLING-STATEMENT)                      CL**6
00298      END-EXEC.                                                    EL6411
00299                                                                   EL6411
00300      IF PI-COMPANY-CD NOT = BILL-CO                                  CL**2
00301         GO TO 2050-END-BR.                                           CL**2
00302                                                                      CL**2
00303      IF BILL-RECORD-TYPE = '1'                                       CL**2
00304             NEXT SENTENCE                                         EL6411
00305      ELSE                                                         EL6411
00306         MOVE 9999                TO BILL-SEQ-NO                      CL**2
00307         GO TO 2010-READ-NEXT.                                     EL6411
00308                                                                   EL6411
00309      MOVE BILL-KEY              TO BILL-SAVE-KEY.                 EL6411
00310                                                                   EL6411
00311      IF PRINT-STATEMENTS                                          EL6411
00312         PERFORM 2900-PRINT-STATEMENTS THRU 2999-EXIT              EL6411
00313         IF WS-BROWSE-STARTED = 'N'                                EL6411
00314            GO TO 2005-START                                       EL6411
00315           ELSE                                                    EL6411
00316            GO TO 2010-READ-NEXT.                                  EL6411
00317                                                                   EL6411
00318      IF REPRINT-STATEMENTS                                        EL6411
00319         PERFORM 2200-REPRINT-STATEMENTS THRU 2299-EXIT            EL6411
00320         IF WS-BROWSE-STARTED = 'N'                                   CL**2
00321            GO TO 2005-START                                          CL**2
00322           ELSE                                                       CL**2
00323         GO TO 2010-READ-NEXT.                                     EL6411
00324                                                                   EL6411
00325      IF PRINT-SINGLE-STATEMENT                                    EL6411
00326         PERFORM 2900-PRINT-STATEMENTS THRU 2999-EXIT              EL6411
00327         GO TO 2050-END-BR.                                        EL6411
00328                                                                   EL6411
00329      PERFORM 2300-STATEMENT-LABELS THRU 2399-EXIT.                EL6411
00330      GO TO 2010-READ-NEXT.                                        EL6411
00331                                                                   EL6411
00332  2050-END-BR.                                                     EL6411
00333      IF PRINT-LABELS                                              EL6411
00334         MOVE 'X'                 TO WS-PROG-END                   EL6411
00335         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                      EL6411
00336                                                                   EL6411
00337      IF WS-BROWSE-STARTED = 'Y'                                   EL6411
00338         MOVE 'N'                 TO WS-BROWSE-STARTED             EL6411
00339         EXEC CICS ENDBR                                           EL6411
00340              DATASET(BILL-ID)                                     EL6411
00341          END-EXEC.                                                EL6411
00342                                                                   EL6411
00343  2099-EXIT.                                                       EL6411
00344       EXIT.                                                       EL6411
00345                                                                      CL**3
00346      EJECT                                                        EL6411
00347  2200-REPRINT-STATEMENTS.                                         EL6411
00348      IF BI-INITIAL-PRINT-DATE   = PI-PRINT-DATE-BIN               EL6411
00349         MOVE BI-NO-OF-COPIES     TO WS-COPIES                     EL6411
00350         PERFORM 7200-PRINT-TEXT  THRU 7299-EXIT                   EL6411
00351                 WS-COPIES TIMES.                                  EL6411
00352                                                                   EL6411
00353  2299-EXIT.                                                       EL6411
00354       EXIT.                                                       EL6411
00355                                                                      CL**3
00356      EJECT                                                        EL6411
00357  2300-STATEMENT-LABELS.                                           EL6411
00358      IF FIRST-TIME                                                EL6411
00359         PERFORM 2400-ALIGNMENT-PRINT THRU 2450-EXIT.              EL6411
00360                                                                   EL6411
00361      IF BI-INITIAL-PRINT-DATE = PI-PRINT-DATE-BIN                 EL6411
00362         PERFORM 7200-PRINT-TEXT THRU 7299-EXIT.                   EL6411
00363                                                                   EL6411
00364  2399-EXIT.                                                       EL6411
00365       EXIT.                                                       EL6411
00366                                                                      CL**3
00367      EJECT                                                        EL6411
00368  2400-ALIGNMENT-PRINT.                                            EL6411
00369      MOVE ALL '*'                TO WS-LABEL-HOLD-AREA.           EL6411
00370      MOVE SPACE                  TO WS-PASSED-CNTL-CHAR.          EL6411
00371      MOVE SPACES                 TO WS-LABEL-LINES (6).           EL6411
00372      PERFORM 2480-MOVE-TO-PRINT THRU 2499-EXIT 6 TIMES.           EL6411
00373                                                                   EL6411
00374  2450-EXIT.                                                       EL6411
00375       EXIT.                                                       EL6411
00376                                                                   EL6411
00377  2480-MOVE-TO-PRINT.                                              EL6411
00378      MOVE WS-LABEL-LINES (1)     TO WS-PASSED-DATA.               EL6411
00379      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6411
00380      MOVE WS-LABEL-LINES (2)     TO WS-PASSED-DATA.               EL6411
00381      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6411
00382      MOVE WS-LABEL-LINES (3)     TO WS-PASSED-DATA.               EL6411
00383      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6411
00384      MOVE WS-LABEL-LINES (4)     TO WS-PASSED-DATA.               EL6411
00385      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6411
00386      MOVE WS-LABEL-LINES (5)     TO WS-PASSED-DATA.               EL6411
00387      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6411
00388      MOVE WS-LABEL-LINES (6)     TO WS-PASSED-DATA.               EL6411
00389      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6411
00390                                                                   EL6411
00391  2499-EXIT.                                                       EL6411
00392       EXIT.                                                       EL6411
00393                                                                      CL**3
00394      EJECT                                                        EL6411
00395  2900-PRINT-STATEMENTS.                                           EL6411
00396      IF BI-INITIAL-PRINT-DATE = LOW-VALUES                        EL6411
00397        OR PRINT-SINGLE-STATEMENT                                  EL6411
00398         PERFORM 7300-UPDATE-BILLING-HEADER THRU 7399-EXIT         EL6411
00399         IF HEADER-REC-FOUND                                       EL6411
00400            PERFORM 7200-PRINT-TEXT THRU 7299-EXIT                 EL6411
00401                    WS-COPIES TIMES.                               EL6411
00402                                                                   EL6411
00403  2999-EXIT.                                                       EL6411
00404      EXIT.                                                        EL6411
00405                                                                      CL**3
00406      EJECT                                                        EL6411
00407  7200-PRINT-TEXT.                                                 EL6411
00408      MOVE SPACES                 TO WS-PROG-END.                  EL6411
00409                                                                   EL6411
00410      IF PRINT-LABELS                                              EL6411
00411         MOVE '2'                 TO BILL-RECORD-TYPE              EL6411
00412         SET L-INDX               TO 1                             EL6411
00413      ELSE                                                         EL6411
00414         MOVE '3'                 TO BILL-RECORD-TYPE.             EL6411
00415                                                                   EL6411
00416      MOVE ZEROS                  TO BILL-SEQ-NO.                  EL6411
00417                                                                   EL6411
00418      EXEC CICS HANDLE CONDITION                                   EL6411
00419           NOTFND (7250-CHECK-FIRST-SW)                            EL6411
00420           ENDFILE(7250-CHECK-FIRST-SW)                            EL6411
00421           NOTOPEN(8890-BILL-NOT-OPEN)                             EL6411
00422      END-EXEC.                                                    EL6411
00423                                                                   EL6411
00424      IF WS-BROWSE-STARTED = 'Y'                                   EL6411
00425          NEXT SENTENCE                                            EL6411
00426      ELSE                                                         EL6411
00427          EXEC CICS STARTBR                                        EL6411
00428              DATASET(BILL-ID)                                     EL6411
00429              RIDFLD (BILL-KEY)                                    EL6411
00430          END-EXEC                                                 EL6411
00431          MOVE 'Y'                TO WS-BROWSE-STARTED.            EL6411
00432                                                                   EL6411
00433      MOVE BILL-PARTIAL-KEY       TO BILL-SAVE-PARTIAL-KEY.        EL6411
00434                                                                   EL6411
00435  7210-READ-NEXT.                                                  EL6411
00436      EXEC CICS READNEXT                                           EL6411
00437           DATASET(BILL-ID)                                        EL6411
00438           RIDFLD (BILL-KEY)                                       EL6411
00439           SET    (ADDRESS OF BILLING-STATEMENT)                      CL**6
00440      END-EXEC.                                                    EL6411
00441                                                                   EL6411
00442      IF BILL-PARTIAL-KEY NOT = BILL-SAVE-PARTIAL-KEY              EL6411
00443         GO TO 7250-CHECK-FIRST-SW.                                EL6411
00444                                                                   EL6411
00445      IF PRINT-LABELS                                              EL6411
00446         MOVE BI-ACCT-ADDRESS-LINE TO WS-LABEL-LINES (L-INDX)      EL6411
00447         SET L-INDX               UP BY 1                          EL6411
00448         GO TO 7210-READ-NEXT.                                     EL6411
00449                                                                   EL6411
00450      MOVE BI-TEXT-LINE           TO WS-PASSED-DATA.               EL6411
00451                                                                   EL6411
00452      MOVE BI-SKIP-CONTROL        TO WS-PASSED-CNTL-CHAR.          EL6411
00453                                                                   EL6411
00454      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6411
00455                                                                   EL6411
00456      GO TO 7210-READ-NEXT.                                        EL6411
00457                                                                   EL6411
00458  7250-CHECK-FIRST-SW.                                             EL6411
00459      IF PRINT-LABELS                                              EL6411
00460         GO TO 7260-LABEL-PRINT.                                   EL6411
00461                                                                   EL6411
00462      IF FIRST-TIME                                                EL6411
00463         GO TO 7299-EXIT.                                          EL6411
00464                                                                   EL6411
00465      MOVE 'X'                 TO WS-PROG-END.                     EL6411
00466      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6411
00467      GO TO 7290-END-BR.                                           EL6411
00468                                                                   EL6411
00469  7260-LABEL-PRINT.                                                EL6411
00470      IF L-INDX = 1 OR                                             EL6411
00471         WS-LABEL-HOLD-AREA = SPACES                               EL6411
00472         GO TO 7290-END-BR.                                        EL6411
00473                                                                   EL6411
00474      IF WS-LABEL-LINES (1) = SPACES                               EL6411
00475         MOVE WS-LABEL-LINES (2)  TO WS-LABEL-LINES (1)            EL6411
00476         MOVE WS-LABEL-LINES (3)  TO WS-LABEL-LINES (2)            EL6411
00477         MOVE WS-LABEL-LINES (4)  TO WS-LABEL-LINES (3)            EL6411
00478         MOVE WS-LABEL-LINES (5)  TO WS-LABEL-LINES (4)            EL6411
00479         MOVE WS-LABEL-LINES (6)  TO WS-LABEL-LINES (5)            EL6411
00480         MOVE SPACES              TO WS-LABEL-LINES (6)            EL6411
00481         GO TO 7260-LABEL-PRINT.                                   EL6411
00482                                                                   EL6411
00483      IF WS-LABEL-LINES (2) = SPACES                               EL6411
00484         MOVE WS-LABEL-LINES (3)  TO WS-LABEL-LINES (2)            EL6411
00485         MOVE WS-LABEL-LINES (4)  TO WS-LABEL-LINES (3)            EL6411
00486         MOVE WS-LABEL-LINES (5)  TO WS-LABEL-LINES (4)            EL6411
00487         MOVE WS-LABEL-LINES (6)  TO WS-LABEL-LINES (5)            EL6411
00488         MOVE SPACES              TO WS-LABEL-LINES (6)            EL6411
00489         GO TO 7260-LABEL-PRINT.                                   EL6411
00490                                                                   EL6411
00491      IF WS-LABEL-LINES (3) = SPACES                               EL6411
00492         MOVE WS-LABEL-LINES (4)  TO WS-LABEL-LINES (3)            EL6411
00493         MOVE WS-LABEL-LINES (5)  TO WS-LABEL-LINES (4)            EL6411
00494         MOVE WS-LABEL-LINES (6)  TO WS-LABEL-LINES (5)            EL6411
00495         MOVE SPACES              TO WS-LABEL-LINES (6)            EL6411
00496         GO TO 7260-LABEL-PRINT.                                   EL6411
00497                                                                   EL6411
00498      IF WS-LABEL-LINES (4) = SPACES                               EL6411
00499         MOVE WS-LABEL-LINES (5)  TO WS-LABEL-LINES (4)            EL6411
00500         MOVE WS-LABEL-LINES (6)  TO WS-LABEL-LINES (5)            EL6411
00501         MOVE SPACES              TO WS-LABEL-LINES (6)            EL6411
00502         GO TO 7260-LABEL-PRINT.                                   EL6411
00503                                                                   EL6411
00504      IF WS-LABEL-LINES (5) = SPACES                               EL6411
00505         MOVE WS-LABEL-LINES (6)  TO WS-LABEL-LINES (5)            EL6411
00506         MOVE SPACES              TO WS-LABEL-LINES (6).           EL6411
00507                                                                   EL6411
00508      IF WS-LABEL-LINES (6) NOT = SPACES                           EL6411
00509         IF WS-LABEL-1ST-ZIP (6) NOT = ZEROS                       EL6411
00510            MOVE WS-LABEL-ZIP (6)     TO WS-LAST-ZIP (5)           EL6411
00511            MOVE SPACES               TO WS-LABEL-LINES (6)        EL6411
00512            ELSE                                                   EL6411
00513            MOVE WS-LABEL-2ND-ZIP (6) TO WS-LAST-2ND-ZIP (5)       EL6411
00514            MOVE SPACES               TO WS-LABEL-LINES (6)        EL6411
00515         ELSE                                                      EL6411
00516         IF WS-LABEL-LINES (5) NOT = SPACES                        EL6411
00517            IF WS-LABEL-1ST-ZIP (5) NOT = ZEROS                    EL6411
00518               MOVE WS-LABEL-ZIP (5)     TO WS-LAST-ZIP (5)        EL6411
00519               MOVE SPACES               TO WS-LABEL-ZIP (5)       EL6411
00520               ELSE                                                EL6411
00521               MOVE WS-LABEL-2ND-ZIP (5) TO WS-LAST-2ND-ZIP (5)    EL6411
00522               MOVE SPACES               TO WS-LABEL-ZIP (5)       EL6411
00523            ELSE                                                   EL6411
00524            IF WS-LABEL-LINES (4) NOT = SPACES                     EL6411
00525               IF WS-LABEL-1ST-ZIP (4) NOT = ZEROS                 EL6411
00526                  MOVE WS-LABEL-ZIP (4)     TO WS-LAST-ZIP (4)     EL6411
00527                  MOVE SPACES               TO WS-LABEL-ZIP (4)    EL6411
00528                  ELSE                                             EL6411
00529                  MOVE WS-LABEL-2ND-ZIP (4) TO WS-LAST-2ND-ZIP (4) EL6411
00530                  MOVE SPACES               TO WS-LABEL-ZIP (4).   EL6411
00531                                                                   EL6411
00532      PERFORM 2480-MOVE-TO-PRINT THRU 2499-EXIT.                   EL6411
00533                                                                   EL6411
00534  7290-END-BR.                                                     EL6411
00535      IF WS-BROWSE-STARTED = 'Y'                                   EL6411
00536         MOVE 'N'                 TO WS-BROWSE-STARTED             EL6411
00537         EXEC CICS ENDBR                                           EL6411
00538              DATASET(BILL-ID)                                     EL6411
00539          END-EXEC.                                                EL6411
00540                                                                      CL**2
00541      EXEC CICS SYNCPOINT                                             CL**2
00542           END-EXEC.                                                  CL**2
00543                                                                      CL**2
00544  7299-EXIT.                                                       EL6411
00545       EXIT.                                                       EL6411
00546                                                                      CL**3
00547      EJECT                                                        EL6411
00548  7300-UPDATE-BILLING-HEADER.                                      EL6411
00549      IF WS-BROWSE-STARTED = 'Y'                                   EL6411
00550         MOVE 'N'                 TO WS-BROWSE-STARTED             EL6411
00551         EXEC CICS ENDBR                                           EL6411
00552              DATASET(BILL-ID)                                     EL6411
00553         END-EXEC.                                                 EL6411
00554                                                                   EL6411
00555      PERFORM 8100-READ-HEADER THRU 8199-EXIT.                     EL6411
00556                                                                   EL6411
00557      IF HEADER-REC-FOUND                                          EL6411
00558          NEXT SENTENCE                                            EL6411
00559      ELSE                                                         EL6411
00560          GO TO 7399-EXIT.                                         EL6411
00561                                                                   EL6411
00562      MOVE J-BILL-LENGTH          TO JOURNAL-LENGTH.               EL6411
00563      MOVE 'B'                    TO JP-RECORD-TYPE.               EL6411
00564      MOVE BILL-ID                TO JP-FILE-ID.                   EL6411
00565      MOVE BILLING-STATEMENT      TO JP-RECORD-AREA.               EL6411
00566      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6411
00567      MOVE CURRENT-SAVE           TO BI-INITIAL-PRINT-DATE.        EL6411
00568      MOVE BI-NO-OF-COPIES        TO WS-COPIES.                    EL6411
00569      MOVE J-BILL-LENGTH          TO JOURNAL-LENGTH.               EL6411
00570      MOVE 'C'                    TO JP-RECORD-TYPE.               EL6411
00571      MOVE BILL-ID                TO JP-FILE-ID.                   EL6411
00572      MOVE BILLING-STATEMENT      TO JP-RECORD-AREA.               EL6411
00573                                                                   EL6411
00574      EXEC CICS REWRITE                                            EL6411
00575           DATASET(BILL-ID)                                        EL6411
00576           FROM   (BILLING-STATEMENT)                              EL6411
00577      END-EXEC.                                                    EL6411
00578                                                                   EL6411
00579      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6411
00580                                                                   EL6411
00581  7399-EXIT.                                                       EL6411
00582      EXIT.                                                        EL6411
00583                                                                      CL**3
00584      EJECT                                                        EL6411
00585  8100-READ-HEADER.                                                EL6411
00586      MOVE BILL-SAVE-KEY          TO BILL-KEY.                     EL6411
00587                                                                   EL6411
00588      EXEC CICS HANDLE CONDITION                                   EL6411
00589           NOTOPEN(8890-BILL-NOT-OPEN)                             EL6411
00590           NOTFND (8150-NOT-FOUND)                                 EL6411
00591      END-EXEC.                                                    EL6411
00592                                                                   EL6411
00593      EXEC CICS READ                                               EL6411
00594           DATASET(BILL-ID)                                        EL6411
00595           RIDFLD (BILL-KEY)                                       EL6411
00596           SET    (ADDRESS OF BILLING-STATEMENT)                      CL**6
00597           UPDATE                                                  EL6411
00598      END-EXEC.                                                    EL6411
00599                                                                   EL6411
00600      GO TO 8199-EXIT.                                             EL6411
00601                                                                   EL6411
00602  8150-NOT-FOUND.                                                  EL6411
00603      MOVE '1'                    TO HEADER-SW.                    EL6411
00604                                                                   EL6411
00605  8199-EXIT.                                                       EL6411
00606       EXIT.                                                       EL6411
00607                                                                      CL**3
00608      EJECT                                                        EL6411
00609  8400-LOG-JOURNAL-RECORD.                                         EL6411
00610      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   EL6411
00611      MOVE THIS-PGM               TO JP-PROGRAM-ID.                EL6411
00612                                                                   EL6411
00613 *    EXEC CICS JOURNAL                                            EL6411
00614 *         JFILEID(PI-JOURNAL-FILE-ID)                             EL6411
00615 *         JTYPEID('EL')                                           EL6411
00616 *         FROM   (JOURNAL-RECORD)                                 EL6411
00617 *         LENGTH (JOURNAL-LENGTH)                                 EL6411
00618 *    END-EXEC.                                                    EL6411
00619                                                                   EL6411
00620  8890-BILL-NOT-OPEN.                                              EL6411
00621      MOVE 'BILLING STATEMENT FILE NOT OPEN - ERBILL' TO ERROR-LINEEL6411
00622      PERFORM 0400-SEND-TEXT                                       EL6411
00623      GO TO 0200-END-DATA.                                         EL6411
00624                                                                   EL6411
00625      EJECT                                                        EL6411
00626  9700-DATE-LINK.                                                  EL6411
00627      MOVE LINK-ELDATCV TO PGM-NAME                                EL6411
00628      EXEC CICS LINK                                               EL6411
00629          PROGRAM (PGM-NAME)                                       EL6411
00630          COMMAREA(DATE-CONVERSION-DATA)                           EL6411
00631          LENGTH  (DC-COMM-LENGTH)                                 EL6411
00632      END-EXEC.                                                    EL6411
00633                                                                   EL6411
00634  9700-EXIT.                                                       EL6411
00635       EXIT.                                                       EL6411
00636      EJECT                                                        EL6411
uktdel*9800-PRINT-ROUTINE.             COPY ELPRTCVP.                   EL6411
uktins 9800-PRINT-ROUTINE.
uktins     COPY ELPRTCVP.
00638                                                                   EL6411
00639  9995-SECURITY-VIOLATION.                                         EL6411
00640                              COPY ELCSCTP.                        EL6411
00641                                                                   EL6411
00642  9995-EXIT.                                                       EL6411
00643      EXIT.                                                        EL6411
