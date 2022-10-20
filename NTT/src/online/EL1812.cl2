00001  IDENTIFICATION DIVISION.                                         06/26/96
00002                                                                   EL1812
00003  PROGRAM-ID.                 EL1812.                                 LV006
00004 *              PROGRAM CONVERTED BY                                  CL**5
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**5
00006 *              CONVERSION DATE 02/12/96 08:55:21.                    CL**5
00007 *                            VMOD=2.006.                             CL**6
00008 *                                                                 EL1812
00009 *AUTHOR.           LOGIC,INC.                                        CL**5
00010 *                  DALLAS,TEXAS.                                     CL**5
00011                                                                   EL1812
00024 *REMARKS. TRANSACTION EX52 - FILE FOLDER LABEL PRINT PROGRAM.        CL**2
121802******************************************************************
121802*                   C H A N G E   L O G
121802*
121802* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
121802*-----------------------------------------------------------------
121802*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
121802* EFFECTIVE    NUMBER
121802*-----------------------------------------------------------------
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
121802******************************************************************
00025      EJECT                                                        EL1812
00026  ENVIRONMENT DIVISION.                                            EL1812
00027  DATA DIVISION.                                                   EL1812
00028  WORKING-STORAGE SECTION.                                         EL1812
00029  77  FILLER  PIC X(32)  VALUE '********************************'. EL1812
00030  77  FILLER  PIC X(32)  VALUE '*   EL1812  WORKING STORAGE    *'. EL1812
00031  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.006 ************'.    CL**6
00032                                                                   EL1812
00033  01  WS-CONSTANTS.                                                EL1812
00034      12  THIS-PGM                PIC X(8)    VALUE 'EL1812'.         CL**6
00035      12  PGM-NAME                PIC X(8).                        EL1812
00036      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     EL1812
00037      12  MSTR-ID                 PIC X(8)    VALUE 'ELMSTR'.      EL1812
00038                                                                      CL**5
00039  01  WS-WORK-AREA.                                                   CL**5
00040      12  W-TIMER                 PIC S9(03) COMP-3 VALUE ZEROS.      CL**5
00041      12  MSTR-KEY.                                                EL1812
00042          16  MSTR-CO             PIC X.                           EL1812
00043          16  MSTR-CARRIER        PIC X.                           EL1812
00044          16  FILLER              PIC X(17).                       EL1812
00045                                                                   EL1812
00046      12  ERROR-LINE              PIC X(80).                       EL1812
00047      12  BROWSE-STARTED          PIC X VALUE 'N'.                 EL1812
00048      12  SUB                     PIC 9  COMP-3.                      CL**5
00049      12  WS-SKIP                 PIC 99.                          EL1812
00050      12  WS-LABEL-HOLD-AREA.                                      EL1812
00051          16  CONTROL-LINE.                                        EL1812
00052              20  WS-CARR         PIC X.                           EL1812
00053              20  FILLER          PIC X     VALUE SPACES.          EL1812
00054              20  WS-CLAIMNO      PIC X(7).                        EL1812
00055              20  FILLER          PIC X     VALUE SPACES.          EL1812
00056              20  WS-CERTNO       PIC X(11).                       EL1812
00057              20  FILLER          PIC X(2)  VALUE SPACES.          EL1812
00058              20  WS-TYPE         PIC X(6).                        EL1812
00059              20  FILLER          PIC X(1)  VALUE SPACES.          EL1812
00060                                                                   EL1812
00061          16  NAME-LINE           PIC X(30).                       EL1812
00062                                                                   EL1812
00063          16  ST-ACCT-LINE.                                        EL1812
00064              20  FILLER          PIC X(3)  VALUE 'ST-'.           EL1812
00065              20  WS-STATE        PIC XX.                          EL1812
00066              20  FILLER          PIC X(4)  VALUE ' AC-'.          EL1812
00067              20  WS-ACCT         PIC X(10).                       EL1812
00068              20  FILLER          PIC X(5) VALUE ' GRP-'.          EL1812
00069              20  WS-GRP          PIC X(6).                        EL1812
00070                                                                   EL1812
00071          16  DATE-LINE.                                           EL1812
00072              20  FILLER          PIC X(4)  VALUE 'INC-'.          EL1812
00073              20  WS-INCUR-DT     PIC X(8).                        EL1812
00074              20  FILLER          PIC X(7)  VALUE '  ESTB-'.       EL1812
00075              20  WS-EST-DT       PIC X(8).                        EL1812
00076              20  FILLER          PIC XXX  VALUE SPACES.           EL1812
00077                                  COPY ELCDMD34.                      CL**6
00078      EJECT                                                        EL1812
00079                                  COPY ELCNWA.                        CL**4
00080      EJECT                                                        EL1812
00081                                  COPY ELCINTF.                       CL**4
00082      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.                    EL1812
00083          16  PI-PRINT-TYPE       PIC X.                           EL1812
00084          16  PI-ON-DATE          PIC XX.                          EL1812
00085          16  PI-THRU-DATE        PIC XX.                          EL1812
00086          16  FILLER              PIC X(635).                         CL**5
00087      EJECT                                                        EL1812
00088                                  COPY ELPRTCVD.                      CL**4
00089      EJECT                                                        EL1812
00090                                  COPY ELCDATE.                       CL**4
00091      EJECT                                                        EL1812
00092  LINKAGE SECTION.                                                 EL1812
00093  01  DFHCOMMAREA                 PIC X(1024).                     EL1812
00094                                                                   EL1812
00095                                  COPY ELCMSTR.                       CL**4
00096      EJECT                                                        EL1812
00097  PROCEDURE DIVISION.                                              EL1812
00098                                                                   EL1812
00099      MOVE SPACES                 TO DL34-PROCESS-TYPE.               CL**6
00100                                                                      CL**6
00101  0100-RETRIEVE-LOOP.                                              EL1812
00102      EXEC CICS HANDLE CONDITION                                   EL1812
00103           ENDDATA  (200-END-DATA)                                 EL1812
00104           NOTFND   (300-NOT-FOUND)                                EL1812
00105           END-EXEC.                                               EL1812
00106                                                                   EL1812
00107      EXEC CICS RETRIEVE                                           EL1812
00108           INTO    (PROGRAM-INTERFACE-BLOCK)                       EL1812
00109           LENGTH  (PI-COMM-LENGTH)                                EL1812
00110           END-EXEC.                                               EL1812
00111                                                                   EL1812
00112      GO TO 5000-PRINT-LABELS.                                     EL1812
00113      EJECT                                                        EL1812
00114  200-END-DATA.                                                    EL1812
00115                                                                      CL**6
00116 * DLO034 CLOSE                                                       CL**6
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**6
00118          MOVE 'C'                TO DL34-PROCESS-TYPE                CL**6
00119          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID                  CL**6
00120          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID            CL**6
00121          MOVE PI-PROCESSOR-ID    TO DL34-USERID                      CL**6
00122          MOVE SPACES             TO DL34-PRINT-LINE                  CL**6
00123                                     DL34-OVERRIDE-PRINTER-ID         CL**6
00124          EXEC CICS LINK                                              CL**6
00125              PROGRAM    ('DLO034')                                   CL**6
00126              COMMAREA   (DLO034-COMMUNICATION-AREA)                  CL**6
00127              LENGTH     (DLO034-REC-LENGTH)                          CL**6
00128          END-EXEC                                                    CL**6
00129          IF DL34-RETURN-CODE NOT = 'OK'                              CL**6
00130              MOVE  '**DLO034 CLOSE ERROR - ABORT**'                  CL**6
00131                                  TO ERROR-LINE                       CL**6
00132              PERFORM 400-SEND-TEXT.                                  CL**6
00133                                                                      CL**6
00134      EXEC CICS RETURN                                             EL1812
00135           END-EXEC.                                               EL1812
00136                                                                   EL1812
00137  300-NOT-FOUND.                                                   EL1812
00138      MOVE 'NO COMMUNICATION AREA FOUND' TO ERROR-LINE.            EL1812
00139      PERFORM 400-SEND-TEXT.                                       EL1812
00140      GO TO 200-END-DATA.                                          EL1812
00141                                                                   EL1812
00142  400-SEND-TEXT.                                                   EL1812
00143      EXEC CICS SEND TEXT                                          EL1812
00144           FROM    (ERROR-LINE)                                    EL1812
00145           LENGTH  (70)                                            EL1812
00146           END-EXEC.                                               EL1812
00147                                                                   EL1812
00148      EJECT                                                        EL1812
00149  5000-PRINT-LABELS.                                               EL1812
00150                                                                      CL**6
00151 * DLO034 OPEN WHEN DMD OR CID                                        CL**6
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**6
00153          IF DL34-PROCESS-TYPE IS EQUAL TO SPACES                     CL**6
00154              MOVE 'O'                TO DL34-PROCESS-TYPE            CL**6
00155              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID              CL**6
00156              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID        CL**6
00157              MOVE PI-PROCESSOR-ID    TO DL34-USERID                  CL**6
00158              MOVE SPACES             TO DL34-PRINT-LINE              CL**6
00159              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID     CL**6
00160              EXEC CICS LINK                                          CL**6
00161                  PROGRAM    ('DLO034')                               CL**6
00162                  COMMAREA   (DLO034-COMMUNICATION-AREA)              CL**6
00163                  LENGTH     (DLO034-REC-LENGTH)                      CL**6
00164              END-EXEC                                                CL**6
00165              IF DL34-RETURN-CODE NOT = 'OK'                          CL**6
00166                  MOVE  '**DLO034 OPEN ERROR - ABORT**'               CL**6
00167                                      TO ERROR-LINE                   CL**6
00168                  PERFORM 400-SEND-TEXT                               CL**6
00169                  EXEC CICS RETURN                                    CL**6
00170                  END-EXEC.                                           CL**6
00171                                                                      CL**6
00172      MOVE 30                     TO WS-LINE-LEN.                  EL1812
00173                                                                   EL1812
00174      IF PI-PRINT-TYPE = '3'                                       EL1812
00175         PERFORM 8000-ALIGNMENT-ROUTINE 6 TIMES                    EL1812
00176         MOVE 'X'                 TO WS-PROG-END                   EL1812
CIDMOD        PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                       EL1812
00178         GO TO 0100-RETRIEVE-LOOP.                                 EL1812
00179                                                                   EL1812
00180      MOVE LOW-VALUES             TO MSTR-KEY.                     EL1812
00181      MOVE PI-COMPANY-CD          TO MSTR-CO.                      EL1812
00182                                                                   EL1812
00183      IF NOT PI-NO-CARRIER-SECURITY                                EL1812
00184         MOVE PI-CARRIER-SECURITY TO MSTR-CARRIER.                 EL1812
00185                                                                   EL1812
00186      EXEC CICS HANDLE CONDITION                                   EL1812
00187           NOTFND   (0100-RETRIEVE-LOOP)                           EL1812
00188           END-EXEC.                                               EL1812
00189                                                                   EL1812
00190      EXEC CICS STARTBR                                            EL1812
00191           DATASET  (MSTR-ID)                                      EL1812
00192           RIDFLD   (MSTR-KEY)                                     EL1812
00193           GTEQ                                                    EL1812
00194           END-EXEC.                                               EL1812
00195                                                                   EL1812
00196      EXEC CICS HANDLE CONDITION                                   EL1812
00197           NOTFND   (5050-END-BR)                                  EL1812
00198           ENDFILE  (5050-END-BR)                                  EL1812
00199           END-EXEC.                                               EL1812
00200                                                                   EL1812
00201      MOVE 'Y'                    TO BROWSE-STARTED.               EL1812
00202                                                                   EL1812
00203  5010-READ-NEXT.                                                  EL1812
00204      EXEC CICS READNEXT                                           EL1812
00205           DATASET  (MSTR-ID)                                      EL1812
00206           RIDFLD   (MSTR-KEY)                                     EL1812
00207           SET      (ADDRESS OF CLAIM-MASTER)                         CL**5
00208           END-EXEC.                                               EL1812
00209                                                                   EL1812
00210      ADD +1                      TO W-TIMER.                         CL**5
00211      IF  W-TIMER EQUAL +30                                           CL**5
00212          MOVE ZEROS              TO W-TIMER                          CL**5
00213          EXEC CICS DELAY                                             CL**3
00214              INTERVAL(1) END-EXEC.                                   CL**3
00215                                                                      CL**3
00216      IF MSTR-CO NOT = PI-COMPANY-CD                               EL1812
00217         GO TO 5050-END-BR.                                        EL1812
00218                                                                   EL1812
00219      IF NOT PI-NO-CARRIER-SECURITY                                EL1812
00220          IF CL-CARRIER GREATER THAN PI-CARRIER-SECURITY           EL1812
00221             GO TO 5050-END-BR.                                    EL1812
00222                                                                   EL1812
00223      IF NOT PI-NO-ACCOUNT-SECURITY                                EL1812
00224         IF CL-CERT-ACCOUNT NOT = PI-ACCOUNT-SECURITY              EL1812
00225            GO TO 5010-READ-NEXT.                                  EL1812
00226                                                                   EL1812
00227      IF PI-ON-DATE = LOW-VALUES                                   EL1812
00228         PERFORM 6000-BUILD-LABEL THRU 6099-EXIT                   EL1812
00229         GO TO 5010-READ-NEXT.                                     EL1812
00230                                                                   EL1812
00231      IF PI-THRU-DATE = LOW-VALUES                                 EL1812
00232         IF PI-ON-DATE  =  CL-FILE-ESTABLISH-DT                    EL1812
00233            PERFORM 6000-BUILD-LABEL THRU 6099-EXIT                EL1812
00234            GO TO 5010-READ-NEXT                                   EL1812
00235         ELSE                                                      EL1812
00236            GO TO 5010-READ-NEXT.                                  EL1812
00237                                                                   EL1812
00238      IF CL-FILE-ESTABLISH-DT GREATER THAN PI-THRU-DATE  OR        EL1812
00239                              LESS    THAN PI-ON-DATE              EL1812
00240         GO TO 5010-READ-NEXT.                                     EL1812
00241                                                                   EL1812
00242      PERFORM 6000-BUILD-LABEL THRU 6099-EXIT.                     EL1812
00243      GO TO 5010-READ-NEXT.                                        EL1812
00244                                                                   EL1812
00245  5050-END-BR.                                                     EL1812
00246      IF BROWSE-STARTED = 'Y'                                      EL1812
00247         EXEC CICS ENDBR                                           EL1812
00248              DATASET  (MSTR-ID)                                   EL1812
00249              END-EXEC.                                            EL1812
00250                                                                   EL1812
00251      MOVE '1'                    TO WS-PASSED-CNTL-CHAR.          EL1812
00252      MOVE 'LABEL PRINTING COMPLETED' TO WS-PASSED-DATA.           EL1812
00253      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1812
00254      MOVE SPACES                 TO WS-PASSED-DATA.               EL1812
00255      MOVE SPACES                 TO WS-PASSED-CNTL-CHAR.          EL1812
00256      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1812
00257      MOVE SPACES                 TO WS-PASSED-DATA.               EL1812
00258      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1812
00259      MOVE 'X'                    TO WS-PROG-END.                  EL1812
CIDMOD     PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1812
00261                                                                   EL1812
00262      GO TO 0100-RETRIEVE-LOOP.                                    EL1812
00263                                                                   EL1812
00264      EJECT                                                        EL1812
00265  6000-BUILD-LABEL.                                                EL1812
00266      MOVE CL-CARRIER             TO WS-CARR.                      EL1812
00267      MOVE CL-CLAIM-NO            TO WS-CLAIMNO.                   EL1812
00268      MOVE CL-CERT-NO             TO WS-CERTNO.                    EL1812
00269                                                                   EL1812
121802     EVALUATE TRUE
121802     WHEN CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1
00271         MOVE PI-AH-OVERRIDE-L6   TO WS-TYPE

121802     WHEN CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
00273         MOVE PI-LIFE-OVERRIDE-L6 TO WS-TYPE

121802     WHEN CL-CLAIM-TYPE = 'I'
121802        MOVE '  IU  '            TO WS-TYPE

121802     WHEN CL-CLAIM-TYPE = 'G'
121802        MOVE ' GAP  '            TO WS-TYPE
052614
052614     WHEN CL-CLAIM-TYPE = 'F'
052614        MOVE ' FAM  '            TO WS-TYPE
080322
080322     WHEN CL-CLAIM-TYPE = 'B'
080322        MOVE ' BRV  '            TO WS-TYPE
080322
080322     WHEN CL-CLAIM-TYPE = 'H'
080322        MOVE ' HOSP '            TO WS-TYPE
100518
100518     WHEN CL-CLAIM-TYPE = 'O'
100518        MOVE ' OTH  '            TO WS-TYPE

121802     END-EVALUATE

00274                                                                   EL1812
00275      IF CL-INCURRED-DT NOT = LOW-VALUE AND SPACES                 EL1812
00276         MOVE CL-INCURRED-DT      TO DC-BIN-DATE-1                 EL1812
00277         MOVE SPACES              TO DC-OPTION-CODE                EL1812
00278         PERFORM 9700-DATE-LINK THRU 9700-EXIT                     EL1812
00279         MOVE DC-GREG-DATE-1-EDIT TO WS-INCUR-DT                   EL1812
00280      ELSE                                                         EL1812
00281         MOVE SPACES              TO WS-INCUR-DT.                  EL1812
00282                                                                   EL1812
00283      IF CL-FILE-ESTABLISH-DT NOT = LOW-VALUE AND SPACES           EL1812
00284         MOVE CL-FILE-ESTABLISH-DT TO DC-BIN-DATE-1                EL1812
00285         MOVE SPACES              TO DC-OPTION-CODE                EL1812
00286         PERFORM 9700-DATE-LINK THRU 9700-EXIT                     EL1812
00287         MOVE DC-GREG-DATE-1-EDIT TO WS-EST-DT                     EL1812
00288      ELSE                                                         EL1812
00289         MOVE SPACES              TO WS-EST-DT.                    EL1812
00290                                                                   EL1812
00291      PERFORM 7000-MOVE-NAME.                                      EL1812
00292      MOVE WS-NAME-WORK           TO NAME-LINE.                    EL1812
00293                                                                   EL1812
00294      MOVE CL-CERT-STATE          TO WS-STATE.                     EL1812
00295      MOVE CL-CERT-ACCOUNT        TO WS-ACCT.                      EL1812
00296      MOVE CL-CERT-GROUPING       TO WS-GRP.                       EL1812
00297      MOVE '1'                    TO WS-PASSED-CNTL-CHAR.          EL1812
00298                                                                   EL1812
00299      IF PI-PRINT-TYPE = '1'                                       EL1812
00300         MOVE CONTROL-LINE        TO WS-PASSED-DATA                EL1812
00301         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                       EL1812
00302         MOVE SPACE               TO WS-PASSED-CNTL-CHAR           EL1812
00303         MOVE NAME-LINE           TO WS-PASSED-DATA                EL1812
00304         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                       EL1812
00305      ELSE                                                         EL1812
00306         MOVE NAME-LINE           TO WS-PASSED-DATA                EL1812
00307         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                       EL1812
00308         MOVE SPACE               TO WS-PASSED-CNTL-CHAR           EL1812
00309         MOVE CONTROL-LINE        TO WS-PASSED-DATA                EL1812
00310         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                      EL1812
00311                                                                   EL1812
00312      MOVE ST-ACCT-LINE           TO WS-PASSED-DATA.               EL1812
00313                                                                   EL1812
00314      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1812
00315      MOVE DATE-LINE              TO WS-PASSED-DATA                EL1812
00316      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1812
00317                                                                   EL1812
00318  6099-EXIT.                                                       EL1812
00319       EXIT.                                                       EL1812
00320      EJECT                                                        EL1812
00321  7000-MOVE-NAME   SECTION.                                           CL**4
00322                                  COPY ELCMNS.                        CL**4
00323      EJECT                                                        EL1812
00324  8000-ALIGNMENT-ROUTINE.                                          EL1812
00325      MOVE '1'                    TO WS-PASSED-CNTL-CHAR.          EL1812
00326      MOVE ALL '*'                TO WS-PASSED-DATA.               EL1812
00327      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1812
00328      MOVE SPACE                  TO WS-PASSED-CNTL-CHAR.          EL1812
00329      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 3 TIMES.                 EL1812
00330                                                                   EL1812
00331  8890-MSTR-NOT-OPEN.                                              EL1812
00332      MOVE 'MASTER  FILE NOT OPEN ' TO ERROR-LINE.                 EL1812
00333      PERFORM 400-SEND-TEXT.                                       EL1812
00334      GO TO 200-END-DATA.                                          EL1812
00335                                                                   EL1812
00336      EJECT                                                        EL1812
00337  9700-DATE-LINK.                                                  EL1812
00338      MOVE LINK-ELDATCV TO PGM-NAME.                               EL1812
00339                                                                   EL1812
00340      EXEC CICS LINK                                               EL1812
00341          PROGRAM   (PGM-NAME)                                     EL1812
00342          COMMAREA  (DATE-CONVERSION-DATA)                         EL1812
00343          LENGTH    (DC-COMM-LENGTH)                               EL1812
00344          END-EXEC.                                                EL1812
00345                                                                   EL1812
00346  9700-EXIT.                                                       EL1812
00347       EXIT.                                                       EL1812
00348      EJECT                                                        EL1812
uktdel*9800-PRINT-ROUTINE.             COPY ELPRTCVP.                   EL1812
uktins 9800-PRINT-ROUTINE.
uktins     COPY ELPRTCVP.
00350                                                                      CL**6
00351                                                                      CL**6
