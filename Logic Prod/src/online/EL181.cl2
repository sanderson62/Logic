00001  ID DIVISION.                                                     06/26/96
00002                                                                   EL181
00003  PROGRAM-ID.                 EL181.                                  LV006
00004 *              PROGRAM CONVERTED BY                                  CL**5
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**5
00006 *              CONVERSION DATE 02/12/96 08:54:30.                    CL**5
00007 *                            VMOD=2.006                              CL**6
00008 *                                                                 EL181
00008 *                                                                 EL181
00009 *AUTHOR.     LOGIC,INC.                                              CL**5
00010 *            DALLAS, TEXAS.                                          CL**5
00011                                                                   EL181
00012 *DATE-COMPILED.                                                      CL**5
00013                                                                   EL181
00014 *SECURITY.   *****************************************************   CL**5
00015 *            *                                                   *   CL**5
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**5
00017 *            *                                                   *   CL**5
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**5
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**5
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**5
00021 *            *                                                   *   CL**5
00022 *            *****************************************************   CL**5
00023                                                                   EL181
00024 *REMARKS.    TRANSACTION - EX51 - LABEL PRINT REQUEST.               CL**2
00025 *        THIS FUNCTION IS USED TO START THE PRINTING OF LABELS       CL**2
00026 *        FOR FILE FOLDERS. THE LABELS MAY BE REQUESTED FOR CLAIMS    CL**2
00027 *        THAT WERE ESTABLISHED WITHIN A RANGE OF DATES OR FOR        CL**2
00028 *        THE ENTIRE FILE.                                            CL**2
00029                                                                   EL181
00030                                                                   EL181
00031      EJECT                                                        EL181
00032  ENVIRONMENT DIVISION.                                            EL181
00033  DATA DIVISION.                                                   EL181
00034  WORKING-STORAGE SECTION.                                         EL181
00035  77  FILLER  PIC X(32)  VALUE '********************************'. EL181
00036  77  FILLER  PIC X(32)  VALUE '*    EL181 WORKING STORAGE     *'. EL181
00037  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.006 *********'.    CL**6
00038                                                                   EL181
00039                                  COPY ELCSCTM.                       CL**3
00040                                                                   EL181
00041                                  COPY ELCSCRTY.                      CL**3
00042                                                                   EL181
00043  01  WS-DATE-AREA.                                                EL181
00044      05  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL181
00045      05  SAVE-BIN-DATE           PIC XX      VALUE SPACES.        EL181
00046                                                                   EL181
00047  01  STANDARD-AREAS.                                              EL181
00048      12  SC-ITEM                 PIC S9(4)   VALUE +0001  COMP.   EL181
00049      12  MAP-NAME.                                                EL181
00050          16  MAP-PREFIX          PIC XX      VALUE 'EL'.          EL181
00051          16  MAP-NUMBER          PIC X(4)    VALUE '181A'.        EL181
00052          16  MAP-FILLER          PIC XX      VALUE '  '.          EL181
00053      12  MAPSET-NAME             PIC X(8)    VALUE 'EL181S'.      EL181
00054      12  TRANS-ID                PIC X(4)    VALUE 'EX51'.        EL181
00055      12  PRINT-TRANS             PIC X(4)    VALUE 'EX52'.        EL181
00056      12  PGM-NAME                PIC X(8).                        EL181
00057      12  TIME-IN                 PIC S9(7).                       EL181
00058      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL181
00059          16  FILLER              PIC X.                           EL181
00060          16  TIME-OUT            PIC 99V99.                       EL181
00061          16  FILLER              PIC XX.                          EL181
00062      12  XCTL-005                PIC X(5)    VALUE 'EL005'.       EL181
00063      12  XCTL-010                PIC X(5)    VALUE 'EL010'.       EL181
00064      12  XCTL-126                PIC X(5)    VALUE 'EL126'.       EL181
00065      12  LINK-001                PIC X(5)    VALUE 'EL001'.       EL181
00066      12  LINK-004                PIC X(5)    VALUE 'EL004'.       EL181
00067      12  LINK-ELDATCV            PIC X(7)    VALUE 'ELDATCV'.     EL181
00068      12  THIS-PGM                PIC X(8)    VALUE 'EL181'.       EL181
00069      12  DATE-WORK               PIC 9(7).                        EL181
00070      12  DT-REDEF REDEFINES DATE-WORK.                            EL181
00071          16  FILLER              PIC XX.                          EL181
00072          16  DT-WORK             PIC 9(5).                        EL181
00073      12  CURRENT-SAVE            PIC XX.                          EL181
00074      12  DEEDIT-FIELD            PIC X(15).                       EL181
00075      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD PIC S9(15).       EL181
00076      12  CNTL-ID                 PIC X(8)    VALUE 'ELCNTL'.      EL181
00077      12  WS-PRINTER-ID           PIC X(4).                        EL181
00078      12  ER-0004                 PIC X(4)    VALUE '0004'.        EL181
00079      12  ER-0029                 PIC X(4)    VALUE '0029'.        EL181
00080      12  ER-0042                 PIC X(4)    VALUE '0042'.        EL181
00081      12  ER-0070                 PIC X(4)    VALUE '0070'.        EL181
00082      12  ER-0190                 PIC X(4)    VALUE '0190'.        EL181
00083      12  ER-0314                 PIC X(4)    VALUE '0314'.        EL181
00084      12  ER-0411                 PIC X(4)    VALUE '0411'.        EL181
00085      12  ER-0412                 PIC X(4)    VALUE '0412'.        EL181
00086      12  ER-0413                 PIC X(4)    VALUE '0413'.        EL181
00087      12  ER-0503                 PIC X(4)    VALUE '0503'.        EL181
00088      12  ER-0504                 PIC X(4)    VALUE '0504'.        EL181
00089      12  ER-0505                 PIC X(4)    VALUE '0505'.        EL181
00090      12  ER-0506                 PIC X(4)    VALUE '0506'.        EL181
00091      12  ER-0507                 PIC X(4)    VALUE '0507'.        EL181
00092      12  ER-1579                 PIC X(4)    VALUE '1579'.           CL**4
00093      12  ER-2379                 PIC X(4)    VALUE '2379'.        EL181
00094      12  ER-7008                 PIC X(4)    VALUE '7008'.        EL181
00095                                                                   EL181
00096      12  CNTL-KEY.                                                EL181
00097          16  CNTL-CO-ID          PIC X(3).                        EL181
00098          16  CNTL-RECORD-TYPE    PIC X       VALUE '1'.           EL181
00099          16  CNTL-GENL.                                           EL181
00100            18 CNTL-GEN1          PIC XX      VALUE SPACES.        EL181
00101            18 CNTL-GEN2.                                          EL181
00102              20 CNTL-GEN3        PIC X       VALUE SPACES.        EL181
00103              20 CNTL-GEN4        PIC X       VALUE SPACES.        EL181
00104          16  CNTL-SEQ            PIC S9(4)   VALUE +0    COMP.    EL181
00105      EJECT                                                        EL181
00106                                  COPY ELCDATE.                       CL**3
00107      EJECT                                                        EL181
00108                                  COPY ELCLOGOF.                      CL**3
00109      EJECT                                                        EL181
00110                                  COPY ELCATTR.                       CL**3
00111      EJECT                                                        EL181
00112                                  COPY ELCEMIB.                       CL**3
00113      EJECT                                                        EL181
00114                                  COPY ELCINTF.                       CL**3
00115      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.                    EL181
00116 **********************************************************        EL181
00117 *    NOTE                                                *        EL181
00118 *        THE WORK AREA IS USED BY EL181 AND EL1812       *        EL181
00119 *        AND CANNOT BE REARRANGED WITHOUT COMPILING      *        EL181
00120 *        BOTH PROGRAMS.                                  *        EL181
00121 **********************************************************        EL181
00122          16  PI-PRINT-TYPE       PIC X.                           EL181
00123          16  PI-ON-DATE          PIC XX.                          EL181
00124          16  PI-THRU-DATE        PIC XX.                          EL181
00125          16  FILLER              PIC X(635).                         CL**5
00126      EJECT                                                        EL181
00127                                  COPY ELCAID.                        CL**3
00128                                                                   EL181
00129  01  FILLER    REDEFINES DFHAID.                                  EL181
00130      12  FILLER                  PIC X(8).                        EL181
00131      12  PF-VALUES               PIC X       OCCURS 2.            EL181
00132      EJECT                                                        EL181
00133                                  COPY EL181S.                        CL**3
00134      EJECT                                                        EL181
00135  LINKAGE SECTION.                                                 EL181
00136  01  DFHCOMMAREA                 PIC X(1024).                     EL181
00137                                                                   EL181
00138      EJECT                                                        EL181
00139 *01 PARMLIST .                                                       CL**5
00140 *    02  FILLER                  PIC S9(8)   COMP.                   CL**5
00141 *    02  CNTL-POINTER            PIC S9(8)   COMP.                   CL**5
00142      EJECT                                                        EL181
00143                                  COPY ELCCNTL SUPPRESS.              CL**5
00144                                                                   EL181
00145      EJECT                                                        EL181
00146  PROCEDURE DIVISION.                                              EL181
00147                                                                   EL181
00148      CONTINUE.                                                       CL**5
00149                                                                   EL181
00150      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL181
00151      MOVE '5'                    TO DC-OPTION-CODE.               EL181
00152      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       EL181
00153      MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE.                    EL181
00154      MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE.                EL181
00155                                                                   EL181
00156      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL181
00157      MOVE 2 TO EMI-NUMBER-OF-LINES.                               EL181
00158                                                                   EL181
00159      IF EIBCALEN = 0                                              EL181
00160          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL181
00161                                                                   EL181
00162      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL181
00163          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL181
00164              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL181
00165              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL181
00166              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL181
00167              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL181
00168              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL181
00169              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL181
00170              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL181
00171              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL181
00172              MOVE LOW-VALUES           TO EL181AO                 EL181
00173              GO TO 8100-SEND-INITIAL-MAP.                         EL181
00174                                                                   EL181
00175      EXEC CICS HANDLE CONDITION                                   EL181
00176          PGMIDERR  (9600-PGMID-ERROR)                             EL181
00177          ERROR     (9990-ABEND)                                   EL181
00178          END-EXEC.                                                EL181
00179                                                                   EL181
00180      IF EIBAID = DFHCLEAR                                         EL181
00181          GO TO 9400-CLEAR.                                        EL181
00182                                                                   EL181
00183      IF PI-PROCESSOR-ID = 'LGXX'                                  EL181
00184          GO TO 0200-RECEIVE.                                      EL181
00185                                                                   EL181
00186      EXEC CICS READQ TS                                           EL181
00187          QUEUE   (PI-SECURITY-TEMP-STORE-ID)                      EL181
00188          INTO    (SECURITY-CONTROL)                               EL181
00189          LENGTH  (SC-COMM-LENGTH)                                 EL181
00190          ITEM    (SC-ITEM)                                        EL181
00191      END-EXEC.                                                    EL181
00192                                                                   EL181
00193      MOVE SC-CLAIMS-DISPLAY (18) TO  PI-DISPLAY-CAP.              EL181
00194      MOVE SC-CLAIMS-UPDATE  (18) TO  PI-MODIFY-CAP.               EL181
00195                                                                   EL181
00196      IF NOT MODIFY-CAP                                            EL181
00197          MOVE 'UPDATE'           TO  SM-READ                      EL181
00198          PERFORM 9995-SECURITY-VIOLATION                          EL181
00199          MOVE ER-0070            TO  EMI-ERROR                    EL181
00200          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL181
00201          GO TO 8100-SEND-INITIAL-MAP.                             EL181
00202                                                                   EL181
00203      EJECT                                                        EL181
00204  0200-RECEIVE.                                                    EL181
00205      MOVE LOW-VALUES TO EL181AI.                                  EL181
00206                                                                   EL181
00207      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL181
00208          MOVE ER-7008            TO EMI-ERROR                     EL181
00209          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL181
00210          MOVE -1                 TO ONDATEL                       EL181
00211          GO TO 8200-SEND-DATAONLY.                                EL181
00212                                                                   EL181
00213      EXEC CICS RECEIVE                                            EL181
00214          MAP     (MAP-NAME)                                       EL181
00215          MAPSET  (MAPSET-NAME)                                    EL181
00216          INTO    (EL181AI)                                        EL181
00217          END-EXEC.                                                EL181
00218                                                                   EL181
00219      IF ENTERPFL = 0                                              EL181
00220          GO TO 0300-CHECK-PFKEYS.                                 EL181
00221                                                                   EL181
00222      IF EIBAID NOT = DFHENTER                                     EL181
00223          MOVE ER-0004            TO EMI-ERROR                     EL181
00224          GO TO 0320-INPUT-ERROR.                                  EL181
00225                                                                   EL181
00226      IF (ENTERPFI NUMERIC) AND (ENTERPFI > 0 AND < 25)            EL181
00227          MOVE PF-VALUES (ENTERPFI) TO EIBAID                      EL181
00228      ELSE                                                         EL181
00229          MOVE ER-0029            TO EMI-ERROR                     EL181
00230          GO TO 0320-INPUT-ERROR.                                  EL181
00231                                                                   EL181
00232  0300-CHECK-PFKEYS.                                               EL181
00233      IF EIBAID = DFHPF23                                          EL181
00234          GO TO 8810-PF23.                                         EL181
00235                                                                   EL181
00236      IF EIBAID = DFHPF24                                          EL181
00237          GO TO 9200-RETURN-MAIN-MENU.                             EL181
00238                                                                   EL181
00239      IF EIBAID = DFHPF12                                          EL181
00240          GO TO 9500-PF12.                                         EL181
00241                                                                   EL181
00242      IF EIBAID = DFHENTER                                         EL181
00243          GO TO 0330-FUNCTION-CHECK.                               EL181
00244                                                                   EL181
00245      MOVE ER-0029                TO EMI-ERROR.                    EL181
00246                                                                   EL181
00247  0320-INPUT-ERROR.                                                EL181
00248      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL181
00249      MOVE AL-UNBON               TO ENTERPFA.                     EL181
00250                                                                   EL181
00251      IF ENTERPFL = 0                                              EL181
00252          MOVE -1                 TO PRTSEQL                       EL181
00253      ELSE                                                         EL181
00254          MOVE -1                 TO ENTERPFL.                     EL181
00255                                                                   EL181
00256      GO TO 8200-SEND-DATAONLY.                                    EL181
00257                                                                   EL181
00258      EJECT                                                        EL181
00259  0330-FUNCTION-CHECK.                                             EL181
00260      IF PRTSEQI = '1'  OR  '2'  OR  '3'                           EL181
00261         MOVE PRTSEQI             TO PI-PRINT-TYPE                 EL181
00262         MOVE AL-UNNON            TO PRTSEQA                       EL181
00263      ELSE                                                         EL181
00264         MOVE ER-0503             TO EMI-ERROR                     EL181
00265         MOVE -1                  TO PRTSEQL                       EL181
00266         MOVE AL-UNBON            TO PRTSEQA                       EL181
00267         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL181
00268                                                                   EL181
00269      IF ONDATEL NOT = ZEROS                                       EL181
00270         MOVE ONDATEI             TO DEEDIT-FIELD                  EL181
00271         PERFORM 8600-DEEDIT                                       EL181
00272         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY            EL181
00273         MOVE '4'                 TO DC-OPTION-CODE                EL181
00274         PERFORM 9700-DATE-LINK THRU 9700-EXIT                     EL181
00275         IF DATE-CONVERSION-ERROR                                  EL181
00276            MOVE ER-0314          TO EMI-ERROR                     EL181
00277            MOVE -1               TO ONDATEL                       EL181
00278            MOVE AL-UABON         TO ONDATEA                       EL181
00279            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL181
00280            MOVE LOW-VALUES       TO PI-ON-DATE                    EL181
00281         ELSE                                                      EL181
00282            MOVE AL-UANON         TO ONDATEA                       EL181
00283            MOVE DC-GREG-DATE-1-EDIT  TO ONDATEI                   EL181
00284            MOVE DC-BIN-DATE-1    TO PI-ON-DATE                    EL181
00285      ELSE                                                         EL181
00286         MOVE ER-1579             TO EMI-ERROR                        CL**4
00287         MOVE -1                  TO ONDATEL                          CL**4
00288         MOVE AL-UABON            TO ONDATEA                          CL**4
00289         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     CL**4
00290         MOVE LOW-VALUES          TO PI-ON-DATE.                   EL181
00291                                                                   EL181
00292      IF THDATEL NOT = ZEROS                                       EL181
00293         MOVE THDATEI             TO DEEDIT-FIELD                  EL181
00294         PERFORM 8600-DEEDIT                                       EL181
00295         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY            EL181
00296         MOVE '4'                 TO DC-OPTION-CODE                EL181
00297         PERFORM 9700-DATE-LINK  THRU  9700-EXIT                   EL181
00298         IF DATE-CONVERSION-ERROR                                  EL181
00299            MOVE ER-0314          TO EMI-ERROR                     EL181
00300            MOVE -1               TO THDATEL                       EL181
00301            MOVE AL-UABON         TO THDATEA                       EL181
00302            PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT             EL181
00303            MOVE LOW-VALUES       TO PI-THRU-DATE                  EL181
00304         ELSE                                                      EL181
00305            MOVE DC-GREG-DATE-1-EDIT  TO THDATEI                   EL181
00306            MOVE AL-UANON         TO THDATEA                       EL181
00307            MOVE DC-BIN-DATE-1    TO PI-THRU-DATE                  EL181
00308      ELSE                                                         EL181
00309         MOVE ER-1579             TO EMI-ERROR                        CL**4
00310         MOVE -1                  TO THDATEL                          CL**4
00311         MOVE AL-UABON            TO THDATEA                          CL**4
00312         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     CL**4
00313         MOVE LOW-VALUES          TO PI-THRU-DATE.                 EL181
00314                                                                   EL181
00315      MOVE SAVE-BIN-DATE          TO CURRENT-SAVE.                 EL181
00316                                                                   EL181
00317      IF PI-ON-DATE NOT = LOW-VALUES                               EL181
00318         IF PI-ON-DATE  GREATER THAN CURRENT-SAVE                  EL181
00319            MOVE ER-0504          TO EMI-ERROR                     EL181
00320            MOVE -1               TO ONDATEL                       EL181
00321            MOVE AL-UABON         TO ONDATEA                       EL181
00322            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL181
00323                                                                   EL181
00324      IF PI-THRU-DATE NOT = LOW-VALUES                             EL181
00325         IF PI-THRU-DATE  GREATER THAN CURRENT-SAVE                EL181
00326            MOVE ER-0505          TO EMI-ERROR                     EL181
00327            MOVE -1               TO THDATEL                       EL181
00328            MOVE AL-UABON         TO THDATEA                       EL181
00329            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL181
00330                                                                   EL181
00331      IF PI-ON-DATE = LOW-VALUES AND PI-THRU-DATE NOT = LOW-VALUES EL181
00332            MOVE ER-0506          TO EMI-ERROR                     EL181
00333            MOVE -1               TO ONDATEL                       EL181
00334            MOVE AL-UABON         TO ONDATEA                       EL181
00335            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL181
00336                                                                   EL181
00337      IF PI-ON-DATE NOT = LOW-VALUES  AND                          EL181
00338             PI-THRU-DATE NOT = LOW-VALUES                         EL181
00339         IF PI-ON-DATE  GREATER THAN PI-THRU-DATE                  EL181
00340            MOVE ER-0507          TO EMI-ERROR                     EL181
00341            MOVE -1               TO ONDATEL                       EL181
00342            MOVE AL-UABON         TO ONDATEA                       EL181
00343            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL181
00344                                                                   EL181
00345      IF (NOT PI-NO-CARRIER-SECURITY OR                            EL181
00346          NOT PI-NO-ACCOUNT-SECURITY)                              EL181
00347         IF PRINTERL NOT GREATER THAN 0                            EL181
00348            MOVE ER-2379 TO EMI-ERROR                              EL181
00349            MOVE -1               TO PRINTERL                      EL181
00350            MOVE AL-UABON         TO PRINTERA                      EL181
00351            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL181
00352                                                                   EL181
00353      IF NOT EMI-NO-ERRORS                                         EL181
00354         GO TO 8200-SEND-DATAONLY.                                 EL181
00355                                                                   EL181
00356      EJECT                                                        EL181
00357  7800-START-PRINT.                                                EL181
00358      MOVE PI-COMPANY-ID          TO CNTL-CO-ID.                   EL181
00359                                                                   EL181
00360      EXEC CICS HANDLE CONDITION                                   EL181
00361           NOTOPEN     (8840-CNTL-NOT-OPEN)                        EL181
00362           NOTFND      (7890-NOT-FOUND)                            EL181
00363           TERMIDERR   (8820-TERMID-ERROR)                         EL181
00364           TRANSIDERR  (8830-TRANS-ERROR)                          EL181
00365           END-EXEC.                                               EL181
00366                                                                   EL181
00367      MOVE SPACES                 TO PI-ALT-DMD-PRT-ID.               CL**6
00368      IF PRINTERL NOT = ZEROS                                      EL181
00369         MOVE PRINTERI            TO WS-PRINTER-ID                 EL181
00370                                     PI-ALT-DMD-PRT-ID                CL**6
00371         GO TO 7820-START.                                         EL181
00372                                                                      CL**2
00373      IF PI-PROCESSOR-PRINTER IS NOT EQUAL TO SPACES                  CL**2
00374          MOVE PI-PROCESSOR-PRINTER TO WS-PRINTER-ID                  CL**3
00375          GO TO 7820-START.                                           CL**2
00376                                                                   EL181
00377      MOVE '1'                    TO CNTL-RECORD-TYPE.             EL181
00378      MOVE SPACES                 TO CNTL-GENL.                    EL181
00379      MOVE ZEROS                  TO CNTL-SEQ.                     EL181
00380                                                                   EL181
00381      EXEC CICS READ                                               EL181
00382           DATASET  (CNTL-ID)                                      EL181
00383           SET      (ADDRESS OF CONTROL-FILE)                         CL**5
00384           RIDFLD   (CNTL-KEY)                                     EL181
00385           END-EXEC.                                               EL181
00386                                                                   EL181
00387      CONTINUE.                                                       CL**5
00388      MOVE CF-FORMS-PRINTER-ID    TO WS-PRINTER-ID.                EL181
00389                                                                   EL181
00390  7820-START.                                                      EL181
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**6
00392 *        MOVE EIBTRMID       TO WS-PRINTER-ID                        CL**6
00393          EXEC CICS START                                             CL**6
00394               INTERVAL  (0)                                          CL**6
00395               TRANSID   (PRINT-TRANS)                                CL**6
00396               FROM      (PROGRAM-INTERFACE-BLOCK)                    CL**6
00397               LENGTH    (PI-COMM-LENGTH)                             CL**6
00398 *             TERMID    (WS-PRINTER-ID)                              CL**6
00399               END-EXEC                                               CL**6
00400      ELSE                                                            CL**6
00401          EXEC CICS START                                             CL**6
00402               INTERVAL  (0)                                          CL**6
00403               TRANSID   (PRINT-TRANS)                                CL**6
00404               FROM      (PROGRAM-INTERFACE-BLOCK)                    CL**6
00405               LENGTH    (PI-COMM-LENGTH)                             CL**6
00406               TERMID    (WS-PRINTER-ID)                              CL**6
00407               END-EXEC.                                              CL**6
00408                                                                   EL181
00409      MOVE ER-0411                TO EMI-ERROR.                    EL181
00410      MOVE -1                     TO PRTSEQL.                      EL181
00411      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL181
00412      GO TO 8200-SEND-DATAONLY.                                    EL181
00413                                                                   EL181
00414  7890-NOT-FOUND.                                                  EL181
00415      MOVE ER-0190                TO EMI-ERROR                     EL181
00416      MOVE -1                     TO PRTSEQL                       EL181
00417      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT                    EL181
00418      GO TO 8200-SEND-DATAONLY.                                    EL181
00419      EJECT                                                        EL181
00420  8100-SEND-INITIAL-MAP.                                           EL181
00421      MOVE SAVE-DATE              TO DATEAO.                       EL181
00422      MOVE EIBTIME                TO TIME-IN.                      EL181
00423      MOVE TIME-OUT               TO TIMEAO.                       EL181
00424      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL181
00425      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                     EL181
00426      MOVE -1                     TO PRTSEQL.                      EL181
00427                                                                   EL181
00428      EXEC CICS SEND                                               EL181
00429          MAP     (MAP-NAME)                                       EL181
00430          MAPSET  (MAPSET-NAME)                                    EL181
00431          FROM    (EL181AO)                                        EL181
00432          ERASE                                                    EL181
00433          CURSOR                                                   EL181
00434          END-EXEC.                                                EL181
00435                                                                   EL181
00436      GO TO 9100-RETURN-TRAN.                                      EL181
00437                                                                   EL181
00438  8200-SEND-DATAONLY.                                              EL181
00439      MOVE SAVE-DATE              TO DATEAO.                       EL181
00440      MOVE EIBTIME                TO TIME-IN.                      EL181
00441      MOVE TIME-OUT               TO TIMEAO.                       EL181
00442      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL181
00443      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                     EL181
00444                                                                   EL181
00445      EXEC CICS SEND                                               EL181
00446          MAP     (MAP-NAME)                                       EL181
00447          MAPSET  (MAPSET-NAME)                                    EL181
00448          FROM    (EL181AO)                                        EL181
00449          DATAONLY                                                 EL181
00450          ERASEAUP                                                 EL181
00451          CURSOR                                                   EL181
00452          END-EXEC.                                                EL181
00453                                                                   EL181
00454      GO TO 9100-RETURN-TRAN.                                      EL181
00455                                                                   EL181
00456  8300-SEND-TEXT.                                                  EL181
00457      EXEC CICS SEND TEXT                                          EL181
00458          FROM    (LOGOFF-TEXT)                                    EL181
00459          LENGTH  (LOGOFF-LENGTH)                                  EL181
00460          ERASE                                                    EL181
00461          FREEKB                                                   EL181
00462          END-EXEC.                                                EL181
00463                                                                   EL181
00464      EXEC CICS RETURN                                             EL181
00465          END-EXEC.                                                EL181
00466                                                                   EL181
00467  8600-DEEDIT.                                                     EL181
00468      EXEC CICS BIF DEEDIT                                         EL181
00469           FIELD   (DEEDIT-FIELD)                                  EL181
00470           LENGTH  (15)                                            EL181
00471           END-EXEC.                                               EL181
00472      EJECT                                                        EL181
00473  8800-UNAUTHORIZED-ACCESS.                                        EL181
00474      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL181
00475      GO TO 8300-SEND-TEXT.                                        EL181
00476                                                                   EL181
00477  8810-PF23.                                                       EL181
00478      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL181
00479      MOVE XCTL-005               TO PGM-NAME.                     EL181
00480      GO TO 9300-XCTL.                                             EL181
00481                                                                   EL181
00482  8820-TERMID-ERROR.                                               EL181
00483      MOVE ER-0412                TO EMI-ERROR                     EL181
00484      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL181
00485      MOVE -1                     TO PRTSEQL.                      EL181
00486      GO TO 8200-SEND-DATAONLY.                                    EL181
00487                                                                   EL181
00488  8830-TRANS-ERROR.                                                EL181
00489      MOVE ER-0413                TO EMI-ERROR                     EL181
00490      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL181
00491      MOVE -1                     TO PRTSEQL.                      EL181
00492      GO TO 8200-SEND-DATAONLY.                                    EL181
00493                                                                   EL181
00494  8840-CNTL-NOT-OPEN.                                              EL181
00495      MOVE ER-0042                TO EMI-ERROR.                    EL181
00496      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL181
00497      MOVE -1                     TO PRTSEQL.                      EL181
00498      GO TO 8200-SEND-DATAONLY.                                    EL181
00499                                                                   EL181
00500  9100-RETURN-TRAN.                                                EL181
00501      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL181
00502      MOVE MAP-NUMBER             TO PI-CURRENT-SCREEN-NO.         EL181
00503                                                                   EL181
00504      EXEC CICS RETURN                                             EL181
00505          TRANSID   (TRANS-ID)                                     EL181
00506          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL181
00507          LENGTH    (PI-COMM-LENGTH)                               EL181
00508          END-EXEC.                                                EL181
00509                                                                   EL181
00510  9200-RETURN-MAIN-MENU.                                           EL181
00511      MOVE XCTL-126               TO PGM-NAME.                     EL181
00512      GO TO 9300-XCTL.                                             EL181
00513                                                                   EL181
00514  9300-XCTL.                                                       EL181
00515      EXEC CICS XCTL                                               EL181
00516          PROGRAM   (PGM-NAME)                                     EL181
00517          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL181
00518          LENGTH    (PI-COMM-LENGTH)                               EL181
00519          END-EXEC.                                                EL181
00520                                                                   EL181
00521  9400-CLEAR.                                                      EL181
00522      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     EL181
00523      GO TO 9300-XCTL.                                             EL181
00524                                                                   EL181
00525  9500-PF12.                                                       EL181
00526      MOVE XCTL-010               TO PGM-NAME.                     EL181
00527      GO TO 9300-XCTL.                                             EL181
00528                                                                   EL181
00529  9600-PGMID-ERROR.                                                EL181
00530      EXEC CICS HANDLE CONDITION                                   EL181
00531          PGMIDERR  (8300-SEND-TEXT)                               EL181
00532          END-EXEC.                                                EL181
00533                                                                   EL181
00534      MOVE ' '                    TO PI-ENTRY-CD-1.                EL181
00535      MOVE XCTL-005               TO PGM-NAME.                     EL181
00536      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL181
00537      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL181
00538      GO TO 9300-XCTL.                                             EL181
00539                                                                   EL181
00540  9700-DATE-LINK.                                                  EL181
00541      MOVE LINK-ELDATCV           TO PGM-NAME                      EL181
00542                                                                   EL181
00543      EXEC CICS LINK                                               EL181
00544          PROGRAM   (PGM-NAME)                                     EL181
00545          COMMAREA  (DATE-CONVERSION-DATA)                         EL181
00546          LENGTH    (DC-COMM-LENGTH)                               EL181
00547          END-EXEC.                                                EL181
00548                                                                   EL181
00549  9700-EXIT.                                                       EL181
00550       EXIT.                                                       EL181
00551                                                                   EL181
00552  9900-ERROR-FORMAT.                                               EL181
00553      IF NOT EMI-ERRORS-COMPLETE                                   EL181
00554          MOVE LINK-001           TO PGM-NAME                      EL181
00555          EXEC CICS LINK                                           EL181
00556              PROGRAM   (PGM-NAME)                                 EL181
00557              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL181
00558              LENGTH    (EMI-COMM-LENGTH)                          EL181
00559              END-EXEC.                                            EL181
00560                                                                   EL181
00561  9900-EXIT.                                                       EL181
00562      EXIT.                                                        EL181
00563                                                                   EL181
00564  9990-ABEND.                                                      EL181
00565      MOVE LINK-004               TO PGM-NAME.                     EL181
00566      MOVE DFHEIBLK               TO EMI-LINE1.                    EL181
00567                                                                   EL181
00568      EXEC CICS LINK                                               EL181
00569          PROGRAM   (PGM-NAME)                                     EL181
00570          COMMAREA  (EMI-LINE1)                                    EL181
00571          LENGTH    (72)                                           EL181
00572          END-EXEC.                                                EL181
00573                                                                   EL181
00574      GO TO 8200-SEND-DATAONLY.                                    EL181
00575                                                                   EL181
00576  9995-SECURITY-VIOLATION.                                         EL181
00577                              COPY ELCSCTP.                        EL181
00578                                                                   EL181
