00001  IDENTIFICATION DIVISION.                                         03/06/96
00002                                                                   EL658
00003  PROGRAM-ID.                 EL658 .                                 LV005
00004 *              PROGRAM CONVERTED BY                                  CL**5
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**5
00006 *              CONVERSION DATE 02/14/96 11:58:19.                    CL**5
00007 *                            VMOD=2.005                              CL**5
00008 *                                                                 EL658
00009 *AUTHOR.     LOGIC,INC.                                              CL**5
00010 *            DALLAS, TEXAS.                                          CL**5
00011                                                                   EL658
00012 *DATE-COMPILED.                                                      CL**5
00013 *SECURITY.   *****************************************************   CL**5
00014 *            *                                                   *   CL**5
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**5
00016 *            *                                                   *   CL**5
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**5
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**5
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**5
00020 *            *                                                   *   CL**5
00021 *            *****************************************************   CL**5
00022                                                                   EL658
00023 *REMARKS.    TRANSACTION - EXJ3 - DISPLAY GENERAL AGENT              CL**5
00024 *                                 CROSS REFERENCE RECORDS.           CL**5
00025                                                                   EL658
00026                                                                   EL658
00027  ENVIRONMENT DIVISION.                                            EL658
00028      EJECT                                                        EL658
00029  DATA DIVISION.                                                   EL658
00030  WORKING-STORAGE SECTION.                                         EL658
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL658
00032  77  FILLER  PIC X(32)  VALUE '*    EL658 WORKING STORAGE     *'. EL658
00033  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.005 ************'.    CL**5
00034                                                                   EL658
00035  77  NDX     PIC S9(5)  COMP-3 VALUE +1.                          EL658
00036                                                                   EL658
00037  01  WS-DATE-AREA.                                                EL658
00038      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL658
00039      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL658
00040                                                                   EL658
00041  01  WS-CONTROL-PRIMARY.                                          EL658
00042      05  WS-COMPANY-CD       PIC X.                               EL658
00043      05  WS-CARRIER          PIC X.                               EL658
00044      05  WS-GROUPING         PIC X(6).                            EL658
00045      05  WS-AGENT            PIC X(10).                           EL658
00046                                                                   EL658
00047      05  WS-OPEN-COUNT       PIC S9(4)    COMP-3 VALUE ZEROS.        CL**5
00048                                                                   EL658
00049                              COPY ELCATTR SUPPRESS.                  CL**5
00050                                                                      CL**3
00051      EJECT                                                        EL658
00052                                                                   EL658
00053  01  ERROR-MESSAGES.                                              EL658
00054      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL658
00055      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL658
00056      12  ER-0033                 PIC X(4)  VALUE '0033'.          EL658
00057      12  ER-0034                 PIC X(4)  VALUE '0034'.          EL658
00058      12  ER-0130                 PIC X(4)  VALUE '0130'.          EL658
00059      12  ER-0131                 PIC X(4)  VALUE '0131'.          EL658
00060      12  ER-0142                 PIC X(4)  VALUE '0142'.          EL658
00061      12  ER-0234                 PIC X(4)  VALUE '0234'.          EL658
00062      12  ER-0235                 PIC X(4)  VALUE '0235'.          EL658
CIDMOD     12  ER-1162                 PIC X(4)  VALUE '1162'.             CL**4
00063      12  ER-1164                 PIC X(4)  VALUE '1164'.             CL**4
00064      12  ER-2237                 PIC X(4)  VALUE '2237'.          EL658
00065      12  ER-2238                 PIC X(4)  VALUE '2238'.          EL658
00066      12  ER-5004                 PIC X(4)  VALUE '5004'.          EL658
00067      12  ER-5005                 PIC X(4)  VALUE '5005'.          EL658
00068      12  ER-6508                 PIC X(4)  VALUE '6508'.          EL658
00069                                                                   EL658
00070      EJECT                                                        EL658
00071                                                                   EL658
00072  01  STANDARD-AREAS.                                              EL658
00073      12  QID.                                                     EL658
00074          16  QID-TERM        PIC X(4).                            EL658
00075          16  FILLER          PIC X(4)    VALUE '658A'.            EL658
00076      12  RETURNED-FROM       PIC X(8)    VALUE SPACES.            EL658
00077      12  QID-MAP-LENGTH      PIC S9(4)   VALUE +1376   COMP.      EL658
00078      12  MAP-NAME                PIC X(8)    VALUE 'EL658A'.      EL658
00079      12  MAPSET-NAME             PIC X(8)    VALUE 'EL658S'.      EL658
00080      12  TRANS-ID                PIC X(4)    VALUE 'EXJ3'.        EL658
00081      12  EL642-TRANS-ID          PIC X(4)    VALUE 'EXH7'.        EL658
00082      12  EL650-TRANS-ID          PIC X(4)    VALUE 'EXC4'.        EL658
00083      12  PGM-NAME                PIC X(8)    VALUE SPACES.        EL658
00084      12  THIS-PGM                PIC X(8)    VALUE 'EL658'.       EL658
00085      12  XCTL-642                PIC X(8)    VALUE 'EL642'.       EL658
00086      12  XCTL-650                PIC X(8)    VALUE 'EL650'.       EL658
00087      12  GXRF-FILE-ID            PIC X(8)    VALUE 'ERGXRF'.      EL658
00088      12  CNTL-FILE-ID            PIC X(8)    VALUE 'ELCNTL'.      EL658
00089      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     EL658
00090      12  LINK-001                PIC X(8)    VALUE 'EL001'.       EL658
00091      12  LINK-004                PIC X(8)    VALUE 'EL004'.       EL658
00092      12  XCTL-005                PIC X(8)    VALUE 'EL005'.       EL658
00093      12  XCTL-010                PIC X(8)    VALUE 'EL010'.          CL**2
00094      12  XCTL-626                PIC X(8)    VALUE 'EL626'.       EL658
00095      12  TIME-IN                 PIC S9(7).                       EL658
00096      12  TIME-OUT-R   REDEFINES TIME-IN.                          EL658
00097          16  FILLER              PIC X.                           EL658
00098          16  TIME-OUT            PIC 99V99.                       EL658
00099          16  FILLER              PIC X(2).                        EL658
00100      12  BROWSE-STARTED-SW       PIC X       VALUE ' '.           EL658
00101          88  BROWSE-STARTED      VALUE 'Y'.                       EL658
00102                                                                   EL658
00103  01  WS-RECORD-LENGTH COMP       SYNCHRONIZED.                    EL658
00104      12  GXRF-MAX-REC-LENGTH     PIC S9(8)    VALUE +32062.       EL658
00105      12  GXRF-REC-LENGTH         PIC S9(4)    VALUE +0.           EL658
00106                                                                   EL658
00107                                                                   EL658
00108  01  BLD-LINE.                                                    EL658
00109      12  BL-ACCOUNT          PIC X(10).                           EL658
00110      12  FILLER              PIC X         VALUE SPACE.           EL658
00111      12  BL-EFFECT           PIC X(8).                            EL658
00112      12  FILLER              PIC X(3)      VALUE SPACE.           EL658
00113      12  BL-EXPIRE           PIC X(8).                            EL658
00114      12  FILLER              PIC X(3)      VALUE SPACE.           EL658
00115      12  BL-LEVEL            PIC ZZ.                              EL658
00116      12  FILLER              PIC X(5)      VALUE SPACE.           EL658
00117      12  BL-LAST-BILL        PIC X(8).                            EL658
00118      12  FILLER              PIC X(6)      VALUE SPACE.           EL658
00119      12  BL-OCCURRENCE       PIC ZZ,ZZZ.                          EL658
00120      12  FILLER              PIC X(6)      VALUE SPACE.           EL658
00121      12  BL-STATE            PIC XX.                              EL658
00122                                                                   EL658
00123      EJECT                                                        EL658
00124                              COPY ELCDATE.                           CL**3
00125      EJECT                                                        EL658
00126                                     COPY ELCEMIB SUPPRESS.           CL**5
00127                                                                   EL658
00128                    COPY ELCLOGOF SUPPRESS.                           CL**5
00129                                                                      CL**3
00130                                      COPY ELCINTF.                   CL**3
00131      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.                EL658
00132          16  PI-ERGXRF-KEY.                                       EL658
00133              20  PI-GXRF-COMP-CD     PIC X.                       EL658
00134              20  PI-GXRF-CARRIER     PIC X.                       EL658
00135              20  PI-GXRF-GROUPING    PIC X(6).                    EL658
00136              20  PI-GXRF-AGENT       PIC X(10).                   EL658
00137          16  PI-NDX                  PIC S9(5)  COMP-3.           EL658
00138          16  PI-EOF-SW               PIC X.                       EL658
00139              88  PI-FILE-EOF             VALUE 'Y'.               EL658
00140          16  FILLER                  PIC X(618).                     CL**5
00141      EJECT                                                        EL658
00142                              COPY ELCAID SUPPRESS.                   CL**5
00143  01  FILLER    REDEFINES DFHAID.                                  EL658
00144      12  FILLER              PIC X(8).                            EL658
00145      12  PF-VALUES           PIC X       OCCURS 24.               EL658
00146                                                                   EL658
00147                              COPY EL658S.                            CL**3
00148      EJECT                                                        EL658
00149                                                                   EL658
00150                              COPY ERCGXRF.                           CL**3
00151      EJECT                                                        EL658
00152                                                                   EL658
00153  LINKAGE SECTION.                                                 EL658
00154  01  DFHCOMMAREA             PIC X(1024).                         EL658
00155      EJECT                                                        EL658
00156  PROCEDURE DIVISION.                                              EL658
00157      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                 EL658
00158                                                                   EL658
00159      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL658
00160      MOVE '5'                   TO DC-OPTION-CODE.                EL658
00161      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL658
00162      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL658
00163      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL658
00164                                                                      CL**5
00165      MOVE EIBTRMID              TO QID-TERM.                         CL**5
00166                                                                   EL658
00167      EXEC CICS HANDLE CONDITION                                   EL658
00168          ERROR     (9990-ABEND)                                   EL658
00169          MAPFAIL   (8100-SEND-INITIAL-MAP)                        EL658
00170          END-EXEC.                                                EL658
00171                                                                   EL658
00172      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL658
00173          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL658
00174              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL658
00175              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL658
00176              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL658
00177              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL658
00178              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL658
00179              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL658
00180              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL658
00181              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL658
00182          ELSE                                                     EL658
00183              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL658
00184              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL658
00185              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL658
00186              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL658
00187              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL658
00188              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL658
00189              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL658
00190              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL658
00191                                                                   EL658
00192      EJECT                                                        EL658
00193      IF EIBTRNID = TRANS-ID                                       EL658
00194          IF EIBAID = DFHCLEAR                                     EL658
00195              GO TO 9400-CLEAR                                     EL658
00196          ELSE                                                     EL658
00197              GO TO 0200-RECEIVE-MAP.                              EL658
00198                                                                   EL658
00199      IF EIBTRNID  = EL642-TRANS-ID                                EL658
00200          MOVE DFHENTER       TO EIBAID                            EL658
00201          MOVE PI-CR-CARRIER  TO CARRIERI                          EL658
00202          MOVE PI-CR-GROUPING TO GROUPI                            EL658
00203          MOVE PI-CR-FIN-RESP TO AGENTI                            EL658
00204          MOVE 1              TO CARRIERL                          EL658
00205          MOVE 6              TO GROUPL                            EL658
00206          MOVE 10             TO AGENTL                            EL658
00207          MOVE AL-UANON       TO CARRIERA GROUPA AGENTA            EL658
00208          GO TO 1000-EDIT-INPUT.                                   EL658
00209                                                                   EL658
00210      MOVE LOW-VALUES       TO PI-ERGXRF-KEY    EL658AI.           EL658
00211      MOVE PI-COMPANY-CD    TO PI-GXRF-COMP-CD  WS-COMPANY-CD.     EL658
00212                                                                   EL658
00213      IF EIBTRNID  = EL650-TRANS-ID                                EL658
00214          GO TO 0600-RECOVER-TEMP-STORAGE.                         EL658
00215                                                                   EL658
00216      GO TO 8100-SEND-INITIAL-MAP.                                 EL658
00217                                                                   EL658
00218      EJECT                                                        EL658
00219                                                                   EL658
00220  0200-RECEIVE-MAP.                                                EL658
00221      MOVE LOW-VALUES TO EL658AI.                                  EL658
00222                                                                   EL658
00223      EXEC CICS RECEIVE                                            EL658
00224          MAP (MAP-NAME)                                           EL658
00225          MAPSET (MAPSET-NAME)                                     EL658
00226          INTO (EL658AI)                                           EL658
00227          END-EXEC.                                                EL658
00228                                                                   EL658
00229      IF PFENTERL = 0                                              EL658
00230          GO TO 0300-CHECK-PFKEYS.                                 EL658
00231      IF EIBAID NOT = DFHENTER                                     EL658
00232          MOVE ER-0004 TO EMI-ERROR                                EL658
00233          GO TO 0320-INPUT-ERROR.                                  EL658
00234      IF (PFENTERI NUMERIC) AND (PFENTERI > 0 AND < 25)            EL658
00235          MOVE PF-VALUES (PFENTERI) TO EIBAID                      EL658
00236      ELSE                                                         EL658
00237          MOVE ER-0029 TO EMI-ERROR                                EL658
00238          GO TO 0320-INPUT-ERROR.                                  EL658
00239                                                                   EL658
00240  0300-CHECK-PFKEYS.                                               EL658
00241      IF EIBAID = DFHPF1 OR DFHPF2                                 EL658
00242          GO TO 5000-BROWSE-FILE.                                  EL658
00243                                                                   EL658
00244      IF EIBAID = DFHPF3 OR DFHPF4                                 EL658
00245          GO TO 6000-OCCURRENCE.                                   EL658
00246                                                                   EL658
00247      IF EIBAID = DFHPF5                                           EL658
00248          GO TO 4000-ACCT-MAINT.                                   EL658
00249                                                                   EL658
00250      IF EIBAID = DFHENTER                                         EL658
00251          GO TO 1000-EDIT-INPUT.                                   EL658
00252                                                                   EL658
00253      IF EIBAID = DFHPF12                                             CL**2
00254          GO TO 9500-PF12.                                            CL**2
00255                                                                      CL**2
00256      IF EIBAID = DFHPF23                                          EL658
00257          GO TO 8810-PF23.                                         EL658
00258                                                                   EL658
00259      IF EIBAID = DFHPF24                                          EL658
00260          GO TO 9200-RETURN-MAIN-MENU.                             EL658
00261                                                                   EL658
00262      MOVE ER-0029                TO EMI-ERROR.                    EL658
00263  0320-INPUT-ERROR.                                                EL658
00264      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL658
00265      MOVE -1                     TO CARRIERL.                     EL658
00266      GO TO 8200-SEND-DATAONLY.                                    EL658
00267      EJECT                                                        EL658
00268  0500-CREATE-TEMP-STORAGE.                                        EL658
00269                                                                   EL658
00270      EXEC CICS WRITEQ TS                                          EL658
00271          QUEUE   (QID)                                            EL658
00272          FROM    (EL658AI)                                        EL658
00273          LENGTH  (QID-MAP-LENGTH)                                 EL658
00274          END-EXEC.                                                EL658
00275                                                                   EL658
00276  0599-EXIT.                                                       EL658
00277       EXIT.                                                       EL658
00278                                                                   EL658
00279  0600-RECOVER-TEMP-STORAGE.                                       EL658
00280                                                                   EL658
00281      EXEC CICS HANDLE CONDITION                                   EL658
00282          NOTFND  (1500-GXRF-NOT-FOUND)                            EL658
00283          QIDERR  (0690-QIDERR)                                    EL658
00284          END-EXEC.                                                EL658
00285                                                                   EL658
00286      EXEC CICS READQ TS                                           EL658
00287          QUEUE    (QID)                                           EL658
00288          INTO     (EL658AI)                                       EL658
00289          LENGTH   (QID-MAP-LENGTH)                                EL658
00290          END-EXEC.                                                EL658
00291                                                                   EL658
00292      EXEC CICS DELETEQ TS                                         EL658
00293          QUEUE   (QID)                                            EL658
00294          END-EXEC.                                                EL658
00295                                                                   EL658
00296      IF CARRIERL NOT = 0                                          EL658
00297          MOVE AL-UANON TO CARRIERA.                               EL658
00298                                                                   EL658
00299      IF GROUPL NOT = 0                                            EL658
00300          MOVE AL-UANON TO GROUPA.                                 EL658
00301                                                                   EL658
00302      IF AGENTL NOT = 0                                            EL658
00303          MOVE AL-UANON TO AGENTA.                                 EL658
00304                                                                   EL658
00305      MOVE PI-COMPANY-CD          TO WS-COMPANY-CD.                EL658
00306      MOVE CARRIERO               TO WS-CARRIER.                   EL658
00307      MOVE GROUPO                 TO WS-GROUPING.                  EL658
00308      MOVE AGENTO                 TO WS-AGENT.                     EL658
00309                                                                   EL658
00310      GO TO 1050-READ-GA.                                          EL658
00311                                                                   EL658
00312  0690-QIDERR.                                                     EL658
00313      MOVE ER-0033 TO EMI-ERROR.                                   EL658
00314      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL658
00315      GO TO 8100-SEND-INITIAL-MAP.                                 EL658
00316                                                                   EL658
00317      EJECT                                                        EL658
00318                                                                   EL658
00319  1000-EDIT-INPUT.                                                 EL658
00320      IF CARRIERL NOT = ZEROS                                      EL658
00321          MOVE CARRIERI           TO WS-CARRIER                    EL658
00322        ELSE                                                       EL658
00323          MOVE ER-0234            TO EMI-ERROR                     EL658
00324          MOVE -1                 TO CARRIERL                      EL658
00325          MOVE AL-UABON           TO CARRIERA                      EL658
00326          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL658
00327                                                                   EL658
00328      IF GROUPL NOT = ZEROS                                        EL658
00329          MOVE GROUPI             TO WS-GROUPING                   EL658
00330        ELSE                                                       EL658
00331          MOVE ER-0235            TO EMI-ERROR                     EL658
00332          MOVE -1                 TO GROUPL                        EL658
00333          MOVE AL-UABON           TO GROUPA                        EL658
00334          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL658
00335                                                                   EL658
00336      IF AGENTL NOT = ZEROS                                        EL658
00337          MOVE AGENTI             TO WS-AGENT                      EL658
00338        ELSE                                                       EL658
00339          MOVE ER-6508            TO EMI-ERROR                     EL658
00340          MOVE -1                 TO AGENTL                        EL658
00341          MOVE AL-UABON           TO AGENTA                        EL658
00342          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL658
00343                                                                   EL658
00344      IF NOT EMI-NO-ERRORS                                         EL658
00345          GO TO 8200-SEND-DATAONLY.                                EL658
00346                                                                   EL658
00347      MOVE PI-COMPANY-CD   TO WS-COMPANY-CD.                       EL658
00348                                                                   EL658
00349      EJECT                                                        EL658
00350  1050-READ-GA.                                                    EL658
00351      EXEC CICS HANDLE CONDITION                                   EL658
00352          NOTFND (1500-GXRF-NOT-FOUND) END-EXEC.                   EL658
00353                                                                   EL658
00354      MOVE GXRF-MAX-REC-LENGTH    TO GXRF-REC-LENGTH.              EL658
00355                                                                   EL658
00356      EXEC CICS READ                                               EL658
00357           INTO    (AGENT-CROSS-REFERENCE)                         EL658
00358           DATASET ('ERGXRF')                                      EL658
00359           LENGTH  (GXRF-REC-LENGTH)                               EL658
00360           RIDFLD  (WS-CONTROL-PRIMARY)                            EL658
00361           END-EXEC.                                               EL658
00362                                                                   EL658
00363                                                                   EL658
00364      MOVE GX-AGENT-POINTER-CNT TO GX-AGENT-POINTER-CNT.           EL658
00365                                                                   EL658
00366      MOVE +1 TO PI-NDX.                                           EL658
00367      GO TO 5200-FORMAT-SCREEN.                                    EL658
00368                                                                   EL658
00369  1500-GXRF-NOT-FOUND.                                             EL658
00370      MOVE ER-0142            TO EMI-ERROR.                        EL658
00371      MOVE -1                 TO CARRIERL.                         EL658
00372      MOVE AL-UABON           TO CARRIERA.                         EL658
00373      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL658
00374      IF EIBTRNID  = EL642-TRANS-ID                                   CL**4
00375          GO TO 8100-SEND-INITIAL-MAP                                 CL**4
00376      ELSE                                                            CL**4
00377          GO TO 8200-SEND-DATAONLY.                                   CL**4
00378                                                                   EL658
00379      EJECT                                                        EL658
00380                                                                   EL658
00381  3000-BLD-LINE.                                                   EL658
00382      MOVE 1  TO NDX.                                              EL658
00383      MOVE SPACES TO XRFLINEO (1) XRFLINEO (2) XRFLINEO (3)        EL658
00384                     XRFLINEO (4) XRFLINEO (5) XRFLINEO (6)        EL658
00385                     XRFLINEO (7) XRFLINEO (8) XRFLINEO (9)        EL658
00386                     XRFLINEO (10) XRFLINEO (11) XRFLINEO (12)     EL658
00387                     XRFLINEO (13) XRFLINEO (14) XRFLINEO (15)     EL658
00388                                   BLD-LINE.                       EL658
00389  3000-BLD-LINE-LOOP.                                              EL658
00390      IF PI-NDX GREATER THAN GX-AGENT-POINTER-CNT                  EL658
00391           GO TO 3000-XIT.                                         EL658
00392                                                                   EL658
00393      MOVE GX-AM-ACCOUNT (PI-NDX) TO BL-ACCOUNT.                   EL658
00394                                                                   EL658
00395      IF GX-AM-EFF-DT (PI-NDX) NOT = SPACES AND LOW-VALUES         EL658
00396          MOVE GX-AM-EFF-DT (PI-NDX) TO DC-BIN-DATE-1              EL658
00397          MOVE SPACE                        TO DC-OPTION-CODE      EL658
00398          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                 EL658
00399          MOVE DC-GREG-DATE-1-EDIT       TO BL-EFFECT.             EL658
00400                                                                   EL658
00401      IF GX-AM-EXPIRATION-DT (PI-NDX) NOT = SPACES AND LOW-VALUES  EL658
00402          MOVE GX-AM-EXPIRATION-DT (PI-NDX) TO DC-BIN-DATE-1       EL658
00403          MOVE SPACE                        TO DC-OPTION-CODE      EL658
00404          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                 EL658
00405          MOVE DC-GREG-DATE-1-EDIT       TO BL-EXPIRE.             EL658
00406                                                                   EL658
00407      IF GX-AM-EXPIRATION-DT (PI-NDX) = HIGH-VALUES                EL658
00408          MOVE '99/99/99'                TO BL-EXPIRE.             EL658
00409                                                                   EL658
00410      MOVE GX-AM-LEVEL-NO (PI-NDX)       TO BL-LEVEL.              EL658
00411                                                                   EL658
00412      IF GX-LAST-BILL-DT (PI-NDX) NOT = SPACES AND LOW-VALUES      EL658
00413          MOVE GX-LAST-BILL-DT (PI-NDX) TO DC-BIN-DATE-1           EL658
00414          MOVE SPACE                 TO DC-OPTION-CODE             EL658
00415          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                 EL658
00416          MOVE DC-GREG-DATE-1-EDIT   TO BL-LAST-BILL.              EL658
00417                                                                   EL658
00418      MOVE PI-NDX                    TO BL-OCCURRENCE.             EL658
00419      MOVE GX-AM-STATE (PI-NDX)      TO BL-STATE.                  EL658
00420                                                                   EL658
00421      MOVE BLD-LINE                  TO XRFLINEO (NDX).            EL658
00422      MOVE SPACES                    TO BLD-LINE.                  EL658
00423                                                                   EL658
00424      IF (PI-NDX = GX-AGENT-POINTER-CNT)  OR  (NDX = 15)           EL658
00425           GO TO 3000-XIT.                                         EL658
00426      ADD  1 TO PI-NDX NDX.                                        EL658
00427      GO TO 3000-BLD-LINE-LOOP.                                    EL658
00428                                                                   EL658
00429  3000-XIT.                                                        EL658
00430      EXIT.                                                        EL658
00431      EJECT                                                        EL658
00432  4000-ACCT-MAINT.                                                 EL658
00433      IF CARRIERI NOT = PI-GXRF-CARRIER   OR                       EL658
00434         GROUPI   NOT = PI-GXRF-GROUPING  OR                       EL658
00435         AGENTI   NOT = PI-GXRF-AGENT                              EL658
00436            MOVE ER-5005 TO EMI-ERROR                              EL658
00437            GO TO 4050-ERR.                                        EL658
00438                                                                   EL658
00439      IF ACCOCCI NOT NUMERIC                                       EL658
00440         GO TO 4010-ERROR.                                         EL658
00441                                                                   EL658
00442      MOVE PI-COMPANY-CD      TO WS-COMPANY-CD.                    EL658
00443      MOVE CARRIERI           TO WS-CARRIER.                       EL658
00444      MOVE GROUPI             TO WS-GROUPING.                      EL658
00445      MOVE AGENTI             TO WS-AGENT.                         EL658
00446                                                                   EL658
00447      EXEC CICS HANDLE CONDITION                                   EL658
00448          NOTFND (4010-ERROR) END-EXEC.                            EL658
00449                                                                   EL658
00450                                                                   EL658
00451      MOVE GXRF-MAX-REC-LENGTH    TO GXRF-REC-LENGTH.              EL658
00452                                                                   EL658
00453      EXEC CICS READ                                               EL658
00454           DATASET ('ERGXRF')                                      EL658
00455           INTO    (AGENT-CROSS-REFERENCE)                         EL658
00456           LENGTH  (GXRF-REC-LENGTH)                               EL658
00457           RIDFLD  (WS-CONTROL-PRIMARY)                            EL658
00458           END-EXEC.                                               EL658
00459                                                                   EL658
00460      MOVE GX-AGENT-POINTER-CNT TO GX-AGENT-POINTER-CNT.           EL658
00461                                                                   EL658
00462      MOVE ACCOCCI TO NDX.                                         EL658
00463      IF NDX  NOT GREATER GX-AGENT-POINTER-CNT                     EL658
00464          MOVE GX-AM-CARRIER  (NDX)   TO PI-CR-CARRIER             EL658
00465          MOVE GX-AM-GROUPING (NDX)   TO PI-CR-GROUPING            EL658
00466          MOVE GX-AM-STATE    (NDX)   TO PI-CR-STATE               EL658
00467          MOVE GX-AM-ACCOUNT  (NDX)   TO PI-CR-ACCOUNT             EL658
00468          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT          EL658
00469          MOVE XCTL-650      TO PGM-NAME                           EL658
00470          GO TO 9300-XCTL.                                         EL658
00471                                                                   EL658
00472  4010-ERROR.                                                      EL658
00473       MOVE ER-5004 TO EMI-ERROR.                                  EL658
00474  4050-ERR.                                                        EL658
00475       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   EL658
00476       MOVE -1                     TO ACCOCCL.                     EL658
00477       MOVE AL-UNBON               TO ACCOCCA.                     EL658
00478       GO TO 8200-SEND-DATAONLY.                                   EL658
00479                                                                   EL658
00480      EJECT                                                        EL658
00481  5000-BROWSE-FILE.                                                EL658
00482      MOVE SPACE TO BROWSE-STARTED-SW.                             EL658
00483      EXEC CICS HANDLE CONDITION                                   EL658
00484          NOTFND (5800-NO-RECORD)                                  EL658
00485          ENDFILE (5900-END-OF-FILE)                               EL658
00486          END-EXEC.                                                EL658
00487                                                                   EL658
00488      MOVE PI-ERGXRF-KEY TO WS-CONTROL-PRIMARY.                    EL658
00489                                                                   EL658
00490      EXEC CICS STARTBR                                            EL658
00491          DATASET (GXRF-FILE-ID)                                   EL658
00492          RIDFLD (WS-CONTROL-PRIMARY)                              EL658
00493          END-EXEC.                                                EL658
00494                                                                   EL658
00495      MOVE 'Y' TO BROWSE-STARTED-SW.                               EL658
00496                                                                   EL658
00497      IF EIBAID = DFHPF2                                           EL658
00498          GO TO 5100-BROWSE-BKWD.                                  EL658
00499      EJECT                                                        EL658
00500  5010-READ-LOOP.                                                  EL658
00501                                                                   EL658
00502      MOVE GXRF-MAX-REC-LENGTH    TO GXRF-REC-LENGTH.              EL658
00503                                                                   EL658
00504      EXEC CICS READNEXT                                           EL658
00505          DATASET (GXRF-FILE-ID)                                   EL658
00506          INTO    (AGENT-CROSS-REFERENCE)                          EL658
00507          RIDFLD  (WS-CONTROL-PRIMARY)                             EL658
00508          LENGTH  (GXRF-REC-LENGTH)                                EL658
00509          END-EXEC.                                                EL658
00510                                                                   EL658
00511                                                                   EL658
00512      MOVE GX-AGENT-POINTER-CNT TO GX-AGENT-POINTER-CNT.           EL658
00513                                                                   EL658
00514      MOVE +1 TO PI-NDX.                                           EL658
00515                                                                   EL658
00516      IF GX-COMPANY-CD NOT = PI-COMPANY-CD                         EL658
00517          IF EIBAID = DFHENTER                                     EL658
00518              GO TO 5800-NO-RECORD                                 EL658
00519          ELSE                                                     EL658
00520              GO TO 5900-END-OF-FILE.                              EL658
00521                                                                   EL658
00522      IF EIBAID = DFHENTER                                         EL658
00523         IF WS-CARRIER    = GX-CARRIER     AND                     EL658
00524            WS-GROUPING   = GX-GROUPING    AND                     EL658
00525            WS-AGENT      = GX-AGENT-NO                            EL658
00526               GO TO 5200-FORMAT-SCREEN                            EL658
00527            ELSE                                                   EL658
00528               GO TO 5800-NO-RECORD.                               EL658
00529                                                                   EL658
00530      IF EIBAID = DFHPF1                                           EL658
00531        IF CARRIERO = GX-CARRIER  AND                              EL658
00532           GROUPO   = GX-GROUPING AND                              EL658
00533           AGENTO   = GX-AGENT-NO                                  EL658
00534          GO TO 5010-READ-LOOP.                                    EL658
00535                                                                   EL658
00536      GO TO 5200-FORMAT-SCREEN.                                    EL658
00537      EJECT                                                        EL658
00538  5100-BROWSE-BKWD.                                                EL658
00539      EXEC CICS HANDLE CONDITION                                   EL658
00540          NOTFND (5900-END-OF-FILE)                                EL658
00541          END-EXEC.                                                EL658
00542                                                                   EL658
00543      MOVE GXRF-MAX-REC-LENGTH    TO GXRF-REC-LENGTH.              EL658
00544                                                                   EL658
00545      EXEC CICS READPREV                                           EL658
00546          DATASET (GXRF-FILE-ID)                                   EL658
00547          INTO    (AGENT-CROSS-REFERENCE)                          EL658
00548          LENGTH  (GXRF-REC-LENGTH)                                EL658
00549          RIDFLD  (WS-CONTROL-PRIMARY)                             EL658
00550          END-EXEC.                                                EL658
00551                                                                   EL658
00552      MOVE GXRF-MAX-REC-LENGTH    TO GXRF-REC-LENGTH.              EL658
00553                                                                   EL658
00554      IF PI-FILE-EOF                                               EL658
00555          MOVE SPACE    TO PI-EOF-SW                               EL658
00556      ELSE                                                         EL658
00557          EXEC CICS READPREV                                       EL658
00558              DATASET (GXRF-FILE-ID)                               EL658
00559              INTO    (AGENT-CROSS-REFERENCE)                      EL658
00560              LENGTH  (GXRF-REC-LENGTH)                            EL658
00561              RIDFLD  (WS-CONTROL-PRIMARY)                         EL658
00562              END-EXEC.                                            EL658
00563                                                                   EL658
00564                                                                   EL658
00565      MOVE GX-AGENT-POINTER-CNT TO GX-AGENT-POINTER-CNT.           EL658
00566                                                                   EL658
00567      MOVE +1 TO PI-NDX.                                           EL658
00568                                                                   EL658
00569      IF GX-COMPANY-CD NOT = PI-COMPANY-CD                         EL658
00570          GO TO 5900-END-OF-FILE.                                  EL658
00571                                                                   EL658
00572      GO TO 5200-FORMAT-SCREEN.                                    EL658
00573                                                                   EL658
00574      EJECT                                                        EL658
00575  5200-FORMAT-SCREEN.                                              EL658
00576      MOVE LOW-VALUES             TO EL658AI.                      EL658
00577                                                                   EL658
00578      MOVE GX-CONTROL-PRIMARY     TO PI-ERGXRF-KEY.                EL658
00579                                                                   EL658
00580      MOVE GX-CARRIER             TO CARRIERO.                     EL658
00581                                                                   EL658
00582      MOVE GX-GROUPING            TO GROUPO.                       EL658
00583                                                                   EL658
00584      MOVE GX-AGENT-NO            TO AGENTO.                       EL658
00585                                                                   EL658
00586      MOVE GX-AGENT-POINTER-CNT   TO TOTOCCO.                      EL658
00587                                                                   EL658
00588      PERFORM 7500-COUNT-OPEN-RANGES  THRU  7500-EXIT.                CL**3
00589                                                                      CL**3
00590      MOVE WS-OPEN-COUNT          TO TOTOPNO.                         CL**3
00591                                                                      CL**3
00592      IF GX-LAST-MAINT-DT NOT = SPACES AND LOW-VALUES AND ZEROS    EL658
00593          MOVE GX-LAST-MAINT-DT          TO DC-BIN-DATE-1          EL658
00594          MOVE SPACE                     TO DC-OPTION-CODE         EL658
00595          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                 EL658
00596          MOVE DC-GREG-DATE-1-EDIT       TO MAINTO                 EL658
00597       ELSE                                                        EL658
00598          MOVE '00/00/00'                TO MAINTO.                EL658
00599                                                                   EL658
00600      PERFORM 3000-BLD-LINE THRU 3000-XIT.                         EL658
00601                                                                   EL658
00602      MOVE AL-UANON               TO CARRIERA GROUPA               EL658
00603                                     AGENTA.                       EL658
00604      GO TO 8100-SEND-INITIAL-MAP.                                 EL658
00605      EJECT                                                        EL658
00606  5800-NO-RECORD.                                                  EL658
00607      MOVE -1                     TO CARRIERL.                     EL658
00608      MOVE AL-UANON               TO CARRIERA   GROUPA             EL658
00609                                      AGENTA.                      EL658
00610      MOVE ER-1164                TO EMI-ERROR.                       CL**4
00611      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL658
00612      IF BROWSE-STARTED                                            EL658
00613        EXEC CICS ENDBR                                            EL658
00614             DATASET  (GXRF-FILE-ID)                               EL658
00615             END-EXEC.                                             EL658
00616                                                                   EL658
00617      GO TO 8200-SEND-DATAONLY.                                    EL658
00618                                                                   EL658
00619  5900-END-OF-FILE.                                                EL658
00620      IF EIBAID = DFHPF1                                           EL658
00621          MOVE 'Y'                TO PI-EOF-SW                     EL658
00622          MOVE ER-2237            TO EMI-ERROR                     EL658
00623      ELSE                                                         EL658
00624          MOVE LOW-VALUES         TO PI-ERGXRF-KEY                 EL658
00625          MOVE PI-COMPANY-CD      TO PI-GXRF-COMP-CD               EL658
00626          MOVE ER-2238            TO EMI-ERROR.                    EL658
00627                                                                   EL658
00628      MOVE -1            TO CARRIERL.                              EL658
00629      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL658
00630      IF BROWSE-STARTED                                            EL658
00631        EXEC CICS ENDBR                                            EL658
00632             DATASET  (GXRF-FILE-ID)                               EL658
00633             END-EXEC.                                             EL658
00634                                                                   EL658
00635      MOVE SPACE TO BROWSE-STARTED-SW.                             EL658
00636      GO TO 8200-SEND-DATAONLY.                                    EL658
00637      EJECT                                                        EL658
00638  6000-OCCURRENCE.                                                 EL658
00639      MOVE PI-ERGXRF-KEY TO WS-CONTROL-PRIMARY.                    EL658
00640                                                                   EL658
00641      EXEC CICS HANDLE CONDITION                                   EL658
00642          NOTFND (1500-GXRF-NOT-FOUND) END-EXEC.                   EL658
00643                                                                   EL658
00644      MOVE GXRF-MAX-REC-LENGTH    TO GXRF-REC-LENGTH.              EL658
00645                                                                   EL658
00646      EXEC CICS READ                                               EL658
00647           DATASET ('ERGXRF')                                      EL658
00648           INTO    (AGENT-CROSS-REFERENCE)                         EL658
00649           LENGTH  (GXRF-REC-LENGTH)                               EL658
00650           RIDFLD  (WS-CONTROL-PRIMARY)                            EL658
00651           END-EXEC.                                               EL658
00652                                                                   EL658
00653                                                                   EL658
00654      MOVE GX-AGENT-POINTER-CNT   TO GX-AGENT-POINTER-CNT.         EL658
00655                                                                   EL658
00656      IF EIBAID = DFHPF3                                           EL658
00657         GO TO 6500-NEXT-OCC                                       EL658
00658       ELSE                                                        EL658
00659         GO TO 7000-PRIOR-OCC.                                     EL658
00660                                                                   EL658
00661  6500-NEXT-OCC.                                                   EL658
00662      IF PI-NDX = GX-AGENT-POINTER-CNT                             EL658
00663          MOVE -1            TO CARRIERL                           EL658
00664          MOVE ER-0130       TO EMI-ERROR                          EL658
00665          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL658
00666          GO TO 8200-SEND-DATAONLY.                                EL658
00667                                                                   EL658
00668      ADD +1 TO PI-NDX.                                            EL658
00669                                                                   EL658
00670      PERFORM 3000-BLD-LINE THRU 3000-XIT.                         EL658
00671                                                                   EL658
00672      MOVE -1      TO CARRIERL.                                    EL658
00673      GO TO 8200-SEND-DATAONLY.                                    EL658
00674                                                                   EL658
00675  7000-PRIOR-OCC.                                                  EL658
00676      IF PI-NDX LESS 16                                            EL658
00677          MOVE -1            TO CARRIERL                           EL658
00678          MOVE ER-0131       TO EMI-ERROR                          EL658
00679          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL658
00680          GO TO 8200-SEND-DATAONLY.                                EL658
00681                                                                   EL658
00682      COMPUTE PI-NDX = (PI-NDX - 1) / 15.                          EL658
00683      IF PI-NDX NOT = 1                                            EL658
00684         COMPUTE PI-NDX = PI-NDX * 15                              EL658
00685         COMPUTE PI-NDX = PI-NDX - 14.                             EL658
00686                                                                   EL658
00687      PERFORM 3000-BLD-LINE THRU 3000-XIT.                         EL658
00688                                                                   EL658
00689      MOVE -1      TO CARRIERL.                                    EL658
00690      GO TO 8200-SEND-DATAONLY.                                    EL658
00691                                                                   EL658
00692      EJECT                                                        EL658
00693                                                                   EL658
00694  7500-COUNT-OPEN-RANGES.                                             CL**3
00695                                                                      CL**3
00696      MOVE +1                     TO PI-NDX.                          CL**3
00697      MOVE +0                     TO WS-OPEN-COUNT.                   CL**3
00698                                                                      CL**3
00699  7500-LOOP.                                                          CL**3
00700                                                                      CL**3
00701      IF PI-NDX GREATER THAN GX-AGENT-POINTER-CNT                     CL**3
00702           MOVE +1                TO PI-NDX                           CL**3
00703           GO TO 7500-EXIT.                                           CL**3
00704                                                                      CL**3
00705      IF GX-AM-EXPIRATION-DT (PI-NDX) = HIGH-VALUES                   CL**3
00706          ADD +1 TO WS-OPEN-COUNT.                                    CL**3
00707                                                                      CL**3
00708      ADD +1 TO PI-NDX.                                               CL**3
00709                                                                      CL**3
00710      GO TO 7500-LOOP.                                                CL**3
00711                                                                      CL**3
00712  7500-EXIT.                                                          CL**3
00713      EXIT.                                                           CL**3
00714                                                                      CL**3
00715      EJECT                                                           CL**3
00716  8100-SEND-INITIAL-MAP.                                           EL658
00717      MOVE SAVE-DATE              TO DATEO.                        EL658
00718      MOVE EIBTIME                TO TIME-IN.                      EL658
00719      MOVE TIME-OUT               TO TIMEO.                        EL658
00720      MOVE -1                     TO CARRIERL.                     EL658
00721      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                      EL658
00722                                                                   EL658
00723      EXEC CICS SEND                                               EL658
00724          MAP    (MAP-NAME)                                        EL658
00725          MAPSET (MAPSET-NAME)                                     EL658
00726          FROM   (EL658AO)                                         EL658
00727          ERASE                                                    EL658
00728          CURSOR                                                   EL658
00729          END-EXEC.                                                EL658
00730      GO TO 9100-RETURN-TRAN.                                      EL658
00731                                                                   EL658
00732  8200-SEND-DATAONLY.                                              EL658
00733      MOVE SAVE-DATE              TO DATEO.                        EL658
00734      MOVE EIBTIME                TO TIME-IN.                      EL658
00735      MOVE TIME-OUT               TO TIMEO.                        EL658
00736      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                      EL658
00737      EXEC CICS SEND                                               EL658
00738          MAP    (MAP-NAME)                                        EL658
00739          MAPSET (MAPSET-NAME)                                     EL658
00740          FROM   (EL658AO)                                         EL658
00741          DATAONLY                                                 EL658
00742          CURSOR                                                   EL658
00743          END-EXEC.                                                EL658
00744      GO TO 9100-RETURN-TRAN.                                      EL658
00745                                                                   EL658
00746      EJECT                                                        EL658
00747  8500-DATE-CONVERT.                                               EL658
00748      MOVE LINK-ELDATCV           TO PGM-NAME.                     EL658
00749      EXEC CICS LINK                                               EL658
00750          PROGRAM    (PGM-NAME)                                    EL658
00751          COMMAREA   (DATE-CONVERSION-DATA)                        EL658
00752          LENGTH     (DC-COMM-LENGTH)                              EL658
00753          END-EXEC.                                                EL658
00754  8500-EXIT.                                                       EL658
00755      EXIT.                                                        EL658
00756                                                                   EL658
00757  8810-PF23.                                                       EL658
00758      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL658
00759      MOVE XCTL-005               TO PGM-NAME.                     EL658
00760      GO TO 9300-XCTL.                                             EL658
00761                                                                   EL658
00762  9100-RETURN-TRAN.                                                EL658
00763      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.                  CL**2
00764      MOVE '658A'               TO PI-CURRENT-SCREEN-NO.              CL**2
00765      EXEC CICS RETURN                                             EL658
00766          TRANSID(TRANS-ID)                                        EL658
00767          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL658
00768          LENGTH(PI-COMM-LENGTH)                                   EL658
00769          END-EXEC.                                                EL658
00770                                                                   EL658
00771      GOBACK.                                                      EL658
00772                                                                   EL658
00773  9200-RETURN-MAIN-MENU.                                           EL658
00774      MOVE XCTL-626               TO PGM-NAME.                     EL658
00775      GO TO 9300-XCTL.                                             EL658
00776                                                                   EL658
00777      EJECT                                                        EL658
00778                                                                   EL658
00779  9300-XCTL.                                                       EL658
00780      EXEC CICS XCTL                                               EL658
00781          PROGRAM    (PGM-NAME)                                    EL658
00782          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL658
00783          LENGTH     (PI-COMM-LENGTH)                              EL658
00784          END-EXEC.                                                EL658
00785                                                                   EL658
00786  9400-CLEAR.                                                      EL658
00787      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     EL658
00788      GO TO 9300-XCTL.                                             EL658
00789                                                                   EL658
00790  9500-PF12.                                                          CL**2
00791      MOVE XCTL-010               TO PGM-NAME.                        CL**2
00792      GO TO 9300-XCTL.                                                CL**2
00793                                                                   EL658
00794  9900-ERROR-FORMAT.                                               EL658
00795      IF NOT EMI-ERRORS-COMPLETE                                   EL658
00796          MOVE LINK-001 TO PGM-NAME                                EL658
00797          EXEC CICS LINK                                           EL658
00798              PROGRAM(PGM-NAME)                                    EL658
00799              COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)              EL658
00800              LENGTH(EMI-COMM-LENGTH)                              EL658
00801              END-EXEC.                                            EL658
00802  9900-EXIT.                                                       EL658
00803      EXIT.                                                        EL658
00804                                                                   EL658
00805  9990-ABEND.                                                      EL658
00806      MOVE LINK-004 TO PGM-NAME.                                   EL658
00807      MOVE DFHEIBLK TO EMI-LINE1.                                  EL658
00808      EXEC CICS LINK                                               EL658
00809          PROGRAM   (PGM-NAME)                                     EL658
00810          COMMAREA  (EMI-LINE1)                                    EL658
00811          LENGTH    (72)                                           EL658
00812          END-EXEC.                                                EL658
00813      GO TO 8200-SEND-DATAONLY.                                    EL658
