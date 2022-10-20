00001  IDENTIFICATION DIVISION.                                         03/06/96
00002                                                                   EL655
00003  PROGRAM-ID.                 EL655 .                                 LV005
00004 *              PROGRAM CONVERTED BY                                  CL**5
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**5
00006 *              CONVERSION DATE 02/12/96 10:51:56.                    CL**5
00007 *                            VMOD=2.005                              CL**5
00008 *                                                                 EL655
00008 *                                                                 EL655
00009 *AUTHOR.        LOGIC,INC.                                           CL**5
00010 *               DALLAS, TEXAS.                                       CL**5
00011                                                                   EL655
00012 *DATE-COMPILED.                                                      CL**5
00013  SKIP1                                                               CL**3
00014 *SECURITY.   *****************************************************   CL**5
00015 *            *                                                   *   CL**5
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**5
00017 *            *                                                   *   CL**5
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**5
00019 *                                                                *   CL**5
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**5
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**5
00022 *            *                                                   *   CL**5
00023 *            *****************************************************   CL**5
00024                                                                   EL655
00025 *REMARKS.                                                            CL**3
00026 *        TRANSACTION - EXD7 - A/H EDIT TABLE.                        CL**3
00027 *                                                                    CL**3
00028  ENVIRONMENT DIVISION.                                            EL655
00029  DATA DIVISION.                                                   EL655
00030  EJECT                                                            EL655
00031  WORKING-STORAGE SECTION.                                         EL655
00032  77  FILLER  PIC X(32)  VALUE '********************************'. EL655
00033  77  FILLER  PIC X(32)  VALUE '*    EL655 WORKING STORAGE     *'. EL655
00034  77  FILLER  PIC X(32)  VALUE '************ V/M 2.005 *********'.    CL**5
00035                                                                   EL655
00036                              COPY ELCSCTM.                           CL**5
00037                                                                      CL**3
00038                              COPY ELCSCRTY.                          CL**5
00039  EJECT                                                               CL**3
00040  01  WS-DATE-AREA.                                                EL655
00041      12  SAVE-DATE           PIC  X(8)       VALUE SPACES.        EL655
00042      12  SAVE-BIN-DATE       PIC  X(2)       VALUE SPACES.        EL655
00043                                                                   EL655
00044  01  ELCNTL-LENGTH           PIC S9(4)       VALUE +750   COMP.      CL**4
00045                                                                   EL655
00046  01  STANDARD-AREAS.                                              EL655
00047      12  MAP-NAME            PIC  X(8)       VALUE 'EL655A'.      EL655
00048      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL655S'.      EL655
00049      12  TRANS-ID            PIC  X(4)       VALUE 'EXD7'.        EL655
00050      12  THIS-PGM            PIC  X(8)       VALUE 'EL655'.       EL655
00051      12  PGM-NAME            PIC  X(8).                           EL655
00052      12  TIME-IN             PIC S9(7).                           EL655
00053      12  TIME-OUT-R  REDEFINES  TIME-IN.                          EL655
00054          16  FILLER          PIC  X.                              EL655
00055          16  TIME-OUT        PIC  9(2)V9(2).                      EL655
00056          16  FILLER          PIC  X(2).                           EL655
00057      12  XCTL-005            PIC  X(8)       VALUE 'EL005'.       EL655
00058      12  XCTL-010            PIC  X(8)       VALUE 'EL010'.       EL655
00059      12  XCTL-601            PIC  X(8)       VALUE 'EL601'.       EL655
00060      12  XCTL-654            PIC  X(8)       VALUE 'EL654'.       EL655
00061      12  LINK-001            PIC  X(8)       VALUE 'EL001'.       EL655
00062      12  LINK-004            PIC  X(8)       VALUE 'EL004'.       EL655
00063      12  LINK-CLDATCV        PIC  X(8)       VALUE 'ELDATCV'.     EL655
00064      12  FILE-ID             PIC  X(8)       VALUE 'ELCNTL'.      EL655
00065      12  M-SUB               PIC  9(3)                  COMP-3.   EL655
00066      12  WS-EDIT-OUT-CODE    PIC  XX.                                CL**2
00067          88  INVALID-OUT-CODE    VALUE '  ' '00'                     CL**2
00068                                        '90' THRU '98'.               CL**2
00069      12  WS-EL655-TITLE.                                          EL655
00070          16  WS-EL655T       PIC  X(6).                           EL655
00071          16  FILLER          PIC  X(13)      VALUE                EL655
00072                  ' - EDIT TABLE'.                                 EL655
00073      12  WS-EL655-PF-TITLE.                                       EL655
00074          16  FILLER          PIC  X(19)      VALUE                EL655
00075                  'PF1=SWITCH TO EDIT '.                           EL655
00076          16  WS-EL655PF      PIC  X(6).                           EL655
00077                                                                   EL655
00078  01  ACCESS-KEYS.                                                 EL655
00079      12  ELCNTL-KEY.                                              EL655
00080          16  CK-COMP-ID      PIC  X(3).                           EL655
00081          16  FILLER          PIC  X          VALUE 'A'.           EL655
00082          16  FILLER          PIC  X(4)       VALUE SPACES.        EL655
00083          16  FILLER          PIC S9(4)       VALUE +0   COMP.     EL655
00084  EJECT                                                            EL655
00085  01  ERROR-NUMBERS.                                               EL655
00086      12  ER-0000             PIC  X(4)       VALUE '0000'.        EL655
00087      12  ER-0004             PIC  X(4)       VALUE '0004'.        EL655
00088      12  ER-0008             PIC  X(4)       VALUE '0008'.        EL655
00089      12  ER-0029             PIC  X(4)       VALUE '0029'.        EL655
00090      12  ER-0070             PIC  X(4)       VALUE '0070'.        EL655
00091      12  ER-2026             PIC  X(4)       VALUE '2026'.        EL655
00092      12  ER-2027             PIC  X(4)       VALUE '2027'.        EL655
00093  EJECT                                                            EL655
00094                              COPY ELCDATE.                           CL**5
00095  EJECT                                                            EL655
00096                              COPY ELCLOGOF.                          CL**5
00097  EJECT                                                            EL655
00098                              COPY ELCATTR.                           CL**5
00099  EJECT                                                            EL655
00100                              COPY ELCEMIB.                           CL**5
00101  EJECT                                                            EL655
00102                              COPY ELCINTF.                           CL**5
00103  EJECT                                                            EL655
00104                              COPY ELCJPFX.                           CL**5
00105                              PIC  X(750).                            CL**4
00106  EJECT                                                            EL655
00107                              COPY ELCAID.                            CL**5
00108                                                                   EL655
00109  01  FILLER  REDEFINES  DFHAID.                                   EL655
00110      12  FILLER              PIC  X(8).                           EL655
00111      12  PF-VALUES           PIC  X          OCCURS 2 TIMES.      EL655
00112  EJECT                                                            EL655
00113                              COPY EL655S.                            CL**5
00114                                                                   EL655
00115  01  EL655AO-R  REDEFINES  EL655AI.                               EL655
00116      12  FILLER              PIC  X(53).                          EL655
00117      12  SCREEN-TABLE    OCCURS 60 TIMES                          EL655
00118                              INDEXED BY INDX.                     EL655
00119          16  ST-TAB-IN1-L    PIC S9(4)                  COMP.     EL655
00120          16  ST-TAB-IN1-A    PIC  X.                              EL655
00121          16  ST-TAB-IN1      PIC  X(3).                           EL655
00122          16  ST-TAB-OUT1-L   PIC S9(4)                  COMP.     EL655
00123          16  ST-TAB-OUT1-A   PIC  X.                              EL655
00124          16  ST-TAB-OUT1     PIC  X(2).                              CL**2
00125      12  FILLER              PIC  X(183).                            CL**5
00126  EJECT                                                            EL655
00127  LINKAGE SECTION.                                                 EL655
00128  01  DFHCOMMAREA             PIC  X(1024).                        EL655
00129                                                                   EL655
00130  EJECT                                                            EL655
00131                    COPY ELCCNTL.                                     CL**5
00132                                                                      CL**5
00133  EJECT                                                            EL655
00134  PROCEDURE DIVISION.                                              EL655
00135                                                                   EL655
00136      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL655
00137      MOVE '5'                    TO  DC-OPTION-CODE.              EL655
00138                                                                   EL655
00139      PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.             EL655
00140                                                                   EL655
00141      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL655
00142      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL655
00143      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL655
00144                                                                   EL655
00145      IF EIBCALEN = ZERO                                           EL655
00146          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL655
00147                                                                   EL655
00148      EXEC CICS HANDLE CONDITION                                   EL655
00149          PGMIDERR  (9600-PGMID-ERROR)                             EL655
00150          ERROR     (9990-ABEND)                                   EL655
00151      END-EXEC.                                                    EL655
00152                                                                   EL655
00153      IF EIBTRNID NOT = TRANS-ID                                   EL655
00154          GO TO 7000-BUILD-INITIAL-MAP.                            EL655
00155                                                                   EL655
00156      IF EIBAID = DFHCLEAR                                         EL655
00157          GO TO 9400-CLEAR.                                        EL655
00158                                                                      CL**3
00159      IF NOT SYSTEM-DISPLAY-CAP                                       CL**3
00160          MOVE 'READ'             TO  SM-READ                         CL**3
00161          PERFORM 9995-SECURITY-VIOLATION                             CL**3
00162          MOVE ER-0070            TO  EMI-ERROR                       CL**3
00163          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                  CL**3
00164          GO TO 8100-SEND-INITIAL-MAP.                                CL**3
00165  EJECT                                                            EL655
00166  0200-RECEIVE.                                                    EL655
00167      MOVE LOW-VALUES             TO  EL655AI.                     EL655
00168                                                                   EL655
00169      IF EIBAID = DFHPA1 OR  DFHPA2  OR  DFHPA3                    EL655
00170          MOVE ER-0008            TO  EMI-ERROR                    EL655
00171          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL655
00172          MOVE -1                 TO  PFENTERL                     EL655
00173          GO TO 8200-SEND-DATAONLY.                                EL655
00174                                                                   EL655
00175      EXEC CICS RECEIVE                                            EL655
00176          MAP     (MAP-NAME)                                       EL655
00177          MAPSET  (MAPSET-NAME)                                    EL655
00178          INTO    (EL655AI)                                        EL655
00179      END-EXEC.                                                    EL655
00180                                                                   EL655
00181      IF PFENTERL = ZERO                                           EL655
00182          GO TO 0300-CHECK-PFKEYS.                                 EL655
00183                                                                   EL655
00184      IF EIBAID NOT = DFHENTER                                     EL655
00185          MOVE ER-0004            TO  EMI-ERROR                    EL655
00186          GO TO 0320-INPUT-ERROR.                                  EL655
00187                                                                   EL655
00188      IF (PFENTERI NUMERIC)                                        EL655
00189        AND (PFENTERI GREATER ZERO AND LESS 25)                    EL655
00190          MOVE PF-VALUES (PFENTERI)  TO  EIBAID                    EL655
00191      ELSE                                                         EL655
00192          MOVE ER-0029               TO  EMI-ERROR                 EL655
00193          GO TO 0320-INPUT-ERROR.                                  EL655
00194                                                                   EL655
00195  0300-CHECK-PFKEYS.                                               EL655
00196      IF EIBAID = DFHPF1                                           EL655
00197          MOVE XCTL-654           TO  PGM-NAME                     EL655
00198          GO TO 9300-XCTL.                                         EL655
00199                                                                   EL655
00200      IF EIBAID = DFHPF23                                          EL655
00201          GO TO 8810-PF23.                                         EL655
00202                                                                   EL655
00203      IF EIBAID = DFHPF24                                          EL655
00204          GO TO 9200-RETURN-MAIN-MENU.                             EL655
00205                                                                   EL655
00206      IF EIBAID = DFHPF12                                          EL655
00207          GO TO 9500-PF12.                                         EL655
00208                                                                   EL655
00209      IF EIBAID = DFHENTER                                         EL655
00210          GO TO 0330-EDIT-DATA.                                    EL655
00211                                                                   EL655
00212      MOVE ER-0029                TO  EMI-ERROR.                   EL655
00213                                                                   EL655
00214  0320-INPUT-ERROR.                                                EL655
00215      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL655
00216                                                                   EL655
00217      MOVE AL-UNBON               TO  PFENTERA.                    EL655
00218      MOVE -1                     TO  PFENTERL.                    EL655
00219                                                                   EL655
00220      GO TO 8200-SEND-DATAONLY.                                    EL655
00221  EJECT                                                            EL655
00222  0330-EDIT-DATA.                                                  EL655
00223      IF NOT SYSTEM-MODIFY-CAP                                        CL**3
00224          MOVE 'UPDATE'           TO  SM-READ                         CL**3
00225          PERFORM 9995-SECURITY-VIOLATION                             CL**3
00226          MOVE ER-0070            TO  EMI-ERROR                    EL655
00227          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL655
00228          GO TO 7000-BUILD-INITIAL-MAP.                            EL655
00229                                                                   EL655
00230      PERFORM 0400-EDIT-IN-OUT  THRU  0499-EXIT                    EL655
00231          VARYING INDX  FROM  1  BY  1                             EL655
00232              UNTIL INDX  IS GREATER THAN  60.                     EL655
00233                                                                   EL655
00234      IF EMI-ERROR NOT = ZEROS                                     EL655
00235         GO TO 8200-SEND-DATAONLY.                                 EL655
00236                                                                   EL655
00237      GO TO 1000-UPDATE-RECORD.                                    EL655
00238                                                                   EL655
00239  0400-EDIT-IN-OUT.                                                EL655
00240      IF (ST-TAB-IN1-L  (INDX) = ZEROS                             EL655
00241        AND ST-TAB-OUT1-L (INDX) = ZEROS)                          EL655
00242          GO TO 0499-EXIT.                                         EL655
00243                                                                   EL655
00244      IF ST-TAB-OUT1-L (INDX) = ZERO                               EL655
00245          MOVE -1                 TO  ST-TAB-OUT1-L (INDX)         EL655
00246          MOVE AL-UABON           TO  ST-TAB-OUT1-A (INDX)            CL**2
00247          MOVE ER-2026            TO  EMI-ERROR                    EL655
00248          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL655
00249      ELSE                                                         EL655
00250          MOVE ST-TAB-OUT1 (INDX) TO WS-EDIT-OUT-CODE                 CL**2
00251          IF INVALID-OUT-CODE                                         CL**2
00252              MOVE -1             TO  ST-TAB-OUT1-L (INDX)         EL655
00253              MOVE AL-UABON       TO  ST-TAB-OUT1-A (INDX)            CL**2
00254              MOVE ER-2027        TO  EMI-ERROR                    EL655
00255              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL655
00256          ELSE                                                     EL655
00257              MOVE AL-UANON       TO  ST-TAB-OUT1-A (INDX).           CL**2
00258                                                                   EL655
00259      IF ST-TAB-IN1-L (INDX) = ZERO                                EL655
00260        OR ST-TAB-IN1   (INDX) = SPACES                            EL655
00261          MOVE -1                 TO  ST-TAB-IN1-L (INDX)          EL655
00262          MOVE AL-UABON           TO  ST-TAB-IN1-A (INDX)             CL**2
00263          MOVE ER-2026            TO  EMI-ERROR                    EL655
00264          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL655
00265                                                                   EL655
00266  0499-EXIT.                                                       EL655
00267       EXIT.                                                       EL655
00268  EJECT                                                            EL655
00269  0500-MOVE-IN-OUT.                                                EL655
00270      IF ST-TAB-IN1-L (INDX) NOT = ZEROS                           EL655
00271          MOVE ST-TAB-OUT1 (INDX)  TO  CF-AH-CODE-OUT (M-SUB)      EL655
00272          MOVE ST-TAB-IN1  (INDX)  TO  CF-AH-CODE-IN  (M-SUB)      EL655
00273          ADD 1                    TO  M-SUB.                      EL655
00274                                                                   EL655
00275  0599-EXIT.                                                       EL655
00276       EXIT.                                                       EL655
00277  EJECT                                                            EL655
00278  0600-FORMAT-SCREEN.                                              EL655
00279      IF CF-AH-CODE-IN (M-SUB) NOT = SPACES                        EL655
00280          MOVE CF-AH-CODE-OUT (M-SUB)  TO  ST-TAB-OUT1 (INDX)      EL655
00281          MOVE CF-AH-CODE-IN  (M-SUB)  TO  ST-TAB-IN1  (INDX)      EL655
00282          MOVE AL-UANON                TO  ST-TAB-IN1-A  (INDX)    EL655
00283                                           ST-TAB-OUT1-A (INDX).      CL**2
00284                                                                   EL655
00285      ADD 1                       TO  M-SUB.                       EL655
00286                                                                   EL655
00287  0699-EXIT.                                                       EL655
00288       EXIT.                                                       EL655
00289                                                                   EL655
00290  0700-ZERO-EMPTY-SLOTS.                                           EL655
00291      MOVE ZEROS                  TO  CF-AH-CODE-OUT (M-SUB).      EL655
00292                                                                   EL655
00293      ADD 1                       TO  M-SUB.                       EL655
00294                                                                   EL655
00295  0799-EXIT.                                                       EL655
00296       EXIT.                                                       EL655
00297  EJECT                                                            EL655
00298  1000-UPDATE-RECORD.                                              EL655
00299      MOVE PI-COMPANY-ID          TO  CK-COMP-ID.                  EL655
00300                                                                   EL655
00301      EXEC CICS HANDLE CONDITION                                   EL655
00302          NOTFND  (2000-ADD-AH)                                    EL655
00303          END-EXEC.                                                EL655
00304                                                                   EL655
00305      EXEC CICS READ                                               EL655
00306          UPDATE                                                   EL655
00307          DATASET  (FILE-ID)                                       EL655
00308          SET      (ADDRESS OF CONTROL-FILE)                          CL**5
00309          RIDFLD   (ELCNTL-KEY)                                    EL655
00310          END-EXEC.                                                EL655
00311                                                                   EL655
00312      MOVE 'B'                    TO  JP-RECORD-TYPE.              EL655
00313      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL655
00314                                                                   EL655
00315      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL655
00316                                                                   EL655
00317      MOVE SPACES                 TO  CF-AH-EDIT-MASTER-REC.       EL655
00318      MOVE +1                     TO  M-SUB.                       EL655
00319                                                                   EL655
00320      PERFORM 0500-MOVE-IN-OUT  THRU  0599-EXIT                    EL655
00321          VARYING INDX  FROM  1  BY  1                             EL655
00322              UNTIL INDX  IS GREATER THAN  60.                     EL655
00323                                                                   EL655
00324      IF M-SUB  IS LESS THAN  96                                   EL655
00325          PERFORM 0700-ZERO-EMPTY-SLOTS  THRU  0799-EXIT           EL655
00326              UNTIL M-SUB  IS GREATER THAN  96.                    EL655
00327                                                                   EL655
00328      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL655
00329      MOVE '5'                    TO  DC-OPTION-CODE.              EL655
00330      MOVE LINK-CLDATCV           TO  PGM-NAME.                    EL655
00331                                                                   EL655
00332      EXEC CICS LINK                                               EL655
00333          PROGRAM   (PGM-NAME)                                     EL655
00334          COMMAREA  (DATE-CONVERSION-DATA)                         EL655
00335          LENGTH    (DC-COMM-LENGTH)                               EL655
00336      END-EXEC.                                                    EL655
00337                                                                   EL655
00338      IF DATE-CONVERSION-ERROR                                     EL655
00339          MOVE LOW-VALUES         TO  CF-LAST-MAINT-DT             EL655
00340      ELSE                                                         EL655
00341          MOVE DC-BIN-DATE-1      TO  CF-LAST-MAINT-DT.            EL655
00342                                                                   EL655
00343      MOVE PI-PROCESSOR-ID        TO  CF-LAST-MAINT-BY.            EL655
00344      MOVE EIBTIME                TO  CF-LAST-MAINT-HHMMSS.        EL655
00345      MOVE 'C'                    TO  JP-RECORD-TYPE.              EL655
00346      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL655
00347                                                                   EL655
00348      EXEC CICS REWRITE                                            EL655
00349          DATASET  (FILE-ID)                                       EL655
00350          FROM     (CONTROL-FILE)                                  EL655
00351      END-EXEC.                                                    EL655
00352                                                                   EL655
00353      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL655
00354                                                                   EL655
00355      MOVE ER-0000                TO  EMI-ERROR.                   EL655
00356                                                                   EL655
00357      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL655
00358                                                                   EL655
00359      GO TO 7000-BUILD-INITIAL-MAP.                                EL655
00360                                                                   EL655
00361  2000-ADD-AH.                                                     EL655
00362      EXEC CICS GETMAIN                                            EL655
00363          SET     (ADDRESS OF CONTROL-FILE)                           CL**5
00364          LENGTH  (ELCNTL-LENGTH)                                  EL655
00365      END-EXEC.                                                    EL655
00366                                                                   EL655
00367      MOVE SPACES                 TO  CONTROL-FILE.                EL655
00368      MOVE ELCNTL-KEY             TO  CF-CONTROL-PRIMARY.          EL655
00369      MOVE 'CF'                   TO  CF-RECORD-ID.                EL655
00370      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL655
00371      MOVE '5'                    TO  DC-OPTION-CODE.              EL655
00372      MOVE LINK-CLDATCV           TO  PGM-NAME.                    EL655
00373                                                                   EL655
00374      EXEC CICS LINK                                               EL655
00375          PROGRAM   (PGM-NAME)                                     EL655
00376          COMMAREA  (DATE-CONVERSION-DATA)                         EL655
00377          LENGTH    (DC-COMM-LENGTH)                               EL655
00378      END-EXEC.                                                    EL655
00379                                                                   EL655
00380      IF DATE-CONVERSION-ERROR                                     EL655
00381          MOVE LOW-VALUES         TO  CF-LAST-MAINT-DT             EL655
00382      ELSE                                                         EL655
00383          MOVE DC-BIN-DATE-1      TO  CF-LAST-MAINT-DT.            EL655
00384                                                                   EL655
00385      MOVE PI-PROCESSOR-ID        TO  CF-LAST-MAINT-BY.            EL655
00386      MOVE EIBTIME                TO  CF-LAST-MAINT-HHMMSS.        EL655
00387      MOVE +1                     TO  M-SUB.                       EL655
00388                                                                   EL655
00389      PERFORM 0500-MOVE-IN-OUT  THRU  0599-EXIT                    EL655
00390          VARYING INDX  FROM  1  BY  1                             EL655
00391              UNTIL INDX  IS GREATER THAN  60.                     EL655
00392                                                                   EL655
00393      IF M-SUB  IS LESS THAN  96                                   EL655
00394          PERFORM 0700-ZERO-EMPTY-SLOTS  THRU  0799-EXIT           EL655
00395              UNTIL M-SUB  IS GREATER THAN  96.                    EL655
00396                                                                   EL655
00397      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL655
00398                                                                   EL655
00399      EXEC CICS WRITE                                              EL655
00400          DATASET  (FILE-ID)                                       EL655
00401          FROM     (CONTROL-FILE)                                  EL655
00402          RIDFLD   (ELCNTL-KEY)                                    EL655
00403      END-EXEC.                                                    EL655
00404                                                                   EL655
00405      MOVE 'A'                    TO  JP-RECORD-TYPE.              EL655
00406                                                                   EL655
00407      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL655
00408                                                                   EL655
00409      MOVE ER-0000                TO  EMI-ERROR.                   EL655
00410                                                                   EL655
00411      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL655
00412  EJECT                                                            EL655
00413  7000-BUILD-INITIAL-MAP.                                          EL655
00414      MOVE LOW-VALUES             TO  EL655AO.                     EL655
00415      MOVE PI-COMPANY-ID          TO  CK-COMP-ID.                  EL655
00416                                                                   EL655
00417      EXEC CICS HANDLE CONDITION                                   EL655
00418          NOTFND  (8100-SEND-INITIAL-MAP)                          EL655
00419      END-EXEC.                                                    EL655
00420                                                                   EL655
00421      EXEC CICS READ                                               EL655
00422          DATASET  (FILE-ID)                                       EL655
00423          SET      (ADDRESS OF CONTROL-FILE)                          CL**5
00424          RIDFLD   (ELCNTL-KEY)                                    EL655
00425      END-EXEC.                                                    EL655
00426                                                                   EL655
00427      MOVE +1                     TO M-SUB.                        EL655
00428                                                                   EL655
00429      PERFORM 0600-FORMAT-SCREEN  THRU  0699-EXIT                  EL655
00430          VARYING INDX  FROM  1  BY  1                             EL655
00431              UNTIL INDX  IS GREATER THAN  60.                     EL655
00432                                                                   EL655
00433      GO TO 8100-SEND-INITIAL-MAP.                                 EL655
00434  EJECT                                                            EL655
00435  8100-SEND-INITIAL-MAP.                                           EL655
00436      MOVE -1                     TO  ST-TAB-IN1-L (1).            EL655
00437      MOVE SAVE-DATE              TO  DATEO.                       EL655
00438      MOVE EIBTIME                TO  TIME-IN.                     EL655
00439      MOVE TIME-OUT               TO  TIMEO.                       EL655
00440      MOVE PI-AH-OVERRIDE-L6      TO  WS-EL655T.                   EL655
00441      MOVE PI-LIFE-OVERRIDE-L6    TO  WS-EL655PF.                  EL655
00442      MOVE WS-EL655-TITLE         TO  EL655TO.                     EL655
00443      MOVE WS-EL655-PF-TITLE      TO  EL655PFO.                    EL655
00444      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    EL655
00445                                                                   EL655
00446      EXEC CICS SEND                                               EL655
00447          MAP     (MAP-NAME)                                       EL655
00448          MAPSET  (MAPSET-NAME)                                    EL655
00449          FROM    (EL655AO)                                        EL655
00450          ERASE                                                    EL655
00451          CURSOR                                                   EL655
00452      END-EXEC.                                                    EL655
00453                                                                   EL655
00454      GO TO 9100-RETURN-TRAN.                                      EL655
00455                                                                   EL655
00456  8200-SEND-DATAONLY.                                              EL655
00457      MOVE SAVE-DATE              TO  DATEO.                       EL655
00458      MOVE EIBTIME                TO  TIME-IN.                     EL655
00459      MOVE TIME-OUT               TO  TIMEO.                       EL655
00460      MOVE PI-AH-OVERRIDE-L6      TO  WS-EL655T.                   EL655
00461      MOVE PI-LIFE-OVERRIDE-L6    TO  WS-EL655PF.                  EL655
00462      MOVE WS-EL655-TITLE         TO  EL655TO.                     EL655
00463      MOVE WS-EL655-PF-TITLE      TO  EL655PFO.                    EL655
00464      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    EL655
00465      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.                    EL655
00466                                                                   EL655
00467      EXEC CICS SEND                                               EL655
00468          MAP     (MAP-NAME)                                       EL655
00469          MAPSET  (MAPSET-NAME)                                    EL655
00470          FROM    (EL655AO)                                        EL655
00471          DATAONLY                                                 EL655
00472          CURSOR                                                   EL655
00473      END-EXEC.                                                    EL655
00474                                                                   EL655
00475      GO TO 9100-RETURN-TRAN.                                      EL655
00476                                                                   EL655
00477  8300-SEND-TEXT.                                                  EL655
00478      EXEC CICS SEND TEXT                                          EL655
00479          FROM    (LOGOFF-TEXT)                                    EL655
00480          LENGTH  (LOGOFF-LENGTH)                                  EL655
00481          ERASE                                                    EL655
00482          FREEKB                                                   EL655
00483      END-EXEC.                                                    EL655
00484                                                                   EL655
00485      EXEC CICS RETURN                                             EL655
00486      END-EXEC.                                                    EL655
00487                                                                   EL655
00488  8400-LOG-JOURNAL-RECORD.                                         EL655
00489      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                  EL655
00490      MOVE FILE-ID                TO  JP-FILE-ID.                  EL655
00491      MOVE THIS-PGM               TO  JP-PROGRAM-ID.               EL655
00492                                                                   EL655
pemuni*    EXEC CICS JOURNAL                                            EL655
pemuni*        JFILEID  (PI-JOURNAL-FILE-ID)                            EL655
pemuni*        JTYPEID  ('EL')                                          EL655
pemuni*        FROM     (JOURNAL-RECORD)                                EL655
pemuni*        LENGTH   (773)                                              CL**4
pemuni*    END-EXEC.                                                    EL655
00499                                                                   EL655
00500  8800-UNAUTHORIZED-ACCESS.                                        EL655
00501      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL655
00502                                                                   EL655
00503      GO TO 8300-SEND-TEXT.                                        EL655
00504                                                                   EL655
00505  8810-PF23.                                                       EL655
00506      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL655
00507      MOVE XCTL-005               TO  PGM-NAME.                    EL655
00508                                                                   EL655
00509      GO TO 9300-XCTL.                                             EL655
00510                                                                   EL655
00511  9100-RETURN-TRAN.                                                EL655
00512      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL655
00513      MOVE '655A'                 TO  PI-CURRENT-SCREEN-NO.        EL655
00514                                                                   EL655
00515      EXEC CICS RETURN                                             EL655
00516          TRANSID   (TRANS-ID)                                     EL655
00517          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL655
00518          LENGTH    (PI-COMM-LENGTH)                               EL655
00519      END-EXEC.                                                    EL655
00520                                                                   EL655
00521  9200-RETURN-MAIN-MENU.                                           EL655
00522      MOVE XCTL-601               TO  PGM-NAME.                    EL655
00523                                                                   EL655
00524      GO TO 9300-XCTL.                                             EL655
00525                                                                   EL655
00526  9300-XCTL.                                                       EL655
00527      EXEC CICS XCTL                                               EL655
00528          PROGRAM   (PGM-NAME)                                     EL655
00529          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL655
00530          LENGTH    (PI-COMM-LENGTH)                               EL655
00531      END-EXEC.                                                    EL655
00532                                                                   EL655
00533  9400-CLEAR.                                                      EL655
00534      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                    EL655
00535                                                                   EL655
00536      GO TO 9300-XCTL.                                             EL655
00537                                                                   EL655
00538  9500-PF12.                                                       EL655
00539      MOVE XCTL-010               TO  PGM-NAME.                    EL655
00540                                                                   EL655
00541      GO TO 9300-XCTL.                                             EL655
00542                                                                   EL655
00543  9600-PGMID-ERROR.                                                EL655
00544      EXEC CICS HANDLE CONDITION                                   EL655
00545          PGMIDERR  (8300-SEND-TEXT)                               EL655
00546      END-EXEC.                                                    EL655
00547                                                                   EL655
00548      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          EL655
00549      MOVE ' '                    TO  PI-ENTRY-CD-1.               EL655
00550      MOVE XCTL-005               TO  PGM-NAME.                    EL655
00551      MOVE PGM-NAME               TO  LOGOFF-PGM.                  EL655
00552      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL655
00553                                                                   EL655
00554      GO TO 9300-XCTL.                                             EL655
00555                                                                   EL655
00556  9700-LINK-DATE-CONVERT.                                          EL655
00557      EXEC CICS LINK                                               EL655
00558          PROGRAM   ('ELDATCV')                                    EL655
00559          COMMAREA  (DATE-CONVERSION-DATA)                         EL655
00560          LENGTH    (DC-COMM-LENGTH)                               EL655
00561      END-EXEC.                                                    EL655
00562                                                                   EL655
00563  9700-EXIT.                                                       EL655
00564      EXIT.                                                        EL655
00565                                                                   EL655
00566  9900-ERROR-FORMAT.                                               EL655
00567      IF NOT EMI-ERRORS-COMPLETE                                   EL655
00568          MOVE LINK-001           TO  PGM-NAME                     EL655
00569          EXEC CICS LINK                                           EL655
00570              PROGRAM   (PGM-NAME)                                 EL655
00571              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL655
00572              LENGTH    (EMI-COMM-LENGTH)                          EL655
00573          END-EXEC.                                                EL655
00574                                                                   EL655
00575  9900-EXIT.                                                       EL655
00576      EXIT.                                                        EL655
00577                                                                   EL655
00578  9990-ABEND.                                                      EL655
00579      MOVE LINK-004               TO  PGM-NAME.                    EL655
00580      MOVE DFHEIBLK               TO  EMI-LINE1                    EL655
00581                                                                   EL655
00582      EXEC CICS LINK                                               EL655
00583          PROGRAM   (PGM-NAME)                                     EL655
00584          COMMAREA  (EMI-LINE1)                                    EL655
00585          LENGTH    (72)                                           EL655
00586      END-EXEC.                                                    EL655
00587                                                                   EL655
00588      GO TO 8200-SEND-DATAONLY.                                    EL655
00589                                                                   EL655
00590      GOBACK.                                                      EL655
00591                                                                      CL**3
00592  9995-SECURITY-VIOLATION.                                            CL**3
00593                              COPY ELCSCTP.                           CL**3
00594                                                                      CL**3
00595  9995-EXIT.                                                          CL**3
00596      EXIT.                                                           CL**3
