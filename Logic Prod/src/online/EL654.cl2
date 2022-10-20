00001  IDENTIFICATION DIVISION.                                         03/06/96
00002                                                                   EL654
00003  PROGRAM-ID.                 EL654 .                                 LV004
00004 *              PROGRAM CONVERTED BY                                  CL**4
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**4
00006 *              CONVERSION DATE 02/12/96 10:24:42.                    CL**4
00007 *                            VMOD=2.004                              CL**4
00008 *                                                                 EL654
00008 *                                                                 EL654
00009 *AUTHOR.        LOGIC,INC.                                           CL**4
00010 *               DALLAS, TEXAS.                                       CL**4
00011                                                                   EL654
00012 *DATE-COMPILED.                                                      CL**4
00013                                                                   EL654
00014 *SECURITY.   *****************************************************   CL**4
00015 *            *                                                   *   CL**4
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**4
00017 *            *                                                   *   CL**4
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**4
00019 *                                                                *   CL**4
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**4
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**4
00022 *            *                                                   *   CL**4
00023 *            *****************************************************   CL**4
00024                                                                   EL654
00025 *REMARKS.                                                            CL**4
00026 *        TRANSACTION - EXD6 - LIFE EDIT TABLE.                       CL**4
00027                                                                   EL654
00028  ENVIRONMENT DIVISION.                                            EL654
00029  DATA DIVISION.                                                   EL654
00030  EJECT                                                            EL654
00031  WORKING-STORAGE SECTION.                                         EL654
00032  77  FILLER  PIC X(32)  VALUE '********************************'. EL654
00033  77  FILLER  PIC X(32)  VALUE '*    EL654 WORKING STORAGE     *'. EL654
00034  77  FILLER  PIC X(32)  VALUE '************ V/M 2.004 *********'.    CL**4
00035                                                                   EL654
00036                              COPY ELCSCTM.                           CL**4
00037                              COPY ELCSCRTY.                          CL**4
00038                                                                   EL654
00039     EJECT                                                         EL654
00040                                                                   EL654
00041  01  WS-DATE-AREA.                                                EL654
00042      12  SAVE-DATE           PIC  X(8)       VALUE SPACES.        EL654
00043      12  SAVE-BIN-DATE       PIC  X(2)       VALUE SPACES.        EL654
00044                                                                   EL654
00045  01  ELCNTL-LENGTH           PIC S9(4)       VALUE +750 COMP.        CL**3
00046                                                                   EL654
00047  01  STANDARD-AREAS.                                              EL654
00048      12  MAP-NAME            PIC  X(8)       VALUE 'EL654A'.      EL654
00049      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL654S'.      EL654
00050      12  TRANS-ID            PIC  X(4)       VALUE 'EXD6'.        EL654
00051      12  THIS-PGM            PIC  X(8)       VALUE 'EL654'.       EL654
00052      12  TEST-PGM            PIC  X(8)       VALUE 'EL601'.       EL654
00053      12  PGM-NAME            PIC  X(8).                           EL654
00054      12  TIME-IN             PIC S9(7).                           EL654
00055      12  TIME-OUT-R  REDEFINES  TIME-IN.                          EL654
00056          16  FILLER          PIC  X.                              EL654
00057          16  TIME-OUT        PIC  9(2)V9(2).                      EL654
00058          16  FILLER          PIC  X(2).                           EL654
00059      12  XCTL-005            PIC  X(8)       VALUE 'EL005'.       EL654
00060      12  XCTL-010            PIC  X(8)       VALUE 'EL010'.       EL654
00061      12  XCTL-601            PIC  X(8)       VALUE 'EL601'.       EL654
00062      12  XCTL-655            PIC  X(8)       VALUE 'EL655'.       EL654
00063      12  LINK-001            PIC  X(8)       VALUE 'EL001'.       EL654
00064      12  LINK-004            PIC  X(8)       VALUE 'EL004'.       EL654
00065      12  LINK-CLDATCV        PIC  X(8)       VALUE 'ELDATCV'.     EL654
00066      12  FILE-ID             PIC  X(8)       VALUE 'ELCNTL'.      EL654
00067      12  M-SUB               PIC  9(3)                  COMP-3.   EL654
00068      12  WS-EDIT-OUT-CODE    PIC XX.                                 CL**2
00069          88  INVALID-OUT-CODE   VALUE '  ' '00'                      CL**2
00070                                       '90' THRU '98'.                CL**2
00071      12  WS-EL654-TITLE.                                          EL654
00072          16  WS-EL654T       PIC  X(6).                           EL654
00073          16  FILLER          PIC  X(13)      VALUE                EL654
00074                  ' - EDIT TABLE'.                                 EL654
00075      12  WS-EL654-PF-TITLE.                                       EL654
00076          16  FILLER          PIC  X(19)      VALUE                EL654
00077                  'PF1=SWITCH TO EDIT '.                           EL654
00078          16  WS-EL654PF      PIC  X(6).                           EL654
00079                                                                   EL654
00080  01  ACCESS-KEYS.                                                 EL654
00081      12  ELCNTL-KEY.                                              EL654
00082          16  CK-COMP-ID      PIC  X(3).                           EL654
00083          16  FILLER          PIC  X          VALUE 'L'.           EL654
00084          16  FILLER          PIC  X(4)       VALUE SPACES.        EL654
00085          16  FILLER          PIC S9(4)       VALUE +0   COMP.     EL654
00086  EJECT                                                            EL654
00087  01  ERROR-NUMBERS.                                               EL654
00088      12  ER-0000             PIC  X(4)       VALUE '0000'.        EL654
00089      12  ER-0004             PIC  X(4)       VALUE '0004'.        EL654
00090      12  ER-0008             PIC  X(4)       VALUE '0008'.        EL654
00091      12  ER-0029             PIC  X(4)       VALUE '0029'.        EL654
00092      12  ER-0070             PIC  X(4)       VALUE '0070'.        EL654
00093      12  ER-2026             PIC  X(4)       VALUE '2026'.        EL654
00094      12  ER-2027             PIC  X(4)       VALUE '2027'.        EL654
00095  EJECT                                                            EL654
00096                                      COPY ELCDATE.                   CL**4
00097  EJECT                                                            EL654
00098                                      COPY ELCLOGOF.                  CL**4
00099  EJECT                                                            EL654
00100                                      COPY ELCATTR.                   CL**4
00101  EJECT                                                            EL654
00102                                      COPY ELCEMIB.                   CL**4
00103  EJECT                                                            EL654
00104                                      COPY ELCINTF.                   CL**4
00105  EJECT                                                            EL654
00106 *                                    COPY ELCJPFX.                   CL**4
00107 *                            PIC  X(750).                            CL**3
00108  EJECT                                                            EL654
00109                              COPY ELCAID.                            CL**4
00110                                                                   EL654
00111  01  FILLER  REDEFINES  DFHAID.                                   EL654
00112      12  FILLER              PIC  X(8).                           EL654
00113      12  PF-VALUES           PIC  X          OCCURS 2 TIMES.      EL654
00114  EJECT                                                            EL654
00115                              COPY EL654S.                            CL**4
00116                                                                   EL654
00117  01  EL654AO-R  REDEFINES  EL654AI.                               EL654
00118      12  FILLER              PIC  X(53).                          EL654
00119      12  SCREEN-TABLE    OCCURS 60 TIMES                          EL654
00120                              INDEXED BY INDX.                     EL654
00121          16  ST-TAB-IN1-L    PIC S9(4)                  COMP.     EL654
00122          16  ST-TAB-IN1-A    PIC  X.                              EL654
00123          16  ST-TAB-IN1      PIC  X(2).                           EL654
00124          16  ST-TAB-OUT1-L   PIC S9(4)                  COMP.     EL654
00125          16  ST-TAB-OUT1-A   PIC  X.                              EL654
00126          16  ST-TAB-OUT1     PIC  X(2).                              CL**2
00127      12  FILLER              PIC  X(189).                            CL**4
00128  EJECT                                                            EL654
00129  LINKAGE SECTION.                                                 EL654
00130  01  DFHCOMMAREA             PIC  X(1024).                        EL654
00131                                                                   EL654
00132  EJECT                                                            EL654
00133                    COPY ELCCNTL.                                     CL**4
00134                                                                      CL**4
00135  EJECT                                                            EL654
00136  PROCEDURE DIVISION.                                              EL654
00137                                                                   EL654
00138      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL654
00139      MOVE '5'                    TO  DC-OPTION-CODE.              EL654
00140                                                                   EL654
00141      PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.             EL654
00142                                                                   EL654
00143      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL654
00144      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL654
00145      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL654
00146                                                                   EL654
00147      IF EIBCALEN = ZERO                                           EL654
00148          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL654
00149                                                                   EL654
00150      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL654
00151          MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6        EL654
00152          MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5        EL654
00153          MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4        EL654
00154          MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3        EL654
00155          MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2        EL654
00156          MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1        EL654
00157          MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM      EL654
00158          MOVE THIS-PGM              TO  PI-CALLING-PROGRAM.       EL654
00159                                                                   EL654
00160      EXEC CICS HANDLE CONDITION                                   EL654
00161          PGMIDERR  (9600-PGMID-ERROR)                             EL654
00162          ERROR     (9990-ABEND)                                   EL654
00163          END-EXEC.                                                EL654
00164                                                                   EL654
00165      IF EIBTRNID NOT = TRANS-ID                                   EL654
00166          GO TO 7000-BUILD-INITIAL-MAP.                            EL654
00167                                                                   EL654
00168      IF EIBAID = DFHCLEAR                                         EL654
00169          GO TO 9400-CLEAR.                                        EL654
00170                                                                   EL654
00171      IF NOT SYSTEM-DISPLAY-CAP                                    EL654
00172          MOVE 'READ'         TO SM-READ                           EL654
00173          PERFORM 9995-SECURITY-VIOLATION                          EL654
00174          MOVE ER-0070        TO EMI-ERROR                         EL654
00175          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL654
00176          GO TO 8100-SEND-INITIAL-MAP.                             EL654
00177                                                                   EL654
00178  EJECT                                                            EL654
00179  0200-RECEIVE.                                                    EL654
00180      MOVE LOW-VALUES             TO  EL654AI.                     EL654
00181                                                                   EL654
00182      IF EIBAID = DFHPA1              OR  DFHPA2  OR  DFHPA3       EL654
00183          MOVE ER-0008            TO  EMI-ERROR                    EL654
00184          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL654
00185          MOVE -1                 TO  PFENTERL                     EL654
00186          GO TO 8200-SEND-DATAONLY.                                EL654
00187                                                                   EL654
00188      EXEC CICS RECEIVE                                            EL654
00189          MAP     (MAP-NAME)                                       EL654
00190          MAPSET  (MAPSET-NAME)                                    EL654
00191          INTO    (EL654AI)                                        EL654
00192          END-EXEC.                                                EL654
00193                                                                   EL654
00194      IF PFENTERL = ZERO                                           EL654
00195          GO TO 0300-CHECK-PFKEYS.                                 EL654
00196                                                                   EL654
00197      IF EIBAID NOT = DFHENTER                                     EL654
00198          MOVE ER-0004            TO  EMI-ERROR                    EL654
00199          GO TO 0320-INPUT-ERROR.                                  EL654
00200                                                                   EL654
00201      IF PFENTERI GREATER 0 AND LESS 25                            EL654
00202          MOVE PF-VALUES (PFENTERI)  TO  EIBAID                    EL654
00203      ELSE                                                         EL654
00204          MOVE ER-0029               TO  EMI-ERROR                 EL654
00205          GO TO 0320-INPUT-ERROR.                                  EL654
00206                                                                   EL654
00207  0300-CHECK-PFKEYS.                                               EL654
00208      IF EIBAID = DFHPF1                                           EL654
00209          MOVE XCTL-655           TO  PGM-NAME                     EL654
00210          GO TO 9300-XCTL.                                         EL654
00211                                                                   EL654
00212      IF EIBAID = DFHPF23                                          EL654
00213          GO TO 8810-PF23.                                         EL654
00214                                                                   EL654
00215      IF EIBAID = DFHPF24                                          EL654
00216          GO TO 9200-RETURN-MAIN-MENU.                             EL654
00217                                                                   EL654
00218      IF EIBAID = DFHPF12                                          EL654
00219          GO TO 9500-PF12.                                         EL654
00220                                                                   EL654
00221      IF EIBAID = DFHENTER                                         EL654
00222          GO TO 0330-EDIT-DATA.                                    EL654
00223                                                                   EL654
00224      MOVE ER-0029                TO  EMI-ERROR.                   EL654
00225                                                                   EL654
00226  0320-INPUT-ERROR.                                                EL654
00227      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL654
00228                                                                   EL654
00229      MOVE AL-UNBON               TO  PFENTERA.                    EL654
00230      MOVE -1                     TO  PFENTERL.                    EL654
00231                                                                   EL654
00232      GO TO 8200-SEND-DATAONLY.                                    EL654
00233  EJECT                                                            EL654
00234  0330-EDIT-DATA.                                                  EL654
00235      IF NOT SYSTEM-MODIFY-CAP                                     EL654
00236          MOVE 'UPDATE'           TO  SM-READ                      EL654
00237          PERFORM 9995-SECURITY-VIOLATION                          EL654
00238          MOVE ER-0070            TO  EMI-ERROR                    EL654
00239          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL654
00240          GO TO 7000-BUILD-INITIAL-MAP.                            EL654
00241                                                                   EL654
00242      PERFORM 0400-EDIT-IN-OUT  THRU  0499-EXIT                    EL654
00243          VARYING INDX  FROM  1  BY  1                             EL654
00244              UNTIL INDX  IS GREATER THAN  60.                     EL654
00245                                                                   EL654
00246      IF EMI-ERROR NOT = ZEROS                                     EL654
00247         GO TO 8200-SEND-DATAONLY.                                 EL654
00248                                                                   EL654
00249      GO TO 1000-UPDATE-RECORD.                                    EL654
00250                                                                   EL654
00251  0400-EDIT-IN-OUT.                                                EL654
00252      IF (ST-TAB-IN1-L  (INDX) = ZEROS                             EL654
00253        AND ST-TAB-OUT1-L (INDX) = ZEROS)                          EL654
00254         GO TO 0499-EXIT.                                          EL654
00255                                                                   EL654
00256      IF ST-TAB-OUT1-L (INDX) = ZERO                               EL654
00257          MOVE -1                 TO  ST-TAB-OUT1-L (INDX)         EL654
00258          MOVE AL-UABON           TO  ST-TAB-OUT1-A (INDX)            CL**2
00259          MOVE ER-2026            TO  EMI-ERROR                    EL654
00260          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL654
00261      ELSE                                                         EL654
00262          MOVE ST-TAB-OUT1 (INDX) TO WS-EDIT-OUT-CODE                 CL**2
00263          IF INVALID-OUT-CODE                                         CL**2
00264              MOVE -1             TO  ST-TAB-OUT1-L (INDX)         EL654
00265              MOVE AL-UABON       TO  ST-TAB-OUT1-A (INDX)            CL**2
00266              MOVE ER-2027        TO  EMI-ERROR                    EL654
00267              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL654
00268          ELSE                                                     EL654
00269              MOVE AL-UANON       TO  ST-TAB-OUT1-A (INDX).           CL**2
00270                                                                   EL654
00271      IF ST-TAB-IN1-L (INDX) = ZERO                                EL654
00272        OR ST-TAB-IN1   (INDX) = SPACES                            EL654
00273          MOVE -1                 TO  ST-TAB-IN1-L (INDX)          EL654
00274          MOVE AL-UNBON           TO  ST-TAB-IN1-A (INDX)          EL654
00275          MOVE ER-2026            TO  EMI-ERROR                    EL654
00276          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL654
00277                                                                   EL654
00278  0499-EXIT.                                                       EL654
00279       EXIT.                                                       EL654
00280  EJECT                                                            EL654
00281  0500-MOVE-IN-OUT.                                                EL654
00282      IF ST-TAB-IN1-L (INDX) NOT = ZEROS                           EL654
00283          MOVE ST-TAB-OUT1 (INDX)  TO  CF-LIFE-CODE-OUT (M-SUB)    EL654
00284          MOVE ST-TAB-IN1  (INDX)  TO  CF-LIFE-CODE-IN  (M-SUB)    EL654
00285          ADD 1                    TO  M-SUB.                      EL654
00286                                                                   EL654
00287  0599-EXIT.                                                       EL654
00288       EXIT.                                                       EL654
00289  EJECT                                                            EL654
00290  0600-FORMAT-SCREEN.                                              EL654
00291      IF CF-LIFE-CODE-IN (M-SUB) NOT = SPACES                      EL654
00292          MOVE CF-LIFE-CODE-OUT (M-SUB)  TO  ST-TAB-OUT1 (INDX)    EL654
00293          MOVE CF-LIFE-CODE-IN  (M-SUB)  TO  ST-TAB-IN1  (INDX)    EL654
00294          MOVE AL-UANON                  TO  ST-TAB-IN1-A  (INDX)  EL654
00295                                             ST-TAB-OUT1-A (INDX).    CL**2
00296                                                                   EL654
00297      ADD 1                       TO  M-SUB.                       EL654
00298                                                                   EL654
00299  0699-EXIT.                                                       EL654
00300       EXIT.                                                       EL654
00301                                                                   EL654
00302  0700-ZERO-EMPTY-SLOTS.                                           EL654
00303      MOVE ZEROS                  TO  CF-LIFE-CODE-OUT (M-SUB).    EL654
00304                                                                   EL654
00305      ADD 1                       TO  M-SUB.                       EL654
00306                                                                   EL654
00307  0799-EXIT.                                                       EL654
00308       EXIT.                                                       EL654
00309  EJECT                                                            EL654
00310  1000-UPDATE-RECORD.                                              EL654
00311      MOVE PI-COMPANY-ID          TO  CK-COMP-ID.                  EL654
00312                                                                   EL654
00313      EXEC CICS HANDLE CONDITION                                   EL654
00314          NOTFND  (2000-ADD-LIFE)                                  EL654
00315          END-EXEC.                                                EL654
00316                                                                   EL654
00317      EXEC CICS READ                                               EL654
00318          UPDATE                                                   EL654
00319          DATASET  (FILE-ID)                                       EL654
00320          SET      (ADDRESS OF CONTROL-FILE)                          CL**4
00321          RIDFLD   (ELCNTL-KEY)                                    EL654
00322          END-EXEC.                                                EL654
00323                                                                   EL654
00324 *    MOVE 'B'                    TO  JP-RECORD-TYPE.              EL654
00325 *    MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL654
00326                                                                   EL654
00327 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL654
00328                                                                   EL654
00329      MOVE SPACES                 TO  CF-LIFE-EDIT-MASTER-REC.     EL654
00330      MOVE +1                     TO  M-SUB.                       EL654
00331                                                                   EL654
00332      PERFORM 0500-MOVE-IN-OUT  THRU  0599-EXIT                    EL654
00333          VARYING INDX  FROM  1  BY  1                             EL654
00334              UNTIL INDX  IS GREATER THAN  60.                     EL654
00335                                                                   EL654
00336      IF M-SUB  IS LESS THAN  120                                  EL654
00337          PERFORM 0700-ZERO-EMPTY-SLOTS  THRU  0799-EXIT           EL654
00338              UNTIL M-SUB  IS GREATER THAN  120.                   EL654
00339                                                                   EL654
00340      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL654
00341      MOVE '5'                    TO  DC-OPTION-CODE.              EL654
00342      MOVE LINK-CLDATCV           TO  PGM-NAME.                    EL654
00343                                                                   EL654
00344      EXEC CICS LINK                                               EL654
00345          PROGRAM   (PGM-NAME)                                     EL654
00346          COMMAREA  (DATE-CONVERSION-DATA)                         EL654
00347          LENGTH    (DC-COMM-LENGTH)                               EL654
00348          END-EXEC.                                                EL654
00349                                                                   EL654
00350      IF DATE-CONVERSION-ERROR                                     EL654
00351          MOVE LOW-VALUES         TO  CF-LAST-MAINT-DT             EL654
00352      ELSE                                                         EL654
00353          MOVE DC-BIN-DATE-1      TO  CF-LAST-MAINT-DT.            EL654
00354                                                                   EL654
00355      MOVE PI-PROCESSOR-ID        TO  CF-LAST-MAINT-BY.            EL654
00356      MOVE EIBTIME                TO  CF-LAST-MAINT-HHMMSS.        EL654
00357 *    MOVE 'C'                    TO  JP-RECORD-TYPE.              EL654
00358 *    MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL654
00359                                                                   EL654
00360      EXEC CICS REWRITE                                            EL654
00361          DATASET  (FILE-ID)                                       EL654
00362          FROM     (CONTROL-FILE)                                  EL654
00363          END-EXEC.                                                EL654
00364                                                                   EL654
00365 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL654
00366                                                                   EL654
00367      MOVE ER-0000                TO  EMI-ERROR.                   EL654
00368                                                                   EL654
00369      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL654
00370                                                                   EL654
00371      GO TO 7000-BUILD-INITIAL-MAP.                                EL654
00372                                                                   EL654
00373  2000-ADD-LIFE.                                                   EL654
00374      EXEC CICS GETMAIN                                            EL654
00375          SET     (ADDRESS OF CONTROL-FILE)                           CL**4
00376          LENGTH  (ELCNTL-LENGTH)                                  EL654
00377          END-EXEC.                                                EL654
00378                                                                   EL654
00379      MOVE SPACES                 TO  CONTROL-FILE.                EL654
00380      MOVE ELCNTL-KEY             TO  CF-CONTROL-PRIMARY.          EL654
00381      MOVE 'CF'                   TO  CF-RECORD-ID.                EL654
00382      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL654
00383      MOVE '5'                    TO  DC-OPTION-CODE.              EL654
00384      MOVE LINK-CLDATCV           TO  PGM-NAME.                    EL654
00385                                                                   EL654
00386      EXEC CICS LINK                                               EL654
00387          PROGRAM   (PGM-NAME)                                     EL654
00388          COMMAREA  (DATE-CONVERSION-DATA)                         EL654
00389          LENGTH    (DC-COMM-LENGTH)                               EL654
00390          END-EXEC.                                                EL654
00391                                                                   EL654
00392      IF DATE-CONVERSION-ERROR                                     EL654
00393          MOVE LOW-VALUES         TO  CF-LAST-MAINT-DT             EL654
00394      ELSE                                                         EL654
00395          MOVE DC-BIN-DATE-1      TO  CF-LAST-MAINT-DT.            EL654
00396                                                                   EL654
00397      MOVE PI-PROCESSOR-ID        TO  CF-LAST-MAINT-BY.            EL654
00398      MOVE EIBTIME                TO  CF-LAST-MAINT-HHMMSS.        EL654
00399      MOVE +1                     TO  M-SUB.                       EL654
00400                                                                   EL654
00401      PERFORM 0500-MOVE-IN-OUT  THRU  0599-EXIT                    EL654
00402          VARYING INDX  FROM  1  BY  1                             EL654
00403              UNTIL INDX  IS GREATER THAN  60.                     EL654
00404                                                                   EL654
00405      IF M-SUB  IS LESS THAN  120                                  EL654
00406          PERFORM 0700-ZERO-EMPTY-SLOTS  THRU  0799-EXIT           EL654
00407              UNTIL M-SUB  IS GREATER THAN  120.                   EL654
00408                                                                   EL654
00409 *    MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL654
00410                                                                   EL654
00411      EXEC CICS WRITE                                              EL654
00412          DATASET  (FILE-ID)                                       EL654
00413          FROM     (CONTROL-FILE)                                  EL654
00414          RIDFLD   (ELCNTL-KEY)                                    EL654
00415          END-EXEC.                                                EL654
00416                                                                   EL654
00417 *    MOVE 'A'                    TO  JP-RECORD-TYPE.              EL654
00418                                                                   EL654
00419 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL654
00420                                                                   EL654
00421      MOVE ER-0000                TO  EMI-ERROR.                   EL654
00422                                                                   EL654
00423      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL654
00424  EJECT                                                            EL654
00425  7000-BUILD-INITIAL-MAP.                                          EL654
00426      MOVE LOW-VALUES             TO  EL654AO.                     EL654
00427      MOVE PI-COMPANY-ID          TO  CK-COMP-ID.                  EL654
00428                                                                   EL654
00429      EXEC CICS HANDLE CONDITION                                   EL654
00430          NOTFND  (8100-SEND-INITIAL-MAP)                          EL654
00431          END-EXEC.                                                EL654
00432                                                                   EL654
00433      EXEC CICS READ                                               EL654
00434          DATASET  (FILE-ID)                                       EL654
00435          SET      (ADDRESS OF CONTROL-FILE)                          CL**4
00436          RIDFLD   (ELCNTL-KEY)                                    EL654
00437          END-EXEC.                                                EL654
00438                                                                   EL654
00439      MOVE +1                     TO  M-SUB.                       EL654
00440                                                                   EL654
00441      PERFORM 0600-FORMAT-SCREEN  THRU  0699-EXIT                  EL654
00442          VARYING INDX  FROM  1  BY  1                             EL654
00443              UNTIL INDX  IS GREATER THAN  60.                     EL654
00444                                                                   EL654
00445      GO TO 8100-SEND-INITIAL-MAP.                                 EL654
00446  EJECT                                                            EL654
00447  8100-SEND-INITIAL-MAP.                                           EL654
00448      MOVE -1                     TO  ST-TAB-IN1-L (1).            EL654
00449      MOVE SAVE-DATE              TO  DATEO.                       EL654
00450      MOVE EIBTIME                TO  TIME-IN.                     EL654
00451      MOVE TIME-OUT               TO  TIMEO.                       EL654
00452      MOVE PI-LIFE-OVERRIDE-L6    TO  WS-EL654T.                   EL654
00453      MOVE PI-AH-OVERRIDE-L6      TO  WS-EL654PF.                  EL654
00454      MOVE WS-EL654-TITLE         TO  EL654TO.                     EL654
00455      MOVE WS-EL654-PF-TITLE      TO  EL654PFO.                    EL654
00456      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    EL654
00457                                                                   EL654
00458      EXEC CICS SEND                                               EL654
00459          MAP     (MAP-NAME)                                       EL654
00460          MAPSET  (MAPSET-NAME)                                    EL654
00461          FROM    (EL654AO)                                        EL654
00462          ERASE                                                    EL654
00463          CURSOR                                                   EL654
00464          END-EXEC.                                                EL654
00465                                                                   EL654
00466      GO TO 9100-RETURN-TRAN.                                      EL654
00467                                                                   EL654
00468  8200-SEND-DATAONLY.                                              EL654
00469      MOVE SAVE-DATE              TO  DATEO.                       EL654
00470      MOVE EIBTIME                TO  TIME-IN.                     EL654
00471      MOVE TIME-OUT               TO  TIMEO.                       EL654
00472      MOVE PI-LIFE-OVERRIDE-L6    TO  WS-EL654T.                   EL654
00473      MOVE PI-AH-OVERRIDE-L6      TO  WS-EL654PF.                  EL654
00474      MOVE WS-EL654-TITLE         TO  EL654TO.                     EL654
00475      MOVE WS-EL654-PF-TITLE      TO  EL654PFO.                    EL654
00476      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    EL654
00477      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.                    EL654
00478                                                                   EL654
00479      EXEC CICS SEND                                               EL654
00480          MAP     (MAP-NAME)                                       EL654
00481          MAPSET  (MAPSET-NAME)                                    EL654
00482          FROM    (EL654AO)                                        EL654
00483          DATAONLY                                                 EL654
00484          CURSOR                                                   EL654
00485          END-EXEC.                                                EL654
00486                                                                   EL654
00487      GO TO 9100-RETURN-TRAN.                                      EL654
00488                                                                   EL654
00489  8300-SEND-TEXT.                                                  EL654
00490      EXEC CICS SEND TEXT                                          EL654
00491          FROM    (LOGOFF-TEXT)                                    EL654
00492          LENGTH  (LOGOFF-LENGTH)                                  EL654
00493          ERASE                                                    EL654
00494          FREEKB                                                   EL654
00495          END-EXEC.                                                EL654
00496                                                                   EL654
00497      EXEC CICS RETURN                                             EL654
00498          END-EXEC.                                                EL654
00499                                                                   EL654
00500 *8400-LOG-JOURNAL-RECORD.                                         EL654
00501 *    MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                  EL654
00502 *    MOVE FILE-ID                TO  JP-FILE-ID.                  EL654
00503 *    MOVE THIS-PGM               TO  JP-PROGRAM-ID.               EL654
00504                                                                   EL654
00505 *    EXEC CICS JOURNAL                                            EL654
00506 *        JFILEID  (PI-JOURNAL-FILE-ID)                            EL654
00507 *        JTYPEID  ('EL')                                          EL654
00508 *        FROM     (JOURNAL-RECORD)                                EL654
00509 *        LENGTH   (773)                                              CL**3
00510 *        END-EXEC.                                                EL654
00511                                                                   EL654
00512  8800-UNAUTHORIZED-ACCESS.                                        EL654
00513      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL654
00514                                                                   EL654
00515      GO TO 8300-SEND-TEXT.                                        EL654
00516                                                                   EL654
00517  8810-PF23.                                                       EL654
00518      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL654
00519      MOVE XCTL-005               TO  PGM-NAME.                    EL654
00520                                                                   EL654
00521      GO TO 9300-XCTL.                                             EL654
00522                                                                   EL654
00523  9100-RETURN-TRAN.                                                EL654
00524      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL654
00525      MOVE '654A'                 TO  PI-CURRENT-SCREEN-NO.        EL654
00526                                                                   EL654
00527      EXEC CICS RETURN                                             EL654
00528          TRANSID   (TRANS-ID)                                     EL654
00529          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL654
00530          LENGTH    (PI-COMM-LENGTH)                               EL654
00531          END-EXEC.                                                EL654
00532                                                                   EL654
00533  9200-RETURN-MAIN-MENU.                                           EL654
00534      MOVE XCTL-601               TO  PGM-NAME.                    EL654
00535                                                                   EL654
00536      GO TO 9300-XCTL.                                             EL654
00537                                                                   EL654
00538  9300-XCTL.                                                       EL654
00539      EXEC CICS XCTL                                               EL654
00540          PROGRAM   (PGM-NAME)                                     EL654
00541          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL654
00542          LENGTH    (PI-COMM-LENGTH)                               EL654
00543          END-EXEC.                                                EL654
00544                                                                   EL654
00545  9400-CLEAR.                                                      EL654
00546      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                    EL654
00547                                                                   EL654
00548      GO TO 9300-XCTL.                                             EL654
00549                                                                   EL654
00550  9500-PF12.                                                       EL654
00551      MOVE XCTL-010               TO  PGM-NAME.                    EL654
00552                                                                   EL654
00553      GO TO 9300-XCTL.                                             EL654
00554                                                                   EL654
00555  9600-PGMID-ERROR.                                                EL654
00556      EXEC CICS HANDLE CONDITION                                   EL654
00557          PGMIDERR  (8300-SEND-TEXT)                               EL654
00558          END-EXEC.                                                EL654
00559                                                                   EL654
00560      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          EL654
00561      MOVE ' '                    TO  PI-ENTRY-CD-1.               EL654
00562      MOVE XCTL-005               TO  PGM-NAME.                    EL654
00563      MOVE PGM-NAME               TO  LOGOFF-PGM.                  EL654
00564      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL654
00565                                                                   EL654
00566      GO TO 9300-XCTL.                                             EL654
00567                                                                   EL654
00568  9700-LINK-DATE-CONVERT.                                          EL654
00569      EXEC CICS LINK                                               EL654
00570          PROGRAM   ('ELDATCV')                                    EL654
00571          COMMAREA  (DATE-CONVERSION-DATA)                         EL654
00572          LENGTH    (DC-COMM-LENGTH)                               EL654
00573          END-EXEC.                                                EL654
00574                                                                   EL654
00575  9700-EXIT.                                                       EL654
00576      EXIT.                                                        EL654
00577                                                                   EL654
00578  9900-ERROR-FORMAT.                                               EL654
00579      IF NOT EMI-ERRORS-COMPLETE                                   EL654
00580          MOVE LINK-001           TO  PGM-NAME                     EL654
00581          EXEC CICS LINK                                           EL654
00582              PROGRAM   (PGM-NAME)                                 EL654
00583              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL654
00584              LENGTH    (EMI-COMM-LENGTH)                          EL654
00585              END-EXEC.                                            EL654
00586                                                                   EL654
00587  9900-EXIT.                                                       EL654
00588      EXIT.                                                        EL654
00589                                                                   EL654
00590  9990-ABEND.                                                      EL654
00591      MOVE LINK-004               TO  PGM-NAME.                    EL654
00592      MOVE DFHEIBLK               TO  EMI-LINE1                    EL654
00593                                                                   EL654
00594      EXEC CICS LINK                                               EL654
00595          PROGRAM   (PGM-NAME)                                     EL654
00596          COMMAREA  (EMI-LINE1)                                    EL654
00597          LENGTH    (72)                                           EL654
00598          END-EXEC.                                                EL654
00599                                                                   EL654
00600      GO TO 8200-SEND-DATAONLY.                                    EL654
00601                                                                   EL654
00602      GOBACK.                                                      EL654
00603                                                                   EL654
00604  9995-SECURITY-VIOLATION.                                         EL654
00605                              COPY ELCSCTP.                        EL654
00606                                                                   EL654
00607  9995-EXIT.                                                       EL654
00608      EXIT.                                                        EL654
00609                                                                   EL654
