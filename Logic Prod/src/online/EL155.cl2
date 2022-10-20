00001  IDENTIFICATION DIVISION.                                         05/14/96
00002                                                                   EL155
00003  PROGRAM-ID.                 EL155 .                                 LV003
00004 *              PROGRAM CONVERTED BY                                  CL**2
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**2
00006 *              CONVERSION DATE 11/01/95 08:50:15.                    CL**2
00007 *                            VMOD=2.003.                             CL**3
00008 *                                                                 EL155
00008 *                                                                 EL155
00009 *AUTHOR.        LOGIC, INC.                                          CL**2
00010 *               DALLAS, TEXAS.                                       CL**2
00011                                                                   EL155
00012 *DATE-COMPILED.                                                      CL**2
00013                                                                   EL155
00014 *SECURITY.   *****************************************************   CL**2
00015 *            *                                                   *   CL**2
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**2
00017 *            *                                                   *   CL**2
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**2
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**2
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**2
00021 *            *                                                   *   CL**2
00022 *            *****************************************************   CL**2
00023                                                                   EL155
00024 *REMARKS. EX31 - POLICY FORM DISPLAY.                                CL**2
00025      EJECT                                                        EL155
00026  ENVIRONMENT DIVISION.                                            EL155
00027  DATA DIVISION.                                                   EL155
00028  WORKING-STORAGE SECTION.                                         EL155
00029  01  LCP-TIME-OF-DAY-XX.                                             CL**2
00030      05  LCP-TIME-OF-DAY-68        PIC 9(6).                         CL**2
00031      05  FILLER                    PIC 99.                           CL**2
00032  01  LCP-CICS-TIME                 PIC 9(15).                        CL**2
00033  77  FILLER  PIC X(32)  VALUE '********************************'. EL155
00034  77  FILLER  PIC X(32)  VALUE '*   EL155  WORKING STORAGE     *'. EL155
00035  77  FILLER  PIC X(32)  VALUE '************ V/M 2.003 *********'.    CL**3
00036                                                                   EL155
00037  01  WS-DATE-AREA.                                                EL155
00038      05  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL155
00039      05  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.        EL155
00040                                                                   EL155
00041  01  LITERALS-NUMBERS.                                            EL155
00042      12  XCTL-EL005              PIC X(8)    VALUE 'EL005'.          CL**2
00043      12  XCTL-EL010              PIC X(8)    VALUE 'EL010'.          CL**2
00044      12  XCTL-EL126              PIC X(8)    VALUE 'EL126'.          CL**2
00045                                                                      CL**2
00046      12  THIS-PROG               PIC X(8)    VALUE 'EL155'.          CL**2
00047      12  TRANS-ID                PIC X(4)    VALUE 'EX31'.        EL155
00048      12  WS-DMD-PASSED-KEY       PIC X(6).                           CL**2
00049                                                                      CL**2
00050  01  ERROR-NUMBERS.                                               EL155
00051      12  ER-0004                 PIC X(4)    VALUE '0004'.        EL155
00052      12  ER-7008                 PIC X(4)    VALUE '7008'.        EL155
00053      12  ER-0015                 PIC X(4)    VALUE '0015'.        EL155
00054      12  ER-0029                 PIC X(4)    VALUE '0029'.        EL155
00055      12  ER-0126                 PIC X(4)    VALUE '0126'.        EL155
00056      12  ER-0130                 PIC X(4)    VALUE '0130'.        EL155
00057      12  ER-0131                 PIC X(4)    VALUE '0131'.        EL155
00058      12  ER-0418                 PIC X(4)    VALUE '0418'.        EL155
00059                                                                      CL**2
00060  01  EDIT-WORK-AREA.                                              EL155
00061      12  COUNT-1                 PIC 99.                          EL155
00062      12  CHECK-PFKEYS            PIC 99.                          EL155
00063      12  CALL-PGM                PIC X(8).                        EL155
00064                                                                      CL**2
00065  01  FORMATTED-LINE.                                              EL155
00066      12  FILLER                  PIC X(2)    VALUE SPACES.        EL155
00067      12  HOLD-LINE               PIC X(70).                       EL155
00068      12  FILLER                  PIC X(7)    VALUE SPACES.        EL155
00069                                                                      CL**2
00070  01  TIME-UNFORMATTED.                                            EL155
00071      12  UN-HOURS                PIC X(2).                        EL155
00072      12  UN-MINUTES              PIC X(2).                        EL155
00073      12  FILLER                  PIC X(2).                        EL155
00074                                                                      CL**2
00075  01  TIME-FORMATTED.                                              EL155
00076      12  FOR-HOURS               PIC X(2).                        EL155
00077      12  FILLER                  PIC X       VALUE '.'.           EL155
00078      12  FOR-MINUTES             PIC X(2).                        EL155
00079                                                                      CL**2
00080  01  ERROR-SWITCHES.                                              EL155
00081      12  ERROR-SWITCH            PIC X       VALUE SPACE.         EL155
00082          88  END-OF-FILE                     VALUE 'E'.           EL155
00083          88  SCREEN-ERROR                    VALUE 'X'.           EL155
00084                                                                      CL**2
00085  01  ELFORM-KEY.                                                     CL**2
00086      12  FILE-PARTIAL-KEY.                                        EL155
00087          16  COMPANY-CODE        PIC X.                           EL155
00088          16  FORM-NO             PIC X(12).                       EL155
00089      12  LINE-SEQUENCE           PIC S9(4)   COMP.                EL155
00090                                                                      CL**2
00091      EJECT                                                        EL155
00092      COPY ELCLOGOF.                                                  CL**2
00093                                                                      CL**2
00094      EJECT                                                        EL155
00095      COPY ELCDATE.                                                   CL**2
00096                                                                      CL**2
00097      EJECT                                                        EL155
00098      COPY EL155S.                                                    CL**2
00099                                                                      CL**2
00100      EJECT                                                        EL155
00101      COPY ELCAID.                                                    CL**2
00102                                                                   EL155
00103  01  FILLER REDEFINES DFHAID.                                     EL155
00104      12  FILLER                  PIC X(8).                        EL155
00105      12  AID-KEYS OCCURS 24 TIMES.                                EL155
00106          16  FILLER              PIC X.                           EL155
00107      EJECT                                                        EL155
00108      COPY ELCINTF.                                                   CL**2
00109                                                                      CL**2
00110      12  EL155-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.          EL155
00111          16  PASSED-KEY          PIC X(12).                       EL155
00112          16  SAVE-KEY.                                            EL155
00113           20  SAVE-PARTIAL-KEY.                                   EL155
00114                  24  FILLER          PIC X.                       EL155
00115                  24  SAVE-CONTROL    PIC X(12).                   EL155
00116              20  SAVE-LINE           PIC S9(4)   COMP.            EL155
00117          16  EOF-SWITCH          PIC X.                           EL155
00118              88  TOP-OF-FILE                 VALUE 'X'.           EL155
00119              88  BOTTOM-OF-FILE              VALUE 'Y'.           EL155
00120              88  BAD-BROWSE                  VALUE 'Z'.           EL155
00121          16  FILLER              PIC X(514).                         CL**3
00122          16  PASSED-FORM-EL150   PIC X(12).                          CL**3
00123          16  FILLER              PIC X(86).                          CL**3
00124                                                                      CL**2
00125      EJECT                                                        EL155
00126      COPY ELCEMIB.                                                   CL**2
00127                                                                      CL**2
00128      EJECT                                                        EL155
00129  LINKAGE SECTION.                                                 EL155
00130  01  DFHCOMMAREA                 PIC X(1024).                     EL155
00131                                                                      CL**2
00132      COPY ELCTEXT.                                                   CL**2
00133                                                                      CL**2
00134      EJECT                                                        EL155
00135  PROCEDURE DIVISION.                                              EL155
00136                                                                   EL155
00137      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL155
00138      MOVE '5'                   TO DC-OPTION-CODE.                EL155
00139      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL155
00140      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL155
00141      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL155
00142                                                                   EL155
00143      IF EIBCALEN = ZERO                                           EL155
00144          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL155
00145                                                                   EL155
00146      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL155
00147                                                                   EL155
00148      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL155
00149          MOVE LOW-VALUES         TO EL155AO                       EL155
00150          MOVE ER-7008            TO EMI-ERROR                     EL155
00151          PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT    EL155
00152          GO TO 8110-SEND-DATA.                                    EL155
00153                                                                   EL155
00154      IF PI-CALLING-PROGRAM NOT = THIS-PROG                           CL**2
00155          MOVE LOW-VALUES         TO EL155AO                       EL155
00156          GO TO 0100-UPDATE-PI.                                    EL155
00157                                                                   EL155
00158      IF EIBAID = DFHCLEAR                                         EL155
00159          GO TO 8200-RETURN-PRIOR.                                 EL155
00160                                                                   EL155
00161      EXEC CICS HANDLE CONDITION                                   EL155
00162          PGMIDERR  (8820-PGMID-ERROR)                             EL155
00163          MAPFAIL   (8100-SEND-MAP)                                EL155
00164          ERROR     (9990-ABEND)                                   EL155
00165      END-EXEC.                                                    EL155
00166                                                                   EL155
00167      EXEC CICS RECEIVE                                            EL155
00168          MAP     ('EL155A')                                       EL155
00169          MAPSET  ('EL155S')                                       EL155
00170      END-EXEC.                                                    EL155
00171                                                                   EL155
00172      IF PFKEYL GREATER THAN ZERO                                  EL155
00173          PERFORM 0200-TRANS-PF THRU 0210-TRANS-PF-EXIT.           EL155
00174                                                                   EL155
00175      IF SCREEN-ERROR                                              EL155
00176          MOVE ER-7008            TO EMI-ERROR                     EL155
00177          PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT    EL155
00178          GO TO 8110-SEND-DATA.                                    EL155
00179                                                                   EL155
00180      IF EIBAID = DFHPF12                                          EL155
00181          GO TO 8300-GET-HELP.                                     EL155
00182                                                                   EL155
00183      IF EIBAID = DFHPF23                                          EL155
00184          GO TO 8810-PF23-ENTERED.                                 EL155
00185                                                                   EL155
00186      IF EIBAID = DFHPF24                                          EL155
00187          GO TO 8400-RETURN-MASTER.                                EL155
00188                                                                   EL155
00189      IF EIBAID = DFHPF1 OR DFHPF3                                 EL155
00190          GO TO 2000-BROWSE-FORWARD.                               EL155
00191                                                                   EL155
00192      IF EIBAID = DFHPF2 OR DFHPF4                                 EL155
00193          GO TO 2100-BROWSE-BACKWARD.                              EL155
00194                                                                   EL155
00195      IF EIBAID = DFHENTER                                         EL155
00196          GO TO 2000-BROWSE-FORWARD.                               EL155
00197                                                                   EL155
00198      MOVE ER-0029                TO EMI-ERROR.                    EL155
00199      PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT.       EL155
00200      GO TO 8110-SEND-DATA.                                        EL155
00201      EJECT                                                        EL155
00202                                                                      CL**2
00203  0100-UPDATE-PI.                                                  EL155
00204      IF PI-CALLING-PROGRAM = 'EL150'                                 CL**3
00205          MOVE PASSED-FORM-EL150  TO PASSED-KEY.                      CL**3
00206                                                                      CL**3
00207      MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-6.           EL155
00208      MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-5.           EL155
00209      MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-4.           EL155
00210      MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-3.           EL155
00211      MOVE PI-SAVED-PROGRAM-1     TO PI-SAVED-PROGRAM-2.           EL155
00212      MOVE PI-RETURN-TO-PROGRAM   TO PI-SAVED-PROGRAM-1.           EL155
00213      MOVE PI-CALLING-PROGRAM     TO PI-RETURN-TO-PROGRAM.         EL155
00214      MOVE THIS-PROG              TO PI-CALLING-PROGRAM.              CL**2
00215      MOVE ZEROS                  TO SAVE-LINE   SAVE-PARTIAL-KEY. EL155
00216      MOVE SPACES                 TO EOF-SWITCH.                   EL155
00217                                                                   EL155
00218      IF PI-RETURN-TO-PROGRAM NOT = XCTL-EL126                        CL**2
00219         IF PASSED-KEY = SPACES                                    EL155
00220            MOVE ER-0418          TO EMI-ERROR                     EL155
00221            PERFORM 9900-ERROR-FORMAT                              EL155
00222               THRU 9900-ERROR-FORMAT-EXIT                         EL155
00223            GO TO 8110-SEND-DATA                                      CL**2
00224         ELSE                                                      EL155
00225         IF PI-COMPANY-ID = 'DMD'                                     CL**2
00226            MOVE PASSED-KEY (7:6)  TO WS-DMD-PASSED-KEY               CL**2
00227            MOVE WS-DMD-PASSED-KEY TO PASSED-KEY                      CL**2
00228         END-IF                                                       CL**2
00229            MOVE PASSED-KEY        TO FORMNOO                         CL**2
00230            GO TO 2000-BROWSE-FORWARD.                             EL155
00231                                                                   EL155
00232      GO TO 8100-SEND-MAP.                                         EL155
00233                                                                      CL**2
00234  0200-TRANS-PF.                                                   EL155
00235                                                                   EL155
00236      IF EIBAID NOT = DFHENTER                                     EL155
00237         MOVE ER-0004             TO EMI-ERROR                     EL155
00238         PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT     EL155
00239         MOVE -1                  TO PFKEYL                           CL**2
00240         GO TO 8110-SEND-DATA.                                     EL155
00241                                                                   EL155
00242      IF PFKEYI NOT NUMERIC                                        EL155
00243          MOVE 'X'                TO ERROR-SWITCH                     CL**2
00244          GO TO 0210-TRANS-PF-EXIT.                                EL155
00245                                                                   EL155
00246      IF PFKEYI LESS THAN 1                                           CL**2
00247        OR                                                         EL155
00248         PFKEYI GREATER THAN 24                                       CL**2
00249          MOVE 'X'                TO ERROR-SWITCH                     CL**2
00250          GO TO 0210-TRANS-PF-EXIT.                                EL155
00251                                                                   EL155
00252      MOVE PFKEYI                 TO CHECK-PFKEYS.                 EL155
00253      MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.                      EL155
00254                                                                      CL**2
00255  0210-TRANS-PF-EXIT.                                              EL155
00256      EXIT.                                                        EL155
00257      EJECT                                                        EL155
00258  2000-BROWSE-FORWARD.                                             EL155
00259                                                                   EL155
00260      IF FORMNOI NOT = SAVE-CONTROL                                EL155
00261         MOVE SPACE               TO EOF-SWITCH                    EL155
00262         MOVE ZEROS               TO LINE-SEQUENCE                 EL155
00263         MOVE FORMNOI             TO FORM-NO                       EL155
00264         MOVE PI-COMPANY-CD       TO COMPANY-CODE                  EL155
00265         MOVE ELFORM-KEY          TO SAVE-KEY                         CL**2
00266         GO TO 2010-SKIP-ADD.                                      EL155
00267                                                                   EL155
00268      IF BOTTOM-OF-FILE                                            EL155
00269          MOVE ER-0130            TO EMI-ERROR                     EL155
00270          PERFORM 9900-ERROR-FORMAT                                EL155
00271              THRU 9900-ERROR-FORMAT-EXIT                          EL155
00272         GO TO 8110-SEND-DATA.                                     EL155
00273                                                                   EL155
00274      MOVE SPACE                  TO EOF-SWITCH.                   EL155
00275                                                                   EL155
00276      IF EIBAID = DFHPF1                                           EL155
00277         ADD 15                   TO SAVE-LINE                        CL**2
00278      ELSE                                                         EL155
00279         ADD 5                    TO SAVE-LINE.                       CL**2
00280                                                                   EL155
00281      MOVE SAVE-KEY               TO ELFORM-KEY.                      CL**2
00282                                                                      CL**2
00283  2010-SKIP-ADD.                                                   EL155
00284      MOVE 1                      TO COUNT-1                       EL155
00285      MOVE SPACES TO ERROR-SWITCH FORMATTED-LINE.                  EL155
00286                                                                   EL155
00287      PERFORM 4000-START-BROWSE THRU 4010-START-BROWSE-EXIT.       EL155
00288                                                                   EL155
00289      IF BAD-BROWSE                                                EL155
00290         MOVE ER-0126             TO EMI-ERROR                     EL155
00291         PERFORM 9900-ERROR-FORMAT                                 EL155
00292            THRU 9900-ERROR-FORMAT-EXIT                            EL155
00293         MOVE 'Y'                 TO EOF-SWITCH                    EL155
00294         PERFORM 3100-FILL-SCREEN THRU 3110-FILL-SCREEN-EXIT       EL155
00295                 VARYING COUNT-1 FROM 1 BY 1 UNTIL COUNT-1 > 15    EL155
00296         GO TO 8110-SEND-DATA.                                     EL155
00297                                                                   EL155
00298      PERFORM 4020-READ-FILE THRU 4040-READ-FILE-EXIT.             EL155
00299                                                                   EL155
00300      IF END-OF-FILE                                               EL155
00301         GO TO 2020-END-FILE.                                      EL155
00302                                                                   EL155
00303      MOVE ELFORM-KEY TO SAVE-KEY.                                    CL**2
00304                                                                   EL155
00305      IF TX-FORM-NO NOT = FORMNOI                                  EL155
00306          MOVE TX-FORM-NO TO FORMNOO.                              EL155
00307                                                                   EL155
00308      PERFORM 3000-BUILD-SCREEN THRU 3010-BUILD-SCREEN-EXIT        EL155
00309          VARYING COUNT-1 FROM 1 BY 1 UNTIL COUNT-1 > 15           EL155
00310                  OR END-OF-FILE.                                  EL155
00311                                                                   EL155
00312  2020-END-FILE.                                                   EL155
00313                                                                   EL155
00314      IF END-OF-FILE                                               EL155
00315          PERFORM 3100-FILL-SCREEN THRU 3110-FILL-SCREEN-EXIT      EL155
00316            VARYING COUNT-1 FROM COUNT-1 BY 1 UNTIL COUNT-1 > 15.  EL155
00317                                                                   EL155
00318      PERFORM 4050-END-BROWSE THRU 4060-END-BROWSE-EXIT.           EL155
00319      GO TO 8110-SEND-DATA.                                        EL155
00320                                                                      CL**2
00321      EJECT                                                        EL155
00322  2100-BROWSE-BACKWARD.                                            EL155
00323                                                                   EL155
00324      IF FORMNOI NOT = SAVE-CONTROL                                EL155
00325         GO TO 2000-BROWSE-FORWARD.                                EL155
00326                                                                   EL155
00327      IF TOP-OF-FILE                                               EL155
00328          MOVE ER-0131            TO EMI-ERROR                     EL155
00329          PERFORM 9900-ERROR-FORMAT                                EL155
00330              THRU 9900-ERROR-FORMAT-EXIT                          EL155
00331          GO TO 8110-SEND-DATA.                                    EL155
00332                                                                   EL155
00333      MOVE SPACE TO EOF-SWITCH.                                    EL155
00334                                                                   EL155
00335      IF SAVE-LINE = 1                                                CL**2
00336          GO TO 2500-NEW-SCREEN.                                   EL155
00337                                                                   EL155
00338      IF EIBAID = DFHPF2                                           EL155
00339         SUBTRACT 15              FROM SAVE-LINE                      CL**2
00340      ELSE                                                         EL155
00341         SUBTRACT 5               FROM SAVE-LINE.                     CL**2
00342                                                                   EL155
00343      IF SAVE-LINE LESS THAN ZEROES                                EL155
00344          MOVE ZEROES             TO SAVE-LINE.                    EL155
00345                                                                   EL155
00346      MOVE SAVE-KEY               TO ELFORM-KEY.                      CL**2
00347      GO TO 2010-SKIP-ADD.                                         EL155
00348                                                                      CL**2
00349      EJECT                                                        EL155
00350  2500-NEW-SCREEN.                                                 EL155
00351      MOVE SAVE-KEY               TO ELFORM-KEY.                      CL**2
00352      PERFORM 4000-START-BROWSE THRU 4010-START-BROWSE-EXIT.       EL155
00353      PERFORM 4070-READ-PREV THRU 4090-READ-PREV-EXIT.             EL155
00354      PERFORM 4070-READ-PREV THRU 4090-READ-PREV-EXIT.             EL155
00355                                                                   EL155
00356      IF END-OF-FILE                                               EL155
00357         MOVE LOW-VALUES          TO FORMNOI                       EL155
00358      ELSE                                                         EL155
00359         MOVE TX-FORM-NO          TO FORMNOI.                      EL155
00360                                                                   EL155
00361      PERFORM 4050-END-BROWSE  THRU 4060-END-BROWSE-EXIT.          EL155
00362      GO TO 2000-BROWSE-FORWARD.                                   EL155
00363                                                                      CL**2
00364      EJECT                                                        EL155
00365  3000-BUILD-SCREEN.                                               EL155
00366      MOVE TX-TEXT-LINE           TO HOLD-LINE.                    EL155
00367      MOVE FORMATTED-LINE         TO INFOO (COUNT-1).              EL155
00368      PERFORM 4020-READ-FILE THRU 4040-READ-FILE-EXIT.             EL155
00369                                                                   EL155
00370      IF TX-FORM-NO NOT = SAVE-CONTROL                             EL155
00371          MOVE 'E'                TO ERROR-SWITCH.                    CL**2
00372                                                                      CL**2
00373  3010-BUILD-SCREEN-EXIT.                                          EL155
00374      EXIT.                                                        EL155
00375                                                                      CL**2
00376      EJECT                                                        EL155
00377  3100-FILL-SCREEN.                                                EL155
00378      MOVE SPACES                 TO FORMATTED-LINE.               EL155
00379      MOVE FORMATTED-LINE         TO INFOO (COUNT-1).              EL155
00380                                                                      CL**2
00381  3110-FILL-SCREEN-EXIT.                                           EL155
00382      EXIT.                                                        EL155
00383                                                                      CL**2
00384      EJECT                                                        EL155
00385  4000-START-BROWSE.                                               EL155
00386                                                                   EL155
00387      EXEC CICS HANDLE CONDITION                                   EL155
00388          NOTOPEN  (5000-FORM-NOT-OPEN)                            EL155
00389          NOTFND   (4005-KEY-NOT-FOUND)                            EL155
00390      END-EXEC.                                                    EL155
00391                                                                   EL155
00392      EXEC CICS STARTBR                                            EL155
00393          DATASET  ('ELFORM')                                      EL155
00394          RIDFLD   (ELFORM-KEY)                                       CL**2
00395      END-EXEC.                                                    EL155
00396                                                                   EL155
00397      GO TO 4010-START-BROWSE-EXIT.                                EL155
00398                                                                   EL155
00399  4005-KEY-NOT-FOUND.                                              EL155
00400      MOVE 'Z'                    TO EOF-SWITCH.                   EL155
00401                                                                   EL155
00402  4010-START-BROWSE-EXIT.                                          EL155
00403      EXIT.                                                        EL155
00404                                                                   EL155
00405  4020-READ-FILE.                                                  EL155
00406                                                                   EL155
00407      EXEC CICS HANDLE CONDITION                                   EL155
00408          NOTFND   (4030-END-OF-FILE)                              EL155
00409          ENDFILE  (4030-END-OF-FILE)                              EL155
00410      END-EXEC.                                                    EL155
00411                                                                   EL155
00412      EXEC CICS READNEXT                                           EL155
00413          SET      (ADDRESS OF TEXT-FILES)                            CL**2
00414          DATASET  ('ELFORM')                                      EL155
00415          RIDFLD   (ELFORM-KEY)                                       CL**2
00416      END-EXEC.                                                    EL155
00417                                                                   EL155
00418      IF PI-COMPANY-CD = TX-COMPANY-CD                             EL155
00419         GO TO 4040-READ-FILE-EXIT.                                EL155
00420                                                                   EL155
00421  4030-END-OF-FILE.                                                EL155
00422      MOVE 'E'                    TO ERROR-SWITCH.                    CL**2
00423      MOVE 'Y'                    TO EOF-SWITCH                    EL155
00424      MOVE ER-0130                TO EMI-ERROR.                    EL155
00425      PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT.       EL155
00426      GO TO 8110-SEND-DATA.                                           CL**2
00427                                                                      CL**2
00428  4040-READ-FILE-EXIT.                                             EL155
00429      EXIT.                                                        EL155
00430                                                                   EL155
00431  4050-END-BROWSE.                                                 EL155
00432                                                                   EL155
00433      EXEC CICS ENDBR                                              EL155
00434          DATASET ('ELFORM')                                       EL155
00435      END-EXEC.                                                    EL155
00436                                                                      CL**2
00437  4060-END-BROWSE-EXIT.                                            EL155
00438      EXIT.                                                        EL155
00439                                                                   EL155
00440  4070-READ-PREV.                                                  EL155
00441                                                                   EL155
00442      EXEC CICS HANDLE CONDITION                                   EL155
00443          NOTFND   (4080-END-OF-FILE)                              EL155
00444          ENDFILE  (4080-END-OF-FILE)                              EL155
00445      END-EXEC.                                                    EL155
00446                                                                   EL155
00447      EXEC CICS READPREV                                           EL155
00448          SET      (ADDRESS OF TEXT-FILES)                            CL**2
00449          DATASET  ('ELFORM')                                      EL155
00450          RIDFLD   (ELFORM-KEY)                                       CL**2
00451      END-EXEC.                                                    EL155
00452                                                                   EL155
00453      CONTINUE.                                                       CL**2
00454                                                                   EL155
00455      IF PI-COMPANY-CD = TX-COMPANY-CD                             EL155
00456         GO TO 4090-READ-PREV-EXIT.                                EL155
00457                                                                   EL155
00458  4080-END-OF-FILE.                                                EL155
00459      MOVE 'X'                    TO EOF-SWITCH                    EL155
00460      MOVE 'E'                    TO ERROR-SWITCH.                    CL**2
00461      MOVE ER-0131                TO EMI-ERROR.                    EL155
00462      PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT.       EL155
00463      GO TO 8110-SEND-DATA.                                           CL**2
00464                                                                      CL**2
00465  4090-READ-PREV-EXIT.                                             EL155
00466      EXIT.                                                        EL155
00467                                                                      CL**2
00468      EJECT                                                        EL155
00469  5000-FORM-NOT-OPEN.                                              EL155
00470      MOVE ER-0015                TO EMI-ERROR.                    EL155
00471      PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT.       EL155
00472      GO TO 8110-SEND-DATA.                                        EL155
00473                                                                      CL**2
00474      EJECT                                                        EL155
00475  8100-SEND-MAP.                                                   EL155
00476      PERFORM 8120-FORMAT-TIME-DATE                                EL155
00477          THRU 8130-FORMAT-TIME-DATE-EXIT.                         EL155
00478                                                                   EL155
00479      IF PFKEYL NOT = -1                                           EL155
00480         MOVE -1                  TO FORMNOL.                         CL**2
00481                                                                   EL155
00482      EXEC CICS SEND                                               EL155
00483          MAP     ('EL155A')                                       EL155
00484          MAPSET  ('EL155S')                                       EL155
00485          ERASE                                                    EL155
00486          CURSOR                                                   EL155
00487          FREEKB                                                   EL155
00488      END-EXEC.                                                    EL155
00489                                                                   EL155
00490      GO TO 9000-RETURN-TRANS.                                     EL155
00491                                                                   EL155
00492  8110-SEND-DATA.                                                  EL155
00493                                                                   EL155
00494      IF EIBTRNID NOT = TRANS-ID                                   EL155
00495          GO TO 8100-SEND-MAP.                                     EL155
00496                                                                   EL155
00497      PERFORM 8120-FORMAT-TIME-DATE                                EL155
00498          THRU 8130-FORMAT-TIME-DATE-EXIT.                         EL155
00499                                                                   EL155
00500      IF PFKEYL NOT = -1                                           EL155
00501         MOVE -1                  TO FORMNOL.                         CL**2
00502                                                                   EL155
00503      EXEC CICS SEND                                               EL155
00504          MAP     ('EL155A')                                       EL155
00505          MAPSET  ('EL155S')                                       EL155
00506          DATAONLY                                                 EL155
00507          CURSOR                                                   EL155
00508          FREEKB                                                   EL155
00509      END-EXEC.                                                    EL155
00510                                                                   EL155
00511      GO TO 9000-RETURN-TRANS.                                     EL155
00512                                                                   EL155
00513  8120-FORMAT-TIME-DATE.                                           EL155
00514      MOVE SAVE-DATE              TO DATEO.                        EL155
00515                                                                      CL**2
00516      EXEC CICS ASKTIME                                               CL**2
00517          ABSTIME(LCP-CICS-TIME)                                      CL**2
00518      END-EXEC.                                                       CL**2
00519                                                                      CL**2
00520      EXEC CICS FORMATTIME                                            CL**2
00521          ABSTIME(LCP-CICS-TIME)                                      CL**2
00522          TIME(LCP-TIME-OF-DAY-XX)                                    CL**2
00523      END-EXEC.                                                       CL**2
00524                                                                      CL**2
00525      MOVE LCP-TIME-OF-DAY-68     TO TIME-UNFORMATTED.                CL**2
00526      MOVE UN-HOURS               TO FOR-HOURS.                    EL155
00527      MOVE UN-MINUTES             TO FOR-MINUTES.                  EL155
00528      MOVE TIME-FORMATTED         TO TIMEO.                        EL155
00529      MOVE '155A'                 TO PI-CURRENT-SCREEN-NO.            CL**2
00530      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL155
00531      MOVE EMI-MESSAGE-AREA (1)   TO MSGO.                         EL155
00532                                                                      CL**2
00533  8130-FORMAT-TIME-DATE-EXIT.                                      EL155
00534      EXIT.                                                        EL155
00535                                                                   EL155
00536  8200-RETURN-PRIOR.                                               EL155
00537      MOVE PI-RETURN-TO-PROGRAM   TO CALL-PGM.                     EL155
00538      GO TO 9200-XCTL.                                             EL155
00539                                                                   EL155
00540  8300-GET-HELP.                                                   EL155
00541      MOVE XCTL-EL010             TO CALL-PGM.                        CL**2
00542      GO TO 9200-XCTL.                                             EL155
00543                                                                   EL155
00544  8400-RETURN-MASTER.                                              EL155
00545      MOVE XCTL-EL126             TO CALL-PGM.                        CL**2
00546      GO TO 9200-XCTL.                                             EL155
00547                                                                   EL155
00548  8800-UNAUTHORIZED-ACCESS.                                        EL155
00549      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL155
00550      GO TO 8990-SEND-TEXT.                                        EL155
00551                                                                   EL155
00552  8810-PF23-ENTERED.                                               EL155
00553      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL155
00554      MOVE XCTL-EL005             TO CALL-PGM.                        CL**2
00555      GO TO 9200-XCTL.                                             EL155
00556                                                                   EL155
00557  8820-PGMID-ERROR.                                                EL155
00558                                                                   EL155
00559      EXEC CICS HANDLE CONDITION                                   EL155
00560          PGMIDERR (8990-SEND-TEXT)                                EL155
00561      END-EXEC.                                                    EL155
00562                                                                   EL155
00563      MOVE SPACE                  TO PI-ENTRY-CD-1.                EL155
00564      MOVE CALL-PGM               TO PI-CALLING-PROGRAM LOGOFF-PGM EL155
00565      MOVE XCTL-EL005             TO CALL-PGM.                        CL**2
00566      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL155
00567      GO TO 9200-XCTL.                                             EL155
00568                                                                   EL155
00569  8990-SEND-TEXT.                                                  EL155
00570                                                                   EL155
00571      EXEC CICS SEND TEXT                                          EL155
00572          FROM    (LOGOFF-TEXT)                                    EL155
00573          LENGTH  (LOGOFF-LENGTH)                                  EL155
00574          ERASE                                                    EL155
00575          FREEKB                                                   EL155
00576      END-EXEC.                                                    EL155
00577                                                                   EL155
00578      GO TO 9100-RETURN-CICS.                                      EL155
00579                                                                      CL**2
00580      EJECT                                                        EL155
00581  9000-RETURN-TRANS.                                               EL155
00582                                                                   EL155
00583      EXEC CICS RETURN                                             EL155
00584          TRANSID   (TRANS-ID)                                     EL155
00585          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL155
00586          LENGTH    (PI-COMM-LENGTH)                               EL155
00587      END-EXEC.                                                    EL155
00588                                                                   EL155
00589      GOBACK.                                                      EL155
00590                                                                   EL155
00591  9100-RETURN-CICS.                                                EL155
00592                                                                   EL155
00593      EXEC CICS RETURN                                             EL155
00594      END-EXEC.                                                    EL155
00595                                                                   EL155
00596      GOBACK.                                                      EL155
00597                                                                   EL155
00598  9200-XCTL.                                                       EL155
00599                                                                   EL155
00600      EXEC CICS XCTL                                               EL155
00601          PROGRAM   (CALL-PGM)                                     EL155
00602          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL155
00603          LENGTH    (PI-COMM-LENGTH)                               EL155
00604      END-EXEC.                                                    EL155
00605                                                                   EL155
00606  9700-LINK-DATE-CONVERT.                                          EL155
00607                                                                   EL155
00608      EXEC CICS LINK                                               EL155
00609          PROGRAM    ('ELDATCV')                                   EL155
00610          COMMAREA   (DATE-CONVERSION-DATA)                        EL155
00611          LENGTH     (DC-COMM-LENGTH)                              EL155
00612          END-EXEC.                                                EL155
00613                                                                   EL155
00614  9700-EXIT.                                                       EL155
00615      EXIT.                                                        EL155
00616                                                                   EL155
00617  9900-ERROR-FORMAT.                                               EL155
00618                                                                   EL155
00619      IF NOT EMI-ERRORS-COMPLETE                                   EL155
00620          EXEC CICS LINK                                           EL155
00621              PROGRAM   ('EL001')                                  EL155
00622              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL155
00623              LENGTH    (EMI-COMM-LENGTH)                          EL155
00624          END-EXEC.                                                EL155
00625                                                                   EL155
00626  9900-ERROR-FORMAT-EXIT.                                          EL155
00627      EXIT.                                                        EL155
00628                                                                   EL155
00629  9990-ABEND.                                                      EL155
00630      MOVE DFHEIBLK TO EMI-LINE1.                                  EL155
00631                                                                   EL155
00632      EXEC CICS LINK                                               EL155
00633          PROGRAM   ('EL004')                                      EL155
00634          COMMAREA  (EMI-LINE1)                                    EL155
00635          LENGTH    (72)                                           EL155
00636      END-EXEC.                                                    EL155
00637                                                                   EL155
00638      GO TO 8110-SEND-DATA.                                        EL155
