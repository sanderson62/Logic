00001  IDENTIFICATION DIVISION.                                         05/14/96
00002                                                                   EL119
00003  PROGRAM-ID.                 EL119 .                                 LV005
00004 *              PROGRAM CONVERTED BY                                  CL**4
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**4
00006 *              CONVERSION DATE 02/13/96 09:58:28.                    CL**4
00007 *                            VMOD=2.005.                             CL**5
00008 *                                                                 EL119
00008 *                                                                 EL119
00009 *AUTHOR.        LOGIC,INC.                                           CL**4
00010 *               DALLAS, TEXAS.                                       CL**4
00011                                                                   EL119
00012 *DATE-COMPILED.                                                      CL**4
00013                                                                   EL119
00014 *SECURITY.   *****************************************************   CL**4
00015 *            *                                                   *   CL**4
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**4
00017 *            *                                                   *   CL**4
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**4
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**4
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**4
00021 *            *                                                   *   CL**4
00022 *            *****************************************************   CL**4
00023                                                                   EL119
00024 *REMARKS.                                                            CL**4
00025 *        TRANSACTION - EX44 - TERMINAL MAINTENANCE                   CL**4
00026                                                                   EL119
00027  ENVIRONMENT DIVISION.                                            EL119
00028  DATA DIVISION.                                                   EL119
00029  EJECT                                                            EL119
00030  WORKING-STORAGE SECTION.                                         EL119
00031  77  FILLER  PIC  X(32) VALUE '********************************'. EL119
00032  77  FILLER  PIC  X(32) VALUE '*    EL119 WORKING STORAGE     *'.    CL**2
00033  77  FILLER  PIC  X(32) VALUE '******** VMOD=2.005 ************'.    CL**5
00034                                                                   EL119
00035  01  WS-DATE-AREA.                                                EL119
00036      12  SAVE-DATE               PIC  X(8)  VALUE SPACES.         EL119
00037      12  SAVE-BIN-DATE           PIC  X(2)  VALUE SPACES.         EL119
00038                                                                   EL119
00039  01  STANDARD-AREAS.                                              EL119
00040      12  MAP-NAME                PIC  X(8).                       EL119
00041      12  MAP-EL119A              PIC  X(8)  VALUE 'EL119A'.       EL119
00042      12  MAPSET-EL119S           PIC  X(8)  VALUE 'EL119S'.       EL119
00043      12  TRAN-EX44               PIC  X(4)  VALUE 'EX44'.         EL119
00044      12  PGM-EL119               PIC  X(8)  VALUE 'EL119'.        EL119
00045      12  PGM-NAME                PIC  X(8).                       EL119
00046      12  DEEDIT-LENGTH           PIC S9(4)  VALUE +15    COMP.    EL119
00047      12  JRLR-ELCNTL-LENGTH      PIC S9(4)  VALUE +773   COMP.       CL**3
00048      12  TIME-IN                 PIC S9(7).                       EL119
00049      12  TIME-OUT-R REDEFINES TIME-IN.                            EL119
00050          16  FILLER              PIC  X.                          EL119
00051          16  TIME-OUT            PIC  9(2)V9(02).                 EL119
00052          16  FILLER              PIC  X(2).                       EL119
00053      12  XCTL-EL005              PIC  X(8)  VALUE 'EL005'.        EL119
00054      12  XCTL-EL010              PIC  X(8)  VALUE 'EL010'.        EL119
00055      12  XCTL-EL126              PIC  X(8)  VALUE 'EL126'.        EL119
00056      12  XCTL-EL626              PIC  X(8)  VALUE 'EL626'.        EL119
00057      12  XCTL-EM626              PIC  X(8)  VALUE 'EM626'.           CL**2
00058      12  XCTL-GL800              PIC  X(8)  VALUE 'GL800'.           CL**2
00059      12  LINK-EL001              PIC  X(8)  VALUE 'EL001'.        EL119
00060      12  LINK-EL004              PIC  X(8)  VALUE 'EL004'.        EL119
00061      12  LINK-ELDATCV            PIC  X(8)  VALUE 'ELDATCV'.      EL119
00062      12  FILE-ELCNTL             PIC  X(8)  VALUE 'ELCNTL'.       EL119
00063      12  ER-0000                 PIC  X(4)  VALUE '0000'.         EL119
00064      12  ER-0004                 PIC  X(4)  VALUE '0004'.         EL119
00065      12  ER-0007                 PIC  X(4)  VALUE '0007'.         EL119
00066      12  ER-0029                 PIC  X(4)  VALUE '0029'.         EL119
00067      12  ER-0042                 PIC  X(4)  VALUE '0042'.         EL119
00068      12  ER-0043                 PIC  X(4)  VALUE '0043'.         EL119
00069      12  ER-0062                 PIC  X(4)  VALUE '0062'.         EL119
00070      12  ER-0063                 PIC  X(4)  VALUE '0063'.         EL119
00071      12  ER-0068                 PIC  X(4)  VALUE '0068'.         EL119
00072      12  ER-0070                 PIC  X(4)  VALUE '0070'.         EL119
00073      12  ER-7008                 PIC  X(4)  VALUE '7008'.         EL119
00074      12  ER-7997                 PIC  X(4)  VALUE '7997'.         EL119
00075      12  ER-7998                 PIC  X(4)  VALUE '7998'.         EL119
00076  EJECT                                                            EL119
00077  01  MISC-WORK-AREAS.                                             EL119
00078      12  PRINTER-TEST-AREA.                                       EL119
00079          16  PTA-1               PIC  X.                          EL119
00080          16  PTA-2               PIC  X.                          EL119
00081          16  PTA-3               PIC  X.                          EL119
00082          16  PTA-4               PIC  X.                          EL119
00083      12  TERMINAL-TEST-AREA.                                      EL119
00084          16  TTA-1               PIC  X.                          EL119
00085          16  TTA-2               PIC  X.                          EL119
00086          16  TTA-3               PIC  X.                          EL119
00087          16  TTA-4               PIC  X.                          EL119
00088      12  DEEDIT-FIELD            PIC  X(15).                      EL119
00089      12  DEEDIT-FIELD-V0 REDEFINES                                EL119
00090          DEEDIT-FIELD            PIC S9(15).                      EL119
00091      12  DEEDIT-FIELD-V1 REDEFINES                                EL119
00092          DEEDIT-FIELD            PIC S9(13)V9(02).                EL119
00093      12  SUB1                    PIC S9(4)  VALUE +0     COMP.    EL119
00094      12  SUB2                    PIC S9(4)  VALUE +0     COMP.    EL119
00095      12  SUB3                    PIC S9(4)  VALUE +0     COMP.    EL119
00096      12  SUB4                    PIC S9(4)  VALUE +0     COMP.    EL119
00097                                                                   EL119
00098  01  ACCESS-KEYS.                                                 EL119
00099      12  ELCNTL-KEY.                                              EL119
00100          16  CK-COMPANY-ID       PIC  X(3).                       EL119
00101          16  CK-RECORD-TYPE      PIC  X.                          EL119
00102          16  FILLER              PIC  X(4)  VALUE SPACES.         EL119
00103          16  CK-SEQUENCE-NO      PIC S9(4)  VALUE +0     COMP.    EL119
00104                                                                   EL119
00105  01  WS-ELCNTL-KEY.                                               EL119
00106      12  WS-COMPANY-ID           PIC  X(3).                       EL119
00107      12  WS-RECORD-TYPE          PIC  X(2).                       EL119
00108      12  FILLER                  PIC  X(4)  VALUE SPACES.         EL119
00109      12  WS-SEQUENCE-NO          PIC S9(4)  VALUE +0     COMP.    EL119
00110                                                                   EL119
00111  01  CONTROL-FILE-SAVE-AREA.                                      EL119
00112      12  FILLER                  PIC  X(600).                     EL119
00113  EJECT                                                            EL119
00114                                  COPY ELCDATE.                       CL**4
00115  EJECT                                                            EL119
00116                                  COPY ELCLOGOF.                      CL**4
00117  EJECT                                                            EL119
00118                                  COPY ELCATTR.                       CL**4
00119  EJECT                                                            EL119
00120                                  COPY ELCEMIB.                       CL**4
00121  EJECT                                                            EL119
00122                                  COPY ELCINTF.                       CL**4
00123      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                   EL119
00124          16  PI-MAP-NAME         PIC  X(8).                       EL119
00125          16  FILLER              PIC  X(632).                        CL**4
00126  EJECT                                                            EL119
00127                                  COPY ELCJPFX.                       CL**4
00128                                  PIC  X(750).                        CL**3
00129  EJECT                                                            EL119
00130      COPY ELCAID.                                                    CL**5
00131                                                                   EL119
00132  01  FILLER REDEFINES DFHAID.                                     EL119
00133      12  FILLER                  PIC  X(8).                       EL119
00134      12  PF-VALUES               PIC  X      OCCURS 2.            EL119
00135                                                                   EL119
00136  01  WS-WORK-AREA-2              PIC  X(100) VALUE SPACES.        EL119
00137  EJECT                                                            EL119
00138                                  COPY EL119S.                        CL**4
00139  EJECT                                                            EL119
00140  01  EL119AO-R REDEFINES EL119AI.                                 EL119
00141      12  FILLER                  PIC  X(57).                      EL119
00142      12  TERM-OCCURS     OCCURS  120  TIMES.                      EL119
00143          16  TERMINALL           PIC S9(4)               COMP.    EL119
00144          16  TERMINALA           PIC  X.                          EL119
00145          16  TERMINALO           PIC  X(4).                       EL119
00146      12  FILLER                  PIC  X(80).                         CL**4
00147  EJECT                                                            EL119
00148  LINKAGE SECTION.                                                 EL119
00149  01  DFHCOMMAREA                 PIC  X(1024).                    EL119
00150                                                                   EL119
00151  EJECT                                                            EL119
00152      COPY ELCCNTL.                                                   CL**5
00153  EJECT                                                            EL119
00154  PROCEDURE DIVISION.                                              EL119
00155                                                                   EL119
00156      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL119
00157      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL119
00158      MOVE '5'                    TO  DC-OPTION-CODE.              EL119
00159                                                                   EL119
00160      PERFORM 8000-CONVERT-DATE  THRU  8099-EXIT.                  EL119
00161                                                                   EL119
00162      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL119
00163      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL119
00164                                                                   EL119
00165      IF EIBCALEN  = ZERO                                          EL119
00166          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL119
00167                                                                   EL119
00168      IF PI-CALLING-PROGRAM  NOT =  PGM-EL119                      EL119
00169          IF PI-RETURN-TO-PROGRAM  NOT = PGM-EL119                 EL119
00170              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6    EL119
00171              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5    EL119
00172              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4    EL119
00173              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3    EL119
00174              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2    EL119
00175              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1    EL119
00176              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM  EL119
00177              MOVE PGM-EL119             TO  PI-CALLING-PROGRAM    EL119
00178              MOVE MAP-EL119A            TO  PI-MAP-NAME           EL119
00179          ELSE                                                     EL119
00180              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM    EL119
00181              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM  EL119
00182              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1    EL119
00183              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2    EL119
00184              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3    EL119
00185              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4    EL119
00186              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5    EL119
00187              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.   EL119
00188                                                                   EL119
00189      IF EIBTRNID  NOT = TRAN-EX44                                 EL119
00190          GO TO 7000-BUILD-INITIAL-MAP.                            EL119
00191                                                                   EL119
00192      GO TO 0200-RECEIVE.                                          EL119
00193  EJECT                                                            EL119
00194  0200-RECEIVE.                                                    EL119
00195      EXEC CICS  HANDLE CONDITION                                  EL119
00196          NOTOPEN   (8870-NOTOPEN)                                 EL119
00197          NOTFND    (8880-NOT-FOUND)                               EL119
00198          PGMIDERR  (9600-PGMID-ERROR)                             EL119
00199          ERROR     (9999-ABEND)                                   EL119
00200          END-EXEC.                                                EL119
00201                                                                   EL119
00202      IF EIBAID  = DFHCLEAR                                        EL119
00203          GO TO 9400-CLEAR.                                        EL119
00204                                                                   EL119
00205      MOVE LOW-VALUES             TO  EL119AI.                     EL119
00206                                                                   EL119
00207      IF EIBAID  =  DFHPA1  OR  DFHPA2  OR  DFHPA3                 EL119
00208          MOVE ER-7008            TO  EMI-ERROR                    EL119
00209          PERFORM 9800-ERROR-FORMAT  THRU  9899-EXIT               EL119
00210          MOVE -1                 TO  TERM1L (1)                   EL119
00211          GO TO 8200-SEND-DATAONLY.                                EL119
00212                                                                   EL119
00213      EXEC CICS  RECEIVE                                           EL119
00214          MAP     (MAP-EL119A)                                     EL119
00215          MAPSET  (MAPSET-EL119S)                                  EL119
00216          INTO    (EL119AI)                                        EL119
00217          END-EXEC.                                                EL119
00218  EJECT                                                            EL119
00219      IF ENTERPFL  = ZERO                                          EL119
00220          GO TO 0300-CHECK-PFKEYS.                                 EL119
00221                                                                   EL119
00222      IF EIBAID  NOT = DFHENTER                                    EL119
00223          MOVE ER-0004            TO  EMI-ERROR                    EL119
00224          GO TO 0310-INPUT-ERROR.                                  EL119
00225                                                                   EL119
00226      IF ENTERPFI GREATER 0 AND LESS 25                            EL119
00227          MOVE PF-VALUES (ENTERPFI)  TO  EIBAID                    EL119
00228      ELSE                                                         EL119
00229          MOVE ER-0029               TO  EMI-ERROR                 EL119
00230          GO TO 0310-INPUT-ERROR.                                  EL119
00231  EJECT                                                            EL119
00232  0300-CHECK-PFKEYS.                                               EL119
00233      IF EIBAID  =            DFHPF23                              EL119
00234          GO TO 8810-PF23.                                         EL119
00235                                                                   EL119
00236      IF EIBAID  =            DFHPF24                              EL119
00237          GO TO 9200-RETURN-MAIN-MENU.                             EL119
00238                                                                   EL119
00239 ***************************************************************   EL119
00240 *   CHECK ALMIGHTY SECURITY TO SEE IF USER IS AUTHORIZED TO   *   EL119
00241 *   BE IN THIS SCREEN.                                        *   EL119
00242 ***************************************************************   EL119
00243                                                                   EL119
00244      IF PI-USER-ALMIGHTY-YES                                      EL119
00245          NEXT SENTENCE                                            EL119
00246      ELSE                                                         EL119
00247          MOVE ER-0007            TO  EMI-ERROR                    EL119
00248          PERFORM 9800-ERROR-FORMAT  THRU  9899-EXIT               EL119
00249          MOVE LOW-VALUES         TO  EL119AO                      EL119
00250          GO TO 7000-BUILD-INITIAL-MAP.                            EL119
00251                                                                   EL119
00252      IF EIBAID  = DFHPF12                                         EL119
00253          GO TO 9500-PF12.                                         EL119
00254                                                                   EL119
00255      IF EIBAID  = DFHENTER                                        EL119
00256          GO TO 0400-EDIT-DATA.                                    EL119
00257                                                                   EL119
00258      MOVE ER-0029                TO  EMI-ERROR.                   EL119
00259                                                                   EL119
00260  0310-INPUT-ERROR.                                                EL119
00261      PERFORM 9800-ERROR-FORMAT  THRU  9899-EXIT.                  EL119
00262                                                                   EL119
00263      MOVE AL-UNBON               TO  ENTERPFA.                    EL119
00264      MOVE -1                     TO  ENTERPFL.                    EL119
00265                                                                   EL119
00266      GO TO 8200-SEND-DATAONLY.                                    EL119
00267  EJECT                                                            EL119
00268  0400-EDIT-DATA.                                                  EL119
00269 *    IF SUPERVISOR-CAP                                            EL119
00270 *      OR  MODIFY-CAP                                             EL119
00271 *        NEXT SENTENCE                                            EL119
00272 *    ELSE                                                         EL119
00273 *        MOVE ER-0070            TO  EMI-ERROR                    EL119
00274 *        PERFORM 9800-ERROR-FORMAT  THRU  9899-EXIT               EL119
00275 *        GO TO 7000-BUILD-INITIAL-MAP.                            EL119
00276                                                                   EL119
00277      PERFORM 0500-EDIT-TERMINALS  THRU  0599-EXIT                 EL119
00278          VARYING  SUB1  FROM  1  BY  1                            EL119
00279              UNTIL  SUB1  IS GREATER THAN  +120.                  EL119
00280                                                                   EL119
00281      IF EMI-NO-ERRORS                                             EL119
00282          GO TO 4000-UPDATE-FILE.                                  EL119
00283                                                                   EL119
00284      GO TO 8200-SEND-DATAONLY.                                    EL119
00285                                                                   EL119
00286  0500-EDIT-TERMINALS.                                             EL119
00287      IF TERMINALL (SUB1) = ZERO                                   EL119
00288          GO TO 0599-EXIT.                                         EL119
00289                                                                   EL119
00290      MOVE TERMINALO (SUB1)       TO  TERMINAL-TEST-AREA.          EL119
00291                                                                   EL119
00292      IF TERMINAL-TEST-AREA = SPACES                               EL119
00293          MOVE AL-UANON           TO  TERMINALA (SUB1)             EL119
00294          GO TO 0599-EXIT.                                         EL119
00295                                                                   EL119
00296      IF ' '  =  TTA-1  OR  TTA-2  OR  TTA-3  OR  TTA-4            EL119
00297          MOVE -1                 TO  TERMINALL (SUB1)             EL119
00298          MOVE AL-UABON           TO  TERMINALA (SUB1)             EL119
00299          MOVE ER-0063            TO  EMI-ERROR                    EL119
00300          PERFORM 9800-ERROR-FORMAT  THRU  9899-EXIT               EL119
00301      ELSE                                                         EL119
00302          MOVE AL-UANON           TO  TERMINALA (SUB1).            EL119
00303                                                                   EL119
00304  0599-EXIT.                                                       EL119
00305      EXIT.                                                        EL119
00306  EJECT                                                            EL119
00307  EJECT                                                            EL119
00308  4000-UPDATE-FILE.                                                EL119
00309      MOVE PI-COMPANY-ID          TO  CK-COMPANY-ID.               EL119
00310      MOVE '9'                    TO  CK-RECORD-TYPE.              EL119
00311                                                                   EL119
00312      EXEC CICS  READ                                              EL119
00313          UPDATE                                                   EL119
00314          DATASET  (FILE-ELCNTL)                                   EL119
00315          SET      (ADDRESS OF CONTROL-FILE)                          CL**4
00316          RIDFLD   (ELCNTL-KEY)                                    EL119
00317          END-EXEC.                                                EL119
00318                                                                   EL119
00319      IF CF-LAST-MAINT-BY  NOT =  PI-UPDATE-BY                     EL119
00320        OR  CF-LAST-MAINT-HHMMSS                                   EL119
00321                NOT = PI-UPDATE-HHMMSS                             EL119
00322          EXEC CICS  UNLOCK                                        EL119
00323              DATASET  (FILE-ELCNTL)                               EL119
00324              END-EXEC                                             EL119
00325          MOVE ER-0068            TO  EMI-ERROR                    EL119
00326          PERFORM 9800-ERROR-FORMAT  THRU  9899-EXIT               EL119
00327          GO TO 7000-BUILD-INITIAL-MAP.                            EL119
00328                                                                   EL119
00329      MOVE 'B'                    TO  JP-RECORD-TYPE.              EL119
00330      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL119
00331                                                                   EL119
00332      PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.            EL119
00333                                                                   EL119
00334      PERFORM 4200-MOVE-TERMINALS-TO-FILE  THRU  4299-EXIT         EL119
00335          VARYING  SUB1  FROM  1  BY  1                            EL119
00336              UNTIL  SUB1  IS GREATER THAN  +120.                  EL119
00337                                                                   EL119
00338      MOVE +1                     TO  SUB2.                        EL119
00339                                                                   EL119
00340      PERFORM 4400-SQUEEZE-TERMINALS  THRU  4499-EXIT              EL119
00341          VARYING  SUB1  FROM  1  BY  1                            EL119
00342              UNTIL  SUB1  IS GREATER THAN  +120.                  EL119
00343                                                                   EL119
00344      PERFORM 4600-BLANK-REMAINING-TERMINALS  THRU  4699-EXIT      EL119
00345          UNTIL  SUB2  IS GREATER THAN  +120.                      EL119
00346                                                                   EL119
00347      MOVE PI-PROCESSOR-ID        TO  CF-LAST-MAINT-BY             EL119
00348                                      PI-UPDATE-BY.                EL119
00349      MOVE EIBTIME                TO  CF-LAST-MAINT-HHMMSS         EL119
00350                                      PI-UPDATE-HHMMSS.            EL119
00351      MOVE SAVE-BIN-DATE          TO  CF-LAST-MAINT-DT.            EL119
00352      MOVE 'C'                    TO  JP-RECORD-TYPE.              EL119
00353      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL119
00354  EJECT                                                            EL119
00355      EXEC CICS  REWRITE                                           EL119
00356          DATASET  (FILE-ELCNTL)                                   EL119
00357          FROM     (CONTROL-FILE)                                  EL119
00358          END-EXEC.                                                EL119
00359                                                                   EL119
00360      PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.            EL119
00361                                                                   EL119
00362      MOVE ER-0000                TO  EMI-ERROR.                   EL119
00363                                                                   EL119
00364      PERFORM 9800-ERROR-FORMAT  THRU  9899-EXIT.                  EL119
00365                                                                   EL119
00366      GO TO 7000-BUILD-INITIAL-MAP.                                EL119
00367  EJECT                                                            EL119
00368  4200-MOVE-TERMINALS-TO-FILE.                                     EL119
00369      IF TERMINALL (SUB1)  NOT = ZERO                              EL119
00370          MOVE TERMINALO (SUB1)   TO  CF-TERMINAL-ID (SUB1).       EL119
00371                                                                   EL119
00372  4299-EXIT.                                                       EL119
00373      EXIT.                                                        EL119
00374                                                                   EL119
00375  4400-SQUEEZE-TERMINALS.                                          EL119
00376      IF CF-TERMINAL-ID (SUB1)  NOT = SPACES                       EL119
00377          MOVE CF-TERMINAL-ID (SUB1)  TO  CF-TERMINAL-ID (SUB2)    EL119
00378          ADD +1                      TO  SUB2.                    EL119
00379                                                                   EL119
00380  4499-EXIT.                                                       EL119
00381      EXIT.                                                        EL119
00382                                                                   EL119
00383  4600-BLANK-REMAINING-TERMINALS.                                  EL119
00384      MOVE SPACES                 TO  CF-TERMINAL-ID (SUB2).       EL119
00385                                                                   EL119
00386      ADD +1                      TO  SUB2.                        EL119
00387                                                                   EL119
00388  4699-EXIT.                                                       EL119
00389      EXIT.                                                        EL119
00390  EJECT                                                            EL119
00391  7000-BUILD-INITIAL-MAP.                                          EL119
00392      MOVE LOW-VALUES             TO  EL119AO.                     EL119
00393      MOVE PI-COMPANY-ID          TO  CK-COMPANY-ID.               EL119
00394      MOVE '9'                    TO  CK-RECORD-TYPE.              EL119
00395                                                                   EL119
00396      EXEC CICS  READ                                              EL119
00397          DATASET  (FILE-ELCNTL)                                   EL119
00398          SET      (ADDRESS OF CONTROL-FILE)                          CL**4
00399          RIDFLD   (ELCNTL-KEY)                                    EL119
00400          END-EXEC.                                                EL119
00401                                                                   EL119
00402      MOVE CF-LAST-MAINT-BY       TO  LSTUSRO                      EL119
00403                                      PI-UPDATE-BY.                EL119
00404      MOVE ' '                    TO  DC-OPTION-CODE.              EL119
00405      MOVE CF-LAST-MAINT-DT       TO  DC-BIN-DATE-1.               EL119
00406                                                                   EL119
00407      PERFORM 8000-CONVERT-DATE  THRU  8099-EXIT.                  EL119
00408                                                                   EL119
00409      IF DATE-CONVERSION-ERROR                                     EL119
00410          MOVE ZEROS                TO  LSTDTEO                    EL119
00411      ELSE                                                         EL119
00412          MOVE DC-GREG-DATE-1-EDIT  TO  LSTDTEO.                   EL119
00413                                                                   EL119
00414      MOVE CF-LAST-MAINT-HHMMSS   TO  TIME-IN                      EL119
00415                                      PI-UPDATE-HHMMSS.            EL119
00416      MOVE TIME-OUT               TO  LSTTIMEO.                    EL119
00417                                                                   EL119
00418      PERFORM 7500-LOAD-TERMINALS  THRU  7599-EXIT                 EL119
00419          VARYING  SUB1  FROM  1  BY  1                            EL119
00420              UNTIL  SUB1  IS GREATER THAN  +120.                  EL119
00421                                                                   EL119
00422      MOVE -1                     TO  TERM1L (1).                  EL119
00423                                                                   EL119
00424      GO TO 8100-SEND-INITIAL-MAP.                                 EL119
00425                                                                   EL119
00426  7500-LOAD-TERMINALS.                                             EL119
00427      IF CF-TERMINAL-ID (SUB1)  =  SPACES                          EL119
00428          MOVE +121               TO  SUB1                         EL119
00429          GO TO 7599-EXIT.                                         EL119
00430                                                                   EL119
00431      MOVE CF-TERMINAL-ID (SUB1)  TO  TERMINALO (SUB1).            EL119
00432                                                                   EL119
00433  7599-EXIT.                                                       EL119
00434      EXIT.                                                        EL119
00435  EJECT                                                            EL119
00436  8000-CONVERT-DATE.                                               EL119
00437      MOVE LINK-ELDATCV           TO  PGM-NAME.                    EL119
00438                                                                   EL119
00439      EXEC CICS  LINK                                              EL119
00440          PROGRAM   (PGM-NAME)                                     EL119
00441          COMMAREA  (DATE-CONVERSION-DATA)                         EL119
00442          LENGTH    (DC-COMM-LENGTH)                               EL119
00443          END-EXEC.                                                EL119
00444                                                                   EL119
00445  8099-EXIT.                                                       EL119
00446      EXIT.                                                        EL119
00447  EJECT                                                            EL119
00448  8100-SEND-INITIAL-MAP.                                           EL119
00449      MOVE SAVE-DATE              TO  RUNDATEO.                    EL119
00450      MOVE EIBTIME                TO  TIME-IN.                     EL119
00451      MOVE TIME-OUT               TO  RUNTIMEO.                    EL119
00452      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSGO.                     EL119
00453                                                                   EL119
00454      EXEC CICS  SEND                                              EL119
00455          MAP     (MAP-EL119A)                                     EL119
00456          MAPSET  (MAPSET-EL119S)                                  EL119
00457          FROM    (EL119AO)                                        EL119
00458          ERASE                                                    EL119
00459          CURSOR                                                   EL119
00460          END-EXEC.                                                EL119
00461                                                                   EL119
00462      GO TO 9100-RETURN-TRAN.                                      EL119
00463  EJECT                                                            EL119
00464  8200-SEND-DATAONLY.                                              EL119
00465      MOVE SAVE-DATE              TO  RUNDATEO.                    EL119
00466      MOVE EIBTIME                TO  TIME-IN.                     EL119
00467      MOVE TIME-OUT               TO  RUNTIMEO.                    EL119
00468      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSGO.                     EL119
00469                                                                   EL119
00470      EXEC CICS  SEND                                              EL119
00471          MAP     (MAP-EL119A)                                     EL119
00472          MAPSET  (MAPSET-EL119S)                                  EL119
00473          FROM    (EL119AO)                                        EL119
00474          DATAONLY                                                 EL119
00475          CURSOR                                                   EL119
00476          END-EXEC.                                                EL119
00477                                                                   EL119
00478      GO TO 9100-RETURN-TRAN.                                      EL119
00479  EJECT                                                            EL119
00480  8300-SEND-TEXT.                                                  EL119
00481      EXEC CICS  SEND TEXT                                         EL119
00482          FROM    (LOGOFF-TEXT)                                    EL119
00483          LENGTH  (LOGOFF-LENGTH)                                  EL119
00484          ERASE                                                    EL119
00485          FREEKB                                                   EL119
00486          END-EXEC.                                                EL119
00487                                                                   EL119
00488      EXEC CICS  RETURN                                            EL119
00489          END-EXEC.                                                EL119
00490  EJECT                                                            EL119
00491  8400-LOG-JOURNAL-RECORD.                                         EL119
00492      IF PI-JOURNAL-FILE-ID =  ZERO                                EL119
00493          GO TO 8499-EXIT.                                         EL119
00494                                                                   EL119
00495      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                  EL119
00496      MOVE FILE-ELCNTL            TO  JP-FILE-ID.                  EL119
00497      MOVE PGM-EL119              TO  JP-PROGRAM-ID.               EL119
00498                                                                   EL119
00499      EXEC CICS  JOURNAL                                           EL119
00500          JFILEID  (PI-JOURNAL-FILE-ID)                            EL119
00501          JTYPEID  ('EL')                                          EL119
00502          FROM     (JOURNAL-RECORD)                                EL119
00503          LENGTH   (JRLR-ELCNTL-LENGTH)                            EL119
00504          END-EXEC.                                                EL119
00505                                                                   EL119
00506  8499-EXIT.                                                       EL119
00507      EXIT.                                                        EL119
00508                                                                   EL119
00509  8800-UNAUTHORIZED-ACCESS.                                        EL119
00510      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL119
00511                                                                   EL119
00512      GO TO 8300-SEND-TEXT.                                        EL119
00513                                                                   EL119
00514  8810-PF23.                                                       EL119
00515      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL119
00516      MOVE XCTL-EL005             TO  PGM-NAME.                    EL119
00517                                                                   EL119
00518      GO TO 9300-XCTL.                                             EL119
00519                                                                   EL119
00520  8870-NOTOPEN.                                                    EL119
00521      MOVE ER-0042                TO  EMI-ERROR.                   EL119
00522                                                                   EL119
00523      PERFORM 9800-ERROR-FORMAT  THRU  9899-EXIT.                  EL119
00524                                                                   EL119
00525      MOVE -1                     TO  ENTERPFL.                    EL119
00526                                                                   EL119
00527      IF EIBTRNID  NOT = TRAN-EX44                                 EL119
00528          GO TO 8100-SEND-INITIAL-MAP.                             EL119
00529                                                                   EL119
00530      GO TO 8200-SEND-DATAONLY.                                    EL119
00531  EJECT                                                            EL119
00532  8880-NOT-FOUND.                                                  EL119
00533      MOVE ER-0043                TO  EMI-ERROR.                   EL119
00534                                                                   EL119
00535      PERFORM 9800-ERROR-FORMAT  THRU  9899-EXIT.                  EL119
00536                                                                   EL119
00537      MOVE -1                     TO  ENTERPFL.                    EL119
00538                                                                   EL119
00539      IF EIBTRNID  NOT = TRAN-EX44                                 EL119
00540          GO TO 8100-SEND-INITIAL-MAP.                             EL119
00541                                                                   EL119
00542      GO TO 8200-SEND-DATAONLY.                                    EL119
00543                                                                   EL119
00544  9100-RETURN-TRAN.                                                EL119
00545      MOVE EMI-ERROR-NUMBER (1)  TO  PI-LAST-ERROR-NO.             EL119
00546      MOVE MAP-EL119A            TO  PI-CURRENT-SCREEN-NO.         EL119
00547                                                                   EL119
00548      EXEC CICS  RETURN                                            EL119
00549          TRANSID   (TRAN-EX44)                                    EL119
00550          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL119
00551          LENGTH    (PI-COMM-LENGTH)                               EL119
00552          END-EXEC.                                                EL119
00553  EJECT                                                            EL119
00554  9200-RETURN-MAIN-MENU.                                           EL119
00555                                                                   EL119
00556      IF  CREDIT-SESSION                                              CL**2
00557          MOVE XCTL-EL626         TO PGM-NAME                         CL**2
00558                                                                   EL119
00559      ELSE                                                            CL**2
00560          IF  CLAIM-SESSION                                           CL**2
00561              MOVE XCTL-EL126     TO PGM-NAME                         CL**2
00562                                                                      CL**2
00563          ELSE                                                        CL**2
00564              IF  MORTGAGE-SESSION                                    CL**2
00565                  MOVE XCTL-EM626 TO PGM-NAME                         CL**2
00566                                                                      CL**2
00567              ELSE                                                    CL**2
00568                  IF  GENERAL-LEDGER-SESSION                          CL**2
00569                      MOVE XCTL-GL800                                 CL**2
00570                                  TO PGM-NAME.                        CL**2
00571                                                                   EL119
00572      GO TO 9300-XCTL.                                             EL119
00573                                                                   EL119
00574  9300-XCTL.                                                       EL119
00575      EXEC CICS  XCTL                                              EL119
00576          PROGRAM   (PGM-NAME)                                     EL119
00577          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL119
00578          LENGTH    (PI-COMM-LENGTH)                               EL119
00579          END-EXEC.                                                EL119
00580  EJECT                                                            EL119
00581  9400-CLEAR.                                                      EL119
00582      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                    EL119
00583                                                                   EL119
00584      GO TO 9300-XCTL.                                             EL119
00585                                                                   EL119
00586  9500-PF12.                                                       EL119
00587      MOVE XCTL-EL010             TO  PGM-NAME.                    EL119
00588                                                                   EL119
00589      GO TO 9300-XCTL.                                             EL119
00590                                                                   EL119
00591  9600-PGMID-ERROR.                                                EL119
00592      EXEC CICS  HANDLE CONDITION                                  EL119
00593          PGMIDERR  (8300-SEND-TEXT)                               EL119
00594          END-EXEC.                                                EL119
00595                                                                   EL119
00596      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          EL119
00597      MOVE ' '                    TO  PI-ENTRY-CD-1.               EL119
00598      MOVE XCTL-EL005             TO  PGM-NAME.                    EL119
00599      MOVE PGM-NAME               TO  LOGOFF-PGM.                  EL119
00600      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL119
00601                                                                   EL119
00602      GO TO 9300-XCTL.                                             EL119
00603  EJECT                                                            EL119
00604  9800-ERROR-FORMAT.                                               EL119
00605      IF NOT  EMI-ERRORS-COMPLETE                                  EL119
00606          MOVE LINK-EL001         TO  PGM-NAME                     EL119
00607          EXEC CICS  LINK                                          EL119
00608              PROGRAM   (PGM-NAME)                                 EL119
00609              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL119
00610              LENGTH    (EMI-COMM-LENGTH)                          EL119
00611              END-EXEC.                                            EL119
00612                                                                   EL119
00613  9899-EXIT.                                                       EL119
00614      EXIT.                                                        EL119
00615                                                                   EL119
00616  9999-ABEND.                                                      EL119
00617      MOVE LINK-EL004             TO  PGM-NAME.                    EL119
00618      MOVE DFHEIBLK               TO  EMI-LINE1.                   EL119
00619                                                                   EL119
00620      EXEC CICS  LINK                                              EL119
00621          PROGRAM   (PGM-NAME)                                     EL119
00622          COMMAREA  (EMI-LINE1)                                    EL119
00623          LENGTH    (72)                                           EL119
00624          END-EXEC.                                                EL119
00625                                                                   EL119
00626      GO TO 8200-SEND-DATAONLY.                                    EL119
00627                                                                   EL119
