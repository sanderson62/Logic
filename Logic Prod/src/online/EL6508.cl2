00001  ID DIVISION.                                                     02/26/96
00002                                                                   EL6508
00003  PROGRAM-ID.                 EL6508.                                 LV002
00004 *              PROGRAM CONVERTED BY                                  CL**2
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**2
00006 *              CONVERSION DATE 02/12/96 08:32:52.                    CL**2
00007 *                            VMOD=2.002                              CL**2
00008 *                                                                 EL6508
00008 *                                                                 EL6508
00009 *AUTHOR.     LOGIC,INC.                                              CL**2
00010 *            DALLAS, TEXAS.                                          CL**2
00011                                                                   EL6508
00012 *DATE-COMPILED.                                                      CL**2
00013 *SECURITY.   *****************************************************   CL**2
00014 *            *                                                   *   CL**2
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**2
00016 *            *                                                   *   CL**2
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**2
00018 *                                                                *   CL**2
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**2
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**2
00021 *            *                                                   *   CL**2
00022 *            *****************************************************   CL**2
00023                                                                   EL6508
00024 *REMARKS.    TRANSACTION - EXD9 - ACCOUNT MAINT (CLIENT ADDL).       CL**2
00025                                                                   EL6508
00026  ENVIRONMENT DIVISION.                                            EL6508
00027                                                                   EL6508
00028      EJECT                                                        EL6508
00029  DATA DIVISION.                                                   EL6508
00030  WORKING-STORAGE SECTION.                                         EL6508
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL6508
00032  77  FILLER  PIC X(32)  VALUE '*    EL6508 WORKING STORAGE    *'. EL6508
00033  77  FILLER  PIC X(32)  VALUE '********* VMOD=2.002 ***********'.    CL**2
00034                                                                   EL6508
00035                                                                   EL6508
00036  01  WS-COMM-LENGTH          PIC S9(4)   COMP VALUE +1500.        EL6508
00037  01  WS-DATE-AREA.                                                EL6508
00038      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL6508
00039      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL6508
00040                                                                   EL6508
00041  01  STANDARD-AREAS.                                              EL6508
00042      12  MAP-NAME            PIC X(8)    VALUE 'EL6508A '.        EL6508
00043      12  MAPSET-NAME         PIC X(8)    VALUE 'EL6508S '.        EL6508
00044      12  SCREEN-NUMBER       PIC X(4)    VALUE '650I'.            EL6508
00045      12  TRANS-ID            PIC X(4)    VALUE 'EXD9'.            EL6508
00046      12  THIS-PGM            PIC X(8)    VALUE 'EL6508  '.        EL6508
00047      12  PGM-NAME            PIC X(8).                            EL6508
00048      12  TIME-IN             PIC S9(7).                           EL6508
00049      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL6508
00050          16  FILLER          PIC X.                               EL6508
00051          16  TIME-OUT        PIC 99V99.                           EL6508
00052          16  FILLER          PIC X(2).                            EL6508
00053      12  XCTL-005            PIC X(8)    VALUE 'EL005   '.        EL6508
00054      12  XCTL-010            PIC X(8)    VALUE 'EL050   '.        EL6508
00055      12  XCTL-626            PIC X(8)    VALUE 'EL626   '.        EL6508
00056      12  XCTL-650            PIC X(8)    VALUE 'EL650   '.        EL6508
00057      12  XCTL-6503           PIC X(8)    VALUE 'EL6503  '.        EL6508
00058      12  XCTL-6502           PIC X(8)    VALUE 'EL6502  '.        EL6508
00059      12  XCTL-6501           PIC X(8)    VALUE 'EL6501  '.        EL6508
00060      12  LINK-001            PIC X(8)    VALUE 'EL001   '.        EL6508
00061      12  LINK-004            PIC X(8)    VALUE 'EL004   '.        EL6508
00062      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV '.        EL6508
00063      12  FILE-ID             PIC X(8)    VALUE SPACES.            EL6508
00064      12  ERACCT-FILE         PIC X(8)    VALUE 'ERACCT  '.        EL6508
00065      12  CNTL-FILE-ID        PIC X(8)    VALUE 'ELCNTL  '.        EL6508
00066      12  BIN-CURRENT-SAVE    PIC XX      VALUE SPACES.            EL6508
00067      12  YMD-CURRENT-SAVE    PIC X(6)    VALUE SPACES.            EL6508
00068      12  SC-ITEM             PIC S9(4)   VALUE +1 COMP.           EL6508
00069                                                                   EL6508
00070      12  SUB1                PIC S9(4)   VALUE +0  COMP.          EL6508
00071      12  SUB2                PIC S9(4)   VALUE +0  COMP.          EL6508
00072      12  AGT-INDX-MAX        PIC S9(4)   VALUE +10 COMP.          EL6508
00073                                                                   EL6508
00074      12  DEEDIT-FIELD        PIC X(15).                           EL6508
00075      12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).      EL6508
00076      12  DEEDIT-FIELD-V1  REDEFINES DEEDIT-FIELD PIC S9(13)V99.   EL6508
00077                                                                   EL6508
00078      12  WS-EDIT-FIELD-CONV      PIC S9(4)   VALUE +0.            EL6508
00079                                                                   EL6508
00080      12  ELCNTL-KEY.                                              EL6508
00081          16  CNTL-COMP-ID        PIC X(3)    VALUE SPACES.        EL6508
00082          16  CNTL-REC-TYPE       PIC X       VALUE SPACES.        EL6508
00083          16  CNTL-ACCESS         PIC X(4)    VALUE SPACES.        EL6508
00084          16  CNTL-SEQ-NO         PIC S9(4)   VALUE +0  COMP.      EL6508
00085                                                                   EL6508
00086      12  WS-SAVE-FIELDS.                                          EL6508
00087          16  WS-SAVE-BANK-BAL    PIC S9(9)V99 VALUE +0.           EL6508
00088          16  WS-SAVE-BANK-PREM   PIC S9(9)V99 VALUE +0.           EL6508
00089          16  WS-SAVE-BANK-AMT    PIC S9(9)V99 VALUE +0.           EL6508
00090                                                                   EL6508
00091          16  WS-SAVE-SHR-PCT OCCURS 10 TIMES                      EL6508
00092                                  PIC S9V99.                       EL6508
00093                                                                   EL6508
00094  01  ERROR-MESSAGES.                                              EL6508
00095      12  ER-0000             PIC X(4)        VALUE '0000'.        EL6508
00096      12  ER-0002             PIC X(4)        VALUE '0002'.        EL6508
00097      12  ER-0004             PIC X(4)        VALUE '0004'.        EL6508
00098      12  ER-0008             PIC X(4)        VALUE '0008'.        EL6508
00099      12  ER-0029             PIC X(4)        VALUE '0029'.        EL6508
00100      12  ER-0068             PIC X(4)        VALUE '0068'.        EL6508
00101      12  ER-0070             PIC X(4)        VALUE '0070'.        EL6508
00102      12  ER-2039             PIC X(4)        VALUE '2039'.        EL6508
00103      12  ER-2387             PIC X(4)        VALUE '2387'.        EL6508
00104      12  ER-2388             PIC X(4)        VALUE '2388'.        EL6508
00105      12  ER-2389             PIC X(4)        VALUE '2389'.        EL6508
00106      12  ER-2390             PIC X(4)        VALUE '2390'.        EL6508
00107      12  ER-2572             PIC X(4)        VALUE '2572'.        EL6508
00108                                                                   EL6508
00109      EJECT                                                        EL6508
00110                              COPY ELCSCTM SUPPRESS.               EL6508
00111                                                                   EL6508
00112                              COPY ELCSCRTY SUPPRESS.              EL6508
00113                                                                   EL6508
00114                              COPY ELCDATE SUPPRESS.               EL6508
00115                                                                   EL6508
00116                              COPY ELCLOGOF SUPPRESS.              EL6508
00117                                                                   EL6508
00118                              COPY ELCATTR SUPPRESS.               EL6508
00119                                                                   EL6508
00120                              COPY ELCEMIB SUPPRESS.               EL6508
00121                                                                   EL6508
00122                              COPY ELCINTF SUPPRESS.               EL6508
00123                              COPY ELC650PI SUPPRESS.              EL6508
00124      EJECT                                                        EL6508
00125                              COPY ELCJPFX SUPPRESS.               EL6508
00126                              PIC X(2000).                         EL6508
00127                                                                   EL6508
00128      EJECT                                                        EL6508
00129                              COPY ELCAID SUPPRESS.                EL6508
00130  01  FILLER    REDEFINES DFHAID.                                  EL6508
00131      12  FILLER              PIC X(8).                            EL6508
00132      12  PF-VALUES           PIC X       OCCURS 2.                EL6508
00133                                                                   EL6508
00134      EJECT                                                        EL6508
00135                                  COPY EL6508S.                    EL6508
00136  01  EL6508AO-R  REDEFINES EL6508AI.                              EL6508
00137      12  FILLER                  PIC X(155).                      EL6508
00138      12  AGENT-TABLE             OCCURS 10 TIMES                  EL6508
00139                                  INDEXED BY AGT-INDX.             EL6508
00140          16  ACCT-AGT-L          PIC S9(4)       COMP.            EL6508
00141          16  ACCT-AGT-A          PIC X.                           EL6508
00142          16  ACCT-AGT            PIC X(9).                        EL6508
00143          16  ACCT-ACCT-L         PIC S9(4)       COMP.            EL6508
00144          16  ACCT-ACCT-A         PIC X.                           EL6508
00145          16  ACCT-ACCT           PIC X.                           EL6508
00146          16  ACCT-SHRPCT-L       PIC S9(4)       COMP.            EL6508
00147          16  ACCT-SHRPCT-A       PIC X.                           EL6508
00148          16  ACCT-SHRPCT         PIC S9(4).                       EL6508
00149          16  ACCT-SHPCT          REDEFINES                        EL6508
00150              ACCT-SHRPCT         PIC Z.99.                        EL6508
00151                                                                   EL6508
00152      EJECT                                                        EL6508
00153  LINKAGE SECTION.                                                 EL6508
00154  01  DFHCOMMAREA             PIC X(1500).                         EL6508
00155                                                                   EL6508
00156      EJECT                                                        EL6508
00157 *01 PARMLIST .                                                       CL**2
00158 *    02  FILLER              PIC S9(8)   COMP.                       CL**2
00159 *    02  ERACCT-POINTER      PIC S9(8)   COMP.                       CL**2
00160 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                       CL**2
00161                                                                   EL6508
00162                              COPY ERCACCT SUPPRESS.               EL6508
00163      EJECT                                                        EL6508
00164                              COPY ELCCNTL SUPPRESS.               EL6508
00165      EJECT                                                        EL6508
00166                                                                   EL6508
00167  PROCEDURE DIVISION.                                              EL6508
00168      CONTINUE.                                                       CL**2
00169                                                                   EL6508
00170      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL6508
00171      MOVE '5'                   TO DC-OPTION-CODE.                EL6508
00172      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL6508
00173      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL6508
00174      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL6508
00175      MOVE DC-GREG-DATE-1-YMD    TO  YMD-CURRENT-SAVE.             EL6508
00176                                                                   EL6508
00177      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL6508
00178      IF EIBCALEN = 0                                              EL6508
00179          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL6508
00180                                                                   EL6508
00181      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL6508
00182          MOVE THIS-PGM             TO PI-CALLING-PROGRAM.         EL6508
00183                                                                   EL6508
00184      MOVE LOW-VALUES             TO EL6508AI.                     EL6508
00185                                                                   EL6508
00186      IF EIBTRNID NOT = TRANS-ID                                   EL6508
00187          MOVE PI-MAINT TO MAINTYPO                                EL6508
00188          MOVE AL-UANON TO MAINTYPA                                EL6508
00189          MOVE -1       TO MAINTYPL                                EL6508
00190                                                                   EL6508
00191          IF PI-MAINT EQUAL 'S' OR 'C'                             EL6508
00192              GO TO 4000-SHOW                                      EL6508
00193          ELSE                                                     EL6508
00194              IF PI-MAINT EQUAL 'A'                                EL6508
00195                  MOVE 'C' TO PI-MAINT                             EL6508
00196                  GO TO 4000-SHOW                                  EL6508
00197              ELSE                                                 EL6508
00198                  GO TO 8100-SEND-INITIAL-MAP.                     EL6508
00199                                                                   EL6508
00200      EXEC CICS HANDLE CONDITION                                   EL6508
00201          PGMIDERR  (9600-PGMID-ERROR)                             EL6508
00202          ERROR     (9990-ABEND)                                   EL6508
00203          END-EXEC.                                                EL6508
00204                                                                   EL6508
00205      IF EIBAID = DFHCLEAR                                         EL6508
00206          GO TO 9400-CLEAR.                                        EL6508
00207                                                                   EL6508
00208      EJECT                                                        EL6508
00209                                                                   EL6508
00210  0200-RECEIVE.                                                    EL6508
00211      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL6508
00212          MOVE ER-0008            TO EMI-ERROR                     EL6508
00213          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6508
00214          MOVE -1                 TO PFENTERL                      EL6508
00215          GO TO 8200-SEND-DATAONLY.                                EL6508
00216      EXEC CICS RECEIVE                                            EL6508
00217          MAP      (MAP-NAME)                                      EL6508
00218          MAPSET   (MAPSET-NAME)                                   EL6508
00219          INTO     (EL6508AI)                                      EL6508
00220          END-EXEC.                                                EL6508
00221                                                                   EL6508
00222      IF PFENTERL = 0                                              EL6508
00223          GO TO 0300-CHECK-PFKEYS.                                 EL6508
00224      IF EIBAID NOT = DFHENTER                                     EL6508
00225          MOVE ER-0004            TO EMI-ERROR                     EL6508
00226          GO TO 0320-INPUT-ERROR.                                  EL6508
00227      IF (PFENTERI NUMERIC) AND (PFENTERI > 0 AND < 25)            EL6508
00228          MOVE PF-VALUES (PFENTERI) TO EIBAID                      EL6508
00229      ELSE                                                         EL6508
00230          MOVE ER-0029            TO EMI-ERROR                     EL6508
00231          GO TO 0320-INPUT-ERROR.                                  EL6508
00232      EJECT                                                        EL6508
00233  0300-CHECK-PFKEYS.                                               EL6508
00234      IF EIBAID = DFHPF23                                          EL6508
00235          GO TO 8810-PF23.                                         EL6508
00236      IF EIBAID = DFHPF24                                          EL6508
00237          GO TO 9200-RETURN-MAIN-MENU.                             EL6508
00238      IF EIBAID = DFHPF12                                          EL6508
00239          GO TO 9500-PF12.                                         EL6508
00240      IF EIBAID = DFHPF7                                           EL6508
00241          MOVE XCTL-6501          TO PGM-NAME                      EL6508
00242          GO TO 9300-XCTL.                                         EL6508
00243      IF EIBAID = DFHENTER                                         EL6508
00244          GO TO 0330-CHECK-MAINTYP.                                EL6508
00245      MOVE ER-0029                TO EMI-ERROR.                    EL6508
00246  0320-INPUT-ERROR.                                                EL6508
00247      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6508
00248      MOVE AL-UNBON               TO PFENTERA.                     EL6508
00249      MOVE -1                     TO PFENTERL.                     EL6508
00250      GO TO 8200-SEND-DATAONLY.                                    EL6508
00251                                                                   EL6508
00252  0330-CHECK-MAINTYP.                                              EL6508
00253                                                                   EL6508
00254      IF MAINTYPL GREATER THAN ZERO                                EL6508
00255          IF MAINTYPI EQUAL 'S' OR 'C' OR 'A'                      EL6508
00256              MOVE AL-UANON TO MAINTYPA                            EL6508
00257              MOVE MAINTYPI TO PI-MAINT                            EL6508
00258          ELSE                                                     EL6508
00259              MOVE -1 TO MAINTYPL                                  EL6508
00260              MOVE AL-UABON TO MAINTYPA                            EL6508
00261              MOVE ER-2039 TO EMI-ERROR                            EL6508
00262              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6508
00263              GO TO 8200-SEND-DATAONLY                             EL6508
00264      ELSE                                                         EL6508
00265          MOVE -1 TO MAINTYPL                                      EL6508
00266          MOVE AL-UABON TO MAINTYPA                                EL6508
00267          MOVE ER-2039 TO EMI-ERROR                                EL6508
00268          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6508
00269          GO TO 8200-SEND-DATAONLY.                                EL6508
00270                                                                   EL6508
00271      IF PI-MAINT EQUAL 'S'                                        EL6508
00272          GO TO 4000-SHOW.                                         EL6508
00273                                                                   EL6508
00274      PERFORM 7800-COMPANY-REC-READ THRU 7899-EXIT.                EL6508
00275      IF EMI-ERROR NOT = ZEROS                                     EL6508
00276          MOVE -1                 TO MAINTYPL                      EL6508
00277          GO TO 8200-SEND-DATAONLY.                                EL6508
00278                                                                   EL6508
00279      GO TO 4200-MAINT.                                            EL6508
00280                                                                   EL6508
00281                                                                   EL6508
00282      EJECT                                                        EL6508
00283  4000-SHOW.                                                       EL6508
00284                                                                   EL6508
00285      PERFORM 7100-READ-ERACCT THRU 7100-EXIT.                     EL6508
00286      MOVE LOW-VALUES TO EL6508AO.                                 EL6508
00287      GO TO 5000-BUILD-INITIAL-SCREEN.                             EL6508
00288                                                                   EL6508
00289      EJECT                                                        EL6508
00290  4200-MAINT.                                                      EL6508
00291                                                                   EL6508
00292      IF NOT MODIFY-CAP                                            EL6508
00293         MOVE 'UPDATE'            TO SM-READ                       EL6508
00294         PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT            EL6508
00295         MOVE ER-0070             TO EMI-ERROR                     EL6508
00296         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6508
00297         GO TO 8100-SEND-INITIAL-MAP.                              EL6508
00298                                                                   EL6508
00299      PERFORM 7000-EDIT THRU 7099-EXIT.                            EL6508
00300                                                                   EL6508
00301      IF EMI-NO-ERRORS                                             EL6508
00302          NEXT SENTENCE                                            EL6508
00303      ELSE                                                         EL6508
00304          GO TO 8200-SEND-DATAONLY.                                EL6508
00305                                                                   EL6508
00306      PERFORM 7300-READ-ERACCT-UPDATE THRU 7300-EXIT.              EL6508
00307                                                                   EL6508
00308      MOVE ACCOUNT-MASTER      TO JP-RECORD-AREA                   EL6508
00309      MOVE ERACCT-FILE         TO FILE-ID.                         EL6508
00310                                                                   EL6508
00311      PERFORM 6000-CHECK-FOR-UPDATE   THRU 6049-EXIT.              EL6508
00312                                                                   EL6508
00313      IF (AM-LAST-MAINT-USER = PI-UPDATE-BY) OR                    EL6508
00314         (AM-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS)                 EL6508
00315          NEXT SENTENCE                                            EL6508
00316      ELSE                                                         EL6508
00317          EXEC CICS UNLOCK                                         EL6508
00318               DATASET  (ERACCT-FILE)                              EL6508
00319               END-EXEC                                            EL6508
00320          MOVE ER-0068 TO EMI-ERROR                                EL6508
00321          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6508
00322          PERFORM 7100-READ-ERACCT  THRU 7100-EXIT                 EL6508
00323          MOVE LOW-VALUES TO EL6508AO                              EL6508
00324          MOVE -1 TO MAINTYPL                                      EL6508
00325          MOVE 'S' TO PI-MAINT                                     EL6508
00326          GO TO 5000-BUILD-INITIAL-SCREEN.                         EL6508
00327                                                                   EL6508
00328      MOVE PI-PROCESSOR-ID     TO AM-LAST-MAINT-USER.              EL6508
00329      MOVE EIBTIME             TO AM-LAST-MAINT-HHMMSS.            EL6508
00330      MOVE EIBDATE             TO DC-JULIAN-YYDDD.                 EL6508
00331      MOVE '5'                 TO DC-OPTION-CODE.                  EL6508
00332      MOVE LINK-ELDATCV        TO PGM-NAME.                        EL6508
00333                                                                   EL6508
00334      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL6508
00335                                                                   EL6508
00336      MOVE DC-BIN-DATE-1       TO AM-LAST-MAINT-DT                 EL6508
00337                                  BIN-CURRENT-SAVE.                EL6508
00338      MOVE 'B'                 TO JP-RECORD-TYPE                   EL6508
00339      PERFORM 8400-LOG-JOURNAL-RECORD                              EL6508
00340      MOVE ACCOUNT-MASTER      TO JP-RECORD-AREA.                  EL6508
00341                                                                   EL6508
00342      EXEC CICS REWRITE                                            EL6508
00343          DATASET  (ERACCT-FILE)                                   EL6508
00344          FROM     (ACCOUNT-MASTER)                                EL6508
00345          END-EXEC.                                                EL6508
00346                                                                   EL6508
00347      MOVE 'C'                 TO JP-RECORD-TYPE                   EL6508
00348      MOVE ERACCT-FILE         TO FILE-ID.                         EL6508
00349      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6508
00350      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.               EL6508
00351      MOVE ER-0000 TO EMI-ERROR.                                   EL6508
00352      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6508
00353                                                                   EL6508
00354      PERFORM 7100-READ-ERACCT THRU 7100-EXIT.                     EL6508
00355      MOVE LOW-VALUES TO EL6508AO.                                 EL6508
00356      MOVE 'S'        TO PI-MAINT.                                 EL6508
00357                                                                   EL6508
00358      EJECT                                                        EL6508
00359  5000-BUILD-INITIAL-SCREEN.                                       EL6508
00360                                                                   EL6508
00361      MOVE AM-CARRIER              TO CARO.                        EL6508
00362      MOVE AM-GROUPING             TO GROUPO.                      EL6508
00363      MOVE AM-STATE                TO STATEO.                      EL6508
00364      MOVE AM-ACCOUNT              TO ACCTO.                       EL6508
00365                                                                   EL6508
00366      MOVE AM-EFFECTIVE-DT         TO DC-BIN-DATE-1.               EL6508
00367      MOVE ' '                     TO DC-OPTION-CODE.              EL6508
00368                                                                   EL6508
00369      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL6508
00370                                                                   EL6508
00371      MOVE DC-GREG-DATE-1-EDIT     TO EFFDTEO.                     EL6508
00372                                                                   EL6508
00373      IF AM-EXPIRATION-DT NOT EQUAL HIGH-VALUES                    EL6508
00374         MOVE AM-EXPIRATION-DT        TO DC-BIN-DATE-1             EL6508
00375         MOVE ' '                     TO DC-OPTION-CODE            EL6508
00376         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT             EL6508
00377         MOVE DC-GREG-DATE-1-EDIT     TO EXPDTEO                   EL6508
00378      ELSE                                                         EL6508
00379         MOVE '99/99/99'          TO EXPDTEO.                      EL6508
00380                                                                   EL6508
00381                                                                   EL6508
00382      MOVE AM-FLI-BANK-NO          TO BANKNOO.                     EL6508
00383                                                                   EL6508
00384      IF AM-FLI-BANK-BALANCE NUMERIC                               EL6508
00385         IF AM-FLI-BANK-BALANCE NOT EQUAL ZEROS                    EL6508
00386             MOVE AM-FLI-BANK-BALANCE                              EL6508
00387                                   TO BNKBALO                      EL6508
00388             MOVE AL-UNNON         TO BNKBALA.                     EL6508
00389                                                                   EL6508
00390      IF AM-FLI-BANK-1ST-6-PREM NUMERIC                            EL6508
00391          IF AM-FLI-BANK-1ST-6-PREM NOT EQUAL ZEROS                EL6508
00392              MOVE AM-FLI-BANK-1ST-6-PREM TO BNKPREMO              EL6508
00393              MOVE AL-UNNON        TO BNKPREMA.                    EL6508
00394                                                                   EL6508
00395      IF AM-FLI-BANK-CAP-AMT NUMERIC                               EL6508
00396         IF AM-FLI-BANK-CAP-AMT NOT EQUAL ZEROS                    EL6508
00397             MOVE AM-FLI-BANK-CAP-AMT TO BNKAMTO                   EL6508
00398             MOVE AL-UNNON         TO BNKAMTA.                     EL6508
00399                                                                   EL6508
00400      MOVE AM-FLI-RETRO-SHARE-CODE TO RETROCDO.                    EL6508
00401      MOVE AM-FLI-UNITED-IDENT     TO FUICDO.                      EL6508
00402      MOVE AM-FLI-ALT-STATE-CODE   TO ALTSTCDO.                    EL6508
00403      MOVE AM-FLI-BILLING-CODE     TO BILLCDO.                     EL6508
00404                                                                   EL6508
00405      MOVE AL-UANON                TO BANKNOA                      EL6508
00406                                      RETROCDA                     EL6508
00407                                      FUICDA                       EL6508
00408                                      BILLCDA                      EL6508
00409                                      ALTSTCDA.                    EL6508
00410      SET AGT-INDX TO +1.                                          EL6508
00411      SET AGT-INDX DOWN BY +1.                                     EL6508
00412      MOVE ZERO TO SUB1.                                           EL6508
00413                                                                   EL6508
00414  5025-SET-UP-AGENTS.                                              EL6508
00415                                                                   EL6508
00416      SET AGT-INDX UP BY +1                                        EL6508
00417      ADD +1 TO SUB1.                                              EL6508
00418                                                                   EL6508
00419      IF AGT-INDX GREATER AGT-INDX-MAX                             EL6508
00420          GO TO 5050-CONT.                                         EL6508
00421                                                                   EL6508
00422      IF AM-FLI-AGT (SUB1) EQUAL SPACES                            EL6508
00423          GO TO 5025-SET-UP-AGENTS.                                EL6508
00424                                                                   EL6508
00425      MOVE AM-FLI-AGT (SUB1)      TO ACCT-AGT (AGT-INDX).          EL6508
00426      MOVE AM-FLI-AGT-COMM-ACC (SUB1) TO ACCT-ACCT (AGT-INDX).     EL6508
00427                                                                   EL6508
00428      IF AM-FLI-AGT-SHARE-PCT (SUB1) NUMERIC                       EL6508
00429          IF AM-FLI-AGT-SHARE-PCT (SUB1) NOT EQUAL ZEROS           EL6508
00430              MOVE AM-FLI-AGT-SHARE-PCT (SUB1)                     EL6508
00431                                  TO ACCT-SHPCT (AGT-INDX)         EL6508
00432              MOVE AL-UNNON       TO ACCT-SHRPCT-A (AGT-INDX).     EL6508
00433                                                                   EL6508
00434      MOVE AL-UANON               TO ACCT-AGT-A (AGT-INDX)         EL6508
00435                                     ACCT-ACCT-A (AGT-INDX).       EL6508
00436      GO TO 5025-SET-UP-AGENTS.                                    EL6508
00437  5050-CONT.                                                       EL6508
00438                                                                   EL6508
00439      MOVE PI-MAINT                TO MAINTYPO                     EL6508
00440      MOVE AL-UANON                TO MAINTYPA                     EL6508
00441      MOVE -1                      TO MAINTYPL                     EL6508
00442                                                                   EL6508
00443      GO TO 8100-SEND-INITIAL-MAP.                                 EL6508
00444                                                                   EL6508
00445  5099-EXIT.                                                       EL6508
00446      EXIT.                                                        EL6508
00447      EJECT                                                        EL6508
00448  6000-CHECK-FOR-UPDATE.                                           EL6508
00449                                                                   EL6508
00450      IF BANKNOL GREATER ZERO                                      EL6508
00451          MOVE BANKNOI            TO  AM-FLI-BANK-NO.              EL6508
00452                                                                   EL6508
00453      IF BNKBALL GREATER ZERO                                      EL6508
00454          MOVE WS-SAVE-BANK-BAL   TO  AM-FLI-BANK-BALANCE.         EL6508
00455                                                                   EL6508
00456      IF BNKPREML GREATER ZERO                                     EL6508
00457          MOVE WS-SAVE-BANK-PREM  TO  AM-FLI-BANK-1ST-6-PREM.      EL6508
00458                                                                   EL6508
00459      IF BNKAMTL GREATER ZERO                                      EL6508
00460          MOVE WS-SAVE-BANK-AMT   TO  AM-FLI-BANK-CAP-AMT.         EL6508
00461                                                                   EL6508
00462      MOVE RETROCDI               TO  AM-FLI-RETRO-SHARE-CODE.     EL6508
00463      MOVE ALTSTCDI               TO  AM-FLI-ALT-STATE-CODE.       EL6508
00464      MOVE FUICDI                 TO  AM-FLI-UNITED-IDENT.         EL6508
00465      MOVE BILLCDI                TO  AM-FLI-BILLING-CODE.         EL6508
00466                                                                   EL6508
00467      SET AGT-INDX TO +1.                                          EL6508
00468      SET AGT-INDX DOWN BY +1.                                     EL6508
00469      MOVE ZERO TO SUB1.                                           EL6508
00470                                                                   EL6508
00471  6025-CHECK-AGENTS.                                               EL6508
00472                                                                   EL6508
00473      SET AGT-INDX UP BY +1                                        EL6508
00474      ADD +1 TO SUB1.                                              EL6508
00475                                                                   EL6508
00476      IF AGT-INDX GREATER AGT-INDX-MAX                             EL6508
00477          GO TO 6049-EXIT.                                         EL6508
00478                                                                   EL6508
00479      IF ACCT-AGT-L (AGT-INDX) EQUAL ZERO                          EL6508
00480          GO TO 6025-CHECK-AGENTS.                                 EL6508
00481                                                                   EL6508
00482      MOVE ACCT-AGT (AGT-INDX)    TO AM-FLI-AGT (SUB1).            EL6508
00483      MOVE ACCT-ACCT (AGT-INDX)   TO AM-FLI-AGT-COMM-ACC (SUB1).   EL6508
00484                                                                   EL6508
00485      IF ACCT-SHRPCT-L (AGT-INDX) GREATER ZEROS                    EL6508
00486          MOVE WS-SAVE-SHR-PCT (SUB1)                              EL6508
00487                                  TO AM-FLI-AGT-SHARE-PCT (SUB1).  EL6508
00488                                                                   EL6508
00489      MOVE AL-UANON               TO ACCT-AGT-A (AGT-INDX)         EL6508
00490                                     ACCT-ACCT-A (AGT-INDX).       EL6508
00491                                                                   EL6508
00492      GO TO 6025-CHECK-AGENTS.                                     EL6508
00493                                                                   EL6508
00494  6049-EXIT.                                                       EL6508
00495      EXIT.                                                        EL6508
00496      EJECT                                                        EL6508
00497  7000-EDIT.                                                       EL6508
00498                                                                   EL6508
00499      MOVE ZEROS                  TO  WS-SAVE-FIELDS.              EL6508
00500                                                                   EL6508
00501      IF BNKBALL EQUAL ZERO OR GREATER ZERO                        EL6508
00502          MOVE BNKBALI            TO  DEEDIT-FIELD-V1              EL6508
00503          PERFORM 7200-DEEDIT THRU 7200-EXIT                       EL6508
00504          IF DEEDIT-FIELD-V0 EQUAL ZERO OR GREATER ZERO            EL6508
00505             MOVE DEEDIT-FIELD-V1 TO  WS-SAVE-BANK-BAL             EL6508
00506             MOVE AL-UNNON        TO  BNKBALA                      EL6508
00507          ELSE                                                     EL6508
00508              MOVE -1             TO  BNKBALL                      EL6508
00509              MOVE AL-UNBON       TO  BNKBALA                      EL6508
00510              MOVE ER-2387        TO  EMI-ERROR                    EL6508
00511              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6508
00512                                                                   EL6508
00513      IF BNKPREML EQUAL ZERO OR GREATER ZERO                       EL6508
00514          MOVE BNKPREMI           TO  DEEDIT-FIELD-V1              EL6508
00515          PERFORM 7200-DEEDIT THRU 7200-EXIT                       EL6508
00516          IF DEEDIT-FIELD-V0 EQUAL ZERO OR GREATER ZERO            EL6508
00517             MOVE DEEDIT-FIELD-V1 TO  WS-SAVE-BANK-PREM            EL6508
00518             MOVE AL-UNNON        TO  BNKPREMA                     EL6508
00519          ELSE                                                     EL6508
00520              MOVE -1             TO  BNKPREML                     EL6508
00521              MOVE AL-UNBON       TO  BNKPREMA                     EL6508
00522              MOVE ER-2388        TO  EMI-ERROR                    EL6508
00523              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6508
00524                                                                   EL6508
00525      IF BNKAMTL EQUAL ZERO OR GREATER ZERO                        EL6508
00526          MOVE BNKAMTI            TO  DEEDIT-FIELD-V1              EL6508
00527          PERFORM 7200-DEEDIT THRU 7200-EXIT                       EL6508
00528          IF DEEDIT-FIELD-V0 EQUAL ZERO OR GREATER ZERO            EL6508
00529             MOVE DEEDIT-FIELD-V1 TO  WS-SAVE-BANK-AMT             EL6508
00530             MOVE AL-UNNON        TO  BNKAMTA                      EL6508
00531          ELSE                                                     EL6508
00532              MOVE -1             TO  BNKAMTL                      EL6508
00533              MOVE AL-UNBON       TO  BNKAMTA                      EL6508
00534              MOVE ER-2389        TO  EMI-ERROR                    EL6508
00535              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6508
00536                                                                   EL6508
00537      SET AGT-INDX TO +1.                                          EL6508
00538      SET AGT-INDX DOWN BY +1.                                     EL6508
00539      MOVE ZERO TO SUB1.                                           EL6508
00540                                                                   EL6508
00541  7025-CHECK-AGENTS.                                               EL6508
00542                                                                   EL6508
00543      SET AGT-INDX UP BY +1                                        EL6508
00544      ADD +1 TO SUB1.                                              EL6508
00545                                                                   EL6508
00546      IF AGT-INDX GREATER AGT-INDX-MAX                             EL6508
00547          GO TO 7099-EXIT.                                         EL6508
00548                                                                   EL6508
00549      IF ACCT-AGT-L (AGT-INDX) GREATER ZERO                        EL6508
00550          NEXT SENTENCE                                            EL6508
00551      ELSE                                                         EL6508
00552          GO TO 7025-CHECK-AGENTS.                                 EL6508
00553                                                                   EL6508
00554      IF ACCT-ACCT-L (AGT-INDX) GREATER ZERO                       EL6508
00555          MOVE AL-UANON            TO  ACCT-ACCT-A (AGT-INDX)      EL6508
00556      ELSE                                                         EL6508
00557          MOVE -1                  TO  ACCT-ACCT-L (AGT-INDX)      EL6508
00558          MOVE AL-UABON            TO  ACCT-ACCT-A (AGT-INDX)      EL6508
00559          MOVE ER-2390             TO  EMI-ERROR                   EL6508
00560          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL6508
00561                                                                   EL6508
00562      IF ACCT-SHRPCT-L (AGT-INDX) NOT GREATER ZERO                 EL6508
00563          GO TO 7025-CHECK-AGENTS.                                 EL6508
00564                                                                   EL6508
00565      MOVE ACCT-SHRPCT (AGT-INDX)  TO  DEEDIT-FIELD.               EL6508
00566                                                                   EL6508
00567      PERFORM 7200-DEEDIT THRU 7200-EXIT.                          EL6508
00568                                                                   EL6508
00569      MOVE DEEDIT-FIELD-V1         TO  WS-SAVE-SHR-PCT (SUB1).     EL6508
00570      MOVE AL-UNNON                TO  ACCT-SHRPCT-A (AGT-INDX).   EL6508
00571                                                                   EL6508
00572      GO TO 7025-CHECK-AGENTS.                                     EL6508
00573                                                                   EL6508
00574                                                                   EL6508
00575  7099-EXIT.                                                       EL6508
00576      EXIT.                                                        EL6508
00577      EJECT                                                        EL6508
00578                                                                   EL6508
00579  7100-READ-ERACCT.                                                EL6508
00580                                                                   EL6508
00581      EXEC CICS READ                                               EL6508
00582           DATASET  (ERACCT-FILE)                                  EL6508
00583           SET      (ADDRESS OF ACCOUNT-MASTER)                       CL**2
00584           RIDFLD   (PI-ACCT-KEY)                                  EL6508
00585           END-EXEC.                                               EL6508
00586                                                                   EL6508
00587      CONTINUE.                                                       CL**2
00588                                                                   EL6508
00589      MOVE AM-LAST-MAINT-USER TO PI-UPDATE-BY.                     EL6508
00590      MOVE AM-LAST-MAINT-HHMMSS TO PI-UPDATE-HHMMSS.               EL6508
00591                                                                   EL6508
00592                                                                   EL6508
00593  7100-EXIT.                                                       EL6508
00594      EXIT.                                                        EL6508
00595      EJECT                                                        EL6508
00596                                                                   EL6508
00597  7200-DEEDIT.                                                     EL6508
00598                                                                   EL6508
00599      EXEC CICS BIF                                                EL6508
00600           DEEDIT                                                  EL6508
00601           FIELD  (DEEDIT-FIELD)                                   EL6508
00602           LENGTH (15)                                             EL6508
00603           END-EXEC.                                               EL6508
00604                                                                   EL6508
00605  7200-EXIT.                                                       EL6508
00606      EXIT.                                                        EL6508
00607      EJECT                                                        EL6508
00608                                                                   EL6508
00609  7300-READ-ERACCT-UPDATE.                                         EL6508
00610                                                                   EL6508
00611      EXEC CICS READ                                               EL6508
00612           DATASET  (ERACCT-FILE)                                  EL6508
00613           SET      (ADDRESS OF ACCOUNT-MASTER)                       CL**2
00614           RIDFLD   (PI-ACCT-KEY)                                  EL6508
00615           UPDATE                                                  EL6508
00616           END-EXEC.                                               EL6508
00617                                                                   EL6508
00618      CONTINUE.                                                       CL**2
00619                                                                   EL6508
00620  7300-EXIT.                                                       EL6508
00621      EXIT.                                                        EL6508
00622      EJECT                                                        EL6508
00623                                                                   EL6508
00624  7800-COMPANY-REC-READ.                                           EL6508
00625      MOVE SPACES                 TO ELCNTL-KEY.                   EL6508
00626      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL6508
00627      MOVE '1'                    TO CNTL-REC-TYPE.                EL6508
00628      MOVE +0                     TO CNTL-SEQ-NO.                  EL6508
00629      EXEC CICS HANDLE CONDITION                                   EL6508
00630          NOTFND   (7880-NO-COMP)                                  EL6508
00631          END-EXEC.                                                EL6508
00632      EXEC CICS READ                                               EL6508
00633          DATASET   (CNTL-FILE-ID)                                 EL6508
00634          SET       (ADDRESS OF CONTROL-FILE)                         CL**2
00635          RIDFLD    (ELCNTL-KEY)                                   EL6508
00636          END-EXEC.                                                EL6508
00637      CONTINUE.                                                       CL**2
00638      IF CF-ACCOUNT-MSTR-MAINT-DT = LOW-VALUES                     EL6508
00639          MOVE ER-2572            TO EMI-ERROR                     EL6508
00640          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL6508
00641      GO TO 7899-EXIT.                                             EL6508
00642                                                                   EL6508
00643  7880-NO-COMP.                                                    EL6508
00644      MOVE ER-0002                TO EMI-ERROR                     EL6508
00645      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6508
00646  7899-EXIT.                                                       EL6508
00647      EXIT.                                                        EL6508
00648      EJECT                                                        EL6508
00649                                                                   EL6508
00650  8000-UPDATE-MAINT-DATE.                                          EL6508
00651                                                                   EL6508
00652      MOVE SPACES                 TO ELCNTL-KEY.                   EL6508
00653                                                                   EL6508
00654      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL6508
00655      MOVE '1'                    TO CNTL-REC-TYPE.                EL6508
00656      MOVE +0                     TO CNTL-SEQ-NO.                  EL6508
00657                                                                   EL6508
00658      EXEC CICS HANDLE CONDITION                                   EL6508
00659          NOTFND   (8000-EXIT)                                     EL6508
00660          END-EXEC.                                                EL6508
00661                                                                   EL6508
00662      EXEC CICS READ                                               EL6508
00663          UPDATE                                                   EL6508
00664          DATASET   (CNTL-FILE-ID)                                 EL6508
00665          SET       (ADDRESS OF CONTROL-FILE)                         CL**2
00666          RIDFLD    (ELCNTL-KEY)                                   EL6508
00667          END-EXEC.                                                EL6508
00668      CONTINUE.                                                       CL**2
00669                                                                   EL6508
00670      MOVE CONTROL-FILE           TO JP-RECORD-AREA                EL6508
00671      MOVE 'B'                    TO JP-RECORD-TYPE                EL6508
00672      MOVE CNTL-FILE-ID           TO FILE-ID                       EL6508
00673      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6508
00674                                                                   EL6508
00675      MOVE BIN-CURRENT-SAVE       TO CF-ACCOUNT-MSTR-MAINT-DT      EL6508
00676                                                                   EL6508
00677      MOVE CONTROL-FILE           TO JP-RECORD-AREA                EL6508
00678      MOVE 'C'                    TO JP-RECORD-TYPE                EL6508
00679      MOVE CNTL-FILE-ID           TO FILE-ID                       EL6508
00680                                                                   EL6508
00681      EXEC CICS REWRITE                                            EL6508
00682          DATASET   (CNTL-FILE-ID)                                 EL6508
00683          FROM      (CONTROL-FILE  )                               EL6508
00684          END-EXEC.                                                EL6508
00685      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6508
00686                                                                   EL6508
00687  8000-EXIT.                                                       EL6508
00688       EXIT.                                                       EL6508
00689      EJECT                                                        EL6508
00690                                                                   EL6508
00691  8100-SEND-INITIAL-MAP.                                           EL6508
00692      MOVE SAVE-DATE              TO DATEO.                        EL6508
00693      MOVE EIBTIME                TO TIME-IN.                      EL6508
00694      MOVE TIME-OUT               TO TIMEO.                        EL6508
00695      MOVE -1                     TO PFENTERL                      EL6508
00696      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL6508
00697      EXEC CICS SEND                                               EL6508
00698          MAP      (MAP-NAME)                                      EL6508
00699          MAPSET   (MAPSET-NAME)                                   EL6508
00700          FROM     (EL6508AO)                                      EL6508
00701          ERASE                                                    EL6508
00702          CURSOR                                                   EL6508
00703          END-EXEC.                                                EL6508
00704      GO TO 9100-RETURN-TRAN.                                      EL6508
00705                                                                   EL6508
00706  8200-SEND-DATAONLY.                                              EL6508
00707      MOVE SAVE-DATE              TO DATEO.                        EL6508
00708      MOVE EIBTIME                TO TIME-IN.                      EL6508
00709      MOVE TIME-OUT               TO TIMEO.                        EL6508
00710      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O                      EL6508
00711      EXEC CICS SEND                                               EL6508
00712          MAP      (MAP-NAME)                                      EL6508
00713          MAPSET   (MAPSET-NAME)                                   EL6508
00714          FROM     (EL6508AO)                                      EL6508
00715          DATAONLY                                                 EL6508
00716          ERASEAUP                                                 EL6508
00717          CURSOR                                                   EL6508
00718          END-EXEC.                                                EL6508
00719      GO TO 9100-RETURN-TRAN.                                      EL6508
00720                                                                   EL6508
00721  8300-SEND-TEXT.                                                  EL6508
00722      EXEC CICS SEND TEXT                                          EL6508
00723          FROM     (LOGOFF-TEXT)                                   EL6508
00724          LENGTH   (LOGOFF-LENGTH)                                 EL6508
00725          ERASE                                                    EL6508
00726          FREEKB                                                   EL6508
00727          END-EXEC.                                                EL6508
00728      EXEC CICS RETURN                                             EL6508
00729          END-EXEC.                                                EL6508
00730                                                                   EL6508
00731  8400-LOG-JOURNAL-RECORD.                                         EL6508
00732                                                                   EL6508
00733      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   EL6508
00734      MOVE FILE-ID                TO JP-FILE-ID.                   EL6508
00735      MOVE THIS-PGM               TO JP-PROGRAM-ID.                EL6508
00736                                                                   EL6508
pemuni*    EXEC CICS JOURNAL                                            EL6508
pemuni*        JFILEID     (PI-JOURNAL-FILE-ID)                         EL6508
pemuni*        JTYPEID     ('AM')                                       EL6508
pemuni*        FROM        (JOURNAL-RECORD)                             EL6508
pemuni*        LENGTH      (2023)                                       EL6508
pemuni*        END-EXEC.                                                EL6508
00743                                                                   EL6508
00744  8800-UNAUTHORIZED-ACCESS.                                        EL6508
00745      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL6508
00746      GO TO 8300-SEND-TEXT.                                        EL6508
00747                                                                   EL6508
00748  8810-PF23.                                                       EL6508
00749      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL6508
00750      MOVE XCTL-005               TO PGM-NAME.                     EL6508
00751      GO TO 9300-XCTL.                                             EL6508
00752  9000-RETURN-CICS.                                                EL6508
00753      EXEC CICS RETURN                                             EL6508
00754          END-EXEC.                                                EL6508
00755                                                                   EL6508
00756  9100-RETURN-TRAN.                                                EL6508
00757      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL6508
00758      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.         EL6508
00759      EXEC CICS RETURN                                             EL6508
00760          TRANSID    (TRANS-ID)                                    EL6508
00761          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6508
00762          LENGTH     (WS-COMM-LENGTH)                              EL6508
00763          END-EXEC.                                                EL6508
00764                                                                   EL6508
00765  9200-RETURN-MAIN-MENU.                                           EL6508
00766      MOVE XCTL-626               TO PGM-NAME.                     EL6508
00767      GO TO 9300-XCTL.                                             EL6508
00768                                                                   EL6508
00769  9300-XCTL.                                                       EL6508
00770      EXEC CICS XCTL                                               EL6508
00771          PROGRAM    (PGM-NAME)                                    EL6508
00772          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6508
00773          LENGTH     (WS-COMM-LENGTH)                              EL6508
00774          END-EXEC.                                                EL6508
00775                                                                   EL6508
00776  9400-CLEAR.                                                      EL6508
00777      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME                      EL6508
00778      GO TO 9300-XCTL.                                             EL6508
00779                                                                   EL6508
00780  9500-PF12.                                                       EL6508
00781      MOVE XCTL-010               TO PGM-NAME.                     EL6508
00782      GO TO 9300-XCTL.                                             EL6508
00783                                                                   EL6508
00784  9600-PGMID-ERROR.                                                EL6508
00785      EXEC CICS HANDLE CONDITION                                   EL6508
00786          PGMIDERR    (8300-SEND-TEXT)                             EL6508
00787          END-EXEC.                                                EL6508
00788      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL6508
00789      MOVE ' '                    TO PI-ENTRY-CD-1.                EL6508
00790      MOVE XCTL-005               TO PGM-NAME.                     EL6508
00791      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL6508
00792      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL6508
00793      GO TO 9300-XCTL.                                             EL6508
00794                                                                   EL6508
00795  9700-LINK-DATE-CONVERT.                                          EL6508
00796                                                                   EL6508
00797      EXEC CICS LINK                                               EL6508
00798          PROGRAM    ('ELDATCV')                                   EL6508
00799          COMMAREA   (DATE-CONVERSION-DATA)                        EL6508
00800          LENGTH     (DC-COMM-LENGTH)                              EL6508
00801          END-EXEC.                                                EL6508
00802  9700-EXIT.                                                       EL6508
00803      EXIT.                                                        EL6508
00804                                                                   EL6508
00805  9900-ERROR-FORMAT.                                               EL6508
00806      IF NOT EMI-ERRORS-COMPLETE                                   EL6508
00807          MOVE LINK-001           TO PGM-NAME                      EL6508
00808          EXEC CICS LINK                                           EL6508
00809              PROGRAM    (PGM-NAME)                                EL6508
00810              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL6508
00811              LENGTH     (EMI-COMM-LENGTH)                         EL6508
00812              END-EXEC.                                            EL6508
00813  9900-EXIT.                                                       EL6508
00814      EXIT.                                                        EL6508
00815                                                                   EL6508
00816  9990-ABEND.                                                      EL6508
00817      MOVE LINK-004               TO PGM-NAME.                     EL6508
00818      MOVE DFHEIBLK               TO EMI-LINE1                     EL6508
00819                                                                   EL6508
00820      EXEC CICS LINK                                               EL6508
00821          PROGRAM   (PGM-NAME)                                     EL6508
00822          COMMAREA  (EMI-LINE1)                                    EL6508
00823          LENGTH    (72)                                           EL6508
00824      END-EXEC.                                                    EL6508
00825                                                                   EL6508
00826      GO TO 8200-SEND-DATAONLY.                                    EL6508
00827      GOBACK.                                                      EL6508
00828  9995-SECURITY-VIOLATION.                                         EL6508
00829                                    COPY ELCSCTP.                  EL6508
00830  9995-EXIT.                                                       EL6508
00831      EXIT.                                                        EL6508
