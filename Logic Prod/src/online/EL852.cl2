00001  IDENTIFICATION DIVISION.                                         02/26/96
00002                                                                   EL852
00003  PROGRAM-ID.                 EL852 .                                 LV007
00004 *              PROGRAM CONVERTED BY                                  CL**7
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**7
00006 *              CONVERSION DATE 02/14/96 08:03:10.                    CL**7
00007 *                            VMOD=2.007                              CL**7
00008 *                                                                 EL852
00008 *                                                                 EL852
00009 *AUTHOR.     LOGIC,INC.                                              CL**7
00010 *            DALLAS, TEXAS.                                          CL**7
00011                                                                   EL852
00012 *DATE-COMPILED.                                                      CL**7
00013 *SECURITY.   *****************************************************   CL**7
00014 *            *                                                   *   CL**7
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**7
00016 *            *                                                   *   CL**7
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**7
00018 *                                                                *   CL**7
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**7
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**7
00021 *            *                                                   *   CL**7
00022 *            *****************************************************   CL**7
00023                                                                   EL852
00024 *REMARKS. TRANSACTION - EXJ2 - ACCOUNTS RECEIVABLE                   CL**7
00025 *                              REQUEST FILE SELECTION.               CL**7
00026                                                                   EL852
00027  ENVIRONMENT DIVISION.                                            EL852
00028                                                                   EL852
00029      EJECT                                                        EL852
00030  DATA DIVISION.                                                   EL852
00031  WORKING-STORAGE SECTION.                                         EL852
00032                                                                   EL852
00033  77  FILLER  PIC X(32)  VALUE '********************************'. EL852
00034  77  FILLER  PIC X(32)  VALUE '*    EL852 WORKING STORAGE     *'. EL852
00035  77  FILLER  PIC X(32)  VALUE '************ V/M 2.007 *********'.    CL**7
00036                                                                   EL852
00037     EJECT                                                         EL852
00038                                                                   EL852
00039                              COPY ELCSCTM.                           CL**4
00040                              COPY ELCSCRTY.                          CL**4
00041                                                                   EL852
00042     EJECT                                                         EL852
00043                                                                   EL852
00044 ******************************************************************EL852
00045 *                                                                *EL852
00046 *              S T A N D A R D   A R E A S                       *EL852
00047 *                                                                *EL852
00048 ******************************************************************EL852
00049                                                                   EL852
00050  01  STANDARD-AREAS.                                              EL852
00051      12  SC-ITEM             PIC S9(4)   VALUE +1   COMP.         EL852
00052      12  GETMAIN-SPACE       PIC X       VALUE SPACE.             EL852
00053      12  EL852A              PIC X(8)    VALUE 'EL852A'.          EL852
00054      12  EL852B              PIC X(8)    VALUE 'EL852B'.          EL852
00055      12  EL852C              PIC X(8)    VALUE 'EL852C'.          EL852
00056      12  EL852D              PIC X(8)    VALUE 'EL852D'.          EL852
00057      12  MAPSET-EL852S       PIC X(8)    VALUE 'EL852S'.          EL852
00058      12  TRANS-EXJ2          PIC X(4)    VALUE 'EXJ2'.            EL852
00059      12  THIS-PGM            PIC X(8)    VALUE 'EL852 '.          EL852
00060      12  PGM-NAME            PIC X(8).                            EL852
00061      12  TIME-IN             PIC S9(7).                           EL852
00062      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL852
00063          16  FILLER          PIC X.                               EL852
00064          16  TIME-OUT        PIC 99V99.                           EL852
00065          16  FILLER              PIC X(2).                        EL852
00066      12  LINK-EL001              PIC X(8)    VALUE 'EL001'.       EL852
00067      12  LINK-EL004              PIC X(8)    VALUE 'EL004'.       EL852
00068      12  XCTL-EL005              PIC X(8)    VALUE 'EL005'.       EL852
00069      12  XCTL-EL010              PIC X(8)    VALUE 'EL010'.       EL852
00070      12  XCTL-EL626              PIC X(8)    VALUE 'EL626'.       EL852
00071      12  XCTL-EL8521             PIC X(8)    VALUE 'EL8521'.      EL852
00072      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     EL852
00073      12  FILE-ID-ERRQST          PIC X(8)    VALUE 'ERRQST'.      EL852
00074      12  FILE-ID-ERRQST2         PIC X(8)    VALUE 'ERRQST2'.     EL852
00075      12  FILE-ID-ERRQST3         PIC X(8)    VALUE 'ERRQST3'.     EL852
00076      12  FILE-ID-ERRQST4         PIC X(8)    VALUE 'ERRQST4'.     EL852
00077      12  FILE-ID-ERRQST5         PIC X(8)    VALUE 'ERRQST5'.     EL852
00078      12  FILE-ID-ERSUMM          PIC X(8)    VALUE 'ERSUMM'.      EL852
00079      12  WS-CURRENT-DT           PIC X(8)    VALUE SPACES.        EL852
00080      12  WS-CURRENT-BIN-DT       PIC XX      VALUE SPACES.        EL852
00081      12  WS-DATA-SELECTED-SW     PIC X       VALUE SPACE.         EL852
00082          88 WS-DATA-SELECTED                 VALUE 'Y'.           EL852
00083      12  WS-OPTION-COUNTER       PIC S999    VALUE ZEROS COMP-3.  EL852
00084      12  WS-MAINT-FUNCTION       PIC X       VALUE SPACE.         EL852
00085          88 WS-BROWSE-FUNCTION               VALUE 'B'.           EL852
00086          88 WS-SUBMIT-FUNCTION               VALUE 'S'.           EL852
00087          88 WS-RESUBMIT-FUNCTION             VALUE 'R'.           EL852
00088      12  WS-REQUEST-BROWSE-SW    PIC X       VALUE SPACE.         EL852
00089          88 WS-REQUEST-BROWSE-STARTED        VALUE 'Y'.           EL852
00090      12  WS-SUMMARY-BROWSE-SW    PIC X       VALUE SPACE.         EL852
00091          88 WS-SUMMARY-BROWSE-STARTED        VALUE 'Y'.           EL852
00092      12  WS-SAVE-ACCESS-KEY      PIC X(46)   VALUE SPACES.        EL852
00093      12  WS-RQST-ACCT-CONTROL    PIC X(20)   VALUE SPACES.        EL852
00094      12  WS-RQST-FIN-RESP-CNTL   PIC X(18)   VALUE SPACES.        EL852
00095      12  WS-ACCESS-KEY           PIC X(46)   VALUE SPACES.        EL852
00096      12  QID.                                                     EL852
00097          16  QID-TERM            PIC X(4)      VALUE SPACES.      EL852
00098          16  FILLER              PIC X(4)      VALUE '125D'.      EL852
00099                                                                   EL852
00100                                                                   EL852
00101      EJECT                                                        EL852
00102                                                                   EL852
00103 ******************************************************************EL852
00104 *                                                                *EL852
00105 *                E R R O R   M E S S A G E S                     *EL852
00106 *                                                                *EL852
00107 ******************************************************************EL852
00108                                                                   EL852
00109  01  ERROR-MESSAGES.                                              EL852
00110      12  ER-0000                 PIC X(4)  VALUE '0000'.          EL852
00111      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL852
00112      12  ER-0008                 PIC X(4)  VALUE '0008'.          EL852
00113      12  ER-0023                 PIC X(4)  VALUE '0023'.          EL852
00114      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL852
00115      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL852
00116      12  ER-2132                 PIC X(4)  VALUE '2132'.          EL852
00117      12  ER-2134                 PIC X(4)  VALUE '2134'.          EL852
00118      12  ER-2919                 PIC X(4)  VALUE '2919'.          EL852
00119      12  ER-2935                 PIC X(4)  VALUE '2935'.          EL852
00120      12  ER-3133                 PIC X(4)  VALUE '3133'.          EL852
00121                                                                   EL852
00122      EJECT                                                        EL852
00123                                                                   EL852
00124 ******************************************************************EL852
00125 *                                                                *EL852
00126 *              A C C E S S   K E Y S                             *EL852
00127 *                                                                *EL852
00128 ******************************************************************EL852
00129                                                                   EL852
00130  01  ACCESS-KEYS.                                                 EL852
00131      12  ERRQST-KEY.                                              EL852
00132          16  ERRQST-COMPANY-CD       PIC X     VALUE SPACE.       EL852
00133          16  ERRQST-ENTRY-BATCH      PIC X(6)  VALUE SPACES.      EL852
00134                                                                   EL852
00135      12  ERRQST-ALT-KEY1.                                         EL852
00136          16  ERRQST-ACCT-CONTROL.                                 EL852
00137              20 ERRQST-COMPANY-CD-A1 PIC X     VALUE SPACES.      EL852
00138              20 ERRQST-CARRIER-A1    PIC X     VALUE SPACES.      EL852
00139              20 ERRQST-GROUPING-A1   PIC X(6)  VALUE SPACES.      EL852
00140              20 ERRQST-STATE-A1      PIC XX    VALUE SPACES.      EL852
00141              20 ERRQST-ACCOUNT-A1    PIC X(10) VALUE SPACES.      EL852
00142          16  ERRQST-REFERENCE-A1     PIC X(12) VALUE SPACES.      EL852
00143          16  ERRQST-BATCH-A1         PIC X(6)  VALUE SPACES.      EL852
00144                                                                   EL852
00145      12  ERRQST-ALT-KEY2.                                         EL852
00146          16  ERRQST-FIN-RESP-CONTROL.                             EL852
00147              20 ERRQST-COMPANY-CD-A2 PIC X     VALUE SPACES.      EL852
00148              20 ERRQST-CARRIER-A2    PIC X     VALUE SPACES.      EL852
00149              20 ERRQST-GROUPING-A2   PIC X(6)  VALUE SPACES.      EL852
00150              20 ERRQST-FIN-RESP-A2   PIC X(10) VALUE SPACES.      EL852
00151          16  ERRQST-ACCT-AGENT-A2    PIC X(10) VALUE SPACES.      EL852
00152          16  ERRQST-REFERENCE-A2     PIC X(12) VALUE SPACES.      EL852
00153          16  ERRQST-BATCH-A2         PIC X(6)  VALUE SPACES.      EL852
00154                                                                   EL852
00155      12  ERRQST-ALT-KEY3.                                         EL852
00156          16  ERRQST-COMPANY-CD-A3    PIC X     VALUE SPACES.      EL852
00157          16  ERRQST-CARRIER-A3       PIC X     VALUE SPACES.      EL852
00158          16  ERRQST-GROUPING-A3      PIC X     VALUE SPACES.      EL852
00159          16  ERRQST-ACCOUNT-AGENT    PIC X     VALUE SPACES.      EL852
00160          16  ERRQST-BATCH-A3         PIC X     VALUE SPACES.      EL852
00161                                                                   EL852
00162      12  ERRQST-ALT-KEY4.                                         EL852
00163          16  ERRQST-COMPANY-CD-A4    PIC X     VALUE SPACES.      EL852
00164          16  ERRQST-SUMMARY-CODE     PIC X(6)  VALUE SPACES.      EL852
00165          16  ERRQST-ACCOUNT-A4       PIC X(10) VALUE SPACES.      EL852
00166          16  ERRQST-REFERENCE-A4     PIC X(12) VALUE SPACES.      EL852
00167          16  ERRQST-BATCH-A4         PIC X(6)  VALUE SPACES.      EL852
00168                                                                   EL852
00169      12  ERRQST-RECORD-LENGTH        PIC S9(4) COMP VALUE +200.   EL852
00170      12  ERRQST-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +223.   EL852
00171                                                                   EL852
00172                                                                   EL852
00173                                                                   EL852
00174      EJECT                                                        EL852
00175                                                                   EL852
00176                              COPY ELCDATE.                           CL**4
00177                                                                   EL852
00178      EJECT                                                        EL852
00179                              COPY ELCLOGOF.                          CL**4
00180                                                                   EL852
00181      EJECT                                                        EL852
00182                              COPY ELCATTR.                           CL**4
00183                                                                   EL852
00184      EJECT                                                        EL852
00185                                    COPY ELCEMIB.                     CL**4
00186                                                                   EL852
00187      EJECT                                                        EL852
00188                              COPY ELCINTF.                           CL**4
00189      COPY ELC852PI.                                               EL852
00190      EJECT                                                        EL852
00191                              COPY ELCJPFX.                           CL**4
00192                              PIC X(223).                          EL852
00193                                                                   EL852
00194      EJECT                                                        EL852
00195                              COPY ELCAID.                            CL**4
00196  01  FILLER    REDEFINES DFHAID.                                  EL852
00197      12  FILLER              PIC X(8).                            EL852
00198      12  PF-VALUES           PIC X       OCCURS 2.                EL852
00199                                                                   EL852
00200      EJECT                                                        EL852
00201                              COPY EL852S.                            CL**4
00202                                                                   EL852
00203      EJECT                                                        EL852
00204                                                                   EL852
00205                                                                   EL852
00206      EJECT                                                        EL852
00207  LINKAGE SECTION.                                                 EL852
00208  01  DFHCOMMAREA             PIC X(1024).                         EL852
00209                                                                   EL852
00210      EJECT                                                        EL852
00211                                                                   EL852
00212                                  COPY ERCRQST.                       CL**4
00213      EJECT                                                        EL852
00214                                                                   EL852
00215                                  COPY ERCSUMM.                       CL**4
00216      EJECT                                                        EL852
00217                                                                   EL852
00218  PROCEDURE DIVISION.                                              EL852
00219                                                                   EL852
00220      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL852
00221      MOVE 1                      TO EMI-NUMBER-OF-LINES.          EL852
00222                                                                   EL852
00223      IF EIBCALEN = 0                                              EL852
00224          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL852
00225                                                                   EL852
00226      MOVE EIBTRMID               TO QID-TERM.                     EL852
00227      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL852
00228      MOVE '5'                    TO DC-OPTION-CODE.               EL852
00229      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL852
00230      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.            EL852
00231      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.                EL852
00232                                                                   EL852
00233      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL852
00234          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL852
00235              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL852
00236              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL852
00237              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL852
00238              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL852
00239              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL852
00240              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL852
00241              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL852
00242              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL852
00243          ELSE                                                     EL852
00244              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL852
00245              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL852
00246              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL852
00247              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL852
00248              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL852
00249              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL852
00250              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL852
00251              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL852
00252                                                                   EL852
00253      MOVE LOW-VALUES             TO EL852AI.                      EL852
00254                                                                   EL852
00255      IF EIBTRNID NOT = TRANS-EXJ2                                 EL852
00256         GO TO 8100-SEND-INITIAL-MAP.                              EL852
00257                                                                   EL852
00258      EXEC CICS HANDLE CONDITION                                   EL852
00259          PGMIDERR  (9600-PGMID-ERROR)                             EL852
00260          ERROR     (9990-ABEND)                                   EL852
00261      END-EXEC.                                                    EL852
00262                                                                   EL852
00263      IF EIBAID = DFHCLEAR                                         EL852
00264          GO TO 9400-CLEAR.                                        EL852
00265                                                                   EL852
00266                                                                   EL852
00267      IF PI-PROCESSOR-ID = 'LGXX'                                  EL852
00268          GO TO 0200-RECEIVE.                                      EL852
00269                                                                   EL852
00270      EXEC CICS READQ TS                                           EL852
00271          QUEUE  (QID)                                             EL852
00272          INTO   (SECURITY-CONTROL)                                EL852
00273          LENGTH (SC-COMM-LENGTH)                                  EL852
00274          ITEM   (SC-ITEM)                                         EL852
00275      END-EXEC.                                                    EL852
00276                                                                   EL852
00277      MOVE SC-CREDIT-DISPLAY (2)   TO PI-DISPLAY-CAP.              EL852
00278      MOVE SC-CREDIT-UPDATE  (2)   TO PI-MODIFY-CAP.               EL852
00279                                                                   EL852
00280      IF NOT DISPLAY-CAP                                           EL852
00281          MOVE 'READ'          TO SM-READ                          EL852
00282          PERFORM 9995-SECURITY-VIOLATION                          EL852
00283          MOVE ER-0070         TO  EMI-ERROR                       EL852
00284          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL852
00285          GO TO 8100-SEND-INITIAL-MAP.                             EL852
00286                                                                   EL852
00287      EJECT                                                        EL852
00288                                                                   EL852
00289 ******************************************************************EL852
00290 *                                                                *EL852
00291 *              R E C E I V E   M A P                             *EL852
00292 *                                                                *EL852
00293 ******************************************************************EL852
00294                                                                   EL852
00295  0200-RECEIVE.                                                    EL852
00296                                                                   EL852
00297      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL852
00298          MOVE ER-0008            TO EMI-ERROR                     EL852
00299          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL852
00300          MOVE -1                 TO MAINTL                        EL852
00301          GO TO 8200-SEND-DATAONLY.                                EL852
00302                                                                   EL852
00303      EXEC CICS RECEIVE                                            EL852
00304          MAP      (EL852A)                                        EL852
00305          MAPSET   (MAPSET-EL852S)                                 EL852
00306          INTO     (EL852AI)                                       EL852
00307      END-EXEC.                                                    EL852
00308                                                                   EL852
00309                                                                   EL852
00310      IF PFENTERL GREATER THAN ZERO                                EL852
00311         IF EIBAID NOT = DFHENTER                                  EL852
00312            MOVE ER-0004          TO EMI-ERROR                     EL852
00313            MOVE AL-UNBOF         TO PFENTERA                      EL852
00314            MOVE -1               TO PFENTERL                      EL852
00315            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL852
00316            GO TO 8200-SEND-DATAONLY                               EL852
00317         ELSE                                                      EL852
00318            IF (PFENTERI NUMERIC) AND                                 CL**3
00319               (PFENTERI GREATER 0 AND LESS 25)                       CL**3
00320               MOVE PF-VALUES (PFENTERI) TO EIBAID                 EL852
00321            ELSE                                                   EL852
00322               MOVE ER-0029       TO EMI-ERROR                     EL852
00323               MOVE AL-UNBOF      TO PFENTERA                      EL852
00324               MOVE -1            TO PFENTERL                      EL852
00325               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            EL852
00326               GO TO 8200-SEND-DATAONLY.                           EL852
00327                                                                   EL852
00328      EJECT                                                        EL852
00329                                                                   EL852
00330 ******************************************************************EL852
00331 *                                                                *EL852
00332 *              C H E C K   P F K E Y S                           *EL852
00333 *                                                                *EL852
00334 ******************************************************************EL852
00335                                                                   EL852
00336  0300-CHECK-PFKEYS.                                               EL852
00337                                                                   EL852
00338      IF EIBAID = DFHPF23                                          EL852
00339          GO TO 8810-PF23.                                         EL852
00340                                                                   EL852
00341      IF EIBAID = DFHPF24                                          EL852
00342          GO TO 9200-RETURN-MAIN-MENU.                             EL852
00343                                                                   EL852
00344      IF EIBAID = DFHPF12                                          EL852
00345          GO TO 9500-PF12.                                         EL852
00346                                                                   EL852
00347      IF EIBAID = DFHENTER                                         EL852
00348          GO TO 1000-EDIT-MAP.                                     EL852
00349                                                                   EL852
00350      MOVE ER-0008 TO EMI-ERROR.                                   EL852
00351      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL852
00352      MOVE -1                     TO PFENTERL.                     EL852
00353                                                                   EL852
00354      GO TO 8200-SEND-DATAONLY.                                    EL852
00355                                                                   EL852
00356      EJECT                                                        EL852
00357                                                                   EL852
00358 ******************************************************************EL852
00359 *                                                                *EL852
00360 *                  E D I T    M A P                              *EL852
00361 *                                                                *EL852
00362 ******************************************************************EL852
00363                                                                   EL852
00364  1000-EDIT-MAP.                                                   EL852
00365                                                                   EL852
00366      MOVE SPACES                 TO PI-PROGRAM-WORK-AREA.         EL852
00367                                                                   EL852
00368      IF MAINTI = 'S' OR 'R' OR 'B'                                EL852
00369         MOVE MAINTI              TO WS-MAINT-FUNCTION             EL852
00370         MOVE AL-UANON            TO MAINTA                        EL852
00371      ELSE                                                         EL852
00372         MOVE ER-0023             TO EMI-ERROR                     EL852
00373         MOVE -1                  TO MAINTL                        EL852
00374         MOVE AL-UABON            TO MAINTA                        EL852
00375         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL852
00376         GO TO 8200-SEND-DATAONLY.                                 EL852
00377                                                                   EL852
00378      IF NOT MODIFY-CAP                                            EL852
00379         IF MAINTI = 'B'                                           EL852
00380            NEXT SENTENCE                                          EL852
00381         ELSE                                                      EL852
00382            MOVE 'UPDATE'            TO SM-READ                    EL852
00383            PERFORM 9995-SECURITY-VIOLATION                        EL852
00384            MOVE ER-0070             TO EMI-ERROR                  EL852
00385            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL852
00386            GO TO 8100-SEND-INITIAL-MAP.                           EL852
00387                                                                   EL852
00388      IF FRCARL GREATER THAN ZERO OR                               EL852
00389         FRGRPL GREATER THAN ZERO OR                               EL852
00390         FRESPL GREATER THAN ZERO                                  EL852
00391         MOVE '1'                 TO PI-OPTION                     EL852
00392         ADD +1                   TO WS-OPTION-COUNTER.            EL852
00393                                                                   EL852
00394      IF ACCTCARL GREATER THAN ZERO OR                             EL852
00395         ACCTGRPL GREATER THAN ZERO OR                             EL852
00396         ACCTSTL  GREATER THAN ZERO OR                             EL852
00397         ACCOUNTL GREATER THAN ZERO OR                             EL852
00398         ACCTREFL GREATER THAN ZERO                                EL852
00399         MOVE '2'                 TO PI-OPTION                     EL852
00400         ADD +1                   TO WS-OPTION-COUNTER.            EL852
00401                                                                   EL852
00402      IF SUMMARYL GREATER THAN ZERO                                EL852
00403         MOVE '3'                 TO PI-OPTION                     EL852
00404         ADD +1                   TO WS-OPTION-COUNTER.            EL852
00405                                                                   EL852
00406      IF BATCHL   GREATER THAN ZERO                                EL852
00407         MOVE '4'                 TO PI-OPTION                     EL852
00408         ADD +1                   TO WS-OPTION-COUNTER.            EL852
00409                                                                   EL852
00410      IF WS-OPTION-COUNTER GREATER THAN +1                         EL852
00411         MOVE SPACE               TO PI-OPTION                     EL852
00412         MOVE ER-2919             TO EMI-ERROR                     EL852
00413         MOVE -1                  TO FRCARL                        EL852
00414         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL852
00415         GO TO 8200-SEND-DATAONLY.                                 EL852
00416                                                                   EL852
00417      IF (WS-SUBMIT-FUNCTION OR WS-RESUBMIT-FUNCTION)              EL852
00418         IF WS-OPTION-COUNTER GREATER THAN +0                      EL852
00419            NEXT SENTENCE                                          EL852
00420         ELSE                                                      EL852
00421            MOVE SPACE            TO PI-OPTION                     EL852
00422            MOVE ER-2935          TO EMI-ERROR                     EL852
00423            MOVE -1               TO MAINTL                        EL852
00424            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL852
00425            GO TO 8200-SEND-DATAONLY.                              EL852
00426                                                                   EL852
00427      IF PI-OPTION-ONE-SELECTED                                    EL852
00428         GO TO 1100-PROCESS-OPTION-1.                              EL852
00429                                                                   EL852
00430      IF PI-OPTION-TWO-SELECTED                                    EL852
00431         GO TO 1200-PROCESS-OPTION-2.                              EL852
00432                                                                   EL852
00433      IF PI-OPTION-THREE-SELECTED                                  EL852
00434         GO TO 1300-PROCESS-OPTION-3.                              EL852
00435                                                                   EL852
00436      MOVE '4'                    TO PI-OPTION.                    EL852
00437      GO TO 1400-PROCESS-OPTION-4.                                 EL852
00438                                                                   EL852
00439      EJECT                                                        EL852
00440                                                                   EL852
00441 ******************************************************************EL852
00442 *                                                                *EL852
00443 *           P R O C E S S   O P T I O N   O N E                  *EL852
00444 *                                                                *EL852
00445 ******************************************************************EL852
00446                                                                   EL852
00447  1100-PROCESS-OPTION-1.                                           EL852
00448                                                                   EL852
00449      MOVE LOW-VALUES             TO ERRQST-ALT-KEY2.              EL852
00450      MOVE PI-COMPANY-CD          TO ERRQST-COMPANY-CD-A2.         EL852
00451                                                                   EL852
00452      IF FRCARL GREATER THAN ZERO                                  EL852
00453         MOVE FRCARI              TO ERRQST-CARRIER-A2.            EL852
00454                                                                   EL852
00455      IF FRGRPL GREATER THAN ZERO                                  EL852
00456         MOVE FRGRPI              TO ERRQST-GROUPING-A2.           EL852
00457                                                                   EL852
00458      IF FRESPL GREATER THAN ZERO                                  EL852
00459         MOVE FRESPI              TO ERRQST-FIN-RESP-A2.           EL852
00460                                                                   EL852
00461      MOVE ERRQST-ALT-KEY2        TO PI-SELECT-KEY                 EL852
00462                                     PI-ACCESS-KEY.                EL852
00463                                                                   EL852
00464      MOVE FILE-ID-ERRQST3        TO PI-FILE-ID.                   EL852
00465                                                                   EL852
00466      PERFORM 4000-READ-REQUEST-FILE THRU 4090-EXIT.               EL852
00467                                                                   EL852
00468      IF ERRQST-CARRIER-A2 = RQ-CARRIER-A2                         EL852
00469         AND ERRQST-GROUPING-A2 = RQ-GROUPING-A2                   EL852
00470            AND ERRQST-FIN-RESP-A2 = RQ-FIN-RESP-A2                EL852
00471               NEXT SENTENCE                                       EL852
00472            ELSE                                                   EL852
00473               GO TO 1190-SELECTION-ERROR.                         EL852
00474                                                                   EL852
00475      IF (WS-SUBMIT-FUNCTION OR WS-RESUBMIT-FUNCTION)              EL852
00476         PERFORM 5000-PROCESS-REQUEST-FILE THRU 5090-EXIT.         EL852
00477                                                                   EL852
00478      MOVE EL852B              TO PI-MAP-NAME.                     EL852
00479      MOVE XCTL-EL8521         TO PGM-NAME.                        EL852
00480      GO TO 9300-XCTL.                                             EL852
00481                                                                   EL852
00482  1190-SELECTION-ERROR.                                            EL852
00483      MOVE SPACE                  TO PI-OPTION.                    EL852
00484      MOVE ER-2132                TO EMI-ERROR.                    EL852
00485      MOVE -1                     TO FRCARL.                       EL852
00486      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL852
00487      GO TO 8200-SEND-DATAONLY.                                    EL852
00488                                                                   EL852
00489      EJECT                                                        EL852
00490                                                                   EL852
00491 ******************************************************************EL852
00492 *                                                                *EL852
00493 *           P R O C E S S   O P T I O N   T W O                  *EL852
00494 *                                                                *EL852
00495 ******************************************************************EL852
00496                                                                   EL852
00497  1200-PROCESS-OPTION-2.                                           EL852
00498                                                                   EL852
00499      MOVE LOW-VALUES             TO ERRQST-ALT-KEY1.              EL852
00500      MOVE PI-COMPANY-CD          TO ERRQST-COMPANY-CD-A1.         EL852
00501                                                                   EL852
00502      IF ACCTCARL GREATER THAN ZERO                                EL852
00503         MOVE ACCTCARI            TO ERRQST-CARRIER-A1.            EL852
00504                                                                   EL852
00505      IF ACCTGRPL GREATER THAN ZERO                                EL852
00506         MOVE ACCTGRPI            TO ERRQST-GROUPING-A1.           EL852
00507                                                                   EL852
00508      IF ACCTSTL GREATER THAN ZERO                                 EL852
00509         MOVE ACCTSTI             TO ERRQST-STATE-A1.              EL852
00510                                                                   EL852
00511      IF ACCOUNTL GREATER THAN ZERO                                EL852
00512         MOVE ACCOUNTI            TO ERRQST-ACCOUNT-A1.            EL852
00513                                                                   EL852
00514      IF ACCTREFL GREATER THAN ZERO                                EL852
00515         MOVE ACCTREFI            TO ERRQST-REFERENCE-A1.          EL852
00516                                                                   EL852
00517      MOVE FILE-ID-ERRQST2        TO PI-FILE-ID.                   EL852
00518      MOVE ERRQST-ALT-KEY1        TO PI-ACCESS-KEY                 EL852
00519                                     PI-SELECT-KEY.                EL852
00520                                                                   EL852
00521      PERFORM 4000-READ-REQUEST-FILE THRU 4090-EXIT.               EL852
00522                                                                   EL852
00523      IF ERRQST-CARRIER-A1 = RQ-CARRIER-A1                         EL852
00524         AND ERRQST-GROUPING-A1 = RQ-GROUPING-A1                   EL852
00525            AND ERRQST-STATE-A1 = RQ-STATE-A1                      EL852
00526               AND ERRQST-ACCOUNT-A1 = RQ-ACCOUNT-A1               EL852
00527                     NEXT SENTENCE                                 EL852
00528                  ELSE                                             EL852
00529                     GO TO 1290-SELECTION-ERROR.                   EL852
00530                                                                   EL852
00531      IF ACCTREFL GREATER THAN ZERO                                EL852
00532         IF ERRQST-REFERENCE-A1 = RQ-REFERENCE-A1                  EL852
00533            NEXT SENTENCE                                          EL852
00534         ELSE                                                      EL852
00535            GO TO 1290-SELECTION-ERROR.                            EL852
00536                                                                   EL852
00537      IF (WS-SUBMIT-FUNCTION OR WS-RESUBMIT-FUNCTION)              EL852
00538         PERFORM 5000-PROCESS-REQUEST-FILE THRU 5090-EXIT.         EL852
00539                                                                   EL852
00540                                                                   EL852
00541      MOVE EL852C                 TO PI-MAP-NAME.                  EL852
00542      MOVE XCTL-EL8521            TO PGM-NAME.                     EL852
00543      GO TO 9300-XCTL.                                             EL852
00544                                                                   EL852
00545  1290-SELECTION-ERROR.                                            EL852
00546                                                                   EL852
00547      MOVE SPACE                  TO PI-OPTION.                    EL852
00548      MOVE ER-2132                TO EMI-ERROR.                    EL852
00549      MOVE -1                     TO ACCTCARL.                     EL852
00550      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL852
00551      GO TO 8200-SEND-DATAONLY.                                    EL852
00552                                                                   EL852
00553      EJECT                                                        EL852
00554                                                                   EL852
00555 ******************************************************************EL852
00556 *                                                                *EL852
00557 *           P R O C E S S   O P T I O N   T H R E E              *EL852
00558 *                                                                *EL852
00559 ******************************************************************EL852
00560                                                                   EL852
00561  1300-PROCESS-OPTION-3.                                           EL852
00562                                                                   EL852
00563      MOVE LOW-VALUES             TO ERRQST-ALT-KEY4.              EL852
00564      MOVE PI-COMPANY-CD          TO ERRQST-COMPANY-CD-A4.         EL852
00565                                                                   EL852
00566      IF SUMMARYL GREATER THAN ZERO                                EL852
00567         MOVE SUMMARYI            TO ERRQST-SUMMARY-CODE.          EL852
00568                                                                   EL852
00569                                                                   EL852
00570      IF (WS-SUBMIT-FUNCTION OR WS-RESUBMIT-FUNCTION)              EL852
00571         PERFORM 6000-PROCESS-SUMMARY-FILE THRU 6090-EXIT.         EL852
00572                                                                   EL852
00573      MOVE FILE-ID-ERRQST5        TO PI-FILE-ID.                   EL852
00574      MOVE ERRQST-ALT-KEY4        TO PI-SELECT-KEY                 EL852
00575                                     PI-ACCESS-KEY.                EL852
00576                                                                   EL852
00577      PERFORM 4000-READ-REQUEST-FILE THRU 4090-EXIT.               EL852
00578                                                                   EL852
00579      IF ERRQST-SUMMARY-CODE = RQ-SUMMARY-CODE                     EL852
00580         NEXT SENTENCE                                             EL852
00581      ELSE                                                         EL852
00582         GO TO 1390-SELECTION-ERROR.                               EL852
00583                                                                   EL852
00584      MOVE EL852B                 TO PI-MAP-NAME                   EL852
00585      MOVE XCTL-EL8521            TO PGM-NAME                      EL852
00586      GO TO 9300-XCTL.                                             EL852
00587                                                                   EL852
00588  1390-SELECTION-ERROR.                                            EL852
00589                                                                   EL852
00590      MOVE SPACE                  TO PI-OPTION.                    EL852
00591      MOVE ER-2132                TO EMI-ERROR.                    EL852
00592      MOVE -1                     TO SUMMARYL.                     EL852
00593      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL852
00594      GO TO 8200-SEND-DATAONLY.                                    EL852
00595                                                                   EL852
00596      EJECT                                                        EL852
00597                                                                   EL852
00598 ******************************************************************EL852
00599 *                                                                *EL852
00600 *           P R O C E S S   O P T I O N   F O U R                *EL852
00601 *                                                                *EL852
00602 ******************************************************************EL852
00603                                                                   EL852
00604  1400-PROCESS-OPTION-4.                                           EL852
00605                                                                   EL852
00606      MOVE LOW-VALUES             TO ERRQST-KEY.                   EL852
00607      MOVE PI-COMPANY-CD          TO ERRQST-COMPANY-CD.            EL852
00608                                                                   EL852
00609      IF BATCHL GREATER THAN ZERO                                  EL852
00610         MOVE BATCHI              TO ERRQST-ENTRY-BATCH.           EL852
00611                                                                   EL852
00612      MOVE FILE-ID-ERRQST         TO PI-FILE-ID.                   EL852
00613      MOVE ERRQST-KEY             TO PI-ACCESS-KEY                 EL852
00614                                     PI-SELECT-KEY.                EL852
00615                                                                   EL852
00616      PERFORM 4000-READ-REQUEST-FILE THRU 4090-EXIT.               EL852
00617                                                                   EL852
00618      IF ERRQST-ENTRY-BATCH = LOW-VALUES                           EL852
00619         MOVE EL852B                 TO PI-MAP-NAME                EL852
00620         MOVE XCTL-EL8521            TO PGM-NAME                   EL852
00621         GO TO 9300-XCTL.                                          EL852
00622                                                                   EL852
00623      IF ERRQST-ENTRY-BATCH = RQ-ENTRY-BATCH                       EL852
00624         NEXT SENTENCE                                             EL852
00625      ELSE                                                         EL852
00626         GO TO 1490-SELECTION-ERROR.                               EL852
00627                                                                   EL852
00628      IF (WS-SUBMIT-FUNCTION OR WS-RESUBMIT-FUNCTION)              EL852
00629         PERFORM 5000-PROCESS-REQUEST-FILE THRU 5090-EXIT.         EL852
00630                                                                   EL852
00631      MOVE EL852B                 TO PI-MAP-NAME.                     CL**2
00632      MOVE XCTL-EL8521            TO PGM-NAME                      EL852
00633      GO TO 9300-XCTL.                                             EL852
00634                                                                   EL852
00635  1490-SELECTION-ERROR.                                            EL852
00636                                                                   EL852
00637      MOVE SPACE                  TO PI-OPTION.                    EL852
00638      MOVE ER-2132                TO EMI-ERROR.                    EL852
00639      MOVE -1                     TO BATCHL.                       EL852
00640      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL852
00641      GO TO 8200-SEND-DATAONLY.                                    EL852
00642                                                                   EL852
00643      EJECT                                                        EL852
00644                                                                   EL852
00645 ******************************************************************EL852
00646 *                                                                *EL852
00647 *           R E A D   R E Q U E S T   F I L E                    *EL852
00648 *                                                                *EL852
00649 ******************************************************************EL852
00650                                                                   EL852
00651  4000-READ-REQUEST-FILE.                                          EL852
00652                                                                   EL852
00653      EXEC CICS HANDLE CONDITION                                   EL852
00654          NOTFND  (4080-REQUEST-NOTFND)                            EL852
00655          ENDFILE (4080-REQUEST-NOTFND)                            EL852
00656      END-EXEC.                                                    EL852
00657                                                                   EL852
00658      EXEC CICS READ                                               EL852
00659          SET    (ADDRESS OF AR-REQUEST-RECORD)                       CL**7
00660          DATASET(PI-FILE-ID)                                      EL852
00661          RIDFLD (PI-ACCESS-KEY)                                   EL852
00662          GTEQ                                                     EL852
00663      END-EXEC.                                                    EL852
00664                                                                   EL852
00665      IF RQ-COMPANY-CD NOT = PI-COMPANY-CD                         EL852
00666         GO TO 4080-REQUEST-NOTFND.                                EL852
00667                                                                   EL852
00668      GO TO 4090-EXIT.                                             EL852
00669                                                                   EL852
00670  4080-REQUEST-NOTFND.                                             EL852
00671                                                                   EL852
00672       MOVE SPACE                 TO PI-OPTION.                    EL852
00673       MOVE ER-2132               TO EMI-ERROR.                    EL852
00674       MOVE -1                    TO FRCARL.                       EL852
00675       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   EL852
00676       GO TO 8200-SEND-DATAONLY.                                   EL852
00677                                                                   EL852
00678  4090-EXIT.                                                       EL852
00679       EXIT.                                                       EL852
00680      EJECT                                                        EL852
00681                                                                   EL852
00682 ******************************************************************EL852
00683 *                                                                *EL852
00684 *           P R O C E S S   R E Q U E S T   F I L E              *EL852
00685 *                                                                *EL852
00686 *   . BROWSE THE REQUEST FILE FOR THE SELECTED OPTION AND        *EL852
00687 *     SET THE REQUEST DATE TO THE CURRENT DATE.                  *EL852
00688 *                                                                *EL852
00689 *   . IF THE OPTION IS BEING RE-REQUESTED, FLAG THEM AS SUCH.    *EL852
00690 *                                                                *EL852
00691 ******************************************************************EL852
00692                                                                   EL852
00693  5000-PROCESS-REQUEST-FILE.                                       EL852
00694                                                                   EL852
00695      IF WS-REQUEST-BROWSE-STARTED                                 EL852
00696         EXEC CICS HANDLE CONDITION                                EL852
00697              NOTFND  (5070-REQUEST-PROCESSED)                     EL852
00698              ENDFILE (5070-REQUEST-PROCESSED)                     EL852
00699         END-EXEC                                                  EL852
00700      ELSE                                                         EL852
00701         EXEC CICS HANDLE CONDITION                                EL852
00702              NOTFND  (5080-REQUEST-NOTFND)                        EL852
00703              ENDFILE (5080-REQUEST-NOTFND)                        EL852
00704         END-EXEC.                                                 EL852
00705                                                                   EL852
00706      EXEC CICS STARTBR                                            EL852
00707          DATASET (PI-FILE-ID)                                     EL852
00708          RIDFLD  (PI-ACCESS-KEY)                                  EL852
00709      END-EXEC.                                                    EL852
00710                                                                   EL852
00711  5020-READ-REQUEST-FILE.                                          EL852
00712                                                                   EL852
00713      EXEC CICS READNEXT                                           EL852
00714          SET    (ADDRESS OF AR-REQUEST-RECORD)                       CL**7
00715          DATASET(PI-FILE-ID)                                      EL852
00716          RIDFLD (PI-ACCESS-KEY)                                   EL852
00717      END-EXEC.                                                    EL852
00718                                                                   EL852
00719      MOVE 'Y'                    TO WS-REQUEST-BROWSE-SW.         EL852
00720                                                                   EL852
00721      IF RQ-COMPANY-CD NOT = PI-COMPANY-CD                         EL852
00722         GO TO 5070-REQUEST-PROCESSED.                             EL852
00723                                                                   EL852
00724      IF WS-SAVE-ACCESS-KEY = PI-ACCESS-KEY                        EL852
00725         GO TO 5020-READ-REQUEST-FILE.                             EL852
00726                                                                   EL852
00727      MOVE PI-ACCESS-KEY          TO WS-SAVE-ACCESS-KEY.           EL852
00728                                                                   EL852
00729      IF RQ-REQUEST-DT GREATER THAN LOW-VALUES OR                     CL**4
00730         RQ-REQUEST-DT = SPACES                                       CL**4
00731         IF MAINTI = 'R'                                           EL852
00732            NEXT SENTENCE                                          EL852
00733         ELSE                                                      EL852
00734            GO TO 5020-READ-REQUEST-FILE.                          EL852
00735                                                                   EL852
00736      IF RQ-MO-END-DT GREATER THAN PI-AR-MONTH-END-DT                 CL**2
00737         GO TO 5020-READ-REQUEST-FILE.                             EL852
00738                                                                   EL852
00739      IF PI-OPTION-ONE-SELECTED                                    EL852
00740         MOVE RQ-CONTROL-BY-FIN-RESP TO WS-RQST-FIN-RESP-CNTL      EL852
00741         IF WS-RQST-FIN-RESP-CNTL GREATER THAN                     EL852
00742               ERRQST-FIN-RESP-CONTROL                             EL852
00743            GO TO 5070-REQUEST-PROCESSED.                          EL852
00744                                                                   EL852
00745      IF PI-OPTION-TWO-SELECTED                                    EL852
00746         MOVE RQ-CONTROL-BY-ACCT-REF TO WS-RQST-ACCT-CONTROL       EL852
00747         IF WS-RQST-ACCT-CONTROL GREATER THAN ERRQST-ACCT-CONTROL  EL852
00748            GO TO 5070-REQUEST-PROCESSED.                          EL852
00749                                                                   EL852
00750      IF PI-OPTION-TWO-SELECTED                                    EL852
00751         IF ERRQST-REFERENCE-A1 GREATER THAN LOW-VALUES            EL852
00752            IF ERRQST-REFERENCE-A1 = RQ-REFERENCE-A1               EL852
00753               NEXT SENTENCE                                       EL852
00754            ELSE                                                   EL852
00755               GO TO 5070-REQUEST-PROCESSED.                       EL852
00756                                                                   EL852
00757      IF NOT PI-OPTION-THREE-SELECTED                              EL852
00758         NEXT SENTENCE                                             EL852
00759      ELSE                                                         EL852
00760         IF  RQ-CARRIER-A2 = ERRQST-CARRIER-A2                     EL852
00761             AND RQ-GROUPING-A2 = ERRQST-GROUPING-A2               EL852
00762                 AND RQ-FIN-RESP-A2 = ERRQST-FIN-RESP-A2           EL852
00763                     AND RQ-ACCT-AGENT-A2 = ERRQST-ACCT-AGENT-A2   EL852
00764                         NEXT SENTENCE                             EL852
00765         ELSE                                                      EL852
00766             GO TO 5070-REQUEST-PROCESSED.                         EL852
00767                                                                   EL852
00768      IF PI-OPTION-FOUR-SELECTED                                   EL852
00769         IF ERRQST-ENTRY-BATCH = RQ-ENTRY-BATCH                    EL852
00770            NEXT SENTENCE                                          EL852
00771         ELSE                                                      EL852
00772            GO TO 5070-REQUEST-PROCESSED.                          EL852
00773                                                                   EL852
00774      MOVE RQ-CONTROL-PRIMARY     TO ERRQST-KEY.                   EL852
00775                                                                   EL852
00776      EXEC CICS ENDBR                                              EL852
00777          DATASET (PI-FILE-ID)                                     EL852
00778      END-EXEC.                                                    EL852
00779                                                                   EL852
00780      EXEC CICS READ                                               EL852
00781          SET     (ADDRESS OF AR-REQUEST-RECORD)                      CL**7
00782          DATASET (FILE-ID-ERRQST)                                 EL852
00783          RIDFLD  (ERRQST-KEY)                                     EL852
00784          UPDATE                                                   EL852
00785      END-EXEC.                                                    EL852
00786                                                                   EL852
00787      MOVE 'B'                    TO JP-RECORD-TYPE.               EL852
00788      MOVE AR-REQUEST-RECORD      TO JP-RECORD-AREA.               EL852
00789                                                                   EL852
00790      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL852
00791                                                                   EL852
00792      MOVE 'Y'                    TO WS-DATA-SELECTED-SW.             CL**5
00793                                                                      CL**5
00794      IF PI-OPTION-THREE-SELECTED                                     CL**5
00795         MOVE SX-SUMMARY          TO RQ-SUMMARY-CODE.                 CL**5
00796                                                                      CL**5
00797      IF RQ-STATUS = 'E'                                           EL852
00798          GO TO 5050-WRITE.                                           CL**6
00799                                                                      CL**6
00800      IF (RQ-CARRIER-A1  = SPACES OR LOW-VALUES) OR                   CL**6
00801         (RQ-GROUPING-A1 = SPACES OR LOW-VALUES) OR                   CL**6
00802         (RQ-STATE-A1    = SPACES OR LOW-VALUES)                      CL**6
00803          GO TO 5050-WRITE.                                           CL**5
00804                                                                      CL**3
00805      IF WS-RESUBMIT-FUNCTION                                      EL852
00806          IF RQ-STMT-DT = LOW-VALUES                                  CL**3
00807              MOVE ' '            TO RQ-STATUS                        CL**3
00808          ELSE                                                        CL**3
00809              MOVE 'R'            TO RQ-STATUS.                       CL**3
00810                                                                   EL852
00811      MOVE PI-PROCESSOR-ID        TO RQ-PROCESSOR-ID.              EL852
00812      MOVE WS-CURRENT-BIN-DT      TO RQ-REQUEST-DT.                EL852
00813                                                                   EL852
00814      IF PI-OPTION-ONE-SELECTED                                    EL852
00815         MOVE 'F'                 TO RQ-REQUEST-METHOD.            EL852
00816                                                                   EL852
00817      IF PI-OPTION-TWO-SELECTED                                    EL852
00818         MOVE 'A'                 TO RQ-REQUEST-METHOD.            EL852
00819                                                                   EL852
00820      IF PI-OPTION-THREE-SELECTED                                  EL852
00821         MOVE 'S'                 TO RQ-REQUEST-METHOD.            EL852
00822                                                                   EL852
00823      IF PI-OPTION-FOUR-SELECTED                                   EL852
00824         MOVE 'B'                 TO RQ-REQUEST-METHOD.            EL852
00825                                                                      CL**5
00826  5050-WRITE.                                                         CL**5
00827                                                                   EL852
00828      MOVE 'C'                    TO JP-RECORD-TYPE.               EL852
00829      MOVE AR-REQUEST-RECORD      TO JP-RECORD-AREA.               EL852
00830                                                                   EL852
00831      EXEC CICS REWRITE                                            EL852
00832          DATASET (FILE-ID-ERRQST)                                 EL852
00833          FROM    (AR-REQUEST-RECORD)                              EL852
00834      END-EXEC.                                                    EL852
00835                                                                   EL852
00836      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL852
00837                                                                   EL852
00838      GO TO 5000-PROCESS-REQUEST-FILE.                             EL852
00839                                                                   EL852
00840  5070-REQUEST-PROCESSED.                                          EL852
00841                                                                   EL852
00842      EXEC CICS ENDBR                                              EL852
00843          DATASET (PI-FILE-ID)                                     EL852
00844      END-EXEC.                                                    EL852
00845                                                                   EL852
00846      IF PI-OPTION-FOUR-SELECTED                                   EL852
00847         GO TO 5090-EXIT.                                          EL852
00848                                                                   EL852
00849      IF PI-OPTION-THREE-SELECTED                                     CL**4
00850         GO TO 5090-EXIT.                                             CL**4
00851                                                                      CL**4
00852      IF WS-DATA-SELECTED                                          EL852
00853         GO TO 5090-EXIT.                                          EL852
00854                                                                   EL852
00855      MOVE SPACE                 TO PI-OPTION.                     EL852
00856      MOVE ER-2134               TO EMI-ERROR.                     EL852
00857      MOVE -1                    TO FRCARL.                        EL852
00858      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL852
00859      GO TO 8200-SEND-DATAONLY.                                    EL852
00860                                                                   EL852
00861  5080-REQUEST-NOTFND.                                             EL852
00862                                                                   EL852
00863      IF PI-OPTION-FOUR-SELECTED                                   EL852
00864         GO TO 5090-EXIT.                                          EL852
00865                                                                   EL852
00866       MOVE SPACE                 TO PI-OPTION.                    EL852
00867       MOVE ER-2134               TO EMI-ERROR.                    EL852
00868       MOVE -1                    TO FRCARL.                       EL852
00869       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   EL852
00870       GO TO 8200-SEND-DATAONLY.                                   EL852
00871                                                                   EL852
00872  5090-EXIT.                                                       EL852
00873       EXIT.                                                       EL852
00874                                                                   EL852
00875      EJECT                                                        EL852
00876                                                                   EL852
00877                                                                   EL852
00878 ******************************************************************EL852
00879 *                                                                *EL852
00880 *           P R O C E S S   S U M M A R Y   F I L E              *EL852
00881 *                                                                *EL852
00882 *   . BROWSE THE SUMMARY FILE FOR THE SELECTED SUMMARY CODE.     *EL852
00883 *                                                                *EL852
00884 *   . BROWSE THE REQUEST FILE FOR THE SELECTED SUMMARY CODE.     *EL852
00885 *                                                                *EL852
00886 *   . UPDATE THE THOSE REQUEST RECORDS THAT ARE ASSOCIATED       *EL852
00887 *     WITH THE SELECTED SUMMARY RECORDS.                         *EL852
00888 *                                                                *EL852
00889 ******************************************************************EL852
00890                                                                   EL852
00891  6000-PROCESS-SUMMARY-FILE.                                       EL852
00892                                                                   EL852
00893       EXEC CICS HANDLE CONDITION                                  EL852
00894            NOTFND  (6080-REQUEST-NOTFND)                          EL852
00895            ENDFILE (6080-REQUEST-NOTFND)                          EL852
00896       END-EXEC.                                                   EL852
00897                                                                   EL852
00898      MOVE ERRQST-ALT-KEY4        TO WS-ACCESS-KEY.                EL852
00899                                                                   EL852
00900      EXEC CICS STARTBR                                            EL852
00901          DATASET (FILE-ID-ERSUMM)                                 EL852
00902          RIDFLD  (WS-ACCESS-KEY)                                  EL852
00903      END-EXEC.                                                    EL852
00904                                                                   EL852
00905      EXEC CICS HANDLE CONDITION                                   EL852
00906           NOTFND  (6070-REQUEST-PROCESSED)                        EL852
00907           ENDFILE (6070-REQUEST-PROCESSED)                        EL852
00908      END-EXEC.                                                    EL852
00909                                                                   EL852
00910  6020-READ-SUMMARY-FILE.                                          EL852
00911                                                                   EL852
00912      EXEC CICS READNEXT                                           EL852
00913          SET    (ADDRESS OF SUMM-CROSS-REFERENCE)                    CL**7
00914          DATASET(FILE-ID-ERSUMM)                                  EL852
00915          RIDFLD (WS-ACCESS-KEY)                                   EL852
00916      END-EXEC.                                                    EL852
00917                                                                   EL852
00918      MOVE 'Y'                    TO WS-SUMMARY-BROWSE-SW.         EL852
00919                                                                   EL852
00920      IF SX-COMPANY-CD NOT = PI-COMPANY-CD                         EL852
00921         GO TO 6070-REQUEST-PROCESSED.                             EL852
00922                                                                   EL852
00923      IF SX-SUMMARY GREATER THAN ERRQST-SUMMARY-CODE               EL852
00924         GO TO 6070-REQUEST-PROCESSED.                             EL852
00925                                                                   EL852
00926 ******************************************************************EL852
00927 *           BYPASS SUMMARY NAME RECORD                           *EL852
00928 ******************************************************************EL852
00929                                                                   EL852
00930      IF SX-CARRIER = LOW-VALUES                                   EL852
00931         GO 6020-READ-SUMMARY-FILE.                                EL852
00932                                                                   EL852
00933      MOVE LOW-VALUES            TO ERRQST-ALT-KEY2.               EL852
00934      MOVE SX-COMPANY-CD         TO ERRQST-COMPANY-CD-A2.          EL852
00935      MOVE SX-CARRIER            TO ERRQST-CARRIER-A2.             EL852
00936      MOVE SX-GROUP              TO ERRQST-GROUPING-A2.            EL852
00937      MOVE SX-FIN-RESP           TO ERRQST-FIN-RESP-A2.            EL852
00938      MOVE SX-ACCT-AGENT         TO ERRQST-ACCT-AGENT-A2.          EL852
00939      MOVE ERRQST-ALT-KEY2       TO PI-ACCESS-KEY.                 EL852
00940      MOVE FILE-ID-ERRQST3       TO PI-FILE-ID.                    EL852
00941                                                                      CL**4
00942      MOVE LOW-VALUES TO WS-SAVE-ACCESS-KEY.                          CL**4
00943                                                                   EL852
00944      PERFORM 5000-PROCESS-REQUEST-FILE THRU 5090-EXIT.            EL852
00945                                                                   EL852
00946      GO TO 6020-READ-SUMMARY-FILE.                                EL852
00947                                                                   EL852
00948  6070-REQUEST-PROCESSED.                                          EL852
00949                                                                   EL852
00950      EXEC CICS ENDBR                                              EL852
00951          DATASET (FILE-ID-ERSUMM)                                 EL852
00952      END-EXEC.                                                    EL852
00953                                                                   EL852
00954      IF WS-DATA-SELECTED                                          EL852
00955         GO TO 6090-EXIT.                                          EL852
00956                                                                   EL852
00957      MOVE SPACE                 TO PI-OPTION.                     EL852
00958      MOVE ER-2134               TO EMI-ERROR.                     EL852
00959      MOVE -1                    TO FRCARL.                        EL852
00960      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL852
00961      GO TO 8200-SEND-DATAONLY.                                    EL852
00962                                                                   EL852
00963  6080-REQUEST-NOTFND.                                             EL852
00964                                                                   EL852
00965      MOVE SPACE                 TO PI-OPTION.                     EL852
00966      MOVE ER-3133               TO EMI-ERROR.                     EL852
00967      MOVE -1                    TO FRCARL.                        EL852
00968      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL852
00969      GO TO 8200-SEND-DATAONLY.                                    EL852
00970                                                                   EL852
00971  6090-EXIT.                                                       EL852
00972       EXIT.                                                       EL852
00973                                                                   EL852
00974      EJECT                                                        EL852
00975 ******************************************************************EL852
00976 *                                                                *EL852
00977 *            S  E N D    I N I T I A L   M A P                   *EL852
00978 *                                                                *EL852
00979 ******************************************************************EL852
00980                                                                   EL852
00981  8100-SEND-INITIAL-MAP.                                           EL852
00982                                                                   EL852
00983      MOVE WS-CURRENT-DT          TO DATEO.                        EL852
00984      MOVE EIBTIME                TO TIME-IN.                      EL852
00985      MOVE TIME-OUT               TO TIMEO.                        EL852
00986      MOVE -1                     TO MAINTL.                       EL852
00987                                                                   EL852
00988      MOVE EMI-MESSAGE-AREA (1)   TO ERMESGO.                      EL852
00989                                                                   EL852
00990      EXEC CICS SEND                                               EL852
00991          MAP      (EL852A)                                        EL852
00992          MAPSET   (MAPSET-EL852S)                                 EL852
00993          FROM     (EL852AI)                                       EL852
00994          ERASE                                                    EL852
00995          CURSOR                                                   EL852
00996      END-EXEC.                                                    EL852
00997                                                                   EL852
00998      GO TO 9100-RETURN-TRAN.                                      EL852
00999                                                                   EL852
01000      EJECT                                                        EL852
01001                                                                   EL852
01002 ******************************************************************EL852
01003 *                                                                *EL852
01004 *              S E N D    D A T A O N L Y                        *EL852
01005 *                                                                *EL852
01006 ******************************************************************EL852
01007                                                                   EL852
01008  8200-SEND-DATAONLY.                                              EL852
01009                                                                   EL852
01010      MOVE WS-CURRENT-DT          TO DATEO.                        EL852
01011      MOVE EIBTIME                TO TIME-IN.                      EL852
01012      MOVE TIME-OUT               TO TIMEO.                        EL852
01013                                                                   EL852
01014      MOVE EMI-MESSAGE-AREA (1)   TO ERMESGO.                      EL852
01015                                                                   EL852
01016      EXEC CICS SEND                                               EL852
01017           MAP      (EL852A)                                       EL852
01018           MAPSET   (MAPSET-EL852S)                                EL852
01019           FROM     (EL852AI)                                      EL852
01020           DATAONLY                                                EL852
01021           CURSOR                                                  EL852
01022      END-EXEC.                                                    EL852
01023                                                                   EL852
01024      GO TO 9100-RETURN-TRAN.                                      EL852
01025                                                                   EL852
01026      EJECT                                                        EL852
01027                                                                   EL852
01028  8300-SEND-TEXT.                                                  EL852
01029      EXEC CICS SEND TEXT                                          EL852
01030          FROM     (LOGOFF-TEXT)                                   EL852
01031          LENGTH   (LOGOFF-LENGTH)                                 EL852
01032          ERASE                                                    EL852
01033          FREEKB                                                   EL852
01034      END-EXEC.                                                    EL852
01035                                                                   EL852
01036      EXEC CICS RETURN                                             EL852
01037      END-EXEC.                                                    EL852
01038                                                                   EL852
01039                                                                   EL852
01040  8400-LOG-JOURNAL-RECORD.                                         EL852
01041      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   EL852
01042      MOVE THIS-PGM                TO JP-PROGRAM-ID.               EL852
01043                                                                   EL852
01044 *    EXEC CICS JOURNAL                                            EL852
01045 *        JFILEID     (PI-JOURNAL-FILE-ID)                         EL852
01046 *        JTYPEID     ('EL')                                       EL852
01047 *        FROM        (JOURNAL-RECORD)                             EL852
01048 *        LENGTH      (WS-JOURNAL-RECORD-LENGTH)                   EL852
01049 *        END-EXEC.                                                EL852
01050                                                                   EL852
01051  8500-DATE-CONVERT.                                               EL852
01052      EXEC CICS LINK                                               EL852
01053          PROGRAM  (LINK-ELDATCV)                                  EL852
01054          COMMAREA (DATE-CONVERSION-DATA)                          EL852
01055          LENGTH   (DC-COMM-LENGTH)                                EL852
01056      END-EXEC.                                                    EL852
01057                                                                   EL852
01058  8500-EXIT.                                                       EL852
01059      EXIT.                                                        EL852
01060                                                                   EL852
01061      EJECT                                                        EL852
01062                                                                   EL852
01063  8800-UNAUTHORIZED-ACCESS.                                        EL852
01064      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL852
01065      GO TO 8300-SEND-TEXT.                                        EL852
01066                                                                   EL852
01067  8810-PF23.                                                       EL852
01068      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL852
01069      MOVE XCTL-EL005             TO PGM-NAME.                     EL852
01070      GO TO 9300-XCTL.                                             EL852
01071                                                                   EL852
01072  9200-RETURN-MAIN-MENU.                                           EL852
01073      MOVE XCTL-EL626             TO PGM-NAME.                     EL852
01074      GO TO 9300-XCTL.                                             EL852
01075                                                                   EL852
01076  9000-RETURN-CICS.                                                EL852
01077      EXEC CICS RETURN                                             EL852
01078      END-EXEC.                                                    EL852
01079                                                                   EL852
01080  9100-RETURN-TRAN.                                                EL852
01081                                                                   EL852
01082      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL852
01083      MOVE '852A'                 TO PI-CURRENT-SCREEN-NO.         EL852
01084                                                                   EL852
01085      EXEC CICS RETURN                                             EL852
01086          TRANSID    (TRANS-EXJ2)                                  EL852
01087          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL852
01088          LENGTH     (PI-COMM-LENGTH)                              EL852
01089      END-EXEC.                                                    EL852
01090                                                                   EL852
01091  9300-XCTL.                                                       EL852
01092      EXEC CICS XCTL                                               EL852
01093          PROGRAM    (PGM-NAME)                                    EL852
01094          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL852
01095          LENGTH     (PI-COMM-LENGTH)                              EL852
01096      END-EXEC.                                                    EL852
01097                                                                   EL852
01098  9400-CLEAR.                                                      EL852
01099      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME                      EL852
01100      GO TO 9300-XCTL.                                             EL852
01101                                                                   EL852
01102  9500-PF12.                                                       EL852
01103      MOVE XCTL-EL010             TO PGM-NAME.                     EL852
01104      GO TO 9300-XCTL.                                             EL852
01105                                                                   EL852
01106  9600-PGMID-ERROR.                                                EL852
01107                                                                   EL852
01108      EXEC CICS HANDLE CONDITION                                   EL852
01109          PGMIDERR    (8300-SEND-TEXT)                             EL852
01110      END-EXEC.                                                    EL852
01111                                                                   EL852
01112      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL852
01113      MOVE ' '                    TO PI-ENTRY-CD-1.                EL852
01114      MOVE XCTL-EL005             TO PGM-NAME.                     EL852
01115      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL852
01116      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL852
01117      GO TO 9300-XCTL.                                             EL852
01118                                                                   EL852
01119  9900-ERROR-FORMAT.                                               EL852
01120                                                                   EL852
01121      IF NOT EMI-ERRORS-COMPLETE                                   EL852
01122          MOVE LINK-EL001         TO PGM-NAME                      EL852
01123          EXEC CICS LINK                                           EL852
01124              PROGRAM    (PGM-NAME)                                EL852
01125              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL852
01126              LENGTH     (EMI-COMM-LENGTH)                         EL852
01127          END-EXEC.                                                EL852
01128                                                                   EL852
01129  9900-EXIT.                                                       EL852
01130      EXIT.                                                        EL852
01131                                                                   EL852
01132  9990-ABEND.                                                      EL852
01133      MOVE LINK-EL004             TO PGM-NAME.                     EL852
01134      MOVE DFHEIBLK               TO EMI-LINE1.                    EL852
01135      EXEC CICS LINK                                               EL852
01136          PROGRAM   (PGM-NAME)                                     EL852
01137          COMMAREA  (EMI-LINE1)                                    EL852
01138          LENGTH    (72)                                           EL852
01139      END-EXEC.                                                    EL852
01140                                                                   EL852
01141      MOVE -1                     TO PFENTERL.                     EL852
01142                                                                   EL852
01143      GO TO 8200-SEND-DATAONLY.                                    EL852
01144                                                                   EL852
01145      GOBACK.                                                      EL852
01146                                                                   EL852
01147      EJECT                                                        EL852
01148                                                                   EL852
01149  9995-SECURITY-VIOLATION.                                         EL852
01150                              COPY ELCSCTP.                        EL852
01151                                                                   EL852
01152  9995-EXIT.                                                       EL852
01153      EXIT.                                                        EL852
01154                                                                   EL852
