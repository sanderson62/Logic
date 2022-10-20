00001  ID DIVISION.                                                     03/06/96
00002                                                                   EL679
00003  PROGRAM-ID.                 EL679.                                  LV003
00004 *              PROGRAM CONVERTED BY                                  CL**3
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**3
00006 *              CONVERSION DATE 02/14/96 07:57:55.                    CL**3
00007 *                            VMOD=2.003                              CL**3
00008 *                                                                 EL679
00008 *                                                                 EL679
00009 *AUTHOR.     LOGIC,INC.                                              CL**3
00010 *            DALLAS, TEXAS.                                          CL**3
00011                                                                   EL679
00012 *DATE-COMPILED.                                                      CL**3
00013 *SECURITY.   *****************************************************   CL**3
00014 *            *                                                   *   CL**3
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**3
00016 *            *                                                   *   CL**3
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**3
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**3
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**3
00020 *            *                                                   *   CL**3
00021 *            *****************************************************   CL**3
00022                                                                   EL679
00023 *REMARKS.    TRANSACTION - EXF5 - BILLING STATUS REPORT.             CL**3
00024 *            PAYMENT SCREEN.                                         CL**3
00025  ENVIRONMENT DIVISION.                                            EL679
00026                                                                   EL679
00027      EJECT                                                        EL679
00028  DATA DIVISION.                                                   EL679
00029  WORKING-STORAGE SECTION.                                         EL679
00030  77  FILLER  PIC X(32)  VALUE '********************************'. EL679
00031  77  FILLER  PIC X(32)  VALUE '*     EL679 WORKING STORAGE    *'. EL679
00032  77  FILLER  PIC X(32)  VALUE '************ V/M 2.003 *********'.    CL**3
00033                                                                   EL679
00034  01  WS-DATE-AREA.                                                EL679
00035      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL679
00036      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL679
00037                                                                   EL679
00038  01  STANDARD-AREAS.                                              EL679
00039      12  MAP-NAME            PIC X(8)    VALUE 'EL679A'.          EL679
00040      12  MAPSET-NAME         PIC X(8)    VALUE 'EL679S'.          EL679
00041      12  SCREEN-NUMBER       PIC X(4)    VALUE '640C'.            EL679
00042      12  TRANS-ID            PIC X(4)    VALUE 'EXF5'.            EL679
00043      12  TRANS-6791          PIC X(4)    VALUE 'EXG5'.            EL679
00044      12  THIS-PGM            PIC X(8)    VALUE 'EL679'.           EL679
00045      12  PGM-NAME            PIC X(8).                            EL679
00046      12  TIME-IN             PIC S9(7).                           EL679
00047      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL679
00048          16  FILLER          PIC X.                               EL679
00049          16  TIME-OUT        PIC 99V99.                           EL679
00050          16  FILLER          PIC X(2).                            EL679
00051      12  XCTL-005            PIC X(8)    VALUE 'EL005'.           EL679
00052      12  XCTL-010            PIC X(8)    VALUE 'EL010'.           EL679
00053      12  XCTL-626            PIC X(8)    VALUE 'EL626'.           EL679
00054      12  LINK-001            PIC X(8)    VALUE 'EL001'.           EL679
00055      12  LINK-004            PIC X(8)    VALUE 'EL004'.           EL679
00056      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.         EL679
00057      12  ERBILL-FILE-ID      PIC X(8)    VALUE 'ERBILL'.          EL679
00058      12  ELCNTL-FILE-ID      PIC X(8)    VALUE 'ELCNTL'.          EL679
00059                                                                   EL679
00060  01  WORK-AREAS.                                                  EL679
00061      12  TEXT-SUB            PIC 99      VALUE ZEROS.             EL679
00062      12  NDX                 PIC 99      VALUE ZEROS.                CL**3
00063      EJECT                                                        EL679
00064  01  ACCESS-KEYS.                                                 EL679
00065      12  ERBILL-KEY.                                              EL679
00066          16  ERBILL-CO-CD        PIC X       VALUE SPACE.         EL679
00067          16  ERBILL-COMPARE.                                      EL679
00068              20  ERBILL-CARRIER  PIC X       VALUE SPACE.         EL679
00069              20  ERBILL-GROUP    PIC X(6)    VALUE SPACES.        EL679
00070              20  ERBILL-ACCT     PIC X(10)   VALUE SPACES.        EL679
00071              20  ERBILL-FIN-RESP PIC X(10)   VALUE SPACES.        EL679
00072          16  ERBILL-REC-TYPE     PIC X       VALUE SPACE.         EL679
00073          16  ERBILL-LINE-SEQ-NO  PIC S9(4)   COMP VALUE ZEROS.    EL679
00074                                                                   EL679
00075      EJECT                                                        EL679
00076      COPY ELCDATE.                                                   CL**3
00077                                                                   EL679
00078      EJECT                                                        EL679
00079      COPY ELCLOGOF.                                                  CL**3
00080                                                                   EL679
00081      EJECT                                                        EL679
00082      COPY ELCATTR.                                                   CL**3
00083                                                                   EL679
00084      EJECT                                                        EL679
00085      COPY ELCEMIB.                                                   CL**3
00086                                                                   EL679
00087      EJECT                                                        EL679
00088      COPY ELCINTF.                                                   CL**3
00089      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.                EL679
00090          16  PI-SAV-ERBILL-KEY.                                   EL679
00091              20  PI-SAV-CO-CD            PIC X.                   EL679
00092              20  PI-SAV-CARRIER          PIC X.                   EL679
00093              20  PI-SAV-GROUP            PIC X(6).                EL679
00094              20  PI-SAV-ACCOUNT          PIC X(10).               EL679
00095              20  PI-SAV-FIN-RESP         PIC X(10).               EL679
00096              20  PI-SAV-REC-TYPE         PIC X.                   EL679
00097              20  PI-SAV-LINE-SEQ-NO      PIC S9(4)   COMP.        EL679
00098          16  PI-PREV-ERBILL-KEY.                                  EL679
00099              20  PI-PREV-CO-CD           PIC X.                   EL679
00100              20  PI-PREV-CARRIER         PIC X.                   EL679
00101              20  PI-PREV-GROUP           PIC X(6).                EL679
00102              20  PI-PREV-ACCOUNT         PIC X(10).               EL679
00103              20  PI-PREV-FIN-RESP        PIC X(10).               EL679
00104              20  PI-PREV-REC-TYPE        PIC X.                   EL679
00105              20  PI-PREV-LINE-SEQ-NO     PIC S9(4)   COMP.        EL679
00106          16  PI-ACCT-EOF-SW              PIC X.                   EL679
00107              88  PI-ACCT-EOF           VALUE 'Y'.                 EL679
00108          16  FILLER                      PIC X(577).                 CL**3
00109      EJECT                                                        EL679
00110      COPY ELCJPFX.                                                   CL**3
00111                              PIC X(750).                             CL**2
00112                                                                   EL679
00113      EJECT                                                        EL679
00114      COPY ELCAID.                                                    CL**3
00115  01  FILLER    REDEFINES DFHAID.                                  EL679
00116      12  FILLER              PIC X(8).                            EL679
00117      12  PF-VALUES           PIC X       OCCURS 2.                EL679
00118                                                                   EL679
00119      EJECT                                                        EL679
00120      COPY EL679S.                                                    CL**3
00121  01  MAP-A REDEFINES EL679AO.                                     EL679
00122      12  FILLER                  PIC X(31).                       EL679
00123      12  FILLER   OCCURS 2 TIMES.                                    CL**3
00124          16  FILLER              PIC X(3).                        EL679
00125          16  CARRIER             PIC X.                           EL679
00126          16  FILLER              PIC X(3).                        EL679
00127          16  GROUPING            PIC X(6).                        EL679
00128          16  FILLER              PIC X(3).                        EL679
00129          16  ACCOUNT             PIC X(10).                       EL679
00130          16  FILLER              PIC X(3).                        EL679
00131          16  FINRESP             PIC X(10).                       EL679
00132          16  FILLER              PIC X(3).                        EL679
00133          16  BALFRWD             PIC Z,ZZZ,ZZ9.99-.               EL679
00134          16  FILLER              PIC X(3).                        EL679
uktdel*        16  NAME                PIC X(28).                       EL679
uktins         16  NAMEX               PIC X(28).                       EL679
00136          16  FILLER              PIC X(3).                        EL679
00137          16  PREMIUM             PIC Z,ZZZ,ZZ9.99-.               EL679
00138          16  FILLER              PIC X(3).                        EL679
00139          16  BILL-DATE           PIC X(8).                        EL679
00140          16  FILLER              PIC X(3).                        EL679
00141          16  REMITTED            PIC Z,ZZZ,ZZ9.99-.               EL679
00142          16  FILLER              PIC X(3).                        EL679
00143          16  PRT-DATE            PIC X(8).                        EL679
00144          16  FILLER              PIC X(3).                        EL679
00145          16  ISSUE-COMP          PIC Z,ZZZ,ZZ9.99-.               EL679
00146          16  FILLER              PIC X(3).                        EL679
00147          16  CANCEL-COMP         PIC Z,ZZZ,ZZ9.99-.               EL679
00148          16  FILLER              PIC X(3).                        EL679
00149          16  ADJUSTMENTS         PIC Z,ZZZ,ZZ9.99-.               EL679
00150          16  FILLER              PIC X(3).                        EL679
00151          16  DISBURSED           PIC Z,ZZZ,ZZ9.99-.               EL679
00152          16  FILLER              PIC X(3).                        EL679
00153          16  END-BAL             PIC Z,ZZZ,ZZ9.99-.               EL679
00154      EJECT                                                        EL679
00155  LINKAGE SECTION.                                                 EL679
00156  01  DFHCOMMAREA             PIC X(1024).                            CL**3
00157                                                                   EL679
00158      EJECT                                                        EL679
00159 *01 PARMLIST .                                                       CL**3
00160 *    02  FILLER              PIC S9(8)   COMP.                       CL**3
00161 *    02  ERBILL-POINTER      PIC S9(8)   COMP.                       CL**3
00162                                                                   EL679
00163      COPY ERCBILL.                                                   CL**3
00164                                                                      CL**3
00165      EJECT                                                        EL679
00166  PROCEDURE DIVISION.                                              EL679
00167                                                                   EL679
00168      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL679
00169      MOVE '5'                   TO DC-OPTION-CODE.                EL679
00170      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL679
00171      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL679
00172      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL679
00173                                                                   EL679
00174      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL679
00175      IF EIBCALEN = 0                                              EL679
00176          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL679
00177                                                                   EL679
00178      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL679
00179          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL679
00180              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL679
00181              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL679
00182              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL679
00183              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL679
00184              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL679
00185              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL679
00186              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL679
00187              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL679
00188            ELSE                                                   EL679
00189              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL679
00190              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL679
00191              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL679
00192              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL679
00193              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL679
00194              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL679
00195              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL679
00196              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL679
00197                                                                   EL679
00198      MOVE LOW-VALUES             TO EL679AI.                      EL679
00199      MOVE PI-COMPANY-CD          TO PI-SAV-CO-CD                  EL679
00200                                     PI-PREV-CO-CD.                EL679
00201      IF EIBTRNID NOT = TRANS-ID                                   EL679
00202          MOVE '1'                TO PI-SAV-REC-TYPE               EL679
00203          MOVE +0                 TO PI-SAV-LINE-SEQ-NO            EL679
00204          MOVE DFHENTER           TO EIBAID                        EL679
00205          GO TO 1000-BROWSE-FORWARD.                               EL679
00206                                                                   EL679
00207      EXEC CICS HANDLE CONDITION                                   EL679
00208          PGMIDERR  (9600-PGMID-ERROR)                             EL679
00209          ERROR     (9990-ABEND)                                   EL679
00210          END-EXEC.                                                EL679
00211                                                                   EL679
00212      IF EIBAID = DFHCLEAR                                         EL679
00213          GO TO 9400-CLEAR.                                        EL679
00214                                                                   EL679
00215      EJECT                                                        EL679
00216  0200-RECEIVE.                                                    EL679
00217      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL679
00218          MOVE 0008               TO EMI-ERROR                     EL679
00219          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL679
00220          MOVE -1                 TO PFENTERL                      EL679
00221          GO TO 8200-SEND-DATAONLY.                                EL679
00222                                                                      CL**3
00223      EXEC CICS RECEIVE                                            EL679
00224          MAP      (MAP-NAME)                                      EL679
00225          MAPSET   (MAPSET-NAME)                                   EL679
00226          INTO     (EL679AI)                                       EL679
00227          END-EXEC.                                                EL679
00228                                                                   EL679
00229      IF PFENTERL = 0                                              EL679
00230          GO TO 0300-CHECK-PFKEYS.                                 EL679
00231      IF EIBAID NOT = DFHENTER                                     EL679
00232          MOVE 0004               TO EMI-ERROR                     EL679
00233          GO TO 0320-INPUT-ERROR.                                  EL679
00234      IF (PFENTERI NUMERIC) AND (PFENTERI > 0 AND < 25)            EL679
00235          MOVE PF-VALUES (PFENTERI) TO EIBAID                      EL679
00236      ELSE                                                         EL679
00237          MOVE 0029               TO EMI-ERROR                     EL679
00238          GO TO 0320-INPUT-ERROR.                                  EL679
00239                                                                   EL679
00240  0300-CHECK-PFKEYS.                                               EL679
00241      IF EIBAID = DFHPF23                                          EL679
00242          GO TO 8810-PF23.                                         EL679
00243      IF EIBAID = DFHPF24                                          EL679
00244          GO TO 9200-RETURN-MAIN-MENU.                             EL679
00245      IF EIBAID = DFHPF12                                          EL679
00246          GO TO 9500-PF12.                                         EL679
00247                                                                      CL**3
00248      IF EIBAID = DFHENTER                                         EL679
00249        OR DFHPF1                                                  EL679
00250          GO TO 1000-BROWSE-FORWARD.                               EL679
00251                                                                      CL**3
00252      IF EIBAID = DFHPF2                                           EL679
00253          GO TO 2000-BROWSE-BACKWARD.                              EL679
00254      IF EIBAID = DFHPF3                                           EL679
00255          GO TO 4000-START-REPORT-TRANSACTION.                     EL679
00256                                                                   EL679
00257  0320-INPUT-ERROR.                                                EL679
00258      MOVE 0029                   TO EMI-ERROR.                    EL679
00259      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL679
00260      MOVE AL-UNBON               TO PFENTERA.                     EL679
00261      MOVE -1                     TO PFENTERL.                     EL679
00262      GO TO 8200-SEND-DATAONLY.                                    EL679
00263                                                                   EL679
00264      EJECT                                                        EL679
00265  1000-BROWSE-FORWARD.                                             EL679
00266      MOVE PI-SAV-ERBILL-KEY      TO ERBILL-KEY.                   EL679
00267      PERFORM 5000-START-BROWSE THRU 5099-EXIT.                    EL679
00268                                                                      CL**3
00269      IF EMI-ERROR NOT = ZEROS                                     EL679
00270          GO TO 8200-SEND-DATAONLY.                                EL679
00271                                                                      CL**3
00272      PERFORM 1100-BROWSE-LOOP THRU 1100-EXIT                      EL679
00273          VARYING NDX FROM 1 BY 1                                  EL679
00274          UNTIL NDX GREATER THAN 2.                                EL679
00275                                                                      CL**3
00276      IF EMI-ERROR NOT = ZEROS                                     EL679
00277          GO TO 8200-SEND-DATAONLY.                                EL679
00278                                                                      CL**3
00279      GO TO 8100-SEND-INITIAL-MAP.                                 EL679
00280                                                                   EL679
00281  1100-BROWSE-LOOP.                                                EL679
00282      IF EIBTRNID = TRANS-ID                                       EL679
00283        OR NDX GREATER THAN 1                                      EL679
00284          MOVE '3'                TO ERBILL-REC-TYPE               EL679
00285          MOVE 9999               TO ERBILL-LINE-SEQ-NO.           EL679
00286                                                                      CL**3
00287      PERFORM 5100-READ-NEXT-RECORD THRU 5199-EXIT.                EL679
00288                                                                      CL**3
00289      IF EMI-ERROR NOT = ZEROS                                     EL679
00290          MOVE 3                  TO NDX                              CL**3
00291          IF EMI-ERROR = 2558                                      EL679
00292              MOVE ZEROS          TO EMI-ERROR                     EL679
00293              GO TO 1100-EXIT                                      EL679
00294          ELSE                                                     EL679
00295              GO TO 1100-EXIT.                                     EL679
00296                                                                      CL**3
00297      IF NDX = 1                                                   EL679
00298          MOVE BI-CONTROL-PRIMARY TO PI-PREV-ERBILL-KEY            EL679
00299      ELSE                                                         EL679
00300          MOVE BI-CONTROL-PRIMARY TO PI-SAV-ERBILL-KEY.            EL679
00301                                                                      CL**3
00302      PERFORM 3000-FORMAT-SCREEN THRU 3099-EXIT.                   EL679
00303                                                                      CL**3
00304  1100-EXIT.                                                       EL679
00305      EXIT.                                                        EL679
00306      EJECT                                                        EL679
00307  2000-BROWSE-BACKWARD.                                            EL679
00308      MOVE 2                      TO NDX.                             CL**3
00309  2100-BROWSE-LOOP.                                                EL679
00310      MOVE PI-PREV-ERBILL-KEY     TO ERBILL-KEY.                   EL679
00311      PERFORM 5000-START-BROWSE THRU 5099-EXIT.                    EL679
00312                                                                      CL**3
00313      IF EMI-ERROR NOT = ZEROS                                     EL679
00314          GO TO 8200-SEND-DATAONLY.                                EL679
00315                                                                      CL**3
00316      PERFORM 5200-READPREV THRU 5299-EXIT 2 TIMES                 EL679
00317                                                                      CL**3
00318      IF EMI-ERROR NOT = ZEROS                                     EL679
00319          GO TO 8200-SEND-DATAONLY.                                EL679
00320                                                                      CL**3
00321      MOVE BI-CONTROL-PRIMARY     TO PI-PREV-ERBILL-KEY.           EL679
00322      MOVE '1'                    TO PI-PREV-REC-TYPE.             EL679
00323      MOVE ZEROS                  TO PI-PREV-LINE-SEQ-NO.          EL679
00324                                                                      CL**3
00325      PERFORM 5400-END-BROWSE THRU 5499-EXIT.                      EL679
00326      PERFORM 5300-READ-RECORD THRU 5399-EXIT.                     EL679
00327                                                                      CL**3
00328      IF EMI-ERROR NOT = ZEROS                                     EL679
00329          GO TO 8200-SEND-DATAONLY.                                EL679
00330      IF NDX = 2                                                   EL679
00331          MOVE BI-CONTROL-PRIMARY TO PI-SAV-ERBILL-KEY.            EL679
00332                                                                      CL**3
00333      PERFORM 3000-FORMAT-SCREEN THRU 3099-EXIT.                   EL679
00334                                                                      CL**3
00335      SUBTRACT 1 FROM NDX.                                            CL**3
00336      IF NDX LESS THAN 1                                           EL679
00337          GO TO 8100-SEND-INITIAL-MAP.                             EL679
00338                                                                      CL**3
00339      GO TO 2100-BROWSE-LOOP.                                      EL679
00340      EJECT                                                        EL679
00341  3000-FORMAT-SCREEN.                                              EL679
00342      MOVE BI-CARRIER             TO CARRIER (NDX).                EL679
00343      MOVE BI-GROUPING            TO GROUPING (NDX).               EL679
00344      MOVE BI-ACCOUNT             TO ACCOUNT (NDX).                EL679
00345      MOVE BI-FIN-RESP            TO FINRESP (NDX).                EL679
00346      MOVE BI-BAL-FRWD            TO BALFRWD (NDX).                EL679
00347      MOVE BI-PREMIUM             TO PREMIUM (NDX).                EL679
00348      MOVE BI-REMITTED            TO REMITTED (NDX).               EL679
00349      MOVE BI-TOT-ISS-COMP        TO ISSUE-COMP (NDX).             EL679
00350      MOVE BI-TOT-CAN-COMP        TO CANCEL-COMP (NDX).            EL679
00351      MOVE BI-ADJUSTMNTS          TO ADJUSTMENTS (NDX).            EL679
00352      MOVE BI-DISBURSED           TO DISBURSED (NDX).              EL679
00353      MOVE BI-END-BAL             TO END-BAL (NDX).                EL679
uktdel*    MOVE BI-FIN-RESP-NAME       TO NAME (NDX).                   EL679
uktins     MOVE BI-FIN-RESP-NAME       TO NAMEX (NDX).                  EL679
00355                                                                   EL679
00356      MOVE BI-CREATION-DT         TO DC-BIN-DATE-1                 EL679
00357      MOVE SPACE                  TO DC-OPTION-CODE.               EL679
00358      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL679
00359      MOVE DC-GREG-DATE-1-EDIT    TO BILL-DATE (NDX).              EL679
00360                                                                   EL679
00361      IF BI-INITIAL-PRINT-DATE NOT = LOW-VALUES                    EL679
00362          MOVE BI-INITIAL-PRINT-DATE  TO DC-BIN-DATE-1             EL679
00363          MOVE SPACE                  TO DC-OPTION-CODE            EL679
00364          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                 EL679
00365          MOVE DC-GREG-DATE-1-EDIT    TO PRT-DATE (NDX).           EL679
00366                                                                   EL679
00367  3099-EXIT.                                                       EL679
00368      EXIT.                                                        EL679
00369      EJECT                                                        EL679
00370  4000-START-REPORT-TRANSACTION.                                   EL679
00371      EXEC CICS START                                                 CL**5
00372           TRANSID       (TRANS-6791)                                 CL**5
00373           FROM          (PROGRAM-INTERFACE-BLOCK)                    CL**5
00374           LENGTH        (PI-COMM-LENGTH)                             CL**5
00375      END-EXEC.                                                       CL**5
00376                                                                      CL**3
00377      MOVE 2559 TO EMI-ERROR.                                         CL**5
00378      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**5
00379      GO TO 8200-SEND-DATAONLY.                                       CL**5
00380      EJECT                                                        EL679
00381  5000-START-BROWSE.                                               EL679
00382      EXEC CICS HANDLE CONDITION                                   EL679
00383          NOTFND (5010-REC-NOT-FND)                                EL679
00384          END-EXEC.                                                EL679
00385                                                                      CL**3
00386      EXEC CICS STARTBR                                            EL679
00387          DATASET(ERBILL-FILE-ID)                                  EL679
00388          RIDFLD(ERBILL-KEY)                                       EL679
00389          END-EXEC.                                                EL679
00390                                                                      CL**3
00391      GO TO 5099-EXIT.                                             EL679
00392                                                                   EL679
00393  5010-REC-NOT-FND.                                                EL679
00394      MOVE 2212                   TO EMI-ERROR.                    EL679
00395      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL679
00396  5099-EXIT.                                                       EL679
00397      EXIT.                                                        EL679
00398      EJECT                                                        EL679
00399  5100-READ-NEXT-RECORD.                                           EL679
00400      EXEC CICS HANDLE CONDITION                                   EL679
00401          ENDFILE (5110-END-OF-FILE)                               EL679
00402          END-EXEC.                                                EL679
00403                                                                      CL**3
00404      EXEC CICS READNEXT                                           EL679
00405          SET (ADDRESS OF BILLING-STATEMENT)                          CL**3
00406          DATASET (ERBILL-FILE-ID)                                 EL679
00407          RIDFLD (ERBILL-KEY)                                      EL679
00408          END-EXEC.                                                EL679
00409                                                                      CL**3
00410      IF BI-COMPANY-CD NOT = PI-SAV-CO-CD                          EL679
00411          GO TO 5110-END-OF-FILE.                                  EL679
00412                                                                      CL**3
00413      IF NOT BI-HEADER-DATA                                        EL679
00414          GO TO 5100-READ-NEXT-RECORD.                             EL679
00415                                                                      CL**3
00416      IF BI-ACCOUNT = LOW-VALUES                                   EL679
00417          GO TO 5100-READ-NEXT-RECORD.                             EL679
00418                                                                      CL**3
00419      GO TO 5199-EXIT.                                             EL679
00420                                                                   EL679
00421  5110-END-OF-FILE.                                                EL679
00422      IF NDX = 2                                                   EL679
00423          MOVE 2558               TO EMI-ERROR                     EL679
00424      ELSE                                                         EL679
00425          MOVE 2237               TO EMI-ERROR.                    EL679
00426                                                                      CL**3
00427      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL679
00428                                                                      CL**3
00429  5199-EXIT.                                                       EL679
00430      EXIT.                                                        EL679
00431      EJECT                                                        EL679
00432  5200-READPREV.                                                   EL679
00433      EXEC CICS HANDLE CONDITION                                   EL679
00434          ENDFILE (5210-END-OF-FILE)                               EL679
00435          END-EXEC.                                                EL679
00436                                                                      CL**3
00437      EXEC CICS READPREV                                           EL679
00438          DATASET (ERBILL-FILE-ID)                                 EL679
00439          SET (ADDRESS OF BILLING-STATEMENT)                          CL**3
00440          RIDFLD (ERBILL-KEY)                                      EL679
00441          END-EXEC.                                                EL679
00442                                                                      CL**3
00443      IF BI-COMPANY-CD NOT = PI-PREV-CO-CD                         EL679
00444          GO TO 5210-END-OF-FILE.                                  EL679
00445                                                                      CL**3
00446      GO TO 5299-EXIT.                                             EL679
00447                                                                   EL679
00448  5210-END-OF-FILE.                                                EL679
00449      MOVE 2238                   TO EMI-ERROR                     EL679
00450      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL679
00451                                                                      CL**3
00452  5299-EXIT.                                                       EL679
00453      EXIT.                                                        EL679
00454      EJECT                                                        EL679
00455  5300-READ-RECORD.                                                EL679
00456      EXEC CICS HANDLE CONDITION                                   EL679
00457          NOTFND (5310-NOT-FOUND)                                  EL679
00458          END-EXEC.                                                EL679
00459                                                                      CL**3
00460      EXEC CICS READ                                               EL679
00461          SET (ADDRESS OF BILLING-STATEMENT)                          CL**3
00462          DATASET (ERBILL-FILE-ID)                                 EL679
00463          RIDFLD (PI-PREV-ERBILL-KEY)                              EL679
00464          END-EXEC.                                                EL679
00465                                                                      CL**3
00466      GO TO 5399-EXIT.                                             EL679
00467                                                                   EL679
00468  5310-NOT-FOUND.                                                  EL679
00469      MOVE 2212                   TO EMI-ERROR                     EL679
00470      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL679
00471                                                                      CL**3
00472  5399-EXIT.                                                       EL679
00473      EXIT.                                                        EL679
00474      EJECT                                                        EL679
00475  5400-END-BROWSE.                                                 EL679
00476      EXEC CICS ENDBR                                              EL679
00477          DATASET (ERBILL-FILE-ID)                                 EL679
00478          END-EXEC.                                                EL679
00479                                                                   EL679
00480  5499-EXIT.                                                       EL679
00481      EXIT.                                                        EL679
00482      EJECT                                                        EL679
00483      EJECT                                                        EL679
00484  8100-SEND-INITIAL-MAP.                                           EL679
00485      MOVE SAVE-DATE              TO DATEO.                        EL679
00486      MOVE EIBTIME                TO TIME-IN.                      EL679
00487      MOVE TIME-OUT               TO TIMEO.                        EL679
00488      MOVE -1                     TO PFENTERL.                     EL679
00489      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                      EL679
00490                                                                      CL**3
00491      EXEC CICS SEND                                               EL679
00492          MAP      (MAP-NAME)                                      EL679
00493          MAPSET   (MAPSET-NAME)                                   EL679
00494          FROM     (EL679AO)                                       EL679
00495          ERASE                                                    EL679
00496          CURSOR                                                   EL679
00497          END-EXEC.                                                EL679
00498                                                                      CL**3
00499      GO TO 9100-RETURN-TRAN.                                      EL679
00500                                                                   EL679
00501  8200-SEND-DATAONLY.                                              EL679
00502      IF EIBTRNID NOT = TRANS-ID                                   EL679
00503          GO TO 8100-SEND-INITIAL-MAP.                             EL679
00504                                                                      CL**3
00505      MOVE SAVE-DATE              TO DATEO.                        EL679
00506      MOVE EIBTIME                TO TIME-IN.                      EL679
00507      MOVE TIME-OUT               TO TIMEO.                        EL679
00508      MOVE -1                     TO PFENTERL.                     EL679
00509      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                         CL**3
00510                                                                      CL**3
00511      EXEC CICS SEND                                               EL679
00512          MAP      (MAP-NAME)                                      EL679
00513          MAPSET   (MAPSET-NAME)                                   EL679
00514          FROM     (EL679AO)                                       EL679
00515          DATAONLY                                                 EL679
00516          ERASEAUP                                                 EL679
00517          CURSOR                                                   EL679
00518          END-EXEC.                                                EL679
00519                                                                      CL**3
00520      GO TO 9100-RETURN-TRAN.                                      EL679
00521                                                                   EL679
00522  8300-SEND-TEXT.                                                  EL679
00523      EXEC CICS SEND TEXT                                          EL679
00524          FROM     (LOGOFF-TEXT)                                   EL679
00525          LENGTH   (LOGOFF-LENGTH)                                 EL679
00526          ERASE                                                    EL679
00527          FREEKB                                                   EL679
00528          END-EXEC.                                                EL679
00529                                                                      CL**3
00530      EXEC CICS RETURN                                             EL679
00531          END-EXEC.                                                EL679
00532                                                                   EL679
00533  8400-LOG-JOURNAL-RECORD.                                         EL679
00534      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   EL679
00535      MOVE ERBILL-FILE-ID         TO JP-FILE-ID.                   EL679
00536      MOVE THIS-PGM               TO JP-PROGRAM-ID.                EL679
00537                                                                      CL**3
pemuni*    EXEC CICS JOURNAL                                            EL679
pemuni*        JFILEID     (PI-JOURNAL-FILE-ID)                         EL679
pemuni*        JTYPEID     ('CL')                                       EL679
pemuni*        FROM        (JOURNAL-RECORD)                             EL679
pemuni*        LENGTH      (773)                                           CL**2
pemuni*        END-EXEC.                                                EL679
00544                                                                   EL679
00545  8500-DATE-CONVERT.                                               EL679
00546      MOVE LINK-ELDATCV           TO PGM-NAME.                     EL679
00547      EXEC CICS LINK                                               EL679
00548          PROGRAM    (PGM-NAME)                                    EL679
00549          COMMAREA   (DATE-CONVERSION-DATA)                        EL679
00550          LENGTH     (DC-COMM-LENGTH)                              EL679
00551          END-EXEC.                                                EL679
00552                                                                      CL**3
00553  8500-EXIT.                                                       EL679
00554      EXIT.                                                        EL679
00555                                                                   EL679
00556  8800-UNAUTHORIZED-ACCESS.                                        EL679
00557      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL679
00558      GO TO 8300-SEND-TEXT.                                        EL679
00559                                                                   EL679
00560  8810-PF23.                                                       EL679
00561      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL679
00562      MOVE XCTL-005               TO PGM-NAME.                     EL679
00563      GO TO 9300-XCTL.                                             EL679
00564                                                                      CL**3
00565  9000-RETURN-CICS.                                                EL679
00566      EXEC CICS RETURN                                             EL679
00567          END-EXEC.                                                EL679
00568                                                                   EL679
00569  9100-RETURN-TRAN.                                                EL679
00570      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL679
00571      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.         EL679
00572      EXEC CICS RETURN                                             EL679
00573          TRANSID    (TRANS-ID)                                    EL679
00574          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL679
00575          LENGTH     (PI-COMM-LENGTH)                              EL679
00576          END-EXEC.                                                EL679
00577                                                                   EL679
00578  9200-RETURN-MAIN-MENU.                                           EL679
00579      MOVE XCTL-626               TO PGM-NAME.                     EL679
00580      GO TO 9300-XCTL.                                             EL679
00581                                                                   EL679
00582  9300-XCTL.                                                       EL679
00583      EXEC CICS XCTL                                               EL679
00584          PROGRAM    (PGM-NAME)                                    EL679
00585          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL679
00586          LENGTH     (PI-COMM-LENGTH)                              EL679
00587          END-EXEC.                                                EL679
00588                                                                   EL679
00589  9400-CLEAR.                                                      EL679
00590      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME                      EL679
00591      GO TO 9300-XCTL.                                             EL679
00592                                                                   EL679
00593  9500-PF12.                                                       EL679
00594      MOVE XCTL-010               TO PGM-NAME.                     EL679
00595      GO TO 9300-XCTL.                                             EL679
00596                                                                   EL679
00597  9600-PGMID-ERROR.                                                EL679
00598      EXEC CICS HANDLE CONDITION                                   EL679
00599          PGMIDERR    (8300-SEND-TEXT)                             EL679
00600          END-EXEC.                                                EL679
00601                                                                      CL**3
00602      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL679
00603      MOVE ' '                    TO PI-ENTRY-CD-1.                EL679
00604      MOVE XCTL-005               TO PGM-NAME.                     EL679
00605      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL679
00606      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL679
00607      GO TO 9300-XCTL.                                             EL679
00608                                                                   EL679
00609  9900-ERROR-FORMAT.                                               EL679
00610      IF NOT EMI-ERRORS-COMPLETE                                   EL679
00611          MOVE LINK-001           TO PGM-NAME                      EL679
00612          EXEC CICS LINK                                           EL679
00613              PROGRAM    (PGM-NAME)                                EL679
00614              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL679
00615              LENGTH     (EMI-COMM-LENGTH)                         EL679
00616              END-EXEC.                                            EL679
00617  9900-EXIT.                                                       EL679
00618      EXIT.                                                        EL679
00619                                                                   EL679
00620  9990-ABEND.                                                      EL679
00621      MOVE LINK-004               TO PGM-NAME.                     EL679
00622      MOVE DFHEIBLK               TO EMI-LINE1                     EL679
00623                                                                      CL**3
00624      EXEC CICS LINK                                               EL679
00625          PROGRAM   (PGM-NAME)                                     EL679
00626          COMMAREA  (EMI-LINE1)                                    EL679
00627          LENGTH    (72)                                           EL679
00628          END-EXEC.                                                EL679
00629                                                                      CL**3
00630      GO TO 8200-SEND-DATAONLY.                                    EL679
00631                                                                      CL**3
00632      GOBACK.                                                      EL679
