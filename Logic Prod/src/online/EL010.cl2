00001  IDENTIFICATION DIVISION.                                         02/26/96
00002                                                                   EL010
00003  PROGRAM-ID.                 EL010 .                                 LV005
00004 *              PROGRAM CONVERTED BY                                  CL**5
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**5
00006 *              CONVERSION DATE 02/12/96 09:26:36.                    CL**5
00007 *                            VMOD=2.005.                             CL**5
00008 *AUTHOR.        LOGIC, INC.                                          CL**5
00009 *               DALLAS, TEXAS.                                       CL**5
00009 *                                                                    CL**5
00010 *DATE-COMPILED.                                                      CL**5
00011 *SECURITY.   *****************************************************   CL**5
00012 *            *                                                   *   CL**5
00013 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**5
00014 *            *                                                   *   CL**5
00015 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**5
00016 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**5
00017 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**5
00018 *            *                                                   *   CL**5
00019 *            *****************************************************   CL**5
00020 *REMARKS. EX99 - OPERATOR HELP FUNCTIONS.                            CL**3
00021      EJECT                                                        EL010
00022  ENVIRONMENT DIVISION.                                            EL010
00023  DATA DIVISION.                                                   EL010
00024  WORKING-STORAGE SECTION.                                         EL010
00025  01  LCP-TIME-OF-DAY-XX.                                             CL**5
00026      05  LCP-TIME-OF-DAY-68        PIC 9(6).                         CL**5
00027      05  FILLER                    PIC 99.                           CL**5
00028  01  LCP-CICS-TIME                 PIC 9(15).                        CL**5
00029  01  LCP-CURRENT-DATE-68.                                            CL**5
00030      05  LCP-MONTH                 PIC X(2).                         CL**5
00031      05  FILLER                    PIC X VALUE '/'.                  CL**5
00032      05  LCP-DAY1                  PIC X(2).                         CL**5
00033      05  FILLER                    PIC X VALUE '/'.                  CL**5
00034      05  LCP-YEAR                  PIC X(2).                         CL**5
00035  01  LCP-CICS-DATE                 PIC 9(15).                        CL**5
00036  77  FILLER  PIC X(32)  VALUE '********************************'. EL010
00037  77  FILLER  PIC X(32)  VALUE '*   EL010  WORKING STORAGE     *'. EL010
00038  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.005 *********'.    CL**5
00039                                                                   EL010
00040  77  WS-COMM-LENGTH              PIC S9(4)   COMP VALUE +1500.       CL**4
00041  77  CHANGE-KEY-SW               PIC X.                              CL**4
00042      88  TIME-TO-CHANGE-KEY            VALUE 'X'.                 EL010
00043                                                                   EL010
00044  01  LITERALS-NUMBERS.                                            EL010
00045      12  LIT-COMM                PIC X(4)    VALUE '010A'.        EL010
00046      12  LIT-BUFF                PIC X(4)    VALUE '010B'.        EL010
00047      12  XCTL-EL005              PIC X(8)    VALUE 'EL005'.       EL010
00048      12  THIS-PGM                PIC X(8)    VALUE 'EL010'.       EL010
00049      12  XCTL-EL126              PIC X(8)    VALUE 'EL126'.       EL010
00050      12  THIS-TRANS              PIC X(4)    VALUE 'EX99'.        EL010
00051                                                                   EL010
00052      12  ER-7008                 PIC X(4)    VALUE '7008'.        EL010
00053      12  ER-0015                 PIC X(4)    VALUE '0015'.        EL010
00054      12  ER-0064                 PIC X(4)    VALUE '0064'.        EL010
00055      12  ER-0130                 PIC X(4)    VALUE '0130'.        EL010
00056      12  ER-0131                 PIC X(4)    VALUE '0131'.        EL010
00057      12  ER-0268                 PIC X(4)    VALUE '0268'.        EL010
00058                                                                   EL010
00059      12  QUEUE-KEY.                                               EL010
00060          16  TERM-ID             PIC X(4).                        EL010
00061          16  SUB-KEY             PIC X(4).                        EL010
00062      12  COMM-KEY                PIC X(8).                        EL010
00063      12  BUFFER-KEY              PIC X(8).                        EL010
00064      12  TRANS-ID                PIC X(4).                        EL010
00065      12  WCC-CTL                 PIC X       VALUE ' '.           EL010
00066      12  COUNT-1                 PIC 99.                          EL010
00067      12  CHECK-PFKEYS            PIC 99.                          EL010
00068      12  CALL-PGM                PIC X(8).                        EL010
00069      EJECT                                                        EL010
00070  01  FORMATTED-LINE.                                              EL010
00071      12  FILLER                  PIC XX      VALUE SPACES.        EL010
00072      12  HOLD-LINE               PIC X(70).                       EL010
00073      12  FILLER                  PIC X(7)    VALUE SPACES.        EL010
00074                                                                   EL010
00075  01  TIME-UNFORMATTED.                                            EL010
00076      12  UN-HOURS                PIC XX.                          EL010
00077      12  UN-MINUTES              PIC XX.                          EL010
00078      12  FILLER                  PIC X(4).                        EL010
00079                                                                   EL010
00080  01  TIME-FORMATTED.                                              EL010
00081      12  FOR-HOURS               PIC XX.                          EL010
00082      12  FILLER                  PIC X       VALUE '.'.           EL010
00083      12  FOR-MINUTES             PIC XX.                          EL010
00084                                                                   EL010
00085  01  ERROR-SWITCHES.                                              EL010
00086      12  ERROR-SWITCH            PIC X.                           EL010
00087          88  END-OF-FILE                     VALUE 'E'.           EL010
00088          88  SCREEN-FULL                     VALUE 'F'.           EL010
00089          88  SCREEN-ERROR                    VALUE 'X'.           EL010
00090                                                                   EL010
00091  01  FILE-READ-KEY.                                               EL010
00092      12  COMPANY-CODE            PIC X.                           EL010
00093      12  HELP-TYPE               PIC X.                           EL010
00094      12  ERROR-SCREEN            PIC X(4).                        EL010
00095      12  HELP-COMPANY            PIC X(3).                        EL010
00096      12  FILLER                  PIC X(4)    VALUE SPACES.        EL010
00097      12  LINE-SEQUENCE           PIC S9(4)   COMP.                EL010
00098                                                                   EL010
00099  01  COMP-LENGTHS.                                                EL010
00100      12  BUFFER-LENGTH           PIC S9(4)   COMP.                EL010
00101      12  CURSOR-POS              PIC S9(4)   COMP.                EL010
00102      EJECT                                                        EL010
00103      COPY ELCLOGOF.                                                  CL**5
00104      EJECT                                                        EL010
00105      COPY EL010S.                                                    CL**5
00106      EJECT                                                        EL010
00107      COPY ELCAID.                                                    CL**5
00108  01  FILLER REDEFINES DFHAID.                                     EL010
00109      12  FILLER                  PIC X(8).                        EL010
00110      12  AID-KEYS OCCURS 24 TIMES.                                EL010
00111          16  FILLER              PIC X.                           EL010
00112      EJECT                                                        EL010
00113      COPY ELCINTF.                                                   CL**5
00114      12  EL010-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.          EL010
00115          16  PI-CALLING-TRANS    PIC X(4).                        EL010
00116          16  SAVE-CURSOR         PIC S9(4)   COMP.                EL010
00117          16  SAVE-KEY.                                            EL010
00118              20  SAVE-COMPANY-CD PIC X.                           EL010
00119              20  SAVE-HELP-TYPE  PIC X.                              CL**3
00120              20  SAVE-CONTROL    PIC X(4).                        EL010
00121              20  FILLER          PIC X(7).                        EL010
00122              20  SAVE-LINE       PIC S9(4)   COMP.                EL010
00123          16  EOF-SWITCH          PIC X.                           EL010
00124              88  LAST-REC-NOT-FOUND          VALUE 'W'.           EL010
00125              88  TOP-OF-FILE                 VALUE 'X'.           EL010
00126              88  BOTTOM-OF-FILE              VALUE 'Y'.           EL010
00127              88  BAD-BROWSE                  VALUE 'Z'.           EL010
00128          16  FILLER              PIC X(618).                         CL**5
00129      EJECT                                                        EL010
00130      COPY ELCEMIB.                                                   CL**5
00131      EJECT                                                        EL010
00132  LINKAGE SECTION.                                                 EL010
00133  01  DFHCOMMAREA                 PIC X(1500).                        CL**4
00134                                                                   EL010
00135 *01 PARM-LIST .                                                      CL**5
00136 *    12  FILLER                  PIC S9(8)   COMP.                   CL**5
00137 *    12  HELP-PNT                PIC S9(8)   COMP.                   CL**5
00138 *    12  BUFF-PNT                PIC S9(8)   COMP.                   CL**5
00139      EJECT                                                        EL010
00140      COPY ELCTEXT.                                                   CL**5
00141                                                                   EL010
00142  01  BUFFER-AREA                 PIC X.                           EL010
00143      EJECT                                                        EL010
00144  PROCEDURE DIVISION.                                              EL010
00145                                                                   EL010
00146      IF EIBCALEN = ZERO                                           EL010
00147          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL010
00148                                                                   EL010
00149      MOVE THIS-TRANS             TO TRANS-ID.                     EL010
00150      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL010
00151                                                                   EL010
00152      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL010
00153          MOVE LOW-VALUES         TO EL010AO                       EL010
00154          MOVE ER-7008            TO EMI-ERROR                     EL010
00155          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL010
00156          GO TO 8110-SEND-DATA.                                    EL010
00157                                                                   EL010
00158      MOVE SPACES                 TO CHANGE-KEY-SW.                EL010
00159                                                                   EL010
00160      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL010
00161          GO TO 0100-SAVE-BUFFER.                                  EL010
00162                                                                   EL010
00163      IF EIBAID = DFHCLEAR                                         EL010
00164          GO TO 0200-RESTORE-BUFFER.                               EL010
00165                                                                   EL010
00166      EXEC CICS HANDLE CONDITION                                   EL010
00167          PGMIDERR  (8820-XCTL-ERROR)                              EL010
00168          MAPFAIL   (0050-MAPFAIL-HANDLE)                          EL010
00169          ERROR     (9990-ABEND)                                   EL010
00170      END-EXEC.                                                    EL010
00171                                                                   EL010
00172      EXEC CICS RECEIVE                                            EL010
00173          MAP       ('EL010A')                                     EL010
00174          MAPSET    ('EL010S')                                     EL010
00175      END-EXEC.                                                    EL010
00176                                                                   EL010
00177      MOVE SPACES                 TO ERROR-SWITCHES.               EL010
00178                                                                   EL010
00179      IF PFKEYL GREATER THAN ZERO                                  EL010
00180          PERFORM 0300-TRANS-PF THRU 0310-TRANS-PF-EXIT.           EL010
00181                                                                   EL010
00182      IF SCREEN-ERROR                                              EL010
00183          MOVE ER-7008            TO EMI-ERROR                     EL010
00184          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL010
00185          GO TO 8110-SEND-DATA.                                    EL010
00186                                                                   EL010
00187      IF EIBAID = DFHPF1 OR DFHPF3                                 EL010
00188          GO TO 2000-BROWSE-FORWARD.                               EL010
00189                                                                   EL010
00190      IF EIBAID = DFHPF2 OR DFHPF4                                 EL010
00191          GO TO 2100-BROWSE-BACKWARD.                              EL010
00192                                                                   EL010
00193      IF EIBAID = DFHPF7                                           EL010
00194         MOVE SPACES              TO FILE-READ-KEY                 EL010
00195         MOVE PI-COMPANY-CD       TO COMPANY-CODE                  EL010
00196          GO TO 2600-USER-NOTES.                                      CL**3
00197                                                                   EL010
00198      IF EIBAID = DFHPF5                                              CL**3
00199         MOVE SPACES              TO FILE-READ-KEY                 EL010
00200         MOVE LOW-VALUES          TO COMPANY-CODE                  EL010
00201          GO TO 2400-SKIP-TO-SCREEN.                               EL010
00202                                                                   EL010
00203      IF EIBAID = DFHPF6                                           EL010
00204         MOVE SPACES              TO FILE-READ-KEY                 EL010
00205         MOVE LOW-VALUES          TO COMPANY-CODE                     CL**3
00206          GO TO 2500-SKIP-TO-ERROR.                                EL010
00207                                                                   EL010
00208      MOVE ER-7008                TO EMI-ERROR.                    EL010
00209      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL010
00210      GO TO 8110-SEND-DATA.                                        EL010
00211                                                                   EL010
00212                                                                   EL010
00213  0050-MAPFAIL-HANDLE.                                             EL010
00214      MOVE LOW-VALUES             TO EL010AO.                      EL010
00215      MOVE 'Y'                    TO EOF-SWITCH.                   EL010
00216      GO TO 2000-BROWSE-FORWARD.                                   EL010
00217      EJECT                                                        EL010
00218                                                                   EL010
00219  0100-SAVE-BUFFER.                                                EL010
00220      MOVE EIBTRMID               TO TERM-ID.                      EL010
00221      MOVE LIT-COMM               TO SUB-KEY.                      EL010
00222      MOVE QUEUE-KEY              TO COMM-KEY.                     EL010
00223      MOVE LIT-BUFF               TO SUB-KEY.                      EL010
00224      MOVE QUEUE-KEY              TO BUFFER-KEY.                   EL010
00225                                                                   EL010
00226      PERFORM 0400-DELETE-TS THRU 0420-DELETE-TS-EXIT.             EL010
00227                                                                   EL010
00228      EXEC CICS WRITEQ TS                                          EL010
00229          QUEUE   (COMM-KEY)                                       EL010
00230          FROM    (PROGRAM-INTERFACE-BLOCK)                        EL010
00231          LENGTH  (WS-COMM-LENGTH)                                    CL**4
00232      END-EXEC.                                                    EL010
00233                                                                   EL010
00234      MOVE EIBTRNID               TO PI-CALLING-TRANS.             EL010
00235      MOVE EIBCPOSN               TO SAVE-CURSOR.                  EL010
00236                                                                   EL010
00237      EXEC CICS RECEIVE                                            EL010
00238          SET     (ADDRESS OF BUFFER-AREA)                            CL**5
00239          LENGTH  (BUFFER-LENGTH)                                  EL010
00240          BUFFER                                                   EL010
00241      END-EXEC.                                                    EL010
00242                                                                   EL010
00243      EXEC CICS WRITEQ TS                                          EL010
00244          QUEUE   (BUFFER-KEY)                                     EL010
00245          FROM    (BUFFER-AREA)                                    EL010
00246          LENGTH  (BUFFER-LENGTH)                                  EL010
00247      END-EXEC.                                                    EL010
00248                                                                   EL010
00249      MOVE THIS-PGM               TO PI-CALLING-PROGRAM.           EL010
00250      MOVE LOW-VALUES             TO EL010AO.                      EL010
00251      INSPECT PI-CURRENT-SCREEN-NO CONVERTING 'S' TO 'A'.             CL**5
00252      MOVE PI-CURRENT-SCREEN-NO   TO SCODEO.                       EL010
00253      MOVE PI-LAST-ERROR-NO       TO ECODEO.                       EL010
00254                                                                   EL010
00255      MOVE SPACES                 TO FILE-READ-KEY                 EL010
00256      MOVE LOW-VALUES             TO COMPANY-CODE                  EL010
00257      GO TO  2400-SKIP-TO-SCREEN.                                     CL**2
00258                                                                      CL**2
00259 *    IF PI-LAST-ERROR-NO = SPACES OR = ZEROS                         CL**2
00260 *       MOVE SPACES              TO FILE-READ-KEY                    CL**2
00261 *       MOVE LOW-VALUES          TO COMPANY-CODE                     CL**2
00262 *        GO TO  2400-SKIP-TO-SCREEN.                                 CL**2
00263 *                                                                    CL**2
00264 *    MOVE SPACES                 TO FILE-READ-KEY                    CL**2
00265 *    MOVE LOW-VALUES             TO COMPANY-CODE                     CL**2
00266 *    GO TO  2500-SKIP-TO-ERROR.                                      CL**2
00267      EJECT                                                        EL010
00268  0200-RESTORE-BUFFER.                                             EL010
00269      MOVE PI-CALLING-TRANS       TO TRANS-ID.                     EL010
00270      MOVE SAVE-CURSOR            TO CURSOR-POS.                   EL010
00271      MOVE EIBTRMID               TO TERM-ID.                      EL010
00272      MOVE LIT-COMM               TO SUB-KEY.                      EL010
00273      MOVE QUEUE-KEY              TO COMM-KEY.                     EL010
00274      MOVE LIT-BUFF               TO SUB-KEY.                      EL010
00275      MOVE QUEUE-KEY              TO BUFFER-KEY.                   EL010
00276                                                                   EL010
00277      EXEC CICS READQ TS                                           EL010
00278          QUEUE   (COMM-KEY)                                       EL010
00279          INTO    (PROGRAM-INTERFACE-BLOCK)                        EL010
00280          LENGTH  (WS-COMM-LENGTH)                                    CL**4
00281      END-EXEC.                                                    EL010
00282                                                                   EL010
00283      EXEC CICS READQ TS                                           EL010
00284          QUEUE   (BUFFER-KEY)                                     EL010
00285          SET     (ADDRESS OF BUFFER-AREA)                            CL**5
00286          LENGTH  (BUFFER-LENGTH)                                  EL010
00287      END-EXEC.                                                    EL010
00288                                                                   EL010
00289      EXEC CICS SEND                                               EL010
00290          FROM     (BUFFER-AREA)                                   EL010
00291          LENGTH   (BUFFER-LENGTH)                                 EL010
00292          CTLCHAR  (WCC-CTL)                                       EL010
00293      END-EXEC.                                                    EL010
00294                                                                   EL010
00295      MOVE LOW-VALUES             TO EL010AO.                      EL010
00296                                                                   EL010
00297      EXEC CICS SEND                                               EL010
00298          MAP      ('EL010A')                                      EL010
00299          MAPSET   ('EL010S')                                      EL010
00300          CURSOR   (CURSOR-POS)                                    EL010
00301          DATAONLY                                                 EL010
00302          FREEKB                                                   EL010
00303      END-EXEC.                                                    EL010
00304                                                                   EL010
00305      PERFORM 0400-DELETE-TS THRU 0420-DELETE-TS-EXIT.             EL010
00306                                                                   EL010
00307      GO TO 9000-RETURN-TRANS.                                     EL010
00308                                                                   EL010
00309      EJECT                                                        EL010
00310  0300-TRANS-PF.                                                   EL010
00311      IF PFKEYI LESS 1 OR GREATER 24                               EL010
00312          MOVE 'X'                TO ERROR-SWITCH                  EL010
00313          GO TO 0310-TRANS-PF-EXIT.                                EL010
00314                                                                   EL010
00315      MOVE PFKEYI                 TO CHECK-PFKEYS.                 EL010
00316      MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.                      EL010
00317                                                                   EL010
00318  0310-TRANS-PF-EXIT.                                              EL010
00319      EXIT.                                                        EL010
00320                                                                   EL010
00321  0400-DELETE-TS.                                                  EL010
00322      EXEC CICS HANDLE CONDITION                                   EL010
00323          QIDERR (0410-BUFF-KEY-NOT-FOUND)                         EL010
00324      END-EXEC.                                                    EL010
00325                                                                   EL010
00326      EXEC CICS DELETEQ TS                                         EL010
00327          QUEUE (BUFFER-KEY)                                       EL010
00328      END-EXEC.                                                    EL010
00329                                                                   EL010
00330  0410-BUFF-KEY-NOT-FOUND.                                         EL010
00331      EXEC CICS HANDLE CONDITION                                   EL010
00332          QIDERR (0420-DELETE-TS-EXIT)                             EL010
00333      END-EXEC.                                                    EL010
00334                                                                   EL010
00335      EXEC CICS DELETEQ TS                                         EL010
00336          QUEUE (COMM-KEY)                                         EL010
00337      END-EXEC.                                                    EL010
00338                                                                   EL010
00339  0420-DELETE-TS-EXIT.                                             EL010
00340      EXIT.                                                        EL010
00341                                                                   EL010
00342      EJECT                                                        EL010
00343  2000-BROWSE-FORWARD.                                             EL010
00344      IF BOTTOM-OF-FILE                                            EL010
00345          MOVE ER-0130            TO EMI-ERROR                     EL010
00346          PERFORM 9900-ERROR-FORMAT                                EL010
00347              THRU 9900-EXIT                                       EL010
00348         GO TO 8110-SEND-DATA.                                     EL010
00349                                                                   EL010
00350      MOVE SPACE                  TO EOF-SWITCH.                   EL010
00351                                                                   EL010
00352      IF EIBAID = DFHPF1                                           EL010
00353         ADD 15                   TO SAVE-LINE                     EL010
00354      ELSE                                                         EL010
00355         ADD 5                    TO SAVE-LINE.                    EL010
00356                                                                   EL010
00357      MOVE SAVE-KEY               TO FILE-READ-KEY.                EL010
00358                                                                   EL010
00359  2010-SKIP-ADD.                                                   EL010
00360      MOVE 1                      TO COUNT-1                       EL010
00361      MOVE SPACES                 TO ERROR-SWITCH FORMATTED-LINE.  EL010
00362                                                                   EL010
00363      PERFORM 4000-START-BROWSE THRU 4010-START-BROWSE-EXIT.       EL010
00364                                                                   EL010
00365      IF BAD-BROWSE                                                EL010
00366         MOVE ER-0064             TO EMI-ERROR                     EL010
00367         PERFORM 9900-ERROR-FORMAT                                 EL010
00368            THRU 9900-EXIT                                         EL010
00369         MOVE 'W'                 TO EOF-SWITCH                    EL010
00370         PERFORM 3100-FILL-SCREEN THRU 3110-FILL-SCREEN-EXIT       EL010
00371              VARYING COUNT-1 FROM 1 BY 1 UNTIL COUNT-1 GREATER 15 EL010
00372         GO TO 8110-SEND-DATA.                                     EL010
00373                                                                   EL010
00374      PERFORM 4020-READ-FILE THRU 4040-READ-FILE-EXIT.             EL010
00375                                                                   EL010
00376      IF (EIBTRNID NOT = THIS-TRANS OR EIBAID = DFHPF5             EL010
00377         OR EIBAID = DFHPF6 OR EIBAID = DFHPF7)                    EL010
00378           AND ERROR-SCREEN NOT = SAVE-CONTROL                     EL010
00379         MOVE ER-0064             TO EMI-ERROR                     EL010
00380         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL010
00381         MOVE 'W'                 TO EOF-SWITCH                    EL010
00382         PERFORM 3100-FILL-SCREEN THRU 3110-FILL-SCREEN-EXIT       EL010
00383               VARYING COUNT-1 FROM 1 BY 1 UNTIL COUNT-1 GREATER 15EL010
00384         GO TO 8110-SEND-DATA.                                     EL010
00385                                                                   EL010
00386      IF END-OF-FILE                                               EL010
00387         GO TO 2020-END-FILE.                                      EL010
00388                                                                   EL010
00389      MOVE FILE-READ-KEY          TO SAVE-KEY.                     EL010
00390                                                                   EL010
00391      IF HELP-BY-SCREEN                                            EL010
00392         MOVE ERROR-SCREEN        TO SCODEO                        EL010
00393      ELSE                                                         EL010
00394         IF HELP-BY-ERROR                                          EL010
00395            MOVE ERROR-SCREEN     TO ECODEO                        EL010
00396         ELSE                                                      EL010
00397            MOVE SPACES           TO ECODEO  SCODEO.               EL010
00398                                                                   EL010
00399      PERFORM 3000-BUILD-SCREEN THRU 3010-BUILD-SCREEN-EXIT        EL010
00400          VARYING COUNT-1 FROM 1 BY 1 UNTIL COUNT-1 GREATER 15     EL010
00401                  OR END-OF-FILE.                                  EL010
00402                                                                   EL010
00403  2020-END-FILE.                                                   EL010
00404      IF END-OF-FILE                                               EL010
00405          PERFORM 3100-FILL-SCREEN THRU 3110-FILL-SCREEN-EXIT      EL010
00406        VARYING COUNT-1 FROM COUNT-1 BY 1 UNTIL COUNT-1 GREATER 15.EL010
00407                                                                   EL010
00408      PERFORM 4050-END-BROWSE THRU 4060-END-BROWSE-EXIT.           EL010
00409      GO TO 8110-SEND-DATA.                                        EL010
00410                                                                   EL010
00411  2100-BROWSE-BACKWARD.                                            EL010
00412      IF TOP-OF-FILE                                               EL010
00413          MOVE ER-0131            TO EMI-ERROR                     EL010
00414          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL010
00415          GO TO 8110-SEND-DATA.                                    EL010
00416                                                                   EL010
00417      IF LAST-REC-NOT-FOUND                                        EL010
00418          MOVE ER-0268            TO EMI-ERROR                     EL010
00419          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL010
00420          GO TO 8110-SEND-DATA.                                    EL010
00421                                                                   EL010
00422      MOVE SPACE TO EOF-SWITCH.                                    EL010
00423                                                                   EL010
00424      IF SAVE-LINE = 1 OR ZEROS                                    EL010
00425          GO TO 2200-NEW-SCREEN.                                   EL010
00426                                                                   EL010
00427      IF EIBAID = DFHPF2                                           EL010
00428         SUBTRACT 15                   FROM SAVE-LINE              EL010
00429      ELSE                                                         EL010
00430         SUBTRACT 5                    FROM SAVE-LINE.             EL010
00431                                                                   EL010
00432      IF SAVE-LINE LESS ZERO                                       EL010
00433          MOVE ZEROES             TO SAVE-LINE.                    EL010
00434                                                                   EL010
00435      MOVE SAVE-KEY               TO FILE-READ-KEY.                EL010
00436      GO TO 2010-SKIP-ADD.                                         EL010
00437      EJECT                                                        EL010
00438  2200-NEW-SCREEN.                                                 EL010
00439      MOVE SAVE-KEY               TO FILE-READ-KEY.                EL010
00440      PERFORM 4000-START-BROWSE THRU 4010-START-BROWSE-EXIT.       EL010
00441      PERFORM 4070-READ-PREV THRU 4090-READ-PREV-EXIT              EL010
00442      PERFORM 4070-READ-PREV THRU 4090-READ-PREV-EXIT.             EL010
00443                                                                   EL010
00444      IF SAVE-COMPANY-CD NOT = COMPANY-CODE                        EL010
00445          MOVE ER-0131            TO EMI-ERROR                     EL010
00446          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL010
00447          GO TO 8110-SEND-DATA.                                    EL010
00448                                                                   EL010
00449      IF END-OF-FILE                                               EL010
00450         MOVE LOW-VALUES          TO ERROR-SCREEN.                 EL010
00451                                                                   EL010
00452      MOVE ZEROS                  TO LINE-SEQUENCE.                EL010
00453      MOVE FILE-READ-KEY          TO SAVE-KEY.                     EL010
00454      PERFORM 4050-END-BROWSE  THRU 4060-END-BROWSE-EXIT.          EL010
00455      GO TO 2010-SKIP-ADD.                                         EL010
00456                                                                   EL010
00457  2400-SKIP-TO-SCREEN.                                             EL010
00458      MOVE SPACES                 TO EOF-SWITCH.                   EL010
00459      MOVE ZEROES                 TO LINE-SEQUENCE.                EL010
00460      MOVE 'S'                    TO HELP-TYPE.                    EL010
00461                                                                   EL010
00462      IF SCODEI = LOW-VALUES OR SPACES                             EL010
00463          MOVE SPACES             TO ERROR-SCREEN                  EL010
00464      ELSE                                                         EL010
00465          MOVE SCODEI             TO ERROR-SCREEN.                 EL010
00466                                                                   EL010
00467      IF SCODEI = ZEROES                                           EL010
00468          MOVE SPACE              TO HELP-TYPE.                    EL010
00469                                                                   EL010
00470      MOVE FILE-READ-KEY          TO SAVE-KEY                      EL010
00471      GO TO 2010-SKIP-ADD.                                         EL010
00472                                                                   EL010
00473  2500-SKIP-TO-ERROR.                                              EL010
00474      MOVE SPACES                 TO EOF-SWITCH.                   EL010
00475      MOVE ZEROS                  TO LINE-SEQUENCE.                EL010
00476      MOVE 'E'                    TO HELP-TYPE.                    EL010
00477                                                                   EL010
00478      IF ECODEI = LOW-VALUES OR ZEROES                             EL010
00479          MOVE SPACES             TO ERROR-SCREEN                  EL010
00480      ELSE                                                         EL010
00481          MOVE ECODEI             TO ERROR-SCREEN.                 EL010
00482                                                                   EL010
00483      IF ECODEI = ZEROES                                           EL010
00484          MOVE SPACE              TO HELP-TYPE.                    EL010
00485                                                                   EL010
00486      MOVE FILE-READ-KEY          TO SAVE-KEY                      EL010
00487      GO TO 2010-SKIP-ADD.                                            CL**3
00488      EJECT                                                           CL**3
00489  2600-USER-NOTES.                                                    CL**3
00490      MOVE SPACES                 TO  EOF-SWITCH.                     CL**3
00491      MOVE ZEROS                  TO  LINE-SEQUENCE.                  CL**3
00492      MOVE SAVE-HELP-TYPE         TO  HELP-TYPE.                      CL**3
00493      IF HELP-TYPE IS EQUAL TO 'S'                                    CL**3
00494          IF SCODEI IS EQUAL TO LOW-VALUES OR SPACES                  CL**3
00495              MOVE SPACES         TO  ERROR-SCREEN                    CL**3
00496          ELSE                                                        CL**3
00497              MOVE SCODEI         TO  ERROR-SCREEN.                   CL**3
00498      IF HELP-TYPE IS EQUAL TO 'E'                                    CL**3
00499          IF ECODEI IS EQUAL TO LOW-VALUES OR SPACES                  CL**3
00500              MOVE SPACES         TO  ERROR-SCREEN                    CL**3
00501          ELSE                                                        CL**3
00502              MOVE ECODEI         TO  ERROR-SCREEN.                   CL**3
00503      IF HELP-TYPE IS EQUAL TO 'S'                                    CL**3
00504          IF SCODEI IS EQUAL TO ZEROS                                 CL**3
00505              MOVE SPACE          TO  HELP-TYPE.                      CL**3
00506      IF HELP-TYPE IS EQUAL TO 'E'                                    CL**3
00507          IF ECODEI IS EQUAL TO ZEROS                                 CL**3
00508              MOVE SPACE          TO  HELP-TYPE.                      CL**3
00509      MOVE FILE-READ-KEY          TO  SAVE-KEY.                       CL**3
00510      GO TO 2010-SKIP-ADD.                                         EL010
00511      EJECT                                                        EL010
00512  3000-BUILD-SCREEN.                                               EL010
00513      MOVE TX-TEXT-LINE           TO HOLD-LINE.                    EL010
00514      MOVE FORMATTED-LINE         TO INFOO (COUNT-1).              EL010
00515      PERFORM 4020-READ-FILE THRU 4040-READ-FILE-EXIT.             EL010
00516                                                                   EL010
00517      IF TX-SCREEN-OR-ERROR NOT = SAVE-CONTROL                     EL010
00518          MOVE 'E'                TO ERROR-SWITCH.                 EL010
00519                                                                   EL010
00520  3010-BUILD-SCREEN-EXIT.                                          EL010
00521      EXIT.                                                        EL010
00522                                                                   EL010
00523                                                                   EL010
00524  3100-FILL-SCREEN.                                                EL010
00525      MOVE SPACES                 TO FORMATTED-LINE.               EL010
00526      MOVE FORMATTED-LINE         TO INFOO (COUNT-1).              EL010
00527                                                                   EL010
00528  3110-FILL-SCREEN-EXIT.                                           EL010
00529      EXIT.                                                        EL010
00530      EJECT                                                        EL010
00531  4000-START-BROWSE.                                               EL010
00532      EXEC CICS HANDLE CONDITION                                   EL010
00533          NOTOPEN (5000-HELP-NOT-OPEN)                             EL010
00534          NOTFND  (4005-KEY-NOT-FOUND)                             EL010
00535      END-EXEC.                                                    EL010
00536                                                                   EL010
00537      EXEC CICS STARTBR                                            EL010
00538          DATASET ('ELHELP')                                       EL010
00539          RIDFLD  (FILE-READ-KEY)                                  EL010
00540      END-EXEC.                                                    EL010
00541                                                                   EL010
00542      GO TO 4010-START-BROWSE-EXIT.                                EL010
00543                                                                   EL010
00544  4005-KEY-NOT-FOUND.                                              EL010
00545         MOVE 'Z'                    TO EOF-SWITCH.                EL010
00546                                                                   EL010
00547  4010-START-BROWSE-EXIT.                                          EL010
00548      EXIT.                                                        EL010
00549      EJECT                                                        EL010
00550  4020-READ-FILE.                                                  EL010
00551      EXEC CICS HANDLE CONDITION                                   EL010
00552          NOTFND  (4030-END-OF-FILE)                               EL010
00553          ENDFILE (4030-END-OF-FILE)                               EL010
00554      END-EXEC.                                                    EL010
00555                                                                   EL010
00556      EXEC CICS READNEXT                                           EL010
00557          SET      (ADDRESS OF TEXT-FILES)                            CL**5
00558          DATASET  ('ELHELP')                                      EL010
00559          RIDFLD   (FILE-READ-KEY)                                 EL010
00560      END-EXEC.                                                    EL010
00561                                                                   EL010
00562      IF TX-COMPANY-CD NOT = SAVE-COMPANY-CD                       EL010
00563            GO TO 4030-END-OF-FILE.                                EL010
00564                                                                   EL010
00565      GO TO 4040-READ-FILE-EXIT.                                   EL010
00566                                                                   EL010
00567  4030-END-OF-FILE.                                                EL010
00568      MOVE 'E'                    TO ERROR-SWITCH.                 EL010
00569      MOVE 'Y'                    TO EOF-SWITCH.                   EL010
00570      MOVE ER-0130                TO EMI-ERROR.                    EL010
00571      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL010
00572                                                                   EL010
00573  4040-READ-FILE-EXIT.                                             EL010
00574      EXIT.                                                        EL010
00575                                                                   EL010
00576  4050-END-BROWSE.                                                 EL010
00577      EXEC CICS ENDBR                                              EL010
00578          DATASET ('ELHELP')                                       EL010
00579      END-EXEC.                                                    EL010
00580                                                                   EL010
00581  4060-END-BROWSE-EXIT.                                            EL010
00582      EXIT.                                                        EL010
00583      EJECT                                                        EL010
00584  4070-READ-PREV.                                                  EL010
00585      EXEC CICS HANDLE CONDITION                                   EL010
00586          ENDFILE (4080-END-OF-FILE)                               EL010
00587          NOTFND  (4080-END-OF-FILE)                               EL010
00588      END-EXEC.                                                    EL010
00589                                                                   EL010
00590      EXEC CICS READPREV                                           EL010
00591          SET      (ADDRESS OF TEXT-FILES)                            CL**5
00592          DATASET  ('ELHELP')                                      EL010
00593          RIDFLD   (FILE-READ-KEY)                                 EL010
00594      END-EXEC.                                                    EL010
00595                                                                   EL010
00596      GO TO 4090-READ-PREV-EXIT.                                   EL010
00597                                                                   EL010
00598  4080-END-OF-FILE.                                                EL010
00599      MOVE 'E'                    TO ERROR-SWITCH.                 EL010
00600      MOVE 'X'                    TO EOF-SWITCH.                   EL010
00601      MOVE ER-0131                TO EMI-ERROR.                    EL010
00602      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL010
00603                                                                   EL010
00604  4090-READ-PREV-EXIT.                                             EL010
00605      EXIT.                                                        EL010
00606                                                                   EL010
00607  5000-HELP-NOT-OPEN.                                              EL010
00608      MOVE ER-0015                TO EMI-ERROR.                    EL010
00609      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL010
00610      GO TO 8110-SEND-DATA.                                        EL010
00611      EJECT                                                        EL010
00612  8100-SEND-INITIAL-MAP.                                           EL010
00613      PERFORM 8130-FORMAT-TIME-DATE                                EL010
00614              THRU 8140-FORMAT-TIME-DATE-EXIT.                     EL010
00615                                                                   EL010
00616      EXEC CICS SEND                                               EL010
00617          MAP     ('EL010A')                                       EL010
00618          MAPSET  ('EL010S')                                       EL010
00619          ERASE                                                    EL010
00620          FREEKB                                                   EL010
00621      END-EXEC.                                                    EL010
00622                                                                   EL010
00623      GO TO 9000-RETURN-TRANS.                                     EL010
00624                                                                   EL010
00625  8110-SEND-DATA.                                                  EL010
00626      IF EIBTRNID NOT = THIS-TRANS                                 EL010
00627         GO TO 8100-SEND-INITIAL-MAP.                              EL010
00628                                                                   EL010
00629      PERFORM 8130-FORMAT-TIME-DATE                                EL010
00630              THRU 8140-FORMAT-TIME-DATE-EXIT.                     EL010
00631                                                                   EL010
00632      EXEC CICS SEND                                               EL010
00633          MAP     ('EL010A')                                       EL010
00634          MAPSET  ('EL010S')                                       EL010
00635          DATAONLY                                                 EL010
00636          FREEKB                                                   EL010
00637      END-EXEC.                                                    EL010
00638                                                                   EL010
00639      GO TO 9000-RETURN-TRANS.                                     EL010
00640                                                                   EL010
00641  8130-FORMAT-TIME-DATE.                                           EL010
00642      EXEC CICS ASKTIME ABSTIME(LCP-CICS-DATE)                        CL**5
00643      END-EXEC                                                        CL**5
00644      EXEC CICS FORMATTIME                                            CL**5
00645                ABSTIME(LCP-CICS-DATE)                                CL**5
00646                YYMMDD(LCP-CURRENT-DATE-68)                           CL**5
00647                DATESEP('/')                                          CL**5
00648      END-EXEC                                                        CL**5
00649      MOVE LCP-CURRENT-DATE-68 TO DATEO.                              CL**5
00650      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL**5
00651      END-EXEC                                                        CL**5
00652      EXEC CICS FORMATTIME                                            CL**5
00653                ABSTIME(LCP-CICS-TIME)                                CL**5
00654                TIME(LCP-TIME-OF-DAY-XX)                              CL**5
00655      END-EXEC                                                        CL**5
00656      MOVE  LCP-TIME-OF-DAY-68 TO TIME-UNFORMATTED.                   CL**5
00657      MOVE UN-HOURS               TO FOR-HOURS.                    EL010
00658      MOVE UN-MINUTES             TO FOR-MINUTES.                  EL010
00659      MOVE TIME-FORMATTED         TO TIMEO.                        EL010
00660      MOVE EMI-MESSAGE-AREA (1)   TO MSGO.                         EL010
00661                                                                   EL010
00662  8140-FORMAT-TIME-DATE-EXIT.                                      EL010
00663      EXIT.                                                        EL010
00664                                                                   EL010
00665  8800-UNAUTHORIZED-ACCESS.                                        EL010
00666      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL010
00667      GO TO 8990-SEND-TEXT.                                        EL010
00668                                                                   EL010
00669  8820-XCTL-ERROR.                                                 EL010
00670      EXEC CICS HANDLE CONDITION                                   EL010
00671          PGMIDERR (8880-SIGN-OFF-ERROR)                           EL010
00672      END-EXEC.                                                    EL010
00673                                                                   EL010
00674      MOVE SPACE                  TO PI-ENTRY-CD-1.                EL010
00675      MOVE CALL-PGM               TO PI-CALLING-PROGRAM.           EL010
00676      MOVE XCTL-EL005             TO CALL-PGM.                     EL010
00677      GO TO 9200-XCTL.                                             EL010
00678                                                                   EL010
00679  8880-SIGN-OFF-ERROR.                                             EL010
00680      MOVE CALL-PGM               TO LOGOFF-PGM.                   EL010
00681      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL010
00682                                                                   EL010
00683  8990-SEND-TEXT.                                                  EL010
00684      EXEC CICS SEND TEXT                                          EL010
00685          FROM    (LOGOFF-TEXT)                                    EL010
00686          LENGTH  (LOGOFF-LENGTH)                                  EL010
00687          ERASE                                                    EL010
00688          FREEKB                                                   EL010
00689      END-EXEC.                                                    EL010
00690                                                                   EL010
00691      GO TO 9100-RETURN-CICS.                                      EL010
00692      EJECT                                                        EL010
00693  9000-RETURN-TRANS.                                               EL010
00694      EXEC CICS RETURN                                             EL010
00695          TRANSID   (TRANS-ID)                                     EL010
00696          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL010
00697          LENGTH    (WS-COMM-LENGTH)                                  CL**4
00698      END-EXEC.                                                    EL010
00699                                                                   EL010
00700      GOBACK.                                                      EL010
00701                                                                   EL010
00702  9100-RETURN-CICS.                                                EL010
00703      EXEC CICS RETURN                                             EL010
00704      END-EXEC.                                                    EL010
00705                                                                   EL010
00706      GOBACK.                                                      EL010
00707                                                                   EL010
00708  9200-XCTL.                                                       EL010
00709      EXEC CICS XCTL                                               EL010
00710          PROGRAM   (CALL-PGM)                                     EL010
00711          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL010
00712          LENGTH    (WS-COMM-LENGTH)                                  CL**4
00713      END-EXEC.                                                    EL010
00714                                                                   EL010
00715  9900-ERROR-FORMAT.                                               EL010
00716      IF NOT EMI-ERRORS-COMPLETE                                   EL010
00717                                                                   EL010
00718          EXEC CICS LINK                                           EL010
00719              PROGRAM   ('EL001')                                  EL010
00720              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL010
00721              LENGTH    (EMI-COMM-LENGTH)                          EL010
00722          END-EXEC.                                                EL010
00723                                                                   EL010
00724  9900-EXIT.                                                       EL010
00725      EXIT.                                                        EL010
00726                                                                   EL010
00727  9990-ABEND.                                                      EL010
00728      EXEC CICS LINK                                               EL010
00729          PROGRAM   ('EL004')                                      EL010
00730          COMMAREA  (DFHEIBLK)                                     EL010
00731          LENGTH    (64)                                           EL010
00732      END-EXEC.                                                    EL010
00733                                                                   EL010
00734      GO TO 9100-RETURN-CICS.                                      EL010
