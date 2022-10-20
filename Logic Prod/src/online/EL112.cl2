00001  IDENTIFICATION DIVISION.                                         02/26/96
00002                                                                   EL112
00003  PROGRAM-ID.                 EL112 .                                 LV004
00004 *              PROGRAM CONVERTED BY                                  CL**4
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**4
00006 *              CONVERSION DATE 02/12/96 09:48:50.                    CL**4
00007 *                            VMOD=2.004.                             CL**4
00008 *                                                                 EL112
00008 *                                                                 EL112
00009 *AUTHOR.        LOGIC, INC.                                          CL**4
00010 *               DALLAS, TEXAS.                                       CL**4
00011                                                                   EL112
00012 *DATE-COMPILED.                                                      CL**4
00013                                                                   EL112
00014 *SECURITY.   *****************************************************   CL**4
00015 *            *                                                   *   CL**4
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**4
00017 *            *                                                   *   CL**4
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**4
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**4
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**4
00021 *            *                                                   *   CL**4
00022 *            *****************************************************   CL**4
00023                                                                   EL112
00024 *REMARKS. TRANSACTION EX37 - PROGRAM FILE MAINTENANCE.               CL**4
00025      EJECT                                                        EL112
00026  ENVIRONMENT DIVISION.                                            EL112
00027  DATA DIVISION.                                                   EL112
00028  WORKING-STORAGE SECTION.                                         EL112
00029  01  LCP-TIME-OF-DAY-XX.                                             CL**4
00030      05  LCP-TIME-OF-DAY-68        PIC 9(6).                         CL**4
00031      05  FILLER                    PIC 99.                           CL**4
00032  01  LCP-CICS-TIME                 PIC 9(15).                        CL**4
00033                                                                   EL112
00034  77  FILLER  PIC X(32)  VALUE '********************************'. EL112
00035  77  FILLER  PIC X(32)  VALUE '*    EL112 WORKING STORAGE     *'. EL112
00036  77  FILLER  PIC X(32)  VALUE '*********** VMOD 2.004 *********'.    CL**4
00037                                                                   EL112
00038  01  WS-DATE-AREA.                                                EL112
00039      05  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL112
00040      05  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.        EL112
00041                                                                   EL112
00042  01  LITERALS-NUMBERS.                                            EL112
00043      12  XCTL-EL126              PIC X(5)    VALUE 'EL126'.          CL**3
00044      12  XCTL-EL626              PIC X(5)    VALUE 'EL626'.          CL**3
00045      12  XCTL-EM626              PIC X(5)    VALUE 'EM626'.          CL**3
00046      12  XCTL-GL800              PIC X(8)    VALUE 'GL800'.          CL**3
00047      12  LIT-A                   PIC X       VALUE 'A'.           EL112
00048      12  LIT-B                   PIC X       VALUE 'B'.           EL112
00049      12  LIT-C                   PIC X       VALUE 'C'.           EL112
00050      12  LIT-D                   PIC X       VALUE 'D'.           EL112
00051      12  LIT-E                   PIC X       VALUE 'E'.           EL112
00052      12  LIT-F                   PIC X       VALUE 'F'.           EL112
00053      12  LIT-S                   PIC X       VALUE 'S'.           EL112
00054      12  LIT-X                   PIC X       VALUE 'X'.           EL112
00055      12  LIT-PN                  PIC XX      VALUE 'PN'.          EL112
00056      12  LIT-CLAIM               PIC XX      VALUE 'EL'.          EL112
00057      12  LIT-GL                  PIC XX      VALUE 'GL'.          EL112
00058      12  LIT-MORTG               PIC XX      VALUE 'EM'.             CL**2
00059      12  LIT-LF                  PIC XX      VALUE 'LF'.          EL112
00060      12  LIT-EC                  PIC XX      VALUE 'EC'.          EL112
00061      12  LIT-WA                  PIC XX      VALUE 'WA'.          EL112
00062      12  LIT-FILE                PIC X(8)    VALUE 'ELPGMN'.      EL112
00063      12  LIT-PROG                PIC X(8)    VALUE 'EL112'.       EL112
00064      12  SCREEN-NUMBER           PIC X(4)    VALUE '112A'.        EL112
00065      12  LIT-SYS                 PIC X(4)    VALUE 'SYS'.         EL112
00066      12  LIT-CICS                PIC X(4)    VALUE 'CICS'.        EL112
00067      12  LIT-MENU                PIC X(4)    VALUE 'MENU'.        EL112
00068      12  LIT-BATCH               PIC X(5)    VALUE 'BATCH'.       EL112
00069      12  LIT-SPACE               PIC X(5)    VALUE SPACE.         EL112
00070      12  LIT-TRANS               PIC X(4)    VALUE 'EX37'.        EL112
00071      12  CALL-PROG               PIC X(8)    VALUE 'EL119'.       EL112
00072      EJECT                                                        EL112
00073  01  COUNT-FIELDS.                                                EL112
00074      12  COUNT-1                 PIC 99.                          EL112
00075      12  COUNT-2                 PIC 99.                          EL112
00076      12  COUNT-3                 PIC 99.                          EL112
00077                                                                   EL112
00078  01  EDIT-WORK-AREA.                                              EL112
00079      12  CHECK-MAINT             PIC X.                           EL112
00080          88  SHOW-OPTION                     VALUE 'S'.           EL112
00081          88  ADD-OPTION                      VALUE 'A'.           EL112
00082          88  CHANGE-OPTION                   VALUE 'C'.           EL112
00083          88  DELETE-OPTION                   VALUE 'D'.           EL112
00084          88  VALID-OPTION                  VALUE 'A' 'C' 'D' 'S'. EL112
00085      12  CHECK-CALL              PIC X.                           EL112
00086          88  CICS-CALL                       VALUE 'C'.           EL112
00087          88  MENU-CALL                       VALUE 'M'.           EL112
00088          88  BATCH-PROG                      VALUE 'B'.           EL112
00089          88  VALID-CALL                      VALUE 'M' 'C' 'B'.   EL112
00090      12  CHECK-PFKEYS            PIC 99.                          EL112
00091      12  SCREEN-SWITCH           PIC X.                           EL112
00092          88  END-OF-FILE                     VALUE 'E'.           EL112
00093          88  SCREEN-FULL                     VALUE 'F'.           EL112
00094          88  SCREEN-ERROR                    VALUE 'X'.           EL112
00095      12  BROWSE-KEY.                                              EL112
00096          16  PGM-PREF            PIC XX.                          EL112
00097          16  FILLER              PIC X(3).                        EL112
00098      EJECT                                                        EL112
00099  01  DISPLAY-LINE.                                                EL112
00100      12  FILLER                  PIC X.                           EL112
00101      12  PGM-NO                  PIC X(5).                        EL112
00102      12  FILLER                  PIC X(5).                        EL112
00103      12  PGM-DESC                PIC X(40).                       EL112
00104      12  FILLER                  PIC X(6).                        EL112
00105      12  PGM-TRANS               PIC X(4).                        EL112
00106      12  FILLER                  PIC X(7).                        EL112
00107      12  PGM-CALLED              PIC X(5).                        EL112
00108                                                                   EL112
00109  01  TIME-UNFORMATTED.                                            EL112
00110      12  UN-HOURS                PIC XX.                          EL112
00111      12  UN-MINUTES              PIC XX.                          EL112
00112      12  FILLER                  PIC X(4).                        EL112
00113                                                                   EL112
00114  01  TIME-FORMATTED.                                              EL112
00115      12  FOR-HOURS               PIC XX.                          EL112
00116      12  FILLER                  PIC X       VALUE '.'.           EL112
00117      12  FOR-MINUTES             PIC XX.                          EL112
00118      EJECT                                                        EL112
00119  01  ERROR-MESSAGES.                                              EL112
00120      12  MAINT-MIS.                                               EL112
00121          16  FILLER              PIC X(39)                        EL112
00122              VALUE 'MAINTENANCE FUNCTION INVALID OR UNAUTHO'.     EL112
00123          16  FILLER              PIC X(18)                        EL112
00124              VALUE 'RIZED             '.                          EL112
00125                                                                   EL112
00126      12  PGM-MIS.                                                 EL112
00127          16  FILLER              PIC X(33)                        EL112
00128              VALUE 'PROGRAM NUMBER MISSING OR INVALID'.           EL112
00129          16  FILLER              PIC X(18)                        EL112
00130              VALUE ' - PLEASE RE-ENTER'.                          EL112
00131                                                                   EL112
00132      12  PGM-DUP                 PIC X(42)                        EL112
00133          VALUE 'PROGRAM NUMBER DUPLICATE - PLEASE RE-ENTER'.      EL112
00134                                                                   EL112
00135      12  DESC-MIS.                                                EL112
00136          16  FILLER              PIC X(38)                        EL112
00137              VALUE 'PROGRAM DESCRIPTION MISSING OR INVALID'.      EL112
00138          16  FILLER              PIC X(18)                        EL112
00139              VALUE ' - PLEASE RE-ENTER'.                          EL112
00140      12  TRANS-MIS               PIC X(45)                        EL112
00141          VALUE 'TRANSACTION MISSING OR INVALID - PLEASE ENTER'.   EL112
00142                                                                   EL112
00143      12  CALL-MIS.                                                EL112
00144          16  FILLER              PIC X(32)                        EL112
00145              VALUE 'CALLABLE CODE MISSING OR INVALID'.            EL112
00146          16  FILLER              PIC X(18)                        EL112
00147              VALUE ' - PLEASE RE-ENTER'.                          EL112
00148      12  INVALID-PFKEY           PIC X(45)                        EL112
00149          VALUE 'INVALID PF-OPTION REQUESTED - PLEASE RE-ENTER'.   EL112
00150                                                                   EL112
00151      12  END-MSG                 PIC X(11)                        EL112
00152          VALUE 'END OF FILE'.                                     EL112
00153                                                                   EL112
00154      12  END-UPDATE              PIC X(16)                        EL112
00155          VALUE 'UPDATE COMPLETED'.                                EL112
00156                                                                   EL112
00157      12  SCREEN-TERM             PIC X(42)                        EL112
00158            VALUE '     CLAS-IC PROGRAM MAINTENANCE COMPLETED'.    EL112
00159                                                                   EL112
00160      12  FILE-MSG                PIC X(25)                        EL112
00161            VALUE '     FILE ELPGMN NOT OPEN'.                     EL112
00162                                                                   EL112
00163  01  COMP-LENGTHS.                                                EL112
00164      12  LIT-IC                  PIC S9(4)  COMP VALUE -1.        EL112
00165      12  FILE-LENGTH             PIC S9(4)  COMP VALUE +25.       EL112
00166      12  TERM-LENGTH             PIC S9(4)  COMP VALUE +41.       EL112
00167      12  PGMN-LENGTH             PIC S9(4)  COMP VALUE +52.       EL112
00168      12  JOURNAL-LENGTH          PIC S9(4)  COMP VALUE +75.       EL112
00169      EJECT                                                        EL112
00170      COPY ELCDATE.                                                   CL**4
00171      EJECT                                                        EL112
00172      COPY ELCATTR.                                                   CL**4
00173      EJECT                                                        EL112
00174      COPY ELCAID.                                                    CL**4
00175  01  FILLER REDEFINES DFHAID.                                     EL112
00176      12  FILLER                  PIC X(8).                        EL112
00177      12  AID-KEYS OCCURS 24 TIMES.                                EL112
00178          16  FILLER              PIC X.                           EL112
00179                                                                   EL112
00180      COPY ELCINTF.                                                   CL**4
00181      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.                 EL112
00182          16  SAVE-BEGIN          PIC X(5).                        EL112
00183          16  SAVE-ENDING         PIC X(5).                        EL112
00184          16  SHOW-SWITCH         PIC X.                           EL112
00185              88  NOT-SHOWN                   VALUE 'X'.           EL112
00186          16  FILLER              PIC X(629).                         CL**4
00187      EJECT                                                        EL112
00188      COPY ELCJPFX.                                                   CL**4
00189                                  PIC X(52).                       EL112
00190      EJECT                                                        EL112
00191      COPY EL112S.                                                    CL**4
00192      EJECT                                                        EL112
00193  LINKAGE SECTION.                                                 EL112
00194  01  DFHCOMMAREA                 PIC X(1024).                     EL112
00195                                                                   EL112
00196 *01 PARM-LIST .                                                      CL**4
00197 *    12  FILLER                  PIC S9(8)  COMP.                    CL**4
00198 *    12  PGMN-PNT                PIC S9(8)  COMP.                    CL**4
00199      EJECT                                                        EL112
00200      COPY ELCPGMN.                                                   CL**4
00201      EJECT                                                        EL112
00202  PROCEDURE DIVISION.                                              EL112
00203                                                                   EL112
00204      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL112
00205                                                                   EL112
00206      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL112
00207      MOVE '5'                    TO DC-OPTION-CODE.               EL112
00208      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL112
00209      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL112
00210      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL112
00211                                                                   EL112
00212      IF EIBCALEN = ZERO                                           EL112
00213          MOVE HIGH-VALUES        TO SAVE-BEGIN                    EL112
00214          MOVE LOW-VALUES         TO SAVE-ENDING                   EL112
00215          MOVE LIT-X              TO SHOW-SWITCH                   EL112
00216          MOVE LOW-VALUES         TO EL112AO                       EL112
00217          MOVE ZEROS              TO COUNT-1                       EL112
00218          PERFORM 3100-FILL-SCREEN THRU 3110-EXIT                  EL112
00219              UNTIL SCREEN-FULL                                    EL112
00220          MOVE LIT-IC             TO MAINTL                        EL112
00221          GO TO 8100-SEND-MAP.                                     EL112
00222                                                                   EL112
00223      IF PI-CALLING-PROGRAM NOT = LIT-PROG                         EL112
00224          IF PI-RETURN-TO-PROGRAM NOT = LIT-PROG                   EL112
00225              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6     EL112
00226              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5     EL112
00227              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4     EL112
00228              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3     EL112
00229              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2     EL112
00230              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1     EL112
00231              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM   EL112
00232              MOVE LIT-PROG             TO  PI-CALLING-PROGRAM     EL112
00233            ELSE                                                   EL112
00234              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM     EL112
00235              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM   EL112
00236              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1     EL112
00237              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2     EL112
00238              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3     EL112
00239              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4     EL112
00240              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5     EL112
00241              MOVE SPACES               TO  PI-SAVED-PROGRAM-6.    EL112
00242                                                                   EL112
00243      IF EIBTRNID NOT = LIT-TRANS                                  EL112
00244          MOVE HIGH-VALUES TO SAVE-BEGIN                           EL112
00245          MOVE LOW-VALUES TO SAVE-ENDING                           EL112
00246          MOVE LIT-X TO SHOW-SWITCH                                EL112
00247          MOVE LOW-VALUES         TO EL112AO                       EL112
00248          MOVE ZEROS              TO COUNT-1                       EL112
00249          PERFORM 3100-FILL-SCREEN THRU 3110-EXIT                  EL112
00250              UNTIL SCREEN-FULL                                    EL112
00251          MOVE LIT-IC             TO MAINTL                        EL112
00252          GO TO 8100-SEND-MAP.                                     EL112
00253                                                                   EL112
00254      IF EIBAID = DFHCLEAR                                         EL112
00255          GO TO 9400-CLEAR.                                        EL112
00256                                                                   EL112
00257      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL112
00258          MOVE LOW-VALUES         TO EL112AO                       EL112
00259          MOVE LIT-IC             TO PFKEYL                        EL112
00260          MOVE INVALID-PFKEY      TO MSGO                          EL112
00261          GO TO 8110-SEND-DATA.                                    EL112
00262                                                                   EL112
00263      EXEC CICS RECEIVE                                            EL112
00264          MAP     ('EL112A')                                       EL112
00265          MAPSET  ('EL112S')                                       EL112
00266      END-EXEC.                                                    EL112
00267                                                                   EL112
00268      MOVE SPACE                  TO SCREEN-SWITCH.                EL112
00269                                                                   EL112
00270      IF PFKEYL GREATER ZERO                                       EL112
00271          PERFORM 0300-TRANS-PF THRU 0310-EXIT.                    EL112
00272                                                                   EL112
00273      IF SCREEN-ERROR                                              EL112
00274          MOVE LIT-IC             TO PFKEYL                        EL112
00275          GO TO 8110-SEND-DATA.                                    EL112
00276                                                                   EL112
00277      IF EIBAID = DFHPF23                                          EL112
00278          GO TO 9100-RETURN-CICS.                                  EL112
00279                                                                   EL112
00280      IF  EIBAID = DFHPF24                                            CL**3
00281                                                                      CL**3
00282          IF  CREDIT-SESSION                                          CL**3
00283              MOVE XCTL-EL626     TO CALL-PROG                        CL**3
00284              GO TO 9000-XCTL                                         CL**3
00285                                                                      CL**3
00286          ELSE                                                     EL112
00287              IF  CLAIM-SESSION                                       CL**3
00288                  MOVE XCTL-EL126 TO CALL-PROG                        CL**3
00289                  GO TO 9000-XCTL                                     CL**3
00290                                                                      CL**3
00291              ELSE                                                    CL**3
00292                  IF  MORTGAGE-SESSION                                CL**3
00293                      MOVE XCTL-EM626                                 CL**3
00294                                  TO CALL-PROG                        CL**3
00295                      GO TO 9000-XCTL                                 CL**3
00296                                                                      CL**3
00297                  ELSE                                                CL**3
00298                      IF  GENERAL-LEDGER-SESSION                      CL**3
00299                          MOVE XCTL-GL800                             CL**3
00300                                  TO CALL-PROG                        CL**3
00301                          GO TO 9000-XCTL.                            CL**3
00302                                                                   EL112
00303      IF EIBAID = DFHPF12                                          EL112
00304          MOVE 'EL010'         TO  CALL-PROG                       EL112
00305          GO TO 9000-XCTL.                                         EL112
00306                                                                   EL112
00307      IF EIBAID = DFHPF1                                           EL112
00308          GO TO 0100-PAGE-FORWARD.                                 EL112
00309                                                                   EL112
00310      IF EIBAID = DFHPF2                                           EL112
00311          GO TO 0200-PAGE-BACKWARD.                                EL112
00312                                                                   EL112
00313      IF EIBAID NOT = DFHENTER                                     EL112
00314          MOVE INVALID-PFKEY      TO MSGO                          EL112
00315          MOVE LIT-IC             TO MAINTL                        EL112
00316          GO TO 8110-SEND-DATA.                                    EL112
00317                                                                   EL112
00318      PERFORM 0400-SET-ATTRB THRU 0410-EXIT.                       EL112
00319      MOVE SPACE                  TO SCREEN-SWITCH.                EL112
00320      PERFORM 1000-EDIT-SCREEN THRU 1010-EXIT.                     EL112
00321                                                                   EL112
00322      IF SCREEN-ERROR                                              EL112
00323          GO TO 8110-SEND-DATA.                                    EL112
00324                                                                   EL112
00325      PERFORM 1100-UPDATE-FILE THRU 1110-EXIT.                     EL112
00326      MOVE LIT-IC                 TO MAINTL.                       EL112
00327                                                                   EL112
00328      IF SHOW-OPTION                                               EL112
00329          MOVE SPACES             TO MSGO                          EL112
00330          GO TO 8110-SEND-DATA                                     EL112
00331      ELSE                                                         EL112
00332          MOVE LIT-X TO SHOW-SWITCH                                EL112
00333          MOVE LOW-VALUES         TO EL112AO                       EL112
00334          MOVE LIT-IC             TO MAINTL                        EL112
00335          MOVE END-UPDATE         TO MSGO                          EL112
00336          MOVE HIGH-VALUES        TO SAVE-BEGIN                    EL112
00337          MOVE LOW-VALUES         TO SAVE-ENDING                   EL112
00338          GO TO 8100-SEND-MAP.                                     EL112
00339      EJECT                                                        EL112
00340  0100-PAGE-FORWARD.                                               EL112
00341      MOVE ZEROS                  TO COUNT-1.                      EL112
00342      MOVE SPACES                 TO SCREEN-SWITCH  MSGO           EL112
00343                                     SHOW-SWITCH.                  EL112
00344                                                                   EL112
00345      IF PGRML = ZEROS                                             EL112
00346          MOVE SAVE-ENDING TO BROWSE-KEY                           EL112
00347      ELSE                                                         EL112
00348          MOVE AL-UANOF           TO PGRMA                         EL112
00349          MOVE PGRMI              TO BROWSE-KEY.                   EL112
00350                                                                   EL112
00351      MOVE SPACES                 TO MAINTO                        EL112
00352                                     DESCO                         EL112
00353                                     TRANCDO                       EL112
00354                                     CALLO.                        EL112
00355                                                                   EL112
00356      EXEC CICS HANDLE CONDITION                                   EL112
00357          NOTOPEN  (8230-NOT-OPEN)                                 EL112
00358          NOTFND   (0110-REC-NOT-FND)                              EL112
00359      END-EXEC.                                                    EL112
00360                                                                   EL112
00361      EXEC CICS STARTBR                                            EL112
00362          DATASET  (LIT-FILE)                                      EL112
00363          RIDFLD   (BROWSE-KEY)                                    EL112
00364      END-EXEC.                                                    EL112
00365                                                                   EL112
00366      PERFORM 3000-BUILD-FWD-PAGE THRU 3020-EXIT                   EL112
00367          UNTIL SCREEN-FULL OR END-OF-FILE.                        EL112
00368                                                                   EL112
00369      EXEC CICS ENDBR                                              EL112
00370          DATASET (LIT-FILE)                                       EL112
00371      END-EXEC.                                                    EL112
00372                                                                   EL112
00373      MOVE LIT-IC                 TO MAINTL.                       EL112
00374      GO TO 8110-SEND-DATA.                                        EL112
00375                                                                   EL112
00376  0110-REC-NOT-FND.                                                EL112
00377      MOVE HIGH-VALUES            TO SAVE-BEGIN SAVE-ENDING.       EL112
00378      PERFORM 3100-FILL-SCREEN THRU 3110-EXIT                      EL112
00379          UNTIL SCREEN-FULL.                                       EL112
00380      MOVE LIT-IC                 TO MAINTL.                       EL112
00381      MOVE END-MSG                TO MSGO.                         EL112
00382      GO TO 8110-SEND-DATA.                                        EL112
00383      EJECT                                                        EL112
00384  0200-PAGE-BACKWARD.                                              EL112
00385      MOVE 11                     TO COUNT-1.                      EL112
00386      MOVE SPACES                 TO SCREEN-SWITCH  MSGO           EL112
00387                                     SHOW-SWITCH.                  EL112
00388                                                                   EL112
00389      IF PGRML = ZEROS                                             EL112
00390          MOVE SAVE-BEGIN         TO BROWSE-KEY                    EL112
00391      ELSE                                                         EL112
00392          MOVE AL-UANOF           TO PGRMA                         EL112
00393          MOVE PGRMI              TO BROWSE-KEY.                   EL112
00394                                                                   EL112
00395      MOVE SPACES                 TO MAINTO                        EL112
00396                                     DESCO                         EL112
00397                                     TRANCDO                       EL112
00398                                     CALLO.                        EL112
00399                                                                   EL112
00400      EXEC CICS HANDLE CONDITION                                   EL112
00401          NOTOPEN  (8230-NOT-OPEN)                                 EL112
00402          NOTFND   (0210-REC-NOT-FND)                              EL112
00403      END-EXEC.                                                    EL112
00404                                                                   EL112
00405      EXEC CICS STARTBR                                            EL112
00406          DATASET  (LIT-FILE)                                      EL112
00407          RIDFLD   (BROWSE-KEY)                                    EL112
00408      END-EXEC.                                                    EL112
00409                                                                   EL112
00410      PERFORM 3030-BUILD-BCK-PAGE THRU 3050-EXIT                   EL112
00411          UNTIL SCREEN-FULL OR END-OF-FILE.                        EL112
00412                                                                   EL112
00413      EXEC CICS ENDBR                                              EL112
00414          DATASET (LIT-FILE)                                       EL112
00415      END-EXEC.                                                    EL112
00416                                                                   EL112
00417      MOVE COUNT-1                TO COUNT-3.                      EL112
00418      PERFORM 3060-RAISE-PAGE THRU 3070-EXIT                       EL112
00419          COUNT-3 TIMES.                                           EL112
00420      MOVE LIT-IC                 TO MAINTL.                       EL112
00421      GO TO 8110-SEND-DATA.                                        EL112
00422                                                                   EL112
00423  0210-REC-NOT-FND.                                                EL112
00424      MOVE HIGH-VALUES            TO SAVE-BEGIN.                   EL112
00425      MOVE ZEROS                  TO PGRML.                        EL112
00426      GO TO 0200-PAGE-BACKWARD.                                    EL112
00427      EJECT                                                        EL112
00428  0300-TRANS-PF.                                                   EL112
00429      IF EIBAID NOT = DFHENTER                                     EL112
00430          MOVE INVALID-PFKEY      TO MSGO                          EL112
00431          MOVE LIT-X              TO SCREEN-SWITCH                 EL112
00432          GO TO 0310-EXIT.                                         EL112
00433                                                                   EL112
00434      IF PFKEYI NOT NUMERIC                                        EL112
00435          MOVE INVALID-PFKEY      TO MSGO                          EL112
00436          MOVE LIT-X              TO SCREEN-SWITCH                 EL112
00437          GO TO 0310-EXIT.                                         EL112
00438                                                                   EL112
00439      IF PFKEYI LESS 1 OR GREATER 24                               EL112
00440          MOVE INVALID-PFKEY      TO MSGO                          EL112
00441          MOVE LIT-X              TO SCREEN-SWITCH                 EL112
00442          GO TO 0310-EXIT.                                         EL112
00443                                                                   EL112
00444      MOVE PFKEYI                 TO CHECK-PFKEYS.                 EL112
00445      MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.                      EL112
00446                                                                   EL112
00447  0310-EXIT.                                                       EL112
00448      EXIT.                                                        EL112
00449                                                                   EL112
00450  0400-SET-ATTRB.                                                  EL112
00451      MOVE AL-UANON               TO MAINTA                        EL112
00452                                     PGRMA                         EL112
00453                                     DESCA                         EL112
00454                                     TRANCDA                       EL112
00455                                     CALLA.                        EL112
00456                                                                   EL112
00457  0410-EXIT.                                                       EL112
00458      EXIT.                                                        EL112
00459      EJECT                                                        EL112
00460  1000-EDIT-SCREEN.                                                EL112
00461      MOVE MAINTI                 TO CHECK-MAINT.                  EL112
00462                                                                   EL112
00463      IF (NOT VALID-OPTION) OR                                     EL112
CIDMOD*       (PI-PROCESSOR-ID NOT = 'LGXX' AND NOT SHOW-OPTION)        EL112
CIDMOD        (PI-PROCESSOR-ID NOT = 'LGXX' AND 'PEMA' AND 'SMVA'       EL112
CIDMOD                        AND NOT SHOW-OPTION)                      EL112
00465          MOVE LIT-IC             TO MAINTL                        EL112
00466          MOVE AL-UABON           TO MAINTA                        EL112
00467          MOVE MAINT-MIS          TO MSGO                          EL112
00468          MOVE LIT-X              TO SCREEN-SWITCH                 EL112
00469          GO TO 1010-EXIT.                                         EL112
00470                                                                   EL112
00471      IF PGRMI = ZEROS OR LOW-VALUES                               EL112
00472          MOVE LIT-IC             TO PGRML                         EL112
00473          MOVE AL-UABON           TO PGRMA                         EL112
00474          MOVE PGM-MIS            TO MSGO                          EL112
00475          MOVE LIT-X              TO SCREEN-SWITCH                 EL112
00476          GO TO 1010-EXIT.                                         EL112
00477                                                                   EL112
00478      MOVE PGRMI                  TO BROWSE-KEY.                   EL112
00479                                                                   EL112
00480      IF PGM-PREF NOT = LIT-CLAIM AND LIT-EC AND LIT-WA            EL112
00481        AND LIT-GL AND LIT-LF AND LIT-MORTG                           CL**2
00482          MOVE LIT-IC             TO PGRML                         EL112
00483          MOVE AL-UABON           TO PGRMA                         EL112
00484          MOVE PGM-MIS            TO MSGO                          EL112
00485          MOVE LIT-X              TO SCREEN-SWITCH                 EL112
00486          GO TO 1010-EXIT.                                         EL112
00487                                                                   EL112
00488      IF CHANGE-OPTION AND NOT-SHOWN                               EL112
00489          MOVE SPACES             TO SHOW-SWITCH SCREEN-SWITCH     EL112
00490          MOVE ZEROS              TO COUNT-1                       EL112
00491          PERFORM 3100-FILL-SCREEN THRU 3110-EXIT                  EL112
00492              UNTIL SCREEN-FULL                                    EL112
00493          MOVE LIT-S              TO CHECK-MAINT.                  EL112
00494                                                                   EL112
00495      PERFORM 1020-VERIFY-PGM THRU 1050-VERIFY-PGM-EXIT.           EL112
00496                                                                   EL112
00497      IF SCREEN-ERROR OR DELETE-OPTION                             EL112
00498          GO TO 1010-EXIT.                                         EL112
00499                                                                   EL112
00500      IF SHOW-OPTION                                               EL112
00501          MOVE SPACES             TO SHOW-SWITCH SCREEN-SWITCH     EL112
00502          MOVE ZEROS              TO COUNT-1                       EL112
00503          PERFORM 3100-FILL-SCREEN THRU 3110-EXIT                  EL112
00504              UNTIL SCREEN-FULL                                    EL112
00505          GO TO 1010-EXIT.                                         EL112
00506                                                                   EL112
00507      IF DESCI = SPACES OR LOW-VALUES                              EL112
00508          MOVE LIT-IC             TO DESCL                         EL112
00509          MOVE AL-UABON           TO DESCA                         EL112
00510          MOVE DESC-MIS           TO MSGO                          EL112
00511          MOVE LIT-X              TO SCREEN-SWITCH                 EL112
00512          GO TO 1010-EXIT.                                         EL112
00513                                                                   EL112
00514      MOVE CALLI                  TO CHECK-CALL.                   EL112
00515                                                                   EL112
00516      IF NOT VALID-CALL                                            EL112
00517          MOVE LIT-IC             TO CALLL                         EL112
00518          MOVE AL-UABON           TO CALLA                         EL112
00519          MOVE CALL-MIS           TO MSGO                          EL112
00520          MOVE LIT-X              TO SCREEN-SWITCH.                EL112
00521                                                                   EL112
00522      IF TRANCDI = SPACES OR LOW-VALUES                            EL112
00523         IF NOT BATCH-PROG                                         EL112
00524            MOVE LIT-IC           TO TRANCDL                       EL112
00525            MOVE AL-UABON         TO TRANCDA                       EL112
00526            MOVE TRANS-MIS        TO MSGO                          EL112
00527            MOVE LIT-X            TO SCREEN-SWITCH                 EL112
00528            GO TO 1010-EXIT.                                       EL112
00529                                                                   EL112
00530  1010-EXIT.                                                       EL112
00531      EXIT.                                                        EL112
00532      EJECT                                                        EL112
00533  1020-VERIFY-PGM.                                                 EL112
00534      EXEC CICS HANDLE CONDITION                                   EL112
00535          NOTOPEN  (1040-RECORD-NOT-FOUND)                         EL112
00536          NOTFND   (1040-RECORD-NOT-FOUND)                         EL112
00537      END-EXEC.                                                    EL112
00538                                                                   EL112
00539      MOVE PGRMI                  TO BROWSE-KEY.                   EL112
00540                                                                   EL112
00541      EXEC CICS READ                                               EL112
00542          DATASET  (LIT-FILE)                                      EL112
00543          RIDFLD   (BROWSE-KEY)                                    EL112
00544          SET      (ADDRESS OF PROGRAM-DESCRIPTIONS)                  CL**4
00545      END-EXEC.                                                    EL112
00546                                                                   EL112
00547      IF ADD-OPTION                                                EL112
00548          MOVE PGM-DUP            TO MSGO                          EL112
00549          MOVE AL-UABON           TO PGRMA                         EL112
00550          MOVE LIT-IC             TO PGRML                         EL112
00551          MOVE LIT-X              TO SCREEN-SWITCH                 EL112
00552          GO TO 1050-VERIFY-PGM-EXIT.                              EL112
00553                                                                   EL112
00554      IF NOT SHOW-OPTION                                           EL112
00555          GO TO 1050-VERIFY-PGM-EXIT.                              EL112
00556                                                                   EL112
00557      MOVE PN-PROGRAM-DESCRIPTION TO DESCO.                        EL112
00558      MOVE PN-TRANSACTION-CODE    TO TRANCDO.                      EL112
00559      MOVE PN-ENTRY-METHOD        TO CALLO.                        EL112
00560      GO TO 1050-VERIFY-PGM-EXIT.                                  EL112
00561                                                                   EL112
00562  1040-RECORD-NOT-FOUND.                                           EL112
00563      IF ADD-OPTION                                                EL112
00564          GO TO 1050-VERIFY-PGM-EXIT.                              EL112
00565                                                                   EL112
00566      MOVE PGM-MIS                TO MSGO.                         EL112
00567      MOVE AL-UABON               TO PGRMA.                        EL112
00568      MOVE LIT-IC                 TO PGRML.                        EL112
00569      MOVE LIT-X                  TO SCREEN-SWITCH.                EL112
00570                                                                   EL112
00571  1050-VERIFY-PGM-EXIT.                                            EL112
00572      EXIT.                                                        EL112
00573      EJECT                                                        EL112
00574  1100-UPDATE-FILE.                                                EL112
00575      IF SHOW-OPTION                                               EL112
00576          GO TO 1110-EXIT.                                         EL112
00577                                                                   EL112
00578      IF ADD-OPTION                                                EL112
00579          PERFORM 1120-ADD-OPTION THRU 1140-EXIT.                  EL112
00580                                                                   EL112
00581      IF CHANGE-OPTION                                             EL112
00582          PERFORM 1150-CHANGE-OPTION THRU 1160-EXIT.               EL112
00583                                                                   EL112
00584      IF DELETE-OPTION                                             EL112
00585          PERFORM 1170-DELETE-OPTION THRU 1180-EXIT.               EL112
00586                                                                   EL112
00587  1110-EXIT.                                                       EL112
00588      EXIT.                                                        EL112
00589                                                                   EL112
00590  1120-ADD-OPTION.                                                 EL112
00591      EXEC CICS HANDLE CONDITION                                   EL112
00592          NOTOPEN  (8230-NOT-OPEN)                                 EL112
00593          NOSPACE  (1130-FILE-FULL)                                EL112
00594      END-EXEC.                                                    EL112
00595                                                                   EL112
00596      EXEC CICS GETMAIN                                            EL112
00597          SET      (ADDRESS OF PROGRAM-DESCRIPTIONS)                  CL**4
00598          LENGTH   (PGMN-LENGTH)                                   EL112
00599          INITIMG  (LIT-SPACE)                                     EL112
00600      END-EXEC.                                                    EL112
00601                                                                   EL112
00602      MOVE LIT-PN                 TO PN-RECORD-ID.                 EL112
00603      MOVE PGRMI                  TO BROWSE-KEY PN-PROGRAM-NUMBER. EL112
00604      MOVE DESCI                  TO PN-PROGRAM-DESCRIPTION.       EL112
00605      MOVE TRANCDI                TO PN-TRANSACTION-CODE.          EL112
00606      MOVE CALLI                  TO PN-ENTRY-METHOD.              EL112
00607      MOVE LIT-A                  TO JP-RECORD-TYPE.               EL112
00608      MOVE PROGRAM-DESCRIPTIONS   TO JP-RECORD-AREA.               EL112
00609                                                                   EL112
00610      EXEC CICS WRITE                                              EL112
00611          DATASET  (LIT-FILE)                                      EL112
00612          FROM     (PROGRAM-DESCRIPTIONS)                          EL112
00613          RIDFLD   (BROWSE-KEY)                                    EL112
00614      END-EXEC.                                                    EL112
00615                                                                   EL112
00616      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.                   EL112
00617                                                                   EL112
00618  1130-FILE-FULL.                                                  EL112
00619      MOVE LIT-X                  TO SCREEN-SWITCH.                EL112
00620                                                                   EL112
00621  1140-EXIT.                                                       EL112
00622      EXIT.                                                        EL112
00623                                                                   EL112
00624  1150-CHANGE-OPTION.                                              EL112
00625      EXEC CICS HANDLE CONDITION                                   EL112
00626          NOTOPEN (8230-NOT-OPEN)                                  EL112
00627      END-EXEC.                                                    EL112
00628                                                                   EL112
00629      MOVE PGRMI                  TO BROWSE-KEY.                   EL112
00630      PERFORM 1190-READ-FILE THRU 1200-EXIT.                       EL112
00631      MOVE LIT-B                  TO JP-RECORD-TYPE.               EL112
00632      MOVE PROGRAM-DESCRIPTIONS   TO JP-RECORD-AREA.               EL112
00633      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.                   EL112
00634      MOVE DESCI                  TO PN-PROGRAM-DESCRIPTION.       EL112
00635      MOVE TRANCDI                TO PN-TRANSACTION-CODE.          EL112
00636      MOVE CALLI                  TO PN-ENTRY-METHOD.              EL112
00637      MOVE LIT-C                  TO JP-RECORD-TYPE.               EL112
00638      MOVE PROGRAM-DESCRIPTIONS   TO JP-RECORD-AREA.               EL112
00639                                                                   EL112
00640      EXEC CICS REWRITE                                            EL112
00641          DATASET  (LIT-FILE)                                      EL112
00642          FROM     (PROGRAM-DESCRIPTIONS)                          EL112
00643      END-EXEC.                                                    EL112
00644                                                                   EL112
00645      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.                   EL112
00646                                                                   EL112
00647  1160-EXIT.                                                       EL112
00648      EXIT.                                                        EL112
00649                                                                   EL112
00650  1170-DELETE-OPTION.                                              EL112
00651      EXEC CICS HANDLE CONDITION                                   EL112
00652          NOTOPEN (8230-NOT-OPEN)                                  EL112
00653      END-EXEC.                                                    EL112
00654                                                                   EL112
00655      MOVE PGRMI                  TO BROWSE-KEY.                   EL112
00656      PERFORM 1190-READ-FILE THRU 1200-EXIT.                       EL112
00657      MOVE LIT-D                  TO JP-RECORD-TYPE.               EL112
00658      MOVE PROGRAM-DESCRIPTIONS   TO JP-RECORD-AREA.               EL112
00659                                                                   EL112
00660      EXEC CICS DELETE                                             EL112
00661          DATASET (LIT-FILE)                                       EL112
00662      END-EXEC.                                                    EL112
00663                                                                   EL112
00664      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.                   EL112
00665                                                                   EL112
00666  1180-EXIT.                                                       EL112
00667      EXIT.                                                        EL112
00668                                                                   EL112
00669  1190-READ-FILE.                                                  EL112
00670      EXEC CICS READ                                               EL112
00671          DATASET  (LIT-FILE)                                      EL112
00672          RIDFLD   (BROWSE-KEY)                                    EL112
00673          SET      (ADDRESS OF PROGRAM-DESCRIPTIONS)                  CL**4
00674          UPDATE                                                   EL112
00675      END-EXEC.                                                    EL112
00676                                                                   EL112
00677  1200-EXIT.                                                       EL112
00678      EXIT.                                                        EL112
00679                                                                   EL112
00680  2000-JOURNAL-WRITE.                                              EL112
00681      MOVE LIT-SYS                TO JP-USER-ID.                   EL112
00682      MOVE LIT-FILE               TO JP-FILE-ID.                   EL112
00683      MOVE LIT-PROG               TO JP-PROGRAM-ID.                EL112
00684                                                                   EL112
pemuni*    IF PI-JOURNAL-FILE-ID NOT = ZERO                             EL112
pemuni*        EXEC CICS JOURNAL                                        EL112
pemuni*            JFILEID  (1)                                         EL112
pemuni*            JTYPEID  ('EL')                                      EL112
pemuni*            FROM     (JOURNAL-RECORD)                            EL112
pemuni*            LENGTH   (JOURNAL-LENGTH)                            EL112
pemuni*        END-EXEC.                                                EL112
00692                                                                   EL112
00693  2010-EXIT.                                                       EL112
00694      EXIT.                                                        EL112
00695      EJECT                                                        EL112
00696  3000-BUILD-FWD-PAGE.                                             EL112
00697      EXEC CICS HANDLE CONDITION                                   EL112
00698          ENDFILE  (3010-END-FILE)                                 EL112
00699          NOTFND   (3010-END-FILE)                                 EL112
00700      END-EXEC.                                                    EL112
00701                                                                   EL112
00702      EXEC CICS READNEXT                                           EL112
00703          DATASET  (LIT-FILE)                                      EL112
00704          RIDFLD   (BROWSE-KEY)                                    EL112
00705          SET      (ADDRESS OF PROGRAM-DESCRIPTIONS)                  CL**4
00706      END-EXEC.                                                    EL112
00707                                                                   EL112
00708      MOVE SPACES                 TO DISPLAY-LINE.                 EL112
00709      MOVE PN-PROGRAM-NUMBER      TO PGM-NO.                       EL112
00710      MOVE PN-PROGRAM-DESCRIPTION     TO PGM-DESC.                 EL112
00711      MOVE PN-TRANSACTION-CODE    TO PGM-TRANS.                    EL112
00712      MOVE PN-ENTRY-METHOD TO CHECK-CALL.                          EL112
00713                                                                   EL112
00714      IF CICS-CALL                                                 EL112
00715          MOVE LIT-CICS           TO PGM-CALLED                    EL112
00716      ELSE                                                         EL112
00717          IF MENU-CALL                                             EL112
00718              MOVE LIT-MENU       TO PGM-CALLED                    EL112
00719          ELSE                                                     EL112
00720              MOVE LIT-BATCH      TO PGM-CALLED.                   EL112
00721                                                                   EL112
00722      ADD 1                       TO COUNT-1.                      EL112
00723      MOVE DISPLAY-LINE           TO PGMDESCO (COUNT-1).           EL112
00724                                                                   EL112
00725      IF COUNT-1 = 1                                               EL112
00726          MOVE PN-PROGRAM-NUMBER  TO SAVE-BEGIN.                   EL112
00727                                                                   EL112
00728      IF COUNT-1 GREATER 10                                        EL112
00729          MOVE PN-PROGRAM-NUMBER  TO SAVE-ENDING                   EL112
00730          MOVE LIT-F              TO SCREEN-SWITCH.                EL112
00731                                                                   EL112
00732      GO TO 3020-EXIT.                                             EL112
00733                                                                   EL112
00734  3010-END-FILE.                                                   EL112
00735      PERFORM 3100-FILL-SCREEN THRU 3110-EXIT                      EL112
00736          UNTIL SCREEN-FULL.                                       EL112
00737      MOVE PGM-NO                 TO BROWSE-KEY.                   EL112
00738      MOVE BROWSE-KEY             TO SAVE-ENDING.                  EL112
00739      MOVE END-MSG                TO MSGO.                         EL112
00740      MOVE LIT-E                  TO SCREEN-SWITCH.                EL112
00741                                                                   EL112
00742  3020-EXIT.                                                       EL112
00743      EXIT.                                                        EL112
00744      EJECT                                                        EL112
00745  3030-BUILD-BCK-PAGE.                                             EL112
00746      EXEC CICS HANDLE CONDITION                                   EL112
00747          ENDFILE  (3040-END-FILE)                                 EL112
00748          NOTFND   (3040-END-FILE)                                 EL112
00749      END-EXEC.                                                    EL112
00750                                                                   EL112
00751      EXEC CICS READPREV                                           EL112
00752          DATASET  (LIT-FILE)                                      EL112
00753          RIDFLD   (BROWSE-KEY)                                    EL112
00754          SET      (ADDRESS OF PROGRAM-DESCRIPTIONS)                  CL**4
00755      END-EXEC.                                                    EL112
00756                                                                   EL112
00757      MOVE SPACES                 TO DISPLAY-LINE.                 EL112
00758      MOVE PN-PROGRAM-NUMBER      TO PGM-NO.                       EL112
00759      MOVE PN-PROGRAM-DESCRIPTION TO PGM-DESC.                     EL112
00760      MOVE PN-TRANSACTION-CODE    TO PGM-TRANS.                    EL112
00761      MOVE PN-ENTRY-METHOD        TO CHECK-CALL.                   EL112
00762                                                                   EL112
00763      IF CICS-CALL                                                 EL112
00764          MOVE LIT-CICS           TO PGM-CALLED                    EL112
00765      ELSE                                                         EL112
00766          IF MENU-CALL                                             EL112
00767              MOVE LIT-MENU       TO PGM-CALLED                    EL112
00768          ELSE                                                     EL112
00769              MOVE LIT-BATCH      TO PGM-CALLED.                   EL112
00770                                                                   EL112
00771      MOVE DISPLAY-LINE           TO PGMDESCO (COUNT-1).           EL112
00772                                                                   EL112
00773      IF COUNT-1 = 11                                              EL112
00774          MOVE PN-PROGRAM-NUMBER  TO SAVE-ENDING.                  EL112
00775                                                                   EL112
00776      IF COUNT-1 = 1                                               EL112
00777          MOVE PN-PROGRAM-NUMBER  TO SAVE-BEGIN                    EL112
00778          MOVE LIT-F              TO SCREEN-SWITCH.                EL112
00779                                                                   EL112
00780      SUBTRACT 1 FROM COUNT-1.                                     EL112
00781      GO TO 3050-EXIT.                                             EL112
00782                                                                   EL112
00783  3040-END-FILE.                                                   EL112
00784      MOVE PGM-NO                 TO BROWSE-KEY.                   EL112
00785      MOVE BROWSE-KEY             TO SAVE-BEGIN.                   EL112
00786      MOVE END-MSG                TO MSGO.                         EL112
00787      MOVE LIT-E                  TO SCREEN-SWITCH.                EL112
00788                                                                   EL112
00789  3050-EXIT.                                                       EL112
00790      EXIT.                                                        EL112
00791      EJECT                                                        EL112
00792  3060-RAISE-PAGE.                                                 EL112
00793      MOVE 1                      TO COUNT-1.                      EL112
00794      MOVE 2                      TO COUNT-2.                      EL112
00795      PERFORM 3080-SHIFT-SCREEN THRU 3090-EXIT                     EL112
00796           UNTIL COUNT-1 GREATER 10.                               EL112
00797      MOVE SPACES                 TO PGMDESCO (COUNT-1).           EL112
00798                                                                   EL112
00799  3070-EXIT.                                                       EL112
00800      EXIT.                                                        EL112
00801                                                                   EL112
00802  3080-SHIFT-SCREEN.                                               EL112
00803      MOVE PGMDESCO (COUNT-2)     TO PGMDESCO (COUNT-1).           EL112
00804      ADD 1                       TO COUNT-1 COUNT-2.              EL112
00805                                                                   EL112
00806  3090-EXIT.                                                       EL112
00807      EXIT.                                                        EL112
00808                                                                   EL112
00809  3100-FILL-SCREEN.                                                EL112
00810      ADD 1                       TO COUNT-1.                      EL112
00811      MOVE SPACES                 TO PGMDESCO (COUNT-1).           EL112
00812                                                                   EL112
00813      IF COUNT-1 GREATER 10                                        EL112
00814          MOVE LIT-F              TO SCREEN-SWITCH.                EL112
00815                                                                   EL112
00816  3110-EXIT.                                                       EL112
00817      EXIT.                                                        EL112
00818      EJECT                                                        EL112
00819  8100-SEND-MAP.                                                   EL112
00820      PERFORM 8130-FORMAT-DATE-TIME THRU 8140-EXIT.                EL112
00821                                                                   EL112
00822      EXEC CICS SEND                                               EL112
00823          MAP     ('EL112A')                                       EL112
00824          MAPSET  ('EL112S')                                       EL112
00825          ERASE                                                    EL112
00826          FREEKB                                                   EL112
00827          CURSOR                                                   EL112
00828      END-EXEC.                                                    EL112
00829                                                                   EL112
00830      GO TO 8120-RETURN-TRANS.                                     EL112
00831                                                                   EL112
00832  8110-SEND-DATA.                                                  EL112
00833      PERFORM 8130-FORMAT-DATE-TIME THRU 8140-EXIT.                EL112
00834      EXEC CICS SEND                                               EL112
00835          MAP     ('EL112A')                                       EL112
00836          MAPSET  ('EL112S')                                       EL112
00837          DATAONLY                                                 EL112
00838          ERASEAUP                                                 EL112
00839          FREEKB                                                   EL112
00840          CURSOR                                                   EL112
00841      END-EXEC.                                                    EL112
00842                                                                   EL112
00843  8120-RETURN-TRANS.                                               EL112
00844      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.         EL112
00845      EXEC CICS RETURN                                             EL112
00846          TRANSID   (LIT-TRANS)                                    EL112
00847          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL112
00848          LENGTH    (PI-COMM-LENGTH)                               EL112
00849      END-EXEC.                                                    EL112
00850                                                                   EL112
00851      GOBACK.                                                      EL112
00852                                                                   EL112
00853  8130-FORMAT-DATE-TIME.                                           EL112
00854      MOVE SAVE-DATE              TO DATEO.                        EL112
00855      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL**4
00856      END-EXEC                                                        CL**4
00857      EXEC CICS FORMATTIME                                            CL**4
00858                ABSTIME(LCP-CICS-TIME)                                CL**4
00859                TIME(LCP-TIME-OF-DAY-XX)                              CL**4
00860      END-EXEC                                                        CL**4
00861      MOVE  LCP-TIME-OF-DAY-68 TO TIME-UNFORMATTED.                   CL**4
00862      MOVE UN-HOURS               TO FOR-HOURS.                    EL112
00863      MOVE UN-MINUTES             TO FOR-MINUTES.                  EL112
00864      MOVE TIME-FORMATTED         TO TIMEO.                        EL112
00865                                                                   EL112
00866  8140-EXIT.                                                       EL112
00867      EXIT.                                                        EL112
00868                                                                   EL112
00869  8200-NO-UPDATES.                                                 EL112
00870      EXEC CICS SEND TEXT                                          EL112
00871          FROM    (SCREEN-TERM)                                    EL112
00872          LENGTH  (TERM-LENGTH)                                    EL112
00873          ERASE                                                    EL112
00874          FREEKB                                                   EL112
00875      END-EXEC.                                                    EL112
00876                                                                   EL112
00877  8220-RETURN-CICS.                                                EL112
00878      EXEC CICS RETURN                                             EL112
00879      END-EXEC.                                                    EL112
00880                                                                   EL112
00881      GOBACK.                                                      EL112
00882                                                                   EL112
00883  8230-NOT-OPEN.                                                   EL112
00884      EXEC CICS SEND TEXT                                          EL112
00885          FROM    (FILE-MSG)                                       EL112
00886          LENGTH  (FILE-LENGTH)                                    EL112
00887          ERASE                                                    EL112
00888          FREEKB                                                   EL112
00889      END-EXEC.                                                    EL112
00890                                                                   EL112
00891      GO TO 8220-RETURN-CICS.                                      EL112
00892                                                                   EL112
00893  9000-XCTL.                                                       EL112
00894      EXEC CICS XCTL                                               EL112
00895          PROGRAM  (CALL-PROG)                                     EL112
00896          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL112
00897          LENGTH   (PI-COMM-LENGTH)                                EL112
00898      END-EXEC.                                                    EL112
00899                                                                   EL112
00900      GOBACK.                                                      EL112
00901                                                                   EL112
00902  9100-RETURN-CICS SECTION.                                        EL112
00903      MOVE 'EL005'                TO  CALL-PROG.                   EL112
00904      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL112
00905      GO TO 9000-XCTL.                                                CL**4
00906                                                                   EL112
00907  9100-EXIT.                                                       EL112
00908      EXIT.                                                        EL112
00909                                                                   EL112
00910  9400-CLEAR SECTION.                                              EL112
00911      MOVE PI-RETURN-TO-PROGRAM  TO  CALL-PROG.                    EL112
00912      GO TO 9000-XCTL.                                                CL**4
00913                                                                   EL112
00914  9400-EXIT.                                                       EL112
00915      EXIT.                                                        EL112
00916                                                                   EL112
00917  9700-LINK-DATE-CONVERT.                                          EL112
00918      EXEC CICS LINK                                               EL112
00919          PROGRAM    ('ELDATCV')                                   EL112
00920          COMMAREA   (DATE-CONVERSION-DATA)                        EL112
00921          LENGTH     (DC-COMM-LENGTH)                              EL112
00922          END-EXEC.                                                EL112
00923                                                                   EL112
00924  9700-EXIT.                                                       EL112
00925      EXIT.                                                        EL112
