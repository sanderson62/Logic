00001  IDENTIFICATION DIVISION.                                         03/09/95
00002                                                                   EL110
00003  PROGRAM-ID.                 EL110 .                                 LV006
00004 *              PROGRAM CONVERTED BY                                  CL**6
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**6
00006 *              CONVERSION DATE 03/02/95 10:36:31.                    CL**6
00007 *                            VMOD=2.006.                             CL**6
00008 *AUTHOR.        LOGIC, INC.                                          CL**6
00009 *               DALLAS, TEXAS.                                       CL**6
00010                                                                   EL110
00011 *DATE-COMPILED.                                                      CL**6
00012                                                                   EL110
00013 *SECURITY.   *****************************************************   CL**6
00014 *            *                                                   *   CL**6
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**6
00016 *            *                                                   *   CL**6
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**6
00018 *                                                                *   CL**6
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**6
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**6
00021 *            *                                                   *   CL**6
00022 *            *****************************************************   CL**6
00023                                                                   EL110
00024 *REMARKS. TRANSACTION EX35 - ERROR FILE MAINTENANCE.                 CL**2
00025      EJECT                                                        EL110
00026  ENVIRONMENT DIVISION.                                            EL110
00027  DATA DIVISION.                                                   EL110
00028                                                                   EL110
00029  WORKING-STORAGE SECTION.                                         EL110
00030  77  LCP-WS-ADDR-COMP              PIC S9(8) COMP.                   CL**6
00031  77  LCP-WS-ADDR-PNTR              REDEFINES LCP-WS-ADDR-COMP        CL**6
00032                                    USAGE POINTER.                    CL**6
00033  01  LCP-TIME-OF-DAY-XX.                                             CL**6
00034      05  LCP-TIME-OF-DAY-68        PIC 9(6).                         CL**6
00035      05  FILLER                    PIC 99.                           CL**6
00036  01  LCP-CICS-TIME                 PIC 9(15).                        CL**6
00037  77  FILLER  PIC X(32)  VALUE '********************************'. EL110
00038  77  FILLER  PIC X(32)  VALUE '*    EL110 WORKING STORAGE     *'. EL110
00039  77  FILLER  PIC X(32)  VALUE '*********** VMOD 2.006 *********'.    CL**6
00040                                                                   EL110
00041  01  WS-DATE-AREA.                                                EL110
00042      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL110
00043      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL110
00044                                                                   EL110
00045  01  LITERALS-NUMBERS.                                            EL110
00046      12  LIT-SPACE               PIC X       VALUE SPACE.         EL110
00047      12  LIT-A                   PIC X       VALUE 'A'.           EL110
00048      12  LIT-B                   PIC X       VALUE 'B'.           EL110
00049      12  LIT-C                   PIC X       VALUE 'C'.           EL110
00050      12  LIT-D                   PIC X       VALUE 'D'.           EL110
00051      12  LIT-E                   PIC X       VALUE 'E'.           EL110
00052      12  LIT-F                   PIC X       VALUE 'F'.           EL110
00053      12  LIT-S                   PIC X       VALUE 'S'.           EL110
00054      12  LIT-X                   PIC X       VALUE 'X'.           EL110
00055      12  LIT-EM                  PIC XX      VALUE 'EM'.          EL110
00056      12  LIT-PGM                 PIC X(8)    VALUE 'EL110'.       EL110
00057      12  MAP-EL110A              PIC X(8)    VALUE 'EL110A'.      EL110
00058      12  LIT-SYS                 PIC X(4)    VALUE 'SYS'.         EL110
00059      12  LIT-TRAN                PIC X(4)    VALUE 'EX35'.        EL110
00060      12  LIT-MAP                 PIC X(4)    VALUE '110A'.        EL110
00061      12  LIT-HELP                PIC X(8)    VALUE 'EL010'.       EL110
00062      12  CALL-PGM                PIC X(8).                        EL110
00063      12  XCTL-005                PIC X(8)    VALUE 'EL005'.       EL110
00064      12  XCTL-EL126              PIC X(8)    VALUE 'EL126'.          CL**3
00065      12  XCTL-EL626              PIC X(8)    VALUE 'EL626'.          CL**3
00066      12  XCTL-EM626              PIC X(8)    VALUE 'EM626'.          CL**3
00067      12  XCTL-GL800              PIC X(8)    VALUE 'GL800'.          CL**3
00068                                                                   EL110
00069  01  COUNT-FIELDS.                                                EL110
00070      12  COUNT-1                 PIC 99.                          EL110
00071      12  COUNT-2                 PIC 99.                          EL110
00072      12  COUNT-3                 PIC 99.                          EL110
00073                                                                   EL110
00074  01  EDIT-WORK-AREA.                                              EL110
00075      12  CHECK-MAINT             PIC X.                           EL110
00076          88  SHOW-OPTION                     VALUE 'S'.           EL110
00077          88  ADD-OPTION                      VALUE 'A'.           EL110
00078          88  CHANGE-OPTION                   VALUE 'C'.           EL110
00079          88  DELETE-OPTION                   VALUE 'D'.           EL110
00080          88  VALID-OPTION                  VALUE 'A' 'C' 'D' 'S'. EL110
00081      12  CHECK-SEV               PIC X.                           EL110
00082          88  VALID-SEV                     VALUE 'N' 'W' 'F' 'X'. EL110
00083      12  CHECK-PFKEYS            PIC 99.                          EL110
00084      12  SCREEN-SWITCH           PIC X.                           EL110
00085          88  END-OF-FILE                     VALUE 'E'.           EL110
00086          88  SCREEN-FULL                     VALUE 'F'.           EL110
00087          88  SCREEN-ERROR                    VALUE 'X'.           EL110
00088      12  BROWSE-KEY              PIC X(4).                        EL110
00089                                                                   EL110
00090  01  DISPLAY-LINE.                                                EL110
00091      12  ERR-CD                  PIC X(4).                        EL110
00092      12  FILLER                  PIC X.                           EL110
00093      12  ERR-SEV                 PIC X.                           EL110
00094      12  FILLER                  PIC XX.                          EL110
00095      12  ERR-TEXT                PIC X(65).                       EL110
00096                                                                   EL110
00097  01  TIME-UNFORMATTED.                                            EL110
00098      12  UN-HOURS                PIC XX.                          EL110
00099      12  UN-MINUTES              PIC XX.                          EL110
00100      12  FILLER                  PIC X(4).                        EL110
00101                                                                   EL110
00102  01  TIME-FORMATTED.                                              EL110
00103      12  FOR-HOURS               PIC XX.                          EL110
00104      12  FILLER                  PIC X       VALUE '.'.           EL110
00105      12  FOR-MINUTES             PIC XX.                          EL110
00106                                                                   EL110
00107  01  ERROR-MESSAGES.                                              EL110
00108      12  MAINT-MIS.                                               EL110
00109          16  FILLER              PIC X(39)                        EL110
00110              VALUE 'MAINTENANCE FUNCTION INVALID OR UNAUTHO'.     EL110
00111          16  FILLER              PIC X(18)                        EL110
00112              VALUE 'RIZED             '.                          EL110
00113      12  LANG-MIS.                                                   CL**5
00114          16  FILLER              PIC X(39)                           CL**5
00115              VALUE 'LANGUAGE CODE INVALID OR NOT YET SUPPOR'.        CL**5
00116          16  FILLER              PIC X(18)                           CL**5
00117              VALUE 'TED               '.                             CL**5
00118      12  ERROR-MIS               PIC X(46)                        EL110
00119          VALUE 'ERROR NUMBER MISSING OR INVALID - PLEASE ENTER'.  EL110
00120      12  ERROR-INV               PIC X(38)                        EL110
00121          VALUE 'ERROR NUMBER INVALID - PLEASE RE-ENTER'.          EL110
00122      12  ERROR-DUP               PIC X(40)                        EL110
00123          VALUE 'ERROR NUMBER DUPLICATE - PLEASE RE-ENTER'.        EL110
00124      12  TEXT-MIS                PIC X(40)                        EL110
00125          VALUE 'TEXT IS A MANDATORY FIELD - PLEASE ENTER'.        EL110
00126      12  SEV-MIS                 PIC X(42)                        EL110
00127          VALUE 'SEVERITY MISSING OR INVALID - PLEASE ENTER'.      EL110
00128      12  INVALID-PFKEY           PIC X(45)                        EL110
00129          VALUE 'INVALID PF-OPTION REQUESTED - PLEASE RE-ENTER'.   EL110
00130      12  END-MSG                 PIC X(11)                        EL110
00131          VALUE 'END OF FILE'.                                     EL110
00132      12  END-UPDATE              PIC X(16)                        EL110
00133          VALUE 'UPDATE COMPLETED'.                                EL110
00134      12  SCREEN-TERM             PIC X(40)                        EL110
00135            VALUE '     CLAS-IC ERROR MAINTENANCE COMPLETED'.      EL110
00136      12  FILE-MSG                PIC X(25)                        EL110
00137            VALUE '     FILE ELERRS NOT OPEN'.                     EL110
00138      12  FILE-MSG-FR             PIC X(25)                           CL**5
00139            VALUE '     FILE MPFERR NOT OPEN'.                        CL**5
00140                                                                   EL110
00141  01  COMP-LENGTHS.                                                EL110
00142      12  LIT-IC                  PIC S9(4)  COMP VALUE -1.        EL110
00143      12  FILE-LENGTH             PIC S9(4)  COMP VALUE +25.       EL110
00144      12  TERM-LENGTH             PIC S9(4)  COMP VALUE +40.       EL110
00145      12  COMM-LENGTH             PIC S9(4)  COMP VALUE +10.       EL110
00146      12  JOURNAL-LENGTH          PIC S9(4)  COMP VALUE +95.       EL110
00147      12  ERRS-LENGTH             PIC S9(4)  COMP VALUE +72.       EL110
00148                                                                   EL110
00149      EJECT                                                        EL110
00150                                  COPY ELCDATE.                       CL**4
00151      EJECT                                                           CL**5
00152                                  COPY ELCERRS.                       CL**5
00153      EJECT                                                        EL110
00154                                  COPY ELCATTR.                       CL**4
00155      EJECT                                                        EL110
00156                                  COPY ELCLOGOF.                      CL**4
00157      EJECT                                                        EL110
00158                                                                   EL110
00159                                  COPY ELCAID.                        CL**4
00160  01  FILLER REDEFINES DFHAID.                                     EL110
00161      12  FILLER                  PIC X(8).                        EL110
00162      12  AID-KEYS OCCURS 24 TIMES.                                EL110
00163          16  FILLER              PIC X.                           EL110
00164      EJECT                                                        EL110
00165                                  COPY ELCJPFX.                       CL**4
00166                                  PIC X(72).                       EL110
00167      EJECT                                                        EL110
00168                                  COPY ELCINTF.                       CL**4
00169      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                   EL110
00170          16  MENU-SWITCH         PIC X.                           EL110
00171          16  SAVE-BEGIN          PIC X(4).                        EL110
00172          16  SAVE-ENDING         PIC X(4).                        EL110
00173          16  SHOW-SWITCH         PIC X.                           EL110
00174              88   NOT-SHOWN                 VALUE 'X'.            EL110
00175          16  PI-FILE-ID          PIC X(8).                           CL**5
00176              88 PI-FRENCH-FILE              VALUE 'MPFERR'.          CL**5
00177              88 PI-ENGLISH-FILE             VALUE 'ELERRS'.          CL**5
00178          16  PI-POINTER          PIC S9(8)  COMP.                    CL**5
00179          16  FILLER              PIC X(618).                         CL**6
00180      EJECT                                                        EL110
00181                                  COPY EL110S.                        CL**4
00182      EJECT                                                        EL110
00183  01  ERROR-MESSAGE-FILE-REST     PIC X(72).                          CL**6
00184  01  ERROR-MESSAGE-FILE-FR       PIC X(72).                          CL**6
00185  LINKAGE SECTION.                                                 EL110
00186  01  DFHCOMMAREA                 PIC X(1024).                     EL110
00187                                                                   EL110
00188 *01 PARM-LIST .                                                      CL**6
00189 *    12  FILLER                  PIC S9(8)  COMP.                    CL**6
00190 *    12  ERRS-PNT                PIC S9(8)  COMP.                    CL**6
00191 *    12  FERR-PNT                PIC S9(8)  COMP.                    CL**6
00192                                                                      CL**5
00193 ******************************************************************   CL**5
00194      EJECT                                                        EL110
00195  PROCEDURE DIVISION.                                              EL110
00196      CONTINUE.                                                       CL**6
00197                                                                   EL110
00198      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL110
00199      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL110
00200      MOVE '5'                    TO DC-OPTION-CODE.               EL110
00201      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL110
00202      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL110
00203      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL110
00204                                                                   EL110
00205      IF EIBCALEN = ZERO                                           EL110
00206          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL110
00207                                                                   EL110
00208      IF PI-CALLING-PROGRAM NOT = LIT-PGM                          EL110
00209          IF PI-RETURN-TO-PROGRAM NOT = LIT-PGM                    EL110
00210              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6    EL110
00211              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5    EL110
00212              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4    EL110
00213              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3    EL110
00214              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2    EL110
00215              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1    EL110
00216              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM  EL110
00217              MOVE LIT-PGM               TO  PI-CALLING-PROGRAM    EL110
00218              MOVE LOW-VALUES            TO  EL110AO               EL110
00219              GO TO 8100-SEND-MAP                                  EL110
00220          ELSE                                                     EL110
00221              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM    EL110
00222              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM  EL110
00223              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1    EL110
00224              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2    EL110
00225              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3    EL110
00226              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4    EL110
00227              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5    EL110
00228              MOVE SPACES                TO  PI-SAVED-PROGRAM-6    EL110
00229              MOVE LOW-VALUES            TO  EL110AO               EL110
00230              GO TO 8100-SEND-MAP.                                 EL110
00231                                                                   EL110
00232      IF EIBAID = DFHCLEAR                                         EL110
00233          GO TO 9000-XCTL.                                         EL110
00234                                                                   EL110
00235      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL110
00236          MOVE LIT-IC             TO PFKEYL                        EL110
00237          MOVE INVALID-PFKEY      TO MSGO                          EL110
00238          GO TO 8110-SEND-DATA.                                    EL110
00239                                                                   EL110
00240      EXEC CICS RECEIVE                                            EL110
00241          MAP ('EL110A')                                           EL110
00242          MAPSET ('EL110S')                                        EL110
00243      END-EXEC.                                                    EL110
00244                                                                   EL110
00245      MOVE SPACE                  TO SCREEN-SWITCH.                EL110
00246                                                                   EL110
00247      IF PFKEYL GREATER ZERO                                       EL110
00248          PERFORM 0300-TRANS-PF THRU 0310-EXIT.                    EL110
00249                                                                   EL110
00250      IF SCREEN-ERROR                                              EL110
00251          MOVE LIT-IC             TO PFKEYL                        EL110
00252          GO TO 8110-SEND-DATA.                                    EL110
00253                                                                   EL110
00254  0010-MAPFAIL-CONTINUE.                                           EL110
00255      IF EIBAID = DFHPF23                                          EL110
00256          GO TO 9100-XCTL.                                         EL110
00257                                                                   EL110
00258      IF EIBAID = DFHPF24                                          EL110
00259          GO TO 9200-XCTL.                                         EL110
00260                                                                   EL110
00261      IF  LANGL NOT EQUAL ZEROS                                       CL**5
00262                                                                      CL**5
00263          IF  LANGI EQUAL 'F'                                         CL**5
00264              MOVE 'MPFERR'       TO PI-FILE-ID                       CL**5
00265              MOVE LCP-WS-ADDR-COMP TO PI-POINTER                     CL**6
00266                                                                      CL**5
00267          ELSE                                                        CL**5
00268              MOVE 'ELERRS'       TO PI-FILE-ID                       CL**5
00269              MOVE LCP-WS-ADDR-COMP TO PI-POINTER                     CL**6
00270                                                                      CL**5
00271              IF  LANGI EQUAL 'E' OR SPACES OR LOW-VALUES             CL**5
00272                  MOVE 'E'        TO LANGI                            CL**5
00273                                                                      CL**5
00274              ELSE                                                    CL**5
00275                  MOVE LIT-IC     TO LANGL                            CL**5
00276                  MOVE AL-UABON   TO LANGA                            CL**5
00277                  MOVE LANG-MIS   TO MSGO                             CL**5
00278                  MOVE LIT-X      TO SCREEN-SWITCH                    CL**5
00279                  GO TO 8110-SEND-DATA                                CL**6
00280                                                                      CL**5
00281      ELSE                                                            CL**5
00282          MOVE 'ELERRS'           TO PI-FILE-ID                       CL**5
00283          MOVE LCP-WS-ADDR-COMP TO PI-POINTER.                        CL**6
00284                                                                      CL**5
00285      IF EIBAID = DFHPF1                                           EL110
00286          GO TO 0100-PAGE-FORWARD.                                 EL110
00287                                                                   EL110
00288      IF EIBAID = DFHPF2                                           EL110
00289          GO TO 0200-PAGE-BACKWARD.                                EL110
00290                                                                   EL110
00291      IF EIBAID = DFHPF12                                          EL110
00292          GO TO 8300-GET-HELP.                                     EL110
00293                                                                   EL110
00294      IF EIBAID NOT = DFHENTER                                     EL110
00295          MOVE INVALID-PFKEY      TO MSGO                          EL110
00296          MOVE LIT-IC             TO MAINTL                        EL110
00297          GO TO 8110-SEND-DATA.                                    EL110
00298                                                                   EL110
00299      PERFORM 0400-SET-ATTRB THRU 0410-EXIT.                       EL110
00300      MOVE SPACE                  TO SCREEN-SWITCH.                EL110
00301      PERFORM 1000-EDIT-SCREEN THRU 1010-EXIT.                     EL110
00302                                                                   EL110
00303      IF SCREEN-ERROR                                              EL110
00304          GO TO 8110-SEND-DATA.                                    EL110
00305                                                                   EL110
00306      PERFORM 1100-UPDATE-FILE THRU 1110-EXIT.                     EL110
00307      MOVE LIT-IC                 TO MAINTL.                       EL110
00308                                                                   EL110
00309      IF SHOW-OPTION                                               EL110
00310          MOVE SPACES             TO MSGO                          EL110
00311          GO TO 8110-SEND-DATA                                     EL110
00312      ELSE                                                         EL110
00313          MOVE LIT-X              TO SHOW-SWITCH                   EL110
00314          MOVE LOW-VALUES         TO EL110AO                       EL110
00315          MOVE LIT-IC             TO MAINTL                        EL110
00316          MOVE END-UPDATE         TO MSGO                          EL110
00317          MOVE HIGH-VALUES        TO SAVE-BEGIN                    EL110
00318          MOVE LOW-VALUES         TO SAVE-ENDING                   EL110
00319          GO TO 8100-SEND-MAP.                                     EL110
00320      EJECT                                                        EL110
00321                                                                   EL110
00322  0100-PAGE-FORWARD.                                               EL110
00323      MOVE ZEROS                  TO COUNT-1.                      EL110
00324      MOVE SPACES                 TO SCREEN-SWITCH MSGO.           EL110
00325                                                                   EL110
00326      IF ERRORL = ZEROS                                            EL110
00327          MOVE SAVE-ENDING        TO BROWSE-KEY                    EL110
00328      ELSE                                                         EL110
00329          MOVE AL-UNNOF           TO ERRORA                        EL110
00330          MOVE ERRORI             TO BROWSE-KEY.                   EL110
00331                                                                   EL110
00332      MOVE SPACES                 TO MAINTO                        EL110
00333                                     TEXTO                         EL110
00334                                     SEVO.                         EL110
00335      EXEC CICS HANDLE CONDITION                                   EL110
00336          NOTOPEN (8230-NOT-OPEN)                                  EL110
00337          NOTFND (0110-REC-NOT-FND)                                EL110
00338      END-EXEC.                                                    EL110
00339                                                                   EL110
00340      EXEC CICS STARTBR                                            EL110
00341          DATASET (PI-FILE-ID)                                        CL**5
00342          RIDFLD (BROWSE-KEY)                                      EL110
00343      END-EXEC.                                                    EL110
00344                                                                   EL110
00345      PERFORM 3000-BUILD-FWD-PAGE THRU 3020-EXIT                   EL110
00346          UNTIL SCREEN-FULL OR END-OF-FILE.                        EL110
00347                                                                   EL110
00348      EXEC CICS ENDBR                                              EL110
00349          DATASET (PI-FILE-ID)                                        CL**5
00350      END-EXEC.                                                    EL110
00351                                                                   EL110
00352      MOVE LIT-IC                 TO MAINTL.                       EL110
00353      GO TO 8110-SEND-DATA.                                        EL110
00354                                                                   EL110
00355  0110-REC-NOT-FND.                                                EL110
00356      MOVE HIGH-VALUES            TO SAVE-BEGIN SAVE-ENDING.       EL110
00357                                                                   EL110
00358      PERFORM 3100-FILL-SCREEN THRU 3110-EXIT                      EL110
00359          UNTIL SCREEN-FULL.                                       EL110
00360      MOVE LIT-IC                 TO MAINTL.                       EL110
00361      MOVE END-MSG                TO MSGO.                         EL110
00362      GO TO 8110-SEND-DATA.                                        EL110
00363      EJECT                                                        EL110
00364  0200-PAGE-BACKWARD.                                              EL110
00365      MOVE 11                     TO COUNT-1.                      EL110
00366      MOVE SPACES                 TO SCREEN-SWITCH MSGO.           EL110
00367                                                                   EL110
00368      IF ERRORL = ZEROS                                            EL110
00369          MOVE SAVE-BEGIN         TO BROWSE-KEY                    EL110
00370      ELSE                                                         EL110
00371          MOVE AL-UNNOF           TO ERRORA                        EL110
00372          MOVE ERRORI             TO BROWSE-KEY SAVE-BEGIN.        EL110
00373          MOVE SPACES             TO MAINTO                        EL110
00374                                     TEXTO                         EL110
00375                                     SEVO.                         EL110
00376                                                                   EL110
00377      EXEC CICS HANDLE CONDITION                                   EL110
00378          NOTOPEN (8230-NOT-OPEN)                                  EL110
00379          NOTFND (0210-REC-NOT-FND)                                EL110
00380      END-EXEC.                                                    EL110
00381                                                                   EL110
00382      EXEC CICS STARTBR                                            EL110
00383          DATASET (PI-FILE-ID)                                        CL**5
00384          RIDFLD (BROWSE-KEY)                                      EL110
00385      END-EXEC.                                                    EL110
00386                                                                   EL110
00387      IF BROWSE-KEY = HIGH-VALUES                                  EL110
00388          GO TO 0205-SKIP-RESET.                                   EL110
00389                                                                   EL110
00390      EXEC CICS READNEXT                                           EL110
00391          DATASET (PI-FILE-ID)                                        CL**5
00392          RIDFLD (BROWSE-KEY)                                      EL110
00393          INTO (ERROR-MESSAGE-FILE)                                   CL**5
00394      END-EXEC.                                                    EL110
00395                                                                   EL110
00396      EXEC CICS RESETBR                                            EL110
00397          DATASET (PI-FILE-ID)                                        CL**5
00398          RIDFLD (BROWSE-KEY)                                      EL110
00399          EQUAL                                                    EL110
00400      END-EXEC.                                                    EL110
00401                                                                   EL110
00402  0205-SKIP-RESET.                                                 EL110
00403      PERFORM 3030-BUILD-BCK-PAGE THRU 3050-EXIT                   EL110
00404          UNTIL SCREEN-FULL OR END-OF-FILE.                        EL110
00405                                                                   EL110
00406      EXEC CICS ENDBR                                              EL110
00407          DATASET (PI-FILE-ID)                                        CL**5
00408      END-EXEC.                                                    EL110
00409                                                                   EL110
00410      MOVE COUNT-1                TO COUNT-3.                      EL110
00411      PERFORM 3060-RAISE-PAGE THRU 3070-EXIT                       EL110
00412          COUNT-3 TIMES.                                           EL110
00413      MOVE LIT-IC                 TO MAINTL.                       EL110
00414      GO TO 8110-SEND-DATA.                                        EL110
00415                                                                   EL110
00416  0210-REC-NOT-FND.                                                EL110
00417      MOVE HIGH-VALUES            TO SAVE-BEGIN.                   EL110
00418      MOVE ZEROS                  TO ERRORL.                       EL110
00419      GO TO 0200-PAGE-BACKWARD.                                    EL110
00420      EJECT                                                        EL110
00421  0300-TRANS-PF.                                                   EL110
00422      IF EIBAID NOT = DFHENTER                                     EL110
00423          MOVE INVALID-PFKEY      TO MSGO                          EL110
00424          MOVE LIT-X              TO SCREEN-SWITCH                 EL110
00425          GO TO 0310-EXIT.                                         EL110
00426                                                                   EL110
00427      IF PFKEYI NOT NUMERIC                                        EL110
00428          MOVE INVALID-PFKEY      TO MSGO                          EL110
00429          MOVE LIT-X              TO SCREEN-SWITCH                 EL110
00430          GO TO 0310-EXIT.                                         EL110
00431                                                                   EL110
00432      IF PFKEYI LESS 1 OR GREATER 24                               EL110
00433          MOVE INVALID-PFKEY      TO MSGO                          EL110
00434          MOVE LIT-X              TO SCREEN-SWITCH                 EL110
00435          GO TO 0310-EXIT.                                         EL110
00436                                                                   EL110
00437      MOVE PFKEYI                 TO CHECK-PFKEYS.                 EL110
00438      MOVE AID-KEYS (CHECK-PFKEYS)                    TO EIBAID.   EL110
00439                                                                   EL110
00440  0310-EXIT.                                                       EL110
00441      EXIT.                                                        EL110
00442                                                                   EL110
00443  0400-SET-ATTRB.                                                  EL110
00444      MOVE AL-UANON               TO MAINTA                        EL110
00445                                     TEXTA                         EL110
00446                                     LANGA                            CL**5
00447                                     SEVA.                         EL110
00448      MOVE AL-UNNON               TO ERRORA.                       EL110
00449                                                                   EL110
00450  0410-EXIT.                                                       EL110
00451      EXIT.                                                        EL110
00452                                                                   EL110
00453  1000-EDIT-SCREEN.                                                EL110
00454 *                                                                    CL**5
00455 *    IF  LANGL NOT EQUAL ZEROS                                       CL**5
00456 *                                                                    CL**5
00457 *        IF  LANGI EQUAL 'F'                                         CL**5
00458 *            MOVE 'MPFERR'       TO PI-FILE-ID                       CL**5
00459 *            MOVE FERR-PNT       TO PI-POINTER                       CL**5
00460 *                                                                    CL**5
00461 *        ELSE                                                        CL**5
00462 *            MOVE 'ELERRS'       TO PI-FILE-ID                       CL**5
00463 *            MOVE ERRS-PNT       TO PI-POINTER                       CL**5
00464 *                                                                    CL**5
00465 *            IF  LANGI EQUAL 'E' OR SPACES OR LOW-VALUES             CL**5
00466 *                MOVE 'E'        TO LANGI                            CL**5
00467 *                                                                    CL**5
00468 *            ELSE                                                    CL**5
00469 *                MOVE LIT-IC     TO LANGL                            CL**5
00470 *                MOVE AL-UABON   TO LANGA                            CL**5
00471 *                MOVE LANG-MIS   TO MSGO                             CL**5
00472 *                MOVE LIT-X      TO SCREEN-SWITCH                    CL**5
00473 *                GO TO 1010-EXIT                                     CL**5
00474 *    ELSE                                                            CL**5
00475 *        MOVE 'ELERRS'           TO PI-FILE-ID                       CL**5
00476 *        MOVE ERRS-PNT           TO PI-POINTER.                      CL**5
00477                                                                      CL**5
00478      MOVE MAINTI                 TO CHECK-MAINT.                  EL110
00479                                                                   EL110
00480      IF PI-COMPANY-ID = 'MON'                                        CL**4
00481       IF NOT VALID-OPTION                                            CL**4
00482          MOVE LIT-IC             TO MAINTL                           CL**4
00483          MOVE AL-UABON           TO MAINTA                           CL**4
00484          MOVE MAINT-MIS          TO MSGO                             CL**4
00485          MOVE LIT-X              TO SCREEN-SWITCH                    CL**4
00486          GO TO 1010-EXIT.                                            CL**4
00487                                                                      CL**4
00488      IF PI-COMPANY-ID = 'LII'                                        CL**5
00489             AND                                                      CL**5
00490         PI-PROCESSOR-ID EQUAL 'TPT'                                  CL**5
00491         NEXT SENTENCE                                                CL**5
00492                                                                      CL**5
00493      ELSE                                                            CL**5
00494      IF PI-COMPANY-ID NOT = 'MON'                                    CL**4
00495       IF (NOT VALID-OPTION)
CIDMOD               OR
CIDMOD        ((PI-PROCESSOR-ID NOT = 'EMER' AND 'PEMA')
CIDMOD        AND (NOT SHOW-OPTION))
00496 **      (PI-PROCESSOR-ID NOT = 'LGXX' AND NOT SHOW-OPTION)        EL110
00497          MOVE LIT-IC             TO MAINTL                        EL110
00498          MOVE AL-UABON           TO MAINTA                        EL110
00499          MOVE MAINT-MIS          TO MSGO                          EL110
00500          MOVE LIT-X              TO SCREEN-SWITCH                 EL110
00501          GO TO 1010-EXIT.                                         EL110
00502                                                                   EL110
00503      IF ERRORI = LOW-VALUES                                       EL110
00504          MOVE LIT-IC             TO ERRORL                        EL110
00505          MOVE AL-UNBON           TO ERRORA                        EL110
00506          MOVE ERROR-MIS          TO MSGO                          EL110
00507          MOVE LIT-X              TO SCREEN-SWITCH                 EL110
00508          GO TO 1010-EXIT.                                         EL110
00509                                                                   EL110
00510      IF DELETE-OPTION AND NOT-SHOWN                               EL110
00511          MOVE SPACES             TO SHOW-SWITCH SCREEN-SWITCH     EL110
00512          MOVE ZEROES             TO COUNT-1                       EL110
00513          PERFORM 3100-FILL-SCREEN THRU 3110-EXIT                  EL110
00514              UNTIL SCREEN-FULL                                    EL110
00515          MOVE LIT-S              TO CHECK-MAINT.                  EL110
00516                                                                   EL110
00517      IF CHANGE-OPTION AND NOT-SHOWN                               EL110
00518          MOVE SPACES             TO SHOW-SWITCH SCREEN-SWITCH     EL110
00519          MOVE ZEROES             TO COUNT-1                       EL110
00520          PERFORM 3100-FILL-SCREEN THRU 3110-EXIT                  EL110
00521              UNTIL SCREEN-FULL                                    EL110
00522          MOVE LIT-S              TO CHECK-MAINT.                  EL110
00523                                                                   EL110
00524      PERFORM 1020-VERIFY-ERROR THRU 1050-EXIT.                    EL110
00525                                                                   EL110
00526      IF SCREEN-ERROR OR DELETE-OPTION                             EL110
00527          GO TO 1010-EXIT.                                         EL110
00528                                                                   EL110
00529      IF SHOW-OPTION                                               EL110
00530          MOVE SPACES             TO SHOW-SWITCH SCREEN-SWITCH     EL110
00531          MOVE ZEROES             TO COUNT-1                       EL110
00532          PERFORM 3100-FILL-SCREEN THRU 3110-EXIT                  EL110
00533              UNTIL SCREEN-FULL                                    EL110
00534          GO TO 1010-EXIT.                                         EL110
00535                                                                   EL110
00536      IF TEXTI = SPACES OR LOW-VALUES                              EL110
00537          MOVE LIT-IC             TO TEXTL                         EL110
00538          MOVE AL-UABON           TO TEXTA                         EL110
00539          MOVE TEXT-MIS           TO MSGO                          EL110
00540          MOVE LIT-X              TO SCREEN-SWITCH                 EL110
00541          GO TO 1010-EXIT.                                         EL110
00542                                                                   EL110
00543      MOVE SEVI                   TO CHECK-SEV.                    EL110
00544                                                                   EL110
00545      IF NOT VALID-SEV                                             EL110
00546          MOVE LIT-IC             TO SEVL                          EL110
00547          MOVE AL-UABON           TO SEVA                          EL110
00548          MOVE SEV-MIS            TO MSGO                          EL110
00549          MOVE LIT-X              TO SCREEN-SWITCH.                EL110
00550                                                                   EL110
00551  1010-EXIT.                                                       EL110
00552      EXIT.                                                        EL110
00553      EJECT                                                        EL110
00554  1020-VERIFY-ERROR.                                               EL110
00555      EXEC CICS HANDLE CONDITION                                   EL110
00556          NOTOPEN (1040-RECORD-NOT-FOUND)                          EL110
00557          NOTFND (1040-RECORD-NOT-FOUND)                           EL110
00558      END-EXEC.                                                    EL110
00559                                                                   EL110
00560      MOVE ERRORI TO BROWSE-KEY.                                   EL110
00561      EXEC CICS READ                                               EL110
00562          DATASET (PI-FILE-ID)                                        CL**5
00563          RIDFLD (BROWSE-KEY)                                      EL110
00564          INTO (ERROR-MESSAGE-FILE)                                   CL**5
00565      END-EXEC.                                                    EL110
00566 *                                                                    CL**5
00567 *    IF  PI-FRENCH-FILE                                              CL**5
00568 *        SERVICE RELOAD ERROR-MESSAGE-FILE-FR                        CL**5
00569 *                                                                    CL**5
00570 *    ELSE                                                            CL**5
00571 *        SERVICE RELOAD ERROR-MESSAGE-FILE-REST.                     CL**5
00572                                                                   EL110
00573      IF ADD-OPTION                                                EL110
00574          MOVE ERROR-DUP          TO MSGO                          EL110
00575          MOVE AL-UNBON           TO ERRORA                        EL110
00576          MOVE LIT-IC             TO ERRORL                        EL110
00577          MOVE LIT-X              TO SCREEN-SWITCH                 EL110
00578          GO TO 1050-EXIT.                                         EL110
00579                                                                   EL110
00580      IF NOT SHOW-OPTION                                           EL110
00581          GO TO 1050-EXIT.                                         EL110
00582                                                                   EL110
00583      MOVE EM-ERROR-TEXT          TO TEXTO.                        EL110
00584      MOVE EM-ERROR-SEVERITY      TO SEVO.                         EL110
00585      GO TO 1050-EXIT.                                             EL110
00586                                                                   EL110
00587  1040-RECORD-NOT-FOUND.                                           EL110
00588      IF NOT ADD-OPTION                                            EL110
00589          MOVE ERROR-INV          TO MSGO                          EL110
00590          MOVE AL-UNBON           TO ERRORA                        EL110
00591          MOVE LIT-IC             TO ERRORL                        EL110
00592          MOVE LIT-X              TO SCREEN-SWITCH.                EL110
00593                                                                   EL110
00594  1050-EXIT.                                                       EL110
00595      EXIT.                                                        EL110
00596      EJECT                                                        EL110
00597  1100-UPDATE-FILE.                                                EL110
00598      IF SHOW-OPTION                                               EL110
00599          GO TO 1110-EXIT.                                         EL110
00600                                                                   EL110
00601      IF ADD-OPTION                                                EL110
00602          PERFORM 1120-ADD-OPTION THRU 1140-EXIT.                  EL110
00603                                                                   EL110
00604      IF CHANGE-OPTION                                             EL110
00605          PERFORM 1150-CHANGE-OPTION THRU 1160-EXIT.               EL110
00606                                                                   EL110
00607      IF DELETE-OPTION                                             EL110
00608          PERFORM 1170-DELETE-OPTION THRU 1180-EXIT.               EL110
00609                                                                   EL110
00610  1110-EXIT.                                                       EL110
00611      EXIT.                                                        EL110
00612                                                                   EL110
00613  1120-ADD-OPTION.                                                 EL110
00614      EXEC CICS HANDLE CONDITION                                   EL110
00615          NOTOPEN (8230-NOT-OPEN)                                  EL110
00616          NOSPACE (1130-FILE-FULL)                                 EL110
00617      END-EXEC.                                                    EL110
00618                                                                   EL110
00619      EXEC CICS GETMAIN                                            EL110
00620          SET (PI-POINTER)                                            CL**5
00621          LENGTH (ERRS-LENGTH)                                     EL110
00622          INITIMG (LIT-SPACE)                                      EL110
00623      END-EXEC.                                                    EL110
00624                                                                   EL110
00625      IF  PI-FRENCH-FILE                                              CL**5
00626          CONTINUE                                                    CL**6
00627                                                                      CL**5
00628      ELSE                                                            CL**5
00629          CONTINUE.                                                   CL**6
00630                                                                   EL110
00631      MOVE SPACES                 TO ERROR-MESSAGE-FILE.           EL110
00632      MOVE LIT-EM                 TO EM-RECORD-ID.                 EL110
00633      MOVE ERRORI                 TO EM-MESSAGE-NUMBER BROWSE-KEY. EL110
00634      MOVE TEXTI                  TO EM-ERROR-TEXT.                EL110
00635      MOVE SEVI                   TO EM-ERROR-SEVERITY.            EL110
00636      MOVE LIT-A                  TO JP-RECORD-TYPE.               EL110
00637      MOVE ERROR-MESSAGE-FILE     TO JP-RECORD-AREA.               EL110
00638      EXEC CICS WRITE                                              EL110
00639          DATASET (PI-FILE-ID)                                        CL**5
00640          FROM (ERROR-MESSAGE-FILE)                                EL110
00641          RIDFLD (BROWSE-KEY)                                      EL110
00642      END-EXEC.                                                    EL110
00643                                                                   EL110
00644      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.                   EL110
00645                                                                   EL110
00646  1130-FILE-FULL.                                                  EL110
00647      MOVE LIT-X                  TO SCREEN-SWITCH.                EL110
00648                                                                   EL110
00649  1140-EXIT.                                                       EL110
00650      EXIT.                                                        EL110
00651      EJECT                                                        EL110
00652  1150-CHANGE-OPTION.                                              EL110
00653      EXEC CICS HANDLE CONDITION                                   EL110
00654          NOTOPEN (8230-NOT-OPEN)                                  EL110
00655      END-EXEC.                                                    EL110
00656                                                                   EL110
00657      MOVE ERRORI                 TO BROWSE-KEY.                   EL110
00658      PERFORM 1190-READ-FILE THRU 1200-EXIT.                       EL110
00659      MOVE LIT-B                  TO JP-RECORD-TYPE.               EL110
00660      MOVE ERROR-MESSAGE-FILE     TO JP-RECORD-AREA.               EL110
00661      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.                   EL110
00662      MOVE TEXTI                  TO EM-ERROR-TEXT.                EL110
00663      MOVE SEVI                   TO EM-ERROR-SEVERITY.            EL110
00664      MOVE LIT-C                  TO JP-RECORD-TYPE.               EL110
00665      MOVE ERROR-MESSAGE-FILE     TO JP-RECORD-AREA.               EL110
00666      EXEC CICS REWRITE                                            EL110
00667          DATASET (PI-FILE-ID)                                        CL**5
00668          FROM (ERROR-MESSAGE-FILE)                                EL110
00669      END-EXEC.                                                    EL110
00670                                                                   EL110
00671      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.                   EL110
00672                                                                   EL110
00673  1160-EXIT.                                                       EL110
00674      EXIT.                                                        EL110
00675                                                                   EL110
00676  1170-DELETE-OPTION.                                              EL110
00677      EXEC CICS HANDLE CONDITION                                   EL110
00678          NOTOPEN (8230-NOT-OPEN)                                  EL110
00679      END-EXEC.                                                    EL110
00680      MOVE ERRORI                 TO BROWSE-KEY.                   EL110
00681      PERFORM 1190-READ-FILE THRU 1200-EXIT.                       EL110
00682      MOVE LIT-D                  TO JP-RECORD-TYPE.               EL110
00683      MOVE ERROR-MESSAGE-FILE     TO JP-RECORD-AREA.               EL110
00684      EXEC CICS DELETE                                             EL110
00685          DATASET (PI-FILE-ID)                                        CL**5
00686      END-EXEC.                                                    EL110
00687      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.                   EL110
00688  1180-EXIT.                                                       EL110
00689      EXIT.                                                        EL110
00690                                                                   EL110
00691  1190-READ-FILE.                                                  EL110
00692      EXEC CICS READ                                               EL110
00693          DATASET (PI-FILE-ID)                                        CL**5
00694          RIDFLD  (BROWSE-KEY)                                     EL110
00695          INTO    (ERROR-MESSAGE-FILE)                                CL**5
00696          UPDATE                                                   EL110
00697      END-EXEC.                                                    EL110
00698 *                                                                    CL**5
00699 *    IF  PI-FRENCH-FILE                                              CL**5
00700 *        SERVICE RELOAD ERROR-MESSAGE-FILE-FR                        CL**5
00701 *                                                                    CL**5
00702 *    ELSE                                                            CL**5
00703 *        SERVICE RELOAD ERROR-MESSAGE-FILE-REST.                     CL**5
00704                                                                   EL110
00705  1200-EXIT.                                                       EL110
00706      EXIT.                                                        EL110
00707                                                                   EL110
00708  2000-JOURNAL-WRITE.                                              EL110
00709      MOVE LIT-SYS                TO JP-USER-ID.                   EL110
00710      MOVE PI-FILE-ID             TO JP-FILE-ID.                      CL**5
00711      MOVE LIT-PGM                TO JP-PROGRAM-ID.                EL110
pemuni*    IF PI-JOURNAL-FILE-ID NOT = ZERO                             EL110
pemuni*        EXEC CICS JOURNAL                                        EL110
pemuni*            JFILEID (1)                                          EL110
pemuni*            JTYPEID ('EL')                                       EL110
pemuni*            FROM    (JOURNAL-RECORD)                             EL110
pemuni*            LENGTH  (JOURNAL-LENGTH)                             EL110
pemuni*        END-EXEC.                                                EL110
00719                                                                   EL110
00720  2010-EXIT.                                                       EL110
00721      EXIT.                                                        EL110
00722      EJECT                                                        EL110
00723  3000-BUILD-FWD-PAGE.                                             EL110
00724      EXEC CICS HANDLE CONDITION                                   EL110
00725          ENDFILE (3010-END-FILE)                                  EL110
00726          NOTFND  (3010-END-FILE)                                  EL110
00727      END-EXEC.                                                    EL110
00728                                                                   EL110
00729      EXEC CICS READNEXT                                           EL110
00730          DATASET (PI-FILE-ID)                                        CL**5
00731          RIDFLD (BROWSE-KEY)                                      EL110
00732          INTO (ERROR-MESSAGE-FILE)                                   CL**5
00733      END-EXEC.                                                    EL110
00734 *                                                                    CL**5
00735 *    IF  PI-FRENCH-FILE                                              CL**5
00736 *        SERVICE RELOAD ERROR-MESSAGE-FILE-FR                        CL**5
00737 *                                                                    CL**5
00738 *    ELSE                                                            CL**5
00739 *        SERVICE RELOAD ERROR-MESSAGE-FILE-REST.                     CL**5
00740                                                                   EL110
00741      MOVE SPACES                 TO DISPLAY-LINE.                 EL110
00742      MOVE EM-CONTROL-PRIMARY     TO ERR-CD.                       EL110
00743      MOVE EM-ERROR-SEVERITY      TO ERR-SEV.                      EL110
00744      MOVE EM-ERROR-TEXT          TO ERR-TEXT.                     EL110
00745      ADD 1 TO COUNT-1.                                            EL110
00746      MOVE DISPLAY-LINE           TO ERRMSGO (COUNT-1).            EL110
00747                                                                   EL110
00748      IF COUNT-1 = 1                                               EL110
00749          MOVE ERR-CD             TO SAVE-BEGIN.                   EL110
00750                                                                   EL110
00751      IF COUNT-1 = 11                                              EL110
00752          MOVE ERR-CD             TO SAVE-ENDING                   EL110
00753          MOVE LIT-F              TO SCREEN-SWITCH.                EL110
00754                                                                   EL110
00755      GO TO 3020-EXIT.                                             EL110
00756                                                                   EL110
00757  3010-END-FILE.                                                   EL110
00758      PERFORM 3100-FILL-SCREEN THRU 3110-EXIT                      EL110
00759          UNTIL SCREEN-FULL.                                       EL110
00760      MOVE ERR-CD                 TO SAVE-ENDING.                  EL110
00761      MOVE END-MSG                TO MSGO.                         EL110
00762      MOVE LIT-E                  TO SCREEN-SWITCH.                EL110
00763                                                                   EL110
00764  3020-EXIT.                                                       EL110
00765      EXIT.                                                        EL110
00766      EJECT                                                        EL110
00767  3030-BUILD-BCK-PAGE.                                             EL110
00768      EXEC CICS HANDLE CONDITION                                   EL110
00769          ENDFILE (3040-END-FILE)                                  EL110
00770          NOTFND  (3040-END-FILE)                                  EL110
00771      END-EXEC.                                                    EL110
00772                                                                   EL110
00773      EXEC CICS READPREV                                           EL110
00774          DATASET (PI-FILE-ID)                                        CL**5
00775          RIDFLD  (BROWSE-KEY)                                     EL110
00776          INTO (ERROR-MESSAGE-FILE)                                   CL**5
00777      END-EXEC.                                                    EL110
00778 *                                                                    CL**5
00779 *    IF  PI-FRENCH-FILE                                              CL**5
00780 *        SERVICE RELOAD ERROR-MESSAGE-FILE-FR                        CL**5
00781 *                                                                    CL**5
00782 *    ELSE                                                            CL**5
00783 *        SERVICE RELOAD ERROR-MESSAGE-FILE-REST.                     CL**5
00784                                                                   EL110
00785      IF EM-MESSAGE-NUMBER GREATER SAVE-BEGIN                      EL110
00786          GO TO 3050-EXIT.                                         EL110
00787                                                                   EL110
00788      MOVE SPACES                 TO DISPLAY-LINE.                 EL110
00789      MOVE EM-CONTROL-PRIMARY     TO ERR-CD.                       EL110
00790      MOVE EM-ERROR-SEVERITY      TO ERR-SEV.                      EL110
00791      MOVE EM-ERROR-TEXT          TO ERR-TEXT.                     EL110
00792      MOVE DISPLAY-LINE           TO ERRMSGO (COUNT-1).            EL110
00793                                                                   EL110
00794      IF COUNT-1 = 11                                              EL110
00795          MOVE ERR-CD             TO SAVE-ENDING.                  EL110
00796                                                                   EL110
00797      IF COUNT-1 = 1                                               EL110
00798          MOVE ERR-CD             TO SAVE-BEGIN                    EL110
00799          MOVE LIT-F              TO SCREEN-SWITCH.                EL110
00800                                                                   EL110
00801      SUBTRACT 1 FROM COUNT-1.                                     EL110
00802      GO TO 3050-EXIT.                                             EL110
00803                                                                   EL110
00804  3040-END-FILE.                                                   EL110
00805      MOVE ERR-CD                 TO SAVE-BEGIN.                   EL110
00806      MOVE END-MSG                TO MSGO.                         EL110
00807      MOVE LIT-E                  TO SCREEN-SWITCH.                EL110
00808                                                                   EL110
00809  3050-EXIT.                                                       EL110
00810      EXIT.                                                        EL110
00811      EJECT                                                        EL110
00812  3060-RAISE-PAGE.                                                 EL110
00813      MOVE 1                      TO COUNT-1.                      EL110
00814      MOVE 2                      TO COUNT-2.                      EL110
00815      PERFORM 3080-SHIFT-SCREEN THRU 3090-EXIT                     EL110
00816           UNTIL COUNT-1 = 11.                                     EL110
00817      MOVE SPACES                 TO ERRMSGO (COUNT-1).            EL110
00818                                                                   EL110
00819  3070-EXIT.                                                       EL110
00820      EXIT.                                                        EL110
00821                                                                   EL110
00822  3080-SHIFT-SCREEN.                                               EL110
00823      MOVE ERRMSGO (COUNT-2)      TO ERRMSGO (COUNT-1).            EL110
00824      ADD 1 TO COUNT-1 COUNT-2.                                    EL110
00825                                                                   EL110
00826  3090-EXIT.                                                       EL110
00827      EXIT.                                                        EL110
00828                                                                   EL110
00829  3100-FILL-SCREEN.                                                EL110
00830      ADD 1 TO COUNT-1.                                            EL110
00831      MOVE SPACES                 TO ERRMSGO (COUNT-1).            EL110
00832                                                                   EL110
00833      IF COUNT-1 GREATER 10                                        EL110
00834          MOVE LIT-F TO SCREEN-SWITCH.                             EL110
00835                                                                   EL110
00836  3110-EXIT.                                                       EL110
00837      EXIT.                                                        EL110
00838      EJECT                                                        EL110
00839  8100-SEND-MAP.                                                   EL110
00840      PERFORM 8130-FORMAT-DATE-TIME THRU 8140-EXIT.                EL110
00841      MOVE LIT-IC                 TO MAINTL.                       EL110
00842      EXEC CICS SEND                                               EL110
00843          MAP    ('EL110A')                                        EL110
00844          MAPSET ('EL110S')                                        EL110
00845          ERASE                                                    EL110
00846          FREEKB                                                   EL110
00847          CURSOR                                                   EL110
00848      END-EXEC.                                                    EL110
00849                                                                   EL110
00850      GO TO 8120-RETURN-TRANS.                                     EL110
00851                                                                   EL110
00852  8110-SEND-DATA.                                                  EL110
00853      PERFORM 8130-FORMAT-DATE-TIME THRU 8140-EXIT.                EL110
00854      EXEC CICS SEND                                               EL110
00855          MAP    ('EL110A')                                        EL110
00856          MAPSET ('EL110S')                                        EL110
00857          DATAONLY                                                 EL110
00858          ERASEAUP                                                 EL110
00859          FREEKB                                                   EL110
00860          CURSOR                                                   EL110
00861      END-EXEC.                                                    EL110
00862                                                                   EL110
00863  8120-RETURN-TRANS.                                               EL110
00864      EXEC CICS RETURN                                             EL110
00865          TRANSID  (LIT-TRAN)                                      EL110
00866          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL110
00867          LENGTH   (PI-COMM-LENGTH)                                EL110
00868      END-EXEC.                                                    EL110
00869      GOBACK.                                                      EL110
00870                                                                   EL110
00871  8130-FORMAT-DATE-TIME.                                           EL110
00872      MOVE SAVE-DATE              TO DATEO.                        EL110
00873      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL**6
00874      END-EXEC                                                        CL**6
00875      EXEC CICS FORMATTIME                                            CL**6
00876                ABSTIME(LCP-CICS-TIME)                                CL**6
00877                TIME(LCP-TIME-OF-DAY-XX)                              CL**6
00878      END-EXEC                                                        CL**6
00879      MOVE  LCP-TIME-OF-DAY-68 TO TIME-UNFORMATTED.                   CL**6
00880      MOVE UN-HOURS               TO FOR-HOURS.                    EL110
00881      MOVE UN-MINUTES             TO FOR-MINUTES.                  EL110
00882      MOVE TIME-FORMATTED         TO TIMEO.                        EL110
00883      MOVE LIT-MAP                TO PI-CURRENT-SCREEN-NO.         EL110
00884                                                                   EL110
00885  8140-EXIT.                                                       EL110
00886      EXIT.                                                        EL110
00887                                                                   EL110
00888                                                                   EL110
00889  8220-RETURN-CICS.                                                EL110
00890      EXEC CICS RETURN                                             EL110
00891      END-EXEC.                                                    EL110
00892      GOBACK.                                                      EL110
00893                                                                   EL110
00894  8230-NOT-OPEN.                                                   EL110
00895      IF  PI-FRENCH-FILE                                              CL**5
00896          EXEC CICS SEND TEXT                                         CL**5
00897              FROM (FILE-MSG-FR)                                      CL**5
00898              LENGTH (FILE-LENGTH)                                    CL**5
00899              ERASE                                                   CL**5
00900              FREEKB                                                  CL**5
00901          END-EXEC                                                    CL**5
00902                                                                      CL**5
00903      ELSE                                                            CL**5
00904          EXEC CICS SEND TEXT                                         CL**5
00905              FROM (FILE-MSG)                                         CL**5
00906              LENGTH (FILE-LENGTH)                                    CL**5
00907              ERASE                                                   CL**5
00908              FREEKB                                                  CL**5
00909          END-EXEC.                                                   CL**5
00910                                                                      CL**5
00911      GO TO 8220-RETURN-CICS.                                      EL110
00912                                                                   EL110
00913  8300-GET-HELP.                                                   EL110
00914      MOVE LIT-HELP TO CALL-PGM.                                   EL110
00915      GO TO 9400-XCTL.                                             EL110
00916                                                                   EL110
00917      EJECT                                                        EL110
00918  8800-UNAUTHORIZED-ACCESS.                                        EL110
00919      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL110
00920      GO TO 8800-SEND-TEXT.                                        EL110
00921                                                                   EL110
00922  8800-SEND-TEXT.                                                  EL110
00923      EXEC CICS SEND TEXT                                          EL110
00924          FROM    (LOGOFF-TEXT)                                    EL110
00925          LENGTH  (LOGOFF-LENGTH)                                  EL110
00926          ERASE                                                    EL110
00927          FREEKB                                                   EL110
00928      END-EXEC.                                                    EL110
00929                                                                      CL**2
00930      GO TO 8220-RETURN-CICS.                                         CL**2
00931                                                                   EL110
00932  EJECT                                                            EL110
00933  9000-XCTL.                                                       EL110
00934      MOVE PI-RETURN-TO-PROGRAM   TO CALL-PGM.                     EL110
00935      GO TO 9400-XCTL.                                             EL110
00936                                                                   EL110
00937  9100-XCTL.                                                       EL110
00938      MOVE XCTL-005               TO CALL-PGM.                     EL110
00939      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL110
00940      GO TO 9400-XCTL.                                             EL110
00941                                                                   EL110
00942  9200-XCTL.                                                       EL110
00943                                                                      CL**3
00944      IF  CREDIT-SESSION                                              CL**3
00945          MOVE XCTL-EL626         TO CALL-PGM                         CL**3
00946                                                                      CL**3
00947      ELSE                                                            CL**3
00948          IF  CLAIM-SESSION                                           CL**3
00949              MOVE XCTL-EL126     TO CALL-PGM                         CL**3
00950                                                                      CL**3
00951          ELSE                                                        CL**3
00952              IF  MORTGAGE-SESSION                                    CL**3
00953                  MOVE XCTL-EM626 TO CALL-PGM                         CL**3
00954                                                                      CL**3
00955              ELSE                                                    CL**3
00956                  IF  GENERAL-LEDGER-SESSION                          CL**3
00957                      MOVE XCTL-GL800                                 CL**3
00958                                  TO CALL-PGM.                        CL**3
00959                                                                   EL110
00960      GO TO 9400-XCTL.                                             EL110
00961                                                                   EL110
00962  9400-XCTL.                                                       EL110
00963      EXEC CICS XCTL                                               EL110
00964          PROGRAM  (CALL-PGM)                                      EL110
00965          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL110
00966          LENGTH   (PI-COMM-LENGTH)                                EL110
00967      END-EXEC.                                                    EL110
00968                                                                   EL110
00969      GOBACK.                                                      EL110
00970                                                                   EL110
00971  9700-LINK-DATE-CONVERT.                                          EL110
00972      EXEC CICS LINK                                               EL110
00973          PROGRAM    ('ELDATCV')                                   EL110
00974          COMMAREA   (DATE-CONVERSION-DATA)                        EL110
00975          LENGTH     (DC-COMM-LENGTH)                              EL110
00976          END-EXEC.                                                EL110
00977  9700-EXIT.                                                       EL110
00978      EXIT.                                                        EL110
