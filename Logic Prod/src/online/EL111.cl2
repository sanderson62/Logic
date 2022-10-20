00001  IDENTIFICATION DIVISION.                                         02/26/96
00002                                                                   EL111
00003  PROGRAM-ID.                 EL111 .                                 LV004
00004 *              PROGRAM CONVERTED BY                                  CL**4
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**4
00006 *              CONVERSION DATE 02/12/96 09:47:58.                    CL**4
00007 *                            VMOD=2.004                              CL**4
00007 *                                                                    CL**4
00008 *AUTHOR.        LOGIC, INC.                                          CL**4
00009 *               DALLAS, TEXAS.                                       CL**4
00010 *DATE-COMPILED.                                                      CL**4
00011 *SECURITY.   *****************************************************   CL**4
00012 *            *                                                   *   CL**4
00013 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**4
00014 *            *                                                   *   CL**4
00015 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**4
00016 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**4
00017 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**4
00018 *            *                                                   *   CL**4
00019 *            *****************************************************   CL**4
00020 *REMARKS. TRANSACTION EX36 - PROGRAM FILE MAINTENANCE.               CL**4
00021    EJECT                                                          EL111
00022  ENVIRONMENT DIVISION.                                            EL111
00023  DATA DIVISION.                                                   EL111
00024  WORKING-STORAGE SECTION.                                         EL111
00025  01  LCP-TIME-OF-DAY-XX.                                             CL**4
00026      05  LCP-TIME-OF-DAY-68        PIC 9(6).                         CL**4
00027      05  FILLER                    PIC 99.                           CL**4
00028  01  LCP-CICS-TIME                 PIC 9(15).                        CL**4
00029                                                                   EL111
00030  77  FILLER  PIC X(32)  VALUE '********************************'. EL111
00031  77  FILLER  PIC X(32)  VALUE '*    EL111 WORKING STORAGE     *'. EL111
00032  77  FILLER  PIC X(32)  VALUE '*********** VMOD 2.004 *********'.    CL**4
00033                                                                   EL111
00034  01  WS-DATE-AREA.                                                EL111
00035      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL111
00036      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL111
00037                                                                   EL111
00038  01  LITERALS-NUMBERS.                                            EL111
00039      12  LIT-A                   PIC X       VALUE 'A'.           EL111
00040      12  LIT-B                   PIC X       VALUE 'B'.           EL111
00041      12  LIT-C                   PIC X       VALUE 'C'.           EL111
00042      12  LIT-D                   PIC X       VALUE 'D'.           EL111
00043      12  LIT-E                   PIC X       VALUE 'E'.           EL111
00044      12  LIT-S                   PIC X       VALUE 'S'.           EL111
00045      12  LIT-X                   PIC X       VALUE 'X'.           EL111
00046      12  LIT-SPACE               PIC X       VALUE SPACE.         EL111
00047      12  LIT-PO                  PIC XX      VALUE 'PO'.          EL111
00048      12  LIT-CLAIM               PIC XX      VALUE 'EL'.          EL111
00049      12  LIT-GL                  PIC XX      VALUE 'GL'.          EL111
00050      12  LIT-EM                  PIC XX      VALUE 'EM'.             CL**3
00051      12  LIT-EC                  PIC XX      VALUE 'EC'.          EL111
00052      12  LIT-FILE                PIC X(8)    VALUE 'ELPGMO'.      EL111
00053      12  LIT-FILE-2              PIC X(8)    VALUE 'ELPGMN'.      EL111
00054      12  LIT-PGM                 PIC X(8)    VALUE 'EL111'.       EL111
00055      12  LIT-SYS                 PIC X(4)    VALUE 'SYS'.         EL111
00056      12  LIT-TRANS               PIC X(4)    VALUE 'EX36'.        EL111
00057      12  LIT-HELP                PIC X(8)    VALUE 'EL010'.       EL111
00058      12  LIT-MAP                 PIC X(4)    VALUE '111A'.        EL111
00059      12  CALL-PGM                PIC X(8)    VALUE SPACES.        EL111
00060      12  XCTL-005                PIC X(8)    VALUE 'EL005'.       EL111
00061      12  XCTL-EL126              PIC X(8)    VALUE 'EL126'.          CL**2
00062      12  XCTL-EL626              PIC X(8)    VALUE 'EL626'.          CL**2
00063      12  XCTL-EM626              PIC X(8)    VALUE 'EM626'.          CL**2
00064      12  XCTL-GL800              PIC X(8)    VALUE 'GL800'.          CL**2
00065      12  COUNT-1                 PIC 9.                           EL111
00066      EJECT                                                        EL111
00067  01  EDIT-WORK-AREA.                                              EL111
00068      12  CHECK-MAINT             PIC X.                           EL111
00069          88  SHOW-OPTION                     VALUE 'S'.           EL111
00070          88  CHANGE-OPTION                   VALUE 'C'.           EL111
00071          88  DELETE-OPTION                   VALUE 'D'.           EL111
00072          88  VALID-OPTION                    VALUE 'C' 'D' 'S'.   EL111
00073      12  CHECK-TYPE              PIC X.                           EL111
00074          88  VALID-TYPE                      VALUE 'F' 'P' 'T'.   EL111
00075      12  CHECK-PFKEYS            PIC 99.                          EL111
00076      12  SCREEN-SWITCH           PIC X.                           EL111
00077          88  UPDATES-MADE                    VALUE 'A'.           EL111
00078          88  DELETE-CODE-PRESENT             VALUE 'D'.           EL111
00079          88  SCREEN-ERROR                    VALUE 'X'.           EL111
00080      12  UPDATE-SWITCH           PIC X.                           EL111
00081          88  NOTHING-CHANGED                 VALUE 'E'.           EL111
00082          88  UPDATE-COMPLETE                 VALUE 'D'.           EL111
00083      12  FIND-SWITCH             PIC X.                           EL111
00084          88  END-UPDATES                     VALUE 'E'.           EL111
00085          88  UPDATE-FOUND                    VALUE 'D'.           EL111
00086      12  BROWSE-KEY.                                              EL111
00087          16  KEY-GENERIC.                                         EL111
00088              20  PGM-NO.                                          EL111
00089                  24  PGM-PREF    PIC XX.                          EL111
00090                  24  PGM-NUM     PIC XXX.                         EL111
00091              20  OPT-TYPE        PIC X.                           EL111
00092          16  OPT-CODE            PIC X.                           EL111
00093      12  SAVE-KEY                PIC X(6).                        EL111
00094      12  HOLD-OPT                PIC 9.                           EL111
00095      12  NAME-KEY                PIC X(5).                        EL111
00096      12  RECORD-KEY.                                              EL111
00097          16  GENERIC-KEY         PIC X(6).                        EL111
00098          16  FILLER              PIC X.                           EL111
00099      12  DESC-HOLD.                                               EL111
00100          16  HOLD-DESC OCCURS 8 TIMES PIC X(40).                  EL111
00101                                                                   EL111
00102  01  TIME-UNFORMATTED.                                            EL111
00103      12  UN-HOURS                PIC XX.                          EL111
00104      12  UN-MINUTES              PIC XX.                          EL111
00105      12  FILLER                  PIC X(4).                        EL111
00106                                                                   EL111
00107  01  TIME-FORMATTED.                                              EL111
00108      12  FOR-HOURS               PIC XX.                          EL111
00109      12  FILLER                  PIC X       VALUE '.'.           EL111
00110      12  FOR-MINUTES             PIC XX.                          EL111
00111      EJECT                                                        EL111
00112  01  ERROR-MESSAGES.                                              EL111
00113      12  MAINT-MIS.                                               EL111
00114          16  FILLER              PIC X(39)                        EL111
00115              VALUE 'MAINTENANCE FUNCTION INVALID OR UNAUTHO'.     EL111
00116          16  FILLER              PIC X(18)                        EL111
00117              VALUE 'RIZED             '.                          EL111
00118                                                                   EL111
00119      12  PGM-MIS.                                                 EL111
00120          16  FILLER              PIC X(33)                        EL111
00121              VALUE 'PROGRAM NUMBER MISSING OR INVALID'.           EL111
00122          16  FILLER              PIC X(18)                        EL111
00123              VALUE ' - PLEASE RE-ENTER'.                          EL111
00124                                                                   EL111
00125      12  PGM-FILE-MIS.                                            EL111
00126          16  FILLER              PIC X(38)                        EL111
00127              VALUE 'PROGRAM NUMBER MUST BE DEFINED IN THE '.      EL111
00128          16  FILLER              PIC X(24)                        EL111
00129              VALUE 'PROGRAM DESCRIPTION FILE'.                    EL111
00130                                                                   EL111
00131      12  PGM-DUP                 PIC X(42)                        EL111
00132          VALUE 'PROGRAM NUMBER DUPLICATE - PLEASE RE-ENTER'.      EL111
00133                                                                   EL111
00134      12  OPT-MIS.                                                 EL111
00135          16  FILLER              PIC X(30)                        EL111
00136              VALUE 'OPTION TYPE MISSING OR INVALID'.              EL111
00137          16  FILLER              PIC X(18)                        EL111
00138              VALUE ' - PLEASE RE-ENTER'.                          EL111
00139                                                                   EL111
00140      12  DEL-OPT-MIS.                                             EL111
00141          16  FILLER              PIC X(35)                        EL111
00142              VALUE 'DELETE OPTION MUST BE ENTERED WITH '.         EL111
00143          16  FILLER              PIC X(22)                        EL111
00144              VALUE 'MAINTENANCE FUNCTION D'.                      EL111
00145                                                                   EL111
00146      12  DEL-OPT-INV             PIC X(35)                        EL111
00147              VALUE 'NOTHING TO DELETE FOR OPTIONS GIVEN'.         EL111
00148                                                                   EL111
00149      12  DEL-OPT-ERROR.                                           EL111
00150          16  FILLER              PIC X(39)                        EL111
00151              VALUE 'DELETE OPTION CAN ONLY BE ENTERED WITH'.      EL111
00152          16  FILLER              PIC X(22)                        EL111
00153              VALUE 'MAINTENANCE FUNCTION D'.                      EL111
00154                                                                   EL111
00155      12  INVALID-PFKEY           PIC X(45)                        EL111
00156          VALUE 'INVALID PF-OPTION REQUESTED - PLEASE RE-ENTER'.   EL111
00157                                                                   EL111
00158      12  END-UPDATE              PIC X(16)                        EL111
00159          VALUE 'UPDATE COMPLETED'.                                EL111
00160                                                                   EL111
00161      12  NO-UPDATES              PIC X(32)                        EL111
00162          VALUE 'NO UPDATES MADE - SCREEN IGNORED'.                EL111
00163                                                                   EL111
00164      12  SCREEN-TERM             PIC X(42)                        EL111
00165            VALUE '     CLAS-IC PROGRAM MAINTENANCE COMPLETED'.    EL111
00166                                                                   EL111
00167      12  FILE-MSG                PIC X(25)                        EL111
00168            VALUE '     FILE ELPGMO NOT OPEN'.                     EL111
00169                                                                   EL111
00170      12  NO-OPTIONS-PRESENT      PIC X(37)                        EL111
00171          VALUE 'NO OPTIONS AVAILABLE FOR THIS PROGRAM'.           EL111
00172      EJECT                                                        EL111
00173  01  COMP-LENGTHS.                                                EL111
00174      12  LIT-IC                  PIC S9(4)  COMP VALUE -1.        EL111
00175      12  FILE-LENGTH             PIC S9(4)  COMP VALUE +25.       EL111
00176      12  TERM-LENGTH             PIC S9(4)  COMP VALUE +42.       EL111
00177      12  JOURNAL-LENGTH          PIC S9(4)  COMP VALUE +72.       EL111
00178      12  SHOW-LENGTH             PIC S9(4)  COMP VALUE +6.        EL111
00179      12  PGMO-LENGTH             PIC S9(4)  COMP VALUE +49.       EL111
00180                                                                   EL111
00181      EJECT                                                        EL111
00182      COPY ELCDATE.                                                   CL**3
00183      EJECT                                                        EL111
00184      COPY ELCLOGOF.                                                  CL**3
00185      EJECT                                                        EL111
00186      COPY ELCATTR.                                                   CL**3
00187      EJECT                                                        EL111
00188      COPY ELCAID.                                                    CL**3
00189  01  FILLER REDEFINES DFHAID.                                     EL111
00190      12  FILLER                  PIC X(8).                        EL111
00191      12  AID-KEYS OCCURS 24 TIMES.                                EL111
00192          16  FILLER              PIC X.                           EL111
00193      EJECT                                                        EL111
00194      COPY ELCJPFX.                                                   CL**3
00195                                  PIC X(49).                       EL111
00196      EJECT                                                        EL111
00197      COPY ELCINTF.                                                   CL**3
00198      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                   EL111
00199          16  MENU-SWITCH         PIC X.                           EL111
00200          16  SHOW-SWITCH         PIC X.                           EL111
00201              88   NOT-SHOWN                 VALUE 'X'.            EL111
00202          16  FILLER              PIC X(638).                      EL111
00203      EJECT                                                        EL111
00204      COPY EL111S.                                                    CL**3
00205      EJECT                                                        EL111
00206  LINKAGE SECTION.                                                 EL111
00207  01  DFHCOMMAREA                 PIC X(1024).                     EL111
00208                                                                   EL111
00209 *01 PARM-LIST .                                                      CL**4
00210 *    12  FILLER                  PIC S9(8)  COMP.                    CL**4
00211 *    12  PGMO-PNT                PIC S9(8)  COMP.                    CL**4
00212 *    12  PGMN-PNT                PIC S9(8)  COMP.                    CL**4
00213      EJECT                                                        EL111
00214      COPY ELCPGMO.                                                   CL**3
00215      EJECT                                                        EL111
00216      COPY ELCPGMN.                                                   CL**3
00217      EJECT                                                        EL111
00218  PROCEDURE DIVISION.                                              EL111
00219                                                                   EL111
00220      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL111
00221                                                                   EL111
00222      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL111
00223      MOVE '5'                    TO DC-OPTION-CODE.               EL111
00224      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL111
00225      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL111
00226      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL111
00227                                                                   EL111
00228      IF EIBCALEN = ZERO                                           EL111
00229          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL111
00230                                                                   EL111
00231      IF PI-CALLING-PROGRAM NOT = LIT-PGM                          EL111
00232          IF PI-RETURN-TO-PROGRAM NOT = LIT-PGM                    EL111
00233              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6    EL111
00234              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5    EL111
00235              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4    EL111
00236              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3    EL111
00237              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2    EL111
00238              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1    EL111
00239              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM  EL111
00240              MOVE LIT-PGM               TO  PI-CALLING-PROGRAM    EL111
00241              MOVE LOW-VALUES            TO  EL111AO               EL111
00242              GO TO 8100-SEND-MAP                                  EL111
00243          ELSE                                                     EL111
00244              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM    EL111
00245              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM  EL111
00246              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1    EL111
00247              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2    EL111
00248              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3    EL111
00249              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4    EL111
00250              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5    EL111
00251              MOVE SPACES                TO  PI-SAVED-PROGRAM-6    EL111
00252              MOVE LOW-VALUES            TO  EL111AO               EL111
00253              GO TO 8100-SEND-MAP.                                 EL111
00254                                                                   EL111
00255                                                                   EL111
00256      IF EIBAID = DFHCLEAR                                         EL111
00257              GO TO 9000-XCTL.                                     EL111
00258                                                                   EL111
00259      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL111
00260          MOVE LOW-VALUES         TO EL111AO                       EL111
00261          MOVE LIT-IC             TO PFKEYL                        EL111
00262          MOVE INVALID-PFKEY      TO MSGO                          EL111
00263          GO TO 8110-SEND-DATA.                                    EL111
00264                                                                   EL111
00265      EXEC CICS RECEIVE                                            EL111
00266          MAP    ('EL111A')                                        EL111
00267          MAPSET ('EL111S')                                        EL111
00268      END-EXEC.                                                    EL111
00269                                                                   EL111
00270      MOVE SPACES                 TO SCREEN-SWITCH MSGO.           EL111
00271      PERFORM 0200-SET-ATTRB THRU 0210-EXIT.                       EL111
00272                                                                   EL111
00273      IF PFKEYL GREATER ZERO                                       EL111
00274          PERFORM 0100-TRANS-PF THRU 0110-EXIT.                    EL111
00275                                                                   EL111
00276      IF SCREEN-ERROR                                              EL111
00277          MOVE LIT-IC             TO PFKEYL                        EL111
00278          GO TO 8110-SEND-DATA.                                    EL111
00279                                                                   EL111
00280      IF EIBAID = DFHPF23                                          EL111
00281          GO TO 9100-XCTL.                                         EL111
00282                                                                   EL111
00283      IF EIBAID = DFHPF24                                          EL111
00284          GO TO 9200-XCTL.                                         EL111
00285                                                                   EL111
00286      IF EIBAID = DFHPF12                                          EL111
00287          GO TO 8300-GET-HELP.                                     EL111
00288                                                                   EL111
00289      IF EIBAID NOT = DFHENTER                                     EL111
00290          MOVE INVALID-PFKEY      TO MSGO                          EL111
00291          MOVE LIT-IC             TO MAINTL                        EL111
00292          GO TO 8110-SEND-DATA.                                    EL111
00293                                                                   EL111
00294      MOVE SPACE                  TO SCREEN-SWITCH.                EL111
00295      PERFORM 1000-EDIT-SCREEN THRU 1010-EXIT.                     EL111
00296                                                                   EL111
00297      IF SCREEN-ERROR                                              EL111
00298          GO TO 8110-SEND-DATA.                                    EL111
00299                                                                   EL111
00300      PERFORM 1100-SAVE-DESC   THRU 1110-EXIT.                     EL111
00301      PERFORM 1200-UPDATE-FILE THRU 1210-EXIT.                     EL111
00302                                                                   EL111
00303      IF SCREEN-ERROR                                              EL111
00304          GO TO 8110-SEND-DATA.                                    EL111
00305                                                                   EL111
00306      MOVE LIT-IC                 TO MAINTL.                       EL111
00307      MOVE SPACE                  TO MAINTO.                       EL111
00308      MOVE SPACE                  TO DELOPTO.                      EL111
00309                                                                   EL111
00310      IF NOT SHOW-OPTION                                           EL111
00311          MOVE END-UPDATE         TO MSGO.                         EL111
00312                                                                   EL111
00313      PERFORM 0300-SET-DESC-ATTRB THRU 0310-EXIT.                  EL111
00314                                                                   EL111
00315      GO TO 8110-SEND-DATA.                                        EL111
00316      EJECT                                                        EL111
00317  0100-TRANS-PF.                                                   EL111
00318      IF EIBAID NOT = DFHENTER                                     EL111
00319          MOVE INVALID-PFKEY      TO MSGO                          EL111
00320          MOVE LIT-X              TO SCREEN-SWITCH                 EL111
00321          GO TO 0110-EXIT.                                         EL111
00322                                                                   EL111
00323      IF PFKEYI NOT NUMERIC                                        EL111
00324          MOVE INVALID-PFKEY      TO MSGO                          EL111
00325          MOVE LIT-X              TO SCREEN-SWITCH                 EL111
00326          GO TO 0110-EXIT.                                         EL111
00327                                                                   EL111
00328      IF PFKEYI LESS 1 OR GREATER 24                               EL111
00329          MOVE INVALID-PFKEY      TO MSGO                          EL111
00330          MOVE LIT-X              TO SCREEN-SWITCH                 EL111
00331          GO TO 0110-EXIT.                                         EL111
00332                                                                   EL111
00333      MOVE PFKEYI                  TO CHECK-PFKEYS.                EL111
00334      MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.                      EL111
00335                                                                   EL111
00336  0110-EXIT.                                                       EL111
00337      EXIT.                                                        EL111
00338                                                                   EL111
00339  0200-SET-ATTRB.                                                  EL111
00340      MOVE AL-UANON               TO MAINTA                        EL111
00341                                     OPTTYPEA                      EL111
00342                                     DELOPTA                       EL111
00343                                     PGRMA.                        EL111
00344                                                                   EL111
00345  0210-EXIT.                                                       EL111
00346      EXIT.                                                        EL111
00347                                                                   EL111
00348  0300-SET-DESC-ATTRB.                                             EL111
00349      MOVE AL-UANOF               TO DESC1A                        EL111
00350                                     DESC2A                        EL111
00351                                     DESC3A                        EL111
00352                                     DESC4A                        EL111
00353                                     DESC5A                        EL111
00354                                     DESC6A                        EL111
00355                                     DESC7A                        EL111
00356                                     DESC8A.                       EL111
00357                                                                   EL111
00358  0310-EXIT.                                                       EL111
00359      EXIT.                                                        EL111
00360      EJECT                                                        EL111
00361  1000-EDIT-SCREEN.                                                EL111
00362      MOVE MAINTI                 TO CHECK-MAINT.                  EL111
00363                                                                   EL111
00364      IF (NOT VALID-OPTION) OR                                     EL111
CIDMOD        (PI-PROCESSOR-ID NOT = 'LGXX' AND 'JJPA' AND 'PEMA'
CIDMOD                         AND NOT SHOW-OPTION)                     EL111
00365 **      (PI-PROCESSOR-ID NOT = 'LGXX' AND NOT SHOW-OPTION)        EL111
00366          MOVE AL-UABON           TO MAINTA                        EL111
00367          MOVE LIT-IC             TO MAINTL                        EL111
00368          MOVE MAINT-MIS          TO MSGO                          EL111
00369          MOVE LIT-X              TO SCREEN-SWITCH                 EL111
00370          GO TO 1010-EXIT.                                         EL111
00371                                                                   EL111
00372      IF PGRMI = SPACES OR LOW-VALUES                              EL111
00373          MOVE LIT-IC             TO PGRML                         EL111
00374          MOVE AL-UABON           TO PGRMA                         EL111
00375          MOVE PGM-MIS            TO MSGO                          EL111
00376          MOVE LIT-X              TO SCREEN-SWITCH                 EL111
00377          GO TO 1010-EXIT.                                         EL111
00378                                                                   EL111
00379      MOVE PGRMI                  TO PGM-NO.                       EL111
00380      IF PGM-PREF NOT = LIT-CLAIM AND LIT-EC AND LIT-GL AND LIT-EM    CL**3
00381          MOVE LIT-IC             TO PGRML                         EL111
00382          MOVE AL-UABON           TO PGRMA                         EL111
00383          MOVE PGM-MIS            TO MSGO                          EL111
00384          MOVE LIT-X              TO SCREEN-SWITCH                 EL111
00385          GO TO 1010-EXIT.                                         EL111
00386                                                                   EL111
00387      IF CHANGE-OPTION                                             EL111
00388          PERFORM 1050-VERIFY-PGM THRU 1070-EXIT.                  EL111
00389                                                                   EL111
00390      IF SCREEN-ERROR                                              EL111
00391          MOVE LIT-IC             TO PGRML                         EL111
00392          MOVE AL-UABON           TO PGRMA                         EL111
00393          MOVE PGM-FILE-MIS       TO MSGO                          EL111
00394          GO TO 1010-EXIT.                                         EL111
00395                                                                   EL111
00396      MOVE OPTTYPEI TO CHECK-TYPE.                                 EL111
00397                                                                   EL111
00398      IF NOT VALID-TYPE                                            EL111
00399          MOVE AL-UABON           TO OPTTYPEA                      EL111
00400          MOVE LIT-IC             TO OPTTYPEL                      EL111
00401          MOVE OPT-MIS            TO MSGO                          EL111
00402          MOVE LIT-X              TO SCREEN-SWITCH                 EL111
00403          GO TO 1010-EXIT.                                         EL111
00404                                                                   EL111
00405      PERFORM 1020-VERIFY-DEL-OPT THRU 1040-EXIT.                  EL111
00406                                                                   EL111
00407  1010-EXIT.                                                       EL111
00408      EXIT.                                                        EL111
00409                                                                   EL111
00410  1020-VERIFY-DEL-OPT.                                             EL111
00411      IF DELOPTI = SPACES  AND                                     EL111
00412         NOT DELETE-OPTION                                         EL111
00413          GO TO 1040-EXIT.                                         EL111
00414                                                                   EL111
00415      IF DELOPTL = ZEROS  AND                                      EL111
00416         NOT DELETE-OPTION                                         EL111
00417          GO TO 1040-EXIT.                                         EL111
00418                                                                   EL111
00419      MOVE LIT-D                  TO SCREEN-SWITCH.                EL111
00420                                                                   EL111
00421      IF DELOPTI = LIT-A                                           EL111
00422          GO TO 1040-EXIT.                                         EL111
00423                                                                   EL111
00424      IF DELOPTI LESS '1'                                          EL111
00425          GO TO 1030-FORMAT-ERR.                                   EL111
00426                                                                   EL111
00427      IF DELOPTI GREATER '8'                                       EL111
00428          GO TO 1030-FORMAT-ERR.                                   EL111
00429                                                                   EL111
00430      IF DELETE-CODE-PRESENT AND DELETE-OPTION                     EL111
00431          GO TO 1040-EXIT.                                         EL111
00432                                                                   EL111
00433      IF DELETE-OPTION AND NOT DELETE-CODE-PRESENT                 EL111
00434          MOVE LIT-IC             TO DELOPTL                       EL111
00435          MOVE AL-UABON           TO DELOPTA                       EL111
00436          MOVE DEL-OPT-MIS        TO MSGO                          EL111
00437          MOVE LIT-X              TO SCREEN-SWITCH                 EL111
00438          GO TO 1040-EXIT.                                         EL111
00439                                                                   EL111
00440      IF DELETE-CODE-PRESENT AND NOT DELETE-OPTION                 EL111
00441          MOVE LIT-IC             TO DELOPTL                       EL111
00442          MOVE AL-UABON           TO DELOPTA                       EL111
00443          MOVE DEL-OPT-ERROR      TO MSGO                          EL111
00444          MOVE LIT-X              TO SCREEN-SWITCH.                EL111
00445                                                                   EL111
00446      GO TO 1040-EXIT.                                             EL111
00447                                                                   EL111
00448  1030-FORMAT-ERR.                                                 EL111
00449      MOVE LIT-IC                 TO DELOPTL.                      EL111
00450      MOVE AL-UABON               TO DELOPTA.                      EL111
00451      MOVE DEL-OPT-INV            TO MSGO.                         EL111
00452      MOVE LIT-X                  TO SCREEN-SWITCH.                EL111
00453                                                                   EL111
00454  1040-EXIT.                                                       EL111
00455      EXIT.                                                        EL111
00456                                                                   EL111
00457  1050-VERIFY-PGM.                                                 EL111
00458      MOVE PGRMI                  TO NAME-KEY.                     EL111
00459      EXEC CICS HANDLE CONDITION                                   EL111
00460          NOTFND (1060-PGM-NOTFND)                                 EL111
00461      END-EXEC.                                                    EL111
00462                                                                   EL111
00463      PERFORM 1680-READ-PGMN THRU 1690-EXIT.                       EL111
00464      GO TO 1070-EXIT.                                             EL111
00465                                                                   EL111
00466  1060-PGM-NOTFND.                                                 EL111
00467      MOVE LIT-X                  TO SCREEN-SWITCH.                EL111
00468                                                                   EL111
00469  1070-EXIT.                                                       EL111
00470      EXIT.                                                        EL111
00471      EJECT                                                        EL111
00472  1100-SAVE-DESC.                                                  EL111
00473      IF DESC1I GREATER LOW-VALUES                                 EL111
00474          MOVE DESC1I             TO HOLD-DESC (1)                 EL111
00475      ELSE                                                         EL111
00476          MOVE SPACES             TO HOLD-DESC (1).                EL111
00477                                                                   EL111
00478      IF DESC2I GREATER LOW-VALUES                                 EL111
00479          MOVE DESC2I             TO HOLD-DESC (2)                 EL111
00480      ELSE                                                         EL111
00481          MOVE SPACES             TO HOLD-DESC (2).                EL111
00482                                                                   EL111
00483      IF DESC3I GREATER LOW-VALUES                                 EL111
00484          MOVE DESC3I             TO HOLD-DESC (3)                 EL111
00485      ELSE                                                         EL111
00486          MOVE SPACES             TO HOLD-DESC (3).                EL111
00487                                                                   EL111
00488      IF DESC4I GREATER LOW-VALUES                                 EL111
00489          MOVE DESC4I             TO HOLD-DESC (4)                 EL111
00490      ELSE                                                         EL111
00491          MOVE SPACES             TO HOLD-DESC (4).                EL111
00492                                                                   EL111
00493      IF DESC5I GREATER LOW-VALUES                                 EL111
00494          MOVE DESC5I             TO HOLD-DESC (5)                 EL111
00495      ELSE                                                         EL111
00496          MOVE SPACES             TO HOLD-DESC (5).                EL111
00497                                                                   EL111
00498      IF DESC6I GREATER LOW-VALUES                                 EL111
00499          MOVE DESC6I             TO HOLD-DESC (6)                 EL111
00500      ELSE                                                         EL111
00501          MOVE SPACES             TO HOLD-DESC (6).                EL111
00502                                                                   EL111
00503      IF DESC7I GREATER LOW-VALUES                                 EL111
00504          MOVE DESC7I             TO HOLD-DESC (7)                 EL111
00505      ELSE                                                         EL111
00506          MOVE SPACES             TO HOLD-DESC (7).                EL111
00507                                                                   EL111
00508      IF DESC8I GREATER LOW-VALUES                                 EL111
00509          MOVE DESC8I             TO HOLD-DESC (8)                 EL111
00510      ELSE                                                         EL111
00511          MOVE SPACES             TO HOLD-DESC (8).                EL111
00512                                                                   EL111
00513  1110-EXIT.                                                       EL111
00514      EXIT.                                                        EL111
00515      EJECT                                                        EL111
00516  1200-UPDATE-FILE.                                                EL111
00517      MOVE ZEROS                  TO COUNT-1.                      EL111
00518      MOVE SPACES                 TO SCREEN-SWITCH UPDATE-SWITCH.  EL111
00519                                                                   EL111
00520      IF DELETE-OPTION                                             EL111
00521          PERFORM 1300-DELETE-OPTION THRU 1310-EXIT.               EL111
00522                                                                   EL111
00523      IF CHANGE-OPTION                                             EL111
00524          PERFORM 1400-CHANGE-OPTION THRU 1410-EXIT                EL111
00525                  UNTIL UPDATE-COMPLETE OR NOTHING-CHANGED.        EL111
00526                                                                   EL111
00527      IF UPDATE-COMPLETE                                           EL111
00528          MOVE ZEROES             TO COUNT-1                       EL111
00529          MOVE SPACES             TO SCREEN-SWITCH UPDATE-SWITCH   EL111
00530          MOVE LIT-S              TO CHECK-MAINT.                  EL111
00531                                                                   EL111
00532      IF SHOW-OPTION                                               EL111
00533          PERFORM 1500-SHOW-OPTION THRU 1540-EXIT.                 EL111
00534                                                                   EL111
00535  1210-EXIT.                                                       EL111
00536      EXIT.                                                        EL111
00537      EJECT                                                        EL111
00538  1300-DELETE-OPTION.                                              EL111
00539      MOVE PGRMI                  TO PGM-NO.                       EL111
00540      MOVE OPTTYPEI               TO OPT-TYPE.                     EL111
00541                                                                   EL111
00542      IF DELOPTI = LIT-A                                           EL111
00543          MOVE ZEROS              TO COUNT-1                       EL111
00544          PERFORM 1320-DELETE-ALL THRU 1330-EXIT 9 TIMES           EL111
00545          GO TO 1310-EXIT.                                         EL111
00546                                                                   EL111
00547      MOVE DELOPTI                TO OPT-CODE.                     EL111
00548      PERFORM 1340-DELETE-ONE THRU 1360-EXIT.                      EL111
00549                                                                   EL111
00550  1310-EXIT.                                                       EL111
00551      EXIT.                                                        EL111
00552                                                                   EL111
00553  1320-DELETE-ALL.                                                 EL111
00554      EXEC CICS HANDLE CONDITION                                   EL111
00555          NOTFND (1330-EXIT)                                       EL111
00556      END-EXEC.                                                    EL111
00557                                                                   EL111
00558      ADD 1 TO COUNT-1.                                            EL111
00559      MOVE COUNT-1 TO OPT-CODE.                                    EL111
00560      PERFORM 1600-READ-UPDATE THRU 1610-EXIT.                     EL111
00561      MOVE LIT-D                     TO JP-RECORD-TYPE.            EL111
00562      MOVE PROGRAM-OPTIONS-AVAILABLE TO JP-RECORD-AREA.            EL111
00563      PERFORM 1620-DELETE THRU 1630-EXIT.                          EL111
00564      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.                   EL111
00565                                                                   EL111
00566  1330-EXIT.                                                       EL111
00567      EXIT.                                                        EL111
00568                                                                   EL111
00569  1340-DELETE-ONE.                                                 EL111
00570      EXEC CICS HANDLE CONDITION                                   EL111
00571          NOTFND (1350-DELETE-NOTFND)                              EL111
00572      END-EXEC.                                                    EL111
00573                                                                   EL111
00574      PERFORM 1600-READ-UPDATE THRU 1610-EXIT.                     EL111
00575      MOVE LIT-D                  TO JP-RECORD-TYPE.               EL111
00576      MOVE PROGRAM-OPTIONS-AVAILABLE TO JP-RECORD-AREA.            EL111
00577      PERFORM 1620-DELETE THRU 1630-EXIT.                          EL111
00578      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.                   EL111
00579      GO TO 1360-EXIT.                                             EL111
00580                                                                   EL111
00581  1350-DELETE-NOTFND.                                              EL111
00582      MOVE LIT-IC                 TO DELOPTL.                      EL111
00583      MOVE AL-UABON               TO DELOPTA.                      EL111
00584      MOVE DEL-OPT-INV            TO MSGO.                         EL111
00585      MOVE LIT-X                  TO SCREEN-SWITCH.                EL111
00586                                                                   EL111
00587  1360-EXIT.                                                       EL111
00588      EXIT.                                                        EL111
00589      EJECT                                                        EL111
00590  1400-CHANGE-OPTION.                                              EL111
00591      MOVE PGRMI                  TO PGM-NO.                       EL111
00592      MOVE OPTTYPEI               TO OPT-TYPE.                     EL111
00593      MOVE SPACES                 TO FIND-SWITCH.                  EL111
00594      PERFORM 1430-FIND-UPDATE THRU 1440-EXIT                      EL111
00595          UNTIL END-UPDATES OR UPDATE-FOUND.                       EL111
00596                                                                   EL111
00597      IF END-UPDATES                                               EL111
00598          PERFORM 1450-CHECK-UPDATE THRU 1460-EXIT                 EL111
00599          GO TO 1410-EXIT.                                         EL111
00600                                                                   EL111
00601      MOVE LIT-A                  TO SCREEN-SWITCH.                EL111
00602      MOVE COUNT-1                TO OPT-CODE.                     EL111
00603                                                                   EL111
00604      EXEC CICS HANDLE CONDITION                                   EL111
00605          NOTFND (1410-ADD-OPTION)                                 EL111
00606      END-EXEC.                                                    EL111
00607                                                                   EL111
00608      PERFORM 1600-READ-UPDATE THRU 1610-EXIT.                     EL111
00609      MOVE LIT-B                  TO JP-RECORD-TYPE.               EL111
00610      MOVE PROGRAM-OPTIONS-AVAILABLE TO JP-RECORD-AREA.            EL111
00611      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.                   EL111
00612      MOVE HOLD-DESC (COUNT-1)    TO PO-OPTION-DESCRIPTION.        EL111
00613      MOVE LIT-C                  TO JP-RECORD-TYPE.               EL111
00614      MOVE PROGRAM-OPTIONS-AVAILABLE TO JP-RECORD-AREA.            EL111
00615      PERFORM 1640-REWRITE THRU 1650-EXIT.                         EL111
00616      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.                   EL111
00617      GO TO 1410-EXIT.                                             EL111
00618                                                                   EL111
00619  1410-ADD-OPTION.                                                 EL111
00620      EXEC CICS GETMAIN                                            EL111
00621          SET     (ADDRESS OF PROGRAM-OPTIONS-AVAILABLE)              CL**4
00622          LENGTH  (PGMO-LENGTH)                                    EL111
00623          INITIMG (LIT-SPACE)                                      EL111
00624      END-EXEC.                                                    EL111
00625                                                                   EL111
00626      MOVE BROWSE-KEY                TO PO-CONTROL-PRIMARY.        EL111
00627      MOVE HOLD-DESC (COUNT-1)       TO PO-OPTION-DESCRIPTION.     EL111
00628      MOVE LIT-PO                    TO PO-RECORD-ID.              EL111
00629      MOVE LIT-A                     TO JP-RECORD-TYPE.            EL111
00630      MOVE PROGRAM-OPTIONS-AVAILABLE TO JP-RECORD-AREA.            EL111
00631                                                                   EL111
00632      PERFORM 1660-WRITE THRU 1670-EXIT.                           EL111
00633      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.                   EL111
00634                                                                   EL111
00635  1410-EXIT.                                                       EL111
00636      EXIT.                                                        EL111
00637                                                                   EL111
00638  1430-FIND-UPDATE.                                                EL111
00639      ADD 1 TO COUNT-1.                                            EL111
00640                                                                   EL111
00641      IF COUNT-1 = 9                                               EL111
00642          MOVE LIT-E              TO FIND-SWITCH                   EL111
00643          GO TO 1440-EXIT.                                         EL111
00644                                                                   EL111
00645      IF HOLD-DESC (COUNT-1) = SPACES                              EL111
00646          GO TO 1440-EXIT.                                         EL111
00647                                                                   EL111
00648      MOVE LIT-D                  TO FIND-SWITCH.                  EL111
00649                                                                   EL111
00650  1440-EXIT.                                                       EL111
00651      EXIT.                                                        EL111
00652                                                                   EL111
00653  1450-CHECK-UPDATE.                                               EL111
00654      IF UPDATES-MADE                                              EL111
00655          MOVE LIT-D              TO UPDATE-SWITCH                 EL111
00656          GO TO 1460-EXIT.                                         EL111
00657                                                                   EL111
00658      MOVE LIT-E                  TO UPDATE-SWITCH.                EL111
00659      MOVE NO-UPDATES             TO MSGO.                         EL111
00660      MOVE LIT-IC                 TO MAINTL.                       EL111
00661      MOVE LIT-X                  TO SCREEN-SWITCH.                EL111
00662                                                                   EL111
00663  1460-EXIT.                                                       EL111
00664      EXIT.                                                        EL111
00665      EJECT                                                        EL111
00666  1500-SHOW-OPTION.                                                EL111
00667      EXEC CICS HANDLE CONDITION                                   EL111
00668          NOTFND (1530-SHOW-BUILD)                                 EL111
00669          ENDFILE (1520-END-BROWSE)                                EL111
00670      END-EXEC.                                                    EL111
00671                                                                   EL111
00672      MOVE PGRMI                  TO PGM-NO.                       EL111
00673      MOVE OPTTYPEI               TO OPT-TYPE.                     EL111
00674      MOVE SPACES                 TO OPT-CODE DESC-HOLD.           EL111
00675      MOVE BROWSE-KEY             TO SAVE-KEY.                     EL111
00676      PERFORM 1700-START-BROWSE THRU 1710-EXIT.                    EL111
00677      PERFORM 1720-RESET-BROWSE THRU 1730-EXIT.                    EL111
00678                                                                   EL111
00679  1510-BROWSE-CONT.                                                EL111
00680      PERFORM 1740-READ-NEXT THRU 1750-EXIT.                       EL111
00681      MOVE PO-CONTROL-PRIMARY     TO RECORD-KEY.                   EL111
00682                                                                   EL111
00683      IF GENERIC-KEY NOT = SAVE-KEY                                EL111
00684          GO TO 1520-END-BROWSE.                                   EL111
00685      ADD 1 TO COUNT-1.                                            EL111
00686                                                                   EL111
00687      MOVE OPT-CODE               TO HOLD-OPT.                     EL111
00688      MOVE PO-OPTION-DESCRIPTION  TO HOLD-DESC (HOLD-OPT).         EL111
00689      GO TO 1510-BROWSE-CONT.                                      EL111
00690                                                                   EL111
00691  1520-END-BROWSE.                                                 EL111
00692      PERFORM 1760-END-BROWSE THRU 1770-EXIT.                      EL111
00693                                                                   EL111
00694  1530-SHOW-BUILD.                                                 EL111
00695      IF DESC-HOLD = SPACES                                        EL111
00696          MOVE NO-OPTIONS-PRESENT TO MSGO                          EL111
00697          GO TO 1540-EXIT.                                         EL111
00698                                                                   EL111
00699      MOVE HOLD-DESC (1)          TO DESC1O.                       EL111
00700      MOVE HOLD-DESC (2)          TO DESC2O.                       EL111
00701      MOVE HOLD-DESC (3)          TO DESC3O.                       EL111
00702      MOVE HOLD-DESC (4)          TO DESC4O.                       EL111
00703      MOVE HOLD-DESC (5)          TO DESC5O.                       EL111
00704      MOVE HOLD-DESC (6)          TO DESC6O.                       EL111
00705      MOVE HOLD-DESC (7)          TO DESC7O.                       EL111
00706      MOVE HOLD-DESC (8)          TO DESC8O.                       EL111
00707                                                                   EL111
00708  1540-EXIT.                                                       EL111
00709      EXIT.                                                        EL111
00710      EJECT                                                        EL111
00711  1600-READ-UPDATE.                                                EL111
00712      EXEC CICS READ                                               EL111
00713          SET (ADDRESS OF PROGRAM-OPTIONS-AVAILABLE)                  CL**4
00714          DATASET (LIT-FILE)                                       EL111
00715          RIDFLD (BROWSE-KEY)                                      EL111
00716          UPDATE                                                   EL111
00717      END-EXEC.                                                    EL111
00718                                                                   EL111
00719  1610-EXIT.                                                       EL111
00720      EXIT.                                                        EL111
00721                                                                   EL111
00722  1620-DELETE.                                                     EL111
00723      EXEC CICS DELETE                                             EL111
00724          DATASET (LIT-FILE)                                       EL111
00725      END-EXEC.                                                    EL111
00726                                                                   EL111
00727  1630-EXIT.                                                       EL111
00728      EXIT.                                                        EL111
00729                                                                   EL111
00730  1640-REWRITE.                                                    EL111
00731      EXEC CICS REWRITE                                            EL111
00732          FROM (PROGRAM-OPTIONS-AVAILABLE)                         EL111
00733          DATASET (LIT-FILE)                                       EL111
00734      END-EXEC.                                                    EL111
00735                                                                   EL111
00736  1650-EXIT.                                                       EL111
00737      EXIT.                                                        EL111
00738                                                                   EL111
00739  1660-WRITE.                                                      EL111
00740      EXEC CICS WRITE                                              EL111
00741          FROM (PROGRAM-OPTIONS-AVAILABLE)                         EL111
00742          DATASET (LIT-FILE)                                       EL111
00743          RIDFLD (BROWSE-KEY)                                      EL111
00744      END-EXEC.                                                    EL111
00745                                                                   EL111
00746  1670-EXIT.                                                       EL111
00747      EXIT.                                                        EL111
00748                                                                   EL111
00749  1680-READ-PGMN.                                                  EL111
00750      EXEC CICS READ                                               EL111
00751          SET     (ADDRESS OF PROGRAM-DESCRIPTIONS)                   CL**4
00752          DATASET (LIT-FILE-2)                                     EL111
00753          RIDFLD  (NAME-KEY)                                       EL111
00754      END-EXEC.                                                    EL111
00755                                                                   EL111
00756  1690-EXIT.                                                       EL111
00757      EXIT.                                                        EL111
00758      EJECT                                                        EL111
00759  1700-START-BROWSE.                                               EL111
00760      EXEC CICS STARTBR                                            EL111
00761          DATASET   (LIT-FILE)                                     EL111
00762          RIDFLD    (BROWSE-KEY)                                   EL111
00763          KEYLENGTH (SHOW-LENGTH)                                  EL111
00764          GENERIC                                                  EL111
00765          EQUAL                                                    EL111
00766      END-EXEC.                                                    EL111
00767                                                                   EL111
00768  1710-EXIT.                                                       EL111
00769      EXIT.                                                        EL111
00770                                                                   EL111
00771  1720-RESET-BROWSE.                                               EL111
00772      EXEC CICS RESETBR                                            EL111
00773          DATASET (LIT-FILE)                                       EL111
00774          RIDFLD  (BROWSE-KEY)                                     EL111
00775      END-EXEC.                                                    EL111
00776                                                                   EL111
00777  1730-EXIT.                                                       EL111
00778      EXIT.                                                        EL111
00779                                                                   EL111
00780  1740-READ-NEXT.                                                  EL111
00781      EXEC CICS READNEXT                                           EL111
00782          DATASET (LIT-FILE)                                       EL111
00783          RIDFLD  (BROWSE-KEY)                                     EL111
00784          SET     (ADDRESS OF PROGRAM-OPTIONS-AVAILABLE)              CL**4
00785      END-EXEC.                                                    EL111
00786                                                                   EL111
00787  1750-EXIT.                                                       EL111
00788      EXIT.                                                        EL111
00789                                                                   EL111
00790  1760-END-BROWSE.                                                 EL111
00791      EXEC CICS ENDBR                                              EL111
00792          DATASET (LIT-FILE)                                       EL111
00793      END-EXEC.                                                    EL111
00794                                                                   EL111
00795  1770-EXIT.                                                       EL111
00796      EXIT.                                                        EL111
00797      EJECT                                                        EL111
00798  2000-JOURNAL-WRITE.                                              EL111
00799      MOVE LIT-SYS                TO JP-USER-ID.                   EL111
00800      MOVE LIT-FILE               TO JP-FILE-ID.                   EL111
00801      MOVE LIT-PGM                TO JP-PROGRAM-ID.                EL111
00802                                                                   EL111
pemuni*    IF PI-JOURNAL-FILE-ID NOT = ZERO                             EL111
pemuni*        EXEC CICS JOURNAL                                        EL111
pemuni*            JFILEID (1)                                          EL111
pemuni*            JTYPEID ('EL')                                       EL111
pemuni*            FROM (JOURNAL-RECORD)                                EL111
pemuni*            LENGTH (JOURNAL-LENGTH)                              EL111
pemuni*        END-EXEC.                                                EL111
00810                                                                   EL111
00811  2010-EXIT.                                                       EL111
00812      EXIT.                                                        EL111
00813      EJECT                                                        EL111
00814  8100-SEND-MAP.                                                   EL111
00815      PERFORM 8130-FORMAT-DATE-TIME THRU 8140-EXIT.                EL111
00816      MOVE LIT-IC                 TO MAINTL.                       EL111
00817      EXEC CICS SEND                                               EL111
00818          MAP    ('EL111A')                                        EL111
00819          MAPSET ('EL111S')                                        EL111
00820          ERASE                                                    EL111
00821          FREEKB                                                   EL111
00822          CURSOR                                                   EL111
00823      END-EXEC.                                                    EL111
00824                                                                   EL111
00825      GO TO 8120-RETURN-TRANS.                                     EL111
00826                                                                   EL111
00827  8110-SEND-DATA.                                                  EL111
00828      PERFORM 8130-FORMAT-DATE-TIME THRU 8140-EXIT.                EL111
00829                                                                   EL111
00830      EXEC CICS SEND                                               EL111
00831          MAP    ('EL111A')                                        EL111
00832          MAPSET ('EL111S')                                        EL111
00833          DATAONLY                                                 EL111
00834          ERASEAUP                                                 EL111
00835          FREEKB                                                   EL111
00836          CURSOR                                                   EL111
00837      END-EXEC.                                                    EL111
00838                                                                   EL111
00839  8120-RETURN-TRANS.                                               EL111
00840      EXEC CICS RETURN                                             EL111
00841          TRANSID  (LIT-TRANS)                                     EL111
00842          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL111
00843          LENGTH   (PI-COMM-LENGTH)                                EL111
00844      END-EXEC.                                                    EL111
00845      GOBACK.                                                      EL111
00846                                                                   EL111
00847  8130-FORMAT-DATE-TIME.                                           EL111
00848      MOVE SAVE-DATE              TO DATEO.                        EL111
00849      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL**4
00850      END-EXEC                                                        CL**4
00851      EXEC CICS FORMATTIME                                            CL**4
00852                ABSTIME(LCP-CICS-TIME)                                CL**4
00853                TIME(LCP-TIME-OF-DAY-XX)                              CL**4
00854      END-EXEC                                                        CL**4
00855      MOVE  LCP-TIME-OF-DAY-68 TO TIME-UNFORMATTED.                   CL**4
00856      MOVE UN-HOURS               TO FOR-HOURS.                    EL111
00857      MOVE UN-MINUTES             TO FOR-MINUTES.                  EL111
00858      MOVE TIME-FORMATTED         TO TIMEO.                        EL111
00859      MOVE LIT-MAP                TO PI-CURRENT-SCREEN-NO.         EL111
00860                                                                   EL111
00861  8140-EXIT.                                                       EL111
00862      EXIT.                                                        EL111
00863                                                                   EL111
00864  8200-NO-UPDATES.                                                 EL111
00865      EXEC CICS SEND TEXT                                          EL111
00866          FROM   (SCREEN-TERM)                                     EL111
00867          LENGTH (TERM-LENGTH)                                     EL111
00868          ERASE                                                    EL111
00869          FREEKB                                                   EL111
00870      END-EXEC.                                                    EL111
00871                                                                   EL111
00872  8220-RETURN-CICS.                                                EL111
00873      EXEC CICS RETURN                                             EL111
00874      END-EXEC.                                                    EL111
00875      GOBACK.                                                      EL111
00876                                                                   EL111
00877  EJECT                                                            EL111
00878                                                                   EL111
00879  8300-GET-HELP.                                                   EL111
00880      MOVE LIT-HELP TO CALL-PGM.                                   EL111
00881      GO TO 9400-XCTL.                                             EL111
00882                                                                   EL111
00883      EJECT                                                        EL111
00884  8800-UNAUTHORIZED-ACCESS.                                        EL111
00885      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL111
00886      GO TO 8800-SEND-TEXT.                                        EL111
00887                                                                   EL111
00888  8800-SEND-TEXT.                                                  EL111
00889      EXEC CICS SEND TEXT                                          EL111
00890          FROM    (LOGOFF-TEXT)                                    EL111
00891          LENGTH  (LOGOFF-LENGTH)                                  EL111
00892          ERASE                                                    EL111
00893          FREEKB                                                   EL111
00894      END-EXEC.                                                    EL111
00895                                                                   EL111
00896  EJECT                                                            EL111
00897  9000-XCTL.                                                       EL111
00898      MOVE PI-RETURN-TO-PROGRAM   TO CALL-PGM.                     EL111
00899      GO TO 9400-XCTL.                                             EL111
00900                                                                   EL111
00901  9100-XCTL.                                                       EL111
00902      MOVE XCTL-005               TO CALL-PGM.                     EL111
00903      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL111
00904      GO TO 9400-XCTL.                                             EL111
00905                                                                   EL111
00906  9200-XCTL.                                                       EL111
00907                                                                      CL**2
00908      IF  CREDIT-SESSION                                              CL**2
00909          MOVE XCTL-EL626         TO CALL-PGM                         CL**2
00910                                                                      CL**2
00911      ELSE                                                            CL**2
00912          IF  CLAIM-SESSION                                           CL**2
00913              MOVE XCTL-EL126     TO CALL-PGM                         CL**2
00914                                                                      CL**2
00915          ELSE                                                        CL**2
00916              IF  MORTGAGE-SESSION                                    CL**2
00917                  MOVE XCTL-EM626 TO CALL-PGM                         CL**2
00918                                                                      CL**2
00919              ELSE                                                    CL**2
00920                  IF  GENERAL-LEDGER-SESSION                          CL**2
00921                      MOVE XCTL-GL800                                 CL**2
00922                                  TO CALL-PGM.                        CL**2
00923                                                                   EL111
00924      GO TO 9400-XCTL.                                             EL111
00925                                                                   EL111
00926  9400-XCTL.                                                       EL111
00927      EXEC CICS XCTL                                               EL111
00928          PROGRAM (CALL-PGM)                                       EL111
00929          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL111
00930          LENGTH   (PI-COMM-LENGTH)                                EL111
00931      END-EXEC.                                                    EL111
00932                                                                   EL111
00933      GOBACK.                                                      EL111
00934                                                                   EL111
00935  9700-LINK-DATE-CONVERT.                                          EL111
00936      EXEC CICS LINK                                               EL111
00937          PROGRAM    ('ELDATCV')                                   EL111
00938          COMMAREA   (DATE-CONVERSION-DATA)                        EL111
00939          LENGTH     (DC-COMM-LENGTH)                              EL111
00940      END-EXEC.                                                    EL111
00941                                                                   EL111
00942  9700-EXIT.                                                       EL111
00943      EXIT.                                                        EL111
