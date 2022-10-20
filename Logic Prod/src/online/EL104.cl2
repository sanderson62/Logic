00001  IDENTIFICATION DIVISION.                                         02/26/96
00002                                                                   EL104
00003  PROGRAM-ID.                 EL104 .                                 LV010
00004 *              PROGRAM CONVERTED BY                                  CL*10
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*10
00006 *              CONVERSION DATE 02/12/96 10:18:15.                    CL*10
00007 *                            VMOD=2.010                              CL*10
00008 *                                                                 EL104
00008 *                                                                 EL104
00009 *AUTHOR.        LOGIC,INC.                                           CL*10
00010 *               DALLAS,TEXAS.                                        CL*10
00011                                                                   EL104
00012 *DATE-COMPILED.                                                      CL*10
00013                                                                   EL104
00014 *SECURITY.   *****************************************************   CL*10
00015 *            *                                                   *   CL*10
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*10
00017 *            *                                                   *   CL*10
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*10
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*10
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*10
00021 *            *                                                   *   CL*10
00022 *            *****************************************************   CL*10
00023                                                                   EL104
00024 *REMARKS.                                                            CL*10
00025 *        TRANSACTION EX06 - TEXT FILE MAINTENANCE                    CL*10
00026 *        THIS PROGRAM IS USED TO PERFORM MAINTENANCE TO THE HELP,    CL*10
00027 *        LETTER, AND FORM FILES.                                     CL*10
00028 *                                                                    CL*10
00029 *        IT WILL RENUMBER, COPY OR DELETE RECORDS FROM THE FILES     CL*10
00030 *        OR IT WILL BROWSE THE FILE AND BUILD TEMP STORAGE ITEMS     CL*10
00031 *        FOR EACH RECORD THAT MATCHES THE KEY AND PASS CONTROL TO    CL*10
00032 *        PROGRAM EL1042 FOR MAINTENANCE.                             CL*10
020410******************************************************************
020410*                   C H A N G E   L O G
020410*
020410* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
020410*-----------------------------------------------------------------
020410*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
020410* EFFECTIVE    NUMBER
020410*-----------------------------------------------------------------
020410* 020410  CR2009061500002  AJRA  FIX HELP FILE SELECTION
020410******************************************************************
00033                                                                   EL104
00034  ENVIRONMENT DIVISION.                                            EL104
00035  DATA DIVISION.                                                   EL104
00036  EJECT                                                            EL104
00037  WORKING-STORAGE SECTION.                                         EL104
00038  77  FILLER  PIC X(32)  VALUE '********************************'. EL104
00039  77  FILLER  PIC X(32)  VALUE '*    EL104 WORKING STORAGE     *'. EL104
00040  77  FILLER  PIC X(32)  VALUE '******** V/M 2.010 *************'.    CL*10
00041                                                                   EL104
00042      COPY ELCSCTM.                                                   CL**4
00043      COPY ELCSCRTY.                                                  CL**4
00044      COPY MPCSCRT.                                                   CL**4
00045                                                                   EL104
00046     EJECT                                                         EL104
00047                                                                   EL104
00048  01  WS-DATE-AREA.                                                EL104
00049      12  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL104
00050      12  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.        EL104
00051                                                                   EL104
00052  01  WS-CONSTANTS.                                                EL104
00053      12  W-APPL-SCRTY-NDX        PIC S9(4) COMP VALUE +7.            CL**4
00054      12  W-SC-ITEM               PIC S9(4) COMP VALUE +1.            CL**4
00055      12  MAP-NAME.                                                EL104
00056          16  MAP-NAME-PRE        PIC X(2)    VALUE 'EL'.          EL104
00057          16  MAP-NUMBER          PIC X(4)    VALUE '104A'.        EL104
00058          16  MAP-NAME-FILL       PIC X(2)    VALUE SPACES.        EL104
00059      12  MAPSET-NAME             PIC X(8)    VALUE 'EL104S'.      EL104
00060      12  TRANS-ID                PIC X(4)    VALUE 'EX06'.        EL104
00061      12  PGM-NAME                PIC X(8).                        EL104
00062      12  PGM-EL1042              PIC X(8)    VALUE 'EL1042'.      EL104
00063      12  THIS-PGM                PIC X(8)    VALUE 'EL104'.       EL104
00064      12  W-MAINT                 PIC X(1).                           CL**9
00065          88  W-ADD                           VALUE 'A'.              CL**9
00066          88  W-CHANGE                        VALUE 'C'.              CL**9
00067          88  W-COPY                          VALUE 'K'.              CL**9
00068          88  W-DELETE                        VALUE 'D' 'E'.          CL**9
00069          88  W-RENAME                        VALUE 'R'.              CL**9
00070          88  W-SHOW                          VALUE 'S' 'M'.          CL**9
00071          88  W-TRANSFORM                     VALUE 'T'.              CL**9
00072          88  W-VALID-MAINT-CR                VALUE 'A' 'C' 'D' 'K'   CL**9
00073                                                    'R' 'S' 'M' 'E'   CL**9
00074                                                    'T'.              CL**9
00075          88  W-VALID-MAINT-REST              VALUE 'A' 'C' 'D' 'K'   CL**9
00076                                                    'R' 'S' 'M'       CL**9
00077                                                    'E'.              CL**9
00078      12  W-FILE-TYPE             PIC X(1).                           CL**9
00079          88  W-FORM                          VALUE 'F'.              CL**9
00080          88  W-HELP                          VALUE 'H' 'A'.          CL**9
00081          88  W-LETTER                        VALUE 'L'.              CL**9
00082      12  XCTL-CLAIM              PIC X(8)    VALUE 'EL126'.       EL104
00083      12  XCTL-CREDIT             PIC X(8)    VALUE 'EL626'.       EL104
00084      12  XCTL-GEN-LEDGER         PIC X(8)    VALUE 'GL800'.       EL104
00085      12  XCTL-LIFE               PIC X(8)    VALUE 'LF400'.          CL**4
00086      12  XCTL-MORTGAGE           PIC X(8)    VALUE 'EM626'.          CL**4
00087      12  XCTL-WARRANTY           PIC X(8)    VALUE 'WA126'.          CL**4
00088      12  FILE-ID                 PIC X(8).                        EL104
00089      12  FILE-KEY.                                                EL104
00090          16  FILE-PARTIAL-KEY.                                    EL104
00091            18  CO-CD             PIC X.                           EL104
00092            18  CNTL-AREA         PIC X(12).                       EL104
00093          16  SEQ                 PIC S9(4)   COMP.                EL104
00094      12  BROWSE-STARTED-SW       PIC X       VALUE SPACE.         EL104
00095          88  BROWSE-STARTED      VALUE 'Y'.                       EL104
00096      12  LETTER-ID               PIC X(8)    VALUE 'ELLETR'.      EL104
00097      12  FORM-ID                 PIC X(8)    VALUE 'ELFORM'.      EL104
00098      12  HELP-ID                 PIC X(8)    VALUE 'ELHELP'.      EL104
00099      12  REQUEST-TYPE            PIC X       VALUE SPACES.        EL104
00100          88  RENUMBER-REQUEST    VALUE '1'.                       EL104
00101          88  COPY-REQUEST        VALUE '2'.                       EL104
00102          88  DELETE-REQUEST      VALUE '3'.                       EL104
00103      12  CNTL-KEY                PIC X(12).                       EL104
00104      12  CNTL-K1  REDEFINES CNTL-KEY.                             EL104
00105          16  K1                  PIC X.                           EL104
00106          16  K2                  PIC X.                           EL104
00107          16  K3                  PIC X.                           EL104
00108          16  K4                  PIC X.                           EL104
00109          16  K5                  PIC X.                           EL104
00110          16  FILLER              PIC X.                           EL104
00111          16  K7                  PIC X.                           EL104
00112          16  K8                  PIC X.                           EL104
00113          16  K9                  PIC X.                           EL104
00114          16  FILLER              PIC X(2).                        EL104
00115          16  K12                 PIC X.                           EL104
00116      12  TS-NAME.                                                 EL104
00117          16  FILLER              PIC X(4)    VALUE '104A'.        EL104
00118          16  TS-TERM             PIC X(4).                        EL104
00119      12  TS-ITEM                 PIC S9(4)   COMP VALUE +0.       EL104
00120                                                                   EL104
00121      12  FILE-LENGTH             PIC S9(4)   COMP VALUE +100.     EL104
00122      12  UPDATE-SW               PIC 9       VALUE 0.             EL104
00123          88  FILE-UPDATED        VALUE 1.                         EL104
00124                                                                   EL104
00125      12  OLD-KEY.                                                 EL104
00126          16  OLD-CO-CD           PIC X.                           EL104
00127          16  OLD-COMM-AREA       PIC X(12).                       EL104
00128          16  OLD-SEQ             PIC S9(4)   COMP.                EL104
00129      12  OLD-KEY-SAVE            PIC X(15).                       EL104
00130                                                                   EL104
00131      12  TIME-IN                 PIC S9(7).                       EL104
00132      12  FILLER REDEFINES TIME-IN.                                EL104
00133          16  FILLER              PIC X.                           EL104
00134          16  TIME-OUT            PIC 99V99.                       EL104
00135          16  FILLER              PIC 9(2).                        EL104
00136      12  WS-HEX-01               PIC S9(4) COMP  VALUE +1.        EL104
00137      12  FILLER REDEFINES WS-HEX-01.                              EL104
00138          16  FILLER              PIC X.                           EL104
00139          16  WS-LGX-COMPANY-CD   PIC X.                           EL104
00140  EJECT                                                            EL104
00141      COPY ELCLOGOF.                                                  CL**4
00142  EJECT                                                            EL104
00143      COPY ELCAID.                                                    CL**4
00144  01  FILLER  REDEFINES DFHAID.                                    EL104
00145      12  FILLER                  PIC X(8).                        EL104
00146      12  PF-VALUES OCCURS 24 TIMES       PIC X.                   EL104
00147  EJECT                                                            EL104
00148      COPY ELCEMIB.                                                   CL**4
00149      05  WS-ERROR-MESSAGE-AREA.                                   EL104
00150          10  ER-0000                 PIC 9(4)   VALUE 0000.       EL104
00151          10  ER-0004                 PIC 9(4)   VALUE 0004.       EL104
00152          10  ER-0005                 PIC 9(4)   VALUE 0005.       EL104
00153          10  ER-0006                 PIC 9(4)   VALUE 0006.       EL104
00154          10  ER-0008                 PIC 9(4)   VALUE 0008.       EL104
00155          10  ER-0009                 PIC 9(4)   VALUE 0009.       EL104
00156          10  ER-0012                 PIC 9(4)   VALUE 0012.       EL104
00157          10  ER-0013                 PIC 9(4)   VALUE 0013.       EL104
00158          10  ER-0014                 PIC 9(4)   VALUE 0014.       EL104
00159          10  ER-0015                 PIC 9(4)   VALUE 0015.       EL104
00160          10  ER-0023                 PIC 9(4)   VALUE 0023.       EL104
00161          10  ER-0029                 PIC 9(4)   VALUE 0029.       EL104
00162          10  ER-0070                 PIC 9(4)   VALUE 0070.       EL104
00163          10  ER-0076                 PIC 9(4)   VALUE 0076.       EL104
00164          10  ER-1162                 PIC 9(4)   VALUE 1162.       EL104
00165          10  ER-2237                 PIC 9(4)   VALUE 2237.       EL104
00166          10  ER-2238                 PIC 9(4)   VALUE 2238.       EL104
00167          10  ER-7270                 PIC 9(4)   VALUE 7270.       EL104
00168          10  ER-7271                 PIC 9(4)   VALUE 7271.       EL104
00169          10  ER-7392                 PIC 9(4)   VALUE 7392.          CL**7
00170          10  ER-9097                 PIC 9(4)   VALUE 9097.          CL**4
00171  EJECT                                                            EL104
00172      COPY ELCINTF.                                                   CL**4
00173      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.                    EL104
00174      COPY ELC1042.                                                   CL**6
00175          16  FILLER              PIC X(431).                         CL**6
00176          16  PI-104-SCREEN-BYPASS.                                   CL**4
00177              20  PI-104-TYPE     PIC  X(01).                         CL**4
00178              20  PI-104-NAME     PIC  X(12).                         CL**4
00179          16  PI-OLD-NAME         PIC  X(04).                         CL**7
00180          16  PI-NEW-NAME         PIC  X(04).                         CL**7
00181          16  PI-1043-ERROR       PIC  9(04).                         CL**7
00182          16  FILLER              PIC X(117).                         CL**7
00183  EJECT                                                            EL104
00184      COPY ELCATTR.                                                   CL**4
00185  EJECT                                                            EL104
00186      COPY ELCDATE.                                                   CL**4
00187  EJECT                                                            EL104
00188      COPY EL104S.                                                    CL**4
00189  EJECT                                                            EL104
00190  LINKAGE SECTION.                                                 EL104
00191  01  DFHCOMMAREA                 PIC X(1024).                     EL104
00192 *01 PARMLIST .                                                       CL*10
00193 *    12  FILLER                  PIC S9(8)  COMP.                    CL*10
00194 *    12  TXT-ADDR                PIC S9(8)  COMP.                    CL*10
00195  EJECT                                                            EL104
00196      COPY ELCTEXT.                                                   CL**4
00197  EJECT                                                            EL104
00198  PROCEDURE DIVISION.                                              EL104
00199                                                                      CL**4
00200      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                 EL104
00201                                                                   EL104
00202      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL104
00203      MOVE '5'                   TO DC-OPTION-CODE.                EL104
00204      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL104
00205      MOVE DC-GREG-DATE-1-EDIT   TO SAVE-DATE.                        CL**4
00206      MOVE DC-BIN-DATE-1         TO SAVE-BIN-DATE.                    CL**4
00207                                                                   EL104
00208      MOVE 2 TO EMI-NUMBER-OF-LINES.                               EL104
00209                                                                   EL104
00210      IF  PI-LANGUAGE-IS-FR                                           CL**9
00211          MOVE 'EL104AF'         TO MAP-NAME.                         CL**9
00212                                                                      CL**9
00213      IF EIBCALEN = ZEROS                                          EL104
00214          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL104
00215                                                                   EL104
00216      IF  PI-CALLING-PROGRAM NOT = THIS-PGM                           CL**4
00217          IF  PI-RETURN-TO-PROGRAM NOT = THIS-PGM                     CL**4
00218              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6         CL**4
00219              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5         CL**4
00220              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4         CL**4
00221              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3         CL**4
00222              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2         CL**4
00223              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1         CL**4
00224              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM       CL**4
00225              MOVE THIS-PGM             TO PI-CALLING-PROGRAM         CL**4
00226              MOVE LOW-VALUES           TO EL104AI                    CL**4
00227              MOVE 'N'                  TO PI-104-SCREEN-SENT-IND     CL**6
00228                                                                      CL**4
00229              IF  PI-104-SCREEN-BYPASS                                CL**4
00230                      NOT EQUAL LOW-VALUES                            CL**4
00231                      AND                                             CL**4
00232                  PI-104-SCREEN-BYPASS                                CL**4
00233                      NOT EQUAL SPACES                                CL**4
00234                  MOVE PI-104-TYPE      TO FILETYPI                   CL**4
00235                  MOVE +1               TO FILETYPL                   CL**4
00236                  MOVE PI-104-NAME      TO CONTROLI                   CL**4
00237                  MOVE +12              TO CONTROLL                   CL**4
00238                  MOVE +1               TO MAINTL                     CL**4
00239                  MOVE QUOTE            TO EIBAID                     CL**4
00240                                                                      CL**9
00241                  IF  PI-LANGUAGE-IS-FR                               CL**9
00242                      MOVE 'M'          TO MAINTI                     CL**9
00243                      GO TO 2001-CHECK-PFKEYS                         CL**9
00244                                                                      CL**9
00245                  ELSE                                                CL**9
00246                      MOVE 'S'          TO MAINTI                     CL**9
00247                      GO TO 2001-CHECK-PFKEYS                         CL**9
00248                                                                      CL**4
00249              ELSE                                                    CL**4
00250                  GO TO 8100-SEND-INITIAL-MAP                         CL**4
00251                                                                      CL**4
00252        ELSE                                                          CL**4
00253            MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM        EL104
00254            MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM      EL104
00255            MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1        EL104
00256            MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2        EL104
00257            MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3        EL104
00258            MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4        EL104
00259            MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5        EL104
00260            MOVE SPACES               TO PI-SAVED-PROGRAM-6        EL104
00261            MOVE LOW-VALUES TO EL104AI                             EL104
00262            MOVE PI-LAST-CONTROL TO CONTROLO                          CL**8
00263            MOVE PI-FILETYP      TO FILETYPO                          CL**8
00264            MOVE +1              TO MAINTL                            CL**8
00265                                    FILETYPL                          CL**8
00266            MOVE +12             TO CONTROLL                          CL**8
00267            MOVE AL-UANON        TO CONTROLA                          CL**8
00268                                    MAINTA                            CL**8
00269                                    FILETYPA                          CL**8
00270            IF  PI-LANGUAGE-IS-FR                                     CL**9
00271                MOVE 'M'         TO MAINTO                            CL**9
00272                GO TO 8100-SEND-INITIAL-MAP                           CL**9
00273                                                                      CL**9
00274            ELSE                                                      CL**9
00275                MOVE 'S'         TO MAINTO                            CL**9
00276                GO TO 8100-SEND-INITIAL-MAP.                          CL**9
00277                                                                   EL104
00278      EXEC CICS HANDLE CONDITION                                   EL104
00279           PGMIDERR(9600-PGMID-ERROR)                              EL104
00280           ERROR   (9990-ABEND)                                    EL104
00281       END-EXEC.                                                   EL104
00282                                                                   EL104
00283      IF EIBAID = DFHCLEAR                                         EL104
00284         GO TO 9400-CLEAR.                                         EL104
00285                                                                   EL104
00286      IF PI-PROCESSOR-ID = 'LGXX'                                  EL104
00287          GO TO 0200-RECEIVE.                                      EL104
00288                                                                   EL104
00289      IF  MORTGAGE-SESSION                                            CL**4
00290          PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT             CL**4
00291          GO TO 0200-RECEIVE.                                         CL**4
00292                                                                      CL**4
00293      EXEC CICS READQ TS                                           EL104
00294          QUEUE  (PI-SECURITY-TEMP-STORE-ID)                       EL104
00295          INTO   (SECURITY-CONTROL)                                EL104
00296          LENGTH (SC-COMM-LENGTH)                                  EL104
00297          ITEM   (W-SC-ITEM)                                          CL**4
00298      END-EXEC.                                                    EL104
00299                                                                   EL104
00300      MOVE SC-CREDIT-DISPLAY (03)  TO PI-DISPLAY-CAP.              EL104
00301      MOVE SC-CREDIT-UPDATE  (03)  TO PI-MODIFY-CAP.               EL104
00302                                                                   EL104
00303      IF NOT DISPLAY-CAP                                           EL104
00304          MOVE 'READ'          TO SM-READ                          EL104
00305          PERFORM 9995-SECURITY-VIOLATION                          EL104
00306          MOVE ER-0070         TO  EMI-ERROR                       EL104
00307          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL104
00308          GO TO 8100-SEND-INITIAL-MAP.                             EL104
00309                                                                   EL104
00310  EJECT                                                            EL104
00311  0200-RECEIVE.                                                    EL104
00312      MOVE LOW-VALUES TO EL104AI.                                  EL104
00313                                                                   EL104
00314      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL104
00315            MOVE -1 TO MAINTL                                      EL104
00316            MOVE ER-0008 TO EMI-ERROR                              EL104
00317            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL104
00318            GO TO 8200-SEND-DATAONLY.                              EL104
00319                                                                   EL104
00320      EXEC CICS RECEIVE                                            EL104
00321           MAP   (MAP-NAME)                                        EL104
00322           MAPSET(MAPSET-NAME)                                     EL104
00323           INTO  (EL104AI)                                         EL104
00324       END-EXEC.                                                   EL104
00325                                                                   EL104
00326      IF PFENTERL = ZEROS                                          EL104
00327         GO TO 2001-CHECK-PFKEYS.                                  EL104
00328                                                                   EL104
00329      IF EIBAID NOT = DFHENTER                                     EL104
00330         MOVE ER-0004 TO EMI-ERROR                                 EL104
00331         GO TO 2002-INPUT-ERROR.                                   EL104
00332                                                                   EL104
00333      IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)      CL**2
00334          MOVE PF-VALUES (PFENTERI) TO EIBAID                      EL104
00335      ELSE                                                         EL104
00336          MOVE ER-0029 TO EMI-ERROR                                EL104
00337          GO TO 2002-INPUT-ERROR.                                  EL104
00338                                                                   EL104
00339  2001-CHECK-PFKEYS.                                               EL104
00340      IF EIBAID = DFHPF23                                          EL104
00341          GO TO 9000-RETURN-CICS.                                  EL104
00342                                                                   EL104
00343      IF EIBAID = DFHPF24                                          EL104
00344          GO TO 9200-RETURN-MAIN-MENU.                             EL104
00345                                                                   EL104
00346      IF EIBAID = DFHPF12                                          EL104
00347          GO TO 9500-PF12.                                         EL104
00348                                                                   EL104
00349      IF  FILETYPL = ZERO                                             CL**9
00350          MOVE ER-7270            TO EMI-ERROR                     EL104
00351          MOVE -1                 TO FILETYPL                      EL104
00352          MOVE AL-UABON           TO FILETYPA                      EL104
00353          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL104
00354          GO TO 8200-SEND-DATAONLY                                 EL104
00355                                                                      CL**9
00356      ELSE                                                         EL104
00357          MOVE FILETYPI           TO W-FILE-TYPE                      CL**9
00358                                                                      CL**9
00359          IF  W-LETTER                                                CL**9
00360              MOVE '1'            TO PI-ENTRY-CD-1                 EL104
00361                                                                      CL**9
00362          ELSE                                                     EL104
00363              IF  W-FORM                                              CL**9
00364                  MOVE '2'        TO PI-ENTRY-CD-1                 EL104
00365                                                                      CL**9
00366              ELSE                                                 EL104
00367                  IF  W-HELP                                          CL**9
00368                      MOVE '3'    TO PI-ENTRY-CD-1                 EL104
00369                                                                      CL**9
00370                  ELSE                                             EL104
00371                      MOVE '0'    TO PI-ENTRY-CD-1                 EL104
00372                      MOVE ER-7270                                    CL**9
00373                                  TO EMI-ERROR                        CL**9
00374                      MOVE -1     TO FILETYPL                         CL**9
00375                      MOVE AL-UABON                                   CL**9
00376                                  TO FILETYPA                         CL**9
00377                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL104
00378                      GO TO 8200-SEND-DATAONLY.                    EL104
00379                                                                   EL104
00380      MOVE FILETYPI               TO PI-FILETYP.                   EL104
00381                                                                   EL104
00382      IF EIBAID = DFHPF1 OR DFHPF2                                 EL104
00383         GO TO 4000-BROWSE-FILE.                                   EL104
00384                                                                   EL104
00385      IF EIBAID = DFHENTER                                         EL104
00386         GO TO 2003-EDIT-DATA.                                     EL104
00387                                                                   EL104
00388      IF EIBAID = DFHPF5  AND                                      EL104
00389         PI-PROCESSOR-ID = 'LGXX'                                  EL104
00390          GO TO 2300-COPY-SAMP.                                    EL104
00391                                                                   EL104
00392      MOVE ER-0029 TO EMI-ERROR.                                   EL104
00393                                                                   EL104
00394  2002-INPUT-ERROR.                                                EL104
00395      MOVE -1       TO PFENTERL.                                   EL104
00396      MOVE AL-UNBON TO PFENTERA.                                   EL104
00397      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     EL104
00398      GO TO 8200-SEND-DATAONLY.                                    EL104
00399                                                                   EL104
00400  2003-EDIT-DATA.                                                  EL104
00401                                                                      CL**9
00402      IF MAINTL = ZERO                                             EL104
00403          MOVE ER-0023            TO EMI-ERROR                     EL104
00404          MOVE -1                 TO MAINTL                        EL104
00405          MOVE AL-UABON           TO MAINTA                        EL104
00406          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL104
00407          GO TO 8200-SEND-DATAONLY.                                EL104
00408                                                                   EL104
00409      MOVE MAINTI                 TO W-MAINT.                         CL**9
00410                                                                      CL**9
00411      IF  CREDIT-SESSION                                              CL**7
00412                                                                      CL**7
00413          IF  W-VALID-MAINT-CR                                        CL**9
00414              MOVE MAINTI         TO PI-ACTION                        CL**9
00415                                                                      CL**7
00416          ELSE                                                        CL**7
00417              MOVE ER-0023        TO EMI-ERROR                        CL**9
00418              MOVE -1             TO MAINTL                           CL**9
00419              MOVE AL-UABON       TO MAINTA                           CL**9
00420              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**7
00421              GO TO 8200-SEND-DATAONLY                                CL**7
00422                                                                      CL**7
00423      ELSE                                                         EL104
00424          IF  W-VALID-MAINT-REST                                      CL**9
00425              MOVE MAINTI         TO PI-ACTION                        CL**9
00426                                                                      CL**7
00427          ELSE                                                        CL**7
00428              MOVE ER-0023        TO EMI-ERROR                        CL**9
00429              MOVE -1             TO MAINTL                           CL**9
00430              MOVE AL-UABON       TO MAINTA                           CL**9
00431              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**7
00432              GO TO 8200-SEND-DATAONLY.                               CL**7
00433                                                                   EL104
00434      IF  W-SHOW                                                      CL**9
00435          NEXT SENTENCE                                            EL104
00436                                                                   EL104
00437      ELSE                                                            CL**9
00438          IF NOT MODIFY-CAP                                           CL**9
00439              MOVE 'UPDATE'       TO SM-READ                          CL**9
00440              PERFORM 9995-SECURITY-VIOLATION                         CL**9
00441              MOVE ER-0070        TO EMI-ERROR                        CL**9
00442              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**9
00443              GO TO 8200-SEND-DATAONLY.                               CL**9
00444                                                                   EL104
00445      MOVE CONTROLI TO OLD-COMM-AREA  CNTL-KEY PI-COMM-CONTROL.    EL104
00446      MOVE PI-COMPANY-CD TO OLD-CO-CD.                             EL104
00447      MOVE ZEROS         TO OLD-SEQ.                               EL104
00448                                                                   EL104
00449      IF PI-ENTRY-CD-1 = '1'                                       EL104
00450          MOVE LETTER-ID TO FILE-ID                                EL104
00451      ELSE                                                         EL104
00452          IF PI-ENTRY-CD-1 = '2'                                   EL104
00453              MOVE FORM-ID TO FILE-ID                              EL104
00454          ELSE                                                     EL104
00455              MOVE HELP-ID TO FILE-ID.                             EL104
00456                                                                   EL104
020410     IF FILE-ID = HELP-ID
020410         MOVE LOW-VALUES TO OLD-CO-CD
020410     END-IF.
020410
00457      MOVE OLD-KEY TO OLD-KEY-SAVE.                                EL104
00458                                                                   EL104
00459      IF CONTROLL = ZEROS                                          EL104
00460          MOVE -1                  TO CONTROLL                     EL104
00461          MOVE ER-0005             TO EMI-ERROR                    EL104
00462          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL104
00463      ELSE                                                         EL104
00464          MOVE AL-UANON            TO CONTROLA.                    EL104
00465                                                                   EL104
00466      IF  W-TRANSFORM                                                 CL**9
00467                                                                      CL**7
00468          IF  NEWCONTL = ZEROS                                        CL**7
00469              MOVE CONTROLI       TO NEWCONTO                         CL**7
00470              MOVE AL-UANON       TO NEWCONTA                         CL**7
00471                                                                      CL**7
00472          ELSE                                                     EL104
00473              MOVE NEWCONTI       TO NEWCONTO                      EL104
00474              MOVE AL-UANON       TO NEWCONTA                         CL**7
00475                                                                      CL**7
00476      ELSE                                                            CL**7
00477          IF  W-RENAME                                                CL**9
00478                  OR                                                  CL**9
00479              W-COPY                                                  CL**9
00480                                                                      CL**7
00481              IF  NEWCONTL = ZEROS                                    CL**7
00482                  MOVE -1         TO NEWCONTL                         CL**7
00483                  MOVE ER-7271    TO EMI-ERROR                        CL**7
00484                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**7
00485                                                                      CL**7
00486              ELSE                                                    CL**7
00487                  MOVE NEWCONTI   TO NEWCONTO                         CL**7
00488                  MOVE AL-UANON   TO NEWCONTA.                        CL**7
00489                                                                   EL104
00490      IF NOT EMI-NO-ERRORS                                         EL104
00491         GO TO 8200-SEND-DATAONLY.                                 EL104
00492                                                                   EL104
00493      IF  W-ADD                                                       CL**9
00494              OR                                                      CL**9
00495          W-CHANGE                                                    CL**9
00496              OR                                                      CL**9
00497          W-SHOW                                                      CL**9
00498          GO TO 2100-BUILD-TS-RECORDS.                             EL104
00499                                                                      CL**7
00500      IF  W-TRANSFORM                                                 CL**9
00501          GO TO 2700-CONVERT-LETTERS.                                 CL**7
00502                                                                   EL104
00503      IF  W-RENAME                                                    CL**9
00504          MOVE NEWCONTI           TO CNTL-KEY                      EL104
00505          MOVE '1'                TO REQUEST-TYPE                  EL104
00506          MOVE AL-UANON           TO NEWCONTA.                     EL104
00507                                                                   EL104
00508      IF  W-COPY                                                      CL**9
00509          MOVE NEWCONTI           TO CNTL-KEY                      EL104
00510          MOVE '2'                TO REQUEST-TYPE                  EL104
00511          MOVE AL-UANON           TO NEWCONTA.                     EL104
00512                                                                   EL104
00513      IF  W-DELETE                                                    CL**9
00514          MOVE CONTROLI           TO CNTL-KEY                      EL104
00515          MOVE '3'                TO REQUEST-TYPE                  EL104
00516          MOVE AL-UANON           TO CONTROLA                      EL104
00517          GO TO 5000-DELETE-FILE.                                  EL104
00518                                                                   EL104
00519      PERFORM 2500-EDIT-CONTROL THRU 2599-EXIT.                    EL104
00520      GO TO 3000-RENUMBER-FILE.                                    EL104
00521                                                                   EL104
00522  2100-BUILD-TS-RECORDS.                                           EL104
00523      MOVE CONTROLI TO CNTL-KEY                                       CL**8
00524                       PI-LAST-CONTROL.                               CL**8
00525      PERFORM 2500-EDIT-CONTROL THRU 2599-EXIT.                    EL104
00526      MOVE PGM-EL1042 TO PGM-NAME.                                 EL104
00527      GO TO 9300-XCTL.                                             EL104
00528  EJECT                                                            EL104
00529  2300-COPY-SAMP.                                                  EL104
00530     EXEC CICS GETMAIN                                             EL104
00531         SET     (ADDRESS OF TEXT-FILES)                              CL*10
00532         LENGTH  (100)                                             EL104
00533     END-EXEC.                                                     EL104
00534                                                                   EL104
00535      MOVE CONTROLI   TO  CNTL-KEY  OLD-COMM-AREA.                 EL104
00536      MOVE ZEROS      TO  OLD-SEQ.                                 EL104
00537      MOVE LETTER-ID  TO  FILE-ID.                                 EL104
00538                                                                   EL104
00539      EXEC CICS HANDLE CONDITION                                   EL104
00540          NOTFND   (2330-NOT-FOUND)                                EL104
00541          NOTOPEN  (6000-NOT-OPEN)                                 EL104
00542          DUPREC   (2350-DUP-REC)                                  EL104
00543          ENDFILE  (2340-END-FILE)                                 EL104
00544      END-EXEC.                                                    EL104
00545                                                                   EL104
00546  2310-LOOP.                                                       EL104
00547      MOVE WS-LGX-COMPANY-CD  TO  OLD-CO-CD.                       EL104
00548                                                                   EL104
020410     IF FILE-ID = HELP-ID
020410         MOVE LOW-VALUES TO OLD-CO-CD
020410     END-IF.
020410
00549      EXEC CICS STARTBR                                            EL104
00550          DATASET    (FILE-ID)                                     EL104
00551          RIDFLD     (OLD-KEY)                                     EL104
00552          KEYLENGTH  (15)                                          EL104
00553          GTEQ                                                     EL104
00554      END-EXEC.                                                    EL104
00555                                                                   EL104
00556  2320-READNEXT.                                                   EL104
00557      EXEC CICS READNEXT                                           EL104
00558          INTO     (TEXT-FILES)                                    EL104
00559          DATASET  (FILE-ID)                                       EL104
00560          RIDFLD   (OLD-KEY)                                       EL104
00561      END-EXEC.                                                    EL104
00562                                                                   EL104
00563      EXEC CICS ENDBR                                              EL104
00564          DATASET  (FILE-ID)                                       EL104
00565      END-EXEC.                                                    EL104
00566                                                                   EL104
00567      IF TX-ACCESS-CD-GENL NOT = CNTL-KEY                          EL104
00568          GO TO 2340-END-FILE.                                     EL104
00569                                                                   EL104
020410     IF FILE-ID = HELP-ID
020410         MOVE LOW-VALUES TO  TX-COMPANY-CD
020410     ELSE
00570      MOVE PI-COMPANY-CD  TO  TX-COMPANY-CD.                       EL104
00571                                                                   EL104
00572      EXEC CICS WRITE                                              EL104
00573          FROM     (TEXT-FILES)                                    EL104
00574          DATASET  (FILE-ID)                                       EL104
00575          RIDFLD   (TX-CONTROL-PRIMARY)                            EL104
00576      END-EXEC.                                                    EL104
00577                                                                   EL104
00578      ADD +1  TO  TX-LINE-SEQUENCE.                                EL104
00579                                                                   EL104
00580      MOVE TX-LINE-SEQUENCE  TO  OLD-SEQ.                          EL104
00581      MOVE 1                 TO  UPDATE-SW.                        EL104
00582                                                                   EL104
00583      GO TO 2310-LOOP.                                             EL104
00584                                                                   EL104
00585  2330-NOT-FOUND.                                                  EL104
00586      MOVE -1    TO  CONTROLL.                                     EL104
00587      MOVE ER-0006  TO  EMI-ERROR.                                 EL104
00588                                                                   EL104
00589      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL104
00590                                                                   EL104
00591      GO TO 8200-SEND-DATAONLY.                                    EL104
00592                                                                   EL104
00593  2340-END-FILE.                                                   EL104
00594      IF NOT FILE-UPDATED                                          EL104
00595          GO TO 2330-NOT-FOUND.                                    EL104
00596                                                                   EL104
00597      MOVE LOW-VALUES  TO  EL104AO.                                EL104
00598      MOVE -1          TO  MAINTL.                                 EL104
00599                                                                   EL104
00600      GO TO 8100-SEND-INITIAL-MAP.                                 EL104
00601                                                                   EL104
00602  2350-DUP-REC.                                                    EL104
00603      MOVE -1    TO  NEWCONTL.                                     EL104
00604      MOVE ER-0076  TO  EMI-ERROR.                                 EL104
00605                                                                   EL104
00606      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL104
00607                                                                   EL104
00608      GO TO 8200-SEND-DATAONLY.                                    EL104
00609  EJECT                                                            EL104
00610  2500-EDIT-CONTROL.                                               EL104
00611      IF PI-ENTRY-CD-1 = '1'                                       EL104
00612         IF K5 NOT = SPACES                                        EL104
00613            MOVE ER-0009 TO EMI-ERROR                              EL104
00614            GO TO 2600-SET-ERROR-FLAG.                             EL104
00615                                                                   EL104
00616      IF PI-ENTRY-CD-1 = '3'                                       EL104
00617          IF (K1 = ' ' OR 'S' OR 'E')  AND                         EL104
00618             K9 = SPACE                                            EL104
00619              NEXT SENTENCE                                        EL104
00620          ELSE                                                     EL104
00621               MOVE ER-0012 TO EMI-ERROR                           EL104
00622               GO TO 2600-SET-ERROR-FLAG.                          EL104
00623                                                                   EL104
00624      IF PI-ENTRY-CD-1 = '3'                                       EL104
00625          IF K1 = 'E'                                              EL104
00626             IF K2 NOT NUMERIC  OR                                 EL104
00627                K3 NOT NUMERIC  OR                                 EL104
00628                K4 NOT NUMERIC  OR                                 EL104
00629                K5 NOT NUMERIC                                     EL104
00630                 MOVE ER-0012 TO EMI-ERROR                         EL104
00631                 GO TO 2600-SET-ERROR-FLAG.                        EL104
00632                                                                   EL104
00633      IF PI-ENTRY-CD-1 = '3'                                       EL104
00634          IF K1 = 'S'                                              EL104
00635             IF K2 NOT NUMERIC  OR                                 EL104
00636                K3 NOT NUMERIC  OR                                 EL104
00637                K4 NOT NUMERIC                                     EL104
00638                 MOVE ER-0012 TO EMI-ERROR                         EL104
00639                 GO TO 2600-SET-ERROR-FLAG.                        EL104
00640                                                                   EL104
00641  2599-EXIT.                                                       EL104
00642       EXIT.                                                       EL104
00643                                                                   EL104
00644  2600-SET-ERROR-FLAG.                                             EL104
00645      IF RENUMBER-REQUEST OR COPY-REQUEST                          EL104
00646          MOVE -1       TO NEWCONTL                                EL104
00647          MOVE AL-UABON TO NEWCONTA                                EL104
00648      ELSE                                                         EL104
00649          MOVE -1       TO CONTROLL                                EL104
00650          MOVE AL-UABON TO CONTROLA.                               EL104
00651                                                                   EL104
00652      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL104
00653      GO TO 8200-SEND-DATAONLY.                                    EL104
00654  EJECT                                                               CL**7
00655  2700-CONVERT-LETTERS.                                               CL**7
00656                                                                      CL**7
00657      MOVE NEWCONTI               TO PI-NEW-NAME.                     CL**7
00658      MOVE CONTROLI               TO PI-OLD-NAME.                     CL**7
00659      MOVE 0000                   TO PI-1043-ERROR.                   CL**7
00660                                                                      CL**7
00661      EXEC CICS LINK                                                  CL**7
00662           PROGRAM   ('EL1043')                                       CL**7
00663           COMMAREA  (PROGRAM-INTERFACE-BLOCK)                        CL**7
00664           LENGTH    (PI-COMM-LENGTH)                                 CL**7
00665      END-EXEC.                                                       CL**7
00666                                                                      CL**7
00667      IF  PI-1043-ERROR NOT EQUAL 0000                                CL**7
00668          MOVE PI-1043-ERROR      TO EMI-ERROR                        CL**7
00669          MOVE -1                 TO CONTROLL                         CL**7
00670                                                                      CL**7
00671      ELSE                                                            CL**7
00672          MOVE NEWCONTI           TO CONTROLO                         CL**7
00673          MOVE SPACES             TO NEWCONTO                         CL**7
00674          MOVE -1                 TO MAINTL                           CL**7
00675          MOVE ER-7392            TO EMI-ERROR.                       CL**7
00676                                                                      CL**9
00677          IF  PI-LANGUAGE-IS-FR                                       CL**9
00678              MOVE 'M'            TO MAINTO                           CL**9
00679                                                                      CL**9
00680          ELSE                                                        CL**9
00681              MOVE 'S'            TO MAINTO.                          CL**9
00682                                                                      CL**7
00683      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**7
00684      GO TO 8100-SEND-INITIAL-MAP.                                    CL**7
00685                                                                      CL**7
00686  2700-EXIT.                                                          CL**7
00687      EXIT.                                                           CL**7
00688  EJECT                                                            EL104
00689  3000-RENUMBER-FILE.                                              EL104
00690      MOVE 0                      TO UPDATE-SW.                    EL104
00691                                                                   EL104
00692      EXEC CICS GETMAIN                                            EL104
00693           SET    (ADDRESS OF TEXT-FILES)                             CL*10
00694           LENGTH (100)                                            EL104
00695       END-EXEC.                                                   EL104
00696                                                                   EL104
00697      MOVE CONTROLI TO CNTL-KEY.                                   EL104
00698                                                                   EL104
00699      EXEC CICS HANDLE CONDITION                                   EL104
00700           NOTFND (3900-NOT-FOUND)                                 EL104
00701           NOTOPEN(6000-NOT-OPEN)                                  EL104
00702           DUPREC (3920-DUP-REC)                                   EL104
00703           ENDFILE(3910-END-FILE)                                  EL104
00704       END-EXEC.                                                   EL104
00705                                                                   EL104
00706  3001-LOOP.                                                       EL104
00707      EXEC CICS STARTBR                                            EL104
00708           DATASET  (FILE-ID)                                      EL104
00709           RIDFLD   (OLD-KEY)                                      EL104
00710           KEYLENGTH(15)                                           EL104
00711           GTEQ                                                    EL104
00712       END-EXEC.                                                   EL104
00713                                                                   EL104
00714  3100-READNEXT.                                                   EL104
00715      EXEC CICS READNEXT                                           EL104
00716           INTO   (TEXT-FILES)                                     EL104
00717           DATASET(FILE-ID)                                        EL104
00718           RIDFLD (OLD-KEY)                                        EL104
00719       END-EXEC.                                                   EL104
00720                                                                   EL104
00721      EXEC CICS ENDBR                                              EL104
00722           DATASET (FILE-ID)                                       EL104
00723       END-EXEC.                                                   EL104
00724                                                                   EL104
00725      IF TX-ACCESS-CD-GENL NOT = CNTL-KEY                          EL104
00726         GO TO 3910-END-FILE.                                      EL104
00727                                                                   EL104
00728      MOVE NEWCONTI TO TX-ACCESS-CD-GENL.                          EL104
00729                                                                   EL104
00730      EXEC CICS WRITE                                              EL104
00731           FROM   (TEXT-FILES)                                     EL104
00732           DATASET(FILE-ID)                                        EL104
00733           RIDFLD (TX-CONTROL-PRIMARY)                             EL104
00734       END-EXEC.                                                   EL104
00735                                                                   EL104
00736      ADD +1 TO TX-LINE-SEQUENCE.                                  EL104
00737      MOVE TX-LINE-SEQUENCE TO OLD-SEQ.                            EL104
00738      MOVE 1 TO UPDATE-SW                                          EL104
00739      GO TO 3001-LOOP.                                             EL104
00740                                                                   EL104
00741  EJECT                                                            EL104
00742  3900-NOT-FOUND.                                                  EL104
00743      MOVE -1      TO CONTROLL.                                    EL104
00744      MOVE ER-0006 TO EMI-ERROR.                                   EL104
00745      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL104
00746      GO TO 8200-SEND-DATAONLY.                                    EL104
00747                                                                   EL104
00748  3910-END-FILE.                                                   EL104
00749      IF NOT FILE-UPDATED                                          EL104
00750         GO TO 3900-NOT-FOUND.                                     EL104
00751                                                                   EL104
00752      IF RENUMBER-REQUEST                                          EL104
00753         EXEC CICS DELETE                                          EL104
00754              DATASET  (FILE-ID)                                   EL104
00755              RIDFLD   (OLD-KEY-SAVE)                              EL104
00756              KEYLENGTH(13)                                        EL104
00757              GENERIC                                              EL104
00758          END-EXEC.                                                EL104
00759                                                                   EL104
00760      MOVE LOW-VALUES TO EL104AO.                                  EL104
00761      MOVE -1         TO CONTROLL.                                 EL104
00762      GO TO 8100-SEND-INITIAL-MAP.                                 EL104
00763                                                                   EL104
00764  3920-DUP-REC.                                                    EL104
00765      MOVE -1      TO NEWCONTL.                                    EL104
00766      MOVE ER-0076 TO EMI-ERROR.                                   EL104
00767      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL104
00768      GO TO 8200-SEND-DATAONLY.                                    EL104
00769                                                                   EL104
00770  EJECT                                                            EL104
00771  4000-BROWSE-FILE.                                                EL104
00772      IF PI-ENTRY-CD-1 = '1'                                       EL104
00773          MOVE LETTER-ID TO FILE-ID                                EL104
00774      ELSE                                                         EL104
00775          IF PI-ENTRY-CD-1 = '2'                                   EL104
00776              MOVE FORM-ID TO FILE-ID                              EL104
00777          ELSE                                                     EL104
00778              MOVE HELP-ID TO FILE-ID.                             EL104
00779                                                                   EL104
00780      MOVE SPACE TO BROWSE-STARTED-SW.                             EL104
00781                                                                   EL104
00782      EXEC CICS HANDLE CONDITION                                   EL104
00783           NOTOPEN (6000-NOT-OPEN)                                 EL104
00784           NOTFND  (4800-NO-RECORD)                                EL104
00785           ENDFILE (4900-END-FILE)                                 EL104
00786       END-EXEC.                                                   EL104
00787                                                                   EL104
00788      IF  CONTROLI = SPACES OR LOW-VALUES                             CL**8
00789                                                                      CL**8
00790          IF  PI-LAST-CONTROL GREATER THAN LOW-VALUES                 CL**8
00791              MOVE PI-LAST-CONTROL                                    CL**8
00792                                  TO CONTROLI                         CL**8
00793                                                                      CL**8
00794          ELSE                                                        CL**8
00795              MOVE LOW-VALUES     TO CONTROLI.                        CL**8
00796                                                                   EL104
00797      MOVE PI-COMPANY-CD TO CO-CD.                                 EL104
020410     IF FILE-ID = HELP-ID
020410         MOVE LOW-VALUES TO CO-CD
020410     END-IF.
020410
00798      MOVE ZEROS         TO SEQ.                                   EL104
00799      MOVE CONTROLI      TO CNTL-AREA  CNTL-KEY  PI-COMM-CONTROL.  EL104
00800                                                                   EL104
00801      EXEC CICS STARTBR                                            EL104
00802           DATASET(FILE-ID)                                        EL104
00803           RIDFLD (FILE-KEY)                                       EL104
00804       END-EXEC.                                                   EL104
00805                                                                   EL104
00806      MOVE 'Y' TO BROWSE-STARTED-SW.                               EL104
00807                                                                   EL104
00808      IF EIBAID = DFHPF2                                           EL104
00809         MOVE 1 TO SEQ                                             EL104
00810         GO TO 4600-BROWSE-BKWD                                    EL104
00811        ELSE                                                       EL104
00812         MOVE 9999 TO SEQ.                                         EL104
00813                                                                   EL104
00814  4500-READNEXT.                                                   EL104
00815      EXEC CICS READNEXT                                           EL104
00816           SET    (ADDRESS OF TEXT-FILES)                             CL*10
00817           DATASET(FILE-ID)                                        EL104
00818           RIDFLD (FILE-KEY)                                       EL104
00819       END-EXEC.                                                   EL104
00820                                                                   EL104
020410     IF FILE-ID = HELP-ID
020410         NEXT SENTENCE
020410     ELSE
00821      IF TX-COMPANY-CD NOT = PI-COMPANY-CD                         EL104
00822         GO TO 4900-END-FILE.                                      EL104
00823                                                                   EL104
00824      IF EIBAID = DFHPF1                                           EL104
00825         IF CONTROLO = TX-ACCESS-CD-GENL                           EL104
00826           GO TO 4500-READNEXT.                                    EL104
00827                                                                   EL104
00828  4550-DISPLAY-SCREEN.                                             EL104
00829      MOVE TX-ACCESS-CD-GENL TO CONTROLO                           EL104
00830                                CNTL-KEY   PI-COMM-CONTROL.        EL104
00831                                                                   EL104
00832      MOVE TX-CONTROL-PRIMARY    TO OLD-KEY.                       EL104
00833      MOVE -1       TO MAINTL.                                     EL104
00834      MOVE AL-UANON TO CONTROLA.                                   EL104
00835      GO TO 8100-SEND-INITIAL-MAP.                                 EL104
00836                                                                   EL104
00837  4600-BROWSE-BKWD.                                                EL104
00838      EXEC CICS HANDLE CONDITION                                   EL104
00839           NOTFND(4900-END-FILE)                                   EL104
00840       END-EXEC.                                                   EL104
00841                                                                   EL104
00842      EXEC CICS READPREV                                           EL104
00843         DATASET (FILE-ID)                                         EL104
00844         SET     (ADDRESS OF TEXT-FILES)                              CL*10
00845         RIDFLD  (FILE-KEY)                                        EL104
00846     END-EXEC.                                                     EL104
00847                                                                   EL104
00848  4650-BROWSE-LOOP.                                                EL104
00849      IF PI-FILE-EOF                                               EL104
00850         MOVE SPACE TO PI-EOF-SW                                   EL104
00851       ELSE                                                        EL104
00852         EXEC CICS READPREV                                        EL104
00853         DATASET (FILE-ID)                                         EL104
00854         SET     (ADDRESS OF TEXT-FILES)                              CL*10
00855         RIDFLD  (FILE-KEY)                                        EL104
00856     END-EXEC.                                                     EL104
00857                                                                   EL104
020410     IF FILE-ID = HELP-ID
020410         NEXT SENTENCE
020410     ELSE
00858      IF TX-COMPANY-CD NOT = PI-COMPANY-CD                         EL104
00859         GO TO 4900-END-FILE.                                      EL104
00860                                                                   EL104
00861      GO TO 4550-DISPLAY-SCREEN.                                   EL104
00862                                                                   EL104
00863  4800-NO-RECORD.                                                  EL104
00864      MOVE -1                     TO CONTROLL.                     EL104
00865      MOVE AL-UANON               TO CONTROLA.                     EL104
00866      MOVE ER-1162                TO EMI-ERROR.                    EL104
00867      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL104
00868                                                                   EL104
00869      IF BROWSE-STARTED                                            EL104
00870        EXEC CICS ENDBR                                            EL104
00871             DATASET  (FILE-ID)                                    EL104
00872         END-EXEC.                                                 EL104
00873                                                                   EL104
00874      GO TO 8200-SEND-DATAONLY.                                    EL104
00875                                                                   EL104
00876  4900-END-FILE.                                                   EL104
00877      IF EIBAID = DFHPF1                                           EL104
00878          MOVE 'Y'                TO PI-EOF-SW                     EL104
00879          MOVE ER-2237            TO EMI-ERROR                     EL104
00880      ELSE                                                         EL104
00881          MOVE ER-2238            TO EMI-ERROR.                    EL104
00882                                                                   EL104
00883      MOVE -1                     TO MAINTL.                       EL104
00884                                                                   EL104
00885      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL104
00886                                                                   EL104
00887      IF BROWSE-STARTED                                            EL104
00888        EXEC CICS ENDBR                                            EL104
00889             DATASET  (FILE-ID)                                    EL104
00890         END-EXEC.                                                 EL104
00891                                                                   EL104
00892      MOVE SPACE TO BROWSE-STARTED-SW.                             EL104
00893      GO TO 8200-SEND-DATAONLY.                                    EL104
00894  EJECT                                                            EL104
00895  5000-DELETE-FILE.                                                EL104
00896      EXEC CICS HANDLE CONDITION                                   EL104
00897           NOTFND (3900-NOT-FOUND)                                 EL104
00898           NOTOPEN(6000-NOT-OPEN)                                  EL104
00899       END-EXEC.                                                   EL104
00900                                                                   EL104
00901      EXEC CICS DELETE                                             EL104
00902           DATASET  (FILE-ID)                                      EL104
00903           RIDFLD   (OLD-KEY-SAVE)                                 EL104
00904           KEYLENGTH(13)                                           EL104
00905           GENERIC                                                 EL104
00906       END-EXEC.                                                   EL104
00907                                                                   EL104
00908      MOVE 1          TO UPDATE-SW.                                EL104
00909      MOVE LOW-VALUES TO EL104AI.                                  EL104
00910      GO TO 8100-SEND-INITIAL-MAP.                                 EL104
00911                                                                   EL104
00912  6000-NOT-OPEN.                                                   EL104
00913      MOVE -1 TO MAINTL.                                           EL104
00914                                                                   EL104
00915      IF PI-ENTRY-CD-1 = '1'                                       EL104
00916          MOVE ER-0013 TO EMI-ERROR                                EL104
00917      ELSE                                                         EL104
00918          IF PI-ENTRY-CD-1 = '2'                                   EL104
00919              MOVE ER-0014 TO EMI-ERROR                            EL104
00920          ELSE                                                     EL104
00921              MOVE ER-0015 TO EMI-ERROR.                           EL104
00922                                                                   EL104
00923      MOVE -1           TO MAINTL.                                    CL**4
00924      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL104
00925      GO TO 8200-SEND-DATAONLY.                                    EL104
00926  EJECT                                                            EL104
00927  8100-SEND-INITIAL-MAP.                                           EL104
00928                                                                      CL**4
00929      MOVE 'Y'          TO PI-104-SCREEN-SENT-IND.                    CL**6
00930      MOVE SAVE-DATE    TO DATEO.                                  EL104
00931      MOVE EIBTIME      TO TIME-IN.                                EL104
00932      MOVE TIME-OUT     TO TIMEO.                                  EL104
00933      MOVE PI-COMPANY-ID          TO COMPAO.                          CL**7
00934                                                                      CL**7
00935      IF  NOT CREDIT-SESSION                                          CL**7
00936          MOVE SPACES             TO TRANSFMO                         CL**7
00937          MOVE AL-PANOF           TO TRANSFMA.                        CL**7
00938                                                                   EL104
00939      IF PI-PROCESSOR-ID = 'LGXX'                                  EL104
00940          MOVE AL-PANOF  TO  HDNPFAA.                              EL104
00941                                                                   EL104
00942      IF FILE-UPDATED                                              EL104
00943          MOVE ER-0000   TO EMI-ERROR                              EL104
00944          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL104
00945                                                                   EL104
00946      MOVE PI-FILETYP    TO FILETYPO.                              EL104
00947      MOVE AL-UANON      TO FILETYPA.                              EL104
00948                                                                   EL104
00949      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.                       EL104
00950      MOVE AL-SABON             TO ERRMSG1A.                       EL104
00951      MOVE EMI-MESSAGE-AREA (2) TO ERRMSG2O.                       EL104
00952      MOVE AL-SABON             TO ERRMSG2A.                       EL104
00953                                                                   EL104
00954      EXEC CICS SEND                                               EL104
00955           MAPSET(MAPSET-NAME)                                     EL104
00956           MAP   (MAP-NAME)                                        EL104
00957           FROM  (EL104AO)                                         EL104
00958           ERASE                                                   EL104
00959       END-EXEC.                                                   EL104
00960                                                                   EL104
00961      GO TO 9100-RETURN-TRAN.                                      EL104
00962                                                                   EL104
00963  8200-SEND-DATAONLY.                                              EL104
00964                                                                      CL**4
00965      IF  PI-104-SCREEN-NOT-SENT                                      CL**6
00966          GO TO 8100-SEND-INITIAL-MAP.                                CL**4
00967                                                                      CL**4
00968      MOVE EIBTIME  TO TIME-IN.                                    EL104
00969      MOVE TIME-OUT TO TIMEO.                                      EL104
00970      MOVE PI-COMPANY-ID          TO COMPAO.                          CL**7
00971                                                                   EL104
00972      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.                       EL104
00973      MOVE AL-SABON             TO ERRMSG1A.                       EL104
00974      MOVE EMI-MESSAGE-AREA (2) TO ERRMSG2O.                       EL104
00975      MOVE AL-SABON             TO ERRMSG2A.                       EL104
00976                                                                      CL**7
00977      IF  NOT CREDIT-SESSION                                          CL**7
00978          MOVE SPACES             TO TRANSFMO                         CL**7
00979          MOVE AL-PANOF           TO TRANSFMA.                        CL**7
00980                                                                   EL104
00981      IF PI-ENTRY-CD-1 NOT = '0'                                   EL104
00982          MOVE PI-FILETYP    TO FILETYPO                           EL104
00983          MOVE AL-UANON      TO FILETYPA.                          EL104
00984                                                                   EL104
00985      EXEC CICS SEND                                               EL104
00986           MAPSET(MAPSET-NAME)                                     EL104
00987           MAP   (MAP-NAME)                                        EL104
00988           DATAONLY                                                EL104
00989           CURSOR                                                  EL104
00990           FROM  (EL104AO)                                         EL104
00991       END-EXEC.                                                   EL104
00992                                                                   EL104
00993      GO TO 9100-RETURN-TRAN.                                      EL104
00994                                                                   EL104
00995  8300-SEND-TEXT.                                                  EL104
00996      EXEC CICS SEND TEXT                                          EL104
00997           FROM  (LOGOFF-TEXT)                                     EL104
00998           ERASE                                                   EL104
00999           FREEKB                                                  EL104
01000           LENGTH(LOGOFF-LENGTH)                                   EL104
01001       END-EXEC.                                                   EL104
01002                                                                   EL104
01003      EXEC CICS RETURN                                             EL104
01004       END-EXEC.                                                   EL104
01005                                                                   EL104
01006  8800-UNAUTHORIZED-ACCESS.                                        EL104
01007      MOVE UNACCESS-MSG TO LOGOFF-MSG.                             EL104
01008      GO TO 8300-SEND-TEXT.                                        EL104
01009                                                                   EL104
01010  9000-RETURN-CICS.                                                EL104
01011      MOVE EIBAID  TO PI-ENTRY-CD-1.                               EL104
01012      MOVE 'EL005' TO PGM-NAME.                                    EL104
01013      GO TO 9300-XCTL.                                             EL104
01014                                                                   EL104
01015  9100-RETURN-TRAN.                                                EL104
01016      MOVE MAP-NUMBER           TO PI-CURRENT-SCREEN-NO.           EL104
01017      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.               EL104
01018                                                                   EL104
01019      MOVE PI-COMM-CONTROL      TO PI-LAST-CONTROL.                EL104
01020                                                                   EL104
01021      EXEC CICS RETURN                                             EL104
01022           TRANSID (TRANS-ID)                                      EL104
01023           COMMAREA(PROGRAM-INTERFACE-BLOCK)                       EL104
01024           LENGTH  (PI-COMM-LENGTH)                                EL104
01025       END-EXEC.                                                   EL104
01026                                                                   EL104
01027  9200-RETURN-MAIN-MENU.                                           EL104
01028      IF CLAIM-SESSION                                             EL104
01029          MOVE XCTL-CLAIM         TO PGM-NAME                      EL104
01030      ELSE                                                         EL104
01031          IF CREDIT-SESSION                                        EL104
01032              MOVE XCTL-CREDIT    TO PGM-NAME                         CL**4
01033          ELSE                                                     EL104
01034              IF WARRANTY-SESSION                                  EL104
01035                  MOVE XCTL-WARRANTY  TO PGM-NAME                     CL**4
01036              ELSE                                                 EL104
01037                  IF MORTGAGE-SESSION                                 CL**3
01038                      MOVE XCTL-MORTGAGE      TO PGM-NAME             CL**4
01039                  ELSE                                             EL104
01040                      MOVE XCTL-GEN-LEDGER    TO PGM-NAME.         EL104
01041                                                                   EL104
01042  9300-XCTL.                                                       EL104
01043      EXEC CICS XCTL                                               EL104
01044           PROGRAM (PGM-NAME)                                      EL104
01045           COMMAREA(PROGRAM-INTERFACE-BLOCK)                       EL104
01046           LENGTH  (PI-COMM-LENGTH)                                EL104
01047       END-EXEC.                                                   EL104
01048                                                                   EL104
01049  9400-CLEAR.                                                      EL104
01050      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.                       EL104
01051      GO TO 9300-XCTL.                                             EL104
01052                                                                   EL104
01053  9500-PF12.                                                       EL104
01054      MOVE 'EL010' TO PGM-NAME.                                    EL104
01055      GO TO 9300-XCTL.                                             EL104
01056                                                                   EL104
01057  9600-PGMID-ERROR.                                                EL104
01058      EXEC CICS HANDLE CONDITION                                   EL104
01059           PGMIDERR(8300-SEND-TEXT)                                EL104
01060       END-EXEC.                                                   EL104
01061                                                                   EL104
01062      MOVE PGM-NAME     TO PI-CALLING-PROGRAM.                     EL104
01063      MOVE SPACES       TO PI-ENTRY-CD-1.                          EL104
01064      MOVE 'EL005'      TO PGM-NAME.                               EL104
01065      MOVE PGM-NAME     TO LOGOFF-PGM.                             EL104
01066      MOVE PGMIDERR-MSG TO LOGOFF-FILL.                            EL104
01067      GO TO 9300-XCTL.                                             EL104
01068                                                                   EL104
01069                                                                   EL104
01070  9700-LINK-DATE-CONVERT.                                          EL104
01071      EXEC CICS LINK                                               EL104
01072          PROGRAM    ('ELDATCV')                                   EL104
01073          COMMAREA   (DATE-CONVERSION-DATA)                        EL104
01074          LENGTH     (DC-COMM-LENGTH)                              EL104
01075      END-EXEC.                                                    EL104
01076                                                                   EL104
01077  9700-EXIT.                                                       EL104
01078      EXIT.                                                        EL104
01079                                                                   EL104
01080  9900-ERROR-FORMAT.                                               EL104
01081                                                                      CL**9
01082      MOVE PI-LANGUAGE-TYPE       TO EMI-LANGUAGE-IND.                CL**9
01083                                                                      CL**9
01084      IF NOT EMI-ERRORS-COMPLETE                                   EL104
01085         EXEC CICS LINK                                            EL104
01086              PROGRAM ('EL001')                                    EL104
01087              COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)              EL104
01088              LENGTH  (EMI-COMM-LENGTH)                            EL104
01089          END-EXEC.                                                EL104
01090                                                                   EL104
01091  9900-EXIT.                                                       EL104
01092       EXIT.                                                       EL104
01093                                                                      CL**4
01094  9910-INITIALIZE-SECURITY.                                           CL**4
01095 ******************************************************************   CL**4
01096 *                                                                *   CL**4
01097 *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *   CL**4
01098 *       USER SECURITY RECORD SET UP BY EL125.  BASED ON THE      *   CL**4
01099 *       APPLICATION NUMBER FOUND IN WORKING STORAGE UNDER        *   CL**4
01100 *       W-APPL-SECRTY-NDX (PIC  S9(04) COMP), THIS PROGRAM       *   CL**4
01101 *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *   CL**4
01102 *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *   CL**4
01103 *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *   CL**4
01104 *       ERROR CONDITION AND EXITS THE PROGRAM.                   *   CL**4
01105 *                                                                *   CL**4
01106 *       NOTE:  THE CARRIER/GRP/STATE/PRODUCER SECURITY DATA      *   CL**4
01107 *       IS ALSO PROVIDED BY THIS LOGIC.                          *   CL**4
01108 *                                                                *   CL**4
01109 ******************************************************************   CL**4
01110                                                                      CL**4
01111      IF  PI-PROCESSOR-ID NOT EQUAL 'LGXX'                            CL**4
01112          MOVE '125E'             TO SC-QUID-SYSTEM                   CL**4
01113          MOVE EIBTRMID           TO SC-QUID-TERMINAL                 CL**4
01114                                                                      CL**4
01115          EXEC CICS READQ TS                                          CL**4
01116              QUEUE  (SC-QUID-KEY)                                    CL**4
01117              INTO   (SECURITY-CONTROL-E)                             CL**4
01118              LENGTH (SC-COMM-LENGTH-E)                               CL**4
01119              ITEM   (SC-ITEM)                                        CL**4
01120          END-EXEC                                                    CL**4
01121                                                                      CL**4
01122          MOVE SC-MP-DISPLAY (W-APPL-SCRTY-NDX)                       CL**4
01123                                  TO PI-DISPLAY-CAP                   CL**4
01124          MOVE SC-MP-UPDATE (W-APPL-SCRTY-NDX)                        CL**4
01125                                  TO PI-MODIFY-CAP                    CL**4
01126                                                                      CL**4
01127          IF  NOT DISPLAY-CAP                                         CL**4
01128              MOVE 'READ'         TO SM-READ                          CL**4
01129              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT          CL**4
01130              MOVE ER-9097        TO EMI-ERROR                        CL**4
01131              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
01132              PERFORM 8100-SEND-INITIAL-MAP.                          CL**4
01133                                                                      CL**4
01134  9910-EXIT.                                                          CL**4
01135      EXIT.                                                           CL**4
01136                                                                   EL104
01137  9990-ABEND.                                                      EL104
01138      MOVE 'EL004'                TO PGM-NAME.                     EL104
01139      MOVE DFHEIBLK               TO EMI-LINE1.                    EL104
01140                                                                   EL104
01141      EXEC CICS LINK                                               EL104
01142          PROGRAM   (PGM-NAME)                                     EL104
01143          COMMAREA  (EMI-LINE1)                                    EL104
01144          LENGTH    (72)                                           EL104
01145      END-EXEC.                                                    EL104
01146                                                                   EL104
01147      GO TO 8200-SEND-DATAONLY.                                    EL104
01148                                                                   EL104
01149      GOBACK.                                                      EL104
01150                                                                   EL104
01151  9995-SECURITY-VIOLATION.                                         EL104
01152                              COPY ELCSCTP.                        EL104
01153                                                                   EL104
01154  9995-EXIT.                                                       EL104
01155      EXIT.                                                        EL104
01156                                                                   EL104
