00001  ID DIVISION.                                                     03/06/96
00002                                                                   EL171
00003  PROGRAM-ID.                 EL171.                                  LV004
00004 *              PROGRAM CONVERTED BY                                  CL**4
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**4
00006 *              CONVERSION DATE 02/13/96 09:31:11.                    CL**4
00007 *                            VMOD=2.004.                             CL**4
00008 *                                                                 EL171
00008 *                                                                 EL171
00009 *AUTHOR.     LOGIC,INC.                                              CL**4
00010 *            DALLAS, TEXAS.                                          CL**4
00011                                                                   EL171
00025 *REMARKS.    REPORT MENU.                                            CL**3
00026                                                                   EL171
012103******************************************************************
012103*                   C H A N G E   L O G
012103*
012103* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
012103*-----------------------------------------------------------------
012103*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
012103* EFFECTIVE    NUMBER
012103*-----------------------------------------------------------------
012103* 012103                   SMVA  FIX SELECT CODE FUNCTIONALITY    
121703* 121703                   SMVA  DISABLE PF11 CHECK RECON
012103******************************************************************
00027      EJECT                                                        EL171
00028  ENVIRONMENT DIVISION.                                            EL171
00029  DATA DIVISION.                                                   EL171
00030  WORKING-STORAGE SECTION.                                         EL171
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL171
00032  77  FILLER  PIC X(32)  VALUE '*    EL171 WORKING STORAGE     *'. EL171
00033  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.004 *********'.    CL**4
00034                                                                      CL**3
00035                                  COPY ELCSCTM.                       CL**3
00036                                                                   EL171
00037                                  COPY ELCSCRTY.                      CL**3
00038                                                                   EL171
00039  01  WS-DATE-AREA.                                                EL171
00040      05  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL171
00041      05  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.        EL171
00042                                                                   EL171
00043  01  MISC-WORK-AREAS.                                             EL171
00044      12  SC-ITEM                 PIC S9(4)   VALUE +0001  COMP.   EL171
00045      12  MAP-NAME                PIC X(8)    VALUE 'EL171A  '.    EL171
00046      12  MAPSET-NAME             PIC X(8)    VALUE 'EL171S  '.    EL171
00047      12  TRANS-ID                PIC X(4)    VALUE 'EX01'.        EL171
00048      12  THIS-PGM                PIC X(8)    VALUE 'EL171   '.    EL171
00049      12  PGM-NAME                PIC X(8).                        EL171
00050      12  TIME-IN                 PIC S9(7).                       EL171
00051      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL171
00052          16  FILLER              PIC X.                           EL171
00053          16  TIME-OUT            PIC 99V99.                       EL171
00054          16  FILLER              PIC X(2).                        EL171
00055      12  XCTL-005                PIC X(5)    VALUE 'EL005'.       EL171
00056      12  XCTL-010                PIC X(5)    VALUE 'EL010'.       EL171
00057      12  XCTL-172                PIC X(5)    VALUE 'EL172'.       EL171
00058      12  XCTL-173                PIC X(5)    VALUE 'EL173'.       EL171
00059      12  XCTL-174                PIC X(5)    VALUE 'EL174'.       EL171
00060      12  XCTL-175                PIC X(5)    VALUE 'EL175'.       EL171
00061      12  XCTL-176                PIC X(5)    VALUE 'EL176'.       EL171
00062      12  XCTL-178                PIC X(5)    VALUE 'EL178'.       EL171
00063      12  XCTL-179                PIC X(5)    VALUE 'EL179'.       EL171
00064      12  XCTL-180                PIC X(5)    VALUE 'EL180'.       EL171
00065      12  XCTL-181                PIC X(5)    VALUE 'EL181'.       EL171
00066      12  XCTL-183                PIC X(5)    VALUE 'EL183'.       EL171
00067      12  XCTL-126                PIC X(5)    VALUE 'EL126'.       EL171
00068      12  XCTL-146                PIC X(5)    VALUE 'EL146'.          CL**3
00069      12  LINK-001                PIC X(5)    VALUE 'EL001'.       EL171
00070      12  LINK-004                PIC X(5)    VALUE 'EL004'.       EL171
00071                                                                   EL171
00072      12  ER-0004                 PIC X(4)    VALUE '0004'.        EL171
00073      12  ER-0070                 PIC X(4)    VALUE '0070'.        EL171
00074      12  ER-7008                 PIC X(4)    VALUE '7008'.        EL171
00075      12  ER-0029                 PIC X(4)    VALUE '0029'.        EL171
00076      12  ER-2371                 PIC X(4)    VALUE '2371'.        EL171
121703     12  ER-9812                 PIC X(4)    VALUE '9812'.        EL171
00077                                                                   EL171
00078      EJECT                                                        EL171
00079                                  COPY ELCLOGOF.                      CL**3
00080                                                                   EL171
00081      EJECT                                                        EL171
00082                                  COPY ELCDATE.                       CL**3
00083                                                                      CL**3
00084      EJECT                                                           CL**3
00085                                  COPY ELCATTR.                       CL**3
00086                                                                      CL**3
00087      EJECT                                                           CL**3
00088                                  COPY ELCEMIB.                       CL**3
00089                                                                      CL**3
00090      EJECT                                                           CL**3
00091                                  COPY ELCINTF.                       CL**3
00092                                                                      CL**3
00093      EJECT                                                           CL**3
00094                                  COPY ELCAID.                        CL**3
00095                                                                   EL171
00096  01  FILLER    REDEFINES DFHAID.                                  EL171
00097      12  FILLER                  PIC X(8).                        EL171
012103     12  PF-VALUES               PIC X       OCCURS 24.           EL171
00099                                                                   EL171
00100      EJECT                                                        EL171
00101                                  COPY EL171S.                        CL**3
00102                                                                   EL171
00103      EJECT                                                        EL171
00104  LINKAGE SECTION.                                                 EL171
00105  01  DFHCOMMAREA                 PIC X(1024).                     EL171
00106                                                                   EL171
00107      EJECT                                                        EL171
00108  PROCEDURE DIVISION.                                              EL171
00109                                                                   EL171
00110      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL171
00111                                                                   EL171
00112      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL171
00113      MOVE '5'                    TO DC-OPTION-CODE.               EL171
00114      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL171
00115      MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE.                    EL171
00116      MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE.                EL171
00117                                                                   EL171
00118      IF EIBCALEN = 0                                              EL171
00119          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL171
00120                                                                   EL171
00121      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL171
00122          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL171
00123              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL171
00124              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL171
00125              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL171
00126              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL171
00127              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL171
00128              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL171
00129              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL171
00130              MOVE THIS-PGM       TO PI-CALLING-PROGRAM            EL171
00131          ELSE                                                     EL171
00132              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL171
00133              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL171
00134              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL171
00135              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL171
00136              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL171
00137              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL171
00138              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL171
00139              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL171
00140                                                                   EL171
00141      IF EIBTRNID NOT = TRANS-ID                                   EL171
00142          GO TO 8100-SEND-INITIAL-MAP.                             EL171
00143                                                                   EL171
00144      EXEC CICS HANDLE CONDITION                                   EL171
00145          PGMIDERR  (9600-PGMID-ERROR)                             EL171
00146          ERROR     (9990-ABEND)                                   EL171
00147          END-EXEC.                                                EL171
00148                                                                   EL171
00149      IF EIBAID = DFHCLEAR                                         EL171
00150          GO TO 9400-CLEAR.                                        EL171
00151                                                                   EL171
00152      IF PI-PROCESSOR-ID = 'LGXX'                                  EL171
00153          GO TO 0200-RECEIVE.                                      EL171
00154                                                                   EL171
00155      EXEC CICS READQ TS                                           EL171
00156          QUEUE   (PI-SECURITY-TEMP-STORE-ID)                      EL171
00157          INTO    (SECURITY-CONTROL)                               EL171
00158          LENGTH  (SC-COMM-LENGTH)                                 EL171
00159          ITEM    (SC-ITEM)                                        EL171
00160      END-EXEC.                                                    EL171
00161                                                                   EL171
00162      EJECT                                                        EL171
00163  0200-RECEIVE.                                                    EL171
00164      MOVE LOW-VALUES             TO EL171AI.                      EL171
00165                                                                   EL171
00166      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL171
00167          MOVE ER-7008            TO EMI-ERROR                     EL171
00168          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL171
00169          MOVE -1                 TO SELCODEL                      EL171
00170          GO TO 8200-SEND-DATAONLY.                                EL171
00171                                                                   EL171
00172      EXEC CICS RECEIVE                                            EL171
00173          MAP     (MAP-NAME)                                       EL171
00174          MAPSET  (MAPSET-NAME)                                    EL171
00175          INTO    (EL171AI)                                        EL171
00176          END-EXEC.                                                EL171
00177                                                                   EL171
00178      IF SELCODEL = 0                                              EL171
00179          GO TO 0300-CHECK-PFKEYS.                                 EL171
00180                                                                   EL171
00181      IF EIBAID NOT = DFHENTER                                     EL171
00182          MOVE ER-0004            TO EMI-ERROR                     EL171
00183          GO TO 0320-INPUT-ERROR.                                  EL171
00184                                                                   EL171
00185      IF (SELCODEI NUMERIC) AND (SELCODEI GREATER 0 AND LESS 25)      CL**2
00186          MOVE PF-VALUES (SELCODEI) TO EIBAID                      EL171
00187      ELSE                                                         EL171
00188          MOVE ER-0029            TO EMI-ERROR                     EL171
00189          GO TO 0320-INPUT-ERROR.                                  EL171
00190                                                                   EL171
00191  0300-CHECK-PFKEYS.                                               EL171
00192      MOVE ' '                    TO PI-ENTRY-CD-1.                EL171
00193                                                                   EL171
00194      IF EIBAID = DFHPF23                                          EL171
00195          GO TO 8810-PF23.                                         EL171
00196                                                                   EL171
00197      IF EIBAID = DFHPF24                                          EL171
00198          GO TO 9200-RETURN-MAIN-MENU.                             EL171
00199                                                                   EL171
00200      IF EIBAID = DFHPF12                                          EL171
00201          GO TO 9500-PF12.                                         EL171
00202                                                                   EL171
00203      IF USERCDL = ZERO                                            EL171
00204          MOVE SPACES             TO PI-PROGRAM-WORK-AREA          EL171
00205      ELSE                                                         EL171
00206          MOVE USERCDI            TO PI-PROGRAM-WORK-AREA.         EL171
00207                                                                   EL171
00208      IF EIBAID = DFHPF1                                           EL171
00209          MOVE XCTL-172           TO PGM-NAME                      EL171
00210          IF (NOT PI-NO-CARRIER-SECURITY) OR                       EL171
00211             (NOT PI-NO-ACCOUNT-SECURITY)                          EL171
00212             MOVE PI-PROCESSOR-ID TO PI-PROGRAM-WORK-AREA          EL171
00213             GO TO 9300-XCTL                                       EL171
00214          ELSE                                                     EL171
00215             GO TO 9300-XCTL.                                      EL171
00216                                                                   EL171
00217      IF EIBAID = DFHPF2                                           EL171
00218          IF PI-PROCESSOR-ID = 'LGXX'                              EL171
00219              MOVE XCTL-173                  TO PGM-NAME           EL171
00220              GO TO 9300-XCTL                                      EL171
00221          ELSE                                                     EL171
00222              IF PI-NO-ACCOUNT-SECURITY                            EL171
00223                 MOVE SC-CLAIMS-DISPLAY (17) TO  PI-DISPLAY-CAP    EL171
00224                 MOVE SC-CLAIMS-UPDATE  (17) TO  PI-MODIFY-CAP     EL171
00225                 IF NOT DISPLAY-CAP                                EL171
00226                     MOVE XCTL-173           TO  THIS-PGM          EL171
00227                     MOVE 'READ'             TO  SM-READ           EL171
00228                     PERFORM 9995-SECURITY-VIOLATION               EL171
00229                                       THRU 9995-EXIT                 CL**4
00230                     MOVE ER-0070            TO  EMI-ERROR         EL171
00231                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT      EL171
00232                     GO TO 8100-SEND-INITIAL-MAP                   EL171
00233                 ELSE                                              EL171
00234                     MOVE XCTL-173        TO PGM-NAME              EL171
00235                     GO TO 9300-XCTL                               EL171
00236              ELSE                                                 EL171
00237                  MOVE ER-2371         TO EMI-ERROR                EL171
00238                  GO TO 0320-INPUT-ERROR.                          EL171
00239                                                                   EL171
00240      IF EIBAID = DFHPF3                                           EL171
00241          IF PI-PROCESSOR-ID = 'LGXX'                              EL171
00242              MOVE XCTL-174              TO  PGM-NAME              EL171
00243              GO TO 9300-XCTL                                      EL171
00244          ELSE                                                     EL171
00245              IF PI-NO-ACCOUNT-SECURITY                            EL171
00246                  MOVE SC-CLAIMS-DISPLAY (11) TO  PI-DISPLAY-CAP   EL171
00247                  MOVE SC-CLAIMS-UPDATE  (11) TO  PI-MODIFY-CAP    EL171
00248                  IF NOT DISPLAY-CAP                               EL171
00249                      MOVE XCTL-174           TO  THIS-PGM         EL171
00250                      MOVE 'READ'             TO  SM-READ          EL171
00251                      PERFORM 9995-SECURITY-VIOLATION              EL171
00252                                              THRU 9995-EXIT          CL**4
00253                      MOVE ER-0070            TO  EMI-ERROR        EL171
00254                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL171
00255                      GO TO 8100-SEND-INITIAL-MAP                  EL171
00256                  ELSE                                             EL171
00257                      MOVE XCTL-174        TO PGM-NAME             EL171
00258                      GO TO 9300-XCTL                              EL171
00259              ELSE                                                 EL171
00260                  MOVE ER-2371         TO EMI-ERROR                EL171
00261                  GO TO 0320-INPUT-ERROR.                          EL171
00262                                                                   EL171
00263      IF EIBAID = DFHPF4                                           EL171
00264          IF PI-PROCESSOR-ID = 'LGXX'                              EL171
00265              MOVE XCTL-175                  TO  PGM-NAME          EL171
00266              GO TO 9300-XCTL                                      EL171
00267          ELSE                                                     EL171
00268              IF PI-NO-ACCOUNT-SECURITY                            EL171
00269                  MOVE SC-CLAIMS-DISPLAY (12) TO  PI-DISPLAY-CAP   EL171
00270                  MOVE SC-CLAIMS-UPDATE  (12) TO  PI-MODIFY-CAP    EL171
00271                  IF NOT DISPLAY-CAP                               EL171
00272                      MOVE XCTL-175           TO  THIS-PGM         EL171
00273                      MOVE 'READ'             TO  SM-READ          EL171
00274                      PERFORM 9995-SECURITY-VIOLATION              EL171
00275                                              THRU 9995-EXIT          CL**4
00276                      MOVE ER-0070            TO  EMI-ERROR        EL171
00277                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL171
00278                      GO TO 8100-SEND-INITIAL-MAP                  EL171
00279                  ELSE                                             EL171
00280                      MOVE XCTL-175        TO PGM-NAME             EL171
00281                      GO TO 9300-XCTL                              EL171
00282              ELSE                                                 EL171
00283                  MOVE ER-2371         TO EMI-ERROR                EL171
00284                  GO TO 0320-INPUT-ERROR.                          EL171
00285                                                                   EL171
00286      IF EIBAID = DFHPF5                                           EL171
00287          IF PI-PROCESSOR-ID = 'LGXX'                              EL171
00288              MOVE XCTL-176              TO  PGM-NAME              EL171
00289              GO TO 9300-XCTL                                      EL171
00290          ELSE                                                     EL171
00291              IF PI-NO-ACCOUNT-SECURITY                            EL171
00292                  MOVE SC-CLAIMS-DISPLAY (13) TO  PI-DISPLAY-CAP   EL171
00293                  MOVE SC-CLAIMS-UPDATE  (13) TO  PI-MODIFY-CAP    EL171
00294                  IF NOT DISPLAY-CAP                               EL171
00295                      MOVE XCTL-176           TO  THIS-PGM         EL171
00296                      MOVE 'READ'             TO  SM-READ          EL171
00297                      PERFORM 9995-SECURITY-VIOLATION              EL171
00298                                              THRU 9995-EXIT          CL**4
00299                      MOVE ER-0070            TO  EMI-ERROR        EL171
00300                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL171
00301                      GO TO 8100-SEND-INITIAL-MAP                  EL171
00302                  ELSE                                             EL171
00303                      MOVE XCTL-176        TO PGM-NAME             EL171
00304                      GO TO 9300-XCTL                              EL171
00305              ELSE                                                 EL171
00306                  MOVE ER-2371         TO EMI-ERROR                EL171
00307                  GO TO 0320-INPUT-ERROR.                          EL171
00308                                                                   EL171
00309      IF EIBAID = DFHPF6                                           EL171
00310          IF PI-PROCESSOR-ID = 'LGXX'                              EL171
00311              MOVE XCTL-178                   TO  PGM-NAME         EL171
00312              GO TO 9300-XCTL                                      EL171
00313          ELSE                                                     EL171
00314              IF PI-NO-CARRIER-SECURITY AND PI-NO-ACCOUNT-SECURITY EL171
00315                  MOVE SC-CLAIMS-DISPLAY (20) TO  PI-DISPLAY-CAP   EL171
00316                  MOVE SC-CLAIMS-UPDATE  (20) TO  PI-MODIFY-CAP    EL171
00317                  IF NOT DISPLAY-CAP                               EL171
00318                      MOVE XCTL-178           TO  THIS-PGM         EL171
00319                      MOVE 'READ'             TO  SM-READ          EL171
00320                      PERFORM 9995-SECURITY-VIOLATION              EL171
00321                                              THRU 9995-EXIT          CL**4
00322                      MOVE ER-0070            TO  EMI-ERROR        EL171
00323                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL171
00324                      GO TO 8100-SEND-INITIAL-MAP                  EL171
00325                  ELSE                                             EL171
00326                      MOVE XCTL-178           TO PGM-NAME          EL171
00327                      GO TO 9300-XCTL                              EL171
00328              ELSE                                                 EL171
00329                  MOVE ER-2371                TO EMI-ERROR         EL171
00330                  GO TO 0320-INPUT-ERROR.                          EL171
00331                                                                   EL171
00332      IF EIBAID = DFHPF7                                           EL171
00333          IF PI-PROCESSOR-ID = 'LGXX'                              EL171
00334              MOVE XCTL-181               TO  PGM-NAME             EL171
00335              GO TO 9300-XCTL                                      EL171
00336          ELSE                                                     EL171
00337              MOVE SC-CLAIMS-DISPLAY (18) TO  PI-DISPLAY-CAP       EL171
00338              MOVE SC-CLAIMS-UPDATE  (18) TO  PI-MODIFY-CAP        EL171
00339              IF NOT DISPLAY-CAP                                   EL171
00340                  MOVE XCTL-181           TO  THIS-PGM             EL171
00341                  MOVE 'READ'             TO  SM-READ              EL171
00342                  PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT      CL**4
00343                  MOVE ER-0070            TO  EMI-ERROR            EL171
00344                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL171
00345                  GO TO 8100-SEND-INITIAL-MAP                      EL171
00346              ELSE                                                 EL171
00347                  MOVE XCTL-181           TO PGM-NAME              EL171
00348                  GO TO 9300-XCTL.                                 EL171
00349                                                                   EL171
00350      IF EIBAID = DFHPF8                                           EL171
00351          IF PI-PROCESSOR-ID = 'LGXX'                              EL171
00352              MOVE XCTL-180                  TO PGM-NAME           EL171
00353              GO TO 9300-XCTL                                      EL171
00354          ELSE                                                     EL171
00355              IF PI-NO-CARRIER-SECURITY AND PI-NO-ACCOUNT-SECURITY EL171
00356                  MOVE SC-CLAIMS-DISPLAY (19) TO  PI-DISPLAY-CAP   EL171
00357                  MOVE SC-CLAIMS-UPDATE  (19) TO  PI-MODIFY-CAP    EL171
00358                  IF NOT DISPLAY-CAP                               EL171
00359                      MOVE XCTL-180           TO  THIS-PGM         EL171
00360                      MOVE 'READ'             TO  SM-READ          EL171
00361                      PERFORM 9995-SECURITY-VIOLATION              EL171
00362                                              THRU 9995-EXIT          CL**4
00363                      MOVE ER-0070            TO  EMI-ERROR        EL171
00364                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL171
00365                      GO TO 8100-SEND-INITIAL-MAP                  EL171
00366                  ELSE                                             EL171
00367                      MOVE XCTL-180        TO PGM-NAME             EL171
00368                      GO TO 9300-XCTL                              EL171
00369              ELSE                                                 EL171
00370                  MOVE ER-2371         TO EMI-ERROR                EL171
00371                  GO TO 0320-INPUT-ERROR.                          EL171
00372                                                                   EL171
00373      IF EIBAID = DFHPF9                                           EL171
00374          IF PI-PROCESSOR-ID IS EQUAL TO 'LGXX'                       CL**3
00375              MOVE XCTL-179                   TO  PGM-NAME            CL**3
00376              GO TO 9300-XCTL                                         CL**3
00377          ELSE                                                     EL171
00378              IF PI-NO-CARRIER-SECURITY AND PI-NO-ACCOUNT-SECURITY    CL**3
00379                  MOVE SC-CLAIMS-DISPLAY (27) TO  PI-DISPLAY-CAP      CL**3
00380                  MOVE SC-CLAIMS-UPDATE  (27) TO  PI-MODIFY-CAP       CL**3
00381                  IF NOT DISPLAY-CAP                                  CL**3
00382                      MOVE XCTL-179           TO  PGM-NAME            CL**3
00383                      MOVE 'READ'             TO  SM-READ             CL**3
00384                      PERFORM 9995-SECURITY-VIOLATION                 CL**3
00385                                              THRU 9995-EXIT          CL**4
00386                      MOVE ER-0070            TO  EMI-ERROR           CL**3
00387                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT        CL**3
00388                      GO TO 8100-SEND-INITIAL-MAP                     CL**3
00389                  ELSE                                                CL**3
00390                      MOVE XCTL-179           TO  PGM-NAME            CL**3
00391                      GO TO 9300-XCTL                                 CL**3
00392              ELSE                                                    CL**3
00393                  MOVE ER-2371                TO  EMI-ERROR           CL**3
00394                  GO TO 0320-INPUT-ERROR.                             CL**3
00395                                                                   EL171
00396      IF EIBAID = DFHPF10                                          EL171
00397          IF PI-PROCESSOR-ID = 'LGXX'                                 CL**3
00398              MOVE XCTL-183                   TO  PGM-NAME            CL**3
00399              GO TO 9300-XCTL                                         CL**3
00400          ELSE                                                     EL171
00401              IF PI-NO-CARRIER-SECURITY AND PI-NO-ACCOUNT-SECURITY    CL**3
00402                  MOVE SC-CLAIMS-DISPLAY (26) TO  PI-DISPLAY-CAP      CL**3
00403                  MOVE SC-CLAIMS-UPDATE  (26) TO  PI-MODIFY-CAP       CL**3
00404                  IF NOT DISPLAY-CAP                                  CL**3
00405                      MOVE XCTL-183           TO  THIS-PGM            CL**3
00406                      MOVE 'READ'             TO  SM-READ             CL**3
00407                      PERFORM 9995-SECURITY-VIOLATION                 CL**3
00408                                             THRU 9995-EXIT           CL**4
00409                      MOVE ER-0070            TO  EMI-ERROR           CL**3
00410                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT        CL**3
00411                      GO TO 8100-SEND-INITIAL-MAP                     CL**3
00412                  ELSE                                                CL**3
00413                      MOVE XCTL-183           TO PGM-NAME             CL**3
00414                      GO TO 9300-XCTL                                 CL**3
00415              ELSE                                                    CL**3
00416                  MOVE ER-2371                TO EMI-ERROR            CL**3
00417                  GO TO 0320-INPUT-ERROR.                             CL**3
00418                                                                      CL**3
00419      IF EIBAID = DFHPF11                                             CL**3
121703         MOVE ER-9812                       TO EMI-ERROR
121703         PERFORM 9900-ERROR-FORMAT          THRU 9900-EXIT  
121703         GO TO 8100-SEND-INITIAL-MAP                       
121703     END-IF.
012103*        IF PI-PROCESSOR-ID = 'LGXX'                                 CL**3
012103*            MOVE XCTL-146                  TO PGM-NAME              CL**3
012103*            GO TO 9300-XCTL                                         CL**3
012103*        ELSE                                                        CL**3
121703*            MOVE SC-CLAIMS-DISPLAY (23)    TO  PI-DISPLAY-CAP       CL**3
121703*            MOVE SC-CLAIMS-UPDATE  (23)    TO  PI-MODIFY-CAP        CL**3
121703*            IF NOT DISPLAY-CAP                                      CL**3
121703*                MOVE XCTL-146              TO  THIS-PGM             CL**3
121703*                MOVE 'READ'                TO  SM-READ              CL**3
121703*                PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT      CL**4
121703*                MOVE ER-0070               TO  EMI-ERROR            CL**3
121703*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**3
121703*                GO TO 8100-SEND-INITIAL-MAP                         CL**3
121703*            ELSE                                                    CL**3
121703*                MOVE XCTL-146              TO PGM-NAME              CL**3
121703*                GO TO 9300-XCTL.                                    CL**3
00436                                                                   EL171
00437      MOVE ER-0029                TO EMI-ERROR.                    EL171
00438                                                                   EL171
00439                                                                   EL171
00440      EJECT                                                        EL171
00441  0320-INPUT-ERROR.                                                EL171
00442      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL171
00443      MOVE AL-UNBON               TO SELCODEA.                     EL171
00444      MOVE -1                     TO SELCODEL.                     EL171
00445      GO TO 8200-SEND-DATAONLY.                                    EL171
00446                                                                   EL171
00447                                                                   EL171
00448                                                                   EL171
00449  8100-SEND-INITIAL-MAP.                                           EL171
00450      MOVE LOW-VALUES             TO EL171AO.                      EL171
00451      MOVE EIBTIME                TO TIME-IN.                      EL171
00452      MOVE TIME-OUT               TO RUNTIMEO.                     EL171
00453      MOVE SAVE-DATE              TO RUNDTEO.                      EL171
00454      MOVE -1                     TO SELCODEL.                     EL171
00455      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                      EL171
00456                                                                   EL171
00457      EXEC CICS SEND                                               EL171
00458          MAP     (MAP-NAME)                                       EL171
00459          MAPSET  (MAPSET-NAME)                                    EL171
00460          FROM    (EL171AO)                                        EL171
00461          ERASE                                                    EL171
00462          CURSOR                                                   EL171
00463          END-EXEC.                                                EL171
00464                                                                   EL171
00465      GO TO 9100-RETURN-TRAN.                                      EL171
00466                                                                   EL171
00467                                                                   EL171
00468      EJECT                                                        EL171
00469  8200-SEND-DATAONLY.                                              EL171
00470      MOVE SAVE-DATE              TO RUNDTEO.                      EL171
00471      MOVE EIBTIME                TO TIME-IN.                      EL171
00472      MOVE TIME-OUT               TO RUNTIMEO.                     EL171
00473      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                      EL171
00474                                                                   EL171
00475      EXEC CICS SEND                                               EL171
00476          MAP     (MAP-NAME)                                       EL171
00477          MAPSET  (MAPSET-NAME)                                    EL171
00478          FROM    (EL171AO)                                        EL171
00479          DATAONLY                                                 EL171
00480          CURSOR                                                   EL171
00481          END-EXEC.                                                EL171
00482                                                                   EL171
00483      GO TO 9100-RETURN-TRAN.                                      EL171
00484                                                                   EL171
00485                                                                   EL171
00486                                                                   EL171
00487  8300-SEND-TEXT.                                                  EL171
00488                                                                   EL171
00489      EXEC CICS SEND TEXT                                          EL171
00490          FROM    (LOGOFF-TEXT)                                    EL171
00491          LENGTH  (LOGOFF-LENGTH)                                  EL171
00492          ERASE                                                    EL171
00493          FREEKB                                                   EL171
00494          END-EXEC.                                                EL171
00495                                                                   EL171
00496      EXEC CICS RETURN                                             EL171
00497          END-EXEC.                                                EL171
00498                                                                   EL171
00499                                                                   EL171
00500                                                                   EL171
00501  8800-UNAUTHORIZED-ACCESS.                                        EL171
00502      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL171
00503      GO TO 8300-SEND-TEXT.                                        EL171
00504                                                                   EL171
00505                                                                   EL171
00506                                                                   EL171
00507  8810-PF23.                                                       EL171
00508      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL171
00509      MOVE XCTL-005               TO PGM-NAME.                     EL171
00510      GO TO 9300-XCTL.                                             EL171
00511                                                                   EL171
00512      EJECT                                                        EL171
00511                                                                   EL171
CIDMOD 9000-RETURN-CICS.                                                     000
CIDMOD     EXEC CICS RETURN                                                  000
CIDMOD         END-EXEC.                                                     000
CIDMOD                                                                       000
00513                                                                   EL171
00514  9100-RETURN-TRAN.                                                EL171
00515      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL171
00516      MOVE '171A'                 TO PI-CURRENT-SCREEN-NO.         EL171
00517                                                                   EL171
00518      EXEC CICS RETURN                                             EL171
00519          TRANSID   (TRANS-ID)                                     EL171
00520          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL171
00521          LENGTH    (PI-COMM-LENGTH)                               EL171
00522          END-EXEC.                                                EL171
00523                                                                   EL171
00524                                                                   EL171
00525                                                                   EL171
00526  9200-RETURN-MAIN-MENU.                                           EL171
00527      MOVE XCTL-126               TO PGM-NAME.                     EL171
00528      GO TO 9300-XCTL.                                             EL171
00529                                                                   EL171
00530                                                                   EL171
00531                                                                   EL171
00532  9300-XCTL.                                                       EL171
00533      MOVE SPACES                 TO PI-CONTROL-IN-PROGRESS.       EL171
00534      MOVE SPACES                 TO PI-ENTRY-CD-2.                EL171
00535      MOVE SPACES                 TO PI-RETURN-CODES.              EL171
00536      MOVE SPACES                 TO PI-UPDATE-BY.                 EL171
00537      MOVE ZEROS                  TO PI-UPDATE-HHMMSS.             EL171
00538      MOVE SPACES                 TO PI-PROGRAM-CONTROLS.          EL171
00539                                                                   EL171
00540      EXEC CICS XCTL                                               EL171
00541          PROGRAM   (PGM-NAME)                                     EL171
00542          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL171
00543          LENGTH  (PI-COMM-LENGTH)                                 EL171
00544          END-EXEC.                                                EL171
00545                                                                   EL171
00546                                                                   EL171
00547                                                                   EL171
00548  9400-CLEAR.                                                      EL171
00549      MOVE XCTL-126               TO PGM-NAME.                     EL171
00550      GO TO 9300-XCTL.                                             EL171
00551                                                                   EL171
00552                                                                   EL171
00553                                                                   EL171
00554  9500-PF12.                                                       EL171
00555      MOVE XCTL-010               TO PGM-NAME.                     EL171
00556      GO TO 9300-XCTL.                                             EL171
00557                                                                   EL171
00558                                                                   EL171
00559      EJECT                                                        EL171
00560  9600-PGMID-ERROR.                                                EL171
00561      EXEC CICS HANDLE CONDITION                                   EL171
00562          PGMIDERR  (8300-SEND-TEXT)                               EL171
00563          END-EXEC.                                                EL171
00564                                                                   EL171
00565      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL171
00566      MOVE ' '                    TO PI-ENTRY-CD-1.                EL171
00567      MOVE XCTL-005               TO PGM-NAME.                     EL171
00568      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL171
00569      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL171
00570      GO TO 9300-XCTL.                                             EL171
00571                                                                   EL171
00572                                                                   EL171
00573  9700-LINK-DATE-CONVERT.                                          EL171
00574      EXEC CICS LINK                                               EL171
00575          PROGRAM    ('ELDATCV')                                   EL171
00576          COMMAREA   (DATE-CONVERSION-DATA)                        EL171
00577          LENGTH     (DC-COMM-LENGTH)                              EL171
00578          END-EXEC.                                                EL171
00579                                                                   EL171
00580  9700-EXIT.                                                       EL171
00581      EXIT.                                                        EL171
00582                                                                   EL171
00583                                                                   EL171
00584  9900-ERROR-FORMAT.                                               EL171
00585      IF NOT EMI-ERRORS-COMPLETE                                   EL171
00586          MOVE LINK-001           TO PGM-NAME                      EL171
00587          EXEC CICS LINK                                           EL171
00588              PROGRAM   (PGM-NAME)                                 EL171
00589              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL171
00590              LENGTH    (EMI-COMM-LENGTH)                          EL171
00591              END-EXEC.                                            EL171
00592                                                                   EL171
00593  9900-EXIT.                                                       EL171
00594      EXIT.                                                        EL171
00595                                                                   EL171
00596                                                                   EL171
00597  9990-ABEND.                                                      EL171
00598      MOVE LINK-004               TO PGM-NAME.                     EL171
00599      MOVE DFHEIBLK               TO EMI-LINE1.                    EL171
00600                                                                   EL171
00601      EXEC CICS LINK                                               EL171
00602          PROGRAM   (PGM-NAME)                                     EL171
00603          COMMAREA  (EMI-LINE1)                                    EL171
00604          LENGTH    (72)                                           EL171
00605          END-EXEC.                                                EL171
00606                                                                   EL171
00607      GO TO 8200-SEND-DATAONLY.                                    EL171
00608                                                                   EL171
00609  9995-SECURITY-VIOLATION.                                         EL171
00610                              COPY ELCSCTP.                        EL171
00611                                                                   EL171
00612  9995-EXIT.                                                       EL171
00613      EXIT.                                                        EL171
00614                                                                   EL171
CIDMOD 9999-LAST-PARAGRAPH.                                                  000
CIDMOD     GOBACK.                                                           000
