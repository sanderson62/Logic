00001  ID DIVISION.                                                     10/07/97
00002                                                                   EL631
00003  PROGRAM-ID.                 EL631.                                  LV021
00004 *              PROGRAM CONVERTED BY                                  CL*19
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*19
00006 *              CONVERSION DATE 04/20/94 16:44:20.                    CL*19
00007 *                            VMOD=2.021.                             CL*21
00008 *                                                                 EL631
00009 *AUTHOR.     LOGIC,INC.                                              CL*19
00010 *            DALLAS, TEXAS.                                          CL*19
00011                                                                   EL631
00012 *DATE-COMPILED.                                                      CL*19
00013 *SECURITY.   *****************************************************   CL*19
00014 *            *                                                   *   CL*19
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*19
00016 *            *                                                   *   CL*19
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*19
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*19
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*19
00020 *            *                                                   *   CL*19
00021 *            *****************************************************   CL*19
00022                                                                   EL631
00023 *REMARKS.    TRANSACTION - EXB0 - NEW BUSINESS                       CL*19
00024 *                                 REVIEW AND CORRECTION              CL*19
00025                                                                   EL631
020816******************************************************************
020816*                   C H A N G E   L O G
020816*
020816* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
020816*-----------------------------------------------------------------
020816*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
020816* EFFECTIVE    NUMBER
020816*-----------------------------------------------------------------
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
      ******************************************************************
00026  ENVIRONMENT DIVISION.                                            EL631
00027                                                                   EL631
00028      EJECT                                                        EL631
00029  DATA DIVISION.                                                   EL631
00030  WORKING-STORAGE SECTION.                                         EL631
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL631
00032  77  FILLER  PIC X(32)  VALUE '*    EL631   WORKING STORAGE   *'.    CL*14
00033  77  FILLER  PIC X(32)  VALUE '************VMOD=2.021 *********'.    CL*21
00034                                                                   EL631
00035      COPY ELCSCTM.                                                   CL*13
00036      COPY ELCSCRTY.                                                  CL*13
00037                                                                   EL631
00038     EJECT                                                         EL631
00039                                                                   EL631
00040  01  WS-DATE-AREA.                                                EL631
00041      05  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL631
00042      05  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.        EL631
00043                                                                   EL631
00044  01  STANDARD-AREAS.                                              EL631
00045      12  SC-ITEM                 PIC S9(4) COMP VALUE +1.         EL631
00046      12  MAP-NAME                PIC X(8)    VALUE 'EL631A'.      EL631
00047      12  MAPSET-NAME             PIC X(8)    VALUE 'EL631S'.      EL631
00048      12  SCREEN-NUMBER           PIC X(4)    VALUE '631A'.        EL631
00049      12  TRANS-ID                PIC X(4)    VALUE 'EXB0'.        EL631
00050      12  THIS-PGM                PIC X(8)    VALUE 'EL631'.       EL631
00051      12  PGM-NAME                PIC X(8).                        EL631
00052      12  TIME-IN                 PIC S9(7).                       EL631
00053      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL631
00054          16  FILLER              PIC X.                           EL631
00055          16  TIME-OUT            PIC 99V99.                       EL631
00056          16  FILLER              PIC XX.                             CL*10
00057      12  XCTL-005                PIC X(8)    VALUE 'EL005'.       EL631
00058      12  XCTL-010                PIC X(8)    VALUE 'EL010'.       EL631
00059      12  XCTL-626                PIC X(8)    VALUE 'EL626'.       EL631
00060      12  XCTL-6311               PIC X(8)    VALUE 'EL6311'.      EL631
00061      12  XCTL-630                PIC X(8)    VALUE 'EL630'.       EL631
00062      12  XCTL-656                PIC X(8)    VALUE 'EL656'.          CL*15
00063      12  XCTL-663                PIC X(8)    VALUE 'EL663'.          CL*10
00064      12  LINK-001                PIC X(8)    VALUE 'EL001'.       EL631
00065      12  LINK-004                PIC X(8)    VALUE 'EL004'.       EL631
00066      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     EL631
00067      12  FILE-ID                 PIC X(8)    VALUE SPACES.        EL631
00068      12  ERPNDB-FILE-ID          PIC X(8)    VALUE 'ERPNDB'.      EL631
00069      12  ERPNDB3-FILE-ID         PIC X(8)    VALUE 'ERPNDB3'.     EL631
00070      12  ERACCT2-FILE-ID         PIC X(8)    VALUE 'ERACCT2'.     EL631
00071                                                                   EL631
00072      EJECT                                                        EL631
00073                                                                   EL631
00074  01  ERROR-MESSAGES.                                              EL631
00075      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL631
00076      12  ER-0008                 PIC X(4)  VALUE '0008'.          EL631
00077      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL631
00078      12  ER-0033                 PIC X(4)  VALUE '0033'.          EL631
00079      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL631
00080      12  ER-0144                 PIC X(4)  VALUE '0144'.          EL631
00081      12  ER-0179                 PIC X(4)  VALUE '0179'.          EL631
00082      12  ER-0193                 PIC X(4)  VALUE '0193'.          EL631
00083      12  ER-0195                 PIC X(4)  VALUE '0195'.          EL631
00084      12  ER-0329                 PIC X(4)  VALUE '0329'.          EL631
00085      12  ER-0348                 PIC X(4)  VALUE '0348'.          EL631
00086      12  ER-2059                 PIC X(4)  VALUE '2059'.          EL631
00087      12  ER-2150                 PIC X(4)  VALUE '2150'.          EL631
00088      12  ER-2190                 PIC X(4)  VALUE '2190'.          EL631
00089      12  ER-2191                 PIC X(4)  VALUE '2191'.          EL631
00090      12  ER-2192                 PIC X(4)  VALUE '2192'.          EL631
00091      12  ER-2193                 PIC X(4)  VALUE '2193'.          EL631
00092      12  ER-2201                 PIC X(4)  VALUE '2201'.             CL**6
00093      12  ER-2314                 PIC X(4)  VALUE '2314'.          EL631
00094      12  ER-2372                 PIC X(4)  VALUE '2372'.          EL631
00095      12  ER-2375                 PIC X(4)  VALUE '2375'.          EL631
00096      12  ER-2377                 PIC X(4)  VALUE '2377'.          EL631
00097      12  ER-2971                 PIC X(4)  VALUE '2971'.             CL*14
00098                                                                   EL631
00099      12  DEEDIT-FIELD            PIC X(15).                       EL631
00100      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).     EL631
00101                                                                   EL631
00102      12  CLIENT-MIC              PIC X(3)  VALUE 'MIC'.           EL631
00103      12  CLIENT-PEM              PIC X(3)  VALUE 'PEM'.           EL631
00104      12  CLIENT-CSO              PIC X(3)  VALUE 'CSO'.           EL631
00105      12  CLIENT-FLA              PIC X(3)  VALUE 'FLA'.              CL*12
00106                                                                   EL631
00107      12  WS-ACCT-KEY.                                             EL631
00108          16  FILLER              PIC X(20).                       EL631
00109          16  WS-EFF-DT           PIC XX.                          EL631
00110                                                                   EL631
00111      12  RETURN-FROM             PIC X(8).                        EL631
00112      12  QID.                                                     EL631
00113          16  QID-TERM            PIC X(4).                        EL631
00114          16  FILLER              PIC X(4)    VALUE '631A'.        EL631
00115      EJECT                                                        EL631
00116      COPY ELCDATE.                                                   CL*13
00117      EJECT                                                        EL631
00118      COPY ELCLOGOF.                                                  CL*13
00119      EJECT                                                        EL631
00120      COPY ELCATTR.                                                   CL*13
00121      EJECT                                                        EL631
00122      COPY ELCEMIB.                                                   CL*13
00123      EJECT                                                        EL631
00124      COPY ELCINTF.                                                   CL*13
00125      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                      CL*13
00126      COPY ELC631PI.                                               EL631
00127          16  FILLER              PIC X(94).                          CL*20
00128                                                                   EL631
00129      EJECT                                                        EL631
00130                                                                   EL631
00131      COPY ELCAID.                                                    CL*13
00132  01  FILLER    REDEFINES DFHAID.                                  EL631
00133      12  FILLER                  PIC X(8).                        EL631
00134      12  PF-VALUES               PIC X       OCCURS 24 TIMES.     EL631
00135                                                                   EL631
00136      EJECT                                                        EL631
00137                                                                   EL631
00138      COPY EL631S.                                                    CL*13
00139                                                                   EL631
00140      EJECT                                                        EL631
00141                                                                   EL631
00142  LINKAGE SECTION.                                                 EL631
00143  01  DFHCOMMAREA                 PIC X(1300).                        CL**9
00144                                                                   EL631
00145 *01 PARMLIST .                                                       CL*19
00146 *    02  FILLER                  PIC S9(8)   COMP.                   CL*19
00147 *    02  ERPNDB-POINTER          PIC S9(8)   COMP.                   CL*19
00148 *    02  ERACCT-POINTER          PIC S9(8)   COMP.                   CL*19
00149                                                                   EL631
00150      EJECT                                                           CL*13
00151      COPY ERCPNDB.                                                   CL*13
00152      EJECT                                                        EL631
00153      COPY ERCACCT.                                                   CL*13
00154      EJECT                                                        EL631
00155  PROCEDURE DIVISION.                                              EL631
00156                                                                   EL631
00157      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL631
00158      MOVE '5'                   TO DC-OPTION-CODE.                EL631
00159      PERFORM 9700-DATE-LINK.                                      EL631
00160      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL631
00161      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL631
00162                                                                   EL631
00163      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL631
00164      MOVE 2                      TO EMI-NUMBER-OF-LINES.          EL631
00165      MOVE EIBTRMID               TO QID-TERM.                     EL631
00166                                                                   EL631
00167      IF EIBCALEN = 0                                              EL631
00168          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL631
00169                                                                   EL631
00170      MOVE LOW-VALUES             TO EL631AI.                      EL631
00171                                                                   EL631
00172      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL631
00173          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL631
00174              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL631
00175              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL631
00176              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL631
00177              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL631
00178              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL631
00179              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL631
00180              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL631
00181              MOVE THIS-PGM             TO PI-CALLING-PROGRAM         CL*10
CIDMOD             IF PI-COMPANY-ID = CLIENT-MIC OR CLIENT-PEM OR            000
CIDMOD                                CLIENT-FLA                             000
00183                 MOVE 'Y'               TO ORGINALI                EL631
CIDMOD             END-IF                                               EL631
00186          ELSE                                                     EL631
00187              MOVE PI-CALLING-PROGRAM   TO RETURN-FROM             EL631
00188              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL631
00189              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL631
00190              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL631
00191              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL631
00192              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL631
00193              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL631
00194              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL631
00195              MOVE SPACES               TO PI-SAVED-PROGRAM-6      EL631
00196              PERFORM 6000-BUILD-SCREEN THRU 6099-EXIT.            EL631
00197                                                                   EL631
00198 ******************************************************************EL631
00199 *                                                                *EL631
00200 *        IF THERE WERE NO RECORDS FOUND FOR CARRIER AND ACCOUNT  *EL631
00201 *        DISPLAY ORIGINAL SCREEN.                                *EL631
00202 *                                                                *EL631
00203 ******************************************************************EL631
00204                                                                   EL631
00205      IF PI-NO-PB-RECS-FOUND                                       EL631
00206          MOVE SPACE TO PI-BROWSE-SW                                  CL*10
00207          GO TO 8100-SEND-INITIAL-MAP.                                CL*10
00208                                                                   EL631
00209      IF EIBTRNID NOT = TRANS-ID                                   EL631
00210          GO TO 8100-SEND-INITIAL-MAP.                             EL631
00211                                                                   EL631
00212      EXEC CICS HANDLE CONDITION                                   EL631
00213          PGMIDERR  (9600-PGMID-ERROR)                             EL631
00214          ERROR     (9990-ABEND)                                   EL631
00215      END-EXEC.                                                    EL631
00216                                                                   EL631
00217      IF EIBAID = DFHCLEAR                                         EL631
00218          GO TO 9400-CLEAR.                                        EL631
00219                                                                   EL631
00220      IF PI-PROCESSOR-ID = 'LGXX'                                  EL631
00221          GO TO 0200-RECEIVE.                                      EL631
00222                                                                   EL631
00223      EXEC CICS READQ TS                                           EL631
00224          QUEUE  (PI-SECURITY-TEMP-STORE-ID)                       EL631
00225          INTO   (SECURITY-CONTROL)                                EL631
00226          LENGTH (SC-COMM-LENGTH)                                  EL631
00227          ITEM   (SC-ITEM)                                         EL631
00228      END-EXEC.                                                    EL631
00229                                                                   EL631
00230      MOVE SC-CREDIT-DISPLAY (12)  TO PI-DISPLAY-CAP.              EL631
00231      MOVE SC-CREDIT-UPDATE  (12)  TO PI-MODIFY-CAP.               EL631
00232                                                                   EL631
00233      IF NOT DISPLAY-CAP                                           EL631
00234          MOVE 'READ'          TO SM-READ                          EL631
00235          PERFORM 9995-SECURITY-VIOLATION                          EL631
00236          MOVE ER-0070         TO  EMI-ERROR                       EL631
00237          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL631
00238          GO TO 8100-SEND-INITIAL-MAP.                             EL631
00239                                                                   EL631
00240      EJECT                                                        EL631
00241                                                                   EL631
00242  0200-RECEIVE.                                                    EL631
00243      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL631
00244          MOVE ER-0008            TO EMI-ERROR                     EL631
00245          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL631
00246          MOVE -1                 TO BATCHL                        EL631
00247          GO TO 8200-SEND-DATAONLY.                                EL631
00248                                                                   EL631
00249      EXEC CICS RECEIVE                                            EL631
00250          MAP      (MAP-NAME)                                      EL631
00251          MAPSET   (MAPSET-NAME)                                   EL631
00252          INTO     (EL631AI)                                       EL631
00253      END-EXEC.                                                    EL631
00254                                                                   EL631
00255      IF PFENTERL = 0                                              EL631
00256          GO TO 0300-CHECK-PFKEYS.                                 EL631
00257                                                                   EL631
00258      IF EIBAID NOT = DFHENTER                                     EL631
00259          MOVE ER-0004            TO EMI-ERROR                     EL631
00260          GO TO 0320-INPUT-ERROR.                                  EL631
00261                                                                   EL631
00262      IF PFENTERI NUMERIC                                             CL**5
00263         IF PFENTERI GREATER 0 AND LESS 25                            CL**5
00264            MOVE PF-VALUES (PFENTERI) TO EIBAID                       CL**5
00265         ELSE                                                         CL**5
00266            MOVE ER-0029            TO EMI-ERROR                      CL**5
00267            GO TO 0320-INPUT-ERROR.                                   CL**5
00268                                                                   EL631
00269  0300-CHECK-PFKEYS.                                               EL631
00270      IF EIBAID = DFHPF1 OR DFHPF2 OR DFHPF3                          CL*15
00271         GO TO 330-EDIT-INPUT-DATA.                                EL631
00272                                                                   EL631
00273      IF EIBAID = DFHPF23                                          EL631
00274          GO TO 8810-PF23.                                         EL631
00275                                                                   EL631
00276      IF EIBAID = DFHPF24                                          EL631
00277          GO TO 9200-RETURN-MAIN-MENU.                             EL631
00278                                                                   EL631
00279      IF EIBAID = DFHPF12                                          EL631
00280          GO TO 9500-PF12.                                         EL631
00281                                                                   EL631
00282      IF EIBAID = DFHENTER                                         EL631
00283          GO TO 330-EDIT-INPUT-DATA.                               EL631
00284                                                                   EL631
00285      MOVE ER-0029                TO EMI-ERROR.                    EL631
00286                                                                   EL631
00287  0320-INPUT-ERROR.                                                EL631
00288      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL631
00289      MOVE AL-UNBON               TO PFENTERA.                     EL631
00290                                                                   EL631
00291      IF PFENTERL = 0                                              EL631
00292          MOVE -1                 TO BATCHL                        EL631
00293      ELSE                                                         EL631
00294          MOVE -1                 TO PFENTERL.                     EL631
00295                                                                      CL*10
00296      GO TO 8200-SEND-DATAONLY.                                    EL631
00297                                                                   EL631
00298      EJECT                                                        EL631
00299                                                                   EL631
00300  330-EDIT-INPUT-DATA.                                             EL631
00301      MOVE SPACES                 TO PI-PROGRAM-WORK-AREA.            CL*10
           MOVE PI-ENTRY-CD-1          TO PI-CSR-SESSION-SW
00302      MOVE LOW-VALUES             TO PI-ERPNDB-KEY                 EL631
00303                                     PI-ERPNDB-ALT-KEY.            EL631
00304                                                                   EL631
00305      MOVE PI-COMPANY-CD          TO PI-PB-COMPANY-CD              EL631
00306                                     PI-PB-COMPANY-CD-A1.          EL631
00307                                                                   EL631
00308      IF ANYISSI NOT = 'N' AND 'Y'                                 EL631
00309         MOVE -1                  TO ANYISSL                       EL631
00310         MOVE AL-UABON            TO ANYISSA                       EL631
00311         PERFORM 0350-SELECTION-ERROR                              EL631
00312      ELSE                                                         EL631
00313         MOVE ANYISSI             TO PI-ISSUES-IN-ERROR-SW.        EL631
00314                                                                   EL631
00315      IF ANYCANI NOT = 'N' AND 'Y'                                 EL631
00316         MOVE -1                  TO ANYCANL                       EL631
00317         MOVE AL-UABON            TO ANYCANA                       EL631
00318         PERFORM 0350-SELECTION-ERROR                              EL631
00319      ELSE                                                         EL631
00320         MOVE ANYCANI             TO PI-CANCELS-IN-ERROR-SW.       EL631
00321                                                                   EL631
00322      IF ALLISSI NOT = 'N' AND 'Y'                                 EL631
00323         MOVE -1                  TO ALLISSL                       EL631
00324         MOVE AL-UABON            TO ALLISSA                       EL631
00325         PERFORM 0350-SELECTION-ERROR                              EL631
00326      ELSE                                                         EL631
00327         MOVE ALLISSI             TO PI-ALL-ISSUES-SW.             EL631
00328                                                                   EL631
00329      IF ALLCANI NOT = 'N' AND 'Y'                                 EL631
00330         MOVE -1                  TO ALLCANL                       EL631
00331         MOVE AL-UABON            TO ALLCANA                       EL631
00332         PERFORM 0350-SELECTION-ERROR                              EL631
00333      ELSE                                                         EL631
00334         MOVE ALLCANI             TO PI-ALL-CANCELS-SW.            EL631
00335                                                                   EL631
00336      IF ONLYHDRI NOT = 'N' AND 'Y'                                EL631
00337         MOVE -1                  TO ONLYHDRL                      EL631
00338         MOVE AL-UABON            TO ONLYHDRA                      EL631
00339         PERFORM 0350-SELECTION-ERROR                              EL631
00340      ELSE                                                         EL631
00341         MOVE ONLYHDRI             TO PI-ONLY-BATCH-HEADERS-SW.    EL631
00342                                                                   EL631
00343      IF OUTBALI NOT = 'N' AND 'Y'                                 EL631
00344         MOVE -1                  TO OUTBALL                       EL631
00345         MOVE AL-UABON            TO OUTBALA                       EL631
00346         PERFORM 0350-SELECTION-ERROR                              EL631
00347      ELSE                                                         EL631
00348         MOVE OUTBALI             TO PI-ALL-OUT-OF-BAL-SW.         EL631
00349                                                                   EL631
00350      IF HLDRECI NOT = 'N' AND 'Y'                                    CL*19
00351         MOVE -1                  TO HLDRECL                          CL*19
00352         MOVE AL-UABON            TO HLDRECA                          CL*19
00353         PERFORM 0350-SELECTION-ERROR                                 CL*19
00354      ELSE                                                            CL*19
00355         MOVE HLDRECI             TO PI-HOLD-REC-SW.                  CL*19
00356                                                                      CL*19
00357      IF CHGRECI NOT = 'N' AND 'Y'                                 EL631
00358         MOVE -1                  TO CHGRECL                       EL631
00359         MOVE AL-UABON            TO CHGRECA                       EL631
00360         PERFORM 0350-SELECTION-ERROR                              EL631
00361      ELSE                                                         EL631
00362         MOVE CHGRECI             TO PI-CHANGE-REC-SW.             EL631
00363                                                                   EL631
00364      IF CHKREQI NOT = 'N' AND 'Y'                                 EL631
00365         MOVE -1                  TO CHKREQL                       EL631
00366         MOVE AL-UABON            TO CHKREQA                       EL631
00367         PERFORM 0350-SELECTION-ERROR                              EL631
00368      ELSE                                                         EL631
00369         MOVE CHKREQI             TO PI-CHK-REQ-REC-SW.            EL631
00370                                                                   EL631
00371      IF STARTATI NOT = 'N' AND 'Y'                                   CL**6
00372         MOVE -1                  TO STARTATL                         CL**6
00373         MOVE AL-UABON            TO STARTATA                         CL**6
00374         PERFORM 0350-SELECTION-ERROR.                                CL**6
00375                                                                      CL**6
00376      IF ORGINALI NOT = 'N' AND 'Y'                                EL631
00377         MOVE -1                  TO ORGINALL                      EL631
00378         MOVE AL-UABON            TO ORGINALA                      EL631
00379         PERFORM 0350-SELECTION-ERROR                              EL631
00380      ELSE                                                         EL631
00381         MOVE ORGINALI            TO PI-ORIGINAL-BATCH-SW.         EL631
00382                                                                   EL631
00383      IF ANYISSWI NOT = 'N' AND 'Y'                                   CL**8
00384         MOVE -1                  TO ANYISSWL                         CL**8
00385         MOVE AL-UABON            TO ANYISSWA                         CL**8
00386         PERFORM 0350-SELECTION-ERROR                                 CL**8
00387      ELSE                                                            CL**8
00388         MOVE ANYISSWI            TO PI-ISSUE-WARNING-SW.             CL**8
00389                                                                      CL**8
00390      IF ANYCANWI NOT = 'N' AND 'Y'                                   CL**8
00391         MOVE -1                  TO ANYCANWL                         CL**8
00392         MOVE AL-UABON            TO ANYCANWA                         CL**8
00393         PERFORM 0350-SELECTION-ERROR                                 CL**8
00394      ELSE                                                            CL**8
00395         MOVE ANYCANWI            TO PI-CANCEL-WARNING-SW.            CL**8
00396                                                                      CL**8
00397      IF NOT EMI-NO-ERRORS                                         EL631
00398         GO TO 8200-SEND-DATAONLY.                                 EL631
00399                                                                   EL631
00400      IF ANYISSI = 'Y' AND (ALLISSI = 'Y' OR ANYISSWI = 'Y')          CL**8
00401         MOVE -1                  TO ANYISSL                       EL631
00402         MOVE AL-UABON            TO ANYISSA  ANYISSWA ALLISSA        CL**8
00403         PERFORM 0360-COMBINATION-ERROR.                           EL631
00404                                                                   EL631
00405      IF ANYISSWI = 'Y' AND (ALLISSI = 'Y' OR ANYISSI = 'Y')          CL**8
00406         MOVE -1                  TO ANYISSL                          CL**8
00407         MOVE AL-UABON            TO ANYISSA  ANYISSWA ALLISSA        CL**8
00408         PERFORM 0360-COMBINATION-ERROR.                              CL**8
00409                                                                      CL**8
00410      IF ANYCANI = 'Y' AND (ALLCANI = 'Y' OR ANYCANWI = 'Y')          CL**8
00411         MOVE -1                  TO ANYCANL                       EL631
00412         MOVE AL-UABON            TO ANYCANA  ANYCANWA ALLCANA        CL**8
00413         PERFORM 0360-COMBINATION-ERROR.                              CL**8
00414                                                                      CL**8
00415      IF ANYCANWI = 'Y' AND (ALLCANI = 'Y' OR ANYCANI = 'Y')          CL**8
00416         MOVE -1                  TO ANYCANL                          CL**8
00417         MOVE AL-UABON            TO ANYCANA  ANYCANWA ALLCANA        CL**8
00418         PERFORM 0360-COMBINATION-ERROR.                           EL631
00419                                                                   EL631
00420      IF OUTBALI = 'Y' AND ONLYHDRI = 'Y'                          EL631
00421         MOVE -1                  TO ONLYHDRL                      EL631
00422         MOVE AL-UABON            TO ONLYHDRA  OUTBALA             EL631
00423         PERFORM 0360-COMBINATION-ERROR.                           EL631
00424                                                                      CL**6
00425      IF STARTATI = 'Y'                                               CL**6
00426         IF BATCHL NOT GREATER THAN ZEROS                             CL**6
00427            MOVE -1                  TO BATCHL                        CL**6
00428            MOVE AL-UANOF            TO BATCHA                        CL**6
00429            MOVE ER-2201             TO EMI-ERROR                     CL**6
00430            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL**6
00431                                                                   EL631
00432      IF ANYISSI  = 'N' AND ALLISSI  = 'N' AND                        CL**8
00433         ANYCANI  = 'N' AND ALLCANI  = 'N' AND                        CL**8
00434         OUTBALI  = 'N' AND ONLYHDRI = 'N' AND                        CL**8
00435         ANYISSWI = 'N' AND ANYCANWI = 'N'                            CL**8
00436         MOVE ER-0329             TO EMI-ERROR                     EL631
00437         MOVE -1                  TO ANYISSL                       EL631
00438         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL631
00439                                                                   EL631
CIDMOD     IF PI-COMPANY-ID = CLIENT-MIC OR CLIENT-PEM OR CLIENT-FLA       CL*19
00441         IF ORGINALI = 'N'                                         EL631
00442            NEXT SENTENCE                                          EL631
00443         ELSE                                                      EL631
00444            IF CARRIERL = ZEROS AND GROUPL = ZEROS AND             EL631
00445               STATEL = ZEROS   AND ACCOUNTL = ZEROS AND           EL631
00446               BATCHL = ZEROS                                      EL631
00447               MOVE -1            TO BATCHL                        EL631
00448               MOVE ER-2150       TO EMI-ERROR                     EL631
00449               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           EL631
00450                                                                   EL631
00451      IF (ONLYHDRI = 'Y' OR OUTBALI = 'Y' OR                       EL631
00452         CHGRECI  = 'Y') AND ORGINALI = 'Y'                        EL631
00453         MOVE -1                  TO ORGINALL                      EL631
00454         PERFORM 0360-COMBINATION-ERROR.                           EL631
00455                                                                   EL631
00456      IF (ANYISSI  = 'Y' OR ALLISSI  = 'Y' OR                         CL**8
00457          ANYISSWI = 'Y' OR ANYCANWI = 'Y' OR                         CL**8
00458          ANYCANI  = 'Y' OR ALLCANI  = 'Y')   AND                     CL**8
00459         (OUTBALI  = 'Y' OR ONLYHDRI = 'Y')                           CL**8
00460         MOVE -1                  TO ANYISSL                       EL631
00461         PERFORM 0360-COMBINATION-ERROR.                           EL631
00462                                                                   EL631
00463      IF (ANYISSI = 'Y' OR ALLISSI   = 'Y' OR                      EL631
00464          CHGRECI = 'Y' OR ONLYHDRI  = 'Y' OR                      EL631
00465          OUTBALI = 'Y' OR ANYISSWI  = 'Y')                           CL**8
00466          AND CHKREQI = 'Y'                                           CL**8
00467          MOVE -1                  TO ANYISSL                      EL631
00468          PERFORM 0360-COMBINATION-ERROR.                          EL631
00469                                                                   EL631
00470      IF  EIBAID = DFHPF1                                          EL631
00471          GO TO 330-CHECK-ERRORS.                                  EL631
00472                                                                   EL631
00473      IF PI-CARRIER-SECURITY GREATER THAN SPACES                   EL631
00474         IF  CARRIERL GREATER THAN ZEROS                           EL631
00475             IF PI-CARRIER-SECURITY = CARRIERI                     EL631
00476                MOVE AL-UANON     TO CARRIERA                      EL631
00477             ELSE                                                  EL631
00478                MOVE ER-2372      TO EMI-ERROR                     EL631
00479                MOVE -1           TO CARRIERL                      EL631
00480                MOVE AL-UABON     TO CARRIERA                      EL631
00481                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.          EL631
00482                                                                   EL631
00483      IF PI-ACCOUNT-SECURITY GREATER THAN SPACES                   EL631
00484         IF  ACCOUNTL GREATER THAN ZEROS                           EL631
00485             IF PI-ACCOUNT-SECURITY = ACCOUNTI                     EL631
00486                MOVE AL-UANON     TO ACCOUNTA                      EL631
00487             ELSE                                                  EL631
00488                MOVE ER-2372      TO EMI-ERROR                     EL631
00489                MOVE -1           TO ACCOUNTL                      EL631
00490                MOVE AL-UABON     TO ACCOUNTA                      EL631
00491                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.          EL631
00492                                                                   EL631
00493      IF BATCHL  GREATER THAN +0                                      CL*14
00494         IF CSRL GREATER THAN +0                                      CL*14
00495            MOVE AL-UABON         TO BATCHA                           CL*14
00496                                     CSRA                             CL*14
00497            MOVE ER-2971          TO EMI-ERROR                        CL*14
00498            MOVE -1               TO BATCHL                           CL*14
00499            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL*14
00500                                                                      CL*14
00501  330-CHECK-ERRORS.                                                EL631
00502      IF NOT EMI-NO-ERRORS                                         EL631
00503         GO TO 8200-SEND-DATAONLY.                                 EL631
00504                                                                   EL631
00505      IF EIBAID = DFHPF1                                           EL631
00506          PERFORM 5000-CREATE-TEMP-STORAGE THRU 5000-EXIT             CL*10
00507          MOVE XCTL-630            TO PGM-NAME                        CL*10
00508          GO TO 9300-XCTL.                                            CL*10
00509                                                                      CL*10
00510      IF EIBAID = DFHPF2                                              CL*10
00511          PERFORM 5000-CREATE-TEMP-STORAGE THRU 5000-EXIT             CL*10
00512          MOVE XCTL-663            TO PGM-NAME                        CL*10
00513          GO TO 9300-XCTL.                                            CL*10
00514                                                                   EL631
00515      IF EIBAID = DFHPF3                                              CL*15
00516          PERFORM 5000-CREATE-TEMP-STORAGE THRU 5000-EXIT             CL*15
00517          MOVE XCTL-656            TO PGM-NAME                        CL*15
00518          GO TO 9300-XCTL.                                            CL*15
00519                                                                      CL*15
00520      IF PI-NO-ACCOUNT-SECURITY                                    EL631
00521         IF PI-NO-CARRIER-SECURITY                                 EL631
00522            GO TO 330-CONTINUE-EDIT.                               EL631
00523                                                                   EL631
00524      IF ONLY-BATCH-HEADERS OR ALL-OUT-OF-BAL                      EL631
00525          GO TO 330-CONTINUE-EDIT.                                    CL*10
00526                                                                   EL631
00527      IF BATCHL GREATER THAN ZEROS                                 EL631
00528          GO TO 330-CONTINUE-EDIT.                                    CL*10
00529                                                                   EL631
00530      IF CARRIERL GREATER THAN ZEROS                               EL631
00531          GO TO 330-CONTINUE-EDIT.                                    CL*10
00532                                                                   EL631
00533      MOVE -1                     TO BATCHL.                       EL631
00534      MOVE ER-2377                TO EMI-ERROR.                    EL631
00535      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL631
00536      GO TO 8200-SEND-DATAONLY.                                    EL631
00537                                                                   EL631
00538  330-CONTINUE-EDIT.                                               EL631
00539      MOVE ZEROS                  TO PI-PB-BATCH-SEQ-NO.           EL631
00540                                                                   EL631
00541      IF BATCHL NOT = ZERO                                         EL631
00542         MOVE BATCHI              TO PI-PB-ENTRY-BATCH             EL631
00543         PERFORM 6200-READ-FILE  THRU 6299-EXIT                    EL631
00544      ELSE                                                         EL631
00545         GO TO 330-SET-BROWSE-CONTROL.                             EL631
00546                                                                      CL*10
00547      IF SEQL NOT = ZERO                                           EL631
00548          IF SEQI NOT NUMERIC                                      EL631
00549              MOVE ER-2314 TO EMI-ERROR                            EL631
00550              MOVE -1             TO SEQL                          EL631
00551              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL631
00552              GO TO 8200-SEND-DATAONLY                             EL631
00553          ELSE                                                     EL631
00554              MOVE SEQI TO PI-PB-BATCH-SEQ-NO.                     EL631
00555                                                                   EL631
00556      IF PI-CARRIER-SECURITY GREATER THAN SPACES                   EL631
00557         IF PI-CARRIER-SECURITY = PB-CARRIER NEXT SENTENCE         EL631
00558      ELSE                                                         EL631
00559         MOVE ER-2372             TO EMI-ERROR                     EL631
00560         MOVE -1                  TO BATCHL                        EL631
00561         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL631
00562         GO TO 8200-SEND-DATAONLY.                                 EL631
00563                                                                   EL631
00564      IF PI-ACCOUNT-SECURITY GREATER THAN SPACES                   EL631
00565         IF PI-ACCOUNT-SECURITY = PB-ACCOUNT                       EL631
00566          NEXT SENTENCE                                            EL631
00567      ELSE                                                         EL631
00568         MOVE ER-2372             TO EMI-ERROR                     EL631
00569         MOVE -1                  TO BATCHL                        EL631
00570         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL631
00571         GO TO 8200-SEND-DATAONLY.                                 EL631
00572                                                                   EL631
00573  330-SET-BROWSE-CONTROL.                                          EL631
00574                                                                      CL*14
00575      IF CSRL GREATER THAN +0                                         CL*14
00576         IF OUTBALI = 'Y' OR ONLYHDRI = 'Y'                           CL*14
00577            MOVE LOW-VALUES          TO PI-ERPNDB-CSR-KEY             CL*14
00578            MOVE PI-COMPANY-CD       TO PI-PB-CSR-COMPANY-CD-A2       CL*14
00579            MOVE CSRI                TO PI-PB-CSR-ID                  CL*14
00580            MOVE SPACES           TO PI-PB-CARRIER   PI-PB-GROUPING   CL*14
00581                                     PI-PB-STATE     PI-PB-ACCOUNT    CL*14
00582            MOVE '4'                 TO PI-BROWSE-TYPE                CL*14
00583            GO TO 0340-XCTL.                                          CL*14
00584                                                                      CL*14
00585      IF OUTBALI = 'Y' OR ONLYHDRI = 'Y'                           EL631
00586         IF CARRIERL = ZEROS AND GROUPL = ZEROS AND                EL631
00587            STATEL = ZEROS   AND ACCOUNTL = ZEROS                  EL631
00588            MOVE SPACES           TO PI-PB-CARRIER   PI-PB-GROUPINGEL631
00589                                     PI-PB-STATE     PI-PB-ACCOUNT EL631
00590            MOVE  '1'             TO PI-BROWSE-TYPE                EL631
00591            GO TO 0340-XCTL                                        EL631
00592         ELSE                                                         CL*14
00593            PERFORM 6100-BUILD-KEY THRU 6199-EXIT                  EL631
00594            MOVE  '1'             TO PI-BROWSE-TYPE                EL631
00595            GO TO 0340-XCTL.                                       EL631
00596                                                                   EL631
00597      IF CERTNOL NOT = ZEROS                                       EL631
00598         MOVE CERTNOI             TO PI-PB-CERT-PRIME                 CL**3
00599         MOVE SPACE               TO PI-PB-CERT-SFX                   CL**4
00600         IF SUFFIXL NOT = ZEROS                                       CL**4
00601            MOVE SUFFIXI          TO PI-PB-CERT-SFX                   CL**4
00602         ELSE                                                         CL**4
00603            NEXT SENTENCE                                             CL**4
00604      ELSE                                                            CL**4
00605         MOVE LOW-VALUES          TO PI-PB-CERT-NO.                   CL**4
00606                                                                   EL631
00607      IF EFFDTL NOT = ZEROS                                        EL631
00608         MOVE EFFDTI                 TO DEEDIT-FIELD               EL631
00609         PERFORM 8600-DEEDIT                                       EL631
00610         MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY         EL631
00611         MOVE '4'                    TO DC-OPTION-CODE             EL631
00612         PERFORM 9700-DATE-LINK                                    EL631
00613         IF DATE-CONVERSION-ERROR                                  EL631
00614            MOVE ER-0348             TO EMI-ERROR                  EL631
00615            MOVE -1                  TO EFFDTL                     EL631
00616            MOVE AL-UABON            TO EFFDTA                     EL631
00617            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL631
00618         ELSE                                                      EL631
00619            MOVE DC-GREG-DATE-1-EDIT TO EFFDTI                     EL631
00620            MOVE AL-UANON            TO EFFDTA                     EL631
00621            MOVE DC-BIN-DATE-1       TO PI-PB-CERT-EFF-DT.         EL631
00622                                                                      CL*14
00623      IF CSRL GREATER THAN +0                                         CL*14
00624         MOVE LOW-VALUES             TO PI-ERPNDB-CSR-KEY             CL*14
00625         MOVE PI-COMPANY-CD          TO PI-PB-CSR-COMPANY-CD-A2       CL*14
00626         MOVE CSRI                   TO PI-PB-CSR-ID                  CL*14
00627         MOVE '4'                    TO PI-BROWSE-TYPE                CL*14
00628         GO TO 0340-XCTL.                                             CL*14
00629                                                                   EL631
00630      IF CARRIERL = ZEROS AND                                      EL631
00631         GROUPL   = ZEROS AND                                      EL631
00632         STATEL   = ZEROS AND                                      EL631
00633         ACCOUNTL = ZEROS                                          EL631
00634         IF BATCHL NOT = ZEROS                                     EL631
00635            GO TO 0335-SET-BROWSE-TYPE                             EL631
00636         ELSE                                                      EL631
00637            MOVE ' '                 TO PI-BROWSE-TYPE             EL631
00638            GO TO 0340-XCTL.                                       EL631
00639                                                                   EL631
00640      PERFORM 6100-BUILD-KEY THRU 6199-EXIT.                       EL631
00641                                                                   EL631
00642      IF NOT EMI-NO-ERRORS                                         EL631
00643         GO TO 8200-SEND-DATAONLY.                                 EL631
00644                                                                   EL631
00645      IF PI-COMPANY-ID NOT = 'DMD'                                    CL*21
00646          PERFORM 6300-READ-ACCOUNT-FILE THRU 6399-EXIT.              CL*21
00647                                                                   EL631
00648  0335-SET-BROWSE-TYPE.                                            EL631
00649      IF STARTATI = 'Y'                                               CL**6
00650         MOVE BATCHI              TO PI-PB-ENTRY-BATCH                CL**6
00651         MOVE SPACE               TO PI-BROWSE-TYPE                   CL**6
00652         GO TO 0340-XCTL.                                             CL**6
00653                                                                      CL**6
00654      IF BATCHL NOT = ZEROS                                        EL631
00655         IF CERTNOL = ZEROS AND EFFDTL = ZEROS                     EL631
00656            MOVE '1'              TO PI-BROWSE-TYPE                EL631
00657         ELSE                                                      EL631
00658            MOVE '3'              TO PI-BROWSE-TYPE                EL631
00659      ELSE                                                         EL631
00660         MOVE '2'              TO PI-BROWSE-TYPE.                  EL631
00661                                                                   EL631
00662  0340-XCTL.                                                       EL631

020816     if pi-company-id = 'VPP'
020816        move 'VP6311'            TO PGM-NAME
020816     ELSE
020816        MOVE XCTL-6311           TO PGM-NAME
020816     END-IF

00665      EXEC CICS XCTL                                               EL631
00666          PROGRAM    (PGM-NAME)                                    EL631
00667          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL631
00668          LENGTH     (1300)                                           CL**9
00669      END-EXEC.                                                    EL631
00670                                                                   EL631
00671  0350-SELECTION-ERROR.                                            EL631
00672      MOVE ER-2190                TO EMI-ERROR.                    EL631
00673      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL631
00674                                                                   EL631
00675  0360-COMBINATION-ERROR.                                          EL631
00676      MOVE ER-2191                TO EMI-ERROR.                    EL631
00677      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL631
00678                                                                   EL631
00679      EJECT                                                        EL631
00680                                                                   EL631
00681  5000-CREATE-TEMP-STORAGE.                                        EL631
00682      EXEC CICS WRITEQ TS                                          EL631
00683          QUEUE    (QID)                                           EL631
00684          FROM     (PROGRAM-INTERFACE-BLOCK)                       EL631
00685          LENGTH   (PI-COMM-LENGTH)                                EL631
00686      END-EXEC.                                                    EL631
00687                                                                   EL631
00688  5000-EXIT.                                                       EL631
00689       EXIT.                                                       EL631
00690                                                                   EL631
00691      EJECT                                                        EL631
00692                                                                   EL631
00693  5050-RECOVER-TEMP-STORAGE.                                       EL631
00694      EXEC CICS HANDLE CONDITION                                   EL631
00695          QIDERR  (5050-QID-ERROR)                                 EL631
00696      END-EXEC.                                                    EL631
00697                                                                   EL631
00698      EXEC CICS READQ TS                                           EL631
00699           QUEUE   (QID)                                           EL631
00700           INTO    (PROGRAM-INTERFACE-BLOCK)                       EL631
00701           LENGTH  (PI-COMM-LENGTH)                                EL631
00702       END-EXEC.                                                   EL631
00703                                                                   EL631
00704      EXEC CICS DELETEQ TS                                         EL631
00705          QUEUE   (QID)                                            EL631
00706      END-EXEC.                                                    EL631
00707                                                                   EL631
00708      GO TO 5050-EXIT.                                             EL631
00709                                                                   EL631
00710  5050-QID-ERROR.                                                  EL631
00711      MOVE ER-0033                TO EMI-ERROR.                       CL*10
00712      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL631
00713                                                                   EL631
00714  5050-EXIT.                                                       EL631
00715       EXIT.                                                       EL631
00716                                                                   EL631
00717      EJECT                                                        EL631
00718                                                                   EL631
00719  6000-BUILD-SCREEN.                                               EL631
00720      IF RETURN-FROM = XCTL-630 OR XCTL-663 OR XCTL-656               CL*15
00721         PERFORM 5050-RECOVER-TEMP-STORAGE THRU 5050-EXIT.         EL631
00722                                                                   EL631
00723      MOVE PI-ISSUES-IN-ERROR-SW      TO ANYISSI.                     CL*10
00724      MOVE PI-CANCELS-IN-ERROR-SW     TO ANYCANI.                     CL*10
00725      MOVE PI-ALL-ISSUES-SW           TO ALLISSI.                     CL*10
00726      MOVE PI-ALL-CANCELS-SW          TO ALLCANI.                     CL*10
00727      MOVE PI-ONLY-BATCH-HEADERS-SW   TO ONLYHDRI.                    CL*10
00728      MOVE PI-ALL-OUT-OF-BAL-SW       TO OUTBALI.                     CL*10
00729      MOVE PI-HOLD-REC-SW             TO HLDRECI.                     CL*19
00730      MOVE PI-CHANGE-REC-SW           TO CHGRECI.                     CL*10
00731      MOVE PI-ORIGINAL-BATCH-SW       TO ORGINALI.                    CL*10
00732      MOVE PI-ISSUE-WARNING-SW        TO ANYISSWI.                    CL*10
00733      MOVE PI-CANCEL-WARNING-SW       TO ANYCANWI.                    CL*10
00734                                                                   EL631
00735      IF PI-PB-ENTRY-BATCH  = SPACES OR = LOW-VALUES               EL631
00736         NEXT SENTENCE                                             EL631
00737      ELSE                                                         EL631
00738         MOVE AL-UANON            TO BATCHA                        EL631
00739         MOVE PI-PB-ENTRY-BATCH   TO BATCHI.                       EL631
00740                                                                   EL631
00741      IF PI-PB-CERT-NO = SPACES OR = LOW-VALUES                    EL631
00742         NEXT SENTENCE                                             EL631
00743      ELSE                                                         EL631
00744         MOVE AL-UANON            TO CERTNOA                       EL631
00745         MOVE PI-PB-CERT-PRIME    TO CERTNOI                          CL**2
00746         MOVE AL-UANON            TO SUFFIXA                          CL**4
00747         MOVE PI-PB-CERT-SFX      TO SUFFIXI.                         CL**4
00748                                                                   EL631
00749      IF PI-PB-CERT-EFF-DT = LOW-VALUES OR SPACES                  EL631
00750         NEXT SENTENCE                                             EL631
00751      ELSE                                                         EL631
00752         MOVE PI-PB-CERT-EFF-DT      TO DC-BIN-DATE-1              EL631
00753         MOVE ' '                    TO DC-OPTION-CODE             EL631
00754         PERFORM 9700-DATE-LINK                                    EL631
00755         MOVE DC-GREG-DATE-1-EDIT    TO EFFDTI                     EL631
00756         MOVE AL-UANON               TO EFFDTA.                    EL631
00757                                                                   EL631
00758      IF PI-NO-PB-RECS-FOUND                                       EL631
00759         MOVE ER-2375             TO EMI-ERROR                     EL631
00760         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL631
00761                                                                   EL631
00762      IF NOT PI-ALTERNATE-BROWSE                                   EL631
00763         GO TO 6099-EXIT.                                          EL631
00764                                                                   EL631
00765      IF PI-PB-CARRIER = SPACES OR = LOW-VALUES                    EL631
00766         NEXT SENTENCE                                             EL631
00767      ELSE                                                         EL631
00768         MOVE PI-PB-CARRIER       TO CARRIERI                      EL631
00769         MOVE AL-UANON            TO CARRIERA.                     EL631
00770                                                                   EL631
00771      IF PI-PB-GROUPING = SPACES OR = LOW-VALUES                   EL631
00772         NEXT SENTENCE                                             EL631
00773      ELSE                                                         EL631
00774         MOVE PI-PB-GROUPING      TO GROUPI                        EL631
00775         MOVE AL-UANON            TO GROUPA.                       EL631
00776                                                                   EL631
00777      IF PI-PB-STATE  = SPACES OR = LOW-VALUES                     EL631
00778         NEXT SENTENCE                                             EL631
00779      ELSE                                                         EL631
00780         MOVE PI-PB-STATE         TO STATEI                        EL631
00781         MOVE AL-UANON            TO STATEA.                       EL631
00782                                                                   EL631
00783      IF PI-PB-ACCOUNT  = SPACES OR = LOW-VALUES                   EL631
00784         NEXT SENTENCE                                             EL631
00785      ELSE                                                         EL631
00786         MOVE PI-PB-ACCOUNT       TO ACCOUNTI                      EL631
00787         MOVE AL-UANON            TO ACCOUNTA.                     EL631
00788                                                                   EL631
00789  6099-EXIT.                                                       EL631
00790      EXIT.                                                        EL631
00791      EJECT                                                        EL631
00792  6100-BUILD-KEY.                                                  EL631
00793      MOVE SPACES                 TO PI-PB-CARRIER                 EL631
00794                                     PI-PB-GROUPING                EL631
00795                                     PI-PB-STATE.                  EL631
00796                                                                   EL631
00797      IF CARRIERL GREATER THAN ZEROS                               EL631
00798         IF (CARR-ACCNT-CNTL       OR                              EL631
00799             CARR-ST-ACCNT-CNTL    OR                              EL631
00800             CARR-GROUP-ST-ACCNT-CNTL)                             EL631
00801             MOVE CARRIERI         TO PI-PB-CARRIER.               EL631
00802                                                                   EL631
00803                                                                   EL631
00804      IF GROUPL GREATER THAN ZEROS                                 EL631
00805         IF CARR-GROUP-ST-ACCNT-CNTL                               EL631
00806            MOVE GROUPI            TO PI-PB-GROUPING.              EL631
00807                                                                   EL631
00808                                                                   EL631
00809      IF STATEL GREATER THAN ZEROS                                 EL631
00810         IF (ST-ACCNT-CNTL         OR                              EL631
00811             CARR-ST-ACCNT-CNTL    OR                              EL631
00812             CARR-GROUP-ST-ACCNT-CNTL)                             EL631
00813             MOVE STATEI           TO PI-PB-STATE.                 EL631
00814                                                                   EL631
00815      IF ACCOUNTI = SPACES OR = LOW-VALUES                         EL631
00816         MOVE SPACES              TO PI-PB-ACCOUNT                 EL631
00817         ELSE                                                      EL631
00818         MOVE ACCOUNTI            TO PI-PB-ACCOUNT.                EL631
00819                                                                   EL631
00820      IF PI-PB-CARRIER = SPACES AND                                EL631
00821         (CARR-ACCNT-CNTL       OR                                 EL631
00822          CARR-ST-ACCNT-CNTL    OR                                 EL631
00823          CARR-GROUP-ST-ACCNT-CNTL)                                EL631
00824         MOVE ER-0193             TO EMI-ERROR                     EL631
00825         MOVE -1                  TO CARRIERL                      EL631
00826         PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                EL631
00827                                                                   EL631
00828      IF PI-PB-GROUPING = SPACES AND                               EL631
00829          CARR-GROUP-ST-ACCNT-CNTL                                 EL631
00830         MOVE ER-0195             TO EMI-ERROR                     EL631
00831         MOVE -1                  TO GROUPL                        EL631
00832         PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                EL631
00833                                                                   EL631
00834      IF PI-PB-STATE = SPACES AND                                  EL631
00835         (ST-ACCNT-CNTL         OR                                 EL631
00836          CARR-ST-ACCNT-CNTL    OR                                 EL631
00837          CARR-GROUP-ST-ACCNT-CNTL)                                EL631
00838         MOVE ER-0144             TO EMI-ERROR                     EL631
00839         MOVE -1                  TO STATEL                        EL631
00840         PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                EL631
00841                                                                   EL631
00842  6199-EXIT.                                                       EL631
00843      EXIT.                                                        EL631
00844      EJECT                                                        EL631
00845  6200-READ-FILE.                                                  EL631
00846      EXEC CICS HANDLE CONDITION                                   EL631
00847          NOTFND   (6250-NOT-FOUND)                                EL631
00848      END-EXEC.                                                    EL631
00849                                                                   EL631
00850      IF PI-COMPANY-ID = CLIENT-MIC OR CLIENT-PEM OR CLIENT-FLA       CL*19
00851         IF PI-DISPLAY-ORIGINAL-BATCH                              EL631
00852            MOVE ERPNDB3-FILE-ID     TO FILE-ID                    EL631
00853         ELSE                                                      EL631
00854            MOVE ERPNDB-FILE-ID      TO FILE-ID                    EL631
00855      ELSE                                                         EL631
00856         MOVE ERPNDB-FILE-ID         TO FILE-ID.                   EL631
00857                                                                   EL631
00858      EXEC CICS READ                                               EL631
00859          GTEQ                                                     EL631
00860          DATASET   (FILE-ID)                                      EL631
00861          SET       (ADDRESS OF PENDING-BUSINESS)                     CL*19
00862          RIDFLD    (PI-ERPNDB-KEY)                                EL631
00863      END-EXEC.                                                    EL631
00864                                                                   EL631
00865      IF ORGINALL GREATER THAN ZEROS                               EL631
00866         AND ORGINALI = 'Y'                                        EL631
00867         IF PI-PB-ENTRY-BATCH = PB-ORIGINAL-ENTRY-BATCH AND        EL631
00868            PI-PB-COMPANY-CD  = PB-ORIGINAL-COMPANY-CD                CL*10
00869            GO TO 6299-EXIT.                                       EL631
00870                                                                   EL631
00871      IF PI-PB-COMPANY-CD  = PB-COMPANY-CD                            CL*11
00872         IF STARTATI = 'Y'                                            CL*11
00873             MOVE PB-ENTRY-BATCH TO PI-PB-ENTRY-BATCH                 CL*11
00874             GO TO 6299-EXIT                                          CL*11
00875         ELSE                                                         CL*11
00876             IF PI-PB-ENTRY-BATCH = PB-ENTRY-BATCH                    CL*11
00877                 GO TO 6299-EXIT.                                     CL*11
00878                                                                   EL631
00879  6250-NOT-FOUND.                                                  EL631
00880      MOVE ER-2193                TO EMI-ERROR                     EL631
00881      MOVE -1                     TO BATCHL                        EL631
00882      MOVE AL-UABON               TO BATCHA                        EL631
00883      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     EL631
00884      GO TO 8200-SEND-DATAONLY.                                    EL631
00885                                                                   EL631
00886  6299-EXIT.                                                       EL631
00887       EXIT.                                                       EL631
00888      EJECT                                                        EL631
00889  6300-READ-ACCOUNT-FILE.                                          EL631
00890      MOVE PI-ERPNDB-ALT-KEY       TO WS-ACCT-KEY.                    CL*10
00891      MOVE LOW-VALUES              TO WS-EFF-DT.                      CL*10
00892                                                                   EL631
00893      EXEC CICS HANDLE CONDITION                                   EL631
00894          NOTFND   (6350-NOT-FOUND)                                EL631
00895      END-EXEC.                                                    EL631
00896                                                                   EL631
00897      EXEC CICS READ                                               EL631
00898          GTEQ                                                     EL631
00899          DATASET   (ERACCT2-FILE-ID)                              EL631
00900          SET       (ADDRESS OF ACCOUNT-MASTER)                       CL*19
00901          RIDFLD    (WS-ACCT-KEY)                                  EL631
00902      END-EXEC.                                                    EL631
00903                                                                   EL631
00904      IF PI-PB-ACCOUNT    = AM-VG-ACCOUNT   AND                    EL631
00905         PI-PB-STATE      = AM-VG-STATE     AND                    EL631
00906         PI-PB-GROUPING   = AM-VG-GROUPING  AND                    EL631
00907         PI-PB-CARRIER    = AM-VG-CARRIER   AND                    EL631
00908         PI-PB-COMPANY-CD = AM-COMPANY-CD-A1                       EL631
00909           GO TO 6399-EXIT.                                           CL*10
00910                                                                   EL631
00911  6350-NOT-FOUND.                                                  EL631
00912      MOVE ER-0179                TO EMI-ERROR.                       CL*10
00913      MOVE -1                     TO BATCHL.                          CL*10
00914      MOVE AL-UABON               TO BATCHA.                          CL*10
00915      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*10
00916      GO TO 8200-SEND-DATAONLY.                                    EL631
00917                                                                   EL631
00918  6399-EXIT.                                                       EL631
00919       EXIT.                                                       EL631
00920      EJECT                                                        EL631
00921  8100-SEND-INITIAL-MAP.                                           EL631
00922      MOVE SPACE                  TO PI-DISPLAY-SCREEN-SW.         EL631
00923                                                                   EL631
00924      IF PI-COMPANY-ID = CLIENT-MIC OR CLIENT-PEM OR CLIENT-FLA       CL*19
00925         MOVE AL-SANOF            TO ORGHDRA                       EL631
00926         MOVE AL-UANON            TO ORGINALA                      EL631
00927         MOVE 'N'                 TO ORGINALI                      EL631
00928      ELSE                                                         EL631
00929         MOVE AL-SADOF            TO ORGHDRA                       EL631
00930         MOVE 'N'                 TO ORGINALI                      EL631
00931         MOVE AL-SADON            TO ORGINALA.                     EL631
00932                                                                   EL631
           MOVE PI-ENTRY-CD-1          TO PI-CSR-SESSION-SW
           IF CSR-EDIT-SESSION
              MOVE '- CUSTOMER SERVICE REVIEW/CORRECTION -'
                                       TO HEADO
           END-IF
00933      MOVE SAVE-DATE              TO DATEO.                        EL631
00934      MOVE EIBTIME                TO TIME-IN.                      EL631
00935      MOVE TIME-OUT               TO TIMEO.                        EL631
00936      MOVE -1                     TO BATCHL.                       EL631
00937      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL631
00938      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                     EL631
00939                                                                   EL631
00940      EXEC CICS SEND                                               EL631
00941          MAP      (MAP-NAME)                                      EL631
00942          MAPSET   (MAPSET-NAME)                                   EL631
00943          FROM     (EL631AO)                                       EL631
00944          ERASE                                                    EL631
00945          CURSOR                                                   EL631
00946      END-EXEC.                                                    EL631
00947                                                                   EL631
00948      GO TO 9100-RETURN-TRAN.                                      EL631
00949                                                                   EL631
00950  8200-SEND-DATAONLY.                                              EL631
00951      MOVE SAVE-DATE              TO DATEO.                        EL631
00952      MOVE EIBTIME                TO TIME-IN.                      EL631
00953      MOVE TIME-OUT               TO TIMEO.                        EL631
00954      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL631
00955      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                     EL631
00956                                                                   EL631
00957      EXEC CICS SEND                                               EL631
00958          MAP      (MAP-NAME)                                      EL631
00959          MAPSET   (MAPSET-NAME)                                   EL631
00960          FROM     (EL631AO)                                       EL631
00961          DATAONLY                                                 EL631
00962          ERASEAUP                                                 EL631
00963          CURSOR                                                   EL631
00964      END-EXEC.                                                    EL631
00965                                                                   EL631
00966      GO TO 9100-RETURN-TRAN.                                      EL631
00967                                                                   EL631
00968  8300-SEND-TEXT.                                                  EL631
00969      EXEC CICS SEND TEXT                                          EL631
00970          FROM     (LOGOFF-TEXT)                                   EL631
00971          LENGTH   (LOGOFF-LENGTH)                                 EL631
00972          ERASE                                                    EL631
00973          FREEKB                                                   EL631
00974      END-EXEC.                                                    EL631
00975                                                                   EL631
00976      EXEC CICS RETURN                                             EL631
00977      END-EXEC.                                                    EL631
00978                                                                   EL631
00979  8600-DEEDIT.                                                     EL631
00980      EXEC CICS BIF DEEDIT                                         EL631
00981           FIELD   (DEEDIT-FIELD)                                  EL631
00982           LENGTH  (15)                                            EL631
00983      END-EXEC.                                                    EL631
00984                                                                   EL631
00985  8800-UNAUTHORIZED-ACCESS.                                        EL631
00986      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL631
00987      GO TO 8300-SEND-TEXT.                                        EL631
00988                                                                   EL631
00989  8810-PF23.                                                       EL631
00990      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL631
00991      MOVE XCTL-005               TO PGM-NAME.                     EL631
00992      GO TO 9300-XCTL.                                             EL631
00993                                                                   EL631
00994  9000-RETURN-CICS.                                                EL631
00995      EXEC CICS RETURN                                             EL631
00996      END-EXEC.                                                    EL631
00997                                                                   EL631
00998  9100-RETURN-TRAN.                                                EL631
00999      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL631
01000      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.         EL631
01001      EXEC CICS RETURN                                             EL631
01002          TRANSID    (TRANS-ID)                                    EL631
01003          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL631
01004          LENGTH     (PI-COMM-LENGTH)                              EL631
01005      END-EXEC.                                                    EL631
01006                                                                   EL631
01007  9200-RETURN-MAIN-MENU.                                           EL631
01008      MOVE XCTL-626               TO PGM-NAME.                     EL631
01009      GO TO 9300-XCTL.                                             EL631
01010                                                                   EL631
01011  9300-XCTL.                                                       EL631
01012      EXEC CICS XCTL                                               EL631
01013          PROGRAM    (PGM-NAME)                                    EL631
01014          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL631
01015          LENGTH     (PI-COMM-LENGTH)                              EL631
01016      END-EXEC.                                                    EL631
01017                                                                   EL631
01018  9400-CLEAR.                                                      EL631
01019      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     EL631
01020      GO TO 9300-XCTL.                                             EL631
01021                                                                   EL631
01022  9500-PF12.                                                       EL631
01023      MOVE XCTL-010               TO PGM-NAME.                     EL631
01024      GO TO 9300-XCTL.                                             EL631
01025                                                                   EL631
01026  9600-PGMID-ERROR.                                                EL631
01027      EXEC CICS HANDLE CONDITION                                   EL631
01028          PGMIDERR    (8300-SEND-TEXT)                             EL631
01029      END-EXEC.                                                    EL631
01030                                                                   EL631
01031      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL631
01032      MOVE ' '                    TO PI-ENTRY-CD-1.                EL631
01033      MOVE XCTL-005               TO PGM-NAME.                     EL631
01034      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL631
01035      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL631
01036      GO TO 9300-XCTL.                                             EL631
01037                                                                   EL631
01038  9700-DATE-LINK.                                                  EL631
01039      MOVE LINK-ELDATCV           TO PGM-NAME                      EL631
01040      EXEC CICS LINK                                               EL631
01041          PROGRAM    (PGM-NAME)                                    EL631
01042          COMMAREA   (DATE-CONVERSION-DATA)                        EL631
01043          LENGTH     (DC-COMM-LENGTH)                              EL631
01044      END-EXEC.                                                    EL631
01045                                                                   EL631
01046  9900-ERROR-FORMAT.                                               EL631
01047      IF NOT EMI-ERRORS-COMPLETE                                   EL631
01048          MOVE LINK-001           TO PGM-NAME                      EL631
01049          EXEC CICS LINK                                           EL631
01050              PROGRAM    (PGM-NAME)                                EL631
01051              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL631
01052              LENGTH     (EMI-COMM-LENGTH)                         EL631
01053          END-EXEC.                                                EL631
01054                                                                   EL631
01055  9900-EXIT.                                                       EL631
01056      EXIT.                                                        EL631
01057                                                                   EL631
01058  9990-ABEND.                                                      EL631
01059      MOVE LINK-004               TO PGM-NAME.                     EL631
01060      MOVE DFHEIBLK               TO EMI-LINE1.                       CL*10
01061      EXEC CICS LINK                                               EL631
01062          PROGRAM   (PGM-NAME)                                     EL631
01063          COMMAREA  (EMI-LINE1)                                    EL631
01064          LENGTH    (72)                                           EL631
01065      END-EXEC.                                                    EL631
01066                                                                   EL631
01067      GO TO 8200-SEND-DATAONLY.                                    EL631
01068                                                                   EL631
01069      GOBACK.                                                      EL631
01070                                                                   EL631
01071  9995-SECURITY-VIOLATION.                                         EL631
01072                              COPY ELCSCTP.                        EL631
01073                                                                   EL631
01074  9995-EXIT.                                                       EL631
01075      EXIT.                                                        EL631
01076                                                                   EL631
01077                                                                   EL631
