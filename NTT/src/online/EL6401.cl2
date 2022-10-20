00001  ID DIVISION.                                                     06/26/96
00002                                                                   EL6401
00003  PROGRAM-ID.                 EL6401.                                 LV005
00004 *              PROGRAM CONVERTED BY                                  CL**4
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**4
00006 *              CONVERSION DATE 02/12/96 09:54:41.                    CL**4
00007 *                            VMOD=2.005.                             CL**5
00008 *                                                                 EL6401
00009 *AUTHOR.     LOGIC,INC.                                              CL**4
00010 *            DALLAS, TEXAS.                                          CL**4
00011                                                                   EL6401
00012 *DATE-COMPILED.                                                      CL**4
00013                                                                   EL6401
00014 *SECURITY.   *****************************************************   CL**4
00015 *            *                                                   *   CL**4
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**4
00017 *            *                                                   *   CL**4
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**4
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**4
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**4
00021 *            *                                                   *   CL**4
00022 *            *****************************************************   CL**4
00023                                                                   EL6401
00024 *REMARKS.    TRANSACTION - EXC2 - ACCOUNT BILLING                    CL**4
00025 *                                 (STATEMENT DISPLAY).               CL**4
030612******************************************************************
030612*                   C H A N G E   L O G
030612*
030612* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
030612*-----------------------------------------------------------------
030612*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
030612* EFFECTIVE    NUMBER
030612*-----------------------------------------------------------------
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
030612******************************************************************
00026  ENVIRONMENT DIVISION.                                            EL6401
00027                                                                   EL6401
00028      EJECT                                                        EL6401
00029  DATA DIVISION.                                                   EL6401
00030  WORKING-STORAGE SECTION.                                         EL6401
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL6401
00032  77  FILLER  PIC X(32)  VALUE '*    EL6401 WORKING STORAGE    *'. EL6401
00033  77  FILLER  PIC X(32)  VALUE '********** V/M 2.005 ***********'.    CL**5
00034                                                                   EL6401
00035                              COPY ELCSCTM.                           CL**2
00036                              COPY ELCSCRTY.                          CL**2
00037                                                                   EL6401
00038     EJECT                                                         EL6401
00039                                                                   EL6401
00040  01  STANDARD-AREAS.                                              EL6401
00041      12  MAP-NAME            PIC X(8)    VALUE 'EL6401A'.         EL6401
00042      12  MAPSET-NAME         PIC X(8)    VALUE 'EL6401S'.         EL6401
00043      12  SCREEN-NUMBER       PIC X(4)    VALUE '640C'.            EL6401
00044      12  TRANS-ID            PIC X(4)    VALUE 'EXC2'.            EL6401
00045      12  EL640-TRANS-ID      PIC X(4)    VALUE 'EXC1'.            EL6401
00046      12  EL642-TRANS-ID      PIC X(4)    VALUE 'EXH7'.            EL6401
00047      12  PRINT-TRANS-ID      PIC X(4)    VALUE 'EXC9'.            EL6401
00048      12  PRINT-TERMID        PIC X(4)    VALUE SPACES.            EL6401
00049      12  THIS-PGM            PIC X(8)    VALUE 'EL6401'.          EL6401
00050      12  EL640               PIC X(8)    VALUE 'EL640'.           EL6401
00051      12  PGM-NAME            PIC X(8).                            EL6401
00052      12  TIME-IN             PIC S9(7).                           EL6401
00053      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL6401
00054          16  FILLER          PIC X.                               EL6401
00055          16  TIME-OUT        PIC 99V99.                           EL6401
00056          16  FILLER          PIC XX.                              EL6401
00057      12  XCTL-005            PIC X(8)    VALUE 'EL005'.           EL6401
00058      12  XCTL-010            PIC X(8)    VALUE 'EL010'.           EL6401
00059      12  XCTL-626            PIC X(8)    VALUE 'EL626'.           EL6401
00060      12  LINK-001            PIC X(8)    VALUE 'EL001'.           EL6401
00061      12  LINK-004            PIC X(8)    VALUE 'EL004'.           EL6401
00062      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.         EL6401
00063      12  ERBILL-FILE-ID      PIC X(8)    VALUE 'ERBILL'.          EL6401
00064      12  ELCNTL-FILE-ID      PIC X(8)    VALUE 'ELCNTL'.          EL6401
00065      12  WS-CURRENT-DATE     PIC X(8)    VALUE SPACES.            EL6401
00066                                                                   EL6401
00067  01  WORK-AREAS.                                                  EL6401
00068      12  TEXT-SUB            PIC 99      VALUE ZEROS.             EL6401
00069      EJECT                                                        EL6401
00070  01  ACCESS-KEYS.                                                 EL6401
00071      12  ERBILL-KEY.                                              EL6401
00072          16  ERBILL-CO-CD        PIC X       VALUE SPACE.         EL6401
00073          16  ERBILL-COMPARE.                                      EL6401
00074              20  ERBILL-CARRIER  PIC X       VALUE SPACE.         EL6401
00075              20  ERBILL-GROUP    PIC X(6)    VALUE SPACES.        EL6401
00076              20  ERBILL-ACCT     PIC X(10)   VALUE LOW-VALUES.    EL6401
00077              20  ERBILL-FIN-RESP PIC X(10)   VALUE SPACES.        EL6401
00078          16  ERBILL-REC-TYPE     PIC X       VALUE SPACE.         EL6401
00079          16  ERBILL-LINE-SEQ-NO  PIC S9(4)   COMP VALUE ZEROS.    EL6401
00080                                                                   EL6401
00081      12  ELCNTL-KEY.                                              EL6401
00082          16  ELCNTL-CO-ID        PIC X(3)    VALUE SPACES.        EL6401
00083          16  ELCNTL-REC-TYPE     PIC X       VALUE '1'.           EL6401
00084          16  ELCNTL-ACCESS       PIC X(4)    VALUE SPACES.        EL6401
00085          16  ELCNTL-SEQ-NO       PIC S9(4)   COMP VALUE ZEROS.    EL6401
00086      EJECT                                                        EL6401
00087                              COPY ELCDATE.                           CL**2
00088                                                                   EL6401
00089      EJECT                                                        EL6401
00090                              COPY ELCLOGOF.                          CL**2
00091                                                                   EL6401
00092      EJECT                                                        EL6401
00093                              COPY ELCATTR.                           CL**2
00094                                                                   EL6401
00095      EJECT                                                        EL6401
00096                              COPY ELCEMIB.                           CL**2
00097                                                                   EL6401
00098      EJECT                                                        EL6401
00099                              COPY ELCINTF.                           CL**2
00100      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.                EL6401
00101          16  PI-SAV-ERBILL-KEY.                                   EL6401
00102              20  PI-SAV-CO-CD            PIC X.                   EL6401
00103              20  PI-SAV-COMPARE.                                  EL6401
00104                  24  PI-SAV-CARRIER      PIC X.                   EL6401
00105                  24  PI-SAV-GROUP        PIC X(6).                EL6401
00106                  24  PI-SAV-ACCOUNT      PIC X(10).               EL6401
00107                  24  PI-SAV-FIN-RESP     PIC X(10).               EL6401
00108              20  PI-SAV-REC-TYPE         PIC X.                   EL6401
00109              20  PI-SAV-LINE-SEQ-NO      PIC S9(4)   COMP.        EL6401
00110          16  PI-VIEW-SW                  PIC X.                   EL6401
00111          16  PI-PREV-LINE-SEQ-NO         PIC S9(4)   COMP.        EL6401
00112          16  PI-FIRST-TIME-SW            PIC X.                   EL6401
00113              88  PI-FIRST-TIME         VALUE 'Y'.                 EL6401
00114          16  PI-ACCT-EOF-SW              PIC X.                   EL6401
00115              88  PI-ACCT-EOF           VALUE 'Y'.                 EL6401
00116          16  FILLER                      PIC X(604).                 CL**4
00117      EJECT                                                        EL6401
00118                              COPY ELCJPFX.                           CL**2
00119                              PIC X(750).                             CL**3
00120                                                                   EL6401
00121      EJECT                                                        EL6401
00122                              COPY ELCAID.                            CL**2
00123  01  FILLER    REDEFINES DFHAID.                                  EL6401
00124      12  FILLER              PIC X(8).                            EL6401
00125      12  PF-VALUES           PIC X       OCCURS 2.                EL6401
00126                                                                   EL6401
00127      EJECT                                                        EL6401
00128                               COPY EL6401S.                          CL**2
00129  01  MAP-A REDEFINES EL6401AO.                                    EL6401
00130      12  FILLER                  PIC X(77).                       EL6401
00131      12  DATA-AREA               PIC X(1312).                     EL6401
00132                                                                   EL6401
00133      EJECT                                                        EL6401
00134  LINKAGE SECTION.                                                 EL6401
00135  01  DFHCOMMAREA             PIC X(1024).                            CL**4
00136                                                                   EL6401
00137      EJECT                                                        EL6401
00138 *01 PARMLIST .                                                       CL**4
00139 *    02  FILLER              PIC S9(8)   COMP.                       CL**4
00140 *    02  ERBILL-POINTER      PIC S9(8)   COMP.                       CL**4
00141 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                       CL**4
00142                                                                   EL6401
00143                              COPY ERCBILL.                           CL**2
00144      EJECT                                                        EL6401
00145                              COPY ELCCNTL.                           CL**2
00146      EJECT                                                        EL6401
00147  PROCEDURE DIVISION.                                              EL6401
00148      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL6401
00149                                                                   EL6401
00150      IF EIBCALEN = 0                                              EL6401
00151          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL6401
00152                                                                   EL6401
00153      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL6401
00154      MOVE '5'                    TO DC-OPTION-CODE.               EL6401
00155      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL6401
00156      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DATE.              EL6401
00157                                                                   EL6401
00158      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL6401
00159          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL6401
00160              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL6401
00161              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL6401
00162              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL6401
00163              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL6401
00164              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL6401
00165              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL6401
00166              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL6401
00167              MOVE SPACES               TO PI-PROGRAM-WORK-AREA    EL6401
00168              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL6401
00169            ELSE                                                   EL6401
00170              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL6401
00171              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL6401
00172              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL6401
00173              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL6401
00174              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL6401
00175              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL6401
00176              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL6401
00177              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL6401
00178                                                                   EL6401
00179      MOVE LOW-VALUES             TO EL6401AI.                     EL6401
00180                                                                   EL6401
00181      IF EIBTRNID NOT = TRANS-ID                                   EL6401
00182          MOVE SPACES             TO PI-SAV-ERBILL-KEY             EL6401
00183          MOVE '3'                TO PI-SAV-REC-TYPE               EL6401
00184          MOVE +0                 TO PI-SAV-LINE-SEQ-NO            EL6401
00185          MOVE '1'                TO PI-VIEW-SW                    EL6401
00186          MOVE 'Y'                TO PI-FIRST-TIME-SW              EL6401
00187          IF EIBTRNID = EL640-TRANS-ID OR EL642-TRANS-ID           EL6401
00188              MOVE -1             TO PRINTERL                      EL6401
00189              MOVE DFHENTER       TO EIBAID                        EL6401
00190              IF PI-CR-CONTROL-IN-PROGRESS = SPACES                EL6401
00191                  GO TO 8100-SEND-INITIAL-MAP                      EL6401
00192              ELSE                                                 EL6401
00193                  MOVE PI-CR-CARRIER  TO CARRIERI                  EL6401
00194                  MOVE +1             TO CARRIERL                  EL6401
00195                  MOVE PI-CR-GROUPING TO GROUPI                    EL6401
00196                  MOVE +3             TO GROUPL                    EL6401
00197                  MOVE PI-CR-ACCOUNT  TO ACCOUNTI                  EL6401
00198                  MOVE +10            TO ACCOUNTL                  EL6401
00199                  MOVE PI-CR-FIN-RESP TO FINRESPI                  EL6401
00200                  MOVE +10            TO FINRESPL                  EL6401
00201                  GO TO 0330-EDIT-DATA                             EL6401
00202          ELSE                                                     EL6401
00203              GO TO 8100-SEND-INITIAL-MAP.                         EL6401
00204                                                                   EL6401
00205      EXEC CICS HANDLE CONDITION                                   EL6401
00206          PGMIDERR  (9600-PGMID-ERROR)                             EL6401
00207          ERROR     (9990-ABEND)                                   EL6401
00208      END-EXEC.                                                    EL6401
00209                                                                   EL6401
00210      IF EIBAID = DFHCLEAR                                         EL6401
00211          GO TO 9400-CLEAR.                                        EL6401
00212                                                                   EL6401
00213      EJECT                                                        EL6401
00214  0200-RECEIVE.                                                    EL6401
00215      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL6401
00216          MOVE 0008               TO EMI-ERROR                     EL6401
00217          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6401
00218          MOVE -1                 TO PFENTERL                      EL6401
00219          GO TO 8200-SEND-DATAONLY.                                EL6401
00220                                                                   EL6401
00221      EXEC CICS RECEIVE                                            EL6401
00222          MAP      (MAP-NAME)                                      EL6401
00223          MAPSET   (MAPSET-NAME)                                   EL6401
00224          INTO     (EL6401AI)                                      EL6401
00225      END-EXEC.                                                    EL6401
00226                                                                   EL6401
00227      IF PFENTERL = 0                                              EL6401
00228          GO TO 0300-CHECK-PFKEYS.                                 EL6401
00229      IF EIBAID NOT = DFHENTER                                     EL6401
00230          MOVE 0004               TO EMI-ERROR                     EL6401
00231          GO TO 0320-INPUT-ERROR.                                  EL6401
00232                                                                   EL6401
00233      IF PFENTERI GREATER 0 AND LESS 25                            EL6401
00234          MOVE PF-VALUES (PFENTERI) TO EIBAID                      EL6401
00235      ELSE                                                         EL6401
00236          MOVE 0029               TO EMI-ERROR                     EL6401
00237          GO TO 0320-INPUT-ERROR.                                  EL6401
00238                                                                   EL6401
00239  0300-CHECK-PFKEYS.                                               EL6401
00240      IF EIBAID = DFHPF23                                          EL6401
00241          GO TO 8810-PF23.                                         EL6401
00242      IF EIBAID = DFHPF24                                          EL6401
00243          GO TO 9200-RETURN-MAIN-MENU.                             EL6401
00244      IF EIBAID = DFHPF12                                          EL6401
00245          GO TO 9500-PF12.                                         EL6401
00246      IF EIBAID = DFHENTER                                         EL6401
00247               OR DFHPF1                                           EL6401
00248               OR DFHPF2                                           EL6401
00249               OR DFHPF3                                           EL6401
00250               OR DFHPF4                                           EL6401
00251               OR DFHPF6                                           EL6401
00252               OR DFHPF7                                           EL6401
00253          GO TO 0330-EDIT-DATA.                                    EL6401
00254                                                                   EL6401
00255      IF EIBAID = DFHPF5                                           EL6401
00256          GO TO 1010-SKIP-ADD.                                     EL6401
00257                                                                   EL6401
00258  0320-INPUT-ERROR.                                                EL6401
00259      MOVE 0029                   TO EMI-ERROR.                    EL6401
00260      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6401
00261      MOVE AL-UNBON               TO PFENTERA.                     EL6401
00262      MOVE -1                     TO PFENTERL.                     EL6401
00263      GO TO 8200-SEND-DATAONLY.                                    EL6401
00264                                                                   EL6401
00265      EJECT                                                        EL6401
00266  0330-EDIT-DATA.                                                  EL6401
00267      MOVE PI-COMPANY-CD          TO PI-SAV-CO-CD.                 EL6401
00268                                                                   EL6401
00269      IF EIBAID = DFHPF1                                           EL6401
00270        AND PI-FIRST-TIME                                          EL6401
00271          MOVE DFHPF5             TO EIBAID                        EL6401
00272          GO TO 1000-BROWSE-FORWARD.                               EL6401
00273                                                                   EL6401
00274      IF    CARRIERL = ZEROS                                       EL6401
00275        AND   GROUPL = ZEROS                                       EL6401
00276        AND ACCOUNTL = ZEROS                                       EL6401
00277        AND FINRESPL = ZEROS                                       EL6401
00278          MOVE 2418               TO EMI-ERROR                     EL6401
00279          MOVE -1                 TO CARRIERL                      EL6401
00280          MOVE AL-UABON           TO CARRIERA                      EL6401
00281                                     GROUPA                        EL6401
00282                                     ACCOUNTA                      EL6401
00283                                     FINRESPA                      EL6401
00284          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6401
00285          GO TO 8200-SEND-DATAONLY.                                EL6401
00286                                                                   EL6401
00287      IF PI-NO-CARRIER-SECURITY AND PI-NO-ACCOUNT-SECURITY         EL6401
00288         GO TO 340-EDIT-DATA.                                      EL6401
00289                                                                   EL6401
00290      IF PI-CARRIER-SECURITY GREATER THAN SPACES                   EL6401
00291         IF CARRIERL GREATER THAN ZEROS                            EL6401
00292            IF PI-CARRIER-SECURITY = CARRIERI                      EL6401
00293               MOVE AL-UANON      TO CARRIERA                      EL6401
00294              ELSE                                                 EL6401
00295               MOVE +2370         TO EMI-ERROR                     EL6401
00296               MOVE -1             TO CARRIERL                     EL6401
00297               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           EL6401
00298                                                                   EL6401
00299      IF PI-ACCOUNT-SECURITY GREATER THAN SPACES                   EL6401
00300         IF ACCOUNTL GREATER THAN ZEROS                            EL6401
00301            IF PI-ACCOUNT-SECURITY = ACCOUNTI                      EL6401
00302               MOVE AL-UANON      TO ACCOUNTA                      EL6401
00303              ELSE                                                 EL6401
00304               MOVE +2371         TO EMI-ERROR                     EL6401
00305               MOVE -1            TO ACCOUNTA                      EL6401
00306               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           EL6401
00307                                                                   EL6401
00308      IF EIBAID = DFHPF7                                           EL6401
00309         IF PRINTERL GREATER THAN ZEROS                            EL6401
00310            NEXT SENTENCE                                          EL6401
00311           ELSE                                                    EL6401
00312            MOVE +2384     TO EMI-ERROR                            EL6401
00313            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL6401
00314                                                                   EL6401
00315      IF EMI-ERROR NOT = ZEROS                                     EL6401
00316         GO TO 8200-SEND-DATAONLY.                                 EL6401
00317                                                                   EL6401
00318  340-EDIT-DATA.                                                   EL6401
00319      IF CARRIERL NOT = ZEROS                                      EL6401
00320          MOVE AL-UANON           TO CARRIERA                      EL6401
00321          MOVE CARRIERI           TO ERBILL-CARRIER.               EL6401
00322                                                                   EL6401
00323      IF GROUPL NOT = ZEROS                                        EL6401
00324          MOVE AL-UANON           TO GROUPA                        EL6401
00325          MOVE GROUPI             TO ERBILL-GROUP.                 EL6401
00326                                                                   EL6401
00327      IF ACCOUNTL NOT = ZEROS                                      EL6401
00328          MOVE AL-UANON           TO ACCOUNTA                      EL6401
00329          MOVE ACCOUNTI           TO ERBILL-ACCT.                  EL6401
00330                                                                   EL6401
00331      IF FINRESPL NOT = ZEROS                                      EL6401
00332          MOVE AL-UANON           TO FINRESPA                      EL6401
00333          MOVE FINRESPI           TO ERBILL-FIN-RESP.              EL6401
00334                                                                   EL6401
00335      IF ERBILL-COMPARE NOT = PI-SAV-COMPARE                       EL6401
00336          MOVE ERBILL-COMPARE     TO PI-SAV-COMPARE                EL6401
00337          MOVE ZEROS              TO PI-SAV-LINE-SEQ-NO.           EL6401
00338                                                                   EL6401
00339      IF EIBAID = DFHPF1 OR DFHPF3 OR DFHENTER                     EL6401
00340          GO TO 1000-BROWSE-FORWARD.                               EL6401
00341                                                                   EL6401
00342      IF EIBAID = DFHPF2 OR DFHPF4                                 EL6401
00343          GO TO 2000-BROWSE-BACKWARD.                              EL6401
00344                                                                   EL6401
00345      IF EIBAID = DFHPF6                                           EL6401
00346          IF PI-VIEW-SW = '1'                                      EL6401
00347              MOVE '2'            TO PI-VIEW-SW                    EL6401
00348              GO TO 1000-BROWSE-FORWARD                            EL6401
00349          ELSE                                                     EL6401
00350              MOVE '1'            TO PI-VIEW-SW                    EL6401
00351              GO TO 1000-BROWSE-FORWARD.                           EL6401
00352                                                                   EL6401
00353      IF EIBAID = DFHPF7                                           EL6401
00354          GO TO 5000-START-PRINT-TRANS.                            EL6401
00355                                                                   EL6401
00356      EJECT                                                        EL6401
00357  1000-BROWSE-FORWARD.                                             EL6401
00358      IF PI-ACCT-EOF                                               EL6401
00359          MOVE SPACE              TO PI-ACCT-EOF-SW                EL6401
00360          GO TO 1010-SKIP-ADD.                                     EL6401
00361                                                                   EL6401
00362      IF EIBAID = DFHPF5                                           EL6401
00363        OR DFHPF6                                                  EL6401
00364          GO TO 1010-SKIP-ADD.                                     EL6401
00365                                                                   EL6401
00366      IF PI-SAV-LINE-SEQ-NO = ZEROS                                EL6401
00367          MOVE 1                  TO PI-SAV-LINE-SEQ-NO            EL6401
00368          GO TO 1010-SKIP-ADD.                                     EL6401
00369                                                                   EL6401
00370      IF EIBAID = DFHPF1 OR DFHENTER                               EL6401
00371          ADD 16                  TO PI-SAV-LINE-SEQ-NO            EL6401
00372      ELSE                                                         EL6401
00373          ADD 8                   TO PI-SAV-LINE-SEQ-NO.           EL6401
00374                                                                   EL6401
00375  1010-SKIP-ADD.                                                   EL6401
00376      PERFORM 3000-FORMAT-TEXT-DISPLAY THRU 3090-EXIT.             EL6401
00377                                                                   EL6401
00378      IF EMI-ERROR NOT = ZEROS                                     EL6401
00379          GO TO 8200-SEND-DATAONLY.                                EL6401
00380                                                                   EL6401
00381      MOVE -1 TO PFENTERL.                                         EL6401
00382      GO TO 8100-SEND-INITIAL-MAP.                                 EL6401
00383                                                                   EL6401
00384  2000-BROWSE-BACKWARD.                                            EL6401
00385      MOVE SPACE                  TO PI-ACCT-EOF-SW.               EL6401
00386                                                                   EL6401
00387      IF EIBAID = DFHPF2                                           EL6401
00388          SUBTRACT 16             FROM PI-SAV-LINE-SEQ-NO          EL6401
00389      ELSE                                                         EL6401
00390          SUBTRACT 8              FROM PI-SAV-LINE-SEQ-NO.         EL6401
00391                                                                   EL6401
00392      IF PI-SAV-LINE-SEQ-NO LESS 2                                 EL6401
00393          MOVE 2419               TO EMI-ERROR                     EL6401
00394          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6401
00395          MOVE ZEROS              TO EMI-ERROR                     EL6401
00396          MOVE 1                  TO PI-SAV-LINE-SEQ-NO.           EL6401
00397                                                                   EL6401
00398      PERFORM 3000-FORMAT-TEXT-DISPLAY THRU 3090-EXIT.             EL6401
00399      IF EMI-ERROR NOT = ZEROS                                     EL6401
00400          GO TO 8200-SEND-DATAONLY.                                EL6401
00401                                                                   EL6401
00402      MOVE -1 TO PFENTERL.                                         EL6401
00403      GO TO 8100-SEND-INITIAL-MAP.                                 EL6401
00404      EJECT                                                        EL6401
00405  3000-FORMAT-TEXT-DISPLAY.                                        EL6401
00406      MOVE LOW-VALUES             TO DATA-AREA.                    EL6401
00407      MOVE 1                      TO TEXT-SUB.                     EL6401
00408      MOVE PI-SAV-ERBILL-KEY      TO ERBILL-KEY.                   EL6401
00409                                                                   EL6401
00410      IF EIBAID = DFHPF5                                           EL6401
00411          MOVE 9999               TO ERBILL-LINE-SEQ-NO.           EL6401
00412                                                                   EL6401
00413      PERFORM 4000-ERBILL-START-BROWSE THRU 4090-EXIT.             EL6401
00414                                                                   EL6401
00415      IF EMI-ERROR NOT = ZEROS                                     EL6401
00416          GO TO 3090-EXIT.                                         EL6401
00417                                                                   EL6401
00418  3010-FORMAT-TEXT-LOOP.                                           EL6401
00419      PERFORM 4100-ERBILL-READNEXT THRU 4190-EXIT.                 EL6401
00420                                                                   EL6401
00421      IF EMI-ERROR NOT = ZEROS                                     EL6401
00422          GO TO 3090-EXIT.                                         EL6401
00423                                                                   EL6401
00424      IF BI-COMPANY-CD = PI-SAV-CO-CD    AND                       EL6401
00425         BI-CARRIER    = PI-SAV-CARRIER  AND                       EL6401
00426         BI-GROUPING   = PI-SAV-GROUP    AND                       EL6401
00427         BI-ACCOUNT    = PI-SAV-ACCOUNT  AND                       EL6401
00428         BI-FIN-RESP   = PI-SAV-FIN-RESP                           EL6401
00429          NEXT SENTENCE                                            EL6401
00430      ELSE                                                         EL6401
00431          GO TO 3080-FORMAT-ACCOUNT-EOF.                           EL6401
00432                                                                   EL6401
00433      IF PI-VIEW-SW = '1'                                          EL6401
00434          MOVE BI-TEXT-2-81       TO PRTLINEO (TEXT-SUB)           EL6401
00435      ELSE                                                         EL6401
00436          MOVE BI-TEXT-55-133     TO PRTLINEO (TEXT-SUB).          EL6401
00437                                                                   EL6401
00438      IF TEXT-SUB = 1                                              EL6401
00439          MOVE BI-LINE-SEQ-NO     TO PI-SAV-LINE-SEQ-NO.           EL6401
00440      IF TEXT-SUB LESS THAN 16                                     EL6401
00441          ADD 1                   TO TEXT-SUB                      EL6401
00442          GO TO 3010-FORMAT-TEXT-LOOP.                             EL6401
00443                                                                   EL6401
00444      PERFORM 4200-ERBILL-END-BROWSE THRU 4290-EXIT.               EL6401
00445                                                                   EL6401
00446      IF PI-FIRST-TIME                                             EL6401
00447          MOVE SPACE              TO PI-FIRST-TIME-SW.             EL6401
00448      GO TO 3090-EXIT.                                             EL6401
00449                                                                   EL6401
00450  3080-FORMAT-ACCOUNT-EOF.                                         EL6401
00451      IF TEXT-SUB = 1                                              EL6401
00452          PERFORM 4010-REC-NOT-FND THRU 4090-EXIT                  EL6401
00453          GO TO 3090-EXIT.                                         EL6401
00454                                                                   EL6401
00455      PERFORM 3100-BLANK-TEXT THRU 3190-EXIT                       EL6401
00456          UNTIL TEXT-SUB GREATER THAN 16.                          EL6401
00457                                                                   EL6401
00458      PERFORM 4200-ERBILL-END-BROWSE THRU 4290-EXIT.               EL6401
00459                                                                   EL6401
00460      MOVE 'Y'                    TO PI-ACCT-EOF-SW.               EL6401
00461                                                                   EL6401
00462      IF BI-CARRIER = HIGH-VALUES                                  EL6401
00463          MOVE 2416               TO EMI-ERROR                     EL6401
00464      ELSE                                                         EL6401
00465          MOVE 2417               TO EMI-ERROR.                    EL6401
00466                                                                   EL6401
00467      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6401
00468                                                                   EL6401
00469      MOVE ZEROS                  TO EMI-ERROR.                    EL6401
00470                                                                   EL6401
00471  3090-EXIT.                                                       EL6401
00472      EXIT.                                                        EL6401
00473      EJECT                                                        EL6401
00474  3100-BLANK-TEXT.                                                 EL6401
00475      MOVE SPACES                 TO PRTLINEO (TEXT-SUB)           EL6401
00476      ADD 1                       TO TEXT-SUB.                     EL6401
00477                                                                   EL6401
00478  3190-EXIT.                                                       EL6401
00479      EXIT.                                                        EL6401
00480      EJECT                                                        EL6401
00481  4000-ERBILL-START-BROWSE.                                        EL6401
00482      EXEC CICS HANDLE CONDITION                                   EL6401
00483          NOTFND (4010-REC-NOT-FND)                                EL6401
00484      END-EXEC.                                                    EL6401
00485                                                                   EL6401
00486      EXEC CICS STARTBR                                            EL6401
00487          DATASET(ERBILL-FILE-ID)                                  EL6401
00488          RIDFLD(ERBILL-KEY)                                       EL6401
00489      END-EXEC.                                                    EL6401
00490                                                                   EL6401
00491      GO TO 4090-EXIT.                                             EL6401
00492                                                                   EL6401
00493  4010-REC-NOT-FND.                                                EL6401
00494      IF EIBAID = DFHPF5                                           EL6401
00495          MOVE 2420               TO EMI-ERROR                     EL6401
00496          MOVE -1                 TO CARRIERL                      EL6401
00497          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6401
00498          GO TO 4090-EXIT.                                         EL6401
00499                                                                   EL6401
00500      MOVE -1                     TO CARRIERL.                     EL6401
00501      MOVE 2212                   TO EMI-ERROR.                    EL6401
00502      MOVE AL-UABON               TO CARRIERA                      EL6401
00503                                     GROUPA                        EL6401
00504                                     ACCOUNTA                      EL6401
00505                                     FINRESPA.                     EL6401
00506                                                                   EL6401
00507      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6401
00508                                                                   EL6401
00509  4090-EXIT.                                                       EL6401
00510      EXIT.                                                        EL6401
00511      EJECT                                                        EL6401
00512  4100-ERBILL-READNEXT.                                            EL6401
00513      EXEC CICS HANDLE CONDITION                                   EL6401
00514          ENDFILE (4110-END-OF-FILE)                               EL6401
00515      END-EXEC.                                                    EL6401
00516                                                                   EL6401
00517      EXEC CICS READNEXT                                           EL6401
00518          SET (ADDRESS OF BILLING-STATEMENT)                          CL**4
00519          DATASET (ERBILL-FILE-ID)                                 EL6401
00520          RIDFLD (ERBILL-KEY)                                      EL6401
00521      END-EXEC.                                                    EL6401
00522                                                                   EL6401
00523      IF EIBAID = DFHPF5                                           EL6401
00524          IF BI-COMPANY-CD = PI-SAV-CO-CD                          EL6401
00525              MOVE BI-CARRIER     TO PI-SAV-CARRIER   CARRIERO     EL6401
00526              MOVE BI-GROUPING    TO PI-SAV-GROUP     GROUPO       EL6401
00527              MOVE BI-ACCOUNT     TO PI-SAV-ACCOUNT   ACCOUNTO     EL6401
00528              MOVE BI-FIN-RESP    TO PI-SAV-FIN-RESP  FINRESPO     EL6401
00529              MOVE AL-UANON       TO CARRIERA                      EL6401
00530                                     GROUPA                        EL6401
00531                                     ACCOUNTA                      EL6401
00532                                     FINRESPA                      EL6401
00533          ELSE                                                     EL6401
00534              GO TO 4110-END-OF-FILE.                              EL6401
00535                                                                   EL6401
00536      IF  BI-RECORD-TYPE NOT = '3'                                 EL6401
00537          GO TO 4100-ERBILL-READNEXT.                              EL6401
00538                                                                   EL6401
00539      IF PI-CARRIER-SECURITY GREATER THAN SPACES                   EL6401
00540         IF  BI-CARRIER = PI-CARRIER-SECURITY                      EL6401
00541             NEXT SENTENCE                                         EL6401
00542            ELSE                                                   EL6401
00543             GO TO 4100-ERBILL-READNEXT.                           EL6401
00544                                                                   EL6401
00545      IF PI-ACCOUNT-SECURITY GREATER THAN SPACES                   EL6401
00546         IF  BI-ACCOUNT = PI-ACCOUNT-SECURITY                      EL6401
00547             NEXT SENTENCE                                         EL6401
00548            ELSE                                                   EL6401
00549             GO TO 4100-ERBILL-READNEXT.                           EL6401
00550                                                                   EL6401
00551      GO TO 4190-EXIT.                                             EL6401
00552                                                                   EL6401
00553  4110-END-OF-FILE.                                                EL6401
00554      MOVE HIGH-VALUES            TO BI-CONTROL-PRIMARY.           EL6401
00555                                                                   EL6401
00556  4190-EXIT.                                                       EL6401
00557      EXIT.                                                        EL6401
00558      EJECT                                                        EL6401
00559  4200-ERBILL-END-BROWSE.                                          EL6401
00560      EXEC CICS ENDBR                                              EL6401
00561          DATASET (ERBILL-FILE-ID)                                 EL6401
00562      END-EXEC.                                                    EL6401
00563                                                                   EL6401
00564  4290-EXIT.                                                       EL6401
00565      EXIT.                                                        EL6401
00566      EJECT                                                        EL6401
00567  5000-START-PRINT-TRANS.                                          EL6401
00568      MOVE PI-COMPANY-ID          TO ELCNTL-CO-ID.                 EL6401
CIDMOD     MOVE '9'                    TO ELCNTL-REC-TYPE.                   000
00569      EXEC CICS HANDLE CONDITION                                   EL6401
00570          NOTFND (5100-NO-COMPANY-REC)                             EL6401
00571      END-EXEC.                                                    EL6401
00572                                                                   EL6401
00573      EXEC CICS READ                                               EL6401
00574          DATASET (ELCNTL-FILE-ID)                                 EL6401
00575          SET     (ADDRESS OF CONTROL-FILE)                           CL**4
00576          RIDFLD  (ELCNTL-KEY)                                     EL6401
00577      END-EXEC.                                                    EL6401
00578                                                                   EL6401
00579      MOVE SPACES                  TO PI-ALT-DMD-PRT-ID.              CL**5
00580      IF PRINTERL GREATER ZERO                                     EL6401
00581          MOVE PRINTERI            TO PRINT-TERMID                 EL6401
00582                                      PI-ALT-DMD-PRT-ID               CL**5
00583      ELSE                                                         EL6401
00584          IF PI-PROCESSOR-PRINTER NOT = SPACES                        CL**2
00585             MOVE PI-PROCESSOR-PRINTER TO PRINT-TERMID                CL**2
00586          ELSE                                                        CL**2
00587             MOVE CF-FORMS-PRINTER-ID  TO PRINT-TERMID.               CL**2
00588                                                                   EL6401
00589      MOVE PI-SAV-CARRIER         TO PI-CR-CARRIER.                EL6401
00590      MOVE PI-SAV-GROUP           TO PI-CR-GROUPING.               EL6401
00591      MOVE PI-SAV-ACCOUNT         TO PI-CR-ACCOUNT.                EL6401
00592      MOVE PI-SAV-FIN-RESP        TO PI-CR-FIN-RESP.               EL6401
00593      MOVE ' 4'                   TO PI-ENTRY-CODES.               EL6401
00594                                                                   EL6401
00595      EXEC CICS HANDLE CONDITION                                   EL6401
00596          TERMIDERR (5100-INVALID-TERMID)                          EL6401
00597      END-EXEC.                                                    EL6401
00598                                                                   EL6401
062121     IF PI-COMPANY-ID = 'DMD' OR 'CID' OR 'AHL' OR 'FNL'
00600 *        MOVE EIBTRMID       TO PRINT-TRANS-ID                       CL**5
CIDMOD         MOVE CF-FORMS-PRINTER-ID  TO  PI-PROCESSOR-PRINTER            000
00601          EXEC CICS START                                             CL**5
00602              TRANSID (PRINT-TRANS-ID)                                CL**5
00603              FROM    (PROGRAM-INTERFACE-BLOCK)                       CL**5
00604              LENGTH  (PI-COMM-LENGTH)                                CL**5
00605 *            TERMID  (PRINT-TERMID)                                  CL**5
00606          END-EXEC                                                    CL**5
00607      ELSE                                                            CL**5
00608          EXEC CICS START                                             CL**5
00609              TRANSID (PRINT-TRANS-ID)                                CL**5
00610              FROM    (PROGRAM-INTERFACE-BLOCK)                       CL**5
00611              LENGTH  (PI-COMM-LENGTH)                                CL**5
00612              TERMID  (PRINT-TERMID)                                  CL**5
00613          END-EXEC.                                                   CL**5
00614                                                                   EL6401
00615      MOVE 2360                   TO EMI-ERROR.                    EL6401
00616      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6401
00617      MOVE -1                     TO PFENTERL.                     EL6401
00618      GO TO 8200-SEND-DATAONLY.                                    EL6401
00619                                                                   EL6401
00620  5100-INVALID-TERMID.                                             EL6401
00621      MOVE 2368                   TO EMI-ERROR.                    EL6401
00622      MOVE -1                     TO PRINTERL.                     EL6401
00623      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6401
00624      MOVE LOW-VALUES             TO DATA-AREA.                    EL6401
00625      GO TO 8200-SEND-DATAONLY.                                    EL6401
00626      EJECT                                                        EL6401
00627  5100-NO-COMPANY-REC.                                             EL6401
00628      MOVE 2248                   TO EMI-ERROR.                    EL6401
00629      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6401
00630      MOVE LOW-VALUES             TO DATA-AREA.                    EL6401
00631      GO TO 8200-SEND-DATAONLY.                                    EL6401
00632      EJECT                                                        EL6401
00633  8100-SEND-INITIAL-MAP.                                           EL6401
00634      MOVE WS-CURRENT-DATE        TO DATEO.                        EL6401
00635      MOVE EIBTIME                TO TIME-IN.                      EL6401
00636      MOVE TIME-OUT               TO TIMEO.                        EL6401
00637                                                                   EL6401
00638      IF PI-RETURN-TO-PROGRAM = EL640                              EL6401
00639          MOVE -1                 TO PRINTERL                      EL6401
00640      ELSE                                                         EL6401
00641          MOVE -1                 TO CARRIERL.                     EL6401
00642                                                                   EL6401
00643      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                      EL6401
00644                                                                   EL6401
00645      EXEC CICS SEND                                               EL6401
00646          MAP      (MAP-NAME)                                      EL6401
00647          MAPSET   (MAPSET-NAME)                                   EL6401
00648          FROM     (EL6401AO)                                      EL6401
00649          ERASE                                                    EL6401
00650          CURSOR                                                   EL6401
00651      END-EXEC.                                                    EL6401
00652                                                                   EL6401
00653      GO TO 9100-RETURN-TRAN.                                      EL6401
00654                                                                   EL6401
00655  8200-SEND-DATAONLY.                                              EL6401
00656      IF EIBTRNID NOT = TRANS-ID                                   EL6401
00657          GO TO 8100-SEND-INITIAL-MAP.                             EL6401
00658                                                                   EL6401
00659      MOVE WS-CURRENT-DATE        TO DATEO.                        EL6401
00660      MOVE EIBTIME                TO TIME-IN.                      EL6401
00661      MOVE TIME-OUT               TO TIMEO.                        EL6401
00662      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO                       EL6401
00663      EXEC CICS SEND                                               EL6401
00664          MAP      (MAP-NAME)                                      EL6401
00665          MAPSET   (MAPSET-NAME)                                   EL6401
00666          FROM     (EL6401AO)                                      EL6401
00667          DATAONLY                                                 EL6401
00668          ERASEAUP                                                 EL6401
00669          CURSOR                                                   EL6401
00670      END-EXEC.                                                    EL6401
00671                                                                   EL6401
00672      GO TO 9100-RETURN-TRAN.                                      EL6401
00673                                                                   EL6401
00674  8300-SEND-TEXT.                                                  EL6401
00675      EXEC CICS SEND TEXT                                          EL6401
00676          FROM     (LOGOFF-TEXT)                                   EL6401
00677          LENGTH   (LOGOFF-LENGTH)                                 EL6401
00678          ERASE                                                    EL6401
00679          FREEKB                                                   EL6401
00680      END-EXEC.                                                    EL6401
00681                                                                   EL6401
00682      EXEC CICS RETURN                                             EL6401
00683      END-EXEC.                                                    EL6401
00684                                                                   EL6401
00685  8400-LOG-JOURNAL-RECORD.                                         EL6401
00686      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   EL6401
00687      MOVE ERBILL-FILE-ID         TO JP-FILE-ID.                   EL6401
00688      MOVE THIS-PGM               TO JP-PROGRAM-ID.                EL6401
00689                                                                   EL6401
00690 *    EXEC CICS JOURNAL                                            EL6401
00691 *        JFILEID     (PI-JOURNAL-FILE-ID)                         EL6401
00692 *        JTYPEID     ('EL')                                       EL6401
00693 *        FROM        (JOURNAL-RECORD)                             EL6401
00694 *        LENGTH      (773)                                           CL**3
00695 *        END-EXEC.                                                EL6401
00696                                                                   EL6401
00697  8500-DATE-CONVERT.                                               EL6401
00698      MOVE LINK-ELDATCV           TO PGM-NAME.                     EL6401
00699      EXEC CICS LINK                                               EL6401
00700          PROGRAM    (PGM-NAME)                                    EL6401
00701          COMMAREA   (DATE-CONVERSION-DATA)                        EL6401
00702          LENGTH     (DC-COMM-LENGTH)                              EL6401
00703          END-EXEC.                                                EL6401
00704  8500-EXIT.                                                       EL6401
00705      EXIT.                                                        EL6401
00706                                                                   EL6401
00707  8800-UNAUTHORIZED-ACCESS.                                        EL6401
00708      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL6401
00709      GO TO 8300-SEND-TEXT.                                        EL6401
00710                                                                   EL6401
00711  8810-PF23.                                                       EL6401
00712      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL6401
00713      MOVE XCTL-005               TO PGM-NAME.                     EL6401
00714      GO TO 9300-XCTL.                                             EL6401
00715  9000-RETURN-CICS.                                                EL6401
00716      EXEC CICS RETURN                                             EL6401
00717          END-EXEC.                                                EL6401
00718                                                                   EL6401
00719  9100-RETURN-TRAN.                                                EL6401
00720      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL6401
00721      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.         EL6401
00722      EXEC CICS RETURN                                             EL6401
00723          TRANSID    (TRANS-ID)                                    EL6401
00724          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6401
00725          LENGTH     (PI-COMM-LENGTH)                              EL6401
00726          END-EXEC.                                                EL6401
00727                                                                   EL6401
00728  9200-RETURN-MAIN-MENU.                                           EL6401
00729      MOVE XCTL-626               TO PGM-NAME.                     EL6401
00730      GO TO 9300-XCTL.                                             EL6401
00731                                                                   EL6401
00732  9300-XCTL.                                                       EL6401
00733      EXEC CICS XCTL                                               EL6401
00734          PROGRAM    (PGM-NAME)                                    EL6401
00735          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6401
00736          LENGTH     (PI-COMM-LENGTH)                              EL6401
00737          END-EXEC.                                                EL6401
00738                                                                   EL6401
00739  9400-CLEAR.                                                      EL6401
00740      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME                      EL6401
00741      GO TO 9300-XCTL.                                             EL6401
00742                                                                   EL6401
00743  9500-PF12.                                                       EL6401
00744      MOVE XCTL-010               TO PGM-NAME.                     EL6401
00745      GO TO 9300-XCTL.                                             EL6401
00746                                                                   EL6401
00747  9600-PGMID-ERROR.                                                EL6401
00748      EXEC CICS HANDLE CONDITION                                   EL6401
00749          PGMIDERR    (8300-SEND-TEXT)                             EL6401
00750      END-EXEC.                                                    EL6401
00751                                                                   EL6401
00752      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL6401
00753      MOVE ' '                    TO PI-ENTRY-CD-1.                EL6401
00754      MOVE XCTL-005               TO PGM-NAME.                     EL6401
00755      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL6401
00756      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL6401
00757      GO TO 9300-XCTL.                                             EL6401
00758                                                                   EL6401
00759  9900-ERROR-FORMAT.                                               EL6401
00760      IF NOT EMI-ERRORS-COMPLETE                                   EL6401
00761          MOVE LINK-001           TO PGM-NAME                      EL6401
00762          EXEC CICS LINK                                           EL6401
00763              PROGRAM    (PGM-NAME)                                EL6401
00764              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL6401
00765              LENGTH     (EMI-COMM-LENGTH)                         EL6401
00766         END-EXEC.                                                 EL6401
00767                                                                   EL6401
00768  9900-EXIT.                                                       EL6401
00769      EXIT.                                                        EL6401
00770                                                                   EL6401
00771  9990-ABEND.                                                      EL6401
00772      MOVE LINK-004               TO PGM-NAME.                     EL6401
00773      MOVE DFHEIBLK               TO EMI-LINE1                     EL6401
00774      EXEC CICS LINK                                               EL6401
00775          PROGRAM   (PGM-NAME)                                     EL6401
00776          COMMAREA  (EMI-LINE1)                                    EL6401
00777          LENGTH    (72)                                           EL6401
00778      END-EXEC.                                                    EL6401
00779                                                                   EL6401
00780      MOVE -1                     TO PFENTERL.                     EL6401
00781      GO TO 8200-SEND-DATAONLY.                                    EL6401
00782      GOBACK.                                                      EL6401
00783                                                                   EL6401
00784  9995-SECURITY-VIOLATION.                                         EL6401
00785                              COPY ELCSCTP.                        EL6401
00786                                                                   EL6401
00787  9995-EXIT.                                                       EL6401
00788      EXIT.                                                        EL6401
00789                                                                   EL6401
00790                                                                   EL6401
