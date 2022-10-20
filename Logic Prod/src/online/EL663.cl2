00001  ID DIVISION.                                                     10/07/98
00002                                                                   EL663
00003  PROGRAM-ID.                 EL663.                                  LV008
00004 *              PROGRAM CONVERTED BY                                  CL**4
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**4
00006 *              CONVERSION DATE 02/14/96 07:26:39.                    CL**4
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             CL**7
00008 *                            VMOD=2.005                              CL**7
00009                                                                   EL663
00010 *AUTHOR.     LOGIC,INC.                                              CL**4
00011 *            DALLAS, TEXAS.                                          CL**4
00012                                                                   EL663
00013 *DATE-COMPILED.                                                      CL**4
00014 *SECURITY.   *****************************************************   CL**4
00015 *            *                                                   *   CL**4
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**4
00017 *            *                                                   *   CL**4
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**4
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**4
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**4
00021 *            *                                                   *   CL**4
00022 *            *****************************************************   CL**4
00023                                                                   EL663
00024 *REMARKS.    TRANSACTION - EXK3 - PC ACCOUNT BALANCING.              CL**4
00025                                                                   EL663
00026  ENVIRONMENT DIVISION.                                            EL663
00027                                                                   EL663
00028      EJECT                                                        EL663
00029  DATA DIVISION.                                                   EL663
00030  WORKING-STORAGE SECTION.                                         EL663
00031                                                                   EL663
00032  77  FILLER  PIC X(32)  VALUE '********************************'. EL663
00033  77  FILLER  PIC X(32)  VALUE '*    EL663 WORKING STORAGE     *'. EL663
00034  77  FILLER  PIC X(32)  VALUE '************VMOD=2.005 *********'.    CL**7
00035                                                                   EL663
00036                              COPY ELCSCTM.                           CL**3
00037                              COPY ELCSCRTY.                          CL**3
00038                                                                   EL663
00039      EJECT                                                        EL663
00040                                                                   EL663
00041  01  STANDARD-AREAS.                                              EL663
00042      12  SC-ITEM                 PIC S9(4) COMP VALUE +1.         EL663
00043      12  GETMAIN-SPACE           PIC X       VALUE SPACE.         EL663
00044      12  MAP-NAME                PIC X(8)    VALUE 'EL663A'.      EL663
00045      12  MAPSET-NAME             PIC X(8)    VALUE 'EL663S'.      EL663
00046      12  SCREEN-NUMBER           PIC X(4)    VALUE '630A'.        EL663
00047      12  TRANS-ID                PIC X(4)    VALUE 'EXK3'.        EL663
00048      12  REPT-TRANS              PIC X(4)    VALUE 'EXK2'.        EL663
00049      12  PASS-AREA-LEN           PIC S9(4)   COMP VALUE +16.      EL663
00050      12  THIS-PGM                PIC X(8)    VALUE 'EL663'.       EL663
00051      12  PGM-NAME                PIC X(8).                        EL663
00052      12  TIME-IN                 PIC S9(7).                       EL663
00053      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL663
00054          16  FILLER              PIC X.                           EL663
00055          16  TIME-OUT            PIC 99V99.                       EL663
00056          16  FILLER              PIC XX.                          EL663
00057      12  XCTL-005                PIC X(8)    VALUE 'EL005'.       EL663
00058      12  XCTL-010                PIC X(8)    VALUE 'EL010'.       EL663
00059      12  XCTL-179                PIC X(8)    VALUE 'EL179'.       EL663
00060      12  XCTL-626                PIC X(8)    VALUE 'EL626'.       EL663
00061      12  XCTL-631                PIC X(8)    VALUE 'EL631'.       EL663
00062      12  XCTL-662                PIC X(8)    VALUE 'EL662'.       EL663
00063      12  LINK-001                PIC X(8)    VALUE 'EL001'.       EL663
00064      12  LINK-004                PIC X(8)    VALUE 'EL004'.       EL663
00065      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     EL663
00066      12  ELCNTL-FILE-ID          PIC X(8)    VALUE 'ELCNTL'.      EL663
00067      12  ERPNDB2-FILE-ID         PIC X(8)    VALUE 'ERPNDB2'.     EL663
00068      12  ERACCT-FILE-ID          PIC X(8)    VALUE 'ERACCT'.      EL663
00069      12  ERACCT2-FILE-ID         PIC X(8)    VALUE 'ERACCT2'.     EL663
00070      12  WS-CURRENT-BIN-DT       PIC XX      VALUE SPACES.        EL663
00071      12  WS-CURRENT-DT           PIC X(8)    VALUE SPACES.        EL663
00072      12  WS-CURRENT-YMD          PIC 9(6)    VALUE ZEROS.         EL663
00073      12  WS-SYNC-CNTR            PIC S9(3)   VALUE +0 COMP-3.     EL663
00074      12  WS-SUB1                 PIC S9(4)   VALUE +0 COMP.       EL663
00075      12  WS-SUB2                 PIC S9(4)   VALUE +0 COMP.       EL663
00076      12  WS-MNTHNDT              PIC X(8)    VALUE SPACES.        EL663
00077                                                                   EL663
00078      12  WS-RECORD-LENGTHS   COMP.                                EL663
00079         16  WS-ERPNDB2-RECORD-LENGTH PIC S9(4)   VALUE +585.      EL663
00080         16  WS-ELCNTL-RECORD-LENGTH  PIC S9(4)   VALUE +504.      EL663
00081                                                                   EL663
00082      EJECT                                                        EL663
00083  01  ERROR-MESSAGES.                                              EL663
00084      12  ER-0000                 PIC X(4)    VALUE '0000'.        EL663
00085      12  ER-0008                 PIC X(4)    VALUE '0008'.        EL663
00086      12  ER-0023                 PIC X(4)    VALUE '0023'.        EL663
00087      12  ER-0029                 PIC X(4)    VALUE '0029'.        EL663
00088      12  ER-0070                 PIC X(4)    VALUE '0070'.        EL663
00089      12  ER-0189                 PIC X(4)    VALUE '0189'.        EL663
00090      12  ER-0194                 PIC X(4)    VALUE '0194'.        EL663
00091      12  ER-0195                 PIC X(4)    VALUE '0195'.        EL663
00092      12  ER-0196                 PIC X(4)    VALUE '0196'.        EL663
00093      12  ER-0197                 PIC X(4)    VALUE '0197'.        EL663
00094      12  ER-0340                 PIC X(4)    VALUE '0340'.        EL663
00095      12  ER-0587                 PIC X(4)    VALUE '0587'.        EL663
00096      12  ER-2119                 PIC X(4)    VALUE '2119'.        EL663
00097      12  ER-2126                 PIC X(4)    VALUE '2126'.        EL663
00098      12  ER-2132                 PIC X(4)    VALUE '2132'.        EL663
00099      12  ER-2201                 PIC X(4)    VALUE '2201'.        EL663
00100      12  ER-2208                 PIC X(4)    VALUE '2208'.        EL663
00101      12  ER-2209                 PIC X(4)    VALUE '2209'.        EL663
00102      12  ER-2210                 PIC X(4)    VALUE '2210'.        EL663
00103      12  ER-2211                 PIC X(4)    VALUE '2211'.        EL663
00104      12  ER-2212                 PIC X(4)    VALUE '2212'.        EL663
00105      12  ER-2213                 PIC X(4)    VALUE '2213'.        EL663
00106      12  ER-2214                 PIC X(4)    VALUE '2214'.        EL663
00107      12  ER-2215                 PIC X(4)    VALUE '2215'.        EL663
00108      12  ER-2216                 PIC X(4)    VALUE '2216'.        EL663
00109      12  ER-2229                 PIC X(4)    VALUE '2229'.        EL663
00110      12  ER-2242                 PIC X(4)    VALUE '2242'.        EL663
00111      12  ER-2248                 PIC X(4)    VALUE '2248'.        EL663
00112      12  ER-2370                 PIC X(4)    VALUE '2370'.        EL663
00113      12  ER-2371                 PIC X(4)    VALUE '2371'.        EL663
00114      12  ER-2402                 PIC X(4)    VALUE '2402'.        EL663
00115      12  ER-2422                 PIC X(4)    VALUE '2422'.        EL663
00116      12  ER-2800                 PIC X(4)    VALUE '2800'.        EL663
00117      12  ER-2880                 PIC X(4)    VALUE '2880'.        EL663
00118                                                                   EL663
00119      EJECT                                                        EL663
00120  01  ACCESS-KEYS.                                                 EL663
00121      12  ELCNTL-KEY.                                              EL663
00122          16  CNTL-COMP-ID        PIC X(3)  VALUE SPACES.          EL663
00123          16  CNTL-REC-TYPE       PIC X     VALUE SPACES.          EL663
00124          16  CNTL-ACCESS.                                         EL663
00125              20  CNTL-STATE      PIC XX    VALUE SPACES.          EL663
00126              20  FILLER          PIC X     VALUE SPACES.          EL663
00127              20  CNTL-CARRIER    PIC X     VALUE SPACES.          EL663
00128          16  CNTL-SEQ            PIC S9(4) VALUE +0 COMP.         EL663
00129                                                                   EL663
00130      12  ERPNDB2-KEY.                                             EL663
00131          16  PNDB-COMP-CD        PIC X     VALUE SPACE.           EL663
00132          16  PNDB-CARRIER        PIC X     VALUE SPACES.          EL663
00133          16  PNDB-GROUPING       PIC X(6)  VALUE SPACES.          EL663
00134          16  PNDB-STATE          PIC XX    VALUE SPACES.          EL663
00135          16  PNDB-ACCOUNT        PIC X(10) VALUE SPACES.          EL663
00136          16  PNDB-CERT-EFFDT     PIC XX    VALUE SPACES.          EL663
00137          16  PNDB-CERT-NO        PIC X(11) VALUE SPACES.          EL663
00138          16  PNDB-ALT-CHG-SEQ    PIC S9(4) VALUE +0 COMP.         EL663
00139                                                                   EL663
00140      12  ERACCT-KEY.                                              EL663
00141          16  ERACCT-COMP-KEY.                                     EL663
00142              20  ACCT-CO         PIC X     VALUE SPACES.          EL663
00143              20  ACCT-CARRIER    PIC X     VALUE SPACES.          EL663
00144              20  ACCT-GROUPING   PIC X(6)  VALUE SPACES.          EL663
00145              20  ACCT-STATE      PIC XX    VALUE SPACES.          EL663
00146              20  ACCT-ACCOUNT    PIC X(10) VALUE SPACES.          EL663
00147          16  ACCT-EXP-DATE       PIC XX    VALUE SPACES.          EL663
00148          16  FILLER              PIC X(4)  VALUE LOW-VALUES.      EL663
00149      12  ERACCT-SAVE-KEY         PIC X(20) VALUE SPACES.          EL663
00150                                                                   EL663
00151  01  FILLER.                                                      EL663
00152      12  WS-DEEDIT-FIELD         PIC S9(8)V99.                    EL663
00153      12  WS-DT-DEEDIT-FIELD REDEFINES                             EL663
00154          WS-DEEDIT-FIELD         PIC X(10).                       EL663
00155      12  WS-OBAL                 PIC S9(8)V99  VALUE ZEROS.       EL663
00156      12  WS-OCNT                 PIC S9(5)     VALUE ZEROS.       EL663
00157                                                                   EL663
00158      12  DATE-TEST-AREA          PIC 9(6).                        EL663
00159      12  DATE-TEST-AREA-R  REDEFINES DATE-TEST-AREA.              EL663
00160          16  DATE-TEST-MM        PIC 99.                          EL663
00161          16  DATE-TEST-DD        PIC 99.                          EL663
00162          16  DATE-TEST-YY        PIC 99.                          EL663
00163      12  DIVIDE-RESULT           PIC 99.                          EL663
00164      12  DIVIDE-REMAINDER        PIC 9.                           EL663
00165      12  WS-CERT-NOTE-SW         PIC X   VALUE ' '.               EL663
00166          88 CERT-NOTES-ARE-PRESENT  VALUE 'Y'.                    EL663
00167      12  WS-CERT-ADDRESS-SW      PIC X   VALUE ' '.               EL663
00168          88 CERT-ADDRESS-PRESENT     VALUE 'Y'.                   EL663
00169      12  WS-PRM-HEADER.                                           EL663
00170          16  WS-PRM-OVERRIDE     PIC XX    VALUE SPACES.          EL663
00171          16  FILLER              PIC X(8)  VALUE '-PREMIUM'.      EL663
00172      12  WS-REFUND-HEADER.                                        EL663
00173          16  WS-REFUND-OVERRIDE  PIC XX    VALUE SPACES.          EL663
00174          16  FILLER              PIC X(7)  VALUE '-REFUND'.       EL663
00175                                                                   EL663
00176      EJECT                                                        EL663
00177                                                                   EL663
00178                              COPY ELCDATE.                           CL**3
00179                                                                   EL663
00180      EJECT                                                        EL663
00181                              COPY ELCLOGOF.                          CL**3
00182                                                                   EL663
00183      EJECT                                                        EL663
00184                              COPY ELCATTR.                           CL**3
00185                                                                   EL663
00186      EJECT                                                        EL663
00187                              COPY ELCEMIB.                           CL**3
00188      EJECT                                                        EL663
00189                                                                   EL663
00190                              COPY ELCINTF.                           CL**3
00191           COPY ELC630PI.                                          EL663
00192                                                                   EL663
00193      EJECT                                                        EL663
00194                              COPY ELCAID.                            CL**3
00195  01  FILLER    REDEFINES DFHAID.                                  EL663
00196      12  FILLER              PIC X(8).                            EL663
00197      12  PF-VALUES           PIC X       OCCURS 2.                EL663
00198                                                                   EL663
00199      EJECT                                                        EL663
00200                              COPY EL663S.                            CL**3
00201                                                                   EL663
00202      EJECT                                                        EL663
00203  LINKAGE SECTION.                                                 EL663
00204  01  DFHCOMMAREA             PIC X(1300).                         EL663
00205                                                                   EL663
00206      EJECT                                                        EL663
00207 *01 PARMLIST       COMP.                                             CL**4
00208 *    02  FILLER              PIC S9(8).                              CL**4
00209 *    02  ELCNTL-POINTER      PIC S9(8).                              CL**4
00210 *    02  ERPNDB2-POINTER     PIC S9(8).                              CL**4
00211 *    02  ERACCT-POINTER      PIC S9(8).                              CL**4
00212                                                                   EL663
00213      EJECT                                                        EL663
00214                              COPY ELCCNTL.                           CL**3
00215      EJECT                                                        EL663
00216                              COPY ERCPNDB.                           CL**3
00217      EJECT                                                        EL663
00218                              COPY ERCACCT.                           CL**3
00219      EJECT                                                        EL663
00220                                                                   EL663
00221  PROCEDURE DIVISION.                                              EL663
00222      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL663
00223      MOVE 2                      TO EMI-NUMBER-OF-LINES.          EL663
00224      IF EIBCALEN = 0                                              EL663
00225          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL663
00226                                                                   EL663
00227      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL663
00228      MOVE '5'                    TO DC-OPTION-CODE.               EL663
00229      MOVE LINK-ELDATCV           TO PGM-NAME.                     EL663
00230                                                                   EL663
00231      EXEC CICS LINK                                               EL663
00232          PROGRAM (PGM-NAME)                                       EL663
00233          COMMAREA(DATE-CONVERSION-DATA)                           EL663
00234          LENGTH  (DC-COMM-LENGTH)                                 EL663
00235      END-EXEC.                                                    EL663
00236                                                                   EL663
00237      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.            EL663
00238      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.                EL663
00239      MOVE DC-GREG-DATE-1-YMD     TO WS-CURRENT-YMD.               EL663
00240                                                                   EL663
00241      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL663
00242          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL663
00243              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL663
00244              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL663
00245              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL663
00246              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL663
00247              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL663
00248              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL663
00249              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL663
00250              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL663
00251          ELSE                                                     EL663
00252              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL663
00253              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL663
00254              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL663
00255              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL663
00256              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL663
00257              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL663
00258              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL663
00259              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL663
00260                                                                   EL663
00261      IF EIBTRNID NOT = TRANS-ID                                   EL663
00262          MOVE LOW-VALUES         TO EL663AI                          CL**4
00263          GO TO 8100-SEND-INITIAL-MAP.                             EL663
00264                                                                   EL663
00265      EXEC CICS HANDLE CONDITION                                   EL663
00266          PGMIDERR  (9600-PGMID-ERROR)                             EL663
00267          ERROR     (9990-ABEND)                                   EL663
00268      END-EXEC.                                                    EL663
00269                                                                   EL663
00270      IF EIBAID = DFHCLEAR                                         EL663
00271         MOVE SPACES               TO PI-ACCT-AGENT-PROCESSED-SW   EL663
00272         GO TO 9400-CLEAR.                                         EL663
00273                                                                   EL663
00274      EJECT                                                        EL663
00275  0200-RECEIVE.                                                    EL663
00276      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL663
00277          MOVE ER-0008            TO EMI-ERROR                     EL663
00278          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL663
00279          MOVE -1                 TO CARRIERL                      EL663
00280          GO TO 8200-SEND-DATAONLY.                                EL663
00281                                                                   EL663
00282      EXEC CICS RECEIVE                                            EL663
00283          MAP      (MAP-NAME)                                      EL663
00284          MAPSET   (MAPSET-NAME)                                   EL663
00285          INTO     (EL663AI)                                       EL663
00286      END-EXEC.                                                    EL663
00287                                                                   EL663
00288      IF PFENTERL = 0                                              EL663
00289          GO TO 0300-CHECK-PFKEYS.                                 EL663
00290                                                                   EL663
00291      IF PFENTERI NUMERIC                                          EL663
00292         IF PFENTERI GREATER 0 AND LESS 25                         EL663
00293            MOVE PF-VALUES (PFENTERI) TO EIBAID                    EL663
00294         ELSE                                                      EL663
00295            MOVE ER-0029            TO EMI-ERROR                   EL663
00296            GO TO 0320-INPUT-ERROR.                                EL663
00297                                                                   EL663
00298  0300-CHECK-PFKEYS.                                               EL663
00299      IF EIBAID = DFHPF23                                          EL663
00300          GO TO 8810-PF23.                                         EL663
00301      IF EIBAID = DFHPF24                                          EL663
00302          GO TO 9200-RETURN-MAIN-MENU.                             EL663
00303      IF EIBAID = DFHPF12                                          EL663
00304          GO TO 9500-PF12.                                         EL663
00305                                                                   EL663
00306      IF EIBAID = DFHPF1                                           EL663
00307          EXEC CICS START                                          EL663
00308              TRANSID       (REPT-TRANS)                           EL663
00309              FROM          (PROGRAM-INTERFACE-BLOCK)              EL663
00310              LENGTH        (PI-COMM-LENGTH)                       EL663
00311          END-EXEC                                                 EL663
00312          MOVE -1             TO CARRIERL                          EL663
00313          MOVE ER-0189        TO EMI-ERROR                         EL663
00314          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL663
00315          GO TO 8200-SEND-DATAONLY.                                EL663
00316                                                                   EL663
00317      IF EIBAID = DFHPF2                                           EL663
00318          MOVE XCTL-179       TO PGM-NAME                          EL663
00319          GO TO 9300-XCTL.                                         EL663
00320                                                                   EL663
00321      IF EIBAID = DFHENTER                                         EL663
00322          GO TO 0330-EDIT-DATA.                                    EL663
00323                                                                   EL663
00324  0320-INPUT-ERROR.                                                EL663
00325      MOVE ER-0029                TO EMI-ERROR.                    EL663
00326      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL663
00327      MOVE AL-UNBON               TO PFENTERA.                     EL663
00328      IF PFENTERL = 0                                              EL663
00329          MOVE -1                 TO CARRIERL                      EL663
00330      ELSE                                                         EL663
00331          MOVE -1                 TO PFENTERL.                     EL663
00332                                                                   EL663
00333      GO TO 8200-SEND-DATAONLY.                                    EL663
00334                                                                   EL663
00335      EJECT                                                        EL663
00336  0330-EDIT-DATA.                                                  EL663
00337      PERFORM 1250-READ-COMPANY-REC THRU 1250-EXIT.                EL663
00338                                                                   EL663
00339      IF CARRIERL GREATER THAN ZEROS                               EL663
00340          MOVE AL-UANON           TO CARRIERA                      EL663
00341          MOVE CARRIERI           TO CARRIERO                      EL663
00342                                     PI-SAV-CARRIER                EL663
00343          PERFORM 1000-VERIFY-CARRIER-ID THRU 1000-EXIT            EL663
00344      ELSE                                                         EL663
00345          IF NOT ST-ACCNT-CNTL AND NOT ACCNT-CNTL                  EL663
00346              MOVE -1             TO CARRIERL                      EL663
00347              MOVE AL-UABON       TO CARRIERA                      EL663
00348              MOVE ER-0194        TO EMI-ERROR                     EL663
00349              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL663
00350                                                                   EL663
00351      IF GROUPL GREATER THAN ZEROS                                 EL663
00352          MOVE AL-UANON           TO GROUPA                        EL663
00353          MOVE GROUPI             TO GROUPO                        EL663
00354                                     PI-SAV-GROUPING               EL663
00355      ELSE                                                         EL663
00356          IF CARR-GROUP-ST-ACCNT-CNTL                              EL663
00357              MOVE -1 TO          GROUPL                           EL663
00358              MOVE AL-UABON       TO GROUPA                        EL663
00359              MOVE ER-0195        TO EMI-ERROR                     EL663
00360              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL663
00361                                                                   EL663
00362      IF STATEL GREATER THAN ZEROS                                 EL663
00363          MOVE AL-UANON           TO STATEA                        EL663
00364          MOVE STATEI             TO STATEO                        EL663
00365                                     PI-SAV-STATE                  EL663
00366          PERFORM 1100-VERIFY-STATE-ID THRU 1100-EXIT              EL663
00367      ELSE                                                         EL663
00368          IF NOT ACCNT-CNTL AND NOT CARR-ACCNT-CNTL                EL663
00369              MOVE -1             TO STATEL                        EL663
00370              MOVE AL-UABON       TO STATEA                        EL663
00371              MOVE ER-0196        TO EMI-ERROR                     EL663
00372              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL663
00373                                                                   EL663
00374      IF ACCOUNTL GREATER THAN ZEROS                               EL663
00375          MOVE AL-UANON           TO ACCOUNTA                      EL663
00376          MOVE ACCOUNTI           TO ACCOUNTO                      EL663
00377                                     PI-SAV-ACCOUNT                EL663
00378 *        PERFORM 1200-VERIFY-ACCOUNT THRU 1200-EXIT               EL663
00379      ELSE                                                         EL663
00380          MOVE -1                 TO ACCOUNTL                      EL663
00381          MOVE AL-UABON           TO ACCOUNTA                      EL663
00382          MOVE ER-0197            TO EMI-ERROR                     EL663
00383          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL663
00384                                                                   EL663
00385      IF MNTHNDTL GREATER THAN ZEROS                               EL663
00386         NEXT SENTENCE                                             EL663
00387        ELSE                                                       EL663
00388         GO TO 0340-DAY-ERROR.                                     EL663
00389                                                                   EL663
00390      MOVE MNTHNDTI               TO WS-DT-DEEDIT-FIELD.           EL663
00391      PERFORM 8600-DEEDIT THRU 8600-EXIT.                          EL663
00392                                                                   EL663
00393      MOVE WS-DT-DEEDIT-FIELD     TO DC-GREG-DATE-1-MDY.           EL663
00394      MOVE '4'          TO DC-OPTION-CODE.                         EL663
00395      MOVE LINK-ELDATCV TO PGM-NAME.                               EL663
00396      EXEC CICS LINK                                               EL663
00397           PROGRAM  (PGM-NAME)                                     EL663
00398           COMMAREA (DATE-CONVERSION-DATA)                         EL663
00399           LENGTH   (DC-COMM-LENGTH)                               EL663
00400      END-EXEC.                                                    EL663
00401                                                                   EL663
00402      IF DATE-CONVERSION-ERROR                                     EL663
00403         GO TO 0340-DAY-ERROR.                                     EL663
00404                                                                   EL663
00405      MOVE DC-GREG-DATE-1-EDIT    TO MNTHNDTO                      EL663
00406                                     WS-MNTHNDT.                   EL663
00407      MOVE DC-BIN-DATE-1          TO PI-NB-MONTH-END-DT.           EL663
00408      MOVE DC-GREG-DATE-1-MDY     TO DATE-TEST-AREA.               EL663
00409                                                                      CL**5
00410      IF DATE-TEST-DD = DC-DAYS-IN-MONTH OR 01                        CL**5
00411         GO TO 0340-ERROR-CHECK.                                      CL**8
00412                                                                   EL663
00413  0340-DAY-ERROR.                                                  EL663
00414      MOVE -1       TO MNTHNDTL.                                   EL663
00415      MOVE AL-UABON TO MNTHNDTA.                                   EL663
00416      MOVE ER-0587  TO EMI-ERROR.                                  EL663
00417      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL663
00418                                                                   EL663
00419  0340-ERROR-CHECK.                                                EL663
00420      IF NOT EMI-NO-ERRORS                                         EL663
00421          GO TO 8200-SEND-DATAONLY.                                EL663
00422                                                                   EL663
00423  0340-CHECK-TOTALS.                                               EL663
00424      IF ELFISSL GREATER THAN ZEROS                                EL663
00425          MOVE ELFISSI            TO WS-DEEDIT-FIELD               EL663
00426          PERFORM 8600-DEEDIT THRU 8600-EXIT                       EL663
00427          MOVE WS-DEEDIT-FIELD    TO PI-LF-ISS-REMITTED            EL663
00428          MOVE AL-UNNON           TO ELFISSA                       EL663
00429      ELSE                                                         EL663
00430          MOVE ZEROS              TO PI-LF-ISS-REMITTED.           EL663
00431                                                                   EL663
00432      IF ELFCANL GREATER THAN ZEROS                                EL663
00433          MOVE ELFCANI            TO WS-DEEDIT-FIELD               EL663
00434          PERFORM 8600-DEEDIT THRU 8600-EXIT                       EL663
00435          MOVE WS-DEEDIT-FIELD    TO PI-LF-CAN-REMITTED            EL663
00436          MOVE AL-UNNON           TO ELFCANA                       EL663
00437      ELSE                                                         EL663
00438          MOVE ZEROS              TO PI-LF-CAN-REMITTED.           EL663
00439                                                                   EL663
00440      IF EAHISSL GREATER THAN ZEROS                                EL663
00441          MOVE EAHISSI            TO WS-DEEDIT-FIELD               EL663
00442          PERFORM 8600-DEEDIT THRU 8600-EXIT                       EL663
00443          MOVE WS-DEEDIT-FIELD    TO PI-AH-ISS-REMITTED            EL663
00444          MOVE AL-UNNON           TO EAHISSA                       EL663
00445      ELSE                                                         EL663
00446          MOVE ZEROS              TO PI-AH-ISS-REMITTED.           EL663
00447                                                                   EL663
00448      IF EAHCANL GREATER THAN ZEROS                                EL663
00449          MOVE EAHCANI            TO WS-DEEDIT-FIELD               EL663
00450          PERFORM 8600-DEEDIT THRU 8600-EXIT                       EL663
00451          MOVE WS-DEEDIT-FIELD    TO PI-AH-CAN-REMITTED            EL663
00452          MOVE AL-UNNON           TO EAHCANA                       EL663
00453      ELSE                                                         EL663
00454          MOVE ZEROS              TO PI-AH-CAN-REMITTED.           EL663
00455                                                                   EL663
00456      IF EISSCNTL GREATER THAN ZEROS                               EL663
00457          MOVE AL-UNNON           TO EISSCNTA                      EL663
00458          EXEC CICS BIF DEEDIT                                     EL663
00459              FIELD  (EISSCNTI)                                    EL663
00460              LENGTH (6)                                           EL663
00461              END-EXEC                                             EL663
00462          MOVE EISSCNTI           TO PI-ISS-CNT-REMITTED           EL663
00463      ELSE                                                         EL663
00464          MOVE ZEROS              TO PI-ISS-CNT-REMITTED.          EL663
00465                                                                   EL663
00466      IF ECANCNTL GREATER THAN ZEROS                               EL663
00467          MOVE AL-UNNON           TO ECANCNTA                      EL663
00468          EXEC CICS BIF DEEDIT                                     EL663
00469              FIELD  (ECANCNTI)                                    EL663
00470              LENGTH (6)                                           EL663
00471              END-EXEC                                             EL663
00472          MOVE ECANCNTI           TO PI-CAN-CNT-REMITTED           EL663
00473      ELSE                                                         EL663
00474          MOVE ZEROS              TO PI-CAN-CNT-REMITTED.          EL663
00475                                                                   EL663
00476      MOVE ZEROS                  TO PI-LF-ISS-ENTERED             EL663
00477                                     PI-LF-CAN-ENTERED             EL663
00478                                     PI-AH-ISS-ENTERED             EL663
00479                                     PI-AH-CAN-ENTERED             EL663
00480                                     PI-ISS-CNT-ENTERED            EL663
00481                                     PI-CAN-CNT-ENTERED.           EL663
00482                                                                   EL663
00483      EXEC CICS HANDLE CONDITION                                   EL663
00484          NOTFND  (0350-NO-RECORDS)                                EL663
00485      END-EXEC.                                                    EL663
00486                                                                   EL663
00487      MOVE SPACES                 TO ERPNDB2-KEY.                  EL663
00488      MOVE PI-COMPANY-CD          TO PNDB-COMP-CD.                 EL663
00489      MOVE PI-SAV-ACCOUNT         TO PNDB-ACCOUNT.                 EL663
00490                                                                   EL663
00491      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL663
00492          MOVE PI-SAV-CARRIER         TO PNDB-CARRIER              EL663
00493          MOVE PI-SAV-GROUPING        TO PNDB-GROUPING             EL663
00494          MOVE PI-SAV-STATE           TO PNDB-STATE.               EL663
00495                                                                   EL663
00496      IF CARR-ST-ACCNT-CNTL                                        EL663
00497          MOVE PI-SAV-CARRIER         TO PNDB-CARRIER              EL663
00498          MOVE PI-SAV-STATE           TO PNDB-STATE.               EL663
00499                                                                   EL663
00500      IF ST-ACCNT-CNTL                                             EL663
00501          MOVE PI-SAV-STATE           TO PNDB-STATE.               EL663
00502                                                                   EL663
00503      IF CARR-ACCNT-CNTL                                           EL663
00504          MOVE PI-SAV-CARRIER         TO PNDB-CARRIER.             EL663
00505                                                                   EL663
00506      EXEC CICS STARTBR                                            EL663
00507          DATASET (ERPNDB2-FILE-ID)                                EL663
00508          RIDFLD  (ERPNDB2-KEY)                                    EL663
00509          GTEQ                                                     EL663
00510      END-EXEC.                                                    EL663
00511                                                                   EL663
00512      EXEC CICS HANDLE CONDITION                                   EL663
00513          ENDFILE (0350-EOF)                                       EL663
00514      END-EXEC.                                                    EL663
00515                                                                   EL663
00516  0345-READNEXT.                                                   EL663
00517      EXEC CICS READNEXT                                           EL663
00518          SET     (ADDRESS OF PENDING-BUSINESS)                       CL**4
00519          DATASET (ERPNDB2-FILE-ID)                                EL663
00520          RIDFLD  (ERPNDB2-KEY)                                    EL663
00521      END-EXEC.                                                    EL663
00522                                                                   EL663
00523      IF PB-COMPANY-CD-A1 NOT = PI-COMPANY-CD                      EL663
00524          GO TO 0350-EOF.                                          EL663
00525                                                                   EL663
00526      IF PB-ACCOUNT NOT = PI-SAV-ACCOUNT                           EL663
00527          GO TO 0350-EOF.                                          EL663
00528                                                                   EL663
00529      IF PI-NB-MONTH-END-DT = PB-CREDIT-SELECT-DT                  EL663
00530           NEXT SENTENCE                                           EL663
00531         ELSE                                                      EL663
00532           GO TO 0345-READNEXT.                                    EL663
00533                                                                   EL663
00534      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL663
00535      IF PI-SAV-CARRIER  = PB-CARRIER   AND                        EL663
00536         PI-SAV-GROUPING = PB-GROUPING  AND                        EL663
00537         PI-SAV-STATE    = PB-STATE                                EL663
00538           NEXT SENTENCE                                           EL663
00539         ELSE                                                      EL663
00540           GO TO 0345-READNEXT.                                    EL663
00541                                                                   EL663
00542      IF CARR-ST-ACCNT-CNTL                                        EL663
00543      IF PI-SAV-CARRIER = PB-CARRIER  AND                          EL663
00544         PI-SAV-STATE   = PB-STATE                                 EL663
00545           NEXT SENTENCE                                           EL663
00546         ELSE                                                      EL663
00547           GO TO 0345-READNEXT.                                    EL663
00548                                                                   EL663
00549      IF ST-ACCNT-CNTL                                             EL663
00550      IF PI-SAV-STATE = PB-STATE                                   EL663
00551           NEXT SENTENCE                                           EL663
00552         ELSE                                                      EL663
00553           GO TO 0345-READNEXT.                                    EL663
00554                                                                   EL663
00555      IF PB-ISSUE                                                  EL663
00556          ADD 1                   TO PI-ISS-CNT-ENTERED            EL663
00557          ADD PB-I-LF-PREMIUM-AMT PB-I-LF-ALT-PREMIUM-AMT          EL663
00558                                  TO PI-LF-ISS-ENTERED             EL663
00559          ADD PB-I-AH-PREMIUM-AMT TO PI-AH-ISS-ENTERED             EL663
00560        ELSE                                                       EL663
00561      IF PB-CANCELLATION                                           EL663
00562          ADD 1                   TO PI-CAN-CNT-ENTERED            EL663
00563          ADD PB-C-LF-CANCEL-AMT  TO PI-LF-CAN-ENTERED             EL663
00564          ADD PB-C-AH-CANCEL-AMT  TO PI-AH-CAN-ENTERED.            EL663
00565                                                                   EL663
00566      GO TO 0345-READNEXT.                                         EL663
00567                                                                   EL663
00568  0350-NO-RECORDS.                                                 EL663
00569  0350-EOF.                                                        EL663
00570      GO TO 4000-SHOW-TOTALS.                                      EL663
00571                                                                   EL663
00572      EJECT                                                        EL663
00573  1000-VERIFY-CARRIER-ID.                                          EL663
00574      MOVE SPACES                 TO ELCNTL-KEY.                   EL663
00575      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL663
00576      MOVE '6'                    TO CNTL-REC-TYPE.                EL663
00577      MOVE CARRIERI               TO CNTL-CARRIER.                 EL663
00578      MOVE +0                     TO CNTL-SEQ.                     EL663
00579                                                                   EL663
00580      EXEC CICS HANDLE CONDITION                                   EL663
00581          NOTFND   (1000-NO-CARRIER)                               EL663
00582      END-EXEC.                                                    EL663
00583                                                                   EL663
00584      EXEC CICS READ                                               EL663
00585          DATASET   (ELCNTL-FILE-ID)                               EL663
00586          SET       (ADDRESS OF CONTROL-FILE)                         CL**4
00587          RIDFLD    (ELCNTL-KEY)                                   EL663
00588      END-EXEC.                                                    EL663
00589                                                                   EL663
00590      GO TO 1000-EXIT.                                             EL663
00591                                                                   EL663
00592  1000-NO-CARRIER.                                                 EL663
00593      MOVE ER-2208                TO EMI-ERROR                     EL663
00594      MOVE -1                     TO CARRIERL                      EL663
00595      MOVE AL-UABON               TO CARRIERA                      EL663
00596      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL663
00597                                                                   EL663
00598  1000-EXIT.                                                       EL663
00599      EXIT.                                                        EL663
00600      EJECT                                                        EL663
00601  1100-VERIFY-STATE-ID.                                            EL663
00602      MOVE SPACES                 TO ELCNTL-KEY.                   EL663
00603      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL663
00604      MOVE '3'                    TO CNTL-REC-TYPE.                EL663
00605      MOVE STATEI                 TO CNTL-STATE.                   EL663
00606      MOVE +0                     TO CNTL-SEQ.                     EL663
00607                                                                   EL663
00608      EXEC CICS HANDLE CONDITION                                   EL663
00609          NOTFND   (1100-NO-STATE)                                 EL663
00610      END-EXEC.                                                    EL663
00611                                                                   EL663
00612      EXEC CICS READ                                               EL663
00613          DATASET   (ELCNTL-FILE-ID)                               EL663
00614          SET       (ADDRESS OF CONTROL-FILE)                         CL**4
00615          RIDFLD    (ELCNTL-KEY)                                   EL663
00616      END-EXEC.                                                    EL663
00617                                                                   EL663
00618      GO TO 1100-EXIT.                                             EL663
00619                                                                   EL663
00620  1100-NO-STATE.                                                   EL663
00621      MOVE ER-2209                TO EMI-ERROR.                    EL663
00622      MOVE -1                     TO STATEL.                       EL663
00623      MOVE AL-UABON               TO STATEA.                       EL663
00624      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL663
00625                                                                   EL663
00626      GO TO 8200-SEND-DATAONLY.                                    EL663
00627                                                                   EL663
00628  1100-EXIT.                                                       EL663
00629      EXIT.                                                        EL663
00630      EJECT                                                        EL663
00631                                                                   EL663
00632  1200-VERIFY-ACCOUNT.                                             EL663
00633      IF CARRIERL GREATER ZERO                                     EL663
00634          MOVE CARRIERI           TO ACCT-CARRIER                  EL663
00635      ELSE                                                         EL663
00636          MOVE SPACES             TO ACCT-CARRIER.                 EL663
00637                                                                   EL663
00638      IF GROUPL GREATER ZERO                                       EL663
00639          MOVE GROUPI             TO ACCT-GROUPING                 EL663
00640      ELSE                                                         EL663
00641          MOVE SPACES             TO ACCT-GROUPING.                EL663
00642                                                                   EL663
00643      IF STATEL GREATER ZERO                                       EL663
00644          MOVE STATEI             TO ACCT-STATE                    EL663
00645      ELSE                                                         EL663
00646          MOVE SPACES             TO ACCT-STATE.                   EL663
00647                                                                   EL663
00648      MOVE ACCOUNTI               TO ACCT-ACCOUNT.                 EL663
00649                                                                   EL663
00650  1200-READ-ACCOUNT.                                               EL663
00651      MOVE PI-COMPANY-CD          TO ACCT-CO.                      EL663
00652      MOVE LOW-VALUES             TO ACCT-EXP-DATE                 EL663
00653                                     PI-ACCT-LOW-EFF-DT            EL663
00654                                     PI-ACCT-HIGH-EXP-DT.          EL663
00655                                                                   EL663
00656      EXEC CICS HANDLE CONDITION                                   EL663
00657          NOTFND   (1200-ACCOUNT-INVALID)                          EL663
00658          ENDFILE  (1200-ACCOUNT-INVALID)                          EL663
00659      END-EXEC.                                                    EL663
00660                                                                   EL663
00661      EXEC CICS STARTBR                                            EL663
00662          DATASET (ERACCT2-FILE-ID)                                EL663
00663          RIDFLD  (ERACCT-KEY)                                     EL663
00664      END-EXEC.                                                    EL663
00665                                                                   EL663
00666      MOVE ERACCT-KEY             TO ERACCT-SAVE-KEY.              EL663
00667                                                                   EL663
00668  1200-READ-LOOP.                                                  EL663
00669      EXEC CICS READNEXT                                           EL663
00670          DATASET   (ERACCT2-FILE-ID)                              EL663
00671          SET       (ADDRESS OF ACCOUNT-MASTER)                       CL**4
00672          RIDFLD    (ERACCT-KEY)                                   EL663
00673      END-EXEC.                                                    EL663
00674                                                                   EL663
00675      IF ERACCT-COMP-KEY NOT = ERACCT-SAVE-KEY                     EL663
00676          GO TO 1200-ACCOUNT-INVALID.                              EL663
00677                                                                   EL663
00678      GO TO 1200-EXIT.                                             EL663
00679                                                                   EL663
00680  1200-ACCOUNT-INVALID.                                            EL663
00681      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL663
00682          MOVE -1                     TO CARRIERL                  EL663
00683          MOVE AL-UABON               TO CARRIERA                  EL663
00684                                         GROUPA                    EL663
00685                                         STATEA                    EL663
00686                                         ACCOUNTA                  EL663
00687      ELSE                                                         EL663
00688          IF ST-ACCNT-CNTL                                         EL663
00689              MOVE -1                 TO STATEL                    EL663
00690              MOVE AL-UABON           TO STATEA                    EL663
00691                                         ACCOUNTA                  EL663
00692          ELSE                                                     EL663
00693              IF CARR-ST-ACCNT-CNTL                                EL663
00694                  MOVE -1             TO CARRIERL                  EL663
00695                  MOVE AL-UABON       TO CARRIERA                  EL663
00696                                         STATEA                    EL663
00697                                         ACCOUNTA                  EL663
00698              ELSE                                                 EL663
00699                  IF ACCNT-CNTL                                    EL663
00700                      MOVE -1         TO ACCOUNTL                  EL663
00701                      MOVE AL-UABON   TO ACCOUNTA                  EL663
00702                  ELSE                                             EL663
00703                      MOVE -1         TO CARRIERL                  EL663
00704                      MOVE AL-UABON   TO CARRIERA                  EL663
00705                                         ACCOUNTA.                 EL663
00706      MOVE ER-2210                TO EMI-ERROR.                    EL663
00707      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL663
00708                                                                   EL663
00709  1200-EXIT.                                                       EL663
00710      EXIT.                                                        EL663
00711      EJECT                                                        EL663
00712  1250-READ-COMPANY-REC.                                           EL663
00713      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL663
00714      MOVE '1'                    TO CNTL-REC-TYPE.                EL663
00715      MOVE SPACES                 TO CNTL-ACCESS.                  EL663
00716      MOVE +0                     TO CNTL-SEQ.                     EL663
00717                                                                   EL663
00718      EXEC CICS HANDLE CONDITION                                   EL663
00719          NOTFND   (1250-NO-COMPANY-REC)                           EL663
00720      END-EXEC.                                                    EL663
00721                                                                   EL663
00722      EXEC CICS READ                                               EL663
00723          DATASET   (ELCNTL-FILE-ID)                               EL663
00724          SET       (ADDRESS OF CONTROL-FILE)                         CL**4
00725          RIDFLD    (ELCNTL-KEY)                                   EL663
00726      END-EXEC.                                                    EL663
00727                                                                   EL663
00728      MOVE CF-CREDIT-EDIT-CONTROLS TO PI-CREDIT-EDIT-CONTROLS.     EL663
00729                                                                   EL663
00730      GO TO 1250-EXIT.                                             EL663
00731                                                                   EL663
00732  1250-NO-COMPANY-REC.                                             EL663
00733      MOVE ER-2248                TO EMI-ERROR.                    EL663
00734      MOVE -1                     TO CARRIERL.                     EL663
00735      MOVE AL-UABON               TO CARRIERA                      EL663
00736      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL663
00737      GO TO 8200-SEND-DATAONLY.                                    EL663
00738                                                                   EL663
00739  1250-EXIT.                                                       EL663
00740      EXIT.                                                        EL663
00741                                                                   EL663
00742      EJECT                                                        EL663
00743  4000-SHOW-TOTALS.                                                EL663
00744      MOVE LOW-VALUES             TO EL663AI.                      EL663
00745                                                                   EL663
00746      IF PI-SAV-CARRIER NOT = SPACES                               EL663
00747          MOVE PI-SAV-CARRIER     TO CARRIERO.                     EL663
00748                                                                   EL663
00749      IF PI-SAV-GROUPING NOT = SPACES                              EL663
00750          MOVE PI-SAV-GROUPING    TO GROUPO.                       EL663
00751                                                                   EL663
00752      IF PI-SAV-STATE NOT = SPACES                                 EL663
00753          MOVE PI-SAV-STATE       TO STATEO.                       EL663
00754                                                                   EL663
00755      MOVE PI-SAV-ACCOUNT         TO ACCOUNTO.                     EL663
00756                                                                   EL663
00757      IF WS-MNTHNDT NOT = SPACES                                   EL663
00758          MOVE WS-MNTHNDT         TO MNTHNDTO.                     EL663
00759                                                                   EL663
00760      MOVE AL-UANON               TO CARRIERA                      EL663
00761                                     GROUPA                        EL663
00762                                     STATEA                        EL663
00763                                     ACCOUNTA                      EL663
00764                                     MNTHNDTA.                     EL663
00765                                                                   EL663
00766      MOVE PI-LF-ISS-ENTERED      TO ALFISSO.                      EL663
00767      IF PI-LF-ISS-REMITTED NOT = ZEROS                            EL663
00768          MOVE PI-LF-ISS-REMITTED TO ELFISSO                       EL663
00769          MOVE AL-UNNON           TO ELFISSA                       EL663
00770          COMPUTE WS-OBAL = PI-LF-ISS-REMITTED - PI-LF-ISS-ENTERED EL663
00771          IF WS-OBAL NOT = ZEROS                                   EL663
00772              MOVE WS-OBAL        TO OLFISSO.                      EL663
00773                                                                   EL663
00774      MOVE PI-LF-CAN-ENTERED      TO ALFCANO.                      EL663
00775      IF PI-LF-CAN-REMITTED NOT = ZEROS                            EL663
00776          MOVE PI-LF-CAN-REMITTED TO ELFCANO                       EL663
00777          MOVE AL-UNNON           TO ELFCANA                       EL663
00778          COMPUTE WS-OBAL = PI-LF-CAN-REMITTED - PI-LF-CAN-ENTERED EL663
00779          IF WS-OBAL NOT = ZEROS                                   EL663
00780              MOVE WS-OBAL        TO OLFCANO.                      EL663
00781                                                                   EL663
00782      MOVE PI-AH-ISS-ENTERED      TO AAHISSO.                      EL663
00783      IF PI-AH-ISS-REMITTED NOT = ZEROS                            EL663
00784          MOVE PI-AH-ISS-REMITTED TO EAHISSO                       EL663
00785          MOVE AL-UNNON           TO EAHISSA                       EL663
00786          COMPUTE WS-OBAL = PI-AH-ISS-REMITTED - PI-AH-ISS-ENTERED EL663
00787          IF WS-OBAL NOT = ZEROS                                   EL663
00788              MOVE WS-OBAL        TO OAHISSO.                      EL663
00789                                                                   EL663
00790      MOVE PI-AH-CAN-ENTERED      TO AAHCANO.                      EL663
00791      IF PI-AH-CAN-REMITTED NOT = ZEROS                            EL663
00792          MOVE PI-AH-CAN-REMITTED TO EAHCANO                       EL663
00793          MOVE AL-UNNON           TO EAHCANA                       EL663
00794          COMPUTE WS-OBAL = PI-AH-CAN-REMITTED - PI-AH-CAN-ENTERED EL663
00795          IF WS-OBAL NOT = ZEROS                                   EL663
00796              MOVE WS-OBAL        TO OAHCANO.                      EL663
00797                                                                   EL663
00798      MOVE PI-ISS-CNT-ENTERED     TO AISSCNTO.                     EL663
00799      IF PI-ISS-CNT-REMITTED NOT = ZEROS                           EL663
00800          MOVE PI-ISS-CNT-REMITTED TO EISSCNTO                     EL663
00801          MOVE AL-UNNON            TO EISSCNTA                     EL663
00802          COMPUTE WS-OCNT = PI-ISS-CNT-REMITTED -                  EL663
00803                                 PI-ISS-CNT-ENTERED                EL663
00804          IF WS-OCNT NOT = ZEROS                                   EL663
00805              MOVE WS-OCNT        TO OISSCNTO.                     EL663
00806                                                                   EL663
00807      MOVE PI-CAN-CNT-ENTERED     TO ACANCNTO.                     EL663
00808      IF PI-CAN-CNT-REMITTED NOT = ZEROS                           EL663
00809          MOVE PI-CAN-CNT-REMITTED TO ECANCNTO                     EL663
00810          MOVE AL-UNNON            TO ECANCNTA                     EL663
00811          COMPUTE WS-OCNT = PI-CAN-CNT-REMITTED -                  EL663
00812                                 PI-CAN-CNT-ENTERED                EL663
00813          IF WS-OCNT NOT = ZEROS                                   EL663
00814              MOVE WS-OCNT        TO OCANCNTO.                     EL663
00815                                                                   EL663
00816      GO TO 8100-SEND-INITIAL-MAP.                                 EL663
00817      EJECT                                                        EL663
00818                                                                   EL663
00819  8100-SEND-INITIAL-MAP.                                           EL663
00820      MOVE WS-CURRENT-DT          TO DATEO.                        EL663
00821      MOVE EIBTIME                TO TIME-IN.                      EL663
00822      MOVE TIME-OUT               TO TIMEO.                        EL663
00823      MOVE -1                     TO CARRIERL.                     EL663
00824      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL663
00825      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                     EL663
00826                                                                   EL663
00827      MOVE PI-LIFE-OVERRIDE-L2    TO WS-PRM-OVERRIDE               EL663
00828                                     WS-REFUND-OVERRIDE.           EL663
00829      MOVE WS-PRM-HEADER          TO LFPHDGO.                      EL663
00830      MOVE WS-REFUND-HEADER       TO LFRHDGO.                      EL663
00831                                                                   EL663
00832      MOVE PI-AH-OVERRIDE-L2      TO WS-PRM-OVERRIDE               EL663
00833                                     WS-REFUND-OVERRIDE.           EL663
00834      MOVE WS-PRM-HEADER          TO AHPHDGO.                      EL663
00835      MOVE WS-REFUND-HEADER       TO AHRHDGO.                      EL663
00836                                                                   EL663
00837 *    IF CARR-GROUP-ST-ACCNT-CNTL                                  EL663
00838 *        NEXT SENTENCE                                            EL663
00839 *    ELSE                                                         EL663
00840 *        IF ST-ACCNT-CNTL                                         EL663
00841 *            MOVE AL-SADOF       TO CARRHDGA                      EL663
00842 *                                   GRPHDGA                       EL663
00843 *            MOVE AL-SANOF       TO CARRIERA                      EL663
00844 *                                   GROUPA                        EL663
00845 *        ELSE                                                     EL663
00846 *            IF CARR-ST-ACCNT-CNTL                                EL663
00847 *                MOVE AL-SADOF   TO GRPHDGA                       EL663
00848 *                MOVE AL-SANOF   TO GROUPA                        EL663
00849 *            ELSE                                                 EL663
00850 *                IF ACCNT-CNTL                                    EL663
00851 *                    MOVE AL-SADOF TO CARRHDGA                    EL663
00852 *                                     GRPHDGA                     EL663
00853 *                                     STHDGA                      EL663
00854 *                    MOVE AL-SANOF TO CARRIERA                    EL663
00855 *                                     GROUPA                      EL663
00856 *                                     STATEA                      EL663
00857 *                ELSE                                             EL663
00858 *                    IF CARR-ACCNT-CNTL                           EL663
00859 *                        MOVE AL-SADOF TO GRPHDGA                 EL663
00860 *                                         STHDGA                  EL663
00861 *                        MOVE AL-SANOF TO GROUPA                  EL663
00862 *                                         STATEA.                 EL663
00863      EXEC CICS SEND                                               EL663
00864          MAP      (MAP-NAME)                                      EL663
00865          MAPSET   (MAPSET-NAME)                                   EL663
00866          FROM     (EL663AO)                                       EL663
00867          ERASE                                                    EL663
00868          CURSOR                                                   EL663
00869      END-EXEC.                                                    EL663
00870                                                                   EL663
00871      GO TO 9100-RETURN-TRAN.                                      EL663
00872      EJECT                                                        EL663
00873                                                                   EL663
00874  8200-SEND-DATAONLY.                                              EL663
00875      MOVE WS-CURRENT-DT          TO DATEO.                        EL663
00876      MOVE EIBTIME                TO TIME-IN.                      EL663
00877      MOVE TIME-OUT               TO TIMEO.                        EL663
00878      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL663
00879      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                     EL663
00880      EXEC CICS SEND                                               EL663
00881          MAP      (MAP-NAME)                                      EL663
00882          MAPSET   (MAPSET-NAME)                                   EL663
00883          FROM     (EL663AO)                                       EL663
00884          DATAONLY                                                 EL663
00885          ERASEAUP                                                 EL663
00886          CURSOR                                                   EL663
00887      END-EXEC.                                                    EL663
00888                                                                   EL663
00889      GO TO 9100-RETURN-TRAN.                                      EL663
00890                                                                   EL663
00891  8300-SEND-TEXT.                                                  EL663
00892      EXEC CICS SEND TEXT                                          EL663
00893          FROM     (LOGOFF-TEXT)                                   EL663
00894          LENGTH   (LOGOFF-LENGTH)                                 EL663
00895          ERASE                                                    EL663
00896          FREEKB                                                   EL663
00897      END-EXEC.                                                    EL663
00898                                                                   EL663
00899      EXEC CICS RETURN                                             EL663
00900      END-EXEC.                                                    EL663
00901                                                                   EL663
00902  8600-DEEDIT.                                                     EL663
00903      EXEC CICS BIF DEEDIT                                         EL663
00904          FIELD (WS-DEEDIT-FIELD)                                  EL663
00905          LENGTH(10)                                               EL663
00906      END-EXEC.                                                    EL663
00907                                                                   EL663
00908  8600-EXIT.                                                       EL663
00909      EXIT.                                                        EL663
00910                                                                   EL663
00911  EJECT                                                            EL663
00912  8800-UNAUTHORIZED-ACCESS.                                        EL663
00913      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL663
00914      GO TO 8300-SEND-TEXT.                                        EL663
00915                                                                   EL663
00916  8810-PF23.                                                       EL663
00917      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL663
00918      MOVE XCTL-005               TO PGM-NAME.                     EL663
00919      GO TO 9300-XCTL.                                             EL663
00920                                                                   EL663
00921  9000-RETURN-CICS.                                                EL663
00922      EXEC CICS RETURN                                             EL663
00923      END-EXEC.                                                    EL663
00924                                                                   EL663
00925  9100-RETURN-TRAN.                                                EL663
00926      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL663
00927      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.         EL663
00928      EXEC CICS RETURN                                             EL663
00929          TRANSID    (TRANS-ID)                                    EL663
00930          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL663
00931          LENGTH     (PI-COMM-LENGTH)                              EL663
00932      END-EXEC.                                                    EL663
00933                                                                   EL663
00934  9200-RETURN-MAIN-MENU.                                           EL663
00935      MOVE XCTL-626               TO PGM-NAME.                     EL663
00936      GO TO 9300-XCTL.                                             EL663
00937                                                                   EL663
00938  9300-XCTL.                                                       EL663
00939      EXEC CICS XCTL                                               EL663
00940          PROGRAM    (PGM-NAME)                                    EL663
00941          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL663
00942          LENGTH     (PI-COMM-LENGTH)                              EL663
00943      END-EXEC.                                                    EL663
00944                                                                   EL663
00945  9400-CLEAR.                                                      EL663
00946      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     EL663
00947      GO TO 9300-XCTL.                                             EL663
00948                                                                   EL663
00949  9500-PF12.                                                       EL663
00950      MOVE XCTL-010               TO PGM-NAME.                     EL663
00951      GO TO 9300-XCTL.                                             EL663
00952                                                                   EL663
00953  9600-PGMID-ERROR.                                                EL663
00954      EXEC CICS HANDLE CONDITION                                   EL663
00955          PGMIDERR    (8300-SEND-TEXT)                             EL663
00956      END-EXEC.                                                    EL663
00957                                                                   EL663
00958      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL663
00959      MOVE ' '                    TO PI-ENTRY-CD-1.                EL663
00960      MOVE XCTL-005               TO PGM-NAME.                     EL663
00961      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL663
00962      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL663
00963      GO TO 9300-XCTL.                                             EL663
00964                                                                   EL663
00965  9700-DATE-LINK.                                                  EL663
00966      MOVE LINK-ELDATCV           TO PGM-NAME.                     EL663
00967      EXEC CICS LINK                                               EL663
00968          PROGRAM    (PGM-NAME)                                    EL663
00969          COMMAREA   (DATE-CONVERSION-DATA)                        EL663
00970          LENGTH     (DC-COMM-LENGTH)                              EL663
00971      END-EXEC.                                                    EL663
00972                                                                   EL663
00973  9900-ERROR-FORMAT.                                               EL663
00974      IF NOT EMI-ERRORS-COMPLETE                                   EL663
00975          MOVE LINK-001           TO PGM-NAME                      EL663
00976          EXEC CICS LINK                                           EL663
00977              PROGRAM    (PGM-NAME)                                EL663
00978              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL663
00979              LENGTH     (EMI-COMM-LENGTH)                         EL663
00980          END-EXEC.                                                EL663
00981                                                                   EL663
00982  9900-EXIT.                                                       EL663
00983      EXIT.                                                        EL663
00984                                                                   EL663
00985  9990-ABEND.                                                      EL663
00986      MOVE LINK-004               TO PGM-NAME.                     EL663
00987      MOVE DFHEIBLK               TO EMI-LINE1.                    EL663
00988      EXEC CICS LINK                                               EL663
00989          PROGRAM   (PGM-NAME)                                     EL663
00990          COMMAREA  (EMI-LINE1)                                    EL663
00991          LENGTH    (72)                                           EL663
00992      END-EXEC.                                                    EL663
00993                                                                   EL663
00994      MOVE -1                     TO PFENTERL.                     EL663
00995                                                                   EL663
00996      GO TO 8200-SEND-DATAONLY.                                    EL663
00997                                                                   EL663
00998  9995-SECURITY-VIOLATION.                                         EL663
00999                              COPY ELCSCTP.                        EL663
01000                                                                   EL663
01001  9995-EXIT.                                                       EL663
01002      EXIT.                                                        EL663
01003                                                                   EL663
