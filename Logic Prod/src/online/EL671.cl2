00001  IDENTIFICATION DIVISION.                                         10/25/94
00002                                                                   EL671
00003  PROGRAM-ID.                 EL671 .                                 LV012
00004 *              PROGRAM CONVERTED BY                                  CL*12
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*12
00006 *              CONVERSION DATE 10/07/94 14:52:40.                    CL*12
00007 *                            VMOD=2.012                              CL*12
00008 *                                                                 EL671
00009 *AUTHOR.        LOGIC,INC.                                           CL*12
00010 *               DALLAS, TEXAS.                                       CL*12
00011                                                                   EL671
00012 *DATE-COMPILED.                                                      CL*12
00013                                                                   EL671
00014 *SECURITY.   *****************************************************   CL*12
00015 *            *                                                   *   CL*12
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*12
00017 *            *                                                   *   CL*12
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*12
00019 *                                                                *   CL*12
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*12
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*12
00022 *            *                                                   *   CL*12
00023 *            *****************************************************   CL*12
00024                                                                   EL671
00025 *REMARKS.                                                            CL*12
00026 *        TRANSACTION - EXE6 - ONLINE REPORT MENU.                    CL*12
00027      EJECT                                                        EL671
00028  ENVIRONMENT DIVISION.                                            EL671
00029  DATA DIVISION.                                                   EL671
00030  WORKING-STORAGE SECTION.                                         EL671
00031  77  FILLER  PIC  X(32) VALUE '********************************'. EL671
00032  77  FILLER  PIC  X(32) VALUE '*    EL671 WORKING STORAGE     *'. EL671
00033  77  FILLER  PIC  X(32) VALUE '*********** VMOD=2.012 *********'.    CL*12
00034                                                                   EL671
00035                              COPY ELCSCTM.                           CL**3
00036                                                                   EL671
00037                              COPY ELCSCRTY.                          CL**3
00038                                                                   EL671
00039  01  MISC-WORK-AREAS.                                             EL671
00040      12  ELCNTL-KEY.                                                 CL**6
00041          16  CK-COMP-ID      PIC X(3).                               CL**6
00042          16  CK-REC-TYPE     PIC X       VALUE '1'.                  CL**6
00043          16  FILLER          PIC X(4)    VALUE SPACES.               CL**6
00044          16  CK-SEQ-NO       PIC S9(4)   VALUE +0    COMP.           CL**6
00045      12  SC-ITEM             PIC S9(4)       VALUE +0001  COMP.   EL671
00046      12  MAP-NAME            PIC  X(8)       VALUE 'EL671A'.      EL671
00047      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL671S'.      EL671
00048      12  TRANS-ID            PIC  X(4)       VALUE 'EXE6'.        EL671
00049      12  THIS-PGM            PIC  X(8)       VALUE 'EL671'.       EL671
00050      12  PGM-NAME            PIC  X(8)       VALUE SPACES.           CL*12
00051      12  TIME-IN             PIC S9(7).                           EL671
00052      12  TIME-OUT-R  REDEFINES  TIME-IN.                          EL671
00053          16  FILLER          PIC  X.                              EL671
00054          16  TIME-OUT        PIC  99V99.                          EL671
00055          16  FILLER          PIC  XX.                             EL671
00056      12  TRANS-673           PIC  X(4)       VALUE 'EXE7'.        EL671
00057      12  TRANS-674           PIC  X(4)       VALUE 'EXE8'.        EL671
00058      12  TRANS-675           PIC  X(4)       VALUE 'EXE9'.        EL671
00059      12  TRANS-676           PIC  X(4)       VALUE 'EXF1'.        EL671
00060      12  TRANS-682           PIC  X(4)       VALUE 'EXF8'.        EL671
00061      12  TRANS-683           PIC  X(4)       VALUE 'EXF9'.        EL671
00062      12  TRANS-684           PIC  X(4)       VALUE 'EXH2'.        EL671
00063      12  XCTL-005            PIC  X(5)       VALUE 'EL005'.       EL671
00064      12  XCTL-010            PIC  X(5)       VALUE 'EL010'.       EL671
00065      12  XCTL-179            PIC  X(5)       VALUE 'EL179'.       EL671
00066      12  XCTL-641            PIC  X(5)       VALUE 'EL641'.       EL671
00067      12  XCTL-673            PIC  X(5)       VALUE 'EL673'.       EL671
00068      12  XCTL-674            PIC  X(5)       VALUE 'EL674'.       EL671
00069      12  XCTL-675            PIC  X(5)       VALUE 'EL675'.       EL671
00070      12  XCTL-676            PIC  X(5)       VALUE 'EL676'.       EL671
00071      12  XCTL-677            PIC  X(5)       VALUE 'EL677'.       EL671
00072      12  XCTL-678            PIC  X(5)       VALUE 'EL678'.       EL671
00073      12  XCTL-679            PIC  X(5)       VALUE 'EL679'.       EL671
00074      12  XCTL-682            PIC  X(5)       VALUE 'EL682'.       EL671
00075      12  XCTL-683            PIC  X(5)       VALUE 'EL683'.       EL671
00076      12  XCTL-684            PIC  X(5)       VALUE 'EL684'.       EL671
00077      12  XCTL-685            PIC  X(5)       VALUE 'EL685'.       EL671
00078      12  XCTL-686            PIC  X(5)       VALUE 'EL686'.       EL671
00079      12  XCTL-687            PIC  X(5)       VALUE 'EL687'.       EL671
00080      12  XCTL-626            PIC  X(5)       VALUE 'EL626'.       EL671
00081      12  XCTL-690            PIC  X(8)       VALUE 'EL690'.       EL671
00082      12  XCTL-692            PIC  X(8)       VALUE 'EL692'.          CL**3
00083      12  XCTL-694            PIC  X(5)       VALUE 'EL694'.          CL**7
CIDMOD     12  XCTL-695            PIC  X(5)       VALUE 'EL695'.          CL**7
00084      12  ER-0004             PIC  X(4)       VALUE '0004'.        EL671
00085      12  ER-0007             PIC  X(4)       VALUE '0007'.           CL**6
00086      12  ER-0008             PIC  X(4)       VALUE '0008'.        EL671
00087      12  ER-0029             PIC  X(4)       VALUE '0029'.        EL671
00088      12  ER-0070             PIC  X(4)       VALUE '0070'.        EL671
00089      12  ER-0166             PIC  X(4)       VALUE '0166'.           CL*11
00090      12  ER-0587             PIC  X(4)       VALUE '0587'.        EL671
00091      12  ER-0636             PIC  X(4)       VALUE '0636'.        EL671
00092      12  ER-2506             PIC  X(4)       VALUE '2506'.        EL671
00093      12  ER-2507             PIC  X(4)       VALUE '2507'.        EL671
00094      12  ER-2508             PIC  X(4)       VALUE '2508'.        EL671
00095      12  ER-2509             PIC  X(4)       VALUE '2509'.        EL671
00096      12  ER-2510             PIC  X(4)       VALUE '2510'.        EL671
00097      12  ER-2515             PIC  X(4)       VALUE '2515'.        EL671
00098      12  ER-2544             PIC  X(4)       VALUE '2544'.        EL671
00099      12  LINK-001            PIC  X(5)       VALUE 'EL001'.       EL671
00100      12  LINK-004            PIC  X(5)       VALUE 'EL004'.       EL671
00101      12  LINK-ELDATCV        PIC  X(8)       VALUE 'ELDATCV '.    EL671
00102      12  WS-CURRENT-DT       PIC  X(8)       VALUE SPACES.        EL671
00103      12  WS-EL676-DATE.                                           EL671
00104          16  WS-EL676-MO     PIC  99.                             EL671
00105          16  WS-EL676-DA     PIC  99.                             EL671
00106          16  WS-EL676-YR     PIC  99.                             EL671
00107      12  WS-EL676-TEST-DAY   PIC S999    COMP-3.                  EL671
00108      12  CHECK-REC-TYPE      PIC X           VALUE SPACE.         EL671
00109          88  VALID-REC-TYPE                  VALUE  'R' 'D' 'C'      CL**4
00110                                                     'S' 'T' 'U'      CL**4
00111                                                     'X' 'Y' 'Z'      CL**4
00112                                                     'F' 'G'.         CL**4
00113                                                                   EL671
00114  01  FILLER.                                                      EL671
00115      12  WS-TERMINAL-TYPE-AND-MODEL.                              EL671
00116          16  WS-TERMINAL-TYPE    PIC  X.                          EL671
00117          16  WS-TERMINAL-MODEL   PIC  X.                          EL671
00118      12  WS-HEX-919299           PIC S9(8)  VALUE +9540249 COMP.  EL671
00119      12  FILLER  REDEFINES  WS-HEX-919299.                        EL671
00120          16  FILLER              PIC  X.                          EL671
00121          16  WS-3277-REMOTE      PIC  X.                          EL671
00122          16  WS-3275-REMOTE      PIC  X.                          EL671
00123          16  WS-3277-LOCAL       PIC  X.                          EL671
00124      EJECT                                                        EL671
00125                              COPY ELCDATE.                           CL**3
00126      EJECT                                                        EL671
00127                              COPY ELCLOGOF.                          CL**3
00128      EJECT                                                        EL671
00129                              COPY ELCATTR.                           CL**3
00130      EJECT                                                        EL671
00131                              COPY ELCEMIB.                           CL**3
00132      EJECT                                                        EL671
00133                              COPY ELCINTF.                           CL**3
00134      12 FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.                  EL671
00135          16  PI-REPORT-RANGE-DATES.                               EL671
00136              20  PI-FROM-DT          PIC  XX.                     EL671
00137              20  PI-THRU-DT          PIC  XX.                     EL671
00138              20  PI-EL676-STOP-DT    PIC  XX.                     EL671
00139          16  PI-PYAJTYP              PIC  X.                      EL671
00140          16  PI-EL676-ENTER-DT       PIC  XX.                        CL**5
00141          16  PI-PYAJCAR              PIC  X.                         CL**8
00142          16  PI-PYAJDTT              PIC  X.                         CL*11
00143 *        16  FILLER                  PIC  X(21).                     CL*12
00144          16  PI-PYAJBY               PIC  X(4).                      CL*12
00145          16  FILLER                  PIC  X(625).                    CL*12
00146      EJECT                                                        EL671
00147                              COPY ELCAID.                            CL**3
00148  01  FILLER  REDEFINES  DFHAID.                                   EL671
00149      12  FILLER              PIC  X(8).                           EL671
00150      12  PF-VALUES           PIC  X      OCCURS 27.               EL671
00151      EJECT                                                        EL671
00152                              COPY EL671S.                            CL**3
00153      EJECT                                                        EL671
00154  LINKAGE SECTION.                                                 EL671
00155  01  DFHCOMMAREA             PIC  X(1024).                        EL671
00156                                                                      CL**6
00157 *01 PARMLIST .                                                       CL*12
00158 *    02  FILLER              PIC S9(8)   COMP.                       CL*12
00159 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                       CL*12
00160      EJECT                                                           CL**6
00161      COPY ELCCNTL.                                                   CL**6
00162      EJECT                                                        EL671
00163  PROCEDURE DIVISION.                                              EL671
00164      CONTINUE.                                                       CL*12
00165                                                                      CL**6
00166      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL671
00167                                                                   EL671
00168      IF EIBCALEN = 0                                              EL671
00169          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL671
00170                                                                   EL671
00171      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL671
00172      MOVE '5'                    TO  DC-OPTION-CODE.              EL671
00173                                                                   EL671
00174      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.                  EL671
00175                                                                   EL671
00176      MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DT.               EL671
00177                                                                   EL671
00178      EXEC CICS  ASSIGN                                            EL671
00179          TERMCODE  (WS-TERMINAL-TYPE-AND-MODEL)                   EL671
00180      END-EXEC.                                                    EL671
00181                                                                   EL671
00182      IF PI-CALLING-PROGRAM  NOT = THIS-PGM                        EL671
00183          IF PI-RETURN-TO-PROGRAM  NOT = THIS-PGM                  EL671
00184              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6    EL671
00185              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5    EL671
00186              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4    EL671
00187              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3    EL671
00188              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2    EL671
00189              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1    EL671
00190              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM  EL671
00191              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM    EL671
00192          ELSE                                                     EL671
00193              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM    EL671
00194              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM  EL671
00195              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1    EL671
00196              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2    EL671
00197              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3    EL671
00198              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4    EL671
00199              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5    EL671
00200              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.   EL671
00201                                                                   EL671
00202      IF EIBTRNID  NOT = TRANS-ID                                  EL671
00203          GO TO 8100-SEND-INITIAL-MAP.                             EL671
00204                                                                   EL671
00205      EXEC CICS  HANDLE CONDITION                                  EL671
00206          PGMIDERR  (9600-PGMID-ERROR)                             EL671
00207          ERROR     (9990-ABEND)                                   EL671
00208      END-EXEC.                                                    EL671
00209                                                                   EL671
00210      IF EIBAID  = DFHCLEAR                                        EL671
00211          GO TO 9400-CLEAR.                                        EL671
00212                                                                   EL671
00213      IF PI-PROCESSOR-ID = 'LGXX'                                  EL671
00214          GO TO 0200-RECEIVE.                                      EL671
00215                                                                   EL671
00216      EXEC CICS READQ TS                                           EL671
00217          QUEUE   (PI-SECURITY-TEMP-STORE-ID)                      EL671
00218          INTO    (SECURITY-CONTROL)                               EL671
00219          LENGTH  (SC-COMM-LENGTH)                                 EL671
00220          ITEM    (SC-ITEM)                                        EL671
00221      END-EXEC.                                                    EL671
00222                                                                   EL671
00223      EJECT                                                        EL671
00224  0200-RECEIVE.                                                    EL671
00225      MOVE LOW-VALUES             TO  EL671AI.                     EL671
00226                                                                   EL671
00227      IF EIBAID  = DFHPA1  OR  DFHPA2  OR  DFHPA3                  EL671
00228          MOVE ER-0008            TO  EMI-ERROR                    EL671
00229          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL671
00230          MOVE -1                 TO  SELCODEL                     EL671
00231          GO TO 8200-SEND-DATAONLY.                                EL671
00232                                                                   EL671
00233      EXEC CICS  RECEIVE                                           EL671
00234          MAP     (MAP-NAME)                                       EL671
00235          MAPSET  (MAPSET-NAME)                                    EL671
00236          INTO    (EL671AI)                                        EL671
00237      END-EXEC.                                                    EL671
00238                                                                   EL671
00239      IF SELCODEL  = 0                                             EL671
00240          GO TO 0300-CHECK-PFKEYS.                                 EL671
00241                                                                   EL671
00242      IF EIBAID  NOT = DFHENTER                                    EL671
00243          MOVE ER-0004            TO  EMI-ERROR                    EL671
00244          GO TO 0320-INPUT-ERROR.                                  EL671
00245                                                                   EL671
00246      IF (SELCODEI IS NUMERIC) AND                                    CL**2
00247          (SELCODEI IS GREATER THAN 0 AND LESS THAN 25)               CL**2
00248              MOVE PF-VALUES (SELCODEI)  TO  EIBAID                   CL**2
00249      ELSE                                                         EL671
00250              MOVE ER-0029               TO  EMI-ERROR                CL**2
00251              GO TO 0320-INPUT-ERROR.                                 CL**2
00252                                                                   EL671
00253  0300-CHECK-PFKEYS.                                               EL671
00254      MOVE ' '                    TO  PI-ENTRY-CD-1.               EL671
00255                                                                   EL671
00256      IF EIBAID  = DFHPF23                                         EL671
00257          GO TO 8810-PF23.                                         EL671
00258                                                                   EL671
00259      IF EIBAID  = DFHPF24                                         EL671
00260          GO TO 9200-RETURN-MAIN-MENU.                             EL671
00261                                                                   EL671
00262      IF EIBAID  = DFHPF12                                         EL671
00263          GO TO 9500-PF12.                                         EL671
00264                                                                   EL671
00265      IF DATEL  GREATER +0                                         EL671
00266         IF DATEI NUMERIC                                          EL671
00267          MOVE DATEI              TO  DC-GREG-DATE-1-MDY           EL671
00268                                      WS-EL676-DATE                EL671
00269          MOVE WS-EL676-DA        TO  WS-EL676-TEST-DAY            EL671
00270          MOVE '4'                TO  DC-OPTION-CODE               EL671
00271          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT               EL671
00272          IF NO-CONVERSION-ERROR                                      CL**5
00273             NEXT SENTENCE                                            CL**5
00274          ELSE                                                     EL671
00275              MOVE ER-0587        TO  EMI-ERROR                    EL671
00276              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL671
00277              MOVE -1             TO  DATEL                        EL671
00278              MOVE AL-UABON       TO  DATEA                        EL671
00279              GO TO 8200-SEND-DATAONLY                             EL671
00280          ELSE                                                     EL671
00281              MOVE ER-0587        TO  EMI-ERROR                    EL671
00282              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL671
00283              MOVE -1             TO  DATEL                        EL671
00284              MOVE AL-UABON       TO  DATEA                        EL671
00285              GO TO 8200-SEND-DATAONLY                             EL671
00286      ELSE                                                         EL671
00287          MOVE LOW-VALUES         TO  PI-EL676-STOP-DT                CL**5
00288          MOVE LOW-VALUES         TO  PI-EL676-ENTER-DT               CL**5
00289          GO TO 0305-CONT.                                            CL**5
00290                                                                   EL671
00291      IF DC-DAYS-IN-MONTH  = WS-EL676-TEST-DAY                        CL**5
00292         MOVE DC-BIN-DATE-1       TO  PI-EL676-STOP-DT                CL**5
00293         MOVE LOW-VALUES          TO  PI-EL676-ENTER-DT               CL**5
00294      ELSE                                                            CL**5
00295         MOVE DC-BIN-DATE-1       TO  PI-EL676-ENTER-DT               CL**5
00296         MOVE LOW-VALUES          TO  PI-EL676-STOP-DT.               CL**5
00297                                                                      CL**5
00298  0305-CONT.                                                          CL**5
00299      IF FROMDTEL  NOT = ZEROS                                     EL671
00300          MOVE FROMDTEI           TO  DC-GREG-DATE-1-MDY           EL671
00301          MOVE '4'                TO  DC-OPTION-CODE               EL671
00302          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT               EL671
00303          IF NO-CONVERSION-ERROR                                   EL671
00304              MOVE DC-BIN-DATE-1  TO  PI-FROM-DT                   EL671
00305          ELSE                                                     EL671
00306              MOVE ER-0636        TO  EMI-ERROR                    EL671
00307              MOVE -1             TO  FROMDTEL                     EL671
00308              MOVE AL-UABON       TO  FROMDTEA                     EL671
00309              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL671
00310      ELSE                                                         EL671
00311          MOVE LOW-VALUES         TO  PI-FROM-DT.                  EL671
00312                                                                   EL671
00313      IF THRUDTEL  NOT = ZEROS                                     EL671
00314          MOVE THRUDTEI           TO  DC-GREG-DATE-1-MDY           EL671
00315          MOVE '4'                TO  DC-OPTION-CODE               EL671
00316          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT               EL671
00317          IF NO-CONVERSION-ERROR                                   EL671
00318              MOVE DC-BIN-DATE-1  TO  PI-THRU-DT                   EL671
00319          ELSE                                                     EL671
00320              MOVE ER-0636        TO  EMI-ERROR                    EL671
00321              MOVE -1             TO  THRUDTEL                     EL671
00322              MOVE AL-UABON       TO  THRUDTEA                     EL671
00323              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL671
00324      ELSE                                                         EL671
00325          MOVE HIGH-VALUES        TO  PI-THRU-DT.                  EL671
00326                                                                   EL671
00327      IF PYAJTYPL NOT = ZEROS                                      EL671
00328          MOVE PYAJTYPI           TO CHECK-REC-TYPE                EL671
00329          IF NOT VALID-REC-TYPE                                    EL671
00330              MOVE -1             TO PYAJTYPL                      EL671
00331              MOVE 2234           TO EMI-ERROR                     EL671
00332              MOVE AL-UABON       TO PYAJTYPA                      EL671
00333              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL671
00334          ELSE                                                     EL671
00335              MOVE PYAJTYPI       TO PI-PYAJTYP                    EL671
00336      ELSE                                                         EL671
00337          MOVE SPACES             TO PI-PYAJTYP.                   EL671
00338                                                                      CL**8
00339      IF PYAJCARL NOT = ZEROS                                         CL**8
00340          MOVE PYAJCARI           TO PI-PYAJCAR                       CL**8
00341      ELSE                                                            CL**8
00342          MOVE SPACES             TO PI-PYAJCAR.                      CL**8
00343                                                                      CL*12
00344      IF PYAJBYL  NOT = ZEROS                                         CL*12
00345          MOVE PYAJBYI            TO PI-PYAJBY                        CL*12
00346      ELSE                                                            CL*12
00347          MOVE SPACES             TO PI-PYAJBY.                       CL*12
00348                                                                   EL671
00349      IF EIBAID  = DFHPF2 OR DFHPF3 OR DFHPF5 OR DFHPF6            EL671
00350         IF FROMDTEL NOT GREATER THAN ZERO                         EL671
00351            MOVE ER-2544               TO EMI-ERROR                EL671
00352            MOVE -1                    TO  FROMDTEL                EL671
00353            MOVE AL-UABON              TO  FROMDTEA                EL671
00354            PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.            EL671
00355                                                                      CL*11
00356      IF PYAJDTTL  >  0                                               CL*11
00357          IF PYAJDTTI  =  'M'  OR 'I'                                 CL*11
00358              MOVE PYAJDTTI            TO PI-PYAJDTT                  CL*11
00359          ELSE                                                        CL*11
00360              MOVE ER-0166             TO EMI-ERROR                   CL*11
00361              MOVE -1                  TO  PYAJDTTL                   CL*11
00362              MOVE AL-UABON            TO  PYAJDTTA                   CL*11
00363              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT              CL*11
00364      ELSE                                                            CL*11
00365          MOVE 'M'                     TO PI-PYAJDTT.                 CL*11
00366                                                                   EL671
00367      IF EMI-ERROR  NOT = ZEROS                                    EL671
00368          GO TO 8200-SEND-DATAONLY.                                EL671
00369                                                                   EL671
00370      IF EIBAID  = DFHPF1                                          EL671
00371          MOVE XCTL-179           TO  PGM-NAME                     EL671
00372          GO TO 9300-XCTL.                                         EL671
00373                                                                   EL671
00374      IF EIBAID = DFHPF2 AND PI-PROCESSOR-ID NOT = 'LGXX'          EL671
00375          MOVE SC-CREDIT-DISPLAY (04)  TO  PI-DISPLAY-CAP          EL671
00376          MOVE SC-CREDIT-UPDATE  (04)  TO  PI-MODIFY-CAP           EL671
00377          IF NOT MODIFY-CAP                                        EL671
00378              MOVE XCTL-673            TO  THIS-PGM                EL671
00379              MOVE 'UPDATE'            TO  SM-READ                 EL671
00380              PERFORM 9995-SECURITY-VIOLATION                      EL671
00381              MOVE ER-0070             TO  EMI-ERROR               EL671
00382              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL671
00383              GO TO 8100-SEND-INITIAL-MAP.                         EL671
00384                                                                   EL671
00385      IF EIBAID = DFHPF2                                           EL671
00386          EXEC CICS  START                                         EL671
00387              TRANSID  (TRANS-673)                                 EL671
00388              FROM     (PROGRAM-INTERFACE-BLOCK)                   EL671
00389              LENGTH   (PI-COMM-LENGTH)                            EL671
00390          END-EXEC                                                 EL671
00391              MOVE ER-2506            TO  EMI-ERROR                EL671
00392              GO TO 0320-INPUT-ERROR.                              EL671
00393                                                                   EL671
00394      IF EIBAID = DFHPF3 AND PI-PROCESSOR-ID NOT = 'LGXX'          EL671
00395          MOVE SC-CREDIT-DISPLAY (05)  TO  PI-DISPLAY-CAP          EL671
00396          MOVE SC-CREDIT-UPDATE  (05)  TO  PI-MODIFY-CAP           EL671
00397          IF NOT MODIFY-CAP                                        EL671
00398              MOVE XCTL-675            TO  THIS-PGM                EL671
00399              MOVE 'UPDATE'            TO  SM-READ                 EL671
00400              PERFORM 9995-SECURITY-VIOLATION                      EL671
00401              MOVE ER-0070             TO  EMI-ERROR               EL671
00402              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL671
00403              GO TO 8100-SEND-INITIAL-MAP.                         EL671
00404                                                                   EL671
00405      IF EIBAID  = DFHPF3                                          EL671
00406          EXEC CICS  START                                         EL671
00407              TRANSID  (TRANS-675)                                 EL671
00408              FROM     (PROGRAM-INTERFACE-BLOCK)                   EL671
00409              LENGTH   (PI-COMM-LENGTH)                            EL671
00410          END-EXEC                                                 EL671
00411              MOVE ER-2507            TO  EMI-ERROR                EL671
00412              GO TO 0320-INPUT-ERROR.                              EL671
00413                                                                   EL671
00414      IF EIBAID = DFHPF4 AND PI-PROCESSOR-ID NOT = 'LGXX'          EL671
00415          MOVE SC-CREDIT-DISPLAY (15)  TO  PI-DISPLAY-CAP          EL671
00416          MOVE SC-CREDIT-UPDATE  (15)  TO  PI-MODIFY-CAP           EL671
00417          IF NOT MODIFY-CAP                                        EL671
00418              MOVE XCTL-682            TO  THIS-PGM                EL671
00419              MOVE 'UPDATE'            TO  SM-READ                 EL671
00420              PERFORM 9995-SECURITY-VIOLATION                      EL671
00421              MOVE ER-0070             TO  EMI-ERROR               EL671
00422              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL671
00423              GO TO 8100-SEND-INITIAL-MAP.                         EL671
00424                                                                   EL671
00425      IF EIBAID  = DFHPF4                                          EL671
00426          EXEC CICS  START                                         EL671
00427              TRANSID  (TRANS-682)                                 EL671
00428              FROM     (PROGRAM-INTERFACE-BLOCK)                   EL671
00429              LENGTH   (PI-COMM-LENGTH)                            EL671
00430          END-EXEC                                                 EL671
00431          MOVE ER-2508            TO  EMI-ERROR                    EL671
00432          GO TO 0320-INPUT-ERROR.                                  EL671
00433                                                                   EL671
00434      IF EIBAID = DFHPF5 AND PI-PROCESSOR-ID NOT = 'LGXX'          EL671
00435          MOVE SC-CREDIT-DISPLAY (07)  TO  PI-DISPLAY-CAP          EL671
00436          MOVE SC-CREDIT-UPDATE  (07)  TO  PI-MODIFY-CAP           EL671
00437          IF NOT MODIFY-CAP                                        EL671
00438              MOVE XCTL-674            TO  THIS-PGM                EL671
00439              MOVE 'UPDATE'            TO  SM-READ                 EL671
00440              PERFORM 9995-SECURITY-VIOLATION                      EL671
00441              MOVE ER-0070             TO  EMI-ERROR               EL671
00442              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL671
00443              GO TO 8100-SEND-INITIAL-MAP.                         EL671
00444                                                                   EL671
00445      IF EIBAID  =  DFHPF5                                         EL671
00446          EXEC CICS  START                                         EL671
00447              TRANSID  (TRANS-674)                                 EL671
00448              FROM     (PROGRAM-INTERFACE-BLOCK)                   EL671
00449              LENGTH   (PI-COMM-LENGTH)                            EL671
00450          END-EXEC                                                 EL671
00451          MOVE ER-2509            TO  EMI-ERROR                    EL671
00452          GO TO 0320-INPUT-ERROR.                                  EL671
00453                                                                   EL671
00454      IF EIBAID = DFHPF6 AND PI-PROCESSOR-ID NOT = 'LGXX'          EL671
00455          MOVE SC-CREDIT-DISPLAY (08)  TO  PI-DISPLAY-CAP          EL671
00456          MOVE SC-CREDIT-UPDATE  (08)  TO  PI-MODIFY-CAP           EL671
00457          IF NOT MODIFY-CAP                                        EL671
00458              MOVE XCTL-683            TO  THIS-PGM                EL671
00459              MOVE 'UPDATE'            TO  SM-READ                 EL671
00460              PERFORM 9995-SECURITY-VIOLATION                      EL671
00461              MOVE ER-0070             TO  EMI-ERROR               EL671
00462              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL671
00463              GO TO 8100-SEND-INITIAL-MAP.                         EL671
00464                                                                   EL671
00465      IF EIBAID  = DFHPF6                                          EL671
00466          EXEC CICS  START                                         EL671
00467              TRANSID  (TRANS-683)                                 EL671
00468              FROM     (PROGRAM-INTERFACE-BLOCK)                   EL671
00469              LENGTH   (PI-COMM-LENGTH)                            EL671
00470          END-EXEC                                                 EL671
00471          MOVE ER-2510            TO  EMI-ERROR                    EL671
00472          GO TO 0320-INPUT-ERROR.                                  EL671
00473                                                                   EL671
090612*     IF PI-COMPANY-ID = 'XXX'
CIDMOD         IF EIBAID = DFHPF7 AND PI-PROCESSOR-ID NOT = 'LGXX'           000
CIDMOD             MOVE 'OPTION NOT AVAILABLE AT THIS TIME'                  000
CIDMOD                                   TO  EMI-MESSAGE-AREA (1)            000
CIDMOD             MOVE -1               TO  SELCODEL                        000
CIDMOD             GO TO 8200-SEND-DATAONLY                                  000
090612        END-IF.
090612*    ELSE
090612*        IF EIBAID = DFHPF7                                          CL**7
090612*            MOVE XCTL-694           TO  PGM-NAME                    CL**7
090612*            GO TO 9300-XCTL                                         CL**7
090612*        END-IF
090612*    END-IF.
00477                                                                   EL671
00478      IF EIBAID  =  DFHPF8                                         EL671
00479          EXEC CICS  START                                         EL671
00480              TRANSID  (TRANS-676)                                 EL671
00481              FROM     (PROGRAM-INTERFACE-BLOCK)                   EL671
00482              LENGTH   (PI-COMM-LENGTH)                            EL671
00483          END-EXEC                                                 EL671
00484          MOVE 'NET PREMIUM TOTALS REPORT STARTED '                   CL**2
00485                                  TO  EMI-MESSAGE-AREA (1)         EL671
00486          MOVE -1                 TO  SELCODEL                     EL671
00487          PERFORM 8200-SEND-DATAONLY.                              EL671
00488                                                                   EL671
090612*    IF EIBAID  = DFHPF9                                          EL671
090612*       MOVE XCTL-695            TO  PGM-NAME                     EL671
090612*       GO TO 9300-XCTL                                           EL671
090612*    END-IF
00598                                                                   EL671
00489      IF EIBAID  = DFHPF9                                          EL671
00490          MOVE 'OPTION NOT AVAILABLE AT THIS TIME'                 EL671
00491                                  TO  EMI-MESSAGE-AREA (1)         EL671
00492          MOVE -1                 TO  SELCODEL                     EL671
00493          GO TO 8200-SEND-DATAONLY.                                EL671
00494                                                                   EL671
00495      IF EIBAID = DFHPF10 AND PI-PROCESSOR-ID NOT = 'LGXX'         EL671
00496          MOVE SC-CREDIT-DISPLAY (21)  TO  PI-DISPLAY-CAP          EL671
00497          MOVE SC-CREDIT-UPDATE  (21)  TO  PI-MODIFY-CAP           EL671
00498          IF NOT MODIFY-CAP                                        EL671
00499              MOVE XCTL-679            TO  THIS-PGM                EL671
00500              MOVE 'UPDATE'            TO  SM-READ                 EL671
00501              PERFORM 9995-SECURITY-VIOLATION                      EL671
00502              MOVE ER-0070             TO  EMI-ERROR               EL671
00503              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL671
00504              GO TO 8100-SEND-INITIAL-MAP.                         EL671
00505                                                                   EL671
00506      IF EIBAID  = DFHPF10                                         EL671
00507          MOVE XCTL-679           TO  PGM-NAME                     EL671
00508          GO TO 9300-XCTL.                                         EL671
00509                                                                   EL671
00510      IF EIBAID = DFHPF11 AND PI-PROCESSOR-ID NOT = 'LGXX'         EL671
00511          MOVE SC-CREDIT-DISPLAY (22)  TO  PI-DISPLAY-CAP          EL671
00512          MOVE SC-CREDIT-UPDATE  (22)  TO  PI-MODIFY-CAP           EL671
00513          IF NOT MODIFY-CAP                                        EL671
00514              MOVE XCTL-685            TO  THIS-PGM                EL671
00515              MOVE 'UPDATE'            TO  SM-READ                 EL671
00516              PERFORM 9995-SECURITY-VIOLATION                      EL671
00517              MOVE ER-0070             TO  EMI-ERROR               EL671
00518              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL671
00519              GO TO 8100-SEND-INITIAL-MAP.                         EL671
00520                                                                   EL671
00521      IF EIBAID  = DFHPF11                                         EL671
00522          MOVE XCTL-685           TO  PGM-NAME                     EL671
00523          GO TO 9300-XCTL.                                         EL671
00524                                                                   EL671
00525      IF EIBAID = DFHPF13 AND PI-PROCESSOR-ID NOT = 'LGXX'         EL671
00526          MOVE SC-CREDIT-DISPLAY (23)  TO  PI-DISPLAY-CAP          EL671
00527          MOVE SC-CREDIT-UPDATE  (23)  TO  PI-MODIFY-CAP           EL671
00528          IF NOT MODIFY-CAP                                        EL671
00529              MOVE XCTL-686            TO  THIS-PGM                EL671
00530              MOVE 'UPDATE'            TO  SM-READ                 EL671
00531              PERFORM 9995-SECURITY-VIOLATION                      EL671
00532              MOVE ER-0070             TO  EMI-ERROR               EL671
00533              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL671
00534              GO TO 8100-SEND-INITIAL-MAP.                         EL671
00535                                                                   EL671
00536      IF EIBAID  = DFHPF13                                         EL671
00537          MOVE XCTL-686           TO  PGM-NAME                     EL671
00538          GO TO 9300-XCTL.                                         EL671
00539                                                                   EL671
00540      IF EIBAID = DFHPF14 AND PI-PROCESSOR-ID NOT = 'LGXX'         EL671
00541          MOVE SC-CREDIT-DISPLAY (24)  TO  PI-DISPLAY-CAP          EL671
00542          MOVE SC-CREDIT-UPDATE  (24)  TO  PI-MODIFY-CAP           EL671
00543          IF NOT MODIFY-CAP                                        EL671
00544              MOVE XCTL-687            TO  THIS-PGM                EL671
00545              MOVE 'UPDATE'            TO  SM-READ                 EL671
00546              PERFORM 9995-SECURITY-VIOLATION                      EL671
00547              MOVE ER-0070             TO  EMI-ERROR               EL671
00548              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL671
00549              GO TO 8100-SEND-INITIAL-MAP.                         EL671
00550                                                                   EL671
00551      IF EIBAID  = DFHPF14                                         EL671
00552          MOVE XCTL-687           TO  PGM-NAME                     EL671
00553          GO TO 9300-XCTL.                                         EL671
00554                                                                   EL671
00555      IF EIBAID  = DFHPF15                                         EL671
00556          MOVE XCTL-641           TO  PGM-NAME                     EL671
00557          GO TO 9300-XCTL.                                         EL671
00558                                                                   EL671
00559      IF EIBAID = DFHPF16 AND PI-PROCESSOR-ID NOT = 'LGXX'         EL671
00560          MOVE SC-CREDIT-DISPLAY (16)  TO  PI-DISPLAY-CAP          EL671
00561          MOVE SC-CREDIT-UPDATE  (16)  TO  PI-MODIFY-CAP           EL671
00562          IF NOT MODIFY-CAP                                        EL671
00563              MOVE XCTL-684            TO  THIS-PGM                EL671
00564              MOVE 'UPDATE'            TO  SM-READ                 EL671
00565              PERFORM 9995-SECURITY-VIOLATION                      EL671
00566              MOVE ER-0070             TO  EMI-ERROR               EL671
00567              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL671
00568              GO TO 8100-SEND-INITIAL-MAP.                         EL671
00569                                                                   EL671
00570      IF EIBAID  = DFHPF16                                         EL671
00571          EXEC CICS  START                                         EL671
00572              TRANSID  (TRANS-684)                                 EL671
00573              FROM     (PROGRAM-INTERFACE-BLOCK)                   EL671
00574              LENGTH   (PI-COMM-LENGTH)                            EL671
00575          END-EXEC                                                 EL671
00576          MOVE ER-2515            TO  EMI-ERROR                    EL671
00577          GO TO 0320-INPUT-ERROR.                                  EL671
00578                                                                      CL**3
00579      IF EIBAID  = DFHPF17                                            CL**3
00580          MOVE PI-COMPANY-ID      TO CK-COMP-ID                       CL**6
00581          EXEC CICS READ                                              CL**6
00582              DATASET('ELCNTL')                                       CL**6
00583              SET    (ADDRESS OF CONTROL-FILE)                        CL*12
00584              RIDFLD (ELCNTL-KEY)                                     CL**6
00585          END-EXEC                                                    CL**6
00586          CONTINUE                                                    CL*12
00587          IF CO-USES-END-USER-REPORTING                               CL**6
00588              MOVE XCTL-692       TO PGM-NAME                         CL**6
00589              GO TO 9300-XCTL                                         CL**6
00590            ELSE                                                      CL**6
00591              MOVE ER-0007        TO  EMI-ERROR                       CL**6
00592              GO TO 0320-INPUT-ERROR.                                 CL**6
00593                                                                   EL671
00594      IF EIBAID  = DFHPF20                                         EL671
00595        AND  WS-TERMINAL-TYPE = WS-3275-REMOTE                     EL671
00596          MOVE XCTL-690           TO  PGM-NAME                     EL671
00597          GO TO 9300-XCTL.                                         EL671
00598                                                                   EL671
00599      MOVE ER-0029                TO  EMI-ERROR.                   EL671
00600                                                                   EL671
00601  0320-INPUT-ERROR.                                                EL671
00602      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL671
00603                                                                   EL671
00604      MOVE AL-UNBON               TO  SELCODEA.                    EL671
00605      MOVE -1                     TO  SELCODEL.                    EL671
00606                                                                   EL671
00607      GO TO 8200-SEND-DATAONLY.                                    EL671
00608      EJECT                                                        EL671
00609  8100-SEND-INITIAL-MAP.                                           EL671
00610      MOVE LOW-VALUES             TO  EL671AO.                     EL671
00611                                                                   EL671
00612      IF WS-TERMINAL-TYPE  =  WS-3275-REMOTE                       EL671
00613          MOVE AL-SANOF           TO  PCSELA                       EL671
00614      ELSE                                                         EL671
00615          MOVE AL-SADOF           TO  PCSELA.                      EL671
00616                                                                   EL671
00617      MOVE EIBTIME                TO  TIME-IN.                     EL671
00618      MOVE TIME-OUT               TO  RUNTIMEO.                    EL671
00619      MOVE WS-CURRENT-DT          TO  RUNDTEO.                     EL671
00620      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSGO.                     EL671
00621      MOVE -1                     TO  SELCODEL.                    EL671
00622                                                                   EL671
00623      EXEC CICS  SEND                                              EL671
00624          MAP     (MAP-NAME)                                       EL671
00625          MAPSET  (MAPSET-NAME)                                    EL671
00626          FROM    (EL671AO)                                        EL671
00627          ERASE                                                    EL671
00628          CURSOR                                                   EL671
00629      END-EXEC.                                                    EL671
00630                                                                   EL671
00631      GO TO 9100-RETURN-TRAN.                                      EL671
00632                                                                   EL671
00633  8200-SEND-DATAONLY.                                              EL671
00634      IF WS-TERMINAL-TYPE = WS-3275-REMOTE                         EL671
00635          MOVE AL-SANOF           TO  PCSELA                       EL671
00636      ELSE                                                         EL671
00637          MOVE AL-SADOF           TO  PCSELA.                      EL671
00638                                                                   EL671
00639      MOVE WS-CURRENT-DT          TO  RUNDTEO.                     EL671
00640      MOVE EIBTIME                TO  TIME-IN.                     EL671
00641      MOVE TIME-OUT               TO  RUNTIMEO.                    EL671
00642      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSGO.                     EL671
00643                                                                   EL671
00644      EXEC CICS  SEND                                              EL671
00645          MAP     (MAP-NAME)                                       EL671
00646          MAPSET  (MAPSET-NAME)                                    EL671
00647          FROM    (EL671AO)                                        EL671
00648          DATAONLY                                                 EL671
00649          CURSOR                                                   EL671
00650      END-EXEC.                                                    EL671
00651                                                                   EL671
00652      GO TO 9100-RETURN-TRAN.                                      EL671
00653                                                                   EL671
00654  8300-SEND-TEXT.                                                  EL671
00655      EXEC CICS  SEND TEXT                                         EL671
00656          FROM    (LOGOFF-TEXT)                                    EL671
00657          LENGTH  (LOGOFF-LENGTH)                                  EL671
00658          ERASE                                                    EL671
00659          FREEKB                                                   EL671
00660      END-EXEC.                                                    EL671
00661                                                                   EL671
00662      EXEC CICS  RETURN                                            EL671
00663      END-EXEC.                                                    EL671
00664                                                                   EL671
00665  8500-DATE-CONVERT.                                               EL671
00666      EXEC CICS  LINK                                              EL671
00667          PROGRAM   (LINK-ELDATCV)                                 EL671
00668          COMMAREA  (DATE-CONVERSION-DATA)                         EL671
00669          LENGTH    (DC-COMM-LENGTH)                               EL671
00670      END-EXEC.                                                    EL671
00671                                                                   EL671
00672  8500-EXIT.                                                       EL671
00673      EXIT.                                                        EL671
00674                                                                   EL671
00675  8800-UNAUTHORIZED-ACCESS.                                        EL671
00676      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL671
00677                                                                   EL671
00678      GO TO 8300-SEND-TEXT.                                        EL671
00679                                                                   EL671
00680  8810-PF23.                                                       EL671
00681      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL671
00682      MOVE XCTL-005               TO  PGM-NAME.                    EL671
00683                                                                   EL671
00684      GO TO 9300-XCTL.                                             EL671
00685                                                                   EL671
00686  9000-RETURN-CICS.                                                EL671
00687      EXEC CICS  RETURN                                            EL671
00688      END-EXEC.                                                    EL671
00689                                                                   EL671
00690  9100-RETURN-TRAN.                                                EL671
00691      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL671
00692      MOVE '671A'                 TO  PI-CURRENT-SCREEN-NO.        EL671
00693                                                                   EL671
00694      EXEC CICS  RETURN                                            EL671
00695          TRANSID   (TRANS-ID)                                     EL671
00696          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL671
00697          LENGTH    (PI-COMM-LENGTH)                               EL671
00698      END-EXEC.                                                    EL671
00699                                                                   EL671
00700  9200-RETURN-MAIN-MENU.                                           EL671
00701      MOVE XCTL-626               TO  PGM-NAME.                    EL671
00702                                                                   EL671
00703      GO TO 9300-XCTL.                                             EL671
00704                                                                   EL671
00705  9300-XCTL.                                                       EL671
00706      MOVE SPACES                 TO  PI-CONTROL-IN-PROGRESS       EL671
00707                                      PI-PROGRAM-CONTROLS          EL671
00708                                      PI-ENTRY-CD-2                EL671
00709                                      PI-RETURN-CODES              EL671
00710                                      PI-UPDATE-BY.                EL671
00711      MOVE ZEROS                  TO  PI-UPDATE-HHMMSS.            EL671
00712                                                                   EL671
00713      EXEC CICS  XCTL                                              EL671
00714          PROGRAM   (PGM-NAME)                                     EL671
00715          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL671
00716          LENGTH    (PI-COMM-LENGTH)                               EL671
00717      END-EXEC.                                                    EL671
00718                                                                   EL671
00719  9400-CLEAR.                                                      EL671
00720      MOVE XCTL-626               TO  PGM-NAME.                    EL671
00721                                                                   EL671
00722      GO TO 9300-XCTL.                                             EL671
00723                                                                   EL671
00724  9500-PF12.                                                       EL671
00725      MOVE XCTL-010               TO  PGM-NAME.                    EL671
00726                                                                   EL671
00727      GO TO 9300-XCTL.                                             EL671
00728                                                                   EL671
00729  9600-PGMID-ERROR.                                                EL671
00730      EXEC CICS  HANDLE CONDITION                                  EL671
00731          PGMIDERR  (8300-SEND-TEXT)                               EL671
00732      END-EXEC.                                                    EL671
00733                                                                   EL671
00734      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          EL671
00735      MOVE ' '                    TO  PI-ENTRY-CD-1.               EL671
00736      MOVE XCTL-005               TO  PGM-NAME.                    EL671
00737      MOVE PGM-NAME               TO  LOGOFF-PGM.                  EL671
00738      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL671
00739                                                                   EL671
00740      GO TO 9300-XCTL.                                             EL671
00741                                                                   EL671
00742  9900-ERROR-FORMAT.                                               EL671
00743      IF NOT EMI-ERRORS-COMPLETE                                   EL671
00744          MOVE LINK-001           TO  PGM-NAME                     EL671
00745          EXEC CICS  LINK                                          EL671
00746              PROGRAM   (PGM-NAME)                                 EL671
00747              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL671
00748              LENGTH    (EMI-COMM-LENGTH)                          EL671
00749          END-EXEC.                                                EL671
00750                                                                   EL671
00751  9900-EXIT.                                                       EL671
00752      EXIT.                                                        EL671
00753                                                                   EL671
00754  9990-ABEND.                                                      EL671
00755      MOVE LINK-004               TO  PGM-NAME.                    EL671
00756      MOVE DFHEIBLK               TO  EMI-LINE1.                   EL671
00757                                                                   EL671
00758      EXEC CICS  LINK                                              EL671
00759          PROGRAM   (PGM-NAME)                                     EL671
00760          COMMAREA  (EMI-LINE1)                                    EL671
00761          LENGTH    (72)                                           EL671
00762      END-EXEC.                                                    EL671
00763                                                                   EL671
00764      GO TO 8200-SEND-DATAONLY.                                    EL671
00765                                                                   EL671
00766  9995-SECURITY-VIOLATION.                                         EL671
00767                              COPY ELCSCTP.                        EL671
00768                                                                   EL671
00769  9995-EXIT.                                                       EL671
00770      EXIT.                                                        EL671
00771                                                                   EL671
00772  9999-LAST-PARAGRAPH.                                             EL671
00773      GOBACK.                                                      EL671
