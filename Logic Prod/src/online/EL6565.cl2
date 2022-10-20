00001  ID DIVISION.                                                     07/21/98
00002                                                                   EL6565
00003  PROGRAM-ID.                 EL6565.                                 LV013
00004 *              PROGRAM CONVERTED BY                                  CL**9
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**9
00006 *              CONVERSION DATE 11/29/95 08:43:10.                    CL**9
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             CL*13
00008 *                            VMOD=2.011                              CL*13
00009                                                                   EL6565
00010 *AUTHOR.     LOGIC,INC.                                              CL**9
00011 *            DALLAS, TEXAS.                                          CL**9
00012                                                                   EL6565
00013 *DATE-COMPILED.                                                      CL**9
00014 *SECURITY.   *****************************************************   CL**9
00015 *            *                                                   *   CL**9
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**9
00017 *            *                                                   *   CL**9
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**9
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**9
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**9
00021 *            *                                                   *   CL**9
00022 *            *****************************************************   CL**9
00023                                                                   EL6565
00024 *REMARKS.    TRANSACTION - EXEA - RATE TABLE DISPLAY                 CL**2
00025      EJECT                                                        EL6565
00026  ENVIRONMENT DIVISION.                                            EL6565
00027  DATA DIVISION.                                                   EL6565
00028  WORKING-STORAGE SECTION.                                         EL6565
00029  77  FILLER  PIC X(32)  VALUE '********************************'. EL6565
00030  77  FILLER  PIC X(32)  VALUE '*    EL6565 WORKING STORAGE    *'. EL6565
00031  77  FILLER  PIC X(32)  VALUE '************ V/M 2.011 *********'.    CL*13
00032                                                                   EL6565
00033  01  WS-DATE-AREA.                                                EL6565
00034      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL6565
00035      05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.            EL6565
00036                                                                   EL6565
00037  01  WS-AREA.                                                        CL**3
00038      05  DEEDIT-FIELD                PIC X(15).                      CL**5
00039      05  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).         CL**5
00040      05  DEEDIT-FIELD-V1  REDEFINES DEEDIT-FIELD PIC S9(14)V9.       CL**5
00041      05  DEEDIT-FIELD-V2  REDEFINES DEEDIT-FIELD PIC S9(13)V99.      CL**5
00042      05  DEEDIT-FIELD-V3  REDEFINES DEEDIT-FIELD PIC S9(12)V999.     CL**5
00043      05  DEEDIT-FIELD-V4  REDEFINES DEEDIT-FIELD PIC S9(11)V9(4).    CL**5
00044      05  DEEDIT-FIELD-V5  REDEFINES DEEDIT-FIELD PIC S9(10)V9(5).    CL**5
00045      05  DEEDIT-FIELD-V6  REDEFINES DEEDIT-FIELD PIC S9(9)V9(6).     CL**5
00046                                                                      CL**5
00047      05  WS-DEV-PCT          PIC S9V9(6) VALUE ZEROS.                CL*10
00048      05  WS-DEV-RATE         PIC S99V9(6) VALUE ZEROS.               CL*10
00049      05  WS-DEV-RATES        PIC X       VALUE ' '.                  CL**3
00050          88  RATES-DEVIATED              VALUE 'X'.                  CL**3
00051      05  WS-CALLED-FROM-ACCT PIC X(8)    VALUE 'EL6507'.             CL**3
00052      05  WS-CALL-FROM-STATE  PIC X(8)    VALUE 'EL106'.              CL**4
00053      05  WS-CALL-FROM-STATE-ALT PIC X(8) VALUE 'EL1061'.             CL**4
00054      05  WS-COMM-LENGTH      PIC S9(4) COMP VALUE +1525.             CL**5
00055      05  TRANS-ID            PIC X(4)    VALUE 'EXEA'.            EL6565
00056      05  THIS-PGM            PIC X(8)    VALUE 'EL6565'.          EL6565
00057      05  WS-MAPNAME          PIC X(8)    VALUE 'EL6565A'.         EL6565
00058      05  WS-MAPSET-NAME      PIC X(8)    VALUE 'EL6565S'.         EL6565
00059      05  PGM-NAME            PIC X(8).                            EL6565
00060      05  SUB1                PIC S999    VALUE +0.                EL6565
00061      05  SUB2                PIC S999    VALUE +0.                EL6565
00062      05  SUB3                PIC S99     VALUE +0.                EL6565
00063          88  EVEN-SUB              VALUE +2  +4  +6  +8 +10       EL6565
00064                                         +12 +14 +16 +18 +20.      EL6565
00065      05  SUB4                PIC S99     VALUE +0.                EL6565
00066      05  TIME-IN             PIC S9(7)   VALUE +0.                   CL*10
00067      05  TIME-OUT-R  REDEFINES TIME-IN.                           EL6565
00068          10  FILLER          PIC X.                               EL6565
00069          10  TIME-OUT        PIC 99V99.                           EL6565
00070          10  FILLER          PIC XX.                              EL6565
00071      05  XCTL-005            PIC X(8)    VALUE 'EL005'.           EL6565
00072      05  XCTL-010            PIC X(8)    VALUE 'EL010'.           EL6565
00073      05  XCTL-626            PIC X(8)    VALUE 'EL126'.           EL6565
00074      05  LINK-001            PIC X(8)    VALUE 'EL001'.           EL6565
00075      05  LINK-004            PIC X(8)    VALUE 'EL004'.           EL6565
00076      05  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.         EL6565
00077                                                                   EL6565
00078      05  ER-0004             PIC X(4)    VALUE  '0004'.           EL6565
00079      05  ER-0008             PIC X(4)    VALUE  '0008'.           EL6565
00080      05  ER-0029             PIC X(4)    VALUE  '0029'.           EL6565
00081      05  ER-0130             PIC X(4)    VALUE  '0130'.              CL**2
00082      05  ER-0142             PIC X(4)    VALUE  '0142'.           EL6565
00083      05  ER-2055             PIC X(4)    VALUE  '2055'.           EL6565
00084      05  ER-2067             PIC X(4)    VALUE  '2067'.           EL6565
00085      05  ER-2154             PIC X(4)    VALUE  '2154'.              CL**2
00086      05  ER-2238             PIC X(4)    VALUE  '2238'.              CL**2
00087      05  ER-2263             PIC X(4)    VALUE  '2263'.              CL**5
00088                                                                   EL6565
00089      05  ACCT-FILE-ID        PIC X(8)    VALUE 'ERACCT'.             CL**2
00090      05  PLAN-FILE-ID        PIC X(8)    VALUE 'ERPLAN'.             CL**5
00091      05  RATE-FILE-ID        PIC X(8)    VALUE 'ERRATE'.          EL6565
00092                                                                   EL6565
00093      05  WS-SAVE-RATE-KEY    PIC X(28)   VALUE SPACES.               CL**2
00094                                                                   EL6565
00095  EJECT                                                            EL6565
00096                                    COPY ELCSCTM.                     CL**7
00097  EJECT                                                            EL6565
00098                                    COPY ELCSCRTY.                    CL**7
00099      EJECT                                                        EL6565
00100                                    COPY ELCDATE.                     CL**7
00101      EJECT                                                        EL6565
00102                                    COPY ELCLOGOF.                    CL**7
00103      EJECT                                                        EL6565
00104                                    COPY ELCATTR.                     CL**7
00105      EJECT                                                        EL6565
00106                                    COPY ELCEMIB.                     CL**7
00107      EJECT                                                        EL6565
00108                                    COPY ELCINTF.                     CL**7
00109      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.                   CL**9
00110          16  FILLER                       PIC X(41).                 CL**9
00111          16  PI-ACCT-KEY.                                            CL**9
00112              20  PI-ACCT-CCGSA-KEY.                                  CL**9
00113                  24  PI-ACCT-CO             PIC X.                   CL**9
00114                  24  PI-ACCT-CARRIER        PIC X.                   CL**9
00115                  24  PI-ACCT-GROUPING       PIC X(6).                CL**9
00116                  24  PI-ACCT-STATE          PIC XX.                  CL**9
00117                  24  PI-ACCT-ACCOUNT        PIC X(10).               CL**9
00118              20  PI-ACCT-EXP-DT           PIC XX.                    CL**9
00119              20  PI-ACCT-REST-OF-EXP      PIC X(4).                  CL**9
00120          16  FILLER                       PIC X(08).                 CL**9
00121          16  PI-PLAN-KEY.                                            CL**9
00122              20  PI-PLAN-ACCT-KEY.                                   CL**9
00123                  24  PI-PLAN-COMPANY-CD PIC X.                       CL**9
00124                  24  PI-PLAN-CARRIER    PIC X.                       CL**9
00125                  24  PI-PLAN-GROUP      PIC X(6).                    CL**9
00126                  24  PI-PLAN-STATE      PIC X(2).                    CL**9
00127                  24  PI-PLAN-ACCOUNT    PIC X(10).                   CL**9
00128              20  PI-PLAN-BEN-TYPE       PIC X.                       CL**9
00129              20  PI-PLAN-BEN            PIC XX.                      CL**9
00130              20  PI-PLAN-REVISION       PIC X(3).                    CL**9
00131                                                                   EL6565
00132          16  PI-WS-STATE                PIC XX.                      CL**9
00133          16  PI-WS-CLASS                PIC XX.                      CL**9
00134          16  PI-WS-DEV                  PIC X(3).                    CL**9
00135          16  PI-WS-TYPE                 PIC X.                       CL**9
00136          16  PI-WS-PLAN                 PIC XX.                      CL**9
00137          16  PI-PREV-STATE              PIC X(4).                    CL**9
00138                                                                   EL6565
00139 *****************                                                    CL**9
00140          16  PI-SUB                            PIC S999.             CL**9
00141          16  PI-YEAR                           PIC S99.              CL**9
00142          16  PI-NDX                            PIC S99.              CL**9
00143          16  PI-SAVE-CALLING-PGM               PIC X(8).             CL**9
00144          16  PI-ERRATE-EOF-SW                  PIC X.                CL**9
00145              88 ERRATE-EOF                            VALUE 'Y'.     CL**9
00146          16  PI-BROWSE-SW                      PIC X.                CL**9
00147              88 BROWSE-STARTED               VALUE 'Y'.              CL**9
00148          16  PI-STATE-FOUND-SW                 PIC X.                CL**9
00149              88 STATE-NOT-FOUND              VALUE 'N'.              CL**9
00150                                                                      CL**9
00151          16  PI-ERRATE-KEY.                                          CL**9
00152              20  PI-RATE-COMPANY-CD         PIC X.                   CL**9
00153              20  PI-RATE-STATE-CODE.                                 CL**9
00154                  24  PI-RATE-CODE           PIC XX.                  CL**9
00155                  24  PI-RATE-CLASS          PIC XX.                  CL**9
00156                  24  PI-RATE-DEV            PIC XXX.                 CL**9
00157              20  PI-RATE-L-AH-CODE.                                  CL**9
00158                  24  PI-RATE-L-AH           PIC X.                   CL**9
00159                  24  PI-RATE-LAH-NUM        PIC XX.                  CL**9
00160              20  PI-RATE-LIMITS             PIC X(11).               CL**9
00161              20  PI-RATE-EXPIRY-DATE        PIC 9(11)   COMP-3.      CL*12
00162          16  PI-LF-DEV-PCT                  PIC S9V9(6) COMP-3.      CL**9
00163          16  PI-AH-DEV-PCT                  PIC S9V9(6) COMP-3.      CL**9
00164          16  PI-SAVE-RATE-CODE              PIC XX.                  CL**9
00165          16  PI-1ST-TIME                    PIC X.                   CL**9
00166              88  PI-FIRST-TIME-THRU          VALUE 'X'.              CL**9
00167          16  FILLER                        PIC X(468).               CL**9
00168      EJECT                                                        EL6565
00169                              COPY ELCAID.                            CL**7
00170                                                                   EL6565
00171  01  FILLER REDEFINES DFHAID.                                        CL**2
00172      05  FILLER              PIC X(8).                            EL6565
00173      05  PF-VALUES           PIC X       OCCURS 2.                EL6565
00174                                                                   EL6565
00175      EJECT                                                        EL6565
00176                              COPY EL6565S.                           CL**7
00177                                                                   EL6565
00178  01  FILLER REDEFINES EL6565AI.                                      CL**2
00179      05  FILLER             PIC X(66).                               CL**2
00180      05  SCREEN-TABLE       OCCURS 20 TIMES                       EL6565
00181                             INDEXED BY ST-INDX.                   EL6565
00182          10  FILLER         PIC XXX.                                 CL**8
00183          10  YEAR           PIC 99.                               EL6565
00184          10  YEAR-1 REDEFINES YEAR                                   CL**2
00185                             PIC XX.                                  CL**2
00186          10  FILLER         PIC XXX.                                 CL**8
00187          10  MONTH          PIC X(5).                             EL6565
00188          10  FILLER         PIC XXX.                                 CL**8
00189          10  RATE-TABLE     OCCURS 6 TIMES                        EL6565
00190                             INDEXED BY RT-INDX.                   EL6565
00191              15  RATE1      PIC Z(4).99999.                          CL**8
00192              15  RATE2 REDEFINES RATE1                               CL**8
00193                             PIC X(10).                               CL**8
00194                                                                   EL6565
00195      EJECT                                                        EL6565
00196                                                                   EL6565
00197  LINKAGE SECTION.                                                 EL6565
00198                                                                   EL6565
00199  01  DFHCOMMAREA             PIC X(1520).                            CL**3
00200                                                                   EL6565
00201 *01 PARMLIST        COMP.                                            CL**9
00202 *    02  FILLER              PIC S9(8).                              CL**9
00203 *    02  ERACCT-POINTER      PIC S9(8).                              CL**9
00204 *    02  ERPLAN-POINTER      PIC S9(8).                              CL**9
00205 *    02  ERRATE-POINTER      PIC S9(8).                              CL**9
00206      EJECT                                                           CL**2
00207                              COPY ERCACCT.                           CL**7
00208      EJECT                                                           CL**5
00209                              COPY ERCPLAN.                           CL**7
00210      EJECT                                                        EL6565
00211                              COPY ERCRATE.                           CL**7
00212      EJECT                                                        EL6565
00213  PROCEDURE DIVISION.                                              EL6565
00214      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL6565
00215      MOVE '5'                   TO DC-OPTION-CODE.                EL6565
00216      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL6565
00217      MOVE DC-GREG-DATE-1-EDIT   TO SAVE-DATE.                        CL**2
00218      MOVE DC-BIN-DATE-1         TO SAVE-BIN-DATE.                    CL**2
00219                                                                   EL6565
00220      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                 EL6565
00221                                                                   EL6565
00222  0100-START.                                                      EL6565
00223      IF EIBCALEN = 0                                              EL6565
00224          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL6565
00225                                                                   EL6565
00226      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL6565
00227          MOVE PI-CALLING-PROGRAM TO PI-SAVE-CALLING-PGM              CL**2
00228          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL6565
00229              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL6565
00230              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL6565
00231              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL6565
00232              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL6565
00233              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL6565
00234              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL6565
00235              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL6565
00236              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL6565
00237          ELSE                                                     EL6565
00238              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL6565
00239              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL6565
00240              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL6565
00241              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL6565
00242              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL6565
00243              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL6565
00244              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL6565
00245              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL6565
00246                                                                   EL6565
00247      EXEC CICS HANDLE CONDITION                                   EL6565
00248          PGMIDERR (9600-PGMID-ERROR)                              EL6565
00249          ERROR    (9990-ABEND)                                    EL6565
00250      END-EXEC.                                                    EL6565
00251                                                                   EL6565
00252      IF EIBTRNID NOT = TRANS-ID                                   EL6565
00253          MOVE LOW-VALUES TO EL6565AI                              EL6565
00254          MOVE +1         TO PI-SUB                                EL6565
00255                             PI-YEAR                               EL6565
00256          MOVE 'X'        TO PI-1ST-TIME                              CL**8
00257          GO TO 2000-INITIAL-DISPLAY.                              EL6565
00258                                                                   EL6565
00259      EJECT                                                           CL**2
00260      IF EIBAID = DFHCLEAR                                         EL6565
00261          GO TO 9400-CLEAR.                                        EL6565
00262                                                                   EL6565
00263  0200-RECEIVE.                                                    EL6565
00264      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL6565
00265          MOVE ER-0008 TO EMI-ERROR                                EL6565
00266          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6565
00267          MOVE -1   TO STATEL                                      EL6565
00268          GO TO 8200-SEND-DATAONLY.                                EL6565
00269                                                                   EL6565
00270      EXEC CICS RECEIVE                                            EL6565
00271          MAP    (WS-MAPNAME)                                      EL6565
00272          MAPSET (WS-MAPSET-NAME)                                  EL6565
00273          INTO   (EL6565AI)                                        EL6565
00274      END-EXEC.                                                    EL6565
00275                                                                   EL6565
00276      IF PFENTERL = 0                                              EL6565
00277          GO TO 0300-CHECK-PFENTERS.                               EL6565
00278                                                                   EL6565
00279      IF EIBAID NOT = DFHENTER                                     EL6565
00280          MOVE ER-0004 TO EMI-ERROR                                EL6565
00281          GO TO 0310-INPUT-ERROR.                                  EL6565
00282                                                                   EL6565
00283      IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)   EL6565
00284          MOVE PF-VALUES (PFENTERI) TO EIBAID                      EL6565
00285          GO TO 0300-CHECK-PFENTERS                                EL6565
00286      ELSE                                                         EL6565
00287          MOVE ER-0029 TO EMI-ERROR                                EL6565
00288          GO TO 0310-INPUT-ERROR.                                  EL6565
00289                                                                   EL6565
00290      EJECT                                                        EL6565
00291                                                                   EL6565
00292  0300-CHECK-PFENTERS.                                             EL6565
00293      IF EIBAID = DFHPF23                                          EL6565
00294          GO TO 8810-PF23.                                         EL6565
00295                                                                   EL6565
00296      IF EIBAID = DFHPF24                                          EL6565
00297          GO TO 9200-RETURN-MAIN-MENU.                             EL6565
00298                                                                   EL6565
00299      IF EIBAID = DFHPF1                                           EL6565
00300          GO TO 5300-PAGE-FORWARD.                                 EL6565
00301                                                                   EL6565
00302      IF EIBAID = DFHPF2                                           EL6565
00303          GO TO 5400-PAGE-BACKWARD.                                EL6565
00304                                                                   EL6565
00305      IF EIBAID = DFHPF3                                              CL**3
00306          MOVE 'X'            TO WS-DEV-RATES                         CL**3
00307          GO TO 1000-EDIT.                                            CL**3
00308                                                                      CL**3
00309      IF EIBAID = DFHENTER                                         EL6565
00310          GO TO 1000-EDIT.                                         EL6565
00311                                                                   EL6565
00312      MOVE ER-0029 TO EMI-ERROR.                                   EL6565
00313                                                                   EL6565
00314  0310-INPUT-ERROR.                                                EL6565
00315      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6565
00316      MOVE AL-UNBON TO PFENTERA.                                   EL6565
00317      MOVE -1       TO PFENTERL.                                   EL6565
00318                                                                   EL6565
00319      GO TO 8200-SEND-DATAONLY.                                    EL6565
00320                                                                   EL6565
00321      EJECT                                                        EL6565
00322                                                                   EL6565
00323  1000-EDIT.                                                       EL6565
00324      MOVE PI-COMPANY-CD             TO PI-RATE-COMPANY-CD.        EL6565
00325                                                                   EL6565
00326      IF STATEL GREATER ZERO                                       EL6565
00327          MOVE STATEI                TO PI-RATE-CODE               EL6565
00328          MOVE AL-UANON              TO STATEA.                    EL6565
00329                                                                   EL6565
00330      IF CLASSL GREATER ZERO                                       EL6565
00331          MOVE CLASSI                TO PI-RATE-CLASS              EL6565
00332          MOVE AL-UANON              TO CLASSA.                    EL6565
00333                                                                   EL6565
00334      IF DEVL GREATER ZERO                                         EL6565
00335          MOVE DEVI                  TO PI-RATE-DEV                EL6565
00336          MOVE AL-UANON              TO DEVA.                      EL6565
00337                                                                      CL**7
00338      MOVE +1                        TO WS-DEV-PCT.                   CL**7
00339                                                                   EL6565
00340      IF PCTL GREATER ZERO                                            CL**5
00341         MOVE PCTI                   TO DEEDIT-FIELD                  CL**5
00342         PERFORM 8600-DEEDIT                                          CL**5
00343         IF DEEDIT-FIELD-V6 NUMERIC                                   CL**5
00344            MOVE DEEDIT-FIELD-V6   TO WS-DEV-PCT                      CL**5
00345                                      PCTO                            CL**5
00346            MOVE AL-UANON          TO PCTA                            CL**5
00347         ELSE                                                         CL**5
00348            MOVE -1                TO PCTL                            CL**5
00349            MOVE AL-UABON          TO PCTA                            CL**5
00350            MOVE ER-2263           TO EMI-ERROR                       CL**5
00351            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL**5
00352                                                                      CL**5
00353      IF TYPEL GREATER ZERO                                        EL6565
00354          MOVE TYPEI                 TO PI-RATE-L-AH               EL6565
00355          MOVE AL-UANON              TO TYPEA.                     EL6565
00356                                                                   EL6565
00357      IF PLANL GREATER ZERO                                        EL6565
00358          MOVE PLANI                 TO PI-RATE-LAH-NUM            EL6565
00359          MOVE AL-UANON              TO PLANA.                     EL6565
00360                                                                   EL6565
00361      MOVE '99999999999'             TO PI-RATE-LIMITS.               CL*12
00362      MOVE 99999999999               TO PI-RATE-EXPIRY-DATE.          CL*12
00363                                                                   EL6565
00364      EXEC CICS HANDLE CONDITION                                   EL6565
00365          NOTOPEN  (9990-ABEND)                                    EL6565
00366          NOTFND   (8880-NOT-FOUND)                                EL6565
00367      END-EXEC.                                                    EL6565
00368                                                                   EL6565
00369      PERFORM 7650-READ-ERRATE THRU 7650-EXIT.                     EL6565
00370                                                                   EL6565
00371      MOVE +1                     TO PI-SUB                        EL6565
00372                                     PI-YEAR.                      EL6565
00373      GO TO 3000-DISPLAY-RATE.                                     EL6565
00374                                                                   EL6565
00375  EJECT                                                               CL**2
00376  2000-INITIAL-DISPLAY.                                            EL6565
00377      MOVE +1                        TO PI-AH-DEV-PCT                 CL**3
00378                                        PI-LF-DEV-PCT                 CL**5
00379                                        WS-DEV-PCT.                   CL**5
00380                                                                      CL**5
00381      IF PI-SAVE-CALLING-PGM NOT = WS-CALLED-FROM-ACCT                CL**2
00382          GO TO 2010-NO-ACCOUNT.                                      CL**2
00383                                                                      CL**2
00384      MOVE 1 TO PI-NDX.                                               CL**2
00385                                                                      CL**2
00386  2001-GET-ACCOUNT.                                                   CL**2
00387      EXEC CICS HANDLE CONDITION                                      CL**2
00388          NOTOPEN  (9990-ABEND)                                       CL**2
00389          NOTFND   (2525-NO-ACCOUNT)                                  CL**2
00390      END-EXEC.                                                       CL**2
00391                                                                      CL**2
00392      PERFORM 7850-READ-ERACCT THRU 7850-EXIT.                        CL**5
00393                                                                      CL**5
00394      PERFORM 7950-READ-ERPLAN THRU 7950-EXIT.                        CL**5
00395                                                                      CL**2
00396      MOVE PI-COMPANY-CD             TO PI-RATE-COMPANY-CD.           CL**2
00397                                                                      CL**2
00398      MOVE PI-WS-STATE               TO STATEO                        CL**2
00399                                        PI-RATE-CODE.                 CL**2
00400      MOVE ZEROS                     TO CLASSO                        CL**2
00401                                        PI-RATE-CLASS.                CL**2
00402  2005-CHECK-TYPE.                                                    CL**2
00403      IF AM-BENEFIT-TYPE (PI-NDX) = SPACES OR ZEROS OR LOW-VALUES     CL**2
00404        IF PI-NDX = +1                                                CL**2
00405          GO TO 5200-NO-RATE                                          CL**2
00406         ELSE                                                         CL**2
00407          GO TO 5350-ENDFILE.                                         CL**2
00408                                                                      CL**2
00409      IF PI-FIRST-TIME-THRU                                           CL**8
00410        IF AM-BENEFIT-TYPE (PI-NDX) = PI-WS-TYPE  AND                 CL**8
00411           AM-BENEFIT-CODE (PI-NDX) = PI-WS-PLAN                      CL**8
00412              NEXT SENTENCE                                           CL**8
00413            ELSE                                                      CL**8
00414              ADD +1 TO PI-NDX                                        CL**8
00415              GO TO 2005-CHECK-TYPE.                                  CL**8
00416                                                                      CL**8
00417      MOVE SPACE                     TO PI-1ST-TIME.                  CL**8
00418                                                                      CL**8
00419      MOVE AM-BENEFIT-TYPE (PI-NDX)  TO TYPEO                         CL**2
00420                                        PI-RATE-L-AH.                 CL**2
00421      MOVE AM-BENEFIT-CODE (PI-NDX)  TO PLANO                         CL**2
00422                                        PI-RATE-LAH-NUM.              CL**2
00423                                                                      CL**2
00424      MOVE ZEROS                     TO DEVO                          CL**6
00425                                        PI-RATE-DEV.                  CL**6
00426                                                                      CL**2
00427      MOVE ZEROS                     TO PCTO.                         CL**3
00428                                                                      CL**2
00429      IF AM-BENEFIT-TYPE (PI-NDX) = PI-AH-OVERRIDE-L1                 CL**2
00430        IF PL-DEV-PCT NUMERIC AND                                     CL**5
00431           PL-DEV-PCT GREATER ZEROS                                   CL**5
00432          MOVE PL-DEV-PCT            TO WS-DEV-PCT                    CL**5
00433        ELSE                                                          CL**5
00434          MOVE AM-AH-DEVIATION-PCT   TO WS-DEV-PCT                    CL**5
00435        ELSE                                                          CL**2
00436      IF AM-BENEFIT-TYPE (PI-NDX) = PI-LIFE-OVERRIDE-L1               CL**2
00437        IF PL-DEV-PCT NUMERIC AND                                     CL**5
00438           PL-DEV-PCT GREATER ZEROS                                   CL**5
00439          MOVE PL-DEV-PCT            TO WS-DEV-PCT                    CL**5
00440        ELSE                                                          CL**5
00441          MOVE AM-LF-DEVIATION-PCT   TO WS-DEV-PCT.                   CL**5
00442                                                                      CL**5
00443      IF WS-DEV-PCT = ZERO                                            CL**5
00444          MOVE +1.0                  TO WS-DEV-PCT.                   CL**5
00445                                                                      CL**2
00446      GO TO 2020-CONTINUE.                                            CL**2
00447                                                                      CL**2
00448  EJECT                                                               CL**2
00449  2010-NO-ACCOUNT.                                                    CL**2
00450      MOVE PI-COMPANY-CD             TO PI-RATE-COMPANY-CD.        EL6565
00451                                                                   EL6565
00452      MOVE PI-WS-STATE               TO STATEO                     EL6565
00453                                        PI-RATE-CODE                  CL**4
00454                                        PI-SAVE-RATE-CODE.            CL**4
00455      MOVE PI-WS-CLASS               TO CLASSO                     EL6565
00456                                        PI-RATE-CLASS.             EL6565
00457      MOVE PI-WS-DEV                 TO DEVO                       EL6565
00458                                        PI-RATE-DEV.               EL6565
00459      MOVE PI-WS-TYPE                TO TYPEO                      EL6565
00460                                        PI-RATE-L-AH.              EL6565
00461      MOVE PI-WS-PLAN                TO PLANO                      EL6565
00462                                        PI-RATE-LAH-NUM.           EL6565
00463                                                                   EL6565
00464  2020-CONTINUE.                                                      CL**2
00465      MOVE '99999999999'             TO PI-RATE-LIMITS.               CL*12
00466      MOVE 99999999999               TO PI-RATE-EXPIRY-DATE.          CL*12
00467                                                                   EL6565
00468      MOVE AL-UANON                  TO STATEA                     EL6565
00469                                        CLASSA                     EL6565
00470                                        DEVA                       EL6565
00471                                        TYPEA                      EL6565
00472                                        PLANA                         CL**5
00473                                        PCTA.                         CL**5
00474                                                                   EL6565
00475      MOVE +2                        TO STATEL.                    EL6565
00476      MOVE +2                        TO CLASSL.                    EL6565
00477      MOVE +3                        TO DEVL.                      EL6565
00478      MOVE +1                        TO TYPEL.                     EL6565
00479      MOVE +2                        TO PLANL.                     EL6565
00480      MOVE +7                        TO PCTL.                         CL**5
00481                                                                   EL6565
00482      EXEC CICS HANDLE CONDITION                                   EL6565
00483          NOTOPEN  (9990-ABEND)                                    EL6565
00484          NOTFND   (5200-NO-RATE)                                     CL**2
00485      END-EXEC.                                                    EL6565
00486                                                                   EL6565
00487      PERFORM 7650-READ-ERRATE THRU 7650-EXIT.                     EL6565
00488                                                                   EL6565
00489      MOVE +1                     TO PI-SUB                        EL6565
00490                                     PI-YEAR.                      EL6565
00491      GO TO 3000-DISPLAY-RATE.                                     EL6565
00492                                                                   EL6565
00493  2525-NO-ACCOUNT.                                                    CL**2
00494      MOVE ER-2154            TO EMI-ERROR.                           CL**2
00495      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**2
00496      GO TO 8100-SEND-INITIAL-MAP.                                    CL**2
00497  EJECT                                                               CL**2
00498  3000-DISPLAY-RATE.                                               EL6565
00499      MOVE PI-SUB                    TO SUB1.                      EL6565
00500      MOVE PI-YEAR                   TO SUB4.                      EL6565
00501                                                                   EL6565
00502  3025-SET-UP-SCREEN.                                              EL6565
00503      MOVE PI-COMPANY-CD             TO PI-RATE-COMPANY-CD.        EL6565
00504                                                                      CL**5
00505      MOVE WS-DEV-PCT                TO PCTO.                         CL**6
00506                                                                   EL6565
00507      MOVE RT-ST-CODE                TO STATEO                     EL6565
00508                                        PI-RATE-CODE.              EL6565
00509      MOVE RT-ST-CLASS               TO CLASSO                     EL6565
00510                                        PI-RATE-CLASS.             EL6565
00511      MOVE RT-ST-DEV                 TO DEVO                       EL6565
00512                                        PI-RATE-DEV.               EL6565
00513      MOVE RT-L-AH                   TO TYPEO                      EL6565
00514                                        PI-RATE-L-AH.              EL6565
00515      MOVE RT-LAH-NUM                TO PLANO                      EL6565
00516                                        PI-RATE-LAH-NUM.           EL6565
00517                                                                   EL6565
00518      MOVE '99999999999'             TO PI-RATE-LIMITS.               CL*12
00519      MOVE 99999999999               TO PI-RATE-EXPIRY-DATE.          CL*12
00520                                                                   EL6565
00521      MOVE +1     TO SUB2                                          EL6565
00522                     SUB3.                                         EL6565
00523      SET ST-INDX                                                  EL6565
00524          RT-INDX TO +1.                                           EL6565
00525                                                                   EL6565
00526  3050-SET-UP.                                                     EL6565
00527      IF ST-INDX GREATER +20                                       EL6565
00528          MOVE +1     TO SUB1                                      EL6565
00529                         SUB2                                      EL6565
00530                         SUB3                                      EL6565
00531                         SUB4                                      EL6565
00532          SET ST-INDX                                              EL6565
00533              RT-INDX TO +1                                        EL6565
00534          GO TO 8100-SEND-INITIAL-MAP.                             EL6565
00535                                                                   EL6565
00536 *    MOVE SUB4 TO YEAR (ST-INDX).                                    CL**6
00537                                                                   EL6565
00538      IF SUB4 GREATER +10                                          EL6565
00539          MOVE SPACES       TO MONTH    (ST-INDX)                  EL6565
00540                               YEAR-1   (ST-INDX)                  EL6565
00541        ELSE                                                          CL**2
00542          IF EVEN-SUB                                              EL6565
00543              ADD +1 TO SUB4                                       EL6565
00544              MOVE SPACES   TO YEAR-1  (ST-INDX)                   EL6565
00545              MOVE '07-12'  TO MONTH   (ST-INDX)                   EL6565
00546            ELSE                                                      CL**2
00547              MOVE '01-06'  TO MONTH   (ST-INDX).                  EL6565
00548                                                                   EL6565
00549      EJECT                                                           CL**2
00550  3075-SET-UP-RATES.                                               EL6565
00551      IF SUB1 GREATER +120                                         EL6565
00552          MOVE SPACES             TO RATE2  (ST-INDX RT-INDX)      EL6565
00553      ELSE                                                         EL6565
00554          IF RT-L-AH = PI-AH-OVERRIDE-L1                           EL6565
00555            IF RATES-DEVIATED AND                                     CL**3
00556               WS-DEV-PCT        GREATER ZERO AND                     CL**5
00557               RT-AH-RATE (SUB1) GREATER ZERO                         CL**3
00558              COMPUTE WS-DEV-RATE = (RT-AH-RATE (SUB1)                CL**3
00559                                   * WS-DEV-PCT)                      CL**5
00560              MOVE WS-DEV-RATE       TO RATE1 (ST-INDX RT-INDX)       CL**3
00561              MOVE AL-UABON          TO PF3DEVA                       CL**3
00562             ELSE                                                     CL**3
00563              MOVE RT-AH-RATE (SUB1) TO RATE1 (ST-INDX RT-INDX)    EL6565
00564          ELSE                                                     EL6565
00565          IF RT-L-AH = PI-LIFE-OVERRIDE-L1                         EL6565
00566            IF RATES-DEVIATED AND                                     CL**3
00567               WS-DEV-PCT       GREATER ZERO AND                      CL**5
00568               RT-L-RATE (SUB1) GREATER ZERO                          CL**3
00569              COMPUTE WS-DEV-RATE = (RT-L-RATE (SUB1)                 CL**3
00570                                   * WS-DEV-PCT)                      CL**5
00571              MOVE WS-DEV-RATE       TO RATE1 (ST-INDX RT-INDX)       CL**3
00572              MOVE AL-UABON          TO PF3DEVA                       CL**3
00573             ELSE                                                     CL**3
00574              MOVE RT-L-RATE (SUB1)  TO RATE1 (ST-INDX RT-INDX).   EL6565
00575                                                                   EL6565
00576      ADD +1 TO SUB1                                               EL6565
00577                SUB2.                                              EL6565
00578      SET RT-INDX UP BY +1.                                        EL6565
00579                                                                   EL6565
00580      IF RT-INDX GREATER +6                                        EL6565
00581          SET RT-INDX TO +1                                        EL6565
00582          SET ST-INDX UP BY +1                                     EL6565
00583          ADD +1  TO  SUB3                                         EL6565
00584          GO TO 3050-SET-UP                                        EL6565
00585      ELSE                                                         EL6565
00586          GO TO 3075-SET-UP-RATES.                                 EL6565
00587                                                                   EL6565
00588  3099-EXIT.                                                       EL6565
00589       EXIT.                                                          CL**2
00590      EJECT                                                        EL6565
00591  5200-NO-RATE.                                                       CL**2
00592      IF PI-SAVE-CALLING-PGM = WS-CALLED-FROM-ACCT                    CL**4
00593          MOVE ER-2154            TO EMI-ERROR                        CL**2
00594          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**2
00595          GO TO 8100-SEND-INITIAL-MAP.                                CL**4
00596                                                                      CL**2
00597  5300-PAGE-FORWARD.                                                  CL**2
00598      IF PI-SAVE-CALLING-PGM = WS-CALLED-FROM-ACCT                    CL**2
00599          ADD +1 TO PI-NDX                                            CL**2
00600          IF PI-NDX GREATER +20                                       CL**2
00601              GO TO 5350-ENDFILE                                      CL**2
00602           ELSE                                                       CL**2
00603              GO TO 2001-GET-ACCOUNT.                                 CL**2
00604                                                                      CL**2
00605      MOVE SPACES                TO PI-ERRATE-EOF-SW                  CL**4
00606                                    PI-STATE-FOUND-SW.                CL**4
00607                                                                   EL6565
00608      EXEC CICS HANDLE CONDITION                                   EL6565
00609          ENDFILE  (5350-ENDFILE)                                  EL6565
00610          NOTFND   (5350-ENDFILE)                                  EL6565
00611      END-EXEC.                                                    EL6565
00612                                                                   EL6565
00613      MOVE PI-COMPANY-CD         TO PI-RATE-COMPANY-CD.            EL6565
00614      MOVE PI-ERRATE-KEY         TO WS-SAVE-RATE-KEY.              EL6565
00615                                                                   EL6565
00616      PERFORM 6000-START-BROWSE THRU 6000-EXIT.                    EL6565
00617                                                                   EL6565
00618  EJECT                                                               CL**2
00619  5305-READ-NEXT.                                                  EL6565
00620      EXEC CICS HANDLE CONDITION                                   EL6565
00621          ENDFILE  (5350-ENDFILE)                                  EL6565
00622          NOTFND   (5350-ENDFILE)                                  EL6565
00623      END-EXEC.                                                    EL6565
00624                                                                   EL6565
00625      PERFORM 6050-READNEXT     THRU 6050-EXIT.                    EL6565
00626                                                                   EL6565
00627      IF ERRATE-EOF                                                EL6565
00628          MOVE ER-0130            TO EMI-ERROR                        CL**4
00629          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6565
00630          GO TO 8100-SEND-INITIAL-MAP.                                CL**4
00631                                                                      CL**4
00632      IF STATE-NOT-FOUND                                              CL**4
00633          MOVE ER-2154            TO EMI-ERROR                        CL**4
00634          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00635          GO TO 8100-SEND-INITIAL-MAP.                                CL**4
00636                                                                   EL6565
00637      MOVE +1                     TO PI-SUB                        EL6565
00638                                     PI-YEAR.                      EL6565
00639      GO TO 3000-DISPLAY-RATE.                                     EL6565
00640                                                                   EL6565
00641      EJECT                                                           CL**2
00642  5350-ENDFILE.                                                    EL6565
00643      IF BROWSE-STARTED                                            EL6565
00644          PERFORM 6500-ENDBROWSE THRU 6500-EXIT.                      CL**2
00645                                                                      CL**2
00646      MOVE ER-0130            TO EMI-ERROR.                           CL**8
00647      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6565
00648      SUBTRACT +1 FROM PI-NDX.                                        CL**8
00649      GO TO 8200-SEND-DATAONLY.                                       CL**8
00650                                                                   EL6565
00651  EJECT                                                            EL6565
00652  5400-PAGE-BACKWARD.                                              EL6565
00653      IF PI-SAVE-CALLING-PGM = WS-CALLED-FROM-ACCT                    CL**2
00654         IF PI-NDX LESS +2                                            CL**2
00655             GO TO 5500-ENDFILE                                       CL**2
00656          ELSE                                                        CL**2
00657             SUBTRACT +1 FROM PI-NDX                                  CL**2
00658             GO TO 2001-GET-ACCOUNT.                                  CL**2
00659                                                                      CL**2
00660      MOVE SPACES             TO PI-ERRATE-EOF-SW.                 EL6565
00661      MOVE PI-COMPANY-CD      TO PI-RATE-COMPANY-CD.               EL6565
00662                                                                   EL6565
00663      EXEC CICS HANDLE CONDITION                                   EL6565
00664          ENDFILE  (5500-ENDFILE)                                  EL6565
00665          NOTFND   (5500-ENDFILE)                                  EL6565
00666      END-EXEC.                                                    EL6565
00667                                                                   EL6565
00668      PERFORM 6000-START-BROWSE THRU 6000-EXIT.                    EL6565
00669                                                                   EL6565
00670      PERFORM 6800-READ-PREV    THRU 6800-EXIT.                    EL6565
00671                                                                   EL6565
00672  5450-READ-PREV.                                                  EL6565
00673      EXEC CICS HANDLE CONDITION                                   EL6565
00674          ENDFILE  (5500-ENDFILE)                                  EL6565
00675          NOTFND   (5500-ENDFILE)                                  EL6565
00676      END-EXEC.                                                    EL6565
00677                                                                   EL6565
00678      PERFORM 6800-READ-PREV    THRU 6800-EXIT.                    EL6565
00679                                                                   EL6565
00680      IF PI-COMPANY-CD NOT = RT-COMPANY-CD                         EL6565
00681          GO TO 5500-ENDFILE.                                      EL6565
00682                                                                   EL6565
00683      MOVE +1                     TO PI-SUB                        EL6565
00684                                     PI-YEAR.                      EL6565
00685      GO TO 3000-DISPLAY-RATE.                                     EL6565
00686                                                                   EL6565
00687      EJECT                                                           CL**2
00688  5500-ENDFILE.                                                    EL6565
00689      IF BROWSE-STARTED                                            EL6565
00690          PERFORM 6500-ENDBROWSE    THRU 6500-EXIT.                EL6565
00691                                                                   EL6565
00692      MOVE ER-2238            TO EMI-ERROR.                        EL6565
00693      MOVE -1                 TO STATEL.                           EL6565
00694      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6565
00695      GO TO 8200-SEND-DATAONLY.                                    EL6565
00696                                                                   EL6565
00697      EJECT                                                           CL**2
00698  6000-START-BROWSE.                                               EL6565
00699      EXEC CICS STARTBR                                            EL6565
00700          DATASET  (RATE-FILE-ID)                                  EL6565
00701          RIDFLD   (PI-ERRATE-KEY)                                 EL6565
00702      END-EXEC.                                                    EL6565
00703                                                                   EL6565
00704      MOVE 'Y'                   TO PI-BROWSE-SW.                  EL6565
00705                                                                   EL6565
00706  6000-EXIT.                                                       EL6565
00707       EXIT.                                                       EL6565
00708                                                                   EL6565
00709  6050-READNEXT.                                                   EL6565
00710      EXEC CICS READNEXT                                           EL6565
00711          DATASET  (RATE-FILE-ID)                                  EL6565
00712          SET      (ADDRESS OF RATE-RECORD)                           CL**9
00713          RIDFLD   (PI-ERRATE-KEY)                                 EL6565
00714      END-EXEC.                                                    EL6565
00715                                                                   EL6565
00716      IF PI-COMPANY-CD NOT = RT-COMPANY-CD                         EL6565
00717          MOVE 'Y'            TO PI-ERRATE-EOF-SW.                 EL6565
00718                                                                      CL**4
00719      IF (PI-SAVE-CALLING-PGM = WS-CALL-FROM-STATE  OR                CL**8
00720                                WS-CALL-FROM-STATE-ALT)               CL**8
00721           AND                                                        CL**4
00722          PI-SAVE-RATE-CODE NOT = RT-ST-CODE                          CL**8
00723              MOVE 'N'       TO PI-STATE-FOUND-SW.                    CL**8
00724                                                                   EL6565
00725      IF PI-ERRATE-KEY = WS-SAVE-RATE-KEY                          EL6565
00726          GO TO 6050-READNEXT.                                     EL6565
00727                                                                   EL6565
00728      MOVE RT-CONTROL-PRIMARY TO PI-ERRATE-KEY.                    EL6565
00729                                                                   EL6565
00730  6050-EXIT.                                                       EL6565
00731       EXIT.                                                       EL6565
00732                                                                   EL6565
00733      EJECT                                                           CL**2
00734  6500-ENDBROWSE.                                                  EL6565
00735      EXEC CICS ENDBR                                              EL6565
00736          DATASET  (RATE-FILE-ID)                                  EL6565
00737      END-EXEC.                                                    EL6565
00738                                                                   EL6565
00739      MOVE 'N'                TO PI-BROWSE-SW.                     EL6565
00740                                                                   EL6565
00741  6500-EXIT.                                                       EL6565
00742       EXIT.                                                       EL6565
00743                                                                   EL6565
00744      EJECT                                                           CL**2
00745  6800-READ-PREV.                                                  EL6565
00746      EXEC CICS READPREV                                           EL6565
00747          DATASET  (RATE-FILE-ID)                                  EL6565
00748          SET      (ADDRESS OF RATE-RECORD)                           CL**9
00749          RIDFLD   (PI-ERRATE-KEY)                                 EL6565
00750      END-EXEC.                                                    EL6565
00751                                                                   EL6565
00752  6800-EXIT.                                                       EL6565
00753       EXIT.                                                       EL6565
00754                                                                   EL6565
00755  EJECT                                                            EL6565
00756                                                                   EL6565
00757  7650-READ-ERRATE.                                                EL6565
00758      MOVE PI-COMPANY-CD  TO  PI-RATE-COMPANY-CD.                  EL6565
00759                                                                   EL6565
00760      EXEC CICS READ                                               EL6565
00761          DATASET  (RATE-FILE-ID)                                     CL**2
00762          SET      (ADDRESS OF RATE-RECORD)                           CL**9
00763          RIDFLD   (PI-ERRATE-KEY)                                    CL**2
00764      END-EXEC.                                                    EL6565
00765                                                                   EL6565
00766  7650-EXIT.                                                       EL6565
00767       EXIT.                                                          CL**2
00768      EJECT                                                           CL**2
00769                                                                      CL**2
00770  7850-READ-ERACCT.                                                   CL**5
00771      EXEC CICS READ                                                  CL**2
00772          DATASET  (ACCT-FILE-ID)                                     CL**2
00773          SET      (ADDRESS OF ACCOUNT-MASTER)                        CL**9
00774          RIDFLD   (PI-ACCT-KEY)                                      CL**2
00775      END-EXEC.                                                       CL**2
00776                                                                      CL**2
00777  7850-EXIT.                                                          CL**5
00778       EXIT.                                                          CL**5
00779                                                                      CL**5
00780  7950-READ-ERPLAN.                                                   CL**5
00781      EXEC CICS READ                                                  CL**5
00782          DATASET  (PLAN-FILE-ID)                                     CL**5
00783          SET      (ADDRESS OF PLAN-MASTER)                           CL**9
00784          RIDFLD   (PI-PLAN-KEY)                                      CL**5
00785      END-EXEC.                                                       CL**5
00786                                                                      CL**5
00787  7950-EXIT.                                                          CL**2
00788       EXIT.                                                          CL**2
00789                                                                      CL**5
00790      EJECT                                                        EL6565
00791                                                                   EL6565
00792  8100-SEND-INITIAL-MAP.                                           EL6565
00793      MOVE EIBTIME              TO TIME-IN.                        EL6565
00794      MOVE SAVE-DATE            TO RUNDATEO.                       EL6565
00795      MOVE TIME-OUT             TO RUNTIMEO.                       EL6565
00796      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.                       EL6565
00797      MOVE -1                   TO STATEL.                         EL6565
00798      EXEC CICS SEND                                               EL6565
00799          MAP   (WS-MAPNAME)                                       EL6565
00800          MAPSET(WS-MAPSET-NAME)                                   EL6565
00801          FROM  (EL6565AO)                                         EL6565
00802          ERASE                                                    EL6565
00803          CURSOR                                                   EL6565
00804      END-EXEC.                                                    EL6565
00805                                                                   EL6565
00806      GO TO 9100-RETURN-TRAN.                                      EL6565
00807                                                                   EL6565
00808  8200-SEND-DATAONLY.                                              EL6565
00809      MOVE EIBTIME              TO TIME-IN.                        EL6565
00810      MOVE SAVE-DATE            TO RUNDATEO.                       EL6565
00811      MOVE TIME-OUT             TO RUNTIMEO.                       EL6565
00812      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.                       EL6565
00813      MOVE -1                   TO STATEL.                         EL6565
00814      EXEC CICS SEND                                               EL6565
00815          MAP   (WS-MAPNAME)                                       EL6565
00816          MAPSET(WS-MAPSET-NAME)                                   EL6565
00817          FROM  (EL6565AO)                                         EL6565
00818          DATAONLY                                                 EL6565
00819          CURSOR                                                   EL6565
00820      END-EXEC.                                                    EL6565
00821                                                                   EL6565
00822      GO TO 9100-RETURN-TRAN.                                      EL6565
00823                                                                   EL6565
00824      EJECT                                                           CL**2
00825  8300-SEND-TEXT.                                                  EL6565
00826      EXEC CICS SEND TEXT                                          EL6565
00827          FROM  (LOGOFF-TEXT)                                      EL6565
00828          LENGTH(LOGOFF-LENGTH)                                    EL6565
00829          ERASE                                                    EL6565
00830          FREEKB                                                   EL6565
00831      END-EXEC.                                                    EL6565
00832                                                                   EL6565
00833      EXEC CICS RETURN                                             EL6565
00834      END-EXEC.                                                    EL6565
00835                                                                   EL6565
00836  8600-DEEDIT.                                                        CL**5
00837      EXEC CICS BIF DEEDIT                                            CL**5
00838           FIELD (DEEDIT-FIELD)                                       CL**5
00839           LENGTH(15)                                                 CL**5
00840      END-EXEC.                                                       CL**5
00841                                                                      CL**5
00842  8800-UNAUTHORIZED-ACCESS.                                        EL6565
00843      MOVE UNACCESS-MSG TO LOGOFF-MSG.                             EL6565
00844      GO TO 8300-SEND-TEXT.                                        EL6565
00845                                                                   EL6565
00846  8810-PF23.                                                       EL6565
00847      MOVE EIBAID   TO PI-ENTRY-CD-1.                              EL6565
00848      MOVE XCTL-005 TO PGM-NAME.                                   EL6565
00849      GO TO 9300-XCTL.                                             EL6565
00850                                                                   EL6565
00851  8880-NOT-FOUND.                                                  EL6565
00852      MOVE ER-0142 TO EMI-ERROR.                                   EL6565
00853      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6565
00854      MOVE -1        TO STATEL.                                    EL6565
00855                                                                   EL6565
00856      GO TO 8200-SEND-DATAONLY.                                    EL6565
00857                                                                   EL6565
00858      EJECT                                                           CL**2
00859  9000-RETURN-CICS.                                                EL6565
00860      EXEC CICS RETURN                                             EL6565
00861      END-EXEC.                                                    EL6565
00862                                                                   EL6565
00863  9100-RETURN-TRAN.                                                EL6565
00864      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.               EL6565
00865      MOVE '656C'               TO PI-CURRENT-SCREEN-NO.           EL6565
00866      EXEC CICS RETURN                                             EL6565
00867          TRANSID (TRANS-ID)                                       EL6565
00868          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL6565
00869          LENGTH  (WS-COMM-LENGTH)                                 EL6565
00870      END-EXEC.                                                    EL6565
00871                                                                   EL6565
00872  9200-RETURN-MAIN-MENU.                                           EL6565
00873      MOVE XCTL-626 TO PGM-NAME.                                   EL6565
00874      GO TO 9300-XCTL.                                             EL6565
00875                                                                   EL6565
00876  9300-XCTL.                                                       EL6565
00877      EXEC CICS XCTL                                               EL6565
00878          PROGRAM (PGM-NAME)                                       EL6565
00879          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL6565
00880          LENGTH  (WS-COMM-LENGTH)                                 EL6565
00881      END-EXEC.                                                    EL6565
00882                                                                   EL6565
00883      EJECT                                                           CL**2
00884  9400-CLEAR.                                                      EL6565
00885      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.                       EL6565
00886      GO TO 9300-XCTL.                                             EL6565
00887                                                                   EL6565
00888  9500-PF12.                                                       EL6565
00889      MOVE XCTL-010 TO PGM-NAME.                                   EL6565
00890      GO TO 9300-XCTL.                                             EL6565
00891                                                                   EL6565
00892  9600-PGMID-ERROR.                                                EL6565
00893      EXEC CICS HANDLE CONDITION                                   EL6565
00894          PGMIDERR(8300-SEND-TEXT)                                 EL6565
00895      END-EXEC.                                                    EL6565
00896                                                                   EL6565
00897      MOVE PGM-NAME     TO PI-CALLING-PROGRAM.                     EL6565
00898      MOVE ' '          TO PI-ENTRY-CD-1.                          EL6565
00899      MOVE XCTL-005     TO PGM-NAME.                               EL6565
00900      MOVE PGM-NAME     TO LOGOFF-PGM.                             EL6565
00901      MOVE PGMIDERR-MSG TO LOGOFF-FILL.                            EL6565
00902      GO TO 9300-XCTL.                                             EL6565
00903                                                                   EL6565
00904      EJECT                                                           CL**2
00905  9700-LINK-DATE-CONVERT.                                          EL6565
00906      MOVE LINK-ELDATCV TO PGM-NAME.                               EL6565
00907                                                                   EL6565
00908      EXEC CICS LINK                                               EL6565
00909          PROGRAM (PGM-NAME)                                       EL6565
00910          COMMAREA(DATE-CONVERSION-DATA)                           EL6565
00911          LENGTH  (DC-COMM-LENGTH)                                 EL6565
00912      END-EXEC.                                                    EL6565
00913                                                                   EL6565
00914  9700-EXIT.                                                       EL6565
00915       EXIT.                                                          CL**2
00916                                                                   EL6565
00917  9900-ERROR-FORMAT.                                               EL6565
00918      IF NOT EMI-ERRORS-COMPLETE                                   EL6565
00919          MOVE LINK-001 TO PGM-NAME                                EL6565
00920          EXEC CICS LINK                                           EL6565
00921              PROGRAM (PGM-NAME)                                   EL6565
00922              COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)              EL6565
00923              LENGTH  (EMI-COMM-LENGTH)                            EL6565
00924          END-EXEC.                                                EL6565
00925                                                                   EL6565
00926  9900-EXIT.                                                       EL6565
00927       EXIT.                                                          CL**2
00928                                                                   EL6565
00929      EJECT                                                           CL**2
00930  9990-ABEND.                                                      EL6565
00931      MOVE LINK-004               TO PGM-NAME.                     EL6565
00932      MOVE DFHEIBLK               TO EMI-LINE1.                    EL6565
00933      EXEC CICS LINK                                               EL6565
00934          PROGRAM   (PGM-NAME)                                     EL6565
00935          COMMAREA  (EMI-LINE1)                                    EL6565
00936          LENGTH    (72)                                           EL6565
00937      END-EXEC.                                                    EL6565
00938                                                                   EL6565
00939      GO TO 8200-SEND-DATAONLY.                                    EL6565
00940                                                                   EL6565
