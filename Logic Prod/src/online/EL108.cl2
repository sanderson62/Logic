00001  IDENTIFICATION DIVISION.                                         04/20/98
00002                                                                   EL108
00003  PROGRAM-ID.                 EL108 .                                 LV012
00004 *              PROGRAM CONVERTED BY                                  CL*11
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*11
00006 *              CONVERSION DATE 02/12/96 09:45:26.                    CL*11
00007 *                            VMOD=2.011.                             CL*11
00008 *                                                                 EL108
00008 *                                                                 EL108
00009 *AUTHOR.        LOGIC, INC.                                          CL*11
00010 *               DALLAS, TEXAS.                                       CL*11
00011                                                                   EL108
00012 *DATE-COMPILED.                                                      CL*11
00013                                                                      CL*12
00014 *SECURITY.   *****************************************************   CL*11
00015 *            *                                                   *   CL*11
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*11
00017 *            *                                                   *   CL*11
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*11
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*11
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*11
00021 *            *                                                   *   CL*11
00022 *            *****************************************************   CL*11
00023                                                                   EL108
00024 *REMARKS.                                                            CL**7
00025 *        TRANSACTION EX13 - PROGRAM OPTION MAINTENANCE.              CL**7
00026 *                                                                    CL**7
00027  ENVIRONMENT DIVISION.                                            EL108
00028  DATA DIVISION.                                                   EL108
00029  EJECT                                                            EL108
00030  WORKING-STORAGE SECTION.                                         EL108
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL108
00032  77  FILLER  PIC X(32)  VALUE '*    EL108 WORKING STORAGE     *'. EL108
00033  77  FILLER  PIC X(32)  VALUE '************VMOD=2.011 *********'.    CL*11
00034                                                                      CL**4
00035  01  FILE-PGMO-KEY.                                                  CL**4
00036      12  PROGRAM-NUMBER          PIC X(5).                           CL**4
00037      12  OPTION-TYPE             PIC X.                              CL**4
00038      12  OPTION-CODE             PIC X.                              CL**4
00039                                                                      CL**4
00040  01  PROGRAM-OPT-HOLD.                                               CL**4
00041      12  HOLD-PROGRAM-OPT OCCURS 4 TIMES.                            CL**4
00042          16  FREQ-CODE           PIC X(4).                           CL**4
00043          16  PRINT-OPT           PIC X.                              CL**4
00044          16  FORMAT-OPT          PIC X.                              CL**4
00045          16  PROCESS-OPT         PIC X.                              CL**4
00046          16  TOTAL-OPT           PIC X.                              CL**4
00047                                                                   EL108
00048      COPY ELCSCTM.                                                   CL**8
00049      COPY ELCSCRTY.                                                  CL**8
00050                                                                   EL108
00051     EJECT                                                         EL108
00052                                                                   EL108
00053  01  WS-DATE-AREA.                                                EL108
00054      12  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL108
00055      12  SAVE-BIN-DATE       PIC XX      VALUE SPACES.            EL108
00056                                                                   EL108
00057  01  WS-AREA.                                                     EL108
00058      12  TIME-IN                 PIC S9(7).                       EL108
00059      12  FILLER REDEFINES TIME-IN.                                EL108
00060         16  FILLER               PIC X.                           EL108
00061         16  TIME-OUT             PIC 99V99.                       EL108
00062         16  FILLER               PIC XX.                          EL108
00063      12  SC-ITEM                 PIC S9(4) COMP VALUE +1.         EL108
00064      12  XCTL-EL005              PIC X(8)    VALUE 'EL005'.       EL108
00065      12  XCTL-EL010              PIC X(8)    VALUE 'EL010'.       EL108
00066      12  XCTL-EL126              PIC X(8)    VALUE 'EL126'.       EL108
00067      12  XCTL-EL626              PIC X(8)    VALUE 'EL626'.          CL**2
00068      12  XCTL-EM626              PIC X(8)    VALUE 'EM626'.          CL**4
00069      12  XCTL-GL800              PIC X(8)    VALUE 'GL800'.          CL**4
00070      12  THIS-PGM                PIC X(8)    VALUE 'EL108'.       EL108
00071      12  LIT-EX13                PIC X(4)    VALUE 'EX13'.        EL108
00072      12  LIT-MAP                 PIC X(4)    VALUE '108A'.        EL108
00073      12  LIT-SPACE               PIC X       VALUE SPACE.         EL108
00074  EJECT                                                            EL108
00075  01  EDIT-WORK-AREA.                                              EL108
00076      12  COUNT-1                 PIC 9.                           EL108
00077      12  CALL-PGM                PIC X(8).                        EL108
00078      12  TRANS-ID                PIC X(4).                        EL108
00079      12  CHECK-PFKEYS            PIC 99.                          EL108
00080      12  CHECK-MAINT             PIC X.                           EL108
00081          88  SHOW-OPTION                     VALUE 'S'.           EL108
00082          88  CHANGE-OPTION                   VALUE 'C'.           EL108
00083          88  VALID-CODE                      VALUE 'C' 'S'.       EL108
00084      12  CHECK-FREQ              PIC X(4).                        EL108
00085          88  OPTION-ALWAYS-SET               VALUE 'NONE'.        EL108
00086 *        88  SET-ON-IF-EDIT                  VALUE 'ED=Y'.           CL**3
00087 *        88  SET-ON-IF-NOT-EDIT              VALUE 'ED=N'.           CL**3
00088 *        88  SET-ON-IF-UPDATE                VALUE 'UP=Y'.           CL**3
00089 *        88  SET-ON-IF-NOT-UPDATE            VALUE 'UP=N'.           CL**3
00090          88  SET-ON-IF-YEAR-END              VALUE 'MO=Y'.        EL108
00091          88  SET-ON-IF-NOT-YEAR-END          VALUE 'MO/Y'.        EL108
00092          88  SET-ON-IF-MO-3-6-9-12           VALUE 'MO=Q'.        EL108
00093          88  SET-ON-IF-NOT-QTR-MO            VALUE 'MO/Q'.        EL108
00094          88  SET-ON-IF-JAN                   VALUE 'MO=1'.        EL108
00095          88  SET-ON-IF-FEB                   VALUE 'MO=2'.        EL108
00096          88  SET-ON-IF-MARCH                 VALUE 'MO=3'.        EL108
00097          88  SET-ON-IF-APRIL                 VALUE 'MO=4'.        EL108
00098          88  SET-ON-IF-MAY                   VALUE 'MO=5'.        EL108
00099          88  SET-ON-IF-JUNE                  VALUE 'MO=6'.        EL108
00100          88  SET-ON-IF-JULY                  VALUE 'MO=7'.        EL108
00101          88  SET-ON-IF-AUG                   VALUE 'MO=8'.        EL108
00102          88  SET-ON-IF-SEPT                  VALUE 'MO=9'.        EL108
00103          88  SET-ON-IF-OCT                   VALUE 'MO=A'.        EL108
00104          88  SET-ON-IF-NOV                   VALUE 'MO=B'.        EL108
00105          88  SET-ON-IF-DEC                   VALUE 'MO=C'.        EL108
00106      12  CHECK-PRINT             PIC X.                           EL108
00107          88  PRINT-TO-HARDCOPY               VALUE 'P'.           EL108
00108          88  PRINT-TO-FICHE-TAPE             VALUE 'F'.           EL108
00109          88  PRINT-TO-BOTH-MEDIA             VALUE 'B'.           EL108
00110          88  SAVE-REPORT-ONLINE-NO-PRINT     VALUE 'S'.           EL108
00111          88  SAVE-REPORT-ONLINE-AND-PRINT    VALUE 'T'.           EL108
00112      12  HOLD-PROGRAM            PIC X(5).                        EL108
00113      12  HOLD-TYPE               PIC X.                           EL108
00114      12  BROWSE-STARTED-SW       PIC X       VALUE ' '.           EL108
00115          88  BROWSE-STARTED                  VALUE '1'.           EL108
00116  EJECT                                                            EL108
00117  01  ERROR-NUMBERS.                                               EL108
00118      12  ER-0000                 PIC X(4)    VALUE '0000'.        EL108
00119      12  ER-0023                 PIC X(4)    VALUE '0023'.        EL108
00120      12  ER-0029                 PIC X(4)    VALUE '0029'.        EL108
00121      12  ER-0050                 PIC X(4)    VALUE '0050'.        EL108
00122      12  ER-0066                 PIC X(4)    VALUE '0066'.        EL108
00123      12  ER-0067                 PIC X(4)    VALUE '0067'.        EL108
00124      12  ER-0070                 PIC X(4)    VALUE '0070'.        EL108
00125      12  ER-0162                 PIC X(4)    VALUE '0162'.        EL108
00126      12  ER-0163                 PIC X(4)    VALUE '0163'.        EL108
00127      12  ER-0164                 PIC X(4)    VALUE '0164'.        EL108
00128      12  ER-0165                 PIC X(4)    VALUE '0165'.        EL108
00129      12  ER-0166                 PIC X(4)    VALUE '0166'.        EL108
00130      12  ER-0167                 PIC X(4)    VALUE '0167'.        EL108
00131      12  ER-0267                 PIC X(4)    VALUE '0267'.        EL108
00132      12  ER-0269                 PIC X(4)    VALUE '0269'.        EL108
00133      12  ER-0486                 PIC X(4)    VALUE '0486'.        EL108
00134      12  ER-0599                 PIC X(4)    VALUE '0599'.        EL108
00135      12  ER-7008                 PIC X(4)    VALUE '7008'.        EL108
00136  EJECT                                                            EL108
00137  01  ERROR-SWITCHES.                                              EL108
00138      12  ERROR-SWITCH            PIC X.                           EL108
00139          88  SCREEN-ERROR                    VALUE 'X'.           EL108
00140      12  TYPE-SWITCH             PIC X.                           EL108
00141          88  END-OF-TYPE                     VALUE 'X'.           EL108
00142                                                                   EL108
00143  01  FILE-PGMS-KEY.                                               EL108
00144      12  COMPANY-CD              PIC X.                           EL108
00145      12  PROGRAM-NO.                                              EL108
00146          16  PROGRAM-LIT         PIC XX.                          EL108
00147          16  PROGRAM-SEQ         PIC XXX.                         EL108
00148                                                                   EL108
00149  01  COMP-LENGTHS.                                                EL108
00150      12  PGMS-LENGTH             PIC S9(4)   COMP VALUE +250.     EL108
00151      12  GENERIC-LENGTH          PIC S9(4)   COMP VALUE +5.       EL108
00152                                                                   EL108
00153  01  PROGRAM-OPT-DESC.                                            EL108
00154      12  DESC-PROGRAM-OPT OCCURS 9 TIMES.                         EL108
00155          16  FILLER              PIC XXX.                         EL108
00156          16  OPT-NO              PIC X.                           EL108
00157          16  FILLER              PIC X.                           EL108
00158          16  CONST-FILL          PIC X.                           EL108
00159          16  FILLER              PIC X.                           EL108
00160          16  PGMO-DESC           PIC X(40).                       EL108
00161          16  FILLER              PIC X(32).                       EL108
00162                                                                   EL108
00163  01  FREQUENCY-OPTIONS.                                           EL108
00164      12  FILLER                  PIC X(76)                        EL108
00165          VALUE 'NONE - OPTION ALWAYS SET'.                        EL108
00166 *    12  FILLER                  PIC X(76)                           CL**3
00167 *        VALUE 'ED=Y RUN WHEN EDIT = YES'.                           CL**3
00168 *    12  FILLER                  PIC X(76)                           CL**3
00169 *        VALUE 'ED=N RUN WHEN EDIT = NO'.                            CL**3
00170 *    12  FILLER                  PIC X(76)                           CL**3
00171 *        VALUE 'UP=Y RUN WHEN UPDATE = YES'.                         CL**3
00172 *    12  FILLER                  PIC X(76)                           CL**3
00173 *        VALUE 'UP=N RUN WHEN UPDATE = NO'.                          CL**3
00174      12  FILLER                  PIC X(76)                        EL108
00175          VALUE 'MO=Y RUN WHEN YEARLY'.                            EL108
00176      12  FILLER                  PIC X(35)                        EL108
00177          VALUE 'MO=X RUN WHEN MONTH = X. '.                       EL108
00178      12  FILLER                  PIC X(41)                        EL108
00179          VALUE 'X MAY BE 1 THRU C WHERE 1=JAN,A=OCT,C=DEC'.       EL108
00180      12  FILLER                  PIC X(76)                        EL108
00181          VALUE 'MO=Q RUN AT QUARTER END (MAR,JUNE,SEPT,DEC)'.     EL108
00182                                                                   EL108
00183  01  OPTIONS-FOR-FEQUENCY REDEFINES FREQUENCY-OPTIONS.            EL108
00184      12  OPTIONS-FREQ OCCURS 4 TIMES.                                CL**3
00185          16  FILLER              PIC X(76).                       EL108
00186                                                                   EL108
00187  01  PRINT-OPTIONS.                                               EL108
00188      12  FILLER                  PIC X(36)                        EL108
00189          VALUE '   P = PRINT TO HARD COPY'.                       EL108
00190      12  FILLER                  PIC X(36)                        EL108
00191          VALUE '   F = PRINT TO FICHE TAPE'.                      EL108
00192      12  FILLER                  PIC X(36)                        EL108
00193          VALUE '   B = PRINT TO BOTH MEDIA'.                      EL108
00194      12  FILLER                  PIC X(36)                        EL108
00195          VALUE '   S = SAVE REPORT ONLINE - NO PRINT'.            EL108
00196      12  FILLER                  PIC X(36)                        EL108
00197          VALUE '   T = SAVE REPORT ONLINE AND PRINT'.             EL108
00198                                                                   EL108
00199  01  OPTIONS-TO-PRINT REDEFINES PRINT-OPTIONS.                    EL108
00200      12  OPTIONS-PRINT OCCURS 5 TIMES.                            EL108
00201          16  FILLER              PIC X(36).                       EL108
00202                                                                   EL108
00203  01  DO-NOT-RUN                  PIC X(36)                        EL108
00204          VALUE '   X = DO NOT RUN'.                               EL108
00205  EJECT                                                            EL108
00206      COPY ELCLOGOF.                                                  CL**8
00207  EJECT                                                            EL108
00208      COPY ELCDATE.                                                   CL**8
00209  EJECT                                                            EL108
00210      COPY ELCATTR.                                                   CL**8
00211  EJECT                                                            EL108
00212      COPY ELCAID.                                                    CL**8
00213                                                                   EL108
00214  01  FILLER REDEFINES DFHAID.                                     EL108
00215      12  FILLER                  PIC X(8).                        EL108
00216      12  AID-KEYS OCCURS 24 TIMES.                                EL108
00217          16  FILLER              PIC X(1).                        EL108
00218  EJECT                                                            EL108
00219      COPY ELCINTF.                                                   CL**8
00220                                                                   EL108
00221      12  PI-PRG-WRK-AREA REDEFINES PI-PROGRAM-WORK-AREA.          EL108
00222          16  PI-PROGRAM-NO   PIC X(5).                            EL108
00223          16  FILLER          PIC X(635).                             CL*11
00224  EJECT                                                            EL108
00225      COPY ELCEMIB.                                                   CL**8
00226  EJECT                                                            EL108
00227      COPY ELCJPFX.                                                   CL**8
00228                                  PIC X(259).                      EL108
00229  EJECT                                                            EL108
00230      COPY EL108S.                                                    CL**8
00231  EJECT                                                            EL108
00232  LINKAGE SECTION.                                                 EL108
00233  01  DFHCOMMAREA                 PIC X(1024).                     EL108
00234                                                                   EL108
00235 *01 PARM-LIST .                                                      CL*11
00236 *    12  FILLER                  PIC S9(8)   COMP.                   CL*11
00237 *    12  PGMS-PNT                PIC S9(8)   COMP.                   CL*11
00238 *    12  PGMO-PNT                PIC S9(8)   COMP.                   CL*11
00239  EJECT                                                            EL108
00240      COPY ELCPGMS.                                                   CL**8
00241  EJECT                                                            EL108
00242      COPY ELCPGMO.                                                   CL**8
00243  EJECT                                                            EL108
00244  PROCEDURE DIVISION.                                              EL108
00245                                                                   EL108
00246      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL108
00247      MOVE '5'                   TO DC-OPTION-CODE.                EL108
00248      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL108
00249      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL108
00250      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL108
00251                                                                   EL108
00252      IF EIBCALEN = ZERO                                           EL108
00253          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL108
00254                                                                   EL108
00255      EXEC CICS HANDLE CONDITION                                   EL108
00256          PGMIDERR (8820-XCTL-ERROR)                               EL108
00257          ERROR    (9990-ABEND)                                    EL108
00258      END-EXEC.                                                    EL108
00259                                                                   EL108
00260      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                 EL108
00261      MOVE SPACES      TO ERROR-SWITCHES MSGO.                     EL108
00262      MOVE LIT-EX13    TO TRANS-ID.                                EL108
00263                                                                   EL108
00264      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL108
00265          MOVE LOW-VALUES TO EL108AO                               EL108
00266          MOVE ER-7008    TO EMI-ERROR                             EL108
00267          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL108
00268          MOVE -1         TO MAINTL                                EL108
00269          GO TO 8110-SEND-DATA.                                    EL108
00270                                                                   EL108
00271      IF THIS-PGM = PI-CALLING-PROGRAM                             EL108
00272          GO TO 0050-CHECK-CLEAR.                                  EL108
00273                                                                   EL108
00274      IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                       EL108
00275          MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6          EL108
00276          MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5          EL108
00277          MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4          EL108
00278          MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3          EL108
00279          MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2          EL108
00280          MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1          EL108
00281          MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM        EL108
00282          MOVE THIS-PGM             TO PI-CALLING-PROGRAM          EL108
00283        ELSE                                                       EL108
00284          MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM          EL108
00285          MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM        EL108
00286          MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1          EL108
00287          MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2          EL108
00288          MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3          EL108
00289          MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4          EL108
00290          MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5          EL108
00291          MOVE SPACES               TO PI-SAVED-PROGRAM-6.         EL108
00292                                                                   EL108
00293      MOVE LOW-VALUES           TO EL108AO.                        EL108
00294      MOVE ZEROS                TO COUNT-1.                        EL108
00295      PERFORM 5130-SPACE-SCREEN THRU 5140-EXIT 9 TIMES.            EL108
00296      MOVE -1                   TO MAINTL.                         EL108
00297      GO TO 8100-SEND-INITIAL-MAP.                                 EL108
00298                                                                   EL108
00299  0050-CHECK-CLEAR.                                                EL108
00300      IF EIBAID = DFHCLEAR                                         EL108
00301          GO TO 8200-RETURN-PRIOR.                                 EL108
00302                                                                   EL108
00303      IF PI-PROCESSOR-ID = 'LGXX'                                  EL108
00304          GO TO 0200-RECEIVE.                                      EL108
00305                                                                   EL108
00306      EXEC CICS READQ TS                                           EL108
00307          QUEUE  (PI-SECURITY-TEMP-STORE-ID)                       EL108
00308          INTO   (SECURITY-CONTROL)                                EL108
00309          LENGTH (SC-COMM-LENGTH)                                  EL108
00310          ITEM   (SC-ITEM)                                         EL108
00311      END-EXEC.                                                    EL108
00312                                                                   EL108
00313      MOVE SC-CREDIT-DISPLAY (02)  TO PI-DISPLAY-CAP.              EL108
00314      MOVE SC-CREDIT-UPDATE  (02)  TO PI-MODIFY-CAP.               EL108
00315                                                                   EL108
00316      IF NOT DISPLAY-CAP                                           EL108
00317          MOVE 'READ'          TO SM-READ                          EL108
00318          PERFORM 9995-SECURITY-VIOLATION                          EL108
00319          MOVE ER-0070         TO  EMI-ERROR                       EL108
00320          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL108
00321          GO TO 8100-SEND-INITIAL-MAP.                             EL108
00322                                                                   EL108
00323       EJECT                                                       EL108
00324  0200-RECEIVE.                                                    EL108
00325      EXEC CICS RECEIVE                                            EL108
00326          MAP    ('EL108A')                                        EL108
00327          MAPSET ('EL108S')                                        EL108
00328      END-EXEC.                                                    EL108
00329                                                                   EL108
00330      IF PFKEYL GREATER THAN ZERO                                  EL108
00331          PERFORM 0200-TRANS-PF THRU 0210-EXIT.                    EL108
00332                                                                   EL108
00333      IF EIBAID = DFHPF1                                           EL108
00334          PERFORM 0300-RESET-ATTRB THRU 0310-EXIT                  EL108
00335          GO TO 5000-SHOW-FREQ.                                    EL108
00336                                                                      CL**7
00337      IF EIBAID = DFHPF2                                           EL108
00338          PERFORM 0300-RESET-ATTRB THRU 0310-EXIT                  EL108
00339          GO TO 5100-SHOW-PRINT.                                   EL108
00340                                                                      CL**7
00341      IF EIBAID = DFHPF3                                           EL108
00342          PERFORM 0300-RESET-ATTRB THRU 0310-EXIT                  EL108
00343          GO TO 5400-SHOW-FORMAT.                                  EL108
00344                                                                      CL**7
00345      IF EIBAID = DFHPF4                                           EL108
00346          PERFORM 0300-RESET-ATTRB THRU 0310-EXIT                  EL108
00347          GO TO 5300-SHOW-PROCESS.                                 EL108
00348                                                                      CL**7
00349      IF EIBAID = DFHPF5                                           EL108
00350          PERFORM 0300-RESET-ATTRB THRU 0310-EXIT                  EL108
00351          GO TO 5200-SHOW-TOTAL.                                   EL108
00352                                                                      CL**7
00353      MOVE SPACES                 TO  VARDESCO.                       CL**7
00354      MOVE ZEROS                  TO  COUNT-1.                        CL**7
00355                                                                      CL**7
00356      PERFORM 5130-SPACE-SCREEN  THRU  5140-EXIT  9  TIMES.           CL**7
00357                                                                      CL**7
00358      IF EIBAID = DFHPF6                                           EL108
00359          PERFORM 0300-RESET-ATTRB THRU 0310-EXIT                  EL108
00360          GO TO 3100-SHOW-OPTIONS.                                 EL108
00361                                                                      CL**7
00362      IF EIBAID = DFHPF7                                           EL108
00363          PERFORM 0300-RESET-ATTRB THRU 0310-EXIT                  EL108
00364          GO TO 3100-SHOW-OPTIONS.                                 EL108
00365                                                                   EL108
00366      IF EIBAID = DFHPF12                                          EL108
00367          GO TO 8300-GET-HELP.                                     EL108
00368      IF EIBAID = DFHPF23                                          EL108
00369          GO TO 8810-PF23-ENTERED.                                 EL108
00370      IF EIBAID = DFHPF24                                          EL108
00371          GO TO 8400-RETURN-MASTER.                                EL108
00372                                                                   EL108
00373      IF EIBAID NOT = DFHENTER                                     EL108
00374          MOVE ER-0029 TO EMI-ERROR                                EL108
00375          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL108
00376          MOVE -1 TO MAINTL                                        EL108
00377          GO TO 8110-SEND-DATA.                                    EL108
00378                                                                   EL108
00379      PERFORM 0300-RESET-ATTRB THRU 0310-EXIT.                     EL108
00380      PERFORM 1000-EDIT-SCREEN THRU 1010-EXIT.                     EL108
00381                                                                   EL108
00382      IF SCREEN-ERROR                                              EL108
00383          GO TO 8110-SEND-DATA.                                    EL108
00384                                                                   EL108
00385      MOVE SPACES TO PROGRAM-OPT-HOLD.                             EL108
00386                                                                   EL108
00387      IF  CHANGE-OPTION                                               CL**9
00388          PERFORM 2000-CHANGE-OPTIONS THRU 2040-EXIT               EL108
00389                                                                      CL**9
00390          IF  NOT EMI-NO-ERRORS                                       CL**9
00391              GO TO 8110-SEND-DATA                                    CL**9
00392                                                                      CL**9
00393          ELSE                                                        CL**9
00394              PERFORM 4000-SHOW-OPTIONS THRU 4020-EXIT                CL**9
00395              MOVE ER-0000 TO EMI-ERROR                               CL**9
00396              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**9
00397                                                                   EL108
00398      IF SHOW-OPTION                                               EL108
00399          PERFORM 4000-SHOW-OPTIONS THRU 4020-EXIT.                EL108
00400                                                                   EL108
00401      MOVE -1    TO MAINTL.                                        EL108
00402      MOVE SPACE TO MAINTO.                                        EL108
00403      GO TO 8110-SEND-DATA.                                        EL108
00404  EJECT                                                            EL108
00405                                                                   EL108
00406  0200-TRANS-PF.                                                   EL108
00407      IF PFKEYI NOT NUMERIC                                        EL108
00408          MOVE ER-7008         TO EMI-ERROR                        EL108
00409          GO TO 0205-ERROR.                                        EL108
00410                                                                   EL108
00411      MOVE PFKEYI TO CHECK-PFKEYS.                                 EL108
00412                                                                   EL108
00413      IF CHECK-PFKEYS LESS 1 OR GREATER 24                         EL108
00414          MOVE ER-7008         TO EMI-ERROR                        EL108
00415          GO TO 0205-ERROR.                                        EL108
00416                                                                   EL108
00417      MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.                      EL108
00418                                                                   EL108
00419      GO TO 0210-EXIT.                                             EL108
00420                                                                   EL108
00421  0205-ERROR.                                                      EL108
00422      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL108
00423      MOVE -1                     TO MAINTL.                       EL108
00424      GO TO 8110-SEND-DATA.                                        EL108
00425                                                                   EL108
00426  0210-EXIT.                                                       EL108
00427      EXIT.                                                        EL108
00428                                                                   EL108
00429  0300-RESET-ATTRB.                                                EL108
00430      MOVE AL-UANON TO MAINTA                                      EL108
00431                       PGRMA.                                      EL108
00432  0310-EXIT.                                                       EL108
00433      EXIT.                                                        EL108
00434  EJECT                                                            EL108
00435  1000-EDIT-SCREEN.                                                EL108
00436      MOVE MAINTI TO CHECK-MAINT.                                  EL108
00437      IF NOT VALID-CODE                                            EL108
00438          MOVE AL-UABON TO MAINTA                                  EL108
00439          MOVE -1       TO MAINTL                                  EL108
00440          MOVE 'X'      TO ERROR-SWITCH                            EL108
00441          MOVE ER-0023  TO EMI-ERROR                               EL108
00442          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL108
00443                                                                   EL108
00444      IF PGRMI = ZEROS OR LOW-VALUES                               EL108
00445          MOVE -1       TO PGRML                                   EL108
00446          MOVE AL-UABON TO PGRMA                                   EL108
00447          MOVE 'X'      TO ERROR-SWITCH                            EL108
00448          MOVE ER-0162  TO EMI-ERROR                               EL108
00449          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL108
00450          GO TO 1010-EXIT.                                         EL108
00451                                                                   EL108
00452      MOVE PGRMI TO PROGRAM-NO.                                    EL108
00453                                                                   EL108
00454      IF PROGRAM-LIT NOT = 'EL' AND 'EC' AND 'GL' AND 'EM'            CL**8
00455          MOVE -1       TO PGRML                                   EL108
00456          MOVE AL-UABON TO PGRMA                                   EL108
00457          MOVE 'X'      TO ERROR-SWITCH                            EL108
00458          MOVE ER-0162  TO EMI-ERROR                               EL108
00459          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL108
00460          GO TO 1010-EXIT.                                         EL108
00461                                                                   EL108
00462      IF CHANGE-OPTION                                             EL108
00463         IF NOT MODIFY-CAP                                         EL108
00464             MOVE 'UPDATE'       TO SM-READ                        EL108
00465             PERFORM 9995-SECURITY-VIOLATION                       EL108
00466             MOVE 'X'        TO ERROR-SWITCH                       EL108
00467             MOVE AL-UABON   TO MAINTA                             EL108
00468             MOVE ER-0070    TO EMI-ERROR                          EL108
00469             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             EL108
00470                                                                   EL108
00471      MOVE PI-COMPANY-CD TO COMPANY-CD.                            EL108
00472      MOVE PGRMI         TO PROGRAM-NUMBER.                        EL108
00473                                                                   EL108
00474  1010-EXIT.                                                       EL108
00475      EXIT.                                                        EL108
00476                                                                   EL108
00477  EJECT                                                            EL108
00478  2000-CHANGE-OPTIONS.                                             EL108
00479      MOVE ZEROS TO COUNT-1.                                       EL108
00480      PERFORM 2100-VERIFY-FREQ THRU 2110-EXIT.                     EL108
00481                                                                   EL108
00482      MOVE ZEROS TO COUNT-1.                                       EL108
00483      PERFORM 2200-VERIFY-PRINT THRU 2210-EXIT.                    EL108
00484                                                                   EL108
00485      MOVE ZEROS TO COUNT-1.                                       EL108
00486      PERFORM 2300-VERIFY-FORMAT THRU 2310-EXIT.                   EL108
00487                                                                   EL108
00488      MOVE ZEROS TO COUNT-1.                                       EL108
00489      PERFORM 2400-VERIFY-PROCESS THRU 2410-EXIT.                  EL108
00490                                                                   EL108
00491      MOVE ZEROS TO COUNT-1.                                       EL108
00492      PERFORM 2500-VERIFY-TOTAL THRU 2510-EXIT.                    EL108
00493                                                                   EL108
00494      PERFORM 2600-CHECK-REQUIRE THRU 2650-EXIT.                   EL108
00495                                                                   EL108
00496      IF NOT EMI-NO-ERRORS                                         EL108
00497         GO TO 2040-EXIT.                                          EL108
00498                                                                   EL108
00499      EXEC CICS HANDLE CONDITION                                   EL108
00500          NOTFND (2010-PGMS-NOT-FOUND)                             EL108
00501      END-EXEC.                                                    EL108
00502                                                                   EL108
00503      EXEC CICS READ                                               EL108
00504          SET     (ADDRESS OF PROGRAM-OPTIONS-SELECTED)               CL*11
00505          DATASET ('ELPGMS')                                       EL108
00506          RIDFLD  (FILE-PGMS-KEY)                                  EL108
00507          UPDATE                                                   EL108
00508      END-EXEC.                                                    EL108
00509                                                                   EL108
00510      MOVE SAVE-BIN-DATE          TO  PS-LAST-MAINT-DT.               CL**6
00511      MOVE EIBTIME                TO  PS-LAST-MAINT-HHMMSS.           CL**6
00512      MOVE PI-PROCESSOR-ID        TO  PS-LAST-MAINT-USER.             CL**6
00513                                                                      CL**6
00514      MOVE ZEROS TO COUNT-1.                                       EL108
00515      PERFORM 3000-FORMAT-RECORD THRU 3010-EXIT 4 TIMES.           EL108
00516                                                                      CL**4
00517      EXEC CICS ASKTIME END-EXEC.                                     CL**4
00518                                                                   EL108
00519      EXEC CICS REWRITE                                            EL108
00520          FROM (PROGRAM-OPTIONS-SELECTED)                          EL108
00521          DATASET ('ELPGMS')                                       EL108
00522      END-EXEC.                                                    EL108
00523                                                                   EL108
00524      GO TO 2040-EXIT.                                             EL108
00525                                                                   EL108
00526  2010-PGMS-NOT-FOUND.                                             EL108
00527      EXEC CICS GETMAIN                                            EL108
00528          SET     (ADDRESS OF PROGRAM-OPTIONS-SELECTED)               CL*11
00529          LENGTH  (PGMS-LENGTH)                                    EL108
00530          INITIMG (LIT-SPACE)                                      EL108
00531      END-EXEC.                                                    EL108
00532                                                                   EL108
00533      MOVE 'PS'          TO PS-RECORD-ID.                          EL108
00534      MOVE FILE-PGMS-KEY TO PS-CONTROL-PRIMARY.                    EL108
00535                                                                      CL**6
00536      MOVE SAVE-BIN-DATE          TO  PS-LAST-MAINT-DT.               CL**6
00537      MOVE EIBTIME                TO  PS-LAST-MAINT-HHMMSS.           CL**6
00538      MOVE PI-PROCESSOR-ID        TO  PS-LAST-MAINT-USER.             CL**6
00539                                                                   EL108
00540      MOVE ZEROS TO COUNT-1.                                       EL108
00541      PERFORM 3000-FORMAT-RECORD THRU 3010-EXIT 4 TIMES.           EL108
00542                                                                   EL108
00543      EXEC CICS WRITE                                              EL108
00544          FROM (PROGRAM-OPTIONS-SELECTED)                          EL108
00545          DATASET ('ELPGMS')                                       EL108
00546          RIDFLD (FILE-PGMS-KEY)                                   EL108
00547      END-EXEC.                                                    EL108
00548                                                                   EL108
00549  2040-EXIT.                                                       EL108
00550      EXIT.                                                        EL108
00551  EJECT                                                            EL108
00552  2100-VERIFY-FREQ.                                                EL108
00553      MOVE FREQ1I TO CHECK-FREQ.                                   EL108
00554      PERFORM 2140-FREQ-CHECK THRU 2150-EXIT.                      EL108
00555                                                                   EL108
00556      IF SCREEN-ERROR                                              EL108
00557          MOVE AL-UABON TO FREQ1A                                  EL108
00558          MOVE -1       TO FREQ1L                                  EL108
00559          MOVE SPACE    TO ERROR-SWITCH                            EL108
00560      ELSE                                                         EL108
00561          MOVE AL-UANON TO FREQ1A                                  EL108
00562          PERFORM 2120-MOVE-FREQ THRU 2130-EXIT.                   EL108
00563                                                                   EL108
00564      MOVE FREQ2I TO CHECK-FREQ.                                   EL108
00565      PERFORM 2140-FREQ-CHECK THRU 2150-EXIT.                      EL108
00566                                                                   EL108
00567      IF SCREEN-ERROR                                              EL108
00568          MOVE AL-UABON TO FREQ2A                                  EL108
00569          MOVE -1       TO FREQ2L                                  EL108
00570          MOVE SPACE    TO ERROR-SWITCH                            EL108
00571      ELSE                                                         EL108
00572          MOVE AL-UANON TO FREQ2A                                  EL108
00573          PERFORM 2120-MOVE-FREQ THRU 2130-EXIT.                   EL108
00574                                                                   EL108
00575      MOVE FREQ3I TO CHECK-FREQ.                                   EL108
00576      PERFORM 2140-FREQ-CHECK THRU 2150-EXIT.                      EL108
00577                                                                   EL108
00578      IF SCREEN-ERROR                                              EL108
00579          MOVE AL-UABON TO FREQ3A                                  EL108
00580          MOVE -1       TO FREQ3L                                  EL108
00581          MOVE SPACE    TO ERROR-SWITCH                            EL108
00582      ELSE                                                         EL108
00583          MOVE AL-UANON TO FREQ3A                                  EL108
00584          PERFORM 2120-MOVE-FREQ THRU 2130-EXIT.                   EL108
00585                                                                   EL108
00586      MOVE FREQ4I TO CHECK-FREQ.                                   EL108
00587      PERFORM 2140-FREQ-CHECK THRU 2150-EXIT                       EL108
00588                                                                   EL108
00589      IF SCREEN-ERROR                                              EL108
00590          MOVE SPACE    TO ERROR-SWITCH                            EL108
00591          MOVE -1       TO FREQ4L                                  EL108
00592          MOVE AL-UABON TO FREQ4A                                  EL108
00593      ELSE                                                         EL108
00594          MOVE AL-UANON TO FREQ4A                                  EL108
00595          PERFORM 2120-MOVE-FREQ THRU 2130-EXIT.                   EL108
00596                                                                   EL108
00597  2110-EXIT.                                                       EL108
00598      EXIT.                                                        EL108
00599                                                                   EL108
00600  2120-MOVE-FREQ.                                                  EL108
00601      ADD 1 TO COUNT-1.                                            EL108
00602      MOVE CHECK-FREQ TO FREQ-CODE (COUNT-1).                      EL108
00603                                                                   EL108
00604  2130-EXIT.                                                       EL108
00605      EXIT.                                                        EL108
00606                                                                   EL108
00607  2140-FREQ-CHECK.                                                 EL108
00608      IF CHECK-FREQ = SPACES OR = LOW-VALUES                       EL108
00609         OR OPTION-ALWAYS-SET                                      EL108
00610 *       OR SET-ON-IF-EDIT                                            CL**3
00611 *       OR SET-ON-IF-NOT-EDIT                                        CL**3
00612 *       OR SET-ON-IF-UPDATE                                          CL**3
00613 *       OR SET-ON-IF-NOT-UPDATE                                      CL**3
00614         OR SET-ON-IF-YEAR-END                                     EL108
00615         OR SET-ON-IF-NOT-YEAR-END                                 EL108
00616         OR SET-ON-IF-MO-3-6-9-12                                  EL108
00617         OR SET-ON-IF-NOT-QTR-MO                                   EL108
00618         OR SET-ON-IF-JAN                                          EL108
00619         OR SET-ON-IF-FEB                                          EL108
00620         OR SET-ON-IF-MARCH                                        EL108
00621         OR SET-ON-IF-APRIL                                        EL108
00622         OR SET-ON-IF-MAY                                          EL108
00623         OR SET-ON-IF-JUNE                                         EL108
00624         OR SET-ON-IF-JULY                                         EL108
00625         OR SET-ON-IF-AUG                                          EL108
00626         OR SET-ON-IF-SEPT                                         EL108
00627         OR SET-ON-IF-OCT                                          EL108
00628         OR SET-ON-IF-NOV                                          EL108
00629         OR SET-ON-IF-DEC                                          EL108
00630          GO TO 2150-EXIT.                                         EL108
00631                                                                   EL108
00632      MOVE 'X' TO ERROR-SWITCH.                                    EL108
00633      MOVE ER-0163 TO EMI-ERROR.                                   EL108
00634      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL108
00635                                                                   EL108
00636  2150-EXIT.                                                       EL108
00637      EXIT.                                                        EL108
00638  EJECT                                                            EL108
00639  2200-VERIFY-PRINT.                                               EL108
00640      MOVE PRT1I TO CHECK-PRINT.                                   EL108
00641      PERFORM 2240-PRT-CHECK THRU 2250-EXIT.                       EL108
00642                                                                   EL108
00643      IF SCREEN-ERROR                                              EL108
00644          MOVE SPACE    TO ERROR-SWITCH                            EL108
00645          MOVE -1       TO PRT1L                                   EL108
00646          MOVE AL-UABON TO PRT1A                                   EL108
00647      ELSE                                                         EL108
00648          MOVE AL-UANON TO PRT1A                                   EL108
00649          PERFORM 2220-MOVE-PRT THRU 2230-EXIT.                    EL108
00650                                                                   EL108
00651      MOVE PRT2I TO CHECK-PRINT.                                   EL108
00652      PERFORM 2240-PRT-CHECK THRU 2250-EXIT.                       EL108
00653                                                                   EL108
00654      IF SCREEN-ERROR                                              EL108
00655          MOVE SPACE    TO ERROR-SWITCH                            EL108
00656          MOVE -1       TO PRT2L                                   EL108
00657          MOVE AL-UABON TO PRT2A                                   EL108
00658      ELSE                                                         EL108
00659          MOVE AL-UANON TO PRT2A                                   EL108
00660          PERFORM 2220-MOVE-PRT THRU 2230-EXIT.                    EL108
00661                                                                   EL108
00662      MOVE PRT3I TO CHECK-PRINT.                                   EL108
00663      PERFORM 2240-PRT-CHECK THRU 2250-EXIT.                       EL108
00664                                                                   EL108
00665      IF SCREEN-ERROR                                              EL108
00666          MOVE AL-UABON TO PRT3A                                   EL108
00667          MOVE -1       TO PRT3L                                   EL108
00668          MOVE SPACE    TO ERROR-SWITCH                            EL108
00669      ELSE                                                         EL108
00670          MOVE AL-UANON TO PRT3A                                   EL108
00671          PERFORM 2220-MOVE-PRT THRU 2230-EXIT.                    EL108
00672                                                                   EL108
00673      MOVE PRT4I TO CHECK-PRINT.                                   EL108
00674      PERFORM 2240-PRT-CHECK THRU 2250-EXIT.                       EL108
00675                                                                   EL108
00676      IF SCREEN-ERROR                                              EL108
00677          MOVE AL-UABON TO PRT4A                                   EL108
00678          MOVE -1       TO PRT4L                                   EL108
00679          MOVE SPACE    TO ERROR-SWITCH                            EL108
00680      ELSE                                                         EL108
00681          MOVE AL-UANON TO PRT4A                                   EL108
00682          PERFORM 2220-MOVE-PRT THRU 2230-EXIT.                    EL108
00683                                                                   EL108
00684  2210-EXIT.                                                       EL108
00685      EXIT.                                                        EL108
00686                                                                   EL108
00687  2220-MOVE-PRT.                                                   EL108
00688      ADD 1 TO COUNT-1.                                            EL108
00689      MOVE CHECK-PRINT TO PRINT-OPT (COUNT-1).                     EL108
00690                                                                   EL108
00691  2230-EXIT.                                                       EL108
00692      EXIT.                                                        EL108
00693                                                                   EL108
00694  2240-PRT-CHECK.                                                  EL108
00695      IF CHECK-PRINT = SPACES OR LOW-VALUES                        EL108
00696         OR PRINT-TO-HARDCOPY                                      EL108
00697         OR PRINT-TO-FICHE-TAPE                                    EL108
00698         OR PRINT-TO-BOTH-MEDIA                                    EL108
00699         OR SAVE-REPORT-ONLINE-NO-PRINT                            EL108
00700         OR SAVE-REPORT-ONLINE-AND-PRINT                           EL108
00701          GO TO 2250-EXIT.                                         EL108
00702                                                                   EL108
00703      MOVE 'X'     TO ERROR-SWITCH.                                EL108
00704      MOVE ER-0164 TO EMI-ERROR.                                   EL108
00705      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL108
00706                                                                   EL108
00707  2250-EXIT.                                                       EL108
00708      EXIT.                                                        EL108
00709  EJECT                                                            EL108
00710                                                                   EL108
00711  2300-VERIFY-FORMAT.                                              EL108
00712      MOVE FMT1I TO OPTION-CODE.                                   EL108
00713      PERFORM 2340-FMT-CHECK THRU 2360-EXIT.                       EL108
00714                                                                   EL108
00715      IF SCREEN-ERROR                                              EL108
00716          MOVE AL-UABON TO FMT1A                                   EL108
00717          MOVE -1       TO FMT1L                                   EL108
00718          MOVE SPACE    TO ERROR-SWITCH                            EL108
00719      ELSE                                                         EL108
00720          MOVE AL-UANON TO FMT1A                                   EL108
00721          PERFORM 2320-MOVE-FMT THRU 2330-EXIT.                    EL108
00722                                                                   EL108
00723                                                                   EL108
00724      MOVE FMT2I TO OPTION-CODE.                                   EL108
00725      PERFORM 2340-FMT-CHECK THRU 2360-EXIT.                       EL108
00726                                                                   EL108
00727      IF SCREEN-ERROR                                              EL108
00728          MOVE AL-UABON TO FMT2A                                   EL108
00729          MOVE -1       TO FMT2L                                   EL108
00730          MOVE SPACE    TO ERROR-SWITCH                            EL108
00731      ELSE                                                         EL108
00732          MOVE AL-UANON TO FMT2A                                   EL108
00733          PERFORM 2320-MOVE-FMT THRU 2330-EXIT.                    EL108
00734                                                                   EL108
00735                                                                   EL108
00736      MOVE FMT3I TO OPTION-CODE.                                   EL108
00737      PERFORM 2340-FMT-CHECK THRU 2360-EXIT.                       EL108
00738                                                                   EL108
00739      IF SCREEN-ERROR                                              EL108
00740          MOVE AL-UABON TO FMT3A                                   EL108
00741          MOVE -1       TO FMT3L                                   EL108
00742          MOVE SPACE    TO ERROR-SWITCH                            EL108
00743      ELSE                                                         EL108
00744          MOVE AL-UANON TO FMT3A                                   EL108
00745          PERFORM 2320-MOVE-FMT THRU 2330-EXIT.                    EL108
00746                                                                   EL108
00747      MOVE FMT4I TO OPTION-CODE.                                   EL108
00748      PERFORM 2340-FMT-CHECK THRU 2360-EXIT.                       EL108
00749                                                                   EL108
00750      IF SCREEN-ERROR                                              EL108
00751          MOVE AL-UABON TO FMT4A                                   EL108
00752          MOVE -1       TO FMT4L                                   EL108
00753          MOVE SPACE    TO ERROR-SWITCH                            EL108
00754      ELSE                                                         EL108
00755          MOVE AL-UANON TO FMT4A                                   EL108
00756          PERFORM 2320-MOVE-FMT THRU 2330-EXIT.                    EL108
00757                                                                   EL108
00758  2310-EXIT.                                                       EL108
00759      EXIT.                                                        EL108
00760                                                                   EL108
00761  2320-MOVE-FMT.                                                   EL108
00762      ADD 1 TO COUNT-1.                                            EL108
00763      MOVE OPTION-CODE TO FORMAT-OPT (COUNT-1).                    EL108
00764                                                                   EL108
00765  2330-EXIT.                                                       EL108
00766      EXIT.                                                        EL108
00767                                                                   EL108
00768  2340-FMT-CHECK.                                                  EL108
00769      IF OPTION-CODE = SPACE OR LOW-VALUES                         EL108
00770          GO TO 2360-EXIT.                                         EL108
00771                                                                   EL108
00772      MOVE PGRMI TO PROGRAM-NUMBER.                                EL108
00773      MOVE 'F'   TO OPTION-TYPE.                                   EL108
00774                                                                   EL108
00775      EXEC CICS HANDLE CONDITION                                   EL108
00776          NOTFND (2350-FMT-NOT-FOUND)                              EL108
00777      END-EXEC.                                                    EL108
00778                                                                   EL108
00779      EXEC CICS READ                                               EL108
00780          SET     (ADDRESS OF PROGRAM-OPTIONS-AVAILABLE)              CL*11
00781          DATASET ('ELPGMO')                                       EL108
00782          RIDFLD  (FILE-PGMO-KEY)                                  EL108
00783      END-EXEC.                                                    EL108
00784                                                                   EL108
00785      GO TO 2360-EXIT.                                             EL108
00786                                                                   EL108
00787  2350-FMT-NOT-FOUND.                                              EL108
00788      MOVE 'X'     TO ERROR-SWITCH.                                EL108
00789      MOVE ER-0165 TO EMI-ERROR.                                   EL108
00790      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL108
00791                                                                   EL108
00792  2360-EXIT.                                                       EL108
00793      EXIT.                                                        EL108
00794  EJECT                                                            EL108
00795  2400-VERIFY-PROCESS.                                             EL108
00796      MOVE PROC1I TO OPTION-CODE.                                  EL108
00797      PERFORM 2440-PROC-CHECK THRU 2460-EXIT.                      EL108
00798                                                                   EL108
00799      IF SCREEN-ERROR                                              EL108
00800          MOVE AL-UABON TO PROC1A                                  EL108
00801          MOVE -1       TO PROC1L                                  EL108
00802          MOVE SPACE    TO ERROR-SWITCH                            EL108
00803      ELSE                                                         EL108
00804          MOVE AL-UANON TO PROC1A                                  EL108
00805          PERFORM 2420-MOVE-PROC THRU 2430-EXIT.                   EL108
00806                                                                   EL108
00807      MOVE PROC2I TO OPTION-CODE.                                  EL108
00808      PERFORM 2440-PROC-CHECK THRU 2460-EXIT.                      EL108
00809                                                                   EL108
00810      IF SCREEN-ERROR                                              EL108
00811          MOVE AL-UABON TO PROC2A                                  EL108
00812          MOVE -1       TO PROC2L                                  EL108
00813          MOVE SPACE    TO ERROR-SWITCH                            EL108
00814      ELSE                                                         EL108
00815          MOVE AL-UANON TO PROC2A                                  EL108
00816          PERFORM 2420-MOVE-PROC THRU 2430-EXIT.                   EL108
00817                                                                   EL108
00818      MOVE PROC3I TO OPTION-CODE.                                  EL108
00819      PERFORM 2440-PROC-CHECK THRU 2460-EXIT.                      EL108
00820                                                                   EL108
00821      IF SCREEN-ERROR                                              EL108
00822          MOVE AL-UABON TO PROC3A                                  EL108
00823          MOVE -1       TO PROC3L                                  EL108
00824          MOVE SPACE    TO ERROR-SWITCH                            EL108
00825      ELSE                                                         EL108
00826          MOVE AL-UANON TO PROC3A                                  EL108
00827          PERFORM 2420-MOVE-PROC THRU 2430-EXIT.                   EL108
00828                                                                   EL108
00829      MOVE PROC4I TO OPTION-CODE.                                  EL108
00830      PERFORM 2440-PROC-CHECK THRU 2460-EXIT.                      EL108
00831                                                                   EL108
00832      IF SCREEN-ERROR                                              EL108
00833          MOVE AL-UABON TO PROC4A                                  EL108
00834          MOVE -1       TO PROC4L                                  EL108
00835          MOVE SPACE    TO ERROR-SWITCH                            EL108
00836      ELSE                                                         EL108
00837          MOVE AL-UANON TO PROC4A                                  EL108
00838          PERFORM 2420-MOVE-PROC THRU 2430-EXIT.                   EL108
00839                                                                   EL108
00840  2410-EXIT.                                                       EL108
00841      EXIT.                                                        EL108
00842                                                                   EL108
00843  2420-MOVE-PROC.                                                  EL108
00844      ADD 1 TO COUNT-1.                                            EL108
00845      MOVE OPTION-CODE TO PROCESS-OPT (COUNT-1).                   EL108
00846                                                                   EL108
00847  2430-EXIT.                                                       EL108
00848      EXIT.                                                        EL108
00849                                                                   EL108
00850  2440-PROC-CHECK.                                                 EL108
00851      IF OPTION-CODE = SPACE OR LOW-VALUES OR  'X'                 EL108
00852          GO TO 2460-EXIT.                                         EL108
00853                                                                   EL108
00854      MOVE PGRMI TO PROGRAM-NUMBER.                                EL108
00855      MOVE 'P'   TO OPTION-TYPE.                                   EL108
00856                                                                   EL108
00857      EXEC CICS HANDLE CONDITION                                   EL108
00858          NOTFND (2450-PROC-NOT-FOUND)                             EL108
00859      END-EXEC.                                                    EL108
00860                                                                   EL108
00861      EXEC CICS READ                                               EL108
00862          SET     (ADDRESS OF PROGRAM-OPTIONS-AVAILABLE)              CL*11
00863          DATASET ('ELPGMO')                                       EL108
00864          RIDFLD  (FILE-PGMO-KEY)                                  EL108
00865      END-EXEC.                                                    EL108
00866                                                                   EL108
00867      GO TO 2460-EXIT.                                             EL108
00868                                                                   EL108
00869  2450-PROC-NOT-FOUND.                                             EL108
00870      MOVE 'X'     TO ERROR-SWITCH.                                EL108
00871      MOVE ER-0166 TO EMI-ERROR.                                   EL108
00872      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL108
00873                                                                   EL108
00874  2460-EXIT.                                                       EL108
00875      EXIT.                                                        EL108
00876  EJECT                                                            EL108
00877  2500-VERIFY-TOTAL.                                               EL108
00878      MOVE TOT1I TO OPTION-CODE.                                   EL108
00879      PERFORM 2540-TOTAL-CHECK THRU 2560-EXIT.                     EL108
00880                                                                   EL108
00881      IF SCREEN-ERROR                                              EL108
00882          MOVE AL-UABON TO TOT1A                                   EL108
00883          MOVE -1       TO TOT1L                                   EL108
00884          MOVE SPACE    TO ERROR-SWITCH                            EL108
00885      ELSE                                                         EL108
00886          MOVE AL-UANON TO TOT1A                                   EL108
00887          PERFORM 2520-MOVE-TOTAL THRU 2530-EXIT.                  EL108
00888                                                                   EL108
00889      MOVE TOT2I TO OPTION-CODE.                                   EL108
00890      PERFORM 2540-TOTAL-CHECK THRU 2560-EXIT.                     EL108
00891                                                                   EL108
00892      IF SCREEN-ERROR                                              EL108
00893          MOVE AL-UABON TO TOT2A                                   EL108
00894          MOVE -1       TO TOT2L                                   EL108
00895          MOVE SPACE    TO ERROR-SWITCH                            EL108
00896      ELSE                                                         EL108
00897          MOVE AL-UANON TO TOT2A                                   EL108
00898          PERFORM 2520-MOVE-TOTAL THRU 2530-EXIT.                  EL108
00899                                                                   EL108
00900      MOVE TOT3I TO OPTION-CODE.                                   EL108
00901      PERFORM 2540-TOTAL-CHECK THRU 2560-EXIT.                     EL108
00902                                                                   EL108
00903      IF SCREEN-ERROR                                              EL108
00904          MOVE AL-UABON TO TOT3A                                   EL108
00905          MOVE -1       TO TOT3L                                   EL108
00906          MOVE SPACE    TO ERROR-SWITCH                            EL108
00907      ELSE                                                         EL108
00908          MOVE AL-UANON TO TOT3A                                   EL108
00909          PERFORM 2520-MOVE-TOTAL THRU 2530-EXIT.                  EL108
00910                                                                   EL108
00911      MOVE TOT4I TO OPTION-CODE.                                   EL108
00912      PERFORM 2540-TOTAL-CHECK THRU 2560-EXIT.                     EL108
00913                                                                   EL108
00914      IF SCREEN-ERROR                                              EL108
00915          MOVE AL-UABON TO TOT4A                                   EL108
00916          MOVE -1       TO TOT4L                                   EL108
00917          MOVE SPACE    TO ERROR-SWITCH                            EL108
00918      ELSE                                                         EL108
00919          MOVE AL-UANON TO TOT4A                                   EL108
00920          PERFORM 2520-MOVE-TOTAL THRU 2530-EXIT.                  EL108
00921                                                                   EL108
00922  2510-EXIT.                                                       EL108
00923      EXIT.                                                        EL108
00924                                                                   EL108
00925  2520-MOVE-TOTAL.                                                 EL108
00926      ADD 1 TO COUNT-1.                                            EL108
00927      MOVE OPTION-CODE TO TOTAL-OPT (COUNT-1).                     EL108
00928                                                                   EL108
00929  2530-EXIT.                                                       EL108
00930      EXIT.                                                        EL108
00931                                                                   EL108
00932  2540-TOTAL-CHECK.                                                EL108
00933      IF OPTION-CODE = SPACE OR LOW-VALUES                         EL108
00934          GO TO 2560-EXIT.                                         EL108
00935                                                                   EL108
00936      MOVE PGRMI TO PROGRAM-NUMBER.                                EL108
00937      MOVE 'T'   TO OPTION-TYPE.                                   EL108
00938                                                                   EL108
00939      EXEC CICS HANDLE CONDITION                                   EL108
00940          NOTFND (2550-TOT-NOT-FOUND)                              EL108
00941      END-EXEC.                                                    EL108
00942                                                                   EL108
00943      EXEC CICS READ                                               EL108
00944          SET     (ADDRESS OF PROGRAM-OPTIONS-AVAILABLE)              CL*11
00945          DATASET ('ELPGMO')                                       EL108
00946          RIDFLD  (FILE-PGMO-KEY)                                  EL108
00947      END-EXEC.                                                    EL108
00948                                                                   EL108
00949      GO TO 2560-EXIT.                                             EL108
00950                                                                   EL108
00951  2550-TOT-NOT-FOUND.                                              EL108
00952      MOVE 'X'     TO ERROR-SWITCH.                                EL108
00953      MOVE ER-0167 TO EMI-ERROR.                                   EL108
00954      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL108
00955                                                                   EL108
00956  2560-EXIT.                                                       EL108
00957      EXIT.                                                        EL108
00958  EJECT                                                            EL108
00959  2600-CHECK-REQUIRE.                                              EL108
00960      IF (FREQ1I    = SPACES OR LOW-VALUES)                        EL108
00961         IF (PRT1I  = SPACES OR LOW-VALUES) AND                    EL108
00962            (FMT1I  = SPACES OR LOW-VALUES) AND                    EL108
00963            (PROC1I = SPACES OR LOW-VALUES) AND                    EL108
00964            (TOT1I  = SPACES OR LOW-VALUES)                        EL108
00965            NEXT SENTENCE                                          EL108
00966           ELSE                                                    EL108
00967            MOVE AL-UABON TO FREQ1A                                EL108
00968            MOVE -1       TO FREQ1L                                EL108
00969            MOVE 'X'      TO ERROR-SWITCH                          EL108
00970        ELSE                                                       EL108
00971         IF (PRT1I  = SPACES OR = LOW-VALUES)                      EL108
00972            MOVE AL-UABON TO PRT1A                                 EL108
00973            MOVE -1       TO PRT1L                                 EL108
00974            MOVE 'X'      TO ERROR-SWITCH.                         EL108
00975                                                                   EL108
00976      IF (FREQ2I    = SPACES OR LOW-VALUES)                        EL108
00977         IF (PRT2I  = SPACES OR LOW-VALUES) AND                    EL108
00978            (FMT2I  = SPACES OR LOW-VALUES) AND                    EL108
00979            (PROC2I = SPACES OR LOW-VALUES) AND                    EL108
00980            (TOT2I  = SPACES OR LOW-VALUES)                        EL108
00981            NEXT SENTENCE                                          EL108
00982           ELSE                                                    EL108
00983            MOVE AL-UABON TO FREQ2A                                EL108
00984            MOVE -1       TO FREQ2L                                EL108
00985            MOVE 'X'      TO ERROR-SWITCH                          EL108
00986        ELSE                                                       EL108
00987         IF (PRT2I  = SPACES OR LOW-VALUES)                        EL108
00988            MOVE AL-UABON TO PRT2A                                 EL108
00989            MOVE -1       TO PRT2L                                 EL108
00990            MOVE 'X'      TO ERROR-SWITCH.                         EL108
00991                                                                   EL108
00992      IF (FREQ3I    = SPACES OR LOW-VALUES)                        EL108
00993         IF (PRT3I  = SPACES OR LOW-VALUES) AND                    EL108
00994            (FMT3I  = SPACES OR LOW-VALUES) AND                    EL108
00995            (PROC3I = SPACES OR LOW-VALUES) AND                    EL108
00996            (TOT3I  = SPACES OR LOW-VALUES)                        EL108
00997            NEXT SENTENCE                                          EL108
00998           ELSE                                                    EL108
00999            MOVE AL-UABON TO FREQ3A                                EL108
01000            MOVE -1       TO FREQ3L                                EL108
01001            MOVE 'X'      TO ERROR-SWITCH                          EL108
01002        ELSE                                                       EL108
01003         IF (PRT3I  = SPACES OR LOW-VALUES)                        EL108
01004            MOVE AL-UABON TO PRT3A                                 EL108
01005            MOVE -1       TO PRT3L                                 EL108
01006            MOVE 'X'      TO ERROR-SWITCH.                         EL108
01007                                                                   EL108
01008      IF (FREQ4I    = SPACES OR LOW-VALUES)                        EL108
01009         IF (PRT4I  = SPACES OR LOW-VALUES) AND                    EL108
01010            (FMT4I  = SPACES OR LOW-VALUES) AND                    EL108
01011            (PROC4I = SPACES OR LOW-VALUES) AND                    EL108
01012            (TOT4I  = SPACES OR LOW-VALUES)                        EL108
01013            NEXT SENTENCE                                          EL108
01014           ELSE                                                    EL108
01015            MOVE AL-UABON TO FREQ4A                                EL108
01016            MOVE -1       TO FREQ4L                                EL108
01017            MOVE 'X'      TO ERROR-SWITCH                          EL108
01018        ELSE                                                       EL108
01019         IF (PRT4I  = SPACES OR LOW-VALUES)                        EL108
01020            MOVE AL-UABON TO PRT4A                                 EL108
01021            MOVE -1       TO PRT4L                                 EL108
01022            MOVE 'X'      TO ERROR-SWITCH.                         EL108
01023                                                                   EL108
01024      IF SCREEN-ERROR                                              EL108
01025         MOVE ER-0486          TO EMI-ERROR                        EL108
01026          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL108
01027                                                                   EL108
01028  2650-EXIT.                                                       EL108
01029      EXIT.                                                        EL108
01030  EJECT                                                            EL108
01031  3000-FORMAT-RECORD.                                              EL108
01032      ADD 1 TO COUNT-1.                                            EL108
01033      MOVE HOLD-PROGRAM-OPT (COUNT-1)                              EL108
01034              TO PS-PROGRAM-OPTIONS (COUNT-1).                     EL108
01035                                                                   EL108
01036  3010-EXIT.                                                       EL108
01037      EXIT.                                                        EL108
01038  EJECT                                                            EL108
01039  3100-SHOW-OPTIONS.                                               EL108
01040      MOVE PI-COMPANY-CD TO COMPANY-CD.                            EL108
01041                                                                   EL108
01042      IF PGRML = ZERO                                              EL108
01043          MOVE LOW-VALUES  TO  PROGRAM-NO                          EL108
01044      ELSE                                                         EL108
01045          MOVE PGRMI       TO  PROGRAM-NO.                         EL108
01046                                                                   EL108
01047      IF EIBAID      = DFHPF7   AND                                EL108
01048         PROGRAM-NO  = LOW-VALUES                                  EL108
01049          GO TO 8700-ENDFILE.                                      EL108
01050                                                                   EL108
01051      EXEC CICS HANDLE CONDITION                                   EL108
01052          NOTFND    (8700-ENDFILE)                                 EL108
01053          ENDFILE   (8700-ENDFILE)                                 EL108
01054      END-EXEC.                                                    EL108
01055                                                                   EL108
01056      EXEC CICS STARTBR                                            EL108
01057          DATASET    ('ELPGMS')                                    EL108
01058          RIDFLD     (FILE-PGMS-KEY)                               EL108
01059      END-EXEC.                                                    EL108
01060                                                                   EL108
01061      MOVE '1'  TO  BROWSE-STARTED-SW.                             EL108
01062                                                                   EL108
01063  3110-READ-NEXT.                                                  EL108
01064      IF EIBAID  = DFHPF7                                          EL108
01065          EXEC CICS READPREV                                       EL108
01066              DATASET    ('ELPGMS')                                EL108
01067              SET        (ADDRESS OF PROGRAM-OPTIONS-SELECTED)        CL*11
01068              RIDFLD     (FILE-PGMS-KEY)                           EL108
01069          END-EXEC                                                 EL108
01070      ELSE                                                         EL108
01071          EXEC CICS READNEXT                                       EL108
01072              DATASET    ('ELPGMS')                                EL108
01073              SET        (ADDRESS OF PROGRAM-OPTIONS-SELECTED)        CL*11
01074              RIDFLD     (FILE-PGMS-KEY)                           EL108
01075          END-EXEC.                                                EL108
01076                                                                   EL108
01077      IF PS-COMPANY-CD  NOT = PI-COMPANY-CD                        EL108
01078          GO TO 8700-ENDFILE.                                      EL108
01079                                                                   EL108
01080      IF PS-PROGRAM-NUMBER  = PI-PROGRAM-NO                        EL108
01081          GO TO 3110-READ-NEXT.                                    EL108
01082                                                                   EL108
01083      MOVE ZEROS   TO  COUNT-1.                                    EL108
01084                                                                   EL108
01085      PERFORM 4100-FORMAT-WS THRU 4110-EXIT 4 TIMES.               EL108
01086                                                                   EL108
01087      MOVE ZEROS   TO  COUNT-1.                                    EL108
01088                                                                   EL108
01089      PERFORM 4200-FORMAT-SCREEN  THRU  4210-EXIT.                 EL108
01090                                                                   EL108
01091      MOVE -1                 TO  MAINTL.                          EL108
01092      MOVE SPACE              TO  MAINTO.                          EL108
01093      MOVE AL-UANON           TO  PGRMA.                           EL108
01094      MOVE PS-PROGRAM-NUMBER  TO  PGRMO  PI-PROGRAM-NO.            EL108
01095                                                                   EL108
01096      IF BROWSE-STARTED                                            EL108
01097          MOVE ' '  TO  BROWSE-STARTED-SW                          EL108
01098          PERFORM 3200-END-BROWSE  THRU  3299-EXIT.                EL108
01099                                                                   EL108
01100      GO TO 8110-SEND-DATA.                                        EL108
01101  EJECT                                                            EL108
01102  3200-END-BROWSE.                                                 EL108
01103      EXEC CICS ENDBR                                              EL108
01104          DATASET    ('ELPGMS')                                    EL108
01105      END-EXEC.                                                    EL108
01106                                                                   EL108
01107  3299-EXIT.                                                       EL108
01108      EXIT.                                                        EL108
01109  EJECT                                                            EL108
01110  4000-SHOW-OPTIONS.                                               EL108
01111      MOVE PI-COMPANY-CD TO COMPANY-CD.                            EL108
01112      MOVE PGRMI         TO PROGRAM-NO.                            EL108
01113                                                                   EL108
01114      EXEC CICS HANDLE CONDITION                                   EL108
01115          NOTFND (4010-FORMAT-OPTIONS)                             EL108
01116      END-EXEC.                                                    EL108
01117                                                                   EL108
01118      EXEC CICS READ                                               EL108
01119          SET     (ADDRESS OF PROGRAM-OPTIONS-SELECTED)               CL*11
01120          DATASET ('ELPGMS')                                       EL108
01121          RIDFLD  (FILE-PGMS-KEY)                                  EL108
01122      END-EXEC.                                                    EL108
01123                                                                   EL108
01124      MOVE ZEROS TO COUNT-1.                                       EL108
01125      PERFORM 4100-FORMAT-WS THRU 4110-EXIT 4 TIMES.               EL108
01126                                                                   EL108
01127  4010-FORMAT-OPTIONS.                                             EL108
01128      IF PROGRAM-OPT-HOLD = SPACES                                 EL108
01129          MOVE ER-0269 TO EMI-ERROR                                EL108
01130          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*10
01131          MOVE -1                 TO  MAINTL                          CL*10
01132          GO TO 8110-SEND-DATA.                                       CL*10
01133                                                                   EL108
01134      MOVE ZEROS TO COUNT-1.                                       EL108
01135      PERFORM 4200-FORMAT-SCREEN THRU 4210-EXIT.                   EL108
01136                                                                   EL108
01137  4020-EXIT.                                                       EL108
01138      EXIT.                                                        EL108
01139  EJECT                                                            EL108
01140  4100-FORMAT-WS.                                                  EL108
01141      ADD 1 TO COUNT-1.                                            EL108
01142      MOVE PS-PROGRAM-OPTIONS (COUNT-1)                            EL108
01143              TO HOLD-PROGRAM-OPT (COUNT-1).                       EL108
01144                                                                   EL108
01145  4110-EXIT.                                                       EL108
01146      EXIT.                                                        EL108
01147  EJECT                                                            EL108
01148  4200-FORMAT-SCREEN.                                              EL108
01149      MOVE ' '                    TO  DC-OPTION-CODE.                 CL**6
01150                                                                      CL**6
01151      IF PS-LAST-MAINT-DT  IS EQUAL TO  SPACES                        CL**6
01152          MOVE LOW-VALUES         TO  PS-LAST-MAINT-DT.               CL**6
01153                                                                      CL**6
01154      MOVE PS-LAST-MAINT-DT       TO  DC-BIN-DATE-1.                  CL**6
01155                                                                      CL**6
01156      PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.                CL**6
01157                                                                      CL**6
01158      IF NO-CONVERSION-ERROR                                          CL**6
01159          MOVE DC-GREG-DATE-1-EDIT                                    CL**6
01160                                  TO  LSTDTEO                         CL**6
01161      ELSE                                                            CL**6
01162          MOVE '00/00/00'         TO  LSTDTEO.                        CL**6
01163                                                                      CL**6
01164      IF PS-LAST-MAINT-HHMMSS  IS NUMERIC                             CL**6
01165          MOVE PS-LAST-MAINT-HHMMSS                                   CL**6
01166                                  TO  TIME-IN                         CL**6
01167          MOVE TIME-OUT           TO  LSTTIMEO                        CL**6
01168      ELSE                                                            CL**6
01169          MOVE ZEROS              TO  TIME-IN                         CL**6
01170          MOVE TIME-OUT           TO  LSTTIMEO.                       CL**6
01171                                                                      CL**6
01172      MOVE PS-LAST-MAINT-USER     TO  LSTUSRO.                        CL**6
01173                                                                      CL**6
01174      ADD 1 TO COUNT-1.                                            EL108
01175      MOVE FREQ-CODE (COUNT-1)   TO FREQ1O.                        EL108
01176      MOVE PRINT-OPT (COUNT-1)   TO PRT1O.                         EL108
01177      MOVE FORMAT-OPT (COUNT-1)  TO FMT1O.                         EL108
01178      MOVE PROCESS-OPT (COUNT-1) TO PROC1O.                        EL108
01179      MOVE TOTAL-OPT (COUNT-1)   TO TOT1O.                         EL108
01180      MOVE AL-UANON              TO FREQ1A                         EL108
01181                                    PRT1A                          EL108
01182                                    FMT1A                          EL108
01183                                    PROC1A                         EL108
01184                                    TOT1A.                         EL108
01185      ADD 1 TO COUNT-1.                                            EL108
01186      MOVE FREQ-CODE (COUNT-1)   TO FREQ2O.                        EL108
01187      MOVE PRINT-OPT (COUNT-1)   TO PRT2O.                         EL108
01188      MOVE FORMAT-OPT (COUNT-1)  TO FMT2O.                         EL108
01189      MOVE PROCESS-OPT (COUNT-1) TO PROC2O.                        EL108
01190      MOVE TOTAL-OPT (COUNT-1)   TO TOT2O.                         EL108
01191      MOVE AL-UANON              TO FREQ2A                         EL108
01192                                    PRT2A                          EL108
01193                                    FMT2A                          EL108
01194                                    PROC2A                         EL108
01195                                    TOT2A.                         EL108
01196      ADD 1 TO COUNT-1.                                            EL108
01197      MOVE FREQ-CODE (COUNT-1)   TO FREQ3O.                        EL108
01198      MOVE PRINT-OPT (COUNT-1)   TO PRT3O.                         EL108
01199      MOVE FORMAT-OPT (COUNT-1)  TO FMT3O.                         EL108
01200      MOVE PROCESS-OPT (COUNT-1) TO PROC3O.                        EL108
01201      MOVE TOTAL-OPT (COUNT-1)   TO TOT3O.                         EL108
01202      MOVE AL-UANON              TO FREQ3A                         EL108
01203                                    PRT3A                          EL108
01204                                    FMT3A                          EL108
01205                                    PROC3A                         EL108
01206                                    TOT3A.                         EL108
01207      ADD 1 TO COUNT-1.                                            EL108
01208      MOVE FREQ-CODE (COUNT-1)   TO FREQ4O.                        EL108
01209      MOVE PRINT-OPT (COUNT-1)   TO PRT4O.                         EL108
01210      MOVE FORMAT-OPT (COUNT-1)  TO FMT4O.                         EL108
01211      MOVE PROCESS-OPT (COUNT-1) TO PROC4O.                        EL108
01212      MOVE TOTAL-OPT (COUNT-1)   TO TOT4O.                         EL108
01213      MOVE AL-UANON              TO FREQ4A                         EL108
01214                                    PRT4A                          EL108
01215                                    FMT4A                          EL108
01216                                    PROC4A                         EL108
01217                                    TOT4A.                         EL108
01218                                                                   EL108
01219  4210-EXIT.                                                       EL108
01220      EXIT.                                                        EL108
01221  EJECT                                                            EL108
01222  5000-SHOW-FREQ.                                                  EL108
01223      MOVE SPACE TO ERROR-SWITCH.                                  EL108
01224      MOVE ZEROS TO COUNT-1.                                       EL108
01225      PERFORM 5010-FREQ-OPTION THRU 5020-EXIT 4 TIMES.                CL**3
01226                                                                   EL108
01227      PERFORM 5130-SPACE-SCREEN THRU 5140-EXIT 5 TIMES.               CL**5
01228                                                                   EL108
01229      MOVE 'FREQUENCY' TO VARDESCO.                                EL108
01230      MOVE -1          TO MAINTL.                                  EL108
01231      GO TO 8110-SEND-DATA.                                        EL108
01232                                                                   EL108
01233  5010-FREQ-OPTION.                                                EL108
01234      ADD 1 TO COUNT-1.                                            EL108
01235      MOVE OPTIONS-FREQ (COUNT-1) TO OPTO (COUNT-1).               EL108
01236                                                                   EL108
01237  5020-EXIT.                                                       EL108
01238      EXIT.                                                        EL108
01239  EJECT                                                            EL108
01240  5100-SHOW-PRINT.                                                 EL108
01241      MOVE SPACES TO ERROR-SWITCH.                                 EL108
01242      MOVE ZEROS  TO COUNT-1.                                      EL108
01243      PERFORM 5110-PRINT-OPTION THRU 5120-EXIT 5 TIMES.            EL108
01244                                                                   EL108
01245      PERFORM 5130-SPACE-SCREEN THRU 5140-EXIT 4 TIMES.            EL108
01246                                                                   EL108
01247      MOVE 'PRINT' TO VARDESCO.                                    EL108
01248      MOVE -1      TO MAINTL.                                      EL108
01249      GO TO 8110-SEND-DATA.                                        EL108
01250                                                                   EL108
01251  5110-PRINT-OPTION.                                               EL108
01252      ADD 1 TO COUNT-1.                                            EL108
01253      MOVE OPTIONS-PRINT (COUNT-1) TO OPTO (COUNT-1).              EL108
01254                                                                   EL108
01255  5120-EXIT.                                                       EL108
01256      EXIT.                                                        EL108
01257                                                                   EL108
01258  5130-SPACE-SCREEN.                                               EL108
01259      ADD 1 TO COUNT-1.                                            EL108
01260      MOVE SPACES TO OPTO (COUNT-1).                               EL108
01261                                                                   EL108
01262  5140-EXIT.                                                       EL108
01263      EXIT.                                                        EL108
01264  EJECT                                                            EL108
01265  5200-SHOW-TOTAL.                                                 EL108
01266      MOVE SPACE TO ERROR-SWITCH.                                  EL108
01267      EXEC CICS HANDLE CONDITION                                   EL108
01268          NOTFND (5210-TOTAL-NOTFND)                               EL108
01269          ENDFILE (6030-END-OF-FILE)                               EL108
01270      END-EXEC.                                                    EL108
01271                                                                   EL108
01272      MOVE PGRMI  TO PROGRAM-NUMBER HOLD-PROGRAM.                  EL108
01273      MOVE 'T'    TO OPTION-TYPE HOLD-TYPE.                        EL108
01274      MOVE SPACES TO OPTION-CODE TYPE-SWITCH PROGRAM-OPT-DESC.     EL108
01275                                                                   EL108
01276      PERFORM 6000-START-BROWSE THRU 6010-EXIT.                    EL108
01277                                                                   EL108
01278      PERFORM 6020-READ-FILE THRU 6040-EXIT                        EL108
01279          UNTIL END-OF-TYPE.                                       EL108
01280                                                                   EL108
01281      PERFORM 6050-END-BROWSE THRU 6080-EXIT.                      EL108
01282                                                                   EL108
01283  5210-TOTAL-NOTFND.                                               EL108
01284      IF PROGRAM-OPT-DESC = SPACES                                 EL108
01285          MOVE ER-0267 TO EMI-ERROR                                EL108
01286          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL108
01287                                                                   EL108
01288      MOVE ZEROS TO COUNT-1.                                       EL108
01289      PERFORM 5500-FORMAT-SCREEN THRU 5510-EXIT 8 TIMES.           EL108
01290                                                                   EL108
01291      PERFORM 5130-SPACE-SCREEN THRU 5140-EXIT.                    EL108
01292                                                                   EL108
01293      MOVE 'TOTAL' TO VARDESCO.                                    EL108
01294      MOVE -1      TO MAINTL.                                      EL108
01295      GO TO 8110-SEND-DATA.                                        EL108
01296  EJECT                                                            EL108
01297  5300-SHOW-PROCESS.                                               EL108
01298      MOVE SPACE TO ERROR-SWITCH.                                  EL108
01299      EXEC CICS HANDLE CONDITION                                   EL108
01300          NOTFND  (5310-PROCESS-NOTFND)                            EL108
01301          ENDFILE (6030-END-OF-FILE)                               EL108
01302      END-EXEC.                                                    EL108
01303                                                                   EL108
01304      MOVE PGRMI  TO PROGRAM-NUMBER HOLD-PROGRAM.                  EL108
01305      MOVE 'P'    TO OPTION-TYPE HOLD-TYPE.                        EL108
01306      MOVE SPACES TO OPTION-CODE TYPE-SWITCH PROGRAM-OPT-DESC.     EL108
01307                                                                   EL108
01308      PERFORM 6000-START-BROWSE THRU 6010-EXIT.                    EL108
01309                                                                   EL108
01310      PERFORM 6020-READ-FILE THRU 6040-EXIT                        EL108
01311          UNTIL END-OF-TYPE.                                       EL108
01312                                                                   EL108
01313      PERFORM 6050-END-BROWSE THRU 6080-EXIT.                      EL108
01314                                                                   EL108
01315  5310-PROCESS-NOTFND.                                             EL108
01316      IF PROGRAM-OPT-DESC = SPACES                                 EL108
01317          MOVE ER-0267 TO EMI-ERROR                                EL108
01318          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL108
01319                                                                   EL108
01320      MOVE ZEROS TO COUNT-1.                                       EL108
01321      PERFORM 5500-FORMAT-SCREEN THRU 5510-EXIT 8 TIMES.           EL108
01322                                                                   EL108
01323      ADD 1                       TO COUNT-1.                      EL108
01324      MOVE DO-NOT-RUN             TO OPTO (COUNT-1).               EL108
01325      MOVE 'PROCESS' TO VARDESCO.                                  EL108
01326      MOVE -1        TO MAINTL.                                    EL108
01327      GO TO 8110-SEND-DATA.                                        EL108
01328  EJECT                                                            EL108
01329  5400-SHOW-FORMAT.                                                EL108
01330      MOVE SPACE TO ERROR-SWITCH.                                  EL108
01331      EXEC CICS HANDLE CONDITION                                   EL108
01332          NOTFND  (5410-FORMAT-NOTFND)                             EL108
01333          ENDFILE (6030-END-OF-FILE)                               EL108
01334      END-EXEC.                                                    EL108
01335                                                                   EL108
01336      MOVE PGRMI  TO PROGRAM-NUMBER HOLD-PROGRAM.                  EL108
01337      MOVE 'F'    TO OPTION-TYPE HOLD-TYPE.                        EL108
01338      MOVE SPACES TO OPTION-CODE TYPE-SWITCH PROGRAM-OPT-DESC.     EL108
01339                                                                   EL108
01340      PERFORM 6000-START-BROWSE THRU 6010-EXIT.                    EL108
01341                                                                   EL108
01342      PERFORM 6020-READ-FILE THRU 6040-EXIT                        EL108
01343          UNTIL END-OF-TYPE.                                       EL108
01344                                                                   EL108
01345      PERFORM 6050-END-BROWSE THRU 6080-EXIT.                      EL108
01346                                                                   EL108
01347  5410-FORMAT-NOTFND.                                              EL108
01348      IF PROGRAM-OPT-DESC = SPACES                                 EL108
01349          MOVE ER-0267 TO EMI-ERROR                                EL108
01350          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL108
01351                                                                   EL108
01352      MOVE ZEROS TO COUNT-1.                                       EL108
01353      PERFORM 5500-FORMAT-SCREEN THRU 5510-EXIT 8 TIMES.           EL108
01354                                                                   EL108
01355      PERFORM 5130-SPACE-SCREEN THRU 5140-EXIT.                    EL108
01356                                                                   EL108
01357      MOVE 'FORMAT' TO VARDESCO.                                   EL108
01358      MOVE -1       TO MAINTL.                                     EL108
01359      GO TO 8110-SEND-DATA.                                        EL108
01360  EJECT                                                            EL108
01361  5500-FORMAT-SCREEN.                                              EL108
01362      ADD 1 TO COUNT-1.                                            EL108
01363      MOVE COUNT-1 TO OPT-NO (COUNT-1).                            EL108
01364      MOVE '='     TO CONST-FILL (COUNT-1).                        EL108
01365      MOVE DESC-PROGRAM-OPT (COUNT-1) TO OPTO (COUNT-1).           EL108
01366                                                                   EL108
01367  5510-EXIT.                                                       EL108
01368      EXIT.                                                        EL108
01369  EJECT                                                            EL108
01370  6000-START-BROWSE.                                               EL108
01371      EXEC CICS STARTBR                                            EL108
01372          DATASET ('ELPGMO')                                       EL108
01373          RIDFLD (FILE-PGMO-KEY)                                   EL108
01374      END-EXEC.                                                    EL108
01375                                                                   EL108
01376  6010-EXIT.                                                       EL108
01377      EXIT.                                                        EL108
01378                                                                   EL108
01379  6020-READ-FILE.                                                  EL108
01380      EXEC CICS READNEXT                                           EL108
01381          SET     (ADDRESS OF PROGRAM-OPTIONS-AVAILABLE)              CL*11
01382          DATASET ('ELPGMO')                                       EL108
01383          RIDFLD  (FILE-PGMO-KEY)                                  EL108
01384      END-EXEC.                                                    EL108
01385                                                                   EL108
01386      IF PO-PROGRAM-NUMBER NOT = HOLD-PROGRAM OR                   EL108
01387         PO-OPTION-TYPE NOT = HOLD-TYPE                            EL108
01388          GO TO 6030-END-OF-FILE.                                  EL108
01389                                                                   EL108
01390      MOVE PO-PGM-OPTION-CD      TO COUNT-1.                       EL108
01391      MOVE PO-OPTION-DESCRIPTION TO PGMO-DESC (COUNT-1).           EL108
01392      GO TO 6040-EXIT.                                             EL108
01393                                                                   EL108
01394  6030-END-OF-FILE.                                                EL108
01395      MOVE 'X' TO TYPE-SWITCH.                                     EL108
01396                                                                   EL108
01397  6040-EXIT.                                                       EL108
01398      EXIT.                                                        EL108
01399                                                                   EL108
01400  6050-END-BROWSE.                                                 EL108
01401      EXEC CICS ENDBR                                              EL108
01402          DATASET ('ELPGMO')                                       EL108
01403      END-EXEC.                                                    EL108
01404                                                                   EL108
01405  6080-EXIT.                                                       EL108
01406      EXIT.                                                        EL108
01407  EJECT                                                            EL108
01408  8100-SEND-INITIAL-MAP.                                           EL108
01409      PERFORM 8120-FORMAT-TIME-DATE THRU 8130-EXIT.                EL108
01410                                                                   EL108
01411      EXEC CICS SEND                                               EL108
01412          MAP    ('EL108A')                                        EL108
01413          MAPSET ('EL108S')                                        EL108
01414          ERASE                                                    EL108
01415          FREEKB                                                   EL108
01416          CURSOR                                                   EL108
01417      END-EXEC.                                                    EL108
01418                                                                   EL108
01419      GO TO 9000-RETURN-TRANS.                                     EL108
01420                                                                   EL108
01421  8110-SEND-DATA.                                                  EL108
01422      PERFORM 8120-FORMAT-TIME-DATE THRU 8130-EXIT.                EL108
01423                                                                   EL108
01424      EXEC CICS SEND                                               EL108
01425          MAP    ('EL108A')                                        EL108
01426          MAPSET ('EL108S')                                        EL108
01427          DATAONLY                                                 EL108
01428          FREEKB                                                   EL108
01429          CURSOR                                                   EL108
01430      END-EXEC.                                                    EL108
01431                                                                   EL108
01432      GO TO 9000-RETURN-TRANS.                                     EL108
01433                                                                   EL108
01434  8120-FORMAT-TIME-DATE.                                           EL108
01435      MOVE SAVE-DATE            TO DATEO.                          EL108
01436      MOVE EIBTIME              TO TIME-IN.                        EL108
01437      MOVE TIME-OUT             TO TIMEO.                          EL108
01438      MOVE LIT-MAP              TO PI-CURRENT-SCREEN-NO.           EL108
01439      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.               EL108
01440      MOVE EMI-MESSAGE-AREA (1) TO MSGO.                           EL108
01441                                                                   EL108
01442  8130-EXIT.                                                       EL108
01443      EXIT.                                                        EL108
01444                                                                   EL108
01445  8200-RETURN-PRIOR.                                               EL108
01446      MOVE PI-RETURN-TO-PROGRAM TO CALL-PGM.                       EL108
01447      GO TO 9200-XCTL.                                             EL108
01448                                                                   EL108
01449  8300-GET-HELP.                                                   EL108
01450      MOVE XCTL-EL010 TO CALL-PGM.                                 EL108
01451      GO TO 9200-XCTL.                                             EL108
01452                                                                   EL108
01453  8400-RETURN-MASTER.                                              EL108
01454                                                                      CL**2
01455      IF  CREDIT-SESSION                                              CL**4
01456          MOVE XCTL-EL626         TO CALL-PGM                         CL**4
01457                                                                      CL**2
01458      ELSE                                                            CL**4
01459          IF  CLAIM-SESSION                                           CL**4
01460              MOVE XCTL-EL126     TO CALL-PGM                         CL**4
01461                                                                      CL**2
01462          ELSE                                                        CL**4
01463              IF  MORTGAGE-SESSION                                    CL**4
01464                  MOVE XCTL-EM626 TO CALL-PGM                         CL**4
01465                                                                      CL**4
01466              ELSE                                                    CL**4
01467                  IF  GENERAL-LEDGER-SESSION                          CL**4
01468                      MOVE XCTL-GL800                                 CL**4
01469                                  TO CALL-PGM.                        CL**4
01470                                                                      CL**4
01471      GO TO 9200-XCTL.                                             EL108
01472                                                                   EL108
01473  8700-ENDFILE.                                                    EL108
01474      IF BROWSE-STARTED                                            EL108
01475          MOVE ' '  TO  BROWSE-STARTED-SW                          EL108
01476          PERFORM 3200-END-BROWSE  THRU  3299-EXIT.                EL108
01477                                                                   EL108
01478      IF EIBAID  = DFHPF7                                          EL108
01479          MOVE ER-0067  TO  EMI-ERROR                              EL108
01480      ELSE                                                         EL108
01481          MOVE ER-0066  TO  EMI-ERROR.                             EL108
01482                                                                   EL108
01483      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL108
01484                                                                   EL108
01485      MOVE -1      TO  MAINTL.                                     EL108
01486      MOVE SPACE   TO  MAINTO.                                     EL108
01487                                                                   EL108
01488      GO TO 8110-SEND-DATA.                                        EL108
01489                                                                   EL108
01490  8800-UNAUTHORIZED-ACCESS.                                        EL108
01491      MOVE UNACCESS-MSG TO LOGOFF-MSG.                             EL108
01492      GO TO 8990-SEND-TEXT.                                        EL108
01493                                                                   EL108
01494  8810-PF23-ENTERED.                                               EL108
01495      MOVE EIBAID     TO PI-ENTRY-CD-1.                            EL108
01496      MOVE XCTL-EL005 TO CALL-PGM.                                 EL108
01497      GO TO 9200-XCTL.                                             EL108
01498                                                                   EL108
01499  8820-XCTL-ERROR.                                                 EL108
01500      EXEC CICS HANDLE CONDITION                                   EL108
01501          PGMIDERR (8990-SEND-TEXT)                                EL108
01502      END-EXEC.                                                    EL108
01503                                                                   EL108
01504      MOVE SPACE      TO PI-ENTRY-CD-1.                            EL108
01505      MOVE CALL-PGM   TO PI-CALLING-PROGRAM.                       EL108
01506      MOVE XCTL-EL005 TO CALL-PGM                                  EL108
01507                         LOGOFF-PGM.                               EL108
01508      MOVE PGMIDERR-MSG TO LOGOFF-FILL.                            EL108
01509      GO TO 9200-XCTL.                                             EL108
01510                                                                   EL108
01511  8990-SEND-TEXT.                                                  EL108
01512      EXEC CICS SEND TEXT                                          EL108
01513          FROM   (LOGOFF-TEXT)                                     EL108
01514          LENGTH (LOGOFF-LENGTH)                                   EL108
01515          ERASE                                                    EL108
01516          FREEKB                                                   EL108
01517      END-EXEC.                                                    EL108
01518                                                                   EL108
01519      GO TO 9100-RETURN-CICS.                                      EL108
01520  EJECT                                                            EL108
01521  9000-RETURN-TRANS.                                               EL108
01522      EXEC CICS RETURN                                             EL108
01523          TRANSID (TRANS-ID)                                       EL108
01524          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL108
01525          LENGTH (PI-COMM-LENGTH)                                  EL108
01526      END-EXEC.                                                    EL108
01527                                                                   EL108
01528      GOBACK.                                                      EL108
01529                                                                   EL108
01530  9100-RETURN-CICS.                                                EL108
01531      EXEC CICS RETURN                                             EL108
01532      END-EXEC.                                                    EL108
01533      GOBACK.                                                      EL108
01534                                                                   EL108
01535  9200-XCTL.                                                       EL108
01536      EXEC CICS XCTL                                               EL108
01537          PROGRAM (CALL-PGM)                                       EL108
01538          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL108
01539          LENGTH (PI-COMM-LENGTH)                                  EL108
01540      END-EXEC.                                                    EL108
01541                                                                   EL108
01542  9700-LINK-DATE-CONVERT.                                          EL108
01543      EXEC CICS LINK                                               EL108
01544          PROGRAM    ('ELDATCV')                                   EL108
01545          COMMAREA   (DATE-CONVERSION-DATA)                        EL108
01546          LENGTH     (DC-COMM-LENGTH)                              EL108
01547          END-EXEC.                                                EL108
01548                                                                   EL108
01549  9700-EXIT.                                                       EL108
01550      EXIT.                                                        EL108
01551                                                                   EL108
01552  9900-ERROR-FORMAT.                                               EL108
01553      IF NOT EMI-ERRORS-COMPLETE                                   EL108
01554          EXEC CICS LINK                                           EL108
01555              PROGRAM ('EL001')                                    EL108
01556              COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)             EL108
01557              LENGTH (EMI-COMM-LENGTH)                             EL108
01558          END-EXEC.                                                EL108
01559                                                                   EL108
01560  9900-EXIT.                                                       EL108
01561      EXIT.                                                        EL108
01562                                                                   EL108
01563  9990-ABEND.                                                      EL108
01564      MOVE DFHEIBLK               TO EMI-LINE1                     EL108
01565      MOVE -1                     TO MAINTL.                       EL108
01566      EXEC CICS LINK                                               EL108
01567          PROGRAM   ('EL004')                                      EL108
01568          COMMAREA  (EMI-LINE1)                                    EL108
01569          LENGTH    (72)                                           EL108
01570          END-EXEC.                                                EL108
01571                                                                   EL108
01572      GO TO 8110-SEND-DATA.                                        EL108
01573                                                                   EL108
01574  9995-SECURITY-VIOLATION.                                         EL108
01575                              COPY ELCSCTP.                        EL108
01576                                                                   EL108
01577  9995-EXIT.                                                       EL108
01578      EXIT.                                                        EL108
01579                                                                   EL108
