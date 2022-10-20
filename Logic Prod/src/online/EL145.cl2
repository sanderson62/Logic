00001  IDENTIFICATION DIVISION.                                         07/27/95
00002                                                                   EL145
00003  PROGRAM-ID.                 EL145 .                                 LV002
00004 *              PROGRAM CONVERTED BY                                  CL**2
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**2
00006 *              CONVERSION DATE 04/19/95 13:07:20.                    CL**2
00007 *                            VMOD=2.002                              CL**2
00008 *                                                                 EL145
00008 *                                                                 EL145
00009 *AUTHOR.    LOGIC, INC.                                              CL**2
00010 *           DALLAS, TEXAS.                                           CL**2
00011                                                                   EL145
00012 *DATE-COMPILED.                                                      CL**2
00013                                                                   EL145
00014 *SECURITY.   *****************************************************   CL**2
00015 *            *                                                   *   CL**2
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**2
00017 *            *                                                   *   CL**2
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**2
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**2
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**2
00021 *            *                                                   *   CL**2
00022 *            *****************************************************   CL**2
00023                                                                   EL145
00024 *REMARKS.                                                         EL145
00025 *        THIS PROGRAM CONTROLS THE MAINTENANCE TO THE AUTOMATIC   EL145
00026 *    ACTIVITY RECORD ON THE CONTROL FILE.                         EL145
00027 *                                                                 EL145
00028 *    SCREENS     - EL145S - AUTOMATIC ACTIVITY MAINTENANCE        EL145
00029 *    ENTERED BY  - EL101  - PROCESSING MENU                       EL145
00030 *                                                                 EL145
00031 *    EXIT TO     - EL101  - RESULT OF CLEAR                       EL145
00032 *                                                                 EL145
00033 *    INPUT FILES - ELCNTL - CONTROL FILE                          EL145
00034 *                  ELLETR - LETTER FILE                           EL145
00035 *                                                                 EL145
00036 *    OUTPUT FILES - ELCNTL                                        EL145
00037 *                                                                 EL145
00038 *    COMMAREA    - PASSED.                                        EL145
00039                                                                   EL145
00040      EJECT                                                        EL145
00041  ENVIRONMENT DIVISION.                                            EL145
00042                                                                   EL145
00043  DATA DIVISION.                                                   EL145
00044                                                                   EL145
00045  WORKING-STORAGE SECTION.                                         EL145
00046                                                                   EL145
00047  77  FILLER  PIC X(32)  VALUE '********************************'. EL145
00048  77  FILLER  PIC X(32)  VALUE '*   EL145  WORKING STORAGE     *'. EL145
00049  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.002 *********'.    CL**2
00050                                                                   EL145
00051      COPY ELCSCTM.                                                   CL**2
00052                                                                   EL145
00053      COPY ELCSCRTY.                                                  CL**2
00054                                                                   EL145
00055  01  WS-DATE-AREA.                                                EL145
00056      12  SAVE-DATE               PIC X(08)       VALUE SPACES.    EL145
00057      12  SAVE-BIN-DATE           PIC X(02)       VALUE SPACES.    EL145
00058                                                                   EL145
00059  01  FILLER                      COMP-3.                          EL145
00060                                                                   EL145
00061      12  TIME-IN                 PIC S9(7)       VALUE ZERO.      EL145
00062      12  TIME-OUT                REDEFINES                        EL145
00063          TIME-IN                 PIC S9(3)V9(4).                  EL145
00064                                                                   EL145
00065  01  FILLER.                                                      EL145
00066      12  GETMAIN-SPACE           PIC X(01)       VALUE SPACES.    EL145
00067      12  WS-ELCNTL-LENGTH        PIC S9(04)      VALUE +750 COMP. EL145
00068      12  ELCNTL-DSID             PIC X(08)       VALUE 'ELCNTL'.  EL145
00069      12  ELTEXT-DSID             PIC X(08)       VALUE 'ELLETR'.  EL145
00070                                                                   EL145
00071      12  LINK-ELDATCV            PIC X(08)       VALUE 'ELDATCV'. EL145
00072      12  LINK-001                PIC X(08)       VALUE 'EL001'.   EL145
00073      12  LINK-004                PIC X(08)       VALUE 'EL004'.   EL145
00074                                                                   EL145
00075      12  XCTL-005                PIC X(08)       VALUE 'EL005'.   EL145
00076      12  XCTL-010                PIC X(08)       VALUE 'EL010'.   EL145
00077      12  XCTL-126                PIC X(08)       VALUE 'EL126'.   EL145
00078                                                                   EL145
00079      12  WS-MAPSET-NAME          PIC X(08)       VALUE 'EL145S'.  EL145
00080      12  WS-MAP-NAME             PIC X(08)       VALUE 'EL145A'.  EL145
00081      12  FILLER                  REDEFINES                        EL145
00082          WS-MAP-NAME.                                             EL145
00083          16  FILLER              PIC X(02).                       EL145
00084          16  WS-MAP-NUMBER       PIC X(04).                       EL145
00085          16  FILLER              PIC X(02).                       EL145
00086                                                                   EL145
00087      12  THIS-PGM                PIC X(08)       VALUE 'EL145'.   EL145
00088      12  WS-TRANS-ID             PIC X(04)       VALUE 'E026'.    EL145
00089      12  SC-ITEM                 PIC S9(04)      VALUE +1 COMP.   EL145
00090      12  SUB                     PIC S9(02)      VALUE +0.        EL145
00091                                                                   EL145
00092      12  WS-LETTER-SW            PIC X(01)       VALUE ' '.       EL145
00093      12  WS-UPDATE-SW            PIC X(01)       VALUE ' '.       EL145
00094                                                                   EL145
00095      12  DEEDIT-FIELD            PIC X(03).                       EL145
00096      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD  PIC S9(03).      EL145
00097                                                                   EL145
00098  01  ACCESS-KEYS.                                                 EL145
00099                                                                   EL145
00100      12  ELCNTL-KEY.                                              EL145
00101          16  ELCNTL-COMPANY-ID   PIC X(03).                       EL145
00102          16  ELCNTL-REC-TYPE     PIC X(01).                       EL145
00103          16  ELCNTL-ACCESS       PIC X(04).                       EL145
00104          16  ELCNTL-SEQ-NO       PIC S9(04)      COMP.            EL145
00105                                                                   EL145
00106      12  ELTEXT-KEY.                                              EL145
00107          16  ELTEXT-COMPANY-CD       PIC X(01).                   EL145
00108          16  ELTEXT-LETTER-ACCESS.                                EL145
00109              20  ELTEXT-LETTER-ID    PIC X(04).                   EL145
00110              20  ELTEXT-FILLER       PIC X(08).                   EL145
00111          16  ELTEXT-SEQ-NO           PIC S9(04)  COMP.            EL145
00112                                                                   EL145
00113      EJECT                                                        EL145
00114  01  ERROR-MESSAGES.                                              EL145
00115      12  ER-0000                 PIC X(04)       VALUE '0000'.    EL145
00116      12  ER-0004                 PIC X(04)       VALUE '0004'.    EL145
00117      12  ER-0005                 PIC X(04)       VALUE '0005'.    EL145
00118      12  ER-0008                 PIC X(04)       VALUE '0008'.    EL145
00119      12  ER-0023                 PIC X(04)       VALUE '0023'.    EL145
00120      12  ER-0029                 PIC X(04)       VALUE '0029'.    EL145
00121      12  ER-0042                 PIC X(04)       VALUE '0042'.    EL145
00122      12  ER-0070                 PIC X(04)       VALUE '0070'.    EL145
00123      12  ER-0284                 PIC X(04)       VALUE '0284'.    EL145
00124      12  ER-0312                 PIC X(04)       VALUE '0312'.    EL145
00125      12  ER-0313                 PIC X(04)       VALUE '0313'.    EL145
00126      12  ER-0676                 PIC X(04)       VALUE '0676'.    EL145
00127      12  ER-0677                 PIC X(04)       VALUE '0677'.    EL145
00128      12  ER-0678                 PIC X(04)       VALUE '0678'.    EL145
00129      12  ER-0679                 PIC X(04)       VALUE '0679'.    EL145
00130      12  ER-3516                 PIC X(04)       VALUE '3516'.    EL145
00131      12  ER-3517                 PIC X(04)       VALUE '3517'.    EL145
00132      12  ER-3518                 PIC X(04)       VALUE '3518'.    EL145
00133      12  ER-3519                 PIC X(04)       VALUE '3519'.    EL145
00134      12  ER-3520                 PIC X(04)       VALUE '3520'.    EL145
00135      12  ER-3521                 PIC X(04)       VALUE '3521'.    EL145
00136      12  ER-3522                 PIC X(04)       VALUE '3522'.    EL145
00137      12  ER-3523                 PIC X(04)       VALUE '3523'.    EL145
00138      12  ER-3524                 PIC X(04)       VALUE '3524'.    EL145
00139      12  ER-3525                 PIC X(04)       VALUE '3525'.    EL145
00140      12  ER-3544                 PIC X(04)       VALUE '3544'.    EL145
00141                                                                   EL145
00142      EJECT                                                        EL145
00143      COPY ELCINTF.                                                   CL**2
00144                                                                   EL145
00145      12  PI-REDEF REDEFINES PI-PROGRAM-WORK-AREA.                 EL145
00146          16  PI-EDIT-ITEMS OCCURS 8 TIMES.                        EL145
00147              20  PI-USER-ACTIVITY-SW    PIC X.                       CL**2
00148              20  PI-USER-DESCRIPTION    PIC X(20).                   CL**2
00149          16  FILLER                     PIC X(472).                  CL**2
00150      EJECT                                                        EL145
00151                                                                   EL145
00152      COPY EL145S.                                                    CL**2
00153  01  FILLER REDEFINES EL145AI.                                       CL**2
00154      12  FILLER                  PIC X(35).                       EL145
00155      12  EL145A-SCREEN.                                           EL145
00156          16  EL145A-SYSTEM-DEFINED-ACTIVITY OCCURS 09 TIMES.      EL145
00157              20  EL145A-SYS-ACT-LENGTH   PIC S9(04)  COMP.        EL145
00158              20  EL145A-SYS-ACT-ATTRB    PIC X(01).               EL145
00159              20  EL145A-SYS-ACTIVITY-SW  PIC X(01).               EL145
00160                                                                   EL145
00161              20  EL145A-SYS-LET-LENGTH   PIC S9(04)  COMP.        EL145
00162              20  EL145A-SYS-LET-ATTRB    PIC X(01).               EL145
00163              20  EL145A-SYS-LETTER-ID    PIC X(04).               EL145
00164                                                                   EL145
00165              20  EL145A-SYS-RSND-LENGTH  PIC S9(04)  COMP.        EL145
00166              20  EL145A-SYS-RSND-ATTRB   PIC X(01).               EL145
00167              20  EL145A-SYS-RESEND-DAYS  PIC X(03).               EL145
00168                                                                   EL145
00169              20  EL145A-SYS-FOLL-LENGTH  PIC S9(04)  COMP.        EL145
00170              20  EL145A-SYS-FOLL-ATTRB   PIC X(01).               EL145
00171              20  EL145A-SYS-FOLL-DAYS    PIC X(03).               EL145
00172                                                                   EL145
00173              20  EL145A-SYS-RES-LENGTH   PIC S9(04)  COMP.        EL145
00174              20  EL145A-SYS-RES-ATTRB    PIC X(01).               EL145
00175              20  EL145A-SYS-RESET-SW     PIC X(01).               EL145
00176                                                                   EL145
00177              20  EL145A-SYS-REP-LENGTH   PIC S9(04)  COMP.        EL145
00178              20  EL145A-SYS-REP-ATTRB    PIC X(01).               EL145
00179              20  EL145A-SYS-REPORT-DAYS  PIC X(03).               EL145
00180                                                                   EL145
00181              20  EL145A-SYS-AFT-LENGTH   PIC S9(04)  COMP.        EL145
00182              20  EL145A-SYS-AFT-ATTRB    PIC X(01).               EL145
00183              20  EL145A-SYS-AFTER-SW     PIC X(01).               EL145
00184                                                                   EL145
00185          16  EL145A-USER-DEFINED-ACTIVITY OCCURS 08 TIMES.        EL145
00186              20  EL145A-USER-ACT-LENGTH  PIC S9(04)  COMP.        EL145
00187              20  EL145A-USER-ACT-ATTRB   PIC X(01).               EL145
00188              20  EL145A-USER-ACTIVITY-SW PIC X(01).               EL145
00189                                                                   EL145
00190              20  EL145A-USER-LET-LENGTH  PIC S9(04)  COMP.        EL145
00191              20  EL145A-USER-LET-ATTRB   PIC X(01).               EL145
00192              20  EL145A-USER-LETTER-ID   PIC X(04).               EL145
00193                                                                   EL145
00194              20  EL145A-USER-RSND-LENGTH PIC S9(04)  COMP.        EL145
00195              20  EL145A-USER-RSND-ATTRB  PIC X(01).               EL145
00196              20  EL145A-USER-RESEND-DAYS PIC X(03).               EL145
00197                                                                   EL145
00198              20  EL145A-USER-FOLL-LENGTH PIC S9(04)  COMP.        EL145
00199              20  EL145A-USER-FOLL-ATTRB  PIC X(01).               EL145
00200              20  EL145A-USER-FOLL-DAYS   PIC X(03).               EL145
00201                                                                   EL145
00202              20  EL145A-USER-RES-LENGTH  PIC S9(04)  COMP.        EL145
00203              20  EL145A-USER-RES-ATTRB   PIC X(01).               EL145
00204              20  EL145A-USER-RESET-SW    PIC X(01).               EL145
00205                                                                   EL145
00206              20  EL145A-USER-REP-LENGTH  PIC S9(04)  COMP.        EL145
00207              20  EL145A-USER-REP-ATTRB   PIC X(01).               EL145
00208              20  EL145A-USER-REPORT-DAYS PIC X(03).               EL145
00209                                                                   EL145
00210              20  EL145A-USER-AFT-LENGTH  PIC S9(04)  COMP.        EL145
00211              20  EL145A-USER-AFT-ATTRB   PIC X(01).               EL145
00212              20  EL145A-USER-AFTER-SW    PIC X(01).               EL145
00213                                                                   EL145
00214              20  EL145A-USER-DESC-LENGTH PIC S9(04)  COMP.        EL145
00215              20  EL145A-USER-DESC-ATTRB  PIC X(01).               EL145
00216              20  EL145A-USER-DESCRIPTION PIC X(20).               EL145
00217                                                                   EL145
00218      EJECT                                                        EL145
00219      COPY ELCEMIB.                                                   CL**2
00220                                                                      CL**2
00221      EJECT                                                        EL145
00222      COPY ELCDATE.                                                   CL**2
00223                                                                      CL**2
00224      EJECT                                                        EL145
00225      COPY ELCLOGOF.                                                  CL**2
00226                                                                      CL**2
00227      EJECT                                                        EL145
00228      COPY ELCATTR.                                                   CL**2
00229                                                                      CL**2
00230      EJECT                                                        EL145
00231      COPY ELCAID.                                                    CL**2
00232                                                                      CL**2
00233  01  FILLER REDEFINES DFHAID.                                        CL**2
00234                                                                   EL145
00235      12  FILLER                  PIC X(08).                       EL145
00236                                                                   EL145
00237      12  PF-VALUES               PIC X(01)                        EL145
00238          OCCURS 24 TIMES.                                         EL145
00239      EJECT                                                        EL145
00240  LINKAGE SECTION.                                                 EL145
00241                                                                   EL145
00242  01  DFHCOMMAREA                 PIC X(1024).                     EL145
00243                                                                   EL145
00244 *01 DFHBLLDS                     COMP SYNC.                          CL**2
00245 *    12  BLLCBAR                 PIC S9(09).                         CL**2
00246 *    12  ELCNTL-POINTER          PIC S9(09).                         CL**2
00247 *    12  ELTEXT-POINTER          PIC S9(09).                         CL**2
00248                                                                   EL145
00249      EJECT                                                        EL145
00250      COPY ELCCNTL.                                                   CL**2
00251                                                                      CL**2
00252      EJECT                                                           CL**2
00253      COPY ELCTEXT.                                                   CL**2
00254                                                                   EL145
00255      EJECT                                                        EL145
00256  PROCEDURE DIVISION.                                              EL145
00257                                                                   EL145
00258      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL145
00259      MOVE '5'                    TO  DC-OPTION-CODE.              EL145
00260      PERFORM 8500-DATE-CONVERSION.                                EL145
00261      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL145
00262      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL145
00263                                                                   EL145
00264      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL145
00265                                                                   EL145
00266      MOVE 1                      TO  EMI-NUMBER-OF-LINES          EL145
00267                                      EMI-SWITCH2.                 EL145
00268                                                                   EL145
00269      IF EIBCALEN IS EQUAL TO 0                                    EL145
00270          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL145
00271                                                                   EL145
00272 *    NOTE ******************************************************* EL145
00273 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * EL145
00274 *         *  FROM ANOTHER MODULE.                               * EL145
00275 *         *******************************************************.EL145
00276                                                                   EL145
00277      EXEC CICS HANDLE CONDITION                                   EL145
00278          ERROR    (9990-ERROR)                                    EL145
00279          NOTOPEN  (8870-NOTOPEN)                                  EL145
00280          PGMIDERR (9600-PGMIDERR)                                 EL145
00281      END-EXEC.                                                    EL145
00282                                                                   EL145
00283      EJECT                                                        EL145
00284  0010-MAIN-LOGIC.                                                 EL145
00285                                                                   EL145
00286      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL145
00287         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                    EL145
00288            MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6       EL145
00289            MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5       EL145
00290            MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4       EL145
00291            MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3       EL145
00292            MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2       EL145
00293            MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1       EL145
00294            MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM     EL145
00295            MOVE THIS-PGM             TO  PI-CALLING-PROGRAM       EL145
00296         ELSE                                                      EL145
00297            MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM       EL145
00298            MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM     EL145
00299            MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1       EL145
00300            MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2       EL145
00301            MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3       EL145
00302            MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4       EL145
00303            MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5       EL145
00304            MOVE SPACES               TO  PI-SAVED-PROGRAM-6       EL145
00305      ELSE                                                         EL145
00306         GO TO 0020-MAIN-LOGIC.                                    EL145
00307                                                                   EL145
00308  0015-MAIN-LOGIC.                                                 EL145
00309 *    NOTE ******************************************************* EL145
00310 *         *     INITIALIZE THE WORK FIELDS FOR THE PROGRAM      *    CL**2
00311 *         *  INTERFACE BLOCK FOR THIS MODULE.                   * EL145
00312 *         *******************************************************.EL145
00313                                                                   EL145
00314      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.        EL145
00315                                                                   EL145
00316      IF EIBTRNID NOT EQUAL WS-TRANS-ID                            EL145
00317         MOVE LOW-VALUES         TO  EL145AI                       EL145
00318         GO TO 1000-SHOW-ACTIVITY-REC.                             EL145
00319                                                                   EL145
00320      EJECT                                                        EL145
00321  0020-MAIN-LOGIC.                                                 EL145
00322 *    NOTE ******************************************************* EL145
00323 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- * EL145
00324 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    * EL145
00325 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         * EL145
00326 *         *******************************************************.EL145
00327                                                                   EL145
00328      IF EIBAID EQUAL DFHCLEAR                                     EL145
00329          GO TO 9400-CLEAR.                                        EL145
00330                                                                   EL145
00331      IF PI-PROCESSOR-ID EQUAL 'LGXX'                              EL145
00332         NEXT SENTENCE                                             EL145
00333      ELSE                                                         EL145
00334         EXEC CICS READQ TS                                        EL145
00335              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  EL145
00336              INTO    (SECURITY-CONTROL)                           EL145
00337              LENGTH  (SC-COMM-LENGTH)                             EL145
00338              ITEM    (SC-ITEM)                                    EL145
00339         END-EXEC                                                  EL145
00340         MOVE SC-CLAIMS-DISPLAY (25)  TO  PI-DISPLAY-CAP           EL145
00341         MOVE SC-CLAIMS-UPDATE  (25)  TO  PI-MODIFY-CAP            EL145
00342         IF NOT DISPLAY-CAP                                        EL145
00343            MOVE 'READ'               TO  SM-READ                  EL145
00344            PERFORM 9995-SECURITY-VIOLATION                        EL145
00345            MOVE ER-0070              TO  EMI-ERROR                EL145
00346            GO TO 8100-SEND-INITIAL-MAP.                           EL145
00347                                                                   EL145
00348  0200-RECEIVE.                                                    EL145
00349                                                                   EL145
00350      IF EIBAID EQUAL DFHPA1 OR DFHPA2 OR DFHPA3                   EL145
00351          MOVE LOW-VALUES         TO  EL145AI                      EL145
00352          MOVE ER-0008            TO  EMI-ERROR                    EL145
00353          PERFORM 8200-SEND-DATAONLY.                              EL145
00354                                                                   EL145
00355      EXEC CICS RECEIVE                                            EL145
00356          INTO   (EL145AI)                                         EL145
00357          MAPSET (WS-MAPSET-NAME)                                  EL145
00358          MAP    (WS-MAP-NAME)                                     EL145
00359      END-EXEC.                                                    EL145
00360                                                                   EL145
00361      IF ENTERPFL IS EQUAL TO +0                                   EL145
00362          GO TO 0300-CHECK-PFKEYS.                                 EL145
00363                                                                   EL145
00364      IF EIBAID NOT = DFHENTER                                     EL145
00365          MOVE ER-0004            TO  EMI-ERROR                    EL145
00366          GO TO 0320-INPUT-ERROR.                                  EL145
00367                                                                   EL145
00368      IF (ENTERPFI NUMERIC) AND (ENTERPFI GREATER 0 AND LESS 25)   EL145
00369          MOVE PF-VALUES (ENTERPFI)   TO  EIBAID                   EL145
00370      ELSE                                                         EL145
00371          MOVE ER-0029                TO  EMI-ERROR                EL145
00372          GO TO 0320-INPUT-ERROR.                                  EL145
00373                                                                   EL145
00374  0300-CHECK-PFKEYS.                                               EL145
00375                                                                   EL145
00376      IF EIBAID EQUAL DFHPF12                                      EL145
00377         MOVE XCTL-010            TO  THIS-PGM                     EL145
00378         GO TO 9300-XCTL.                                          EL145
00379                                                                   EL145
00380      IF EIBAID EQUAL DFHPF23                                      EL145
00381         GO TO 9000-RETURN-CICS.                                   EL145
00382                                                                   EL145
00383      IF EIBAID EQUAL DFHPF24                                      EL145
00384         MOVE XCTL-126            TO  THIS-PGM                     EL145
00385         GO TO 9300-XCTL.                                          EL145
00386                                                                   EL145
00387      IF EIBAID EQUAL DFHENTER                                     EL145
00388          GO TO 0330-EDIT-DATA.                                    EL145
00389                                                                   EL145
00390      MOVE ER-0029                TO  EMI-ERROR.                   EL145
00391                                                                   EL145
00392  0320-INPUT-ERROR.                                                EL145
00393                                                                   EL145
00394      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL145
00395      MOVE AL-UNBON               TO  ENTERPFA.                    EL145
00396                                                                   EL145
00397      IF ENTERPFL IS EQUAL TO +0                                   EL145
00398          MOVE -1                 TO  MAINTL                       EL145
00399      ELSE                                                         EL145
00400          MOVE -1                 TO  ENTERPFL.                    EL145
00401                                                                   EL145
00402      GO TO 8200-SEND-DATAONLY.                                    EL145
00403                                                                   EL145
00404  0330-EDIT-DATA.                                                  EL145
00405                                                                   EL145
00406      IF MAINTI IS EQUAL TO 'S'                                    EL145
00407          GO TO 1000-SHOW-ACTIVITY-REC.                            EL145
00408                                                                   EL145
00409      IF SYSTEM-MODIFY-CAP                                         EL145
00410          NEXT SENTENCE                                            EL145
00411      ELSE                                                         EL145
00412          IF MAINTI IS EQUAL TO 'A' OR 'C'                         EL145
00413              MOVE 'UPDATE'       TO  SM-READ                      EL145
00414              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT       EL145
00415              MOVE ER-0070        TO  EMI-ERROR                    EL145
00416              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL145
00417              MOVE LOW-VALUES     TO  EL145AO                      EL145
00418              GO TO 8100-SEND-INITIAL-MAP.                         EL145
00419                                                                   EL145
00420      IF MAINTI IS EQUAL TO 'C'                                    EL145
00421          GO TO 2000-CHANGE-ACTIVITY-REC.                          EL145
00422                                                                   EL145
00423      IF MAINTI IS EQUAL TO 'A'                                    EL145
00424          GO TO 3000-ADD-ACTIVITY-REC.                             EL145
00425                                                                   EL145
00426      MOVE ER-0023                TO  EMI-ERROR.                   EL145
00427      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL145
00428      MOVE -1                     TO  MAINTL.                      EL145
00429      MOVE AL-UABON               TO  MAINTA.                      EL145
00430      GO TO 8200-SEND-DATAONLY.                                    EL145
00431                                                                   EL145
00432      EJECT                                                        EL145
00433  1000-SHOW-ACTIVITY-REC.                                          EL145
00434                                                                   EL145
00435      EXEC CICS HANDLE CONDITION                                   EL145
00436          NOTFND   (1000-SHOW-REC-NOT-FND)                         EL145
00437      END-EXEC.                                                    EL145
00438                                                                   EL145
00439      MOVE PI-COMPANY-ID          TO  ELCNTL-COMPANY-ID.           EL145
00440      MOVE 'T'                    TO  ELCNTL-REC-TYPE.             EL145
00441      MOVE SPACES                 TO  ELCNTL-ACCESS.               EL145
00442      MOVE +0                     TO  ELCNTL-SEQ-NO.               EL145
00443                                                                   EL145
00444      EXEC CICS READ                                               EL145
00445          DATASET   (ELCNTL-DSID)                                  EL145
00446          SET       (ADDRESS OF CONTROL-FILE)                         CL**2
00447          RIDFLD    (ELCNTL-KEY)                                   EL145
00448      END-EXEC.                                                    EL145
00449                                                                   EL145
00450      MOVE +0                     TO  SUB.                         EL145
00451      GO TO 5000-BUILD-SYS-DEF-ACTIVITY.                           EL145
00452                                                                   EL145
00453  1000-SHOW-REC-NOT-FND.                                           EL145
00454                                                                   EL145
00455      MOVE -1                     TO  MAINTL.                      EL145
00456      MOVE 'S'                    TO  MAINTO.                      EL145
00457      MOVE AL-UABON               TO  MAINTA.                      EL145
00458      MOVE ER-3516                TO  EMI-ERROR.                      CL**2
00459      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL145
00460      GO TO 8100-SEND-INITIAL-MAP.                                 EL145
00461      EJECT                                                        EL145
00462  2000-CHANGE-ACTIVITY-REC.                                        EL145
00463                                                                   EL145
00464      MOVE +0                     TO  SUB.                         EL145
00465      PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT.                 EL145
00466                                                                   EL145
00467      IF WS-UPDATE-SW IS EQUAL TO 'Y'                              EL145
00468          NEXT SENTENCE                                            EL145
00469      ELSE                                                         EL145
00470          MOVE ER-3544            TO  EMI-ERROR                    EL145
00471          MOVE -1                 TO  MAINTL                       EL145
00472          GO TO 8200-SEND-DATAONLY.                                EL145
00473                                                                   EL145
00474      IF NOT EMI-NO-ERRORS                                         EL145
00475          GO TO 8200-SEND-DATAONLY.                                EL145
00476                                                                   EL145
00477      EXEC CICS HANDLE CONDITION                                   EL145
00478          NOTFND   (2000-NOTFND)                                   EL145
00479      END-EXEC.                                                    EL145
00480                                                                   EL145
00481      MOVE SPACES           TO  ELCNTL-KEY.                        EL145
00482      MOVE PI-COMPANY-ID    TO  ELCNTL-COMPANY-ID.                 EL145
00483      MOVE 'T'              TO  ELCNTL-REC-TYPE.                   EL145
00484      MOVE SPACES           TO  ELCNTL-ACCESS.                     EL145
00485      MOVE +0               TO  ELCNTL-SEQ-NO.                     EL145
00486                                                                   EL145
00487      EXEC CICS READ                                               EL145
00488          DATASET   (ELCNTL-DSID)                                  EL145
00489          SET       (ADDRESS OF CONTROL-FILE)                         CL**2
00490          RIDFLD    (ELCNTL-KEY)                                   EL145
00491          UPDATE                                                   EL145
00492      END-EXEC.                                                    EL145
00493                                                                   EL145
00494      MOVE +0               TO  SUB.                               EL145
00495                                                                   EL145
00496      PERFORM 4000-MOVE-DATA-TO-REC THRU 4000-EXIT.                EL145
00497                                                                   EL145
00498      MOVE SAVE-BIN-DATE    TO  CF-LAST-MAINT-DT.                  EL145
00499      MOVE PI-PROCESSOR-ID  TO  CF-LAST-MAINT-BY.                  EL145
00500      MOVE EIBTIME          TO  CF-LAST-MAINT-HHMMSS.              EL145
00501                                                                   EL145
00502      EXEC CICS REWRITE                                            EL145
00503          DATASET   (ELCNTL-DSID)                                  EL145
00504          FROM      (CONTROL-FILE)                                 EL145
00505      END-EXEC.                                                    EL145
00506                                                                   EL145
00507      MOVE ER-0000          TO  EMI-ERROR.                         EL145
00508      MOVE -1               TO  MAINTL.                            EL145
00509      MOVE AL-UANON         TO  MAINTA.                            EL145
00510      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL145
00511      GO TO 1000-SHOW-ACTIVITY-REC.                                EL145
00512                                                                   EL145
00513  2000-NOTFND.                                                     EL145
00514                                                                   EL145
00515      MOVE ER-3516          TO  EMI-ERROR.                         EL145
00516      MOVE -1               TO  MAINTL.                            EL145
00517      MOVE AL-UABON         TO  MAINTA.                            EL145
00518      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL145
00519      GO TO 8200-SEND-DATAONLY.                                    EL145
00520                                                                   EL145
00521      EJECT                                                        EL145
00522  3000-ADD-ACTIVITY-REC.                                           EL145
00523                                                                   EL145
00524      MOVE +0               TO  SUB.                               EL145
00525      PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT.                 EL145
00526                                                                   EL145
00527      IF WS-UPDATE-SW IS EQUAL TO 'Y'                              EL145
00528          NEXT SENTENCE                                            EL145
00529      ELSE                                                         EL145
00530          MOVE ER-3544      TO  EMI-ERROR                          EL145
00531          MOVE -1           TO  MAINTL                             EL145
00532          GO TO 8200-SEND-DATAONLY.                                EL145
00533                                                                   EL145
00534      IF NOT EMI-NO-ERRORS                                         EL145
00535          GO TO 8200-SEND-DATAONLY.                                EL145
00536                                                                   EL145
00537      EXEC CICS GETMAIN                                            EL145
00538          SET       (ADDRESS OF CONTROL-FILE)                         CL**2
00539          LENGTH    (WS-ELCNTL-LENGTH)                             EL145
00540          INITIMG   (GETMAIN-SPACE)                                EL145
00541      END-EXEC.                                                    EL145
00542                                                                   EL145
00543      MOVE 'CF'             TO  CF-RECORD-ID.                      EL145
00544      MOVE PI-COMPANY-ID    TO  CF-COMPANY-ID.                     EL145
00545      MOVE 'T'              TO  CF-RECORD-TYPE.                    EL145
00546      MOVE SPACES           TO  CF-ACCESS-CD-GENL.                 EL145
00547      MOVE +0               TO  CF-SEQUENCE-NO.                    EL145
00548                                                                   EL145
00549      MOVE +0               TO  SUB.                               EL145
00550      PERFORM 3050-INIT-SYS-USER-FIELDS THRU 3050-EXIT.            EL145
00551                                                                   EL145
00552      MOVE +0               TO  SUB.                               EL145
00553      PERFORM 4000-MOVE-DATA-TO-REC THRU 4000-EXIT.                EL145
00554                                                                   EL145
00555      MOVE SAVE-BIN-DATE    TO  CF-LAST-MAINT-DT.                  EL145
00556      MOVE PI-PROCESSOR-ID  TO  CF-LAST-MAINT-BY.                  EL145
00557      MOVE EIBTIME          TO  CF-LAST-MAINT-HHMMSS.              EL145
00558                                                                   EL145
00559      EXEC CICS HANDLE CONDITION                                   EL145
00560          DUPREC    (3000-DUP-REC)                                 EL145
00561      END-EXEC.                                                    EL145
00562                                                                   EL145
00563      EXEC CICS WRITE                                              EL145
00564          DATASET   (ELCNTL-DSID)                                  EL145
00565          FROM      (CONTROL-FILE)                                 EL145
00566          RIDFLD    (CF-CONTROL-PRIMARY)                           EL145
00567      END-EXEC.                                                    EL145
00568                                                                   EL145
00569      MOVE ER-0000                TO  EMI-ERROR.                   EL145
00570      MOVE -1                     TO  MAINTL.                      EL145
00571      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL145
00572      GO TO 1000-SHOW-ACTIVITY-REC.                                EL145
00573                                                                   EL145
00574  3000-DUP-REC.                                                    EL145
00575                                                                   EL145
00576      MOVE ER-3517                TO  EMI-ERROR.                   EL145
00577      MOVE -1                     TO  MAINTL.                      EL145
00578      MOVE AL-UABON               TO  MAINTA.                      EL145
00579      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL145
00580      GO TO 8200-SEND-DATAONLY.                                    EL145
00581                                                                   EL145
00582      EJECT                                                        EL145
00583  3050-INIT-SYS-USER-FIELDS.                                       EL145
00584                                                                   EL145
00585      ADD +1                      TO  SUB.                         EL145
00586      IF SUB IS GREATER THAN +9                                    EL145
00587          MOVE +0                 TO  SUB                          EL145
00588          GO TO 3050-INIT-USER-FIELDS.                             EL145
00589                                                                   EL145
00590      MOVE 'N'                    TO  CF-SYS-ACTIVE-SW      (SUB)  EL145
00591                                      CF-SYS-RESET-SW       (SUB)  EL145
00592                                      CF-SYS-EACH-DAY-AFTER-SW     EL145
00593                                                            (SUB). EL145
00594      MOVE ZEROS                  TO  CF-SYS-RESEND-DAYS    (SUB)  EL145
00595                                      CF-SYS-FOLLOW-UP-DAYS (SUB)  EL145
00596                                      CF-SYS-REPORT-DAYS    (SUB). EL145
00597                                                                   EL145
00598      GO TO 3050-INIT-SYS-USER-FIELDS.                             EL145
00599                                                                   EL145
00600  3050-INIT-USER-FIELDS.                                           EL145
00601                                                                   EL145
00602      ADD +1                      TO  SUB.                         EL145
00603      IF SUB IS GREATER THAN +8                                    EL145
00604          GO TO 3050-EXIT.                                         EL145
00605                                                                   EL145
00606      MOVE 'N'                    TO  CF-USER-ACTIVE-SW      (SUB) EL145
00607                                      CF-USER-RESET-SW       (SUB) EL145
00608                                      CF-USER-EACH-DAY-AFTER-SW    EL145
00609                                                             (SUB).EL145
00610      MOVE ZEROS                  TO  CF-USER-RESEND-DAYS    (SUB) EL145
00611                                      CF-USER-FOLLOW-UP-DAYS (SUB) EL145
00612                                      CF-USER-REPORT-DAYS    (SUB).EL145
00613                                                                   EL145
00614      GO TO 3050-INIT-USER-FIELDS.                                 EL145
00615                                                                   EL145
00616  3050-EXIT.                                                       EL145
00617      EXIT.                                                        EL145
00618      EJECT                                                        EL145
00619  4000-MOVE-DATA-TO-REC.                                           EL145
00620                                                                   EL145
00621      ADD +1                      TO  SUB.                         EL145
00622      IF SUB IS GREATER THAN +9                                    EL145
00623          MOVE +0                 TO  SUB                          EL145
00624          GO TO 4000-MOVE-USER-DEFINED-DATA.                       EL145
00625                                                                   EL145
00626      IF EL145A-SYS-ACT-LENGTH (SUB) IS GREATER THAN +0            EL145
00627          MOVE EL145A-SYS-ACTIVITY-SW   (SUB)      TO              EL145
00628                      CF-SYS-ACTIVE-SW  (SUB).                     EL145
00629                                                                   EL145
00630      IF EL145A-SYS-LET-LENGTH (SUB) IS GREATER THAN +0            EL145
00631          MOVE EL145A-SYS-LETTER-ID (SUB)          TO              EL145
00632                      CF-SYS-LETTER-ID (SUB).                      EL145
00633                                                                   EL145
00634      IF EL145A-SYS-RSND-LENGTH (SUB) IS GREATER THAN +0           EL145
00635          MOVE EL145A-SYS-RESEND-DAYS (SUB)        TO              EL145
00636                      CF-SYS-RESEND-DAYS (SUB).                    EL145
00637                                                                   EL145
00638      IF EL145A-SYS-FOLL-LENGTH (SUB) GREATER +0                   EL145
00639          MOVE EL145A-SYS-FOLL-DAYS (SUB)          TO              EL145
00640                      CF-SYS-FOLLOW-UP-DAYS (SUB).                 EL145
00641                                                                   EL145
00642      IF EL145A-SYS-RES-LENGTH (SUB) GREATER +0                    EL145
00643          MOVE EL145A-SYS-RESET-SW (SUB)           TO              EL145
00644                      CF-SYS-RESET-SW (SUB).                       EL145
00645                                                                   EL145
00646      IF EL145A-SYS-REP-LENGTH (SUB) GREATER +0                    EL145
00647          MOVE EL145A-SYS-REPORT-DAYS (SUB)        TO              EL145
00648                      CF-SYS-REPORT-DAYS (SUB).                    EL145
00649                                                                   EL145
00650      IF EL145A-SYS-AFT-LENGTH (SUB) GREATER +0                    EL145
00651          MOVE EL145A-SYS-AFTER-SW (SUB)           TO              EL145
00652                      CF-SYS-EACH-DAY-AFTER-SW (SUB).              EL145
00653                                                                   EL145
00654      GO TO 4000-MOVE-DATA-TO-REC.                                 EL145
00655                                                                   EL145
00656  4000-MOVE-USER-DEFINED-DATA.                                     EL145
00657                                                                   EL145
00658      ADD +1                      TO  SUB.                         EL145
00659                                                                   EL145
00660      IF SUB IS GREATER THAN +8                                    EL145
00661          GO TO 4000-EXIT.                                         EL145
00662                                                                   EL145
00663      IF EL145A-USER-ACT-LENGTH (SUB) IS GREATER THAN +0           EL145
00664          MOVE EL145A-USER-ACTIVITY-SW  (SUB)      TO              EL145
00665                      CF-USER-ACTIVE-SW (SUB).                     EL145
00666                                                                   EL145
00667      IF EL145A-USER-LET-LENGTH (SUB) IS GREATER THAN +0           EL145
00668          MOVE EL145A-USER-LETTER-ID (SUB)         TO              EL145
00669                      CF-USER-LETTER-ID (SUB).                     EL145
00670                                                                   EL145
00671      IF EL145A-USER-RSND-LENGTH (SUB) IS GREATER THAN +0          EL145
00672          MOVE EL145A-USER-RESEND-DAYS (SUB)       TO              EL145
00673                      CF-USER-RESEND-DAYS (SUB).                   EL145
00674                                                                   EL145
00675      IF EL145A-USER-FOLL-LENGTH (SUB) GREATER +0                  EL145
00676          MOVE EL145A-USER-FOLL-DAYS (SUB)         TO              EL145
00677                      CF-USER-FOLLOW-UP-DAYS (SUB).                EL145
00678                                                                   EL145
00679      IF EL145A-USER-RES-LENGTH (SUB) GREATER +0                   EL145
00680          MOVE EL145A-USER-RESET-SW (SUB)          TO              EL145
00681                      CF-USER-RESET-SW (SUB).                      EL145
00682                                                                   EL145
00683      IF EL145A-USER-REP-LENGTH (SUB) GREATER +0                   EL145
00684          MOVE EL145A-USER-REPORT-DAYS (SUB)       TO              EL145
00685                      CF-USER-REPORT-DAYS (SUB).                   EL145
00686                                                                   EL145
00687      IF EL145A-USER-AFT-LENGTH (SUB) GREATER +0                   EL145
00688          MOVE EL145A-USER-AFTER-SW (SUB)          TO              EL145
00689                      CF-USER-EACH-DAY-AFTER-SW (SUB).             EL145
00690                                                                   EL145
00691      IF EL145A-USER-DESC-LENGTH (SUB) GREATER +0                  EL145
00692          MOVE EL145A-USER-DESCRIPTION (SUB)       TO              EL145
00693                      CF-USER-ACTIVITY-DESC (SUB).                 EL145
00694                                                                   EL145
00695      GO TO 4000-MOVE-USER-DEFINED-DATA.                           EL145
00696                                                                   EL145
00697  4000-EXIT.                                                       EL145
00698      EXIT.                                                        EL145
00699      EJECT                                                        EL145
00700  5000-BUILD-SYS-DEF-ACTIVITY.                                     EL145
00701                                                                   EL145
00702      MOVE CF-LAST-MAINT-DT           TO  DC-BIN-DATE-1.           EL145
00703      MOVE ' '                        TO  DC-OPTION-CODE.          EL145
00704      PERFORM 8500-DATE-CONVERSION.                                EL145
00705      IF DATE-CONVERSION-ERROR                                     EL145
00706          MOVE SPACES                 TO  MAINTONO                 EL145
00707      ELSE                                                         EL145
00708          MOVE DC-GREG-DATE-1-EDIT    TO  MAINTONO.                EL145
00709                                                                   EL145
00710      MOVE CF-LAST-MAINT-BY           TO  MAINTBYO.                EL145
00711      MOVE CF-LAST-MAINT-HHMMSS       TO  TIME-IN.                 EL145
00712      MOVE TIME-OUT                   TO  MAINTATO.                EL145
00713                                                                   EL145
00714      ADD +1                      TO  SUB.                         EL145
00715      IF SUB IS GREATER THAN +9                                    EL145
00716          MOVE +0                 TO  SUB                          EL145
00717          GO TO 5010-BUILD-USER-DEF-ACTIVITY.                      EL145
00718                                                                   EL145
00719      MOVE CF-SYS-ACTIVE-SW         (SUB)   TO                     EL145
00720                                  EL145A-SYS-ACTIVITY-SW (SUB).    EL145
00721                                                                   EL145
00722      MOVE CF-SYS-LETTER-ID         (SUB)   TO                     EL145
00723                                  EL145A-SYS-LETTER-ID   (SUB).    EL145
00724                                                                   EL145
00725      MOVE CF-SYS-RESEND-DAYS       (SUB)   TO                     EL145
00726                                  EL145A-SYS-RESEND-DAYS (SUB).    EL145
00727                                                                   EL145
00728      MOVE CF-SYS-FOLLOW-UP-DAYS   (SUB)   TO                      EL145
00729                                  EL145A-SYS-FOLL-DAYS   (SUB).    EL145
00730                                                                   EL145
00731      MOVE CF-SYS-RESET-SW          (SUB)   TO                     EL145
00732                                  EL145A-SYS-RESET-SW    (SUB).    EL145
00733                                                                   EL145
00734      MOVE CF-SYS-REPORT-DAYS       (SUB)   TO                     EL145
00735                                  EL145A-SYS-REPORT-DAYS (SUB).    EL145
00736                                                                   EL145
00737      MOVE CF-SYS-EACH-DAY-AFTER-SW (SUB)   TO                     EL145
00738                                  EL145A-SYS-AFTER-SW    (SUB).    EL145
00739                                                                   EL145
00740      GO TO 5000-BUILD-SYS-DEF-ACTIVITY.                           EL145
00741                                                                   EL145
00742  5010-BUILD-USER-DEF-ACTIVITY.                                    EL145
00743                                                                   EL145
00744      ADD +1                      TO  SUB.                         EL145
00745      IF SUB IS GREATER THAN +8                                    EL145
00746          MOVE -1                 TO  MAINTL                       EL145
00747          GO TO 8100-SEND-INITIAL-MAP.                             EL145
00748                                                                   EL145
00749      MOVE CF-USER-ACTIVE-SW        (SUB)   TO                     EL145
00750                                  EL145A-USER-ACTIVITY-SW (SUB)    EL145
00751                                  PI-USER-ACTIVITY-SW     (SUB).   EL145
00752                                                                   EL145
00753      MOVE CF-USER-LETTER-ID        (SUB)   TO                     EL145
00754                                  EL145A-USER-LETTER-ID   (SUB).   EL145
00755                                                                   EL145
00756      MOVE CF-USER-RESEND-DAYS      (SUB)   TO                     EL145
00757                                  EL145A-USER-RESEND-DAYS (SUB).   EL145
00758                                                                   EL145
00759      MOVE CF-USER-FOLLOW-UP-DAYS  (SUB)   TO                      EL145
00760                                  EL145A-USER-FOLL-DAYS   (SUB).   EL145
00761                                                                   EL145
00762      MOVE CF-USER-RESET-SW         (SUB)   TO                     EL145
00763                                  EL145A-USER-RESET-SW    (SUB).   EL145
00764                                                                   EL145
00765      MOVE CF-USER-REPORT-DAYS      (SUB)   TO                     EL145
00766                                  EL145A-USER-REPORT-DAYS (SUB).   EL145
00767                                                                   EL145
00768      MOVE CF-USER-EACH-DAY-AFTER-SW (SUB)  TO                     EL145
00769                                  EL145A-USER-AFTER-SW    (SUB).   EL145
00770                                                                   EL145
00771      MOVE CF-USER-ACTIVITY-DESC     (SUB)  TO                     EL145
00772                                  EL145A-USER-DESCRIPTION (SUB)    EL145
00773                                  PI-USER-DESCRIPTION     (SUB).   EL145
00774                                                                   EL145
00775      GO TO 5010-BUILD-USER-DEF-ACTIVITY.                          EL145
00776                                                                   EL145
00777      EJECT                                                        EL145
00778  6000-EDIT-INPUT-DATA.                                            EL145
00779                                                                   EL145
00780      ADD +1                      TO  SUB.                         EL145
00781      IF SUB IS GREATER THAN +9                                    EL145
00782          MOVE +0                 TO  SUB                          EL145
00783          GO TO 6010-EDIT-USER-DATA.                               EL145
00784                                                                   EL145
00785      IF EL145A-SYS-ACT-LENGTH (SUB) GREATER ZERO                  EL145
00786          IF (EL145A-SYS-ACTIVITY-SW (SUB) EQUAL 'Y' OR 'N')       EL145
00787              MOVE AL-UANON   TO  EL145A-SYS-ACT-ATTRB (SUB)       EL145
00788              MOVE 'Y'        TO  WS-UPDATE-SW                     EL145
00789          ELSE                                                     EL145
00790              MOVE ER-3518    TO  EMI-ERROR                        EL145
00791              MOVE -1         TO  EL145A-SYS-ACT-LENGTH (SUB)      EL145
00792              MOVE AL-UABON   TO  EL145A-SYS-ACT-ATTRB  (SUB)      EL145
00793              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL145
00794                                                                   EL145
00795      IF EL145A-SYS-LET-LENGTH (SUB) GREATER ZERO                  EL145
00796          MOVE EL145A-SYS-LETTER-ID (SUB) TO  ELTEXT-LETTER-ID     EL145
00797          PERFORM 7000-READ-LETTER-FILE THRU 7000-EXIT             EL145
00798          IF WS-LETTER-SW IS EQUAL TO 'Y'                          EL145
00799              MOVE AL-UANON   TO  EL145A-SYS-LET-ATTRB (SUB)       EL145
00800              MOVE 'Y'        TO  WS-UPDATE-SW                     EL145
00801          ELSE                                                     EL145
00802              MOVE ER-3519    TO  EMI-ERROR                        EL145
00803              MOVE -1         TO  EL145A-SYS-LET-LENGTH (SUB)      EL145
00804              MOVE AL-UABON   TO  EL145A-SYS-LET-ATTRB  (SUB)      EL145
00805              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL145
00806                                                                   EL145
00807      IF EL145A-SYS-RSND-LENGTH (SUB) GREATER ZERO                 EL145
00808        MOVE EL145A-SYS-RESEND-DAYS (SUB) TO  DEEDIT-FIELD         EL145
00809        PERFORM 8000-DEEDIT THRU 8000-EXIT                         EL145
00810        IF DEEDIT-FIELD-V0 IS NUMERIC                              EL145
00811          MOVE DEEDIT-FIELD-V0    TO  EL145A-SYS-RESEND-DAYS (SUB) EL145
00812          MOVE AL-UANON           TO  EL145A-SYS-RSND-ATTRB  (SUB) EL145
00813          MOVE 'Y'                TO  WS-UPDATE-SW                 EL145
00814        ELSE                                                       EL145
00815          MOVE ER-3520            TO  EMI-ERROR                    EL145
00816          MOVE -1                 TO  EL145A-SYS-RSND-LENGTH (SUB) EL145
00817          MOVE AL-UABON           TO  EL145A-SYS-RSND-ATTRB  (SUB) EL145
00818          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL145
00819                                                                   EL145
00820      IF EL145A-SYS-FOLL-LENGTH (SUB) GREATER ZERO                 EL145
00821        MOVE EL145A-SYS-FOLL-DAYS (SUB) TO  DEEDIT-FIELD           EL145
00822        PERFORM 8000-DEEDIT THRU 8000-EXIT                         EL145
00823        IF DEEDIT-FIELD-V0 IS NUMERIC                              EL145
00824          MOVE DEEDIT-FIELD-V0    TO  EL145A-SYS-FOLL-DAYS  (SUB)  EL145
00825          MOVE AL-UANON           TO  EL145A-SYS-FOLL-ATTRB  (SUB) EL145
00826          MOVE 'Y'                TO  WS-UPDATE-SW                 EL145
00827        ELSE                                                       EL145
00828          MOVE ER-3521            TO  EMI-ERROR                    EL145
00829          MOVE -1                 TO  EL145A-SYS-FOLL-LENGTH (SUB) EL145
00830          MOVE AL-UABON           TO  EL145A-SYS-FOLL-ATTRB  (SUB) EL145
00831          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL145
00832                                                                   EL145
00833      IF EL145A-SYS-REP-LENGTH (SUB) GREATER ZERO                  EL145
00834        MOVE EL145A-SYS-REPORT-DAYS (SUB) TO  DEEDIT-FIELD         EL145
00835        PERFORM 8000-DEEDIT THRU 8000-EXIT                         EL145
00836        IF DEEDIT-FIELD-V0 IS NUMERIC                              EL145
00837          MOVE DEEDIT-FIELD-V0    TO  EL145A-SYS-REPORT-DAYS (SUB) EL145
00838          MOVE AL-UANON           TO  EL145A-SYS-REP-ATTRB   (SUB) EL145
00839          MOVE 'Y'                TO  WS-UPDATE-SW                 EL145
00840        ELSE                                                       EL145
00841          MOVE ER-3522            TO  EMI-ERROR                    EL145
00842          MOVE -1                 TO  EL145A-SYS-REP-LENGTH  (SUB) EL145
00843          MOVE AL-UABON           TO  EL145A-SYS-REP-ATTRB   (SUB) EL145
00844          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL145
00845                                                                   EL145
00846      IF EL145A-SYS-RES-LENGTH (SUB) GREATER ZERO                  EL145
00847          IF (EL145A-SYS-RESET-SW (SUB) EQUAL 'Y' OR 'N')          EL145
00848              MOVE AL-UANON   TO  EL145A-SYS-RES-ATTRB  (SUB)      EL145
00849              MOVE 'Y'        TO  WS-UPDATE-SW                     EL145
00850          ELSE                                                     EL145
00851              MOVE ER-3523    TO  EMI-ERROR                        EL145
00852              MOVE -1         TO  EL145A-SYS-RES-LENGTH (SUB)      EL145
00853              MOVE AL-UABON   TO  EL145A-SYS-RES-ATTRB  (SUB)      EL145
00854              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL145
00855                                                                   EL145
00856      IF EL145A-SYS-AFT-LENGTH (SUB) GREATER ZERO                  EL145
00857          IF (EL145A-SYS-AFTER-SW (SUB) EQUAL 'Y' OR 'N')          EL145
00858              MOVE AL-UANON   TO  EL145A-SYS-AFT-ATTRB  (SUB)      EL145
00859              MOVE 'Y'        TO  WS-UPDATE-SW                     EL145
00860          ELSE                                                     EL145
00861              MOVE ER-3524    TO  EMI-ERROR                        EL145
00862              MOVE -1         TO  EL145A-SYS-AFT-LENGTH (SUB)      EL145
00863              MOVE AL-UABON   TO  EL145A-SYS-AFT-ATTRB  (SUB)      EL145
00864              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL145
00865                                                                   EL145
00866      GO TO 6000-EDIT-INPUT-DATA.                                  EL145
00867                                                                   EL145
00868  6010-EDIT-USER-DATA.                                             EL145
00869                                                                   EL145
00870      ADD +1                      TO  SUB.                         EL145
00871      IF SUB IS GREATER THAN +8                                    EL145
00872          GO TO 6000-EXIT.                                         EL145
00873                                                                   EL145
00874      IF EL145A-USER-ACT-LENGTH (SUB) GREATER ZERO                 EL145
00875          IF (EL145A-USER-ACTIVITY-SW (SUB) EQUAL 'Y' OR 'N')      EL145
00876              MOVE AL-UANON   TO  EL145A-USER-ACT-ATTRB (SUB)      EL145
00877              MOVE 'Y'        TO  WS-UPDATE-SW                     EL145
00878              MOVE EL145A-USER-ACTIVITY-SW (SUB)                   EL145
00879                              TO  PI-USER-ACTIVITY-SW   (SUB)      EL145
00880          ELSE                                                     EL145
00881              MOVE ER-3518    TO  EMI-ERROR                        EL145
00882              MOVE -1         TO  EL145A-USER-ACT-LENGTH (SUB)     EL145
00883              MOVE AL-UABON   TO  EL145A-USER-ACT-ATTRB (SUB)      EL145
00884              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL145
00885                                                                   EL145
00886      IF EL145A-USER-LET-LENGTH (SUB) GREATER ZERO                 EL145
00887          MOVE EL145A-USER-LETTER-ID (SUB) TO ELTEXT-LETTER-ID     EL145
00888          PERFORM 7000-READ-LETTER-FILE THRU 7000-EXIT             EL145
00889          IF WS-LETTER-SW IS EQUAL TO 'Y'                          EL145
00890              MOVE AL-UANON   TO  EL145A-USER-LET-ATTRB (SUB)      EL145
00891              MOVE 'Y'        TO  WS-UPDATE-SW                     EL145
00892          ELSE                                                     EL145
00893              MOVE ER-3519    TO  EMI-ERROR                        EL145
00894              MOVE -1         TO  EL145A-USER-LET-LENGTH (SUB)     EL145
00895              MOVE AL-UABON   TO  EL145A-USER-LET-ATTRB (SUB)      EL145
00896              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL145
00897                                                                   EL145
00898      IF EL145A-USER-RSND-LENGTH (SUB) GREATER ZERO                EL145
00899        MOVE EL145A-USER-RESEND-DAYS (SUB) TO DEEDIT-FIELD         EL145
00900        PERFORM 8000-DEEDIT THRU 8000-EXIT                         EL145
00901        IF DEEDIT-FIELD-V0 IS NUMERIC                              EL145
00902          MOVE DEEDIT-FIELD-V0    TO  EL145A-USER-RESEND-DAYS (SUB)EL145
00903          MOVE AL-UANON           TO  EL145A-USER-RSND-ATTRB (SUB) EL145
00904          MOVE 'Y'                TO  WS-UPDATE-SW                 EL145
00905        ELSE                                                       EL145
00906          MOVE ER-3520            TO  EMI-ERROR                    EL145
00907          MOVE -1                 TO  EL145A-USER-RSND-LENGTH (SUB)EL145
00908          MOVE AL-UABON           TO  EL145A-USER-RSND-ATTRB (SUB) EL145
00909          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL145
00910                                                                   EL145
00911      IF EL145A-USER-FOLL-LENGTH (SUB) GREATER ZERO                EL145
00912        MOVE EL145A-USER-FOLL-DAYS (SUB) TO DEEDIT-FIELD           EL145
00913        PERFORM 8000-DEEDIT THRU 8000-EXIT                         EL145
00914        IF DEEDIT-FIELD-V0 IS NUMERIC                              EL145
00915          MOVE DEEDIT-FIELD-V0    TO  EL145A-USER-FOLL-DAYS (SUB)  EL145
00916          MOVE AL-UANON           TO  EL145A-USER-FOLL-ATTRB (SUB) EL145
00917          MOVE 'Y'                TO  WS-UPDATE-SW                 EL145
00918        ELSE                                                       EL145
00919          MOVE ER-3521            TO  EMI-ERROR                    EL145
00920          MOVE -1                 TO  EL145A-USER-FOLL-LENGTH (SUB)EL145
00921          MOVE AL-UABON           TO  EL145A-USER-FOLL-ATTRB (SUB) EL145
00922          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL145
00923                                                                   EL145
00924      IF EL145A-USER-REP-LENGTH (SUB) GREATER ZERO                 EL145
00925        MOVE EL145A-USER-REPORT-DAYS (SUB) TO DEEDIT-FIELD         EL145
00926        PERFORM 8000-DEEDIT THRU 8000-EXIT                         EL145
00927        IF DEEDIT-FIELD-V0 IS NUMERIC                              EL145
00928          MOVE DEEDIT-FIELD-V0    TO  EL145A-USER-REPORT-DAYS (SUB)EL145
00929          MOVE AL-UANON           TO  EL145A-USER-REP-ATTRB  (SUB) EL145
00930          MOVE 'Y'                TO  WS-UPDATE-SW                 EL145
00931        ELSE                                                       EL145
00932          MOVE ER-3522            TO  EMI-ERROR                    EL145
00933          MOVE -1                 TO  EL145A-USER-REP-LENGTH (SUB) EL145
00934          MOVE AL-UABON           TO  EL145A-USER-REP-ATTRB  (SUB) EL145
00935          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL145
00936                                                                   EL145
00937      IF EL145A-USER-RES-LENGTH (SUB) GREATER ZERO                 EL145
00938          IF (EL145A-USER-RESET-SW (SUB) EQUAL 'Y' OR 'N')         EL145
00939              MOVE AL-UANON   TO  EL145A-USER-RES-ATTRB (SUB)      EL145
00940              MOVE 'Y'        TO  WS-UPDATE-SW                     EL145
00941          ELSE                                                     EL145
00942              MOVE ER-3523    TO  EMI-ERROR                        EL145
00943              MOVE -1         TO  EL145A-USER-RES-LENGTH (SUB)     EL145
00944              MOVE AL-UABON   TO  EL145A-USER-RES-ATTRB (SUB)      EL145
00945              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL145
00946                                                                   EL145
00947      IF EL145A-USER-AFT-LENGTH (SUB) GREATER ZERO                 EL145
00948          IF (EL145A-USER-AFTER-SW (SUB) EQUAL 'Y' OR 'N')         EL145
00949              MOVE AL-UANON   TO  EL145A-USER-AFT-ATTRB (SUB)      EL145
00950              MOVE 'Y'        TO  WS-UPDATE-SW                     EL145
00951          ELSE                                                     EL145
00952              MOVE ER-3524    TO  EMI-ERROR                        EL145
00953              MOVE -1         TO  EL145A-USER-AFT-LENGTH (SUB)     EL145
00954              MOVE AL-UABON   TO  EL145A-USER-AFT-ATTRB (SUB)      EL145
00955              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL145
00956                                                                   EL145
00957      IF EL145A-USER-DESC-LENGTH (SUB) GREATER ZERO                EL145
00958          MOVE AL-UANON   TO  EL145A-USER-DESC-ATTRB (SUB)         EL145
00959          MOVE 'Y'        TO  WS-UPDATE-SW                         EL145
00960          MOVE EL145A-USER-DESCRIPTION (SUB)                       EL145
00961                          TO  PI-USER-DESCRIPTION    (SUB).        EL145
00962                                                                   EL145
00963      IF (PI-USER-ACTIVITY-SW (SUB) EQUAL 'Y')  AND                EL145
00964          (PI-USER-DESCRIPTION (SUB) EQUAL SPACES OR               EL145
00965                                               LOW-VALUES)         EL145
00966              MOVE ER-3525    TO  EMI-ERROR                        EL145
00967              MOVE -1         TO  EL145A-USER-DESC-LENGTH (SUB)    EL145
00968              MOVE AL-UABON   TO  EL145A-USER-DESC-ATTRB  (SUB)    EL145
00969              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL145
00970                                                                   EL145
00971      GO TO 6010-EDIT-USER-DATA.                                   EL145
00972                                                                   EL145
00973  6000-EXIT.                                                       EL145
00974      EXIT.                                                        EL145
00975                                                                   EL145
00976      EJECT                                                        EL145
00977  7000-READ-LETTER-FILE.                                           EL145
00978                                                                   EL145
00979      IF ELTEXT-LETTER-ID EQUAL SPACES                             EL145
00980          MOVE 'Y'                    TO  WS-LETTER-SW             EL145
00981          GO TO 7000-EXIT.                                         EL145
00982                                                                   EL145
00983      MOVE PI-COMPANY-CD          TO  ELTEXT-COMPANY-CD.           EL145
00984      MOVE SPACES                 TO  ELTEXT-FILLER.               EL145
00985      MOVE +1                     TO  ELTEXT-SEQ-NO.               EL145
00986                                                                   EL145
00987      EXEC CICS HANDLE CONDITION                                   EL145
00988          NOTFND   (7000-NOT-FOUND)                                EL145
00989      END-EXEC.                                                    EL145
00990                                                                   EL145
00991      EXEC CICS READ                                               EL145
00992          DATASET   (ELTEXT-DSID)                                  EL145
00993          RIDFLD    (ELTEXT-KEY)                                   EL145
00994          SET       (ADDRESS OF TEXT-FILES)                           CL**2
00995      END-EXEC.                                                    EL145
00996                                                                   EL145
00997      MOVE 'Y'                    TO  WS-LETTER-SW.                EL145
00998      GO TO 7000-EXIT.                                             EL145
00999                                                                   EL145
01000  7000-NOT-FOUND.                                                  EL145
01001      MOVE 'N'                    TO  WS-LETTER-SW.                EL145
01002                                                                   EL145
01003  7000-EXIT.                                                       EL145
01004      EXIT.                                                        EL145
01005                                                                   EL145
01006  8000-DEEDIT.                                                     EL145
01007      EXEC CICS BIF DEEDIT                                         EL145
01008          FIELD    (DEEDIT-FIELD)                                  EL145
01009          LENGTH   (3)                                             EL145
01010      END-EXEC.                                                    EL145
01011                                                                   EL145
01012  8000-EXIT.                                                       EL145
01013      EXIT.                                                        EL145
01014      EJECT                                                        EL145
01015  8100-SEND-INITIAL-MAP.                                           EL145
01016                                                                   EL145
01017      IF EMI-ERROR NOT = ZERO                                      EL145
01018         PERFORM 9900-ERROR-FORMAT.                                EL145
01019                                                                   EL145
01020      MOVE EIBTIME                TO  TIME-IN.                     EL145
01021      MOVE SAVE-DATE              TO  DATEO.                       EL145
01022      MOVE TIME-OUT               TO  TIMEO.                       EL145
01023      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSGO.                     EL145
01024                                                                   EL145
01025      EXEC CICS SEND                                                  CL**2
01026          FROM   (EL145AI)                                            CL**2
01027          MAPSET (WS-MAPSET-NAME)                                     CL**2
01028          MAP    (WS-MAP-NAME)                                        CL**2
01029          CURSOR                                                      CL**2
01030          ERASE                                                       CL**2
01031      END-EXEC.                                                       CL**2
01032                                                                      CL**2
01033      GO TO 9100-RETURN-TRAN.                                      EL145
01034                                                                   EL145
01035  8200-SEND-DATAONLY.                                              EL145
01036                                                                   EL145
01037      IF EMI-ERROR NOT = ZERO                                      EL145
01038          PERFORM 9900-ERROR-FORMAT.                               EL145
01039                                                                   EL145
01040      MOVE EIBTIME                TO  TIME-IN.                     EL145
01041      MOVE SAVE-DATE              TO  DATEO.                       EL145
01042      MOVE TIME-OUT               TO  TIMEO.                       EL145
01043      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSGO.                     EL145
01044                                                                   EL145
01045      EXEC CICS SEND DATAONLY                                      EL145
01046          FROM   (EL145AI)                                         EL145
01047          MAPSET (WS-MAPSET-NAME)                                  EL145
01048          MAP    (WS-MAP-NAME)                                     EL145
01049          CURSOR                                                   EL145
01050      END-EXEC.                                                    EL145
01051                                                                      CL**2
01052      GO TO 9100-RETURN-TRAN.                                      EL145
01053                                                                   EL145
01054      EJECT                                                        EL145
01055  8300-SEND-TEXT.                                                  EL145
01056                                                                   EL145
01057      EXEC CICS SEND TEXT                                          EL145
01058          FROM   (LOGOFF-TEXT)                                     EL145
01059          LENGTH (LOGOFF-LENGTH)                                   EL145
01060          ERASE  FREEKB                                            EL145
01061      END-EXEC.                                                    EL145
01062                                                                   EL145
01063      EXEC CICS RETURN                                             EL145
01064      END-EXEC.                                                    EL145
01065                                                                   EL145
01066      EJECT                                                        EL145
01067  8500-DATE-CONVERSION.                                            EL145
01068                                                                   EL145
01069      MOVE LINK-ELDATCV           TO  THIS-PGM.                    EL145
01070      EXEC CICS LINK                                               EL145
01071          PROGRAM  (THIS-PGM)                                      EL145
01072          COMMAREA (DATE-CONVERSION-DATA)                          EL145
01073          LENGTH   (DC-COMM-LENGTH)                                EL145
01074      END-EXEC.                                                    EL145
01075                                                                   EL145
01076  8500-EXIT.                                                       EL145
01077      EXIT.                                                        EL145
01078                                                                   EL145
01079  8800-UNAUTHORIZED-ACCESS.                                        EL145
01080                                                                   EL145
01081      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL145
01082      GO TO 8300-SEND-TEXT.                                        EL145
01083                                                                   EL145
01084  8870-NOTOPEN.                                                    EL145
01085                                                                   EL145
01086      MOVE ER-0042                TO  EMI-ERROR.                   EL145
01087      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL145
01088      MOVE -1                     TO  MAINTL.                      EL145
01089      GO TO 8200-SEND-DATAONLY.                                    EL145
01090                                                                   EL145
01091      EJECT                                                        EL145
01092  9000-RETURN-CICS.                                                EL145
01093                                                                   EL145
01094      MOVE XCTL-005               TO  THIS-PGM.                    EL145
01095      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL145
01096      GO TO 9300-XCTL.                                             EL145
01097                                                                   EL145
01098  9100-RETURN-TRAN.                                                EL145
01099                                                                   EL145
01100      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL145
01101      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.        EL145
01102                                                                   EL145
01103      EXEC CICS RETURN                                             EL145
01104          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL145
01105          LENGTH   (PI-COMM-LENGTH)                                EL145
01106          TRANSID  (WS-TRANS-ID)                                   EL145
01107      END-EXEC.                                                    EL145
01108                                                                   EL145
01109  9100-EXIT.                                                       EL145
01110      EXIT.                                                        EL145
01111                                                                   EL145
01112  9300-XCTL.                                                       EL145
01113                                                                   EL145
01114      MOVE DFHENTER               TO  EIBAID.                      EL145
01115                                                                   EL145
01116      EXEC CICS XCTL                                               EL145
01117          PROGRAM  (THIS-PGM)                                      EL145
01118          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL145
01119          LENGTH   (PI-COMM-LENGTH)                                EL145
01120      END-EXEC.                                                    EL145
01121                                                                   EL145
01122  9300-EXIT.                                                       EL145
01123      EXIT.                                                        EL145
01124                                                                   EL145
01125      EJECT                                                        EL145
01126  9400-CLEAR.                                                      EL145
01127                                                                   EL145
01128      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.                     EL145
01129      GO TO 9300-XCTL.                                             EL145
01130                                                                   EL145
01131  9600-PGMIDERR.                                                   EL145
01132                                                                   EL145
01133      EXEC CICS HANDLE CONDITION                                   EL145
01134          PGMIDERR (8300-SEND-TEXT)                                EL145
01135      END-EXEC.                                                    EL145
01136                                                                   EL145
01137      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM           EL145
01138                                      LOGOFF-PGM.                  EL145
01139                                                                   EL145
01140      MOVE XCTL-005               TO  THIS-PGM.                    EL145
01141      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL145
01142      MOVE SPACES                 TO  PI-ENTRY-CD-1.               EL145
01143      GO TO 9300-XCTL.                                             EL145
01144                                                                   EL145
01145  9900-ERROR-FORMAT.                                               EL145
01146                                                                   EL145
01147      MOVE LINK-001               TO  THIS-PGM.                    EL145
01148      EXEC CICS LINK                                               EL145
01149          PROGRAM  (THIS-PGM)                                      EL145
01150          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 EL145
01151          LENGTH   (EMI-COMM-LENGTH)                               EL145
01152      END-EXEC.                                                    EL145
01153                                                                   EL145
01154  9900-EXIT.                                                       EL145
01155      EXIT.                                                        EL145
01156                                                                   EL145
01157  9990-ERROR.                                                      EL145
01158                                                                   EL145
01159      MOVE DFHEIBLK               TO  EMI-LINE1.                   EL145
01160      MOVE LINK-004               TO  THIS-PGM.                    EL145
01161      EXEC CICS LINK                                               EL145
01162          PROGRAM  (THIS-PGM)                                      EL145
01163          COMMAREA (EMI-LINE1)                                     EL145
01164          LENGTH   (72)                                            EL145
01165      END-EXEC.                                                    EL145
01166                                                                   EL145
01167      GO TO 8200-SEND-DATAONLY.                                    EL145
01168                                                                   EL145
01169      EJECT                                                        EL145
01170  9995-SECURITY-VIOLATION.                                         EL145
01171      COPY ELCSCTP.                                                   CL**2
01172                                                                   EL145
01173  9995-EXIT.                                                       EL145
01174      EXIT.                                                        EL145
01175                                                                   EL145
01176  9999-LAST-PARAGRAPH SECTION.                                     EL145
01177                                                                   EL145
01178      GOBACK.                                                      EL145
