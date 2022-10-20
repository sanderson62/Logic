00001  IDENTIFICATION DIVISION.                                         03/06/96
00002                                                                   EL653
00003  PROGRAM-ID.                 EL653 .                                 LV007
00004 *              PROGRAM CONVERTED BY                                  CL**7
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**7
00006 *              CONVERSION DATE 02/12/96 09:14:25.                    CL**7
00007 *                            VMOD 2.007                              CL**7
00008 *                                                                 EL653
00008 *                                                                 EL653
00009 *AUTHOR.        LOGIC,INC.                                           CL**7
00010 *               DALLAS, TEXAS.                                       CL**7
00011                                                                   EL653
00012 *DATE-COMPILED.                                                      CL**7
00013                                                                   EL653
00014 *SECURITY.   *****************************************************   CL**7
00015 *            *                                                   *   CL**7
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**7
00017 *            *                                                   *   CL**7
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**7
00019 *                                                                *   CL**7
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**7
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**7
00022 *            *                                                   *   CL**7
00023 *            *****************************************************   CL**7
00024 *                                                                 EL653
00025 *REMARKS.                                                            CL**3
00026 *        TRANSACTION - EXD5 - COMMISSION MASTER MAINT.               CL**3
00024 *                                                                 EL653
101101******************************************************************
101101*                   C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101101*                              ADJUSTED REDEFINES EL653AI FILLER
101101******************************************************************

00027                                                                   EL653
00028  ENVIRONMENT DIVISION.                                            EL653
00029  DATA DIVISION.                                                   EL653
00030  EJECT                                                            EL653
00031  WORKING-STORAGE SECTION.                                         EL653
00032  77  FILLER  PIC X(32)  VALUE '********************************'. EL653
00033  77  FILLER  PIC X(32)  VALUE '*    EL653 WORKING STORAGE     *'. EL653
00034  77  FILLER  PIC X(32)  VALUE '************ V/M 2.007 *********'.    CL**7
00035                                                                   EL653
00036  01  WS-DATE-AREA.                                                EL653
00037      12  SAVE-DATE           PIC  X(8)       VALUE SPACES.        EL653
00038      12  SAVE-BIN-DATE       PIC  XX         VALUE SPACES.        EL653
00039                                                                   EL653
00040  01  STANDARD-AREAS.                                              EL653
00041      12  GETMAIN-SPACE       PIC  X          VALUE SPACE.         EL653
00042      12  MAP-NAME            PIC  X(8)       VALUE 'EL653A'.      EL653
00043      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL653S'.      EL653
00044      12  TRANS-ID            PIC  X(4)       VALUE 'EXD5'.        EL653
00045      12  THIS-PGM            PIC  X(8)       VALUE 'EL653'.       EL653
00046      12  PGM-NAME            PIC  X(8).                           EL653
00047      12  WS-COMP-CD-R.                                            EL653
00048          16  FILLER          PIC  X.                              EL653
00049          16  WS-COMP-CD-X    PIC  X.                              EL653
00050      12  WS-COMP-CD  REDEFINES                                    EL653
00051          WS-COMP-CD-R        PIC S9(4)                  COMP.     EL653
00052      12  TIME-IN             PIC S9(7).                           EL653
00053      12  TIME-OUT-R  REDEFINES  TIME-IN.                          EL653
00054          16  FILLER          PIC  X.                              EL653
00055          16  TIME-OUT        PIC  99V99.                          EL653
00056          16  FILLER          PIC  XX.                             EL653
00057      12  XCTL-005            PIC  X(8)       VALUE 'EL005'.       EL653
00058      12  XCTL-010            PIC  X(8)       VALUE 'EL010'.       EL653
00059      12  XCTL-626            PIC  X(8)       VALUE 'EL126'.       EL653
00060      12  LINK-001            PIC  X(8)       VALUE 'EL001'.       EL653
00061      12  LINK-004            PIC  X(8)       VALUE 'EL004'.       EL653
00062      12  LINK-CLDATCV        PIC  X(8)       VALUE 'ELDATCV'.     EL653
00063      12  FILE-ID             PIC  X(8)       VALUE  SPACES.       EL653
00064      12  CTBL-FILE-ID        PIC  X(8)       VALUE 'ERCTBL'.      EL653
00065      12  CNTL-FILE-ID        PIC  X(8)       VALUE 'ELCNTL'.      EL653
00066                                                                   EL653
00067  01  MISC-WORK-AREAS.                                             EL653
00068      12  WS-PHONE-IN         PIC  9(10).                          EL653
00069      12  WS-PHONE-IN-R  REDEFINES  WS-PHONE-IN.                   EL653
00070          16  WSPI-AREA       PIC  X(3).                           EL653
00071          16  WSPI-PFX        PIC  X(3).                           EL653
00072          16  WSPI-SFX        PIC  X(4).                           EL653
00073      12  WS-PHONE-OUT.                                            EL653
00074          16  WSPO-AREA       PIC  X(3).                           EL653
00075          16  FILLER          PIC  X           VALUE '-'.          EL653
00076          16  WSPO-PFX        PIC  X(3).                           EL653
00077          16  FILLER          PIC  X           VALUE '-'.          EL653
00078          16  WSPO-SFX        PIC  X(4).                           EL653
00079      12  DEEDIT-FIELD        PIC  X(15).                          EL653
00080      12  FILLER  REDEFINES  DEEDIT-FIELD.                            CL**3
00081          16  FILLER          PIC  X(8).                              CL**3
00082          16  DEEDIT-FIELD-X7 PIC  X(7).                              CL**3
00083      12  DEEDIT-FIELD-V0  REDEFINES                               EL653
00084          DEEDIT-FIELD        PIC S9(15).                          EL653
00085      12  DEEDIT-FIELD-V1  REDEFINES                               EL653
00086          DEEDIT-FIELD        PIC S9(13)V99.                       EL653
00087      12  DEEDIT-FIELD-V5  REDEFINES                                  CL**3
00088          DEEDIT-FIELD        PIC S9(10)V9(5).                        CL**3
00089      12  SUB1                PIC S9(4)        VALUE +0   COMP.    EL653
00090      12  SUB2                PIC S9(4)        VALUE +0   COMP.    EL653
00091      12  SUB3                PIC S9(4)        VALUE +0   COMP.    EL653
00092      12  SC-ITEM             PIC S9(4)        VALUE +1   COMP.    EL653
00093      12  ERCTBL-LENGTH       PIC S9(4)        VALUE +340 COMP.    EL653
00094      12  SV-CLMTOL           PIC  9(3)V99   VALUE ZEROS.          EL653
00095      12  DATE-TEST-AREA      PIC  9(6).                           EL653
00096      12  DATE-TEST-AREA-R  REDEFINES  DATE-TEST-AREA.             EL653
00097          16  DATE-TEST-MM    PIC  99.                             EL653
00098          16  DATE-TEST-DD    PIC  99.                             EL653
00099          16  DATE-TEST-YY    PIC  99.                             EL653
00100      12  DIVIDE-RESULT       PIC  99.                             EL653
00101      12  DIVIDE-REMAINDER    PIC  9.                              EL653
00102      12  WS-ZERO             PIC  X           VALUE '0'.          EL653
00103      12  WS-ONE              PIC  X           VALUE '1'.          EL653
00104      12  WS-TWO              PIC  X           VALUE '2'.          EL653
00105      12  WS-MAX-AGE          PIC  99          VALUE 99.           EL653
00106      12  WS-MAX-BENEFIT      PIC S9(7)V99 VALUE +9999999.99.         CL**6
00107      12  WS-BENEFIT          PIC S9(7)V99 VALUE +0.                  CL**6
00108      12  WS-PREV-TERM        PIC  9(3)        VALUE ZERO.         EL653
00109      12  WS-PREV-AGE         PIC  99          VALUE ZERO.         EL653
00110      12  WS-PREV-BENEFIT     PIC S9(7)V99 VALUE +0.                  CL**6
00111      12  WS-RATE             PIC SV9(5)   VALUE ZEROS.               CL**3
00112      12  WS-SAVE-TABLE       PIC  X(3)        VALUE SPACES.       EL653
00113      12  BIN-CURRENT-SAVE    PIC  XX          VALUE SPACES.       EL653
00114      12  ERCTBL-KEY.                                              EL653
00115          16  CTBL-COMP-ID    PIC  X(3)        VALUE SPACES.       EL653
00116          16  CTBL-REC-TYPE   PIC  X           VALUE SPACES.       EL653
00117          16  CTBL-ACCESS     PIC  X(4)        VALUE SPACES.       EL653
00118          16  CTBL-SEQ-NO     PIC S9(4)        VALUE +0   COMP.    EL653
00119      12  ELCNTL-KEY.                                              EL653
00120          16  CNTL-COMP-ID    PIC  X(3)        VALUE SPACES.       EL653
00121          16  CNTL-REC-TYPE   PIC  X           VALUE SPACES.       EL653
00122          16  CNTL-ACCESS     PIC  X(4)        VALUE SPACES.       EL653
00123          16  CNTL-SEQ-NO     PIC S9(4)        VALUE +0   COMP.    EL653
00124  EJECT                                                            EL653
00125  01  ERROR-NUMBERS.                                               EL653
00126      12  ER-0000             PIC  X(4)       VALUE '0000'.        EL653
00127      12  ER-0004             PIC  X(4)       VALUE '0004'.        EL653
00128      12  ER-0008             PIC  X(4)       VALUE '0008'.        EL653
00129      12  ER-0029             PIC  X(4)       VALUE '0029'.        EL653
00130      12  ER-0050             PIC  X(4)       VALUE '0050'.        EL653
00131      12  ER-0068             PIC  X(4)       VALUE '0068'.        EL653
00132      12  ER-0070             PIC  X(4)       VALUE '0070'.        EL653
00133      12  ER-0142             PIC  X(4)       VALUE '0142'.        EL653
00134      12  ER-0583             PIC  X(4)       VALUE '0583'.        EL653
00135      12  ER-0590             PIC  X(4)       VALUE '0590'.        EL653
00136      12  ER-0591             PIC  X(4)       VALUE '0591'.        EL653
00137      12  ER-2039             PIC  X(4)       VALUE '2039'.        EL653
00138      12  ER-2055             PIC  X(4)       VALUE '2055'.        EL653
00139      12  ER-2056             PIC  X(4)       VALUE '2056'.        EL653
00140      12  ER-2067             PIC  X(4)       VALUE '2067'.        EL653
00141      12  ER-2115             PIC  X(4)       VALUE '2115'.        EL653
00142      12  ER-2116             PIC  X(4)       VALUE '2116'.        EL653
00143      12  ER-2117             PIC  X(4)       VALUE '2117'.        EL653
00144      12  ER-2118             PIC  X(4)       VALUE '2118'.        EL653
00145      12  ER-2120             PIC  X(4)       VALUE '2120'.        EL653
00146      12  ER-2121             PIC  X(4)       VALUE '2121'.        EL653
00147      12  ER-2122             PIC  X(4)       VALUE '2122'.        EL653
00148      12  ER-2123             PIC  X(4)       VALUE '2123'.        EL653
00149      12  ER-2124             PIC  X(4)       VALUE '2124'.        EL653
00150      12  ER-2125             PIC  X(4)       VALUE '2125'.        EL653
00151      12  ER-2127             PIC  X(4)       VALUE '2127'.        EL653
00152      12  ER-2128             PIC  X(4)       VALUE '2128'.        EL653
00153      12  ER-2130             PIC  X(4)       VALUE '2130'.        EL653
00154      12  ER-2133             PIC  X(4)       VALUE '2133'.        EL653
00155      12  ER-2135             PIC  X(4)       VALUE '2135'.        EL653
00156  EJECT                                                            EL653
00157                              COPY ELCSCTM.                           CL**6
00158  EJECT                                                            EL653
00159                              COPY ELCSCRTY.                          CL**6
00160  EJECT                                                            EL653
00161                              COPY ELCDATE.                           CL**6
00162  EJECT                                                            EL653
00163                              COPY ELCLOGOF.                          CL**6
00164  EJECT                                                            EL653
00165                              COPY ELCATTR.                           CL**6
00166  EJECT                                                            EL653
00167                              COPY ELCEMIB.                           CL**6
00168  EJECT                                                            EL653
00169                              COPY ELCINTF.                           CL**6
00170      12  PI-WORK-AREA  REDEFINES  PI-PROGRAM-WORK-AREA.           EL653
00171          16  PI-CHECK-MAINT-TYPE     PIC  X.                      EL653
00172              88  VALID-MAINT-TYPE            VALUE 'S' 'A'        EL653
00173                                                    'C' 'D'.       EL653
00174              88  ADD-FUNCTION                VALUE 'A'.           EL653
00175              88  SHOW-FUNCTION               VALUE 'S'.           EL653
00176              88  DELETE-FUNCTION             VALUE 'D'.           EL653
00177              88  CHANGE-FUNCTION             VALUE 'C'.           EL653
00178          16  PI-CHECK-TBLCODE.                                    EL653
00179              20  PI-TBLCODE-1ST-CHAR PIC  X.                      EL653
00180                  88  1ST-CHAR-ALPHA          VALUE 'A' THRU 'Z'.  EL653
00181              20  PI-TBLCODE-REST     PIC  XX.                     EL653
00182          16  PI-CHECK-COVERAGE       PIC  X.                      EL653
00183          16  PI-BROWSE-SW            PIC  X.                      EL653
00184              88  BROWSE-STARTED              VALUE 'Y'.           EL653
00185          16  PI-FIRST-TIME-SW        PIC  X.                      EL653
00186              88  FIRST-TIME                  VALUE 'Y'.           EL653
00187          16  PI-ERCTBL-EOF-SW        PIC  X.                      EL653
00188              88  ERCTBL-EOF                  VALUE 'Y'.           EL653
00189          16  PI-ERCTBL-KEY.                                       EL653
00190              20  PI-ERC-COMPANY-CD   PIC  X.                      EL653
00191              20  PI-ERC-TABLE        PIC  X(3).                   EL653
00192              20  PI-ERC-CNTRL2.                                   EL653
00193                  24  PI-ERC-BEN-TYPE                              EL653
00194                                      PIC  X.                      EL653
00195                  24  PI-ERC-BEN-CODE                              EL653
00196                                      PIC  XX.                     EL653
00197          16  PI-SAVE-ERCTBL-KEY      PIC  X(7).                   EL653
00198          16  FILLER                  PIC  X(618).                    CL**7
00199  EJECT                                                            EL653
00200                              COPY ELCJPFX.                           CL**6
00201                              PIC  X(750).                            CL**5
00202  EJECT                                                            EL653
00203                              COPY ELCAID.                            CL**6
00204                                                                   EL653
00205  01  FILLER  REDEFINES  DFHAID.                                   EL653
00206      12  FILLER              PIC  X(8).                           EL653
00207      12  PF-VALUES           PIC  X          OCCURS 2 TIMES.      EL653
00208  EJECT                                                            EL653
00209                              COPY EL653S.                            CL**6
00210                                                                   EL653
00211  01  EL653AO-R  REDEFINES  EL653AI.                               EL653
101101     12  FILLER                      PIC  X(81).                  EL653
00213      12  SCREEN-TABLE    OCCURS 3 TIMES                           EL653
00214                              INDEXED BY ST-INDX.                  EL653
00215          16  CTBL-TBF-L              PIC S9(4)          COMP.     EL653
00216          16  CTBL-TBF-A              PIC  X.                      EL653
00217          16  CTBL-TBF                PIC S9(11).                     CL**6
00218          16  CTBL-TBF-DISP  REDEFINES                             EL653
00219              CTBL-TBF                PIC ZZZZ,ZZZ.99.                CL**6
00220          16  CTBL-AGE-TABLE  OCCURS 3 TIMES                       EL653
00221                                  INDEXED BY AT-INDX.              EL653
00222              20  CTBL-AGE-L          PIC S9(4)          COMP.     EL653
00223              20  CTBL-AGE-A          PIC  X.                      EL653
00224              20  CTBL-AGE            PIC  99.                     EL653
00225              20  CTBL-RATE-TABLE OCCURS 3 TIMES                   EL653
00226                                      INDEXED BY RT-INDX.          EL653
00227                  24  CTBL-RATE-L     PIC S9(4)          COMP.     EL653
00228                  24  CTBL-RATE-A     PIC  X.                      EL653
00229                  24  CTBL-RATE       PIC X(7).                       CL**3
00230                  24  CTBL-RATE-DISP  REDEFINES                       CL**3
00231                      CTBL-RATE       PIC .99999-.                    CL**3
00232      12  FILLER                      PIC  X(5).                   EL653
00233  EJECT                                                            EL653
00234  LINKAGE SECTION.                                                 EL653
00235                                                                   EL653
00236  01  DFHCOMMAREA             PIC  X(1024).                        EL653
00237                                                                   EL653
00238  EJECT                                                            EL653
00239                              COPY ERCCTBL.                           CL**6
00240  EJECT                                                            EL653
00241                              COPY ELCCNTL.                           CL**6
00242  EJECT                                                            EL653
00243  PROCEDURE DIVISION.                                              EL653
00244      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL653
00245      MOVE '5'                    TO  DC-OPTION-CODE.              EL653
00246                                                                   EL653
00247      PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.             EL653
00248                                                                   EL653
00249      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL653
00250      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL653
00251      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL653
00252                                                                   EL653
00253  1000-START.                                                      EL653
00254      IF EIBCALEN = ZERO                                           EL653
00255          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL653
00256                                                                   EL653
00257      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL653
00258          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL653
00259              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6    EL653
00260              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5    EL653
00261              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4    EL653
00262              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3    EL653
00263              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2    EL653
00264              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1    EL653
00265              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM  EL653
00266              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM    EL653
00267          ELSE                                                     EL653
00268              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM    EL653
00269              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM  EL653
00270              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1    EL653
00271              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2    EL653
00272              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3    EL653
00273              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4    EL653
00274              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5    EL653
00275              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.   EL653
00276                                                                   EL653
00277      EXEC CICS HANDLE CONDITION                                   EL653
00278          NOTOPEN   (9990-ABEND)                                   EL653
00279          NOTFND    (8880-NOT-FOUND)                               EL653
00280          PGMIDERR  (9600-PGMID-ERROR)                             EL653
00281          ERROR     (9990-ABEND)                                   EL653
00282      END-EXEC.                                                    EL653
00283                                                                   EL653
00284      IF EIBTRNID NOT = TRANS-ID                                   EL653
00285          MOVE 'Y'                TO  PI-FIRST-TIME-SW             EL653
00286          MOVE LOW-VALUES         TO  EL653AI                      EL653
00287          GO TO 8100-SEND-INITIAL-MAP.                             EL653
00288                                                                   EL653
00289      IF EIBAID = DFHCLEAR                                         EL653
00290          GO TO 9400-CLEAR.                                        EL653
00291                                                                   EL653
00292      IF PI-PROCESSOR-ID = 'LGXX'                                  EL653
00293          GO TO 2000-RECEIVE.                                      EL653
00294                                                                   EL653
00295      EXEC CICS  READQ TS                                          EL653
00296          QUEUE   (PI-SECURITY-TEMP-STORE-ID)                      EL653
00297          INTO    (SECURITY-CONTROL)                               EL653
00298          LENGTH  (SC-COMM-LENGTH)                                 EL653
00299          ITEM    (SC-ITEM)                                        EL653
00300      END-EXEC.                                                    EL653
00301                                                                   EL653
00302      MOVE SC-CREDIT-DISPLAY (08)  TO  PI-DISPLAY-CAP.             EL653
00303      MOVE SC-CREDIT-UPDATE  (08)  TO  PI-MODIFY-CAP.              EL653
00304                                                                   EL653
00305      IF NOT DISPLAY-CAP                                           EL653
00306          MOVE 'READ'          TO SM-READ                          EL653
00307          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT           EL653
00308          MOVE ER-0070        TO  EMI-ERROR                        EL653
00309          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL653
00310          GO TO 8100-SEND-INITIAL-MAP.                             EL653
00311                                                                   EL653
00312  EJECT                                                            EL653
00313  2000-RECEIVE.                                                    EL653
00314      MOVE LOW-VALUES             TO  EL653AI.                     EL653
00315                                                                   EL653
00316      IF EIBAID = DFHPA1 OR  DFHPA2  OR  DFHPA3                    EL653
00317          MOVE ER-0008            TO  EMI-ERROR                    EL653
00318          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL653
00319          MOVE -1                 TO  MAINTYPL                     EL653
00320          GO TO 8200-SEND-DATAONLY.                                EL653
00321                                                                   EL653
00322      EXEC CICS RECEIVE                                            EL653
00323          MAP     (MAP-NAME)                                       EL653
00324          MAPSET  (MAPSET-NAME)                                    EL653
00325          INTO    (EL653AI)                                        EL653
00326      END-EXEC.                                                    EL653
00327                                                                   EL653
00328      IF PFENTERL = ZERO                                           EL653
00329          GO TO 3000-CHECK-PFENTERS.                               EL653
00330                                                                   EL653
00331      IF EIBAID NOT = DFHENTER                                     EL653
00332          MOVE ER-0004            TO  EMI-ERROR                    EL653
00333          GO TO 3100-INPUT-ERROR.                                  EL653
00334                                                                   EL653
00335      IF PFENTERI GREATER 0 AND LESS 25                            EL653
00336          MOVE PF-VALUES (PFENTERI)  TO  EIBAID                    EL653
00337      ELSE                                                         EL653
00338          MOVE ER-0029               TO  EMI-ERROR                 EL653
00339          GO TO 3100-INPUT-ERROR.                                  EL653
00340  EJECT                                                            EL653
00341  3000-CHECK-PFENTERS.                                             EL653
00342      IF EIBAID = DFHPF23                                          EL653
00343          GO TO 8810-PF23.                                         EL653
00344                                                                   EL653
00345      IF EIBAID = DFHPF24                                          EL653
00346          GO TO 9200-RETURN-MAIN-MENU.                             EL653
00347                                                                   EL653
00348      IF EIBAID = DFHPF12                                          EL653
00349          GO TO 9500-PF12.                                         EL653
00350                                                                   EL653
00351      IF MAINTYPL GREATER  ZERO                                    EL653
00352          IF MAINTYPI NOT = SPACE                                  EL653
00353              IF EIBAID NOT = DFHENTER                             EL653
00354                  MOVE ER-0050    TO  EMI-ERROR                    EL653
00355                  GO TO 3100-INPUT-ERROR.                          EL653
00356                                                                   EL653
00357      IF EIBAID = DFHPF1                                           EL653
00358          GO TO 7300-PAGE-TABLE-FORWARD.                           EL653
00359                                                                   EL653
00360      IF EIBAID = DFHPF2                                           EL653
00361          GO TO 7400-PAGE-TABLE-BACKWARD.                          EL653
00362                                                                   EL653
00363      IF EIBAID = DFHPF3                                           EL653
00364          GO TO 7500-PAGE-BENEFIT-FORWARD.                         EL653
00365                                                                   EL653
00366      IF EIBAID = DFHPF4                                           EL653
00367          GO TO 7600-PAGE-BENEFIT-BACKWARD.                        EL653
00368                                                                   EL653
00369      IF EIBAID = DFHENTER                                         EL653
00370          GO TO 4000-EDIT-MAINT.                                   EL653
00371                                                                   EL653
00372      MOVE ER-0029                TO  EMI-ERROR.                   EL653
00373                                                                   EL653
00374  3100-INPUT-ERROR.                                                EL653
00375      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL653
00376                                                                   EL653
00377      MOVE AL-UNBON               TO  PFENTERA.                    EL653
00378      MOVE -1                     TO  PFENTERL.                    EL653
00379                                                                   EL653
00380      GO TO 8200-SEND-DATAONLY.                                    EL653
00381  EJECT                                                            EL653
00382  4000-EDIT-MAINT.                                                 EL653
00383      IF MAINTYPL GREATER ZERO                                     EL653
00384          MOVE MAINTYPI           TO  PI-CHECK-MAINT-TYPE          EL653
00385          IF VALID-MAINT-TYPE                                      EL653
00386              MOVE AL-UANON       TO  MAINTYPA                     EL653
00387          ELSE                                                     EL653
00388              MOVE -1             TO  MAINTYPL                     EL653
00389              MOVE AL-UABON       TO  MAINTYPA                     EL653
00390              MOVE ER-2039        TO  EMI-ERROR                    EL653
00391              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL653
00392      ELSE                                                         EL653
00393          MOVE -1                 TO  MAINTYPL                     EL653
00394          MOVE AL-UABON           TO  MAINTYPA                     EL653
00395          MOVE ER-2039            TO  EMI-ERROR                    EL653
00396          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL653
00397                                                                   EL653
00398      IF NOT MODIFY-CAP                                            EL653
00399          IF SHOW-FUNCTION                                         EL653
00400              NEXT SENTENCE                                        EL653
00401          ELSE                                                     EL653
00402              MOVE 'UPDATE'       TO SM-READ                       EL653
00403              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT       EL653
00404              MOVE ER-0070        TO  EMI-ERROR                    EL653
00405              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL653
00406              GO TO 8100-SEND-INITIAL-MAP.                         EL653
00407                                                                   EL653
00408      IF TBLCODEL GREATER ZERO                                     EL653
00409          MOVE TBLCODEI           TO  PI-CHECK-TBLCODE             EL653
00410          IF 1ST-CHAR-ALPHA                                        EL653
00411              MOVE TBLCODEI       TO  PI-ERC-TABLE                 EL653
00412              MOVE AL-UANON       TO  TBLCODEA                     EL653
00413          ELSE                                                     EL653
00414              MOVE -1             TO  TBLCODEL                     EL653
00415              MOVE AL-UABON       TO  TBLCODEA                     EL653
00416              MOVE ER-2115        TO  EMI-ERROR                    EL653
00417              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL653
00418      ELSE                                                         EL653
00419          MOVE -1                 TO  TBLCODEL                     EL653
00420          MOVE AL-UABON           TO  TBLCODEA                     EL653
00421          MOVE ER-2116            TO  EMI-ERROR                    EL653
00422          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL653
00423                                                                   EL653
00424      IF COVTYPEL GREATER ZERO                                     EL653
00425          MOVE COVTYPEI           TO  PI-CHECK-COVERAGE            EL653
00426          IF PI-CHECK-COVERAGE = PI-LIFE-OVERRIDE-L1               EL653
00427            OR  PI-CHECK-COVERAGE = PI-AH-OVERRIDE-L1              EL653
00428              MOVE COVTYPEI       TO  PI-ERC-BEN-TYPE              EL653
00429              MOVE AL-UANON       TO  COVTYPEA                     EL653
00430          ELSE                                                     EL653
00431              MOVE -1             TO  COVTYPEL                     EL653
00432              MOVE AL-UABON       TO  COVTYPEA                     EL653
00433              MOVE ER-2117        TO  EMI-ERROR                    EL653
00434              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL653
00435      ELSE                                                         EL653
00436          MOVE -1                 TO  COVTYPEL                     EL653
00437          MOVE AL-UABON           TO  COVTYPEA                     EL653
00438          MOVE ER-2117            TO  EMI-ERROR                    EL653
00439          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL653
00440                                                                   EL653
00441      IF BENCODEL GREATER ZERO                                     EL653
00442          MOVE BENCODEI           TO  PI-ERC-BEN-CODE              EL653
00443          MOVE AL-UANON           TO  BENCODEA                     EL653
00444      ELSE                                                         EL653
00445          MOVE -1                 TO  BENCODEL                     EL653
00446          MOVE AL-UABON           TO  BENCODEA                     EL653
00447          MOVE ER-2118            TO  EMI-ERROR                    EL653
00448          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL653
00449                                                                   EL653
00450      IF EMI-NO-ERRORS                                             EL653
00451          NEXT SENTENCE                                            EL653
00452      ELSE                                                         EL653
00453          GO TO 8200-SEND-DATAONLY.                                EL653
00454                                                                   EL653
00455      IF CHANGE-FUNCTION                                           EL653
00456          GO TO 4400-CHANGE.                                       EL653
00457                                                                   EL653
00458      IF DELETE-FUNCTION                                           EL653
00459          GO TO 4600-DELETE.                                       EL653
00460                                                                   EL653
00461      IF SHOW-FUNCTION                                             EL653
00462          GO TO 5000-BUILD-INITIAL-SCREEN.                         EL653
00463                                                                   EL653
00464      IF ADD-FUNCTION                                              EL653
00465          GO TO 4200-ADD.                                          EL653
00466                                                                   EL653
00467  4000-EXIT.                                                       EL653
00468      EXIT.                                                        EL653
00469  EJECT                                                            EL653
00470  4200-ADD.                                                        EL653
00471      PERFORM 7000-EDIT  THRU  7049-EXIT.                          EL653
00472                                                                   EL653
00473      IF EMI-NO-ERRORS                                             EL653
00474          NEXT SENTENCE                                            EL653
00475      ELSE                                                         EL653
00476          GO TO 8200-SEND-DATAONLY.                                EL653
00477                                                                   EL653
00478      EXEC CICS HANDLE CONDITION                                   EL653
00479          NOTOPEN  (9990-ABEND)                                    EL653
00480          NOTFND   (4250-CONT)                                     EL653
00481          END-EXEC.                                                EL653
00482                                                                   EL653
00483      PERFORM 7050-READ-ERCTBL  THRU  7050-EXIT.                   EL653
00484                                                                   EL653
00485      MOVE ER-2133                TO  EMI-ERROR.                   EL653
00486                                                                   EL653
00487      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL653
00488                                                                   EL653
00489      MOVE -1                     TO  MAINTYPL.                    EL653
00490                                                                   EL653
00491      GO TO 8200-SEND-DATAONLY.                                    EL653
00492                                                                   EL653
00493  4250-CONT.                                                       EL653
00494      PERFORM 7150-ERCTBL-GETMAIN  THRU  7150-EXIT.                EL653
00495                                                                   EL653
00496      MOVE SPACES                 TO  COMM-TABLE-RECORD.              CL**2
00497      MOVE 'CT'                   TO  CT-RECORD-ID.                EL653
00498      MOVE PI-COMPANY-CD          TO  CT-COMPANY-CD.               EL653
00499      MOVE ZEROS                  TO  CT-LAST-MAINT-HHMMSS.        EL653
00500      MOVE +99                    TO  CT-AGE (1)                   EL653
00501                                      CT-AGE (2)                   EL653
00502                                      CT-AGE (3).                  EL653
00503      MOVE +999                   TO  CT-TRM (1)                   EL653
00504                                      CT-TRM (2)                   EL653
00505                                      CT-TRM (3).                  EL653
00506      MOVE WS-MAX-BENEFIT         TO  CT-TBF (1)                   EL653
00507                                      CT-TBF (2)                   EL653
00508                                      CT-TBF (3).                  EL653
00509      MOVE +1                     TO  SUB1.                        EL653
00510                                                                   EL653
00511  4275-ZERO-RATES.                                                 EL653
00512      IF SUB1 GREATER +27                                          EL653
00513          NEXT SENTENCE                                            EL653
00514      ELSE                                                         EL653
00515          MOVE ZEROS              TO  CT-RT (SUB1)                 EL653
00516          ADD +1                  TO  SUB1                         EL653
00517          GO TO 4275-ZERO-RATES.                                   EL653
00518                                                                   EL653
00519      PERFORM 6000-CHECK-FOR-UPDATE  THRU  6099-EXIT.              EL653
00520                                                                   EL653
00521      MOVE PI-PROCESSOR-ID        TO  CT-LAST-MAINT-USER.          EL653
00522      MOVE EIBTIME                TO  CT-LAST-MAINT-HHMMSS.        EL653
00523      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL653
00524      MOVE '5'                    TO  DC-OPTION-CODE.              EL653
00525      MOVE LINK-CLDATCV           TO  PGM-NAME.                    EL653
00526                                                                   EL653
00527      EXEC CICS LINK                                               EL653
00528          PROGRAM   (PGM-NAME)                                     EL653
00529          COMMAREA  (DATE-CONVERSION-DATA)                         EL653
00530          LENGTH    (DC-COMM-LENGTH)                               EL653
00531      END-EXEC.                                                    EL653
00532                                                                   EL653
00533      MOVE DC-BIN-DATE-1          TO  CT-LAST-MAINT-DT             EL653
00534                                      BIN-CURRENT-SAVE.            EL653
00535      MOVE PI-COMPANY-CD          TO  CT-COMPANY-CD.               EL653
00536      MOVE 'CT'                   TO  CT-RECORD-ID.                EL653
00537      MOVE CTBL-FILE-ID           TO  FILE-ID.                     EL653
00538      MOVE 'A'                    TO  JP-RECORD-TYPE.              EL653
00539      MOVE COMM-TABLE-RECORD      TO  JP-RECORD-AREA.                 CL**2
00540                                                                   EL653
00541      EXEC CICS WRITE                                              EL653
00542          DATASET  (CTBL-FILE-ID)                                  EL653
00543          FROM     (COMM-TABLE-RECORD)                                CL**2
00544          RIDFLD   (CT-CONTROL-PRIMARY)                            EL653
00545      END-EXEC.                                                    EL653
00546                                                                   EL653
00547      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL653
00548                                                                   EL653
00549      PERFORM 8000-UPDATE-MAINT-DATE  THRU  8000-EXIT.             EL653
00550                                                                   EL653
00551      MOVE ER-0000                TO  EMI-ERROR.                   EL653
00552                                                                   EL653
00553      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL653
00554                                                                   EL653
00555      MOVE LOW-VALUES             TO  EL653AO.                     EL653
00556      MOVE PI-ERC-TABLE           TO  TBLCODEO.                    EL653
00557      MOVE PI-ERC-BEN-TYPE        TO  COVTYPEO.                    EL653
00558      MOVE PI-ERC-BEN-CODE        TO  BENCODEO.                    EL653
00559      MOVE AL-UANON               TO  TBLCODEA                     EL653
00560                                      COVTYPEA                     EL653
00561                                      BENCODEA.                    EL653
00562                                                                   EL653
00563      GO TO 8100-SEND-INITIAL-MAP.                                 EL653
00564                                                                   EL653
00565  4299-EXIT.                                                       EL653
00566      EXIT.                                                        EL653
00567  EJECT                                                            EL653
00568  4400-CHANGE.                                                     EL653
00569      IF PI-ERCTBL-KEY = PI-SAVE-ERCTBL-KEY                        EL653
00570          NEXT SENTENCE                                            EL653
00571      ELSE                                                         EL653
00572          MOVE ER-2056            TO  EMI-ERROR                    EL653
00573          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL653
00574          MOVE -1                 TO  MAINTYPL                     EL653
00575          GO TO 8200-SEND-DATAONLY.                                EL653
00576                                                                   EL653
00577      PERFORM 7000-EDIT  THRU  7049-EXIT.                          EL653
00578                                                                   EL653
00579      IF EMI-NO-ERRORS                                             EL653
00580          NEXT SENTENCE                                            EL653
00581      ELSE                                                         EL653
00582          GO TO 8200-SEND-DATAONLY.                                EL653
00583                                                                   EL653
00584      PERFORM 7200-READ-ERCTBL-UPDATE  THRU  7200-EXIT.            EL653
00585                                                                   EL653
00586      MOVE COMM-TABLE-RECORD      TO  JP-RECORD-AREA.                 CL**2
00587                                                                   EL653
00588      PERFORM 6000-CHECK-FOR-UPDATE  THRU  6099-EXIT.              EL653
00589                                                                   EL653
00590      IF CT-LAST-MAINT-USER   = PI-UPDATE-BY   OR                  EL653
00591         CT-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS                   EL653
00592          NEXT SENTENCE                                            EL653
00593      ELSE                                                         EL653
00594          EXEC CICS UNLOCK                                         EL653
00595               DATASET  (CTBL-FILE-ID)                             EL653
00596          END-EXEC                                                 EL653
00597          MOVE ER-0068            TO  EMI-ERROR                    EL653
00598          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL653
00599          GO TO 8200-SEND-DATAONLY.                                EL653
00600                                                                   EL653
00601      MOVE PI-PROCESSOR-ID        TO  CT-LAST-MAINT-USER.          EL653
00602      MOVE EIBTIME                TO  CT-LAST-MAINT-HHMMSS.        EL653
00603      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL653
00604      MOVE '5'                    TO  DC-OPTION-CODE.              EL653
00605      MOVE LINK-CLDATCV           TO  PGM-NAME.                    EL653
00606                                                                   EL653
00607      EXEC CICS LINK                                               EL653
00608          PROGRAM   (PGM-NAME)                                     EL653
00609          COMMAREA  (DATE-CONVERSION-DATA)                         EL653
00610          LENGTH    (DC-COMM-LENGTH)                               EL653
00611      END-EXEC.                                                    EL653
00612                                                                   EL653
00613      MOVE DC-BIN-DATE-1          TO  CT-LAST-MAINT-DT             EL653
00614                                      BIN-CURRENT-SAVE.            EL653
00615      MOVE 'B'                    TO  JP-RECORD-TYPE.              EL653
00616      MOVE CTBL-FILE-ID           TO  FILE-ID.                     EL653
00617                                                                   EL653
00618      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL653
00619                                                                   EL653
00620      MOVE COMM-TABLE-RECORD      TO  JP-RECORD-AREA.                 CL**2
00621                                                                   EL653
00622      EXEC CICS REWRITE                                            EL653
00623          DATASET  (CTBL-FILE-ID)                                  EL653
00624          FROM     (COMM-TABLE-RECORD)                                CL**2
00625      END-EXEC.                                                    EL653
00626                                                                   EL653
00627      MOVE 'C'                    TO  JP-RECORD-TYPE.              EL653
00628      MOVE CTBL-FILE-ID           TO  FILE-ID.                     EL653
00629                                                                   EL653
00630      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL653
00631                                                                   EL653
00632      PERFORM 8000-UPDATE-MAINT-DATE  THRU  8000-EXIT.             EL653
00633                                                                   EL653
00634      MOVE ER-0000                TO  EMI-ERROR.                   EL653
00635                                                                   EL653
00636      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL653
00637                                                                   EL653
00638      MOVE LOW-VALUES             TO  EL653AO.                     EL653
00639      MOVE PI-ERC-TABLE           TO  TBLCODEO.                    EL653
00640      MOVE PI-ERC-BEN-TYPE        TO  COVTYPEO.                    EL653
00641      MOVE PI-ERC-BEN-CODE        TO  BENCODEO.                    EL653
00642      MOVE AL-UANON               TO  TBLCODEA                     EL653
00643                                      COVTYPEA                     EL653
00644                                      BENCODEA.                    EL653
00645                                                                   EL653
00646      GO TO 5000-BUILD-INITIAL-SCREEN.                                CL**4
00647                                                                   EL653
00648  4400-EXIT.                                                       EL653
00649      EXIT.                                                        EL653
00650  EJECT                                                            EL653
00651  4600-DELETE.                                                     EL653
00652      IF PI-ERCTBL-KEY = PI-SAVE-ERCTBL-KEY                        EL653
00653          NEXT SENTENCE                                            EL653
00654      ELSE                                                         EL653
00655          MOVE ER-2056            TO  EMI-ERROR                    EL653
00656          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL653
00657          MOVE -1                 TO  MAINTYPL                     EL653
00658          GO TO 8200-SEND-DATAONLY.                                EL653
00659                                                                   EL653
00660      PERFORM 7200-READ-ERCTBL-UPDATE  THRU  7200-EXIT.            EL653
00661                                                                   EL653
00662      MOVE COMM-TABLE-RECORD      TO  JP-RECORD-AREA.                 CL**2
00663                                                                   EL653
00664      IF CT-LAST-MAINT-USER   = PI-UPDATE-BY   OR                  EL653
00665         CT-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS                   EL653
00666          NEXT SENTENCE                                            EL653
00667      ELSE                                                         EL653
00668          EXEC CICS UNLOCK                                         EL653
00669               DATASET  (CTBL-FILE-ID)                             EL653
00670          END-EXEC                                                 EL653
00671          MOVE ER-0068            TO  EMI-ERROR                    EL653
00672          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL653
00673          GO TO 8200-SEND-DATAONLY.                                EL653
00674                                                                   EL653
00675      EXEC CICS DELETE                                             EL653
00676           DATASET  (CTBL-FILE-ID)                                 EL653
00677      END-EXEC.                                                    EL653
00678                                                                   EL653
00679      MOVE 'D'                    TO  JP-RECORD-TYPE.              EL653
00680      MOVE CTBL-FILE-ID           TO  FILE-ID.                     EL653
00681                                                                   EL653
00682      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL653
00683                                                                   EL653
00684      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL653
00685      MOVE '5'                    TO  DC-OPTION-CODE.              EL653
00686      MOVE LINK-CLDATCV           TO  PGM-NAME.                    EL653
00687                                                                   EL653
00688      EXEC CICS LINK                                               EL653
00689          PROGRAM   (PGM-NAME)                                     EL653
00690          COMMAREA  (DATE-CONVERSION-DATA)                         EL653
00691          LENGTH    (DC-COMM-LENGTH)                               EL653
00692      END-EXEC.                                                    EL653
00693                                                                   EL653
00694      MOVE DC-BIN-DATE-1          TO  BIN-CURRENT-SAVE.            EL653
00695                                                                   EL653
00696      PERFORM 8000-UPDATE-MAINT-DATE  THRU  8000-EXIT.             EL653
00697                                                                   EL653
00698      MOVE ER-0000                TO  EMI-ERROR.                   EL653
00699                                                                   EL653
00700      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL653
00701                                                                   EL653
00702      MOVE LOW-VALUES             TO  EL653AO.                     EL653
00703      MOVE PI-ERC-TABLE           TO  TBLCODEO.                    EL653
00704      MOVE PI-ERC-BEN-TYPE        TO  COVTYPEO.                    EL653
00705      MOVE PI-ERC-BEN-CODE        TO  BENCODEO.                    EL653
00706      MOVE AL-UANON               TO  TBLCODEA                     EL653
00707                                      COVTYPEA                     EL653
00708                                      BENCODEA.                    EL653
00709                                                                   EL653
00710      GO TO 8100-SEND-INITIAL-MAP.                                 EL653
00711                                                                   EL653
00712  4600-EXIT.                                                       EL653
00713      EXIT.                                                        EL653
00714  EJECT                                                            EL653
00715  5000-BUILD-INITIAL-SCREEN.                                       EL653
00716      MOVE LOW-VALUES             TO  EL653AO.                     EL653
00717      MOVE PI-ERC-TABLE           TO  TBLCODEO.                    EL653
00718      MOVE PI-ERC-BEN-TYPE        TO  COVTYPEO.                    EL653
00719      MOVE PI-ERC-BEN-CODE        TO  BENCODEO.                    EL653
00720      MOVE AL-UANON               TO  MAINTYPA                     EL653
00721                                      TBLCODEA                     EL653
00722                                      COVTYPEA                     EL653
00723                                      BENCODEA.                    EL653
00724      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.           EL653
00725                                                                   EL653
00726      PERFORM 7050-READ-ERCTBL  THRU  7050-EXIT.                   EL653
00727                                                                   EL653
00728      MOVE CT-LAST-MAINT-USER     TO  PI-UPDATE-BY.                EL653
00729      MOVE CT-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.            EL653
00730                                                                   EL653
00731  5005-SET-UP-SCREEN.                                              EL653
00732      MOVE CT-TRM (1)             TO  TERM1O.                      EL653
00733      MOVE CT-TRM (2)             TO  TERM2O.                      EL653
00734      MOVE CT-TRM (3)             TO  TERM3O.                      EL653
00735      MOVE AL-UNNON               TO  TERM1A                       EL653
00736                                      TERM2A                       EL653
00737                                      TERM3A.                      EL653
00738                                                                   EL653
00739      SET ST-INDX                 TO  +1.                          EL653
00740      SET AT-INDX                 TO  +1.                          EL653
00741      SET RT-INDX                 TO  +1.                          EL653
00742                                                                   EL653
00743      MOVE +1                     TO  SUB1                         EL653
00744                                      SUB2                         EL653
00745                                      SUB3.                        EL653
00746                                                                   EL653
00747  5010-SET-UP-BENEFIT.                                             EL653
00748      IF SUB1 GREATER +3                                           EL653
00749          MOVE -1                 TO  MAINTYPL                     EL653
00750          GO TO 8100-SEND-INITIAL-MAP.                             EL653
00751                                                                   EL653
00752      MOVE CT-TBF (SUB1)          TO  CTBL-TBF-DISP (ST-INDX)      EL653
00753      MOVE AL-UNNON               TO  CTBL-TBF-A (ST-INDX).        EL653
00754                                                                   EL653
00755  5020-SET-UP-AGES.                                                EL653
00756      IF SUB2 GREATER +3                                           EL653
00757          SET ST-INDX  UP  BY  +1                                  EL653
00758          SET AT-INDX             TO  +1                           EL653
00759          ADD  +1                 TO  SUB1                         EL653
00760          MOVE +1                 TO  SUB2                         EL653
00761          GO TO 5010-SET-UP-BENEFIT.                               EL653
00762                                                                   EL653
00763      MOVE CT-AGE (SUB2)          TO  CTBL-AGE (ST-INDX AT-INDX)   EL653
00764                                                                   EL653
00765      IF SUB1 = +1                                                 EL653
00766          MOVE AL-UNNON       TO  CTBL-AGE-A (ST-INDX AT-INDX)     EL653
00767      ELSE                                                         EL653
00768          MOVE AL-SANOF       TO  CTBL-AGE-A (ST-INDX AT-INDX).    EL653
00769                                                                   EL653
00770  5030-SET-UP-RATES.                                               EL653
00771      IF RT-INDX GREATER +3                                        EL653
00772          ADD +1                  TO  SUB2                         EL653
00773          SET AT-INDX  UP  BY  +1                                  EL653
00774          SET RT-INDX             TO  +1                           EL653
00775          GO TO 5020-SET-UP-AGES.                                  EL653
00776                                                                   EL653
00777      IF CT-RT (SUB3) = ZEROS                                      EL653
00778          NEXT SENTENCE                                            EL653
00779      ELSE                                                         EL653
00780          MOVE CT-RT (SUB3)                                        EL653
00781              TO  CTBL-RATE-DISP (ST-INDX AT-INDX RT-INDX)            CL**3
00782          MOVE AL-UNNON                                            EL653
00783              TO  CTBL-RATE-A (ST-INDX AT-INDX RT-INDX).           EL653
00784                                                                   EL653
00785      ADD +1                      TO  SUB3.                        EL653
00786                                                                   EL653
00787      SET RT-INDX  UP  BY  +1.                                     EL653
00788                                                                   EL653
00789      GO TO 5030-SET-UP-RATES.                                     EL653
00790                                                                   EL653
00791  5099-EXIT.                                                       EL653
00792      EXIT.                                                        EL653
00793  EJECT                                                            EL653
00794  6000-CHECK-FOR-UPDATE.                                           EL653
00795       IF CHANGE-FUNCTION                                          EL653
00796           GO TO 6010-CONT.                                        EL653
00797                                                                   EL653
00798       IF TBLCODEL GREATER ZERO                                    EL653
00799           MOVE TBLCODEI          TO  CT-TABLE.                    EL653
00800                                                                   EL653
00801       IF COVTYPEL GREATER ZERO                                    EL653
00802           MOVE COVTYPEI          TO  CT-BEN-TYPE.                 EL653
00803                                                                   EL653
00804       IF BENCODEL GREATER ZERO                                    EL653
00805           MOVE BENCODEI          TO  CT-BEN-CODE.                 EL653
00806                                                                   EL653
00807  6010-CONT.                                                       EL653
00808      IF TERM1L GREATER ZERO                                       EL653
00809          MOVE TERM1I             TO  CT-TRM (1).                  EL653
00810                                                                   EL653
00811      IF TERM2L GREATER ZERO                                       EL653
00812          MOVE TERM2I             TO  CT-TRM (2).                  EL653
00813                                                                   EL653
00814      IF TERM3L GREATER ZERO                                       EL653
00815          MOVE TERM3I             TO  CT-TRM (3).                  EL653
00816                                                                   EL653
00817      SET ST-INDX                 TO  +1.                          EL653
00818      SET AT-INDX                 TO  +1.                          EL653
00819      SET RT-INDX                 TO  +1.                          EL653
00820                                                                   EL653
00821      MOVE +1                     TO  SUB1                         EL653
00822                                      SUB2                         EL653
00823                                      SUB3.                        EL653
00824                                                                   EL653
00825  6010-PROCESS.                                                    EL653
00826      IF ST-INDX GREATER +3                                        EL653
00827          GO TO 6099-EXIT.                                         EL653
00828                                                                   EL653
00829      IF CTBL-TBF-L (ST-INDX) GREATER ZERO                         EL653
00830          MOVE CTBL-TBF (ST-INDX)  TO  DEEDIT-FIELD                EL653
00831          PERFORM 7100-DEEDIT  THRU  7100-EXIT                     EL653
00832          MOVE DEEDIT-FIELD-V1     TO  CT-TBF (SUB1).              EL653
00833                                                                   EL653
00834  6020-UPDATE-AGES.                                                EL653
00835      IF AT-INDX GREATER +3                                        EL653
00836          SET ST-INDX  UP  BY  +1                                  EL653
00837          SET AT-INDX             TO  +1                           EL653
00838          ADD +1                  TO  SUB1                         EL653
00839          MOVE +1                 TO  SUB2                         EL653
00840          GO TO 6010-PROCESS.                                      EL653
00841                                                                   EL653
00842      IF CTBL-AGE-L (ST-INDX AT-INDX) GREATER ZERO                 EL653
00843          MOVE CTBL-AGE (ST-INDX AT-INDX)  TO  CT-AGE (SUB2).      EL653
00844                                                                   EL653
00845  6030-UPDATE-RATES.                                               EL653
00846      IF RT-INDX GREATER +3                                        EL653
00847          SET AT-INDX  UP  BY  +1                                  EL653
00848          ADD +1                  TO  SUB2                         EL653
00849          SET RT-INDX             TO  +1                           EL653
00850          GO TO 6020-UPDATE-AGES.                                  EL653
00851                                                                   EL653
00852      IF CTBL-RATE-L (ST-INDX AT-INDX RT-INDX) GREATER ZERO        EL653
00853          MOVE SPACES             TO  DEEDIT-FIELD                    CL**3
00854          MOVE CTBL-RATE (ST-INDX AT-INDX RT-INDX)                 EL653
00855              TO  DEEDIT-FIELD-X7                                     CL**3
00856          PERFORM 7100-DEEDIT  THRU  7100-EXIT                        CL**3
00857          MOVE DEEDIT-FIELD-V5    TO  CT-RT (SUB3).                   CL**3
00858                                                                   EL653
00859      SET RT-INDX  UP  BY  +1.                                     EL653
00860                                                                   EL653
00861      ADD +1                      TO  SUB3.                        EL653
00862                                                                   EL653
00863      GO TO 6030-UPDATE-RATES.                                     EL653
00864                                                                   EL653
00865  6099-EXIT.                                                       EL653
00866      EXIT.                                                        EL653
00867  EJECT                                                            EL653
00868  7000-EDIT.                                                       EL653
00869      IF TERM1L GREATER ZERO                                       EL653
00870          IF TERM1I NUMERIC                                        EL653
00871              IF TERM1I GREATER WS-PREV-TERM  OR                   EL653
00872                 TERM1I = 000 OR  999                              EL653
00873                  MOVE TERM1I     TO  WS-PREV-TERM                 EL653
00874                  MOVE AL-UNNON   TO  TERM1A                       EL653
00875              ELSE                                                 EL653
00876                  MOVE -1         TO  TERM1L                       EL653
00877                  MOVE AL-UNBON   TO  TERM1A                       EL653
00878                  MOVE ER-2123    TO  EMI-ERROR                    EL653
00879                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT       EL653
00880          ELSE                                                     EL653
00881              MOVE -1             TO  TERM1L                       EL653
00882              MOVE AL-UNBON       TO  TERM1A                       EL653
00883              MOVE ER-2120        TO  EMI-ERROR                    EL653
00884              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL653
00885                                                                   EL653
00886      IF TERM2L GREATER ZERO                                       EL653
00887          IF TERM2I NUMERIC                                        EL653
00888              IF TERM2I GREATER WS-PREV-TERM  OR                   EL653
00889                 TERM2I = 999                                      EL653
00890                  MOVE TERM2I     TO  WS-PREV-TERM                 EL653
00891                  MOVE AL-UNNON   TO  TERM2A                       EL653
00892              ELSE                                                 EL653
00893                  MOVE -1         TO  TERM2L                       EL653
00894                  MOVE AL-UNBON   TO  TERM2A                       EL653
00895                  MOVE ER-2123    TO  EMI-ERROR                    EL653
00896                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT       EL653
00897          ELSE                                                     EL653
00898              MOVE -1             TO  TERM2L                       EL653
00899              MOVE AL-UNBON       TO  TERM2A                       EL653
00900              MOVE ER-2121        TO  EMI-ERROR                    EL653
00901              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL653
00902                                                                   EL653
00903      IF TERM3L GREATER ZERO                                       EL653
00904          IF TERM3I NUMERIC                                        EL653
00905              IF TERM3I GREATER WS-PREV-TERM  OR                   EL653
00906                 TERM3I = 999                                      EL653
00907                  MOVE TERM3I     TO  WS-PREV-TERM                 EL653
00908                  MOVE AL-UNNON   TO  TERM3A                       EL653
00909              ELSE                                                 EL653
00910                  MOVE -1         TO  TERM3L                       EL653
00911                  MOVE AL-UNBON   TO  TERM3A                       EL653
00912                  MOVE ER-2123    TO  EMI-ERROR                    EL653
00913                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT       EL653
00914          ELSE                                                     EL653
00915              MOVE -1             TO  TERM3L                       EL653
00916              MOVE AL-UNBON       TO  TERM3A                       EL653
00917              MOVE ER-2122        TO  EMI-ERROR                    EL653
00918              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL653
00919                                                                   EL653
00920      SET ST-INDX                 TO  +1.                          EL653
00921      SET AT-INDX                 TO  +1.                          EL653
00922      SET RT-INDX                 TO  +1.                          EL653
00923                                                                   EL653
00924  7010-PROCESS.                                                    EL653
00925      IF ST-INDX GREATER +3                                        EL653
00926          GO TO 7049-EXIT.                                         EL653
00927                                                                   EL653
00928      IF CTBL-TBF-L (ST-INDX) GREATER ZERO                         EL653
00929          MOVE CTBL-TBF (ST-INDX)  TO  DEEDIT-FIELD                EL653
00930          IF CTBL-TBF (ST-INDX) NUMERIC                            EL653
00931              IF CTBL-TBF (ST-INDX) = ZEROS                        EL653
00932                  MOVE AL-UNBON    TO  CTBL-TBF-A (ST-INDX)        EL653
00933                  MOVE -1          TO  CTBL-TBF-L (ST-INDX)        EL653
00934                  MOVE ER-2124     TO  EMI-ERROR                   EL653
00935                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT       EL653
00936              END-IF                                               EL653
00938          END-IF                                                   EL653
00940      ELSE                                                         EL653
00941          GO TO 7020-EDIT-AGE.                                     EL653
00942                                                                   EL653
00943      PERFORM 7100-DEEDIT  THRU  7100-EXIT.                        EL653
00944                                                                   EL653
00945      IF DEEDIT-FIELD-V1 GREATER ZERO                              EL653
00946          MOVE DEEDIT-FIELD-V1    TO  WS-BENEFIT                   EL653
CIDMOD         IF WS-BENEFIT = +0999999.99
CIDMOD             MOVE +9999999.99    TO WS-BENEFIT
CIDMOD         END-IF
00947          IF WS-BENEFIT GREATER WS-PREV-BENEFIT OR                 EL653
00948             WS-BENEFIT = WS-MAX-BENEFIT                           EL653
00949              MOVE AL-UNNON       TO  CTBL-TBF-A (ST-INDX)         EL653
00950              MOVE WS-BENEFIT     TO  CTBL-TBF-DISP (ST-INDX)      EL653
00951                                      WS-PREV-BENEFIT              EL653
00952          ELSE                                                     EL653
00953              MOVE WS-BENEFIT     TO  CTBL-TBF-DISP (ST-INDX)      EL653
00954              MOVE AL-UNBON       TO  CTBL-TBF-A (ST-INDX)         EL653
00955              MOVE -1             TO  CTBL-TBF-L (ST-INDX)         EL653
00956              MOVE ER-2125        TO  EMI-ERROR                    EL653
00957              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL653
CIDMOD         END-IF
00958      ELSE                                                         EL653
00959          MOVE AL-UNBON           TO  CTBL-TBF-A (ST-INDX)         EL653
00960          MOVE -1                 TO  CTBL-TBF-L (ST-INDX)         EL653
00961          MOVE ER-2124            TO  EMI-ERROR                    EL653
00962          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL653
00963                                                                   EL653
00964  7020-EDIT-AGE.                                                   EL653
00965      IF AT-INDX GREATER +3                                        EL653
00966          SET ST-INDX  UP  BY  +1                                  EL653
00967          SET AT-INDX      TO  +1                                  EL653
00968          GO TO 7010-PROCESS.                                      EL653
00969                                                                   EL653
00970      IF CTBL-AGE-L (ST-INDX AT-INDX) GREATER ZERO                 EL653
00971          NEXT SENTENCE                                            EL653
00972      ELSE                                                         EL653
00973          GO TO 7030-EDIT-RATE.                                    EL653
00974                                                                   EL653
00975      IF CTBL-AGE (ST-INDX AT-INDX) NUMERIC                        EL653
00976          IF CTBL-AGE (ST-INDX AT-INDX) GREATER WS-PREV-AGE  OR    EL653
00977             CTBL-AGE (ST-INDX AT-INDX) = WS-MAX-AGE               EL653
00978              MOVE AL-UNNON                                        EL653
00979                  TO  CTBL-AGE-A (ST-INDX AT-INDX)                 EL653
00980              MOVE CTBL-AGE (ST-INDX AT-INDX)                      EL653
00981                  TO  WS-PREV-AGE                                  EL653
00982          ELSE                                                     EL653
00983              MOVE -1        TO  CTBL-AGE-L (ST-INDX AT-INDX)      EL653
00984              MOVE AL-UNBON  TO  CTBL-AGE-A (ST-INDX AT-INDX)      EL653
00985              MOVE ER-2127   TO  EMI-ERROR                         EL653
00986              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL653
00987      ELSE                                                         EL653
00988          MOVE -1            TO  CTBL-AGE-L (ST-INDX AT-INDX)      EL653
00989          MOVE AL-UNBON      TO  CTBL-AGE-A (ST-INDX AT-INDX)      EL653
00990          MOVE ER-2128       TO  EMI-ERROR                         EL653
00991          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL653
00992                                                                   EL653
00993  7030-EDIT-RATE.                                                  EL653
00994      IF RT-INDX GREATER +3                                        EL653
00995          SET AT-INDX  UP  BY  +1                                  EL653
00996          SET RT-INDX      TO  +1                                  EL653
00997          GO TO 7020-EDIT-AGE.                                     EL653
00998                                                                   EL653
00999      IF CTBL-RATE-L (ST-INDX AT-INDX RT-INDX) GREATER ZERO        EL653
01000          MOVE SPACES             TO  DEEDIT-FIELD                    CL**3
01001          MOVE CTBL-RATE (ST-INDX AT-INDX RT-INDX)                    CL**3
01002              TO  DEEDIT-FIELD-X7                                     CL**3
01003          PERFORM 7100-DEEDIT  THRU  7100-EXIT                        CL**3
01004          MOVE DEEDIT-FIELD-V5    TO  WS-RATE                         CL**3
01005          IF WS-RATE  NUMERIC                                         CL**3
01006              MOVE AL-UNNON                                        EL653
01007                  TO  CTBL-RATE-A (ST-INDX AT-INDX RT-INDX)        EL653
01008              MOVE WS-RATE                                            CL**3
01009                  TO  CTBL-RATE-DISP (ST-INDX AT-INDX RT-INDX)        CL**3
01010          ELSE                                                     EL653
01011              MOVE AL-UNBON                                        EL653
01012                  TO  CTBL-RATE-A (ST-INDX AT-INDX RT-INDX)        EL653
01013              MOVE -1                                              EL653
01014                  TO  CTBL-RATE-L (ST-INDX AT-INDX RT-INDX)        EL653
01015              MOVE WS-RATE                                            CL**3
01016                  TO  CTBL-RATE-DISP (ST-INDX AT-INDX RT-INDX)        CL**3
01017              MOVE ER-2130   TO EMI-ERROR                          EL653
01018              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL653
01019                                                                   EL653
01020      SET RT-INDX  UP  BY  +1.                                     EL653
01021                                                                   EL653
01022      GO TO 7030-EDIT-RATE.                                        EL653
01023                                                                   EL653
01024  7049-EXIT.                                                       EL653
01025      EXIT.                                                        EL653
01026  EJECT                                                            EL653
01027  7050-READ-ERCTBL.                                                EL653
01028      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.           EL653
01029                                                                   EL653
01030      EXEC CICS READ                                               EL653
01031           DATASET  (CTBL-FILE-ID)                                 EL653
01032           SET      (ADDRESS OF COMM-TABLE-RECORD)                    CL**7
01033           RIDFLD   (PI-ERCTBL-KEY)                                EL653
01034      END-EXEC.                                                    EL653
01035                                                                   EL653
01036      MOVE PI-ERCTBL-KEY          TO  PI-SAVE-ERCTBL-KEY.          EL653
01037                                                                   EL653
01038  7050-EXIT.                                                       EL653
01039      EXIT.                                                        EL653
01040  EJECT                                                            EL653
01041  7100-DEEDIT.                                                     EL653
01042      EXEC CICS BIF                                                EL653
01043          DEEDIT                                                   EL653
01044          FIELD   (DEEDIT-FIELD)                                   EL653
01045          LENGTH  (15)                                             EL653
01046      END-EXEC.                                                    EL653
01047                                                                   EL653
01048  7100-EXIT.                                                       EL653
01049      EXIT.                                                        EL653
01050  EJECT                                                            EL653
01051  7150-ERCTBL-GETMAIN.                                             EL653
01052      EXEC CICS GETMAIN                                            EL653
01053          SET      (ADDRESS OF COMM-TABLE-RECORD)                     CL**7
01054          LENGTH   (ERCTBL-LENGTH)                                 EL653
01055          INITIMG  (GETMAIN-SPACE)                                 EL653
01056      END-EXEC.                                                    EL653
01057                                                                   EL653
01058  7150-EXIT.                                                       EL653
01059      EXIT.                                                        EL653
01060  EJECT                                                            EL653
01061  7200-READ-ERCTBL-UPDATE.                                         EL653
01062      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.           EL653
01063                                                                   EL653
01064      EXEC CICS READ                                               EL653
01065          DATASET  (CTBL-FILE-ID)                                  EL653
01066          SET      (ADDRESS OF COMM-TABLE-RECORD)                     CL**7
01067          RIDFLD   (PI-ERCTBL-KEY)                                 EL653
01068          UPDATE                                                   EL653
01069      END-EXEC.                                                    EL653
01070                                                                   EL653
01071  7200-EXIT.                                                       EL653
01072      EXIT.                                                        EL653
01073  EJECT                                                            EL653
01074  7250-READ-ERCTBL-GTEQ.                                           EL653
01075      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.           EL653
01076                                                                   EL653
01077      EXEC CICS READ                                               EL653
01078          DATASET  (CTBL-FILE-ID)                                  EL653
01079          SET      (ADDRESS OF COMM-TABLE-RECORD)                     CL**7
01080          RIDFLD   (PI-ERCTBL-KEY)                                 EL653
01081          GTEQ                                                     EL653
01082      END-EXEC.                                                    EL653
01083                                                                   EL653
01084  7250-EXIT.                                                       EL653
01085      EXIT.                                                        EL653
01086  EJECT                                                            EL653
01087  7300-PAGE-TABLE-FORWARD.                                         EL653
01088      MOVE SPACES                 TO  PI-ERCTBL-EOF-SW.            EL653
01089                                                                   EL653
01090      IF TBLCODEL GREATER ZERO                                     EL653
01091          MOVE TBLCODEI           TO  PI-ERC-TABLE                 EL653
01092                                      WS-SAVE-TABLE                EL653
01093          MOVE LOW-VALUES         TO  PI-ERC-BEN-TYPE              EL653
01094                                      PI-ERC-BEN-CODE              EL653
01095      ELSE                                                         EL653
01096          MOVE 'Y'                TO  PI-FIRST-TIME-SW             EL653
01097          MOVE LOW-VALUES         TO  PI-ERCTBL-KEY                EL653
01098                                      WS-SAVE-TABLE.               EL653
01099                                                                   EL653
01100      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.           EL653
01101                                                                   EL653
01102      PERFORM 7700-START-BROWSE  THRU  7700-EXIT.                  EL653
01103                                                                   EL653
01104  7310-READNEXT.                                                   EL653
01105      EXEC CICS HANDLE CONDITION                                   EL653
01106          ENDFILE  (7350-ENDFILE)                                  EL653
01107          NOTFND   (7375-NOTFOUND)                                 EL653
01108      END-EXEC.                                                    EL653
01109                                                                   EL653
01110      PERFORM 7800-READNEXT  THRU  7800-EXIT.                      EL653
01111                                                                   EL653
01112      IF ERCTBL-EOF                                                EL653
01113          IF FIRST-TIME                                            EL653
01114              MOVE LOW-VALUES     TO  EL653AO                      EL653
01115              MOVE ER-0583        TO  EMI-ERROR                    EL653
01116              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL653
01117              MOVE -1             TO  MAINTYPL                     EL653
01118              GO TO 8100-SEND-INITIAL-MAP                          EL653
01119          ELSE                                                     EL653
01120              IF BROWSE-STARTED                                    EL653
01121                  PERFORM 7950-END-BROWSE  THRU  7950-EXIT         EL653
01122                  MOVE LOW-VALUES  TO  EL653AO                     EL653
01123                  GO TO 7300-PAGE-TABLE-FORWARD.                   EL653
01124                                                                   EL653
01125      MOVE SPACE                  TO  PI-FIRST-TIME-SW.            EL653
01126                                                                   EL653
01127      IF WS-SAVE-TABLE = PI-ERC-TABLE                              EL653
01128          GO TO 7310-READNEXT.                                     EL653
01129                                                                   EL653
01130      MOVE CT-LAST-MAINT-USER     TO  PI-UPDATE-BY.                EL653
01131      MOVE CT-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.            EL653
01132      MOVE PI-ERCTBL-KEY          TO  PI-SAVE-ERCTBL-KEY.          EL653
01133      MOVE LOW-VALUES             TO  EL653AO.                     EL653
01134      MOVE CT-TABLE               TO  TBLCODEO.                    EL653
01135      MOVE CT-BEN-TYPE            TO  COVTYPEO.                    EL653
01136      MOVE CT-BEN-CODE            TO  BENCODEO.                    EL653
01137      MOVE AL-UANON               TO  TBLCODEA                     EL653
01138                                      COVTYPEA                     EL653
01139                                      BENCODEA                     EL653
01140                                      MAINTYPA.                    EL653
01141                                                                   EL653
01142      GO TO 5005-SET-UP-SCREEN.                                    EL653
01143                                                                   EL653
01144  7350-ENDFILE.                                                    EL653
01145      IF FIRST-TIME                                                EL653
01146          MOVE LOW-VALUES         TO  EL653AO                      EL653
01147          MOVE ER-0583            TO  EMI-ERROR                    EL653
01148          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL653
01149          MOVE -1                 TO  MAINTYPL                     EL653
01150          GO TO 8100-SEND-INITIAL-MAP.                             EL653
01151                                                                   EL653
01152      IF BROWSE-STARTED                                            EL653
01153          PERFORM 7950-END-BROWSE  THRU  7950-EXIT.                EL653
01154                                                                   EL653
01155      MOVE ER-2067                TO  EMI-ERROR                    EL653
01156                                                                   EL653
01157      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                   EL653
01158                                                                   EL653
01159      MOVE LOW-VALUES             TO  EL653AO                      EL653
01160                                                                   EL653
01161      GO TO 7300-PAGE-TABLE-FORWARD.                               EL653
01162                                                                   EL653
01163  7375-NOTFOUND.                                                   EL653
01164      IF BROWSE-STARTED                                            EL653
01165          PERFORM 7950-END-BROWSE  THRU  7950-EXIT.                EL653
01166                                                                   EL653
01167      GO TO 8880-NOT-FOUND.                                        EL653
01168                                                                   EL653
01169  7399-EXIT.                                                       EL653
01170      EXIT.                                                        EL653
01171  EJECT                                                            EL653
01172  7400-PAGE-TABLE-BACKWARD.                                        EL653
01173      MOVE SPACES                 TO  PI-ERCTBL-EOF-SW.            EL653
01174                                                                   EL653
01175      IF TBLCODEL GREATER ZERO                                     EL653
01176          MOVE TBLCODEI           TO  PI-ERC-TABLE                 EL653
01177                                      WS-SAVE-TABLE                EL653
01178          MOVE LOW-VALUES         TO  PI-ERC-BEN-TYPE              EL653
01179                                      PI-ERC-BEN-CODE              EL653
01180      ELSE                                                         EL653
01181          GO TO 7450-ENDFILE.                                      EL653
01182                                                                   EL653
01183      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.           EL653
01184                                                                   EL653
01185      PERFORM 7700-START-BROWSE  THRU  7700-EXIT.                  EL653
01186                                                                   EL653
01187      PERFORM 7800-READNEXT      THRU  7800-EXIT.                  EL653
01188                                                                   EL653
01189  7410-READPREV.                                                   EL653
01190      EXEC CICS HANDLE CONDITION                                   EL653
01191          ENDFILE  (7450-ENDFILE)                                  EL653
01192          NOTFND   (7475-NOTFOUND)                                 EL653
01193      END-EXEC.                                                    EL653
01194                                                                   EL653
01195      PERFORM 7900-READPREV  THRU  7900-EXIT.                      EL653
01196                                                                   EL653
01197      IF ERCTBL-EOF                                                EL653
01198          IF BROWSE-STARTED                                        EL653
01199              PERFORM 7950-END-BROWSE  THRU  7950-EXIT             EL653
01200              MOVE LOW-VALUES     TO  PI-ERCTBL-KEY                EL653
01201              GO TO 7300-PAGE-TABLE-FORWARD.                       EL653
01202                                                                   EL653
01203      IF WS-SAVE-TABLE = PI-ERC-TABLE                              EL653
01204          GO TO 7410-READPREV                                      EL653
01205      ELSE                                                         EL653
01206          MOVE LOW-VALUES         TO  PI-ERC-BEN-TYPE              EL653
01207                                      PI-ERC-BEN-CODE              EL653
01208          PERFORM 7250-READ-ERCTBL-GTEQ  THRU  7250-EXIT.          EL653
01209                                                                   EL653
01210      IF ERCTBL-EOF                                                EL653
01211          IF BROWSE-STARTED                                        EL653
01212              PERFORM 7950-END-BROWSE  THRU  7950-EXIT             EL653
01213              MOVE LOW-VALUES     TO  PI-ERCTBL-KEY                EL653
01214              GO TO 7300-PAGE-TABLE-FORWARD.                       EL653
01215                                                                   EL653
01216      MOVE CT-LAST-MAINT-USER     TO  PI-UPDATE-BY.                EL653
01217      MOVE CT-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.            EL653
01218      MOVE PI-ERCTBL-KEY          TO  PI-SAVE-ERCTBL-KEY.          EL653
01219      MOVE LOW-VALUES             TO  EL653AO.                     EL653
01220      MOVE CT-TABLE               TO  TBLCODEO.                    EL653
01221      MOVE CT-BEN-TYPE            TO  COVTYPEO.                    EL653
01222      MOVE CT-BEN-CODE            TO  BENCODEO.                    EL653
01223      MOVE AL-UANON               TO  TBLCODEA                     EL653
01224                                      COVTYPEA                     EL653
01225                                      BENCODEA                     EL653
01226                                      MAINTYPA.                    EL653
01227                                                                   EL653
01228      GO TO 5005-SET-UP-SCREEN.                                    EL653
01229                                                                   EL653
01230  7450-ENDFILE.                                                    EL653
01231      IF BROWSE-STARTED                                            EL653
01232          PERFORM 7950-END-BROWSE  THRU  7950-EXIT.                EL653
01233                                                                   EL653
01234      MOVE ER-2135                TO  EMI-ERROR                    EL653
01235                                                                   EL653
01236      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                   EL653
01237                                                                   EL653
01238      MOVE LOW-VALUES             TO  EL653AO                      EL653
01239                                                                   EL653
01240      GO TO 7300-PAGE-TABLE-FORWARD.                               EL653
01241                                                                   EL653
01242  7475-NOTFOUND.                                                   EL653
01243      IF BROWSE-STARTED                                            EL653
01244          PERFORM 7950-END-BROWSE  THRU  7950-EXIT.                EL653
01245                                                                   EL653
01246      GO TO 8880-NOT-FOUND.                                        EL653
01247                                                                   EL653
01248  7499-EXIT.                                                       EL653
01249      EXIT.                                                        EL653
01250  EJECT                                                            EL653
01251  7500-PAGE-BENEFIT-FORWARD.                                       EL653
01252      MOVE SPACES                 TO  PI-ERCTBL-EOF-SW.            EL653
01253                                                                   EL653
01254      IF TBLCODEL GREATER ZERO                                     EL653
01255          MOVE TBLCODEI           TO  PI-ERC-TABLE                 EL653
01256                                      WS-SAVE-TABLE                EL653
01257          MOVE COVTYPEI           TO  PI-ERC-BEN-TYPE              EL653
01258          MOVE BENCODEI           TO  PI-ERC-BEN-CODE              EL653
01259      ELSE                                                         EL653
01260          MOVE 'Y'                TO  PI-FIRST-TIME-SW             EL653
01261          MOVE LOW-VALUES         TO  PI-ERCTBL-KEY                EL653
01262                                      WS-SAVE-TABLE.               EL653
01263                                                                   EL653
01264      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.           EL653
01265                                                                   EL653
01266      PERFORM 7700-START-BROWSE  THRU  7700-EXIT.                  EL653
01267                                                                   EL653
01268      EXEC CICS HANDLE CONDITION                                   EL653
01269          ENDFILE  (7550-ENDFILE)                                  EL653
01270          NOTFND   (7575-NOTFOUND)                                 EL653
01271      END-EXEC.                                                    EL653
01272                                                                   EL653
01273      PERFORM 7800-READNEXT  THRU  7800-EXIT.                      EL653
01274                                                                   EL653
01275      IF ERCTBL-EOF                                                EL653
01276          IF FIRST-TIME                                            EL653
01277              MOVE LOW-VALUES     TO  EL653AO                      EL653
01278              MOVE ER-0583        TO  EMI-ERROR                    EL653
01279              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL653
01280              MOVE -1             TO  MAINTYPL                     EL653
01281              GO TO 8100-SEND-INITIAL-MAP                          EL653
01282          ELSE                                                     EL653
01283              IF BROWSE-STARTED                                    EL653
01284                  PERFORM 7950-END-BROWSE  THRU  7950-EXIT         EL653
01285                  GO TO 7500-PAGE-BENEFIT-FORWARD.                 EL653
01286                                                                   EL653
01287      IF NOT FIRST-TIME                                            EL653
01288          PERFORM 7800-READNEXT  THRU  7800-EXIT.                  EL653
01289                                                                   EL653
01290      IF ERCTBL-EOF                                                EL653
01291          PERFORM 7950-END-BROWSE  THRU  7950-EXIT                 EL653
01292          MOVE LOW-VALUES         TO  PI-ERCTBL-KEY                EL653
01293          MOVE ER-0590            TO  EMI-ERROR                    EL653
01294          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL653
01295          MOVE 'Y'                TO  PI-FIRST-TIME-SW             EL653
01296          GO TO 7500-PAGE-BENEFIT-FORWARD.                         EL653
01297                                                                   EL653
01298      IF PI-ERC-TABLE = WS-SAVE-TABLE                              EL653
01299          NEXT SENTENCE                                            EL653
01300      ELSE                                                         EL653
01301          IF FIRST-TIME                                            EL653
01302              NEXT SENTENCE                                        EL653
01303          ELSE                                                     EL653
01304              MOVE LOW-VALUES     TO  PI-ERCTBL-KEY                EL653
01305              MOVE ER-0590        TO  EMI-ERROR                    EL653
01306              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL653
01307              MOVE 'Y'            TO  PI-FIRST-TIME-SW             EL653
01308              PERFORM 7950-END-BROWSE  THRU  7950-EXIT             EL653
01309              GO TO 7500-PAGE-BENEFIT-FORWARD.                     EL653
01310                                                                   EL653
01311      MOVE SPACES                 TO  PI-FIRST-TIME-SW.            EL653
01312      MOVE CT-LAST-MAINT-USER     TO  PI-UPDATE-BY.                EL653
01313      MOVE CT-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.            EL653
01314      MOVE PI-ERCTBL-KEY          TO  PI-SAVE-ERCTBL-KEY.          EL653
01315      MOVE LOW-VALUES             TO  EL653AO.                     EL653
01316      MOVE CT-TABLE               TO  TBLCODEO.                    EL653
01317      MOVE CT-BEN-TYPE            TO  COVTYPEO.                    EL653
01318      MOVE CT-BEN-CODE            TO  BENCODEO.                    EL653
01319      MOVE AL-UANON               TO  TBLCODEA                     EL653
01320                                      COVTYPEA                     EL653
01321                                      BENCODEA                     EL653
01322                                      MAINTYPA.                    EL653
01323                                                                   EL653
01324      GO TO 5005-SET-UP-SCREEN.                                    EL653
01325                                                                   EL653
01326  7550-ENDFILE.                                                    EL653
01327      IF BROWSE-STARTED                                            EL653
01328          PERFORM 7950-END-BROWSE  THRU  7950-EXIT.                EL653
01329                                                                   EL653
01330      MOVE LOW-VALUES             TO  PI-ERCTBL-KEY.               EL653
01331      MOVE ER-0590                TO  EMI-ERROR.                   EL653
01332                                                                   EL653
01333      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL653
01334                                                                   EL653
01335      MOVE 'Y'                    TO  PI-FIRST-TIME-SW.            EL653
01336                                                                   EL653
01337      GO TO 7500-PAGE-BENEFIT-FORWARD.                             EL653
01338                                                                   EL653
01339  7575-NOTFOUND.                                                   EL653
01340      IF BROWSE-STARTED                                            EL653
01341          PERFORM 7950-END-BROWSE  THRU  7950-EXIT.                EL653
01342                                                                   EL653
01343      GO TO 8880-NOT-FOUND.                                        EL653
01344                                                                   EL653
01345  7599-EXIT.                                                       EL653
01346      EXIT.                                                        EL653
01347  EJECT                                                            EL653
01348  7600-PAGE-BENEFIT-BACKWARD.                                      EL653
01349      MOVE SPACES                 TO  PI-ERCTBL-EOF-SW.            EL653
01350                                                                   EL653
01351      IF TBLCODEL GREATER ZERO                                     EL653
01352          MOVE TBLCODEI           TO  PI-ERC-TABLE                 EL653
01353                                      WS-SAVE-TABLE                EL653
01354          MOVE COVTYPEI           TO  PI-ERC-BEN-TYPE              EL653
01355          MOVE BENCODEI           TO  PI-ERC-BEN-CODE              EL653
01356      ELSE                                                         EL653
01357          GO TO 7650-ENDFILE.                                      EL653
01358                                                                   EL653
01359      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.           EL653
01360                                                                   EL653
01361      PERFORM 7700-START-BROWSE  THRU  7700-EXIT.                  EL653
01362                                                                   EL653
01363      EXEC CICS HANDLE CONDITION                                   EL653
01364          ENDFILE  (7650-ENDFILE)                                  EL653
01365          NOTFND   (7675-NOTFOUND)                                 EL653
01366      END-EXEC.                                                    EL653
01367                                                                   EL653
01368      PERFORM 7900-READPREV  THRU  7900-EXIT.                      EL653
01369                                                                   EL653
01370      IF ERCTBL-EOF                                                EL653
01371          IF BROWSE-STARTED                                        EL653
01372              PERFORM 7950-END-BROWSE  THRU  7950-EXIT             EL653
01373              GO TO 7500-PAGE-BENEFIT-FORWARD.                     EL653
01374                                                                   EL653
01375      IF FIRST-TIME                                                EL653
01376          MOVE SPACES             TO  PI-FIRST-TIME-SW             EL653
01377      ELSE                                                         EL653
01378          PERFORM 7900-READPREV  THRU  7900-EXIT.                  EL653
01379                                                                   EL653
01380      IF ERCTBL-EOF                                                EL653
01381          IF BROWSE-STARTED                                        EL653
01382              PERFORM 7950-END-BROWSE  THRU  7950-EXIT             EL653
01383              GO TO 7500-PAGE-BENEFIT-FORWARD.                     EL653
01384                                                                   EL653
01385      IF PI-ERC-TABLE = WS-SAVE-TABLE                              EL653
01386          NEXT SENTENCE                                            EL653
01387      ELSE                                                         EL653
01388          MOVE LOW-VALUES         TO  PI-ERCTBL-KEY                EL653
01389          MOVE ER-0591            TO  EMI-ERROR                    EL653
01390          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL653
01391          MOVE 'Y'                TO  PI-FIRST-TIME-SW             EL653
01392          PERFORM 7950-END-BROWSE  THRU  7950-EXIT                 EL653
01393          GO TO 7500-PAGE-BENEFIT-FORWARD.                         EL653
01394                                                                   EL653
01395      MOVE CT-LAST-MAINT-USER     TO  PI-UPDATE-BY.                EL653
01396      MOVE CT-LAST-MAINT-USER     TO  PI-UPDATE-BY.                EL653
01397      MOVE CT-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.            EL653
01398      MOVE PI-ERCTBL-KEY          TO  PI-SAVE-ERCTBL-KEY.          EL653
01399      MOVE LOW-VALUES             TO  EL653AO.                     EL653
01400      MOVE CT-TABLE               TO  TBLCODEO.                    EL653
01401      MOVE CT-BEN-TYPE            TO  COVTYPEO.                    EL653
01402      MOVE CT-BEN-CODE            TO  BENCODEO.                    EL653
01403      MOVE AL-UANON               TO  TBLCODEA                     EL653
01404                                      COVTYPEA                     EL653
01405                                      BENCODEA                     EL653
01406                                      MAINTYPA.                    EL653
01407                                                                   EL653
01408      GO TO 5005-SET-UP-SCREEN.                                    EL653
01409                                                                   EL653
01410  7650-ENDFILE.                                                    EL653
01411      IF BROWSE-STARTED                                            EL653
01412          PERFORM 7950-END-BROWSE  THRU  7950-EXIT.                EL653
01413                                                                   EL653
01414      MOVE ER-0591                TO  EMI-ERROR.                   EL653
01415                                                                   EL653
01416      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL653
01417                                                                   EL653
01418      MOVE LOW-VALUES             TO  EL653AO.                     EL653
01419                                                                   EL653
01420      GO TO 7300-PAGE-TABLE-FORWARD.                               EL653
01421                                                                   EL653
01422  7675-NOTFOUND.                                                   EL653
01423      IF BROWSE-STARTED                                            EL653
01424          PERFORM 7950-END-BROWSE  THRU  7950-EXIT.                EL653
01425                                                                   EL653
01426      GO TO 8880-NOT-FOUND.                                        EL653
01427                                                                   EL653
01428  7699-EXIT.                                                       EL653
01429      EXIT.                                                        EL653
01430  EJECT                                                            EL653
01431  7700-START-BROWSE.                                               EL653
01432      EXEC CICS STARTBR                                            EL653
01433          DATASET  (CTBL-FILE-ID)                                  EL653
01434          RIDFLD   (PI-ERCTBL-KEY)                                 EL653
01435      END-EXEC.                                                    EL653
01436                                                                   EL653
01437      MOVE 'Y'                    TO  PI-BROWSE-SW.                EL653
01438                                                                   EL653
01439  7700-EXIT.                                                       EL653
01440      EXIT.                                                        EL653
01441  EJECT                                                            EL653
01442  7800-READNEXT.                                                   EL653
01443      EXEC CICS READNEXT                                           EL653
01444          DATASET  (CTBL-FILE-ID)                                  EL653
01445          SET      (ADDRESS OF COMM-TABLE-RECORD)                     CL**7
01446          RIDFLD   (PI-ERCTBL-KEY)                                 EL653
01447      END-EXEC.                                                    EL653
01448                                                                   EL653
01449      IF PI-COMPANY-CD NOT = CT-COMPANY-CD                         EL653
01450          MOVE LOW-VALUES         TO  EL653AO                      EL653
01451          MOVE 'Y'                TO  PI-ERCTBL-EOF-SW             EL653
01452          IF FIRST-TIME                                            EL653
01453              MOVE ER-0583        TO  EMI-ERROR                    EL653
01454          ELSE                                                     EL653
01455              MOVE ER-2067        TO  EMI-ERROR                    EL653
01456      ELSE                                                         EL653
01457          GO TO 7800-EXIT.                                         EL653
01458                                                                   EL653
01459      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL653
01460                                                                   EL653
01461  7800-EXIT.                                                       EL653
01462      EXIT.                                                        EL653
01463  EJECT                                                            EL653
01464  7900-READPREV.                                                   EL653
01465      EXEC CICS READPREV                                           EL653
01466          DATASET  (CTBL-FILE-ID)                                  EL653
01467          SET      (ADDRESS OF COMM-TABLE-RECORD)                     CL**7
01468          RIDFLD   (PI-ERCTBL-KEY)                                 EL653
01469      END-EXEC.                                                    EL653
01470                                                                   EL653
01471      IF PI-COMPANY-CD NOT = CT-COMPANY-CD                         EL653
01472          MOVE ER-2067            TO EMI-ERROR                     EL653
01473          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL653
01474          MOVE LOW-VALUES         TO  EL653AO                      EL653
01475          MOVE 'Y'                TO  PI-ERCTBL-EOF-SW.            EL653
01476                                                                   EL653
01477  7900-EXIT.                                                       EL653
01478      EXIT.                                                        EL653
01479  EJECT                                                            EL653
01480  7950-END-BROWSE.                                                 EL653
01481      EXEC CICS ENDBR                                              EL653
01482          DATASET  (CTBL-FILE-ID)                                  EL653
01483      END-EXEC.                                                    EL653
01484                                                                   EL653
01485      MOVE SPACE          TO PI-BROWSE-SW.                         EL653
01486                                                                   EL653
01487  7950-EXIT.                                                       EL653
01488      EXIT.                                                        EL653
01489  EJECT                                                            EL653
01490  8000-UPDATE-MAINT-DATE.                                          EL653
01491      MOVE SPACES                 TO  ELCNTL-KEY.                  EL653
01492      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL653
01493      MOVE '1'                    TO  CNTL-REC-TYPE.               EL653
01494      MOVE +0                     TO  CNTL-SEQ-NO.                 EL653
01495                                                                   EL653
01496      EXEC CICS HANDLE CONDITION                                   EL653
01497          NOTFND  (8000-EXIT)                                      EL653
01498      END-EXEC.                                                    EL653
01499                                                                   EL653
01500      EXEC CICS READ                                               EL653
01501          UPDATE                                                   EL653
01502          DATASET  (CNTL-FILE-ID)                                  EL653
01503          SET      (ADDRESS OF CONTROL-FILE)                          CL**7
01504          RIDFLD   (ELCNTL-KEY)                                    EL653
01505      END-EXEC.                                                    EL653
01506                                                                   EL653
01507      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL653
01508      MOVE 'B'                    TO  JP-RECORD-TYPE.              EL653
01509      MOVE CNTL-FILE-ID           TO  FILE-ID.                     EL653
01510                                                                   EL653
01511      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL653
01512                                                                   EL653
01513      MOVE BIN-CURRENT-SAVE       TO  CF-COMMISSION-TAB-MAINT-DT.  EL653
01514      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL653
01515      MOVE 'C'                    TO  JP-RECORD-TYPE.              EL653
01516      MOVE CNTL-FILE-ID           TO  FILE-ID.                     EL653
01517                                                                   EL653
01518      EXEC CICS REWRITE                                            EL653
01519          DATASET  (CNTL-FILE-ID)                                  EL653
01520          FROM     (CONTROL-FILE)                                  EL653
01521      END-EXEC.                                                    EL653
01522                                                                   EL653
01523      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL653
01524                                                                   EL653
01525  8000-EXIT.                                                       EL653
01526       EXIT.                                                       EL653
01527  EJECT                                                            EL653
01528  8100-SEND-INITIAL-MAP.                                           EL653
01529      MOVE SAVE-DATE              TO  RUNDATEO.                    EL653
01530      MOVE EIBTIME                TO  TIME-IN.                     EL653
01531      MOVE TIME-OUT               TO  RUNTIMEO.                    EL653
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01532      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    EL653
01533      MOVE -1                     TO  MAINTYPL.                    EL653
01534                                                                   EL653
01535      EXEC CICS SEND                                               EL653
01536          MAP     (MAP-NAME)                                       EL653
01537          MAPSET  (MAPSET-NAME)                                    EL653
01538          FROM    (EL653AO)                                        EL653
01539          ERASE                                                    EL653
01540          CURSOR                                                   EL653
01541      END-EXEC.                                                    EL653
01542                                                                   EL653
01543      GO TO 9100-RETURN-TRAN.                                      EL653
01544                                                                   EL653
01545  8200-SEND-DATAONLY.                                              EL653
01546      MOVE SAVE-DATE              TO  RUNDATEO.                    EL653
01547      MOVE EIBTIME                TO  TIME-IN.                     EL653
01548      MOVE TIME-OUT               TO  RUNTIMEO.                    EL653
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01549      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    EL653
01550                                                                   EL653
01551      EXEC CICS SEND                                               EL653
01552          MAP     (MAP-NAME)                                       EL653
01553          MAPSET  (MAPSET-NAME)                                    EL653
01554          FROM    (EL653AO)                                        EL653
01555          DATAONLY                                                 EL653
01556          CURSOR                                                   EL653
01557      END-EXEC.                                                    EL653
01558                                                                   EL653
01559      GO TO 9100-RETURN-TRAN.                                      EL653
01560                                                                   EL653
01561  8300-SEND-TEXT.                                                  EL653
01562      EXEC CICS SEND TEXT                                          EL653
01563          FROM    (LOGOFF-TEXT)                                    EL653
01564          LENGTH  (LOGOFF-LENGTH)                                  EL653
01565          ERASE                                                    EL653
01566          FREEKB                                                   EL653
01567      END-EXEC.                                                    EL653
01568                                                                   EL653
01569      EXEC CICS RETURN                                             EL653
01570      END-EXEC.                                                    EL653
01571                                                                   EL653
01572  8400-LOG-JOURNAL-RECORD.                                         EL653
01573      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                  EL653
01574      MOVE FILE-ID                TO  JP-FILE-ID.                  EL653
01575      MOVE THIS-PGM               TO  JP-PROGRAM-ID.               EL653
01576                                                                   EL653
pemuni*    EXEC CICS JOURNAL                                            EL653
pemuni*        JFILEID  (PI-JOURNAL-FILE-ID)                            EL653
pemuni*        JTYPEID  ('EL')                                          EL653
pemuni*        FROM     (JOURNAL-RECORD)                                EL653
pemuni*        LENGTH   (223)                                           EL653
pemuni*    END-EXEC.                                                    EL653
01583                                                                   EL653
01584  8800-UNAUTHORIZED-ACCESS.                                        EL653
01585      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL653
01586                                                                   EL653
01587      GO TO 8300-SEND-TEXT.                                        EL653
01588                                                                   EL653
01589  8810-PF23.                                                       EL653
01590      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL653
01591      MOVE XCTL-005               TO  PGM-NAME.                    EL653
01592                                                                   EL653
01593      GO TO 9300-XCTL.                                             EL653
01594                                                                   EL653
01595  8880-NOT-FOUND.                                                  EL653
01596      MOVE ER-0142                TO  EMI-ERROR.                   EL653
01597                                                                   EL653
01598      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL653
01599                                                                   EL653
01600      MOVE -1                     TO  MAINTYPL.                    EL653
01601                                                                   EL653
01602      IF EIBTRNID NOT = TRANS-ID                                   EL653
01603          GO TO 8100-SEND-INITIAL-MAP.                             EL653
01604                                                                   EL653
01605      GO TO 8200-SEND-DATAONLY.                                    EL653
01606                                                                   EL653
01607  9100-RETURN-TRAN.                                                EL653
01608      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL653
01609      MOVE '653A'                 TO  PI-CURRENT-SCREEN-NO.        EL653
01610                                                                   EL653
01611      EXEC CICS RETURN                                             EL653
01612          TRANSID   (TRANS-ID)                                     EL653
01613          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL653
01614          LENGTH    (PI-COMM-LENGTH)                               EL653
01615      END-EXEC.                                                    EL653
01616                                                                   EL653
01617  9200-RETURN-MAIN-MENU.                                           EL653
01618      MOVE XCTL-626               TO  PGM-NAME.                    EL653
01619                                                                   EL653
01620      GO TO 9300-XCTL.                                             EL653
01621                                                                   EL653
01622  9300-XCTL.                                                       EL653
01623      EXEC CICS XCTL                                               EL653
01624          PROGRAM   (PGM-NAME)                                     EL653
01625          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL653
01626          LENGTH    (PI-COMM-LENGTH)                               EL653
01627      END-EXEC.                                                    EL653
01628                                                                   EL653
01629  9400-CLEAR.                                                      EL653
01630      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                    EL653
01631                                                                   EL653
01632      GO TO 9300-XCTL.                                             EL653
01633                                                                   EL653
01634  9500-PF12.                                                       EL653
01635      MOVE XCTL-010               TO  PGM-NAME.                    EL653
01636                                                                   EL653
01637      GO TO 9300-XCTL.                                             EL653
01638                                                                   EL653
01639  9600-PGMID-ERROR.                                                EL653
01640      EXEC CICS HANDLE CONDITION                                   EL653
01641          PGMIDERR  (8300-SEND-TEXT)                               EL653
01642      END-EXEC.                                                    EL653
01643                                                                   EL653
01644      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          EL653
01645      MOVE ' '                    TO  PI-ENTRY-CD-1.               EL653
01646      MOVE XCTL-005               TO  PGM-NAME.                    EL653
01647      MOVE PGM-NAME               TO  LOGOFF-PGM.                  EL653
01648      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL653
01649                                                                   EL653
01650      GO TO 9300-XCTL.                                             EL653
01651                                                                   EL653
01652  9700-LINK-DATE-CONVERT.                                          EL653
01653      EXEC CICS LINK                                               EL653
01654          PROGRAM   ('ELDATCV')                                    EL653
01655          COMMAREA  (DATE-CONVERSION-DATA)                         EL653
01656          LENGTH    (DC-COMM-LENGTH)                               EL653
01657      END-EXEC.                                                    EL653
01658                                                                   EL653
01659  9700-EXIT.                                                       EL653
01660      EXIT.                                                        EL653
01661                                                                   EL653
01662  9900-ERROR-FORMAT.                                               EL653
01663      IF NOT EMI-ERRORS-COMPLETE                                   EL653
01664          MOVE LINK-001           TO  PGM-NAME                     EL653
01665          EXEC CICS LINK                                           EL653
01666              PROGRAM   (PGM-NAME)                                 EL653
01667              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL653
01668              LENGTH    (EMI-COMM-LENGTH)                          EL653
01669          END-EXEC.                                                EL653
01670                                                                   EL653
01671  9900-EXIT.                                                       EL653
01672      EXIT.                                                        EL653
01673                                                                   EL653
01674  9990-ABEND.                                                      EL653
01675      MOVE LINK-004               TO  PGM-NAME.                    EL653
01676      MOVE DFHEIBLK               TO  EMI-LINE1.                   EL653
01677                                                                   EL653
01678      EXEC CICS LINK                                               EL653
01679          PROGRAM   (PGM-NAME)                                     EL653
01680          COMMAREA  (EMI-LINE1)                                    EL653
01681          LENGTH    (72)                                           EL653
01682      END-EXEC.                                                    EL653
01683                                                                   EL653
01684      GO TO 8200-SEND-DATAONLY.                                    EL653
01685                                                                   EL653
01686      GOBACK.                                                      EL653
01687                                                                   EL653
01688  9995-SECURITY-VIOLATION.                                         EL653
01689                              COPY ELCSCTP.                        EL653
01690                                                                   EL653
01691  9995-EXIT.                                                       EL653
01692       EXIT.                                                       EL653
