00001  ID DIVISION.                                                     04/04/96
00002                                                                   EL6505
00003  PROGRAM-ID.                 EL6505.                                 LV015
00004 *              PROGRAM CONVERTED BY                                  CL*13
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*13
00006 *              CONVERSION DATE 02/16/95 10:30:26.                    CL*13
00007 *                            VMOD=2.015                              CL*15
00008 *                                                                 EL6505
00009 *AUTHOR.     LOGIC,INC.                                              CL*13
00010 *            DALLAS, TEXAS.                                          CL*13
00011                                                                   EL6505
00012 *DATE-COMPILED.                                                      CL*13
00013 *SECURITY.   *****************************************************   CL*13
00014 *            *                                                   *   CL*13
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*13
00016 *            *                                                   *   CL*13
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*13
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*13
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*13
00020 *            *                                                   *   CL*13
00021 *            *****************************************************   CL*13
00022 *                                                                 EL6505
00023 *REMARKS.  TRANSACTION - EXH6 - ACCOUNT MAINT (BENEFIT CONTROLS).    CL**7
00022 *                                                                 EL6505
101101******************************************************************
101101*                   C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101101*                              ADJUSTED REDEFINES EL6505AI FILLER
101101******************************************************************

00024                                                                   EL6505
00025  ENVIRONMENT DIVISION.                                            EL6505
00026                                                                   EL6505
00027      EJECT                                                        EL6505
00028  DATA DIVISION.                                                   EL6505
00029  WORKING-STORAGE SECTION.                                         EL6505
00030  77  FILLER  PIC X(32)  VALUE '********************************'. EL6505
00031  77  FILLER  PIC X(32)  VALUE '*    EL6505 WORKING STORAGE    *'. EL6505
00032  77  FILLER  PIC X(32)  VALUE '*********** VMOD 2.015 *********'.    CL*15
00033                                                                   EL6505
00034  01  WS-DATE-AREA.                                                EL6505
00035      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.    EL6505
00036      05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.       CL**5
00037                                                                   EL6505
00038  01  STANDARD-AREAS.                                              EL6505
00039      12  RETURNED-FROM               PIC X(8)    VALUE SPACES.       CL**6
00040      12  QID1.                                                       CL**6
00041          16  QID1-TERM               PIC X(4).                       CL**6
00042          16  FILLER                  PIC X(4)    VALUE '650A'.       CL**6
00043      12  QID2.                                                       CL**6
00044          16  QID2-TERM               PIC X(4).                       CL**6
00045          16  FILLER                  PIC X(4)    VALUE '650B'.       CL**6
00046      12  WS-COMM-LENGTH              PIC S9(4) COMP VALUE +1500.     CL**5
00047      12  WS-MAP-LENGTH               PIC S9(4) COMP VALUE +885.      CL**6
00048      12  MAP-NAME                    PIC X(8)    VALUE 'EL6505A'. EL6505
00049      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL6505S'. EL6505
00050      12  SCREEN-NUMBER               PIC X(4)    VALUE '650F'.    EL6505
00051      12  TRANS-ID                    PIC X(4)    VALUE 'EXH6'.    EL6505
00052      12  THIS-PGM                    PIC X(8)    VALUE 'EL6505'.  EL6505
00053      12  PGM-NAME                    PIC X(8).                    EL6505
00054      12  TIME-IN                     PIC S9(7).                   EL6505
00055      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL6505
00056          16  FILLER                  PIC X.                       EL6505
00057          16  TIME-OUT                PIC 99V99.                   EL6505
00058          16  FILLER                  PIC XX.                         CL**5
00059      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.   EL6505
00060      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.   EL6505
00061      12  XCTL-626                    PIC X(8)    VALUE 'EL626'.   EL6505
00062      12  XCTL-650                    PIC X(8)    VALUE 'EL650'.   EL6505
00063      12  XCTL-6501                   PIC X(8)    VALUE 'EL6501'.  EL6505
00064      12  XCTL-6502                   PIC X(8)    VALUE 'EL6502'.     CL**2
00065      12  XCTL-6503                   PIC X(8)    VALUE 'EL6503'.     CL**2
00066      12  XCTL-6504                   PIC X(8)    VALUE 'EL6504'.     CL**2
00067      12  XCTL-6506                   PIC X(8)    VALUE 'EL6506'.     CL**2
00068      12  XCTL-6507                   PIC X(8)    VALUE 'EL6507'.     CL**7
00069      12  XCTL-6565                   PIC X(8)    VALUE 'EL6565'.     CL**6
00070      12  LINK-001                    PIC X(8)    VALUE 'EL001'.   EL6505
00071      12  LINK-004                    PIC X(8)    VALUE 'EL004'.   EL6505
00072      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'. EL6505
00073      12  FILE-ID                     PIC X(8)    VALUE SPACES.    EL6505
00074      12  ERACCT-FILE                 PIC X(8)    VALUE 'ERACCT'.  EL6505
00075      12  CNTL-FILE-ID                PIC X(8)    VALUE 'ELCNTL'.  EL6505
00076      12  BIN-CURRENT-SAVE            PIC XX      VALUE SPACES.    EL6505
00077      12  YMD-CURRENT-SAVE            PIC X(6)    VALUE SPACES.    EL6505
00078                                                                   EL6505
00079      12  ERACCT-LENGTH               PIC S9(4)   VALUE +2023 COMP.EL6505
00080      12  ELCNTL-LENGTH               PIC S9(4)   VALUE +527  COMP.EL6505
00081      12  SC-ITEM                     PIC S9(4)   VALUE +1    COMP.EL6505
00082      12  WS-JOURNAL-FILE-LENGTH      PIC S9(4)   VALUE +0    COMP.EL6505
00083      12  SUB1                        PIC S9(4)   VALUE +0    COMP.EL6505
00084      12  SUB2                        PIC S9(4)   VALUE +0    COMP.   CL**7
00085      12  SUB3                        PIC S9(4)   VALUE +0    COMP.   CL**7
00086                                                                   EL6505
00087      12  DEEDIT-FIELD                PIC X(15).                   EL6505
00088      12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).      EL6505
CIDMOD     12  DEEDIT-FIELD-V6  REDEFINES DEEDIT-FIELD PIC S9(9)V9(6).  000
00089                                                                   EL6505
00090      12  WS-EDIT-FIELD-CONV          PIC S9(4)   VALUE +0.        EL6505
00091                                                                   EL6505
00092      EJECT                                                        EL6505
00093      12  ERROR-MESSAGES.                                          EL6505
00094          16  ER-0000                 PIC X(4)    VALUE '0000'.    EL6505
00095          16  ER-0002                 PIC X(4)    VALUE '0002'.    EL6505
00096          16  ER-0004                 PIC X(4)    VALUE '0004'.    EL6505
00097          16  ER-0008                 PIC X(4)    VALUE '0008'.    EL6505
00098          16  ER-0029                 PIC X(4)    VALUE '0029'.    EL6505
00099          16  ER-0068                 PIC X(4)    VALUE '0068'.    EL6505
00100          16  ER-0070                 PIC X(4)    VALUE '0070'.    EL6505
00101          16  ER-0150                 PIC X(4)    VALUE '0150'.       CL**2
00102          16  ER-0151                 PIC X(4)    VALUE '0151'.       CL**2
00103          16  ER-0627                 PIC X(4)    VALUE '0627'.       CL*10
00104          16  ER-2039                 PIC X(4)    VALUE '2039'.    EL6505
00105          16  ER-2298                 PIC X(4)    VALUE '2298'.       CL*10
00106          16  ER-2387                 PIC X(4)    VALUE '2387'.    EL6505
00107          16  ER-2388                 PIC X(4)    VALUE '2388'.    EL6505
00108          16  ER-2389                 PIC X(4)    VALUE '2389'.    EL6505
00109          16  ER-2390                 PIC X(4)    VALUE '2390'.    EL6505
00110          16  ER-2572                 PIC X(4)    VALUE '2572'.    EL6505
00111          16  ER-2946                 PIC X(4)    VALUE '2946'.       CL**8
00112          16  ER-2947                 PIC X(4)    VALUE '2947'.       CL**8
00113          16  ER-2948                 PIC X(4)    VALUE '2948'.       CL**8
00114          16  ER-3126                 PIC X(4)    VALUE '3126'.       CL**2
00115          16  ER-7240                 PIC X(4)    VALUE '7240'.       CL**3
00116          16  ER-7241                 PIC X(4)    VALUE '7241'.       CL**3
00117          16  ER-8034                 PIC X(4)    VALUE '8034'.       CL*13
00118          16  ER-8149                 PIC X(4)    VALUE '8149'.       CL*14
00119          16  ER-8150                 PIC X(4)    VALUE '8150'.       CL*14
00120          16  ER-XXXX                 PIC X(4)    VALUE '9999'.       CL*10
00121                                                                   EL6505
00122      12  ELCNTL-KEY.                                              EL6505
00123          16  CNTL-COMP-ID            PIC X(3)    VALUE SPACES.    EL6505
00124          16  CNTL-REC-TYPE           PIC X       VALUE SPACES.    EL6505
00125          16  CNTL-ACCESS.                                            CL**7
00126              20  FILLER              PIC X(02)   VALUE SPACES.       CL**7
00127              20  CNTL-HI-BEN         PIC X(02)   VALUE SPACES.       CL**7
00128          16  CNTL-SEQ-NO             PIC S9(4)   VALUE +0  COMP.  EL6505
00129                                                                      CL*14
00130      12  WS-MAX-BNDX                 PIC S99.                        CL*14
00131                                                                      CL*14
00132      12  WS-SELECT-LINE              PIC S99.                        CL*14
00133                                                                      CL**2
00134      12  WS-EDIT-USECODE             PIC X.                          CL**3
00135          88  VALID-USE-CODE             VALUE ' ' 'N' 'Y'.           CL*11
00136      12  WS-EDIT-BENCODE             PIC XX.                         CL**2
00137          88  INVALID-BENEFIT            VALUE '  '.                  CL**7
00138      12  WS-EDIT-REMTERM             PIC X.                          CL*10
00139          88  VALID-REM-TERM             VALUE ' ' '1' THRU '7'.      CL*10
00140      12  WS-WORK-BENEFIT-TABLE.                                      CL**7
00141          16  WS-WORK-TABLE OCCURS 20.                                CL**7
00142              20  WS-WORK-BENEFIT         PIC X(02).                  CL**7
00143              20  WS-WORK-TYPE            PIC X(01).                  CL**7
00144              20  WS-WORK-REVISION        PIC X(03).                  CL**7
00145              20  WS-WORK-REM-TERM        PIC X(01).                  CL*10
00146              20  WS-WORK-RETRO-Y-N       PIC X(01).                  CL*10
00147              20  WS-FILLER               PIC X(02).                  CL*10
00148          16  FILLER                      PIC X(80).                  CL*14
00149                                                                      CL*14
00150      12  WS-DMD-WORK-BENEFIT-TABLE.                                  CL*14
00151          16  WS-DMD-WORK-TABLE OCCURS 50.                            CL*14
00152              20  WS-DMD-WORK-BENEFIT     PIC X(02).                  CL*14
00153              20  WS-DMD-WORK-TYPE        PIC X(01).                  CL*14
00154              20  WS-DMD-WORK-REVISION    PIC X(03).                  CL*14
00155              20  WS-DMD-WORK-REM-TERM    PIC X(01).                  CL*14
00156              20  WS-DMD-WORK-RETRO-Y-N   PIC X(01).                  CL*14
00157              20  WS-DMD-FILLER           PIC X(02).                  CL*14
00158          16  FILLER                      PIC X(80).                  CL*14
00159                                                                      CL*14
00160      12  WS-DMD-WORK-BENEFIT-TABLE-2.                                CL*14
00161          16  WS-DMD-WORK-TABLE-2 OCCURS 50.                          CL*14
00162              20  WS-DMD-WORK-BENEFIT-2   PIC X(02).                  CL*14
00163              20  WS-DMD-WORK-TYPE-2      PIC X(01).                  CL*14
00164              20  WS-DMD-WORK-REVISION-2  PIC X(03).                  CL*14
00165              20  WS-DMD-WORK-REM-TERM-2  PIC X(01).                  CL*14
00166              20  WS-DMD-WORK-RETRO-Y-N-2 PIC X(01).                  CL*14
00167              20  WS-DMD-FILLER-2         PIC X(02).                  CL*14
00168          16  FILLER                      PIC X(80).                  CL**7
00169                                                                   EL6505
00170      EJECT                                                        EL6505
00171                            COPY ELCSCTM.                             CL*10
00172      EJECT                                                        EL6505
00173                            COPY ELCSCRTY.                            CL*10
00174      EJECT                                                        EL6505
00175                                      COPY ELCLOGOF.                  CL*10
00176                                                                   EL6505
00177      EJECT                                                        EL6505
00178                                      COPY ELCDATE.                   CL*10
00179                                                                   EL6505
00180      EJECT                                                        EL6505
00181                                      COPY ELCATTR.                   CL*10
00182                                                                   EL6505
00183      EJECT                                                        EL6505
00184                                      COPY ELCEMIB.                   CL*10
00185                                                                   EL6505
00186      EJECT                                                        EL6505
00187                                      COPY ELCINTF.                   CL*10
00188                                      COPY ELC650PI.                  CL**6
00189                                                                      CL**6
00190                                      COPY ELCJPFX.                   CL*10
00191                                      PIC X(2000).                 EL6505
00192                                                                   EL6505
00193      EJECT                                                        EL6505
00194                                      COPY ELCAID.                    CL*10
00195  01  FILLER    REDEFINES DFHAID.                                  EL6505
00196      12  FILLER                      PIC X(8).                    EL6505
00197      12  PF-VALUES                   PIC X       OCCURS 2.        EL6505
00198                                                                   EL6505
00199      EJECT                                                        EL6505
00200                                      COPY EL6505S.                   CL*10
00201  01  EL6505AO-R                  REDEFINES                        EL6505
00202      EL6505AI.                                                    EL6505
101101     12  FILLER                      PIC X(144).                     CL*14
00204      12  BENEFIT-TABLE           OCCURS 20 TIMES                     CL**2
00205                                           INDEXED BY BNDX.           CL**2
00206          16  BENCODE-L               PIC S9(4)       COMP.           CL**2
00207          16  BENCODE-A               PIC X.                          CL**2
00208          16  BENCODE                 PIC XX.                         CL**2
00209          16  BENTYPE-L               PIC S9(4)       COMP.           CL**2
00210          16  BENTYPE-A               PIC X.                          CL**2
00211          16  BENTYPE                 PIC X.                          CL**2
00212          16  REVNO-L                 PIC S9(4)       COMP.           CL**7
00213          16  REVNO-A                 PIC X.                          CL**7
00214          16  REVNO                   PIC XXX.                        CL**7
00215          16  RETROYN-L               PIC S9(4)       COMP.           CL*10
00216          16  RETROYN-A               PIC X.                          CL*10
00217          16  RETROYN                 PIC X.                          CL*10
00218          16  REMTERM-L               PIC S9(4)       COMP.           CL*10
00219          16  REMTERM-A               PIC X.                          CL*10
00220          16  REMTERM                 PIC X.                          CL*10
00221          16  FILLER                  PIC X(7).                       CL*14
00222                                                                   EL6505
00223      EJECT                                                        EL6505
00224  LINKAGE SECTION.                                                 EL6505
00225  01  DFHCOMMAREA                     PIC X(1500).                    CL**5
00226                                                                   EL6505
00227      EJECT                                                        EL6505
00228 *01 PARMLIST .                                                       CL*13
00229 *    02  FILLER                      PIC S9(8)   COMP.               CL*13
00230 *    02  ERACCT-POINTER              PIC S9(8)   COMP.               CL*13
00231 *    02  ELCNTL-POINTER              PIC S9(8)   COMP.               CL*13
00232                                                                   EL6505
00233                                      COPY ERCACCT.                   CL*10
00234      EJECT                                                        EL6505
00235                                      COPY ELCCNTL.                   CL*10
00236      EJECT                                                        EL6505
00237                                                                   EL6505
00238  PROCEDURE DIVISION.                                              EL6505
00239                                                                      CL**7
00240      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL6505
00241      MOVE '5'                    TO  DC-OPTION-CODE.              EL6505
00242      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL6505
00243      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL6505
00244      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL6505
00245      MOVE DC-GREG-DATE-1-YMD     TO  YMD-CURRENT-SAVE.            EL6505
00246                                                                   EL6505
00247      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL6505
00248      MOVE 2                      TO  EMI-NUMBER-OF-LINES             CL**6
00249                                      EMI-SWITCH2.                    CL**6
00250                                                                      CL**6
00251      MOVE EIBTRMID               TO  QID1-TERM                       CL**6
00252                                      QID2-TERM.                      CL**6
00253                                                                      CL**6
00254      IF EIBCALEN = 0                                              EL6505
00255          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL6505
00256                                                                      CL*14
00257      IF PI-COMPANY-ID EQUAL 'DMD'                                    CL*14
00258          IF PI-CALLING-PROGRAM NOT = THIS-PGM                        CL*14
00259              IF PI-CALLING-PROGRAM = XCTL-6507                       CL*14
00260                  IF PI-DMD-SCREEN EQUAL '2'                          CL*14
00261                      MOVE +0         TO PI-DMD-OCCURS                CL*14
00262                  ELSE                                                CL*14
00263                      IF PI-DMD-SCREEN EQUAL '3'                      CL*14
00264                          MOVE +20    TO PI-DMD-OCCURS                CL*14
00265                      ELSE                                            CL*14
00266                          MOVE 'F'    TO PI-DMD-FILE-SW               CL*14
00267                          MOVE +0     TO PI-DMD-OCCURS                CL*14
00268                          MOVE '1'    TO PI-DMD-SCREEN                CL*14
00269              ELSE                                                    CL*14
00270                  MOVE 'F'            TO PI-DMD-FILE-SW               CL*14
00271                  MOVE +0             TO PI-DMD-OCCURS                CL*14
00272                  MOVE '1'            TO PI-DMD-SCREEN.               CL*14
00273                                                                   EL6505
00274      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL6505
00275          IF PI-CALLING-PROGRAM = XCTL-6501                           CL*12
00276              MOVE PI-CALLING-PROGRAM                                 CL*12
00277                                  TO  PI-RETURN-TO-PROGRAM            CL*12
00278              MOVE THIS-PGM       TO  PI-CALLING-PROGRAM              CL*12
00279          ELSE                                                        CL*12
00280              MOVE THIS-PGM       TO  PI-CALLING-PROGRAM.             CL*12
00281                                                                   EL6505
00282      MOVE LOW-VALUES             TO  EL6505AI.                    EL6505
00283                                                                   EL6505
00284      IF EIBTRNID NOT = TRANS-ID                                   EL6505
00285          MOVE PI-MAINT           TO  MAINTYPO                     EL6505
00286          MOVE AL-UANON           TO  MAINTYPA                     EL6505
00287          MOVE -1                 TO  MAINTYPL                     EL6505
00288         IF PI-MAINT = 'S' OR 'C'                                     CL**5
00289             GO TO 4000-SHOW                                          CL**5
00290          ELSE                                                     EL6505
00291             IF PI-MAINT = 'A'                                        CL**5
00292                 MOVE 'C'        TO  PI-MAINT                         CL**5
00293                 GO TO 4000-SHOW                                      CL**5
00294              ELSE                                                 EL6505
00295                 GO TO 8100-SEND-INITIAL-MAP.                         CL**5
00296                                                                   EL6505
00297      EXEC CICS HANDLE CONDITION                                   EL6505
00298          PGMIDERR  (9600-PGMID-ERROR)                             EL6505
00299          ERROR     (9990-ABEND)                                   EL6505
00300      END-EXEC.                                                    EL6505
00301                                                                   EL6505
00302      IF EIBAID = DFHCLEAR                                         EL6505
00303          GO TO 9400-CLEAR.                                        EL6505
00304                                                                   EL6505
00305      EJECT                                                        EL6505
00306  0200-RECEIVE.                                                    EL6505
00307      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL6505
00308          MOVE ER-0008            TO  EMI-ERROR                    EL6505
00309          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6505
00310          MOVE -1                 TO  PFENTERL                     EL6505
00311          GO TO 8200-SEND-DATAONLY.                                EL6505
00312                                                                   EL6505
00313      EXEC CICS RECEIVE                                            EL6505
00314          MAP      (MAP-NAME)                                      EL6505
00315          MAPSET   (MAPSET-NAME)                                   EL6505
00316          INTO     (EL6505AI)                                      EL6505
00317      END-EXEC.                                                    EL6505
00318                                                                   EL6505
00319      IF PFENTERL = 0                                              EL6505
00320          GO TO 0300-CHECK-PFKEYS.                                 EL6505
00321                                                                      CL**7
00322      IF EIBAID NOT = DFHENTER                                     EL6505
00323          MOVE ER-0004            TO  EMI-ERROR                    EL6505
00324          GO TO 0320-INPUT-ERROR.                                  EL6505
00325                                                                   EL6505
00326      IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)      CL**5
00327          MOVE PF-VALUES (PFENTERI)   TO  EIBAID                   EL6505
00328      ELSE                                                         EL6505
00329          MOVE ER-0029                TO  EMI-ERROR                EL6505
00330          GO TO 0320-INPUT-ERROR.                                  EL6505
00331                                                                      CL**7
00332      EJECT                                                        EL6505
00333  0300-CHECK-PFKEYS.                                               EL6505
00334                                                                      CL**7
00335      IF EIBAID = DFHPF23                                          EL6505
00336          GO TO 8810-PF23.                                         EL6505
00337      IF EIBAID = DFHPF24                                          EL6505
00338          GO TO 9200-RETURN-MAIN-MENU.                             EL6505
00339      IF EIBAID = DFHPF12                                          EL6505
00340          GO TO 9500-PF12.                                         EL6505
00341                                                                      CL**9
00342      IF PI-COMPANY-ID EQUAL 'DMD'                                    CL*14
00343          IF EIBAID EQUAL DFHPF1                                      CL*14
00344             IF FIRST-OCCURS                                          CL*14
00345                 MOVE 'I'         TO PI-DMD-FILE-SW                   CL*14
00346                 MOVE +0          TO PI-DMD-OCCURS                    CL*14
00347                 MOVE '2'         TO PI-DMD-SCREEN                    CL*14
00348                 GO TO 4000-SHOW                                      CL*14
00349             ELSE                                                     CL*14
00350                 IF PI-DMD-OCCURS EQUAL  +0                           CL*14
00351                     MOVE '2'     TO PI-DMD-SCREEN                    CL*14
00352                     GO TO 4000-SHOW                                  CL*14
00353                 ELSE                                                 CL*14
00354                     MOVE 'E'     TO PI-DMD-FILE-SW                   CL*14
00355                     MOVE '3'     TO PI-DMD-SCREEN                    CL*14
00356                     MOVE +20     TO PI-DMD-OCCURS                    CL*14
00357                     GO TO 4000-SHOW.                                 CL*14
00358                                                                      CL*14
00359      IF PI-COMPANY-ID EQUAL 'DMD'                                    CL*14
00360          IF EIBAID EQUAL DFHPF2                                      CL*14
00361             IF  FIRST-OCCURS                                         CL*14
00362                 MOVE ER-8149     TO  EMI-ERROR                       CL*14
00363                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             CL*14
00364                 GO TO 4000-SHOW                                      CL*14
00365             ELSE                                                     CL*14
00366                 IF (INTO-NEXT-BENEFITS AND                           CL*14
00367                    PI-DMD-OCCURS EQUAL +20)                          CL*14
00368                 MOVE 'F'         TO  PI-DMD-FILE-SW                  CL*14
00369                 MOVE ER-8149     TO  EMI-ERROR                       CL*14
00370                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             CL*14
00371                 GO TO 4000-SHOW                                      CL*14
00372             ELSE                                                     CL*14
00373                 IF (INTO-NEXT-BENEFITS AND                           CL*14
00374                    PI-DMD-OCCURS NOT LESS THAN +20)                  CL*14
00375                     MOVE +0          TO  PI-DMD-OCCURS               CL*14
00376                     GO TO 4000-SHOW                                  CL*14
00377                 ELSE                                                 CL*14
00378                     IF END-OF-FILE                                   CL*14
00379                         MOVE 'I'         TO  PI-DMD-FILE-SW          CL*14
00380                         MOVE +0          TO  PI-DMD-OCCURS           CL*14
00381                         GO TO 4000-SHOW.                             CL*14
00382                                                                      CL*14
00387      IF EIBAID = DFHPF5                                              CL**9
00388          MOVE XCTL-6502          TO  PGM-NAME                        CL**9
00389          GO TO 9300-XCTL.                                            CL**9
00390                                                                      CL**5
00391      IF EIBAID = DFHPF7                                           EL6505
00392          MOVE XCTL-6504          TO  PGM-NAME                        CL**2
00393          GO TO 9300-XCTL.                                            CL**2
00394                                                                      CL**5
00395      IF EIBAID = DFHPF8                                              CL**2
00396          MOVE XCTL-6506          TO  PGM-NAME                        CL**2
00397          GO TO 9300-XCTL.                                            CL**2
00398                                                                      CL**5
00399      IF EIBAID = DFHPF9                                              CL**2
00400          MOVE XCTL-6501          TO  PGM-NAME                     EL6505
00401          GO TO 9300-XCTL.                                         EL6505
00402                                                                      CL*14
00403      IF EIBAID EQUAL DFHENTER                                        CL*14
00404         IF PI-COMPANY-ID EQUAL 'DMD'                                 CL*14
00405             IF SELCDEL GREATER THAN +0                               CL*14
00406                IF (SELCDEI NUMERIC) AND                              CL*14
00407                   (SELCDEI GREATER THAN 00 AND                       CL*14
00408                   SELCDEI LESS THAN 21)                              CL*14
00409                      NEXT SENTENCE                                   CL*14
00410                ELSE                                                  CL*14
00411                    MOVE SELCDEI  TO WS-SELECT-LINE                   CL*14
00412                    IF WS-SELECT-LINE LESS THAN 41                    CL*14
00413                       ADD -20         TO WS-SELECT-LINE              CL*14
00414                       MOVE WS-SELECT-LINE TO SELCDEI                 CL*14
00415                    ELSE                                              CL*14
00416                       ADD -40         TO WS-SELECT-LINE              CL*14
00417                       MOVE WS-SELECT-LINE TO SELCDEI.                CL*14
00418                                                                      CL*14
00419                                                                      CL**5
00420      IF EIBAID EQUAL DFHENTER                                        CL**7
00421         IF SELCDEL GREATER THAN +0                                   CL**7
00422            IF (SELCDEI NUMERIC) AND                                  CL**7
00423               (SELCDEI GREATER THAN 00 AND                           CL**7
00424               SELCDEI LESS THAN 21)                                  CL**7
00425               MOVE XCTL-6507     TO  PGM-NAME                        CL**7
00426               MOVE PI-ACCT-CCGSA-KEY                                 CL**7
00427                                  TO PI-PLAN-KEY                      CL**7
00428               MOVE BENCODE (SELCDEI)                                 CL**7
00429                                  TO PI-PLAN-BEN                      CL**7
00430               MOVE BENTYPE (SELCDEI)                                 CL**7
00431                                  TO PI-PLAN-BEN-TYPE                 CL**7
00432               MOVE REVNO (SELCDEI)                                   CL**7
00433                                  TO PI-PLAN-REVISION                 CL**7
00434               GO TO 9300-XCTL                                        CL**7
00435            ELSE                                                      CL**7
00436               MOVE -1            TO SELCDEL                          CL**7
00437               MOVE AL-UNBON      TO SELCDEA                          CL**7
00438               MOVE ER-2946       TO EMI-ERROR                        CL**8
00439               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               CL**7
00440               GO TO 8200-SEND-DATAONLY.                              CL**7
00441                                                                      CL**6
00442      IF EIBAID = DFHENTER                                         EL6505
00443          GO TO 0330-CHECK-MAINTYP.                                EL6505
00444                                                                   EL6505
00445      MOVE ER-0029                TO  EMI-ERROR.                   EL6505
00446  EJECT                                                               CL*14
00447  0320-INPUT-ERROR.                                                EL6505
00448                                                                      CL**7
00449      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6505
00450      MOVE AL-UNBON               TO  PFENTERA.                    EL6505
00451      MOVE -1                     TO  PFENTERL.                    EL6505
00452      GO TO 8200-SEND-DATAONLY.                                    EL6505
00453                                                                   EL6505
00454  0330-CHECK-MAINTYP.                                              EL6505
00455                                                                      CL**7
00456      IF MAINTYPL GREATER ZERO                                        CL**5
00457          IF MAINTYPI = 'S' OR 'C' OR 'A'                          EL6505
00458              MOVE AL-UANON       TO  MAINTYPA                     EL6505
00459              MOVE MAINTYPI       TO  PI-MAINT                     EL6505
00460          ELSE                                                     EL6505
00461              MOVE -1             TO  MAINTYPL                     EL6505
00462              MOVE AL-UABON       TO  MAINTYPA                     EL6505
00463              MOVE ER-2039        TO  EMI-ERROR                    EL6505
00464              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6505
00465              GO TO 8200-SEND-DATAONLY                             EL6505
00466      ELSE                                                         EL6505
00467          MOVE -1                 TO  MAINTYPL                     EL6505
00468          MOVE AL-UABON           TO  MAINTYPA                     EL6505
00469          MOVE ER-2039            TO  EMI-ERROR                    EL6505
00470          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6505
00471          GO TO 8200-SEND-DATAONLY.                                EL6505
00472                                                                   EL6505
00473      IF PI-MAINT = 'S'                                            EL6505
00474          IF PI-COMPANY-ID EQUAL 'DMD'                                CL*14
00475              IF SCREEN-1-DISPLAYED                                   CL*14
00476                  MOVE 'F'       TO PI-DMD-FILE-SW                    CL*14
00477                  MOVE +0        TO PI-DMD-OCCURS                     CL*14
00478                  GO TO 4000-SHOW                                     CL*14
00479              ELSE                                                    CL*14
00480                  IF SCREEN-2-DISPLAYED                               CL*14
00481                      MOVE +0        TO PI-DMD-OCCURS                 CL*14
00482                      GO TO 4000-SHOW                                 CL*14
00483                  ELSE                                                CL*14
00484                     IF SCREEN-3-DISPLAYED                            CL*14
00485                        MOVE +20     TO PI-DMD-OCCURS                 CL*14
00486                        GO TO 4000-SHOW                               CL*14
00487          ELSE                                                        CL*14
00488              GO TO 4000-SHOW.                                        CL*14
00489                                                                   EL6505
00490      PERFORM 7800-COMPANY-REC-READ THRU 7899-EXIT.                EL6505
00491                                                                   EL6505
00492      IF EMI-ERROR NOT = ZEROS                                     EL6505
00493          MOVE -1                 TO  MAINTYPL                     EL6505
00494          GO TO 8200-SEND-DATAONLY.                                EL6505
00495                                                                   EL6505
00496      GO TO 4200-MAINT.                                            EL6505
00497                                                                   EL6505
00498      EJECT                                                        EL6505
00499  4000-SHOW.                                                       EL6505
00500      PERFORM 7100-READ-ERACCT THRU 7100-EXIT.                     EL6505
00501      MOVE LOW-VALUES             TO  EL6505AO.                    EL6505
00502      GO TO 5000-BUILD-INITIAL-SCREEN.                             EL6505
00503                                                                   EL6505
00504      EJECT                                                        EL6505
00505                                                                   EL6505
00506  4200-MAINT.                                                      EL6505
00507      IF NOT MODIFY-CAP                                            EL6505
00508          MOVE 'UPDATE'       TO SM-READ                           EL6505
00509          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT           EL6505
00510          MOVE ER-0070             TO  EMI-ERROR                   EL6505
00511          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6505
00512          GO TO 8100-SEND-INITIAL-MAP.                             EL6505
00513                                                                   EL6505
00514      PERFORM 7000-EDIT THRU 7099-EXIT.                            EL6505
00515                                                                   EL6505
00516      IF EMI-NO-ERRORS                                             EL6505
00517          NEXT SENTENCE                                            EL6505
00518      ELSE                                                         EL6505
00519          GO TO 8200-SEND-DATAONLY.                                EL6505
00520                                                                   EL6505
00521      PERFORM 7300-READ-ERACCT-UPDATE THRU 7300-EXIT.              EL6505
00522                                                                   EL6505
00523      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA.                 CL**6
00524      MOVE ERACCT-FILE            TO  FILE-ID.                     EL6505
00525                                                                   EL6505
00526      IF PI-COMPANY-ID EQUAL 'DMD'                                    CL*14
00527          MOVE SPACES           TO WS-DMD-WORK-BENEFIT-TABLE          CL*14
00528                                   WS-DMD-WORK-BENEFIT-TABLE-2        CL*14
00529          MOVE +1                       TO SUB1                       CL*14
00530                                           SUB2                       CL*14
00531          PERFORM 6700-LOAD-BENEFIT-CODES                             CL*14
00532                                        THRU 6700-LOAD-EXIT           CL*14
00533          PERFORM 6600-CHECK-FOR-UPDATE                               CL*14
00534                                        THRU 6649-EXIT                CL*14
00535          PERFORM 6800-SORT-BENEFITS    THRU 6899-EXIT                CL*14
00536          MOVE +1                       TO SUB1                       CL*14
00537                                           SUB2                       CL*14
00538          PERFORM 6900-LOAD-ACCOUNT-MASTER                            CL*14
00539                                        THRU 6900-LOAD-EXIT           CL*14
00540      ELSE                                                            CL*14
00541          PERFORM 6000-CHECK-FOR-UPDATE THRU 6049-EXIT                CL*14
00542          PERFORM 6500-SORT-BENEFITS    THRU 6599-EXIT.               CL*14
00543                                                                   EL6505
00544      IF AM-LAST-MAINT-USER   = PI-UPDATE-BY OR                       CL**5
00545         AM-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS                   EL6505
00546          NEXT SENTENCE                                            EL6505
00547      ELSE                                                         EL6505
00548          EXEC CICS UNLOCK                                         EL6505
00549               DATASET  (ERACCT-FILE)                              EL6505
00550          END-EXEC                                                 EL6505
00551          MOVE ER-0068            TO  EMI-ERROR                    EL6505
00552          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6505
00553          PERFORM 7100-READ-ERACCT  THRU 7100-EXIT                 EL6505
00554          MOVE LOW-VALUES         TO  EL6505AO                     EL6505
00555          MOVE -1                 TO  MAINTYPL                     EL6505
00556          MOVE 'S'                TO  PI-MAINT                     EL6505
00557          GO TO 5000-BUILD-INITIAL-SCREEN.                         EL6505
00558                                                                   EL6505
00559      MOVE PI-PROCESSOR-ID        TO  AM-LAST-MAINT-USER.          EL6505
00560      MOVE EIBTIME                TO  AM-LAST-MAINT-HHMMSS.        EL6505
00561      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL6505
00562      MOVE '5'                    TO  DC-OPTION-CODE.              EL6505
00563      MOVE LINK-ELDATCV           TO  PGM-NAME.                    EL6505
00564                                                                   EL6505
00565      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL6505
00566                                                                   EL6505
00567      MOVE DC-BIN-DATE-1          TO  AM-LAST-MAINT-DT             EL6505
00568                                      BIN-CURRENT-SAVE.            EL6505
00569      MOVE ERACCT-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.      EL6505
00570      MOVE 'B'                    TO  JP-RECORD-TYPE.                 CL**6
00571      PERFORM 8400-LOG-JOURNAL-RECORD.                                CL**6
00572      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA.              EL6505
00573                                                                   EL6505
00574      EXEC CICS REWRITE                                            EL6505
00575          DATASET  (ERACCT-FILE)                                   EL6505
00576          FROM     (ACCOUNT-MASTER)                                EL6505
00577      END-EXEC.                                                    EL6505
00578                                                                   EL6505
00579      MOVE 'C'                    TO  JP-RECORD-TYPE.                 CL**6
00580      MOVE ERACCT-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.      EL6505
00581      MOVE ERACCT-FILE            TO  FILE-ID.                     EL6505
00582      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6505
00583      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.               EL6505
00584      MOVE ER-0000                TO  EMI-ERROR.                   EL6505
00585      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6505
00586                                                                   EL6505
00587      PERFORM 7100-READ-ERACCT THRU 7100-EXIT.                     EL6505
00588      MOVE LOW-VALUES             TO  EL6505AO.                    EL6505
00589      MOVE 'C'                    TO  PI-MAINT.                       CL**2
00590                                                                   EL6505
00591      EJECT                                                        EL6505
00592  5000-BUILD-INITIAL-SCREEN.                                       EL6505
00593      MOVE AM-CARRIER             TO  CARO.                        EL6505
00594      MOVE AM-GROUPING            TO  GROUPO.                      EL6505
00595      MOVE AM-STATE               TO  STATEO                          CL**6
00596                                      PI-WS-STATE.                    CL**6
00597      MOVE AM-ACCOUNT             TO  ACCTO.                       EL6505
00598                                                                   EL6505
00599      MOVE AM-EFFECTIVE-DT        TO  DC-BIN-DATE-1.               EL6505
00600      MOVE ' '                    TO  DC-OPTION-CODE.              EL6505
00601                                                                   EL6505
00602      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL6505
00603                                                                   EL6505
00604      MOVE DC-GREG-DATE-1-EDIT    TO  EFFDTEO.                     EL6505
00605                                                                   EL6505
00606      IF AM-EXPIRATION-DT NOT = HIGH-VALUES                        EL6505
00607         MOVE AM-EXPIRATION-DT    TO  DC-BIN-DATE-1                EL6505
00608         MOVE ' '                 TO  DC-OPTION-CODE               EL6505
00609         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT             EL6505
00610         MOVE DC-GREG-DATE-1-EDIT TO  EXPDTEO                      EL6505
00611      ELSE                                                         EL6505
00612         MOVE '99/99/99'          TO  EXPDTEO.                     EL6505
00613                                                                   EL6505
00614      MOVE AM-BENEFIT-TABLE-USAGE TO  USECODEO.                       CL**3
00615      MOVE AL-UANON               TO  USECODEA.                       CL**3
00616                                                                   EL6505
00617      MOVE +0                     TO  SUB1.                           CL**2
00618      SET BNDX                    TO  SUB1.                           CL**2
00619                                                                   EL6505
00620      IF PI-COMPANY-ID EQUAL 'DMD'                                    CL*14
00621         IF FIRST-OCCURS                                              CL*14
00622             NEXT SENTENCE                                            CL*14
00623         ELSE                                                         CL*14
00624             IF END-OF-FILE                                           CL*14
00625                 MOVE ER-8150     TO  EMI-ERROR                       CL*14
00626                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             CL*14
00627                 MOVE +0          TO SUB1                             CL*15
00628                 SET BNDX TO SUB1                                     CL*15
00629                 MOVE +20         TO SUB1                             CL*14
00630                 MOVE '3'         TO PI-DMD-SCREEN                    CL*14
00631                 GO TO 5125-DMD-MOVE-EXTRA                            CL*14
00632             ELSE                                                     CL*14
00633                 MOVE +0          TO SUB1                             CL*15
00634                 SET BNDX TO SUB1                                     CL*15
00635                 MOVE PI-DMD-OCCURS   TO SUB1                         CL*14
00636                 MOVE '2'             TO PI-DMD-SCREEN                CL*14
00637                 GO TO 5125-DMD-MOVE-EXTRA.                           CL*14
00638                                                                      CL*14
00639  5025-SET-UP-BENEFITS.                                               CL**2
00640      SET BNDX UP BY +1.                                              CL**2
00641      ADD +1                      TO  SUB1.                        EL6505
00642      MOVE '1'                    TO PI-DMD-SCREEN.                   CL*14
00643                                                                   EL6505
00644      IF BNDX GREATER +20                                             CL**5
00645          GO TO 5050-CONT.                                         EL6505
00646                                                                   EL6505
00647      IF AM-BENEFIT-CODE (SUB1) = SPACES  OR  ZEROS                   CL**2
00648          GO TO 5025-SET-UP-BENEFITS.                                 CL**2
00649                                                                      CL**2
00650      MOVE AM-BENEFIT-CODE (SUB1)         TO  BENCODE (BNDX).         CL**2
00651      MOVE AM-BENEFIT-TYPE (SUB1)         TO  BENTYPE (BNDX).         CL**2
00652      MOVE AM-BENEFIT-REVISION (SUB1)     TO  REVNO   (BNDX).         CL**7
00653                                                                      CL*13
00654      IF PI-COMPANY-ID = 'DMD'                                        CL*13
00655         MOVE AM-BENEFIT-RETRO-Y-N (SUB1) TO  RETROYN (BNDX)          CL*13
00656      ELSE                                                            CL*10
00657         IF AM-BENEFIT-RETRO-Y-N (SUB1) = 'N'                         CL*13
00658            MOVE 'N'                      TO  RETROYN (BNDX)          CL*13
00659         ELSE                                                         CL*13
00660            MOVE 'Y'                      TO  RETROYN (BNDX).         CL*13
00661                                                                      CL*13
00662      MOVE AM-BENEFIT-REM-TERM (SUB1)     TO  REMTERM (BNDX).         CL*10
00663                                                                   EL6505
00664      MOVE AL-UANON               TO  BENCODE-A (BNDX)                CL**2
00665                                      BENTYPE-A (BNDX)                CL**2
00666                                      REVNO-A   (BNDX)                CL**7
00667                                      RETROYN-A (BNDX)                CL*10
00668                                      REMTERM-A (BNDX).               CL*10
00669                                                                   EL6505
00670      GO TO 5025-SET-UP-BENEFITS.                                     CL**2
00671                                                                      CL**2
00672  5050-CONT.                                                       EL6505
00673      IF PI-COMPANY-ID EQUAL 'DMD'                                    CL*14
00674          MOVE +0                 TO  PI-DMD-OCCURS                   CL*14
00675          MOVE 'I'                TO  PI-DMD-FILE-SW.                 CL*14
00676                                                                      CL*14
00677      MOVE PI-MAINT               TO  MAINTYPO.                       CL**6
00678      MOVE AL-UANON               TO  MAINTYPA.                       CL**6
00679      MOVE -1                     TO  MAINTYPL.                       CL**6
00680                                                                   EL6505
00681      GO TO 8100-SEND-INITIAL-MAP.                                 EL6505
00682                                                                   EL6505
00683  5099-EXIT.                                                       EL6505
00684      EXIT.                                                           CL*14
00685      EJECT                                                           CL*14
00686  5125-DMD-MOVE-EXTRA.                                                CL*14
00687                                                                      CL*14
00688      SET BNDX UP BY +1.                                              CL*14
00689      ADD +1                      TO  SUB1.                           CL*14
00690      ADD +1                      TO  PI-DMD-OCCURS.                  CL*14
00691                                                                      CL*14
00692      IF BNDX GREATER +20                                             CL*14
00693          SUBTRACT +1             FROM PI-DMD-OCCURS                  CL*14
00694          GO TO 5150-CONT.                                            CL*14
00695                                                                      CL*14
00696      IF PI-DMD-OCCURS GREATER +30                                    CL*14
00697          MOVE +19                TO PI-DMD-OCCURS                    CL*14
00698          MOVE 'E'                TO PI-DMD-FILE-SW                   CL*14
00699          MOVE ER-8150            TO  EMI-ERROR                       CL*14
00700          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*14
00701          GO TO 5150-CONT.                                            CL*14
00702                                                                      CL*14
00703      IF AM-BENEFIT-DMD-CODE (SUB1) = SPACES  OR  ZEROS               CL*14
00704          GO TO 5125-DMD-MOVE-EXTRA.                                  CL*14
00705                                                                      CL*14
00706      MOVE AM-BENEFIT-DMD-CODE (SUB1)     TO  BENCODE (BNDX).         CL*14
00707      MOVE AM-BENEFIT-DMD-TYPE (SUB1)     TO  BENTYPE (BNDX).         CL*14
00708      MOVE AM-BENEFIT-DMD-REVISION (SUB1) TO  REVNO   (BNDX).         CL*14
00709                                                                      CL*14
00710      MOVE AM-BENEFIT-DMD-RETRO-Y-N (SUB1) TO  RETROYN (BNDX).        CL*14
00711                                                                      CL*14
00712      MOVE AM-BENEFIT-DMD-REM-TERM (SUB1)  TO  REMTERM (BNDX).        CL*14
00713                                                                      CL*14
00714      MOVE AL-UANON               TO  BENCODE-A (BNDX)                CL*14
00715                                      BENTYPE-A (BNDX)                CL*14
00716                                      REVNO-A   (BNDX)                CL*14
00717                                      RETROYN-A (BNDX)                CL*14
00718                                      REMTERM-A (BNDX).               CL*14
00719                                                                      CL*14
00720      GO TO 5125-DMD-MOVE-EXTRA.                                      CL*14
00721                                                                      CL*14
00722  5150-CONT.                                                          CL*14
00723      MOVE PI-MAINT               TO  MAINTYPO.                       CL*14
00724      MOVE AL-UANON               TO  MAINTYPA.                       CL*14
00725      MOVE -1                     TO  MAINTYPL.                       CL*14
00726                                                                      CL*14
00727      GO TO 8100-SEND-INITIAL-MAP.                                    CL*14
00728                                                                      CL*14
00729  5199-EXIT.                                                          CL*14
00730      EXIT.                                                        EL6505
00731      EJECT                                                        EL6505
00732                                                                   EL6505
00733  6000-CHECK-FOR-UPDATE.                                           EL6505
00734                                                                      CL**7
00735      IF USECODEL GREATER +0                                          CL**5
00736          MOVE USECODEI         TO  AM-BENEFIT-TABLE-USAGE.           CL**3
00737                                                                      CL**5
00738      MOVE AL-UANON             TO  USECODEA.                         CL**3
00739                                                                      CL**3
00740      MOVE +0                   TO  SUB1                              CL**7
00741      SET BNDX                  TO  SUB1.                             CL*13
00742                                                                   EL6505
00743  6005-CHECK-BENEFITS.                                                CL**7
00744                                                                      CL**7
00745      SET BNDX UP BY +1.                                              CL**2
00746      ADD +1                    TO  SUB1.                             CL**2
00747                                                                   EL6505
00748      IF BNDX GREATER +20                                             CL**5
00749          GO TO 6049-EXIT.                                         EL6505
00750                                                                      CL**7
00751      IF BENCODE-L (BNDX) NOT GREATER THAN +0                         CL**7
00752         GO TO 6030-BYPASS-BENEFIT-CHECK.                             CL**7
00753                                                                      CL**7
00754      IF BENCODE-L (BNDX) GREATER THAN +0                             CL**7
00755         IF BENCODE (BNDX) EQUAL '91' OR '92' OR '93' OR '94'         CL**8
00756                              OR '98' OR '99' OR '  ' OR '00'         CL**8
00757            GO TO 6030-BYPASS-BENEFIT-CHECK.                          CL**7
00758                                                                      CL**7
00759  6010-READ-BEN-CNTL.                                                 CL**7
00760                                                                      CL**7
00761      MOVE SPACES                 TO ELCNTL-KEY.                      CL**7
00762      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                    CL**7
00763                                                                      CL**7
00764      IF BENTYPE (BNDX) EQUAL PI-LIFE-OVERRIDE-L1                     CL**7
00765         MOVE '4'                 TO CNTL-REC-TYPE                    CL**7
00766      ELSE                                                            CL**7
00767         MOVE '5'                 TO CNTL-REC-TYPE.                   CL**7
00768                                                                      CL**7
00769      MOVE BENCODE (BNDX)         TO CNTL-HI-BEN                      CL**7
00770      MOVE +0                     TO CNTL-SEQ-NO.                     CL**7
00771                                                                      CL**7
00772      EXEC CICS HANDLE CONDITION                                      CL**7
00773          NOTFND   (6025-NOT-FOUND)                                   CL**7
00774          ENDFILE  (6025-NOT-FOUND)                                   CL**7
00775      END-EXEC.                                                       CL**7
00776                                                                      CL**7
00777      EXEC CICS READ                                                  CL**7
00778          DATASET   (CNTL-FILE-ID)                                    CL**7
00779          SET       (ADDRESS OF CONTROL-FILE)                         CL*13
00780          RIDFLD    (ELCNTL-KEY)                                      CL**7
00781          GTEQ                                                        CL**7
00782      END-EXEC.                                                       CL**7
00783                                                                      CL**7
00784  6015-BYPASS-ELCNTL-READ.                                            CL**7
00785                                                                      CL**7
00786      PERFORM 6020-BENEFIT-LOOP-DUMMY                                 CL**7
00787        VARYING SUB2 FROM +1 BY +1 UNTIL                              CL**7
00788         CF-BENEFIT-CODE (SUB2) EQUAL BENCODE (BNDX) OR               CL**7
00789          SUB2 GREATER THAN +8.                                       CL**7
00790                                                                      CL**7
00791      IF SUB2 GREATER THAN +8                                         CL**7
00792         GO TO 6025-NOT-FOUND.                                        CL**7
00793                                                                      CL**7
00794      GO TO 6030-BYPASS-BENEFIT-CHECK.                                CL**7
00795                                                                      CL**7
00796  6020-BENEFIT-LOOP-DUMMY.                                            CL**7
00797                                                                      CL**7
00798  6025-NOT-FOUND.                                                     CL**7
00799                                                                      CL**7
00800      MOVE -1                     TO  BENCODE-L (BNDX)                CL**7
00801      MOVE AL-UABON               TO  BENCODE-A (BNDX)                CL**7
00802      MOVE ER-0150                TO  EMI-ERROR                       CL**7
00803      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                        CL**7
00804      GO TO 8200-SEND-DATAONLY.                                       CL**7
00805                                                                      CL**7
00806  6030-BYPASS-BENEFIT-CHECK.                                          CL**7
00807                                                                   EL6505
00808      IF BENCODE-L (BNDX) GREATER +0                                  CL**5
00809          MOVE BENCODE (BNDX)   TO  AM-BENEFIT-CODE (SUB1).           CL**2
00810                                                                      CL**2
00811      IF BENTYPE-L (BNDX) GREATER +0                                  CL**5
00812          MOVE BENTYPE (BNDX)   TO  AM-BENEFIT-TYPE (SUB1).           CL**2
00813                                                                   EL6505
00814      IF REVNO-L (BNDX) GREATER +0                                    CL**7
00815          MOVE REVNO (BNDX)     TO  AM-BENEFIT-REVISION (SUB1).       CL**7
00816                                                                   EL6505
00817      IF RETROYN-L (BNDX) GREATER +0                                  CL*10
00818          MOVE RETROYN (BNDX)   TO  AM-BENEFIT-RETRO-Y-N (SUB1).      CL*10
00819                                                                      CL*10
00820      IF REMTERM-L (BNDX) GREATER +0                                  CL*10
00821          MOVE REMTERM (BNDX)   TO  AM-BENEFIT-REM-TERM (SUB1).       CL*10
00822                                                                   EL6505
00823      MOVE AL-UANON             TO  BENCODE-A (BNDX)                  CL**2
00824                                    BENTYPE-A (BNDX)                  CL**2
00825                                    REVNO-A   (BNDX)                  CL**7
00826                                    RETROYN-A (BNDX)                  CL*10
00827                                    REMTERM-A (BNDX).                 CL*10
00828                                                                      CL**2
00829      IF AM-BENEFIT-CODE (SUB1) = ZEROS                               CL**2
00830          MOVE SPACES           TO AM-ALLOWABLE-BENEFITS (SUB1).      CL**7
00831                                                                      CL**2
00832      GO TO 6005-CHECK-BENEFITS.                                      CL**7
00833                                                                   EL6505
00834  6049-EXIT.                                                       EL6505
00835      EXIT.                                                        EL6505
00836      EJECT                                                        EL6505
00837  6500-SORT-BENEFITS.                                                 CL**7
00838                                                                   EL6505
00839      MOVE +1                     TO SUB2                             CL**7
00840                                     SUB3                             CL**7
00841      MOVE SPACES                 TO WS-WORK-BENEFIT-TABLE.           CL**7
00842                                                                      CL**6
00843  6510-BENEFIT-MOVE.                                                  CL**7
00844                                                                      CL**6
00845      IF AM-BENEFIT-CODE (SUB2) NOT EQUAL SPACES AND ZEROS            CL**7
00846         MOVE AM-ALLOWABLE-BENEFITS (SUB2)                            CL**7
00847                                  TO WS-WORK-TABLE (SUB3)             CL**7
00848         MOVE SPACES              TO WS-FILLER (SUB3)                 CL**7
00849         ADD +1                   TO SUB3.                            CL**7
00850                                                                      CL**6
00851      ADD +1                      TO SUB2                             CL**7
00852      IF SUB2 NOT GREATER THAN +20                                    CL**7
00853         GO TO 6510-BENEFIT-MOVE.                                     CL**7
00854                                                                      CL**6
00855      MOVE WS-WORK-BENEFIT-TABLE  TO AM-BENEFIT-CONTROLS              CL**7
00856                                                                      CL**6
00857      IF AM-BENEFIT-CONTROLS EQUAL SPACES                             CL**7
00858         GO TO 6599-EXIT.                                             CL**7
00859                                                                      CL**6
00860      MOVE +1                     TO SUB2                             CL**7
00861                                     SUB3                             CL**7
00862      MOVE HIGH-VALUES            TO WS-WORK-BENEFIT-TABLE.           CL**7
00863                                                                      CL**6
00864  6520-CONTINUE-SORT.                                                 CL**7
00865                                                                      CL**6
00866      IF SUB3 GREATER THAN +20                                        CL**7
00867         GO TO 6540-RID-OF-HIGH-VALUES.                               CL**7
00868                                                                      CL**6
00869      IF (AM-BENEFIT-CODE (SUB2) LESS THAN WS-WORK-BENEFIT (SUB3))    CL**7
00870        AND                                                           CL**7
00871         (AM-BENEFIT-CODE (SUB2) NOT EQUAL SPACES)                    CL**7
00872         MOVE AM-ALLOWABLE-BENEFITS (SUB2)                            CL**7
00873                                  TO WS-WORK-TABLE (SUB3)             CL**7
00874         MOVE SPACES              TO WS-FILLER (SUB3).                CL**7
00875                                                                      CL**6
00876      ADD +1 TO SUB2                                                  CL**7
00877                                                                      CL**7
00878      IF SUB2 NOT GREATER THAN +20                                    CL**7
00879         GO TO 6520-CONTINUE-SORT.                                    CL**7
00880                                                                      CL**7
00881      PERFORM 6530-DUMMY VARYING SUB2 FROM +1 BY +1                   CL**7
00882      UNTIL WS-WORK-BENEFIT (SUB3) EQUAL                              CL**7
00883            AM-BENEFIT-CODE (SUB2) OR                                 CL**7
00884            SUB2 GREATER THAN +20.                                    CL**7
00885                                                                      CL**7
00886      IF SUB2 GREATER THAN +20                                        CL**7
00887         MOVE +1 TO SUB2                                              CL**7
00888         GO TO 6540-RID-OF-HIGH-VALUES.                               CL**7
00889                                                                      CL**7
00890      MOVE HIGH-VALUES            TO AM-ALLOWABLE-BENEFITS (SUB2)     CL**7
00891      ADD +1                      TO SUB3                             CL**7
00892      MOVE +1                     TO SUB2                             CL**7
00893      GO TO 6520-CONTINUE-SORT.                                       CL**7
00894                                                                      CL**7
00895  6530-DUMMY.                                                         CL**7
00896                                                                      CL**7
00897  6540-RID-OF-HIGH-VALUES.                                            CL**7
00898                                                                      CL**7
00899      IF WS-WORK-TABLE (SUB2) EQUAL HIGH-VALUES                       CL**7
00900         MOVE SPACES               TO WS-WORK-TABLE (SUB2).           CL**7
00901                                                                      CL**7
00902      ADD +1 TO SUB2                                                  CL**7
00903                                                                      CL**7
00904      IF SUB2 NOT GREATER THAN +20                                    CL**7
00905         GO TO 6540-RID-OF-HIGH-VALUES.                               CL**7
00906                                                                      CL**7
00907      MOVE +1                     TO SUB2                             CL**7
00908      MOVE +2                     TO SUB3.                            CL**7
00909                                                                      CL**7
00910  6550-CHECK-DUPS.                                                    CL**7
00911                                                                      CL**7
00912      IF WS-WORK-BENEFIT (SUB2) EQUAL WS-WORK-BENEFIT (SUB3) AND      CL**7
00913         WS-WORK-TYPE (SUB2)    EQUAL WS-WORK-TYPE (SUB3) AND         CL**7
00914         WS-WORK-BENEFIT (SUB2) NOT EQUAL SPACES                      CL**7
00915         MOVE ER-2947             TO  EMI-ERROR                       CL**8
00916         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     CL**7
00917         MOVE -1                  TO  MAINTYPL                        CL**7
00918         GO TO 8200-SEND-DATAONLY.                                    CL**7
00919                                                                      CL**7
00920      ADD +1 TO SUB3                                                  CL**7
00921                                                                      CL**7
00922      IF SUB3 NOT GREATER THAN +20                                    CL**7
00923         GO TO 6550-CHECK-DUPS.                                       CL**7
00924                                                                      CL**7
00925      ADD +1                      TO SUB2                             CL**7
00926      MOVE SUB2                   TO SUB3                             CL**7
00927      ADD +1                      TO SUB3                             CL**7
00928                                                                      CL**7
00929      IF SUB2 NOT GREATER THAN +20                                    CL**7
00930         GO TO 6550-CHECK-DUPS.                                       CL**7
00931                                                                      CL**7
00932      MOVE WS-WORK-BENEFIT-TABLE  TO AM-BENEFIT-CONTROLS.             CL**7
00933                                                                      CL**7
00934  6599-EXIT.                                                          CL**7
00935      EXIT.                                                           CL**7
00936                                                                      CL**7
00937  6600-CHECK-FOR-UPDATE.                                              CL*14
00938                                                                      CL*14
00939      IF USECODEL GREATER +0                                          CL*14
00940          MOVE USECODEI         TO  AM-BENEFIT-TABLE-USAGE.           CL*14
00941                                                                      CL*14
00942      MOVE AL-UANON             TO  USECODEA.                         CL*14
00943                                                                      CL*14
00944      MOVE +0                   TO  SUB1.                             CL*14
00945      SET BNDX                  TO  SUB1.                             CL*14
00946      MOVE +20                  TO  WS-MAX-BNDX.                      CL*14
00947                                                                      CL*14
00948      IF SCREEN-1-DISPLAYED                                           CL*14
00949          NEXT SENTENCE                                               CL*14
00950      ELSE                                                            CL*14
00951          IF SCREEN-2-DISPLAYED                                       CL*14
00952              MOVE +20              TO SUB1                           CL*14
00953          ELSE                                                        CL*14
00954              MOVE +40              TO SUB1                           CL*14
00955              MOVE +10              TO WS-MAX-BNDX.                   CL*14
00956                                                                      CL*14
00957  6605-CHECK-BENEFITS.                                                CL*14
00958                                                                      CL*14
00959      SET BNDX UP BY +1.                                              CL*14
00960      ADD +1                    TO  SUB1.                             CL*14
00961                                                                      CL*14
00962      IF BNDX GREATER WS-MAX-BNDX                                     CL*14
00963          GO TO 6649-EXIT.                                            CL*14
00964                                                                      CL*14
00965      IF BENCODE-L (BNDX) NOT GREATER THAN +0                         CL*14
00966         GO TO 6630-BYPASS-BENEFIT-CHECK.                             CL*14
00967                                                                      CL*14
00968      IF BENCODE-L (BNDX) GREATER THAN +0                             CL*14
00969         IF BENCODE (BNDX) EQUAL '91' OR '92' OR '93' OR '94'         CL*14
00970                              OR '98' OR '99' OR '  ' OR '00'         CL*14
00971            GO TO 6630-BYPASS-BENEFIT-CHECK.                          CL*14
00972                                                                      CL*14
00973  6610-READ-BEN-CNTL.                                                 CL*14
00974                                                                      CL*14
00975      MOVE SPACES                 TO ELCNTL-KEY.                      CL*14
00976      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                    CL*14
00977                                                                      CL*14
00978      IF BENTYPE (BNDX) EQUAL PI-LIFE-OVERRIDE-L1                     CL*14
00979         MOVE '4'                 TO CNTL-REC-TYPE                    CL*14
00980      ELSE                                                            CL*14
00981         MOVE '5'                 TO CNTL-REC-TYPE.                   CL*14
00982                                                                      CL*14
00983      MOVE BENCODE (BNDX)         TO CNTL-HI-BEN                      CL*14
00984      MOVE +0                     TO CNTL-SEQ-NO.                     CL*14
00985                                                                      CL*14
00986      EXEC CICS HANDLE CONDITION                                      CL*14
00987          NOTFND   (6625-NOT-FOUND)                                   CL*14
00988          ENDFILE  (6625-NOT-FOUND)                                   CL*14
00989      END-EXEC.                                                       CL*14
00990                                                                      CL*14
00991      EXEC CICS READ                                                  CL*14
00992          DATASET   (CNTL-FILE-ID)                                    CL*14
00993          SET       (ADDRESS OF CONTROL-FILE)                         CL*14
00994          RIDFLD    (ELCNTL-KEY)                                      CL*14
00995          GTEQ                                                        CL*14
00996      END-EXEC.                                                       CL*14
00997                                                                      CL*14
00998  6615-BYPASS-ELCNTL-READ.                                            CL*14
00999                                                                      CL*14
01000      PERFORM 6620-BENEFIT-LOOP-DUMMY                                 CL*14
01001        VARYING SUB2 FROM +1 BY +1 UNTIL                              CL*14
01002         CF-BENEFIT-CODE (SUB2) EQUAL BENCODE (BNDX) OR               CL*14
01003          SUB2 GREATER THAN +8.                                       CL*14
01004                                                                      CL*14
01005      IF SUB2 GREATER THAN +8                                         CL*14
01006         GO TO 6625-NOT-FOUND.                                        CL*14
01007                                                                      CL*14
01008      GO TO 6630-BYPASS-BENEFIT-CHECK.                                CL*14
01009                                                                      CL*14
01010  6620-BENEFIT-LOOP-DUMMY.                                            CL*14
01011                                                                      CL*14
01012  6625-NOT-FOUND.                                                     CL*14
01013                                                                      CL*14
01014      MOVE -1                     TO  BENCODE-L (BNDX)                CL*14
01015      MOVE AL-UABON               TO  BENCODE-A (BNDX)                CL*14
01016      MOVE ER-0150                TO  EMI-ERROR                       CL*14
01017      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                        CL*14
01018      GO TO 8200-SEND-DATAONLY.                                       CL*14
01019                                                                      CL*14
01020  6630-BYPASS-BENEFIT-CHECK.                                          CL*14
01021                                                                      CL*14
01022      IF BENCODE-L (BNDX) GREATER +0                                  CL*14
01023          MOVE BENCODE (BNDX) TO  WS-DMD-WORK-BENEFIT-2 (SUB1).       CL*14
01024                                                                      CL*14
01025      IF BENTYPE-L (BNDX) GREATER +0                                  CL*14
01026          MOVE BENTYPE (BNDX) TO  WS-DMD-WORK-TYPE-2 (SUB1).          CL*14
01027                                                                      CL*14
01028      IF REVNO-L (BNDX) GREATER +0                                    CL*14
01029          MOVE REVNO (BNDX)   TO  WS-DMD-WORK-REVISION-2 (SUB1).      CL*14
01030                                                                      CL*14
01031      IF RETROYN-L (BNDX) GREATER +0                                  CL*14
01032          MOVE RETROYN (BNDX) TO  WS-DMD-WORK-RETRO-Y-N-2 (SUB1).     CL*14
01033                                                                      CL*14
01034      IF REMTERM-L (BNDX) GREATER +0                                  CL*14
01035          MOVE REMTERM (BNDX)   TO  WS-DMD-WORK-REM-TERM-2 (SUB1).    CL*14
01036                                                                      CL*14
01037      MOVE AL-UANON             TO  BENCODE-A (BNDX)                  CL*14
01038                                    BENTYPE-A (BNDX)                  CL*14
01039                                    REVNO-A   (BNDX)                  CL*14
01040                                    RETROYN-A (BNDX)                  CL*14
01041                                    REMTERM-A (BNDX).                 CL*14
01042                                                                      CL*14
01043      IF WS-DMD-WORK-BENEFIT-2 (SUB1) = ZEROS                         CL*14
01044          MOVE SPACES           TO WS-DMD-WORK-TABLE-2 (SUB1).        CL*14
01045                                                                      CL*14
01046      GO TO 6605-CHECK-BENEFITS.                                      CL*14
01047                                                                      CL*14
01048  6649-EXIT.                                                          CL*14
01049      EXIT.                                                           CL*14
01050  6700-LOAD-BENEFIT-CODES.                                            CL*14
01051                                                                      CL*14
01052      MOVE AM-ALLOWABLE-BENEFITS (SUB1)                               CL*14
01053                                 TO WS-DMD-WORK-TABLE-2 (SUB2).       CL*14
01054      MOVE SPACES                TO WS-DMD-FILLER-2 (SUB2).           CL*14
01055                                                                      CL*14
01056      ADD +1                     TO SUB1                              CL*14
01057                                    SUB2.                             CL*14
01058                                                                      CL*14
01059      IF SUB1 NOT GREATER THAN +20                                    CL*14
01060         GO TO 6700-LOAD-BENEFIT-CODES.                               CL*14
01061                                                                      CL*14
01062      MOVE +1                    TO SUB1.                             CL*14
01063                                                                      CL*14
01064  6700-LOAD-CONTINUE.                                                 CL*14
01065                                                                      CL*14
01066      MOVE AM-ALLOWABLE-DMD-BENEFITS (SUB1)                           CL*14
01067                                 TO WS-DMD-WORK-TABLE-2 (SUB2).       CL*14
01068      MOVE SPACES                TO WS-DMD-FILLER-2 (SUB2).           CL*14
01069                                                                      CL*14
01070      ADD +1                     TO SUB1                              CL*14
01071                                    SUB2.                             CL*14
01072                                                                      CL*14
01073      IF SUB1 NOT GREATER THAN +30                                    CL*14
01074         GO TO 6700-LOAD-CONTINUE.                                    CL*14
01075                                                                      CL*14
01076  6700-LOAD-EXIT.                                                     CL*14
01077  EJECT                                                               CL*14
01078  6800-SORT-BENEFITS.                                                 CL*14
01079                                                                      CL*14
01080      IF WS-DMD-WORK-BENEFIT-TABLE-2 EQUAL SPACES                     CL*14
01081         GO TO 6899-EXIT.                                             CL*14
01082                                                                      CL*14
01083      MOVE +1                     TO SUB2                             CL*14
01084                                     SUB3.                            CL*14
01085                                                                      CL*14
01086      MOVE HIGH-VALUES            TO WS-DMD-WORK-BENEFIT-TABLE.       CL*14
01087                                                                      CL*14
01088                                                                      CL*14
01089  6820-CONTINUE-SORT.                                                 CL*14
01090                                                                      CL*14
01091      IF SUB3 GREATER THAN +50                                        CL*14
01092         GO TO 6840-RID-OF-HIGH-VALUES.                               CL*14
01093                                                                      CL*14
01094      IF (WS-DMD-WORK-BENEFIT-2 (SUB2) LESS THAN                      CL*14
01095                           WS-DMD-WORK-BENEFIT (SUB3))                CL*14
01096        AND                                                           CL*14
01097         (WS-DMD-WORK-BENEFIT-2 (SUB2) NOT EQUAL SPACES)              CL*14
01098         MOVE WS-DMD-WORK-TABLE-2 (SUB2)                              CL*14
01099                                  TO WS-DMD-WORK-TABLE (SUB3)         CL*14
01100         MOVE SPACES              TO WS-DMD-FILLER (SUB3).            CL*14
01101                                                                      CL*14
01102      ADD +1                      TO SUB2.                            CL*14
01103                                                                      CL*14
01104      IF SUB2 NOT GREATER THAN +50                                    CL*14
01105         GO TO 6820-CONTINUE-SORT.                                    CL*14
01106                                                                      CL*14
01107      PERFORM 6830-DUMMY VARYING SUB2 FROM +1 BY +1                   CL*14
01108      UNTIL WS-DMD-WORK-BENEFIT (SUB3) EQUAL                          CL*14
01109            WS-DMD-WORK-BENEFIT-2 (SUB2) OR                           CL*14
01110            SUB2 GREATER THAN +50.                                    CL*14
01111                                                                      CL*14
01112      IF SUB2 GREATER THAN +50                                        CL*14
01113         MOVE +1                  TO SUB2                             CL*14
01114         GO TO 6840-RID-OF-HIGH-VALUES.                               CL*14
01115                                                                      CL*14
01116      MOVE HIGH-VALUES         TO WS-DMD-WORK-BENEFIT-2 (SUB2).       CL*14
01117      ADD  +1                  TO SUB3.                               CL*14
01118      MOVE +1                  TO SUB2.                               CL*14
01119      GO TO 6820-CONTINUE-SORT.                                       CL*14
01120                                                                      CL*14
01121  6830-DUMMY.                                                         CL*14
01122                                                                      CL*14
01123  6840-RID-OF-HIGH-VALUES.                                            CL*14
01124                                                                      CL*14
01125      IF WS-DMD-WORK-TABLE (SUB2) EQUAL HIGH-VALUES                   CL*14
01126         MOVE SPACES               TO WS-DMD-WORK-TABLE (SUB2).       CL*14
01127                                                                      CL*14
01128      ADD +1                       TO SUB2.                           CL*14
01129                                                                      CL*14
01130      IF SUB2 NOT GREATER THAN +50                                    CL*14
01131         GO TO 6840-RID-OF-HIGH-VALUES.                               CL*14
01132                                                                      CL*14
01133      MOVE +1                     TO SUB2                             CL*14
01134      MOVE +2                     TO SUB3.                            CL*14
01135                                                                      CL*14
01136  6850-CHECK-DUPS.                                                    CL*14
01137                                                                      CL*14
01138      IF WS-DMD-WORK-BENEFIT (SUB2) EQUAL                             CL*14
01139                                 WS-DMD-WORK-BENEFIT (SUB3) AND       CL*14
01140         WS-DMD-WORK-TYPE (SUB2) EQUAL WS-DMD-WORK-TYPE (SUB3) AND    CL*14
01141         WS-DMD-WORK-BENEFIT (SUB2) NOT EQUAL SPACES                  CL*14
01142         MOVE ER-2947             TO  EMI-ERROR                       CL*14
01143         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     CL*14
01144         MOVE -1                  TO  MAINTYPL                        CL*14
01145         GO TO 8200-SEND-DATAONLY.                                    CL*14
01146                                                                      CL*14
01147      ADD +1                     TO SUB3.                             CL*14
01148                                                                      CL*14
01149      IF SUB3 NOT GREATER THAN +50                                    CL*14
01150         GO TO 6850-CHECK-DUPS.                                       CL*14
01151                                                                      CL*14
01152      ADD +1                      TO SUB2                             CL*14
01153      MOVE SUB2                   TO SUB3                             CL*14
01154      ADD +1                      TO SUB3                             CL*14
01155                                                                      CL*14
01156      IF SUB2 NOT GREATER THAN +50                                    CL*14
01157         GO TO 6850-CHECK-DUPS.                                       CL*14
01158                                                                      CL*14
01159  6899-EXIT.                                                          CL*14
01160      EXIT.                                                           CL*14
01161      EJECT                                                           CL**7
01162                                                                      CL**6
01163                                                                      CL*14
01164  6900-LOAD-ACCOUNT-MASTER.                                           CL*14
01165                                                                      CL*14
01166      MOVE WS-DMD-WORK-TABLE (SUB2)                                   CL*14
01167                                 TO AM-ALLOWABLE-BENEFITS (SUB1).     CL*14
01168                                                                      CL*14
01169      ADD +1                     TO SUB1                              CL*14
01170                                    SUB2.                             CL*14
01171                                                                      CL*14
01172      IF SUB1 NOT GREATER THAN +20                                    CL*14
01173         GO TO 6900-LOAD-ACCOUNT-MASTER.                              CL*14
01174                                                                      CL*14
01175      MOVE +1                    TO SUB1.                             CL*14
01176                                                                      CL*14
01177  6900-LOAD-CONTINUE.                                                 CL*14
01178                                                                      CL*14
01179      MOVE WS-DMD-WORK-TABLE (SUB2)                                   CL*14
01180                              TO AM-ALLOWABLE-DMD-BENEFITS (SUB1).    CL*14
01181                                                                      CL*14
01182      ADD +1                     TO SUB1                              CL*14
01183                                    SUB2.                             CL*14
01184                                                                      CL*14
01185      IF SUB1 NOT GREATER THAN +30                                    CL*14
01186         GO TO 6900-LOAD-CONTINUE.                                    CL*14
01187                                                                      CL*14
01188      IF SCREEN-1-DISPLAYED                                           CL*14
01189          MOVE 'F'                TO PI-DMD-FILE-SW                   CL*14
01190          MOVE +0                 TO PI-DMD-OCCURS                    CL*14
01191      ELSE                                                            CL*14
01192          IF SCREEN-2-DISPLAYED                                       CL*14
01193              MOVE 'I'            TO PI-DMD-FILE-SW                   CL*14
01194              MOVE +0             TO PI-DMD-OCCURS                    CL*14
01195          ELSE                                                        CL*14
01196              IF SCREEN-3-DISPLAYED                                   CL*14
01197                  MOVE 'E'        TO PI-DMD-FILE-SW                   CL*14
01198                  MOVE +20        TO PI-DMD-OCCURS.                   CL*14
01199                                                                      CL*14
01200  6900-LOAD-EXIT.                                                     CL*14
01201  EJECT                                                               CL*14
01202  7000-EDIT.                                                       EL6505
01203                                                                      CL**7
01204      IF USECODEL GREATER ZERO                                        CL**3
01205          MOVE USECODEI         TO  WS-EDIT-USECODE                   CL**3
01206          IF NOT VALID-USE-CODE                                       CL**3
01207              MOVE -1           TO  USECODEL                          CL**3
01208              MOVE AL-UABON     TO  USECODEA                          CL**3
01209              MOVE ER-7240      TO  EMI-ERROR                         CL**3
01210              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**3
01211                                                                   EL6505
01212      MOVE +0                   TO  SUB1.                             CL**2
01213      SET BNDX                  TO  SUB1.                             CL**2
01214                                                                      CL**2
01215  7025-EDIT-BENEFITS.                                                 CL**2
01216      SET BNDX UP BY +1.                                              CL**2
01217      ADD +1                    TO  SUB1.                             CL**2
01218                                                                      CL**2
01219      IF BNDX GREATER +20                                             CL**5
01220          GO TO 7099-EXIT.                                         EL6505
01221                                                                   EL6505
01222      IF BENCODE-L (BNDX) GREATER ZERO                                CL**2
01223          MOVE BENCODE (BNDX)   TO  WS-EDIT-BENCODE.                  CL**8
01224                                                                   EL6505
01225      IF BENTYPE-L (BNDX) GREATER ZERO  OR                            CL**2
01226         BENCODE-L (BNDX) GREATER ZERO                                CL**2
01227          IF BENTYPE (BNDX) = PI-LIFE-OVERRIDE-L1  OR                 CL**2
01228                              PI-AH-OVERRIDE-L1                       CL**6
01229              MOVE AL-UANON     TO  BENTYPE-A (BNDX)                  CL**2
01230          ELSE                                                        CL**2
01231              MOVE -1           TO  BENTYPE-L (BNDX)                  CL**2
01232              MOVE AL-UABON     TO  BENTYPE-A (BNDX)                  CL**2
01233              MOVE ER-0151      TO  EMI-ERROR                         CL**2
01234              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**2
01235                                                                   EL6505
01236      IF (BENTYPE-L (BNDX) GREATER ZERO  OR                           CL**8
01237         BENCODE-L (BNDX) GREATER ZERO) AND                           CL**8
01238         REVNO-L (BNDX) EQUAL +0 AND                                  CL**8
01239         REVNO (BNDX) EQUAL LOW-VALUES OR SPACES                      CL**8
01240         MOVE '000'               TO REVNO (BNDX)                     CL**8
01241         MOVE AL-UANON            TO REVNO-L (BNDX).                  CL**8
01242                                                                      CL**8
01243      IF REVNO-L (BNDX) GREATER THAN +0                               CL**8
01244         IF REVNO (BNDX) NOT EQUAL SPACES                             CL**8
01245            MOVE AL-UANON     TO  REVNO-A   (BNDX)                    CL**8
01246         ELSE                                                         CL**8
01247            MOVE -1           TO  REVNO-L   (BNDX)                    CL**8
01248            MOVE AL-UABON     TO  REVNO-A   (BNDX)                    CL**8
01249            MOVE ER-2948      TO  EMI-ERROR                           CL**8
01250            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL**8
01251                                                                   EL6505
01252      IF BENTYPE-L (BNDX) NOT GREATER ZERO  AND                       CL*13
01253         BENCODE-L (BNDX) NOT GREATER ZERO  AND                       CL*13
01254         REVNO-L (BNDX)   NOT GREATER ZERO                            CL*13
01255         GO TO 7025-CONTINUE.                                         CL*13
01256                                                                      CL*13
01257      IF PI-COMPANY-ID = 'DMD'                                        CL*13
01258          IF RETROYN (BNDX) = 'R'  OR  'C'  OR  'S'                   CL*13
01259              GO TO 7025-CONTINUE                                     CL*13
01260          ELSE                                                        CL*13
01261              MOVE -1           TO  RETROYN-L   (BNDX)                CL*13
01262              MOVE AL-UABON     TO  RETROYN-A   (BNDX)                CL*13
01263              MOVE ER-8034      TO  EMI-ERROR                         CL*13
01264              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*13
CIDMOD         END-IF
01265      ELSE                                                            CL*13
CIDMOD*        IF RETROYN (BNDX) = ' '  OR  'Y'  OR  'N'                   CL*10
CIDMOD         IF RETROYN-L (BNDX) GREATER THAN +0                           000
CIDMOD             MOVE 'Y'          TO  RETROYN     (BNDX)                CL*10
01269          ELSE                                                        CL*10
01270              MOVE -1           TO  RETROYN-L   (BNDX)                CL*10
01271              MOVE AL-UABON     TO  RETROYN-A   (BNDX)                CL*10
01272              MOVE ER-0627      TO  EMI-ERROR                         CL*10
01273              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*10
CIDMOD         END-IF
CIDMOD     END-IF.
01274                                                                      CL*10
01275  7025-CONTINUE.                                                      CL*13
01276      IF REMTERM-L (BNDX) GREATER ZERO                                CL*10
01277          MOVE REMTERM (BNDX)   TO  WS-EDIT-REMTERM                   CL*10
01278          IF NOT VALID-REM-TERM                                       CL*10
01279              MOVE -1           TO  REMTERM-L (BNDX)                  CL*10
01280              MOVE AL-UABON     TO  REMTERM-A (BNDX)                  CL*10
01281              MOVE ER-2298      TO  EMI-ERROR                         CL*10
01282              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*10
01283                                                                   EL6505
01284      GO TO 7025-EDIT-BENEFITS.                                       CL**2
01285                                                                   EL6505
01286  7099-EXIT.                                                       EL6505
01287      EXIT.                                                        EL6505
01288      EJECT                                                        EL6505
01289  7100-READ-ERACCT.                                                EL6505
01290      EXEC CICS READ                                               EL6505
01291           DATASET  (ERACCT-FILE)                                  EL6505
01292           SET      (ADDRESS OF ACCOUNT-MASTER)                       CL*13
01293           RIDFLD   (PI-ACCT-KEY)                                  EL6505
01294      END-EXEC.                                                    EL6505
01295                                                                   EL6505
01296      MOVE AM-LAST-MAINT-USER     TO  PI-UPDATE-BY.                EL6505
01297      MOVE AM-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.            EL6505
01298                                                                   EL6505
01299  7100-EXIT.                                                       EL6505
01300      EXIT.                                                        EL6505
01301      EJECT                                                        EL6505
01302  EJECT                                                               CL**6
01303  7200-DEEDIT.                                                     EL6505
01304      EXEC CICS BIF                                                EL6505
01305           DEEDIT                                                  EL6505
01306           FIELD  (DEEDIT-FIELD)                                   EL6505
01307           LENGTH (15)                                             EL6505
01308      END-EXEC.                                                    EL6505
01309                                                                   EL6505
01310  7200-EXIT.                                                       EL6505
01311      EXIT.                                                        EL6505
01312      EJECT                                                        EL6505
01313  7300-READ-ERACCT-UPDATE.                                         EL6505
01314      EXEC CICS READ                                               EL6505
01315           DATASET  (ERACCT-FILE)                                  EL6505
01316           SET      (ADDRESS OF ACCOUNT-MASTER)                       CL*13
01317           RIDFLD   (PI-ACCT-KEY)                                  EL6505
01318           UPDATE                                                  EL6505
01319      END-EXEC.                                                    EL6505
01320                                                                   EL6505
01321  7300-EXIT.                                                       EL6505
01322      EXIT.                                                        EL6505
01323      EJECT                                                        EL6505
01324  7800-COMPANY-REC-READ.                                           EL6505
01325                                                                      CL**7
01326      MOVE SPACES                 TO  ELCNTL-KEY.                  EL6505
01327      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL6505
01328      MOVE '1'                    TO  CNTL-REC-TYPE.               EL6505
01329      MOVE +0                     TO  CNTL-SEQ-NO.                 EL6505
01330      EXEC CICS HANDLE CONDITION                                   EL6505
01331          NOTFND   (7880-NO-COMP)                                  EL6505
01332      END-EXEC.                                                    EL6505
01333                                                                   EL6505
01334      EXEC CICS READ                                               EL6505
01335          DATASET   (CNTL-FILE-ID)                                 EL6505
01336          SET       (ADDRESS OF CONTROL-FILE)                         CL*13
01337          RIDFLD    (ELCNTL-KEY)                                   EL6505
01338      END-EXEC.                                                    EL6505
01339                                                                   EL6505
01340      IF CF-ACCOUNT-MSTR-MAINT-DT = LOW-VALUES                     EL6505
01341          MOVE ER-2572            TO  EMI-ERROR                    EL6505
01342          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL6505
01343                                                                   EL6505
01344      GO TO 7899-EXIT.                                             EL6505
01345                                                                   EL6505
01346  7880-NO-COMP.                                                    EL6505
01347      MOVE ER-0002                TO  EMI-ERROR.                      CL**6
01348      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6505
01349                                                                   EL6505
01350  7899-EXIT.                                                       EL6505
01351      EXIT.                                                        EL6505
01352      EJECT                                                        EL6505
01353  8000-UPDATE-MAINT-DATE.                                          EL6505
01354      MOVE SPACES                 TO  ELCNTL-KEY.                  EL6505
01355                                                                   EL6505
01356      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL6505
01357      MOVE '1'                    TO  CNTL-REC-TYPE.               EL6505
01358      MOVE +0                     TO  CNTL-SEQ-NO.                 EL6505
01359                                                                   EL6505
01360      EXEC CICS HANDLE CONDITION                                   EL6505
01361          NOTFND   (8000-EXIT)                                     EL6505
01362      END-EXEC.                                                    EL6505
01363                                                                   EL6505
01364      EXEC CICS READ                                               EL6505
01365          UPDATE                                                   EL6505
01366          DATASET   (CNTL-FILE-ID)                                 EL6505
01367          SET       (ADDRESS OF CONTROL-FILE)                         CL*13
01368          RIDFLD    (ELCNTL-KEY)                                   EL6505
01369      END-EXEC.                                                    EL6505
01370                                                                   EL6505
01371      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL6505
01372      MOVE 'B'                    TO  JP-RECORD-TYPE.              EL6505
01373      MOVE ELCNTL-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.      EL6505
01374      MOVE CNTL-FILE-ID           TO  FILE-ID.                     EL6505
01375      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6505
01376                                                                   EL6505
01377      MOVE BIN-CURRENT-SAVE       TO  CF-ACCOUNT-MSTR-MAINT-DT.    EL6505
01378                                                                   EL6505
01379      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL6505
01380      MOVE 'C'                    TO  JP-RECORD-TYPE.              EL6505
01381      MOVE CNTL-FILE-ID           TO  FILE-ID.                     EL6505
01382                                                                   EL6505
01383      EXEC CICS REWRITE                                            EL6505
01384          DATASET   (CNTL-FILE-ID)                                 EL6505
01385          FROM      (CONTROL-FILE)                                    CL**5
01386      END-EXEC.                                                    EL6505
01387                                                                   EL6505
01388      MOVE ELCNTL-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.      EL6505
01389      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6505
01390                                                                   EL6505
01391  8000-EXIT.                                                       EL6505
01392       EXIT.                                                       EL6505
01393      EJECT                                                        EL6505
01394  8100-SEND-INITIAL-MAP.                                           EL6505
01395      MOVE SAVE-DATE              TO  DATEO.                       EL6505
01396      MOVE EIBTIME                TO  TIME-IN.                     EL6505
01397      MOVE TIME-OUT               TO  TIMEO.                       EL6505
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01398      MOVE -1                     TO  PFENTERL.                       CL**6
01399      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    EL6505
01400                                                                      CL**2
01401      MOVE 'RETRO'                TO  RETRO1I                         CL*13
01402                                      RETRO2I.                        CL*13
01403      MOVE ' Y/N '                TO  RETRO3I                         CL*13
01404                                      RETRO4I.                        CL*13
01405      IF PI-COMPANY-ID NOT = 'DMD'                                    CL*14
01406         GO TO 8100-SEND-CONTINUE.                                    CL*14
01407                                                                      CL*13
01408      MOVE ' CONT'               TO  RETRO1I                          CL*14
01409                                     RETRO2I.                         CL*14
01410                                                                      CL*14
01411      MOVE ' TYPE'               TO  RETRO3I                          CL*14
01412                                     RETRO4I.                         CL*14
01413                                                                      CL*14
01414      MOVE 'PF1=BROWSE FORWARD'  TO  PF1KEYI                          CL*14
01415      MOVE 'PF2=BROWSE BACK   '  TO  PF2KEYI.                         CL*14
01416                                                                      CL*14
01417      IF SCREEN-2-DISPLAYED                                           CL*14
01418         MOVE '21. '             TO  NUMB01I                          CL*14
01419         MOVE '22. '             TO  NUMB02I                          CL*14
01420         MOVE '23. '             TO  NUMB03I                          CL*14
01421         MOVE '24. '             TO  NUMB04I                          CL*14
01422         MOVE '25. '             TO  NUMB05I                          CL*14
01423         MOVE '26. '             TO  NUMB06I                          CL*14
01424         MOVE '27. '             TO  NUMB07I                          CL*14
01425         MOVE '28. '             TO  NUMB08I                          CL*14
01426         MOVE '29. '             TO  NUMB09I                          CL*14
01427         MOVE '30. '             TO  NUMB10I                          CL*14
01428         MOVE '31. '             TO  NUMB11I                          CL*14
01429         MOVE '32. '             TO  NUMB12I                          CL*14
01430         MOVE '33. '             TO  NUMB13I                          CL*14
01431         MOVE '34. '             TO  NUMB14I                          CL*14
01432         MOVE '35. '             TO  NUMB15I                          CL*14
01433         MOVE '36. '             TO  NUMB16I                          CL*14
01434         MOVE '37. '             TO  NUMB17I                          CL*14
01435         MOVE '38. '             TO  NUMB18I                          CL*14
01436         MOVE '39. '             TO  NUMB19I                          CL*14
01437         MOVE '40. '             TO  NUMB20I                          CL*14
01438      ELSE                                                            CL*14
01439         IF SCREEN-3-DISPLAYED                                        CL*14
01440             MOVE '41. '             TO  NUMB01I                      CL*14
01441             MOVE '42. '             TO  NUMB02I                      CL*14
01442             MOVE '43. '             TO  NUMB03I                      CL*14
01443             MOVE '44. '             TO  NUMB04I                      CL*14
01444             MOVE '45. '             TO  NUMB05I                      CL*14
01445             MOVE '46. '             TO  NUMB06I                      CL*14
01446             MOVE '47. '             TO  NUMB07I                      CL*14
01447             MOVE '48. '             TO  NUMB08I                      CL*14
01448             MOVE '49. '             TO  NUMB09I                      CL*14
01449             MOVE '50. '             TO  NUMB10I                      CL*14
01450             MOVE '    '             TO  NUMB11I                      CL*14
01451             MOVE '    '             TO  NUMB12I                      CL*14
01452             MOVE '    '             TO  NUMB13I                      CL*14
01453             MOVE '    '             TO  NUMB14I                      CL*14
01454             MOVE '    '             TO  NUMB15I                      CL*14
01455             MOVE '    '             TO  NUMB16I                      CL*14
01456             MOVE '    '             TO  NUMB17I                      CL*14
01457             MOVE '    '             TO  NUMB18I                      CL*14
01458             MOVE '    '             TO  NUMB19I                      CL*14
01459             MOVE '    '             TO  NUMB20I                      CL*14
01460             MOVE AL-SANON           TO  CODE11A                      CL*14
01461                                         TYPE11A                      CL*14
01462                                         REVSN11A                     CL*14
01463                                         RETRO11A                     CL*14
01464                                         RTRM11A                      CL*14
01465                                         CODE12A                      CL*14
01466                                         TYPE12A                      CL*14
01467                                         REVSN12A                     CL*14
01468                                         RETRO12A                     CL*14
01469                                         RTRM12A                      CL*14
01470                                         CODE13A                      CL*14
01471                                         TYPE13A                      CL*14
01472                                         REVSN13A                     CL*14
01473                                         RETRO13A                     CL*14
01474                                         RTRM13A                      CL*14
01475                                         CODE14A                      CL*14
01476                                         TYPE14A                      CL*14
01477                                         REVSN14A                     CL*14
01478                                         RETRO14A                     CL*14
01479                                         RTRM14A                      CL*14
01480                                         CODE15A                      CL*14
01481                                         TYPE15A                      CL*14
01482                                         REVSN15A                     CL*14
01483                                         RETRO15A                     CL*14
01484                                         RTRM15A                      CL*14
01485                                         CODE16A                      CL*14
01486                                         TYPE16A                      CL*14
01487                                         REVSN16A                     CL*14
01488                                         RETRO16A                     CL*14
01489                                         RTRM16A                      CL*14
01490                                         CODE17A                      CL*14
01491                                         TYPE17A                      CL*14
01492                                         REVSN17A                     CL*14
01493                                         RETRO17A                     CL*14
01494                                         RTRM17A                      CL*14
01495                                         CODE18A                      CL*14
01496                                         TYPE18A                      CL*14
01497                                         REVSN18A                     CL*14
01498                                         RETRO18A                     CL*14
01499                                         RTRM18A                      CL*14
01500                                         CODE19A                      CL*14
01501                                         TYPE19A                      CL*14
01502                                         REVSN19A                     CL*14
01503                                         RETRO19A                     CL*14
01504                                         RTRM19A                      CL*14
01505                                         CODE20A                      CL*14
01506                                         TYPE20A                      CL*14
01507                                         REVSN20A                     CL*14
01508                                         RETRO20A                     CL*14
01509                                         RTRM20A.                     CL*14
01510                                                                      CL*14
01511                                                                      CL*14
01512  8100-SEND-CONTINUE.                                                 CL*14
01513                                                                      CL**2
01514      EXEC CICS SEND                                               EL6505
01515          MAP      (MAP-NAME)                                      EL6505
01516          MAPSET   (MAPSET-NAME)                                   EL6505
01517          FROM     (EL6505AO)                                      EL6505
01518          ERASE                                                    EL6505
01519          CURSOR                                                   EL6505
01520      END-EXEC.                                                    EL6505
01521                                                                   EL6505
01522      GO TO 9100-RETURN-TRAN.                                      EL6505
01523                                                                   EL6505
01524  8200-SEND-DATAONLY.                                              EL6505
01525      MOVE SAVE-DATE              TO  DATEO.                       EL6505
01526      MOVE EIBTIME                TO  TIME-IN.                     EL6505
01527      MOVE TIME-OUT               TO  TIMEO.                       EL6505
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01528      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O                     EL6505
01529                                                                      CL**2
01530      MOVE 'RETRO'                TO  RETRO1I                         CL*13
01531                                      RETRO2I.                        CL*13
01532      MOVE ' Y/N '                TO  RETRO3I                         CL*13
01533                                      RETRO4I.                        CL*13
01534      IF PI-COMPANY-ID = 'DMD'                                        CL*13
01535         MOVE ' CONT'             TO  RETRO1I                         CL*13
01536                                      RETRO2I                         CL*13
01537         MOVE ' TYPE'             TO  RETRO3I                         CL*13
01538                                      RETRO4I.                        CL*13
01539                                                                      CL*14
01540      IF PI-COMPANY-ID = 'DMD'                                        CL*14
01541         MOVE 'PF1=BROWSE FORWARD' TO PF1KEYI                         CL*14
01542         MOVE 'PF2=BROWSE BACK   ' TO PF2KEYI.                        CL*14
01543                                                                      CL**2
01544      EXEC CICS SEND                                               EL6505
01545          MAP      (MAP-NAME)                                      EL6505
01546          MAPSET   (MAPSET-NAME)                                   EL6505
01547          FROM     (EL6505AO)                                      EL6505
01548          DATAONLY                                                 EL6505
01549          ERASEAUP                                                 EL6505
01550          CURSOR                                                   EL6505
01551      END-EXEC.                                                    EL6505
01552                                                                   EL6505
01553      GO TO 9100-RETURN-TRAN.                                      EL6505
01554                                                                   EL6505
01555  8300-SEND-TEXT.                                                  EL6505
01556      EXEC CICS SEND TEXT                                          EL6505
01557          FROM     (LOGOFF-TEXT)                                   EL6505
01558          LENGTH   (LOGOFF-LENGTH)                                 EL6505
01559          ERASE                                                    EL6505
01560          FREEKB                                                   EL6505
01561      END-EXEC.                                                    EL6505
01562                                                                   EL6505
01563      EXEC CICS RETURN                                             EL6505
01564      END-EXEC.                                                    EL6505
01565                                                                   EL6505
01566  8400-LOG-JOURNAL-RECORD.                                         EL6505
01567      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                  EL6505
01568      MOVE FILE-ID                TO  JP-FILE-ID.                  EL6505
01569      MOVE THIS-PGM               TO  JP-PROGRAM-ID.               EL6505
pemuni*    IF PI-JOURNAL-FILE-ID NOT = ZERO                             EL6505
pemuni*        EXEC CICS JOURNAL                                        EL6505
pemuni*            JFILEID     (PI-JOURNAL-FILE-ID)                     EL6505
pemuni*            JTYPEID     ('ER')                                   EL6505
pemuni*            FROM        (JOURNAL-RECORD)                         EL6505
pemuni*            LENGTH      (WS-JOURNAL-FILE-LENGTH)                 EL6505
pemuni*        END-EXEC.                                                EL6505
01577                                                                   EL6505
01578  8800-UNAUTHORIZED-ACCESS.                                        EL6505
01579      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL6505
01580      GO TO 8300-SEND-TEXT.                                        EL6505
01581                                                                   EL6505
01582  8810-PF23.                                                       EL6505
01583      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL6505
01584      MOVE XCTL-005               TO  PGM-NAME.                    EL6505
01585      GO TO 9300-XCTL.                                             EL6505
01586                                                                   EL6505
01587  9000-RETURN-CICS.                                                EL6505
01588      EXEC CICS RETURN                                             EL6505
01589      END-EXEC.                                                    EL6505
01590                                                                   EL6505
01591  9100-RETURN-TRAN.                                                EL6505
01592      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL6505
01593      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.        EL6505
01594      EXEC CICS RETURN                                             EL6505
01595          TRANSID    (TRANS-ID)                                    EL6505
01596          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6505
01597          LENGTH     (WS-COMM-LENGTH)                                 CL**5
01598      END-EXEC.                                                    EL6505
01599                                                                   EL6505
01600  9200-RETURN-MAIN-MENU.                                           EL6505
01601      MOVE XCTL-626               TO  PGM-NAME.                    EL6505
01602      GO TO 9300-XCTL.                                             EL6505
01603                                                                   EL6505
01604  9300-XCTL.                                                       EL6505
01605      EXEC CICS XCTL                                               EL6505
01606          PROGRAM    (PGM-NAME)                                    EL6505
01607          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6505
01608          LENGTH     (WS-COMM-LENGTH)                                 CL**5
01609      END-EXEC.                                                    EL6505
01610                                                                   EL6505
01611  9400-CLEAR.                                                      EL6505
01612      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                       CL**6
01613      GO TO 9300-XCTL.                                             EL6505
01614                                                                   EL6505
01615  9500-PF12.                                                       EL6505
01616      MOVE XCTL-010               TO  PGM-NAME.                    EL6505
01617      GO TO 9300-XCTL.                                             EL6505
01618                                                                   EL6505
01619  9600-PGMID-ERROR.                                                EL6505
01620      EXEC CICS HANDLE CONDITION                                   EL6505
01621          PGMIDERR    (8300-SEND-TEXT)                             EL6505
01622      END-EXEC.                                                    EL6505
01623                                                                   EL6505
01624      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          EL6505
01625      MOVE ' '                    TO  PI-ENTRY-CD-1.               EL6505
01626      MOVE XCTL-005               TO  PGM-NAME.                    EL6505
01627      MOVE PGM-NAME               TO  LOGOFF-PGM.                  EL6505
01628      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL6505
01629      GO TO 9300-XCTL.                                             EL6505
01630                                                                   EL6505
01631  9700-LINK-DATE-CONVERT.                                          EL6505
01632      EXEC CICS LINK                                               EL6505
01633          PROGRAM    ('ELDATCV')                                   EL6505
01634          COMMAREA   (DATE-CONVERSION-DATA)                        EL6505
01635          LENGTH     (DC-COMM-LENGTH)                              EL6505
01636      END-EXEC.                                                    EL6505
01637                                                                   EL6505
01638  9700-EXIT.                                                       EL6505
01639      EXIT.                                                        EL6505
01640                                                                   EL6505
01641  9900-ERROR-FORMAT.                                               EL6505
01642      IF NOT EMI-ERRORS-COMPLETE                                   EL6505
01643          MOVE LINK-001           TO  PGM-NAME                     EL6505
01644          EXEC CICS LINK                                           EL6505
01645              PROGRAM    (PGM-NAME)                                EL6505
01646              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL6505
01647              LENGTH     (EMI-COMM-LENGTH)                         EL6505
01648          END-EXEC.                                                EL6505
01649                                                                   EL6505
01650  9900-EXIT.                                                       EL6505
01651      EXIT.                                                        EL6505
01652                                                                   EL6505
01653  9990-ABEND.                                                      EL6505
01654      MOVE LINK-004               TO  PGM-NAME.                    EL6505
01655      MOVE DFHEIBLK               TO  EMI-LINE1.                      CL**6
01656      EXEC CICS LINK                                               EL6505
01657          PROGRAM   (PGM-NAME)                                     EL6505
01658          COMMAREA  (EMI-LINE1)                                    EL6505
01659          LENGTH    (72)                                           EL6505
01660      END-EXEC.                                                    EL6505
01661                                                                   EL6505
01662      GO TO 8200-SEND-DATAONLY.                                    EL6505
01663                                                                   EL6505
01664      GOBACK.                                                      EL6505
01665                                                                   EL6505
01666  9995-SECURITY-VIOLATION.                                         EL6505
01667             COPY ELCSCTP.                                         EL6505
01668  9995-EXIT.                                                       EL6505
01669       EXIT.                                                       EL6505
01670                                                                      CL*14
